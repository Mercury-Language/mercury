%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2012 The University of Melbourne.
% Copyright (C) 2013-2018 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: intermod.m.
% Main author: stayl (the original intermod.m).
%
% This module writes out the first half of .opt files, which we use
% to implement inter-module optimization. The second half is written out
% by intermod_analysis.m.
%
% The first half of the .opt file includes:
%   - The clauses for exported preds that can be inlined.
%   - The clauses for exported preds that have higher-order pred arguments.
%   - The pred/mode declarations for local predicates that the
%     above clauses use.
%   - pragma declarations for the exported preds.
%   - Non-exported types, insts and modes used by the above.
%   - Pragma foreign_enum, or foreign_type declarations for
%     any types output due to the line above.
%   - :- import_module declarations to import stuff used by the above.
%   - pragma foreign_import_module declarations if any pragma foreign_proc
%     preds are written.
% All these items should be module qualified.
%
% Note that predicates which call predicates that do not have mode or
% determinism declarations do not have clauses exported, since this would
% require running mode analysis and determinism analysis before writing the
% .opt file, significantly increasing compile time for a very small gain.
%
% This module also contains predicates to adjust the import status
% of local predicates which are exported for intermodule optimization.
%
%---------------------------------------------------------------------------%

:- module transform_hlds.intermod.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module parse_tree.
:- import_module parse_tree.prog_item.
:- import_module transform_hlds.intermod_info.

:- import_module io.

%---------------------------------------------------------------------------%

    % Open the file "<module-name>.opt.tmp", and write out the declarations
    % and clauses for intermodule optimization.
    %
    % Although this predicate creates the .opt.tmp file, it does not
    % necessarily create it in its final form. Later compiler passes
    % may append to this file using the append_analysis_pragmas_to_opt_file
    % predicate in intermod_analysis.m.
    % XXX This is not an elegant arrangement.
    %
    % Update_interface and touch_module_ext_datestamp are called from
    % mercury_compile_front_end.m, since they must be called after
    % the last time anything is appended to the .opt.tmp file.
    %
:- pred write_initial_opt_file(io.text_output_stream::in, module_info::in,
    intermod_info::out, parse_tree_plain_opt::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_class.
:- import_module hlds.hlds_clauses.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_inst_mode.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_out.hlds_out_goal.
:- import_module hlds.hlds_out.hlds_out_pred.
:- import_module hlds.hlds_out.hlds_out_util.
:- import_module hlds.hlds_pred.
:- import_module hlds.status.
:- import_module hlds.var_table_hlds.
:- import_module libs.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.parse_tree_out.
:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.parse_tree_out_misc.
:- import_module parse_tree.parse_tree_out_pragma.
:- import_module parse_tree.parse_tree_out_sym_name.
:- import_module parse_tree.parse_tree_to_term.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.var_db.
:- import_module parse_tree.var_table.
:- import_module transform_hlds.intermod_decide.
:- import_module transform_hlds.intermod_order_pred_info.
:- import_module transform_hlds.intermod_status.

:- import_module assoc_list.
:- import_module bool.
:- import_module cord.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module multi_map.
:- import_module one_or_more.
:- import_module one_or_more_map.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term.
:- import_module term_context.
:- import_module term_subst.
:- import_module varset.

%---------------------------------------------------------------------------%

write_initial_opt_file(TmpOptStream, ModuleInfo, IntermodInfo,
        ParseTreePlainOpt, !IO) :-
    decide_what_to_opt_export(ModuleInfo, IntermodInfo),
    write_opt_file_initial(TmpOptStream, IntermodInfo, ParseTreePlainOpt, !IO).

%---------------------------------------------------------------------------%

    % Output module imports, types, modes, insts and predicates.
    %
:- pred write_opt_file_initial(io.text_output_stream::in,
    intermod_info::in, parse_tree_plain_opt::out, io::di, io::uo) is det.

write_opt_file_initial(Stream, IntermodInfo, ParseTreePlainOpt, !IO) :-
    deconstruct_intermod_info(IntermodInfo, ModuleInfo, _,
        PredDecls, PredDefns, Instances, _, _),
    module_info_get_name(ModuleInfo, ModuleName),
    ModuleNameStr = mercury_bracketed_sym_name_to_string(ModuleName),
    io.format(Stream, ":- module %s.\n", [s(ModuleNameStr)], !IO),
    ( if
        % If none of these kinds of items need writing, then
        % nothing else needs to be written.
        set.is_empty(PredDecls),
        set.is_empty(PredDefns),
        Instances = [],
        module_info_get_type_table(ModuleInfo, TypeTable),
        get_all_type_ctor_defns(TypeTable, TypeCtorsDefns),
        some_type_needs_to_be_written(TypeCtorsDefns, no)
    then
        ParseTreePlainOpt = parse_tree_plain_opt(ModuleName, dummy_context,
            map.init, set.init, [], [], [], [], [], [], [], [], [], [], [], [],
            [], [], [], [], [], [], [], [], [], [])
    else
        write_opt_file_initial_body(Stream, IntermodInfo, ParseTreePlainOpt,
            !IO)
    ).

:- pred some_type_needs_to_be_written(
    assoc_list(type_ctor, hlds_type_defn)::in, bool::out) is det.

some_type_needs_to_be_written([], no).
some_type_needs_to_be_written([_ - TypeDefn | TypeCtorDefns], NeedWrite) :-
    hlds_data.get_type_defn_status(TypeDefn, TypeStatus),
    ( if
        ( TypeStatus = type_status(status_abstract_exported)
        ; TypeStatus = type_status(status_exported_to_submodules)
        )
    then
        NeedWrite = yes
    else
        some_type_needs_to_be_written(TypeCtorDefns, NeedWrite)
    ).

:- pred write_opt_file_initial_body(io.text_output_stream::in,
    intermod_info::in, parse_tree_plain_opt::out, io::di, io::uo) is det.

write_opt_file_initial_body(Stream, IntermodInfo, ParseTreePlainOpt, !IO) :-
    deconstruct_intermod_info(IntermodInfo, ModuleInfo, _,
        WriteDeclPredIdSet, WriteDefnPredIdSet, InstanceDefns,
        Types, NeedFIMs),
    set.to_sorted_list(WriteDeclPredIdSet, WriteDeclPredIds),
    set.to_sorted_list(WriteDefnPredIdSet, WriteDefnPredIds),

    module_info_get_avail_module_map(ModuleInfo, AvailModuleMap),
    % XXX CLEANUP We could and should reduce AvailModules to the set of modules
    % that are *actually needed* by the items being written.
    % XXX CLEANUP And even if builtin.m and/or private_builtin.m is needed
    % by an item, we *still* shouldn't include them, since the importing
    % module will import and use them respectively anyway.
    map.keys(AvailModuleMap, UsedModuleNames),
    AddToUseMap =
        ( pred(MN::in, UM0::in, UM::out) is det :-
            % We don't have a context for any use_module declaration
            % of this module (since it may have a import_module declaration
            % instead), which is why we specify a dummy context.
            % However, these contexts are used only when the .opt file
            % is read in, not when it is being generated.
            one_or_more_map.add(MN, dummy_context, UM0, UM)
        ),
    list.foldl(AddToUseMap, UsedModuleNames, one_or_more_map.init, UseMap),

    (
        NeedFIMs = do_need_foreign_import_modules,
        module_info_get_c_j_cs_fims(ModuleInfo, CJCsFIMs),
        FIMSpecsSet = get_all_fim_specs(CJCsFIMs),
        FIMSpecs = set.to_sorted_list(FIMSpecsSet)
    ;
        NeedFIMs = do_not_need_foreign_import_modules,
        set.init(FIMSpecsSet),
        FIMSpecs = []
    ),

    module_info_get_globals(ModuleInfo, Globals),
    OutInfo0 = init_hlds_out_info(Globals, output_mercury),

    % We don't want to write line numbers from the source file to .opt files,
    % because that causes spurious changes to the .opt files
    % when you make trivial changes (e.g. add comments) to the source files.
    MercInfo0 = OutInfo0 ^ hoi_merc_out_info,
    MercInfo = merc_out_info_disable_line_numbers(MercInfo0),
    OutInfo = OutInfo0 ^ hoi_merc_out_info := MercInfo,
    % Disable verbose dumping of clauses.
    OutInfoForPreds = OutInfo ^ hoi_dump_hlds_options := "",

    intermod_gather_types(Types, TypeDefns, ForeignEnums),
    intermod_gather_insts(ModuleInfo, InstDefns),
    intermod_gather_modes(ModuleInfo, ModeDefns),
    intermod_gather_classes(ModuleInfo, TypeClasses),
    intermod_gather_instances(InstanceDefns, Instances),

    list.foldl(mercury_output_module_decl(Stream, "use_module"),
        UsedModuleNames, !IO),
    maybe_format_block_start_blank_line(Stream, FIMSpecs, !IO),
    list.foldl(mercury_output_fim_spec(Stream), FIMSpecs, !IO),
    maybe_format_block_start_blank_line(Stream, TypeDefns, !IO),
    list.foldl(mercury_format_item_type_defn(MercInfo, Stream),
        TypeDefns, !IO),
    maybe_format_block_start_blank_line(Stream, ForeignEnums, !IO),
    list.foldl(mercury_format_item_foreign_enum(MercInfo, Stream),
        ForeignEnums, !IO),
    maybe_format_block_start_blank_line(Stream, InstDefns, !IO),
    list.foldl(mercury_format_item_inst_defn(MercInfo, Stream),
        InstDefns, !IO),
    maybe_format_block_start_blank_line(Stream, ModeDefns, !IO),
    list.foldl(mercury_format_item_mode_defn(MercInfo, Stream),
        ModeDefns, !IO),
    maybe_format_block_start_blank_line(Stream, TypeClasses, !IO),
    list.foldl(mercury_format_item_typeclass(MercInfo, Stream),
        TypeClasses, !IO),
    maybe_format_block_start_blank_line(Stream, Instances, !IO),
    list.foldl(mercury_format_item_instance(MercInfo, Stream),
        Instances, !IO),

    generate_order_pred_infos(ModuleInfo, WriteDeclPredIds,
        DeclOrderPredInfos),
    generate_order_pred_infos(ModuleInfo, WriteDefnPredIds,
        DefnOrderPredInfos),
    (
        DeclOrderPredInfos = [],
        PredDecls = [],
        ModeDecls = [],
        DeclMarkersCord0 = cord.init,
        ImplMarkersCord0 = cord.init,
        TypeSpecs = []
    ;
        DeclOrderPredInfos = [_ | _],
        io.nl(Stream, !IO),
        intermod_write_pred_decls(MercInfo, Stream, ModuleInfo,
            DeclOrderPredInfos,
            cord.init, PredDeclsCord,
            cord.init, ModeDeclsCord,
            cord.init, DeclMarkersCord0,
            cord.init, ImplMarkersCord0,
            cord.init, TypeSpecsCord, !IO),
        PredDecls = cord.list(PredDeclsCord),
        ModeDecls = cord.list(ModeDeclsCord),
        TypeSpecs = cord.list(TypeSpecsCord)
    ),
    % Each of these writes a newline at the start.
    intermod_write_pred_defns(OutInfoForPreds, Stream, ModuleInfo,
        DefnOrderPredInfos,
        DeclMarkersCord0, DeclMarkersCord,
        ImplMarkersCord0, ImplMarkersCord, !IO),
    Clauses = [],
    ForeignProcs = [],
    % XXX CLEANUP This *may* be a lie, in that some of the predicates we have
    % written out above *may* have goal_type_promise. However, until
    % we switch over completely to creating .opt files purely by building up
    % and then writing out a parse_tree_plain_opt, this shouldn't matter.
    Promises = [],
    DeclMarkers = cord.list(DeclMarkersCord),
    ImplMarkers = cord.list(ImplMarkersCord),

    module_info_get_name(ModuleInfo, ModuleName),
    ParseTreePlainOpt = parse_tree_plain_opt(ModuleName, dummy_context,
        UseMap, FIMSpecsSet, TypeDefns, ForeignEnums,
        InstDefns, ModeDefns, TypeClasses, Instances,
        PredDecls, ModeDecls, Clauses, ForeignProcs, Promises,
        DeclMarkers, ImplMarkers, TypeSpecs, [], [], [], [], [], [], [], []).

:- type maybe_first
    --->    is_not_first
    ;       is_first.

%---------------------------------------------------------------------------%

:- pred intermod_gather_types(assoc_list(type_ctor, hlds_type_defn)::in,
    list(item_type_defn_info)::out, list(item_foreign_enum_info)::out) is det.

intermod_gather_types(Types, TypeDefns, ForeignEnums) :-
    list.sort(Types, SortedTypes),
    list.foldl2(intermod_gather_type, SortedTypes,
        cord.init, TypeDefnsCord, cord.init, ForeignEnumsCord),
    TypeDefns = cord.list(TypeDefnsCord),
    ForeignEnums = cord.list(ForeignEnumsCord).

:- pred intermod_gather_type(pair(type_ctor, hlds_type_defn)::in,
    cord(item_type_defn_info)::in, cord(item_type_defn_info)::out,
    cord(item_foreign_enum_info)::in, cord(item_foreign_enum_info)::out)
    is det.

intermod_gather_type(TypeCtor - TypeDefn,
        !TypeDefnsCord, !ForeignEnumsCord) :-
    hlds_data.get_type_defn_tvarset(TypeDefn, TVarSet),
    hlds_data.get_type_defn_tparams(TypeDefn, TypeParams),
    hlds_data.get_type_defn_body(TypeDefn, Body),
    hlds_data.get_type_defn_context(TypeDefn, Context),
    TypeCtor = type_ctor(TypeSymName, _Arity),
    (
        Body = hlds_du_type(TypeBodyDu),
        TypeBodyDu = type_body_du(Ctors, MaybeSubType, MaybeCanon,
            MaybeRepnA, MaybeForeignTypeBody),
        (
            MaybeRepnA = no,
            unexpected($pred, "MaybeRepnA = no")
        ;
            MaybeRepnA = yes(RepnA),
            MaybeDirectArgCtors = RepnA ^ dur_direct_arg_ctors
        ),
        (
            MaybeSubType = subtype_of(SuperType),
            % TypeCtor may be noncanonical, and MaybeDirectArgCtors may be
            % nonempty, but any reader of the .opt file has to find out
            % both those facts from the base type of this subtype.
            DetailsSub = type_details_sub(SuperType, Ctors),
            TypeBody = parse_tree_sub_type(DetailsSub)
        ;
            MaybeSubType = not_a_subtype,
            % XXX TYPE_REPN We should output information about any direct args
            % as a separate type_repn item.
            DetailsDu = type_details_du(Ctors, MaybeCanon,
                MaybeDirectArgCtors),
            TypeBody = parse_tree_du_type(DetailsDu)
        )
    ;
        Body = hlds_eqv_type(EqvType),
        TypeBody = parse_tree_eqv_type(type_details_eqv(EqvType)),
        MaybeForeignTypeBody = no
    ;
        Body = hlds_abstract_type(Details),
        TypeBody = parse_tree_abstract_type(Details),
        MaybeForeignTypeBody = no
    ;
        Body = hlds_foreign_type(ForeignTypeBody0),
        TypeBody = parse_tree_abstract_type(abstract_type_general),
        MaybeForeignTypeBody = yes(ForeignTypeBody0)
    ;
        Body = hlds_solver_type(DetailsSolver),
        TypeBody = parse_tree_solver_type(DetailsSolver),
        MaybeForeignTypeBody = no
    ),
    MainItemTypeDefn = item_type_defn_info(TypeSymName, TypeParams, TypeBody,
        TVarSet, Context, item_no_seq_num),
    cord.snoc(MainItemTypeDefn, !TypeDefnsCord),
    (
        MaybeForeignTypeBody = no
    ;
        MaybeForeignTypeBody = yes(ForeignTypeBody),
        ForeignTypeBody = foreign_type_body(MaybeC, MaybeJava, MaybeCsharp),
        maybe_acc_foreign_type_defn_info(TypeSymName, TypeParams, TVarSet,
            Context, (func(FT) = c(FT)), MaybeC, !TypeDefnsCord),
        maybe_acc_foreign_type_defn_info(TypeSymName, TypeParams, TVarSet,
            Context, (func(FT) = java(FT)), MaybeJava, !TypeDefnsCord),
        maybe_acc_foreign_type_defn_info(TypeSymName, TypeParams, TVarSet,
            Context, (func(FT) = csharp(FT)), MaybeCsharp, !TypeDefnsCord)
    ),
    ( if
        Body = hlds_du_type(type_body_du(_, _, _, MaybeRepnB, _)),
        MaybeRepnB = yes(RepnB),
        RepnB = du_type_repn(CtorRepns, _, _, DuTypeKind, _),
        DuTypeKind = du_type_kind_foreign_enum(Lang)
    then
        % XXX TYPE_REPN This code puts into the .opt file the foreign enum
        % specification for this type_ctor ONLY for the foreign language
        % used by the current target platform. We cannot fix this until
        % we preserve the same information for all the other foreign languages
        % as well.
        list.foldl(gather_foreign_enum_value_pair, CtorRepns,
            [], RevForeignEnumVals),
        list.reverse(RevForeignEnumVals, ForeignEnumVals),
        (
            ForeignEnumVals = []
            % This can only happen if the type has no function symbols.
            % which should have been detected and reported by now.
        ;
            ForeignEnumVals = [HeadForeignEnumVal | TailForeignEnumVals],
            OoMForeignEnumVals =
                one_or_more(HeadForeignEnumVal, TailForeignEnumVals),
            ForeignEnum = item_foreign_enum_info(Lang, TypeCtor,
                OoMForeignEnumVals, Context, item_no_seq_num),
            cord.snoc(ForeignEnum, !ForeignEnumsCord)
        )
    else
        true
    ).

:- pred maybe_acc_foreign_type_defn_info(sym_name::in, list(type_param)::in,
    tvarset::in, prog_context::in,
    (func(T) = generic_language_foreign_type)::in,
    maybe(type_details_foreign(T))::in,
    cord(item_type_defn_info)::in, cord(item_type_defn_info)::out) is det.

maybe_acc_foreign_type_defn_info(TypeSymName, TypeParams, TVarSet, Context,
        MakeGeneric, MaybeDetails, !TypeDefnsCord) :-
    (
        MaybeDetails = no
    ;
        MaybeDetails = yes(Details),
        Details = type_details_foreign(LangForeignType, MaybeUserEqComp,
            Assertions),
        DetailsForeign = type_details_foreign(MakeGeneric(LangForeignType),
            MaybeUserEqComp, Assertions),
        ItemTypeDefn = item_type_defn_info(TypeSymName, TypeParams,
            parse_tree_foreign_type(DetailsForeign),
            TVarSet, Context, item_no_seq_num),
        cord.snoc(ItemTypeDefn, !TypeDefnsCord)
    ).

:- pred gather_foreign_enum_value_pair(constructor_repn::in,
    assoc_list(sym_name, string)::in, assoc_list(sym_name, string)::out)
    is det.

gather_foreign_enum_value_pair(CtorRepn, !RevValues) :-
    CtorRepn = ctor_repn(_, _, SymName, Tag, _, Arity, _),
    expect(unify(Arity, 0), $pred, "Arity != 0"),
    ( if Tag = foreign_tag(_ForeignLang, ForeignTag) then
        !:RevValues = [SymName - ForeignTag | !.RevValues]
    else
        unexpected($pred, "expected foreign tag")
    ).

%---------------------------------------------------------------------------%

:- pred intermod_gather_insts(module_info::in,
    list(item_inst_defn_info)::out) is det.

intermod_gather_insts(ModuleInfo, InstDefns) :-
    module_info_get_name(ModuleInfo, ModuleName),
    module_info_get_inst_table(ModuleInfo, Insts),
    inst_table_get_user_insts(Insts, UserInstMap),
    map.foldl(intermod_gather_inst(ModuleName), UserInstMap,
        cord.init, InstDefnsCord),
    InstDefns = cord.list(InstDefnsCord).

:- pred intermod_gather_inst(module_name::in,
    inst_ctor::in, hlds_inst_defn::in,
    cord(item_inst_defn_info)::in, cord(item_inst_defn_info)::out) is det.

intermod_gather_inst(ModuleName, InstCtor, InstDefn, !InstDefnsCord) :-
    InstCtor = inst_ctor(SymName, _Arity),
    InstDefn = hlds_inst_defn(VarSet, Args, Inst, IFTC, Context, InstStatus),
    ( if
        SymName = qualified(ModuleName, _),
        inst_status_to_write(InstStatus) = yes
    then
        (
            IFTC = iftc_applicable_declared(ForTypeCtor),
            MaybeForTypeCtor = yes(ForTypeCtor)
        ;
            ( IFTC = iftc_not_bound_inst
            ; IFTC = iftc_applicable_known(_)
            ; IFTC = iftc_applicable_not_known
            ; IFTC = iftc_applicable_error_unknown_type
            ; IFTC = iftc_applicable_error_eqv_type(_)
            ; IFTC = iftc_applicable_error_visibility(_)
            ; IFTC = iftc_applicable_error_mismatches(_)
            ),
            MaybeForTypeCtor = no
        ),
        ItemInstDefn = item_inst_defn_info(SymName, Args, MaybeForTypeCtor,
            nonabstract_inst_defn(Inst), VarSet, Context, item_no_seq_num),
        cord.snoc(ItemInstDefn, !InstDefnsCord)
    else
        true
    ).

%---------------------------------------------------------------------------%

:- pred intermod_gather_modes(module_info::in,
    list(item_mode_defn_info)::out) is det.

intermod_gather_modes(ModuleInfo, ModeDefns) :-
    module_info_get_name(ModuleInfo, ModuleName),
    module_info_get_mode_table(ModuleInfo, Modes),
    mode_table_get_mode_defns(Modes, ModeDefnMap),
    map.foldl(intermod_gather_mode(ModuleName), ModeDefnMap,
        cord.init, ModeDefnsCord),
    ModeDefns = cord.list(ModeDefnsCord).

:- pred intermod_gather_mode(module_name::in,
    mode_ctor::in, hlds_mode_defn::in,
    cord(item_mode_defn_info)::in, cord(item_mode_defn_info)::out) is det.

intermod_gather_mode(ModuleName, ModeCtor, ModeDefn, !ModeDefnsCord) :-
    ModeCtor = mode_ctor(SymName, _Arity),
    ModeDefn = hlds_mode_defn(VarSet, Args, hlds_mode_body(Mode), Context,
        ModeStatus),
    ( if
        SymName = qualified(ModuleName, _),
        mode_status_to_write(ModeStatus) = yes
    then
        MaybeAbstractModeDefn = nonabstract_mode_defn(eqv_mode(Mode)),
        ItemModeDefn = item_mode_defn_info(SymName, Args,
            MaybeAbstractModeDefn, VarSet, Context, item_no_seq_num),
        cord.snoc(ItemModeDefn, !ModeDefnsCord)
    else
        true
    ).

%---------------------------------------------------------------------------%

:- pred intermod_gather_classes(module_info::in,
    list(item_typeclass_info)::out) is det.

intermod_gather_classes(ModuleInfo, TypeClasses) :-
    module_info_get_name(ModuleInfo, ModuleName),
    module_info_get_class_table(ModuleInfo, ClassDefnMap),
    map.foldl(intermod_gather_class(ModuleName), ClassDefnMap,
        cord.init, TypeClassesCord),
    TypeClasses = cord.list(TypeClassesCord).

:- pred intermod_gather_class(module_name::in,
    class_id::in, hlds_class_defn::in,
    cord(item_typeclass_info)::in, cord(item_typeclass_info)::out) is det.

intermod_gather_class(ModuleName, ClassId, ClassDefn, !TypeClassesCord) :-
    ClassDefn = hlds_class_defn(TypeClassStatus, TVarSet, _Kinds, TVars,
        Constraints, HLDSFunDeps, _Ancestors,
        InstanceBody, _MaybeMethodInfos, Context, _HasBadDefn),
    ClassId = class_id(QualifiedClassName, _),
    ( if
        QualifiedClassName = qualified(ModuleName, _),
        typeclass_status_to_write(TypeClassStatus) = yes
    then
        FunDeps = list.map(unmake_hlds_class_fundep(TVars), HLDSFunDeps),
        ItemTypeClass = item_typeclass_info(QualifiedClassName, TVars,
            Constraints, FunDeps, InstanceBody, TVarSet,
            Context, item_no_seq_num),
        cord.snoc(ItemTypeClass, !TypeClassesCord)
    else
        true
    ).

:- func unmake_hlds_class_fundep(list(tvar), hlds_class_fundep) = prog_fundep.

unmake_hlds_class_fundep(TVars, HLDSFunDep) = ParseTreeFunDep :-
    HLDSFunDep = fundep(DomainArgPosns, RangeArgPosns),
    DomainTVars = unmake_hlds_class_fundep_arg_posns(TVars, DomainArgPosns),
    RangeTVars = unmake_hlds_class_fundep_arg_posns(TVars, RangeArgPosns),
    ParseTreeFunDep = fundep(DomainTVars, RangeTVars).

:- func unmake_hlds_class_fundep_arg_posns(list(tvar), set(hlds_class_argpos))
    = list(tvar).

unmake_hlds_class_fundep_arg_posns(TVars, ArgPosns) = ArgTVars :-
    ArgTVarsSet = set.map(list.det_index1(TVars), ArgPosns),
    set.to_sorted_list(ArgTVarsSet, ArgTVars).

%---------------------------------------------------------------------------%

:- pred intermod_gather_instances(assoc_list(class_id, hlds_instance_defn)::in,
    list(item_instance_info)::out) is det.

intermod_gather_instances(InstanceDefns, Instances) :-
    list.sort(InstanceDefns, SortedInstanceDefns),
    list.foldl(intermod_gather_instance, SortedInstanceDefns,
        cord.init, InstancesCord),
    Instances = cord.list(InstancesCord).

:- pred intermod_gather_instance(pair(class_id, hlds_instance_defn)::in,
    cord(item_instance_info)::in, cord(item_instance_info)::out) is det.

intermod_gather_instance(ClassId - InstanceDefn, !InstancesCord) :-
    InstanceDefn = hlds_instance_defn(ModuleName, _, TVarSet,
        OriginalTypes, Types, Constraints, _, _, Body, _, Context),
    ClassId = class_id(ClassName, _),
    ItemInstance = item_instance_info(ClassName, Types, OriginalTypes,
        Constraints, Body, TVarSet, ModuleName, Context, item_no_seq_num),
    cord.snoc(ItemInstance, !InstancesCord).

%---------------------------------------------------------------------------%

    % We need to write all the declarations for local predicates so
    % the procedure labels for the C code are calculated correctly.
    %
:- pred intermod_write_pred_decls(merc_out_info::in, io.text_output_stream::in,
    module_info::in, list(order_pred_info)::in,
    cord(item_pred_decl_info)::in, cord(item_pred_decl_info)::out,
    cord(item_mode_decl_info)::in, cord(item_mode_decl_info)::out,
    cord(item_decl_marker_info_opt)::in, cord(item_decl_marker_info_opt)::out,
    cord(item_impl_marker_info_opt)::in, cord(item_impl_marker_info_opt)::out,
    cord(decl_pragma_type_spec_info)::in,
        cord(decl_pragma_type_spec_info)::out,
    io::di, io::uo) is det.

intermod_write_pred_decls(_, _, _, [],
        !PredDeclsCord, !ModeDeclsCord,
        !DeclMarkersCord, !ImplMarkersCord, !TypeSpecsCord, !IO).
intermod_write_pred_decls(MercInfo, Stream, ModuleInfo,
        [OrderPredInfo | OrderPredInfos],
        !PredDeclsCord, !ModeDeclsCord,
        !DeclMarkersCord, !ImplMarkersCord, !TypeSpecsCord, !IO) :-
    intermod_write_pred_decl(MercInfo, Stream, ModuleInfo, OrderPredInfo,
        !PredDeclsCord, !ModeDeclsCord,
        !DeclMarkersCord, !ImplMarkersCord, !TypeSpecsCord, !IO),
    intermod_write_pred_decls(MercInfo, Stream, ModuleInfo, OrderPredInfos,
        !PredDeclsCord, !ModeDeclsCord,
        !DeclMarkersCord, !ImplMarkersCord, !TypeSpecsCord, !IO).

:- pred intermod_write_pred_decl(merc_out_info::in, io.text_output_stream::in,
    module_info::in, order_pred_info::in,
    cord(item_pred_decl_info)::in, cord(item_pred_decl_info)::out,
    cord(item_mode_decl_info)::in, cord(item_mode_decl_info)::out,
    cord(item_decl_marker_info_opt)::in, cord(item_decl_marker_info_opt)::out,
    cord(item_impl_marker_info_opt)::in, cord(item_impl_marker_info_opt)::out,
    cord(decl_pragma_type_spec_info)::in,
        cord(decl_pragma_type_spec_info)::out,
    io::di, io::uo) is det.

intermod_write_pred_decl(MercInfo, Stream, ModuleInfo, OrderPredInfo,
        !PredDeclsCord, !ModeDeclsCord,
        !DeclMarkersCord, !ImplMarkersCord, !TypeSpecsCord, !IO) :-
    OrderPredInfo = order_pred_info(PredName, _PredArity, PredOrFunc,
        PredId, PredInfo),
    ModuleName = pred_info_module(PredInfo),
    pred_info_get_arg_types(PredInfo, TVarSet, ExistQVars, ArgTypes),
    pred_info_get_purity(PredInfo, Purity),
    pred_info_get_class_context(PredInfo, ClassContext),
    pred_info_get_context(PredInfo, Context),
    PredSymName = qualified(ModuleName, PredName),
    TypesAndNoModes = list.map((func(T) = type_only(T)), ArgTypes),
    MaybeWithType = maybe.no,
    MaybeWithInst = maybe.no,
    MaybeDetism = maybe.no,     % We are NOT declaring the mode.
    varset.init(InstVarSet),
    % Origin is a dummy, which is OK because the origin is never printed.
    % If that ever changes, we would have to reverse the transform done
    % by record_pred_origin in add_pred.m.
    Origin = item_origin_user,
    PredDecl = item_pred_decl_info(PredSymName, PredOrFunc,
        TypesAndNoModes, MaybeWithType, MaybeWithInst, MaybeDetism, Origin,
        TVarSet, InstVarSet, ExistQVars, Purity, ClassContext,
        Context, item_no_seq_num),
    % NOTE: The names of type variables in type_spec pragmas must match
    % *exactly* the names of the corresponding type variables in the
    % predicate declaration to which they apply. This is why one variable,
    % VarNamePrint, controls both.
    %
    % If a predicate is defined by a foreign_proc, then its declaration
    % *must* be printed with print_name_only, because that is the only way
    % that any reference to the type_info variable in the foreign code
    % in the body of the foreign_proc will match the declared name of the
    % type variable that it is for.
    %
    % We used to print the predicate declarations with print_name_only
    % for such predicates (predicates defined by foreign_procs) and with
    % print_name_and_num for all other predicates. (That included predicates
    % representing promises.) However, the predicates whose declarations
    % we are writing out have not been through any transformation that
    % would have either (a) changed the names of any existing type variables,
    % or (b) introduced any new type variables, so the mapping between
    % type variable numbers and names should be the same now as when the
    % the predicate declaration was first parsed. And at that time, two
    % type variable occurrences with the same name obviously referred to the
    % same type variable, so the numeric suffix added by print_name_and_num
    % was obviously not needed.
    VarNamePrint = print_name_only,
    mercury_format_item_pred_decl(output_mercury, VarNamePrint, Stream,
        PredDecl, !IO),
    pred_info_get_proc_table(PredInfo, ProcMap),
    % Make sure the mode declarations go out in the same order they came in,
    % so that the all the modes get the same proc_id in the importing modules.
    % SortedProcPairs will be sorted on proc_ids. (map.values is not
    % *documented* to return a list sorted by keys.)
    map.to_sorted_assoc_list(ProcMap, SortedProcPairs),
    intermod_gather_pred_valid_modes(PredOrFunc, PredSymName,
        SortedProcPairs, ModeDecls),
    intermod_gather_pred_marker_pragmas(PredInfo, DeclMarkers, ImplMarkers),
    intermod_gather_pred_type_spec_pragmas(ModuleInfo, PredId, TypeSpecs),

    list.foldl(mercury_format_item_mode_decl(MercInfo, Stream),
        ModeDecls, !IO),
    list.foldl(mercury_format_item_decl_marker(Stream),
        coerce(DeclMarkers), !IO),
    list.foldl(mercury_format_item_impl_marker(Stream),
        coerce(ImplMarkers), !IO),
    Lang = output_mercury,
    list.foldl(mercury_format_pragma_type_spec(Stream, Lang), TypeSpecs, !IO),

    cord.snoc(PredDecl, !PredDeclsCord),
    !:ModeDeclsCord = !.ModeDeclsCord ++ cord.from_list(ModeDecls),
    !:DeclMarkersCord = !.DeclMarkersCord ++ cord.from_list(DeclMarkers),
    !:ImplMarkersCord = !.ImplMarkersCord ++ cord.from_list(ImplMarkers),
    !:TypeSpecsCord = !.TypeSpecsCord ++ cord.from_list(TypeSpecs).

:- pred intermod_gather_pred_valid_modes(pred_or_func::in, sym_name::in,
    assoc_list(proc_id, proc_info)::in, list(item_mode_decl_info)::out) is det.

intermod_gather_pred_valid_modes(_, _, [], []).
intermod_gather_pred_valid_modes(PredOrFunc, PredSymName,
        [ProcIdInfo | ProcIdInfos], [HeadModeDecl | TailModeDecls]) :-
    intermod_gather_pred_valid_modes(PredOrFunc, PredSymName,
        ProcIdInfos, TailModeDecls),
    ProcIdInfo = _ProcId - ProcInfo,
    proc_info_get_maybe_declared_argmodes(ProcInfo, MaybeArgModes),
    proc_info_get_declared_determinism(ProcInfo, MaybeDetism),
    ( if
        MaybeArgModes = yes(ArgModesPrime),
        MaybeDetism = yes(DetismPrime)
    then
        ArgModes = ArgModesPrime,
        Detism = DetismPrime
    else
        unexpected($pred, "attempt to write undeclared mode")
    ),
    MaybeWithInst = maybe.no,
    varset.init(InstVarSet),
    HeadModeDecl = item_mode_decl_info(PredSymName, yes(PredOrFunc),
        ArgModes, MaybeWithInst, yes(Detism), InstVarSet,
        dummy_context, item_no_seq_num).

:- pred intermod_gather_pred_marker_pragmas(pred_info::in,
    list(item_decl_marker_info_opt)::out, list(item_impl_marker_info_opt)::out)
    is det.

intermod_gather_pred_marker_pragmas(PredInfo, DeclMarkers, ImplMarkers) :-
    ModuleName = pred_info_module(PredInfo),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    PredName = pred_info_name(PredInfo),
    PredSymName = qualified(ModuleName, PredName),
    pred_info_get_orig_arity(PredInfo, PredFormArity),
    user_arity_pred_form_arity(PredOrFunc, UserArity, PredFormArity),
    pred_info_get_markers(PredInfo, Markers),
    markers_to_marker_list(Markers, MarkerList),
    ( PredOrFunc = pf_predicate, PFU = pfu_predicate
    ; PredOrFunc = pf_function,  PFU = pfu_function
    ),
    intermod_gather_pred_marker_pragmas_loop(PFU,
        PredSymName, UserArity, MarkerList,
        [], RevDeclMarkers, [], RevImplMarkers),
    list.reverse(RevDeclMarkers, DeclMarkers),
    list.reverse(RevImplMarkers, ImplMarkers).

:- pred intermod_gather_pred_marker_pragmas_loop(pred_func_or_unknown_pf::in,
    sym_name::in, user_arity::in, list(pred_marker)::in,
    list(item_decl_marker_info_opt)::in, list(item_decl_marker_info_opt)::out,
    list(item_impl_marker_info_opt)::in, list(item_impl_marker_info_opt)::out)
    is det.

intermod_gather_pred_marker_pragmas_loop(_, _, _,
        [], !RevDeclMarkers, !RevImplMarkers).
intermod_gather_pred_marker_pragmas_loop(PredOrFunc, PredSymName, UserArity,
        [Marker | Markers], !RevDeclMarkers, !RevImplMarkers) :-
    (
        % We do not output these markers.
        ( Marker = marker_stub
        ; Marker = marker_builtin_stub
        ; Marker = marker_no_pred_decl
        ; Marker = marker_no_detism_warning
        ; Marker = marker_heuristic_inline
        ; Marker = marker_mmc_marked_no_inline
        ; Marker = marker_consider_used
        ; Marker = marker_calls_are_fully_qualified
        ; Marker = marker_mutable_access_pred
        ; Marker = marker_has_require_scope
        ; Marker = marker_has_incomplete_switch
        ; Marker = marker_has_format_call
        ; Marker = marker_has_rhs_lambda
        ; Marker = marker_fact_table_semantic_errors

        % Since the inferred declarations are output, these don't need
        % to be done in the importing module.
        ; Marker = marker_infer_type
        ; Marker = marker_infer_modes

        % Purity is output as part of the pred/func decl.
        ; Marker = marker_is_impure
        ; Marker = marker_is_semipure

        % There is no pragma required for generated class methods.
        ; Marker = marker_class_method
        ; Marker = marker_class_instance_method
        ; Marker = marker_named_class_instance_method

        % Termination should only be checked in the defining module.
        ; Marker = marker_check_termination
        )
    ;
        % We do output these markers.
        (
            Marker = marker_terminates,
            DeclPragmaKind = dpmk_terminates
        ;
            Marker = marker_does_not_terminate,
            DeclPragmaKind = dpmk_does_not_terminate
        ),
        PredSpec = pred_pfu_name_arity(PredOrFunc, PredSymName, UserArity),
        DeclMarker = item_decl_marker_info(DeclPragmaKind, PredSpec,
            dummy_context, item_no_seq_num),
        !:RevDeclMarkers = [DeclMarker | !.RevDeclMarkers]
    ;
        (
            Marker = marker_user_marked_inline,
            ImplPragmaKind = ipmk_inline
        ;
            Marker = marker_user_marked_no_inline,
            ImplPragmaKind = ipmk_no_inline
        ;
            Marker = marker_promised_pure,
            ImplPragmaKind = ipmk_promise_pure
        ;
            Marker = marker_mode_check_clauses,
            ImplPragmaKind = ipmk_mode_check_clauses
        ;
            Marker = marker_promised_semipure,
            ImplPragmaKind = ipmk_promise_semipure
        ;
            Marker = marker_promised_equivalent_clauses,
            ImplPragmaKind = ipmk_promise_eqv_clauses
        ),
        PredSpec = pred_pfu_name_arity(PredOrFunc, PredSymName, UserArity),
        ImplMarker = item_impl_marker_info(ImplPragmaKind, PredSpec,
            dummy_context, item_no_seq_num),
        !:RevImplMarkers = [ImplMarker | !.RevImplMarkers]
    ),
    intermod_gather_pred_marker_pragmas_loop(PredOrFunc, PredSymName,
        UserArity, Markers, !RevDeclMarkers, !RevImplMarkers).

:- pred intermod_gather_pred_type_spec_pragmas(module_info::in, pred_id::in,
    list(decl_pragma_type_spec_info)::out) is det.

intermod_gather_pred_type_spec_pragmas(ModuleInfo, PredId, TypeSpecs) :-
    module_info_get_type_spec_info(ModuleInfo, TypeSpecInfo),
    PragmaMap = TypeSpecInfo ^ pragma_map,
    ( if multi_map.search(PragmaMap, PredId, TypeSpecsPrime) then
        TypeSpecs = TypeSpecsPrime
    else
        TypeSpecs = []
    ).

%---------------------------------------------------------------------------%

:- pred intermod_write_pred_defns(hlds_out_info::in, io.text_output_stream::in,
    module_info::in, list(order_pred_info)::in,
    cord(item_decl_marker_info_opt)::in, cord(item_decl_marker_info_opt)::out,
    cord(item_impl_marker_info_opt)::in, cord(item_impl_marker_info_opt)::out,
    io::di, io::uo) is det.

intermod_write_pred_defns(_, _, _, [], !DeclMarkers, !ImplMarkers, !IO).
intermod_write_pred_defns(OutInfo, Stream, ModuleInfo,
        [OrderPredInfo | OrderPredInfos], !DeclMarkers, !ImplMarkers, !IO) :-
    intermod_write_pred_defn(OutInfo, Stream, ModuleInfo, OrderPredInfo,
        !DeclMarkers, !ImplMarkers, !IO),
    intermod_write_pred_defns(OutInfo, Stream, ModuleInfo, OrderPredInfos,
        !DeclMarkers, !ImplMarkers, !IO).

:- pred intermod_write_pred_defn(hlds_out_info::in, io.text_output_stream::in,
    module_info::in, order_pred_info::in,
    cord(item_decl_marker_info_opt)::in, cord(item_decl_marker_info_opt)::out,
    cord(item_impl_marker_info_opt)::in, cord(item_impl_marker_info_opt)::out,
    io::di, io::uo) is det.

intermod_write_pred_defn(OutInfo, Stream, ModuleInfo, OrderPredInfo,
        !DeclMarkers, !ImplMarkers, !IO) :-
    io.nl(Stream, !IO),
    OrderPredInfo = order_pred_info(PredName, _PredArity, PredOrFunc,
        PredId, PredInfo),
    ModuleName = pred_info_module(PredInfo),
    PredSymName = qualified(ModuleName, PredName),
    intermod_gather_pred_marker_pragmas(PredInfo, DeclMarkers, ImplMarkers),
    list.foldl(mercury_format_item_decl_marker(Stream),
        coerce(DeclMarkers), !IO),
    list.foldl(mercury_format_item_impl_marker(Stream),
        coerce(ImplMarkers), !IO),
    !:DeclMarkers = !.DeclMarkers ++ cord.from_list(DeclMarkers),
    !:ImplMarkers = !.ImplMarkers ++ cord.from_list(ImplMarkers),
    % The type specialization pragmas for exported preds should
    % already be in the interface file.
    pred_info_get_clauses_info(PredInfo, ClausesInfo),
    clauses_info_get_var_table(ClausesInfo, VarTable),
    clauses_info_get_headvar_list(ClausesInfo, HeadVars),
    clauses_info_get_clauses_rep(ClausesInfo, ClausesRep, _ItemNumbers),
    get_clause_list_maybe_repeated(ClausesRep, Clauses),

    pred_info_get_goal_type(PredInfo, GoalType),
    pred_info_get_typevarset(PredInfo, TVarSet),
    (
        GoalType = goal_for_promise(PromiseType),
        (
            Clauses = [Clause],
            write_promise(OutInfo, Stream, ModuleInfo, TVarSet, VarTable,
                PromiseType, HeadVars, Clause, !IO)
        ;
            ( Clauses = []
            ; Clauses = [_, _ | _]
            ),
            unexpected($pred, "assertion not a single clause.")
        )
    ;
        GoalType = goal_not_for_promise(_),
        TypeQual = tvarset_var_table(TVarSet, VarTable),
        list.foldl(
            intermod_write_clause(OutInfo, Stream, ModuleInfo, PredId,
                PredSymName, PredOrFunc, VarTable, TypeQual, HeadVars),
            Clauses, !IO)
    ).

:- pred write_promise(hlds_out_info::in, io.text_output_stream::in,
    module_info::in, tvarset::in, var_table::in, promise_type::in,
    list(prog_var)::in, clause::in, io::di, io::uo) is det.

write_promise(Info, Stream, ModuleInfo, TVarSet, VarTable, PromiseType,
        HeadVars, Clause, !IO) :-
    % Please *either* keep this code in sync with mercury_output_item_promise
    % in parse_tree_out.m, *or* rewrite it to forward the work to that
    % predicate.
    HeadVarStrs = list.map(var_table_entry_name(VarTable), HeadVars),
    HeadVarsStr = string.join_list(", ", HeadVarStrs),
    % Print initial formatting differently for assertions.
    (
        PromiseType = promise_type_true,
        io.format(Stream, ":- promise all [%s] (\n", [s(HeadVarsStr)], !IO)
    ;
        ( PromiseType = promise_type_exclusive
        ; PromiseType = promise_type_exhaustive
        ; PromiseType = promise_type_exclusive_exhaustive
        ),
        io.format(Stream, ":- all [%s] %s\n(\n",
            [s(HeadVarsStr), s(promise_to_string(PromiseType))], !IO)
    ),
    Goal = Clause ^ clause_body,
    varset.init(InstVarSet),
    InfoGoal = hlds_out_info_goal(Info, ModuleInfo, 
        vns_var_table(VarTable), print_name_only,
        TVarSet, InstVarSet, no_tvarset_var_table),
    do_write_goal(InfoGoal, Stream, 1, "\n).\n", Goal, !IO).

:- pred intermod_write_clause(hlds_out_info::in, io.text_output_stream::in,
    module_info::in, pred_id::in, sym_name::in, pred_or_func::in,
    var_table::in, type_qual::in, list(prog_var)::in, clause::in,
    io::di, io::uo) is det.

intermod_write_clause(OutInfo, Stream, ModuleInfo, PredId, SymName, PredOrFunc,
        VarTable, TypeQual, HeadVars, Clause0, !IO) :-
    Clause0 = clause(ApplicableProcIds, Goal, ImplLang, _, _),
    (
        ImplLang = impl_lang_mercury,
        strip_headvar_unifications(HeadVars, Clause0, ClauseHeadVars, Clause),
        % Variable numbers need to be used for the case where the added
        % arguments for a DCG pred expression are named the same
        % as variables in the enclosing clause.
        %
        % We don't need the actual names, and including them in the .opt file
        % would lead to unnecessary recompilations when the *only* changes
        % in a .opt file are changes in variable variables.
        %
        % We could standardize the variables in the clause before printing
        % it out, numbering them e.g. in the order of their appearance,
        % so that changes in variable *numbers* don't cause recompilations
        % either. However, the variable numbers *are* initially allocated
        % in such an order, both by the code that reads in terms and the
        % code that converts parse tree goals into HLDS goals, so this is
        % not likely to be necessary, while its cost may be non-negligible.
        init_var_table(EmptyVarTable),
        write_clause(OutInfo, Stream, output_mercury, ModuleInfo,
            PredId, PredOrFunc, vns_var_table(EmptyVarTable), TypeQual,
            print_name_and_num, write_declared_modes, 1, ClauseHeadVars,
            Clause, !IO)
    ;
        ImplLang = impl_lang_foreign(_),
        module_info_pred_info(ModuleInfo, PredId, PredInfo),
        pred_info_get_proc_table(PredInfo, Procs),
        ( if
            (
                % Pull the foreign code out of the goal.
                Goal = hlds_goal(conj(plain_conj, Goals), _),
                list.filter(
                    ( pred(G::in) is semidet :-
                        G = hlds_goal(GE, _),
                        GE = call_foreign_proc(_, _, _, _, _, _, _)
                    ), Goals, [ForeignCodeGoal]),
                ForeignCodeGoal = hlds_goal(ForeignCodeGoalExpr, _),
                ForeignCodeGoalExpr = call_foreign_proc(Attributes, _, _,
                    Args, _ExtraArgs, _MaybeTraceRuntimeCond, PragmaCode)
            ;
                Goal = hlds_goal(GoalExpr, _),
                GoalExpr = call_foreign_proc(Attributes, _, _,
                    Args, _ExtraArgs, _MaybeTraceRuntimeCond, PragmaCode)
            )
        then
            (
                ApplicableProcIds = all_modes,
                unexpected($pred, "all_modes foreign_proc")
            ;
                ApplicableProcIds = selected_modes(ProcIds),
                list.foldl(
                    intermod_write_foreign_clause(Stream, Procs, PredOrFunc,
                        VarTable, PragmaCode, Attributes, Args, SymName),
                    ProcIds, !IO)
            ;
                ( ApplicableProcIds = unify_in_in_modes
                ; ApplicableProcIds = unify_non_in_in_modes
                ),
                unexpected($pred, "unify modes foreign_proc")
            )
        else
            unexpected($pred, "did not find foreign_proc")
        )
    ).

    % Strip the `Headvar.n = Term' unifications from each clause,
    % except if the `Term' is a lambda expression.
    %
    % At least two problems occur if this is not done:
    %
    % - in some cases where nested unique modes were accepted by mode analysis,
    %   the extra aliasing added by the extra level of headvar unifications
    %   caused mode analysis to report an error (ground expected unique),
    %   when analysing the clauses read in from `.opt' files.
    %
    % - only HeadVar unifications may be reordered with impure goals,
    %   so a mode error results for the second level of headvar unifications
    %   added when the clauses are read in again from the `.opt' file.
    %   Clauses containing impure goals are not written to the `.opt' file
    %   for this reason.
    %
:- pred strip_headvar_unifications(list(prog_var)::in,
    clause::in, list(prog_term)::out, clause::out) is det.

strip_headvar_unifications(HeadVars, Clause0, HeadTerms, Clause) :-
    Goal0 = Clause0 ^ clause_body,
    Goal0 = hlds_goal(_, GoalInfo0),
    goal_to_conj_list(Goal0, Goals0),
    map.init(HeadVarMap0),
    ( if
        strip_headvar_unifications_from_goal_list(Goals0, HeadVars,
            [], Goals, HeadVarMap0, HeadVarMap)
    then
        list.map(
            ( pred(HeadVar0::in, HeadTerm::out) is det :-
                ( if map.search(HeadVarMap, HeadVar0, HeadTerm0) then
                    HeadTerm = HeadTerm0
                else
                    Context = Clause0 ^ clause_context,
                    HeadTerm = term.variable(HeadVar0, Context)
                )
            ), HeadVars, HeadTerms),
        conj_list_to_goal(Goals, GoalInfo0, Goal),
        Clause = Clause0 ^ clause_body := Goal
    else
        term_subst.var_list_to_term_list(HeadVars, HeadTerms),
        Clause = Clause0
    ).

:- pred strip_headvar_unifications_from_goal_list(list(hlds_goal)::in,
    list(prog_var)::in, list(hlds_goal)::in, list(hlds_goal)::out,
    map(prog_var, prog_term)::in,
    map(prog_var, prog_term)::out) is semidet.

strip_headvar_unifications_from_goal_list([], _, RevGoals, Goals,
        !HeadVarMap) :-
    list.reverse(RevGoals, Goals).
strip_headvar_unifications_from_goal_list([Goal | Goals0], HeadVars,
        RevGoals0, Goals, !HeadVarMap) :-
    ( if
        Goal = hlds_goal(unify(LHSVar, RHS, _, _, _), _),
        list.member(LHSVar, HeadVars),
        Context = dummy_context,
        (
            RHS = rhs_var(RHSVar),
            RHSTerm = term.variable(RHSVar, Context)
        ;
            RHS = rhs_functor(ConsId, _, Args),
            require_complete_switch [ConsId]
            (
                ConsId = some_int_const(IntConst),
                RHSTerm = int_const_to_decimal_term(IntConst, Context)
            ;
                ConsId = float_const(Float),
                RHSTerm = term.functor(term.float(Float), [], Context)
            ;
                ConsId = char_const(Char),
                RHSTerm = term.functor(term.atom(string.from_char(Char)),
                    [], Context)
            ;
                ConsId = string_const(String),
                RHSTerm = term.functor(term.string(String), [], Context)
            ;
                ConsId = cons(SymName, _, _),
                term_subst.var_list_to_term_list(Args, ArgTerms),
                construct_qualified_term(SymName, ArgTerms, RHSTerm)
            ;
                ( ConsId = base_typeclass_info_const(_, _, _, _)
                ; ConsId = closure_cons(_, _)
                ; ConsId = deep_profiling_proc_layout(_)
                ; ConsId = ground_term_const(_, _)
                ; ConsId = tabling_info_const(_)
                ; ConsId = impl_defined_const(_)
                ; ConsId = table_io_entry_desc(_)
                ; ConsId = tuple_cons(_)
                ; ConsId = type_ctor_info_const(_, _, _)
                ; ConsId = type_info_cell_constructor(_)
                ; ConsId = typeclass_info_cell_constructor
                ; ConsId = type_info_const(_)
                ; ConsId = typeclass_info_const(_)
                ),
                fail
            )
        ;
            RHS = rhs_lambda_goal(_, _, _, _, _, _, _, _),
            fail
        )
    then
        % Don't strip the headvar unifications if one of the headvars
        % appears twice. This should probably never happen.
        map.insert(LHSVar, RHSTerm, !HeadVarMap),
        RevGoals1 = RevGoals0
    else
        RevGoals1 = [Goal | RevGoals0]
    ),
    strip_headvar_unifications_from_goal_list(Goals0, HeadVars,
        RevGoals1, Goals, !HeadVarMap).

:- pred intermod_write_foreign_clause(io.text_output_stream::in,
    proc_table::in, pred_or_func::in, var_table::in,
    pragma_foreign_proc_impl::in, foreign_proc_attributes::in,
    list(foreign_arg)::in, sym_name::in, proc_id::in,
    io::di, io::uo) is det.

intermod_write_foreign_clause(Stream, Procs, PredOrFunc, VarTable0, PragmaImpl,
        Attributes, Args, SymName, ProcId, !IO) :-
    map.lookup(Procs, ProcId, ProcInfo),
    proc_info_get_maybe_declared_argmodes(ProcInfo, MaybeArgModes),
    (
        MaybeArgModes = yes(ArgModes),
        get_pragma_foreign_code_vars(Args, ArgModes, PragmaVars,
            VarTable0, VarTable),
        proc_info_get_inst_varset(ProcInfo, InstVarSet),
        split_var_table(VarTable, ProgVarSet, _VarTypes),
        FPInfo = item_foreign_proc_info(Attributes, SymName,
            PredOrFunc, PragmaVars, ProgVarSet, InstVarSet, PragmaImpl,
            term_context.dummy_context, item_no_seq_num),
        mercury_output_item_foreign_proc(Stream, output_mercury, FPInfo, !IO)
    ;
        MaybeArgModes = no,
        unexpected($pred, "no mode declaration")
    ).

:- pred get_pragma_foreign_code_vars(list(foreign_arg)::in, list(mer_mode)::in,
    list(pragma_var)::out, var_table::in, var_table::out) is det.

get_pragma_foreign_code_vars(Args, Modes, PragmaVars, !VarTable) :-
    (
        Args = [Arg | ArgsTail],
        Modes = [Mode | ModesTail],
        Arg = foreign_arg(Var, MaybeNameAndMode, _, _),
        (
            MaybeNameAndMode = no,
            Name = "_"
        ;
            MaybeNameAndMode = yes(foreign_arg_name_mode(Name, _Mode2))
        ),
        PragmaVar = pragma_var(Var, Name, Mode, bp_native_if_possible),
        update_var_name(Var, Name, !VarTable),
        get_pragma_foreign_code_vars(ArgsTail, ModesTail, PragmaVarsTail,
            !VarTable),
        PragmaVars = [PragmaVar | PragmaVarsTail]
    ;
        Args = [],
        Modes = [],
        PragmaVars = []
    ;
        Args = [],
        Modes = [_ | _],
        unexpected($pred, "list length mismatch")
    ;
        Args = [_ | _],
        Modes = [],
        unexpected($pred, "list length mismatch")
    ).

%---------------------------------------------------------------------------%
:- end_module transform_hlds.intermod.
%---------------------------------------------------------------------------%
