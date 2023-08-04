%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2015-2016, 2018-2021 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: comp_unit_interface.m.
% Authors: fjh (original version), zs (current version).
%
% Given the raw compilation unit of a module, extract the part of that module
% that will go into the .int file of the module.
%
%---------------------------------------------------------------------------%

:- module parse_tree.comp_unit_interface.
:- interface.

:- import_module libs.
:- import_module libs.globals.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.prog_item.

:- import_module list.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % This qualifies everything as much as it can given the information
    % in the current module and writes out the .int3 file.
    % XXX document me better
    %
:- pred generate_short_interface_int3(globals::in, parse_tree_module_src::in,
    parse_tree_int3::out, list(error_spec)::in, list(error_spec)::out) is det.

%---------------------------------------------------------------------------%

    % generate_private_interface_int0(AugMakeIntUnit, ParseTreeInt0):
    %
    % Generate the private interface of a module (its .int0 file), which
    % makes available some not-generally-available items to the other modules
    % nested inside it.
    %
:- pred generate_private_interface_int0(aug_make_int_unit::in,
    parse_tree_int0::out, list(error_spec)::in, list(error_spec)::out) is det.

%---------------------------------------------------------------------------%

    % generate_pre_grab_pre_qual_interface_for_int1_int2(ParseTreeModuleSrc,
    %   IntParseTreeModuleSrc):
    %
    % Prepare for the generation of .int and .int2 files by generating
    % the part of the module's parse tree that needs to be module qualified
    % before the invocation of generate_interfaces_int1_int2.
    %
    % We return interface sections almost intact, changing them only by
    % making instance declarations abstract. We delete most kinds of items
    % from implementation sections, keeping only
    %
    % - Module includes.
    %
    % - Module imports and uses.
    %
    % - Type definitions, in a possibly changed form. Specifically,
    %   we replace the definitions (a) solver types and (b) noncanonical
    %   du and foreign types with their abstract forms. We leave the
    %   definitions of all other types (canonical du and foreign types,
    %   equivalence types, and already abtract types) unchanged.
    %
    % - Typeclass declarations in their abstract from.
    %
    % - Foreign_enum pragmas.
    %
    % - Foreign_import_module declarations.
    %
    % XXX ITEM_LIST Document why we do all this *before* module qualification.
    %
    % XXX ITEM_LIST The original comment on this predicate,
    % when it was conjoined with the code of get_interface above, was:
    % "Given the raw compilation unit of a module, extract and return
    % the part of that module that will go into the .int file of the module.
    % This will typically mostly be the interface section of the module,
    % but it may also contain parts of the implementation section as well.
    % Both parts may be somewhat modified; for example, we may remove
    % the bodies of instance definitions in an interface section,
    % but put the original, non-abstract instance definition in the
    % implementation section."
    %
:- pred generate_pre_grab_pre_qual_interface_for_int1_int2(
    parse_tree_module_src::in, parse_tree_module_src::out) is det.

    % Generate the contents for the .int and .int2 files.
    %
:- pred generate_interfaces_int1_int2(globals::in, aug_make_int_unit::in,
    parse_tree_int1::out, parse_tree_int2::out,
    list(error_spec)::in, list(error_spec)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.check_type_inst_mode_defns.
:- import_module parse_tree.convert_parse_tree.
:- import_module parse_tree.decide_type_repn.
:- import_module parse_tree.item_util.
:- import_module parse_tree.module_qual.
:- import_module parse_tree.parse_tree_out.
:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_foreign.
:- import_module parse_tree.prog_mutable.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_subst.

:- import_module bool.
:- import_module cord.
:- import_module map.
:- import_module maybe.
:- import_module one_or_more.
:- import_module one_or_more_map.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module set_tree234.
:- import_module term.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

generate_short_interface_int3(Globals, ParseTreeModuleSrc, ParseTreeInt3,
        !Specs) :-
    ParseTreeModuleSrc = parse_tree_module_src(ModuleName, ModuleNameContext,
        OrigInclMap, OrigImportUseMap,
        _IntFIMSpecMap, _ImpFIMSpecMap, _IntSelfFIMLangs, _ImpSelfFIMLangs,

        TypeCtorCheckedMap, InstCtorCheckedMap, ModeCtorCheckedMap,
        _TypeSpecs, _InstModeSpecs,

        OrigIntTypeClasses, OrigIntInstances, _IntPredDecls, _IntModeDecls,
        _IntDeclPragmas, _IntPromises, _IntBadClauses,

        _ImpTypeClasses, _ImpInstances, _ImpPredDecls, _ImpModeDecls,
        _ImpClauses, _ImpForeignProcs, _ImpForeignExportEnums,
        _ImpDeclPragmas, _ImpImplPragmas, _ImpPromises,
        _ImpInitialises, _ImpFinalises, _ImpMutables),

    map.foldl(add_only_int_include, OrigInclMap, map.init, IntInclMap),
    IntTypeClasses = list.map(make_typeclass_abstract_for_int3,
        OrigIntTypeClasses),
    IntInstances = list.map(make_instance_abstract, OrigIntInstances),
    (
        IntInstances = [],
        map.init(IntImportMap)
    ;
        IntInstances = [_ | _],
        map.foldl(acc_int_imports, OrigImportUseMap, map.init, IntImportMap)
    ),
    map.foldl(make_type_ctor_checked_defn_abstract_for_int3,
        TypeCtorCheckedMap, map.init, IntTypeCtorCheckedMap),
    map.foldl(make_inst_ctor_checked_defn_abstract_for_int3,
        InstCtorCheckedMap, map.init, IntInstCtorCheckedMap),
    map.foldl(make_mode_ctor_checked_defn_abstract_for_int3,
        ModeCtorCheckedMap, map.init, IntModeCtorCheckedMap),

    decide_repns_for_simple_types_for_int3(ModuleName, TypeCtorCheckedMap,
        IntTypeRepnMap),
    OrigParseTreeInt3 = parse_tree_int3(ModuleName, ModuleNameContext,
        IntInclMap, IntImportMap,
        IntTypeCtorCheckedMap, IntInstCtorCheckedMap, IntModeCtorCheckedMap,
        IntTypeClasses, IntInstances, IntTypeRepnMap),
    % Any Specs this can generate would be better reported
    % when the module is being compiled to target language code.
    module_qualify_parse_tree_int3(Globals, OrigParseTreeInt3, ParseTreeInt3,
        [], _Specs).

:- pred acc_int_imports(module_name::in, maybe_implicit_import_and_or_use::in,
    int_import_map::in, int_import_map::out) is det.

acc_int_imports(ModuleName, ImportUseInfo, !ContextMap) :-
    (
        ImportUseInfo = implicit_avail(_, MaybeSectionImportAndOrUse),
        (
            MaybeSectionImportAndOrUse = no
        ;
            MaybeSectionImportAndOrUse = yes(SectionImportAndOrUse),
            (
                SectionImportAndOrUse = int_import(Context),
                map.det_insert(ModuleName, int_import(Context), !ContextMap)
            ;
                ( SectionImportAndOrUse = int_use(_)
                ; SectionImportAndOrUse = imp_import(_)
                ; SectionImportAndOrUse = imp_use(_)
                ; SectionImportAndOrUse = int_use_imp_import(_, _)
                )
            )
        )
    ;
        ImportUseInfo = explicit_avail(SectionImportAndOrUse),
        (
            SectionImportAndOrUse = int_import(Context),
            map.det_insert(ModuleName, int_import(Context), !ContextMap)
        ;
            ( SectionImportAndOrUse = int_use(_)
            ; SectionImportAndOrUse = imp_import(_)
            ; SectionImportAndOrUse = imp_use(_)
            ; SectionImportAndOrUse = int_use_imp_import(_, _)
            )
        )
    ).

%---------------------------------------------------------------------------%

:- pred make_type_ctor_checked_defn_abstract_for_int3(
    type_ctor::in, type_ctor_checked_defn::in,
    type_ctor_checked_map::in, type_ctor_checked_map::out) is det.

make_type_ctor_checked_defn_abstract_for_int3(TypeCtor, CheckedTypeDefn0,
        !CheckedTypeMap) :-
    (
        CheckedTypeDefn0 = checked_defn_solver(SolverDefn0, _SrcDefns0),
        ( if
            (
                SolverDefn0 = solver_type_abstract(AbstractStatus,
                    AbstractSolverDefn),
                AbstractStatus = abstract_solver_type_exported
            ;
                SolverDefn0 = solver_type_full(MaybeAbstractSolverDefn,
                    _ActualSolverDefn),
                MaybeAbstractSolverDefn = yes(AbstractSolverDefn)
            )
        then
            SolverDefn = solver_type_abstract(abstract_solver_type_exported,
                AbstractSolverDefn),
            IntDefn = wrap_abstract_type_defn(AbstractSolverDefn),
            SrcDefns = src_defns_solver(yes(IntDefn), no),
            CheckedTypeDefn = checked_defn_solver(SolverDefn, SrcDefns),
            map.det_insert(TypeCtor, CheckedTypeDefn, !CheckedTypeMap)
        else
            true
        )
    ;
        CheckedTypeDefn0 = checked_defn_std(StdDefn0, _SrcDefns0),
        (
            StdDefn0 = std_mer_type_eqv(EqvStatus, EqvDefn0),
            (
                ( EqvStatus = std_eqv_type_mer_exported
                ; EqvStatus = std_eqv_type_abstract_exported
                ),
                AbsStatus = std_abs_type_abstract_exported,
                % XXX Is this right for solver types?
                % XXX TYPE_REPN Is this right for types that are eqv to enums,
                % or to known size ints/uints?
                DetailsAbstract = abstract_type_general,
                AbsDefn = EqvDefn0 ^ td_ctor_defn := DetailsAbstract,
                CSCsMaybeDefn = c_java_csharp(no, no, no),
                StdDefn = std_mer_type_abstract(AbsStatus, AbsDefn,
                    CSCsMaybeDefn),
                IntDefn = wrap_abstract_type_defn(AbsDefn),
                SrcDefns = src_defns_std([IntDefn], [], []),
                CheckedTypeDefn = checked_defn_std(StdDefn, SrcDefns),
                map.det_insert(TypeCtor, CheckedTypeDefn, !CheckedTypeMap)
            ;
                EqvStatus = std_eqv_type_all_private
            )
        ;
            StdDefn0 = std_mer_type_subtype(SubStatus, SubDefn0),
            (
                ( SubStatus = std_sub_type_mer_exported
                ; SubStatus = std_sub_type_abstract_exported
                ),
                AbsStatus = std_abs_type_abstract_exported,
                DetailsSub = SubDefn0 ^ td_ctor_defn,
                make_sub_type_abstract(DetailsSub, DetailsAbstract),
                AbsDefn = SubDefn0 ^ td_ctor_defn := DetailsAbstract,
                CJCsMaybeDefn = c_java_csharp(no, no, no),
                StdDefn = std_mer_type_abstract(AbsStatus, AbsDefn,
                    CJCsMaybeDefn),
                IntDefn = wrap_abstract_type_defn(AbsDefn),
                SrcDefns = src_defns_std([IntDefn], [], []),
                CheckedTypeDefn = checked_defn_std(StdDefn, SrcDefns),
                map.det_insert(TypeCtor, CheckedTypeDefn, !CheckedTypeMap)
            ;
                SubStatus = std_sub_type_all_private
            )
        ;
            (
                StdDefn0 = std_mer_type_du_all_plain_constants(DuStatus,
                    DuDefn0, _HeadCtor, _TailCtors, CJCsMaybeDefnOrEnum),
                CJCsMaybeDefnOrEnum = c_java_csharp(MaybeDefnOrEnumC,
                    MaybeDefnOrEnumJava, MaybeDefnOrEnumCsharp),
                GetForeignTypeOnly =
                    ( pred(MaybeDorE::in, MaybeFT::out) is det :-
                        (
                            MaybeDorE = no,
                            MaybeFT = no
                        ;
                            MaybeDorE = yes(DorE),
                            (
                                DorE = foreign_type_or_enum_enum(_),
                                MaybeFT = no
                            ;
                                DorE = foreign_type_or_enum_type(FT),
                                MaybeFT = yes(FT)
                            )
                        )
                    ),
                GetForeignTypeOnly(MaybeDefnOrEnumC, MaybeDefnC0),
                GetForeignTypeOnly(MaybeDefnOrEnumJava, MaybeDefnJava0),
                GetForeignTypeOnly(MaybeDefnOrEnumCsharp, MaybeDefnCsharp0),
                CJCsMaybeDefn0 = c_java_csharp(MaybeDefnC0, MaybeDefnJava0,
                    MaybeDefnCsharp0)
            ;
                StdDefn0 = std_mer_type_du_not_all_plain_constants(DuStatus,
                    DuDefn0, CJCsMaybeDefn0)
            ),
            (
                ( DuStatus = std_du_type_mer_ft_exported
                ; DuStatus = std_du_type_mer_exported
                ; DuStatus = std_du_type_abstract_exported
                ),
                DetailsDu = DuDefn0 ^ td_ctor_defn,
                (
                    DuStatus = std_du_type_mer_ft_exported,
                    AbsStatus = std_abs_type_ft_exported,
                    make_du_type_abstract(DetailsDu, DetailsAbstract),
                    CJCsMaybeDefn = CJCsMaybeDefn0,
                    get_c_j_cs_defns(CJCsMaybeDefn, CJCsDefns),
                    IntCJCsDefns = list.map(wrap_foreign_type_defn, CJCsDefns)
                ;
                    DuStatus = std_du_type_mer_exported,
                    AbsStatus = std_abs_type_abstract_exported,
                    make_du_type_abstract(DetailsDu, DetailsAbstract),
                    CJCsMaybeDefn = c_java_csharp(no, no, no),
                    IntCJCsDefns = []
                ;
                    DuStatus = std_du_type_abstract_exported,
                    AbsStatus = std_abs_type_abstract_exported,
                    % XXX We *could* use the DetailsAbstract value computed by
                    % make_du_type_abstract in this case as well as in
                    % all the other cases. The difference would be that
                    % this *could* add to the .int3 file we are generating
                    % information about TypeCtor being a direct dummy type,
                    % a notag type, or an enum type, in the form of e.g.
                    % a "where type_is_abstract_enum(N)" clause in the type
                    % declaration.
                    DetailsAbstract = abstract_type_general,
                    CJCsMaybeDefn = c_java_csharp(no, no, no),
                    IntCJCsDefns = []
                ),
                AbsDefn = DuDefn0 ^ td_ctor_defn := DetailsAbstract,
                StdDefn = std_mer_type_abstract(AbsStatus, AbsDefn,
                    CJCsMaybeDefn),
                IntDefn = wrap_abstract_type_defn(AbsDefn),
                SrcDefns = src_defns_std([IntDefn | IntCJCsDefns], [], []),
                CheckedTypeDefn = checked_defn_std(StdDefn, SrcDefns),
                map.det_insert(TypeCtor, CheckedTypeDefn, !CheckedTypeMap)
            ;
                DuStatus = std_du_type_all_private
            )
        ;
            StdDefn0 = std_mer_type_abstract(AbsStatus, AbsDefn,
                CJCsMaybeDefn0),
            (
                (
                    AbsStatus = std_abs_type_ft_exported,
                    CJCsMaybeDefn = CJCsMaybeDefn0
                ;
                    AbsStatus = std_abs_type_abstract_exported,
                    CJCsMaybeDefn = c_java_csharp(no, no, no)
                ),
                get_c_j_cs_defns(CJCsMaybeDefn, CJCsDefns),
                IntCJCsDefns = list.map(wrap_foreign_type_defn, CJCsDefns),
                StdDefn = std_mer_type_abstract(AbsStatus, AbsDefn,
                    CJCsMaybeDefn),
                IntDefn = wrap_abstract_type_defn(AbsDefn),
                SrcDefns = src_defns_std([IntDefn | IntCJCsDefns], [], []),
                CheckedTypeDefn = checked_defn_std(StdDefn, SrcDefns),
                map.det_insert(TypeCtor, CheckedTypeDefn, !CheckedTypeMap)
            ;
                AbsStatus = std_abs_type_all_private
            )
        )
    ).

:- pred get_c_j_cs_defns(c_j_cs_maybe_defn::in,
    list(item_type_defn_info_foreign)::out) is det.

get_c_j_cs_defns(CJCsMaybeDefn, CJCsDefns) :-
    CJCsMaybeDefn = c_java_csharp(MaybeDefnC, MaybeDefnJava, MaybeDefnCsharp),
    MaybeToList =
        ( pred(MaybeDefn::in, Defns::out) is det :-
            (
                MaybeDefn = no,
                Defns = []
            ;
                MaybeDefn = yes(Defn),
                Defns = [Defn]
            )
        ),
    MaybeToList(MaybeDefnC, DefnsC),
    MaybeToList(MaybeDefnJava, DefnsJava),
    MaybeToList(MaybeDefnCsharp, DefnsCsharp),
    CJCsDefns = DefnsC ++ DefnsJava ++ DefnsCsharp.

:- pred make_inst_ctor_checked_defn_abstract_for_int3(
    inst_ctor::in, inst_ctor_checked_defn::in,
    inst_ctor_checked_map::in, inst_ctor_checked_map::out) is det.

make_inst_ctor_checked_defn_abstract_for_int3(InstCtor, CheckedInstDefn0,
        !CheckedInstMap) :-
    CheckedInstDefn0 = checked_defn_inst(StdDefn0, SrcDefns0),
    StdDefn0 = std_inst_defn(Status0, MaybeAbstractDefn),
    (
        ( Status0 = std_inst_exported
        ; Status0 = std_inst_abstract_exported
        ),
        Status = std_inst_abstract_exported,
        AbstractDefn = MaybeAbstractDefn ^ id_inst_defn := abstract_inst_defn,
        StdDefn = std_inst_defn(Status, AbstractDefn),
        SrcDefns0 = src_defns_inst(MaybeIntDefn0, _MaybeImpDefn0),
        MaybeIntDefn = map_maybe(make_inst_defn_abstract, MaybeIntDefn0),
        SrcDefns = src_defns_inst(MaybeIntDefn, no),
        CheckedInstDefn = checked_defn_inst(StdDefn, SrcDefns),
        map.det_insert(InstCtor, CheckedInstDefn, !CheckedInstMap)
    ;
        Status0 = std_inst_all_private
    ).

:- pred make_mode_ctor_checked_defn_abstract_for_int3(
    mode_ctor::in, mode_ctor_checked_defn::in,
    mode_ctor_checked_map::in, mode_ctor_checked_map::out) is det.

make_mode_ctor_checked_defn_abstract_for_int3(ModeCtor, CheckedModeDefn0,
        !CheckedModeMap) :-
    CheckedModeDefn0 = checked_defn_mode(StdDefn0, SrcDefns0),
    StdDefn0 = std_mode_defn(Status0, MaybeAbstractDefn),
    (
        ( Status0 = std_mode_exported
        ; Status0 = std_mode_abstract_exported
        ),
        Status = std_mode_abstract_exported,
        AbstractDefn = MaybeAbstractDefn ^ md_mode_defn := abstract_mode_defn,
        StdDefn = std_mode_defn(Status, AbstractDefn),
        SrcDefns0 = src_defns_mode(MaybeIntDefn0, _MaybeImpDefn0),
        MaybeIntDefn = map_maybe(make_mode_defn_abstract, MaybeIntDefn0),
        SrcDefns = src_defns_mode(MaybeIntDefn, no),
        CheckedModeDefn = checked_defn_mode(StdDefn, SrcDefns),
        map.det_insert(ModeCtor, CheckedModeDefn, !CheckedModeMap)
    ;
        Status0 = std_mode_all_private
    ).

%---------------------------------------------------------------------------%

:- func make_typeclass_abstract_for_int3(item_typeclass_info)
    = item_abstract_typeclass_info.

make_typeclass_abstract_for_int3(TypeClass) = AbstractTypeClass :-
    TypeClass = item_typeclass_info(ClassName, ParamsTVars,
        _Constraints, _FunDeps, _Methods, TVarSet, Context, SeqNum),
    AbstractTypeClass = item_typeclass_info(ClassName, ParamsTVars,
        [], [], class_interface_abstract, TVarSet, Context, SeqNum).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

generate_private_interface_int0(AugMakeIntUnit, ParseTreeInt0, !Specs) :-
    AugMakeIntUnit = aug_make_int_unit(ParseTreeModuleSrc, _, _, _,
        ModuleItemVersionNumbersMap),

    ( if map.search(ModuleItemVersionNumbersMap, ModuleName, MIVNs) then
        MaybeVersionNumbers = version_numbers(MIVNs)
    else
        MaybeVersionNumbers = no_version_numbers
    ),

    ParseTreeModuleSrc = parse_tree_module_src(ModuleName, ModuleNameContext,
        InclMap, ImportUseMap,
        IntFIMSpecMap, ImpFIMSpecMap, IntSelfFIMLangs, ImpSelfFIMLangs,

        TypeCtorCheckedMap, InstCtorCheckedMap, ModeCtorCheckedMap,
        _TypeSpecs, _InstModeSpecs,

        IntTypeClasses, IntInstances0, IntPredDecls, IntModeDecls,
        IntDeclPragmas, IntPromises, _IntBadClausePreds,

        ImpTypeClasses, ImpInstances0, ImpPredDecls0, ImpModeDecls,
        _ImpClauses, _ImpForeignProcs, _ImpForeignExportEnums,
        ImpDeclPragmas, _ImpImplPragmas, ImpPromises,
        _ImpInitialises, _ImpFinalises, ImpMutables),

    import_and_or_use_map_to_explicit_int_imp_import_use_maps(ImportUseMap,
        SectionImportUseMap, _, _, _, _),
    map.keys_as_set(IntFIMSpecMap, IntFIMSpecs0),
    map.keys_as_set(ImpFIMSpecMap, ImpFIMSpecs0),
    % Add implicit self FIMs for the {Int,Imp}SelfFIMLangs
    % to their respective sections.
    set.union(
        set.map(fim_module_lang_to_spec(ModuleName), IntSelfFIMLangs),
        IntFIMSpecs0, IntFIMSpecs),
    set.union(
        set.map(fim_module_lang_to_spec(ModuleName), ImpSelfFIMLangs),
        ImpFIMSpecs0, ImpFIMSpecs1),
    % Make the implementation FIMs disjoint from the interface FIMs.
    set.difference(ImpFIMSpecs1, IntFIMSpecs, ImpFIMSpecs),

    OutInfo = init_write_int_merc_out_info,
    IntInstances = list.map(make_instance_abstract, IntInstances0),
    IntInstanceStrs =
        list.map(item_abstract_instance_to_string(OutInfo), IntInstances),
    set_tree234.list_to_set(IntInstanceStrs, IntInstanceStrSet),
    ImpInstances1 = list.map(make_instance_abstract, ImpInstances0),
    KeepImpInstanceTest =
        ( pred(AbsInstance::in) is semidet :-
            Str = item_abstract_instance_to_string(OutInfo, AbsInstance),
            not set_tree234.contains(IntInstanceStrSet, Str)
        ),
    list.filter(KeepImpInstanceTest, ImpInstances1, ImpInstances),

    ImpPredDecls = ImpPredDecls0 ++ list.condense(
        list.map(declare_mutable_aux_preds_for_int0(ModuleName), ImpMutables)),

    ParseTreeInt0 = parse_tree_int0(ModuleName, ModuleNameContext,
        MaybeVersionNumbers, InclMap, SectionImportUseMap,
        IntFIMSpecs, ImpFIMSpecs,
        TypeCtorCheckedMap, InstCtorCheckedMap, ModeCtorCheckedMap,
        IntTypeClasses, IntInstances, IntPredDecls, IntModeDecls,
        IntDeclPragmas, IntPromises,
        ImpTypeClasses, ImpInstances, ImpPredDecls, ImpModeDecls,
        ImpDeclPragmas, ImpPromises).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

generate_pre_grab_pre_qual_interface_for_int1_int2(ParseTreeModuleSrc,
        IntParseTreeModuleSrc) :-
    ParseTreeModuleSrc = parse_tree_module_src(ModuleName,
        ModuleNameContext, InclMap, ImportUseMap,
        IntFIMSpecMap, ImpFIMSpecMap, IntSelfFIMLangs, ImpSelfFIMLangs,

        TypeCtorCheckedMap, InstCtorCheckedMap, ModeCtorCheckedMap,
        TypeSpecs, InstModeSpecs,

        IntTypeClasses, IntInstances, IntPredDecls, IntModeDecls,
        IntDeclPragmas, IntPromises, IntBadClausePreds,

        ImpTypeClasses, _ImpInstances, _ImpPredDecls, _ImpModeDecls,
        _ImpClauses, _ImpForeignProcs, _ImpForeignExportEnums,
        _ImpDeclPragmas, _ImpImplPragmas, _ImpPromises,
        _ImpInitialises, _ImpFinalises, _ImpMutables),

    IntInstancesAbstract = list.map(make_instance_abstract, IntInstances),
    map.map_values_only(pre_grab_pre_qual_type_ctor_checked_defn,
        TypeCtorCheckedMap, IntTypeCtorCheckedMap),
    map.foldl(pre_grab_pre_qual_inst_ctor_checked_defn,
        InstCtorCheckedMap, map.init, IntInstCtorCheckedMap),
    map.foldl(pre_grab_pre_qual_mode_ctor_checked_defn,
        ModeCtorCheckedMap, map.init, IntModeCtorCheckedMap),
    AbstractImpTypeClasses = list.map(make_typeclass_abstract, ImpTypeClasses),

    IntParseTreeModuleSrc = parse_tree_module_src(ModuleName,
        ModuleNameContext, InclMap, ImportUseMap,
        IntFIMSpecMap, ImpFIMSpecMap, IntSelfFIMLangs, ImpSelfFIMLangs,

        IntTypeCtorCheckedMap, IntInstCtorCheckedMap, IntModeCtorCheckedMap,
        TypeSpecs, InstModeSpecs,

        IntTypeClasses, coerce(IntInstancesAbstract),
        IntPredDecls, IntModeDecls, IntDeclPragmas, IntPromises,
        IntBadClausePreds,

        coerce(AbstractImpTypeClasses), [],
        [], [], [], [], [], [], [], [], [], [], []).

    % Keep the interface part of the given type_ctor_checked_defn unchanged,
    % but modify its implementation-section part by
    %
    % - making solver types abstract, and
    %
    % - deleting any user-specified equality and comparison predicates.
    %
:- pred pre_grab_pre_qual_type_ctor_checked_defn(
    type_ctor_checked_defn::in, type_ctor_checked_defn::out) is det.

pre_grab_pre_qual_type_ctor_checked_defn(CheckedDefn0, CheckedDefn) :-
    (
        CheckedDefn0 = checked_defn_solver(SolverDefn0, _SrcDefns0),
        (
            SolverDefn0 = solver_type_abstract(_Status, _Defn0),
            % This solver type has only a declaration. If it is in the
            % interface section, we keep it unchanged because it is there.
            % If it is in the implementation section, we want to keep
            % an abstract version of it, but it already abstract,
            % so we keep in unchanged for that reason.
            CheckedDefn = CheckedDefn0
        ;
            SolverDefn0 = solver_type_full(MaybeAbstractDefn0, FullDefn0),
            % Solver type *definitions* can occur only in implementation
            % sections. This means that
            %
            % - if there is a declaration of the solver type in the interface,
            %   we keep only that declaration;
            %
            % - otherwise, we turn the definition in the implementation section
            %   into a declaration.
            (
                MaybeAbstractDefn0 = yes(AbstractDefn0),
                Status = abstract_solver_type_exported,
                SolverDefn = solver_type_abstract(Status, AbstractDefn0),
                WrapAbstractDefn0 = wrap_abstract_type_defn(AbstractDefn0),
                SrcDefns = src_defns_solver(yes(WrapAbstractDefn0), no)
            ;
                MaybeAbstractDefn0 = no,
                Status = abstract_solver_type_private,
                AbstractDefn = FullDefn0 ^ td_ctor_defn
                    := abstract_solver_type,
                SolverDefn = solver_type_abstract(Status, AbstractDefn),
                WrapAbstractDefn = wrap_abstract_type_defn(AbstractDefn),
                SrcDefns = src_defns_solver(no, yes(WrapAbstractDefn))
            ),
            CheckedDefn = checked_defn_solver(SolverDefn, SrcDefns)
        )
    ;
        CheckedDefn0 = checked_defn_std(StdDefn0, SrcDefns0),
        (
            ( StdDefn0 = std_mer_type_eqv(_Status, _EqvDefn)
            ; StdDefn0 = std_mer_type_subtype(_Status, _SubDefn)
            ),
            % These kinds of types
            % - are not solver types, and
            % - they cannot refer to equality or comparison predicates.
            CheckedDefn = CheckedDefn0
        ;
            StdDefn0 = std_mer_type_du_all_plain_constants(Status,
                DuDefn0, HeadCtor, TailCtors, MaybeCJCsDefnOrEnum0),
            SrcDefns0 = src_defns_std(IntDefns0, ImpDefns0, ImpForeignEnums0),
            (
                Status = std_du_type_mer_ft_exported,
                StdDefn = StdDefn0,
                SrcDefns = SrcDefns0
            ;
                Status = std_du_type_mer_exported,
                delete_uc_preds_from_c_j_cs_maybe_defn_or_enum(
                    MaybeCJCsDefnOrEnum0, MaybeCJCsDefnOrEnum),
                StdDefn = std_mer_type_du_all_plain_constants(Status,
                    DuDefn0, HeadCtor, TailCtors, MaybeCJCsDefnOrEnum),
                list.map(delete_uc_preds_make_solver_type_dummy,
                    ImpDefns0, ImpDefns),
                SrcDefns = src_defns_std(IntDefns0, ImpDefns,
                    ImpForeignEnums0)
            ;
                ( Status = std_du_type_abstract_exported
                ; Status = std_du_type_all_private
                ),
                delete_uc_preds_from_du_type_defn(DuDefn0, DuDefn),
                delete_uc_preds_from_c_j_cs_maybe_defn_or_enum(
                    MaybeCJCsDefnOrEnum0, MaybeCJCsDefnOrEnum),
                StdDefn = std_mer_type_du_all_plain_constants(Status,
                    DuDefn, HeadCtor, TailCtors, MaybeCJCsDefnOrEnum),
                list.map(delete_uc_preds_make_solver_type_dummy,
                    ImpDefns0, ImpDefns),
                SrcDefns = src_defns_std(IntDefns0, ImpDefns,
                    ImpForeignEnums0)
            ),
            CheckedDefn = checked_defn_std(StdDefn, SrcDefns)
        ;
            StdDefn0 = std_mer_type_du_not_all_plain_constants(Status,
                DuDefn0, MaybeCJCsDefn0),
            SrcDefns0 = src_defns_std(IntDefns0, ImpDefns0, ImpForeignEnums0),
            (
                Status = std_du_type_mer_ft_exported,
                StdDefn = StdDefn0,
                SrcDefns = SrcDefns0
            ;
                Status = std_du_type_mer_exported,
                delete_uc_preds_from_c_j_cs_maybe_defn(MaybeCJCsDefn0,
                    MaybeCJCsDefn),
                StdDefn = std_mer_type_du_not_all_plain_constants(Status,
                    DuDefn0, MaybeCJCsDefn),
                list.map(delete_uc_preds_make_solver_type_dummy,
                    ImpDefns0, ImpDefns),
                SrcDefns = src_defns_std(IntDefns0, ImpDefns,
                    ImpForeignEnums0)
            ;
                ( Status = std_du_type_abstract_exported
                ; Status = std_du_type_all_private
                ),
                delete_uc_preds_from_du_type_defn(DuDefn0, DuDefn),
                delete_uc_preds_from_c_j_cs_maybe_defn(MaybeCJCsDefn0,
                    MaybeCJCsDefn),
                StdDefn = std_mer_type_du_not_all_plain_constants(Status,
                    DuDefn, MaybeCJCsDefn),
                list.map(delete_uc_preds_make_solver_type_dummy,
                    ImpDefns0, ImpDefns),
                SrcDefns = src_defns_std(IntDefns0, ImpDefns,
                    ImpForeignEnums0)
            ),
            CheckedDefn = checked_defn_std(StdDefn, SrcDefns)
        ;
            StdDefn0 = std_mer_type_abstract(Status, AbsDefn,
                MaybeCJCsDefn0),
            (
                Status = std_abs_type_ft_exported,
                StdDefn = StdDefn0,
                SrcDefns = SrcDefns0
            ;
                ( Status = std_abs_type_abstract_exported
                ; Status = std_abs_type_all_private
                ),
                delete_uc_preds_from_c_j_cs_maybe_defn(MaybeCJCsDefn0,
                    MaybeCJCsDefn),
                StdDefn = std_mer_type_abstract(Status, AbsDefn,
                    MaybeCJCsDefn),
                list.map(delete_uc_preds_make_solver_type_dummy,
                    ImpDefns0, ImpDefns),
                SrcDefns0 = src_defns_std(IntDefns0, ImpDefns0,
                    ImpForeignEnums0),
                SrcDefns = src_defns_std(IntDefns0, ImpDefns,
                    ImpForeignEnums0)
            ),
            CheckedDefn = checked_defn_std(StdDefn, SrcDefns)
        )
    ).

    % Keep only the part of the inst_ctor_checked_defn
    % that is in the interface section.
    %
:- pred pre_grab_pre_qual_inst_ctor_checked_defn(inst_ctor::in,
    inst_ctor_checked_defn::in,
    inst_ctor_checked_map::in, inst_ctor_checked_map::out) is det.

pre_grab_pre_qual_inst_ctor_checked_defn(InstCtor, CheckedDefn0,
        !InstCtorCheckedMap) :-
    CheckedDefn0 = checked_defn_inst(StdDefn0, SrcDefns0),
    StdDefn0 = std_inst_defn(Status, _Defn0),
    SrcDefns0 = src_defns_inst(MaybeIntDefn, MaybeImpDefn),
    (
        Status = std_inst_exported,
        expect(unify(MaybeImpDefn, no), $pred, "exported but has imp defn"),
        map.det_insert(InstCtor, CheckedDefn0, !InstCtorCheckedMap)
    ;
        Status = std_inst_abstract_exported,
        (
            MaybeIntDefn = yes(IntDefn),
            StdDefn = std_inst_defn(Status, IntDefn),
            SrcDefns = src_defns_inst(MaybeIntDefn, no),
            CheckedDefn = checked_defn_inst(StdDefn, SrcDefns),
            map.det_insert(InstCtor, CheckedDefn, !InstCtorCheckedMap)
        ;
            MaybeIntDefn = no,
            unexpected($pred, "std_inst_abstract_exported but no int defn")
        )
    ;
        Status = std_inst_all_private
        % We do not put any checked definition into !InstCtorCheckedMap.
    ).

    % Keep only the part of the mode_ctor_checked_defn
    % that is in the interface section.
    %
:- pred pre_grab_pre_qual_mode_ctor_checked_defn(mode_ctor::in,
    mode_ctor_checked_defn::in,
    mode_ctor_checked_map::in, mode_ctor_checked_map::out) is det.

pre_grab_pre_qual_mode_ctor_checked_defn(ModeCtor, CheckedDefn0,
        !ModeCtorCheckedMap) :-
    CheckedDefn0 = checked_defn_mode(StdDefn0, SrcDefns0),
    StdDefn0 = std_mode_defn(Status, _Defn0),
    SrcDefns0 = src_defns_mode(MaybeIntDefn, MaybeImpDefn),
    (
        Status = std_mode_exported,
        expect(unify(MaybeImpDefn, no), $pred, "exported but has imp defn"),
        map.det_insert(ModeCtor, CheckedDefn0, !ModeCtorCheckedMap)
    ;
        Status = std_mode_abstract_exported,
        (
            MaybeIntDefn = yes(IntDefn),
            StdDefn = std_mode_defn(Status, IntDefn),
            SrcDefns = src_defns_mode(MaybeIntDefn, no),
            CheckedDefn = checked_defn_mode(StdDefn, SrcDefns),
            map.det_insert(ModeCtor, CheckedDefn, !ModeCtorCheckedMap)
        ;
            MaybeIntDefn = no,
            unexpected($pred, "std_mode_abstract_exported but no int defn")
        )
    ;
        Status = std_mode_all_private
        % We do not put any checked definition into !ModeCtorCheckedMap.
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

generate_interfaces_int1_int2(Globals, AugMakeIntUnit,
        ParseTreeInt1, ParseTreeInt2, !Specs) :-
    generate_interface_int1(Globals, AugMakeIntUnit,
        IntExplicitFIMSpecs, ImpExplicitFIMSpecs,
        TypeCtorCheckedMap, InstCtorCheckedMap, ModeCtorCheckedMap,
        TypeCtorRepnMap, ParseTreeInt1, !Specs),
    generate_interface_int2(AugMakeIntUnit,
        IntExplicitFIMSpecs, ImpExplicitFIMSpecs,
        TypeCtorCheckedMap, InstCtorCheckedMap, ModeCtorCheckedMap,
        TypeCtorRepnMap, ParseTreeInt2).

:- pred generate_interface_int1(globals::in, aug_make_int_unit::in,
    set(fim_spec)::out, set(fim_spec)::out,
    type_ctor_checked_map::out,
    inst_ctor_checked_map::out, mode_ctor_checked_map::out,
    type_ctor_repn_map::out, parse_tree_int1::out,
    list(error_spec)::in, list(error_spec)::out) is det.

generate_interface_int1(Globals, AugMakeIntUnit,
        IntExplicitFIMSpecs, ImpExplicitFIMSpecs,
        IntTypeCtorCheckedMap, IntInstCtorCheckedMap, IntModeCtorCheckedMap,
        TypeCtorRepnMap, ParseTreeInt1, !Specs) :-
    % We return some of our intermediate results to our caller, for use
    % in constructing the .int2 file.
    AugMakeIntUnit = aug_make_int_unit(ParseTreeModuleSrc,
        _, DirectIntSpecs, IndirectIntSpecs, _),

    ParseTreeModuleSrc = parse_tree_module_src(ModuleName, ModuleNameContext,
        InclMap, ImportUseMap,
        IntFIMSpecMap, ImpFIMSpecMap, IntSelfFIMLangs, _ImpSelfFIMLangs,

        TypeCtorCheckedMap0, InstCtorCheckedMap0, ModeCtorCheckedMap0,
        _TypeSpecs, _InstModeSpecs,

        IntTypeClasses, IntInstances0, IntPredDecls, IntModeDecls,
        IntDeclPragmas, IntPromises0, _IntBadClausePreds,

        ImpTypeClasses0, _ImpInstances, _ImpPredDecls, _ImpModeDecls,
        _ImpClauses, _ImpForeignProcs, _ImpForeignExportEnums,
        _ImpDeclPragmas, _ImpImplPragmas, _ImpPromises,
        _ImpInitialises, _ImpFinalises, _ImpMutables),

    % Separate out the contents of the interface section(s) from the
    % contents of the implementation section(s). Separate out the
    % foreign enum pragmas and foreign_import_module items in the
    % implementation section, for possible selective reinclusion later.
    % Likewise, remove type definitions from the implementation section
    % after recording them in ImpTypesMap. Record the type definitions
    % in the interface section as well, in IntTypesMap. Record the set of
    % modules that we need access to due to references in typeclass
    % definition items.

    type_ctor_checked_map_get_src_defns(TypeCtorCheckedMap0,
        IntTypeDefns0, ImpTypeDefns0, _ImpForeignEnums0),
    list.foldl(record_type_defn_int, IntTypeDefns0,
        one_or_more_map.init, IntTypesMap),
    list.foldl(record_type_defn_imp, ImpTypeDefns0,
        one_or_more_map.init, ImpTypesMap),
    BothTypesMap = one_or_more_map.merge(IntTypesMap, ImpTypesMap),

    % Compute the set of type_ctors whose definitions in the implementation
    % section we need to preserve, possibly in abstract form (that is
    % figured out below).
    %
    % Also, work out which modules we will need access to due to the
    % definitions of equivalence types, foreign types, dummy, enum and other
    % du types whose definitions we are keeping in the implementation
    % section.
    get_requirements_of_imp_exported_types(IntTypesMap, ImpTypesMap,
        BothTypesMap, NeededImpTypeCtors, ImpModulesNeededByTypeDefns),
    ImpTypeClasses = list.map(check_typeclass_is_abstract, ImpTypeClasses0),
    list.foldl(record_modules_needed_by_typeclass_imp, ImpTypeClasses,
        set.init, ImpModulesNeededByTypeClassDefns),
    set.union(ImpModulesNeededByTypeClassDefns, ImpModulesNeededByTypeDefns,
        ImpNeededModules),

    % XXX ITEM_LIST We should put a use_module decl into the interface
    % of the .int file ONLY IF the module is actually used in the interface.
    %
    % We already *do* generate warnings for any modules we import or use
    % in the interface that are not required in the interface, and programmers
    % do tend to delete such unnecessary imports from the interface,
    % so fixing this overestimation is not all that urgent.
    %
    % Since everything we put into a .int file should be fully module
    % qualified, we convert all import_modules into use_modules.
    map.filter_map_values(
        make_imports_into_uses_maybe_implicit(ImpNeededModules),
        ImportUseMap, SectionUseOnlyMap),

    map.keys_as_set(IntFIMSpecMap, IntExplicitFIMSpecs),
    map.keys_as_set(ImpFIMSpecMap, ImpExplicitFIMSpecs),

    % Note that _ImpSelfFIMLangs above contains the set of foreign languages
    % for which an implicit self FIM is needed by anything in the
    % implementation section of the *source file*. We are now starting to
    % compute the set of foreign languages for which an implicit self FIM
    % is needed by anything in the implementation section *of the interface
    % file we are constructing*, which will be a *subset* of _ImpSelfFIMLangs.
    % XXX Using _ImpSelfFIMLangs from ParseTreeModuleSrc instead of the value
    % of ImpSelfFIMLangs we compute here and below would therefore be
    % an overapproximation, but I (zs) don't think the cost in code complexity
    % of avoiding this overapproximation is worth the negligible benefits
    % it gets us.
    map.foldl2(
        hide_type_ctor_checked_defn_imp_details_for_int1(BothTypesMap,
            NeededImpTypeCtors),
        TypeCtorCheckedMap0, map.init, IntTypeCtorCheckedMap,
        set.init, ImpSelfFIMLangs),

    set.foldl(add_self_fim(ModuleName), IntSelfFIMLangs,
        IntExplicitFIMSpecs, IntFIMSpecs),
    set.foldl(add_self_fim(ModuleName), ImpSelfFIMLangs,
        ImpExplicitFIMSpecs, ImpFIMSpecs0),
    set.difference(ImpFIMSpecs0, IntFIMSpecs, ImpFIMSpecs),

    inst_ctor_checked_map_get_src_defns(InstCtorCheckedMap0,
        IntInstDefns, _ImpInstDefns),
    IntInstDefnMap = inst_ctor_defn_items_to_map(IntInstDefns),
    create_inst_ctor_checked_map(do_not_insist_on_defn,
        IntInstDefnMap, map.init, IntInstCtorCheckedMap, !Specs),

    mode_ctor_checked_map_get_src_defns(ModeCtorCheckedMap0,
        IntModeDefns, _ImpModeDefns),
    IntModeDefnMap = mode_ctor_defn_items_to_map(IntModeDefns),
    create_mode_ctor_checked_map(do_not_insist_on_defn,
        IntModeDefnMap, map.init, IntModeCtorCheckedMap, !Specs),

    globals.lookup_bool_option(Globals, experiment1, Experiment1),
    (
        Experiment1 = no,
        map.init(TypeCtorRepnMap)
    ;
        Experiment1 = yes,
        decide_repns_for_all_types_for_int1(Globals, ModuleName,
            TypeCtorCheckedMap0, DirectIntSpecs, IndirectIntSpecs,
            TypeCtorRepnMap, RepnSpecs),
        !:Specs = !.Specs ++ RepnSpecs
    ),

    IntInstances = list.map(check_instance_is_abstract, IntInstances0),
    list.filter(keep_promise_item_int, IntPromises0, IntPromises),

    DummyMaybeVersionNumbers = no_version_numbers,
    % XXX TODO
    ParseTreeInt1 = parse_tree_int1(ModuleName, ModuleNameContext,
        DummyMaybeVersionNumbers, InclMap, SectionUseOnlyMap,
        IntFIMSpecs, ImpFIMSpecs,
        IntTypeCtorCheckedMap, IntInstCtorCheckedMap, IntModeCtorCheckedMap,
        IntTypeClasses, IntInstances, IntPredDecls, IntModeDecls,
        IntDeclPragmas, IntPromises, TypeCtorRepnMap, ImpTypeClasses).

%---------------------%

:- pred add_self_fim(module_name::in, foreign_language::in,
    set(fim_spec)::in, set(fim_spec)::out) is det.

add_self_fim(ModuleName, Lang, !FIMSpecs) :-
    FIMSpec = fim_spec(Lang, ModuleName),
    set.insert(FIMSpec, !FIMSpecs).

:- pred make_imports_into_uses_maybe_implicit(set(module_name)::in,
    module_name::in, maybe_implicit_import_and_or_use::in,
    section_use::out) is semidet.

make_imports_into_uses_maybe_implicit(ImpNeededModules, ModuleName,
        ImportUse, SectionUseOnly) :-
    (
        ImportUse = explicit_avail(Explicit),
        make_imports_into_uses(ImpNeededModules, ModuleName,
            Explicit, SectionUseOnly)
    ;
        ImportUse = implicit_avail(_Implicit, MaybeExplicit),
        MaybeExplicit = yes(Explicit),
        make_imports_into_uses(ImpNeededModules, ModuleName,
            Explicit, SectionUseOnly)
    ).

:- pred make_imports_into_uses(set(module_name)::in, module_name::in,
    section_import_and_or_use::in, section_use::out) is semidet.

make_imports_into_uses(ImpNeededModules, ModuleName, Explicit0, Explicit) :-
    (
        ( Explicit0 = int_import(IntContext)
        ; Explicit0 = int_use(IntContext)
        ; Explicit0 = int_use_imp_import(IntContext, _ImpContext)
        ),
        Explicit = int_use(IntContext)
    ;
        ( Explicit0 = imp_import(ImpContext)
        ; Explicit0 = imp_use(ImpContext)
        ),
        ( if set.contains(ImpNeededModules, ModuleName) then
            Explicit = imp_use(ImpContext)
        else
            fail
        )
    ).

%---------------------%

:- type type_defn_map == one_or_more_map(type_ctor, item_type_defn_info).
:- type type_defn_pair == pair(type_ctor, item_type_defn_info).

:- pred record_type_defn_int(item_type_defn_info::in,
    type_defn_map::in, type_defn_map::out) is det.

record_type_defn_int(ItemTypeDefn, !IntTypesMap) :-
    ItemTypeDefn = item_type_defn_info(Name, TypeParams, _, _, _, _),
    TypeCtor = type_ctor(Name, list.length(TypeParams)),
    one_or_more_map.add(TypeCtor, ItemTypeDefn, !IntTypesMap).

:- pred record_type_defn_imp(item_type_defn_info::in,
    type_defn_map::in, type_defn_map::out) is det.

record_type_defn_imp(ItemTypeDefn, !ImpTypesMap) :-
    % We don't add this to the final item cord we intend to put
    % into the interface file yet -- we may be removing it.
    % If we *do* want the items for a given type_ctor, we will create
    % new copies of the items from the type_ctor's entry in ImpTypesMap.
    % We do however gather it for use in checking the type definitions
    % in the module.
    ItemTypeDefn = item_type_defn_info(Name, TypeParams, TypeDefn, _, _, _),
    TypeCtor = type_ctor(Name, list.length(TypeParams)),
    (
        TypeDefn = parse_tree_solver_type(_),
        % generate_pre_grab_pre_qual_interface_for_int1_int2 has replaced
        % solver type definitions with a dummy definition, and we want
        % to put that dummy definition into !OrigImpTypeDefnsCord
        % so that we don't generate inappropriate error messages
        % about the solver type being declared but not defined.
        % On the other hand, we want to put just a declaration,
        % not a definition, of the solver type into .int and .int2 files.
        TypeDefn1 = parse_tree_abstract_type(abstract_solver_type),
        ItemTypeDefn1 = ItemTypeDefn ^ td_ctor_defn := TypeDefn1
    ;
        ( TypeDefn = parse_tree_abstract_type(_)
        ; TypeDefn = parse_tree_du_type(_)
        ; TypeDefn = parse_tree_sub_type(_)
        ; TypeDefn = parse_tree_eqv_type(_)
        ; TypeDefn = parse_tree_foreign_type(_)
        ),
        ItemTypeDefn1 = ItemTypeDefn
    ),
    one_or_more_map.add(TypeCtor, ItemTypeDefn1, !ImpTypesMap).

:- pred record_modules_needed_by_typeclass_imp(
    item_abstract_typeclass_info::in,
    set(module_name)::in, set(module_name)::out) is det.

record_modules_needed_by_typeclass_imp(ItemTypeClass,
        !ImpModulesNeededByTypeClassDefns) :-
    % The superclass constraints on the typeclass being declared
    % may refer to typeclasses that this module has imported.
    Constraints = ItemTypeClass ^ tc_superclasses,
    list.foldl(accumulate_modules_in_qual_constraint, Constraints,
        !ImpModulesNeededByTypeClassDefns).

:- pred keep_promise_item_int(item_promise_info::in) is semidet.

keep_promise_item_int(ItemPromise) :-
    PromiseType = ItemPromise ^ prom_type,
    require_complete_switch [PromiseType]
    (
        PromiseType = promise_type_true,
        fail
    ;
        ( PromiseType = promise_type_exclusive
        ; PromiseType = promise_type_exhaustive
        ; PromiseType = promise_type_exclusive_exhaustive
        )
    ).

%---------------------------------------------------------------------------%

    % get_requirements_of_imp_exported_types(IntTypesMap, ImpTypesMap,
    %   BothTypesMap, NeededTypeCtors, ModulesNeededByTypeDefns):
    %
    % Compute NeededTypeCtors, the set of type constructors whose definitions
    % we need to keep in the implementation section of the .int file
    % (in their original or abstract form), and ModulesNeededByTypeDefns,
    % the set of modules whose :- import_module and :- use_module declarations
    % we need to keep because they define type_ctors used in these kept
    % type definitions.
    %
    % We do this using two passes.
    %
    % In the first pass, we process every type with a definition in the
    % implementation.
    %
    % - If that definition is equivalence type definition, and there is
    %   any definition of that same type_ctor in the interface (presumably
    %   but necessarily as an abstract type), then include the type_ctor
    %   in AbsExpEqvLhsTypeCtors. We include these type_ctors in
    %   NeededImpTypeCtors because on 32-bit platforms, if type t1 is
    %   defined to be equivalent to a 64 bit float, then we need to take
    %   this into account when deciding the representation of types
    %   with t1 fields even if type t1 is abstract exported.
    %   XXX TYPE_REPN We should convey this info in type_repn items,
    %   not type_defn items, since the latter can be used for purposes
    %   other than type representation.
    %
    % - We handle foreign type definitions the same way as equivalence type
    %   definitions, just in case the foreign type is also bigger than a word.
    %   XXX TYPE_REPN Again, this info should be in a type_repn item.
    %   XXX TYPE_REPN Shouldn't boxing make the size of the foreign type
    %   immaterial?
    %
    % - If the definition defines a subtype, and there are any definitions of
    %   that same type_ctor in the interface, then include the type_ctor in
    %   AbsExpEqvLhsTypeCtors, and the type_ctors of any supertype or
    %   equivalence types up to the base type. We include these type_ctors in
    %   NeededImpTypeCtors because the representation of subtypes must be the
    %   same as that of their base types.
    %
    % - If the definition defines an enum type (not a subtype), and there is a
    %   definition of the same type_ctor in the interface, we include the
    %   type_ctor in AbsExpEnumTypeCtors. This is so that when we abstract
    %   export the type_ctor, we can record that its size is less than one
    %   word.
    %   XXX TYPE_REPN Again, this info should be in a type_repn item.
    %
    % - If the definition defines a dummy type (not a subtype), we include the
    %   type_ctor in DirectDummyTypeCtors.
    %   XXX ITEM_LIST Presumably (by me -zs) this is so that when we abstract
    %   export them, we can record that it needs no storage.
    %   XXX However, we currently include dummy types in the
    %   implementation section of the .int file unchanged, and we do so
    %   even if the type is not mentioned in the interface section at all.
    %   XXX TYPE_REPN Again, this info should be in a type_repn item.
    %
    % The first pass ignores all other type definitions.
    %
    % The second pass processes the type_ctors in AbsExpEqvLhsTypeCtors,
    % i.e. the abstract exported type_ctors which have an equivalence type,
    % foreign type, or subtype definition in the implementation section.
    % Its job is to compute three sets.
    %
    % - The first set is AbsExpEqvRhsTypeCtors, the set of type_ctors
    %   that occur in any (partial or full) expansion of an equivalence type
    %   in AbsExpEqvLhsTypeCtors. This means that if e.g. type t2 is abstract
    %   exported and its definition in the implementation section is
    %
    %       :- type t2 == t3(t4, t5).
    %       :- type t3(A, B) ---> ... a discriminated union definition ...
    %       :- type t4 --->       ... a discriminated union definition ...
    %       :- type t5 == t6.
    %       :- type t6 --->       ... a discriminated union definition ...
    %
    %   then we return {t2, t3, t4, t5, t6} as AbsExpEqvRhsTypeCtors.
    %
    % - The second set is DuArgTypeCtors, the set of type_ctors that occur
    %   on the right hand side (i.e. among the field argument types) of
    %   a discriminated union definition of a type_ctor that is in
    %   AbsExpEqvLhsTypeCtors, which should happen only when that type_ctor
    %   also has foreign language definitions or a subtype definition
    %   (since we put a type_ctor into AbsExpEqvLhsTypeCtors only if it has
    %   either an equivalence definition, foreign language definition,
    %   or subtype definition). If these type_ctors are not
    %   otherwise included in the .int file, this will cause our caller
    %   to include an abstract declaration of these type_ctors in the
    %   .int file, to disambiguate the references to these types
    %   in the full (in the sense of non-abstractified) du Mercury definitions
    %   we include in the .int file next to the foreign language definitions.
    %
    % - The third set we return is ModulesNeededByTypeDefns, which consists
    %   of the names of the modules that define the type_ctors in the first
    %   two sets.
    %
    % XXX ITEM_LIST The comment lines starting with a double percent
    % are the comment on the original version of this predicate.
    %
    %% Figure out the set of abstract equivalence type constructors (i.e.
    %% the types that are exported as abstract types and which are defined
    %% in the implementation section as equivalence types or as foreign types).
    %% Return in NeededTypeCtors the smallest set containing those
    %% constructors, and the set of private type constructors referred to
    %% by the right hand side of any type in NeededTypeCtors.
    %%
    %% XXX Return in DirectDummyTypeCtors the set of dummy type constructors.
    %%
    %% Given a du type definition in the implementation section, we should
    %% include it in AbsImpExpLhsTypeCtors if the type constructor is abstract
    %% exported and the implementation section also contains a foreign_type
    %% definition of the type constructor.
    %%
    %% Given a enumeration type definition in the implementation section, we
    %% should include it in AbsImpExpEnumTypeCtors if the type constructor is
    %% abstract exported.
    %%
    %% Return in NeededModules the set of modules that define the type
    %% constructors in NeededTypeCtors.
    %
:- pred get_requirements_of_imp_exported_types(type_defn_map::in,
    type_defn_map::in, type_defn_map::in,
    set(type_ctor)::out, set(module_name)::out) is det.

get_requirements_of_imp_exported_types(IntTypesMap, ImpTypesMap,
        BothTypesMap, NeededImpTypeCtors, ModulesNeededByTypeDefns) :-
    % XXX may want to rename AbsExpEqvLhsTypeCtors as it also includes
    % foreign types and subtypes
    map.foldl3(
        accumulate_abs_imp_exported_type_lhs(IntTypesMap, BothTypesMap),
        ImpTypesMap,
        set.init, AbsExpEqvLhsTypeCtors,
        set.init, AbsExpEnumTypeCtors,
        set.init, DirectDummyTypeCtors),
    set.fold3(accumulate_abs_imp_exported_type_rhs(ImpTypesMap),
        AbsExpEqvLhsTypeCtors,
        set.init, AbsExpEqvRhsTypeCtors,
        set.init, DuArgTypeCtors,
        set.init, ModulesNeededByTypeDefns),
    NeededImpTypeCtors = set.union_list([AbsExpEqvLhsTypeCtors,
        AbsExpEqvRhsTypeCtors, AbsExpEnumTypeCtors, DirectDummyTypeCtors,
        DuArgTypeCtors]).

:- pred accumulate_abs_imp_exported_type_lhs(type_defn_map::in,
    type_defn_map::in, type_ctor::in, one_or_more(item_type_defn_info)::in,
    set(type_ctor)::in, set(type_ctor)::out,
    set(type_ctor)::in, set(type_ctor)::out,
    set(type_ctor)::in, set(type_ctor)::out) is det.

accumulate_abs_imp_exported_type_lhs(IntTypesMap, BothTypesMap,
        TypeCtor, ImpItemTypeDefnInfos, !AbsExpEqvLhsTypeCtors,
        !AbsExpEnumTypeCtors, !DirectDummyTypeCtors) :-
    ImpItemTypeDefnInfos =
        one_or_more(HeadImpItemTypeDefnInfo, TailImpItemTypeDefnInfos),
    (
        TailImpItemTypeDefnInfos = [],
        % Don't construct a closure when a type_ctor has only one definition
        % in the implementation section, since this the common case.
        accumulate_abs_imp_exported_type_lhs_in_defn(IntTypesMap, BothTypesMap,
            TypeCtor, HeadImpItemTypeDefnInfo,
            !AbsExpEqvLhsTypeCtors, !AbsExpEnumTypeCtors,
            !DirectDummyTypeCtors)
    ;
        TailImpItemTypeDefnInfos = [_ | _],
        % A type may have multiple definitions in the implementation section
        % because it may be defined both in Mercury and in a foreign language.
        % A type with multiple definitions doesn't typically include
        % an equivalence type among those definitions, but we have to be
        % prepared for such an eventuality anyway.
        one_or_more.foldl3(
            accumulate_abs_imp_exported_type_lhs_in_defn(IntTypesMap,
                BothTypesMap, TypeCtor),
            ImpItemTypeDefnInfos,
            !AbsExpEqvLhsTypeCtors, !AbsExpEnumTypeCtors,
            !DirectDummyTypeCtors)
    ).

:- pred accumulate_abs_imp_exported_type_lhs_in_defn(type_defn_map::in,
    type_defn_map::in, type_ctor::in, item_type_defn_info::in,
    set(type_ctor)::in, set(type_ctor)::out,
    set(type_ctor)::in, set(type_ctor)::out,
    set(type_ctor)::in, set(type_ctor)::out) is det.

accumulate_abs_imp_exported_type_lhs_in_defn(IntTypesMap, BothTypesMap,
        TypeCtor, ImpItemTypeDefnInfo, !AbsExpEqvLhsTypeCtors,
        !AbsExpEnumTypeCtors, !DirectDummyTypeCtors) :-
    ImpItemTypeDefnInfo = item_type_defn_info(_, _, ImpTypeDefn, TVarSet,
        _, _),
    (
        ImpTypeDefn = parse_tree_eqv_type(_),
        ( if map.search(IntTypesMap, TypeCtor, _) then
            set.insert(TypeCtor, !AbsExpEqvLhsTypeCtors)
        else
            true
        )
    ;
        ImpTypeDefn = parse_tree_foreign_type(_),
        ( if map.search(IntTypesMap, TypeCtor, _) then
            % XXX ITEM_LIST This looks like a lost opportunity to me (zs),
            % because the only foreign types that *need* the same treatment
            % as equivalence types are foreign types that are bigger than
            % one word in size. The ones that have can_pass_as_mercury_type
            % as an attribute are supposed to fit into one word (though
            % that assertion may be valid for some platforms only) and thus
            % *could* be left out of !AbsExpEqvLhsTypeCtors.
            %
            % However, before making such a change, consider everything
            % in the discussion on this topic on m-rev on 2019 feb 18-19.
            set.insert(TypeCtor, !AbsExpEqvLhsTypeCtors)
        else
            true
        )
    ;
        ImpTypeDefn = parse_tree_du_type(DetailsDu),
        DetailsDu = type_details_du(OoMCtors, MaybeEqCmp, MaybeDirectArgCtors),
        ( if
            map.search(IntTypesMap, TypeCtor, _),
            non_sub_du_type_is_enum(DetailsDu, _NumFunctors)
        then
            set.insert(TypeCtor, !AbsExpEnumTypeCtors)
        else if
            % XXX ITEM_LIST Why don't we insist that TypeCtor occurs
            % in IntTypesMap?
            % XXX ITEM_LIST If a type has one function symbol
            % with arity one and the argument type is equivalent
            % to a dummy type that is defined in another module,
            % we will NOT include TypeCtor in !DirectDummyTypeCtors,
            % since we won't know enough about the contents of the
            % other module.
            non_sub_du_constructor_list_represents_dummy_type(BothTypesMap,
                TVarSet, OoMCtors, MaybeEqCmp, MaybeDirectArgCtors)
        then
            set.insert(TypeCtor, !DirectDummyTypeCtors)
        else
            true
        )
    ;
        ImpTypeDefn = parse_tree_sub_type(DetailsSub),
        DetailsSub = type_details_sub(SuperType, _OoMCtors),
        ( if map.search(IntTypesMap, TypeCtor, _) then
            set.insert(TypeCtor, !AbsExpEqvLhsTypeCtors),
            ( if type_to_ctor(SuperType, SuperTypeCtor) then
                set.singleton_set(TypeCtor, Seen0),
                accumulate_eqv_and_supertypes(BothTypesMap,
                    SuperTypeCtor, !AbsExpEqvLhsTypeCtors, Seen0, _Seen)
            else
                true
            )
        else
            true
        )
    ;
        ( ImpTypeDefn = parse_tree_abstract_type(_)
        ; ImpTypeDefn = parse_tree_solver_type(_)
        )
    ).

    % Accumulate all supertype and equivalence type ctors leading to the
    % base type ctor. The base type ctor does not need to be included.
    %
:- pred accumulate_eqv_and_supertypes(type_defn_map::in, type_ctor::in,
    set(type_ctor)::in, set(type_ctor)::out,
    set(type_ctor)::in, set(type_ctor)::out) is det.

accumulate_eqv_and_supertypes(BothTypesMap, TypeCtor, !AbsExpEqvLhsTypeCtors,
        !Seen) :-
    % Check for circular types.
    ( if set.insert_new(TypeCtor, !Seen) then
        set.insert(TypeCtor, !AbsExpEqvLhsTypeCtors),
        ( if map.search(BothTypesMap, TypeCtor, ItemTypeDefnInfos) then
            one_or_more.foldl2(
                accumulate_eqv_and_supertypes_in_defn(BothTypesMap, TypeCtor),
                ItemTypeDefnInfos, !AbsExpEqvLhsTypeCtors, !Seen)
        else
            true
        )
    else
        true
    ).

:- pred accumulate_eqv_and_supertypes_in_defn(type_defn_map::in,
    type_ctor::in, item_type_defn_info::in,
    set(type_ctor)::in, set(type_ctor)::out,
    set(type_ctor)::in, set(type_ctor)::out) is det.

accumulate_eqv_and_supertypes_in_defn(BothTypesMap, TypeCtor, ItemTypeDefnInfo,
        !AbsExpEqvLhsTypeCtors, !Seen) :-
    ItemTypeDefnInfo = item_type_defn_info(_, _, TypeDefn, _, _, _),
    (
        TypeDefn = parse_tree_eqv_type(DetailsEqv),
        set.insert(TypeCtor, !AbsExpEqvLhsTypeCtors),
        DetailsEqv = type_details_eqv(RhsType),
        ( if type_to_ctor(RhsType, RhsTypeCtor) then
            accumulate_eqv_and_supertypes(BothTypesMap, RhsTypeCtor,
                !AbsExpEqvLhsTypeCtors, !Seen)
        else
            true
        )
    ;
        TypeDefn = parse_tree_sub_type(DetailsSub),
        DetailsSub = type_details_sub(SuperType, _),
        % Not yet at the base type.
        set.insert(TypeCtor, !AbsExpEqvLhsTypeCtors),
        ( if type_to_ctor(SuperType, SuperTypeCtor) then
            accumulate_eqv_and_supertypes(BothTypesMap, SuperTypeCtor,
                !AbsExpEqvLhsTypeCtors, !Seen)
        else
            true
        )
    ;
        TypeDefn = parse_tree_du_type(_DetailsDu)
        % This is the base type.
    ;
        ( TypeDefn = parse_tree_foreign_type(_)
        ; TypeDefn = parse_tree_abstract_type(_)
        ; TypeDefn = parse_tree_solver_type(_)
        )
    ).

:- pred accumulate_abs_imp_exported_type_rhs(type_defn_map::in, type_ctor::in,
    set(type_ctor)::in, set(type_ctor)::out,
    set(type_ctor)::in, set(type_ctor)::out,
    set(module_name)::in, set(module_name)::out) is det.

accumulate_abs_imp_exported_type_rhs(ImpTypesMap, TypeCtor,
        !AbsExpEqvRhsTypeCtors, !DuArgTypeCtors, !ModulesNeededByTypeDefns) :-
    ( if map.search(ImpTypesMap, TypeCtor, ImpTypeDefns) then
        one_or_more.foldl3(
            accumulate_abs_eqv_type_rhs_in_defn(ImpTypesMap),
            ImpTypeDefns,
            !AbsExpEqvRhsTypeCtors, !DuArgTypeCtors, !ModulesNeededByTypeDefns)
    else
        % TypeCtor is not defined in the implementation section
        % of this module.
        true
    ).

:- pred accumulate_abs_eqv_type_rhs_in_defn(type_defn_map::in,
    item_type_defn_info::in,
    set(type_ctor)::in, set(type_ctor)::out,
    set(type_ctor)::in, set(type_ctor)::out,
    set(module_name)::in, set(module_name)::out) is det.

accumulate_abs_eqv_type_rhs_in_defn(ImpTypesMap, ImpItemTypeDefnInfo,
        !AbsExpEqvRhsTypeCtors, !DuArgTypeCtors, !ModulesNeededByTypeDefns) :-
    ImpItemTypeDefnInfo = item_type_defn_info(_, _, ImpTypeDefn, _, _, _),
    (
        ImpTypeDefn = parse_tree_eqv_type(DetailsEqv),
        DetailsEqv = type_details_eqv(RhsType),
        type_to_user_type_ctor_set(RhsType, set.init, RhsTypeCtors),

        % Logically, we want to invoke the call to set.union and the
        % calls to set.foldl/foldl3 below on all RhsTypeCtors. However, for
        % any type_ctor in RhsTypeCtors that is in !.AbsExpEqvRhsTypeCtors,
        % we have alteady done so, and since all three operations are
        % idempotent, there is no point in invoking them again.
        set.difference(RhsTypeCtors, !.AbsExpEqvRhsTypeCtors, NewRhsTypeCtors),
        set.union(NewRhsTypeCtors, !AbsExpEqvRhsTypeCtors),
        set.fold(accumulate_modules_in_qual_type_ctor, NewRhsTypeCtors,
            !ModulesNeededByTypeDefns),
        % XXX ITEM_LIST I (zs) *think* that the reason why we ignore the
        % result of the second accumulator (!DuArgTypeCtors) in this call
        % is because the appearance of a type_ctor in RhsTypeCtors
        % on the right hand side of an equivalence type definition
        % will (by itself) only generate an abstract definition for that
        % type_ctor in the .int file, so other modules need not know about
        % any type_ctors just because they appear on the right hand side
        % of *its* definition. However, I am far from sure.
        set.fold3(accumulate_abs_imp_exported_type_rhs(ImpTypesMap),
            NewRhsTypeCtors,
            !AbsExpEqvRhsTypeCtors, set.init, _, !ModulesNeededByTypeDefns)
    ;
        (
            ImpTypeDefn = parse_tree_du_type(DetailsDu),
            DetailsDu = type_details_du(OoMCtors, _, _)
        ;
            ImpTypeDefn = parse_tree_sub_type(DetailsSub),
            DetailsSub = type_details_sub(_, OoMCtors)
        ),
        % There must exist a foreign type alternative to this type.
        % XXX ITEM_LIST I (zs) would like to see a proof argument for that,
        % since I don't think it is true. Unfortunately, we cannot check it
        % locally.

        % As the du type will be exported, we require all the type_ctors
        % inside all the argument types of all the data constructors, and the
        % modules that define them.
        ctors_to_user_type_ctor_set(one_or_more_to_list(OoMCtors),
            set.init, RhsTypeCtors),
        set.union(RhsTypeCtors, !DuArgTypeCtors),
        set.fold(accumulate_modules_in_qual_type_ctor, RhsTypeCtors,
            !ModulesNeededByTypeDefns)
    ;
        ( ImpTypeDefn = parse_tree_abstract_type(_)
        ; ImpTypeDefn = parse_tree_solver_type(_)
        ; ImpTypeDefn = parse_tree_foreign_type(_)
        )
    ).

%---------------------%

    % Given a type, return the set of user-defined type constructors
    % occurring in it. We do not gather the type constructors of
    % builtin types, higher-order types and typle types, because
    % are always available without any module needing to be imported,
    % which is what our caller uses our results for.
    %
:- pred type_to_user_type_ctor_set(mer_type::in,
    set(type_ctor)::in, set(type_ctor)::out) is det.

type_to_user_type_ctor_set(Type, !TypeCtors) :-
    ( if type_to_ctor_and_args(Type, TypeCtor, ArgTypes) then
        TypeCtor = type_ctor(SymName, _Arity),
        ( if
            ( is_builtin_type_sym_name(SymName)
            ; type_ctor_is_higher_order(TypeCtor, _, _, _)
            ; type_ctor_is_tuple(TypeCtor)
            )
        then
            true
        else
            set.insert(TypeCtor, !TypeCtors)
        ),
        list.foldl(type_to_user_type_ctor_set, ArgTypes, !TypeCtors)
    else
        true
    ).

:- pred ctors_to_user_type_ctor_set(list(constructor)::in,
    set(type_ctor)::in, set(type_ctor)::out) is det.

ctors_to_user_type_ctor_set([], !TypeCtors).
ctors_to_user_type_ctor_set([Ctor | Ctors], !TypeCtors) :-
    Ctor = ctor(_, _, _, CtorArgs, _, _),
    ctor_args_to_user_type_ctor_set(CtorArgs, !TypeCtors),
    ctors_to_user_type_ctor_set(Ctors, !TypeCtors).

:- pred ctor_args_to_user_type_ctor_set(list(constructor_arg)::in,
    set(type_ctor)::in, set(type_ctor)::out) is det.

ctor_args_to_user_type_ctor_set([], !TypeCtors).
ctor_args_to_user_type_ctor_set([Arg | Args], !TypeCtors) :-
    Arg = ctor_arg(_, Type, _),
    type_to_user_type_ctor_set(Type, !TypeCtors),
    ctor_args_to_user_type_ctor_set(Args, !TypeCtors).

%---------------------%

    % Certain types, e.g. io.state and store.store(S), are just dummy types
    % used to ensure logical semantics; there is no need to actually pass them,
    % and so when importing or exporting procedures to/from C, we don't include
    % arguments with these types.
    %
    % See the documentation for `type_util.is_type_a_dummy' for the definition
    % of a dummy type.
    %
    % NOTE: changes here may require changes to `type_util.is_type_a_dummy'.
    %
    % This predicate can only be used to test non-subtype du types.
    %
:- pred non_sub_du_constructor_list_represents_dummy_type(type_defn_map::in,
    tvarset::in, one_or_more(constructor)::in, maybe_canonical::in,
    maybe(list(sym_name_arity))::in) is semidet.

non_sub_du_constructor_list_represents_dummy_type(TypeDefnMap, TVarSet,
        OoMCtors, MaybeCanonical, MaybeDirectArgCtors) :-
    non_sub_du_constructor_list_represents_dummy_type_2(TypeDefnMap, TVarSet,
        OoMCtors, MaybeCanonical, MaybeDirectArgCtors, []).

:- pred non_sub_du_constructor_list_represents_dummy_type_2(type_defn_map::in,
    tvarset::in, one_or_more(constructor)::in, maybe_canonical::in,
    maybe(list(sym_name_arity))::in, list(mer_type)::in) is semidet.

non_sub_du_constructor_list_represents_dummy_type_2(TypeDefnMap, TVarSet,
        OoMCtors, canon, no, CoveredTypes) :-
    OoMCtors = one_or_more(Ctor, []),
    Ctor = ctor(_Ordinal, MaybeExistConstraints, _Name, CtorArgs, _Arity,
        _Context),
    MaybeExistConstraints = no_exist_constraints,
    (
        % A single zero-arity constructor.
        CtorArgs = []
    ;
        % A constructor with a single dummy argument.
        CtorArgs = [ctor_arg(_, ArgType, _)],
        ctor_arg_is_dummy_type(TypeDefnMap, TVarSet, ArgType, CoveredTypes)
            = yes
    ).

:- func ctor_arg_is_dummy_type(type_defn_map, tvarset, mer_type,
    list(mer_type)) = bool.

ctor_arg_is_dummy_type(TypeDefnMap, TVarSet, Type, CoveredTypes0)
        = IsDummyType :-
    (
        Type = defined_type(SymName, TypeArgs, _Kind),
        ( if list.member(Type, CoveredTypes0) then
            % The type is circular.
            IsDummyType = no
        else
            Arity = list.length(TypeArgs),
            TypeCtor = type_ctor(SymName, Arity),
            ( if
                (
                    is_type_ctor_a_builtin_dummy(TypeCtor)
                        = is_builtin_dummy_type_ctor
                ;
                    % Can we find a definition of the type that tells us
                    % it is a dummy type?
                    ctor_arg_is_dummy_type_by_some_type_defn(TypeDefnMap,
                        TVarSet, Type, TypeCtor, TypeArgs, CoveredTypes0)
                )
            then
                IsDummyType = yes
            else
                IsDummyType = no
            )
        )
    ;
        ( Type = type_variable(_, _)
        ; Type = builtin_type(_)
        ; Type = tuple_type(_, _)
        ; Type = higher_order_type(_, _, _, _, _)
        ; Type = apply_n_type(_, _, _)
        ),
        IsDummyType = no
    ;
        Type = kinded_type(_, _),
        unexpected($pred, "kinded_type")
    ).

:- pred ctor_arg_is_dummy_type_by_some_type_defn(type_defn_map::in,
    tvarset::in, mer_type::in, type_ctor::in, list(mer_type)::in,
    list(mer_type)::in) is semidet.

ctor_arg_is_dummy_type_by_some_type_defn(TypeDefnMap, TVarSet, Type, TypeCtor,
        TypeArgs, CoveredTypes0) :-
    one_or_more_map.search(TypeDefnMap, TypeCtor, ItemTypeDefnInfos),
    one_or_more.member(ItemTypeDefnInfo, ItemTypeDefnInfos),
    ItemTypeDefnInfo = item_type_defn_info(_TypeCtor, TypeDefnTypeParams,
        TypeDefn, TypeDefnTVarSet, _Context, _SeqNum),
    (
        TypeDefn = parse_tree_du_type(DetailsDu),
        DetailsDu = type_details_du(OoMCtors, MaybeEqCmp, MaybeDirectArgCtors),
        non_sub_du_constructor_list_represents_dummy_type_2(TypeDefnMap,
            TVarSet, OoMCtors, MaybeEqCmp, MaybeDirectArgCtors,
            [Type | CoveredTypes0])
    ;
        TypeDefn = parse_tree_sub_type(DetailsSub),
        DetailsSub = type_details_sub(SuperType0, _OoMCtors),
        % A subtype can only be a dummy type if its base type is a dummy type.
        merge_tvarsets_and_subst_type_args(TVarSet, TypeArgs, TypeDefnTVarSet,
            TypeDefnTypeParams, SuperType0, SuperType),
        get_base_type(TypeDefnMap, TVarSet, SuperType, BaseType, set.init),
        ctor_arg_is_dummy_type(TypeDefnMap, TVarSet, BaseType, CoveredTypes0)
            = yes
    ).

:- pred merge_tvarsets_and_subst_type_args(tvarset::in, list(mer_type)::in,
    tvarset::in, list(type_param)::in, mer_type::in, mer_type::out) is det.

merge_tvarsets_and_subst_type_args(TVarSet, TypeArgs,
        TVarSet0, TypeParams0, Type0, Type) :-
    tvarset_merge_renaming(TVarSet, TVarSet0, _MergedTVarSet, Renaming),
    apply_variable_renaming_to_tvar_list(Renaming, TypeParams0, TypeParams),
    map.from_corresponding_lists(TypeParams, TypeArgs, TSubst),
    apply_variable_renaming_to_type(Renaming, Type0, Type1),
    apply_rec_subst_to_type(TSubst, Type1, Type).

    % This predicate is nondet because in a non-checked type_defn_map,
    % a type_ctor may have two or more subtype definitions.
    %
    % XXX CLEANUP Make both this predicate and its callers operate on
    % type_ctor_checked_maps.
    %
:- pred get_base_type(type_defn_map::in, tvarset::in, mer_type::in,
    mer_type::out, set(mer_type)::in) is nondet.

get_base_type(TypeDefnMap, TVarSet, Type, BaseType, !.SeenTypes) :-
    Type = defined_type(SymName, TypeArgs, _Kind),
    % If Type is in !.SeenTypes, fail. Otherwise, add Type to !SeenTypes.
    set.insert_new(Type, !SeenTypes),
    Arity = list.length(TypeArgs),
    TypeCtor = type_ctor(SymName, Arity),
    one_or_more_map.search(TypeDefnMap, TypeCtor, ItemTypeDefnInfos),
    one_or_more.member(ItemTypeDefnInfo, ItemTypeDefnInfos),
    ItemTypeDefnInfo = item_type_defn_info(_TypeCtor, TypeDefnTypeParams,
        TypeDefn, TypeDefnTVarSet, _Context, _SeqNum),
    (
        TypeDefn = parse_tree_du_type(_DetailsDu),
        BaseType = Type
    ;
        TypeDefn = parse_tree_sub_type(DetailsSub),
        DetailsSub = type_details_sub(SuperType0, _OoMCtors),
        merge_tvarsets_and_subst_type_args(TVarSet, TypeArgs,
            TypeDefnTVarSet, TypeDefnTypeParams, SuperType0, SuperType),
        get_base_type(TypeDefnMap, TVarSet, SuperType, BaseType, !.SeenTypes)
    ).

%---------------------------------------------------------------------------%

:- pred hide_type_ctor_checked_defn_imp_details_for_int1(type_defn_map::in,
    set(type_ctor)::in, type_ctor::in, type_ctor_checked_defn::in,
    type_ctor_checked_map::in, type_ctor_checked_map::out,
    set(foreign_language)::in, set(foreign_language)::out) is det.

hide_type_ctor_checked_defn_imp_details_for_int1(BothTypesMap,
        NeededImpTypeCtors, TypeCtor, TypeCtorCheckedDefn0,
        !TypeCtorCheckedMap, !ImpImplicitFIMLangs) :-
    (
        TypeCtorCheckedDefn0 = checked_defn_solver(_, _),
        hide_type_ctor_checked_defn_solver_imp_details_for_int1(TypeCtor,
            TypeCtorCheckedDefn0, !TypeCtorCheckedMap, !ImpImplicitFIMLangs)
    ;
        TypeCtorCheckedDefn0 = checked_defn_std(_, _),
        hide_type_ctor_checked_defn_std_imp_details_for_int1(BothTypesMap,
            NeededImpTypeCtors, TypeCtor, TypeCtorCheckedDefn0,
            !TypeCtorCheckedMap, !ImpImplicitFIMLangs)
    ).

:- inst type_ctor_checked_defn_solver for type_ctor_checked_defn/0
    --->    checked_defn_solver(ground, ground).
:- inst type_ctor_checked_defn_std for type_ctor_checked_defn/0
    --->    checked_defn_std(ground, ground).

:- pred hide_type_ctor_checked_defn_solver_imp_details_for_int1(type_ctor::in,
    type_ctor_checked_defn::in(type_ctor_checked_defn_solver),
    type_ctor_checked_map::in, type_ctor_checked_map::out,
    set(foreign_language)::in, set(foreign_language)::out) is det.

hide_type_ctor_checked_defn_solver_imp_details_for_int1(TypeCtor,
        TypeCtorCheckedDefn0, !TypeCtorCheckedMap, !ImpImplicitFIMLangs) :-
    TypeCtorCheckedDefn0 = checked_defn_solver(SolverTypeDefn0, _SrcDefns0),
    % Leave everything in interface section as is.
    % For items in implementation section:
    %
    % - replace solver types with abstract_solver_type
    (
        SolverTypeDefn0 = solver_type_abstract(AbstractSolverStatus,
            _AbstractDefn),
        (
            AbstractSolverStatus = abstract_solver_type_exported,
            map.det_insert(TypeCtor, TypeCtorCheckedDefn0, !TypeCtorCheckedMap)
        ;
            AbstractSolverStatus = abstract_solver_type_private
        )
    ;
        SolverTypeDefn0 = solver_type_full(MaybeAbstractDefn, _FullDefn),
        (
            MaybeAbstractDefn = no
        ;
            MaybeAbstractDefn = yes(AbstractDefn),
            SolverTypeDefn = solver_type_abstract(
                abstract_solver_type_exported, AbstractDefn),
            SrcDefnsSolver = src_defns_solver(
                yes(wrap_abstract_type_defn(AbstractDefn)), no),
            TypeCtorCheckedDefn =
                checked_defn_solver(SolverTypeDefn, SrcDefnsSolver),
            map.det_insert(TypeCtor, TypeCtorCheckedDefn, !TypeCtorCheckedMap)
        )
    ).

:- pred hide_type_ctor_checked_defn_std_imp_details_for_int1(
    type_defn_map::in, set(type_ctor)::in,
    type_ctor::in, type_ctor_checked_defn::in(type_ctor_checked_defn_std),
    type_ctor_checked_map::in, type_ctor_checked_map::out,
    set(foreign_language)::in, set(foreign_language)::out) is det.

hide_type_ctor_checked_defn_std_imp_details_for_int1(BothTypesMap,
        NeededImpTypeCtors, TypeCtor, TypeCtorCheckedDefn0,
        !TypeCtorCheckedMap, !ImpImplicitFIMLangs) :-
    TypeCtorCheckedDefn0 = checked_defn_std(StdTypeDefn0, SrcDefnsStd0),
    SrcDefnsStd0 =
        src_defns_std(SrcIntDefns0, SrcImpDefns0, SrcImpForeignEnums),
    % Recording the foreign languages used by foreign enum items in
    % !ImpImplicitFIMLangs *even if the item does not end up in the .int file*
    % preserves old behavior.
    list.foldl(record_foreign_lang_in_foreign_enum,
        SrcImpForeignEnums, !ImpImplicitFIMLangs),

    % Leave everything in interface section as is.
    % For items in implementation section:
    %
    % - If TypeCtor is not in NeededImpTypeCtors, delete all imp items.
    %
    % - If TypeCtor is in NeededImpTypeCtors:
    %   - Leave any equivalences alone.
    %   - Leave any foreign types alone.
    %   - Make du types abstract (via make_imp_types_abstract), except where
    %     we need to convey info that parse_tree_out.m cannot convey.
    %   - Keep foreign enum item if the type's du constructors are exported.
    (
        StdTypeDefn0 = std_mer_type_eqv(EqvStatus, EqvDefn),
        ( if set.member(TypeCtor, NeededImpTypeCtors) then
            % We keep both the int and imp parts of this type unchanged.
            map.det_insert(TypeCtor, TypeCtorCheckedDefn0, !TypeCtorCheckedMap)
        else
            % We keep only the int part of this type.
            (
                EqvStatus = std_eqv_type_mer_exported,
                % The entirety of this type is in the interface.
                map.det_insert(TypeCtor, TypeCtorCheckedDefn0,
                    !TypeCtorCheckedMap)
            ;
                EqvStatus = std_eqv_type_abstract_exported,
                AbstractStatus = std_abs_type_abstract_exported,
                AbstractDefn = EqvDefn ^ td_ctor_defn
                    := abstract_type_general,
                MaybeCJCsDefn = c_java_csharp(no, no, no),
                StdTypeDefn = std_mer_type_abstract(AbstractStatus,
                    AbstractDefn, MaybeCJCsDefn),
                SrcDefnsStd = src_defns_std(
                    [wrap_abstract_type_defn(AbstractDefn)], [], []),
                TypeCtorCheckedDefn = checked_defn_std(StdTypeDefn,
                    SrcDefnsStd),
                map.det_insert(TypeCtor, TypeCtorCheckedDefn,
                    !TypeCtorCheckedMap)
            ;
                EqvStatus = std_eqv_type_all_private
                % No part of this type is in the interface.
            )
        )
    ;
        StdTypeDefn0 = std_mer_type_subtype(SubStatus, SubDefn),
        (
            SubStatus = std_sub_type_mer_exported,
            % The entirety of this type is in the interface.
            map.det_insert(TypeCtor, TypeCtorCheckedDefn0,
                !TypeCtorCheckedMap)
        ;
            SubStatus = std_sub_type_abstract_exported,
            AbstractDefn = make_subtype_defn_abstract(SubDefn),
            ( if set.member(TypeCtor, NeededImpTypeCtors) then
                % There should be exactly one SrcImpDefn0,
                % which we replace with AbstractDefn.
                SrcImpDefns = [wrap_abstract_type_defn(AbstractDefn)]
            else
                SrcImpDefns = []
            ),
            AbstractStatus = std_abs_type_abstract_exported,
            MaybeCJCsDefn = c_java_csharp(no, no, no),
            StdTypeDefn = std_mer_type_abstract(AbstractStatus,
                AbstractDefn, MaybeCJCsDefn),
            SrcDefnsStd = src_defns_std(SrcIntDefns0, SrcImpDefns, []),
            TypeCtorCheckedDefn = checked_defn_std(StdTypeDefn,
                SrcDefnsStd),
            map.det_insert(TypeCtor, TypeCtorCheckedDefn,
                !TypeCtorCheckedMap)
        ;
            SubStatus = std_sub_type_all_private,
            % No part of this type is in the interface.
            ( if set.member(TypeCtor, NeededImpTypeCtors) then
                % There should be exactly one SrcImpDefn0,
                % which we replace with AbstractDefn.
                %
                % XXX CLEANUP We generate the same SrcDefnsStd that we used to,
                % but the StdTypeDefn we generate is wrong, because there
                % is no std_abs_type_status that exactly matches
                % the type_ctor_checked_defn we generate. This is not nice,
                % but it *should* be ok, since we will use *only* the
                % SrcDefnsStd part of the TypeCtorCheckedDefn; we won't use
                % the StdTypeDefn part.
                AbstractDefn = make_subtype_defn_abstract(SubDefn),
                AbstractStatus = std_abs_type_all_private,
                MaybeCJCsDefn = c_java_csharp(no, no, no),
                StdTypeDefn = std_mer_type_abstract(AbstractStatus,
                    AbstractDefn, MaybeCJCsDefn),
                SrcImpDefns = [wrap_abstract_type_defn(AbstractDefn)],
                SrcDefnsStd = src_defns_std([], SrcImpDefns, []),
                TypeCtorCheckedDefn = checked_defn_std(StdTypeDefn,
                    SrcDefnsStd),
                map.det_insert(TypeCtor, TypeCtorCheckedDefn,
                    !TypeCtorCheckedMap)
            else
                true
            )
        )
    ;
        (
            StdTypeDefn0 = std_mer_type_du_all_plain_constants(DuStatus,
                DuDefn, HeadCtor0, TailCtors0, MaybeCJCsDefnOrEnum0),
            Extras0 = extras_enum(HeadCtor0, TailCtors0, MaybeCJCsDefnOrEnum0)
        ;
            StdTypeDefn0 = std_mer_type_du_not_all_plain_constants(DuStatus,
                DuDefn, MaybeCJCsDefn0),
            Extras0 = extras_non_enum(MaybeCJCsDefn0)
        ),
        (
            DuStatus = std_du_type_mer_ft_exported,
            % The entirety of this type is in the interface, except any foreign
            % enum items, and we want all components where they are.
            map.det_insert(TypeCtor, TypeCtorCheckedDefn0,
                !TypeCtorCheckedMap)
        ;
            DuStatus = std_du_type_mer_exported,
            ( if set.member(TypeCtor, NeededImpTypeCtors) then
                % This type has a du Mercury definition in the interface,
                % and possibly one or more foreign type and/or enum
                % definitions in the implementation section, and we want
                % all of those items where they are.
                list.foldl(record_foreign_lang_in_type_defn,
                    SrcImpDefns0, !ImpImplicitFIMLangs),
                map.det_insert(TypeCtor, TypeCtorCheckedDefn0,
                    !TypeCtorCheckedMap)
            else
                % This type has a du Mercury definition in the interface.
                % We don't want any of its foreign type definitions in the
                % implementation section, but (since the Mercury function
                % symbols are exported) we do want any foreign enum items
                % in the implementation section to stay where they are.
                delete_any_foreign_type_defn_from_extras(Extras0, Extras),
                % Did deleting type definitions make a difference?
                ( if Extras = Extras0 then
                    % No, it did not.
                    map.det_insert(TypeCtor, TypeCtorCheckedDefn0,
                        !TypeCtorCheckedMap)
                else
                    % Yes, it did, so build the updated TypeCtorCheckedDefn.
                    (
                        Extras = extras_enum(HeadCtor, TailCtors,
                            MaybeCJCsDefnOrEnum),
                        StdTypeDefn = std_mer_type_du_all_plain_constants(
                            DuStatus, DuDefn, HeadCtor, TailCtors,
                            MaybeCJCsDefnOrEnum)
                    ;
                        Extras = extras_non_enum(MaybeCJCsDefn),
                        StdTypeDefn = std_mer_type_du_not_all_plain_constants(
                            DuStatus, DuDefn, MaybeCJCsDefn)
                    ),
                    SrcDefnsStd =
                        src_defns_std(SrcIntDefns0, [], SrcImpForeignEnums),
                    TypeCtorCheckedDefn =
                        checked_defn_std(StdTypeDefn, SrcDefnsStd),
                    map.det_insert(TypeCtor, TypeCtorCheckedDefn,
                        !TypeCtorCheckedMap)
                )
            )
        ;
            DuStatus = std_du_type_abstract_exported,
            % Since we do not export the Mercury function symbols,
            % we delete any foreign enum definition from the implementation
            % section. We also delete any foreign type definition from
            % in implementation section if TypeCtor is not in
            % NeededImpTypeCtors.
            delete_any_foreign_enum_from_extras(Extras0, MaybeCJCsDefn1),
            ( if set.member(TypeCtor, NeededImpTypeCtors) then
                ( if MaybeCJCsDefn1 = c_java_csharp(no, no, no) then
                    % After deleting any foreign enum items in the
                    % implementation section, this type has only a du Mercury
                    % definition left there. Making it abstract preserves
                    % old behavior.
                    make_du_type_defn_abstract(BothTypesMap,
                        DuDefn, MaybeAbstractDefn),
                    (
                        MaybeAbstractDefn = no,
                        % We have to keep the original du definition.
                        (
                            Extras0 = extras_enum(HeadCtor, TailCtors, _),
                            wrap_cjcs_foreign_type_no_enums(MaybeCJCsDefn1,
                                MaybeCJCsDefnOrEnum),
                            StdTypeDefn = std_mer_type_du_all_plain_constants(
                                DuStatus, DuDefn, HeadCtor, TailCtors,
                                MaybeCJCsDefnOrEnum)
                        ;
                            Extras0 = extras_non_enum(_),
                            % A non-enum type can be a dummy by being a notag
                            % type wrapped around a dummy type.
                            StdTypeDefn =
                                std_mer_type_du_not_all_plain_constants(
                                    DuStatus, DuDefn, MaybeCJCsDefn1)
                        ),
                        SrcImpDefns = [wrap_du_type_defn(DuDefn)]
                    ;
                        MaybeAbstractDefn = yes(AbstractDefn),
                        AbstractStatus = std_abs_type_abstract_exported,
                        StdTypeDefn = std_mer_type_abstract(AbstractStatus,
                            AbstractDefn, MaybeCJCsDefn1),
                        DetailsAbs = AbstractDefn ^ td_ctor_defn,
                        ( if DetailsAbs = abstract_type_general then
                            % There is nothing that including AbstractDefn
                            % in the implementation can tell readers of the
                            % .int file that they don't already get from
                            % SrcIntDefns0.
                            SrcImpDefns = []
                        else
                            % XXX None of the available values of
                            % std_abs_type_status fit this use case.
                            % XXX Should we replace SrcIntDefns0 with
                            % AbstractDefn, and SrcImpDefns with []?
                            SrcImpDefns =
                                [wrap_abstract_type_defn(AbstractDefn)]
                        )
                    ),
                    SrcDefnsStd = src_defns_std(SrcIntDefns0, SrcImpDefns, []),
                    TypeCtorCheckedDefn =
                        checked_defn_std(StdTypeDefn, SrcDefnsStd),
                    map.det_insert(TypeCtor, TypeCtorCheckedDefn,
                        !TypeCtorCheckedMap)
                else
                    % This type has a du Mercury definition, and one or more
                    % foreign type definitions left in the implementation
                    % section, which means we have two or more definitions
                    % of the type in the implementation section. Keeping
                    % all of those definitions preserves old behavior.
                    (
                        Extras0 = extras_enum(HeadCtor, TailCtors, _),
                        wrap_cjcs_foreign_type_no_enums(MaybeCJCsDefn1,
                            MaybeCJCsDefnOrEnum),
                        StdTypeDefn = std_mer_type_du_all_plain_constants(
                            DuStatus, DuDefn, HeadCtor, TailCtors,
                            MaybeCJCsDefnOrEnum)
                    ;
                        Extras0 = extras_non_enum(_),
                        StdTypeDefn = std_mer_type_du_not_all_plain_constants(
                            DuStatus, DuDefn, MaybeCJCsDefn1)
                    ),
                    SrcDefnsStd =
                        src_defns_std(SrcIntDefns0, SrcImpDefns0, []),
                    list.foldl(record_foreign_lang_in_type_defn,
                        SrcImpDefns0, !ImpImplicitFIMLangs),
                    TypeCtorCheckedDefn =
                        checked_defn_std(StdTypeDefn, SrcDefnsStd),
                    map.det_insert(TypeCtor, TypeCtorCheckedDefn,
                        !TypeCtorCheckedMap)
                )
            else
                make_du_type_defn_abstract(BothTypesMap,
                    DuDefn, MaybeAbstractDefn),
                (
                    MaybeAbstractDefn = no,
                    % We need to tell the readers of the .int file that
                    % this type is a dummy type, but there is no way
                    % an abstract definition can tell them that. We therefore
                    % have to tell them that by including DuDefn in the
                    % implementation section.
                    AbstractDefn = DuDefn ^ td_ctor_defn
                        := abstract_type_general,
                    SrcImpDefns = [wrap_du_type_defn(DuDefn)]
                ;
                    MaybeAbstractDefn = yes(AbstractDefn),
                    % The AbstractDefn in the interface says everything
                    % we want to say about this type.
                    SrcImpDefns = []
                ),
                AbstractStatus = std_abs_type_abstract_exported,
                MaybeCJCsDefn = c_java_csharp(no, no, no),
                % XXX Should we use SrcIntDefns?
                % SrcIntDefns = [wrap_abstract_type_defn(AbstractDefn)],
                StdTypeDefn = std_mer_type_abstract(AbstractStatus,
                    AbstractDefn, MaybeCJCsDefn),
                SrcDefnsStd = src_defns_std(SrcIntDefns0, SrcImpDefns, []),
                TypeCtorCheckedDefn =
                    checked_defn_std(StdTypeDefn, SrcDefnsStd),
                map.det_insert(TypeCtor, TypeCtorCheckedDefn,
                    !TypeCtorCheckedMap)
            )
        ;
            DuStatus = std_du_type_all_private,
            % Since we do not export the Mercury function symbols,
            % we delete any foreign enum definition from the implementation
            % section. We also delete any foreign type definition from
            % in implementation section if TypeCtor is not in
            % NeededImpTypeCtors.
            delete_any_foreign_enum_from_extras(Extras0, MaybeCJCsDefn1),
            ( if set.member(TypeCtor, NeededImpTypeCtors) then
                ( if MaybeCJCsDefn1 = c_java_csharp(no, no, no) then
                    % This type has only a du Mercury definition in the
                    % implementation section. Making it abstract
                    % preserves old behavior.
                    make_du_type_defn_abstract(BothTypesMap,
                        DuDefn, MaybeAbstractDefn),
                    (
                        MaybeAbstractDefn = no,
                        StdTypeDefn = StdTypeDefn0,
                        SrcImpDefns = [wrap_du_type_defn(DuDefn)]
                    ;
                        MaybeAbstractDefn = yes(AbstractDefn),
                        % XXX None of the available values of
                        % std_abs_type_status fit this use case.
                        AbstractStatus = std_abs_type_abstract_exported,
                        StdTypeDefn = std_mer_type_abstract(AbstractStatus,
                            AbstractDefn, MaybeCJCsDefn1),
                        SrcImpDefns = [wrap_abstract_type_defn(AbstractDefn)]
                    ),
                    SrcDefnsStd = src_defns_std([], SrcImpDefns, []),
                    TypeCtorCheckedDefn =
                        checked_defn_std(StdTypeDefn, SrcDefnsStd),
                    map.det_insert(TypeCtor, TypeCtorCheckedDefn,
                        !TypeCtorCheckedMap)
                else
                    % This type has a du Mercury definition, and
                    % one or more foreign type definitions in the
                    % implementation section, which means we have two
                    % or more definitions of the type in the implementation
                    % section. Keeping all of those definitions
                    % preserves old behavior. However, we do delete
                    % any foreign enum items.
                    SrcDefnsStd =
                        src_defns_std(SrcIntDefns0, SrcImpDefns0, []),
                    list.foldl(record_foreign_lang_in_type_defn,
                        SrcImpDefns0, !ImpImplicitFIMLangs),
                    TypeCtorCheckedDefn =
                        checked_defn_std(StdTypeDefn0, SrcDefnsStd),
                    map.det_insert(TypeCtor, TypeCtorCheckedDefn,
                        !TypeCtorCheckedMap)
                )
            else
                true
            )
        )
    ;
        StdTypeDefn0 = std_mer_type_abstract(AbstractStatus,
            AbstractDefn, _MaybeCJCsDefn0),
        (
            AbstractStatus = std_abs_type_ft_exported,
            % The entirety of this type is in the interface.
            list.foldl(record_foreign_lang_in_type_defn,
                SrcImpDefns0, !ImpImplicitFIMLangs),
            map.det_insert(TypeCtor, TypeCtorCheckedDefn0,
                !TypeCtorCheckedMap)
        ;
            AbstractStatus = std_abs_type_abstract_exported,
            ( if set.member(TypeCtor, NeededImpTypeCtors) then
                % This type has an abstract Mercury declaration in the
                % interface and one or more foreign type definitions
                % in the implementation section, but we want both
                % where they are.
                list.foldl(record_foreign_lang_in_type_defn,
                    SrcImpDefns0, !ImpImplicitFIMLangs),
                map.det_insert(TypeCtor, TypeCtorCheckedDefn0,
                    !TypeCtorCheckedMap)
            else
                MaybeCJCsDefn = c_java_csharp(no, no, no),
                StdTypeDefn = std_mer_type_abstract(AbstractStatus,
                    AbstractDefn, MaybeCJCsDefn),
                SrcDefnsStd = src_defns_std(SrcIntDefns0, [], []),
                TypeCtorCheckedDefn = checked_defn_std(StdTypeDefn,
                    SrcDefnsStd),
                map.det_insert(TypeCtor, TypeCtorCheckedDefn,
                    !TypeCtorCheckedMap)
            )
        ;
            AbstractStatus = std_abs_type_all_private,
            ( if set.member(TypeCtor, NeededImpTypeCtors) then
                % This type has both an abstract Mercury declaration
                % and one or more foreign type definitions in the
                % implementation section, and we want both where they are.
                list.foldl(record_foreign_lang_in_type_defn,
                    SrcImpDefns0, !ImpImplicitFIMLangs),
                map.det_insert(TypeCtor, TypeCtorCheckedDefn0,
                    !TypeCtorCheckedMap)
            else
                true
            )
        )
    ).

:- pred make_du_type_defn_abstract(type_defn_map::in,
    item_type_defn_info_du::in, maybe(item_type_defn_info_abstract)::out)
    is det.

make_du_type_defn_abstract(BothTypesMap, DuDefnInfo, MaybeAbstractDefnInfo) :-
    % XXX TYPE_REPN We should record the aspects of the type definition
    % that are relevant to type representation (such as "is dummy",
    % "fits in n bits", "is equivalent to ...") in a type_repn item,
    % and then make the type definition abstract.
    DuDefnInfo = item_type_defn_info(_, _, DetailsDu, TVarSet, _, _),
    DetailsDu = type_details_du(OoMCtors, MaybeEqCmp, MaybeDirectArgCtors),
    ( if
        non_sub_du_constructor_list_represents_dummy_type(BothTypesMap,
            TVarSet, OoMCtors, MaybeEqCmp, MaybeDirectArgCtors)
    then
        % We cannot return DetailsAbs = abstract_dummy_type, because
        % parse_tree_out.m writes out abstract_dummy_types as if they were
        % abstract_type_general, which means that if we output
        % AbstractDefnInfo, readers of the .int file won't know that
        % the type is abstract.
        %
        % The only way we can tell them that is to keep the original
        % DuDefnInfo. We tell our caller that by returning nothing.
        MaybeAbstractDefnInfo = no
    else
        ( if non_sub_du_type_is_enum(DetailsDu, NumFunctors) then
            num_bits_needed_for_n_dense_values(NumFunctors, NumBits),
            DetailsAbs = abstract_type_fits_in_n_bits(NumBits)
        else
            DetailsAbs = abstract_type_general
        ),
        AbstractDefnInfo = DuDefnInfo ^ td_ctor_defn := DetailsAbs,
        MaybeAbstractDefnInfo = yes(AbstractDefnInfo)
    ).

:- func make_subtype_defn_abstract(item_type_defn_info_sub)
    = item_type_defn_info_abstract.

make_subtype_defn_abstract(SubDefn) = AbstractDefn :-
    TypeDefn = SubDefn ^ td_ctor_defn,
    SuperType = TypeDefn ^ sub_supertype,
    type_to_ctor_det(SuperType, SuperTypeCtor),
    AbstractDefn = SubDefn ^ td_ctor_defn := abstract_subtype(SuperTypeCtor).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % generate_interface_int2(AugMakeIntUnit,
    %   IntExplicitFIMSpecs, ImpExplicitFIMSpecs,
    %   TypeCtorCheckedMap, InstCtorCheckedMap, ModeCtorCheckedMap,
    %   TypeCtorRepnMap, ParseTreeInt2):
    %
    % The input arguments should be the relevant parts of the .int1 file
    % computed by our parent.
    %
:- pred generate_interface_int2(aug_make_int_unit::in,
    set(fim_spec)::in, set(fim_spec)::in,
    type_ctor_checked_map::in, inst_ctor_checked_map::in,
    mode_ctor_checked_map::in, type_ctor_repn_map::in,
    parse_tree_int2::out) is det.

generate_interface_int2(AugMakeIntUnit,
        IntExplicitFIMSpecs, ImpExplicitFIMSpecs,
        TypeCtorCheckedMap, InstCtorCheckedMap, ModeCtorCheckedMap,
        TypeCtorRepnMap, ParseTreeInt2) :-
    AugMakeIntUnit = aug_make_int_unit(ParseTreeModuleSrc, _, _, _, _),
    ModuleName = ParseTreeModuleSrc ^ ptms_module_name,
    ModuleNameContext = ParseTreeModuleSrc ^ ptms_module_name_context,

    InclMap = ParseTreeModuleSrc ^ ptms_include_map,
    map.foldl(add_only_int_include, InclMap, map.init, ShortIntInclMap),
    IntTypeClasses = ParseTreeModuleSrc ^ ptms_int_typeclasses,
    IntInstances = ParseTreeModuleSrc ^ ptms_int_instances,

    some [!UnqualSymNames, !UsedModuleNames] (
        !:UnqualSymNames = no_unqual_symnames,
        set.init(!:UsedModuleNames),

        map.foldl5(restrict_type_ctor_checked_defn_for_int2,
            TypeCtorCheckedMap,
            map.init, ShortTypeCtorCheckedMap,
            !UnqualSymNames, !UsedModuleNames,
            set.init, ShortIntImplicitFIMLangs,
            set.init, ShortImpImplicitFIMLangs),

        map.foldl2_values(restrict_inst_ctor_checked_defn_for_int2,
            InstCtorCheckedMap, !UnqualSymNames, !UsedModuleNames),
        map.foldl2_values(restrict_mode_ctor_checked_defn_for_int2,
            ModeCtorCheckedMap, !UnqualSymNames, !UsedModuleNames),

        get_int2_items_from_int1_int_typeclass(IntTypeClasses,
            !UnqualSymNames, !UsedModuleNames,
            cord.init, ShortIntTypeClassesCord),
        get_int2_items_from_int1_int_instance(IntInstances,
            !UnqualSymNames, !UsedModuleNames,
            cord.init, ShortIntInstancesCord),

        ShortIntTypeClasses = cord.list(ShortIntTypeClassesCord),
        ShortIntInstances = cord.list(ShortIntInstancesCord),

        UnqualSymNames = !.UnqualSymNames,
        UsedModuleNames = !.UsedModuleNames
    ),

    ImportUseMap = ParseTreeModuleSrc ^ ptms_import_use_map,
    map.foldl(
        make_imports_into_uses_int_only(UnqualSymNames, UsedModuleNames),
        ImportUseMap, map.init, ShortUseOnlyMap),

    % If there is nothing involving a foreign language in the interface,
    % then we do not need either explicit or implicit FIMs for that
    % language in the interface.
    set.filter(fim_spec_is_for_needed_language(ShortIntImplicitFIMLangs),
        IntExplicitFIMSpecs, ShortIntExplicitFIMSpecs),
    set.foldl(add_self_fim(ModuleName), ShortIntImplicitFIMLangs,
        ShortIntExplicitFIMSpecs, ShortIntFIMSpecs),

    % The same is true for the implementation section, with two
    % differences. One is that the implementation section may need
    % a language that the interface does not, and there is an
    % explicit FIM for this language that we did not include
    % in the interface, we must include it in the implementation.
    % Second, we don't want to include a FIM in *both* the interface
    % and the implementation.
    set.union(IntExplicitFIMSpecs, ImpExplicitFIMSpecs, ExplicitFIMSpecs),
    set.filter(fim_spec_is_for_needed_language(ShortImpImplicitFIMLangs),
        ExplicitFIMSpecs, ShortImpExplicitFIMSpecs),
    set.foldl(add_self_fim(ModuleName), ShortImpImplicitFIMLangs,
        ShortImpExplicitFIMSpecs, ShortImpFIMSpecs0),
    set.difference(ShortImpFIMSpecs0, ShortIntFIMSpecs, ShortImpFIMSpecs),

    DummyMaybeVersionNumbers = no_version_numbers,

    ParseTreeInt2 = parse_tree_int2(ModuleName, ModuleNameContext,
        DummyMaybeVersionNumbers, ShortIntInclMap,
        ShortUseOnlyMap, ShortIntFIMSpecs, ShortImpFIMSpecs,
        ShortTypeCtorCheckedMap, InstCtorCheckedMap, ModeCtorCheckedMap,
        ShortIntTypeClasses, ShortIntInstances, TypeCtorRepnMap).

%---------------------%

:- pred fim_spec_is_for_needed_language(set(foreign_language)::in,
    fim_spec::in) is semidet.

fim_spec_is_for_needed_language(NeededLangs, FIMSpec) :-
    FIMSpec = fim_spec(Lang, _ModuleName),
    set.contains(NeededLangs, Lang).

:- pred make_imports_into_uses_int_only(
    maybe_unqual_symnames::in, set(module_name)::in,
    module_name::in, maybe_implicit_import_and_or_use::in,
    section_use_map::in, section_use_map::out) is det.

make_imports_into_uses_int_only(UnqualSymNames, UsedModuleNames,
        ModuleName, ImportUse0, !ShortUseOnlyMap) :-
    ( if
        UnqualSymNames = no_unqual_symnames,
        not set.contains(UsedModuleNames, ModuleName)
    then
        % If every sym_name in the .int2 file is fully module qualified,
        % then we keep every use_module declarations only for the modules
        % that they name.
        % This requires UsedModuleNames to cover even implicitly used
        % module names.
        true
    else
        (
            ImportUse0 = explicit_avail(Explicit0),
            ( if make_imports_into_uses_int_only(Explicit0, Explicit) then
                map.det_insert(ModuleName, Explicit, !ShortUseOnlyMap)
            else
                true
            )
        ;
            ImportUse0 = implicit_avail(_Implicit0, MaybeExplicit0),
            ( if
                MaybeExplicit0 = yes(Explicit0),
                make_imports_into_uses_int_only(Explicit0, Explicit)
            then
                map.det_insert(ModuleName, Explicit, !ShortUseOnlyMap)
            else
                true
            )
        )
    ).

:- pred make_imports_into_uses_int_only(section_import_and_or_use::in,
    section_use::out) is semidet.

make_imports_into_uses_int_only(Explicit0, Explicit) :-
    require_complete_switch [Explicit0]
    (
        ( Explicit0 = int_import(IntContext)
        ; Explicit0 = int_use(IntContext)
        ; Explicit0 = int_use_imp_import(IntContext, _ImpContext)
        ),
        Explicit = int_use(IntContext)
    ;
        ( Explicit0 = imp_import(_ImpContext)
        ; Explicit0 = imp_use(_ImpContext)
        ),
        fail
    ).

%---------------------%

:- pred restrict_type_ctor_checked_defn_for_int2(type_ctor::in,
    type_ctor_checked_defn::in,
    type_ctor_checked_map::in, type_ctor_checked_map::out,
    maybe_unqual_symnames::in, maybe_unqual_symnames::out,
    set(module_name)::in, set(module_name)::out,
    set(foreign_language)::in, set(foreign_language)::out,
    set(foreign_language)::in, set(foreign_language)::out) is det.

restrict_type_ctor_checked_defn_for_int2(TypeCtor, TypeCtorCheckedDefn0,
        !ShortTypeCtorCheckedMap, !MaybeUnqual, !ModuleNames,
        !IntImplicitFIMLangs, !ImpImplicitFIMLangs) :-
    % For now, we need the implementation sections of .int2 files to contain
    % all the information that other modules reading that .int file will need
    % to correctly decide the representation of the types exported by this
    % module.
    %
    % The computation we use to decide which types' type_defn items
    % need to stay in the implementation section of the .int file,
    % and in what form, computes exactly this information. Therefore
    % we need only the copy the type_defn items that this previous
    % computation has given us.
    %
    % XXX TYPE_REPN In the future, these type_defn items (which include
    % some for types that *shouldn't* be exported from the module)
    % should be replaced by type_repn items (for only the types which
    % *are* exported from the module).
    %
    % The implementation section of .int2 files needs no other items,
    % and when we switch to using type_repn items to decide type
    % representations, the implementation sections of .int2 files
    % should be empty (as are the implementation sections of .int3 files).
    %
    % XXX CLEANUP We update only the source definition half of each checked
    % definition, and leave the actual definition part alone. This is
    % sufficient for our current needs, because the code that generates
    % .int2 files looks only at the source definitions. If we ever gave
    % the compiler the ability to both construct a .int2 file, and use it,
    % in the same compiler invocation, *without* reading in the .int2 file
    % again, we would have to fix that.
    (
        TypeCtorCheckedDefn0 =
            checked_defn_solver(SolverTypeDefn0, SrcDefnsSolver0),
        SolverTypeDefn = SolverTypeDefn0,
        SrcDefnsSolver0 = src_defns_solver(MaybeIntDefn0, MaybeImpDefn),
        maybe.map_fold3_maybe(restrict_type_ctor_int_defn_for_int2,
            MaybeIntDefn0, MaybeIntDefn,
            !MaybeUnqual, !ModuleNames, !IntImplicitFIMLangs),
        SrcDefnsSolver = src_defns_solver(MaybeIntDefn, MaybeImpDefn),
        TypeCtorCheckedDefn =
            checked_defn_solver(SolverTypeDefn, SrcDefnsSolver)
    ;
        TypeCtorCheckedDefn0 = checked_defn_std(StdTypeDefn0, SrcDefnsStd0),
        StdTypeDefn = StdTypeDefn0,
        SrcDefnsStd0 = src_defns_std(IntTypeDefns0, ImpTypeDefns,
            _ImpForeignEnums),
        list.map_foldl3(restrict_type_ctor_int_defn_for_int2,
            IntTypeDefns0, IntTypeDefns,
            !MaybeUnqual, !ModuleNames, !IntImplicitFIMLangs),
        get_int2_langs_from_int1_imp_types(ImpTypeDefns, !ImpImplicitFIMLangs),
        % Foreign enums are never included in .int2 files.
        SrcDefnsStd = src_defns_std(IntTypeDefns, ImpTypeDefns, []),
        TypeCtorCheckedDefn = checked_defn_std(StdTypeDefn, SrcDefnsStd)
    ),
    map.det_insert(TypeCtor, TypeCtorCheckedDefn, !ShortTypeCtorCheckedMap).

:- pred restrict_type_ctor_int_defn_for_int2(
    item_type_defn_info::in, item_type_defn_info::out,
    maybe_unqual_symnames::in, maybe_unqual_symnames::out,
    set(module_name)::in, set(module_name)::out,
    set(foreign_language)::in, set(foreign_language)::out) is det.

restrict_type_ctor_int_defn_for_int2(TypeDefnInfo0, TypeDefnInfo,
        !MaybeUnqual, !ModuleNames, !IntImplicitFIMLangs) :-
    % generate_pre_grab_pre_qual_interface_for_int1_int2 had invoked
    % delete_uc_preds_make_solver_type_dummy on type_defn items
    % in the implementation section of the module. We now do the same job
    % on type_defn items in the interface section, but we also make any
    % solver types abstract.
    TypeDefn0 = TypeDefnInfo0 ^ td_ctor_defn,
    (
        TypeDefn0 = parse_tree_du_type(DetailsDu0),
        delete_uc_preds_from_du_type(DetailsDu0, DetailsDu),
        TypeDefn = parse_tree_du_type(DetailsDu),
        TypeDefnInfo = TypeDefnInfo0 ^ td_ctor_defn := TypeDefn
        % XXX DetailsDu cannot refer to other modules in its MaybeCanon
        % field, but it *can* refer to other modules in the argument types
        % of its constructors.
        % zs: This *should* be ok, in that the code consuming the .int2 file
        % should not need to do anything with the types of those arguments,
        % but I would like to see a correctness argument for that.
    ;
        TypeDefn0 = parse_tree_sub_type(_),
        % The consideration just above about the types of constructors
        % in du types applies also to subtypes.
        TypeDefnInfo = TypeDefnInfo0
    ;
        TypeDefn0 = parse_tree_solver_type(_),
        % TypeDefnInfo cannot refer to other modules.
        % XXX ITEM_LIST This should not be necessary, since a full
        % (i.e. non-abstract) solver type definition in the interface section
        % is an error that should have been caught and reported
        % when we constructed the type_ctor_checked_map.
        % TypeDefn = parse_tree_abstract_type(abstract_solver_type),
        % TypeDefnInfo = TypeDefnInfo0 ^ td_ctor_defn := TypeDefn
        unexpected($pred, "parse_tree_abstract_type")
    ;
        TypeDefn0 = parse_tree_abstract_type(_),
        % TypeDefnInfo0 cannot refer to other modules.
        TypeDefnInfo = TypeDefnInfo0
    ;
        TypeDefn0 = parse_tree_foreign_type(DetailsForeign0),
        delete_uc_preds_from_foreign_type(DetailsForeign0, DetailsForeign),
        TypeDefn = parse_tree_foreign_type(DetailsForeign),
        TypeDefnInfo = TypeDefnInfo0 ^ td_ctor_defn := TypeDefn,
        % Foreign types can never refer to Mercury code in other modules,
        % but they can refer to *target language* code in other modules.
        DetailsForeign = type_details_foreign(ForeignType, _, _),
        Lang = foreign_type_language(ForeignType),
        set.insert(Lang, !IntImplicitFIMLangs)
    ;
        TypeDefn0 = parse_tree_eqv_type(DetailsEqv0),
        TypeDefnInfo = TypeDefnInfo0,
        DetailsEqv0 = type_details_eqv(EqvType0),
        accumulate_modules_in_type(EqvType0, !MaybeUnqual, !ModuleNames)
    ).

:- pred restrict_inst_ctor_checked_defn_for_int2(inst_ctor_checked_defn::in,
    maybe_unqual_symnames::in, maybe_unqual_symnames::out,
    set(module_name)::in, set(module_name)::out) is det.

restrict_inst_ctor_checked_defn_for_int2(InstCtorCheckedDefn,
        !MaybeUnqual, !ModuleNames) :-
    InstCtorCheckedDefn = checked_defn_inst(StdDefn, _SrcDefns),
    StdDefn = std_inst_defn(_Status, InstDefnInfo),
    InstDefnInfo = item_inst_defn_info(_SymName, _InstArgVars,
        MaybeForTypeCtor, MaybeAbstractInstDefn, _InstVarSet,
        _Context, _SeqNum),
    (
        MaybeForTypeCtor = no
    ;
        MaybeForTypeCtor = yes(TypeCtor),
        TypeCtor = type_ctor(TypeCtorSymName, _TypectorArity),
        accumulate_module(TypeCtorSymName, !MaybeUnqual, !ModuleNames)
    ),
    (
        MaybeAbstractInstDefn = abstract_inst_defn
    ;
        MaybeAbstractInstDefn = nonabstract_inst_defn(InstDefn),
        InstDefn = eqv_inst(Inst),
        accumulate_modules_in_inst(Inst, !MaybeUnqual, !ModuleNames)
    ).

:- pred restrict_mode_ctor_checked_defn_for_int2(mode_ctor_checked_defn::in,
    maybe_unqual_symnames::in, maybe_unqual_symnames::out,
    set(module_name)::in, set(module_name)::out) is det.

restrict_mode_ctor_checked_defn_for_int2(ModeCtorCheckedDefn,
        !MaybeUnqual, !ModuleNames) :-
    ModeCtorCheckedDefn = checked_defn_mode(StdDefn, _SrcDefns),
    StdDefn = std_mode_defn(_Status, ModeDefnInfo),
    ModeDefnInfo = item_mode_defn_info(_SymName, _InstArgVars,
        MaybeAbstractModeDefn, _InstVarSet, _Context, _SeqNum),
    (
        MaybeAbstractModeDefn = abstract_mode_defn
    ;
        MaybeAbstractModeDefn = nonabstract_mode_defn(ModeDefn),
        ModeDefn = eqv_mode(Mode),
        accumulate_modules_in_mode(Mode, !MaybeUnqual, !ModuleNames)
    ).

:- pred get_int2_items_from_int1_int_typeclass(list(item_typeclass_info)::in,
    maybe_unqual_symnames::in, maybe_unqual_symnames::out,
    set(module_name)::in, set(module_name)::out,
    cord(item_typeclass_info)::in, cord(item_typeclass_info)::out) is det.

get_int2_items_from_int1_int_typeclass([],
        !MaybeUnqual, !ModuleNames, !IntTypeClassesCord).
get_int2_items_from_int1_int_typeclass([TypeClassInfo | TypeClassInfos],
        !MaybeUnqual, !ModuleNames, !IntTypeClassesCord) :-
    TypeClassInfo = item_typeclass_info(ClassSymName, TypeParams,
        SuperclassConstraints, FunDeps, _Methods0, TVarSet, Context, SeqNum),
    accumulate_modules_in_constraints(SuperclassConstraints,
        !MaybeUnqual, !ModuleNames),
    Methods = class_interface_abstract,
    AbstractTypeClassInfo = item_typeclass_info(ClassSymName, TypeParams,
        SuperclassConstraints, FunDeps, Methods, TVarSet, Context, SeqNum),
    cord.snoc(AbstractTypeClassInfo, !IntTypeClassesCord),
    get_int2_items_from_int1_int_typeclass(TypeClassInfos,
        !MaybeUnqual, !ModuleNames, !IntTypeClassesCord).

:- pred get_int2_items_from_int1_int_instance(list(item_instance_info)::in,
    maybe_unqual_symnames::in, maybe_unqual_symnames::out,
    set(module_name)::in, set(module_name)::out,
    cord(item_abstract_instance_info)::in,
        cord(item_abstract_instance_info)::out) is det.

get_int2_items_from_int1_int_instance([],
        !MaybeUnqual, !ModuleNames, !IntInstancesCord).
get_int2_items_from_int1_int_instance([InstanceInfo | InstanceInfos],
        !MaybeUnqual, !ModuleNames, !IntInstancesCord) :-
    InstanceInfo = item_instance_info(ClassSymName,
        ArgTypes, OrigArgTypes, ClassConstraints, InstanceBody0,
        TVarSet, ContainingModuleName, Context, SeqNum),
    expect(unify(InstanceBody0, instance_body_abstract), $pred,
        "instance_body_abstract"),
    accumulate_module(ClassSymName, !MaybeUnqual, !ModuleNames),
    accumulate_modules_in_types(ArgTypes, !MaybeUnqual, !ModuleNames),
    accumulate_modules_in_types(OrigArgTypes, !MaybeUnqual, !ModuleNames),
    accumulate_modules_in_constraints(ClassConstraints,
        !MaybeUnqual, !ModuleNames),
    InstanceBody = instance_body_abstract,
    AbstractInstanceInfo = item_instance_info(ClassSymName,
        ArgTypes, OrigArgTypes, ClassConstraints, InstanceBody,
        TVarSet, ContainingModuleName, Context, SeqNum),
    cord.snoc(AbstractInstanceInfo, !IntInstancesCord),
    get_int2_items_from_int1_int_instance(InstanceInfos,
        !MaybeUnqual, !ModuleNames, !IntInstancesCord).

%---------------------%

:- pred get_int2_langs_from_int1_imp_types(list(item_type_defn_info)::in,
    set(foreign_language)::in, set(foreign_language)::out) is det.

get_int2_langs_from_int1_imp_types([], !ImpImplicitFIMLangs).
get_int2_langs_from_int1_imp_types([ImpTypeDefn | ImpTypeDefns],
        !ImpImplicitFIMLangs) :-
    TypeDefn = ImpTypeDefn ^ td_ctor_defn,
    ( if TypeDefn = parse_tree_foreign_type(DetailsForeign) then
        DetailsForeign = type_details_foreign(ForeignType, _, _),
        Lang = foreign_type_language(ForeignType),
        set.insert(Lang, !ImpImplicitFIMLangs)
    else
        true
    ),
    get_int2_langs_from_int1_imp_types(ImpTypeDefns, !ImpImplicitFIMLangs).

%---------------------------------------------------------------------------%

:- pred delete_uc_preds_from_du_type_defn(
    item_type_defn_info_du::in, item_type_defn_info_du::out) is det.

delete_uc_preds_from_du_type_defn(ItemTypeDefn0, ItemTypeDefn) :-
    DetailsDu0 = ItemTypeDefn0 ^ td_ctor_defn,
    delete_uc_preds_from_du_type(DetailsDu0, DetailsDu),
    ItemTypeDefn = ItemTypeDefn0 ^ td_ctor_defn := DetailsDu.

:- pred delete_uc_preds_from_c_j_cs_maybe_defn_or_enum(
    c_j_cs_maybe_defn_or_enum::in, c_j_cs_maybe_defn_or_enum::out) is det.

delete_uc_preds_from_c_j_cs_maybe_defn_or_enum(CJCsMaybeDefnOrEnum0,
        CJCsMaybeDefnOrEnum) :-
    CJCsMaybeDefnOrEnum0 = c_java_csharp(MaybeDefnOrEnumC0, MaybeDefnOrEnumJ0,
        MaybeDefnOrEnumCs0),
    delete_uc_preds_from_maybe_foreign_type_defn_or_enum(MaybeDefnOrEnumC0,
        MaybeDefnOrEnumC),
    delete_uc_preds_from_maybe_foreign_type_defn_or_enum(MaybeDefnOrEnumJ0,
        MaybeDefnOrEnumJ),
    delete_uc_preds_from_maybe_foreign_type_defn_or_enum(MaybeDefnOrEnumCs0,
        MaybeDefnOrEnumCs),
    CJCsMaybeDefnOrEnum = c_java_csharp(MaybeDefnOrEnumC,
        MaybeDefnOrEnumJ, MaybeDefnOrEnumCs).

:- pred delete_uc_preds_from_c_j_cs_maybe_defn(
    c_j_cs_maybe_defn::in, c_j_cs_maybe_defn::out) is det.

delete_uc_preds_from_c_j_cs_maybe_defn(CJCsMaybeDefn0, CJCsMaybeDefn) :-
    CJCsMaybeDefn0 = c_java_csharp(MaybeDefnC0, MaybeDefnJ0, MaybeDefnCs0),
    delete_uc_preds_from_maybe_foreign_type_defn(MaybeDefnC0, MaybeDefnC),
    delete_uc_preds_from_maybe_foreign_type_defn(MaybeDefnJ0, MaybeDefnJ),
    delete_uc_preds_from_maybe_foreign_type_defn(MaybeDefnCs0, MaybeDefnCs),
    CJCsMaybeDefn = c_java_csharp(MaybeDefnC, MaybeDefnJ, MaybeDefnCs).

:- pred delete_uc_preds_from_maybe_foreign_type_defn_or_enum(
    maybe(foreign_type_or_enum)::in, maybe(foreign_type_or_enum)::out) is det.

delete_uc_preds_from_maybe_foreign_type_defn_or_enum(MaybeDefnOrEnum0,
        MaybeDefnOrEnum) :-
    (
        MaybeDefnOrEnum0 = no,
        MaybeDefnOrEnum = no
    ;
        MaybeDefnOrEnum0 = yes(DefnOrEnum0),
        (
            DefnOrEnum0 = foreign_type_or_enum_enum(_),
            MaybeDefnOrEnum = MaybeDefnOrEnum0
        ;
            DefnOrEnum0 = foreign_type_or_enum_type(ItemTypeDefn0),
            DetailsForeign0 = ItemTypeDefn0 ^ td_ctor_defn,
            delete_uc_preds_from_foreign_type(DetailsForeign0, DetailsForeign),
            ItemTypeDefn = ItemTypeDefn0 ^ td_ctor_defn := DetailsForeign,
            DefnOrEnum = foreign_type_or_enum_type(ItemTypeDefn),
            MaybeDefnOrEnum = yes(DefnOrEnum)
        )
    ).

:- pred delete_uc_preds_from_maybe_foreign_type_defn(
    maybe(item_type_defn_info_foreign)::in,
    maybe(item_type_defn_info_foreign)::out) is det.

delete_uc_preds_from_maybe_foreign_type_defn(MaybeDefn0, MaybeDefn) :-
    (
        MaybeDefn0 = no,
        MaybeDefn = no
    ;
        MaybeDefn0 = yes(ItemTypeDefn0),
        DetailsForeign0 = ItemTypeDefn0 ^ td_ctor_defn,
        delete_uc_preds_from_foreign_type(DetailsForeign0, DetailsForeign),
        ItemTypeDefn = ItemTypeDefn0 ^ td_ctor_defn := DetailsForeign,
        MaybeDefn = yes(ItemTypeDefn)
    ).

    % XXX TYPE_REPN Consider the relationship between this predicate and
    % make_impl_type_abstract in write_module_interface_files.m. Unlike this
    % predicate, that one has access to the definitions of the types
    % in this module, so it knows whether e.g. an equivalence type definition
    % makes the defined type equivalent to a type that needs special treatment
    % by the algorithm that decides data representations.
    %
:- pred delete_uc_preds_make_solver_type_dummy(
    item_type_defn_info::in, item_type_defn_info::out) is det.

delete_uc_preds_make_solver_type_dummy(ItemTypeDefn0, ItemTypeDefn) :-
    TypeDefn0 = ItemTypeDefn0 ^ td_ctor_defn,
    (
        TypeDefn0 = parse_tree_du_type(DetailsDu0),
        delete_uc_preds_from_du_type(DetailsDu0, DetailsDu),
        TypeDefn = parse_tree_du_type(DetailsDu),
        ItemTypeDefn = ItemTypeDefn0 ^ td_ctor_defn := TypeDefn
    ;
        TypeDefn0 = parse_tree_sub_type(_),
        ItemTypeDefn = ItemTypeDefn0
    ;
        TypeDefn0 = parse_tree_abstract_type(_),
        ItemTypeDefn = ItemTypeDefn0
    ;
        TypeDefn0 = parse_tree_solver_type(_),
        % rafe: XXX we need to also export the details of the
        % forwarding type for the representation and the forwarding
        % pred for initialization.
        ItemTypeDefn = ItemTypeDefn0 ^ td_ctor_defn :=
            parse_tree_solver_type(dummy_solver_type)
    ;
        TypeDefn0 = parse_tree_eqv_type(_),
        % For the `.int2' files, we need the full definitions of
        % equivalence types. They are needed to ensure that
        % non-abstract equivalence types always get fully expanded
        % before code generation, even in modules that only indirectly
        % import the definition of the equivalence type.
        % XXX TYPE_REPN: *After* we have generated a type_repn item
        % including this information, we should be able to make
        % MaybeAbstractItemTypeDefn actually abstract.
        ItemTypeDefn = ItemTypeDefn0
    ;
        TypeDefn0 = parse_tree_foreign_type(DetailsForeign0),
        % We always need the definitions of foreign types
        % to handle inter-language interfacing correctly.
        % However, we want to abstract away any unify and compare predicates.
        delete_uc_preds_from_foreign_type(DetailsForeign0, DetailsForeign),
        TypeDefn = parse_tree_foreign_type(DetailsForeign),
        ItemTypeDefn = ItemTypeDefn0 ^ td_ctor_defn := TypeDefn
    ).

    % Return a dummy solver type definition, one that does not refer
    % to any other modules. We use this to replace actual solver type
    % definitions that will be made abstract later (so we do not lose
    % information we do not intend to lose), but for which we do want
    % to remember the fact that they *do* have a definition, to avoid
    % generating misleading error messages about missing definitions.
    %
:- func dummy_solver_type = type_details_solver.

dummy_solver_type = DetailsSolver :-
    RepnType = tuple_type([], kind_star),
    GroundInst = not_reached,
    AnyInst = not_reached,
    MutableItems = [],
    SolverDetails = solver_type_details(RepnType, GroundInst, AnyInst,
        MutableItems),
    MaybeCanon = canon,
    DetailsSolver = type_details_solver(SolverDetails, MaybeCanon).

:- pred make_du_type_abstract(type_details_du::in, type_details_abstract::out)
    is det.

make_du_type_abstract(DetailsDu, DetailsAbstract) :-
    DetailsDu = type_details_du(Ctors, MaybeCanonical, _MaybeDirectArgCtors),
    ( if non_sub_du_type_is_enum(DetailsDu, NumFunctors) then
        num_bits_needed_for_n_dense_values(NumFunctors, NumBits),
        DetailsAbstract = abstract_type_fits_in_n_bits(NumBits)
    else if non_sub_du_type_is_notag(Ctors, MaybeCanonical) then
        DetailsAbstract = abstract_notag_type
    else if non_sub_du_type_is_dummy(DetailsDu) then
        DetailsAbstract = abstract_dummy_type
    else
        DetailsAbstract = abstract_type_general
    ).

:- pred make_sub_type_abstract(type_details_sub::in,
    type_details_abstract::out) is det.

make_sub_type_abstract(DetailsSub, DetailsAbstract) :-
    DetailsSub = type_details_sub(SuperType, _Ctors),
    type_to_ctor_det(SuperType, SuperTypeCtor),
    DetailsAbstract = abstract_subtype(SuperTypeCtor).

    % For the `.int2' files, we need the full definitions of
    % discriminated union types. Even if the functors for a type
    % are not used within a module, we may need to know them for
    % comparing insts, e.g. for comparing `ground' and `bound(...)'.
    % XXX ITEM_LIST: zs: That may be so, but writing out the type
    % definition unchanged, without something on it that says
    % "use these functors *only* for these purposes",
    % is a bug in my opinion.
    % XXX ITEM_LIST: And most types do NOT have any insts defined for them.
    % We could collect (a) the set of type constructors mentioned
    % explicitly in insts as being for that type, and (b) the set of
    % function symbol/arity pairs that occur in bound insts, and then
    % make the type definition totally abstract unless the type constructor
    % either is in set (a) or a member of Ctors is in set (b).
    %
:- pred delete_uc_preds_from_du_type(type_details_du::in,
    type_details_du::out) is det.

delete_uc_preds_from_du_type(DetailsDu0, DetailsDu) :-
    MaybeCanonical = DetailsDu0 ^ du_canonical,
    (
        MaybeCanonical = canon,
        DetailsDu = DetailsDu0
    ;
        MaybeCanonical = noncanon(_NonCanonical),
        DetailsDu = DetailsDu0 ^ du_canonical
            := noncanon(noncanon_abstract(non_solver_type))
    ).

:- pred delete_uc_preds_from_foreign_type(type_details_foreign(T)::in,
    type_details_foreign(T)::out) is det.

delete_uc_preds_from_foreign_type(DetailsForeign0, DetailsForeign) :-
    MaybeCanonical0 = DetailsForeign0 ^ foreign_canonical,
    (
        MaybeCanonical0 = canon,
        DetailsForeign = DetailsForeign0
    ;
        MaybeCanonical0 = noncanon(_NonCanonical),
        DetailsForeign = DetailsForeign0 ^ foreign_canonical
            := noncanon(noncanon_abstract(non_solver_type))
    ).

%---------------------------------------------------------------------------%

:- func make_inst_defn_abstract(item_inst_defn_info) = item_inst_defn_info.

make_inst_defn_abstract(InstDefn) =
    InstDefn ^ id_inst_defn := abstract_inst_defn.

:- func make_mode_defn_abstract(item_mode_defn_info) = item_mode_defn_info.

make_mode_defn_abstract(ModeDefn) =
    ModeDefn ^ md_mode_defn := abstract_mode_defn.

:- func make_typeclass_abstract(item_typeclass_info) =
    item_abstract_typeclass_info.

make_typeclass_abstract(TypeClassInfo) = AbstractTypeClassInfo :-
    % XXX AbstractTypeClassInfo = TypeClassInfo ^ tc_class_methods :=
    %   class_interface_abstract
    % does not work; it gets an error about TypeClassInfo not being
    % *already* of type item_abstract_typeclass_info.
    TypeClassInfo = item_typeclass_info(ClassName, Params,
        Supers, FunDeps, _, TVarSet, Context, SeqNum),
    AbstractTypeClassInfo = item_typeclass_info(ClassName, Params,
        Supers, FunDeps, class_interface_abstract, TVarSet, Context, SeqNum).

:- func check_typeclass_is_abstract(item_typeclass_info)
    = item_abstract_typeclass_info.

check_typeclass_is_abstract(TypeClassInfo) = AbstractTypeClassInfo :-
    % XXX AbstractTypeClassInfo = TypeClassInfo ^ tc_class_methods :=
    %   class_interface_abstract
    % does not work; it gets an error about TypeClassInfo not being
    % *already* of type item_abstract_typeclass_info.
    TypeClassInfo = item_typeclass_info(ClassName, Params,
        Supers, FunDeps, Methods, TVarSet, Context, SeqNum),
    (
        Methods = class_interface_abstract,
        AbstractTypeClassInfo = item_typeclass_info(ClassName, Params,
            Supers, FunDeps, class_interface_abstract, TVarSet,
            Context, SeqNum)
    ;
        Methods = class_interface_concrete(_),
        unexpected($pred, "class_interface_concrete")
    ).

:- func make_instance_abstract(item_instance_info)
    = item_abstract_instance_info.

make_instance_abstract(InstanceInfo) = AbstractInstanceInfo :-
    % XXX AbstractInstanceInfo = InstanceInfo ^ ci_method_instances :=
    %   instance_body_abstract
    % does not work; it gets an error about InstanceInfo not being
    % *already* of type item_abstract_instance_info.
    InstanceInfo = item_instance_info(ClassName, Types, OrigTypes,
        Constraints, _Methods, TVarSet, Module, Context, SeqNum),
    AbstractInstanceInfo = item_instance_info(ClassName, Types, OrigTypes,
        Constraints, instance_body_abstract, TVarSet, Module, Context, SeqNum).

:- func check_instance_is_abstract(item_instance_info)
    = item_abstract_instance_info.

check_instance_is_abstract(InstanceInfo) = AbstractInstanceInfo :-
    % XXX AbstractInstanceInfo = InstanceInfo ^ ci_method_instances :=
    %   instance_body_abstract
    % does not work; it gets an error about InstanceInfo not being
    % *already* of type item_abstract_instance_info.
    InstanceInfo = item_instance_info(ClassName, Types, OrigTypes,
        Constraints, Methods, TVarSet, Module, Context, SeqNum),
    (
        Methods = instance_body_abstract,
        AbstractInstanceInfo = item_instance_info(ClassName, Types, OrigTypes,
            Constraints, instance_body_abstract, TVarSet, Module,
            Context, SeqNum)
    ;
        Methods = instance_body_concrete(_),
        unexpected($pred, "instance_body_concrete")
    ).

%---------------------------------------------------------------------------%

:- pred wrap_cjcs_foreign_type_no_enums(c_j_cs_maybe_defn::in,
    c_j_cs_maybe_defn_or_enum::out) is det.

wrap_cjcs_foreign_type_no_enums(CJCsMaybeDefn, CJCsMaybeDefnOrEnum) :-
    CJCsMaybeDefn = c_java_csharp(MaybeDefnC, MaybeDefnJava, MaybeDefnCsharp),
    wrap_cjcs_foreign_type_no_enum(MaybeDefnC, MaybeDefnOrEnumC),
    wrap_cjcs_foreign_type_no_enum(MaybeDefnJava, MaybeDefnOrEnumJava),
    wrap_cjcs_foreign_type_no_enum(MaybeDefnCsharp, MaybeDefnOrEnumCsharp),
    CJCsMaybeDefnOrEnum = c_java_csharp(MaybeDefnOrEnumC, MaybeDefnOrEnumJava,
        MaybeDefnOrEnumCsharp).

:- pred wrap_cjcs_foreign_type_no_enum(maybe(item_type_defn_info_foreign)::in,
    maybe(foreign_type_or_enum)::out) is det.

wrap_cjcs_foreign_type_no_enum(MaybeDefn, MaybeDefnOrEnum) :-
    (
        MaybeDefn = no,
        MaybeDefnOrEnum = no
    ;
        MaybeDefn = yes(Defn),
        MaybeDefnOrEnum = yes(foreign_type_or_enum_type(Defn))
    ).

%---------------------------------------------------------------------------%

:- type non_sub_du_extras
    --->    extras_enum(string, list(string), c_j_cs_maybe_defn_or_enum)
    ;       extras_non_enum(c_j_cs_maybe_defn).

:- pred delete_any_foreign_type_defn_from_extras(non_sub_du_extras::in,
    non_sub_du_extras::out) is det.

delete_any_foreign_type_defn_from_extras(Extras0, Extras) :-
    (
        Extras0 = extras_enum(HeadCtor, TailCtors, MaybeCJCsDefnOrEnum0),
        MaybeCJCsDefnOrEnum0 = c_java_csharp(MaybeDefnOrEnumC0,
            MaybeDefnOrEnumJava0, MaybeDefnOrEnumCsharp0),
        delete_any_foreign_type_defn(MaybeDefnOrEnumC0,
            MaybeDefnOrEnumC),
        delete_any_foreign_type_defn(MaybeDefnOrEnumJava0,
            MaybeDefnOrEnumJava),
        delete_any_foreign_type_defn(MaybeDefnOrEnumCsharp0,
            MaybeDefnOrEnumCsharp),
        MaybeCJCsDefnOrEnum = c_java_csharp(MaybeDefnOrEnumC,
            MaybeDefnOrEnumJava, MaybeDefnOrEnumCsharp),
        Extras = extras_enum(HeadCtor, TailCtors, MaybeCJCsDefnOrEnum)
    ;
        Extras0 = extras_non_enum(_MaybeCJCsDefn0),
        MaybeCJCsDefn = c_java_csharp(no, no, no),
        Extras = extras_non_enum(MaybeCJCsDefn)
    ).

:- pred delete_any_foreign_type_defn(maybe(foreign_type_or_enum)::in,
    maybe(foreign_type_or_enum)::out) is det.

delete_any_foreign_type_defn(MaybeDefnOrEnum0, MaybeDefnOrEnum) :-
    (
        MaybeDefnOrEnum0 = no,
        MaybeDefnOrEnum = no
    ;
        MaybeDefnOrEnum0 = yes(DefnOrEnum0),
        (
            DefnOrEnum0 = foreign_type_or_enum_type(_),
            MaybeDefnOrEnum = no
        ;
            DefnOrEnum0 = foreign_type_or_enum_enum(_),
            MaybeDefnOrEnum = MaybeDefnOrEnum0
        )
    ).

:- pred delete_any_foreign_enum_from_extras(non_sub_du_extras::in,
    c_j_cs_maybe_defn::out) is det.

delete_any_foreign_enum_from_extras(Extras0, MaybeCJCsDefn) :-
    (
        Extras0 = extras_enum(_HeadCtor, _TailCtors, MaybeCJCsDefnOrEnum0),
        MaybeCJCsDefnOrEnum0 = c_java_csharp(MaybeDefnOrEnumC0,
            MaybeDefnOrEnumJava0, MaybeDefnOrEnumCsharp0),
        delete_any_foreign_enum(MaybeDefnOrEnumC0, MaybeDefnC),
        delete_any_foreign_enum(MaybeDefnOrEnumJava0, MaybeDefnJava),
        delete_any_foreign_enum(MaybeDefnOrEnumCsharp0, MaybeDefnCsharp),
        MaybeCJCsDefn = c_java_csharp(MaybeDefnC,
            MaybeDefnJava, MaybeDefnCsharp)
    ;
        Extras0 = extras_non_enum(MaybeCJCsDefn)
    ).

:- pred delete_any_foreign_enum(maybe(foreign_type_or_enum)::in,
    maybe(item_type_defn_info_foreign)::out) is det.

delete_any_foreign_enum(MaybeDefnOrEnum0, MaybeDefn) :-
    (
        MaybeDefnOrEnum0 = no,
        MaybeDefn = no
    ;
        MaybeDefnOrEnum0 = yes(DefnOrEnum0),
        (
            DefnOrEnum0 = foreign_type_or_enum_type(Defn),
            MaybeDefn = yes(Defn)
        ;
            DefnOrEnum0 = foreign_type_or_enum_enum(_),
            MaybeDefn = no
        )
    ).

%---------------------------------------------------------------------------%

:- pred record_foreign_lang_in_type_defn(item_type_defn_info::in,
    set(foreign_language)::in, set(foreign_language)::out) is det.

record_foreign_lang_in_type_defn(TypeDefnInfo, !Langs) :-
    TypeDefn = TypeDefnInfo ^ td_ctor_defn,
    (
        ( TypeDefn = parse_tree_du_type(_)
        ; TypeDefn = parse_tree_sub_type(_)
        ; TypeDefn = parse_tree_abstract_type(_)
        ; TypeDefn = parse_tree_solver_type(_)
        ; TypeDefn = parse_tree_eqv_type(_)
        )
    ;
        TypeDefn = parse_tree_foreign_type(DetailsForeign),
        DetailsForeign = type_details_foreign(LangType, _, _),
        ( LangType = c(_), Lang = lang_c
        ; LangType = java(_), Lang = lang_java
        ; LangType = csharp(_), Lang = lang_csharp
        ),
        set.insert(Lang, !Langs)
    ).

:- pred record_foreign_lang_in_foreign_enum(item_foreign_enum_info::in,
    set(foreign_language)::in, set(foreign_language)::out) is det.

record_foreign_lang_in_foreign_enum(ForeignEnumInfo, !Langs) :-
    ForeignEnumInfo = item_foreign_enum_info(Lang, _, _, _, _),
    set.insert(Lang, !Langs).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- pred accumulate_modules_in_constraints(list(prog_constraint)::in,
    maybe_unqual_symnames::in, maybe_unqual_symnames::out,
    set(module_name)::in, set(module_name)::out) is det.

accumulate_modules_in_constraints([], !MaybeUnqual, !ModuleNames).
accumulate_modules_in_constraints([Constraint | Constraints],
        !MaybeUnqual, !ModuleNames) :-
    accumulate_modules_in_constraint(Constraint, !MaybeUnqual, !ModuleNames),
    accumulate_modules_in_constraints(Constraints, !MaybeUnqual, !ModuleNames).

:- pred accumulate_modules_in_constraint(prog_constraint::in,
    maybe_unqual_symnames::in, maybe_unqual_symnames::out,
    set(module_name)::in, set(module_name)::out) is det.

accumulate_modules_in_constraint(Constraint, !MaybeUnqual, !ModuleNames) :-
    Constraint = constraint(ClassSymName, ArgTypes),
    accumulate_module(ClassSymName, !MaybeUnqual, !ModuleNames),
    accumulate_modules_in_types(ArgTypes, !MaybeUnqual, !ModuleNames).

%---------------------%

:- pred accumulate_modules_in_qual_constraint(prog_constraint::in,
    set(module_name)::in, set(module_name)::out) is det.

accumulate_modules_in_qual_constraint(Constraint, !Modules) :-
    Constraint = constraint(ClassSymName, ArgTypes),
    (
        ClassSymName = qualified(ModuleName, _),
        set.insert(ModuleName, !Modules)
    ;
        ClassSymName = unqualified(_),
        unexpected($pred, "unknown typeclass in constraint")
    ),
    accumulate_modules_in_qual_types(ArgTypes, !Modules).

%---------------------%

:- pred accumulate_modules_in_types(list(mer_type)::in,
    maybe_unqual_symnames::in, maybe_unqual_symnames::out,
    set(module_name)::in, set(module_name)::out) is det.

accumulate_modules_in_types([], !MaybeUnqual, !ModuleNames).
accumulate_modules_in_types([Type | Types], !MaybeUnqual, !ModuleNames) :-
    accumulate_modules_in_type(Type, !MaybeUnqual, !ModuleNames),
    accumulate_modules_in_types(Types, !MaybeUnqual, !ModuleNames).

:- pred accumulate_modules_in_type(mer_type::in,
    maybe_unqual_symnames::in, maybe_unqual_symnames::out,
    set(module_name)::in, set(module_name)::out) is det.

accumulate_modules_in_type(Type, !MaybeUnqual, !ModuleNames) :-
    (
        ( Type = type_variable(_, _)
        ; Type = builtin_type(_)
        )
    ;
        Type = defined_type(SymName, ArgTypes, _Kind),
        accumulate_module(SymName, !MaybeUnqual, !ModuleNames),
        accumulate_modules_in_types(ArgTypes, !MaybeUnqual, !ModuleNames)
    ;
        ( Type = tuple_type(ArgTypes, _Kind)
        ; Type = apply_n_type(_TVar, ArgTypes, _Kind)
        ; Type = higher_order_type(_PredOrFunc, ArgTypes,
            _HOInstInfo, _Purity, _EvalMethod)
        ),
        accumulate_modules_in_types(ArgTypes, !MaybeUnqual, !ModuleNames)
    ;
        Type = kinded_type(ArgType, _Kind),
        accumulate_modules_in_type(ArgType, !MaybeUnqual, !ModuleNames)
    ).

%---------------------%

:- pred accumulate_modules_in_qual_types(list(mer_type)::in,
    set(module_name)::in, set(module_name)::out) is det.

accumulate_modules_in_qual_types([], !Modules).
accumulate_modules_in_qual_types([Type | Types], !Modules) :-
    accumulate_modules_in_qual_type(Type, !Modules),
    accumulate_modules_in_qual_types(Types, !Modules).

:- pred accumulate_modules_in_qual_type(mer_type::in,
    set(module_name)::in, set(module_name)::out) is det.

accumulate_modules_in_qual_type(Type, !Modules) :-
    (
        % Do nothing for these types - they cannot affect the set of
        % implementation imports in an interface file.
        ( Type = type_variable(_, _)
        ; Type = builtin_type(_)
        )
    ;
        Type = defined_type(TypeName, ArgTypes, _),
        det_sym_name_get_module_name(TypeName, ModuleName),
        set.insert(ModuleName, !Modules),
        accumulate_modules_in_qual_types(ArgTypes, !Modules)
    ;
        Type = kinded_type(KindedType, _),
        accumulate_modules_in_qual_type(KindedType, !Modules)
    ;
        ( Type = tuple_type(ArgTypes, _)
        ; Type = apply_n_type(_, ArgTypes, _)
        ; Type = higher_order_type(_, ArgTypes, _HOInstInfo, _, _)
        ),
        % XXX ITEM_LIST accumulate modules from _HOInstInfo
        accumulate_modules_in_qual_types(ArgTypes, !Modules)
    ).

:- pred accumulate_modules_in_qual_type_ctor(type_ctor::in,
    set(module_name)::in, set(module_name)::out) is det.

accumulate_modules_in_qual_type_ctor(TypeCtor, !Modules) :-
    TypeCtor = type_ctor(SymName, _Arity),
    (
        SymName = qualified(ModuleName, _),
        set.insert(ModuleName, !Modules)
    ;
        SymName = unqualified(_)
        % Our ancestor generate_interfaces_int1_int2 should be invoked
        % only *after* the module qualification of the augmented compilation
        % unit whose contents we are now processing, and the module
        % qualification pass would have generated an error message
        % for this cannot-be-uniquely-qualified name. However, if the
        % user has turned off the halt_at_invalid_interface option,
        % which is on by default, then the compiler ignores that error,
        % and proceeds to call generate_interfaces_int1_int2 above,
        % which calls us indirectly.
    ).

%---------------------%

:- pred accumulate_modules_in_insts(list(mer_inst)::in,
    maybe_unqual_symnames::in, maybe_unqual_symnames::out,
    set(module_name)::in, set(module_name)::out) is det.

accumulate_modules_in_insts([], !MaybeUnqual, !ModuleNames).
accumulate_modules_in_insts([Inst | Insts], !MaybeUnqual, !ModuleNames) :-
    accumulate_modules_in_inst(Inst, !MaybeUnqual, !ModuleNames),
    accumulate_modules_in_insts(Insts, !MaybeUnqual, !ModuleNames).

:- pred accumulate_modules_in_inst(mer_inst::in,
    maybe_unqual_symnames::in, maybe_unqual_symnames::out,
    set(module_name)::in, set(module_name)::out) is det.

accumulate_modules_in_inst(Inst, !MaybeUnqual, !ModuleNames) :-
    (
        ( Inst = free
        ; Inst = not_reached
        ; Inst = ground(_Uniq, _HOInstInfo)
        ; Inst = inst_var(_InstVar)
        ; Inst = any(_Uniq, _HOInstInfo)
        )
    ;
        Inst = bound(_Uniq, _InstTestsResults, BoundInsts),
        accumulate_modules_in_bound_insts(BoundInsts,
            !MaybeUnqual, !ModuleNames)
    ;
        Inst = constrained_inst_vars(_InstVars, ArgInst),
        accumulate_modules_in_inst(ArgInst, !MaybeUnqual, !ModuleNames)
    ;
        Inst = defined_inst(InstName),
        accumulate_modules_in_inst_name(InstName, !MaybeUnqual, !ModuleNames)
    ).

:- pred accumulate_modules_in_inst_name(inst_name::in,
    maybe_unqual_symnames::in, maybe_unqual_symnames::out,
    set(module_name)::in, set(module_name)::out) is det.

accumulate_modules_in_inst_name(InstName, !MaybeUnqual, !ModuleNames) :-
    (
        InstName = user_inst(SymName, ArgInsts),
        accumulate_module(SymName, !MaybeUnqual, !ModuleNames),
        accumulate_modules_in_insts(ArgInsts, !MaybeUnqual, !ModuleNames)
    ;
        ( InstName = unify_inst(_IsLive, _IsReal, ArgInstA, ArgInstB)
        ; InstName = merge_inst(ArgInstA, ArgInstB)
        ),
        accumulate_modules_in_insts([ArgInstA, ArgInstB],
            !MaybeUnqual, !ModuleNames)
    ;
        ( InstName = ground_inst(ArgInstName, _Uniq, _IsLive, _IsReal)
        ; InstName = any_inst(ArgInstName, _Uniq, _IsLive, _IsReal)
        ; InstName = shared_inst(ArgInstName)
        ; InstName = mostly_uniq_inst(ArgInstName)
        ),
        accumulate_modules_in_inst_name(ArgInstName,
            !MaybeUnqual, !ModuleNames)
    ;
        InstName = typed_ground(_Uniq, Type),
        accumulate_modules_in_type(Type, !MaybeUnqual, !ModuleNames)
    ;
        InstName = typed_inst(Type, ArgInstName),
        accumulate_modules_in_type(Type, !MaybeUnqual, !ModuleNames),
        accumulate_modules_in_inst_name(ArgInstName,
            !MaybeUnqual, !ModuleNames)
    ).

:- pred accumulate_modules_in_bound_insts(list(bound_inst)::in,
    maybe_unqual_symnames::in, maybe_unqual_symnames::out,
    set(module_name)::in, set(module_name)::out) is det.

accumulate_modules_in_bound_insts([], !MaybeUnqual, !ModuleNames).
accumulate_modules_in_bound_insts([BoundInst | BoundInsts],
        !MaybeUnqual, !ModuleNames) :-
    accumulate_modules_in_bound_inst(BoundInst, !MaybeUnqual, !ModuleNames),
    accumulate_modules_in_bound_insts(BoundInsts, !MaybeUnqual, !ModuleNames).

:- pred accumulate_modules_in_bound_inst(bound_inst::in,
    maybe_unqual_symnames::in, maybe_unqual_symnames::out,
    set(module_name)::in, set(module_name)::out) is det.

accumulate_modules_in_bound_inst(BoundInst, !MaybeUnqual, !ModuleNames) :-
    BoundInst = bound_functor(ConsId, ArgInsts),
    ( if ConsId = cons(SymName, _ConsArity, TypeCtor) then
        accumulate_module(SymName, !MaybeUnqual, !ModuleNames),
        TypeCtor = type_ctor(TypeCtorSymName, _Arity),
        accumulate_module(TypeCtorSymName, !MaybeUnqual, !ModuleNames)
    else
        true
    ),
    accumulate_modules_in_insts(ArgInsts, !MaybeUnqual, !ModuleNames).

%---------------------%

:- pred accumulate_modules_in_mode(mer_mode::in,
    maybe_unqual_symnames::in, maybe_unqual_symnames::out,
    set(module_name)::in, set(module_name)::out) is det.

accumulate_modules_in_mode(Mode, !MaybeUnqual, !ModuleNames) :-
    (
        Mode = from_to_mode(InstA, InstB),
        accumulate_modules_in_inst(InstA, !MaybeUnqual, !ModuleNames),
        accumulate_modules_in_inst(InstB, !MaybeUnqual, !ModuleNames)
    ;
        Mode = user_defined_mode(SymName, ArgInsts),
        accumulate_module(SymName, !MaybeUnqual, !ModuleNames),
        accumulate_modules_in_insts(ArgInsts, !MaybeUnqual, !ModuleNames)
    ).

%---------------------%

:- type maybe_unqual_symnames
    --->    no_unqual_symnames
    ;       some_unqual_symnames.

:- pred accumulate_module(sym_name::in,
    maybe_unqual_symnames::in, maybe_unqual_symnames::out,
    set(module_name)::in, set(module_name)::out) is det.

accumulate_module(SymName, !MaybeUnqual, !ModuleNames) :-
    (
        SymName = unqualified(_),
        !:MaybeUnqual = some_unqual_symnames
    ;
        SymName = qualified(ModuleName, _),
        set.insert(ModuleName, !ModuleNames)
    ).

%---------------------------------------------------------------------------%
:- end_module parse_tree.comp_unit_interface.
%---------------------------------------------------------------------------%
