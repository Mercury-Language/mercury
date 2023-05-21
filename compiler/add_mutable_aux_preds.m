%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2015-2021 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This module creates both the public predicates users use to access mutables,
% and the private auxiliary predicates needed to make that possible.
%
%-----------------------------------------------------------------------------%
%
% Mutables are implemented as a source-to-source transformation on the
% parse tree. The transformation, whose details depend on the compilation
% target, is documented in notes/mutables.
%
%-----------------------------------------------------------------------------%

:- module hlds.make_hlds.add_mutable_aux_preds.
:- interface.

:- import_module hlds.hlds_module.
:- import_module hlds.make_hlds.make_hlds_types.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.prog_foreign.
:- import_module parse_tree.prog_item.

:- import_module cord.
:- import_module list.

%---------------------------------------------------------------------------%

    % Check the mutables that are local to this module for errors,
    % and implement them by transforming them into other kinds of items
    % and pred_target_names to be added to the HLDS by our caller.
    %
:- pred implement_mutables_if_local(module_info::in,
    sec_list(item_mutable_info)::in,
    sec_list(item_pred_decl_info)::out,
    ims_list(item_clause_info)::out, ims_list(item_foreign_proc)::out,
    list(foreign_decl_code)::out, list(foreign_body_code)::out,
    cord(item_fproc_export)::out,
    pred_target_names::in, pred_target_names::out,
    list(error_spec)::in, list(error_spec)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.foreign.
:- import_module hlds.error_msg_inst.
:- import_module hlds.hlds_inst_mode.
:- import_module hlds.status.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_mutable.

:- import_module bool.
:- import_module map.
:- import_module require.
:- import_module varset.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

implement_mutables_if_local(ModuleInfo, SecList, PredDecls,
        ClauseInfos, ForeignProcs, ForeignDeclCodes, ForeignBodyCodes,
        FPEInfoCord, !PredTargetNames, !Specs) :-
    implement_mutables_sec_loop(ModuleInfo, SecList,
        cord.init, PredDeclCord,
        cord.init, ClauseInfoCord, cord.init, ForeignProcCord,
        cord.init, ForeignDeclCodeCord, cord.init, ForeignBodyCodeCord,
        cord.init, FPEInfoCord, !PredTargetNames, !Specs),
    PredDecls = cord.list(PredDeclCord),
    ClauseInfos = cord.list(ClauseInfoCord),
    ForeignProcs = cord.list(ForeignProcCord),
    ForeignDeclCodes = cord.list(ForeignDeclCodeCord),
    ForeignBodyCodes = cord.list(ForeignBodyCodeCord).

:- pred implement_mutables_sec_loop(module_info::in,
    sec_list(item_mutable_info)::in,
    sec_cord(item_pred_decl_info)::in, sec_cord(item_pred_decl_info)::out,
    ims_cord(item_clause_info)::in, ims_cord(item_clause_info)::out,
    ims_cord(item_foreign_proc)::in, ims_cord(item_foreign_proc)::out,
    cord(foreign_decl_code)::in, cord(foreign_decl_code)::out,
    cord(foreign_body_code)::in, cord(foreign_body_code)::out,
    cord(item_fproc_export)::in, cord(item_fproc_export)::out,
    pred_target_names::in, pred_target_names::out,
    list(error_spec)::in, list(error_spec)::out) is det.

implement_mutables_sec_loop(_ModuleInfo, [],
        !PredDeclCord, !ClauseInfoCord, !ForeignProcCord,
        !ForeignDeclCodeCord, !ForeignBodyCodeCord,
        !FPEInfoCord, !PredTargetNames, !Specs).
implement_mutables_sec_loop(ModuleInfo, [SecSubList | SecSubLists],
        !PredDeclCord, !ClauseInfoCord, !ForeignProcCord,
        !ForeignDeclCodeCord, !ForeignBodyCodeCord,
        !FPEInfoCord, !PredTargetNames, !Specs) :-
    SecSubList = sec_sub_list(SectionInfo, ItemMutables),
    SectionInfo = sec_info(ItemMercuryStatus, _NeedQual),
    (
        ItemMercuryStatus = item_defined_in_this_module(_),
        module_info_get_globals(ModuleInfo, Globals),
        module_info_get_name(ModuleInfo, ModuleName),
        TypeNameFunc = exported_type_to_string(ModuleInfo),
        ModuleParams = module_params(Globals, ModuleName, TypeNameFunc),
        implement_mutables_loop(ModuleInfo, SectionInfo, ModuleParams,
            ItemMutables,
            cord.init, PredDeclSubCord,
            cord.init, ClauseInfoSubCord, cord.init, ForeignProcSubCord,
            !ForeignDeclCodeCord, !ForeignBodyCodeCord,
            !FPEInfoCord, !PredTargetNames, !Specs),
        PredDeclSubList =
            sec_sub_list(SectionInfo, cord.list(PredDeclSubCord)),
        cord.snoc(PredDeclSubList, !PredDeclCord),
        ( if cord.is_empty(ClauseInfoSubCord) then
            true
        else
            ClauseInfoSubList =
                ims_sub_list(ItemMercuryStatus, cord.list(ClauseInfoSubCord)),
            cord.snoc(ClauseInfoSubList, !ClauseInfoCord)
        ),
        ( if cord.is_empty(ForeignProcSubCord) then
            true
        else
            ForeignProcSubList =
                ims_sub_list(ItemMercuryStatus, cord.list(ForeignProcSubCord)),
            cord.snoc(ForeignProcSubList, !ForeignProcCord)
        )
    ;
        ItemMercuryStatus = item_defined_in_other_module(_)
        % We don't implement the `mutable' declaration unless it is defined
        % in this module. If we did not have this check, we would duplicate
        % the definition of the global variable storing the mutable
        % in any submodules of the module that actually defined the mutable.
    ),
    implement_mutables_sec_loop(ModuleInfo, SecSubLists,
        !PredDeclCord, !ClauseInfoCord, !ForeignProcCord,
        !ForeignDeclCodeCord, !ForeignBodyCodeCord,
        !FPEInfoCord, !PredTargetNames, !Specs).

:- pred implement_mutables_loop(module_info::in, sec_info::in,
    module_params::in, list(item_mutable_info)::in,
    cord(item_pred_decl_info)::in, cord(item_pred_decl_info)::out,
    cord(item_clause_info)::in, cord(item_clause_info)::out,
    cord(item_foreign_proc)::in, cord(item_foreign_proc)::out,
    cord(foreign_decl_code)::in, cord(foreign_decl_code)::out,
    cord(foreign_body_code)::in, cord(foreign_body_code)::out,
    cord(item_fproc_export)::in, cord(item_fproc_export)::out,
    pred_target_names::in, pred_target_names::out,
    list(error_spec)::in, list(error_spec)::out) is det.

implement_mutables_loop(_ModuleInfo, _SectionInfo, _ModuleParams,
        [], !PredDeclCord, !ClauseInfoCord, !ForeignProcCord,
        !ForeignDeclCodeCord, !ForeignBodyCodeCord,
        !FPEInfoCord, !PredTargetNames, !Specs).
implement_mutables_loop(ModuleInfo, SectionInfo, ModuleParams,
        [ItemMutable | ItemMutables], !PredDeclCord,
        !ClauseInfoCord, !ForeignProcCord,
        !ForeignDeclCodeCord, !ForeignBodyCodeCord,
        !FPEInfoCord, !PredTargetNames, !Specs) :-
    check_mutable(ModuleInfo, ItemMutable, !Specs),
    implement_mutable(ModuleParams, ItemMutable,
        PredDecls, ClauseInfos, ForeignProcs, FPEInfo,
        !ForeignDeclCodeCord, !ForeignBodyCodeCord, !PredTargetNames),
    !:PredDeclCord = !.PredDeclCord ++ cord.from_list(PredDecls),
    !:ClauseInfoCord = !.ClauseInfoCord ++ cord.from_list(ClauseInfos),
    !:ForeignProcCord = !.ForeignProcCord ++ cord.from_list(ForeignProcs),
    cord.snoc(FPEInfo, !FPEInfoCord),
    implement_mutables_loop(ModuleInfo, SectionInfo, ModuleParams,
        ItemMutables, !PredDeclCord, !ClauseInfoCord, !ForeignProcCord,
        !ForeignDeclCodeCord, !ForeignBodyCodeCord,
        !FPEInfoCord, !PredTargetNames, !Specs).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- pred check_mutable(module_info::in, item_mutable_info::in,
    list(error_spec)::in, list(error_spec)::out) is det.

check_mutable(ModuleInfo, ItemMutable, !Specs) :-
    ItemMutable = item_mutable_info(_MutableName,
        _OrigType, _Type, OrigInst, Inst,
        _InitTerm, _VarSetMutable, MutAttrs, Context, _SeqNum),
    module_info_get_globals(ModuleInfo, Globals),
    globals.get_target(Globals, CompilationTarget),
    % NOTE We currently support the foreign_name attribute for all targets,
    % but we did not do so when we supported Erlang.
    (
        ( CompilationTarget = target_c
        ; CompilationTarget = target_java
        ; CompilationTarget = target_csharp
        )
    ),

    % If the mutable is to be trailed, then we need to be in a trailing grade.
    MutAttrs = mutable_var_attributes(_LangMap, Const),
    globals.lookup_bool_option(Globals, use_trail, UseTrail),
    ( if
        Const = mutable_is_not_constant(_, Local),
        mutable_thread_local_trailed(Local) = mutable_trailed,
        UseTrail = no
    then
        TrailPieces = [words("Error: trailed"), decl("mutable"),
            words("declaration in non-trailing grade."), nl],
        TrailSpec = simplest_spec($pred, severity_error,
            phase_parse_tree_to_hlds, Context, TrailPieces),
        !:Specs = [TrailSpec | !.Specs]
    else
        true
    ),

    % Check that the inst in the mutable declaration is a valid inst
    % for a mutable declaration.
    % It is okay to pass a dummy varset in here since any attempt
    % to use inst variables in a mutable declaration should already
    % been dealt with when the mutable declaration was parsed.
    DummyInstVarSet = varset.init,
    check_mutable_inst(ModuleInfo, Context, DummyInstVarSet, [], Inst,
        [], ExpandedInstSpecs),
    (
        ExpandedInstSpecs = []
    ;
        ExpandedInstSpecs = [_ | _],
        % We found some insts in Inst that are not allowed in mutables.
        %
        % Inst has been processed by equiv_type.m, which replaces named insts
        % with the definition of the named inst. When we check it, the error
        % messages we generate for any errors in it will lack information
        % about what nested sequence of named inst definitions the errors is
        % inside. We therefore compute the error messages on the original
        % inst as well.
        %
        % If ExpandedInstSpecs is nonempty, then UnexpandedInstSpecs should
        % be nonempty as well, but we prepare for it to be empty just in case.
        check_mutable_inst(ModuleInfo, Context, DummyInstVarSet, [], OrigInst,
            [], UnexpandedInstSpecs),
        (
            UnexpandedInstSpecs = [],
            % Printing error messages without the proper context is better than
            % not printing error messages at all, once we have discovered
            % an error.
            !:Specs = ExpandedInstSpecs ++ !.Specs
        ;
            UnexpandedInstSpecs = [_ | _],
            !:Specs = UnexpandedInstSpecs ++ !.Specs
        )
    ).

%---------------------------------------------------------------------------%

    % Add an error to !Specs for each part of the inst that isn't allowed
    % inside a mutable declaration.
    %
:- pred check_mutable_inst(module_info::in, prog_context::in,
    inst_varset::in, list(inst_ctor)::in, mer_inst::in,
    list(error_spec)::in, list(error_spec)::out) is det.

check_mutable_inst(ModuleInfo, Context, InstVarSet, ParentInsts, Inst,
        !Specs) :-
    (
        ( Inst = any(Uniq, _)
        ; Inst = ground(Uniq, _)
        ),
        check_mutable_inst_uniqueness(ModuleInfo, Context, InstVarSet,
            ParentInsts, Inst, Uniq, !Specs)
    ;
        Inst = bound(Uniq, _, BoundInsts),
        check_mutable_inst_uniqueness(ModuleInfo, Context, InstVarSet,
            ParentInsts, Inst, Uniq, !Specs),
        check_mutable_bound_insts(ModuleInfo, Context, InstVarSet,
            ParentInsts, BoundInsts, !Specs)
    ;
        Inst = defined_inst(InstName),
        (
            InstName = user_inst(UserInstSymName, UserInstArgs),
            list.length(UserInstArgs, UserInstArity),
            UserInstCtor = inst_ctor(UserInstSymName, UserInstArity),
            ( if
                list.member(UserInstCtor, ParentInsts)
            then
                true
            else if
                UserInstSymName =
                    qualified(UserInstModuleName, UserInstBaseName),
                UserInstModuleName = mercury_public_builtin_module,
                UserInstArity = 0,
                ( UserInstBaseName = "dead"
                ; UserInstBaseName = "mostly_dead"
                )
            then
                FreePieces = [words("may not appear in"),
                    decl("mutable"), words("declarations.")],
                UnqualInstName =
                    user_inst(unqualified(UserInstBaseName), UserInstArgs),
                UnqualInst = defined_inst(UnqualInstName),
                invalid_inst_in_mutable(ModuleInfo, Context, InstVarSet,
                    ParentInsts, UnqualInst, FreePieces, !Specs)
            else
                check_mutable_insts(ModuleInfo, Context, InstVarSet,
                    ParentInsts, UserInstArgs, !Specs),

                module_info_get_inst_table(ModuleInfo, InstTable),
                inst_table_get_user_insts(InstTable, UserInstTable),
                ( if map.search(UserInstTable, UserInstCtor, InstDefn) then
                    InstDefn = hlds_inst_defn(DefnInstVarSet, _Params,
                        InstBody, _MMTC, _Context, _Status),
                    InstBody = eqv_inst(EqvInst),
                    DefnParentInsts = [UserInstCtor | ParentInsts],
                    check_mutable_inst(ModuleInfo, Context, DefnInstVarSet,
                        DefnParentInsts, EqvInst, !Specs)
                else
                    UndefinedPieces = [words("is not defined.")],
                    invalid_inst_in_mutable(ModuleInfo, Context, InstVarSet,
                        ParentInsts, Inst, UndefinedPieces, !Specs)
                )
            )
        ;
            ( InstName = unify_inst(_, _, _, _)
            ; InstName = merge_inst(_, _)
            ; InstName = ground_inst(_, _, _, _)
            ; InstName = any_inst(_, _, _, _)
            ; InstName = shared_inst(_)
            ; InstName = mostly_uniq_inst(_)
            ; InstName = typed_inst(_, _)
            ; InstName = typed_ground(_, _)
            ),
            unexpected($pred, "non-user inst")
        )
    ;
        ( Inst = free
        ; Inst = free(_)
        ),
        FreePieces = [words("may not appear in"),
            decl("mutable"), words("declarations.")],
        invalid_inst_in_mutable(ModuleInfo, Context, InstVarSet, ParentInsts,
            Inst, FreePieces, !Specs)
    ;
        Inst = constrained_inst_vars(_, _),
        ConstrainedPieces = [words("is constrained, and thus"),
            words("may not appear in"), decl("mutable"),
            words("declarations.")],
        invalid_inst_in_mutable(ModuleInfo, Context, InstVarSet, ParentInsts,
            Inst, ConstrainedPieces, !Specs)
    ;
        Inst = abstract_inst(_, _),
        AbstractPieces = [words("is abstract, and thus"),
            words("may not appear in"), decl("mutable"),
            words("declarations.")],
        invalid_inst_in_mutable(ModuleInfo, Context, InstVarSet, ParentInsts,
            Inst, AbstractPieces, !Specs)
    ;
        Inst = inst_var(_)
        % The parser ensures that the inst in the mutable declaration does
        % not have any variables. Any variables we encounter here must be
        % a parameter from a named inst that the top level inst refers to
        % either directly or indirectly.
    ;
        Inst = not_reached,
        unexpected($pred, "not_reached")
    ).

:- pred check_mutable_bound_insts(module_info::in, prog_context::in,
    inst_varset::in, list(inst_ctor)::in, list(bound_inst)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

check_mutable_bound_insts(_ModuleInfo, _Context, _InstVarSet, _ParentInsts,
        [], !Specs).
check_mutable_bound_insts(ModuleInfo, Context, InstVarSet, ParentInsts,
        [BoundInst | BoundInsts], !Specs) :-
    BoundInst = bound_functor(_ConsId, ArgInsts),
    check_mutable_insts(ModuleInfo, Context, InstVarSet, ParentInsts,
        ArgInsts, !Specs),
    check_mutable_bound_insts(ModuleInfo, Context, InstVarSet, ParentInsts,
        BoundInsts, !Specs).

:- pred check_mutable_insts(module_info::in, prog_context::in,
    inst_varset::in, list(inst_ctor)::in, list(mer_inst)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

check_mutable_insts(_ModuleInfo, _Context, _InstVarSet, _ParentInsts,
        [], !Specs).
check_mutable_insts(ModuleInfo, Context, InstVarSet, ParentInsts,
        [Inst | Insts], !Specs) :-
    check_mutable_inst(ModuleInfo, Context, InstVarSet, ParentInsts,
        Inst, !Specs),
    check_mutable_insts(ModuleInfo, Context, InstVarSet, ParentInsts,
        Insts, !Specs).

%---------------------%

:- pred check_mutable_inst_uniqueness(module_info::in, prog_context::in,
    inst_varset::in, list(inst_ctor)::in, mer_inst::in, uniqueness::in,
    list(error_spec)::in, list(error_spec)::out) is det.

check_mutable_inst_uniqueness(ModuleInfo, Context, InstVarSet, ParentInsts,
        Inst, Uniq, !Specs) :-
    (
        Uniq = shared
    ;
        (
            Uniq = unique,
            UniqStr = "unique"
        ;
            Uniq = mostly_unique,
            UniqStr = "mostly_unique"
        ;
            Uniq = clobbered,
            UniqStr = "clobbered"
        ;
            Uniq = mostly_clobbered,
            UniqStr = "mostly_clobbered"
        ),
        ( if Inst = ground(Uniq, _) then
            UniqPieces = [words("may not appear in"),
                decl("mutable"), words("declarations.")]
        else
            UniqPieces = [words("has uniqueness"), quote(UniqStr), suffix(","),
                words("which may not appear in"),
                decl("mutable"), words("declarations.")]
        ),
        invalid_inst_in_mutable(ModuleInfo, Context, InstVarSet, ParentInsts,
            Inst, UniqPieces, !Specs)
    ).

:- pred invalid_inst_in_mutable(module_info::in, prog_context::in,
    inst_varset::in, list(inst_ctor)::in, mer_inst::in,
    list(format_piece)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

invalid_inst_in_mutable(ModuleInfo, Context, InstVarSet, ParentInsts, Inst,
        ProblemPieces, !Specs) :-
    named_parents_to_pieces(ParentInsts, ParentPieces),
    InstPieces = error_msg_inst(ModuleInfo, InstVarSet,
        dont_expand_named_insts, quote_short_inst,
        [], [nl_indent_delta(1)], [nl_indent_delta(-1)], Inst),
    Pieces = [words("Error:") | ParentPieces] ++
        [words("the inst") | InstPieces] ++ ProblemPieces ++ [nl],
    Spec = simplest_spec($pred, severity_error, phase_parse_tree_to_hlds,
        Context, Pieces),
    !:Specs = [Spec | !.Specs].

:- pred named_parents_to_pieces(list(inst_ctor)::in,
    list(format_piece)::out) is det.

named_parents_to_pieces([], []).
named_parents_to_pieces([InstCtor | InstCtors], Pieces) :-
    named_parent_to_pieces(InstCtor, HeadPieces),
    named_parents_to_pieces(InstCtors, TailPieces),
    Pieces = HeadPieces ++ TailPieces.

:- pred named_parent_to_pieces(inst_ctor::in,
    list(format_piece)::out) is det.

named_parent_to_pieces(InstCtor, Pieces) :-
    InstCtor = inst_ctor(InstName, InstArity),
    Pieces = [words("in the expansion of the named inst"),
        qual_sym_name_arity(sym_name_arity(InstName, InstArity)),
        suffix(":"), nl].

%---------------------------------------------------------------------------%
:- end_module hlds.make_hlds.add_mutable_aux_preds.
%---------------------------------------------------------------------------%
