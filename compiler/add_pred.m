%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1993-2012,2014 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: add_pred.m.
%
% This submodule of make_hlds handles the type and mode declarations
% for predicates.
%
%-----------------------------------------------------------------------------%

:- module hlds.make_hlds.add_pred.
:- interface.

:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module mdbcomp.prim_data.
:- import_module hlds.make_hlds.make_hlds_passes.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module list.
:- import_module maybe.
:- import_module pair.

%-----------------------------------------------------------------------------%

:- pred module_add_pred_or_func(tvarset::in, inst_varset::in, existq_tvars::in,
    pred_or_func::in, sym_name::in, list(type_and_mode)::in,
    maybe(determinism)::in, purity::in,
    prog_constraints::in, pred_markers::in, prog_context::in,
    item_status::in, maybe(pair(pred_id, proc_id))::out,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred do_add_new_proc(inst_varset::in, arity::in, list(mer_mode)::in,
    maybe(list(mer_mode))::in, maybe(list(is_live))::in,
    detism_decl::in, maybe(determinism)::in, prog_context::in,
    is_address_taken::in, pred_info::in, pred_info::out, proc_id::out) is det.

    % Add a mode declaration for a predicate.
    %
:- pred module_add_mode(inst_varset::in, sym_name::in, list(mer_mode)::in,
    maybe(determinism)::in, import_status::in, prog_context::in,
    pred_or_func::in, bool::in, pair(pred_id, proc_id)::out,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

    % Whenever there is a clause or mode declaration for an undeclared
    % predicate, we add an implicit declaration
    %   :- pred p(T1, T2, ..., Tn).
    % for that predicate; the real types will be inferred by type inference.
    %
:- pred preds_add_implicit_report_error(module_info::in, module_info::out,
    module_name::in, sym_name::in, arity::in, pred_or_func::in, 
    import_status::in, bool::in, prog_context::in, pred_origin::in,
    list(format_component)::in, pred_id::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred preds_add_implicit_for_assertion(module_info::in, module_info::out,
    module_name::in, sym_name::in, arity::in, pred_or_func::in, prog_vars::in,
    import_status::in, prog_context::in, pred_id::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_args.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_rtti.
:- import_module hlds.make_hlds.make_hlds_error.
:- import_module hlds.pred_table.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.set_of_var.

:- import_module map.
:- import_module set.
:- import_module require.
:- import_module term.
:- import_module varset.

module_add_pred_or_func(TypeVarSet, InstVarSet, ExistQVars,
        PredOrFunc, PredName, TypesAndModes, MaybeDet, Purity,
        ClassContext, Markers, Context, item_status(Status, NeedQual),
        MaybePredProcId, !ModuleInfo, !Specs) :-
    split_types_and_modes(TypesAndModes, Types, MaybeModes0),
    add_new_pred(TypeVarSet, ExistQVars, PredName, Types, Purity, ClassContext,
        Markers, Context, Status, NeedQual, PredOrFunc, !ModuleInfo, !Specs),
    (
        PredOrFunc = pf_predicate,
        MaybeModes0 = yes(Modes0),

        % For predicates with no arguments, if the determinism is not declared
        % a mode is not added. The determinism can be specified by a separate
        % mode declaration.
        Modes0 = [],
        MaybeDet = no
    ->
        MaybeModes = no
    ;
        % Assume that a function with no modes but with a determinism
        % declared has the default modes.
        PredOrFunc = pf_function,
        MaybeModes0 = no,
        MaybeDet = yes(_)
    ->
        list.length(Types, Arity),
        adjust_func_arity(pf_function, FuncArity, Arity),
        in_mode(InMode),
        list.duplicate(FuncArity, InMode, InModes),
        out_mode(OutMode),
        list.append(InModes, [OutMode], ArgModes),
        MaybeModes = yes(ArgModes)
    ;
        MaybeModes = MaybeModes0
    ),
    (
        MaybeModes = yes(Modes),
        ( check_marker(Markers, marker_class_method) ->
            IsClassMethod = yes
        ;
            IsClassMethod = no
        ),
        module_add_mode(InstVarSet, PredName, Modes, MaybeDet, Status, Context,
            PredOrFunc, IsClassMethod, PredProcId, !ModuleInfo, !Specs),
        MaybePredProcId = yes(PredProcId)
    ;
        MaybeModes = no,
        MaybePredProcId = no
    ).

    % NB. Predicates are also added in lambda.m, which converts
    % lambda expressions into separate predicates, so any changes may need
    % to be reflected there too.
    %
:- pred add_new_pred(tvarset::in, existq_tvars::in, sym_name::in,
    list(mer_type)::in, purity::in, prog_constraints::in,
    pred_markers::in, prog_context::in, import_status::in,
    need_qualifier::in, pred_or_func::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_new_pred(TVarSet, ExistQVars, PredName, Types, Purity, ClassContext,
        Markers0, Context, ItemStatus, NeedQual, PredOrFunc, !ModuleInfo,
        !Specs) :-
    % Only preds with opt_imported clauses are tagged as opt_imported, so
    % that the compiler doesn't look for clauses for other preds read in
    % from optimization interfaces.
    ( ItemStatus = status_opt_imported ->
        Status = status_imported(import_locn_interface)
    ;
        Status = ItemStatus
    ),
    module_info_get_name(!.ModuleInfo, ModuleName),
    list.length(Types, Arity),
    (
        PredName = unqualified(_PName),
        module_info_incr_errors(!ModuleInfo),
        unqualified_pred_error(PredName, Arity, Context, !Specs)
        % All predicate names passed into this predicate should have
        % been qualified by prog_io.m, when they were first read.
    ;
        PredName = qualified(MNameOfPred, PName),
        module_info_get_predicate_table(!.ModuleInfo, PredTable0),
        clauses_info_init(PredOrFunc, Arity, init_clause_item_numbers_user,
            ClausesInfo),
        map.init(Proofs),
        map.init(ConstraintMap),
        purity_to_markers(Purity, PurityMarkers),
        markers_to_marker_list(PurityMarkers, MarkersList),
        list.foldl(add_marker, MarkersList, Markers0, Markers),
        map.init(VarNameRemap),
        pred_info_init(ModuleName, PredName, Arity, PredOrFunc, Context,
            origin_user(PredName), Status, goal_type_none, Markers, Types,
            TVarSet, ExistQVars, ClassContext, Proofs, ConstraintMap,
            ClausesInfo, VarNameRemap, PredInfo0),
        predicate_table_lookup_pf_m_n_a(PredTable0, is_fully_qualified,
            PredOrFunc, MNameOfPred, PName, Arity, PredIds),
        (
            PredIds = [OrigPred | _],
            module_info_pred_info(!.ModuleInfo, OrigPred, OrigPredInfo),
            pred_info_get_context(OrigPredInfo, OrigContext),
            DeclString = pred_or_func_to_str(PredOrFunc),
            adjust_func_arity(PredOrFunc, OrigArity, Arity),
            multiple_def_error(ItemStatus, PredName, OrigArity, DeclString,
                Context, OrigContext, [], !Specs)
        ;
            PredIds = [],
            module_info_get_partial_qualifier_info(!.ModuleInfo, PQInfo),
            predicate_table_insert_qual(PredInfo0, NeedQual, PQInfo, PredId,
                PredTable0, PredTable1),
            ( pred_info_is_builtin(PredInfo0) ->
                module_info_get_globals(!.ModuleInfo, Globals),
                globals.get_target(Globals, CompilationTarget),
                add_builtin(PredId, Types, CompilationTarget,
                    PredInfo0, PredInfo),
                predicate_table_get_preds(PredTable1, Preds1),
                map.det_update(PredId, PredInfo, Preds1, Preds),
                predicate_table_set_preds(Preds, PredTable1, PredTable)
            ;
                PredTable = PredTable1
            ),
            module_info_set_predicate_table(PredTable, !ModuleInfo)
        )
    ).

%-----------------------------------------------------------------------------%

    % For most builtin predicates, say foo/2, we add a clause
    %
    %   foo(H1, H2) :- foo(H1, H2).
    %
    % This does not generate an infinite loop! Instead, the compiler will
    % generate the usual builtin inline code for foo/2 in the body. The reason
    % for generating this forwarding code stub is so that things work correctly
    % if you take the address of the predicate.
    %
    % A few builtins are treated specially.
    %
:- pred add_builtin(pred_id::in, list(mer_type)::in, compilation_target::in,
    pred_info::in, pred_info::out) is det.

add_builtin(PredId, Types, CompilationTarget, !PredInfo) :-
    Module = pred_info_module(!.PredInfo),
    Name = pred_info_name(!.PredInfo),
    pred_info_get_context(!.PredInfo, Context),
    pred_info_get_clauses_info(!.PredInfo, ClausesInfo0),
    clauses_info_get_varset(ClausesInfo0, VarSet0),
    clauses_info_get_headvars(ClausesInfo0, HeadVars),
    % XXX ARGVEC - clean this up after the pred_info is converted to use
    % the arg_vector structure.
    clauses_info_get_headvar_list(ClausesInfo0, HeadVarList),

    goal_info_init(Context, GoalInfo0),
    NonLocals = set_of_var.list_to_set(proc_arg_vector_to_list(HeadVars)),
    goal_info_set_nonlocals(NonLocals, GoalInfo0, GoalInfo1),
    (
        Module = mercury_private_builtin_module,
        (
            ( Name = "builtin_compound_eq"
            ; Name = "builtin_compound_lt"
            )
        ;
            % These predicates are incompatible with Java and Erlang.
            ( Name = "store_at_ref_impure"
            ; Name = "store_at_ref"
            ),
            ( CompilationTarget = target_java
            ; CompilationTarget = target_csharp
            ; CompilationTarget = target_erlang
            )
        )
    ->
        GoalExpr = conj(plain_conj, []),
        GoalInfo = GoalInfo1,
        ExtraVars = [],
        ExtraTypes = [],
        VarSet = VarSet0,
        Stub = yes
    ;
        Module = mercury_private_builtin_module,
        Name = "trace_get_io_state"
    ->
        varset.new_var(ZeroVar, VarSet0, VarSet),
        ExtraVars = [ZeroVar],
        ExtraTypes = [int_type],

        Free = free,
        Ground = ground(shared, none),
        ConsId = int_const(0),
        LHS = ZeroVar,
        RHS = rhs_functor(ConsId, no, []),
        UniMode = ((Free - Ground) -> (Ground - Ground)),
        Unification = construct(ZeroVar, ConsId, [], [UniMode],
            construct_dynamically, cell_is_shared, no_construct_sub_info),
        UnifyMode = ((Free -> Ground) - (Ground -> Ground)),
        UnifyContext = unify_context(umc_explicit, []),
        AssignExpr = unify(LHS, RHS, UnifyMode, Unification, UnifyContext),
        goal_info_set_nonlocals(set_of_var.make_singleton(ZeroVar),
            GoalInfo0, GoalInfoWithZero),
        AssignGoal = hlds_goal(AssignExpr, GoalInfoWithZero),

        CastExpr = generic_call(cast(unsafe_type_inst_cast),
            [ZeroVar] ++ HeadVarList, [in_mode, uo_mode], arg_reg_types_unset,
            detism_det),
        goal_info_set_nonlocals(
            set_of_var.list_to_set([ZeroVar | HeadVarList]),
            GoalInfo0, GoalInfoWithZeroHeadVars),
        CastGoal = hlds_goal(CastExpr, GoalInfoWithZeroHeadVars),

        ConjExpr = conj(plain_conj, [AssignGoal, CastGoal]),
        ConjGoal = hlds_goal(ConjExpr, GoalInfoWithZeroHeadVars),

        Reason = promise_purity(purity_semipure),
        GoalExpr = scope(Reason, ConjGoal),
        GoalInfo = GoalInfo1,
        Stub = no
    ;
        Module = mercury_private_builtin_module,
        Name = "trace_set_io_state"
    ->
        ConjExpr = conj(plain_conj, []),
        ConjGoal = hlds_goal(ConjExpr, GoalInfo),
        Reason = promise_purity(purity_impure),
        GoalExpr = scope(Reason, ConjGoal),
        GoalInfo = GoalInfo1,
        ExtraVars = [],
        ExtraTypes = [],
        VarSet = VarSet0,
        Stub = no
    ;
        % Construct the pseudo-recursive call to Module.Name(HeadVars).
        SymName = qualified(Module, Name),
        % Mode checking will figure out the mode.
        ModeId = invalid_proc_id,
        MaybeUnifyContext = no,
        % XXX ARGVEC
        GoalExpr = plain_call(PredId, ModeId, HeadVarList, inline_builtin,
            MaybeUnifyContext, SymName),
        pred_info_get_purity(!.PredInfo, Purity),
        goal_info_set_purity(Purity, GoalInfo1, GoalInfo),
        ExtraVars = [],
        ExtraTypes = [],
        VarSet = VarSet0,
        Stub = no
    ),

    (
        Stub = no,
        % Construct a clause containing that pseudo-recursive call.
        Goal = hlds_goal(GoalExpr, GoalInfo),
        Clause = clause(all_modes, Goal, impl_lang_mercury, Context, []),
        set_clause_list([Clause], ClausesRep)
    ;
        Stub = yes,
        set_clause_list([], ClausesRep)
    ),

    % Put the clause we just built (if any) into the pred_info,
    % annotated with the appropriate types.
    vartypes_from_corresponding_lists(ExtraVars ++ HeadVarList,
        ExtraTypes ++ Types, VarTypes),
    map.init(TVarNameMap),
    rtti_varmaps_init(RttiVarMaps),
    HasForeignClauses = no,
    ClausesInfo = clauses_info(VarSet, VarTypes, TVarNameMap, VarTypes,
        HeadVars, ClausesRep, init_clause_item_numbers_comp_gen,
        RttiVarMaps, HasForeignClauses),
    pred_info_set_clauses_info(ClausesInfo, !PredInfo),

    % It's pointless but harmless to inline these clauses. The main purpose
    % of the `no_inline' marker is to stop constraint propagation creating
    % real infinite loops in the generated code when processing calls to these
    % predicates. The code generator will still generate inline code for calls
    % to these predicates.
    pred_info_get_markers(!.PredInfo, Markers0),
    add_marker(marker_user_marked_no_inline, Markers0, Markers1),
    (
        Stub = yes,
        add_marker(marker_stub, Markers1, Markers2),
        add_marker(marker_builtin_stub, Markers2, Markers)
    ;
        Stub = no,
        Markers = Markers1
    ),
    pred_info_set_markers(Markers, !PredInfo).

%-----------------------------------------------------------------------------%

do_add_new_proc(InstVarSet, Arity, ArgModes, MaybeDeclaredArgModes,
        MaybeArgLives, DetismDecl, MaybeDet, Context, IsAddressTaken,
        PredInfo0, PredInfo, ModeId) :-
    pred_info_get_procedures(PredInfo0, Procs0),
    pred_info_get_arg_types(PredInfo0, ArgTypes),
    pred_info_get_var_name_remap(PredInfo0, VarNameRemap),
    next_mode_id(Procs0, ModeId),
    proc_info_init(Context, Arity, ArgTypes, MaybeDeclaredArgModes, ArgModes,
        MaybeArgLives, DetismDecl, MaybeDet, IsAddressTaken, VarNameRemap,
        NewProc0),
    proc_info_set_inst_varset(InstVarSet, NewProc0, NewProc),
    map.det_insert(ModeId, NewProc, Procs0, Procs),
    pred_info_set_procedures(Procs, PredInfo0, PredInfo).

%-----------------------------------------------------------------------------%

module_add_mode(InstVarSet, PredName, Modes, MaybeDet, Status, MContext,
        PredOrFunc, IsClassMethod, PredProcId, !ModuleInfo, !Specs) :-
    % We should store the mode varset and the mode condition in the HLDS
    % - at the moment we just ignore those two arguments.

    % Lookup the pred or func declaration in the predicate table.
    % If it is not there (or if it is ambiguous), optionally print a warning
    % message and insert an implicit definition for the predicate;
    % it is presumed to be local, and its type will be inferred automatically.

    module_info_get_name(!.ModuleInfo, ModuleName0),
    sym_name_get_module_name_default(PredName, ModuleName0, ModuleName),
    list.length(Modes, Arity),
    module_info_get_predicate_table(!.ModuleInfo, PredicateTable0),
    predicate_table_lookup_pf_sym_arity(PredicateTable0,
        is_fully_qualified, PredOrFunc, PredName, Arity, PredIds),
    ( PredIds = [PredIdPrime] ->
        PredId = PredIdPrime
    ;
        preds_add_implicit_report_error(!ModuleInfo, ModuleName,
            PredName, Arity, PredOrFunc, Status, IsClassMethod, MContext,
            origin_user(PredName), [words("mode declaration")], PredId, !Specs)
    ),
    module_info_get_predicate_table(!.ModuleInfo, PredicateTable1),
    predicate_table_get_preds(PredicateTable1, Preds0),
    map.lookup(Preds0, PredId, PredInfo0),

    module_do_add_mode(InstVarSet, Arity, Modes, MaybeDet, IsClassMethod,
        MContext, PredInfo0, PredInfo, ProcId, !Specs),
    map.det_update(PredId, PredInfo, Preds0, Preds),
    predicate_table_set_preds(Preds, PredicateTable1, PredicateTable),
    module_info_set_predicate_table(PredicateTable, !ModuleInfo),
    PredProcId = PredId - ProcId.

:- pred module_do_add_mode(inst_varset::in, arity::in, list(mer_mode)::in,
    maybe(determinism)::in, bool::in, prog_context::in,
    pred_info::in, pred_info::out, proc_id::out,
    list(error_spec)::in, list(error_spec)::out) is det.

module_do_add_mode(InstVarSet, Arity, Modes, MaybeDet, IsClassMethod, MContext,
        !PredInfo, ProcId, !Specs) :-
    % Check that the determinism was specified.
    (
        MaybeDet = no,
        DetismDecl = detism_decl_none,
        pred_info_get_import_status(!.PredInfo, ImportStatus),
        PredOrFunc = pred_info_is_pred_or_func(!.PredInfo),
        PredModule = pred_info_module(!.PredInfo),
        PredName = pred_info_name(!.PredInfo),
        PredSymName = qualified(PredModule, PredName),
        (
            IsClassMethod = yes,
            unspecified_det_for_method(PredSymName, Arity, PredOrFunc,
                MContext, !Specs)
        ;
            IsClassMethod = no,
            IsExported = status_is_exported(ImportStatus),
            (
                IsExported = yes,
                unspecified_det_for_exported(PredSymName, Arity, PredOrFunc,
                    MContext, !Specs)
            ;
                IsExported = no,
                unspecified_det_for_local(PredSymName, Arity, PredOrFunc,
                    MContext, !Specs)
            )
        )
    ;
        MaybeDet = yes(_),
        DetismDecl = detism_decl_explicit
    ),
    % Add the mode declaration to the pred_info for this procedure.
    ArgLives = no,
    do_add_new_proc(InstVarSet, Arity, Modes, yes(Modes), ArgLives,
        DetismDecl, MaybeDet, MContext, address_is_not_taken,
        !PredInfo, ProcId).

preds_add_implicit_report_error(!ModuleInfo, ModuleName, PredName, Arity,
        PredOrFunc, Status, IsClassMethod, Context, Origin, DescPieces,
        PredId, !Specs) :-
    module_info_get_predicate_table(!.ModuleInfo, PredicateTable0),
    module_info_get_globals(!.ModuleInfo, Globals),
    maybe_undefined_pred_error(Globals, PredName, Arity, PredOrFunc,
        Status, IsClassMethod, Context, DescPieces, !Specs),
    (
        PredOrFunc = pf_function,
        adjust_func_arity(pf_function, FuncArity, Arity),
        maybe_check_field_access_function(!.ModuleInfo, PredName, FuncArity,
            Status, Context, !Specs)
    ;
        PredOrFunc = pf_predicate
    ),
    clauses_info_init(PredOrFunc, Arity, init_clause_item_numbers_user,
        ClausesInfo),
    preds_do_add_implicit(!.ModuleInfo, ModuleName, PredName, Arity,
        PredOrFunc, Status, Context, Origin, ClausesInfo, PredId,
        PredicateTable0, PredicateTable),
    module_info_set_predicate_table(PredicateTable, !ModuleInfo).

preds_add_implicit_for_assertion(!ModuleInfo, ModuleName, PredName,
        Arity, PredOrFunc, HeadVars, Status, Context, PredId) :-
    clauses_info_init_for_assertion(HeadVars, ClausesInfo),
    term.context_file(Context, FileName),
    term.context_line(Context, LineNum),
    module_info_get_predicate_table(!.ModuleInfo, PredicateTable0),
    preds_do_add_implicit(!.ModuleInfo, ModuleName, PredName, Arity,
        PredOrFunc,Status, Context, origin_assertion(FileName, LineNum), 
        ClausesInfo, PredId, PredicateTable0, PredicateTable),
    module_info_set_predicate_table(PredicateTable, !ModuleInfo).

:- pred preds_do_add_implicit(module_info::in, module_name::in,
    sym_name::in, arity::in, pred_or_func::in,
    import_status::in, prog_context::in, pred_origin::in, clauses_info::in,
    pred_id::out, predicate_table::in, predicate_table::out) is det.

preds_do_add_implicit(ModuleInfo, ModuleName, PredName, Arity, PredOrFunc,
        Status, Context, Origin, ClausesInfo, PredId, !PredicateTable) :-
    varset.init(TVarSet0),
    make_n_fresh_vars("T", Arity, TypeVars, TVarSet0, TVarSet),
    prog_type.var_list_to_type_list(map.init, TypeVars, Types),
    map.init(Proofs),
    map.init(ConstraintMap),
    % The class context is empty since this is an implicit definition.
    % Inference will fill it in.
    ClassContext = constraints([], []),
    % We assume none of the arguments are existentially typed.
    % Existential types must be declared, they won't be inferred.
    ExistQVars = [],
    init_markers(Markers0),
    map.init(VarNameRemap),
    pred_info_init(ModuleName, PredName, Arity, PredOrFunc, Context,
        Origin, Status, goal_type_none, Markers0, Types, TVarSet, ExistQVars,
        ClassContext, Proofs, ConstraintMap, ClausesInfo, VarNameRemap,
        PredInfo0),
    add_marker(marker_infer_type, Markers0, Markers),
    pred_info_set_markers(Markers, PredInfo0, PredInfo),
    predicate_table_lookup_pf_sym_arity(!.PredicateTable,
        is_fully_qualified, PredOrFunc, PredName, Arity, PredIds),
    (
        PredIds = [],
        module_info_get_partial_qualifier_info(ModuleInfo, MQInfo),
        predicate_table_insert_qual(PredInfo, may_be_unqualified, MQInfo,
            PredId, !PredicateTable)
    ;
        PredIds = [_ | _],
        unexpected($module, $pred, "search succeeded")
    ).

%-----------------------------------------------------------------------------%

:- pred unspecified_det_for_local(sym_name::in, arity::in, pred_or_func::in,
    prog_context::in, list(error_spec)::in, list(error_spec)::out) is det.

unspecified_det_for_local(Name, Arity, PredOrFunc, Context, !Specs) :-
    MainPieces = [words("Error: no determinism declaration for local"),
        simple_call(simple_call_id(PredOrFunc, Name, Arity)), suffix(".")],
    VerbosePieces = [words("(This is an error because"),
        words("you specified the `--no-infer-det' options."),
        words("Use the `--infer-det' option if you want the compiler"),
        words("to automatically infer the determinism"),
        words("of local predicates.)")],
    InnerComponents = [always(MainPieces), verbose_only(VerbosePieces)],
    Msg = simple_msg(Context,
        [option_is_set(infer_det, no, InnerComponents)]),
    Severity = severity_conditional(infer_det, no, severity_error, no),
    Spec = error_spec(Severity, phase_parse_tree_to_hlds, [Msg]),
    !:Specs = [Spec | !.Specs].

:- pred unspecified_det_for_method(sym_name::in, arity::in, pred_or_func::in,
    prog_context::in, list(error_spec)::in, list(error_spec)::out) is det.

unspecified_det_for_method(Name, Arity, PredOrFunc, Context, !Specs) :-
    Pieces = [words("Error: no determinism declaration"),
        words("for type class method"), p_or_f(PredOrFunc),
        sym_name_and_arity(Name / Arity), suffix(".")],
    Msg = simple_msg(Context, [always(Pieces)]),
    Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
    !:Specs = [Spec | !.Specs].

:- pred unspecified_det_for_exported(sym_name::in, arity::in, pred_or_func::in,
    prog_context::in, list(error_spec)::in, list(error_spec)::out) is det.

unspecified_det_for_exported(Name, Arity, PredOrFunc, Context, !Specs) :-
    Pieces = [words("Error: no determinism declaration for exported"),
        p_or_f(PredOrFunc), sym_name_and_arity(Name / Arity), suffix(".")],
    Msg = simple_msg(Context, [always(Pieces)]),
    Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
    !:Specs = [Spec | !.Specs].

:- pred unqualified_pred_error(sym_name::in, int::in, prog_context::in,
    list(error_spec)::in, list(error_spec)::out) is det.

unqualified_pred_error(PredName, Arity, Context, !Specs) :-
    Pieces = [words("Internal error: the unqualified predicate name"),
        sym_name_and_arity(PredName / Arity),
        words("should have been qualified by prog_io.m.")],
    Msg = simple_msg(Context, [always(Pieces)]),
    Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
    !:Specs = [Spec | !.Specs].

%-----------------------------------------------------------------------------%
:- end_module hlds.make_hlds.add_pred.
%-----------------------------------------------------------------------------%
