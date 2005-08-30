%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1993-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% This submodule of make_hlds handles the type and mode declarations
% for predicates.

:- module hlds__make_hlds__add_pred.
:- interface.

:- import_module hlds__hlds_module.
:- import_module hlds__hlds_pred.
:- import_module mdbcomp__prim_data.
:- import_module hlds__make_hlds__make_hlds_passes.
:- import_module parse_tree__prog_data.

:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module std_util.

:- pred module_add_pred_or_func(tvarset::in, inst_varset::in, existq_tvars::in,
    pred_or_func::in, sym_name::in, list(type_and_mode)::in,
    maybe(determinism)::in, purity::in,
    prog_constraints::in, pred_markers::in, prog_context::in,
    item_status::in, maybe(pair(pred_id, proc_id))::out,
    module_info::in, module_info::out, io::di, io::uo) is det.

:- pred do_add_new_proc(inst_varset::in, arity::in, list(mode)::in,
    maybe(list(mode))::in, maybe(list(is_live))::in,
    maybe(determinism)::in, prog_context::in, is_address_taken::in,
    pred_info::in, pred_info::out, proc_id::out) is det.

    % Add a mode declaration for a predicate.
    %
:- pred module_add_mode(inst_varset::in, sym_name::in, list(mode)::in,
    maybe(determinism)::in, import_status::in, prog_context::in,
    pred_or_func::in, bool::in, pair(pred_id, proc_id)::out,
    module_info::in, module_info::out, io::di, io::uo) is det.

    % Whenever there is a clause or mode declaration for an undeclared
    % predicate, we add an implicit declaration
    %   :- pred p(T1, T2, ..., Tn).
    % for that predicate; the real types will be inferred by
    % type inference.
    %
:- pred preds_add_implicit_report_error(module_name::in, pred_or_func::in,
    sym_name::in, arity::in, import_status::in, bool::in, prog_context::in,
    pred_origin::in, string::in, pred_id::out,
    module_info::in, module_info::out, io::di, io::uo) is det.

:- pred preds_add_implicit_for_assertion(prog_vars::in, module_info::in,
    module_name::in, sym_name::in, arity::in, import_status::in,
    prog_context::in, pred_or_func::in, pred_id::out,
    predicate_table::in, predicate_table::out) is det.

:- implementation.

:- import_module hlds__hlds_data.
:- import_module hlds__hlds_goal.
:- import_module hlds__hlds_out.
:- import_module hlds__hlds_pred.
:- import_module hlds__make_hlds__make_hlds_error.
:- import_module libs__globals.
:- import_module libs__options.
:- import_module parse_tree__error_util.
:- import_module parse_tree__prog_mode.
:- import_module parse_tree__prog_out.
:- import_module parse_tree__prog_util.

:- import_module map.
:- import_module require.
:- import_module set.
:- import_module term.
:- import_module varset.

module_add_pred_or_func(TypeVarSet, InstVarSet, ExistQVars,
        PredOrFunc, PredName, TypesAndModes, MaybeDet, Purity,
        ClassContext, Markers, Context, item_status(Status, NeedQual),
        MaybePredProcId, !ModuleInfo, !IO) :-
    split_types_and_modes(TypesAndModes, Types, MaybeModes0),
    add_new_pred(TypeVarSet, ExistQVars, PredName, Types, Purity, ClassContext,
        Markers, Context, Status, NeedQual, PredOrFunc, !ModuleInfo, !IO),
    (
        PredOrFunc = predicate,
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
        PredOrFunc = function,
        MaybeModes0 = no,
        MaybeDet = yes(_)
    ->
        list__length(Types, Arity),
        adjust_func_arity(function, FuncArity, Arity),
        in_mode(InMode),
        list__duplicate(FuncArity, InMode, InModes),
        out_mode(OutMode),
        list__append(InModes, [OutMode], ArgModes),
        MaybeModes = yes(ArgModes)
    ;
        MaybeModes = MaybeModes0
    ),
    (
        MaybeModes = yes(Modes),
        ( check_marker(Markers, class_method) ->
            IsClassMethod = yes
        ;
            IsClassMethod = no
        ),
        module_add_mode(InstVarSet, PredName, Modes, MaybeDet, Status, Context,
            PredOrFunc, IsClassMethod, PredProcId, !ModuleInfo, !IO),
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
    list(type)::in, purity::in, prog_constraints::in,
    pred_markers::in, prog_context::in, import_status::in,
    need_qualifier::in, pred_or_func::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

add_new_pred(TVarSet, ExistQVars, PredName, Types, Purity, ClassContext,
        Markers0, Context, ItemStatus, NeedQual, PredOrFunc, !ModuleInfo,
        !IO) :-
    % Only preds with opt_imported clauses are tagged as opt_imported, so
    % that the compiler doesn't look for clauses for other preds read in
    % from optimization interfaces.
    ( ItemStatus = opt_imported ->
        Status = imported(interface)
    ;
        Status = ItemStatus
    ),
    module_info_name(!.ModuleInfo, ModuleName),
    list__length(Types, Arity),
    (
        PredName = unqualified(_PName),
        module_info_incr_errors(!ModuleInfo),
        unqualified_pred_error(PredName, Arity, Context, !IO)
        % All predicate names passed into this predicate should have
        % been qualified by prog_io.m, when they were first read.
    ;
        PredName = qualified(MNameOfPred, PName),
        module_info_get_predicate_table(!.ModuleInfo, PredTable0),
        clauses_info_init(Arity, ClausesInfo),
        map__init(Proofs),
        map__init(ConstraintMap),
        purity_to_markers(Purity, PurityMarkers),
        markers_to_marker_list(PurityMarkers, MarkersList),
        list__foldl(add_marker, MarkersList, Markers0, Markers),
        globals__io_lookup_string_option(aditi_user, Owner, !IO),
        pred_info_init(ModuleName, PredName, Arity, PredOrFunc, Context,
            user(PredName), Status, none, Markers, Types, TVarSet, ExistQVars,
            ClassContext, Proofs, ConstraintMap, Owner, ClausesInfo,
            PredInfo0),
        (
            predicate_table_search_pf_m_n_a(PredTable0,
                is_fully_qualified, PredOrFunc, MNameOfPred,
                PName, Arity, [OrigPred|_])
        ->
            module_info_pred_info(!.ModuleInfo, OrigPred, OrigPredInfo),
            pred_info_context(OrigPredInfo, OrigContext),
            DeclString = pred_or_func_to_str(PredOrFunc),
            adjust_func_arity(PredOrFunc, OrigArity, Arity),
            multiple_def_error(ItemStatus, PredName, OrigArity, DeclString,
                Context, OrigContext, FoundError, !IO),
            (
                FoundError = yes,
                module_info_incr_errors(!ModuleInfo)
            ;
                FoundError = no
            )
        ;
            module_info_get_partial_qualifier_info(!.ModuleInfo, PQInfo),
            predicate_table_insert(PredInfo0, NeedQual, PQInfo, PredId,
                PredTable0, PredTable1),
            ( pred_info_is_builtin(PredInfo0) ->
                add_builtin(PredId, Types, PredInfo0, PredInfo),
                predicate_table_get_preds(PredTable1, Preds1),
                map__det_update(Preds1, PredId, PredInfo, Preds),
                predicate_table_set_preds(Preds, PredTable1, PredTable)
            ;
                PredTable = PredTable1
            ),
            module_info_set_predicate_table(PredTable, !ModuleInfo)
        )
    ).

%-----------------------------------------------------------------------------%

:- pred add_builtin(pred_id::in, list(type)::in, pred_info::in, pred_info::out)
    is det.

    % For a builtin predicate, say foo/2, we add a clause
    %
    %   foo(H1, H2) :- foo(H1, H2).
    %
    % This does not generate an infinite loop!
    % Instead, the compiler will generate the usual builtin inline code
    % for foo/2 in the body.  The reason for generating this
    % forwarding code stub is so that things work correctly if
    % you take the address of the predicate.
    %
add_builtin(PredId, Types, !PredInfo) :-
        %
        % lookup some useful info: Module, Name, Context, HeadVars
        %
    Module = pred_info_module(!.PredInfo),
    Name = pred_info_name(!.PredInfo),
    pred_info_context(!.PredInfo, Context),
    pred_info_clauses_info(!.PredInfo, ClausesInfo0),
    clauses_info_varset(ClausesInfo0, VarSet),
    clauses_info_headvars(ClausesInfo0, HeadVars),

        %
        % construct the pseudo-recursive call to Module:Name(HeadVars)
        %
    SymName = qualified(Module, Name),
    ModeId = invalid_proc_id,   % mode checking will figure it out
    MaybeUnifyContext = no,
    Call = call(PredId, ModeId, HeadVars, inline_builtin, MaybeUnifyContext,
        SymName),

        %
        % construct a clause containing that pseudo-recursive call
        %
    goal_info_init(Context, GoalInfo0),
    set__list_to_set(HeadVars, NonLocals),
    goal_info_set_nonlocals(NonLocals, GoalInfo0, GoalInfo),
    Goal = Call - GoalInfo,
    Clause = clause([], Goal, mercury, Context),

        %
        % put the clause we just built into the pred_info,
        % annotateed with the appropriate types
        %
    map__from_corresponding_lists(HeadVars, Types, VarTypes),
    map__init(TVarNameMap),
    rtti_varmaps_init(RttiVarMaps),
    HasForeignClauses = no,
    set_clause_list([Clause], ClausesRep),
    ClausesInfo = clauses_info(VarSet, VarTypes, TVarNameMap, VarTypes,
        HeadVars, ClausesRep, RttiVarMaps, HasForeignClauses),
    pred_info_set_clauses_info(ClausesInfo, !PredInfo),

        %
        % It's pointless but harmless to inline these clauses.
        % The main purpose of the `no_inline' marker is to stop
        % constraint propagation creating real infinite loops in
        % the generated code when processing calls to these
        % predicates. The code generator will still generate
        % inline code for calls to these predicates.
        %
    pred_info_get_markers(!.PredInfo, Markers0),
    add_marker(no_inline, Markers0, Markers),
    pred_info_set_markers(Markers, !PredInfo).

%-----------------------------------------------------------------------------%

do_add_new_proc(InstVarSet, Arity, ArgModes, MaybeDeclaredArgModes,
        MaybeArgLives, MaybeDet, Context, IsAddressTaken, PredInfo0, PredInfo,
        ModeId) :-
    pred_info_procedures(PredInfo0, Procs0),
    pred_info_arg_types(PredInfo0, ArgTypes),
    next_mode_id(Procs0, ModeId),
    proc_info_init(Context, Arity, ArgTypes, MaybeDeclaredArgModes,
        ArgModes, MaybeArgLives, MaybeDet, IsAddressTaken, NewProc0),
    proc_info_set_inst_varset(InstVarSet, NewProc0, NewProc),
    map__det_insert(Procs0, ModeId, NewProc, Procs),
    pred_info_set_procedures(Procs, PredInfo0, PredInfo).

%-----------------------------------------------------------------------------%

    % We should store the mode varset and the mode condition in the HLDS
    % - at the moment we just ignore those two arguments.
    %
module_add_mode(InstVarSet, PredName, Modes, MaybeDet, Status, MContext,
        PredOrFunc, IsClassMethod, PredProcId, !ModuleInfo, !IO) :-
    % Lookup the pred or func declaration in the predicate table.
    % If it's not there (or if it is ambiguous), optionally print a
    % warning message and insert an implicit definition for the
    % predicate; it is presumed to be local, and its type
    % will be inferred automatically.

    module_info_name(!.ModuleInfo, ModuleName0),
    sym_name_get_module_name(PredName, ModuleName0, ModuleName),
    list__length(Modes, Arity),
    module_info_get_predicate_table(!.ModuleInfo, PredicateTable0),
    (
        predicate_table_search_pf_sym_arity(PredicateTable0,
            is_fully_qualified, PredOrFunc, PredName, Arity, [PredId0])
    ->
        PredId = PredId0
    ;
        preds_add_implicit_report_error(ModuleName, PredOrFunc, PredName,
            Arity, Status, IsClassMethod, MContext, user(PredName),
            "mode declaration", PredId, !ModuleInfo, !IO)
    ),
    module_info_get_predicate_table(!.ModuleInfo, PredicateTable1),
    predicate_table_get_preds(PredicateTable1, Preds0),
    map__lookup(Preds0, PredId, PredInfo0),

    module_do_add_mode(InstVarSet, Arity, Modes, MaybeDet, IsClassMethod,
        MContext, PredInfo0, PredInfo, ProcId, !IO),
    map__det_update(Preds0, PredId, PredInfo, Preds),
    predicate_table_set_preds(Preds, PredicateTable1, PredicateTable),
    module_info_set_predicate_table(PredicateTable, !ModuleInfo),
    PredProcId = PredId - ProcId.

:- pred module_do_add_mode(inst_varset::in, arity::in, list(mode)::in,
    maybe(determinism)::in, bool::in, prog_context::in,
    pred_info::in, pred_info::out, proc_id::out, io::di, io::uo) is det.

module_do_add_mode(InstVarSet, Arity, Modes, MaybeDet, IsClassMethod, MContext,
        !PredInfo, ProcId, !IO) :-
    % check that the determinism was specified
    (
        MaybeDet = no,
        pred_info_import_status(!.PredInfo, ImportStatus),
        PredOrFunc = pred_info_is_pred_or_func(!.PredInfo),
        PredModule = pred_info_module(!.PredInfo),
        PredName = pred_info_name(!.PredInfo),
        PredSymName = qualified(PredModule, PredName),
        ( IsClassMethod = yes ->
            unspecified_det_for_method(PredSymName, Arity, PredOrFunc,
                MContext, !IO)
        ; status_is_exported(ImportStatus, yes) ->
            unspecified_det_for_exported(PredSymName, Arity, PredOrFunc,
                MContext, !IO)
        ;
            globals__io_lookup_bool_option(infer_det, InferDet, !IO),
            (
                InferDet = no,
                unspecified_det_for_local(PredSymName, Arity, PredOrFunc,
                    MContext, !IO)
            ;
                InferDet = yes
            )
        )
    ;
        MaybeDet = yes(_)
    ),
    % Add the mode declaration to the pred_info for this procedure.
    ArgLives = no,
    do_add_new_proc(InstVarSet, Arity, Modes, yes(Modes), ArgLives,
        MaybeDet, MContext, address_is_not_taken, !PredInfo, ProcId).

preds_add_implicit_report_error(ModuleName, PredOrFunc, PredName, Arity,
        Status, IsClassMethod, Context, Origin, Description, PredId,
        !ModuleInfo, !IO) :-
    maybe_undefined_pred_error(PredName, Arity, PredOrFunc, Status,
        IsClassMethod, Context, Description, !IO),
    (
        PredOrFunc = function,
        adjust_func_arity(function, FuncArity, Arity),
        maybe_check_field_access_function(PredName, FuncArity, Status, Context,
            !.ModuleInfo, !IO)
    ;
        PredOrFunc = predicate
    ),
    module_info_get_predicate_table(!.ModuleInfo, PredicateTable0),
    preds_add_implicit(!.ModuleInfo, ModuleName, PredName, Arity, Status,
        Context, Origin, PredOrFunc, PredId, PredicateTable0, PredicateTable),
    module_info_set_predicate_table(PredicateTable, !ModuleInfo).

:- pred preds_add_implicit(module_info::in, module_name::in, sym_name::in,
    arity::in, import_status::in, prog_context::in, pred_origin::in,
    pred_or_func::in, pred_id::out,
    predicate_table::in, predicate_table::out) is det.

preds_add_implicit(ModuleInfo, ModuleName, PredName, Arity, Status, Context,
        Origin, PredOrFunc, PredId, !PredicateTable) :-
    clauses_info_init(Arity, ClausesInfo),
    preds_add_implicit_2(ClausesInfo, ModuleInfo, ModuleName, PredName,
        Arity, Status, Context, Origin, PredOrFunc, PredId, !PredicateTable).

preds_add_implicit_for_assertion(HeadVars, ModuleInfo, ModuleName, PredName,
        Arity, Status, Context, PredOrFunc, PredId, !PredicateTable) :-
    clauses_info_init_for_assertion(HeadVars, ClausesInfo),
    term__context_file(Context, FileName),
    term__context_line(Context, LineNum),
    preds_add_implicit_2(ClausesInfo, ModuleInfo, ModuleName, PredName,
        Arity, Status, Context, assertion(FileName, LineNum),
        PredOrFunc, PredId, !PredicateTable).

:- pred preds_add_implicit_2(clauses_info::in, module_info::in,
    module_name::in, sym_name::in, arity::in, import_status::in,
    prog_context::in, pred_origin::in, pred_or_func::in, pred_id::out,
    predicate_table::in, predicate_table::out) is det.

preds_add_implicit_2(ClausesInfo, ModuleInfo, ModuleName, PredName, Arity,
        Status, Context, Origin, PredOrFunc, PredId, !PredicateTable) :-
    varset__init(TVarSet0),
    make_n_fresh_vars("T", Arity, TypeVars, TVarSet0, TVarSet),
    term__var_list_to_term_list(TypeVars, Types),
    map__init(Proofs),
    map__init(ConstraintMap),
        % The class context is empty since this is an implicit
        % definition. Inference will fill it in.
    ClassContext = constraints([], []),
        % We assume none of the arguments are existentially typed.
        % Existential types must be declared, they won't be inferred.
    ExistQVars = [],
    init_markers(Markers0),
    module_info_globals(ModuleInfo, Globals),
    globals__lookup_string_option(Globals, aditi_user, Owner),
    pred_info_init(ModuleName, PredName, Arity, PredOrFunc, Context,
        Origin, Status, none, Markers0, Types, TVarSet, ExistQVars,
        ClassContext, Proofs, ConstraintMap, Owner, ClausesInfo, PredInfo0),
    add_marker(infer_type, Markers0, Markers),
    pred_info_set_markers(Markers, PredInfo0, PredInfo),
    (
        \+ predicate_table_search_pf_sym_arity(!.PredicateTable,
            is_fully_qualified, PredOrFunc, PredName, Arity, _)
    ->
        module_info_get_partial_qualifier_info(ModuleInfo, MQInfo),
        predicate_table_insert(PredInfo, may_be_unqualified, MQInfo, PredId,
            !PredicateTable)
    ;
        error("preds_add_implicit")
    ).

%-----------------------------------------------------------------------------%

:- pred unspecified_det_for_local(sym_name::in, arity::in, pred_or_func::in,
    prog_context::in, io::di, io::uo) is det.

unspecified_det_for_local(Name, Arity, PredOrFunc, Context, !IO) :-
    Pieces = [words("Error: no determinism declaration for local"),
        words(simple_call_id_to_string(PredOrFunc, Name, Arity)),
        suffix(".")],
    write_error_pieces(Context, 0, Pieces, !IO),
    record_warning(!IO),
    globals__io_lookup_bool_option(verbose_errors, VerboseErrors, !IO),
    (
        VerboseErrors = yes,
        VerbosePieces = [words("(This is an error because"),
            words("you specified the `--no-infer-det' options."),
            words("Use the `--infer-det' option if you want the compiler"),
            words("to automatically infer the determinism"),
            words("of local predicates.)")],
        write_error_pieces(Context, 0, VerbosePieces, !IO)
    ;
        VerboseErrors = no
    ).

:- pred unspecified_det_for_method(sym_name::in, arity::in, pred_or_func::in,
    prog_context::in, io::di, io::uo) is det.

unspecified_det_for_method(Name, Arity, PredOrFunc, Context, !IO) :-
    Pieces = [words("Error: no determinism declaration"),
        words("for type class method"),
        pred_or_func(PredOrFunc),
        sym_name_and_arity(Name / Arity),
        suffix(".")],
    write_error_pieces(Context, 0, Pieces, !IO),
    io__set_exit_status(1, !IO).

:- pred unspecified_det_for_exported(sym_name::in, arity::in, pred_or_func::in,
    prog_context::in, io::di, io::uo) is det.

unspecified_det_for_exported(Name, Arity, PredOrFunc, Context, !IO) :-
    Pieces = [words("Error: no determinism declaration for exported"),
        pred_or_func(PredOrFunc),
        sym_name_and_arity(Name / Arity),
        suffix(".")],
    write_error_pieces(Context, 0, Pieces, !IO),
    io__set_exit_status(1, !IO).

:- pred unqualified_pred_error(sym_name::in, int::in, prog_context::in,
    io::di, io::uo) is det.

unqualified_pred_error(PredName, Arity, Context, !IO) :-
    Pieces = [words("Internal error: the unqualified predicate name"),
        sym_name_and_arity(PredName / Arity),
        words("should have been qualified by prog_io.m.")],
    write_error_pieces(Context, 0, Pieces, !IO),
    io__set_exit_status(1, !IO).

%-----------------------------------------------------------------------------%
