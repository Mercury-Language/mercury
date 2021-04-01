%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1993-2012 The University of Melbourne.
% Copyright (C) 2014-2021 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: typecheck.m.
% Main author: fjh.
%
% This file contains the Mercury type-checker.
%
% The predicates in this module are named as follows:
%
% - Predicates that type check a particular language construct
%   (goal, clause, etc.) are called typecheck_*. These will eventually
%   have to iterate over every type assignment in the type assignment set.
%
% - Predicates that unify two things with respect to a single type assignment,
%   as opposed to a type assignment set are called type_assign_*.
%
% There are four sorts of types:
%
% 1 discriminated unions:
%   :- type tree(T) ---> nil ; t(tree(T), T, tree(T)).
%
% 2 equivalence types (treated identically, ie, same name. Any number of types
%   can be equivalent; the *canonical* one is the one which is not defined
%   using ==):
%   :- type real == float.
%
%   Currently references to equivalence types are expanded in a separate pass
%   by mercury_compile_front_end.m. It would be better to avoid expanding them
%   (and instead modify the type unification algorithm to handle equivalent
%   types) because this would give better error messages. However, this is
%   not a high priority.
%
% 3 higher-order predicate and function types
%   pred, pred(T), pred(T1, T2), pred(T1, T2, T3), ...
%   func(T1) = T2, func(T1, T2) = T3, ...
%
% 4 builtin types
%   character, int, float, string; These types have special syntax
%   for constants. There may be other types (list(T), unit, univ, etc.)
%   provided by the system, but they can just be part of the standard library.
%
% Each exported predicate must have a `:- pred' declaration specifying the
% types of the arguments for that predicate. For predicates that are
% local to a module, we infer the types.
%
%---------------------------------------------------------------------------%
%
% Known Bugs:
%
% XXX Type inference doesn't handle ambiguity as well as it could do.
% We should do a topological sort, and then typecheck it all bottom-up.
% If we infer an ambiguous type for a pred, we should not reject it
% immediately; instead we should give it an overloaded type, and keep going.
% When we have finished type inference, we should then delete unused
% overloadings, and only then should we report ambiguity errors,
% if any overloading still remains.
%
% Wish list:
%
% - We should handle equivalence types here.
%
%---------------------------------------------------------------------------%

:- module check_hlds.typecheck.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module parse_tree.
:- import_module parse_tree.error_util.

:- import_module bool.
:- import_module list.

    % typecheck_module(!ModuleInfo, Specs, FoundSyntaxError,
    %   ExceededIterationLimit):
    %
    % Type checks ModuleInfo and annotates it with variable type information.
    % Specs is set to the list of errors and warnings found, plus messages
    % about the predicates and functions whose types have been inferred.
    % We set FoundSyntaxError to yes if some of the clauses in the typechecked
    % predicates contained syntax errors.
    % We set ExceededIterationLimit to `yes' iff the type inference iteration
    % limit was reached.
    %
:- pred typecheck_module(module_info::in, module_info::out,
    list(error_spec)::out, bool::out, bool::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.type_assign.
:- import_module check_hlds.type_util.
:- import_module check_hlds.typecheck_errors.
:- import_module check_hlds.typecheck_info.
:- import_module check_hlds.typeclasses.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_class.
:- import_module hlds.hlds_clauses.
:- import_module hlds.hlds_cons.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_error_util.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_rtti.
:- import_module hlds.instmap.
:- import_module hlds.make_goal.
:- import_module hlds.passes_aux.
:- import_module hlds.pred_table.
:- import_module hlds.special_pred.
:- import_module hlds.status.
:- import_module hlds.vartypes.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.goal_path.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.file_names.         % undesirable dependency
:- import_module parse_tree.parse_tree_out_pred_decl.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_event.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_event.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_subst.
:- import_module parse_tree.prog_util.

:- import_module assoc_list.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module one_or_more.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module set_tree234.
:- import_module string.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%

typecheck_module(!ModuleInfo, Specs, FoundSyntaxError,
        ExceededIterationLimit) :-
    module_info_get_globals(!.ModuleInfo, Globals),
    globals.lookup_int_option(Globals, type_inference_iteration_limit,
        MaxIterations),

    module_info_get_valid_pred_id_set(!.ModuleInfo, OrigValidPredIdSet),
    OrigValidPredIds = set_tree234.to_sorted_list(OrigValidPredIdSet),

    typecheck_to_fixpoint(1, MaxIterations, !ModuleInfo,
        OrigValidPredIds, OrigValidPredIdSet, FinalValidPredIdSet,
        CheckSpecs, FoundSyntaxError, ExceededIterationLimit),

    construct_type_inference_messages(!.ModuleInfo, FinalValidPredIdSet,
        OrigValidPredIds, [], InferSpecs),
    Specs = InferSpecs ++ CheckSpecs.

    % Repeatedly typecheck the code for a group of predicates
    % until a fixpoint is reached, or until some errors are detected.
    %
:- pred typecheck_to_fixpoint(int::in, int::in,
    module_info::in, module_info::out,
    list(pred_id)::in, set_tree234(pred_id)::in, set_tree234(pred_id)::out,
    list(error_spec)::out, bool::out, bool::out) is det.

typecheck_to_fixpoint(Iteration, MaxIterations, !ModuleInfo,
        OrigValidPredIds, OrigValidPredIdSet, FinalValidPredIdSet,
        Specs, FoundSyntaxError, ExceededIterationLimit) :-
    module_info_get_preds(!.ModuleInfo, PredMap0),
    map.to_assoc_list(PredMap0, PredIdsInfos0),
    typecheck_module_one_iteration(!.ModuleInfo, OrigValidPredIdSet,
        PredIdsInfos0, PredIdsInfos, [], NewlyInvalidPredIds,
        [], CurSpecs, no, CurFoundSyntaxError,
        next_iteration_is_not_needed, NextIteration),
    map.from_sorted_assoc_list(PredIdsInfos, PredMap),
    module_info_set_preds(PredMap, !ModuleInfo),

    module_info_make_pred_ids_invalid(NewlyInvalidPredIds, !ModuleInfo),
    module_info_get_valid_pred_id_set(!.ModuleInfo, NewValidPredIdSet),

    module_info_get_globals(!.ModuleInfo, Globals),
    ( if
        ( NextIteration = next_iteration_is_not_needed
        ; contains_errors(Globals, CurSpecs) = yes
        )
    then
        FinalValidPredIdSet = NewValidPredIdSet,
        Specs = CurSpecs,
        FoundSyntaxError = CurFoundSyntaxError,
        ExceededIterationLimit = no
    else
        globals.lookup_bool_option(Globals, debug_types, DebugTypes),
        (
            DebugTypes = yes,
            construct_type_inference_messages(!.ModuleInfo, NewValidPredIdSet,
                OrigValidPredIds, [], ProgressSpecs),
            trace [io(!IO)] (
                module_info_get_name(!.ModuleInfo, ModuleName),
                get_debug_output_stream(Globals, ModuleName, OutputStream,
                    !IO),
                write_error_specs_ignore(OutputStream, Globals,
                    ProgressSpecs, !IO)
            )
        ;
            DebugTypes = no
        ),
        ( if Iteration < MaxIterations then
            typecheck_to_fixpoint(Iteration + 1, MaxIterations, !ModuleInfo,
                OrigValidPredIds, OrigValidPredIdSet, FinalValidPredIdSet,
                Specs, FoundSyntaxError, ExceededIterationLimit)
        else
            FinalValidPredIdSet = NewValidPredIdSet,
            Specs = [typecheck_report_max_iterations_exceeded(MaxIterations)],
            FoundSyntaxError = CurFoundSyntaxError,
            ExceededIterationLimit = yes
        )
    ).

    % Write out the inferred `pred' or `func' declarations for a list of
    % predicates. Don't write out the inferred types for assertions.
    %
:- pred construct_type_inference_messages(module_info::in,
    set_tree234(pred_id)::in, list(pred_id)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

construct_type_inference_messages(_, _, [], !Specs).
construct_type_inference_messages(ModuleInfo, ValidPredIdSet,
        [PredId | PredIds], !Specs) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_get_markers(PredInfo, Markers),
    ( if
        check_marker(Markers, marker_infer_type),
        set_tree234.contains(ValidPredIdSet, PredId),
        not pred_info_is_promise(PredInfo, _)
    then
        Spec = construct_type_inference_message(ModuleInfo, PredId, PredInfo),
        !:Specs = [Spec | !.Specs]
    else
        true
    ),
    construct_type_inference_messages(ModuleInfo, ValidPredIdSet,
        PredIds, !Specs).

    % Construct a message containing the inferred `pred' or `func' declaration
    % for a single predicate.
    %
:- func construct_type_inference_message(module_info, pred_id, pred_info)
    = error_spec.

construct_type_inference_message(ModuleInfo, PredId, PredInfo) = Spec :-
    PredName = pred_info_name(PredInfo),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    UnqualPredSymName = unqualified(PredName),
    pred_info_get_context(PredInfo, Context),
    pred_info_get_arg_types(PredInfo, VarSet, ExistQVars, Types0),
    strip_builtin_qualifiers_from_type_list(Types0, Types),
    pred_info_get_class_context(PredInfo, ClassContext),
    pred_info_get_purity(PredInfo, Purity),
    MaybeDet = no,
    VarNamePrint = print_name_only,
    (
        PredOrFunc = pf_predicate,
        ArgTypes = Types,
        MaybeReturnType = no,
        TypeStr = mercury_pred_type_to_string(VarSet, VarNamePrint, ExistQVars,
            UnqualPredSymName, Types, MaybeDet, Purity, ClassContext)
    ;
        PredOrFunc = pf_function,
        pred_args_to_func_args(Types, ArgTypes, ReturnType),
        MaybeReturnType = yes(ReturnType),
        TypeStr = mercury_func_type_to_string(VarSet, VarNamePrint, ExistQVars,
            UnqualPredSymName, ArgTypes, ReturnType, MaybeDet, Purity,
            ClassContext)
    ),
    InferredPieces = [invis_order_default_start(2),
        words("Inferred"), words(TypeStr), nl],

    module_info_get_predicate_table(ModuleInfo, PredicateTable),
    ModuleName = pred_info_module(PredInfo),
    QualPredSymName = qualified(ModuleName, PredName),
    predicate_table_lookup_pf_sym(PredicateTable, is_fully_qualified,
        PredOrFunc, QualPredSymName, AllPredIds),
    list.delete_all(AllPredIds, PredId, AllOtherPredIds),
    PredIsDeclared =
        ( pred(OtherPredId::in) is semidet :-
            module_info_pred_info(ModuleInfo, OtherPredId, OtherPredInfo),
            pred_info_get_markers(OtherPredInfo, OtherPredMarkers),
            not check_marker(OtherPredMarkers, marker_infer_type)
        ),
    list.filter(PredIsDeclared, AllOtherPredIds, AllOtherDeclaredPredIds),
    (
        AllOtherDeclaredPredIds = [],
        Spec = conditional_spec($pred, inform_inferred_types, yes,
            severity_informational, phase_type_check,
            [simplest_msg(Context, InferredPieces)])
    ;
        AllOtherDeclaredPredIds = [_ | _],
        list.map(
            construct_pred_decl_diff(ModuleInfo, ArgTypes, MaybeReturnType),
            AllOtherDeclaredPredIds, DiffPieceLists),
        Pieces = [invis_order_default_start(2)] ++ InferredPieces ++
            list.condense(DiffPieceLists),
        Spec = simplest_spec($pred, severity_informational, phase_type_check,
            Context, Pieces)
    ).

%---------------------------------------------------------------------------%

:- func typecheck_report_max_iterations_exceeded(int) = error_spec.

typecheck_report_max_iterations_exceeded(MaxIterations) = Spec :-
    Pieces = [words("Type inference iteration limit exceeded."),
        words("This probably indicates that your program has a type error."),
        words("You should declare the types explicitly."),
        words("(The current limit is"), int_fixed(MaxIterations),
        words("iterations."),
        words("You can use the"), quote("--type-inference-iteration-limit"),
        words("option to increase the limit).")],
    Msg = error_msg(no, do_not_treat_as_first, 0, [always(Pieces)]),
    Spec = error_spec($pred, severity_error, phase_type_check, [Msg]).

%---------------------------------------------------------------------------%

:- type next_iteration
    --->    next_iteration_is_not_needed
    ;       next_iteration_is_needed.

    % Iterate over the list of pred_ids in a module.
    %
    % NOTE: Please update Mercury.options if this predicate is moved to another
    % module. It must be compiled with --optimize-constructor-last-call.
    %
:- pred typecheck_module_one_iteration(module_info::in,
    set_tree234(pred_id)::in,
    assoc_list(pred_id, pred_info)::in, assoc_list(pred_id, pred_info)::out,
    list(pred_id)::in, list(pred_id)::out,
    list(error_spec)::in, list(error_spec)::out,
    bool::in, bool::out, next_iteration::in, next_iteration::out) is det.

typecheck_module_one_iteration(_, _, [], [],
        !NewlyInvalidPredIds, !Specs, !FoundSyntaxError, !NextIteration).
typecheck_module_one_iteration(ModuleInfo, ValidPredIdSet,
        [HeadPredIdInfo0 | TailPredIdsInfos0], PredIdInfos,
        !NewlyInvalidPredIds, !Specs, !FoundSyntaxError, !NextIteration) :-
    HeadPredIdInfo0 = PredId - PredInfo0,
    ( if
        (
            pred_info_is_imported(PredInfo0)
        ;
            not set_tree234.contains(ValidPredIdSet, PredId)
        )
    then
        HeadPredIdInfo = HeadPredIdInfo0,
        typecheck_module_one_iteration(ModuleInfo, ValidPredIdSet,
            TailPredIdsInfos0, TailPredIdsInfos, !NewlyInvalidPredIds,
            !Specs, !FoundSyntaxError, !NextIteration),
        PredIdInfos = [HeadPredIdInfo | TailPredIdsInfos] % lcmc
    else
        % Potential parallelization site.
        typecheck_pred_if_needed(ModuleInfo, PredId, PredInfo0, PredInfo,
            PredSpecs, PredSyntaxError, ContainsErrors, PredNextIteration),
        (
            ContainsErrors = no
        ;
            ContainsErrors = yes,
            % This code is not needed at the moment, since currently we don't
            % run mode analysis if there are any type errors. And this code
            % also causes problems: if there are undefined modes, it can end up
            % calling error/1, since post_finish_ill_typed_pred assumes that
            % there are no undefined modes.
            %
            % If we get an error, we need to call post_finish_ill_typed_pred
            % on the pred, to ensure that its mode declaration gets properly
            % module qualified; then we call `remove_predid', so that the
            % predicate's definition will be ignored by later passes
            % (the declaration will still be used to check any calls to it).
            %
            % post_finish_ill_typed_pred(ModuleInfo0, PredId,
            %   PredInfo1, PredInfo)
            !:NewlyInvalidPredIds = [PredId | !.NewlyInvalidPredIds]
        ),
        HeadPredIdInfo = PredId - PredInfo,
        !:Specs = PredSpecs ++ !.Specs,
        bool.or(PredSyntaxError, !FoundSyntaxError),
        (
            PredNextIteration = next_iteration_is_not_needed
        ;
            PredNextIteration = next_iteration_is_needed,
            !:NextIteration = next_iteration_is_needed
        ),
        typecheck_module_one_iteration(ModuleInfo, ValidPredIdSet,
            TailPredIdsInfos0, TailPredIdsInfos, !NewlyInvalidPredIds,
            !Specs, !FoundSyntaxError, !NextIteration),
        PredIdInfos = [HeadPredIdInfo | TailPredIdsInfos] % lcmc
    ).

:- pred typecheck_pred_if_needed(module_info::in, pred_id::in,
    pred_info::in, pred_info::out, list(error_spec)::out,
    bool::out, bool::out, next_iteration::out) is det.

typecheck_pred_if_needed(ModuleInfo, PredId, !PredInfo, !:Specs,
        FoundSyntaxError, ContainsErrors, NextIteration) :-
    ( if is_pred_created_type_correct(ModuleInfo, !PredInfo) then
        !:Specs = [],
        FoundSyntaxError = no,
        ContainsErrors = no,
        NextIteration = next_iteration_is_not_needed
    else
        pred_info_get_clauses_info(!.PredInfo, ClausesInfo0),
        clauses_info_get_had_syntax_errors(ClausesInfo0, FoundSyntaxError0),
        ( FoundSyntaxError0 = no_clause_syntax_errors, FoundSyntaxError = no
        ; FoundSyntaxError0 = some_clause_syntax_errors, FoundSyntaxError = yes
        ),
        typecheck_predicate_if_stub(ModuleInfo, PredId, !PredInfo,
            FoundSyntaxError, !:Specs, MaybeNeedTypecheck),
        (
            MaybeNeedTypecheck = do_not_need_typecheck(ContainsErrors,
                NextIteration)
        ;
            MaybeNeedTypecheck = do_need_typecheck,
            do_typecheck_pred(ModuleInfo, PredId, !PredInfo, !Specs,
                NextIteration),
            module_info_get_globals(ModuleInfo, Globals),
            ContainsErrors = contains_errors(Globals, !.Specs)
        )
    ).

:- pred is_pred_created_type_correct(module_info::in,
    pred_info::in, pred_info::out) is semidet.

is_pred_created_type_correct(ModuleInfo, !PredInfo) :-
    ( if
        (
            % Most compiler-generated unify and compare predicates are created
            % already type-correct, so there is no need to typecheck them.
            % The exceptions are predicates that call a user-defined equality
            % or comparison predicate, and unify and compare predicates for
            % existentially typed data types.
            is_unify_index_or_compare_pred(!.PredInfo),
            not special_pred_needs_typecheck(!.PredInfo, ModuleInfo)
        ;
            % Most predicates for builtins are also created already
            % type-correct. The exceptions still need to have their stub
            % clauses generated; these are marked with marker_builtin_stub.
            % XXX Why the delay?
            pred_info_is_builtin(!.PredInfo),
            pred_info_get_markers(!.PredInfo, Markers),
            not check_marker(Markers, marker_builtin_stub)
        )
    then
        pred_info_get_clauses_info(!.PredInfo, ClausesInfo0),
        clauses_info_get_clauses_rep(ClausesInfo0, ClausesRep0, _ItemNumbers),
        IsEmpty = clause_list_is_empty(ClausesRep0),
        (
            IsEmpty = yes,
            pred_info_mark_as_external(!PredInfo)
        ;
            IsEmpty = no
        )
    else
        fail
    ).

:- type maybe_need_typecheck
    --->    do_not_need_typecheck(
                notc_contains_errors    :: bool,
                notc_next_iteration     :: next_iteration
            )
    ;       do_need_typecheck.

:- pred typecheck_predicate_if_stub(module_info::in, pred_id::in,
    pred_info::in, pred_info::out, bool::in,
    list(error_spec)::out, maybe_need_typecheck::out) is det.

typecheck_predicate_if_stub(ModuleInfo, PredId, !PredInfo, FoundSyntaxError,
        !:Specs, MaybeNeedTypecheck) :-
    % Handle the --allow-stubs and --warn-stubs options.
    % If --allow-stubs is set, and there are no clauses, then
    % - issue a warning (if --warn-stubs is set), and then
    % - generate a "stub" clause that just throws an exception.
    % The real work is done by do_typecheck_pred.

    module_info_get_globals(ModuleInfo, Globals),
    pred_info_get_markers(!.PredInfo, Markers0),

    pred_info_get_clauses_info(!.PredInfo, ClausesInfo0),
    clauses_info_get_clauses_rep(ClausesInfo0, ClausesRep0, ItemNumbers0),
    clause_list_is_empty(ClausesRep0) = ClausesRep0IsEmpty,
    (
        ClausesRep0IsEmpty = yes,
        % There are no clauses, so there can be no clause non-contiguity
        % errors.
        ( if
            globals.lookup_bool_option(Globals, allow_stubs, yes),
            not check_marker(Markers0, marker_class_method)
        then
            !:Specs =
                maybe_report_no_clauses_stub(ModuleInfo, PredId, !.PredInfo),
            generate_stub_clause(ModuleInfo, PredId, !PredInfo)
        else if
            check_marker(Markers0, marker_builtin_stub)
        then
            !:Specs = [],
            generate_stub_clause(ModuleInfo, PredId, !PredInfo)
        else
            !:Specs = []
        )
    ;
        ClausesRep0IsEmpty = no,
        % There are clauses, so there can be no need to add stub clauses.
        globals.lookup_bool_option(Globals, warn_non_contiguous_foreign_procs,
            WarnNonContiguousForeignProcs),
        (
            WarnNonContiguousForeignProcs = yes,
            !:Specs = report_any_non_contiguous_clauses(ModuleInfo,
                PredId, !.PredInfo, ItemNumbers0, clauses_and_foreign_procs)
        ;
            WarnNonContiguousForeignProcs = no,
            globals.lookup_bool_option(Globals, warn_non_contiguous_clauses,
                WarnNonContiguousClauses),
            (
                WarnNonContiguousClauses = yes,
                !:Specs = report_any_non_contiguous_clauses(ModuleInfo,
                    PredId, !.PredInfo, ItemNumbers0, only_clauses)
            ;
                WarnNonContiguousClauses = no,
                !:Specs = []
            )
        )
    ),

    % The above code may add stub clauses to the predicate, which would
    % invalidate ClausesInfo0.
    pred_info_get_clauses_info(!.PredInfo, ClausesInfo1),
    clauses_info_get_clauses_rep(ClausesInfo1, ClausesRep1, _ItemNumbers),
    clause_list_is_empty(ClausesRep1) = ClausesRep1IsEmpty,
    (
        ClausesRep1IsEmpty = yes,
        expect(unify(!.Specs, []), $pred, "starting Specs not empty"),

        % There are no clauses for class methods. The clauses are generated
        % later on, in polymorphism.expand_class_method_bodies.
        % XXX Why the delay?
        ( if check_marker(Markers0, marker_class_method) then
            % For the moment, we just insert the types of the head vars
            % into the clauses_info.
            clauses_info_get_headvar_list(ClausesInfo1, HeadVars),
            pred_info_get_arg_types(!.PredInfo, _ArgTypeVarSet, _ExistQVars,
                ArgTypes),
            vartypes_from_corresponding_lists(HeadVars, ArgTypes, VarTypes),
            clauses_info_set_vartypes(VarTypes, ClausesInfo1, ClausesInfo),
            pred_info_set_clauses_info(ClausesInfo, !PredInfo),
            % We also need to set the external_type_params field
            % to indicate that all the existentially quantified tvars
            % in the head of this pred are indeed bound by this predicate.
            type_vars_list(ArgTypes, HeadVarsInclExistentials),
            pred_info_set_external_type_params(HeadVarsInclExistentials,
                !PredInfo),
            ContainsErrors = no,
            !:Specs = []
        else
            ContainsErrors = yes,
            (
                FoundSyntaxError = no,
                !:Specs =
                    maybe_report_no_clauses(ModuleInfo, PredId, !.PredInfo)
            ;
                FoundSyntaxError = yes,
                % There were clauses, they just had errors. Printing a message
                % saying that there were no clauses would be misleading,
                % and the messages for the syntax errors will mean that
                % this compiler invocation won't succeed anyway.
                !:Specs = []
            )
        ),
        MaybeNeedTypecheck = do_not_need_typecheck(ContainsErrors,
            next_iteration_is_not_needed)
    ;
        ClausesRep1IsEmpty = no,
        (
            FoundSyntaxError = no,
            MaybeNeedTypecheck = do_need_typecheck
        ;
            FoundSyntaxError = yes,
            % Printing the messages we generated above could be misleading,
            % and the messages for the syntax errors will mean that
            % this compiler invocation won't succeed anyway.
            !:Specs = [],
            ContainsErrors = yes,
            MaybeNeedTypecheck = do_not_need_typecheck(ContainsErrors,
                next_iteration_is_not_needed)
        )
    ).

:- func report_any_non_contiguous_clauses(module_info, pred_id, pred_info,
    clause_item_numbers, clause_item_number_types) = list(error_spec).

report_any_non_contiguous_clauses(ModuleInfo, PredId, PredInfo, ItemNumbers,
        Type) = Specs :-
    ( if
        clauses_are_non_contiguous(ItemNumbers, Type,
            FirstRegion, SecondRegion, LaterRegions)
    then
        Spec = report_non_contiguous_clauses(ModuleInfo, PredId,
            PredInfo, FirstRegion, SecondRegion, LaterRegions),
        Specs = [Spec]
    else
        Specs = []
    ).

%---------------------------------------------------------------------------%

:- pred do_typecheck_pred(module_info::in, pred_id::in,
    pred_info::in, pred_info::out,
    list(error_spec)::in, list(error_spec)::out, next_iteration::out) is det.

do_typecheck_pred(ModuleInfo, PredId, !PredInfo, !Specs, NextIteration) :-
    some [!Info, !TypeAssignSet, !ClausesInfo, !ExternalTypeParams] (
        pred_info_get_clauses_info(!.PredInfo, !:ClausesInfo),
        clauses_info_get_clauses_rep(!.ClausesInfo, ClausesRep0, ItemNumbers),
        clauses_info_get_headvar_list(!.ClausesInfo, HeadVars),
        clauses_info_get_varset(!.ClausesInfo, ClauseVarSet),
        clauses_info_get_explicit_vartypes(!.ClausesInfo, ExplicitVarTypes0),
        pred_info_get_status(!.PredInfo, PredStatus),
        pred_info_get_typevarset(!.PredInfo, TypeVarSet0),
        pred_info_get_arg_types(!.PredInfo, _ArgTypeVarSet, ExistQVars0,
            ArgTypes0),
        pred_info_get_markers(!.PredInfo, Markers0),
        ( if check_marker(Markers0, marker_infer_type) then
            % For a predicate whose type is inferred, the predicate is allowed
            % to bind the type variables in the head of the predicate's type
            % declaration. Such predicates are given an initial type
            % declaration of `pred foo(T1, T2, ..., TN)' by make_hlds.m.
            Inferring = yes,
            trace [io(!IO)] (
                write_pred_progress_message("% Inferring type of ",
                    PredId, ModuleInfo, !IO)
            ),
            !:ExternalTypeParams = [],
            PredConstraints = constraints([], [])
        else
            Inferring = no,
            trace [io(!IO)] (
                write_pred_progress_message("% Type-checking ", PredId,
                    ModuleInfo, !IO)
            ),
            type_vars_list(ArgTypes0, !:ExternalTypeParams),
            pred_info_get_class_context(!.PredInfo, PredConstraints),
            constraint_list_get_tvars(PredConstraints ^ univ_constraints,
                UnivTVars),
            !:ExternalTypeParams = UnivTVars ++ !.ExternalTypeParams,
            list.sort_and_remove_dups(!ExternalTypeParams),
            list.delete_elems(!.ExternalTypeParams, ExistQVars0,
                !:ExternalTypeParams)
        ),

        module_info_get_class_table(ModuleInfo, ClassTable),
        make_head_hlds_constraints(ClassTable, TypeVarSet0,
            PredConstraints, Constraints),
        type_assign_set_init(TypeVarSet0, ExplicitVarTypes0,
            !.ExternalTypeParams, Constraints, !:TypeAssignSet),
        pred_info_get_markers(!.PredInfo, PredMarkers),
        typecheck_info_init(ModuleInfo, PredId, !.PredInfo,
            ClauseVarSet, PredStatus, PredMarkers, !.Specs, !:Info),
        get_clause_list_for_replacement(ClausesRep0, Clauses0),
        typecheck_clause_list(HeadVars, ArgTypes0, Clauses0, Clauses,
            !TypeAssignSet, !Info),
        % We need to perform a final pass of context reduction at the end,
        % before checking the typeclass constraints.
        pred_info_get_context(!.PredInfo, Context),
        perform_context_reduction(Context, !TypeAssignSet, !Info),
        typecheck_check_for_ambiguity(Context, whole_pred, HeadVars,
            !.TypeAssignSet, !Info),
        typecheck_check_for_unsatisfied_coercions(!.TypeAssignSet, !Info),
        type_assign_set_get_final_info(!.TypeAssignSet,
            !.ExternalTypeParams, ExistQVars0, ExplicitVarTypes0, TypeVarSet,
            !:ExternalTypeParams, InferredVarTypes0, InferredTypeConstraints0,
            ConstraintProofMap, ConstraintMap,
            TVarRenaming, ExistTypeRenaming),
        vartypes_optimize(InferredVarTypes0, InferredVarTypes),
        clauses_info_set_vartypes(InferredVarTypes, !ClausesInfo),

        % Apply substitutions to the explicit vartypes.
        (
            ExistQVars0 = [],
            ExplicitVarTypes1 = ExplicitVarTypes0
        ;
            ExistQVars0 = [_ | _],
            apply_variable_renaming_to_vartypes(ExistTypeRenaming,
                ExplicitVarTypes0, ExplicitVarTypes1)
        ),
        apply_variable_renaming_to_vartypes(TVarRenaming,
            ExplicitVarTypes1, ExplicitVarTypes),

        clauses_info_set_explicit_vartypes(ExplicitVarTypes, !ClausesInfo),
        set_clause_list(Clauses, ClausesRep),
        clauses_info_set_clauses_rep(ClausesRep, ItemNumbers, !ClausesInfo),
        pred_info_set_clauses_info(!.ClausesInfo, !PredInfo),
        pred_info_set_typevarset(TypeVarSet, !PredInfo),
        pred_info_set_constraint_proof_map(ConstraintProofMap, !PredInfo),
        pred_info_set_constraint_map(ConstraintMap, !PredInfo),

        % Split the inferred type class constraints into those that apply
        % only to the head variables, and those that apply to type variables
        % which occur only in the body.
        lookup_var_types(InferredVarTypes, HeadVars, ArgTypes),
        type_vars_list(ArgTypes, ArgTypeVars),
        restrict_to_head_vars(InferredTypeConstraints0, ArgTypeVars,
            InferredTypeConstraints, UnprovenBodyConstraints),

        % If there are any as-yet-unproven constraints on type variables
        % in the body, then save these in the pred_info. If it turns out that
        % this pass was the last pass of type inference, the post_typecheck
        % pass will report an error. But we can't report an error now, because
        % a later pass of type inference could cause some type variables
        % to become bound to types that make the constraints satisfiable,
        % causing the error to go away.
        pred_info_set_unproven_body_constraints(UnprovenBodyConstraints,
            !PredInfo),

        (
            Inferring = yes,
            % We need to infer which of the head variable types must be
            % existentially quantified.
            infer_existential_types(ArgTypeVars, ExistQVars,
                !ExternalTypeParams),

            % Now save the information we inferred in the pred_info.
            pred_info_set_external_type_params(!.ExternalTypeParams,
                !PredInfo),
            pred_info_set_arg_types(TypeVarSet, ExistQVars, ArgTypes,
                !PredInfo),
            pred_info_get_class_context(!.PredInfo, OldTypeConstraints),
            pred_info_set_class_context(InferredTypeConstraints, !PredInfo),

            % Check if anything changed.
            ( if
                (
                    % If the argument types and the type constraints are
                    % identical up to renaming, then nothing has changed.
                    pred_info_get_tvar_kind_map(!.PredInfo, TVarKindMap),
                    argtypes_identical_up_to_renaming(TVarKindMap, ExistQVars0,
                        ArgTypes0, OldTypeConstraints, ExistQVars, ArgTypes,
                        InferredTypeConstraints)
                ;
                    % Promises cannot be called from anywhere. Therefore
                    % even if the types of their arguments have changed,
                    % this fact won't affect the type analysis of any other
                    % predicate.
                    pred_info_get_goal_type(!.PredInfo, GoalType),
                    GoalType = goal_type_promise(_)
                )
            then
                NextIteration = next_iteration_is_not_needed
            else
                NextIteration = next_iteration_is_needed
            )
        ;
            Inferring = no,
            pred_info_set_external_type_params(!.ExternalTypeParams,
                !PredInfo),
            pred_info_get_origin(!.PredInfo, Origin0),

            % Leave the original argtypes etc., but apply any substitutions
            % that map existentially quantified type variables to other
            % type vars, and then rename them all to match the new typevarset,
            % so that the type variables names match up (e.g. with the type
            % variables in the constraint_proofs)

            % Apply any type substitutions that map existentially quantified
            % type variables to other type vars.
            (
                ExistQVars0 = [],
                % Optimize common case.
                ExistQVars1 = [],
                ArgTypes1 = ArgTypes0,
                PredConstraints1 = PredConstraints,
                Origin1 = Origin0
            ;
                ExistQVars0 = [_ | _],
                list.foldl(
                    check_existq_clause(TypeVarSet0, ExistQVars0),
                    Clauses, !Info),

                apply_renaming_in_vars(ExistTypeRenaming,
                    ExistQVars0, ExistQVars1),
                apply_variable_renaming_to_type_list(ExistTypeRenaming,
                    ArgTypes0, ArgTypes1),
                apply_variable_renaming_to_prog_constraints(
                    ExistTypeRenaming, PredConstraints, PredConstraints1),
                rename_instance_method_constraints(ExistTypeRenaming,
                    Origin0, Origin1)
            ),

            % Rename them all to match the new typevarset.
            apply_renaming_in_vars(TVarRenaming,
                ExistQVars1, ExistQVars),
            apply_variable_renaming_to_type_list(TVarRenaming, ArgTypes1,
                RenamedOldArgTypes),
            apply_variable_renaming_to_prog_constraints(TVarRenaming,
                PredConstraints1, RenamedOldConstraints),
            rename_instance_method_constraints(TVarRenaming, Origin1, Origin),

            % Save the results in the pred_info.
            pred_info_set_arg_types(TypeVarSet, ExistQVars, RenamedOldArgTypes,
                !PredInfo),
            pred_info_set_class_context(RenamedOldConstraints, !PredInfo),
            pred_info_set_origin(Origin, !PredInfo),

            NextIteration = next_iteration_is_not_needed
        ),
        typecheck_info_get_all_errors(!.Info, !:Specs)
    ).

:- pred check_existq_clause(tvarset::in, existq_tvars::in, clause::in,
    typecheck_info::in, typecheck_info::out) is det.

check_existq_clause(TypeVarSet, ExistQVars, Clause, !Info) :-
    Goal = Clause ^ clause_body,
    ( if Goal = hlds_goal(call_foreign_proc(_, _, _, _, _, _, Impl), _) then
        Context = Clause ^ clause_context,
        list.foldl2(check_mention_existq_var(Context, TypeVarSet, Impl),
            ExistQVars, 1, _N, !Info)
    else
        true
    ).

:- pred check_mention_existq_var(prog_context::in, tvarset::in,
    pragma_foreign_proc_impl::in, tvar::in, int::in, int::out,
    typecheck_info::in, typecheck_info::out) is det.

check_mention_existq_var(Context, TypeVarSet, Impl, TVar, !ExistQVarNum,
        !Info) :-
    varset.lookup_name(TypeVarSet, TVar, Name),
    OldVarName = "TypeInfo_for_" ++ Name,
    NewVarName = "TypeInfo_Out_" ++ string.int_to_string(!.ExistQVarNum),
    !:ExistQVarNum = !.ExistQVarNum + 1,
    ( if
        ( foreign_proc_uses_variable(Impl, OldVarName)
        ; foreign_proc_uses_variable(Impl, NewVarName)
        )
    then
        true
    else
        typecheck_info_get_error_clause_context(!.Info, ClauseContext),
        Spec = report_missing_tvar_in_foreign_code(ClauseContext, Context,
            OldVarName),
        typecheck_info_add_error(Spec, !Info)
    ).

    % Mark the predicate as a stub, and generate a clause of the form
    %   <p>(...) :-
    %       PredName = "<Predname>",
    %       private_builtin.no_clauses(PredName).
    % or
    %   <p>(...) :-
    %       PredName = "<Predname>",
    %       private_builtin.sorry(PredName).
    % depending on whether the predicate is part of
    % the Mercury standard library or not.
    %
:- pred generate_stub_clause(module_info::in, pred_id::in,
    pred_info::in, pred_info::out) is det.

generate_stub_clause(ModuleInfo, PredId, !PredInfo) :-
    some [!ClausesInfo] (
        pred_info_get_clauses_info(!.PredInfo, !:ClausesInfo),
        clauses_info_get_varset(!.ClausesInfo, VarSet0),
        PredPieces = describe_one_pred_name(ModuleInfo, should_module_qualify,
            PredId),
        PredName = error_pieces_to_string(PredPieces),
        generate_stub_clause_2(PredName, !PredInfo, ModuleInfo, StubClause,
            VarSet0, VarSet),
        set_clause_list([StubClause], ClausesRep),
        ItemNumbers = init_clause_item_numbers_comp_gen,
        clauses_info_set_clauses_rep(ClausesRep, ItemNumbers, !ClausesInfo),
        clauses_info_set_varset(VarSet, !ClausesInfo),
        pred_info_set_clauses_info(!.ClausesInfo, !PredInfo)
    ).

:- pred generate_stub_clause_2(string::in, pred_info::in, pred_info::out,
    module_info::in, clause::out, prog_varset::in, prog_varset::out) is det.

generate_stub_clause_2(PredName, !PredInfo, ModuleInfo, StubClause, !VarSet) :-
    % Mark the predicate as a stub, i.e. record that it originally
    % had no clauses.
    pred_info_get_markers(!.PredInfo, Markers0),
    add_marker(marker_stub, Markers0, Markers),
    pred_info_set_markers(Markers, !PredInfo),

    % Generate `PredName = "<PredName>"'.
    pred_info_get_context(!.PredInfo, Context),
    varset.new_named_var("PredName", PredNameVar, !VarSet),
    make_string_const_construction(Context, PredNameVar, PredName, UnifyGoal),

    % Generate `private_builtin.no_clauses(PredName)'
    % or `private_builtin.sorry(PredName)'
    ModuleName = pred_info_module(!.PredInfo),
    ( if mercury_std_library_module_name(ModuleName) then
        CalleeName = "sorry"
    else
        CalleeName = "no_clauses"
    ),
    generate_simple_call(ModuleInfo, mercury_private_builtin_module,
        CalleeName, pf_predicate, only_mode, detism_det, purity_pure,
        [PredNameVar], [], instmap_delta_bind_no_var, Context, CallGoal),

    % Combine the unification and call into a conjunction.
    goal_info_init(Context, GoalInfo),
    Body = hlds_goal(conj(plain_conj, [UnifyGoal, CallGoal]), GoalInfo),
    StubClause = clause(all_modes, Body, impl_lang_mercury, Context, []).

:- pred rename_instance_method_constraints(tvar_renaming::in,
    pred_origin::in, pred_origin::out) is det.

rename_instance_method_constraints(Renaming, Origin0, Origin) :-
    ( if Origin0 = origin_instance_method(MethodName, Constraints0) then
        Constraints0 = instance_method_constraints(ClassId, InstanceTypes0,
            InstanceConstraints0, ClassMethodClassContext0),
        apply_variable_renaming_to_type_list(Renaming, InstanceTypes0,
            InstanceTypes),
        apply_variable_renaming_to_prog_constraint_list(Renaming,
            InstanceConstraints0, InstanceConstraints),
        apply_variable_renaming_to_prog_constraints(Renaming,
            ClassMethodClassContext0, ClassMethodClassContext),
        Constraints = instance_method_constraints(ClassId,
            InstanceTypes, InstanceConstraints, ClassMethodClassContext),
        Origin = origin_instance_method(MethodName, Constraints)
    else
        Origin = Origin0
    ).

    % Infer which of the head variable types must be existentially quantified.
    %
:- pred infer_existential_types(list(tvar)::in, existq_tvars::out,
    external_type_params::in, external_type_params::out) is det.

infer_existential_types(ArgTypeVars, ExistQVars,
        ExternalTypeParams0, ExternalTypeParams) :-
    % First, infer which of the head variable types must be existentially
    % quantified: anything that was inserted into the ExternalTypeParams0 set
    % must have been inserted due to an existential type in something we
    % called, and thus must be existentially quantified. (Note that concrete
    % types are "more general" than existentially quantified types, so we
    % prefer to infer a concrete type if we can rather than an
    % existential type.)

    set.list_to_set(ArgTypeVars, ArgTypeVarsSet),
    set.list_to_set(ExternalTypeParams0, ExternalTypeParamsSet),
    set.intersect(ArgTypeVarsSet, ExternalTypeParamsSet, ExistQVarsSet),
    set.difference(ArgTypeVarsSet, ExistQVarsSet, UnivQVarsSet),
    set.to_sorted_list(ExistQVarsSet, ExistQVars),
    set.to_sorted_list(UnivQVarsSet, UnivQVars),

    % Then we need to insert the universally quantified head variable types
    % into the ExternalTypeParams set, which will now contain all the type
    % variables that are produced either by stuff we call or by our caller.
    % This is needed so that it has the right value when post_typecheck.m
    % uses it to check for unbound type variables.
    ExternalTypeParams = UnivQVars ++ ExternalTypeParams0.

    % restrict_to_head_vars(Constraints0, HeadVarTypes, Constraints,
    %       UnprovenConstraints):
    %
    % Constraints is the subset of Constraints0 which contain no type variables
    % other than those in HeadVarTypes. UnprovenConstraints is any unproven
    % (universally quantified) type constraints on variables not in
    % HeadVarTypes.
    %
:- pred restrict_to_head_vars(prog_constraints::in, list(tvar)::in,
    prog_constraints::out, list(prog_constraint)::out) is det.

restrict_to_head_vars(constraints(UnivCs0, ExistCs0), ArgVarTypes,
        constraints(UnivCs, ExistCs), UnprovenCs) :-
    restrict_to_head_vars_2(UnivCs0, ArgVarTypes, UnivCs, UnprovenCs),
    restrict_to_head_vars_2(ExistCs0, ArgVarTypes, ExistCs, _).

:- pred restrict_to_head_vars_2(list(prog_constraint)::in, list(tvar)::in,
    list(prog_constraint)::out, list(prog_constraint)::out) is det.

restrict_to_head_vars_2(ClassConstraints, HeadTypeVars, HeadClassConstraints,
        OtherClassConstraints) :-
    list.filter(is_head_class_constraint(HeadTypeVars),
        ClassConstraints, HeadClassConstraints, OtherClassConstraints).

:- pred is_head_class_constraint(list(tvar)::in, prog_constraint::in)
    is semidet.

is_head_class_constraint(HeadTypeVars, Constraint) :-
    Constraint = constraint(_ClassName, ArgTypes),
    all [TVar] (
            prog_type.type_list_contains_var(ArgTypes, TVar)
        =>
            list.member(TVar, HeadTypeVars)
    ).

    % Check whether the argument types, type quantifiers, and type constraints
    % are identical up to renaming.
    %
    % Note that we can't compare each of the parts separately, since we need
    % to ensure that the renaming (if any) is consistent over all the arguments
    % and all the constraints. So we need to append all the relevant types
    % into one big type list and then compare them in a single call
    % to identical_up_to_renaming.
    %
:- pred argtypes_identical_up_to_renaming(tvar_kind_map::in,
    existq_tvars::in, list(mer_type)::in, prog_constraints::in,
    existq_tvars::in, list(mer_type)::in, prog_constraints::in) is semidet.

argtypes_identical_up_to_renaming(KindMap, ExistQVarsA, ArgTypesA,
        TypeConstraintsA, ExistQVarsB, ArgTypesB, TypeConstraintsB) :-
    same_structure(TypeConstraintsA, TypeConstraintsB,
        ConstrainedTypesA, ConstrainedTypesB),
    prog_type.var_list_to_type_list(KindMap, ExistQVarsA, ExistQVarTypesA),
    prog_type.var_list_to_type_list(KindMap, ExistQVarsB, ExistQVarTypesB),
    list.condense([ExistQVarTypesA, ArgTypesA, ConstrainedTypesA], TypesListA),
    list.condense([ExistQVarTypesB, ArgTypesB, ConstrainedTypesB], TypesListB),
    identical_up_to_renaming(TypesListA, TypesListB).

    % Check if two sets of type class constraints have the same structure
    % (i.e. they specify the same list of type classes with the same arities)
    % and if so, concatenate the argument types for all the type classes
    % in each set of type class constraints and return them.
    %
:- pred same_structure(prog_constraints::in, prog_constraints::in,
    list(mer_type)::out, list(mer_type)::out) is semidet.

same_structure(ConstraintsA, ConstraintsB, TypesA, TypesB) :-
    ConstraintsA = constraints(UnivCsA, ExistCsA),
    ConstraintsB = constraints(UnivCsB, ExistCsB),
    % these calls to same_length are just an optimization,
    % to catch the simple cases quicker
    list.same_length(UnivCsA, UnivCsB),
    list.same_length(ExistCsA, ExistCsB),
    same_structure_2(UnivCsA, UnivCsB, UnivTypesA, UnivTypesB),
    same_structure_2(ExistCsA, ExistCsB, ExistTypesA, ExistTypesB),
    TypesA = ExistTypesA ++ UnivTypesA,
    TypesB = ExistTypesB ++ UnivTypesB.

:- pred same_structure_2(list(prog_constraint)::in, list(prog_constraint)::in,
    list(mer_type)::out, list(mer_type)::out) is semidet.

same_structure_2([], [], [], []).
same_structure_2([ConstraintA | ConstraintsA], [ConstraintB | ConstraintsB],
        TypesA, TypesB) :-
    ConstraintA = constraint(ClassName, ArgTypesA),
    ConstraintB = constraint(ClassName, ArgTypesB),
    list.same_length(ArgTypesA, ArgTypesB),
    same_structure_2(ConstraintsA, ConstraintsB, TypesA0, TypesB0),
    TypesA = ArgTypesA ++ TypesA0,
    TypesB = ArgTypesB ++ TypesB0.

    % Check whether two lists of types are identical up to renaming.
    %
:- pred identical_up_to_renaming(list(mer_type)::in, list(mer_type)::in)
    is semidet.

identical_up_to_renaming(TypesList1, TypesList2) :-
    % They are identical up to renaming if they each subsume each other.
    type_list_subsumes(TypesList1, TypesList2, _),
    type_list_subsumes(TypesList2, TypesList1, _).

    % A compiler-generated predicate only needs type checking if
    % (a) it is a user-defined equality pred, or
    % (b) it is the unification or comparison predicate for an existentially
    %     quantified type.
    %
    % In case (b), we need to typecheck it to fill in the external_type_params
    % field in the pred_info.
    %
:- pred special_pred_needs_typecheck(pred_info::in, module_info::in)
    is semidet.

special_pred_needs_typecheck(PredInfo, ModuleInfo) :-
    % Check if the predicate is a compiler-generated special
    % predicate, and if so, for which type.
    pred_info_get_origin(PredInfo, Origin),
    Origin = origin_special_pred(SpecialPredId, TypeCtor),

    % Check that the special pred isn't one of the builtin types which don't
    % have a hlds_type_defn.
    not list.member(TypeCtor, builtin_type_ctors_with_no_hlds_type_defn),

    % Check whether that type is a type for which there is a user-defined
    % equality predicate, or which is existentially typed.
    module_info_get_type_table(ModuleInfo, TypeTable),
    lookup_type_ctor_defn(TypeTable, TypeCtor, TypeDefn),
    hlds_data.get_type_defn_body(TypeDefn, Body),
    special_pred_for_type_needs_typecheck(ModuleInfo, SpecialPredId, Body).

%---------------------------------------------------------------------------%

    % Iterate over the list of clauses for a predicate.
    %
:- pred typecheck_clause_list(list(prog_var)::in, list(mer_type)::in,
    list(clause)::in, list(clause)::out,
    type_assign_set::in, type_assign_set::out,
    typecheck_info::in, typecheck_info::out) is det.

typecheck_clause_list(_, _, [], [], !TypeAssignSet, !Info).
typecheck_clause_list(HeadVars, ArgTypes, [Clause0 | Clauses0],
        [Clause | Clauses], !TypeAssignSet, !Info) :-
    typecheck_clause(HeadVars, ArgTypes, Clause0, Clause,
        !TypeAssignSet, !Info),
    typecheck_clause_list(HeadVars, ArgTypes, Clauses0, Clauses,
        !TypeAssignSet, !Info).

%---------------------------------------------------------------------------%

    % Type-check a single clause.
    %
    % As we go through a clause, we determine the set of possible type
    % assignments for the clause. A type assignment is an assignment of a type
    % to each variable in the clause.
    %
    % Note that this may have exponential complexity for both time and space.
    % If there are n variables Vi (for i in 1..n) that may each have either
    % type Ti1 or Ti2, then we generate 2^n type assignments to represent all
    % the possible combinations of their types. This can easily be a serious
    % problem for even medium-sized predicates that extensively use function
    % symbols that belong to more than one type (such as `no', which belongs
    % to both `bool' and `maybe').
    %
    % The pragmatic short-term solution we apply here is to generate a warning
    % when the number of type assignments exceeds one bound (given by the value
    % of the typecheck_ambiguity_warn_limit option), and an error when it
    % exceeds another, higher bound (given by typecheck_ambiguity_error_limit).
    %
    % The better but more long-term solution is to switch to using
    % a constraint based type checker, which does not need to materialize
    % the cross product of all the possible type assignments of different
    % variables in a clause. The module type_constraints.m contains
    % an incomplete prototype of such a type checker.
    %
:- pred typecheck_clause(list(prog_var)::in, list(mer_type)::in,
    clause::in, clause::out, type_assign_set::in, type_assign_set::out,
    typecheck_info::in, typecheck_info::out) is det.

typecheck_clause(HeadVars, ArgTypes, !Clause, !TypeAssignSet, !Info) :-
    Body0 = !.Clause ^ clause_body,
    Context = !.Clause ^ clause_context,

    % Typecheck the clause - first the head unification, and then the body.
    ArgVectorKind = arg_vector_clause_head,
    typecheck_vars_have_types(ArgVectorKind, Context, HeadVars, ArgTypes,
        !TypeAssignSet, !Info),
    typecheck_goal(Body0, Body, Context, !TypeAssignSet, !Info),
    trace [compiletime(flag("type_checkpoint")), io(!IO)] (
        typecheck_info_get_error_clause_context(!.Info, ClauseContext),
        ModuleInfo = ClauseContext ^ tecc_module_info,
        VarSet = ClauseContext ^ tecc_varset,
        type_checkpoint("end of clause", ModuleInfo, VarSet, !.TypeAssignSet,
            !IO)
    ),
    typecheck_prune_coerce_constraints(!TypeAssignSet, !Info),
    !Clause ^ clause_body := Body,
    typecheck_check_for_ambiguity(Context, clause_only, HeadVars,
        !.TypeAssignSet, !Info).
    % We should perhaps do manual garbage collection here.

%---------------------------------------------------------------------------%

:- type stuff_to_check
    --->    clause_only
    ;       whole_pred.

    % If there are multiple type assignments, then issue an error message.
    %
    % If stuff-to-check = whole_pred, report an error for any ambiguity,
    % and also check for unbound type variables.
    % But if stuff-to-check = clause_only, then only report errors
    % for type ambiguities that don't involve the head vars, because
    % we may be able to resolve a type ambiguity for a head var in one clause
    % by looking at later clauses. (Ambiguities in the head variables
    % can only arise if we are inferring the type for this pred.)
    %
:- pred typecheck_check_for_ambiguity(prog_context::in, stuff_to_check::in,
    list(prog_var)::in, type_assign_set::in,
    typecheck_info::in, typecheck_info::out) is det.

typecheck_check_for_ambiguity(Context, StuffToCheck, HeadVars,
        TypeAssignSet, !Info) :-
    (
        % There should always be a type assignment, because if there is
        % an error somewhere, instead of setting the current type assignment
        % set to the empty set, the type-checker should continue with the
        % previous type assignment set (so that it can detect other errors
        % in the same clause).
        TypeAssignSet = [],
        unexpected($pred, "no type-assignment")
    ;
        TypeAssignSet = [_SingleTypeAssign]
    ;
        TypeAssignSet = [TypeAssign1, TypeAssign2 | _],
        % XXX Why do we check only the first two type assigns?

        % We only report an ambiguity error if
        % (a) we haven't encountered any other errors and if
        %     StuffToCheck = clause_only(_), and also
        % (b) the ambiguity occurs only in the body, rather than in the
        %     head variables (and hence can't be resolved by looking at
        %     later clauses).

        typecheck_info_get_all_errors(!.Info, ErrorsSoFar),
        ( if
            ErrorsSoFar = [],
            (
                StuffToCheck = whole_pred
            ;
                StuffToCheck = clause_only,

                % Only report an error if the headvar types are identical
                % (which means that the ambiguity must have occurred
                % in the body).
                type_assign_get_var_types(TypeAssign1, VarTypes1),
                type_assign_get_var_types(TypeAssign2, VarTypes2),
                type_assign_get_type_bindings(TypeAssign1, TypeBindings1),
                type_assign_get_type_bindings(TypeAssign2, TypeBindings2),
                lookup_var_types(VarTypes1, HeadVars, HeadTypes1),
                lookup_var_types(VarTypes2, HeadVars, HeadTypes2),
                apply_rec_subst_to_type_list(TypeBindings1, HeadTypes1,
                    FinalHeadTypes1),
                apply_rec_subst_to_type_list(TypeBindings2, HeadTypes2,
                    FinalHeadTypes2),
                identical_up_to_renaming(FinalHeadTypes1, FinalHeadTypes2)
            )
        then
            typecheck_info_get_error_clause_context(!.Info, ClauseContext),
            typecheck_info_get_overloaded_symbol_map(!.Info,
                OverloadedSymbolMap),
            Spec = report_ambiguity_error(ClauseContext, Context,
                OverloadedSymbolMap, TypeAssign1, TypeAssign2),
            typecheck_info_add_error(Spec, !Info)
        else
            true
        )
    ).

%---------------------------------------------------------------------------%

:- pred typecheck_check_for_unsatisfied_coercions(type_assign_set::in,
    typecheck_info::in, typecheck_info::out) is det.

typecheck_check_for_unsatisfied_coercions(TypeAssignSet, !Info) :-
    (
        TypeAssignSet = [],
        unexpected($pred, "no type-assignment")
    ;
        TypeAssignSet = [TypeAssign],
        type_assign_get_coerce_constraints(TypeAssign, Coercions),
        (
            Coercions = []
        ;
            Coercions = [_ | _],
            % All valid coercion constraints have been removed from the
            % type assignment already.
            list.foldl(report_invalid_coercion(TypeAssign), Coercions, !Info)
        )
    ;
        TypeAssignSet = [_, _ | _]
        % If there are multple type assignments then there is a type ambiguity
        % error anyway. Reporting invalid coercions from different type
        % assignments would be confusing.
    ).

:- pred report_invalid_coercion(type_assign::in, coerce_constraint::in,
    typecheck_info::in, typecheck_info::out) is det.

report_invalid_coercion(TypeAssign, Coercion, !Info) :-
    % XXX When inferring types for a predicate/function with no declared type,
    % we should not report coercions as invalid until the argument types have
    % been inferred.
    Coercion = coerce_constraint(FromType0, ToType0, Context, _Status),
    type_assign_get_typevarset(TypeAssign, TVarSet),
    type_assign_get_type_bindings(TypeAssign, TypeBindings),
    apply_rec_subst_to_type(TypeBindings, FromType0, FromType),
    apply_rec_subst_to_type(TypeBindings, ToType0, ToType),
    typecheck_info_get_error_clause_context(!.Info, ClauseContext),
    Spec = report_invalid_coerce_from_to(ClauseContext, Context, TVarSet,
        FromType, ToType),
    typecheck_info_add_error(Spec, !Info).

%---------------------------------------------------------------------------%

:- pred typecheck_goal(hlds_goal::in, hlds_goal::out, prog_context::in,
    type_assign_set::in, type_assign_set::out,
    typecheck_info::in, typecheck_info::out) is det.

typecheck_goal(Goal0, Goal, EnclosingContext, !TypeAssignSet, !Info) :-
    % If the context of the goal is empty, we set the context of the goal
    % to the surrounding context. (That should probably be done in make_hlds,
    % but it was easier to do here.)
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    Context0 = goal_info_get_context(GoalInfo0),
    ( if is_dummy_context(Context0) then
        Context = EnclosingContext,
        goal_info_set_context(Context, GoalInfo0, GoalInfo)
    else
        Context = Context0,
        GoalInfo = GoalInfo0
    ),

    % Our algorithm handles overloading quite inefficiently: for each
    % unification of a variable with a function symbol that matches N type
    % declarations, we make N copies of the existing set of type assignments.
    % In the worst case, therefore, the complexity of our algorithm
    % (space complexity as well as time complexity) is therefore exponential
    % in the number of ambiguous symbols.
    %
    % We issue a warning whenever the number of type assignments exceeds
    % the warn limit, and stop typechecking (after generating an error)
    % whenever it exceeds the error limit.

    list.length(!.TypeAssignSet, NumTypeAssignSets),
    typecheck_info_get_ambiguity_warn_limit(!.Info, WarnLimit),
    ( if NumTypeAssignSets > WarnLimit then
        typecheck_info_get_ambiguity_error_limit(!.Info, ErrorLimit),
        typecheck_info_get_error_clause_context(!.Info, ClauseContext),
        typecheck_info_get_overloaded_symbol_map(!.Info, OverloadedSymbolMap),
        ( if NumTypeAssignSets > ErrorLimit then
            % Override any existing overload warning.
            ErrorSpec = report_error_too_much_overloading(ClauseContext,
                Context, OverloadedSymbolMap),
            typecheck_info_set_overload_error(yes(ErrorSpec), !Info),

            % Don't call typecheck_goal_expr to do the actual typechecking,
            % since it will almost certainly take too much time and memory.
            GoalExpr = GoalExpr0
        else
            typecheck_info_get_overload_error(!.Info, MaybePrevSpec),
            (
                MaybePrevSpec = no,
                WarnSpec = report_warning_too_much_overloading(ClauseContext,
                    Context, OverloadedSymbolMap),
                typecheck_info_set_overload_error(yes(WarnSpec), !Info)
            ;
                MaybePrevSpec = yes(_)
            ),
            typecheck_goal_expr(GoalExpr0, GoalExpr, GoalInfo,
                !TypeAssignSet, !Info)
        )
    else
        typecheck_goal_expr(GoalExpr0, GoalExpr, GoalInfo,
            !TypeAssignSet, !Info)
    ),
    Goal = hlds_goal(GoalExpr, GoalInfo).

:- pred typecheck_goal_expr(hlds_goal_expr::in, hlds_goal_expr::out,
    hlds_goal_info::in, type_assign_set::in, type_assign_set::out,
    typecheck_info::in, typecheck_info::out) is det.

typecheck_goal_expr(GoalExpr0, GoalExpr, GoalInfo, !TypeAssignSet, !Info) :-
    typecheck_info_get_error_clause_context(!.Info, ClauseContext),
    ModuleInfo = ClauseContext ^ tecc_module_info,
    VarSet = ClauseContext ^ tecc_varset,
    Context = goal_info_get_context(GoalInfo),
    (
        GoalExpr0 = conj(ConjType, List0),
        trace [compiletime(flag("type_checkpoint")), io(!IO)] (
            type_checkpoint("conj", ModuleInfo, VarSet, !.TypeAssignSet, !IO)
        ),
        typecheck_goal_list(List0, List, Context, !TypeAssignSet, !Info),
        GoalExpr = conj(ConjType, List)
    ;
        GoalExpr0 = disj(List0),
        trace [compiletime(flag("type_checkpoint")), io(!IO)] (
            type_checkpoint("disj", ModuleInfo, VarSet, !.TypeAssignSet, !IO)
        ),
        typecheck_goal_list(List0, List, Context, !TypeAssignSet, !Info),
        GoalExpr = disj(List)
    ;
        GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),
        trace [compiletime(flag("type_checkpoint")), io(!IO)] (
            type_checkpoint("if", ModuleInfo, VarSet, !.TypeAssignSet, !IO)
        ),
        typecheck_goal(Cond0, Cond, Context, !TypeAssignSet, !Info),
        trace [compiletime(flag("type_checkpoint")), io(!IO)] (
            type_checkpoint("then", ModuleInfo, VarSet, !.TypeAssignSet, !IO)
        ),
        typecheck_goal(Then0, Then, Context, !TypeAssignSet, !Info),
        trace [compiletime(flag("type_checkpoint")), io(!IO)] (
            type_checkpoint("else", ModuleInfo, VarSet, !.TypeAssignSet, !IO)
        ),
        typecheck_goal(Else0, Else, Context, !TypeAssignSet, !Info),
        ensure_vars_have_a_type(var_vector_cond_quant, Context, Vars,
            !TypeAssignSet, !Info),
        GoalExpr = if_then_else(Vars, Cond, Then, Else)
    ;
        GoalExpr0 = negation(SubGoal0),
        trace [compiletime(flag("type_checkpoint")), io(!IO)] (
            type_checkpoint("not", ModuleInfo, VarSet, !.TypeAssignSet, !IO)
        ),
        typecheck_goal(SubGoal0, SubGoal, Context, !TypeAssignSet, !Info),
        GoalExpr = negation(SubGoal)
    ;
        GoalExpr0 = scope(Reason, SubGoal0),
        trace [compiletime(flag("type_checkpoint")), io(!IO)] (
            type_checkpoint("scope", ModuleInfo, VarSet, !.TypeAssignSet, !IO)
        ),
        typecheck_goal(SubGoal0, SubGoal, Context, !TypeAssignSet, !Info),
        (
            (
                (
                    Reason = exist_quant(Vars),
                    VarVectorKind = var_vector_exist_quant
                ;
                    Reason = promise_solutions(Vars, _),
                    VarVectorKind = var_vector_promise_solutions
                )
            ;
                % These variables are introduced by the compiler and may
                % only have a single, specific type.
                Reason = loop_control(LCVar, LCSVar, _),
                Vars = [LCVar, LCSVar],
                VarVectorKind = var_vector_loop_control
            ),
            ensure_vars_have_a_type(VarVectorKind, Context, Vars,
                !TypeAssignSet, !Info)
        ;
            ( Reason = disable_warnings(_, _)
            ; Reason = promise_purity(_)
            ; Reason = require_detism(_)
            ; Reason = require_complete_switch(_)
            ; Reason = require_switch_arms_detism(_, _)
            ; Reason = commit(_)
            ; Reason = barrier(_)
            ; Reason = from_ground_term(_, _)
            ; Reason = trace_goal(_, _, _, _, _)
            )
        ),
        GoalExpr = scope(Reason, SubGoal)
    ;
        GoalExpr0 = plain_call(_, ProcId, Args, BI, UC, Name),
        trace [compiletime(flag("type_checkpoint")), io(!IO)] (
            type_checkpoint("call", ModuleInfo, VarSet, !.TypeAssignSet, !IO)
        ),
        list.length(Args, Arity),
        SymNameArity = sym_name_arity(Name, Arity),
        GoalId = goal_info_get_goal_id(GoalInfo),
        typecheck_call_pred_name(SymNameArity, Context, GoalId, Args, PredId,
            !TypeAssignSet, !Info),
        GoalExpr = plain_call(PredId, ProcId, Args, BI, UC, Name)
    ;
        GoalExpr0 = generic_call(GenericCall, Args, _Modes, _MaybeArgRegs,
            _Detism),
        (
            GenericCall = higher_order(PredVar, Purity, _, _),
            trace [compiletime(flag("type_checkpoint")), io(!IO)] (
                type_checkpoint("higher-order call", ModuleInfo, VarSet,
                    !.TypeAssignSet, !IO)
            ),
            hlds_goal.generic_call_to_id(GenericCall, GenericCallId),
            typecheck_higher_order_call(GenericCallId, Context,
                PredVar, Purity, Args, !TypeAssignSet, !Info)
        ;
            GenericCall = class_method(_, _, _, _),
            unexpected($pred, "unexpected class method call")
        ;
            GenericCall = event_call(EventName),
            trace [compiletime(flag("type_checkpoint")), io(!IO)] (
                type_checkpoint("event call", ModuleInfo, VarSet,
                    !.TypeAssignSet, !IO)
            ),
            typecheck_event_call(Context, EventName, Args,
                !TypeAssignSet, !Info)
        ;
            GenericCall = cast(_)
            % A cast imposes no restrictions on its argument types,
            % so nothing needs to be done here.
        ;
            GenericCall = subtype_coerce,
            trace [compiletime(flag("type_checkpoint")), io(!IO)] (
                type_checkpoint("coerce", ModuleInfo, VarSet,
                    !.TypeAssignSet, !IO)
            ),
            typecheck_coerce(Context, Args, !TypeAssignSet, !Info)
        ),
        GoalExpr = GoalExpr0
    ;
        GoalExpr0 = unify(LHS, RHS0, UnifyMode, Unification, UnifyContext),
        trace [compiletime(flag("type_checkpoint")), io(!IO)] (
            type_checkpoint("unify", ModuleInfo, VarSet, !.TypeAssignSet, !IO)
        ),
        GoalId = goal_info_get_goal_id(GoalInfo),
        typecheck_unification(UnifyContext, Context, GoalId,
            LHS, RHS0, RHS, !TypeAssignSet, !Info),
        GoalExpr = unify(LHS, RHS, UnifyMode, Unification, UnifyContext)
    ;
        GoalExpr0 = switch(_, _, _),
        % We haven't run switch detection yet.
        unexpected($pred, "switch")
    ;
        GoalExpr0 = call_foreign_proc(_, PredId, _, Args, _, _, _),
        % Foreign_procs are automatically generated, so they will always be
        % type-correct, but we need to do the type analysis in order to
        % correctly compute the HeadTypeParams that result from existentially
        % typed foreign_procs. (We could probably do that more efficiently
        % than the way it is done below, though.)
        ArgVectorKind = arg_vector_foreign_proc_call(PredId),
        ArgVars = list.map(foreign_arg_var, Args),
        GoalId = goal_info_get_goal_id(GoalInfo),
        typecheck_call_pred_id(ArgVectorKind, Context, GoalId,
            PredId, ArgVars, !TypeAssignSet, !Info),
        perform_context_reduction(Context, !TypeAssignSet, !Info),
        GoalExpr = GoalExpr0
    ;
        GoalExpr0 = shorthand(ShortHand0),
        (
            ShortHand0 = bi_implication(LHS0, RHS0),
            trace [compiletime(flag("type_checkpoint")), io(!IO)] (
                type_checkpoint("<=>", ModuleInfo, VarSet,
                    !.TypeAssignSet, !IO)
            ),
            typecheck_goal(LHS0, LHS, Context, !TypeAssignSet, !Info),
            typecheck_goal(RHS0, RHS, Context, !TypeAssignSet, !Info),
            ShortHand = bi_implication(LHS, RHS)
        ;
            ShortHand0 = atomic_goal(GoalType, Outer, Inner, MaybeOutputVars,
                MainGoal0, OrElseGoals0, OrElseInners),
            trace [compiletime(flag("type_checkpoint")), io(!IO)] (
                type_checkpoint("atomic_goal", ModuleInfo, VarSet,
                    !.TypeAssignSet, !IO)
            ),
            (
                MaybeOutputVars = yes(OutputVars),
                VarVectorKindOutput = var_vector_atomic_output,
                ensure_vars_have_a_type(VarVectorKindOutput, Context,
                    OutputVars, !TypeAssignSet, !Info)
            ;
                MaybeOutputVars = no
            ),

            typecheck_goal(MainGoal0, MainGoal, Context,
                !TypeAssignSet, !Info),
            typecheck_goal_list(OrElseGoals0, OrElseGoals, Context,
                !TypeAssignSet, !Info),

            VarVectorKindOuter = var_vector_atomic_outer,
            Outer = atomic_interface_vars(OuterDI, OuterUO),
            ensure_vars_have_a_single_type(VarVectorKindOuter, Context,
                [OuterDI, OuterUO], !TypeAssignSet, !Info),

            % The outer variables must either be both I/O states or STM states.
            % Checking that here could double the number of type assign sets.
            % We therefore delay the check until after we have typechecked
            % the predicate body, in post_typecheck. The code in the
            % post_typecheck pass (actually in purity.m) will do this
            % if the GoalType is unknown_atomic_goal_type.
            InnerVars =
                atomic_interface_list_to_var_list([Inner | OrElseInners]),
            list.foldl2(typecheck_var_has_stm_atomic_type(Context),
                InnerVars, !TypeAssignSet, !Info),
            expect(unify(GoalType, unknown_atomic_goal_type), $pred,
                "GoalType != unknown_atomic_goal_type"),
            ShortHand = atomic_goal(GoalType, Outer, Inner, MaybeOutputVars,
                MainGoal, OrElseGoals, OrElseInners)
        ;
            ShortHand0 = try_goal(MaybeIO, ResultVar, SubGoal0),
            trace [compiletime(flag("type_checkpoint")), io(!IO)] (
                type_checkpoint("try_goal", ModuleInfo, VarSet,
                    !.TypeAssignSet, !IO)
            ),
            typecheck_goal(SubGoal0, SubGoal, Context, !TypeAssignSet, !Info),
            (
                MaybeIO = yes(try_io_state_vars(InitialIO, FinalIO)),
                VarVectorKind = var_vector_try_io,
                ensure_vars_have_a_type(VarVectorKind, Context,
                    [InitialIO, FinalIO], !TypeAssignSet, !Info),
                InitialGoalContext =
                    type_error_in_var_vector(VarVectorKind, 1),
                FinalGoalContext =
                    type_error_in_var_vector(VarVectorKind, 2),
                typecheck_var_has_type(InitialGoalContext, Context,
                    InitialIO, io_state_type, !TypeAssignSet, !Info),
                typecheck_var_has_type(FinalGoalContext, Context,
                    FinalIO, io_state_type, !TypeAssignSet, !Info)
            ;
                MaybeIO = no
            ),
            ShortHand = try_goal(MaybeIO, ResultVar, SubGoal)
        ),
        GoalExpr = shorthand(ShortHand)
    ).

:- func atomic_interface_list_to_var_list(list(atomic_interface_vars)) =
    list(prog_var).

atomic_interface_list_to_var_list([]) = [].
atomic_interface_list_to_var_list([atomic_interface_vars(I, O) | Interfaces]) =
    [I, O | atomic_interface_list_to_var_list(Interfaces)].

%---------------------------------------------------------------------------%

:- pred typecheck_goal_list(list(hlds_goal)::in, list(hlds_goal)::out,
    prog_context::in, type_assign_set::in, type_assign_set::out,
    typecheck_info::in, typecheck_info::out) is det.

typecheck_goal_list([], [], _, !TypeAssignSet, !Info).
typecheck_goal_list([Goal0 | Goals0], [Goal | Goals], Context,
        !TypeAssignSet, !Info) :-
    typecheck_goal(Goal0, Goal, Context, !TypeAssignSet, !Info),
    typecheck_goal_list(Goals0, Goals, Context, !TypeAssignSet, !Info).

%---------------------------------------------------------------------------%

    % Ensure that each variable in Vars has been assigned a type.
    %
:- pred ensure_vars_have_a_type(var_vector_kind::in, prog_context::in,
    list(prog_var)::in, type_assign_set::in, type_assign_set::out,
    typecheck_info::in, typecheck_info::out) is det.

ensure_vars_have_a_type(VarVectorKind, Context, Vars, !TypeAssignSet, !Info) :-
    (
        Vars = []
    ;
        Vars = [_ | _],
        % Invent some new type variables to use as the types of these
        % variables. Since each type is the type of a program variable,
        % each must have kind `star'.
        list.length(Vars, NumVars),
        varset.init(TypeVarSet0),
        varset.new_vars(NumVars, TypeVars, TypeVarSet0, TypeVarSet),
        prog_type.var_list_to_type_list(map.init, TypeVars, Types),
        empty_hlds_constraints(EmptyConstraints),
        typecheck_var_has_polymorphic_type_list(VarVectorKind, Context,
            Vars, TypeVarSet, [], Types, EmptyConstraints,
            !TypeAssignSet, !Info)
    ).

    % Ensure that each variable in Vars has been assigned a single type.
    %
:- pred ensure_vars_have_a_single_type(var_vector_kind::in, prog_context::in,
    list(prog_var)::in, type_assign_set::in, type_assign_set::out,
    typecheck_info::in, typecheck_info::out) is det.

ensure_vars_have_a_single_type(VarVectorKind, Context, Vars,
        !TypeAssignSet, !Info) :-
    (
        Vars = []
    ;
        Vars = [_ | _],
        % Invent a new type variable to use as the type of these
        % variables. Since the type is the type of a program variable,
        % each must have kind `star'.
        varset.init(TypeVarSet0),
        varset.new_var(TypeVar, TypeVarSet0, TypeVarSet),
        Type = type_variable(TypeVar, kind_star),
        list.length(Vars, NumVars),
        list.duplicate(NumVars, Type, Types),
        empty_hlds_constraints(EmptyConstraints),
        typecheck_var_has_polymorphic_type_list(VarVectorKind, Context,
            Vars, TypeVarSet, [], Types, EmptyConstraints,
            !TypeAssignSet, !Info)
    ).

%---------------------------------------------------------------------------%

:- pred typecheck_higher_order_call(generic_call_id::in, prog_context::in,
    prog_var::in, purity::in, list(prog_var)::in,
    type_assign_set::in, type_assign_set::out,
    typecheck_info::in, typecheck_info::out) is det.

typecheck_higher_order_call(GenericCallId, Context, PredVar, Purity, Args,
        !TypeAssignSet, !Info) :-
    list.length(Args, Arity),
    higher_order_pred_type(Purity, Arity, lambda_normal,
        TypeVarSet, PredVarType, ArgTypes),
    VarVectorKind = var_vector_args(arg_vector_generic_call(GenericCallId)),
    % The class context is empty because higher-order predicates
    % are always monomorphic. Similarly for ExistQVars.
    empty_hlds_constraints(EmptyConstraints),
    ExistQVars = [],
    typecheck_var_has_polymorphic_type_list(VarVectorKind, Context,
        [PredVar | Args], TypeVarSet, ExistQVars, [PredVarType | ArgTypes],
        EmptyConstraints, !TypeAssignSet, !Info).

    % higher_order_pred_type(Purity, N, EvalMethod,
    %   TypeVarSet, PredType, ArgTypes):
    %
    % Given an arity N, let TypeVarSet = {T1, T2, ..., TN},
    % PredType = `Purity EvalMethod pred(T1, T2, ..., TN)', and
    % ArgTypes = [T1, T2, ..., TN].
    %
:- pred higher_order_pred_type(purity::in, int::in, lambda_eval_method::in,
    tvarset::out, mer_type::out, list(mer_type)::out) is det.

higher_order_pred_type(Purity, Arity, EvalMethod, TypeVarSet, PredType,
        ArgTypes) :-
    varset.init(TypeVarSet0),
    varset.new_vars(Arity, ArgTypeVars, TypeVarSet0, TypeVarSet),
    % Argument types always have kind `star'.
    prog_type.var_list_to_type_list(map.init, ArgTypeVars, ArgTypes),
    construct_higher_order_type(Purity, pf_predicate, EvalMethod, ArgTypes,
        PredType).

    % higher_order_func_type(Purity, N, EvalMethod, TypeVarSet,
    %   FuncType, ArgTypes, RetType):
    %
    % Given an arity N, let TypeVarSet = {T0, T1, T2, ..., TN},
    % FuncType = `Purity EvalMethod func(T1, T2, ..., TN) = T0',
    % ArgTypes = [T1, T2, ..., TN], and
    % RetType = T0.
    %
:- pred higher_order_func_type(purity::in, int::in, lambda_eval_method::in,
    tvarset::out, mer_type::out, list(mer_type)::out, mer_type::out) is det.

higher_order_func_type(Purity, Arity, EvalMethod, TypeVarSet,
        FuncType, ArgTypes, RetType) :-
    varset.init(TypeVarSet0),
    varset.new_vars(Arity, ArgTypeVars, TypeVarSet0, TypeVarSet1),
    varset.new_var(RetTypeVar, TypeVarSet1, TypeVarSet),
    % Argument and return types always have kind `star'.
    prog_type.var_list_to_type_list(map.init, ArgTypeVars, ArgTypes),
    RetType = type_variable(RetTypeVar, kind_star),
    construct_higher_order_func_type(Purity, EvalMethod, ArgTypes, RetType,
        FuncType).

%---------------------------------------------------------------------------%

:- pred typecheck_event_call(prog_context::in, string::in, list(prog_var)::in,
    type_assign_set::in, type_assign_set::out,
    typecheck_info::in, typecheck_info::out) is det.

typecheck_event_call(Context, EventName, Args, !TypeAssignSet, !Info) :-
    typecheck_info_get_module_info(!.Info, ModuleInfo),
    module_info_get_event_set(ModuleInfo, EventSet),
    EventSpecMap = EventSet ^ event_set_spec_map,
    ( if event_arg_types(EventSpecMap, EventName, EventArgTypes) then
        list.length(Args, NumArgs),
        list.length(EventArgTypes, NumEventArgTypes),
        ( if NumArgs = NumEventArgTypes then
            ArgVectorKind = arg_vector_event(EventName),
            typecheck_vars_have_types(ArgVectorKind, Context,
                Args, EventArgTypes, !TypeAssignSet, !Info)
        else
            Spec = report_event_args_mismatch(Context,
                EventName, EventArgTypes, Args),
            typecheck_info_add_error(Spec, !Info)
        )
    else
        Spec = report_unknown_event_call_error(Context, EventName),
        typecheck_info_add_error(Spec, !Info)
    ).

%---------------------------------------------------------------------------%

:- pred typecheck_call_pred_name(sym_name_arity::in, prog_context::in,
    goal_id::in, list(prog_var)::in, pred_id::out,
    type_assign_set::in, type_assign_set::out,
    typecheck_info::in, typecheck_info::out) is det.

typecheck_call_pred_name(SymNameArity, Context, GoalId, Args, PredId,
        !TypeAssignSet, !Info) :-
    % Look up the called predicate's arg types.
    typecheck_info_get_module_info(!.Info, ModuleInfo),
    module_info_get_predicate_table(ModuleInfo, PredicateTable),
    SymNameArity = sym_name_arity(SymName, Arity),
    typecheck_info_get_calls_are_fully_qualified(!.Info, IsFullyQualified),
    predicate_table_lookup_pf_sym_arity(PredicateTable, IsFullyQualified,
        pf_predicate, SymName, Arity, PredIds),
    (
        PredIds = [],
        PredId = invalid_pred_id,
        typecheck_info_get_error_clause_context(!.Info, ClauseContext),
        Spec = report_pred_call_error(ClauseContext, Context, SymNameArity),
        typecheck_info_add_error(Spec, !Info)
    ;
        PredIds = [HeadPredId | TailPredIds],
        (
            TailPredIds = [],
            % Handle the case of a non-overloaded predicate specially
            % (so that we can optimize the case of a non-overloaded,
            % non-polymorphic predicate).
            PredId = HeadPredId,
            ArgVectorKind = arg_vector_plain_call_pred_id(PredId),
            typecheck_call_pred_id(ArgVectorKind, Context, GoalId,
                PredId, Args, !TypeAssignSet, !Info)
        ;
            TailPredIds = [_ | _],
            typecheck_call_overloaded_pred(SymNameArity, Context, GoalId,
                PredIds, Args, !TypeAssignSet, !Info),

            % In general, we can't figure out which predicate it is until
            % after we have resolved any overloading, which may require
            % type-checking the entire clause. Hence, for the moment, we just
            % record an invalid pred_id in the HLDS. This will be rectified
            % by modes.m during mode-checking; at that point, enough
            % information is available to determine which predicate it is.
            PredId = invalid_pred_id
        ),

        % Arguably, we could do context reduction at a different point.
        % See the paper: "Type classes: an exploration of the design space",
        % S. Peyton-Jones, M. Jones 1997, for a discussion of some of the
        % issues.
        perform_context_reduction(Context, !TypeAssignSet, !Info)
    ).

    % Typecheck a call to a specific predicate.
    %
:- pred typecheck_call_pred_id(arg_vector_kind::in, prog_context::in,
    goal_id::in, pred_id::in, list(prog_var)::in,
    type_assign_set::in, type_assign_set::out,
    typecheck_info::in, typecheck_info::out) is det.

typecheck_call_pred_id(ArgVectorKind, Context, GoalId, PredId, Args,
        !TypeAssignSet, !Info) :-
    typecheck_info_get_module_info(!.Info, ModuleInfo),
    module_info_get_predicate_table(ModuleInfo, PredicateTable),
    predicate_table_get_preds(PredicateTable, Preds),
    map.lookup(Preds, PredId, PredInfo),
    pred_info_get_arg_types(PredInfo, PredTypeVarSet, PredExistQVars,
        PredArgTypes),
    pred_info_get_class_context(PredInfo, PredClassContext),

    % Rename apart the type variables in the called predicate's arg types
    % and then unify the types of the call arguments with the called
    % predicates' arg types (optimize for the common case of a non-polymorphic,
    % non-constrained predicate).
    ( if
        varset.is_empty(PredTypeVarSet),
        PredClassContext = constraints([], [])
    then
        typecheck_vars_have_types(ArgVectorKind, Context, Args,
            PredArgTypes, !TypeAssignSet, !Info)
    else
        module_info_get_class_table(ModuleInfo, ClassTable),
        make_body_hlds_constraints(ClassTable, PredTypeVarSet,
            GoalId, PredClassContext, PredConstraints),
        typecheck_var_has_polymorphic_type_list(var_vector_args(ArgVectorKind),
            Context, Args, PredTypeVarSet, PredExistQVars,
            PredArgTypes, PredConstraints, !TypeAssignSet, !Info)
    ).

:- pred typecheck_call_overloaded_pred(sym_name_arity::in, prog_context::in,
    goal_id::in, list(pred_id)::in, list(prog_var)::in,
    type_assign_set::in, type_assign_set::out,
    typecheck_info::in, typecheck_info::out) is det.

typecheck_call_overloaded_pred(SymNameArity, Context, GoalId, PredIdList,
        Args, TypeAssignSet0, TypeAssignSet, !Info) :-
    Symbol = overloaded_pred(SymNameArity, PredIdList),
    typecheck_info_add_overloaded_symbol(Symbol, Context, !Info),

    % Let the new arg_type_assign_set be the cross-product of the current
    % type_assign_set and the set of possible lists of argument types
    % for the overloaded predicate, suitable renamed apart.
    typecheck_info_get_module_info(!.Info, ModuleInfo),
    module_info_get_class_table(ModuleInfo, ClassTable),
    module_info_get_predicate_table(ModuleInfo, PredicateTable),
    predicate_table_get_preds(PredicateTable, Preds),
    get_overloaded_pred_arg_types(PredIdList, Preds, ClassTable, GoalId,
        TypeAssignSet0, [], ArgsTypeAssignSet),

    % Then unify the types of the call arguments with the
    % called predicates' arg types.
    VarVectorKind = var_vector_args(arg_vector_plain_pred_call(SymNameArity)),
    typecheck_var_has_arg_type_list(VarVectorKind, 1, Context, Args,
        ArgsTypeAssignSet, TypeAssignSet, !Info).

:- pred get_overloaded_pred_arg_types(list(pred_id)::in, pred_table::in,
    class_table::in, goal_id::in, type_assign_set::in,
    args_type_assign_set::in, args_type_assign_set::out) is det.

get_overloaded_pred_arg_types([], _Preds, _ClassTable, _GoalId,
        _TypeAssignSet0, !ArgsTypeAssignSet).
get_overloaded_pred_arg_types([PredId | PredIds], Preds, ClassTable, GoalId,
        TypeAssignSet0, !ArgsTypeAssignSet) :-
    map.lookup(Preds, PredId, PredInfo),
    pred_info_get_arg_types(PredInfo, PredTypeVarSet, PredExistQVars,
        PredArgTypes),
    pred_info_get_class_context(PredInfo, PredClassContext),
    pred_info_get_typevarset(PredInfo, TVarSet),
    make_body_hlds_constraints(ClassTable, TVarSet, GoalId,
        PredClassContext, PredConstraints),
    rename_apart(TypeAssignSet0, PredTypeVarSet, PredExistQVars,
        PredArgTypes, PredConstraints, !ArgsTypeAssignSet),
    get_overloaded_pred_arg_types(PredIds, Preds, ClassTable, GoalId,
        TypeAssignSet0, !ArgsTypeAssignSet).

%---------------------------------------------------------------------------%

    % Rename apart the type variables in called predicate's arg types
    % separately for each type assignment, resulting in an "arg type
    % assignment set", and then for each arg type assignment in the
    % arg type assignment set, check that the argument variables have
    % the expected types.
    % A set of class constraints are also passed in, which must have the
    % types contained within renamed apart.
    %
:- pred typecheck_var_has_polymorphic_type_list(var_vector_kind::in,
    prog_context::in, list(prog_var)::in, tvarset::in,
    existq_tvars::in, list(mer_type)::in, hlds_constraints::in,
    type_assign_set::in, type_assign_set::out,
    typecheck_info::in, typecheck_info::out) is det.

typecheck_var_has_polymorphic_type_list(VarVectorKind, Context, Args,
        PredTypeVarSet, PredExistQVars, PredArgTypes, PredConstraints,
        TypeAssignSet0, TypeAssignSet, !Info) :-
    rename_apart(TypeAssignSet0, PredTypeVarSet, PredExistQVars,
        PredArgTypes, PredConstraints, [], ArgsTypeAssignSet),
    typecheck_var_has_arg_type_list(VarVectorKind, 1, Context, Args,
        ArgsTypeAssignSet, TypeAssignSet, !Info).

:- pred rename_apart(type_assign_set::in, tvarset::in, existq_tvars::in,
    list(mer_type)::in, hlds_constraints::in,
    args_type_assign_set::in, args_type_assign_set::out) is det.

rename_apart([], _, _, _, _, !ArgTypeAssigns).
rename_apart([TypeAssign0 | TypeAssigns0], PredTypeVarSet, PredExistQVars,
        PredArgTypes, PredConstraints, !ArgTypeAssigns) :-
    % Rename everything apart.
    type_assign_rename_apart(TypeAssign0, PredTypeVarSet, PredArgTypes,
        TypeAssign1, ParentArgTypes, Renaming),
    apply_variable_renaming_to_tvar_list(Renaming, PredExistQVars,
        ParentExistQVars),
    apply_variable_renaming_to_constraints(Renaming, PredConstraints,
        ParentConstraints),

    % Insert the existentially quantified type variables for the called
    % predicate into HeadTypeParams (which holds the set of type
    % variables which the caller is not allowed to bind).
    type_assign_get_external_type_params(TypeAssign1, HeadTypeParams0),
    list.append(ParentExistQVars, HeadTypeParams0, HeadTypeParams),
    type_assign_set_external_type_params(HeadTypeParams,
        TypeAssign1, TypeAssign),

    % Save the results and recurse.
    NewArgTypeAssign =
        args_type_assign(TypeAssign, ParentArgTypes, ParentConstraints),
    !:ArgTypeAssigns = [NewArgTypeAssign | !.ArgTypeAssigns],
    rename_apart(TypeAssigns0, PredTypeVarSet, PredExistQVars,
        PredArgTypes, PredConstraints, !ArgTypeAssigns).

:- pred type_assign_rename_apart(type_assign::in, tvarset::in,
    list(mer_type)::in, type_assign::out, list(mer_type)::out,
    tvar_renaming::out) is det.

type_assign_rename_apart(TypeAssign0, PredTypeVarSet, PredArgTypes,
        TypeAssign, ParentArgTypes, Renaming) :-
    type_assign_get_typevarset(TypeAssign0, TypeVarSet0),
    tvarset_merge_renaming(TypeVarSet0, PredTypeVarSet, TypeVarSet, Renaming),
    apply_variable_renaming_to_type_list(Renaming, PredArgTypes,
        ParentArgTypes),
    type_assign_set_typevarset(TypeVarSet, TypeAssign0, TypeAssign).

%---------------------------------------------------------------------------%

:- pred typecheck_var_has_arg_type_list(var_vector_kind::in, int::in,
    prog_context::in, list(prog_var)::in,
    args_type_assign_set::in, type_assign_set::out,
    typecheck_info::in, typecheck_info::out) is det.

typecheck_var_has_arg_type_list(_, _, _, [],
        ArgTypeAssignSet, TypeAssignSet, !Info) :-
    TypeAssignSet =
        convert_args_type_assign_set_check_empty_args(ArgTypeAssignSet).
typecheck_var_has_arg_type_list(VarVectorKind, ArgNum, Context, [Var | Vars],
        ArgTypeAssignSet0, TypeAssignSet, !Info) :-
    GoalContext = type_error_in_var_vector(VarVectorKind, ArgNum),
    typecheck_var_has_arg_type(GoalContext, Context, Var,
        ArgTypeAssignSet0, ArgTypeAssignSet1, !Info),
    typecheck_var_has_arg_type_list(VarVectorKind, ArgNum + 1, Context,
        Vars, ArgTypeAssignSet1, TypeAssignSet, !Info).

:- pred typecheck_var_has_arg_type(type_error_goal_context::in,
    prog_context::in, prog_var::in,
    args_type_assign_set::in, args_type_assign_set::out,
    typecheck_info::in, typecheck_info::out) is det.

typecheck_var_has_arg_type(GoalContext, Context, Var,
        ArgTypeAssignSet0, ArgTypeAssignSet, !Info) :-
    typecheck_var_has_arg_type_2(ArgTypeAssignSet0,
        Var, [], ArgTypeAssignSet1),
    ( if
        ArgTypeAssignSet1 = [],
        ArgTypeAssignSet0 = [_ | _]
    then
        delete_first_arg_in_each_arg_type_assign(ArgTypeAssignSet0,
            ArgTypeAssignSet),
        typecheck_info_get_error_clause_context(!.Info, ClauseContext),
        Spec = report_error_arg_var(!.Info, ClauseContext, GoalContext,
            Context, Var, ArgTypeAssignSet0),
        typecheck_info_add_error(Spec, !Info)
    else
        ArgTypeAssignSet = ArgTypeAssignSet1
    ).

:- pred delete_first_arg_in_each_arg_type_assign(args_type_assign_set::in,
    args_type_assign_set::out) is det.

delete_first_arg_in_each_arg_type_assign([], []).
delete_first_arg_in_each_arg_type_assign([ArgTypeAssign0 | ArgTypeAssigns0],
        [ArgTypeAssign | ArgTypeAssigns]) :-
    ArgTypeAssign0 = args_type_assign(TypeAssign, Args0, Constraints),
    (
        Args0 = [_ | Args]
    ;
        Args0 = [],
        % this should never happen
        unexpected($pred, "skip_arg")
    ),
    ArgTypeAssign = args_type_assign(TypeAssign, Args, Constraints),
    delete_first_arg_in_each_arg_type_assign(ArgTypeAssigns0, ArgTypeAssigns).

:- pred typecheck_var_has_arg_type_2(args_type_assign_set::in, prog_var::in,
    args_type_assign_set::in, args_type_assign_set::out) is det.

typecheck_var_has_arg_type_2([], _, !ArgTypeAssignSet).
typecheck_var_has_arg_type_2([ArgsTypeAssign | ArgsTypeAssignSets], Var,
        !ArgsTypeAssignSet) :-
    ArgsTypeAssign = args_type_assign(TypeAssign0, ArgTypes0, ClassContext),
    arg_type_assign_var_has_type(TypeAssign0, ArgTypes0,
        Var, ClassContext, !ArgsTypeAssignSet),
    typecheck_var_has_arg_type_2(ArgsTypeAssignSets, Var,
        !ArgsTypeAssignSet).

:- pred arg_type_assign_var_has_type(type_assign::in, list(mer_type)::in,
    prog_var::in, hlds_constraints::in,
    args_type_assign_set::in, args_type_assign_set::out) is det.

arg_type_assign_var_has_type(TypeAssign0, ArgTypes0, Var, ClassContext,
        !ArgTypeAssignSet) :-
    type_assign_get_var_types(TypeAssign0, VarTypes0),
    (
        ArgTypes0 = [Type | ArgTypes],
        search_insert_var_type(Var, Type, MaybeOldVarType,
            VarTypes0, VarTypes),
        (
            MaybeOldVarType = yes(OldVarType),
            ( if
                type_assign_unify_type(OldVarType, Type,
                    TypeAssign0, TypeAssign1)
            then
                NewTypeAssign =
                    args_type_assign(TypeAssign1, ArgTypes, ClassContext),
                !:ArgTypeAssignSet = [NewTypeAssign | !.ArgTypeAssignSet]
            else
                true
            )
        ;
            MaybeOldVarType = no,
            type_assign_set_var_types(VarTypes, TypeAssign0, TypeAssign),
            NewTypeAssign =
                args_type_assign(TypeAssign, ArgTypes, ClassContext),
            !:ArgTypeAssignSet = [NewTypeAssign | !.ArgTypeAssignSet]
        )
    ;
        ArgTypes0 = [],
        unexpected($pred, "ArgTypes0 = []")
    ).

%---------------------------------------------------------------------------%

    % Given a list of variables and a list of types, ensure that
    % each variable has the corresponding type.
    %
:- pred typecheck_vars_have_types(arg_vector_kind::in,
    prog_context::in, list(prog_var)::in, list(mer_type)::in,
    type_assign_set::in, type_assign_set::out,
    typecheck_info::in, typecheck_info::out) is det.

typecheck_vars_have_types(ArgVectorKind, Context, Vars, Types,
        !TypeAssignSet, !Info) :-
    typecheck_vars_have_types_in_arg_vector(!.Info, Context, ArgVectorKind, 1,
        Vars, Types, !TypeAssignSet, [], Specs, yes([]),
        MaybeArgVectorTypeErrors),
    ( if
        MaybeArgVectorTypeErrors = yes(ArgVectorTypeErrors),
        ArgVectorTypeErrors = [_, _ | _]
    then
        typecheck_info_get_error_clause_context(!.Info, ClauseContext),
        AllArgsSpec = report_arg_vector_type_errors(!.Info, ClauseContext,
            Context, ArgVectorKind, !.TypeAssignSet, ArgVectorTypeErrors),
        typecheck_info_add_error(AllArgsSpec, !Info)
    else
        list.foldl(typecheck_info_add_error, Specs, !Info)
    ).

:- pred typecheck_vars_have_types_in_arg_vector(typecheck_info::in,
    prog_context::in, arg_vector_kind::in, int::in,
    list(prog_var)::in, list(mer_type)::in,
    type_assign_set::in, type_assign_set::out,
    list(error_spec)::in, list(error_spec)::out,
    maybe(list(arg_vector_type_error))::in,
    maybe(list(arg_vector_type_error))::out) is det.

typecheck_vars_have_types_in_arg_vector(_, _, _, _, [], [],
        !TypeAssignSet, !Specs, !MaybeArgVectorTypeErrors).
typecheck_vars_have_types_in_arg_vector(_, _, _, _, [], [_ | _],
        !TypeAssignSet, !Specs, !MaybeArgVectorTypeErrors) :-
    unexpected($pred, "length mismatch").
typecheck_vars_have_types_in_arg_vector(_, _, _, _, [_ | _], [],
        !TypeAssignSet, !Specs, !MaybeArgVectorTypeErrors) :-
    unexpected($pred, "length mismatch").
typecheck_vars_have_types_in_arg_vector(Info, Context, ArgVectorKind, ArgNum,
        [Var | Vars], [Type | Types], !TypeAssignSet, !Specs,
        !MaybeArgVectorTypeErrors) :-
    typecheck_var_has_type_in_arg_vector(Info, Context, ArgVectorKind, ArgNum,
        Var, Type, !TypeAssignSet, !Specs, !MaybeArgVectorTypeErrors),
    typecheck_vars_have_types_in_arg_vector(Info, Context,
        ArgVectorKind, ArgNum + 1, Vars, Types, !TypeAssignSet, !Specs,
        !MaybeArgVectorTypeErrors).

:- pred typecheck_var_has_type_in_arg_vector(typecheck_info::in,
    prog_context::in, arg_vector_kind::in, int::in,
    prog_var::in, mer_type::in, type_assign_set::in, type_assign_set::out,
    list(error_spec)::in, list(error_spec)::out,
    maybe(list(arg_vector_type_error))::in,
    maybe(list(arg_vector_type_error))::out) is det.

typecheck_var_has_type_in_arg_vector(Info, Context, ArgVectorKind, ArgNum,
        Var, Type, TypeAssignSet0, TypeAssignSet, !Specs,
        !MaybeArgVectorTypeErrors) :-
    typecheck_var_has_type_2(TypeAssignSet0, Var, Type, [], TypeAssignSet1),
    ( if
        TypeAssignSet1 = [],
        TypeAssignSet0 = [_ | _]
    then
        TypeAssignSet = TypeAssignSet0,
        GoalContext =
            type_error_in_var_vector(var_vector_args(ArgVectorKind), ArgNum),
        SpecAndMaybeActualExpected = report_error_var(Info,
            GoalContext, Context, Var, Type, TypeAssignSet0),
        SpecAndMaybeActualExpected =
            spec_and_maybe_actual_expected(Spec, MaybeActualExpected),
        !:Specs = [Spec | !.Specs],
        (
            !.MaybeArgVectorTypeErrors = no
        ;
            !.MaybeArgVectorTypeErrors = yes(ArgVectorTypeErrors0),
            (
                MaybeActualExpected = no,
                !:MaybeArgVectorTypeErrors = no
            ;
                MaybeActualExpected = yes(ActualExpected),
                ArgVectorTypeError = arg_vector_type_error(ArgNum, Var,
                    ActualExpected),
                ArgVectorTypeErrors =
                    [ArgVectorTypeError | ArgVectorTypeErrors0],
                !:MaybeArgVectorTypeErrors = yes(ArgVectorTypeErrors)
            )
        )
    else
        TypeAssignSet = TypeAssignSet1
    ).

:- pred typecheck_var_has_stm_atomic_type(prog_context::in, prog_var::in,
    type_assign_set::in, type_assign_set::out,
    typecheck_info::in, typecheck_info::out) is det.

typecheck_var_has_stm_atomic_type(Context, Var, !TypeAssignSet, !Info) :-
    typecheck_var_has_type(type_error_in_atomic_inner, Context,
        Var, stm_atomic_type, !TypeAssignSet, !Info).

:- pred typecheck_var_has_type(type_error_goal_context::in, prog_context::in,
    prog_var::in, mer_type::in,
    type_assign_set::in, type_assign_set::out,
    typecheck_info::in, typecheck_info::out) is det.

typecheck_var_has_type(GoalContext, Context, Var, Type,
        TypeAssignSet0, TypeAssignSet, !Info) :-
    typecheck_var_has_type_2(TypeAssignSet0, Var, Type, [], TypeAssignSet1),
    ( if
        TypeAssignSet1 = [],
        TypeAssignSet0 = [_ | _]
    then
        TypeAssignSet = TypeAssignSet0,
        SpecAndMaybeActualExpected = report_error_var(!.Info,
            GoalContext, Context, Var, Type, TypeAssignSet0),
        SpecAndMaybeActualExpected = spec_and_maybe_actual_expected(Spec, _),
        typecheck_info_add_error(Spec, !Info)
    else
        TypeAssignSet = TypeAssignSet1
    ).

:- pred typecheck_var_has_type_2(type_assign_set::in, prog_var::in,
    mer_type::in, type_assign_set::in, type_assign_set::out) is det.

typecheck_var_has_type_2([], _, _, !TypeAssignSet).
typecheck_var_has_type_2([TypeAssign0 | TypeAssigns0], Var, Type,
        !TypeAssignSet) :-
    type_assign_var_has_type(TypeAssign0, Var, Type, !TypeAssignSet),
    typecheck_var_has_type_2(TypeAssigns0, Var, Type, !TypeAssignSet).

:- pred type_assign_var_has_type(type_assign::in, prog_var::in, mer_type::in,
    type_assign_set::in, type_assign_set::out) is det.

type_assign_var_has_type(TypeAssign0, Var, Type, !TypeAssignSet) :-
    type_assign_get_var_types(TypeAssign0, VarTypes0),
    search_insert_var_type(Var, Type, MaybeOldVarType, VarTypes0, VarTypes),
    (
        MaybeOldVarType = yes(OldVarType),
        ( if
            type_assign_unify_type(OldVarType, Type, TypeAssign0, TypeAssign1)
        then
            !:TypeAssignSet = [TypeAssign1 | !.TypeAssignSet]
        else
            !:TypeAssignSet = !.TypeAssignSet
        )
    ;
        MaybeOldVarType = no,
        type_assign_set_var_types(VarTypes, TypeAssign0, TypeAssign),
        !:TypeAssignSet = [TypeAssign | !.TypeAssignSet]
    ).

%---------------------------------------------------------------------------%

    % Type check a unification.
    % Get the type assignment set from the type info, and then just iterate
    % over all the possible type assignments.
    %
:- pred typecheck_unification(unify_context::in, prog_context::in, goal_id::in,
    prog_var::in, unify_rhs::in, unify_rhs::out,
    type_assign_set::in, type_assign_set::out,
    typecheck_info::in, typecheck_info::out) is det.

typecheck_unification(UnifyContext, Context, GoalId, X, RHS0, RHS,
        !TypeAssignSet, !Info) :-
    (
        RHS0 = rhs_var(Y),
        typecheck_unify_var_var(UnifyContext, Context, X, Y,
            !TypeAssignSet, !Info),
        RHS = RHS0
    ;
        RHS0 = rhs_functor(Functor, _ExistConstraints, Args),
        typecheck_unify_var_functor(UnifyContext, Context, X,
            Functor, Args, GoalId, !TypeAssignSet, !Info),
        perform_context_reduction(Context, !TypeAssignSet, !Info),
        RHS = RHS0
    ;
        RHS0 = rhs_lambda_goal(Purity, Groundness, PredOrFunc, EvalMethod,
            NonLocals, Vars, Modes, Det, Goal0),
        typecheck_lambda_var_has_type(UnifyContext, Context, Purity,
            PredOrFunc, EvalMethod, X, Vars, !TypeAssignSet, !Info),
        typecheck_goal(Goal0, Goal, Context, !TypeAssignSet, !Info),
        RHS = rhs_lambda_goal(Purity, Groundness, PredOrFunc, EvalMethod,
            NonLocals, Vars, Modes, Det, Goal)
    ).

:- pred typecheck_unify_var_var(unify_context::in, prog_context::in,
    prog_var::in, prog_var::in,
    type_assign_set::in, type_assign_set::out,
    typecheck_info::in, typecheck_info::out) is det.

typecheck_unify_var_var(UnifyContext, Context, X, Y,
        TypeAssignSet0, TypeAssignSet, !Info) :-
    type_assigns_unify_var_var(TypeAssignSet0, X, Y, [], TypeAssignSet1),
    ( if
        TypeAssignSet1 = [],
        TypeAssignSet0 = [_ | _]
    then
        TypeAssignSet = TypeAssignSet0,
        typecheck_info_get_error_clause_context(!.Info, ClauseContext),
        Spec = report_error_unif_var_var(!.Info, ClauseContext, UnifyContext,
            Context, X, Y, TypeAssignSet0),
        typecheck_info_add_error(Spec, !Info)
    else
        TypeAssignSet = TypeAssignSet1
    ).

:- pred cons_id_must_be_builtin_type(cons_id::in, mer_type::out, string::out)
    is semidet.

cons_id_must_be_builtin_type(ConsId, ConsType, BuiltinTypeName) :-
    (
        ConsId = int_const(_),
        BuiltinTypeName = "int",
        BuiltinType = builtin_type_int(int_type_int)
    ;
        ConsId = uint_const(_),
        BuiltinTypeName = "uint",
        BuiltinType = builtin_type_int(int_type_uint)
    ;
        ConsId = int8_const(_),
        BuiltinTypeName = "int8",
        BuiltinType = builtin_type_int(int_type_int8)
    ;
        ConsId = uint8_const(_),
        BuiltinTypeName = "uint8",
        BuiltinType = builtin_type_int(int_type_uint8)
    ;
        ConsId = int16_const(_),
        BuiltinTypeName = "int16",
        BuiltinType = builtin_type_int(int_type_int16)
    ;
        ConsId = uint16_const(_),
        BuiltinTypeName = "uint16",
        BuiltinType = builtin_type_int(int_type_uint16)
    ;
        ConsId = int32_const(_),
        BuiltinTypeName = "int32",
        BuiltinType = builtin_type_int(int_type_int32)
    ;
        ConsId = uint32_const(_),
        BuiltinTypeName = "uint32",
        BuiltinType = builtin_type_int(int_type_uint32)
    ;
        ConsId = int64_const(_),
        BuiltinTypeName = "int64",
        BuiltinType = builtin_type_int(int_type_int64)
    ;
        ConsId = uint64_const(_),
        BuiltinTypeName = "uint64",
        BuiltinType = builtin_type_int(int_type_uint64)
    ;
        ConsId = float_const(_),
        BuiltinTypeName = "float",
        BuiltinType = builtin_type_float
    ;
        ConsId = string_const(_),
        BuiltinTypeName = "string",
        BuiltinType = builtin_type_string
    ),
    ConsType = builtin_type(BuiltinType).

:- pred typecheck_unify_var_functor(unify_context::in, prog_context::in,
    prog_var::in, cons_id::in, list(prog_var)::in, goal_id::in,
    type_assign_set::in, type_assign_set::out,
    typecheck_info::in, typecheck_info::out) is det.

typecheck_unify_var_functor(UnifyContext, Context, Var, ConsId, ArgVars,
        GoalId, TypeAssignSet0, TypeAssignSet, !Info) :-
    typecheck_info_get_error_clause_context(!.Info, ClauseContext),
    ( if cons_id_must_be_builtin_type(ConsId, ConsType, BuiltinTypeName) then
        ( if ConsType = builtin_type(builtin_type_int(int_type_int)) then
            typecheck_info_add_nosuffix_integer_var(Var, !Info)
        else
            true
        ),
        list.foldl(
            type_assign_check_functor_type_builtin(ConsType, Var),
            TypeAssignSet0, [], TypeAssignSet1),
        (
            TypeAssignSet1 = [_ | _],
            TypeAssignSet = TypeAssignSet1
        ;
            TypeAssignSet1 = [],
            % If we encountered an error, continue checking with the
            % original type assign set.
            TypeAssignSet = TypeAssignSet0,
            (
                TypeAssignSet0 = []
                % The error did not originate here, so generating an error
                % message here would be misleading.
            ;
                TypeAssignSet0 = [_ | _],
                varset.init(ConsTypeVarSet),
                empty_hlds_constraints(EmptyConstraints),
                ConsDefn = cons_type_info(ConsTypeVarSet, [], ConsType, [],
                    EmptyConstraints, source_builtin_type(BuiltinTypeName)),
                ConsIdSpec = report_error_functor_type(!.Info,
                    UnifyContext, Context, Var, [ConsDefn],
                    ConsId, 0, TypeAssignSet0),
                typecheck_info_add_error(ConsIdSpec, !Info)
            )
        )
    else
        % Get the list of possible constructors that match this functor/arity.
        % If there aren't any, report an undefined constructor error.
        list.length(ArgVars, Arity),
        typecheck_info_get_ctor_list(!.Info, ConsId, Arity, GoalId,
            ConsDefns, ConsErrors),
        (
            ConsDefns = [],
            TypeAssignSet = TypeAssignSet0,
            GoalContext = type_error_in_unify(UnifyContext),
            Spec = report_error_undef_cons(ClauseContext, GoalContext,
                Context, ConsErrors, ConsId, Arity),
            typecheck_info_add_error(Spec, !Info)
        ;
            (
                ConsDefns = [_]
            ;
                ConsDefns = [_, _ | _],
                Sources = list.map(project_cons_type_info_source, ConsDefns),
                Symbol = overloaded_func(ConsId, Sources),
                typecheck_info_add_overloaded_symbol(Symbol, Context, !Info)
            ),

            % Produce the ConsTypeAssignSet, which is essentially the
            % cross-product of the ConsDefns and the TypeAssignSet0.
            get_cons_type_assigns_for_cons_defns(ConsDefns, TypeAssignSet0,
                [], ConsTypeAssignSet),
            ( if
                ConsTypeAssignSet = [],
                TypeAssignSet0 = [_ | _]
            then
                % This should never happen, since undefined ctors
                % should be caught by the check just above.
                unexpected($pred, "undefined cons?")
            else
                true
            ),

            % Check that the type of the functor matches the type of the
            % variable.
            typecheck_var_functor_types(Var, ConsTypeAssignSet,
                [], ArgsTypeAssignSet),
            ( if
                ArgsTypeAssignSet = [],
                ConsTypeAssignSet = [_ | _]
            then
                ConsIdSpec = report_error_functor_type(!.Info,
                    UnifyContext, Context, Var, ConsDefns, ConsId, Arity,
                    TypeAssignSet0),
                typecheck_info_add_error(ConsIdSpec, !Info)
            else
                true
            ),

            % Check that the type of the arguments of the functor matches
            % their expected type for this functor.
            typecheck_functor_arg_types(!.Info, ArgVars, ArgsTypeAssignSet,
                [], TypeAssignSet1),
            (
                TypeAssignSet1 = [_ | _],
                TypeAssignSet = TypeAssignSet1
            ;
                TypeAssignSet1 = [],
                % If we encountered an error, continue checking with the
                % original type assign set.
                TypeAssignSet = TypeAssignSet0,
                (
                    ArgsTypeAssignSet = []
                    % The error did not originate here, so generating an error
                    % message here would be misleading.
                ;
                    ArgsTypeAssignSet = [_ | _],
                    ArgSpec = report_error_functor_arg_types(!.Info,
                        ClauseContext, UnifyContext, Context, Var,
                        ConsDefns, ConsId, ArgVars, ArgsTypeAssignSet),
                    typecheck_info_add_error(ArgSpec, !Info)
                )
            )
        )
    ).

%---------------------%

:- type cons_type_assign
    --->    cons_type_assign(type_assign, mer_type, list(mer_type)).

:- type cons_type_assign_set == list(cons_type_assign).

    % typecheck_unify_var_functor_get_ctors_for_type_assigns(TypeAssignSet,
    %   ConsDefns, !ConsTypeAssignSet):
    %
    % Iterate over all the different possible type assignments and
    % constructor definitions.
    % For each type assignment in `TypeAssignSet', and constructor
    % definition in `ConsDefns', produce a pair
    %
    %   TypeAssign - cons_type(Type, ArgTypes)
    %
    % where `cons_type(Type, ArgTypes)' records one of the possible types
    % for the constructor in `ConsDefns', and where `TypeAssign' is the type
    % assignment renamed apart from the types of the constructors.
    %
    % This predicate iterates over the type assign sets;
    % typecheck_unify_var_functor_get_ctors iterates over the cons defns.
    %
:- pred get_cons_type_assigns_for_cons_defns(list(cons_type_info)::in,
    type_assign_set::in,
    cons_type_assign_set::in, cons_type_assign_set::out) is det.

get_cons_type_assigns_for_cons_defns([], _, !ConsTypeAssignSet).
get_cons_type_assigns_for_cons_defns([ConsDefn | ConsDefns], TypeAssigns,
        !ConsTypeAssignSet) :-
    get_cons_type_assigns_for_cons_defn(ConsDefn, TypeAssigns,
        !ConsTypeAssignSet),
    get_cons_type_assigns_for_cons_defns(ConsDefns, TypeAssigns,
        !ConsTypeAssignSet).

:- pred get_cons_type_assigns_for_cons_defn(cons_type_info::in,
    type_assign_set::in,
    cons_type_assign_set::in, cons_type_assign_set::out) is det.

get_cons_type_assigns_for_cons_defn(_, [], !ConsTypeAssignSet).
get_cons_type_assigns_for_cons_defn(ConsDefn, [TypeAssign | TypeAssigns],
        !ConsTypeAssignSet) :-
    get_cons_type_assign(ConsDefn, TypeAssign, ConsTypeAssign),
    !:ConsTypeAssignSet = [ConsTypeAssign | !.ConsTypeAssignSet],
    get_cons_type_assigns_for_cons_defn(ConsDefn, TypeAssigns,
        !ConsTypeAssignSet).

    % Given an cons_type_info, construct a type for the constructor
    % and a list of types of the arguments, suitably renamed apart
    % from the current type_assign's typevarset. Return them in a
    % cons_type_assign with the updated-for-the-renaming type_assign.
    %
:- pred get_cons_type_assign(cons_type_info::in, type_assign::in,
    cons_type_assign::out) is det.

get_cons_type_assign(ConsDefn, TypeAssign0, ConsTypeAssign) :-
    ConsDefn = cons_type_info(ConsTypeVarSet, ConsExistQVars0,
        ConsType0, ArgTypes0, ClassConstraints0, _Source),

    % Rename apart the type vars in the type of the constructor
    % and the types of its arguments.
    % (Optimize the common case of a non-polymorphic type.)
    ( if
        varset.is_empty(ConsTypeVarSet)
    then
        ConsType = ConsType0,
        ArgTypes = ArgTypes0,
        TypeAssign2 = TypeAssign0,
        ConstraintsToAdd = ClassConstraints0
    else if
        type_assign_rename_apart(TypeAssign0, ConsTypeVarSet,
            [ConsType0 | ArgTypes0], TypeAssign1, [ConsType1 | ArgTypes1],
            Renaming)
    then
        apply_variable_renaming_to_tvar_list(Renaming,
            ConsExistQVars0, ConsExistQVars),
        apply_variable_renaming_to_constraints(Renaming,
            ClassConstraints0, ConstraintsToAdd),
        type_assign_get_external_type_params(TypeAssign1, HeadTypeParams0),
        HeadTypeParams = ConsExistQVars ++ HeadTypeParams0,
        type_assign_set_external_type_params(HeadTypeParams,
            TypeAssign1, TypeAssign2),

        ConsType = ConsType1,
        ArgTypes = ArgTypes1
    else
        unexpected($pred, "type_assign_rename_apart failed")
    ),

    % Add the constraints for this functor to the current constraint set.
    % Note that there can still be (ground) constraints even if the varset
    % is empty.
    %
    % For functors which are data constructors, the fact that we don't take
    % the dual corresponds to assuming that they will be used as deconstructors
    % rather than as constructors.

    type_assign_get_typeclass_constraints(TypeAssign2, OldConstraints),
    merge_hlds_constraints(ConstraintsToAdd, OldConstraints, ClassConstraints),
    type_assign_set_typeclass_constraints(ClassConstraints,
        TypeAssign2, TypeAssign),
    ConsTypeAssign = cons_type_assign(TypeAssign, ConsType, ArgTypes).

%---------------------%

    % typecheck_functor_arg_types(ConsTypeAssignSet, Var, Args, ...):
    %
    % For each possible cons type assignment in `ConsTypeAssignSet',
    % for each possible constructor argument types,
    % check that the types of `Args' matches these types.
    %
:- pred typecheck_functor_arg_types(typecheck_info::in, list(prog_var)::in,
    args_type_assign_set::in,
    type_assign_set::in, type_assign_set::out) is det.

typecheck_functor_arg_types(_, _, [], !TypeAssignSet).
typecheck_functor_arg_types(Info, ArgVars, [ConsTypeAssign | ConsTypeAssigns],
        !TypeAssignSet) :-
    ConsTypeAssign = args_type_assign(TypeAssign, ArgTypes, _),
    type_assign_vars_have_types(Info, TypeAssign, ArgVars, ArgTypes,
        !TypeAssignSet),
    typecheck_functor_arg_types(Info, ArgVars, ConsTypeAssigns,
        !TypeAssignSet).

    % type_assign_vars_have_types(Info, TypeAssign, ArgVars, Types,
    %   TypeAssignSet0, TypeAssignSet):
    % Let TAs = { TA | TA is an extension of TypeAssign for which
    %   the types of the ArgVars unify with their respective Types },
    % list.append(TAs, TypeAssignSet0, TypeAssignSet).
    %
:- pred type_assign_vars_have_types(typecheck_info::in, type_assign::in,
    list(prog_var)::in, list(mer_type)::in,
    type_assign_set::in, type_assign_set::out) is det.

type_assign_vars_have_types(_, TypeAssign, [], [],
        TypeAssignSet, [TypeAssign | TypeAssignSet]).
type_assign_vars_have_types(_, _, [], [_ | _], _, _) :-
    unexpected($pred, "length mismatch").
type_assign_vars_have_types(_, _, [_ | _], [], _, _) :-
    unexpected($pred, "length mismatch").
type_assign_vars_have_types(Info, TypeAssign0,
        [ArgVar | ArgVars], [Type | Types], TypeAssignSet0, TypeAssignSet) :-
    type_assign_var_has_type(TypeAssign0, ArgVar, Type, [], TypeAssignSet1),
    type_assigns_vars_have_types(Info, TypeAssignSet1,
        ArgVars, Types, TypeAssignSet0, TypeAssignSet).

    % type_assigns_vars_have_types(Info, TypeAssigns, ArgVars, Types,
    %       TypeAssignSet0, TypeAssignSet):
    % Let TAs = { TA | TA is an extension of a member of TypeAssigns for which
    %   the types of the ArgVars unify with their respective Types },
    % list.append(TAs, TypeAssignSet0, TypeAssignSet).
    %
:- pred type_assigns_vars_have_types(typecheck_info::in,
    type_assign_set::in, list(prog_var)::in, list(mer_type)::in,
    type_assign_set::in, type_assign_set::out) is det.

type_assigns_vars_have_types(_, [], _, _, !TypeAssignSet).
type_assigns_vars_have_types(Info, [TypeAssign | TypeAssigns],
        ArgVars, Types, !TypeAssignSet) :-
    type_assign_vars_have_types(Info, TypeAssign, ArgVars, Types,
        !TypeAssignSet),
    type_assigns_vars_have_types(Info, TypeAssigns, ArgVars, Types,
        !TypeAssignSet).

%---------------------------------------------------------------------------%

    % Iterate type_assign_unify_var_var over all the given type assignments.
    %
:- pred type_assigns_unify_var_var(type_assign_set::in,
    prog_var::in, prog_var::in,
    type_assign_set::in, type_assign_set::out) is det.

type_assigns_unify_var_var([], _, _, !TypeAssignSet).
type_assigns_unify_var_var([TypeAssign | TypeAssigns], X, Y, !TypeAssignSet) :-
    type_assign_unify_var_var(TypeAssign, X, Y, !TypeAssignSet),
    type_assigns_unify_var_var(TypeAssigns, X, Y, !TypeAssignSet).

    % Typecheck the unification of two variables,
    % and update the type assignment.
    % TypeAssign0 is the type assignment we are updating,
    % TypeAssignSet0 is an accumulator for the list of possible
    % type assignments so far, and TypeAssignSet is TypeAssignSet plus
    % any type assignment(s) resulting from TypeAssign0 and this unification.
    %
:- pred type_assign_unify_var_var(type_assign::in, prog_var::in, prog_var::in,
    type_assign_set::in, type_assign_set::out) is det.

type_assign_unify_var_var(TypeAssign0, X, Y, !TypeAssignSet) :-
    type_assign_get_var_types(TypeAssign0, VarTypes0),
    ( if search_var_type(VarTypes0, X, TypeX) then
        search_insert_var_type(Y, TypeX, MaybeTypeY, VarTypes0, VarTypes),
        (
            MaybeTypeY = yes(TypeY),
            % Both X and Y already have types - just unify their types.
            ( if
                type_assign_unify_type(TypeX, TypeY, TypeAssign0, TypeAssign3)
            then
                !:TypeAssignSet = [TypeAssign3 | !.TypeAssignSet]
            else
                !:TypeAssignSet = !.TypeAssignSet
            )
        ;
            MaybeTypeY = no,
            type_assign_set_var_types(VarTypes, TypeAssign0, TypeAssign),
            !:TypeAssignSet = [TypeAssign | !.TypeAssignSet]
        )
    else
        ( if search_var_type(VarTypes0, Y, TypeY) then
            % X is a fresh variable which hasn't been assigned a type yet.
            add_var_type(X, TypeY, VarTypes0, VarTypes),
            type_assign_set_var_types(VarTypes, TypeAssign0, TypeAssign),
            !:TypeAssignSet = [TypeAssign | !.TypeAssignSet]
        else
            % Both X and Y are fresh variables - introduce a fresh type
            % variable with kind `star' to represent their type.
            type_assign_get_typevarset(TypeAssign0, TypeVarSet0),
            varset.new_var(TypeVar, TypeVarSet0, TypeVarSet),
            type_assign_set_typevarset(TypeVarSet, TypeAssign0, TypeAssign1),
            Type = type_variable(TypeVar, kind_star),
            add_var_type(X, Type, VarTypes0, VarTypes1),
            ( if X = Y then
                VarTypes = VarTypes1
            else
                add_var_type(Y, Type, VarTypes1, VarTypes)
            ),
            type_assign_set_var_types(VarTypes, TypeAssign1, TypeAssign),
            !:TypeAssignSet = [TypeAssign | !.TypeAssignSet]
        )
    ).

%---------------------------------------------------------------------------%

    % typecheck_var_functor_type(Var, ConsTypeAssignSet, !ArgsTypeAssignSet):
    %
    % For each possible cons type assignment in `ConsTypeAssignSet',
    % for each possible constructor type,
    % check that the type of `Var' matches this type.
    % If it does, add the type binding to !ArgsTypeAssignSet.
    %
:- pred typecheck_var_functor_types(prog_var::in, cons_type_assign_set::in,
    args_type_assign_set::in, args_type_assign_set::out) is det.

typecheck_var_functor_types(_, [], !ArgsTypeAssignSet).
typecheck_var_functor_types(Var, [ConsTypeAssign | ConsTypeAssigns],
        !ArgsTypeAssignSet) :-
    typecheck_var_functor_type(Var, ConsTypeAssign, !ArgsTypeAssignSet),
    typecheck_var_functor_types(Var, ConsTypeAssigns, !ArgsTypeAssignSet).

:- pred typecheck_var_functor_type(prog_var::in, cons_type_assign::in,
    args_type_assign_set::in, args_type_assign_set::out) is det.

typecheck_var_functor_type(Var, ConsTypeAssign0, !ArgsTypeAssignSet) :-
    ConsTypeAssign0 = cons_type_assign(TypeAssign0, ConsType, ConsArgTypes),

    % Unify the type of Var with the type of the constructor.
    type_assign_get_var_types(TypeAssign0, VarTypes0),
    search_insert_var_type(Var, ConsType, MaybeTypeVar, VarTypes0, VarTypes),
    (
        MaybeTypeVar = yes(TypeVar),
        % VarTypes wasn't updated, so don't need to update its containing
        % type assign either.
        ( if
            type_assign_unify_type(ConsType, TypeVar, TypeAssign0, TypeAssign)
        then
            % The constraints are empty here because none are added by
            % unification with a functor.
            empty_hlds_constraints(EmptyConstraints),
            ArgsTypeAssign =
                args_type_assign(TypeAssign, ConsArgTypes, EmptyConstraints),
            !:ArgsTypeAssignSet = [ArgsTypeAssign | !.ArgsTypeAssignSet]
        else
            true
        )
    ;
        MaybeTypeVar = no,
        type_assign_set_var_types(VarTypes, TypeAssign0, TypeAssign),
        % The constraints are empty here because none are added by
        % unification with a functor.
        empty_hlds_constraints(EmptyConstraints),
        ArgsTypeAssign =
            args_type_assign(TypeAssign, ConsArgTypes, EmptyConstraints),
        !:ArgsTypeAssignSet = [ArgsTypeAssign | !.ArgsTypeAssignSet]
    ).

:- pred type_assign_check_functor_type_builtin(mer_type::in,
    prog_var::in, type_assign::in,
    type_assign_set::in, type_assign_set::out) is det.

type_assign_check_functor_type_builtin(ConsType, Y, TypeAssign0,
        !TypeAssignSet) :-
    % Unify the type of Var with the type of the constructor.
    type_assign_get_var_types(TypeAssign0, VarTypes0),
    search_insert_var_type(Y, ConsType, MaybeTypeY, VarTypes0, VarTypes),
    (
        MaybeTypeY = yes(TypeY),
        ( if
            type_assign_unify_type(ConsType, TypeY, TypeAssign0, TypeAssign)
        then
            % The constraints are empty here because none are added by
            % unification with a functor.
            !:TypeAssignSet = [TypeAssign | !.TypeAssignSet]
        else
            true
        )
    ;
        MaybeTypeY = no,
        % The constraints are empty here because none are added by
        % unification with a functor.
        type_assign_set_var_types(VarTypes, TypeAssign0, TypeAssign),
        !:TypeAssignSet = [TypeAssign | !.TypeAssignSet]
    ).

%---------------------------------------------------------------------------%

    % typecheck_lambda_var_has_type(..., Var, ArgVars, !Info)
    %
    % Check that `Var' has type `pred(T1, T2, ...)' where T1, T2, ...
    % are the types of the `ArgVars'.
    %
:- pred typecheck_lambda_var_has_type(unify_context::in, prog_context::in,
    purity::in, pred_or_func::in, lambda_eval_method::in,
    prog_var::in, list(prog_var)::in,
    type_assign_set::in, type_assign_set::out,
    typecheck_info::in, typecheck_info::out) is det.

typecheck_lambda_var_has_type(UnifyContext, Context, Purity, PredOrFunc,
        EvalMethod, Var, ArgVars, TypeAssignSet0, TypeAssignSet, !Info) :-
    typecheck_lambda_var_has_type_2(TypeAssignSet0, Purity, PredOrFunc,
        EvalMethod, Var, ArgVars, [], TypeAssignSet1),
    ( if
        TypeAssignSet1 = [],
        TypeAssignSet0 = [_ | _]
    then
        TypeAssignSet = TypeAssignSet0,
        typecheck_info_get_error_clause_context(!.Info, ClauseContext),
        Spec = report_error_lambda_var(!.Info, ClauseContext, UnifyContext,
            Context, PredOrFunc, EvalMethod, Var, ArgVars, TypeAssignSet0),
        typecheck_info_add_error(Spec, !Info)
    else
        TypeAssignSet = TypeAssignSet1
    ).

:- pred typecheck_lambda_var_has_type_2(type_assign_set::in, purity::in,
    pred_or_func::in, lambda_eval_method::in, prog_var::in,
    list(prog_var)::in, type_assign_set::in, type_assign_set::out) is det.

typecheck_lambda_var_has_type_2([], _, _, _, _, _, !TypeAssignSet).
typecheck_lambda_var_has_type_2([TypeAssign0 | TypeAssignSet0], Purity,
        PredOrFunc, EvalMethod, Var, ArgVars, !TypeAssignSet) :-
    type_assign_get_types_of_vars(ArgVars, ArgVarTypes,
        TypeAssign0, TypeAssign1),
    construct_higher_order_type(Purity, PredOrFunc, EvalMethod,
        ArgVarTypes, LambdaType),
    type_assign_var_has_type(TypeAssign1, Var, LambdaType, !TypeAssignSet),
    typecheck_lambda_var_has_type_2(TypeAssignSet0,
        Purity, PredOrFunc, EvalMethod, Var, ArgVars, !TypeAssignSet).

:- pred type_assign_get_types_of_vars(list(prog_var)::in, list(mer_type)::out,
    type_assign::in, type_assign::out) is det.

type_assign_get_types_of_vars([], [], !TypeAssign).
type_assign_get_types_of_vars([Var | Vars], [Type | Types], !TypeAssign) :-
    % Check whether the variable already has a type.
    type_assign_get_var_types(!.TypeAssign, VarTypes0),
    ( if search_var_type(VarTypes0, Var, VarType) then
        % If so, use that type.
        Type = VarType
    else
        % Otherwise, introduce a fresh type variable with kind `star' to use
        % as the type of that variable.
        type_assign_fresh_type_var(Var, Type, !TypeAssign)
    ),
    % Recursively process the rest of the variables.
    type_assign_get_types_of_vars(Vars, Types, !TypeAssign).

:- pred type_assign_fresh_type_var(prog_var::in, mer_type::out,
    type_assign::in, type_assign::out) is det.

type_assign_fresh_type_var(Var, Type, !TypeAssign) :-
    type_assign_get_var_types(!.TypeAssign, VarTypes0),
    type_assign_get_typevarset(!.TypeAssign, TypeVarSet0),
    varset.new_var(TypeVar, TypeVarSet0, TypeVarSet),
    type_assign_set_typevarset(TypeVarSet, !TypeAssign),
    Type = type_variable(TypeVar, kind_star),
    add_var_type(Var, Type, VarTypes0, VarTypes1),
    type_assign_set_var_types(VarTypes1, !TypeAssign).

%---------------------------------------------------------------------------%

    % Unify (with occurs check) two types in a type assignment
    % and update the type bindings.
    %
:- pred type_assign_unify_type(mer_type::in, mer_type::in,
    type_assign::in, type_assign::out) is semidet.

type_assign_unify_type(X, Y, TypeAssign0, TypeAssign) :-
    type_assign_get_external_type_params(TypeAssign0, HeadTypeParams),
    type_assign_get_type_bindings(TypeAssign0, TypeBindings0),
    type_unify(X, Y, HeadTypeParams, TypeBindings0, TypeBindings),
    type_assign_set_type_bindings(TypeBindings, TypeAssign0, TypeAssign).

%---------------------------------------------------------------------------%

:- pred typecheck_coerce(prog_context::in, list(prog_var)::in,
    type_assign_set::in, type_assign_set::out,
    typecheck_info::in, typecheck_info::out) is det.

typecheck_coerce(Context, Args, TypeAssignSet0, TypeAssignSet, !Info) :-
    ( if Args = [FromVar0, ToVar0] then
        FromVar = FromVar0,
        ToVar = ToVar0
    else
        unexpected($pred, "coerce requires two arguments")
    ),
    list.foldl2(typecheck_coerce_2(Context, FromVar, ToVar),
        TypeAssignSet0, [], TypeAssignSet1, !Info),
    ( if
        TypeAssignSet1 = [],
        TypeAssignSet0 = [_ | _]
    then
        TypeAssignSet = TypeAssignSet0
    else
        TypeAssignSet = TypeAssignSet1
    ).

:- pred typecheck_coerce_2(prog_context::in, prog_var::in, prog_var::in,
    type_assign::in, type_assign_set::in, type_assign_set::out,
    typecheck_info::in, typecheck_info::out) is det.

typecheck_coerce_2(Context, FromVar, ToVar, TypeAssign0,
        !TypeAssignSet, !Info) :-
    type_assign_get_var_types(TypeAssign0, VarTypes),
    type_assign_get_typevarset(TypeAssign0, TVarSet),
    type_assign_get_external_type_params(TypeAssign0, ExternalTypeParams),
    type_assign_get_type_bindings(TypeAssign0, TypeBindings),

    ( if search_var_type(VarTypes, FromVar, FromType0) then
        apply_rec_subst_to_type(TypeBindings, FromType0, FromType1),
        MaybeFromType = yes(FromType1)
    else
        MaybeFromType = no
    ),
    ( if search_var_type(VarTypes, ToVar, ToType0) then
        apply_rec_subst_to_type(TypeBindings, ToType0, ToType1),
        MaybeToType = yes(ToType1)
    else
        MaybeToType = no
    ),

    ( if
        MaybeFromType = yes(FromType),
        MaybeToType = yes(ToType),
        type_is_ground_except_vars(FromType, ExternalTypeParams),
        type_is_ground_except_vars(ToType, ExternalTypeParams)
    then
        % We can compare the types on both sides immediately.
        typecheck_info_get_type_table(!.Info, TypeTable),
        ( if
            typecheck_coerce_between_types(TypeTable, TVarSet,
                FromType, ToType, TypeAssign0, TypeAssign1)
        then
            TypeAssign = TypeAssign1
        else
            type_assign_get_coerce_constraints(TypeAssign0, Coercions0),
            Coercion = coerce_constraint(FromType, ToType, Context,
                unsatisfiable),
            Coercions = [Coercion | Coercions0],
            type_assign_set_coerce_constraints(Coercions,
                TypeAssign0, TypeAssign)
        ),
        !:TypeAssignSet = [TypeAssign | !.TypeAssignSet]
    else
        % One or both of the types is not known yet. Add a coercion constraint
        % on the type assignment to be checked after typechecking the clause.
        (
            MaybeFromType = yes(FromType),
            TypeAssign1 = TypeAssign0
        ;
            MaybeFromType = no,
            type_assign_fresh_type_var(FromVar, FromType,
                TypeAssign0, TypeAssign1)
        ),
        (
            MaybeToType = yes(ToType),
            TypeAssign2 = TypeAssign1
        ;
            MaybeToType = no,
            type_assign_fresh_type_var(ToVar, ToType,
                TypeAssign1, TypeAssign2)
        ),
        type_assign_get_coerce_constraints(TypeAssign2, Coercions0),
        Coercion = coerce_constraint(FromType, ToType, Context, need_to_check),
        Coercions = [Coercion | Coercions0],
        type_assign_set_coerce_constraints(Coercions, TypeAssign2, TypeAssign),
        !:TypeAssignSet = [TypeAssign | !.TypeAssignSet]
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % builtin_atomic_type(Const, TypeName):
    %
    % If Const is *or can be* a constant of a builtin atomic type,
    % set TypeName to the name of that type, otherwise fail.
    %
:- pred builtin_atomic_type(cons_id::in, string::out) is semidet.

builtin_atomic_type(int_const(_), "int").
builtin_atomic_type(uint_const(_), "uint").
builtin_atomic_type(int8_const(_), "int8").
builtin_atomic_type(uint8_const(_), "uint8").
builtin_atomic_type(int16_const(_), "int16").
builtin_atomic_type(uint16_const(_), "uint16").
builtin_atomic_type(int32_const(_), "int32").
builtin_atomic_type(uint32_const(_), "uint32").
builtin_atomic_type(int64_const(_), "int64").
builtin_atomic_type(uint64_const(_), "uint64").
builtin_atomic_type(float_const(_), "float").
builtin_atomic_type(char_const(_), "character").
builtin_atomic_type(string_const(_), "string").
builtin_atomic_type(cons(unqualified(String), 0, _), "character") :-
    % We are before post-typecheck, so character constants have not yet been
    % converted to char_consts.
    %
    % XXX The parser should have a separate term.functor representation
    % for character constants, which should be converted to char_consts
    % during the term to item translation.
    string.char_to_string(_, String).
builtin_atomic_type(impl_defined_const(IDCKind), Type) :-
    (
        ( IDCKind = idc_file
        ; IDCKind = idc_module
        ; IDCKind = idc_pred
        ; IDCKind = idc_grade
        ),
        Type = "string"
    ;
        IDCKind = idc_line,
        Type = "int"
    ).

    % builtin_pred_type(Info, ConsId, Arity, GoalId, PredConsInfoList):
    %
    % If ConsId/Arity is a constant of a pred type, instantiates
    % the output parameters, otherwise fails.
    %
    % Instantiates PredConsInfoList to the set of cons_type_info structures
    % for each predicate with name `ConsId' and arity greater than or equal to
    % Arity. GoalId is used to identify any constraints introduced.
    %
    % For example, functor `map.search/1' has type `pred(K, V)'
    % (hence PredTypeParams = [K, V]) and argument types [map(K, V)].
    %
:- pred builtin_pred_type(typecheck_info::in, cons_id::in, int::in,
    goal_id::in, list(cons_type_info)::out) is semidet.

builtin_pred_type(Info, ConsId, Arity, GoalId, ConsTypeInfos) :-
    ConsId = cons(SymName, _, _),
    typecheck_info_get_pred_table(Info, PredicateTable),
    typecheck_info_get_calls_are_fully_qualified(Info, IsFullyQualified),
    predicate_table_lookup_sym(PredicateTable, IsFullyQualified, SymName,
        PredIds),
    (
        PredIds = [_ | _],
        predicate_table_get_preds(PredicateTable, Preds),
        accumulate_cons_type_infos_for_pred_ids(Info, Preds, GoalId,
            PredIds, Arity, [], ConsTypeInfos)
    ;
        PredIds = [],
        ConsTypeInfos = []
    ).

:- pred accumulate_cons_type_infos_for_pred_ids(typecheck_info::in,
    pred_table::in, goal_id::in, list(pred_id)::in, int::in,
    list(cons_type_info)::in, list(cons_type_info)::out) is det.

accumulate_cons_type_infos_for_pred_ids(_, _, _, [], _, !ConsTypeInfos).
accumulate_cons_type_infos_for_pred_ids(Info, PredTable, GoalId,
        [PredId | PredIds], Arity, !ConsTypeInfos) :-
    accumulate_cons_type_infos_for_pred_id(Info, PredTable, GoalId,
        PredId, Arity, !ConsTypeInfos),
    accumulate_cons_type_infos_for_pred_ids(Info, PredTable, GoalId,
        PredIds, Arity, !ConsTypeInfos).

:- pred accumulate_cons_type_infos_for_pred_id(typecheck_info::in,
    pred_table::in, goal_id::in, pred_id::in, int::in,
    list(cons_type_info)::in, list(cons_type_info)::out) is det.

accumulate_cons_type_infos_for_pred_id(Info, PredTable, GoalId,
        PredId, FuncArity, !ConsTypeInfos) :-
    typecheck_info_get_module_info(Info, ModuleInfo),
    module_info_get_class_table(ModuleInfo, ClassTable),
    map.lookup(PredTable, PredId, PredInfo),
    PredArity = pred_info_orig_arity(PredInfo),
    IsPredOrFunc = pred_info_is_pred_or_func(PredInfo),
    pred_info_get_class_context(PredInfo, PredClassContext),
    pred_info_get_arg_types(PredInfo, PredTypeVarSet, PredExistQVars,
        CompleteArgTypes),
    pred_info_get_purity(PredInfo, Purity),
    ( if
        IsPredOrFunc = pf_predicate,
        PredArity >= FuncArity,
        % We don't support first-class polymorphism, so you can't take the
        % address of an existentially quantified predicate.
        PredExistQVars = []
    then
        list.det_split_list(FuncArity, CompleteArgTypes,
            ArgTypes, PredTypeParams),
        construct_higher_order_pred_type(Purity, lambda_normal,
            PredTypeParams, PredType),
        make_body_hlds_constraints(ClassTable, PredTypeVarSet,
            GoalId, PredClassContext, PredConstraints),
        ConsTypeInfo = cons_type_info(PredTypeVarSet, PredExistQVars,
            PredType, ArgTypes, PredConstraints, source_pred(PredId)),
        !:ConsTypeInfos = [ConsTypeInfo | !.ConsTypeInfos]
    else if
        IsPredOrFunc = pf_function,
        PredAsFuncArity = PredArity - 1,
        PredAsFuncArity >= FuncArity,
        % We don't support first-class polymorphism, so you can't take
        % the address of an existentially quantified function. You can however
        % call such a function, so long as you pass *all* the parameters.
        ( PredExistQVars = []
        ; PredAsFuncArity = FuncArity
        )
    then
        list.det_split_list(FuncArity, CompleteArgTypes,
            FuncArgTypes, FuncTypeParams),
        pred_args_to_func_args(FuncTypeParams,
            FuncArgTypeParams, FuncReturnTypeParam),
        (
            FuncArgTypeParams = [],
            FuncType = FuncReturnTypeParam
        ;
            FuncArgTypeParams = [_ | _],
            construct_higher_order_func_type(Purity, lambda_normal,
                FuncArgTypeParams, FuncReturnTypeParam, FuncType)
        ),
        make_body_hlds_constraints(ClassTable, PredTypeVarSet,
            GoalId, PredClassContext, PredConstraints),
        ConsTypeInfo = cons_type_info(PredTypeVarSet,
            PredExistQVars, FuncType, FuncArgTypes, PredConstraints,
            source_pred(PredId)),
        !:ConsTypeInfos = [ConsTypeInfo | !.ConsTypeInfos]
    else
        true
    ).

    % builtin_apply_type(Info, ConsId, Arity, ConsTypeInfos):
    %
    % Succeed if ConsId is the builtin apply/N or ''/N (N>=2),
    % which is used to invoke higher-order functions.
    % If so, bind ConsTypeInfos to a singleton list containing
    % the appropriate type for apply/N of the specified Arity.
    %
:- pred builtin_apply_type(typecheck_info::in, cons_id::in, int::in,
    list(cons_type_info)::out) is semidet.

builtin_apply_type(_Info, ConsId, Arity, ConsTypeInfos) :-
    ConsId = cons(unqualified(ApplyName), _, _),
    % XXX FIXME handle impure apply/N more elegantly (e.g. nicer syntax)
    (
        ApplyName = "apply",
        ApplyNameToUse = ApplyName,
        Purity = purity_pure
    ;
        ApplyName = "",
        ApplyNameToUse = "apply",
        Purity = purity_pure
    ;
        ApplyName = "impure_apply",
        ApplyNameToUse = ApplyName,
        Purity = purity_impure
    ;
        ApplyName = "semipure_apply",
        ApplyNameToUse = ApplyName,
        Purity = purity_semipure
    ),
    Arity >= 1,
    Arity1 = Arity - 1,
    higher_order_func_type(Purity, Arity1, lambda_normal, TypeVarSet, FuncType,
        ArgTypes, RetType),
    ExistQVars = [],
    empty_hlds_constraints(EmptyConstraints),
    ConsTypeInfos = [cons_type_info(TypeVarSet, ExistQVars, RetType,
        [FuncType | ArgTypes], EmptyConstraints,
        source_apply(ApplyNameToUse))].

    % builtin_field_access_function_type(Info, GoalId, ConsId,
    %   Arity, ConsTypeInfos):
    %
    % Succeed if ConsId is the name of one the automatically
    % generated field access functions (fieldname, '<fieldname> :=').
    %
:- pred builtin_field_access_function_type(typecheck_info::in, goal_id::in,
    cons_id::in, arity::in, list(maybe_cons_type_info)::out) is semidet.

builtin_field_access_function_type(Info, GoalId, ConsId, Arity,
        MaybeConsTypeInfos) :-
    % Taking the address of automatically generated field access functions
    % is not allowed, so currying does have to be considered here.
    ConsId = cons(Name, Arity, _),
    typecheck_info_get_module_info(Info, ModuleInfo),
    is_field_access_function_name(ModuleInfo, Name, Arity, AccessType,
        FieldName),

    module_info_get_ctor_field_table(ModuleInfo, CtorFieldTable),
    map.search(CtorFieldTable, FieldName, FieldDefns),

    list.filter_map(
        make_field_access_function_cons_type_info(Info, GoalId, Name,
            Arity, AccessType, FieldName),
        FieldDefns, MaybeConsTypeInfos).

:- pred make_field_access_function_cons_type_info(typecheck_info::in,
    goal_id::in, sym_name::in, arity::in, field_access_type::in,
    sym_name::in, hlds_ctor_field_defn::in,
    maybe_cons_type_info::out) is semidet.

make_field_access_function_cons_type_info(Info, GoalId, FuncName, Arity,
        AccessType, FieldName, FieldDefn, ConsTypeInfo) :-
    get_field_access_constructor(Info, GoalId, FuncName, Arity,
        AccessType, FieldDefn, OrigExistTVars,
        MaybeFunctorConsTypeInfo),
    (
        MaybeFunctorConsTypeInfo = ok(FunctorConsTypeInfo),
        typecheck_info_get_module_info(Info, ModuleInfo),
        module_info_get_class_table(ModuleInfo, ClassTable),
        convert_field_access_cons_type_info(ClassTable, AccessType,
            FieldName, FieldDefn, FunctorConsTypeInfo,
            OrigExistTVars, ConsTypeInfo)
    ;
        MaybeFunctorConsTypeInfo = error(_),
        ConsTypeInfo = MaybeFunctorConsTypeInfo
    ).

:- pred get_field_access_constructor(typecheck_info::in, goal_id::in,
    sym_name::in, arity::in, field_access_type::in, hlds_ctor_field_defn::in,
    existq_tvars::out, maybe_cons_type_info::out) is semidet.

get_field_access_constructor(Info, GoalId, FuncName, Arity, AccessType,
        FieldDefn, OrigExistTVars, FunctorConsTypeInfo) :-
    FieldDefn = hlds_ctor_field_defn(_, _, TypeCtor, ConsId, _),
    TypeCtor = type_ctor(qualified(TypeModule, _), _),

    % If the user has supplied a declaration, we use that instead
    % of the automatically generated version, unless we are typechecking
    % the clause introduced for the user-supplied declaration.
    % The user-declared version will be picked up by builtin_pred_type.

    typecheck_info_get_module_info(Info, ModuleInfo),
    module_info_get_predicate_table(ModuleInfo, PredTable),
    UnqualFuncName = unqualify_name(FuncName),
    typecheck_info_get_is_field_access_function(Info, IsFieldAccessFunc),
    (
        IsFieldAccessFunc = no,
        predicate_table_lookup_func_m_n_a(PredTable, is_fully_qualified,
            TypeModule, UnqualFuncName, Arity, PredIds),
        PredIds = []
    ;
        IsFieldAccessFunc = yes(_)
    ),
    module_info_get_cons_table(ModuleInfo, ConsTable),
    lookup_cons_table_of_type_ctor(ConsTable, TypeCtor, ConsId, ConsDefn),
    MaybeExistConstraints = ConsDefn ^ cons_maybe_exist,
    (
        MaybeExistConstraints = no_exist_constraints,
        OrigExistTVars = []
    ;
        MaybeExistConstraints = exist_constraints(ExistConstraints),
        ExistConstraints = cons_exist_constraints(OrigExistTVars, _, _, _)
    ),
    (
        AccessType = get,
        ConsAction = do_not_flip_constraints,
        convert_cons_defn(Info, GoalId, ConsAction, ConsDefn,
            FunctorConsTypeInfo)
    ;
        AccessType = set,
        ConsAction = flip_constraints_for_field_set,
        convert_cons_defn(Info, GoalId, ConsAction, ConsDefn,
            FunctorConsTypeInfo)
    ).

:- type maybe_cons_type_info
    --->    ok(cons_type_info)
    ;       error(cons_error).

:- pred convert_field_access_cons_type_info(class_table::in,
    field_access_type::in, sym_name::in, hlds_ctor_field_defn::in,
    cons_type_info::in, existq_tvars::in, maybe_cons_type_info::out) is det.

convert_field_access_cons_type_info(ClassTable, AccessType, FieldName,
        FieldDefn, FunctorConsTypeInfo, OrigExistTVars, ConsTypeInfo) :-
    FunctorConsTypeInfo = cons_type_info(TVarSet0, ExistQVars,
        FunctorType, ConsArgTypes, Constraints0, Source0),
    (
        Source0 = source_type(SourceType)
    ;
        ( Source0 = source_builtin_type(_)
        ; Source0 = source_get_field_access(_)
        ; Source0 = source_set_field_access(_)
        ; Source0 = source_apply(_)
        ; Source0 = source_pred(_)
        ),
        unexpected($pred, "not type")
    ),
    FieldDefn = hlds_ctor_field_defn(_, _, _, _, FieldNumber),
    list.det_index1(ConsArgTypes, FieldNumber, FieldType),
    (
        AccessType = get,
        Source = source_get_field_access(SourceType),
        RetType = FieldType,
        ArgTypes = [FunctorType],
        ConsTypeInfo = ok(cons_type_info(TVarSet0, ExistQVars,
            RetType, ArgTypes, Constraints0, Source))
    ;
        AccessType = set,
        Source = source_set_field_access(SourceType),

        % When setting a polymorphic field, the type of the field in the result
        % is not necessarily the same as in the input. If a type variable
        % occurs only in the field being set, create a new type variable for it
        % in the result type.
        %
        % This allows code such as
        % :- type pair(T, U)
        %   ---> '-'(fst::T, snd::U).
        %
        %   Pair0 = 1 - 'a',
        %   Pair = Pair0 ^ snd := 2.

        type_vars(FieldType, TVarsInField),
        % Most of the time, TVarsInField is [], so provide a fast path
        % for this case.
        (
            TVarsInField = [],
            RetType = FunctorType,
            ArgTypes = [FunctorType, FieldType],
            % None of the constraints are affected by the updated field,
            % so the constraints are unchanged.
            ConsTypeInfo = ok(cons_type_info(TVarSet0, ExistQVars,
                RetType, ArgTypes, Constraints0, Source))
        ;
            TVarsInField = [_ | _],

            % XXX This demonstrates a problem - if a type variable occurs
            % in the types of multiple fields, any predicates changing values
            % of one of these fields cannot change their types. This especially
            % a problem for existentially typed fields, because setting the
            % field always changes the type.
            %
            % Haskell gets around this problem by allowing multiple fields
            % to be set by the same expression. Haskell doesn't handle all
            % cases -- it is not possible to get multiple existentially typed
            % fields using record syntax and pass them to a function whose type
            % requires that the fields are of the same type. It probably won't
            % come up too often.
            %
            list.det_replace_nth(ConsArgTypes, FieldNumber, int_type,
                ArgTypesWithoutField),
            type_vars_list(ArgTypesWithoutField, TVarsInOtherArgs),
            set.intersect(
                set.list_to_set(TVarsInField),
                set.intersect(
                    set.list_to_set(TVarsInOtherArgs),
                    set.list_to_set(OrigExistTVars)
                ),
                ExistQVarsInFieldAndOthers),
            ( if set.is_empty(ExistQVarsInFieldAndOthers) then
                % Rename apart type variables occurring only in the field
                % to be replaced - the values of those type variables will be
                % supplied by the replacement field value.
                list.delete_elems(TVarsInField,
                    TVarsInOtherArgs, TVarsOnlyInField0),
                list.sort_and_remove_dups(TVarsOnlyInField0, TVarsOnlyInField),
                list.length(TVarsOnlyInField, NumNewTVars),
                varset.new_vars(NumNewTVars, NewTVars, TVarSet0, TVarSet),
                map.from_corresponding_lists(TVarsOnlyInField,
                    NewTVars, TVarRenaming),
                apply_variable_renaming_to_type(TVarRenaming, FieldType,
                    RenamedFieldType),
                apply_variable_renaming_to_type(TVarRenaming, FunctorType,
                    OutputFunctorType),
                % Rename the class constraints, projecting the constraints
                % onto the set of type variables occurring in the types of the
                % arguments of the call to `'field :='/2'. Note that we have
                % already flipped the constraints.
                type_vars_list([FunctorType, FieldType], CallTVars0),
                set.list_to_set(CallTVars0, CallTVars),
                project_and_rename_constraints(ClassTable, TVarSet, CallTVars,
                    TVarRenaming, Constraints0, Constraints),
                RetType = OutputFunctorType,
                ArgTypes = [FunctorType, RenamedFieldType],
                ConsTypeInfo = ok(cons_type_info(TVarSet, ExistQVars,
                    RetType, ArgTypes, Constraints, Source))
            else
                % This field cannot be set. Pass out some information so that
                % we can give a better error message. Errors involving changing
                % the types of universally quantified type variables will be
                % caught by typecheck_functor_arg_types.
                set.to_sorted_list(ExistQVarsInFieldAndOthers,
                    ExistQVarsInFieldAndOthers1),
                ConsTypeInfo = error(invalid_field_update(FieldName,
                    FieldDefn, TVarSet0, ExistQVarsInFieldAndOthers1))
            )
        )
    ).

    % Add new universal constraints for constraints containing variables that
    % have been renamed. These new constraints are the ones that will need
    % to be supplied by the caller. The other constraints will be supplied
    % from non-updated fields.
    %
:- pred project_and_rename_constraints(class_table::in, tvarset::in,
    set(tvar)::in, tvar_renaming::in,
    hlds_constraints::in, hlds_constraints::out) is det.

project_and_rename_constraints(ClassTable, TVarSet, CallTVars, TVarRenaming,
        !Constraints) :-
    !.Constraints = hlds_constraints(Unproven0, Assumed,
        Redundant0, Ancestors),

    % Project the constraints down onto the list of tvars in the call.
    list.filter(project_constraint(CallTVars), Unproven0, NewUnproven0),
    list.filter_map(rename_constraint(TVarRenaming), NewUnproven0,
        NewUnproven),
    update_redundant_constraints(ClassTable, TVarSet, NewUnproven,
        Redundant0, Redundant),
    list.append(NewUnproven, Unproven0, Unproven),
    !:Constraints = hlds_constraints(Unproven, Assumed, Redundant, Ancestors).

:- pred project_constraint(set(tvar)::in, hlds_constraint::in) is semidet.

project_constraint(CallTVars, Constraint) :-
    Constraint = hlds_constraint(_Ids, _ClassName, TypesToCheck),
    type_vars_list(TypesToCheck, TVarsToCheck0),
    set.list_to_set(TVarsToCheck0, TVarsToCheck),
    set.intersect(TVarsToCheck, CallTVars, RelevantTVars),
    set.is_non_empty(RelevantTVars).

:- pred rename_constraint(tvar_renaming::in, hlds_constraint::in,
    hlds_constraint::out) is semidet.

rename_constraint(TVarRenaming, Constraint0, Constraint) :-
    Constraint0 = hlds_constraint(Ids, ClassName, ArgTypes0),
    some [Var] (
        type_list_contains_var(ArgTypes0, Var),
        map.contains(TVarRenaming, Var)
    ),
    apply_variable_renaming_to_type_list(TVarRenaming, ArgTypes0, ArgTypes),
    Constraint = hlds_constraint(Ids, ClassName, ArgTypes).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % Note: changes here may require changes to
    % post_typecheck.resolve_unify_functor,
    % intermod.module_qualify_unify_rhs,
    % recompilation.usage.find_matching_constructors
    % and recompilation.check.check_functor_ambiguities.
    %
:- pred typecheck_info_get_ctor_list(typecheck_info::in, cons_id::in, int::in,
    goal_id::in, list(cons_type_info)::out, list(cons_error)::out) is det.

typecheck_info_get_ctor_list(Info, Functor, Arity, GoalId, ConsInfos,
        ConsErrors) :-
    typecheck_info_get_is_field_access_function(Info, IsFieldAccessFunc),
    ( if
        % If we are typechecking the clause added for a field access function
        % for which the user has supplied type or mode declarations, the goal
        % should only contain an application of the field access function,
        % not constructor applications or function calls. The clauses in
        % `.opt' files will already have been expanded into unifications.
        IsFieldAccessFunc = yes(PredStatus),
        PredStatus \= pred_status(status_opt_imported)
    then
        ( if
            builtin_field_access_function_type(Info, GoalId,
                Functor, Arity, FieldAccessConsInfos)
        then
            split_cons_errors(FieldAccessConsInfos, ConsInfos, ConsErrors)
        else
            ConsInfos = [],
            ConsErrors = []
        )
    else
        typecheck_info_get_ctor_list_2(Info, Functor, Arity, GoalId,
            ConsInfos, ConsErrors)
    ).

:- pred typecheck_info_get_ctor_list_2(typecheck_info::in, cons_id::in,
    int::in, goal_id::in, list(cons_type_info)::out, list(cons_error)::out)
    is det.

typecheck_info_get_ctor_list_2(Info, Functor, Arity, GoalId, ConsInfos,
        DataConsErrors) :-
    empty_hlds_constraints(EmptyConstraints),

    % Check if `Functor/Arity' has been defined as a constructor in some
    % discriminated union type(s). This gives us a list of possible
    % cons_type_infos.
    typecheck_info_get_cons_table(Info, ConsTable),
    ( if
        Functor = cons(_, _, _),
        search_cons_table(ConsTable, Functor, HLDS_ConsDefns)
    then
        convert_cons_defn_list(Info, GoalId, do_not_flip_constraints,
            HLDS_ConsDefns, PlainMaybeConsInfos)
    else
        PlainMaybeConsInfos = []
    ),

    % For "existentially typed" functors, whether the functor is actually
    % existentially typed depends on whether it is used as a constructor
    % or as a deconstructor. As a constructor, it is universally typed,
    % but as a deconstructor, it is existentially typed. But type checking
    % and polymorphism need to know whether it is universally or existentially
    % quantified _before_ mode analysis has inferred the mode of the
    % unification. Therefore, we use a special syntax for construction
    % unifications with existentially quantified functors: instead of
    % just using the functor name (e.g. "Y = foo(X)", the programmer must use
    % the special functor name "new foo" (e.g. "Y = 'new foo'(X)").
    %
    % Here we check for occurrences of functor names starting with "new ".
    % For these, we look up the original functor in the constructor symbol
    % table, and for any occurrences of that functor we flip the quantifiers on
    % the type definition (i.e. convert the existential quantifiers and
    % constraints into universal ones).
    ( if
        Functor = cons(Name, Arity, FunctorTypeCtor),
        remove_new_prefix(Name, OrigName),
        OrigFunctor = cons(OrigName, Arity, FunctorTypeCtor),
        search_cons_table(ConsTable, OrigFunctor, HLDS_ExistQConsDefns)
    then
        convert_cons_defn_list(Info, GoalId, flip_constraints_for_new,
            HLDS_ExistQConsDefns, UnivQuantifiedMaybeConsInfos)
    else
        UnivQuantifiedMaybeConsInfos = []
    ),

    % Check if Functor is a field access function for which the user
    % has not supplied a declaration.
    ( if
        builtin_field_access_function_type(Info, GoalId, Functor,
            Arity, FieldAccessMaybeConsInfosPrime)
    then
        FieldAccessMaybeConsInfos = FieldAccessMaybeConsInfosPrime
    else
        FieldAccessMaybeConsInfos = []
    ),

    DataMaybeConsInfos = PlainMaybeConsInfos ++ UnivQuantifiedMaybeConsInfos
        ++ FieldAccessMaybeConsInfos,
    split_cons_errors(DataMaybeConsInfos, DataConsInfos, DataConsErrors),

    % Check if Functor is a constant of one of the builtin atomic types
    % (string, float, int, character). If so, insert the resulting
    % cons_type_info at the start of the list.
    ( if
        Arity = 0,
        builtin_atomic_type(Functor, BuiltInTypeName)
    then
        TypeCtor = type_ctor(unqualified(BuiltInTypeName), 0),
        construct_type(TypeCtor, [], ConsType),
        varset.init(ConsTypeVarSet),
        ConsInfo = cons_type_info(ConsTypeVarSet, [], ConsType, [],
            EmptyConstraints, source_builtin_type(BuiltInTypeName)),
        BuiltinConsInfos = [ConsInfo]
    else
        BuiltinConsInfos = []
    ),

    % Check if Functor is a tuple constructor.
    ( if
        ( Functor = cons(unqualified("{}"), TupleArity, _)
        ; Functor = tuple_cons(TupleArity)
        )
    then
        % Make some fresh type variables for the argument types. These have
        % kind `star' since there are values (namely the arguments of the
        % tuple constructor) which have these types.

        varset.init(TupleConsTypeVarSet0),
        varset.new_vars(TupleArity, TupleArgTVars,
            TupleConsTypeVarSet0, TupleConsTypeVarSet),
        prog_type.var_list_to_type_list(map.init, TupleArgTVars,
            TupleArgTypes),

        TupleTypeCtor = type_ctor(unqualified("{}"), TupleArity),
        construct_type(TupleTypeCtor, TupleArgTypes, TupleConsType),

        % Tuples can't have existentially typed arguments.
        TupleExistQVars = [],
        TupleConsInfo = cons_type_info(TupleConsTypeVarSet, TupleExistQVars,
            TupleConsType, TupleArgTypes, EmptyConstraints,
            source_builtin_type("tuple")),
        TupleConsInfos = [TupleConsInfo]
    else
        TupleConsInfos = []
    ),

    % Check if Functor is the name of a predicate which takes at least
    % Arity arguments. If so, insert the resulting cons_type_info
    % at the start of the list.
    ( if
        builtin_pred_type(Info, Functor, Arity, GoalId, PredConsInfosPrime)
    then
        PredConsInfos = PredConsInfosPrime
    else
        PredConsInfos = []
    ),

    % Check for higher-order function calls.
    ( if
        builtin_apply_type(Info, Functor, Arity, ApplyConsInfosPrime)
    then
        ApplyConsInfos = ApplyConsInfosPrime
    else
        ApplyConsInfos = []
    ),

    OtherConsInfos = BuiltinConsInfos ++ TupleConsInfos
        ++ PredConsInfos ++ ApplyConsInfos,
    ConsInfos = DataConsInfos ++ OtherConsInfos.

    % Filter out the errors (they aren't actually reported as errors
    % unless there was no other matching constructor).
    %
:- pred split_cons_errors(list(maybe_cons_type_info)::in,
    list(cons_type_info)::out, list(cons_error)::out) is det.

split_cons_errors([], [], []).
split_cons_errors([MaybeConsInfo | MaybeConsInfos], Infos, Errors) :-
    split_cons_errors(MaybeConsInfos, InfosTail, ErrorsTail),
    (
        MaybeConsInfo = ok(ConsInfo),
        Infos = [ConsInfo | InfosTail],
        Errors = ErrorsTail
    ;
        MaybeConsInfo = error(ConsError),
        Infos = InfosTail,
        Errors = [ConsError | ErrorsTail]
    ).

%---------------------------------------------------------------------------%

:- type cons_constraints_action
    --->    flip_constraints_for_new
    ;       flip_constraints_for_field_set
    ;       do_not_flip_constraints.

:- pred convert_cons_defn_list(typecheck_info::in, goal_id::in,
    cons_constraints_action::in, list(hlds_cons_defn)::in,
    list(maybe_cons_type_info)::out) is det.

convert_cons_defn_list(_Info, _GoalId, _Action, [], []).
convert_cons_defn_list(Info, GoalId, Action, [X | Xs], [Y | Ys]) :-
    convert_cons_defn(Info, GoalId, Action, X, Y),
    convert_cons_defn_list(Info, GoalId, Action, Xs, Ys).

:- pred convert_cons_defn(typecheck_info, goal_id,
    cons_constraints_action, hlds_cons_defn, maybe_cons_type_info).
:- mode convert_cons_defn(in, in, in(bound(do_not_flip_constraints)), in, out)
    is det.
:- mode convert_cons_defn(in, in, in, in, out) is det.

convert_cons_defn(Info, GoalId, Action, HLDS_ConsDefn, ConsTypeInfo) :-
    % XXX We should investigate whether the job done by this predicate
    % on demand and therefore possibly lots of times for the same type,
    % would be better done just once, either by invoking it (at least with
    % Action = do_not_flip_constraints) before type checking even starts and
    % recording the result, or by putting the result into the HLDS_ConsDefn
    % or some related data structure.

    HLDS_ConsDefn = hlds_cons_defn(TypeCtor, ConsTypeVarSet, ConsTypeParams,
        ConsTypeKinds, MaybeExistConstraints, Args, _),
    ArgTypes = list.map(func(C) = C ^ arg_type, Args),
    typecheck_info_get_type_table(Info, TypeTable),
    lookup_type_ctor_defn(TypeTable, TypeCtor, TypeDefn),
    hlds_data.get_type_defn_body(TypeDefn, Body),

    % If this type has `:- pragma foreign_type' declarations, we can only use
    % its constructors in predicates which have foreign clauses and in the
    % unification and comparison predicates for the type (otherwise the code
    % wouldn't compile when using a back-end which caused another version
    % of the type to be selected). The constructors may also appear in the
    % automatically generated unification and comparison predicates.
    %
    % XXX This check isn't quite right -- we really need to check for
    % each procedure that there is a foreign_proc declaration for all
    % languages for which this type has a foreign_type declaration, but
    % this will do for now. Such a check may be difficult because by
    % this point we have thrown away the clauses which we are not using
    % in the current compilation.
    %
    % The `.opt' files don't contain the foreign clauses from the source
    % file that are not used when compiling in the current grade, so we
    % allow foreign type constructors in `opt_imported' predicates even
    % if there are no foreign clauses. Errors will be caught when creating
    % the `.opt' file.

    typecheck_info_get_pred_id(Info, PredId),
    typecheck_info_get_module_info(Info, ModuleInfo),
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_get_status(PredInfo, PredStatus),
    ( if
        Body ^ du_type_is_foreign_type = yes(_),
        not pred_info_get_goal_type(PredInfo, goal_type_clause_and_foreign),
        not is_unify_index_or_compare_pred(PredInfo),
        PredStatus \= pred_status(status_opt_imported)
    then
        ConsTypeInfo = error(foreign_type_constructor(TypeCtor, TypeDefn))
    else if
        % Do not allow constructors for abstract_imported types unless
        % the current predicate is opt_imported.
        hlds_data.get_type_defn_status(TypeDefn, TypeStatus),
        TypeStatus = type_status(status_abstract_imported),
        not is_unify_index_or_compare_pred(PredInfo),
        PredStatus \= pred_status(status_opt_imported)
    then
        ConsTypeInfo = error(abstract_imported_type)
    else if
        Action = flip_constraints_for_new,
        MaybeExistConstraints = no_exist_constraints
    then
        % Do not allow 'new' constructors except on existential types.
        ConsTypeInfo = error(new_on_non_existential_type(TypeCtor))
    else
        prog_type.var_list_to_type_list(ConsTypeKinds, ConsTypeParams,
            ConsTypeArgs),
        construct_type(TypeCtor, ConsTypeArgs, ConsType),
        UnivProgConstraints = [],
        (
            MaybeExistConstraints = no_exist_constraints,
            ExistQVars0 = [],
            ExistProgConstraints = []
        ;
            MaybeExistConstraints = exist_constraints(ExistConstraints),
            ExistConstraints = cons_exist_constraints(ExistQVars0,
                ExistProgConstraints, _, _)
        ),
        (
            Action = do_not_flip_constraints,
            ProgConstraints = constraints(UnivProgConstraints,
                ExistProgConstraints),
            ExistQVars = ExistQVars0
        ;
            Action = flip_constraints_for_new,
            % Make the existential constraints into universal ones, and discard
            % the existentially quantified variables (since they are now
            % universally quantified).
            ProgConstraints = constraints(ExistProgConstraints,
                UnivProgConstraints),
            ExistQVars = []
        ;
            Action = flip_constraints_for_field_set,
            % The constraints are existential for the deconstruction, and
            % universal for the construction. Even though all of the unproven
            % constraints here can be trivially reduced by the assumed ones,
            % we still need to process them so that the appropriate tables
            % get updated.
            ProgConstraints = constraints(ExistProgConstraints,
                ExistProgConstraints),
            ExistQVars = ExistQVars0
        ),
        module_info_get_class_table(ModuleInfo, ClassTable),
        make_body_hlds_constraints(ClassTable, ConsTypeVarSet,
            GoalId, ProgConstraints, Constraints),
        ConsTypeInfo = ok(cons_type_info(ConsTypeVarSet, ExistQVars,
            ConsType, ArgTypes, Constraints, source_type(TypeCtor)))
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- pred typecheck_coerce_between_types(type_table::in, tvarset::in,
    mer_type::in, mer_type::in, type_assign::in, type_assign::out)
    is semidet.

typecheck_coerce_between_types(TypeTable, TVarSet, FromType, ToType,
        !TypeAssign) :-
    % Type bindings must have been aplpied to FromType and ToType already.
    replace_principal_type_ctor_with_base(TypeTable, TVarSet,
        FromType, FromBaseType),
    replace_principal_type_ctor_with_base(TypeTable, TVarSet,
        ToType, ToBaseType),
    type_to_ctor_and_args(FromBaseType, FromBaseTypeCtor, FromBaseTypeArgs),
    type_to_ctor_and_args(ToBaseType, ToBaseTypeCtor, ToBaseTypeArgs),

    % The input type and result type must share a base type constructor.
    BaseTypeCtor = FromBaseTypeCtor,
    BaseTypeCtor = ToBaseTypeCtor,

    % Check the variance of type arguments.
    hlds_data.search_type_ctor_defn(TypeTable, BaseTypeCtor, BaseTypeDefn),
    hlds_data.get_type_defn_tparams(BaseTypeDefn, BaseTypeParams),
    build_type_param_variance_restrictions(TypeTable, BaseTypeCtor,
        InvariantSet),
    check_coerce_type_params(TypeTable, TVarSet, InvariantSet,
        BaseTypeParams, FromBaseTypeArgs, ToBaseTypeArgs, !TypeAssign).

:- pred replace_principal_type_ctor_with_base(type_table::in, tvarset::in,
    mer_type::in, mer_type::out) is det.

replace_principal_type_ctor_with_base(TypeTable, TVarSet, Type0, Type) :-
    ( if
        type_to_ctor_and_args(Type0, TypeCtor, Args),
        get_supertype(TypeTable, TVarSet, TypeCtor, Args, SuperType)
    then
        replace_principal_type_ctor_with_base(TypeTable, TVarSet,
            SuperType, Type)
    else
        Type = Type0
    ).

:- pred get_supertype(type_table::in, tvarset::in, type_ctor::in,
    list(mer_type)::in, mer_type::out) is semidet.

get_supertype(TypeTable, TVarSet, TypeCtor, Args, SuperType) :-
    hlds_data.search_type_ctor_defn(TypeTable, TypeCtor, TypeDefn),
    hlds_data.get_type_defn_body(TypeDefn, TypeBody),
    TypeBody = hlds_du_type(_, yes(SuperType0), _, _, _),
    require_det (
        % Create substitution from type parameters to Args.
        hlds_data.get_type_defn_tvarset(TypeDefn, TVarSet0),
        hlds_data.get_type_defn_tparams(TypeDefn, TypeParams0),
        tvarset_merge_renaming(TVarSet, TVarSet0, _NewTVarSet, Renaming),
        apply_variable_renaming_to_tvar_list(Renaming,
            TypeParams0, TypeParams),
        map.from_corresponding_lists(TypeParams, Args, TSubst),

        % Apply substitution to the declared supertype.
        apply_variable_renaming_to_type(Renaming, SuperType0, SuperType1),
        apply_rec_subst_to_type(TSubst, SuperType1, SuperType)
    ).

%---------------------%

:- type invariant_set == set(tvar).

:- pred build_type_param_variance_restrictions(type_table::in,
    type_ctor::in, invariant_set::out) is det.

build_type_param_variance_restrictions(TypeTable, TypeCtor, InvariantSet) :-
    ( if
        hlds_data.search_type_ctor_defn(TypeTable, TypeCtor, TypeDefn),
        hlds_data.get_type_defn_tparams(TypeDefn, TypeParams),
        hlds_data.get_type_defn_body(TypeDefn, TypeBody),
        TypeBody = hlds_du_type(OoMCtors, _MaybeSuperType, _MaybeCanonical,
            _MaybeTypeRepn, _IsForeignType)
    then
        Ctors = one_or_more_to_list(OoMCtors),
        list.foldl(
            build_type_param_variance_restrictions_in_ctor(TypeTable,
                TypeCtor, TypeParams),
            Ctors, set.init, InvariantSet)
    else
        unexpected($pred, "not du type")
    ).

:- pred build_type_param_variance_restrictions_in_ctor(type_table::in,
    type_ctor::in, list(tvar)::in, constructor::in,
    invariant_set::in, invariant_set::out) is det.

build_type_param_variance_restrictions_in_ctor(TypeTable, CurTypeCtor,
        CurTypeParams, Ctor, !InvariantSet) :-
    Ctor = ctor(_Ordinal, _MaybeExistConstraints, _CtorName, CtorArgs, _Arity,
        _Context),
    list.foldl(
        build_type_param_variance_restrictions_in_ctor_arg(TypeTable,
            CurTypeCtor, CurTypeParams),
        CtorArgs, !InvariantSet).

:- pred build_type_param_variance_restrictions_in_ctor_arg(type_table::in,
    type_ctor::in, list(tvar)::in, constructor_arg::in,
    invariant_set::in, invariant_set::out) is det.

build_type_param_variance_restrictions_in_ctor_arg(TypeTable, CurTypeCtor,
        CurTypeParams, CtorArg, !InvariantSet) :-
    CtorArg = ctor_arg(_MaybeFieldName, CtorArgType, _Context),
    build_type_param_variance_restrictions_in_ctor_arg_type(TypeTable,
        CurTypeCtor, CurTypeParams, CtorArgType, !InvariantSet).

:- pred build_type_param_variance_restrictions_in_ctor_arg_type(type_table::in,
    type_ctor::in, list(tvar)::in, mer_type::in,
    invariant_set::in, invariant_set::out) is det.

build_type_param_variance_restrictions_in_ctor_arg_type(TypeTable, CurTypeCtor,
        CurTypeParams, CtorArgType, !InvariantSet) :-
    (
        CtorArgType = builtin_type(_)
    ;
        CtorArgType = type_variable(_TypeVar, _Kind)
    ;
        CtorArgType = defined_type(_SymName, ArgTypes, _Kind),
        ( if
            type_to_ctor_and_args(CtorArgType, TypeCtor, TypeArgs),
            hlds_data.search_type_ctor_defn(TypeTable, TypeCtor, TypeDefn)
        then
            hlds_data.get_type_defn_body(TypeDefn, TypeBody),
            require_complete_switch [TypeBody]
            (
                TypeBody = hlds_du_type(_, _, _, _, _),
                ( if
                    TypeCtor = CurTypeCtor,
                    type_list_to_var_list(TypeArgs, CurTypeParams)
                then
                    % A recursive type that matches exactly the current type
                    % head does not impose any restrictions on the type
                    % parameters.
                    true
                else
                    type_vars_list(ArgTypes, TypeVars),
                    set.insert_list(TypeVars, !InvariantSet)
                )
            ;
                ( TypeBody = hlds_foreign_type(_)
                ; TypeBody = hlds_abstract_type(_)
                ; TypeBody = hlds_solver_type(_)
                ),
                type_vars_list(ArgTypes, TypeVars),
                set.insert_list(TypeVars, !InvariantSet)
            ;
                TypeBody = hlds_eqv_type(_),
                unexpected($pred, "hlds_eqv_type")
            )
        else
            unexpected($pred, "undefined type")
        )
    ;
        CtorArgType = tuple_type(ArgTypes, _Kind),
        list.foldl(
            build_type_param_variance_restrictions_in_ctor_arg_type(TypeTable,
                CurTypeCtor, CurTypeParams),
            ArgTypes, !InvariantSet)
    ;
        CtorArgType = higher_order_type(_PredOrFunc, ArgTypes, _HOInstInfo,
            _Purity, _EvalMethod),
        type_vars_list(ArgTypes, TypeVars),
        set.insert_list(TypeVars, !InvariantSet)
    ;
        CtorArgType = apply_n_type(_, _, _),
        sorry($pred, "apply_n_type")
    ;
        CtorArgType = kinded_type(CtorArgType1, _Kind),
        build_type_param_variance_restrictions_in_ctor_arg_type(TypeTable,
            CurTypeCtor, CurTypeParams, CtorArgType1, !InvariantSet)
    ).

%---------------------%

:- pred check_coerce_type_params(type_table::in, tvarset::in,
    invariant_set::in, list(tvar)::in, list(mer_type)::in, list(mer_type)::in,
    type_assign::in, type_assign::out) is semidet.

check_coerce_type_params(TypeTable, TVarSet, InvariantSet,
        TypeParams, FromTypeArgs, ToTypeArgs, !TypeAssign) :-
    (
        TypeParams = [],
        FromTypeArgs = [],
        ToTypeArgs = []
    ;
        TypeParams = [TypeVar | TailTypeParams],
        FromTypeArgs = [FromType | TailFromTypes],
        ToTypeArgs = [ToType | TailToTypes],
        check_coerce_type_param(TypeTable, TVarSet, InvariantSet,
            TypeVar, FromType, ToType, !TypeAssign),
        check_coerce_type_params(TypeTable, TVarSet, InvariantSet,
            TailTypeParams, TailFromTypes, TailToTypes, !TypeAssign)
    ).

:- pred check_coerce_type_param(type_table::in, tvarset::in, invariant_set::in,
    tvar::in, mer_type::in, mer_type::in, type_assign::in, type_assign::out)
    is semidet.

check_coerce_type_param(TypeTable, TVarSet, InvariantSet,
        TypeVar, FromType, ToType, !TypeAssign) :-
    ( if set.contains(InvariantSet, TypeVar) then
        compare_types(TypeTable, TVarSet, compare_equal, FromType, ToType,
            !TypeAssign)
    else
        ( if
            compare_types(TypeTable, TVarSet, compare_equal_lt,
                FromType, ToType, !TypeAssign)
        then
            true
        else
            compare_types(TypeTable, TVarSet, compare_equal_lt,
                ToType, FromType, !TypeAssign)
        )
    ).

%---------------------%

:- type types_comparison
    --->    compare_equal
    ;       compare_equal_lt.

    % Succeed if TypeA unifies with TypeB (possibly binding type vars).
    % If Comparison is compare_equal_lt, then also succeed if TypeA =< TypeB
    % by subtype definitions.
    %
:- pred compare_types(type_table::in, tvarset::in,
    types_comparison::in, mer_type::in, mer_type::in,
    type_assign::in, type_assign::out) is semidet.

compare_types(TypeTable, TVarSet, Comparison, TypeA, TypeB,
        !TypeAssign) :-
    ( if
        ( TypeA = type_variable(_, _)
        ; TypeB = type_variable(_, _)
        )
    then
        type_assign_unify_type(TypeA, TypeB, !TypeAssign)
    else
        compare_types_nonvar(TypeTable, TVarSet, Comparison, TypeA, TypeB,
            !TypeAssign)
    ).

:- pred compare_types_nonvar(type_table::in, tvarset::in, types_comparison::in,
    mer_type::in, mer_type::in, type_assign::in, type_assign::out) is semidet.

compare_types_nonvar(TypeTable, TVarSet, Comparison, TypeA, TypeB,
        !TypeAssign) :-
    require_complete_switch [TypeA]
    (
        TypeA = builtin_type(BuiltinType),
        TypeB = builtin_type(BuiltinType)
    ;
        TypeA = type_variable(_, _),
        TypeB = type_variable(_, _),
        unexpected($pred, "type_variable")
    ;
        TypeA = defined_type(_, _, _),
        type_to_ctor_and_args(TypeA, TypeCtorA, ArgsA),
        type_to_ctor_and_args(TypeB, TypeCtorB, ArgsB),
        ( if TypeCtorA = TypeCtorB then
            compare_types_corresponding(TypeTable, TVarSet, Comparison,
                ArgsA, ArgsB, !TypeAssign)
        else
            Comparison = compare_equal_lt,
            get_supertype(TypeTable, TVarSet, TypeCtorA, ArgsA, SuperTypeA),
            compare_types(TypeTable, TVarSet, Comparison, SuperTypeA, TypeB,
                !TypeAssign)
        )
    ;
        TypeA = tuple_type(ArgsA, Kind),
        TypeB = tuple_type(ArgsB, Kind),
        compare_types_corresponding(TypeTable, TVarSet, Comparison,
            ArgsA, ArgsB, !TypeAssign)
    ;
        TypeA = higher_order_type(PredOrFunc, ArgsA, _HOInstInfoA,
            Purity, EvalMethod),
        TypeB = higher_order_type(PredOrFunc, ArgsB, _HOInstInfoB,
            Purity, EvalMethod),
        % We do not allow subtyping in higher order argument types.
        compare_types_corresponding(TypeTable, TVarSet, compare_equal,
            ArgsA, ArgsB, !TypeAssign)
    ;
        TypeA = apply_n_type(_, _, _),
        sorry($pred, "apply_n_type")
    ;
        TypeA = kinded_type(TypeA1, Kind),
        TypeB = kinded_type(TypeB1, Kind),
        compare_types(TypeTable, TVarSet, Comparison, TypeA1, TypeB1,
            !TypeAssign)
    ).

:- pred compare_types_corresponding(type_table::in, tvarset::in,
    types_comparison::in, list(mer_type)::in, list(mer_type)::in,
    type_assign::in, type_assign::out) is semidet.

compare_types_corresponding(_TypeTable, _TVarSet, _Comparison,
        [], [], !TypeAssign).
compare_types_corresponding(TypeTable, TVarSet, Comparison,
        [TypeA | TypesA], [TypeB | TypesB], !TypeAssign) :-
    compare_types(TypeTable, TVarSet, Comparison, TypeA, TypeB, !TypeAssign),
    compare_types_corresponding(TypeTable, TVarSet, Comparison, TypesA, TypesB,
        !TypeAssign).

%---------------------------------------------------------------------------%

    % Remove satisfied coerce constraints from each type assignment,
    % then drop any type assignments with unsatisfied coerce constraints
    % if there is at least one type assignment that does satisfy coerce
    % constraints.
    %
:- pred typecheck_prune_coerce_constraints(type_assign_set::in,
    type_assign_set::out, typecheck_info::in, typecheck_info::out) is det.

typecheck_prune_coerce_constraints(TypeAssignSet0, TypeAssignSet, !Info) :-
    typecheck_info_get_type_table(!.Info, TypeTable),
    list.map(type_assign_prune_coerce_constraints(TypeTable),
        TypeAssignSet0, TypeAssignSet1),
    list.filter(type_assign_has_no_coerce_constraints,
        TypeAssignSet1, SatisfiedTypeAssignSet, UnsatisfiedTypeAssignSet),
    (
        SatisfiedTypeAssignSet = [_ | _],
        TypeAssignSet = SatisfiedTypeAssignSet
    ;
        SatisfiedTypeAssignSet = [],
        TypeAssignSet = UnsatisfiedTypeAssignSet
    ).

:- pred type_assign_prune_coerce_constraints(type_table::in,
    type_assign::in, type_assign::out) is det.

type_assign_prune_coerce_constraints(TypeTable, !TypeAssign) :-
    type_assign_get_coerce_constraints(!.TypeAssign, Coercions0),
    (
        Coercions0 = []
    ;
        Coercions0 = [_ | _],
        check_and_drop_coerce_constraints(TypeTable, Coercions0, Coercions,
            !TypeAssign),
        type_assign_set_coerce_constraints(Coercions, !TypeAssign)
    ).

:- pred check_and_drop_coerce_constraints(type_table::in,
    list(coerce_constraint)::in, list(coerce_constraint)::out,
    type_assign::in, type_assign::out) is det.

check_and_drop_coerce_constraints(_TypeTable, [], [], !TypeAssign).
check_and_drop_coerce_constraints(TypeTable, [Coercion0 | Coercions0],
        KeepCoercions, !TypeAssign) :-
    check_coerce_constraint(TypeTable, Coercion0, !.TypeAssign, Satisfied),
    (
        Satisfied = yes(!:TypeAssign),
        check_and_drop_coerce_constraints(TypeTable, Coercions0,
            KeepCoercions, !TypeAssign)
    ;
        Satisfied = no,
        check_and_drop_coerce_constraints(TypeTable, Coercions0,
            TailKeepCoercions, !TypeAssign),
        KeepCoercions = [Coercion0 | TailKeepCoercions]
    ).

:- pred check_coerce_constraint(type_table::in, coerce_constraint::in,
    type_assign::in, maybe(type_assign)::out) is det.

check_coerce_constraint(TypeTable, Coercion, TypeAssign0, Satisfied) :-
    Coercion = coerce_constraint(FromType0, ToType0, _Context, Status),
    (
        Status = need_to_check,
        type_assign_get_type_bindings(TypeAssign0, TypeBindings),
        type_assign_get_typevarset(TypeAssign0, TVarSet),
        apply_rec_subst_to_type(TypeBindings, FromType0, FromType),
        apply_rec_subst_to_type(TypeBindings, ToType0, ToType),
        ( if
            typecheck_coerce_between_types(TypeTable, TVarSet,
                FromType, ToType, TypeAssign0, TypeAssign)
        then
            Satisfied = yes(TypeAssign)
        else
            Satisfied = no
        )
    ;
        Status = unsatisfiable,
        Satisfied = no
    ).

:- pred type_assign_has_no_coerce_constraints(type_assign::in)
    is semidet.

type_assign_has_no_coerce_constraints(TypeAssign) :-
    type_assign_get_coerce_constraints(TypeAssign, []).

%---------------------------------------------------------------------------%
:- end_module check_hlds.typecheck.
%---------------------------------------------------------------------------%
