%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1993-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
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
%   Access predicates for the typecheck_info data structure are called
%   typecheck_info_*.
%
% There are four sorts of types:
%
% 1 discriminated unions:
%   :- type tree(T) ---> nil ; t(tree(T), T, tree(T)).
%
% 2 equivalent types (treated identically, ie, same name. Any number of types
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
%-----------------------------------------------------------------------------%
%
% Known Bugs:
%
% XXX Type inference doesn't handle ambiguity as well as it could do.
% We should do a topological sort, and then typecheck it all bottom-up.
% If we infer an ambiguous type for a pred, we should not reject it
% immediately; instead we should give it an overloaded type, and keep going.
% When we've finished type inference, we should then delete unused
% overloadings, and only then should we report ambiguity errors,
% if any overloading still remains.
%
% Wish list:
%
% - We should handle equivalence types here.
%
%-----------------------------------------------------------------------------%

:- module check_hlds.typecheck.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module parse_tree.
:- import_module parse_tree.error_util.

:- import_module bool.
:- import_module list.

    % typecheck_module(!ModuleInfo, Specs, ExceededIterationLimit):
    %
    % Type checks ModuleInfo and annotates it with variable type information.
    % Specs is set to the list of errors and warnings found, plus messages
    % about the predicates and functions whose types have been inferred.
    % ExceededIterationLimit is set to `yes' if the type inference iteration
    % limit was reached and `no' otherwise.
    %
:- pred typecheck_module(module_info::in, module_info::out,
    list(error_spec)::out, bool::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.type_util.
:- import_module check_hlds.typecheck_errors.
:- import_module check_hlds.typecheck_info.
:- import_module check_hlds.typeclasses.
:- import_module hlds.goal_path.
:- import_module hlds.goal_util.
:- import_module hlds.headvar_names.
:- import_module hlds.hlds_args.
:- import_module hlds.hlds_clauses.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_error_util.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_rtti.
:- import_module hlds.instmap.
:- import_module hlds.passes_aux.
:- import_module hlds.pred_table.
:- import_module hlds.special_pred.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.goal_path.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.file_names.         % undesirable dependency
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_event.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_subst.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.set_of_var.

:- import_module assoc_list.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module set_tree234.
:- import_module string.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

typecheck_module(!ModuleInfo, Specs, ExceededIterationLimit) :-
    module_info_get_globals(!.ModuleInfo, Globals),
    globals.lookup_int_option(Globals, type_inference_iteration_limit,
        MaxIterations),

    module_info_get_valid_predids(OrigValidPredIds, !ModuleInfo),
    OrigValidPredIdSet = set_tree234.list_to_set(OrigValidPredIds),

    module_info_get_preds(!.ModuleInfo, PredMap0),
    map.to_assoc_list(PredMap0, PredIdsInfos0),

    % We seem to need this prepass. Without it, the typechecker throws
    % an exception for two test cases involving field access functions.
    % The reason is almost certainly that some part of the typechecker
    % inspects the definitions of callees (even though it shouldn't),
    % and thus gets things wrong if field access functions haven't yet had
    % their defining clauses added to their pred_infos. We cannot add the
    % defining clauses to the pred_infos of field access functions when those
    % pred_infos are created, since those clauses are only defaults; any
    % clauses given by the user override them. This pass is the first chance
    % to decide that there won't be any user-given clauses coming, and that
    % therefore the default clauses should be the actual clauses.
    prepare_for_typecheck(!.ModuleInfo, OrigValidPredIdSet,
        PredIdsInfos0, PredIdsInfos),

    map.from_sorted_assoc_list(PredIdsInfos, PredMap),
    module_info_set_preds(PredMap, !ModuleInfo),

    typecheck_to_fixpoint(1, MaxIterations, !ModuleInfo,
        OrigValidPredIds, OrigValidPredIdSet, FinalValidPredIdSet,
        CheckSpecs, ExceededIterationLimit),

    construct_type_inference_messages(!.ModuleInfo, FinalValidPredIdSet,
        OrigValidPredIds, [], InferSpecs),
    Specs = InferSpecs ++ CheckSpecs.

:- pred prepare_for_typecheck(module_info::in, set_tree234(pred_id)::in,
    assoc_list(pred_id, pred_info)::in, assoc_list(pred_id, pred_info)::out)
    is det.

prepare_for_typecheck(_, _, [], []).
prepare_for_typecheck(ModuleInfo, ValidPredIdSet,
        [PredIdInfo0 | PredIdsInfos0], [PredIdInfo | PredIdsInfos]) :-
    some [!PredInfo] (
        PredIdInfo0 = PredId - !:PredInfo,
        ( set_tree234.contains(ValidPredIdSet, PredId) ->
            % Goal ids are used to identify typeclass constraints.
            pred_info_get_clauses_info(!.PredInfo, GoalIdClausesInfo0),
            fill_goal_id_slots_in_clauses(ModuleInfo, _ContainingGoalMap,
                GoalIdClausesInfo0, GoalIdClausesInfo),
            pred_info_set_clauses_info(GoalIdClausesInfo, !PredInfo),

            maybe_add_field_access_function_clause(ModuleInfo, !PredInfo),

            module_info_get_globals(ModuleInfo, Globals),
            maybe_improve_headvar_names(Globals, !PredInfo),
            PredIdInfo = PredId - !.PredInfo
        ;
            PredIdInfo = PredIdInfo0
        )
    ),
    prepare_for_typecheck(ModuleInfo, ValidPredIdSet,
        PredIdsInfos0, PredIdsInfos).

    % Repeatedly typecheck the code for a group of predicates
    % until a fixpoint is reached, or until some errors are detected.
    %
:- pred typecheck_to_fixpoint(int::in, int::in,
    module_info::in, module_info::out,
    list(pred_id)::in, set_tree234(pred_id)::in, set_tree234(pred_id)::out,
    list(error_spec)::out, bool::out) is det.

typecheck_to_fixpoint(Iteration, MaxIterations, !ModuleInfo,
        OrigValidPredIds, OrigValidPredIdSet, FinalValidPredIdSet,
        Specs, ExceededIterationLimit) :-
    module_info_get_preds(!.ModuleInfo, PredMap0),
    map.to_assoc_list(PredMap0, PredIdsInfos0),
    typecheck_module_one_iteration(!.ModuleInfo, OrigValidPredIdSet,
        PredIdsInfos0, PredIdsInfos, [], NewlyInvalidPredIds,
        [], CurSpecs, no, Changed),
    map.from_sorted_assoc_list(PredIdsInfos, PredMap),
    module_info_set_preds(PredMap, !ModuleInfo),

    set_tree234.delete_list(NewlyInvalidPredIds,
        OrigValidPredIdSet, NewValidPredIdSet),
    NewValidPredIds = set_tree234.to_sorted_list(NewValidPredIdSet),
    module_info_set_valid_predids(NewValidPredIds, !ModuleInfo),

    module_info_get_globals(!.ModuleInfo, Globals),
    (
        ( Changed = no
        ; contains_errors(Globals, CurSpecs) = yes
        )
    ->
        FinalValidPredIdSet = NewValidPredIdSet,
        Specs = CurSpecs,
        ExceededIterationLimit = no
    ;
        globals.lookup_bool_option(Globals, debug_types, DebugTypes),
        (
            DebugTypes = yes,
            construct_type_inference_messages(!.ModuleInfo, NewValidPredIdSet,
                OrigValidPredIds, [], ProgressSpecs),
            trace [io(!IO)] (
                write_error_specs(ProgressSpecs, Globals, 0, _, 0, _, !IO)
            )
        ;
            DebugTypes = no
        ),
        ( Iteration < MaxIterations ->
            typecheck_to_fixpoint(Iteration + 1, MaxIterations, !ModuleInfo,
                OrigValidPredIds, OrigValidPredIdSet, FinalValidPredIdSet,
                Specs, ExceededIterationLimit)
        ;
            FinalValidPredIdSet = NewValidPredIdSet,
            Specs = [typecheck_report_max_iterations_exceeded(MaxIterations)],
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
    (
        check_marker(Markers, marker_infer_type),
        set_tree234.contains(ValidPredIdSet, PredId),
        not pred_info_is_promise(PredInfo, _)
    ->
        Spec = construct_type_inference_message(PredInfo),
        !:Specs = [Spec | !.Specs]
    ;
        true
    ),
    construct_type_inference_messages(ModuleInfo, ValidPredIdSet,
        PredIds, !Specs).

    % Construct a message containing the inferred `pred' or `func' declaration
    % for a single predicate.
    %
:- func construct_type_inference_message(pred_info) = error_spec.

construct_type_inference_message(PredInfo) = Spec :-
    PredName = pred_info_name(PredInfo),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    Name = unqualified(PredName),
    pred_info_get_context(PredInfo, Context),
    pred_info_get_arg_types(PredInfo, VarSet, ExistQVars, Types0),
    strip_builtin_qualifiers_from_type_list(Types0, Types),
    pred_info_get_class_context(PredInfo, ClassContext),
    pred_info_get_purity(PredInfo, Purity),
    MaybeDet = no,
    AppendVarNums = no,
    (
        PredOrFunc = pf_predicate,
        TypeStr = mercury_pred_type_to_string(VarSet, ExistQVars, Name, Types,
            MaybeDet, Purity, ClassContext, Context, AppendVarNums)
    ;
        PredOrFunc = pf_function,
        pred_args_to_func_args(Types, ArgTypes, RetType),
        TypeStr = mercury_func_type_to_string(VarSet, ExistQVars, Name,
            ArgTypes, RetType, MaybeDet, Purity, ClassContext, Context,
            AppendVarNums)
    ),
    Pieces = [words("Inferred"), words(TypeStr), nl],
    Msg = simple_msg(Context,
        [option_is_set(inform_inferred_types, yes, [always(Pieces)])]),
    Severity = severity_conditional(inform_inferred_types, yes,
        severity_informational, no),
    Spec = error_spec(Severity, phase_type_check, [Msg]).

:- func typecheck_report_max_iterations_exceeded(int) = error_spec.

typecheck_report_max_iterations_exceeded(MaxIterations) = Spec :-
    Pieces = [words("Type inference iteration limit exceeded."),
        words("This probably indicates that your program has a type error."),
        words("You should declare the types explicitly."),
        words("(The current limit is"), int_fixed(MaxIterations),
        words("iterations."),
        words("You can use the `--type-inference-iteration-limit' option"),
        words("to increase the limit).")],
    Msg = error_msg(no, do_not_treat_as_first, 0, [always(Pieces)]),
    Spec = error_spec(severity_error, phase_type_check, [Msg]).

%-----------------------------------------------------------------------------%

    % Iterate over the list of pred_ids in a module.
    %
:- pred typecheck_module_one_iteration(module_info::in,
    set_tree234(pred_id)::in,
    assoc_list(pred_id, pred_info)::in, assoc_list(pred_id, pred_info)::out,
    list(pred_id)::in, list(pred_id)::out,
    list(error_spec)::in, list(error_spec)::out, bool::in, bool::out) is det.

typecheck_module_one_iteration(_, _, [], [],
        !NewlyInvalidPredIds, !Specs, !Changed).
typecheck_module_one_iteration(ModuleInfo, ValidPredIdSet,
        [PredIdInfo0 | PredIdsInfos0], [PredIdInfo | PredIdsInfos],
        !NewlyInvalidPredIds, !Specs, !Changed) :-
    PredIdInfo0 = PredId - PredInfo0,
    (
        (
            pred_info_is_imported(PredInfo0)
        ;
            not set_tree234.contains(ValidPredIdSet, PredId)
        )
    ->
        PredIdInfo = PredIdInfo0,
        typecheck_module_one_iteration(ModuleInfo, ValidPredIdSet,
            PredIdsInfos0, PredIdsInfos, !NewlyInvalidPredIds,
            !Specs, !Changed)
    ;
        % Potential parallelization site.
        typecheck_pred_if_needed(ModuleInfo, PredId, PredInfo0, PredInfo,
            PredSpecs, PredChanged),

        module_info_get_globals(ModuleInfo, Globals),
        ContainsErrors = contains_errors(Globals, PredSpecs),
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
            % % If we get an error, we need to call post_finish_ill_typed_pred
            % on the pred, to ensure that its mode declaration gets properly
            % module qualified; then we call `remove_predid', so that the
            % predicate's definition will be ignored by later passes
            % (the declaration will still be used to check any calls to it).
            %
            % post_finish_ill_typed_pred(ModuleInfo0, PredId,
            %   PredInfo1, PredInfo)
            !:NewlyInvalidPredIds = [PredId | !.NewlyInvalidPredIds]
        ),
        PredIdInfo = PredId - PredInfo,
        !:Specs = PredSpecs ++ !.Specs,
        bool.or(PredChanged, !Changed),
        typecheck_module_one_iteration(ModuleInfo, ValidPredIdSet,
            PredIdsInfos0, PredIdsInfos, !NewlyInvalidPredIds,
            !Specs, !Changed)
    ).

:- pred typecheck_pred_if_needed(module_info::in, pred_id::in,
    pred_info::in, pred_info::out, list(error_spec)::out, bool::out) is det.

typecheck_pred_if_needed(ModuleInfo, PredId, !PredInfo, Specs, Changed) :-
    (
        % Compiler-generated predicates are created already type-correct,
        % so there's no need to typecheck them. The same is true for builtins,
        % except for builtins marked with marker_builtin_stub which need to
        % have their stub clauses generated.
        % But, compiler-generated unify predicates are not guaranteed to be
        % type-correct if they call a user-defined equality or comparison
        % predicate or if it is a special pred for an existentially typed
        % data type.
        (
            is_unify_or_compare_pred(!.PredInfo),
            \+ special_pred_needs_typecheck(!.PredInfo, ModuleInfo)
        ;
            pred_info_is_builtin(!.PredInfo),
            pred_info_get_markers(!.PredInfo, Markers),
            \+ check_marker(Markers, marker_builtin_stub)
        )
    ->
        pred_info_get_clauses_info(!.PredInfo, ClausesInfo0),
        clauses_info_get_clauses_rep(ClausesInfo0, ClausesRep0, _ItemNumbers),
        IsEmpty = clause_list_is_empty(ClausesRep0),
        (
            IsEmpty = yes,
            pred_info_mark_as_external(!PredInfo)
        ;
            IsEmpty = no
        ),
        Specs = [],
        Changed = no
    ;
        typecheck_pred(ModuleInfo, PredId, !PredInfo, Specs, Changed)
    ).

:- pred typecheck_pred(module_info::in, pred_id::in,
    pred_info::in, pred_info::out, list(error_spec)::out, bool::out) is det.

typecheck_pred(ModuleInfo, PredId, !PredInfo, Specs, Changed) :-
    % Handle the --allow-stubs and --warn-stubs options.
    % If --allow-stubs is set, and there are no clauses, then
    % - issue a warning (if --warn-stubs is set), and then
    % - generate a "stub" clause that just throws an exception.
    % The real work is done by do_typecheck_pred.

    module_info_get_globals(ModuleInfo, Globals),
    pred_info_get_arg_types(!.PredInfo, _ArgTypeVarSet, _ExistQVars0,
        ArgTypes0),
    pred_info_get_clauses_info(!.PredInfo, ClausesInfo0),
    clauses_info_get_clauses_rep(ClausesInfo0, ClausesRep0, ItemNumbers0),
    pred_info_get_markers(!.PredInfo, Markers0),
    clause_list_is_empty(ClausesRep0) = ClausesRep0IsEmpty,
    (
        ClausesRep0IsEmpty = yes,
        (
            globals.lookup_bool_option(Globals, allow_stubs, yes),
            \+ check_marker(Markers0, marker_class_method)
        ->
            Spec = report_no_clauses_stub(ModuleInfo, PredId, !.PredInfo),
            StartingSpecs = [Spec],
            generate_stub_clause(PredId, !PredInfo, ModuleInfo)
        ;
            check_marker(Markers0, marker_builtin_stub)
        ->
            StartingSpecs = [],
            generate_stub_clause(PredId, !PredInfo, ModuleInfo)
        ;
            StartingSpecs = []
        )
    ;
        ClausesRep0IsEmpty = no,
        globals.lookup_bool_option(Globals, warn_non_contiguous_foreign_procs,
            WarnNonContiguousForeignProcs),
        (
            WarnNonContiguousForeignProcs = yes,
            StartingSpecs = report_any_non_contiguous_clauses(ModuleInfo,
                PredId, !.PredInfo, ItemNumbers0, clauses_and_foreign_procs)
        ;
            WarnNonContiguousForeignProcs = no,
            globals.lookup_bool_option(Globals, warn_non_contiguous_clauses,
                WarnNonContiguousClauses),
            (
                WarnNonContiguousClauses = yes,
                StartingSpecs = report_any_non_contiguous_clauses(ModuleInfo,
                    PredId, !.PredInfo, ItemNumbers0, only_clauses)
            ;
                WarnNonContiguousClauses = no,
                StartingSpecs = []
            )
        )
    ),

    some [!ClausesInfo] (
        pred_info_get_clauses_info(!.PredInfo, !:ClausesInfo),
        clauses_info_get_clauses_rep(!.ClausesInfo, ClausesRep1, _ItemNumbers),
        clauses_info_get_headvar_list(!.ClausesInfo, HeadVars),
        clause_list_is_empty(ClausesRep1) = ClausesRep1IsEmpty,
        (
            ClausesRep1IsEmpty = yes,
            expect(unify(StartingSpecs, []), $module, $pred,
                "StartingSpecs not empty"),

            % There are no clauses for class methods. The clauses are generated
            % later on, in polymorphism.expand_class_method_bodies.
            ( check_marker(Markers0, marker_class_method) ->
                % For the moment, we just insert the types of the head vars
                % into the clauses_info.
                vartypes_from_corresponding_lists(HeadVars, ArgTypes0,
                    VarTypes),
                clauses_info_set_vartypes(VarTypes, !ClausesInfo),
                pred_info_set_clauses_info(!.ClausesInfo, !PredInfo),
                % We also need to set the head_type_params field to indicate
                % that all the existentially quantified tvars in the head
                % of this pred are indeed bound by this predicate.
                type_vars_list(ArgTypes0, HeadVarsIncludingExistentials),
                pred_info_set_head_type_params(HeadVarsIncludingExistentials,
                    !PredInfo),
                Specs = [],
                Changed = no
            ;
                Specs = [report_no_clauses(ModuleInfo, PredId, !.PredInfo)],
                Changed = no
            )
        ;
            ClausesRep1IsEmpty = no,
            do_typecheck_pred(ModuleInfo, PredId, !PredInfo,
                StartingSpecs, Specs, Changed)
        )
    ).

:- pred do_typecheck_pred(module_info::in, pred_id::in,
    pred_info::in, pred_info::out,
    list(error_spec)::in, list(error_spec)::out, bool::out) is det.

do_typecheck_pred(ModuleInfo, PredId, !PredInfo, !Specs, Changed) :-
    some [!Info, !ClausesInfo, !HeadTypeParams] (
        pred_info_get_clauses_info(!.PredInfo, !:ClausesInfo),
        clauses_info_get_clauses_rep(!.ClausesInfo, ClausesRep0, ItemNumbers),
        clauses_info_get_headvar_list(!.ClausesInfo, HeadVars),
        clauses_info_get_varset(!.ClausesInfo, VarSet),
        clauses_info_get_explicit_vartypes(!.ClausesInfo, ExplicitVarTypes0),
        pred_info_get_import_status(!.PredInfo, Status),
        pred_info_get_typevarset(!.PredInfo, TypeVarSet0),
        pred_info_get_arg_types(!.PredInfo, _ArgTypeVarSet, ExistQVars0,
            ArgTypes0),
        pred_info_get_markers(!.PredInfo, Markers0),
        ( check_marker(Markers0, marker_infer_type) ->
            % For a predicate whose type is inferred, the predicate is allowed
            % to bind the type variables in the head of the predicate's type
            % declaration. Such predicates are given an initial type
            % declaration of `pred foo(T1, T2, ..., TN)' by make_hlds.m.
            Inferring = yes,
            trace [io(!IO)] (
                write_pred_progress_message("% Inferring type of ",
                    PredId, ModuleInfo, !IO)
            ),
            !:HeadTypeParams = [],
            PredConstraints = constraints([], [])
        ;
            Inferring = no,
            trace [io(!IO)] (
                write_pred_progress_message("% Type-checking ", PredId,
                    ModuleInfo, !IO)
            ),
            type_vars_list(ArgTypes0, !:HeadTypeParams),
            pred_info_get_class_context(!.PredInfo, PredConstraints),
            constraint_list_get_tvars(PredConstraints ^ univ_constraints,
                UnivTVars),
            list.append(UnivTVars, !HeadTypeParams),
            list.sort_and_remove_dups(!HeadTypeParams),
            list.delete_elems(!.HeadTypeParams, ExistQVars0, !:HeadTypeParams)
        ),

        module_info_get_class_table(ModuleInfo, ClassTable),
        make_head_hlds_constraints(ClassTable, TypeVarSet0,
            PredConstraints, Constraints),
        ( pred_info_is_field_access_function(ModuleInfo, !.PredInfo) ->
            IsFieldAccessFunction = yes
        ;
            IsFieldAccessFunction = no
        ),
        pred_info_get_markers(!.PredInfo, PredMarkers0),
        typecheck_info_init(ModuleInfo, PredId, IsFieldAccessFunction,
            TypeVarSet0, VarSet, ExplicitVarTypes0, !.HeadTypeParams,
            Constraints, Status, PredMarkers0, !.Specs, !:Info),
        get_clause_list(ClausesRep0, Clauses0),
        typecheck_clause_list(HeadVars, ArgTypes0, Clauses0, Clauses, !Info),
        % We need to perform a final pass of context reduction at the end,
        % before checking the typeclass constraints.
        perform_context_reduction(!Info),
        typecheck_check_for_ambiguity(whole_pred, HeadVars, !Info),
        typecheck_info_get_final_info(!.Info, !.HeadTypeParams, ExistQVars0,
            ExplicitVarTypes0, TypeVarSet, !:HeadTypeParams, InferredVarTypes0,
            InferredTypeConstraints0, ConstraintProofs, ConstraintMap,
            TVarRenaming, ExistTypeRenaming),
        typecheck_info_get_pred_markers(!.Info, PredMarkers),
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
        pred_info_set_constraint_proofs(ConstraintProofs, !PredInfo),
        pred_info_set_constraint_map(ConstraintMap, !PredInfo),
        pred_info_set_markers(PredMarkers, !PredInfo),

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
            infer_existential_types(ArgTypeVars, ExistQVars, !HeadTypeParams),

            % Now save the information we inferred in the pred_info
            pred_info_set_head_type_params(!.HeadTypeParams, !PredInfo),
            pred_info_set_arg_types(TypeVarSet, ExistQVars, ArgTypes,
                !PredInfo),
            pred_info_get_class_context(!.PredInfo, OldTypeConstraints),
            pred_info_set_class_context(InferredTypeConstraints, !PredInfo),

            % Check if anything changed.
            (
                % If the argument types and the type constraints are identical
                % up to renaming, then nothing has changed.
                pred_info_get_tvar_kinds(!.PredInfo, TVarKinds),
                argtypes_identical_up_to_renaming(TVarKinds, ExistQVars0,
                    ArgTypes0, OldTypeConstraints, ExistQVars, ArgTypes,
                    InferredTypeConstraints)
            ->
                Changed = no
            ;
                Changed = yes
            )
        ;
            Inferring = no,
            pred_info_set_head_type_params(!.HeadTypeParams, !PredInfo),
            pred_info_get_origin(!.PredInfo, Origin0),

            % Leave the original argtypes etc., but apply any substitutions
            % that map existentially quantified type variables to other
            % type vars, and then rename them all to match the new typevarset,
            % so that the type variables names match up (e.g. with the type
            % variables in the constraint_proofs)

            % Apply any type substititions that map existentially quantified
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
                    check_existq_clause(TypeVarSet, ExistQVars0),
                    Clauses, !Info),

                apply_var_renaming_to_var_list(ExistQVars0,
                    ExistTypeRenaming, ExistQVars1),
                apply_variable_renaming_to_type_list(ExistTypeRenaming,
                    ArgTypes0, ArgTypes1),
                apply_variable_renaming_to_prog_constraints(
                    ExistTypeRenaming, PredConstraints, PredConstraints1),
                rename_instance_method_constraints(ExistTypeRenaming,
                    Origin0, Origin1)
            ),

            % Rename them all to match the new typevarset.
            apply_var_renaming_to_var_list(ExistQVars1,
                TVarRenaming, ExistQVars),
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

            Changed = no
        ),
        typecheck_info_get_all_errors(!.Info, !:Specs)
    ).

:- func report_any_non_contiguous_clauses(module_info, pred_id, pred_info,
    clause_item_numbers, clause_item_number_types) = list(error_spec).

report_any_non_contiguous_clauses(ModuleInfo, PredId, PredInfo, ItemNumbers,
        Type) = Specs :-
    (
        clauses_are_non_contiguous(ItemNumbers, Type,
            FirstRegion, SecondRegion, LaterRegions)
    ->
        Spec = report_non_contiguous_clauses(ModuleInfo, PredId,
            PredInfo, FirstRegion, SecondRegion, LaterRegions),
        Specs = [Spec]
    ;
        Specs = []
    ).

:- pred check_existq_clause(tvarset::in, existq_tvars::in, clause::in,
    typecheck_info::in, typecheck_info::out) is det.

check_existq_clause(TypeVarSet, ExistQVars, Clause, !Info) :-
    Goal = Clause ^ clause_body,
    ( Goal = hlds_goal(call_foreign_proc(_, _, _, _, _, _, Impl), _) ->
        list.foldl(check_mention_existq_var(TypeVarSet, Impl), ExistQVars,
            !Info)
    ;
        true
    ).

:- pred check_mention_existq_var(tvarset::in, pragma_foreign_proc_impl::in,
    tvar::in, typecheck_info::in, typecheck_info::out) is det.

check_mention_existq_var(TypeVarSet, Impl, TVar, !Info) :-
    varset.lookup_name(TypeVarSet, TVar, Name),
    VarName = "TypeInfo_for_" ++ Name,
    ( foreign_proc_uses_variable(Impl, VarName) ->
        true
    ;
        Spec = report_missing_tvar_in_foreign_code(!.Info, VarName),
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
:- pred generate_stub_clause(pred_id::in, pred_info::in, pred_info::out,
    module_info::in) is det.

generate_stub_clause(PredId, !PredInfo, ModuleInfo) :-
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
    % Mark the predicate as a stub
    % (i.e. record that it originally had no clauses)
    pred_info_get_markers(!.PredInfo, Markers0),
    add_marker(marker_stub, Markers0, Markers),
    pred_info_set_markers(Markers, !PredInfo),

    % Generate `PredName = "<PredName>"'.
    varset.new_named_var("PredName", PredNameVar, !VarSet),
    make_string_const_construction(PredNameVar, PredName, UnifyGoal),

    % Generate `private_builtin.no_clauses(PredName)'
    % or `private_builtin.sorry(PredName)'
    ModuleName = pred_info_module(!.PredInfo),
    ( mercury_std_library_module_name(ModuleName) ->
        CalleeName = "sorry"
    ;
        CalleeName = "no_clauses"
    ),
    pred_info_get_context(!.PredInfo, Context),
    generate_simple_call(mercury_private_builtin_module, CalleeName,
        pf_predicate, only_mode, detism_det, purity_pure, [PredNameVar], [],
        instmap_delta_bind_no_var, ModuleInfo, Context, CallGoal),

    % Combine the unification and call into a conjunction.
    goal_info_init(Context, GoalInfo),
    Body = hlds_goal(conj(plain_conj, [UnifyGoal, CallGoal]), GoalInfo),
    StubClause = clause(all_modes, Body, impl_lang_mercury, Context, []).

:- pred rename_instance_method_constraints(tvar_renaming::in,
    pred_origin::in, pred_origin::out) is det.

rename_instance_method_constraints(Renaming, Origin0, Origin) :-
    ( Origin0 = origin_instance_method(MethodName, Constraints0) ->
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
    ;
        Origin = Origin0
    ).

    % Infer which of the head variable types must be existentially quantified.
    %
:- pred infer_existential_types(list(tvar)::in, existq_tvars::out,
    head_type_params::in, head_type_params::out) is det.

infer_existential_types(ArgTypeVars, ExistQVars,
        HeadTypeParams0, HeadTypeParams) :-
    % First, infer which of the head variable types must be existentially
    % quantified: anything that was inserted into the HeadTypeParams0 set must
    % have been inserted due to an existential type in something we called,
    % and thus must be existentially quantified. (Note that concrete types
    % are "more general" than existentially quantified types, so we prefer to
    % infer a concrete type if we can rather than an existential type.)

    set.list_to_set(ArgTypeVars, ArgTypeVarsSet),
    set.list_to_set(HeadTypeParams0, HeadTypeParamsSet),
    set.intersect(ArgTypeVarsSet, HeadTypeParamsSet, ExistQVarsSet),
    set.difference(ArgTypeVarsSet, ExistQVarsSet, UnivQVarsSet),
    set.to_sorted_list(ExistQVarsSet, ExistQVars),
    set.to_sorted_list(UnivQVarsSet, UnivQVars),

    % Then we need to insert the universally quantified head variable types
    % into the HeadTypeParams set, which will now contain all the type
    % variables that are produced either by stuff we call or by our caller.
    % This is needed so that it has the right value when post_typecheck.m
    % uses it to check for unbound type variables.

    list.append(UnivQVars, HeadTypeParams0, HeadTypeParams).

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

is_head_class_constraint(HeadTypeVars, constraint(_Name, Types)) :-
    all [TVar] (
            prog_type.type_list_contains_var(Types, TVar)
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
    % (b) it is the unification or comparison predicate for an existially
    %     quantified type.
    %
    % In case (b), we need to typecheck it to fill in the head_type_params
    % field in the pred_info.
    %
:- pred special_pred_needs_typecheck(pred_info::in, module_info::in)
    is semidet.

special_pred_needs_typecheck(PredInfo, ModuleInfo) :-
    % Check if the predicate is a compiler-generated special
    % predicate, and if so, for which type.
    pred_info_get_origin(PredInfo, Origin),
    Origin = origin_special_pred(SpecialPredId - TypeCtor),

    % Check that the special pred isn't one of the builtin types which don't
    % have a hlds_type_defn.
    \+ list.member(TypeCtor, builtin_type_ctors_with_no_hlds_type_defn),

    % Check whether that type is a type for which there is a user-defined
    % equality predicate, or which is existentially typed.
    module_info_get_type_table(ModuleInfo, TypeTable),
    lookup_type_ctor_defn(TypeTable, TypeCtor, TypeDefn),
    hlds_data.get_type_defn_body(TypeDefn, Body),
    special_pred_for_type_needs_typecheck(ModuleInfo, SpecialPredId, Body).

%-----------------------------------------------------------------------------%

    % For a field access function for which the user has supplied
    % a declaration but no clauses, add a clause
    % 'foo :='(X, Y) = 'foo :='(X, Y).
    % As for the default clauses added for builtins, this is not a recursive
    % call -- post_typecheck.m will expand the body into unifications.
    %
:- pred maybe_add_field_access_function_clause(module_info::in,
    pred_info::in, pred_info::out) is det.

maybe_add_field_access_function_clause(ModuleInfo, !PredInfo) :-
    pred_info_get_import_status(!.PredInfo, ImportStatus),
    pred_info_get_clauses_info(!.PredInfo, ClausesInfo0),
    clauses_info_get_clauses_rep(ClausesInfo0, ClausesRep0, _ItemNumbers0),
    (
        pred_info_is_field_access_function(ModuleInfo, !.PredInfo),
        clause_list_is_empty(ClausesRep0) = yes,
        status_defined_in_this_module(ImportStatus) = yes
    ->
        clauses_info_get_headvars(ClausesInfo0, HeadVars),
        proc_arg_vector_to_func_args(HeadVars, FuncArgs, FuncRetVal),
        pred_info_get_context(!.PredInfo, Context),
        FuncModule = pred_info_module(!.PredInfo),
        FuncName = pred_info_name(!.PredInfo),
        PredArity = pred_info_orig_arity(!.PredInfo),
        adjust_func_arity(pf_function, FuncArity, PredArity),
        FuncSymName = qualified(FuncModule, FuncName),
        FuncConsId = cons(FuncSymName, FuncArity, cons_id_dummy_type_ctor),
        FuncRHS = rhs_functor(FuncConsId, no, FuncArgs),
        create_pure_atomic_complicated_unification(FuncRetVal,
            FuncRHS, Context, umc_explicit, [], Goal0),
        Goal0 = hlds_goal(GoalExpr, GoalInfo0),
        NonLocals = set_of_var.list_to_set(proc_arg_vector_to_list(HeadVars)),
        goal_info_set_nonlocals(NonLocals, GoalInfo0, GoalInfo),
        Goal = hlds_goal(GoalExpr, GoalInfo),
        Clause = clause(all_modes, Goal, impl_lang_mercury, Context, []),
        set_clause_list([Clause], ClausesRep),
        ItemNumbers = init_clause_item_numbers_comp_gen,
        clauses_info_set_clauses_rep(ClausesRep, ItemNumbers,
            ClausesInfo0, ClausesInfo),
        pred_info_update_goal_type(goal_type_clause_and_foreign, !PredInfo),
        pred_info_set_clauses_info(ClausesInfo, !PredInfo),
        pred_info_get_markers(!.PredInfo, Markers0),
        add_marker(marker_calls_are_fully_qualified, Markers0, Markers),
        pred_info_set_markers(Markers, !PredInfo)
    ;
        true
    ).

%-----------------------------------------------------------------------------%

    % Iterate over the list of clauses for a predicate.
    %
:- pred typecheck_clause_list(list(prog_var)::in, list(mer_type)::in,
    list(clause)::in, list(clause)::out,
    typecheck_info::in, typecheck_info::out) is det.

typecheck_clause_list(_, _, [], [], !Info).
typecheck_clause_list(HeadVars, ArgTypes, [Clause0 | Clauses0],
        [Clause | Clauses], !Info) :-
    typecheck_clause(HeadVars, ArgTypes, Clause0, Clause, !Info),
    typecheck_clause_list(HeadVars, ArgTypes, Clauses0, Clauses, !Info).

%-----------------------------------------------------------------------------%

    % Type-check a single clause.

    % As we go through a clause, we determine the possible type assignments
    % for the clause. A type assignment is an assignment of a type to each
    % variable in the clause.
    %
    % Note that this may cause exponential time & space usage in the presence
    % of overloading of predicates and/or functors. This is a potentially
    % serious problem, but there's no easy solution apparent.
    %
    % It would be more natural to use non-determinism to write this code,
    % and perhaps even more efficient. But doing it nondeterministically
    % would make good error messages very difficult.
    %
    % We should perhaps do manual garbage collection here.
    %
:- pred typecheck_clause(list(prog_var)::in, list(mer_type)::in,
    clause::in, clause::out, typecheck_info::in, typecheck_info::out) is det.

typecheck_clause(HeadVars, ArgTypes, !Clause, !Info) :-
    Body0 = !.Clause ^ clause_body,
    Context = !.Clause ^clause_context,
    !Info ^ tc_info_context := Context,

    % Typecheck the clause - first the head unification, and then the body.
    typecheck_var_has_type_list(HeadVars, ArgTypes, 1, !Info),
    typecheck_goal(Body0, Body, !Info),
    trace [compiletime(flag("type_checkpoint")), io(!IO)] (
        type_checkpoint("end of clause", !.Info, !IO)
    ),
    !Clause ^ clause_body := Body,
    !Info ^ tc_info_context := Context,
    typecheck_check_for_ambiguity(clause_only, HeadVars, !Info).

%-----------------------------------------------------------------------------%

    % typecheck_check_for_ambiguity/3:
    % If there are multiple type assignments,
    % then we issue an error message here.

    % If stuff-to-check = whole_pred, report an error for any ambiguity,
    % and also check for unbound type variables.
    % But if stuff-to-check = clause_only, then only report
    % errors for type ambiguities that don't involve the head vars,
    % because we may be able to resolve a type ambiguity for a head var
    % in one clause by looking at later clauses.
    % (Ambiguities in the head variables can only arise if we are
    % inferring the type for this pred.)
    %
:- type stuff_to_check
    --->    clause_only
    ;       whole_pred.

:- pred typecheck_check_for_ambiguity(stuff_to_check::in, list(prog_var)::in,
    typecheck_info::in, typecheck_info::out) is det.

typecheck_check_for_ambiguity(StuffToCheck, HeadVars, !Info) :-
    TypeAssignSet = !.Info ^ tc_info_type_assign_set,
    (
        % There should always be a type assignment, because if there is
        % an error somewhere, instead of setting the current type assignment
        % set to the empty set, the type-checker should continue with the
        % previous type assignment set (so that it can detect other errors
        % in the same clause).
        TypeAssignSet = [],
        unexpected($module, $pred, "no type-assignment")
    ;
        TypeAssignSet = [_SingleTypeAssign]
    ;
        TypeAssignSet = [TypeAssign1, TypeAssign2 | _],

        % We only report an ambiguity error if
        % (a) we haven't encountered any other errors and if
        %     StuffToCheck = clause_only(_), and also
        % (b) the ambiguity occurs only in the body, rather than in the
        %     head variables (and hence can't be resolved by looking at
        %     later clauses).

        typecheck_info_get_all_errors(!.Info, ErrorsSoFar),
        (
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
        ->
            Spec = report_ambiguity_error(!.Info, TypeAssign1, TypeAssign2),
            typecheck_info_add_error(Spec, !Info)
        ;
            true
        )
    ).

%-----------------------------------------------------------------------------%

    % Typecheck a goal.
    % Note that we save the context of the goal in the typecheck_info for
    % use in error messages. Also, if the context of the goal is empty,
    % we set the context of the goal to the surrounding context saved in
    % the typecheck_info. (That should probably be done in make_hlds,
    % but it was easier to do here.)
    %
:- pred typecheck_goal(hlds_goal::in, hlds_goal::out,
    typecheck_info::in, typecheck_info::out) is det.

typecheck_goal(Goal0, Goal, !Info) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    Context = goal_info_get_context(GoalInfo0),
    term.context_init(EmptyContext),
    ( Context = EmptyContext ->
        EnclosingContext = !.Info ^ tc_info_context,
        goal_info_set_context(EnclosingContext, GoalInfo0, GoalInfo)
    ;
        GoalInfo = GoalInfo0,
        !Info ^ tc_info_context := Context
    ),

    TypeAssignSet = !.Info ^ tc_info_type_assign_set,
    list.length(TypeAssignSet, Count),

    % Our algorithm handles overloading quite inefficiently: for each symbol
    % that matches N declarations, we make N copies of the existing set of type
    % assignments. In the worst case, therefore, the complexity of our
    % algorithm (space complexity as well as time complexity) is therefore
    % exponential in the number of ambiguous symbols.
    %
    % We issue a warning whenever the number of type assignments exceeds
    % the warn limit, and stop typechecking (after generating an error)
    % whenever it exceeds the error limit.

    WarnLimit = !.Info ^ tc_info_ambiguity_warn_limit,
    ( Count > WarnLimit ->
        typecheck_info_get_ambiguity_error_limit(!.Info, ErrorLimit),
        ( Count > ErrorLimit ->
            % Override any existing overload warning.
            ErrorSpec = report_error_too_much_overloading(!.Info),
            typecheck_info_set_overload_error(yes(ErrorSpec), !Info),

            % Don't call typecheck_goal_2 to do the actual typechecking,
            % since it will almost certainly take too much time and memory.
            GoalExpr = GoalExpr0
        ;
            typecheck_info_get_overload_error(!.Info, MaybePrevSpec),
            (
                MaybePrevSpec = no,
                WarnSpec = report_warning_too_much_overloading(!.Info),
                typecheck_info_set_overload_error(yes(WarnSpec), !Info)
            ;
                MaybePrevSpec = yes(_)
            ),
            typecheck_goal_2(GoalExpr0, GoalExpr, GoalInfo, !Info)
        )
    ;
        typecheck_goal_2(GoalExpr0, GoalExpr, GoalInfo, !Info)
    ),
    Goal = hlds_goal(GoalExpr, GoalInfo).

:- pred typecheck_goal_2(hlds_goal_expr::in, hlds_goal_expr::out,
    hlds_goal_info::in, typecheck_info::in, typecheck_info::out) is det.

typecheck_goal_2(GoalExpr0, GoalExpr, GoalInfo, !Info) :-
    (
        GoalExpr0 = conj(ConjType, List0),
        trace [compiletime(flag("type_checkpoint")), io(!IO)] (
            type_checkpoint("conj", !.Info, !IO)
        ),
        typecheck_goal_list(List0, List, !Info),
        GoalExpr = conj(ConjType, List)
    ;
        GoalExpr0 = disj(List0),
        trace [compiletime(flag("type_checkpoint")), io(!IO)] (
            type_checkpoint("disj", !.Info, !IO)
        ),
        typecheck_goal_list(List0, List, !Info),
        GoalExpr = disj(List)
    ;
        GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),
        trace [compiletime(flag("type_checkpoint")), io(!IO)] (
            type_checkpoint("if", !.Info, !IO)
        ),
        typecheck_goal(Cond0, Cond, !Info),
        trace [compiletime(flag("type_checkpoint")), io(!IO)] (
            type_checkpoint("then", !.Info, !IO)
        ),
        typecheck_goal(Then0, Then, !Info),
        trace [compiletime(flag("type_checkpoint")), io(!IO)] (
            type_checkpoint("else", !.Info, !IO)
        ),
        typecheck_goal(Else0, Else, !Info),
        ensure_vars_have_a_type(Vars, !Info),
        GoalExpr = if_then_else(Vars, Cond, Then, Else)
    ;
        GoalExpr0 = negation(SubGoal0),
        trace [compiletime(flag("type_checkpoint")), io(!IO)] (
            type_checkpoint("not", !.Info, !IO)
        ),
        typecheck_goal(SubGoal0, SubGoal, !Info),
        GoalExpr = negation(SubGoal)
    ;
        GoalExpr0 = scope(Reason, SubGoal0),
        trace [compiletime(flag("type_checkpoint")), io(!IO)] (
            type_checkpoint("scope", !.Info, !IO)
        ),
        typecheck_goal(SubGoal0, SubGoal, !Info),
        (
            (
                ( Reason = exist_quant(Vars)
                ; Reason = promise_solutions(Vars, _)
                )
            ;
                % These variables are introduced by the compiler and may
                % only have a single, specific type.
                Reason = loop_control(LCVar, LCSVar, _),
                Vars = [LCVar, LCSVar]
            ),
            ensure_vars_have_a_type(Vars, !Info)
        ;
            ( Reason = promise_purity(_)
            ; Reason = require_detism(_)
            ; Reason = require_complete_switch(_)
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
            type_checkpoint("call", !.Info, !IO)
        ),
        list.length(Args, Arity),
        CurCall = simple_call_id(pf_predicate, Name, Arity),
        typecheck_info_set_called_predid(plain_call_id(CurCall), !Info),
        GoalId = goal_info_get_goal_id(GoalInfo),
        typecheck_call_pred(CurCall, Args, GoalId, PredId, !Info),
        GoalExpr = plain_call(PredId, ProcId, Args, BI, UC, Name)
    ;
        GoalExpr0 = generic_call(GenericCall0, Args, Modes, MaybeArgRegs,
            Detism),
        hlds_goal.generic_call_id(GenericCall0, CallId),
        typecheck_info_set_called_predid(CallId, !Info),
        (
            GenericCall0 = higher_order(PredVar, Purity, _, _),
            GenericCall = GenericCall0,
            trace [compiletime(flag("type_checkpoint")), io(!IO)] (
                type_checkpoint("higher-order call", !.Info, !IO)
            ),
            typecheck_higher_order_call(PredVar, Purity, Args, !Info)
        ;
            GenericCall0 = class_method(_, _, _, _),
            unexpected($module, $pred, "unexpected class method call")
        ;
            GenericCall0 = event_call(EventName),
            GenericCall = GenericCall0,
            trace [compiletime(flag("type_checkpoint")), io(!IO)] (
                type_checkpoint("event call", !.Info, !IO)
            ),
            typecheck_event_call(EventName, Args, !Info)
        ;
            GenericCall0 = cast(_),
            % A cast imposes no restrictions on its argument types,
            % so nothing needs to be done here.
            GenericCall = GenericCall0
        ),
        GoalExpr = generic_call(GenericCall, Args, Modes, MaybeArgRegs, Detism)
    ;
        GoalExpr0 = unify(LHS, RHS0, UnifyMode, Unification, UnifyContext),
        trace [compiletime(flag("type_checkpoint")), io(!IO)] (
            type_checkpoint("unify", !.Info, !IO)
        ),
        !Info ^ tc_info_arg_num := 0,
        !Info ^ tc_info_unify_context := UnifyContext,
        GoalId = goal_info_get_goal_id(GoalInfo),
        typecheck_unification(LHS, RHS0, RHS, GoalId, !Info),
        GoalExpr = unify(LHS, RHS, UnifyMode, Unification, UnifyContext)
    ;
        GoalExpr0 = switch(_, _, _),
        unexpected($module, $pred, "switch")
    ;
        GoalExpr0 = call_foreign_proc(_, PredId, _, Args, _, _, _),
        % Foreign_procs are automatically generated, so they will always be
        % type-correct, but we need to do the type analysis in order to
        % correctly compute the HeadTypeParams that result from existentially
        % typed foreign_procs. (We could probably do that more efficiently
        % than the way it is done below, though.)
        ArgVars = list.map(foreign_arg_var, Args),
        GoalId = goal_info_get_goal_id(GoalInfo),
        typecheck_call_pred_id(PredId, ArgVars, GoalId, !Info),
        perform_context_reduction(!Info),
        GoalExpr = GoalExpr0
    ;
        GoalExpr0 = shorthand(ShortHand0),
        (
            ShortHand0 = bi_implication(LHS0, RHS0),
            trace [compiletime(flag("type_checkpoint")), io(!IO)] (
                type_checkpoint("<=>", !.Info, !IO)
            ),
            typecheck_goal(LHS0, LHS, !Info),
            typecheck_goal(RHS0, RHS, !Info),
            ShortHand = bi_implication(LHS, RHS)
        ;
            ShortHand0 = atomic_goal(GoalType, Outer, Inner, MaybeOutputVars,
                MainGoal0, OrElseGoals0, OrElseInners),
            trace [compiletime(flag("type_checkpoint")), io(!IO)] (
                type_checkpoint("atomic_goal", !.Info, !IO)
            ),
            (
                MaybeOutputVars = yes(OutputVars),
                ensure_vars_have_a_type(OutputVars, !Info)
            ;
                MaybeOutputVars = no
            ),

            typecheck_goal(MainGoal0, MainGoal, !Info),
            typecheck_goal_list(OrElseGoals0, OrElseGoals, !Info),

            Outer = atomic_interface_vars(OuterDI, OuterUO),
            ensure_vars_have_a_single_type([OuterDI, OuterUO], !Info),

            % The outer variables must either be both I/O states or STM states.
            % Checking that here could double the number of type assign sets.
            % We therefore delay the check until after we have typechecked
            % the predicate body, in post_typecheck. The code in the
            % post_typecheck pass (actually in purity.m) will do this
            % if the GoalType is unknown_atomic_goal_type.
            InnerVars = atomic_interface_list_to_var_list(
                [Inner | OrElseInners]),
            list.foldl((pred(Var::in, Info0::in, Info::out) is det :-
                    typecheck_var_has_type(Var, stm_atomic_type, Info0, Info)),
                InnerVars, !Info),
            expect(unify(GoalType, unknown_atomic_goal_type), $module, $pred,
                "GoalType != unknown_atomic_goal_type"),
            ShortHand = atomic_goal(GoalType, Outer, Inner, MaybeOutputVars,
                MainGoal, OrElseGoals, OrElseInners)
        ;
            ShortHand0 = try_goal(MaybeIO, ResultVar, SubGoal0),
            trace [compiletime(flag("type_checkpoint")), io(!IO)] (
                type_checkpoint("try_goal", !.Info, !IO)
            ),
            typecheck_goal(SubGoal0, SubGoal, !Info),
            (
                MaybeIO = yes(try_io_state_vars(InitialIO, FinalIO)),
                ensure_vars_have_a_type([InitialIO, FinalIO], !Info),
                typecheck_var_has_type(InitialIO, io_state_type, !Info),
                typecheck_var_has_type(FinalIO, io_state_type, !Info)
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

%-----------------------------------------------------------------------------%

:- pred typecheck_goal_list(list(hlds_goal)::in, list(hlds_goal)::out,
    typecheck_info::in, typecheck_info::out) is det.

typecheck_goal_list([], [], !Info).
typecheck_goal_list([Goal0 | Goals0], [Goal | Goals], !Info) :-
    typecheck_goal(Goal0, Goal, !Info),
    typecheck_goal_list(Goals0, Goals, !Info).

%-----------------------------------------------------------------------------%

    % Ensure that each variable in Vars has been assigned a type.
    %
:- pred ensure_vars_have_a_type(list(prog_var)::in,
    typecheck_info::in, typecheck_info::out) is det.

ensure_vars_have_a_type(Vars, !Info) :-
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
        typecheck_var_has_polymorphic_type_list(Vars, TypeVarSet, [],
            Types, EmptyConstraints, !Info)
    ).

    % Ensure that each variable in Vars has been assigned a single type.
    %
:- pred ensure_vars_have_a_single_type(list(prog_var)::in,
    typecheck_info::in, typecheck_info::out) is det.

ensure_vars_have_a_single_type(Vars, !Info) :-
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
        typecheck_var_has_polymorphic_type_list(Vars, TypeVarSet, [],
            Types, EmptyConstraints, !Info)
    ).

%-----------------------------------------------------------------------------%

:- pred typecheck_higher_order_call(prog_var::in, purity::in,
    list(prog_var)::in, typecheck_info::in, typecheck_info::out) is det.

typecheck_higher_order_call(PredVar, Purity, Args, !Info) :-
    list.length(Args, Arity),
    higher_order_pred_type(Purity, Arity, lambda_normal,
        TypeVarSet, PredVarType, ArgTypes),
    % The class context is empty because higher-order predicates
    % are always monomorphic. Similarly for ExistQVars.
    empty_hlds_constraints(EmptyConstraints),
    ExistQVars = [],
    typecheck_var_has_polymorphic_type_list([PredVar | Args], TypeVarSet,
        ExistQVars, [PredVarType | ArgTypes], EmptyConstraints, !Info).

:- pred higher_order_pred_type(purity::in, int::in, lambda_eval_method::in,
    tvarset::out, mer_type::out, list(mer_type)::out) is det.

    % higher_order_pred_type(Purity, N, EvalMethod,
    %   TypeVarSet, PredType, ArgTypes):
    %
    % Given an arity N, let TypeVarSet = {T1, T2, ..., TN},
    % PredType = `Purity EvalMethod pred(T1, T2, ..., TN)', and
    % ArgTypes = [T1, T2, ..., TN].
    %
higher_order_pred_type(Purity, Arity, EvalMethod, TypeVarSet, PredType,
        ArgTypes) :-
    varset.init(TypeVarSet0),
    varset.new_vars(Arity, ArgTypeVars, TypeVarSet0, TypeVarSet),
    % Argument types always have kind `star'.
    prog_type.var_list_to_type_list(map.init, ArgTypeVars, ArgTypes),
    construct_higher_order_type(Purity, pf_predicate, EvalMethod, ArgTypes,
        PredType).

:- pred higher_order_func_type(purity::in, int::in, lambda_eval_method::in,
    tvarset::out, mer_type::out, list(mer_type)::out, mer_type::out) is det.

    % higher_order_func_type(Purity, N, EvalMethod, TypeVarSet,
    %   FuncType, ArgTypes, RetType):
    %
    % Given an arity N, let TypeVarSet = {T0, T1, T2, ..., TN},
    % FuncType = `Purity EvalMethod func(T1, T2, ..., TN) = T0',
    % ArgTypes = [T1, T2, ..., TN], and
    % RetType = T0.
    %
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

%-----------------------------------------------------------------------------%

:- pred typecheck_event_call(string::in, list(prog_var)::in,
    typecheck_info::in, typecheck_info::out) is det.

typecheck_event_call(EventName, Args, !Info) :-
    ModuleInfo = !.Info ^ tc_info_module_info,
    module_info_get_event_set(ModuleInfo, EventSet),
    EventSpecMap = EventSet ^ event_set_spec_map,
    ( event_arg_types(EventSpecMap, EventName, EventArgTypes) ->
        ( list.same_length(Args, EventArgTypes) ->
            typecheck_var_has_type_list(Args, EventArgTypes, 1, !Info)
        ;
            Spec = report_event_args_mismatch(!.Info, EventName, EventArgTypes,
                Args),
            typecheck_info_add_error(Spec, !Info)
        )
    ;
        Spec = report_unknown_event_call_error(!.Info, EventName),
        typecheck_info_add_error(Spec, !Info)
    ).

%-----------------------------------------------------------------------------%

:- pred typecheck_call_pred(simple_call_id::in, list(prog_var)::in,
    goal_id::in, pred_id::out, typecheck_info::in, typecheck_info::out) is det.

typecheck_call_pred(CallId, Args, GoalId, PredId, !Info) :-
    % Look up the called predicate's arg types.
    ModuleInfo = !.Info ^ tc_info_module_info,
    module_info_get_predicate_table(ModuleInfo, PredicateTable),
    CallId = simple_call_id(PorF, SymName, Arity),
    typecheck_info_get_pred_markers(!.Info, PredMarkers),
    IsFullyQualified = calls_are_fully_qualified(PredMarkers),
    predicate_table_lookup_pf_sym_arity(PredicateTable, IsFullyQualified,
        PorF, SymName, Arity, PredIds),
    (
        PredIds = [],
        PredId = invalid_pred_id,
        Spec = report_pred_call_error(!.Info, CallId),
        typecheck_info_add_error(Spec, !Info)
    ;
        PredIds = [HeadPredId | TailPredIds],
        (
            TailPredIds = [],
            % Handle the case of a non-overloaded predicate specially
            % (so that we can optimize the case of a non-overloaded,
            % non-polymorphic predicate).
            PredId = HeadPredId,
            typecheck_call_pred_id(PredId, Args, GoalId, !Info)
        ;
            TailPredIds = [_ | _],
            typecheck_call_overloaded_pred(CallId, PredIds, Args, GoalId,
                !Info),

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
        perform_context_reduction(!Info)
    ).

    % Typecheck a call to a specific predicate.
    %
:- pred typecheck_call_pred_id(pred_id::in, list(prog_var)::in, goal_id::in,
    typecheck_info::in, typecheck_info::out) is det.

typecheck_call_pred_id(PredId, Args, GoalId, !Info) :-
    ModuleInfo = !.Info ^ tc_info_module_info,
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
    (
        varset.is_empty(PredTypeVarSet),
        PredClassContext = constraints([], [])
    ->
        typecheck_var_has_type_list(Args, PredArgTypes, 1, !Info)
    ;
        module_info_get_class_table(ModuleInfo, ClassTable),
        make_body_hlds_constraints(ClassTable, PredTypeVarSet,
            GoalId, PredClassContext, PredConstraints),
        typecheck_var_has_polymorphic_type_list(Args, PredTypeVarSet,
            PredExistQVars, PredArgTypes, PredConstraints, !Info)
    ).

:- pred typecheck_call_overloaded_pred(simple_call_id::in, list(pred_id)::in,
    list(prog_var)::in, goal_id::in, typecheck_info::in, typecheck_info::out)
    is det.

typecheck_call_overloaded_pred(CallId, PredIdList, Args, GoalId, !Info) :-
    Context = !.Info ^ tc_info_context,
    Symbol = overloaded_pred(CallId, PredIdList),
    typecheck_info_add_overloaded_symbol(Symbol, Context, !Info),

    % Let the new arg_type_assign_set be the cross-product of the current
    % type_assign_set and the set of possible lists of argument types
    % for the overloaded predicate, suitable renamed apart.
    ModuleInfo = !.Info ^ tc_info_module_info,
    module_info_get_class_table(ModuleInfo, ClassTable),
    module_info_get_predicate_table(ModuleInfo, PredicateTable),
    predicate_table_get_preds(PredicateTable, Preds),
    TypeAssignSet0 = !.Info ^ tc_info_type_assign_set,
    get_overloaded_pred_arg_types(PredIdList, Preds, ClassTable, GoalId,
        TypeAssignSet0, [], ArgsTypeAssignSet),

    % Then unify the types of the call arguments with the
    % called predicates' arg types.
    typecheck_var_has_arg_type_list(Args, 1, ArgsTypeAssignSet, !Info).

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

%-----------------------------------------------------------------------------%

    % Rename apart the type variables in called predicate's arg types
    % separately for each type assignment, resulting in an "arg type
    % assignment set", and then for each arg type assignment in the
    % arg type assignment set, check that the argument variables have
    % the expected types.
    % A set of class constraints are also passed in, which must have the
    % types contained within renamed apart.
    %
:- pred typecheck_var_has_polymorphic_type_list(list(prog_var)::in,
    tvarset::in, existq_tvars::in, list(mer_type)::in, hlds_constraints::in,
    typecheck_info::in, typecheck_info::out) is det.

typecheck_var_has_polymorphic_type_list(Args, PredTypeVarSet, PredExistQVars,
        PredArgTypes, PredConstraints, !Info) :-
    TypeAssignSet0 = !.Info ^ tc_info_type_assign_set,
    rename_apart(TypeAssignSet0, PredTypeVarSet, PredExistQVars,
        PredArgTypes, PredConstraints, [], ArgsTypeAssignSet),
    typecheck_var_has_arg_type_list(Args, 1, ArgsTypeAssignSet, !Info).

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
    type_assign_get_head_type_params(TypeAssign1, HeadTypeParams0),
    list.append(ParentExistQVars, HeadTypeParams0, HeadTypeParams),
    type_assign_set_head_type_params(HeadTypeParams, TypeAssign1, TypeAssign),

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

%-----------------------------------------------------------------------------%

    % Given a list of variables and a list of types, ensure that each variable
    % has the corresponding type.
    %
:- pred typecheck_var_has_arg_type_list(list(prog_var)::in, int::in,
    args_type_assign_set::in,
    typecheck_info::in, typecheck_info::out) is det.

typecheck_var_has_arg_type_list([], _, ArgTypeAssignSet, !Info) :-
    TypeAssignSet =
        convert_args_type_assign_set_check_empty_args(ArgTypeAssignSet),
    !Info ^ tc_info_type_assign_set := TypeAssignSet.

typecheck_var_has_arg_type_list([Var | Vars], ArgNum, ArgTypeAssignSet0,
        !Info) :-
    !Info ^ tc_info_arg_num := ArgNum,
    typecheck_var_has_arg_type(Var, ArgTypeAssignSet0, ArgTypeAssignSet1,
        !Info),
    typecheck_var_has_arg_type_list(Vars, ArgNum + 1, ArgTypeAssignSet1,
        !Info).

:- pred typecheck_var_has_arg_type(prog_var::in,
    args_type_assign_set::in, args_type_assign_set::out,
    typecheck_info::in, typecheck_info::out) is det.

typecheck_var_has_arg_type(Var, ArgTypeAssignSet0, ArgTypeAssignSet, !Info) :-
    typecheck_var_has_arg_type_2(ArgTypeAssignSet0,
        Var, [], ArgTypeAssignSet1),
    (
        ArgTypeAssignSet1 = [],
        ArgTypeAssignSet0 = [_ | _]
    ->
        skip_arg(ArgTypeAssignSet0, ArgTypeAssignSet),
        Spec = report_error_arg_var(!.Info, Var, ArgTypeAssignSet0),
        typecheck_info_add_error(Spec, !Info)
    ;
        ArgTypeAssignSet = ArgTypeAssignSet1
    ).

:- pred skip_arg(args_type_assign_set::in, args_type_assign_set::out) is det.

skip_arg([], []).
skip_arg([ArgTypeAssign0 | ArgTypeAssigns0],
        [ArgTypeAssign | ArgTypeAssigns]) :-
    ArgTypeAssign0 = args_type_assign(TypeAssign, Args0, Constraints),
    (
        Args0 = [_ | Args]
    ;
        Args0 = [],
        % this should never happen
        unexpected($module, $pred, "skip_arg")
    ),
    ArgTypeAssign = args_type_assign(TypeAssign, Args, Constraints),
    skip_arg(ArgTypeAssigns0, ArgTypeAssigns).

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
            (
                type_assign_unify_type(TypeAssign0, OldVarType, Type,
                    TypeAssign1)
            ->
                NewTypeAssign =
                    args_type_assign(TypeAssign1, ArgTypes, ClassContext),
                !:ArgTypeAssignSet = [NewTypeAssign | !.ArgTypeAssignSet]
            ;
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
        unexpected($module, $pred, "ArgTypes0 = []")
    ).

:- pred type_assign_var_has_one_of_these_types(type_assign::in,
    prog_var::in, mer_type::in, mer_type::in, type_assign_set::in,
    type_assign_set::out) is det.

type_assign_var_has_one_of_these_types(TypeAssign0, Var, TypeA, TypeB,
        !TypeAssignSet) :-
    type_assign_get_var_types(TypeAssign0, VarTypes0),
    ( search_var_type(VarTypes0, Var, VarType) ->
        ( type_assign_unify_type(TypeAssign0, VarType, TypeA, TypeAssignA) ->
            !:TypeAssignSet = [TypeAssignA | !.TypeAssignSet]
        ;
            !:TypeAssignSet = !.TypeAssignSet
        ),
        ( type_assign_unify_type(TypeAssign0, VarType, TypeB, TypeAssignB) ->
            !:TypeAssignSet = [TypeAssignB | !.TypeAssignSet]
        ;
            !:TypeAssignSet = !.TypeAssignSet
        )
    ;
        add_var_type(Var, TypeA, VarTypes0, VarTypesA),
        type_assign_set_var_types(VarTypesA, TypeAssign0, TypeAssignA),
        add_var_type(Var, TypeB, VarTypes0, VarTypesB),
        type_assign_set_var_types(VarTypesB, TypeAssign0, TypeAssignB),
        !:TypeAssignSet = [TypeAssignA, TypeAssignB | !.TypeAssignSet]
    ).

%-----------------------------------------------------------------------------%

    % Given a list of variables and a list of types, ensure
    % that each variable has the corresponding type.
    %
:- pred typecheck_var_has_type_list(list(prog_var)::in, list(mer_type)::in,
    int::in, typecheck_info::in, typecheck_info::out) is det.

typecheck_var_has_type_list([], [_ | _], _, !Info) :-
    unexpected($module, $pred, "length mismatch").
typecheck_var_has_type_list([_ | _], [], _, !Info) :-
    unexpected($module, $pred, "length mismatch").
typecheck_var_has_type_list([], [], _, !Info).
typecheck_var_has_type_list([Var | Vars], [Type | Types], ArgNum, !Info) :-
    !Info ^ tc_info_arg_num := ArgNum,
    typecheck_var_has_type(Var, Type, !Info),
    typecheck_var_has_type_list(Vars, Types, ArgNum + 1, !Info).

:- pred typecheck_var_has_type(prog_var::in, mer_type::in,
    typecheck_info::in, typecheck_info::out) is det.

typecheck_var_has_type(Var, Type, !Info) :-
    TypeAssignSet0 = !.Info ^ tc_info_type_assign_set,
    typecheck_var_has_type_2(TypeAssignSet0, Var, Type, [], TypeAssignSet),
    (
        TypeAssignSet = [],
        TypeAssignSet0 = [_ | _]
    ->
        Spec = report_error_var(!.Info, Var, Type, TypeAssignSet0),
        typecheck_info_add_error(Spec, !Info)
    ;
        !Info ^ tc_info_type_assign_set := TypeAssignSet
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
        ( type_assign_unify_type(TypeAssign0, OldVarType, Type, TypeAssign1) ->
            !:TypeAssignSet = [TypeAssign1 | !.TypeAssignSet]
        ;
            !:TypeAssignSet = !.TypeAssignSet
        )
    ;
        MaybeOldVarType = no,
        type_assign_set_var_types(VarTypes, TypeAssign0, TypeAssign),
        !:TypeAssignSet = [TypeAssign | !.TypeAssignSet]
    ).

%-----------------------------------------------------------------------------%

    % type_assign_var_has_type_list(Vars, Types, TypeAssign, Info,
    %       TypeAssignSet0, TypeAssignSet):
    % Let TAs = { TA | TA is an extension of TypeAssign
    %       for which the types of the Vars unify with
    %       their respective Types },
    % list.append(TAs, TypeAssignSet0, TypeAssignSet).
    %
:- pred type_assign_var_has_type_list(list(prog_var)::in, list(mer_type)::in,
    type_assign::in, typecheck_info::in,
    type_assign_set::in, type_assign_set::out) is det.

type_assign_var_has_type_list([], [_ | _], _, _, _, _) :-
    unexpected($module, $pred, "length mismatch").
type_assign_var_has_type_list([_ | _], [], _, _, _, _) :-
    unexpected($module, $pred, "length mismatch").
type_assign_var_has_type_list([], [], TypeAssign, _,
        TypeAssignSet, [TypeAssign | TypeAssignSet]).
type_assign_var_has_type_list([Arg | Args], [Type | Types], TypeAssign0,
        Info, TypeAssignSet0, TypeAssignSet) :-
    type_assign_var_has_type(TypeAssign0, Arg, Type, [], TypeAssignSet1),
    type_assign_list_var_has_type_list(TypeAssignSet1,
        Args, Types, Info, TypeAssignSet0, TypeAssignSet).

    % type_assign_list_var_has_type_list(TAs, Terms, Types,
    %       Info, TypeAssignSet0, TypeAssignSet):
    % Let TAs2 = { TA | TA is an extension of a member of TAs
    %       for which the types of the Terms unify with
    %       their respective Types },
    % list.append(TAs, TypeAssignSet0, TypeAssignSet).
    %
:- pred type_assign_list_var_has_type_list(type_assign_set::in,
    list(prog_var)::in, list(mer_type)::in, typecheck_info::in,
    type_assign_set::in, type_assign_set::out) is det.

type_assign_list_var_has_type_list([], _, _, _, !TypeAssignSet).
type_assign_list_var_has_type_list([TA | TAs], Args, Types, Info,
        !TypeAssignSet) :-
    type_assign_var_has_type_list(Args, Types, TA, Info, !TypeAssignSet),
    type_assign_list_var_has_type_list(TAs, Args, Types, Info, !TypeAssignSet).

%-----------------------------------------------------------------------------%

    % Type check a unification.
    % Get the type assignment set from the type info and then just
    % iterate over all the possible type assignments.
    %
:- pred typecheck_unification(prog_var::in, unify_rhs::in, unify_rhs::out,
    goal_id::in, typecheck_info::in, typecheck_info::out) is det.

typecheck_unification(X, RHS0, RHS, GoalId, !Info) :-
    (
        RHS0 = rhs_var(Y),
        typecheck_unify_var_var(X, Y, !Info),
        RHS = RHS0
    ;
        RHS0 = rhs_functor(Functor, _ExistConstraints, Args),
        typecheck_unify_var_functor(X, Functor, Args, GoalId, !Info),
        perform_context_reduction(!Info),
        RHS = RHS0
    ;
        RHS0 = rhs_lambda_goal(Purity, Groundness, PredOrFunc, EvalMethod,
            NonLocals, Vars, Modes, Det, Goal0),
        typecheck_lambda_var_has_type(Purity, PredOrFunc, EvalMethod, X, Vars,
            !Info),
        typecheck_goal(Goal0, Goal, !Info),
        RHS = rhs_lambda_goal(Purity, Groundness, PredOrFunc, EvalMethod,
            NonLocals, Vars, Modes, Det, Goal)
    ).

:- pred typecheck_unify_var_var(prog_var::in, prog_var::in,
    typecheck_info::in, typecheck_info::out) is det.

typecheck_unify_var_var(X, Y, !Info) :-
    TypeAssignSet0 = !.Info ^ tc_info_type_assign_set,
    typecheck_unify_var_var_2(TypeAssignSet0, X, Y, [], TypeAssignSet),
    (
        TypeAssignSet = [],
        TypeAssignSet0 = [_ | _]
    ->
        Spec = report_error_unif_var_var(!.Info, X, Y, TypeAssignSet0),
        typecheck_info_add_error(Spec, !Info)
    ;
        !Info ^ tc_info_type_assign_set := TypeAssignSet
    ).

:- pred cons_id_must_be_builtin_type(cons_id::in, mer_type::out, string::out)
    is semidet.

cons_id_must_be_builtin_type(ConsId, ConsType, BuiltinTypeName) :-
    (
        ConsId = int_const(_),
        BuiltinTypeName = "int",
        BuiltinType = builtin_type_int
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

:- pred typecheck_unify_var_functor(prog_var::in, cons_id::in,
    list(prog_var)::in, goal_id::in,
    typecheck_info::in, typecheck_info::out) is det.

typecheck_unify_var_functor(Var, ConsId, Args, GoalId, !Info) :-
    ( cons_id_must_be_builtin_type(ConsId, ConsType, BuiltinTypeName) ->
        TypeAssignSet0 = !.Info ^ tc_info_type_assign_set,
        list.foldl(
            type_assign_check_functor_type_builtin(ConsType, Var),
            TypeAssignSet0, [], TypeAssignSet),
        (
            TypeAssignSet = [_ | _],
            !Info ^ tc_info_type_assign_set := TypeAssignSet
        ;
            TypeAssignSet = [],
            % If we encountered an error, continue checking with the
            % original type assign set.
            !Info ^ tc_info_type_assign_set := TypeAssignSet0,
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
                ConsIdSpec = report_error_functor_type(!.Info, Var, [ConsDefn],
                    ConsId, 0, TypeAssignSet0),
                typecheck_info_add_error(ConsIdSpec, !Info)
            )
        )
    ;
        % Get the list of possible constructors that match this functor/arity.
        % If there aren't any, report an undefined constructor error.
        list.length(Args, Arity),
        typecheck_info_get_ctor_list(!.Info, ConsId, Arity, GoalId,
            ConsDefns, ConsErrors),
        (
            ConsDefns = [],
            Spec = report_error_undef_cons(!.Info, ConsErrors, ConsId, Arity),
            typecheck_info_add_error(Spec, !Info)
        ;
            (
                ConsDefns = [_]
            ;
                ConsDefns = [_, _ | _],
                Context = !.Info ^ tc_info_context,
                Sources = list.map(project_cons_type_info_source, ConsDefns),
                Symbol = overloaded_func(ConsId, Sources),
                typecheck_info_add_overloaded_symbol(Symbol, Context, !Info)
            ),

            % Produce the ConsTypeAssignSet, which is essentially the
            % cross-product of the TypeAssignSet0 and the ConsDefnList.
            TypeAssignSet0 = !.Info ^ tc_info_type_assign_set,
            typecheck_unify_var_functor_get_ctors(TypeAssignSet0,
                !.Info, ConsDefns, [], ConsTypeAssignSet),
            (
                ConsTypeAssignSet = [],
                TypeAssignSet0 = [_ | _]
            ->
                % This should never happen, since undefined ctors
                % should be caught by the check just above.
                unexpected($module, $pred, "undefined cons?")
            ;
                true
            ),

            % Check that the type of the functor matches the type of the
            % variable.
            typecheck_functor_type(ConsTypeAssignSet, Var,
                [], ArgsTypeAssignSet),
            (
                ArgsTypeAssignSet = [],
                ConsTypeAssignSet = [_ | _]
            ->
                ConsIdSpec = report_error_functor_type(!.Info, Var, ConsDefns,
                    ConsId, Arity, TypeAssignSet0),
                typecheck_info_add_error(ConsIdSpec, !Info)
            ;
                true
            ),

            % Check that the type of the arguments of the functor matches
            % their expected type for this functor.
            typecheck_functor_arg_types(ArgsTypeAssignSet, Args, !.Info,
                [], TypeAssignSet),
            (
                TypeAssignSet = [_ | _],
                !Info ^ tc_info_type_assign_set := TypeAssignSet
            ;
                TypeAssignSet = [],
                % If we encountered an error, continue checking with the
                % original type assign set.
                !Info ^ tc_info_type_assign_set := TypeAssignSet0,
                (
                    ArgsTypeAssignSet = []
                    % The error did not originate here, so generating an error
                    % message here would be misleading.
                ;
                    ArgsTypeAssignSet = [_ | _],
                    ArgSpec = report_error_functor_arg_types(!.Info, Var,
                        ConsDefns, ConsId, Args, ArgsTypeAssignSet),
                    typecheck_info_add_error(ArgSpec, !Info)
                )
            )
        )
    ).

:- type cons_type
    --->    cons_type(mer_type, list(mer_type)).

:- type cons_type_assign_set == list(pair(type_assign, cons_type)).

    % typecheck_unify_var_functor_get_ctors(TypeAssignSet, Info, ConsDefns):
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
:- pred typecheck_unify_var_functor_get_ctors(type_assign_set::in,
    typecheck_info::in, list(cons_type_info)::in,
    cons_type_assign_set::in, cons_type_assign_set::out) is det.

    % Iterate over the type assign sets.
    %
typecheck_unify_var_functor_get_ctors([], _, _, !ConsTypeAssignSet).
typecheck_unify_var_functor_get_ctors([TypeAssign | TypeAssigns], Info,
        ConsDefns, !ConsTypeAssignSet) :-
    typecheck_unify_var_functor_get_ctors_2(ConsDefns, Info, TypeAssign,
        !ConsTypeAssignSet),
    typecheck_unify_var_functor_get_ctors(TypeAssigns, Info, ConsDefns,
        !ConsTypeAssignSet).

    % Iterate over all the different cons defns.
    %
:- pred typecheck_unify_var_functor_get_ctors_2(list(cons_type_info)::in,
    typecheck_info::in, type_assign::in,
    cons_type_assign_set::in, cons_type_assign_set::out) is det.

typecheck_unify_var_functor_get_ctors_2([], _, _, !ConsTypeAssignSet).
typecheck_unify_var_functor_get_ctors_2([ConsDefn | ConsDefns], Info,
        TypeAssign0, !ConsTypeAssignSet) :-
    get_cons_stuff(ConsDefn, TypeAssign0, Info, ConsType, ArgTypes,
        TypeAssign1),
    ConsTypeAssign = TypeAssign1 - cons_type(ConsType, ArgTypes),
    !:ConsTypeAssignSet = [ConsTypeAssign | !.ConsTypeAssignSet],
    typecheck_unify_var_functor_get_ctors_2(ConsDefns, Info, TypeAssign0,
        !ConsTypeAssignSet).

    % typecheck_functor_type(ConsTypeAssignSet, Var):
    %
    % For each possible cons type assignment in `ConsTypeAssignSet',
    % for each possible constructor type,
    % check that the type of `Var' matches this type.
    %
:- pred typecheck_functor_type(cons_type_assign_set::in, prog_var::in,
    args_type_assign_set::in, args_type_assign_set::out) is det.

typecheck_functor_type([], _, !ArgsTypeAssignSet).
typecheck_functor_type([TypeAssign - ConsType | ConsTypeAssigns], Var,
        !ArgsTypeAssignSet) :-
    ConsType = cons_type(Type, ArgTypes),
    type_assign_check_functor_type(Type, ArgTypes, Var, TypeAssign,
        !ArgsTypeAssignSet),
    typecheck_functor_type(ConsTypeAssigns, Var, !ArgsTypeAssignSet).

    % typecheck_functor_arg_types(ConsTypeAssignSet, Var, Args, ...):
    %
    % For each possible cons type assignment in `ConsTypeAssignSet',
    % for each possible constructor argument types,
    % check that the types of `Args' matches these types.
    %
:- pred typecheck_functor_arg_types(args_type_assign_set::in,
    list(prog_var)::in, typecheck_info::in,
    type_assign_set::in, type_assign_set::out) is det.

typecheck_functor_arg_types([], _, _, !TypeAssignSet).
typecheck_functor_arg_types([ConsTypeAssign | ConsTypeAssigns], Args, Info,
        !TypeAssignSet) :-
    ConsTypeAssign = args_type_assign(TypeAssign, ArgTypes, _),
    type_assign_var_has_type_list(Args, ArgTypes, TypeAssign, Info,
        !TypeAssignSet),
    typecheck_functor_arg_types(ConsTypeAssigns, Args, Info, !TypeAssignSet).

    % Iterate over all the possible type assignments.
    %
:- pred typecheck_unify_var_var_2(type_assign_set::in,
    prog_var::in, prog_var::in,
    type_assign_set::in, type_assign_set::out) is det.

typecheck_unify_var_var_2([], _, _, !TypeAssignSet).
typecheck_unify_var_var_2([TypeAssign0 | TypeAssigns0], X, Y,
        !TypeAssignSet) :-
    type_assign_unify_var_var(X, Y, TypeAssign0, !TypeAssignSet),
    typecheck_unify_var_var_2(TypeAssigns0, X, Y, !TypeAssignSet).

%-----------------------------------------------------------------------------%

    % Type-check the unification of two variables,
    % and update the type assignment.
    % TypeAssign0 is the type assignment we are updating,
    % TypeAssignSet0 is an accumulator for the list of possible
    % type assignments so far, and TypeAssignSet is TypeAssignSet plus
    % any type assignment(s) resulting from TypeAssign0 and this
    % unification.
    %
:- pred type_assign_unify_var_var(prog_var::in, prog_var::in, type_assign::in,
    type_assign_set::in, type_assign_set::out) is det.

type_assign_unify_var_var(X, Y, TypeAssign0, !TypeAssignSet) :-
    type_assign_get_var_types(TypeAssign0, VarTypes0),
    ( search_var_type(VarTypes0, X, TypeX) ->
        search_insert_var_type(Y, TypeX, MaybeTypeY, VarTypes0, VarTypes),
        (
            MaybeTypeY = yes(TypeY),
            % Both X and Y already have types - just unify their types.
            ( type_assign_unify_type(TypeAssign0, TypeX, TypeY, TypeAssign3) ->
                !:TypeAssignSet = [TypeAssign3 | !.TypeAssignSet]
            ;
                !:TypeAssignSet = !.TypeAssignSet
            )
        ;
            MaybeTypeY = no,
            type_assign_set_var_types(VarTypes, TypeAssign0, TypeAssign),
            !:TypeAssignSet = [TypeAssign | !.TypeAssignSet]
        )
    ;
        ( search_var_type(VarTypes0, Y, TypeY) ->
            % X is a fresh variable which hasn't been assigned a type yet.
            add_var_type(X, TypeY, VarTypes0, VarTypes),
            type_assign_set_var_types(VarTypes, TypeAssign0, TypeAssign),
            !:TypeAssignSet = [TypeAssign | !.TypeAssignSet]
        ;
            % Both X and Y are fresh variables - introduce a fresh type
            % variable with kind `star' to represent their type.
            type_assign_get_typevarset(TypeAssign0, TypeVarSet0),
            varset.new_var(TypeVar, TypeVarSet0, TypeVarSet),
            type_assign_set_typevarset(TypeVarSet, TypeAssign0, TypeAssign1),
            Type = type_variable(TypeVar, kind_star),
            add_var_type(X, Type, VarTypes0, VarTypes1),
            ( X \= Y ->
                add_var_type(Y, Type, VarTypes1, VarTypes)
            ;
                VarTypes = VarTypes1
            ),
            type_assign_set_var_types(VarTypes, TypeAssign1, TypeAssign),
            !:TypeAssignSet = [TypeAssign | !.TypeAssignSet]
        )
    ).

%-----------------------------------------------------------------------------%

:- pred type_assign_check_functor_type(mer_type::in, list(mer_type)::in,
    prog_var::in, type_assign::in,
    args_type_assign_set::in, args_type_assign_set::out) is det.

type_assign_check_functor_type(ConsType, ArgTypes, Y, TypeAssign0,
        !ArgsTypeAssignSet) :-
    % Unify the type of Var with the type of the constructor.
    type_assign_get_var_types(TypeAssign0, VarTypes0),
    search_insert_var_type(Y, ConsType, MaybeTypeY, VarTypes0, VarTypes),
    (
        MaybeTypeY = yes(TypeY),
        ( type_assign_unify_type(TypeAssign0, ConsType, TypeY, TypeAssign) ->
            % The constraints are empty here because none are added by
            % unification with a functor.
            empty_hlds_constraints(EmptyConstraints),
            ArgsTypeAssign =
                args_type_assign(TypeAssign, ArgTypes, EmptyConstraints),
            !:ArgsTypeAssignSet = [ArgsTypeAssign | !.ArgsTypeAssignSet]
        ;
            true
        )
    ;
        MaybeTypeY = no,
        % The constraints are empty here because none are added by
        % unification with a functor.
        type_assign_set_var_types(VarTypes, TypeAssign0, TypeAssign),
        empty_hlds_constraints(EmptyConstraints),
        ArgsTypeAssign =
            args_type_assign(TypeAssign, ArgTypes, EmptyConstraints),
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
        ( type_assign_unify_type(TypeAssign0, ConsType, TypeY, TypeAssign) ->
            % The constraints are empty here because none are added by
            % unification with a functor.
            !:TypeAssignSet = [TypeAssign | !.TypeAssignSet]
        ;
            true
        )
    ;
        MaybeTypeY = no,
        % The constraints are empty here because none are added by
        % unification with a functor.
        type_assign_set_var_types(VarTypes, TypeAssign0, TypeAssign),
        !:TypeAssignSet = [TypeAssign | !.TypeAssignSet]
    ).

%-----------------------------------------------------------------------------%

    % Given an cons_type_info, construct a type for the constructor
    % and a list of types of the arguments, suitable renamed apart
    % from the current type_assign's typevarset.
    %
:- pred get_cons_stuff(cons_type_info::in, type_assign::in, typecheck_info::in,
    mer_type::out, list(mer_type)::out, type_assign::out) is det.

get_cons_stuff(ConsDefn, TypeAssign0, _Info, ConsType, ArgTypes, TypeAssign) :-
    ConsDefn = cons_type_info(ConsTypeVarSet, ConsExistQVars0,
        ConsType0, ArgTypes0, ClassConstraints0, _Source),

    % Rename apart the type vars in the type of the constructor
    % and the types of its arguments.
    % (Optimize the common case of a non-polymorphic type.)
    (
        varset.is_empty(ConsTypeVarSet)
    ->
        ConsType = ConsType0,
        ArgTypes = ArgTypes0,
        TypeAssign2 = TypeAssign0,
        ConstraintsToAdd = ClassConstraints0
    ;
        type_assign_rename_apart(TypeAssign0, ConsTypeVarSet,
            [ConsType0 | ArgTypes0], TypeAssign1, [ConsType1 | ArgTypes1],
            Renaming)
    ->
        apply_variable_renaming_to_tvar_list(Renaming,
            ConsExistQVars0, ConsExistQVars),
        apply_variable_renaming_to_constraints(Renaming,
            ClassConstraints0, ConstraintsToAdd),
        type_assign_get_head_type_params(TypeAssign1, HeadTypeParams0),
        HeadTypeParams = ConsExistQVars ++ HeadTypeParams0,
        type_assign_set_head_type_params(HeadTypeParams,
            TypeAssign1, TypeAssign2),

        ConsType = ConsType1,
        ArgTypes = ArgTypes1
    ;
        unexpected($module, $pred, "type_assign_rename_apart failed")
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
    type_assign_set_typeclass_constraints(ClassConstraints, TypeAssign2,
        TypeAssign).

:- pred apply_substitution_to_var_list(list(var(T))::in, map(var(T),
    term(T))::in, list(var(T))::out) is det.

apply_substitution_to_var_list(Vars0, RenameSubst, Vars) :-
    term.var_list_to_term_list(Vars0, Terms0),
    term.apply_substitution_to_list(Terms0, RenameSubst, Terms),
    Vars = term.term_list_to_var_list(Terms).

:- pred apply_var_renaming_to_var_list(list(var(T))::in, map(var(T),
    var(T))::in, list(var(T))::out) is det.

apply_var_renaming_to_var_list(Vars0, RenameSubst, Vars) :-
    list.map(apply_var_renaming_to_var(RenameSubst), Vars0, Vars).

:- pred apply_var_renaming_to_var(map(var(T), var(T))::in, var(T)::in,
    var(T)::out) is det.

apply_var_renaming_to_var(RenameSubst, Var0, Var) :-
    ( map.search(RenameSubst, Var0, Var1) ->
        Var = Var1
    ;
        Var = Var0
    ).

%-----------------------------------------------------------------------------%

    % typecheck_lambda_var_has_type(Var, ArgVars, ...):
    %
    % Check that `Var' has type `pred(T1, T2, ...)' where T1, T2, ...
    % are the types of the `ArgVars'.
    %
:- pred typecheck_lambda_var_has_type(purity::in, pred_or_func::in,
    lambda_eval_method::in, prog_var::in, list(prog_var)::in,
    typecheck_info::in, typecheck_info::out) is det.

typecheck_lambda_var_has_type(Purity, PredOrFunc, EvalMethod, Var, ArgVars,
        !Info) :-
    TypeAssignSet0 = !.Info ^ tc_info_type_assign_set,
    typecheck_lambda_var_has_type_2(TypeAssignSet0, Purity, PredOrFunc,
        EvalMethod, Var, ArgVars, [], TypeAssignSet),
    (
        TypeAssignSet = [],
        TypeAssignSet0 = [_ | _]
    ->
        Spec = report_error_lambda_var(!.Info, PredOrFunc, EvalMethod,
            Var, ArgVars, TypeAssignSet0),
        typecheck_info_add_error(Spec, !Info)
    ;
        !Info ^ tc_info_type_assign_set := TypeAssignSet
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
    ( search_var_type(VarTypes0, Var, VarType) ->
        % If so, use that type.
        Type = VarType
    ;
        % Otherwise, introduce a fresh type variable with kind `star' to use
        % as the type of that variable.
        type_assign_get_typevarset(!.TypeAssign, TypeVarSet0),
        varset.new_var(TypeVar, TypeVarSet0, TypeVarSet),
        type_assign_set_typevarset(TypeVarSet, !TypeAssign),
        Type = type_variable(TypeVar, kind_star),
        add_var_type(Var, Type, VarTypes0, VarTypes1),
        type_assign_set_var_types(VarTypes1, !TypeAssign)
    ),
    % Recursively process the rest of the variables.
    type_assign_get_types_of_vars(Vars, Types, !TypeAssign).

%-----------------------------------------------------------------------------%

    % Unify (with occurs check) two types in a type assignment
    % and update the type bindings.
    %
:- pred type_assign_unify_type(type_assign::in, mer_type::in, mer_type::in,
    type_assign::out) is semidet.

type_assign_unify_type(TypeAssign0, X, Y, TypeAssign) :-
    type_assign_get_head_type_params(TypeAssign0, HeadTypeParams),
    type_assign_get_type_bindings(TypeAssign0, TypeBindings0),
    type_unify(X, Y, HeadTypeParams, TypeBindings0, TypeBindings),
    type_assign_set_type_bindings(TypeBindings, TypeAssign0, TypeAssign).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

    % builtin_atomic_type(Const, TypeName):
    %
    % If Const is *or can be* a constant of a builtin atomic type,
    % set TypeName to the name of that type, otherwise fail.
    %
:- pred builtin_atomic_type(cons_id::in, string::out) is semidet.

builtin_atomic_type(int_const(_), "int").
builtin_atomic_type(float_const(_), "float").
builtin_atomic_type(char_const(_), "character").
builtin_atomic_type(string_const(_), "string").
builtin_atomic_type(cons(unqualified(String), 0, _), "character") :-
    % We are before post-typecheck, so character constants have not yet been
    % converted to char_consts.
    %
    % XXX We cannot use "term_io.string_is_escaped_char(_, String)"
    % because the characters in String have already had any backslash
    % characters in them processed by now.
    %
    % XXX The parser should have a separate term.functor representation
    % for character constants, which should be converted to char_consts
    % during the term to item translation.
    string.char_to_string(_, String).
builtin_atomic_type(impl_defined_const(Name), Type) :-
    (
        ( Name = "file"
        ; Name = "module"
        ; Name = "pred"
        ; Name = "grade"
        ),
        Type = "string"
    ;
        Name = "line",
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
    ModuleInfo = Info ^ tc_info_module_info,
    module_info_get_predicate_table(ModuleInfo, PredicateTable),
    typecheck_info_get_pred_markers(Info, PredMarkers),
    IsFullyQualified = calls_are_fully_qualified(PredMarkers),
    predicate_table_lookup_sym(PredicateTable, IsFullyQualified, SymName,
        PredIds),
    (
        PredIds = [_ | _],
        predicate_table_get_preds(PredicateTable, Preds),
        make_pred_cons_info_list(Info, PredIds, Preds, Arity, GoalId,
            [], ConsTypeInfos)
    ;
        PredIds = [],
        ConsTypeInfos = []
    ).

:- pred make_pred_cons_info_list(typecheck_info::in, list(pred_id)::in,
    pred_table::in, int::in, goal_id::in,
    list(cons_type_info)::in, list(cons_type_info)::out) is det.

make_pred_cons_info_list(_, [], _, _, _, !ConsTypeInfos).
make_pred_cons_info_list(Info, [PredId | PredIds], PredTable, Arity,
        GoalId, !ConsTypeInfos) :-
    make_pred_cons_info(Info, PredId, PredTable, Arity,
        GoalId, !ConsTypeInfos),
    make_pred_cons_info_list(Info, PredIds, PredTable, Arity,
        GoalId, !ConsTypeInfos).

:- pred make_pred_cons_info(typecheck_info::in, pred_id::in, pred_table::in,
    int::in, goal_id::in,
    list(cons_type_info)::in, list(cons_type_info)::out) is det.

make_pred_cons_info(Info, PredId, PredTable, FuncArity, GoalId, !ConsInfos) :-
    ModuleInfo = Info ^ tc_info_module_info,
    module_info_get_class_table(ModuleInfo, ClassTable),
    map.lookup(PredTable, PredId, PredInfo),
    PredArity = pred_info_orig_arity(PredInfo),
    IsPredOrFunc = pred_info_is_pred_or_func(PredInfo),
    pred_info_get_class_context(PredInfo, PredClassContext),
    pred_info_get_arg_types(PredInfo, PredTypeVarSet, PredExistQVars,
        CompleteArgTypes),
    pred_info_get_purity(PredInfo, Purity),
    (
        IsPredOrFunc = pf_predicate,
        PredArity >= FuncArity,
        % We don't support first-class polymorphism, so you can't take the
        % address of an existentially quantified predicate.
        PredExistQVars = []
    ->
        (
            list.split_list(FuncArity, CompleteArgTypes,
                ArgTypes, PredTypeParams)
        ->
            construct_higher_order_pred_type(Purity, lambda_normal,
                PredTypeParams, PredType),
            make_body_hlds_constraints(ClassTable, PredTypeVarSet,
                GoalId, PredClassContext, PredConstraints),
            ConsInfo = cons_type_info(PredTypeVarSet, PredExistQVars,
                PredType, ArgTypes, PredConstraints, source_pred(PredId)),
            !:ConsInfos = [ConsInfo | !.ConsInfos]
        ;
            unexpected($module, $pred, "split_list failed")
        )
    ;
        IsPredOrFunc = pf_function,
        PredAsFuncArity = PredArity - 1,
        PredAsFuncArity >= FuncArity,
        % We don't support first-class polymorphism, so you can't take
        % the address of an existentially quantified function. You can however
        % call such a function, so long as you pass *all* the parameters.
        ( PredExistQVars = []
        ; PredAsFuncArity = FuncArity
        )
    ->
        (
            list.split_list(FuncArity, CompleteArgTypes,
                FuncArgTypes, FuncTypeParams),
            pred_args_to_func_args(FuncTypeParams,
                FuncArgTypeParams, FuncReturnTypeParam)
        ->
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
            ConsInfo = cons_type_info(PredTypeVarSet,
                PredExistQVars, FuncType, FuncArgTypes, PredConstraints,
                source_pred(PredId)),
            !:ConsInfos = [ConsInfo | !.ConsInfos]
        ;
            unexpected($module, $pred, "split_list failed")
        )
    ;
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
    ModuleInfo = Info ^ tc_info_module_info,
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
    ctor_field_name::in, hlds_ctor_field_defn::in,
    maybe_cons_type_info::out) is semidet.

make_field_access_function_cons_type_info(Info, GoalId, FuncName, Arity,
        AccessType, FieldName, FieldDefn, ConsTypeInfo) :-
    get_field_access_constructor(Info, GoalId, FuncName, Arity,
        AccessType, FieldDefn, OrigExistTVars,
        MaybeFunctorConsTypeInfo),
    (
        MaybeFunctorConsTypeInfo = ok(FunctorConsTypeInfo),
        ModuleInfo = Info ^ tc_info_module_info,
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

    ModuleInfo = Info ^ tc_info_module_info,
    module_info_get_predicate_table(ModuleInfo, PredTable),
    UnqualFuncName = unqualify_name(FuncName),
    typecheck_info_get_is_field_access_function(Info, IsFieldAccessFunc),
    (
        IsFieldAccessFunc = no,
        predicate_table_lookup_func_m_n_a(PredTable, is_fully_qualified,
            TypeModule, UnqualFuncName, Arity, PredIds),
        PredIds = []
    ;
        IsFieldAccessFunc = yes
    ),
    module_info_get_cons_table(ModuleInfo, Ctors),
    lookup_cons_table_of_type_ctor(Ctors, TypeCtor, ConsId, ConsDefn),
    (
        AccessType = get,
        ConsAction = do_not_flip_constraints
    ;
        AccessType = set,
        ConsAction = flip_constraints_for_field_set
    ),
    OrigExistTVars = ConsDefn ^ cons_exist_tvars,
    convert_cons_defn(Info, GoalId, ConsAction, ConsDefn, FunctorConsTypeInfo).

:- type maybe_cons_type_info
    --->    ok(cons_type_info)
    ;       error(cons_error).

:- pred convert_field_access_cons_type_info(class_table::in,
    field_access_type::in, ctor_field_name::in, hlds_ctor_field_defn::in,
    cons_type_info::in, existq_tvars::in, maybe_cons_type_info::out) is det.

convert_field_access_cons_type_info(ClassTable, AccessType, FieldName,
        FieldDefn, FunctorConsTypeInfo, OrigExistTVars, ConsTypeInfo) :-
    FunctorConsTypeInfo = cons_type_info(TVarSet0, ExistQVars,
        FunctorType, ConsArgTypes, Constraints0, Source0),
    (
        Source0 = source_type(SourceTypePrime),
        SourceType = SourceTypePrime
    ;
        ( Source0 = source_builtin_type(_)
        ; Source0 = source_get_field_access(_)
        ; Source0 = source_set_field_access(_)
        ; Source0 = source_apply(_)
        ; Source0 = source_pred(_)
        ),
        unexpected($module, $pred, "not type")
    ),
    FieldDefn = hlds_ctor_field_defn(_, _, _, _, FieldNumber),
    list.det_index1(ConsArgTypes, FieldNumber, FieldType),
    (
        AccessType = get,
        Source = source_get_field_access(SourceType),
        RetType = FieldType,
        ArgTypes = [FunctorType],
        TVarSet = TVarSet0,
        Constraints = Constraints0,
        ConsTypeInfo = ok(cons_type_info(TVarSet, ExistQVars,
            RetType, ArgTypes, Constraints, Source))
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
        (
            TVarsInField = [],
            TVarSet = TVarSet0,
            RetType = FunctorType,
            ArgTypes = [FunctorType, FieldType],

            % None of the constraints are affected by the updated field,
            % so the constraints are unchanged.
            Constraints = Constraints0,

            ConsTypeInfo = ok(cons_type_info(TVarSet, ExistQVars,
                RetType, ArgTypes, Constraints, Source))
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
            ( set.empty(ExistQVarsInFieldAndOthers) ->
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
                % onto the set of type variables occuring in the types of the
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
            ;
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
    Constraint = hlds_constraint(_, _, TypesToCheck),
    type_vars_list(TypesToCheck, TVarsToCheck0),
    set.list_to_set(TVarsToCheck0, TVarsToCheck),
    set.intersect(TVarsToCheck, CallTVars, RelevantTVars),
    \+ set.empty(RelevantTVars).

:- pred rename_constraint(tvar_renaming::in, hlds_constraint::in,
    hlds_constraint::out) is semidet.

rename_constraint(TVarRenaming, Constraint0, Constraint) :-
    Constraint0 = hlds_constraint(Ids, Name, Types0),
    some [Var] (
        type_list_contains_var(Types0, Var),
        map.contains(TVarRenaming, Var)
    ),
    apply_variable_renaming_to_type_list(TVarRenaming, Types0, Types),
    Constraint = hlds_constraint(Ids, Name, Types).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

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
    (
        % If we're typechecking the clause added for a field access function
        % for which the user has supplied type or mode declarations, the goal
        % should only contain an application of the field access function,
        % not constructor applications or function calls. The clauses in
        % `.opt' files will already have been expanded into unifications.
        IsFieldAccessFunc = yes,
        typecheck_info_get_pred_import_status(Info, ImportStatus),
        ImportStatus \= status_opt_imported
    ->
        (
            builtin_field_access_function_type(Info, GoalId,
                Functor, Arity, FieldAccessConsInfos)
        ->
            split_cons_errors(FieldAccessConsInfos, ConsInfos, ConsErrors)
        ;
            ConsInfos = [],
            ConsErrors = []
        )
    ;
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
    typecheck_info_get_ctors(Info, Ctors),
    (
        Functor = cons(_, _, _),
        search_cons_table(Ctors, Functor, HLDS_ConsDefns)
    ->
        convert_cons_defn_list(Info, GoalId, do_not_flip_constraints,
            HLDS_ConsDefns, PlainMaybeConsInfos)
    ;
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
    (
        Functor = cons(Name, Arity, FunctorTypeCtor),
        remove_new_prefix(Name, OrigName),
        OrigFunctor = cons(OrigName, Arity, FunctorTypeCtor),
        search_cons_table(Ctors, OrigFunctor, HLDS_ExistQConsDefns)
    ->
        convert_cons_defn_list(Info, GoalId, flip_constraints_for_new,
            HLDS_ExistQConsDefns, UnivQuantifiedMaybeConsInfos)
    ;
        UnivQuantifiedMaybeConsInfos = []
    ),

    % Check if Functor is a field access function for which the user
    % has not supplied a declaration.
    (
        builtin_field_access_function_type(Info, GoalId, Functor,
            Arity, FieldAccessMaybeConsInfosPrime)
    ->
        FieldAccessMaybeConsInfos = FieldAccessMaybeConsInfosPrime
    ;
        FieldAccessMaybeConsInfos = []
    ),

    DataMaybeConsInfos = PlainMaybeConsInfos ++ UnivQuantifiedMaybeConsInfos
        ++ FieldAccessMaybeConsInfos,
    split_cons_errors(DataMaybeConsInfos, DataConsInfos, DataConsErrors),

    % Check if Functor is a constant of one of the builtin atomic types
    % (string, float, int, character). If so, insert the resulting
    % cons_type_info at the start of the list.
    (
        Arity = 0,
        builtin_atomic_type(Functor, BuiltInTypeName)
    ->
        TypeCtor = type_ctor(unqualified(BuiltInTypeName), 0),
        construct_type(TypeCtor, [], ConsType),
        varset.init(ConsTypeVarSet),
        ConsInfo = cons_type_info(ConsTypeVarSet, [], ConsType, [],
            EmptyConstraints, source_builtin_type(BuiltInTypeName)),
        BuiltinConsInfos = [ConsInfo]
    ;
        BuiltinConsInfos = []
    ),

    % Check if Functor is a tuple constructor.
    (
        ( Functor = cons(unqualified("{}"), TupleArity, _)
        ; Functor = tuple_cons(TupleArity)
        )
    ->
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
    ;
        TupleConsInfos = []
    ),

    % Check if Functor is the name of a predicate which takes at least
    % Arity arguments. If so, insert the resulting cons_type_info
    % at the start of the list.
    ( builtin_pred_type(Info, Functor, Arity, GoalId, PredConsInfosPrime) ->
        PredConsInfos = PredConsInfosPrime
    ;
        PredConsInfos = []
    ),

    % Check for higher-order function calls.
    ( builtin_apply_type(Info, Functor, Arity, ApplyConsInfosPrime) ->
        ApplyConsInfos = ApplyConsInfosPrime
    ;
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

%-----------------------------------------------------------------------------%

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
        ConsTypeKinds, ExistQVars0, ExistProgConstraints, Args, _),
    ArgTypes = list.map(func(C) = C ^ arg_type, Args),
    typecheck_info_get_types(Info, TypeTable),
    lookup_type_ctor_defn(TypeTable, TypeCtor, TypeDefn),
    hlds_data.get_type_defn_body(TypeDefn, Body),

    % If this type has `:- pragma foreign_type' declarations, we
    % can only use its constructors in predicates which have foreign
    % clauses and in the unification and comparison predicates for
    % the type (otherwise the code wouldn't compile when using a
    % back-end which caused another version of the type to be selected).
    % The constructors may also appear in the automatically generated
    % unification and comparison predicates.
    %
    % XXX This check isn't quite right -- we really need to check for
    % each procedure that there is a foreign_proc declaration for all
    % languages for which this type has a foreign_type declaration, but
    % this will do for now. Such a check may be difficult because by
    % this point we've thrown away the clauses which we aren't using
    % in the current compilation.
    %
    % The `.opt' files don't contain the foreign clauses from the source
    % file that aren't used when compiling in the current grade, so we
    % allow foreign type constructors in `opt_imported' predicates even
    % if there are no foreign clauses. Errors will be caught when creating
    % the `.opt' file.

    typecheck_info_get_predid(Info, PredId),
    ModuleInfo = Info ^ tc_info_module_info,
    module_info_get_class_table(ModuleInfo, ClassTable),
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    (
        Body ^ du_type_is_foreign_type = yes(_),
        \+ pred_info_get_goal_type(PredInfo, goal_type_clause_and_foreign),
        \+ is_unify_or_compare_pred(PredInfo),
        \+ pred_info_get_import_status(PredInfo, status_opt_imported)
    ->
        ConsTypeInfo = error(foreign_type_constructor(TypeCtor, TypeDefn))
    ;
        % Do not allow constructors for abstract_imported types unless
        % the current predicate is opt_imported.
        hlds_data.get_type_defn_status(TypeDefn, status_abstract_imported),
        \+ is_unify_or_compare_pred(PredInfo),
        \+ pred_info_get_import_status(PredInfo, status_opt_imported)
    ->
        ConsTypeInfo = error(abstract_imported_type)
    ;
        Action = flip_constraints_for_new,
        ExistQVars0 = []
    ->
        % Do not allow 'new' constructors except on existential types.
        ConsTypeInfo = error(new_on_non_existential_type(TypeCtor))
    ;
        prog_type.var_list_to_type_list(ConsTypeKinds, ConsTypeParams,
            ConsTypeArgs),
        construct_type(TypeCtor, ConsTypeArgs, ConsType),
        UnivProgConstraints = [],
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
        make_body_hlds_constraints(ClassTable, ConsTypeVarSet,
            GoalId, ProgConstraints, Constraints),
        ConsTypeInfo = ok(cons_type_info(ConsTypeVarSet, ExistQVars,
            ConsType, ArgTypes, Constraints, source_type(TypeCtor)))
    ).

%-----------------------------------------------------------------------------%
:- end_module check_hlds.typecheck.
%-----------------------------------------------------------------------------%
