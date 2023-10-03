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
% 1 Discriminated union types, such as
%   :- type tree(T) ---> nil ; t(tree(T), T, tree(T)).
%
% 2 Equivalence types, such as
%   :- type real == float.
%   Any number of types can be equivalent; the *canonical* one is the one
%   which is not defined using "==".
%
%   Currently references to equivalence types are expanded in a separate pass
%   by mercury_compile_front_end.m. It would be better to avoid expanding them
%   (and instead modify the type unification algorithm to handle equivalent
%   types) because this would give better error messages. However, this is
%   not a high priority.
%
% 3 Higher-order predicate and function types
%   pred, pred(T), pred(T1, T2), pred(T1, T2, T3), ...
%   func(T1) = T2, func(T1, T2) = T3, ...
%
% 4 Builtin types, such as
%   character, int, float, string.
%   These types have special syntax for constants. There may be other types
%   (list(T), unit, univ, etc.) provided by the system, but they can just be
%   part of the standard library.
%
% Each exported predicate must have a `:- pred' declaration specifying the
% types of the arguments for that predicate. For predicates that are
% local to a module, we can infer the types.
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
:- import_module hlds.hlds_clauses.
:- import_module hlds.hlds_module.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.

:- import_module io.
:- import_module list.

%---------------------------------------------------------------------------%

:- type number_of_iterations
    --->    within_iteration_limit
    ;       exceeded_iteration_limit.

    % typecheck_module(ProgressStream, !ModuleInfo, Specs, FoundSyntaxError,
    %   NumberOfIterations):
    %
    % Type checks ModuleInfo and annotates it with variable type information.
    % Specs is set to the list of errors and warnings found, plus messages
    % about the predicates and functions whose types have been inferred.
    % We set FoundSyntaxError to `some_clause_syntax_errors' if some of
    % the clauses in the typechecked predicates contained syntax errors.
    % We set NumberOfIterations to `exceeded_iteration_limit'
    % iff the type inference iteration limit was reached.
    %
:- pred typecheck_module(io.text_output_stream::in,
    module_info::in, module_info::out, list(error_spec)::out,
    maybe_clause_syntax_errors::out, number_of_iterations::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.type_assign.
:- import_module check_hlds.typecheck_clauses.
:- import_module check_hlds.typecheck_error_undef.
:- import_module check_hlds.typecheck_errors.
:- import_module check_hlds.typecheck_info.
:- import_module check_hlds.typecheck_msgs.
:- import_module check_hlds.typeclasses.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_args.
:- import_module hlds.hlds_class.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_error_util.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_rtti.
:- import_module hlds.instmap.
:- import_module hlds.make_goal.
:- import_module hlds.passes_aux.
:- import_module hlds.pred_name.
:- import_module hlds.pred_table.
:- import_module hlds.special_pred.
:- import_module hlds.status.
:- import_module hlds.var_table_hlds.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.error_util.
:- import_module parse_tree.file_names.         % undesirable dependency
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_subst.
:- import_module parse_tree.var_table.
:- import_module parse_tree.vartypes.
:- import_module parse_tree.write_error_spec.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module set_tree234.
:- import_module string.
:- import_module term_context.
:- import_module term_subst.
:- import_module varset.

%---------------------------------------------------------------------------%

typecheck_module(ProgressStream, !ModuleInfo, Specs, FoundSyntaxError,
        NumberOfIterations) :-
    module_info_get_globals(!.ModuleInfo, Globals),
    globals.lookup_int_option(Globals, type_inference_iteration_limit,
        MaxIterations),

    module_info_get_valid_pred_id_set(!.ModuleInfo, OrigValidPredIdSet),
    OrigValidPredIds = set_tree234.to_sorted_list(OrigValidPredIdSet),

    typecheck_to_fixpoint(ProgressStream, 1, MaxIterations, !ModuleInfo,
        OrigValidPredIds, OrigValidPredIdSet, FinalValidPredIdSet,
        CheckSpecs, FoundSyntaxError, NumberOfIterations),

    construct_type_inference_messages(!.ModuleInfo, FinalValidPredIdSet,
        OrigValidPredIds, [], InferSpecs),
    Specs = InferSpecs ++ CheckSpecs.

    % Repeatedly typecheck the code for a group of predicates
    % until a fixpoint is reached, or until some errors are detected.
    %
:- pred typecheck_to_fixpoint(io.text_output_stream::in, int::in, int::in,
    module_info::in, module_info::out,
    list(pred_id)::in, set_tree234(pred_id)::in, set_tree234(pred_id)::out,
    list(error_spec)::out, maybe_clause_syntax_errors::out,
    number_of_iterations::out) is det.

typecheck_to_fixpoint(ProgressStream, Iteration, MaxIterations, !ModuleInfo,
        OrigValidPredIds, OrigValidPredIdSet, FinalValidPredIdSet,
        Specs, FoundSyntaxError, NumberOfIterations) :-
    module_info_get_pred_id_table(!.ModuleInfo, PredIdTable0),
    map.to_assoc_list(PredIdTable0, PredIdsInfos0),
    typecheck_module_one_iteration(ProgressStream, !.ModuleInfo,
        OrigValidPredIdSet, PredIdsInfos0, [], RevPredIdsInfos,
        [], NewlyInvalidPredIds, [], CurSpecs,
        no_clause_syntax_errors, CurFoundSyntaxError,
        next_iteration_is_not_needed, NextIteration),
    map.from_rev_sorted_assoc_list(RevPredIdsInfos, PredIdTable),
    module_info_set_pred_id_table(PredIdTable, !ModuleInfo),

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
        NumberOfIterations = within_iteration_limit
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
                write_error_specs(OutputStream, Globals, ProgressSpecs, !IO)
            )
        ;
            DebugTypes = no
        ),
        ( if Iteration < MaxIterations then
            typecheck_to_fixpoint(ProgressStream, Iteration + 1, MaxIterations,
                !ModuleInfo, OrigValidPredIds,
                OrigValidPredIdSet, FinalValidPredIdSet, Specs,
                FoundSyntaxError, NumberOfIterations)
        else
            FinalValidPredIdSet = NewValidPredIdSet,
            Specs = [typecheck_report_max_iterations_exceeded(MaxIterations)],
            FoundSyntaxError = CurFoundSyntaxError,
            NumberOfIterations = exceeded_iteration_limit
        )
    ).

%---------------------------------------------------------------------------%

:- type next_iteration
    --->    next_iteration_is_not_needed
    ;       next_iteration_is_needed.

    % Iterate over the list of pred_ids in a module.
    %
:- pred typecheck_module_one_iteration(io.text_output_stream::in,
    module_info::in, set_tree234(pred_id)::in,
    assoc_list(pred_id, pred_info)::in,
    assoc_list(pred_id, pred_info)::in, assoc_list(pred_id, pred_info)::out,
    list(pred_id)::in, list(pred_id)::out,
    list(error_spec)::in, list(error_spec)::out,
    maybe_clause_syntax_errors::in, maybe_clause_syntax_errors::out,
    next_iteration::in, next_iteration::out) is det.

typecheck_module_one_iteration(_, _, _, [], !RevPredIdsInfos,
        !NewlyInvalidPredIds, !Specs, !FoundSyntaxError, !NextIteration).
typecheck_module_one_iteration(ProgressStream, ModuleInfo, ValidPredIdSet,
        [HeadPredIdInfo0 | TailPredIdsInfos0], !RevPredIdsInfos,
        !NewlyInvalidPredIds, !Specs, !FoundSyntaxError, !NextIteration) :-
    HeadPredIdInfo0 = PredId - PredInfo0,
    ( if
        (
            pred_info_is_imported(PredInfo0)
        ;
            not set_tree234.contains(ValidPredIdSet, PredId)
        )
    then
        !:RevPredIdsInfos = [HeadPredIdInfo0 | !.RevPredIdsInfos],
        typecheck_module_one_iteration(ProgressStream, ModuleInfo,
            ValidPredIdSet, TailPredIdsInfos0, !RevPredIdsInfos,
            !NewlyInvalidPredIds, !Specs, !FoundSyntaxError, !NextIteration)
    else
        % Potential parallelization site.
        typecheck_pred_if_needed(ProgressStream, ModuleInfo, PredId,
            PredInfo0, PredInfo, PredSpecs, PredSyntaxError, ContainsErrors,
            PredNextIteration),
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
        (
            PredSyntaxError = some_clause_syntax_errors,
            !:FoundSyntaxError = some_clause_syntax_errors
        ;
            PredSyntaxError = no_clause_syntax_errors
        ),
        (
            PredNextIteration = next_iteration_is_not_needed
        ;
            PredNextIteration = next_iteration_is_needed,
            !:NextIteration = next_iteration_is_needed
        ),
        !:RevPredIdsInfos = [HeadPredIdInfo | !.RevPredIdsInfos],
        typecheck_module_one_iteration(ProgressStream, ModuleInfo,
            ValidPredIdSet, TailPredIdsInfos0, !RevPredIdsInfos,
            !NewlyInvalidPredIds, !Specs, !FoundSyntaxError, !NextIteration)
    ).

:- pred typecheck_pred_if_needed(io.text_output_stream::in, module_info::in,
    pred_id::in, pred_info::in, pred_info::out, list(error_spec)::out,
    maybe_clause_syntax_errors::out, bool::out, next_iteration::out) is det.

typecheck_pred_if_needed(ProgressStream, ModuleInfo, PredId, !PredInfo,
        !:Specs, FoundSyntaxError, ContainsErrors, NextIteration) :-
    ( if is_pred_created_type_correct(ModuleInfo, !PredInfo) then
        !:Specs = [],
        FoundSyntaxError = no_clause_syntax_errors,
        ContainsErrors = no,
        NextIteration = next_iteration_is_not_needed
    else
        pred_info_get_clauses_info(!.PredInfo, ClausesInfo0),
        clauses_info_get_had_syntax_errors(ClausesInfo0, FoundSyntaxError),
        handle_stubs_and_non_contiguous_clauses(ModuleInfo, PredId, !PredInfo,
            FoundSyntaxError, !:Specs, MaybeNeedTypecheck),
        (
            MaybeNeedTypecheck = do_not_need_typecheck(ContainsErrors,
                NextIteration)
        ;
            MaybeNeedTypecheck = do_need_typecheck,
            do_typecheck_pred(ProgressStream, ModuleInfo, PredId, !PredInfo,
                !Specs, NextIteration),
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
            % The exceptions are unify and compare predicates that either
            %
            % - call a user-defined predicate, or
            % - involve existentially quantified type variables.
            is_unify_index_or_compare_pred(!.PredInfo),
            not special_pred_needs_typecheck(ModuleInfo, !.PredInfo)
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

    % This predicate has two tasks.
    %
    % One is to handle stubs, and in particular the --allow-stubs and
    % --warn-stubs options.
    %
    % If --allow-stubs is set, and there are no clauses, then
    % - issue a warning (if --warn-stubs is set), and then
    % - generate a "stub" clause that just throws an exception.
    %
    % The other is to generate warnings for non-contiguous clauses.
    %
    % The two tasks are done together because they are complementary:
    % the first handles only empty clause lists, the second handles
    % only nonempty clause lists. Instead of two separate traversals,
    % one to handle stubs and one to handle non-contiguous clauses,
    % this predicate enables one traversal to do both tasks.
    %
:- pred handle_stubs_and_non_contiguous_clauses(module_info::in, pred_id::in,
    pred_info::in, pred_info::out, maybe_clause_syntax_errors::in,
    list(error_spec)::out, maybe_need_typecheck::out) is det.

handle_stubs_and_non_contiguous_clauses(ModuleInfo, PredId, !PredInfo,
        FoundSyntaxError, !:Specs, MaybeNeedTypecheck) :-
    pred_info_get_markers(!.PredInfo, Markers0),
    pred_info_get_clauses_info(!.PredInfo, ClausesInfo0),
    clauses_info_get_clauses_rep(ClausesInfo0, ClausesRep0, ItemNumbers0),
    clause_list_is_empty(ClausesRep0) = ClausesRep0IsEmpty,
    (
        ClausesRep0IsEmpty = yes,
        module_info_get_globals(ModuleInfo, Globals),
        % There are no clauses, so there can be no clause non-contiguity
        % errors.
        ( if
            globals.lookup_bool_option(Globals, allow_stubs, yes),
            not check_marker(Markers0, marker_class_method)
        then
            !:Specs =
                maybe_report_no_clauses_stub(ModuleInfo, PredId, !.PredInfo),
            generate_and_add_stub_clause(ModuleInfo, PredId, !PredInfo)
        else if
            check_marker(Markers0, marker_builtin_stub)
        then
            !:Specs = [],
            generate_and_add_stub_clause(ModuleInfo, PredId, !PredInfo)
        else
            !:Specs = []
        )
    ;
        ClausesRep0IsEmpty = no,
        % There are clauses, so there can be no need to add stub clauses.
        maybe_check_for_and_report_any_non_contiguous_clauses(ModuleInfo,
            PredId, !.PredInfo, ItemNumbers0, !:Specs)
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
            clauses_info_get_varset(ClausesInfo1, VarSet),
            corresponding_vars_types_to_var_table(ModuleInfo, VarSet,
                HeadVars, ArgTypes, VarTable),
            clauses_info_set_explicit_vartypes(VarTypes,
                ClausesInfo1, ClausesInfo2),
            clauses_info_set_var_table(VarTable, ClausesInfo2, ClausesInfo),
            pred_info_set_clauses_info(ClausesInfo, !PredInfo),
            % We also need to set the external_type_params field
            % to indicate that all the existentially quantified tvars
            % in the head of this pred are indeed bound by this predicate.
            type_vars_in_types(ArgTypes, HeadVarsInclExistentials),
            pred_info_set_external_type_params(HeadVarsInclExistentials,
                !PredInfo),
            ContainsErrors = no,
            !:Specs = []
        else
            ContainsErrors = yes,
            (
                FoundSyntaxError = no_clause_syntax_errors,
                !:Specs =
                    maybe_report_no_clauses(ModuleInfo, PredId, !.PredInfo)
            ;
                FoundSyntaxError = some_clause_syntax_errors,
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
            FoundSyntaxError = no_clause_syntax_errors,
            MaybeNeedTypecheck = do_need_typecheck
        ;
            FoundSyntaxError = some_clause_syntax_errors,
            % Printing the messages we generated above could be misleading,
            % and the messages for the syntax errors will mean that
            % this compiler invocation won't succeed anyway.
            !:Specs = [],
            ContainsErrors = yes,
            MaybeNeedTypecheck = do_not_need_typecheck(ContainsErrors,
                next_iteration_is_not_needed)
        )
    ).

%---------------------------------------------------------------------------%

:- pred do_typecheck_pred(io.text_output_stream::in, module_info::in,
    pred_id::in, pred_info::in, pred_info::out,
    list(error_spec)::in, list(error_spec)::out, next_iteration::out) is det.

do_typecheck_pred(ProgressStream, ModuleInfo, PredId, !PredInfo,
        !Specs, NextIteration) :-
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
                maybe_write_pred_progress_message(ProgressStream, ModuleInfo,
                    "Inferring type of", PredId, !IO)
            ),
            !:ExternalTypeParams = [],
            PredConstraints = constraints([], [])
        else
            Inferring = no,
            trace [io(!IO)] (
                maybe_write_pred_progress_message(ProgressStream, ModuleInfo,
                    "Type-checking", PredId, !IO)
            ),
            type_vars_in_types(ArgTypes0, !:ExternalTypeParams),
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
        pred_info_get_markers(!.PredInfo, PredMarkers0),
        typecheck_info_init(ModuleInfo, PredId, !.PredInfo,
            ClauseVarSet, PredStatus, PredMarkers0, !.Specs, !:Info),
        get_clause_list_for_replacement(ClausesRep0, Clauses0),
        typecheck_clauses(HeadVars, ArgTypes0, Clauses0, Clauses,
            !TypeAssignSet, !Info),
        typecheck_info_get_rhs_lambda(!.Info, MaybeRHSLambda),
        (
            MaybeRHSLambda = has_no_rhs_lambda
        ;
            MaybeRHSLambda = has_rhs_lambda,
            add_marker(marker_has_rhs_lambda, PredMarkers0, PredMarkers),
            pred_info_set_markers(PredMarkers, !PredInfo)
        ),
        % We need to perform a final pass of context reduction at the end,
        % before checking the typeclass constraints.
        pred_info_get_context(!.PredInfo, Context),
        perform_context_reduction(Context, !TypeAssignSet, !Info),
        typecheck_check_for_ambiguity(Context, whole_pred, HeadVars,
            !.TypeAssignSet, !Info),
        typecheck_check_for_unsatisfied_coercions(!.TypeAssignSet, !Info),
        type_assign_set_get_final_info(!.TypeAssignSet,
            !.ExternalTypeParams, ExistQVars0, ExplicitVarTypes0, TypeVarSet,
            !:ExternalTypeParams, InferredVarTypes, InferredTypeConstraints0,
            ConstraintProofMap, ConstraintMap,
            TVarRenaming, ExistTypeRenaming),
        vartypes_to_sorted_assoc_list(InferredVarTypes, VarsTypes),
        vars_types_to_var_table(ModuleInfo, ClauseVarSet, VarsTypes,
            VarTable0),
        var_table_optimize(VarTable0, VarTable),
        clauses_info_set_var_table(VarTable, !ClausesInfo),

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
        type_vars_in_types(ArgTypes, ArgTypeVars),
        restrict_constraints_to_head_vars(ArgTypeVars,
            InferredTypeConstraints0, InferredTypeConstraints,
            UnprovenBodyConstraints),

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
                    GoalType = goal_for_promise(_)
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

                term_subst.apply_renaming_in_vars(ExistTypeRenaming,
                    ExistQVars0, ExistQVars1),
                apply_variable_renaming_to_type_list(ExistTypeRenaming,
                    ArgTypes0, ArgTypes1),
                apply_variable_renaming_to_prog_constraints(
                    ExistTypeRenaming, PredConstraints, PredConstraints1),
                rename_instance_method_constraints(ExistTypeRenaming,
                    Origin0, Origin1)
            ),

            % Rename them all to match the new typevarset.
            term_subst.apply_renaming_in_vars(TVarRenaming,
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
:- pred generate_and_add_stub_clause(module_info::in, pred_id::in,
    pred_info::in, pred_info::out) is det.

generate_and_add_stub_clause(ModuleInfo, PredId, !PredInfo) :-
    some [!ClausesInfo] (
        pred_info_get_clauses_info(!.PredInfo, !:ClausesInfo),
        !.ClausesInfo = clauses_info(VarSet0, _VarTypes,
            _VarTable, RttiVarMaps, TVarNameMap, ArgVec,
            _ClausesRep, _ItemNumbers, _ForeignClauses, _SyntaxErrors),
        PredPieces = describe_one_pred_name(ModuleInfo, should_module_qualify,
            PredId),
        PredName = error_pieces_to_one_line_string(PredPieces),
        HeadVars = proc_arg_vector_to_list(ArgVec),
        pred_info_get_arg_types(!.PredInfo, ArgTypes),
        vartypes_from_corresponding_lists(HeadVars, ArgTypes, VarTypes1),
        generate_stub_clause(ModuleInfo, !.PredInfo, PredName, StubClause,
            VarSet0, VarSet, VarTypes1, VarTypes),
        make_var_table(ModuleInfo, VarSet, VarTypes, VarTable),
        set_clause_list([StubClause], ClausesRep),
        ItemNumbers = init_clause_item_numbers_comp_gen,
        !:ClausesInfo = clauses_info(VarSet, VarTypes, VarTable, RttiVarMaps,
            TVarNameMap, ArgVec, ClausesRep, ItemNumbers,
            no_foreign_lang_clauses, no_clause_syntax_errors),
        pred_info_set_clauses_info(!.ClausesInfo, !PredInfo),

        % Mark the predicate as a stub, i.e. record that it originally
        % had no clauses.
        pred_info_get_markers(!.PredInfo, Markers0),
        add_marker(marker_stub, Markers0, Markers),
        pred_info_set_markers(Markers, !PredInfo)
    ).

:- pred generate_stub_clause(module_info::in, pred_info::in,
    string::in, clause::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

generate_stub_clause(ModuleInfo, PredInfo, PredName, StubClause,
        !VarSet, !VarTypes) :-
    % Generate `PredName = "<PredName>"'.
    varset.new_named_var("PredName", PredNameVar, !VarSet),
    add_var_type(PredNameVar, string_type, !VarTypes),
    pred_info_get_context(PredInfo, Context),
    make_string_const_construction(Context, PredNameVar, PredName, UnifyGoal),

    % Generate `private_builtin.no_clauses(PredName)'
    % or `private_builtin.sorry(PredName)'
    PredModuleName = pred_info_module(PredInfo),
    ( if mercury_std_library_module_name(PredModuleName) then
        CalleeName = "sorry"
    else
        CalleeName = "no_clauses"
    ),
    generate_plain_call(ModuleInfo, pf_predicate,
        mercury_private_builtin_module, CalleeName,
        [], [PredNameVar], instmap_delta_bind_no_var, only_mode,
        detism_det, purity_pure, [], Context, CallGoal),

    % Combine the unification and call into a conjunction.
    goal_info_init(Context, GoalInfo),
    Body = hlds_goal(conj(plain_conj, [UnifyGoal, CallGoal]), GoalInfo),
    StubClause = clause(all_modes, Body, impl_lang_mercury, Context, []).

:- pred rename_instance_method_constraints(tvar_renaming::in,
    pred_origin::in, pred_origin::out) is det.

rename_instance_method_constraints(Renaming, Origin0, Origin) :-
    ( if
        Origin0 = origin_user(OriginUser0),
        OriginUser0 = user_made_instance_method(PFSymNameArity, Constraints0)
    then
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
        OriginUser = user_made_instance_method(PFSymNameArity, Constraints),
        Origin = origin_user(OriginUser)
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

    % restrict_constraints_to_head_vars(HeadVarTypes, Constraints0,
    %   Constraints, UnprovenConstraints):
    %
    % Constraints is the subset of Constraints0 which contain no type variables
    % other than those in HeadVarTypes. UnprovenConstraints is any unproven
    % (universally quantified) type constraints on variables not in
    % HeadVarTypes.
    %
:- pred restrict_constraints_to_head_vars(list(tvar)::in,
    prog_constraints::in, prog_constraints::out,
    list(prog_constraint)::out) is det.

restrict_constraints_to_head_vars(ArgVarTypes, constraints(UnivCs0, ExistCs0),
        constraints(UnivCs, ExistCs), UnprovenCs) :-
    restrict_constraints_to_head_vars_2(ArgVarTypes, UnivCs0, UnivCs,
        UnprovenCs),
    restrict_constraints_to_head_vars_2(ArgVarTypes, ExistCs0, ExistCs, _).

:- pred restrict_constraints_to_head_vars_2(list(tvar)::in,
    list(prog_constraint)::in,
    list(prog_constraint)::out, list(prog_constraint)::out) is det.

restrict_constraints_to_head_vars_2(HeadTypeVars, ClassConstraints,
        HeadClassConstraints, OtherClassConstraints) :-
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
    constraints_have_same_structure(TypeConstraintsA, TypeConstraintsB,
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
:- pred constraints_have_same_structure(
    prog_constraints::in, prog_constraints::in,
    list(mer_type)::out, list(mer_type)::out) is semidet.

constraints_have_same_structure(ConstraintsA, ConstraintsB, TypesA, TypesB) :-
    ConstraintsA = constraints(UnivCsA, ExistCsA),
    ConstraintsB = constraints(UnivCsB, ExistCsB),
    % these calls to same_length are just an optimization,
    % to catch the simple cases quicker
    list.same_length(UnivCsA, UnivCsB),
    list.same_length(ExistCsA, ExistCsB),
    constraints_have_same_structure_loop(UnivCsA, UnivCsB,
        UnivTypesA, UnivTypesB),
    constraints_have_same_structure_loop(ExistCsA, ExistCsB,
        ExistTypesA, ExistTypesB),
    TypesA = ExistTypesA ++ UnivTypesA,
    TypesB = ExistTypesB ++ UnivTypesB.

:- pred constraints_have_same_structure_loop(
    list(prog_constraint)::in, list(prog_constraint)::in,
    list(mer_type)::out, list(mer_type)::out) is semidet.

constraints_have_same_structure_loop([], [], [], []).
constraints_have_same_structure_loop(
        [ConstraintA | ConstraintsA], [ConstraintB | ConstraintsB],
        TypesA, TypesB) :-
    ConstraintA = constraint(ClassName, ArgTypesA),
    ConstraintB = constraint(ClassName, ArgTypesB),
    list.same_length(ArgTypesA, ArgTypesB),
    constraints_have_same_structure_loop(ConstraintsA, ConstraintsB,
        TypesA0, TypesB0),
    TypesA = ArgTypesA ++ TypesA0,
    TypesB = ArgTypesB ++ TypesB0.

    % A compiler-generated predicate only needs type checking if
    % (a) it is a user-defined equality pred, or
    % (b) it is the unification or comparison predicate for an existentially
    %     quantified type.
    %
    % In case (b), we need to typecheck it to fill in the external_type_params
    % field in the pred_info.
    %
:- pred special_pred_needs_typecheck(module_info::in, pred_info::in)
    is semidet.

special_pred_needs_typecheck(ModuleInfo, PredInfo) :-
    % Check if the predicate is a compiler-generated special
    % predicate, and if so, for which type.
    pred_info_get_origin(PredInfo, Origin),
    Origin = origin_compiler(made_for_uci(SpecialPredId, TypeCtor)),

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
        % If there are multiple type assignments then there is a type ambiguity
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
:- end_module check_hlds.typecheck.
%---------------------------------------------------------------------------%
