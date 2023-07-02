%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1997-2012,2014 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% Author: fjh
%
% This module does most of the final parts of type analysis:
%
%   - it reports errors for any unsatisfied type class constraints;
%   - it reports an error or a warning for unbound type variables,
%     binding them to the type `void';
%   - it propagates type information into the argument modes of procedures;
%   - it reports errors for unbound inst variables in mode declarations;
%   - it reports an error if a predicate or function has two or more
%     indistinguishable modes.
%
% These actions cannot be done until after type inference is complete,
% so they need to be done in a pass *after* the typecheck pass.
%
% A few other related actions that have similar constraints on when they
% should be done are handled by resolve_unify_functor.m, by check_promise.m,
% or by code in purity.m itself.
%
%---------------------------------------------------------------------------%

:- module check_hlds.post_typecheck.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.

:- import_module list.

%---------------------------------------------------------------------------%

    % post_typecheck_finish_preds(!ModuleInfo, NumErrors,
    %   AlwaysSpecs, NoTypeErrorSpecs):
    %
    % Check that the types of variables in predicates contain no unbound type
    % variables other than those that occur in the types of the predicate's
    % head variables, and that there are no unsatisfied type class constraints.
    % Also bind any unbound type variables to the type `void'.
    %
    % Return two lists of error messages. AlwaysSpecs will be the messages
    % we want to print in all cases, and NoTypeErrorSpecs will be the messages
    % we want to print only if type checking did not find any errors. The
    % latter will be the kinds of errors that you can get as "avalanche"
    % messages from type errors.
    %
    % Separately, we return NumBadErrors, the number of errors that prevent us
    % from proceeding further in compilation. We do this separately since some
    % errors (e.g. bad type for main) do NOT prevent us from going further.
    %
    % Note that when checking assertions we take the conservative approach
    % of warning about unbound type variables. There may be cases for which
    % this doesn't make sense.
    %
:- pred post_typecheck_finish_preds(module_info::in, module_info::out,
    int::out, list(error_spec)::out, list(error_spec)::out) is det.

    % Make sure the vartypes field in the clauses_info is valid for imported
    % predicates. (Non-imported predicates should already have it set up.)
    %
:- pred setup_var_table_in_clauses_for_imported_pred(module_info::in,
    pred_info::in, pred_info::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.inst_mode_type_prop.
:- import_module check_hlds.mode_comparison.
:- import_module check_hlds.mode_errors.
:- import_module check_hlds.type_util.
:- import_module check_hlds.types_into_modes.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_class.
:- import_module hlds.hlds_clauses.
:- import_module hlds.hlds_error_util.
:- import_module hlds.hlds_goal.
:- import_module hlds.pred_name.
:- import_module hlds.status.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.op_mode.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.parse_tree_out_type.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.var_table.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module set.
:- import_module set_tree234.
:- import_module solutions.
:- import_module string.
:- import_module varset.

%---------------------------------------------------------------------------%

post_typecheck_finish_preds(!ModuleInfo, NumBadErrors,
        AlwaysSpecs, NoTypeErrorSpecs) :-
    module_info_get_valid_pred_ids(!.ModuleInfo, ValidPredIds),
    ValidPredIdSet = set_tree234.list_to_set(ValidPredIds),
    module_info_get_pred_id_table(!.ModuleInfo, PredIdTable0),
    map.map_foldl4(post_typecheck_do_finish_pred(!.ModuleInfo, ValidPredIdSet),
        PredIdTable0, PredIdTable, map.init, _Cache, 0, NumBadErrors,
        [], AlwaysSpecs, [], NoTypeErrorSpecs),
    module_info_set_pred_id_table(PredIdTable, !ModuleInfo).

:- pred post_typecheck_do_finish_pred(module_info::in,
    set_tree234(pred_id)::in, pred_id::in,
    pred_info::in, pred_info::out, tprop_cache::in, tprop_cache::out,
    int::in, int::out, list(error_spec)::in, list(error_spec)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

post_typecheck_do_finish_pred(ModuleInfo, ValidPredIdSet, PredId, !PredInfo,
        !Cache, !NumBadErrors, !AlwaysSpecs, !NoTypeErrorSpecs) :-
    ( if set_tree234.contains(ValidPredIdSet, PredId) then
        % Regardless of the path we take when processing a valid predicate,
        % we need to ensure that we fill in the vte_is_dummy field in all
        % the entries in the predicate's var_table with valid information,
        % to replace the placeholder values put there earlier.
        %
        % In the then-part of this if-then-else, that is done by
        % setup_var_table_in_clauses_for_imported_pred. In the else-part,
        % it is done by find_unresolved_types_fill_in_is_dummy_in_pred.
        ( if
            ( pred_info_is_imported(!.PredInfo)
            ; pred_info_is_pseudo_imported(!.PredInfo)
            )
        then
            setup_var_table_in_clauses_for_imported_pred(ModuleInfo, !PredInfo)
        else
            % Emptying out the varset tells hlds_out_pred.m that the
            % clauses_info has been through typechecking, and that
            % the authoritative source for information about variables' names
            % is now the var_table field, not the varset field.
            % This is because all the compiler passes after typechecking
            % that create new variables add them to the var_table field,
            % not to the varset field.
            %
            % setup_var_table_in_clauses_for_imported_pred does the same
            % in the then branch of this if-then-else.
            pred_info_get_clauses_info(!.PredInfo, ClausesInfo0),
            varset.init(EmptyVarSet),
            clauses_info_set_varset(EmptyVarSet, ClausesInfo0, ClausesInfo),
            pred_info_set_clauses_info(ClausesInfo, !PredInfo),

            find_unproven_body_constraints(ModuleInfo, PredId, !.PredInfo,
                !NumBadErrors, !NoTypeErrorSpecs),
            find_unresolved_types_fill_in_is_dummy_in_pred(ModuleInfo, PredId,
                !PredInfo, !NoTypeErrorSpecs),
            check_type_of_main(!.PredInfo, !AlwaysSpecs)
        ),
        propagate_checked_types_into_pred_modes(ModuleInfo, ErrorProcs,
            InstForTypeSpecs, !Cache, !PredInfo),
        !:NoTypeErrorSpecs = InstForTypeSpecs ++ !.NoTypeErrorSpecs,
        report_unbound_inst_vars(ModuleInfo, PredId, ErrorProcs, !PredInfo,
            !AlwaysSpecs),
        check_for_indistinguishable_modes(ModuleInfo, PredId, !PredInfo,
            !AlwaysSpecs)
    else
        true
    ).

%---------------------%

:- pred report_unbound_inst_vars(module_info::in, pred_id::in,
    list(proc_id)::in, pred_info::in, pred_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

report_unbound_inst_vars(ModuleInfo, PredId, ErrorProcIds, !PredInfo,
        !Specs) :-
    (
        ErrorProcIds = []
    ;
        ErrorProcIds = [_ | _],
        pred_info_get_proc_table(!.PredInfo, ProcTable0),
        list.foldl2(report_unbound_inst_var_error(ModuleInfo, PredId),
            ErrorProcIds, ProcTable0, ProcTable, !Specs),
        pred_info_set_proc_table(ProcTable, !PredInfo)
    ).

:- pred report_unbound_inst_var_error(module_info::in,
    pred_id::in, proc_id::in, proc_table::in, proc_table::out,
    list(error_spec)::in, list(error_spec)::out) is det.

report_unbound_inst_var_error(ModuleInfo, PredId, ProcId, Procs0, Procs,
        !Specs) :-
    map.lookup(Procs0, ProcId, ProcInfo),
    proc_info_get_context(ProcInfo, Context),
    Pieces = [words("In"), decl("mode"), words("declaration for")] ++
        describe_one_pred_name(ModuleInfo, should_not_module_qualify, PredId)
        ++ [suffix(":"), nl,
        words("error: unbound inst variable(s)."), nl,
        words("(Sorry, polymorphic modes are not supported.)"), nl],
    Spec = simplest_spec($pred, severity_error, phase_type_check,
        Context, Pieces),
    !:Specs = [Spec | !.Specs],
    % Delete this mode, to avoid internal errors.
    map.det_remove(ProcId, _, Procs0, Procs).

%---------------------------------------------------------------------------%

    % Check that the all of the types which have been inferred for the
    % variables in the predicate do not contain any unbound type variables
    % other than those that occur in the types of head variables, and that
    % there are no unsatisfied type class constraints.
    %
:- pred find_unproven_body_constraints(module_info::in, pred_id::in,
    pred_info::in, int::in, int::out,
    list(error_spec)::in, list(error_spec)::out) is det.
:- pragma inline(pred(find_unproven_body_constraints/7)).

find_unproven_body_constraints(ModuleInfo, PredId, PredInfo,
        !NumBadErrors, !NoTypeErrorSpecs) :-
    pred_info_get_unproven_body_constraints(PredInfo, UnprovenConstraints0),
    (
        UnprovenConstraints0 = [_ | _],
        list.sort_and_remove_dups(UnprovenConstraints0, UnprovenConstraints),
        report_unsatisfied_constraints(ModuleInfo, PredId, PredInfo,
            UnprovenConstraints, !NoTypeErrorSpecs),
        list.length(UnprovenConstraints, NumUnprovenConstraints),
        !:NumBadErrors = !.NumBadErrors + NumUnprovenConstraints
    ;
        UnprovenConstraints0 = []
    ).

%---------------------%

    % Report unsatisfied typeclass constraints.
    %
:- pred report_unsatisfied_constraints(module_info::in,
    pred_id::in, pred_info::in, list(prog_constraint)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_unsatisfied_constraints(ModuleInfo, PredId, PredInfo, Constraints,
        !Specs) :-
    pred_info_get_typevarset(PredInfo, TVarSet),
    pred_info_get_context(PredInfo, Context),

    PredIdPieces = describe_one_pred_name(ModuleInfo,
        should_not_module_qualify, PredId),

    MainPieces = [words("In")] ++ PredIdPieces ++ [suffix(":"), nl,
        fixed("type error: unsatisfied typeclass " ++
        choose_number(Constraints, "constraint:", "constraints:")),
        nl_indent_delta(1)] ++
        component_list_to_line_pieces(
            list.map(constraint_to_error_piece(TVarSet), Constraints),
                [nl_indent_delta(-1)]),
    MainMsg = simplest_msg(Context, MainPieces),

    ConstrainedGoals = find_constrained_goals(PredInfo, Constraints),
    (
        % This can happen because the call to find_constraint_goals/2 will not
        % necessarily return goal_ids for every unproven constraint.  See the
        % comment in that function for details.
        % XXX If we performed this check after checking for unresolved
        % polymorphism we could at least report the problem is due to unbound
        % type variables occurring in Constraints.
        ConstrainedGoals = [],
        ContextMsgs = []
    ;
        ConstrainedGoals = [_ | _],
        DueToPieces = choose_number(Constraints,
            [words("The constraint is due to:")],
            [words("The constraints are due to:")]),
        ContextMsgsPrefix = simplest_msg(Context, DueToPieces),
        ContextMsgsList = constrained_goals_to_error_msgs(ModuleInfo,
            ConstrainedGoals),
        ContextMsgs = [ContextMsgsPrefix | ContextMsgsList]
    ),
    Spec = error_spec($pred, severity_error, phase_type_check,
        [MainMsg | ContextMsgs]),
    !:Specs = [Spec | !.Specs].

:- func constraint_to_error_piece(tvarset, prog_constraint)
    = list(format_piece).

constraint_to_error_piece(TVarset, Constraint) =
    [quote(mercury_constraint_to_string(TVarset, print_name_only,
        Constraint))].

    % A prog_constraint cannot contain context information (see the comment on
    % the type definition). However, a constraint_id happens to contain a
    % goal_id, so we can look up a constraint_id for a prog_constraint, then
    % use the goal_id to reach the goal.
    %
:- func find_constrained_goals(pred_info, list(prog_constraint))
    = list(hlds_goal).

find_constrained_goals(PredInfo, Constraints) = Goals :-
    pred_info_get_clauses_info(PredInfo, ClausesInfo),
    clauses_info_get_clauses_rep(ClausesInfo, ClausesRep, _ItemNumbers),
    get_clause_list_maybe_repeated(ClausesRep, Clauses),

    pred_info_get_constraint_map(PredInfo, ConstraintMap),
    ReverseConstraintMap = map.reverse_map(ConstraintMap),
    list.foldl(gather_constraint_ids(ReverseConstraintMap), Constraints,
        [], ConstraintIdSets),
    ConstraintIds = set.union_list(ConstraintIdSets),

    % This could be more efficient.
    FindGoals =
        ( pred(Goal::out) is nondet :-
            set.member(ConstraintId, ConstraintIds),
            ConstraintId = constraint_id(_, ConstraintGoalId, _),
            promise_equivalent_solutions [Goal] (
                list.member(Clause, Clauses),
                goal_contains_goal(Clause ^ clause_body, Goal),
                Goal = hlds_goal(_, GoalInfo),
                GoalId = goal_info_get_goal_id(GoalInfo),
                GoalId = ConstraintGoalId
            )
        ),
    solutions(FindGoals, Goals).

:- pred gather_constraint_ids(map(prog_constraint, set(constraint_id))::in,
    prog_constraint::in,
    list(set(constraint_id))::in, list(set(constraint_id))::out) is det.

gather_constraint_ids(ReverseConstraintMap, Constraint, !ConstraintIdSets) :-
    % Note that not all unproven constraints will appear in the reverse
    % constraint map (it only stores as many as the type checker requires).
    % We should store context information for unproven constraints separately
    % so we can report it in error messages.
    ( if map.search(ReverseConstraintMap, Constraint, ConstraintIdSet) then
        !:ConstraintIdSets = [ConstraintIdSet | !.ConstraintIdSets]
    else
        true
    ).

:- func constrained_goals_to_error_msgs(module_info, list(hlds_goal))
    = list(error_msg).

constrained_goals_to_error_msgs(_, []) = [].
constrained_goals_to_error_msgs(ModuleInfo, [Goal | Goals]) = [Msg | Msgs] :-
    (
        Goals = [_, _ | _],
        Words = describe_constrained_goal(ModuleInfo, Goal),
        Suffix = suffix(",")
    ;
        Goals = [_],
        Words = describe_constrained_goal(ModuleInfo, Goal),
        Suffix = suffix(", and")
    ;
        Goals = [],
        Words = describe_constrained_goal(ModuleInfo, Goal),
        Suffix = suffix(".")
    ),
    Goal = hlds_goal(_, GoalInfo),
    Context = goal_info_get_context(GoalInfo),
    Msg = error_msg(yes(Context), treat_based_on_posn, 1,
        [always(Words ++ [Suffix])]),
    Msgs = constrained_goals_to_error_msgs(ModuleInfo, Goals).

:- func describe_constrained_goal(module_info, hlds_goal)
    = list(format_piece).

describe_constrained_goal(ModuleInfo, Goal) = Pieces :-
    Goal = hlds_goal(GoalExpr, _),
    (
        (
            GoalExpr = plain_call(PredId, _, _, _, _, _),
            CallPieces = describe_one_pred_name(ModuleInfo,
                should_module_qualify, PredId)
        ;
            GoalExpr = generic_call(GenericCall, _, _, _, _),
            GenericCall = class_method(_, _, _, PFSymNameArity),
            CallPieces = [qual_pf_sym_name_pred_form_arity(PFSymNameArity)]
        ;
            GoalExpr = call_foreign_proc(_, PredId, _, _, _, _, _),
            CallPieces = describe_one_pred_name(ModuleInfo,
                should_module_qualify, PredId)
        ),
        Pieces = [words("the call to") | CallPieces]
    ;
        GoalExpr = generic_call(higher_order(_, _, _, _), _, _, _, _),
        Pieces = [words("a higher-order call here")]
    ;
        ( GoalExpr = generic_call(event_call(_), _, _, _, _)
        ; GoalExpr = generic_call(cast(_), _, _, _, _)
        ; GoalExpr = unify(_, _, _, _, _)
        ; GoalExpr = conj(_, _)
        ; GoalExpr = disj(_)
        ; GoalExpr = switch(_, _, _)
        ; GoalExpr = negation(_)
        ; GoalExpr = scope(_, _)
        ; GoalExpr = if_then_else(_, _, _, _)
        ; GoalExpr = shorthand(_)
        ),
        Pieces = [words("a goal here")]
    ).

%---------------------------------------------------------------------------%

    % Check that all of the types which have been inferred for the
    % variables in the predicate are free of unbound type variables
    % other than those that occur in the types of head variables, and that
    % there are no unsatisfied type class constraints.
    %
    % Also, fill in the vte_is_dummy field in all the entries in predicate's
    % var_table. We do this by flattening the old var table to VarsEntries0,
    % filling in those slots in VarsEntries0 to yield RevVarsEntries, and then
    % constructing the updated var table from RevVarsEntries.
    %
:- pred find_unresolved_types_fill_in_is_dummy_in_pred(module_info::in,
    pred_id::in, pred_info::in, pred_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.
:- pragma inline(pred(find_unresolved_types_fill_in_is_dummy_in_pred/6)).

find_unresolved_types_fill_in_is_dummy_in_pred(ModuleInfo, PredId, !PredInfo,
        !NoTypeErrorSpecs) :-
    pred_info_get_clauses_info(!.PredInfo, ClausesInfo0),
    pred_info_get_external_type_params(!.PredInfo, ExternalTypeParams),
    clauses_info_get_var_table(ClausesInfo0, VarTable0),
    var_table_to_sorted_assoc_list(VarTable0, VarsEntries0),
    set.init(BindToVoidTVars0),
    find_unresolved_types_fill_in_is_dummy(ModuleInfo, ExternalTypeParams,
        VarsEntries0, [], RevVarsEntries, [], UnresolvedVarsEntries,
        BindToVoidTVars0, BindToVoidTVars),
    var_table_from_rev_sorted_assoc_list(RevVarsEntries, VarTable1),
    (
        UnresolvedVarsEntries = [],
        VarTable = VarTable1
    ;
        UnresolvedVarsEntries = [_ | _],
        pred_info_get_status(!.PredInfo, PredStatus),
        DefinedHere = pred_status_defined_in_this_module(PredStatus),
        (
            DefinedHere = no
        ;
            DefinedHere = yes,
            report_unresolved_type_warning(ModuleInfo, PredId, !.PredInfo,
                UnresolvedVarsEntries, !NoTypeErrorSpecs)
        ),

        % Bind all the type variables in `BindToVoidTVars' to `void' ...
        pred_info_get_constraint_proof_map(!.PredInfo, ProofMap0),
        pred_info_get_constraint_map(!.PredInfo, ConstraintMap0),
        bind_type_vars_to_void(BindToVoidTVars, VarTable1, VarTable,
            ProofMap0, ProofMap, ConstraintMap0, ConstraintMap),
        pred_info_set_constraint_proof_map(ProofMap, !PredInfo),
        pred_info_set_constraint_map(ConstraintMap, !PredInfo)
    ),
    clauses_info_set_var_table(VarTable, ClausesInfo0, ClausesInfo),
    pred_info_set_clauses_info(ClausesInfo, !PredInfo).

    % The number of variables can be huge here (hundred of thousands for
    % Doug Auclair's training_cars program). The code below prevents stack
    % overflows in grades that do not permit tail recursion.
    %
:- pred find_unresolved_types_fill_in_is_dummy(module_info::in, list(tvar)::in,
    assoc_list(prog_var, var_table_entry)::in,
    assoc_list(prog_var, var_table_entry)::in,
    assoc_list(prog_var, var_table_entry)::out,
    assoc_list(prog_var, var_table_entry)::in,
    assoc_list(prog_var, var_table_entry)::out,
    set(tvar)::in, set(tvar)::out) is det.

find_unresolved_types_fill_in_is_dummy(ModuleInfo, ExternalTypeParams,
        VarsEntries0,
        !RevVarsEntries, !UnresolvedVarsEntries, !BindToVoidTVars) :-
    find_unresolved_types_fill_in_is_dummy_inner(ModuleInfo,
        ExternalTypeParams, 1000, VarsEntries0, LeftOverVarsEntries0,
        !RevVarsEntries, !UnresolvedVarsEntries, !BindToVoidTVars),
    (
        LeftOverVarsEntries0 = []
    ;
        LeftOverVarsEntries0 = [_ | _],
        find_unresolved_types_fill_in_is_dummy(ModuleInfo, ExternalTypeParams,
            LeftOverVarsEntries0,
            !RevVarsEntries, !UnresolvedVarsEntries, !BindToVoidTVars)
    ).

:- pred find_unresolved_types_fill_in_is_dummy_inner(module_info::in,
    list(tvar)::in, int::in,
    assoc_list(prog_var, var_table_entry)::in,
    assoc_list(prog_var, var_table_entry)::out,
    assoc_list(prog_var, var_table_entry)::in,
    assoc_list(prog_var, var_table_entry)::out,
    assoc_list(prog_var, var_table_entry)::in,
    assoc_list(prog_var, var_table_entry)::out,
    set(tvar)::in, set(tvar)::out) is det.

find_unresolved_types_fill_in_is_dummy_inner(_, _, _, [], [],
        !RevVarsEntries, !UnresolvedVarsEntries, !BindToVoidTVars).
find_unresolved_types_fill_in_is_dummy_inner(ModuleInfo, ExternalTypeParams,
        VarsToDo, [Var - Entry0 | VarsEntries0], LeftOverVarsEntries0,
        !RevVarsEntries, !UnresolvedVarsEntries, !BindToVoidTVars) :-
    ( if VarsToDo < 0 then
        LeftOverVarsEntries0 = [Var - Entry0 | VarsEntries0]
    else
        fill_in_is_dummy_slot(ModuleInfo, Entry0, Entry),
        !:RevVarsEntries = [Var - Entry | !.RevVarsEntries],
        Type = Entry ^ vte_type,
        type_vars_in_type(Type, TVars),
        set.list_to_set(TVars, TVarsSet0),
        set.delete_list(ExternalTypeParams, TVarsSet0, TVarsSet1),
        ( if set.is_empty(TVarsSet1) then
            true
        else
            !:UnresolvedVarsEntries = [Var - Entry | !.UnresolvedVarsEntries],
            set.union(TVarsSet1, !BindToVoidTVars)
        ),
        find_unresolved_types_fill_in_is_dummy_inner(ModuleInfo,
            ExternalTypeParams, VarsToDo - 1,
            VarsEntries0, LeftOverVarsEntries0,
            !RevVarsEntries, !UnresolvedVarsEntries, !BindToVoidTVars)
    ).

    % Bind all the type variables in `UnboundTypeVarsSet' to the type `void'.
    %
:- pred bind_type_vars_to_void(set(tvar)::in, var_table::in, var_table::out,
    constraint_proof_map::in, constraint_proof_map::out,
    constraint_map::in, constraint_map::out) is det.

bind_type_vars_to_void(UnboundTypeVarsSet, !VarTable, !ProofMap,
        !ConstraintMap) :-
    % Create a substitution that maps all of the unbound type variables
    % to `void'.
    MapToVoid =
        ( pred(TVar::in, Subst0::in, Subst::out) is det :-
            map.det_insert(TVar, void_type, Subst0, Subst)
        ),
    set.fold(MapToVoid, UnboundTypeVarsSet, map.init, VoidSubst),

    % Then apply the substitution we just created to the various maps.
    IsDummyFunc = (func(_Type) = is_dummy_type),
    apply_subst_to_var_table(IsDummyFunc, VoidSubst, !VarTable),
    apply_subst_to_constraint_proof_map(VoidSubst, !ProofMap),
    apply_subst_to_constraint_map(VoidSubst, !ConstraintMap).

%---------------------%

:- pred fill_in_is_dummy_slot(module_info::in,
    var_table_entry::in, var_table_entry::out) is det.
:- pragma inline(pred(fill_in_is_dummy_slot/3)).

fill_in_is_dummy_slot(ModuleInfo, !Entry) :-
    !.Entry = vte(Name, Type, _OldIsDummy),
    IsDummy = is_type_a_dummy(ModuleInfo, Type),
    % We always allocate a new entry. We put is_dummy_type in the third slot
    % of var_table_entries before typecheck, before this authoritative filling
    % in of that slot, to make any bugs caused by *not* doing this filling-in
    % more visible. They would be more visible because in most programs,
    % most types are not dummy types. But this fact also means that if we
    % tested whether IsDummy = _OldIsDummy, and allocated a new memory cell
    % for a new entry if that test failed, we would lose more time in doing
    % the test than we saved by not doing the allocation if the test succeeded.
    !:Entry = vte(Name, Type, IsDummy).

%---------------------%

    % Report a warning: uninstantiated type parameter.
    %
:- pred report_unresolved_type_warning(module_info::in, pred_id::in,
    pred_info::in, assoc_list(prog_var, var_table_entry)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_unresolved_type_warning(ModuleInfo, PredId, PredInfo, VarsEntries,
        !Specs) :-
    pred_info_get_typevarset(PredInfo, TypeVarSet),
    pred_info_get_context(PredInfo, Context),

    PredIdPieces =
        describe_one_pred_name(ModuleInfo, should_not_module_qualify, PredId),
    list.map_foldl2(var_vte_to_name_and_type_strs(TypeVarSet),
        VarsEntries, VarTypeStrs0,
        0, MaxVarNameLen0, all_tvars, MaybeAllTVars),
    list.sort(VarTypeStrs0, VarTypeStrs),
    (
        MaybeAllTVars = all_tvars,
        VarTypePieceLists = list.map(var_only_to_pieces, VarTypeStrs),
        SetPieces = [
            words(choose_number(VarsEntries, "Its type", "Their types")),
            words("will be implicitly set to the builtin type"),
            quote("void"), suffix("."), nl],
        Known = "known"
    ;
        MaybeAllTVars = not_all_tvars,
        % var_and_type_to_pieces will line things up so that instead of
        % output such as
        %
        %   Var1: Type1
        %   VarABCD: TypeABCD
        %
        % we get
        %
        %   Var1:    Type1
        %   VarABCD: TypeABCD
        %
        % However, if we allow MaxVarNameLen to be *too* long, then
        % the code writing out the error_spec we are constructing
        % will be forced to break the line between the variable name
        % and the type. The value 15 is a guess at a value that is
        % - small enough not to cause such unwanted breaks, but also
        % - long enough to allow the types to line up in blocks that
        %   do *not* get any unwanted line breaks.
        ( if MaxVarNameLen0 > 15 then
            MaxVarNameLen = 15
        else 
            MaxVarNameLen = MaxVarNameLen0
        ),
        VarTypePieceLists =
            list.map(var_and_type_to_pieces(MaxVarNameLen), VarTypeStrs),
        % XXX Just because the is only entry in VarsEntries does NOT
        % necessarily that there is only one type variable; the type
        % of that one variable could be something like "map(T, U)".
        SetPieces = [words("The unbound type"),
            words(choose_number(VarsEntries, "variable", "variables")),
            words("will be implicitly bound to the builtin type"),
            quote("void"), suffix("."), nl],
        Known = "fully known"
    ),
    list.condense(VarTypePieceLists, VarTypePieces),
    MainPieces = [words("In")] ++ PredIdPieces ++ [suffix(":"), nl,
        words("warning: unresolved polymorphism."), nl,
        words(choose_number(VarsEntries,
            "The variable with an unbound type was:",
            "The variables with unbound types were:")), nl_indent_delta(1)] ++
        VarTypePieces ++
        [nl_indent_delta(-1)] ++ SetPieces,
    TypeOrTypes = choose_number(VarsEntries, "type", "types"),
    VarOrVars = choose_number(VarsEntries, "variable", "variables"),
    IsOrAre = choose_number(VarsEntries, "is", "are"),
    VerbosePieces = [words("The body of the clause contains a call"),
        words("to a polymorphic predicate,"),
        words("but I can't determine which version should be called,"),
        words("because the"), words(TypeOrTypes),
        words("of the"), words(VarOrVars), words("listed above"),
        words(IsOrAre), words("not"), words(Known), suffix("."),
        % words("You may need to use an explicit type qualifier."),
        % XXX improve error message
        words("(I ought to tell you which call caused the problem,"),
        words("but I am afraid you will have to work it out yourself."),
        words("My apologies.)"), nl],
    Msg = simple_msg(Context,
        [always(MainPieces), verbose_only(verbose_once, VerbosePieces)]),
    Spec = conditional_spec($pred, warn_unresolved_polymorphism, yes,
        severity_warning, phase_type_check, [Msg]),
    !:Specs = [Spec | !.Specs].

:- type maybe_all_tvars
    --->    not_all_tvars
    ;       all_tvars.

:- pred var_vte_to_name_and_type_strs(tvarset::in, 
    pair(prog_var, var_table_entry)::in, pair(string, string)::out,
    int::in, int::out, maybe_all_tvars::in, maybe_all_tvars::out) is det.

var_vte_to_name_and_type_strs(TVarSet, Var - Entry, VarStr - TypeStr,
        !MaxVarNameLen, !AllTVars) :-
    Entry = vte(Name, Type, _IsDummy),
    VarStr = mercury_var_raw_to_string(print_name_only, Var, Name),
    TypeStr = mercury_type_to_string(TVarSet, print_name_only, Type),
    string.count_code_points(VarStr, VarStrLen),
    ( if VarStrLen > !.MaxVarNameLen then
        !:MaxVarNameLen = VarStrLen
    else
        true
    ),
    ( if Type = type_variable(_, _) then
        true
    else
        !:AllTVars = not_all_tvars
    ).

:- func var_only_to_pieces(pair(string, string)) = list(format_piece).

var_only_to_pieces(VarStr - _TypeStr) = Pieces :-
    Pieces = [fixed(VarStr), nl].

:- func var_and_type_to_pieces(int, pair(string, string)) = list(format_piece).

var_and_type_to_pieces(MaxVarNameLen, VarStr - TypeStr) = Pieces :-
    string.pad_right(VarStr ++ ":", ' ', MaxVarNameLen, VarColonStr),
    Pieces = [fixed(VarColonStr), words(TypeStr), nl].

%---------------------------------------------------------------------------%

:- pred check_type_of_main(pred_info::in,
    list(error_spec)::in, list(error_spec)::out) is det.

check_type_of_main(PredInfo, !Specs) :-
    ( if
        % Check if this predicate is the program entry point main/2.
        pred_info_name(PredInfo) = "main",
        pred_info_get_orig_arity(PredInfo, pred_form_arity(2)),
        pred_info_is_exported(PredInfo)
    then
        pred_info_get_arg_types(PredInfo, ArgTypes),
        % Check that both arguments of main/2 have type `io.state'.
        (  if
            % This part of the test cannot fail, since we checked the arity.
            ArgTypes = [ArgType1, ArgType2],
            % These parts can fail.
            type_is_io_state(ArgType1),
            type_is_io_state(ArgType2)
        then
            true
        else
            pred_info_get_context(PredInfo, Context),
            Pieces = [words("Error: both arguments of"), quote("main/2"),
                words("must have type"), quote("io.state"), suffix("."), nl],
            Spec = simplest_spec($pred, severity_error, phase_type_check,
                Context, Pieces),
            !:Specs = [Spec | !.Specs]
        )
    else
        true
    ).

%---------------------------------------------------------------------------%

setup_var_table_in_clauses_for_imported_pred(ModuleInfo, !PredInfo) :-
    % Make sure the var_table field in the clauses_info is valid for imported
    % predicates. Unification and comparison procedures have their clauses
    % generated automatically, and the code that creates the clauses also
    % fills in the clauses' var_table.
    % NOTE The code that creates the clauses and fills in the var_table
    % is executed lazily, on demand.
    pred_info_get_clauses_info(!.PredInfo, ClausesInfo0),
    ( if pred_info_is_pseudo_imported(!.PredInfo) then
        clauses_info_get_var_table(ClausesInfo0, VarTable0),
        transform_var_table(fill_in_is_dummy_slot(ModuleInfo),
            VarTable0, VarTable),
        clauses_info_set_var_table(VarTable, ClausesInfo0, ClausesInfo)
    else
        clauses_info_get_varset(ClausesInfo0, VarSet),
        clauses_info_get_headvar_list(ClausesInfo0, HeadVars),
        pred_info_get_arg_types(!.PredInfo, ArgTypes),
        % This call fills in all the vte_is_dummy fields in VarTable.
        corresponding_vars_types_to_var_table(ModuleInfo, VarSet,
            HeadVars, ArgTypes, VarTable),
        clauses_info_set_var_table(VarTable, ClausesInfo0, ClausesInfo1),
        varset.init(EmptyVarSet),
        clauses_info_set_varset(EmptyVarSet, ClausesInfo1, ClausesInfo)
    ),
    pred_info_set_clauses_info(ClausesInfo, !PredInfo).

%---------------------------------------------------------------------------%

:- pred check_for_indistinguishable_modes(module_info::in, pred_id::in,
    pred_info::in, pred_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

check_for_indistinguishable_modes(ModuleInfo, PredId, !PredInfo, !Specs) :-
    ( if
        % Don't check for indistinguishable modes in unification predicates.
        % The default (in, in) mode must be semidet, but for single-value types
        % we also want to create a det mode which will be indistinguishable
        % from the semidet mode. (When the type is known, the det mode is
        % called, but the polymorphic unify needs to be able to call
        % the semidet mode.)
        pred_info_get_origin(!.PredInfo, Origin),
        Origin = origin_compiler(made_for_uci(spec_pred_unify, _))
    then
        true
    else
        ProcIds = pred_info_all_procids(!.PredInfo),
        check_for_indistinguishable_modes_in_procs(ModuleInfo, PredId,
            ProcIds, [], !PredInfo, !Specs)
    ).

:- pred check_for_indistinguishable_modes_in_procs(module_info::in,
    pred_id::in, list(proc_id)::in, list(proc_id)::in,
    pred_info::in, pred_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

check_for_indistinguishable_modes_in_procs(_, _, [], _, !PredInfo, !Specs).
check_for_indistinguishable_modes_in_procs(ModuleInfo, PredId,
        [ProcId | ProcIds], PrevProcIds, !PredInfo, !Specs) :-
    check_for_indistinguishable_mode(ModuleInfo, PredId, ProcId,
        PrevProcIds, Removed, !PredInfo, !Specs),
    (
        Removed = yes,
        PrevProcIds1 = PrevProcIds
    ;
        Removed = no,
        PrevProcIds1 = [ProcId | PrevProcIds]
    ),
    check_for_indistinguishable_modes_in_procs(ModuleInfo, PredId, ProcIds,
        PrevProcIds1, !PredInfo, !Specs).

:- pred check_for_indistinguishable_mode(module_info::in, pred_id::in,
    proc_id::in, list(proc_id)::in, bool::out,
    pred_info::in, pred_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

check_for_indistinguishable_mode(_, _, _, [], no, !PredInfo, !Specs).
check_for_indistinguishable_mode(ModuleInfo, PredId, ProcId1,
        [ProcId | ProcIds], Removed, !PredInfo, !Specs) :-
    ( if
        modes_are_indistinguishable(ModuleInfo, !.PredInfo, ProcId, ProcId1)
    then
        pred_info_get_status(!.PredInfo, Status),
        module_info_get_globals(ModuleInfo, Globals),
        ( if
            % XXX I (zs) don't understand the reason behind the logic
            % we use here to decide whether to report the error.
            (
                pred_status_defined_in_this_module(Status) = yes
            ;
                % With intermodule optimization, we can read the declarations
                % for a predicate from the `.int' and `.int0' files, so ignore
                % the error in those cases.
                %
                % XXX We should ignore the error only if we DID read the
                % predicate declaration from a place for which we shouldn't
                % report errors. This tests whether we COULD HAVE, which is
                % not the same thing.
                globals.lookup_bool_option(Globals, intermodule_optimization,
                    no),
                globals.lookup_bool_option(Globals, intermodule_analysis, no)
            ;
                globals.get_op_mode(Globals, OpMode),
                OpMode = opm_top_args(opma_augment(opmau_make_plain_opt), _)
            )
        then
            % XXX We shouldn't ignore the updated ModuleInfo, which may
            % differ from the old one in including an updated error count.
            Spec = report_indistinguishable_modes_error(ModuleInfo,
                ProcId1, ProcId, PredId, !.PredInfo),
            !:Specs = [Spec | !.Specs]
        else
            true
        ),
        % XXX doing this leaves dangling references the deleted proc_id in the
        % method definitions in the class table if the predicate being
        % processed is one of those introduced for type class methods.
        % See also: the comment above expand_class_method_body/5 in
        % polymorphism.m.
        pred_info_remove_procid(ProcId1, !PredInfo),
        Removed = yes
    else
        check_for_indistinguishable_mode(ModuleInfo, PredId, ProcId1,
            ProcIds, Removed, !PredInfo, !Specs)
    ).

    % Report an error for the case when two mode declarations
    % declare indistinguishable modes.
    %
:- func report_indistinguishable_modes_error(module_info, proc_id, proc_id,
    pred_id, pred_info) = error_spec.

report_indistinguishable_modes_error(ModuleInfo, OldProcId, NewProcId,
        PredId, PredInfo) = Spec :-
    pred_info_get_proc_table(PredInfo, Procs),
    map.lookup(Procs, OldProcId, OldProcInfo),
    map.lookup(Procs, NewProcId, NewProcInfo),
    proc_info_get_context(OldProcInfo, OldContext),
    proc_info_get_context(NewProcInfo, NewContext),

    MainPieces = [words("In mode declarations for ")] ++
        describe_one_pred_name(ModuleInfo, should_module_qualify, PredId)
        ++ [suffix(":"), nl, words("error: duplicate mode declaration."), nl],
    VerbosePieces = [words("Modes"),
        words_quote(mode_decl_to_string(output_mercury, OldProcId, PredInfo)),
        words("and"),
        words_quote(mode_decl_to_string(output_mercury, NewProcId, PredInfo)),
        words("are indistinguishable.")],
    OldPieces = [words("Here is the conflicting mode declaration.")],
    Spec = error_spec($pred, severity_error,
        phase_mode_check(report_in_any_mode),
        [simple_msg(NewContext,
            [always(MainPieces), verbose_only(verbose_always, VerbosePieces)]),
        simplest_msg(OldContext, OldPieces)]).

%---------------------------------------------------------------------------%
:- end_module check_hlds.post_typecheck.
%---------------------------------------------------------------------------%
