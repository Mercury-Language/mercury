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
:- import_module parse_tree.error_util.

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
:- pred setup_vartypes_in_clauses_for_imported_pred(pred_info::in,
    pred_info::out) is det.

    % Propagate type information into the argument modes of all
    % the procedures of the given predicate.
    %
    % Return a list of the ids of the procedures in which this process failed,
    % and a list of errors describing situations in which a named inst
    % was applied to a type whose type constructor is *not* the type
    % constructor that this inst was declared to be for.
    %
    % This predicate is exported for use by add_special_pred.m, which does
    % most of the task of this module itself, but delegates this one to us.
    %
:- pred propagate_checked_types_into_pred_modes(module_info::in,
    list(proc_id)::out, list(error_spec)::out,
    pred_info::in, pred_info::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.inst_match.
:- import_module check_hlds.inst_mode_type_prop.
:- import_module check_hlds.mode_comparison.
:- import_module check_hlds.mode_errors.
:- import_module check_hlds.type_util.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_class.
:- import_module hlds.hlds_clauses.
:- import_module hlds.hlds_error_util.
:- import_module hlds.hlds_goal.
:- import_module hlds.status.
:- import_module hlds.vartypes.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.op_mode.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_type.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module set.
:- import_module solutions.
:- import_module string.
:- import_module set_tree234.
:- import_module varset.

%---------------------------------------------------------------------------%

post_typecheck_finish_preds(!ModuleInfo, NumBadErrors,
        AlwaysSpecs, NoTypeErrorSpecs) :-
    module_info_get_valid_pred_ids(!.ModuleInfo, ValidPredIds),
    ValidPredIdSet = set_tree234.list_to_set(ValidPredIds),
    module_info_get_pred_id_table(!.ModuleInfo, PredIdTable0),
    map.map_foldl3(post_typecheck_do_finish_pred(!.ModuleInfo, ValidPredIdSet),
        PredIdTable0, PredIdTable, 0, NumBadErrors,
        [], AlwaysSpecs, [], NoTypeErrorSpecs),
    module_info_set_pred_id_table(PredIdTable, !ModuleInfo).

:- pred post_typecheck_do_finish_pred(module_info::in,
    set_tree234(pred_id)::in,
    pred_id::in, pred_info::in, pred_info::out, int::in, int::out,
    list(error_spec)::in, list(error_spec)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

post_typecheck_do_finish_pred(ModuleInfo, ValidPredIdSet, PredId, !PredInfo,
        !NumBadErrors, !AlwaysSpecs, !NoTypeErrorSpecs) :-
    ( if set_tree234.contains(ValidPredIdSet, PredId) then
        ( if
            ( pred_info_is_imported(!.PredInfo)
            ; pred_info_is_pseudo_imported(!.PredInfo)
            )
        then
            setup_vartypes_in_clauses_for_imported_pred(!PredInfo)
        else
            find_unproven_body_constraints(ModuleInfo, PredId, !.PredInfo,
                !NumBadErrors, !NoTypeErrorSpecs),
            find_unresolved_types_in_pred(ModuleInfo, PredId, !PredInfo,
                !NoTypeErrorSpecs),
            check_type_of_main(!.PredInfo, !AlwaysSpecs)
        ),
        propagate_checked_types_into_pred_modes(ModuleInfo, ErrorProcs,
            InstForTypeSpecs, !PredInfo),
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
            list.map(constraint_to_error_piece(TVarSet), Constraints), []) ++
        [nl_indent_delta(-1)],
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
    = list(format_component).

constraint_to_error_piece(TVarset, Constraint) =
    [quote(mercury_constraint_to_string(TVarset, Constraint))].

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
    = list(format_component).

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
            CallPieces = [qual_pf_sym_name_orig_arity(PFSymNameArity)]
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

    % Check that the all of the types which have been inferred for the
    % variables in the predicate do not contain any unbound type variables
    % other than those that occur in the types of head variables, and that
    % there are no unsatisfied type class constraints.
    %
:- pred find_unresolved_types_in_pred(module_info::in, pred_id::in,
    pred_info::in, pred_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.
:- pragma inline(pred(find_unresolved_types_in_pred/6)).

find_unresolved_types_in_pred(ModuleInfo, PredId, !PredInfo,
        !NoTypeErrorSpecs) :-
    pred_info_get_clauses_info(!.PredInfo, ClausesInfo0),
    pred_info_get_external_type_params(!.PredInfo, ExternalTypeParams),
    clauses_info_get_varset(ClausesInfo0, VarSet),
    clauses_info_get_vartypes(ClausesInfo0, VarTypesMap0),
    vartypes_to_sorted_assoc_list(VarTypesMap0, VarTypesList),
    set.init(BindToVoidTVars0),
    find_unresolved_types_in_vars(VarTypesList, ExternalTypeParams,
        [], UnresolvedVarsTypes, BindToVoidTVars0, BindToVoidTVars),
    (
        UnresolvedVarsTypes = []
    ;
        UnresolvedVarsTypes = [_ | _],
        report_unresolved_type_warning(ModuleInfo, PredId, !.PredInfo,
            VarSet, UnresolvedVarsTypes, !NoTypeErrorSpecs),

        % Bind all the type variables in `BindToVoidTVars' to `void' ...
        pred_info_get_constraint_proof_map(!.PredInfo, ProofMap0),
        pred_info_get_constraint_map(!.PredInfo, ConstraintMap0),
        bind_type_vars_to_void(BindToVoidTVars, VarTypesMap0, VarTypesMap,
            ProofMap0, ProofMap, ConstraintMap0, ConstraintMap),
        clauses_info_set_vartypes(VarTypesMap, ClausesInfo0, ClausesInfo),
        pred_info_set_clauses_info(ClausesInfo, !PredInfo),
        pred_info_set_constraint_proof_map(ProofMap, !PredInfo),
        pred_info_set_constraint_map(ConstraintMap, !PredInfo)
    ).

    % The number of variables can be huge here (hundred of thousands for
    % Doug Auclair's training_cars program). The code below prevents stack
    % overflows in grades that do not permit tail recursion.
    %
:- pred find_unresolved_types_in_vars(assoc_list(prog_var, mer_type)::in,
    list(tvar)::in,
    assoc_list(prog_var, mer_type)::in, assoc_list(prog_var, mer_type)::out,
    set(tvar)::in, set(tvar)::out) is det.

find_unresolved_types_in_vars(VarTypes, ExternalTypeParams,
        !UnresolvedVarsTypes, !BindToVoidTVars) :-
    find_unresolved_types_in_vars_inner(VarTypes, ExternalTypeParams, 1000,
        LeftOverVarTypes, !UnresolvedVarsTypes, !BindToVoidTVars),
    (
        LeftOverVarTypes = []
    ;
        LeftOverVarTypes = [_ | _],
        find_unresolved_types_in_vars(LeftOverVarTypes, ExternalTypeParams,
            !UnresolvedVarsTypes, !BindToVoidTVars)
    ).

:- pred find_unresolved_types_in_vars_inner(assoc_list(prog_var, mer_type)::in,
    list(tvar)::in, int::in, assoc_list(prog_var, mer_type)::out,
    assoc_list(prog_var, mer_type)::in, assoc_list(prog_var, mer_type)::out,
    set(tvar)::in, set(tvar)::out) is det.

find_unresolved_types_in_vars_inner([], _, _, [],
        !UnresolvedVarsTypes, !BindToVoidTVars).
find_unresolved_types_in_vars_inner([Var - Type | VarTypes],
        ExternalTypeParams, VarsToDo, LeftOverVarTypes,
        !UnresolvedVarsTypes, !BindToVoidTVars) :-
    ( if VarsToDo < 0 then
        LeftOverVarTypes = [Var - Type | VarTypes]
    else
        type_vars(Type, TVars),
        set.list_to_set(TVars, TVarsSet0),
        set.delete_list(ExternalTypeParams, TVarsSet0, TVarsSet1),
        ( if set.is_empty(TVarsSet1) then
            true
        else
            !:UnresolvedVarsTypes = [Var - Type | !.UnresolvedVarsTypes],
            set.union(TVarsSet1, !BindToVoidTVars)
        ),
        find_unresolved_types_in_vars_inner(VarTypes, ExternalTypeParams,
            VarsToDo - 1, LeftOverVarTypes,
            !UnresolvedVarsTypes, !BindToVoidTVars)
    ).

    % Bind all the type variables in `UnboundTypeVarsSet' to the type `void'.
    %
:- pred bind_type_vars_to_void(set(tvar)::in, vartypes::in, vartypes::out,
    constraint_proof_map::in, constraint_proof_map::out,
    constraint_map::in, constraint_map::out) is det.

bind_type_vars_to_void(UnboundTypeVarsSet, !VarTypes, !ProofMap,
        !ConstraintMap) :-
    % Create a substitution that maps all of the unbound type variables
    % to `void'.
    MapToVoid =
        ( pred(TVar::in, Subst0::in, Subst::out) is det :-
            map.det_insert(TVar, void_type, Subst0, Subst)
        ),
    set.fold(MapToVoid, UnboundTypeVarsSet, map.init, VoidSubst),

    % Then apply the substitution we just created to the various maps.
    apply_subst_to_vartypes(VoidSubst, !VarTypes),
    apply_subst_to_constraint_proof_map(VoidSubst, !ProofMap),
    apply_subst_to_constraint_map(VoidSubst, !ConstraintMap).

%---------------------%

    % Report a warning: uninstantiated type parameter.
    %
:- pred report_unresolved_type_warning(module_info::in, pred_id::in,
    pred_info::in, prog_varset::in, assoc_list(prog_var, mer_type)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_unresolved_type_warning(ModuleInfo, PredId, PredInfo, VarSet, Errs,
        !Specs) :-
    pred_info_get_typevarset(PredInfo, TypeVarSet),
    pred_info_get_context(PredInfo, Context),

    PredIdPieces =
        describe_one_pred_name(ModuleInfo, should_not_module_qualify, PredId),
    VarTypePieceLists =
        list.map(var_and_type_to_pieces(VarSet, TypeVarSet), Errs),
    list.condense(VarTypePieceLists, VarTypePieces),
    MainPieces = [words("In")] ++ PredIdPieces ++ [suffix(":"), nl,
        words("warning: unresolved polymorphism."), nl,
        words(choose_number(Errs,
            "The variable with an unbound type was:",
            "The variables with unbound types were:")), nl_indent_delta(1)] ++
        VarTypePieces ++
        [nl_indent_delta(-1), words("The unbound type"),
        words(choose_number(Errs, "variable", "variables")),
        words("will be implicitly bound to the builtin type"),
        quote("void"), suffix("."), nl],
    VerbosePieces = [words("The body of the clause contains a call"),
        words("to a polymorphic predicate,"),
        words("but I can't determine which version should be called,"),
        words("because the type variables listed above didn't get bound."),
        % words("You may need to use an explicit type qualifier."),
        % XXX improve error message
        words("(I ought to tell you which call caused the problem,"),
        words("but I'm afraid you'll have to work it out yourself."),
        words("My apologies.)")],
    Msg = simple_msg(Context,
        [always(MainPieces), verbose_only(verbose_once, VerbosePieces)]),
    Spec = conditional_spec($pred, warn_unresolved_polymorphism, yes,
        severity_warning, phase_type_check, [Msg]),
    !:Specs = [Spec | !.Specs].

:- func var_and_type_to_pieces(prog_varset, tvarset,
    pair(prog_var, mer_type)) = list(format_component).

var_and_type_to_pieces(VarSet, TVarSet, Var - Type) =
    [words(mercury_var_to_string(VarSet, print_name_only, Var)), suffix(":"),
    words(mercury_type_to_string(TVarSet, print_name_only, Type)), nl].

%---------------------------------------------------------------------------%

:- pred check_type_of_main(pred_info::in,
    list(error_spec)::in, list(error_spec)::out) is det.

check_type_of_main(PredInfo, !Specs) :-
    ( if
        % Check if this predicate is the program entry point main/2.
        pred_info_name(PredInfo) = "main",
        pred_info_orig_arity(PredInfo) = 2,
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

setup_vartypes_in_clauses_for_imported_pred(!PredInfo) :-
    % Make sure the vartypes field in the clauses_info is valid for imported
    % predicates. Unification and comparison procedures have their clauses
    % generated automatically, and the code that creates the clauses also
    % fills in the clauses' vartypes.
    ( if pred_info_is_pseudo_imported(!.PredInfo) then
        true
    else
        pred_info_get_clauses_info(!.PredInfo, ClausesInfo0),
        clauses_info_get_headvar_list(ClausesInfo0, HeadVars),
        pred_info_get_arg_types(!.PredInfo, ArgTypes),
        vartypes_from_corresponding_lists(HeadVars, ArgTypes, VarTypes),
        clauses_info_set_vartypes(VarTypes, ClausesInfo0, ClausesInfo),
        pred_info_set_clauses_info(ClausesInfo, !PredInfo)
    ).

%---------------------------------------------------------------------------%

propagate_checked_types_into_pred_modes(ModuleInfo, ErrorProcIds,
        !:Specs, !PredInfo) :-
    pred_info_get_proc_table(!.PredInfo, Procs0),
    ProcIds = pred_info_all_procids(!.PredInfo),
    !:Specs = [],
    propagate_checked_types_into_procs_modes(ModuleInfo, !.PredInfo, ProcIds,
        [], RevErrorProcIds, !Specs, Procs0, Procs),
    list.reverse(RevErrorProcIds, ErrorProcIds),
    pred_info_set_proc_table(Procs, !PredInfo),
    pred_info_get_markers(!.PredInfo, Markers),
    ( if check_marker(Markers, marker_has_rhs_lambda) then
        % We have not copied goals in clauses to become the bodies
        % of procedures, so the lambda expressions in whose arguments
        % we should propagate types into insts exist only in the clauses_info.
        pred_info_get_clauses_info(!.PredInfo, ClausesInfo0),
        clauses_info_get_had_syntax_errors(ClausesInfo0, FoundSyntaxError),
        (
            FoundSyntaxError = some_clause_syntax_errors
            % Any errors we could generate could be spurious. Any that aren't
            % will be found and reported by the first compiler invocation
            % after the user fixes the syntax errors.
        ;
            FoundSyntaxError = no_clause_syntax_errors,
            clauses_info_get_clauses_rep(ClausesInfo0, ClausesRep0, ItemNums),
            get_clause_list_for_replacement(ClausesRep0, Clauses0),
            VarTypes = ClausesInfo0 ^ cli_vartypes,
            propagate_checked_types_into_lambda_modes_in_clauses(ModuleInfo,
                VarTypes, Clauses0, Clauses, !Specs),
            set_clause_list(Clauses, ClausesRep),
            clauses_info_set_clauses_rep(ClausesRep, ItemNums,
                ClausesInfo0, ClausesInfo),
            pred_info_set_clauses_info(ClausesInfo, !PredInfo)
        )
    else
        true
    ).

%---------------------%

:- pred propagate_checked_types_into_procs_modes(module_info::in,
    pred_info::in, list(proc_id)::in, list(proc_id)::in, list(proc_id)::out,
    list(error_spec)::in, list(error_spec)::out,
    proc_table::in, proc_table::out) is det.

propagate_checked_types_into_procs_modes(_, _, [],
        !RevErrorProcIds, !Specs, !Procs).
propagate_checked_types_into_procs_modes(ModuleInfo, PredInfo,
        [ProcId | ProcIds], !RevErrorProcIds, !Specs, !Procs) :-
    propagate_checked_types_into_proc_modes(ModuleInfo, PredInfo, ProcId,
        !RevErrorProcIds, !Specs, !Procs),
    propagate_checked_types_into_procs_modes(ModuleInfo, PredInfo, ProcIds,
        !RevErrorProcIds, !Specs, !Procs).

:- pred propagate_checked_types_into_proc_modes(module_info::in,
    pred_info::in, proc_id::in, list(proc_id)::in, list(proc_id)::out,
    list(error_spec)::in, list(error_spec)::out,
    proc_table::in, proc_table::out) is det.

propagate_checked_types_into_proc_modes(ModuleInfo, PredInfo, ProcId,
        !RevErrorProcIds, !Specs, !Procs) :-
    pred_info_get_arg_types(PredInfo, ArgTypes),
    map.lookup(!.Procs, ProcId, ProcInfo0),
    proc_info_get_argmodes(ProcInfo0, ArgModes0),
    propagate_checked_types_into_modes(ModuleInfo, ta_pred(PredInfo),
        ArgTypes, ArgModes0, ArgModes, !Specs),

    % Check for unbound inst vars.
    %
    % This needs to be done after calling propagate_checked_types_into_modes,
    % because we need the insts to be module qualified.
    %
    % It also needs to be done before mode analysis, to avoid internal errors
    % in mode analysis.
    ( if
        mode_list_contains_inst_var(ArgModes, _InstVar)
        % XXX This should be
        %   some [InstVar] (
        %       mode_list_contains_inst_var(ArgModes, InstVar)
        %   )
        % but that gets a singleton variable warning, because quantification.m
        % replaces the list of quantified variables with the empty list
        % BEFORE the singleton variable warning is generated.
    then
        !:RevErrorProcIds = [ProcId | !.RevErrorProcIds]
    else
        proc_info_set_argmodes(ArgModes, ProcInfo0, ProcInfo),
        map.det_update(ProcId, ProcInfo, !Procs)
    ).

%---------------------%

:- pred propagate_checked_types_into_lambda_modes_in_clauses(module_info::in,
    vartypes::in, list(clause)::in, list(clause)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

propagate_checked_types_into_lambda_modes_in_clauses(_, _, [], [], !Specs).
propagate_checked_types_into_lambda_modes_in_clauses(ModuleInfo, VarTypes,
        [Clause0 | Clauses0], [Clause | Clauses], !Specs) :-
    propagate_checked_types_into_lambda_modes_in_clause(ModuleInfo, VarTypes,
        Clause0, Clause, !Specs),
    propagate_checked_types_into_lambda_modes_in_clauses(ModuleInfo, VarTypes,
        Clauses0, Clauses, !Specs).

:- pred propagate_checked_types_into_lambda_modes_in_clause(module_info::in,
    vartypes::in, clause::in, clause::out,
    list(error_spec)::in, list(error_spec)::out) is det.

propagate_checked_types_into_lambda_modes_in_clause(ModuleInfo, VarTypes,
        Clause0, Clause, !Specs) :-
    Lang = Clause0 ^ clause_lang,
    (
        Lang = impl_lang_mercury,
        Goal0 = Clause0 ^ clause_body,
        propagate_checked_types_into_lambda_modes_in_goal(ModuleInfo, VarTypes,
            Goal0, Goal, !Specs),
        Clause = Clause0 ^ clause_body := Goal
    ;
        Lang = impl_lang_foreign(_),
        Clause = Clause0
    ).

:- pred propagate_checked_types_into_lambda_modes_in_goal(module_info::in,
    vartypes::in, hlds_goal::in, hlds_goal::out,
    list(error_spec)::in, list(error_spec)::out) is det.

propagate_checked_types_into_lambda_modes_in_goal(ModuleInfo,
        VarTypes, Goal0, Goal, !Specs) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    (
        GoalExpr0 = unify(LHS0, RHS0, UnifyMode0, Unification0, UniContext0),
        (
            ( RHS0 = rhs_var(_)
            ; RHS0 = rhs_functor(_, _, _)
            ),
            Goal = Goal0
        ;
            RHS0 = rhs_lambda_goal(Purity0, HOGroundness0, PorF0, EvalMethod0,
                ClosureVars0, ArgVarsModes0, Detism0, LambdaGoal0),
            list.length(ArgVarsModes0, NumArgs),
            Context = goal_info_get_context(GoalInfo0),
            Args = ta_lambda(PorF0, NumArgs, Context),
            propagate_checked_types_into_var_modes(ModuleInfo,
                VarTypes, Args, 1, ArgVarsModes0, ArgVarsModes, !Specs),
            propagate_checked_types_into_lambda_modes_in_goal(ModuleInfo,
                VarTypes, LambdaGoal0, LambdaGoal, !Specs),
            RHS = rhs_lambda_goal(Purity0, HOGroundness0, PorF0, EvalMethod0,
                ClosureVars0, ArgVarsModes, Detism0, LambdaGoal),
            GoalExpr = unify(LHS0, RHS, UnifyMode0, Unification0, UniContext0),
            Goal = hlds_goal(GoalExpr, GoalInfo0)
        )
    ;
        ( GoalExpr0 = plain_call(_, _, _, _, _, _)
        ; GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _)
        ; GoalExpr0 = generic_call(_, _, _, _, _)
        ),
        Goal = Goal0
    ;
        GoalExpr0 = conj(ConjType, Conjuncts0),
        propagate_checked_types_into_lambda_modes_in_goals(ModuleInfo,
            VarTypes, Conjuncts0, Conjuncts, !Specs),
        GoalExpr = conj(ConjType, Conjuncts),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = disj(Disjuncts0),
        propagate_checked_types_into_lambda_modes_in_goals(ModuleInfo,
            VarTypes, Disjuncts0, Disjuncts, !Specs),
        GoalExpr = disj(Disjuncts),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = switch(Var0, CanFail0, Cases0),
        propagate_checked_types_into_lambda_modes_in_cases(ModuleInfo,
            VarTypes, Cases0, Cases, !Specs),
        GoalExpr = switch(Var0, CanFail0, Cases),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = if_then_else(Vars0, Cond0, Then0, Else0),
        propagate_checked_types_into_lambda_modes_in_goal(ModuleInfo,
            VarTypes, Cond0, Cond, !Specs),
        propagate_checked_types_into_lambda_modes_in_goal(ModuleInfo,
            VarTypes, Then0, Then, !Specs),
        propagate_checked_types_into_lambda_modes_in_goal(ModuleInfo,
            VarTypes, Else0, Else, !Specs),
        GoalExpr = if_then_else(Vars0, Cond, Then, Else),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = negation(SubGoal0),
        propagate_checked_types_into_lambda_modes_in_goal(ModuleInfo,
            VarTypes, SubGoal0, SubGoal, !Specs),
        GoalExpr = negation(SubGoal),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = scope(Reason0, SubGoal0),
        propagate_checked_types_into_lambda_modes_in_goal(ModuleInfo,
            VarTypes, SubGoal0, SubGoal, !Specs),
        GoalExpr = scope(Reason0, SubGoal),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = shorthand(ShortHand0),
        (
            ShortHand0 = bi_implication(GoalA0, GoalB0),
            propagate_checked_types_into_lambda_modes_in_goal(ModuleInfo,
                VarTypes, GoalA0, GoalA, !Specs),
            propagate_checked_types_into_lambda_modes_in_goal(ModuleInfo,
                VarTypes, GoalB0, GoalB, !Specs),
            ShortHand = bi_implication(GoalA, GoalB)
        ;
            ShortHand0 = atomic_goal(AtomicGoalType0, OuterVars0, InnerVars0,
                OutputVars0, MainGoal0, OrElseGoals0, OrElseInners0),
            propagate_checked_types_into_lambda_modes_in_goal(ModuleInfo,
                VarTypes, MainGoal0, MainGoal, !Specs),
            propagate_checked_types_into_lambda_modes_in_goals(ModuleInfo,
                VarTypes, OrElseGoals0, OrElseGoals, !Specs),
            ShortHand = atomic_goal(AtomicGoalType0, OuterVars0, InnerVars0,
                OutputVars0, MainGoal, OrElseGoals, OrElseInners0)
        ;
            ShortHand0 = try_goal(MaybeIOVars0, ResultVars0, SubGoal0),
            propagate_checked_types_into_lambda_modes_in_goal(ModuleInfo,
                VarTypes, SubGoal0, SubGoal, !Specs),
            ShortHand = try_goal(MaybeIOVars0, ResultVars0, SubGoal)
        ),
        GoalExpr = shorthand(ShortHand),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ).

:- pred propagate_checked_types_into_lambda_modes_in_goals(module_info::in,
    vartypes::in, list(hlds_goal)::in, list(hlds_goal)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

propagate_checked_types_into_lambda_modes_in_goals(_, _, [], [], !Specs).
propagate_checked_types_into_lambda_modes_in_goals(ModuleInfo, VarTypes,
        [Goal0 | Goals0], [Goal | Goals], !Specs) :-
    propagate_checked_types_into_lambda_modes_in_goal(ModuleInfo, VarTypes,
        Goal0, Goal, !Specs),
    propagate_checked_types_into_lambda_modes_in_goals(ModuleInfo, VarTypes,
        Goals0, Goals, !Specs).

:- pred propagate_checked_types_into_lambda_modes_in_cases(module_info::in,
    vartypes::in, list(case)::in, list(case)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

propagate_checked_types_into_lambda_modes_in_cases(_, _, [], [], !Specs).
propagate_checked_types_into_lambda_modes_in_cases(ModuleInfo, VarTypes,
        [Case0 | Cases0], [Case | Cases], !Specs) :-
    Case0 = case(MainConsId0, OtherConsIds0, Goal0),
    propagate_checked_types_into_lambda_modes_in_goal(ModuleInfo, VarTypes,
        Goal0, Goal, !Specs),
    Case = case(MainConsId0, OtherConsIds0, Goal),
    propagate_checked_types_into_lambda_modes_in_cases(ModuleInfo, VarTypes,
        Cases0, Cases, !Specs).

%---------------------%

:- pred propagate_checked_types_into_var_modes(module_info::in, vartypes::in,
    tprop_args::in, int::in,
    assoc_list(prog_var, mer_mode)::in, assoc_list(prog_var, mer_mode)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

propagate_checked_types_into_var_modes(_, _, _, _, [], [], !Specs).
propagate_checked_types_into_var_modes(ModuleInfo, VarTypes, Args, ArgNum,
        [Var - Mode0 | VarsModes0], [Var - Mode | VarsModes], !Specs) :-
    lookup_var_type(VarTypes, Var, Type),
    Context = tprop_arg_list_slot(Args, ArgNum),
    propagate_checked_type_into_mode(ModuleInfo, Context,
        Type, Mode0, Mode, !Specs),
    propagate_checked_types_into_var_modes(ModuleInfo, VarTypes,
        Args, ArgNum + 1, VarsModes0, VarsModes, !Specs).

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
        Origin = origin_special_pred(spec_pred_unify, _)
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
        modes_are_indistinguishable(ProcId, ProcId1, !.PredInfo, ModuleInfo)
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
                OpMode = opm_top_args(opma_augment(opmau_make_opt_int))
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
