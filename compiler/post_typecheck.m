%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1997-2012,2014 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File      : post_typecheck.m
% Author    : fjh
% Purpose   : finish off type checking.
%
% This module does the final parts of type analysis:
%
%   - it resolves predicate overloading
%   - it resolves function overloading
%   - it expands field access functions
%   - it propagates type information into the modes of procedures
%   - it checks for unbound type variables and if there are any,
%     it reports an error (or a warning, binding them to the type `void').
%   - it reports errors for unbound inst variables in predicate or
%     function mode declarations
%   - it reports errors for unsatisfied type class constraints
%   - it reports an error if there are indistinguishable modes for
%     a predicate of function.
%   - it checks that declarations for abstract types also have a
%     corresponding definition somewhere in the module.
%
% These actions cannot be done until after type inference is complete,
% so they need to be a separate "post-typecheck pass".  For efficiency
% reasons, this is in fact done at the same time as purity analysis --
% the routines here are called from purity.m rather than mercury_compile.m.

%-----------------------------------------------------------------------------%

:- module check_hlds.post_typecheck.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.

:- import_module list.

%-----------------------------------------------------------------------------%

    % Check that every abstract type in the module has at least one definition
    % in either the interface or implementation of the module.
    %
    % Note that a type may have several definitions, e.g. some foreign
    % definitions and a default Mercury definition.
    %
:- pred check_for_missing_type_defns(module_info::in, list(error_spec)::out)
    is det.

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

    % As above, but return the list of procedures containing unbound inst
    % variables instead of reporting the errors directly.
    % XXX This is incredibly misleading.
    %
:- pred post_typecheck_finish_pred_no_io(module_info::in, list(proc_id)::out,
    pred_info::in, pred_info::out) is det.

:- pred post_typecheck_finish_imported_pred_no_io(module_info::in,
    list(proc_id)::out, pred_info::in, pred_info::out) is det.

    % Now that the assertion has finished being typechecked, remove it
    % from further processing and store it in the assertion_table.
    %
:- pred post_typecheck_finish_promise(promise_type::in, pred_id::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

    % Handle any unresolved overloading for a predicate call.
    %
:- pred finally_resolve_pred_overloading(list(prog_var)::in,
    pred_info::in, module_info::in, prog_context::in,
    sym_name::in, sym_name::out, pred_id::in, pred_id::out) is det.

:- type is_plain_unify
    --->    is_not_plain_unify
    ;       is_plain_unify
    ;       is_unknown_ref(error_spec).

    % Work out whether a var-functor unification is actually a function call.
    % If so, replace the unification goal with a call.
    %
:- pred resolve_unify_functor(prog_var::in, cons_id::in,
    list(prog_var)::in, unify_mode::in, unification::in, unify_context::in,
    hlds_goal_info::in, module_info::in, pred_info::in, pred_info::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    hlds_goal::out, is_plain_unify::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.inst_match.
:- import_module check_hlds.modecheck_call.
:- import_module check_hlds.mode_errors.
:- import_module check_hlds.mode_util.
:- import_module check_hlds.type_util.
:- import_module hlds.assertion.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_clauses.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_error_util.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_out.hlds_out_util.
:- import_module hlds.pred_table.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.goal_path.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_subst.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.set_of_var.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module solutions.
:- import_module string.
:- import_module set_tree234.
:- import_module term_io.
:- import_module varset.

%-----------------------------------------------------------------------------%

post_typecheck_finish_preds(!ModuleInfo, NumBadErrors,
        AlwaysSpecs, NoTypeErrorSpecs) :-
    module_info_get_valid_predids(ValidPredIds, !ModuleInfo),
    ValidPredIdSet = set_tree234.list_to_set(ValidPredIds),
    module_info_get_preds(!.ModuleInfo, PredMap0),
    map.to_assoc_list(PredMap0, PredIdsInfos0),
    post_typecheck_do_finish_preds(!.ModuleInfo, ValidPredIdSet,
        PredIdsInfos0, PredIdsInfos, NumBadErrors,
        AlwaysSpecsList, NoTypeErrorSpecsList),
    list.condense(AlwaysSpecsList, AlwaysSpecs),
    list.condense(NoTypeErrorSpecsList, NoTypeErrorSpecs),
    map.from_sorted_assoc_list(PredIdsInfos, PredMap),
    module_info_set_preds(PredMap, !ModuleInfo).

:- pred post_typecheck_do_finish_preds(module_info::in,
    set_tree234(pred_id)::in,
    assoc_list(pred_id, pred_info)::in, assoc_list(pred_id, pred_info)::out,
    int::out, list(list(error_spec))::out, list(list(error_spec))::out) is det.

post_typecheck_do_finish_preds(_, _, [], [], 0, [], []).
post_typecheck_do_finish_preds(ModuleInfo, ValidPredIdSet,
        [PredIdInfo0 | PredIdsInfos0], [PredIdInfo | PredIdsInfos],
        NumBadErrors, [HeadAlwaysSpecs | TailAlwaysSpecs],
        [HeadNoTypeErrorSpecs | TailNoTypeErrorSpecs]) :-
    PredIdInfo0 = PredId - PredInfo0,
    ( set_tree234.contains(ValidPredIdSet, PredId) ->
        post_typecheck_do_finish_pred(ModuleInfo, PredId, PredInfo0, PredInfo,
            HeadNumBadErrors, HeadAlwaysSpecs, HeadNoTypeErrorSpecs)
    ;
        PredInfo = PredInfo0,
        HeadNumBadErrors = 0,
        HeadAlwaysSpecs = [],
        HeadNoTypeErrorSpecs = []
    ),
    PredIdInfo = PredId - PredInfo,
    post_typecheck_do_finish_preds(ModuleInfo, ValidPredIdSet,
        PredIdsInfos0, PredIdsInfos, TailNumBadErrors,
        TailAlwaysSpecs, TailNoTypeErrorSpecs),
    NumBadErrors = HeadNumBadErrors + TailNumBadErrors.

:- pred post_typecheck_do_finish_pred(module_info::in,
    pred_id::in, pred_info::in, pred_info::out, int::out,
    list(error_spec)::out, list(error_spec)::out) is det.

post_typecheck_do_finish_pred(ModuleInfo, PredId, !PredInfo, NumBadErrors,
        !:AlwaysSpecs, !:NoTypeErrorSpecs) :-
    (
        ( pred_info_is_imported(!.PredInfo)
        ; pred_info_is_pseudo_imported(!.PredInfo)
        )
    ->
        % For imported preds, we just need to ensure that all constructors
        % occurring in predicate mode declarations are module qualified.
        post_typecheck_finish_imported_pred_no_io(ModuleInfo, ErrorProcs,
            !PredInfo),
        report_unbound_inst_vars(ModuleInfo, PredId, ErrorProcs, !PredInfo,
            [], !:AlwaysSpecs),
        check_for_indistinguishable_modes(ModuleInfo, PredId, !PredInfo,
            !AlwaysSpecs),
        !:NoTypeErrorSpecs = [],
        NumBadErrors = 0
    ;
        check_pred_type_bindings(ModuleInfo, PredId, !PredInfo,
            NumBadErrors, !:NoTypeErrorSpecs),

        post_typecheck_finish_pred_no_io(ModuleInfo, ErrorProcs, !PredInfo),
        report_unbound_inst_vars(ModuleInfo, PredId, ErrorProcs, !PredInfo,
            [], !:AlwaysSpecs),
        check_for_indistinguishable_modes(ModuleInfo, PredId, !PredInfo,
            !AlwaysSpecs),

        % Check that main/2 has the right type.
        check_type_of_main(!.PredInfo, !AlwaysSpecs)
    ).

%-----------------------------------------------------------------------------%

    % Check that the all of the types which have been inferred for the
    % variables in the predicate do not contain any unbound type variables
    % other than those that occur in the types of head variables, and that
    % there are no unsatisfied type class constraints.
    %
:- pred check_pred_type_bindings(module_info::in, pred_id::in,
    pred_info::in, pred_info::out, int::out, list(error_spec)::out) is det.

check_pred_type_bindings(ModuleInfo, PredId, !PredInfo, NumBadErrors,
        !:NoTypeErrorSpecs) :-
    pred_info_get_unproven_body_constraints(!.PredInfo, UnprovenConstraints0),
    !:NoTypeErrorSpecs = [],
    (
        UnprovenConstraints0 = [_ | _],
        list.sort_and_remove_dups(UnprovenConstraints0, UnprovenConstraints),
        report_unsatisfied_constraints(ModuleInfo, PredId, !.PredInfo,
            UnprovenConstraints, !NoTypeErrorSpecs),
        list.length(UnprovenConstraints, NumBadErrors)
    ;
        UnprovenConstraints0 = [],
        NumBadErrors = 0
    ),

    pred_info_get_clauses_info(!.PredInfo, ClausesInfo0),
    pred_info_get_head_type_params(!.PredInfo, HeadTypeParams),
    clauses_info_get_varset(ClausesInfo0, VarSet),
    clauses_info_get_vartypes(ClausesInfo0, VarTypesMap0),
    vartypes_to_assoc_list(VarTypesMap0, VarTypesList),
    set.init(Set0),
    check_var_type_bindings(VarTypesList, HeadTypeParams,
        [], UnresolvedVarsTypes, Set0, Set),
    (
        UnresolvedVarsTypes = []
    ;
        UnresolvedVarsTypes = [_ | _],
        report_unresolved_type_warning(ModuleInfo, PredId, !.PredInfo,
            VarSet, UnresolvedVarsTypes, !NoTypeErrorSpecs),

        % Bind all the type variables in `Set' to `void' ...
        pred_info_get_constraint_proofs(!.PredInfo, Proofs0),
        pred_info_get_constraint_map(!.PredInfo, ConstraintMap0),
        bind_type_vars_to_void(Set, VarTypesMap0, VarTypesMap, Proofs0, Proofs,
            ConstraintMap0, ConstraintMap),
        clauses_info_set_vartypes(VarTypesMap, ClausesInfo0, ClausesInfo),
        pred_info_set_clauses_info(ClausesInfo, !PredInfo),
        pred_info_set_constraint_proofs(Proofs, !PredInfo),
        pred_info_set_constraint_map(ConstraintMap, !PredInfo)
    ).

    % The number of variables can be huge here (hundred of thousands for
    % Doug Auclair's training_cars program). The code below prevents stack
    % overflows in grades that do not permit tail recursion.
    %
:- pred check_var_type_bindings(assoc_list(prog_var, mer_type)::in,
    list(tvar)::in,
    assoc_list(prog_var, mer_type)::in, assoc_list(prog_var, mer_type)::out,
    set(tvar)::in, set(tvar)::out) is det.

check_var_type_bindings(VarTypes, HeadTypeParams, !UnresolvedVarsTypes,
        !Set) :-
    check_var_type_bindings_2(VarTypes, HeadTypeParams, 1000, LeftOverVarTypes,
        !UnresolvedVarsTypes, !Set),
    (
        LeftOverVarTypes = []
    ;
        LeftOverVarTypes = [_ | _],
        check_var_type_bindings(LeftOverVarTypes, HeadTypeParams,
            !UnresolvedVarsTypes, !Set)
    ).

:- pred check_var_type_bindings_2(assoc_list(prog_var, mer_type)::in,
    list(tvar)::in, int::in, assoc_list(prog_var, mer_type)::out,
    assoc_list(prog_var, mer_type)::in, assoc_list(prog_var, mer_type)::out,
    set(tvar)::in, set(tvar)::out) is det.

check_var_type_bindings_2([], _, _, [], !UnresolvedVarsTypes, !Set).
check_var_type_bindings_2([Var - Type | VarTypes], HeadTypeParams,
        VarsToDo, LeftOverVarTypes, !UnresolvedVarsTypes, !Set) :-
    ( VarsToDo < 0 ->
        LeftOverVarTypes = [Var - Type | VarTypes]
    ;
        type_vars(Type, TVars),
        set.list_to_set(TVars, TVarsSet0),
        set.delete_list(HeadTypeParams, TVarsSet0, TVarsSet1),
        ( set.empty(TVarsSet1) ->
            true
        ;
            !:UnresolvedVarsTypes = [Var - Type | !.UnresolvedVarsTypes],
            set.union(!.Set, TVarsSet1, !:Set)
        ),
        check_var_type_bindings_2(VarTypes, HeadTypeParams,
            VarsToDo - 1, LeftOverVarTypes, !UnresolvedVarsTypes, !Set)
    ).

    % Bind all the type variables in `UnboundTypeVarsSet' to the type `void'.
    %
:- pred bind_type_vars_to_void(set(tvar)::in, vartypes::in, vartypes::out,
    constraint_proof_map::in, constraint_proof_map::out,
    constraint_map::in, constraint_map::out) is det.

bind_type_vars_to_void(UnboundTypeVarsSet, !VarTypes, !Proofs,
        !ConstraintMap) :-
    % Create a substitution that maps all of the unbound type variables
    % to `void'.
    MapToVoid = (pred(TVar::in, Subst0::in, Subst::out) is det :-
            map.det_insert(TVar, void_type, Subst0, Subst)
        ),
    set.fold(MapToVoid, UnboundTypeVarsSet, map.init, VoidSubst),

    % Then apply the substitution we just created to the various maps.
    apply_subst_to_vartypes(VoidSubst, !VarTypes),
    apply_subst_to_constraint_proofs(VoidSubst, !Proofs),
    apply_subst_to_constraint_map(VoidSubst, !ConstraintMap).

%-----------------------------------------------------------------------------%

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

    Pieces = [words("In")] ++ PredIdPieces ++ [suffix(":"), nl,
        fixed("type error: unsatisfied typeclass " ++
        choose_number(Constraints, "constraint:", "constraints:")),
        nl_indent_delta(1)] ++
        component_list_to_line_pieces(
            list.map(constraint_to_error_piece(TVarSet), Constraints), []) ++
        [nl_indent_delta(-1)],
    Msg = simple_msg(Context, [always(Pieces)]),

    DueTo = choose_number(Constraints,
        "The constraint is due to:",
        "The constraints are due to:"),
    ContextMsgStart = error_msg(yes(Context), do_not_treat_as_first, 0,
        [always([words(DueTo)])]),
    ConstrainedGoals = find_constrained_goals(PredInfo, Constraints),
    ContextMsgs = constrained_goals_to_error_msgs(ModuleInfo,
        ConstrainedGoals),

    Spec = error_spec(severity_error, phase_type_check,
        [Msg, ContextMsgStart | ContextMsgs]),
    !:Specs = [Spec | !.Specs].

:- func constraint_to_error_piece(tvarset, prog_constraint)
    = list(format_component).

constraint_to_error_piece(TVarset, Constraint) =
    [quote(mercury_constraint_to_string(TVarset, Constraint))].

    % A prog_constraint cannot contain context information (see the comment on
    % the type definition). However, a constraint_id happens to contain a
    % goal_id so we can look up a constraint_id for a prog_constraint, then
    % use the goal_id to reach the goal.
    %
:- func find_constrained_goals(pred_info, list(prog_constraint))
    = list(hlds_goal).

find_constrained_goals(PredInfo, Constraints) = Goals :-
    pred_info_get_clauses_info(PredInfo, ClausesInfo),
    clauses_info_get_clauses_rep(ClausesInfo, ClausesRep, _ItemNumbers),
    get_clause_list_any_order(ClausesRep, Clauses),

    pred_info_get_constraint_map(PredInfo, ConstraintMap),
    ReverseConstraintMap = map.reverse_map(ConstraintMap),
    map.apply_to_list(Constraints, ReverseConstraintMap, ConstraintIdSets),
    ConstraintIds = set.union_list(ConstraintIdSets),

    % This could be more efficient.
    FindGoals = (pred(Goal::out) is nondet :-
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
    Msg = error_msg(yes(Context), do_not_treat_as_first, 1,
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
            GenericCall = class_method(_, _, _, SimpleCallId),
            CallPieces = [simple_call(SimpleCallId)]
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

%-----------------------------------------------------------------------------%

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
        words("will be implicitly bound to the builtin type `void'."), nl],
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
        [option_is_set(warn_unresolved_polymorphism, yes,
            [always(MainPieces), verbose_only(VerbosePieces)])]),
    Severity = severity_conditional(warn_unresolved_polymorphism, yes,
        severity_warning, no),
    Spec = error_spec(Severity, phase_type_check, [Msg]),
    !:Specs = [Spec | !.Specs].

:- func var_and_type_to_pieces(prog_varset, tvarset,
    pair(prog_var, mer_type)) = list(format_component).

var_and_type_to_pieces(VarSet, TVarSet, Var - Type) =
    [words(mercury_var_to_string(VarSet, no, Var)), suffix(":"),
    words(mercury_type_to_string(TVarSet, no, Type)), nl].

%-----------------------------------------------------------------------------%

finally_resolve_pred_overloading(Args0, CallerPredInfo, ModuleInfo, Context,
        !PredName, !PredId) :-
    % In the case of a call to an overloaded predicate, typecheck.m
    % does not figure out the correct pred_id. We must do that here.

    ( !.PredId = invalid_pred_id ->
        % Find the set of candidate pred_ids for predicates which
        % have the specified name and arity.
        pred_info_get_typevarset(CallerPredInfo, TVarSet),
        pred_info_get_exist_quant_tvars(CallerPredInfo, ExistQVars),
        pred_info_get_head_type_params(CallerPredInfo, HeadTypeParams),
        pred_info_get_markers(CallerPredInfo, Markers),
        pred_info_get_clauses_info(CallerPredInfo, ClausesInfo),
        clauses_info_get_vartypes(ClausesInfo, VarTypes),
        lookup_var_types(VarTypes, Args0, ArgTypes),
        resolve_pred_overloading(ModuleInfo, Markers, TVarSet, ExistQVars,
            ArgTypes, HeadTypeParams, Context, !PredName, !:PredId)
    ;
        !:PredName = get_qualified_pred_name(ModuleInfo, !.PredId)
    ).

:- func get_qualified_pred_name(module_info, pred_id) = sym_name.

get_qualified_pred_name(ModuleInfo, PredId) = SymName :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    PredModule = pred_info_module(PredInfo),
    PredName = pred_info_name(PredInfo),
    SymName = qualified(PredModule, PredName).

%-----------------------------------------------------------------------------%

post_typecheck_finish_pred_no_io(ModuleInfo, ErrorProcs, !PredInfo) :-
    propagate_types_into_modes(ModuleInfo, ErrorProcs, !PredInfo).

post_typecheck_finish_imported_pred_no_io(ModuleInfo, ErrorProcIds,
        !PredInfo) :-
    % Make sure the vartypes field in the clauses_info is valid for imported
    % predicates. Unification procedures have clauses generated, so they
    % already have valid vartypes.
    ( pred_info_is_pseudo_imported(!.PredInfo) ->
        true
    ;
        pred_info_get_clauses_info(!.PredInfo, ClausesInfo0),
        clauses_info_get_headvar_list(ClausesInfo0, HeadVars),
        pred_info_get_arg_types(!.PredInfo, ArgTypes),
        vartypes_from_corresponding_lists(HeadVars, ArgTypes, VarTypes),
        clauses_info_set_vartypes(VarTypes, ClausesInfo0, ClausesInfo),
        pred_info_set_clauses_info(ClausesInfo, !PredInfo)
    ),
    propagate_types_into_modes(ModuleInfo, ErrorProcIds, !PredInfo).

post_typecheck_finish_promise(PromiseType, PromiseId, !ModuleInfo, !Specs) :-
    % Now that the promise has finished being typechecked, and has had all
    % of its pred_ids identified, remove the promise from the list of pred ids
    % to be processed in the future and place the pred_id associated with the
    % promise into the assertion or promise_ex table. For each assertion
    % that is in the interface, you need to check that it doesn't refer
    % to any symbols which are local to that module. Also record for each
    % predicate that is used in an assertion which assertion it is used in,
    % or for a promise ex declaration record in the promise ex table
    % the predicates used by the declaration.

    % Store the declaration in the appropriate table and get the goal
    % for the promise.
    store_promise(PromiseType, PromiseId, !ModuleInfo, Goal),

    % Remove from further processing.
    module_info_remove_predid(PromiseId, !ModuleInfo),

    % If the promise is in the interface, then ensure that it doesn't refer
    % to any local symbols.
    module_info_pred_info(!.ModuleInfo, PromiseId, PredInfo),
    ( pred_info_is_exported(PredInfo) ->
        in_interface_check(!.ModuleInfo, PredInfo, Goal, !Specs)
    ;
        true
    ).

    % Store promise declaration, normalise goal and return new module_info
    % and the goal for further processing.
    %
:- pred store_promise(promise_type::in, pred_id::in,
    module_info::in, module_info::out, hlds_goal::out) is det.

store_promise(PromiseType, PromiseId, !ModuleInfo, Goal) :-
    (
        % Case for assertions.
        PromiseType = promise_type_true,
        module_info_get_assertion_table(!.ModuleInfo, AssertTable0),
        assertion_table_add_assertion(PromiseId, AssertionId,
            AssertTable0, AssertTable),
        module_info_set_assertion_table(AssertTable, !ModuleInfo),
        assertion.assert_id_goal(!.ModuleInfo, AssertionId, Goal),
        assertion.record_preds_used_in(Goal, AssertionId, !ModuleInfo)
    ;
        % Case for exclusivity.
        ( PromiseType = promise_type_exclusive
        ; PromiseType = promise_type_exclusive_exhaustive
        ),
        promise_ex_goal(!.ModuleInfo, PromiseId, Goal),
        predids_from_goal(Goal, PredIds),
        module_info_get_exclusive_table(!.ModuleInfo, Table0),
        list.foldl(exclusive_table_add(PromiseId), PredIds, Table0, Table),
        module_info_set_exclusive_table(Table, !ModuleInfo)
    ;
        % Case for exhaustiveness -- XXX not yet implemented.
        PromiseType = promise_type_exhaustive,
        promise_ex_goal(!.ModuleInfo, PromiseId, Goal)
    ).

    % Get the goal from a promise_ex declaration.
    %
:- pred promise_ex_goal(module_info::in, pred_id::in, hlds_goal::out) is det.

promise_ex_goal(ModuleInfo, ExclusiveDeclPredId, Goal) :-
    module_info_pred_info(ModuleInfo, ExclusiveDeclPredId, PredInfo),
    pred_info_get_clauses_info(PredInfo, ClausesInfo),
    clauses_info_get_clauses_rep(ClausesInfo, ClausesRep, _ItemNumbers),
    get_clause_list(ClausesRep, Clauses),
    ( Clauses = [Clause] ->
        Goal0 = Clause ^ clause_body,
        assertion.normalise_goal(Goal0, Goal)
    ;
        unexpected($module, $pred, "not a single clause")
    ).

%-----------------------------------------------------------------------------%

    % Ensure that an assertion which is defined in an interface doesn't
    % refer to any constructors, functions and predicates defined in the
    % implementation of that module.
    %
:- pred in_interface_check(module_info::in, pred_info::in, hlds_goal::in,
    list(error_spec)::in, list(error_spec)::out) is det.

in_interface_check(ModuleInfo, PredInfo, Goal, !Specs) :-
    Goal = hlds_goal(GoalExpr, GoalInfo),
    (
        GoalExpr = plain_call(PredId, _, _, _, _,SymName),
        module_info_pred_info(ModuleInfo, PredId, CallPredInfo),
        pred_info_get_import_status(CallPredInfo, ImportStatus),
        ( status_defined_in_impl_section(ImportStatus) = yes ->
            Context = goal_info_get_context(GoalInfo),
            PredOrFunc = pred_info_is_pred_or_func(CallPredInfo),
            Arity = pred_info_orig_arity(CallPredInfo),
            IdPieces =
                [simple_call(simple_call_id(PredOrFunc, SymName, Arity))],
            report_assertion_interface_error(ModuleInfo, Context, IdPieces,
                !Specs)
        ;
            true
        )
    ;
        GoalExpr = generic_call(_, _, _, _, _)
    ;
        GoalExpr = unify(Var, RHS, _, _, _),
        Context = goal_info_get_context(GoalInfo),
        in_interface_check_unify_rhs(ModuleInfo, PredInfo, RHS, Var, Context,
            !Specs)
    ;
        GoalExpr = call_foreign_proc(_, PredId, _, _, _, _, _),
        module_info_pred_info(ModuleInfo, PredId, PragmaPredInfo),
        pred_info_get_import_status(PragmaPredInfo, ImportStatus),
        ( status_defined_in_impl_section(ImportStatus) = yes ->
            Context = goal_info_get_context(GoalInfo),
            PredOrFunc = pred_info_is_pred_or_func(PragmaPredInfo),
            Name = pred_info_name(PragmaPredInfo),
            SymName = unqualified(Name),
            Arity = pred_info_orig_arity(PragmaPredInfo),
            IdPieces =
                [simple_call(simple_call_id(PredOrFunc, SymName, Arity))],
            report_assertion_interface_error(ModuleInfo, Context, IdPieces,
                !Specs)
        ;
            true
        )
    ;
        GoalExpr = conj(_, Goals),
        in_interface_check_list(ModuleInfo, PredInfo, Goals, !Specs)
    ;
        GoalExpr = switch(_, _, _),
        unexpected($module, $pred, "assertion contains switch")
    ;
        GoalExpr = disj(Goals),
        in_interface_check_list(ModuleInfo, PredInfo, Goals, !Specs)
    ;
        GoalExpr = negation(SubGoal),
        in_interface_check(ModuleInfo, PredInfo, SubGoal, !Specs)
    ;
        GoalExpr = scope(_, SubGoal),
        in_interface_check(ModuleInfo, PredInfo, SubGoal, !Specs)
    ;
        GoalExpr = if_then_else(_, Cond, Then, Else),
        in_interface_check(ModuleInfo, PredInfo, Cond, !Specs),
        in_interface_check(ModuleInfo, PredInfo, Then, !Specs),
        in_interface_check(ModuleInfo, PredInfo, Else, !Specs)
    ;
        GoalExpr = shorthand(ShortHand),
        (
            ShortHand = atomic_goal(_, _, _, _, MainGoal, OrElseGoals, _),
            in_interface_check(ModuleInfo, PredInfo, MainGoal, !Specs),
            in_interface_check_list(ModuleInfo, PredInfo, OrElseGoals, !Specs)
        ;
            ShortHand = try_goal(_, _, SubGoal),
            in_interface_check(ModuleInfo, PredInfo, SubGoal, !Specs)
        ;
            ShortHand = bi_implication(LHS, RHS),
            in_interface_check(ModuleInfo, PredInfo, LHS, !Specs),
            in_interface_check(ModuleInfo, PredInfo, RHS, !Specs)
        )
    ).

%-----------------------------------------------------------------------------%

:- pred in_interface_check_unify_rhs(module_info::in, pred_info::in,
    unify_rhs::in, prog_var::in, prog_context::in,
    list(error_spec)::in, list(error_spec)::out) is det.

in_interface_check_unify_rhs(ModuleInfo, PredInfo, RHS, Var, Context,
        !Specs) :-
    (
        RHS = rhs_var(_)
    ;
        RHS = rhs_functor(ConsId, _, _),
        pred_info_get_clauses_info(PredInfo, ClausesInfo),
        clauses_info_get_vartypes(ClausesInfo, VarTypes),
        lookup_var_type(VarTypes, Var, Type),
        type_to_ctor_det(Type, TypeCtor),
        module_info_get_type_table(ModuleInfo, TypeTable),
        lookup_type_ctor_defn(TypeTable, TypeCtor, TypeDefn),
        get_type_defn_status(TypeDefn, TypeStatus),
        DefinedInImpl = status_defined_in_impl_section(TypeStatus),
        (
            DefinedInImpl = yes,
            ConsIdStr = cons_id_and_arity_to_string(ConsId),
            IdPieces = [words("constructor"), quote(ConsIdStr)],
            report_assertion_interface_error(ModuleInfo, Context, IdPieces,
                !Specs)
        ;
            DefinedInImpl = no
        )
    ;
        RHS = rhs_lambda_goal(_, _, _, _, _, _, _, _, Goal),
        in_interface_check(ModuleInfo, PredInfo, Goal, !Specs)
    ).

%-----------------------------------------------------------------------------%

:- pred in_interface_check_list(module_info::in, pred_info::in, hlds_goals::in,
    list(error_spec)::in, list(error_spec)::out) is det.

in_interface_check_list(_ModuleInfo, _PredInfo, [], !Specs).
in_interface_check_list(ModuleInfo, PredInfo, [Goal0 | Goal0s], !Specs) :-
    in_interface_check(ModuleInfo, PredInfo, Goal0, !Specs),
    in_interface_check_list(ModuleInfo, PredInfo, Goal0s, !Specs).

%-----------------------------------------------------------------------------%

:- pred report_assertion_interface_error(module_info::in, prog_context::in,
    list(format_component)::in, list(error_spec)::in, list(error_spec)::out)
    is det.

report_assertion_interface_error(ModuleInfo, Context, IdPieces, !Specs) :-
    module_info_get_name(ModuleInfo, ModuleName),
    MainPieces =
        [words("In interface for module"), sym_name(ModuleName), suffix(":"),
        nl, words("error: exported promise refers to")] ++ IdPieces ++
        [words("which is defined in the implementation section of module"),
        sym_name(ModuleName), suffix("."), nl],
    VerbosePieces =
        [words("Either move the promise into the implementation section"),
        words("or move the definition into the interface."), nl],
    Msgs = [always(MainPieces), verbose_only(VerbosePieces)],
    Spec = error_spec(severity_error, phase_type_check,
        [simple_msg(Context, Msgs)]),
    !:Specs = [Spec | !.Specs].

%-----------------------------------------------------------------------------%

:- pred check_type_of_main(pred_info::in,
    list(error_spec)::in, list(error_spec)::out) is det.

check_type_of_main(PredInfo, !Specs) :-
    (
        % Check if this predicate is the program entry point main/2.
        pred_info_name(PredInfo) = "main",
        pred_info_orig_arity(PredInfo) = 2,
        pred_info_is_exported(PredInfo)
    ->
        % Check that the arguments of main/2 have type `io.state'.
        pred_info_get_arg_types(PredInfo, ArgTypes),
        (
            ArgTypes = [Arg1, Arg2],
            type_is_io_state(Arg1),
            type_is_io_state(Arg2)
        ->
            true
        ;
            pred_info_get_context(PredInfo, Context),
            Pieces = [words("Error: arguments of main/2"),
                words("must have type `io.state'."), nl],
            Msg = simple_msg(Context, [always(Pieces)]),
            Spec = error_spec(severity_error, phase_type_check, [Msg]),
            !:Specs = [Spec | !.Specs]
        )
    ;
        true
    ).

%-----------------------------------------------------------------------------%

    % Ensure that all constructors occurring in predicate mode declarations
    % are module qualified.
    %
:- pred propagate_types_into_modes(module_info::in,
    list(proc_id)::out, pred_info::in, pred_info::out) is det.

propagate_types_into_modes(ModuleInfo, ErrorProcIds, !PredInfo) :-
    pred_info_get_arg_types(!.PredInfo, ArgTypes),
    pred_info_get_procedures(!.PredInfo, Procs0),
    ProcIds = pred_info_procids(!.PredInfo),
    propagate_types_into_proc_modes(ModuleInfo, ProcIds, ArgTypes,
        [], RevErrorProcIds, Procs0, Procs),
    ErrorProcIds = list.reverse(RevErrorProcIds),
    pred_info_set_procedures(Procs, !PredInfo).

%-----------------------------------------------------------------------------%

:- pred propagate_types_into_proc_modes(module_info::in, list(proc_id)::in,
    list(mer_type)::in, list(proc_id)::in, list(proc_id)::out,
    proc_table::in, proc_table::out) is det.

propagate_types_into_proc_modes(_, [], _, !RevErrorProcIds, !Procs).
propagate_types_into_proc_modes(ModuleInfo, [ProcId | ProcIds], ArgTypes,
        !RevErrorProcIds, !Procs) :-
    map.lookup(!.Procs, ProcId, ProcInfo0),
    proc_info_get_argmodes(ProcInfo0, ArgModes0),
    propagate_types_into_mode_list(ModuleInfo, ArgTypes, ArgModes0, ArgModes),

    % Check for unbound inst vars. (This needs to be done after
    % propagate_types_into_mode_list, because we need the insts
    % to be module-qualified; and it needs to be done before mode analysis,
    % to avoid internal errors.)
    ( mode_list_contains_inst_var(ArgModes, ModuleInfo, _InstVar) ->
        !:RevErrorProcIds = [ProcId | !.RevErrorProcIds]
    ;
        proc_info_set_argmodes(ArgModes, ProcInfo0, ProcInfo),
        map.det_update(ProcId, ProcInfo, !Procs)
    ),
    propagate_types_into_proc_modes(ModuleInfo, ProcIds, ArgTypes,
        !RevErrorProcIds, !Procs).

:- pred report_unbound_inst_vars(module_info::in, pred_id::in,
    list(proc_id)::in, pred_info::in, pred_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

report_unbound_inst_vars(ModuleInfo, PredId, ErrorProcIds, !PredInfo,
        !Specs) :-
    (
        ErrorProcIds = []
    ;
        ErrorProcIds = [_ | _],
        pred_info_get_procedures(!.PredInfo, ProcTable0),
        list.foldl2(report_unbound_inst_var_error(ModuleInfo, PredId),
            ErrorProcIds, ProcTable0, ProcTable, !Specs),
        pred_info_set_procedures(ProcTable, !PredInfo)
    ).

:- pred report_unbound_inst_var_error(module_info::in,
    pred_id::in, proc_id::in, proc_table::in, proc_table::out,
    list(error_spec)::in, list(error_spec)::out) is det.

report_unbound_inst_var_error(ModuleInfo, PredId, ProcId, Procs0, Procs,
        !Specs) :-
    map.lookup(Procs0, ProcId, ProcInfo),
    proc_info_get_context(ProcInfo, Context),
    Pieces = [words("In mode declaration for")] ++
        describe_one_pred_name(ModuleInfo, should_not_module_qualify, PredId)
        ++ [suffix(":"), nl,
        words("error: unbound inst variable(s)."), nl,
        words("(Sorry, polymorphic modes are not supported.)"), nl],
    Msg = simple_msg(Context, [always(Pieces)]),
    Spec = error_spec(severity_error, phase_type_check, [Msg]),
    !:Specs = [Spec | !.Specs],
    % Delete this mode, to avoid internal errors.
    map.det_remove(ProcId, _, Procs0, Procs).

%-----------------------------------------------------------------------------%

:- pred check_for_indistinguishable_modes(module_info::in, pred_id::in,
    pred_info::in, pred_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

check_for_indistinguishable_modes(ModuleInfo, PredId, !PredInfo, !Specs) :-
    (
        % Don't check for indistinguishable modes in unification predicates.
        % The default (in, in) mode must be semidet, but for single-value types
        % we also want to create a det mode which will be indistinguishable
        % from the semidet mode. (When the type is known, the det mode is
        % called, but the polymorphic unify needs to be able to call
        % the semidet mode.)
        pred_info_get_origin(!.PredInfo, Origin),
        Origin = origin_special_pred(spec_pred_unify - _)
    ->
        true
    ;
        ProcIds = pred_info_procids(!.PredInfo),
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
    ( modes_are_indistinguishable(ProcId, ProcId1, !.PredInfo, ModuleInfo) ->
        pred_info_get_import_status(!.PredInfo, Status),
        module_info_get_globals(ModuleInfo, Globals),
        globals.lookup_bool_option(Globals, intermodule_optimization,
            IntermodOpt),
        globals.lookup_bool_option(Globals, intermodule_analysis,
            IntermodAnalysis),
        globals.lookup_bool_option(Globals, make_optimization_interface,
            MakeOptInt),
        (
            % With intermodule optimization we can read the declarations
            % for a predicate from the `.int' and `.int0' files, so ignore
            % the error in those cases.
            (
                status_defined_in_this_module(Status) = yes
            ;
                IntermodOpt = no,
                IntermodAnalysis = no
            ;
                MakeOptInt = yes
            )
        ->
            % XXX We shouldn't ignore the updated ModuleInfo, which may
            % differ from the old one in including an updated error count.
            Spec = report_indistinguishable_modes_error(ModuleInfo,
                ProcId1, ProcId, PredId, !.PredInfo),
            !:Specs = [Spec | !.Specs]
        ;
            true
        ),
        pred_info_remove_procid(ProcId1, !PredInfo),
        Removed = yes
    ;
        check_for_indistinguishable_mode(ModuleInfo, PredId, ProcId1,
            ProcIds, Removed, !PredInfo, !Specs)
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

resolve_unify_functor(X0, ConsId0, ArgVars0, Mode0, Unification0, UnifyContext,
        GoalInfo0, ModuleInfo, !PredInfo, !VarSet, !VarTypes,
        Goal, IsPlainUnify) :-
    lookup_var_type(!.VarTypes, X0, TypeOfX),
    list.length(ArgVars0, Arity),
    (
        % Is the function symbol apply/N or ''/N, representing a higher-order
        % function call? Or the impure/semipure equivalents impure_apply/N
        % and semipure_apply/N?
        % (XXX FIXME We should use nicer syntax for impure apply/N.)
        ConsId0 = cons(unqualified(ApplyName), _, _),
        ( ApplyName = "apply", Purity = purity_pure
        ; ApplyName = "", Purity = purity_pure
        ; ApplyName = "impure_apply", Purity = purity_impure
        ; ApplyName = "semipure_apply", Purity = purity_semipure
        ),
        Arity >= 1,
        ArgVars0 = [FuncVar | FuncArgVars]
    ->
        % Convert the higher-order function call (apply/N) into a higher-order
        % predicate call (i.e., replace `X = apply(F, A, B, C)'
        % with `call(F, A, B, C, X)')
        ArgVars = FuncArgVars ++ [X0],
        Modes = [],
        Det = detism_erroneous,
        adjust_func_arity(pf_function, Arity, FullArity),
        HOCall = generic_call(
            higher_order(FuncVar, Purity, pf_function, FullArity),
            ArgVars, Modes, arg_reg_types_unset, Det),
        Goal = hlds_goal(HOCall, GoalInfo0),
        IsPlainUnify = is_not_plain_unify
    ;
        % Is the function symbol a user-defined function, rather than
        % a functor which represents a data constructor?

        % Find the set of candidate predicates which have the
        % specified name and arity (and module, if module-qualified)
        ConsId0 = cons(PredName, _, _),

        pred_info_get_markers(!.PredInfo, Markers),
        module_info_get_predicate_table(ModuleInfo, PredTable),
        % This search will usually fail, so do it first.
        predicate_table_lookup_func_sym_arity(PredTable,
            calls_are_fully_qualified(Markers), PredName, Arity, PredIds),
        PredIds = [_ | _],

        % We don't do this for compiler-generated predicates; they are assumed
        % to have been generated with all functions already expanded. If we did
        % this check for compiler-generated predicates, it would cause the
        % wrong behaviour in the case where there is a user-defined function
        % whose type is exactly the same as the type of a constructor.
        % (Normally that would cause a type ambiguity error, but
        % compiler-generated predicates are not type-checked.)
        \+ is_unify_or_compare_pred(!.PredInfo),

        % We don't do this for the clause introduced by the compiler for a
        % field access function -- that needs to be expanded into
        % unifications below.
        \+ pred_info_is_field_access_function(ModuleInfo, !.PredInfo),

        % Check if any of the candidate functions have argument/return types
        % which subsume the actual argument/return types of this function call,
        % and which have universal constraints consistent with what we expect.
        pred_info_get_typevarset(!.PredInfo, TVarSet),
        pred_info_get_exist_quant_tvars(!.PredInfo, ExistQTVars),
        pred_info_get_head_type_params(!.PredInfo, HeadTypeParams),
        lookup_var_types(!.VarTypes, ArgVars0, ArgTypes0),
        ArgTypes = ArgTypes0 ++ [TypeOfX],
        pred_info_get_constraint_map(!.PredInfo, ConstraintMap),
        GoalId = goal_info_get_goal_id(GoalInfo0),
        ConstraintSearch =
            search_hlds_constraint_list(ConstraintMap, unproven, GoalId),
        Context = goal_info_get_context(GoalInfo0),
        find_matching_pred_id(ModuleInfo, PredIds, TVarSet, ExistQTVars,
            ArgTypes, HeadTypeParams, yes(ConstraintSearch), Context,
            PredId, QualifiedFuncName)
    ->
        % Convert function calls in unifications into plain calls:
        % replace `X = f(A, B, C)' with `f(A, B, C, X)'.

        ProcId = invalid_proc_id,
        ArgVars = ArgVars0 ++ [X0],
        FuncCallUnifyContext = call_unify_context(X0,
            rhs_functor(ConsId0, no, ArgVars0), UnifyContext),
        FuncCall = plain_call(PredId, ProcId, ArgVars, not_builtin,
            yes(FuncCallUnifyContext), QualifiedFuncName),
        Goal = hlds_goal(FuncCall, GoalInfo0),
        IsPlainUnify = is_not_plain_unify
    ;
        % Is the function symbol a higher-order predicate or function constant?
        ConsId0 = cons(Name, _, _),
        type_is_higher_order_details(TypeOfX, _Purity, PredOrFunc,
            EvalMethod, HOArgTypes),

        % We don't do this for the clause introduced by the compiler
        % for a field access function -- that needs to be expanded
        % into unifications below.
        \+ pred_info_is_field_access_function(ModuleInfo, !.PredInfo),

        % Find the pred_id of the constant.
        lookup_var_types(!.VarTypes, ArgVars0, ArgTypes0),
        AllArgTypes = ArgTypes0 ++ HOArgTypes,
        pred_info_get_typevarset(!.PredInfo, TVarSet),
        pred_info_get_exist_quant_tvars(!.PredInfo, ExistQVars),
        pred_info_get_head_type_params(!.PredInfo, HeadTypeParams),
        pred_info_get_markers(!.PredInfo, Markers),
        Context = goal_info_get_context(GoalInfo0),
        get_pred_id_by_types(calls_are_fully_qualified(Markers), Name,
            PredOrFunc, TVarSet, ExistQVars, AllArgTypes, HeadTypeParams,
            ModuleInfo, Context, PredId)
    ->
        module_info_pred_info(ModuleInfo, PredId, PredInfo),
        ProcIds = pred_info_procids(PredInfo),
        (
            ProcIds = [ProcId0],
            MaybeProcId = yes(ProcId0)
        ;
            ProcIds = [_, _ | _],
            % We don't know which mode to pick. Defer it until mode checking.
            MaybeProcId = yes(invalid_proc_id)
        ;
            ProcIds = [],
            MaybeProcId = no
        ),
        (
            MaybeProcId = yes(ProcId),
            ShroudedPredProcId = shroud_pred_proc_id(proc(PredId, ProcId)),
            ConsId = closure_cons(ShroudedPredProcId, EvalMethod),
            GoalExpr = unify(X0, rhs_functor(ConsId, no, ArgVars0), Mode0,
                Unification0, UnifyContext),
            Goal = hlds_goal(GoalExpr, GoalInfo0),
            IsPlainUnify = is_not_plain_unify
        ;
            MaybeProcId = no,
            Goal = true_goal,
            Pieces = [words("Error: reference to"),
                words("undeclared function or predicate"),
                sym_name_and_arity(Name / Arity), suffix("."), nl],
            Msg = simple_msg(Context, [always(Pieces)]),
            Spec = error_spec(severity_error, phase_purity_check, [Msg]),
            IsPlainUnify = is_unknown_ref(Spec)
        )
    ;
        % Is it a call to an automatically generated field access function.
        % This test must come after the tests for function calls and
        % higher-order terms above. It's done that way because it's easier
        % to check that the types match for functions calls and higher-order
        % terms.
        ConsId0 = cons(Name, Arity, _),
        is_field_access_function_name(ModuleInfo, Name, Arity,
            AccessType, FieldName),

        % We don't do this for compiler-generated predicates --
        % they will never contain calls to field access functions.
        \+ is_unify_or_compare_pred(!.PredInfo),

        % If there is a constructor for which the argument types match,
        % this unification couldn't be a call to a field access function,
        % otherwise there would have been an error reported for unresolved
        % overloading.
        pred_info_get_typevarset(!.PredInfo, TVarSet),
        lookup_var_types(!.VarTypes, ArgVars0, ArgTypes0),
        \+ find_matching_constructor(ModuleInfo, TVarSet, ConsId0,
            TypeOfX, ArgTypes0)
    ->
        finish_field_access_function(ModuleInfo, !PredInfo, !VarTypes, !VarSet,
            AccessType, FieldName, UnifyContext, X0, ArgVars0, GoalInfo0,
            Goal),
        IsPlainUnify = is_not_plain_unify
    ;
        % Module qualify ordinary construction/deconstruction unifications.
        type_to_ctor_det(TypeOfX, TypeCtorOfX),
        ( ConsId0 = cons(SymName0, Arity, _OldTypeCtor) ->
            ( TypeOfX = tuple_type(_, _) ->
                ConsId = tuple_cons(Arity)
            ; TypeOfX = builtin_type(builtin_type_char) ->
                (
                    SymName0 = unqualified(Name0),
                    ( encode_escaped_char(Char, Name0) ->
                        ConsId = char_const(Char)
                    ;
                        unexpected($module, $pred, "encode_escaped_char")
                    )
                ;
                    SymName0 = qualified(_, _),
                    unexpected($module, $pred, "qualified char const")
                )
            ;
                Name = unqualify_name(SymName0),
                TypeCtorOfX = type_ctor(TypeCtorSymName, _),
                (
                    TypeCtorSymName = qualified(TypeCtorModule, _),
                    SymName = qualified(TypeCtorModule, Name),
                    ConsId = cons(SymName, Arity, TypeCtorOfX)
                ;
                    TypeCtorSymName = unqualified(_),
                    unexpected($module, $pred, "unqualified type_ctor")
                )
            )
        ;
            ConsId = ConsId0
        ),
        GoalExpr = unify(X0, rhs_functor(ConsId, no, ArgVars0), Mode0,
            Unification0, UnifyContext),
        Goal = hlds_goal(GoalExpr, GoalInfo0),
        IsPlainUnify = is_plain_unify
    ).

%-----------------------------------------------------------------------------%

    % Succeed if there is a constructor which matches the given cons_id,
    % type and argument types.
    %
:- pred find_matching_constructor(module_info::in, tvarset::in,
    cons_id::in, mer_type::in, list(mer_type)::in) is semidet.

find_matching_constructor(ModuleInfo, TVarSet, ConsId, Type, ArgTypes) :-
    type_to_ctor(Type, TypeCtor),
    module_info_get_cons_table(ModuleInfo, ConsTable),
    search_cons_table_of_type_ctor(ConsTable, TypeCtor, ConsId, ConsDefn),

    % Overloading resolution ignores the class constraints.
    ConsDefn = hlds_cons_defn(_, _, _, _, ConsExistQVars, _, ConsArgs, _),

    module_info_get_type_table(ModuleInfo, TypeTable),
    search_type_ctor_defn(TypeTable, TypeCtor, TypeDefn),
    hlds_data.get_type_defn_tvarset(TypeDefn, TypeTVarSet),
    hlds_data.get_type_defn_kind_map(TypeDefn, TypeKindMap),

    ConsArgTypes = list.map(func(C) = C ^ arg_type, ConsArgs),
    % XXX is this correct?
    ExistQVars = [],
    HeadTypeParams = [],
    arg_type_list_subsumes(TVarSet, ExistQVars, ArgTypes, HeadTypeParams,
        TypeTVarSet, TypeKindMap, ConsExistQVars, ConsArgTypes).

%-----------------------------------------------------------------------------%

    % Convert a field access function call into the equivalent unifications
    % so that later passes do not have to handle them as a special case.
    % The error messages from mode analysis and determinism analysis
    % shouldn't be too much worse than if the goals were special cases.
    %
:- pred finish_field_access_function(module_info::in,
    pred_info::in, pred_info::out, vartypes::in, vartypes::out,
    prog_varset::in, prog_varset::out,
    field_access_type::in, ctor_field_name::in,
    unify_context::in, prog_var::in, list(prog_var)::in,
    hlds_goal_info::in, hlds_goal::out) is det.

finish_field_access_function(ModuleInfo, !PredInfo, !VarTypes, !VarSet,
        AccessType, FieldName, UnifyContext, Var, Args, GoalInfo,
        hlds_goal(GoalExpr, GoalInfo)) :-
    (
        AccessType = get,
        field_extraction_function_args(Args, TermVar),
        translate_get_function(ModuleInfo, !PredInfo, !VarTypes, !VarSet,
            FieldName, UnifyContext, Var, TermVar, GoalInfo, GoalExpr)
    ;
        AccessType = set,
        field_update_function_args(Args, TermInputVar, FieldVar),
        translate_set_function(ModuleInfo, !PredInfo, !VarTypes, !VarSet,
            FieldName, UnifyContext, FieldVar, TermInputVar, Var,
            GoalInfo, GoalExpr)
    ).

:- pred translate_get_function(module_info::in,
    pred_info::in, pred_info::out, vartypes::in, vartypes::out,
    prog_varset::in, prog_varset::out, ctor_field_name::in,
    unify_context::in, prog_var::in, prog_var::in,
    hlds_goal_info::in, hlds_goal_expr::out) is det.

translate_get_function(ModuleInfo, !PredInfo, !VarTypes, !VarSet, FieldName,
        UnifyContext, FieldVar, TermInputVar, OldGoalInfo, GoalExpr) :-
    lookup_var_type(!.VarTypes, TermInputVar, TermType),
    get_constructor_containing_field(ModuleInfo, TermType, FieldName,
        ConsId, FieldNumber),

    GoalId = goal_info_get_goal_id(OldGoalInfo),
    get_cons_id_arg_types_adding_existq_tvars(ModuleInfo, GoalId, ConsId,
        TermType, ArgTypes0, ExistQVars, !PredInfo),

    % If the type of the field we are extracting contains existentially
    % quantified type variables then we need to rename any other
    % occurrences of those type variables in the arguments of the
    % constructor so that they match those in the type of the field.
    % (We don't need to do this for field updates because if any
    % existentially quantified type variables occur in field to set
    % and other fields then the field update should have been disallowed
    % by typecheck.m because the result can't be well-typed).
    (
        ExistQVars = [_ | _],
        lookup_var_type(!.VarTypes, FieldVar, FieldType),
        list.det_index1(ArgTypes0, FieldNumber, FieldArgType),
        ( type_list_subsumes([FieldArgType], [FieldType], FieldSubst) ->
            apply_rec_subst_to_type_list(FieldSubst, ArgTypes0, ArgTypes)
        ;
            unexpected($module, $pred, "type_list_subsumes failed")
        )
    ;
        ExistQVars = [],
        ArgTypes = ArgTypes0
    ),

    split_list_at_index(FieldNumber, ArgTypes, TypesBeforeField,
        _, TypesAfterField),

    make_new_vars(TypesBeforeField, VarsBeforeField, !VarTypes, !VarSet),
    make_new_vars(TypesAfterField, VarsAfterField, !VarTypes, !VarSet),

    ArgVars = VarsBeforeField ++ [FieldVar | VarsAfterField],

    RestrictNonLocals = goal_info_get_nonlocals(OldGoalInfo),
    create_pure_atomic_unification_with_nonlocals(TermInputVar,
        rhs_functor(ConsId, no, ArgVars), OldGoalInfo, RestrictNonLocals,
        [FieldVar, TermInputVar], UnifyContext, FunctorGoal),
    FunctorGoal = hlds_goal(GoalExpr, _).

:- pred translate_set_function(module_info::in, pred_info::in, pred_info::out,
    vartypes::in, vartypes::out, prog_varset::in, prog_varset::out,
    ctor_field_name::in, unify_context::in,
    prog_var::in, prog_var::in, prog_var::in,
    hlds_goal_info::in, hlds_goal_expr::out) is det.

translate_set_function(ModuleInfo, !PredInfo, !VarTypes, !VarSet, FieldName,
        UnifyContext, FieldVar, TermInputVar, TermOutputVar, OldGoalInfo,
        Goal) :-
    lookup_var_type(!.VarTypes, TermInputVar, TermType),
    get_constructor_containing_field(ModuleInfo, TermType, FieldName,
        ConsId0, FieldNumber),

    GoalId = goal_info_get_goal_id(OldGoalInfo),
    get_cons_id_arg_types_adding_existq_tvars(ModuleInfo, GoalId, ConsId0,
        TermType, ArgTypes, ExistQVars, !PredInfo),

    split_list_at_index(FieldNumber, ArgTypes,
        TypesBeforeField, TermFieldType, TypesAfterField),

    make_new_vars(TypesBeforeField, VarsBeforeField, !VarTypes, !VarSet),
    make_new_var(TermFieldType, SingletonFieldVar, !VarTypes, !VarSet),
    make_new_vars(TypesAfterField, VarsAfterField, !VarTypes, !VarSet),

    % Build a goal to deconstruct the input.
    DeconstructArgs = VarsBeforeField ++ [SingletonFieldVar | VarsAfterField],
    OldNonLocals = goal_info_get_nonlocals(OldGoalInfo),
    NonLocalArgs = VarsBeforeField ++ VarsAfterField,
    set_of_var.insert_list(NonLocalArgs, OldNonLocals,
        DeconstructRestrictNonLocals),

    create_pure_atomic_unification_with_nonlocals(TermInputVar,
        rhs_functor(ConsId0, no, DeconstructArgs), OldGoalInfo,
        DeconstructRestrictNonLocals, [TermInputVar | DeconstructArgs],
        UnifyContext, DeconstructGoal),

    % Build a goal to construct the output.
    ConstructArgs = VarsBeforeField ++ [FieldVar | VarsAfterField],
    set_of_var.insert_list(NonLocalArgs, OldNonLocals,
        ConstructRestrictNonLocals),

    % If the cons_id is existentially quantified, add a `new' prefix
    % so that polymorphism.m adds the appropriate type_infos.
    (
        ExistQVars = [],
        ConsId = ConsId0
    ;
        ExistQVars = [_ | _],
        ( ConsId0 = cons(ConsName0, ConsArity, TypeCtor) ->
            add_new_prefix(ConsName0, ConsName),
            ConsId = cons(ConsName, ConsArity, TypeCtor)
        ;
            unexpected($module, $pred, "invalid cons_id")
        )
    ),

    create_pure_atomic_unification_with_nonlocals(TermOutputVar,
        rhs_functor(ConsId, no, ConstructArgs), OldGoalInfo,
        ConstructRestrictNonLocals, [TermOutputVar | ConstructArgs],
        UnifyContext, ConstructGoal),

    ConjExpr = conj(plain_conj, [DeconstructGoal, ConstructGoal]),
    Conj = hlds_goal(ConjExpr, OldGoalInfo),

    % Make mode analysis treat the translated access function
    % as an atomic goal.
    Goal = scope(barrier(removable), Conj).

:- pred get_cons_id_arg_types_adding_existq_tvars(module_info::in,
    goal_id::in, cons_id::in, mer_type::in, list(mer_type)::out,
    list(tvar)::out, pred_info::in, pred_info::out) is det.

get_cons_id_arg_types_adding_existq_tvars(ModuleInfo, GoalId, ConsId,
        TermType, ActualArgTypes, ActualExistQVars, !PredInfo) :-
    % Split the list of argument types at the named field.
    type_to_ctor_det(TermType, TypeCtor),
    get_cons_defn_det(ModuleInfo, TypeCtor, ConsId, ConsDefn),
    ConsDefn = hlds_cons_defn(_, _, TypeParams, _, ConsExistQVars,
        ConsConstraints, ConsArgs, _),
    ConsArgTypes = list.map(func(C) = C ^ arg_type, ConsArgs),

    (
        ConsExistQVars = [],
        ActualArgTypes0 = ConsArgTypes,
        ActualExistQVars = []
    ;
        ConsExistQVars = [_ | _],
        % Rename apart the existentially quantified type variables.
        list.length(ConsExistQVars, NumExistQVars),
        pred_info_get_typevarset(!.PredInfo, TVarSet0),
        varset.new_vars(NumExistQVars, ParentExistQVars, TVarSet0, TVarSet),
        pred_info_set_typevarset(TVarSet, !PredInfo),
        map.from_corresponding_lists(ConsExistQVars, ParentExistQVars,
            ConsToParentRenaming),
        apply_variable_renaming_to_type_list(ConsToParentRenaming,
            ConsArgTypes, ParentArgTypes),
        apply_variable_renaming_to_prog_constraint_list(ConsToParentRenaming,
            ConsConstraints, ParentConstraints),

        % Constrained existentially quantified tvars will have already been
        % created during typechecking, so we need to ensure that the new ones
        % we allocate here are bound to those created earlier, so that
        % the varmaps remain meaningful.

        pred_info_get_constraint_map(!.PredInfo, ConstraintMap),
        list.length(ConsConstraints, NumConstraints),
        lookup_hlds_constraint_list(ConstraintMap, assumed, GoalId,
            NumConstraints, ActualConstraints),
        constraint_list_subsumes_det(ParentConstraints, ActualConstraints,
            ExistTSubst),
        apply_rec_subst_to_type_list(ExistTSubst, ParentArgTypes,
            ActualArgTypes0),

        % The kinds will be ignored when the types are converted back to tvars.
        map.init(KindMap),
        apply_rec_subst_to_tvar_list(KindMap, ExistTSubst, ParentExistQVars,
            ActualExistQVarTypes),
        ( type_list_to_var_list(ActualExistQVarTypes, ActualExistQVars0) ->
            ActualExistQVars = ActualExistQVars0
        ;
            unexpected($module, $pred, "existq_tvar bound to non-var")
        )
    ),
    type_to_ctor_and_args_det(TermType, _, TypeArgs),
    map.from_corresponding_lists(TypeParams, TypeArgs, UnivTSubst),
    apply_subst_to_type_list(UnivTSubst, ActualArgTypes0, ActualArgTypes).

:- pred constraint_list_subsumes_det(list(prog_constraint)::in,
    list(prog_constraint)::in, tsubst::out) is det.

constraint_list_subsumes_det(ConstraintsA, ConstraintsB, Subst) :-
    constraint_list_get_tvars(ConstraintsB, TVarsB),
    map.init(Subst0),
    (
        unify_constraint_list(ConstraintsA, ConstraintsB, TVarsB,
            Subst0, Subst1)
    ->
        Subst = Subst1
    ;
        unexpected($module, $pred, "failed")
    ).

:- pred unify_constraint_list(list(prog_constraint)::in,
    list(prog_constraint)::in, list(tvar)::in, tsubst::in, tsubst::out)
    is semidet.

unify_constraint_list([], [], _, !Subst).
unify_constraint_list([A | As], [B | Bs], TVars, !Subst) :-
    A = constraint(_, ArgsA),
    B = constraint(_, ArgsB),
    type_unify_list(ArgsA, ArgsB, TVars, !Subst),
    unify_constraint_list(As, Bs, TVars, !Subst).

:- pred split_list_at_index(int::in, list(T)::in, list(T)::out, T::out,
    list(T)::out) is det.

split_list_at_index(Index, List, Before, At, After) :-
    (
        list.split_list(Index - 1, List, Before0, AtAndAfter),
        AtAndAfter = [At0 | After0]
    ->
        Before = Before0,
        At = At0,
        After = After0
    ;
        unexpected($module, $pred, "split_list_at_index")
    ).

%-----------------------------------------------------------------------------%

    % Work out which constructor of the type has an argument with the
    % given field name.
    %
:- pred get_constructor_containing_field(module_info::in, mer_type::in,
    ctor_field_name::in, cons_id::out, int::out) is det.

get_constructor_containing_field(ModuleInfo, TermType, FieldName,
        ConsId, FieldNumber) :-
    type_to_ctor_det(TermType, TermTypeCtor),
    module_info_get_type_table(ModuleInfo, TypeTable),
    lookup_type_ctor_defn(TypeTable, TermTypeCtor, TermTypeDefn),
    hlds_data.get_type_defn_body(TermTypeDefn, TermTypeBody),
    (
        TermTypeBody = hlds_du_type(Ctors, _, _, _, _, _, _, _, _),
        get_constructor_containing_field_2(TermTypeCtor, Ctors, FieldName,
            ConsId, FieldNumber)
    ;
        ( TermTypeBody = hlds_eqv_type(_)
        ; TermTypeBody = hlds_foreign_type(_)
        ; TermTypeBody = hlds_solver_type(_, _)
        ; TermTypeBody = hlds_abstract_type(_)
        ),
        unexpected($module, $pred, "not du type")
    ).

:- pred get_constructor_containing_field_2(type_ctor::in,
    list(constructor)::in, ctor_field_name::in, cons_id::out, int::out) is det.

get_constructor_containing_field_2(_, [], _, _, _) :-
    unexpected($module, $pred, "can't find field").
get_constructor_containing_field_2(TypeCtor, [Ctor | Ctors], FieldName,
        ConsId, FieldNumber) :-
    Ctor = ctor(_, _, SymName, CtorArgs, _Ctxt),
    (
        get_constructor_containing_field_3(CtorArgs, FieldName,
            1, FieldNumber0)
    ->
        list.length(CtorArgs, Arity),
        ConsId = cons(SymName, Arity, TypeCtor),
        FieldNumber = FieldNumber0
    ;
        get_constructor_containing_field_2(TypeCtor, Ctors, FieldName,
            ConsId, FieldNumber)
    ).

:- pred get_constructor_containing_field_3(list(constructor_arg)::in,
    ctor_field_name::in, int::in, int::out) is semidet.

get_constructor_containing_field_3([CtorArg | CtorArgs],
        FieldName, FieldNumber0, FieldNumber) :-
    (
        CtorArg ^ arg_field_name = yes(ArgFieldName),
        UnqualFieldName = unqualify_name(ArgFieldName),
        UnqualFieldName = unqualify_name(FieldName)
    ->
        FieldNumber = FieldNumber0
    ;
        get_constructor_containing_field_3(CtorArgs, FieldName,
            FieldNumber0 + 1, FieldNumber)
    ).

%-----------------------------------------------------------------------------%

:- pred create_pure_atomic_unification_with_nonlocals(prog_var::in,
    unify_rhs::in, hlds_goal_info::in, set_of_progvar::in, list(prog_var)::in,
    unify_context::in, hlds_goal::out) is det.

create_pure_atomic_unification_with_nonlocals(Var, RHS, OldGoalInfo,
        RestrictNonLocals, VarsList, UnifyContext, Goal) :-
    Context = goal_info_get_context(OldGoalInfo),
    GoalId = goal_info_get_goal_id(OldGoalInfo),
    UnifyContext = unify_context(UnifyMainContext, UnifySubContext),
    create_pure_atomic_complicated_unification(Var, RHS,
        Context, UnifyMainContext, UnifySubContext, Goal0),
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),

    % Compute the nonlocals of the goal.
    set_of_var.list_to_set(VarsList, NonLocals1),
    set_of_var.intersect(RestrictNonLocals, NonLocals1, NonLocals),
    goal_info_set_nonlocals(NonLocals, GoalInfo0, GoalInfo1),

    % Use the goal id from the original goal, so that the constraint_ids
    % will be as expected.  (See the XXX comment near the definition of
    % constraint_id in hlds_data.m for more info.)
    goal_info_set_goal_id(GoalId, GoalInfo1, GoalInfo),
    Goal = hlds_goal(GoalExpr0, GoalInfo).

:- pred make_new_vars(list(mer_type)::in, list(prog_var)::out,
    vartypes::in, vartypes::out, prog_varset::in, prog_varset::out) is det.

make_new_vars(Types, Vars, !VarTypes, !VarSet) :-
    list.length(Types, NumVars),
    varset.new_vars(NumVars, Vars, !VarSet),
    vartypes_add_corresponding_lists(Vars, Types, !VarTypes).

:- pred make_new_var(mer_type::in, prog_var::out, vartypes::in, vartypes::out,
    prog_varset::in, prog_varset::out) is det.

make_new_var(Type, Var, !VarTypes, !VarSet) :-
    varset.new_var(Var, !VarSet),
    add_var_type(Var, Type, !VarTypes).

%-----------------------------------------------------------------------------%

check_for_missing_type_defns(ModuleInfo, Specs) :-
    module_info_get_type_table(ModuleInfo, TypeTable),
    foldl_over_type_ctor_defns(check_for_missing_type_defns_2, TypeTable,
        [], Specs).

:- pred check_for_missing_type_defns_2(type_ctor::in, hlds_type_defn::in,
    list(error_spec)::in, list(error_spec)::out) is det.

check_for_missing_type_defns_2(TypeCtor, TypeDefn, !Specs) :-
    (
        get_type_defn_status(TypeDefn, ImportStatus),
        status_defined_in_this_module(ImportStatus) = yes,
        get_type_defn_body(TypeDefn, TypeBody),
        TypeBody = hlds_abstract_type(_)
    ->
        % We expect the builtin types character, float, int and string to have
        % abstract declarations with no definitions.  The following types from
        % the type_desc module also only have abstract declarations:
        %
        %   - type_desc/0
        %   - pseudo_type_desc/0
        %   - type_ctor_desc/0
        %
        % We do not emit an error for these types.  In addition, we also don't
        % bother checking for corresponding definitions in any of the builtin
        % modules in the standard library.

        TypeCtor = type_ctor(SymName, Arity),
        BuiltinTypeCtors = builtin_type_ctors_with_no_hlds_type_defn,
        (
            sym_name_get_module_name(SymName, ModuleName),
            not any_mercury_builtin_module(ModuleName),

            % Several of the type defined in type_desc do not
            % have Mercury definitions.
            not ModuleName = unqualified("type_desc"),
            not list.member(TypeCtor, BuiltinTypeCtors)
        ->
            get_type_defn_context(TypeDefn, TypeContext),
            Pieces = [words("Error: abstract declaration for type"),
                sym_name_and_arity(SymName / Arity),
                words("has no corresponding definition."), nl],
            Msg = simple_msg(TypeContext, [always(Pieces)]),
            Spec = error_spec(severity_error, phase_type_check, [Msg]),
            !:Specs = [Spec | !.Specs]
        ;
            true
        )
    ;
        true
    ).

%-----------------------------------------------------------------------------%
:- end_module check_hlds.post_typecheck.
%-----------------------------------------------------------------------------%
