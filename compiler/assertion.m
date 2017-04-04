%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1999-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Module: assertion.m.
% Main authors: petdr.
%
% This module is an abstract interface to the assertion table.
% Note that this is a first design and will probably change
% substantially in the future.
%
%-----------------------------------------------------------------------------%

:- module hlds.assertion.
:- interface.

:- import_module hlds.hlds_data.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.set_of_var.

:- import_module list.

%-----------------------------------------------------------------------------%

    % Get the hlds_goal which represents the assertion.
    %
:- pred assert_id_goal(module_info::in, assert_id::in, hlds_goal::out) is det.

    % Record into the pred_info of each pred used in the assertion
    % the assert_id.
    %
:- pred record_preds_used_in(hlds_goal::in, assert_id::in,
    module_info::in, module_info::out) is det.

    % is_commutativity_assertion(MI, Id, Vs, CVs):
    %
    % Does the assertion represented by the assertion id, Id,
    % state the commutativity of a pred/func?
    % We extend the usual definition of commutativity to apply to
    % predicates or functions with more than two arguments
    % by allowing extra arguments which must be invariant.
    % If so, this predicate returns (in CVs) the two variables which
    % can be swapped in order if it was a call to Vs.
    %
    % The assertion must be in a form similar to this
    %   all [Is,A,B,C] ( p(Is,A,B,C) <=> p(Is,B,A,C) )
    % for the predicate to return true (note that the invariant
    % arguments, Is, can be any where providing they are in
    % identical locations on both sides of the equivalence).
    %
:- pred is_commutativity_assertion(module_info::in, assert_id::in,
    list(prog_var)::in, set_of_progvar::out) is semidet.

:- type associative_vars_output_var
    --->    associative_vars_output_var(
                % The associative vars.
                set_of_progvar,

                % The output var.
                prog_var
            ).

    % is_associativity_assertion(MI, Id, Vs, AssocVarsOutputVar):
    %
    % Does the assertion represented by the assertion id, Id,
    % state the associativity of a pred/func?
    % (We extend the usual definition of associativity to apply to
    % predicates or functions with more than two arguments
    % by allowing extra arguments which must be invariant.)
    % If so, this predicate returns
    %   associative_vars_output_var(AssocVars, OutputVar),
    % where AssocVars contains the two input variables which can be swapped
    % in order if it (what?) was a call to Vs, and OutputVar is the output
    % variable related to these two variables (for the case below, it would be
    % the variable in the same position as AB, BC or ABC).
    % XXX The above comment is both confused and confusing.
    %
    % The assertion must be in a form similar to this
    %
    %   all [Is,A,B,C,ABC]
    %   (
    %     some [AB] p(Is,A,B,AB), p(Is,AB,C,ABC)
    %   <=>
    %     some [BC] p(Is,B,C,BC), p(Is,A,BC,ABC)
    %   )
    %
    % for the predicate to return true.
    %
    % Note that the invariant arguments, Is, can be anywhere, provided
    % they are in identical locations on both sides of the equivalence.
    %
:- pred is_associativity_assertion(module_info::in, assert_id::in,
    list(prog_var)::in, associative_vars_output_var::out) is semidet.

:- type state_update_vars
    --->    state_update_vars(prog_var, prog_var).

    % is_update_assertion(MI, Id, PId, StateVars):
    %
    % is true iff the assertion, Id, is about a predicate, PId,
    % which takes some state as input and produces some state as output
    % and we are guaranteed to get the same final state regardless of
    % the order that the state is updated.
    %
    % The promise should look something like the following.
    % Note that A and B could be vectors of variables.
    %
    % :- promise all [A,B,SO,S]
    %   (
    %       (some [SA] (update(S0,A,SA), update(SA,B,S)))
    %   <=>
    %       (some [SB] (update(S0,B,SB), update(SB,A,S)))
    %   ).
    %
    % Given the actual variables, Vs, to the call to update, return
    % the pair of variables which are state variables, S0 and S,
    % in StateVars.
    %
:- pred is_update_assertion(module_info::in, assert_id::in,
    pred_id::in, list(prog_var)::in, state_update_vars::out) is semidet.

    % is_construction_equivalence_assertion(MI, Id, C, P):
    %
    % Can a single construction unification whose functor is determined
    % by the cons_id, C, be expressed as a call to the predid, P (with possibly
    % some construction unifications to initialise the arguments).
    %
    % The assertion will be in a form similar to
    %
    %   all [L,H,T] ( L = [H | T] <=> append([H], T, L) )
    %
:- pred is_construction_equivalence_assertion(module_info::in, assert_id::in,
    cons_id::in, pred_id::in) is semidet.

    % Place a hlds_goal into a standard form. Currently all the code does
    % is replace conj([G]) with G.
    %
:- pred normalise_goal(hlds_goal::in, hlds_goal::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.goal_util.
:- import_module hlds.hlds_clauses.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.

:- import_module assoc_list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module solutions.

:- type subst == map(prog_var, prog_var).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

is_commutativity_assertion(Module, AssertId, CallVars, CommutativeVars) :-
    assert_id_goal(Module, AssertId, Goal),
    goal_is_equivalence(Goal, P, Q),
    P = hlds_goal(plain_call(PredId, _, VarsP, _, _, _), _),
    Q = hlds_goal(plain_call(PredId, _, VarsQ, _, _, _), _),
    commutative_var_ordering(VarsP, VarsQ, CallVars, CommutativeVars).

    % commutative_var_ordering(Ps, Qs, Vs, CommutativeVs):
    %
    % Check that the two list of variables are identical except that
    % the position of two variables has been swapped.
    % For example, commutative_var_ordering is true for [A,B,C] and [B,A,C].
    % It also takes a list of variables, Vs, to a call and returns
    % the two variables in that list that can be swapped. In this case,
    % that will be [A,B].
    %
:- pred commutative_var_ordering(list(prog_var)::in, list(prog_var)::in,
    list(prog_var)::in, set_of_progvar::out) is semidet.

commutative_var_ordering([P | Ps], [Q | Qs], [V | Vs], CommutativeVars) :-
    ( if P = Q then
        commutative_var_ordering(Ps, Qs, Vs, CommutativeVars)
    else
        commutative_var_ordering_2(P, Q, Ps, Qs, Vs, CallVarB),
        CommutativeVars = set_of_var.list_to_set([V, CallVarB])
    ).

:- pred commutative_var_ordering_2(prog_var::in, prog_var::in,
    list(prog_var)::in, list(prog_var)::in, list(prog_var)::in,
    prog_var::out) is semidet.

commutative_var_ordering_2(VarP, VarQ, [P | Ps], [Q | Qs], [V | Vs],
        CallVarB) :-
    ( if P = Q then
        commutative_var_ordering_2(VarP, VarQ, Ps, Qs, Vs, CallVarB)
    else
        CallVarB = V,
        P = VarQ,
        Q = VarP,
        Ps = Qs
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

is_associativity_assertion(Module, AssertId, CallVars,
        AssociativeVarsOutputVar) :-
    assert_id_goal(Module, AssertId, hlds_goal(GoalExpr, GoalInfo)),
    goal_is_equivalence(hlds_goal(GoalExpr, GoalInfo), P, Q),

    UniversiallyQuantifiedVars = goal_info_get_nonlocals(GoalInfo),

    get_conj_goals(P, PCalls),
    get_conj_goals(Q, QCalls),
    promise_equivalent_solutions [AssociativeVarsOutputVar] (
        associative(PCalls, QCalls, UniversiallyQuantifiedVars, CallVars,
            AssociativeVarsOutputVar)
    ).

    % associative(Ps, Qs, Us, R):
    %
    % If the assertion was in the form
    %   all [Us] (some [] (Ps)) <=> (some [] (Qs))
    % try and rearrange the order of Ps and Qs so that the assertion
    % is in the standard from
    %
    %   compose( A, B,  AB),        compose(B,  C,  BC),
    %   compose(AB, C, ABC)     <=> compose(A, BC, ABC)
    %
:- pred associative(list(hlds_goal)::in, list(hlds_goal)::in,
    set_of_progvar::in, list(prog_var)::in,
    associative_vars_output_var::out) is cc_nondet.

associative(PCalls, QCalls, UniversiallyQuantifiedVars, CallVars,
        AssociativeVarsOutputVar) :-
    reorder(PCalls, QCalls, LHSCalls, RHSCalls),
    process_one_side(LHSCalls, UniversiallyQuantifiedVars, PredId,
        AB, PairsL, Vs),
    process_one_side(RHSCalls, UniversiallyQuantifiedVars, PredId,
        BC, PairsR, _),

    % If you read the predicate documentation, you will note that
    % for each pair of variables on the left hand side, there is an equivalent
    % pair of variables on the right hand side. As the pairs of variables
    % are not symmetric, the call to list.perm will only succeed once,
    % if at all.
    assoc_list.from_corresponding_lists(PairsL, PairsR, Pairs),
    list.perm(Pairs, [(A - AB) - (B - A), (B - C) - (C - BC),
        (AB - ABC) - (BC - ABC)]),

    assoc_list.from_corresponding_lists(Vs, CallVars, AssocList),
    list.filter((pred(X - _Y::in) is semidet :- X = AB),
        AssocList, [_AB - OutputVar]),
    list.filter((pred(X - _Y::in) is semidet :- X = A),
        AssocList, [_A - CallVarA]),
    list.filter((pred(X - _Y::in) is semidet :- X = B),
        AssocList, [_B - CallVarB]),

    AssociativeVars = set_of_var.list_to_set([CallVarA, CallVarB]),
    AssociativeVarsOutputVar =
        associative_vars_output_var(AssociativeVars, OutputVar).

    % reorder(Ps, Qs, Ls, Rs):
    %
    % Given both sides of the equivalence return another possible ordering.
    %
:- pred reorder(list(hlds_goal)::in, list(hlds_goal)::in,
    list(hlds_goal)::out, list(hlds_goal)::out) is multi.

reorder(PCalls, QCalls, LHSCalls, RHSCalls) :-
    list.perm(PCalls, LHSCalls),
    list.perm(QCalls, RHSCalls).
reorder(PCalls, QCalls, LHSCalls, RHSCalls) :-
    list.perm(PCalls, RHSCalls),
    list.perm(QCalls, LHSCalls).

    % process_one_side(Gs, Us, L, Ps):
    %
    % Given the list of goals, Gs, which are one side of a possible
    % associative equivalence, and the universally quantified
    % variables, Us, of the goals return L the existentially
    % quantified variable that links the two calls and Ps the list
    % of variables which are not invariants.
    %
    % i.e. for app(TypeInfo, X, Y, XY), app(TypeInfo, XY, Z, XYZ)
    % L <= XY and Ps <= [X - XY, Y - Z, XY - XYZ]
    %
:- pred process_one_side(list(hlds_goal)::in, set_of_progvar::in, pred_id::out,
    prog_var::out, assoc_list(prog_var)::out, list(prog_var)::out) is semidet.

process_one_side(Goals, UniversiallyQuantifiedVars, PredId,
        LinkingVar, Vars, VarsA) :-
    process_two_linked_calls(Goals, UniversiallyQuantifiedVars, PredId,
        LinkingVar, Vars0, VarsA),

    % Filter out all the invariant arguments, and then make sure that
    % there are only 3 arguments left.
    list.filter((pred(X - Y::in) is semidet :- X \= Y), Vars0, Vars),
    list.length(Vars, number_of_associative_vars).

:- func number_of_associative_vars = int.

number_of_associative_vars = 3.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

is_update_assertion(Module, AssertId, _PredId, CallVars, StateVars) :-
    assert_id_goal(Module, AssertId, hlds_goal(GoalExpr, GoalInfo)),
    goal_is_equivalence(hlds_goal(GoalExpr, GoalInfo), P, Q),
    UniversiallyQuantifiedVars = goal_info_get_nonlocals(GoalInfo),

    get_conj_goals(P, PCalls),
    get_conj_goals(Q, QCalls),
    solutions.solutions(
        update(PCalls, QCalls, UniversiallyQuantifiedVars, CallVars),
        [StateVars | _]).

    %   compose(S0, A, SA),     compose(SB, A, S),
    %   compose(SA, B, S)   <=> compose(S0, B, SB)
    %
:- pred update(list(hlds_goal)::in, list(hlds_goal)::in, set_of_progvar::in,
    list(prog_var)::in, state_update_vars::out) is nondet.

update(PCalls, QCalls, UniversiallyQuantifiedVars, CallVars,
        StateVars) :-
    reorder(PCalls, QCalls, LHSCalls, RHSCalls),
    process_two_linked_calls(LHSCalls, UniversiallyQuantifiedVars, PredId,
        SA, PairsL, Vs),
    process_two_linked_calls(RHSCalls, UniversiallyQuantifiedVars, PredId,
        SB, PairsR, _),

    assoc_list.from_corresponding_lists(PairsL, PairsR, Pairs0),
    list.filter((pred(X - Y::in) is semidet :- X \= Y), Pairs0, Pairs),
    list.length(Pairs) = 2,

    % If you read the predicate documentation, you will note that
    % for each pair of variables on the left hand side there is an equivalent
    % pair of variables on the right hand side. As the pairs of variables
    % are not symmetric, the call to list.perm will only succeed once,
    % if at all.
    list.perm(Pairs, [(S0 - SA) - (SB - S0), (SA - S) - (S - SB)]),

    assoc_list.from_corresponding_lists(Vs, CallVars, AssocList),
    list.filter((pred(X - _Y::in) is semidet :- X = S0),
        AssocList, [_S0 - StateA]),
    list.filter((pred(X - _Y::in) is semidet :- X = SA),
        AssocList, [_SA - StateB]),
    StateVars = state_update_vars(StateA, StateB).

%-----------------------------------------------------------------------------%

    % process_two_linked_calls(Gs, UQVs, PId, LV, AL, VAs):
    %
    % is true iff the list of goals, Gs, with universally quantified
    % variables, UQVs, is two calls to the same predicate, PId, with
    % one variable that links them, LV. AL will be the assoc list
    % that is the each variable from the first call with its
    % corresponding variable in the second call, and VAs are the
    % variables of the first call.
    %
:- pred process_two_linked_calls(list(hlds_goal)::in, set_of_progvar::in,
    pred_id::out, prog_var::out, assoc_list(prog_var)::out,
    list(prog_var)::out) is semidet.

process_two_linked_calls(Goals, UniversiallyQuantifiedVars, PredId,
        LinkingVar, Vars, VarsA) :-
    Goals = [
        hlds_goal(plain_call(PredId, _, VarsA, _, _, _), _),
        hlds_goal(plain_call(PredId, _, VarsB, _, _, _), _)
    ],

    % Determine the linking variable, L. By definition, it must be
    % existentially quantified, and a member of both variable lists.
    SetVarsA = set_of_var.list_to_set(VarsA),
    SetVarsB = set_of_var.list_to_set(VarsB),
    CommonVars = set_of_var.intersect(SetVarsA, SetVarsB),
    set_of_var.is_singleton(
        set_of_var.difference(CommonVars, UniversiallyQuantifiedVars),
        LinkingVar),

    % Set up mapping between the variables in the two calls.
    assoc_list.from_corresponding_lists(VarsA, VarsB, Vars).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

is_construction_equivalence_assertion(Module, AssertId, ConsId, PredId) :-
    assert_id_goal(Module, AssertId, Goal),
    goal_is_equivalence(Goal, P, Q),
    ( if single_construction(P, ConsId) then
        predicate_call(Q, PredId)
    else
        single_construction(Q, ConsId),
        predicate_call(P, PredId)
    ).

    % One side of the equivalence must be just the single unification
    % with the correct cons_id.
    %
:- pred single_construction(hlds_goal::in, cons_id::in) is semidet.

single_construction(Goal, ConsId) :-
    Goal = hlds_goal(GoalExpr, _),
    GoalExpr = unify(_, UnifyRHS, _, _, _),
    UnifyRHS = rhs_functor(cons(UnqualifiedSymName, Arity, _TypeCtorA), _, _),
    ConsId = cons(QualifiedSymName, Arity, _TypeCtorB),
    % Before post-typecheck, TypeCtorA and TypeCtorB would be dummies,
    % and would thus match even if the two functors are NOT of the same type.
    % Note that by insisting on cons, we effectively disallow assertions
    % about tuples.
    partial_sym_name_matches_full(UnqualifiedSymName, QualifiedSymName).

    % The side containing the predicate call must be a single call
    % to the predicate with 0 or more construction unifications
    % which setup the arguments to the predicates.
    %
:- pred predicate_call(hlds_goal::in, pred_id::in) is semidet.

predicate_call(Goal, PredId) :-
    ( if Goal = hlds_goal(conj(plain_conj, Goals), _) then
        list.member(Call, Goals),
        Call = hlds_goal(plain_call(PredId, _, _, _, _, _), _),
        list.delete(Goals, Call, Unifications),
        P =
            ( pred(G::in) is semidet :-
                not (
                    G = hlds_goal(unify(_, UnifyRhs, _, _, _), _),
                    UnifyRhs = rhs_functor(_, _, _)
                )
            ),
        list.filter(P, Unifications, [])
    else
        Goal = hlds_goal(plain_call(PredId, _, _, _, _, _), _)
    ).

%-----------------------------------------------------------------------------%

:- pred get_conj_goals(hlds_goal::in, list(hlds_goal)::out) is semidet.

get_conj_goals(Goal0, ConjList) :-
    % The user may have explicitly quantified the goals.
    ignore_exist_quant_scope(Goal0, Goal),
    Goal = hlds_goal(conj(plain_conj, ConjList), _).

:- pred ignore_exist_quant_scope(hlds_goal::in, hlds_goal::out) is det.

ignore_exist_quant_scope(Goal0, Goal) :-
    Goal0 = hlds_goal(GoalExpr0, _Context),
    ( if
        GoalExpr0 = scope(Reason, Goal1),
        Reason = exist_quant(_)
    then
        Goal = Goal1
    else
        Goal = Goal0
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

assert_id_goal(Module, AssertId, Goal) :-
    module_info_get_assertion_table(Module, AssertTable),
    assertion_table_lookup(AssertTable, AssertId, PredId),
    module_info_pred_info(Module, PredId, PredInfo),
    pred_info_get_clauses_info(PredInfo, ClausesInfo),
    clauses_info_get_clauses_rep(ClausesInfo, ClausesRep, _ItemNumbers),
    get_clause_list_maybe_repeated(ClausesRep, Clauses),
    (
        Clauses = [Clause],
        Goal0 = Clause ^ clause_body,
        normalise_goal(Goal0, Goal)
    ;
        ( Clauses = []
        ; Clauses = [_, _ | _]
        ),
        unexpected($module, $pred, "goal is not an assertion")
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred goal_is_implication(hlds_goal::in, hlds_goal::out, hlds_goal::out)
    is semidet.

goal_is_implication(Goal, P, Q) :-
    % Goal = (P => Q)
    Goal = hlds_goal(negation(hlds_goal(conj(plain_conj, GoalList), _)), GI),
    list.reverse(GoalList) = [NotQ | Ps],
    ( if Ps = [P0] then
        P = P0
    else
        P = hlds_goal(conj(plain_conj, list.reverse(Ps)), GI)
    ),
    NotQ = hlds_goal(negation(Q), _).

:- pred goal_is_equivalence(hlds_goal::in, hlds_goal::out, hlds_goal::out)
    is semidet.

goal_is_equivalence(Goal, P, Q) :-
    % Goal = P <=> Q
    Goal = hlds_goal(conj(plain_conj, [A, B]), _GoalInfo),
    map.init(Subst),
    goal_is_implication(A, PA, QA),
    goal_is_implication(B, QB, PB),
    equal_goals(PA, PB, Subst, _),
    equal_goals(QA, QB, Subst, _),
    P = PA,
    Q = QA.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

    % equal_goals(GA, GB, !Subst):
    %
    % Do these two goals represent the same hlds_goal modulo renaming?
    %
:- pred equal_goals(hlds_goal::in, hlds_goal::in,
    subst::in, subst::out) is semidet.

equal_goals(GoalA, GoalB, !Subst) :-
    GoalA = hlds_goal(GoalExprA, _GoalInfoA),
    GoalB = hlds_goal(GoalExprB, _GoalInfoB),
    require_complete_switch [GoalExprA]
    (
        GoalExprA = conj(ConjType, GoalsA),
        GoalExprB = conj(ConjType, GoalsB),
        equal_goals_list(GoalsA, GoalsB, !Subst)
    ;
        GoalExprA = plain_call(PredId, _, ArgVarsA, _, _, _),
        GoalExprB = plain_call(PredId, _, ArgVarsB, _, _, _),
        equal_vars(ArgVarsA, ArgVarsB, !Subst)
    ;
        GoalExprA = generic_call(CallDetails, ArgVarsA, _, _, _),
        GoalExprB = generic_call(CallDetails, ArgVarsB, _, _, _),
        equal_vars(ArgVarsA, ArgVarsB, !Subst)
    ;
        GoalExprA = switch(Var, CanFail, CasesA),
        GoalExprB = switch(Var, CanFail, CasesB),
        equal_goals_cases(CasesA, CasesB, !Subst)
    ;
        GoalExprA = unify(VarA, RHSA, _, _, _),
        GoalExprB = unify(VarB, RHSB, _, _, _),
        equal_var(VarA, VarB, !Subst),
        equal_unification(RHSA, RHSB, !Subst)
    ;
        GoalExprA = disj(GoalsA),
        GoalExprB = disj(GoalsB),
        equal_goals_list(GoalsA, GoalsB, !Subst)
    ;
        GoalExprA = negation(SubGoalA),
        GoalExprB = negation(SubGoalB),
        equal_goals(SubGoalA, SubGoalB, !Subst)
    ;
        GoalExprA = scope(ReasonA, SubGoalA),
        GoalExprB = scope(ReasonB, SubGoalB),
        equal_reason(ReasonA, ReasonB, !Subst),
        equal_goals(SubGoalA, SubGoalB, !Subst)
    ;
        GoalExprA = if_then_else(VarsA, CondA, ThenA, ElseA),
        GoalExprB = if_then_else(VarsB, CondB, ThenB, ElseB),
        equal_vars(VarsA, VarsB, !Subst),
        equal_goals(CondA, CondB, !Subst),
        equal_goals(ThenA, ThenB, !Subst),
        equal_goals(ElseA, ElseB, !Subst)
    ;
        GoalExprA = call_foreign_proc(Attributes, PredId, _,
            ArgsA, ExtraA, MaybeTraceA, _),
        GoalExprB = call_foreign_proc(Attributes, PredId, _,
            ArgsB, ExtraB, MaybeTraceB, _),
        % Foreign_procs with extra args and trace runtime conditions are
        % compiler generated, and as such will not participate in assertions.
        ExtraA = [],
        ExtraB = [],
        MaybeTraceA = no,
        MaybeTraceB = no,
        VarsA = list.map(foreign_arg_var, ArgsA),
        VarsB = list.map(foreign_arg_var, ArgsB),
        equal_vars(VarsA, VarsB, !Subst)
    ;
        GoalExprA = shorthand(ShortHandA),
        GoalExprB = shorthand(ShortHandB),
        equal_goals_shorthand(ShortHandA, ShortHandB, !Subst)
    ).

:- pred equal_reason(scope_reason::in, scope_reason::in, subst::in, subst::out)
    is semidet.

equal_reason(ReasonA, ReasonB, !Subst) :-
    require_complete_switch [ReasonA]
    (
        ReasonA = exist_quant(VarsA),
        ReasonB = exist_quant(VarsB),
        equal_vars(VarsA, VarsB, !Subst)
    ;
        ReasonA = disable_warnings(HeadWarning, TailWarnings),
        ReasonB = disable_warnings(HeadWarning, TailWarnings)
    ;
        ReasonA = promise_solutions(VarsA, Kind),
        ReasonB = promise_solutions(VarsB, Kind),
        equal_vars(VarsA, VarsB, !Subst)
    ;
        ReasonA = promise_purity(Purity),
        ReasonB = promise_purity(Purity)
    ;
        ReasonA = require_detism(Detism),
        ReasonB = require_detism(Detism)
    ;
        ReasonA = require_complete_switch(VarA),
        ReasonB = require_complete_switch(VarB),
        equal_var(VarA, VarB, !Subst)
    ;
        ReasonA = require_switch_arms_detism(VarA, Detism),
        ReasonB = require_switch_arms_detism(VarB, Detism),
        equal_var(VarA, VarB, !Subst)
    ;
        ReasonA = commit(ForcePruning),
        ReasonB = commit(ForcePruning)
    ;
        ReasonA = barrier(Removable),
        ReasonB = barrier(Removable)
    ;
        ReasonA = from_ground_term(VarA, Kind),
        ReasonB = from_ground_term(VarB, Kind),
        equal_var(VarA, VarB, !Subst)
    ;
        ReasonA = trace_goal(_CompileTime, _Runtime, _MaybeIO,
            _MutableVars, _QuantVars),
        % We could match all the vars, but there would be no point;
        % trace goals should not occur in assertions.
        fail
    ;
        ReasonA = loop_control(_LCVar, _LCSVar, _UseParentStack),
        % We could match the vars, but there would be no point.
        % These scopes can never occur in user-written code;
        % they can only added by the compiler itself.
        fail
    ).

:- pred equal_goals_shorthand(shorthand_goal_expr::in, shorthand_goal_expr::in,
    subst::in, subst::out) is semidet.

equal_goals_shorthand(ShortHandA, ShortHandB, !Subst) :-
    ShortHandA = bi_implication(LeftGoalA, RightGoalA),
    ShortHandB = bi_implication(LeftGoalB, RightGoalB),
    equal_goals(LeftGoalA, LeftGoalB, !Subst),
    equal_goals(RightGoalA, RightGoalB, !Subst).

:- pred equal_var(prog_var::in, prog_var::in, subst::in, subst::out)
    is semidet.

equal_var(VA, VB, !Subst) :-
    ( if map.search(!.Subst, VA, SubstVA) then
        SubstVA = VB
    else
        map.insert(VA, VB, !Subst)
    ).

:- pred equal_vars(list(prog_var)::in, list(prog_var)::in,
    subst::in, subst::out) is semidet.

equal_vars([], [], !Subst).
equal_vars([VA | VAs], [VB | VBs], !Subst) :-
    equal_var(VA, VB, !Subst),
    equal_vars(VAs, VBs, !Subst).

:- pred equal_unification(unify_rhs::in, unify_rhs::in, subst::in, subst::out)
    is semidet.

equal_unification(RhsA, RhsB, !Subst) :-
    (
        RhsA = rhs_var(A),
        RhsB = rhs_var(B),
        equal_var(A, B, !Subst)
    ;
        RhsA = rhs_functor(ConsId, E, VarsA),
        RhsB = rhs_functor(ConsId, E, VarsB),
        equal_vars(VarsA, VarsB, !Subst)
    ;
        RhsA = rhs_lambda_goal(Purity, Groundness, PredOrFunc, EvalMethod,
            NLVarsA, LVarsA, Modes, Det, GoalA),
        RhsB = rhs_lambda_goal(Purity, Groundness, PredOrFunc, EvalMethod,
            NLVarsB, LVarsB, Modes, Det, GoalB),
        equal_vars(NLVarsA, NLVarsB, !Subst),
        equal_vars(LVarsA, LVarsB, !Subst),
        equal_goals(GoalA, GoalB, !Subst)
    ).

:- pred equal_goals_list(list(hlds_goal)::in, list(hlds_goal)::in,
    subst::in, subst::out) is semidet.

equal_goals_list([], [], !Subst).
equal_goals_list([GoalA | GoalAs], [GoalB | GoalBs], !Subst) :-
    equal_goals(GoalA, GoalB, !Subst),
    equal_goals_list(GoalAs, GoalBs, !Subst).

:- pred equal_goals_cases(list(case)::in, list(case)::in,
    subst::in, subst::out) is semidet.

equal_goals_cases([], [], !Subst).
equal_goals_cases([CaseA | CaseAs], [CaseB | CaseBs], !Subst) :-
    CaseA = case(MainConsIdA, OtherConsIdsA, GoalA),
    CaseB = case(MainConsIdB, OtherConsIdsB, GoalB),
    list.sort([MainConsIdA | OtherConsIdsA], SortedConsIds),
    list.sort([MainConsIdB | OtherConsIdsB], SortedConsIds),
    equal_goals(GoalA, GoalB, !Subst),
    equal_goals_cases(CaseAs, CaseBs, !Subst).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

record_preds_used_in(Goal, AssertId, !Module) :-
    predids_from_goal(Goal, PredIds),
    % Sanity check.
    ( if list.member(invalid_pred_id, PredIds) then
        unexpected($module, $pred, "invalid pred_id")
    else
        true
    ),
    list.foldl(update_pred_info(AssertId), PredIds, !Module).

%-----------------------------------------------------------------------------%

    % update_pred_info(Id, A, !Module):
    %
    % Record in the pred_info pointed to by Id that that predicate
    % is used in the assertion pointed to by A.
    %
:- pred update_pred_info(assert_id::in, pred_id::in,
    module_info::in, module_info::out) is det.

update_pred_info(AssertId, PredId, !Module) :-
    module_info_pred_info(!.Module, PredId, PredInfo0),
    pred_info_get_assertions(PredInfo0, Assertions0),
    set.insert(AssertId, Assertions0, Assertions),
    pred_info_set_assertions(Assertions, PredInfo0, PredInfo),
    module_info_set_pred_info(PredId, PredInfo, !Module).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

normalise_goal(Goal0, Goal) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo),
    (
        ( GoalExpr0 = plain_call(_, _, _, _, _, _)
        ; GoalExpr0 = generic_call(_, _, _, _, _)
        ; GoalExpr0 = unify(_, _, _, _, _)
        ; GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _)
        ),
        GoalExpr = GoalExpr0
    ;
        GoalExpr0 = conj(ConjType, SubGoals0),
        (
            ConjType = plain_conj,
            normalise_conj(SubGoals0, SubGoals)
        ;
            ConjType = parallel_conj,
            normalise_goals(SubGoals0, SubGoals)
        ),
        GoalExpr = conj(ConjType, SubGoals)
    ;
        GoalExpr0 = switch(Var, CanFail, Cases0),
        normalise_cases(Cases0, Cases),
        GoalExpr = switch(Var, CanFail, Cases)
    ;
        GoalExpr0 = disj(SubGoals0),
        normalise_goals(SubGoals0, SubGoals),
        GoalExpr = disj(SubGoals)
    ;
        GoalExpr0 = negation(SubGoal0),
        normalise_goal(SubGoal0, SubGoal),
        GoalExpr = negation(SubGoal)
    ;
        GoalExpr0 = scope(Reason, SubGoal0),
        normalise_goal(SubGoal0, SubGoal),
        GoalExpr = scope(Reason, SubGoal)
    ;
        GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),
        normalise_goal(Cond0, Cond),
        normalise_goal(Then0, Then),
        normalise_goal(Else0, Else),
        GoalExpr = if_then_else(Vars, Cond, Then, Else)
    ;
        GoalExpr0 = shorthand(ShortHand0),
        (
            ShortHand0 = atomic_goal(GoalType, Outer, Inner, Vars,
                MainGoal0, OrElseAlternatives0, OrElseInners),
            normalise_goal(MainGoal0, MainGoal),
            normalise_goals(OrElseAlternatives0, OrElseAlternatives),
            ShortHand = atomic_goal(GoalType, Outer, Inner, Vars, MainGoal,
                OrElseAlternatives, OrElseInners)
        ;
            ShortHand0 = try_goal(MaybeIO, ResultVar, SubGoal0),
            normalise_goal(SubGoal0, SubGoal),
            ShortHand = try_goal(MaybeIO, ResultVar, SubGoal)
        ;
            ShortHand0 = bi_implication(LHS0, RHS0),
            normalise_goal(LHS0, LHS),
            normalise_goal(RHS0, RHS),
            ShortHand = bi_implication(LHS, RHS)
        ),
        GoalExpr = shorthand(ShortHand)
    ),
    Goal = hlds_goal(GoalExpr, GoalInfo).

%-----------------------------------------------------------------------------%

:- pred normalise_conj(list(hlds_goal)::in, list(hlds_goal)::out) is det.

normalise_conj([], []).
normalise_conj([Goal0 | Goals0], Goals) :-
    goal_to_conj_list(Goal0, ConjGoals),
    normalise_conj(Goals0, Goals1),
    list.append(ConjGoals, Goals1, Goals).

:- pred normalise_cases(list(case)::in, list(case)::out) is det.

normalise_cases([], []).
normalise_cases([Case0 | Cases0], [Case | Cases]) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    normalise_goal(Goal0, Goal),
    Case = case(MainConsId, OtherConsIds, Goal),
    normalise_cases(Cases0, Cases).

:- pred normalise_goals(list(hlds_goal)::in, list(hlds_goal)::out) is det.

normalise_goals([], []).
normalise_goals([Goal0 | Goals0], [Goal | Goals]) :-
    normalise_goal(Goal0, Goal),
    normalise_goals(Goals0, Goals).

%-----------------------------------------------------------------------------%
:- end_module hlds.assertion.
%-----------------------------------------------------------------------------%
