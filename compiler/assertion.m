%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1999-2006 The University of Melbourne.
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
:- import_module parse_tree.prog_data.

:- import_module pair.

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
    % predicates or functions with more than two arguments as
    % follows by allowing extra arguments which must be invariant.
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
    prog_vars::in, pair(prog_var)::out) is semidet.

    % is_associativity_assertion(MI, Id, Vs, CVs, OV):
    %
    % Does the assertion represented by the assertion id, Id,
    % state the associativity of a pred/func?
    % We extend the usual definition of associativity to apply to
    % predicates or functions with more than two arguments as
    % follows by allowing extra arguments which must be invariant.
    % If so, this predicate returns (in CVs) the two variables which
    % can be swapped in order if it was a call to Vs, and the
    % output variable, OV, related to these two variables (for the
    % case below it would be the variable in the same position as
    % AB, BC or ABC).
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
    % for the predicate to return true (note that the invariant
    % arguments, Is, can be any where providing they are in
    % identical locations on both sides of the equivalence).
    %
:- pred is_associativity_assertion(module_info::in, assert_id::in,
    prog_vars::in, pair(prog_var)::out, prog_var::out) is semidet.

    % is_update_assertion(MI, Id, PId, Ss):
    %
    % is true iff the assertion, Id, is about a predicate, PId,
    % which takes some state as input and produces some state as output
    % and we are guaranteed to get the same final state regardless of
    % the order that the state is updated.
    %
    % i.e. the promise should look something like this, note that A
    % and B could be vectors of variables.
    %
    % :- promise all [A,B,SO,S]
    %   (
    %       (some [SA] (update(S0,A,SA), update(SA,B,S)))
    %   <=>
    %       (some [SB] (update(S0,B,SB), update(SB,A,S)))
    %   ).
    %
    % Given the actual variables, Vs, to the call to update, return
    % the pair of variables which are state variables, SPair.
    %
:- pred is_update_assertion(module_info::in, assert_id::in,
    pred_id::in, prog_vars::in, pair(prog_var)::out) is semidet.

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

    % Place a hlds_goal into a standard form.  Currently all the
    % code does is replace conj([G]) with G.
    %
:- pred normalise_goal(hlds_goal::in, hlds_goal::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.type_util.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_clauses.
:- import_module libs.compiler_util.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_util.

:- import_module assoc_list.
:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module set.
:- import_module solutions.
:- import_module string.

:- type subst == map(prog_var, prog_var).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

is_commutativity_assertion(Module, AssertId, CallVars, CommutativeVars) :-
    assert_id_goal(Module, AssertId, Goal),
    goal_is_equivalence(Goal, P, Q),
    P = plain_call(PredId, _, VarsP, _, _, _) - _,
    Q = plain_call(PredId, _, VarsQ, _, _, _) - _,
    commutative_var_ordering(VarsP, VarsQ, CallVars, CommutativeVars).

    % commutative_var_ordering(Ps, Qs, Vs, CommutativeVs):
    %
    % Check that the two list of variables are identical except that
    % the position of two variables has been swapped.
    % e.g [A,B,C] and [B,A,C] is true.
    % It also takes a list of variables, Vs, to a call and returns
    % the two variables in that list that can be swapped, ie [A,B].
    %
:- pred commutative_var_ordering(prog_vars::in, prog_vars::in,
    prog_vars::in, pair(prog_var)::out) is semidet.

commutative_var_ordering([P | Ps], [Q | Qs], [V | Vs], CommutativeVars) :-
    ( P = Q ->
        commutative_var_ordering(Ps, Qs, Vs, CommutativeVars)
    ;
        commutative_var_ordering_2(P, Q, Ps, Qs, Vs, CallVarB),
        CommutativeVars = V - CallVarB
    ).

:- pred commutative_var_ordering_2(prog_var::in, prog_var::in, prog_vars::in,
    prog_vars::in, prog_vars::in, prog_var::out) is semidet.

commutative_var_ordering_2(VarP, VarQ, [P | Ps], [Q | Qs], [V | Vs],
        CallVarB) :-
    ( P = Q ->
        commutative_var_ordering_2(VarP, VarQ, Ps, Qs, Vs, CallVarB)
    ;
        CallVarB = V,
        P = VarQ,
        Q = VarP,
        Ps = Qs
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

is_associativity_assertion(Module, AssertId, CallVars,
        AssociativeVars, OutputVar) :-
    assert_id_goal(Module, AssertId, Goal - GoalInfo),
    goal_is_equivalence(Goal - GoalInfo, P, Q),

    goal_info_get_nonlocals(GoalInfo, UniversiallyQuantifiedVars),

        % There may or may not be a some [] depending on whether
        % the user explicity qualified the call or not.
    (
        P = scope(_, conj(plain_conj, PCalls0) - _) - _PGoalInfo,
        Q = scope(_, conj(plain_conj, QCalls0) - _) - _QGoalInfo
    ->
        PCalls = PCalls0,
        QCalls = QCalls0
    ;
        P = conj(plain_conj, PCalls) - _PGoalInfo,
        Q = conj(plain_conj, QCalls) - _QGoalInfo
    ),
    promise_equivalent_solutions [AssociativeVars, OutputVar] (
         associative(PCalls, QCalls, UniversiallyQuantifiedVars, CallVars,
            AssociativeVars - OutputVar)
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
:- pred associative(hlds_goals::in, hlds_goals::in,
    set(prog_var)::in, prog_vars::in,
    pair(pair(prog_var), prog_var)::out) is cc_nondet.

associative(PCalls, QCalls, UniversiallyQuantifiedVars, CallVars,
        (CallVarA - CallVarB) - OutputVar) :-
    reorder(PCalls, QCalls, LHSCalls, RHSCalls),
    process_one_side(LHSCalls, UniversiallyQuantifiedVars, PredId,
        AB, PairsL, Vs),
    process_one_side(RHSCalls, UniversiallyQuantifiedVars, PredId,
        BC, PairsR, _),

    % If you read the predicate documentation, you will note that
    % for each pair of variables on the left hand side there are an equivalent
    % pair of variables on the right hand side. As the pairs of variables
    % are not symmetric, the call to list.perm will only succeed once,
    % if at all.
    assoc_list.from_corresponding_lists(PairsL, PairsR, Pairs),
    list.perm(Pairs, [(A - AB) - (B - A), (B - C) - (C - BC),
        (AB - ABC) - (BC - ABC)]),

    assoc_list.from_corresponding_lists(Vs, CallVars, AssocList),
    list.filter((pred(X-_Y::in) is semidet :- X = AB),
        AssocList, [_AB - OutputVar]),
    list.filter((pred(X-_Y::in) is semidet :- X = A),
        AssocList, [_A - CallVarA]),
    list.filter((pred(X-_Y::in) is semidet :- X = B),
        AssocList, [_B - CallVarB]).

    % reorder(Ps, Qs, Ls, Rs):
    %
    % Given both sides of the equivalence return another possible ordering.
    %
:- pred reorder(hlds_goals::in, hlds_goals::in,
    hlds_goals::out, hlds_goals::out) is multi.

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
:- pred process_one_side(hlds_goals::in, set(prog_var)::in, pred_id::out,
    prog_var::out, assoc_list(prog_var)::out, prog_vars::out) is semidet.

process_one_side(Goals, UniversiallyQuantifiedVars, PredId,
        LinkingVar, Vars, VarsA) :-
    process_two_linked_calls(Goals, UniversiallyQuantifiedVars, PredId,
        LinkingVar, Vars0, VarsA),

    % Filter out all the invariant arguments, and then make sure that
    % their is only 3 arguments left.
    list.filter((pred(X-Y::in) is semidet :- not X = Y), Vars0, Vars),
    list.length(Vars, number_of_associative_vars).

:- func number_of_associative_vars = int.

number_of_associative_vars = 3.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

is_update_assertion(Module, AssertId, _PredId, CallVars, StateA - StateB) :-
    assert_id_goal(Module, AssertId, Goal - GoalInfo),
    goal_is_equivalence(Goal - GoalInfo, P, Q),
    goal_info_get_nonlocals(GoalInfo, UniversiallyQuantifiedVars),

        % There may or may not be an explicit some [Vars] there,
        % as quantification now works correctly.
    (
        P = scope(_, conj(plain_conj, PCalls0) - _) - _PGoalInfo,
        Q = scope(_, conj(plain_conj, QCalls0) - _) - _QGoalInfo
    ->
        PCalls = PCalls0,
        QCalls = QCalls0
    ;
        P = conj(plain_conj, PCalls) - _PGoalInfo,
        Q = conj(plain_conj, QCalls) - _QGoalInfo
    ),

    solutions.solutions(update(PCalls, QCalls,
        UniversiallyQuantifiedVars, CallVars), [StateA - StateB | _]).

    %   compose(S0, A, SA),     compose(SB, A, S),
    %   compose(SA, B, S)   <=> compose(S0, B, SB)
    %
:- pred update(hlds_goals::in, hlds_goals::in, set(prog_var)::in,
    prog_vars::in, pair(prog_var)::out) is nondet.

update(PCalls, QCalls, UniversiallyQuantifiedVars, CallVars,
        StateA - StateB) :-
    reorder(PCalls, QCalls, LHSCalls, RHSCalls),
    process_two_linked_calls(LHSCalls, UniversiallyQuantifiedVars, PredId,
        SA, PairsL, Vs),
    process_two_linked_calls(RHSCalls, UniversiallyQuantifiedVars, PredId,
        SB, PairsR, _),

    assoc_list.from_corresponding_lists(PairsL, PairsR, Pairs0),
    list.filter((pred(X-Y::in) is semidet :- X \= Y), Pairs0, Pairs),
    list.length(Pairs) = 2,

    % If you read the predicate documentation, you will note that
    % for each pair of variables on the left hand side there is an equivalent
    % pair of variables on the right hand side. As the pairs of variables
    % are not symmetric, the call to list.perm will only succeed once,
    % if at all.
    list.perm(Pairs, [(S0 - SA) - (SB - S0), (SA - S) - (S - SB)]),

    assoc_list.from_corresponding_lists(Vs, CallVars, AssocList),
    list.filter((pred(X-_Y::in) is semidet :- X = S0),
        AssocList, [_S0 - StateA]),
    list.filter((pred(X-_Y::in) is semidet :- X = SA),
        AssocList, [_SA - StateB]).

%-----------------------------------------------------------------------------%

    % process_two_linked_calls(Gs, UQVs, PId, LV, AL, VAs):
    %
    % is true iff the list of goals, Gs, with universally quantified
    % variables, UQVs, is two calls to the same predicate, PId, with
    % one variable that links them, LV.  AL will be the assoc list
    % that is the each variable from the first call with its
    % corresponding variable in the second call, and VAs are the
    % variables of the first call.
    %
:- pred process_two_linked_calls(hlds_goals::in, set(prog_var)::in,
    pred_id::out, prog_var::out, assoc_list(prog_var)::out, prog_vars::out)
    is semidet.

process_two_linked_calls(Goals, UniversiallyQuantifiedVars, PredId,
        LinkingVar, Vars, VarsA) :-
    Goals = [plain_call(PredId, _, VarsA, _, _, _) - _,
        plain_call(PredId, _, VarsB, _, _, _) - _],

        % Determine the linking variable, L.
        % By definition it must be existentially quantified and
        % a member of both variable lists.
    CommonVars = list_to_set(VarsA) `intersect` list_to_set(VarsB),
    set.singleton_set(CommonVars `difference` UniversiallyQuantifiedVars,
        LinkingVar),

        % Set up mapping between the variables in the two calls.
    assoc_list.from_corresponding_lists(VarsA, VarsB, Vars).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

is_construction_equivalence_assertion(Module, AssertId, ConsId, PredId) :-
    assert_id_goal(Module, AssertId, Goal),
    goal_is_equivalence(Goal, P, Q),
    ( single_construction(P, ConsId) ->
        predicate_call(Q, PredId)
    ;
        single_construction(Q, ConsId),
        predicate_call(P, PredId)
    ).

    % One side of the equivalence must be just the single
    % unification with the correct cons_id.
    %
:- pred single_construction(hlds_goal::in, cons_id::in) is semidet.

single_construction(unify(_, UnifyRhs, _, _, _) - _,
        cons(QualifiedSymName, Arity)) :-
    UnifyRhs = rhs_functor(cons(UnqualifiedSymName, Arity), _, _),
    match_sym_name(UnqualifiedSymName, QualifiedSymName).

    % The side containing the predicate call must be a single call
    % to the predicate with 0 or more construction unifications
    % which setup the arguments to the predicates.
    %
:- pred predicate_call(hlds_goal::in, pred_id::in) is semidet.

predicate_call(Goal, PredId) :-
    ( Goal = conj(plain_conj, Goals) - _ ->
        list.member(Call, Goals),
        Call = plain_call(PredId, _, _, _, _, _) - _,
        list.delete(Goals, Call, Unifications),
        P = (pred(G::in) is semidet :-
            not (
                G = unify(_, UnifyRhs, _, _, _) - _,
                UnifyRhs = rhs_functor(_, _, _)
            )
        ),
        list.filter(P, Unifications, [])
    ;
        Goal = plain_call(PredId, _, _, _, _, _) - _
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

assert_id_goal(Module, AssertId, Goal) :-
    module_info_get_assertion_table(Module, AssertTable),
    assertion_table_lookup(AssertTable, AssertId, PredId),
    module_info_pred_info(Module, PredId, PredInfo),
    pred_info_clauses_info(PredInfo, ClausesInfo),
    clauses_info_clauses_only(ClausesInfo, Clauses),
    ( Clauses = [clause(_ProcIds, Goal0, _Lang, _Context)] ->
        normalise_goal(Goal0, Goal)
    ;
        unexpected(this_file, "goal: not an assertion")
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred goal_is_implication(hlds_goal::in, hlds_goal::out, hlds_goal::out)
    is semidet.

goal_is_implication(Goal, P, Q) :-
        % Goal = (P => Q)
    Goal = negation(conj(plain_conj, GoalList) - GI) - _,
    list.reverse(GoalList) = [NotQ | Ps],
    ( Ps = [P0] ->
        P = P0
    ;
        P = conj(plain_conj, list.reverse(Ps)) - GI
    ),
    NotQ = negation(Q) - _.

:- pred goal_is_equivalence(hlds_goal::in, hlds_goal::out, hlds_goal::out)
    is semidet.

goal_is_equivalence(Goal, P, Q) :-
        % Goal = P <=> Q
    Goal = conj(plain_conj, [A, B]) - _GoalInfo,
    map.init(Subst),
    goal_is_implication(A, PA, QA),
    goal_is_implication(B, QB, PB),
    equal_goals(PA, PB, Subst, _),
    equal_goals(QA, QB, Subst, _),
    P = PA,
    Q = QA.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

    % equal_goals(GA, GB):
    %
    % Do these two goals represent the same hlds_goal modulo renaming?
    %
:- pred equal_goals(hlds_goal::in, hlds_goal::in,
    subst::in, subst::out) is semidet.

equal_goals(conj(ConjType, GoalAs) - _, conj(ConjType, GoalBs) - _, !Subst) :-
    equal_goals_list(GoalAs, GoalBs, !Subst).
equal_goals(plain_call(PredId, _, VarsA, _, _, _) - _,
        plain_call(PredId, _, VarsB, _, _, _) - _, !Subst) :-
    equal_vars(VarsA, VarsB, !Subst).
equal_goals(generic_call(Type, VarsA, _, _) - _,
        generic_call(Type, VarsB, _, _) - _, !Subst) :-
    equal_vars(VarsA, VarsB, !Subst).
equal_goals(switch(Var, CanFail, CasesA) - _,
        switch(Var, CanFail, CasesB) - _, !Subst) :-
    equal_goals_cases(CasesA, CasesB, !Subst).
equal_goals(unify(VarA, RHSA, _, _, _) - _, unify(VarB, RHSB, _, _, _) - _,
        !Subst) :-
    equal_vars([VarA], [VarB], !Subst),
    equal_unification(RHSA, RHSB, !Subst).
equal_goals(disj(GoalAs) - _, disj(GoalBs) - _, !Subst) :-
    equal_goals_list(GoalAs, GoalBs, !Subst).
equal_goals(negation(GoalA) - _, negation(GoalB) - _, !Subst) :-
    equal_goals(GoalA, GoalB, !Subst).
equal_goals(scope(ReasonA, GoalA) - _, scope(ReasonB, GoalB) - _, !Subst) :-
    equal_reason(ReasonA, ReasonB, !Subst),
    equal_goals(GoalA, GoalB, !Subst).
equal_goals(if_then_else(VarsA, IfA, ThenA, ElseA) - _,
        if_then_else(VarsB, IfB, ThenB, ElseB) - _, !Subst) :-
    equal_vars(VarsA, VarsB, !Subst),
    equal_goals(IfA, IfB, !Subst),
    equal_goals(ThenA, ThenB, !Subst),
    equal_goals(ElseA, ElseB, !Subst).
equal_goals(
        call_foreign_proc(Attribs, PredId, _, ArgsA, ExtraA, MaybeTraceA, _)
            - _,
        call_foreign_proc(Attribs, PredId, _, ArgsB, ExtraB, MaybeTraceB, _)
            - _,
        !Subst) :-
    % Foreign_procs with extra args and trace runtime conditions are compiler
    % generated, and as such will not participate in assertions.
    ExtraA = [],
    ExtraB = [],
    MaybeTraceA = no,
    MaybeTraceB = no,
    VarsA = list.map(foreign_arg_var, ArgsA),
    VarsB = list.map(foreign_arg_var, ArgsB),
    equal_vars(VarsA, VarsB, !Subst).
equal_goals(shorthand(ShorthandGoalA) - GoalInfoA,
        shorthand(ShorthandGoalB) - GoalInfoB, !Subst) :-
    equal_goals_shorthand(ShorthandGoalA - GoalInfoA,
        ShorthandGoalB - GoalInfoB, !Subst).

:- pred equal_reason(scope_reason::in, scope_reason::in, subst::in, subst::out)
    is semidet.

equal_reason(exist_quant(VarsA), exist_quant(VarsB), !Subst) :-
    equal_vars(VarsA, VarsB, !Subst).
equal_reason(barrier(Removable), barrier(Removable), !Subst).
equal_reason(commit(ForcePruning), commit(ForcePruning), !Subst).
equal_reason(from_ground_term(VarA), from_ground_term(VarB), !Subst) :-
    equal_var(VarA, VarB, !Subst).

:- pred equal_goals_shorthand(pair(shorthand_goal_expr, hlds_goal_info)::in,
    pair(shorthand_goal_expr, hlds_goal_info)::in, subst::in, subst::out)
    is semidet.

equal_goals_shorthand(bi_implication(LeftGoalA, RightGoalA) - GoalInfo,
        bi_implication(LeftGoalB, RightGoalB) - GoalInfo, !Subst) :-
    equal_goals(LeftGoalA, LeftGoalB, !Subst),
    equal_goals(RightGoalA, RightGoalB, !Subst).

:- pred equal_var(prog_var::in, prog_var::in, subst::in, subst::out)
    is semidet.

equal_var(VA, VB, !Subst) :-
    ( map.search(!.Subst, VA, SubstVA) ->
        SubstVA = VB
    ;
        map.insert(!.Subst, VA, VB, !:Subst)
    ).

:- pred equal_vars(prog_vars::in, prog_vars::in, subst::in, subst::out)
    is semidet.

equal_vars([], [], !Subst).
equal_vars([VA | VAs], [VB | VBs], !Subst) :-
    equal_var(VA, VB, !Subst),
    equal_vars(VAs, VBs, !Subst).

:- pred equal_unification(unify_rhs::in, unify_rhs::in, subst::in, subst::out)
    is semidet.

equal_unification(rhs_var(A), rhs_var(B), !Subst) :-
    equal_vars([A], [B], !Subst).
equal_unification(rhs_functor(ConsId, E, VarsA), rhs_functor(ConsId, E, VarsB),
        !Subst) :-
    equal_vars(VarsA, VarsB, !Subst).
equal_unification(LambdaGoalA, LambdaGoalB, !Subst) :-
    LambdaGoalA = rhs_lambda_goal(Purity, PredOrFunc, EvalMethod,
        NLVarsA, LVarsA, Modes, Det, GoalA),
    LambdaGoalB = rhs_lambda_goal(Purity, PredOrFunc, EvalMethod,
        NLVarsB, LVarsB, Modes, Det, GoalB),
    equal_vars(NLVarsA, NLVarsB, !Subst),
    equal_vars(LVarsA, LVarsB, !Subst),
    equal_goals(GoalA, GoalB, !Subst).

:- pred equal_goals_list(hlds_goals::in, hlds_goals::in, subst::in, subst::out)
    is semidet.

equal_goals_list([], [], !Subst).
equal_goals_list([GoalA | GoalAs], [GoalB | GoalBs], !Subst) :-
    equal_goals(GoalA, GoalB, !Subst),
    equal_goals_list(GoalAs, GoalBs, !Subst).

:- pred equal_goals_cases(list(case)::in, list(case)::in,
    subst::in, subst::out) is semidet.

equal_goals_cases([], [], !Subst).
equal_goals_cases([CaseA | CaseAs], [CaseB | CaseBs], !Subst) :-
    CaseA = case(ConsId, GoalA),
    CaseB = case(ConsId, GoalB),
    equal_goals(GoalA, GoalB, !Subst),
    equal_goals_cases(CaseAs, CaseBs, !Subst).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

record_preds_used_in(Goal, AssertId, !Module) :-
    % Explicit lambda expression needed since goal_calls_pred_id
    % has multiple modes.
    P = (pred(PredId::out) is nondet :- goal_calls_pred_id(Goal, PredId)),
    solutions.solutions(P, PredIds),
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
    set.insert(Assertions0, AssertId, Assertions),
    pred_info_set_assertions(Assertions, PredInfo0, PredInfo),
    module_info_set_pred_info(PredId, PredInfo, !Module).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

normalise_goal(Goal @ plain_call(_, _, _, _, _, _) - GI, Goal - GI).
normalise_goal(Goal @ generic_call(_, _, _, _) - GI, Goal - GI).
normalise_goal(Goal @ unify(_, _, _, _, _) - GI, Goal - GI).
normalise_goal(Goal @ call_foreign_proc(_, _, _, _, _, _, _) - GI, Goal - GI).
normalise_goal(conj(ConjType, Goals0) - GI, conj(ConjType, Goals) - GI) :-
    (
        ConjType = plain_conj,
        normalise_conj(Goals0, Goals)
    ;
        ConjType = parallel_conj,
        normalise_goals(Goals0, Goals)
    ).
normalise_goal(switch(A,B,Case0s) - GI, switch(A,B,Cases)-GI) :-
    normalise_cases(Case0s, Cases).
normalise_goal(disj(Goal0s) - GI, disj(Goals) - GI) :-
    normalise_goals(Goal0s, Goals).
normalise_goal(negation(Goal0) - GI, negation(Goal) - GI) :-
    normalise_goal(Goal0, Goal).
normalise_goal(scope(Reason, Goal0) - GI,
        scope(Reason, Goal) - GI) :-
    normalise_goal(Goal0, Goal).
normalise_goal(if_then_else(A, If0, Then0, Else0) - GI,
        if_then_else(A, If, Then, Else) - GI) :-
    normalise_goal(If0, If),
    normalise_goal(Then0, Then),
    normalise_goal(Else0, Else).
normalise_goal(shorthand(ShortHandGoal0) - GI0,
        shorthand(ShortHandGoal) - GI) :-
    normalise_goal_shorthand(ShortHandGoal0 - GI0, ShortHandGoal - GI).

    % Place a shorthand goal into a standard form. Currently
    % all the code does is replace conj([G]) with G.
    %
:- pred normalise_goal_shorthand(
    pair(shorthand_goal_expr, hlds_goal_info)::in,
    pair(shorthand_goal_expr, hlds_goal_info)::out) is det.

normalise_goal_shorthand(bi_implication(LHS0, RHS0) - GI,
        bi_implication(LHS, RHS) - GI) :-
    normalise_goal(LHS0, LHS),
    normalise_goal(RHS0, RHS).

%-----------------------------------------------------------------------------%

:- pred normalise_conj(hlds_goals::in, hlds_goals::out) is det.

normalise_conj([], []).
normalise_conj([Goal0 | Goal0s], Goals) :-
    goal_to_conj_list(Goal0, ConjGoals),
    normalise_conj(Goal0s, Goal1s),
    list.append(ConjGoals, Goal1s, Goals).

:- pred normalise_cases(list(case)::in, list(case)::out) is det.

normalise_cases([], []).
normalise_cases([Case0 | Case0s], [Case | Cases]) :-
    Case0 = case(ConsId, Goal0),
    normalise_goal(Goal0, Goal),
    Case = case(ConsId, Goal),
    normalise_cases(Case0s, Cases).

:- pred normalise_goals(hlds_goals::in, hlds_goals::out) is det.

normalise_goals([], []).
normalise_goals([Goal0 | Goal0s], [Goal | Goals]) :-
    normalise_goal(Goal0, Goal),
    normalise_goals(Goal0s, Goals).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "assertion.m".

%-----------------------------------------------------------------------------%
