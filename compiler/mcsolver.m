%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2004-2007, 2009-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: mcsolver.m.
% Main authors: rafe, richardf.
%
% Original author:
% Ralph Becket <rafe@cs.mu.oz.au>
% Fri Dec 31 14:45:18 EST 2004
%
% A constraint solver targeted specifically at David Overton's
% constraint-based mode analysis.
%
%-----------------------------------------------------------------------------%

:- module check_hlds.mcsolver.

:- interface.

:- import_module check_hlds.abstract_mode_constraints.

:- import_module bool.
:- import_module list.
:- import_module pair.

    % Convenient abbreviations.
:- type mcvar == abstract_mode_constraints.mc_var.
:- type mcvars == list(mcvar).

    % Structure in which to collect constraints.
    %
:- type prep_cstrts.

    % Structure on which the solver operates.
    %
:- type solver_cstrts.

    % We start by collecting our constraints together in a prep_cstrts
    % structure, before preparing them for the solver.
    %
:- func new_prep_cstrts = prep_cstrts.

    % Prepares the constraints described in abstract_mode_constraints.m
    % appropriately.
    %
:- pred prepare_abstract_constraints(list(mc_constraint)::in,
    prep_cstrts::in, prep_cstrts::out) is det.

    % NOTE: where possible, prepare_abstract_constraints/3 should be used
    % rather than this predicate.
    %
:- pred equivalent(mcvar::in, mcvar::in, prep_cstrts::in, prep_cstrts::out)
    is det.

    % NOTE: where possible, prepare_abstract_constraints/3 should be used
    % rather than this predicate.
    %
:- pred equivalent(list(mcvar)::in, prep_cstrts::in, prep_cstrts::out) is det.

    % NOTE: where possible, prepare_abstract_constraints/3 should be used
    % rather than this predicate.
    %
:- pred implies(mcvar::in, mcvar::in, prep_cstrts::in, prep_cstrts::out)
    is det.

    % NOTE: where possible, prepare_abstract_constraints/3 should be used
    % rather than this predicate.
    %
:- pred not_both(mcvar::in, mcvar::in, prep_cstrts::in, prep_cstrts::out)
    is det.

    % NOTE: where possible, prepare_abstract_constraints/3 should be used
    % rather than this predicate.
    %
:- pred different(mcvar::in, mcvar::in, prep_cstrts::in, prep_cstrts::out)
    is det.

    % NOTE: where possible, prepare_abstract_constraints/3 should be used
    % rather than this predicate.
    %
:- pred assign(mcvar::in, bool::in, prep_cstrts::in, prep_cstrts::out) is det.

    % NOTE: where possible, prepare_abstract_constraints/3 should be used
    % rather than this predicate.
    %
:- pred equivalent_to_disjunction(mcvar::in, mcvars::in,
    prep_cstrts::in, prep_cstrts::out) is det.

    % NOTE: where possible, prepare_abstract_constraints/3 should be used
    % rather than this predicate.
    %
:- pred at_most_one(mcvars::in, prep_cstrts::in, prep_cstrts::out) is det.

    % NOTE: where possible, prepare_abstract_constraints/3 should be used
    % rather than this predicate.
    %
:- pred exactly_one(mcvars::in, prep_cstrts::in, prep_cstrts::out) is det.

    % NOTE: where possible, prepare_abstract_constraints/3 should be used
    % rather than this predicate.
    %
:- pred disjunction_of_assignments(list(list(pair(mcvar, bool)))::in,
    prep_cstrts::in, prep_cstrts::out) is det.

    % Convert the set of constraints for use by the solver.
    %
:- func make_solver_cstrts(prep_cstrts) = solver_cstrts.

    % Nondeterministically enumerate solutions to the constraints.
    %
:- pred solve(solver_cstrts::in, mc_bindings::out) is nondet.

    % For debugging purposes only.
% :- pred main(io :: di, io :: uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module eqvclass.
:- import_module io.
:- import_module map.
:- import_module multi_map.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term.

%-----------------------------------------------------------------------------%

    % Assignment constraints.
    %
:- type assgt
    --->    mcvar `assign` bool.
:- type assgts == list(assgt).

    % Binary implication constraints. (Not a logical implication,
    % just a unidirectional propagation path).
    %
:- type impl
    --->    assgt `implies` assgt.
:- type impls == list(impl).

    % Complex constraints.
    %
:- type complex_cstrt
    --->    eqv_disj(mcvar, mcvars)
            % var <-> OR(vars)
    ;       at_most_one(mcvars)
    ;       exactly_one(mcvars)
    ;       disj_of_assgts(list(assgts)).
            % Each element of the list is a list of assignments
            % which should occur concurrently.

:- type complex_cstrts == list(complex_cstrt).

    % Map from a variable to the complex constraints it participates in.
    %
:- type complex_cstrt_map == multi_map(mcvar, complex_cstrt).

    % A propagation graph is an optimised representation of a set of
    % binary implication constraints. It consists of a pair of mappings
    % from vars to consequent assignments, where prop_graph_yes field
    % is the mapping when the var in question is bound to `yes', and the
    % prop_graph_no field is the mapping when the var in question
    % is bound to `no'.
    %
:- type prop_graph
    --->    prop_graph(
                prop_graph_yes  :: multi_map(mcvar, assgt),
                prop_graph_no   :: multi_map(mcvar, assgt)
            ).

    % We keep track of variables known to be equivalent in order
    % to reduce the number of variables we have to worry about
    % when solving the constraints.
    %
:- type eqv_vars == eqvclass(mcvar).

    % This type is just used to collect constraints before
    % we prepare them for use by the solver.
    %
:- type prep_cstrts
    --->    prep_cstrts(
                prep_eqv_vars       :: eqv_vars,
                prep_assgts         :: assgts,
                prep_impls          :: impls,
                prep_complex_cstrts :: complex_cstrts
            ).

    % This type just holds the various constraints passed to
    % the solver.  We separate constraints into four classes:
    %
    % - necessary equivalences are handled by renaming vars in
    %   an equivalence class to a single member of the equivalence
    %   class;
    % - necessary assignments are dealt with after equivalence
    %   renaming;
    % - the prop_graph represents the graph of binary implications,
    %   allowing propagation from assignments;
    % - complex constraints, such as eqv_disj and at_most_one, which
    %   are attached to each variable in the complex constraint and
    %   tested after propagation through the prop_graph.  Where
    %   possible, binary implications that follow from complex
    %   constraints are added to the prop_graph in order to simplify
    %   testing the complex constraints.
    %
    %   TODO:
    %
    % - We should consider adding backjumping if performance is an issue.
    %
:- type solver_cstrts
    --->    solver_cstrts(
                vars                :: mcvars,
                eqv_vars            :: eqv_vars,
                assgts              :: assgts,
                prop_graph          :: prop_graph,
                complex_cstrt_map   :: complex_cstrt_map
            ).

%-----------------------------------------------------------------------------%

new_prep_cstrts = prep_cstrts(eqvclass.init, [], [], []).

%-----------------------------------------------------------------------------%

    % Prepares the constraints described in abstract_mode_constraints.m
    % appropriately.
    %
prepare_abstract_constraints(Constraints, !PCs) :-
    list.foldl(prepare_abstract_constraint, Constraints, !PCs).

    % Prepares a constraint (as described in abstract_mode_constraints.m)
    % appropriately.
    %
:- pred prepare_abstract_constraint(mc_constraint::in,
    prep_cstrts::in, prep_cstrts::out) is det.

prepare_abstract_constraint(Constraint, !PCs) :-
    (
        Constraint = mc_atomic(VarConstraint),
        prepare_var_constraint(VarConstraint, !PCs)
    ;
        Constraint = mc_conj(Constraints),
        prepare_abstract_constraints(Constraints, !PCs)
    ;
        Constraint = mc_disj(Constraints),
        ( if
            % Build var - bool pairs assuming structure
            % mc_disj(mc_conj(assgts), mc_conj(assgts), ...), otherwise fail.
            list.map(
                ( pred(mc_conj(Fls)::in, VarValPairs::out) is semidet :-
                    list.map((pred(mc_atomic(equiv_bool(Var, Val))::in,
                        (Var - Val)::out) is semidet), Fls, VarValPairs)
                ),
                Constraints, DisjOfAssgts)
        then
            disjunction_of_assignments(DisjOfAssgts, !PCs)
        else
            sorry($pred, "Disjuction of constraints - general case.")
        )
    ).

    % Prepares an atomic constraint (as described in
    % abstract_mode_constraints.m) appropriately.
    %
:- pred prepare_var_constraint(var_constraint::in,
    prep_cstrts::in, prep_cstrts::out) is det.

prepare_var_constraint(equiv_bool(Var, Value), !PCs) :-
    assign(Var, Value, !PCs).
prepare_var_constraint(equivalent(Vars), !PCs) :-
    equivalent(Vars, !PCs).
prepare_var_constraint(implies(Var1, Var2), !PCs) :-
    implies(Var1, Var2, !PCs).
prepare_var_constraint(equiv_disj(X, Ys), !PCs) :-
    equivalent_to_disjunction(X, Ys, !PCs).
prepare_var_constraint(at_most_one(Vars), !PCs) :-
    at_most_one(Vars, !PCs).
prepare_var_constraint(exactly_one(Vars), !PCs) :-
    exactly_one(Vars, !PCs).

%-----------------------------------------------------------------------------%

equivalent(X, Y, !PCs) :-
    !PCs ^ prep_eqv_vars := ensure_equivalence(!.PCs ^ prep_eqv_vars, X, Y).

%-----------------------------------------------------------------------------%

equivalent([], !PCs).
equivalent([X | Xs], !PCs) :-
    list.foldl(equivalent(X), Xs, !PCs).

%-----------------------------------------------------------------------------%

implies(X, Y, !PCs) :-
    !PCs ^ prep_impls := [ (X `assign` yes) `implies` (Y `assign` yes),
                                 (Y `assign` no)  `implies` (X `assign` no)
                         | !.PCs ^ prep_impls ].

%-----------------------------------------------------------------------------%

not_both(X, Y, !PCs) :-
    !PCs ^ prep_impls := [ (X `assign` yes) `implies` (Y `assign` no),
                                 (Y `assign` yes) `implies` (X `assign` no)
                         | !.PCs ^ prep_impls ].

%-----------------------------------------------------------------------------%

different(X, Y, !PCs) :-
    !PCs ^ prep_impls := [ (X `assign` yes) `implies` (Y `assign` no),
                                 (X `assign` no)  `implies` (Y `assign` yes),
                                 (Y `assign` yes) `implies` (X `assign` no),
                                 (Y `assign` no)  `implies` (X `assign` yes)
                         | !.PCs ^ prep_impls ].

%-----------------------------------------------------------------------------%

assign(X, V, !PCs) :-
    !PCs ^ prep_assgts := [X `assign` V | !.PCs ^ prep_assgts].

%-----------------------------------------------------------------------------%

equivalent_to_disjunction(X, Ys, !PCs) :-
    (
        Ys = [],
        assign(X, no, !PCs)
    ;
        Ys = [Y],
        equivalent(X, Y, !PCs)
    ;
        Ys = [_, _ | _],
        !PCs ^ prep_complex_cstrts :=
            [eqv_disj(X, Ys) | !.PCs ^ prep_complex_cstrts]
    ).

%-----------------------------------------------------------------------------%

at_most_one(Xs, !PCs) :-
    (
        Xs = []
    ;
        Xs = [_]
    ;
        Xs = [X, Y],
        not_both(X, Y, !PCs)
    ;
        Xs = [_, _, _ | _],
        !PCs ^ prep_complex_cstrts :=
            [at_most_one(Xs) | !.PCs ^ prep_complex_cstrts]
    ).

%-----------------------------------------------------------------------------%

exactly_one(Xs, !PCs) :-
    (
        Xs = [],
        unexpected($pred, "exactly_one of zero variables")
    ;
        Xs = [X],
        assign(X, yes, !PCs)
    ;
        Xs = [_, _ | _],
        !PCs ^ prep_complex_cstrts :=
            [exactly_one(Xs) | !.PCs ^ prep_complex_cstrts]
    ).

%-----------------------------------------------------------------------------%

disjunction_of_assignments(DisjOfAssgts, !PCs) :-
    Assgtss =
        list.map(list.map(func((Var - Value)) = Var `assign` Value),
            DisjOfAssgts),
    !PCs ^ prep_complex_cstrts :=
        [disj_of_assgts(Assgtss) | !.PCs ^ prep_complex_cstrts].

%-----------------------------------------------------------------------------%

make_solver_cstrts(PCs) = SCs:-
    % Simplify away equivalences.
    Eqvs = PCs ^ prep_eqv_vars,

    Assgts =
        list.map(
            func(X `assign` V) = (eqv_var(Eqvs, X) `assign` V),
            PCs ^ prep_assgts
        ),

    Impls =
        list.map(
            func((X `assign` VX) `implies` (Y `assign` VY)) =
                ((eqv_var(Eqvs, X) `assign` VX) `implies`
                    (eqv_var(Eqvs, Y) `assign` VY)),
            PCs ^ prep_impls
        ),

    ComplexCstrts =
        list.map(eqv_complex_cstrt(Eqvs), PCs ^ prep_complex_cstrts),

    % Construct the propagation graph.
    PropGraph =
        list.foldl(
            func(((X `assign` VX) `implies` (Y `assign` VY)),
                    prop_graph(YesPG, NoPG)) =
                ( if VX = yes then
                    prop_graph(set(YesPG, X, (Y `assign` VY)), NoPG)
                else
                    prop_graph(YesPG, set(NoPG, X, (Y `assign` VY)))
                ),
            Impls,
            prop_graph(multi_map.init, multi_map.init)
        ),

    % Construct the complex constraints map.
    ComplexCstrtsMap =
        list.foldl(
            func(ComplexCstrt, CCM) =
                foldl(
                    func(Z, CCMa) = multi_map.set(CCMa, Z, ComplexCstrt),
                    complex_cstrt_vars(ComplexCstrt),
                    CCM
                ),
            ComplexCstrts,
            multi_map.init
        ),

    % Find the set of variables we have to solve for.
    AssgtVars =
        list.foldl(
            func((X `assign` _V), Vars) = [X | Vars],
            Assgts,
            []
        ),

    AndImplVars =
        list.foldl(
            func((X `assign` _VX) `implies` (Y `assign` _VY), Vars) =
                [X, Y | Vars],
            Impls,
            AssgtVars
        ),

    AndComplexCstrtVars =
        list.foldl(
            func(ComplexCstrt, Vars) =
                complex_cstrt_vars(ComplexCstrt) ++ Vars,
            ComplexCstrts,
            AndImplVars
        ),

    AllVars = sort_and_remove_dups(AndComplexCstrtVars),

    % Make the solver constraints record.
    SCs = solver_cstrts(AllVars, Eqvs, Assgts, PropGraph, ComplexCstrtsMap).

    % eqv_var(Eqvs, Var) returns a representative member of all the
    % variables equivalent to Var (in Eqvs)
    %
:- func eqv_var(eqv_vars, mcvar) = mcvar.

eqv_var(Eqvs, X) = eqvclass.get_minimum_element(Eqvs, X).

    % eqv_vars(Eqvs, Vars) just maps eqv_var to each Var in Vars.
    %
:- func eqv_vars(eqv_vars, mcvars) = mcvars.

eqv_vars(Eqvs, Xs) = list.map(eqv_var(Eqvs), Xs).

    % Returns all the variables that participate in this constraint.
    %
:- func complex_cstrt_vars(complex_cstrt) = mcvars.

complex_cstrt_vars(eqv_disj(X, Ys)) = [X | Ys].
complex_cstrt_vars(at_most_one(Xs)) = Xs.
complex_cstrt_vars(exactly_one(Xs)) = Xs.
complex_cstrt_vars(disj_of_assgts(Assgtss)) =
    list.foldl(list.foldl(func((V `assign` _), Vs) = [V | Vs]), Assgtss, []).

    % Replaces all the variables in the supplied constraint with
    % a representative variable from those constrained to be equivalent
    % to the original.
    %
:- func eqv_complex_cstrt(eqv_vars, complex_cstrt) = complex_cstrt.

eqv_complex_cstrt(Eqvs, eqv_disj(X, Ys)) =
    eqv_disj(eqv_var(Eqvs, X), eqv_vars(Eqvs, Ys)).

eqv_complex_cstrt(Eqvs, at_most_one(Xs)) =
    at_most_one(eqv_vars(Eqvs, Xs)).

eqv_complex_cstrt(Eqvs, exactly_one(Xs)) =
    exactly_one(eqv_vars(Eqvs, Xs)).

eqv_complex_cstrt(Eqvs, disj_of_assgts(Assgtss)) =
    disj_of_assgts(list.map(
        list.map(func((Var `assign` Val)) = (eqv_var(Eqvs, Var) `assign` Val)),
        Assgtss
    )).

%-----------------------------------------------------------------------------%

    % Succeeds if Bindings satisfies the constraints SCs.
    %
solve(SCs, Bindings) :-
    solve(SCs, map.init, Bindings0),
    bind_equivalent_vars(SCs, Bindings0, Bindings),
    trace [compiletime(flag("debug_mcsolver")), io(!IO)] (
        io.nl(!IO)
    ).

    % solve(SCs, Bs0, Bs) succeeds if Bs satisfies the constraints SCs,
    % given that Bs0 is known to not conflict with any of the constraints
    % in SCs.
    %
:- pred solve(solver_cstrts::in, mc_bindings::in, mc_bindings::out) is nondet.

solve(SCs, Bs0, Bs) :-
    solve_assgts(SCs, SCs ^ assgts, Bs0, Bs1),
    solve_vars(SCs, SCs ^ vars, Bs1, Bs).

    % Propagates the binding for every variable that has been solved for
    % to every variable it is equivalent to.
    %
:- pred bind_equivalent_vars(solver_cstrts::in,
    mc_bindings::in, mc_bindings::out) is det.

bind_equivalent_vars(SCs, !Bindings) :-
    Equivalences = SCs ^ eqv_vars,
    list.foldl(
        ( pred(Var::in, Binds0::in, Binds::out) is det :-
            EquivVars = set.to_sorted_list(get_equivalent_elements(
                Equivalences, Var)),
            map.lookup(Binds0, Var, Val),
            bind_all(EquivVars, Val, Binds0, Binds)
        ),
        map.keys(!.Bindings), !Bindings).

    % bind_all(Vars, Val, !Bindings) Binds Var to Val in Bindings for
    % every Var in Vars.
    %
:- pred bind_all(mcvars::in, bool::in, mc_bindings::in, mc_bindings::out)
    is det.

bind_all(Vars, Val, !Bindings) :-
    list.foldl(
        ( pred(Var::in, Binds0::in, Binds::out) is det :-
            map.set(Var, Val, Binds0, Binds)
        ),
        Vars, !Bindings).

%-----------------------------------------------------------------------------%

    % solve_assgts(SCs, Assgts, Bs0, Bs) attempts to perform each variable
    % binding in Assgts, propagating the results when called for.
    %
:- pred solve_assgts(solver_cstrts::in, assgts::in,
    mc_bindings::in, mc_bindings::out) is semidet.

solve_assgts(SCs, Assgts, Bs0, Bs) :-
    list.foldl(solve_assgt(SCs), Assgts, Bs0, Bs).

    % solve_assgt(SCs, (X `assign` V), Bs0, Bs) attempts to bind variable X
    % to value V. It propagates the results if it succeeds.
    %
:- pred solve_assgt(solver_cstrts::in, assgt::in,
    mc_bindings::in, mc_bindings::out) is semidet.

solve_assgt(SCs, (X `assign` V), Bs0, Bs) :-
    % XXX
    ( if Bs0 ^ elem(X) = V0 then
        ( if V = V0  then
            true
        else
            trace [compiletime(flag("debug_mcsolver")), io(!IO)] (
                io.write_string(mc_var_to_string(X) ++ " conflict", !IO)
            )
        ),
        V  = V0,
        Bs = Bs0
    else
        trace [compiletime(flag("debug_mcsolver")), io(!IO)] (
            io.write_string(".", !IO)
        ),
        % XXX
        Bs1 = Bs0 ^ elem(X) := V,

        Assgts = var_consequents(SCs ^ prop_graph, X, V),
        solve_assgts(SCs, Assgts, Bs1, Bs2),

        ComplexCstrts = var_complex_cstrts(SCs ^ complex_cstrt_map, X),
        solve_complex_cstrts(SCs, X, V, ComplexCstrts, Bs2, Bs)
    ).

%-----------------------------------------------------------------------------%

    % solve_complex_cstrts(SCs, X, V, ComplexCstrts, Bs0, Bs) succeeds
    % if the binding (X `assign` V) (which should already have been added
    % to Bs0) is consistant with the complex constraints variable X
    % participates in SCs. It also propagates results where appropriate.
    %
:- pred solve_complex_cstrts(solver_cstrts::in, mcvar::in, bool::in,
    complex_cstrts::in, mc_bindings::in, mc_bindings::out) is semidet.

solve_complex_cstrts(SCs, X, V, ComplexCstrts, Bs0, Bs) :-
    list.foldl(solve_complex_cstrt(SCs, X, V), ComplexCstrts, Bs0, Bs).

    % solve_complex_cstrt(SCs, X, V, ComplexCstrt, Bs0, Bs) succeeds
    % if the binding (X `assign` V) (which should already have been added
    % to Bs0) is consistant with ComplexCstrt (in which X should participate
    % in SCs). It also propagates results where appropriate.
    %
:- pred solve_complex_cstrt(solver_cstrts::in, mcvar::in, bool::in,
    complex_cstrt::in, mc_bindings::in, mc_bindings::out) is semidet.

solve_complex_cstrt(SCs, X, V, eqv_disj(Y, Zs), Bs0, Bs) :-
    ( if X = Y then
        (
            V  = yes,
            trace [compiletime(flag("debug_mcsolver")), io(!IO)] (
                io.write_string("1<", !IO)
            ),
            not all_no(Bs0, Zs),
            Bs = Bs0
        ;
            V  = no,
            trace [compiletime(flag("debug_mcsolver")), io(!IO)] (
                io.write_string("0<", !IO)
            ),
            solve_assgts(SCs, list.map(func(Z) = Z `assign` no, Zs), Bs0, Bs)
        )
     else
        % X in Zs
        (
            V = yes,
            trace [compiletime(flag("debug_mcsolver")), io(!IO)] (
                io.write_string(">1", !IO)
            ),
            solve_assgt(SCs, Y `assign` yes, Bs0, Bs)
        ;
            V = no,
            trace [compiletime(flag("debug_mcsolver")), io(!IO)] (
                io.write_string(">0", !IO)
            ),
            ( if all_no(Bs0, Zs) then
                solve_assgt(SCs, Y `assign` no, Bs0, Bs)
            else
                Bs = Bs0
            )
        )
    ).

solve_complex_cstrt(SCs, X, V, at_most_one(Ys0), Bs0, Bs) :-
    (
        V  = no,
        Bs = Bs0
    ;
        V  = yes,
        list.delete_first(Ys0, X, Ys),
        solve_assgts(SCs, list.map(func(Y) = Y `assign` no, Ys), Bs0, Bs)
    ).

solve_complex_cstrt(SCs, X, V, exactly_one(Ys0), Bs0, Bs) :-
    (
        V  = no,
        % A variable in Ys0 uniquely not bound to 'no' is bound to yes.
        % Fails if all Ys0 are 'no'.
        Ys = list.filter(
            (pred(Y0::in) is semidet :- not map.search(Bs0, Y0, no)), Ys0),
        (
            Ys = [Y],
            solve_assgts(SCs, [Y `assign` yes], Bs0, Bs)
        ;
            Ys = [_, _ | _],
            Bs = Bs0
        )
    ;
        V  = yes,
        list.delete_first(Ys0, X, Ys),
        solve_assgts(SCs, list.map(func(Y) = Y `assign` no, Ys), Bs0, Bs)
    ).

solve_complex_cstrt(SCs, X, V, disj_of_assgts(Assgtss), Bs0, Bs) :-
    % Filter for the assignments compatible with binding X to V.
    list.filter(
        (pred(Assgts::in) is semidet :-
            list.member(X `assign` bool.not(V), Assgts)
        ),
        Assgtss, _Conflicts, NotConflicting),

    % If only one set of assignments is possible now, make them.
    % If none are possible, fail.

    (
        NotConflicting = [Assignments],
        solve_assgts(SCs, Assignments, Bs0, Bs)
    ;
        NotConflicting = [_, _ | _],
        Bs = Bs0
    ).

%-----------------------------------------------------------------------------%

    % var_consequents(PropGraph, X, V) returns the assignments
    % directly entailed by biding X to V.
    %
:- func var_consequents(prop_graph, mcvar, bool) = assgts.

var_consequents(prop_graph(YesPG, _NoPG), X, yes) =
    ( if YesPG ^ elem(X) = Assgts then
        Assgts
    else
        []
    ).

var_consequents(prop_graph(_YesPG, NoPG), X, no) =
    ( if NoPG  ^ elem(X) = Assgts then
        Assgts
    else
        []
    ).

%-----------------------------------------------------------------------------%

    % var_complex_cstrts(ComplexCstrtMap, X) returns the complex constraints,
    % if any, that variable X participates in.
    %
:- func var_complex_cstrts(complex_cstrt_map, mcvar) = complex_cstrts.

var_complex_cstrts(ComplexCstrtMap, X) =
    ( if ComplexCstrtMap ^ elem(X) = CmplxCstrts then
        CmplxCstrts
    else
        []
    ).

%-----------------------------------------------------------------------------%

    % solve_vars(SCs, Vars, Bs0, Bs) non-deterministically assigns a value
    % to each of Vars and propagates results, looking for a solution to SCs.
    %
:- pred solve_vars(solver_cstrts::in, mcvars::in,
    mc_bindings::in, mc_bindings::out) is nondet.

solve_vars(SCs, Vars, Bs0, Bs) :-
    list.foldl(solve_var(SCs), Vars, Bs0, Bs).

:- pred solve_var(solver_cstrts::in, mcvar::in,
    mc_bindings::in, mc_bindings::out) is nondet.

solve_var(SCs, X, Bs0, Bs) :-
    ( if map.contains(Bs0, X) then
        Bs = Bs0
    else
        ( V = yes ; V = no ),
        trace [compiletime(flag("debug_mcsolver")), io(!IO)] (
            write_string("\n" ++ mc_var_to_string(X) ++ " = ", !IO),
            print(V, !IO)
        ),
        solve_assgt(SCs, X `assign` V, Bs0, Bs)
    ).

%-----------------------------------------------------------------------------%

    % all_no(Bs, Xs) succeeds if Bs indicates all Xs are bound to no.
    %
:- pred all_no(mc_bindings::in, mcvars::in) is semidet.

all_no(_,  []).
all_no(Bs, [X | Xs]) :-
    % XXX
    Bs ^ elem(X) = no,
    all_no(Bs, Xs).

%-----------------------------------------------------------------------------%

% main(!IO) :-
%
%     NameBindingss = solutions(solve(append_simple)),
%
%     io.nl(!IO),
%     io.write_list(NameBindingss, "\n\n", io.print, !IO),
%     io.nl(!IO).

:- func mc_var_to_string(mc_var) = string.

mc_var_to_string(MCVar) = int_to_string(var_to_int(MCVar)).

%-----------------------------------------------------------------------------%
:- end_module check_hlds.mcsolver.
%-----------------------------------------------------------------------------%
