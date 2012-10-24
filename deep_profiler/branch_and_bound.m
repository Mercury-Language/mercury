%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2010-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: branch_and_bound.m.
% Author: pbone.
%
% This module contains a generic branch and bound solver.  It is designed to be
% generic and easy to use.  To use it write non-deterministic search code that
% uses test_incomplete_solution after every choice point.  Call this code using
% branch_and_bound.
%
% This module may be compiled with the debug_branch_and_bound trace flag to
% enable the debugging trace goals.
%
%-----------------------------------------------------------------------------%

:- module branch_and_bound.
:- interface.

:- import_module set.

%-----------------------------------------------------------------------------%

    % The state of the branch and bound solver.  This structure contains
    % mutable that are not reverted during backtracking.  This is how the
    % solver maintains the current best value for the objective function.
    % This means that only one value of this type needs to be passed into the
    % search code.
    %
:- type bnb_state(T).

    % Profiling information for an execution of the solver.
    %
:- type bnb_profile
    --->    bnb_profile(
                bnbp_tests_succeeded        :: int,
                bnbp_tests_failed           :: int,
                bnbp_new_best_solution      :: int,
                bnbp_new_equal_solution     :: int,
                bnbp_not_best_solution      :: int,
                bnbp_open_branches          :: int,
                bnbp_closed_branches        :: int,
                bnbp_time_msecs             :: int
            ).

    % branch_and_bound(GenerateSolutions, ObjectiveFn, BestSolutions).
    %
    % Use a branch and bound search to return the set of BestSolutions
    % according to the ObjectiveFn that GenerateSolutions can generate.
    %
    % Note that more optimal solutions return _smaller_ values
    % from ObjectiveFn.
    %
    % The set of best solutions is returned, it is up to the caller to break
    % ties if necessary.
    %
:- pred branch_and_bound(impure pred(bnb_state(T), T),
    func(T) = float, set(T), bnb_profile).
:- mode branch_and_bound(pred(in, out) is nondet,
    func(in) = out is det, out, out) is det.

    % test_incomplete_solution(State, PartialSolution).
    %
    % This is true if PartialSolution is not worse than the current best
    % solution.  It does not update the best solution.  Programmers should use
    % this after every choice point to avoid searching non-optimistic parts of
    % their search tree.
    %
:- semipure pred test_incomplete_solution(bnb_state(T)::in, T::in) is semidet.

    % score_solution(State, Solution, Score).
    %
    % Score the complete or partial solution without comparing it to the best
    % solution.
    %
:- pred score_solution(bnb_state(T)::in, T::in, float::out) is det.

    % add_alternative(State).
    %
    % Record that a new alternative is being added to the search.
    %
:- impure pred add_alternative(bnb_state(T)::in) is det.

    % close_alternative(State).
    %
    % Note that a branch failed such that we're now executing it's alternative.
    %
:- impure pred close_alternative(bnb_state(T)::in) is det.

    % num_alternatives(State, Open, Closed).
    %
    % Retrive the number of alternative branches both opened and closed.
    %
:- semipure pred num_alternatives(bnb_state(T)::in, int::out, int::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module benchmarking.
:- import_module float.
:- import_module int.
:- import_module io.
:- import_module list.
:- import_module mutvar.
:- import_module pair.
:- import_module require.
:- import_module string.
:- import_module unit.

%-----------------------------------------------------------------------------%

    % The best solutions found so far and the value of the objective
    % function for these solutions.
    %
:- type best_solutions(T)
    --->    no_best_solutions
    ;       best_solutions(
                bs_solutions            :: list(T),
                bs_objective_value      :: float
                    % Note that the solver tries to minimise this value.  That
                    % is smaller numbers are more optimal.
            ).

%-----------------------------------------------------------------------------%

:- type bnb_state(T)
    --->    bnb_state(
                best_solutions_mutable      :: mutvar(best_solutions(T)),
                objective_function          :: func(T) = float,
                profile                     :: mutvar(bnb_profile)
            ).

:- inst bnb_state
    --->    bnb_state(ground, func(in) = out is det, ground).

%-----------------------------------------------------------------------------%

branch_and_bound(Generator, ObjectiveFn, BestSolutions, Profile) :-
    promise_equivalent_solutions [Time, BestSolutions, Profile0] (
        benchmark_det(branch_and_bound_2(Generator, ObjectiveFn), unit,
            (BestSolutions - Profile0), 1, Time)
    ),
    Profile = Profile0 ^ bnbp_time_msecs := Time.

:- pred branch_and_bound_2(impure pred(bnb_state(T), T),
    func(T) = float, unit,
    pair(set(T), bnb_profile)).
:- mode branch_and_bound_2(pred(in, out) is nondet,
    func(in) = out is det, in, out) is det.

branch_and_bound_2(Generator, ObjectiveFn, unit,
        FinalBestSolutions - FinalProfile) :-
    % Use a failure driven loop to implement a branch and bound solver.
    promise_pure (
        trace [compile_time(flag("debug_branch_and_bound")), io(!IO)] (
            io.write_string("D: Branch and bound loop starting\n", !IO),
            io.flush_output(!IO)
        ),
        impure new_mutvar(no_best_solutions, BestSolutionsMutvar),
        impure new_mutvar(new_bnb_profile, ProfileMutvar),
        State = bnb_state(BestSolutionsMutvar, ObjectiveFn, ProfileMutvar),
        (
            impure Generator(State, CurrentSolution),

            impure test_complete_solution(State, CurrentSolution),
            trace [compile_time(flag("debug_branch_and_bound")), io(!IO)] (
                CurrentObjective = ObjectiveFn(CurrentSolution),
                io.format(
                    "D: Branch and bound: Solution found with objective: %f\n",
                    [f(CurrentObjective)], !IO),
                io.flush_output(!IO)
            ),

            semidet_fail
        ->
            unexpected($module, "Failure driven loop must fail")
        ;
            true
        ),

        % Return results.
        impure get_mutvar(BestSolutionsMutvar, FinalBestSolutions0),
        impure get_mutvar(ProfileMutvar, FinalProfile),
        trace [compile_time(flag("debug_branch_and_bound")), io(!IO)] (
            io.write_string("D: Branch and bound loop finished\n", !IO),
            io.flush_output(!IO)
        ),
        (
            FinalBestSolutions0 = no_best_solutions,
            FinalBestSolutions = set.init
        ;
            FinalBestSolutions0 = best_solutions(Solutions, _),
            FinalBestSolutions = set.from_list(Solutions)
        )
    ).

%-----------------------------------------------------------------------------%

    % test_complete_solution(State, Solution).
    %
    % True of Solution is the best or equal best solution so far.
    %
    % The current best solutions are updated.
    %
:- impure pred test_complete_solution(bnb_state(T)::in, T::in) is semidet.

test_complete_solution(State, CurrentSolution) :-
    State = bnb_state(BestSolutionsMutvar, ObjectiveFn, ProfileMutvar),
    CurrentObjective = ObjectiveFn(CurrentSolution),

    impure get_mutvar(BestSolutionsMutvar, BestSolutions0),
    impure get_mutvar(ProfileMutvar, Profile0),
    (
        (
            BestSolutions0 = no_best_solutions,
            BestSolutions = best_solutions([CurrentSolution],
                CurrentObjective),
            profile_new_best_solution(Profile0, Profile)
        ;
            BestSolutions0 = best_solutions(Solutions, BestObjective),
            ( CurrentObjective < BestObjective ->
                BestSolutions = best_solutions([CurrentSolution],
                    CurrentObjective),
                profile_new_best_solution(Profile0, Profile)
            ; CurrentObjective = BestObjective ->
                BestSolutions = best_solutions(
                    [CurrentSolution | Solutions], BestObjective),
                profile_equal_best_solution(Profile0, Profile)
            ;
                fail
            )
        )
    ->
        % If this solution is best or equal best.
        impure set_mutvar(BestSolutionsMutvar, BestSolutions),
        impure set_mutvar(ProfileMutvar, Profile)
    ;
        % If this solution is not better.
        profile_not_best_solution(Profile0, Profile),
        impure set_mutvar(ProfileMutvar, Profile),
        semidet_fail
    ).

%-----------------------------------------------------------------------------%

test_incomplete_solution(State, Solution) :-
    State = bnb_state(BestSolutionsMutvar, ObjectiveFn, ProfileMutvar),
    promise_semipure (
        impure get_mutvar(BestSolutionsMutvar, BestSolutions),
        impure get_mutvar(ProfileMutvar, Profile0),
        (
            (
                BestSolutions = no_best_solutions
            ;
                BestSolutions = best_solutions(_, BestObjective),
                CurrentObjective = ObjectiveFn(Solution),
                CurrentObjective =< BestObjective
            )
        ->
            profile_test_succeeded(Profile0, Profile),
            impure set_mutvar(ProfileMutvar, Profile)
        ;
            profile_test_failed(Profile0, Profile),
            impure set_mutvar(ProfileMutvar, Profile),
            semidet_fail
        )
    ).

%-----------------------------------------------------------------------------%

score_solution(State, Solution, Score) :-
    ObjectiveFn = State ^ objective_function,
    Score = ObjectiveFn(Solution).

%----------------------------------------------------------------------------%

add_alternative(State) :-
    ProfileMutvar = State ^ profile,
    impure get_mutvar(ProfileMutvar, Profile0),
    profile_add_alternative(Profile0, Profile),
    impure set_mutvar(ProfileMutvar, Profile).

close_alternative(State) :-
    ProfileMutvar = State ^ profile,
    impure get_mutvar(ProfileMutvar, Profile0),
    profile_close_alternative(Profile0, Profile),
    impure set_mutvar(ProfileMutvar, Profile).

num_alternatives(State, Open, Closed) :-
    ProfileMutvar = State ^ profile,
    promise_semipure (
        impure get_mutvar(ProfileMutvar, Profile)
    ),
    profile_num_alternatives(Profile, Open, Closed).

%-----------------------------------------------------------------------------%

:- func new_bnb_profile = bnb_profile.

new_bnb_profile = bnb_profile(0, 0, 0, 0, 0, 1, 0, 0).

:- pred profile_new_best_solution(bnb_profile::in, bnb_profile::out) is det.

profile_new_best_solution(!Profile) :-
    !Profile ^ bnbp_new_best_solution :=
        !.Profile ^ bnbp_new_best_solution + 1.

:- pred profile_equal_best_solution(bnb_profile::in, bnb_profile::out) is det.

profile_equal_best_solution(!Profile) :-
    !Profile ^ bnbp_new_equal_solution :=
        !.Profile ^ bnbp_new_equal_solution + 1.

:- pred profile_not_best_solution(bnb_profile::in, bnb_profile::out) is det.

profile_not_best_solution(!Profile) :-
    !Profile ^ bnbp_not_best_solution :=
        !.Profile ^ bnbp_not_best_solution + 1.

:- pred profile_test_succeeded(bnb_profile::in, bnb_profile::out) is det.

profile_test_succeeded(!Profile) :-
    !Profile ^ bnbp_tests_succeeded :=
        !.Profile ^ bnbp_tests_succeeded + 1.

:- pred profile_test_failed(bnb_profile::in, bnb_profile::out) is det.

profile_test_failed(!Profile) :-
    !Profile ^ bnbp_tests_failed :=
        !.Profile ^ bnbp_tests_failed + 1.

:- pred profile_add_alternative(bnb_profile::in, bnb_profile::out) is det.

profile_add_alternative(!Profile) :-
    !Profile ^ bnbp_open_branches :=
        !.Profile ^ bnbp_open_branches + 1.

:- pred profile_close_alternative(bnb_profile::in, bnb_profile::out) is det.

profile_close_alternative(!Profile) :-
    !Profile ^ bnbp_closed_branches :=
        !.Profile ^ bnbp_closed_branches + 1.

:- pred profile_num_alternatives(bnb_profile::in, int::out, int::out) is det.

profile_num_alternatives(Profile, Open, Closed) :-
    Open = Profile ^ bnbp_open_branches,
    Closed = Profile ^ bnbp_closed_branches.

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "branch_and_bound.m".

%-----------------------------------------------------------------------------%
:- end_module branch_and_bound.
%-----------------------------------------------------------------------------%
