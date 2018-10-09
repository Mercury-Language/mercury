%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This module contains types and predicates for working with determinism
% information.
%
%---------------------------------------------------------------------------%

:- module parse_tree.prog_detism.
:- interface.

:- import_module parse_tree.prog_data.

:- import_module maybe.

%---------------------------------------------------------------------------%

    % The following predicates implement the tables for computing the
    % determinism of compound goals from the determinism of their components.

:- pred det_conjunction_detism(determinism::in, determinism::in,
    determinism::out) is det.

:- pred det_par_conjunction_detism(determinism::in, determinism::in,
    determinism::out) is det.

:- pred det_switch_detism(determinism::in, determinism::in, determinism::out)
    is det.

:- pred det_negation_det(determinism::in, maybe(determinism)::out) is det.

    % The following predicates do abstract interpretation to count
    % the number of solutions and the possible number of failures.
    %
    % If the num_solns is at_most_many_cc, this means that the goal might have
    % many logical solutions if there were no pruning, but that the goal occurs
    % in a single-solution context, so only the first solution will be
    % returned.
    %
    % The reason why we don't throw an exception in det_switch_maxsoln and
    % det_disjunction_maxsoln is given in the documentation of the test case
    % invalid/magicbox.m.

:- pred det_conjunction_maxsoln(soln_count::in, soln_count::in,
    soln_count::out) is det.

:- pred det_conjunction_canfail(can_fail::in, can_fail::in, can_fail::out)
    is det.

:- pred det_disjunction_maxsoln(soln_count::in, soln_count::in,
    soln_count::out) is det.

:- pred det_disjunction_canfail(can_fail::in, can_fail::in, can_fail::out)
    is det.

:- pred det_switch_maxsoln(soln_count::in, soln_count::in, soln_count::out)
    is det.

:- pred det_switch_canfail(can_fail::in, can_fail::in, can_fail::out) is det.

%---------------------------------------------------------------------------%

:- type det_comparison
    --->    first_detism_tighter_than
            % The first determinism promises strictly more than the second.

    ;       first_detism_same_as
            % The first determinism promises exactly as much as the second.

    ;       first_detism_looser_than
            % The first determinism promises strictly less than the second.

    ;       first_detism_incomparable.
            % The first determinism promises more than the second in one aspect
            % (can_fail or soln_count), but promises less in the other aspect.

:- pred compare_determinisms(determinism::in, determinism::in,
    det_comparison::out) is det.

:- type det_component_comparison
    --->    first_tighter_than
    ;       first_same_as
    ;       first_looser_than.

:- pred compare_canfails(can_fail::in, can_fail::in,
    det_component_comparison::out) is det.

:- pred compare_solncounts(soln_count::in, soln_count::in,
    det_component_comparison::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module require.

%---------------------------------------------------------------------------%

det_conjunction_detism(DetismA, DetismB, Detism) :-
    % When figuring out the determinism of a conjunction, if the second goal
    % is unreachable, then the determinism of the conjunction is just
    % the determinism of the first goal.

    determinism_components(DetismA, CanFailA, MaxSolnA),
    (
        MaxSolnA = at_most_zero,
        Detism = DetismA
    ;
        ( MaxSolnA = at_most_one
        ; MaxSolnA = at_most_many
        ; MaxSolnA = at_most_many_cc
        ),
        determinism_components(DetismB, CanFailB, MaxSolnB),
        det_conjunction_canfail(CanFailA, CanFailB, CanFail),
        det_conjunction_maxsoln(MaxSolnA, MaxSolnB, MaxSoln),
        determinism_components(Detism, CanFail, MaxSoln)
    ).

det_par_conjunction_detism(DetismA, DetismB, Detism) :-
    % Figuring out the determinism of a parallel conjunction is much easier
    % than for a sequential conjunction, since you simply ignore the case
    % where the second goal is unreachable. Just do a normal solution count.

    determinism_components(DetismA, CanFailA, MaxSolnA),
    determinism_components(DetismB, CanFailB, MaxSolnB),
    det_conjunction_canfail(CanFailA, CanFailB, CanFail),
    det_conjunction_maxsoln(MaxSolnA, MaxSolnB, MaxSoln),
    determinism_components(Detism, CanFail, MaxSoln).

det_switch_detism(DetismA, DetismB, Detism) :-
    determinism_components(DetismA, CanFailA, MaxSolnA),
    determinism_components(DetismB, CanFailB, MaxSolnB),
    det_switch_canfail(CanFailA, CanFailB, CanFail),
    det_switch_maxsoln(MaxSolnA, MaxSolnB, MaxSoln),
    determinism_components(Detism, CanFail, MaxSoln).

det_negation_det(detism_det,       yes(detism_failure)).
det_negation_det(detism_semi,      yes(detism_semi)).
det_negation_det(detism_multi,     no).
det_negation_det(detism_non,       no).
det_negation_det(detism_cc_multi,  no).
det_negation_det(detism_cc_non,    no).
det_negation_det(detism_erroneous, yes(detism_erroneous)).
det_negation_det(detism_failure,   yes(detism_det)).

%---------------------------------------------------------------------------%

det_conjunction_maxsoln(at_most_zero,    at_most_zero,    at_most_zero).
det_conjunction_maxsoln(at_most_zero,    at_most_one,     at_most_zero).
det_conjunction_maxsoln(at_most_zero,    at_most_many_cc, at_most_zero).
det_conjunction_maxsoln(at_most_zero,    at_most_many,    at_most_zero).

det_conjunction_maxsoln(at_most_one,     at_most_zero,    at_most_zero).
det_conjunction_maxsoln(at_most_one,     at_most_one,     at_most_one).
det_conjunction_maxsoln(at_most_one,     at_most_many_cc, at_most_many_cc).
det_conjunction_maxsoln(at_most_one,     at_most_many,    at_most_many).

det_conjunction_maxsoln(at_most_many_cc, at_most_zero,    at_most_zero).
det_conjunction_maxsoln(at_most_many_cc, at_most_one,     at_most_many_cc).
det_conjunction_maxsoln(at_most_many_cc, at_most_many_cc, at_most_many_cc).
det_conjunction_maxsoln(at_most_many_cc, at_most_many,    _) :-
    % If the first conjunct could be cc pruned, the second conj ought to have
    % been cc pruned too.
    unexpected($module, $pred, "many_cc, many").

det_conjunction_maxsoln(at_most_many,    at_most_zero,    at_most_zero).
det_conjunction_maxsoln(at_most_many,    at_most_one,     at_most_many).
det_conjunction_maxsoln(at_most_many,    at_most_many_cc, at_most_many).
det_conjunction_maxsoln(at_most_many,    at_most_many,    at_most_many).

det_conjunction_canfail(can_fail,    can_fail,    can_fail).
det_conjunction_canfail(can_fail,    cannot_fail, can_fail).
det_conjunction_canfail(cannot_fail, can_fail,    can_fail).
det_conjunction_canfail(cannot_fail, cannot_fail, cannot_fail).

%---------------------------------------------------------------------------%

det_disjunction_maxsoln(at_most_zero,    at_most_zero,    at_most_zero).
det_disjunction_maxsoln(at_most_zero,    at_most_one,     at_most_one).
det_disjunction_maxsoln(at_most_zero,    at_most_many_cc, at_most_many_cc).
det_disjunction_maxsoln(at_most_zero,    at_most_many,    at_most_many).

det_disjunction_maxsoln(at_most_one,     at_most_zero,    at_most_one).
det_disjunction_maxsoln(at_most_one,     at_most_one,     at_most_many).
det_disjunction_maxsoln(at_most_one,     at_most_many_cc, at_most_many_cc).
det_disjunction_maxsoln(at_most_one,     at_most_many,    at_most_many).

det_disjunction_maxsoln(at_most_many_cc, at_most_zero,    at_most_many_cc).
det_disjunction_maxsoln(at_most_many_cc, at_most_one,     at_most_many_cc).
det_disjunction_maxsoln(at_most_many_cc, at_most_many_cc, at_most_many_cc).
det_disjunction_maxsoln(at_most_many_cc, at_most_many,    at_most_many_cc).

det_disjunction_maxsoln(at_most_many,    at_most_zero,    at_most_many).
det_disjunction_maxsoln(at_most_many,    at_most_one,     at_most_many).
det_disjunction_maxsoln(at_most_many,    at_most_many_cc, at_most_many_cc).
det_disjunction_maxsoln(at_most_many,    at_most_many,    at_most_many).

det_disjunction_canfail(can_fail,    can_fail,    can_fail).
det_disjunction_canfail(can_fail,    cannot_fail, cannot_fail).
det_disjunction_canfail(cannot_fail, can_fail,    cannot_fail).
det_disjunction_canfail(cannot_fail, cannot_fail, cannot_fail).

%---------------------------------------------------------------------------%

det_switch_maxsoln(at_most_zero,    at_most_zero,    at_most_zero).
det_switch_maxsoln(at_most_zero,    at_most_one,     at_most_one).
det_switch_maxsoln(at_most_zero,    at_most_many_cc, at_most_many_cc).
det_switch_maxsoln(at_most_zero,    at_most_many,    at_most_many).

det_switch_maxsoln(at_most_one,     at_most_zero,    at_most_one).
det_switch_maxsoln(at_most_one,     at_most_one,     at_most_one).
det_switch_maxsoln(at_most_one,     at_most_many_cc, at_most_many_cc).
det_switch_maxsoln(at_most_one,     at_most_many,    at_most_many).

det_switch_maxsoln(at_most_many_cc, at_most_zero,    at_most_many_cc).
det_switch_maxsoln(at_most_many_cc, at_most_one,     at_most_many_cc).
det_switch_maxsoln(at_most_many_cc, at_most_many_cc, at_most_many_cc).
det_switch_maxsoln(at_most_many_cc, at_most_many,    at_most_many_cc).

det_switch_maxsoln(at_most_many,    at_most_zero,    at_most_many).
det_switch_maxsoln(at_most_many,    at_most_one,     at_most_many).
det_switch_maxsoln(at_most_many,    at_most_many_cc, at_most_many_cc).
det_switch_maxsoln(at_most_many,    at_most_many,    at_most_many).

det_switch_canfail(can_fail,    can_fail,    can_fail).
det_switch_canfail(can_fail,    cannot_fail, can_fail).
det_switch_canfail(cannot_fail, can_fail,    can_fail).
det_switch_canfail(cannot_fail, cannot_fail, cannot_fail).

%---------------------------------------------------------------------------%

compare_determinisms(DetismA, DetismB, CmpDetism) :-
    determinism_components(DetismA, CanFailA, SolnsA),
    determinism_components(DetismB, CanFailB, SolnsB),
    compare_canfails(CanFailA, CanFailB, CmpCanFail),
    compare_solncounts(SolnsA, SolnsB, CmpSolns),

    % We can get e.g. tighter canfail and looser solncount
    % e.g. for a predicate declared multidet and inferred semidet.
    % Therefore the ordering of the following two tests is important:
    % we want errors to take precedence over warnings.

    (
        CmpCanFail = first_tighter_than,
        (
            ( CmpSolns = first_tighter_than
            ; CmpSolns = first_same_as
            ),
            CmpDetism = first_detism_tighter_than
        ;
            CmpSolns = first_looser_than,
            CmpDetism = first_detism_incomparable
        )
    ;
        CmpCanFail = first_same_as,
        (
            CmpSolns = first_tighter_than,
            CmpDetism = first_detism_tighter_than
        ;
            CmpSolns = first_same_as,
            CmpDetism = first_detism_same_as
        ;
            CmpSolns = first_looser_than,
            CmpDetism = first_detism_looser_than
        )
    ;
        CmpCanFail = first_looser_than,
        (
            CmpSolns = first_tighter_than,
            CmpDetism = first_detism_incomparable
        ;
            ( CmpSolns = first_same_as
            ; CmpSolns = first_looser_than
            ),
            CmpDetism = first_detism_looser_than
        )
    ).

compare_canfails(cannot_fail, cannot_fail, first_same_as).
compare_canfails(cannot_fail, can_fail,    first_tighter_than).
compare_canfails(can_fail,    cannot_fail, first_looser_than).
compare_canfails(can_fail,    can_fail,    first_same_as).

compare_solncounts(at_most_zero,    at_most_zero,    first_same_as).
compare_solncounts(at_most_zero,    at_most_one,     first_tighter_than).
compare_solncounts(at_most_zero,    at_most_many_cc, first_tighter_than).
compare_solncounts(at_most_zero,    at_most_many,    first_tighter_than).

compare_solncounts(at_most_one,     at_most_zero,    first_looser_than).
compare_solncounts(at_most_one,     at_most_one,     first_same_as).
compare_solncounts(at_most_one,     at_most_many_cc, first_tighter_than).
compare_solncounts(at_most_one,     at_most_many,    first_tighter_than).

compare_solncounts(at_most_many_cc, at_most_zero,    first_looser_than).
compare_solncounts(at_most_many_cc, at_most_one,     first_looser_than).
compare_solncounts(at_most_many_cc, at_most_many_cc, first_same_as).
compare_solncounts(at_most_many_cc, at_most_many,    first_tighter_than).

compare_solncounts(at_most_many,    at_most_zero,    first_looser_than).
compare_solncounts(at_most_many,    at_most_one,     first_looser_than).
compare_solncounts(at_most_many,    at_most_many_cc, first_looser_than).
compare_solncounts(at_most_many,    at_most_many,    first_same_as).

%---------------------------------------------------------------------------%
:- end_module parse_tree.prog_detism.
%---------------------------------------------------------------------------%
