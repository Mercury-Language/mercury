%-----------------------------------------------------------------------------%
% Copyright (C) 1999-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: term_rep.m
% Author: Ian MacLarty
%
% This module implements an abstract type, term_rep, values of which are the
% representation of some other value.  Constructing a representation from a
% term is cc_multi, but then doing comparisons on the representation is
% deterministic.
%
% This is useful when we only want to consider the representation of a term
% and don't care about it's actual value.

:- module mdb.term_rep.

:- interface.

:- import_module mdbcomp.
:- import_module mdbcomp.program_representation.

:- import_module std_util.

:- type term_rep.

:- pred univ_to_rep(univ::in, term_rep::out) is cc_multi.

:- pred rep_to_univ(term_rep::in, univ::out) is det.

:- pred deref_path(term_rep::in, term_path::in, term_rep::out) is semidet.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.

:- type term_rep
	---> term_rep(univ)
	where
		equality is term_rep_equal,
		comparison is term_rep_compare.

:- pred term_rep_equal(term_rep::in, term_rep::in) is semidet.

term_rep_equal(Rep1, Rep2) :-
	(=) = promise_only_solution(comp_rep_2(Rep1, Rep2)).

:- pred comp_rep_2(term_rep::in, term_rep::in, builtin.comparison_result::uo)
	is cc_multi.

comp_rep_2(Rep1, Rep2, Result) :-
	builtin.compare_representation(Result, Rep1, Rep2).

:- pred term_rep_compare(builtin.comparison_result::uo, term_rep::in,
	term_rep::in) is det.

term_rep_compare(Result, Rep1, Rep2) :-
	Result = promise_only_solution(comp_rep_2(Rep1, Rep2)).

univ_to_rep(Univ0, term_rep(Univ)) :- cc_multi_equal(Univ0, Univ).

rep_to_univ(Rep, Univ) :-
	Univ = promise_only_solution(
		pred(U::out) is cc_multi :- Rep = term_rep(U)
	).

deref_path(Term, Path, SubTerm):-
	(
		Path = [],
		SubTerm = Term
	;
		Path = [Head | Tail],
		%
		% There is only one representation of a subterm, given
		% the representation of the containing term and a term path.
		%
		promise_equivalent_solutions [NextSubTerm] (
			rep_to_univ(Term, Univ),
			% Argument indexes in the term path start from one, but
			% the argument function wants argument indexes to
			% start from zero.
			SubUniv = argument(univ_value(Univ), Head - 1),
			univ_to_rep(SubUniv, NextSubTerm)
		),
		deref_path(NextSubTerm, Tail, SubTerm)
	).
