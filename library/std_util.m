%---------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%

% File: std_util.m.
% Main author: fjh.
% Stability: medium to high.

% This file is intended for all the useful standard utilities
% that don't belong elsewhere, like <stdlib.h> in C.
%
% It contains the predicates solutions/2, semidet_succeed/0, semidet_fail/0,
% and report_stats/0; the types univ, unit, maybe(T), pair(T1, T2);
% and some predicates which operate on those types.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module std_util.

:- interface.

:- import_module list.

%-----------------------------------------------------------------------------%

% The universal type.
% Note that the current NU-Prolog implementation of univ_to_type
% is buggy in that it always succeeds, even if the types didn't
% match, so until this gets implemented correctly, don't use
% univ_to_type unless you are sure that the types will definely match.

:- type univ.

:- pred type_to_univ(_T, univ).
:- mode type_to_univ(di, uo) is det.
:- mode type_to_univ(in, out) is det.
:- mode type_to_univ(out, in) is semidet.

:- pred univ_to_type(univ, _T).
:- mode univ_to_type(in, out) is semidet.
:- mode univ_to_type(out, in) is det.

%-----------------------------------------------------------------------------%

% The "maybe" type.

:- type maybe(T) ---> yes(T) ; no.

%-----------------------------------------------------------------------------%

% The "unit" type - stores no information at all.

:- type unit		--->	unit.

%-----------------------------------------------------------------------------%

% The "pair" type.  Useful for many purposes.

:- type pair(T1, T2)	--->	(T1 - T2).
:- type pair(T)		==	pair(T,T).

%-----------------------------------------------------------------------------%

% solutions/2 collects all the solutions to a predicate and
% returns them as a list (in sorted order, with duplicates removed).

:- pred solutions(pred(T), list(T)).
:- mode solutions(pred(out) is multi, out) is det.
:- mode solutions(pred(out) is nondet, out) is det.

%-----------------------------------------------------------------------------%

% Declaratively, `report_stats' is the same as `true'.
% It has the side-effect of reporting some memory and time usage statistics
% to stdout.  (Technically, every Mercury implementation must offer
% a mode of invocation which disables this side-effect.)

:- pred report_stats is det.

%-----------------------------------------------------------------------------%

	% `semidet_succeed' is exactly the same as `true', except that
	% the compiler thinks that it is semi-deterministic.  You can
	% use calls to `semidet_succeed' to suppress warnings about
	% determinism declarations which could be stricter.
	% Similarly, `semidet_fail' is like `fail' except that its
	% determinism is semidet rather than failure.

:- pred semidet_succeed is semidet.

:- pred semidet_fail is semidet.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require, set.

:- external(report_stats/0).
:- external(type_to_univ/2).
:- external(semidet_fail/0).
:- external(semidet_succeed/0).

:- pred builtin_solutions(pred(T), set(T)).
:- mode builtin_solutions(pred(out) is multi, out) is det.
:- mode builtin_solutions(pred(out) is nondet, out) is det.
:- external(builtin_solutions/2).

solutions(Pred, List) :-
	builtin_solutions(Pred, Set),
	set__to_sorted_list(Set, List).

univ_to_type(Univ, X) :- type_to_univ(X, Univ).

%-----------------------------------------------------------------------------%
