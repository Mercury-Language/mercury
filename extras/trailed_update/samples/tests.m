%-----------------------------------------------------------------------------%
% Copyright (C) 1997 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%

% Some test cases for extras/trailed_update/var.m.

% author: fjh

:- module tests.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is cc_multi.

:- implementation.
:- import_module var.
:- import_module require, int, std_util.
:- import_module unsafe.

main -->
	print("test_delaying_1: "),
	( { test_delaying_1 } ->
		print("yes"), nl
	;
		print("no"), nl
	),
	print("test_delaying_2: "),
	( { test_delaying_2 } ->
		print("yes"), nl
	;
		print("no"), nl
	),

	( { create_solvable_delayed_goal(X, Y) } ->
		print("X = "), dump_var(X), nl,
		print("Y = "), dump_var(Y), nl
	;
		print("Oops.\n")
	),

	print("test_delaying_1: "),
	( { test_delaying_1 } ->
		print("yes"), nl
	;
		print("no"), nl
	),
	print("test_delaying_2: "),
	( { test_delaying_2 } ->
		print("yes"), nl
	;
		print("no"), nl
	),

	print("test_delaying_3: "),
	( { create_solvable_delayed_goal(X3, Y3) } ->
		print("yes: X = "), dump_var(X3),
		print(", Y = "), dump_var(Y3), nl
	;
		print("no"), nl
	),
	print("test_delaying_4: "),
	( { create_unsolvable_delayed_goal(X4) } ->
		print("yes: X = "), dump_var(X4), nl
	;
		print("no"), nl
	),
	print("Done.\n").

test_delaying_1 :-
	create_solvable_delayed_goal(X, Y),
	wake_and_fail(X, Y).

test_delaying_2 :-
	create_solvable_delayed_goal(X, Y),
	wake_and_succeed(X, Y).

create_solvable_delayed_goal(X, Y) :-
	% debug_freeze("add_one",
	freeze(
		X, (pred(XVal::in, YVal::out) is det :-
			YVal = XVal + 1), Y).

wake_and_succeed(var(0), var(1)).  % 1 = 0 + 1 succeeds
%	unsafe_perform_io(print("Y = ")),
%	unsafe_perform_io(dump_var(Y)),
%	unsafe_perform_io(nl).

wake_and_fail(var(0), var(42)). % 42 = 0 + 1 fails.

create_unsolvable_delayed_goal(X) :-
	init(X),
	% debug_freeze("always_fail",
	freeze(
		X, (pred(_::in) is semidet :- fail)).

% :- mode test_modes_1.  (not yet supported)
test_modes_1 :-
	% test auto-initialize (implied free -> any)
	p(_),
	p2(_).

:- mode test_modes_2(in(any)).
test_modes_2(X) :-
	% test implied mode
	q(X).

:- mode p(any -> any) is semidet.
p(_) :- semidet_succeed.

:- mode p2(any -> ground) is failure.
p2(_) :- fail.

:- mode q(free -> any) is det.
q(X) :- init(X).
