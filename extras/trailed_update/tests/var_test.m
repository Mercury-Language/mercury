%-----------------------------------------------------------------------------%
% Copyright (C) 1997 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%

% Some test cases for extras/trailed_update/var.m.

% author: fjh

:- module var_test.

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
		print("X = "), output_var(X), nl,
		print("Y = "), output_var(Y), nl
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
		print("yes: X = "), output_var(X3),
		print(", Y = "), output_var(Y3), nl
	;
		print("no"), nl
	),
	print("test_delaying_4: "),
	( { create_unsolvable_delayed_goal(X4) } ->
		print("yes: X = "), output_var(X4), nl
	;
		print("no"), nl
	),
	print("test_ground:"), nl,
	{ Z = var(42) },
	print("Z = "), output_var(Z), nl,
	( { var__init(Z2), var__init(Z3), Z2 = Z3, Z3 = Z } ->
		print("Z2 = "), output_var(Z2), nl
	;
		print("oops"), nl
	),
	print("test_alias_twice:"), nl,
	( { A == B, A = B } ->
		print("A = "), output_var(A), nl,
		print("B = "), output_var(B), nl
	;
		print("oops"), nl
	),
	print("test_dup_call_bug:"), nl,
	( { var__init(A1), var__init(A2), A1 = var(42) } ->
		print("A1 = "), output_var(A1), nl,
		print("A2 = "), output_var(A2), nl
	;
		print("oops"), nl
	),
	print("Done.\n").

:- mode output_var(in(any), di, uo) is cc_multi.
output_var(Var) -->
	dump_var(Var),
	{ var__is_ground(Var, MaybeVal) },
	print(" [ground: "), write(MaybeVal), print("]").

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
%	unsafe_perform_io(output_var(Y)),
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
