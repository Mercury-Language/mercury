:- module untraced_subgoal.
:- interface.
:- import_module io.
:- pred main(io__state::di, io__state::uo) is det.
:- implementation.
:- import_module int, untraced_subgoal_sub.

main -->
	(
		{ p(1, X) },
		{ X > 10 }
	->
		io__write_string("yes\n")
	;
		io__write_string("no\n")
	),
	(
		{ p(2, Y) },
		{ Y > 10 }
	->
		io__write_string("yes\n")
	;
		io__write_string("no\n")
	).

:- pred p(int::in, int::out) is nondet.

p(1, X) :-
	untraced_subgoal_sub__q(X).

p(2, X) :-
	untraced_subgoal_sub__q(X),
	r(X, Y),
	s(Y).

:- pred r(int::in, int::out) is det.

r(X, X).

:- pred s(int::out) is multi.

s(2).
s(3).

% :- pred q(int::out) is multi.
%
% q(1).
% q(2).

