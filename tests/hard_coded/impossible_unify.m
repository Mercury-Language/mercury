:- module impossible_unify.
:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.
:- import_module bool.

main -->
	\+ { bar(42) },
	print("ok"), nl.

:- pred bar(int::in) is failure.

:- pred foo(int, bool).
:- mode foo(in, out(bound(no))) is det.

foo(_, no).

bar(X) :-
	foo(X, yes).
