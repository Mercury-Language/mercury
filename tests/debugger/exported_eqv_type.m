% This test case used to cause the compiler to generate C code containing
% a reference to an undefined common cell.

:- module exported_eqv_type.

:- interface.

:- type bug(T) == T.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.

main(!IO) :-
	X = p(2, 55),
	Y = p(3, "a"),
	io__write(X, !IO),
	io__nl(!IO),
	io__write(Y, !IO),
	io__nl(!IO).

:- func p(int, bug(T)) = bug(list(T)).

p(Num, Item) = Dups :-
	list__duplicate(Num, Item, Dups).
