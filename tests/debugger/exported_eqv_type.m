%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
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
    io.write(X, !IO),
    io.nl(!IO),
    io.write(Y, !IO),
    io.nl(!IO).

:- func p(int, bug(T)) = bug(list(T)).

p(Num, Item) = Dups :-
    list.duplicate(Num, Item, Dups).
