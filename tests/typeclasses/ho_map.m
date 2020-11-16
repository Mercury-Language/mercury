%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module ho_map.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.

main(!IO) :-
    list__map(p, [1, 2], X),
    io.write(X, !IO),
    io.nl(!IO).

:- typeclass foo(T) where [
    pred p(T::in, T::out) is det
].

:- instance foo(int) where [
    pred(p/2) is blah
].

:- pred blah(int::in, int::out) is det.

blah(X, X+1).
