%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module ignore.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module ignore_1.
:- import_module int.
:- import_module list.

main(!IO) :-
    p(X),
    p(Y),
    write_int(X+Y, !IO),
    nl(!IO).

:- pred p(int::out) is det.

p(ignore_1.fold(q, [1, 2, 3, 4, 5], 0)).

:- func q(int, int) = int.

q(X, Y) = X+Y.
