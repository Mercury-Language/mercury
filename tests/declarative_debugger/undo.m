%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module undo.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.

main(!IO) :-
    sum([1, 2, 3, 4, 5, 6], S),
    io.write_int(S, !IO),
    io.nl(!IO).

:- pred sum(list(int)::in, int::out) is det.

sum([], 0).
sum([H | T], H + Sum) :-
    sum(T, Sum).
