%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module resume.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.

main(!IO) :-
    ( if wrapper(A, B, C, D, E, F) then
        io.write(A, !IO),
        io.write(B, !IO),
        io.write(C, !IO),
        io.write(D, !IO),
        io.write(E, !IO),
        io.write(F, !IO)
    else
        io.write_string("fail", !IO)
    ),
    io.nl(!IO).

:- pred wrapper(int::out, int::out, int::out, int::out, int::out, int::out)
    is semidet.

wrapper(A, B, C, D, E, F) :-
    A = l([1, 2, 3, 4, 5]),
    B = l([6, 7, 8, 9, 10]),
    C = l([11, 12, 13, 14, 15]),
    D = l([16, 17, 18, 19, 20]),
    E = l([21, 22, 23, 24, 25]),
    F = l([26, 27, 28, 29, 30]).

:- func l(list(T)) = T is semidet.

l([H | []]) = H.
l([_ | [TH | TT]]) = l([TH | TT]).
