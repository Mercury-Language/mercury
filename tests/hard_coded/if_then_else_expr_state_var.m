% vim: ft=mercury ts=4 sw=4 et

:- module if_then_else_expr_state_var.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.
:- import_module string.

main(!IO) :-
    test(1, 101, !IO),
    test(2, 102, !IO),
    test(11, 111, !IO),
    test(12, 112, !IO).

:- pred test(int::in, int::in, io::di, io::uo) is det.

test(InitX, InitY, !IO) :-
    some [!X, !Y] (
        !:X = InitX,
        !:Y = InitY,
        set( ( if big(!.X) then increment3(!.Y, !:Y) else !.X + 5 ), !:X),
        FinalX = !.X,
        FinalY = !.Y
    ),
    io.format("InitX = %3d, FinalX = %3d\n", [i(InitX), i(FinalX)], !IO),
    io.format("InitY = %3d, FinalY = %3d\n", [i(InitY), i(FinalY)], !IO),
    io.nl(!IO).

:- pred big(int::in) is semidet.

big(N) :-
    N > 10.

:- func increment3(int::in, int::out) = (int::out) is det.

increment3(N, M) = M :-
    M = N + 3.

:- pred set(int::in, int::out) is det.

set(X, X).
