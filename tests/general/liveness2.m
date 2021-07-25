%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% The Mercury compiler of Monday November 25, 1996 (13:00) produced
% incorrect code for this.
%
% Suspected liveness bug, basically the compiler picks the value of
% the result variable (Num) up from the wrong register - containing
% Test not NumLast as it should.
%
% The switch must be in the condition of the if-then-else, and the
% test unification must follow it.

:- module liveness2.

:- interface.

:- import_module bool.
:- import_module int.
:- import_module io.
:- import_module std_util.

:- pred main(io, io).
:- mode main(di, uo) is det.

:- pred gather(bool::in, int::in, int::in, int::out) is det.

:- implementation.

main(!IO) :-
    gather(yes, 0, 7, Num),
    ( if Num = 0 then
        io.write_string("Num is correct: ", !IO)
    else
        io.write_string("Num is incorrect: ", !IO)
    ),
    io.write_int(Num, !IO),
    io.nl(!IO).

gather(P, NumLast, Test, Num) :-
    ( if
        ( P = yes, Test1 = 1, NumThis = 10
        ; P = no,  Test1 = 2, NumThis = 20
        ),
        Test = Test1
    then
        Num = NumThis
    else
        Num = NumLast
    ).
