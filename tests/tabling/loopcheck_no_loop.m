%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Check that loopcheck isn't overzealous.

:- module loopcheck_no_loop.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- pragma require_feature_set([memo]).

:- import_module int.

main(!IO) :-
    sum(3, Sum3),
    io.write_int(Sum3, !IO),
    io.write_string("\n", !IO),
    sum(2, Sum2),
    io.write_int(Sum2, !IO),
    io.write_string("\n", !IO),
    ( if semisum(3, Semisum3) then
        io.write_int(Semisum3, !IO),
        io.write_string("\n", !IO)
    else
        io.write_string("semisum(3) failed\n", !IO)
    ),
    ( if semisum(2, Semisum2) then
        io.write_int(Semisum2, !IO),
        io.write_string("\n", !IO)
    else
        io.write_string("semisum(2) failed\n", !IO)
    ),
    ( if semisum(-2, SemisumN2) then
        io.write_int(SemisumN2, !IO),
        io.write_string("\n", !IO)
    else
        io.write_string("semisum(-2) failed\n", !IO)
    ),
    ( if count(3) then
        io.write_string("count(3) succeeded\n", !IO)
    else
        io.write_string("count(3) failed\n", !IO)
    ),
    ( if count(2) then
        io.write_string("count(2) succeeded\n", !IO)
    else
        io.write_string("count(2) failed\n", !IO)
    ),
    ( if count(-2) then
        io.write_string("count(-2) succeeded\n", !IO)
    else
        io.write_string("count(-2) failed\n", !IO)
    ).

:- pred sum(int::in, int::out) is det.
:- pragma loop_check(sum/2).

sum(N, SumN) :-
    ( if N = 0 then
        SumN = 0
    else
        sum(N - 1, Sum1),
        SumN = Sum1 + N
    ).

:- pred semisum(int::in, int::out) is semidet.
:- pragma loop_check(semisum/2).

semisum(N, SumN) :-
    ( if N < 0 then
        fail
    else if N = 0 then
        SumN = 0
    else
        semisum(N - 1, Sum1),
        SumN = Sum1 + N
    ).

:- pred count(int::in) is semidet.
:- pragma loop_check(count/1).

count(N) :-
    ( if N < 0 then
        fail
    else if N = 0 then
        true
    else
        count(N - 1)
    ).
