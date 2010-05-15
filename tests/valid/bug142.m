% Regression test for bug #142 (worked around in inlining.m)
%
% mmc --optimise-higher-order --inline-single-use -C bug142.m
% Uncaught Mercury exception:
% Software Error: hlds_rtti.m: Unexpected: inconsistent typeclass_infos

:- module bug142.
:- interface.

:- type r(T)
    --->    ok(T)
    ;       err.

:- type dcg(T, State) == (pred(r(T), State, State)).
:- mode dcg           == in(pred(out, in, out) is det).

:- typeclass dcg(Token, State) <= ((State -> (Token))) where [
].

:- pred or(dcg(T, State)::dcg, dcg(T, State)::dcg,
    r(T)::out, State::in, State::out) is det <= dcg(Token, State).

:- pred orr(dcg(T, State)::dcg, dcg(T, State)::dcg, dcg(T, State)::dcg,
    r(T)::out, State::in, State::out) is det <= dcg(Token, State).

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- implementation.

or(PA, PB, Result, S0, S) :-
    PA(RA, S0, S1),
    (
        RA = ok(V),
        S = S1,
        Result = ok(V)
    ;
        RA = err,
        PB(RB, S0, S),
        (
            RB = ok(V),
            Result = ok(V)
        ;
            RB = err,
            Result = err
        )
    ).

orr(PA, PB, PC, Result, S0, S) :-
    or(or(PA, PB), PC, Result, S0, S).

%------------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
