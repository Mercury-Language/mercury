%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module implication.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

main(!IO) :-
    io.write_string("implication:\n", !IO),
    ( if t => t then
        ok(!IO)
    else
        bad(!IO)
    ),
    ( if t => f then
        bad(!IO)
    else
        ok(!IO)
    ),
    ( if f => t then
        ok(!IO)
    else
        bad(!IO)
    ),
    ( if f => f then
        ok(!IO)
    else
        bad(!IO)
    ),

    io.write_string("reverse implication:\n", !IO),
    ( if t <= t then
        ok(!IO)
    else
        bad(!IO)
    ),
    ( if f <= t then
        bad(!IO)
    else
        ok(!IO)
    ),
    ( if t <= f then
        ok(!IO)
    else
        bad(!IO)
    ),
    ( if f <= f then
        ok(!IO)
    else
        bad(!IO)
    ),

    io.write_string("logical equivalence:\n", !IO),
    ( if t <=> t then
        ok(!IO)
    else
        bad(!IO)
    ),
    ( if t <=> f then
        bad(!IO)
    else
        ok(!IO)
    ),
    ( if f <=> t then
        bad(!IO)
    else
        ok(!IO)
    ),
    ( if f <=> f then
        ok(!IO)
    else
        bad(!IO)
    ),

    io.write_string("done.\n", !IO).

:- pred t is semidet.

t :- semidet_true.

:- pred f is semidet.

f :- semidet_false.

:- pred ok(io::di, io::uo) is det.

ok(!IO) :-
    io.write_string("ok\n", !IO).

:- pred bad(io::di, io::uo) is det.

bad(!IO) :-
    io.write_string("fail\n", !IO).
