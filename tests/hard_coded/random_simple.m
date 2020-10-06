%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module random_simple.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int.
:- import_module random.

main(!IO) :-
    Seed = 3,
    random.init(Seed, RS0),
    test(1, 20, RS0, RS1, !IO),
    test(-1, 20, RS1, _, !IO).

:- pred test(int::in, int::in, random.supply::mdi, random.supply::muo,
    io::di, io::uo) is det.

test(Range, Count, RS0, RS, !IO) :-
    ( if Count > 0 then
        random.random(0, Range, N, RS0, RS1),
        ( if N = 0 then
            test(Range, Count - 1, RS1, RS, !IO)
        else
            io.write_string("Test failed.\n", !IO),
            RS = RS1
        )
    else
        io.write_string("Test passed.\n", !IO),
        RS = RS0
    ).
