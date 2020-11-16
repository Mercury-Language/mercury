%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module intermod_typeclass_bug.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module char.
:- import_module intermod_typeclass_bug2.

main(!IO) :-
    p('a', Int1),
    ( if char.to_int(Char1, Int1) then
        io.write_string("Test 1 succeeded: ", !IO),
        io.write_char(Char1, !IO),
        io.nl(!IO)
    else
        io.write_string("Test 1 failed: ", !IO),
        io.write_int(Int1, !IO),
        io.nl(!IO)
    ),

    ( if p(Char2, char.to_int('b')) then
        io.write_string("Test 2 succeeded: ", !IO),
        io.write_char(Char2, !IO),
        io.nl(!IO)
    else
        io.write_string("Test 2 failed", !IO),
        io.nl(!IO)
    ),

    ( if Int3 = q('c'), char.to_int(Char3, Int3) then
        io.write_string("Test 3 succeeded: ", !IO),
        io.write_char(Char3, !IO),
        io.nl(!IO)
    else
        io.write_string("Test 3 failed", !IO),
        io.nl(!IO)
    ).
