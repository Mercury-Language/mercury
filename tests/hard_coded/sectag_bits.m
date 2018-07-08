%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test both the reading and the writing out of terms whose representation
% contains arguments packed in the same word as a secondary tag.
%
%---------------------------------------------------------------------------%

:- module sectag_bits.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- type kind
    --->    kind_a
    ;       kind_b.

:- type test
    --->    test_int(int)
    ;       test_kind(kind)
    ;       test_kinds(kind, kind)
    ;       test_two(test, test).

main(!IO) :-
    io.open_input("sectag_bits_test_data", Res, !IO),
    ( if Res = ok(Stream) then
        io.read(Stream, X : io.read_result(test), !IO),
        io.write_line(X, !IO)
    else
        io.write_string("cannot open sectag_bits_test_data\n", !IO)
    ).
