%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% The .exp file is for when int is 32-bit.
% The .exp2 file is for when int is 64-bit.
%
%---------------------------------------------------------------------------%

:- module test_string_to_int_overflow.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module maybe.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    test(string.to_int("999"), !IO),
    test(base_string_to_int(10, "999"), !IO),
    test(string.to_int("99999999999999999999"), !IO),
    test(string.to_int("-99999999999999999999"), !IO),

    line(!IO),

    test(base_string_to_int(10, "2147483647"), !IO),
    test(base_string_to_int(10, "2147483648"), !IO),
    test(base_string_to_int(10, "-2147483648"), !IO),
    test(base_string_to_int(10, "-2147483649"), !IO),

    line(!IO),

    test(base_string_to_int(10, "9223372036854775807"), !IO),
    test(base_string_to_int(10, "9223372036854775808"), !IO),
    test(base_string_to_int(10, "-9223372036854775808"), !IO),
    test(base_string_to_int(10, "-9223372036854775809"), !IO),

    line(!IO),

    test(base_string_to_int(16, "7fffffff"), !IO),
    test(base_string_to_int(16, "80000000"), !IO),
    test(base_string_to_int(16, "-80000000"), !IO),
    test(base_string_to_int(16, "-80000001"), !IO),

    line(!IO),

    test(base_string_to_int(16, "7fffffffffffffff"), !IO),
    test(base_string_to_int(16, "8000000000000000"), !IO),
    test(base_string_to_int(16, "-8000000000000000"), !IO),
    test(base_string_to_int(16, "-8000000000000001"), !IO),

    line(!IO),

    test(base_string_to_int(36, "ZIK0ZJ"), !IO),
    test(base_string_to_int(36, "ZIK0ZK"), !IO),
    test(base_string_to_int(36, "-ZIK0ZK"), !IO),
    test(base_string_to_int(36, "-ZIK0ZL"), !IO),

    line(!IO),

    test(base_string_to_int(36, "1Y2P0IJ32E8E7"), !IO),
    test(base_string_to_int(36, "1Y2P0IJ32E8E8"), !IO),
    test(base_string_to_int(36, "-1Y2P0IJ32E8E8"), !IO),
    test(base_string_to_int(36, "-1Y2P0IJ32E8E9"), !IO).

:- pred test(pred(T), io, io).
:- mode test(pred(out) is semidet, di, uo) is det.

test(P, !IO) :-
    ( P(X) ->
        io.write(X, !IO),
        io.nl(!IO)
    ;
        io.write_string("no\n", !IO)
    ).

:- pred line(io::di, io::uo) is det.

line(!IO) :-
    io.write_string("--------\n", !IO).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
