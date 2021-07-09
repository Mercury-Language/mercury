%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%

% Test conversion of ints to signed 16-bit integers.

:- module from_int_int16.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int16.

:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    list.foldl(do_test, numbers, !IO).

:- pred do_test(int::in, io::di, io::uo) is det.

do_test(Int, !IO) :-
    io.format("from_int(%d) = ", [i(Int)], !IO),
    ( if
        int16.from_int(Int, Int16)
    then
        io.format("%s\n", [s(int16_to_string(Int16))], !IO)
    else
        io.write_string("<<out-of-range>>\n", !IO)
    ).

:- func numbers = list(int).

numbers = [
    -2147483648,
    -32769,
    -32768,
    -128,
    0,
    1,
    2,
    8,
    10,
    16,
    127,
    32767,
    32768,
    2147483647
].

%---------------------------------------------------------------------------%
:- end_module from_int_int16.
%---------------------------------------------------------------------------%
