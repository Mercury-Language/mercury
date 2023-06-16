%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%

% Test conversion of ints to signed 8-bit integers.

:- module from_int_int8.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int8.

:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    list.foldl(do_test, numbers, !IO).

:- pred do_test(int::in, io::di, io::uo) is det.

do_test(Int, !IO) :-
    io.format("from_int(%d) = ", [i(Int)], !IO),
    ( if int8.from_int(Int, Int8) then
        io.format("%s\n", [s(int8_to_string(Int8))], !IO)
    else
        io.write_string("<<out-of-range>>\n", !IO)
    ).

:- func numbers = list(int).

numbers = [
    -2147483648,
    -129,
    -128,
    0,
    1,
    2,
    8,
    10,
    16,
    127,
    128,
    2147483647
].

%---------------------------------------------------------------------------%
:- end_module from_int_int8.
%---------------------------------------------------------------------------%
