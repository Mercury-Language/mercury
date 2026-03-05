%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%

% Test clamp/3 signed 8-bit integers.

:- module clamp_int8.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module exception.
:- import_module int8.
:- import_module list.
:- import_module pair.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    list.foldl(run_clamp_test(numbers), ranges, !IO).

:- pred run_clamp_test(list(int8)::in, pair(int8)::in, io::di, io::uo)
    is cc_multi.

run_clamp_test(Numbers, Range, !IO) :-
    list.foldl(run_clamp_test_2(Range), Numbers, !IO).

:- pred run_clamp_test_2(pair(int8)::in, int8::in, io::di, io::uo)
    is cc_multi.

run_clamp_test_2(Range, N, !IO) :-
    Range = Min - Max,
    ( try []
        Result = int8.clamp(Min, Max, N)
    then
        ResultStr = int8_to_string(Result)
    catch_any _ ->
        ResultStr = "<<exception>>"
    ),
    io.format("clamp(%d, %d, %d) = %s\n",
        [i8(Min), i8(Max), i8(N), s(ResultStr)], !IO).

%---------------------------------------------------------------------------%

:- func numbers = list(int8).

numbers = [
    -128i8,
    -16i8,
    -10i8,
    -8i8,
    -2i8,
    -1i8,
    0i8,
    1i8,
    2i8,
    8i8,
    10i8,
    16i8,
    127i8
].

:- func ranges = list(pair(int8)).

ranges = [
    -2i8  - 12i8,
    -10i8 - 5i8,
    0i8   - 0i8,
    -1i8  - 1i8,
    5i8   - 10i8,
    12i8  - 6i8
].

%---------------------------------------------------------------------------%
:- end_module clamp_int8.
%---------------------------------------------------------------------------%
