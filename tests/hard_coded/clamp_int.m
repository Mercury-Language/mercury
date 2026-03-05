%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%

% Test clamp/3 for word-sized signed integers.

:- module clamp_int.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module exception.
:- import_module int.
:- import_module list.
:- import_module pair.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    list.foldl(run_clamp_test(numbers), ranges, !IO).

:- pred run_clamp_test(list(int)::in, pair(int)::in, io::di, io::uo)
    is cc_multi.

run_clamp_test(Numbers, Range, !IO) :-
    list.foldl(run_clamp_test_2(Range), Numbers, !IO).

:- pred run_clamp_test_2(pair(int)::in, int::in, io::di, io::uo)
    is cc_multi.

run_clamp_test_2(Range, N, !IO) :-
    Range = Min - Max,
    ( try []
        Result = int.clamp(Min, Max, N)
    then
        ResultStr = int_to_string(Result)
    catch_any _ ->
        ResultStr = "<<exception>>"
    ),
    io.format("clamp(%d, %d, %d) = %s\n",
        [i(Min), i(Max), i(N), s(ResultStr)], !IO).

%---------------------------------------------------------------------------%

:- func numbers = list(int).

numbers = [
    -2_147_483_648,
    -32_768,
    -128,
    0,
    1,
    2,
    8,
    10,
    16,
    127,
    32_767,
    2_147_483_647
].

:- func ranges = list(pair(int)).

ranges = [
    -2  - 12,
    -10 - 5,
    0   - 0,
    -1  - 1,
    5   - 10,
    12  - 6
].

%---------------------------------------------------------------------------%
:- end_module clamp_int.
%---------------------------------------------------------------------------%
