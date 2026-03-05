%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%

% Test clamp/3 for word-sized unsigned integers.

:- module clamp_uint.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module exception.
:- import_module uint.
:- import_module list.
:- import_module pair.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    list.foldl(run_clamp_test(numbers), ranges, !IO).

:- pred run_clamp_test(list(uint)::in, pair(uint)::in, io::di, io::uo)
    is cc_multi.

run_clamp_test(Numbers, Range, !IO) :-
    list.foldl(run_clamp_test_2(Range), Numbers, !IO).

:- pred run_clamp_test_2(pair(uint)::in, uint::in, io::di, io::uo)
    is cc_multi.

run_clamp_test_2(Range, N, !IO) :-
    Range = Min - Max,
    ( try []
        Result = uint.clamp(Min, Max, N)
    then
        ResultStr = uint_to_string(Result)
    catch_any _ ->
        ResultStr = "<<exception>>"
    ),
    io.format("clamp(%u, %u, %u) = %s\n",
        [u(Min), u(Max), u(N), s(ResultStr)], !IO).

%---------------------------------------------------------------------------%

:- func numbers = list(uint).

numbers = [
    0u,
    1u,
    2u,
    8u,
    10u,
    16u,
    255u,
    65535u,
    4294967295u
].

:- func ranges = list(pair(uint)).

ranges = [
    0u - 0u,
    0u - 5u,
    5u - 10u,
    0u - 255u,
    12u - 6u
].

%---------------------------------------------------------------------------%
:- end_module clamp_uint.
%---------------------------------------------------------------------------%
