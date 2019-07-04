%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module run.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- pred use(T::in) is semidet.

:- implementation.

:- import_module benchmarking.
:- import_module gc.
:- import_module list.
:- import_module string.
:- import_module int.
:- import_module pair.

% The imports which are modules to be benchmarked.
:- import_module advisor.
:- import_module applast.
:- import_module doubleapp.
:- import_module flip.
:- import_module grammar.
:- import_module imperative_solve_power.
:- import_module map_reduce.
:- import_module map_rev.
:- import_module match_kmp.
:- import_module match.
:- import_module match_append.
:- import_module maxlength.
:- import_module missionaries.
:- import_module regexp_r1.
:- import_module regexp_r2.
:- import_module regexp_r3.
:- import_module relative.
:- import_module remove.
:- import_module remove2.
:- import_module ssuply.
:- import_module transpose.
:- import_module upto_sum1.
:- import_module upto_sum2.
:- import_module contains_kmp.
:- import_module contains_lam.
:- import_module rotateprune.

main(!IO) :-
    io.command_line_arguments(Args, !IO),
    ( if
        Args = ["-n", Arg2],
        string.to_int(Arg2, Iterations0)
    then
        Iterations = Iterations0
    else
        Iterations = 1
    ),
    io.write_string("Iterations: ", !IO),
    io.write_int(Iterations, !IO),
    io.nl(!IO),
    io.nl(!IO),
    run_benchmark_list(Iterations, benchmark_list, !IO).

:- pred run_benchmark_list(int::in, list(benchmark)::in(list_skel(benchmark)),
    io::di, io::uo) is cc_multi.

run_benchmark_list(_, [], !IO).
run_benchmark_list(Iterations, [Name - Closure | Benchmarks], !IO) :-
    run_benchmark(Iterations, Name, Closure, !IO),
    run_benchmark_list(Iterations, Benchmarks, !IO).

:- pred run_benchmark(int, string, pred, io__state, io__state).
:- mode run_benchmark(in, in, (pred) is semidet, di, uo) is cc_multi.

run_benchmark(Iterations, Name, Closure, !IO) :-
    % By default, we just run a single iteration and print out
    % for each test whether the query succeeded or failed;
    % this is used by the test suite framework.
    % If the `-n' option is used (see above), we run
    % multiple iterations, and print out the times for each benchmark.
    % This can be useful for testing the effect of optimizations.
    CallClosure =
        ( pred(_Input::in, Output::out) is det :-
            ( if call(Closure) then
                Output = 1
            else
                Output = 0
            )
        ),
    benchmark_det(CallClosure, 0, Result, Iterations, Time),
    ( if Iterations > 1 then
        io.format("%-30s     result %3d        time (ms) %8d\n",
            [s(Name), i(Result), i(Time)], !IO)
    else
        io.format("%-30s     result %3d\n", [s(Name), i(Result)], !IO)
    ),
    io.flush_output(!IO),
    garbage_collect(!IO).

:- func benchmark_list = list(benchmark).
:- mode benchmark_list = list_skel_out(benchmark).

:- type benchmark == pair(string, pred).
:- inst benchmark == pair(ground, (pred) is semidet).

use(_) :-
    semidet_succeed.

benchmark_list = [
    "advisor" - advisor,
    "applast" - applast,
    "contains_kmp" - contains_kmp,
    "contains_lam" - contains_lam,
    "doubleapp" - doubleapp,
    "flip" - flip,
    "grammar" - grammar,
    "imperative_solve_power" - imperative_solve_power,
    "map_reduce" - map_reduce,
    "map_rev" - map_rev,
    "match_kmp" - match_kmp,
    "match" - match,
    "match_append" - match_append,
    "maxlength" - maxlength,
    "missionaries" - missionaries,
    "regexp_r1" - regexp_r1,
    "regexp_r2" - regexp_r2,
    "regexp_r3" - regexp_r3,
    "relative" - relative,
    "remove" - remove,
    "remove2" - remove2,
    "rotateprune" - rotateprune,
    "ssuply" - ssuply,
    "transpose" - transpose,
    "upto_sum1" - upto_sum1,
    "upto_sum2" - upto_sum2
].
