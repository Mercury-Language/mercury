:- module run.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is cc_multi.

:- pred use(T::in) is semidet.

:- implementation.

:- import_module benchmarking, gc, list, string, int, std_util.

	% The imports which are modules to be benchmarked.
:- import_module 
	advisor,
	applast,
	doubleapp,
	flip,
	grammar,
	imperative_solve_power,
	map_reduce,
	map_rev,
	match_kmp,
	match,
	match_append,
	maxlength,
	missionaries,
	regexp_r1,
	regexp_r2,
	regexp_r3,
	relative,
	remove,
	remove2,
	ssuply,
	transpose,
	upto_sum1,
	upto_sum2,
	contains_kmp,
	contains_lam,
	rotateprune.

main -->
	io__command_line_arguments(Args), 
	(
		{ Args = ["-n", Arg2] },
		{ string__to_int(Arg2, Iterations0) }
	->
		{ Iterations = Iterations0 }
	;
		{ Iterations = 1 }
	),
	io__write_string("Iterations: "),
	io__write_int(Iterations),
	io__nl,
	io__nl,
	run_benchmark_list(Iterations, benchmark_list).

:- pred run_benchmark_list(int::in, list(benchmark)::in(list_skel(benchmark)), 
		io__state::di, io__state::uo) is cc_multi.

run_benchmark_list(_, []) --> [].
run_benchmark_list(Iterations, [Name - Closure | Benchmarks]) -->
	run_benchmark(Iterations, Name, Closure),
	run_benchmark_list(Iterations, Benchmarks).

:- pred run_benchmark(int, string, pred, io__state, io__state). 
:- mode run_benchmark(in, in, (pred) is semidet, di, uo) is cc_multi.

run_benchmark(Iterations, Name, Closure) -->
	% By default, we just run a single iteration and print out
	% for each test whether the query succeeded or failed;
	% this is used by the test suite framework.
	% If the `-n' option is used (see above), we run
	% multiple iterations, and print out the times for each benchmark.
	% This can be useful for testing the effect of optimizations.
	{ CallClosure = 
		( pred(_Input::in, Output::out) is det :-
			( call(Closure) ->
				Output = 1
			;
				Output = 0
			)
		) },
	{ benchmark_det(CallClosure, 0, Result, Iterations, Time) },
	( { Iterations > 1 } ->
		io__format("%-30s     result %3d        time (ms) %8d\n",
			[s(Name), i(Result), i(Time)])
	;
		io__format("%-30s     result %3d\n", [s(Name), i(Result)])
	),
	io__flush_output,
	garbage_collect.

:- func benchmark_list = list(benchmark).
:- mode benchmark_list = list_skel_out(benchmark).

:- type benchmark == pair(string, pred).
:- inst benchmark == pair(ground, (pred) is semidet).

use(_) :- semidet_succeed.

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
