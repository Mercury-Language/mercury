:- module fib_list.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module benchmarking, require, std_util.
:- import_module int, list, assoc_list.

main(!IO) :-
	perform_trials([1, 4], !IO).

:- pred perform_trials(list(int)::in, io::di, io::uo) is cc_multi.

perform_trials(N, !IO) :-
	trial(N, Time, MTime),
	% io__write(N, !IO),
	% io__write_string(": ", !IO),
	% io__write_int(Time, !IO),
	% io__write_string("ms vs ", !IO),
	% io__write_int(MTime, !IO),
	% io__write_string("ms\n", !IO),
	(
		(
			Time > 10 * MTime,
			MTime > 0	% untabled takes ten times as long
		;
			Time > 100,	% untabled takes at least 100 ms
			MTime < 1	% while tabled takes at most 1 ms
		)
	->
		io__write_string("tabling works\n", !IO)
	;
		Time > 10000		% untabled takes at least 10 seconds
	->
		io__write_string("tabling does not appear to work\n", !IO)
	;
		% We couldn't get a measurable result with N,
		% and it looks like we can afford a bigger trial
		perform_trials(add_digits(N, [3]), !IO)
	).

:- pred trial(list(int)::in, int::out, int::out) is cc_multi.

trial(N, Time, MTime) :-
	benchmark_det(fib, N, Res, 1, Time),
	benchmark_det(mfib, N, MRes, 1, MTime),
	require(unify(Res, MRes), "tabling produces wrong answer").

:- pred fib(list(int)::in, list(int)::out) is det.

fib(N, F) :-
	( digits_to_num(N) < 2 ->
		F = num_to_digits(1)
	;
		One = num_to_digits(1),
		Two = num_to_digits(2),
		fib(subtract_digits(N, One), F1),
		fib(subtract_digits(N, Two), F2),
		F = add_digits(F1, F2)
	).

:- pred mfib(list(int)::in, list(int)::out) is det.
:- pragma memo(mfib/2).

mfib(N, F) :-
	( digits_to_num(N) < 2 ->
		F = num_to_digits(1)
	;
		One = num_to_digits(1),
		Two = num_to_digits(2),
		mfib(subtract_digits(N, One), F1),
		mfib(subtract_digits(N, Two), F2),
		F = add_digits(F1, F2)
	).

:- func add_digits(list(int), list(int)) = list(int).

add_digits(S1, S2) =
	num_to_digits(digits_to_num(S1) + digits_to_num(S2)).

:- func subtract_digits(list(int), list(int)) = list(int).

subtract_digits(S1, S2) =
	num_to_digits(digits_to_num(S1) - digits_to_num(S2)).

:- func digits_to_num(list(int)) = int.

digits_to_num(Digits) = Num :-
	list__reverse(Digits, RevDigits),
	Num = digits_to_num_2(RevDigits).

:- func digits_to_num_2(list(int)) = int.

digits_to_num_2([]) = 0.
digits_to_num_2([Last | Rest]) =
	10 * digits_to_num_2(Rest) + Last.

:- func num_to_digits(int) = list(int).

num_to_digits(Int) = Digits :-
	( Int < 10 ->
		Digits = [Int]
	;
		Last = Int mod 10,
		Rest = Int // 10,
		list__append(num_to_digits(Rest), [Last], Digits)
	).
