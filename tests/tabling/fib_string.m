:- module fib_string.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module benchmarking, require, std_util.
:- import_module int, string, list, assoc_list.

main(!IO) :-
	perform_trials("oneone", !IO).

:- pred perform_trials(string::in, io::di, io::uo) is cc_multi.

perform_trials(N, !IO) :-
	trial(N, Time, MTime),
	% io__write_string(N, !IO),
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
		perform_trials(add_strings(N, "three"), !IO)
	).

:- pred trial(string::in, int::out, int::out) is cc_multi.

trial(N, Time, MTime) :-
	benchmark_det(fib, N, Res, 1, Time),
	benchmark_det(mfib, N, MRes, 1, MTime),
	require(unify(Res, MRes), "tabling produces wrong answer").

:- pred fib(string::in, string::out) is det.

fib(N, F) :-
	( string_to_num(N) < 2 ->
		F = num_to_string(1)
	;
		One = num_to_string(1),
		Two = num_to_string(2),
		fib(subtract_strings(N, One), F1),
		fib(subtract_strings(N, Two), F2),
		F = add_strings(F1, F2)
	).

:- pred mfib(string::in, string::out) is det.
:- pragma memo(mfib/2).

mfib(N, F) :-
	( string_to_num(N) < 2 ->
		F = num_to_string(1)
	;
		One = num_to_string(1),
		Two = num_to_string(2),
		mfib(subtract_strings(N, One), F1),
		mfib(subtract_strings(N, Two), F2),
		F = add_strings(F1, F2)
	).

:- func add_strings(string, string) = string.

add_strings(S1, S2) =
	num_to_string(string_to_num(S1) + string_to_num(S2)).

:- func subtract_strings(string, string) = string.

subtract_strings(S1, S2) =
	num_to_string(string_to_num(S1) - string_to_num(S2)).

:- func string_to_num(string) = int.

string_to_num(String) = Num :-
	translate_last_digit(String, LastNum, RestString),
	( RestString = "" ->
		Num = LastNum
	;
		Num = string_to_num(RestString) * 10 + LastNum
	).

:- pred translate_last_digit(string::in, int::out, string::out) is det.

translate_last_digit(String, LastDigit, Rest) :-
	digits(Pairs),
	translate_last_digit_2(Pairs, String, LastDigit, Rest).

:- pred translate_last_digit_2(assoc_list(string, int)::in, string::in,
	int::out, string::out) is det.

translate_last_digit_2([], _, _, _) :-
	error("cannot determine last digit").
translate_last_digit_2([DigitStr - DigitNum | Digits], String, Last, Rest) :-
	( string__remove_suffix(String, DigitStr, RestPrime) ->
		Last = DigitNum,
		Rest = RestPrime
	;
		translate_last_digit_2(Digits, String, Last, Rest)
	).

:- func num_to_string(int) = string.

num_to_string(Int) = String :-
	translate_digits(Int, Digits),
	string__append_list(Digits, String).

:- pred translate_digits(int::in, list(string)::out) is det.

translate_digits(N, Digits) :-
	( N < 10 ->
		translate_digit(N, Digit),
		Digits = [Digit]
	;
		Last = N mod 10,
		Rest = N // 10,
		translate_digit(Last, LastDigit),
		translate_digits(Rest, RestDigits),
		list__append(RestDigits, [LastDigit], Digits)
	).

:- pred translate_digit(int::in, string::out) is det.

translate_digit(Int, String) :-
	( translate_digit_2(Int, StringPrime) ->
		String = StringPrime
	;
		error("translate_digit give non-digit")
	).

:- pred translate_digit_2(int, string).
:- mode translate_digit_2(in, out) is semidet.
:- mode translate_digit_2(out, out) is multi.

translate_digit_2(0, "zero").
translate_digit_2(1, "one").
translate_digit_2(2, "two").
translate_digit_2(3, "three").
translate_digit_2(4, "four").
translate_digit_2(5, "five").
translate_digit_2(6, "six").
translate_digit_2(7, "seven").
translate_digit_2(8, "eight").
translate_digit_2(9, "nine").

:- pred digits(assoc_list(string, int)::out) is det.
:- pragma memo(digits/1).

digits(PairList) :-
	solutions((pred(Pair::out) is multi :-
		translate_digit_2(Int, String),
		Pair = String - Int
	), PairList).
