% This is a regression test.
%
% The Mercury compiler of 26/10/1999 failed the first part of this test
% (the part concerned with the implied mode of append).
%
% The Mercury compiler of 30/3/2000 failed the second part of this test
% (the part with comparison_test1), due to overeager specialization of
% comparisons involving ENUM_USEREQ types.
%
% The Mercury compiler still fails the third part of this test (the part
% with comparison_test2) with --no-special-preds, because the exception
% is not propagated across MR_call_engine properly. (It should work fine
% with the default --special-preds.)

:- module user_defined_equality.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is cc_multi.

:- implementation.
:- import_module list, std_util, exception.

:- type foo ---> bar ; baz
	where equality is foo_equal.

foo_equal(_, _) :-
	semidet_succeed.

main -->
	( { append([bar], [baz], [baz, bar]) } ->
		io__write_string("yes\n")
	;
		io__write_string("no\n")
	),
	perform_comparison_test(comparison_test1),
	perform_comparison_test(comparison_test2).

:- pred perform_comparison_test(pred(T), io__state, io__state).
:- mode perform_comparison_test(pred(out) is det, di, uo) is cc_multi.

perform_comparison_test(Test) -->
	{ try(Test, TryResult) },
	(
		{ TryResult = failed },
		io__write_string("failed\n")
	;
		{ TryResult = succeeded(Result) },
		io__write_string("succeeded: "),
		io__write(Result),
		io__write_string("\n")
	;
		{ TryResult = exception(Exception) },
		io__write_string("threw exception: "),
		io__write(Exception),
		io__write_string("\n")
	).

:- pred comparison_test1(comparison_result::out) is det.

comparison_test1(R) :-
	compare(R, bar, baz).

:- pred comparison_test2(comparison_result::out) is det.

comparison_test2(R) :-
	compare(R, [bar], [baz]).
