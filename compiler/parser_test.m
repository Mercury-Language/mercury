%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% This file contains some test cases for the parser,
% together with a simple Prolog test harness.

run_tests :-
	max_test(Max),
	between(1, Max, TestNum),
	test(TestNum, Name, _String, _Term, _ExpectedTerm, Result),
	format("Test ~d (~s) ~a.\n", [TestNum, Name, Result]),
	fail.
run_tests.

test(TestNum, Name, String, Term, ExpectedTerm, Result) :-
	get_test(TestNum, Name, String, ExpectedTerm),
	mercury__sread(String, Term),
	( Term = ExpectedTerm ->
		Result = succeeded
	;
		Result = failed
	).

get_test(1, "empty string", "\"\"",
	term__functor(term__string(""), [], _)).
get_test(2, "non-empty string", "\"foo\"",
	term__functor(term__string("foo"), [], _)).
get_test(3, "empty list", "[]",
	term__functor(term__atom("[]"), [], _)).
get_test(4, "singleton list", "[x]",
	term__functor(term__atom("."), [
		term__functor(term__atom("x"), [], _),
		term__functor(term__atom("[]"), [], _)
	], _)).
get_test(5, "long list", "[x,y,z]",
	term__functor(term__atom("."), [
		term__functor(term__atom("x"), [], _),
		term__functor(term__atom("."), [
			term__functor(term__atom("y"), [], _),
			term__functor(term__atom("."), [
				term__functor(term__atom("z"), [], _),
				term__functor(term__atom("[]"), [], _)
			], _)
		], _)
	], _)).
get_test(6, "list with tail", "[x|y]",
	term__functor(term__atom("."), [
		term__functor(term__atom("x"), [], _),
		term__functor(term__atom("y"), [], _)
	], _)).
get_test(7, "long list with tail", "[x,y|z]",
	term__functor(term__atom("."), [
		term__functor(term__atom("x"), [], _),
		term__functor(term__atom("."), [
			term__functor(term__atom("y"), [], _),
			term__functor(term__atom("z"), [], _)
		], _)
	], _)).
get_test(8, "integer", "3", term__functor(term__integer(3), [], _)).
get_test(9, "float", "3.0", term__functor(term__float(3.0), [], _)).
get_test(10, "braces", "{x}",
	term__functor(term__atom("{}"), [
		term__functor(term__atom("x"), [], _)
	], _)).
get_test(11, "braces pair", "{x,y}",
	term__functor(term__atom("{}"), [
		term__functor(term__atom(","), [
			term__functor(term__atom("x"), [], _),
			term__functor(term__atom("y"), [], _)
		], _)
	], _)).

get_test(12, "atom", "x",
	term__functor(term__atom("x"), [], _)).
get_test(13, "quoted atom", "'x'",
	term__functor(term__atom("x"), [], _)).
get_test(14, "operator", "-",
	term__functor(term__atom("-"), [], _)).
get_test(15, "random junk", "-----",
	term__functor(term__atom("-----"), [], _)).

get_test(16, "'[]'(x)", "'[]'(x)",
	term__functor(term__atom("[]"), [
		term__functor(term__atom("x"), [], _)
	], _)).
get_test(17, "variable", "X", term__variable(_)).

max_test(17).
