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
	term_functor(term_string(""), [], _)).
get_test(2, "non-empty string", "\"foo\"",
	term_functor(term_string("foo"), [], _)).
get_test(3, "empty list", "[]",
	term_functor(term_atom("[]"), [], _)).
get_test(4, "singleton list", "[x]",
	term_functor(term_atom("."), [
		term_functor(term_atom("x"), [], _),
		term_functor(term_atom("[]"), [], _)
	], _)).
get_test(5, "long list", "[x,y,z]",
	term_functor(term_atom("."), [
		term_functor(term_atom("x"), [], _),
		term_functor(term_atom("."), [
			term_functor(term_atom("y"), [], _),
			term_functor(term_atom("."), [
				term_functor(term_atom("z"), [], _),
				term_functor(term_atom("[]"), [], _)
			], _)
		], _)
	], _)).
get_test(6, "list with tail", "[x|y]",
	term_functor(term_atom("."), [
		term_functor(term_atom("x"), [], _),
		term_functor(term_atom("y"), [], _)
	], _)).
get_test(7, "long list with tail", "[x,y|z]",
	term_functor(term_atom("."), [
		term_functor(term_atom("x"), [], _),
		term_functor(term_atom("."), [
			term_functor(term_atom("y"), [], _),
			term_functor(term_atom("z"), [], _)
		], _)
	], _)).
get_test(8, "integer", "3", term_functor(term_integer(3), [], _)).
get_test(9, "float", "3.0", term_functor(term_float(3.0), [], _)).
get_test(10, "braces", "{x}",
	term_functor(term_atom("{}"), [
		term_functor(term_atom("x"), [], _)
	], _)).
get_test(11, "braces pair", "{x,y}",
	term_functor(term_atom("{}"), [
		term_functor(term_atom(","), [
			term_functor(term_atom("x"), [], _),
			term_functor(term_atom("y"), [], _)
		], _)
	], _)).

get_test(12, "atom", "x",
	term_functor(term_atom("x"), [], _)).
get_test(13, "quoted atom", "'x'",
	term_functor(term_atom("x"), [], _)).
get_test(14, "operator", "-",
	term_functor(term_atom("-"), [], _)).
get_test(15, "random junk", "-----",
	term_functor(term_atom("-----"), [], _)).

get_test(16, "'[]'(x)", "'[]'(x)",
	term_functor(term_atom("[]"), [
		term_functor(term_atom("x"), [], _)
	], _)).
get_test(17, "variable", "X", term_variable(_)).

max_test(17).
