% Test case for io__write_binary/io__read_binary.
% (adapted from tests/hard_coded/write.m, which tests io__write).

% XXX currently we do not pass the test of "univ"!

:- module write_binary.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is cc_multi.

:- implementation.

:- import_module char, list, int, std_util, term, map, array, exception.

:- pred test_ops(io__state::di, io__state::uo) is cc_multi.
:- pred test_builtins(io__state::di, io__state::uo) is cc_multi.
:- pred test_discriminated(io__state::di, io__state::uo) is cc_multi.
:- pred test_polymorphism(io__state::di, io__state::uo) is cc_multi.
:- pred test_other(io__state::di, io__state::uo) is cc_multi.
:- pred do_test(T::in, io__state::di, io__state::uo) is cc_multi.
:- pred do_test_2(T::in, T::out, io__state::di, io__state::uo) is det.


:- type enum	--->	one	;	two	;	three.

:- type fruit	--->	apple(list(int))
		;	banana(list(enum)).

:- type thingie	--->	foo ; bar(int) ; bar(int, int) ; qux(int) ;
			quux(int) ; quuux(int, int) ; wombat ; 
			zoom(int) ; zap(int, float) ; zip(int, int) ;
			zop(float, float).

:- type poly(A, B)	--->	poly_one(A) ; poly_two(B) ; 
				poly_three(B, A, poly(B, A)).

:- type no_tag		---> 	qwerty(int).

:- type expr		--->	var(string)
			;	int(int)
			;	expr + expr
			;	expr - expr
			; 	expr * expr
			;	(expr, expr)
			;	{expr; expr}
			;	{{expr}}
			;	(type)
			;	blah
			;	(:-)
			.

main -->
	test_ops,
	test_discriminated,
	test_polymorphism,
	test_builtins, 
	test_other.


test_ops -->
	io__write_string("TESTING TERMS WITH OPERATORS\n"),
	do_test(var("X") + int(3) * var("X^2") ; (type)),
	do_test(write_binary.{type}),
	do_test(write_binary.{:-}),
	do_test((:-)),
	do_test(write_binary.{blah}),
	do_test((blah ; (type), (type) * blah ; (type))),
	do_test(((blah ; blah), blah) * blah ; blah),
	do_test((type) * blah ; (type)).

test_discriminated -->
	io__write_string("TESTING DISCRIMINATED UNIONS\n"),

		% test enumerations
	do_test(one),
	do_test(two),
	do_test(three),

		% test simple tags
	do_test(apple([9,5,1])),
	do_test(banana([three, one, two])),

		% test complicated tags
	do_test(zop(3.3, 2.03)),
	do_test(zip(3, 2)),
	do_test(zap(3, -2.111)),

		% test complicated constant
	do_test(wombat),
	do_test(foo).

test_polymorphism -->
	io__write_string("TESTING POLYMORPHISM\n"),
	do_test(poly_one([2399.3]) `with_type` poly(list(float), int)),
	do_test(poly_two(3) `with_type` poly(list(float), int)),
	do_test(poly_three(3.33, 4, poly_one(9.11))).


test_builtins -->
	io__write_string("TESTING BUILTINS\n"),

		% test strings
 	do_test(""),
 	do_test("Hello, world\n"),
 	do_test("Foo%sFoo"),
 	do_test(""""),

		% test characters
	do_test('a' `with_type` char),
	do_test('&' `with_type` char),
	do_test('.' `with_type` char),
	do_test('%' `with_type` char),
	do_test(' ' `with_type` char),
	do_test('\t' `with_type` char),
	do_test('\n' `with_type` char),
	do_test(('\\') `with_type` char),
	do_test('*' `with_type` char),
	do_test('/' `with_type` char),

		% test floats
	do_test(3.14159),
	do_test(11.28324983E-22),
	do_test(22.3954899E22),

		% test integers
	do_test(-65),
	do_test(4),

%%% XXX currently we do not pass this test!
%%%		% test univ.
%%%	{ type_to_univ(["hi! I'm a univ!"], Univ) }, 
%%%	do_test(Univ),
	
		% test predicates	
		% io__read_binary doesn't work for higher-order terms,
		% so this test is expected to fail.
	io__write_string("next text is expected to fail:\n"),
	do_test(do_test `with_type` pred(int, io, io)),

	{ true }.

test_other -->
	io__write_string("TESTING OTHER TYPES\n"),
	{ term__init_var_supply(VarSupply `with_type` var_supply(generic)) },
	{ term__create_var(VarSupply, Var, NewVarSupply) },
	do_test(Var),
	do_test(VarSupply),
	do_test(NewVarSupply),

		% presently, at least, map is an equivalence and
		% an abstract type.
	{ map__init(Map `with_type` map(int, string)) },
	do_test(Map),

		% a no tag type 
	do_test(qwerty(4)),

	{ array__from_list([1,2,3,4], Array) },
	do_test(Array).

do_test(Term) -->
	try_io(do_test_2(Term), Result),
	( { Result = succeeded(TermRead) } ->
		io__print("test passed:\n"),
		io__print(Term), io__nl,
		io__print(TermRead), io__nl
	;
		io__print("test failed:\n"),
		io__print(Result), io__nl,
		io__print(Term), io__nl
	).

do_test_2(Term, TermRead) -->
	io__make_temp(FileName),
	io__open_binary_output(FileName, OutputRes),
	( { OutputRes = ok(OutputStream) } ->
		io__write_byte(OutputStream, 42),
		io__write_binary(OutputStream, Term),
		io__write_byte(OutputStream, 43),
		io__close_binary_output(OutputStream),
		io__open_binary_input(FileName, InputRes),
		( { InputRes = ok(InputStream) } ->
			io__read_byte(InputStream, B42),
			io__read_binary(InputStream, Result),
			io__read_byte(InputStream, B43),
			io__close_binary_input(InputStream),
			(
				{ B42 = ok(42) },
				{ B43 = ok(43) },
				{ Result = ok(TermRead0) },
				{ TermRead0 = Term }
			->
				io__remove_file(FileName, _),
				io__print("ok... "),
				{ TermRead = TermRead0 }
			;
				io__remove_file(FileName, _),
				{ throw("error reading term back in again") }
			)
		;
			io__remove_file(FileName, _),
			{ throw(InputRes) }
		)
	;
		io__remove_file(FileName, _),
		{ throw(OutputRes) }
	).
