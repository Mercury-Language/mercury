% Test case for construct, num_functors, type_of and get_functor.
% 
% Author: trd

:- module construct_test.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module list, int, std_util, term, map, string, require.

:- pred test_builtins(io__state::di, io__state::uo) is det.
:- pred test_discriminated(io__state::di, io__state::uo) is det.
:- pred test_polymorphism(io__state::di, io__state::uo) is det.
:- pred test_other(io__state::di, io__state::uo) is det.
:- pred test_construct(io__state::di, io__state::uo) is det.

:- pred newline(io__state::di, io__state::uo) is det.

:- pred test_num_functors(type_desc::in, io__state::di, io__state::uo) is det.
:- pred test_nth_functor(type_desc::in, io__state::di, io__state::uo) is det.
:- pred test_all(T::in, io__state::di, io__state::uo) is det.

:- type enum	--->	one	;	two	;	three.

:- type fruit	--->	apple(list(int))
		;	banana(list(enum)).

:- type thingie	--->	foo ; bar(int) ; bar(int, int) ; qux(int) ;
			quux(int) ; quuux(int, int) ; wombat ; 
			zoom(int) ; zap(int, float) ; zip(int, int) ;
			zop(float, float).

:- type poly(A, B)	--->	poly_one(A) ; poly_two(B) ; 
				poly_three(B, A, poly(B, A));
				poly_four(A, B).

:- type no_tag		---> 	qwerty(int).

%----------------------------------------------------------------------------%

main -->
	test_discriminated,
	test_polymorphism,
	test_builtins, 
	test_other,
	test_construct.

%----------------------------------------------------------------------------%

test_construct -->

	% Valid tests.

		% Enumerations:

	test_construct_2(type_of(one), "three", 0, []),

	{ type_to_univ([1, 2, 3], NumList) },
	test_construct_2(type_of(apple([])), "apple", 1, [NumList]),

	{ type_to_univ([one, two, three], EnumList) },
	test_construct_2(type_of(apple([])), "banana", 1, [EnumList]),

		% Discriminated union:
		% (Simple, complicated and complicated constant tags).

	{ type_to_univ(1, One) },
	{ type_to_univ(2.1, TwoPointOne) },

	test_construct_2(type_of(wombat), "foo", 0, []),
	test_construct_2(type_of(wombat), "bar", 1, [One]),
	test_construct_2(type_of(wombat), "bar", 2, [One, One]),
	test_construct_2(type_of(wombat), "qux", 1, [One]),
	test_construct_2(type_of(wombat), "quux", 1, [One]),
	test_construct_2(type_of(wombat), "quuux", 2, [One, One]),
	test_construct_2(type_of(wombat), "wombat", 0, []),
	test_construct_2(type_of(wombat), "zoom", 1, [One]),
	test_construct_2(type_of(wombat), "zap", 2, [One, TwoPointOne]),
	test_construct_2(type_of(wombat), "zip", 2, [One, One]),
	test_construct_2(type_of(wombat), "zop", 2, [TwoPointOne, TwoPointOne]),

		% No-tag type:
	test_construct_2(type_of(qwerty(7)), "qwerty", 1, [One]),

	{ type_to_univ("goodbye", Bye) },

	test_construct_2(type_of(poly_four(3, "hello")), "poly_one", 1, [One]),
	test_construct_2(type_of(poly_four(3, "hello")), "poly_two", 1, [Bye]),
	test_construct_2(type_of(poly_four(3, "hello")), "poly_four", 2, 
		[One, Bye]),
	test_construct_2(type_of({1, "two", '3'}), "{}", 3,
		[univ(4), univ("five"), univ('6')]),

	io__write_string("About to call construct_tuple\n"),
	{ Tuple = construct_tuple([NumList, EnumList, One, TwoPointOne]) },
	io__write_string("Constructed tuple: "),
	io__write(Tuple),
	io__nl.

:- pred test_construct_2(type_desc::in, string::in, int::in, list(univ)::in,
	io__state::di, io__state::uo) is det.

test_construct_2(TypeInfo, FunctorName, Arity, Args) -->
	{ find_functor(TypeInfo, FunctorName, Arity, FunctorNumber) },
	io__write_string("About to construct "),
	io__write_string(FunctorName),
	io__write_string("/"),
	io__write_int(Arity),
	newline,
	( 
		{ Constructed = construct(TypeInfo, FunctorNumber, Args) }
	->
		io__write_string("Constructed: "),
		io__print(Constructed),
		newline
	;
		io__write_string("Construction failed.\n")
	).

:- pred find_functor(type_desc::in, string::in, int::in, int::out) is det.

find_functor(TypeInfo, Functor, Arity, FunctorNumber) :-
	N = num_functors(TypeInfo),
	find_functor2(TypeInfo, Functor, Arity, N, FunctorNumber).
	
:- pred find_functor2(type_desc::in, string::in, int::in, int::in, 
	int::out) is det.

find_functor2(TypeInfo, Functor, Arity, Num, FunctorNumber) :-
	(
		Num < 0
	->
		error("unable to find functor")
	;
		(
			get_functor(TypeInfo, Num, Functor, Arity, _List)
		->
			FunctorNumber = Num
		;
			find_functor2(TypeInfo, Functor, Arity, Num - 1,
				FunctorNumber)
		)
	).

%----------------------------------------------------------------------------%

test_all(T) -->
	{ TypeInfo = type_of(T) },
	test_num_functors(TypeInfo),
	test_nth_functor(TypeInfo), newline.

test_num_functors(TypeInfo) -->
	{ N = num_functors(TypeInfo) },
	io__write_int(N),
	io__write_string(" functors in this type"),
	newline.

test_nth_functor(TypeInfo) -->
	{ N = num_functors(TypeInfo) },
	test_all_functors(TypeInfo, N - 1).

:- pred test_all_functors(type_desc::in, int::in, 
	io__state::di, io__state::uo) is det.

test_all_functors(TypeInfo, N) -->
	(
		{ N < 0 }
	->
		[]
	;
		io__write_int(N),
		( 
			{ get_functor(TypeInfo, N, Name, Arity, _List) }
		->
			io__write_string(" - "),
			io__write_string(Name),
			io__write_string("/"),
			io__write_int(Arity),
			newline
		;
			io__write_string(" failed "),
			newline
		),
		test_all_functors(TypeInfo, N - 1)
	).

%----------------------------------------------------------------------------%

test_discriminated -->
	io__write_string("TESTING DISCRIMINATED UNIONS\n"),

		% test enumerations
	test_all(two), newline,
	test_all(one), newline,
	test_all(three), newline,

		% test simple tags
	test_all(apple([9,5,1])), newline,
	test_all(banana([three, one, two])), newline,

		% test complicated tags
	test_all(zop(3.3, 2.03)), newline,
	test_all(zip(3, 2)), newline,
	test_all(zap(3, -2.111)), newline,

		% test complicated constant

	test_all(wombat), newline,
	test_all(foo), newline,

	newline.	

test_polymorphism -->
	io__write_string("TESTING POLYMORPHISM\n"),
	test_all(poly_three(3.33, 4, poly_one(9.11))), newline,
	test_all(poly_two(3)), newline,
	test_all(poly_one([2399.3])), newline,

	newline.

test_builtins -->
	io__write_string("TESTING BUILTINS\n"),

		% test strings
 	test_all(""), newline,
 	test_all("Hello, world\n"), newline,
 	test_all("Foo%sFoo"), newline,
 	test_all(""""), newline,

		% test characters
	test_all('a'), newline,
	test_all('&'), newline,

		% test floats
	test_all(3.14159), newline,
	test_all(11.28324983E-22), newline,
	test_all(22.3954899E22), newline,

		% test integers
	test_all(-65), newline,
	test_all(4), newline,

		% test univ.
	%{ type_to_univ(["hi! I'm a univ!"], Univ) }, 
	% test_all(Univ), newline,
	
		% test predicates	
	test_all(newline), newline,

		% test tuples
	test_all({1, "a", 'a', {4, 'd'}}), newline,

	newline.

	% Note: testing abstract types is always going to have results
	% that are dependent on the implementation. If someone changes
	% the implementation, the results of this test can change.

test_other -->
	io__write_string("TESTING OTHER TYPES\n"),
	{ term__init_var_supply(VarSupply) },
	{ term__create_var(VarSupply, Var, NewVarSupply) },
	test_all(Var), newline,
	test_all(VarSupply), newline,
	test_all(NewVarSupply), newline,

		% presently, at least, map is an equivalence and
		% an abstract type.
	{ map__init(Map) },
	test_all(Map), newline,

		% a no tag type 
	test_all(qwerty(4)), newline,

	newline.

newline -->
	io__write_char('\n').
