% Test case for deconstruct and arg
% 
% Author: zs

:- module deconstruct_arg.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module array, list, string, std_util.

:- pred test_all(T::in, io__state::di, io__state::uo) is det.
:- pred test_functor(T::in, io__state::di, io__state::uo) is det.
:- pred test_arg(T::in, int::in, io__state::di, io__state::uo) is det.
:- pred test_deconstruct(T::in, io__state::di, io__state::uo) is det.
:- pred test_limited_deconstruct(T::in, int::in, io__state::di, io__state::uo)
	is det.
:- pred newline(io__state::di, io__state::uo) is det.

:- type enum	--->	one	;	two	;	three.

:- type fruit	--->	apple(list(int))
		;	banana(list(enum)).

:- type thingie	--->	foo ; bar(int) ; bar(int, int) ; qux(int) ;
			quux(int) ; quuux(int, int) ; wombat ; 
			zoom(int) ; zap(int, float, int) ;
			zip(int, int, int, int) ; zop(float, float).

:- type poly(A, B)	--->	poly_one(A) ; poly_two(B) ; 
				poly_three(B, A, poly(B, A)).

:- type no_tag		---> 	qwerty(int).

main -->
		% test enumerations
	% test_all(one), newline,
		% test primary tags
	test_all(apple([])), newline,
	test_all(apple([9,5,1])), newline,
		% test remote secondary tags
	test_all(zop(3.3, 2.03)), newline,
	test_all(zap(50, 51.0, 52)), newline,
	test_all(zip(50, 51, 52, 53)), newline,
		% test local secondary tags
	test_all(wombat), newline,
		% test notag
	test_all(qwerty(5)), newline,
		% test characters
	test_all('a'), newline,
		% test floats
	test_all(3.14159), newline,
		% test integers
	test_all(4), newline,
		% test univ.
	{ type_to_univ(["hi! I'm a univ!"], Univ) }, 
	test_all(Univ), newline,
		% test predicates	
	test_all(newline), newline,
		% test tuples
	test_all({1, 'b'}), newline,
	test_all({1, 'b', "third"}), newline,
	test_all({1, 'b', "third", {1,2,3,4}}), newline,
		% test arrays
	test_all(array([1000, 2000])), newline,
	test_all(array([100, 200, 300])), newline,
	test_all(array([10, 20, 30, 40])), newline.

test_all(T) -->
	test_functor(T),
	test_arg(T, 0),
	test_arg(T, 1),
	test_arg(T, 2),
	test_deconstruct(T),
	test_limited_deconstruct(T, 3).

test_functor(T) -->
	{ functor(T, Functor, Arity) },
	io__write_string(Functor),
	io__write_string("/"),
	io__write_int(Arity),
	io__write_string("\n").

test_arg(T, ArgNum) -->
	{ string__format("argument %d of ", [i(ArgNum)], Str) },
	io__write_string(Str),
	io__print(T),
	( { Argument = argument(T, ArgNum) } ->
		io__write_string(" is "),
		io__write_univ(Argument),
		io__write_string("\n")
	;
		io__write_string(" doesn't exist\n")
	).

test_deconstruct(T) -->
	{ deconstruct(T, Functor, Arity, Arguments) },
	{ string__format("deconstruct: functor %s arity %d\n",
		[s(Functor), i(Arity)], Str) },
	io__write_string(Str),
	io__write_string("["),
	io__write_list(Arguments, ", ", io__print),
	io__write_string("]\n").

test_limited_deconstruct(T, Limit) -->
	{ string__format("limited deconstruct %d of ", [i(Limit)], Str) },
	io__write_string(Str),
	io__print(T),
	io__write_string("\n"),
	( { limited_deconstruct(T, Limit, Functor, Arity, Arguments) } ->
		{ string__format("functor %s arity %d ",
			[s(Functor), i(Arity)], Str2) },
		io__write_string(Str2),
		io__write_string("["),
		io__write_list(Arguments, ", ", io__print),
		io__write_string("]\n")
	;
		io__write_string("failed\n")
	).

newline -->
	io__write_char('\n').
