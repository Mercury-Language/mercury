%-----------------------------------------------------------------------------%
% Test case for functor, arg, deconstruct and their variants.
% 
% Author: zs

:- module deconstruct_arg.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is cc_multi.

:- implementation.

%-----------------------------------------------------------------------------%

:- import_module array, list, string, std_util, deconstruct.

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

:- type set(T) --->	set_rep(list(T)) where equality is set_equal.

%-----------------------------------------------------------------------------%

% convert list to set
:- func set(list(T)) = set(T).

set(List) = set_rep(List).

% convert set to sorted list
:- func set_to_sorted_list(set(T)) = list(T).

set_to_sorted_list(Set) =
	promise_only_solution((pred(Sorted::out) is cc_multi :-
		Set = set_rep(Unsorted),
		list__sort_and_remove_dups(Unsorted, Sorted)
	)).

:- pred set_equal(set(T)::in, set(T)::in) is semidet.

set_equal(Set1, Set2) :-
	set_to_sorted_list(Set1) = set_to_sorted_list(Set2).

%-----------------------------------------------------------------------------%

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
		% test noncanonical type
	test_all(set([1,2,3,3])), newline,
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

%-----------------------------------------------------------------------------%

:- pred test_all(T::in, io__state::di, io__state::uo) is cc_multi.

test_all(T) -->
	test_std_util_functor(T),
	test_deconstruct_functor(T),
	test_std_util_arg(T, 0),
	test_deconstruct_arg(T, 0),
	test_std_util_arg(T, 1),
	test_deconstruct_arg(T, 1),
	test_std_util_arg(T, 2),
	test_deconstruct_arg(T, 2),
	test_std_util_deconstruct(T),
	test_deconstruct_deconstruct(T),
	test_std_util_limited_deconstruct(T, 3),
	test_deconstruct_limited_deconstruct(T, 3).

%-----------------------------------------------------------------------------%

:- pred test_std_util_functor(T::in, io__state::di, io__state::uo) is det.

test_std_util_functor(T) -->
	io__write_string("std_util    functor: "),
	{ std_util__functor(T, Functor, Arity) },
	io__write_string(Functor),
	io__write_string("/"),
	io__write_int(Arity),
	io__write_string("\n").

:- pred test_deconstruct_functor(T::in, io__state::di, io__state::uo)
	is cc_multi.

test_deconstruct_functor(T) -->
	io__write_string("deconstruct functor: "),
	{ deconstruct__functor(T, include_details_cc, Functor, Arity) },
	io__write_string(Functor),
	io__write_string("/"),
	io__write_int(Arity),
	io__write_string("\n").

:- pred test_std_util_arg(T::in, int::in, io__state::di, io__state::uo) is det.

test_std_util_arg(T, ArgNum) -->
	{ string__format("std_util    argument %d of ", [i(ArgNum)], Str) },
	io__write_string(Str),
	io__print(T),
	( { Argument = std_util__argument(T, ArgNum) } ->
		io__write_string(" is "),
		io__write_univ(Argument),
		io__write_string("\n")
	;
		io__write_string(" doesn't exist\n")
	).

:- pred test_deconstruct_arg(T::in, int::in, io__state::di, io__state::uo)
	is cc_multi.

test_deconstruct_arg(T, ArgNum) -->
	{ string__format("deconstruct argument %d of ", [i(ArgNum)], Str) },
	io__write_string(Str),
	io__print(T),
	( { deconstruct__arg(T, include_details_cc, ArgNum, Arg) } ->
		io__write_string(" is "),
		io__write(Arg),
		io__write_string("\n")
	;
		io__write_string(" doesn't exist\n")
	).

:- pred test_std_util_deconstruct(T::in, io__state::di, io__state::uo) is det.

test_std_util_deconstruct(T) -->
	{ std_util__deconstruct(T, Functor, Arity, Arguments) },
	{ string__format("std_util    deconstruct: functor %s arity %d\n",
		[s(Functor), i(Arity)], Str) },
	io__write_string(Str),
	io__write_string("["),
	io__write_list(Arguments, ", ", io__print),
	io__write_string("]\n").

:- pred test_deconstruct_deconstruct(T::in, io__state::di, io__state::uo)
	is cc_multi.

test_deconstruct_deconstruct(T) -->
	{ deconstruct__deconstruct(T, include_details_cc,
		Functor, Arity, Arguments) },
	{ string__format("deconstruct deconstruct: functor %s arity %d\n",
		[s(Functor), i(Arity)], Str) },
	io__write_string(Str),
	io__write_string("["),
	io__write_list(Arguments, ", ", io__print),
	io__write_string("]\n").

:- pred test_std_util_limited_deconstruct(T::in, int::in,
	io__state::di, io__state::uo) is det.

test_std_util_limited_deconstruct(T, Limit) -->
	{ string__format("std_util    limited deconstruct %d of ",
		[i(Limit)], Str) },
	io__write_string(Str),
	io__print(T),
	io__write_string("\n"),
	(
		{ std_util__limited_deconstruct(T,
			Limit, Functor, Arity, Arguments) }
	->
		{ string__format("functor %s arity %d ",
			[s(Functor), i(Arity)], Str2) },
		io__write_string(Str2),
		io__write_string("["),
		io__write_list(Arguments, ", ", io__print),
		io__write_string("]\n")
	;
		io__write_string("failed\n")
	).

:- pred test_deconstruct_limited_deconstruct(T::in, int::in,
	io__state::di, io__state::uo) is cc_multi.

test_deconstruct_limited_deconstruct(T, Limit) -->
	{ string__format("deconstruct limited deconstruct %d of ",
		[i(Limit)], Str) },
	io__write_string(Str),
	io__print(T),
	io__write_string("\n"),
	(
		{ deconstruct__limited_deconstruct(T, include_details_cc,
			Limit, Functor, Arity, Arguments) }
	->
		{ string__format("functor %s arity %d ",
			[s(Functor), i(Arity)], Str2) },
		io__write_string(Str2),
		io__write_string("["),
		io__write_list(Arguments, ", ", io__print),
		io__write_string("]\n")
	;
		io__write_string("failed\n")
	).

%-----------------------------------------------------------------------------%

:- pred newline(io__state::di, io__state::uo) is det.

newline -->
	io__write_char('\n').
