:- module tuple_test.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module bool.
:- import_module list.
:- import_module char.
:- import_module string.
:- import_module int.
:- import_module term.
:- import_module std_util.
:- import_module type_desc.
:- import_module term_io.
:- import_module varset.

main -->
	io__write_string("testing io__write:\n"),
	{ Tuple = {'a', {'b', 1, [1,2,3], {}, {1}}, "string"} },
	io__write(Tuple),
	io__nl,
	io__write({}('a', {'b', 1, [1,2,3], {}, {1}}, "string")),
	io__nl,
	
	io__write_string("testing type_to_term:\n"),
	{ type_to_term(Tuple, TupleTerm) },
	{ is_generic_term(TupleTerm) },
	{ varset__init(VarSet) },
	term_io__write_term(VarSet, TupleTerm),
	io__nl,

	io__write_string("testing term_to_type:\n"),
	(
		{ has_type(NewTuple, type_of(Tuple)) },
		{ term_to_type(TupleTerm, NewTuple) }
	->
		io__write_string("term_to_type succeeded\n"),
		io__write(NewTuple),
		io__nl
	;
		io__write_string("term_to_type failed\n")
	),

	% Test in-in unification of tuples.
	io__write_string("testing unification:\n"),
	( { unify_tuple({1, 'a', 1, "string"}, {1, 'a', 1, "string"}) } ->
		io__write_string("unify test 1 succeeded\n")
	;
		io__write_string("unify test 1 failed\n")
	),
 	( { unify_tuple({2, 'b', 1, "foo"}, {2, 'b', 1, "bar"}) } ->
		io__write_string("unify test 2 failed\n")
	;
		io__write_string("unify test 2 succeeded\n")
	),

	% Test comparison of tuples.
	io__write_string("testing comparison:\n"),
	{ compare(Res1, {1, 'a', 1, "string"}, {1, 'a', 1, "string"}) },
	( { Res1 = (=) } ->
		io__write_string("comparison test 1 succeeded\n")
	;
		io__write_string("comparison test 1 failed\n")
	),		

	{ compare(Res2, {2, 'b', 1, "foo"}, {2, 'b', 1, "bar"}) },
	( { Res2 = (>) } ->
		io__write_string("comparison test 2 succeeded\n")
	;
		io__write_string("comparison test 2 failed\n")
	),

	io__write_string("testing tuple switches:\n"),
	{ solutions(tuple_switch_test({1, 2}), Solns1) },
	io__write(Solns1),
	io__nl,

	{ tuple_switch_test_2({no, 3, 4}, Int) },
	io__write_int(Int),
	io__nl,

	%
	% These tests should generate an out-of-line unification
	% predicate for the unification with the output argument.
	%
	io__write_string("testing complicated unification\n"),
	( { choose(yes, {1, "b", 'c'}, {4, "e", 'f'}, {1, _, _}) } ->
		io__write_string("complicated unification test 1 succeeded\n")
	;
		io__write_string("complicated unification test 1 failed\n")
	),
	( { choose(yes, {5, "b", 'c'}, {9, "e", 'f'}, {1, _, _}) } ->
		io__write_string("complicated unification test 2 failed\n")
	;
		io__write_string("complicated unification test 2 succeeded\n")
	).

:- pred is_generic_term(term::unused).

is_generic_term(_).

:- type foo(A, B, C) == {int, A, B, C}.

:- pred unify_tuple(foo(A, B, C)::in,
	{int, A, B, C}::in(bound({ground, ground, ground, ground}))) is semidet.
:- pragma no_inline(unify_tuple/2).

unify_tuple(X, X).

:- pred choose(bool::in, T::in, T::in, T::out) is det.
:- pragma no_inline(choose/4).

choose(yes, A, _, A).
choose(no, _, B, B).

:- pred tuple_switch_test({int, int}::in, int::out) is multi.
:- pragma no_inline(tuple_switch_test/2).

tuple_switch_test({A, _}, A).
tuple_switch_test({_, B}, B).

:- pred tuple_switch_test_2({bool, int, int}::in, int::out) is det.
:- pragma no_inline(tuple_switch_test_2/2).

tuple_switch_test_2({yes, A, _}, A).
tuple_switch_test_2({no, _, B}, B).
