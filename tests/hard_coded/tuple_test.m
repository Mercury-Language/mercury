%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module tuple_test.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module bool.
:- import_module char.
:- import_module int.
:- import_module list.
:- import_module solutions.
:- import_module string.
:- import_module term.
:- import_module term_conversion.
:- import_module term_io.
:- import_module type_desc.
:- import_module varset.

main(!IO) :-
    io.write_string("testing io__write:\n", !IO),
    Tuple = {'a', {'b', 1, [1, 2, 3], {}, {1}}, "string"},
    io.write_line(Tuple, !IO),
    io.write_line({}('a', {'b', 1, [1, 2, 3], {}, {1}}, "string"), !IO),

    io.write_string("testing type_to_term:\n", !IO),
    type_to_term(Tuple, TupleTerm),
    is_generic_term(TupleTerm),
    varset.init(VarSet),
    term_io.write_term(VarSet, TupleTerm, !IO),
    io.nl(!IO),

    io.write_string("testing term_to_type:\n", !IO),
    ( if
        has_type(NewTuple, type_of(Tuple)),
        term_to_type(TupleTerm, NewTuple)
    then
        io.write_string("term_to_type succeeded\n", !IO),
        io.write_line(NewTuple, !IO)
    else
        io.write_string("term_to_type failed\n", !IO)
    ),

    % Test in-in unification of tuples.
    io.write_string("testing unification:\n", !IO),
    ( if unify_tuple({1, 'a', 1, "string"}, {1, 'a', 1, "string"}) then
        io.write_string("unify test 1 succeeded\n", !IO)
    else
        io.write_string("unify test 1 failed\n", !IO)
    ),
    ( if unify_tuple({2, 'b', 1, "foo"}, {2, 'b', 1, "bar"}) then
        io.write_string("unify test 2 failed\n", !IO)
    else
        io.write_string("unify test 2 succeeded\n", !IO)
    ),

    % Test comparison of tuples.
    io.write_string("testing comparison:\n", !IO),
    compare(Res1, {1, 'a', 1, "string"}, {1, 'a', 1, "string"}),
    ( if Res1 = (=) then
        io.write_string("comparison test 1 succeeded\n", !IO)
    else
        io.write_string("comparison test 1 failed\n", !IO)
    ),

    compare(Res2, {2, 'b', 1, "foo"}, {2, 'b', 1, "bar"}),
    ( if Res2 = (>) then
        io.write_string("comparison test 2 succeeded\n", !IO)
    else
        io.write_string("comparison test 2 failed\n", !IO)
    ),

    io.write_string("testing tuple switches:\n", !IO),
    solutions(tuple_switch_test({1, 2}), Solns1),
    io.write_line(Solns1, !IO),

    tuple_switch_test_2({no, 3, 4}, Int),
    io.write_int(Int, !IO),
    io.nl(!IO),

    % These tests should generate an out-of-line unification
    % predicate for the unification with the output argument.
    %
    io.write_string("testing complicated unification\n",!IO),
    ( if choose(yes, {1, "b", 'c'}, {4, "e", 'f'}, {1, _, _}) then
        io.write_string("complicated unification test 1 succeeded\n", !IO)
    else
        io.write_string("complicated unification test 1 failed\n", !IO)
    ),
    ( if choose(yes, {5, "b", 'c'}, {9, "e", 'f'}, {1, _, _}) then
        io.write_string("complicated unification test 2 failed\n", !IO)
    else
        io.write_string("complicated unification test 2 succeeded\n", !IO)
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
