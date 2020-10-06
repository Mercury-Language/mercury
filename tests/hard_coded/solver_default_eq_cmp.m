%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Check the generated equality/comparison predicates for solver types
% where they are not specified in the solver type definition.

:- module solver_default_eq_cmp.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module exception.
:- import_module string.
:- import_module unit.
:- import_module univ.

main(!IO) :-
    io.write_string("Checking equality predicate: ", !IO),
    try_io(test_eq, EqResult, !IO),
    (
        EqResult = succeeded(_),
        io.write_string("equality predicate not called", !IO)
    ;
        EqResult = exception(EqExcp),
        ( if univ_to_type(EqExcp, software_error(EqErr)) then
            io.write_string(EqErr ++ "\n", !IO)
        else
            io.write_string("unknown exception thrown\n", !IO)
        )
    ),
    io.write_string("Checking comparison predicate: ", !IO),
    try_io(test_cmp, CmpResult, !IO),
    (
        CmpResult = succeeded(_),
        io.write_string("comparison predicate not called", !IO)
    ;
        CmpResult = exception(CmpExcp),
        ( if univ_to_type(CmpExcp, software_error(CmpErr)) then
            io.write_string(CmpErr ++ "\n", !IO)
        else
            io.write_string("unknown exception thrown\n", !IO)
        )
    ).

:- pred test_eq(unit::out, io::di, io::uo) is det.

test_eq(unit, !IO) :-
    new_foo(300, X),
    new_foo(400, Y),
    write_solver_type_eq(X, Y, !IO).

:- pragma no_inline(write_solver_type_eq/4).
:- pred write_solver_type_eq(T::ia, T::ia, io::di, io::uo) is det.

write_solver_type_eq(X, Y, !IO) :-
    promise_pure (
        ( if X =  Y then
            io.write_string("Same\n", !IO)
        else
            io.write_string("Different\n", !IO)
        )
    ).

:- pred test_cmp(unit::out, io::di, io::uo) is det.

test_cmp(unit, !IO) :-
    new_foo(300, X),
    new_foo(400, Y),
    write_solver_type_cmp(X, Y, !IO).

:- pred write_solver_type_cmp(T::ia, T::ia, io::di, io::uo) is det.

write_solver_type_cmp(X0, Y0, !IO) :-
    X = unsafe_cast_any_to_ground(X0),
    Y = unsafe_cast_any_to_ground(Y0),
    compare(Res, X, Y),
    io.write(Res, !IO).

:- solver type foo
    where   representation is int.

:- pred new_foo(int::in, foo::oa) is det.

new_foo(X, Y) :-
    promise_pure (
        impure Y = 'representation to any foo/0'(X)
    ).

%---------------------------------------------------------------------------%
:- end_module solver_default_eq_cmp.
%---------------------------------------------------------------------------%
