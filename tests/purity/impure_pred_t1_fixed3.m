%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module impure_pred_t1_fixed3.

:- interface.

:- import_module io.

:- impure pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int.
:- import_module require.
:- import_module list.

:- type foo
    --->    foo(impure pred(int, int)).

main(!IO) :-
    Y = foo(get_counter),
    impure main2(Y, !IO).

:- impure pred main2(foo::in(bound(foo(pred(in, out) is det))),
    io::di, io::uo) is det.

main2(Y, !IO) :-
    Y = foo(X),
    impure X(4, Z),
    io.print("X = ", !IO),
    io.print_line(Z, !IO).

:- pragma foreign_decl("C", "extern MR_Integer counter;").
:- pragma foreign_code("C", "MR_Integer counter = 0;").

:- impure pred get_counter(int::in, int::out) is det.

get_counter(X, X).
:- pragma foreign_proc("C",
    get_counter(Y::in, X::out),
    [will_not_call_mercury],
"
    X = counter + Y;
").
