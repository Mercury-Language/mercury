%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module impure_func_t5_fixed2.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.
:- import_module require.

:- type foo
    --->    foo(impure func(int) = int).

:- pragma promise_pure(main/2).

main(!IO) :-
    X = get_counter,
    io.print("X(4) = ", !IO),
    impure X4 = impure_apply(X, 4),
    io.print_line(X4, !IO),
    io.print("X(4) = ", !IO),
    impure X4b = impure_apply(X, 4),
    io.print_line(X4b, !IO).

:- pragma foreign_decl("C", "extern MR_Integer counter;").
:- pragma foreign_code("C", "MR_Integer counter = 42;").

:- impure func get_counter(int) = int is det.

get_counter(X) = X.
:- pragma foreign_proc("C",
    get_counter(Y::in) = (X::out),
    [will_not_call_mercury],
"
    X = counter + Y; counter++;
").
