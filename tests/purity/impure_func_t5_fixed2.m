%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module impure_func_t5_fixed2.

:- interface.

:- import_module io.

:- pred main(state::di, state::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.
:- import_module require.

:- type foo
    --->    foo(impure func(int) = int).

:- pragma promise_pure(main/2).

main -->
    { X = get_counter },
    print("X(4) = "),
    { impure X4 = impure_apply(X, 4) },
    print(X4), nl,
    print("X(4) = "),
    { impure X4b = impure_apply(X, 4) },
    print(X4b), nl.

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
