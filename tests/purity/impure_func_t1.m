%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module impure_func_t1.

:- interface.

:- import_module io.

:- pred main(state::di, state::uo) is det.

:- implementation.
:- import_module int.
:- import_module require.

:- pragma promise_pure(main/2).

main -->
    { impure X = get_counter },
    print("X = "),
    print(X),
    nl.

:- pragma foreign_decl("C", "extern MR_Integer counter;").
:- pragma foreign_code("C", "MR_Integer counter = 0;").

:- impure func get_counter = int is det.

get_counter = 0.
:- pragma foreign_proc("C",
    get_counter = (X::out),
    [will_not_call_mercury],
"
    X = counter;
").
