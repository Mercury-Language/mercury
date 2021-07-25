%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module impure_func_t6.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int.
:- import_module require.

:- pragma promise_pure(main/2).

main(!IO) :-
    % A test of functions with arguments.
    impure X = get_counter(4),
    io.print("X = ", !IO),
    io.print_line(X, !IO).

:- pragma foreign_decl("C", "extern MR_Integer counter;").
:- pragma foreign_code("C", "MR_Integer counter = 0;").

:- impure func get_counter(int) = int.

get_counter(X) = X.
:- pragma foreign_proc("C",
    get_counter(Y::in) = (X::out),
    [will_not_call_mercury],
"
    X = counter + Y;
").

:- impure pred some_pred(int::in, int::out) is det.

some_pred(X, X).
:- pragma foreign_proc("C",
    some_pred(Y::in, X::out),
    [will_not_call_mercury],
"
    X = counter + Y;
").
