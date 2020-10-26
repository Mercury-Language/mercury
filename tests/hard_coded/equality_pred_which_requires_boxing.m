%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% equality_pred_which_requires_boxing
%
% Check that the unification predicate we generate handles the case where
% the arguments are boxed and unboxed correctly.

:- module equality_pred_which_requires_boxing.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- type type_which_needs_boxing.
:- pragma foreign_type(c, type_which_needs_boxing, "double")
        where equality is unify_ft.
:- pragma foreign_type("C#", type_which_needs_boxing,
        "System.Double") where equality is unify_ft.
:- pragma foreign_type(java, type_which_needs_boxing,
        "Double") where equality is unify_ft.

:- type type_which_needs_boxing(T).
:- pragma foreign_type(c, type_which_needs_boxing(T), "double")
        where equality is unify_ft_T.
:- pragma foreign_type("C#", type_which_needs_boxing(T),
        "System.Double")
        where equality is unify_ft_T.
:- pragma foreign_type(java, type_which_needs_boxing(T), "Double")
        where equality is unify_ft_T.

main(!IO) :-
    % Test a builtin type which requires boxing.
    A = float_a,
    B = float_b,
    unify("float (boxed)", A, B, !IO),
    ( A = B ->
        io__write_string("float (unboxed): true\n", !IO)
    ;
        io__write_string("float (unboxed): false\n", !IO)
    ),

    % Test a simple foreign_type.
    X = create(A),
    Y = create(B),
    unify("foreign_type (boxed)", X, Y, !IO),
    ( X = Y ->
        io__write_string("foreign_type (unboxed): true\n", !IO)
    ;
        io__write_string("foreign_type (unboxed): false\n", !IO)
    ),

    % Test a foreign_type which requires a type_info.
    G = create_T(A),
    H = create_T(B),
    unify("foreign_type(T) (boxed)", G, H, !IO),
    ( G = H ->
        io__write_string("foreign_type(T) (unboxed): true\n", !IO)
    ;
        io__write_string("foreign_type(T) (unboxed): false\n", !IO)
    ).

    % Here we pass in a generic type, so if needed the input
    % arguments will have been boxed.  We then test if the
    % unification pred handles this correctly.
:- pred unify(string::in, T::in, T::in, io::di, io::uo) is det.
unify(S, X, Y, !IO) :-
    io__write_string(S, !IO),
    ( X = Y ->
        io__write_string(": true\n", !IO)
    ;
        io__write_string(": false\n", !IO)
    ).

:- func create(float) = type_which_needs_boxing.
:- pragma foreign_proc("C", create(X::in) = (Y::out), [promise_pure], "
    Y = X;
").
:- pragma foreign_proc("C#", create(X::in) = (Y::out), [promise_pure], "
    Y = X;
").
:- pragma foreign_proc("Java", create(X::in) = (Y::out), [promise_pure], "
    Y = X;
").

:- func create_T(float) = type_which_needs_boxing(int).
:- pragma foreign_proc("C", create_T(X::in) = (Y::out), [promise_pure], "
    Y = X;
").
:- pragma foreign_proc("C#", create_T(X::in) = (Y::out), [promise_pure], "
    Y = X;
").
:- pragma foreign_proc("Java", create_T(X::in) = (Y::out), [promise_pure], "
    Y = X;
").

:- pred unify_ft(type_which_needs_boxing::in, type_which_needs_boxing::in)
        is semidet.
:- pragma foreign_proc("C", unify_ft(X::in, Y::in), [promise_pure], "
    SUCCESS_INDICATOR = (X == Y);
").
:- pragma foreign_proc("C#", unify_ft(X::in, Y::in), [promise_pure], "
    SUCCESS_INDICATOR = (X == Y);
").
:- pragma foreign_proc("Java", unify_ft(X::in, Y::in), [promise_pure], "
    SUCCESS_INDICATOR = X.equals(Y);
").

:- pred unify_ft_T(type_which_needs_boxing(T)::in,
        type_which_needs_boxing(T)::in) is semidet.
:- pragma foreign_proc("C", unify_ft_T(X::in, Y::in), [promise_pure], "
    SUCCESS_INDICATOR = (X == Y);
").
:- pragma foreign_proc("C#", unify_ft_T(X::in, Y::in), [promise_pure], "
    SUCCESS_INDICATOR = (X == Y);
").
:- pragma foreign_proc("Java", unify_ft_T(X::in, Y::in), [promise_pure], "
    SUCCESS_INDICATOR = X.equals(Y);
").

:- pragma no_inline(float_a/0).
:- func float_a = float.
float_a = 1.0.

:- pragma no_inline(float_b/0).
:- func float_b = float.
float_b = 1.0.
