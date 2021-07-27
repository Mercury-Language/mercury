%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module spurious_purity_warning.
:- interface.

:- impure pred foo(int::out) is det.
:- impure pred bar(int::in) is det.

:- implementation.
:- import_module require.

foo(X::out) :-
    ( if semidet_succeed then
        error("foo/1")
    else
        X = 5
    ).

bar(_::in) :-
    ( if semidet_succeed then
        error("bar/1")
    else
        true
    ).

:- pragma foreign_proc("C", foo(X::out),
    [will_not_call_mercury, thread_safe],
"
    X = 0;
").

:- pragma foreign_proc("C", bar(_X::in),
    [will_not_call_mercury, thread_safe],
"
").
