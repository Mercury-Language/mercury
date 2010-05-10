% vim: ft=mercury ts=4 sw=4 et

% Test --warn-suspicious-foreign-procs for SUCCESS_INDICATOR

:- module warn_succ_ind.
:- interface.

:- pred foo(int::in, int::out) is det.
:- pred foo2(int::in, int::out) is cc_multi.

:- pred bar(int::in, int::out) is semidet.
:- pred bar2(int::in, int::out) is cc_nondet.

:- implementation.

:- pragma foreign_proc("C",
    foo(X::in, Y::out),
    [will_not_call_mercury, promise_pure],
"
    X = Y;
    SUCCESS_INDICATOR = MR_FALSE;
").

:- pragma foreign_proc("C#",
    foo(X::in, Y::out),
    [will_not_call_mercury, promise_pure],
"
    X = Y;
    SUCCESS_INDICATOR = MR_FALSE;
").

:- pragma foreign_proc("Java",
    foo(X::in, Y::out),
    [will_not_call_mercury, promise_pure],
"
    X = Y;
    SUCCESS_INDICATOR = false;
").

:- pragma foreign_proc("Erlang",
    foo(X::in, Y::out),
    [will_not_call_mercury, promise_pure],
"
    X = Y,
    SUCCESS_INDICATOR = false
").

:- pragma foreign_proc("C",
    foo2(X::in, Y::out),
    [will_not_call_mercury, promise_pure],
"
    X = Y;
    SUCCESS_INDICATOR = MR_FALSE;
").

:- pragma foreign_proc("C#",
    foo2(X::in, Y::out),
    [will_not_call_mercury, promise_pure],
"
    X = Y;
    SUCCESS_INDICATOR = MR_FALSE;
").

:- pragma foreign_proc("Java",
    foo2(X::in, Y::out),
    [will_not_call_mercury, promise_pure],
"
    X = Y;
    SUCCESS_INDICATOR = false;
").

:- pragma foreign_proc("Erlang",
    foo2(X::in, Y::out),
    [will_not_call_mercury, promise_pure],
"
    X = Y,
    SUCCESS_INDICATOR = false
").

:- pragma foreign_proc("C",
    bar(X::in, Y::out),
    [will_not_call_mercury, promise_pure],
"
    X = Y;
").

:- pragma foreign_proc("C#",
    bar(X::in, Y::out),
    [will_not_call_mercury, promise_pure],
"
    X = Y;
").

:- pragma foreign_proc("Java",
    bar(X::in, Y::out),
    [will_not_call_mercury, promise_pure],
"
    X = Y;
").

:- pragma foreign_proc("Erlang",
    bar(X::in, Y::out),
    [will_not_call_mercury, promise_pure],
"
    X = Y
").

:- pragma foreign_proc("C",
    bar2(X::in, Y::out),
    [will_not_call_mercury, promise_pure],
"
    X = Y;
").

:- pragma foreign_proc("C#",
    bar2(X::in, Y::out),
    [will_not_call_mercury, promise_pure],
"
    X = Y;
").

:- pragma foreign_proc("Java",
    bar2(X::in, Y::out),
    [will_not_call_mercury, promise_pure],
"
    X = Y;
").

:- pragma foreign_proc("Erlang",
    bar2(X::in, Y::out),
    [will_not_call_mercury, promise_pure],
"
    X = Y
").

:- end_module warn_succ_ind.
