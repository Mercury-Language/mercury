%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test --warn-suspicious-foreign-procs for return statements.
%
%---------------------------------------------------------------------------%

:- module warn_return.
:- interface.

:- pred foo(int::in, int::out) is det.

:- implementation.

:- pragma foreign_proc("C",
    foo(X::in, Y::out),
    [will_not_call_mercury, promise_pure],
"
    X = Y;
    return;
").

:- pragma foreign_proc("C#",
    foo(X::in, Y::out),
    [will_not_call_mercury, promise_pure],
"
    X = Y;
    return;
").

:- pragma foreign_proc("Java",
    foo(X::in, Y::out),
    [will_not_call_mercury, promise_pure],
"
    X = Y;
    return;
").

:- end_module warn_return.
