%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module erroneous_throw_promise.

:- interface.

:- pred foo(T::in) is erroneous.

:- pred bar(T::in) is erroneous.

:- implementation.

:- pragma foreign_proc("C",
    foo(_X::in),
    [will_not_call_mercury, promise_pure, will_not_throw_exception],
"
    /* Do something with X */
").

:- pragma foreign_proc("C",
    bar(_X::in),
    [may_call_mercury, promise_pure, will_not_throw_exception],
"
    /* Do something with X */
").

:- pragma foreign_proc("Java",
    foo(_X::in),
    [will_not_call_mercury, promise_pure, will_not_throw_exception],
"
    /* Do something with X */
").

:- pragma foreign_proc("Java",
    bar(_X::in),
    [may_call_mercury, promise_pure, will_not_throw_exception],
"
    /* Do something with X */
").

:- pragma foreign_proc("C#",
    foo(_X::in),
    [will_not_call_mercury, promise_pure, will_not_throw_exception],
"
    /* Do something with X */
").

:- pragma foreign_proc("C#",
    bar(_X::in),
    [may_call_mercury, promise_pure, will_not_throw_exception],
"
    /* Do something with X */
").
