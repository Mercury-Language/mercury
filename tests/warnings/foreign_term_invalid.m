%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%

    % These should issue warnings about termination pragmas conflicting
    % with foreign proc attributes.
:- module foreign_term_invalid.

:- interface.

:- pred test1(int::out) is det.
:- pred test2(int::out) is det.

:- implementation.

:- pragma does_not_terminate(test1/1).
:- pragma foreign_proc("C", test1(X::out),
    [will_not_call_mercury, promise_pure, thread_safe, terminates], "
    X = (MR_Integer) 3;
").
:- pragma foreign_proc("C#", test1(X::out),
    [will_not_call_mercury, promise_pure, thread_safe, terminates], "
    X = 3;
").
:- pragma foreign_proc("Java", test1(X::out),
    [will_not_call_mercury, promise_pure, thread_safe, terminates], "
    X = 3;
").

:- pragma terminates(test2/1).
:- pragma foreign_proc("C", test2(X::out),
    [may_call_mercury, promise_pure, thread_safe, does_not_terminate], "
    X = (MR_Integer) 3;
").
:- pragma foreign_proc("C#", test2(X::out),
    [may_call_mercury, promise_pure, thread_safe, does_not_terminate], "
    X = 3;
").
:- pragma foreign_proc("Java", test2(X::out),
    [may_call_mercury, promise_pure, thread_safe, does_not_terminate], "
    X = 3;
").

:- end_module foreign_term_invalid.
