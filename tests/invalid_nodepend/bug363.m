%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module bug363.
:- interface.

:- func foo = int.

:- implementation.

foo = 42.

:- pragma foreign_proc("C",
    foo = (F::out),
    [will_not_call_mercury, promise_pure thread_safe, will_not_modify_trail,
        does_not_affect_liveness, bad_attr],
"
    F = 561;
").
