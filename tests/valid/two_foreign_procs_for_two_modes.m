%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% A test for the case where we have foreign_procs for
% different modes of the same predicate.
%

:- module two_foreign_procs_for_two_modes.

:- interface.

:- pred c_int_unify(int, int).
:- mode c_int_unify(in, out) is det.
:- mode c_int_unify(out, in) is det.

:- implementation.

:- pragma promise_pure(c_int_unify/2).

:- pragma foreign_proc("C",
    c_int_unify(Int0::in, Int::out),
    [promise_pure, will_not_call_mercury],
"
    Int = Int0;
").

:- pragma foreign_proc("C",
    c_int_unify(Int::out, Int0::in),
    [promise_pure, will_not_call_mercury],
"
    Int = Int0;
").

c_int_unify(X, X).
