%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test --warn-suspicious-foreign-procs for SUCCESS_INDICATOR.
%
%---------------------------------------------------------------------------%

:- module warn_succ_ind.
:- interface.

:- pred test_det(int::in, int::out) is det.
:- pred test_cc_multi(int::in, int::out) is cc_multi.

:- pred test_semidet(int::in, int::out) is semidet.
:- pred test_cc_nondet(int::in, int::out) is cc_nondet.

%---------------------------------------------------------------------------%
:- implementation.
%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    test_det(X::in, Y::out),
    [will_not_call_mercury, promise_pure],
"
    X = Y;
    SUCCESS_INDICATOR = MR_FALSE;
").
:- pragma foreign_proc("Java",
    test_det(X::in, Y::out),
    [will_not_call_mercury, promise_pure],
"
    X = Y;
    SUCCESS_INDICATOR = false;
").
:- pragma foreign_proc("C#",
    test_det(X::in, Y::out),
    [will_not_call_mercury, promise_pure],
"
    X = Y;
    SUCCESS_INDICATOR = MR_FALSE;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    test_cc_multi(X::in, Y::out),
    [will_not_call_mercury, promise_pure],
"
    X = Y;
    SUCCESS_INDICATOR = MR_FALSE;
").
:- pragma foreign_proc("Java",
    test_cc_multi(X::in, Y::out),
    [will_not_call_mercury, promise_pure],
"
    X = Y;
    SUCCESS_INDICATOR = false;
").
:- pragma foreign_proc("C#",
    test_cc_multi(X::in, Y::out),
    [will_not_call_mercury, promise_pure],
"
    X = Y;
    SUCCESS_INDICATOR = MR_FALSE;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    test_semidet(X::in, Y::out),
    [will_not_call_mercury, promise_pure],
"
    X = Y;
").
:- pragma foreign_proc("Java",
    test_semidet(X::in, Y::out),
    [will_not_call_mercury, promise_pure],
"
    X = Y;
").
:- pragma foreign_proc("C#",
    test_semidet(X::in, Y::out),
    [will_not_call_mercury, promise_pure],
"
    X = Y;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    test_cc_nondet(X::in, Y::out),
    [will_not_call_mercury, promise_pure],
"
    X = Y;
").
:- pragma foreign_proc("Java",
    test_cc_nondet(X::in, Y::out),
    [will_not_call_mercury, promise_pure],
"
    X = Y;
").
:- pragma foreign_proc("C#",
    test_cc_nondet(X::in, Y::out),
    [will_not_call_mercury, promise_pure],
"
    X = Y;
").

%---------------------------------------------------------------------------%
:- end_module warn_succ_ind.
%---------------------------------------------------------------------------%
