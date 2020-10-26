%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test --warn-suspicious-foreign-procs for SUCCESS_INDICATOR.
%
% The .exp, .exp2, .exp3 files contain the expected warnings
% for the C, C# and Java foreign_procs respectively.
%
% XXX We should add all foreign_procs to the HLDS to enable checks like this,
% and only *after* these checks should we delete from the HLDS any
% foreign_procs that are not applicable to the current compiler target.
% That way, we could report the problems with *all* the foreign_procs,
% regardless of language, in a SINGLE invocation of the compiler.
% We would then need only a single .exp file.
%
%---------------------------------------------------------------------------%

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

:- end_module warn_succ_ind.
