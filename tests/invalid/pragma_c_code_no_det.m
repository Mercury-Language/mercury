%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% The .err_exp{,2,3} files are for targeting C, Java and C# respectively.
%---------------------------------------------------------------------------%

:- module pragma_c_code_no_det.

:- interface.

:- pred test(int::out) is det.

:- implementation.

test(Int) :-
    fproc(Int).

:- pred fproc(int::out).

:- pragma foreign_proc("C",
    fproc(X::out),
    [will_not_call_mercury, promise_pure],
"
    X = 1;
").
:- pragma foreign_proc("Java",
    fproc(X::out),
    [will_not_call_mercury, promise_pure],
"
    X = 1;
").
:- pragma foreign_proc("C#",
    fproc(X::out),
    [will_not_call_mercury, promise_pure],
"
    X = 1;
").
