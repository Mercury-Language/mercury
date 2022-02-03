%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This module tests the effect of the presence of misplaced foreign_proc
% pragmas in the interface on error messages about missing clauses
% for the named predicates and/or functions.
%

:- module foreign_proc_in_interface.

:- interface.

:- pred foo(int::in, int::out) is det.
:- func foo(int) = int.
:- pred bar(int::in, int::out) is det.
:- func bar(int) = int.

:- pragma foreign_proc("C",
    foo(In::in, Out::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Out = In;
").

:- pragma foreign_proc("C",
    bar(In::in) = (Out::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Out = In;
").

:- implementation.

% No definition of pred foo/2, but there is a misplaced foreign_proc above.
% No definition of func foo/1. Should get an error message.
% No definition of pred bar/2. Should get an error message.
% No definition of func bar/1, but there is a misplaced foreign_proc above.
