%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% See foreign_proc_make.m
%

:- module foreign_proc_make_helper_1.

:- interface.

:- func f3 = int.

:- implementation.

:- pragma foreign_proc("C#", f3 = (X::out), [promise_pure], "X=5;").

f3 = 5.
