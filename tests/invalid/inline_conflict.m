%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module inline_conflict.

:- interface.

:- pred foo(int::in, int::out) is det.

:- implementation.

foo(X, Y) :-
    bar(X, Y).

:- pred inline_conflict.bar(int::in, int::out) is det.

:- pragma inline(bar/2).
:- pragma no_inline(bar/2).

bar(X, X).
