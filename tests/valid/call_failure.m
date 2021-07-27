%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module call_failure.
:- interface.

:- pred foo(int::in, int::out) is failure.

:- implementation.

:- import_module int.
:- import_module require.

foo(X, Y) :-
    ( if X = 42 then
        bar(X, Y0),
        Y = Y0 + 1
    else
        error("foo")
    ).

:- pred bar(int, int).
:- mode bar(in(bound(42)), out) is failure.
:- pragma no_inline(bar/2).

bar(42, _) :-
    fail.
