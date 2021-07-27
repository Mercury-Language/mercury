%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% rotd-02-17 emitted a warning about the call to false never succeeding if
% compiling with --intermodule-optimization and --no-fully-strict.

:- module builtin_false.

:- interface.

:- pred foo(int::in, int::out) is semidet.

:- implementation.

foo(X, Y) :-
    ( if X = 3 then
        Y = 4
    else
        false
    ).
