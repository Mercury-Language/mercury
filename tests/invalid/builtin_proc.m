%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Check that compiler/typecheck.m does not add a spurious
% "(the module `builtin' has not been imported)" to the
% end of the undefined pred error message.

:- module builtin_proc.

:- interface.

:- pred foo(int::in, int::out) is det.
:- pred bar(int::in, int::out) is det.

:- implementation.

foo(X, Y) :-
    builtin.no_such_pred(X, Y).

bar(X, Y) :-
    builtin.copy(X, Y).
