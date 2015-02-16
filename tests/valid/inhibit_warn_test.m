%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module inhibit_warn_test.

:- interface.

:- pred p(int).
:- mode p(out) is semidet. % determinism could be tighter

:- implementation.

p(1).
