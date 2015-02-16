%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% rotd-2011-12-03 nad before didn't recongise the require_semidet scope.

:- module require_bug.
:- interface.

:- pred foo(int::out, int::out) is failure.

:- implementation.

foo(Y, Z) :-
    require_det true,
    require_semidet semidet_succeed,
    require_multi (Y = 1; Y = 2),
    require_nondet ((Z = 1; Z = 2), semidet_succeed),
    require_failure false.
