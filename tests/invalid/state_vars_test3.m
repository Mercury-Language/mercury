%------------------------------------------------------------------------------%
% state_vars_test3.m
% Ralph Becket <rafe@cs.mu.oz.au>
% Thu May 30 14:22:14 EST 2002
% vim: ft=mercury ff=unix ts=4 sw=4 et wm=0 tw=0
%
%------------------------------------------------------------------------------%

:- module state_vars_test3.

:- interface.

:- import_module int.

:- func f(int) = int.

:- implementation.

    % Illegally uses !Y as a func result.
    %
f(!X) = !Y.

