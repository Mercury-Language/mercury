%------------------------------------------------------------------------------%
% state_vars_test4.m
% Ralph Becket <rafe@cs.mu.oz.au>
% Thu May 30 14:22:14 EST 2002
% vim: ft=mercury ff=unix ts=4 sw=4 et wm=0 tw=0
%
%------------------------------------------------------------------------------%

:- module state_vars_test4.

:- interface.

:- import_module list, int.

:- func f(list(int)) = int.

:- implementation.

    % Illegally uses !X as a lambda arg.
    %
f(Xs) = foldl(func(!X) = !.X * !:X, Xs, 1).

