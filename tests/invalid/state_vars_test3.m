%------------------------------------------------------------------------------%
% state_vars_test3.m
% Ralph Becket <rafe@cs.mu.oz.au>
% Thu May 30 14:22:14 EST 2002
% vim: ft=mercury ff=unix ts=4 sw=4 et wm=0 tw=0
%
%------------------------------------------------------------------------------%

:- module state_vars_test3.

:- interface.

:- implementation.

:- import_module int.

:- pred p(int::out) is det.

    % Warning about referring to "uninitialized" !.X.
    %
p(!:X) :-
    !.X = 1.

