%------------------------------------------------------------------------------%
% state_vars_test1.m
% Ralph Becket <rafe@cs.mu.oz.au>
% Thu May 30 14:22:14 EST 2002
% vim: ft=mercury ff=unix ts=4 sw=4 et wm=0 tw=0
%
%------------------------------------------------------------------------------%

:- module state_vars_test1.

:- interface.

:- import_module int.

:- pred p(int::in, int::out) is det.

:- implementation.

    % Illegally refers to !:X in an if-then-else expr.
    %
p(!X) :-
    !:X = !.X +
                ( if   max(0, !.X, !:X)
                  then 1
                  else 2
                ).

