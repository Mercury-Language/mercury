%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This test case extends and effectively replaces the old multiply_star
% test case.
%
%---------------------------------------------------------------------------%

:- module int_ops.

:- interface.

:- pred p(int::in, int::out) is det.

:- implementation.

% :- import_module int.         % This import is missing.

p(A, Z) :-
    ( if A < 0 then             % comparison
        Z = A << 3              % bitwise operation
    else if A >= 2 then         % comparison
        Z = A * 2               % int and float arithmetic
    else
        Z = A // 2              % int only arithmetic
    ).

