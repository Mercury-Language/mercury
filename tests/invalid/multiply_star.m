%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%

:- module multiply_star.

:- interface.

:- pred p(int::in, int::out) is det.

:- implementation.

% :- import_module int.         % This import is missing.

p(A, Z) :-
    ( if A = 0 then
        Z = 1
    else
        Z = A * 2
    ).
