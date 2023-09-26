%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% The .err_exp file is for compiling without intermodule optimization.
% The .err_exp2 file is for compiling with intermodule optimization.

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
