%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% The .err_exp file was for non-debug grades without --intermod-opt.
% The .err_exp2 file was for all grades with --intermod-opt.
% The .err_exp3 file was for debug grades without --intermod-opt.
% They differed when the "did you mean" part of the error message included
% a list of the visible one-character symbols, and --intermod-opt and
% debugging (through implicit import of table_builtin.m for I/O tabling)
% affected the list of such symbols.
%
% Now, the compiler generates more-specific text than the "did you mean"
% message, so we have only the .err_exp file.
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
