%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% The .err_exp file is for non-debug grades without --intermod-opt.
% The .err_exp2 file is for all grades with --intermod-opt.
% The .err_exp3 file is for debug grades without --intermod-opt.
%
% The presence of --intermod-opt yields a kind of error message
% which makes the presence or absence of debugging irrelevant.
% However, without --intermod-opt, debug grades affect the error message
% by causing the implicit import of table_builtin.m (for I/O tabling),
% which causes the implicit import of io.m, which causes the implicit import
% of string.m.
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
