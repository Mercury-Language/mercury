%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% The .err_exp file is for non .par C grades.
% The .err_exp2 file is for .par C grades.
% The .err_exp3 file is for non-C grades.
%---------------------------------------------------------------------------%

:- module table_with_inline.

:- interface.

:- import_module int.

:- func foo(int) = int.

:- implementation.

:- pragma memo(foo/1).
:- pragma inline(foo/1).

foo(A) = A + A.
