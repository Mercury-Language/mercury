%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% The .err_exp file is for --no-use-subdirs.
% The .err_exp2 file is for --use-subdirs.
%
%---------------------------------------------------------------------------%

:- module exported_unify.

:- interface.

:- pred unify_foo(T::in, T::in) is semidet.

:- implementation.

:- import_module exported_unify_helper_1.

unify_foo(A, A).
