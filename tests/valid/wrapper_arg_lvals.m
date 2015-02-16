%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% ml_gen_wrapper_arg_lvals was incorrectly determining `unused' arguments to be
% output variables.  This led to a compiler abort when compiling this module to
% Java.
%
% Software Error:
%   assoc_list.from_corresponding_lists: lists have different lengths.
%     Key list type: list.list(ml_backend.mlds.mlds_type)
%     Key list length: 1
%     Value list type: list.list(ml_backend.mlds.mlds_rval)
%     Value list length: 2

:- module wrapper_arg_lvals.
:- interface.

:- typeclass tc(T, V) where [
    pred bananas(T::unused, V::out) is det
].

:- instance tc(int, string).

%---------------------------------------------------------------------------%

:- implementation.

:- instance tc(int, string) where [
    bananas(_, "no bananas today")
].
