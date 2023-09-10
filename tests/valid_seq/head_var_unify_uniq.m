%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This test was originally called intermod_nested_uniq.
%

:- module head_var_unify_uniq.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module head_var_unify_uniq_helper_1.

main(!IO) :-
    init(1, 1, Matrix),
    lookup(1, 1, Matrix, _).
