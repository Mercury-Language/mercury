%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is a regression test for a bug in dead procedure elimination
% of pseudo-imported preds.

:- module compl_unify_bug.

:- interface.

:- import_module list.

:- pred bug(int::in, list(T)::in) is semidet.

:- implementation.

bug(NextInputArgNum, InputArgs) :-
    list.drop(NextInputArgNum, InputArgs, [_ | _]).
