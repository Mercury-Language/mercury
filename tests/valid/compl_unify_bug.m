% This is a regression test for a bug in dead procedure elimination
% of pseudo-imported preds.
:- module compl_unify_bug.

:- interface.

:- import_module list.

:- pred bug(int::in, list(T)::in) is semidet.

:- implementation.

bug(NextInputArgNum, InputArgs) :-
	list__drop(NextInputArgNum, InputArgs, [_|_]).

