:- module any_free_unify.

:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.


:- implementation.

:- import_module std_util, list, bool.

main -->
	{ test_any_free_unify([], Result1) },
	io__print(Result1), io__nl.

:- solver type foo.

:- pred test_any_free_unify(list(foo), bool).
:- mode test_any_free_unify(in(list_skel(any)), out) is det.

% In the unification in the condition of the if-then-else, the variable
% List has an inst which includes `any' components.  Normally, we can't
% check whether `any' has become further instantiated over a goal so we
% do not allow it in a negated context.  However, in this case, the
% `any' component is unified with `free' so we know that it cannot have
% become further instantiated.  Therefore we should allow the
% unification in the condition.

test_any_free_unify(List, Result) :-
	( List = [_ | _] ->
		Result = no
	;
		Result = yes
	).
