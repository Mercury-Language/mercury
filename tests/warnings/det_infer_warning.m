% Tests that abstractly_unify_bound_inst_list handles the case of
% different length bound_inst lists where all the cons_ids are the same
% correctly.
% Should give a warning that determinism of test2 is inferred det, and
% not give a warning at all for test.

:- module det_infer_warning.

:- interface.

:- type two ---> true;false.

:- pred test is semidet.
:- pred test2 is semidet.

:- implementation.

:- import_module require.

test :-
	(
		cond(true)
	->
		RA = true
	;
		RA = false
	),
	(
		cond(false)
	->
		RB = true
	;
		RB = false
	),
	RA = RB.

test2 :-
	RA = true,
	RB = true,
	RA = RB.

:- pred cond(two::in) is semidet.

cond(true).
