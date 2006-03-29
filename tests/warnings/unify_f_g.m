% This tests the warnings you should get when a conjunction unifies the same
% variable with different function symbols. It is a cut-down version of the
% predicate proc_info_has_io_state_pair_2 from hlds_pred.m, in which the bug
% marked below took me a long time to find.

:- module unify_f_g.
:- interface.

:- import_module list.
:- import_module maybe.

:- pred p(list(int)::in, maybe(int)::in, maybe(int)::out,
	maybe(int)::in, maybe(int)::out) is semidet.

:- implementation.

:- import_module int.

p([], !In, !Out).
p([H | T], !In, !Out) :-
	( H < 10 ->
		(
			!.In = no,
			!.In = yes(H)	% hard to see bug: !.In should be !:In
		;
			!.In = yes(_),
			fail
		)
	; H > 20 ->
		(
			!.Out = no,
			!:Out = yes(H)
		;
			!.Out = yes(_),
			fail
		)
	;
		fail
	),
	p(T, !In, !Out).
