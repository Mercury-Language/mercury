% This tests the warnings you should get when a conjunction unifies the same
% variable with different function symbols.

:- module unify_f_g.
:- interface.

:- import_module list.
:- import_module std_util.

:- pred p(list(int)::in, maybe(int)::in, maybe(int)::out,
	maybe(int)::in, maybe(int)::out) is semidet.

:- implementation.

:- import_module int.

p([], !In, !Out).
p([H | T], !In, !Out) :-
	( H < 10 ->
		(
			!.In = no,
			!.In = yes(H)
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
