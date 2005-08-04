:- module generic_call.

:- interface.

:- type what ---> double ; triple.

:- pred alpha(what::in, int::in, int::out) is det.

:- implementation.

:- import_module int.

alpha(What, In0, Out) :-
	force_deps(In0, In),
	(
		What = double,
		Op   = double_int
	;
		What = triple,
		Op   = triple_int
	),
	Op(In, Out).

:- pred double_int(int::in, int::out) is det.

double_int(X, X + X).

:- pred triple_int(int::in, int::out) is det.

triple_int(X, X + X + X).

:- pred force_deps(int::in, int::out) is det.

force_deps(!N) :- double_int(!N), triple_int(!N).
