%-----------------------------------------------------------------------------%
% A regression test.
% This tests a case where the compiler marked cells as being compile
% time garbage collectable, where references to that cell existed in
% other data structures.
%-----------------------------------------------------------------------------%
:- module interpret.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module exception, list, map, float, int, require.

:- type element
	--->	float(float)
	;	int(int).


:- type operation
	--->	addf
	;	addi
	;	float(float)
	;	lookup
	;	pop.

:- type stack == list(element).
:- type env == map(int, element).

main -->
	{ Env = map__set(map__init, 1, int(5)) },
	{ Stack0 = [int(1)] },
	{ Ops = [lookup, float(3.14)] },
	{ interpret(Ops, Env, Stack0, Stack1) },

		% This list must be of at least length two to as the
		% first cell is correctly marked as not being cgc'able.
	{ Stack1 = [float(_), int(X0)] ->
		X = X0
	;
		error("incorrect stack")
	},
		
		% XXX If int(X0) is incorrectly being marked as cgc'able
		% then P will reuse it's memory and hence the later
		% map__lookup will return int(3) instead of int(5).
	{ P = int(3) },
	io__write(P),
	io__nl,
	{ map__lookup(Env, 1, Q) },
	( { Q = int(X) } ->
		io__write_string("Element of map hasn't changed.\n")
	;
		io__write_string("BEEP! BEEP! Map changed!!!.\n")
	).

:- pred interpret(list(operation)::in, env::in, stack::in, stack::out) is det.

interpret([], _, Stack, Stack).
interpret([Op | Ops], Env, Stack0, Stack) :-
	do_op(Op, Env, Stack0, Stack1),
	interpret(Ops, Env, Stack1, Stack).


:- pred do_op(operation::in, env::in, stack::in, stack::out) is det.

do_op(float(F), _Env, Stack, [float(F) | Stack]).
do_op(addi, _Env, Stack0, Stack) :-
	( Stack0 = [int(A), int(B) | Stack1] ->
		Stack = [int(A+B) | Stack1]
	;
		throw(Stack0)
	).
do_op(addf, _Env, Stack0, Stack) :-
	( Stack0 = [float(A), float(B) | Stack1] ->
		Stack = [float(A+B) | Stack1]
	;
		error("addi: wrong arguments")
	).
do_op(lookup, Env, Stack0, Stack) :-
	( Stack0 = [int(Loc) | Stack1] ->
			% Here we create an alias between the Env
			% variable and the elements in the stack.
		map__lookup(Env, Loc, Element),
		Stack = [Element | Stack1]
	;
		error("lookup: wrong arguments")
	).
do_op(pop, _Env, Stack0, Stack) :-
	( Stack0 = [_ | Stack1] ->
		Stack = Stack1
	;
		error("pop: no arguments on the stack")
	).
