%-----------------------------------------------------------------------------%
% Regression test for a bug in higher_order.m
% Symptom: "Software Error: variable not found" during code generation.
% Cause: Prior to Feb 1998, lambda.m reordered the argument variables
% 	of lambda expressions so that inputs came before outputs.
%	higher_order.m was not doing the reordering before
%	replacing a higher_order_call with a call to the lambda
% 	expression.
%-----------------------------------------------------------------------------%
:- module ho_order.

:- interface.

:- import_module io.


:- pred main(io__state, io__state).
:- mode main(di, uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module int, list, map, maybe, string, pair.

%-----------------------------------------------------------------------------%

		% information used during the elimination phase.

main(State0, State) :-
	map__from_assoc_list([0 - 1], Needed),
	map__from_assoc_list([0 - 1, 1 - 2, 2 - 4], ProcTable0),
	ProcIds = [1, 2],
	Keep = no,
	fldl2(ho_order__eliminate_proc(Keep, Needed),
		ProcIds, ProcTable0, _ProcTable, State0, State).

		% eliminate a procedure, if unused

:- pred ho_order__eliminate_proc(maybe(int), map(int, int),
	int, map(int, int), map(int, int), io__state, io__state).
:- mode ho_order__eliminate_proc(in, in, in, in, out, di, uo) is det.

ho_order__eliminate_proc(Keep, Needed, ProcId, 
		ProcTable0, ProcTable) -->
	(
		( { map__search(Needed, ProcId, _) }
		; { Keep = yes(_) }
		)
	->
		{ ProcTable = ProcTable0 }
	;
		io__format("Deleting %i\n", [i(ProcId)]),
		{ map__delete(ProcTable0, ProcId, ProcTable) }
	).

:- pred fldl2(pred(X, Y, Y, Z, Z), list(X), Y, Y, Z, Z).
:- mode fldl2(pred(in, in, out, in, out) is det,
		in, in, out, in, out) is det.
:- mode fldl2(pred(in, in, out, di, uo) is det,
		in, in, out, di, uo) is det.
:- mode fldl2(pred(in, di, uo, di, uo) is det,
		in, di, uo, di, uo) is det.

fldl2(_, [], FirstAcc, FirstAcc, SecAcc, SecAcc).
fldl2(P, [H|T], FirstAcc0, FirstAcc, SecAcc0, SecAcc) :-
	call(P, H, FirstAcc0, FirstAcc1, SecAcc0, SecAcc1),
	fldl2(P, T, FirstAcc1, FirstAcc, SecAcc1, SecAcc).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
