%-----------------------------------------------------------------------------%
% Regression test for a bug in higher_order.m
% Symptom: seg fault or incorrect behaviour at runtime
% Cause: Incorrect ordering of curried arguments to multiple known
% higher-order input arguments in the specialised version.
%-----------------------------------------------------------------------------%
:- module ho_order2.

:- interface.

:- import_module io.

:- pred main(io__state, io__state).
:- mode main(di, uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module int, list, map, require, maybe, string, pair.

%-----------------------------------------------------------------------------%

		% information used during the elimination phase.

main(State0, State) :-
	map__from_assoc_list([0 - 1], Needed),
	map__from_assoc_list([0 - 1, 1 - 2, 2 - 4], ProcTable0),
	ProcIds = [1, 2],
	Keep = no,
	fldl2(ho_order2__eliminate_proc(Keep, Needed),
		ho_order2__eliminate_proc_2(Needed, Keep),
		ProcIds, ProcTable0, _ProcTable, State0, State).

		% eliminate a procedure, if unused

:- pred ho_order2__eliminate_proc(maybe(int), map(int, int),
	int, map(int, int), map(int, int), io__state, io__state).
:- mode ho_order2__eliminate_proc(in, in, in, in, out, di, uo) is det.

ho_order2__eliminate_proc(Keep, Needed, ProcId, 
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

:- pred ho_order2__eliminate_proc_2(map(int, int), maybe(int),
	int, map(int, int), map(int, int), io__state, io__state).
:- mode ho_order2__eliminate_proc_2(in, in, in, in, out, di, uo) is det.

ho_order2__eliminate_proc_2(Needed, Keep, ProcId, 
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
:- pred fldl2(pred(X, Y, Y, Z, Z), pred(X, Y, Y, Z, Z), list(X), Y, Y, Z, Z).
:- mode fldl2(pred(in, in, out, di, uo) is det,
		pred(in, in, out, di, uo) is det,
		in, in, out, di, uo) is det.

fldl2(_, _, [], FirstAcc, FirstAcc, SecAcc, SecAcc).
fldl2(P1, P2, [H|T], FirstAcc0, FirstAcc, SecAcc0, SecAcc) :-
	call(P1, H, FirstAcc0, FirstAcc1, SecAcc0, SecAcc1),
	call(P2, H, FirstAcc0, FirstAcc2, SecAcc1, SecAcc2),
	( FirstAcc1 = FirstAcc2 ->
		fldl2(P1, P2, T, FirstAcc1, FirstAcc, SecAcc2, SecAcc)
	;
		error("fldl2: results don't agree")
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
