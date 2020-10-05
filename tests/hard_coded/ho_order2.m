%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Regression test for a bug in higher_order.m
% Symptom: seg fault or incorrect behaviour at runtime
% Cause: Incorrect ordering of curried arguments to multiple known
% higher-order input arguments in the specialised version.
%---------------------------------------------------------------------------%

:- module ho_order2.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module string.

%---------------------------------------------------------------------------%

    % information used during the elimination phase.

main(State0, State) :-
    map.from_assoc_list([0 - 1], Needed),
    map.from_assoc_list([0 - 1, 1 - 2, 2 - 4], ProcTable0),
    ProcIds = [1, 2],
    Keep = no,
    fldl2(eliminate_proc(Keep, Needed),
        eliminate_proc_2(Needed, Keep),
        ProcIds, ProcTable0, _ProcTable, State0, State).

    % eliminate a procedure, if unused

:- pred eliminate_proc(maybe(int)::in, map(int, int)::in,
    int::in, map(int, int)::in, map(int, int)::out, io::di, io::uo) is det.

eliminate_proc(Keep, Needed, ProcId, ProcTable0, ProcTable, !IO) :-
    ( if
        ( map.search(Needed, ProcId, _)
        ; Keep = yes(_)
        )
    then
        ProcTable = ProcTable0
    else
        io.format("Deleting %i\n", [i(ProcId)], !IO),
        map.delete(ProcId, ProcTable0, ProcTable)
    ).

:- pred eliminate_proc_2(map(int, int)::in, maybe(int)::in,
    int::in, map(int, int)::in, map(int, int)::out, io::di, io::uo) is det.

eliminate_proc_2(Needed, Keep, ProcId, ProcTable0, ProcTable, !IO) :-
    ( if
        ( map.search(Needed, ProcId, _)
        ; Keep = yes(_)
        )
    then
        ProcTable = ProcTable0
    else
        io.format("Deleting %i\n", [i(ProcId)], !IO),
        map.delete(ProcId, ProcTable0, ProcTable)
    ).

:- pred fldl2(pred(X, Y, Y, Z, Z), pred(X, Y, Y, Z, Z), list(X), Y, Y, Z, Z).
:- mode fldl2(pred(in, in, out, di, uo) is det,
    pred(in, in, out, di, uo) is det,
    in, in, out, di, uo) is det.

fldl2(_, _, [], FirstAcc, FirstAcc, SecAcc, SecAcc).
fldl2(P1, P2, [H | T], FirstAcc0, FirstAcc, SecAcc0, SecAcc) :-
    call(P1, H, FirstAcc0, FirstAcc1, SecAcc0, SecAcc1),
    call(P2, H, FirstAcc0, FirstAcc2, SecAcc1, SecAcc2),
    ( if FirstAcc1 = FirstAcc2 then
        fldl2(P1, P2, T, FirstAcc1, FirstAcc, SecAcc2, SecAcc)
    else
        error("fldl2: results don't agree")
    ).

%---------------------------------------------------------------------------%
