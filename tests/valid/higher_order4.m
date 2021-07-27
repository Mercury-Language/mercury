%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module higher_order4.

:- interface.

:- import_module io.
:- import_module list.
:- import_module unit.

:- type analysis_request
    --->    some [FuncInfo, Call]
            analysis_request(unit(FuncInfo), Call)
                => call_pattern(FuncInfo, Call).

:- type mymap(K, V)
    --->    empty
    ;       two(K, V, mymap(K, V), mymap(K, V)).

:- type module_analysis_map(T) == mymap(analysis_name, func_analysis_map(T)).
:- type func_analysis_map(T) == mymap(func_id, list(T)).
:- type analysis_name == string.
:- type func_id == string.
:- typeclass call_pattern(T, U) where [].
:- typeclass compiler(T) where [].

:- pred write_module_analysis_requests(Compiler::in,
    module_analysis_map(analysis_request)::in, io::di, io::uo) is det
    <= compiler(Compiler).

:- implementation.

write_module_analysis_requests(Compiler, ModuleRequests, !IO) :-
    write_analysis_entries(write_request_entry(Compiler),
        ModuleRequests, !IO).

:- pred write_request_entry(Compiler::in)
    `with_type` write_entry(analysis_request)
    `with_inst` write_entry <= compiler(Compiler).

write_request_entry(_, _, _, analysis_request(_, _), !IO).

:- type write_entry(T) == pred(analysis_name, func_id, T, io, io).
:- inst write_entry == (pred(in, in, in, di, uo) is det).

:- pred write_analysis_entries(write_entry(T)::in(write_entry),
    module_analysis_map(T)::in, io::di, io::uo) is det.

write_analysis_entries(WriteEntry, ModuleResults, !IO) :-
    mymap_foldl(
        ( pred(AnalysisName::in, FuncResults::in, di, uo) is det -->
            mymap_foldl(
                ( pred(FuncId::in, FuncResultList::in, di, uo) is det -->
                    list.foldl(
                        ( pred(FuncResult::in, di, uo) is det -->
                            WriteEntry(AnalysisName, FuncId, FuncResult)
                        ), FuncResultList)
                ), FuncResults)
        ), ModuleResults, !IO).

:- pred mymap_foldl(pred(K, V, T, T), mymap(K, V), T, T).
:- mode mymap_foldl(pred(in, in, di, uo) is det, in, di, uo) is det.

mymap_foldl(_Pred, empty, Acc, Acc).
mymap_foldl(Pred, two(K, V, T0, T1), Acc0, Acc) :-
    mymap_foldl(Pred, T0, Acc0, Acc1),
    call(Pred, K, V, Acc1, Acc2),
    mymap_foldl(Pred, T1, Acc2, Acc).
