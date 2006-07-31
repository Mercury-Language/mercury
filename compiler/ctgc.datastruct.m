%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: ctgc.datastruct.m.
% Main author: nancy.
% 
% Definition of predicates and functions for the manipulation of
% datastructures.
% 
%-----------------------------------------------------------------------------%

:- module transform_hlds.ctgc.datastruct.
:- interface.

:- import_module parse_tree.prog_data.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.

:- import_module list.

%-----------------------------------------------------------------------------%

    % Create an initial top-datastruct of the given variable.
    %
:- func datastruct_init(prog_var) = datastruct.
:- func datastruct_init_with_selector(prog_var, selector) = datastruct.
:- func datastruct_init_with_pos(prog_var, cons_id, int) = datastruct.

    % Verify whether the given datastructs are identical.
    %
:- pred datastruct_equal(datastruct::in, datastruct::in) is semidet.

    % Verify whether the datastructure represents a top cell, i.e. where
    % the selector path is an empty path.
    %
:- pred datastruct_refers_to_topcell(datastruct::in) is semidet.

    % Select a subterm of the given datastructure using the specified selector.
    % It is assumed that the selector is a valid selector for that
    % datastructure.
    %
:- pred datastruct_termshift(selector::in, datastruct::in, datastruct::out)
    is det.
:- func datastruct_termshift(selector, datastruct) = datastruct.

    % Normalize the representation of the datastructure.
    % (proc_info is needed to obtain the type of the variable of the
    % datastructure).
    % The selector of a datastruct is normalized
    % iff none of the term nodes met on the path to the actual selected
    % term by the selector has the same type as the selected node.
    %
:- func normalize_datastruct(module_info, proc_info, datastruct) = datastruct.

    % Normalize the representation of the datastructure using its
    % type information.
    %
:- pred normalize_datastruct_with_type_information(module_info::in, 
    mer_type::in, datastruct::in, datastruct::out) is det.
:- func normalize_datastruct_with_type_information(module_info, mer_type,
    datastruct) = datastruct.

:- pred datastruct_subsumed_by_return_selector(module_info::in, proc_info::in,
    datastruct::in, datastruct::in, selector::out) is semidet.
:- pred datastruct_subsumed_by(module_info::in, proc_info::in,
    datastruct::in, datastruct::in) is semidet.
:- pred datastruct_subsumed_by_list(module_info::in, proc_info::in,
    datastruct::in, list(datastruct)::in) is semidet.
:- pred datastructs_subsumed_by_list(module_info::in, proc_info::in,
    list(datastruct)::in, list(datastruct)::in) is semidet.

:- func datastruct_lists_least_upper_bound(module_info, proc_info,
    list(datastruct), list(datastruct)) = list(datastruct).

:- pred datastruct_apply_widening(module_info::in, proc_info::in,
    datastruct::in, datastruct::out) is det.

:- func datastructs_project(list(prog_var), 
    list(datastruct)) = list(datastruct).

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module transform_hlds.ctgc.selector.

:- import_module map.

%-----------------------------------------------------------------------------%

datastruct_init(V) = datastruct_init_with_selector(V, []).

datastruct_init_with_selector(V, Sel) = selected_cel(V, Sel).

datastruct_init_with_pos(V, ConsId, Int)
    = datastruct_init_with_selector(V, selector_init(ConsId, Int)).

datastruct_equal(D1, D2) :- D1 = D2.

datastruct_refers_to_topcell(Data):-
    DSel = Data ^ sc_selector,
    DSel = [].

datastruct_termshift(Sel, Data0, Data) :-
    DSel = Data0 ^ sc_selector,
    selector_termshift(DSel, Sel, NewSel),
    Data = Data0 ^ sc_selector := NewSel.

datastruct_termshift(Sel, Data0) = Data :-
    datastruct_termshift(Sel, Data0, Data).

normalize_datastruct_with_type_information(ModuleInfo, Type, !Datastruct) :-
    DSel0 = !.Datastruct ^ sc_selector,
    normalize_selector_with_type_information(ModuleInfo, Type, DSel0, DSel),
    !:Datastruct = !.Datastruct ^ sc_selector := DSel.

normalize_datastruct_with_type_information(ModuleInfo, Type, Data0) = Data :-
    normalize_datastruct_with_type_information(ModuleInfo, Type, Data0, Data).

normalize_datastruct(ModuleInfo, ProcInfo, Data0) = Data :-
    Var = Data0 ^ sc_var,
    proc_info_get_vartypes(ProcInfo, VarTypes),
    map.lookup(VarTypes, Var, Type),
    Data = normalize_datastruct_with_type_information(ModuleInfo, Type, Data0).

datastruct_subsumed_by_return_selector(ModuleInfo, ProcInfo, Data1, Data2,
        Extension) :-
    Var = Data1 ^ sc_var,
    Var = Data2 ^ sc_var,
    Sel1 = Data1 ^ sc_selector,
    Sel2 = Data2 ^ sc_selector,
    proc_info_get_vartypes(ProcInfo, VarTypes),
    map.lookup(VarTypes, Var, Type),
    ctgc.selector.subsumed_by(ModuleInfo, Sel1, Sel2, Type, Extension).

datastruct_subsumed_by(ModuleInfo, ProcInfo, Data1, Data2) :-
    datastruct_subsumed_by_return_selector(ModuleInfo, ProcInfo, Data1, Data2,
        _).

datastruct_subsumed_by_list(ModuleInfo, ProcInfo, Data0, [Data | Rest]):-
    (
        datastruct_subsumed_by(ModuleInfo, ProcInfo, Data0, Data)
    ;
        datastruct_subsumed_by_list(ModuleInfo, ProcInfo, Data0, Rest)
    ).

datastructs_subsumed_by_list(ModuleInfo, ProcInfo, PerhapsSubsumedData, 
        Data) :- 
    list.takewhile(datastructs_subsume_datastruct(ModuleInfo, ProcInfo, Data), 
        PerhapsSubsumedData, _, NotSubsumed), 
    NotSubsumed = [].

:- pred datastructs_subsume_datastruct(module_info::in, proc_info::in, 
    list(datastruct)::in, datastruct::in) is semidet.

datastructs_subsume_datastruct(ModuleInfo, ProcInfo, Datastructs, Data):- 
    datastruct_subsumed_by_list(ModuleInfo, ProcInfo, Data, Datastructs).

datastruct_apply_widening(ModuleInfo, ProcInfo, !Data) :-
    Var = !.Data ^ sc_var,
    Sel0 = !.Data ^ sc_selector,
    proc_info_get_vartypes(ProcInfo, VarTypes),
    map.lookup(VarTypes, Var, Type),
    selector_apply_widening(ModuleInfo, Type, Sel0, Sel),
    !:Data = datastruct_init_with_selector(Var, Sel).

datastruct_lists_least_upper_bound(ModuleInfo, ProcInfo, Data1, Data2) 
        = Data :- 
    list.filter(
        datastructs_subsume_datastruct(ModuleInfo, ProcInfo, Data1),
        Data2, _SubsumedData, NotSubsumedData),
    Data = list.append(NotSubsumedData, Data1).

datastructs_project(Vars, DataIn) = 
    list.filter(
        (pred(Data::in) is semidet :- list.member(Data ^ sc_var, Vars)),
        DataIn).

%-----------------------------------------------------------------------------%
:- end_module transform_hlds.ctgc.datastruct.
%-----------------------------------------------------------------------------%
