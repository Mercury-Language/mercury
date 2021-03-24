%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2008, 2010-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File smm_common.m.
% Main author: Quan Phan.
%
% This module contains defines types and procedures that are common to
% various static memory management analyses, e.g. CTGC, RBMM.
%
%-----------------------------------------------------------------------------%

:- module transform_hlds.smm_common.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_module.
:- import_module mdbcomp.
:- import_module mdbcomp.goal_path.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_pragma.

:- import_module io.
:- import_module list.
:- import_module term.

%-----------------------------------------------------------------------------%

    % Succeeds if the selector selects the type node of the input type.
    %
:- pred check_type_of_node(module_info::in, mer_type::in, selector::in)
    is semidet.

    % Succeeds if some (or all) of the procedures in the list are either
    %
    % - compiler generated unify, index, compare or init predicates, or
    % - not defined in this module.
    %
:- pred some_are_special_preds(list(pred_proc_id)::in, module_info::in)
    is semidet.

%-----------------------------------------------------------------------------%
%
% Definition of a program point.
%

    % A program point identifies a place at which a data structure possibly
    % becomes garbage.
    %
:- type program_point
    --->    pp(
                pp_context  :: term.context,
                pp_id       :: reverse_goal_path
            ).

    % Compute the program point from the given goal_info.
    %
:- func program_point_init(hlds_goal_info) = program_point.

    % Dump the information contained in a program point.
    %
:- pred dump_program_point(program_point::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.type_util.
:- import_module hlds.special_pred.
:- import_module hlds.status.
:- import_module parse_tree.prog_out.

:- import_module bool.
:- import_module map.

%-----------------------------------------------------------------------------%

    % Check if the selector is valid w.r.t the type.
    %
check_type_of_node(ModuleInfo, StartType, Selector) :-
    (
        Selector = [Sel | Sels],
        (
            Sel = termsel(Cons_id, Choice),
            select_subtype(ModuleInfo, StartType, Cons_id, Choice, SubType)
        ;
            Sel = typesel(SubType)
        ),
        check_type_of_node(ModuleInfo, SubType, Sels)
    ;
        Selector = []
    ).

    % Select the subtype of a type Type, selecting ConsId's position Position.
    % Position counts starting from 1 (instead of 0).
    % The predicate aborts if the subtype cannot be determined.
    %
:- pred select_subtype(module_info::in, mer_type::in, cons_id::in, int::in,
    mer_type::out) is semidet.

select_subtype(ModuleInfo, Type, ConsId, Choice, SubType) :-
    get_cons_id_non_existential_arg_types(ModuleInfo, Type, ConsId, ArgTypes),
    list.index1(ArgTypes, Choice, SubType).

some_are_special_preds(PPIds, ModuleInfo) :-
    (
        module_info_get_special_pred_maps(ModuleInfo, SpecialPredMaps),
        SpecialPredMaps = special_pred_maps(UnifyMap, IndexMap, CompareMap),
        ( some_special_preds_are_in_list(UnifyMap, PPIds)
        ; some_special_preds_are_in_list(IndexMap, PPIds)
        ; some_special_preds_are_in_list(CompareMap, PPIds)
        )
    ;
        list.filter(proc_not_defined_in_module(ModuleInfo), PPIds,
            FilteredPPIds),
        FilteredPPIds = [_ | _]
    ).

:- pred some_special_preds_are_in_list(map(type_ctor, pred_id)::in,
    list(pred_proc_id)::in) is semidet.

some_special_preds_are_in_list(SpecMap, PPIds) :-
    map.values(SpecMap, SpecialPredIds),
    list.filter(
        ( pred(PPId::in) is semidet :-
            PPId = proc(PredId, _),
            list.member(PredId, SpecialPredIds)
        ),
        PPIds, SpecialPPIds),
    SpecialPPIds = [_ | _].

:- pred proc_not_defined_in_module(module_info::in, pred_proc_id::in)
    is semidet.

proc_not_defined_in_module(ModuleInfo, proc(PredId, _)):-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_get_status(PredInfo, PredStatus),
    pred_status_defined_in_this_module(PredStatus) = no.

    % Note: for a meaningful use of this predicate the goal needs to be
    % filled with goal id information, i.e. call to fill_goal_id_slots(...).
    %
program_point_init(GoalInfo) = ProgPoint :-
    Context = goal_info_get_context(GoalInfo),
    RevGoalPath = goal_info_get_reverse_goal_path(GoalInfo),
    ProgPoint = pp(Context, RevGoalPath).

dump_program_point(pp(Context, RevGoalPath), !IO):-
    prog_out.write_context(Context, !IO),
    io.write_string("--", !IO),
    io.write_string(rev_goal_path_to_string(RevGoalPath), !IO).

%-----------------------------------------------------------------------------%
:- end_module transform_hlds.smm_common.
%-----------------------------------------------------------------------------%
