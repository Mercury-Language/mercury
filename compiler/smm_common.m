%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2008 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File smm_common.m.
% Main author: Quan Phan.
%
% This module contains defines types and procedures that are common to
% various static memory mangement analyses, e.g. CTGC, RBMM.
%
%-----------------------------------------------------------------------------%

:- module transform_hlds.smm_common.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_module.
:- import_module mdbcomp.
:- import_module mdbcomp.program_representation.
:- import_module parse_tree.
:- import_module parse_tree.prog_data. 

:- import_module io.
:- import_module list.
:- import_module term.

%-----------------------------------------------------------------------------%

    % Succeeds if the selector selects the type node of the input type.
    %
:- pred check_type_of_node(module_info::in, mer_type::in, selector::in) 
    is semidet.

    % Succeeds if some (or all) of the procedures in the list are special.
    %
:- pred some_are_special_preds(list(pred_proc_id)::in, module_info::in) 
    is semidet.

%-----------------------------------------------------------------------------%
%
% Definition of a program point
%
    
    % A program point records the place at which a data structure possibly
    % becomes garbage. 
    %
:- type program_point 
    ---> 	pp( 
                pp_context	:: term.context, 
                pp_path		:: goal_path
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
:- import_module ll_backend.
:- import_module ll_backend.liveness.
:- import_module parse_tree.prog_out.

:- import_module bool.
:- import_module cord.
:- import_module int.
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

	% Select the subtype of a type Type, selecting ConsId's position
	% Position. Position counts starting from 1 (instead of 0). 
	% Predicate aborts if the subtype cannot be determined. 
    %
:- pred select_subtype(module_info::in, mer_type::in, cons_id::in, int::in, 
    mer_type::out) is semidet.

select_subtype(ModuleInfo, Type, ConsId, Choice, SubType) :-
	get_cons_id_non_existential_arg_types(ModuleInfo, Type, ConsId, ArgTypes),
	list.index1(ArgTypes, Choice, SubType).

    % Special predicates are either compiler-generated ones, such as 
    % __Unify__ and others or ones that are not defined in the module. 
    %
some_are_special_preds(PPIds, ModuleInfo) :- 
	module_info_get_special_pred_map(ModuleInfo, SpecialPredMap), 
	map.values(SpecialPredMap, SpecialPredIds), 
	(
		list.filter(pred(PPId::in) is semidet :- (
                        PPId = proc(PredId, _),
                        list.member(PredId, SpecialPredIds)
                    ),
                    PPIds, SpecialPPIds),
		SpecialPPIds = [_ | _]
	; 
		list.filter(proc_not_defined_in_module(ModuleInfo),	PPIds,
            FilteredPPIds), 
		FilteredPPIds = [_ | _]
	).

:- pred proc_not_defined_in_module(module_info::in, pred_proc_id::in) 
    is semidet.

proc_not_defined_in_module(ModuleInfo, proc(PredId, _)):-
	module_info_pred_info(ModuleInfo, PredId, PredInfo),
	pred_info_get_import_status(PredInfo, Status),
	status_defined_in_this_module(Status) = no.

	% Note: for a meaningful use of this predicate the goal needs to be 
    % filled with path information, i.e. call to fill_goal_path_slots(...).
    %
program_point_init(GoalInfo) = ProgPoint :-
	Context = goal_info_get_context(GoalInfo),
	GoalPath = goal_info_get_goal_path(GoalInfo),
	ProgPoint = pp(Context, GoalPath).

dump_program_point(pp(Context, GoalPath), !IO):- 
    prog_out.write_context(Context, !IO), 
    io.write_string("--", !IO),
    GoalPathSteps = cord.list(GoalPath),
    list.foldl(dump_goal_path_step, GoalPathSteps, !IO).

:- pred dump_goal_path_step(goal_path_step::in, io::di, io::uo) is det.

dump_goal_path_step(step_conj(N)) -->
    io.write_char('c'),
    io.write_int(N).
dump_goal_path_step(step_disj(N)) -->
    io.write_char('d'),
    io.write_int(N).
dump_goal_path_step(step_switch(N, _)) -->
    io.write_char('s'),
    io.write_int(N).
dump_goal_path_step(step_ite_cond) -->
    io.write_char('c').
dump_goal_path_step(step_ite_then) -->
    io.write_char('t').
dump_goal_path_step(step_ite_else) -->
    io.write_char('e').
dump_goal_path_step(step_neg) -->
    io.write_char('n').
dump_goal_path_step(step_scope(_)) -->
    io.write_char('q').
dump_goal_path_step(step_first) -->
    io.write_char('f').
dump_goal_path_step(step_later) -->
    io.write_char('l').
dump_goal_path_step(step_atomic_main) -->
    io.write_char('a').
dump_goal_path_step(step_atomic_orelse(N)) -->
    io.write_char('o'),
    io.write_int(N).
	
%-----------------------------------------------------------------------------%
:- end_module smm_common.
%-----------------------------------------------------------------------------%
