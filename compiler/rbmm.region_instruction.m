%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2007 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File rbmm.region_instruction.m.
% Main author: Quan Phan
%
% This module implements the process of introducing region instructions to 
% each program point in a procedure based on its region points-to graph and 
% live region information.
%

:- module transform_hlds.rbmm.region_instruction.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred. 
:- import_module transform_hlds.rbmm.points_to_info.
:- import_module transform_hlds.rbmm.region_liveness_info.
:- import_module transform_hlds.smm_common.

:- import_module list.
:- import_module map.
:- import_module string.

:- type annotation_table == map(pred_proc_id, annotation_proc).

:- type annotation_proc == map(program_point, before_after).
:- type before_after ---> before_after(list(string), list(string)).

    % Currently the region instructions are strings, which are attached to
    % either before or after a program point.
    % 
:- pred transform(module_info::in, rpta_info_table::in,
    execution_path_table::in, proc_pp_region_set_table::in,
    proc_pp_region_set_table::in, proc_pp_region_set_table::in,
    proc_region_set_table::in, proc_region_set_table::in,
    proc_region_set_table::in, annotation_table::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation. 

:- import_module check_hlds.
:- import_module check_hlds.goal_path.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_llds.
:- import_module libs.
:- import_module libs.compiler_util.
:- import_module transform_hlds.rbmm.points_to_graph.

:- import_module bool.
:- import_module pair.
:- import_module set.
:- import_module svmap.
:- import_module svset.
:- import_module term.
:- import_module varset.

transform(ModuleInfo, RptaInfoTable, ExecPathTable, LRBeforeTable,
        LRAfterTable, VoidVarRegionTable, BornRTable, DeadRTable,
        LocalRTable, AnnotationTable) :-
    module_info_predids(PredIds, ModuleInfo, _),
	map.init(AnnotationTable0),
	list.foldl(transform_pred(ModuleInfo, RptaInfoTable, ExecPathTable,
        LRBeforeTable, LRAfterTable, VoidVarRegionTable, BornRTable,
        DeadRTable, LocalRTable), PredIds,
        AnnotationTable0, AnnotationTable).

:- pred transform_pred(module_info::in, rpta_info_table::in,
    execution_path_table::in, proc_pp_region_set_table::in,
    proc_pp_region_set_table::in, proc_pp_region_set_table::in,
    proc_region_set_table::in, proc_region_set_table::in,
    proc_region_set_table::in, pred_id::in, 
    annotation_table::in, annotation_table::out) is det.

transform_pred(ModuleInfo, RptaInfoTable, ExecPathTable, LRBeforeTable,
        LRAfterTable, VoidVarRegionTable, BornRTable, DeadRTable,
        LocalRTable, PredId, !AnnotationTable) :-
	module_info_pred_info(ModuleInfo, PredId, PredInfo),
	pred_info_non_imported_procids(PredInfo) = ProcIds,
	list.foldl(transform_proc(ModuleInfo, PredId, RptaInfoTable,
        ExecPathTable, LRBeforeTable, LRAfterTable, VoidVarRegionTable,
        BornRTable, DeadRTable, LocalRTable), ProcIds, !AnnotationTable).

:- pred transform_proc(module_info::in, pred_id::in, rpta_info_table::in, 
    execution_path_table::in, proc_pp_region_set_table::in,
    proc_pp_region_set_table::in, proc_pp_region_set_table::in,
    proc_region_set_table::in, proc_region_set_table::in,
    proc_region_set_table::in, proc_id::in, 
    annotation_table::in, annotation_table::out) is det.

transform_proc(ModuleInfo, PredId, RptaInfoTable, ExecPathTable,
        LRBeforeTable, LRAfterTable, VoidVarRegionTable, BornRTable,
        DeadRTable, LocalRTable, ProcId, !AnnotationTable) :-
	PPId = proc(PredId, ProcId),
	( if
		some_are_special_preds([PPId], ModuleInfo)
	  then
		true
	  else
		module_info_proc_info(ModuleInfo, PPId, ProcInfo),
		map.lookup(RptaInfoTable, PPId, RptaInfo),
		map.lookup(BornRTable, PPId, BornR),
		map.lookup(DeadRTable, PPId, DeadR),
		map.lookup(LocalRTable, PPId, LocalR),
		map.lookup(LRBeforeTable, PPId, ProcLRBefore),
		map.lookup(LRAfterTable, PPId, ProcLRAfter),
		map.lookup(VoidVarRegionTable, PPId, ProcVoidVarRegion),
		map.lookup(ExecPathTable, PPId, ExecPaths), 
		transform_exec_paths(ExecPaths, RptaInfo, BornR, DeadR, LocalR,
            ProcLRBefore, ProcLRAfter, ProcVoidVarRegion,
            BornRTable, DeadRTable, ModuleInfo, ProcInfo,
            map.init, AnnotationProc),
		svmap.set(PPId, AnnotationProc, !AnnotationTable)
	).

	% Follow each execution path of a procedure and introduce 
    % region instructions at each program point.
    %
:- pred transform_exec_paths(list(execution_path)::in, rpta_info::in, 
    set(rptg_node)::in, set(rptg_node)::in, set(rptg_node)::in, 
    pp_region_set_table::in, pp_region_set_table::in, 
    pp_region_set_table::in, proc_region_set_table::in, 
    proc_region_set_table::in, module_info::in, proc_info::in, 
    annotation_proc::in, annotation_proc::out) is det.

transform_exec_paths([], _, _, _, _, _, _, _, _, _, _, _, !AnnotationProc).
transform_exec_paths([ExecPath|ExecPaths], RptaInfo, BornR, DeadR, 
        LocalR, ProcLRBefore, ProcLRAfter, ProcVoidVarRegion,
        BornRTable, DeadRTable, ModuleInfo, ProcInfo, !AnnotationProc) :-
	transform_exec_path(ExecPath, RptaInfo, BornR, DeadR, LocalR,
        ProcLRBefore, ProcLRAfter, ProcVoidVarRegion,
        BornRTable, DeadRTable, ModuleInfo, ProcInfo, !AnnotationProc),
	transform_exec_paths(ExecPaths, RptaInfo, BornR, DeadR, LocalR,
        ProcLRBefore, ProcLRAfter, ProcVoidVarRegion,
        BornRTable, DeadRTable, ModuleInfo, ProcInfo, !AnnotationProc).

:- pred transform_exec_path(execution_path::in, rpta_info::in,
    set(rptg_node)::in, set(rptg_node)::in, set(rptg_node)::in,
    pp_region_set_table::in, pp_region_set_table::in,
    pp_region_set_table::in, proc_region_set_table::in,
    proc_region_set_table::in, module_info::in, proc_info::in,
    annotation_proc::in, annotation_proc::out) is det.
transform_exec_path([], _, _, _, _, _, _, _, _, _, _, _, !AnnotationProc).
transform_exec_path([ProgPoint - Goal | ProgPoint_Goals], RptaInfo,
        BornR, DeadR, LocalR, ProcLRBefore, ProcLRAfter, ProcVoidVarRegion,
        BornRTable, DeadRTable, ModuleInfo, ProcInfo, !AnnotationProc) :-
	map.lookup(ProcLRBefore, ProgPoint, LRBefore),
	map.lookup(ProcLRAfter, ProgPoint, LRAfter),
	map.lookup(ProcVoidVarRegion, ProgPoint, VoidVarRegions),

    % Because void variables at this program point are considered dead
    % right after they get bound, the regions that are reached from them
    % are also candidates to be removed. They will be destroyed after this
    % program point if they are not live after the program point and they
    % are either in DeadR or LocalR or BornR.
	set.difference(VoidVarRegions, LRAfter, DeadVoidVarRegions0),
	set.union(LocalR, BornR, Local_Born),
	set.union(Local_Born, DeadR, Local_Born_Dead),
	set.intersect(Local_Born_Dead, DeadVoidVarRegions0, DeadVoidVarRegions),
	RptaInfo = rpta_info(Graph_p, _),
	set.fold(record_instruction_after_prog_point(ProgPoint, Graph_p,
        "Tn", "remove"), DeadVoidVarRegions, !AnnotationProc),

    set.difference(LRBefore, LRAfter, ToBeRemoved),
    set.intersect(ToBeRemoved, Local_Born_Dead, ToBeRemovedAndAllowed),
	( if 
		Goal = hlds_goal(switch(_, _, _), _)
	  then
        % This is a switch, i.e. unification, only rule 4 applied.
		transformation_rule_4_2(ProgPoint, ToBeRemovedAndAllowed,
            RptaInfo, !AnnotationProc)
	  else 
		Goal = hlds_goal(Expr, _),
        set.difference(LRAfter, LRBefore, ToBeCreated),
        set.intersect(ToBeCreated, Local_Born, ToBeCreatedAndAllowed),
        transformation_rule_1(Expr, ProgPoint, ToBeCreatedAndAllowed,
            RptaInfo, BornRTable, !AnnotationProc),
        transformation_rule_2(Expr, ProgPoint, ToBeCreatedAndAllowed,
            RptaInfo, ModuleInfo, ProcInfo, !AnnotationProc),
        transformation_rule_3(Expr, ProgPoint, ToBeRemovedAndAllowed,
            RptaInfo, DeadRTable, !AnnotationProc),
        transformation_rule_4(Expr, ProgPoint, ToBeRemovedAndAllowed,
            RptaInfo, !AnnotationProc)
	),

    (
        ProgPoint_Goals = [NextProgPoint - _ | _],
        % Transformation rule T5:
        % When a region is live after a program point but not live before the
        % next program point in the same execution path, it will be removed
        % before the next program point if the current procedure is allowed
        % to remove it.
        map.lookup(ProcLRBefore, NextProgPoint, LRBeforeNext),
        set.difference(LRAfter, LRBeforeNext, ToBeRemovedBeforeNext),
        set.intersect(Local_Born_Dead, ToBeRemovedBeforeNext, 
            ToBeRemovedBeforeNextAndAllowed),
        set.fold(record_instruction_before_prog_point(NextProgPoint, Graph_p,
                    "T5", "remove"),
            ToBeRemovedBeforeNextAndAllowed, !AnnotationProc),

        transform_exec_path(ProgPoint_Goals, RptaInfo, BornR, DeadR, LocalR,
            ProcLRBefore, ProcLRAfter, ProcVoidVarRegion,
            BornRTable, DeadRTable, ModuleInfo, ProcInfo, !AnnotationProc)
    ;
        % This is the last program point, we finish.
        ProgPoint_Goals = [],
        true
    ).
	
	% Rule 1: if an output region of q is not live, not created by q, 
	% and p is allowed to create it, it is created before the call to q.
	%
	% There are two cases: either q is defined in this module or q is
    % imported.
	% The former is dealt with as per the rule.
	% The latter: for now, with the assumption that all source code is in
    % only one module, imported preds will only be ones from 
    % Mercury's library. We do not intent to deal with the library's code
    % now therefore we have to assume here that the caller will always
    % *CREATE* the OUTPUT REGION for those procedures.
	%
:- pred transformation_rule_1(hlds_goal_expr::in, program_point::in,
    region_set::in, rpta_info::in, proc_region_set_table::in,
    annotation_proc::in, annotation_proc::out) is det.

transformation_rule_1(Expr, ProgPoint, ToBeCreatedAndAllowed, RptaInfo,
        BornRTable, !AnnotationProc) :-
    (
        Expr = plain_call(PredId_q, ProcId_q, _, _, _, _)
    -> 
        PPId_q = proc(PredId_q, ProcId_q),
        RptaInfo = rpta_info(Graph, AlphaMapping),
        ( if
            % Currently we do not collect BornR for non-defined-in-module 
            % procedure, so if we cannot find one here then q is an
            % imported.
            map.search(BornRTable, PPId_q, _)
        then
            map.lookup(AlphaMapping, ProgPoint, AlphaAtProgPoint),
            map.lookup(BornRTable, PPId_q, BornR_q),
            map.foldl(process_mapping_rule_1(ProgPoint, ToBeCreatedAndAllowed, 
                BornR_q, Graph), AlphaAtProgPoint, !AnnotationProc)
        else
            % q is from an imported module, therefore we consider  
            % BornR of q empty, so just create whatever regions becoming
            % live provided that they are in BornR or LocalR, i.e., p is
            % allowed to create them.
            set.fold(record_instruction_before_prog_point(ProgPoint, Graph,
                "T1", "create"), ToBeCreatedAndAllowed, !AnnotationProc)
        )
    ;
        (Expr = conj(_, [])
        ;Expr = disj([])
        ;Expr = unify(_, _, _, _, _))
    -> 
        true
    ;
        unexpected(this_file,
            "transformation_rule_1: found non-atomic goal")
    ).

:- pred process_mapping_rule_1(program_point::in, region_set::in,
    region_set::in, rpt_graph::in, rptg_node::in, rptg_node::in,
    annotation_proc::in, annotation_proc::out) is det.

process_mapping_rule_1(ProgPoint, ToBeCreatedAndAllowed, BornR_q, Graph_p,
        SourceRegion, TargetRegion, !AnnotationProc) :-
	( if
		(
			set.contains(ToBeCreatedAndAllowed, TargetRegion),
			not set.contains(BornR_q, SourceRegion)
		)
	  then
		record_instruction_before_prog_point(ProgPoint, Graph_p, "T1",
            "create", TargetRegion, !AnnotationProc)
	  else
		true
	).

	% Transformation rule 2: if a region reachable from the left variable
    % of a construction is not live before the construction but it is live
    % after and is a local region or in a BornR set, then the region is
    % created before the unification.
	% 
:- pred transformation_rule_2(hlds_goal_expr::in, program_point::in,
    region_set::in, rpta_info::in, module_info::in, proc_info::in, 
    annotation_proc::in, annotation_proc::out) is det.

transformation_rule_2(Expr, ProgPoint, ToBeCreatedAndAllowed, RptaInfo,
        ModuleInfo, ProcInfo, !AnnotationProc) :-
    (
        Expr = unify(X, _, _, construct(_, _, _, _, _, _, _), _)
    ->
        RptaInfo = rpta_info(Graph, _AlphaMapping),
        % need to be regions reachable from X
        reach_from_a_variable(Graph, ModuleInfo, ProcInfo, X,
            set.init, Reach_X),

        set.intersect(Reach_X, ToBeCreatedAndAllowed,
            ToBeCreatedAllowedAndReached),
        set.fold(record_instruction_before_prog_point(ProgPoint, Graph,
                    "T2", "create"),
            ToBeCreatedAllowedAndReached, !AnnotationProc)
    ;
        (Expr = unify(_, _, _, deconstruct(_, _, _, _, _, _), _)
        ;Expr = unify(_, _, _, assign(_, _), _)
        ;Expr = unify(_, _, _, simple_test(_, _), _)
        ;Expr = unify(_, _, _, complicated_unify(_, _, _), _)
        ;Expr = plain_call(_, _, _, _, _, _)
        ;Expr = conj(_, [])
        ;Expr = disj([]))
    ->    
        true
    ;
        unexpected(this_file,
            "transformation_rule_2: non-atomic goal found")
    ).

	% Transformation rule 3: if a region is live before q but it is not
    % live after q and it is not removed by q, the caller will remove it
    % after calling q.
	% 
	% There are two cases: either q is defined in this module or q is
    % imported.
	% The former is straightforward because we have the analysis information
    % for q.
	% The latter for now, with the assumption that all source code is in
    % only one module, imported preds will only be ones from
    % Mercury's library. We do not intent to deal with the library's code
    % now therefore we have to assume here that the caller will always
    % REMOVE the REGIONs for those procedures.
	% 
:- pred transformation_rule_3(hlds_goal_expr::in, program_point::in,
    region_set::in, rpta_info::in, proc_region_set_table::in,
    annotation_proc::in, annotation_proc::out) is det.

transformation_rule_3(Expr, ProgPoint, ToBeRemovedAndAllowed, RptaInfo,
        DeadRTable, !AnnotationProc) :-
    (
        Expr =  plain_call(PredId_q, ProcId_q, _, _, _, _)
    ->
        PPId_q = proc(PredId_q, ProcId_q),
        ( if
            map.search(DeadRTable, PPId_q, _)
          then
            RptaInfo = rpta_info(Graph, AlphaMapping),
            map.lookup(AlphaMapping, ProgPoint, AlphaAtProgPoint),

            map.lookup(DeadRTable, PPId_q, DeadR_q),
            map.foldl(process_mapping_rule_3(ProgPoint,
                        ToBeRemovedAndAllowed, DeadR_q, Graph),
                AlphaAtProgPoint, !AnnotationProc)
          else
            % q is from an imported module. So just remove whatever regions
            % become dead provided that p is allowed to remove those regions.
            RptaInfo = rpta_info(Graph_p, _),
            set.fold(record_instruction_after_prog_point(ProgPoint, Graph_p,
                        "T3", "remove"),
                ToBeRemovedAndAllowed, !AnnotationProc)
        )
    ;
        (Expr = unify(_, _, _, _, _)
        ;Expr = conj(_, [])
        ;Expr = disj([]))
    ->
        true
    ;
        unexpected(this_file,
            "transformation_rule_3: non-atomic goal found")
    ).

:- pred process_mapping_rule_3(program_point::in, region_set::in, 
    region_set::in, rpt_graph::in, rptg_node::in, rptg_node::in,
    annotation_proc::in, annotation_proc::out) is det.

process_mapping_rule_3(ProgPoint, ToBeRemovedAndAllowed, DeadR_q, Graph_p, 
        SourceRegion, TargetRegion, !AnnotationProc) :-
 	( if
		(
            set.contains(ToBeRemovedAndAllowed, TargetRegion),
            not set.contains(DeadR_q, SourceRegion)
        )
	  then
	  	record_instruction_after_prog_point(ProgPoint, Graph_p, "T3",
            "remove", TargetRegion, !AnnotationProc)
	  else
		true
	).

	% Transformation rule 4: if a region is live before a unification but 
	% it is not live after and it is in the dead region set or is a local
	% region, then it is removed after the unification if the current
    % procedure is allowed to remove it.
    %
:- pred transformation_rule_4(hlds_goal_expr::in, program_point::in,
    region_set::in, rpta_info::in, annotation_proc::in, 
    annotation_proc::out) is det.

transformation_rule_4(Expr, ProgPoint, ToBeRemovedAndAllowed, RptaInfo,
        !AnnotationProc) :-
    (
        Expr = unify(_, _, _, _, _)
    ->
        RptaInfo = rpta_info(Graph, _),
	  	set.fold(record_instruction_after_prog_point(ProgPoint, Graph,
            "T4", "remove"), ToBeRemovedAndAllowed, !AnnotationProc)
    ;
        (Expr = plain_call(_, _, _, _, _, _)
        ;Expr = conj(_, [])
        ;Expr = disj([]))
    ->
        true
    ;
        unexpected(this_file,
            "transformation_rule_4: non-atomic goal found")
    ).

	% This is for the case rule 4 applied for a unification in a switch, 
	% the unification has been removed by MMC.
	% We will record the annotations after the program point.
    %
:- pred transformation_rule_4_2(program_point::in, region_set::in,
    rpta_info::in, annotation_proc::in, annotation_proc::out) is det.

transformation_rule_4_2(ProgPoint, ToBeRemovedAndAllowed, RptaInfo,
        !AnnotationProc) :-
	RptaInfo = rpta_info(Graph, _),
    set.fold(record_instruction_after_prog_point(ProgPoint, Graph,
        "T4", "remove"), ToBeRemovedAndAllowed, !AnnotationProc).

:- pred record_instruction_after_prog_point(program_point::in,
    rpt_graph::in, string::in, string::in, rptg_node::in,
    annotation_proc::in, annotation_proc::out) is det.

record_instruction_after_prog_point(ProgPoint, Graph_p, Rule, Action, Region,
        !AnnotationProc) :-
	rptg_node_contents(Graph_p, Region, Content),
	RegionAnno = string.format("%s: %s %s",
        [s(Rule), s(Action), s(Content^reg_var_name)]),
    % get After of the pp and add annotation to it
	( if
		map.search(!.AnnotationProc, ProgPoint,
            before_after(Before, After))
	then
		( if 
			list.member(RegionAnno, After)
		  then
			true
		  else
			svmap.set(ProgPoint,
                before_after(Before, [RegionAnno | After]), !AnnotationProc)
		)
	else
		svmap.set(ProgPoint, before_after([], [RegionAnno]),
            !AnnotationProc)
	).

:- pred record_instruction_before_prog_point(program_point::in,
    rpt_graph::in, string::in, string::in, rptg_node::in,
    annotation_proc::in, annotation_proc::out) is det.

record_instruction_before_prog_point(ProgPoint, Graph_p, Rule, Action,
        Region, !AnnotationProc) :-
	rptg_node_contents(Graph_p, Region, Content),
	RegionAnno = string.format("%s: %s %s",
        [s(Rule), s(Action), s(Content^reg_var_name)]),
    % get After of the pp and add annotation to it
	( if
		map.search(!.AnnotationProc, ProgPoint,
            before_after(Before, After))
	then
		( if 
			list.member(RegionAnno, Before)
		  then
			true
		  else
			svmap.set(ProgPoint,
                before_after([RegionAnno | Before], After), !AnnotationProc)
		)
	else
		svmap.set(ProgPoint, before_after([RegionAnno], []),
            !AnnotationProc)
	).

:- func this_file = string.
this_file = "rbmm.region_instruction.m".
