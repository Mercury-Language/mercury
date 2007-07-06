%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4
%-----------------------------------------------------------------------------%
% Copyright (C) 2007 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: rbmm.condition_renaming.m.
% Main author: Quan Phan.
%
% The region analysis may introduce creations for region variables
% which are non-local to an if-then-else in the condition goal of the
% if-then-else.
% This module finds which renaming and reverse renaming are needed at each
% program point so that the binding of non-local regions in the condition
% goal of an if-then-else is resolved. 
%
% When reasoning about the renaming and reverse renaming needed for
% an if-then-else here we take into account the changes to regions caused
% by the renaming and renaming annotations needed for region resurrection.
% This can be viewed as if the program is transformed by the renaming
% and reverse renaming for region resurrection first. This is to solve the
% problem with region resurrection. Then that transformed program is
% transformed again to solve the problem with if-then-else. Note that the
% first transformation may add to the problem with if-then-else, e.g.,
% when it introduces reverse renaming to a non-local variable inside the
% condition goal of an if-then-else.
%
%-----------------------------------------------------------------------------%

:- module transform_hlds.rbmm.condition_renaming.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module mdbcomp.
:- import_module mdbcomp.program_representation.
:- import_module transform_hlds.rbmm.points_to_info.
:- import_module transform_hlds.rbmm.region_liveness_info.
:- import_module transform_hlds.rbmm.region_resurrection_renaming.

:- import_module map.
:- import_module set.
:- import_module string.

%-----------------------------------------------------------------------------%

:- type proc_goal_path_regions_table ==
	map(pred_proc_id, goal_path_regions_table).
:- type goal_path_regions_table == map(goal_path, set(string)).

	% This predicate collects two pieces of information.
	% 1. The non-local regions of if-then-elses.
	%		A region is non-local to an if-then-else if the region
	%		is created in the if-then-else and outlives the scope
	%		of the if-then-else.
	% 2. The regions which are created (get bound) in the condition
	% goals of if-then-else.
	% We will only store information about a procedure if the information
	% exists. That means, for example, there is no entry which maps a PPId
	% to empty.
	%
	% This information is used to compute the regions which need to be 
	% renamed, i.e., both non-local and created in the condition of an
	% if-then-else.
	%
:- pred collect_non_local_and_in_cond_regions(module_info::in,
	rpta_info_table::in, proc_pp_region_set_table::in,
	proc_pp_region_set_table::in, renaming_table::in, 
	renaming_annotation_table::in, proc_goal_path_regions_table::out,
	proc_goal_path_regions_table::out) is det.

	% After having the 2 pieces of information calculated above, this step
	% is simple. The only thing to note here is that we will only store
	% information for a procedure when the information exists.
	% This means that a procedure in which no renaming is required will not
	% be in the resulting table.
	%
:- pred collect_ite_renamed_regions(proc_goal_path_regions_table::in,
	proc_goal_path_regions_table::in, proc_goal_path_regions_table::out)
	is det.

	% This predicate ONLY traverses the procedures which requires
	% condition renaming and for each condition goal in such a procedure
	% it introduces the necessary renamings.
	% The renaming information is stored in the form:
	% a program point (in the condition goal) --> necessary renaming at
	% the point.
	% A new name for a region (variable) at a program point is:
	% RegionName_ite_Number, where Number is the number of condition goals
	% to which the program point belongs.
	%
:- pred collect_ite_renaming(module_info::in, rpta_info_table::in,
	proc_goal_path_regions_table::in, renaming_table::out) is det.

	% In the then branch of an if-then-else in which renaming happens we 
	% need to introduce reverse renaming annotation in the form of
	% assignments, e.g., if R is renamed to R_ite_1 in the condition
	% then we add R = R_ite_1 in the then branch.
	% Because the if-then-else can lie inside the condition goals of
	% other if-then-elses, we need to apply the renaming at the 
	% program point where the annotation is attached.
	% E.g., At the point renaming R --> R_ite_2 exists, then the
	% added annotation is R_ite_2 = R_ite_1.
	%
	% This predicate will also traverse only procedures in which renaming
	% happens. For each Condition where renaming happens,
	% it finds the first program point in the corresponding Then and
	% introduces the reverse renaming annotation before that ponit.
	%
:- pred collect_ite_annotation(proc_goal_path_regions_table::in,
	execution_path_table::in, rpta_info_table::in, renaming_table::in,
	renaming_annotation_table::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.goal_path.
:- import_module hlds.hlds_goal.
:- import_module libs.
:- import_module libs.compiler_util.
:- import_module transform_hlds.rbmm.points_to_graph.
:- import_module transform_hlds.smm_common.

:- import_module int.
:- import_module list.
:- import_module pair.
:- import_module set.
:- import_module svmap.
:- import_module svset.

%-----------------------------------------------------------------------------%

collect_non_local_and_in_cond_regions(ModuleInfo, RptaInfoTable,
		LRBeforeTable, LRAfterTable, ResurRenamingTable,
		ResurRenamingAnnoTable, NonLocalRegionsTable, InCondRegionsTable) :-
    module_info_predids(PredIds, ModuleInfo, _),
    list.foldl2(collect_non_local_and_in_cond_regions_pred(ModuleInfo,
		RptaInfoTable, LRBeforeTable, LRAfterTable, ResurRenamingTable,
		ResurRenamingAnnoTable), PredIds,
		map.init, NonLocalRegionsTable, map.init, InCondRegionsTable).

:- pred collect_non_local_and_in_cond_regions_pred(module_info::in,
	rpta_info_table::in, proc_pp_region_set_table::in,
	proc_pp_region_set_table::in, renaming_table::in,
	renaming_annotation_table::in, pred_id::in,
	proc_goal_path_regions_table::in, proc_goal_path_regions_table::out,
	proc_goal_path_regions_table::in, proc_goal_path_regions_table::out)
	is det.

collect_non_local_and_in_cond_regions_pred(ModuleInfo, RptaInfoTable,
		LRBeforeTable, LRAfterTable, ResurRenamingTable,
		ResurRenamingAnnoTable, PredId, !NonLocalRegionsTable,
		!InCondRegionsTable) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    ProcIds = pred_info_non_imported_procids(PredInfo),
    list.foldl2(collect_non_local_and_in_cond_regions_proc(ModuleInfo,
		PredId, RptaInfoTable, LRBeforeTable, LRAfterTable,
		ResurRenamingTable, ResurRenamingAnnoTable), ProcIds,
		!NonLocalRegionsTable, !InCondRegionsTable).

:- pred collect_non_local_and_in_cond_regions_proc(module_info::in,
	pred_id::in, rpta_info_table::in, proc_pp_region_set_table::in,
	proc_pp_region_set_table::in, renaming_table::in,
	renaming_annotation_table::in, proc_id::in,
	proc_goal_path_regions_table::in, proc_goal_path_regions_table::out,
	proc_goal_path_regions_table::in, proc_goal_path_regions_table::out)
	is det.

collect_non_local_and_in_cond_regions_proc(ModuleInfo, PredId,
		RptaInfoTable, LRBeforeTable, LRAfterTable, ResurRenamingTable,
		ResurRenamingAnnoTable, ProcId,
		!NonLocalRegionsTable, !InCondRegionsTable) :-
    PPId = proc(PredId, ProcId),
    ( if    some_are_special_preds([PPId], ModuleInfo)
      then  true
      else
            module_info_proc_info(ModuleInfo, PPId, ProcInfo0),
			fill_goal_path_slots(ModuleInfo, ProcInfo0, ProcInfo),
            proc_info_get_goal(ProcInfo, Goal),
			map.lookup(RptaInfoTable, PPId, rpta_info(Graph, _)),
			map.lookup(LRBeforeTable, PPId, LRBeforeProc),
			map.lookup(LRAfterTable, PPId, LRAfterProc),
			( if	map.search(ResurRenamingTable, PPId,
						ResurRenamingProc0)
			  then	ResurRenamingProc = ResurRenamingProc0
			  else	ResurRenamingProc = map.init
			),
			( if 	map.search(ResurRenamingAnnoTable, PPId,
						ResurRenamingAnnoProc0)
			  then	ResurRenamingAnnoProc = ResurRenamingAnnoProc0
			  else	ResurRenamingAnnoProc = map.init
			),
            collect_non_local_and_in_cond_regions_goal(Graph,
				LRBeforeProc, LRAfterProc, ResurRenamingProc,
				ResurRenamingAnnoProc, Goal,
				map.init, NonLocalRegionsProc,
				map.init, InCondRegionsProc),
			( if	map.count(NonLocalRegionsProc) = 0
			  then	true
			  else	svmap.set(PPId, NonLocalRegionsProc,
						!NonLocalRegionsTable)
			),
			( if	map.count(InCondRegionsProc) = 0
			  then	true
			  else	svmap.set(PPId, InCondRegionsProc,
						!InCondRegionsTable)
			)
    ).

:- pred collect_non_local_and_in_cond_regions_goal(rpt_graph::in,
	pp_region_set_table::in, pp_region_set_table::in,
	renaming_proc::in, renaming_annotation_proc::in, hlds_goal::in,
    goal_path_regions_table::in, goal_path_regions_table::out,
	goal_path_regions_table::in, goal_path_regions_table::out) is det.

collect_non_local_and_in_cond_regions_goal(Graph, LRBeforeProc, LRAfterProc,
		ResurRenamingProc, ResurRenamingAnnoProc, Goal,
		!NonLocalRegionsProc, !InCondRegionsProc) :- 
	Goal = hlds_goal(Expr, _),
	collect_non_local_and_in_cond_regions_expr(Graph, LRBeforeProc,
		LRAfterProc, ResurRenamingProc, ResurRenamingAnnoProc, Expr,
		!NonLocalRegionsProc, !InCondRegionsProc).

:- pred collect_non_local_and_in_cond_regions_expr(rpt_graph::in,
	pp_region_set_table::in, pp_region_set_table::in,
	renaming_proc::in, renaming_annotation_proc::in, hlds_goal_expr::in,
	goal_path_regions_table::in, goal_path_regions_table::out,
	goal_path_regions_table::in, goal_path_regions_table::out) is det.

collect_non_local_and_in_cond_regions_expr(Graph, LRBeforeProc, LRAfterProc,
		ResurRenamingProc, ResurRenamingAnnoProc, conj(_, Conjs),
		!NonLocalRegionsProc, !InCondRegionsProc) :- 
    list.foldl2(collect_non_local_and_in_cond_regions_goal(Graph,
					LRBeforeProc, LRAfterProc,
					ResurRenamingProc,
					ResurRenamingAnnoProc),
		Conjs, !NonLocalRegionsProc, !InCondRegionsProc). 

collect_non_local_and_in_cond_regions_expr(_, _, _, _, _,
		plain_call(_, _, _, _, _, _),
		!NonLocalRegionsProc, !InCondRegionsProc).
collect_non_local_and_in_cond_regions_expr(_, _, _, _, _,
		generic_call(_, _, _, _),
		!NonLocalRegionsProc, !InCondRegionsProc).
collect_non_local_and_in_cond_regions_expr(_, _, _, _, _,
		call_foreign_proc(_, _, _, _, _, _, _),
		!NonLocalRegionsProc, !InCondRegionsProc).

collect_non_local_and_in_cond_regions_expr(Graph, LRBeforeProc, LRAfterProc,
		ResurRenamingProc, ResurRenamingAnnoProc, switch(_, _, Cases),
		!NonLocalRegionsProc, !InCondRegionsProc) :- 
    list.foldl2(collect_non_local_and_in_cond_regions_case(Graph,
					LRBeforeProc, LRAfterProc,
					ResurRenamingProc,
					ResurRenamingAnnoProc),
		Cases, !NonLocalRegionsProc, !InCondRegionsProc).
collect_non_local_and_in_cond_regions_expr(Graph, LRBeforeProc, LRAfterProc,
		ResurRenamingProc, ResurRenamingAnnoProc, disj(Disjs),
		!NonLocalRegionsProc, !InCondRegionsProc) :-
    list.foldl2(collect_non_local_and_in_cond_regions_goal(Graph,
					LRBeforeProc, LRAfterProc,
					ResurRenamingProc,
					ResurRenamingAnnoProc),
		Disjs, !NonLocalRegionsProc, !InCondRegionsProc). 
collect_non_local_and_in_cond_regions_expr(Graph, LRBeforeProc, LRAfterProc,
		ResurRenamingProc, ResurRenamingAnnoProc, negation(Goal),
		!NonLocalRegionsProc, !InCondRegionsProc) :- 
    collect_non_local_and_in_cond_regions_goal(Graph, LRBeforeProc,
		LRAfterProc, ResurRenamingProc, ResurRenamingAnnoProc, Goal,
		!NonLocalRegionsProc, !InCondRegionsProc). 
collect_non_local_and_in_cond_regions_expr(_, _, _, _, _,
		unify(_, _, _, _, _), !NonLocalRegionsProc, !InCondRegionsProc).

collect_non_local_and_in_cond_regions_expr(Graph, LRBeforeProc, LRAfterProc,
		ResurRenamingProc, ResurRenamingAnnoProc, scope(_, Goal),
		!NonLocalRegionsProc, !InCondRegionsProc) :- 
	collect_non_local_and_in_cond_regions_goal(Graph, LRBeforeProc,
		LRAfterProc, ResurRenamingProc, ResurRenamingAnnoProc, Goal,
		!NonLocalRegionsProc, !InCondRegionsProc).

collect_non_local_and_in_cond_regions_expr(Graph, LRBeforeProc, LRAfterProc,
		ResurRenamingProc, ResurRenamingAnnoProc, Expr,
		!NonLocalRegionProc, !InCondRegionsProc) :- 
	Expr = if_then_else(_, Cond, Then, Else),

	% We only care about regions created inside condition goals.
	collect_regions_created_in_condition(Graph, LRBeforeProc, LRAfterProc,
		ResurRenamingProc, ResurRenamingAnnoProc, Cond,
		!InCondRegionsProc),

	% The sets of non_local regions in the (Cond, Then) and in the (Else)
	% branch are the same, therefore we will only calculate in one of them.
	% As it is here, we calculate for (Else) with the hope that it is
	% usually more efficient (only Else compared to both Cond and Then).
    collect_non_local_and_in_cond_regions_goal(Graph, LRBeforeProc,
		LRAfterProc, ResurRenamingProc, ResurRenamingAnnoProc,
		Cond, !NonLocalRegionProc, !InCondRegionsProc),
    collect_non_local_and_in_cond_regions_goal(Graph, LRBeforeProc,
		LRAfterProc, ResurRenamingProc, ResurRenamingAnnoProc,
		Then, !NonLocalRegionProc, !InCondRegionsProc),
    collect_non_local_regions_in_ite(Graph, LRBeforeProc, LRAfterProc,
		ResurRenamingProc, ResurRenamingAnnoProc, Else,
		!NonLocalRegionProc).

collect_non_local_and_in_cond_regions_expr(_, _, _, _, _, shorthand(_),
		!NonLocalRegionProc, !InCondRegionsProc) :- 
    unexpected(this_file, "collect_non_local_and_in_cond_regions_expr: "
		++ "shorthand not handled").

:- pred collect_non_local_and_in_cond_regions_case(rpt_graph::in,
	pp_region_set_table::in, pp_region_set_table::in,
	renaming_proc::in, renaming_annotation_proc::in, case::in,
	goal_path_regions_table::in, goal_path_regions_table::out,
	goal_path_regions_table::in, goal_path_regions_table::out) is det.

collect_non_local_and_in_cond_regions_case(Graph, LRBeforeProc, LRAfterProc,
		ResurRenamingProc, ResurRenamingAnnoProc, Case,
		!NonLocalRegionProc, !InCondRegionsProc) :-
    Case = case(_, Goal),
    collect_non_local_and_in_cond_regions_goal(Graph, LRBeforeProc,
		LRAfterProc, ResurRenamingProc, ResurRenamingAnnoProc, Goal,
		!NonLocalRegionProc, !InCondRegionsProc).

:- pred collect_non_local_regions_in_ite(rpt_graph::in,
	pp_region_set_table::in, pp_region_set_table::in, renaming_proc::in,
	renaming_annotation_proc::in, hlds_goal::in,
	goal_path_regions_table::in, goal_path_regions_table::out) is det.

collect_non_local_regions_in_ite(Graph, LRBeforeProc, LRAfterProc,
		ResurRenamingProc, ResurRenamingAnnoProc, GoalInIte,
		!NonLocalRegionProc) :-
	GoalInIte = hlds_goal(Expr, Info),
	( if	goal_is_atomic(Expr)
	  then
			ProgPoint = program_point_init(Info),
			ProgPoint = pp(_, GoalPath),
			map.lookup(LRBeforeProc, ProgPoint, LRBefore),
			map.lookup(LRAfterProc, ProgPoint, LRAfter),
			
			% XXX We may also need VoidVarRegionTable to be
			% included in RemovedAfter.
			set.difference(LRBefore, LRAfter, RemovedAfterNodes),
			set.difference(LRAfter, LRBefore, CreatedBeforeNodes),
			% Those sets need to subject to resurrection renaming
			% and annotations.
			% Apply resurrection renaming to those sets.
			% For each renaming annotation, the left one is put
			% into CreatedBefore, and the right one is put into
			% RemovedAfter.
			( if	map.search(ResurRenamingProc, ProgPoint,
						ResurRenaming0)
			  then	ResurRenaming = ResurRenaming0
			  else  ResurRenaming = map.init
			),
			set.fold(apply_region_renaming(Graph, ResurRenaming),
				RemovedAfterNodes, set.init,
				RemovedAfterRegions0),
			set.fold(apply_region_renaming(Graph, ResurRenaming),
				CreatedBeforeNodes, set.init,
				CreatedBeforeRegions0),

			( if	map.search(ResurRenamingAnnoProc, ProgPoint,
						ResurRenamingAnnos0)
			  then	ResurRenamingAnnos = ResurRenamingAnnos0
			  else	ResurRenamingAnnos = []
			),  
			list.foldl2(renaming_annotation_to_regions,
				ResurRenamingAnnos, set.init, LeftRegions,
				set.init, RightRegions),
			set.union(RemovedAfterRegions0, RightRegions,
				RemovedAfterRegions),
			set.union(CreatedBeforeRegions0, LeftRegions,
				CreatedBeforeRegions),
			record_non_local_regions(GoalPath, CreatedBeforeRegions,
				RemovedAfterRegions, !NonLocalRegionProc)
	  else
			collect_non_local_regions_in_ite_compound_goal(Graph,
				LRBeforeProc, LRAfterProc, ResurRenamingProc,
				ResurRenamingAnnoProc, GoalInIte,
				!NonLocalRegionProc)
	).

:- pred apply_region_renaming(rpt_graph::in, renaming::in, rptg_node::in,
	set(string)::in, set(string)::out) is det.

apply_region_renaming(Graph, Renaming, Node, !Regions) :-
	RegionName = rptg_lookup_region_name(Graph, Node),	
	( if	map.search(Renaming, RegionName, RenamedRegionName)
	  then	svset.insert(RenamedRegionName, !Regions)
	  else	svset.insert(RegionName, !Regions)
	).
	  		
:- pred renaming_annotation_to_regions(string::in,
	set(string)::in, set(string)::out,
	set(string)::in, set(string)::out) is det.

renaming_annotation_to_regions(RenameAnnotation, !LeftRegions,
		!RightRegions) :-
	( if	string.sub_string_search(RenameAnnotation, "=", Index)
	  then
	  		LeftRegion = string.substring(RenameAnnotation, 0,
				Index - 1),

			RightRegion = string.substring(RenameAnnotation,
				Index + 2,
				string.length(RenameAnnotation) - (Index + 2)),
			svset.insert(LeftRegion, !LeftRegions),
			svset.insert(RightRegion, !RightRegions)
	  else
	  		unexpected(this_file, "renaming_annotation_to_regions: "
				++ "annotation is not assignment")
	).

	% The non-local regions of an if-then-else will be attached to 
	% the goal path to the condition.
	% Non-local regions of an if-then-else are ones that are created
	% somewhere inside the if-then-else and not be removed inside it
	% (i.e., outlive the if-then-else's scope).
	% If a region is created in an if-then-else, it is created
	% in both (Cond, Then) and (Else) branches. (Of cource one of them
	% is in effect at runtime). If it is removed in the if-then-else
	% then it is also removed in both. Therefore it is enough to 
	% consider either (Cond, Then) or (Else). As said above,
	% we here choose to calculate for (Else).
	%
	% The algorithm is that: at each program point (inside the else),
	% the non-local set of regions is updated by including the regions
	% created before or at that program point and excluding those removed
	% at or after the program point.
	% Because if-then-else can be nestted, we need to update the
	% non-local sets of all the surrounding if-then-elses of this
	% program point.
	%
:- pred record_non_local_regions(goal_path::in, set(string)::in,
	set(string)::in, goal_path_regions_table::in,
	goal_path_regions_table::out) is det.

record_non_local_regions([], _, _, !NonLocalRegionProc).
record_non_local_regions(Path, Created, Removed, !NonLocalRegionProc) :-
	Path = [Step | Steps],
	(
		Step = step_ite_else
	->
		% The current NonLocalRegions are attached to the goal path to
		% the corresponding condition.
		PathToCond = [step_ite_cond | Steps],
		( if	map.search(!.NonLocalRegionProc, PathToCond,
					NonLocalRegions0)
		  then
				set.union(NonLocalRegions0, Created,
					NonLocalRegions1),
				set.difference(NonLocalRegions1, Removed,
					NonLocalRegions)
		  else
				set.difference(Created, Removed,
					NonLocalRegions)
		),
		% Only record if some non-local region(s) exist.
		( if	set.empty(NonLocalRegions)
		  then	true
		  else	svmap.set(PathToCond, NonLocalRegions,
					!NonLocalRegionProc)
		)
	;
		true
	),

	% Need to update the non-local sets of outer if-then-elses of this
	% one, if any.
	record_non_local_regions(Steps, Created, Removed, !NonLocalRegionProc).

:- pred collect_non_local_regions_in_ite_compound_goal(rpt_graph::in,
	pp_region_set_table::in, pp_region_set_table::in,
	renaming_proc::in, renaming_annotation_proc::in, hlds_goal::in,
	goal_path_regions_table::in, goal_path_regions_table::out) is det.

collect_non_local_regions_in_ite_compound_goal(Graph, LRBeforeProc,
		LRAfterProc, ResurRenamingProc, ResurRenamingAnnoProc,
		GoalInIte, !NonLocalRegionProc) :-
	GoalInIte = hlds_goal(Expr, _),
	(
		Expr = conj(_, [Conj | Conjs]),
		list.foldl(collect_non_local_regions_in_ite(Graph, LRBeforeProc,
						LRAfterProc, ResurRenamingProc,
						ResurRenamingAnnoProc), 
			[Conj | Conjs], !NonLocalRegionProc)
	;
		Expr = disj([Disj | Disjs]),
		list.foldl(collect_non_local_regions_in_ite(Graph, LRBeforeProc,
						LRAfterProc, ResurRenamingProc,
						ResurRenamingAnnoProc), 
			[Disj | Disjs], !NonLocalRegionProc)
	;
		Expr = switch(_, _, Cases),
		list.foldl(collect_non_local_regions_in_ite_case(Graph,
						LRBeforeProc, LRAfterProc,
						ResurRenamingProc,
						ResurRenamingAnnoProc),
			Cases, !NonLocalRegionProc)
	;
		Expr = negation(Goal),
		collect_non_local_regions_in_ite(Graph, LRBeforeProc,
			LRAfterProc, ResurRenamingProc, ResurRenamingAnnoProc,
			Goal, !NonLocalRegionProc)
	;
		Expr = scope(_, Goal),
		collect_non_local_regions_in_ite(Graph, LRBeforeProc,
			LRAfterProc, ResurRenamingProc, ResurRenamingAnnoProc,
			Goal, !NonLocalRegionProc)
	;
		Expr = if_then_else(_, Cond, Then, Else),
		collect_non_local_regions_in_ite(Graph, LRBeforeProc,
			LRAfterProc, ResurRenamingProc, ResurRenamingAnnoProc,
			Cond, !NonLocalRegionProc),
		collect_non_local_regions_in_ite(Graph, LRBeforeProc,
			LRAfterProc, ResurRenamingProc, ResurRenamingAnnoProc,
			Then, !NonLocalRegionProc),
		collect_non_local_regions_in_ite(Graph, LRBeforeProc,
			LRAfterProc, ResurRenamingProc, ResurRenamingAnnoProc,
			Else, !NonLocalRegionProc)
	;
		( Expr = unify(_, _, _, _, _) 
        ; Expr = plain_call(_, _, _, _, _, _) 
        ; Expr = conj(_, [])
        ; Expr = disj([])
        ; Expr = call_foreign_proc(_, _, _, _, _, _, _)
        ; Expr = generic_call(_, _, _, _)
        ; Expr = shorthand(_)
        ),
		unexpected(this_file,
            "collect_non_local_regions_in_ite_compound_goal: "
			++ "encountered atomic or unsupported goal")
	).

:- pred collect_non_local_regions_in_ite_case(rpt_graph::in,
	pp_region_set_table::in, pp_region_set_table::in,
	renaming_proc::in, renaming_annotation_proc::in, case::in,
	goal_path_regions_table::in, goal_path_regions_table::out) is det.

collect_non_local_regions_in_ite_case(Graph, LRBeforeProc, LRAfterProc, ResurRenamingProc, ResurRenamingAnnoProc, Case,
		!NonLocalRegionProc) :-
	Case = case(_, Goal),
	collect_non_local_regions_in_ite(Graph, LRBeforeProc, LRAfterProc,
		ResurRenamingProc, ResurRenamingAnnoProc, Goal,
		!NonLocalRegionProc).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

%-----------------------------------------------------------------------------%
%
% Collect regions created inside condition goals of if-then-elses.
%

	% The process here is very similar to that of
	% collect_non_local_regions_in_ite predicate.
	% The difference is that this predicate is used only in the scope of
	% a condition goal. 
	%
:- pred collect_regions_created_in_condition(rpt_graph::in,
	pp_region_set_table::in, pp_region_set_table::in,
	renaming_proc::in, renaming_annotation_proc::in, hlds_goal::in,
	goal_path_regions_table::in, goal_path_regions_table::out) is det.

collect_regions_created_in_condition(Graph, LRBeforeProc, LRAfterProc, 
		ResurRenamingProc, ResurRenamingAnnoProc, Cond,
		!InCondRegionsProc) :-
	Cond = hlds_goal(Expr, Info),
	( if	goal_is_atomic(Expr)
	  then
			ProgPoint = program_point_init(Info),
			ProgPoint = pp(_, GoalPath),
			map.lookup(LRBeforeProc, ProgPoint, LRBefore),
			map.lookup(LRAfterProc, ProgPoint, LRAfter),
			
			set.difference(LRAfter, LRBefore, CreatedNodes),
			% We need to apply renaming to this CreatedNodes set
			% and look up the renaming annotations after this
			% program point. For each renaming annotation the left
			% one is created and the right is removed.
			( if	map.search(ResurRenamingProc, ProgPoint,
						ResurRenaming0)
			  then	ResurRenaming = ResurRenaming0
			  else  ResurRenaming = map.init
			),
			set.fold(apply_region_renaming(Graph, ResurRenaming),
				CreatedNodes, set.init, CreatedRegions0),

			( if	map.search(ResurRenamingAnnoProc, ProgPoint,
						ResurRenamingAnnos0)
			  then	ResurRenamingAnnos = ResurRenamingAnnos0
			  else	ResurRenamingAnnos = []
			),  
			list.foldl2(renaming_annotation_to_regions,
				ResurRenamingAnnos, set.init, LeftRegions,
				set.init, _RightRegions),
			set.union(CreatedRegions0, LeftRegions, CreatedRegions),
		
			record_regions_created_in_condition(GoalPath,
				CreatedRegions, !InCondRegionsProc)
	  else
			collect_regions_created_in_condition_compound_goal(
				Graph, LRBeforeProc, LRAfterProc,
				ResurRenamingProc, ResurRenamingAnnoProc, Cond,
				!InCondRegionsProc)
	).
 
	% The regions created inside the condition of an if-then-else will
	% be attached to the goal path to the condition.
	%
	% We need to update the sets of all the conditions surrounding this
	% program point.
	%
:- pred record_regions_created_in_condition(goal_path::in, set(string)::in,
	goal_path_regions_table::in, goal_path_regions_table::out) is det.

record_regions_created_in_condition([], _, !InCondRegionsProc).
record_regions_created_in_condition(Path, Created, !InCondRegionsProc) :-
	Path = [Step | Steps],
	(
		Step = step_ite_cond
	->
		( if	map.search(!.InCondRegionsProc, Path, InCondRegions0)
		  then	set.union(InCondRegions0, Created,
					InCondRegions)
		  else	InCondRegions = Created
		),
		% Only record if the some region(s) is actually created inside
		% the condition.
		( if	set.empty(InCondRegions)
		  then	true
		  else	svmap.set(Path, InCondRegions, !InCondRegionsProc)
		)
	;
		true
	),
	record_regions_created_in_condition(Steps, Created, !InCondRegionsProc).

:- pred collect_regions_created_in_condition_compound_goal(rpt_graph::in,
	pp_region_set_table::in, pp_region_set_table::in,
	renaming_proc::in, renaming_annotation_proc::in, hlds_goal::in,
	goal_path_regions_table::in, goal_path_regions_table::out) is det.

collect_regions_created_in_condition_compound_goal(Graph, LRBeforeProc,
		LRAfterProc, ResurRenamingProc, ResurRenamingAnnoProc, GoalInIte,
		!InCondRegionsProc) :-
	GoalInIte = hlds_goal(Expr, _),
	(
		Expr = conj(_, [Conj | Conjs]),
		list.foldl(collect_regions_created_in_condition(Graph,
			LRBeforeProc, LRAfterProc, ResurRenamingProc,
			ResurRenamingAnnoProc), [Conj | Conjs],
			!InCondRegionsProc)
	;
		Expr = disj([Disj | Disjs]),
		list.foldl(collect_regions_created_in_condition(Graph, 
			LRBeforeProc, LRAfterProc, ResurRenamingProc,
			ResurRenamingAnnoProc), [Disj | Disjs],
			!InCondRegionsProc)
	;
		Expr = switch(_, _, Cases),
		list.foldl(
			collect_regions_created_in_condition_case(Graph,
			LRBeforeProc, LRAfterProc, ResurRenamingProc,
			ResurRenamingAnnoProc),
			Cases, !InCondRegionsProc)
	;
		Expr = negation(Goal),
		collect_regions_created_in_condition(Graph, LRBeforeProc,
			LRAfterProc, ResurRenamingProc,
			ResurRenamingAnnoProc, Goal, !InCondRegionsProc)
	;
		Expr = scope(_, Goal),
		collect_regions_created_in_condition(Graph, LRBeforeProc,
			LRAfterProc, ResurRenamingProc,
			ResurRenamingAnnoProc, Goal, !InCondRegionsProc)
	;
		Expr = if_then_else(_, Cond, Then, Else),
		collect_regions_created_in_condition(Graph, LRBeforeProc,
			LRAfterProc, ResurRenamingProc,
			ResurRenamingAnnoProc, Cond, !InCondRegionsProc),
		collect_regions_created_in_condition(Graph, LRBeforeProc,
			LRAfterProc, ResurRenamingProc,
			ResurRenamingAnnoProc, Then, !InCondRegionsProc),
		collect_regions_created_in_condition(Graph, LRBeforeProc,
			LRAfterProc, ResurRenamingProc,
			ResurRenamingAnnoProc, Else, !InCondRegionsProc)
	;
		( Expr = unify(_, _, _, _, _) 
        ; Expr = plain_call(_, _, _, _, _, _) 
        ; Expr = conj(_, [])
        ; Expr = disj([])
        ; Expr = call_foreign_proc(_, _, _, _, _, _, _)
        ; Expr = generic_call(_, _, _, _)
        ; Expr = shorthand(_)
        ),
		unexpected(this_file,
            "collect_regions_created_in_condition_compound_goal: "
			++ "encountered atomic or unsupported goal")
	).

:- pred collect_regions_created_in_condition_case(rpt_graph::in,
	pp_region_set_table::in, pp_region_set_table::in,
	renaming_proc::in, renaming_annotation_proc::in, case::in,
	goal_path_regions_table::in, goal_path_regions_table::out) is det.

collect_regions_created_in_condition_case(Graph, LRBeforeProc, LRAfterProc,
		ResurRenamingProc, ResurRenamingAnnoProc,
		Case, !InCondRegionsProc) :-
	Case = case(_, Goal),
	collect_regions_created_in_condition(Graph, LRBeforeProc, LRAfterProc,
		ResurRenamingProc, ResurRenamingAnnoProc,
		Goal, !InCondRegionsProc).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

%-----------------------------------------------------------------------------%
% 
% Collect regions that need to be renamed, i.e., both created in a condition
% goal and non-local to the corresponding if-then-else.
%

collect_ite_renamed_regions(InCondRegionsTable, NonLocalRegionsTable,
		IteRenamedRegionsTable) :-
	map.foldl(collect_ite_renamed_regions_proc(NonLocalRegionsTable), 
		InCondRegionsTable, map.init, IteRenamedRegionsTable).

:- pred collect_ite_renamed_regions_proc(proc_goal_path_regions_table::in,
	pred_proc_id::in, goal_path_regions_table::in,
	proc_goal_path_regions_table::in, proc_goal_path_regions_table::out)
	is det.

collect_ite_renamed_regions_proc(NonLocalRegionsTable, PPId,
		InCondRegionsProc, !IteRenamedRegionTable) :-
	( if	map.search(NonLocalRegionsTable, PPId, NonLocalRegionsProc)
	  then
			map.foldl(collect_ite_renamed_regions_ite(
				NonLocalRegionsProc),
				InCondRegionsProc,
				map.init, IteRenamedRegionProc),
			( if	map.count(IteRenamedRegionProc) = 0
			  then	true
			  else	svmap.set(PPId, IteRenamedRegionProc,
						!IteRenamedRegionTable)
			)
	  else	true
	).

:- pred collect_ite_renamed_regions_ite(goal_path_regions_table::in,
	goal_path::in, set(string)::in,
	goal_path_regions_table::in, goal_path_regions_table::out) is det.

collect_ite_renamed_regions_ite(NonLocalRegionsProc, PathToCond,
		InCondRegions, !IteRenamedRegionProc) :-
	( if	map.search(NonLocalRegionsProc, PathToCond, NonLocalRegions)
	  then
			set.intersect(NonLocalRegions, InCondRegions,
				RenamedRegions),
			( if	set.empty(RenamedRegions)
			  then	true
			  else	svmap.set(PathToCond, RenamedRegions,
						!IteRenamedRegionProc)
			)
	  else	true
	).

%-----------------------------------------------------------------------------%
%
% Derive necessary renaming.
%

collect_ite_renaming(ModuleInfo, RptaInfoTable, IteRenamedRegionTable,
		IteRenamingTable) :-
	map.foldl(collect_ite_renaming_proc(ModuleInfo, RptaInfoTable),
		IteRenamedRegionTable, map.init, IteRenamingTable).

:- pred collect_ite_renaming_proc(module_info::in, rpta_info_table::in,
	pred_proc_id::in, goal_path_regions_table::in,
	renaming_table::in, renaming_table::out) is det.

collect_ite_renaming_proc(ModuleInfo, RptaInfoTable,
		PPId, IteRenamedRegionProc, !IteRenamingTable) :-
	module_info_proc_info(ModuleInfo, PPId, ProcInfo0),
	fill_goal_path_slots(ModuleInfo, ProcInfo0, ProcInfo),
	proc_info_get_goal(ProcInfo, Goal),
	map.lookup(RptaInfoTable, PPId, RptaInfo),
	RptaInfo = rpta_info(Graph, _),
	collect_ite_renaming_goal(IteRenamedRegionProc, Graph, Goal,
		map.init, IteRenamingProc),
	svmap.set(PPId, IteRenamingProc, !IteRenamingTable).

:- pred collect_ite_renaming_goal(goal_path_regions_table::in,
	rpt_graph::in, hlds_goal::in, renaming_proc::in,
	renaming_proc::out) is det.

collect_ite_renaming_goal(IteRenamedRegionProc, Graph, Goal,
		!IteRenamingProc) :-
	Goal = hlds_goal(Expr, _),
	collect_ite_renaming_expr(Expr, IteRenamedRegionProc, Graph,
		!IteRenamingProc).

:- pred collect_ite_renaming_expr(hlds_goal_expr::in,
	goal_path_regions_table::in, rpt_graph::in, renaming_proc::in,
	renaming_proc::out) is det.

collect_ite_renaming_expr(conj(_, Conjs), IteRenamedRegionProc,
		Graph, !IteRenamingProc) :- 
    list.foldl(collect_ite_renaming_goal(IteRenamedRegionProc, Graph),
		Conjs, !IteRenamingProc). 

collect_ite_renaming_expr(plain_call(_, _, _, _, _, _), _, _,
		!IteRenamingProc).
collect_ite_renaming_expr(generic_call(_, _, _, _), _, _, 
		!IteRenamingProc).
collect_ite_renaming_expr(call_foreign_proc(_, _, _, _, _, _, _),
		_, _, !IteRenamingProc).

collect_ite_renaming_expr(switch(_, _, Cases), IteRenamedRegionProc,
		Graph, !IteRenamingProc) :- 
    list.foldl(
		collect_ite_renaming_case(IteRenamedRegionProc, Graph),
		Cases, !IteRenamingProc).
collect_ite_renaming_expr(disj(Disjs), IteRenamedRegionProc, Graph,
		!IteRenamingProc) :-
    list.foldl(
		collect_ite_renaming_goal(IteRenamedRegionProc, Graph),
		Disjs, !IteRenamingProc). 
collect_ite_renaming_expr(negation(Goal), IteRenamedRegionProc, Graph,
		!IteRenamingProc) :- 
    collect_ite_renaming_goal(IteRenamedRegionProc, Graph, Goal,
		!IteRenamingProc). 
collect_ite_renaming_expr(unify(_, _, _, _, _), _, _, !IteRenamingProc).

collect_ite_renaming_expr(scope(_, Goal), IteRenamedRegionProc,
		Graph, !IteRenamingProc) :- 
	collect_ite_renaming_goal(IteRenamedRegionProc, Graph, Goal,
		!IteRenamingProc).

collect_ite_renaming_expr(shorthand(_), _, _, !IteRenamingProc) :- 
    unexpected(this_file,
		"collect_ite_renaming_expr: shorthand not handled").

collect_ite_renaming_expr(Expr, IteRenamedRegionProc, Graph,
		!IteRenamingProc) :- 
	Expr = if_then_else(_, Cond, Then, Else),

	% Renaming for if-then-else only happens in condition goals.
	collect_ite_renaming_in_condition(IteRenamedRegionProc, Graph, Cond,
		!IteRenamingProc),

    collect_ite_renaming_goal(IteRenamedRegionProc, Graph, Then,
		!IteRenamingProc),
    collect_ite_renaming_goal(IteRenamedRegionProc, Graph, Else,
		!IteRenamingProc).

:- pred collect_ite_renaming_case(goal_path_regions_table::in, 
	rpt_graph::in, case::in, renaming_proc::in,
	renaming_proc::out) is det.

collect_ite_renaming_case(IteRenamedRegionProc, Graph, Case,
		!IteRenamingProc) :-
	Case = case(_, Goal),
	collect_ite_renaming_goal(IteRenamedRegionProc, Graph, Goal,
		!IteRenamingProc).

	% Introduce renaming for each program point in a condition goal.
	%
:- pred collect_ite_renaming_in_condition(
	goal_path_regions_table::in, rpt_graph::in, hlds_goal::in,
	renaming_proc::in, renaming_proc::out) is det.

collect_ite_renaming_in_condition(IteRenamedRegionProc, Graph, Cond,
		!IteRenamingProc) :-
	Cond = hlds_goal(Expr, Info),
	( if	goal_is_atomic(Expr)
	  then
			ProgPoint = program_point_init(Info),
			% It is enough to look for the regions to be renamed 
			% at the closest condition because if a region is 
			% to be renamed for a compounding if-then-else of the
			% closest if-then-else then it also needs to be renamed
			% for the closest if-then-else. 
			ProgPoint = pp(_, GoalPath),
			get_closest_condition_in_goal_path(GoalPath,
				PathToClosestCond, 0, HowMany),
			( if	map.search(IteRenamedRegionProc,
						PathToClosestCond,
						RenamedRegions)
			  then
					set.fold(record_ite_renaming(ProgPoint,
						HowMany, Graph),
						RenamedRegions,
						!IteRenamingProc)
			  else
					% No region needs to be renamed due to
					% if-then-else covering this program
					% point.
					true
			)
	  else
			collect_ite_renaming_in_condition_compound_goal(
				IteRenamedRegionProc, Graph, Cond,
				!IteRenamingProc)
	).

	% A renaming is of the form: R --> R_ite_HowMany.
	%
:- pred record_ite_renaming(program_point::in, int::in, rpt_graph::in,
	string::in, renaming_proc::in, renaming_proc::out) is det.

record_ite_renaming(ProgPoint, HowMany, _Graph, RegName, !IteRenamingProc) :-
	NewName = RegName ++ "_ite_" ++ string.int_to_string(HowMany),
	( if	map.search(!.IteRenamingProc, ProgPoint, IteRenaming0)
	  then 	svmap.set(RegName, NewName, IteRenaming0, IteRenaming)
	  else	svmap.set(RegName, NewName, map.init, IteRenaming)
	),
	svmap.set(ProgPoint, IteRenaming, !IteRenamingProc).

:- pred collect_ite_renaming_in_condition_compound_goal(
	goal_path_regions_table::in, rpt_graph::in, hlds_goal::in,
	renaming_proc::in, renaming_proc::out) is det.

collect_ite_renaming_in_condition_compound_goal(IteRenamedRegionProc,
		Graph, GoalInCond, !IteRenamingProc) :-
	GoalInCond = hlds_goal(Expr, _),
	(
		Expr = conj(_, [Conj | Conjs]),
		list.foldl(collect_ite_renaming_in_condition(
			IteRenamedRegionProc, Graph), [Conj | Conjs],
			!IteRenamingProc)
	;
		Expr = disj([Disj | Disjs]),
		list.foldl(collect_ite_renaming_in_condition(
			IteRenamedRegionProc, Graph), [Disj | Disjs],
			!IteRenamingProc)
	;
		Expr = switch(_, _, Cases),
		list.foldl(
			collect_ite_renaming_in_condition_case(
				IteRenamedRegionProc, Graph), Cases,
				!IteRenamingProc)
	;
		Expr = negation(Goal),
		collect_ite_renaming_in_condition(IteRenamedRegionProc,
			Graph, Goal, !IteRenamingProc)
	;
		Expr = scope(_, Goal),
		collect_ite_renaming_in_condition(IteRenamedRegionProc,
			Graph, Goal, !IteRenamingProc)
	;
		Expr = if_then_else(_, Cond, Then, Else),
		collect_ite_renaming_in_condition(IteRenamedRegionProc,
			Graph, Cond, !IteRenamingProc),
		collect_ite_renaming_in_condition(IteRenamedRegionProc,
			Graph, Then, !IteRenamingProc),
		collect_ite_renaming_in_condition(IteRenamedRegionProc,
			Graph, Else, !IteRenamingProc)
	;
		( Expr = unify(_, _, _, _, _) 
        ; Expr = plain_call(_, _, _, _, _, _) 
        ; Expr = conj(_, [])
        ; Expr = disj([])
        ; Expr = call_foreign_proc(_, _, _, _, _, _, _)
        ; Expr = generic_call(_, _, _, _)
        ; Expr = shorthand(_)
        ),
		unexpected(this_file,
            "collect_ite_renaming_in_condition_compound_goal: "
			++ "encountered atomic or unsupported goal")
	).

:- pred collect_ite_renaming_in_condition_case(
	goal_path_regions_table::in, rpt_graph::in, case::in,
	renaming_proc::in, renaming_proc::out) is det.

collect_ite_renaming_in_condition_case(IteRenamedRegionProc, Graph, Case,
		!IteRenamingProc) :-
	Case = case(_, Goal),
	collect_ite_renaming_in_condition(IteRenamedRegionProc, Graph, Goal,
		!IteRenamingProc).

	% This predicate receives a goal path (to some goal) and returns
	% the subpath to the closest condition containing the goal. It also 
	% returns the number of conditions that contain the goal.
	% If the goal is in no condition, the output path is empty.
	% e.g., ( if
	%			( if
	%				goal
	%				...
	% then HowMany is 2.
	%
:- pred get_closest_condition_in_goal_path(goal_path::in,
	goal_path::out, int::in, int::out) is det.
get_closest_condition_in_goal_path([], [], !HowMany).
get_closest_condition_in_goal_path([Step | Steps], PathToCond, !HowMany) :-
    ( if    Step = step_ite_cond
      then
            PathToCond = [Step | Steps],
            get_closest_condition_in_goal_path(Steps, _, !.HowMany, HowMany),
            !:HowMany = HowMany + 1
      else
            get_closest_condition_in_goal_path(Steps, PathToCond, !HowMany)
    ).

%-----------------------------------------------------------------------------%
%
% Derive necessary reverse renaming.
%

collect_ite_annotation(IteRenamedRegionTable, ExecPathTable,
		RptaInfoTable, IteRenamingTable, IteAnnotationTable) :-
	map.foldl(collect_ite_annotation_proc(ExecPathTable, RptaInfoTable,
					IteRenamingTable),
		IteRenamedRegionTable, map.init, IteAnnotationTable).

:- pred collect_ite_annotation_proc(execution_path_table::in,
	rpta_info_table::in, renaming_table::in, pred_proc_id::in,
	goal_path_regions_table::in,
	renaming_annotation_table::in, renaming_annotation_table::out) is det.

collect_ite_annotation_proc(ExecPathTable, RptaInfoTable, IteRenamingTable,
		PPId, IteRenamedRegionProc, !IteAnnotationTable) :-
	map.lookup(ExecPathTable, PPId, ExecPaths),
	map.lookup(RptaInfoTable, PPId, RptaInfo),
	map.lookup(IteRenamingTable, PPId, IteRenamingProc),
	RptaInfo = rpta_info(Graph, _),
	map.foldl(collect_ite_annotation_region_names(ExecPaths, Graph,
		IteRenamingProc),
		IteRenamedRegionProc, map.init, IteAnnotationProc),
	svmap.set(PPId, IteAnnotationProc, !IteAnnotationTable).

:- pred collect_ite_annotation_region_names(list(execution_path)::in,
	rpt_graph::in, renaming_proc::in, goal_path::in, set(string)::in,
	renaming_annotation_proc::in, renaming_annotation_proc::out) is det.

collect_ite_annotation_region_names(ExecPaths, Graph, IteRenamingProc,
		PathToCond, RenamedRegions, !IteAnnotationProc) :-
	(
		PathToCond = [],
		unexpected(this_file,
			"collect_ite_annotation_region_set: " ++
			"empty path to condition.")
	;
		PathToCond = [_ | Steps],
		PathToThen = [step_ite_then | Steps],
		get_closest_condition_in_goal_path(PathToCond, _, 0, HowMany),
		list.foldl(collect_ite_annotation_exec_path(Graph,
			IteRenamingProc, PathToThen,
			RenamedRegions, HowMany), ExecPaths,
			!IteAnnotationProc)
	).

:- pred collect_ite_annotation_exec_path(rpt_graph::in,
	renaming_proc::in, goal_path::in,
	set(string)::in, int::in, execution_path::in,
	renaming_annotation_proc::in, renaming_annotation_proc::out) is det.

collect_ite_annotation_exec_path(_, _, _, _, _, [], !IteAnnotationProc).
collect_ite_annotation_exec_path(Graph, IteRenamingProc, PathToThen,
		RenamedRegions, HowMany,
		[ProgPoint - _ | ProgPoint_Goals], !IteAnnotationProc) :-
	ProgPoint = pp(_, GoalPath),
	( if	list.append(_, PathToThen, GoalPath)
	  then
			% This is the first goal in the corresponding then
			% branch, we need to introduce reverse renaming at
			% this point.
			set.fold(introduce_reverse_renaming(Graph, ProgPoint,
				IteRenamingProc, HowMany),
				RenamedRegions, !IteAnnotationProc)
	  else
			collect_ite_annotation_exec_path(Graph, IteRenamingProc,
				PathToThen, RenamedRegions, HowMany,
				ProgPoint_Goals, !IteAnnotationProc)
	).

	% The reverse renaming annotation is in the form: R = R_ite_HowMany.
	% The annotation is attached to the program point but actually means
	% to be added before the program point.
	% If there exists a renaming at the program point related to R, e.g.,
	% R --> R_1, then the annotation is R_1 = R_ite_HowMany.
	%
:- pred introduce_reverse_renaming(rpt_graph::in, program_point::in,
	renaming_proc::in, int::in, string::in,
	renaming_annotation_proc::in, renaming_annotation_proc::out) is det.

introduce_reverse_renaming(_Graph, ProgPoint, IteRenamingProc,
		HowMany, RegName, !IteAnnotationProc) :-
	RightHand =
		" = " ++ RegName ++ "_ite_" ++ string.int_to_string(HowMany),
	( if	map.search(IteRenamingProc, ProgPoint, Renaming)
	  then
			( if	map.search(Renaming, RegName, RenameTo)
			  then
					Anno = RenameTo ++ RightHand 
			  else
					Anno = RegName ++ RightHand 
			)
	  else
			% No renaming exists at this program point.
			Anno = RegName ++ RightHand
	),
	record_annotation(ProgPoint, Anno, !IteAnnotationProc).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "rbmm.condition_renaming.m".

%-----------------------------------------------------------------------------%

