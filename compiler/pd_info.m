%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1998-2001, 2003-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: pd_info.m
% Main author: stayl
%
% Types for deforestation.
%
%-----------------------------------------------------------------------------%

:- module transform_hlds__pd_info.

:- interface.

:- import_module hlds__hlds_goal.
:- import_module hlds__hlds_module.
:- import_module hlds__hlds_pred.
:- import_module hlds__instmap.
:- import_module parse_tree__prog_data.
:- import_module transform_hlds__pd_term.

:- import_module bool.
:- import_module counter.
:- import_module io.
:- import_module list.
:- import_module map.
:- import_module set.
:- import_module std_util.

:- type pd_info
    --->    pd_info(
                module_info		    :: module_info,
                maybe_unfold_info	:: maybe(unfold_info),
                goal_version_index	:: goal_version_index,
                versions		    :: version_index,
                proc_arg_info		:: pd_arg_info,
                counter			    :: counter,
                global_term_info	:: global_term_info,
                parent_versions		:: set(pred_proc_id),
                depth			    :: int,
                created_versions	:: set(pred_proc_id),
                useless_versions	:: useless_versions
            ).

    % Map from list of called preds in the conjunctions
    % to the specialised versions.
:- type goal_version_index == map(list(pred_proc_id), list(pred_proc_id)).

:- type useless_versions == set(pair(pred_proc_id)).

    % Map from version id to the info about the version.
:- type version_index == map(pred_proc_id, version_info).

:- pred pd_info_init(module_info::in, pd_arg_info::in, pd_info::out) is det.

:- pred pd_info_init_unfold_info(pred_proc_id::in, pred_info::in,
	proc_info::in, pd_info::in, pd_info::out) is det.

:- pred pd_info_get_module_info(pd_info::in, module_info::out) is det.
:- pred pd_info_get_unfold_info(pd_info::in, unfold_info::out) is det.
:- pred pd_info_get_goal_version_index(pd_info::in, goal_version_index::out)
	is det.
:- pred pd_info_get_versions(pd_info::in, version_index::out) is det.
:- pred pd_info_get_proc_arg_info(pd_info::in, pd_arg_info::out) is det.
:- pred pd_info_get_counter(pd_info::in, counter::out) is det.
:- pred pd_info_get_global_term_info(pd_info::in, global_term_info::out)
    is det.
:- pred pd_info_get_parent_versions(pd_info::in, set(pred_proc_id)::out)
    is det.
:- pred pd_info_get_depth(pd_info::in, int::out) is det.
:- pred pd_info_get_created_versions(pd_info::in, set(pred_proc_id)::out)
	is det.
:- pred pd_info_get_useless_versions(pd_info::in, useless_versions::out)
    is det.

:- pred pd_info_set_module_info(module_info::in,
	pd_info::in, pd_info::out) is det.
:- pred pd_info_set_unfold_info(unfold_info::in,
	pd_info::in, pd_info::out) is det.
:- pred pd_info_set_goal_version_index(goal_version_index::in,
	pd_info::in, pd_info::out) is det.
:- pred pd_info_set_versions(version_index::in,
	pd_info::in, pd_info::out) is det.
:- pred pd_info_set_proc_arg_info(pd_arg_info::in,
	pd_info::in, pd_info::out) is det.
:- pred pd_info_set_counter(counter::in,
	pd_info::in, pd_info::out) is det.
:- pred pd_info_set_global_term_info(global_term_info::in,
	pd_info::in, pd_info::out) is det.
:- pred pd_info_set_parent_versions(set(pred_proc_id)::in,
	pd_info::in, pd_info::out) is det.
:- pred pd_info_set_depth(int::in,
	pd_info::in, pd_info::out) is det.
:- pred pd_info_set_created_versions(set(pred_proc_id)::in,
	pd_info::in, pd_info::out) is det.
:- pred pd_info_set_useless_versions(useless_versions::in,
	pd_info::in, pd_info::out) is det.

:- pred pd_info_update_goal(hlds_goal::in, pd_info::in, pd_info::out) is det.

:- pred pd_info_bind_var_to_functor(prog_var::in, cons_id::in,
	pd_info::in, pd_info::out) is det.

:- pred pd_info_unset_unfold_info(pd_info::in, pd_info::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds__det_util.
:- import_module check_hlds__inst_match.
:- import_module hlds__hlds_data.
:- import_module hlds__hlds_goal.
:- import_module hlds__hlds_pred.
:- import_module libs__globals.
:- import_module mdbcomp__prim_data.
:- import_module parse_tree__prog_data.
:- import_module parse_tree__prog_util.
:- import_module transform_hlds__pd_debug.
:- import_module transform_hlds__pd_util.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module require.
:- import_module string.
:- import_module term.

pd_info_init(ModuleInfo, ProcArgInfos, PDInfo) :-
	map__init(GoalVersionIndex),
	map__init(Versions),
	set__init(ParentVersions),
	pd_term__global_term_info_init(GlobalInfo),
	set__init(CreatedVersions),
	set__init(UselessVersions),
	PDInfo = pd_info(ModuleInfo, no, GoalVersionIndex, Versions,
		ProcArgInfos, counter__init(0), GlobalInfo, ParentVersions,
		0, CreatedVersions, UselessVersions).

pd_info_init_unfold_info(PredProcId, PredInfo, ProcInfo, !PDInfo) :-
	pd_info_get_module_info(!.PDInfo, ModuleInfo),
	proc_info_get_initial_instmap(ProcInfo, ModuleInfo, InstMap),
	CostDelta = 0,
	pd_term__local_term_info_init(LocalTermInfo),
	set__singleton_set(Parents, PredProcId),
	UnfoldInfo = unfold_info(ProcInfo, InstMap, CostDelta, LocalTermInfo,
		PredInfo, Parents, PredProcId, no, 0, no),
	pd_info_set_unfold_info(UnfoldInfo, !PDInfo).

pd_info_get_module_info(PDInfo, PDInfo ^ module_info).
pd_info_get_unfold_info(PDInfo, UnfoldInfo) :-
	MaybeUnfoldInfo = PDInfo ^ maybe_unfold_info,
	(
		MaybeUnfoldInfo = yes(UnfoldInfo)
	;
		MaybeUnfoldInfo = no,
		error("pd_info_get_unfold_info: unfold_info not set.")
	).
pd_info_get_goal_version_index(PDInfo, PDInfo ^ goal_version_index).
pd_info_get_versions(PDInfo, PDInfo ^ versions).
pd_info_get_proc_arg_info(PDInfo, PDInfo ^ proc_arg_info).
pd_info_get_counter(PDInfo, PDInfo ^ counter).
pd_info_get_global_term_info(PDInfo, PDInfo ^ global_term_info).
pd_info_get_parent_versions(PDInfo, PDInfo ^ parent_versions).
pd_info_get_depth(PDInfo, PDInfo ^ depth).
pd_info_get_created_versions(PDInfo, PDInfo ^ created_versions).
pd_info_get_useless_versions(PDInfo, PDInfo ^ useless_versions).

pd_info_set_module_info(ModuleInfo, PDInfo,
	PDInfo ^ module_info := ModuleInfo).
pd_info_set_unfold_info(UnfoldInfo, PDInfo,
	PDInfo ^ maybe_unfold_info := yes(UnfoldInfo)).
pd_info_unset_unfold_info(PDInfo,
	PDInfo ^ maybe_unfold_info := no).
pd_info_set_goal_version_index(Index, PDInfo,
	PDInfo ^ goal_version_index := Index).
pd_info_set_versions(Versions, PDInfo,
	PDInfo ^ versions := Versions).
pd_info_set_proc_arg_info(ProcArgInfo, PDInfo,
	PDInfo ^ proc_arg_info := ProcArgInfo).
pd_info_set_counter(Counter, PDInfo,
	PDInfo ^ counter := Counter).
pd_info_set_global_term_info(TermInfo, PDInfo,
	PDInfo ^ global_term_info := TermInfo).
pd_info_set_parent_versions(Parents, PDInfo,
	PDInfo ^ parent_versions := Parents).
pd_info_set_depth(Depth, PDInfo, PDInfo ^ depth := Depth).
pd_info_set_created_versions(Versions, PDInfo,
	PDInfo ^ created_versions := Versions).
pd_info_set_useless_versions(Versions, PDInfo,
	PDInfo ^ useless_versions := Versions).

pd_info_update_goal(_ - GoalInfo, !PDInfo) :-
	pd_info_get_instmap(!.PDInfo, InstMap0),
	goal_info_get_instmap_delta(GoalInfo, Delta),
	instmap__apply_instmap_delta(InstMap0, Delta, InstMap),
	pd_info_set_instmap(InstMap, !PDInfo).

pd_info_bind_var_to_functor(Var, ConsId, !PDInfo) :-
	pd_info_get_instmap(!.PDInfo, InstMap0),
	pd_info_get_module_info(!.PDInfo, ModuleInfo0),
	pd_info_get_proc_info(!.PDInfo, ProcInfo),
	proc_info_vartypes(ProcInfo, VarTypes),
	map__lookup(VarTypes, Var, Type),
	instmap__bind_var_to_functor(Var, Type, ConsId, InstMap0, InstMap,
		ModuleInfo0, ModuleInfo),
	pd_info_set_instmap(InstMap, !PDInfo),
	pd_info_set_module_info(ModuleInfo, !PDInfo).

%-----------------------------------------------------------------------------%

:- interface.

	% unfold_info contains information used while searching a procedure
	% body for unfolding and deforestation opportunities.
:- type unfold_info
	--->	unfold_info(
                proc_info	    :: proc_info,
                instmap		    :: instmap,
                cost_delta	    :: int,
                                % improvement in cost measured while
                                % processing this procedure
                local_term_info	:: local_term_info,
                                % information used to prevent
                                % infinite unfolding within the
                                % current procedure.
                pred_info	    :: pred_info,
                parents		    :: set(pred_proc_id),
                pred_proc_id	:: pred_proc_id,
                                % current pred_proc_id
                changed		    :: bool,
                                % has anything changed
                size_delta	    :: int,
                                % increase in size measured while
                                % processing this procedure
                rerun_det	    :: bool
                                % does determinism analysis
                                % need to be rerun.
            ).

	% pd_arg_info records which procedures have arguments for which
	% it might be worthwhile to attempt deforestation if there
	% is extra information about them, and the branches of the single
	% branched goal in the top level conjunction which produce that extra
	% information.
:- type pd_arg_info == map(pred_proc_id, pd_proc_arg_info).

:- type pd_proc_arg_info	==	pd_branch_info(int).

:- type pd_branch_info(T)
	--->	pd_branch_info(
                branch_info_map(T),
                set(T),		% variables for which we want
                            % extra left context
                set(T)		% outputs for which we have no
                            % extra information
            ).

	% Vars for which there is extra information at the end
	% of some branches, and the branches which add the extra
	% information (numbered from 1).
:- type branch_info_map(T)	==	map(T, set(int)).

:- pred pd_info_get_proc_info(pd_info::in, proc_info::out) is det.
:- pred pd_info_get_instmap(pd_info::in, instmap::out) is det.
:- pred pd_info_get_cost_delta(pd_info::in, int::out) is det.
:- pred pd_info_get_local_term_info(pd_info::in, local_term_info::out) is det.
:- pred pd_info_get_pred_info(pd_info::in, pred_info::out) is det.
:- pred pd_info_get_parents(pd_info::in, set(pred_proc_id)::out) is det.
:- pred pd_info_get_pred_proc_id(pd_info::in, pred_proc_id::out) is det.
:- pred pd_info_get_changed(pd_info::in, bool::out) is det.
:- pred pd_info_get_size_delta(pd_info::in, int::out) is det.
:- pred pd_info_get_rerun_det(pd_info::in, bool::out) is det.

:- pred pd_info_set_proc_info(proc_info::in,
	pd_info::in, pd_info::out) is det.
:- pred pd_info_set_instmap(instmap::in,
	pd_info::in, pd_info::out) is det.
:- pred pd_info_set_cost_delta(int::in,
	pd_info::in, pd_info::out) is det.
:- pred pd_info_set_local_term_info(local_term_info::in,
	pd_info::in, pd_info::out) is det.
:- pred pd_info_set_pred_info(pred_info::in,
	pd_info::in, pd_info::out) is det.
:- pred pd_info_set_parents(set(pred_proc_id)::in,
	pd_info::in, pd_info::out) is det.
:- pred pd_info_set_pred_proc_id(pred_proc_id::in,
	pd_info::in, pd_info::out) is det.
:- pred pd_info_set_changed(bool::in,
	pd_info::in, pd_info::out) is det.
:- pred pd_info_set_size_delta(int::in,
	pd_info::in, pd_info::out) is det.
:- pred pd_info_set_rerun_det(bool::in,
	pd_info::in, pd_info::out) is det.
:- pred pd_info_incr_cost_delta(int::in,
	pd_info::in, pd_info::out) is det.
:- pred pd_info_incr_size_delta(int::in,
	pd_info::in, pd_info::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

pd_info_get_proc_info(PDInfo, UnfoldInfo ^ proc_info) :-
	pd_info_get_unfold_info(PDInfo, UnfoldInfo).
pd_info_get_instmap(PDInfo, UnfoldInfo ^ instmap) :-
	pd_info_get_unfold_info(PDInfo, UnfoldInfo).
pd_info_get_cost_delta(PDInfo, UnfoldInfo ^ cost_delta) :-
	pd_info_get_unfold_info(PDInfo, UnfoldInfo).
pd_info_get_local_term_info(PDInfo, UnfoldInfo ^ local_term_info) :-
	pd_info_get_unfold_info(PDInfo, UnfoldInfo).
pd_info_get_pred_info(PDInfo, UnfoldInfo ^ pred_info) :-
	pd_info_get_unfold_info(PDInfo, UnfoldInfo).
pd_info_get_parents(PDInfo, UnfoldInfo ^ parents) :-
	pd_info_get_unfold_info(PDInfo, UnfoldInfo).
pd_info_get_pred_proc_id(PDInfo, UnfoldInfo ^ pred_proc_id) :-
	pd_info_get_unfold_info(PDInfo, UnfoldInfo).
pd_info_get_changed(PDInfo, UnfoldInfo ^ changed) :-
	pd_info_get_unfold_info(PDInfo, UnfoldInfo).
pd_info_get_size_delta(PDInfo, UnfoldInfo ^ size_delta) :-
	pd_info_get_unfold_info(PDInfo, UnfoldInfo).
pd_info_get_rerun_det(PDInfo, UnfoldInfo ^ rerun_det) :-
	pd_info_get_unfold_info(PDInfo, UnfoldInfo).

pd_info_set_proc_info(ProcInfo, !PDInfo) :-
	pd_info_get_unfold_info(!.PDInfo, UnfoldInfo0),
	UnfoldInfo = UnfoldInfo0 ^ proc_info := ProcInfo,
	pd_info_set_unfold_info(UnfoldInfo, !PDInfo).
pd_info_set_instmap(InstMap, !PDInfo) :-
	pd_info_get_unfold_info(!.PDInfo, UnfoldInfo0),
	UnfoldInfo = UnfoldInfo0 ^ instmap := InstMap,
	pd_info_set_unfold_info(UnfoldInfo, !PDInfo).
pd_info_set_cost_delta(CostDelta, !PDInfo) :-
	pd_info_get_unfold_info(!.PDInfo, UnfoldInfo0),
	UnfoldInfo = UnfoldInfo0 ^ cost_delta := CostDelta,
	pd_info_set_unfold_info(UnfoldInfo, !PDInfo).
pd_info_set_local_term_info(TermInfo, !PDInfo) :-
	pd_info_get_unfold_info(!.PDInfo, UnfoldInfo0),
	UnfoldInfo = UnfoldInfo0 ^ local_term_info := TermInfo,
	pd_info_set_unfold_info(UnfoldInfo, !PDInfo).
pd_info_set_pred_info(PredInfo, !PDInfo) :-
	pd_info_get_unfold_info(!.PDInfo, UnfoldInfo0),
	UnfoldInfo = UnfoldInfo0 ^ pred_info := PredInfo,
	pd_info_set_unfold_info(UnfoldInfo, !PDInfo).
pd_info_set_parents(Parents, !PDInfo) :-
	pd_info_get_unfold_info(!.PDInfo, UnfoldInfo0),
	UnfoldInfo = UnfoldInfo0 ^ parents := Parents,
	pd_info_set_unfold_info(UnfoldInfo, !PDInfo).
pd_info_set_pred_proc_id(PredProcId, !PDInfo) :-
	pd_info_get_unfold_info(!.PDInfo, UnfoldInfo0),
	UnfoldInfo = UnfoldInfo0 ^ pred_proc_id := PredProcId,
	pd_info_set_unfold_info(UnfoldInfo, !PDInfo).
pd_info_set_changed(Changed, !PDInfo) :-
	pd_info_get_unfold_info(!.PDInfo, UnfoldInfo0),
	UnfoldInfo = UnfoldInfo0 ^ changed := Changed,
	pd_info_set_unfold_info(UnfoldInfo, !PDInfo).
pd_info_set_size_delta(SizeDelta, !PDInfo) :-
	pd_info_get_unfold_info(!.PDInfo, UnfoldInfo0),
	UnfoldInfo = UnfoldInfo0 ^ size_delta := SizeDelta,
	pd_info_set_unfold_info(UnfoldInfo, !PDInfo).
pd_info_set_rerun_det(Rerun, !PDInfo) :-
	pd_info_get_unfold_info(!.PDInfo, UnfoldInfo0),
	UnfoldInfo = UnfoldInfo0 ^ rerun_det := Rerun,
	pd_info_set_unfold_info(UnfoldInfo, !PDInfo).

pd_info_incr_cost_delta(Delta1, !PDInfo) :-
	pd_info_get_cost_delta(!.PDInfo, Delta0),
	Delta = Delta0 + Delta1,
	pd_info_set_cost_delta(Delta, !PDInfo).

pd_info_incr_size_delta(Delta1, !PDInfo) :-
	pd_info_get_size_delta(!.PDInfo, Delta0),
	Delta = Delta0 + Delta1,
	pd_info_set_size_delta(Delta, !PDInfo).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- interface.

	% Find the deforestation procedure which most closely
	% matches the given goal.
    %
:- pred pd_info__search_version(pd_info::in, hlds_goal::in, maybe_version::out,
	io::di, io::uo) is det.

	% Create a new predicate for the input goal, returning a
	% goal which calls the new predicate.
    %
:- pred pd_info__define_new_pred(pred_origin::in, hlds_goal::in,
	pred_proc_id::out, hlds_goal::out, pd_info::in, pd_info::out) is det.

	% Add a version to the table.
    %
:- pred pd_info__register_version(pred_proc_id::in, version_info::in,
	pd_info::in, pd_info::out, io::di, io::uo) is det.

	% Remove a version and make sure it is never recreated.
    %
:- pred pd_info__invalidate_version(pred_proc_id::in,
	pd_info::in, pd_info::out) is det.

	% Remove a version, but allow it to be recreated if it
	% is used elsewhere.
    %
:- pred pd_info__remove_version(pred_proc_id::in,
	pd_info::in, pd_info::out) is det.

	% The result of looking up a specialised version of a pred.
:- type maybe_version
	--->	no_version
	;	    version(
                mv_is_exact	        :: version_is_exact,
                mv_ppid		        :: pred_proc_id,
                mv_version	        :: version_info,
                mv_renaming	        :: map(prog_var, prog_var),
                                    % renaming of the version info
                mv_tsubst	        :: tsubst
                                    % var types substitution
            ).

:- type version_is_exact
	--->	exact
	;	    more_general.

:- type version_info
    --->    version_info(
                version_orig_goal	:: hlds_goal,
                                    % goal before unfolding.
                version_deforest_calls	:: list(pred_proc_id),
                                    % calls being deforested.
                version_arg_vars	:: list(prog_var),
                                    % arguments.
                version_arg_types	:: list(mer_type),
                                    % argument types.
                version_init_insts	:: instmap,
                                    % initial insts of the nonlocals.
                version_orig_cost	:: int,
                                    % cost of the original goal.
                version_cost_improv	:: int,
                                    % improvement in cost.
                version_parents		:: set(pred_proc_id),
                                    % parent versions.
                version_source		:: maybe(pred_proc_id)
                                    % the version which was generalised
                                    % to produce this version.
            ).

%-----------------------------------------------------------------------------%

:- implementation.

pd_info__search_version(PDInfo, Goal, MaybeVersion, !IO) :-
	pd_debug__output_goal(PDInfo, "Searching for version:\n", Goal, !IO),
	pd_util__goal_get_calls(Goal, CalledPreds),
	pd_info_get_versions(PDInfo, Versions),
	pd_info_get_goal_version_index(PDInfo, GoalVersionIndex),
	pd_info_get_module_info(PDInfo, ModuleInfo),
	pd_info_get_proc_info(PDInfo, ProcInfo),
	pd_info_get_instmap(PDInfo, InstMap),
	proc_info_vartypes(ProcInfo, VarTypes),
	(
		map__search(GoalVersionIndex, CalledPreds, VersionIds),
		pd_info__get_matching_version(ModuleInfo, Goal, InstMap,
			VarTypes, VersionIds, Versions, MaybeVersion0)
	->
		MaybeVersion = MaybeVersion0
	;
		MaybeVersion = no_version
	),
	pd_debug__search_version_result(PDInfo, MaybeVersion, !IO).

%-----------------------------------------------------------------------------%

:- pred pd_info__get_matching_version(module_info::in, hlds_goal::in,
	instmap::in, vartypes::in, list(pred_proc_id)::in,
	version_index::in, maybe_version::out) is semidet.

pd_info__get_matching_version(_, _, _, _, [], _, no_version).
pd_info__get_matching_version(ModuleInfo, ThisGoal, ThisInstMap, VarTypes,
		[VersionId | VersionIds], Versions, MaybeVersion) :-
	map__lookup(Versions, VersionId, Version),
	Version = version_info(OldGoal, _, OldArgs, OldArgTypes,
		OldInstMap, _, _, _, _),
	(
		pd_info__goal_is_more_general(ModuleInfo, OldGoal, OldInstMap, OldArgs,
            OldArgTypes, ThisGoal, ThisInstMap, VarTypes, VersionId, Version,
			MaybeVersion1)
	->
		(
			MaybeVersion1 = no_version,
			pd_info__get_matching_version(ModuleInfo, ThisGoal, ThisInstMap,
                VarTypes, VersionIds, Versions, MaybeVersion)
		;
			MaybeVersion1 = version(exact, _, _, _, _),
			MaybeVersion = MaybeVersion1
		;
			MaybeVersion1 = version(more_general, PredProcId,
				MoreGeneralVersion, Renaming, TypeSubn),
			pd_info__get_matching_version(ModuleInfo, ThisGoal,
				ThisInstMap, VarTypes, VersionIds,
				Versions, MaybeVersion2),
			pd_info__pick_version(ModuleInfo, PredProcId, Renaming,
				TypeSubn, MoreGeneralVersion, MaybeVersion2, MaybeVersion)
		)
	;
		pd_info__get_matching_version(ModuleInfo, ThisGoal, ThisInstMap,
            VarTypes, VersionIds, Versions, MaybeVersion)
	).

%-----------------------------------------------------------------------------%

	% Choose between two versions.
    %
:- pred pd_info__pick_version(module_info::in, pred_proc_id::in,
	map(prog_var, prog_var)::in, tsubst::in, version_info::in,
	maybe_version::in, maybe_version::out) is det.

pd_info__pick_version(_, PredProcId, Renaming, TSubn, VersionInfo, no_version,
	version(more_general, PredProcId, VersionInfo, Renaming, TSubn)).
pd_info__pick_version(_, _, _, _, _,
		version(exact, PredProcId, Version2, Renaming2, TSubn2),
		version(exact, PredProcId, Version2, Renaming2, TSubn2)).
pd_info__pick_version(_ModuleInfo, PredProcId1, Renaming1, TSubn1, Version1,
		version(more_general, PredProcId2, Version2, Renaming2, TSubn2),
		MaybeVersion) :-
	Version1 = version_info(_, _, _, _, _, _, CostDelta1, _, _),
	Version2 = version_info(_, _, _, _, _, _, CostDelta2, _, _),
	% Select the version with the biggest decrease in cost.
	( CostDelta1 > CostDelta2 ->
		MaybeVersion = version(more_general, PredProcId1,
			Version1, Renaming1, TSubn1)
	;
		MaybeVersion = version(more_general, PredProcId2,
			Version2, Renaming2, TSubn2)
	).

%-----------------------------------------------------------------------------%

	% The aim of this is to check whether the first goal can be used
	% instead of the second if specialisation on the second goal does
	% not produce any more improvement.
	%
	% An old version is more general than a new one if:
	% - the goals have the same "shape" (see pd_util__goals_match).
	% - each variable in the old goal maps to exactly one
	% 	variable in the new (multiple vars in the new goal can
	% 	map to one var in the old).
	% - each nonlocal in the new goal maps to a non-local in the
	% 	old (i.e. the old version produces all the variables
	% 	that the new one does).
	% - for each pair of corresponding insts in the above mapping,
	%	the old inst must be at least as general as the
	% 	new one, i.e inst_matches_initial(FirstInst, SecondInst) (?)
	%
:- pred pd_info__goal_is_more_general(module_info::in, hlds_goal::in,
	instmap::in, list(prog_var)::in, list(mer_type)::in, hlds_goal::in,
	instmap::in, vartypes::in, pred_proc_id::in,
	version_info::in, maybe_version::out) is semidet.

pd_info__goal_is_more_general(ModuleInfo, OldGoal, OldInstMap, OldArgs,
		OldArgTypes, NewGoal, NewInstMap, NewVarTypes, PredProcId,
		Version, MaybeVersion) :-
	pd_util__goals_match(ModuleInfo, OldGoal, OldArgs, OldArgTypes,
		NewGoal, NewVarTypes, OldNewRenaming, TypeRenaming),
	OldGoal = _ - OldGoalInfo,
	goal_info_get_nonlocals(OldGoalInfo, OldNonLocals0),
	set__to_sorted_list(OldNonLocals0, OldNonLocalsList),
	pd_info__check_insts(ModuleInfo, OldNonLocalsList, OldNewRenaming,
		OldInstMap, NewInstMap, NewVarTypes, exact, Exact),

	MaybeVersion = version(Exact, PredProcId, Version,
		OldNewRenaming, TypeRenaming).

%-----------------------------------------------------------------------------%

	% Check that all the insts in the old version are at least as
	% general as the insts in the new version.
    %
:- pred pd_info__check_insts(module_info::in, list(prog_var)::in,
	map(prog_var, prog_var)::in, instmap::in, instmap::in, vartypes::in,
	version_is_exact::in, version_is_exact::out) is semidet.

pd_info__check_insts(_, [], _, _, _, _, !ExactSoFar).
pd_info__check_insts(ModuleInfo, [OldVar | Vars], VarRenaming, OldInstMap,
		NewInstMap, VarTypes, !ExactSoFar) :-
	instmap__lookup_var(OldInstMap, OldVar, OldVarInst),
	map__lookup(VarRenaming, OldVar, NewVar),
	instmap__lookup_var(NewInstMap, NewVar, NewVarInst),
	map__lookup(VarTypes, NewVar, Type),
	inst_matches_initial(NewVarInst, OldVarInst, Type, ModuleInfo),
	( !.ExactSoFar = exact ->
		% Does inst_matches_initial(Inst1, Inst2, M) and
		% inst_matches_initial(Inst2, Inst1, M) imply that Inst1
		% and Inst2 are interchangable?
		(
			inst_matches_initial(OldVarInst, NewVarInst, Type,
				ModuleInfo)
		->
			!:ExactSoFar = exact
		;
			!:ExactSoFar = more_general
		)
	;
		!:ExactSoFar = more_general
	),
	pd_info__check_insts(ModuleInfo, Vars, VarRenaming, OldInstMap,
		NewInstMap, VarTypes, !ExactSoFar).

%-----------------------------------------------------------------------------%

pd_info__define_new_pred(Origin, Goal, PredProcId, CallGoal, !PDInfo) :-
	pd_info_get_instmap(!.PDInfo, InstMap),
	Goal = _ - GoalInfo,
	goal_info_get_nonlocals(GoalInfo, NonLocals),
	set__to_sorted_list(NonLocals, Args),
	pd_info_get_counter(!.PDInfo, Counter0),
	counter__allocate(Count, Counter0, Counter),
	pd_info_set_counter(Counter, !PDInfo),
	pd_info_get_pred_info(!.PDInfo, PredInfo),
	PredName = pred_info_name(PredInfo),
	goal_info_get_context(GoalInfo, Context),
	term__context_line(Context, Line),
	pd_info_get_module_info(!.PDInfo, ModuleInfo0),
	module_info_get_name(ModuleInfo0, ModuleName),
	make_pred_name_with_context(ModuleName, "DeforestationIn",
		predicate, PredName, Line, Count, SymName),
	unqualify_name(SymName, Name),

	pd_info_get_proc_info(!.PDInfo, ProcInfo),
	pred_info_typevarset(PredInfo, TVarSet),
	pred_info_get_markers(PredInfo, Markers),
	pred_info_get_class_context(PredInfo, ClassContext),
	pred_info_get_aditi_owner(PredInfo, Owner),
	proc_info_varset(ProcInfo, VarSet),
	proc_info_vartypes(ProcInfo, VarTypes),
	proc_info_rtti_varmaps(ProcInfo, RttiVarMaps),
	proc_info_inst_varset(ProcInfo, InstVarSet),
	% XXX handle the extra typeinfo arguments for
	% --typeinfo-liveness properly.
	hlds_pred__define_new_pred(Origin, Goal, CallGoal, Args, _ExtraArgs,
		InstMap, Name, TVarSet, VarTypes, ClassContext, RttiVarMaps,
		VarSet, InstVarSet, Markers, Owner, address_is_not_taken,
		ModuleInfo0, ModuleInfo, PredProcId),
	pd_info_set_module_info(ModuleInfo, !PDInfo).

%-----------------------------------------------------------------------------%

pd_info__register_version(PredProcId, Version, !PDInfo, !IO) :-
	pd_debug__register_version(!.PDInfo, PredProcId, Version, !IO),
	pd_info_get_goal_version_index(!.PDInfo, GoalVersionIndex0),
	Goal = Version ^ version_orig_goal,
	pd_util__goal_get_calls(Goal, Calls),
	( map__search(GoalVersionIndex0, Calls, VersionList0) ->
		map__det_update(GoalVersionIndex0, Calls,
			[PredProcId | VersionList0], GoalVersionIndex)
	;
		map__set(GoalVersionIndex0, Calls, [PredProcId],
			GoalVersionIndex)
	),
	pd_info_set_goal_version_index(GoalVersionIndex, !PDInfo),
	pd_info_get_versions(!.PDInfo, Versions0),
	map__det_insert(Versions0, PredProcId, Version, Versions),
	pd_info_set_versions(Versions, !PDInfo),
	pd_info_get_created_versions(!.PDInfo, CreatedVersions0),
	set__insert(CreatedVersions0, PredProcId, CreatedVersions),
	pd_info_set_created_versions(CreatedVersions, !PDInfo).

%-----------------------------------------------------------------------------%

pd_info__invalidate_version(PredProcId, !PDInfo) :-
	pd_info_get_versions(!.PDInfo, Versions0),
	map__lookup(Versions0, PredProcId, Version),
	Goal = Version ^ version_orig_goal,
	pd_util__goal_get_calls(Goal, Calls),
	(
		Calls = [FirstCall | _],
		list__last(Calls, LastCall)
	->
        % Make sure we never create another version to deforest
        % this pair of calls.
		pd_info_get_useless_versions(!.PDInfo, Useless0),
		set__insert(Useless0, FirstCall - LastCall, Useless),
		pd_info_set_useless_versions(Useless, !PDInfo)
	;
		true
	),
	pd_info__remove_version(PredProcId, !PDInfo).

pd_info__remove_version(PredProcId, !PDInfo) :-
	pd_info_get_versions(!.PDInfo, Versions0),
	map__lookup(Versions0, PredProcId, Version),
	Goal = Version ^ version_orig_goal,
	pd_util__goal_get_calls(Goal, Calls),
	map__delete(Versions0, PredProcId, Versions),
	pd_info_set_versions(Versions, !PDInfo),

	pd_info_get_goal_version_index(!.PDInfo, GoalIndex0),
	( map__search(GoalIndex0, Calls, GoalVersions0) ->
		list__delete_all(GoalVersions0, PredProcId, GoalVersions),
		map__det_update(GoalIndex0, Calls,
			GoalVersions, GoalIndex),
		pd_info_set_goal_version_index(GoalIndex, !PDInfo)
	;
		true
	),

	pd_info_get_created_versions(!.PDInfo, CreatedVersions0),
	set__delete(CreatedVersions0, PredProcId, CreatedVersions),
	pd_info_set_created_versions(CreatedVersions, !PDInfo),

	pd_info_get_module_info(!.PDInfo, ModuleInfo0),
	PredProcId = proc(PredId, _),
	module_info_remove_predicate(PredId, ModuleInfo0, ModuleInfo),
	pd_info_set_module_info(ModuleInfo, !PDInfo).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
