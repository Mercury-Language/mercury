%-----------------------------------------------------------------------------%
% Copyright (C) 1998 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% File: pd_info.m
% Main author: stayl
%
% Types for deforestation.
%-----------------------------------------------------------------------------%

:- module pd_info.

:- interface.

:- import_module pd_term, hlds_module, hlds_pred, options, instmap.
:- import_module hlds_goal, hlds_data, prog_data.
:- import_module bool, map, list, io, set, std_util, term, getopt.

:- type pd_info 
	---> pd_info(
		io__state,
		module_info,
		maybe(unfold_info),
		goal_version_index,
		version_index,
		pd_arg_info,
		int,			% version counter.
		global_term_info,
		set(pred_proc_id),
		int,			% current depth
		set(pred_proc_id),	% created versions
		set(pair(pred_proc_id)),% pairs of procedures which when
					% paired for deforestation produce
					% little improvement
		unit,
		unit
	).

		% map from list of called preds in the 
		% conjunctions to the specialised versions.
:- type goal_version_index == map(list(pred_proc_id), list(pred_proc_id)).

		% map from version id to the info about the version.
:- type version_index == map(pred_proc_id, version_info).

:- inst unique_pd_info = ground.

:- mode pd_info_di :: unique_pd_info -> dead.
:- mode pd_info_uo :: free -> unique_pd_info.
:- mode pd_info_ui :: unique_pd_info -> unique_pd_info.

:- inst pd_info_no_io = ground.
:- mode pd_info_set_io :: pd_info_no_io -> dead.

:- pred pd_info_init(module_info, pd_arg_info, io__state, pd_info).
:- mode pd_info_init(in, in, di, pd_info_uo) is det.

:- pred pd_info_init_unfold_info(pred_proc_id, 
		pred_info, proc_info, pd_info, pd_info).
:- mode pd_info_init_unfold_info(in, in, in, pd_info_di, pd_info_uo) is det.

:- pred pd_info_get_io_state(io__state, pd_info, pd_info).
:- mode pd_info_get_io_state(uo, pd_info_di, out(pd_info_no_io)) is det.

:- pred pd_info_get_module_info(module_info, pd_info, pd_info).
:- mode pd_info_get_module_info(out, pd_info_di, pd_info_uo) is det.

:- pred pd_info_get_unfold_info(unfold_info, pd_info, pd_info).
:- mode pd_info_get_unfold_info(out, pd_info_di, pd_info_uo) is det.

:- pred pd_info_get_goal_version_index(goal_version_index, pd_info, pd_info).
:- mode pd_info_get_goal_version_index(out, pd_info_di, pd_info_uo) is det.

:- pred pd_info_get_versions(version_index, pd_info, pd_info).
:- mode pd_info_get_versions(out, pd_info_di, pd_info_uo) is det.

:- pred pd_info_get_proc_arg_info(pd_arg_info, pd_info, pd_info).
:- mode pd_info_get_proc_arg_info(out, pd_info_di, pd_info_uo) is det.

:- pred pd_info_get_counter(int, pd_info, pd_info).
:- mode pd_info_get_counter(out, pd_info_di, pd_info_uo) is det.

:- pred pd_info_get_global_term_info(global_term_info, pd_info, pd_info).
:- mode pd_info_get_global_term_info(out, pd_info_di, pd_info_uo) is det.

:- pred pd_info_get_parent_versions(set(pred_proc_id), pd_info, pd_info).
:- mode pd_info_get_parent_versions(out, pd_info_di, pd_info_uo) is det.

:- pred pd_info_get_depth(int, pd_info, pd_info).
:- mode pd_info_get_depth(out, pd_info_di, pd_info_uo) is det.

:- pred pd_info_get_created_versions(set(pred_proc_id), pd_info, pd_info).
:- mode pd_info_get_created_versions(out, pd_info_di, pd_info_uo) is det.

:- pred pd_info_get_useless_versions(set(pair(pred_proc_id)), pd_info, pd_info).
:- mode pd_info_get_useless_versions(out, pd_info_di, pd_info_uo) is det.

:- pred pd_info_set_io_state(io__state, pd_info, pd_info).
:- mode pd_info_set_io_state(di, pd_info_set_io, pd_info_uo) is det.

:- pred pd_info_set_module_info(module_info, pd_info, pd_info).
:- mode pd_info_set_module_info(in, pd_info_di, pd_info_uo) is det.

:- pred pd_info_set_unfold_info(unfold_info, pd_info, pd_info).
:- mode pd_info_set_unfold_info(in, pd_info_di, pd_info_uo) is det.

:- pred pd_info_set_goal_version_index(goal_version_index, pd_info, pd_info).
:- mode pd_info_set_goal_version_index(in, pd_info_di, pd_info_uo) is det.

:- pred pd_info_set_versions(version_index, pd_info, pd_info).
:- mode pd_info_set_versions(in, pd_info_di, pd_info_uo) is det.

:- pred pd_info_set_proc_arg_info(pd_arg_info, pd_info, pd_info).
:- mode pd_info_set_proc_arg_info(in, pd_info_di, pd_info_uo) is det.

:- pred pd_info_set_counter(int, pd_info, pd_info).
:- mode pd_info_set_counter(in, pd_info_di, pd_info_uo) is det.

:- pred pd_info_set_global_term_info(global_term_info, pd_info, pd_info).
:- mode pd_info_set_global_term_info(in, pd_info_di, pd_info_uo) is det.

:- pred pd_info_set_parent_versions(set(pred_proc_id), pd_info, pd_info).
:- mode pd_info_set_parent_versions(in, pd_info_di, pd_info_uo) is det.

:- pred pd_info_set_depth(int, pd_info, pd_info).
:- mode pd_info_set_depth(in, pd_info_di, pd_info_uo) is det.

:- pred pd_info_set_created_versions(set(pred_proc_id), pd_info, pd_info).
:- mode pd_info_set_created_versions(in, pd_info_di, pd_info_uo) is det.

:- pred pd_info_set_useless_versions(set(pair(pred_proc_id)), pd_info, pd_info).
:- mode pd_info_set_useless_versions(in, pd_info_di, pd_info_uo) is det.

:- pred pd_info_update_goal(hlds_goal, pd_info, pd_info).
:- mode pd_info_update_goal(in, pd_info_di, pd_info_uo) is det.

:- pred pd_info_lookup_option(option, option_data, pd_info, pd_info).
:- mode pd_info_lookup_option(in, out, pd_info_di, pd_info_uo) is det.

:- pred pd_info_lookup_bool_option(option, bool, pd_info, pd_info).
:- mode pd_info_lookup_bool_option(in, out, pd_info_di, pd_info_uo) is det.

:- pred pd_info_apply_instmap_delta(instmap_delta, pd_info, pd_info).
:- mode pd_info_apply_instmap_delta(in, pd_info_di, pd_info_uo) is det.

:- pred pd_info_unset_unfold_info(pd_info, pd_info).
:- mode pd_info_unset_unfold_info(pd_info_di, pd_info_uo) is det.

	% With polymorphic modes these would be unnecessary.
:- pred pd_info_foldl(pred(T, pd_info, pd_info), list(T), pd_info, pd_info).
:- mode pd_info_foldl(pred(in, pd_info_di, pd_info_uo) is det, in,
		pd_info_di, pd_info_uo) is det.

:- pred pd_info_foldl2(pred(T, U, U, pd_info, pd_info), list(T), U, U, 
		pd_info, pd_info).
:- mode pd_info_foldl2(pred(in, in, out, pd_info_di, pd_info_uo) is det, in,
		in, out, pd_info_di, pd_info_uo) is det.

%-----------------------------------------------------------------------------%
:- implementation.

:- import_module hlds_pred, prog_data, pd_debug, pd_util, det_util, globals.
:- import_module inst_match, hlds_goal, prog_util, hlds_data.
:- import_module assoc_list, bool, int, require, string.

pd_info_init(ModuleInfo, ProcArgInfos, IO, PdInfo) :-
	map__init(GoalVersionIndex),
	map__init(Versions),
	set__init(ParentVersions),
	pd_term__global_term_info_init(GlobalInfo),
	set__init(CreatedVersions),
	set__init(UselessVersions),
	PdInfo = pd_info(IO, ModuleInfo, no, GoalVersionIndex, Versions, 
		ProcArgInfos, 0, GlobalInfo, ParentVersions, 0, 
		CreatedVersions, UselessVersions, unit, unit).

pd_info_init_unfold_info(PredProcId, PredInfo, ProcInfo) -->
	pd_info_get_module_info(ModuleInfo),
	{
	proc_info_get_initial_instmap(ProcInfo, ModuleInfo, InstMap),
	CostDelta = 0,
	pd_term__local_term_info_init(LocalTermInfo),
	set__singleton_set(Parents, PredProcId),
	UnfoldInfo = unfold_info(ProcInfo, InstMap, CostDelta, LocalTermInfo, 
			PredInfo, Parents, PredProcId, no, 0, no)
	},
	pd_info_set_unfold_info(UnfoldInfo).

pd_info_get_io_state(IO, PdInfo, PdInfo) :-
	PdInfo = pd_info(IO0, _,_,_,_,_,_,_,_,_,_,_,_,_),
	unsafe_promise_unique(IO0, IO).
pd_info_get_module_info(ModuleInfo, PdInfo, PdInfo) :-
	PdInfo = pd_info(_, ModuleInfo, _,__,_,_,_,_,_,_,_,_,_,_).
pd_info_get_unfold_info(UnfoldInfo, PdInfo, PdInfo) :-
	PdInfo = pd_info(_,_, MaybeUnfoldInfo, _,_,_,_,_,_,_,_,_,_,_),
	(
		MaybeUnfoldInfo = yes(UnfoldInfo)
	;
		MaybeUnfoldInfo = no,
		error("pd_info_get_unfold_info: unfold_info not set.")
	).
pd_info_get_goal_version_index(Index, PdInfo, PdInfo) :-
	PdInfo = pd_info(_,_,_,Index,_,_,_,_,_,_,_,_,_,_).
pd_info_get_versions(Versions, PdInfo, PdInfo) :-
	PdInfo = pd_info(_,_,_,_,Versions,_,_,_,_,_,_,_,_,_).
pd_info_get_proc_arg_info(ProcArgInfo, PdInfo, PdInfo) :-
	PdInfo = pd_info(_,_,_,_,_,ProcArgInfo,_,_,_,_,_,_,_,_).
pd_info_get_counter(Counter, PdInfo, PdInfo) :-
	PdInfo = pd_info(_,_,_,_,_,_,Counter,_,_,_,_,_,_,_).
pd_info_get_global_term_info(TermInfo, PdInfo, PdInfo) :-
	PdInfo = pd_info(_,_,_,_,_,_,_,TermInfo,_,_,_,_,_,_).
pd_info_get_parent_versions(Parents, PdInfo, PdInfo) :-
	PdInfo = pd_info(_,_,_,_,_,_,_,_,Parents,_,_,_,_,_).
pd_info_get_depth(Depth, PdInfo, PdInfo) :-
	PdInfo = pd_info(_,_,_,_,_,_,_,_,_,Depth,_,_,_,_).
pd_info_get_created_versions(Versions, PdInfo, PdInfo) :-
	PdInfo = pd_info(_,_,_,_,_,_,_,_,_,_,Versions,_,_,_).
pd_info_get_useless_versions(Versions, PdInfo, PdInfo) :-
	PdInfo = pd_info(_,_,_,_,_,_,_,_,_,_,_,Versions,_,_).
pd_info_set_io_state(IO0, pd_info(_, B,C,D,E,F,G,H,I,J,K,L,M,N), 
		pd_info(IO, B,C,D,E,F,G,H,I,J,K,L,M,N)) :-
	unsafe_promise_unique(IO0, IO).
pd_info_set_module_info(ModuleInfo, pd_info(A,_,C,D,E,F,G,H,I,J,K,L,M,N),
		pd_info(A, ModuleInfo, C,D,E,F,G,H,I,J,K,L,M,N)).
pd_info_set_unfold_info(UnfoldInfo, pd_info(A,B,_,D,E,F,G,H,I,J,K,L,M,N),
		pd_info(A,B, yes(UnfoldInfo), D,E,F,G,H,I,J,K,L,M,N)).
pd_info_unset_unfold_info(pd_info(A,B,_,D,E,F,G,H,I,J,K,L,M,N),
		pd_info(A,B, no, D,E,F,G,H,I,J,K,L,M,N)).
pd_info_set_goal_version_index(Index, pd_info(A,B,C,_,E,F,G,H,I,J,K,L,M,N),
		pd_info(A,B,C,Index,E,F,G,H,I,J,K,L,M,N)).
pd_info_set_versions(Versions, pd_info(A,B,C,D,_,F,G,H,I,J,K,L,M,N),
		pd_info(A,B,C,D,Versions,F,G,H,I,J,K,L,M,N)).
pd_info_set_proc_arg_info(ProcArgInfo, pd_info(A,B,C,D,E,_,G,H,I,J,K,L,M,N),
		pd_info(A,B,C,D,E,ProcArgInfo,G,H,I,J,K,L,M,N)).
pd_info_set_counter(Counter, pd_info(A,B,C,D,E,F,_,H,I,J,K,L,M,N),
		pd_info(A,B,C,D,E,F,Counter,H,I,J,K,L,M,N)).
pd_info_set_global_term_info(TermInfo, pd_info(A,B,C,D,E,F,G,_,I,J,K,L,M,N),
		pd_info(A,B,C,D,E,F,G,TermInfo,I,J,K,L,M,N)).
pd_info_set_parent_versions(Parents, pd_info(A,B,C,D,E,F,G,H,_,J,K,L,M,N),
		pd_info(A,B,C,D,E,F,G,H,Parents,J,K,L,M,N)).
pd_info_set_depth(Depth, pd_info(A,B,C,D,E,F,G,H,I,_,K,L,M,N),
		pd_info(A,B,C,D,E,F,G,H,I,Depth,K,L,M,N)).
pd_info_set_created_versions(Versions, pd_info(A,B,C,D,E,F,G,H,I,J,_,L,M,N),
		pd_info(A,B,C,D,E,F,G,H,I,J,Versions,L,M,N)).
pd_info_set_useless_versions(Versions, pd_info(A,B,C,D,E,F,G,H,I,J,K,_,M,N),
		pd_info(A,B,C,D,E,F,G,H,I,J,K,Versions,M,N)).

pd_info_update_goal(_ - GoalInfo) -->
	{ goal_info_get_instmap_delta(GoalInfo, Delta) },
	pd_info_apply_instmap_delta(Delta).

pd_info_lookup_option(Option, OptionData) -->
	pd_info_get_io_state(IO0),
	{ globals__io_lookup_option(Option, OptionData, IO0, IO) },
	pd_info_set_io_state(IO).

pd_info_lookup_bool_option(Option, Value) -->
	pd_info_lookup_option(Option, Value0),
	{ Value0 = bool(Value1) ->
		Value = Value1
	;
		error("pd_info_lookup_bool_option")
	}.

pd_info_apply_instmap_delta(InstMapDelta) -->
	pd_info_get_instmap(InstMap0),
	{ instmap__apply_instmap_delta(InstMap0, InstMapDelta, InstMap) },
	pd_info_set_instmap(InstMap).

pd_info_foldl(_, []) --> [].
pd_info_foldl(Pred, [H | T]) -->
	call(Pred, H),
	pd_info_foldl(Pred, T).

pd_info_foldl2(_, [], Acc, Acc) --> [].
pd_info_foldl2(Pred, [H | T], Acc0, Acc) -->
	call(Pred, H, Acc0, Acc1),
	pd_info_foldl2(Pred, T, Acc1, Acc).

%-----------------------------------------------------------------------------%

:- interface.

	% unfold_info contains information used while searching a procedure
	% body for unfolding and deforestation opportunities.
:- type unfold_info
	--->	unfold_info(
			proc_info,
			instmap,
			int,		% improvement in cost measured while
					% processing this procedure
			local_term_info,% information used to prevent
					% infinite unfolding within the 
					% current procedure.
			pred_info,
			set(pred_proc_id),
			pred_proc_id,	% current pred_proc_id
			bool,		% has anything changed
			int,		% increase in size measured while
					% processing this procedure
			bool		% does determinism analysis
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

:- pred pd_info_get_proc_info(proc_info, pd_info, pd_info).
:- mode pd_info_get_proc_info(out, pd_info_di, pd_info_uo) is det.

:- pred pd_info_get_instmap(instmap, pd_info, pd_info).
:- mode pd_info_get_instmap(out, pd_info_di, pd_info_uo) is det.

:- pred pd_info_get_cost_delta(int, pd_info, pd_info).
:- mode pd_info_get_cost_delta(out, pd_info_di, pd_info_uo) is det.

:- pred pd_info_get_local_term_info(local_term_info, pd_info, pd_info).
:- mode pd_info_get_local_term_info(out, pd_info_di, pd_info_uo) is det.

:- pred pd_info_get_pred_info(pred_info, pd_info, pd_info).
:- mode pd_info_get_pred_info(out, pd_info_di, pd_info_uo) is det.

:- pred pd_info_get_parents(set(pred_proc_id), pd_info, pd_info).
:- mode pd_info_get_parents(out, pd_info_di, pd_info_uo) is det.

:- pred pd_info_get_pred_proc_id(pred_proc_id, pd_info, pd_info).
:- mode pd_info_get_pred_proc_id(out, pd_info_di, pd_info_uo) is det.

:- pred pd_info_get_changed(bool, pd_info, pd_info).
:- mode pd_info_get_changed(out, pd_info_di, pd_info_uo) is det.

:- pred pd_info_get_size_delta(int, pd_info, pd_info).
:- mode pd_info_get_size_delta(out, pd_info_di, pd_info_uo) is det.

:- pred pd_info_get_rerun_det(bool, pd_info, pd_info).
:- mode pd_info_get_rerun_det(out, pd_info_di, pd_info_uo) is det.

:- pred pd_info_set_proc_info(proc_info, pd_info, pd_info).
:- mode pd_info_set_proc_info(in, pd_info_di, pd_info_uo) is det.

:- pred pd_info_set_instmap(instmap, pd_info, pd_info).
:- mode pd_info_set_instmap(in, pd_info_di, pd_info_uo) is det.

:- pred pd_info_set_cost_delta(int, pd_info, pd_info).
:- mode pd_info_set_cost_delta(in, pd_info_di, pd_info_uo) is det.

:- pred pd_info_set_local_term_info(local_term_info, pd_info, pd_info).
:- mode pd_info_set_local_term_info(in, pd_info_di, pd_info_uo) is det.

:- pred pd_info_set_pred_info(pred_info, pd_info, pd_info).
:- mode pd_info_set_pred_info(in, pd_info_di, pd_info_uo) is det.

:- pred pd_info_set_parents(set(pred_proc_id), pd_info, pd_info).
:- mode pd_info_set_parents(in, pd_info_di, pd_info_uo) is det.

:- pred pd_info_set_pred_proc_id(pred_proc_id, pd_info, pd_info).
:- mode pd_info_set_pred_proc_id(in, pd_info_di, pd_info_uo) is det.

:- pred pd_info_set_changed(bool, pd_info, pd_info).
:- mode pd_info_set_changed(in, pd_info_di, pd_info_uo) is det.

:- pred pd_info_set_size_delta(int, pd_info, pd_info).
:- mode pd_info_set_size_delta(in, pd_info_di, pd_info_uo) is det.

:- pred pd_info_set_rerun_det(bool, pd_info, pd_info).
:- mode pd_info_set_rerun_det(in, pd_info_di, pd_info_uo) is det.

:- pred pd_info_incr_cost_delta(int, pd_info, pd_info).
:- mode pd_info_incr_cost_delta(in, pd_info_di, pd_info_uo) is det.

:- pred pd_info_incr_size_delta(int, pd_info, pd_info).
:- mode pd_info_incr_size_delta(in, pd_info_di, pd_info_uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.

pd_info_get_proc_info(ProcInfo) -->
	pd_info_get_unfold_info(UnfoldInfo),
	{ UnfoldInfo = unfold_info(ProcInfo, _,_,_,_,_,_,_,_,_) }.
pd_info_get_instmap(InstMap) -->
	pd_info_get_unfold_info(UnfoldInfo),
	{ UnfoldInfo = unfold_info(_, InstMap, _,_,_,_,_,_,_,_) }.
pd_info_get_cost_delta(CostDelta) -->
	pd_info_get_unfold_info(UnfoldInfo),
	{ UnfoldInfo = unfold_info(_,_, CostDelta, _,_,_,_,_,_,_) }.
pd_info_get_local_term_info(TermInfo) -->
	pd_info_get_unfold_info(UnfoldInfo),
	{ UnfoldInfo = unfold_info(_,_,_,TermInfo,_,_,_,_,_,_) }.
pd_info_get_pred_info(PredInfo) -->
	pd_info_get_unfold_info(UnfoldInfo),
	{ UnfoldInfo = unfold_info(_,_,_,_,PredInfo,_,_,_,_,_) }.
pd_info_get_parents(Parents) -->
	pd_info_get_unfold_info(UnfoldInfo),
	{ UnfoldInfo = unfold_info(_,_,_,_,_,Parents,_,_,_,_) }.
pd_info_get_pred_proc_id(PredProcId) -->
	pd_info_get_unfold_info(UnfoldInfo),
	{ UnfoldInfo = unfold_info(_,_,_,_,_,_,PredProcId,_,_,_) }.
pd_info_get_changed(Changed) -->
	pd_info_get_unfold_info(UnfoldInfo),
	{ UnfoldInfo = unfold_info(_,_,_,_,_,_,_,Changed,_,_) }.
pd_info_get_size_delta(SizeDelta) -->
	pd_info_get_unfold_info(UnfoldInfo),
	{ UnfoldInfo = unfold_info(_,_,_,_,_,_,_,_,SizeDelta,_) }.
pd_info_get_rerun_det(Rerun) -->
	pd_info_get_unfold_info(UnfoldInfo),
	{ UnfoldInfo = unfold_info(_,_,_,_,_,_,_,_,_,Rerun) }.
	
pd_info_set_proc_info(ProcInfo) -->
	pd_info_get_unfold_info(UnfoldInfo0),
	{ UnfoldInfo0 = unfold_info(_,B,C,D,E,F,G,H,I,J) },
	{ UnfoldInfo = unfold_info(ProcInfo, B,C,D,E,F,G,H,I,J) },
	pd_info_set_unfold_info(UnfoldInfo).
pd_info_set_instmap(InstMap) -->
	pd_info_get_unfold_info(UnfoldInfo0),
	{ UnfoldInfo0 = unfold_info(A,_,C,D,E,F,G,H,I,J) },
	{ UnfoldInfo = unfold_info(A, InstMap,C,D,E,F,G,H,I,J) },
	pd_info_set_unfold_info(UnfoldInfo).
pd_info_set_cost_delta(CostDelta) -->
	pd_info_get_unfold_info(UnfoldInfo0),
	{ UnfoldInfo0 = unfold_info(A,B,_,D,E,F,G,H,I,J) },
	{ UnfoldInfo = unfold_info(A,B,CostDelta,D,E,F,G,H,I,J) },
	pd_info_set_unfold_info(UnfoldInfo).
pd_info_set_local_term_info(TermInfo) -->
	pd_info_get_unfold_info(UnfoldInfo0),
	{ UnfoldInfo0 = unfold_info(A,B,C,_,E,F,G,H,I,J) },
	{ UnfoldInfo = unfold_info(A,B,C,TermInfo,E,F,G,H,I,J) },
	pd_info_set_unfold_info(UnfoldInfo).
pd_info_set_pred_info(PredInfo) -->
	pd_info_get_unfold_info(UnfoldInfo0),
	{ UnfoldInfo0 = unfold_info(A,B,C,D,_,F,G,H,I,J) },
	{ UnfoldInfo = unfold_info(A,B,C,D,PredInfo,F,G,H,I,J) },
	pd_info_set_unfold_info(UnfoldInfo).
pd_info_set_parents(Parents) -->
	pd_info_get_unfold_info(UnfoldInfo0),
	{ UnfoldInfo0 = unfold_info(A,B,C,D,E,_,G,H,I,J) },
	{ UnfoldInfo = unfold_info(A,B,C,D,E,Parents,G,H,I,J) },
	pd_info_set_unfold_info(UnfoldInfo).
pd_info_set_pred_proc_id(PredProcId) -->
	pd_info_get_unfold_info(UnfoldInfo0),
	{ UnfoldInfo0 = unfold_info(A,B,C,D,E,F,_,H,I,J) },
	{ UnfoldInfo = unfold_info(A,B,C,D,E,F,PredProcId,H,I,J) },
	pd_info_set_unfold_info(UnfoldInfo).
pd_info_set_changed(Changed) -->
	pd_info_get_unfold_info(UnfoldInfo0),
	{ UnfoldInfo0 = unfold_info(A,B,C,D,E,F,G,_,I,J) },
	{ UnfoldInfo = unfold_info(A,B,C,D,E,F,G, Changed, I,J) },
	pd_info_set_unfold_info(UnfoldInfo).
pd_info_set_size_delta(SizeDelta) -->
	pd_info_get_unfold_info(UnfoldInfo0),
	{ UnfoldInfo0 = unfold_info(A,B,C,D,E,F,G,H,_,J) },
	{ UnfoldInfo = unfold_info(A,B,C,D,E,F,G,H, SizeDelta, J) },
	pd_info_set_unfold_info(UnfoldInfo).
pd_info_set_rerun_det(Rerun) -->
	pd_info_get_unfold_info(UnfoldInfo0),
	{ UnfoldInfo0 = unfold_info(A,B,C,D,E,F,G,H,I,_) },
	{ UnfoldInfo = unfold_info(A,B,C,D,E,F,G,H,I, Rerun) },
	pd_info_set_unfold_info(UnfoldInfo).

pd_info_incr_cost_delta(Delta1) -->
	pd_info_get_cost_delta(Delta0),
	{ Delta is Delta0 + Delta1 },
	pd_info_set_cost_delta(Delta).

pd_info_incr_size_delta(Delta1) -->
	pd_info_get_size_delta(Delta0),
	{ Delta is Delta0 + Delta1 },
	pd_info_set_size_delta(Delta).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- interface.

	% Find the deforestation procedure which most closely
	% matches the given goal.
:- pred pd_info__search_version(hlds_goal::in, maybe_version::out,
	pd_info::pd_info_di, pd_info::pd_info_uo) is det.

	% Create a new predicate for the input goal, returning a
	% goal which calls the new predicate.
:- pred pd_info__define_new_pred(hlds_goal::in, pred_proc_id::out,
	hlds_goal::out, pd_info::pd_info_di, pd_info::pd_info_uo) is det.

	% Add a version to the table.
:- pred pd_info__register_version(pred_proc_id::in, version_info::in,
	pd_info::pd_info_di, pd_info::pd_info_uo) is det.

	% Remove a version and make sure it is never recreated.
:- pred pd_info__invalidate_version(pred_proc_id::in,
	pd_info::pd_info_di, pd_info::pd_info_uo) is det.

	% Remove a version, but allow it to be recreated if it 
	% is used elsewhere.
:- pred pd_info__remove_version(pred_proc_id::in,
	pd_info::pd_info_di, pd_info::pd_info_uo) is det.

	% The result of looking up a specialised version of a pred.
:- type maybe_version
	--->	no_version
	;	version(
			version_is_exact, 
			pred_proc_id, 
			version_info,
			map(var, var),	% renaming of the version info
			tsubst		% var types substitution
		).

:- type version_is_exact
	--->	exact
	;	more_general.

:- type version_info
	---> version_info(
		hlds_goal,		% goal before unfolding.
		list(pred_proc_id),	% calls being deforested. 
		list(var),		% arguments.
		list(type),		% argument types.
		instmap,		% initial insts of the nonlocals.
		int,			% cost of the original goal.
		int,			% improvement in cost.
		set(pred_proc_id), 	% parent versions.
		maybe(pred_proc_id)	% the version which was generalised
					% to produce this version.
	).

%-----------------------------------------------------------------------------%

:- implementation.

pd_info__search_version(Goal, MaybeVersion) -->
	pd_debug__output_goal("Searching for version:\n", Goal),
	{ pd_util__goal_get_calls(Goal, CalledPreds) },
	pd_info_get_versions(Versions),
	pd_info_get_goal_version_index(GoalVersionIndex),
	pd_info_get_module_info(ModuleInfo),
	pd_info_get_proc_info(ProcInfo),
	pd_info_get_instmap(InstMap),
	{ proc_info_inst_table(ProcInfo, InstTable) },
	{ proc_info_vartypes(ProcInfo, VarTypes) },
	(
		{ map__search(GoalVersionIndex, CalledPreds, VersionIds) },
		{ pd_info__get_matching_version(InstTable, ModuleInfo, Goal,
			InstMap, VarTypes, VersionIds, Versions,
			MaybeVersion0) }
	->
		{ MaybeVersion = MaybeVersion0 }
	;
		{ MaybeVersion = no_version }
	),
	pd_debug__search_version_result(MaybeVersion).

%-----------------------------------------------------------------------------%

:- pred pd_info__get_matching_version(inst_table::in, module_info::in,
		hlds_goal::in, instmap::in, map(var, type)::in,
		list(pred_proc_id)::in, version_index::in, maybe_version::out)
		is semidet.

pd_info__get_matching_version(_, _, _, _, _, [], _, no_version).
pd_info__get_matching_version(InstTable, ModuleInfo, ThisGoal, ThisInstMap,
		VarTypes, [VersionId | VersionIds], Versions, MaybeVersion) :-
	map__lookup(Versions, VersionId, Version),
	Version = version_info(OldGoal, _, OldArgs, OldArgTypes,
			OldInstMap, _, _, _, _),
	(
		pd_info__goal_is_more_general(InstTable, ModuleInfo,
			OldGoal, OldInstMap, OldArgs, OldArgTypes, 
			ThisGoal, ThisInstMap, VarTypes, VersionId, Version, 
			MaybeVersion1)
	->
		(
			MaybeVersion1 = no_version,
			pd_info__get_matching_version(InstTable, ModuleInfo,
				ThisGoal, ThisInstMap, VarTypes, VersionIds,
				Versions, MaybeVersion)
		;
			MaybeVersion1 = version(exact, _, _, _, _),
			MaybeVersion = MaybeVersion1
		;
			MaybeVersion1 =
				version(more_general, PredProcId,
					MoreGeneralVersion, Renaming, TypeSubn),
			pd_info__get_matching_version(InstTable, ModuleInfo,
				ThisGoal, ThisInstMap, VarTypes, VersionIds, 
				Versions, MaybeVersion2),
			pd_info__pick_version(ModuleInfo, PredProcId, Renaming,
				TypeSubn, MoreGeneralVersion, MaybeVersion2,
				MaybeVersion)
		)
	;
		pd_info__get_matching_version(InstTable, ModuleInfo, ThisGoal,
			ThisInstMap, VarTypes, VersionIds,
			Versions, MaybeVersion)
	).

%-----------------------------------------------------------------------------%

	% Choose between two versions.
:- pred pd_info__pick_version(module_info::in, pred_proc_id::in, 
		map(var, var)::in, tsubst::in, version_info::in, 
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
:- pred pd_info__goal_is_more_general(inst_table::in, module_info::in,
	hlds_goal::in, instmap::in, list(var)::in, list(type)::in,
	hlds_goal::in, instmap::in, map(var, type)::in, pred_proc_id::in, 
	version_info::in, maybe_version::out) is semidet.

pd_info__goal_is_more_general(InstTable, ModuleInfo, OldGoal, OldInstMap,
		OldArgs, OldArgTypes, NewGoal, NewInstMap, NewVarTypes,
		PredProcId, Version, MaybeVersion) :-
	pd_util__goals_match(ModuleInfo, OldGoal, OldArgs, OldArgTypes, 
		NewGoal, NewVarTypes, OldNewRenaming, TypeRenaming), 
	OldGoal = _ - OldGoalInfo,
	goal_info_get_nonlocals(OldGoalInfo, OldNonLocals0),
	set__to_sorted_list(OldNonLocals0, OldNonLocalsList),
	pd_info__check_insts(InstTable, ModuleInfo, OldNonLocalsList,
		OldNewRenaming, OldInstMap, NewInstMap, exact, Exact),
		
	MaybeVersion = version(Exact, PredProcId, Version, 
		OldNewRenaming, TypeRenaming).

%-----------------------------------------------------------------------------%

	% Check that all the insts in the old version are at least as
	% general as the insts in the new version.
:- pred pd_info__check_insts(inst_table::in, module_info::in, list(var)::in,
		map(var, var)::in, instmap::in, instmap::in,
		version_is_exact::in, version_is_exact::out) is semidet.

pd_info__check_insts(_, _, [], _, _, _, Exact, Exact).
pd_info__check_insts(InstTable, ModuleInfo, [OldVar | Vars], VarRenaming,
		OldInstMap, NewInstMap, ExactSoFar0, ExactSoFar) :-
	instmap__lookup_var(OldInstMap, OldVar, OldVarInst),
	map__lookup(VarRenaming, OldVar, NewVar),
	instmap__lookup_var(NewInstMap, NewVar, NewVarInst),
	inst_matches_initial(NewVarInst, NewInstMap, OldVarInst, OldInstMap,
			InstTable, ModuleInfo),
	( ExactSoFar0 = exact ->
		% Does inst_matches_initial(Inst1, Inst2, M) and
		% inst_matches_initial(Inst2, Inst1, M) imply that Inst1
		% and Inst2 are interchangable? 
		( 
			inst_matches_initial(OldVarInst, OldInstMap,
				NewVarInst, NewInstMap, InstTable,
				ModuleInfo)
		->
			ExactSoFar1 = exact
		;
			ExactSoFar1 = more_general
		)
	;
		ExactSoFar1 = more_general
	),
	pd_info__check_insts(InstTable, ModuleInfo, Vars, VarRenaming,
		OldInstMap, NewInstMap, ExactSoFar1, ExactSoFar).

%-----------------------------------------------------------------------------%

pd_info__define_new_pred(Goal, PredProcId, CallGoal) -->
	pd_info_get_instmap(InstMap),
	{ Goal = _ - GoalInfo },
	{ goal_info_get_nonlocals(GoalInfo, NonLocals) },
	{ set__to_sorted_list(NonLocals, Args) },
	pd_info_get_counter(Counter0),
	{ Counter is Counter0 + 1 },
	pd_info_set_counter(Counter),
	pd_info_get_pred_info(PredInfo),
	{ pred_info_name(PredInfo, PredName) },
	{ goal_info_get_context(GoalInfo, Context) },
	{ term__context_line(Context, Line) },
	pd_info_get_module_info(ModuleInfo0),
	{ module_info_name(ModuleInfo0, ModuleName) },	
	{ make_pred_name_with_context(ModuleName, "DeforestationIn",
		predicate, PredName, Line, Counter0, SymName) },
	{ unqualify_name(SymName, Name) },

	pd_info_get_proc_info(ProcInfo),
	{ pred_info_typevarset(PredInfo, TVarSet) },
	{ pred_info_get_markers(PredInfo, Markers) },
	{ pred_info_get_class_context(PredInfo, ClassContext) },
	{ proc_info_varset(ProcInfo, VarSet) },
	{ proc_info_vartypes(ProcInfo, VarTypes) },
	{ proc_info_typeinfo_varmap(ProcInfo, TVarMap) },
	{ proc_info_typeclass_info_varmap(ProcInfo, TCVarMap) },
	{ proc_info_inst_table(ProcInfo, InstTable) },
	% XXX handle the extra typeinfo arguments for
	% --typeinfo-liveness properly.
	{ hlds_pred__define_new_pred(Goal, CallGoal, Args, _ExtraArgs, InstMap, 
		Name, TVarSet, VarTypes, ClassContext, TVarMap, TCVarMap,
		VarSet, Markers, InstTable, ModuleInfo0, ModuleInfo,
		PredProcId) },
	pd_info_set_module_info(ModuleInfo).

%-----------------------------------------------------------------------------%

pd_info__register_version(PredProcId, Version) -->
	pd_debug__register_version(PredProcId, Version),
	pd_info_get_goal_version_index(GoalVersionIndex0),
	{ Version = version_info(Goal, _, _, _, _, _, _, _, _) },
	{ pd_util__goal_get_calls(Goal, Calls) },
	{ map__search(GoalVersionIndex0, Calls, VersionList0) ->
		map__det_update(GoalVersionIndex0, Calls,
			[PredProcId | VersionList0], GoalVersionIndex)
	;
		map__set(GoalVersionIndex0, Calls, [PredProcId], 
			GoalVersionIndex)
	},
	pd_info_set_goal_version_index(GoalVersionIndex),
	pd_info_get_versions(Versions0),
	{ map__det_insert(Versions0, PredProcId, Version, Versions) },
	pd_info_set_versions(Versions),
	pd_info_get_created_versions(CreatedVersions0),
	{ set__insert(CreatedVersions0, PredProcId, CreatedVersions) },
	pd_info_set_created_versions(CreatedVersions).

%-----------------------------------------------------------------------------%

pd_info__invalidate_version(PredProcId) -->
	pd_info_get_versions(Versions0),
	{ map__lookup(Versions0, PredProcId, Version) },
	{ Version = version_info(Goal, _, _, _, _, _, _, _, _) },
	{ pd_util__goal_get_calls(Goal, Calls) },
	( { Calls = [FirstCall | _], list__last(Calls, LastCall) } ->
			% Make sure we never create another version to
			% deforest this pair of calls.
		pd_info_get_useless_versions(Useless0),
		{ set__insert(Useless0, FirstCall - LastCall, Useless) },
		pd_info_set_useless_versions(Useless)
	;
		[]
	),
	pd_info__remove_version(PredProcId).

pd_info__remove_version(PredProcId) -->
	pd_info_get_versions(Versions0),
	{ map__lookup(Versions0, PredProcId, Version) },
	{ Version = version_info(Goal, _, _, _, _, _, _, _, _) },
	{ pd_util__goal_get_calls(Goal, Calls) },
	{ map__delete(Versions0, PredProcId, Versions) },
	pd_info_set_versions(Versions),

	pd_info_get_goal_version_index(GoalIndex0),
	( { map__search(GoalIndex0, Calls, GoalVersions0) } ->
		{ list__delete_all(GoalVersions0, PredProcId, GoalVersions) },
		{ map__det_update(GoalIndex0, Calls, 
			GoalVersions, GoalIndex) },
		pd_info_set_goal_version_index(GoalIndex)
	;
		[]
	),

	pd_info_get_created_versions(CreatedVersions0),
	{ set__delete(CreatedVersions0, PredProcId, CreatedVersions) },
	pd_info_set_created_versions(CreatedVersions),

	pd_info_get_module_info(ModuleInfo0),
	{ PredProcId = proc(PredId, _) },
	{ module_info_remove_predicate(PredId, ModuleInfo0, ModuleInfo) },
	pd_info_set_module_info(ModuleInfo).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
