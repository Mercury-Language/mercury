%-----------------------------------------------------------------------------%
% Copyright (C) 1998-2001 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% File: pd_term.m
% Main author: stayl
%
% Termination checking for the deforestation process. 
% There are two places where care must be taken to ensure
% termination of the process:
% - when unfolding a call to a recursive procedure
% - when creating a new version, to avoid creating an infinite sequence of 
% 	new versions for which folding never occurs.
%
% For conjunctions, count up the length of the conjunction.
% For each pair of calls on the end of the conjunction,
% this length must decrease for the check to succeed. 
%
% For single calls, the first call records the sizes of the insts
% of all the arguments. If the total size of a later call increases,
% the increasing arguments are removed from the record. If there are 
% no decreasing arguments, the termination check fails. Otherwise
% the check succeeds and the new argument sizes are recorded.
%
% There are many possible improvements to this:
% - Partition on subterms of arguments rather than whole arguments - useful
% 	when partially instantiated structures are present.
% - Use homeomorphic embedding instead of term sizes as suggested in
% 	the papers on partial deduction from K.U. Leuven. This will be 
% 	useful (necessary?) if we start propagating equality constraints.
%
%-----------------------------------------------------------------------------%
:- module pd_term.

:- interface.

:- import_module hlds_goal, hlds_module, hlds_pred, instmap, pd_info.
:- import_module list, std_util.

	% pd_term__global_check(Module, CallGoal1, BetweenGoals, CallGoal2,
	% 	InstMap, Versions, Info0, Info, Result)
	%
	% Check whether a new version can be created for the conjunction
	% (CallGoal1, BetweenGoals, CallGoal2) without the deforestation
	% process looping.
:- pred pd_term__global_check(module_info::in, hlds_goal::in,
		list(hlds_goal)::in, maybe(hlds_goal)::in,
		instmap::in, version_index::in, 
		global_term_info::in, global_term_info::out, 
		global_check_result::out) is det.

	% A proc_pair holds the pred_proc_ids of the procedures called at
	% the ends of a conjunction to be deforested.
	% The maybe(pred_proc_id) is `no' in the case of a predicate
	% created for constraint propagation.
:- type proc_pair == pair(pred_proc_id, maybe(pred_proc_id)).

:- type global_check_result
	--->	ok(proc_pair, int)
	;	possible_loop(proc_pair, int, pred_proc_id)
	;	loop.

	% Check whether a call can be unfolded without the
	% unfolding process looping.
:- pred pd_term__local_check(module_info::in, hlds_goal::in,
	instmap::in, local_term_info::in, local_term_info::out) is semidet.

:- pred pd_term__global_term_info_init(global_term_info::out) is det.

:- pred pd_term__local_term_info_init(local_term_info::out) is det.

:- pred pd_term__get_proc_term_info(local_term_info::in, pred_proc_id::in,
		pd_proc_term_info::out) is semidet.

	% Update the global termination information when we find
	% out the pred_proc_id that has been assigned to a version.
:- pred pd_term__update_global_term_info(global_term_info::in,
		proc_pair::in, pred_proc_id::in,
		int::in, global_term_info::out) is det.

:- type global_term_info. 
:- type local_term_info.
:- type pd_proc_term_info.

%-----------------------------------------------------------------------------%
:- implementation.

:- import_module hlds_pred, (inst), mode_util, prog_data, pd_util.
:- import_module assoc_list, bool, int, map, require, set.

:- type global_term_info
	--->	global_term_info(
			single_covering_goals,
			multiple_covering_goals
		).

	% We only deal with single atoms while unfolding.
:- type local_term_info == single_covering_goals.

	% For single goals, use the argument partition method.
:- type single_covering_goals == map(pred_proc_id, pd_proc_term_info).

	% Map from a pair of procedures at the end of the conjunction
	% to be deforested and the most recent ancestor with this pair
	% of goals.
:- type multiple_covering_goals ==
		map(proc_pair, pair(int, maybe(pred_proc_id))).

	% Mapping from argument to size.
:- type pd_proc_term_info	== 	assoc_list(int, int).
	
%-----------------------------------------------------------------------------%

pd_term__global_term_info_init(TermInfo) :-
	map__init(SingleGoals),
	map__init(MultipleGoals),
	TermInfo = global_term_info(SingleGoals, MultipleGoals).

pd_term__local_term_info_init(TermInfo) :-
	map__init(TermInfo).

pd_term__get_proc_term_info(TermInfo, PredProcId, ProcTermInfo) :-
	map__search(TermInfo, PredProcId, ProcTermInfo).

%-----------------------------------------------------------------------------%

pd_term__global_check(_ModuleInfo, EarlierGoal, BetweenGoals, MaybeLaterGoal, 
		_InstMap, Versions, Info0, Info, Result) :-
	Info0 = global_term_info(SingleGoalCover0, MultipleGoalCover0),
	(
		EarlierGoal = call(PredId1, ProcId1, _, _, _, _) - _,
		Hd = lambda([List::in, Head::out] is semidet, 
			List = [Head | _]),
		expand_calls(Hd, Versions, proc(PredId1, ProcId1), 
			FirstPredProcId),
		(
			MaybeLaterGoal = yes(
				call(PredId2, ProcId2, _, _, _, _) - _),
			expand_calls(list__last, Versions,
				proc(PredId2, ProcId2), LastPredProcId),
			MaybeLastPredProcId = yes(LastPredProcId)
		;
			MaybeLaterGoal = no,
			MaybeLastPredProcId = no
		)
	->
		ProcPair = FirstPredProcId - MaybeLastPredProcId,
		list__length(BetweenGoals, Length),
		( 
			map__search(MultipleGoalCover0, ProcPair, 
				MaxLength - MaybeCoveringPredProcId) 
		->
			(
				Length < MaxLength 
			->
				Result = ok(ProcPair, Length),
					% set the maybe(pred_proc_id)
					% when we create the new predicate
				map__set(MultipleGoalCover0, ProcPair,
					Length - no, MultipleGoalCover)
			;
				Length = MaxLength, 
				MaybeCoveringPredProcId = 
					yes(CoveringPredProcId) 
			->
				% If the goals match, check that the
				% argument insts decrease.
				% If not, we may need to do a 
				% generalisation step.
				Result = possible_loop(ProcPair, Length,
						CoveringPredProcId),
				MultipleGoalCover = MultipleGoalCover0
			;
				Result = loop,
				MultipleGoalCover = MultipleGoalCover0
			)
		;
			% We haven't seen this pair before, so it must
			% be okay to specialise.
			Result = ok(ProcPair, Length),

			% set the maybe(pred_proc_id)
			% when we create the new predicate
			map__set(MultipleGoalCover0, ProcPair,
				Length - no, MultipleGoalCover)
		),
		SingleGoalCover = SingleGoalCover0
	;
		error("pd_term__global_check")
	),
	Info = global_term_info(SingleGoalCover, MultipleGoalCover).

	% We don't want to use folded calls to parent versions 
	% when doing the global termination check, since that 
	% could give a sequence:
	%	old ....pred1
	% 	new1 .... pred1
	% 	new2 ....... pred1
	% 	new3 ......... pred1
	% Instead, we expand to predicates from the original program, 
	% which must contain a finite number of pairs of pred_proc_ids.
:- pred expand_calls(pred(list(pred_proc_id), pred_proc_id), version_index,
		pred_proc_id, pred_proc_id).
:- mode expand_calls(pred(in, out) is semidet, in, in, out) is semidet.

expand_calls(GetEnd, Versions, PredProcId0, PredProcId) :-
	( map__search(Versions, PredProcId0, VersionInfo) ->
		VersionInfo = version_info(_, Calls, _, _, _, _, _, _, _),
		call(GetEnd, Calls, PredProcId1),
		expand_calls(GetEnd, Versions, PredProcId1, PredProcId)
	;
		PredProcId = PredProcId0	
	).

%-----------------------------------------------------------------------------%

pd_term__local_check(ModuleInfo, Goal1, InstMap, Cover0, Cover) :-
	Goal1 = call(PredId, ProcId, Args, _, _, _) - _,
	( map__search(Cover0, proc(PredId, ProcId), CoveringInstSizes0) ->
		pd_term__do_local_check(ModuleInfo, InstMap, Args,
			CoveringInstSizes0, CoveringInstSizes),
		map__set(Cover0, proc(PredId, ProcId),
			CoveringInstSizes, Cover)
	;
		pd_term__initial_sizes(ModuleInfo, InstMap, 
			Args, 1, ArgInstSizes),
		map__set(Cover0, proc(PredId, ProcId), 
			ArgInstSizes, Cover)
	).

:- pred pd_term__do_local_check(module_info::in, instmap::in, 
		list(prog_var)::in, assoc_list(int, int)::in, 
		assoc_list(int, int)::out) is semidet.

pd_term__do_local_check(ModuleInfo, InstMap, Args, OldSizes, NewSizes) :-
	pd_term__get_matching_sizes(ModuleInfo, InstMap, Args, 
		OldSizes, NewSizes1, OldTotal, NewTotal),
	( NewTotal < OldTotal ->
		NewSizes = NewSizes1
	;
		pd_term__split_out_non_increasing(OldSizes, NewSizes1, 
			yes, NewSizes)
	).

%-----------------------------------------------------------------------------%

pd_term__update_global_term_info(TermInfo0, ProcPair, 
		PredProcId, Size, TermInfo) :-
	TermInfo0 = global_term_info(Single, Multiple0),
	map__set(Multiple0, ProcPair, Size - yes(PredProcId), Multiple),
	TermInfo = global_term_info(Single, Multiple).

%-----------------------------------------------------------------------------%

:- pred pd_term__initial_sizes(module_info::in, instmap::in, list(prog_var)::in,
		int::in, assoc_list(int, int)::out) is det.

pd_term__initial_sizes(_, _, [], _, []).
pd_term__initial_sizes(ModuleInfo, InstMap, [Arg | Args], ArgNo, 
		[ArgNo - Size | Sizes]) :-
	NextArgNo is ArgNo + 1,
	pd_term__initial_sizes(ModuleInfo, InstMap, Args, NextArgNo, Sizes),
	instmap__lookup_var(InstMap, Arg, ArgInst),
	pd_util__inst_size(ModuleInfo, ArgInst, Size).

%-----------------------------------------------------------------------------%

:- pred pd_term__get_matching_sizes(module_info::in, instmap::in, 
		list(prog_var)::in, assoc_list(int, int)::in, 
		assoc_list(int, int)::out, int::out, int::out) is det.

pd_term__get_matching_sizes(_, _, _, [], [], 0, 0).
pd_term__get_matching_sizes(ModuleInfo, InstMap, Args, 
		[ArgNo - OldSize | OldSizes], [ArgNo - NewSize | NewSizes], 
		OldTotal, NewTotal) :-
	pd_term__get_matching_sizes(ModuleInfo, InstMap, Args,
		OldSizes, NewSizes, OldTotal1, NewTotal1),
	list__index1_det(Args, ArgNo, Arg),
	instmap__lookup_var(InstMap, Arg, ArgInst),
	pd_util__inst_size(ModuleInfo, ArgInst, NewSize),
	OldTotal = OldTotal1 + OldSize,
	NewTotal = NewTotal1 + NewSize.
	
%-----------------------------------------------------------------------------%

:- pred pd_term__split_out_non_increasing(assoc_list(int, int)::in,
		assoc_list(int, int)::in, bool::out,
		assoc_list(int, int)::out) is semidet.

pd_term__split_out_non_increasing([], [], no, []).
pd_term__split_out_non_increasing([_|_], [], _, _) :-
	error("pd_term__split_out_non_increasing").
pd_term__split_out_non_increasing([], [_|_], _, _) :-
	error("pd_term__split_out_non_increasing").
pd_term__split_out_non_increasing([Arg - OldSize | Args0], 
		[_ - NewSize | Args], FoundDecreasing, NonIncreasing) :-
	pd_term__split_out_non_increasing(Args0, Args,
		FoundDecreasing1, NonIncreasing1),
	( NewSize =< OldSize ->
		NonIncreasing = [Arg - NewSize | NonIncreasing1],
		( NewSize = OldSize ->
			FoundDecreasing = no
		;
			FoundDecreasing = yes
		)
	;
		NonIncreasing = NonIncreasing1,
		FoundDecreasing = FoundDecreasing1
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
