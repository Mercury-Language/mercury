%-----------------------------------------------------------------------------
% Copyright (C) 1997-1998, 2003 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------
%
% term_pass2.m
%
% Main author of original version: crs.
% Main author of this version: zs.
%
% This file contains the code that tries to prove that procedures terminate.
%
% For details, please refer to the papers mentioned in termination.m.
%-----------------------------------------------------------------------------

:- module transform_hlds__term_pass2.
:- interface.

:- import_module hlds__hlds_module.
:- import_module hlds__hlds_pred.
:- import_module transform_hlds__term_util.

:- import_module list.

:- pred prove_termination_in_scc(list(pred_proc_id)::in, module_info::in,
	pass_info::in, int::in, termination_info::out) is det.

:- implementation.

:- import_module transform_hlds__term_traversal.
:- import_module transform_hlds__term_errors.
:- import_module hlds__hlds_goal.
:- import_module parse_tree__prog_data.
:- import_module check_hlds__type_util.
:- import_module check_hlds__mode_util.

:- import_module std_util, bool, int, assoc_list.
:- import_module set, bag, map, term, require.

:- type fixpoint_dir
	--->	up
	;	down.

:- type call_weight_info
	==	pair(list(term_errors__error), call_weight_graph).


:- type call_weight_graph
	==	map(pred_proc_id,		% The max noninfinite weight
						% call from this proc
			map(pred_proc_id,	% to this proc
				pair(prog_context, int))).
						% is at this context and with
						% this weight.

:- type pass2_result
	--->	ok(
			call_weight_info,
			used_args
		)
	;	error(
			list(term_errors__error)
		).

%-----------------------------------------------------------------------------

prove_termination_in_scc(SCC, Module, PassInfo, SingleArgs, Termination) :-
	init_rec_input_suppliers(SCC, Module, InitRecSuppliers),
	prove_termination_in_scc_trial(SCC, InitRecSuppliers, down,
		Module, PassInfo, Termination1),
	(
		Termination1 = can_loop(Errors),
		(
			% On large SCCs, single arg analysis can require
			% many iterations, so we allow the user to limit
			% the size of the SCCs we will try it on.
			list__length(SCC, ProcCount),
			ProcCount =< SingleArgs,

			% Don't try single arg analysis if it cannot cure
			% the reason for the failure of the main analysis.
			\+ (
				list__member(Error, Errors),
				Error = _ - imported_pred
			),
			prove_termination_in_scc_single_arg(SCC,
				Module, PassInfo)
		->
			Termination = cannot_loop
		;
			Termination = Termination1
		)
	;
		Termination1 = cannot_loop,
		Termination = Termination1
	).

	% Initialise the set of recursive input suppliers to be the set
	% of all input variables in all procedures of the SCC.

:- pred init_rec_input_suppliers(list(pred_proc_id)::in, module_info::in,
	used_args::out) is det.

init_rec_input_suppliers([], _, InitMap) :-
	map__init(InitMap).
init_rec_input_suppliers([PPId | PPIds], Module, RecSupplierMap) :-
	init_rec_input_suppliers(PPIds, Module, RecSupplierMap0),
	PPId = proc(PredId, ProcId),
	module_info_pred_proc_info(Module, PredId, ProcId, _, ProcInfo),
	proc_info_headvars(ProcInfo, HeadVars),
	proc_info_argmodes(ProcInfo, ArgModes),
	partition_call_args(Module, ArgModes, HeadVars, InArgs, _OutVars),
	MapIsInput = (pred(HeadVar::in, Bool::out) is det :-
		( bag__contains(InArgs, HeadVar) ->
			Bool = yes
		;
			Bool = no
		)
	),
	list__map(MapIsInput, HeadVars, BoolList),
	map__det_insert(RecSupplierMap0, PPId, BoolList, RecSupplierMap).

%-----------------------------------------------------------------------------

	% Perform single arg analysis on the SCC.
	%
	% We pick one procedure in the SCC (one of those with minimal arity).
	% We set the recursive input suppliers of this procedure to contain
	% only the first input argument, and the recursive input suppliers
	% of the other procedures to the empty set, and try a fixpoint
	% iteration. If it works, great, if not, try again with the next
	% input arg of the selected procedure, until we run out of input
	% arguments of that procedure.
	%
	% While the fixpoint iteration in the main algorithm looks for the
	% greatest fixpoint, in which the recursive input supplier sets
	% cannot increase, in single arg analysis we are looking for a
	% smallest fixpoint starting from a given location, so we must
	% make sure that the recursive input supplier sets cannot decrease.

:- pred prove_termination_in_scc_single_arg(list(pred_proc_id)::in,
	module_info::in, pass_info::in) is semidet.

prove_termination_in_scc_single_arg(SCC, Module, PassInfo) :-
	( SCC = [FirstPPId | LaterPPIds] ->
		lookup_proc_arity(FirstPPId, Module, FirstArity),
		find_min_arity_proc(LaterPPIds, FirstPPId, FirstArity, Module,
			TrialPPId, RestSCC),
		prove_termination_in_scc_single_arg_2(TrialPPId, RestSCC, 1,
			Module, PassInfo)
	;
		error("empty SCC in prove_termination_in_scc_single_arg")
	).

	% Find a procedure of minimum arity among the given list and the
	% tentative guess.

:- pred find_min_arity_proc(list(pred_proc_id)::in, pred_proc_id::in, int::in,
	module_info::in, pred_proc_id::out, list(pred_proc_id)::out) is det.

find_min_arity_proc([], BestSofarPPId, _, _, BestSofarPPId, []).
find_min_arity_proc([PPId | PPIds], BestSofarPPId, BestSofarArity, Module,
		BestPPId, RestSCC) :-
	lookup_proc_arity(PPId, Module, Arity),
	( Arity < BestSofarArity ->
		find_min_arity_proc(PPIds, PPId, Arity,
			Module, BestPPId, RestSCC0),
		RestSCC = [BestSofarPPId | RestSCC0]
	;
		find_min_arity_proc(PPIds, BestSofarPPId, BestSofarArity,
			Module, BestPPId, RestSCC0),
		RestSCC = [PPId | RestSCC0]
	).

	% Perform single arg analysis on the SCC.

:- pred prove_termination_in_scc_single_arg_2(pred_proc_id::in,
	list(pred_proc_id)::in, int::in, module_info::in, pass_info::in)
	is semidet.

prove_termination_in_scc_single_arg_2(TrialPPId, RestSCC, ArgNum0,
		Module, PassInfo) :-
	init_rec_input_suppliers_single_arg(TrialPPId, RestSCC,
		ArgNum0, Module, InitRecSuppliers),
	prove_termination_in_scc_trial([TrialPPId | RestSCC], InitRecSuppliers,
		up, Module, PassInfo, Termination),
	( Termination = cannot_loop ->
		true
	;
		ArgNum1 = ArgNum0 + 1,
		prove_termination_in_scc_single_arg_2(TrialPPId, RestSCC,
			ArgNum1, Module, PassInfo)
	).

:- pred init_rec_input_suppliers_single_arg(pred_proc_id::in,
	list(pred_proc_id)::in, int::in, module_info::in, used_args::out)
	is semidet.

init_rec_input_suppliers_single_arg(TrialPPId, RestSCC, ArgNum, Module,
		RecSupplierMap) :-
	TrialPPId = proc(PredId, ProcId),
	module_info_pred_proc_info(Module, PredId, ProcId, _, ProcInfo),
	proc_info_argmodes(ProcInfo, ArgModes),
	init_rec_input_suppliers_add_single_arg(ArgModes, ArgNum,
		Module, TrialPPIdRecSuppliers),
	map__init(RecSupplierMap0),
	map__det_insert(RecSupplierMap0, TrialPPId, TrialPPIdRecSuppliers,
		RecSupplierMap1),
	init_rec_input_suppliers_single_arg_others(RestSCC, Module,
		RecSupplierMap1, RecSupplierMap).

:- pred init_rec_input_suppliers_add_single_arg(list(mode)::in, int::in,
	module_info::in, list(bool)::out) is semidet.

init_rec_input_suppliers_add_single_arg([Mode | Modes], ArgNum, Module,
		BoolList) :-
	(
		mode_is_input(Module, Mode),
		ArgNum = 1
	->
		list__map(map_to_no, Modes, BoolList1),
		BoolList = [yes | BoolList1]
	;
		(
			mode_is_output(Module, Mode)
		->
			NextArgNum = ArgNum
		;
			mode_is_input(Module, Mode),
			ArgNum > 1
		->
			NextArgNum = ArgNum - 1
		;
			fail
		)
	->
		init_rec_input_suppliers_add_single_arg(Modes, NextArgNum,
			Module, BoolList1),
		BoolList = [no | BoolList1]
	;
		fail
	).

:- pred init_rec_input_suppliers_single_arg_others(list(pred_proc_id)::in,
	module_info::in, used_args::in, used_args::out) is det.

init_rec_input_suppliers_single_arg_others([], _,
	RecSupplierMap, RecSupplierMap).
init_rec_input_suppliers_single_arg_others([PPId | PPIds], Module,
		RecSupplierMap0, RecSupplierMap) :-
	PPId = proc(PredId, ProcId),
	module_info_pred_proc_info(Module, PredId, ProcId, _, ProcInfo),
	proc_info_headvars(ProcInfo, HeadVars),
	list__map(map_to_no, HeadVars, BoolList),
	map__det_insert(RecSupplierMap0, PPId, BoolList, RecSupplierMap1),
	init_rec_input_suppliers_single_arg_others(PPIds, Module,
		RecSupplierMap1, RecSupplierMap).

:- pred lookup_proc_arity(pred_proc_id::in, module_info::in, int::out) is det.

lookup_proc_arity(PPId, Module, Arity) :-
	PPId = proc(PredId, ProcId),
	module_info_pred_proc_info(Module, PredId, ProcId, _, ProcInfo),
	proc_info_headvars(ProcInfo, HeadVars),
	list__length(HeadVars, Arity).

%-----------------------------------------------------------------------------

:- pred prove_termination_in_scc_trial(list(pred_proc_id)::in, used_args::in,
	fixpoint_dir::in, module_info::in, pass_info::in,
	termination_info::out) is det.

prove_termination_in_scc_trial(SCC, InitRecSuppliers, FixDir, Module,
		PassInfo, Termination) :-
	prove_termination_in_scc_fixpoint(SCC, FixDir, Module, PassInfo,
		InitRecSuppliers, Result),
	(
		Result = ok(CallInfo, _),
		CallInfo = InfCalls - CallWeights,
		(
			InfCalls \= []
		->
			PassInfo = pass_info(_, MaxErrors, _),
			list__take_upto(MaxErrors, InfCalls, ReportedInfCalls),
			Termination = can_loop(ReportedInfCalls)
		;
			zero_or_positive_weight_cycles(CallWeights, Module,
				Cycles),
			Cycles \= []
		->
			PassInfo = pass_info(_, MaxErrors, _),
			list__take_upto(MaxErrors, Cycles, ReportedCycles),
			Termination = can_loop(ReportedCycles)
		;
			Termination = cannot_loop
		)
	;
		Result = error(Errors),
		Termination = can_loop(Errors)
	).

%-----------------------------------------------------------------------------

:- pred prove_termination_in_scc_fixpoint(list(pred_proc_id)::in,
	fixpoint_dir::in, module_info::in, pass_info::in, used_args::in,
	pass2_result::out) is det.

prove_termination_in_scc_fixpoint(SCC, FixDir, Module, PassInfo,
		RecSupplierMap0, Result) :-
	% unsafe_perform_io(io__write_string("prove_termination_in_scc\n")),
	% unsafe_perform_io(io__write(RecSupplierMap0)),
	% unsafe_perform_io(io__write_string("\n")),
	map__init(NewRecSupplierMap0),
	map__init(CallWeightGraph0),
	CallInfo0 = [] - CallWeightGraph0,
	prove_termination_in_scc_pass(SCC, FixDir, Module, PassInfo,
		RecSupplierMap0, NewRecSupplierMap0, CallInfo0, Result1),
	(
		Result1 = ok(_, RecSupplierMap1),
		( RecSupplierMap1 = RecSupplierMap0 ->
			% We are at a fixed point, so further analysis
			% will not get any better results.
			Result = Result1
		;
			prove_termination_in_scc_fixpoint(SCC, FixDir,
				Module, PassInfo, RecSupplierMap1, Result)
		)
	;
		Result1 = error(_),
		Result = Result1
	).

%-----------------------------------------------------------------------------

	% Process a whole SCC, to determine the termination property of each
	% procedure in that SCC.

:- pred prove_termination_in_scc_pass(list(pred_proc_id)::in, fixpoint_dir::in,
	module_info::in, pass_info::in, used_args::in, used_args::in,
	call_weight_info::in, pass2_result::out) is det.

prove_termination_in_scc_pass([], _, _, _, _, NewRecSupplierMap, CallInfo,
		ok(CallInfo, NewRecSupplierMap)).
prove_termination_in_scc_pass([PPId | PPIds], FixDir, Module, PassInfo,
		RecSupplierMap, NewRecSupplierMap0, CallInfo0, Result) :-
	% Get the goal info.
	PPId = proc(PredId, ProcId),
	module_info_pred_proc_info(Module, PredId, ProcId, PredInfo, ProcInfo),
	pred_info_context(PredInfo, Context),
	proc_info_goal(ProcInfo, Goal),
	proc_info_vartypes(ProcInfo, VarTypes),
	map__init(EmptyMap),
	PassInfo = pass_info(FunctorInfo, MaxErrors, MaxPaths),
	init_traversal_params(Module, FunctorInfo, PPId, Context, VarTypes,
		EmptyMap, RecSupplierMap, MaxErrors, MaxPaths, Params),
	set__init(PathSet0),
	Info0 = ok(PathSet0, []),
	traverse_goal(Goal, Params, Info0, Info),
	(
		Info = ok(Paths, CanLoop),
		require(unify(CanLoop, []),
			"can_loop detected in pass2 but not pass1"),
		set__to_sorted_list(Paths, PathList),
		upper_bound_active_vars(PathList, ActiveVars),
		map__lookup(RecSupplierMap, PPId, RecSuppliers0),
		proc_info_headvars(ProcInfo, Args),
		bag__init(EmptyBag),
		update_rec_input_suppliers(Args, ActiveVars, FixDir,
			RecSuppliers0, RecSuppliers,
			EmptyBag, RecSuppliers0Bag),
		map__det_insert(NewRecSupplierMap0, PPId, RecSuppliers,
			NewRecSupplierMap1),
		add_call_arcs(PathList, RecSuppliers0Bag,
			CallInfo0, CallInfo1),
		prove_termination_in_scc_pass(PPIds, FixDir, Module,
			PassInfo, RecSupplierMap,
			NewRecSupplierMap1, CallInfo1, Result)
	;
		Info = error(Errors, CanLoop),
		require(unify(CanLoop, []),
			"can_loop detected in pass2 but not pass1"),
		Result = error(Errors)
	).

%-----------------------------------------------------------------------------

:- pred update_rec_input_suppliers(list(prog_var)::in, bag(prog_var)::in,
	fixpoint_dir::in, list(bool)::in, list(bool)::out,
	bag(prog_var)::in, bag(prog_var)::out) is det.

update_rec_input_suppliers([], _, _, [], [], RecBag, RecBag).
update_rec_input_suppliers([_ | _], _, _, [], [], _, _) :-
	error("update_rec_input_suppliers: Unmatched variables").
update_rec_input_suppliers([], _, _, [_ | _], [], _, _) :-
	error("update_rec_input_suppliers: Unmatched variables").
update_rec_input_suppliers([Arg | Args], ActiveVars, FixDir,
		[RecInputSupplier0 | RecInputSuppliers0],
		[RecInputSupplier | RecInputSuppliers],
		RecBag0, RecBag) :-
	(
		RecInputSupplier0 = yes,
		bag__insert(RecBag0, Arg, RecBag1)
	;
		RecInputSupplier0 = no,
		RecBag1 = RecBag0
	),
	(
		FixDir = down,
		% This guarantees that the set of rec input suppliers
		% can only decrease.
		( bag__contains(ActiveVars, Arg) ->
			RecInputSupplier = RecInputSupplier0
		;
			RecInputSupplier = no
		)
	;
		FixDir = up,
		% This guarantees that the set of rec input suppliers
		% can only increase.
		( bag__contains(ActiveVars, Arg) ->
			RecInputSupplier = yes
		;
			RecInputSupplier = RecInputSupplier0
		)
	),
	update_rec_input_suppliers(Args, ActiveVars, FixDir,
		RecInputSuppliers0, RecInputSuppliers, RecBag1, RecBag).

%-----------------------------------------------------------------------------

% This adds the information from a stage 2 traversal to the graph.
% The graph's nodes are the procedures in the current SCC.
% The graph's edges represent calls from one procedure in the SCC to another.
% The number attached to the edge from p to q shows the upper bound
% on the difference between the size of the recursive input supplier arguments
% in the call to q and the size of the recursive input supplier arguments
% in the head of p. If there is no finite upper bound, then we insert the
% details of the call into the list of "infinite" calls.

:- pred add_call_arcs(list(path_info)::in,
	bag(prog_var)::in, call_weight_info::in, call_weight_info::out) is det.

add_call_arcs([], _RecInputSuppliers, CallInfo, CallInfo).
add_call_arcs([Path | Paths], RecInputSuppliers, CallInfo0, CallInfo) :-
	Path = path_info(PPId, CallSite, GammaConst, GammaVars, ActiveVars),
	( CallSite = yes(CallPPIdPrime - ContextPrime) ->
		CallPPId = CallPPIdPrime,
		Context = ContextPrime
	;
		error("no call site in path in stage 2")
	),
	( GammaVars = [] ->
		true
	;
		error("gamma variables in path in stage 2")
	),
	CallInfo0 = InfCalls0 - CallWeights0,
	( bag__is_subbag(ActiveVars, RecInputSuppliers) ->
		( map__search(CallWeights0, PPId, NeighbourMap0) ->
			( map__search(NeighbourMap0, CallPPId, OldEdgeInfo) ->
				OldEdgeInfo = _OldContext - OldWeight,
				( OldWeight >= GammaConst ->
					EdgeInfo = OldEdgeInfo
				;
					EdgeInfo = Context - GammaConst
				),
				map__det_update(NeighbourMap0, CallPPId,
					EdgeInfo, NeighbourMap)
			;
				map__det_insert(NeighbourMap0, CallPPId,
					Context - GammaConst, NeighbourMap)
			),
			map__det_update(CallWeights0, PPId, NeighbourMap,
				CallWeights1)
		;
			map__init(NeighbourMap0),
			map__det_insert(NeighbourMap0, CallPPId,
				Context - GammaConst, NeighbourMap),
			map__det_insert(CallWeights0, PPId, NeighbourMap,
				CallWeights1)
		),
		CallInfo1 = InfCalls0 - CallWeights1
	;
		InfCalls1 = [Context - inf_call(PPId, CallPPId) | InfCalls0],
		CallInfo1 = InfCalls1 - CallWeights0
	),
	add_call_arcs(Paths, RecInputSuppliers, CallInfo1, CallInfo).

%-----------------------------------------------------------------------------

	% We use a simple depth first search to find and return the list
	% of all cycles in the call graph of the SCC where the change in
	% the size of the recursive input supplier arguments of the procedure
	% that serves as the start and end point of the circularity are
	% not guaranteed to decrease.
	%
	% Finding one such cycle is enough for us to conclude that we
	% cannot prove termination of the procedures in the SCC; we collect
	% all cycles because it may be useful to print them out (if not
	% all, then maybe a limited set).

:- pred zero_or_positive_weight_cycles(call_weight_graph::in,
	module_info::in, list(term_errors__error)::out) is det.

zero_or_positive_weight_cycles(CallWeights, Module, Cycles) :-
	map__keys(CallWeights, PPIds),
	zero_or_positive_weight_cycles_2(PPIds, CallWeights, Module, Cycles).

:- pred zero_or_positive_weight_cycles_2(list(pred_proc_id)::in,
	call_weight_graph::in, module_info::in,
	list(term_errors__error)::out) is det.

zero_or_positive_weight_cycles_2([], _, _, []).
zero_or_positive_weight_cycles_2([PPId | PPIds], CallWeights, Module, Cycles) :-
	zero_or_positive_weight_cycles_from(PPId, CallWeights, Module, Cycles1),
	zero_or_positive_weight_cycles_2(PPIds, CallWeights, Module, Cycles2),
	list__append(Cycles1, Cycles2, Cycles).

:- pred zero_or_positive_weight_cycles_from(pred_proc_id::in,
	call_weight_graph::in, module_info::in,
	list(term_errors__error)::out) is det.

zero_or_positive_weight_cycles_from(PPId, CallWeights, Module, Cycles) :-
	map__lookup(CallWeights, PPId, NeighboursMap),
	map__to_assoc_list(NeighboursMap, NeighboursList),
	PPId = proc(PredId, _ProcId),
	module_info_pred_info(Module, PredId, PredInfo),
	pred_info_context(PredInfo, Context),
	zero_or_positive_weight_cycles_from_neighbours(NeighboursList,
		PPId, Context, 0, [], CallWeights, Cycles).

:- pred zero_or_positive_weight_cycles_from_neighbours(assoc_list(pred_proc_id,
	pair(prog_context, int))::in, pred_proc_id::in, prog_context::in,
	int::in, assoc_list(pred_proc_id, prog_context)::in,
	call_weight_graph::in, list(term_errors__error)::out) is det.

zero_or_positive_weight_cycles_from_neighbours([], _, _, _, _, _, []).
zero_or_positive_weight_cycles_from_neighbours([Neighbour | Neighbours],
		LookforPPId, Context, WeightSoFar, VisitedCalls, CallWeights,
		Cycles) :-
	zero_or_positive_weight_cycles_from_neighbour(Neighbour, LookforPPId,
		Context, WeightSoFar, VisitedCalls, CallWeights, Cycles1),
	zero_or_positive_weight_cycles_from_neighbours(Neighbours, LookforPPId,
		Context, WeightSoFar, VisitedCalls, CallWeights, Cycles2),
	list__append(Cycles1, Cycles2, Cycles).

:- pred zero_or_positive_weight_cycles_from_neighbour(pair(pred_proc_id,
	pair(prog_context, int))::in, pred_proc_id::in, prog_context::in,
	int::in, assoc_list(pred_proc_id, prog_context)::in,
	call_weight_graph::in, list(term_errors__error)::out) is det.

zero_or_positive_weight_cycles_from_neighbour(CurPPId - (Context - EdgeWeight),
		LookforPPId, ProcContext, WeightSoFar0, VisitedCalls,
		CallWeights, Cycles) :-
	WeightSoFar1 = WeightSoFar0 + EdgeWeight,
	(
		CurPPId = LookforPPId
	->
		% We have a cycle on the looked for ppid.
		( WeightSoFar1 >= 0 ->
			FinalVisitedCalls = [CurPPId - Context | VisitedCalls],
			list__reverse(FinalVisitedCalls, RevFinalVisitedCalls),
			Cycles = [ProcContext -
				cycle(LookforPPId, RevFinalVisitedCalls)]
		;
			Cycles = []
		)
	;
		assoc_list__keys(VisitedCalls, VisitedPPIds),
		list__member(CurPPId, VisitedPPIds)
	->
		% We have a cycle, but not on the looked for ppid.
		% We ignore it here; it will be picked up when we process
		% that ppid.
		Cycles = []
	;
		% No cycle; try all possible edges from this node.
		NewVisitedCalls = [CurPPId - Context | VisitedCalls],
		map__lookup(CallWeights, CurPPId, NeighboursMap),
		map__to_assoc_list(NeighboursMap, NeighboursList),
		zero_or_positive_weight_cycles_from_neighbours(NeighboursList,
			LookforPPId, ProcContext, WeightSoFar1,
			NewVisitedCalls, CallWeights, Cycles)
	).

%-----------------------------------------------------------------------------

:- pred map_to_no(T::in, bool::out) is det.

map_to_no(_, no).

%-----------------------------------------------------------------------------
