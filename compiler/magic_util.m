%-----------------------------------------------------------------------------%
% Copyright (C) 1998-2002 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% File: magic_util.m
% Main author: stayl
%
% Predicates used by magic.m and context.m to transform Aditi procedures.
%
% Note: this module contains multiple interface sections.
%-----------------------------------------------------------------------------%
:- module aditi_backend__magic_util.

:- interface.

:- import_module hlds__hlds_goal, hlds__hlds_module, hlds__hlds_pred.
:- import_module parse_tree__prog_data.
:- import_module bool, io, list, map, set, std_util.

	% Check that the argument types and modes are legal for
	% an Aditi relation.
:- pred magic_util__check_args(list(prog_var)::in, list(mode)::in,
		list(type)::in, prog_context::in, magic_arg_id_type::in,
		magic_info::in, magic_info::out) is det.

:- pred magic_util__report_errors(list(magic_error)::in, module_info::in,
		bool::in, io__state::di, io__state::uo) is det.

	% Determine whether a given goal contains a call to an 
	% Aditi procedure. Strip out any explicit quantifications
	% around Aditi calls, since they just get in the way.
	% Multiple nested explicit quantifications should have
	% been removed by simplify.m.
:- pred magic_util__goal_is_aditi_call(module_info::in,
		map(pred_proc_id, pred_proc_id)::in, hlds_goal::in,
		db_call::out, list(hlds_goal)::out) is semidet.
	
	% Information about a database call.
:- type db_call
	--->	db_call(
			maybe(list(hlds_goal)),	% aggregate input closures
			hlds_goal,		% goal containing the call
			pred_proc_id,
			list(prog_var),		% arguments
			list(prog_var),		% input arguments
			list(prog_var),		% output arguments
			maybe(pair(list(hlds_goal), hlds_goal_info))
					% goals after the call in a negation,
					% and the goal_info for the negation.
		).

:- pred magic_util__db_call_nonlocals(db_call::in, set(prog_var)::out) is det.
:- pred magic_util__db_call_input_args(db_call::in,
		list(prog_var)::out) is det.
:- pred magic_util__db_call_output_args(db_call::in,
		list(prog_var)::out) is det.
:- pred magic_util__db_call_context(db_call::in, prog_context::out) is det.
:- pred magic_util__db_call_pred_proc_id(db_call::in,
		pred_proc_id::out) is det.

:- pred magic_util__rename_vars_in_db_call(db_call::in,
		map(prog_var, prog_var)::in, db_call::out) is det.

	% Do all the necessary goal fiddling to handle the input
	% to an Aditi procedure.
:- pred magic_util__setup_call(list(hlds_goal)::in, db_call::in,
		set(prog_var)::in, list(hlds_goal)::out,
		magic_info::in, magic_info::out) is det.

	% Create a closure given the goal and arguments.
:- pred magic_util__create_closure(int::in, prog_var::in, (mode)::in,
		hlds_goal::in, list(prog_var)::in, list(prog_var)::in,
		hlds_goal::out, magic_info::in, magic_info::out) is det.

	% Add the goal as a disjunct of the magic predicate for the
	% pred_proc_id. The list of variables is the list of head
	% variables of the `clause'.
:- pred magic_util__add_to_magic_predicate(pred_proc_id::in, hlds_goal::in,
		list(prog_var)::in, magic_info::in, magic_info::out) is det.

	% Get information to build a call to the magic
	% predicate for the current procedure.
:- pred magic_util__magic_call_info(pred_id::out, proc_id::out, sym_name::out,
		list(prog_var)::out, list(prog_var)::out, list(mode)::out,
		magic_info::in, magic_info::out) is det.

	% Convert all modes to output, creating test unifications 
	% where the original mode was input. This will result in
	% a join on the input attributes.
:- pred magic_util__create_input_test_unifications(module_info::in,
		list(prog_var)::in, list(prog_var)::in, list(mode)::in,
		list(prog_var)::out, list(hlds_goal)::in, list(hlds_goal)::out,
		hlds_goal_info::in, hlds_goal_info::out,
		proc_info::in, proc_info::out) is det.
		
	% Convert an input mode to output.
:- pred magic_util__mode_to_output_mode(module_info::in,
		(mode)::in, (mode)::out) is det.

	% Adjust an index to account for the removal of the `aditi:state'
	% from the argument list.
:- pred magic_util__adjust_index(list(type)::in, index_spec::in,
		index_spec::out) is det.

	% Remove any aditi:states from the set of vars.
:- pred magic_util__restrict_nonlocals(set(prog_var)::in, set(prog_var)::out,
		magic_info::in, magic_info::out) is det.

	% Given a prefix, create a unique new name for the predicate
	% using prog_util__make_pred_name_with_context. The boolean
	% states whether a counter should be attached to the name. This
	% should be `no' for names which should be visible to other modules.
:- pred magic_util__make_pred_name(pred_info::in, proc_id::in, string::in,
	bool::in, sym_name::out, magic_info::in, magic_info::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds__hlds_data, ll_backend__code_util.
:- import_module check_hlds__type_util, check_hlds__mode_util.
:- import_module check_hlds__inst_match.
:- import_module hlds__instmap, hlds__goal_util, parse_tree__prog_out.
:- import_module hlds__hlds_out, hlds__error_util, parse_tree__prog_util.
:- import_module (parse_tree__inst), check_hlds__polymorphism.

:- import_module assoc_list, int, require, string, term, varset.

magic_util__db_call_nonlocals(
		db_call(MaybeClosures, Call, _, _, _, _, MaybeNegGoals),
		NonLocals) :-
	( MaybeClosures = yes(Closures) ->
		goal_list_nonlocals(Closures, NonLocals0)
	;
		set__init(NonLocals0)
	),
	Call = _ - CallInfo,
	goal_info_get_nonlocals(CallInfo, NonLocals1),
	set__union(NonLocals0, NonLocals1, NonLocals2),
	( MaybeNegGoals = yes(_ - NegGoalInfo) ->
		goal_info_get_nonlocals(NegGoalInfo, NonLocals3),
		set__union(NonLocals2, NonLocals3, NonLocals)
	;
		NonLocals = NonLocals2
	).

magic_util__db_call_context(db_call(_, _ - Info, _, _, _, _, _), Context) :-
	goal_info_get_context(Info, Context).
magic_util__db_call_pred_proc_id(db_call(_, _, PredProcId, _, _, _, _),
		PredProcId).
magic_util__db_call_input_args(db_call(_, _, _, _, Inputs, _, _), Inputs).
magic_util__db_call_output_args(db_call(_, _, _, _, _, Outputs, _), Outputs).

magic_util__rename_vars_in_db_call(Call0, Subn, Call) :-
	Call0 = db_call(MaybeClosures0, Goal0, PredProcId, Args0,
			Inputs0, Outputs0, MaybeNegGoals0),
	(
		MaybeClosures0 = yes(Closures0),
		goal_util__rename_vars_in_goals(Closures0, no, Subn, Closures),
		MaybeClosures = yes(Closures)
	;
		MaybeClosures0 = no,
		MaybeClosures = no
	),
	goal_util__rename_vars_in_goal(Goal0, Subn, Goal),
	goal_util__rename_var_list(Args0, no, Subn, Args),
	goal_util__rename_var_list(Inputs0, no, Subn, Inputs),
	goal_util__rename_var_list(Outputs0, no, Subn, Outputs), 
	(
		MaybeNegGoals0 = yes(NegGoals0 - NegGoalInfo0),
		goal_util__rename_vars_in_goals(NegGoals0, no, Subn, NegGoals),
		goal_util__rename_vars_in_goal(conj([]) - NegGoalInfo0,
			Subn, _ - NegGoalInfo),
		MaybeNegGoals = yes(NegGoals - NegGoalInfo)
	;
		MaybeNegGoals0 = no,
		MaybeNegGoals = no
	),
	Call = db_call(MaybeClosures, Goal, PredProcId, Args,
			Inputs, Outputs, MaybeNegGoals).

%-----------------------------------------------------------------------------%

magic_util__goal_is_aditi_call(ModuleInfo, PredMap,
		Goal0, Call, AfterGoals) :-
	%
	% Strip off any explicit quantification. There should only
	% be one, since simplification removes nested quantifications
	% and multiple nested quantifications are not considered 
	% atomic by dnf.m.
	%
	( Goal0 = some(_, _, Goal1) - _ -> 
		Goal2 = Goal1
	;
		Goal2 = Goal0
	),
	goal_to_conj_list(Goal2, Goals2),
	Goals2 = [PossibleCallGoal | AfterGoals0],
	( PossibleCallGoal = not(NegGoal0) - NegGoalInfo ->
		magic_util__neg_goal_is_aditi_call(ModuleInfo, PredMap,
			NegGoal0, NegGoalInfo, Call),
		AfterGoals = AfterGoals0
	;			
		magic_util__goal_is_aditi_call_2(ModuleInfo, PredMap,
			Goals2, Call, AfterGoals)
	).

:- pred magic_util__goal_is_aditi_call_2(module_info::in, pred_map::in,
	list(hlds_goal)::in, db_call::out, list(hlds_goal)::out) is semidet.

magic_util__goal_is_aditi_call_2(ModuleInfo, PredMap,
		Goals, Call, AfterGoals) :-
	(
		% Is the goal an aggregate? If so, magic__preprocess_goal
		% should have placed the closures next to the aggregate call.
		Goals = [Closure1a, Closure2a, Closure3a,
				CallGoal | AfterGoals0],
		CallGoal = call(PredId, ProcId, Args, _,_,_) - _,
		hlds_pred__is_aditi_aggregate(ModuleInfo, PredId),
		magic_util__check_aggregate_closure(Closure1a, Closure1),
		magic_util__check_aggregate_closure(Closure2a, Closure2),
		magic_util__check_aggregate_closure(Closure3a, Closure3)
	->
		AfterGoals = AfterGoals0,
		Call = db_call(yes([Closure1, Closure2, Closure3]),
			CallGoal, proc(PredId, ProcId), Args, [], Args, no)
	;
		% Is the goal an ordinary database call.
		Goals = [Goal0 | AfterGoals],
		Goal0 = call(PredId, ProcId, Args, _, _, _) - _,
		(
			% The original predicate may have been stripped of its
			% aditi marker by magic__interface_to_c, so check
			% if the procedure was renamed by the preprocessing
			% pass, if so it is an Aditi procedure.
			map__contains(PredMap, proc(PredId, ProcId))
		;
			hlds_pred__is_aditi_relation(ModuleInfo, PredId)
		),
		magic_util__construct_db_call(ModuleInfo, PredId, ProcId,
			Args, Goal0, Call)
	).	

:- pred magic_util__neg_goal_is_aditi_call(module_info::in, pred_map::in,
		hlds_goal::in, hlds_goal_info::in, db_call::out) is semidet.

magic_util__neg_goal_is_aditi_call(ModuleInfo, PredMap,
		NegGoal0, NegGoalInfo, Call) :-
	% This is safe because nested negations should be
	% transformed into calls by dnf.m.
	magic_util__goal_is_aditi_call(ModuleInfo, PredMap,
		NegGoal0, NegCall, AfterGoals),
	NegCall = db_call(A, B, C, D, E, F, _),
	Call = db_call(A, B, C, D, E, F, yes(AfterGoals - NegGoalInfo)).

:- pred magic_util__check_aggregate_closure(hlds_goal::in,
		hlds_goal::out) is semidet.

magic_util__check_aggregate_closure(Goal, Goal) :-
	Goal = unify(_, _, _, Uni, _) - _,
	Uni = construct(_, pred_const(_, _, _), _, _, _, _, _).	

:- pred magic_util__construct_db_call(module_info::in, pred_id::in,
	proc_id::in, list(prog_var)::in, hlds_goal::in, db_call::out) is det.

magic_util__construct_db_call(ModuleInfo, PredId, ProcId,
		Args0, Goal0, Call) :-
	module_info_pred_proc_info(ModuleInfo, PredId, ProcId,
		PredInfo, ProcInfo),
	pred_info_arg_types(PredInfo, ArgTypes),
	proc_info_argmodes(ProcInfo, ArgModes0),
	type_util__remove_aditi_state(ArgTypes, ArgModes0, ArgModes),
	type_util__remove_aditi_state(ArgTypes, Args0, Args),
	partition_args(ModuleInfo, ArgModes, Args, InputArgs, OutputArgs),
	Call = db_call(no, Goal0, proc(PredId, ProcId), Args,
		InputArgs, OutputArgs, no).

%-----------------------------------------------------------------------------%

magic_util__adjust_index(ArgTypes, index_spec(IndexType, Attrs0),
		index_spec(IndexType, Attrs)) :-
	construct_type(qualified(unqualified("aditi"), "state") - 0,
		[], StateType),
	( list__nth_member_search(ArgTypes, StateType, StateIndex) ->
		AdjustAttr = lambda([Attr0::in, Attr::out] is det, (
			( Attr0 < StateIndex ->
				Attr = Attr0
			; Attr0 > StateIndex ->
				Attr is Attr0 - 1
			;
	 			error("base relation indexed on aditi__state attribute")
			))),
		list__map(AdjustAttr, Attrs0, Attrs)	
	;
		error("magic_util__adjust_index: no aditi__state in base relation argument types")
	).

magic_util__restrict_nonlocals(NonLocals0, NonLocals) -->
	magic_info_get_proc_info(ProcInfo),
	{ proc_info_vartypes(ProcInfo, VarTypes) },
	{ set__to_sorted_list(NonLocals0, NonLocals1) },
	{ map__apply_to_list(NonLocals1, VarTypes, NonLocalTypes) },
	{ type_util__remove_aditi_state(NonLocalTypes,
		NonLocals1, NonLocals2) },
	{ set__sorted_list_to_set(NonLocals2, NonLocals) }.

magic_util__make_pred_name(PredInfo, ProcId, Prefix0, AddCount, Name) -->
	{ pred_info_get_is_pred_or_func(PredInfo, PredOrFunc) },
	{ pred_info_module(PredInfo, Module) },
	{ pred_info_name(PredInfo, Name0) },
	{ proc_id_to_int(ProcId, ProcInt) },
	{ string__int_to_string(ProcInt, ProcStr) },
	{ string__append_list([Prefix0, "_Mode_", ProcStr, "_Of"], Prefix) },

	( { AddCount = yes } ->
		magic_info_get_next_supp_id(Count)
	;
		% Note that we can't use a counter here because the names
		% can be exported to other modules.
		{ Count = 0 }
	),

	{ Line = 0 },
	{ make_pred_name_with_context(Module, Prefix, PredOrFunc, Name0,
		Line, Count, Name) }.

%-----------------------------------------------------------------------------%

magic_util__setup_call(PrevGoals, DBCall1, NonLocals, Goals) -->

	{ DBCall1 = db_call(MaybeAggInputs, CallGoal0,
			PredProcId0, Args, InputArgs, _, MaybeNegGoals) },

	%
	% Check whether this procedure was renamed
	% during the preprocessing pass.
	% 
	magic_info_get_pred_map(PredMap),
	{ map__search(PredMap, PredProcId0, PredProcId1) ->
		PredProcId = PredProcId1
	;
		PredProcId = PredProcId0
	},

	( { MaybeAggInputs = yes(AggInputs0) } ->
		% The preprocessing pass ensures that the closures
		% for the aggregate are right next to the call.
		% There should be three - one for the query, one to
		% compute the initial accumulator and one to update
		% the accumulator.
		list__map_foldl(magic_util__setup_aggregate_input,
			AggInputs0, AggInputs1),
		{ list__condense(AggInputs1, AggInputs) },

		{ CallGoal0 = _ - CallGoalInfo },
		{ goal_info_get_context(CallGoalInfo, Context) },
		magic_util__maybe_create_supp_call(PrevGoals, NonLocals, [],
			Context, SuppCall),

		{ BeforeGoals = [SuppCall | AggInputs] },
		{ Tests = [] },
		{ CallGoal = CallGoal0 }
	;
		{ PredProcId = proc(PredId, ProcId) },
		magic_info_get_module_info(ModuleInfo0),
		( { hlds_pred__is_base_relation(ModuleInfo0, PredId) } ->
			{ CallGoal0 = _ - CallGoalInfo0 },
			{ goal_info_get_context(CallGoalInfo0, Context) },
			magic_util__maybe_create_supp_call(PrevGoals,
				NonLocals, [], Context, SuppCall),
			{ BeforeGoals = [SuppCall] },

			% Convert input args to outputs, and test that
			% the input matches the output.
			magic_info_get_module_info(ModuleInfo),
			{ module_info_pred_proc_info(ModuleInfo, PredProcId,
				CalledPredInfo, CalledProcInfo) },
			{ pred_info_module(CalledPredInfo, PredModule) },
			{ pred_info_name(CalledPredInfo, PredName) },
			{ Name = qualified(PredModule, PredName) },
			{ proc_info_argmodes(CalledProcInfo, ArgModes) },
			magic_info_get_proc_info(ProcInfo0),
			{ magic_util__create_input_test_unifications(
				ModuleInfo, Args, InputArgs, ArgModes,
				NewArgs, [], Tests, CallGoalInfo0,
				CallGoalInfo1, ProcInfo0, ProcInfo) },
			magic_info_set_proc_info(ProcInfo),
			{ goal_info_get_nonlocals(CallGoalInfo1,
				CallNonLocals1) },
			magic_util__restrict_nonlocals(CallNonLocals1,
				CallNonLocals),
			{ goal_info_set_nonlocals(CallGoalInfo1, CallNonLocals,
				CallGoalInfo) },
			{ CallGoal = call(PredId, ProcId, NewArgs,
				not_builtin, no, Name) - CallGoalInfo }
		;
			% Transform away the input arguments. 
			magic_util__handle_input_args(PredProcId0, PredProcId,
				PrevGoals, NonLocals, Args, InputArgs,
				BeforeGoals, CallGoal0, CallGoal, Tests)
		)
	),

	( { MaybeNegGoals = yes(NegAfterGoals - NegGoalInfo) } ->
		{ list__append([CallGoal | Tests], NegAfterGoals, NegGoals) },

		%
		% Compute a goal info for the conjunction
		% inside the negation.
		%
		{ goal_info_get_nonlocals(NegGoalInfo, NegNonLocals0) },
		{ goal_list_nonlocals(NegGoals, InnerNonLocals0) },  
		{ set__intersect(NegNonLocals0, InnerNonLocals0,
			InnerNonLocals1) },
		magic_util__restrict_nonlocals(InnerNonLocals1,
			InnerNonLocals),
		{ goal_list_instmap_delta(NegGoals, InnerDelta0) },
		{ instmap_delta_restrict(InnerDelta0,
			InnerNonLocals, InnerDelta) },
		{ goal_list_determinism(NegGoals, InnerDet) },
		{ goal_info_init(InnerNonLocals, InnerDelta,
			InnerDet, InnerInfo) },

		{ conj_list_to_goal(NegGoals, InnerInfo, InnerConj) },
		{ list__append(BeforeGoals, [not(InnerConj) - NegGoalInfo],
			Goals) }
	;
		{ list__append(BeforeGoals, [CallGoal | Tests], Goals) }
	).

	% Construct the input for the query for an aggregate.
	% XXX we should check that the input query of an aggregate
	% is an Aditi relation, not a top-down Mercury predicate.
:- pred magic_util__setup_aggregate_input(hlds_goal::in, list(hlds_goal)::out,
		magic_info::in, magic_info::out) is det.

magic_util__setup_aggregate_input(Closure, InputAndClosure) -->

	magic_info_get_module_info(ModuleInfo0),
	magic_info_get_pred_map(PredMap),
	(
		{ Closure = unify(_, _, UniMode, Uni0, Context) - Info },
		{ Uni0 = construct(Var, ConsId0, _, Modes, _, _, _) },
		{ ConsId0 = pred_const(PredId0, ProcId0, Method) }
	->
		%
		% Replace the pred_proc_id of the procedure being aggregated
		% over with its Aditi version.
		%
		{ map__search(PredMap, proc(PredId0, ProcId0), PredProcId) ->
			PredProcId = proc(PredId, ProcId),
			ConsId = pred_const(PredId, ProcId, Method)
		;
			PredId = PredId0,
			ProcId = ProcId0,
			ConsId = ConsId0
		},

		( { hlds_pred__is_derived_relation(ModuleInfo0, PredId) } ->

			%
			% Create the input relation for the aggregate query.
			% This is just `true', since we don't allow curried
			% arguments (except for aditi:states).
			%
			magic_info_get_magic_proc_info(MagicProcInfo),
			{ map__lookup(MagicProcInfo, proc(PredId, ProcId),
				CallProcInfo) },
			{ CallProcInfo = magic_proc_info(_, MagicInputs,
						_, _, _) },

			{ true_goal(InputGoal) },
			magic_util__create_input_closures(MagicInputs, [], [],
				InputGoal, CallProcInfo, 1,
				InputGoals, InputVars)
		;
			% Base relation. It could actually be another
			% aggregate, but if aggregate becomes a new goal
			% type we won't be able to handle that, in the
			% same way that call(call(X)) doesn't work. 
			{ InputGoals = [] },
			{ InputVars = [] }
		),

		% Update the unify_rhs.
		magic_info_get_module_info(ModuleInfo),
		{ module_info_pred_info(ModuleInfo, PredId, CallPredInfo) },
		{ pred_info_module(CallPredInfo, PredModule) },
		{ pred_info_name(CallPredInfo, PredName) },
		{ list__length(InputVars, Arity) },
		{ Rhs = functor(cons(qualified(PredModule, PredName),
				Arity), InputVars) },

		{ RLExprnId = no },
		{ Uni = construct(Var, ConsId, InputVars, Modes,
			construct_dynamically, cell_is_unique, RLExprnId) },
		{ Goal1 = unify(Var, Rhs, UniMode, Uni, Context) - Info },

		{ list__append(InputGoals, [Goal1], InputAndClosure) }
	;
		{ error(
	"magic_util__setup_aggregate_input: non-closure input to aggregate") }
	).

%-----------------------------------------------------------------------------%

	% Transform away the input arguments to a derived relation.
:- pred magic_util__handle_input_args(pred_proc_id::in, pred_proc_id::in,
	list(hlds_goal)::in, set(prog_var)::in, list(prog_var)::in,
	list(prog_var)::in, list(hlds_goal)::out, hlds_goal::in,
	hlds_goal::out, list(hlds_goal)::out,
	magic_info::in, magic_info::out) is det.

magic_util__handle_input_args(PredProcId0, PredProcId, PrevGoals, NonLocals,
		Args, InputArgs, InputGoals, _ - GoalInfo0,
		CallGoal, Tests) -->
				
	magic_info_get_magic_proc_info(MagicProcInfo),
	{ map__lookup(MagicProcInfo, PredProcId, CallProcInfo) },
	{ CallProcInfo = magic_proc_info(OldArgModes, MagicInputs, _, _, _) },

	magic_info_get_module_info(ModuleInfo0),
	{ partition_args(ModuleInfo0, OldArgModes,
		OldArgModes, InputArgModes, _) },

	{ goal_info_get_context(GoalInfo0, Context) },
	magic_util__maybe_create_supp_call(PrevGoals, NonLocals, InputArgs,
		Context, SuppCall),

	% Convert input args to outputs, and test that
	% the input matches the output.
	magic_info_get_module_info(ModuleInfo1),
	magic_info_get_proc_info(ProcInfo0),
	{ magic_util__create_input_test_unifications(ModuleInfo1,
		Args, InputArgs, OldArgModes, NewOutputArgs, [], Tests,
		GoalInfo0, GoalInfo1, ProcInfo0, ProcInfo) },
	magic_info_set_proc_info(ProcInfo),

	% All database predicates are considered nondet after this.
	{ goal_info_set_determinism(GoalInfo1, 
		nondet, GoalInfo) },

	magic_info_get_scc(SCC),
	( { list__member(PredProcId0, SCC) } ->
		magic_info_get_magic_vars(MagicVars),
		{ list__append(MagicVars, InputArgs, AllMagicVars) },
		magic_util__add_to_magic_predicate(PredProcId,
			SuppCall, AllMagicVars),
		magic_info_get_magic_vars(MagicInputArgs),
		{ list__append(MagicInputArgs, NewOutputArgs, AllArgs) },
		{ InputGoals0 = [] }
	;
		magic_util__create_input_closures(MagicInputs,
			InputArgs, InputArgModes, SuppCall,
			CallProcInfo, 1, InputGoals0, InputVars),
		{ list__append(InputVars, NewOutputArgs, AllArgs) }
	),

	{ InputGoals = [SuppCall | InputGoals0] },

	magic_info_get_module_info(ModuleInfo),
	{ PredProcId = proc(PredId, ProcId) },
	{ module_info_pred_info(ModuleInfo, PredId, PredInfo) },
	{ pred_info_module(PredInfo, PredModule) },
	{ pred_info_name(PredInfo, PredName) },
	{ CallGoal = call(PredId, ProcId, AllArgs, not_builtin, no,
			qualified(PredModule, PredName)) - GoalInfo }.

magic_util__create_input_test_unifications(_, [], _, [_|_],
		_, _, _, _, _, _, _) :-
	error("magic_util__create_input_test_unifications").
magic_util__create_input_test_unifications(_, [_|_], _, [],
		_, _, _, _, _, _, _) :-
	error("magic_util__create_input_test_unifications").
magic_util__create_input_test_unifications(_, [], _, [], [], Tests, Tests,
		CallInfo, CallInfo, ProcInfo, ProcInfo).
magic_util__create_input_test_unifications(ModuleInfo, [Var | Vars], InputArgs,
		[Mode | Modes], [OutputVar | OutputVars], Tests0, Tests,
		CallInfo0, CallInfo, ProcInfo0, ProcInfo) :-
	( list__member(Var, InputArgs) ->
		magic_util__create_input_test_unification(ModuleInfo,
			Var, Mode, OutputVar, Test, CallInfo0, CallInfo1,
			ProcInfo0, ProcInfo1),
		Tests1 = [Test | Tests0]
	;
		ProcInfo1 = ProcInfo0,
		OutputVar = Var,
		CallInfo1 = CallInfo0,
		Tests1 = Tests0
	),	
	magic_util__create_input_test_unifications(ModuleInfo, Vars, InputArgs,
		Modes, OutputVars, Tests1, Tests, CallInfo1, CallInfo,
		ProcInfo1, ProcInfo).

:- pred magic_util__create_input_test_unification(module_info::in,
		prog_var::in, (mode)::in, prog_var::out,
		hlds_goal::out, hlds_goal_info::in,
		hlds_goal_info::out, proc_info::in, proc_info::out) is det.

magic_util__create_input_test_unification(ModuleInfo, Var, Mode, OutputVar,
		Test, CallInfo0, CallInfo, ProcInfo0, ProcInfo) :-
	mode_get_insts(ModuleInfo, Mode, _, FinalInst), 
	proc_info_varset(ProcInfo0, VarSet0),
	varset__new_var(VarSet0, OutputVar, VarSet),
	proc_info_vartypes(ProcInfo0, VarTypes0),
	map__lookup(VarTypes0, Var, VarType),
	map__det_insert(VarTypes0, OutputVar, VarType, VarTypes),
	proc_info_set_varset(ProcInfo0, VarSet, ProcInfo1),
	proc_info_set_vartypes(ProcInfo1, VarTypes, ProcInfo),

	set__list_to_set([Var, OutputVar], NonLocals),
	instmap_delta_init_reachable(InstMapDelta),
	goal_info_init(NonLocals, InstMapDelta, semidet, GoalInfo),
	( type_is_atomic(VarType, ModuleInfo) ->
		%
		% The type is a builtin, so create a simple_test unification.
		%
		Unification = simple_test(Var, OutputVar),
		UnifyMode = ((FinalInst -> FinalInst) 
			- (FinalInst -> FinalInst)),
		Test = unify(Var, var(OutputVar), UnifyMode,
			Unification, unify_context(explicit, [])) - GoalInfo
	; type_to_ctor_and_args(VarType, _TypeCtor, _ArgTypes) ->
		% XXX for now we pretend that the unification is
		% a simple test, since otherwise we would have to
		% go through the rigmarole of creating type_info variables
		% (and then ignoring them in code generation).
		Unification = simple_test(Var, OutputVar),
		UnifyMode = ((FinalInst -> FinalInst) 
			- (FinalInst -> FinalInst)),
		Test = unify(Var, var(OutputVar), UnifyMode,
			Unification, unify_context(explicit, [])) - GoalInfo

		/*
		% 
		% The type is non-builtin, so look up the unification 
		% procedure for the type.
		%
		module_info_get_special_pred_map(ModuleInfo,
			SpecialPredMap),
		map__lookup(SpecialPredMap, unify - TypeCtor, UniPredId),

		% It had better be an in-in unification, since Aditi
		% relations cannot have non-ground arguments. This is 
		% checked elsewhere.
		% XXX __Unify__/2 needs to be special cased in rl_exprn.m 
		% because we don't add the type_info arguments.

		hlds_pred__in_in_unification_proc_id(UniProcId),
		SymName = unqualified("__Unify__"),
		ArgVars = [Var, OutputVar],
		Test = call(UniPredId, UniProcId, ArgVars, not_builtin,
			no, SymName) - GoalInfo
		*/
	;
		error("magic_util__create_input_test_unifications: \
			type_to_ctor_and_args failed")
	),
	goal_info_get_nonlocals(CallInfo0, CallNonLocals0),
	set__delete(CallNonLocals0, Var, CallNonLocals1),
	set__insert(CallNonLocals1, OutputVar, CallNonLocals),
	goal_info_get_instmap_delta(CallInfo0, CallDelta0),
	instmap_delta_insert(CallDelta0, OutputVar, FinalInst, CallDelta),
	goal_info_set_nonlocals(CallInfo0, CallNonLocals, CallInfo1),
	goal_info_set_instmap_delta(CallInfo1, CallDelta, CallInfo).

%-----------------------------------------------------------------------------%

	% Create the magic input closures for a call to a lower sub-module.
:- pred magic_util__create_input_closures(list(prog_var)::in,
		list(prog_var)::in, list(mode)::in, hlds_goal::in,
		magic_proc_info::in, int::in, list(hlds_goal)::out,
		list(prog_var)::out, magic_info::in, magic_info::out) is det.

magic_util__create_input_closures([], _, _, _, _, _, [], []) --> [].
magic_util__create_input_closures([_ | MagicVars], InputArgs, 
		InputArgModes, SuppCall, ThisProcInfo, CurrVar,
		[InputGoal | InputGoals], [ClosureVar | ClosureVars]) -->
	{ ThisProcInfo = magic_proc_info(_OldArgModes, _MagicInputs,
		MagicTypes, MagicModes, MaybeIndex) },
	magic_info_get_proc_info(ProcInfo0),

	%
	% Create a new variable to hold the input.
	%
	{ magic_util__get_input_var(MagicTypes, CurrVar, ClosureVar, ArgTypes, 
		ProcInfo0, ProcInfo1) },

	( { MaybeIndex = yes(CurrVar) } ->
		%
		% This argument is the magic input for the call we are
		% processing now. Create the input closure by projecting
		% the previous database call onto the input arguments.
		%
		( { SuppCall = conj([]) - _ } ->
			{ LambdaGoal = SuppCall },
			{ LambdaVars = [] },
			{ LambdaInputs = [] },
			{ ProcInfo = ProcInfo1 }
		;
			magic_util__project_supp_call(SuppCall, InputArgs,
				ProcInfo1, ProcInfo, LambdaInputs, 
				LambdaVars, LambdaGoal)
		)
	;	
		%
		% There is no input for this member of the lower sub-module
		% since it is not being directly called, so create an empty
		% input relation.
		%
		{ proc_info_create_vars_from_types(ProcInfo1, ArgTypes, 
			LambdaVars, ProcInfo) },
		{ fail_goal(LambdaGoal) },
		{ LambdaInputs = [] }
	),

	magic_info_set_proc_info(ProcInfo),

	{ list__index1_det(MagicModes, CurrVar, ClosureVarMode) },
	magic_util__create_closure(CurrVar, ClosureVar, ClosureVarMode,
		LambdaGoal, LambdaInputs, LambdaVars, InputGoal),

	{ NextIndex is CurrVar + 1 },
	magic_util__create_input_closures(MagicVars, InputArgs,
		InputArgModes, SuppCall, ThisProcInfo, NextIndex, 
		InputGoals, ClosureVars).

%-----------------------------------------------------------------------------%

	% Create a variable to hold an input closure for a lower sub-module
	% call, returning the argument types of the closure.
:- pred magic_util__get_input_var(list(type)::in, int::in, prog_var::out, 
		list(type)::out, proc_info::in, proc_info::out) is det.

magic_util__get_input_var(MagicTypes, CurrVar, InputVar, ArgTypes, 
		ProcInfo0, ProcInfo) :-
	list__index1_det(MagicTypes, CurrVar, MagicType),
	(
		type_is_higher_order(MagicType, predicate,
			(aditi_bottom_up), ArgTypes1)
	->
		ArgTypes = ArgTypes1,
		construct_higher_order_type(predicate, (aditi_bottom_up),
			ArgTypes, ClosureType),
		proc_info_create_var_from_type(ProcInfo0, ClosureType, no,
			InputVar, ProcInfo)
	;
		error("magic_util__get_input_var")
	).

magic_util__create_closure(_CurrVar, InputVar, InputMode, LambdaGoal, 
		LambdaInputs, LambdaVars, InputGoal) -->

	%
	% Create a new predicate to hold the projecting goal,
	% unless the arguments match so no projection is needed.
	%
	(
		{ LambdaGoal = call(_, _, CallArgs, _, _, _) - _ },
		{ list__append(LambdaInputs, LambdaVars, CallArgs) }
	->
		% No projection is needed.
		{ SuppCall = LambdaGoal }
	;
		{ term__context_init(Context) },
		{ goal_to_conj_list(LambdaGoal, LambdaGoalList) },

		%
		% The projecting goal must be generated inline.
		% Otherwise there could be problems with transformed
		% code such as:
		% 
		% q(InP, X, Y, Z) :-
		% 	magic_p(InP, X, Y),
		% 	InR = q_supp1(InP),
		%	r(InR, A, Z),
		% 	Y == A.
		% q_supp1(InP, Y) :-
		% 	magic_p(InP, _X, Y).	
		%
		% 'r/3' is defined in a lower SCC.
		%
		% rl_gen could produce the recursive part of this SCC as:
		% 
		% toplabel:
		% if (diffs empty) goto bottomlabel:
		% 	evaluate magic_p;
		%	evaluate q;
		%	evaluate q_supp1;
		% goto toplabel;
		% bottomlabel:
		%
		% In this case, if `q_supp1/2' was not generated inline,
		% the input relation for the call to `r/3' in `q/4'
		% would use the value of `magic_p/3' from the previous
		% iteration, but the join of `magic_p/3' and the result
		% of `r/3' uses the value of `magic_p/3' from the
		% current iteration.
		%
		% The generate_inline marker forces rl_gen.m evaluate 
		% `q_supp1/2' in the correct location so that it uses
		% the correct version of `magic_p/3' as input to `r/3'.
		% 
		magic_util__create_supp_call(LambdaGoalList, LambdaInputs,
			LambdaVars, Context,
			[aditi_no_memo, naive, generate_inline], SuppCall)
	),

	magic_info_get_module_info(ModuleInfo),
	(
		{ SuppCall = call(SuppPredId, SuppProcId, _, _, _, _) - _ },
		{ mode_get_insts(ModuleInfo, InputMode, Inst, _) },
		{ Inst = ground(_, higher_order(PredInstInfo)) }
	->
		% Find the mode of the unification.
		{ PredInstInfo = pred_inst_info(_, LambdaModes, _) },
		{ LambdaInst = ground(shared, 
			higher_order(pred_inst_info(predicate, LambdaModes,
				nondet))) },
		{ UnifyMode = (free -> LambdaInst) -
				(LambdaInst -> LambdaInst) },
		{ mode_util__modes_to_uni_modes(LambdaModes, LambdaModes,
			ModuleInfo, UniModes) },

		% Construct the unify_rhs.
		{ module_info_pred_info(ModuleInfo, SuppPredId, PredInfo) },
		{ pred_info_module(PredInfo, SuppModule) },
		{ pred_info_name(PredInfo, SuppName) },
		{ list__length(LambdaInputs, SuppArity) },
		{ Rhs = functor(cons(qualified(SuppModule, SuppName), 
				SuppArity), LambdaInputs) },

		{ RLExprnId = no },
		{ Unify = construct(InputVar, 
			pred_const(SuppPredId, SuppProcId, (aditi_bottom_up)), 
			LambdaInputs, UniModes, construct_dynamically,
			cell_is_unique, RLExprnId) },
		{ UnifyContext = unify_context(explicit, []) },

		% Construct a goal_info.
		{ set__list_to_set([InputVar | LambdaInputs], NonLocals) },
		{ instmap_delta_init_reachable(InstMapDelta0) },
		{ instmap_delta_insert(InstMapDelta0, InputVar, LambdaInst, 
			InstMapDelta) },
		{ goal_info_init(NonLocals, InstMapDelta, det, GoalInfo) },

		{ InputGoal = unify(InputVar, Rhs, UnifyMode, 
				Unify, UnifyContext) - GoalInfo }
	;
		{ error("magic_util__create_closure") }
	).

%-----------------------------------------------------------------------------%

	% Project the supplementary predicate call onto the input
	% arguments of the following call.
:- pred magic_util__project_supp_call(hlds_goal::in, list(prog_var)::in, 
	proc_info::in, proc_info::out, list(prog_var)::out,
	list(prog_var)::out, hlds_goal::out,
	magic_info::in, magic_info::out) is det.

magic_util__project_supp_call(SuppCall, UnrenamedInputVars,
		ProcInfo0, ProcInfo, SuppInputArgs, LambdaVars, LambdaGoal) -->
	(
		{ SuppCall = call(SuppPredId1, SuppProcId1,
			SuppArgs1, _, _, _) - _ }
	->
		{ SuppArgs = SuppArgs1 },
		magic_info_get_module_info(ModuleInfo),
		{ module_info_pred_proc_info(ModuleInfo, SuppPredId1,
			SuppProcId1, _, SuppProcInfo) },
		{ proc_info_argmodes(SuppProcInfo, SuppArgModes) },
		{ partition_args(ModuleInfo, SuppArgModes, 
			SuppArgs, SuppInputArgs, SuppOutputArgs) }
	;
		{ error("magic_util__project_supp_call: not a call") }
	),

		% Rename the outputs of the supp call, 
		% but not the magic input relations.
	{ proc_info_vartypes(ProcInfo0, VarTypes0) },
	{ map__apply_to_list(SuppOutputArgs, VarTypes0, SuppOutputArgTypes) },
	{ proc_info_create_vars_from_types(ProcInfo0, SuppOutputArgTypes, 
		NewArgs, ProcInfo) },
	{ map__from_corresponding_lists(SuppOutputArgs, NewArgs, Subn) },
	{ map__apply_to_list(UnrenamedInputVars, Subn, LambdaVars) },
	{ goal_util__rename_vars_in_goal(SuppCall, Subn, LambdaGoal0) },
	{ LambdaGoal0 = LambdaExpr - LambdaInfo0 },

	{ list__append(SuppInputArgs, LambdaVars, LambdaNonLocals0) },
	{ set__list_to_set(LambdaNonLocals0, LambdaNonLocals) },
	{ goal_info_set_nonlocals(LambdaInfo0, LambdaNonLocals, LambdaInfo) },
	{ LambdaGoal = LambdaExpr - LambdaInfo }.

%-----------------------------------------------------------------------------%

magic_util__add_to_magic_predicate(PredProcId, Rule, RuleArgs) -->
	magic_info_get_magic_map(MagicMap),
	{ map__lookup(MagicMap, PredProcId, MagicPred) },
	magic_info_get_module_info(ModuleInfo0),
	{ MagicPred = proc(MagicPredId, MagicProcId) },
	{ module_info_preds(ModuleInfo0, Preds0) },
	{ map__lookup(Preds0, MagicPredId, MagicPredInfo0) },
	{ pred_info_procedures(MagicPredInfo0, MagicProcs0) },
	{ map__lookup(MagicProcs0, MagicProcId, MagicProcInfo0) },
	{ proc_info_goal(MagicProcInfo0, MagicGoal0) },

	{ proc_info_varset(MagicProcInfo0, MagicVarSet0) },
	{ proc_info_vartypes(MagicProcInfo0, MagicVarTypes0) },
	{ proc_info_headvars(MagicProcInfo0, MagicProcHeadVars) },

	magic_info_get_proc_info(ProcInfo),
	{ proc_info_vartypes(ProcInfo, VarTypes) },
	{ proc_info_varset(ProcInfo, VarSet) },
		
	%
	% Rename the variables in the supp predicate call.
	%
	{ map__from_corresponding_lists(RuleArgs, MagicProcHeadVars, Subn0) },
	{ goal_util__goal_vars(Rule, RuleVars0) },
	{ set__to_sorted_list(RuleVars0, RuleVars) },
	{ goal_util__create_variables(RuleVars, MagicVarSet0, MagicVarTypes0,
		Subn0, VarTypes, VarSet, MagicVarSet, MagicVarTypes, Subn) },
	{ Rule = RuleExpr - RuleInfo0 },
	{ set__list_to_set(RuleArgs, RuleArgSet) },
	{ goal_info_set_nonlocals(RuleInfo0, RuleArgSet, RuleInfo) },
	{ goal_util__must_rename_vars_in_goal(RuleExpr - RuleInfo,
		Subn, ExtraDisjunct) },

	%
	% Add in the new disjunct.
	%
	{ goal_to_disj_list(MagicGoal0, MagicDisjList0) },
	{ MagicGoal0 = _ - GoalInfo },	% near enough.
	{ disj_list_to_goal([ExtraDisjunct | MagicDisjList0], 
		GoalInfo, MagicGoal) },
	{ proc_info_set_vartypes(MagicProcInfo0,
		MagicVarTypes, MagicProcInfo1) },
	{ proc_info_set_varset(MagicProcInfo1,
		MagicVarSet, MagicProcInfo2) },
	{ proc_info_set_goal(MagicProcInfo2, MagicGoal, MagicProcInfo) },
	{ map__det_update(MagicProcs0, MagicProcId, MagicProcInfo,
		MagicProcs) },
	{ pred_info_set_procedures(MagicPredInfo0, 
		MagicProcs, MagicPredInfo) },
	{ map__det_update(Preds0, MagicPredId, MagicPredInfo, Preds) },
	{ module_info_set_preds(ModuleInfo0, Preds, ModuleInfo) },
	magic_info_set_module_info(ModuleInfo).

%-----------------------------------------------------------------------------%

magic_util__magic_call_info(MagicPredId, MagicProcId,
		qualified(PredModule, PredName), InputRels,
		InputArgs, MagicOutputModes) -->
	magic_info_get_curr_pred_proc_id(PredProcId),
	magic_info_get_magic_proc_info(MagicProcInfo),
	{ map__lookup(MagicProcInfo, PredProcId, ThisProcInfo) },
	{ ThisProcInfo = magic_proc_info(OldArgModes, _, _, _, _) },
	magic_info_get_module_info(ModuleInfo),

	%
	% Get the arguments of the magic call.
	%
	magic_info_get_proc_info(ProcInfo0),
	{ proc_info_headvars(ProcInfo0, HeadVars) },
	{ proc_info_argmodes(ProcInfo0, ArgModes) },
	{ partition_args(ModuleInfo, ArgModes, HeadVars, _, OldHeadVars) },
	{ partition_args(ModuleInfo, OldArgModes, OldHeadVars, InputArgs, _) },
	{ partition_args(ModuleInfo, OldArgModes, 
		OldArgModes, InputArgModes, _) },
	{ list__map(magic_util__mode_to_output_mode(ModuleInfo),
		InputArgModes, MagicOutputModes) },

	magic_info_get_magic_vars(InputRels),

	magic_info_get_magic_map(MagicMap),
	{ map__lookup(MagicMap, PredProcId, proc(MagicPredId, MagicProcId)) },
	{ module_info_pred_info(ModuleInfo, MagicPredId, MagicPredInfo) },
	{ pred_info_name(MagicPredInfo, PredName) },
	{ pred_info_module(MagicPredInfo, PredModule) }.

%-----------------------------------------------------------------------------%

	% Create the supplementary predicate for a part of a goal that
	% has been transformed. If the goal is already a single call
	% this is unnecessary.
:- pred magic_util__maybe_create_supp_call(list(hlds_goal)::in,
		set(prog_var)::in, list(prog_var)::in, term__context::in,
		hlds_goal::out, magic_info::in, magic_info::out) is det.

magic_util__maybe_create_supp_call(PrevGoals, NonLocals, InputArgs,
		Context, SuppCall) -->
	( 
		{ PrevGoals = [PrevGoal] },
		{ PrevGoal = call(_, _, _, _, _, _) - _ }
	->
		{ SuppCall = PrevGoal }
	;
		magic_info_get_magic_vars(MagicVars),
		{ magic_util__order_supp_call_outputs(PrevGoals, MagicVars,
			NonLocals, InputArgs, SuppOutputArgs) },
		magic_util__create_supp_call(PrevGoals, MagicVars,
			SuppOutputArgs, Context, [], SuppCall)
	).

	% If the supplementary call is to be used as input to
	% another call, attempt to get the arguments in the right order 
	% to avoid an unnecessary projection. If this is not
	% possible, choose any order. If there are duplicates in the
	% call input list, a projection is unavoidable.
:- pred magic_util__order_supp_call_outputs(list(hlds_goal)::in,
		list(prog_var)::in, set(prog_var)::in,
		list(prog_var)::in, list(prog_var)::out) is det.

magic_util__order_supp_call_outputs(Goals, MagicVars, NonLocals,
		ArgsInOrder, Args) :-
	goal_list_nonlocals(Goals, SuppNonLocals),
	set__intersect(SuppNonLocals, NonLocals, SuppArgSet0),
	set__delete_list(SuppArgSet0, MagicVars, SuppArgSet1),
	(
		\+ (
			set__member(Arg, SuppArgSet1),
			\+ list__member(Arg, ArgsInOrder)
		)
	->
		Args = ArgsInOrder
	;	
		set__to_sorted_list(SuppArgSet1, Args)
	).

:- pred magic_util__create_supp_call(list(hlds_goal)::in, list(prog_var)::in,
	list(prog_var)::in, prog_context::in, list(marker)::in,
	hlds_goal::out, magic_info::in, magic_info::out) is det.

magic_util__create_supp_call(Goals, MagicVars, SuppOutputArgs, Context,
		ExtraMarkers, SuppCall) -->

	{ list__append(MagicVars, SuppOutputArgs, SuppArgs) },

	%
	% Compute a goal_info for the call.
	%
	{ goal_list_instmap_delta(Goals, Delta0) },
	{ set__list_to_set(SuppArgs, SuppArgSet) },
	{ instmap_delta_restrict(Delta0, SuppArgSet, Delta) },
	{ goal_info_init(SuppArgSet, Delta, nondet, GoalInfo) },

	%
	% Verify that the supplementary predicate does not have any partially
	% instantiated or higher-order arguments other than the input closures.
	%
	magic_info_get_proc_info(ProcInfo),
	{ proc_info_vartypes(ProcInfo, VarTypes) },
	{ map__apply_to_list(SuppOutputArgs, VarTypes, SuppOutputTypes) },  

	{ GetSuppMode = 
	    lambda([Var::in, Mode::out] is det, (
		( instmap_delta_search_var(Delta, Var, NewInst) ->
			Mode = (free -> NewInst)
		;
			% This is a lie, but we're only using this to check
			% that the output arguments aren't partially
			% instantiated. Any arguments that are partially
			% instantiated in the initial instmap for the
			% procedure will be reported there.
			Mode = (ground(shared, none) -> ground(shared, none))
		)
	    )) },
	{ list__map(GetSuppMode, SuppOutputArgs, SuppOutputModes) },
	magic_util__check_args(SuppOutputArgs, SuppOutputModes,
		SuppOutputTypes, Context, var_name),

	%
	% Fill in the fields of the new predicate.
	%
	magic_info_get_pred_info(PredInfo),
	magic_info_get_curr_pred_proc_id(proc(_, ProcId)),
	magic_util__make_pred_name(PredInfo, ProcId, "Supp_Proc_For",
		yes, NewName),

	magic_info_get_module_info(ModuleInfo0),
	{ proc_info_get_initial_instmap(ProcInfo, ModuleInfo0, InstMap) },
	{ proc_info_inst_varset(ProcInfo, InstVarSet) },
	{ pred_info_get_aditi_owner(PredInfo, Owner) },
	{ pred_info_get_markers(PredInfo, Markers0) },
	{ AddMarkers = lambda([Marker::in, Ms0::in, Ms::out] is det,
			add_marker(Ms0, Marker, Ms)
		) },
	{ list__foldl(AddMarkers, ExtraMarkers, Markers0, Markers) },

	% Add the predicate to the predicate table.
	{ conj_list_to_goal(Goals, GoalInfo, SuppGoal) },
	{ varset__init(TVarSet) },
	{ ClassConstraints = constraints([], []) },
	{ map__init(TVarMap) },
	{ map__init(TCVarMap) },
	{ proc_info_varset(ProcInfo, VarSet) },
	{ unqualify_name(NewName, NewPredName) },
	{ hlds_pred__define_new_pred(SuppGoal, SuppCall, SuppArgs, ExtraArgs,
		InstMap, NewPredName, TVarSet, VarTypes, ClassConstraints,
		TVarMap, TCVarMap, VarSet, InstVarSet, Markers, Owner,
		address_is_not_taken, ModuleInfo0, ModuleInfo, _) },
	{ ExtraArgs = [] ->
		true
	;
		error("magic_util__create_supp_call: typeinfo arguments")
	},
	magic_info_set_module_info(ModuleInfo).

%-----------------------------------------------------------------------------%

magic_util__mode_to_output_mode(ModuleInfo, Mode, OutputMode) :-
	mode_get_insts(ModuleInfo, Mode, _, FinalInst),
	OutputMode = (free -> FinalInst).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

magic_util__check_args(Vars, Modes, Types, Context, IdType) -->
	(
		magic_util__check_args_2(Vars, Modes, Types, Context,
			1, IdType, no_rtti, MaybeRtti)
	->
		(
			{ MaybeRtti = no_rtti }
		;
			{ MaybeRtti = found_rtti(RttiArg) },
			magic_info_get_error_pred_proc_id(PredProcId),
			magic_info_get_errors(Errors0),
			{ Error = nonspecific_polymorphism(PredProcId, RttiArg)
					- Context },
			{ set__insert(Errors0, Error, Errors) },
			magic_info_set_errors(Errors)
		;
			{ MaybeRtti = found_polymorphic }
		)
	;
		{ error("magic_util__check_args") }
	).

:- pred magic_util__check_args_2(list(prog_var)::in, list(mode)::in,
	list(type)::in, term__context::in, int::in,
	magic_arg_id_type::in, rtti_arg_state::in, rtti_arg_state::out,
	magic_info::in, magic_info::out) is semidet.

magic_util__check_args_2([], [], [], _, _, _, Rtti, Rtti) --> [].
magic_util__check_args_2([Var | Vars], [ArgMode | ArgModes],
		[ArgType | ArgTypes], Context, ArgNo,
		ArgIdType, Rtti0, Rtti) -->
	magic_info_get_error_vars(ErrorVars0),
	( { set__member(Var, ErrorVars0) } ->
		{ NextArgNo = ArgNo + 1 },
		{ Rtti1 = Rtti0 }
	;
		(
			{ ArgIdType = arg_number },
			{ ArgId = arg_number(ArgNo) }
		;
			{ ArgIdType = var_name },
			magic_info_get_proc_info(ProcInfo),
			{ proc_info_varset(ProcInfo, VarSet) },
			{ varset__lookup_name(VarSet, Var, VarName) },
			{ ArgId = var_name(VarName) }
		),

		magic_info_get_error_pred_proc_id(PredProcId),
		magic_info_get_module_info(ModuleInfo),
		( { type_is_aditi_state(ArgType) } ->
			(
				{ \+ mode_is_input(ModuleInfo, ArgMode) },

				% The second `aditi__state' of the closure
				% passed to `aditi_bulk_modify' has mode
				% `unused'.
				{ \+ mode_is_unused(ModuleInfo, ArgMode) }
			->
				% aditi__states must not be output.
				{ StateError =
					[argument_error(output_aditi_state,
						ArgId, PredProcId) - Context] }
			;
				{ StateError = [] }
			)
		;
			{ StateError = [] }
		),

			% Check that the argument types are legal.
		magic_util__check_type(ArgType, ErrorTypes, MaybeRtti),
		{ set__to_sorted_list(ErrorTypes, ErrorTypeList0) },
		
			% Check that partially instantiated modes are not used.
		{ mode_get_insts(ModuleInfo, ArgMode, Inst1, Inst2) },
		(
			{ inst_is_free(ModuleInfo, Inst1)
			; inst_is_ground(ModuleInfo, Inst1)
			},
			{ inst_is_free(ModuleInfo, Inst2)
			; inst_is_ground(ModuleInfo, Inst2)
			}
		->
			{ ErrorTypeList = ErrorTypeList0 }
		;
			{ ErrorTypeList =
				[partially_instantiated | ErrorTypeList0] }
		),

		{ ConvertError = 
			lambda([ErrorType::in, MagicError::out] is det, (
				MagicError = argument_error(ErrorType,
						ArgId, PredProcId) - Context
			)) },
		{ list__map(ConvertError, ErrorTypeList, TypeErrors) },

		( { TypeErrors = [] } ->
			{ set__insert(ErrorVars0, Var, ErrorVars) },
			magic_info_set_error_vars(ErrorVars)
		;
			[]
		),
		magic_info_get_errors(Errors0),
		{ set__insert_list(Errors0, TypeErrors, Errors1) },
		{ set__insert_list(Errors1, StateError, Errors) },
		magic_info_set_errors(Errors),

		{ list__member(polymorphic, ErrorTypeList) ->
			NextArgNo = ArgNo + 1,
			Rtti1 = found_polymorphic
		; MaybeRtti = yes(RttiArg) ->
			% Don't count type-infos when working
			% out what number the current argument is.
			NextArgNo = ArgNo,
			update_rtti_arg_state(Rtti0, RttiArg, Rtti1)
		;
			NextArgNo = ArgNo + 1,
			Rtti1 = Rtti0
		}
	),
	magic_util__check_args_2(Vars, ArgModes, ArgTypes,
		Context, NextArgNo, ArgIdType, Rtti1, Rtti).

%-----------------------------------------------------------------------------%

:- type rtti_arg_state
	--->	no_rtti
	;	found_rtti(rtti_arg)
	;	found_polymorphic	% Report errors for the polymorphic
					% arguments, but don't report for the
					% typeinfos and typeclass infos
	.

:- pred update_rtti_arg_state(rtti_arg_state::in,
		rtti_arg::in, rtti_arg_state::out) is det.

update_rtti_arg_state(no_rtti, Arg, found_rtti(Arg)).
update_rtti_arg_state(found_rtti(Arg0), Arg1, found_rtti(Arg)) :-
	update_rtti_arg(Arg0, Arg1, Arg).
update_rtti_arg_state(found_polymorphic, _, found_polymorphic).

:- pred update_rtti_arg(rtti_arg::in, rtti_arg::in, rtti_arg::out) is det.

update_rtti_arg(both, _, both).
update_rtti_arg(type_info, type_info, type_info).
update_rtti_arg(type_info, typeclass_info, both).
update_rtti_arg(type_info, both, both).
update_rtti_arg(typeclass_info, typeclass_info, typeclass_info).
update_rtti_arg(typeclass_info, type_info, both).
update_rtti_arg(typeclass_info, both, both).

%-----------------------------------------------------------------------------%


	% Go over a type collecting any reasons why that type cannot
	% be an argument type of an Aditi relation.
:- pred magic_util__check_type((type)::in, set(argument_error)::out,
		maybe(rtti_arg)::out, magic_info::in, magic_info::out) is det.

magic_util__check_type(ArgType, Errors, MaybeRtti) -->

	% Polymorphic types are not allowed.
	% Errors for type_infos and typeclass_infos are only reported
	% if there are no other polymorphic arguments.
	( { polymorphism__type_info_type(ArgType, _) } ->
		{ set__init(Errors) },
		{ MaybeRtti = yes(type_info) }
	; { polymorphism__typeclass_info_class_constraint(ArgType, _) } ->
		{ set__init(Errors) },
		{ MaybeRtti = yes(typeclass_info) }
	;
		{ MaybeRtti = no },
		{ map__init(Subn) },
		{ set__init(Errors0) },
		{ term__is_ground(ArgType, Subn) ->
			Errors1 = Errors0
		;
			set__insert(Errors0, polymorphic, Errors1)
		},

		{ set__init(Parents) }, 
		magic_util__traverse_type(yes, Parents, ArgType,
			Errors1, Errors)
	).

:- pred magic_util__traverse_type(bool::in, set(type_ctor)::in, (type)::in, 
	set(argument_error)::in, set(argument_error)::out, 
	magic_info::in, magic_info::out) is det.

magic_util__traverse_type(IsTopLevel, Parents, ArgType, Errors0, Errors) -->
	magic_info_get_module_info(ModuleInfo),
	( { type_is_atomic(ArgType, ModuleInfo) } ->
		{ Errors = Errors0 }
	; { type_is_higher_order(ArgType, _, _, _) } ->
		% Higher-order types are not allowed.
		{ set__insert(Errors0, higher_order, Errors) }
	; { type_is_tuple(ArgType, TupleArgTypes) } ->
		list__foldl2(magic_util__traverse_type(no, Parents),
			TupleArgTypes, Errors0, Errors)
	; { type_is_aditi_state(ArgType) } ->
		( { IsTopLevel = no } ->
			{ set__insert(Errors0, embedded_aditi_state, Errors) }
		;
			{ Errors = Errors0 }
		)	
	;
		% The type is user-defined.
		( { type_to_ctor_and_args(ArgType, TypeCtor, Args) } ->
			magic_util__check_type_ctor(Parents, TypeCtor, 
				Errors0, Errors1),
			list__foldl2(magic_util__traverse_type(no, Parents),
				Args, Errors1, Errors)
		;
			% type variable - the type parameters 
			% are checked separately.
			{ Errors = Errors0 }
		)
	).

:- pred magic_util__check_type_ctor(set(type_ctor)::in, type_ctor::in, 
	set(argument_error)::in, set(argument_error)::out, 
	magic_info::in, magic_info::out) is det.

magic_util__check_type_ctor(Parents, TypeCtor, Errors0, Errors) -->
	magic_info_get_ok_types(OKTypes0),
	magic_info_get_bad_types(BadTypes0),
	( { set__member(TypeCtor, Parents) } ->
		{ Errors = Errors0 }
	; { set__member(TypeCtor, OKTypes0) } ->
		{ Errors = Errors0 }
	; { map__search(BadTypes0, TypeCtor, TypeErrors) } ->
		{ set__union(Errors0, TypeErrors, Errors) }
	;	
		magic_info_get_module_info(ModuleInfo),
		{ module_info_types(ModuleInfo, Types) },
		{ map__lookup(Types, TypeCtor, TypeDefn) },
		{ hlds_data__get_type_defn_body(TypeDefn, TypeBody) },
		{ set__init(NewErrors0) },
		{ set__insert(Parents, TypeCtor, Parents1) },
		magic_util__check_type_defn(TypeBody, Parents1, 
			NewErrors0, NewErrors),
		( { set__empty(NewErrors) } ->
			{ set__insert(OKTypes0, TypeCtor, OKTypes) },
			{ Errors = Errors0 },
			magic_info_set_ok_types(OKTypes)
		;
			{ map__det_insert(BadTypes0, TypeCtor, 
				NewErrors, BadTypes) },
			{ set__union(Errors0, NewErrors, Errors) },
			magic_info_set_bad_types(BadTypes)
		)
	).

:- pred magic_util__check_type_defn(hlds_type_body::in, set(type_ctor)::in,
		set(argument_error)::in, set(argument_error)::out, 
		magic_info::in, magic_info::out) is det.

magic_util__check_type_defn(du_type(Ctors, _, _, _),
		Parents, Errors0, Errors) -->
	list__foldl2(magic_util__check_ctor(Parents), Ctors, Errors0, Errors).
magic_util__check_type_defn(eqv_type(_), _, _, _) -->
	{ error("magic_util__check_type_defn: eqv_type") }.
magic_util__check_type_defn(abstract_type, _, Errors0, Errors) -->
	{ set__insert(Errors0, abstract, Errors) }.
magic_util__check_type_defn(foreign_type(_, _), _, _, _) -->
	{ error("magic_util__check_type_defn: foreign_type") }.

:- pred magic_util__check_ctor(set(type_ctor)::in, constructor::in, 
		set(argument_error)::in, set(argument_error)::out, 
		magic_info::in, magic_info::out) is det.
		
magic_util__check_ctor(Parents, ctor(ExistQVars, _, _, CtorArgs),
		Errors0, Errors) -->
	( { ExistQVars = [] } ->
		{ assoc_list__values(CtorArgs, CtorArgTypes) },
		list__foldl2(magic_util__traverse_type(no, Parents), 
			CtorArgTypes, Errors0, Errors)
	;
		{ set__insert(Errors0, existentially_typed, Errors) }
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- interface.

	% Information from the preprocessing pass about the magic input
	% variables for a procedure.
:- type magic_proc_info
	--->	magic_proc_info(
			list(mode),	% pre-transformation arg modes
					% (minus aditi__states).
			list(prog_var),	% magic input vars.
			list(type),	% types of magic input vars.
			list(mode),	% modes of magic input vars.
			maybe(int)	% index of this proc's magic 
					% input var in the above lists,
					% no if the procedure is not
					% an entry point of the sub-module.
		).

	% Map from post-transformation pred_proc_id to the 
	% corresponding magic predicate. Magic predicates
	% collect the tuples which would occur as inputs in
	% a top-down execution.
:- type magic_map	==	map(pred_proc_id, pred_proc_id).

	% Map from pre-transformation pred_proc_id to
	% post transformation pred_proc_id.
:- type pred_map	==	map(pred_proc_id, pred_proc_id).

:- type magic_errors == set(magic_error).

:- type magic_info.

:- pred magic_info_init(module_info, magic_info).
:- mode magic_info_init(in, out) is det.

:- pred magic_info_get_module_info(module_info::out, magic_info::in,
		magic_info::out) is det.
:- pred magic_info_get_error_pred_proc_id(pred_proc_id::out, magic_info::in,
		magic_info::out) is det.
:- pred magic_info_get_curr_pred_proc_id(pred_proc_id::out, magic_info::in,
		magic_info::out) is det.
:- pred magic_info_get_pred_info(pred_info::out, magic_info::in,
		magic_info::out) is det.
:- pred magic_info_get_proc_info(proc_info::out, magic_info::in,
		magic_info::out) is det.
:- pred magic_info_get_scc(list(pred_proc_id)::out, magic_info::in,
		magic_info::out) is det.
:- pred magic_info_get_magic_map(magic_map::out, magic_info::in,
		magic_info::out) is det.
:- pred magic_info_get_magic_vars(list(prog_var)::out, magic_info::in,
		magic_info::out) is det.
:- pred magic_info_get_magic_var_map(map(pred_proc_id, prog_var)::out,
		magic_info::in, magic_info::out) is det.
:- pred magic_info_get_next_supp_id(int::out, magic_info::in,
		magic_info::out) is det.
:- pred magic_info_get_magic_proc_info(map(pred_proc_id, magic_proc_info)::out,
		magic_info::in, magic_info::out) is det.
:- pred magic_info_get_pred_map(pred_map::out,
		magic_info::in, magic_info::out) is det.
:- pred magic_info_get_error_vars(set(prog_var)::out,
		magic_info::in, magic_info::out) is det.
:- pred magic_info_get_errors(magic_errors::out,
		magic_info::in, magic_info::out) is det.
:- pred magic_info_get_ok_types(set(type_ctor)::out,
		magic_info::in, magic_info::out) is det.
:- pred magic_info_get_bad_types(map(type_ctor, set(argument_error))::out,
		magic_info::in, magic_info::out) is det.

:- pred magic_info_set_module_info(module_info::in, magic_info::in,
		magic_info::out) is det.
:- pred magic_info_set_error_pred_proc_id(pred_proc_id::in, magic_info::in,
		magic_info::out) is det.
:- pred magic_info_set_curr_pred_proc_id(pred_proc_id::in, magic_info::in,
		magic_info::out) is det.
:- pred magic_info_set_pred_info(pred_info::in, magic_info::in,
		magic_info::out) is det.
:- pred magic_info_set_proc_info(proc_info::in, magic_info::in,
		magic_info::out) is det.
:- pred magic_info_set_scc(list(pred_proc_id)::in, magic_info::in,
		magic_info::out) is det.
:- pred magic_info_set_magic_map(magic_map::in, magic_info::in,
		magic_info::out) is det.
:- pred magic_info_set_magic_vars(list(prog_var)::in, magic_info::in,
		magic_info::out) is det.
:- pred magic_info_set_magic_var_map(map(pred_proc_id, prog_var)::in,
		magic_info::in, magic_info::out) is det.
:- pred magic_info_set_magic_proc_info(map(pred_proc_id, magic_proc_info)::in,
		magic_info::in, magic_info::out) is det.
:- pred magic_info_set_pred_map(pred_map::in,
		magic_info::in, magic_info::out) is det.
:- pred magic_info_set_error_vars(set(prog_var)::in,
		magic_info::in, magic_info::out) is det.
:- pred magic_info_set_errors(magic_errors::in,
		magic_info::in, magic_info::out) is det.
:- pred magic_info_set_ok_types(set(type_ctor)::in,
		magic_info::in, magic_info::out) is det.
:- pred magic_info_set_bad_types(map(type_ctor, set(argument_error))::in,
		magic_info::in, magic_info::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- type magic_info
	--->	magic_info(
			module_info :: module_info,
			error_pred_proc_id :: maybe(pred_proc_id),
			curr_pred_proc_id :: maybe(pred_proc_id),
			pred_info :: maybe(pred_info),
			proc_info :: maybe(proc_info),
			scc :: list(pred_proc_id),
						% preds in the current 
						% sub-module 
			magic_map :: magic_map,	% magic pred_proc_id for
						% each pred_proc_id
					
			magic_vars :: list(prog_var),	
						% magic input variables

			magic_var_map :: map(pred_proc_id, prog_var),
						% magic input variables for
						% each entry-point of the
						% sub-module
			next_supp_id :: int,	% next supp id
			magic_proc_info :: map(pred_proc_id, magic_proc_info),
			pred_map :: pred_map,
						% map from old to transformed
						% pred_proc_id
			error_vars :: set(prog_var),
						% vars for which errors have
						% been reported.
			errors :: magic_errors,
			ok_types :: set(type_ctor),	
						% type_ctors which are allowed
						% as argument types of
						% Aditi predicates. A type
						% is ok if no part of it is
						% higher-order or abstract.
			bad_types :: map(type_ctor, set(argument_error))
						% type_ctors which are not ok
						% as Aditi argument types.
		).	

%-----------------------------------------------------------------------------%

magic_info_init(ModuleInfo, MagicInfo) :-
	map__init(MagicMap),
	map__init(VarMap),
	map__init(MagicProcInfo),
	map__init(PredMap),
	set__init(Errors),
	set__init(OKTypes),
	map__init(BadTypes),
	set__init(ErrorVars),
	MagicInfo = magic_info(ModuleInfo, no, no, no, no, [], MagicMap, [],
			VarMap, 1, MagicProcInfo, PredMap, ErrorVars, Errors, 
			OKTypes, BadTypes).
	
magic_info_get_module_info(Info ^ module_info, Info, Info).

magic_info_get_error_pred_proc_id(PredProcId, Info, Info) :-
	( Info ^ error_pred_proc_id = yes(PredProcId1) ->
		PredProcId = PredProcId1
	;
		error("magic_info_get_error_pred_proc_id")
	).

magic_info_get_curr_pred_proc_id(PredProcId, Info, Info) :-
	( Info ^ curr_pred_proc_id = yes(PredProcId1) ->
		PredProcId = PredProcId1
	;
		error("magic_info_get_curr_pred_proc_id")
	).

magic_info_get_pred_info(PredInfo, Info, Info) :-
	( Info ^ pred_info = yes(PredInfo1) ->
		PredInfo = PredInfo1
	;
		error("magic_info_get_pred_info")
	).

magic_info_get_proc_info(ProcInfo, Info, Info) :-
	( Info ^ proc_info = yes(ProcInfo1) ->
		ProcInfo = ProcInfo1
	;
		error("magic_info_get_proc_info")
	).

magic_info_get_scc(Info ^ scc, Info, Info).

magic_info_get_magic_map(Info ^ magic_map, Info, Info).

magic_info_get_magic_vars(Info ^ magic_vars, Info, Info).

magic_info_get_magic_var_map(Info ^ magic_var_map, Info, Info).

magic_info_get_next_supp_id(SuppId0, Info0,
			Info0 ^ next_supp_id := SuppId0 + 1) :-
		SuppId0 = Info0 ^ next_supp_id.

magic_info_get_magic_proc_info(Info ^ magic_proc_info, Info, Info).

magic_info_get_pred_map(Info ^ pred_map, Info, Info).

magic_info_get_error_vars(Info ^ error_vars, Info, Info).

magic_info_get_errors(Info ^ errors, Info, Info).

magic_info_get_ok_types(Info ^ ok_types, Info, Info).

magic_info_get_bad_types(Info ^ bad_types, Info, Info).

%-----------------------------------------------------------------------------%

magic_info_set_module_info(ModuleInfo, Info, Info ^ module_info := ModuleInfo).

magic_info_set_error_pred_proc_id(PredProcId, Info0,
		Info0 ^ error_pred_proc_id := yes(PredProcId)).

magic_info_set_curr_pred_proc_id(PredProcId, Info0,
		Info0 ^ curr_pred_proc_id := yes(PredProcId)).

magic_info_set_pred_info(PredInfo, Info0, Info0 ^ pred_info := yes(PredInfo)).

magic_info_set_proc_info(ProcInfo, Info0, Info0 ^ proc_info := yes(ProcInfo)).

magic_info_set_scc(SCC, Info0, Info0 ^ scc := SCC).

magic_info_set_magic_map(MagicMap, Info0, Info0 ^ magic_map := MagicMap). 

magic_info_set_magic_vars(Vars, Info0, Info0 ^ magic_vars := Vars).

magic_info_set_magic_var_map(Map, Info0, Info0 ^ magic_var_map := Map).

magic_info_set_magic_proc_info(MagicProcInfo, Info0,
		Info0 ^ magic_proc_info := MagicProcInfo).

magic_info_set_pred_map(PredMap, Info0, Info0 ^ pred_map := PredMap).

magic_info_set_error_vars(ErrorVars, Info0, Info0 ^ error_vars := ErrorVars).

magic_info_set_errors(Errors, Info0, Info0 ^ errors := Errors).

magic_info_set_ok_types(Types, Info0, Info0 ^ ok_types := Types).

magic_info_set_bad_types(Types, Info0, Info0 ^ bad_types := Types).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
% Error handling.

:- interface.

:- type magic_error == pair(magic_error_type, term__context).

:- type magic_arg_id_type
	--->	arg_number
	;	var_name
	.

:- type magic_arg_id
	--->	arg_number(int)
	;	var_name(string)
	.

:- type rtti_arg
	--->	type_info
	;	typeclass_info
	;	both
	.	

:- type magic_error_type
	--->	argument_error(argument_error, magic_arg_id, pred_proc_id)
				% The maybe(int) here is an argument number.
				% If there is no argument number the error
				% occurred creating a supplementary predicate.
	;	nonspecific_polymorphism(pred_proc_id, rtti_arg)
				% There are type-info or typeclass-info
				% arguments, but there are no polymorphic
				% arguments.
	;	curried_argument(pred_proc_id)
				% Curried args to an aggregate closure are NYI.
	;	non_removeable_aditi_state(pred_proc_id,
			prog_varset, list(prog_var))
				% Other than in database calls, `aditi:state'
				% variables can only occur in assignment
				% unifications, since magic sets needs to
				% be able to remove them.
	;	context_error(linearity_error, pred_proc_id)
	;	mutually_recursive_context(pred_proc_id, list(pred_proc_id))
				% Procedures with a `context' marker must
				% not be mutually recursive with other
				% predicates.
	;	mixed_scc(list(pred_proc_id))
				% SCC with Aditi and non-Aditi components.
	.

:- type argument_error
	--->	partially_instantiated
	;	higher_order
	;	abstract
	;	polymorphic
	;	existentially_typed
	;	output_aditi_state
	;	embedded_aditi_state
	.

:- type linearity_error
	--->	end_goals_not_recursive
			% For a goal to be linear, either the first or
			% the last goal must be a recursive call.

	;	multi_rec_goal_not_multi_linear
			% The last call in a rule with multiple recursive
			% calls was not recursive.

	;	inputs_to_recursive_call
			% for the recursive call in a left-linear rule,
			% and for the internal recursive calls in
			% a multi-linear rule, the inputs must be the
			% inputs to the procedure. 

	;	outputs_of_recursive_call
			% for the last recursive call in a right- or
			% multi-linear rule, the outputs must be the
			% outputs of the procedure. 

	;	inputs_occur_in_other_goals
			% For left-linear rules, the inputs to the procedure
			% may only occur in the recursive call.
			% For multi-linear rules, the inputs to the procedure
			% may only occur as inputs to the interior recursive
			% calls.

	;	multi_inputs_occur_in_last_rec_call
			% For multi-linear predicates, the inputs
			% to the last recursive call may not include
			% any inputs to the procedure.
	.

%-----------------------------------------------------------------------------%
:- implementation.

magic_util__report_errors(Errors, ModuleInfo, Verbose) -->
	list__foldl(magic_util__report_error(ModuleInfo, Verbose), Errors).

:- pred magic_util__report_error(module_info::in, bool::in, magic_error::in,
		io__state::di, io__state::uo) is det.

magic_util__report_error(ModuleInfo, Verbose,
		argument_error(Error, Arg, proc(PredId, _)) - Context) -->

	{ error_util__describe_one_pred_name(ModuleInfo, PredId, PredName) },
	{ string__append_list(["In Aditi ", PredName, ":"], PredNamePiece) },
	{ magic_util__error_arg_id_piece(Arg, ArgPiece) },
	{ magic_util__report_argument_error(Context, Error, ArgPiece,
		Verbose, SecondPart) },
	write_error_pieces(Context, 0, [fixed(PredNamePiece), nl | SecondPart]).

magic_util__report_error(ModuleInfo, _Verbose,
		nonspecific_polymorphism(proc(PredId, _), _) - Context) -->
	{ error_util__describe_one_pred_name(ModuleInfo, PredId, PredName) },
	{ string__append_list(["In ", PredName, ":"], PredNamePiece) },
	{ SecondPart = [words("the code uses polymorphism or type-classes"),
			words("which are not supported by Aditi.")] },
	write_error_pieces(Context, 0, [fixed(PredNamePiece), nl | SecondPart]).

magic_util__report_error(ModuleInfo, _Verbose,
		curried_argument(proc(PredId, _)) - Context) -->
	{ error_util__describe_one_pred_name(ModuleInfo, PredId, PredName) },
	{ string__append_list(["In ", PredName, ":"], PredNamePiece) },
	{ SecondPart = [words("sorry, curried closure arguments are not"),
			words("implemented for Aditi procedures."),
			words("Construct them within the closure instead.")] },
	write_error_pieces(Context, 0, [fixed(PredNamePiece), nl | SecondPart]).

magic_util__report_error(ModuleInfo, _Verbose,
		non_removeable_aditi_state(proc(PredId, _), VarSet, Vars)
			- Context) -->
	{ error_util__describe_one_pred_name(ModuleInfo, PredId, PredName) },
	{ string__append_list(["In ", PredName, ":"], PredNamePiece) },
	{ Vars = [_] ->
		VarPiece = words("variable"),
		StatePiece = words("is a non-removable `aditi:state'.")
	;
		VarPiece = words("variables"),
		StatePiece = words("are non-removable `aditi:state's.")
	},
	{ list__map(varset__lookup_name(VarSet), Vars, VarNames) },
	{ error_util__list_to_pieces(VarNames, VarNamePieces) },
	{ list__condense([[fixed(PredNamePiece), nl, VarPiece],
		VarNamePieces, [StatePiece]], Pieces) },
	write_error_pieces(Context, 0, Pieces).

magic_util__report_error(ModuleInfo, Verbose,
		context_error(Error, proc(PredId, _ProcId)) - Context) -->
	{ error_util__describe_one_pred_name(ModuleInfo, PredId, PredName) },
	{ string__append_list(["In ", PredName, ":"], PredNamePiece) },
	{ SecondPart = [words("with `:- pragma context(...)' declaration:"),
		nl, words("error: recursive rule is not linear.\n")] },
	{ magic_util__report_linearity_error(ModuleInfo,
		Context, Verbose, Error, LinearityPieces) },
	{ list__append([fixed(PredNamePiece), nl | SecondPart],
		LinearityPieces, Pieces) },
	write_error_pieces(Context, 0, Pieces).

magic_util__report_error(ModuleInfo, _Verbose,
		mutually_recursive_context(PredProcId,
			OtherPredProcIds) - Context) -->
	{ error_util__describe_one_proc_name(ModuleInfo,
		PredProcId, ProcPiece) },
	{ error_util__describe_several_proc_names(ModuleInfo,
		OtherPredProcIds, OtherProcPieces) },
	{ list__condense(
		[[words("Error: procedure"), words(ProcPiece), words("with a"),
		fixed("`:- pragma context(...)"),
		words("declaration is mutually recursive with")],
		OtherProcPieces, [words(".")]], Pieces) },
	write_error_pieces(Context, 0, Pieces).

magic_util__report_error(ModuleInfo, _Verbose,
		mixed_scc(PredProcIds) - Context) -->
	{ error_util__describe_several_proc_names(ModuleInfo,
		PredProcIds, SCCPieces) },
	{ list__condense([
		[words("In the strongly connected component consisting of")],
		SCCPieces,
		[words("some, but not all procedures are marked"),
		words("for Aditi compilation.")]], Pieces) },
	write_error_pieces(Context, 0, Pieces).

:- pred magic_util__error_arg_id_piece(magic_arg_id::in,
		format_component::out) is det.

magic_util__error_arg_id_piece(arg_number(ArgNo), words(ArgWords)) :-
	string__int_to_string(ArgNo, ArgStr),
	string__append("argument ", ArgStr, ArgWords).
magic_util__error_arg_id_piece(var_name(Name), words(NameStr)) :-
	string__append_list(["`", Name, "'"], NameStr).

:- pred magic_util__report_argument_error(term__context::in,
		argument_error::in, format_component::in, bool::in,
		list(format_component)::out) is det.

magic_util__report_argument_error(_Context, partially_instantiated,
		ArgPiece, _Verbose, Pieces) :-
	Pieces = [ArgPiece, words("is partially instantiated.")].
magic_util__report_argument_error(_Context, higher_order,
		ArgPiece, _, Pieces) :-
	Pieces = [words("the type of"), ArgPiece, words("is higher order.")].
magic_util__report_argument_error(_Context, polymorphic, ArgPiece, _, Pieces) :-
	Pieces = [words("the type of"), ArgPiece, words("is polymorphic.")].
magic_util__report_argument_error(_Context, existentially_typed,
		ArgPiece, _, Pieces) :-
	Pieces = [words("the type of"), ArgPiece,
		words("contains existentially typed constructors.")].
magic_util__report_argument_error(_Context, abstract, ArgPiece, _, Pieces) :-
	Pieces = [words("the type of"), ArgPiece,
			words("contains abstract types.")].
magic_util__report_argument_error(_Context, output_aditi_state,
		ArgPiece, _, Pieces) :-
	Pieces = [ArgPiece, words("is an output `aditi:state'.")].
magic_util__report_argument_error(_Context, embedded_aditi_state,
		ArgPiece, _, Pieces) :-
	Pieces = [words("the type of"), ArgPiece,
		words("contains an embedded `aditi:state'.")].

:- pred magic_util__report_linearity_error(module_info::in, term__context::in,
	bool::in, linearity_error::in, list(format_component)::out) is det.

magic_util__report_linearity_error(_ModuleInfo, _Context, _Verbose, 
		end_goals_not_recursive, Pieces) :-
	Pieces = [words("For a rule to be linear, either the first or last"),
		words("goal must be a recursive call.")].
magic_util__report_linearity_error(_ModuleInfo, _Context, _Verbose, 
		multi_rec_goal_not_multi_linear, Pieces) :-
	Pieces = [words("The rule contains multiple recursive calls but is"),
		words("not multi-linear because the last goal"),
		words("is not recursive.")].
magic_util__report_linearity_error(_ModuleInfo, _Context, _Verbose,
		inputs_to_recursive_call, Pieces) :-
	Pieces = [words("For the rule to be linear, the input variables of"),
		words("this recursive call must be the same as the input"),
		words("variables of the clause head.")].
magic_util__report_linearity_error(_, _, _, 
		outputs_of_recursive_call, Pieces) :-
	Pieces = [words("For the rule to be linear, the output variables of"),
		words("this recursive call must be the same as the output"),
		words("variables of the clause head.")].
magic_util__report_linearity_error(_, _, _, 
		inputs_occur_in_other_goals, Pieces) :-
	Pieces = [words("The inputs to the rule may only occur in"),
		words("recursive calls, unless the rule is right-linear.")].
magic_util__report_linearity_error(_, _, _, 
		multi_inputs_occur_in_last_rec_call, Pieces) :-
	Pieces = [words("In a multi-linear rule, the inputs to the"),
		words("procedure may not occur as arguments of the last"),
		words("recursive call.")].

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
