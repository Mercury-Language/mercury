%-----------------------------------------------------------------------------%
% Copyright (C) 1999-2000,2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Module:	accumulator
% Main authors: petdr
%
% Attempts to transform a single proc to a tail recursive form by
% introducing accumlators.  The algorithm can do this if the code after
% the recursive call has either the order independent state update or
% associative property.
%
% /* Order independent State update property */
% :- promise all [A,B,S0,S]
% 	(
% 		(some[SA] (update(A, S0, SA), update(B, SA, S)))
% 	<=>
% 		(some[SB] (update(B, S0, SB), update(A, SB, S)))
% 	).	
%
% /* Associativity property */
% :- promise all [A,B,C,ABC]
% 	(
% 		(some[AB] (assoc(A, B, AB), assoc(AB, C, ABC)))
% 	<=>
% 		(some[BC] (assoc(B, C, BC), assoc(A, BC, ABC)))
% 	).	
%
% The algorithm implemented is a combintation of the algorithms from
% "Making Mercury Programs Tail Recursive" and
% "State Update Transformation", which can be found at
% <http://www.cs.mu.oz.au/research/mercury/information/papers.html>.
%
% Note that currently "State Update Transformation" paper only resides
% in CVS papers archive in the directory update, but has been submitted
% to PPDP '00.
%
% The transformation recognises predicates in the form
%
% p(In, OutUpdate, OutAssoc) :-
% 	minimal(In),
% 	initialize(OutUpdate),
%	base(OutAssoc).
% p(In, OutUpdate, OutAssoc) :-
% 	decompose(In, Current, Rest),
% 	p(Rest, OutUpdate0, OutAssoc0),
% 	update(Current, OutUpdate0, OutUpdate),
% 	assoc(Current, OutAssoc0, OutAssoc).
%
% which can be transformed by the algorithm in "State Update
% Transformation" to
%
% p(In, OutUpdate, OutAssoc) :-
% 	initialize(AccUpdate),
% 	p_acc(In, OutUpdate, OutAssoc, AccUpdate).
%
% p_acc(In, OutUpdate, OutAssoc, AccUpdate) :-
% 	minimal(In),
% 	base(OutAssoc),
% 	OutUpdate = AccUpdate.
% p_acc(In, OutUpdate, OutAssoc, AccUpdate0) :-
% 	decompose(In, Current, Rest),
% 	update(Current, AccUpdate0, AccUpdate),
% 	p_acc(Rest, OutUpdate, OutAssoc0, AccUpdate),
% 	assoc(Current, OutAssoc0, OutAssoc).
%
% we then apply the algorithm from "Making Mercury Programs Tail
% Recursive" to p_acc to obtain
%
% p_acc(In, OutUpdate, OutAssoc, AccUpdate) :-
% 	minimal(In),
% 	base(OutAssoc),
% 	OutUpdate = AccUpdate.
% p_acc(In, OutUpdate, OutAssoc, AccUpdate0) :-
% 	decompose(In, Current, Rest),
% 	update(Current, AccUpdate0, AccUpdate),
% 	p_acc2(Rest, OutUpdate, OutAssoc, AccUpdate, Current).
%
% p_acc2(In, OutUpdate, OutAssoc, AccUpdate0, AccAssoc0) :-
% 	minimal(In),
% 	base(Base),
% 	assoc(AccAssoc0, Base, OutAssoc),
% 	OutUpdate = AccUpdate0.
% p_acc2(In, OutUpdate, OutAssoc, AccUpdate0, AccAssoc0) :-
% 	decompose(In, Current, Rest),
% 	update(Current, AccUpdate0, AccUpdate),
% 	assoc(AccAssoc0, Current, AccAssoc),
% 	p_acc2(Rest, OutUpdate, OutAssoc, AccUpdate, AccAssoc).
%
% p_acc is no longer recursive and is only ever called from p, so we
% inline p_acc into p to obtain the final schema.
%
% p(In, OutUpdate, OutAssoc) :-
% 	minimal(In),
% 	base(OutAssoc),
% 	initialize(AccUpdate),
% 	OutUpdate = AccUpdate.
% p(In, OutUpdate, OutAssoc) :-
% 	decompose(In, Current, Rest),
% 	initialize(AccUpdate0),
% 	update(Current, AccUpdate0, AccUpdate),
% 	p_acc2(Rest, OutUpdate, OutAssoc, AccUpdate, Current).
%
% p_acc2(In, OutUpdate, OutAssoc, AccUpdate0, AccAssoc0) :-
% 	minimal(In),
% 	base(Base),
% 	assoc(AccAssoc0, Base, OutAssoc),
% 	OutUpdate = AccUpdate0.
% p_acc2(In, OutUpdate, OutAssoc, AccUpdate0, AccAssoc0) :-
% 	decompose(In, Current, Rest),
% 	update(Current, AccUpdate0, AccUpdate),
% 	assoc(AccAssoc0, Current, AccAssoc),
% 	p_acc2(Rest, OutUpdate, OutAssoc, AccUpdate, AccAssoc).
%
% The only real difficulty in this new transformation is identifying the
% initialize/1 and base/1 goals from the original base case.
%
% Note that if the recursive clause contains multiple calls to p, the
% transformation attempts to move each recursive call to the end
% until one succeeds.  This makes the order of independent recursive
% calls in the body irrelevant.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module transform_hlds__accumulator.

:- interface.

:- import_module hlds__hlds_module, hlds__hlds_pred, io.

:- pred accumulator__process_proc(pred_id::in, proc_id::in, proc_info::in,
		proc_info::out, module_info::in, module_info::out,
		io__state::di, io__state::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module (hlds__assertion), hlds__error_util.
:- import_module transform_hlds__goal_store, hlds__goal_util, libs__globals.
:- import_module hlds__hlds_data, hlds__hlds_goal, hlds__hlds_out.
:- import_module (parse_tree__inst).
:- import_module check_hlds__inst_match, hlds__instmap, check_hlds__mode_util.
:- import_module libs__options.
:- import_module parse_tree__prog_data, parse_tree__prog_util.
:- import_module hlds__quantification.

:- import_module assoc_list, bool, int, list, map, multi_map.
:- import_module require, set, std_util, string, term, varset.

%-----------------------------------------------------------------------------%

	% The form of the goal around the base and recursive cases.
:- type top_level
	--->	switch_base_rec
	;	switch_rec_base
	;	disj_base_rec
	;	disj_rec_base
	;	ite_base_rec
	;	ite_rec_base.

	% A goal is represented by two integers, the first integer
	% stores which conjunction the goal came from (base or
	% recursive), and the second stores the location of the goal in
	% the conjunction.
:- type goal_id == pair(int).

	% The goal_store associates a goal with each goal_id.
:- type goal_store == goal_store(goal_id).

	% A substition from the first variable name to the second.
:- type subst == map(prog_var, prog_var).

:- type warning 
			% warn that two prog_vars in call to pred_id
			% at prog_context were swapped, which may cause
			% an efficiency problem.
	--->	warn(prog_context, pred_id, prog_var, prog_var).

:- type warnings == list(warning).

%-----------------------------------------------------------------------------%

	%
	% process_proc
	%
	% Attempt to transform a procedure into accumulator
	% recursive form.
	%
process_proc(PredId, ProcId, ProcInfo0, ProcInfo, 
		ModuleInfo0, ModuleInfo) -->
	globals__io_lookup_bool_option(optimize_constructor_last_call, DoLCO),
	globals__io_lookup_bool_option(fully_strict, FullyStrict),
	(
		{ module_info_pred_info(ModuleInfo0, PredId, PredInfo) },
		{ attempt_transform(ProcId, ProcInfo0, PredId,
				PredInfo, DoLCO, FullyStrict, ModuleInfo0,
				ProcInfo1, ModuleInfo1, Warnings) }
	->
		globals__io_lookup_bool_option(very_verbose, VeryVerbose),
		( 
			{ VeryVerbose = yes }
		->
			io__write_string("% Accumulators introduced into "),
			hlds_out__write_pred_id(ModuleInfo1, PredId),
			io__write_string("\n")
		;
			[]
		),

		globals__io_lookup_bool_option(inhibit_accumulator_warnings,
				InhibitWarnings),
		(
			{ InhibitWarnings = yes ; Warnings = [] }
		->
			{ ModuleInfo = ModuleInfo1 }
		;
			{ error_util__describe_one_pred_name(ModuleInfo1,
					PredId, PredName) },
			{ pred_info_context(PredInfo, Context) },

			error_util__write_error_pieces(Context, 0,
					[words("In"), words(PredName)]),

			{ proc_info_varset(ProcInfo, VarSet) },
			output_warnings(Warnings, VarSet, ModuleInfo1),

			error_util__write_error_pieces(Context, 2, 
					[
					words("Please ensure that these"),
					words("argument rearrangements"),
					words("do not introduce"),
					words("performance problems."),
					words("These warnings can"),
					words("be suppressed by"),
					words("`--inhibit-accumulator-warnings'.")
					]),

			globals__io_lookup_bool_option(verbose_errors,
					VerboseErrors),
			(
				{ VerboseErrors = yes }
			->
				error_util__write_error_pieces(Context, 2, 
					[
					words("If a predicate has been"),
					words("declared associative via"),
					words("a `promise' declaration,"),
					words("the compiler will"),
					words("rearrange the order"),
					words("of the arguments"),
					words("in calls to that"),
					words("predicate, if by so doing"),
					words("it makes the containing"),
					words("predicate tail recursive."),
					words("In such situations, the"),
					words("compiler will issue"),
					words("this warning."),
					words("If this reordering changes the"),
					words("performance characteristics"),
					words("of the call to the"),
					words("predicate, use"),
					words("`--no-accumulator-introduction'"),
					words("to turn the optimization"),
					words("off, or"),
					words("`--inhibit-accumulator-warnings'"),
					words("to turn off the warnings.")
					])
			;
				[]
			),

			globals__io_lookup_bool_option(halt_at_warn,
					HaltAtWarn),
			(
				{ HaltAtWarn = yes }
			->
				io__set_exit_status(1),
				{ module_info_incr_errors(ModuleInfo1,
						ModuleInfo) }
			;
				{ ModuleInfo = ModuleInfo1 }
			)
		),
		{ ProcInfo   = ProcInfo1 }
	;
		{ ProcInfo   = ProcInfo0 },
		{ ModuleInfo = ModuleInfo0 }
	).


%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred output_warnings(list(warning)::in, prog_varset::in,
		module_info::in, io__state::di, io__state::uo) is det.

output_warnings([], _, _) --> [].
output_warnings([W | Ws], VarSet, ModuleInfo) -->
	{ output_warning(W, VarSet, ModuleInfo, Context, Format) },
	error_util__write_error_pieces(Context, 2, Format),
	output_warnings(Ws, VarSet, ModuleInfo).

:- pred output_warning(warning::in, prog_varset::in, module_info::in,
		prog_context::out, list(format_component)::out) is det.

output_warning(warn(Context, PredId, VarA, VarB), VarSet, ModuleInfo,
		Context, Formats) :-
	error_util__describe_one_pred_name(ModuleInfo, PredId, PredStr),

	varset__lookup_name(VarSet, VarA, VarAStr0),
	varset__lookup_name(VarSet, VarB, VarBStr0),
	VarAStr = string__append_list(["`", VarAStr0, "'"]),
	VarBStr = string__append_list(["`", VarBStr0, "'"]),

	Formats = [words("warning: the call to"), words(PredStr),
			words("has had the location of the variables"),
			words(VarAStr), words("and"), words(VarBStr),
			words("swapped to allow accumulator introduction.")
			].

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	%
	% attempt_transform is only true iff the current
	% proc has been transformed to call the newly created
	% accumulator proc.
	%
:- pred attempt_transform(proc_id::in, proc_info::in,
		pred_id::in, pred_info::in, bool::in, bool::in, module_info::in,
		proc_info::out, module_info::out, warnings::out) is semidet.

attempt_transform(ProcId, ProcInfo0, PredId, PredInfo, DoLCO,
		FullyStrict, ModuleInfo0, ProcInfo, ModuleInfo, Warnings) :-
	proc_info_goal(ProcInfo0, Goal0),
	proc_info_headvars(ProcInfo0, HeadVars),
	proc_info_get_initial_instmap(ProcInfo0, ModuleInfo0, InitialInstMap),

	standardize(Goal0, Goal),

	identify_goal_type(PredId, ProcId, Goal, InitialInstMap,
			TopLevel, Base, BaseInstMap, Rec, RecInstMap),

	C = initialize_goal_store(Rec, RecInstMap, Base, BaseInstMap),

	identify_recursive_calls(PredId, ProcId, C, RecCallIds),

	M = list__length(Rec),

	attempt_transform_2(RecCallIds, C, M, Rec, HeadVars, InitialInstMap,
			TopLevel, DoLCO, FullyStrict, PredInfo,
			ProcInfo0, ModuleInfo0, Warnings, ProcInfo, ModuleInfo).



	%
	% attempt_transform_2 takes a list of locations of the recursive
	% calls, and attempts to introduce accumulator into each of the
	% recursive calls, stopping at the first one that succeeds.
	% This catches the following case, as selecting the first
	% recursive call allows the second recursive call to be moved
	% before it, and OutA is in the correct spot in list__append.
	%
	% 	p(InA, OutA),
	% 	p(InB, OutB),
	% 	list__append(OutB, OutA, Out)
	%
:- pred attempt_transform_2(list(goal_id)::in, goal_store::in, int::in,
		hlds_goals::in, prog_vars::in, instmap::in, top_level::in,
		bool::in, bool::in, pred_info::in, proc_info::in,
		module_info::in, warnings::out,
		proc_info::out, module_info::out) is semidet.

attempt_transform_2([Id | Ids], C, M, Rec, HeadVars, InitialInstMap, TopLevel,
		DoLCO, FullyStrict, PredInfo, ProcInfo0, ModuleInfo0,
		Warnings, ProcInfo, ModuleInfo) :-
	(
		proc_info_vartypes(ProcInfo0, VarTypes0),
		identify_out_and_out_prime(Id, Rec, HeadVars,
				InitialInstMap, VarTypes0, ModuleInfo0, Out,
				OutPrime, HeadToCallSubst, CallToHeadSubst),

		stage1(Id, M, C, DoLCO, FullyStrict, VarTypes0, ModuleInfo0,
			Sets),

		stage2(Id, C, Sets, OutPrime, Out, ModuleInfo0, ProcInfo0,
				VarSet - VarTypes, Accs, BaseCase, BasePairs,
				Substs, CS, Warnings0),

		stage3(Id, Accs, VarSet, VarTypes, C, CS, Substs,
				HeadToCallSubst, CallToHeadSubst,
				BaseCase, BasePairs, Sets, TopLevel, PredInfo,
				ProcInfo0, ModuleInfo0, ProcInfo1,
				ModuleInfo1)
	->
		Warnings = Warnings0,
		ProcInfo = ProcInfo1,
		ModuleInfo = ModuleInfo1
	;
		attempt_transform_2(Ids, C, M, Rec, HeadVars, InitialInstMap,
				TopLevel, DoLCO, FullyStrict,
				PredInfo, ProcInfo0, ModuleInfo0,
				Warnings, ProcInfo, ModuleInfo)
	).


%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	%
	% standardize
	%
	% Transform the goal into a standard form that is amenable to
	% introducing accumulators.
	%
	% At the moment all this does is remove any extra disj/conj
	% wrappers around the top level goal.
	%
	% Future work is for this code to rearrange code with multiple
	% base and recursive cases into a single base and recursive
	% case.
	%
:- pred standardize(hlds_goal, hlds_goal).
:- mode standardize(in, out) is det.

standardize(Goal0, Goal) :-
	(
		(
			Goal0 = conj([Goal1]) - _
		;
			Goal0 = disj([Goal1]) - _
		)
	->
		standardize(Goal1, Goal)
	;
		Goal = Goal0
	).


%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	%
	% This predicate takes the original goal and identifies the
	% `shape' of the goal around the recursive and base cases.
	% Note that the base case can contain a recursive call, as the
	% transformation doesn't depend on what is in the base case.
	%
:- pred identify_goal_type(pred_id::in, proc_id::in, hlds_goal::in,
		instmap::in, top_level::out, hlds_goals::out, instmap::out,
		hlds_goals::out, instmap::out) is semidet.

identify_goal_type(PredId, ProcId, Goal, InitialInstMap,
		Type, Base, BaseInstMap, Rec, RecInstMap) :-
	(
		Goal = switch(_Var, _CanFail, Cases) - _GoalInfo,
		Cases = [case(_IdA, GoalA), case(_IdB, GoalB)],
		goal_to_conj_list(GoalA, GoalAList),
		goal_to_conj_list(GoalB, GoalBList)
	->
		(
			is_recursive_case(GoalAList, proc(PredId, ProcId))
		->
			Type = switch_rec_base,
			Base = GoalBList,
			Rec = GoalAList
		;
			is_recursive_case(GoalBList, proc(PredId, ProcId))
		->
			Type = switch_base_rec,
			Base = GoalAList,
			Rec = GoalBList
		;
			fail
		),
		BaseInstMap = InitialInstMap,
		RecInstMap = InitialInstMap
	;
		Goal = disj(Goals) - _GoalInfo,
		Goals = [GoalA, GoalB],
		goal_to_conj_list(GoalA, GoalAList),
		goal_to_conj_list(GoalB, GoalBList)
	->
		(
			is_recursive_case(GoalAList, proc(PredId, ProcId))
		->
			Type = disj_rec_base,
			Base = GoalBList,
			Rec = GoalAList
		;
			is_recursive_case(GoalBList, proc(PredId, ProcId))
		->
			Type = disj_base_rec,
			Base = GoalAList,
			Rec = GoalBList
		;
			fail
		),
		BaseInstMap = InitialInstMap,
		RecInstMap = InitialInstMap
	;
		Goal = if_then_else(_Vars, If, Then, Else) - _GoalInfo,

		If = _IfGoal - IfGoalInfo,
		goal_info_get_instmap_delta(IfGoalInfo, IfInstMapDelta),

		goal_to_conj_list(Then, GoalAList),
		goal_to_conj_list(Else, GoalBList)
	->
		(
			is_recursive_case(GoalAList, proc(PredId, ProcId))
		->
			Type = ite_rec_base,
			Base = GoalBList,
			Rec = GoalAList,

			BaseInstMap = InitialInstMap,
			instmap__apply_instmap_delta(InitialInstMap,
					IfInstMapDelta, RecInstMap)
		;
			is_recursive_case(GoalBList, proc(PredId, ProcId))
		->
			Type = ite_base_rec,
			Base = GoalAList,
			Rec = GoalBList,

			RecInstMap = InitialInstMap,
			instmap__apply_instmap_delta(InitialInstMap,
					IfInstMapDelta, BaseInstMap)
		;
			fail
		)
	;
		fail
	).


	%
	% is_recursive_case(Gs, Id) is true iff the list of goals, Gs,
	% contains a call to the procedure specified by Id, where the
	% call is located in a position that can be used by the
	% transformation (ie not hidden in a compound goal).
	%
:- pred is_recursive_case(list(hlds_goal)::in, pred_proc_id::in) is semidet.

is_recursive_case(Goals, proc(PredId, ProcId)) :-
	list__append(_Initial, [RecursiveCall | _Final], Goals),
	RecursiveCall = call(PredId, ProcId, _, _, _, _) - _.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% The store info is folded over the list of goals which
	% represent the base and recursive case conjunctions.
:- type store_info
	--->	store_info(
			int,		% the location of the goal in
					% the conjunction.
			instmap,
			goal_store
		).

	%
	% Initialise the goal_store, which will hold the C_{a,b} goals.
	%
:- func initialize_goal_store(hlds_goals, instmap,
		hlds_goals, instmap) = goal_store.

initialize_goal_store(Rec, RecInstMap, Base, BaseInstMap) = C :-
	goal_store__init(C0),
	list__foldl(store(rec), Rec, store_info(1, RecInstMap, C0), 
			store_info(_, _, C1)),
	list__foldl(store(base), Base,
			store_info(1, BaseInstMap, C1), 
			store_info(_, _, C)).

	%
	% store(Id, G, SI0, SI) is true iff the goal G is stored inside
	% the goal_store (which is part of SI) with the correct
	% identifier and instmap associatied with the goal.
	%
:- pred store(int::in, hlds_goal::in, store_info::in, store_info::out) is det.

store(Identifier, Goal, store_info(N, IM0, GS0), store_info(N+1, IM, GS)) :-
	Goal = _ - GoalInfo,
	goal_info_get_instmap_delta(GoalInfo, InstMapDelta),
	instmap__apply_instmap_delta(IM0, InstMapDelta, IM),
	
	goal_store__det_insert(GS0, Identifier - N, Goal - IM0, GS).
	
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Determine the k's which are recursive calls
	% Note that this doesn't find recursive calls which are `hidden'
	% in compound goals, this is not a problem as currently we can't
	% use these to do transformation.
:- pred identify_recursive_calls(pred_id::in, proc_id::in,
		goal_store::in, list(goal_id)::out) is det.

identify_recursive_calls(PredId, ProcId, GoalStore, Ids) :-
	P = (pred(Key::out) is nondet :-
		goal_store__member(GoalStore, Key, Goal - _InstMap),
		Key = rec - _,
		Goal = call(PredId, ProcId, _, _, _, _) - _
	),
	solutions(P, Ids).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	%
	% identify_out_and_out_prime
	%
	% Determine the variables which are members of the sets Out and
	% Out', and initialize the substitutions between the two sets.
	%
	% This is done by identifing those variables whose
	% instantiatedness change in the goals after the recursive call
	% and are headvars.
	%
	% Note that we are only identifying the output variables which
	% will need to be accumulated, as there may be other output
	% variables which are produced prior to the recursive call.
	%
:- pred identify_out_and_out_prime(goal_id::in, hlds_goals::in, prog_vars::in,
		instmap::in, vartypes::in, module_info::in, prog_vars::out,
		prog_vars::out, subst::out, subst::out) is det.

identify_out_and_out_prime(_N - K, Rec, HeadVars, InitialInstMap, VarTypes,
		ModuleInfo, Out, OutPrime, HeadToCallSubst, CallToHeadSubst) :-
	(
		list__take(K, Rec, InitialGoals),
		list__drop(K-1, Rec, FinalGoals),
		FinalGoals = [call(_, _, Args, _, _, _) - _ | Rest]
	->
		goal_list_instmap_delta(InitialGoals, InitInstMapDelta),
		instmap__apply_instmap_delta(InitialInstMap,
				InitInstMapDelta, InstMapBeforeRest),


		goal_list_instmap_delta(Rest, InstMapDelta),
		instmap__apply_instmap_delta(InstMapBeforeRest,
				InstMapDelta, InstMapAfterRest),

		instmap_changed_vars(InstMapBeforeRest, InstMapAfterRest,
			VarTypes, ModuleInfo, ChangedVars),

		assoc_list__from_corresponding_lists(HeadVars, Args, HeadArg0),

		Member = (pred(M::in) is semidet :-
				M = HeadVar - _,
				set__member(HeadVar, ChangedVars)
		),
		list__filter(Member, HeadArg0, HeadArg),
		list__map(fst, HeadArg, Out),
		list__map(snd, HeadArg, OutPrime),

		map__from_assoc_list(HeadArg, HeadToCallSubst),

		list__map((pred(X-Y::in, Y-X::out) is det), HeadArg, ArgHead),
		map__from_assoc_list(ArgHead, CallToHeadSubst)
	;
		error("accumulator__identify_out_and_out_prime")
	).


%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	%
	% For each goal after the recursive call, we place that goal
	% into a set according to what properties that goal has.
	% For the definition of what goes into each set, inspect the
	% documentation for the functions named before, assoc, and so on.
	%
:- type sets
	--->	sets(
			before		::	set(goal_id),
			assoc		::	set(goal_id),
			construct_assoc	::	set(goal_id),
			construct	::	set(goal_id),
			update		::	set(goal_id),
			reject		::	set(goal_id)
		).

	%
	% Stage 1 is responsible for identifying which goals are
	% associative, which can be moved before the recursive call and
	% so on.
	%
:- pred stage1(goal_id::in, int::in, goal_store::in, bool::in, bool::in,
		vartypes::in, module_info::in, sets::out) is semidet.

stage1(N - K, M, GoalStore, DoLCO, FullyStrict, VarTypes, ModuleInfo, Sets) :-
	sets_init(Sets0),
	stage1_2(N - (K+1), K, M, GoalStore, FullyStrict, VarTypes, ModuleInfo,
			Sets0, Sets1),
	Sets1 = sets(Before, Assoc, ConstructAssoc, Construct, Update, Reject),
	Sets = sets(Before `set__union` set_upto(N, (K-1)), Assoc,
			ConstructAssoc, Construct, Update, Reject),

		% Continue the transformation only if the set reject is
		% empty and the set assoc or update contains something
		% that needs to be moved before the recursive call.
	set__empty(Reject),
	(
		not set__empty(Assoc)
	;
		not set__empty(Update)
	),
	(
		DoLCO = no
	->
			% If LCMC is not turned on then there must be no
			% construction unifications after the recursive
			% call.
		set__empty(Construct),
		set__empty(ConstructAssoc)
	;
		true
	).



	%
	% For each goal after the recursive call decide which set the
	% goal belongs to.
	%
:- pred stage1_2(goal_id::in, int::in, int::in, goal_store::in,
	bool::in, vartypes::in, module_info::in, sets::in, sets::out) is det.

stage1_2(N - I, K, M, GoalStore, FullyStrict, VarTypes, ModuleInfo,
		Sets0, Sets) :-
	(
		I > M
	->
		Sets0 = Sets
	;
		(
			before(N - I, K, GoalStore, Sets0,
					FullyStrict, VarTypes, ModuleInfo)
		->
			stage1_2(N - (I+1), K, M, GoalStore,
				FullyStrict, VarTypes, ModuleInfo,
				Sets0^before :=
					set__insert(Sets0^before, N - I),
				Sets)
		;
			assoc(N - I, K, GoalStore, Sets0,
					FullyStrict, VarTypes, ModuleInfo)
		->
			stage1_2(N - (I+1), K, M, GoalStore,
				FullyStrict, VarTypes, ModuleInfo,
				Sets0^assoc :=
					set__insert(Sets0^assoc, N - I),
				Sets)
		;
			construct(N - I, K, GoalStore, Sets0,
					FullyStrict, VarTypes, ModuleInfo)
		->
			stage1_2(N - (I+1), K, M, GoalStore,
				FullyStrict, VarTypes, ModuleInfo,
				Sets0^construct :=
					set__insert(Sets0^construct, N - I),
				Sets)
		;
			construct_assoc(N - I, K, GoalStore, Sets0,
					FullyStrict, VarTypes, ModuleInfo)
		->
			stage1_2(N - (I+1), K, M, GoalStore,
				FullyStrict, VarTypes, ModuleInfo,
				Sets0^construct_assoc :=
					set__insert(Sets0^construct_assoc, N-I),
				Sets)
		;
			update(N - I, K, GoalStore, Sets0,
					FullyStrict, VarTypes, ModuleInfo)
		->
			stage1_2(N - (I+1), K, M, GoalStore,
				FullyStrict, VarTypes, ModuleInfo,
				Sets0^update :=
					set__insert(Sets0^update, N - I),
				Sets)
		;
			Sets = Sets0^reject := set__insert(Sets0^reject, N - I)
		)
	).
	
%-----------------------------------------------------------------------------%

:- pred sets_init(sets::out) is det.

sets_init(Sets) :-
	set__init(EmptySet),
	Before = EmptySet,
	Assoc = EmptySet,
	ConstructAssoc = EmptySet,
	Construct = EmptySet,
	Update = EmptySet,
	Reject = EmptySet,
	Sets = sets(Before, Assoc, ConstructAssoc, Construct, Update, Reject).

	%
	% set_upto(N, K) returns the set {(N,1)...(N,K)}
	%
:- func set_upto(int, int) = set(goal_id).

set_upto(N, K) = Set :-
	(
		K =< 0
	->
		set__init(Set)
	;
		Set0 = set_upto(N, K-1),
		set__insert(Set0, pair(N, K), Set)
	).


%-----------------------------------------------------------------------------%

	%
	% A goal is a member of the before set iff the goal only depends on
	% goals which are before the recursive call or can be moved
	% before the recursive call (member of the before set).
	%
:- pred before(goal_id::in, int::in, goal_store::in, sets::in,
		bool::in, vartypes::in, module_info::in) is semidet.

before(N - I, K, GoalStore, sets(Before, _, _, _, _, _),
		FullyStrict, VarTypes, ModuleInfo) :-
	goal_store__lookup(GoalStore, N - I, LaterGoal - LaterInstMap),
	(
		member_lessthan_goalid(GoalStore, N - I, N - J,
				EarlierGoal - EarlierInstMap),
		not goal_util__can_reorder_goals(ModuleInfo, VarTypes,
			FullyStrict, EarlierInstMap, EarlierGoal,
			LaterInstMap, LaterGoal)
	)
	=>
	(
		set__member(N - J, set_upto(N, K-1) `union` Before)
	).

	% 
	% A goal is a member of the assoc set iff the goal only depends
	% on goals upto and including the recursive call and goals which
	% can be moved before the recursive call (member of the before
	% set) AND the goal is associative.
	%  
:- pred assoc(goal_id::in, int::in, goal_store::in, sets::in, bool::in,
		vartypes::in, module_info::in) is semidet.

assoc(N - I, K, GoalStore, sets(Before, _, _, _, _, _),
		FullyStrict, VarTypes, ModuleInfo) :-
	goal_store__lookup(GoalStore, N - I, LaterGoal - LaterInstMap),
	LaterGoal = call(PredId, ProcId, Args, _, _, _) - _,
	is_associative(PredId, ProcId, ModuleInfo, Args, _),
	(
		member_lessthan_goalid(GoalStore, N - I, _N - J,
				EarlierGoal - EarlierInstMap),
		not goal_util__can_reorder_goals(ModuleInfo, VarTypes,
			FullyStrict, EarlierInstMap, EarlierGoal,
			LaterInstMap, LaterGoal)
	)
	=>
	(
		set__member(N - J, set_upto(N, K) `union` Before)
	).

	% 
	% A goal is a member of the construct set iff the goal only depends
	% on goals upto and including the recursive call and goals which
	% can be moved before the recursive call (member of the before
	% set) AND the goal is construction unification.
	%
:- pred construct(goal_id::in, int::in, goal_store::in, sets::in,
		bool::in, vartypes::in, module_info::in) is semidet.

construct(N - I, K, GoalStore, sets(Before, _, _, Construct, _, _),
		FullyStrict, VarTypes, ModuleInfo) :-
	goal_store__lookup(GoalStore, N - I, LaterGoal - LaterInstMap),
	LaterGoal = unify(_, _, _, Unify, _) - _GoalInfo,
	Unify = construct(_, _, _, _, _, _, _),
	(
		member_lessthan_goalid(GoalStore, N - I, _N - J,
				EarlierGoal - EarlierInstMap),
		not goal_util__can_reorder_goals(ModuleInfo, VarTypes,
			FullyStrict, EarlierInstMap, EarlierGoal,
			LaterInstMap, LaterGoal)
	)
	=>
	(
		set__member(N - J, set_upto(N, K) `union` Before `union`
				Construct)
	).

	% 
	% A goal is a member of the construct_assoc set iff the goal
	% only depends on goals upto and including the recursive call
	% and goals which can be moved before the recursive call (member
	% of the before set) and goals which are associative AND the
	% goal is construction unification AND the there is only one
	% member of the assoc set which the construction unification
	% depends on AND the construction unification can be expressed
	% as a call to the member of the assoc set which the
	% construction unification depends on.
	%
:- pred construct_assoc(goal_id::in, int::in, goal_store::in, sets::in,
		bool::in, vartypes::in, module_info::in) is semidet.

construct_assoc(N - I, K, GoalStore, sets(Before, Assoc, ConstructAssoc,
		_, _, _), FullyStrict, VarTypes, ModuleInfo) :-
	goal_store__lookup(GoalStore, N - I, LaterGoal - LaterInstMap),
	LaterGoal = unify(_, _, _, Unify, _) - _GoalInfo,
	Unify = construct(_, ConsId, _, _, _, _, _),

	goal_store__all_ancestors(GoalStore, N - I, VarTypes, ModuleInfo,
		FullyStrict, Ancestors),

	set__singleton_set(Assoc `intersect` Ancestors, AssocId),
	goal_store__lookup(GoalStore, AssocId, AssocGoal - _AssocInstMap),
	AssocGoal = call(PredId, _, _, _, _, _) - _,

	is_associative_construction(ConsId, PredId, ModuleInfo),
	(
		member_lessthan_goalid(GoalStore, N - I, _N - J,
				EarlierGoal - EarlierInstMap),
		not goal_util__can_reorder_goals(ModuleInfo, VarTypes,
			FullyStrict, EarlierInstMap, EarlierGoal,
			LaterInstMap, LaterGoal)
	)
	=>
	(
		set__member(N - J, set_upto(N, K) `union` Before `union`
				Assoc `union` ConstructAssoc)
	).

	% 
	% A goal is a member of the update set iff the goal only depends
	% on goals upto and including the recursive call and goals which
	% can be moved before the recursive call (member of the before
	% set) AND the goal updates some state.
	%  
:- pred update(goal_id::in, int::in, goal_store::in, sets::in, bool::in,
		vartypes::in, module_info::in) is semidet.

update(N - I, K, GoalStore, sets(Before, _, _, _, _, _),
		FullyStrict, VarTypes, ModuleInfo) :-
	goal_store__lookup(GoalStore, N - I, LaterGoal - LaterInstMap),
	LaterGoal = call(PredId, ProcId, Args, _, _, _) - _,
	is_update(PredId, ProcId, ModuleInfo, Args, _),
	(
		member_lessthan_goalid(GoalStore, N - I, _N - J,
				EarlierGoal - EarlierInstMap),
		not goal_util__can_reorder_goals(ModuleInfo, VarTypes,
			FullyStrict, EarlierInstMap, EarlierGoal,
			LaterInstMap, LaterGoal)
	)
	=>
	(
		set__member(N - J, set_upto(N, K) `union` Before)
	).

	% 
	% member_lessthan_goalid(GS, IdA, IdB, GB) is true iff the
	% goal_id, IdB, and its associated goal, GB, is a member of the
	% goal_store, GS, and IdB is less than IdA.
	%
:- pred member_lessthan_goalid(goal_store::in, goal_id::in,
		goal_id::out, goal_store__goal::out) is nondet.

member_lessthan_goalid(GoalStore, N - I, N - J, Goal) :-
	goal_store__member(GoalStore, N - J, Goal),
	J < I.

%-----------------------------------------------------------------------------%

:- type assoc
	--->	assoc(
			set(prog_var),		% the associative input args
			prog_var,		% the corresponding output arg
			bool			% is the predicate commutative?
		).


	%
	% If accumulator_is_associative is true, it returns the two 
	% arguments which are associative and the variable which depends
	% on those two arguments, and an indicator of whether or not
	% the predicate is commutative.
	%
:- pred is_associative(pred_id::in, proc_id::in, module_info::in,
		prog_vars::in, assoc::out) is semidet.

is_associative(PredId, ProcId, ModuleInfo, Args, Result) :-
	module_info_pred_proc_info(ModuleInfo, PredId, ProcId,
			PredInfo, _ProcInfo),

	pred_info_get_assertions(PredInfo, Assertions),

	associativity_assertion(set__to_sorted_list(Assertions),
			ModuleInfo, Args, AssociativeVars, OutputVar),

	(
		commutativity_assertion(set__to_sorted_list(Assertions),
				ModuleInfo, Args, _CommutativeVars)
	->
		IsCommutative = yes
	;
		IsCommutative = no
	),
	Result = assoc(AssociativeVars, OutputVar, IsCommutative).


	%
	% associativity_assertion
	%
	% Does there exist one (and only one) associativity assertion for the 
	% current predicate.
	% The 'and only one condition' is required because we currently
	% don't handle the case of predicates which have individual
	% parts which are associative, because then we don't know which
	% variable is descended from which.
	%
:- pred associativity_assertion(list(assert_id)::in, module_info::in,
		prog_vars::in, set(prog_var)::out, prog_var::out) is semidet.

associativity_assertion([AssertId | AssertIds], ModuleInfo, Args0, VarAB,
		OutputVar) :-
	(
		assertion__is_associativity_assertion(AssertId, ModuleInfo,
				Args0, VarA - VarB, OutputVar0)
	->
		\+ associativity_assertion(AssertIds, ModuleInfo, Args0, _, _),
		VarAB = set__list_to_set([VarA, VarB]),
		OutputVar = OutputVar0
	;
		associativity_assertion(AssertIds, ModuleInfo, Args0,
				VarAB, OutputVar)
	).

	%
	% commutativity_assertion
	%
	% Does there exist one (and only one) commutativity assertion for the 
	% current predicate.
	% The 'and only one condition' is required because we currently
	% don't handle the case of predicates which have individual
	% parts which are commutative, because then we don't know which
	% variable is descended from which.
	%
:- pred commutativity_assertion(list(assert_id)::in, module_info::in,
		prog_vars::in, set(prog_var)::out) is semidet.

commutativity_assertion([AssertId | AssertIds], ModuleInfo, Args0,
		PossibleStaticVars) :-
	(
		assertion__is_commutativity_assertion(AssertId, ModuleInfo,
				Args0, StaticVarA - StaticVarB)
	->
		\+ commutativity_assertion(AssertIds, ModuleInfo, Args0, _),
		PossibleStaticVars = set__list_to_set([StaticVarA, StaticVarB])
	;
		commutativity_assertion(AssertIds, ModuleInfo, Args0,
				PossibleStaticVars)
	).

%-----------------------------------------------------------------------------%

	%
	% Does the current predicate update some state?
	%
:- pred is_update(pred_id::in, proc_id::in, module_info::in,
		prog_vars::in, pair(prog_var)::out) is semidet.

is_update(PredId, ProcId, ModuleInfo, Args, ResultStateVars) :-
	module_info_pred_proc_info(ModuleInfo, PredId, ProcId,
			PredInfo, _ProcInfo),

	pred_info_get_assertions(PredInfo, Assertions),

	list__filter_map((pred(AssertId::in, StateVars::out) is semidet :-
			assertion__is_update_assertion(AssertId, ModuleInfo,
					PredId, Args, StateVars)
		),
		set__to_sorted_list(Assertions),
		Result),

		% XXX maybe we should just match on the first result,
		% just in case there is duplicate promises.
	Result = [ResultStateVars].

%-----------------------------------------------------------------------------%

	%
	% Can the construction unification be expressed as a call to the
	% specified predicate.
	%
:- pred is_associative_construction(cons_id::in, pred_id::in,
		module_info::in) is semidet.

is_associative_construction(ConsId, PredId, ModuleInfo) :-
	module_info_pred_info(ModuleInfo, PredId, PredInfo),

	pred_info_get_assertions(PredInfo, Assertions),

	list__filter((pred(AssertId::in) is semidet :-
			assertion__is_construction_equivalence_assertion(
					AssertId, ModuleInfo, ConsId, PredId)
		),
		set__to_sorted_list(Assertions),
		Result),
	Result \= [].

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- type substs
	--->	substs(
			subst,	% acc_var_subst
			subst,	% rec_call_subst
			subst,	% assoc_call_subst
			subst	% update_subst
		).

:- type base
	--->	base(
			set(goal_id),	% goals which initialize update
			set(goal_id),	% goals which initialize assoc
			set(goal_id)	% other goals
		).


	%
	% Stage 2 is responsible for identifying the substitutions which
	% are needed to mimic the unfold/fold process that was used as
	% the justification of the algorithm in the paper.
	% It is also responsible for ensuring that the reordering of
	% arguments doesn't worsen the big-O complexity of the
	% procedure.
	% It also divides the base case into goals that initialize the
	% variables used by the update goals and those used by the assoc
	% goals and then all the rest.
	%
:- pred stage2(goal_id::in, goal_store::in, sets::in,
		prog_vars::in, prog_vars::in, module_info::in, proc_info::in,
		pair(prog_varset, vartypes)::out,
		prog_vars::out, base::out, list(pair(prog_var))::out,
		substs::out, goal_store::out, warnings::out) is semidet.

stage2(N - K, GoalStore, Sets, OutPrime, Out, ModuleInfo, ProcInfo0,
		VarSetVarTypesPair, Accs, BaseCase, BasePairs,
		Substs, CS, Warnings) :-

	Sets = sets(Before0, Assoc, ConstructAssoc, Construct, Update, _Reject),
	Before = Before0 `union` set_upto(N, K-1),

		% Note Update set is not placed in the after set, as the
		% after set is used to determine the variables that need
		% to be accumulated for the associative calls.
	After = Assoc `union` ConstructAssoc `union` Construct,

	P = (pred(Id::in, Set0::in, Set::out) is det :-
		goal_store__lookup(GoalStore, Id, Goal - _InstMap),
		Goal = _GoalExpr - GoalInfo,
		goal_info_get_nonlocals(GoalInfo, NonLocals),
		Set = NonLocals `union` Set0
	),
	list__foldl(P, set__to_sorted_list(Before), set__init, BeforeNonLocals),
	list__foldl(P, set__to_sorted_list(After), set__init, AfterNonLocals),
	InitAccs = BeforeNonLocals `intersect` AfterNonLocals,

	proc_info_varset(ProcInfo0, VarSet0),
	proc_info_vartypes(ProcInfo0, VarTypes0),

	substs_init(set__to_sorted_list(InitAccs), VarSet0, VarTypes0,
			VarSet1, VarTypes1, Substs0),

	process_assoc_set(set__to_sorted_list(Assoc), GoalStore,
			set__list_to_set(OutPrime), ModuleInfo, Substs0,
			VarSet1 - VarTypes1, Substs1, VarSetVarTypesPair1, CS,
			Warnings),

	process_update_set(set__to_sorted_list(Update), GoalStore,
			set__list_to_set(OutPrime), ModuleInfo,
			Substs1, VarSetVarTypesPair1,
			Substs, VarSetVarTypesPair, UpdateOut,
			UpdateAccOut, BasePairs),

	Accs = set__to_sorted_list(InitAccs) `append` UpdateAccOut,

	VarSetVarTypesPair = _ - VarTypes,
	divide_base_case(UpdateOut, Out, GoalStore, VarTypes, ModuleInfo,
			UpdateBase, AssocBase, OtherBase),

	BaseCase = base(UpdateBase, AssocBase, OtherBase).

%-----------------------------------------------------------------------------%

:- pred substs_init(prog_vars::in, prog_varset::in, vartypes::in,
		prog_varset::out, vartypes::out, substs::out) is det.

substs_init(InitAccs, VarSet0, VarTypes0, VarSet, VarTypes, Substs) :-
	map__init(Subst),
	acc_var_subst_init(InitAccs, VarSet0, VarTypes0,
			VarSet, VarTypes, AccVarSubst),
	RecCallSubst = Subst,
	AssocCallSubst = Subst,
	UpdateSubst = Subst,
	Substs = substs(AccVarSubst, RecCallSubst, AssocCallSubst, UpdateSubst).

	%
	% Initialise the acc_var_subst to be from Var to A_Var where
	% Var is a member of InitAccs and A_Var is a fresh variable of
	% the same type of Var.
	%
:- pred acc_var_subst_init(prog_vars::in, prog_varset::in, vartypes::in,
		prog_varset::out, vartypes::out, subst::out) is det.

acc_var_subst_init([], VarSet, VarTypes, VarSet, VarTypes, Subst) :-
	map__init(Subst).
acc_var_subst_init([Var | Vars], VarSet0, VarTypes0,
		VarSet, VarTypes, Subst) :-
	create_new_var(Var, "A_", AccVar, VarSet0 - VarTypes0,
			VarSet1 - VarTypes1),
	acc_var_subst_init(Vars, VarSet1, VarTypes1, VarSet, VarTypes, Subst0),

	map__det_insert(Subst0, Var, AccVar, Subst).

	%
	% Create a fresh variable which is the same type as the old
	% variable and has the same name except that it begins with the
	% prefix.
	%
:- pred create_new_var(prog_var::in, string::in, prog_var::out,
		pair(prog_varset, vartypes)::in,
		pair(prog_varset, vartypes)::out) is det.

create_new_var(OldVar, Prefix, NewVar, VarSet0 - VarTypes0,
		VarSet - VarTypes) :-
	varset__lookup_name(VarSet0, OldVar, OldName),
	string__append(Prefix, OldName, NewName),
	varset__new_named_var(VarSet0, NewName, NewVar, VarSet),
	map__lookup(VarTypes0, OldVar, Type),
	map__det_insert(VarTypes0, NewVar, Type, VarTypes).
	
%-----------------------------------------------------------------------------%

	%
	% For each member of the assoc set determine the substitutions
	% needed, and also check the efficiency of the procedure isn't
	% worsened by reordering the arguments to a call.
	%
:- pred process_assoc_set(list(goal_id)::in, goal_store::in, set(prog_var)::in,
		module_info::in, substs::in, pair(prog_varset, vartypes)::in,
		substs::out, pair(prog_varset, vartypes)::out,
		goal_store::out, warnings::out) is semidet.

process_assoc_set([], _GS, _OutPrime, _ModuleInfo, Substs, Types,
		Substs, Types, CS, []) :-
			goal_store__init(CS).
process_assoc_set([Id | Ids], GS, OutPrime, ModuleInfo, Substs0, Types0,
		Substs, Types, CS, Warnings) :-
	Substs0 = substs(AccVarSubst, RecCallSubst0, AssocCallSubst0,
			UpdateSubst),

	lookup_call(GS, Id, Goal - InstMap),

	Goal = call(PredId, ProcId, Args, _, _, _) - GoalInfo,
	is_associative(PredId, ProcId, ModuleInfo, Args, AssocInfo),
	AssocInfo = assoc(Vars, AssocOutput, IsCommutative),
	set__singleton_set(Vars `intersect` OutPrime, DuringAssocVar),
	set__singleton_set(Vars `difference` (Vars `intersect` OutPrime),
			BeforeAssocVar),

	map__lookup(AccVarSubst, BeforeAssocVar, AccVar),
	create_new_var(BeforeAssocVar, "NewAcc_", NewAcc, Types0, Types1),


	map__det_insert(AssocCallSubst0, DuringAssocVar, AccVar,
			AssocCallSubst1),
	map__det_insert(AssocCallSubst1, AssocOutput, NewAcc,
			AssocCallSubst),
	map__det_insert(RecCallSubst0, DuringAssocVar, AssocOutput,
			RecCallSubst1),
	map__det_insert(RecCallSubst1, BeforeAssocVar, NewAcc, RecCallSubst),

	Substs1 = substs(AccVarSubst, RecCallSubst, AssocCallSubst,
			UpdateSubst),

		% ONLY swap the order of the variables if the goal is
		% associative and not commutative.
	(
		IsCommutative = yes,
		CSGoal = Goal - InstMap,
		Warning = []
	;
		IsCommutative = no,

			% Ensure that the reordering doesn't cause a
			% efficiency problem
		module_info_pred_info(ModuleInfo, PredId, PredInfo),
		pred_info_module(PredInfo, ModuleName),
		pred_info_name(PredInfo, PredName),
		pred_info_arity(PredInfo, Arity),
		(
			has_heuristic(ModuleName, PredName, Arity)
		->
				% Only do the transformation if the
				% accumulator variable is *not* in a
				% position where it will control the
				% running time of the predicate.

			heuristic(ModuleName, PredName, Arity, Args,
					PossibleDuringAssocVars),
			set__member(DuringAssocVar, PossibleDuringAssocVars),
			Warning = []
		;
			goal_info_get_context(GoalInfo, ProgContext),
			Warning = [warn(ProgContext, PredId,
					BeforeAssocVar, DuringAssocVar)]

		),

			% Swap the arguments.
		[A,B] = set__to_sorted_list(Vars),
		map__from_assoc_list([A-B, B-A], Subst),
		goal_util__rename_vars_in_goal(Goal, Subst, SwappedGoal),
		CSGoal = SwappedGoal - InstMap
	),

	process_assoc_set(Ids, GS, OutPrime, ModuleInfo, Substs1, Types1,
			Substs, Types, CS0, Warnings0),

	goal_store__det_insert(CS0, Id, CSGoal, CS),
	list__append(Warnings0, Warning, Warnings).


:- pred has_heuristic(module_name::in, string::in, arity::in) is semidet.

has_heuristic(unqualified("list"), "append", 3).


	%
	% heuristic returns the set of which head variables are
	% important in the running time of the predicate.
	%
:- pred heuristic(module_name::in, string::in, arity::in, prog_vars::in,
		set(prog_var)::out) is semidet.

heuristic(unqualified("list"), "append", 3, [_Typeinfo, A, _B, _C], Set) :-
	set__list_to_set([A], Set).

%-----------------------------------------------------------------------------%

	%
	% For each member of the update set determine the substitions
	% needed (creating the accumulator variables when needed).
	% Also associate with each Output variable which accumulator
	% variable to get the result from.
	%
:- pred process_update_set(list(goal_id)::in, goal_store::in, set(prog_var)::in,
		module_info::in, substs::in, pair(prog_varset, vartypes)::in,
		substs::out, pair(prog_varset, vartypes)::out,
		prog_vars::out, prog_vars::out,
		list(pair(prog_var))::out) is semidet.

process_update_set([], _GS, _OutPrime, _ModuleInfo, Substs, Types, Substs,
		Types, [], [], []).
process_update_set([Id | Ids], GS, OutPrime, ModuleInfo, Substs0, Types0,
		Substs, Types, StateOutputVars, Accs, BasePairs) :-

	Substs0 = substs(AccVarSubst0, RecCallSubst0, AssocCallSubst,
				UpdateSubst0),
	lookup_call(GS, Id, Goal - _InstMap),

	Goal = call(PredId, ProcId, Args, _, _, _) - _GoalInfo,
	is_update(PredId, ProcId, ModuleInfo, Args, StateVarA - StateVarB),

	(
		set__member(StateVarA, OutPrime)
	->
		StateInputVar = StateVarA,
		StateOutputVar = StateVarB
	;
		StateInputVar = StateVarB,
		StateOutputVar = StateVarA
	),

	create_new_var(StateInputVar, "Acc_", Acc0, Types0, Types1),
	create_new_var(StateOutputVar, "Acc_", Acc, Types1, Types2),

	map__det_insert(UpdateSubst0, StateInputVar, Acc0, UpdateSubst1),
	map__det_insert(UpdateSubst1, StateOutputVar, Acc, UpdateSubst),

	map__det_insert(RecCallSubst0, StateInputVar, StateOutputVar,
			RecCallSubst),

	map__det_insert(AccVarSubst0, Acc, Acc0, AccVarSubst),

	Substs1 = substs(AccVarSubst, RecCallSubst, AssocCallSubst,
				UpdateSubst),

	process_update_set(Ids, GS, OutPrime, ModuleInfo, Substs1, Types2,
		Substs, Types, StateOutputVars0, Accs0, BasePairs0),

		% Rather then concatenating to start of the list we
		% concatenate to the end of the list.  This allows the
		% accumulator introduction to be applied as the
		% heuristic will succeed (remember after transforming the
		% two input variables will have their order swapped, so
		% they must be in the inefficient order to start with)

	append(StateOutputVars0, [StateOutputVar], StateOutputVars),
	append(Accs0, [Acc], Accs),
	append(BasePairs0, [StateOutputVar - Acc0], BasePairs).

%-----------------------------------------------------------------------------%

	%
	% divide_base_case(UpdateOut, Out, U, A, O)
	%
	% is true iff given the output variables which are instantiated
	% by update goals, UpdateOut, and all the variables that need to
	% be accumulated, Out, divide the base case up into three sets,
	% those base case goals which initialize the variables used by
	% update calls, U, those which initialize variables used by
	% assoc calls, A, and the rest of the goals, O.  Note that the
	% sets are not necessarily disjoint, as the result of a goal may
	% be used to initialize a variable in both U and A, so both U
	% and A will contain the same goal_id.
	%
:- pred divide_base_case(prog_vars::in, prog_vars::in, goal_store::in,
		vartypes::in, module_info::in, set(goal_id)::out,
		set(goal_id)::out, set(goal_id)::out) is det.

divide_base_case(UpdateOut, Out, C, VarTypes, ModuleInfo,
		UpdateBase, AssocBase, OtherBase) :-
	list__delete_elems(Out, UpdateOut, AssocOut),

	list__map(related(C, VarTypes, ModuleInfo), UpdateOut, UpdateBaseList),
	list__map(related(C, VarTypes, ModuleInfo), AssocOut, AssocBaseList),
	UpdateBase = set__power_union(set__list_to_set(UpdateBaseList)),
	AssocBase = set__power_union(set__list_to_set(AssocBaseList)),

	Set = base_case_ids_set(C) `difference` (UpdateBase `union` AssocBase),
	set__to_sorted_list(Set, List),

	list__map((pred(GoalId::in, Ancestors::out) is det :-
			goal_store__all_ancestors(C, GoalId, VarTypes,
				ModuleInfo, no, Ancestors)
		), List, OtherBaseList),
	
	OtherBase = set__list_to_set(List) `union`
			(base_case_ids_set(C) `intersect`
			set__power_union(set__list_to_set(OtherBaseList))).

	% 
	% related(GS, MI, V, Ids)
	%
	% Return all the goal_ids, Ids, which are needed to initialize
	% the variable, V, from the goal store, GS.
	%
:- pred related(goal_store::in, vartypes::in, module_info::in, prog_var::in,
		set(goal_id)::out) is det.

related(GS, VarTypes, ModuleInfo, Var, Related) :-
	solutions((pred(Key::out) is nondet :-
			goal_store__member(GS, Key, Goal - InstMap0),
			Key = base - _,
			Goal = _GoalExpr - GoalInfo,
			goal_info_get_instmap_delta(GoalInfo, InstMapDelta),
			apply_instmap_delta(InstMap0, InstMapDelta, InstMap),
			instmap_changed_vars(InstMap0, InstMap, VarTypes,
				ModuleInfo, ChangedVars),
			set__singleton_set(ChangedVars, Var)
		), Ids),
	(
		Ids = [Id]
	->
		goal_store__all_ancestors(GS, Id, VarTypes, ModuleInfo, no,
			Ancestors),
		list__filter((pred((base - _)::in) is semidet),
				set__to_sorted_list(set__insert(Ancestors, Id)),
				RelatedList),
		Related = set__list_to_set(RelatedList)
	;
		error("accumulator:related")
	).
	
%-----------------------------------------------------------------------------%

:- inst call = bound(call(ground, ground, ground, ground, ground, ground)).
:- inst hlds_call = bound('-'(call, ground)).
:- inst call_goal = bound('-'(hlds_call, ground)).

	% Do a goal_store__lookup where the result is known to be a
	% call.
:- pred lookup_call(goal_store, goal_id, goal_store__goal) is det.
:- mode lookup_call(in, in, out(call_goal)) is det.

lookup_call(GoalStore, Id, Call - InstMap) :-
	goal_store__lookup(GoalStore, Id, Goal - InstMap),
	(
		Goal = call(_, _, _, _, _, _) - _
	->
		Call = Goal
	;
		error("accumulator__lookup_call: not a call.")
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	%
	% stage3 creates the accumulator version of the predicate using
	% the substitutions determined in stage2, it also redefines the
	% original procedure to call the accumulator version of the
	% procedure.
	%
:- pred stage3(goal_id::in, prog_vars::in, prog_varset::in, vartypes::in,
		goal_store::in, goal_store::in, substs::in, subst::in,
		subst::in, base::in, list(pair(prog_var))::in, sets::in,
		top_level::in, pred_info::in, proc_info::in,
		module_info::in, proc_info::out, module_info::out) is
		det.

stage3(RecCallId, Accs, VarSet, VarTypes, C, CS, Substs,
		HeadToCallSubst, CallToHeadSubst, BaseCase, BasePairs,
		Sets, TopLevel, OrigPredInfo, OrigProcInfo0,
		ModuleInfo0, OrigProcInfo, ModuleInfo) :-

	acc_proc_info(Accs, VarSet, VarTypes, Substs,
			OrigProcInfo0, AccTypes, AccProcInfo),
	acc_pred_info(AccTypes, AccProcInfo, OrigPredInfo, AccProcId,
			AccPredInfo),

	pred_info_name(AccPredInfo, AccPredName),
	AccName = unqualified(AccPredName),

	module_info_get_predicate_table(ModuleInfo0, PredTable0),
	predicate_table_insert(PredTable0, AccPredInfo, AccPredId, PredTable),
	module_info_set_predicate_table(ModuleInfo0, PredTable, ModuleInfo1),

	create_goal(RecCallId, Accs, AccPredId, AccProcId, AccName, Substs,
			HeadToCallSubst, CallToHeadSubst, BaseCase,
			BasePairs, Sets, C, CS, OrigBaseGoal,
			OrigRecGoal, AccBaseGoal, AccRecGoal),

	proc_info_goal(OrigProcInfo0, OrigGoal0),

	top_level(TopLevel, OrigGoal0, OrigBaseGoal, OrigRecGoal,
			AccBaseGoal, AccRecGoal, OrigGoal, AccGoal),

	proc_info_set_goal(OrigProcInfo0, OrigGoal, OrigProcInfo1),
	proc_info_set_varset(OrigProcInfo1, VarSet, OrigProcInfo2),
	proc_info_set_vartypes(OrigProcInfo2, VarTypes, OrigProcInfo3),

	requantify_proc(OrigProcInfo3, OrigProcInfo),

	update_accumulator_pred(AccPredId, AccProcId, AccGoal,
			ModuleInfo1, ModuleInfo).

%-----------------------------------------------------------------------------%

	%
	% acc_proc_info
	%
	% Construct a proc_info for the introduced predicate.
	%
:- pred acc_proc_info(prog_vars::in, prog_varset::in, vartypes::in,
		substs::in, proc_info::in,
		list(type)::out, proc_info::out) is det.

acc_proc_info(Accs0, VarSet, VarTypes, Substs,
		OrigProcInfo, AccTypes, AccProcInfo) :-

		% ProcInfo Stuff that must change.
	proc_info_headvars(OrigProcInfo, HeadVars0),
	proc_info_argmodes(OrigProcInfo, HeadModes0),

	proc_info_inst_varset(OrigProcInfo, InstVarSet),
	proc_info_inferred_determinism(OrigProcInfo, Detism),
	proc_info_goal(OrigProcInfo, Goal),
	proc_info_context(OrigProcInfo, Context),
	proc_info_typeinfo_varmap(OrigProcInfo, TVarMap),
	proc_info_typeclass_info_varmap(OrigProcInfo, TCVarsMap),
	proc_info_is_address_taken(OrigProcInfo, IsAddressTaken),
	

	Substs = substs(AccVarSubst, _RecCallSubst, _AssocCallSubst,
			_UpdateSubst),
	list__map(map__lookup(AccVarSubst), Accs0, Accs),

		% We place the extra accumulator variables at the start,
		% because placing them at the end breaks the convention
		% that the last variable of a function is the output
		% variable.
	HeadVars = Accs `append` HeadVars0,

		% XXX we don't want to use the inst of the var as it can
		% be more specific than it should be. ie int_const(1)
		% when it should be any integer.
		% However this will no longer handle partially
		% instantiated data structures.
	Inst = ground(shared, none),
	inst_lists_to_mode_list([Inst], [Inst], Mode),
	list__duplicate(list__length(Accs), list__det_head(Mode), AccModes),
	HeadModes = AccModes `append` HeadModes0,

	list__map(map__lookup(VarTypes), Accs, AccTypes),

	proc_info_create(VarSet, VarTypes, HeadVars, HeadModes, InstVarSet,
		Detism, Goal, Context, TVarMap, TCVarsMap, IsAddressTaken,
		AccProcInfo).

%-----------------------------------------------------------------------------%

	%
	% acc_pred_info
	%
	% Construct the pred_info for the introduced predicate
	%
:- pred acc_pred_info(list(type)::in, proc_info::in, pred_info::in,
		proc_id::out, pred_info::out) is det.

acc_pred_info(NewTypes, NewProcInfo, PredInfo, NewProcId, NewPredInfo) :-
		
		% PredInfo stuff that must change.
	pred_info_arg_types(PredInfo, TypeVarSet, ExistQVars, Types0),

	pred_info_module(PredInfo, ModuleName),
	pred_info_name(PredInfo, Name),
	Cond = true,
	pred_info_context(PredInfo, PredContext),
	pred_info_get_markers(PredInfo, Markers),
	pred_info_get_is_pred_or_func(PredInfo, PredOrFunc),
	pred_info_get_class_context(PredInfo, ClassContext),
	pred_info_get_aditi_owner(PredInfo, Owner),

	set__init(Assertions),

	proc_info_context(NewProcInfo, Context),
	term__context_line(Context, Line),
	Counter = 0,

	Types = NewTypes `append` Types0,

	make_pred_name_with_context(ModuleName, "AccFrom", PredOrFunc, Name,
		Line, Counter, SymName),

	pred_info_create(ModuleName, SymName, TypeVarSet, ExistQVars, Types,
			Cond, PredContext, local, Markers, PredOrFunc,
			ClassContext, Owner, Assertions, NewProcInfo, NewProcId,
			NewPredInfo).

%-----------------------------------------------------------------------------%

	%
	% create_goal creates the new base and recursive case of the
	% original procedure (OrigBaseGoal and OrigRecGoal) and the base
	% and recursive cases of accumulator version (AccBaseGoal and
	% AccRecGoal).
	%
:- pred create_goal(goal_id, prog_vars, pred_id, proc_id, sym_name,
		substs, subst, subst, base, list(pair(prog_var)), sets,
		goal_store, goal_store, hlds_goal, hlds_goal, hlds_goal,
		hlds_goal).
:- mode create_goal(in, in, in, in, in, in,
		in, in, in, in, in, in, in, out, out, out, out) is det.

create_goal(RecCallId, Accs, AccPredId, AccProcId, AccName, Substs,
		HeadToCallSubst, CallToHeadSubst, BaseIds, BasePairs,
		Sets, C, CS, OrigBaseGoal, OrigRecGoal, AccBaseGoal,
		AccRecGoal) :-

	lookup_call(C, RecCallId, OrigCall - _InstMap),

	Call = create_acc_call(OrigCall, Accs, AccPredId,
			AccProcId, AccName),

	create_orig_goal(Call, Substs, HeadToCallSubst, CallToHeadSubst,
			BaseIds, Sets, C, OrigBaseGoal, OrigRecGoal),

	create_acc_goal(Call, Substs, HeadToCallSubst, BaseIds, BasePairs,
			Sets, C, CS, AccBaseGoal, AccRecGoal).


	%
	% create_acc_call takes the original call and generates a call
	% to the accumulator version of the call, which can have the
	% substitutions applied to it easily.
	%
:- func create_acc_call(hlds_goal, prog_vars, pred_id, proc_id,
		sym_name) = hlds_goal.
:- mode create_acc_call(in(hlds_call), in, in, in, in) = out(hlds_call) is det.

create_acc_call(OrigCall, Accs, AccPredId, AccProcId, AccName) = Call :-
	OrigCall = call(_PredId, _ProcId, Args, Builtin, Context, _Name) - GI,
	Call = call(AccPredId, AccProcId, Accs `append` Args,
			Builtin, Context, AccName) - GI.

	%
	% Create the goals which are to replace the original predicate.
	%
:- pred create_orig_goal(hlds_goal::in, substs::in, subst::in,
		subst::in, base::in, sets::in, goal_store::in,
		hlds_goal::out, hlds_goal::out) is det.

create_orig_goal(Call, Substs, HeadToCallSubst, CallToHeadSubst,
		BaseIds, Sets, C, OrigBaseGoal, OrigRecGoal) :-

	Substs = substs(_AccVarSubst, _RecCallSubst, _AssocCallSubst,
			UpdateSubst),

	BaseIds = base(UpdateBase, _AssocBase, _OtherBase),
	Sets = sets(Before, _Assoc, _ConstructAssoc,
			_Construct, Update, _Reject),

	U = create_new_orig_recursive_goals(UpdateBase, Update,
			HeadToCallSubst, UpdateSubst, C),

	goal_util__rename_vars_in_goal(Call, CallToHeadSubst, BaseCall),

	Cbefore = goal_list(set__to_sorted_list(Before), C),

	Uupdate = goal_list(set__to_sorted_list(UpdateBase) `append`
			set__to_sorted_list(Update), U),
	Cbase = goal_list(base_case_ids(C), C),

	calculate_goal_info(conj(Cbefore `append` Uupdate `append`
			[BaseCall]), OrigRecGoal),
	calculate_goal_info(conj(Cbase), OrigBaseGoal).


	% 
	% Create the goals which are to go in the new accumulator
	% version of the predicate.
	%
:- pred create_acc_goal(hlds_goal::in, substs::in, subst::in, base::in,
		list(pair(prog_var))::in, sets::in, goal_store::in,
		goal_store::in, hlds_goal::out, hlds_goal::out) is det.

create_acc_goal(Call, Substs, HeadToCallSubst,
		BaseIds, BasePairs, Sets, C, CS, AccBaseGoal, AccRecGoal) :-
	Substs = substs(AccVarSubst, RecCallSubst, AssocCallSubst,
			UpdateSubst),

	BaseIds = base(_UpdateBase, AssocBase, OtherBase),
	Sets = sets(Before, Assoc, ConstructAssoc,
			Construct, Update, _Reject),

	goal_util__rename_vars_in_goal(Call, RecCallSubst, RecCall),

	Cbefore = goal_list(set__to_sorted_list(Before), C),

		% Create the goals which will be used in the new
		% recursive case.
	R = create_new_recursive_goals(Assoc, Construct `union` ConstructAssoc,
			Update, AssocCallSubst, AccVarSubst,
			UpdateSubst, C, CS),

	Rassoc = goal_list(set__to_sorted_list(Assoc), R),
	Rupdate = goal_list(set__to_sorted_list(Update), R),
	Rconstruct = goal_list(set__to_sorted_list(Construct `union`
			ConstructAssoc), R),

		% Create the goals which will be used in the new
		% base case.
	B = create_new_base_goals(Assoc `union` Construct `union`
			ConstructAssoc, C, AccVarSubst, HeadToCallSubst),
	Bafter = set__to_sorted_list(Assoc `union`
			Construct `union` ConstructAssoc),

	BaseCase = goal_list(set__to_sorted_list(AssocBase `union` OtherBase)
			`append` Bafter, B),

	list__map(acc_unification, BasePairs, UpdateBase),
	
	calculate_goal_info(conj(Cbefore `append` Rassoc `append`
			Rupdate `append` [RecCall] `append` Rconstruct),
			AccRecGoal),
	calculate_goal_info(conj(UpdateBase `append` BaseCase), AccBaseGoal).


	%
	% Create the U set of goals (those that will be used in the
	% original recursive case) by renaming all the goals which are
	% used to initialize the update state variable using the
	% head_to_call followed by the update_subst, and rename all the
	% update goals using the update_subst.
	%
:- func create_new_orig_recursive_goals(set(goal_id), set(goal_id),
		subst, subst, goal_store) = goal_store.

create_new_orig_recursive_goals(UpdateBase, Update, HeadToCallSubst,
		UpdateSubst, C)
	= rename(set__to_sorted_list(Update), UpdateSubst, C, Ubase) :-
	Ubase = rename(set__to_sorted_list(UpdateBase),
			chain_subst(HeadToCallSubst, UpdateSubst), C,
			goal_store__init).

	%
	% Create the R set of goals (those that will be used in the new
	% recursive case) by renaming all the members of assoc in CS
	% using assoc_call_subst and all the members of (construct U
	% construct_assoc) in C with acc_var_subst.
	%
:- func create_new_recursive_goals(set(goal_id), set(goal_id), set(goal_id),
		subst, subst, subst,
		goal_store, goal_store) = goal_store.

create_new_recursive_goals(Assoc, Constructs, Update,
		AssocCallSubst, AccVarSubst, UpdateSubst, C, CS)
	= rename(set__to_sorted_list(Constructs), AccVarSubst, C, RBase) :-
	RBase0 = rename(set__to_sorted_list(Assoc), AssocCallSubst, CS,
			goal_store__init),
	RBase = rename(set__to_sorted_list(Update), UpdateSubst, C, RBase0).

	%
	% Create the B set of goals (those that will be used in the new
	% base case) by renaming all the base case goals of C with
	% head_to_call and all the members of (assoc U construct U
	% construct_assoc) of C with acc_var_subst.
	%
:- func create_new_base_goals(set(goal_id), goal_store, subst, subst) =
		goal_store.

create_new_base_goals(Ids, C, AccVarSubst, HeadToCallSubst)
	= rename(set__to_sorted_list(Ids), AccVarSubst, C, Bbase) :-
	Bbase =	rename(base_case_ids(C), HeadToCallSubst, C, goal_store__init).


	%
	% acc_unification(O-A, G)
	%
	% is true if G represents the assignment unification Out = Acc.
	%
:- pred acc_unification(pair(prog_var)::in, hlds_goal::out) is det.
            
acc_unification(Out - Acc, Goal) :-
	out_mode(LHSMode),
	in_mode(RHSMode),
	UniMode = LHSMode - RHSMode,

	Context = unify_context(explicit, []),
	Expr = unify(Out, var(Acc), UniMode, assign(Out,Acc), Context),
	set__list_to_set([Out,Acc], NonLocalVars),
	instmap_delta_from_assoc_list([Out - ground(shared, none)],
		InstMapDelta),

	goal_info_init(NonLocalVars, InstMapDelta, det, Info),
					   
	Goal = Expr - Info.


%-----------------------------------------------------------------------------%

	%
	% Given the top level structure of the goal create new version
	% with new base and recursive cases plugged in.
	%
:- pred top_level(top_level::in, hlds_goal::in,
		hlds_goal::in, hlds_goal::in, hlds_goal::in,
		hlds_goal::in, hlds_goal::out, hlds_goal::out) is det.

top_level(switch_base_rec, Goal, OrigBaseGoal, OrigRecGoal,
		NewBaseGoal, NewRecGoal, OrigGoal, NewGoal) :-
	(
		Goal = switch(Var, CanFail, Cases0) - GoalInfo,
		Cases0 = [case(IdA, _), case(IdB, _)]
	->
		OrigCases = [case(IdA, OrigBaseGoal), case(IdB, OrigRecGoal)],
		OrigGoal = switch(Var, CanFail, OrigCases) - GoalInfo,

		NewCases = [case(IdA, NewBaseGoal), case(IdB, NewRecGoal)],
		NewGoal = switch(Var, CanFail, NewCases) - GoalInfo
	;
		error("top_level: not the correct top level")
	).
top_level(switch_rec_base, Goal, OrigBaseGoal, OrigRecGoal,
		NewBaseGoal, NewRecGoal, OrigGoal, NewGoal) :-
	(
		Goal = switch(Var, CanFail, Cases0) - GoalInfo,
		Cases0 = [case(IdA, _), case(IdB, _)]
	->
		OrigCases = [case(IdA, OrigRecGoal), case(IdB, OrigBaseGoal)],
		OrigGoal = switch(Var, CanFail, OrigCases) - GoalInfo,

		NewCases = [case(IdA, NewRecGoal), case(IdB, NewBaseGoal)],
		NewGoal = switch(Var, CanFail, NewCases) - GoalInfo
	;
		error("top_level: not the correct top level")
	).
top_level(disj_base_rec, Goal, OrigBaseGoal,
		OrigRecGoal, NewBaseGoal, NewRecGoal, OrigGoal, NewGoal) :-
	(
		Goal = disj(Goals) - GoalInfo,
		Goals = [_, _]
	->
		OrigGoals = [OrigBaseGoal, OrigRecGoal],
		OrigGoal = disj(OrigGoals) - GoalInfo,

		NewGoals = [NewBaseGoal, NewRecGoal],
		NewGoal = disj(NewGoals) - GoalInfo
	;
		error("top_level: not the correct top level")
	).
top_level(disj_rec_base, Goal, OrigBaseGoal,
		OrigRecGoal, NewBaseGoal, NewRecGoal, OrigGoal, NewGoal) :-
	(
		Goal = disj(Goals) - GoalInfo,
		Goals = [_, _]
	->
		OrigGoals = [OrigRecGoal, OrigBaseGoal],
		OrigGoal = disj(OrigGoals) - GoalInfo,

		NewGoals = [NewRecGoal, NewBaseGoal],
		NewGoal = disj(NewGoals) - GoalInfo
	;
		error("top_level: not the correct top level")
	).
top_level(ite_base_rec, Goal, OrigBaseGoal,
		OrigRecGoal, NewBaseGoal, NewRecGoal, OrigGoal, NewGoal) :-
	(
		Goal = if_then_else(Vars, If, _, _) - GoalInfo
	->
		OrigGoal = if_then_else(Vars, If,
				OrigBaseGoal, OrigRecGoal) - GoalInfo,

		NewGoal = if_then_else(Vars, If,
				NewBaseGoal, NewRecGoal) - GoalInfo
	;
		error("top_level: not the correct top level")
	).
top_level(ite_rec_base, Goal, OrigBaseGoal,
		OrigRecGoal, NewBaseGoal, NewRecGoal, OrigGoal, NewGoal) :-
	(
		Goal = if_then_else(Vars, If, _, _) - GoalInfo
	->
		OrigGoal = if_then_else(Vars, If,
				OrigRecGoal, OrigBaseGoal) - GoalInfo,

		NewGoal = if_then_else(Vars, If,
				NewRecGoal, NewBaseGoal) - GoalInfo
	;
		error("top_level: not the correct top level")
	).

%-----------------------------------------------------------------------------%

	%
	% update_accumulator_pred
	%
	% Place the accumulator version of the predicate in the
	% module_info structure.
	%
:- pred update_accumulator_pred(pred_id::in, proc_id::in,
		hlds_goal::in, module_info::in, module_info::out) is det.

update_accumulator_pred(NewPredId, NewProcId, AccGoal,
		ModuleInfo0, ModuleInfo) :-
	module_info_pred_proc_info(ModuleInfo0, NewPredId, NewProcId,
			PredInfo, ProcInfo0),

	proc_info_set_goal(ProcInfo0, AccGoal, ProcInfo1),

	requantify_proc(ProcInfo1, ProcInfo),
	module_info_set_pred_proc_info(ModuleInfo0, NewPredId, NewProcId,
		PredInfo, ProcInfo, ModuleInfo).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	%
	% rename(Ids, Subst, From, Initial)
	%
	% return a goal_store, Final, which is the result of looking up each
	% member of set of goal_ids, Ids, in the goal_store, From,
	% applying the substitution and then storing the goal into
	% the goal_store, Initial.
	%
:- func rename(list(goal_id), subst, goal_store, goal_store) = goal_store.

rename(Ids, Subst, From, Initial) = Final :-
	list__foldl((pred(Id::in, GS0::in, GS::out) is det :-
			goal_store__lookup(From, Id, Goal0 - InstMap),
			goal_util__rename_vars_in_goal(Goal0, Subst, Goal),
			goal_store__det_insert(GS0, Id, Goal - InstMap, GS)
		), Ids, Initial, Final).

	%
	% Return all the goal_ids which belong in the base case.
	%
:- func base_case_ids(goal_store) = list(goal_id).

base_case_ids(GS) = Base :-
	solutions((pred(Key::out) is nondet :-
			goal_store__member(GS, Key, _Goal),
			Key = base - _
		), Base).

:- func base_case_ids_set(goal_store) = set(goal_id).

base_case_ids_set(GS) = set__list_to_set(base_case_ids(GS)).

	%
	% Given a list of goal_ids, return the list of hlds_goals from
	% the goal_store.
	%
:- func goal_list(list(goal_id), goal_store) = hlds_goals.

goal_list(Ids, GS) = Goals :-
	list__map((pred(Key::in, G::out) is det :-
			goal_store__lookup(GS, Key, G - _)
		), Ids, Goals).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred calculate_goal_info(hlds_goal_expr::in, hlds_goal::out) is det.

calculate_goal_info(GoalExpr, GoalExpr - GoalInfo) :-
	(
		GoalExpr = conj(GoalList)
	->
		goal_list_nonlocals(GoalList, NonLocals),
		goal_list_instmap_delta(GoalList, InstMapDelta),
		goal_list_determinism(GoalList, Determinism),

		goal_info_init(NonLocals, InstMapDelta, Determinism, GoalInfo)
	;
		error("calculate_goal_info: not a conj.")
	).
	
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% The number which indicates the base case.
:- func base = int.
base = 2.

	% The number which indicates the recursive case.
:- func rec = int.
rec = 1.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- func reverse_subst(subst) = subst.

reverse_subst(Subst0) = Subst :-
	map__to_assoc_list(Subst0, List0),
	assoc_list__reverse_members(List0, List),
	map__from_assoc_list(List, Subst).

:- func chain_subst(subst, subst) = subst.

chain_subst(AtoB, BtoC) = AtoC :-
	map__keys(AtoB, Keys),
	chain_subst_2(Keys, AtoB, BtoC, AtoC).

:- pred chain_subst_2(list(A)::in, map(A, B)::in, map(B, C)::in,
		map(A, C)::out) is det.

chain_subst_2([], _, _, AtoC) :-
	map__init(AtoC).
chain_subst_2([A|As], AtoB, BtoC, AtoC) :-
	chain_subst_2(As, AtoB, BtoC, AtoC0),
	map__lookup(AtoB, A, B),
	(
		map__search(BtoC, B, C)
	->
		map__det_insert(AtoC0, A, C, AtoC)
	;
		AtoC = AtoC0
	).


%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
