%-----------------------------------------------------------------------------%
% Copyright (C) 1999-2000 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Module:	accumulator
% Main authors: petdr
%
% Attempts to transform a single proc to a tail recursive form by
% introducing accumlators.
%
% The transformation is described more fully in the paper
% "Making Mercury programs tail recursive", which will be available 
% from the Mercury web site.
%
% Basically the transformation is to locate predicates in the following
% form.  The transformation also handles if-then-elses since they can
% conceptually be treated like a switch, we just need to be a little bit
% more careful about what happens in the condition.
%
% p(X,Ys) :-
%         minimal(X),
%         base(Ys).
% p(X, Ys) :-
%         decompose(X, Xhead, Xrest),
%         process(Xhead, Hs),
%         p(Xrest, Y0s),
%         compose(Hs, Y0s, Ys).
%
% and transform them into this form
%
% p(X,Ys) :-
%         minimal(X),
%         base(Ys).
% p(X, Ys) :-
%         decompose(X, Xhead, Xrest),
%         process(Xhead, Hs),
%         p'(Xrest, Ys, Hs).
% 
% p'(X, Ys, As) :-
%         minimal(X),
%         base(Y0s),
%         compose(As, Y0s, Ys).
% p'(X, Ys, As) :-
%         decompose(X, Xhead, Xrest),
%         process(Xhead, Hs),
%         compose(As, Hs, A1s),
%         p'(Xrest, Ys, A1s).
%
% Any variable that ends with an 's' represents a set of variables.
% The constraint on the transformation is that the compose goal must
% obey the following law
%      
%	some [BC] (compose(I, B, C, BC), compose(I, A, BC, ABC))
%		<=> some [AB] (compose(I, A, B, AB), compose(I, AB, C, ABC))
%
% The above law denotes that the compose goal must be associative, or
% maybe more intuitively the compose goal can construct an answer processing
% in a right to left manner or a left to right manner.
%
% Currently the knowledge of which goals are associative is hard-wired
% into this module, at a later date we should add the ability to add
% pragmas to supply this information.
%
% Another subtlety is that making the code tail recursive doesn't
% necessarily improve the efficiency of code.  Note that the call 
% to compose in the accumulator version of the code has Hs located
% in a different position.  For append(in, in, out) it is the first
% argument which controls the complexity of append.  So if the compose
% goal was append, the complexity of the predicate as a whole will
% change.  This problem is dealt with by ensuring that only a variable
% that is a member of Hs ends up in the first position of append,
% because in general the variables in Hs are smaller then those in As.
%
% Note that the transformation will leave construction unifications
% after the recursive call if '--optimize-constructor-last-call' is
% enabled.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module accumulator.

:- interface.

:- import_module hlds_module, hlds_pred, io.

:- pred accumulator__process_proc(pred_id::in, proc_id::in, proc_info::in,
		proc_info::out, module_info::in, module_info::out,
		io__state::di, io__state::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module (assertion), error_util, goal_util, globals.
:- import_module hlds_data, hlds_goal, hlds_out, (inst).
:- import_module inst_match, instmap, mode_util, options, prog_data, prog_util.

:- import_module assoc_list, bool, list, map, multi_map.
:- import_module require, set, std_util, term, varset.

%-----------------------------------------------------------------------------%

:- type base_goal
	--->	base(
			hlds_goals
		).

:- type rec_goal
	--->	recursive(
			a_goals,	% Goals inside the condition of an
					% if/then/else
			a_goals,	% Decompose, Process
			a_goal,		% Recursive call
			a_goals		% Compose calls
		).

:- type split_result
	--->	recursive(
			hlds_goals,	% Decompose, Process
			hlds_goal,	% Recursive call
			hlds_goals	% Compose calls
		).

:- type a_goal  == goal(hlds_goal).
:- type a_goals == goal(hlds_goals).

:- type goal(T)
	--->	goal(
			T,		% goal/s
			instmap		% instmap at the start of the goal
		).

:- type top_level
	--->	switch_base_rec
	;	switch_rec_base
	;	disj_base_rec
	;	disj_rec_base
	;	ite_base_rec
	;	ite_rec_base.

:- type subst == map(prog_var, prog_var).

:- type assoc_info
	--->	assoc_info(
			set(prog_var),	% Static set
					% A static variable is one whose
					% value is set before the
					% recursive call.

			set(prog_var),	% Dynamic set
					% The dynamic set is initialised
					% to Y0s.  At the end of the
					% process it will contain all
					% the variables that are
					% constructed using another
					% dynamic variable.
			module_info,
			prev_call_map,
			orig_dynvar_map,
			subst,		% Y0s -> As
			subst,		% Hs -> As
			set(prog_var),	% Ys
			warnings
		).

:- type warning 
			% warn that two prog_vars in call to pred_id
			% at prog_context were swapped.
	--->	w(prog_context, pred_id, prog_var, prog_var).

:- type warnings == list(warning).

	% is the pred commutative?
:- type commutative == bool.

:- type prev_call
	--->	prev_call(
			pred_id,
			proc_id,
			commutative
		).

	% If a variable is constructed from a chain of calls, what are
	% the details of the previous call in the chain.
:- type prev_call_map == map(prog_var, prev_call).
	
	% Given a dynamic variable, from which dynamic variable was it
	% descended (ie which variable in Y0s).
	%
	% For the following call
	%
	% append(R0, ListH, R)
	%
	% The variable ListH is static and R0 is dynamic, therefore R is
	% descended from R0.
	%
:- type orig_dynvar_map == map(prog_var, prog_var).

%-----------------------------------------------------------------------------%


	%
	% accumulator__process_proc
	%
	% Attempt to transform one procedure into a accumulator
	% recursive form.
	%
accumulator__process_proc(PredId, ProcId, ProcInfo0, ProcInfo, 
		ModuleInfo0, ModuleInfo) -->
	globals__io_lookup_bool_option(optimize_constructor_last_call, DoLCO),
	globals__io_lookup_bool_option(fully_strict, FullyStrict),
	(
		{ module_info_pred_info(ModuleInfo0, PredId, PredInfo) },
		{ accumulator__attempt_transform(ProcId, ProcInfo0, PredId,
				PredInfo, DoLCO, FullyStrict, ModuleInfo0,
				Warnings, ProcInfo1, ModuleInfo1) }
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
			( { Warnings = [] } ; { InhibitWarnings = yes } )
		->
			{ ModuleInfo = ModuleInfo1 }
		;
			{ error_util__describe_one_pred_name(ModuleInfo1,
					PredId, PredName) },
			{ pred_info_context(PredInfo, Context) },

			error_util__write_error_pieces(Context, 0,
					[words("In"), words(PredName)]),

			{ proc_info_varset(ProcInfo, VarSet) },
			accumulator__warnings(Warnings, VarSet, ModuleInfo1),

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
					words("a promise declaration,"),
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

	%
	% accumulator__attempt_transform is only true if the current
	% proc has been transformed to call the newly created
	% accumulator proc.
	%
:- pred accumulator__attempt_transform(proc_id::in, proc_info::in,
		pred_id::in, pred_info::in, bool::in, bool::in, module_info::in,
		warnings::out, proc_info::out, module_info::out) is semidet.

accumulator__attempt_transform(ProcId, ProcInfo0, PredId, PredInfo0, DoLCO,
		FullyStrict, ModuleInfo0, Warnings, ProcInfo, ModuleInfo) :-
	proc_info_goal(ProcInfo0, Goal0),
	proc_info_headvars(ProcInfo0, HeadVars),
	proc_info_get_initial_instmap(ProcInfo0, ModuleInfo0, InitialInstMap),

	accumulator__simplify(Goal0, Goal),
	accumulator__rearrange_goal(PredId, ProcId, Goal, InitialInstMap,
			ModuleInfo0, FullyStrict, GoalType, Base, Rec),

	accumulator__create_accumulator_pred(Rec, PredInfo0, ProcInfo0,
			HstoAs_Subst, NewPredId, NewProcId, NewPredName,
			ModuleInfo0, ModuleInfo1),

	accumulator__transform(GoalType, Base, Rec, Goal, DoLCO, FullyStrict,
			ModuleInfo1, HeadVars, HstoAs_Subst, NewPredId,
			NewProcId, NewPredName, OrigGoal, Warnings, AccGoal),

	accumulator__update_accumulator_pred(NewPredId, NewProcId, AccGoal,
			ModuleInfo1, ModuleInfo),

	proc_info_set_goal(ProcInfo0, OrigGoal, ProcInfo).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred accumulator__warnings(list(warning)::in, prog_varset::in,
		module_info::in, io__state::di, io__state::uo) is det.

accumulator__warnings([], _, _) --> [].
accumulator__warnings([W | Ws], VarSet, ModuleInfo) -->
	{ accumulator__warning(W, VarSet, ModuleInfo, Context, Format) },
	error_util__write_error_pieces(Context, 2, Format),
	accumulator__warnings(Ws, VarSet, ModuleInfo).

:- pred accumulator__warning(warning::in, prog_varset::in, module_info::in,
		prog_context::out, list(format_component)::out) is det.

accumulator__warning(w(Context, PredId, VarA, VarB), VarSet, ModuleInfo,
		Context, Formats) :-
	error_util__describe_one_pred_name(ModuleInfo, PredId, PredStr),
	varset__lookup_name(VarSet, VarA, VarAStr),
	varset__lookup_name(VarSet, VarB, VarBStr),
	Formats = [words("warning: the call to"), words(PredStr),
			words("has had the location of the variables"),
			words(VarAStr), words("and"), words(VarBStr),
			words("swapped to allow accumulator introduction.")
			].


%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	%
	% accumulator__simplify
	%
	% Simplify the goal to make it more amenable to introducing
	% accumulators.
	%
	% At the moment all this does is remove any extra disj/conj
	% wrappers around the top level goal.
	%
	% Future work is for this code to rearrange code with multiple
	% base and recursive cases into a single base and recursive
	% case.
	%
:- pred accumulator__simplify(hlds_goal, hlds_goal).
:- mode accumulator__simplify(in, out) is det.

accumulator__simplify(Goal0, Goal) :-
	(
		(
			Goal0 = conj([Goal1]) - _
		;
			Goal0 = disj([Goal1], _) - _
		)
	->
		accumulator__simplify(Goal1, Goal)
	;
		Goal = Goal0
	).


%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	%
	% This predicate is meant to take the original goal and
	% rearrange it into a standard form that can be used in the rest
	% of module.
	%
:- pred accumulator__rearrange_goal(pred_id::in, proc_id::in,
		hlds_goal::in, instmap::in, module_info::in, bool::in,
		top_level::out, base_goal::out, rec_goal::out) is semidet.

accumulator__rearrange_goal(PredId, ProcId, Goal, InitialInstMap, ModuleInfo,
		FullyStrict, Type, Base, Rec) :-
	(
		Goal = switch(_Var, _CanFail, Cases, _StoreMap) - _GoalInfo,
		Cases = [case(_IdA, GoalA), case(_IdB, GoalB)],
		goal_to_conj_list(GoalA, GoalAList),
		goal_to_conj_list(GoalB, GoalBList)
	->
		(
			accumulator__split_recursive_case(PredId, ProcId,
					InitialInstMap, [],
					InitialInstMap, ModuleInfo, FullyStrict,
					GoalAList, Rec0),

				% Make sure that the base case doesn't
				% contain a recursive call.
			\+ accumulator__split_recursive_case(PredId, ProcId,
					InitialInstMap, [],
					InitialInstMap, ModuleInfo, FullyStrict,
					GoalBList, _)
		->
			Type = switch_rec_base,
			Base = base(GoalBList),
			Rec = Rec0
		;
			accumulator__split_recursive_case(PredId, ProcId,
					InitialInstMap, [],
					InitialInstMap, ModuleInfo, FullyStrict,
					GoalBList, Rec0),

				% Make sure that the base case doesn't
				% contain a recursive call.
			\+ accumulator__split_recursive_case(PredId, ProcId,
					InitialInstMap, [],
					InitialInstMap, ModuleInfo, FullyStrict,
					GoalAList, _)
		->
			Type = switch_base_rec,
			Base = base(GoalAList),
			Rec = Rec0
		;
			fail
		)
	;
		Goal = disj(Goals, _SM) - _GoalInfo,
		Goals = [GoalA, GoalB],
		goal_to_conj_list(GoalA, GoalAList),
		goal_to_conj_list(GoalB, GoalBList)
	->
		(
			accumulator__split_recursive_case(PredId, ProcId,
					InitialInstMap, [],
					InitialInstMap, ModuleInfo, FullyStrict,
					GoalAList, Rec0),

				% Make sure that the base case doesn't
				% contain a recursive call.
			\+ accumulator__split_recursive_case(PredId, ProcId,
					InitialInstMap, [],
					InitialInstMap, ModuleInfo, FullyStrict,
					GoalBList, _)
		->
			Type = disj_rec_base,
			Base = base(GoalBList),
			Rec = Rec0
		;
			accumulator__split_recursive_case(PredId, ProcId,
					InitialInstMap, [],
					InitialInstMap, ModuleInfo, FullyStrict,
					GoalBList, Rec0),

				% Make sure that the base case doesn't
				% contain a recursive call.
			\+ accumulator__split_recursive_case(PredId, ProcId,
					InitialInstMap, [],
					InitialInstMap, ModuleInfo, FullyStrict,
					GoalAList, _)
		->
			Type = disj_base_rec,
			Base = base(GoalAList),
			Rec = Rec0
		;
			fail
		)
	;
		Goal = if_then_else(_Vars, If, Then, Else, _SM) - _GoalInfo,

		If = _ - IfGoalInfo,
		goal_info_get_instmap_delta(IfGoalInfo, IMDelta),
		instmap__apply_instmap_delta(InitialInstMap, IMDelta,
				BeforeThenInstMap),

		goal_to_conj_list(If, IfList),
		goal_to_conj_list(Then, ThenList),
		goal_to_conj_list(Else, ElseList)
	->
		(
			accumulator__split_recursive_case(PredId, ProcId,
					InitialInstMap, IfList,
					BeforeThenInstMap, ModuleInfo,
					FullyStrict, ThenList, Rec0),

				% Make sure that the base case doesn't
				% contain a recursive call.
			\+ accumulator__split_recursive_case(PredId, ProcId,
					InitialInstMap, [],
					InitialInstMap, ModuleInfo, FullyStrict,
					ElseList, _)
		->
			Type = ite_rec_base,
			Base = base(ElseList),
			Rec = Rec0
		;
			accumulator__split_recursive_case(PredId, ProcId,
					InitialInstMap, [],
					InitialInstMap, ModuleInfo, FullyStrict,
					ElseList, Rec0),

				% Make sure that the base case doesn't
				% contain a recursive call.
			\+ accumulator__split_recursive_case(PredId, ProcId,
					InitialInstMap, [],
					InitialInstMap, ModuleInfo, FullyStrict,
					ThenList, _)
		->
			Type = ite_base_rec,
			Base = base(ThenList),
			Rec = Rec0
		;
			fail
		)
		
	;
		fail
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	%
	% accumulator__create_accumulator_pred
	%
	% Create a new predicate which is an accumulator version of the
	% current proc being looked at.
	%
:- pred accumulator__create_accumulator_pred(rec_goal::in, pred_info::in,
		proc_info::in, subst::out, pred_id::out, proc_id::out,
		sym_name::out, module_info::in, module_info::out) is det.

accumulator__create_accumulator_pred(RecGoal, PredInfo, ProcInfo, HstoAs_Subst,
		NewPredId, NewProcId, NewPredName, ModuleInfo0, ModuleInfo) :-

	proc_info_get_initial_instmap(ProcInfo, ModuleInfo0, InstMap0),

	accumulator__acc_proc_info(RecGoal, InstMap0, ProcInfo, HstoAs_Subst,
			NewTypes, NewProcInfo),
	accumulator__acc_pred_info(NewTypes, NewProcInfo, PredInfo, NewProcId,
			NewPredInfo),

	pred_info_name(NewPredInfo, NewPredName0),
	NewPredName = unqualified(NewPredName0),

	module_info_get_predicate_table(ModuleInfo0, PredTable0),
	predicate_table_insert(PredTable0, NewPredInfo, NewPredId, PredTable),
	module_info_set_predicate_table(ModuleInfo0, PredTable, ModuleInfo).


	%
	% accumulator__acc_proc_info
	%
	% Construct a proc_info for the introduced predicate, it also
	% creates the substitutions that maps between each variable that
	% is a member of Hs and those in As.
	%
:- pred accumulator__acc_proc_info(rec_goal::in, instmap::in, proc_info::in,
		subst::out, list(type)::out, proc_info::out) is det.

accumulator__acc_proc_info(recursive(PreDP, DP, _R, C), InstMap0,
		ProcInfo, HstoAs_Subst, NewTypes, NewProcInfo) :-

		% ProcInfo Stuff that must change.
	proc_info_varset(ProcInfo, VarSet0),
	proc_info_vartypes(ProcInfo, VarTypes0),
	proc_info_headvars(ProcInfo, HeadVars0),
	proc_info_argmodes(ProcInfo, HeadModes0),

	proc_info_inferred_determinism(ProcInfo, Detism),
	proc_info_goal(ProcInfo, Goal),
	proc_info_context(ProcInfo, Context),
	proc_info_typeinfo_varmap(ProcInfo, TVarMap),
	proc_info_typeclass_info_varmap(ProcInfo, TCVarsMap),
	proc_info_is_address_taken(ProcInfo, IsAddressTaken),

	accumulator__extra_vars_for_recursive_call(PreDP, DP, C, Vars),

	DP = goal(DPGoals, _DPInstMap),
	goal_list_instmap_delta(DPGoals, InstMapDelta),
	instmap__apply_instmap_delta(InstMap0, InstMapDelta, InstMap),

	accumulator__new_acc_var(Vars, InstMap, VarSet0, VarTypes0,
			HstoAs_Subst, NewHeadVars, VarSet,
			VarTypes, NewHeadModes),

	list__append(HeadVars0, NewHeadVars, HeadVars),
	list__append(HeadModes0, NewHeadModes, HeadModes),

	list__map(map__lookup(VarTypes), NewHeadVars, NewTypes),
	
	proc_info_create(VarSet, VarTypes, HeadVars, HeadModes, Detism, Goal,
	                Context, TVarMap, TCVarsMap, IsAddressTaken,
			NewProcInfo).

	%
	% accumulator__new_acc_var(Hs, IM, VS0, VT0, HstoAs0, HstoAs,
	% 		As, VS, VT, Ms)
	%
	% For each variable, Hs, that needs to be accumulated create a
	% corresponding variable in As updating the varset, VS, var-type
	% mapping, VT, and recording the mode in Ms.  Also record the
	% mapping from each H to A in HstoAs.
	%
:- pred accumulator__new_acc_var(prog_vars::in, instmap::in,
		prog_varset::in, map(prog_var, type)::in,
		map(prog_var, prog_var)::out, prog_vars::out, prog_varset::out,
		map(prog_var, type)::out, list(mode)::out) is det.

accumulator__new_acc_var([], _InstMap, VarSet, VarTypes,
		Subst, [], VarSet, VarTypes, []) :-
	map__init(Subst).
accumulator__new_acc_var([Var | Vars], InstMap, VarSet0, VarTypes0,
		Subst, HeadVars, VarSet, VarTypes, Modes) :-
	accumulator__new_acc_var(Vars, InstMap, VarSet0, VarTypes0,
			Subst0, HeadVars0, VarSet1, VarTypes1, Modes0),

	varset__new_var(VarSet1, NewVar, VarSet),

	map__det_insert(Subst0, Var, NewVar, Subst),

	HeadVars = [NewVar | HeadVars0],

	map__lookup(VarTypes0, Var, Type),
	map__det_insert(VarTypes1, NewVar, Type, VarTypes),

		% XXX we don't want to use the inst of the var as it can
		% be more specific than it should be. ie int_const(1)
		% when it should be any integer.
		% However this will no longer handle partially
		% instantiated data structures.
	% instmap__lookup_var(InstMap, Var, Inst),
	Inst = ground(shared, no),
	inst_lists_to_mode_list([Inst], [Inst], Mode),
	list__append(Mode, Modes0, Modes).

	%
	% accumulator__acc_pred_info
	%
	% Construct the pred_info for the introduced predicate
	%
:- pred accumulator__acc_pred_info(list(type)::in, proc_info::in, pred_info::in,
		proc_id::out, pred_info::out) is det.

accumulator__acc_pred_info(NewTypes, NewProcInfo, PredInfo,
		NewProcId, NewPredInfo) :-
		
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

	list__append(Types0, NewTypes, Types),

	make_pred_name_with_context(ModuleName, "AccFrom", PredOrFunc, Name,
		Line, Counter, SymName),

	pred_info_create(ModuleName, SymName, TypeVarSet, ExistQVars, Types,
			Cond, PredContext, local, Markers, PredOrFunc,
			ClassContext, Owner, Assertions, NewProcInfo, NewProcId,
			NewPredInfo).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	%
	% accumulator__transform
	%
	% Actually do the transformation.  This predicate may fail if
	% for some reason the transformation cannot be completed.
	%
:- pred accumulator__transform(top_level::in, base_goal::in, rec_goal::in,
		hlds_goal::in, bool::in, bool::in, module_info::in,
		prog_vars::in, subst::in, pred_id::in,
		proc_id::in, sym_name::in,
		hlds_goal::out, warnings::out, hlds_goal::out) is semidet.

accumulator__transform(TopLevel, base(BaseGoalList), recursive(PreDP, DP, R, C),
		Goal, DoLCO, FullyStrict, ModuleInfo, HeadVars,
		HstoAs_Subst, NewPredId, NewProcId, NewPredName,
		OrigGoal, Warnings, NewGoal) :-

	accumulator__Ys_descended_from_Y0s(HeadVars, DP, ModuleInfo),

	accumulator__orig_base_case(BaseGoalList, OrigBaseGoal),

	accumulator__extra_vars_for_recursive_call(PreDP, DP, C, Vars),
	accumulator__orig_recursive_case(DP, R, HeadVars, NewPredId, NewProcId,
			NewPredName, Vars, Y0stoYs_Subst, OrigRecGoal),

	accumulator__new_base_case(BaseGoalList, C,
			Y0stoYs_Subst, HstoAs_Subst, NewBaseGoal),
	accumulator__new_recursive_case(DP, C, R, DoLCO, FullyStrict,
			ModuleInfo, NewPredId, NewProcId, NewPredName,
			Vars, HeadVars, Y0stoYs_Subst, HstoAs_Subst,
			Warnings, NewRecGoal),

	accumulator__top_level(TopLevel, Goal, OrigBaseGoal, OrigRecGoal,
			NewBaseGoal, NewRecGoal, OrigGoal, NewGoal).


:- pred accumulator__top_level(top_level::in, hlds_goal::in,
		hlds_goal::in, hlds_goal::in, hlds_goal::in,
		hlds_goal::in, hlds_goal::out, hlds_goal::out) is det.

accumulator__top_level(switch_base_rec, Goal, OrigBaseGoal, OrigRecGoal,
		NewBaseGoal, NewRecGoal, OrigGoal, NewGoal) :-
	(
		Goal = switch(Var, CanFail, Cases0, StoreMap) - GoalInfo,
		Cases0 = [case(IdA, _), case(IdB, _)]
	->
		OrigCases = [case(IdA, OrigBaseGoal), case(IdB, OrigRecGoal)],
		OrigGoal = switch(Var, CanFail, OrigCases, StoreMap) - GoalInfo,

		NewCases = [case(IdA, NewBaseGoal), case(IdB, NewRecGoal)],
		NewGoal = switch(Var, CanFail, NewCases, StoreMap) - GoalInfo
	;
		error("accumulator__top_level: not the correct top level")
	).
accumulator__top_level(switch_rec_base, Goal, OrigBaseGoal, OrigRecGoal,
		NewBaseGoal, NewRecGoal, OrigGoal, NewGoal) :-
	(
		Goal = switch(Var, CanFail, Cases0, StoreMap) - GoalInfo,
		Cases0 = [case(IdA, _), case(IdB, _)]
	->
		OrigCases = [case(IdA, OrigRecGoal), case(IdB, OrigBaseGoal)],
		OrigGoal = switch(Var, CanFail, OrigCases, StoreMap) - GoalInfo,

		NewCases = [case(IdA, NewRecGoal), case(IdB, NewBaseGoal)],
		NewGoal = switch(Var, CanFail, NewCases, StoreMap) - GoalInfo
	;
		error("accumulator__top_level: not the correct top level")
	).
accumulator__top_level(disj_base_rec, Goal, OrigBaseGoal,
		OrigRecGoal, NewBaseGoal, NewRecGoal, OrigGoal, NewGoal) :-
	(
		Goal = disj(Goals, StoreMap) - GoalInfo,
		Goals = [_, _]
	->
		OrigGoals = [OrigBaseGoal, OrigRecGoal],
		OrigGoal = disj(OrigGoals, StoreMap) - GoalInfo,

		NewGoals = [NewBaseGoal, NewRecGoal],
		NewGoal = disj(NewGoals, StoreMap) - GoalInfo
	;
		error("accumulator__top_level: not the correct top level")
	).
accumulator__top_level(disj_rec_base, Goal, OrigBaseGoal,
		OrigRecGoal, NewBaseGoal, NewRecGoal, OrigGoal, NewGoal) :-
	(
		Goal = disj(Goals, StoreMap) - GoalInfo,
		Goals = [_, _]
	->
		OrigGoals = [OrigRecGoal, OrigBaseGoal],
		OrigGoal = disj(OrigGoals, StoreMap) - GoalInfo,

		NewGoals = [NewRecGoal, NewBaseGoal],
		NewGoal = disj(NewGoals, StoreMap) - GoalInfo
	;
		error("accumulator__top_level: not the correct top level")
	).
accumulator__top_level(ite_base_rec, Goal, OrigBaseGoal,
		OrigRecGoal, NewBaseGoal, NewRecGoal, OrigGoal, NewGoal) :-
	(
		Goal = if_then_else(Vars, If, _, _, StoreMap) - GoalInfo
	->
		OrigGoal = if_then_else(Vars, If,
				OrigBaseGoal, OrigRecGoal, StoreMap) - GoalInfo,

		NewGoal = if_then_else(Vars, If,
				NewBaseGoal, NewRecGoal, StoreMap) - GoalInfo
	;
		error("accumulator__top_level: not the correct top level")
	).
accumulator__top_level(ite_rec_base, Goal, OrigBaseGoal,
		OrigRecGoal, NewBaseGoal, NewRecGoal, OrigGoal, NewGoal) :-
	(
		Goal = if_then_else(Vars, If, _, _, StoreMap) - GoalInfo
	->
		OrigGoal = if_then_else(Vars, If,
				OrigRecGoal, OrigBaseGoal, StoreMap) - GoalInfo,

		NewGoal = if_then_else(Vars, If,
				NewRecGoal, NewBaseGoal, StoreMap) - GoalInfo
	;
		error("accumulator__top_level: not the correct top level")
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	%
	% accumulator__update_accumulator_pred
	%
	% Place the accumulator version of the predicate in the
	% module_info structure.
	%
:- pred accumulator__update_accumulator_pred(pred_id::in, proc_id::in,
		hlds_goal::in, module_info::in, module_info::out) is det.

accumulator__update_accumulator_pred(NewPredId, NewProcId, AccGoal,
		ModuleInfo0, ModuleInfo) :-
	module_info_pred_proc_info(ModuleInfo0, NewPredId, NewProcId,
			PredInfo, ProcInfo0),
	proc_info_set_goal(ProcInfo0, AccGoal, ProcInfo),
	module_info_set_pred_proc_info(ModuleInfo0, NewPredId, NewProcId,
			PredInfo, ProcInfo, ModuleInfo).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	%
	% accumulator__split_recursive_case(PredId, ProcId, IM0, MI, FS, Gs, R)
	%
	% Split the goals, Gs, which make up the recursive case into the
	% decompose and process list of goals, the compose goals and the 
	% recursive goal and place the components in the rec_goal
	% structure, R.
	%
:- pred accumulator__split_recursive_case(pred_id::in, proc_id::in,
		instmap::in, hlds_goals::in,
		instmap::in, module_info::in, bool::in,
		hlds_goals::in, rec_goal::out) is semidet.

accumulator__split_recursive_case(PredId, ProcId,
		PreInstMap, PreGoals,
		InitialInstMap, ModuleInfo, FullyStrict, Goals, RecGoal) :-
	solutions(accumulator__split_goals(Goals, PredId, ProcId), Solns),
	Solns = [recursive(DP0, R, C0)],
	calculate_instmap(DP0, InitialInstMap, InitialInstMapBeforeR),

		% Any goal which doesn't depend on the recursive call is
		% moved before the recursive call, because this means
		% that only goals which contain dynamic variables are
		% left after the recursive call, simplifying the latter
		% stages.
	move_goals(R, InitialInstMapBeforeR, ModuleInfo, FullyStrict,
			C0, PreC0, PostC0),
	list__append(DP0, PreC0, DP),
	C = PostC0,

	calculate_instmap(DP, InitialInstMap, InstMapBeforeR),
	calculate_instmap([R], InstMapBeforeR, InstMapBeforeC),

	Pre		 = goal(PreGoals, PreInstMap),
	DecomposeProcess = goal(DP, InitialInstMap),
	Recursive 	 = goal(R, InstMapBeforeR),
	Compose 	 = goal(C, InstMapBeforeC),

	RecGoal = recursive(Pre, DecomposeProcess, Recursive, Compose).

:- pred accumulator__split_goals(hlds_goals::in, pred_id::in, proc_id::in,
		split_result::out) is nondet.

accumulator__split_goals(Goals, PredId, ProcId, RecGoal) :-
	list__append(DecomposeProcess, [RecursiveCall | Compose], Goals),
	RecursiveCall = call(PredId, ProcId, _, _, _, _) - _,

		% An empty compose means the predicate is already tail
		% recursive.
	Compose \= [],
	RecGoal = recursive(DecomposeProcess, RecursiveCall, Compose).


	%
	% Given a list of goals and an instmap before the list of goals,
	% work out what the instmap at the end of the goals is.
	%
:- pred calculate_instmap(hlds_goals::in, instmap::in, instmap::out) is det.

calculate_instmap(Goals, InstMap0, InstMap) :-
	goal_list_instmap_delta(Goals, InstMapDelta),
	instmap__apply_instmap_delta(InstMap0, InstMapDelta, InstMap).


%-----------------------------------------------------------------------------%

	%
	% move_goals(G, IM, MI, FS, Gs, BGs, AGs)
	%
	% Seperate the list of Goals, Gs, into the goals, BGs, that can be
	% placed before the goal, G, and the goals, AGs, which must be
	% placed after the goal, G.  IM is the instmap before the goal
	% G.
	%
	% XXX This should be able to be transformed to accumulator
	% recursive form, much harder to do though.  Look into this
	% later. NB you need LCO for the else case.
	%
:- pred move_goals(hlds_goal::in, instmap::in, module_info::in, bool::in,
		hlds_goals::in, hlds_goals::out, hlds_goals::out) is det.

move_goals(_StartGoal, _IMBeforeStartGoal, _MI, _FullyStrict, [], [], []).
move_goals(StartGoal, InstMapBeforeStartGoal, ModuleInfo, FullyStrict,
		[Goal|Goals], PreGoals, PostGoals) :-

	StartGoal = _GoalExpr - GoalInfo,
	goal_info_get_instmap_delta(GoalInfo, InstMapDelta),
	instmap__apply_instmap_delta(InstMapBeforeStartGoal,
			InstMapDelta, InstMapBeforeGoal),
	(
		goal_util__can_reorder_goals(ModuleInfo, FullyStrict,
				InstMapBeforeStartGoal, StartGoal,
				InstMapBeforeGoal, Goal)
	->
		move_goals(StartGoal, InstMapBeforeStartGoal, ModuleInfo,
				FullyStrict, Goals, PreGoals0, PostGoals),
		PreGoals = [Goal | PreGoals0]
	;
		move_goals(Goal, InstMapBeforeGoal, ModuleInfo, FullyStrict,
				Goals, PreGoalsForGoal, PostGoalsForGoal),
		move_goals(StartGoal, InstMapBeforeStartGoal,
				ModuleInfo, FullyStrict, PreGoalsForGoal,
				PreGoals, PostGoalsForStartGoal),
		
		list__append(PostGoalsForStartGoal, [Goal | PostGoalsForGoal],
				PostGoals)
	).


%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	%
	% accumulator__Ys_descended_from_Y0s(HV, DP)
	%
	% If any head variable is calculated in the decompose/process,
	% DP, list of goals then it cannot be descended from the Y0s.
	%
:- pred accumulator__Ys_descended_from_Y0s(prog_vars::in,
		a_goals::in, module_info::in) is semidet.

accumulator__Ys_descended_from_Y0s(HeadVars, DecomposeProcess, ModuleInfo) :-
	accumulator__vars_to_accumulate(HeadVars, DecomposeProcess,
			ModuleInfo, ChangedHeadVars),

	ChangedHeadVars = [].

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	%
	% accumulator__vars_to_accumulate(HV, C, VA)
	%
	% Given the list of goals, C, which represent the compose
	% goal and the list of the head variables, HV, determine the
	% head variables that will need to be accumulated, VA.
	%
	% The variables that will need to be accumulated are simply
	% those whose instantiatedness change in the compose goals and
	% that are headvars.
	%
:- pred accumulator__vars_to_accumulate(prog_vars::in, a_goals::in,
		module_info::in, prog_vars::out) is det.

accumulator__vars_to_accumulate(HeadVars, C, ModuleInfo, VarsToAccumulate) :-
	C = goal(ComposeGoals, InstMapBeforeCompose),
		
	goal_list_instmap_delta(ComposeGoals, InstMapDelta),
	instmap__apply_instmap_delta(InstMapBeforeCompose,
			InstMapDelta,InstMapAfterCompose),

	instmap_changed_vars(InstMapBeforeCompose,
			InstMapAfterCompose, ModuleInfo, ChangedVars),

	Member = (pred(M::in) is semidet :- set__member(M, ChangedVars)),
	list__filter(Member, HeadVars, VarsToAccumulate).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	%
	% accumulator__extra_vars_for_recursive_call(DP, C, Hs)
	%
	% If the decompose/process list of goals, DP, produce a value
	% that is needed in the compose list of goals, C, then that
	% value will need to be passed via the introduced recursive
	% call.  This predicate identifies these variables, Hs.
	%
:- pred accumulator__extra_vars_for_recursive_call(a_goals::in, a_goals::in,
		a_goals::in, prog_vars::out) is det.

accumulator__extra_vars_for_recursive_call(
		goal(PreDecomposeProcess, _InstMapBeforePreDecomposeProcess),
		goal(DecomposeProcess, _InstMapBeforeDecomposeProcess),
		goal(Compose, _InstMapBeforeCompose), Vars) :-

	goal_list_nonlocals(PreDecomposeProcess, PreDPNonLocalsSet),
	goal_list_nonlocals(DecomposeProcess, DPNonLocalsSet),
	set__union(PreDPNonLocalsSet, DPNonLocalsSet, NonLocals),

	goal_list_nonlocals(Compose, CNonLocalsSet),

	set__intersect(NonLocals, CNonLocalsSet, VarsSet),
	set__to_sorted_list(VarsSet, Vars).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	%
	% accumulator__static_vars_in_recursive_call
	%
	% Identify the variables which are static and only appear in the
	% recursive call.
	%
	% This predicate is currently unused.
	%
:- pred accumulator__static_vars_in_recursive_call(a_goal::in,
		a_goals::in, module_info::in, prog_vars::out) is det.

accumulator__static_vars_in_recursive_call(Recursive, Compose,
		ModuleInfo, Vars) :-

	Recursive = goal(_RecGoalExpr - RecGoalInfo, InstMapBeforeRec),
	Compose = goal(ComposeGoals, _InstMapBeforeCompose),

	goal_info_get_instmap_delta(RecGoalInfo, RecInstMapDelta),
	goal_info_get_nonlocals(RecGoalInfo, RecNonLocals),
	instmap__apply_instmap_delta(InstMapBeforeRec, RecInstMapDelta,
			InstMapAfterRec),
	instmap_changed_vars(InstMapBeforeRec, InstMapAfterRec,
			ModuleInfo, ChangedVars),

	set__difference(RecNonLocals, ChangedVars, PossibleStaticVars),

	goal_list_nonlocals(ComposeGoals, CNonLocalsSet),

	set__intersect(CNonLocalsSet, PossibleStaticVars, VarsSet),
	set__to_sorted_list(VarsSet, Vars).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	%
	% accumulator__orig_base_case
	%
	% Determine the base case of the original goal.
	%
:- pred accumulator__orig_base_case(hlds_goals::in, hlds_goal::out) is det.

accumulator__orig_base_case(BaseGoalList, BaseGoal) :-
		
		% Note the the goal_info constructed should be identical
		% to the original goal_info.  It is just that it is no
		% longer easily accessible.
	calculate_goal_info(conj(BaseGoalList), BaseGoal).

%-----------------------------------------------------------------------------%

	%
	% accumulator__orig_recursive_case
	%
	% Work out a new recursive case for the original predicate
	% which replaces the recursive call with a call to the new
	% accumulator predicate.
	%
:- pred accumulator__orig_recursive_case(a_goals::in, a_goal::in,
		prog_vars::in, pred_id::in, proc_id::in, sym_name::in,
		prog_vars::in, subst::out, hlds_goal::out) is det.

accumulator__orig_recursive_case(DP, R0, HeadVars, PredId, ProcId, Name,
		ExtraVars, Y0stoYs_Subst, Goal) :-
	DP = goal(DecomposeProcess, _InstMapBeforeDecomposeProcess),
	R0 = goal(Recursive, _InstMapBeforeRecursive),
	(
		Recursive = GoalExpr0 - GoalInfo0,
		GoalExpr0 = call(_, _, Vars, Builtin, Context, _)
	->
			% Calculate what vars the new call should use.
		goal_list_nonlocals(DecomposeProcess, NonLocals),
		assoc_list__from_corresponding_lists(HeadVars, Vars, Pairs),
		list__map(accumulator__which_var(NonLocals), Pairs, CallVars0),
		list__append(CallVars0, ExtraVars, CallVars),
		GoalExpr = call(PredId, ProcId, CallVars,
				Builtin, Context, Name),

		goal_info_set_nonlocals(GoalInfo0, set__list_to_set(CallVars),
				GoalInfo),

			% Rename the variables in the goal.
		map__init(Subst0),
		map__det_insert_from_corresponding_lists(Subst0, Vars,
				CallVars0, Y0stoYs_Subst),
		goal_util__rename_vars_in_goal(GoalExpr - GoalInfo,
				Y0stoYs_Subst, R),
				

		list__append(DecomposeProcess, [R], GoalList),
		calculate_goal_info(conj(GoalList), Goal)
	;
		error("accumulator__orig_recursive_case: Not a call.")
	).


	%
	% If a var is a member of the nonlocal set of variables then it
	% must be that variable we are to use in the call, otherwise we
	% use the corresponding head variable.
	%
:- pred accumulator__which_var(set(prog_var)::in,
		pair(prog_var, prog_var)::in, prog_var::out) is det.

accumulator__which_var(NonLocals, HeadVar - CallVar, Var) :-
	(
		set__member(CallVar, NonLocals)
	->
		Var = CallVar
	;
		Var = HeadVar
	).


%-----------------------------------------------------------------------------%

	%
	% accumulator__new_base_case
	%
	% Determine the base case of the introduced predicate.
	%
:- pred accumulator__new_base_case(hlds_goals::in, a_goals::in,
		subst::in, subst::in, hlds_goal::out) is det.

accumulator__new_base_case(Base, C, Y0stoYs_Subst, HstoAs_Subst, Goal) :-
	C = goal(Compose, _InstMapBeforeCompose),

	reverse_subst(Y0stoYs_Subst, YstoY0s_Subst0),

	goal_list_nonlocals(Compose, NonLocals),
	map__select(YstoY0s_Subst0, NonLocals, YstoY0s_Subst),

	goal_util__rename_vars_in_goals(Base, no, YstoY0s_Subst, NewBase),
	goal_util__rename_vars_in_goals(Compose, no, HstoAs_Subst, NewCompose),

	list__append(NewBase, NewCompose, GoalList),
	calculate_goal_info(conj(GoalList), Goal).

%-----------------------------------------------------------------------------%

	%
	% accumulator__new_recursive_case
	%
	% Determine the recursive case of the introduced predicate.
	%
:- pred accumulator__new_recursive_case(a_goals::in, a_goals::in,
		a_goal::in, bool::in, bool::in,
		module_info::in, pred_id::in, proc_id::in,
		sym_name::in, prog_vars::in, prog_vars::in, subst::in,
		subst::in, warnings::out, hlds_goal::out) is semidet.

accumulator__new_recursive_case(DP, C, R0, DoLCO, FullyStrict,
		ModuleInfo, PredId, ProcId, Name, Hs, HeadVars,
		Y0stoYs_Subst, HstoAs_Subst, Warnings, Goal) :-
	DP = goal(DecomposeProcess, _InstMapBeforeDecomposeProcess),
	C  = goal(Compose, InstMapBeforeCompose),
	R0 = goal(Recursive0, _InstMapBeforeRecursive0),

	assoc_info_init(ModuleInfo, HeadVars, DP, C, R0,
		Y0stoYs_Subst, HstoAs_Subst, AssocInfo0),
	accumulator__check_assoc(Compose, InstMapBeforeCompose, ModuleInfo,
			FullyStrict, PreRecGoal0, PostRecGoal0,
			AssocInfo0, AssocInfo),

	(
		DoLCO = yes,

			% If there are no goals that can be moved before
			% the recursive call, then there is nothing to
			% accumulate so fail.
		PreRecGoal0 \= [],

		map__init(LCO_Subst0),
		assoc_info_dynamic_set(DynamicSet, AssocInfo, _),
		accumulator__check_post_rec_goals(PostRecGoal0, DynamicSet,
				LCO_Subst0, LCO_Subst)
	;
		DoLCO = no,
		map__init(LCO_Subst),
		PostRecGoal0 = []
	),

	assoc_info_Y0stoAs(Y0stoAs_Subst, AssocInfo, _),
	assoc_info_warnings(Warnings, AssocInfo, _),

	accumulator__rename_prerec_goals(PreRecGoal0, Y0stoAs_Subst,
			Y0stoYs_Subst, LCO_Subst, HstoAs_Subst, PreRecGoal),

	accumulator__rename_recursive_goal(Recursive0, Hs, PredId, ProcId,
			Name, HstoAs_Subst, Y0stoAs_Subst,
			Y0stoYs_Subst, LCO_Subst, Recursive),

	accumulator__rename_postrec_goals(PostRecGoal0, HstoAs_Subst,
			PostRecGoal),

	list__append(PreRecGoal, [Recursive | PostRecGoal], GoalList0),
	list__append(DecomposeProcess, GoalList0, GoalList),
	calculate_goal_info(conj(GoalList), Goal).

%-----------------------------------------------------------------------------%


	%
	% accumulator__check_post_rec_goals
	%
	% Ensure that each goal which is to be placed after the
	% recursive goal is a construction unification.
	%
	% Also create a substition which records from which dynamic var
	% each headvar is descended.
	%
:- pred accumulator__check_post_rec_goals(hlds_goals::in, set(prog_var)::in,
		subst::in, subst::out) is semidet.

accumulator__check_post_rec_goals([], _DynamicSet, Subst, Subst).
accumulator__check_post_rec_goals([Goal | Goals], DynamicSet, Subst0, Subst) :-
	Goal = unify(_TermL, _TermR, _Mode, Unify, _Context) - _GoalInfo,
	Unify = construct(Var, _ConsId, Vars, _Modes, _, _, _),
	set__list_to_set(Vars, VarsSet),
	set__intersect(VarsSet, DynamicSet, DynamicVarsSet),
	set__singleton_set(DynamicVarsSet, DynamicVar),
	(
		map__search(Subst0, DynamicVar, DescendedFrom)
	->
		map__delete(Subst0, DynamicVar, Subst1),
		map__insert(Subst1, Var, DescendedFrom, Subst2)
	;
		map__insert(Subst0, Var, DynamicVar, Subst2)
	),
	accumulator__check_post_rec_goals(Goals, DynamicSet, Subst2, Subst).
	

%-----------------------------------------------------------------------------%

:- pred accumulator__rename_prerec_goals(hlds_goals::in, subst::in,
		subst::in, subst::in, subst::in, hlds_goals::out) is det.

accumulator__rename_prerec_goals(Goal0, Y0stoAs_Subst, Y0stoYs_Subst,
		LCO_Subst, HstoAs_Subst0, Goal) :-
	reverse_subst(Y0stoYs_Subst, YstoY0s_Subst),
	reverse_subst(LCO_Subst, LCOtoYs_Subst),
	chain_subst(LCOtoYs_Subst, YstoY0s_Subst, LCOtoY0s_Subst),

	delete_used_as(HstoAs_Subst0, Y0stoAs_Subst, HstoAs_Subst),

	goal_util__rename_vars_in_goals(Goal0, no, Y0stoAs_Subst, Goal1),
	goal_util__rename_vars_in_goals(Goal1, no, LCOtoY0s_Subst, Goal2),
	goal_util__rename_vars_in_goals(Goal2, no, YstoY0s_Subst, Goal3),
	goal_util__rename_vars_in_goals(Goal3, no, HstoAs_Subst, Goal).

:- pred delete_used_as(subst::in, subst::in, subst::out) is det.
delete_used_as(HstoAs_Subst0, Y0stoAs_Subst, HstoAs_Subst) :-
	reverse_subst(HstoAs_Subst0, AstoHs_Subst0),
	reverse_subst(Y0stoAs_Subst, AstoY0s_Subst),

	map__keys(AstoY0s_Subst, AstoDelete),
	map__delete_list(AstoHs_Subst0, AstoDelete, AstoHs_Subst),

	reverse_subst(AstoHs_Subst, HstoAs_Subst).

:- pred accumulator__rename_recursive_goal(hlds_goal::in, prog_vars::in,
		pred_id::in, proc_id::in, sym_name::in, subst::in, subst::in,
		subst::in, subst::in, hlds_goal::out) is det.

accumulator__rename_recursive_goal(Goal0, Hs, PredId, ProcId, Name,
		HstoAs_Subst, Y0stoAs_Subst, Y0stoYs_Subst, LCO_Subst, Goal) :-

	Goal0 = GoalExpr0 - GoalInfo0,
	(
		GoalExpr0 = call(_, _, Args0, Builtin, Context, _)
	->
		reverse_subst(Y0stoAs_Subst, AstoY0s_Subst),
		reverse_subst(HstoAs_Subst, AstoHs_Subst),

		list__append(Args0, Hs, Args),
		GoalExpr1 = call(PredId, ProcId, Args, Builtin, Context, Name),

		goal_info_get_nonlocals(GoalInfo0, NonLocals0),
		set__list_to_set(Args, ArgsSet),
		set__union(ArgsSet, NonLocals0, NonLocals),
		goal_info_set_nonlocals(GoalInfo0, NonLocals, GoalInfo1),

		Goal1 = GoalExpr1 - GoalInfo1,

		goal_util__rename_vars_in_goal(Goal1, Y0stoYs_Subst, Goal2),
		goal_util__rename_vars_in_goal(Goal2, HstoAs_Subst, Goal3),
		goal_util__rename_vars_in_goal(Goal3, AstoY0s_Subst, Goal4),
		goal_util__rename_vars_in_goal(Goal4, LCO_Subst, Goal5),
		goal_util__rename_vars_in_goal(Goal5, AstoHs_Subst, Goal)
	;
		error("accumulator__rename_recursive_goal: to make det.")
	).

:- pred accumulator__rename_postrec_goals(hlds_goals::in, subst::in,
		hlds_goals::out) is det.

accumulator__rename_postrec_goals(Goals0, HstoAs_Subst, Goals) :-
	goal_util__rename_vars_in_goals(Goals0, no, HstoAs_Subst, Goals).
	
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

	%
	% accumulator__check_assoc(Gs, MGs, AGs)
	%
	% Given a list of goals, Gs, check to ensure that the list of
	% goals Gs is associative rearranging a goal if necessary to
	% ensure associativity.  These goals are placed into MGs.
	%
	% It is not possible for construction unfications to be
	% associative.  However if only construction unfications remain
	% after the recursive call, it is possible to do the lco (see
	% lco.m) optimisation.  So whenever the predicate encounters a
	% construction unfication, it places it any goals that depend on
	% that goal into AGs.
	%
:- pred accumulator__check_assoc(hlds_goals::in, instmap::in,
		module_info::in, bool::in, hlds_goals::out,
		hlds_goals::out, assoc_info::in, assoc_info::out) is semidet.

accumulator__check_assoc([], _InstMap, _MI, _FullyStrict, [], []) --> [].
accumulator__check_assoc([Goal0 | Goal0s], InstMapBeforeGoal0,
		ModuleInfo, FullyStrict, MovedGoals, AfterGoals) -->
	(
		{ goal_is_construction_unification(Goal0) }
	->
		{ move_goals(Goal0, InstMapBeforeGoal0, ModuleInfo, FullyStrict,
				Goal0s, PreGoal0s, PostGoal0s) },
		accumulator__check_assoc(PreGoal0s, InstMapBeforeGoal0,
				ModuleInfo, FullyStrict,
				MovedGoals, AfterGoal0s),
		{ list__append(AfterGoal0s, [Goal0 | PostGoal0s], AfterGoals) }
	;
		{ Goal0 = _ - GoalInfo0 },
		{ goal_info_get_instmap_delta(GoalInfo0, InstMapDelta) },
		{ instmap__apply_instmap_delta(InstMapBeforeGoal0 ,
				InstMapDelta, InstMapBeforeGoal0s) },


		accumulator__check_goal(Goal0, Goal),
		accumulator__check_assoc(Goal0s, InstMapBeforeGoal0s,
				ModuleInfo, FullyStrict,
				MovedGoal0s, AfterGoals),
		{ MovedGoals = [Goal | MovedGoal0s] }
	).


%-----------------------------------------------------------------------------%

	%
	% Is the goal a construction unification?
	%
:- pred goal_is_construction_unification(hlds_goal::in) is semidet.

goal_is_construction_unification(Goal - _GoalInfo) :-
	Goal = unify(_, _, _, Unification, _),
	Unification = construct(_, _, _, _, _, _, _).

%-----------------------------------------------------------------------------%

	%
	% Is the current goal associative?
	%
:- pred accumulator__check_goal(hlds_goal::in, hlds_goal::out,
		assoc_info::in, assoc_info::out) is semidet.

accumulator__check_goal(conj(Goals0) - GoalInfo, conj(Goals) - GoalInfo) -->
	accumulator__check_goallist(Goals0, Goals).

accumulator__check_goal(par_conj(_, _) - _, _) -->
	{ fail }.

	% Branched Goals
	%
	% XXX the previous version of accumulator ensured the condition
	% that each arm of the disjunct only updated static variables.
	% However this will not handle the vectorising case.
accumulator__check_goal(disj(_, _) - _, _) --> { fail }.
accumulator__check_goal(switch(_, _, _, _) - _, _) --> { fail }.
accumulator__check_goal(if_then_else(_, _, _, _, _) - _, _) --> { fail }.

accumulator__check_goal(not(_) - _, _) --> { fail }.

accumulator__check_goal(some(Vars, CanRemove, Goal0) - GoalInfo,
		some(Vars, CanRemove, Goal) - GoalInfo) -->
	accumulator__check_goal(Goal0, Goal).

	% All of these must fail because there is no way we can know
	% whether or not these calls are associative.
	% XXX this may not be true for class_method_call, it may be
	% possible to make it a condition that this method is always
	% associative.
accumulator__check_goal(generic_call(_,_,_,_) - _, _) --> { fail }.
accumulator__check_goal(pragma_c_code(_,_,_,_,_,_,_) - _, _) --> { fail }.

accumulator__check_goal(unify(TermL, TermR, Mode, Unify, Context) - GoalInfo, 
		unify(TermL, TermR, Mode, Unify, Context) - GoalInfo) -->
	{ accumulator__check_assoc_unify_rhs(TermR) },
	accumulator__check_assoc_unify(Unify).

accumulator__check_goal(call(PredId, ProcId, Arg0s, Builtin, Context, Sym)
			- GoalInfo,
		call(PredId, ProcId, Args, Builtin, Context, Sym)
			- GoalInfo) -->
	assoc_info_module_info(ModuleInfo),
	accumulator__call_dynamic_var(Arg0s, DynamicCallVar),

	{ goal_info_get_context(GoalInfo, ProgContext) },
	accumulator__is_associative(PredId, ProcId, ProgContext, ModuleInfo,
			Arg0s, Args, PossibleStaticVars, Commutative),

	accumulator__check_previous_calls(DynamicCallVar,
			PredId, ProcId, Commutative),
	accumulator__new_dynamic_var(Arg0s, DynamicCallVar, NewDynamicVar),
	accumulator__update_orig_var_map(DynamicCallVar, NewDynamicVar),
	accumulator__update_Y0stoAs_subst(DynamicCallVar, [PossibleStaticVars]).

%-----------------------------------------------------------------------------%

:- pred accumulator__check_goallist(hlds_goals::in, hlds_goals::out,
		assoc_info::in, assoc_info::out) is semidet.

accumulator__check_goallist([], []) --> [].
accumulator__check_goallist([Goal0 | Goals0], [Goal | Goals]) -->
	accumulator__check_goal(Goal0, Goal),
	accumulator__check_goallist(Goals0, Goals).

%-----------------------------------------------------------------------------%

	%
	% This predicate is meant to fail if the rhs of the unification
	% is a lambda goal, because I am not convinced that I know how
	% to handle this correctly.
	%
:- pred accumulator__check_assoc_unify_rhs(unify_rhs::in) is semidet.

accumulator__check_assoc_unify_rhs(var(_)).
accumulator__check_assoc_unify_rhs(functor(_, _)).
accumulator__check_assoc_unify_rhs(lambda_goal(_, _, _, _, _, _, _, _)) :-
		%
		% For the moment just fail, as I am not sure how to
		% handle this.
		%
	fail.

	%
	% accumulator__check_assoc_unify_rhs
	%
	% The only safe unifications are assignment unifications.
	%
:- pred accumulator__check_assoc_unify(unification::in,
		assoc_info::in, assoc_info::out) is semidet.

accumulator__check_assoc_unify(
		construct(_Var, _ConsId, _Vars, _Modes, _, _, _)) -->
		%
		% We shouldn't fail if we have this case as all the
		% construction unification does is put a wrapper 
		% around the dynamic variables.  What we need to
		% recognise is that the construction/deconstruction
		% pair do nothing.
		%
		% f(X,Y) :-
		% 	decompose(X,Xh,Xr),
		% 	f(Xr,Y0),
		% 	Y0 = c(A0, B0),
		%	composeA(Xh,A0,A),
		%	composeB(Xh,B0,B),
		%	Y = c(A, B).
		%
		% I think that the way to recognise these
		% situations is when the type of Y is flat
		% (non-recursive).
		%
	{ fail }.
accumulator__check_assoc_unify(
		deconstruct(_Var, _ConsId, _Vars, _Modes, _Cat)) -->
		% see comment in construct
	{ fail }.
accumulator__check_assoc_unify(assign(L, _R)) -->
	assoc_info_dynamic_set(DynamicSet0),
	{ set__insert(DynamicSet0, L, DynamicSet) },
	assoc_info_set_dynamic_set(DynamicSet).
accumulator__check_assoc_unify(simple_test(_L, _R)) --> 
	{ fail }.
accumulator__check_assoc_unify(complicated_unify(_Modes, _Cat, _)) -->
	{ fail }.	% XXX not sure what this should be.

%-----------------------------------------------------------------------------%

	%
	% accumulator__call_dynamic_var(As, DV)
	%
	% Given the list of arguments to a call try and determine which
	% of the arguments is the current dynamic variable.
	%
	% Fails if there is more then one dynamic variable in a call.
	% This is because in general more then one dynamic variable
	% being used in a call prevents the goals from being
	% associative.
	%
:- pred accumulator__call_dynamic_var(prog_vars::in, prog_var::out,
		assoc_info::in, assoc_info::out) is semidet.

accumulator__call_dynamic_var(Args, DynamicCallVar) -->
	assoc_info_dynamic_set(DynamicSet),
	{ set__list_to_set(Args, ArgSet) },
	{ set__intersect(ArgSet, DynamicSet, DynamicCallArgs) },
	{ set__singleton_set(DynamicCallArgs, DynamicCallVar) }.

	
	%
	% accumulator__new_dynamic_var(As, DV, O)
	%
	% Given the original arguments to a call, As, and the dynamic
	% var, DV, determine the new dynamic var, O, also update the
	% dynamic set at the same time.
	%
:- pred accumulator__new_dynamic_var(prog_vars::in, prog_var::in,
		prog_var::out, assoc_info::in, assoc_info::out) is semidet.

accumulator__new_dynamic_var(Args0, DynamicCallVar, NewDynamicVar) -->
	assoc_info_static_set(StaticSet0),
	assoc_info_dynamic_set(DynamicSet0),

	{ set__list_to_set(Args0, ArgSet) },
	{ set__difference(ArgSet, StaticSet0, ArgDynamicSet) },
	{ set__union(ArgDynamicSet, DynamicSet0, DynamicSet) },

	assoc_info_set_dynamic_set(DynamicSet),

	{ set__delete(ArgDynamicSet, DynamicCallVar, OutputDynamicCallArgs) },
	{ set__singleton_set(OutputDynamicCallArgs, NewDynamicVar) }.

	%
	% accumulator__check_previous_calls
	%
	% Ensure that the previous calls have been commutative and calling
	% the same procedure, or if it is associative that there has
	% been no previous calls.
	%
:- pred accumulator__check_previous_calls(prog_var::in,
		pred_id::in, proc_id::in, commutative::in,
		assoc_info::in, assoc_info::out) is semidet.

accumulator__check_previous_calls(DynamicCallVar,
		PredId, ProcId, Commutative) -->
	assoc_info_orig_dynvar_map(OrigDynMap),
	assoc_info_prev_call_map(PrevCallMap0),

	{ map__lookup(OrigDynMap, DynamicCallVar, OrigVar) },
	{ accumulator__check_prevcalls(OrigVar, PredId, ProcId, Commutative,
		PrevCallMap0, PrevCallMap) },

	assoc_info_set_prev_call_map(PrevCallMap).

	%
	% accumulator__update_orig_var_map(DV, Ns)
	%
	% Record for each variable Ns which variable that variable
	% is descended from.
	%
:- pred accumulator__update_orig_var_map(prog_var::in, prog_var::in,
		assoc_info::in, assoc_info::out) is det.

accumulator__update_orig_var_map(DynamicCallVar, NewDynamicVar) -->
	assoc_info_orig_dynvar_map(OrigDynMap0),

	{ map__lookup(OrigDynMap0, DynamicCallVar, OrigVar) },
	{ map__det_insert(OrigDynMap0, NewDynamicVar, OrigVar, OrigDynMap) },

	assoc_info_set_orig_dynvar_map(OrigDynMap).

	%
	% accumulator__update_Y0stoAs_subst
	%
	% Update the Y0s -> As Substitution.
	%
:- pred accumulator__update_Y0stoAs_subst(prog_var::in, list(set(prog_var))::in,
		assoc_info::in, assoc_info::out) is semidet.

accumulator__update_Y0stoAs_subst(DynamicCallVar, PossibleStaticVarsList) -->
	assoc_info_orig_dynvar_map(OrigDynVarMap),
	assoc_info_static_set(StaticSet),
	assoc_info_HstoAs(HstoAs_Subst),

	{ map__lookup(OrigDynVarMap, DynamicCallVar, OrigDynVar) },

	assoc_info_Y0stoAs(Y0stoAs_Subst0),
	{ accumulator__update_Y0stoAs_subst_2(PossibleStaticVarsList,
			OrigDynVar, StaticSet, HstoAs_Subst,
			Y0stoAs_Subst0, Y0stoAs_Subst) },
	assoc_info_set_Y0stoAs(Y0stoAs_Subst).


:- pred accumulator__update_Y0stoAs_subst_2(list(set(prog_var))::in,
		prog_var::in, set(prog_var)::in,
		subst::in, subst::in, subst::out) is semidet.

accumulator__update_Y0stoAs_subst_2([], _, _, _, Y0stoAs_Subst, Y0stoAs_Subst).
accumulator__update_Y0stoAs_subst_2([PossibleStaticVars | T], OrigDynVar,
		StaticSet, HstoAs_Subst, Y0stoAs_Subst0, Y0stoAs_Subst) :-

	set__intersect(PossibleStaticVars, StaticSet, StaticVarSet),
	set__to_sorted_list(StaticVarSet, StaticVar),
	list__map(map__lookup(HstoAs_Subst), StaticVar, As),

		% There should only be one variable that is being
		% accumulated.
	As = [AccVar],

		%
		% If you have a chain of commutative calls,
		% only the first one will need to have the
		% accumulator placed in the call.
		%
	(
		map__insert(Y0stoAs_Subst0, OrigDynVar, AccVar,
				Y0stoAs_Subst1)
	->
		Y0stoAs_Subst2 = Y0stoAs_Subst1
	;
		Y0stoAs_Subst2 = Y0stoAs_Subst0
	),

	accumulator__update_Y0stoAs_subst_2(T, OrigDynVar, StaticSet,
			HstoAs_Subst, Y0stoAs_Subst2, Y0stoAs_Subst).


%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	%
	% accumulator__check_prevcalls
	%
	% We must ensure that if the dynamic variables which the current
	% variable is descended from have been used in any other
	% calls previously that all these calls have been commutative
	% including the current call.
	%
	% If this is true update the prev call map.
	%
:- pred accumulator__check_prevcalls(prog_var::in,
		pred_id::in, proc_id::in, commutative::in,
		prev_call_map::in, prev_call_map::out) is semidet.

accumulator__check_prevcalls(OrigVar, PredId, ProcId, Commutative,
		PrevCallMap0, PrevCallMap) :-
	(
		map__search(PrevCallMap0, OrigVar, PrevCall)
	->
			%
			% Only succeed if the current call is
			% commutative and all the previous calls are
			% commutative and to the same procedure.
			%
		PrevCall = prev_call(PredId, ProcId, yes),
		PrevCallMap = PrevCallMap0
	;
			%
			% There are no previous calls so set whether or
			% not the current call is commutative for all
			% the original variables.
			%
		map__det_insert(PrevCallMap0, OrigVar,
				prev_call(PredId, ProcId, Commutative),
				PrevCallMap)
	).


%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	%
	% If accumulator_is_associative is true, it returns a reordering
	% of the args to make it associative when executed left to right
	% and an indicator of whether or not the predicate is
	% commutative.
	%
:- pred accumulator__is_associative(pred_id::in, proc_id::in, prog_context::in,
		module_info::in, prog_vars::in, prog_vars::out,
		set(prog_var)::out, commutative::out,
		assoc_info::in, assoc_info::out) is semidet.

accumulator__is_associative(PredId, ProcId, Context, ModuleInfo,
		Args0, Args, PossibleStaticVars, Commutative,
		AssocInfo0, AssocInfo):-
	module_info_pred_proc_info(ModuleInfo, PredId, ProcId, 
			PredInfo, ProcInfo),

	proc_info_argmodes(ProcInfo, Modes),
	pred_info_get_assertions(PredInfo, Assertions),

	(
		commutativity_assertion(set__to_sorted_list(Assertions),
				ModuleInfo, Args0, PossibleStaticVars0)
	->
		check_modes(Args0, PossibleStaticVars0, Modes, ModuleInfo),
		Args = Args0,
		PossibleStaticVars = PossibleStaticVars0,
		AssocInfo = AssocInfo0,
		Commutative = yes
	;
		associativity_assertion(set__to_sorted_list(Assertions),
				ModuleInfo, Args0, VarA - VarB),

		PossibleStaticVars = set__list_to_set([VarA, VarB]),

			% Swap the order of the arguments
		list__map((pred(V0::in, V::out) is det :-
				(
					V0 = VarA
				->
					V = VarB
				;
					V0 = VarB
				->
					V = VarA
				;
					V = V0
				)
			), Args0, Args),

		check_modes(Args, PossibleStaticVars, Modes, ModuleInfo),

			% Determine what variables can be static
		pred_info_module(PredInfo, ModuleName),
		pred_info_name(PredInfo, PredName),
		pred_info_arity(PredInfo, Arity),
		(
			has_heuristic(ModuleName, PredName, Arity)
		->
				% If there is a heuristic for that call
				% then ensure that the call obeys
				% the heuristic that the static
				% variables are in certain positions.
				%
				% For example, a call to append in the
				% forward mode will have the following
				% types of variables: (static, dynamic,
				% dynamic).  After rearrangment that
				% order will be (dynamic, static,
				% dynamic).  Having a dynamic variable
				% in the first position will probably
				% take O(N) time to process while having
				% a static variable will probably take
				% O(1) time.  Therefore the complexity
				% of the predicate as a whole will
				% change, we must ensure that it changes
				% for the better.
				%
			heuristic(ModuleName, PredName, Arity, Args,
					MustBeStaticVars),
			assoc_info_static_set(StaticSet, AssocInfo0,
					AssocInfo),
			(StaticSet `intersect` MustBeStaticVars) `equal`
					MustBeStaticVars
		;
				% If no heuristic is known, then record
				% which variables were swapped and warn
				% the user.
			assoc_info_add_warning(w(Context, PredId, VarA, VarB),
					AssocInfo0, AssocInfo)
		),
		Commutative = no
	).


:- pred has_heuristic(module_name::in, string::in, arity::in) is semidet.

has_heuristic(unqualified("list"), "append", 3).

:- pred heuristic(module_name::in, string::in, arity::in, prog_vars::in,
		set(prog_var)::out) is semidet.

heuristic(unqualified("list"), "append", 3, [_Typeinfo, A, _B, _C], Set) :-
	set__list_to_set([A], Set).


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
		prog_vars::in, pair(prog_var)::out) is semidet.

associativity_assertion([AssertId | AssertIds], ModuleInfo, Args0, VarAB) :-
	(
		assertion__is_associativity_assertion(AssertId, ModuleInfo,
				Args0, VarAB0)
	->
		\+ associativity_assertion(AssertIds, ModuleInfo, Args0, _),
		VarAB = VarAB0
	;
		associativity_assertion(AssertIds, ModuleInfo, Args0, VarAB)
	).
	

	%
	% check_modes(Vs, CVs, Ms, MI)
	%
	% Given a list of variables, Vs, and associated modes, Ms, make
	% sure that each variable whose order can be rearranged (member of CVs)
	% has a mode where the instantiatedness of the the variable
	% doesn't change (ie an in mode).
	%
:- pred check_modes(prog_vars::in, set(prog_var)::in,
		list(mode)::in, module_info::in) is semidet.

check_modes([], _, _, _).
check_modes([V | Vs], PossibleStaticVars, [M | Ms], ModuleInfo) :-
	(
		set__member(V, PossibleStaticVars)
	->
		mode_get_insts(ModuleInfo, M, InitialInst, FinalInst),
		inst_matches_final(InitialInst, FinalInst, ModuleInfo)
	;
		true
	),
	check_modes(Vs, PossibleStaticVars, Ms, ModuleInfo).


%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred assoc_info_init(module_info::in, prog_vars::in, a_goals::in,
		a_goals::in, a_goal::in,
		subst::in, subst::in, assoc_info::out) is det.

assoc_info_init(ModuleInfo, HeadVars, DP, Compose, R, Y0stoYs_Subst,
		HstoAs_Subst, AssocInfo) :-
	DP = goal(Decompose, _InstMapBeforeDecomposeProcess),
	R = goal(_RecGoalExpr - RecGoalInfo, _InstMapBeforeRecursive),

	map__init(PrevCallMap),

		%
		% Set up the OrigDynVarMap
		%
	reverse_subst(Y0stoYs_Subst, YstoY0s_Subst),
	accumulator__vars_to_accumulate(HeadVars, Compose, ModuleInfo, Ys),
	list__map(map__lookup(YstoY0s_Subst), Ys, Y0s),

	assoc_list__from_corresponding_lists(Y0s, Y0s, AssocDynamicList),
	map__from_assoc_list(AssocDynamicList, OrigDynVarMap),

		%
		% Dynamic Set
		%
	set__list_to_set(Y0s, DynamicSet),

		%
		% Static Set
		%
	GetGoalInfos = (pred(Goal::in, GoalInfo::out) is det :-
				Goal = _ - GoalInfo),
	list__map(GetGoalInfos, Decompose, DPGoalInfos),
	list__map(goal_info_get_nonlocals, DPGoalInfos, DPNonLocals),
	set__list_to_set(DPNonLocals, DPNonLocalsPowerSet),
	set__power_union(DPNonLocalsPowerSet, StaticSet0),

	goal_info_get_nonlocals(RecGoalInfo, RecNonLocals),
	set__delete_list(RecNonLocals, Y0s, StaticSet1),

	set__union(StaticSet0, StaticSet1, StaticSet),

		%
		% Y0stoAs
		%
	map__init(Y0stoAs_Subst),

		%
		% Ys
		%
	set__list_to_set(Ys, YsSet),

	ErrorMessages = [],

	AssocInfo = assoc_info(StaticSet, DynamicSet,
			ModuleInfo, PrevCallMap, OrigDynVarMap,
			Y0stoAs_Subst, HstoAs_Subst, YsSet,
			ErrorMessages).

:- pred assoc_info_static_set(set(prog_var)::out,
		assoc_info::in, assoc_info::out) is det.
assoc_info_static_set(StaticSet, AssocInfo, AssocInfo) :-
	AssocInfo = assoc_info(StaticSet, _, _, _, _, _, _, _, _).

:- pred assoc_info_dynamic_set(set(prog_var)::out,
		assoc_info::in, assoc_info::out) is det.
assoc_info_dynamic_set(DynamicSet, AssocInfo, AssocInfo) :-
	AssocInfo = assoc_info(_, DynamicSet, _, _, _, _, _, _, _).

:- pred assoc_info_module_info(module_info::out,
		assoc_info::in, assoc_info::out) is det.
assoc_info_module_info(ModuleInfo, AssocInfo, AssocInfo) :-
	AssocInfo = assoc_info(_, _, ModuleInfo, _, _, _, _, _, _).

:- pred assoc_info_prev_call_map(prev_call_map::out,
		assoc_info::in, assoc_info::out) is det.
assoc_info_prev_call_map(PrevCallMap, AssocInfo, AssocInfo) :-
	AssocInfo = assoc_info(_, _, _, PrevCallMap, _, _, _, _, _).

:- pred assoc_info_orig_dynvar_map(orig_dynvar_map::out,
		assoc_info::in, assoc_info::out) is det.
assoc_info_orig_dynvar_map(OrigDynVarMap, AssocInfo, AssocInfo) :-
	AssocInfo = assoc_info(_, _, _, _, OrigDynVarMap, _, _, _, _).

:- pred assoc_info_Y0stoAs(subst::out,
		assoc_info::in, assoc_info::out) is det.
assoc_info_Y0stoAs(Y0stoAs_Subst, AssocInfo, AssocInfo) :-
	AssocInfo = assoc_info(_, _, _, _, _, Y0stoAs_Subst, _, _, _).

:- pred assoc_info_HstoAs(subst::out,
		assoc_info::in, assoc_info::out) is det.
assoc_info_HstoAs(HstoAs_Subst, AssocInfo, AssocInfo) :-
	AssocInfo = assoc_info(_, _, _, _, _, _, HstoAs_Subst, _, _).

:- pred assoc_info_Ys(set(prog_var)::out,
		assoc_info::in, assoc_info::out) is det.
assoc_info_Ys(Ys, AssocInfo, AssocInfo) :-
	AssocInfo = assoc_info(_, _, _, _, _, _, _, Ys, _).

:- pred assoc_info_warnings(warnings::out,
		assoc_info::in, assoc_info::out) is det.
assoc_info_warnings(Warnings, AssocInfo, AssocInfo) :-
	AssocInfo = assoc_info(_, _, _, _, _, _, _, _, Warnings).

/*
:- pred assoc_info_set_static_set(set(prog_var)::in, assoc_info::in,
		assoc_info::out) is det.
assoc_info_set_static_set(StaticSet, assoc_info(_, B, C, D, E, F, G, H, I),
		assoc_info(StaticSet, B, C, D, E, F, G, H, I)).
*/

:- pred assoc_info_set_dynamic_set(set(prog_var)::in, assoc_info::in,
		assoc_info::out) is det.
assoc_info_set_dynamic_set(DynamicSet, assoc_info(A, _, C, D, E, F, G, H, I),
		assoc_info(A, DynamicSet, C, D, E, F, G, H, I)).

:- pred assoc_info_set_prev_call_map(prev_call_map::in, assoc_info::in,
		assoc_info::out) is det.
assoc_info_set_prev_call_map(PrevCallMap, assoc_info(A, B, C, _, E, F, G, H, I),
		assoc_info(A, B, C, PrevCallMap, E, F, G, H, I)).

:- pred assoc_info_set_orig_dynvar_map(orig_dynvar_map::in, assoc_info::in,
		assoc_info::out) is det.
assoc_info_set_orig_dynvar_map(OrigDynMap,
		assoc_info(A, B, C, D, _, F, G, H, I),
		assoc_info(A, B, C, D, OrigDynMap, F, G, H, I)).

:- pred assoc_info_set_Y0stoAs(subst::in, assoc_info::in,
		assoc_info::out) is det.
assoc_info_set_Y0stoAs(Y0stoAs_Subst, assoc_info(A, B, C, D, E, _, G, H, I),
		assoc_info(A, B, C, D, E, Y0stoAs_Subst, G, H, I)).

:- pred assoc_info_add_warning(warning::in, assoc_info::in,
		assoc_info::out) is det.
assoc_info_add_warning(Warning,
		assoc_info(A, B, C, D, E, F, G, H, Warnings),
		assoc_info(A, B, C, D, E, F, G, H, [Warning | Warnings])).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred reverse_subst(subst::in, subst::out) is det.

reverse_subst(Subst0, Subst) :-
	map__to_assoc_list(Subst0, List0),
	assoc_list__reverse_members(List0, List),
	map__from_assoc_list(List, Subst).

:- pred chain_subst(subst::in, subst::in, subst::out) is det.

chain_subst(AtoB, BtoC, AtoC) :-
	map__keys(AtoB, Keys),
	chain_subst_2(Keys, AtoB, BtoC, AtoC).

:- pred chain_subst_2(list(A)::in, map(A, B)::in, map(B, C)::in,
		map(A, C)::out) is det.

chain_subst_2([], _, _, AtoC) :-
	map__init(AtoC).
chain_subst_2([A|As], AtoB, BtoC, AtoC) :-
	chain_subst_2(As, AtoB, BtoC, AtoC0),
	map__lookup(AtoB, A, B),
	map__lookup(BtoC, B, C),
	map__det_insert(AtoC0, A, C, AtoC).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
