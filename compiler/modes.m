%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% File: modes.nl.
% Main author: fjh.
%
% This file contains a mode-checker.
% Adapted from the mode-checker; still very incomplete.
% 
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module modes.
:- interface.
:- import_module hlds, io, prog_io.

:- pred modecheck(module_info, module_info, bool, io__state, io__state).
:- mode modecheck(input, output, output, di, uo).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module list, map, varset, prog_out, string, require, std_util.
:- import_module globals, getopt, options.

%-----------------------------------------------------------------------------%

	% XXX need to pass FoundError to all steps

modecheck(Module0, Module, FoundError) -->
	lookup_option(statistics, bool(Statistics)),
	lookup_option(verbose, bool(Verbose)),
	io__stderr_stream(StdErr),
	io__set_output_stream(StdErr, OldStream),
	maybe_report_stats(Statistics),

	maybe_write_string(Verbose,
		"% Checking for undefined insts and modes...\n"),
	check_undefined_modes(Module0, Module1),
	maybe_report_stats(Statistics),

	maybe_write_string(Verbose, "% Mode-checking clauses...\n"),
	check_pred_modes(Module1, Module, FoundError),
	maybe_report_stats(Statistics),

	io__set_output_stream(OldStream, _).

/*****
	{ FoundError = no },
	{ Module = Module1 },
*****/

%-----------------------------------------------------------------------------%
	
	% Mode-check the code for all the predicates in a module.

:- pred check_pred_modes(module_info, module_info, bool, io__state, io__state).
:- mode check_pred_modes(input, output, output, di, uo).

check_pred_modes(Module0, Module, FoundError) -->
	{ moduleinfo_predids(Module0, PredIds) },
	modecheck_pred_modes_2(PredIds, Module0, no, Module, FoundError).

%-----------------------------------------------------------------------------%

	% Iterate over the list of pred_ids in a module.

:- pred modecheck_pred_modes_2(list(pred_id), module_info, bool,
			module_info, bool, io__state, io__state).
:- mode modecheck_pred_modes_2(input, input, input, output, output, di, uo).

modecheck_pred_modes_2([], ModuleInfo, Error, ModuleInfo, Error) --> [].
modecheck_pred_modes_2([PredId | PredIds], ModuleInfo0, Error0,
				ModuleInfo, Error) -->
	{ moduleinfo_preds(ModuleInfo0, Preds0) },
	{ map__search(Preds0, PredId, PredInfo0) },
	{ predinfo_clauses_info(PredInfo0, ClausesInfo0) },
	{ ClausesInfo0 = clauses_info(_, _, _, Clauses0) },
	( { Clauses0 = [] } ->
		{ ModuleInfo1 = ModuleInfo0 }
	;
		lookup_option(very_verbose, bool(VeryVerbose)),
		( { VeryVerbose = yes } ->
			io__write_string("% Mode-checking predicate "),
			write_pred_id(PredId),
			io__write_string("\n")
		;
			[]
		),
		{ copy_clauses_to_procs(PredInfo0, PredInfo) },
		{ map__set(Preds0, PredId, PredInfo, Preds) },
		{ moduleinfo_set_preds(ModuleInfo0, Preds, ModuleInfo1) }
	),
/******
		% XXX fix here
	{ predinfo_argmodes(PredInfo1, ModeVarSet, ArgModes) },
	modecheck_clause_list(Clauses0, PredId, ModeVarSet, ArgModes,
		ModuleInfo0, Error0, Clauses, Error1),
*****/
	{ Error1 = Error0 },
	modecheck_pred_modes_2(PredIds, ModuleInfo1, Error1, ModuleInfo, Error).

%-----------------------------------------------------------------------------%

:- pred copy_clauses_to_procs(pred_info, pred_info).
:- mode copy_clauses_to_procs(input, output).

copy_clauses_to_procs(PredInfo0, PredInfo) :-
	predinfo_clauses_info(PredInfo0, ClausesInfo),
	predinfo_procedures(PredInfo0, Procs0),
	map__keys(Procs0, ProcIds),
	copy_clauses_to_procs_2(ProcIds, ClausesInfo, Procs0, Procs),
	predinfo_set_procedures(PredInfo0, Procs, PredInfo).

:- pred copy_clauses_to_procs_2(list(proc_id)::in, clauses_info::in,
				proc_table::in, proc_table::out).

copy_clauses_to_procs_2([], _, Procs, Procs).
copy_clauses_to_procs_2([ProcId | ProcIds], ClausesInfo, Procs0, Procs) :-
	ClausesInfo = clauses_info(VarSet, VarTypes, HeadVars, Clauses),
	select_matching_clauses(Clauses, ProcId, MatchingClauses),
	get_clause_goals(MatchingClauses, GoalList),
	(GoalList = [SingleGoal] ->
		Goal = SingleGoal
	;
		goalinfo_init(GoalInfo),
		Goal = disj(GoalList) - GoalInfo
	),
	map__lookup(Procs0, ProcId, Proc0),
	Proc0 = procedure(DeclaredDet, _, _, _, ArgModes, _, Context, CallInfo,
			InferredDet),
	Proc = procedure(DeclaredDet, VarSet, VarTypes, HeadVars, ArgModes,
			Goal, Context, CallInfo, InferredDet),
	map__set(Procs0, ProcId, Proc, Procs1),
	copy_clauses_to_procs_2(ProcIds, ClausesInfo, Procs1, Procs).

:- pred select_matching_clauses(list(clause), proc_id, list(clause)).
:- mode select_matching_clauses(input, input, output).

select_matching_clauses([], _, []).
select_matching_clauses([Clause | Clauses], ProcId, MatchingClauses) :-
	Clause = clause(ProcIds, _, _),
	( member(ProcId, ProcIds) ->
		MatchingClauses = [Clause | MatchingClauses1]
	;
		MatchingClauses = MatchingClauses1
	),
	select_matching_clauses(Clauses, ProcId, MatchingClauses1).

:- pred get_clause_goals(list(clause)::in, list(hlds__goal)::out) is det.

get_clause_goals([], []).
get_clause_goals([Clause | Clauses], [Goal | Goals]) :-
	Clause = clause(_, Goal, _),
	get_clause_goals(Clauses, Goals).


/*********************** ALL THIS IS COMMENTED OUT!

	% Iterate over the list of clauses for a predicate.
	% 

:- pred modecheck_clause_list(list(clause), pred_id, tvarset, list(mode),
		module_info, bool, list(clause), bool, io__state, io__state).
:- mode modecheck_clause_list(input, input, input, input, input, input,
			output, output, di, uo).

modecheck_clause_list([], _PredId, _ModeVarSet, _ArgModes, _ModuleInfo, Error,
			[], Error)
		--> [].
modecheck_clause_list([Clause0|Clauses0], PredId, ModeVarSet, ArgModes,
		ModuleInfo, Error0, [Clause|Clauses], Error) -->
	modecheck_clause(Clause0, PredId, ModeVarSet, ArgModes,
		ModuleInfo, Error0, Clause, Error1),
	modecheck_clause_list(Clauses0, PredId, ModeVarSet, ArgModes,
		ModuleInfo, Error1, Clauses, Error).

%-----------------------------------------------------------------------------%

	% Mode-check a single clause.

	% As we go through a clause, we determine the possible
	% mode assignments for the clause.  A mode assignment
	% is an assignment of a mode to each variable in the
	% clause.
	%
	% Note that this may cause exponential time & space usage
	% in the presence of overloading of predicates and/or
	% functors.  This is a potentially serious problem, but
	% there's no easy solution apparent.
	%
	% It would be more natural to use non-determinism to write
	% this code, and perhaps even more efficient.
	% But doing it deterministically would make bootstrapping more
	% difficult, and most importantly would make good error
	% messages very difficult.

	% XXX we should do manual garbage collection here

:- pred modecheck_clause(clause, pred_id, tvarset, list(mode), module_info,
		bool, clause, bool, io__state, io__state).
:- mode modecheck_clause(input, input, input, input, input, input,
		output, output, di, uo).

modecheck_clause(Clause0, PredId, ModeVarSet, ArgModes, ModuleInfo, Error0,
		Clause, Error, IOState0, IOState) :-

		% initialize the modeinfo
		% XXX abstract clause/6

	Clause0 = clause(Modes, VarSet, _DummyVarModes, HeadVars, Body,
			Context),
	modeinfo_init(IOState0, ModuleInfo, PredId, Context, ModeVarSet,
			VarSet, ModeInfo0),

		% modecheck the clause - first the head unification, and
		% then the body

	modecheck_var_has_mode_list(HeadVars, ArgModes, ModeInfo0, ModeInfo1),
	modecheck_goal(Body, ModeInfo1, ModeInfo),

		% finish up

	modeinfo_get_mode_assign_set(ModeInfo, ModeAssignSet),
	modeinfo_get_io_state(ModeInfo, IOState1),
	modecheck_finish_up(ModeAssignSet, ModeInfo, Error0, VarModes, Error,
			IOState1, IOState),
	Clause = clause(Modes, VarSet, VarModes, HeadVars, Body, Context).

	% At this stage, there are three possibilities.
	% There are either zero, one, or multiple mode assignments
	% for the clause.  In the first case, we have already
	% issued an error message.  In the second case, the
	% clause is mode-correct.  In the third case, we have to
	% issue an error message here.

:- pred modecheck_finish_up(mode_assign_set, mode_info, bool, map(var, mode),
		bool, io__state, io__state).
:- mode modecheck_finish_up(input, input, input, output, output, di, uo).

modecheck_finish_up([], _ModeInfo, _Error, VarModes, yes) -->
	{ map__init(VarModes) }.
modecheck_finish_up([ModeAssign], _ModeInfo, Error, VarModes, Error) -->
	{ mode_assign_get_var_modes(ModeAssign, VarModes) }.
modecheck_finish_up([ModeAssign1, ModeAssign2 | _], ModeInfo, _Error,
		VarModes1, yes) -->
	{ mode_assign_get_var_modes(ModeAssign1, VarModes1) },
	report_ambiguity_error(ModeInfo, ModeAssign1, ModeAssign2).

%-----------------------------------------------------------------------------%

:- pred modecheck_goal(hlds__goal, mode_info, mode_info).
:- mode modecheck_goal(input, modeinfo_di, modeinfo_uo).

modecheck_goal(Goal - _GoalInfo, ModeInfo0, ModeInfo) :-
	(modecheck_goal_2(Goal, ModeInfo0, ModeInfo)).

:- pred modecheck_goal_2(hlds__goal_expr, mode_info, mode_info).
:- mode modecheck_goal_2(input, modeinfo_di, modeinfo_uo).

modecheck_goal_2(conj(List)) -->
	%%% checkpoint("conj"),
	modecheck_goal_list(List).
modecheck_goal_2(disj(List)) -->
	%%% checkpoint("disj"),
	modecheck_goal_list(List).
modecheck_goal_2(if_then_else(_Vs, A, B, C)) -->
	%%% checkpoint("if"),
	modecheck_goal(A),
	%%% checkpoint("then"),
	modecheck_goal(B),
	%%% checkpoint("else"),
	modecheck_goal(C).
modecheck_goal_2(not(_Vs, A)) -->
	%%% checkpoint("not"),
	modecheck_goal(A).
modecheck_goal_2(some(_Vs, G)) -->
	%%% checkpoint("some"),
	modecheck_goal(G).
modecheck_goal_2(all(_Vs, G)) -->
	%%% checkpoint("all"),
	modecheck_goal(G).
modecheck_goal_2(call(PredId, _Mode, Args, _Builtin)) -->
	%%% checkpoint("call"),
	modecheck_call_pred(PredId, Args).
modecheck_goal_2(unify(A, B, _Mode, _Info)) -->
	%%% checkpoint("unify"),
	modecheck_unification(A, B).

%-----------------------------------------------------------------------------%

:- pred modecheck_goal_list(list(hlds__goal), mode_info, mode_info).
:- mode modecheck_goal_list(input, modeinfo_di, modeinfo_uo).

modecheck_goal_list([]) --> [].
modecheck_goal_list([Goal | Goals]) -->
	modecheck_goal(Goal),
	modecheck_goal_list(Goals).

%-----------------------------------------------------------------------------%

:- pred modecheck_call_pred(pred_id, list(term), mode_info, mode_info).
:- mode modecheck_call_pred(input, input, modeinfo_di, modeinfo_uo).

	% XXX we should handle overloading of predicates

modecheck_call_pred(PredId, Args, ModeInfo0, ModeInfo) :-
		% look up the called predicate's arg modes
	modeinfo_get_preds(ModeInfo0, Preds),
	( % if some [PredInfo]
		map__search(Preds, PredId, PredInfo)
	->
		predinfo_arg_modes(PredInfo, PredModeVarSet, PredArgModes0),

			% rename apart the mode variables in called
			% predicate's arg modes
			% (optimize for the common case of
			% a non-polymorphic predicate)
		( varset__is_empty(PredModeVarSet) ->
			PredArgModes = PredArgModes0,
			ModeInfo1 = ModeInfo0
		;
			rename_apart(ModeInfo0, PredModeVarSet, PredArgModes0,
					ModeInfo1, PredArgModes)
		),
			% unify the modes of the call arguments with the
			% called predicates' arg modes
		modecheck_term_has_mode_list(Args, PredArgModes, ModeInfo1,
				ModeInfo)
	;
		modeinfo_get_io_state(ModeInfo0, IOState0),
		modeinfo_get_predid(ModeInfo0, CallingPredId),
		modeinfo_get_context(ModeInfo0, Context),
		report_error_undef_pred(CallingPredId, Context, PredId,
			IOState0, IOState),
		modeinfo_set_io_state(ModeInfo0, IOState, ModeInfoB1),
		modeinfo_set_found_error(ModeInfoB1, yes, ModeInfoB2),
		modeinfo_set_mode_assign_set(ModeInfoB2, [], ModeInfo)
	).

%-----------------------------------------------------------------------------%

	% Rename apart the mode variables in called predicate's arg modes.
	%
	% Each mode_assign has it's own set of mode variables, but these
	% are supposed to stay in synch with each other.  We need to
	% iterate over the set of mode_assigns, but we check that
	% the resulting renamed apart list of predicate arg modes
	% is the same for each mode_assign (i.e. that the tvarsets
	% were indeed in synch).

:- pred rename_apart(mode_info, tvarset, list(mode), mode_info, list(mode)).
:- mode rename_apart(modeinfo_di, input, input, modeinfo_uo, output).

rename_apart(ModeInfo0, PredModeVarSet, PredArgModes0, ModeInfo, PredArgModes)
		:-
	modeinfo_get_mode_assign_set(ModeInfo0, ModeAssignSet0),
	( ModeAssignSet0 = [ModeAssign0 | ModeAssigns0] ->
			% process the first mode_assign and get
			% the resulting PredArgModes
		mode_assign_rename_apart(ModeAssign0, PredModeVarSet,
				PredArgModes0, ModeAssign, PredArgModes),
			% process the remaining mode_assigns and check
			% that they produce matching PredArgModes
		rename_apart_2(ModeAssigns0, PredModeVarSet, PredArgModes0,
				  ModeAssigns, PredArgModes),
		ModeAssignSet = [ModeAssign | ModeAssigns],
		modeinfo_set_mode_assign_set(ModeInfo0, ModeAssignSet, ModeInfo)
	;
		ModeInfo = ModeInfo0
	).

:- pred rename_apart_2(mode_assign_set, tvarset, list(mode),
			mode_assign_set, list(mode)).
:- mode rename_apart_2(input, input, input, output, input).

rename_apart_2([], _, _, [], _).
rename_apart_2([ModeAssign0 | ModeAssigns0], PredModeVarSet, PredArgModes0,
		[ModeAssign | ModeAssigns], PredArgModes) :-
	mode_assign_rename_apart(ModeAssign0, PredModeVarSet, PredArgModes0,
			ModeAssign, NewPredArgModes),
	(PredArgModes = NewPredArgModes ->
		true
	;
		error("problem synchronizing mode vars")
	),
	rename_apart_2(ModeAssigns0, PredModeVarSet, PredArgModes0,
			ModeAssigns, PredArgModes).

:- pred mode_assign_rename_apart(mode_assign, tvarset, list(mode),
			mode_assign, list(mode)).
:- mode mode_assign_rename_apart(input, input, input, output, output).

mode_assign_rename_apart(ModeAssign0, PredModeVarSet, PredArgModes0,
		ModeAssign, PredArgModes) :-
	mode_assign_get_modevarset(ModeAssign0, ModeVarSet0),
	varset__merge(ModeVarSet0, PredModeVarSet, PredArgModes0,
			  ModeVarSet, PredArgModes),
	mode_assign_set_modevarset(ModeAssign0, ModeVarSet, ModeAssign).

%-----------------------------------------------------------------------------%

	% Given a list of variables and a list of modes, ensure
	% that each variable has the corresponding mode.

:- pred modecheck_var_has_mode_list(list(var), list(mode), mode_info,
					mode_info).
:- mode modecheck_var_has_mode_list(input, input, input, output).

modecheck_var_has_mode_list([], []) --> [].
modecheck_var_has_mode_list([Var|Vars], [Mode|Modes]) -->
	modecheck_var_has_mode(Var, Mode),
	modecheck_var_has_mode_list(Vars, Modes).

:- pred modecheck_var_has_mode(var, mode, mode_info, mode_info).
:- mode modecheck_var_has_mode(input, input, modeinfo_di, modeinfo_uo).

modecheck_var_has_mode(VarId, Mode, ModeInfo0, ModeInfo) :-
	modeinfo_get_mode_assign_set(ModeInfo0, ModeAssignSet0),
	modeinfo_get_varset(ModeInfo0, VarSet),
	modecheck_var_has_mode_2(ModeAssignSet0, VarId, Mode, [],
		ModeAssignSet),
	(
		ModeAssignSet = [],
		(not ModeAssignSet0 = [])
	->
		modeinfo_get_io_state(ModeInfo0, IOState0),
		modeinfo_get_context(ModeInfo0, Context),
		modeinfo_get_predid(ModeInfo0, PredId),
		get_mode_stuff(ModeAssignSet0, VarId, ModeStuffList),
		report_error_var(PredId, Context, VarSet, VarId, ModeStuffList,
				Mode, ModeAssignSet0, IOState0, IOState),
		modeinfo_set_io_state(ModeInfo0, IOState, ModeInfo1),
		modeinfo_set_found_error(ModeInfo1, yes, ModeInfo2),
		modeinfo_set_mode_assign_set(ModeInfo2, ModeAssignSet, ModeInfo)
	;
		modeinfo_set_mode_assign_set(ModeInfo0, ModeAssignSet, ModeInfo)
	).

	% Given a mode assignment set and a variable id,
	% return the list of possible different modes for the variable.

:- mode mode_stuff ---> mode_stuff(mode, tvarset, tsubst).

:- pred get_mode_stuff(mode_assign_set, var, list(mode_stuff)).
:- mode get_mode_stuff(input, input, output).
get_mode_stuff([], _VarId, []).
get_mode_stuff([ModeAssign | ModeAssigns], VarId, L) :-
	get_mode_stuff(ModeAssigns, VarId, L0),
	mode_assign_get_mode_bindings(ModeAssign, ModeBindings),
	mode_assign_get_modevarset(ModeAssign, TVarSet),
	mode_assign_get_var_modes(ModeAssign, VarModes),
	( %%% if some [Mode0]
		map__search(VarModes, VarId, Mode0)
	->
		Mode = Mode0
	;
		% this shouldn't happen - how can a variable which has
		% not yet been assigned a mode variable fail to have
		% the correct mode?
		error("problem in mode unification")
	),
	ModeStuff = mode_stuff(Mode, TVarSet, ModeBindings),
	(
		member_chk(ModeStuff, L0)
	->
		L = L0
	;
		L = [ModeStuff | L0]
	).

:- pred modecheck_var_has_mode_2(mode_assign_set, var, mode,
				mode_assign_set, mode_assign_set).
:- mode modecheck_var_has_mode_2(input, input, input, input, output).

modecheck_var_has_mode_2([], _, _) --> [].
modecheck_var_has_mode_2([ModeAssign0 | ModeAssignSet0], VarId, Mode) -->
	mode_assign_var_has_mode(ModeAssign0, VarId, Mode),
	modecheck_var_has_mode_2(ModeAssignSet0, VarId, Mode).

:- pred mode_assign_var_has_mode(mode_assign, var, mode,
				mode_assign_set, mode_assign_set).
:- mode mode_assign_var_has_mode(input, input, input, input, output).

mode_assign_var_has_mode(ModeAssign0, VarId, Mode,
		ModeAssignSet0, ModeAssignSet) :-
	mode_assign_get_var_modes(ModeAssign0, VarModes0),
	( %%% if some [VarMode]
		map__search(VarModes0, VarId, VarMode)
	->
		( %%% if some [ModeAssign1]
			mode_assign_unify_mode(ModeAssign0, VarMode, Mode,
					ModeAssign1)
		->
			ModeAssignSet = [ModeAssign1 | ModeAssignSet0]
		;
			ModeAssignSet = ModeAssignSet0
		)
	;
		map__set(VarModes0, VarId, Mode, VarModes),
		mode_assign_set_var_modes(ModeAssign0, VarModes, ModeAssign),
		ModeAssignSet = [ModeAssign | ModeAssignSet0]
	).

%-----------------------------------------------------------------------------%
	
:- pred modecheck_term_has_mode_list(list(term), list(mode), 
					mode_info, mode_info).
:- mode modecheck_term_has_mode_list(input, input, modeinfo_di, modeinfo_uo).

modecheck_term_has_mode_list([], []) --> [].
modecheck_term_has_mode_list([Arg | Args], [Mode | Modes]) -->
	modecheck_term_has_mode(Arg, Mode),
	modecheck_term_has_mode_list(Args, Modes).

:- pred modecheck_term_has_mode(term, mode, mode_info, mode_info).
:- mode modecheck_term_has_mode(input, input, modeinfo_di, modeinfo_uo).

modecheck_term_has_mode(term_variable(Var), Mode, ModeInfo0, ModeInfo) :-
	modecheck_var_has_mode(Var, Mode, ModeInfo0, ModeInfo).

modecheck_term_has_mode(term_functor(F, As, C), Mode, ModeInfo0, ModeInfo) :-
	length(As, Arity),
	modeinfo_get_ctor_list(ModeInfo0, F, Arity, ConsDefnList),
	(ConsDefnList = [] ->
	    modeinfo_get_io_state(ModeInfo0, IOState0),
	    modeinfo_get_predid(ModeInfo0, PredId),
	    report_error_undef_cons(PredId, C, F, Arity, IOState0, IOState),
	    modeinfo_set_io_state(ModeInfo0, IOState, ModeInfo1),
	    modeinfo_set_found_error(ModeInfo1, yes, ModeInfo2),
	    modeinfo_set_mode_assign_set(ModeInfo2, [], ModeInfo)
	;
	    modeinfo_get_mode_assign_set(ModeInfo0, ModeAssignSet0),
	    modecheck_cons_has_mode(ModeAssignSet0, ConsDefnList, As, Mode,
			ModeInfo0, [], ModeAssignSet),
	    (
		ModeAssignSet = [],
		(\+ ModeAssignSet0 = [])
	    ->
		modeinfo_get_io_state(ModeInfo0, IOState0),
		modeinfo_get_predid(ModeInfo0, PredId),
		modeinfo_get_varset(ModeInfo0, VarSet),
		report_error_cons(PredId, C, VarSet, F, As, Mode,
					ModeAssignSet, IOState0, IOState),
		modeinfo_set_io_state(ModeInfo0, IOState, ModeInfo1),
		modeinfo_set_found_error(ModeInfo1, yes, ModeInfo2),
		modeinfo_set_mode_assign_set(ModeInfo2, ModeAssignSet, ModeInfo)
	    ;
		modeinfo_set_mode_assign_set(ModeInfo0, ModeAssignSet, ModeInfo)
	    )
	).

%-----------------------------------------------------------------------------%

	% Check that a constructor has the specified mode
	% and that the arguments of the constructor have the appropriate
	% modes for that constructor.
	% We do this by iterating over all the possible current
	% mode assignments. 
	% For each possible current mode assignment, we produce a
	% list of the possible resulting mode assignments after
	% we have unified the mode of this constructor with
	% the specified mode.

:- pred modecheck_cons_has_mode(mode_assign_set, list(hlds__cons_defn),
		list(term), mode, mode_info, mode_assign_set, mode_assign_set).
:- mode modecheck_cons_has_mode(input, input, input, input,
		modeinfo_ui, input, output).

modecheck_cons_has_mode([], _, _, _, _) --> [].
modecheck_cons_has_mode([ModeAssign|ModeAssigns], ConsDefnList, Args, Mode, 
		ModeInfo) -->
	mode_assign_cons_has_mode(ConsDefnList, ModeAssign, Args, Mode,
		ModeInfo),
	modecheck_cons_has_mode(ModeAssigns, ConsDefnList, Args, Mode,
		ModeInfo).

%-----------------------------------------------------------------------------%

	% For each possible constructor which matches the
	% term (overloading means that there may be more than one),
	% if this constructor matches the specified mode and
	% the modes of it's arguments are ok, then add the resulting
	% mode assignment to the mode assignment set.

:- pred mode_assign_cons_has_mode(list(hlds__cons_defn), mode_assign,
		list(term), mode, mode_info, mode_assign_set, mode_assign_set).
:- mode mode_assign_cons_has_mode(input, input, input, input, 
		modeinfo_ui, input, output).

mode_assign_cons_has_mode([], _ModeAssign0, _Args, _Mode, _ModeInfo) -->
	[].
mode_assign_cons_has_mode([ConsDefn | ConsDefns], ModeAssign0, Args, Mode,
		ModeInfo) -->
	mode_assign_cons_has_mode_2(ConsDefn, ModeAssign0, Args, Mode,
		ModeInfo),
	mode_assign_cons_has_mode(ConsDefns, ModeAssign0, Args, Mode, ModeInfo).

:- pred mode_assign_cons_has_mode_2(hlds__cons_defn, mode_assign, list(term),
		mode, mode_info, mode_assign_set, mode_assign_set).
:- mode mode_assign_cons_has_mode_2(input, input, input, input,
		modeinfo_ui, input, output).

mode_assign_cons_has_mode_2(ConsDefn, ModeAssign0, Args, Mode, ModeInfo,
		ModeAssignSet0, ModeAssignSet) :-

	get_cons_stuff(ConsDefn, ModeAssign0, ModeInfo,
			ConsMode, ArgModes, ModeAssign1),

	( mode_assign_unify_mode(ModeAssign1, ConsMode, Mode, ModeAssign2) ->
			% check the modes of the arguments
		mode_assign_term_has_mode_list(Args, ArgModes, ModeAssign2,
			ModeInfo, ModeAssignSet0, ModeAssignSet)
	;
		ModeAssignSet = ModeAssignSet0
	).

%-----------------------------------------------------------------------------%

	% mode_assign_term_has_mode_list(Terms, Modes, ModeAssign, ModeInfo,
	%		ModeAssignSet0, ModeAssignSet):
	% 	Let TAs = { TA | TA is a an extension of ModeAssign
	%		    	 for which the modes of the Terms unify with
	%		    	 their respective Modes },
	% 	append(TAs, ModeAssignSet0, ModeAssignSet).

:- pred mode_assign_term_has_mode_list(list(term), list(mode), mode_assign,
			mode_info, mode_assign_set, mode_assign_set).
:- mode mode_assign_term_has_mode_list(input, input, input,
			modeinfo_ui, input, output).

mode_assign_term_has_mode_list([], [], ModeAssign, _,
		ModeAssignSet, [ModeAssign|ModeAssignSet]).
mode_assign_term_has_mode_list([Arg | Args], [Mode | Modes], ModeAssign0,
		ModeInfo, ModeAssignSet0, ModeAssignSet) :-
	mode_assign_term_has_mode(Arg, Mode, ModeAssign0, ModeInfo,
		[], ModeAssignSet1),
	mode_assign_list_term_has_mode_list(ModeAssignSet1,
		Args, Modes, ModeInfo, ModeAssignSet0, ModeAssignSet).

	% mode_assign_list_term_has_mode_list(TAs, Terms, Modes, 
	%		ModeInfo, ModeAssignSet0, ModeAssignSet):
	% 	Let TAs2 = { TA | TA is a an extension of a member of TAs
	%		    	  for which the modes of the Terms unify with
	%		    	  their respective Modes },
	% 	append(TAs, ModeAssignSet0, ModeAssignSet).

:- pred mode_assign_list_term_has_mode_list(mode_assign_set, list(term),
		list(mode), mode_info, mode_assign_set, mode_assign_set).
:- mode mode_assign_list_term_has_mode_list(input, input, input,
			modeinfo_ui, input, output).

mode_assign_list_term_has_mode_list([], _, _, _) --> [].
mode_assign_list_term_has_mode_list([TA | TAs], Args, Modes, ModeInfo) -->
	mode_assign_term_has_mode_list(Args, Modes, TA, ModeInfo),
	mode_assign_list_term_has_mode_list(TAs, Args, Modes, ModeInfo).
	
:- pred mode_assign_term_has_mode(term, mode, mode_assign,
			mode_info, mode_assign_set, mode_assign_set).
:- mode mode_assign_term_has_mode(input, input, input,
			modeinfo_ui, input, output).

mode_assign_term_has_mode(term_variable(V), Mode, ModeAssign, _ModeInfo) -->
	mode_assign_var_has_mode(ModeAssign, V, Mode).
mode_assign_term_has_mode(term_functor(F, Args, _Context), Mode, ModeAssign,
		ModeInfo) -->
	{ length(Args, Arity) },
	{ modeinfo_get_ctor_list(ModeInfo, F, Arity, ConsDefnList) },
	mode_assign_cons_has_mode(ConsDefnList, ModeAssign, Args, Mode,
		ModeInfo).

%-----------------------------------------------------------------------------%

	% used for debugging

:- pred checkpoint(string, mode_info, mode_info).
:- mode checkpoint(input, modeinfo_di, modeinfo_uo).

%%% checkpoint(_, T, T).
checkpoint(Msg, T0, T) :-
	modeinfo_get_io_state(T0, I0),
	checkpoint_2(Msg, T0, I0, I),
	modeinfo_set_io_state(T0, I, T).

:- pred checkpoint_2(string, mode_info, io__state, io__state).
:- mode checkpoint_2(input, modeinfo_ui, di, uo).

checkpoint_2(Msg, T0) -->
	io__write_string("At "),
	io__write_string(Msg),
	io__write_string(": "),
	%%% { report_stats },
	io__write_string("\n"),
	{ modeinfo_get_mode_assign_set(T0, ModeAssignSet) },
	{ modeinfo_get_varset(T0, VarSet) },
	write_mode_assign_set(ModeAssignSet, VarSet).

%-----------------------------------------------------------------------------%

	% Mode check a unification.
	% Get the mode assignment set from the mode info and then just
	% iterate over all the possible mode assignments.

:- pred modecheck_unification(term, term, mode_info, mode_info).
:- mode modecheck_unification(input, input, modeinfo_di, modeinfo_uo).

modecheck_unification(X, Y, ModeInfo0, ModeInfo) :-
	modeinfo_get_mode_assign_set(ModeInfo0, ModeAssignSet0),
	modecheck_unification_2(ModeAssignSet0, X, Y, ModeInfo0,
		[], ModeAssignSet),
		% XXX report errors properly!!
	( ModeAssignSet = [], not (ModeAssignSet0 = []) ->
		modeinfo_get_predid(ModeInfo0, PredId),
		modeinfo_get_context(ModeInfo0, Context),
		modeinfo_get_varset(ModeInfo0, VarSet),
		modeinfo_get_io_state(ModeInfo0, IOState0),
		report_error_unif(PredId, Context, VarSet, X, Y,
				ModeAssignSet0, IOState0, IOState1),
		modeinfo_set_io_state(ModeInfo0, IOState1, ModeInfo1),
		modeinfo_set_found_error(ModeInfo1, yes, ModeInfo2)
	;
		ModeInfo2 = ModeInfo0
	),
	modeinfo_set_mode_assign_set(ModeInfo2, ModeAssignSet, ModeInfo).


	% iterate over all the possible mode assignments.

:- pred modecheck_unification_2(mode_assign_set, term, term,
				mode_info, mode_assign_set, mode_assign_set).
:- mode modecheck_unification_2(input, input, input,
				modeinfo_ui, input, output).

modecheck_unification_2([], _, _, _) --> [].
modecheck_unification_2([ModeAssign0 | ModeAssigns0], X, Y, ModeInfo) -->
	mode_assign_unify_term(X, Y, ModeAssign0, ModeInfo),
	modecheck_unification_2(ModeAssigns0, X, Y, ModeInfo).
	
%-----------------------------------------------------------------------------%

	% Mode-check the unification of two terms,
	% and update the mode assignment.
	% ModeAssign0 is the mode assignment we are updating,
	% ModeAssignSet0 is an accumulator for the list of possible
	% mode assignments so far, and ModeAssignSet is ModeAssignSet plus
	% any mode assignment(s) resulting from ModeAssign0 and this
	% unification.

:- pred mode_assign_unify_term(term, term, mode_assign, mode_info,
				mode_assign_set, mode_assign_set).
:- mode mode_assign_unify_term(input, input, input, modeinfo_ui, input, output).

	% NU-Prolog indexing
:- mode_assign_unify_term(T1, T2, _, _, _, _) when T1 and T2.

mode_assign_unify_term(term_variable(X), term_variable(Y), ModeAssign0,
		_ModeInfo, ModeAssignSet0, ModeAssignSet) :-
	mode_assign_get_var_modes(ModeAssign0, VarModes0),
	( %%% if some [ModeX]
		map__search(VarModes0, X, ModeX)
	->
		( %%% if some [ModeY]
			map__search(VarModes0, Y, ModeY)
		->
			% both X and Y already have modes - just
			% unify their modes
			( %%% if some [ModeAssign3]
				mode_assign_unify_mode(ModeAssign0, ModeX,
					ModeY, ModeAssign3)
			->
				ModeAssignSet = [ModeAssign3 | ModeAssignSet0]
			;
				ModeAssignSet = ModeAssignSet0
			)
		;
			% Y is a fresh variable which hasn't been
			% assigned a mode yet
			map__set(VarModes0, Y, ModeX, VarModes),
			mode_assign_set_var_modes(ModeAssign0, VarModes,
				ModeAssign),
			ModeAssignSet = [ModeAssign | ModeAssignSet0]
		)
	;
		( %%% if some [ModeY2]
			map__search(VarModes0, Y, ModeY2)
		->
			% X is a fresh variable which hasn't been
			% assigned a mode yet
			map__set(VarModes0, X, ModeY2, VarModes),
			mode_assign_set_var_modes(ModeAssign0, VarModes,
				ModeAssign),
			ModeAssignSet = [ModeAssign | ModeAssignSet0]
		;
			% both X and Y are fresh variables -
			% introduce a fresh mode variable to represent
			% their mode
			mode_assign_get_modevarset(ModeAssign0, ModeVarSet0),
			varset__new_var(ModeVarSet0, ModeVar, ModeVarSet),
			mode_assign_set_modevarset(ModeAssign0, ModeVarSet,
				ModeAssign1),
			Mode = term_variable(ModeVar),
			map__set(VarModes0, X, Mode, VarModes1),
			map__set(VarModes1, Y, Mode, VarModes),
			mode_assign_set_var_modes(ModeAssign1, VarModes,
				ModeAssign),
			ModeAssignSet = [ModeAssign | ModeAssignSet0]
		)
	).

mode_assign_unify_term(term_functor(Functor, Args, _), term_variable(Y),
		ModeAssign0, ModeInfo, ModeAssignSet0, ModeAssignSet) :-
	length(Args, Arity),
	modeinfo_get_ctor_list(ModeInfo, Functor, Arity, ConsDefnList),
	mode_assign_unify_var_functor(ConsDefnList, Args, Y, ModeAssign0,
		ModeInfo, ModeAssignSet0, ModeAssignSet).

mode_assign_unify_term(term_variable(Y), term_functor(F, As, _), ModeAssign0,
		ModeInfo, ModeAssignSet0, ModeAssignSet) :-
	mode_assign_unify_term(term_functor(F, As, _), term_variable(Y),
		ModeAssign0, ModeInfo, ModeAssignSet0, ModeAssignSet).
	
mode_assign_unify_term(term_functor(FX, AsX, _), term_functor(FY, AsY, _),
		ModeAssign0, ModeInfo, ModeAssignSet0, ModeAssignSet) :-
	    % XXX we should handle this properly
	error("XXX not implemented: unification of term with term\n"),
	ModeAssignSet = ModeAssignSet0.

%-----------------------------------------------------------------------------%

	% Mode-check the unification of a variable with a functor:
	% for each possible mode of the constructor,
	% unify the mode of the variable with the mode of
	% the constructor and if this succeeds insert that
	% mode assignment into the mode assignment set.

:- pred mode_assign_unify_var_functor(list(hlds__cons_defn), list(term),
		var, mode_assign,
		mode_info, mode_assign_set, mode_assign_set).
:- mode mode_assign_unify_var_functor(input, input, input, input,
		modeinfo_ui, input, output).

mode_assign_unify_var_functor([], _, _, _, _, ModeAssignSet, ModeAssignSet).
mode_assign_unify_var_functor([ConsDefn | ConsDefns], Args, Y, ModeAssign0,
		ModeInfo, ModeAssignSet0, ModeAssignSet) :-

	get_cons_stuff(ConsDefn, ModeAssign0, ModeInfo,
			ConsMode, ArgModes, ModeAssign1),
	
		% unify the mode of Var with the mode of the constructor
	mode_assign_get_var_modes(ModeAssign1, VarModes0),
	( %%% if some [ModeY]
		map__search(VarModes0, Y, ModeY)
	->
		( %%% if some [ModeAssign2]
			mode_assign_unify_mode(ModeAssign1, ConsMode, ModeY,
						ModeAssign2)
		->
			% check that the modes of the arguments matches the
			% specified arg modes for this constructor
			mode_assign_term_has_mode_list(Args, ArgModes,
				ModeAssign2, ModeInfo,
				ModeAssignSet0, ModeAssignSet1)
		;
			% the top-level modes didn't unify - no need to
			% check the modes of the arguments, since this
			% mode-assignment has already been rules out
			ModeAssignSet1 = ModeAssignSet0
		)
	;
		map__set(VarModes0, Y, ConsMode, VarModes),
		mode_assign_set_var_modes(ModeAssign1, VarModes, ModeAssign3),

			% check that the modes of the arguments matches the
			% specified arg modes for this constructor
		mode_assign_term_has_mode_list(Args, ArgModes, ModeAssign3,
			ModeInfo, ModeAssignSet0, ModeAssignSet1)
	),

		% recursively handle all the other possible constructors
		% that match this functor.
	mode_assign_unify_var_functor(ConsDefns, Args, Y, ModeAssign0,
		ModeInfo, ModeAssignSet1, ModeAssignSet).

%-----------------------------------------------------------------------------%

	% Given an hlds__cons_defn, construct a mode for the
	% constructor and a list of modes of the arguments.
	% First we construct the mode and the arg modes using
	% the information in the hlds__cons_defn and the information
	% in the hlds mode table entry for the cons' mode.
	% Then we rename these apart from the current mode_assign's
	% modevarset.
	%
	% XXX abstract the use of hlds__cons_defn/3 and hlds__mode_defn/5

:- pred get_cons_stuff(hlds__cons_defn, mode_assign, mode_info,
			mode, list(mode), mode_assign).
:- mode get_cons_stuff(input, input, input, output, output, output).

get_cons_stuff(ConsDefn, ModeAssign0, ModeInfo, ConsMode, ArgModes,
			ModeAssign) :-

	ConsDefn = hlds__cons_defn(ArgModes0, ModeId, Context),

	( is_builtin_mode(ModeId) ->
		% XXX assumes arity = 0
		varset__init(ConsModeVarSet),
		ConsModeParams = []
	;
		modeinfo_get_modes(ModeInfo, Modes),
		map__search(Modes, ModeId, ModeDefn),
		ModeDefn = hlds__mode_defn(ConsModeVarSet, ConsModeParams,
						_, _, _)
	),

	ModeId = QualifiedName - _Arity,
	unqualify_name(QualifiedName, Name),
	ConsMode0 = term_functor(term_atom(Name), ConsModeParams, Context),

	% Rename apart the mode vars in the mode of the constructor
	% and the modes of it's arguments.
	% (Optimize the common case of a non-polymorphic mode)
	(ConsModeParams = [] ->
		ConsMode = ConsMode0,
		ArgModes = ArgModes0,
		ModeAssign = ModeAssign0
	;
		mode_assign_rename_apart(ModeAssign0, ConsModeVarSet,
			[ConsMode0 | ArgModes0],
			ModeAssign, [ConsMode | ArgModes])
	).

%-----------------------------------------------------------------------------%

	% Unify (with occurs check) two modes in a mode assignment 
	% and update the mode bindings.

:- pred mode_assign_unify_mode(mode_assign, mode, mode, mode_assign).
:- mode mode_assign_unify_mode(input, input, input, output).

mode_assign_unify_mode(ModeAssign0, X, Y, ModeAssign) :-
	mode_assign_get_mode_bindings(ModeAssign0, ModeBindings0),
	mode_unify(X, Y, ModeBindings0, ModeBindings),
	mode_assign_set_mode_bindings(ModeAssign0, ModeBindings, ModeAssign).

%-----------------------------------------------------------------------------%

	% Unify (with occurs check) two modes with respect to a mode
	% substitution and update the mode bindings.
	% (Modes are represented as terms, but we can't just use term__unify
	% because we need to handle equivalent modes).

:- mode_unify(X, Y, _, _) when X and Y.		% NU-Prolog indexing

:- pred mode_unify(mode, mode, substitution, substitution).
:- mode mode_unify(input, input, input, output).

mode_unify(term_variable(X), term_variable(Y), Bindings0, Bindings) :-
	( %%% if some [BindingOfX]
		map__search(Bindings0, X, BindingOfX)
	->
		( %%% if some [BindingOfY]
			map__search(Bindings0, Y, BindingOfY)
		->
			% both X and Y already have bindings - just
			% unify the modes they are bound to
			mode_unify(BindingOfX, BindingOfY, Bindings0, Bindings)
		;
			term__apply_rec_substitution(BindingOfX,
				Bindings0, SubstBindingOfX),
			% Y is a mode variable which hasn't been bound yet
			( SubstBindingOfX = term_variable(Y) ->
				Bindings = Bindings0
			;
				\+ term__occurs(SubstBindingOfX, Y, Bindings0),
				map__set(Bindings0, Y, SubstBindingOfX,
					Bindings)
			)
		)
	;
		( %%% if some [BindingOfY2]
			map__search(Bindings0, Y, BindingOfY2)
		->
			term__apply_rec_substitution(BindingOfY2,
				Bindings0, SubstBindingOfY2),
			% X is a mode variable which hasn't been bound yet
			( SubstBindingOfY2 = term_variable(X) ->
				Bindings = Bindings0
			;
				\+ term__occurs(SubstBindingOfY2, X, Bindings0),
				map__set(Bindings0, X, SubstBindingOfY2,
					Bindings)
			)
		;
			% both X and Y are unbound mode variables -
			% bind one to the other
			( X = Y ->
				Bindings = Bindings0
			;
				map__set(Bindings0, X, term_variable(Y),
					Bindings)
			)
		)
	).

mode_unify(term_variable(X), term_functor(F, As, C), Bindings0, Bindings) :-
	( %%% if some [BindingOfX]
		map__search(Bindings0, X, BindingOfX)
	->
		mode_unify(BindingOfX, term_functor(F, As, C), Bindings0,
			Bindings)
	;
		\+ term__occurs_list(As, X, Bindings0),
		map__set(Bindings0, X, term_functor(F, As, C), Bindings)
	).

mode_unify(term_functor(F, As, C), term_variable(X), Bindings0, Bindings) :-
	( %%% if some [BindingOfX]
		map__search(Bindings0, X, BindingOfX)
	->
		\+ term__occurs_list(As, X, Bindings0),
		mode_unify(term_functor(F, As, C), BindingOfX, Bindings0,
			Bindings)
	;
		map__set(Bindings0, X, term_functor(F, As, C), Bindings)
	).

mode_unify(term_functor(FX, AsX, _), term_functor(FY, AsY, _), Bindings0,
		Bindings) :-
	length(AsX, ArityX),
	length(AsY, ArityY),
	(
		FX = FY,
		ArityX = ArityY
	->
		mode_unify_list(AsX, AsY, Bindings0, Bindings)
	;
		% XXX check if these modes have been defined to be
		% equivalent using equivalence modes
		fail	% XXX stub only!!!
	).

:- pred mode_unify_list(list(mode), list(mode), substitution, substitution).
:- mode mode_unify_list(input, input, input, output).

mode_unify_list([], []) --> [].
mode_unify_list([X | Xs], [Y | Ys]) -->
	mode_unify(X, Y),
	mode_unify_list(Xs, Ys).

ALL THIS IS COMMENTED OUT! ***********************/

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% XXX - At the moment we don't check for circular modes.
	% (If they aren't used, the compiler will probably not
	% detect the error; if they are, it will probably go into
	% an infinite loop).

:- pred check_circular_modes(module_info, module_info, io__state, io__state).
:- mode check_circular_modes(input, output, di, uo).

check_circular_modes(Module0, Module) -->
	{ Module = Module0 }.

/**** JUNK
	{ moduleinfo_modes(Module0, Modes0 },
	{ map__keys(Modes0, ModeIds) },
	check_circular_modes_2(ModeIds, Modes0, Modes),
	{ moduleinfo_set_modes(Module0, Modes, Module) }.

check_circular_modes_2([], Modes, Modes) --> [].
check_circular_modes_2([ModeId | ModeIds], Modes0, Modes) -->

JUNK ****/
	

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Check for any possible undefined insts/modes.
	% XXX should we add a definition for undefined modes?

:- pred check_undefined_modes(module_info, module_info, io__state, io__state).
:- mode check_undefined_modes(input, output, di, uo).
check_undefined_modes(Module, Module) -->
	{ moduleinfo_insts(Module, InstDefns) },
	{ map__keys(InstDefns, InstIds) },
	find_undef_inst_bodies(InstIds, InstDefns),
	{ moduleinfo_modes(Module, ModeDefns) },
	{ map__keys(ModeDefns, ModeIds) },
	find_undef_mode_bodies(ModeIds, ModeDefns, InstDefns),
	{ moduleinfo_preds(Module, Preds) },
	{ moduleinfo_predids(Module, PredIds) },
	find_undef_pred_modes(PredIds, Preds, ModeDefns, InstDefns).

	% Find any undefined insts/modes used in predicate mode declarations.

:- pred find_undef_pred_modes(list(pred_id), pred_table, mode_table,
				inst_table, io__state, io__state).
:- mode find_undef_pred_modes(input, input, input, input, di, uo).

find_undef_pred_modes([], _Preds, _ModeDefns, _InstDefns) --> [].
find_undef_pred_modes([PredId | PredIds], Preds, ModeDefns, InstDefns) -->
	{ map__search(Preds, PredId, PredDefn) },
	{ predinfo_procedures(PredDefn, Procs) },
	{ map__keys(Procs, ProcIds) },
	find_undef_proc_modes(ProcIds, PredId, Procs, ModeDefns, InstDefns),
	find_undef_pred_modes(PredIds, Preds, ModeDefns, InstDefns).

:- pred find_undef_proc_modes(list(proc_id), pred_id, proc_table, mode_table,
				inst_table, io__state, io__state).
:- mode find_undef_proc_modes(input, input, input, input, input, di, uo).

find_undef_proc_modes([], _PredId, _Procs, _ModeDefns, _InstDefns) --> [].
find_undef_proc_modes([ProcId | ProcIds], PredId, Procs, ModeDefns,
		InstDefns) -->
	{ map__search(Procs, ProcId, ProcDefn) },
	{ procinfo_argmodes(ProcDefn, ArgModes) },
	{ procinfo_context(ProcDefn, Context) },
	find_undef_mode_list(ArgModes, pred(PredId) - Context, ModeDefns, 
		InstDefns),
	find_undef_proc_modes(ProcIds, PredId, Procs, ModeDefns, InstDefns).

%-----------------------------------------------------------------------------%

	% Find any undefined insts/modes used in the bodies of other mode
	% declarations.

:- pred find_undef_mode_bodies(list(mode_id), mode_table, inst_table,
				io__state, io__state).
:- mode find_undef_mode_bodies(input, input, input, di, uo).

find_undef_mode_bodies([], _, _) --> [].
find_undef_mode_bodies([ModeId | ModeIds], ModeDefns, InstDefns) -->
	{ map__search(ModeDefns, ModeId, HLDS_ModeDefn) },
		% XXX abstract hlds__mode_defn/5
	{ HLDS_ModeDefn = hlds__mode_defn(_, _, Mode, _, Context) },
	find_undef_mode_body(Mode, mode(ModeId) - Context, ModeDefns,
			InstDefns),
	find_undef_mode_bodies(ModeIds, ModeDefns, InstDefns).

	% Find any undefined insts/modes used in the given mode definition.

:- pred find_undef_mode_body(hlds__mode_body, mode_error_context,
				mode_table, inst_table, io__state, io__state).
:- mode find_undef_mode_body(input, input, input, input, di, uo).

find_undef_mode_body(eqv_mode(Mode), ErrorContext, ModeDefns, InstDefns) -->
	find_undef_mode(Mode, ErrorContext, ModeDefns, InstDefns).

	% Find any undefined modes in a list of modes.

:- pred find_undef_mode_list(list(mode), mode_error_context,
				mode_table, inst_table, io__state, io__state).
:- mode find_undef_mode_list(input, input, input, input, di, uo).

find_undef_mode_list([], _, _, _) --> [].
find_undef_mode_list([Mode|Modes], ErrorContext, ModeDefns, InstDefns) -->
	find_undef_mode(Mode, ErrorContext, ModeDefns, InstDefns),
	find_undef_mode_list(Modes, ErrorContext, ModeDefns, InstDefns).

	% Find any undefined modes/insts used in a mode.
	% The mode itself may be undefined, and also
	% any inst arguments may also be undefined.
	% (eg. the mode `undef1(undef2, undef3)' should generate 3 errors.)

:- pred find_undef_mode(mode, mode_error_context, mode_table, inst_table,
				io__state, io__state).
:- mode find_undef_mode(input, input, input, input, di, uo).

find_undef_mode((InstA -> InstB), ErrorContext, _ModeDefns, InstDefns) -->
	find_undef_inst(InstA, ErrorContext, InstDefns),
	find_undef_inst(InstB, ErrorContext, InstDefns).
find_undef_mode(user_defined_mode(Name, Args), ErrorContext, ModeDefns,
		InstDefns) -->
	{ length(Args, Arity) },
	{ ModeId = Name - Arity },
	(
		{ \+ map__contains(ModeDefns, ModeId)
		  %%% \+ is_builtin_mode(ModeId) % no builtin modes as yet
		}
	->
		report_undef_mode(ModeId, ErrorContext)
	;
		[]
	),
	find_undef_inst_list(Args, ErrorContext, InstDefns).

%-----------------------------------------------------------------------------%

	% Find any undefined insts used in the bodies of other inst
	% declarations.

:- pred find_undef_inst_bodies(list(inst_id), inst_table, io__state, io__state).
:- mode find_undef_inst_bodies(input, input, di, uo).

find_undef_inst_bodies([], _) --> [].
find_undef_inst_bodies([InstId | InstIds], InstDefns) -->
	{ map__search(InstDefns, InstId, HLDS_InstDefn) },
		% XXX abstract hlds__inst_defn/5
	{ HLDS_InstDefn = hlds__inst_defn(_, _, Inst, _, Context) },
	find_undef_inst_body(Inst, inst(InstId) - Context, InstDefns),
	find_undef_inst_bodies(InstIds, InstDefns).

	% Find any undefined insts used in the given inst definition.

:- pred find_undef_inst_body(hlds__inst_body, mode_error_context, inst_table,
				io__state, io__state).
:- mode find_undef_inst_body(input, input, input, di, uo).

find_undef_inst_body(eqv_inst(Inst), ErrorContext, InstDefns) -->
	find_undef_inst(Inst, ErrorContext, InstDefns).
find_undef_inst_body(abstract_inst, _, _) --> [].

	% Find any undefined insts in a list of insts.

:- pred find_undef_inst_list(list(inst), mode_error_context, inst_table,
				io__state, io__state).
:- mode find_undef_inst_list(input, input, input, di, uo).

find_undef_inst_list([], _ErrorContext, _InstDefns) --> [].
find_undef_inst_list([Inst|Insts], ErrorContext, InstDefns) -->
	find_undef_inst(Inst, ErrorContext, InstDefns),
	find_undef_inst_list(Insts, ErrorContext, InstDefns).

	% Find any undefined insts used in an inst.
	% The inst itself may be undefined, and also
	% any inst arguments may also be undefined.
	% (eg. the inst `undef1(undef2, undef3)' should generate 3 errors.)

:- pred find_undef_inst(inst, mode_error_context, inst_table,
				io__state, io__state).
:- mode find_undef_inst(input, input, input, di, uo).

find_undef_inst(free, _, _) --> [].
find_undef_inst(ground, _, _) --> [].
find_undef_inst(inst_var(_), _, _) --> [].
find_undef_inst(bound(BoundInsts), ErrorContext, InstDefns) -->
	find_undef_bound_insts(BoundInsts, ErrorContext, InstDefns).
find_undef_inst(user_defined_inst(Name, Args), ErrorContext, InstDefns) -->
	{ length(Args, Arity) },
	{ InstId = Name - Arity },
	(
		{ \+ map__contains(InstDefns, InstId) }
		%%% \+ is_builtin_inst(InstId) % no builtin modes as yet
	->
		report_undef_inst(InstId, ErrorContext)
	;
		[]
	),
	find_undef_inst_list(Args, ErrorContext, InstDefns).
find_undef_inst(abstract_inst(Name, Args), ErrorContext, InstDefns) -->
	find_undef_inst(user_defined_inst(Name, Args), ErrorContext, InstDefns).

:- pred find_undef_bound_insts(list(bound_inst), mode_error_context, inst_table,
				io__state, io__state).
:- mode find_undef_bound_insts(input, input, input, di, uo).

find_undef_bound_insts([], _, _) --> [].
find_undef_bound_insts([functor(_Name, Args) | BoundInsts], ErrorContext,
		InstDefns) -->
	find_undef_inst_list(Args, ErrorContext, InstDefns),
	find_undef_bound_insts(BoundInsts, ErrorContext, InstDefns).

%-----------------------------------------------------------------------------%

:- type mode_error_context == pair(mode_error_context_2, term__context).
:- type mode_error_context_2	--->	inst(inst_id)
				;	mode(mode_id)
				;	pred(pred_id).

	% Output an error message about an undefined mode
	% in the specified context.

:- pred report_undef_mode(mode_id, mode_error_context, io__state, io__state).
:- mode report_undef_mode(input, input, di, uo).
report_undef_mode(ModeId, ErrorContext - Context) -->
	prog_out__write_context(Context),
	io__write_string("In "),
	write_mode_error_context(ErrorContext),
	io__write_string(":\n"),
	prog_out__write_context(Context),
	io__write_string("error: undefined mode "),
	write_mode_id(ModeId),
	io__write_string(".\n").

	% Output an error message about an undefined inst
	% in the specified context.

:- pred report_undef_inst(inst_id, mode_error_context, io__state, io__state).
:- mode report_undef_inst(input, input, di, uo).
report_undef_inst(InstId, ErrorContext - Context) -->
	prog_out__write_context(Context),
	io__write_string("In "),
	write_mode_error_context(ErrorContext),
	io__write_string(":\n"),
	prog_out__write_context(Context),
	io__write_string("error: undefined inst "),
	write_inst_id(InstId),
	io__write_string(".\n").

	% Output a description of the context where an undefined mode was
	% used.

:- pred write_mode_error_context(mode_error_context_2, io__state, io__state).
:- mode write_mode_error_context(input, di, uo).

write_mode_error_context(pred(PredId)) -->
	io__write_string("mode declaration for predicate "),
	write_pred_id(PredId).
write_mode_error_context(mode(ModeId)) -->
	io__write_string("definition of mode "),
	write_mode_id(ModeId).
write_mode_error_context(inst(InstId)) -->
	io__write_string("definition of inst "),
	write_inst_id(InstId).

%-----------------------------------------------------------------------------%

	% Predicates to output mode_ids and pred_ids.
	% XXX mode_ids should include the module.

:- pred write_mode_id(mode_id, io__state, io__state).
:- mode write_mode_id(input, di, uo).

write_mode_id(F - N) -->
	prog_out__write_sym_name(F),
	io__write_string("/"),
	io__write_int(N).

	% XXX inst_ids should include the module.

:- pred write_inst_id(inst_id, io__state, io__state).
:- mode write_inst_id(input, di, uo).

write_inst_id(F - N) -->
	prog_out__write_sym_name(F),
	io__write_string("/"),
	io__write_int(N).


:- pred write_pred_id(pred_id, io__state, io__state).
:- mode write_pred_id(input, di, uo).

/******************** ALL THIS IS COMMENTED OUT

write_pred_id(PredId) -->
	% XXX module name
	%%% { predicate_module(PredId, Module) },
	{ predicate_name(PredId, Name) },
	{ predicate_arity(PredId, Arity) },
	%%% io__write_string(Module),
	%%% io__write_string(":"),
	io__write_string(Name),
	io__write_string("/"),
	io__write_int(Arity).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% builtin_mode(Term, Mode)
	%	is true iff 'Term' is a constant of the builtin mode 'Mode'.

:- pred builtin_mode(const, string).
:- mode builtin_mode(input, output).

builtin_mode(term_integer(_), "int").
builtin_mode(term_float(_), "float").
builtin_mode(term_string(_), "string").
builtin_mode(term_atom(String), "character") :-
	string__char_to_string(_, String).

	% is_builtin_mode(ModeId)
	%	is true iff 'ModeId' is the mode_id of a builting mode

:- pred is_builtin_mode(mode_id).
:- mode is_builtin_mode(input).

is_builtin_mode(unqualified("int") - 0).
is_builtin_mode(unqualified("float") - 0).
is_builtin_mode(unqualified("string") - 0).
is_builtin_mode(unqualified("character") - 0).
is_builtin_mode(qualified(_,"int") - 0).
is_builtin_mode(qualified(_,"float") - 0).
is_builtin_mode(qualified(_,"string") - 0).
is_builtin_mode(qualified(_,"character") - 0).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% The modeinfo data structure and access predicates.

:- mode tvarset		==	varset.

:- mode tsubst		==	map(var, mode).

:- mode mode_info 	--->	modeinfo(
					io__state,
					pred_table,
					mode_table,
					cons_table,
					pred_id,
					term__context,
					int,	% XXX this field is never used
					varset,		% variables
					mode_assign_set,
					bool	% did we find any mode errors?
				).

	% The normal inst of a mode_info struct: ground, with
	% the io_state and the struct itself unique, but with
	% multiple references allowed for the other parts.

:- inst uniq_mode_info	=	bound_unique(
					modeinfo(
						ground_unique, ground,
						ground, ground, ground, ground,
						ground, ground, ground, ground
					)
				).

:- mode modeinfo_di :: uniq_mode_info -> dead.
:- mode modeinfo_uo :: free -> uniq_mode_info.

	% Some fiddly modes used when we want to extract
	% the io_state from a modeinfo struct and then put it back again.

:- inst mode_info_no_io	=	bound_unique(
					modeinfo(
						dead, ground,
						ground, ground, ground, ground,
						ground, ground, ground, ground
					)
				).

:- mode modeinfo_get_io_state :: uniq_mode_info -> mode_info_no_io.
:- mode modeinfo_set_io_state :: mode_info_no_io -> dead.

%-----------------------------------------------------------------------------%

:- pred modeinfo_init(io__state, module_info, pred_id, term__context,
			varset, varset, mode_info).
:- mode modeinfo_init(di, input, input, input, input, input, modeinfo_uo).

modeinfo_init(IOState, ModuleInfo, PredId, Context, ModeVarSet, VarSet,
		ModeInfo) :-
	moduleinfo_preds(ModuleInfo, Preds),
	moduleinfo_modes(ModuleInfo, Modes),
	moduleinfo_ctors(ModuleInfo, Ctors),
	map__init(ModeBindings),
	map__init(VarModes),
	ModeInfo = modeinfo(
		IOState, Preds, Modes, Ctors, PredId, Context, 0,
		VarSet, [mode_assign(VarModes, ModeVarSet, ModeBindings)],
		no
	).

%-----------------------------------------------------------------------------%

:- pred modeinfo_get_io_state(mode_info, io__state).
:- mode modeinfo_get_io_state(modeinfo_get_io_state, uo).

modeinfo_get_io_state(modeinfo(IOState,_,_,_,_,_,_,_,_,_), IOState).

%-----------------------------------------------------------------------------%

:- pred modeinfo_set_io_state(mode_info, io__state, mode_info).
:- mode modeinfo_set_io_state(modeinfo_set_io_state, ui, modeinfo_uo).

modeinfo_set_io_state( modeinfo(_,B,C,D,E,F,G,H,I,J), IOState,
			modeinfo(IOState,B,C,D,E,F,G,H,I,J)).

%-----------------------------------------------------------------------------%

:- pred modeinfo_get_preds(mode_info, pred_table).
:- mode modeinfo_get_preds(input, output).

modeinfo_get_preds(modeinfo(_,Preds,_,_,_,_,_,_,_,_), Preds).

%-----------------------------------------------------------------------------%

:- pred modeinfo_get_modes(mode_info, mode_table).
:- mode modeinfo_get_modes(input, output).

modeinfo_get_modes(modeinfo(_,_,Modes,_,_,_,_,_,_,_), Modes).

%-----------------------------------------------------------------------------%

:- pred modeinfo_get_ctors(mode_info, cons_table).
:- mode modeinfo_get_ctors(input, output).

modeinfo_get_ctors(modeinfo(_,_,_,Ctors,_,_,_,_,_,_), Ctors).

%-----------------------------------------------------------------------------%

:- pred modeinfo_get_predid(mode_info, pred_id).
:- mode modeinfo_get_predid(input, output).

modeinfo_get_predid(modeinfo(_,_,_,_,PredId,_,_,_,_,_), PredId).

%-----------------------------------------------------------------------------%

:- pred modeinfo_get_context(mode_info, term__context).
:- mode modeinfo_get_context(input, output).

modeinfo_get_context(modeinfo(_,_,_,_,_,Context,_,_,_,_), Context).

%-----------------------------------------------------------------------------%

:- pred modeinfo_get_varset(mode_info, varset).
:- mode modeinfo_get_varset(input, output).

modeinfo_get_varset(modeinfo(_,_,_,_,_,_,_,VarSet,_,_), VarSet).

%-----------------------------------------------------------------------------%

:- pred modeinfo_get_mode_assign_set(mode_info, mode_assign_set).
:- mode modeinfo_get_mode_assign_set(input, output).

modeinfo_get_mode_assign_set(modeinfo(_,_,_,_,_,_,_,_,ModeAssignSet,_),
			ModeAssignSet).

%-----------------------------------------------------------------------------%

:- pred modeinfo_set_mode_assign_set(mode_info, mode_assign_set, mode_info).
:- mode modeinfo_set_mode_assign_set(modeinfo_di, input, modeinfo_uo).

modeinfo_set_mode_assign_set( modeinfo(A,B,C,D,E,F,G,H,_,J), ModeAssignSet,
			modeinfo(A,B,C,D,E,F,G,H,ModeAssignSet,J)).

%-----------------------------------------------------------------------------%

:- pred modeinfo_set_found_error(mode_info, bool, mode_info).
:- mode modeinfo_set_found_error(modeinfo_di, input, modeinfo_uo).

modeinfo_set_found_error( modeinfo(A,B,C,D,E,F,G,H,I,_), FoundError,
			modeinfo(A,B,C,D,E,F,G,H,I,FoundError)).

%-----------------------------------------------------------------------------%

:- pred modeinfo_get_ctor_list(mode_info, const, int, list(hlds__cons_defn)).
:- mode modeinfo_get_ctor_list(input, input, input, output).

modeinfo_get_ctor_list(ModeInfo, Functor, Arity, ConsDefnList) :-
	modeinfo_get_ctors(ModeInfo, Ctors),
	(
		Functor = term_atom(Name),
		map__search(Ctors, cons(Name, Arity), ConsDefnList0)
	->
		ConsDefnList1 = ConsDefnList0
	;
		ConsDefnList1 = []
	),
	(
		Arity = 0,
		builtin_mode(Functor, BuiltInMode)
	->
		term__context_init("<builtin>", 0, Context),
		ModeId = unqualified(BuiltInMode) - 0,
		ConsDefn = hlds__cons_defn([], ModeId, Context),
		ConsDefnList = [ConsDefn | ConsDefnList1]
	;
		ConsDefnList = ConsDefnList1
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% The mode_assign and mode_assign_set data structures.

:- mode mode_assign_set	==	list(mode_assign).

:- mode mode_assign	--->	mode_assign(
					map(var, mode),		% var modes
					tvarset,		% mode names
					tsubst			% mode bindings
				).

%-----------------------------------------------------------------------------%

	% Access predicates for the mode_assign data structure.
	% Excruciatingly boring code.

:- pred mode_assign_get_var_modes(mode_assign, map(var, mode)).
:- mode mode_assign_get_var_modes(input, output).

mode_assign_get_var_modes(mode_assign(VarModes, _, _), VarModes).

%-----------------------------------------------------------------------------%

:- pred mode_assign_get_modevarset(mode_assign, tvarset).
:- mode mode_assign_get_modevarset(input, output).

mode_assign_get_modevarset(mode_assign(_, ModeVarSet, _), ModeVarSet).

%-----------------------------------------------------------------------------%

:- pred mode_assign_get_mode_bindings(mode_assign, tsubst).
:- mode mode_assign_get_mode_bindings(input, output).

mode_assign_get_mode_bindings(mode_assign(_, _, ModeBindings), ModeBindings).

%-----------------------------------------------------------------------------%

:- pred mode_assign_set_var_modes(mode_assign, map(var, mode), mode_assign).
:- mode mode_assign_set_var_modes(input, input, output).

mode_assign_set_var_modes(mode_assign(_, B, C), VarModes,
			mode_assign(VarModes, B, C)).

%-----------------------------------------------------------------------------%

:- pred mode_assign_set_modevarset(mode_assign, tvarset, mode_assign).
:- mode mode_assign_set_modevarset(input, input, output).

mode_assign_set_modevarset(mode_assign(A, _, C), ModeVarSet,
			mode_assign(A, ModeVarSet, C)).

%-----------------------------------------------------------------------------%

:- pred mode_assign_set_mode_bindings(mode_assign, tsubst, mode_assign).
:- mode mode_assign_set_mode_bindings(input, input, output).

mode_assign_set_mode_bindings(mode_assign(A, B, _), ModeBindings,
			mode_assign(A, B, ModeBindings)).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% The next section contains predicates for writing error diagnostics.

%-----------------------------------------------------------------------------%

:- pred report_error_unif(pred_id, term__context, varset, term, term,
			mode_assign_set, io__state, io__state).
:- mode report_error_unif(input, input, input, input, input, input, di, uo).

report_error_unif(PredId, Context, VarSet, X, Y, ModeAssignSet) -->
	write_context_and_predid(Context, PredId),
	prog_out__write_context(Context),
	io__write_string("mode error in unification of `"),
	io__write_term(VarSet, X),
	io__write_string("' and `"),
	io__write_term(VarSet, Y),
	io__write_string("'.\n"),
	write_mode_assign_set_msg(ModeAssignSet, VarSet).

%-----------------------------------------------------------------------------%

:- pred write_mode_assign_set_msg(mode_assign_set, tvarset,
				io__state, io__state).
:- mode write_mode_assign_set_msg(input, input, di, uo).

write_mode_assign_set_msg(ModeAssignSet, VarSet) -->
	( { ModeAssignSet = [_] } ->
	    io__write_string("\tThe partial mode assignment was:\n")
	;
	    io__write_string("\tThe possible partial mode assignments were:\n")
	),
	write_mode_assign_set(ModeAssignSet, VarSet).

%-----------------------------------------------------------------------------%

:- pred write_mode_assign_set(mode_assign_set, tvarset, io__state, io__state).
:- mode write_mode_assign_set(input, input, di, uo).

write_mode_assign_set([], _) --> [].
write_mode_assign_set([ModeAssign | ModeAssigns], VarSet) -->
	io__write_string("\t"),
	write_mode_assign(ModeAssign, VarSet),
	write_mode_assign_set(ModeAssigns, VarSet).

:- pred write_mode_assign(mode_assign, tvarset, io__state, io__state).
:- mode write_mode_assign(input, input, di, uo).

write_mode_assign(ModeAssign, VarSet) -->
	{
	  mode_assign_get_var_modes(ModeAssign, VarModes),
	  mode_assign_get_mode_bindings(ModeAssign, ModeBindings),
	  mode_assign_get_modevarset(ModeAssign, ModeVarSet),
	  map__keys(VarModes, Vars),
	  Vars = [Var | Vars1]
	},
	( 
		{ map__search(VarModes, Var, Mode) },
		{ varset__lookup_name(VarSet, Var, _) }
	->
		io__write_variable(Var, VarSet),
		io__write_string(" :: "),
		write_mode_b(Mode, ModeVarSet, ModeBindings)
	;
		[]
	),
	write_mode_assign_2(Vars1, VarSet, VarModes, ModeBindings, ModeVarSet),
	io__write_string("\n").

:- pred write_mode_assign_2(list(var), varset, map(var, mode),
			tsubst, tvarset, io__state, io__state).
:- mode write_mode_assign_2(input, input, input, input, input, di, uo).

write_mode_assign_2([], _, _, _, _) --> [].
write_mode_assign_2([Var | Vars], VarSet, VarModes, ModeBindings, ModeVarSet)
		-->
	( 
		{ map__search(VarModes, Var, Mode) },
		{ varset__lookup_name(VarSet, Var, _) }
	->
		io__write_string(", "),
		io__write_variable(Var, VarSet),
		io__write_string(" :: "),
		write_mode_b(Mode, ModeVarSet, ModeBindings)
	;
		[]
	),
	write_mode_assign_2(Vars, VarSet, VarModes, ModeBindings, ModeVarSet).

	% write_mode_b writes out a mode after applying the mode bindings.

:- pred write_mode_b(mode, tvarset, tsubst, io__state, io__state).
:- mode write_mode_b(input, input, input, di, uo).

write_mode_b(Mode, ModeVarSet, ModeBindings) -->
	{ term__apply_rec_substitution(Mode, ModeBindings, Mode2) },
	io__write_term(ModeVarSet, Mode2).

%-----------------------------------------------------------------------------%

:- pred report_error_var(pred_id, term__context, varset, var,
			list(mode_stuff), mode, mode_assign_set,
			io__state, io__state).
:- mode report_error_var(input, input, input, input, input, input, input,
			di, uo).

report_error_var(PredId, Context, VarSet, VarId, ModeStuffList, Mode,
			ModeAssignSet0) -->
	write_context_and_predid(Context, PredId),
	prog_out__write_context(Context),
	io__write_string("mode error: "),
	io__write_string("variable `"),
	io__write_variable(VarId, VarSet),
	( { ModeStuffList = [SingleModeStuff] } ->
		{ SingleModeStuff = mode_stuff(VMode, TVarSet, TBinding) },
		io__write_string("' has mode `"),
		write_mode_b(VMode, TVarSet, TBinding),
		io__write_string("',\n"),
		prog_out__write_context(Context),
		io__write_string("expected mode was `"),
		write_mode_b(Mode, TVarSet, TBinding),
		io__write_string("'.\n")
	;
		io__write_string("' has overloaded mode { `"),
		write_mode_stuff_list(ModeStuffList),
		io__write_string(" },\n"),
		io__write_string("which doesn't match the expected mode.\n")
			% XXX improve error message: should output
			% the expected mode.
	),
	write_mode_assign_set_msg(ModeAssignSet0, VarSet).

:- pred write_mode_stuff_list(list(mode_stuff), io__state, io__state).
:- mode write_mode_stuff_list(input, di, uo).

write_mode_stuff_list([]) --> [].
write_mode_stuff_list([mode_stuff(T, TVarSet, TBinding) | Ts]) -->
	write_mode_b(T, TVarSet, TBinding),
	write_mode_stuff_list_2(Ts).

:- pred write_mode_stuff_list_2(list(mode_stuff), io__state, io__state).
:- mode write_mode_stuff_list_2(input, di, uo).

write_mode_stuff_list_2([]) --> [].
write_mode_stuff_list_2([mode_stuff(T, TVarSet, TBinding) | Ts]) -->
	io__write_string(", "),
	write_mode_b(T, TVarSet, TBinding),
	write_mode_stuff_list_2(Ts).

%-----------------------------------------------------------------------------%

:- pred report_error_undef_pred(pred_id, term__context, pred_id,
			io__state, io__state).
:- mode report_error_undef_pred(input, input, input, di, uo).

report_error_undef_pred(CallingPredId, Context, PredId) -->
	write_context_and_predid(Context, CallingPredId),
	prog_out__write_context(Context),
	io__write_string("error: undefined predicate `"),
	write_pred_id(PredId),
	io__write_string("'.\n").

:- pred report_error_undef_cons(pred_id, term__context, const, int, 
			io__state, io__state).
:- mode report_error_undef_cons(input, input, input, input, di, uo).

report_error_undef_cons(PredId, Context, Functor, Arity) -->
	write_context_and_predid(Context, PredId),
	prog_out__write_context(Context),
	io__write_string("error: undefined symbol `"),
	io__write_constant(Functor),
	io__write_string("/"),
	io__write_int(Arity),
	io__write_string("'.\n").

:- pred report_error_cons(pred_id, term__context, varset, const, list(term),
			mode, mode_assign_set, io__state, io__state).
:- mode report_error_cons(input, input, input, input, input, input, input,
			di, uo).

report_error_cons(PredId, Context, VarSet, Functor, Args, Mode,
			ModeAssignSet) -->
	write_context_and_predid(Context, PredId),
	prog_out__write_context(Context),
	io__write_string("mode error: term `"),
	io__write_term(VarSet, term_functor(Functor, Args, Context)),
	io__write_string("' does not have mode `"),
	write_mode(Mode),	% XXX
	io__write_string("'.\n"),
	write_mode_assign_set_msg(ModeAssignSet, VarSet).

%-----------------------------------------------------------------------------%

:- pred write_context_and_predid(term__context, pred_id, io__state, io__state).
:- mode write_context_and_predid(input, input, di, uo).

write_context_and_predid(Context, PredId) -->
	prog_out__write_context(Context),
	io__write_string("In clause for predicate `"),
	write_pred_id(PredId),
	io__write_string("':\n").

%-----------------------------------------------------------------------------%

:- pred report_ambiguity_error(mode_info, mode_assign, mode_assign,
				io__state, io__state).
:- mode report_ambiguity_error(input, input, input, di, uo).

report_ambiguity_error(ModeInfo, ModeAssign1, ModeAssign2) -->
	{ modeinfo_get_context(ModeInfo, Context) },
	{ modeinfo_get_predid(ModeInfo, PredId) },
	write_context_and_predid(Context, PredId),
	prog_out__write_context(Context),
	io__write_string("error: ambiguous overloading causes mode ambiguity.\n"),
	io__write_string("\tpossible mode assignments include:\n"),
	{ modeinfo_get_varset(ModeInfo, VarSet) },
	{ mode_assign_get_var_modes(ModeAssign1, VarModes1) },
	{ map__keys(VarModes1, Vars1) },
	report_ambiguity_error_2(Vars1, VarSet, ModeAssign1, ModeAssign2).

:- pred report_ambiguity_error_2(list(var), varset, mode_assign, mode_assign,
				io__state, io__state).
:- mode report_ambiguity_error_2(input, input, input, input, di, uo).

report_ambiguity_error_2([], _VarSet, _ModeAssign1, _ModeAssign2) --> [].
report_ambiguity_error_2([V | Vs], VarSet, ModeAssign1, ModeAssign2) -->
	{ mode_assign_get_var_modes(ModeAssign1, VarModes1) },
	{ mode_assign_get_var_modes(ModeAssign2, VarModes2) },
	( {
		map__search(VarModes1, V, T1),
		map__search(VarModes2, V, T2),
		not (T1 = T2)
	} ->
		io__write_string("\t"),
		io__write_variable(V, VarSet),
		io__write_string(" :: "),
		{ mode_assign_get_modevarset(ModeAssign1, TVarSet1) },
		{ mode_assign_get_mode_bindings(ModeAssign1, ModeBindings1) },
		write_mode_b(T1, TVarSet1, ModeBindings1),
		io__write_string(" OR "),
		{ mode_assign_get_modevarset(ModeAssign2, TVarSet2) },
		{ mode_assign_get_mode_bindings(ModeAssign2, ModeBindings2) },
		write_mode_b(T2, TVarSet2, ModeBindings2),
		io__write_string("\n")
	;
		[]
	),
	report_ambiguity_error_2(Vs, VarSet, ModeAssign1, ModeAssign2).

:- pred write_mode(mode, io__state, io__state).
:- mode write_mode(input, di, uo).

write_mode(Mode) -->
	{ varset__init(TVarSet) },	% XXX mode parameter names
	io__write_term(TVarSet, Mode).

ALL THIS IS COMMENTED OUT ********************/

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
