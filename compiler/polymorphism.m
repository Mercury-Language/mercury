%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% file: polymorphism.nl
% main author: fjh

% This module is a pass over the HLDS.
% It does a syntactic transformation to implement polymorphic unifications
% using higher-order predicates.  Every polymorphic predicate is transformed
% so that it takes one additional argument for every type variable in the
% predicate's type declaration.  The argument is a higher-order predicate
% argument which is (the address of) the unification predicate for that type.
% Every polymorphic unification is replaced by a call to one of these
% predicates.
%
% For example, we translate
%
%	:- pred p(T1).
%	:- pred q(T2).
%	:- pred r(T3).
%	p(X) :- q([X]), r(0).
%
% into
%
%	:- pred p(T1, pred(T1, T1)).
%	:- pred q(T2, pred(T2, T2)).
%	:- pred r(T3, pred(T2, T2)).
%	p(X, Unify) :- q([X], list_unify(Unify)), r(0, int_unify).
%
% (except that both the input and output of the transformation are
% actually in super-homogeneous form).

%-----------------------------------------------------------------------------%

:- module polymorphism.
:- interface. 
:- import_module hlds.

:- pred polymorphism__process_module(module_info, module_info).
:- mode polymorphism__process_module(in, out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module int, string, list, set, map, term, varset, std_util, require.
:- import_module prog_io, type_util, mode_util, quantification.
:- import_module code_util, unify_proc.

%-----------------------------------------------------------------------------%

	% This whole section just traverses the module structure.
	% We do two passes, the first to fix up the procedure bodies,
	% (and in fact everything except the pred_info argtypes),
	% the second to fix up the pred_info argtypes.
	% The reason we need two passes is that the first pass looks at
	% the argtypes of the called predicates, and so we need to make
	% sure we don't much them up before we've finished the first pass.

polymorphism__process_module(ModuleInfo0, ModuleInfo) :-
	module_info_preds(ModuleInfo0, Preds),
	map__keys(Preds, PredIds),
	polymorphism__process_preds(PredIds, ModuleInfo0, ModuleInfo1),
	polymorphism__fixup_preds(PredIds, ModuleInfo1, ModuleInfo).

:- pred polymorphism__process_preds(list(pred_id), module_info, module_info).
:- mode polymorphism__process_preds(in, in, out) is det.

polymorphism__process_preds([], ModuleInfo, ModuleInfo).
polymorphism__process_preds([PredId | PredIds], ModuleInfo0, ModuleInfo) :-
	module_info_pred_info(ModuleInfo0, PredId, PredInfo),
	pred_info_name(PredInfo, PredName),
	( PredName = "call" ->
		ModuleInfo1 = ModuleInfo0
	;
		pred_info_procids(PredInfo, ProcIds),
		polymorphism__process_procs(PredId, ProcIds, ModuleInfo0,
			ModuleInfo1)
	),
	polymorphism__process_preds(PredIds, ModuleInfo1, ModuleInfo).

:- pred polymorphism__process_procs(pred_id, list(proc_id),
					module_info, module_info).
:- mode polymorphism__process_procs(in, in, in, out) is det.

polymorphism__process_procs(_PredId, [], ModuleInfo, ModuleInfo).
polymorphism__process_procs(PredId, [ProcId | ProcIds], ModuleInfo0,
		ModuleInfo) :-
	module_info_preds(ModuleInfo0, PredTable0),
	map__lookup(PredTable0, PredId, PredInfo0),
	pred_info_procedures(PredInfo0, ProcTable0),
	map__lookup(ProcTable0, ProcId, ProcInfo0),
	polymorphism__process_proc(ProcInfo0, PredInfo0, ModuleInfo0,
					ProcInfo, PredInfo1),
	map__set(ProcTable0, ProcId, ProcInfo, ProcTable),
	pred_info_set_procedures(PredInfo1, ProcTable, PredInfo),
	map__set(PredTable0, PredId, PredInfo, PredTable),
	module_info_set_preds(ModuleInfo0, PredTable, ModuleInfo1),
	polymorphism__process_procs(PredId, ProcIds, ModuleInfo1, ModuleInfo).

%---------------------------------------------------------------------------%

:- pred polymorphism__fixup_preds(list(pred_id), module_info, module_info).
:- mode polymorphism__fixup_preds(in, in, out) is det.

polymorphism__fixup_preds([], ModuleInfo, ModuleInfo).
polymorphism__fixup_preds([PredId | PredIds], ModuleInfo0, ModuleInfo) :-
	%
	% Recompute the arg types by finding the headvars and the var->type
	% mapping (from the first procedure for the predicate) and
	% applying the type mapping to the extra headvars to get the new
	% arg types.  Note that we are careful to only apply the mapping
	% to the extra head vars, not to the originals, because otherwise
	% we would stuff up the arg types for unification predicates for
	% equivalence types.
	%
	module_info_preds(ModuleInfo0, PredTable0),
	map__lookup(PredTable0, PredId, PredInfo0),
	pred_info_procedures(PredInfo0, ProcTable0),
	pred_info_proc_ids(PredInfo0, ProcIds),
	( ProcIds = [ProcId|_] ->
		map__lookup(ProcTable0, ProcId, ProcInfo),
		proc_info_vartypes(ProcInfo, VarTypes),
		proc_info_headvars(ProcInfo, HeadVars),
		pred_info_arg_types(PredInfo0, TypeVarSet, ArgTypes0),
		list__length(ArgTypes0, NumOldArgs),
		list__length(HeadVars, NumNewArgs),
		NumExtraArgs is NumNewArgs - NumOldArgs,
		(
			list__split_list(NumExtraArgs, HeadVars, ExtraHeadVars,
					_OldHeadVars)
		->
			map__apply_to_list(ExtraHeadVars, VarTypes,
				ExtraArgTypes),
			list__append(ExtraArgTypes, ArgTypes0, ArgTypes)
		;
			error("polymorphism.nl: list__split_list failed")
		),

		pred_info_set_arg_types(PredInfo0, TypeVarSet, ArgTypes,
			PredInfo),
		map__set(PredTable0, PredId, PredInfo, PredTable),
		module_info_set_preds(ModuleInfo0, PredTable, ModuleInfo1),
		polymorphism__fixup_preds(PredIds, ModuleInfo1, ModuleInfo)
	;
		ModuleInfo = ModuleInfo0
	).

%---------------------------------------------------------------------------%

:- type poly_info --->
		poly_info(
			varset,			% from the proc_info
			map(var, type),		% from the proc_info
			tvarset,		% from the proc_info
			map(tvar, var),		% specifies the unify proc var
						% for each of the pred's type
						% parameters
			module_info
		).
	
:- pred polymorphism__process_proc(proc_info, pred_info, module_info,
					proc_info, pred_info).
:- mode polymorphism__process_proc(in, in, in, out, out) is det.

polymorphism__process_proc(ProcInfo0, PredInfo0, ModuleInfo,
					ProcInfo, PredInfo) :-
	% grab the appropriate fields from the pred_info and proc_info
	pred_info_arg_types(PredInfo0, ArgTypeVarSet, ArgTypes),
	pred_info_typevarset(PredInfo0, TypeVarSet0),
	proc_info_headvars(ProcInfo0, HeadVars0),
	proc_info_variables(ProcInfo0, VarSet0),
	proc_info_vartypes(ProcInfo0, VarTypes0),
	proc_info_goal(ProcInfo0, Goal0),
	proc_info_argmodes(ProcInfo0, ArgModes0),
	% insert extra head variables to hold the address of the
	% equality predicate for each polymorphic type in the predicate's
	% type declaration
	term__vars_list(ArgTypes, HeadTypeVars0),
	list__sort(HeadTypeVars0, HeadTypeVars), % remove duplicates
	polymorphism__make_head_vars(HeadTypeVars, ArgTypeVarSet,
					VarSet0, VarTypes0,
				ExtraHeadVars, VarSet1, VarTypes1),
	list__append(ExtraHeadVars, HeadVars0, HeadVars),
	list__length(ExtraHeadVars, NumExtraVars),
	list__duplicate(NumExtraVars, ground -> ground, ExtraModes),
	list__append(ExtraModes, ArgModes0, ArgModes),

	pred_info_name(PredInfo0, PredName),
	( PredName = "call" ->
		VarTypes = VarTypes1,
		VarSet = VarSet1,
		TypeVarSet = TypeVarSet0,
		Goal = Goal0
	;
		% process any polymorphic calls inside the goal
		map__from_corresponding_lists(HeadTypeVars, ExtraHeadVars,
					UnifyProcMap),
		Info0 = poly_info(VarSet1, VarTypes1, TypeVarSet0,
					UnifyProcMap, ModuleInfo),
		polymorphism__process_goal(Goal0, Goal1, Info0, Info),
		Info = poly_info(VarSet, VarTypes, TypeVarSet, _, _),
		% if we introduced any new head variables, we need to
		% fix up the quantification (non-local variables)
		( ExtraHeadVars = [] ->
			Goal = Goal1
		;
			implicitly_quantify_clause_body(HeadVars, Goal1, Goal)
		)
	),

	% set the new values of the fields in proc_info and pred_info
	proc_info_set_headvars(ProcInfo0, HeadVars, ProcInfo1),
	proc_info_set_goal(ProcInfo1, Goal, ProcInfo2),
	proc_info_set_varset(ProcInfo2, VarSet, ProcInfo3),
	proc_info_set_vartypes(ProcInfo3, VarTypes, ProcInfo4),
	proc_info_set_argmodes(ProcInfo4, ArgModes, ProcInfo),
	pred_info_set_typevarset(PredInfo0, TypeVarSet, PredInfo).
	
:- pred polymorphism__process_goal(hlds__goal, hlds__goal,
					poly_info, poly_info).
:- mode polymorphism__process_goal(in, out, in, out) is det.

polymorphism__process_goal(Goal0 - GoalInfo0, Goal) -->
	polymorphism__process_goal_2(Goal0, GoalInfo0, Goal).
	
:- pred polymorphism__process_goal_2(hlds__goal_expr, hlds__goal_info,
					hlds__goal, poly_info, poly_info).
:- mode polymorphism__process_goal_2(in, in, out, in, out) is det.

polymorphism__process_goal_2(
			call(PredId, ProcId, Args0, Builtin, Name, FollowVars),
			GoalInfo, Goal) -->
	{ term__term_list_to_var_list(Args0, ArgVars0) },
	polymorphism__process_call(PredId, ProcId, ArgVars0, 
			ArgVars, ExtraGoals),
	{ term__var_list_to_term_list(ArgVars, Args) },
	{ Call = call(PredId, ProcId, Args, Builtin, Name, FollowVars)
			- GoalInfo },
	{ list__append(ExtraGoals, [Call], GoalList) },
	{ conj_list_to_goal(GoalList, GoalInfo, Goal) }.

polymorphism__process_goal_2(unify(X, Y, Mode, Unification, Context), GoalInfo,
		Goal) -->
	(
		{ Unification = complicated_unify(UniMode, _Category,
			FollowVars) },
		{ X = term__variable(XVar) }
	->
		=(poly_info(_, VarTypes, _, UnifyProcMap, ModuleInfo)),
		{ map__lookup(VarTypes, XVar, Type) },
		( { Type = term__variable(TypeVar) } ->
			% Convert polymorphic unifications into calls to
			% 	call(UnifyProcVar, X, Y)
			% where UnifyProcVar is the higher-order pred var
			% associated with the type of the variables that
			% are being unified.

			{ module_info_get_predicate_table(ModuleInfo,
				PredicateTable) },
			{ predicate_table_search_name_arity(PredicateTable,
				"call", 3, [CallPredId]) ->
				PredId = CallPredId
			;
				error("polymorphism.nl: can't find `call/3'")
			},
			{ ProcId = 0 },
			{ map__lookup(UnifyProcMap, TypeVar, UnifyProcVar) },
			{ SymName = unqualified("call") },
			{ Args = [term__variable(UnifyProcVar), X, Y] },
			{ code_util__is_builtin(ModuleInfo, PredId, ProcId,
				IsBuiltin) },
			{ Goal = call(PredId, ProcId, Args, IsBuiltin,
					SymName, FollowVars) - GoalInfo }

		; { type_to_type_id(Type, TypeId, _) } ->

			% Convert other complicated unifications into
			% calls to unification predicates, and then
			% recursively call polymorphism__process_goal_2
			% to insert extra arguments if necessary.

			{ module_info_get_unify_pred_map(ModuleInfo,
				UnifyPredMap) },
			{ map__lookup(UnifyPredMap, TypeId, PredId) },

			% We handle (in, in) unifications specially - they
			% are always mode zero
			{ UniMode = (XInitial - YInitial -> _Final) },
			(
				{ inst_is_ground(ModuleInfo, XInitial) },
				{ inst_is_ground(ModuleInfo, YInitial) }
			->
				{ ProcId = 0 }
			;
				{ XInitial = not_reached }
			->
				{ ProcId = 0 }
			;
				{ YInitial = not_reached }
			->
				{ ProcId = 0 }
			;
				{ module_info_get_unify_requests(ModuleInfo,
					UnifyRequests) },
				{ unify_proc__lookup_num(UnifyRequests,
					TypeId, UniMode, ProcId) }
			),

			{ SymName = unqualified("=") },
			{ Args = [X, Y] },
			{ Call = call(PredId, ProcId,  Args, not_builtin,
					SymName, FollowVars) },
			polymorphism__process_goal_2(Call, GoalInfo, Goal)
		;
			{ error("polymorphism: type_to_type_id failed") }
		)
	;
		% ordinary unifications are left unchanged
		{ Goal = unify(X, Y, Mode, Unification, Context) - GoalInfo }
	).

	% the rest of the clauses just process goals recursively

polymorphism__process_goal_2(conj(Goals0), GoalInfo, conj(Goals) - GoalInfo) -->
	polymorphism__process_goal_list(Goals0, Goals).
polymorphism__process_goal_2(disj(Goals0), GoalInfo, disj(Goals) - GoalInfo) -->
	polymorphism__process_goal_list(Goals0, Goals).
polymorphism__process_goal_2(not(Goal0), GoalInfo, not(Goal) - GoalInfo) -->
	polymorphism__process_goal(Goal0, Goal).
polymorphism__process_goal_2(switch(_, _, _), _, _) -->
	{ error("polymorphism__process_goal_2: switch unexpected") }.
polymorphism__process_goal_2(some(Vars, Goal0), GoalInfo,
			some(Vars, Goal) - GoalInfo) -->
	polymorphism__process_goal(Goal0, Goal).
polymorphism__process_goal_2(if_then_else(Vars, A0, B0, C0), GoalInfo,
			if_then_else(Vars, A, B, C) - GoalInfo) -->
	polymorphism__process_goal(A0, A),
	polymorphism__process_goal(B0, B),
	polymorphism__process_goal(C0, C).

:- pred polymorphism__process_goal_list(list(hlds__goal), list(hlds__goal),
					poly_info, poly_info).
:- mode polymorphism__process_goal_list(in, out, in, out) is det.

polymorphism__process_goal_list([], []) --> [].
polymorphism__process_goal_list([Goal0 | Goals0], [Goal | Goals]) -->
	polymorphism__process_goal(Goal0, Goal),
	polymorphism__process_goal_list(Goals0, Goals).

:- pred polymorphism__process_call(pred_id, proc_id, list(var),
					list(var), list(hlds__goal),
					poly_info, poly_info).
:- mode polymorphism__process_call(in, in, in, out, out, in, out) is det.

polymorphism__process_call(PredId, _ProcId, ArgVars0, ArgVars, ExtraGoals,
				Info0, Info) :-
	Info0 = poly_info(VarSet0, VarTypes0, TypeVarSet0,
				UnifyProcMap, ModuleInfo),
	module_info_pred_info(ModuleInfo, PredId, PredInfo),
	pred_info_arg_types(PredInfo, PredTypeVarSet, PredArgTypes0),
		% rename apart
		% (this merge might be a performance bottleneck?)
	varset__merge(TypeVarSet0, PredTypeVarSet, PredArgTypes0,
			TypeVarSet, PredArgTypes),
	term__vars_list(PredArgTypes, PredTypeVars0),
	( PredTypeVars0 = [] ->
		% optimize for common case of non-polymorphic call
		ArgVars = ArgVars0,
		ExtraGoals = [],
		Info = Info0
	;
		list__sort(PredTypeVars0, PredTypeVars), % eliminate duplicates
		map__apply_to_list(ArgVars0, VarTypes0, ActualArgTypes),
		map__keys(UnifyProcMap, HeadTypeVars),
		map__init(TypeSubst0),
		( type_unify_list(ActualArgTypes, PredArgTypes, HeadTypeVars,
				TypeSubst0, TypeSubst1) ->
			TypeSubst = TypeSubst1
		;
		error("polymorphism__process_goal_2: type unification failed")
		),
		term__var_list_to_term_list(PredTypeVars, PredTypes0),
		term__apply_rec_substitution_to_list(PredTypes0, TypeSubst,
			PredTypes),
		polymorphism__make_vars(PredTypes, ModuleInfo, UnifyProcMap,
				VarSet0, VarTypes0,
				ExtraVars, ExtraGoals, VarSet, VarTypes),
		list__append(ExtraVars, ArgVars0, ArgVars),
		Info = poly_info(VarSet, VarTypes, TypeVarSet,
				UnifyProcMap, ModuleInfo)
	).

%---------------------------------------------------------------------------%

:- pred polymorphism__make_vars(list(type), module_info,
				map(tvar, var), varset, map(var, type),
				list(var), list(hlds__goal),
				varset, map(var, type)).
:- mode polymorphism__make_vars(in, in, in, in, in, out, out, out, out) is det.

polymorphism__make_vars([], _, _, VarSet, VarTypes, [], [], VarSet, VarTypes).
polymorphism__make_vars([Type|Types], ModuleInfo, UnifyProcMap,
				VarSet0, VarTypes0,
				ExtraVars, ExtraGoals, VarSet, VarTypes) :-
	(
		type_to_type_id(Type, _TypeId, TypeArgs)
	->
		% This occurs for code where a predicate calls a polymorphic
		% predicate with a known value of the type variable.
		% For example, given
		%
		%	:- pred p(T1).
		%	:- pred q(T2).
		%	:- pred r(T3).
		%	p(X) :- q([X]), r(0).
		%
		% we know that in the call to q/1, T2 is bound to `list(T1)',
		% and in the call to r/1, T3 is bound to `int', and so
		% we translate it into
		%	:- pred p(T1, pred(T1, T1)).
		%	:- pred q(T2, pred(T2, T2)).
		%	:- pred r(T3, pred(T3, T3)).
		%	p(X, Unify) :- q([X], list_unify(Unify)), r(int_unify).

		% create a term which holds the unification pred for
		% the type, processing any argument types recursively
		polymorphism__make_vars(TypeArgs, ModuleInfo, UnifyProcMap,
			VarSet0, VarTypes0,
			ArgVars, ExtraGoals0, VarSet1, VarTypes1),
		term__var_list_to_term_list(ArgVars, Args),
		Functor = term__atom("="), 
		term__context_init(Context),
		ValTerm = term__functor(Functor, Args, Context),
		classify_type(Type, ModuleInfo, TypeCategory),
		polymorphism__get_unify_proc(TypeCategory, ModuleInfo, ConsId),
		
		% introduce new variable
		varset__new_var(VarSet1, Var, VarSet2),
		UnifyPredType = term__functor(term__atom("pred"),
					[Type, Type],
					Context),
		map__set(VarTypes1, Var, UnifyPredType, VarTypes2),
		VarTerm = term__variable(Var),

		% create a construction unification which initializes the
		% variable to the unification pred for the type
		UniMode = (free - ground -> ground - ground),
		list__length(ArgVars, NumArgVars),
		list__duplicate(NumArgVars, UniMode, UniModes),
		Unification = construct(Var, ConsId, ArgVars, UniModes),
		UnifyMode = (free -> ground) - (ground -> ground),
		UnifyContext = unify_context(explicit, []),
			% XXX the UnifyContext is wrong
		Unify = unify(VarTerm, ValTerm, UnifyMode, Unification,
					UnifyContext),

		% create a goal_info for the unification

		goal_info_init(GoalInfo0),
		set__list_to_set([Var | ArgVars], NonLocals),
		goal_info_set_nonlocals(GoalInfo0, NonLocals, GoalInfo1),
		map__init(InstMapping0),
		list__duplicate(NumArgVars, ground, ArgInsts),
			% note that we could perhaps be more accurate than
			% `ground', but hopefully it shouldn't make any
			% difference.
		map__set(InstMapping0, Var, bound([functor(Functor, ArgInsts)]),
			InstMapping),
		goal_info_set_instmap_delta(GoalInfo1, reachable(InstMapping),
			GoalInfo),

		ExtraGoal = Unify - GoalInfo,
		list__append(ExtraGoals0, [ExtraGoal], ExtraGoals1)
	;
		Type = term__variable(TypeVar1),
		map__search(UnifyProcMap, TypeVar1, UnifyProcVar)
	->
		% This occurs for code where a predicate calls a polymorphic
		% predicate with a bound but unknown value of the type variable.
		% For example, in
		%
		%	:- pred p(T1).
		%	:- pred q(T2).
		%	p(X) :- q(X).
		%
		% we know that `T2' is bound to `T1', and we translate it into
		%
		%	:- pred p(T1, pred(T1, T1)).
		%	:- pred q(T2, pred(T2, T2)).
		%	p(X, UnifyProc) :- q(X, UnifyProc).
		
		Var = UnifyProcVar,
		ExtraGoals1 = [],
		VarSet2 = VarSet0,
		VarTypes2 = VarTypes0
	;
		% This occurs for code where a predicate calls a polymorphic
		% predicate with an unbound type variable, for example
		%
		%	:- pred p.
		%	:- pred q(list(T)).
		%	p :- q([]).
		%
		% In this case T is unbound, so there cannot be any objects
		% ot type T, and so q/1 cannot possibly use the unification
		% predicate for type T.  We just pass a dummy value (zero).
		%
		%	:- pred p.
		%	:- pred q(T, pred(T, T)).
		%	p :- q([], 0).
		%
		% (This isn't really type-correct, but we're already past
		% the type-checker.  Passing 0 should ensure that we get
		% a core dump if we ever attempt to call the unify pred.)

		% introduce new variable
		varset__new_var(VarSet0, Var, VarSet2),
		UnifyPredType = term__functor(term__atom("pred"),
					[Type, Type],
					Context),
		map__set(VarTypes0, Var, UnifyPredType, VarTypes2),
		VarTerm = term__variable(Var),

		% create a construction unification which initializes the
		% variable to zero
		term__context_init(Context),
		Functor = term__integer(0),
		ZeroTerm = term__functor(Functor, [], Context),
		ConsId = int_const(0),
		Unification = construct(Var, ConsId, [], []),
		UnifyMode = (free -> ground) - (ground -> ground),
		UnifyContext = unify_context(explicit, []),
			% XXX the UnifyContext is wrong
		Unify = unify(VarTerm, ZeroTerm, UnifyMode, Unification,
					UnifyContext),
		goal_info_init(GoalInfo0),
		set__singleton_set(NonLocals, Var),
		goal_info_set_nonlocals(GoalInfo0, NonLocals, GoalInfo1),
		map__init(InstMapping0),
		map__set(InstMapping0, Var, bound([functor(Functor, [])]),
			InstMapping),
		goal_info_set_instmap_delta(GoalInfo1, reachable(InstMapping),
			GoalInfo),
		Goal = Unify - GoalInfo,
		ExtraGoals1 = [Goal]
	),
	ExtraVars = [Var | ExtraVars1],
	list__append(ExtraGoals1, ExtraGoals2, ExtraGoals),
	polymorphism__make_vars(Types, ModuleInfo, UnifyProcMap,
				VarSet2, VarTypes2,
				ExtraVars1, ExtraGoals2, VarSet, VarTypes).

:- pred polymorphism__get_unify_proc(builtin_type, module_info, cons_id).
:- mode polymorphism__get_unify_proc(in, in, out) is det.

polymorphism__get_unify_proc(int_type, ModuleInfo, ConsId) :-
	polymorphism__get_proc("builtin_unify_int", ModuleInfo, ConsId).
polymorphism__get_unify_proc(char_type, ModuleInfo, ConsId) :-
	polymorphism__get_proc("builtin_unify_int", ModuleInfo, ConsId).
polymorphism__get_unify_proc(enum_type, ModuleInfo, ConsId) :-
	polymorphism__get_proc("builtin_unify_int", ModuleInfo, ConsId).
polymorphism__get_unify_proc(str_type, ModuleInfo, ConsId) :-
	polymorphism__get_proc("builtin_unify_string", ModuleInfo, ConsId).
/*
polymorphism__get_unify_proc(float_type, ModuleInfo, ConsId) :-
	polymorphism__get_proc("builtin_unify_float", ModuleInfo, ConsId).
*/
polymorphism__get_unify_proc(pred_type, ModuleInfo, ConsId) :-
	polymorphism__get_proc("builtin_unify_pred", ModuleInfo, ConsId).
polymorphism__get_unify_proc(polymorphic_type, _ModuleInfo, _ConsId) :-
	error("polymorphism__get_unify_proc: polymorphic type").
polymorphism__get_unify_proc(user_type(Type), ModuleInfo, ConsId) :-
	module_info_get_unify_pred_map(ModuleInfo, UnifyPredMap),
	( type_to_type_id(Type, TypeId, _TypeArgs) ->
		map__lookup(UnifyPredMap, TypeId, UnifyPredId),
		ConsId = pred_const(UnifyPredId, 0)
	;
		error("polymorphism__get_unify_proc: type_to_type_id failed")
	).

	% find the unification procedure with the specified name

:- pred polymorphism__get_proc(string, module_info, cons_id).
:- mode polymorphism__get_proc(in, in, out) is det.

polymorphism__get_proc(Name, ModuleInfo, ConsId) :-
	module_info_get_predicate_table(ModuleInfo, PredicateTable),
	(
		predicate_table_search_name_arity(PredicateTable, Name, 2,
			[PredId])
	->
		ConsId = pred_const(PredId, 0)
	;
		error("polymorphism__get_proc: lookup failed")
	).

:- pred polymorphism__make_head_vars(list(tvar), tvarset,
				varset, map(var, type),
				list(var), varset, map(var, type)).
:- mode polymorphism__make_head_vars(in, in, in, in, out, out, out) is det.

polymorphism__make_head_vars([], _, VarSet, VarTypes, [], VarSet, VarTypes).
polymorphism__make_head_vars([TypeVar|TypeVars], TypeVarSet,
				VarSet0, VarTypes0,
				UnifyProcVars, VarSet, VarTypes) :-
	varset__new_var(VarSet0, Var, VarSet1),
	( varset__lookup_name(TypeVarSet, TypeVar, TypeVarName) ->
		string__append("Unify__", TypeVarName, VarName),
		varset__name_var(VarSet1, Var, VarName, VarSet2)
	;
		VarSet2 = VarSet1
	),
	term__context_init(Context),
	Type = term__variable(TypeVar),
	UnifyPredType = term__functor(term__atom("pred"), [Type, Type],
					Context),
	map__set(VarTypes0, Var, UnifyPredType, VarTypes1),
	UnifyProcVars = [Var | UnifyProcVars1],
	polymorphism__make_head_vars(TypeVars, TypeVarSet,
				VarSet2, VarTypes1,
				UnifyProcVars1, VarSet, VarTypes).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
