%-----------------------------------------------------------------------------
%
% Copyright (C) 1997 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------
%
% term_pass1.m
% Main author: crs.
%
% This file contains the first pass of the termination analysis.
% It sets the termination constant for all procedures, and also sets the
% terminates value for some procedures. (eg if the predicate contains higher
% order arguments or calls.)
%
% The first pass processes each SCC in turn, following the dependency
% ordering given by hlds_dependency_info_get_dependency_ordering/2.
% The termination constant is set by first generating a list of
% constraints.  This list of constraints will contain one variable for each
% procedure in the SCC that is currently being processed.  After the
% complete set of constraints have been created, they are then solved using
% lp_solve.  As there is a single variable associated with each procedure,
% each variable is named using the pred_proc_id of the relevant procedure.
% Therefore, in some places in the program (eg term_pass1__equation), the
% pred_proc_id is actually referring to the variable associated with that
% procedure which still needs to be solved.
% 			
%-----------------------------------------------------------------------------
%

:- module term_pass1.

:- interface.

:- import_module io, hlds_module, term_util.

% This is the top level predicate of term_pass1.  It processes all of the
% procedures in the module, and sets the termination constant of each of
% them.  The procedures are processed in the order given by
% hlds_dependency_info_get_dependency_ordering.  The results of the
% analysis are stored in the termination subterm of the proc_info
% structure.
:- pred proc_inequalities(module_info, functor_info, module_info, 
	io__state, io__state).
:- mode proc_inequalities(in, in, out, di, uo) is det.

%------------------------------------------------------------------------------

:- implementation.

:- import_module int, list, bag, require, bool, std_util, char, map, string.
:- import_module hlds_pred, hlds_goal, hlds_data. 
:- import_module term_errors, mode_util, type_util, term.

%------------------------------------------------------------------------------

% This section contains proc_inequalities and its supporting functions.
% proc_inequalities goes through the dependency ordering, applying 
% proc_inequalities to each SCC in turn.  proc_inequalities returns a set of 
% constraints which are then solved using lp_solve.

% term_pass1__equation stores a single constraint.
% The constraint is of the form:
% pred_proc_id - list(pred_proc_id) >= term_util__constant
% Where pred_proc_id represents a variable which relates the total size of
% the input variables to the total size of the output variables. For
% details of what these constraints mean, please refer to one of the papers
% listed at the start of termination.m.
:- type term_pass1__equation
	---> 	eqn(term_util__constant, pred_proc_id, list(pred_proc_id)).

:- type used_args == map(pred_proc_id, list(bool)).

proc_inequalities(Module0, FunctorInfo, Module) -->
	{ module_info_dependency_info(Module0, DependencyInfo) },
	{ hlds_dependency_info_get_dependency_ordering(DependencyInfo, 
		DependOrdering) },
	proc_inequalities_module(Module0, DependOrdering, FunctorInfo, Module).	

:- pred proc_inequalities_module(module_info, dependency_ordering, functor_info,
	module_info, io__state, io__state).
:- mode proc_inequalities_module(in, in, in, out, di, uo) is det.
proc_inequalities_module(Module, [], _FunctorInfo, Module) --> [].
proc_inequalities_module(Module0, [ SCC | SCCs ], FunctorInfo, Module) -->
	{ init_used_args(Module0, SCC, InitUsedArgs) },
	proc_inequalities_scc(Module0, SCC, FunctorInfo, SCC - InitUsedArgs, 
		InitUsedArgs, [], Module2),
	proc_inequalities_module(Module2, SCCs, FunctorInfo, Module).

:- pred proc_inequalities_scc(module_info, list(pred_proc_id), functor_info,
	pair(list(pred_proc_id), used_args), used_args,
	list(term_pass1__equation), module_info, io__state, io__state).
:- mode proc_inequalities_scc(in, in, in, in, in, in, out, di, uo) is det.
proc_inequalities_scc(Module0, [], FunctorInfo, SCC - OldUsedArgs, 
		NewUsedArgs, Offs0, Module) --> 
	% First check used_args.  If a fixed point has been reached, then 
	% continue with the analysis and solve the resulting constraints.  
	% If a fixed point has not been reached, then recurse on the new
	% used args.
	% XXX is it valid to compare maps using equality?
	( { OldUsedArgs = NewUsedArgs } ->
		% A fixed point has been reached.  So process the
		% constraints that have been created.

		% Solve the equations that have been created.
		{ proc_inequalities_scc_remove_useless_offsets(Offs0, Module0, 
			Offs, Res) },

		% XXX what is the correct context to use when referring to
		% a whole SCC?
		( { SCC = [proc(PredId, _)] } ->
			{ module_info_pred_info(Module0, PredId, PredInfo) },
			{ pred_info_context(PredInfo, Context) }
		;
			{ term__context_init(Context) }
		),
		( { Res = error(Error) } ->
			% There is a directly recursive call where the
			% variables grow between the head and recursive
			% call.  Therefore the output is infinitly larger
			% than the input.  
			% e.g. foo(A) :- foo([1|A]).
			{ set_pred_proc_ids_const(SCC, inf(Error), Module0, 
				Module) }
		; { Offs = [] } ->
			% There are no equations in this SCC
			% This has 2 possible causes.  If the predicate has
			% no output arguments, then the relative size of
			% input and output arguments is undefined.  If
			% there are no output arguments, then there will be
			% no equations created.  The other possibility is
			% that the procedure is a builtin predicate,  which
			% has an empty body.
	
			{ NewConst = inf(Context - no_eqns) },
			{ set_pred_proc_ids_const(SCC, NewConst,
				Module0, Module) } 
		;
			solve_equations(Module0, Offs, SCC, NewUsedArgs, 
				Context, Module)
		)
	;
		% The analysis has not reached a fixed point, so recurse.
		proc_inequalities_scc(Module0, SCC, FunctorInfo, 
			SCC - NewUsedArgs, NewUsedArgs, [], Module)
	).

proc_inequalities_scc(Module0, [PPId | PPIds], FunctorInfo, 
		SCC - OldUsedArgsMap, UsedArgsMap0, Offs0, Module) -->
	{ PPId = proc(PredId, ProcId) },
	{ module_info_pred_proc_info(Module0, PredId, ProcId, _PredInfo, 
		ProcInfo) },
	{ proc_info_termination(ProcInfo, Termination) },

	( { Termination = term(not_set, _, _, _) } ->
		{ proc_inequalities_pred(Module0, PredId, ProcId, FunctorInfo,
			Offs1, Res, OldUsedArgsMap, NewUsedArgs) },
		( 
			{ Res = error(Error) },
			% proc_inequalities failed, so set all the
			% termination constants to infinity.
			{ set_pred_proc_ids_const(SCC, inf(Error), 
				Module0, Module2) },

			% If the error detected guarantees that the
			% analysis will not be able to prove termination,
			% then we may as well set the termination property
			% to dont_know, and save time in the second stage.
			( 
				{ ( Error = _Context - horder_call
				;   Error = _Context - horder_args(_, _)
				;   Error = _ - dont_know_proc_called(_, _)
				) }
			->
				do_ppid_check_terminates(SCC, Error, 
					Module2, Module)
			;
				{ Module = Module2 }
			)
		;
			{ Res = ok },
			{ list__append(Offs0, Offs1, Offs) },
			{ map__det_update(UsedArgsMap0, PPId, NewUsedArgs, 
				UsedArgsMap) },
			proc_inequalities_scc(Module0, PPIds, FunctorInfo,
				SCC - OldUsedArgsMap,
				UsedArgsMap, Offs, Module) 
		)
	;
		% The termination constant has already been set - hopefully
		% this is true of all the procedures in this SCC.  Perhaps
		% it would be wise to add a check that this is the case.
		{ Module = Module0 }
	).
	
% This procedure removes offsets where there are no variables in the offset.
% It would be nice if lp_solve would accept constraints of the form 
% (0 >= -1), but it doesnt so they need to be removed manually, which is
% what this procedure does. If this procedure returns an error then the
% constraints are unsatisfiable (0 >= 1).  If the return value is `ok' the
% the constraints that were removed were all satisfiable.
:- pred proc_inequalities_scc_remove_useless_offsets(
	list(term_pass1__equation), module_info, list(term_pass1__equation), 
	term_util__result(term_errors__error)).
:- mode proc_inequalities_scc_remove_useless_offsets(in, in, out, out) is det.
proc_inequalities_scc_remove_useless_offsets([], _Module, [], ok).
proc_inequalities_scc_remove_useless_offsets([ Off0 | Offs0 ], Module, 
		Offs, Res) :-
	( Off0 = eqn(Const, PPId, [ PPId ]) ->
		% in this case there is direct recursion
		( 
			(
				Const = set(Int),
				Int > 0
			;
				Const = inf(_)
			)
		->
			% In this case the recursive call is with larger
			% variables.  Hence the output could be unbounded
			PPId = proc(PredId, _ProcId),
			module_info_pred_info(Module, PredId, PredInfo),
			pred_info_context(PredInfo, Context),
			Res = error(Context - positive_value(PPId, PPId)),
			Offs = Offs0
		;
			proc_inequalities_scc_remove_useless_offsets(Offs0, 
				Module, Offs, Res)
		)
	;
		proc_inequalities_scc_remove_useless_offsets(Offs0, Module, 
			Offs1, Res),
		Offs = [ Off0 | Offs1]
	).

% This predicate takes the results from solve_equations (if it successfully
% solved the constraints), and inserts these results into the
% predicate_table.
:- pred proc_inequalities_set_module(list(pair(pred_proc_id, int)),
	used_args, pred_table, pred_table).
:- mode proc_inequalities_set_module(in, in, in, out) is det.
proc_inequalities_set_module([], _, PredTable, PredTable). 
proc_inequalities_set_module([ Soln | Solns ], UsedArgsMap, 
		PredTable0, PredTable) :-
	Soln = PPId - Int,
	Const = set(Int),
	PPId = proc(PredId, ProcId),

	map__lookup(PredTable0, PredId, PredInfo),
	pred_info_procedures(PredInfo, ProcTable),
	map__lookup(ProcTable, ProcId, ProcInfo),
	map__lookup(UsedArgsMap, PPId, UsedArgs),

	proc_info_termination(ProcInfo, term(Const0, B, _, D)),
	( Const0 = not_set ->
		proc_info_set_termination(ProcInfo, 
			term(Const, B, yes(UsedArgs), D), ProcInfo1)
	;
		% This can only happen if an imported pred was in the same
		% SCC as some local preds, or if somehow some equations
		% were made for an imported pred.  Both of these occurances
		% represent an error in the code.
		error("term_pass1__proc_inequalities_set_module: Error"),
		ProcInfo1 = ProcInfo
	),
	map__set(ProcTable, ProcId, ProcInfo1, ProcTable1),
	pred_info_set_procedures(PredInfo, ProcTable1, PredInfo1),
	map__set(PredTable0, PredId, PredInfo1, PredTable1),
	proc_inequalities_set_module(Solns, UsedArgsMap,PredTable1, PredTable).

% Used to initialise the used_args map.  Initially, all arguments are
% considered to be unused.
:- pred init_used_args(module_info, list(pred_proc_id), used_args).
:- mode init_used_args(in, in, out) is det.
init_used_args(_Module, [], InitMap) :-
	map__init(InitMap).
init_used_args(Module, [PPId | PPIds], Out) :-
	init_used_args(Module, PPIds, Out0),
	PPId = proc(PredId, ProcId),
	module_info_pred_proc_info(Module, PredId, ProcId, _, ProcInfo),
	proc_info_headvars(ProcInfo, HeadVars),
	term_util__make_bool_list(HeadVars, [], BoolList),
	map__det_insert(Out0, PPId, BoolList, Out).

% Used to insert the information in the used_args map into the pred_table.
:- pred set_used_args(pred_table, list(pred_proc_id), used_args, pred_table).
:- mode set_used_args(in, in, in, out) is det.
set_used_args(PredTable, [], _, PredTable).
set_used_args(PredTable0, [PPId | PPIds], UsedArgsMap, PredTable) :-
	PPId = proc(PredId, ProcId),
	map__lookup(PredTable0, PredId, PredInfo0),
	pred_info_procedures(PredInfo0, ProcTable0),
	map__lookup(ProcTable0, ProcId, ProcInfo0),
	proc_info_termination(ProcInfo0, Term0),
	map__lookup(UsedArgsMap, PPId, UsedArgs),
	Term0 = term(Const, Terminates, _UsedArgs, MaybeError),
	Term = term(Const, Terminates, yes(UsedArgs), MaybeError),
	proc_info_set_termination(ProcInfo0, Term, ProcInfo),
	map__det_update(ProcTable0, ProcId, ProcInfo, ProcTable),
	pred_info_set_procedures(PredInfo0, ProcTable, PredInfo),
	map__det_update(PredTable0, PredId, PredInfo, PredTable1),
	set_used_args(PredTable1, PPIds, UsedArgsMap, PredTable).
	

%-----------------------------------------------------------------------------%
% This section contains proc_inequalities_pred and its supporting functions.
% proc_inequalities_pred processes a predicate, and forms a set of
% inequalities relating the size of that predicates inputs to the size of
% the outputs.  proc_inequalities_goal processes a goal, and finds an
% inequality relating the sizeof(input arguments) to the sizeof(output
% arguments).  If it finds a valid inequality, then it returns the offset
% of the inequality (with Res set to ok).  If no inequality can be found,
% then proc_inequalities_pred returns Res = error().

:- type proc_inequalities_equ == list(pair(term_pass1__equation, bag(var))).
:- type proc_inequalities_info == pair(unify_info, used_args).
%	proc_inequalities_info == UnifyInfo - CallInfo

:- pred proc_inequalities_pred(module_info, pred_id, proc_id, functor_info,
	list(term_pass1__equation), term_util__result(term_errors__error), 
	used_args, list(bool)).
:- mode proc_inequalities_pred(in, in, in, in, out, out, in, out) is det.

proc_inequalities_pred(Module, PredId, ProcId, FunctorInfo, Offs, Res, 
		OldUsedArgsMap, NewUsedArgs):-
	module_info_pred_proc_info(Module, PredId, ProcId, PredInfo, ProcInfo),
	proc_info_headvars(ProcInfo, Args),
	proc_info_argmodes(ProcInfo, ArgModes),
	proc_info_vartypes(ProcInfo, VarTypes),
	proc_info_goal(ProcInfo, GoalExpr - GoalInfo),

	partition_call_args(Module, ArgModes, Args, InVars, OutVars),
	bag__from_list(InVars, InVarsBag),
	bag__from_list(OutVars, OutVarsBag),

	PPId = proc(PredId, ProcId),
	InitEqn = eqn(set(0), PPId, []),

	UnifyInfo = VarTypes - FunctorInfo,
	CallInfo = OldUsedArgsMap,
	Info = UnifyInfo - CallInfo,
	proc_inequalities_goal(GoalExpr, GoalInfo, Module, Info, PPId, 
		Res1, [ InitEqn - OutVarsBag ], OffsVars), 
	split_offs_vars(OffsVars, Offs, InVars2Bag),

	( Res1 = ok ->
		( bag__is_subbag(InVars2Bag, InVarsBag) ->
			map__lookup(OldUsedArgsMap, PPId, OldUsedArgs),
			proc_inequalities_used_args(Args, InVars2Bag, 
				OldUsedArgs, NewUsedArgs),
			Res = ok
		;
			pred_info_context(PredInfo, Context),
			% If Res is error(), then The Used Args should not
			% matter
			NewUsedArgs = [],
			Res = error(Context - 
				not_subset(PPId, InVars2Bag, InVarsBag))
		)
	;
		NewUsedArgs = [],
		Res = Res1
	).

:- pred proc_inequalities_used_args(list(var), bag(var), list(bool), list(bool)).
:- mode proc_inequalities_used_args(in, in, in, out) is det.
proc_inequalities_used_args([], _InVarsBag, [], []).
proc_inequalities_used_args([_ | _], _InVarsBag, [], []) :-
	error("term_pass1__proc_inequalities_used_args: Unmatched variables").
proc_inequalities_used_args([], _InVarsBag, [_ | _], []) :-
	error("term_pass1:proc_inequalities_used_args: Unmatched variables").
proc_inequalities_used_args([ Arg | Args ], InVarsBag, 
		[ OldUsedArg | OldUsedArgs ], [ UsedArg | UsedArgs ]):-
	( bag__contains(InVarsBag, Arg) ->
		UsedArg = yes
	;
		UsedArg = OldUsedArg	% This guarantees monotonicity
	),
	proc_inequalities_used_args(Args, InVarsBag, OldUsedArgs, UsedArgs).
 

% proc_inequalities_goal fails (returns Result = error()) if it cannot form an
% inequality.  i.e. if there are higher order calls, higher order
% arguments, or pragma-c-code with relevent output variables. The reason
% for checking for higher order arguments is that this traps calls to
% solutions, which would not otherwise be checked.

:- pred proc_inequalities_goal(hlds_goal_expr, hlds_goal_info, module_info, 
	proc_inequalities_info, pred_proc_id, 
	term_util__result(term_errors__error),
	proc_inequalities_equ, proc_inequalities_equ).
:- mode proc_inequalities_goal(in, in, in, in, in, out, in, out) is det.

proc_inequalities_goal(conj([]), _, _Module, _, _PPId, ok, Offs, Offs).
proc_inequalities_goal(conj([ Goal | Goals ]), GoalInfo, Module, Info, 
		PPId, Res, Offs0, Offs) :-
	( goal_will_fail(GoalInfo) ->
		Res = ok,
		Offs = []
	;
		proc_inequalities_conj(Goal, Goals, Module, Info, PPId, 
			Res, Offs0, Offs)
	).

% This clause fails (returns Res=error()) if:
%	The called predicate contains higher order arguments
%	The terminates value of the called predicate is 'dont_know'
%	The termination constant of the called predicate is infinite
proc_inequalities_goal(call(CallPredId, CallProcId, Args, _IsBuiltin, _, _SymName),
		GoalInfo, Module, Info, PPId, Res, Offs0, Offs) :-
	module_info_pred_proc_info(Module, CallPredId, 
		CallProcId, _CallPredInfo, CallProcInfo),
	proc_info_argmodes(CallProcInfo, CallArgModes),
	partition_call_args(Module, CallArgModes, Args, InVars, OutVars),
	bag__from_list(OutVars, OutVarsBag),

	( goal_will_fail(GoalInfo) ->
		Res = ok,
		Offs = []
	; proc_inequalities_check_intersect(Offs0, OutVarsBag) ->
		% no intersection.
		Offs = Offs0,
		Res = ok
	;
		Info = _UnifyInfo - UsedArgsMap,
		CallPPId = proc(CallPredId, CallProcId),
		PPId = proc(PredId, ProcId),	
		module_info_pred_proc_info(Module, PredId, ProcId, _PredInfo, 
			ProcInfo),
		goal_info_get_context(GoalInfo, Context),
			proc_info_vartypes(ProcInfo, VarType),
		proc_info_termination(CallProcInfo, CallTermination),
		CallTermination = term(CallTermConst, CallTerminates, 
			CallUsedArgs, _),
		( CallTerminates = dont_know ->
			Error = dont_know_proc_called(PPId, CallPPId),
			Res = error(Context - Error),
			Offs = Offs0
		; \+ check_horder_args(Args, VarType) ->
			Res = error(Context - horder_args(PPId, CallPPId)),
			Offs = Offs0
		;
			bag__from_list(InVars, InVarsBag0),
			( 
				CallUsedArgs = yes(UsedVars) 
			->
				remove_unused_args(InVarsBag0, Args, UsedVars,
					InVarsBag1)
			; %else if
				map__search(UsedArgsMap, CallPPId, UsedVars)
			->
				% In this case, it must be a mutually
				% recursive call.  As it is a mutually
				% recursive call, the termination constant
				% should be not_set.
				require(unify(CallTermConst, not_set),
					"Unexpected value in term_pass1__\
					proc_inequalities(call)"),
				remove_unused_args(InVarsBag0, Args, 
					UsedVars, InVarsBag1)
			;
				InVarsBag1 = InVarsBag0
			),
			( 
				CallTermConst = not_set,
				Res = ok,
				Eqn = eqn(set(0), PPId, [CallPPId]),
				proc_inequalities_modify_offs(InVarsBag1,
					OutVarsBag, Eqn, Offs0, Offs)
			; 
				CallTermConst = set(Int),
				Res = ok,
				Eqn = eqn(set(Int), PPId, []),
				proc_inequalities_modify_offs(InVarsBag1,
					OutVarsBag, Eqn, Offs0, Offs)
			;
				CallTermConst = inf(_),
				Offs = Offs0,
				Res = error(Context - 
					inf_termination_const(PPId, CallPPId))
			)
		)
	).
	
proc_inequalities_goal(higher_order_call(_, _, _, _, _, _), 
		GoalInfo, _Module, _, _PPId, Error, Offs, Offs) :-
	goal_info_get_context(GoalInfo, Context),
	Error = error(Context - horder_call).

proc_inequalities_goal(switch(_SwitchVar, _CanFail, Cases, _StoreMap), GoalInfo,
		Module, Info, PPId, Res, Offs0, Offs) :-
	( goal_will_fail(GoalInfo) ->
		Res = ok,
		Offs = []
	;
		proc_inequalities_switch(Cases, GoalInfo, Module, 
			Info, PPId, Res, Offs0, Offs)
	).

proc_inequalities_goal(unify(_Var, _RHS, _UnifyMode, Unification, _UnifyContext),
		GoalInfo, Module, Info, PPId, ok, Offs0, Offs) :-
	Info = UnifyInfo - _CallInfo,
	( goal_will_fail(GoalInfo) ->
		Offs = []
	;
		(
			Unification = construct(OutVar, ConsId, Args0, Modes0),
			UnifyInfo = VarTypes - FunctorInfo,
			% Need to check if OutVar is a higher order type.
			% If so, return Offs unmodified.  XXX i am not sure
			% that this is always valid.  If the horder type is
			% used elsewhere (eg in an argument to a call),
			% then it will be picked up there, and will return
			% Res = error(horder...). If this check is not made
			% then split_unification_vars can quit with an
			% error, as length(Args) is not necessarily equal
			% to length(Modes) for higher order unifications.
			map__lookup(VarTypes, OutVar, Type),
			( type_is_higher_order(Type, _, _) ->
				Offs = Offs0
			;
				( type_to_type_id(Type, TypeIdPrime, _) ->
					TypeId = TypeIdPrime
				;
					error("Variable type in termination analysis")
				),
				functor_norm(FunctorInfo, TypeId, ConsId,
					Module, FunctorNorm, Args0, Args, 
					Modes0, Modes),
				split_unification_vars(Args, Modes, Module,
					InVarsBag , OutVarsBag0),
				bag__insert(OutVarsBag0, OutVar, OutVarsBag),
				Eqn = eqn(set(FunctorNorm), PPId, []),
				proc_inequalities_modify_offs(InVarsBag, 
					OutVarsBag, Eqn, Offs0, Offs)
			)
		;
			Unification = deconstruct(InVar, ConsId, 
				Args0, Modes0, _),
			UnifyInfo = VarTypes - FunctorInfo,
			map__lookup(VarTypes, InVar, Type),
			( type_to_type_id(Type, TypeIdPrime, _) ->
				TypeId = TypeIdPrime
			;
				error("variable type in termination analysis")
			),
			functor_norm(FunctorInfo, TypeId, ConsId, Module,
				FunctorNorm, Args0, Args, Modes0, Modes),
			split_unification_vars(Args, Modes, Module,
				InVarsBag , OutVarsBag),
			bag__insert(InVarsBag, InVar, InVarsBag0),
			Eqn = eqn(set(- FunctorNorm), PPId, []),
			proc_inequalities_modify_offs(InVarsBag0, OutVarsBag,
				Eqn, Offs0, Offs)
		;
			Unification = assign(OutVar, InVar),
			Eqn = eqn(set(0), PPId, []),
			bag__init(InitBag),
			bag__insert(InitBag, InVar, InVarBag),
			bag__insert(InitBag, OutVar, OutVarBag),
			proc_inequalities_modify_offs(InVarBag, OutVarBag, Eqn, 
				Offs0, Offs)
		;
			Unification = simple_test(_InVar1, _InVar2),
			Offs = Offs0
		;
			Unification = complicated_unify(_, _),
			error("Unexpected complicated_unify in termination.m")
		)
	).

% No variables are bound in an empty disjunction (fail), so it does not
% make sense to define an equation relating input variables to output
% variables.
proc_inequalities_goal(disj([], _), _, _Module, _, _PPId, ok, _Offs, []).
proc_inequalities_goal(disj([ Goal | Goals ], _StoreMap), 
		GoalInfo, Module, Info, PPId, Res, Offs0, Offs) :-
	( goal_will_fail(GoalInfo) ->
		Res = ok,
		Offs = []
	;
		proc_inequalities_disj(Goal, Goals, GoalInfo, Module, Info, 
			PPId, Res, Offs0, Offs)
	).

% As we are trying to form a relationship between variables sizes, and no
% variables can be bound in a not, we dont need to evaluate inside the not
proc_inequalities_goal(not(_), GoalInfo, _Module, _, _PPId, ok, Offs0, Offs) :-
	( goal_will_fail(GoalInfo) ->
		Offs = []
	;
		Offs = Offs0
	).

proc_inequalities_goal(some(_Vars, GoalExpr - GoalInfo), SomeGoalInfo, 
		Module, Info, PPId, Res, Offs0, Offs) :-
	( goal_will_fail(SomeGoalInfo) ->
		Res = ok,
		Offs = []
	;
		proc_inequalities_goal(GoalExpr, GoalInfo, Module, Info, 
			PPId, Res, Offs0, Offs)
	).
% an if-then-else is processed as:
% (
% 	if_goal,
% 	then_goal
% ;
% 	% the reason that there is no need for a not(if_goal) here is that
% 	% no variables are bound in a not.  If the if_goal is
% 	% non-terminating, then this will be discovered in when the if-then
% 	% goal is processed
% 	else_goal
% )
proc_inequalities_goal(if_then_else(_Vars, IfGoal, ThenGoal, ElseGoal, _),
		GoalInfo, Module, Info, PPId, Res, Offs0, Offs) :-	
	( goal_will_fail(GoalInfo) ->
		Res = ok,
		Offs = []
	;
		NewThenGoal = conj([IfGoal, ThenGoal]) - GoalInfo,
		proc_inequalities_disj(NewThenGoal, [ElseGoal], GoalInfo, 
			Module, Info, PPId, Res, Offs0, Offs)
	).

proc_inequalities_goal(
		pragma_c_code(_, _, CallPredId, CallProcId, Args, _, _, _), 
		GoalInfo, Module, _Info, _PPId, Res, Offs0, Offs) :-
	(goal_will_fail(GoalInfo) ->
		Res = ok,
		Offs = []
	;
		module_info_pred_proc_info(Module, CallPredId, CallProcId, _,
			CallProcInfo),
		proc_info_argmodes(CallProcInfo, CallArgModes),
		partition_call_args(Module, CallArgModes, Args, 
			_InVars, OutVars),
		bag__from_list(OutVars, OutVarBag),
	
		( proc_inequalities_check_intersect(Offs, OutVarBag) ->
			% no intersection
			% c_code has no important output vars, so we need
			% no error
			Res = ok
		;
			goal_info_get_context(GoalInfo, Context),
			Res = error(Context - pragma_c_code)
		),
		Offs = Offs0
	).

%-----------------------------------------------------------------------------%

:- pred proc_inequalities_conj(hlds_goal, list(hlds_goal), 
	module_info, proc_inequalities_info, pred_proc_id,
	term_util__result(term_errors__error), proc_inequalities_equ,
	proc_inequalities_equ).
:- mode proc_inequalities_conj(in, in, in, in, in, out, in, out) is det.

proc_inequalities_conj(Goal, [], Module, Info, PPId, Res,
		Offs0, Offs) :-
	Goal = GoalExpr - GoalInfo,
	proc_inequalities_goal(GoalExpr, GoalInfo, Module, Info, 
		PPId, Res, Offs0, Offs).

proc_inequalities_conj(Goal, [ Goal2 | Goals ], Module, Info, PPId, 
		Res, Offs0, Offs) :-
	proc_inequalities_conj(Goal2, Goals, Module, Info, 
		PPId, Res1, Offs0, Offs1),
	( Res1 = ok ->
		Goal = GoalExpr - GoalInfo,
		proc_inequalities_goal(GoalExpr, GoalInfo, Module, Info, 
			PPId, Res, Offs1, Offs)
	;
		Res = Res1,
		Offs = Offs1
	). 

:- pred proc_inequalities_switch(list(case), hlds_goal_info, 
	module_info, proc_inequalities_info, pred_proc_id,
	term_util__result(term_errors__error), proc_inequalities_equ,
	proc_inequalities_equ).
:- mode proc_inequalities_switch(in, in, in, in, in, out, in, out) is det.

proc_inequalities_switch([], _, _Module, _, _PPId, ok, Offs, Offs) :-
	error("Unexpected empty switch in term_pass1:proc_inequalities_switch").

proc_inequalities_switch([ Case | Cases ], SwitchGoalInfo, 
	Module, Info, PPId, Res, Offs0, Offs) :-

	Case = case(_ConsId, Goal),
	Goal = GoalExpr - GoalInfo,	
	proc_inequalities_goal(GoalExpr, GoalInfo, Module, Info, 
		PPId, Res1, Offs0, Offs1),

	( Res1 = error(_) ->
		Res = Res1,
		Offs = Offs1
	; Cases = [] ->
		Res = Res1,
		Offs = Offs1
	;
		proc_inequalities_switch(Cases, SwitchGoalInfo, 
			Module, Info, PPId, Res2, Offs0, Offs2),

		( Res2 = error(_) ->
			Res = Res2,
			Offs = Offs2
		;
			list__append(Offs1, Offs2, Offs),
			Res = ok
		)
	).
	
:- pred proc_inequalities_disj(hlds_goal, list(hlds_goal), hlds_goal_info, 
	module_info, proc_inequalities_info, pred_proc_id,
	term_util__result(term_errors__error), proc_inequalities_equ,
	proc_inequalities_equ).
:- mode proc_inequalities_disj(in, in, in, in, in, in, out, in, out) is det.
proc_inequalities_disj(Goal, [], _, Module, Info, PPId, 
		Res, Offs0, Offs) :-
	Goal = GoalExpr - GoalInfo,
	proc_inequalities_goal(GoalExpr, GoalInfo, Module, Info, 
		PPId, Res, Offs0, Offs).

proc_inequalities_disj(Goal, [Goal2 | Goals], DisjGoalInfo, Module, 
		Info, PPId, Res, Offs0, Offs) :-
	proc_inequalities_disj(Goal2, Goals, DisjGoalInfo, Module, 
		Info, PPId, Res1, Offs0, Offs1),
	Goal = GoalExpr - GoalInfo,
	proc_inequalities_goal(GoalExpr, GoalInfo, Module, Info, 
		PPId, Res2, Offs0, Offs2),
	( Res1 = error(_) ->
		Res = Res1,
		Offs = Offs1
	; Res2 = error(_) ->
		Res = Res2,
		Offs = Offs2
	;
		list__append(Offs1, Offs2, Offs),
		Res = ok
	).

		
%-----------------------------------------------------------------------------%
% support functions for proc_inequalities

:- pred proc_inequalities_modify_offs(bag(var), bag(var), term_pass1__equation,
	proc_inequalities_equ, proc_inequalities_equ).
:- mode proc_inequalities_modify_offs(in, in, in, in, out) is det.
proc_inequalities_modify_offs(_, _, _, [], []).
proc_inequalities_modify_offs(InVars, OutVars, ModEqn, [Out0 | Out0s], 
		[Out | Outs ]) :-
	Out0 = Equation0 - Vars0,
	( bag__intersect(OutVars, Vars0) ->
		% There is an intersection
		bag__subtract(Vars0, OutVars, Vars1),
		bag__union(Vars1, InVars, Vars),
		eqn_add(ModEqn, Equation0, Equation),
		Out = Equation - Vars
	;
		Out = Out0
	),
	proc_inequalities_modify_offs(InVars, OutVars, ModEqn, Out0s, Outs).

% Adds two equations together.
:- pred eqn_add(term_pass1__equation, term_pass1__equation,
	term_pass1__equation).
:- mode eqn_add(in, in, out) is det.
eqn_add(eqn(Const1, PPId1, PPList1), eqn(Const2, PPId2, PPList2), Out) :-
	( PPId1 = PPId2 ->
		( ( Const1 = not_set ; Const2 = not_set ) ->
			OutConst = not_set
		; ( Const1 = inf(Error) ) ->
			OutConst = inf(Error)
		; ( Const2 = inf(Error) ) ->
			OutConst = inf(Error)
		; ( Const1 = set(Num1), Const2 = set(Num2)) ->
			OutNum = Num1 + Num2,
			OutConst = set(OutNum)
		;
			% This really cant happen, as Const1 and Const2 can 
			% only be (not_set ; inf ; set(int))
			% as the disjunction is not flattened, mercury cant
			% work it out, and would call the pred semidet
			error("term_pass1__eqn_add\n")
		),
		list__append(PPList1, PPList2, OutPPList),
		Out = eqn(OutConst, PPId1, OutPPList)
	;
		% It makes no sense to add equations with different PPId
		% values.  Its like trying to add apples to oranges.
		error("term_pass1:eqn_add was called with illegal arguments\n")
	).

:- pred proc_inequalities_check_intersect(proc_inequalities_equ, bag(var)).
:- mode proc_inequalities_check_intersect(in, in) is semidet.
% Succeeds if there is no intersection.
proc_inequalities_check_intersect([], _).
proc_inequalities_check_intersect([ Out | Outs ], OutVarBag) :-
	Out = _Equation - OutBag,
	\+ bag__intersect(OutBag, OutVarBag),
	proc_inequalities_check_intersect(Outs, OutVarBag).


% This predicate takes the list of pair(equation, bag(var)), and splits it
% up into a list(equation) and a bag(var).  The output bag(var) is given by
% taking the least-upper-bound of each of the bags in the initial list.
% The least upper bound is the smallest multiset such that each bag in the
% initial list is a subset of the least upper bound.
:- pred split_offs_vars(proc_inequalities_equ, list(term_pass1__equation),
	bag(var)).
:- mode split_offs_vars(in, out, out) is det.
split_offs_vars([], [], InitBag) :-
	bag__init(InitBag).
split_offs_vars([ X | Xs ], [ Off | Offs ], VarBag) :-
	split_offs_vars(Xs, Offs, Bag0),
	X = Off - SubBag,
	% now need to produce the least upper bound of Bag0 and SubBag
	% eg. need least upper bound of {1, 1, 2, 2, 3 }, {1, 1, 2, 3, 3, 3}
	% bag__subtract({1, 1, 2, 2, 3 }, {1, 1, 2, 3, 3, 3}, {2}),
	% bag__union({1, 1, 2, 3, 3, 3}, {2}, {1, 1, 2, 2, 3, 3, 3}).
	% and {1, 1, 2, 2, 3, 3, 3} is the correct least upper bound
	bag__subtract(Bag0, SubBag, Bag1),
	bag__union(Bag1, SubBag, VarBag).

% This predicate takes a goal_info, and succeeds iff that goal will fail.
:- pred goal_will_fail(hlds_goal_info).
:- mode goal_will_fail(in) is semidet.
goal_will_fail(GoalInfo) :-
	goal_info_get_determinism(GoalInfo, Detism),
	determinism_components(Detism, _, at_most_zero).


%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
% Solve the list of constraints 

% output is of the form required by lp_solve.
% which is given the input = [eqn(Const, PPid, [PPidList])]
% max: .......
% c1: PPid - (PPidList) > Const;
% c2: PPid - (PPidList) > Const;
% where PPid (proc(PredId, ProcId)) is printed as ' aPredId_ProcId - b '
% The choice of the letter `a' is arbitrary, and is chosen as lp_solve does
% not allow variables to start with digits.
% The variable `b' is used as lp_solve will only solve for positive values
% of variables.  replacing each variable occurance with ` a#_# - b ', this
% avoids the problem of only allowing positive variables as  a#_# - b can
% be negative even when a#_# and b are both positive.
%

:- pred solve_equations(module_info, list(term_pass1__equation), 
	list(pred_proc_id), used_args, term__context, module_info, 
	io__state, io__state).
:- mode solve_equations(in, in, in, in, in, out, di, uo) is det.

solve_equations(Module0, Equations, PPIds, UsedArgs, Context, Module) -->
	io__tmpnam(ConstraintFile),
	solve_equations_create_constraint_file(Equations, PPIds, 
		ConstraintFile, ConstraintSucc),
	( { ConstraintSucc = yes } ->
		solve_equations_solve_constraint_file(ConstraintFile, Soln)
	;
		% Failed to create the constraint file.
		{ Soln = fatal_error }
	),
	% The equations have been solved, now put the
	% results into the module_info.
	( { Soln = solved(SolutionList) } ->
		% The solver successfully solved the constraints.
		{ module_info_preds(Module0, PredTable0) },
		{ proc_inequalities_set_module(SolutionList, UsedArgs, 
			PredTable0, PredTable) },
		{ module_info_set_preds(Module0, PredTable, 
			Module) }
	; { Soln = optimal } ->
		% All 'optimal' results should have been
		% changed into a list of solutions in
		% solve_equations.  
		{ error("term_pass1__solve_equations: Unexpected Value\n")}
	;
		% The constraint solver failed to solve the
		% set of constraints - set the termination
		% constant to infinity.
		{ Error = Context - lpsolve_failed(Soln) },
		{ set_pred_proc_ids_const(PPIds, 
			inf(Error), Module0, Module) }
	).

:- pred solve_equations_solve_constraint_file(string, eqn_soln, 
	io__state, io__state).
:- mode solve_equations_solve_constraint_file(in, out, di, uo) is det.
solve_equations_solve_constraint_file(ConstraintFile, Soln) -->
	io__tmpnam(OutputFile),
	% run lp_solve
	solve_equations_run_lp_solve(ConstraintFile, OutputFile, MaybeResult),
	( 
		{ MaybeResult = yes(Result) },
		( { Result = optimal } ->
			solve_equations_process_output_file(OutputFile, Soln0)
		;
			% lp_solve failed to solve the constraints.
			% This could be for a number of reasons,
			% and the value of Result will represent
			% the reason.
			{ Soln0 = Result }
		)
	;
		{ MaybeResult = no },
		{ Soln0 = fatal_error }
	),

	% Remove and close all temporary files.
	solve_equations_remove_file(ConstraintFile, Success0),
	solve_equations_remove_file(OutputFile, Success1),
	{ bool__or(Success0, Success1, Success) },
	{ ( 
		Success = yes,
		Soln = Soln0
	;
		Success = no,
		Soln = fatal_error
	) }.

% This runs lp_solve, and outputs an error message and returns `no' 
% if the system call fails.  On success, it returns the return value of
% lp_solve after the return value has been changed from an integer into a
% lpsolve_ret_val type.
:- pred solve_equations_run_lp_solve(string, string, maybe(eqn_soln),
	io__state, io__state).
:- mode solve_equations_run_lp_solve(in, in, out, di, uo) is det.
solve_equations_run_lp_solve(ConstraintFile, OutputFile, MaybeResult) -->
	io__get_environment_var("MERCURY_LPSOLVE", Lpsolve0),
	( { Lpsolve0 = yes(Lpsolve1) } ->
		{ Lpsolve = Lpsolve1 }
	;
		{ Lpsolve = "lp_solve" }
	),
	{ string__append_list([ Lpsolve, " <",
		ConstraintFile, " > ",
		OutputFile], Command) },
	io__call_system(Command, Res0),
	% Test the return value
	( 
		{ Res0  = ok(RetVal) },
		{ lpsolve_ret_val(RetVal, Result) },
		{ MaybeResult = yes(Result) }
	;
		{ Res0 = error(Error0) },
		io__progname_base("term_pass1.m", ProgName),
		{ io__error_message(Error0, Msg0) },
		io__write_strings([
			ProgName,
			": Error with system call `",
			Command,
			"': ",
			Msg0,
			"\n" ]),
		io__set_exit_status(1),
		{ MaybeResult = no }
	).

:- pred solve_equations_remove_file(string, bool, io__state, io__state).
:- mode solve_equations_remove_file(in, out, di, uo) is det.
solve_equations_remove_file(File, Success) -->
	io__remove_file(File, Res1),
	( { Res1 = error(Error1) } ->
		io__progname_base("term_pass1.m", ProgName),
		{ io__error_message(Error1, Msg1) },
		io__write_strings([
			ProgName,
			": Error deleting temporary file `",
			File,
			"' : ",
			Msg1,
			"\n" ]),
		io__set_exit_status(1),
		{ Success = no }
	;
		{ Success = yes }
	).

% This predicate creates the constraint file in a format suitable for
% lp_solve.  This really shouldn't be called with Equations=[] as lp_solve
% exits with an error if it is called without any constraints.
:- pred solve_equations_create_constraint_file(list(term_pass1__equation),
	list(pred_proc_id), string, bool, io__state, io__state).
:- mode solve_equations_create_constraint_file(in, in, in, out, di, uo) is det.

solve_equations_create_constraint_file(Equations, PPIds, 
		ConstraintFile, Success) -->
	io__open_output(ConstraintFile, Res),
	( 	{ Res = error(Error) },
		% error message and quit
		io__progname_base("termination.m", ProgName),
		{ io__error_message(Error, Msg) },
	
		io__write_string(ProgName),
		io__write_string(": cannot open temporary file `"),
		io__write_string(ConstraintFile),
		io__write_string("' for output: "),
		io__write_string(Msg),
		io__write_string("\n"),
		io__set_exit_status(1),
		{ Success = no }
	;
		{ Res = ok(Stream) },
		( { Equations = [] } ->
			{ Success = no }
		;
			io__set_output_stream(Stream, OldStream),
			% create the constraint file
			output_equations(Equations, PPIds, Success),
	
			io__set_output_stream(OldStream, _),
			io__close_output(Stream)
		)
	).

% Prepare to parse the output from lp_solve.
:- pred solve_equations_process_output_file(string, eqn_soln, 
	io__state, io__state).
:- mode solve_equations_process_output_file(in, out, di, uo) is det.
solve_equations_process_output_file(OutputFile, Soln) -->
	io__open_input(OutputFile, OutputRes),
	( 	{ OutputRes = error(Error) },
		io__progname_base("term_pass1.m", ProgName),
		{ io__error_message(Error, Msg) },

		io__write_string(ProgName),
		io__write_string(": cannot open temporary file `"),
		io__write_string(OutputFile),
		io__write_string("' for input: "),
		io__write_string(Msg),
		io__write_string("\n"),
		io__set_exit_status(1),
		{ Soln = fatal_error }
	;
		{ OutputRes = ok(Stream) },
		io__set_input_stream(Stream, OldStream),
		% need to interpret it now
		solve_equations_parse_output_file(Soln),
		io__set_input_stream(OldStream, _),
		io__close_input(Stream)
	).

% Parse the output from lp_solve.
:- pred solve_equations_parse_output_file(eqn_soln, io__state, io__state).
:- mode solve_equations_parse_output_file(out, di, uo) is det.
solve_equations_parse_output_file(Soln) -->
	io__read_line(Res1),
	( { Res1 = ok(_) } ->
		solve_equations_parse_output_file_2(Soln0, MaybeBVal),
		( { Soln0 = solved(Result0) } ->
			( { MaybeBVal = yes(BVal) } ->
				{ solve_equations_output_file_2(Result0, BVal,
					Result) },
				{ Soln = solved(Result) }
			;
				{ Soln = parse_error }
			)
		;
			io__write_string(
				"parse_output_file returned not solved\n"),
			{ Soln = parse_error }
		)
	;
		{ Soln = parse_error }
	).

:- pred solve_equations_output_file_2(list(pair(pred_proc_id, int)), int,
	list(pair(pred_proc_id, int))).
:- mode solve_equations_output_file_2(in, in, out) is det.
solve_equations_output_file_2([], _, []). 
solve_equations_output_file_2([X | Xs], BVal, [Y | Ys]) :-
	X = PPId - XVal, 	% pair deconstruction
	YVal is XVal - BVal,	% subtraction
	Y = PPId - YVal,	% pair construction
	solve_equations_output_file_2(Xs, BVal, Ys).
		
:- pred solve_equations_parse_output_file_2(eqn_soln, maybe(int), io__state,
	io__state).
:- mode solve_equations_parse_output_file_2(out, out, di, uo) is det.
solve_equations_parse_output_file_2(Soln, BVal) -->
	io__read_line(Res1),
	( { Res1 = ok([ X | Xs ]) } ->
		( 
			{ X = 'a' },
			{ char_list_remove_int(Xs, PredInt, Xs1) },
			{ Xs1 = [ '_' | Xs2 ] },
			{ char_list_remove_int(Xs2, ProcInt, Xs3) },
			{ char_list_remove_whitespace(Xs3, Xs4) },
			{ char_list_remove_int(Xs4, Value, _Xs5) }
		->
			% Have found a solution.
			{ pred_id_to_int(PredId, PredInt) },
			{ proc_id_to_int(ProcId, ProcInt) },
			solve_equations_parse_output_file_2(Soln0, BVal),
			( { Soln0 = solved(SolnList) } ->
				{ NewSoln = proc(PredId, ProcId) - Value },
				{ Soln = solved([NewSoln | SolnList ]) }
			;
				{ Soln = Soln0 }
			)
		; % else if
			{ X = 'b' },
			{ char_list_remove_whitespace(Xs, Xs1) },
			{ char_list_remove_int(Xs1, Value, _Xs2) }
		->
			solve_equations_parse_output_file_2(Soln, _Bval),
			{ BVal = yes(Value) }
		;
			{ Soln = parse_error },
			{ BVal = no }
		)
	; { Res1 = eof } ->
		{ Soln = solved([]) },
		{ BVal = no }
	;
		{ Soln = parse_error },
		{ BVal = no }
	).

:- pred char_list_remove_int(list(char), int, list(char)).
:- mode char_list_remove_int(in, out, out) is semidet.
char_list_remove_int([X | Xs], Int, ListOut) :-
	char__is_digit(X),
	char__to_int(X, Int0),
	char__to_int('0', IntValueofZero),
	Int1 is Int0 - IntValueofZero,   
	char_list_remove_int_2(Xs, Int1, Int, ListOut).

:- pred char_list_remove_int_2(list(char), int, int, list(char)).
:- mode char_list_remove_int_2(in, in, out, out) is semidet.

char_list_remove_int_2([], Int, Int, []).
char_list_remove_int_2([X | Xs], Int0, Int, ListOut) :-
	( char__is_digit(X) ->
		char__to_int('0', IntValueofZero),
		char__to_int(X, Int1),
		Int2 is Int0 * 10 + Int1 - IntValueofZero,
		char_list_remove_int_2(Xs, Int2, Int, ListOut)
	;
		ListOut = [ X | Xs ],
		Int = Int0
	).
		
:- pred char_list_remove_whitespace(list(char), list(char)).
:- mode char_list_remove_whitespace(in, out) is det.
char_list_remove_whitespace([], []).
char_list_remove_whitespace([ X | Xs ], Out) :-
	( char__is_whitespace(X) ->
		char_list_remove_whitespace(Xs, Out)
	;
		Out = [ X | Xs ]
	).

:- pred lpsolve_ret_val(int, eqn_soln).
:- mode lpsolve_ret_val(in, out) is det.
lpsolve_ret_val(Int, Result) :-
	( Int = 0	->	Result = optimal
	; Int = 2	->	Result = infeasible
	; Int = 3	->	Result = unbounded
	; 			Result = failure
	).

%-----------------------------------------------------------------------------%
% These predicates are used to output a list of equations in a form
% suitable for lp_solve.  
:- pred output_equations(list(term_pass1__equation), list(pred_proc_id),
	bool, io__state , io__state).
:- mode output_equations(in, in, out, di, uo) is det.

output_equations(Xs, PPIds, Success) --> 
	% output: 'max: # b - PPIds'
	io__write_string("max: "),
	{ list__length(PPIds, Length) },
	io__write_int(Length),
	io__write_string(" b "),
	output_eqn_2(PPIds),
	io__write_string(";\n"),

	output_equations_2(Xs, 1, Success).

:- pred output_equations_2(list(term_pass1__equation), int,
	bool, io__state , io__state).
:- mode output_equations_2(in, in, out, di, uo) is det.

output_equations_2([], _Count, yes) --> [].
output_equations_2([ X | Xs ], Count, Succ) --> 
	output_eqn(X, Count, Succ0),
	( { Succ0 = yes } ->
		{ Count1 is Count + 1 },
		output_equations_2(Xs, Count1, Succ)
	;
		{ Succ = Succ0 }
	).

:- pred output_eqn(term_pass1__equation, int, bool, io__state,
	io__state).
:- mode output_eqn(in, in, out, di, uo) is det.

% each constraint is of the form:
% c#: # b + PPId - (PPIdList) > Const;
% each PPId is printed as `aPredId_ProcId'
% As each PPId can be negative, and lp_solve allows only positive
% variables, we introduce a dummy variable 'b' such that 
% Actual PPId value = returned PPId value - b, where the returned value is 
% always non-negative
output_eqn(Eqn, Count, Succ) -->
	{ Eqn = eqn(Const, PPId, PPIdList0) },
	{ list__length(PPIdList0, Length) },
	% As there are `Length' negative PPIds, and 1 positive PPId, and
	% each PPId is replaced with `PPId - b', the multiplier of b is
	% Length - 1.
	{ BMultiplier is Length - 1 },
	( { list__delete_first(PPIdList0, PPId, PPIdList1) } ->
		{ Deleted = yes },
		{ PPIdList = PPIdList1 }
	;
		{ Deleted = no },
		{ PPIdList = PPIdList0 }
	),

	( 
		{ Length = 1 },
		{ Deleted = yes }
	->
		% There is nothing on the left hand side  of the
		% constraint, as there was only one element in the list,
		% and it was deleted.  Therefore the left hand side of the
		% equation is PPId - PPId which lpsolve does not process.
		% Constraints of this type should all be removed by 
		% proc_inequalities_scc_remove_useless_offsets.
		{ Succ = no }
	;
		% output 'c#: '
		io__write_char('c'),
		io__write_int(Count),
		io__write_string(": "),
	
		% maybe output '# b '
		( { BMultiplier = 0 } ->
			[]
		;	
			io__write_int(BMultiplier),
			io__write_string(" b ")
		),
		
		% maybe output ' + PPId'
		( { Deleted = yes } ->
			[]
		;
			io__write_string(" + a"),
			output_eqn_ppid(PPId)
		),

		% output `PPIdList' 
		output_eqn_2(PPIdList),

		% output ` > Const;'
		io__write_string(" > "),
		( { Const = set(Int) } ->
			io__write_int(Int), 
			{ Succ = yes }
		;
			{ Succ = no }
		),
		io__write_string(";\n")
	).
	
% Outputs each of the pred proc id's in the form: ' - aPredId_ProcId'
:- pred output_eqn_2(list(pred_proc_id), io__state, io__state).
:- mode output_eqn_2(in, di, uo) is det.

output_eqn_2([]) --> [].
output_eqn_2([ X | Xs ]) --> 
	io__write_string(" - a"),
	output_eqn_ppid(X),
	output_eqn_2(Xs).

% Outputs a pred proc id in the form "PredId_ProcId"
:- pred output_eqn_ppid(pred_proc_id, io__state, io__state).
:- mode output_eqn_ppid(in, di, uo) is det.
output_eqn_ppid(proc(PredId, ProcId)) -->
	{ pred_id_to_int(PredId, PredInt) },
	{ proc_id_to_int(ProcId, ProcInt) },
	io__write_int(PredInt),
	io__write_char('_'),
	io__write_int(ProcInt).

