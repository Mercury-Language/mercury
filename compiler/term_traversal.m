%-----------------------------------------------------------------------------%
% Copyright (C) 1997-2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% term_traversal.m
%
% Main author: crs.
% Significant rewrite by zs.
%
% This module contains the code used to traverse procedure bodies
% for both passes of termination analysis.
%
% For details, please refer to the papers mentioned in termination.m.
%
%-----------------------------------------------------------------------------%

:- module transform_hlds__term_traversal.

:- interface.

:- import_module transform_hlds__term_util, transform_hlds__term_errors.
:- import_module hlds__hlds_module, hlds__hlds_pred, hlds__hlds_goal.
:- import_module parse_tree__prog_data.
:- import_module list, bag, map, std_util, set.

:- type traversal_info
	--->	ok(
			set(path_info),
					% Information about the paths we have
					% followed. With a conjunction of
					% length N, each of whose elements is
					% a branched control structure, the
					% number of paths through the
					% conjunction is 2^N. The reason why
					% we use a set of path_infos instead
					% of a list is that this can postpone
					% the representation getting too big
					% if (as is at least moderately likely)
					% many of the paths have identical
					% properties.
			list(term_errors__error)
					% Have we processed a call to a
					% procedure whose maybe termination
					% info was yes(can_loop(_))?
					% If yes, record the error here.
					% (This is not an error in pass 1,
					% but we want to find this out in
					% pass 1 so we can avoid doing pass 2.)
		)
	;	error(
			list(term_errors__error),
					% Errors which are fatal in both
					% passes.
			list(term_errors__error)
					% Have we processed a call to a
					% procedure whose maybe termination
					% info was yes(can_loop(_))?
					% If yes, record the error here.
					% (This is not an error in pass 1,
					% but we want to find this out in
					% pass 1 so we can avoid doing pass 2.)
		).

:- type path_info
	--->	path_info(
			pred_proc_id,	% The identify of the procedure
					% that this path is within.
			maybe(pair(pred_proc_id, prog_context)),
					% If no, path was started at the end
					% of the procedure given by field 1.
					% If yes, the arg names the procedure
					% at the call to which the path started
					% and the context of the call.
					% In pass 1, all starts should be no.
					% In pass 2, all starts should be yes.
			int,			
			list(pred_proc_id),
			bag(prog_var)
					% These three fields describe the
					% right hand side of the inequation
					% we are propagating.
		).

:- type traversal_params.

:- pred init_traversal_params(module_info::in, functor_info::in,
	pred_proc_id::in, prog_context::in, map(prog_var, type)::in,
	used_args::in, used_args::in, int::in, int::in,
	traversal_params::out) is det.

:- pred traverse_goal(hlds_goal::in, traversal_params::in,
	traversal_info::in, traversal_info::out) is det.

:- pred upper_bound_active_vars(list(path_info)::in, bag(prog_var)::out) is det.

:- implementation.

:- import_module hlds__hlds_data, check_hlds__type_util.
:- import_module bool, int, require.

traverse_goal(Goal, Params, Info0, Info) :-
	Goal = GoalExpr - GoalInfo,
	(
		goal_info_get_determinism(GoalInfo, Detism),
		determinism_components(Detism, _, at_most_zero)
	->
		cannot_succeed(Info0, Info1)
	;
		Info1 = Info0
	),
	traverse_goal_2(GoalExpr, GoalInfo, Params, Info1, Info).

:- pred traverse_goal_2(hlds_goal_expr::in, hlds_goal_info::in,
	traversal_params::in, traversal_info::in, traversal_info::out) is det.

traverse_goal_2(unify(_Var, _RHS, _UniMode, Unification, _Context),
		_GoalInfo, Params, Info0, Info) :-
	(
		Unification = construct(OutVar, ConsId, Args, Modes, _, _, _),
		(
			unify_change(OutVar, ConsId, Args, Modes, Params,
				Gamma, InVars, OutVars0)
		->
			bag__insert(OutVars0, OutVar, OutVars),
			record_change(InVars, OutVars, Gamma, [], Info0, Info)
		;
			% length(Args) is not necessarily equal to length(Modes)
			% for higher order constructions.
			Info = Info0
		)
	;
		Unification = deconstruct(InVar, ConsId, Args, Modes, _, _),
		(
			unify_change(InVar, ConsId, Args, Modes, Params,
				Gamma0, InVars0, OutVars)
		->
			bag__insert(InVars0, InVar, InVars),
			Gamma is 0 - Gamma0,
			record_change(InVars, OutVars, Gamma, [], Info0, Info)
		;
			error("higher order deconstruction")
		)
	;
		Unification = assign(OutVar, InVar),
		bag__init(Empty),
		bag__insert(Empty, InVar, InVars),
		bag__insert(Empty, OutVar, OutVars),
		record_change(InVars, OutVars, 0, [], Info0, Info)
	;
		Unification = simple_test(_InVar1, _InVar2),
		Info = Info0
	;
		Unification = complicated_unify(_, _, _),
		error("Unexpected complicated_unify in termination analysis")
	).

traverse_goal_2(conj(Goals), _, Params, Info0, Info) :-
	list__reverse(Goals, RevGoals),
	traverse_conj(RevGoals, Params, Info0, Info).

traverse_goal_2(par_conj(Goals, _SM), _, Params, Info0, Info) :-
	list__reverse(Goals, RevGoals),
	traverse_conj(RevGoals, Params, Info0, Info).

traverse_goal_2(switch(_, _, Cases, _), _, Params, Info0, Info) :-
	traverse_switch(Cases, Params, Info0, Info).

traverse_goal_2(disj(Goals, _StoreMap), _, Params, Info0, Info) :-
	traverse_disj(Goals, Params, Info0, Info).

traverse_goal_2(not(Goal), _, Params, Info0, Info) :-
		% Since goal cannot bind any active variables,
		% we don't need to traverse Goal for pass1,
		% but it shouldn't hurt either.
	traverse_goal(Goal, Params, Info0, Info).

traverse_goal_2(some(_Vars, _, Goal), _GoalInfo, Params, Info0, Info) :-
	traverse_goal(Goal, Params, Info0, Info).

traverse_goal_2(if_then_else(_, Cond, Then, Else, _), _, Params, Info0, Info) :-
	traverse_conj([Then, Cond], Params, Info0, Info1),
	traverse_goal(Else, Params, Info0, Info2),
	combine_paths(Info1, Info2, Params, Info).

traverse_goal_2(foreign_proc(_, CallPredId, CallProcId, Args, _,_,_),
		GoalInfo, Params, Info0, Info) :-
	params_get_module_info(Params, Module),
	module_info_pred_proc_info(Module, CallPredId, CallProcId, _,
		CallProcInfo),
	proc_info_argmodes(CallProcInfo, CallArgModes),
	partition_call_args(Module, CallArgModes, Args, _InVars, OutVars),
	goal_info_get_context(GoalInfo, Context),
	error_if_intersect(OutVars, Context, pragma_foreign_code, Info0, Info).

traverse_goal_2(generic_call(_, _, _, _), GoalInfo, Params, Info0, Info) :-
	%
	% For class method calls, we could probably analyse further
	% than this, since we know that the method being called must come
	% from one of the instance declarations, and we could potentially
	% (globally) analyse these.
	%
	% Aditi builtins are not guaranteed to terminate
	% - all of them cause the transaction to abort if an error occurs
	% (e.g. if the database server dies).
	% - all except `aditi_insert' execute a user-specified goal
	% which could possibly loop. Analysis of the termination of
	% goals executed bottom-up is not yet implemented.
	%
	% The error message for `generic_call's other than higher-order calls
	% could be better.
	%
	goal_info_get_context(GoalInfo, Context),
	add_error(Context, horder_call, Params, Info0, Info).

traverse_goal_2(call(CallPredId, CallProcId, Args, _, _, _),
		GoalInfo, Params, Info0, Info) :-
	goal_info_get_context(GoalInfo, Context),
	params_get_module_info(Params, Module),
	params_get_ppid(Params, PPId),
	CallPPId = proc(CallPredId, CallProcId),

	module_info_pred_proc_info(Module, CallPredId, CallProcId, _,
		CallProcInfo),
	proc_info_argmodes(CallProcInfo, CallArgModes),
	proc_info_get_maybe_arg_size_info(CallProcInfo, CallArgSizeInfo),
	proc_info_get_maybe_termination_info(CallProcInfo, CallTerminationInfo),

	partition_call_args(Module, CallArgModes, Args, InVars, OutVars),

	% Handle existing paths
	(
		CallArgSizeInfo = yes(finite(CallGamma, OutputSuppliers)),
		remove_unused_args(InVars, Args, OutputSuppliers, UsedInVars),
		record_change(UsedInVars, OutVars, CallGamma, [], Info0, Info1)
	;
		CallArgSizeInfo = yes(infinite(_)),
		error_if_intersect(OutVars, Context,
			inf_termination_const(PPId, CallPPId), Info0, Info1)
	;
		CallArgSizeInfo = no,
		% We should get to this point only in pass 1.
		% In pass 2, OutputSuppliersMap will be empty,
		% which will lead to a runtime abort in map__lookup.
		params_get_output_suppliers(Params, OutputSuppliersMap),
		map__lookup(OutputSuppliersMap, CallPPId, OutputSuppliers),
		remove_unused_args(InVars, Args, OutputSuppliers, UsedInVars),
		record_change(UsedInVars, OutVars, 0, [CallPPId], Info0, Info1)
	),

	% Did we call a non-terminating procedure?
	(
		CallTerminationInfo = yes(can_loop(_))
	->
		called_can_loop(Context, can_loop_proc_called(PPId, CallPPId),
			Params, Info1, Info2)
	;
		Info2 = Info1
	),

	% Did we call a procedure with some procedure-valued arguments?
	(
		% This is an overapproximation, since it includes
		% higher order outputs. XXX
		params_get_var_types(Params, VarTypes),
		horder_vars(Args, VarTypes)
	->
		add_error(Context, horder_args(PPId, CallPPId), Params,
			Info2, Info3)
	;
		Info3 = Info2
	),

	% Do we start another path?
	(
		params_get_rec_input_suppliers(Params, RecInputSuppliersMap),
		map__search(RecInputSuppliersMap, CallPPId, RecInputSuppliers)
	->
		% We should get to this point only in pass 2, and then
		% only if this call is to a procedure in the current SCC.
		% In pass 1, RecInputSuppliersMap will be empty.

		compute_rec_start_vars(Args, RecInputSuppliers, Bag),
		PathStart = yes(CallPPId - Context),
		NewPath = path_info(PPId, PathStart, 0, [], Bag),
		add_path(NewPath, Info3, Info)
	;
		Info = Info3
	).

traverse_goal_2(shorthand(_), _, _, _, _) :-
	% these should have been expanded out by now
	error("traverse_goal_2traverse_goal_2: unexpected shorthand").

%-----------------------------------------------------------------------------%

	% Traverse_conj should be invoked with a reversed list of goals.
	% This is to keep stack consumption down.

:- pred traverse_conj(list(hlds_goal)::in, traversal_params::in,
	traversal_info::in, traversal_info::out) is det.

traverse_conj([], _, Info, Info).
traverse_conj([Goal | Goals], Params, Info0, Info) :-
	traverse_goal(Goal, Params, Info0, Info1),
	traverse_conj(Goals, Params, Info1, Info).

:- pred traverse_disj(list(hlds_goal)::in, traversal_params::in,
	traversal_info::in, traversal_info::out) is det.

traverse_disj([], _, _, ok(Empty, [])) :-
	set__init(Empty).
traverse_disj([Goal | Goals], Params, Info0, Info) :-
	traverse_goal(Goal, Params, Info0, Info1),
	traverse_disj(Goals, Params, Info0, Info2),
	combine_paths(Info1, Info2, Params, Info).

:- pred traverse_switch(list(case)::in, traversal_params::in,
	traversal_info::in, traversal_info::out) is det.

traverse_switch([], _, _, ok(Empty, [])) :-
	set__init(Empty).
traverse_switch([case(_, Goal) | Cases], Params, Info0, Info) :-
	traverse_goal(Goal, Params, Info0, Info1),
	traverse_switch(Cases, Params, Info0, Info2),
	combine_paths(Info1, Info2, Params, Info).

%-----------------------------------------------------------------------------%

:- pred cannot_succeed(traversal_info::in, traversal_info::out) is det.

cannot_succeed(error(Errors, CanLoop), error(Errors, CanLoop)).
cannot_succeed(ok(_, CanLoop), ok(Empty, CanLoop)) :-
	set__init(Empty).

:- pred add_path(path_info::in, traversal_info::in, traversal_info::out) is det.

add_path(_, error(Errors, CanLoop), error(Errors, CanLoop)).
add_path(Path, ok(Paths0, CanLoop), ok(Paths, CanLoop)) :-
	set__insert(Paths0, Path, Paths).

:- pred add_error(prog_context::in, termination_error::in,
	traversal_params::in, traversal_info::in, traversal_info::out) is det.

add_error(Context, Error, Params, error(Errors0, CanLoop),
		error(Errors, CanLoop)) :-
	Errors1 = [Context - Error | Errors0],
	params_get_max_errors(Params, MaxErrors),
	list__take_upto(MaxErrors, Errors1, Errors).
add_error(Context, Error, _, ok(_, CanLoop),
		error([Context - Error], CanLoop)).

:- pred called_can_loop(prog_context::in, termination_error::in,
	traversal_params::in, traversal_info::in, traversal_info::out) is det.

called_can_loop(Context, Error, Params, error(Errors, CanLoop0),
		error(Errors, CanLoop)) :-
	CanLoop1 = [Context - Error | CanLoop0],
	params_get_max_errors(Params, MaxErrors),
	list__take_upto(MaxErrors, CanLoop1, CanLoop).
called_can_loop(Context, Error, Params, ok(Paths, CanLoop0),
		ok(Paths, CanLoop)) :-
	CanLoop1 = [Context - Error | CanLoop0],
	params_get_max_errors(Params, MaxErrors),
	list__take_upto(MaxErrors, CanLoop1, CanLoop).

:- pred combine_paths(traversal_info::in, traversal_info::in,
	traversal_params::in, traversal_info::out) is det.

combine_paths(error(Errors1, CanLoop1), error(Errors2, CanLoop2), Params,
		error(Errors, CanLoop)) :-
	params_get_max_errors(Params, MaxErrors),
	list__append(Errors1, Errors2, Errors3),
	list__take_upto(MaxErrors, Errors3, Errors),
	list__append(CanLoop1, CanLoop2, CanLoop3),
	list__take_upto(MaxErrors, CanLoop3, CanLoop).
combine_paths(error(Errors1, CanLoop1), ok(_, CanLoop2), Params,
		error(Errors1, CanLoop)) :-
	params_get_max_errors(Params, MaxErrors),
	list__append(CanLoop1, CanLoop2, CanLoop3),
	list__take_upto(MaxErrors, CanLoop3, CanLoop).
combine_paths(ok(_, CanLoop1), error(Errors2, CanLoop2), Params,
		error(Errors2, CanLoop)) :-
	params_get_max_errors(Params, MaxErrors),
	list__append(CanLoop1, CanLoop2, CanLoop3),
	list__take_upto(MaxErrors, CanLoop3, CanLoop).
combine_paths(ok(Paths1, CanLoop1), ok(Paths2, CanLoop2), Params,
		Info) :-
	params_get_max_errors(Params, MaxErrors),
	list__append(CanLoop1, CanLoop2, CanLoop3),
	list__take_upto(MaxErrors, CanLoop3, CanLoop),
	set__union(Paths2, Paths1, Paths),
	params_get_max_paths(Params, MaxPaths),
	(
		% Don't try to track the state of too many paths;
		% doing so can require too much memory.
		set__count(Paths, Count),
		Count =< MaxPaths
	->
		Info = ok(Paths, CanLoop)
	;
		params_get_context(Params, Context),
		Info = error([Context - too_many_paths], CanLoop)
	).

%-----------------------------------------------------------------------------%

:- pred compute_rec_start_vars(list(prog_var)::in, list(bool)::in,
	bag(prog_var)::out) is det.

compute_rec_start_vars([], [], Out) :-
	bag__init(Out).
compute_rec_start_vars([_|_], [], _Out) :-
	error("Unmatched vars in compute_rec_start_vars\n").
compute_rec_start_vars([], [_|_], _Out) :-
	error("Unmatched vars in compute_rec_start_vars\n").
compute_rec_start_vars([Var | Vars], [RecInputSupplier | RecInputSuppliers],
		Out) :-
	compute_rec_start_vars(Vars, RecInputSuppliers, Out1),
	( RecInputSupplier = yes ->
		bag__insert(Out1, Var, Out)
	;
		Out = Out1
	).

%-----------------------------------------------------------------------------%

	% unify_change is invoked for unifications of the form X = f(Yi),
	% with the first argument giving the identity of X, the second the
	% identity of f, the third and fourth the identity and modes of the Yi.
	% unify_change returns the norm of f and the bags of input and output
	% variables among the Yi. It is up to the caller to look after the
	% sign of the norm of f and after the membership of X in either the
	% input or output bags. The predicate fails if invoked on a higher
	% order unification.

:- pred unify_change(prog_var::in, cons_id::in, list(prog_var)::in,
	list(uni_mode)::in, traversal_params::in, int::out, bag(prog_var)::out,
	bag(prog_var)::out) is semidet.

unify_change(OutVar, ConsId, Args0, Modes0, Params, Gamma, InVars, OutVars) :-
	params_get_functor_info(Params, FunctorInfo),
	params_get_var_types(Params, VarTypes),
	map__lookup(VarTypes, OutVar, Type),
	\+ type_is_higher_order(Type, _, _, _),
	( type_to_ctor_and_args(Type, TypeCtor, _) ->
		params_get_module_info(Params, Module),
		functor_norm(FunctorInfo, TypeCtor, ConsId, Module,
			Gamma, Args0, Args, Modes0, Modes),
		split_unification_vars(Args, Modes, Module, InVars, OutVars)
	;
		error("variable type in traverse_goal_2")
	).

%-----------------------------------------------------------------------------%

:- pred record_change(bag(prog_var)::in, bag(prog_var)::in, int::in,
	list(pred_proc_id)::in, traversal_info::in, traversal_info::out) is det.

record_change(_, _, _, _, error(Errors, CanLoop), error(Errors, CanLoop)).
record_change(InVars, OutVars, Gamma, CalledPPIds, ok(Paths0, CanLoop),
		ok(NewPaths, CanLoop)) :-
	set__to_sorted_list(Paths0, PathsList0),
	set__init(NewPaths0),
	record_change_2(PathsList0, InVars, OutVars, Gamma, CalledPPIds,
		NewPaths0, NewPaths).

:- pred record_change_2(list(path_info)::in, bag(prog_var)::in,
		bag(prog_var)::in, int::in, list(pred_proc_id)::in,
	set(path_info)::in, set(path_info)::out) is det.

record_change_2([], _, _, _, _, PathSet, PathSet).
record_change_2([Path0 | Paths0], InVars, OutVars, CallGamma, CallPPIds,
		PathSet0, PathSet) :-
	Path0 = path_info(ProcData, Start, Gamma0, PPIds0, Vars0),
	( bag__intersect(OutVars, Vars0) ->
		% The change produces some active variables.
		Gamma is CallGamma + Gamma0,
		list__append(CallPPIds, PPIds0, PPIds),
		bag__subtract(Vars0, OutVars, Vars1),
		bag__union(InVars, Vars1, Vars),
		Path = path_info(ProcData, Start, Gamma, PPIds, Vars)
	;
		% The change produces no active variables.
		Path = Path0
	),
	set__insert(PathSet0, Path, PathSet1),
	record_change_2(Paths0, InVars, OutVars, CallGamma, CallPPIds,
		PathSet1, PathSet).

%-----------------------------------------------------------------------------%

:- pred error_if_intersect(bag(prog_var)::in, prog_context::in,
	termination_error::in, traversal_info::in, traversal_info::out) is det.

error_if_intersect(_, _, _, error(Errors, CanLoop), error(Errors, CanLoop)).
error_if_intersect(OutVars, Context, ErrorMsg, ok(Paths, CanLoop), Info)
		:-
	(
		set__to_sorted_list(Paths, PathList),
		some_active_vars_in_bag(PathList, OutVars)
	->
		Info = error([Context - ErrorMsg], CanLoop)
	;
		Info = ok(Paths, CanLoop)
	).

:- pred some_active_vars_in_bag(list(path_info)::in,
		bag(prog_var)::in) is semidet.

some_active_vars_in_bag([Path | Paths], OutVars) :-
	(
		Path = path_info(_, _, _, _, Vars),
		bag__intersect(Vars, OutVars)
	;
		some_active_vars_in_bag(Paths, OutVars)
	).

%-----------------------------------------------------------------------------%

upper_bound_active_vars([], ActiveVars) :-
	bag__init(ActiveVars).
upper_bound_active_vars([Path | Paths], ActiveVars) :-
	upper_bound_active_vars(Paths, ActiveVars1),
	Path = path_info(_, _, _, _, ActiveVars2),
	bag__least_upper_bound(ActiveVars1, ActiveVars2, ActiveVars).

%-----------------------------------------------------------------------------%

:- type traversal_params
	--->	traversal_params(
			module_info,
			functor_info,
			pred_proc_id,	% The procedure we are tracing through.
			prog_context,	% The context of the procedure.
			map(prog_var, type),
			map(pred_proc_id, list(bool)),
					% Output suppliers of each procedure.
					% Empty during pass 2.
			map(pred_proc_id, list(bool)),
					% Rec input suppliers of each procedure.
					% Empty during pass 1.
			int,		% Max number of errors to gather.
			int		% Max number of paths to analyze.
		).

init_traversal_params(ModuleInfo, FunctorInfo, PredProcId, Context, VarTypes,
		OutputSuppliers, RecInputSuppliers, MaxErrors, MaxPaths,
		Params) :-
	Params = traversal_params(ModuleInfo, FunctorInfo, PredProcId, Context,
		VarTypes, OutputSuppliers, RecInputSuppliers,
		MaxErrors, MaxPaths).

:- pred params_get_module_info(traversal_params::in, module_info::out)
	is det.
:- pred params_get_functor_info(traversal_params::in, functor_info::out)
	is det.
:- pred params_get_ppid(traversal_params::in, pred_proc_id::out)
	is det.
:- pred params_get_context(traversal_params::in, prog_context::out)
	is det.
:- pred params_get_var_types(traversal_params::in, map(prog_var, type)::out)
	is det.
:- pred params_get_output_suppliers(traversal_params::in,
	map(pred_proc_id, list(bool))::out) is det.
:- pred params_get_rec_input_suppliers(traversal_params::in,
	map(pred_proc_id, list(bool))::out) is det.
:- pred params_get_max_errors(traversal_params::in, int::out) is det.
:- pred params_get_max_paths(traversal_params::in, int::out) is det.

params_get_module_info(Params, A) :-
	Params = traversal_params(A, _, _, _, _, _, _, _, _).

params_get_functor_info(Params, B) :-
	Params = traversal_params(_, B, _, _, _, _, _, _, _).

params_get_ppid(Params, C) :-
	Params = traversal_params(_, _, C, _, _, _, _, _, _).

params_get_context(Params, D) :-
	Params = traversal_params(_, _, _, D, _, _, _, _, _).

params_get_var_types(Params, E) :-
	Params = traversal_params(_, _, _, _, E, _, _, _, _).

params_get_output_suppliers(Params, F) :-
	Params = traversal_params(_, _, _, _, _, F, _, _, _).

params_get_rec_input_suppliers(Params, G) :-
	Params = traversal_params(_, _, _, _, _, _, G, _, _).

params_get_max_errors(Params, H) :-
	Params = traversal_params(_, _, _, _, _, _, _, H, _).

params_get_max_paths(Params, I) :-
	Params = traversal_params(_, _, _, _, _, _, _, _, I).

%-----------------------------------------------------------------------------%
