%-----------------------------------------------------------------------------%
% Copyright (C) 1997-1998, 2003-2004 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% term_pass1.m
%
% Main author: crs.
% Significant parts rewritten by zs.
%
% This file contains the first pass of the termination analysis,
% whose job is to discover an upper bound on the difference between
% the sizes of the output arguments of a procedure on the one hand and
% the sizes of a selected set of input arguments of the procedure
% on the other hand. We refer to this selected set of input arguments
% as the "output suppliers".
%
% For details, please refer to the papers mentioned in termination.m.
%
%-----------------------------------------------------------------------------%

:- module transform_hlds__term_pass1.

:- interface.

:- import_module hlds__hlds_module.
:- import_module hlds__hlds_pred.
:- import_module transform_hlds__term_errors.
:- import_module transform_hlds__term_util.

:- import_module io, list, std_util.

:- type arg_size_result
	--->	ok(
			list(pair(pred_proc_id, int)),
					% Gives the gamma of each procedure
					% in the SCC.
			used_args
					% Gives the output suppliers of
					% each procedure in the SCC.
		)
	;	error(
			list(term_errors__error)
		).

:- pred find_arg_sizes_in_scc(list(pred_proc_id)::in, module_info::in,
	pass_info::in, arg_size_result::out, list(term_errors__error)::out,
	io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds__mode_util.
:- import_module check_hlds__type_util.
:- import_module hlds__hlds_data.
:- import_module hlds__hlds_goal.
:- import_module parse_tree__error_util.
:- import_module parse_tree__prog_data.
:- import_module transform_hlds__lp.
:- import_module transform_hlds__term_errors.
:- import_module transform_hlds__term_traversal.

:- import_module int, float, char, string, bool, set, bag, map.
:- import_module string, term, varset, require.

%-----------------------------------------------------------------------------%

:- type pass1_result
	--->	ok(
			list(path_info),
					% One entry for each path through the
					% code.
			used_args,
					% The next output_supplier map.
			list(term_errors__error)
					% There is an entry in this list for
					% each procedure in the SCC in which
					% the set of active vars is not
					% a subset of the input arguments.
		)
	;	error(
			list(term_errors__error)
		).

find_arg_sizes_in_scc(SCC, Module, PassInfo, ArgSize, TermErrors, !IO) :-
	init_output_suppliers(SCC, Module, InitOutputSupplierMap),
	find_arg_sizes_in_scc_fixpoint(SCC, Module, PassInfo,
		InitOutputSupplierMap, Result, TermErrors),
	(
		Result = ok(Paths, OutputSupplierMap, SubsetErrors),

		( SubsetErrors = [_ | _] ->
			ArgSize = error(SubsetErrors)
		; Paths = [] ->
			get_context_from_scc(SCC, Module, Context),
			ArgSize = error([Context - no_eqns])
		;
			solve_equations(Paths, SCC, MaybeSolution, !IO),
			(
				MaybeSolution = yes(Solution),
				ArgSize = ok(Solution, OutputSupplierMap)
			;
				MaybeSolution = no,
				get_context_from_scc(SCC, Module, Context),
				ArgSize = error([Context - solver_failed])
			)
		)
	;
		Result = error(Errors),
		ArgSize = error(Errors)
	).

%-----------------------------------------------------------------------------%

% Initialise the output suppliers map.
% Initially, we consider that no input arguments contribute their size
% to the output arguments.

:- pred init_output_suppliers(list(pred_proc_id)::in, module_info::in,
	used_args::out) is det.

init_output_suppliers([], _Module, InitMap) :-
	map__init(InitMap).
init_output_suppliers([PPId | PPIds], Module, OutputSupplierMap) :-
	init_output_suppliers(PPIds, Module, OutputSupplierMap0),
	module_info_pred_proc_info(Module, PPId, _, ProcInfo),
	proc_info_headvars(ProcInfo, HeadVars),
	MapToNo = (pred(_HeadVar::in, Bool::out) is det :- Bool = no),
	list__map(MapToNo, HeadVars, BoolList),
	map__det_insert(OutputSupplierMap0, PPId, BoolList, OutputSupplierMap).

%-----------------------------------------------------------------------------%

:- pred find_arg_sizes_in_scc_fixpoint(list(pred_proc_id)::in,
	module_info::in, pass_info::in, used_args::in, pass1_result::out,
	list(term_errors__error)::out) is det.

find_arg_sizes_in_scc_fixpoint(SCC, Module, PassInfo, OutputSupplierMap0,
		Result, TermErrors) :-
	% unsafe_perform_io(io__write_string("find_arg_sizes_in_scc_pass\n")),
	% unsafe_perform_io(io__write(OutputSupplierMap0)),
	% unsafe_perform_io(io__write_string("\n")),
	find_arg_sizes_in_scc_pass(SCC, Module, PassInfo,
		OutputSupplierMap0, [], [], Result1, [], TermErrors1),
	(
		Result1 = error(_),
		Result = Result1,
		TermErrors = TermErrors1
	;
		Result1 = ok(_, OutputSupplierMap1, _),
		( OutputSupplierMap1 = OutputSupplierMap0 ->
			Result = Result1,
			TermErrors = TermErrors1
		;
			find_arg_sizes_in_scc_fixpoint(SCC, Module,
				PassInfo, OutputSupplierMap1,
				Result, TermErrors)
		)
	).

:- pred find_arg_sizes_in_scc_pass(list(pred_proc_id)::in,
	module_info::in, pass_info::in, used_args::in,
	list(path_info)::in, list(term_errors__error)::in, pass1_result::out,
	list(term_errors__error)::in, list(term_errors__error)::out) is det.

find_arg_sizes_in_scc_pass([], _, _, OutputSupplierMap, Paths, SubsetErrors,
		Result, !TermErrors) :-
	Result = ok(Paths, OutputSupplierMap, SubsetErrors).
find_arg_sizes_in_scc_pass([PPId | PPIds], Module, PassInfo,
		OutputSupplierMap0, Paths0, SubsetErrors0, Result,
		!TermErrors) :-
	find_arg_sizes_pred(PPId, Module, PassInfo, OutputSupplierMap0,
		Result1, ProcTermErrors),
	list__append(!.TermErrors, ProcTermErrors, !:TermErrors),
	PassInfo = pass_info(_, MaxErrors, _),
	list__take_upto(MaxErrors, !TermErrors),
	(
		Result1 = error(_),
		Result  = Result1,
			% The error does not necessarily mean that this
			% SCC is nonterminating.  We need to check that
			% the remainder of this SCC does not make any
			% nonterminating calls otherwise we might get
			% a software error during pass 2.
			% (See below for details).
		list__foldl(check_proc_non_term_calls(Module), PPIds, [],
			OtherTermErrors),
		list__append(OtherTermErrors, !TermErrors)
	;
		Result1 = ok(Paths1, OutputSupplierMap1, SubsetErrors1),
		list__append(Paths0, Paths1, Paths),
		list__append(SubsetErrors0, SubsetErrors1, SubsetErrors),
		find_arg_sizes_in_scc_pass(PPIds, Module, PassInfo,
			OutputSupplierMap1, Paths, SubsetErrors, Result,
			!TermErrors)
	).

%-----------------------------------------------------------------------------%

:- pred find_arg_sizes_pred(pred_proc_id::in, module_info::in,
	pass_info::in, used_args::in, pass1_result::out,
	list(term_errors__error)::out) is det.

find_arg_sizes_pred(PPId, Module, PassInfo, OutputSupplierMap0, Result,
		TermErrors) :-
	module_info_pred_proc_info(Module, PPId, PredInfo, ProcInfo),
	pred_info_context(PredInfo, Context),
	proc_info_headvars(ProcInfo, Args),
	proc_info_argmodes(ProcInfo, ArgModes),
	proc_info_vartypes(ProcInfo, VarTypes),
	proc_info_goal(ProcInfo, Goal),
	map__init(EmptyMap),
	PassInfo = pass_info(FunctorInfo, MaxErrors, MaxPaths),
	init_traversal_params(Module, FunctorInfo, PPId, Context, VarTypes,
		OutputSupplierMap0, EmptyMap, MaxErrors, MaxPaths, Params),

	partition_call_args(Module, ArgModes, Args, InVars, OutVars),
	Path0 = path_info(PPId, no, 0, [], OutVars),
	set__singleton_set(PathSet0, Path0),
	Info0 = ok(PathSet0, []),
	traverse_goal(Goal, Params, Info0, Info),

	(
		Info = ok(Paths, TermErrors),
		set__to_sorted_list(Paths, PathList),
		upper_bound_active_vars(PathList, AllActiveVars),
		map__lookup(OutputSupplierMap0, PPId,
			OutputSuppliers0),
		update_output_suppliers(Args, AllActiveVars,
			OutputSuppliers0, OutputSuppliers),
		map__det_update(OutputSupplierMap0, PPId,
			OutputSuppliers, OutputSupplierMap),
		( bag__is_subbag(AllActiveVars, InVars) ->
			SubsetErrors = []
		;
			SubsetErrors = [Context -
				not_subset(PPId, AllActiveVars, InVars)]
		),
		Result = ok(PathList, OutputSupplierMap, SubsetErrors)
	;
		Info = error(Errors, TermErrors),
		Result = error(Errors)
	).

:- pred update_output_suppliers(list(prog_var)::in, bag(prog_var)::in,
		list(bool)::in, list(bool)::out) is det.

update_output_suppliers([], _ActiveVars, [], []).
update_output_suppliers([_ | _], _ActiveVars, [], []) :-
	unexpected(this_file, "update_output_suppliers/4: umatched variables.").
update_output_suppliers([], _ActiveVars, [_ | _], []) :-
	unexpected(this_file, "update_output_suppliers/4: umatched variables.").
update_output_suppliers([Arg | Args], ActiveVars,
		[OutputSupplier0 | OutputSuppliers0],
		[OutputSupplier | OutputSuppliers]) :-
	( bag__contains(ActiveVars, Arg) ->
		OutputSupplier = yes
	;
		% This guarantees that the set of output suppliers can only
		% increase, which in turn guarantees that our fixpoint
		% computation is monotonic and therefore terminates.
		OutputSupplier = OutputSupplier0
	),
	update_output_suppliers(Args, ActiveVars,
		OutputSuppliers0, OutputSuppliers).

%------------------------------------------------------------------------------%
%
% Check if a procedure makes any nonterminating calls.
%

% We only use this if we have detected an error at some point during the
% argument size analysis.  The idea is to quickly analyse a procedure and
% see if it does anything that would prevent us from running pass 2.
% We cannot run pass 2 if the procedure contains any calls to nonterminating
% procedures lower down the call-graph (see term_pass2.m for details).

:- pred check_proc_non_term_calls(module_info::in, pred_proc_id::in,
	list(term_errors__error)::in, list(term_errors__error)::out) is det.

check_proc_non_term_calls(Module, PPId, !Errors) :-
	module_info_pred_proc_info(Module, PPId, _, ProcInfo),
	proc_info_goal(ProcInfo, Goal),
	proc_info_vartypes(ProcInfo, VarTypes),
	check_goal_non_term_calls(Module, PPId, VarTypes, Goal, !Errors).

:- pred check_goal_non_term_calls(module_info::in,
	pred_proc_id::in, vartypes::in, hlds_goal::in,
	list(term_errors__error)::in, list(term_errors__error)::out) is det.

check_goal_non_term_calls(Module, PPId, VarTypes, GoalExpr - GoalInfo,
		!Errors) :-
	check_goal_expr_non_term_calls(Module, PPId, VarTypes, GoalExpr,
		GoalInfo, !Errors).

:- pred check_goal_expr_non_term_calls(module_info::in, pred_proc_id::in,
	vartypes::in, hlds_goal_expr::in, hlds_goal_info::in,
	list(term_errors__error)::in, list(term_errors__error)::out) is det.

check_goal_expr_non_term_calls(Module, PPId, VarTypes, conj(Goals), _,
		!Errors):-
	list__foldl(check_goal_non_term_calls(Module, PPId, VarTypes), Goals,
		!Errors).
check_goal_expr_non_term_calls(Module, PPId, VarTypes,
		call(CallPredId, CallProcId, Args, _, _, _), GoalInfo,
		!Errors) :-
	CallPPId = proc(CallPredId, CallProcId),
	module_info_pred_proc_info(Module, CallPPId, _, ProcInfo),
	proc_info_get_maybe_termination_info(ProcInfo, TerminationInfo),
	goal_info_get_context(GoalInfo, Context),
	(
		TerminationInfo = yes(can_loop(_))
	->
		TermError = Context - can_loop_proc_called(PPId, CallPPId),
		!:Errors = [ TermError | !.Errors ]
	;
		true
	),
	(
		horder_vars(Args, VarTypes)
	->
		HigherOrderError = Context - horder_args(PPId, CallPPId),
		!:Errors = [ HigherOrderError | !.Errors ]
	;
		true
	).
check_goal_expr_non_term_calls(_, _, _, generic_call(_,_,_,_), GoalInfo,
		!Errors) :-
	goal_info_get_context(GoalInfo, Context),
	!:Errors = [ Context - horder_call | !.Errors ].
check_goal_expr_non_term_calls(Module, PPId, VarTypes, switch(_, _, Cases), _,
		!Errors) :-
	list__foldl(check_cases_non_term_calls(Module, PPId, VarTypes), Cases,
		!Errors).
check_goal_expr_non_term_calls(_, _, _, unify(_,_,_,_,_), _, !Errors).
check_goal_expr_non_term_calls(Module, PPId, VarTypes, disj(Goals), _,
		!Errors) :-
	list__foldl(check_goal_non_term_calls(Module, PPId, VarTypes), Goals,
		!Errors).
check_goal_expr_non_term_calls(Module, PPId, VarTypes, not(Goal), _, !Errors) :-
	check_goal_non_term_calls(Module, PPId, VarTypes, Goal, !Errors).
check_goal_expr_non_term_calls(Module, PPId, VarTypes, some(_, _, Goal), _,
		!Errors) :-
	check_goal_non_term_calls(Module, PPId, VarTypes, Goal, !Errors).
check_goal_expr_non_term_calls(Module, PPId, VarTypes,
		if_then_else(_, Cond, Then, Else), _, !Errors) :-
	list__foldl(check_goal_non_term_calls(Module, PPId, VarTypes),
		[Cond, Then, Else], !Errors).
check_goal_expr_non_term_calls(_, _, _, foreign_proc(_, _, _, _, _, _),
		_, !Errors).
check_goal_expr_non_term_calls(Module, PPId, VarTypes, par_conj(Goals), _,
		!Errors) :-
	list__foldl(check_goal_non_term_calls(Module, PPId, VarTypes), Goals,
		!Errors).
check_goal_expr_non_term_calls(_, _, _, shorthand(_), _, _, _) :-
	unexpected(this_file,
		"shorthand goal encountered during termination analysis.").

:- pred check_cases_non_term_calls(module_info::in, pred_proc_id::in,
	vartypes::in, case::in, list(term_errors__error)::in,
	list(term_errors__error)::out) is det.

check_cases_non_term_calls(Module, PPId, VarTypes, case(_, Goal), !Errors) :-
	check_goal_non_term_calls(Module, PPId, VarTypes, Goal, !Errors).

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

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

:- pred solve_equations(list(path_info)::in, list(pred_proc_id)::in,
	maybe(list(pair(pred_proc_id, int)))::out, io::di, io::uo) is det.

solve_equations(Paths, PPIds, Result, !IO) :-
	(
		convert_equations(Paths, Varset, Equations,
			Objective, PPVars)
	->
		map__values(PPVars, AllVars0),
		list__sort_and_remove_dups(AllVars0, AllVars),
		% unsafe_perform_io(io__write_string("before\n")),
		% unsafe_perform_io(io__write(Equations)),
		% unsafe_perform_io(io__write_string("\n")),
		% unsafe_perform_io(io__write(Objective)),
		% unsafe_perform_io(io__write_string("\n")),
		% unsafe_perform_io(io__write(AllVars)),
		% unsafe_perform_io(io__write_string("\n")),
		lp_solve(Equations, min, Objective, Varset, AllVars, Soln,
			!IO),
		% unsafe_perform_io(io__write_string("after\n")),
		(
			Soln = unsatisfiable,
			Result = no
		;
			Soln = satisfiable(_ObjVal, SolnVal),
			list__map(lookup_coeff(PPVars, SolnVal), PPIds,
				SolutionList),
			Result = yes(SolutionList)
		)
	;
		Result = no
	).

:- pred convert_equations(list(path_info)::in, varset::out, lp__equations::out,
	objective::out, map(pred_proc_id, var)::out) is semidet.

convert_equations(Paths, Varset, Equations, Objective, PPVars) :-
	varset__init(Varset0),
	map__init(PPVars0),
	set__init(EqnSet0),
	convert_equations_2(Paths, PPVars0, PPVars, Varset0, Varset,
		EqnSet0, EqnSet),
	set__to_sorted_list(EqnSet, Equations),
	map__values(PPVars, Vars),
	Convert = (pred(Var::in, Coeff::out) is det :- Coeff = Var - 1.0),
	list__map(Convert, Vars, Objective).

:- pred convert_equations_2(list(path_info)::in,
	map(pred_proc_id, var)::in, map(pred_proc_id, var)::out,
	varset::in, varset::out,
	set(lp__equation)::in, set(lp__equation)::out) is semidet.

convert_equations_2([], !PPVars, !Varset, !Eqns).
convert_equations_2([Path | Paths], !PPVars, !Varset, !Eqns) :-
	Path = path_info(ThisPPId, _, IntGamma, PPIds, _),
	FloatGamma = float__float(IntGamma),
	Eqn = eqn(Coeffs, (>=), FloatGamma),
	pred_proc_var(ThisPPId, ThisVar, !Varset, !PPVars),
	Coeffs = [ThisVar - 1.0 | RestCoeffs],
	Convert = (pred(PPId::in, Coeff::out, !.VS::in, !:VS::out, !.PPV::in,
			!:PPV::out) is det :-
		pred_proc_var(PPId, Var, !VS, !PPV),
		Coeff = Var - (-1.0)
	),
	list__map_foldl2(Convert, PPIds, RestCoeffs, !Varset, !PPVars),
	set__insert(!.Eqns, Eqn, !:Eqns),
	convert_equations_2(Paths, !PPVars, !Varset, !Eqns).

:- pred lookup_coeff(map(pred_proc_id, var)::in, map(var, float)::in,
	pred_proc_id::in, pair(pred_proc_id, int)::out) is det.

lookup_coeff(PPIds, Soln, PPId, PPId - ICoeff) :-
	map__lookup(PPIds, PPId, Var),
	map__lookup(Soln, Var, Coeff),
	ICoeff = float__ceiling_to_int(Coeff).

:- pred pred_proc_var(pred_proc_id::in, var::out, varset::in, varset::out,
	map(pred_proc_id, var)::in, map(pred_proc_id, var)::out) is det.

pred_proc_var(PPId, Var, !Varset, !PPVars) :-
	( map__search(!.PPVars, PPId, Var0) ->
		Var = Var0
	;
		varset__new_var(!.Varset, Var, !:Varset),
		map__det_insert(!.PPVars, PPId, Var, !:PPVars)
	).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "term_pass1.m".

%-----------------------------------------------------------------------------%
:- end_module term_pass1.
%-----------------------------------------------------------------------------%
