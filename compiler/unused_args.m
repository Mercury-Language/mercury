%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
%  unused_args.m
%
%  Main author - stayl, Jan 1996
%
%  Detects and removes unused input arguments in procedures, especially
%  type_infos. Currently only does analysis within a module.
%
%  To enable the warnings use --warn-unused-args
%  To enable the optimisation use --optimize-unused-args
%
%  An argument is considered used if it (or any of its aliases) are
%	- used in a call to a predicate external to the current module
%	- used in a higher-order call
%	- used to instantiate an output variable
%	- involved in a simple test, switch or a semidet deconstruction
%	- used as an argument to another predicate in this module which is used.
%  When using alternate liveness calculation, the following variables are
%  also considered used
%	- a type-info (or part of a type-info) of a type parameter of the
%	  type of a variable that is used (for example, if a variable
%	  of type list(T) is used, then TypeInfo_for_T is used)
%
%  The first step is to determine which arguments of which predicates are
%	used locally to their predicate. For each unused argument, a set of
%	other arguments that it depends on is built up.
%  The next step is to iterate over the this map, checking for each unused
%	argument whether any of the arguments it depends on has become used
%	in the last iteration. Iterations are repeated until a fixpoint is
%	reached.
%  Warnings are then output. The warning message indicates which arguments
%	are used in none of the modes of a predicate.
%  The predicates are then fixed up. Unused variables and unifications are
%	removed.

:- module transform_hlds__unused_args.

%-------------------------------------------------------------------------------

:- interface.

:- import_module analysis.
:- import_module hlds__hlds_module.

:- import_module io.

:- pred unused_args__process_module(module_info::in, module_info::out,
	io::di, io::uo) is det.

	% Instances used by mmc_analysis.m.
:- type unused_args_answer.
:- type unused_args_func_info.
:- instance analysis(unused_args_func_info, any_call, unused_args_answer).
:- instance partial_order(unused_args_func_info, any_call).
:- instance call_pattern(unused_args_func_info, any_call).
:- instance partial_order(unused_args_func_info, unused_args_answer).
:- instance answer_pattern(unused_args_func_info, unused_args_answer).
:- instance to_string(unused_args_answer).

%-------------------------------------------------------------------------------

:- implementation.

:- import_module check_hlds__inst_match.
:- import_module check_hlds__mode_util.
:- import_module check_hlds__polymorphism.
:- import_module check_hlds__type_util.
:- import_module hlds__goal_util.
:- import_module hlds__hlds_data.
:- import_module hlds__hlds_goal.
:- import_module hlds__hlds_out.
:- import_module hlds__hlds_pred.
:- import_module hlds__instmap.
:- import_module hlds__make_hlds.
:- import_module hlds__passes_aux.
:- import_module hlds__quantification.
:- import_module hlds__special_pred.
:- import_module libs__globals.
:- import_module libs__options.
:- import_module mdbcomp__prim_data.
:- import_module parse_tree__mercury_to_mercury.
:- import_module parse_tree__modules.
:- import_module parse_tree__prog_data.
:- import_module parse_tree__prog_out.
:- import_module parse_tree__prog_util.
:- import_module parse_tree__prog_type.
:- import_module transform_hlds__mmc_analysis.

:- import_module assoc_list.
:- import_module bool.
:- import_module char.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module require.
:- import_module set.
:- import_module std_util.
:- import_module string.

		% Information about the dependencies of a variable
		% that is not known to be used.
:- type usage_info
	--->	unused(set(prog_var), set(arg)).

	% A collection of variable usages for each procedure.
:- type var_usage == map(pred_proc_id, var_dep).

	% arguments are stored as their variable id, not their index
	%	in the argument vector
:- type arg == pair(pred_proc_id, prog_var).

		% Contains dependency information for the variables
		% in a procedure that are not yet known to be used.
:- type var_dep == map(prog_var, usage_info).

:- type warning_info
	--->	warning_info(prog_context, string, int, list(int)).
			% context, pred name, arity, list of args to warn

%-----------------------------------------------------------------------------%
	% Types and instances used by mmc_analysis.m.

	% The list of unused arguments is in sorted order.
:- type unused_args_answer
	--->	unused_args(
			args	:: list(int)
		).

:- func get_unused_args(unused_args_answer) = list(int).

get_unused_args(UnusedArgs) = UnusedArgs ^ args.

:- instance analysis(unused_args_func_info, any_call, unused_args_answer)
		where [
	analysis_name(_, _, _) = "unused_args",
	analysis_version_number(_, _, _) = 1,
	preferred_fixpoint_type(_, _, _) = least_fixpoint
].

:- type unused_args_func_info ---> unused_args_func_info(arity).

:- instance call_pattern(unused_args_func_info, any_call) where [].
:- instance partial_order(unused_args_func_info, any_call) where [
	(more_precise_than(_, _, _) :- semidet_fail),
	(equivalent(_, _, _) :- semidet_succeed)
].

:- instance answer_pattern(unused_args_func_info, unused_args_answer) where [
	bottom(unused_args_func_info(Arity)) = unused_args(1 `..` Arity),
	top(_) = unused_args([])
].
:- instance partial_order(unused_args_func_info, unused_args_answer) where [
	(more_precise_than(_, unused_args(Args1), unused_args(Args2)) :-
		set__subset(sorted_list_to_set(Args2),
			sorted_list_to_set(Args1))
	),
	equivalent(_, Args, Args)
].

:- instance to_string(unused_args_answer) where [
	func(to_string/1) is unused_args_answer_to_string,
	func(from_string/1) is unused_args_answer_from_string
].

:- func unused_args_answer_to_string(unused_args_answer) = string.

unused_args_answer_to_string(unused_args(Args)) =
	string__join_list(" ", list__map(int_to_string, Args)).

:- func unused_args_answer_from_string(string) = unused_args_answer is semidet.

unused_args_answer_from_string(String) = unused_args(Args) :-
	{Digits, Args0} = string__foldl((func(Char, {Digits0, Ints0}) =
		( char__is_digit(Char) ->
			{[Char | Digits0], Ints0}
		;
			{[], [string__det_to_int(
				string__from_rev_char_list(Digits0)) | Ints0]}
		)), String, {[], []}),
	Args = list__reverse([string__det_to_int(
		string__from_rev_char_list(Digits)) | Args0]),
	semidet_succeed.

%-----------------------------------------------------------------------------%

unused_args__process_module(!ModuleInfo, !IO) :-
	globals__io_lookup_bool_option(very_verbose, VeryVerbose, !IO),
	init_var_usage(VarUsage0, PredProcs, ProcCallInfo0, !ModuleInfo, !IO),
	% maybe_write_string(VeryVerbose, "% Finished initialisation.\n", !IO),

	unused_args_pass(PredProcs, VarUsage0, VarUsage),
	% maybe_write_string(VeryVerbose, "% Finished analysis.\n", !IO),

	map__init(UnusedArgInfo0),
	get_unused_arg_info(!.ModuleInfo, PredProcs, VarUsage,
		UnusedArgInfo0, UnusedArgInfo),

	map__keys(UnusedArgInfo, PredProcsToFix),
	globals__io_lookup_bool_option(make_optimization_interface, MakeOpt,
		!IO),
	(
		MakeOpt = yes,
		module_info_name(!.ModuleInfo, ModuleName),
		module_name_to_file_name(ModuleName, ".opt.tmp", no,
			OptFileName, !IO),
		io__open_append(OptFileName, OptFileRes, !IO),
		( OptFileRes = ok(OptFile),
			MaybeOptFile = yes(OptFile)
		; OptFileRes = error(IOError),
			io__error_message(IOError, IOErrorMessage),
			io__write_strings(["Cannot open `", OptFileName,
				"' for output: ", IOErrorMessage], !IO),
			io__set_exit_status(1, !IO),
			MaybeOptFile = no
		)
	;
		MakeOpt = no,
		MaybeOptFile = no
	),
	globals__io_lookup_bool_option(warn_unused_args, DoWarn, !IO),
	( ( DoWarn = yes ; MakeOpt = yes ) ->
		set__init(WarnedPredIds0),
		output_warnings_and_pragmas(!.ModuleInfo, UnusedArgInfo,
			MaybeOptFile, DoWarn, PredProcsToFix, WarnedPredIds0,
			!IO)
	;
		true
	),
	(
		MaybeOptFile = yes(OptFile2),
		io__close_output(OptFile2, !IO)
	;
		MaybeOptFile = no
	),
	globals__io_lookup_bool_option(optimize_unused_args, DoFixup, !IO),
	(
		DoFixup = yes,
		list__foldl3(create_new_pred(UnusedArgInfo), PredProcsToFix,
			ProcCallInfo0, ProcCallInfo, !ModuleInfo, !IO),
		% maybe_write_string(VeryVerbose, "% Finished new preds.\n",
		%	!IO),
		fixup_unused_args(VarUsage, PredProcs, ProcCallInfo,
			!ModuleInfo, VeryVerbose, !IO),
		% maybe_write_string(VeryVerbose, "% Fixed up goals.\n", !IO),
		( map__is_empty(ProcCallInfo) ->
			true
		;
			% The dependencies have changed, so the dependency
			% graph needs rebuilding.
			module_info_clobber_dependency_info(!ModuleInfo)
		)
	;
		DoFixup = no
	).

%-------------------------------------------------------------------------------

	% Initialisation section

	% init_var_usage/4 -  set initial status of all args of local
	%	procs by examining the module_info.
	% PredProcList is the list of procedures to do the fixpoint
	% iteration over.
	% OptPredProcList is a list of procedures for which we got
	% unused argument information from .opt files.
:- pred init_var_usage(var_usage::out, pred_proc_list::out,
	proc_call_info::out, module_info::in, module_info::out,
	io::di, io::uo) is det.

init_var_usage(VarUsage, PredProcList, ProcCallInfo, !ModuleInfo, !IO) :-
	map__init(ProcCallInfo0),
	map__init(VarUsage0),
	module_info_predids(!.ModuleInfo, PredIds),
	module_info_unused_arg_info(!.ModuleInfo, UnusedArgInfo),
	setup_local_var_usage(PredIds, UnusedArgInfo,
		VarUsage0, VarUsage, [], PredProcList,
		ProcCallInfo0, ProcCallInfo, !ModuleInfo, !IO).

	% setup args for the whole module.
:- pred setup_local_var_usage(list(pred_id)::in, unused_arg_info::in,
	var_usage::in, var_usage::out, pred_proc_list::in, pred_proc_list::out,
	proc_call_info::in, proc_call_info::out,
	module_info::in, module_info::out, io::di, io::uo) is det.

setup_local_var_usage([], _, !VarUsage, !PredProcs, !OptProcs, !ModuleInfo,
		!IO).
setup_local_var_usage([PredId | PredIds], UnusedArgInfo, !VarUsage,
		!PredProcList, !OptProcs, !ModuleInfo, !IO) :-
	module_info_pred_info(!.ModuleInfo, PredId, PredInfo),
	(
		% The builtins use all their arguments.
		% We also want to treat stub procedures
		% (those which originally had no clauses)
		% as if they use all of their arguments,
		% to avoid spurious warnings in their callers.
		(
			pred_info_is_builtin(PredInfo)
		;
			pred_info_get_markers(PredInfo, Markers),
			check_marker(Markers, stub)
		)
	->
		setup_local_var_usage(PredIds, UnusedArgInfo, !VarUsage,
			!PredProcList, !OptProcs, !ModuleInfo, !IO)
	;
		ProcIds = pred_info_procids(PredInfo),
		setup_pred_args(PredId, ProcIds, UnusedArgInfo, !VarUsage,
			!PredProcList, !OptProcs, !ModuleInfo, !IO),
		setup_local_var_usage(PredIds, UnusedArgInfo, !VarUsage,
			!PredProcList, !OptProcs, !ModuleInfo, !IO)
	).

	% setup args for a predicate
:- pred setup_pred_args(pred_id::in, list(proc_id)::in, unused_arg_info::in,
	var_usage::in, var_usage::out, pred_proc_list::in, pred_proc_list::out,
	proc_call_info::in, proc_call_info::out,
	module_info::in, module_info::out, io::di, io::uo) is det.

setup_pred_args(_, [], _, !VarUsage, !PredProcs, !OptProcs, !ModuleInfo, !IO).
setup_pred_args(PredId, [ProcId | Rest], UnusedArgInfo, !VarUsage, !PredProcs,
		!OptProcs, !ModuleInfo, !IO) :-
	module_info_pred_proc_info(!.ModuleInfo, PredId, ProcId,
		PredInfo, ProcInfo),
	map__init(VarDep0),
	globals__io_lookup_bool_option(intermodule_analysis, Intermod, !IO),
	(
		% Don't use the intermodule analysis info when we have the
		% clauses (opt_imported preds) since we may be able to do
		% better with the information in this module.
		Intermod = yes,
		pred_info_is_imported(PredInfo)
	->
		PredModule = pred_info_module(PredInfo),
		PredOrFunc = pred_info_is_pred_or_func(PredInfo),
		PredName = pred_info_name(PredInfo),
		PredArity = pred_info_orig_arity(PredInfo),
		FuncId = pred_or_func_name_arity_to_func_id(PredOrFunc,
			PredName, PredArity, ProcId),
		module_info_analysis_info(!.ModuleInfo, AnalysisInfo0),
		lookup_best_result(module_name_to_module_id(PredModule),
			FuncId, unused_args_func_info(PredArity),
			any_call, MaybeBestResult,
			AnalysisInfo0, AnalysisInfo, !IO),
		module_info_set_analysis_info(AnalysisInfo, !ModuleInfo),
		( MaybeBestResult = yes(_ - unused_args(UnusedArgs)) ->
			proc_info_headvars(ProcInfo, HeadVars),
			list__map(list__index1_det(HeadVars),
				UnusedArgs, UnusedVars),
			initialise_vardep(VarDep0, UnusedVars, VarDep),
			!:VarUsage = map__set(!.VarUsage,
				proc(PredId, ProcId), VarDep),
			globals__io_lookup_bool_option(optimize_unused_args,
				Optimize, !IO),
			( Optimize = yes ->
				make_imported_unused_args_pred_info(
					proc(PredId, ProcId), UnusedArgs,
					!OptProcs, !ModuleInfo)
			;
				true
			)
		;
			true
		)
	;
		(
			pred_info_is_imported(PredInfo)
		;
			pred_info_is_pseudo_imported(PredInfo),
			hlds_pred__in_in_unification_proc_id(ProcId)
		)
	->
		true
	;
		proc_info_vartypes(ProcInfo, VarTypes),
		map__keys(VarTypes, Vars),
		initialise_vardep(VarDep0, Vars, VarDep1),
		setup_output_args(!.ModuleInfo, ProcInfo, VarDep1, VarDep2),

		module_info_globals(!.ModuleInfo, Globals),
		proc_interface_should_use_typeinfo_liveness(PredInfo, ProcId,
			Globals, TypeInfoLiveness),
		( TypeInfoLiveness = yes ->
			proc_info_typeinfo_varmap(ProcInfo, TVarMap),
			setup_typeinfo_deps(Vars, VarTypes,
				proc(PredId, ProcId), TVarMap, VarDep2,
				VarDep3)
		;
			VarDep2 = VarDep3
		),

		proc_info_goal(ProcInfo, Goal - _),
		Info = traverse_info(!.ModuleInfo, VarTypes),
		traverse_goal(Info, Goal, VarDep3, VarDep),
		!:VarUsage = map__set(!.VarUsage,
			proc(PredId, ProcId), VarDep),

		!:PredProcs = [proc(PredId, ProcId) | !.PredProcs]
	),
	setup_pred_args(PredId, Rest, UnusedArgInfo,
		!VarUsage, !PredProcs, !OptProcs, !ModuleInfo, !IO).

:- pred initialise_vardep(var_dep::in, list(prog_var)::in, var_dep::out) is det.

initialise_vardep(VarDep, [], VarDep).
initialise_vardep(VarDep0, [Var | Vars], VarDep) :-
	set__init(VDep),
	set__init(Args),
	map__set(VarDep0, Var, unused(VDep, Args), VarDep1),
	initialise_vardep(VarDep1, Vars, VarDep).

%-------------------------------------------------------------------------------
	% Predicates for manipulating the var_usage and var_dep structures.

	% For each variable ensure the typeinfos describing the
	% type parameters of the type of the variable depend on the
	% head variable.
	% For example, if HeadVar1 has type list(T), then TypeInfo_for_T
	% is used if HeadVar1 is used.
:- pred setup_typeinfo_deps(list(prog_var)::in, map(prog_var, type)::in,
	pred_proc_id::in, map(tvar, type_info_locn)::in,
	var_dep::in, var_dep::out) is det.

setup_typeinfo_deps([], _, _, _, !VarDep).
setup_typeinfo_deps([Var | Vars], VarTypeMap, PredProcId, TVarMap, !VarDep) :-
	map__lookup(VarTypeMap, Var, Type),
	prog_type__vars(Type, TVars),
	list__map((pred(TVar::in, TypeInfoVar::out) is det :-
		map__lookup(TVarMap, TVar, Locn),
		type_info_locn_var(Locn, TypeInfoVar)
	), TVars, TypeInfoVars),
	AddArgDependency =
		(pred(TVar::in, VarDepA::in, VarDepB::out) is det :-
			add_arg_dep(TVar, PredProcId, Var, VarDepA, VarDepB)
		),
	list__foldl(AddArgDependency, TypeInfoVars, !VarDep),
	setup_typeinfo_deps(Vars, VarTypeMap, PredProcId, TVarMap, !VarDep).

	% Get output arguments for a procedure given the headvars and the
	% argument modes, and set them as used.
:- pred setup_output_args(module_info::in, proc_info::in,
	var_dep::in, var_dep::out) is det.

setup_output_args(ModuleInfo, ProcInfo, !VarDep) :-
	proc_info_instantiated_head_vars(ModuleInfo, ProcInfo,
		ChangedInstHeadVars),
	list__foldl(set_var_used, ChangedInstHeadVars, !VarDep).

	% searches for the dependencies of a variable, succeeds if the variable
	%	is definitely used
:- pred var_is_used(pred_proc_id::in, prog_var::in, var_usage::in) is semidet.

var_is_used(PredProc, Var, VarUsage) :-
	\+ (
		map__search(VarUsage, PredProc, UsageInfos),
		map__contains(UsageInfos, Var)
	).

:- pred local_var_is_used(var_dep::in, prog_var::in) is semidet.

local_var_is_used(VarDep, Var) :-
	\+ map__contains(VarDep, Var).

		% add a list of aliases for a variable
:- pred add_aliases(prog_var::in, list(prog_var)::in,
	var_dep::in, var_dep::out) is det.

add_aliases(Var, Aliases, !VarDep) :-
	( map__search(!.VarDep, Var, VarInf0) ->
		VarInf0 = unused(VarDep0, ArgDep),
		set__insert_list(VarDep0, Aliases, VarDep),
		VarInf = unused(VarDep, ArgDep),
		map__det_update(!.VarDep, Var, VarInf, !:VarDep)
	;
		true
	).

:- pred set_list_vars_used(list(prog_var)::in, var_dep::in, var_dep::out)
	is det.

set_list_vars_used(Vars, !VarDep) :-
	map__delete_list(!.VarDep, Vars, !:VarDep).

:- pred set_var_used(prog_var::in, var_dep::in, var_dep::out) is det.

set_var_used(Var, !VarDep) :-
	map__delete(!.VarDep, Var, !:VarDep).

:- pred lookup_local_var(var_dep::in, prog_var::in, usage_info::out) is semidet.

lookup_local_var(VarDep, Var, UsageInfo) :-
	map__search(VarDep, Var, UsageInfo).

%-------------------------------------------------------------------------------
	% Traversal of goal structure, building up dependencies for all
	% variables.

:- type traverse_info
	--->	traverse_info(
			module_info :: module_info,
			vartypes :: vartypes
		).

:- pred traverse_goal(traverse_info::in, hlds_goal_expr::in,
	var_dep::in, var_dep::out) is det.

% handle conjunction
traverse_goal(Info, conj(Goals), !VarDep) :-
	traverse_list_of_goals(Info, Goals, !VarDep).

% handle parallel conjunction
traverse_goal(Info, par_conj(Goals), !VarDep) :-
	traverse_list_of_goals(Info, Goals, !VarDep).

% handle disjunction
traverse_goal(Info, disj(Goals), !VarDep) :-
	traverse_list_of_goals(Info, Goals, !VarDep).

% handle switch
traverse_goal(Info, switch(Var, _, Cases), !VarDep) :-
	set_var_used(Var, !VarDep),
	list_case_to_list_goal(Cases, Goals),
	traverse_list_of_goals(Info, Goals, !VarDep).

% handle predicate call
traverse_goal(Info, call(PredId, ProcId, Args, _, _, _), !VarDep) :-
	module_info_pred_proc_info(Info^module_info, PredId, ProcId, _Pred,
		Proc),
	proc_info_headvars(Proc, HeadVars),
	add_pred_call_arg_dep(proc(PredId, ProcId), Args, HeadVars,
		!VarDep).

% handle if then else
traverse_goal(Info, if_then_else(_, Cond - _, Then - _, Else - _),
			!VarDep) :-
	traverse_goal(Info, Cond, !VarDep),
	traverse_goal(Info, Then, !VarDep),
	traverse_goal(Info, Else, !VarDep).

% handle negation
traverse_goal(Info, not(Goal - _), !VarDep) :-
	traverse_goal(Info, Goal, !VarDep).

% handle quantification
traverse_goal(Info, scope(_, Goal - _), !VarDep) :-
	traverse_goal(Info, Goal, !VarDep).

% we assume that higher-order predicate calls use all variables involved
traverse_goal(_, generic_call(GenericCall, Args, _, _), !VarDep) :-
	goal_util__generic_call_vars(GenericCall, CallArgs),
	set_list_vars_used(CallArgs, !VarDep),
	set_list_vars_used(Args, !VarDep).

% handle pragma foreign_proc(...) -
% only those arguments which have names can be used in the foreign code.
traverse_goal(_, foreign_proc(_, _, _, Args, ExtraArgs, _),
		!VarDep) :-
	ArgIsUsed = (pred(Arg::in, Var::out) is semidet :-
		Arg = foreign_arg(Var, MaybeNameAndMode, _),
		MaybeNameAndMode = yes(_)
	),
	list__filter_map(ArgIsUsed, Args ++ ExtraArgs, UsedVars),
	set_list_vars_used(UsedVars, !VarDep).

% cases to handle all the different types of unification
traverse_goal(_, unify(_, _, _, simple_test(Var1, Var2),_), !VarDep)
		:-
	set_var_used(Var1, !VarDep),
	set_var_used(Var2, !VarDep).

traverse_goal(_, unify(_, _, _, assign(Var1, Var2), _), !VarDep) :-
	( local_var_is_used(!.VarDep, Var1) ->
		% if Var1 used to instantiate an output argument, Var2 used
		set_var_used(Var2, !VarDep)
	;
		add_aliases(Var2, [Var1], !VarDep)
	).

traverse_goal(Info,
		unify(Var1, _, _,
			deconstruct(_, _, Args, Modes, CanFail, _), _),
		!VarDep) :-
	partition_deconstruct_args(Info, Args,
		Modes, InputVars, OutputVars),
		% The deconstructed variable is used if any of the
		% variables, that the deconstruction binds are used.
	add_aliases(Var1, OutputVars, !VarDep),
		% Treat a deconstruction that further instantiates its
		% left arg as a partial construction.
	add_construction_aliases(Var1, InputVars, !VarDep),
	(
		CanFail = can_fail
	->
		% a deconstruction that can_fail uses its left arg
		set_var_used(Var1, !VarDep)
	;
		true
	).

traverse_goal(_, unify(Var1, _, _, construct(_, _, Args, _, _, _, _), _),
		!VarDep) :-
	( local_var_is_used(!.VarDep, Var1) ->
		set_list_vars_used(Args, !VarDep)
	;
		add_construction_aliases(Var1, Args, !VarDep)
	).

	% These should be transformed into calls by polymorphism.m.
traverse_goal(_, unify(Var, Rhs, _, complicated_unify(_, _, _), _),
		!VarDep) :-
	% This is here to cover the case where unused arguments is called
	% with --error-check-only and polymorphism has not been run.
	% Complicated unifications should only be var-var.
	( Rhs = var(RhsVar) ->
		set_var_used(RhsVar, !VarDep),
		set_var_used(Var, !VarDep)
	;
		error("complicated unifications should only be var-var")
	).

traverse_goal(_, shorthand(_), _, _) :-
	% these should have been expanded out by now
	error("traverse_goal: unexpected shorthand").

	% add PredProc - HeadVar as an alias for the same element of Args.
:- pred add_pred_call_arg_dep(pred_proc_id::in, list(prog_var)::in,
	list(prog_var)::in, var_dep::in, var_dep::out) is det.

add_pred_call_arg_dep(PredProc, LocalArguments, HeadVarIds, !VarDep) :-
	(
		LocalArguments = [Arg | Args],
		HeadVarIds = [HeadVar | HeadVars]
	->
		add_arg_dep(Arg, PredProc, HeadVar, !VarDep),
		add_pred_call_arg_dep(PredProc, Args, HeadVars, !VarDep)
	;
		LocalArguments = [],
		HeadVarIds = []
	->
		true
	;
		error("add_pred_call_arg_dep: invalid call")
	).

:- pred add_arg_dep(prog_var::in, pred_proc_id::in, prog_var::in,
	var_dep::in, var_dep::out) is det.

add_arg_dep(Var, PredProc, Arg, !VarDep) :-
	( lookup_local_var(!.VarDep, Var, VarUsage0) ->
		VarUsage0 = unused(VarDep, ArgDep0),
		set__insert(ArgDep0, PredProc - Arg, ArgDep),
		map__det_update(!.VarDep, Var, unused(VarDep, ArgDep),
			!:VarDep)
	;
		true
	).

	% Partition the arguments to a deconstruction into inputs
	% and outputs.
:- pred partition_deconstruct_args(traverse_info::in, list(prog_var)::in,
	list(uni_mode)::in, list(prog_var)::out, list(prog_var)::out) is det.

partition_deconstruct_args(Info, ArgVars, ArgModes, InputVars, OutputVars) :-
	(
		ArgVars = [Var | Vars],
		ArgModes = [Mode | Modes]
	->
		partition_deconstruct_args(Info, Vars, Modes, InputVars1,
			OutputVars1),
		Mode = ((InitialInst1 - InitialInst2) ->
			(FinalInst1 - FinalInst2)),

		map__lookup(Info^vartypes, Var, Type),

		% If the inst of the argument of the LHS is changed,
		% the argument is input.
		(
			inst_matches_binding(InitialInst1, FinalInst1,
				Type, Info^module_info)
		->
			InputVars = InputVars1
		;
			InputVars = [Var | InputVars1]
		),

		% If the inst of the argument of the RHS is changed,
		% the argument is output.
		(
			inst_matches_binding(InitialInst2, FinalInst2,
				Type, Info^module_info)
		->
			OutputVars = OutputVars1
		;
			OutputVars = [Var | OutputVars1]
		)
	;
		ArgVars = [],
		ArgModes = []
	->
		InputVars = [],
		OutputVars = []
	;
		error("get_instantiating_variables - invalid call")
	).

		% add Alias as an alias for all of Vars
:- pred add_construction_aliases(prog_var::in, list(prog_var)::in,
	var_dep::in, var_dep::out) is det.

add_construction_aliases(_, [], !VarDep).
add_construction_aliases(Alias, [Var | Vars], !VarDep) :-
	( lookup_local_var(!.VarDep, Var, VarInf) ->
		VarInf = unused(VarDep0, ArgDep),
		set__insert(VarDep0, Alias, VarDep),
		map__set(!.VarDep, Var, unused(VarDep, ArgDep), !:VarDep)
	;
		true
	),
	add_construction_aliases(Alias, Vars, !VarDep).

:- pred list_case_to_list_goal(list(case)::in, list(hlds_goal)::out) is det.

list_case_to_list_goal([], []).
list_case_to_list_goal([case(_, Goal) | Cases], [Goal | Goals]) :-
	list_case_to_list_goal(Cases, Goals).

:- pred traverse_list_of_goals(traverse_info::in, list(hlds_goal)::in,
	var_dep::in, var_dep::out) is det.

traverse_list_of_goals(_, [], !VarDep).
traverse_list_of_goals(Info, [Goal - _ | Goals], !VarDep) :-
	traverse_goal(Info, Goal, !VarDep),
	traverse_list_of_goals(Info, Goals, !VarDep).

%-------------------------------------------------------------------------------
	% Analysis section - do the fixpoint iteration.

	% Do a full iteration, check if anything changed, if so, repeat.
:- pred unused_args_pass(pred_proc_list::in, var_usage::in,var_usage::out)
	is det.

unused_args_pass(LocalPredProcs, !VarUsage) :-
	unused_args_single_pass(LocalPredProcs, no, Changed, !VarUsage),
	(
		Changed = yes,
		unused_args_pass(LocalPredProcs, !VarUsage)
	;
		Changed = no
	).

	% check over all the procedures in a module
:- pred unused_args_single_pass(pred_proc_list::in, bool::in, bool::out,
	var_usage::in, var_usage::out) is det.

unused_args_single_pass([], !Changed, !VarUsage).
unused_args_single_pass([PredProc | Rest], !Changed, !VarUsage) :-
	unused_args_check_proc(PredProc, !Changed, !VarUsage),
	unused_args_single_pass(Rest, !Changed, !VarUsage).

	% check a single procedure
:- pred unused_args_check_proc(pred_proc_id::in, bool::in, bool::out,
	var_usage::in, var_usage::out) is det.

unused_args_check_proc(PredProcId, !Changed, !VarUsage) :-
	map__lookup(!.VarUsage, PredProcId, LocalUsages0),
	map__keys(LocalUsages0, Vars),
	unused_args_check_all_vars(!.VarUsage, Vars, no, LocalChanged,
		LocalUsages0, LocalUsages),
	(
		LocalChanged = yes,
		map__det_update(!.VarUsage, PredProcId, LocalUsages,
			!:VarUsage),
		!:Changed = yes
	;
		LocalChanged = no
	).

	% check each var of a procedure in turn
:- pred unused_args_check_all_vars(var_usage::in, list(prog_var)::in,
	bool::in, bool::out, var_dep::in, var_dep::out) is det.

unused_args_check_all_vars(_, [], !Changed, !LocalVars).
unused_args_check_all_vars(VarUsage, [Var| Vars], !Changed, !LocalVars) :-
	( lookup_local_var(!.LocalVars, Var, Usage) ->
		Usage = unused(VarDep0, ArgDep0),
		(
			(
				% Check whether any arguments that the
				% current variable depends on are used.
				set__member(Argument, ArgDep0),
				Argument = PredProc - ArgVar,
				var_is_used(PredProc, ArgVar, VarUsage)
			;
				% Check whether any variables that the
				% current variable depends on are used.
				set__member(Var2, VarDep0),
				local_var_is_used(!.LocalVars, Var2)
			)
		->
			% set the current variable to used
			set_var_used(Var, !LocalVars),
			!:Changed = yes
		;
			true
		)
	;
		true
	),
	unused_args_check_all_vars(VarUsage, Vars, !Changed, !LocalVars).

:- pred get_unused_arg_info(module_info::in, pred_proc_list::in, var_usage::in,
	unused_arg_info::in, unused_arg_info::out) is det.

get_unused_arg_info(_, [], _, !UnusedArgInfo).
get_unused_arg_info(ModuleInfo, [PredProc | PredProcs], VarUsage,
		!UnusedArgInfo) :-
	PredProc = proc(PredId, ProcId),
	map__lookup(VarUsage, PredProc, LocalVarUsage),
	module_info_pred_proc_info(ModuleInfo, PredId, ProcId, _, ProcInfo),
	proc_info_headvars(ProcInfo, HeadVars),
	get_unused_arg_nos(LocalVarUsage, HeadVars, 1, UnusedArgs),
	map__det_insert(!.UnusedArgInfo, PredProc, UnusedArgs,
		!:UnusedArgInfo),
	get_unused_arg_info(ModuleInfo, PredProcs, VarUsage, !UnusedArgInfo).

%-------------------------------------------------------------------------------
	% Fix up the module

	% information about predicates which have new predicates
	% created for the optimized version
:- type proc_call_info == map(pred_proc_id, new_proc_info).

	% new pred_id, proc_id, name, and the indices in the argument
	% vector of the arguments that have been removed.
:- type new_proc_info
	--->	call_info(pred_id, proc_id, sym_name, list(int)).

		% Create a new predicate for each procedure which has unused
		% arguments. There are two reasons why we can't throw away
		% the old procedure for non-exported predicates. One is
		% higher-order terms - we can't remove arguments from them
		% without changing their type, so they need the old calling
		% interface.
		% The other is that the next proc_id for a predicate is
		% chosen based on the length of the list of proc_ids.
:- pred create_new_pred(unused_arg_info::in, pred_proc_id::in,
	proc_call_info::in, proc_call_info::out,
	module_info::in, module_info::out, io::di, io::uo) is det.

create_new_pred(UnusedArgInfo, proc(PredId, ProcId), !ProcCallInfo,
		!ModuleInfo, !IO) :-
	map__lookup(UnusedArgInfo, proc(PredId, ProcId), UnusedArgs),
	module_info_pred_proc_info(!.ModuleInfo, PredId, ProcId,
		OrigPredInfo, OrigProcInfo),
	(
		UnusedArgs = []
	;
		UnusedArgs = [_ | _],
		PredModule = pred_info_module(OrigPredInfo),
		PredName = pred_info_name(OrigPredInfo),

		globals__io_lookup_bool_option(intermodule_analysis,
			Intermod, !IO),
		(
			Intermod = yes,
			module_info_analysis_info(!.ModuleInfo, AnalysisInfo0),
			PredOrFunc = pred_info_is_pred_or_func(OrigPredInfo),
			PredArity = pred_info_orig_arity(OrigPredInfo),
			ModuleId = module_name_to_module_id(PredModule),
			FuncId = pred_or_func_name_arity_to_func_id(PredOrFunc,
				PredName, PredArity, ProcId),
			FuncInfo = unused_args_func_info(PredArity),

			lookup_results(ModuleId, FuncId, FuncInfo,
				IntermodResultsPairs `with_type`
				assoc_list(any_call, unused_args_answer),
				AnalysisInfo0, AnalysisInfo1, !IO),
			IntermodResultsArgsLists = list__map(get_unused_args,
				assoc_list__values(IntermodResultsPairs)),
			( list__member(UnusedArgs, IntermodResultsArgsLists) ->
				AnalysisInfo = AnalysisInfo1
			;
				analysis__record_result(ModuleId,
					FuncId, FuncInfo, any_call,
					unused_args(UnusedArgs),
					AnalysisInfo1, AnalysisInfo)
			),
			module_info_set_analysis_info(AnalysisInfo,
				!ModuleInfo),

			%
			% XXX Mark versions which have more unused arguments
			% than what we have computed here as invalid
			% in the AnalysisInfo, so that modules which use
			% those versions can be recompiled.
			%
			FilterUnused = (pred(VersionUnused::in) is semidet :-
				VersionUnused \= UnusedArgs,
				set__subset(
					sorted_list_to_set(VersionUnused),
					sorted_list_to_set(UnusedArgs)
				)
			),
			IntermodResults = list__filter(FilterUnused,
				IntermodResultsArgsLists)
		;
			Intermod = no,
			IntermodResultsPairs = [],
			IntermodResults = []
		),
		pred_info_import_status(OrigPredInfo, Status0),
		(
			Status0 = opt_imported,
			IntermodResultsPairs = [_ | _],
			IntermodResults = []
		->
			% If this predicate is from a .opt file, and
			% no more arguments have been removed than in the
			% original module, then leave the import status
			% as opt_imported so that dead_proc_elim will remove
			% it if no other optimization is performed on it.
			Status = opt_imported
		;
			status_is_exported(Status0, yes)
		->
			% This specialized version of the predicate will be
			% declared in the analysis file for this module so
			% it must be exported.
			Status = Status0
		;
			Status = local
		),
		make_new_pred_info(!.ModuleInfo, UnusedArgs, Status,
			proc(PredId, ProcId), OrigPredInfo, NewPredInfo0),
		NewPredName = pred_info_name(NewPredInfo0),
		pred_info_procedures(NewPredInfo0, NewProcs0),

			% Assign the old procedure to a new predicate, which
			% will be fixed up in fixup_unused_args.
		map__set(NewProcs0, ProcId, OrigProcInfo, NewProcs),
		pred_info_set_procedures(NewProcs, NewPredInfo0, NewPredInfo),

			% add the new proc to the pred table
		module_info_get_predicate_table(!.ModuleInfo, PredTable0),
		predicate_table_insert(NewPredInfo, NewPredId,
			PredTable0, PredTable),
		module_info_set_predicate_table(PredTable, !ModuleInfo),

			% add the new proc to the proc_call_info map
		PredSymName = qualified(PredModule, NewPredName),
		map__det_insert(!.ProcCallInfo, proc(PredId, ProcId),
			call_info(NewPredId, ProcId, PredSymName, UnusedArgs),
			!:ProcCallInfo),

			% add a forwarding predicate with the
			% original interface.
		create_call_goal(UnusedArgs, NewPredId, ProcId, PredModule,
			NewPredName, OrigProcInfo, ForwardingProcInfo),
		module_info_set_pred_proc_info(PredId, ProcId, OrigPredInfo,
			ForwardingProcInfo, !ModuleInfo),

			% add forwarding predicates for results
			% produced in previous compilations.
		list__foldl(
			make_intermod_proc(PredId, NewPredId, ProcId,
				NewPredName, OrigPredInfo, OrigProcInfo,
				UnusedArgs),
			IntermodResults, !ModuleInfo)
	).

:- pred make_intermod_proc(pred_id::in, pred_id::in, proc_id::in, string::in,
	pred_info::in, proc_info::in, list(int)::in, list(int)::in,
	module_info::in, module_info::out) is det.

make_intermod_proc(PredId, NewPredId, ProcId, NewPredName,
		OrigPredInfo, OrigProcInfo, UnusedArgs,
		UnusedArgs2, !ModuleInfo) :-
	% Add an exported predicate with the number of removed
	% arguments promised in the analysis file which just calls
	% the new predicate.
	make_new_pred_info(!.ModuleInfo, UnusedArgs2, exported,
		proc(PredId, ProcId), OrigPredInfo, ExtraPredInfo0),
	PredModule = pred_info_module(OrigPredInfo),
	create_call_goal(UnusedArgs, NewPredId, ProcId,
		PredModule, NewPredName, OrigProcInfo, ExtraProc0),
	proc_info_headvars(OrigProcInfo, HeadVars0),
	remove_listof_elements(HeadVars0, 1, UnusedArgs2, IntermodHeadVars),
	proc_info_set_headvars(IntermodHeadVars, ExtraProc0, ExtraProc1),
	proc_info_argmodes(OrigProcInfo, ArgModes0),
	remove_listof_elements(ArgModes0, 1, UnusedArgs2, IntermodArgModes),
	proc_info_set_argmodes(IntermodArgModes, ExtraProc1, ExtraProc),
	pred_info_procedures(ExtraPredInfo0, ExtraProcs0),
	map__set(ExtraProcs0, ProcId, ExtraProc, ExtraProcs),
	pred_info_set_procedures(ExtraProcs, ExtraPredInfo0, ExtraPredInfo),
	module_info_get_predicate_table(!.ModuleInfo, PredTable0),
	predicate_table_insert(ExtraPredInfo, _, PredTable0, PredTable),
	module_info_set_predicate_table(PredTable, !ModuleInfo).

:- pred make_new_pred_info(module_info::in, list(int)::in, import_status::in,
	pred_proc_id::in, pred_info::in, pred_info::out) is det.

make_new_pred_info(ModuleInfo, UnusedArgs, Status, proc(PredId, ProcId),
		!PredInfo) :-
	PredModule = pred_info_module(!.PredInfo),
	Name0 = pred_info_name(!.PredInfo),
	PredOrFunc = pred_info_is_pred_or_func(!.PredInfo),
	pred_info_arg_types(!.PredInfo, Tvars, ExistQVars, ArgTypes0),
	pred_info_get_origin(!.PredInfo, OrigOrigin),
		% create a unique new pred name using the old proc_id
	(
		string__prefix(Name0, "__"),
		\+ string__prefix(Name0, "__LambdaGoal__")
	->
		(
				% fix up special pred names
			OrigOrigin = special_pred(_SpecialId - TypeCtor)
		->
			type_util__type_ctor_module(ModuleInfo,
				TypeCtor, TypeModule),
			type_util__type_ctor_name(ModuleInfo,
				TypeCtor, TypeName),
			type_util__type_ctor_arity(ModuleInfo,
				TypeCtor, TypeArity),
			string__int_to_string(TypeArity, TypeAr),
			mdbcomp__prim_data__sym_name_to_string(TypeModule,
				TypeModuleString0),
			string__replace_all(TypeModuleString0, ".", "__",
				TypeModuleString1),
			string__replace_all(TypeModuleString1, ":", "__",
				TypeModuleString),
			string__append_list([Name0, "_", TypeModuleString,
				"__", TypeName, "_", TypeAr], Name1)
		;
			% The special predicate has already been specialised.
			Name1 = Name0
		)
	;
		Name1 = Name0
	),
	make_pred_name(PredModule, "UnusedArgs", yes(PredOrFunc),
		Name1, unused_args(UnusedArgs), Name2),
	% The mode number is included because we want to
	% avoid the creation of more than one predicate with the same
	% name if more than one mode of a predicate is specialized.
	% Since the names of e.g. deep profiling proc_static structures
	% are derived from the names of predicates, duplicate predicate
	% names lead to duplicate global variable names and hence to
	% link errors.
	proc_id_to_int(ProcId, ProcInt),
	add_sym_name_suffix(Name2, "_" ++ int_to_string(ProcInt), Name),
	Arity = pred_info_orig_arity(!.PredInfo),
	pred_info_typevarset(!.PredInfo, TypeVars),
	remove_listof_elements(ArgTypes0, 1, UnusedArgs, ArgTypes),
	pred_info_context(!.PredInfo, Context),
	pred_info_clauses_info(!.PredInfo, ClausesInfo),
	pred_info_get_markers(!.PredInfo, Markers),
	pred_info_get_goal_type(!.PredInfo, GoalType),
	pred_info_get_class_context(!.PredInfo, ClassContext),
	pred_info_get_aditi_owner(!.PredInfo, Owner),

		% Since this pred_info isn't built until after the
		% polymorphism transformation is complete, we just use
		% dummy maps for the class constraints.
		%
	map__init(EmptyProofs),
	map__init(EmptyConstraintMap),
	Origin = transformed(unused_argument_elimination(UnusedArgs),
		OrigOrigin, PredId),
	pred_info_init(PredModule, Name, Arity, PredOrFunc, Context, Origin,
		Status, GoalType, Markers, ArgTypes, Tvars, ExistQVars,
		ClassContext, EmptyProofs, EmptyConstraintMap, Owner,
		ClausesInfo, !:PredInfo),
	pred_info_set_typevarset(TypeVars, !PredInfo).

	% Replace the goal in the procedure with one to call the given
	% pred_id and proc_id.
:- pred create_call_goal(list(int)::in, pred_id::in, proc_id::in,
	module_name::in, string::in, proc_info::in, proc_info::out) is det.

create_call_goal(UnusedArgs, NewPredId, NewProcId, PredModule, PredName,
		!OldProc) :-
	proc_info_headvars(!.OldProc, HeadVars),
	proc_info_goal(!.OldProc, Goal0),
	Goal0 = _GoalExpr - GoalInfo0,

		% We must use the interface determinism for determining
		% the determinism of the version of the goal with its
		% arguments removed, not the actual determinism of the
		% body is it may be more lax, which will lead to code
		% gen problems.
	proc_info_interface_determinism(!.OldProc, Determinism),
	goal_info_set_determinism(GoalInfo0, Determinism, GoalInfo1),

	proc_info_vartypes(!.OldProc, VarTypes0),
	set__list_to_set(HeadVars, NonLocals),
	map__apply_to_list(HeadVars, VarTypes0, VarTypeList),
	map__from_corresponding_lists(HeadVars, VarTypeList, VarTypes1),
		% the varset should probably be fixed up, but it
		% shouldn't make too much difference
	proc_info_varset(!.OldProc, Varset0),
	remove_listof_elements(HeadVars, 1, UnusedArgs, NewHeadVars),
	GoalExpr = call(NewPredId, NewProcId, NewHeadVars,
		not_builtin, no, qualified(PredModule, PredName)),
	Goal1 = GoalExpr - GoalInfo1,
	implicitly_quantify_goal(NonLocals, _, Goal1, Goal, Varset0, Varset,
		VarTypes1, VarTypes),
	proc_info_set_goal(Goal, !OldProc),
	proc_info_set_varset(Varset, !OldProc),
	proc_info_set_vartypes(VarTypes, !OldProc).

	% Create a pred_info for an imported pred with a pragma unused_args
	% in the .opt file.
:- pred make_imported_unused_args_pred_info(pred_proc_id::in, list(int)::in,
	proc_call_info::in, proc_call_info::out,
	module_info::in, module_info::out) is det.

make_imported_unused_args_pred_info(OptProc, UnusedArgs,
		!ProcCallInfo, !ModuleInfo) :-
	OptProc = proc(PredId, ProcId),
	module_info_pred_proc_info(!.ModuleInfo,
		PredId, ProcId, PredInfo0, ProcInfo0),
	make_new_pred_info(!.ModuleInfo, UnusedArgs, imported(interface),
		OptProc, PredInfo0, NewPredInfo0),
	pred_info_procedures(NewPredInfo0, NewProcs0),

		% Assign the old procedure to a new predicate.
	proc_info_headvars(ProcInfo0, HeadVars0),
	remove_listof_elements(HeadVars0, 1, UnusedArgs, HeadVars),
	proc_info_set_headvars(HeadVars, ProcInfo0, HeadVarsProcInfo),
	proc_info_argmodes(HeadVarsProcInfo, ArgModes0),
	remove_listof_elements(ArgModes0, 1, UnusedArgs, ArgModes),
	proc_info_set_argmodes(ArgModes, ProcInfo0, ProcInfo),
	map__set(NewProcs0, ProcId, ProcInfo, NewProcs),
	pred_info_set_procedures(NewProcs, NewPredInfo0, NewPredInfo),

		% Add the new proc to the pred table.
	module_info_get_predicate_table(!.ModuleInfo, PredTable0),
	predicate_table_insert(NewPredInfo, NewPredId, PredTable0, PredTable1),
	module_info_set_predicate_table(PredTable1, !ModuleInfo),
	PredModule = pred_info_module(NewPredInfo),
	PredName = pred_info_name(NewPredInfo),
	PredSymName = qualified(PredModule, PredName),
		% Add the new proc to the proc_call_info map.
	map__det_insert(!.ProcCallInfo, proc(PredId, ProcId),
		call_info(NewPredId, ProcId, PredSymName, UnusedArgs),
		!:ProcCallInfo).

:- pred remove_listof_elements(list(T)::in, int::in, list(int)::in,
	list(T)::out) is det.

remove_listof_elements(List0, ArgNo, ElemsToRemove, List) :-
	( ElemsToRemove = [] ->
		List = List0
	;
		(
			List0 = [Head | Tail],
			NextArg = ArgNo + 1,
			( list__member(ArgNo, ElemsToRemove) ->
				List = List1
			;
				List = [Head | List1]
			),
			remove_listof_elements(Tail, NextArg, ElemsToRemove,
				List1)
		;
			List0 = [],
			List = List0
		)
	).

:- pred get_unused_arg_nos(var_dep::in, list(prog_var)::in, int::in,
	list(int)::out) is det.

get_unused_arg_nos(_, [], _, []).
get_unused_arg_nos(LocalVars, [HeadVar | HeadVars], ArgNo, UnusedArgs) :-
	NextArg = ArgNo + 1,
	( map__contains(LocalVars, HeadVar) ->
		UnusedArgs = [ArgNo | UnusedArgs1]
	;
		UnusedArgs = UnusedArgs1
	),
	get_unused_arg_nos(LocalVars, HeadVars, NextArg, UnusedArgs1).

	% note - we should probably remove unused variables from the type map.
:- pred fixup_unused_args(var_usage::in, pred_proc_list::in, proc_call_info::in,
	module_info::in, module_info::out, bool::in, io::di, io::uo) is det.

fixup_unused_args(_, [], _, !ModuleInfo, _, !IO).
fixup_unused_args(VarUsage, [PredProc | PredProcs], ProcCallInfo,
		!ModuleInfo, VeryVerbose, !IO) :-
	( VeryVerbose = yes ->
		PredProc = proc(PredId, ProcId),
		io__write_string("% Fixing up `", !IO),
		predicate_name(!.ModuleInfo, PredId, Name),
		predicate_arity(!.ModuleInfo, PredId, Arity),
		proc_id_to_int(ProcId, ProcInt),
		io__write_string(Name, !IO),
		io__write_string("/", !IO),
		io__write_int(Arity, !IO),
		io__write_string("' in mode ", !IO),
		io__write_int(ProcInt, !IO),
		io__write_char('\n', !IO)
	;
		true
	),
	do_fixup_unused_args(VarUsage, PredProc, ProcCallInfo, !ModuleInfo),
	fixup_unused_args(VarUsage, PredProcs, ProcCallInfo, !ModuleInfo,
		VeryVerbose, !IO).

:- pred do_fixup_unused_args(var_usage::in, pred_proc_id::in,
	proc_call_info::in, module_info::in, module_info::out) is det.

do_fixup_unused_args(VarUsage, proc(OldPredId, OldProcId), ProcCallInfo,
		ModuleInfo0, ModuleInfo) :-
	(
			% work out which proc we should be fixing up
		map__search(ProcCallInfo, proc(OldPredId, OldProcId),
			call_info(NewPredId, NewProcId, _, UnusedArgs0))
	->
		UnusedArgs = UnusedArgs0,
		PredId = NewPredId,
		ProcId = NewProcId
	;
		UnusedArgs = [],
		PredId = OldPredId,
		ProcId = OldProcId
	),
	map__lookup(VarUsage, proc(OldPredId, OldProcId), UsageInfos),
	map__keys(UsageInfos, UnusedVars),
	module_info_pred_proc_info(ModuleInfo0, PredId, ProcId, PredInfo0,
		ProcInfo0),
	proc_info_vartypes(ProcInfo0, VarTypes0),
	module_info_preds(ModuleInfo0, Preds0),
	pred_info_procedures(PredInfo0, Procs0),

	proc_info_headvars(ProcInfo0, HeadVars0),
	proc_info_argmodes(ProcInfo0, ArgModes0),
	proc_info_varset(ProcInfo0, Varset0),
	proc_info_goal(ProcInfo0, Goal0),
	remove_listof_elements(HeadVars0, 1, UnusedArgs, HeadVars),
	remove_listof_elements(ArgModes0, 1, UnusedArgs, ArgModes),
	proc_info_set_headvars(HeadVars, ProcInfo0, FixedProc1),
	proc_info_set_argmodes(ArgModes, FixedProc1, FixedProc2),

		% remove unused vars from goal
	fixup_goal(ModuleInfo0, UnusedVars, ProcCallInfo, Changed,
		Goal0, Goal1),
	(
		Changed = yes,
			% if anything has changed, rerun quantification
		set__list_to_set(HeadVars, NonLocals),
		implicitly_quantify_goal(NonLocals, _, Goal1, Goal,
			Varset0, Varset, VarTypes0, VarTypes),
		proc_info_set_goal(Goal, FixedProc2, FixedProc3),
		proc_info_set_varset(Varset, FixedProc3, FixedProc4),
		proc_info_set_vartypes(VarTypes, FixedProc4, FixedProc5)
	;
		Changed = no,
		proc_info_set_vartypes(VarTypes0, FixedProc2, FixedProc5)
	),
	map__set(Procs0, ProcId, FixedProc5, Procs),
	pred_info_set_procedures(Procs, PredInfo0, PredInfo),
	map__set(Preds0, PredId, PredInfo, Preds),
	module_info_set_preds(Preds, ModuleInfo0, ModuleInfo).

	% this is the important bit of the transformation
:- pred fixup_goal(module_info::in, list(prog_var)::in, proc_call_info::in,
	bool::out, hlds_goal::in, hlds_goal::out) is det.

fixup_goal(ModuleInfo, UnusedVars, ProcCallInfo, Changed, Goal0, Goal) :-
	fixup_goal_expr(ModuleInfo, UnusedVars, ProcCallInfo, Changed,
		Goal0, Goal1),
	Goal1 = GoalExpr - GoalInfo0,
	( Changed = yes ->
		fixup_goal_info(UnusedVars, GoalInfo0, GoalInfo)
	;
		GoalInfo = GoalInfo0
	),
	Goal = GoalExpr - GoalInfo.

:- pred fixup_goal_expr(module_info::in, list(prog_var)::in,
	proc_call_info::in, bool::out, hlds_goal::in, hlds_goal::out) is det.

fixup_goal_expr(ModuleInfo, UnusedVars, ProcCallInfo, Changed,
		conj(Goals0) - GoalInfo, conj(Goals) - GoalInfo) :-
	fixup_conjuncts(ModuleInfo, UnusedVars, ProcCallInfo, no, Changed,
		Goals0, Goals).

fixup_goal_expr(ModuleInfo, UnusedVars, ProcCallInfo, Changed,
		par_conj(Goals0) - GoalInfo,
		par_conj(Goals) - GoalInfo) :-
	fixup_conjuncts(ModuleInfo, UnusedVars, ProcCallInfo, no, Changed,
		Goals0, Goals).

fixup_goal_expr(ModuleInfo, UnusedVars, ProcCallInfo, Changed,
		disj(Goals0) - GoalInfo, disj(Goals) - GoalInfo) :-
	fixup_disjuncts(ModuleInfo, UnusedVars, ProcCallInfo, no, Changed,
		Goals0, Goals).

fixup_goal_expr(ModuleInfo, UnusedVars, ProcCallInfo, Changed,
		not(NegGoal0) - GoalInfo, not(NegGoal) - GoalInfo) :-
	fixup_goal(ModuleInfo, UnusedVars, ProcCallInfo, Changed,
		NegGoal0, NegGoal).

fixup_goal_expr(ModuleInfo, UnusedVars, ProcCallInfo, Changed,
		switch(Var, CanFail, Cases0) - GoalInfo,
		switch(Var, CanFail, Cases) - GoalInfo) :-
	fixup_cases(ModuleInfo, UnusedVars, ProcCallInfo, no, Changed,
		Cases0, Cases).

fixup_goal_expr(ModuleInfo, UnusedVars, ProcCallInfo, Changed,
		if_then_else(Vars, Cond0, Then0, Else0) - GoalInfo,
		if_then_else(Vars, Cond, Then, Else) - GoalInfo) :-
	fixup_goal(ModuleInfo, UnusedVars, ProcCallInfo, Changed1,
		Cond0, Cond),
	fixup_goal(ModuleInfo, UnusedVars, ProcCallInfo, Changed2,
		Then0, Then),
	fixup_goal(ModuleInfo, UnusedVars, ProcCallInfo, Changed3,
		Else0, Else),
	bool__or_list([Changed1, Changed2, Changed3], Changed).

fixup_goal_expr(ModuleInfo, UnusedVars, ProcCallInfo, Changed,
		scope(Reason, SubGoal0) - GoalInfo,
		scope(Reason, SubGoal) - GoalInfo) :-
	fixup_goal(ModuleInfo, UnusedVars, ProcCallInfo, Changed,
		SubGoal0, SubGoal).

fixup_goal_expr(_ModuleInfo, _UnusedVars, ProcCallInfo, Changed,
		call(PredId0, ProcId0, ArgVars0, B, C, Name0) - GoalInfo,
		call(PredId, ProcId, ArgVars, B, C, Name) - GoalInfo) :-
	(
		map__search(ProcCallInfo, proc(PredId0, ProcId0),
			call_info(NewPredId, NewProcId, NewName, UnusedArgs))
	->
		Changed = yes,
		remove_listof_elements(ArgVars0, 1, UnusedArgs, ArgVars),
		PredId = NewPredId,
		ProcId = NewProcId,
		Name = NewName
	;
		Changed = no,
		PredId = PredId0,
		ProcId = ProcId0,
		ArgVars = ArgVars0,
		Name = Name0
	).

fixup_goal_expr(ModuleInfo, UnusedVars, _ProcCallInfo, Changed,
		GoalExpr0 - GoalInfo, GoalExpr - GoalInfo) :-
	GoalExpr0 = unify(Var, Rhs, Mode, Unify0, Context),
	( fixup_unify(ModuleInfo, UnusedVars, Changed0, Unify0, Unify) ->
		GoalExpr = unify(Var, Rhs, Mode, Unify, Context),
		Changed = Changed0
	;
		GoalExpr = conj([]),
		Changed = yes
	).

fixup_goal_expr(_ModuleInfo, _UnusedVars, _ProcCallInfo, no,
		GoalExpr - GoalInfo, GoalExpr - GoalInfo) :-
	GoalExpr = generic_call(_, _, _, _).

fixup_goal_expr(_ModuleInfo, _UnusedVars, _ProcCallInfo, no,
		GoalExpr - GoalInfo, GoalExpr - GoalInfo) :-
	GoalExpr = foreign_proc(_, _, _, _, _, _).

fixup_goal_expr(_, _, _, _, shorthand(_) - _, _) :-
	% these should have been expanded out by now
	error("fixup_goal_expr: unexpected shorthand").

	% Remove useless unifications from a list of conjuncts.
:- pred fixup_conjuncts(module_info::in, list(prog_var)::in, proc_call_info::in,
	bool::in, bool::out, hlds_goals::in, hlds_goals::out) is det.

fixup_conjuncts(_, _, _, !Changed, [], []).
fixup_conjuncts(ModuleInfo, UnusedVars, ProcCallInfo, !Changed,
		[Goal0 | Goals0], Goals) :-
	fixup_goal(ModuleInfo, UnusedVars, ProcCallInfo, LocalChanged,
		Goal0, Goal),
	( LocalChanged = yes ->
		!:Changed = yes
	;
		true
	),
		% replacing a goal with conj([]) signals that it is
		%	no longer needed
	( Goal = conj([]) - _ ->
		Goals = Goals1
	;
		Goals = [Goal | Goals1]
	),
	fixup_conjuncts(ModuleInfo, UnusedVars, ProcCallInfo, !Changed,
		Goals0, Goals1).

	% We can't remove unused goals from the list of disjuncts as we do
	% for conjuncts, since that would change the determinism of
	% the goal.
:- pred fixup_disjuncts(module_info::in, list(prog_var)::in, proc_call_info::in,
	bool::in, bool::out, hlds_goals::in, hlds_goals::out) is det.

fixup_disjuncts(_, _, _, !Changed, [], []).
fixup_disjuncts(ModuleInfo, UnusedVars, ProcCallInfo, !Changed,
		[Goal0 | Goals0], [Goal | Goals]) :-
	fixup_goal(ModuleInfo, UnusedVars, ProcCallInfo, LocalChanged,
		Goal0, Goal),
	( LocalChanged = yes ->
		!:Changed = yes
	;
		true
	),
	fixup_disjuncts(ModuleInfo, UnusedVars, ProcCallInfo, !Changed,
		Goals0, Goals).

:- pred fixup_cases(module_info::in, list(prog_var)::in, proc_call_info::in,
	bool::in, bool::out, list(case)::in, list(case)::out) is det.

fixup_cases(_, _, _, !Changed, [], []).
fixup_cases(ModuleInfo, UnusedVars, ProcCallInfo, !Changed,
		[case(ConsId, Goal0) | Cases0],
		[case(ConsId, Goal) | Cases]) :-
	fixup_goal(ModuleInfo, UnusedVars, ProcCallInfo, LocalChanged,
		Goal0, Goal),
	( LocalChanged = yes ->
		!:Changed = yes
	;
		true
	),
	fixup_cases(ModuleInfo, UnusedVars, ProcCallInfo, !Changed,
		Cases0, Cases).

	% fix up a unification, fail if the unification is no
	%	longer needed
:- pred fixup_unify(module_info::in, list(prog_var)::in, bool::out,
	unification::in, unification::out) is semidet.

	% a simple test doesn't have any unused vars to fixup
fixup_unify(_, _UnusedVars, no, simple_test(A, B), simple_test(A, B)).

	% Var1 unused => we don't need the assignment
	% Var2 unused => Var1 unused
fixup_unify(_, UnusedVars, no, assign(Var1, Var2), assign(Var1, Var2)) :-
	\+ list__member(Var1, UnusedVars).

	% LVar unused => we don't need the unification
fixup_unify(_, UnusedVars, no, Unify, Unify) :-
	Unify = construct(LVar, _, _, _, _, _, _),
	\+ list__member(LVar, UnusedVars).

fixup_unify(ModuleInfo, UnusedVars, Changed, Unify, Unify) :-
	Unify =	deconstruct(LVar, _, ArgVars, ArgModes, CanFail, _CanCGC),
	\+ list__member(LVar, UnusedVars),
	(
			% are any of the args unused, if so we need to
			% to fix up the goal_info
		CanFail = cannot_fail,
		check_deconstruct_args(ModuleInfo, UnusedVars, ArgVars,
			ArgModes, Changed, no)
	;
		CanFail = can_fail,
		Changed = no
	).

	% These should be transformed into calls by polymorphism.m.
fixup_unify(_, _, _, complicated_unify(_, _, _), _) :-
		error("unused_args:fixup_goal : complicated unify").

	% Check if any of the arguments of a deconstruction are unused, if
	% so Changed will be yes and quantification will be rerun. Fails if
	% none of the arguments are used. Arguments which further instantiate
	% the deconstructed variable are ignored in this.
:- pred check_deconstruct_args(module_info::in, list(prog_var)::in,
	list(prog_var)::in, list(uni_mode)::in,
	bool::out, bool::in) is semidet.

check_deconstruct_args(ModuleInfo, UnusedVars, Args, Modes, Changed, Used) :-
	(
		Args = [ArgVar | ArgVars],
		Modes = [ArgMode | ArgModes]
	->
		(
			ArgMode = ((Inst1 - Inst2) -> _),
			mode_is_output(ModuleInfo, (Inst1 -> Inst2)),
			list__member(ArgVar, UnusedVars)
		->
			check_deconstruct_args(ModuleInfo, UnusedVars,
				ArgVars, ArgModes, _, Used),
			Changed = yes
		;
			check_deconstruct_args(ModuleInfo, UnusedVars,
				ArgVars, ArgModes, Changed, yes)
		)
	;
		Args = [],
		Modes = []
	->
		Changed = no,
		Used = yes
	;
		error("check_deconstruct_args - invalid call")
	).

	% Remove unused vars from the instmap_delta, quantification fixes
	%	up the rest.
:- pred fixup_goal_info(list(prog_var)::in, hlds_goal_info::in,
	hlds_goal_info::out) is det.

fixup_goal_info(UnusedVars, !GoalInfo) :-
	goal_info_get_instmap_delta(!.GoalInfo, InstMap0),
	instmap_delta_delete_vars(InstMap0, UnusedVars, InstMap),
	goal_info_set_instmap_delta(!.GoalInfo, InstMap, !:GoalInfo).

%-------------------------------------------------------------------------------

		% Except for type_infos, all args that are unused
		% in one mode of a predicate should be unused in all of the
		% modes of a predicate, so we only need to put out one warning
		% for each predicate.
:- pred output_warnings_and_pragmas(module_info::in, unused_arg_info::in,
	maybe(io__output_stream)::in, bool::in, pred_proc_list::in,
	set(pred_id)::in, io::di, io::uo) is det.

output_warnings_and_pragmas(_, _, _, _, [], _, !IO).
output_warnings_and_pragmas(ModuleInfo, UnusedArgInfo, WriteOptPragmas,
		DoWarn, [proc(PredId, ProcId) | Rest], !.WarnedPredIds, !IO) :-
	( map__search(UnusedArgInfo, proc(PredId, ProcId), UnusedArgs) ->
		module_info_pred_info(ModuleInfo, PredId, PredInfo),
		(
			Name = pred_info_name(PredInfo),
			\+ pred_info_is_imported(PredInfo),
			\+ pred_info_import_status(PredInfo, opt_imported),
				% Don't warn about builtins
				% that have unused arguments.
			\+ pred_info_is_builtin(PredInfo),
			\+ is_unify_or_compare_pred(PredInfo),
				% Don't warn about stubs for procedures
				% with no clauses -- in that case,
				% we *expect* that none of the arguments
				% will be used,
			pred_info_get_markers(PredInfo, Markers),
			\+ check_marker(Markers, stub),
				% Don't warn about lambda expressions
				% not using arguments. (The warning
				% message for these doesn't contain
				% context, so it's useless)
			\+ string__sub_string_search(Name,
				"__LambdaGoal__", _),
			\+ (
				% don't warn for a specialized version
				string__sub_string_search(Name, "__ho",
					Position),
				string__length(Name, Length),
				IdLen = Length - Position - 4,
				string__right(Name, IdLen, Id),
				string__to_int(Id, _)
			),
			% XXX We don't currently generate pragmas
			% for the automatically generated class
			% instance methods because the compiler
			% aborts when trying to read them back in
			% from the `.opt' files. 
			\+ check_marker(Markers, class_instance_method),
			\+ check_marker(Markers, named_class_instance_method)	

		->
			write_unused_args_to_opt_file(WriteOptPragmas,
				PredInfo, ProcId, UnusedArgs, !IO),
			maybe_warn_unused_args(DoWarn, ModuleInfo, PredInfo,
				PredId, ProcId, UnusedArgs, !WarnedPredIds,
				!IO)
		;
			true
		)
	;
		true
	),
	output_warnings_and_pragmas(ModuleInfo, UnusedArgInfo, WriteOptPragmas,
		DoWarn, Rest, !.WarnedPredIds, !IO).

:- pred write_unused_args_to_opt_file(maybe(io__output_stream)::in,
	pred_info::in, proc_id::in, list(int)::in, io::di, io::uo) is det.

write_unused_args_to_opt_file(no, _, _, _, !IO).
write_unused_args_to_opt_file(yes(OptStream), PredInfo, ProcId, UnusedArgs,
		!IO) :-
	(
		( pred_info_is_exported(PredInfo)
		; pred_info_is_opt_exported(PredInfo)
		; pred_info_is_exported_to_submodules(PredInfo)
		),
		UnusedArgs \= []
	->
		Module = pred_info_module(PredInfo),
		Name = pred_info_name(PredInfo),
		Arity = pred_info_orig_arity(PredInfo),
		PredOrFunc = pred_info_is_pred_or_func(PredInfo),
		io__set_output_stream(OptStream, OldOutput, !IO),
		proc_id_to_int(ProcId, ModeNum),
		mercury_output_pragma_unused_args(PredOrFunc,
			qualified(Module, Name), Arity, ModeNum, UnusedArgs,
			!IO),
		io__set_output_stream(OldOutput, _, !IO)
	;
		true
	).

:- pred maybe_warn_unused_args(bool::in, module_info::in, pred_info::in,
	pred_id::in, proc_id::in, list(int)::in,
	set(pred_id)::in, set(pred_id)::out, io::di, io::uo) is det.

maybe_warn_unused_args(no, _, _, _, _, _, !WarnedPredIds, !IO).
maybe_warn_unused_args(yes, _ModuleInfo, PredInfo, PredId, ProcId,
		UnusedArgs0, !WarnedPredIds, !IO) :-
	( set__member(PredId, !.WarnedPredIds) ->
		true
	;
		set__insert(!.WarnedPredIds, PredId, !:WarnedPredIds),
		pred_info_procedures(PredInfo, Procs),
		map__lookup(Procs, ProcId, Proc),
		proc_info_headvars(Proc, HeadVars),
		list__length(HeadVars, NumHeadVars),

		% Strip off the extra type_info arguments
		% inserted at the front by polymorphism.m
		NumToDrop = NumHeadVars - pred_info_orig_arity(PredInfo),
		adjust_unused_args(NumToDrop, UnusedArgs0, UnusedArgs),
		(
			UnusedArgs = [_ | _],
			report_unused_args(PredInfo, UnusedArgs, !IO)
		;
			UnusedArgs = []
		)
	).

	% Warn about unused arguments in a predicate. Only arguments unused
	% in every mode of a predicate are warned about. The warning is
	% suppressed for type_infos.
:- pred report_unused_args(pred_info::in, list(int)::in,
	io::di, io::uo) is det.

report_unused_args(PredInfo, UnusedArgs) -->
	{ list__length(UnusedArgs, NumArgs) },
	{ pred_info_context(PredInfo, Context) },
	{ PredOrFunc = pred_info_is_pred_or_func(PredInfo) },
	{ Module = pred_info_module(PredInfo) },
	{ Name = pred_info_name(PredInfo) },
	{ Arity = pred_info_orig_arity(PredInfo) },
	prog_out__write_context(Context),
	io__write_string("In "),
	write_pred_or_func(PredOrFunc),
	io__write_string(" `"),
	prog_out__write_sym_name(Module),
	io__write_string("."),
	io__write_string(Name),
	io__write_string("/"),
	io__write_int(Arity),
	io__write_string("':\n"),
	prog_out__write_context(Context),
	io__write_string("  warning: "),
	(
		{ NumArgs = 1 }
	->
		io__write_string("argument "),
		output_arg_list(UnusedArgs),
		io__write_string(" is unused.\n")
	;
		io__write_string("arguments "),
		output_arg_list(UnusedArgs),
		io__write_string(" are unused.\n")
	).

	% adjust warning message for the presence of type_infos.
:- pred adjust_unused_args(int::in, list(int)::in, list(int)::out) is det.

adjust_unused_args(_, [], []).
adjust_unused_args(NumToDrop, [UnusedArgNo | UnusedArgNos0], AdjUnusedArgs) :-
	NewArg = UnusedArgNo - NumToDrop,
	( NewArg < 1 ->
		AdjUnusedArgs = AdjUnusedArgs1
	;
		AdjUnusedArgs = [NewArg | AdjUnusedArgs1]
	),
	adjust_unused_args(NumToDrop, UnusedArgNos0, AdjUnusedArgs1).

:- pred output_arg_list(list(int)::in, io::di, io::uo) is det.

output_arg_list([], !IO) :-
	error("output_list_int called with empty list").
output_arg_list([Arg | Rest], !IO) :-
	io__write_int(Arg, !IO),
	(
		Rest = []
	;
		Rest = [_ | _],
		output_arg_list_2(Rest, !IO)
	).

:- pred output_arg_list_2(list(int)::in, io::di, io::uo) is det.

output_arg_list_2(Args, !IO) :-
	(
		Args = [First, Second | Rest]
	->
		io__write_string(", ", !IO),
		io__write_int(First, !IO),
		output_arg_list_2([Second | Rest], !IO)
	;
		Args = [Last]
	->
		io__write_string(" and ", !IO),
		io__write_int(Last, !IO)
	;
		error("output_arg_list_2 called with empty list")
	).
