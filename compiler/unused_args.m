%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
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

:- module unused_args.

%-------------------------------------------------------------------------------
:- interface.

:- import_module hlds_module.
:- import_module io.

:- pred unused_args__process_module(module_info::in, module_info::out,
				io__state::di, io__state::uo) is det.

%-------------------------------------------------------------------------------
:- implementation.

:- import_module hlds_pred, hlds_goal, hlds_data, hlds_out, type_util, instmap.
:- import_module code_util, globals, make_hlds, mercury_to_mercury, mode_util.
:- import_module options, prog_data, prog_out, quantification, special_pred.
:- import_module passes_aux, inst_match.

:- import_module bool, char, int, list, map, require.
:- import_module set, std_util, string, term, varset. 

		% Information about the dependencies of a variable
		% that is not known to be used.
:- type usage_info --->
		unused(set(var), set(arg)).

	% A collection of variable usages for each procedure.
:- type var_usage == map(pred_proc_id, var_dep).

	% arguments are stored as their variable id, not their index
	%	in the argument vector
:- type arg == pair(pred_proc_id, var). 

		% Contains dependency information for the variables
		% in a procedure that are not yet known to be used.
:- type var_dep == map(var, usage_info).

:- type warning_info --->
		warning_info(term__context, string, int, list(int)).
			% context, pred name, arity, list of args to warn 


unused_args__process_module(ModuleInfo0, ModuleInfo) -->
	globals__io_lookup_bool_option(very_verbose, VeryVerbose),
	{ init_var_usage(ModuleInfo0, VarUsage0, PredProcs, OptProcs) },
	%maybe_write_string(VeryVerbose, "% Finished initialisation.\n"),

	{ unused_args_pass(PredProcs, VarUsage0, VarUsage) },
	%maybe_write_string(VeryVerbose, "% Finished analysis.\n"),

	{ map__init(UnusedArgInfo0) },
	{ get_unused_arg_info(ModuleInfo0, PredProcs, VarUsage,
					UnusedArgInfo0, UnusedArgInfo) },

	{ map__keys(UnusedArgInfo, PredProcsToFix) },

	globals__io_lookup_bool_option(make_optimization_interface, MakeOpt),
	( { MakeOpt = yes } ->
		{ module_info_name(ModuleInfo0, ModuleName) },
		{ string__append(ModuleName, ".opt", OptFileName) },
		io__open_append(OptFileName, OptFileRes),
		( { OptFileRes = ok(OptFile) } ->
			{ MaybeOptFile = yes(OptFile) }
		;
			io__write_strings(["Cannot open `",
				OptFileName, "' for output\n"]),
			io__set_exit_status(1),
			{ MaybeOptFile = no }
		)
	;
		{ MaybeOptFile = no }
	),
	globals__io_lookup_bool_option(warn_unused_args, DoWarn),
	( { DoWarn = yes ; MakeOpt = yes } ->
		{ set__init(WarnedPredIds0) },
		output_warnings_and_pragmas(ModuleInfo0, UnusedArgInfo,
			MaybeOptFile, DoWarn, PredProcsToFix, WarnedPredIds0)
	;
		[]
	),
	( { MaybeOptFile = yes(OptFile2) } ->
		io__close_output(OptFile2)
	;
		[]
	),
	globals__io_lookup_bool_option(optimize_unused_args, DoFixup), 
	(
		{ DoFixup = yes }
	->
		{ map__init(ProcCallInfo0) },
		{ create_new_preds(PredProcsToFix, UnusedArgInfo,
				ProcCallInfo0, ProcCallInfo1,
				ModuleInfo0, ModuleInfo1) }, 
		{ make_imported_unused_args_pred_infos(OptProcs,
				ProcCallInfo1, ProcCallInfo,
				ModuleInfo1, ModuleInfo2) },
		% maybe_write_string(VeryVerbose, "% Finished new preds.\n"),
		fixup_unused_args(VarUsage, PredProcs, ProcCallInfo,
				ModuleInfo2, ModuleInfo3, VeryVerbose),
		% maybe_write_string(VeryVerbose, "% Fixed up goals.\n"),
		{ map__is_empty(ProcCallInfo) ->
			ModuleInfo = ModuleInfo3
		;
			% The dependencies have changed, so the dependency
			% graph needs rebuilding.
			module_info_clobber_dependency_info(ModuleInfo3,
				ModuleInfo)
		}
	;
		{ ModuleInfo = ModuleInfo0 }
	).
	  

%-------------------------------------------------------------------------------
	% Initialisation section
	
	% init_var_usage/4 -  set initial status of all args of local
 	%	procs by examining the module_info.
	% PredProcList is the list of procedures to do the fixpoint
	% iteration over.
	% OptPredProcList is a list of procedures for which we got
	% unused argument information from .opt files.
:- pred init_var_usage(module_info::in, var_usage::out,
		pred_proc_list::out, pred_proc_list::out) is det.

init_var_usage(ModuleInfo, VarUsage, PredProcList, OptPredProcList) :-
	map__init(VarUsage0),
	module_info_predids(ModuleInfo, PredIds),
	module_info_preds(ModuleInfo, PredTable),
	module_info_unused_arg_info(ModuleInfo, UnusedArgInfo),
	setup_local_var_usage(ModuleInfo, PredTable, PredIds, UnusedArgInfo,
		VarUsage0, VarUsage, [], PredProcList, [], OptPredProcList).



	% setup args for the whole module.	
:- pred setup_local_var_usage(module_info::in, pred_table::in,
	list(pred_id)::in, unused_arg_info::in, var_usage::in,
	var_usage::out, pred_proc_list::in, pred_proc_list::out,
	pred_proc_list::in,  pred_proc_list::out) is det.

setup_local_var_usage(_, _, [], _, VarUsage, VarUsage,
		PredProcs, PredProcs, OptProcs, OptProcs).
setup_local_var_usage(ModuleInfo, PredTable, [PredId | PredIds], UnusedArgInfo,
		VarUsage0, VarUsage, PredProcList0, PredProcList,
		OptProcList0, OptProcList) :-
	map__lookup(PredTable, PredId, PredInfo),
		% The builtins use all their arguments.
	( code_util__predinfo_is_builtin(PredInfo) ->
		VarUsage1 = VarUsage0,
		setup_local_var_usage(ModuleInfo, PredTable, PredIds,
			UnusedArgInfo, VarUsage1, VarUsage, PredProcList0,
			PredProcList, OptProcList0, OptProcList)
	;
		pred_info_procids(PredInfo, ProcIds),
		setup_pred_args(ModuleInfo, PredId, ProcIds, UnusedArgInfo,
			VarUsage0, VarUsage1, PredProcList0, PredProcList1,
			OptProcList0, OptProcList1),
		setup_local_var_usage(ModuleInfo, PredTable, PredIds,
			UnusedArgInfo, VarUsage1, VarUsage, PredProcList1,
			PredProcList, OptProcList1, OptProcList)
	).


	% setup args for a predicate
:- pred setup_pred_args(module_info::in, pred_id::in, list(proc_id)::in,
			unused_arg_info::in, var_usage::in, var_usage::out,
			pred_proc_list::in, pred_proc_list::out,
			pred_proc_list::in, pred_proc_list::out) is det.

setup_pred_args(_, _, [], _, VarUsage, VarUsage,
		PredProcs, PredProcs, OptProcs, OptProcs).
setup_pred_args(ModuleInfo, PredId, [ProcId | Rest], UnusedArgInfo, VarUsage0,
		VarUsage, PredProcs0, PredProcs, OptProcs0, OptProcs) :-
	module_info_pred_proc_info(ModuleInfo, PredId, ProcId,
		PredInfo, ProcInfo),
	map__init(VarDep0),
	Proc = proc(PredId, ProcId),
	(
		% Get the unused argument info from the .opt files.
		% Don't use the .opt file info when we have the clauses
		% (opt_imported preds) since we may be able to do better with
		% the information in this module.
		pred_info_is_imported(PredInfo)
	->
		( map__search(UnusedArgInfo, Proc, UnusedArgs) ->
			proc_info_headvars(ProcInfo, HeadVars),
			list__map(list__index1_det(HeadVars),
				UnusedArgs, UnusedVars),
			initialise_vardep(VarDep0, UnusedVars, VarDep),
			map__set(VarUsage0, proc(PredId, ProcId),
				VarDep, VarUsage1),
			OptProcs1 = [proc(PredId, ProcId) | OptProcs0]
		;
			VarUsage1 = VarUsage0,
			OptProcs1 = OptProcs0
		),
		PredProcs1 = PredProcs0
	;
		pred_info_is_pseudo_imported(PredInfo),
		hlds_pred__in_in_unification_proc_id(ProcId)
	->
		PredProcs1 = PredProcs0,
		OptProcs1 = OptProcs0,
		VarUsage1 = VarUsage0
	;
		proc_info_argmodes(ProcInfo, ArgModes),
		proc_info_headvars(ProcInfo, HeadVars),
		proc_info_vartypes(ProcInfo, VarTypes),
		map__keys(VarTypes, Vars),
		initialise_vardep(VarDep0, Vars, VarDep1),
		setup_output_args(ModuleInfo, HeadVars,
			ArgModes, VarDep1, VarDep2),
		proc_info_goal(ProcInfo, Goal - _),
		traverse_goal(ModuleInfo, Goal, VarDep2, VarDep),
		map__set(VarUsage0, proc(PredId, ProcId), VarDep, VarUsage1),
		PredProcs1 = [proc(PredId, ProcId) | PredProcs0],
		OptProcs1 = OptProcs0
	),
	setup_pred_args(ModuleInfo, PredId, Rest, UnusedArgInfo, VarUsage1,
		VarUsage, PredProcs1, PredProcs, OptProcs1, OptProcs).


:- pred initialise_vardep(var_dep::in, list(var)::in, var_dep::out) is det.

initialise_vardep(VarDep, [], VarDep).
initialise_vardep(VarDep0, [Var | Vars], VarDep) :-
	set__init(VDep),
	set__init(Args),
	map__set(VarDep0, Var, unused(VDep, Args), VarDep1),
	initialise_vardep(VarDep1, Vars, VarDep).

%-------------------------------------------------------------------------------
	% Predicates for manipulating the var_usage and var_dep structures.

	% Get output arguments for a procedure given the headvars and the
	% argument modes.
:- pred setup_output_args(module_info::in, list(var)::in, list(mode)::in,
			var_dep::in, var_dep::out) is det.

setup_output_args(ModuleInfo, HVars, ArgModes, VarDep0, VarDep) :-
	(
		HVars = [Var | Vars], ArgModes = [Mode | Modes]
	->
		(
			% Any argument which has its instantiatedness
			% changed by the predicate is used.
			mode_get_insts(ModuleInfo, Mode, Inst1, Inst2),
			\+ inst_matches_binding(Inst1, Inst2, ModuleInfo)
		->
			set_var_used(VarDep0, Var, VarDep1)		
		;
			VarDep1 = VarDep0	
		),
		setup_output_args(ModuleInfo, Vars, Modes, VarDep1, VarDep)
	;
		HVars = [], ArgModes = []
	->
		VarDep = VarDep0	
	;
		error("setup_output_args: invalid call")
	).


	% searches for the dependencies of a variable, succeeds if the variable
	%	is definitely used
:- pred var_is_used(pred_proc_id::in, var::in, var_usage::in) is semidet.

var_is_used(PredProc, Var, VarUsage) :-
	\+ (
		map__search(VarUsage, PredProc, UsageInfos),
		map__contains(UsageInfos, Var)
	).

:- pred local_var_is_used(var_dep::in, var::in) is semidet.

local_var_is_used(VarDep, Var) :-
	\+ map__contains(VarDep, Var).

		% add a list of aliases for a variable
:- pred add_aliases(var_dep::in, var::in, list(var)::in, var_dep::out) is det.

add_aliases(UseInf0, Var, Aliases, UseInf) :-
	(
		map__search(UseInf0, Var, VarInf0)
	->
		VarInf0 = unused(VarDep0, ArgDep),
		set__insert_list(VarDep0, Aliases, VarDep),
		VarInf = unused(VarDep, ArgDep),
		map__det_update(UseInf0, Var, VarInf, UseInf)
	;
		UseInf = UseInf0
	).

:- pred set_list_vars_used(var_dep::in, list(var)::in, var_dep::out) is det.

set_list_vars_used(UseInfo0, Vars, UseInfo) :-
	map__delete_list(UseInfo0, Vars, UseInfo).

:- pred set_var_used(var_dep::in, var::in, var_dep::out) is det.

set_var_used(UseInfo0, Var, UseInfo) :-
	map__delete(UseInfo0, Var, UseInfo).


:- pred lookup_local_var(var_dep::in, var::in, usage_info::out) is semidet.

lookup_local_var(VarDep, Var, UsageInfo) :-
	map__search(VarDep, Var, UsageInfo).


%-------------------------------------------------------------------------------
	% Traversal of goal structure, building up dependencies for all
	% variables. 

:- pred traverse_goal(module_info::in, hlds_goal_expr::in,
				var_dep::in, var_dep::out) is det.

% handle conjunction
traverse_goal(ModuleInfo, conj(Goals), UseInf0, UseInf) :-
	traverse_list_of_goals(ModuleInfo, Goals, UseInf0, UseInf).

% handle disjunction
traverse_goal(ModuleInfo, disj(Goals, _), UseInf0, UseInf) :-
	traverse_list_of_goals(ModuleInfo, Goals, UseInf0, UseInf).

% handle switch
traverse_goal(ModuleInfo, switch(Var, _, Cases, _), UseInf0, UseInf) :-
	set_var_used(UseInf0, Var, UseInf1),
	list_case_to_list_goal(Cases, Goals),
	traverse_list_of_goals(ModuleInfo, Goals, UseInf1, UseInf).

% handle predicate call
traverse_goal(ModuleInfo, call(PredId, ProcId, Args, _, _, _),
						UseInf0, UseInf) :-
	module_info_pred_proc_info(ModuleInfo, PredId, ProcId, _Pred, Proc),
	proc_info_headvars(Proc, HeadVars),
	add_pred_call_arg_dep(proc(PredId, ProcId), Args, HeadVars,
		UseInf0, UseInf).

% handle if then else
traverse_goal(ModuleInfo, if_then_else(_, Cond - _, Then - _, Else - _, _),
			UseInf0, UseInf) :-
	traverse_goal(ModuleInfo, Cond, UseInf0, UseInf1),
	traverse_goal(ModuleInfo, Then, UseInf1, UseInf2),
	traverse_goal(ModuleInfo, Else, UseInf2, UseInf).

% handle negation
traverse_goal(ModuleInfo, not(Goal - _), UseInf0, UseInf) :-
	traverse_goal(ModuleInfo, Goal, UseInf0, UseInf).

% handle quantification
traverse_goal(ModuleInfo, some(_,  Goal - _), UseInf0, UseInf) :-
	traverse_goal(ModuleInfo, Goal, UseInf0, UseInf).


% we assume that higher-order predicate calls use all variables involved
traverse_goal(_, higher_order_call(PredVar,Args,_,_,_), UseInf0, UseInf) :-
	set_list_vars_used(UseInf0, [PredVar|Args], UseInf).

% handle pragma(c_code, ...) - pragma_c_code uses all its args
traverse_goal(_, pragma_c_code(_, _, _, _, Args, _, _, _), UseInf0, UseInf) :-
	set_list_vars_used(UseInf0, Args, UseInf).

% cases to handle all the different types of unification
traverse_goal(_, unify(_, _, _, simple_test(Var1, Var2),_), UseInf0, UseInf)
		:-
	set_var_used(UseInf0, Var1, UseInf1),
	set_var_used(UseInf1, Var2, UseInf).
		
traverse_goal(_, unify(_, _, _, assign(Var1, Var2), _), UseInf0, UseInf) :-
	(
		map__contains(UseInf0, Var1)
	->
		add_aliases(UseInf0, Var2, [Var1], UseInf)
	;
		% if Var1 used to instantiate an output argument, Var2 used
		set_var_used(UseInf0, Var2, UseInf)
	).

traverse_goal(ModuleInfo,
		unify(Var1, _, _, deconstruct(_, _, Args, Modes, CanFail), _),
		UseInf0, UseInf) :-
	(
		CanFail = can_fail	
	->
		% a deconstruction that can_fail uses its left arg
		set_var_used(UseInf0, Var1, UseInf)
	;
		get_instantiating_variables(ModuleInfo, Args, Modes, InputVars),
		list__delete_elems(Args, InputVars, OutputVars),
			% The deconstructed variable is used if any of the
			% variables, that the deconstruction binds are used.
		add_aliases(UseInf0, Var1, OutputVars, UseInf1),
			% Treat a deconstruction that further instantiates its
			% left arg as a partial construction.
		add_construction_aliases(UseInf1, Var1, InputVars, UseInf)	
	).

traverse_goal(_, unify(Var1, _, _, construct(_, _, Args, _), _),
					UseInf0, UseInf) :-
	(
		map__contains(UseInf0, Var1)
	->
		add_construction_aliases(UseInf0, Var1, Args, UseInf)
	;
		set_list_vars_used(UseInf0, Args, UseInf)
	).
	
	% These should be transformed into calls by polymorphism.m.
traverse_goal(_, unify(Var, Rhs, _, complicated_unify(_, _), _),
		UseInf0, UseInf) :-
    	% This is here to cover the case where unused arguments is called 
	% with --error-check-only and polymorphism has not been run.
	% Complicated unifications should only be var-var.
	( Rhs = var(RhsVar) ->
		set_var_used(UseInf0, RhsVar, UseInf1),
		set_var_used(UseInf1, Var, UseInf)
	;
		error("complicated unifications should only be var-var")
	).

	% add PredProc - HeadVar as an alias for the same element of Args.
:- pred add_pred_call_arg_dep(pred_proc_id::in, list(var)::in, list(var)::in,
					var_dep::in, var_dep::out) is det.

add_pred_call_arg_dep(PredProc, LocalArguments, HeadVarIds,
					UseInf0, UseInf) :-
	(
		LocalArguments = [Arg | Args], HeadVarIds = [HeadVar | HeadVars]
	->
		add_arg_dep(UseInf0, Arg, PredProc, HeadVar, UseInf1),
		add_pred_call_arg_dep(PredProc, Args, HeadVars,
							UseInf1, UseInf)
	;
		LocalArguments = [], HeadVarIds = []
	->
		UseInf = UseInf0
	;
		error("add_pred_call_arg_dep: invalid call")
	).
		

:- pred add_arg_dep(var_dep::in, var::in, pred_proc_id::in,
					var::in, var_dep::out) is det.

add_arg_dep(UseInf0, Var, PredProc, Arg, UseInf) :-
	(
		lookup_local_var(UseInf0, Var, VarUsage0)
	->
		VarUsage0 = unused(VarDep, ArgDep0),
		set__insert(ArgDep0, PredProc - Arg, ArgDep),
		map__det_update(UseInf0, Var, unused(VarDep, ArgDep), UseInf)
	;
		UseInf = UseInf0
	).
			
	% Returns variables which further instantiate a deconstructed variable.
:- pred get_instantiating_variables(module_info::in, list(var)::in,
				list(uni_mode)::in, list(var)::out) is det.

get_instantiating_variables(ModuleInfo, ArgVars, ArgModes, InstVars) :-
	(
		ArgVars = [Var | Vars], ArgModes = [Mode | Modes]
	->
		Mode = ((Inst1 - _) -> (Inst2 - _)),
		(
			inst_matches_binding(Inst1, Inst2, ModuleInfo)
		->
			InstVars = InstVars1
		;
			InstVars = [Var | InstVars1]
		),
		get_instantiating_variables(ModuleInfo, Vars, Modes, InstVars1)
	;
		( ArgVars = [], ArgModes = [] )
	->
		InstVars = []
	;
		error("get_instantiating_variables - invalid call")
	).

		% add Alias as an alias for all of Vars
:- pred add_construction_aliases(var_dep::in, var::in, list(var)::in,
						var_dep::out) is det.

add_construction_aliases(UseInf, _, [], UseInf).
add_construction_aliases(UseInf0, Alias, [Var | Vars], UseInf) :-
	(
		lookup_local_var(UseInf0, Var, VarInf)
	->
		VarInf = unused(VarDep0, ArgDep),
		set__insert(VarDep0, Alias, VarDep), 
		map__set(UseInf0, Var, unused(VarDep, ArgDep), UseInf1)
	;
		UseInf1 = UseInf0
	),
	add_construction_aliases(UseInf1, Alias, Vars, UseInf).


:- pred list_case_to_list_goal(list(case)::in, list(hlds_goal)::out) is det.

list_case_to_list_goal([], []).
list_case_to_list_goal([case(_, Goal) | Cases], [Goal | Goals]) :-
	list_case_to_list_goal(Cases, Goals).


:- pred traverse_list_of_goals(module_info::in, list(hlds_goal)::in,
					var_dep::in, var_dep::out) is det.

traverse_list_of_goals(_, [], UseInf, UseInf).
traverse_list_of_goals(ModuleInfo, [Goal - _ | Goals], UseInf0, UseInf) :-
	traverse_goal(ModuleInfo, Goal, UseInf0, UseInf1),
	traverse_list_of_goals(ModuleInfo, Goals, UseInf1, UseInf).  


%-------------------------------------------------------------------------------
	% Analysis section - do the fixpoint iteration.

	% Do a full iteration, check if anything changed, if so, repeat.
:- pred unused_args_pass(pred_proc_list::in, var_usage::in,var_usage::out)
	is det.

unused_args_pass(LocalPredProcs, VarUsage0, VarUsage) :-
	unused_args_single_pass(LocalPredProcs, no, Changed,
						VarUsage0, VarUsage1),
	(
		Changed = yes
	->
		unused_args_pass(LocalPredProcs, VarUsage1, VarUsage)
	;
		VarUsage = VarUsage1
	).


	% check over all the procedures in a module	
:- pred unused_args_single_pass(pred_proc_list::in, bool::in, bool::out,
				var_usage::in, var_usage::out) is det.

unused_args_single_pass([], Changed, Changed, VarUsage, VarUsage).
unused_args_single_pass([PredProc | Rest], Changed0, Changed,
		VarUsage0, VarUsage) :-
	unused_args_check_proc(PredProc, Changed0, Changed1,
							VarUsage0, VarUsage1),
	unused_args_single_pass(Rest, Changed1, Changed, VarUsage1, VarUsage).


	% check a single procedure
:- pred unused_args_check_proc(pred_proc_id::in, bool::in, bool::out,
				var_usage::in, var_usage::out) is det.

unused_args_check_proc(PredProcId, Changed0, Changed, VarUsage0, VarUsage) :-
	map__lookup(VarUsage0, PredProcId, LocalUsages0),
	map__keys(LocalUsages0, Vars),
	unused_args_check_all_vars(VarUsage0, no, LocalChanged, Vars,
						LocalUsages0, LocalUsages),
	(
		LocalChanged = yes
	->
		map__det_update(VarUsage0, PredProcId, LocalUsages, VarUsage),
		Changed = yes
	;
		VarUsage = VarUsage0,
		Changed = Changed0	
	).



	% check each var of a procedure in turn 
:- pred unused_args_check_all_vars(var_usage::in, bool::in, bool::out,
			list(var)::in, var_dep::in, var_dep::out) is det.

unused_args_check_all_vars(_, Changed, Changed, [], LocalVars, LocalVars). 
unused_args_check_all_vars(VarUsage, Changed0, Changed, [Var| Vars],
						LocalVars0, LocalVars) :-
	(
		lookup_local_var(LocalVars0, Var, Usage)
	->
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
				local_var_is_used(LocalVars0, Var2)
			)
		->
			% set the current variable to used
			set_var_used(LocalVars0, Var, LocalVars1),
			Changed1 = yes
		;
			Changed1 = Changed0,
			LocalVars1 = LocalVars0	
		)	
	;
		LocalVars1 = LocalVars0,
		Changed1 = Changed0
	),
	unused_args_check_all_vars(VarUsage, Changed1, Changed,
						Vars, LocalVars1, LocalVars).
	


:- pred get_unused_arg_info(module_info::in, pred_proc_list::in, var_usage::in,
			unused_arg_info::in, unused_arg_info::out) is det.

get_unused_arg_info(_, [], _, UnusedArgInfo, UnusedArgInfo).
get_unused_arg_info(ModuleInfo, [PredProc | PredProcs], VarUsage,
					UnusedArgInfo0, UnusedArgInfo) :-
	PredProc = proc(PredId, ProcId), 
	map__lookup(VarUsage, PredProc, LocalVarUsage),

	module_info_preds(ModuleInfo, Preds),
	map__lookup(Preds, PredId, PredInfo),
	pred_info_procedures(PredInfo, Procs),
	map__lookup(Procs, ProcId, ProcInfo),
	proc_info_headvars(ProcInfo, HeadVars),
	get_unused_arg_nos(LocalVarUsage, HeadVars, 1, UnusedArgs),
	map__det_insert(UnusedArgInfo0, PredProc, UnusedArgs, UnusedArgInfo1),
	get_unused_arg_info(ModuleInfo, PredProcs, VarUsage,
					UnusedArgInfo1, UnusedArgInfo).


%-------------------------------------------------------------------------------
	% Fix up the module

	% information about predicates which have new predicates
	% created for the optimized version
:- type proc_call_info == map(pred_proc_id, new_proc_info). 

	% new pred_id, proc_id, name, and the indices in the argument
	% vector of the arguments that have been removed.
:- type new_proc_info --->
		call_info(pred_id, proc_id, sym_name, list(int)).


		% Create a new predicate for each procedure which has unused
		% arguments. There are two reasons why we can't throw away
		% the old procedure for non-exported predicates. One is
		% higher-order terms - we can't remove arguments from them
		% without changing their type, so they need the old calling
		% interface. 
		% The other is that the next proc_id for a predicate is
		% chosen based on the length of the list of proc_ids.
:- pred create_new_preds(pred_proc_list::in, unused_arg_info::in,
		proc_call_info::in, proc_call_info::out,
		module_info::in, module_info::out) is det.

create_new_preds([], _UnusedArgInfo, ProcCallInfo, ProcCallInfo, Mod, Mod).
create_new_preds([PredProc | PredProcs], UnusedArgInfo,
		ProcCallInfo0, ProcCallInfo, ModuleInfo0, ModuleInfo) :- 
	create_new_pred(PredProc, UnusedArgInfo, ProcCallInfo0, ProcCallInfo1,
			ModuleInfo0, ModuleInfo1),
	create_new_preds(PredProcs, UnusedArgInfo, ProcCallInfo1, ProcCallInfo,
			ModuleInfo1, ModuleInfo).

:- pred create_new_pred(pred_proc_id::in, unused_arg_info::in,
		proc_call_info::in, proc_call_info::out,
		module_info::in, module_info::out) is det.

create_new_pred(proc(PredId, ProcId), UnusedArgInfo,
		ProcCallInfo0, ProcCallInfo, ModuleInfo0, ModuleInfo) :-
	map__lookup(UnusedArgInfo, proc(PredId, ProcId), UnusedArgs),
	module_info_pred_proc_info(ModuleInfo0, PredId, ProcId, PredInfo0,
								OldProc0), 
	(
		UnusedArgs = []
	->
		ModuleInfo = ModuleInfo0,
		ProcCallInfo = ProcCallInfo0
	;
		need_extra_proc(ModuleInfo0, UnusedArgs, proc(PredId, ProcId), 
			IntermodUnusedArgs, InMap),
		pred_info_import_status(PredInfo0, Status0),
		(
			Status0 = opt_imported,
			InMap = yes,
			IntermodUnusedArgs = no
		->
			% If this predicate is from a .opt file, and
			% no more arguments have been removed than in the
			% original module, then leave the import status
			% as opt_imported so that dead_proc_elim will remove
			% it if no other optimization is performed on it.
			Status = opt_imported
		;
			Status0 = exported,
			InMap = yes
		->
			% This specialized version of the predicate was
			% declared in the .opt file for this module so
			% it must be exported.
			Status = exported
		;
			Status = local
		),
		( IntermodUnusedArgs = no ->
			NameSuffix = "__ua"
		;
			% This predicate was declared in a .opt file,
			% but more arguments were removed than was declared
			% in the .opt file, so a different name is required. 
			NameSuffix = "__uab"
		),
		make_new_pred_info(ModuleInfo0, PredInfo0, UnusedArgs,
		    NameSuffix, Status, proc(PredId, ProcId), NewPredInfo0),
		pred_info_procedures(NewPredInfo0, NewProcs0),
		next_mode_id(NewProcs0, no, NewProcId),

			% Assign the old procedure to a new predicate, which
			% will be fixed up in fixup_unused_args.
		map__set(NewProcs0, NewProcId, OldProc0, NewProcs),
		pred_info_set_procedures(NewPredInfo0, NewProcs, NewPredInfo),

			% add the new proc to the pred table
		module_info_get_predicate_table(ModuleInfo0, PredTable0),
		predicate_table_insert(PredTable0, NewPredInfo, NewPredId,
								PredTable1),
		pred_info_module(NewPredInfo, PredModule),
		pred_info_name(NewPredInfo, PredName),
		PredSymName = qualified(PredModule, PredName),
			% add the new proc to the proc_call_info map
		map__det_insert(ProcCallInfo0, proc(PredId, ProcId),
		    call_info(NewPredId, NewProcId, PredSymName, UnusedArgs),
		    ProcCallInfo),
		(
			Status0 = exported,
			IntermodUnusedArgs = yes(UnusedArgs2)
		->
			% Add an exported predicate with the number of removed
			% arguments promised in the .opt file which just calls
			% the new predicate.
			make_new_pred_info(ModuleInfo0, PredInfo0,
				UnusedArgs2, "__ua", exported,
				proc(PredId, ProcId), ExtraPredInfo0),
			create_call_goal(UnusedArgs, NewPredId, NewProcId,
				PredModule, PredName, OldProc0, ExtraProc0),
			proc_info_headvars(OldProc0, HeadVars0),
			remove_listof_elements(HeadVars0, 1, UnusedArgs2,
				IntermodHeadVars),
			proc_info_set_headvars(ExtraProc0, IntermodHeadVars,
				ExtraProc1),
			proc_info_argmodes(OldProc0, ArgModes0),
			remove_listof_elements(ArgModes0, 1, UnusedArgs2,
				IntermodArgModes),
			proc_info_set_argmodes(ExtraProc1, IntermodArgModes,
					ExtraProc),
			pred_info_procedures(ExtraPredInfo0, ExtraProcs0),
			next_mode_id(ExtraProcs0, no, ExtraProcId),
			map__set(ExtraProcs0, ExtraProcId,
				ExtraProc, ExtraProcs),
			pred_info_set_procedures(ExtraPredInfo0, ExtraProcs,
				ExtraPredInfo),
			predicate_table_insert(PredTable1, ExtraPredInfo,
					_, PredTable2)
		;
			PredTable2 = PredTable1
		),

		predicate_table_get_preds(PredTable2, Preds0),
		pred_info_procedures(PredInfo0, Procs0),
		create_call_goal(UnusedArgs, NewPredId, NewProcId,
			PredModule, PredName, OldProc0, OldProc),
		map__set(Procs0, ProcId, OldProc, Procs),
		pred_info_set_procedures(PredInfo0, Procs, PredInfo),
		map__det_update(Preds0, PredId, PredInfo, Preds1),
		predicate_table_set_preds(PredTable2, Preds1, PredTable),
		module_info_set_predicate_table(ModuleInfo0, PredTable,
							ModuleInfo)
	).

	% Check that if this procedure has a pragma unused_args declaration 
	% in the .opt file, the number of removed arguments matches with what
	% has just been computed. If not, it means that more arguments were
	% found to be unused given the information from .opt files, so we need
	% to create an interface predicate with the promised number of
	% arguments removed which calls the fully optimized version.
:- pred need_extra_proc(module_info::in, list(int)::in, pred_proc_id::in,
		maybe(list(int))::out, bool::out) is det.

need_extra_proc(ModuleInfo, UnusedArgs, PredProcId,
		MaybeIntermodUnusedArgs, InMap) :-
	module_info_unused_arg_info(ModuleInfo, IntermodUnusedArgInfo),
	(
		map__search(IntermodUnusedArgInfo,
			PredProcId, IntermodUnusedArgs)
	->
		InMap = yes,
		( IntermodUnusedArgs = UnusedArgs ->
			MaybeIntermodUnusedArgs = no
		;
			MaybeIntermodUnusedArgs = yes(IntermodUnusedArgs)
		)
	;
		InMap = no,
		MaybeIntermodUnusedArgs = no
	).

:- pred make_new_pred_info(module_info::in, pred_info::in, list(int)::in,
	    string::in, import_status::in, pred_proc_id::in,
	    pred_info::out) is det.

make_new_pred_info(ModuleInfo, PredInfo0, UnusedArgs, NameSuffix, Status,
		proc(_PredId, ProcId), PredInfo) :-
	pred_info_module(PredInfo0, PredModule),
	pred_info_name(PredInfo0, Name0),
	pred_info_get_is_pred_or_func(PredInfo0, PredOrFunc),
	proc_id_to_int(ProcId, ProcInt),
	string__int_to_string(ProcInt, Id),
	pred_info_arg_types(PredInfo0, Tvars, ArgTypes0),
		% create a unique new pred name using the old proc_id
	(
		string__prefix(Name0, "__"),
		\+ string__prefix(Name0, "__LambdaGoal__")
	->
		(
				% fix up special pred names
			special_pred_get_type(Name0, ArgTypes0, Type),
			type_to_type_id(Type, TypeId0, _)
		->
			TypeId = TypeId0
		;
			string__append_list(["unused_args:make_new_pred_info\n",
					"cannot make label for special pred `",
					Name0, "'."], Message),
			error(Message)
		),
		type_util__type_id_module(ModuleInfo, TypeId, TypeModule),
		type_util__type_id_name(ModuleInfo, TypeId, TypeName),
		type_util__type_id_arity(ModuleInfo, TypeId, TypeArity),
		string__int_to_string(TypeArity, TypeAr),
		string__append_list(
			[Name0, "_", TypeModule, "_", TypeName, "_", TypeAr],
			Name1)
	;
		Name1 = Name0
	),
	string__append_list([Name1, NameSuffix, Id], Name),
	pred_info_arity(PredInfo0, Arity),
	pred_info_typevarset(PredInfo0, TypeVars),
	remove_listof_elements(ArgTypes0, 1, UnusedArgs, ArgTypes),
	pred_info_context(PredInfo0, Context),
	pred_info_clauses_info(PredInfo0, ClausesInfo),
	pred_info_get_marker_list(PredInfo0, MarkerList),
	pred_info_get_goal_type(PredInfo0, GoalType),
		% *** This will need to be fixed when the condition
		%	field of the pred_info becomes used.
	pred_info_init(PredModule, qualified(PredModule, Name), Arity, Tvars,
		ArgTypes, true, Context, ClausesInfo, Status, MarkerList,
		GoalType, PredOrFunc, PredInfo1),
	pred_info_set_typevarset(PredInfo1, TypeVars, PredInfo).


	% Replace the goal in the procedure with one to call the given
	% pred_id and proc_id.
:- pred create_call_goal(list(int)::in, pred_id::in, proc_id::in,
		string::in, string::in, proc_info::in, proc_info::out) is det.

create_call_goal(UnusedArgs, NewPredId, NewProcId, PredModule,
		PredName, OldProc0, OldProc) :-
	proc_info_headvars(OldProc0, HeadVars),
	proc_info_goal(OldProc0, Goal0), 
	Goal0 = _GoalExpr - GoalInfo0,
	proc_info_vartypes(OldProc0, VarTypes0),
	set__list_to_set(HeadVars, NonLocals),
	map__apply_to_list(HeadVars, VarTypes0, VarTypeList),
	map__from_corresponding_lists(HeadVars, VarTypeList, VarTypes1),
		% the varset should probably be fixed up, but it
		% shouldn't make too much difference
	proc_info_variables(OldProc0, Varset0),
	remove_listof_elements(HeadVars, 1, UnusedArgs, NewHeadVars),
	GoalExpr = call(NewPredId, NewProcId, NewHeadVars,
		      not_builtin, no, qualified(PredModule, PredName)),
	Goal1 = GoalExpr - GoalInfo0,
	implicitly_quantify_goal(Goal1, Varset0, VarTypes1, NonLocals, 
			Goal, Varset, VarTypes, _),
	proc_info_set_goal(OldProc0, Goal, OldProc1),
	proc_info_set_varset(OldProc1, Varset, OldProc2),
	proc_info_set_vartypes(OldProc2, VarTypes, OldProc).


	% Create a pred_info for an imported pred with a pragma unused_args
	% in the .opt file.
:- pred make_imported_unused_args_pred_infos(pred_proc_list::in,
		proc_call_info::in, proc_call_info::out,
		module_info::in, module_info::out) is det.

 make_imported_unused_args_pred_infos([], ProcCallInfo, ProcCallInfo,
				ModuleInfo, ModuleInfo).
 make_imported_unused_args_pred_infos([OptProc | OptProcs],
 		ProcCallInfo0, ProcCallInfo, ModuleInfo0, ModuleInfo) :-
	module_info_unused_arg_info(ModuleInfo0, UnusedArgInfo),
	map__lookup(UnusedArgInfo, OptProc, UnusedArgs),
	OptProc = proc(PredId, ProcId),
	module_info_pred_proc_info(ModuleInfo0,
		PredId, ProcId, PredInfo0, ProcInfo0),
	make_new_pred_info(ModuleInfo0, PredInfo0, UnusedArgs,
		"__ua", imported, OptProc, NewPredInfo0),
	pred_info_procedures(NewPredInfo0, NewProcs0),
	next_mode_id(NewProcs0, no, NewProcId),

		% Assign the old procedure to a new predicate.
	proc_info_headvars(ProcInfo0, HeadVars0),
	remove_listof_elements(HeadVars0, 1, UnusedArgs, HeadVars),
	proc_info_set_headvars(ProcInfo0, HeadVars, ProcInfo1),
	proc_info_argmodes(ProcInfo1, ArgModes0),
	remove_listof_elements(ArgModes0, 1, UnusedArgs, ArgModes),
	proc_info_set_argmodes(ProcInfo0, ArgModes, ProcInfo),
	map__set(NewProcs0, NewProcId, ProcInfo, NewProcs),
	pred_info_set_procedures(NewPredInfo0, NewProcs, NewPredInfo),

		% Add the new proc to the pred table.
	module_info_get_predicate_table(ModuleInfo0, PredTable0),
	predicate_table_insert(PredTable0, NewPredInfo, NewPredId, PredTable1),
	module_info_set_predicate_table(ModuleInfo0, PredTable1, ModuleInfo1),
	pred_info_module(NewPredInfo, PredModule),
	pred_info_name(NewPredInfo, PredName),
	PredSymName = qualified(PredModule, PredName),
		% Add the new proc to the proc_call_info map.
	map__det_insert(ProcCallInfo0, proc(PredId, ProcId),
		call_info(NewPredId, NewProcId, PredSymName, UnusedArgs),
		ProcCallInfo1),
	make_imported_unused_args_pred_infos(OptProcs, ProcCallInfo1,
		ProcCallInfo, ModuleInfo1, ModuleInfo).


:- pred remove_listof_elements(list(T)::in, int::in, list(int)::in,
							 list(T)::out) is det.

remove_listof_elements(List0, ArgNo, ElemsToRemove, List) :-
	(
		ElemsToRemove = []
	->
		List = List0
	;
		(
			List0 = [Head | Tail],
			NextArg is ArgNo + 1,
			(
				list__member(ArgNo, ElemsToRemove)
			->
				List = List1
			;
				List = [Head | List1]
			),
			remove_listof_elements(Tail, NextArg,
							ElemsToRemove, List1)
		;
			List0 = [],
			List = List0
		)
	).

	
:- pred get_unused_arg_nos(var_dep::in, list(var)::in, int::in,
						list(int)::out) is det.

get_unused_arg_nos(_, [], _, []).
get_unused_arg_nos(LocalVars, [HeadVar | HeadVars], ArgNo, UnusedArgs) :-
	NextArg is ArgNo + 1,
	(
		map__contains(LocalVars, HeadVar)
	->
		UnusedArgs = [ArgNo | UnusedArgs1]
	;
		UnusedArgs = UnusedArgs1
	),
	get_unused_arg_nos(LocalVars, HeadVars, NextArg, UnusedArgs1). 
				

		% note - we should probably remove unused variables from
		%	the type map
:- pred fixup_unused_args(var_usage::in, pred_proc_list::in, proc_call_info::in,
			module_info::in, module_info::out, bool::in,
			io__state::di, io__state::uo) is det.

fixup_unused_args(_, [], _, Mod, Mod, _) --> []. 
fixup_unused_args(VarUsage, [PredProc | PredProcs], ProcCallInfo,
			ModuleInfo0, ModuleInfo, VeryVerbose) -->
	(
		{ VeryVerbose = yes }
	->
		{ PredProc = proc(PredId, ProcId) },
		io__write_string("% Fixing up `"),
		{ predicate_name(ModuleInfo0, PredId, Name) },
		{ predicate_arity(ModuleInfo0, PredId, Arity) },
		{ proc_id_to_int(ProcId, ProcInt) },
		io__write_string(Name),
		io__write_string("/"),
		io__write_int(Arity),
		io__write_string("' in mode "),
		io__write_int(ProcInt),
		io__write_char('\n')
	;
		[]
	),
	{ do_fixup_unused_args(VarUsage, PredProc, ProcCallInfo, ModuleInfo0,
								ModuleInfo1) },
	fixup_unused_args(VarUsage, PredProcs, ProcCallInfo, ModuleInfo1,
						ModuleInfo, VeryVerbose).

:- pred do_fixup_unused_args(var_usage::in, pred_proc_id::in,
		proc_call_info::in, module_info::in, module_info::out) is det.

do_fixup_unused_args(VarUsage, proc(OldPredId, OldProcId), ProcCallInfo,
								Mod0, Mod) :- 
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
	module_info_pred_proc_info(Mod0, PredId, ProcId, PredInfo0, ProcInfo0),
	proc_info_vartypes(ProcInfo0, VarTypes0),
	module_info_preds(Mod0, Preds0),
	pred_info_procedures(PredInfo0, Procs0),

	proc_info_headvars(ProcInfo0, HeadVars0),
	proc_info_argmodes(ProcInfo0, ArgModes0),
	proc_info_variables(ProcInfo0, Varset0),
	proc_info_goal(ProcInfo0, Goal0),
	remove_listof_elements(HeadVars0, 1, UnusedArgs, HeadVars),
	remove_listof_elements(ArgModes0, 1, UnusedArgs, ArgModes),
	proc_info_set_headvars(ProcInfo0, HeadVars, FixedProc1),
	proc_info_set_argmodes(FixedProc1, ArgModes, FixedProc2),

		% remove unused vars from goal
	fixup_goal(Mod0, UnusedVars, ProcCallInfo, Changed, Goal0, Goal1),
	(
		Changed = yes,
			% if anything has changed, rerun quantification
		set__list_to_set(HeadVars, NonLocals),
		implicitly_quantify_goal(Goal1, Varset0, VarTypes0, NonLocals,
						Goal, Varset, VarTypes, _),
		proc_info_set_goal(FixedProc2, Goal, FixedProc3),
		proc_info_set_varset(FixedProc3, Varset, FixedProc4),
		proc_info_set_vartypes(FixedProc4, VarTypes, FixedProc5)
	;
		Changed = no,
		proc_info_set_vartypes(FixedProc2, VarTypes0, FixedProc5)
	),
	map__set(Procs0, ProcId, FixedProc5, Procs),
	pred_info_set_procedures(PredInfo0, Procs, PredInfo),
	map__set(Preds0, PredId, PredInfo, Preds),
	module_info_set_preds(Mod0, Preds, Mod).


% 	this is the important bit of the transformation
:- pred fixup_goal(module_info::in, list(var)::in, proc_call_info::in,
			bool::out, hlds_goal::in, hlds_goal::out) is det.

fixup_goal(ModuleInfo, UnusedVars, ProcCallInfo, Changed, Goal0, Goal) :-
	fixup_goal_expr(ModuleInfo, UnusedVars, ProcCallInfo,
						Changed, Goal0, Goal1),
	Goal1 = GoalExpr - GoalInfo0,
	(
		Changed = yes
	->
		fixup_goal_info(UnusedVars, GoalInfo0, GoalInfo)
	;
		GoalInfo = GoalInfo0
	),
	Goal = GoalExpr - GoalInfo.
		

:- pred fixup_goal_expr(module_info::in, list(var)::in, proc_call_info::in,
			bool::out, hlds_goal::in, hlds_goal::out) is det.

fixup_goal_expr(ModuleInfo, UnusedVars, ProcCallInfo, Changed,
		conj(Goals0) - GoalInfo, conj(Goals) - GoalInfo) :-
	fixup_conjuncts(ModuleInfo, UnusedVars, ProcCallInfo, no,
						Changed, Goals0, Goals).

fixup_goal_expr(ModuleInfo, UnusedVars, ProcCallInfo, Changed,
		disj(Goals0, SM) - GoalInfo, disj(Goals, SM) - GoalInfo) :-
	fixup_disjuncts(ModuleInfo, UnusedVars, ProcCallInfo,
				no, Changed, Goals0, Goals).

fixup_goal_expr(ModuleInfo, UnusedVars, ProcCallInfo, Changed,
		not(NegGoal0) - GoalInfo, not(NegGoal) - GoalInfo) :-
	fixup_goal(ModuleInfo, UnusedVars, ProcCallInfo,
				Changed, NegGoal0, NegGoal).

fixup_goal_expr(ModuleInfo, UnusedVars, ProcCallInfo, Changed,
		switch(Var, CanFail, Cases0, SM) - GoalInfo,
		switch(Var, CanFail, Cases, SM) - GoalInfo) :-
	fixup_cases(ModuleInfo, UnusedVars, ProcCallInfo,
				no, Changed, Cases0, Cases).

fixup_goal_expr(ModuleInfo, UnusedVars, ProcCallInfo, Changed,
		if_then_else(Vars, Cond0, Then0, Else0, SM) - GoalInfo, 
		if_then_else(Vars, Cond, Then, Else, SM) - GoalInfo) :- 
	fixup_goal(ModuleInfo, UnusedVars, ProcCallInfo, Changed1, Cond0, Cond),
	fixup_goal(ModuleInfo, UnusedVars, ProcCallInfo, Changed2, Then0, Then),
	fixup_goal(ModuleInfo, UnusedVars, ProcCallInfo, Changed3, Else0, Else),
	bool__or_list([Changed1, Changed2, Changed3], Changed).

fixup_goal_expr(ModuleInfo, UnusedVars, ProcCallInfo, Changed,
		some(Vars, SubGoal0) - GoalInfo,
		some(Vars, SubGoal) - GoalInfo) :-
	fixup_goal(ModuleInfo, UnusedVars, ProcCallInfo,
				Changed, SubGoal0, SubGoal).

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

fixup_goal_expr(ModuleInfo, UnusedVars, _ProcCallInfo,
			Changed, GoalExpr0 - GoalInfo, GoalExpr - GoalInfo) :-
	GoalExpr0 = unify(Var, Rhs, Mode, Unify0, Context),
	(
		fixup_unify(ModuleInfo, UnusedVars, Changed0, Unify0, Unify)
	->
		GoalExpr = unify(Var, Rhs, Mode, Unify, Context),
		Changed = Changed0 
	;
		GoalExpr = conj([]),
		Changed = yes
	).

fixup_goal_expr(_ModuleInfo, _UnusedVars, _ProcCallInfo, no,
			GoalExpr - GoalInfo, GoalExpr - GoalInfo) :-
	GoalExpr = higher_order_call(_, _, _, _, _).

fixup_goal_expr(_ModuleInfo, _UnusedVars, _ProcCallInfo, no,
			GoalExpr - GoalInfo, GoalExpr - GoalInfo) :-
	GoalExpr = pragma_c_code(_, _, _, _, _, _, _, _).

	% Remove useless unifications from a list of conjuncts.
:- pred fixup_conjuncts(module_info::in, list(var)::in, proc_call_info::in,
		bool::in, bool::out, hlds_goals::in, hlds_goals::out) is det. 

fixup_conjuncts(_, _, _, Changed, Changed, [], []).
fixup_conjuncts(ModuleInfo, UnusedVars, ProcCallInfo, Changed0, Changed,
					[Goal0 | Goals0], Goals) :-
	fixup_goal(ModuleInfo, UnusedVars, ProcCallInfo,
				LocalChanged, Goal0, Goal),
	(
		LocalChanged = yes
	->
		Changed1 = yes
	;
		Changed1 = Changed0
	),
	(
			% replacing a goal with conj([]) signals that it is
			%	no longer needed
		Goal = conj([]) - _
	->
		Goals = Goals1
	;
		Goals = [Goal | Goals1]
	),
	fixup_conjuncts(ModuleInfo, UnusedVars, ProcCallInfo,
			Changed1, Changed, Goals0, Goals1).
	

	% We can't remove unused goals from the list of disjuncts as we do
	% for conjuncts, since that would change the determinism of
	% the goal.
:- pred fixup_disjuncts(module_info::in, list(var)::in, proc_call_info::in,
		bool::in, bool::out, hlds_goals::in, hlds_goals::out) is det. 

fixup_disjuncts(_, _, _, Changed, Changed, [], []).
fixup_disjuncts(ModuleInfo, UnusedVars, ProcCallInfo, Changed0, Changed,
					[Goal0 | Goals0], [Goal | Goals]) :-
	fixup_goal(ModuleInfo, UnusedVars, ProcCallInfo,
				LocalChanged, Goal0, Goal),
	(
		LocalChanged = yes
	->
		Changed1 = yes
	;
		Changed1 = Changed0
	),
	fixup_disjuncts(ModuleInfo, UnusedVars, ProcCallInfo,
			Changed1, Changed, Goals0, Goals).

:- pred fixup_cases(module_info::in, list(var)::in, proc_call_info::in,
		bool::in, bool::out, list(case)::in, list(case)::out) is det.
		
fixup_cases(_, _, _, Changed, Changed, [], []).
fixup_cases(ModuleInfo, UnusedVars, ProcCallInfo, Changed0, Changed, 
		[case(ConsId, Goal0) | Cases0], [case(ConsId, Goal) | Cases]) :-
	fixup_goal(ModuleInfo, UnusedVars, ProcCallInfo,
				LocalChanged, Goal0, Goal),
	(	
		LocalChanged = yes
	->
		Changed1 = yes
	;
		Changed1 = Changed0
	),
	fixup_cases(ModuleInfo, UnusedVars, ProcCallInfo,
			Changed1, Changed, Cases0, Cases).


		% fix up a unification, fail if the unification is no
		%	longer needed
:- pred fixup_unify(module_info::in, list(var)::in, bool::out,
				unification::in, unification::out) is semidet.

	% a simple test doesn't have any unused vars to fixup
fixup_unify(_, _UnusedVars, no, simple_test(A, B), simple_test(A, B)).

	% Var1 unused => we don't need the assignment
	% Var2 unused => Var1 unused
fixup_unify(_, UnusedVars, no, assign(Var1, Var2), assign(Var1, Var2)) :-
	\+ list__member(Var1, UnusedVars).

	% LVar unused => we don't need the unification
fixup_unify(_, UnusedVars, no, construct(LVar, ConsId, ArgVars, ArgModes),
				construct(LVar, ConsId, ArgVars, ArgModes)) :-	
	\+ list__member(LVar, UnusedVars).
	
fixup_unify(ModuleInfo, UnusedVars, Changed,
		deconstruct(LVar, ConsId, ArgVars, ArgModes, CanFail),
		deconstruct(LVar, ConsId, ArgVars, ArgModes, CanFail)) :-
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
fixup_unify(_, _, _, complicated_unify(_, _), _) :-
		error("unused_args:fixup_goal : complicated unify").

	% Check if any of the arguments of a deconstruction are unused, if
	% so Changed will be yes and quantification will be rerun. Fails if
	% none of the arguments are used. Arguments which further instantiate
	% the deconstructed variable are ignored in this.
:- pred check_deconstruct_args(module_info::in, list(var)::in, list(var)::in,
			list(uni_mode)::in, bool::out, bool::in) is semidet.
						
check_deconstruct_args(ModuleInfo, UnusedVars, Args, Modes, Changed, Used) :-
	(
		Args = [ArgVar | ArgVars], Modes = [ArgMode | ArgModes]
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
		Args = [], Modes = []
	->
		Changed = no,
		Used = yes
	;
		error("check_deconstruct_args - invalid call")
	).

	% Remove unused vars from the instmap_delta, quantification fixes
	%	up the rest.
:- pred fixup_goal_info(list(var)::in, hlds_goal_info::in,
						hlds_goal_info::out) is det.

fixup_goal_info(UnusedVars, GoalInfo0, GoalInfo) :-
	goal_info_get_instmap_delta(GoalInfo0, InstMap0),
	instmap_delta_delete_vars(InstMap0, UnusedVars, InstMap),
	goal_info_set_instmap_delta(GoalInfo0, InstMap, GoalInfo).

%-------------------------------------------------------------------------------

		% Except for type_infos, all args that are unused
		% in one mode of a predicate should be unused in all of the
		% modes of a predicate, so we only need to put out one warning
		% for each predicate.
:- pred output_warnings_and_pragmas(module_info::in, unused_arg_info::in,
	maybe(io__output_stream)::in, bool::in, pred_proc_list::in,
	set(pred_id)::in, io__state::di, io__state::uo) is det.

output_warnings_and_pragmas(_, _, _, _, [], _) --> [].
output_warnings_and_pragmas(ModuleInfo, UnusedArgInfo, WriteOptPragmas,
		DoWarn, [proc(PredId, ProcId) | Rest], WarnedPredIds0) -->
	(
		{ map__search(UnusedArgInfo, proc(PredId, ProcId),
				UnusedArgs) }
	->
		{ module_info_pred_info(ModuleInfo, PredId, PredInfo) },
		(
			{
			pred_info_name(PredInfo, Name),
			\+ pred_info_is_imported(PredInfo),
				% Don't warn about builtins
				% that have unused arguments.
			\+ code_util__predinfo_is_builtin(PredInfo),
			\+ code_util__compiler_generated(PredInfo),
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
				IdLen is Length - Position - 4,
				string__right(Name, IdLen, Id),
				string__to_int(Id, _)
			)
			}
		->	
			write_unused_args_to_opt_file(WriteOptPragmas,
				PredInfo, ProcId, UnusedArgs),
			maybe_warn_unused_args(DoWarn, ModuleInfo, PredInfo,
				PredId, ProcId, UnusedArgs,
				WarnedPredIds0, WarnedPredIds1)
		;
			{ WarnedPredIds1 = WarnedPredIds0 }
		)
	;
		{ WarnedPredIds1 = WarnedPredIds0 }

	),
	output_warnings_and_pragmas(ModuleInfo, UnusedArgInfo,
		WriteOptPragmas, DoWarn, Rest, WarnedPredIds1).


:- pred write_unused_args_to_opt_file(maybe(io__output_stream)::in,
		pred_info::in, proc_id::in, list(int)::in,
		io__state::di, io__state::uo) is det.

write_unused_args_to_opt_file(no, _, _, _) --> [].
write_unused_args_to_opt_file(yes(OptStream), PredInfo, ProcId, UnusedArgs) -->
	(
		{ pred_info_is_exported(PredInfo) },
		{ UnusedArgs \= [] }
	->
		{ pred_info_module(PredInfo, Module) },
		{ pred_info_name(PredInfo, Name) },
		{ pred_info_arity(PredInfo, Arity) },
		{ pred_info_get_is_pred_or_func(PredInfo, PredOrFunc) },
		io__set_output_stream(OptStream, OldOutput),	
		mercury_output_pragma_unused_args(PredOrFunc,
			qualified(Module, Name), Arity, ProcId, UnusedArgs),
		io__set_output_stream(OldOutput, _)
	;
		[]
	).

:- pred maybe_warn_unused_args(bool::in, module_info::in, pred_info::in,
		pred_id::in, proc_id::in, list(int)::in, set(pred_id)::in,
		set(pred_id)::out, io__state::di, io__state::uo) is det.


maybe_warn_unused_args(no, _, _, _, _, _, WarnedPredIds, WarnedPredIds) --> [].
maybe_warn_unused_args(yes, _ModuleInfo, PredInfo, PredId, ProcId,
		UnusedArgs0, WarnedPredIds0, WarnedPredIds) -->
	( { set__member(PredId, WarnedPredIds0) } ->
		{ WarnedPredIds = WarnedPredIds0 }
	;
		{
		set__insert(WarnedPredIds0, PredId, WarnedPredIds),
		pred_info_procedures(PredInfo, Procs),
		map__lookup(Procs, ProcId, Proc),
		proc_info_headvars(Proc, HeadVars),
		list__length(HeadVars, NumHeadVars),

		% Strip off the extra type_info arguments 
		% inserted at the front by polymorphism.m
		pred_info_arity(PredInfo, Arity),
		NumToDrop is NumHeadVars - Arity,
		adjust_unused_args(NumToDrop,
			UnusedArgs0, UnusedArgs)
		},
		( { UnusedArgs \= [] } ->
			report_unused_args(PredInfo, UnusedArgs)
		;
			[]
		)
	).


	% Warn about unused arguments in a predicate. Only arguments unused
	% in every mode of a predicate are warned about. The warning is
	% suppressed for type_infos.
:- pred report_unused_args(pred_info::in, list(int)::in,
		io__state::di, io__state::uo) is det.

report_unused_args(PredInfo, UnusedArgs) --> 
	{ list__length(UnusedArgs, NumArgs) },
	{ pred_info_context(PredInfo, Context) },
	prog_out__write_context(Context),
	io__write_string("In "),
	{ pred_info_get_is_pred_or_func(PredInfo, PredOrFunc) },
	hlds_out__write_pred_or_func(PredOrFunc),
	io__write_string(" `"),
	{ pred_info_module(PredInfo, Module) },
	io__write_string(Module),
	io__write_string(":"),
	{ pred_info_name(PredInfo, Name) },
	io__write_string(Name), 
	io__write_string(("'/")),
	{ pred_info_arity(PredInfo, Arity) },
	io__write_int(Arity),
	io__write_string(":\n"),
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

:- pred write_int_list(list(int)::in, io__state::di, io__state::uo) is det.

write_int_list([]) --> [].
write_int_list([First | Rest]) -->
	io__write_int(First),
	write_int_list_2(Rest).

:- pred write_int_list_2(list(int)::in, io__state::di, io__state::uo) is det.

write_int_list_2([]) --> [].
write_int_list_2([First | Rest]) -->
	io__write_string(", "),
	io__write_int(First),
	write_int_list_2(Rest).

	% adjust warning message for the presence of type_infos.
:- pred adjust_unused_args(int::in, list(int)::in, list(int)::out) is det.

adjust_unused_args(_, [], []).
adjust_unused_args(NumToDrop, [UnusedArgNo | UnusedArgNos0], AdjUnusedArgs) :-
	NewArg is UnusedArgNo - NumToDrop,
	(
		NewArg < 1
	->
		AdjUnusedArgs = AdjUnusedArgs1
	;
		AdjUnusedArgs = [NewArg | AdjUnusedArgs1]
	),
	adjust_unused_args(NumToDrop, UnusedArgNos0, AdjUnusedArgs1).	


:- pred output_arg_list(list(int)::in, io__state::di, io__state::uo) is det. 

output_arg_list([]) --> { error("output_list_int called with empty list") }.
output_arg_list([Arg | Rest]) -->
	io__write_int(Arg),
	(
		{ Rest = [] } 
	;	
		{ Rest = [_ | _] },
		output_arg_list_2(Rest)
	).


:- pred output_arg_list_2(list(int)::in, io__state::di,
						io__state::uo) is det.

output_arg_list_2(Args) -->
	(
		{ Args = [First, Second | Rest] }
	->
		io__write_string(", "),
		io__write_int(First),
		output_arg_list_2([Second | Rest])
	;
		{ Args = [Last] }
	->
		io__write_string(" and "),
		io__write_int(Last)
	;
		{ error("output_arg_list_2 called with empty list") }
	).

