%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
/* unused_args.m
/
/  Main author - stayl, Jan 1996
/
/  Detects and removes unused input arguments in procedures, especially
/  type_infos. Currently only does analysis within a module.
/
/  To disable the warnings use --no-warn-useless-args
/  To disable the optimisation use --no-optimize-useless-args
/
/  An argument is used if it
/	- it is in a predicate external to the current module
/	- it or any of its aliases are used to instantiate an output variable
/	- it is involved in a simple test, switch or a semidet deconstruction 
/	- it is an argument to another predicate in this module which is used.
/
/  The first step is to determine which arguments of which predicates are
/	used locally to their predicate. For each unused argument, a set of
/	other arguments that it depends on is built up.
/  The next step is to iterate over the this map, checking for each unused
/	argument whether any of the arguments it depends on has become used
/	in the last iteration. Iterations are repeated until a fixpoint is
/	reached.
/  Warnings are then output. The warning message indicates which arguments
/	are used in none of the modes of a predicate. 
/  The predicates are then fixed up. Unused variables and unifications are
/	removed.
/
*/

:- module unused_args.

%-------------------------------------------------------------------------------
:- interface.

:- import_module hlds.
:- import_module io.

:- pred unused_args__process_module(module_info::in, module_info::out,
				io__state::di, io__state::uo) is det.

%-------------------------------------------------------------------------------
:- implementation.

:- import_module code_util, globals, make_hlds, mercury_to_mercury, mode_util.
:- import_module options, prog_io, prog_out, quantification, special_pred.
:- import_module type_util.

:- import_module bool, char, int, list, map, require.
:- import_module set, std_util, string, varset. 

		% stores whether a given variable is definitely used,
		%	or depends on a set of other variables and arguments.
:- type usage_info --->
		unused(set(var), set(arg)).

	% a collection of variable usages for each procedure
:- type var_usage == map(pred_proc_id, var_dep).

	% arguments are stored as their variable id, not their index
	%	in the argument vector
:- type arg == pair(pred_proc_id, var). 

:- type var_dep == map(var, usage_info).

		% map from pred_proc_id to a list of argument numbers
:- type unused_arg_info == map(pred_proc_id, list(int)).

:- type warning_info --->
		warning_info(term__context, string, int, list(int)).
			% context, pred name, arity, list of args to warn 


unused_args__process_module(ModuleInfo0, ModuleInfo) -->
	globals__io_lookup_bool_option(very_verbose, VeryVerbose),

	{ init_var_usage(ModuleInfo0, VarUsage0, PredProcs) },
	% maybe_write_string(VeryVerbose, "% Finished initialisation.\n"),

	{ unused_args_pass(PredProcs, VarUsage0, VarUsage) },
	% maybe_write_string(VeryVerbose, "% Finished analysis.\n"),

	{ map__init(UnusedArgInfo0) },
	{ get_unused_arg_info(ModuleInfo0, PredProcs, VarUsage,
					UnusedArgInfo0, UnusedArgInfo) },
	{ map__keys(UnusedArgInfo, PredProcsToFix) },

	globals__io_lookup_bool_option(warn_unused_args, DoWarn),
	(
		{ DoWarn = yes } 
	->
		{ map__init(WarningsMap0) }, 
		{ create_warning_info(ModuleInfo0, UnusedArgInfo,
				PredProcsToFix, WarningsMap0, WarningsMap) },
		{ map__values(WarningsMap, Warnings0) },
		{ list__sort(Warnings0, Warnings) },
		report_unused_args(Warnings)
		% maybe_write_string(VeryVerbose, "% Finished warnings.\n")	
	;
		[]
	),
	globals__io_lookup_bool_option(optimize_unused_args, DoFixup), 
	(
		{ DoFixup = yes }
	->
		{ map__init(ProcCallInfo0) },
		{ create_new_preds(PredProcsToFix, UnusedArgInfo, ProcCallInfo0,
				ProcCallInfo, ModuleInfo0, ModuleInfo1) }, 
		fixup_unused_args(VarUsage, PredProcsToFix, ProcCallInfo,
					ModuleInfo1, ModuleInfo, VeryVerbose)
		% maybe_write_string(VeryVerbose, "% Fixed up goals.\n")
	;
		{ ModuleInfo = ModuleInfo0 }
	).
	  

%-------------------------------------------------------------------------------
	% Initialisation section
	
	% init_var_usage/4 -  set initial status of all args of local
 	%	procs by examining the module_info
:- pred init_var_usage(module_info::in, var_usage::out,
				pred_proc_list::out) is det.

init_var_usage(ModuleInfo, VarUsage, PredProcList) :-
	map__init(VarUsage0),
	module_info_predids(ModuleInfo, PredIds),
	module_info_preds(ModuleInfo, PredTable),
	setup_local_var_usage(ModuleInfo, PredTable, PredIds,
				VarUsage0, VarUsage, PredProcLists),
	list__condense(PredProcLists, PredProcList).


	% setup args for the whole module.	
:- pred setup_local_var_usage(module_info::in, pred_table::in,
	list(pred_id)::in, var_usage::in, var_usage::out,
	list(pred_proc_list)::out) is det.

setup_local_var_usage(_, _, [], VarUsage, VarUsage, [[]]).
setup_local_var_usage(ModuleInfo, PredTable, [PredId | PredIds],
			VarUsage0, VarUsage, [PredProcList | PredProcLists]) :-
	map__lookup(PredTable, PredId, PredInfo),
	pred_info_non_imported_procids(PredInfo, ProcIds),
	setup_pred_args(ModuleInfo, PredId, ProcIds, VarUsage0, VarUsage1,
							[], PredProcList),
	setup_local_var_usage(ModuleInfo, PredTable, PredIds,
			VarUsage1, VarUsage, PredProcLists).


	% setup args for a predicate
:- pred setup_pred_args(module_info::in, pred_id::in, list(proc_id)::in,
			var_usage::in, var_usage::out,
			pred_proc_list::in, pred_proc_list::out) is det.

setup_pred_args(_, _, [], VarUsage, VarUsage, PredProcs, PredProcs).
setup_pred_args(ModuleInfo, PredId, [ProcId | Rest], VarUsage0, VarUsage,
						PredProcs0, PredProcs) :-
	module_info_pred_proc_info(ModuleInfo, PredId, ProcId,
						_PredInfo, ProcInfo),
	proc_info_argmodes(ProcInfo, ArgModes),
	proc_info_headvars(ProcInfo, HeadVars),
	proc_info_vartypes(ProcInfo, VarTypes),
	map__keys( VarTypes, Vars),
	map__init(VarDep0),
	initialise_vardep(VarDep0, Vars, VarDep1),
	setup_output_args(ModuleInfo, HeadVars, ArgModes, VarDep1, VarDep2),
	proc_info_goal(ProcInfo, Goal - _),
	traverse_goal(ModuleInfo, Goal, VarDep2, VarDep),
	map__set(VarUsage0, proc(PredId, ProcId), VarDep, VarUsage1),
	PredProcs1 = [proc(PredId, ProcId) | PredProcs0],
	setup_pred_args(ModuleInfo, PredId, Rest, VarUsage1, VarUsage,
						PredProcs1, PredProcs).


:- pred initialise_vardep(var_dep::in, list(var)::in, var_dep::out) is det.

initialise_vardep( VarDep, [], VarDep).
initialise_vardep( VarDep0, [Var | Vars], VarDep) :-
	set__init(VDep),
	set__init(Args),
	map__set( VarDep0, Var, unused(VDep, Args), VarDep1),
	initialise_vardep( VarDep1, Vars, VarDep).

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
			mode_is_output(ModuleInfo, Mode)
		->
			map__delete(VarDep0, Var, VarDep1)		
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


		% add a list of aliases for a variable
:- pred add_aliases(var_dep::in, var::in, list(var)::in, var_dep::out) is det.

add_aliases(UseInf0, Var, Aliases, UseInf) :-
	(
		map__search(UseInf0, Var, VarInf0)
	->
		VarInf0 = unused(VarDep0, ArgDep),
		set__insert_list(VarDep0, Aliases, VarDep),
		VarInf = unused(VarDep, ArgDep),
		map__set(UseInf0, Var, VarInf, UseInf)
	;
		UseInf = UseInf0
	).


:- pred set_list_vars_used(var_dep::in, list(var)::in, var_dep::out) is det.

set_list_vars_used(UseInf, [], UseInf).
set_list_vars_used(UseInf0, [Var | Vars], UseInf) :-
	map__delete(UseInf0, Var, UseInf1),
	set_list_vars_used(UseInf1, Vars, UseInf).


%-------------------------------------------------------------------------------
	% Traversal of goal structure, building up dependencies for all
	% variables. 

:- pred traverse_goal(module_info::in, hlds__goal_expr::in,
				var_dep::in, var_dep::out) is det.

% handle conjunction
traverse_goal(ModuleInfo, conj(Goals), UseInf0, UseInf) :-
	traverse_list_of_goals(ModuleInfo, Goals, UseInf0, UseInf).

% handle disjunction
traverse_goal(ModuleInfo, disj(Goals), UseInf0, UseInf) :-
	traverse_list_of_goals(ModuleInfo, Goals, UseInf0, UseInf).

% handle switch
traverse_goal(ModuleInfo, switch(Var, _, Cases), UseInf0, UseInf) :-
	map__delete(UseInf0, Var, UseInf1),
	list_case_to_list_goal(Cases, Goals),
	traverse_list_of_goals(ModuleInfo, Goals, UseInf1, UseInf).

% handle predicate call
traverse_goal(ModuleInfo, call(PredId, ProcId, Args, _, _, _, _),
						UseInf0, UseInf) :-
	module_info_pred_proc_info(ModuleInfo, PredId, ProcId, _Pred, Proc),
	proc_info_headvars(Proc, HeadVars),
	add_pred_call_arg_dep(proc(PredId, ProcId), Args, HeadVars,
		UseInf0, UseInf).

% handle if then else
traverse_goal(ModuleInfo, if_then_else(_, Cond - _, Then - _, Else - _),
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


% handle pragma(c_code, ...) - pragma_c_code uses all its args
traverse_goal(_, pragma_c_code(_, _, _, Args, _), UseInf0, UseInf) :-
	set_list_vars_used(UseInf0, Args, UseInf).

% cases to handle all the different types of unification
traverse_goal(_, unify(_, _, _, simple_test(Var1, Var2),_), UseInf0, UseInf)
		:-
	map__delete(UseInf0, Var1, UseInf1),
	map__delete(UseInf1, Var2, UseInf).
		
traverse_goal(_, unify(_, _, _, assign(Var1, Var2), _), UseInf0, UseInf) :-
	(
		map__contains(UseInf0, Var1)
	->
		add_aliases(UseInf0, Var2, [Var1], UseInf)
	;
		% if Var1 used to inst an output argument, Var2 used
		map__delete(UseInf0, Var2, UseInf)
	).

traverse_goal(_ModuleInfo,
		unify(Var1, _, _, deconstruct(_, _, Args, _, CanFail), _),
		UseInf0, UseInf) :-
	(
		CanFail = can_fail	
	->
		% a deconstruction that can_fail uses its left arg
		map__delete(UseInf0, Var1, UseInf)
	;
		add_aliases( UseInf0, Var1, Args, UseInf)	
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
	
traverse_goal(_, unify(_, _, _, complicated_unify(_, _, _), _),
		UseInf, UseInf) :-
    error("unused_args should come after making out of line compl unify pred").


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
		map__search(UseInf0, Var, VarUsage0)
	->
		VarUsage0 = unused(VarDep, ArgDep0),
		set__insert(ArgDep0, PredProc - Arg, ArgDep),
		map__det_update(UseInf0, Var, unused(VarDep, ArgDep), UseInf)
	;
		UseInf = UseInf0
	).
			

		% add Alias as an alias for all of Vars
:- pred add_construction_aliases(var_dep::in, var::in, list(var)::in,
						var_dep::out) is det.

add_construction_aliases(UseInf, _, [], UseInf).
add_construction_aliases(UseInf0, Alias, [Var | Vars], UseInf) :-
	(
		map__search(UseInf0, Var, VarInf)
	->
		VarInf = unused(VarDep0, ArgDep),
		set__insert(VarDep0, Alias, VarDep), 
		map__set(UseInf0, Var, unused(VarDep, ArgDep), UseInf1)
	;
		UseInf1 = UseInf0
	),
	add_construction_aliases(UseInf1, Alias, Vars, UseInf).


:- pred list_case_to_list_goal(list(case)::in, list(hlds__goal)::out) is det.

list_case_to_list_goal([], []).
list_case_to_list_goal([case(_, Goal) | Cases], [Goal | Goals]) :-
	list_case_to_list_goal(Cases, Goals).


:- pred traverse_list_of_goals(module_info::in, list(hlds__goal)::in,
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
		map__search(LocalVars0, Var, Usage)
	->
		Usage = unused(VarDep0, ArgDep0),
		(
			(
				set__member(Argument, ArgDep0),
				Argument = PredProc - ArgVar,
				var_is_used(PredProc, ArgVar, VarUsage)
			;	
				set__member(Var2, VarDep0),
				\+ map__contains(LocalVars0, Var2)
			)
		->
			map__delete(LocalVars0, Var, LocalVars1),	
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

	% new pred_id, proc_id, and the indices in the argument vector
	% of the arguments that have been removed.
:- type new_proc_info --->
		call_info(pred_id, proc_id, list(int)).


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
create_new_preds([proc(PredId, ProcId) | PredProcs], UnusedArgInfo,
			ProcCallInfo0, ProcCallInfo, ModuleInfo0, ModuleInfo) :- 
	map__lookup(UnusedArgInfo, proc(PredId, ProcId), UnusedArgs),
	module_info_pred_proc_info(ModuleInfo0, PredId, ProcId, PredInfo0,
								OldProc0), 
	(
		UnusedArgs = []
	->
		ModuleInfo1 = ModuleInfo0,
		ProcCallInfo1 = ProcCallInfo0
	;
		make_new_pred_info(ModuleInfo0, PredInfo0, UnusedArgs,
							ProcId, NewPredInfo0),
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

			% add the new proc to the proc_call_info map
		map__det_insert(ProcCallInfo0, proc(PredId, ProcId),
			call_info(NewPredId, NewProcId, UnusedArgs),
			ProcCallInfo1),

		predicate_table_get_preds(PredTable1, Preds0),
		pred_info_procedures(PredInfo0, Procs0),

			% create goal which just calls the new fixed up proc
		proc_info_goal(OldProc0, Goal0), 
		Goal0 = _GoalExpr - GoalInfo0,
		proc_info_headvars(OldProc0, HeadVars),
		proc_info_vartypes( OldProc0, VarTypes0),
		set__list_to_set(HeadVars, NonLocals),
		map__apply_to_list(HeadVars, VarTypes0, VarTypeList),
		map__from_corresponding_lists(HeadVars, VarTypeList, VarTypes1),
			% the varset should probably be fixed up, but it
			%	shouldn't make too much difference
		proc_info_variables(OldProc0, Varset0),
		hlds__is_builtin_make_builtin(no, no, IsBuiltin),
		map__init(FVars0),
		pred_info_name(PredInfo0, Name),
		remove_listof_elements(HeadVars, 1, UnusedArgs, NewHeadVars),
		GoalExpr = call(NewPredId, NewProcId, NewHeadVars,
			      IsBuiltin, no, unqualified(Name), FVars0),
		Goal1 = GoalExpr - GoalInfo0,
		implicitly_quantify_goal(Goal1, Varset0, VarTypes1, NonLocals, 
				Goal, Varset, VarTypes, _),
		proc_info_set_goal(OldProc0, Goal, OldProc1),
		proc_info_set_varset(OldProc1, Varset, OldProc2),
		proc_info_set_vartypes(OldProc2, VarTypes, OldProc3),
		proc_info_argmodes(OldProc3, ArgModes0),
		fixup_unused_arg_modes(ArgModes0, 1, UnusedArgs, ArgModes),
		proc_info_set_argmodes(OldProc3, ArgModes, OldProc),
		map__set(Procs0, ProcId, OldProc, Procs),
		pred_info_set_procedures(PredInfo0, Procs, PredInfo),
		map__det_update(Preds0, PredId, PredInfo, Preds1),
		predicate_table_set_preds(PredTable1, Preds1, PredTable2),
		module_info_set_predicate_table(ModuleInfo0, PredTable2,
							ModuleInfo1)
	),
	create_new_preds(PredProcs, UnusedArgInfo, ProcCallInfo1, ProcCallInfo,
						ModuleInfo1, ModuleInfo).
	

:- pred make_new_pred_info(module_info::in, pred_info::in, list(int)::in,
					proc_id::in, pred_info::out) is det.

make_new_pred_info(ModuleInfo, PredInfo0, UnusedArgs, ProcId, PredInfo) :-
	pred_info_module(PredInfo0, Module),
	pred_info_name(PredInfo0, Name0),
	string__int_to_string(ProcId, Id),
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
	        type_util__type_id_name(ModuleInfo, TypeId, TypeName),
                type_util__type_id_arity(ModuleInfo, TypeId, TypeArity),
		string__int_to_string(TypeArity, TypeAr),
		string__append_list([Name0, "_", TypeName, "_", TypeAr], Name1)
        ;

		Name1 = Name0
	),
	string__append_list([Name1, "__ua", Id], Name),
	pred_info_arity(PredInfo0, Arity),
	pred_info_typevarset(PredInfo0, TypeVars),
	remove_listof_elements(ArgTypes0, 1, UnusedArgs, ArgTypes),
	pred_info_context(PredInfo0, Context),
	pred_info_clauses_info(PredInfo0, ClausesInfo),
	(
		pred_info_is_inlined(PredInfo0)
	->
		Inline = yes
	;
		Inline = no
	),
	pred_info_get_goal_type(PredInfo0, GoalType),
		% *** This will need to be fixed when the condition
		%	field of the pred_info becomes used.
	pred_info_init(Module, unqualified(Name), Arity, Tvars, ArgTypes, true,
		Context, ClausesInfo, local, Inline, GoalType, predicate,
		PredInfo1),
	pred_info_set_typevarset(PredInfo1, TypeVars, PredInfo).


:- pred fixup_unused_arg_modes(list(mode)::in, int::in, list(int)::in,
						list(mode)::out) is det.

fixup_unused_arg_modes([], _, _, []).
fixup_unused_arg_modes([Mode0 | Modes0], ArgNo, UnusedArgs, [Mode | Modes]) :-
	(
		list__member(ArgNo, UnusedArgs)
	->
		Mode = (free -> free)
	;
		Mode = Mode0
	),
	NextArg is ArgNo + 1,
	fixup_unused_arg_modes(Modes0, NextArg, UnusedArgs, Modes).


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
		io__write_string(Name),
		io__write_string("/"),
		io__write_int(Arity),
		io__write_string("' in mode "),
		io__write_int(ProcId),
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
				call_info(NewPredId, NewProcId, UnusedArgs0))
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
	fixup_goal(UnusedVars, ProcCallInfo, Changed, Goal0, Goal1),
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
:- pred fixup_goal(list(var)::in, proc_call_info::in, bool::out,
				hlds__goal::in, hlds__goal::out) is det.

fixup_goal(UnusedVars, ProcCallInfo, Changed, Goal0, Goal) :-
	fixup_goal_expr(UnusedVars, ProcCallInfo, Changed, Goal0, Goal1),
	Goal1 = GoalExpr - GoalInfo0,
	(
		Changed = yes
	->
		fixup_goal_info(UnusedVars, GoalInfo0, GoalInfo)
	;
		GoalInfo = GoalInfo0
	),
	Goal = GoalExpr - GoalInfo.
		

:- pred fixup_goal_expr(list(var)::in, proc_call_info::in, bool::out,
				hlds__goal::in, hlds__goal::out) is det.

fixup_goal_expr(UnusedVars, ProcCallInfo, Changed,
		conj(Goals0) - GoalInfo, conj(Goals) - GoalInfo) :-
	fixup_conjuncts(UnusedVars, ProcCallInfo, no, Changed, Goals0, Goals).

fixup_goal_expr(UnusedVars, ProcCallInfo, Changed,
		disj(Goals0) - GoalInfo, disj(Goals) - GoalInfo) :-
	fixup_disjuncts(UnusedVars, ProcCallInfo, no, Changed, Goals0, Goals).

fixup_goal_expr(UnusedVars, ProcCallInfo, Changed,
		not(NegGoal0) - GoalInfo, not(NegGoal) - GoalInfo) :-
	fixup_goal(UnusedVars, ProcCallInfo, Changed, NegGoal0, NegGoal).

fixup_goal_expr(UnusedVars, ProcCallInfo, Changed,
		switch(Var, CanFail, Cases0) - GoalInfo,
		switch(Var, CanFail, Cases) - GoalInfo) :-
	fixup_cases(UnusedVars, ProcCallInfo, no, Changed, Cases0, Cases).

fixup_goal_expr(UnusedVars, ProcCallInfo, Changed,
		if_then_else(Vars, Cond0, Then0, Else0) - GoalInfo, 
		if_then_else(Vars, Cond, Then, Else) - GoalInfo) :- 
	fixup_goal(UnusedVars, ProcCallInfo, Changed1, Cond0, Cond),
	fixup_goal(UnusedVars, ProcCallInfo, Changed2, Then0, Then),
	fixup_goal(UnusedVars, ProcCallInfo, Changed3, Else0, Else),
	bool__or_list([Changed1, Changed2, Changed3], Changed).

fixup_goal_expr(UnusedVars, ProcCallInfo, Changed,
		some(Vars, SubGoal0) - GoalInfo,
		some(Vars, SubGoal) - GoalInfo) :-
	fixup_goal(UnusedVars, ProcCallInfo, Changed, SubGoal0, SubGoal).

fixup_goal_expr(_UnusedVars, ProcCallInfo, Changed,
        	call(PredId0, ProcId0, ArgVars0, B, C, D, E) - GoalInfo, 
        	call(PredId, ProcId, ArgVars, B, C, D, E) - GoalInfo) :-
	(
		map__search(ProcCallInfo, proc(PredId0, ProcId0),
				call_info(NewPredId, NewProcId, UnusedArgs))
	->
		Changed = yes,
		remove_listof_elements(ArgVars0, 1, UnusedArgs, ArgVars),
		PredId = NewPredId,
		ProcId = NewProcId
	;
		Changed = no,
		PredId = PredId0,
		ProcId = ProcId0,
		ArgVars = ArgVars0
	). 

fixup_goal_expr(UnusedVars, _ProcCallInfo, Changed, GoalExpr0 - GoalInfo, 
							GoalExpr - GoalInfo) :-
	GoalExpr0 = unify(Var, Rhs, Mode, Unify0, Context),
	(
		fixup_unify(UnusedVars, Changed0, Unify0, Unify)
	->
		GoalExpr = unify(Var, Rhs, Mode, Unify, Context),
		Changed = Changed0 
	;
		GoalExpr = conj([]),
		Changed = yes
	).

fixup_goal_expr(_UnusedVars, _ProcCallInfo, no, GoalExpr - GoalInfo, 
							GoalExpr - GoalInfo) :-
        GoalExpr = pragma_c_code(_, _, _, _, _).


	% Remove useless unifications from a list of conjuncts.
:- pred fixup_conjuncts(list(var)::in, proc_call_info::in, bool::in,
			bool::out, hlds__goals::in, hlds__goals::out) is det. 

fixup_conjuncts(_, _, Changed, Changed, [], []).
fixup_conjuncts(UnusedVars, ProcCallInfo, Changed0, Changed,
					[Goal0 | Goals0], Goals) :-
	fixup_goal(UnusedVars, ProcCallInfo, LocalChanged, Goal0, Goal),
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
	fixup_conjuncts(UnusedVars, ProcCallInfo, Changed1, Changed,
								Goals0, Goals1).
	

	% We can't remove unused goals from the list of disjuncts as we do
	% for conjuncts, since that would change the determinism of
	% the goal.
:- pred fixup_disjuncts(list(var)::in, proc_call_info::in, bool::in,
			bool::out, hlds__goals::in, hlds__goals::out) is det. 

fixup_disjuncts(_, _, Changed, Changed, [], []).
fixup_disjuncts(UnusedVars, ProcCallInfo, Changed0, Changed,
					[Goal0 | Goals0], [Goal | Goals]) :-
	fixup_goal(UnusedVars, ProcCallInfo, LocalChanged, Goal0, Goal),
	(
		LocalChanged = yes
	->
		Changed1 = yes
	;
		Changed1 = Changed0
	),
	fixup_disjuncts(UnusedVars, ProcCallInfo, Changed1, Changed,
								Goals0, Goals).


:- pred fixup_cases(list(var)::in, proc_call_info::in, bool::in,
			bool::out, list(case)::in, list(case)::out) is det.
		
fixup_cases(_, _, Changed, Changed, [], []).
fixup_cases(UnusedVars, ProcCallInfo, Changed0, Changed, 
		[case(ConsId, Goal0) | Cases0], [case(ConsId, Goal) | Cases]) :-
	fixup_goal(UnusedVars, ProcCallInfo, LocalChanged, Goal0, Goal),
	(	
		LocalChanged = yes
	->
		Changed1 = yes
	;
		Changed1 = Changed0
	),
	fixup_cases(UnusedVars, ProcCallInfo, Changed1, Changed, Cases0, Cases).


		% fix up a unification, fail if the unification is no
		%	longer needed
:- pred fixup_unify(list(var)::in, bool::out, unification::in,
						unification::out) is semidet.

	% a simple test doesn't have any unused vars to fixup
fixup_unify(_UnusedVars, no, simple_test(A, B), simple_test(A, B)).

	% Var1 unused => we don't need the assignment
	% Var2 unused => Var1 unused
fixup_unify(UnusedVars, no, assign(Var1, Var2), assign(Var1, Var2)) :-
	\+ list__member(Var1, UnusedVars).

	% LVar unused => we don't need the unification
fixup_unify(UnusedVars, no, construct(LVar, ConsId, ArgVars, ArgModes),
				construct(LVar, ConsId, ArgVars, ArgModes)) :-	
	\+ list__member(LVar, UnusedVars).
	
fixup_unify(UnusedVars, Changed,
		deconstruct(LVar, ConsId, ArgVars, ArgModes, CanFail),
		deconstruct(LVar, ConsId, ArgVars, ArgModes, CanFail)) :-
	\+ list__member(LVar, UnusedVars),
	(
			% are any of the args unused, if so we need to 	
			% to fix up the goal_info
		CanFail = cannot_fail,
		check_deconstruct_args(UnusedVars, ArgVars, Changed, no)
	;
		CanFail = can_fail,
		Changed = no
	).

fixup_unify(_, _, complicated_unify(_, _, _), _) :-
		error("unused_args:fixup_goal : complicated unify").

		
	% Remove unused vars from the instmap_delta, quantification fixes
	%	up the rest.
:- pred fixup_goal_info(list(var)::in, hlds__goal_info::in,
						hlds__goal_info::out) is det.


	% Check if any of the arguments of a deconstruction are unused, if
	% so Changed will be yes and quantification will be rerun. Fails if
	% none of the arguments are used.
:- pred check_deconstruct_args(list(var)::in, list(var)::in,
						bool::out, bool::in) is semidet.
						
check_deconstruct_args(_, [], no, yes).
check_deconstruct_args(UnusedVars, [ArgVar | ArgVars], Changed, Used) :-
	(
		list__member(ArgVar, UnusedVars)
	->
		check_deconstruct_args(UnusedVars, ArgVars, _, Used),
		Changed = yes
	;
		check_deconstruct_args(UnusedVars, ArgVars, Changed, yes)
	).


fixup_goal_info(UnusedVars, GoalInfo0, GoalInfo) :-
	goal_info_get_instmap_delta(GoalInfo0, InstMap0),
	(
		InstMap0 = reachable(InstMapping0),
		map__delete_list(InstMapping0, UnusedVars, InstMapping),
		InstMap = reachable(InstMapping)
	;
		InstMap0 = unreachable,
		InstMap = unreachable
	),
	goal_info_set_instmap_delta(GoalInfo0, InstMap, GoalInfo).

%-------------------------------------------------------------------------------
	% Warn about unused arguments in each predicate. Only arguments unused
	% in every mode of a predicate are warned about. The warning is
	% suppressed for type_infos.

:- pred report_unused_args(list(warning_info)::in,
				io__state::di, io__state::uo) is det.

report_unused_args([]) --> [].
report_unused_args([warning_info(Context, Name, Arity, UnusedArgs)
							| Rest]) --> 
	{ list__length(UnusedArgs, NumArgs) },
	(
		{ NumArgs = 0 }
	->
		[]
	;
		prog_out__write_context(Context),
		io__write_string("In `"),
		io__write_string(Name), 
		io__write_string(("'/")),
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
		)
	),
	report_unused_args(Rest).


		% Except for type_infos, all args that are unused
		% in one mode of a predicate should be unused in all of the
		% modes of a predicate, so we only need to put out one warning
		% for each predicate.
:- pred create_warning_info(module_info::in, unused_arg_info::in,
	pred_proc_list::in, map(pred_id, warning_info)::in,
	map(pred_id, warning_info)::out) is det.

create_warning_info(_, _, [], Warnings, Warnings).
create_warning_info(ModuleInfo, UnusedArgInfo, [proc(PredId, ProcId) | Rest],
					Warnings0, Warnings) :-
(
	map__contains(Warnings0, PredId)	
->
	Warnings1 = Warnings0
;
	(
		map__search(UnusedArgInfo, proc(PredId, ProcId), UnusedArgs0)
	->
		module_info_pred_info(ModuleInfo, PredId, PredInfo),
		pred_info_name(PredInfo, Name),
		(
			(
				% Don't warn about builtins, i.e. index/2
				% that have unused arguments.
				code_util__predinfo_is_builtin(ModuleInfo,
								PredInfo)
			;
				code_util__compiler_generated(PredInfo)
			;
				% Don't warn about lambda expressions not
				% using arguments. (The warning message for
				% these doesn't contain context, so it's 
				% useless)
				string__sub_string_search(Name,
						"__LambdaGoal__", _)
			;
				% don't warn for a specialized version
				% **** Remove this when the warning section gets
				%	moved into the semantic checking phase.
				string__sub_string_search(Name, "__ho",
								Position),
				string__length(Name, Length),
				IdLen is Length - Position - 4,
				string__right(Name, IdLen, Id),
				string__to_int(Id, _)
			)
		->	
			Warnings1 = Warnings0
		;
			pred_info_procedures(PredInfo, Procs),
			map__lookup(Procs, ProcId, Proc),
			proc_info_headvars(Proc, HeadVars),
			list__length(HeadVars, NumHeadVars),

	       		% Strip off the extra type_info arguments inserted at
			% the front by polymorphism.m
			pred_info_arity(PredInfo, Arity),
			NumToDrop is NumHeadVars - Arity,
			adjust_unused_args(NumToDrop, UnusedArgs0, UnusedArgs),
			pred_info_context(PredInfo, Context),
			(
				UnusedArgs = []
			->
				Warnings1 = Warnings0
			;
				map__det_insert(Warnings0, PredId,
					warning_info(Context, Name,
						Arity, UnusedArgs), Warnings1)
			)
		)
	;
		Warnings1 = Warnings0
	)	
),
create_warning_info(ModuleInfo, UnusedArgInfo, Rest, Warnings1, Warnings).


	% adjust warning message for the presence of type_infos.
:- pred adjust_unused_args(int::in, list(int)::in,  list(int)::out) is det.

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
