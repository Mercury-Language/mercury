%-----------------------------------------------------------------------------%
% Copyright (C) 1999 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: ml_code_gen.m
% Main author: fjh

% MLDS code generation -- convert from HLDS to MLDS.

% This module is an alternative to the original code generator.
% The original code generator compiles from HLDS to LLDS, generating
% very low-level code.  This code generator instead compiles to MLDS,
% generating much higher-level code than the original code generator.

% For nondeterministic predicates, we generate code using an explicit
% continuation passing style.  Each nondeterministic predicate gets
% translated into a function which takes an extra parameter which is a
% function pointer that points to the success continuation.  On success,
% the function calls its success continuation, and on failure it returns.

% To keep things easy, this pass generates code which may contain nested
% functions; if the target language doesn't support nested functions (or
% doesn't support them _efficiently_) then a later MLDS->MLDS simplification
% pass will convert it to a form that does not use nested functions.

% Note that when we take the address of a nested function, we only ever
% do two things with it: pass it as a continuation argument, or call it.
% The continuations are never returned and never stored inside heap objects
% or global variables.  These conditions are sufficient to ensure that
% we never keep the address of a nested function after the containing
% functions has returned, so we won't get any dangling continuations.

%-----------------------------------------------------------------------------%
% CODE GENERATION SUMMARY
%-----------------------------------------------------------------------------%
%
% The calling convention for sub-goals is as follows.
%
%	model_det goal:
%		On success, fall through.
%		(May clobber `succeeded'.)
%	model_semi goal:
%		On success, set `succeeded' to TRUE and fall through.
%		On failure, set `succeeded' to FALSE and fall through.
%	multi/nondet goal:
%		On success, call the current success continuation.
%		On failure, fall through.
%		(May clobber `succeeded' in either case.)
%
% In comments, we use the following notation to distinguish between
% these three.
%
%	model_det goal:
%		<do Goal>
%			This means execute Goal (which must be model_det).
%	model_semi goal:
%		<succeeded = Goal>
%			This means execute Goal, and set `succeeded' to
%			TRUE if the goal succeeds and FALSE if it fails.
%	model_non goal:
%		<Goal && CONT()>
%			This means execute Goal, calling the success
%			continuation function CONT() if it succeeds,
%			and falling through if it fails.
%
% The notation 
%
%	[situation]:
%		<[construct]>
%	===>
%		[code]
%
% means that in the situation described by [situation],
% for the the specified [construct] we will generate the specified [code].

%-----------------------------------------------------------------------------%
%
% Code for wrapping goals
%
%	If a model_foo goal occurs in a model_bar context, where foo != bar,
%	then we need to modify the code that we emit for the goal so that
%	it conforms to the calling convenion expected for model_bar.

%	det goal in semidet context:
%		<succeeded = Goal>
%	===>
%	{
%		bool succeeded;
%
%		<do Goal>
%		succeeded = TRUE
%	}

%	det goal in nondet context:
%		<Goal && SUCCEED()>
%	===>
%		<do Goal>
%		SUCCEED()

%	semi goal in nondet context:
%		<Goal && SUCCEED()>
%	===>
%	{
%		bool succeeded;
%	
%		<succeeded = Goal>
%		if (succeeded) SUCCEED()
%	}

%-----------------------------------------------------------------------------%
%
% Code for commits
%

% There's several different ways of handling commits:
%	- using catch/throw
%	- using setjmp/longjmp
%	- exiting nested functions via gotos to
%	  their containing functions
%
% The MLDS data structure abstracts away these differences
% using the `try_commit' and `do_commit' instructions.
% The comments below show the MLDS try_commit/do_commit version first,
% but for clarity I've also included sample code using each of the three
% different techniques.
%
% If those methods turn out to be too inefficient,
% another alternative would be to change the generated
% code so that after every function call, it would check a flag,
% and if that flag was set, it would return.
% Then MR_DO_COMMIT would just set the flag and return.
% The flag could be in a global (or thread-local) variable,
% or it could be an additional value returned from each function.

%	model_non in semi context: (using try_commit/do_commit)
%		<succeeded = Goal>
% 	===>
%		bool succeeded;
%		MR_COMMIT_TYPE ref;
%		void success() {
%			succeeded = TRUE;
%			MR_DO_COMMIT(ref);
%		}
%		MR_TRY_COMMIT(ref, {
%			<Goal && success()>
%			succeeded = FALSE;
%		}, {
%			succeeded = TRUE;
%		})
%
%	done:

%	model_non in semi context: (using catch/throw)
%		<succeeded = Goal>
% 	===>
%		bool succeeded;
%		void success() {
%			throw COMMIT;
%		}
%		try {
%			<Goal && success()>
%			succeeded = FALSE;
%		} catch (COMMIT) {
%			succeeded = TRUE;
%		}

%	model_non in semi context: (using setjmp/longjmp)
%		<succeeded = Goal>
% 	===>
%		bool succeeded;
%		jmp_buf buf;
%		void success() {
%			longjmp(buf, 1);
%		}
%		if (setjmp(buf)) {
%			succeeded = TRUE;
%		} else {
%			<Goal && success()>
%			succeeded = FALSE;
%		}

%	model_non in semi context: (using GNU C nested functions,
%				GNU C local labels, and exiting
%				the nested function by a goto
%				to a label in the containing function)
%		<succeeded = Goal>
% 	===>
%		bool succeeded;
%		__label__ commit;
%		void success() {
%			goto commit;
%		}
%		<Goal && success()>
%		succeeded = FALSE;
%		goto commit_done;
%	commit:
%		succeeded = TRUE;
%	commit_done:
%		;

%	model_non in det context: (using try_commit/do_commit)
%		<do Goal>
%	===>
%		MR_COMMIT_TYPE ref;
%		void success() {
%			MR_DO_COMMIT(ref);
%		}
%		MR_TRY_COMMIT(ref, {
%			<Goal && success()>
%		}, {})

%	model_non in det context (using GNU C nested functions,
%				GNU C local labels, and exiting
%				the nested function by a goto
%				to a label in the containing function)
%		<do Goal>
%	===>
%		__label__ done;
%		void success() {
%			goto done;
%		}
%		try {
%			<Goal && success()>
%		} catch (COMMIT) {}
%	done:	;

%	model_non in det context (using catch/throw):
%		<do Goal>
%	===>
%		void success() {
%			throw COMMIT;
%		}
%		try {
%			<Goal && success()>
%		} catch (COMMIT) {}

%	model_non in det context (using setjmp/longjmp):
%		<do Goal>
% 	===>
%		jmp_buf buf;
%		void success() {
%			longjmp(buf, TRUE);
%		}
%		if (setjmp(buf) == 0) {
%			<Goal && success()>
%		}

%-----------------------------------------------------------------------------%
%
% Code for empty conjunctions (`true')
%

%	model_det goal:
%		<do true>
%	===>
%		/* fall through */

%	model_semi goal:
%		<succeeded = true>
%	===>
%		succceeded = TRUE;

%	model_non goal
%		<true && CONT()>
%	===>
%		CONT();

%-----------------------------------------------------------------------------%
%
% Code for non-empty conjunctions
%

%	model_det Goal:
%		<Goal, Goals>
% 	===>
%		<do Goal>
%		<Goals>
%	

%	model_semi Goal:
%		<Goal, Goals>
% 	===>
%	{
%		bool succeeded;
%
%		<succeeded = Goal>;
%		if (succeeded) {
%			<Goals>;
%		}
%	}

%	model_non Goal (optimized for readability)
%		<Goal, Goals>
% 	===>
%	{
%		entry_func() {
%			<Goal && succ_func()>;
%		}
%		succ_func() {
%			<Goals && SUCCEED()>;
%		}
%
%		entry_func();
%	}
%
%	model_non Goal (optimized for efficiency):
%		<Goal, Goals>
% 	===>
%	{
%		succ_func() {
%			<Goals && SUCCEED()>;
%		}
%
%		<Goal && succ_func()>;
%	}

%	model_non goals (optimized for readability):
%		<Goal1, Goal2, Goal3, Goals>
% 	===>
%	{
%		label0_func() {
%			<Goal1 && label1_func()>;
%		}
%		label1_func() {
%			<Goal2 && label2_func()>;
%		}
%		label2_func() {
%			<Goal3 && label3_func()>;
%		}
%		label3_func() {
%			<Goals && SUCCEED()>;
%		}
%
%		label0_func();
%	}

%	model_non goals (optimized for efficiency):
%		<Goal1, Goal2, Goal3, Goals>
% 	===>
%	{
%		label3_func() {
%			<Goals && SUCCEED()>;
%		}
%		label2_func() {
%			<Goal3 && label3_func()>;
%		}
%		label1_func() {
%			<Goal2 && label2_func()>;
%		}
%
%		<Goal1 && label1_func()>;
%	}

%-----------------------------------------------------------------------------%
%
% Code for empty disjunctions (`fail')
%

%	model_semi goal:
%		<succeeded = fail>
%	===>
%		succeeded = FALSE;

%	model_non goal:
%		<fail && CONT()>
%	===>
%		/* fall through */

%-----------------------------------------------------------------------------%
%
% Code for non-empty disjunctions
%

% model_det disj:

%	model_det Goal:
%		<do (Goal ; Goals)>
%	===>
%		<do Goal>
%		/* <Goals> will never be reached */

%	model_semi Goal:
%		<do (Goal ; Goals)>
%	===>
%	{
%		bool succeeded;
%	
%		<succeeded = Goal>;
%		if (!succeeded) {
%			<do Goals>;
%		}
%	}

% model_semi disj:

%	model_det Goal:
%		<succeeded = (Goal ; Goals)>
%	===>
%	{
%		bool succeeded;
%
%		<do Goal>
%		succeeded = TRUE
%		/* <Goals> will never be reached */
%	}

%	model_semi Goal:
%		<succeeded = (Goal ; Goals)>
%	===>
%	{
%		bool succeeded;
%
%		<succeeded = Goal>;
%		if (!succeeded) {
%			<succeeded = Goals>;
%		}

% model_non disj:
%
%	model_det Goal:
%		<(Goal ; Goals) && SUCCEED()>
%	===>
%		<Goal>
%		SUCCEED();
%		<Goals && SUCCEED()>
%
%	model_semi Goal:
%		<(Goal ; Goals) && SUCCEED()>
%	===>
%	{
%		bool succeeded;
%	
%		<succeeded = Goal>
%		if (succeeded) SUCCEED();
%		<Goals && SUCCEED()>
%	}
%
%	model_non Goal:
%		<(Goal ; Goals) && SUCCEED()>
%	===>
%		<Goal && SUCCEED()>
%		<Goals && SUCCEED()>

%-----------------------------------------------------------------------------%
%
% Code for if-then-else
%

%	model_semi Cond:
%		<(Cond -> Then ; Else)>
%	===>
%	{
%		bool succeeded;
%	
%		<succeeded = Cond>
%		if (succeeded) {
%			<Then>
%		} else {
%			<Else>
%		}
%	}

%	/*
%	** XXX The following transformation does not do as good a job of GC
%	**     as it could.  Ideally we ought to ensure that stuff used only
%	**     in the `Else' part will be reclaimed if a GC occurs during
%	**     the `Then' part.  But that is a bit tricky to achieve.
%	*/
%
%	model_non Cond:
%		<(Cond -> Then ; Else)>
%	===>
%	{
%		bool succeeded;
%
%		void then_func() {
%			succeeded = TRUE;
%			<Then>
%		}
%
%		succeeded = FALSE;
%		<Cond && then_func()>
%		if (!succeeded) {
%			<Else>
%		}
%	}

%-----------------------------------------------------------------------------%
%
% Code for negation
%

% model_det negation
%		<not(Goal)>
%	===>
%	{
%		bool succeeded;
%		<succeeded = Goal>
%		/* now ignore the value of succeeded,
%		   which we know will be FALSE */
%	}

% model_semi negation, model_det Goal:
%		<succeeded = not(Goal)>
%	===>
%	{
%		bool succeeded;
%		<succeeded = Goal>
%		succeeded = FALSE;
%	}

% model_semi negation, model_semi Goal:
%		<succeeded = not(Goal)>
%	===>
%	{
%		bool succeeded;
%		<succeeded = Goal>
%		succeeded = !succeeded;
%	}

%-----------------------------------------------------------------------------%
%
% Code for deconstruction unifications
%

%	det (cannot_fail) deconstruction:
%		<succeeded = (X => f(A1, A2, ...))>
% 	===>
%		A1 = arg(X, f, 1);		% extract arguments
%		A2 = arg(X, f, 2);
%		...

%	semidet (can_fail) deconstruction:
%		<X => f(A1, A2, ...)>
% 	===>
%		<succeeded = (X => f(_, _, _, _))>	% tag test
%		if (succeeded) {
%			A1 = arg(X, f, 1);		% extract arguments
%			A2 = arg(X, f, 2);
%			...
%		}

%-----------------------------------------------------------------------------%


% XXX This is still very incomplete!!!
%
% Done:
%	- function prototypes
%	- code generation for det, semidet, and nondet predicates:
%		- conjunctions
%		- disjunctions
%		- negation
%		- if-then-else
%		- predicate/function calls
%		- higher-order calls
%		- unifications
%			- assignment
%			- simple tests
%			- constructions
%			- deconstructions
%		- switches
%		- commits
% TODO:
%	- type_infos
%	- c_code pragmas
%	- typeclass_infos and class method calls
%	- type declarations for user-defined types
%	...
%
% POTENTIAL EFFICIENCY IMPROVEMENTS:
%	- generate local declarations for the `succeeded' variable;
%	  this would help in nondet code, because it would avoid
%	  the need to access the outermost function's `succeeded'
%	  variable via the environment pointer

%-----------------------------------------------------------------------------%

:- module ml_code_gen.

:- interface.

:- import_module hlds_module, hlds_pred, mlds.
:- import_module io.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Generate MLDS code for an entire module.
	%
:- pred ml_code_gen(module_info, mlds, io__state, io__state).
:- mode ml_code_gen(in, out, di, uo) is det.

	% Generate the mlds__pred_label and module name
	% for a given procedure.
	%
:- pred ml_gen_pred_label(module_info, pred_id, proc_id,
		mlds__pred_label, mlds_module_name).
:- mode ml_gen_pred_label(in, in, in, out, out) is det.

	% Generate the function prototype for a given procedure.
	%
:- func ml_gen_proc_params(module_info, pred_id, proc_id) = mlds__func_params.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module ml_base_type_info.
:- import_module llds. % XXX needed for `code_model'.
:- import_module export, llds_out. % XXX needed for pragma C code
:- import_module code_util. % XXX needed for `code_util__compiler_generated'.
			    % and `code_util__cons_id_to_tag'.
:- import_module goal_util, instmap, inst_table.
:- import_module hlds_pred, hlds_goal, hlds_data, prog_data, special_pred.
:- import_module hlds_out, builtin_ops, passes_aux, type_util, mode_util.
:- import_module prog_util.
:- import_module globals, options.

:- import_module string, int, bool, varset, term.
:- import_module list, map, set, stack.
:- import_module require, std_util.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Generate MLDS code for an entire module.
	%
ml_code_gen(ModuleInfo, MLDS) -->
	{ module_info_name(ModuleInfo, ModuleName) },
	ml_gen_foreign_code(ModuleInfo, ForeignCode),
	{ ml_gen_imports(ModuleInfo, Imports) },
	ml_gen_defns(ModuleInfo, Defns),
	{ MLDS = mlds(ModuleName, ForeignCode, Imports, Defns) }.

:- pred ml_gen_foreign_code(module_info, mlds__foreign_code,
				io__state, io__state).
:- mode ml_gen_foreign_code(in, out, di, uo) is det.

ml_gen_foreign_code(ModuleInfo, MLDS_ForeignCode) -->
	{ module_info_get_c_header(ModuleInfo, C_Header_Info) },
	{ module_info_get_c_body_code(ModuleInfo, C_Body_Info) },
	{ ConvBody = (func(S - C) = user_c_code(S, C)) },
	{ User_C_Code = list__map(ConvBody, C_Body_Info) },
	%
	% XXX not yet implemented -- this is just a stub
	%
	{ C_Exports = [] },
	{ MLDS_ForeignCode = mlds__foreign_code(C_Header_Info, User_C_Code,
			C_Exports) }.

:- pred ml_gen_imports(module_info, mlds__imports).
:- mode ml_gen_imports(in, out) is det.

ml_gen_imports(ModuleInfo, MLDS_ImportList) :-
	module_info_get_imported_module_specifiers(ModuleInfo, ImportSet),
	set__to_sorted_list(ImportSet, ImportList),
	MLDS_ImportList = list__map(mercury_module_name_to_mlds, ImportList).

:- pred ml_gen_defns(module_info, mlds__defns, io__state, io__state).
:- mode ml_gen_defns(in, out, di, uo) is det.

ml_gen_defns(ModuleInfo, MLDS_Defns) -->
	ml_gen_types(ModuleInfo, MLDS_TypeDefns),
	ml_gen_preds(ModuleInfo, MLDS_PredDefns),
	{ MLDS_Defns = list__append(MLDS_TypeDefns, MLDS_PredDefns) }.

%-----------------------------------------------------------------------------%

	% Generate MLDS definitions for all the types,
	% typeclasses, and instances in the HLDS.
	%
:- pred ml_gen_types(module_info, mlds__defns, io__state, io__state).
:- mode ml_gen_types(in, out, di, uo) is det.

ml_gen_types(ModuleInfo, MLDS_BaseTypeInfoDefns) -->
	{ ml_base_type_info__generate_mlds(ModuleInfo, MLDS_BaseTypeInfoDefns) }.

%-----------------------------------------------------------------------------%
%
% Stuff to generate MLDS code for HLDS predicates & functions.
%

	% Generate MLDS definitions for all the non-imported
	% predicates (and functions) in the HLDS.
	%
:- pred ml_gen_preds(module_info, mlds__defns, io__state, io__state).
:- mode ml_gen_preds(in, out, di, uo) is det.

ml_gen_preds(ModuleInfo, MLDS_PredDefns) -->
	{ module_info_preds(ModuleInfo, PredTable) },
	{ map__keys(PredTable, PredIds) },
	{ MLDS_PredDefns0 = [] },
	ml_gen_preds_2(ModuleInfo, PredIds, PredTable,
		MLDS_PredDefns0, MLDS_PredDefns).

:- pred ml_gen_preds_2(module_info, list(pred_id), pred_table,
			mlds__defns, mlds__defns, io__state, io__state).
:- mode ml_gen_preds_2(in, in, in, in, out, di, uo) is det.

ml_gen_preds_2(ModuleInfo, PredIds0, PredTable, MLDS_Defns0, MLDS_Defns) --> 
	(
		{ PredIds0 = [PredId|PredIds] }
	->
		{ map__lookup(PredTable, PredId, PredInfo) },
		( { pred_info_is_imported(PredInfo) } ->
			{ MLDS_Defns1 = MLDS_Defns0 }
		;
			ml_gen_pred(ModuleInfo, PredId, PredInfo,
				MLDS_Defns0, MLDS_Defns1)
		),
		ml_gen_preds_2(ModuleInfo, PredIds, PredTable,
			MLDS_Defns1, MLDS_Defns)
	;
		{ MLDS_Defns = MLDS_Defns0 }
	).

	% Generate MLDS definitions for all the non-imported
	% procedures of a given predicate (or function).
	%
:- pred ml_gen_pred(module_info, pred_id, pred_info,
				mlds__defns, mlds__defns, io__state, io__state).
:- mode ml_gen_pred(in, in, in, in, out, di, uo) is det.

ml_gen_pred(ModuleInfo, PredId, PredInfo, MLDS_Defns0, MLDS_Defns) -->
	{ pred_info_non_imported_procids(PredInfo, ProcIds) },
	( { ProcIds = [] } ->
		{ MLDS_Defns = MLDS_Defns0 }
	;
		write_pred_progress_message("% Generating MLDS code for ",
			PredId, ModuleInfo),
		{ pred_info_procedures(PredInfo, ProcTable) },
		{ ml_gen_procs(ProcIds, ModuleInfo, PredId, PredInfo,
				ProcTable, MLDS_Defns0, MLDS_Defns) }
	).

:- pred ml_gen_procs(list(proc_id), module_info, pred_id, pred_info,
			proc_table, mlds__defns, mlds__defns).
:- mode ml_gen_procs(in, in, in, in, in, in, out) is det.

ml_gen_procs([], _, _, _, _) --> [].
ml_gen_procs([ProcId | ProcIds], ModuleInfo, PredId, PredInfo, ProcTable)
		--> 
	{ map__lookup(ProcTable, ProcId, ProcInfo) },
	ml_gen_proc(ModuleInfo, PredId, ProcId, PredInfo, ProcInfo),
	ml_gen_procs(ProcIds, ModuleInfo, PredId, PredInfo, ProcTable).

%-----------------------------------------------------------------------------%
%
% Code for handling individual procedures
%

	% Generate MLDS code for the specified procedure.
	%
:- pred ml_gen_proc(module_info, pred_id, proc_id, pred_info, proc_info,
			mlds__defns, mlds__defns).
:- mode ml_gen_proc(in, in, in, in, in, in, out) is det.

ml_gen_proc(ModuleInfo, PredId, ProcId, _PredInfo, ProcInfo, Defns0, Defns) :-
	proc_info_context(ProcInfo, Context),

	MLDS_Name = ml_gen_proc_label(ModuleInfo, PredId, ProcId),
	MLDS_Context = mlds__make_context(Context),
	MLDS_DeclFlags = ml_gen_proc_decl_flags(ModuleInfo, PredId, ProcId),
	ml_gen_proc_defn(ModuleInfo, PredId, ProcId,
		MLDS_ProcDefnBody, ExtraDefns),
	MLDS_ProcDefn = mlds__defn(MLDS_Name, MLDS_Context, MLDS_DeclFlags,
				MLDS_ProcDefnBody),
	Defns = list__append(ExtraDefns, [MLDS_ProcDefn | Defns0]).

	% Return the declaration flags appropriate for a procedure definition.
	%
:- func ml_gen_proc_decl_flags(module_info, pred_id, proc_id)
		= mlds__decl_flags.
ml_gen_proc_decl_flags(ModuleInfo, PredId, ProcId) = MLDS_DeclFlags :-
	module_info_pred_info(ModuleInfo, PredId, PredInfo),
	( procedure_is_exported(PredInfo, ProcId) ->
		Access = public
	;
		Access = private
	),
	PerInstance = per_instance,
	Virtuality = non_virtual,
	Finality = overridable,
	Constness = modifiable,
	Abstractness = concrete,
	MLDS_DeclFlags = init_decl_flags(Access, PerInstance,
		Virtuality, Finality, Constness, Abstractness).

	% Generate an MLDS definition for the specified procedure.
	%
:- pred ml_gen_proc_defn(module_info, pred_id, proc_id, mlds__entity_defn,
		mlds__defns).
:- mode ml_gen_proc_defn(in, in, in, out, out) is det.

ml_gen_proc_defn(ModuleInfo, PredId, ProcId, MLDS_ProcDefnBody, ExtraDefns) :-
	module_info_pred_proc_info(ModuleInfo, PredId, ProcId,
			_PredInfo, ProcInfo),
	proc_info_interface_code_model(ProcInfo, CodeModel),
	proc_info_goal(ProcInfo, Goal0),

	%
	% The HLDS front-end sometimes over-estimates
	% the set of non-locals.  We need to restrict
	% the set of non-locals for the top-level goal
	% to just the headvars, because otherwise variables
	% which occur in the top-level non-locals but which
	% are not really non-local will not be declared.
	%
	proc_info_headvars(ProcInfo, HeadVars),
	Goal0 = GoalExpr - GoalInfo0,
	goal_info_get_nonlocals(GoalInfo0, NonLocals0),
	set__list_to_set(HeadVars, HeadVarsSet),
	set__intersect(HeadVarsSet, NonLocals0, NonLocals),
	goal_info_set_nonlocals(GoalInfo0, NonLocals, GoalInfo),
	Goal = GoalExpr - GoalInfo,

	goal_info_get_context(GoalInfo, Context),

	MLDSGenInfo0 = ml_gen_info_init(ModuleInfo, PredId, ProcId),
	MLDS_Params = ml_gen_proc_params(ModuleInfo, PredId, ProcId),
	( CodeModel = model_non ->
		% set up the initial success continuation
		ml_initial_cont(InitialCont, MLDSGenInfo0, MLDSGenInfo1),
		ml_gen_info_push_success_cont(InitialCont,
			MLDSGenInfo1, MLDSGenInfo2)
	;
		MLDSGenInfo2 = MLDSGenInfo0
	),
	% This would generate all the local variables at the top of the
	% function:
	%	proc_info_varset(ProcInfo, VarSet),
	%	proc_info_vartypes(ProcInfo, VarTypes),
	%	proc_info_headvars(ProcInfo, HeadVars),
	%	MLDS_LocalVars = ml_gen_all_local_var_decls(Goal, VarSet,
	% 		VarTypes, HeadVars),
	% But instead we now generate them locally for each goal.
	% We just declare the `succeeded' var here.
	MLDS_Context = mlds__make_context(Context),
	MLDS_LocalVars = [ml_gen_succeeded_var_decl(MLDS_Context)],
	ml_gen_proc_body(CodeModel, Goal, MLDS_Decls0, MLDS_Statements,
			MLDSGenInfo2, MLDSGenInfo),
	ml_gen_info_get_extra_defns(MLDSGenInfo, ExtraDefns),
	MLDS_Decls = list__append(MLDS_LocalVars, MLDS_Decls0),
	MLDS_Statement = ml_gen_block(MLDS_Decls, MLDS_Statements, Context),
	MLDS_ProcDefnBody = mlds__function(yes(proc(PredId, ProcId)),
			MLDS_Params, yes(MLDS_Statement)).

	% Generate MLDS definitions for all the local variables in a function.
	%
	% Note that this function generates all the local variables at the
	% top of the function.  It might be a better idea to instead
	% generate local declarations for all the variables used in
	% each sub-goal.
	%
:- func ml_gen_all_local_var_decls(hlds_goal, prog_varset,
		map(prog_var, prog_type), list(prog_var)) = mlds__defns.
ml_gen_all_local_var_decls(Goal, VarSet, VarTypes, HeadVars) =
		MLDS_LocalVars :-
	Goal = _ - GoalInfo,
	goal_info_get_context(GoalInfo, Context),
	goal_util__goal_vars(Goal, AllVarsSet),
	set__delete_list(AllVarsSet, HeadVars, LocalVarsSet),
	set__to_sorted_list(LocalVarsSet, LocalVars),
	MLDS_Context = mlds__make_context(Context),
	MLDS_LocalVars0 = ml_gen_local_var_decls(VarSet, VarTypes,
				MLDS_Context, LocalVars),
	MLDS_SucceededVar = ml_gen_succeeded_var_decl(MLDS_Context),
	MLDS_LocalVars = [MLDS_SucceededVar | MLDS_LocalVars0].

	% Generate declarations for a list of local variables.
	%
:- func ml_gen_local_var_decls(prog_varset, map(prog_var, prog_type),
		mlds__context, prog_vars) = mlds__defns.
ml_gen_local_var_decls(VarSet, VarTypes, Context, Vars) =
	list__map(ml_gen_local_var_decl(VarSet, VarTypes, Context), Vars).

	% Generate a declaration for a local variable.
	%
:- func ml_gen_local_var_decl(prog_varset, map(prog_var, prog_type),
		mlds__context, prog_var) = mlds__defn.
ml_gen_local_var_decl(VarSet, VarTypes, Context, Var) = MLDS_Defn :-
	VarName = ml_gen_var_name(VarSet, Var),
	map__lookup(VarTypes, Var, Type),
	MLDS_Defn = ml_gen_var_decl(VarName, Type, Context).

:- func ml_gen_var_decls(list(var_name), list(prog_type), mlds__context) =
	mlds__defns.
ml_gen_var_decls([], [], _) = [].
ml_gen_var_decls([_|_], [], _) = _ :-
	error("ml_gen_var_decls: length mismatch").
ml_gen_var_decls([], [_|_], _) = _ :-
	error("ml_gen_var_decls: length mismatch").
ml_gen_var_decls([Name|Names], [Type|Types], Context) = [Defn|Defns] :-
	Defn = ml_gen_var_decl(Name, Type, Context),
	Defns = ml_gen_var_decls(Names, Types, Context).

:- func ml_gen_var_decl(var_name, prog_type, mlds__context) = mlds__defn.
ml_gen_var_decl(VarName, Type, Context) =
	ml_gen_mlds_var_decl(VarName, mercury_type_to_mlds_type(Type),
		Context).

:- func ml_gen_mlds_var_decl(var_name, mlds__type, mlds__context) = mlds__defn.
ml_gen_mlds_var_decl(VarName, MLDS_Type, Context) = MLDS_Defn :-
	Name = data(var(VarName)),
	MaybeInitializer = no,
	Defn = data(MLDS_Type, MaybeInitializer),
	DeclFlags = ml_gen_var_decl_flags,
	MLDS_Defn = mlds__defn(Name, Context, DeclFlags, Defn).

:- func ml_gen_var_names(prog_varset, list(prog_var)) = list(string).
ml_gen_var_names(VarSet, Vars) = list__map(ml_gen_var_name(VarSet), Vars).

:- func ml_gen_var_name(prog_varset, prog_var) = string.
ml_gen_var_name(VarSet, Var) = UniqueVarName :-
	varset__lookup_name(VarSet, Var, VarName),
	term__var_to_int(Var, VarNumber),
	string__format("%s_%d", [s(VarName), i(VarNumber)], UniqueVarName).

	% Generate the declaration for the built-in `succeeded' variable.
	%
:- func ml_gen_succeeded_var_decl(mlds__context) = mlds__defn.
ml_gen_succeeded_var_decl(Context) =
	ml_gen_mlds_var_decl("succeeded", mlds__bool_type, Context).

	% Generate the code for a procedure body.
	%
:- pred ml_gen_proc_body(code_model, hlds_goal, mlds__defns, mlds__statements,
			ml_gen_info, ml_gen_info).
:- mode ml_gen_proc_body(in, in, out, out, in, out) is det.

ml_gen_proc_body(CodeModel, Goal, MLDS_Decls, MLDS_Statements) -->
	%
	% First just generate the code for the procedure's goal.
	%
	ml_gen_goal(CodeModel, Goal, MLDS_Decls, MLDS_Statements0),
	%
	% Then append an appropriate `return' statement, if needed.
	%
	( { CodeModel = model_semi } ->
		ml_gen_test_success(Succeeded),
		{ ReturnStmt = return([Succeeded]) },
		{ Goal = _ - GoalInfo },
		{ goal_info_get_context(GoalInfo, Context) },
		{ ReturnStatement = mlds__statement(ReturnStmt,
			mlds__make_context(Context)) },
		{ MLDS_Statements = list__append(MLDS_Statements0,
			[ReturnStatement]) }
	;
		{ MLDS_Statements = MLDS_Statements0 }
	).

%-----------------------------------------------------------------------------%
%
% Stuff to generate code for goals.
%

	% Generate MLDS code for the specified goal in the
	% specified code model.  Return the result as a single statement
	% (which may be a block statement containing nested declarations).
	%
:- pred ml_gen_goal(code_model, hlds_goal, mlds__statement,
			ml_gen_info, ml_gen_info).
:- mode ml_gen_goal(in, in, out, in, out) is det.

ml_gen_goal(CodeModel, Goal, MLDS_Statement) -->
	ml_gen_goal(CodeModel, Goal, MLDS_Decls, MLDS_Statements),
	{ Goal = _ - GoalInfo },
	{ goal_info_get_context(GoalInfo, Context) },
	{ MLDS_Statement = ml_gen_block(MLDS_Decls, MLDS_Statements,
		Context) }.

	% Generate MLDS code for the specified goal in the
	% specified code model.  Return the result as two lists,
	% one containing the necessary declarations and the other
	% containing the generated statements.
	%
:- pred ml_gen_goal(code_model, hlds_goal, mlds__defns, mlds__statements,
			ml_gen_info, ml_gen_info).
:- mode ml_gen_goal(in, in, out, out, in, out) is det.

ml_gen_goal(CodeModel, Goal, MLDS_Decls, MLDS_Statements) -->
	{ Goal = GoalExpr - GoalInfo },
	%
	% Generate the local variables for this goal.
	% We need to declare any variables which
	% are local to this goal (including its subgoals),
	% but which are not local to a subgoal.
	% (If they're local to a subgoal, they'll be declared
	% when we generate code for that subgoal.)

	{ Locals = goal_local_vars(Goal) },
	{ SubGoalLocals = union_of_direct_subgoal_locals(Goal) },
	{ set__difference(Locals, SubGoalLocals, VarsToDeclareHere) },
	{ set__to_sorted_list(VarsToDeclareHere, VarsList) },
	=(MLDSGenInfo),
	{ ml_gen_info_get_varset(MLDSGenInfo, VarSet) },
	{ ml_gen_info_get_var_types(MLDSGenInfo, VarTypes) },
	{ VarDecls = ml_gen_local_var_decls(VarSet, VarTypes,
		mlds__make_context(Context), VarsList) },

	%
	% Generate code for the goal in its own code model.
	%
	{ goal_info_get_context(GoalInfo, Context) },
	{ goal_info_get_code_model(GoalInfo, GoalCodeModel) },
	ml_gen_goal_expr(GoalExpr, GoalCodeModel, Context,
		GoalDecls, GoalStatements0),

	%
	% Add whatever wrapper is needed to convert the goal's
	% code model to the desired code model.
	%
	ml_gen_wrap_goal(CodeModel, GoalCodeModel, Context,
		GoalStatements0, GoalStatements),
	
	{ ml_join_decls(VarDecls, [], GoalDecls, GoalStatements, Context,
		MLDS_Decls, MLDS_Statements) }.

	% Return the set of variables which occur in the specified goal
	% (including in its subgoals) and which are local to that goal.
:- func goal_local_vars(hlds_goal) = set(prog_var).
goal_local_vars(Goal) = LocalVars :-
	% find all the variables in the goal
	goal_util__goal_vars(Goal, GoalVars),
	% delete the non-locals
	Goal = _ - GoalInfo,
	goal_info_get_nonlocals(GoalInfo, NonLocalVars),
	set__difference(GoalVars, NonLocalVars, LocalVars).

:- func union_of_direct_subgoal_locals(hlds_goal) = set(prog_var).

union_of_direct_subgoal_locals(Goal - _GoalInfo) =
	promise_only_solution((pred(UnionOfSubGoalLocals::out) is cc_multi :-
		set__init(EmptySet),
		unsorted_aggregate(direct_subgoal(Goal),
			union_subgoal_locals, EmptySet, UnionOfSubGoalLocals)
	)).

:- pred union_subgoal_locals(hlds_goal, set(prog_var), set(prog_var)).
:- mode union_subgoal_locals(in, in, out) is det.

union_subgoal_locals(SubGoal, UnionOfSubGoalLocals0, UnionOfSubGoalLocals) :-
	SubGoalLocals = goal_local_vars(SubGoal),
	set__union(UnionOfSubGoalLocals0, SubGoalLocals, UnionOfSubGoalLocals).

	% ml_gen_wrap_goal(OuterCodeModel, InnerCodeModel, Context,
	%		MLDS_Statements0, MLDS_Statements):
	%
	%	OuterCodeModel is the code model expected by the
	%	context in which a goal is called. InnerCodeModel
	%	is the code model which the goal actually has.
	%	This predicate converts the code generated for
	%	the goal using InnerCodeModel into code that uses
	%	the calling convention appropriate for OuterCodeModel.
	%
:- pred ml_gen_wrap_goal(code_model, code_model, prog_context,
		mlds__statements, mlds__statements,
		ml_gen_info, ml_gen_info).
:- mode ml_gen_wrap_goal(in, in, in, in, out, in, out) is det.

	% If the inner and outer code models are equal,
	% we don't need to do anything special.

ml_gen_wrap_goal(model_det, model_det, _,
		MLDS_Statements, MLDS_Statements) --> [].
ml_gen_wrap_goal(model_semi, model_semi, _,
		MLDS_Statements, MLDS_Statements) --> [].
ml_gen_wrap_goal(model_non, model_non, _,
		MLDS_Statements, MLDS_Statements) --> [].

	% If the inner code model is more precise than the outer code
	% model, then we need to append some statements to convert
	% the calling convention for the inner code model to that of
	% the outer code model.

ml_gen_wrap_goal(model_semi, model_det, Context,
		MLDS_Statements0, MLDS_Statements) -->
	%
	% det goal in semidet context:
	%	<succeeded = Goal>
	% ===>
	% {
	%	bool succeeded;
	%
	%	<do Goal>
	%	succeeded = TRUE
	% }
	%
	ml_gen_set_success(const(true), Context, SetSuccessTrue),
	{ MLDS_Statements = list__append(MLDS_Statements0, [SetSuccessTrue]) }.

ml_gen_wrap_goal(model_non, model_det, Context,
		MLDS_Statements0, MLDS_Statements) -->
	%
	% det goal in nondet context:
	%	<Goal && SUCCEED()>
	% ===>
	%	<do Goal>
	%	SUCCEED()
	%
	ml_gen_call_current_success_cont(Context, CallCont),
	{ MLDS_Statements = list__append(MLDS_Statements0, [CallCont]) }.

ml_gen_wrap_goal(model_non, model_semi, Context,
		MLDS_Statements0, MLDS_Statements) -->
	%
	% semi goal in nondet context:
	%	<Goal && SUCCEED()>
	% ===>
	% {
	%	bool succeeded;
	%
	%	<succeeded = Goal>
	%	if (succeeded) SUCCEED()
	% }
	%
	ml_gen_test_success(Succeeded),
	ml_gen_call_current_success_cont(Context, CallCont),
	{ IfStmt = if_then_else(Succeeded, CallCont, no) },
	{ IfStatement = mlds__statement(IfStmt, mlds__make_context(Context)) },
	{ MLDS_Statements = list__append(MLDS_Statements0, [IfStatement]) }.

	% If the inner code model is less precise than the outer code model,
	% then simplify.m is supposed to wrap the goal inside a `some'
	% to indicate that a commit is needed.

ml_gen_wrap_goal(model_det, model_semi, _, _, _) -->
	{ error("ml_gen_wrap_goal: code model mismatch -- semi in det") }.
ml_gen_wrap_goal(model_det, model_non, _, _, _) -->
	{ error("ml_gen_wrap_goal: code model mismatch -- nondet in det") }.
ml_gen_wrap_goal(model_semi, model_non, _, _, _) -->
	{ error("ml_gen_wrap_goal: code model mismatch -- nondet in semi") }.

	% Generate code for a commit.
	%
:- pred ml_gen_commit(hlds_goal, code_model, prog_context,
			mlds__defns, mlds__statements,
			ml_gen_info, ml_gen_info).
:- mode ml_gen_commit(in, in, in, out, out, in, out) is det.

ml_gen_commit(Goal, CodeModel, Context, MLDS_Decls, MLDS_Statements) -->
	{ Goal = _ - GoalInfo },
	{ goal_info_get_code_model(GoalInfo, GoalCodeModel) },

	( { GoalCodeModel = model_non, CodeModel = model_semi } ->

		%	model_non in semi context: (using try_commit/do_commit)
		%		<succeeded = Goal>
		% 	===>
		%		bool succeeded;
		%		MR_COMMIT_TYPE ref;
		%		void success() {
		%			succeeded = TRUE;
		%			MR_DO_COMMIT(ref);
		%		}
		%		MR_TRY_COMMIT(ref, {
		%			<Goal && success()>
		%			succeeded = FALSE;
		%		}, {
		%			succeeded = TRUE;
		%		})

		% generate the `success()' function
		ml_gen_new_func_label(SuccessFuncLabel, SuccessFuncLabelRval),
		/* push nesting level */
		{ MLDS_Context = mlds__make_context(Context) },
		ml_gen_info_new_commit_label(CommitLabelNum),
		{ string__format("commit_%d", [i(CommitLabelNum)],
			CommitRef) },
		ml_qualify_var(CommitRef, CommitRefLval),
		{ CommitRefDecl = ml_gen_commit_var_decl(MLDS_Context,
			CommitRef) },
		{ DoCommitStmt = do_commit(lval(CommitRefLval)) },
		{ DoCommitStatement = mlds__statement(DoCommitStmt,
			MLDS_Context) },
		/* pop nesting level */
		ml_gen_nondet_label_func(SuccessFuncLabel, Context,
			DoCommitStatement, SuccessFunc),

		ml_get_env_ptr(EnvPtrRval),
		{ SuccessCont = success_cont(SuccessFuncLabelRval,
			EnvPtrRval) },
		ml_gen_info_push_success_cont(SuccessCont),
		ml_gen_goal(model_non, Goal, GoalStatement),
		ml_gen_info_pop_success_cont,
		ml_gen_set_success(const(false), Context, SetSuccessFalse),
		ml_gen_set_success(const(true), Context, SetSuccessTrue),
		{ TryCommitStmt = try_commit(CommitRefLval,
			ml_gen_block([], [GoalStatement, SetSuccessFalse],
				Context),
			SetSuccessTrue) },
		{ TryCommitStatement = mlds__statement(TryCommitStmt,
			MLDS_Context) },

		{ MLDS_Decls = [CommitRefDecl, SuccessFunc] },
		{ MLDS_Statements = [TryCommitStatement] }

	; { GoalCodeModel = model_non, CodeModel = model_det } ->

		%	model_non in det context: (using try_commit/do_commit)
		%		<do Goal>
		%	===>
		%		MR_COMMIT_TYPE ref;
		%		void success() {
		%			MR_DO_COMMIT(ref);
		%		}
		%		MR_TRY_COMMIT(ref, {
		%			<Goal && success()>
		%		}, {})

		% generate the `success()' function
		ml_gen_new_func_label(SuccessFuncLabel, SuccessFuncLabelRval),
		/* push nesting level */
		{ MLDS_Context = mlds__make_context(Context) },
		ml_gen_info_new_commit_label(CommitLabelNum),
		{ string__format("commit_%d", [i(CommitLabelNum)],
			CommitRef) },
		ml_qualify_var(CommitRef, CommitRefLval),
		{ CommitRefDecl = ml_gen_commit_var_decl(MLDS_Context,
			CommitRef) },
		{ DoCommitStmt = do_commit(lval(CommitRefLval)) },
		{ DoCommitStatement = mlds__statement(DoCommitStmt,
			MLDS_Context) },
		/* pop nesting level */
		ml_gen_nondet_label_func(SuccessFuncLabel, Context,
			DoCommitStatement, SuccessFunc),

		ml_get_env_ptr(EnvPtrRval),
		{ SuccessCont = success_cont(SuccessFuncLabelRval,
			EnvPtrRval) },
		ml_gen_info_push_success_cont(SuccessCont),
		ml_gen_goal(model_non, Goal, GoalStatement),
		ml_gen_info_pop_success_cont,

		{ TryCommitStmt = try_commit(CommitRefLval, GoalStatement,
			ml_gen_block([], [], Context)) },
		{ TryCommitStatement = mlds__statement(TryCommitStmt,
			MLDS_Context) },

		{ MLDS_Decls = [CommitRefDecl, SuccessFunc] },
		{ MLDS_Statements = [TryCommitStatement] }
	;
		% no commit required
		ml_gen_goal(CodeModel, Goal, MLDS_Decls, MLDS_Statements)
	).

	% Generate the declaration for the `commit' variable.
	%
:- func ml_gen_commit_var_decl(mlds__context, mlds__var_name) = mlds__defn.
ml_gen_commit_var_decl(Context, VarName) =
	ml_gen_mlds_var_decl(VarName, mlds__commit_type, Context).

	% Qualify the name of the specified commit var.
	%
:- pred ml_qualify_var(mlds__var_name, mlds__lval,
		ml_gen_info, ml_gen_info).
:- mode ml_qualify_var(in, out, in, out) is det.
ml_qualify_var(CommitRef, CommitLval) -->
	=(MLDSGenInfo),
	{ ml_gen_info_get_module_name(MLDSGenInfo, ModuleName) },
	{ MLDS_Module = mercury_module_name_to_mlds(ModuleName) },
	{ CommitLval = var(qual(MLDS_Module, CommitRef)) }.

	% Generate MLDS code for the different kinds of HLDS goals.
	%
:- pred ml_gen_goal_expr(hlds_goal_expr, code_model, prog_context,
			mlds__defns, mlds__statements,
			ml_gen_info, ml_gen_info).
:- mode ml_gen_goal_expr(in, in, in, out, out, in, out) is det.

ml_gen_goal_expr(switch(Var, CanFail, CasesList, _), CodeModel, Context,
		MLDS_Decls, MLDS_Statements) -->
	ml_gen_switch(Var, CanFail, CasesList, CodeModel, Context,
		MLDS_Decls, MLDS_Statements).

ml_gen_goal_expr(some(_Vars, _CanRemove, Goal), CodeModel, Context,
		MLDS_Decls, MLDS_Statements) -->
	ml_gen_commit(Goal, CodeModel, Context, MLDS_Decls, MLDS_Statements).

ml_gen_goal_expr(if_then_else(_Vars, Cond, Then, Else, _),
		CodeModel, Context, MLDS_Decls, MLDS_Statements) -->
	ml_gen_ite(CodeModel, Cond, Then, Else, Context,
			MLDS_Decls, MLDS_Statements).

ml_gen_goal_expr(not(Goal), CodeModel, Context,
		MLDS_Decls, MLDS_Statements) -->
	ml_gen_negation(Goal, CodeModel, Context, MLDS_Decls, MLDS_Statements).

ml_gen_goal_expr(conj(Goals), CodeModel, Context,
		MLDS_Decls, MLDS_Statements) -->
	ml_gen_conj(Goals, CodeModel, Context, MLDS_Decls, MLDS_Statements).

ml_gen_goal_expr(disj(Goals, _), CodeModel, Context,
		MLDS_Decls, MLDS_Statements) -->
	ml_gen_disj(Goals, CodeModel, Context, MLDS_Decls, MLDS_Statements).

ml_gen_goal_expr(par_conj(Goals, _SM), CodeModel, Context,
		MLDS_Decls, MLDS_Statements) -->
	%
	% XXX currently we treat parallel conjunction the same as
	%     sequential conjunction -- parallelism is not yet implemented
	%
	ml_gen_conj(Goals, CodeModel, Context, MLDS_Decls, MLDS_Statements).

ml_gen_goal_expr(generic_call(GenericCall, Vars, Modes, Detism), CodeModel,
		Context, MLDS_Decls, MLDS_Statements) -->
	{ determinism_to_code_model(Detism, CallCodeModel) },
	{ require(unify(CodeModel, CallCodeModel),
		"ml_gen_generic_call: code model mismatch") },
	ml_gen_generic_call(GenericCall, Vars, Modes, CodeModel, Context,
		MLDS_Decls, MLDS_Statements).

ml_gen_goal_expr(call(PredId, ProcId, ArgVars, BuiltinState, _, _PredName),
		CodeModel, Context, MLDS_Decls, MLDS_Statements) -->
	(
		{ BuiltinState = not_builtin }
	->
		ml_gen_var_list(ArgVars, ArgLvals),
		=(MLDSGenInfo),
		{ ml_gen_info_get_varset(MLDSGenInfo, VarSet) },
		{ ArgNames = ml_gen_var_names(VarSet, ArgVars) },
		ml_variable_types(ArgVars, ActualArgTypes),
		ml_gen_call(PredId, ProcId, ArgNames, ArgLvals, ActualArgTypes,
			CodeModel, Context, MLDS_Decls, MLDS_Statements)
	;
		ml_gen_builtin(PredId, ProcId, ArgVars, CodeModel, Context,
			MLDS_Decls, MLDS_Statements)
	).

ml_gen_goal_expr(unify(_A, _B, _, Unification, _), CodeModel, Context,
		MLDS_Decls, MLDS_Statements) -->
	ml_gen_unification(Unification, CodeModel, Context,
		MLDS_Decls, MLDS_Statements).

ml_gen_goal_expr(pragma_c_code(Attributes,
                PredId, ProcId, ArgVars, ArgDatas, OrigArgTypes, PragmaImpl),
		CodeModel, OuterContext, MLDS_Decls, MLDS_Statements) -->
        (
                { PragmaImpl = ordinary(C_Code, _MaybeContext) },
                ml_gen_ordinary_pragma_c_code(CodeModel, Attributes,
                        PredId, ProcId, ArgVars, ArgDatas, OrigArgTypes,
                        C_Code, OuterContext, MLDS_Decls, MLDS_Statements)
        ;
                { PragmaImpl = nondet( _, _, _, _, _, _, _, _, _) },
		{ sorry("nondet pragma c_code") }
		/*
                { PragmaImpl = nondet(
                        Fields, FieldsContext, First, FirstContext,
                        Later, LaterContext, Treat, Shared, SharedContext) },
                ml_gen_nondet_pragma_c_code(CodeModel, Attributes,
                        PredId, ProcId, ArgVars, ArgDatas, OrigArgTypes,
                        Fields, FieldsContext, First, FirstContext,
                        Later, LaterContext, Treat, Shared, SharedContext,
                        MLDS_Decls, MLDS_Statements)
		*/
        ).

ml_gen_goal_expr(bi_implication(_, _), _, _, _, _) -->
	% these should have been expanded out by now
	{ error("ml_gen_goal_expr: unexpected bi_implication") }.

:- pred ml_gen_ordinary_pragma_c_code(code_model, pragma_c_code_attributes,
		pred_id, proc_id, list(prog_var), pragma_c_code_arg_info,
		list(prog_type), string, prog_context,
		mlds__defns, mlds__statements, ml_gen_info, ml_gen_info).
:- mode ml_gen_ordinary_pragma_c_code(in, in, in, in, in, in, 
		in, in, in, out, out, in, out) is det.

	% We generate code of the following form:
	%
	% model_det pragma_c_code:
	%
	%	{
	% 		<declaration of one local variable for each arg>
	%
	%		<assign input args>
	%		<obtain global lock>
	%		<c code>
	%		<release global lock>
	%		<assign output args>
	%	}
	%		
	% model_semi pragma_c_code:
	%
	%	{
	% 		<declaration of one local variable for each arg>
	%		#define SUCCESS_INDICATOR <succeeded>
	%
	%		<assign input args>
	%		<obtain global lock>
	%		<c code>
	%		<release global lock>
	%		if (SUCCESS_INDICATOR) {
	%			<assign output args>
	%		}
	%	}
	%		
	% Note that we generate this code directly as
	% `target_code(lang_C, <string>)' instructions in the MLDS.
	% It would probably be nicer to encode more of the structure
	% in the MLDS, so that (a) we could do better MLDS optimization
	% and (b) so that the generation of C code strings could be
	% isolated in mlds_to_c.m.  Also we will need to do something
	% different for targets other than C, e.g. when compiling to
	% Java.
	%
ml_gen_ordinary_pragma_c_code(CodeModel, Attributes,
		PredId, _ProcId, ArgVars, ArgDatas0, OrigArgTypes,
		C_Code, Context, MLDS_Decls, MLDS_Statements) -->
	{ ArgDatas0 = pragma_c_code_arg_info(InstTable, ArgDatas) },

	%
	% Combine all the information about the each arg
	%
	{ ml_make_c_arg_list(ArgVars, ArgDatas, OrigArgTypes,
		ArgList) },

	%
	% Generate <declaration of one local variable for each arg>
	%
	{ ml_gen_pragma_c_decls(ArgList, ArgDeclsList) },

	% The modes of the C code should not depend on the
	% aliasing information in the instmap.
	{ instmap__init_reachable(InstMap) },

	%
	% Generate code to set the values of the input variables.
	%
	list__map_foldl(ml_gen_pragma_c_input_arg(InstMap, InstTable),
		ArgList, AssignInputsList),

	%
	% Generate code to assign the values of the output variables.
	%
	list__map_foldl(ml_gen_pragma_c_output_arg(InstMap, InstTable),
		ArgList, AssignOutputsList),

	%
	% Generate code fragments to obtain and release the global lock
	% (this is used for ensuring thread safety in a concurrent
	% implementation)
	% XXX we should only generate these if the `parallel' option
	% was enabled
	%
	=(MLDSGenInfo),
	{ thread_safe(Attributes, ThreadSafe) },
	{ ThreadSafe = thread_safe ->
		ObtainLock = "",
		ReleaseLock = ""
	;
		ml_gen_info_get_module_info(MLDSGenInfo, ModuleInfo),
		module_info_pred_info(ModuleInfo, PredId, PredInfo),
		pred_info_name(PredInfo, Name),
		llds_out__quote_c_string(Name, MangledName),
		string__append_list(["\tMR_OBTAIN_GLOBAL_LOCK(""",
			MangledName, """);\n"], ObtainLock),
		string__append_list(["\tMR_RELEASE_GLOBAL_LOCK(""",
			MangledName, """);\n"], ReleaseLock)
	},

	%
	% Put it all together
	%
	{ string__append_list(ArgDeclsList, ArgDecls) },
	{ string__append_list(AssignInputsList, AssignInputsCode) },
	{ string__append_list(AssignOutputsList, AssignOutputsCode) },
	( { CodeModel = model_det } ->
		{ string__append_list([
				"{\n",
				ArgDecls,
				"\n",
				AssignInputsCode,
				ObtainLock,
				"\t\t{\n",
				C_Code,
				"\n\t\t}\n",
				ReleaseLock,
				AssignOutputsCode,
				"}\n"],
			Combined_C_Code) }
	; { CodeModel = model_semi } ->
		ml_success_lval(SucceededLval),
		{ ml_gen_c_code_for_rval(lval(SucceededLval), SucceededVar) },
		{ DefineSuccessIndicator = string__format(
			"#define SUCCESS_INDICATOR = %s\n",
			[s(SucceededVar)]) },
		{ MaybeAssignOutputsCode = string__format(
			"\tif (SUCCESS_INDICATOR) {\n%s\n\t}",
			[s(AssignOutputsCode)]) },
		{ UndefSuccessIndicator = "#undef SUCCESS_INDICATOR" },
		{ string__append_list([
				"{\n",
				ArgDecls,
				DefineSuccessIndicator,
				"\n",
				AssignInputsCode,
				ObtainLock,
				"\t\t{\n",
				C_Code,
				"\n\t\t}\n",
				ReleaseLock,
				MaybeAssignOutputsCode,
				UndefSuccessIndicator,
				"}\n"],
			Combined_C_Code) }
	;
		{ error("ml_gen_ordinary_pragma_c_code: unexpected code model") }
	),
	{ C_Code_Stmt = target_code(lang_C, Combined_C_Code) },
	{ C_Code_Statement = mlds__statement(atomic(C_Code_Stmt),
		mlds__make_context(Context)) },
	{ MLDS_Statements = [C_Code_Statement] },
	{ MLDS_Decls = [] }.

%---------------------------------------------------------------------------%

%
% we gather all the information about each pragma_c argument
% together into this struct
%

:- type ml_c_arg
	--->	ml_c_arg(
			prog_var,
			maybe(pair(string, mode)),	% name and mode
			prog_type	% original type before
					% inlining/specialization
					% (the actual type may be an instance
					% of this type, if this type is
					% polymorphic).
		).

:- pred ml_make_c_arg_list(list(prog_var)::in,
		list(maybe(pair(string, mode)))::in, list(prog_type)::in,
		list(ml_c_arg)::out) is det.

ml_make_c_arg_list(Vars, ArgDatas, Types, ArgList) :-
	( Vars = [], ArgDatas = [], Types = [] ->
		ArgList = []
	; Vars = [V|Vs], ArgDatas = [N|Ns], Types = [T|Ts] ->
		Arg = ml_c_arg(V, N, T),
		ml_make_c_arg_list(Vs, Ns, Ts, Args),
		ArgList = [Arg | Args]
	;
		error("ml_code_gen:make_c_arg_list - length mismatch")
	).

%---------------------------------------------------------------------------%

% ml_gen_pragma_c_decls generates C code to declare the arguments
% for a `pragma c_code' declaration.
%
:- pred ml_gen_pragma_c_decls(list(ml_c_arg)::in, list(string)::out) is det.

ml_gen_pragma_c_decls([], []).
ml_gen_pragma_c_decls([Arg|Args], [Decl|Decls]) :-
	ml_gen_pragma_c_decl(Arg, Decl),
	ml_gen_pragma_c_decls(Args, Decls).

% ml_gen_pragma_c_decl generates C code to declare an argument
% of a `pragma c_code' declaration.
%
:- pred ml_gen_pragma_c_decl(ml_c_arg::in, string::out) is det.

ml_gen_pragma_c_decl(ml_c_arg(_Var, MaybeNameAndMode, Type), DeclString) :-
	(
		MaybeNameAndMode = yes(ArgName - _Mode),
		\+ var_is_singleton(ArgName)
	->
		export__type_to_type_string(Type, TypeString),
		string__format("\t%s %s;\n", [s(TypeString), s(ArgName)],
			DeclString)
	;
		% if the variable doesn't occur in the ArgNames list,
		% it can't be used, so we just ignore it
		DeclString = ""
	).

%-----------------------------------------------------------------------------%

% var_is_singleton determines whether or not a given pragma_c variable
% is singleton (i.e. starts with an underscore)
%
% Singleton vars should be ignored when generating the declarations for
% pragma_c arguments because:
%
%	- they should not appear in the C code
% 	- they could clash with the system name space
%
:- pred var_is_singleton(string) is semidet.
:- mode var_is_singleton(in) is semidet.

var_is_singleton(Name) :-
	string__first_char(Name, '_', _).

%-----------------------------------------------------------------------------%

% ml_gen_pragma_c_input_arg generates C code to assign the value of an input
% arg for a `pragma c_code' declaration.
%
:- pred ml_gen_pragma_c_input_arg(instmap::in, inst_table::in,
		ml_c_arg::in, string::out,
		ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_pragma_c_input_arg(InstMap, InstTable,
		ml_c_arg(Var, MaybeNameAndMode, OrigType),
		AssignInputString) -->
	=(MLDSGenInfo),
	{ ml_gen_info_get_module_info(MLDSGenInfo, ModuleInfo) },
	(
		{ MaybeNameAndMode = yes(ArgName - Mode) },
		{ \+ var_is_singleton(ArgName) },
		{ mode_to_arg_mode(InstMap, InstTable, ModuleInfo,
			Mode, OrigType, top_in) }
	->
		ml_variable_type(Var, VarType),
		ml_gen_var(Var, VarLval),
		{ type_util__is_dummy_argument_type(VarType) ->
			% The variable may not have been declared,
			% so we need to generate a dummy value for it.
			% Using `0' here is more efficient than
			% using private_builtin__dummy_var, which is
			% what ml_gen_var will have generated for this
			% variable.
			Var_ArgName = "0"
		;
			ml_gen_box_or_unbox_rval(VarType, OrigType, lval(VarLval),
				ArgRval),
			ml_gen_c_code_for_rval(ArgRval, Var_ArgName)
		},
		{ string__format("\t%s = %s;\n", [s(ArgName), s(Var_ArgName)],
			AssignInputString) }
	;
		% if the variable doesn't occur in the ArgNames list,
		% it can't be used, so we just ignore it
		{ AssignInputString = "" }
	).

% ml_gen_pragma_c_output_arg generates C code to assign the value of an output
% arg for a `pragma c_code' declaration.
%
:- pred ml_gen_pragma_c_output_arg(instmap::in, inst_table::in,
		ml_c_arg::in, string::out,
		ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_pragma_c_output_arg(InstMap, InstTable,
		ml_c_arg(Var, MaybeNameAndMode, OrigType),
		AssignOutputString) -->
	=(MLDSGenInfo),
	{ ml_gen_info_get_module_info(MLDSGenInfo, ModuleInfo) },
	(
		{ MaybeNameAndMode = yes(ArgName - Mode) },
		{ \+ var_is_singleton(ArgName) },
		{ \+ type_util__is_dummy_argument_type(OrigType) },
		{ mode_to_arg_mode(InstMap, InstTable, ModuleInfo,
			Mode, OrigType, top_out) }
	->
		ml_variable_type(Var, VarType),
		ml_gen_var(Var, VarLval),
		{ ml_gen_box_or_unbox_rval(OrigType, VarType, lval(VarLval),
			ArgRval) },
		{ ml_gen_c_code_for_rval(ArgRval, Var_ArgName) },
		{ string__format("\t%s = %s;\n", [s(Var_ArgName), s(ArgName)],
			AssignOutputString) }
	;
		% if the variable doesn't occur in the ArgNames list,
		% it can't be used, so we just ignore it
		{ AssignOutputString = "" }
	).

	%
	% XXX this is a bit of a hack --
	% for `pragma c_code', we generate the C code for an mlds__rval
	% directly rather than going via the MLDS
	%
:- pred ml_gen_c_code_for_rval(mlds__rval::in, string::out) is det.
ml_gen_c_code_for_rval(ArgRval, Var_ArgName) :-
	( ArgRval = lval(var(qual(ModuleName, VarName))) ->
		SymName = mlds_module_name_to_sym_name(ModuleName),
		llds_out__sym_name_mangle(SymName, MangledModuleName),
		llds_out__name_mangle(VarName, MangledVarName),
		string__append_list([MangledModuleName, "__",
			MangledVarName], Var_ArgName)
	; ArgRval = lval(mem_ref(lval(var(qual(ModuleName, VarName))))) ->
		SymName = mlds_module_name_to_sym_name(ModuleName),
		llds_out__sym_name_mangle(SymName, MangledModuleName),
		llds_out__name_mangle(VarName, MangledVarName),
		string__append_list(["*", MangledModuleName, "__",
			MangledVarName], Var_ArgName)
	;
		sorry("complicated pragma c_code")
	).

%-----------------------------------------------------------------------------%
%
% Code for procedure calls
%

:- pred ml_gen_generic_call(generic_call, list(prog_var), argument_modes,
		code_model, prog_context, mlds__defns, mlds__statements,
		ml_gen_info, ml_gen_info).
:- mode ml_gen_generic_call(in, in, in, in, in, out, out, in, out) is det.

ml_gen_generic_call(GenericCall, ArgVars, Modes, CodeModel, Context,
		MLDS_Decls, MLDS_Statements) -->
	%
	% allocate some fresh type variables to use as the Mercury types
	% of the boxed arguments
	%
	{ NumArgs = list__length(ArgVars) },
	{ varset__init(TypeVarSet0) },
	{ varset__new_vars(TypeVarSet0, NumArgs, ArgTypeVars,
		_TypeVarSet) },
	{ term__var_list_to_term_list(ArgTypeVars, BoxedArgTypes) },

	%
	% create the boxed parameter types for the called function
	%
	=(MLDSGenInfo),
	{ ml_gen_info_get_module_info(MLDSGenInfo, ModuleInfo) },
	{ ml_gen_info_get_varset(MLDSGenInfo, VarSet) },
	{ ml_gen_info_get_instmap(MLDSGenInfo, InstMap) },
	{ ml_gen_info_get_inst_table(MLDSGenInfo, InstTable) },
	{ ArgNames = ml_gen_var_names(VarSet, ArgVars) },
	{ Modes = argument_modes(_, ArgModes) },
	{ Params0 = ml_gen_params(ModuleInfo, ArgNames,
		BoxedArgTypes, ArgModes, InstMap, InstTable, CodeModel) },

	%
	% insert the `closure_arg' parameter
	%
	{ ClosureArgType = mlds__generic_env_ptr_type },
	{ ClosureArg = data(var("closure_arg")) - ClosureArgType },
	{ Params0 = mlds__func_params(ArgParams0, RetParam) },
	{ Params = mlds__func_params([ClosureArg | ArgParams0], RetParam) },
	{ Signature = mlds__get_func_signature(Params) },

	%
	% compute the function address
	%
	(
		{ GenericCall = higher_order(ClosureVar, _PredOrFunc,
			_Arity) },
		ml_gen_var(ClosureVar, ClosureLval),
		{ FieldId = offset(const(int_const(1))) },
		{ FuncLval = field(yes(0), lval(ClosureLval), FieldId) },
		{ FuncType = mlds__func_type(Params) },
		{ FuncRval = unop(unbox(FuncType), lval(FuncLval)) }
	;
		{ GenericCall = class_method(_, _, _, _) },
		{ sorry("type class methods") }
	;
		{ GenericCall = aditi_builtin(_, _) },
		{ sorry("Aditi builtins") }
	),

	%
	% Generate code to box/unbox the arguments
	% and compute the list of properly converted rvals/lvals
	% to pass as the function call's arguments and return values
	%
	ml_gen_var_list(ArgVars, ArgLvals),
	ml_variable_types(ArgVars, ActualArgTypes),
	ml_gen_arg_list(ArgNames, ArgLvals, ActualArgTypes, BoxedArgTypes,
		ArgModes, InstMap, InstTable, Context, InputRvals, OutputLvals,
		ConvArgDecls, ConvOutputStatements),
	{ ClosureRval = unop(unbox(ClosureArgType), lval(ClosureLval)) },

	%
	% Prepare to generate the call, passing the closure as the first
	% argument.
	% (We can't actually generate the call yet, since it might be nondet,
	% and we don't yet know what its success continuation will be;
	% instead for now we just construct a higher-order term `DoGenCall',
	% which when called will generate it.)
	%
	{ ObjectRval = no },
	{ DoGenCall = ml_gen_mlds_call(Signature, ObjectRval, FuncRval,
		[ClosureRval | InputRvals], OutputLvals,
		CodeModel, Context) },

	( { ConvArgDecls = [], ConvOutputStatements = [] } ->
		DoGenCall(MLDS_Decls, MLDS_Statements)
	;
		%
		% Construct a closure to generate code to 
		% convert the output arguments and then succeed
		%
		{ DoGenConvOutputAndSucceed = (
			pred(COAS_Decls::out, COAS_Statements::out, in, out)
			is det -->
				{ COAS_Decls = [] },
				ml_gen_success(CodeModel, Context,
					SucceedStmts),
				{ COAS_Statements = list__append(
					ConvOutputStatements, SucceedStmts) }
		) },

		%
		% Conjoin the code generated by the two closures that we
		% computed above.  `ml_combine_conj' will generate whatever
		% kind of sequence is necessary for this code model.
		%
		ml_combine_conj(CodeModel, Context,
			DoGenCall, DoGenConvOutputAndSucceed,
			CallAndConvOutputDecls, CallAndConvOutputStatements),
		{ MLDS_Decls = list__append(ConvArgDecls,
			CallAndConvOutputDecls) },
		{ MLDS_Statements = CallAndConvOutputStatements }
	).

	%
	% Generate code for a procedure call, making sure to
	% box/unbox the arguments if necessary.
	%
:- pred ml_gen_call(pred_id, proc_id, list(var_name), list(mlds__lval),
		list(prog_data__type), code_model, prog_context,
		mlds__defns, mlds__statements, ml_gen_info, ml_gen_info).
:- mode ml_gen_call(in, in, in, in, in, in, in, out, out, in, out) is det.

	%
	% Generate code for the various parts that are needed for
	% a procedure call: declarations of variables needed for
	% boxing/unboxing output arguments,
	% a closure to generate code to call the function
	% with the input arguments appropriate boxed,
	% and code to unbox/box the return values.
	%
	% For example, if the callee is declared as
	%
	%	:- some [T2]
	%	   pred callee(float::in, T1::in, float::out, T2::out, ...).
	%
	% then for a call `callee(Arg1, Arg2, Arg3, Arg4, ...)'
	% with arguments of types `U1, float, U2, float, ...',
	% we generate the following fragments:
	%
	% 	/* declarations of variables needed for boxing/unboxing */
	%	Float conv_Arg3;
	%	MR_Box conv_Arg4;
	%	...
	%
	% 	/* code to call the function */
	%	func(unbox(Arg1), box(Arg2), &boxed_Arg3, &unboxed_Arg4);
	%
	%	/* code to box/unbox the output arguments */
	%	*Arg3 = unbox(boxed_Arg3);
	%	*Arg4 = box(unboxed_Arg4);
	%	...
	%
	% Note that of course in general not every argument will need
	% to be boxed/unboxed; for those where no conversion is required,
	% we just pass the original argument unchanged.
	%
ml_gen_call(PredId, ProcId, ArgNames, ArgLvals, ActualArgTypes, CodeModel,
		Context, MLDS_Decls, MLDS_Statements) -->
	%
	% Compute the function signature
	%
	{ Params = ml_gen_proc_params(ModuleInfo, PredId, ProcId) },
	{ Signature = mlds__get_func_signature(Params) },

	%
	% Compute the function address
	%
	ml_gen_proc_addr_rval(PredId, ProcId, FuncRval),

	%
	% Compute the callee's Mercury argument types and modes
	%
	=(MLDSGenInfo),
	{ ml_gen_info_get_module_info(MLDSGenInfo, ModuleInfo) },
	{ module_info_pred_proc_info(ModuleInfo, PredId, ProcId,
		PredInfo, ProcInfo) },
	{ pred_info_arg_types(PredInfo, PredArgTypes) },
	{ proc_info_argmodes(ProcInfo, Modes) },

	%
	% Generate code to box/unbox the arguments
	% and compute the list of properly converted rvals/lvals
	% to pass as the function call's arguments and return values
	%
	{ Modes = argument_modes(InstTable, ArgModes) },
	% The modes of the arguments do not depend on the argument
	% substitutions in the instmap.
	{ instmap__init_reachable(InstMap) },
	ml_gen_arg_list(ArgNames, ArgLvals, ActualArgTypes, PredArgTypes,
		ArgModes, InstMap, InstTable, Context,
		InputRvals, OutputLvals, ConvArgDecls, ConvOutputStatements),

	%
	% Construct a closure to generate the call
	% (We can't actually generate the call yet, since it might be nondet,
	% and we don't yet know what its success continuation will be;
	% that's why for now we just construct a closure `DoGenCall'
	% to generate it.)
	%
	{ ObjectRval = no },
	{ DoGenCall = ml_gen_mlds_call(Signature, ObjectRval, FuncRval,
		InputRvals, OutputLvals, CodeModel, Context) },

	( { ConvArgDecls = [], ConvOutputStatements = [] } ->
		DoGenCall(MLDS_Decls, MLDS_Statements)
	;
		%
		% Construct a closure to generate code to 
		% convert the output arguments and then succeed
		%
		{ DoGenConvOutputAndSucceed = (
			pred(COAS_Decls::out, COAS_Statements::out, in, out)
			is det -->
				{ COAS_Decls = [] },
				ml_gen_success(CodeModel, Context,
					SucceedStmts),
				{ COAS_Statements = list__append(
					ConvOutputStatements, SucceedStmts) }
		) },

		%
		% Conjoin the code generated by the two closures that we
		% computed above.  `ml_combine_conj' will generate whatever
		% kind of sequence is necessary for this code model.
		%
		ml_combine_conj(CodeModel, Context,
			DoGenCall, DoGenConvOutputAndSucceed,
			CallAndConvOutputDecls, CallAndConvOutputStatements),
		{ MLDS_Decls = list__append(ConvArgDecls,
			CallAndConvOutputDecls) },
		{ MLDS_Statements = CallAndConvOutputStatements }
	).

	%
	% This generates a call in the specified code model.
	% This is a lower-level routine called by both ml_gen_call_parts
	% and ml_gen_generic_call.
	%
:- pred ml_gen_mlds_call(mlds__func_signature, maybe(mlds__rval), mlds__rval,
		list(mlds__rval), list(mlds__lval), code_model, prog_context,
		mlds__defns, mlds__statements, ml_gen_info, ml_gen_info).
:- mode ml_gen_mlds_call(in, in, in, in, in, in, in, out, out, in, out) is det.

ml_gen_mlds_call(Signature, ObjectRval, FuncRval, ArgRvals0, RetLvals0,
		CodeModel, Context, MLDS_Decls, MLDS_Statements) -->
	%
	% append the extra argument or return val for this code_model
	%
	(
		{ CodeModel = model_non },
		% pass the current success continuation
		ml_gen_info_current_success_cont(Cont),
		{ Cont = success_cont(FuncPtrRval, EnvPtrRval) },
		ml_gen_info_use_gcc_nested_functions(UseNestedFuncs),
		( { UseNestedFuncs = yes } ->
			{ ArgRvals = list__append(ArgRvals0, [FuncPtrRval]) }
		;
			{ ArgRvals = list__append(ArgRvals0,
				[FuncPtrRval, EnvPtrRval]) }
		),
		{ RetLvals = RetLvals0 }
	;
		{ CodeModel = model_semi },
		% return a bool indicating whether or not it succeeded
		ml_success_lval(Success),
		{ ArgRvals = ArgRvals0 },
		{ RetLvals = list__append([Success], RetLvals0) }
	;
		{ CodeModel = model_det },
		{ ArgRvals = ArgRvals0 },
		{ RetLvals = RetLvals0 }
	),

	%
	% build the MLDS call statement
	%
	{ CallOrTailcall = call },
	{ MLDS_Stmt = call(Signature, FuncRval, ObjectRval, ArgRvals, RetLvals,
			CallOrTailcall) },
	{ MLDS_Statement = mlds__statement(MLDS_Stmt,
			mlds__make_context(Context)) },
	{ MLDS_Statements = [MLDS_Statement] },
	{ MLDS_Decls = [] }.

%
% Generate an rval containing the address of the specified procedure
%
:- pred ml_gen_proc_addr_rval(pred_id, proc_id, mlds__rval,
		ml_gen_info, ml_gen_info).
:- mode ml_gen_proc_addr_rval(in, in, out, in, out) is det.

ml_gen_proc_addr_rval(PredId, ProcId, CodeAddrRval) -->
	=(MLDSGenInfo),
	{ ml_gen_info_get_module_info(MLDSGenInfo, ModuleInfo) },
	{ ml_gen_pred_label(ModuleInfo, PredId, ProcId,
		PredLabel, PredModule) },
	{ QualifiedProcLabel = qual(PredModule, PredLabel - ProcId) },
	{ CodeAddrRval = const(code_addr_const(proc(QualifiedProcLabel))) }.

%
% Generate rvals and lvals for the arguments of a procedure call
%
:- pred ml_gen_arg_list(list(var_name), list(mlds__lval), list(prog_type),
		list(prog_type), list(mode), instmap, inst_table,
		prog_context, list(mlds__rval), list(mlds__lval),
		mlds__defns, mlds__statements,
		ml_gen_info, ml_gen_info).
:- mode ml_gen_arg_list(in, in, in, in, in, in, in, in, out, out, out, out,
		in, out) is det.

ml_gen_arg_list(VarNames, VarLvals, CallerTypes, CalleeTypes, Modes,
		InstMap, InstTable, Context, InputRvals,
		OutputLvals, ConvDecls, ConvOutputStatements) -->
	(
		{ VarNames = [] },
		{ VarLvals = [] },
		{ CallerTypes = [] },
		{ CalleeTypes = [] },
		{ Modes = [] }
	->
		{ InputRvals = [] },
		{ OutputLvals = [] },
		{ ConvDecls = [] },
		{ ConvOutputStatements = [] }
	;
		{ VarNames = [VarName | VarNames1] },
		{ VarLvals = [VarLval | VarLvals1] },
		{ CallerTypes = [CallerType | CallerTypes1] },
		{ CalleeTypes = [CalleeType | CalleeTypes1] },
		{ Modes = [Mode | Modes1] }
	->
		ml_gen_arg_list(VarNames1, VarLvals1,
			CallerTypes1, CalleeTypes1, Modes1, InstMap, InstTable,
			Context, InputRvals1, OutputLvals1,
			ConvDecls1, ConvOutputStatements1),
		=(MLDSGenInfo),
		{ ml_gen_info_get_module_info(MLDSGenInfo, ModuleInfo) },
		(
			{ type_util__is_dummy_argument_type(CalleeType) }
		->
			%
			% exclude arguments of type io__state etc.
			%
			{ InputRvals = InputRvals1 },
			{ OutputLvals = OutputLvals1 },
			{ ConvDecls = ConvDecls1 },
			{ ConvOutputStatements = ConvOutputStatements1 }
		;
			{ mode_to_arg_mode(InstMap, InstTable, ModuleInfo,
				Mode, CalleeType, top_in) }
		->
			%
			% it's an input argument
			%
			{ type_util__is_dummy_argument_type(CallerType) ->
				% The variable may not have been declared,
				% so we need to generate a dummy value for it.
				% Using `0' here is more efficient than
				% using private_builtin__dummy_var, which is
				% what ml_gen_var will have generated for this
				% variable.
				VarRval = const(int_const(0))
			;
				VarRval = lval(VarLval)
			},
			{ ml_gen_box_or_unbox_rval(CallerType, CalleeType,
				VarRval, ArgRval) },
			{ InputRvals = [ArgRval | InputRvals1] },
			{ OutputLvals = OutputLvals1 },
			{ ConvDecls = ConvDecls1 },
			{ ConvOutputStatements = ConvOutputStatements1 }
		;
			%
			% it's an output argument
			%
			ml_gen_box_or_unbox_lval(CallerType, CalleeType,
				VarLval, VarName, Context, ArgLval,
				ThisArgConvDecls, ThisArgConvOutput),
			{ ConvDecls = list__append(ThisArgConvDecls,
				ConvDecls1) },
			{ ConvOutputStatements = list__append(
				ThisArgConvOutput, ConvOutputStatements1) },
			(
		/************
				%
				% if the target language allows multiple
				% return values, then use them
				%
				{ UseMultipleOutputs = yes }
			->
				{ InputRvals = InputLvals1 },
				{ OutputLvals = [ArgLval | OutputLvals1] },
			;
		************/
				%
				% otherwise use the traditional C style
				% of passing the address of the output value
				%
				{ InputRvals = [ml_gen_mem_addr(ArgLval)
					| InputRvals1] },
				{ OutputLvals = OutputLvals1 }
			)
		)
	;
		{ error("ml_gen_arg_list: length mismatch") }
	).

	% ml_gen_mem_addr(Lval) returns a value equal to &Lval.
	% For the case where Lval = *Rval, for some Rval,
	% we optimize &*Rval to just Rval.
:- func ml_gen_mem_addr(mlds__lval) = mlds__rval.
ml_gen_mem_addr(Lval) =
	(if Lval = mem_ref(Rval) then Rval else mem_addr(Lval)).

:- pred ml_gen_box_or_unbox_rval(prog_type, prog_type, mlds__rval, mlds__rval).
:- mode ml_gen_box_or_unbox_rval(in, in, in, out) is det.

ml_gen_box_or_unbox_rval(SourceType, DestType, VarRval, ArgRval) :-
	(
		%
		% if converting from polymorphic type to concrete type,
		% then unbox
		%
		SourceType = term__variable(_),
		DestType = term__functor(_, _, _)
	->
		ArgRval = unop(unbox(mercury_type(DestType)), VarRval)
	;
		%
		% if converting from concrete type to polymorphic type,
		% then box
		%
		SourceType = term__functor(_, _, _),
		DestType = term__variable(_)
	->
		ArgRval = unop(box(mercury_type(SourceType)), VarRval)
	;
		%
		% otherwise leave unchanged
		%
		ArgRval = VarRval
	).
	
:- pred ml_gen_box_or_unbox_lval(prog_type, prog_type, mlds__lval, var_name,
		prog_context, mlds__lval, mlds__defns, mlds__statements,
		ml_gen_info, ml_gen_info).
:- mode ml_gen_box_or_unbox_lval(in, in, in, in, in, out, out, out,
		in, out) is det.

ml_gen_box_or_unbox_lval(CallerType, CalleeType, VarLval, VarName, Context,
		ArgLval, ConvDecls, ConvStatements) -->
	%
	% First see if we can just convert the lval as an rval;
	% if no boxing/unboxing is required, then ml_box_or_unbox_rval
	% will return its argument unchanged, and so we're done.
	%
	(
		{ ml_gen_box_or_unbox_rval(CalleeType, CallerType,
			lval(VarLval), lval(VarLval)) }
	->
		{ ArgLval = VarLval },
		{ ConvDecls = [] },
		{ ConvStatements = [] }
	;
		%
		% If that didn't work, then we need to declare a fresh variable
		% to use as the arg, and to generate a statement to box/unbox
		% that fresh arg variable and assign it to the output argument
		% whose address we were passed.
		%

		% generate a declaration for the fresh variable
		{ ArgVarName = string__append("conv_", VarName) },
		{ ArgVarDecl = ml_gen_var_decl(ArgVarName, CalleeType,
			mlds__make_context(Context)) },
		{ ConvDecls = [ArgVarDecl] },

		% create the lval for the variable and use it for the
		% argument lval
		ml_qualify_var(ArgVarName, ArgLval),

		( { type_util__is_dummy_argument_type(CallerType) } ->
			% if it is a dummy argument type (e.g. io__state),
			% then we don't need to bother assigning it
			{ ConvStatements = [] }
		;
			% generate a statement to box/unbox the fresh variable
			% and assign it to the output argument whose address
			% we were passed.  Note that we swap the caller type
			% and the callee type, since this is an output not
			% an input, so the callee type is the source type
			% and the caller type is the destination type.
			{ ml_gen_box_or_unbox_rval(CalleeType, CallerType,
				lval(ArgLval), ConvertedArgRval) },
			{ AssignStmt = assign(VarLval, ConvertedArgRval) },
			{ AssignStatement = mlds__statement(atomic(AssignStmt),
				mlds__make_context(Context)) },
			{ ConvStatements = [AssignStatement] }
		)
	).
	
%-----------------------------------------------------------------------------%
%
% Code for builtins
%

:- pred ml_gen_builtin(pred_id, proc_id, list(prog_var), code_model,
		prog_context, mlds__defns, mlds__statements,
		ml_gen_info, ml_gen_info).
:- mode ml_gen_builtin(in, in, in, in, in, out, out, in, out) is det.

ml_gen_builtin(PredId, ProcId, ArgVars, CodeModel, Context,
		MLDS_Decls, MLDS_Statements) -->
	
	ml_gen_var_list(ArgVars, ArgLvals),

	=(Info),
	{ ml_gen_info_get_module_info(Info, ModuleInfo) },
	{ predicate_module(ModuleInfo, PredId, ModuleName) },
	{ predicate_name(ModuleInfo, PredId, PredName) },
	{
		ml_translate_builtin(ModuleName, PredName,
			ProcId, ArgLvals, MaybeTest0, MaybeAssign0)
	->
		MaybeTest = MaybeTest0,
		MaybeAssign = MaybeAssign0
	;
		error("ml_gen_builtin: unknown builtin predicate")
	},
	(
		{ CodeModel = model_det },
		(
			{ MaybeTest = no },
			{ MaybeAssign = yes(Lval - Rval) }
		->
			{ MLDS_Statement = ml_gen_assign(Lval, Rval,
				Context) }
		;
			{ error("Malformed det builtin predicate") }
		)
	;
		{ CodeModel = model_semi },
		(
			{ MaybeTest = yes(Test) },
			{ MaybeAssign = no }
		->
			ml_gen_set_success(Test, Context, MLDS_Statement)
		;
			{ error("Malformed semi builtin predicate") }
		)
	;
		{ CodeModel = model_non },
		{ error("Nondet builtin predicate") }
	),
	{ MLDS_Statements = [MLDS_Statement] },
	{ MLDS_Decls = [] }.

	% Given a module name, a predicate name, a proc_id and a list of
	% the lvals for the arguments, find out if that procedure of that
	% predicate is an inline builtin. If yes, the last two arguments
	% return two things:
	%
	% - an rval to execute as a test if the builtin is semidet; or
	%
	% - an rval to assign to an lval if the builtin is det.
	%
	% Exactly one of these will be present.
	%
	% XXX this is not great interface design -
	% better to return a discriminated union than
	% returning two maybes.  But I kept it this way so that
	% the code stays similar to code_util__translate_builtin.

:- pred ml_translate_builtin(module_name, string, proc_id, list(mlds__lval),
		maybe(mlds__rval), maybe(pair(mlds__lval, mlds__rval))).
:- mode ml_translate_builtin(in, in, in, in, out, out) is semidet.

ml_translate_builtin(FullyQualifiedModule, PredName, ProcId, Args,
		TestOp, AssignmentOp) :-
	proc_id_to_int(ProcId, ProcInt),
	% -- not yet:
	% FullyQualifiedModule = qualified(unqualified("std"), ModuleName),
	FullyQualifiedModule = unqualified(ModuleName),
	ml_translate_builtin_2(ModuleName, PredName, ProcInt, Args,
		TestOp, AssignmentOp).

:- pred ml_translate_builtin_2(string, string, int, list(mlds__lval),
	maybe(mlds__rval), maybe(pair(mlds__lval, mlds__rval))).
:- mode ml_translate_builtin_2(in, in, in, in, out, out) is semidet.

% WARNING: any changes here may need to be duplicated in
% code_util__translate_builtin_2 and vice versa.

ml_translate_builtin_2("private_builtin", "unsafe_type_cast", 0,
		[X, Y], no, yes(Y - lval(X))).
ml_translate_builtin_2("builtin", "unsafe_promise_unique", 0,
		[X, Y], no, yes(Y - lval(X))).

ml_translate_builtin_2("private_builtin", "builtin_int_gt", 0, [X, Y],
	yes(binop((>), lval(X), lval(Y))), no).
ml_translate_builtin_2("private_builtin", "builtin_int_lt", 0, [X, Y],
	yes(binop((<), lval(X), lval(Y))), no).

ml_translate_builtin_2("int", "builtin_plus", 0, [X, Y, Z],
	no, yes(Z - binop((+), lval(X), lval(Y)))).
ml_translate_builtin_2("int", "builtin_plus", 1, [X, Y, Z],
	no, yes(X - binop((-), lval(Z), lval(Y)))).
ml_translate_builtin_2("int", "builtin_plus", 2, [X, Y, Z],
	no, yes(Y - binop((-), lval(Z), lval(X)))).
ml_translate_builtin_2("int", "+", 0, [X, Y, Z],
	no, yes(Z - binop((+), lval(X), lval(Y)))).
ml_translate_builtin_2("int", "+", 1, [X, Y, Z],
	no, yes(X - binop((-), lval(Z), lval(Y)))).
ml_translate_builtin_2("int", "+", 2, [X, Y, Z],
	no, yes(Y - binop((-), lval(Z), lval(X)))).
ml_translate_builtin_2("int", "builtin_minus", 0, [X, Y, Z],
	no, yes(Z - binop((-), lval(X), lval(Y)))).
ml_translate_builtin_2("int", "builtin_minus", 1, [X, Y, Z],
	no, yes(X - binop((+), lval(Y), lval(Z)))).
ml_translate_builtin_2("int", "builtin_minus", 2, [X, Y, Z],
	no, yes(Y - binop((-), lval(X), lval(Z)))).
ml_translate_builtin_2("int", "-", 0, [X, Y, Z],
	no, yes(Z - binop((-), lval(X), lval(Y)))).
ml_translate_builtin_2("int", "-", 1, [X, Y, Z],
	no, yes(X - binop((+), lval(Y), lval(Z)))).
ml_translate_builtin_2("int", "-", 2, [X, Y, Z],
	no, yes(Y - binop((-), lval(X), lval(Z)))).
ml_translate_builtin_2("int", "builtin_times", 0, [X, Y, Z],
	no, yes(Z - binop((*), lval(X), lval(Y)))).
ml_translate_builtin_2("int", "builtin_times", 1, [X, Y, Z],
	no, yes(X - binop((/), lval(Z), lval(Y)))).
ml_translate_builtin_2("int", "builtin_times", 2, [X, Y, Z],
	no, yes(Y - binop((/), lval(Z), lval(X)))).
ml_translate_builtin_2("int", "*", 0, [X, Y, Z],
	no, yes(Z - binop((*), lval(X), lval(Y)))).
ml_translate_builtin_2("int", "*", 1, [X, Y, Z],
	no, yes(X - binop((/), lval(Z), lval(Y)))).
ml_translate_builtin_2("int", "*", 2, [X, Y, Z],
	no, yes(Y - binop((/), lval(Z), lval(X)))).
ml_translate_builtin_2("int", "builtin_div", 0, [X, Y, Z],
	no, yes(Z - binop((/), lval(X), lval(Y)))).
ml_translate_builtin_2("int", "builtin_div", 1, [X, Y, Z],
	no, yes(X - binop((*), lval(Y), lval(Z)))).
ml_translate_builtin_2("int", "builtin_div", 2, [X, Y, Z],
	no, yes(Y - binop((/), lval(X), lval(Z)))).
ml_translate_builtin_2("int", "//", 0, [X, Y, Z],
	no, yes(Z - binop((/), lval(X), lval(Y)))).
ml_translate_builtin_2("int", "//", 1, [X, Y, Z],
	no, yes(X - binop((*), lval(Y), lval(Z)))).
ml_translate_builtin_2("int", "//", 2, [X, Y, Z],
	no, yes(Y - binop((/), lval(X), lval(Z)))).
ml_translate_builtin_2("int", "builtin_mod", 0, [X, Y, Z],
	no, yes(Z - binop((mod), lval(X), lval(Y)))).
ml_translate_builtin_2("int", "rem", 0, [X, Y, Z],
	no, yes(Z - binop((mod), lval(X), lval(Y)))).
ml_translate_builtin_2("int", "builtin_left_shift", 0, [X, Y, Z],
	no, yes(Z - binop((<<), lval(X), lval(Y)))).
ml_translate_builtin_2("int", "unchecked_left_shift", 0, [X, Y, Z],
	no, yes(Z - binop((<<), lval(X), lval(Y)))).
ml_translate_builtin_2("int", "builtin_right_shift", 0, [X, Y, Z],
	no, yes(Z - binop((>>), lval(X), lval(Y)))).
ml_translate_builtin_2("int", "unchecked_right_shift", 0, [X, Y, Z],
	no, yes(Z - binop((>>), lval(X), lval(Y)))).
ml_translate_builtin_2("int", "builtin_bit_and", 0, [X, Y, Z],
	no, yes(Z - binop((&), lval(X), lval(Y)))).
ml_translate_builtin_2("int", "/\\", 0, [X, Y, Z],
	no, yes(Z - binop((&), lval(X), lval(Y)))).
ml_translate_builtin_2("int", "builtin_bit_or", 0, [X, Y, Z],
	no, yes(Z - binop(('|'), lval(X), lval(Y)))).
ml_translate_builtin_2("int", "\\/", 0, [X, Y, Z],
	no, yes(Z - binop(('|'), lval(X), lval(Y)))).
ml_translate_builtin_2("int", "builtin_bit_xor", 0, [X, Y, Z],
	no, yes(Z - binop((^), lval(X), lval(Y)))).
ml_translate_builtin_2("int", "^", 0, [X, Y, Z],
	no, yes(Z - binop((^), lval(X), lval(Y)))).
ml_translate_builtin_2("int", "xor", 0, [X, Y, Z],
	no, yes(Z - binop((^), lval(X), lval(Y)))).
ml_translate_builtin_2("int", "builtin_unary_plus", 0, [X, Y],
	no, yes(Y - lval(X))).
ml_translate_builtin_2("int", "+", 0, [X, Y],
	no, yes(Y - lval(X))).
ml_translate_builtin_2("int", "builtin_unary_minus", 0, [X, Y],
	no, yes(Y - binop((-), const(int_const(0)), lval(X)))).
ml_translate_builtin_2("int", "-", 0, [X, Y],
	no, yes(Y - binop((-), const(int_const(0)), lval(X)))).
ml_translate_builtin_2("int", "builtin_bit_neg", 0, [X, Y],
	no, yes(Y - unop(std_unop(bitwise_complement), lval(X)))).
ml_translate_builtin_2("int", "\\", 0, [X, Y],
	no, yes(Y - unop(std_unop(bitwise_complement), lval(X)))).
ml_translate_builtin_2("int", ">", 0, [X, Y],
	yes(binop((>), lval(X), lval(Y))), no).
ml_translate_builtin_2("int", "<", 0, [X, Y],
	yes(binop((<), lval(X), lval(Y))), no).
ml_translate_builtin_2("int", ">=", 0, [X, Y],
	yes(binop((>=), lval(X), lval(Y))), no).
ml_translate_builtin_2("int", "=<", 0, [X, Y],
	yes(binop((<=), lval(X), lval(Y))), no).

ml_translate_builtin_2("float", "builtin_float_plus", 0, [X, Y, Z],
	no, yes(Z - binop(float_plus, lval(X), lval(Y)))).
ml_translate_builtin_2("float", "builtin_float_plus", 1, [X, Y, Z],
	no, yes(X - binop(float_minus, lval(Z), lval(Y)))).
ml_translate_builtin_2("float", "builtin_float_plus", 2, [X, Y, Z],
	no, yes(Y - binop(float_minus, lval(Z), lval(X)))).
ml_translate_builtin_2("float", "+", 0, [X, Y, Z],
	no, yes(Z - binop(float_plus, lval(X), lval(Y)))).
ml_translate_builtin_2("float", "+", 1, [X, Y, Z],
	no, yes(X - binop(float_minus, lval(Z), lval(Y)))).
ml_translate_builtin_2("float", "+", 2, [X, Y, Z],
	no, yes(Y - binop(float_minus, lval(Z), lval(X)))).
ml_translate_builtin_2("float", "builtin_float_minus", 0, [X, Y, Z],
	no, yes(Z - binop(float_minus, lval(X), lval(Y)))).
ml_translate_builtin_2("float", "builtin_float_minus", 1, [X, Y, Z],
	no, yes(X - binop(float_plus, lval(Y), lval(Z)))).
ml_translate_builtin_2("float", "builtin_float_minus", 2, [X, Y, Z],
	no, yes(Y - binop(float_minus, lval(X), lval(Z)))).
ml_translate_builtin_2("float", "-", 0, [X, Y, Z],
	no, yes(Z - binop(float_minus, lval(X), lval(Y)))).
ml_translate_builtin_2("float", "-", 1, [X, Y, Z],
	no, yes(X - binop(float_plus, lval(Y), lval(Z)))).
ml_translate_builtin_2("float", "-", 2, [X, Y, Z],
	no, yes(Y - binop(float_minus, lval(X), lval(Z)))).
ml_translate_builtin_2("float", "builtin_float_times", 0, [X, Y, Z],
	no, yes(Z - binop(float_times, lval(X), lval(Y)))).
ml_translate_builtin_2("float", "builtin_float_times", 1, [X, Y, Z],
	no, yes(X - binop(float_divide, lval(Z), lval(Y)))).
ml_translate_builtin_2("float", "builtin_float_times", 2, [X, Y, Z],
	no, yes(Y - binop(float_divide, lval(Z), lval(X)))).
ml_translate_builtin_2("float", "*", 0, [X, Y, Z],
	no, yes(Z - binop(float_times, lval(X), lval(Y)))).
ml_translate_builtin_2("float", "*", 1, [X, Y, Z],
	no, yes(X - binop(float_divide, lval(Z), lval(Y)))).
ml_translate_builtin_2("float", "*", 2, [X, Y, Z],
	no, yes(Y - binop(float_divide, lval(Z), lval(X)))).
ml_translate_builtin_2("float", "builtin_float_divide", 0, [X, Y, Z],
	no, yes(Z - binop(float_divide, lval(X), lval(Y)))).
ml_translate_builtin_2("float", "builtin_float_divide", 1, [X, Y, Z],
	no, yes(X - binop(float_times, lval(Y), lval(Z)))).
ml_translate_builtin_2("float", "builtin_float_divide", 2, [X, Y, Z],
	no, yes(Y - binop(float_divide, lval(X), lval(Z)))).
ml_translate_builtin_2("float", "/", 0, [X, Y, Z],
	no, yes(Z - binop(float_divide, lval(X), lval(Y)))).
ml_translate_builtin_2("float", "/", 1, [X, Y, Z],
	no, yes(X - binop(float_times, lval(Y), lval(Z)))).
ml_translate_builtin_2("float", "/", 2, [X, Y, Z],
	no, yes(Y - binop(float_divide, lval(X), lval(Z)))).
ml_translate_builtin_2("float", "+", 0, [X, Y],
	no, yes(Y - lval(X))).
ml_translate_builtin_2("float", "-", 0, [X, Y],
	no, yes(Y - binop(float_minus, const(float_const(0.0)), lval(X)))).
ml_translate_builtin_2("float", "builtin_float_gt", 0, [X, Y],
	yes(binop(float_gt, lval(X), lval(Y))), no).
ml_translate_builtin_2("float", ">", 0, [X, Y],
	yes(binop(float_gt, lval(X), lval(Y))), no).
ml_translate_builtin_2("float", "builtin_float_lt", 0, [X, Y],
	yes(binop(float_lt, lval(X), lval(Y))), no).
ml_translate_builtin_2("float", "<", 0, [X, Y],
	yes(binop(float_lt, lval(X), lval(Y))), no).
ml_translate_builtin_2("float", "builtin_float_ge", 0, [X, Y],
	yes(binop(float_ge, lval(X), lval(Y))), no).
ml_translate_builtin_2("float", ">=", 0, [X, Y],
	yes(binop(float_ge, lval(X), lval(Y))), no).
ml_translate_builtin_2("float", "builtin_float_le", 0, [X, Y],
	yes(binop(float_le, lval(X), lval(Y))), no).
ml_translate_builtin_2("float", "=<", 0, [X, Y],
	yes(binop(float_le, lval(X), lval(Y))), no).

%-----------------------------------------------------------------------------%
%
% Code for switches
%

	% Generate MLDS code for a switch.
	%
:- pred ml_gen_switch(prog_var, can_fail, list(case), code_model, prog_context,
			mlds__defns, mlds__statements,
			ml_gen_info, ml_gen_info).
:- mode ml_gen_switch(in, in, in, in, in, out, out, in, out) is det.


:- type extended_case
	---> case(
		int,
		cons_tag,
		cons_id,
		instmap_delta,
		hlds_goal
	).
:- type cases_list == list(extended_case).

	% TODO: optimize various different special kinds of switches,
	% such as string switches, dense switches, lookup switches,
	% etc. (see switch_gen.m, etc.).
	% TODO: optimize switches so that the recursive case comes
	% first (see switch_gen.m).

ml_gen_switch(Var, CanFail, Cases, CodeModel, Context,
		MLDS_Decls, MLDS_Statements) -->
	%
	% Lookup the representation of the constructors for the tag tests
	% and their corresponding priorities.
	%
	ml_switch_lookup_tags(Cases, Var, TaggedCases0),
	%
	% Sort the cases according to the priority of their tag tests.
	%
	{ list__sort_and_remove_dups(TaggedCases0, TaggedCases) },
	%
	% Generate an if-then-else chain which tests each of the cases
	% in turn.
	%
	ml_switch_generate_cases(TaggedCases, Var,
		CodeModel, CanFail, Context,
		MLDS_Decls, MLDS_Statements).

	% Look up the representation (tag) for the cons_id in each case.
	% Also look up the priority of each tag test.
	%
:- pred ml_switch_lookup_tags(list(case), prog_var, cases_list,
				ml_gen_info, ml_gen_info).
:- mode ml_switch_lookup_tags(in, in, out, in, out) is det.

ml_switch_lookup_tags([], _, []) --> [].
ml_switch_lookup_tags([Case | Cases], Var, [TaggedCase | TaggedCases]) -->
	{ Case = case(ConsId, IMDelta, Goal) },
	ml_variable_type(Var, Type),
	ml_cons_id_to_tag(ConsId, Type, Tag),
	{ ml_switch_priority(Tag, Priority) },
	{ TaggedCase = case(Priority, Tag, ConsId, IMDelta, Goal) },
	ml_switch_lookup_tags(Cases, Var, TaggedCases).

	% Return the priority of a tag test.
	% A low number here indicates a high priority.
	% We prioritize the tag tests so that the cheapest
	% (most efficient) ones come first.
	%
:- pred ml_switch_priority(cons_tag, int).
:- mode ml_switch_priority(in, out) is det.

ml_switch_priority(no_tag, 0).			% should never occur
ml_switch_priority(int_constant(_), 1).
ml_switch_priority(shared_local_tag(_, _), 1).
ml_switch_priority(unshared_tag(_), 2).
ml_switch_priority(float_constant(_), 3).
ml_switch_priority(shared_remote_tag(_, _), 4).
ml_switch_priority(string_constant(_), 5).
	% The following tags should all never occur in switches.
ml_switch_priority(pred_closure_tag(_, _, _), 6).
ml_switch_priority(code_addr_constant(_, _), 6).
ml_switch_priority(type_ctor_info_constant(_, _, _), 6).
ml_switch_priority(base_typeclass_info_constant(_, _, _), 6).
ml_switch_priority(tabling_pointer_constant(_, _), 6).

	% Generate a chain of if-then-elses to test each case in turn.
	%
:- pred ml_switch_generate_cases(list(extended_case), prog_var,
	code_model, can_fail, prog_context, mlds__defns, mlds__statements,
	ml_gen_info, ml_gen_info).
:- mode ml_switch_generate_cases(in, in, in, in, in, out, out,
	in, out) is det.

ml_switch_generate_cases([], _Var, CodeModel, CanFail, Context,
		[], MLDS_Statements) -->
	( { CanFail = can_fail } ->
		ml_gen_failure(CodeModel, Context, MLDS_Statements)
	;
		{ error("switch failure") }
	).
ml_switch_generate_cases([Case | Cases], Var, CodeModel, CanFail, Context,
		MLDS_Decls, MLDS_Statements) -->
	{ Case = case(_, _Tag, ConsId, IMDelta, Goal) },
	=(MLDSGenInfo0),
	{ ml_gen_info_get_instmap(MLDSGenInfo0, InstMap0) },
	{ instmap__apply_instmap_delta(InstMap0, IMDelta, InstMap1) },
	ml_gen_info_set_instmap(InstMap1),
	(
		{ Cases = [], CanFail = cannot_fail }
	->
		ml_gen_goal(CodeModel, Goal, MLDS_Decls, MLDS_Statements)
	;
		ml_gen_tag_test(Var, ConsId, TagTestDecls, TagTestStatements,
			TagTestExpression),
		ml_gen_goal(CodeModel, Goal, GoalStatement),
		ml_gen_info_set_instmap(InstMap0),
		ml_switch_generate_cases(Cases, Var, CodeModel, CanFail,
			Context, RestDecls, RestStatements),
		{ Rest = ml_gen_block(RestDecls, RestStatements, Context) },
		{ IfStmt = if_then_else(TagTestExpression,
				GoalStatement, yes(Rest)) },
		{ IfStatement = mlds__statement(IfStmt,
			mlds__make_context(Context)) },
		{ MLDS_Decls = TagTestDecls },
		{ MLDS_Statements = list__append(TagTestStatements,
			[IfStatement]) }
	).

%-----------------------------------------------------------------------------%
%
% Code for if-then-else
%

:- pred ml_gen_ite(code_model, hlds_goal, hlds_goal, hlds_goal, prog_context,
		mlds__defns, mlds__statements, ml_gen_info, ml_gen_info).
:- mode ml_gen_ite(in, in, in, in, in, out, out, in, out) is det.

ml_gen_ite(CodeModel, Cond, Then, Else, Context,
		MLDS_Decls, MLDS_Statements) -->
	{ Cond = _ - CondGoalInfo },
	{ goal_info_get_code_model(CondGoalInfo, CondCodeModel) },
	=(MLDSGenInfo0),
	{ ml_gen_info_get_instmap(MLDSGenInfo0, InstMap0) },
	(
		{ CondCodeModel = model_det },
		% simplify.m should remove these
		{ error("ml_gen_ite: det cond") }
	;
		%	model_semi cond:
		%		<(Cond -> Then ; Else)>
		%	===>
		%	{
		%		bool succeeded;
		%	
		%		<succeeded = Cond>
		%		if (succeeded) {
		%			<Then>
		%		} else {
		%			<Else>
		%		}
		%	}
		{ CondCodeModel = model_semi },
		ml_gen_goal(model_semi, Cond, CondDecls, CondStatements),
		ml_gen_test_success(Succeeded),
		ml_gen_info_update_instmap(Cond),
		ml_gen_goal(CodeModel, Then, ThenStatement),
		ml_gen_info_set_instmap(InstMap0),
		ml_gen_goal(CodeModel, Else, ElseStatement),
		{ IfStmt = if_then_else(Succeeded, ThenStatement,
			yes(ElseStatement)) },
		{ IfStatement = mlds__statement(IfStmt,
			mlds__make_context(Context)) },
		{ MLDS_Decls = CondDecls },
		{ MLDS_Statements = list__append(CondStatements,
			[IfStatement]) }
	;
		%	/*
		%	** XXX The following transformation does not do as
		%	**     good a job of GC as it could.  Ideally we ought
		%	**     to ensure that stuff used only in the `Else'
		%	**     part will be reclaimed if a GC occurs during
		%	**     the `Then' part.  But that is a bit tricky to
		%	**     achieve.
		%	*/
		%
		% /* XXX Bug: Cond might clobber the value of succeeded! */
		%
		%	model_non cond:
		%		<(Cond -> Then ; Else)>
		%	===>
		%	{
		%		bool succeeded;
		%
		%		void then_func() {
		%			succeeded = TRUE;
		%			<Then>
		%		}
		%
		%		succeeded = FALSE;
		%		<Cond && then_func()>
		%		if (!succeeded) {
		%			<Else>
		%		}
		%	}

		{ CondCodeModel = model_non },

		% generate the `then_func'
		ml_gen_info_update_instmap(Cond),
		ml_gen_new_func_label(ThenFuncLabel, ThenFuncLabelRval),
		/* push nesting level */
		{ Then = _ - ThenGoalInfo },
		{ goal_info_get_context(ThenGoalInfo, ThenContext) },
		ml_gen_set_success(const(true), ThenContext, SetSuccessTrue),
		ml_gen_goal(CodeModel, Then, ThenStatement),
		{ ThenFuncBody = ml_gen_block([],
			[SetSuccessTrue, ThenStatement], ThenContext) },
		/* pop nesting level */
		ml_gen_nondet_label_func(ThenFuncLabel, ThenContext,
			ThenFuncBody, ThenFunc),

		% generate the main body
		ml_gen_set_success(const(false), Context, SetSuccessFalse),
		ml_get_env_ptr(EnvPtrRval),
		{ SuccessCont = success_cont(ThenFuncLabelRval, EnvPtrRval) },
		ml_gen_info_push_success_cont(SuccessCont),
		ml_gen_info_set_instmap(InstMap0),
		ml_gen_goal(model_non, Cond, CondDecls, CondStatements),
		ml_gen_info_pop_success_cont,
		ml_gen_test_success(Succeeded),
		ml_gen_info_set_instmap(InstMap0),
		ml_gen_goal(CodeModel, Else, ElseStatement),
		{ IfStmt = if_then_else(unop(std_unop(not), Succeeded),
			ElseStatement, no) },
		{ IfStatement = mlds__statement(IfStmt,
			mlds__make_context(Context)) },

		% package it all up in the right order
		{ MLDS_Decls = [ThenFunc | CondDecls] },
		{ MLDS_Statements = list__append(
			[SetSuccessFalse | CondStatements], [IfStatement]) }
	).

%-----------------------------------------------------------------------------%
%
% Code for negation
%

:- pred ml_gen_negation(hlds_goal, code_model, prog_context,
		mlds__defns, mlds__statements, ml_gen_info, ml_gen_info).
:- mode ml_gen_negation(in, in, in, out, out, in, out) is det.

ml_gen_negation(Cond, CodeModel, Context,
		MLDS_Decls, MLDS_Statements) -->
	{ Cond = _ - CondGoalInfo },
	{ goal_info_get_code_model(CondGoalInfo, CondCodeModel) },
	(
		% model_det negation:
		%		<not(Goal)>
		%	===>
		%	{
		%		bool succeeded;
		%		<succeeded = Goal>
		%		/* now ignore the value of succeeded,
		%		   which we know will be FALSE */
		%	}
		{ CodeModel = model_det },
		ml_gen_goal(model_semi, Cond, MLDS_Decls, MLDS_Statements)
	;
		% model_semi negation, model_det goal:
		%		<succeeded = not(Goal)>
		%	===>
		%	{
		%		bool succeeded;
		%		<succeeded = Goal>
		%		succeeded = FALSE;
		%	}
		{ CodeModel = model_semi, CondCodeModel = model_det },
		ml_gen_goal(model_det, Cond, CondDecls, CondStatements),
		ml_gen_set_success(const(false), Context, SetSuccessFalse),
		{ MLDS_Decls = CondDecls },
		{ MLDS_Statements = list__append(CondStatements,
			[SetSuccessFalse]) }
	;
		% model_semi negation, model_semi goal:
		%		<succeeded = not(Goal)>
		%	===>
		%	{
		%		bool succeeded;
		%		<succeeded = Goal>
		%		succeeded = !succeeded;
		%	}
		{ CodeModel = model_semi, CondCodeModel = model_semi },
		ml_gen_goal(model_semi, Cond, CondDecls, CondStatements),
		ml_gen_test_success(Succeeded),
		ml_gen_set_success(unop(std_unop(not), Succeeded), Context,
			InvertSuccess),
		{ MLDS_Decls = CondDecls },
		{ MLDS_Statements = list__append(CondStatements,
			[InvertSuccess]) }
	;
		{ CodeModel = model_semi, CondCodeModel = model_non },
		{ error("ml_gen_negation: nondet cond") }
	;
		{ CodeModel = model_non },
		{ error("ml_gen_negation: nondet negation") }
	).

%-----------------------------------------------------------------------------%
%
% Code for conjunctions
%

:- pred ml_gen_conj(hlds_goals, code_model, prog_context,
		mlds__defns, mlds__statements, ml_gen_info, ml_gen_info).
:- mode ml_gen_conj(in, in, in, out, out, in, out) is det.

ml_gen_conj([], CodeModel, Context, [], MLDS_Statements) -->
	ml_gen_success(CodeModel, Context, MLDS_Statements).
ml_gen_conj([SingleGoal], CodeModel, _Context,
		MLDS_Decls, MLDS_Statements) -->
	ml_gen_goal(CodeModel, SingleGoal, MLDS_Decls, MLDS_Statements).
ml_gen_conj([First | Rest], CodeModel, Context,
		MLDS_Decls, MLDS_Statements) -->
	{ Rest = [_ | _] },
	=(MLDSGenInfo0),
	{ ml_gen_info_get_instmap(MLDSGenInfo0, InstMap0) },
	{ First = _ - FirstGoalInfo },
	{ goal_info_get_code_model(FirstGoalInfo, FirstCodeModel) },
	{ goal_info_get_instmap_delta(FirstGoalInfo, FirstInstMapDelta) },
	{ instmap__apply_instmap_delta(InstMap0, FirstInstMapDelta,
		InstMapAfterFirst) },
	{ DoGenFirst =
	    (pred(FirstDecls::out, FirstStatements::out, in, out) is det -->
		ml_gen_info_set_instmap(InstMap0),
		ml_gen_goal(FirstCodeModel, First, FirstDecls, FirstStatements)
	    ) },
	{ DoGenRest =
	    (pred(RestDecls::out, RestStatements::out, in, out) is det -->
	     	ml_gen_info_set_instmap(InstMapAfterFirst),	
		ml_gen_conj(Rest, CodeModel, Context,
			RestDecls, RestStatements)
	    ) },
	ml_combine_conj(FirstCodeModel, Context, DoGenFirst, DoGenRest,
		MLDS_Decls, MLDS_Statements).

:- type gen_pred == pred(mlds__defns, mlds__statements,
		ml_gen_info, ml_gen_info).
:- inst gen_pred = (pred(out, out, in, out) is det).

:- pred ml_combine_conj(code_model, prog_context, gen_pred, gen_pred,
		mlds__defns, mlds__statements, ml_gen_info, ml_gen_info).
:- mode ml_combine_conj(in, in, in(gen_pred), in(gen_pred),
		out, out, in, out) is det.

ml_combine_conj(FirstCodeModel, Context, DoGenFirst, DoGenRest,
		MLDS_Decls, MLDS_Statements) -->
	(
		%	model_det goal:
		%		<First, Rest>
		% 	===>
		%		<do First>
		%		<Rest>
		%	
		{ FirstCodeModel = model_det },
		DoGenFirst(FirstDecls, FirstStatements),
		DoGenRest(RestDecls, RestStatements),
		{ ml_join_decls(FirstDecls, FirstStatements,
			RestDecls, RestStatements, Context,
			MLDS_Decls, MLDS_Statements) }
	;
		%	model_semi goal:
		%		<Goal, Goals>
		% 	===>
		%	{
		%		bool succeeded;
		%
		%		<succeeded = Goal>;
		%		if (succeeded) {
		%			<Goals>;
		%		}
		%	}
		{ FirstCodeModel = model_semi },
		DoGenFirst(FirstDecls, FirstStatements),
		ml_gen_test_success(Succeeded),
		DoGenRest(RestDecls, RestStatements),
		{ IfBody = ml_gen_block(RestDecls, RestStatements, Context) },
		{ IfStmt = if_then_else(Succeeded, IfBody, no) },
		{ IfStatement = mlds__statement(IfStmt,
			mlds__make_context(Context)) },
		{ MLDS_Decls = FirstDecls },
		{ MLDS_Statements = list__append(FirstStatements,
			[IfStatement]) }
	;
		%	model_non goal:
		%		<First, Rest>
		% 	===>
		%	{
		%		succ_func() {
		%			<Rest && SUCCEED()>;
		%		}
		%
		%		<First && succ_func()>;
		%	}
		%
		% XXX this leads to deep nesting for long conjunctions;
		%     we should avoid that.

		{ FirstCodeModel = model_non },

		% generate the `succ_func'
		ml_gen_new_func_label(RestFuncLabel, RestFuncLabelRval),
		/* push nesting level */
		DoGenRest(RestDecls, RestStatements),
		{ RestStatement = ml_gen_block(RestDecls, RestStatements,
			Context) },
		/* pop nesting level */
		ml_gen_nondet_label_func(RestFuncLabel, Context, RestStatement,
			RestFunc),

		ml_get_env_ptr(EnvPtrRval),
		{ SuccessCont = success_cont(RestFuncLabelRval,
			EnvPtrRval) },
		ml_gen_info_push_success_cont(SuccessCont),
		DoGenFirst(FirstDecls, FirstStatements),
		ml_gen_info_pop_success_cont,

		% it might be better to put the decls in the other order:
		/* { MLDS_Decls = list__append(FirstDecls, [RestFunc]) }, */
		{ MLDS_Decls = [RestFunc | FirstDecls] },
		{ MLDS_Statements = FirstStatements }
	).


	% Allocate a new function label and return an rval containing
	% the function's address.
	%
:- pred ml_gen_new_func_label(ml_label_func, mlds__rval,
		ml_gen_info, ml_gen_info).
:- mode ml_gen_new_func_label(out, out, in, out) is det.

ml_gen_new_func_label(FuncLabel, FuncLabelRval) -->
	ml_gen_info_new_func_label(FuncLabel),
	=(Info),
	{ ml_gen_info_get_module_info(Info, ModuleInfo) },
	{ ml_gen_info_get_pred_id(Info, PredId) },
	{ ml_gen_info_get_proc_id(Info, ProcId) },
	{ ml_gen_pred_label(ModuleInfo, PredId, ProcId,
		PredLabel, PredModule) },
	{ ProcLabel = qual(PredModule, PredLabel - ProcId) },
	{ FuncLabelRval = const(code_addr_const(internal(ProcLabel,
		FuncLabel))) }.

	% Given a function label and the statement which will comprise
	% the function body for that function, generate an mlds__defn
	% which defines that function.
	%
:- pred ml_gen_nondet_label_func(ml_label_func, prog_context,
		mlds__statement, mlds__defn, ml_gen_info, ml_gen_info).
:- mode ml_gen_nondet_label_func(in, in, in, out, in, out) is det.

ml_gen_nondet_label_func(FuncLabel, Context, Statement, Func) -->
	ml_gen_info_use_gcc_nested_functions(UseNested),
	( { UseNested = yes } ->
		{ FuncParams = mlds__func_params([], []) }
	;
		ml_declare_env_ptr_arg(EnvPtrArg),
		{ FuncParams = mlds__func_params([EnvPtrArg], []) }
	),
	ml_gen_label_func(FuncLabel, FuncParams, Context, Statement, Func).

	% Given a function label, the function parameters, and the statement
	% which will comprise the function body for that function,
	% generate an mlds__defn which defines that function.
	%
:- pred ml_gen_label_func(ml_label_func, mlds__func_params, prog_context,
		mlds__statement, mlds__defn, ml_gen_info, ml_gen_info).
:- mode ml_gen_label_func(in, in, in, in, out, in, out) is det.

ml_gen_label_func(FuncLabel, FuncParams, Context, Statement, Func) -->
	%
	% compute the function name
	%
	=(Info),
	{ ml_gen_info_get_module_info(Info, ModuleInfo) },
	{ ml_gen_info_get_pred_id(Info, PredId) },
	{ ml_gen_info_get_proc_id(Info, ProcId) },
	{ FuncName = ml_gen_nondet_label(ModuleInfo, PredId, ProcId,
		FuncLabel) },

	%
	% compute the function definition
	%
	{ DeclFlags = ml_gen_label_func_decl_flags },
	{ MaybePredProcId = no },
	{ FuncDefn = function(MaybePredProcId, FuncParams, yes(Statement)) },
	{ Func = mlds__defn(FuncName, mlds__make_context(Context), DeclFlags,
			FuncDefn) }.

	% Return the declaration flags appropriate for a label func
	% (a label func is a function used as a continuation
	% when generating nondet code).
	%
:- func ml_gen_label_func_decl_flags = mlds__decl_flags.
ml_gen_label_func_decl_flags = MLDS_DeclFlags :-
	Access = private,
	PerInstance = per_instance,
	Virtuality = non_virtual,
	Finality = overridable,
	Constness = modifiable,
	Abstractness = concrete,
	MLDS_DeclFlags = init_decl_flags(Access, PerInstance,
		Virtuality, Finality, Constness, Abstractness).

	% Return the declaration flags appropriate for a local variable.
:- func ml_gen_var_decl_flags = mlds__decl_flags.
ml_gen_var_decl_flags = MLDS_DeclFlags :-
	Access = public,
	PerInstance = per_instance,
	Virtuality = non_virtual,
	Finality = overridable,
	Constness = modifiable,
	Abstractness = concrete,
	MLDS_DeclFlags = init_decl_flags(Access, PerInstance,
		Virtuality, Finality, Constness, Abstractness).

%-----------------------------------------------------------------------------%
%
% Code for disjunctions
%

:- pred ml_gen_disj(hlds_goals, code_model, prog_context,
		mlds__defns, mlds__statements, ml_gen_info, ml_gen_info).
:- mode ml_gen_disj(in, in, in, out, out, in, out) is det.

	%
	% handle empty disjunctions (a.ka. `fail')
	%
ml_gen_disj([], CodeModel, Context, [], Statements) -->
	ml_gen_failure(CodeModel, Context, Statements).

	%
	% handle singleton disjunctions
	% (the HLDS should not contain singleton disjunctions,
	% but this code is needed to handle recursive calls to ml_gen_disj)
	%
ml_gen_disj([SingleGoal], CodeModel, _, MLDS_Decls, MLDS_Statements) -->
	ml_gen_goal(CodeModel, SingleGoal, MLDS_Decls, MLDS_Statements).

ml_gen_disj([First | Rest], CodeModel, Context,
		MLDS_Decls, MLDS_Statements) -->
	{ Rest = [_ | _] },

	=(MLDSGenInfo0),
	{ ml_gen_info_get_instmap(MLDSGenInfo0, InstMap0) },

	( { CodeModel = model_non } ->
		%
		% model_non disj:
		%
		%		<(Goal ; Goals) && SUCCEED()>
		%	===>
		%		<Goal && SUCCEED()>
		%		<Goals && SUCCEED()>
		%
		ml_gen_goal(model_non, First, FirstDecls, FirstStatements),
		ml_gen_info_set_instmap(InstMap0),
		ml_gen_disj(Rest, model_non, Context,
			RestDecls, RestStatements),
		{ ml_join_decls(FirstDecls, FirstStatements,
			RestDecls, RestStatements, Context,
			MLDS_Decls, MLDS_Statements) }
	; /* CodeModel is model_det or model_semi */
		%
		% model_det/model_semi disj:
		%
		%	model_det goal:
		%		<Goal ; Goals>
		%	===>
		%		<Goal>
		%		/* <Goals> will never be reached */
		%
		%	model_semi goal:
		%		<Goal ; Goals>
		%	===>
		%	{
		%		bool succeeded;
		%	
		%		<succeeded = Goal>;
		%		if (!succeeded) {
		%			<Goals>;
		%		}
		%	}
		%
		{ First = _ - FirstGoalInfo },
		{ goal_info_get_code_model(FirstGoalInfo, FirstCodeModel) },
		(
			{ FirstCodeModel = model_det },
			ml_gen_goal(model_det, First,
				MLDS_Decls, MLDS_Statements)
		;
			{ FirstCodeModel = model_semi },
			ml_gen_goal(model_semi, First,
				FirstDecls, FirstStatements),
			ml_gen_test_success(Succeeded),
			ml_gen_info_set_instmap(InstMap0),
			ml_gen_disj(Rest, CodeModel, Context,
				RestDecls, RestStatements),
			{ RestStatement = ml_gen_block(RestDecls,
				RestStatements, Context) },
			{ IfStmt = if_then_else(unop(std_unop(not), Succeeded),
						RestStatement, no) },
			{ IfStatement = mlds__statement(IfStmt,
				mlds__make_context(Context)) },
			{ MLDS_Decls = FirstDecls },
			{ MLDS_Statements = list__append(FirstStatements,
				[IfStatement]) }
		;
			{ FirstCodeModel = model_non },
			% simplify.m should get wrap commits around these
			{ error("model_non disj in model_det disjunction") }
		)
	).

	% ml_join_decls:
	% 	Join two statement lists and their corresponding
	% 	declaration lists in sequence.
	% 
	% 	If the statements have no declarations in common,
	% 	then their corresponding declaration lists will be
	% 	concatenated together into a single list of declarations.
	% 	But if they have any declarations in common, then we
	% 	put each statement list and its declarations into
	% 	a block, so that the declarations remain local to
	% 	each statement list.
	% 
:- pred ml_join_decls(mlds__defns, mlds__statements,
		mlds__defns, mlds__statements, prog_context,
		mlds__defns, mlds__statements).
:- mode ml_join_decls(in, in, in, in, in, out, out) is det.

ml_join_decls(FirstDecls, FirstStatements, RestDecls, RestStatements, Context,
		MLDS_Decls, MLDS_Statements) :-
	(
		list__member(mlds__defn(Name, _, _, _), FirstDecls),
		list__member(mlds__defn(Name, _, _, _), RestDecls)
	->
		First = ml_gen_block(FirstDecls, FirstStatements, Context),
		Rest = ml_gen_block(RestDecls, RestStatements, Context),
		MLDS_Decls = [],
		MLDS_Statements = [First, Rest]
	;
		MLDS_Decls = list__append(FirstDecls, RestDecls),
		MLDS_Statements = list__append(FirstStatements, RestStatements)
	).

%-----------------------------------------------------------------------------%
%
% Code for unifications
%

:- pred ml_gen_unification(unification, code_model, prog_context,
		mlds__defns, mlds__statements, ml_gen_info, ml_gen_info).
:- mode ml_gen_unification(in, in, in, out, out, in, out) is det.

ml_gen_unification(assign(Var1, Var2), CodeModel, Context,
		[], MLDS_Statements) -->
	{ require(unify(CodeModel, model_det),
		"ml_code_gen: assign not det") },
	(
		%
		% skip dummy argument types, since they will not have
		% been declared
		%
		ml_variable_type(Var1, Type),
		{ type_util__is_dummy_argument_type(Type) }
	->
		{ MLDS_Statements = [] }
	;
		ml_gen_var(Var1, Var1Lval),
		ml_gen_var(Var2, Var2Lval),
		{ MLDS_Statement = ml_gen_assign(Var1Lval, lval(Var2Lval),
			Context) },
		{ MLDS_Statements = [MLDS_Statement] }
	).

ml_gen_unification(simple_test(Var1, Var2), CodeModel, Context,
		[], [MLDS_Statement]) -->
	{ require(unify(CodeModel, model_semi),
		"ml_code_gen: simple_test not semidet") },
	ml_variable_type(Var1, Type),
	{ Type = term__functor(term__atom("string"), [], _) ->
		EqualityOp = str_eq
	; Type = term__functor(term__atom("float"), [], _) ->
		EqualityOp = float_eq
	;
		EqualityOp = eq
	},
	ml_gen_var(Var1, Var1Lval),
	ml_gen_var(Var2, Var2Lval),
	{ Test = binop(EqualityOp, lval(Var1Lval), lval(Var2Lval)) },
	ml_gen_set_success(Test, Context, MLDS_Statement).

ml_gen_unification(construct(Var, ConsId, Args, ArgModes,
		MaybeCellToReuse, _CellIsUnique, MaybeAditiRLExprnID),
		CodeModel, Context, MLDS_Decls, MLDS_Statements) -->
	{ require(unify(CodeModel, model_det),
		"ml_code_gen: construct not det") },
	{ MaybeAditiRLExprnID = yes(_) ->
		sorry("Aditi closures")
	;
		true
	},
	{ MaybeCellToReuse = yes(_) ->
		sorry("cell reuse")
	;
		true
	},
	ml_gen_construct(Var, ConsId, Args, ArgModes, Context,
		MLDS_Decls, MLDS_Statements).
ml_gen_unification(deconstruct(Var, ConsId, Args, ArgModes, CanFail),
		CodeModel, Context, MLDS_Decls, MLDS_Statements) -->
	(
		{ CanFail = can_fail },
		{ require(unify(CodeModel, model_semi),
			"ml_code_gen: can_fail deconstruct not semidet") },
		ml_gen_semi_deconstruct(Var, ConsId, Args, ArgModes, Context,
			MLDS_Decls, MLDS_Statements)
	;
		{ CanFail = cannot_fail },
		{ require(unify(CodeModel, model_det),
			"ml_code_gen: cannot_fail deconstruct not det") },
		ml_gen_det_deconstruct(Var, ConsId, Args, ArgModes, Context,
			MLDS_Decls, MLDS_Statements)
	).

ml_gen_unification(complicated_unify(_, _, _), _, _, [], []) -->
	% simplify.m should convert these into procedure calls
	{ error("ml_code_gen: complicated unify") }.


:- func ml_gen_assign(mlds__lval, mlds__rval, prog_context) = mlds__statement.
ml_gen_assign(Lval, Rval, Context) = MLDS_Statement :-
	Assign = assign(Lval, Rval),
	MLDS_Stmt = atomic(Assign),
	MLDS_Statement = mlds__statement(MLDS_Stmt,
		mlds__make_context(Context)).

:- pred ml_gen_construct(prog_var, cons_id, prog_vars, list(uni_mode),
		prog_context, mlds__defns, mlds__statements,
		ml_gen_info, ml_gen_info).
:- mode ml_gen_construct(in, in, in, in, in, out, out, in, out) is det.

ml_gen_construct(Var, ConsId, Args, ArgModes, Context,
		MLDS_Decls, MLDS_Statements) -->
	%
	% figure out how this cons_id is represented
	%
	ml_variable_type(Var, Type),
	ml_cons_id_to_tag(ConsId, Type, Tag),
	%
	% generate code to construct the specified representation
	%
	ml_gen_construct_rep(Tag, ConsId, Var, Args, ArgModes, Context,
			MLDS_Decls, MLDS_Statements).

:- pred ml_gen_construct_rep(cons_tag, cons_id, prog_var, prog_vars,
		list(uni_mode), prog_context, mlds__defns, mlds__statements,
		ml_gen_info, ml_gen_info).
:- mode ml_gen_construct_rep(in, in, in, in, in, in, out, out, in, out) is det.

ml_gen_construct_rep(string_constant(String), _, Var, Args, _ArgModes, Context,
		[], [MLDS_Statement]) -->
	( { Args = [] } ->
		[]
	;
		{ error("ml_code_gen: string constant has args") }
	),
	ml_gen_var(Var, VarLval),
	{ MLDS_Statement = ml_gen_assign(VarLval, const(string_const(String)),
		Context) }.
ml_gen_construct_rep(int_constant(Int), _, Var, Args, _ArgModes, Context,
		[], [MLDS_Statement]) -->
	( { Args = [] } ->
		[]
	;
		{ error("ml_code_gen: int constant has args") }
	),
	ml_gen_var(Var, VarLval),
	{ MLDS_Statement = ml_gen_assign(VarLval, const(int_const(Int)),
		Context) }.
ml_gen_construct_rep(float_constant(Float), _, Var, Args, _ArgModes, Context,
		[], [MLDS_Statement]) -->
	( { Args = [] } ->
		[]
	;
		{ error("ml_code_gen: float constant has args") }
	),
	ml_gen_var(Var, VarLval),
	{ MLDS_Statement = ml_gen_assign(VarLval, const(float_const(Float)),
		Context) }.

ml_gen_construct_rep(no_tag, _ConsId, Var, Args, Modes, Context,
		MLDS_Decls, MLDS_Statements) -->
	( { Args = [Arg], Modes = [Mode] } ->
		ml_variable_type(Arg, ArgType),
		ml_gen_var(Arg, ArgLval),
		ml_gen_var(Var, VarLval),
		ml_gen_sub_unify(ArgLval, Mode, ArgType, VarLval,
			Context, [], MLDS_Statements),
		{ MLDS_Decls = [] }
	;
		{ error("ml_code_gen: no_tag: arity != 1") }
	).

ml_gen_construct_rep(unshared_tag(Tag), ConsId, Var, Args, ArgModes,
		Context, MLDS_Decls, MLDS_Statements) -->
	ml_gen_new_object(Tag, no, ConsId, Var, Args, ArgModes, Context,
		MLDS_Decls, MLDS_Statements).
ml_gen_construct_rep(shared_remote_tag(Tag, SecondaryTag), ConsId, Var, Args,
		ArgModes, Context, MLDS_Decls, MLDS_Statements) -->
	ml_gen_new_object(Tag, yes(SecondaryTag), ConsId, Var, Args, ArgModes,
		Context, MLDS_Decls, MLDS_Statements).

ml_gen_construct_rep(shared_local_tag(Bits1, Num1), _ConsId, Var, Args,
		_ArgModes, Context, [], [MLDS_Statement]) -->
	( { Args = [] } ->
		[]
	;
		{ error("ml_code_gen: shared_local_tag constant has args") }
	),
	ml_gen_var(Var, VarLval),
	{ MLDS_Statement = ml_gen_assign(VarLval, 
		mkword(Bits1, unop(std_unop(mkbody), const(int_const(Num1)))),
		Context) }.

ml_gen_construct_rep(type_ctor_info_constant(ModuleName0, TypeName, TypeArity),
		_ConsId, Var, Args, _ArgModes, Context,
		[], [MLDS_Statement]) -->
	( { Args = [] } ->
		[]
	;
		{ error("ml_code_gen: type-info constant has args") }
	),
	ml_gen_var(Var, VarLval),
	%
	% Although the builtin types `int', `float', etc. are treated as part
	% of the `builtin' module, for historical reasons they don't have
	% any qualifiers at this point, so we need to add the `builtin'
	% qualifier now.
	%
	{ ModuleName0 = unqualified("") ->
		mercury_public_builtin_module(ModuleName)
	;
		ModuleName = ModuleName0
	},
	{ MLDS_Module = mercury_module_name_to_mlds(ModuleName) },
	{ DataAddr = data_addr(MLDS_Module,
		type_ctor(info, TypeName, TypeArity)) },
	{ MLDS_Statement = ml_gen_assign(VarLval, 
		const(data_addr_const(DataAddr)), Context) }.
ml_gen_construct_rep(base_typeclass_info_constant(ModuleName, ClassId,
			Instance), _ConsId, Var, Args, _ArgModes, Context,
		[], [MLDS_Statement]) -->
	( { Args = [] } ->
		[]
	;
		{ error("ml_code_gen: typeclass-info constant has args") }
	),
	ml_gen_var(Var, VarLval),
	{ MLDS_Module = mercury_module_name_to_mlds(ModuleName) },
	{ DataAddr = data_addr(MLDS_Module,
		base_typeclass_info(ClassId, Instance)) },
	{ MLDS_Statement = ml_gen_assign(VarLval, 
		const(data_addr_const(DataAddr)), Context) }.

ml_gen_construct_rep(tabling_pointer_constant(PredId, ProcId), _ConsId,
		Var, Args, _ArgModes, Context, [], [MLDS_Statement]) -->
	( { Args = [] } ->
		[]
	;
		{ error("ml_code_gen: tabling pointer constant has args") }
	),
	ml_gen_var(Var, VarLval),
	=(Info),
	{ ml_gen_info_get_module_info(Info, ModuleInfo) },
	{ ml_gen_pred_label(ModuleInfo, PredId, ProcId,
		PredLabel, PredModule) },
	{ DataAddr = data_addr(PredModule,
			tabling_pointer(PredLabel - ProcId)) },
	{ MLDS_Statement = ml_gen_assign(VarLval, 
		const(data_addr_const(DataAddr)), Context) }.

ml_gen_construct_rep(code_addr_constant(PredId, ProcId), _ConsId,
		Var, Args, _ArgModes, Context, [], [MLDS_Statement]) -->
	( { Args = [] } ->
		[]
	;
		{ error("ml_code_gen: address constant has args") }
	),
	ml_gen_var(Var, VarLval),
	ml_gen_proc_addr_rval(PredId, ProcId, ProcAddrRval),
	{ MLDS_Statement = ml_gen_assign(VarLval, ProcAddrRval, Context) }.

ml_gen_construct_rep(pred_closure_tag(PredId, ProcId, EvalMethod), _ConsId,
		Var, ArgVars, ArgModes, Context,
		MLDS_Decls, MLDS_Statements) -->
	% This constructs a closure.
	% The representation of closures for the LLDS backend is defined in
	% runtime/mercury_ho_call.h.
	% XXX should we use a different representation for closures
	% in the MLDS backend?

	(
		{ EvalMethod = normal }
	;
		{ EvalMethod = (aditi_bottom_up) },
		% XXX not yet implemented
		{ sorry("`aditi_bottom_up' closures") }
	;
		{ EvalMethod = (aditi_top_down) },
		% XXX not yet implemented
		{ sorry("`aditi_top_down' closures") }
	),

	%
	% Compute the lval where we will put the final result,
	% and its type.
	%
	ml_gen_var(Var, VarLval),
	ml_variable_type(Var, Type),
	{ MLDS_Type = mercury_type_to_mlds_type(Type) },

	%
	% Generate a dummy value for the closure layout
	% (we do this just to match the structure used
	% by the LLDS closure representation)
	%
	{ ClosureLayoutRval = const(int_const(0)) },
	{ mercury_private_builtin_module(PrivateBuiltinModule) },
	{ MLDS_PrivateBuiltinModule = mercury_module_name_to_mlds(
		PrivateBuiltinModule) },
	{ ClosureLayoutType = mlds__class_type(qual(MLDS_PrivateBuiltinModule,
			"closure_layout"), 0) },

	%
	% Generate a wrapper function which just unboxes the
	% arguments and then calls the specified procedure,
	% and put the address of the wrapper function in the closure.
	%
	% We insert the wrapper function in the extra_defns field
	% in the ml_gen_info; ml_gen_proc will extract it and will
	% insert it before the mlds__defn for the current procedure.
	%
	{ list__length(ArgVars, NumArgs) },
	ml_gen_closure_wrapper(PredId, ProcId, NumArgs, Type,
		Context, WrapperFunc, WrapperFuncRval, WrapperFuncType),
	ml_gen_info_add_extra_defn(WrapperFunc),

	%
	% Generate rvals for the arguments
	%
	ml_gen_var_list(ArgVars, ArgLvals),
	ml_variable_types(ArgVars, ArgTypes),
	{ MLDS_ArgTypes0 = list__map(mercury_type_to_mlds_type, ArgTypes) },
	=(Info),
	{ ml_gen_info_get_module_info(Info, ModuleInfo) },
	{ ml_gen_info_get_instmap(Info, InstMap) },
	{ ml_gen_info_get_inst_table(Info, InstTable) },
	{ ml_gen_cons_args(ArgLvals, ArgTypes, ArgModes, InstMap, InstTable,
		ModuleInfo, ArgRvals0) },

	%
	% Compute the rval which holds the number of arguments
	%
	{ NumArgsRval = const(int_const(NumArgs)) },
	{ NumArgsType = mlds__int_type },

	%
	% the pointer will not be tagged (i.e. the tag will be zero)
	%
	{ MaybeTag = yes(0) },
	{ CtorName = "<closure>" },

	%
	% put all the arguments of the closure together
	%
	{ ArgRvals = [ClosureLayoutRval, WrapperFuncRval, NumArgsRval
		| ArgRvals0] },
	{ MLDS_ArgTypes = [ClosureLayoutType, WrapperFuncType, NumArgsType
			| MLDS_ArgTypes0] },

	%
	% Compute the number of bytes to allocate
	%
	{ list__length(ArgRvals, TotalNumArgs) },
	{ SizeInWordsRval = const(int_const(TotalNumArgs)) },
	{ SizeOfWordRval = ml_sizeof_word_rval },
	{ SizeInBytesRval = binop((*), SizeInWordsRval, SizeOfWordRval) },
	
	%
	% Now put it all together.
	%
	{ MLDS_Decls = [] },
	{ MakeNewObject = new_object(VarLval, MaybeTag, MLDS_Type,
		yes(SizeInBytesRval), yes(CtorName), ArgRvals,
		MLDS_ArgTypes) },
	{ MLDS_Stmt = atomic(MakeNewObject) },
	{ MLDS_Statement = mlds__statement(MLDS_Stmt,
		mlds__make_context(Context)) },
	{ MLDS_Statements = [MLDS_Statement] }.

%-----------------------------------------------------------------------------%
	%
	% ml_gen_closure_wrapper:
	% Generate a wrapper function which unboxes the input arguments,
	% calls the specified procedure, and then boxes the output arguments.
	%
	% The generated function will be of the following form:
	%
	%	foo_wrapper(void *closure_arg,
	%			MR_Box arg1, MR_Box *arg2, ..., MR_Box argn)
	%	{
	%		FooClosure *closure;
	%		...
	%		/* declarations needed for converting output args */
	%		Arg2Type conv_arg2;
	%		...
	%		bool succeeded;
	%		
	%		closure = closure_arg; 	/* XXX should add cast */
	%
	%	    CONJ(code_model, 
	%		/* call function, boxing/unboxing inputs if needed */
	%		foo(closure->f1, unbox(closure->f2), ...,
	%			unbox(arg1), &unboxed_arg2, arg3, ...);
	%	    ,
	%		/* box output arguments */
	%		*arg2 = box(unboxed_arg2);
	%		...
	%	    )
	%	}
	%
	% where the stuff in CONJ() expands to the appropriate code
	% for a conjunction, which depends on the code model:
	%
	% #if MODEL_DET
	%		/* call function, boxing/unboxing inputs if needed */
	%		foo(closure->f1, unbox(closure->f2), ...,
	%			unbox(arg1), &unboxed_arg2, arg3, ...);
	%
	%		/* box output arguments */
	%		*arg2 = box(unboxed_arg2);
	%		...
	% #elif MODEL_SEMI
	%		/* call function, boxing/unboxing inputs if needed */
	%		succeeded = foo(closure->f1, unbox(closure->f2), ...,
	%			unbox(arg1), &unboxed_arg2, arg3, ...);
	%		
	%		if (succeeded) {
	%			/* box output arguments */
	%			*arg2 = box(unboxed_arg2);
	%			...
	%		}
	%
	%		return succeeded;
	%	}
	% #else /* MODEL_NON */
	%		foo_1() {
	%			/* box output arguments */
	%			*arg2 = box(unboxed_arg2);
	%			...
	%			(*succ_cont)();
	%		}
	%			
	%		/* call function, boxing/unboxing inputs if needed */
	%		foo(closure->f1, unbox(closure->f2), ...,
	%			unbox(arg1), &unboxed_arg2, arg3, ...,
	%			foo_1);
	% #endif
	%
:- pred ml_gen_closure_wrapper(pred_id, proc_id, int, prog_type, prog_context,
		mlds__defn, mlds__rval, mlds__type,
		ml_gen_info, ml_gen_info).
:- mode ml_gen_closure_wrapper(in, in, in, in, in, out, out, out,
		in, out) is det.

ml_gen_closure_wrapper(PredId, ProcId, NumClosureArgs, _ClosureType,
		Context, WrapperFunc, WrapperFuncRval, WrapperFuncType) -->
	%
	% grab the relevant information about the called procedure
	%
	=(Info),
	{ ml_gen_info_get_module_info(Info, ModuleInfo) },
	{ module_info_pred_proc_info(ModuleInfo, PredId, ProcId,
		_PredInfo, ProcInfo) },
	{ proc_info_headvars(ProcInfo, ProcHeadVars) },
	{ proc_info_argmodes(ProcInfo, ProcModes) },
	{ ProcModes = argument_modes(InstTable, ProcArgModes) },
	{ proc_info_interface_code_model(ProcInfo, CodeModel) },
	{ proc_info_varset(ProcInfo, ProcVarSet) },
	{ ProcArity = list__length(ProcHeadVars) },
	{ ProcHeadVarNames = ml_gen_var_names(ProcVarSet, ProcHeadVars) },

	%
	% allocate some fresh type variables to use as the Mercury types
	% of the boxed arguments
	%
	{ varset__init(TypeVarSet0) },
	{ varset__new_vars(TypeVarSet0, ProcArity, ProcBoxedArgTypeVars,
		_TypeVarSet) },
	{ term__var_list_to_term_list(ProcBoxedArgTypeVars,
		ProcBoxedArgTypes) },

	%
	% compute the parameters for the wrapper function
	%	(void *closure_arg,
	%	MR_Box arg1, MR_Box *arg2, ..., MR_Box argn)
	%

	% first generate the declarations for the boxed arguments
	{ 
		list__drop(NumClosureArgs, ProcHeadVars, WrapperHeadVars0),
		list__drop(NumClosureArgs, ProcArgModes, WrapperArgModes0),
		list__drop(NumClosureArgs, ProcBoxedArgTypes,
			WrapperBoxedArgTypes0)
	->
		WrapperHeadVars = WrapperHeadVars0,
		WrapperArgModes = WrapperArgModes0,
		WrapperBoxedArgTypes = WrapperBoxedArgTypes0
	;
		error("ml_gen_closure_wrapper: list__drop failed")
	},
	{ WrapperHeadVarNames = ml_gen_wrapper_head_var_names(1,
		list__length(WrapperHeadVars)) },
	% The argument modes do not depend on the alias substitutions
	% in the instmap.
	{ instmap__init_reachable(InstMap) },
	{ WrapperParams0 = ml_gen_params(ModuleInfo,
		WrapperHeadVarNames, WrapperBoxedArgTypes, WrapperArgModes,
		InstMap, InstTable, CodeModel) },

	% then insert the `closure_arg' parameter
	{ ClosureArg = data(var("closure_arg")) - mlds__generic_env_ptr_type },
	{ WrapperParams0 = mlds__func_params(WrapperArgs0, WrapperRetType) },
	{ WrapperParams = mlds__func_params([ClosureArg | WrapperArgs0],
		WrapperRetType) },

	%
	% generate code to declare and initialize the closure pointer.
	% XXX we should use a struct type for the closure, but
	% currently we're using a low-level data representation
	% in the closure
	%
	% #if HIGH_LEVEL_DATA
	%	FooClosure *closure;
	% #else
	%	void *closure;
	% #endif
	%	closure = closure_arg;
	%
	{ ClosureName = "closure" },
	{ ClosureArgName = "closure_arg" },
	{ ClosureDecl = ml_gen_mlds_var_decl(ClosureName,
		mlds__generic_env_ptr_type, MLDS_Context) },
	ml_qualify_var(ClosureName, ClosureLval),
	ml_qualify_var(ClosureArgName, ClosureArgLval),
	{ AssignClosure = assign(ClosureLval, lval(ClosureArgLval)) },
	{ MLDS_Context = mlds__make_context(Context) },
	{ InitClosure = mlds__statement(atomic(AssignClosure), MLDS_Context) },

	%
	% if the wrapper function is model_non, then
	% set up the initial success continuation;
	% this is needed by ml_gen_call which we call below
	%
	( { CodeModel = model_non } ->
		ml_initial_cont(InitialCont),
		ml_gen_info_push_success_cont(InitialCont)
	;
		[]
	),

	% prepare to generate code to call the function:
	% XXX currently we're using a low-level data representation
	% in the closure
	%
	%	foo(
	% #if HIGH_LEVEL_DATA
	%		closure->arg1, closure->arg2, ...,
	% #else
	%		MR_field(MR_mktag(0), closure, 3),
	%		MR_field(MR_mktag(0), closure, 4),
	%		...
	% #endif
	%		unbox(arg1), &unboxed_arg2, arg3, ...
	%	);
	%
	% field 0 is the closure layout
	% field 1 is the closure address
	% field 2 is the number of arguments
	% field 3 is the first argument field
	{ Offset = 2 },
	ml_gen_closure_field_lvals(ClosureLval, Offset, 1, NumClosureArgs,
		ClosureArgLvals),
	ml_gen_wrapper_arg_lvals(WrapperHeadVarNames, WrapperBoxedArgTypes,
		WrapperArgModes, WrapperHeadVarLvals),
	{ CallLvals = list__append(ClosureArgLvals, WrapperHeadVarLvals) },
	ml_gen_call(PredId, ProcId, ProcHeadVarNames, CallLvals,
		ProcBoxedArgTypes, CodeModel, Context, Decls0, Statements0),

	% insert the stuff to declare and initialize the closure
	{ Decls1 = [ClosureDecl | Decls0] },
	{ Statements1 = [InitClosure | Statements0] },

	%
	% For semidet code, add the declaration `bool succeeded;'
	% and the `return succeeded;' statement.
	%
	( { CodeModel = model_semi } ->
		{ SucceededVarDecl = ml_gen_succeeded_var_decl(MLDS_Context) },
		{ Decls = [SucceededVarDecl | Decls1] },
		ml_gen_test_success(Succeeded),
		{ ReturnStmt = return([Succeeded]) },
		{ ReturnStatement = mlds__statement(ReturnStmt, MLDS_Context) },
		{ Statements = list__append(Statements1, [ReturnStatement]) }
	;
		{ Decls = Decls1 },
		{ Statements = Statements1 }
	),

	%
	% if the wrapper function was model_non, then
	% pop the success continuation that we pushed
	%
	( { CodeModel = model_non } ->
		ml_gen_info_pop_success_cont
	;
		[]
	),

	%
	% Put it all together
	%
	{ WrapperFuncBody = ml_gen_block(Decls, Statements, Context) },
	ml_gen_new_func_label(WrapperFuncName, WrapperFuncRval),
	ml_gen_label_func(WrapperFuncName, WrapperParams, Context,
		WrapperFuncBody, WrapperFunc),
	{ WrapperFuncType = mlds__func_type(WrapperParams) }.


:- func ml_gen_wrapper_head_var_names(int, int) = list(string).
ml_gen_wrapper_head_var_names(Num, Max) = Names :-
	( Num > Max ->
		Names = []
	;
		Name = string__format("wrapper_arg_%d", [i(Num)]),
		Names1 = ml_gen_wrapper_head_var_names(Num + 1, Max),
		Names = [Name | Names1]
	).

	% ml_gen_wrapper_arg_lvals(HeadVarNames, ArgModes, HeadVarLvals):
	%	Generate lvals for the specified head variables
	%	passed in the specified modes.
	%
:- pred ml_gen_wrapper_arg_lvals(list(var_name), list(prog_type), list(mode),
		list(mlds__lval), ml_gen_info, ml_gen_info).
:- mode ml_gen_wrapper_arg_lvals(in, in, in, out, in, out) is det.

ml_gen_wrapper_arg_lvals(Names, Types, Modes, Lvals) -->
	(
		{ Names = [], Types = [], Modes = [] }
	->
		{ Lvals = [] }
	;
		{ Names = [Name|Names1] },
		{ Types = [Type|Types1] },
		{ Modes = [Mode|Modes1] }
	->
		ml_qualify_var(Name, VarLval),
		=(Info),
		{ ml_gen_info_get_module_info(Info, ModuleInfo) },
		{ ml_gen_info_get_instmap(Info, InstMap) },
		{ ml_gen_info_get_inst_table(Info, InstTable) },
		{
			mode_to_arg_mode(InstMap, InstTable,
				ModuleInfo, Mode, Type, top_in)
		->
			Lval = VarLval
		;
			% output arguments are passed by reference,
			% so we need to dereference them
			Lval = mem_ref(lval(VarLval))
		},
		ml_gen_wrapper_arg_lvals(Names1, Types1, Modes1, Lvals1),
		{ Lvals = [Lval|Lvals1] }
	;
		{ error("ml_gen_wrapper_arg_lvals: length mismatch") }
	).

:- pred ml_gen_closure_field_lvals(mlds__lval, int, int, int,
		list(mlds__lval),
		ml_gen_info, ml_gen_info).
:- mode ml_gen_closure_field_lvals(in, in, in, in, out, in, out) is det.

ml_gen_closure_field_lvals(ClosureLval, Offset, ArgNum, NumClosureArgs,
		ClosureArgLvals) -->
	( { ArgNum > NumClosureArgs } ->
		{ ClosureArgLvals = [] }
	;
		%
		% generate `MR_field(MR_mktag(0), closure, <N>)'
		%
		{ FieldId = offset(const(int_const(ArgNum + Offset))) },
		{ FieldLval = field(yes(0), lval(ClosureLval), FieldId) },
		%
		% recursively handle the remaining fields
		%
		ml_gen_closure_field_lvals(ClosureLval, Offset, ArgNum + 1,
			NumClosureArgs, ClosureArgLvals0),
		{ ClosureArgLvals = [FieldLval | ClosureArgLvals0] }
	).
		
	% convert a cons_id for a given type to a cons_tag
:- pred ml_cons_id_to_tag(cons_id, prog_type, cons_tag,
		ml_gen_info, ml_gen_info).
:- mode ml_cons_id_to_tag(in, in, out, in, out) is det.

ml_cons_id_to_tag(ConsId, Type, Tag) -->
	=(Info),
	{ ml_gen_info_get_module_info(Info, ModuleInfo) },
	{ code_util__cons_id_to_tag(ConsId, Type, ModuleInfo, Tag) }.

	% generate code to construct a new object
:- pred ml_gen_new_object(mlds__tag, maybe(int), cons_id, prog_var, prog_vars,
		list(uni_mode), prog_context, mlds__defns, mlds__statements,
		ml_gen_info, ml_gen_info).
:- mode ml_gen_new_object(in, in, in, in, in, in, in, out, out, in, out)
		is det.

ml_gen_new_object(Tag, MaybeSecondaryTag, ConsId, Var, ArgVars, ArgModes,
		Context, MLDS_Decls, MLDS_Statements) -->
	%
	% Determine the variable's type and lval,
	% and determine the constructor name and the tag to use.
	%
	ml_variable_type(Var, Type),
	{ MLDS_Type = mercury_type_to_mlds_type(Type) },
	ml_gen_var(Var, Lval),
	ml_cons_name(ConsId, CtorName),
	{ Tag = 0 ->
		MaybeTag = no
	;
		MaybeTag = yes(Tag)
	},

	%
	% Generate rvals for the arguments
	%
	ml_gen_var_list(ArgVars, ArgLvals),
	ml_variable_types(ArgVars, ArgTypes),
	{ MLDS_ArgTypes0 = list__map(mercury_type_to_mlds_type, ArgTypes) },
	=(Info),
	{ ml_gen_info_get_module_info(Info, ModuleInfo) },
	{ ml_gen_info_get_instmap(Info, InstMap) },
	{ ml_gen_info_get_inst_table(Info, InstTable) },
	{ ml_gen_cons_args(ArgLvals, ArgTypes, ArgModes, InstMap, InstTable,
		ModuleInfo, ArgRvals0) },

	% 
	% If there is a secondary tag, it goes in the first field
	%
	{ MaybeSecondaryTag = yes(SecondaryTag) ->
		SecondaryTagRval = const(int_const(SecondaryTag)),
		SecondaryTagType = mlds__int_type,
		ArgRvals = [SecondaryTagRval | ArgRvals0],
		MLDS_ArgTypes = [SecondaryTagType | MLDS_ArgTypes0]
	;
		ArgRvals = ArgRvals0,
		MLDS_ArgTypes = MLDS_ArgTypes0
	},

	%
	% Compute the number of bytes to allocate
	%
	{ list__length(ArgRvals, NumArgs) },
	{ SizeInWordsRval = const(int_const(NumArgs)) },
	{ SizeOfWordRval = ml_sizeof_word_rval },
	{ SizeInBytesRval = binop((*), SizeInWordsRval, SizeOfWordRval) },
	
	%
	% Now put it all together.
	%
	{ MakeNewObject = new_object(Lval, MaybeTag, MLDS_Type,
		yes(SizeInBytesRval), yes(CtorName), ArgRvals,
		MLDS_ArgTypes) },
	{ MLDS_Stmt = atomic(MakeNewObject) },
	{ MLDS_Statement = mlds__statement(MLDS_Stmt,
		mlds__make_context(Context)) },
	{ MLDS_Statements = [MLDS_Statement] },
	{ MLDS_Decls = [] }.

:- pred ml_cons_name(cons_id, ctor_name, ml_gen_info, ml_gen_info).
:- mode ml_cons_name(in, out, in, out) is det.

ml_cons_name(ConsId, ConsName) -->
	{ hlds_out__cons_id_to_string(ConsId, ConsName) }.

	% Return an rval for the `SIZEOF_WORD' constant.
	% This constant is supposed to be defined by the Mercury library.
	% It holds `sizeof(Word)'.  (Using this constant allows us to avoid
	% hard-coding the word size without having to add support for
	% `sizeof' to MLDS.)
	%
:- func ml_sizeof_word_rval = mlds__rval.
ml_sizeof_word_rval = SizeofWordRval :-
	mercury_private_builtin_module(PrivateBuiltin),
	MLDS_Module = mercury_module_name_to_mlds(PrivateBuiltin),
	SizeofWordRval = lval(var(qual(MLDS_Module, "SIZEOF_WORD"))).

:- pred ml_gen_cons_args(list(mlds__lval), list(prog_type),
		list(uni_mode), instmap, inst_table,
		module_info, list(mlds__rval)).
:- mode ml_gen_cons_args(in, in, in, in, in, in, out) is det.

ml_gen_cons_args(Lvals, Types, Modes, InstMap, InstTable, ModuleInfo, Rvals) :-
	(
		ml_gen_cons_args_2(Lvals, Types, Modes, InstMap, InstTable,
			ModuleInfo, Rvals0)
	->
		Rvals = Rvals0
	;
		error("ml_gen_cons_args: length mismatch")
	).

	% Create a list of rvals for the arguments
	% for a construction unification.  For each argument which
	% is input to the construction unification, we produce the
	% corresponding lval, but if the argument is free,
	% we just produce `0', meaning initialize that field to a
	% null value.  (XXX perhaps we should have a special `null' rval.)

:- pred ml_gen_cons_args_2(list(mlds__lval), list(prog_type),
		list(uni_mode), instmap, inst_table,
		module_info, list(mlds__rval)).
:- mode ml_gen_cons_args_2(in, in, in, in, in, in, out) is semidet.

ml_gen_cons_args_2([], [], [], _, _, _, []).
ml_gen_cons_args_2([Lval|Lvals], [Type|Types], [UniMode|UniModes],
			InstMap, InstTable, ModuleInfo, [Rval|Rvals]) :-
	UniMode = ((_LI - RI) -> (_LF - RF)),
	(
		mode_to_arg_mode(InstMap, InstTable, ModuleInfo,
			(RI -> RF), Type, top_in) ->
		Rval = lval(Lval)
	;
		% XXX perhaps we should have a special `null' rval.
		Rval = const(int_const(0))
	),
	ml_gen_cons_args_2(Lvals, Types, UniModes, InstMap, InstTable,
		ModuleInfo, Rvals).

%-----------------------------------------------------------------------------%

	% Generate a deterministic deconstruction. In a deterministic
	% deconstruction, we know the value of the tag, so we don't
	% need to generate a test.
	%
:- pred ml_gen_det_deconstruct(prog_var, cons_id, prog_vars, list(uni_mode),
		prog_context, mlds__defns, mlds__statements,
		ml_gen_info, ml_gen_info).
:- mode ml_gen_det_deconstruct(in, in, in, in, in, out, out, in, out) is det.

%	det (cannot_fail) deconstruction:
%		<succeeded = (X => f(A1, A2, ...))>
% 	===>
%		A1 = arg(X, f, 1);		% extract arguments
%		A2 = arg(X, f, 2);
%		...

ml_gen_det_deconstruct(Var, ConsId, Args, Modes, Context,
		MLDS_Decls, MLDS_Statements) -->
	{ MLDS_Decls = [] },
	ml_variable_type(Var, Type),
	ml_cons_id_to_tag(ConsId, Type, Tag),
	% For constants, if the deconstruction is det, then we already know
	% the value of the constant, so MLDS_Statements = [].
	(
		{ Tag = string_constant(_String) },
		{ MLDS_Statements = [] }
	;
		{ Tag = int_constant(_Int) },
		{ MLDS_Statements = [] }
	;
		{ Tag = float_constant(_Float) },
		{ MLDS_Statements = [] }
	;
		{ Tag = pred_closure_tag(_, _, _) },
		{ MLDS_Statements = [] }
	;
		{ Tag = code_addr_constant(_, _) },
		{ MLDS_Statements = [] }
	;
		{ Tag = type_ctor_info_constant(_, _, _) },
		{ MLDS_Statements = [] }
	;
		{ Tag = base_typeclass_info_constant(_, _, _) },
		{ MLDS_Statements = [] }
	;
		{ Tag = tabling_pointer_constant(_, _) },
		{ MLDS_Statements = [] }
	;
		{ Tag = no_tag },
		( { Args = [Arg], Modes = [Mode] } ->
			ml_variable_type(Arg, ArgType),
			ml_gen_var(Arg, ArgLval),
			ml_gen_var(Var, VarLval),
			ml_gen_sub_unify(ArgLval, Mode, ArgType, VarLval,
				Context, [], MLDS_Statements)
		;
			{ error("ml_code_gen: no_tag: arity != 1") }
		)
	;
		{ Tag = unshared_tag(UnsharedTag) },
		ml_gen_var(Var, VarLval),
		ml_variable_types(Args, ArgTypes),
		ml_gen_unify_args(Args, Modes, ArgTypes,
			VarLval, 0, UnsharedTag, Context, MLDS_Statements)
	;
		{ Tag = shared_remote_tag(PrimaryTag, _SecondaryTag) },
		ml_gen_var(Var, VarLval),
		ml_variable_types(Args, ArgTypes),
		ml_gen_unify_args(Args, Modes, ArgTypes,
			VarLval, 1, PrimaryTag, Context, MLDS_Statements)
	;
		{ Tag = shared_local_tag(_Bits1, _Num1) },
		{ MLDS_Statements = [] } % if this is det, then nothing happens
	).

:- pred ml_gen_unify_args(prog_vars, list(uni_mode), list(prog_type),
		mlds__lval, int, mlds__tag, prog_context,
		mlds__statements, ml_gen_info, ml_gen_info).
:- mode ml_gen_unify_args(in, in, in, in, in, in, in, out, in, out) is det.

ml_gen_unify_args(Args, Modes, ArgTypes, VarLval, ArgNum, PrimaryTag, Context,
		MLDS_Statements) -->
	(
		ml_gen_unify_args_2(Args, Modes, ArgTypes,
			VarLval, ArgNum, PrimaryTag, Context,
			[], MLDS_Statements0)
	->
		{ MLDS_Statements = MLDS_Statements0 }
	;
		{ error("ml_gen_unify_args: length mismatch") }
	).

:- pred ml_gen_unify_args_2(prog_vars, list(uni_mode), list(prog_type),
		mlds__lval, int, mlds__tag, prog_context,
		mlds__statements, mlds__statements, ml_gen_info, ml_gen_info).
:- mode ml_gen_unify_args_2(in, in, in, in, in, in, in, in, out, in, out)
		is semidet.

ml_gen_unify_args_2([], [], [], _, _, _, _, Statements, Statements) --> [].
ml_gen_unify_args_2([Arg|Args], [Mode|Modes], [ArgType|ArgTypes],
			VarLval, ArgNum, PrimaryTag, Context,
			MLDS_Statements0, MLDS_Statements) -->
	{ ArgNum1 = ArgNum + 1 },
	ml_gen_unify_args_2(Args, Modes, ArgTypes, VarLval, ArgNum1,
		PrimaryTag, Context, MLDS_Statements0, MLDS_Statements1),
	ml_gen_unify_arg(Arg, Mode, ArgType, VarLval, ArgNum, PrimaryTag,
		Context, MLDS_Statements1, MLDS_Statements).

:- pred ml_gen_unify_arg(prog_var, uni_mode, prog_type,
		mlds__lval, int, mlds__tag, prog_context,
		mlds__statements, mlds__statements, ml_gen_info, ml_gen_info).
:- mode ml_gen_unify_arg(in, in, in, in, in, in, in, in, out, in, out)
		is det.

ml_gen_unify_arg(Arg, Mode, ArgType, VarLval, ArgNum, PrimaryTag, Context,
		MLDS_Statements0, MLDS_Statements) -->
	%
	% Generate lvals for the LHS and the RHS
	%
	{ FieldId = offset(const(int_const(ArgNum))) },
	{ FieldLval = field(yes(PrimaryTag), lval(VarLval), FieldId) },
	ml_gen_var(Arg, ArgLval),
	%
	% Now generate code to unify them
	%
	ml_gen_sub_unify(ArgLval, Mode, ArgType, FieldLval, Context,
		MLDS_Statements0, MLDS_Statements).

:- pred ml_gen_sub_unify(mlds__lval, uni_mode, prog_type, mlds__lval,
		prog_context, mlds__statements, mlds__statements,
		ml_gen_info, ml_gen_info).
:- mode ml_gen_sub_unify(in, in, in, in, in, in, out, in, out) is det.

ml_gen_sub_unify(ArgLval, Mode, ArgType, FieldLval, Context,
		MLDS_Statements0, MLDS_Statements) -->
	%
	% With the current low-level data representation,
	% we store all fields as boxed, so we need to box
	% values when storing them into fields and unbox them
	% when extracting them from fields.
	% Hence we compute a polymorphic type here, for use in
	% the calls to ml_gen_box_or_unbox_rval below.
	% 
	{ varset__init(TypeVarSet0) },
	{ varset__new_var(TypeVarSet0, TypeVar, _TypeVarSet) },
	{ type_util__var(BoxedFieldType, TypeVar) },

	%
	% Figure out the direction of data-flow from the mode,
	% and generate code accordingly
	%
	{ Mode = ((LI - RI) -> (LF - RF)) },
	=(Info),
	{ ml_gen_info_get_module_info(Info, ModuleInfo) },
	{ ml_gen_info_get_instmap(Info, InstMap) },
	{ ml_gen_info_get_inst_table(Info, InstTable) },
	{ mode_to_arg_mode(InstMap, InstTable, ModuleInfo,
		(LI -> LF), ArgType, LeftMode) },
	{ mode_to_arg_mode(InstMap, InstTable, ModuleInfo,
		(RI -> RF), ArgType, RightMode) },
	(
		% skip dummy argument types, since they will not have
		% been declared
		{ type_util__is_dummy_argument_type(ArgType) }
	->
		{ MLDS_Statements = MLDS_Statements0 }
	;
		% both input: it's a test unification
		{ LeftMode = top_in },
		{ RightMode = top_in }
	->
		% This shouldn't happen, since mode analysis should
		% avoid creating any tests in the arguments
		% of a construction or deconstruction unification.
		{ error("test in arg of [de]construction") }
	;
		% input - output: it's an assignment to the RHS
		{ LeftMode = top_in },
		{ RightMode = top_out }
	->
		{ ml_gen_box_or_unbox_rval(BoxedFieldType, ArgType,
			lval(FieldLval), FieldRval) },
		{ MLDS_Statement = ml_gen_assign(ArgLval, FieldRval,
			Context) },
		{ MLDS_Statements = [MLDS_Statement | MLDS_Statements0] }
	;
		% output - input: it's an assignment to the LHS
		{ LeftMode = top_out },
		{ RightMode = top_in }
	->
		{ ml_gen_box_or_unbox_rval(ArgType, BoxedFieldType,
			lval(ArgLval), ArgRval) },
		{ MLDS_Statement = ml_gen_assign(FieldLval, ArgRval,
			Context) },
		{ MLDS_Statements = [MLDS_Statement | MLDS_Statements0] }
	;
		% unused - unused: the unification has no effect
		{ LeftMode = top_unused },
		{ RightMode = top_unused }
	->
		{ MLDS_Statements = MLDS_Statements0 }
	;
		{ error("ml_gen_sub_unify: some strange unify") }
	).

%-----------------------------------------------------------------------------%

	% Generate a semidet deconstruction.
	% A semidet deconstruction unification is tag test
	% followed by a deterministic deconstruction
	% (which is executed only if the tag test succeeds).
	%
:- pred ml_gen_semi_deconstruct(prog_var, cons_id, prog_vars, list(uni_mode),
		prog_context, mlds__defns, mlds__statements,
		ml_gen_info, ml_gen_info).
:- mode ml_gen_semi_deconstruct(in, in, in, in, in, out, out, in, out) is det.

%	semidet (can_fail) deconstruction:
%		<X => f(A1, A2, ...)>
% 	===>
%		<succeeded = (X => f(_, _, _, _))>	% tag test
%		if (succeeded) {
%			A1 = arg(X, f, 1);		% extract arguments
%			A2 = arg(X, f, 2);
%			...
%		}

ml_gen_semi_deconstruct(Var, ConsId, Args, ArgModes, Context,
		MLDS_Decls, MLDS_Statements) -->
	ml_gen_tag_test(Var, ConsId, TagTestDecls, TagTestStatements,
		TagTestExpression),
	ml_gen_set_success(TagTestExpression, Context, SetTagTestResult),
	ml_gen_det_deconstruct(Var, ConsId, Args, ArgModes, Context,
		GetArgsDecls, GetArgsStatements),
	{ GetArgsDecls = [], GetArgsStatements = [] ->
		MLDS_Decls = TagTestDecls,
		MLDS_Statements = list__append(TagTestStatements,
			[SetTagTestResult])
	;
		GetArgs = ml_gen_block(GetArgsDecls, GetArgsStatements,
			Context),
		IfStmt = if_then_else(TagTestExpression, GetArgs, no),
		IfStatement = mlds__statement(IfStmt,
			mlds__make_context(Context)),
		MLDS_Decls = TagTestDecls,
		MLDS_Statements = list__append(TagTestStatements,
			[SetTagTestResult, IfStatement])
	}.

	% ml_gen_tag_test_rval(Var, ConsId, Defns, Statements, Expression):
	%	Generate code to perform a tag test.
	%
	%	The test checks whether Var has the functor specified by
	%	ConsId.  The generated code may contain Defns, Statements
	%	and an Expression.  The Expression is a boolean rval.
	%	After execution of the Statements, Expression will evaluate
	%	to true iff the Var has the functor specified by ConsId.
	%
:- pred ml_gen_tag_test(prog_var, cons_id, mlds__defns, mlds__statements,
		mlds__rval, ml_gen_info, ml_gen_info).
:- mode ml_gen_tag_test(in, in, out, out, out, in, out) is det.

	% TODO: apply the reverse tag test optimization
	% for types with two functors (see unify_gen.m).

ml_gen_tag_test(Var, ConsId, TagTestDecls, TagTestStatements,
		TagTestExpression) -->
	ml_gen_var(Var, VarLval),
	ml_variable_type(Var, Type),
	ml_cons_id_to_tag(ConsId, Type, Tag),
	{ TagTestExpression = ml_gen_tag_test_rval(Tag, lval(VarLval)) },
	{ TagTestDecls = [] },
	{ TagTestStatements = [] }.

	% ml_gen_tag_test_rval(Tag, VarRval) = TestRval:
	%	TestRval is a Rval of type bool which evaluates to
	%	true if VarRval has the specified Tag and false otherwise.
	%
:- func ml_gen_tag_test_rval(cons_tag, mlds__rval) = mlds__rval.

ml_gen_tag_test_rval(string_constant(String), Rval) =
	binop(str_eq, Rval, const(string_const(String))).
ml_gen_tag_test_rval(float_constant(Float), Rval) =
	binop(float_eq, Rval, const(float_const(Float))).
ml_gen_tag_test_rval(int_constant(Int), Rval) =
	binop(eq, Rval, const(int_const(Int))).
ml_gen_tag_test_rval(pred_closure_tag(_, _, _), _Rval) = _TestRval :-
	% This should never happen, since the error will be detected
	% during mode checking.
	error("Attempted higher-order unification").
ml_gen_tag_test_rval(code_addr_constant(_, _), _Rval) = _TestRval :-
	% This should never happen
	error("Attempted code_addr unification").
ml_gen_tag_test_rval(type_ctor_info_constant(_, _, _), _) = _ :-
	% This should never happen
	error("Attempted type_ctor_info unification").
ml_gen_tag_test_rval(base_typeclass_info_constant(_, _, _), _) = _ :-
	% This should never happen
	error("Attempted base_typeclass_info unification").
ml_gen_tag_test_rval(tabling_pointer_constant(_, _), _) = _ :-
	% This should never happen
	error("Attempted tabling_pointer unification").
ml_gen_tag_test_rval(no_tag, _Rval) = const(true).
ml_gen_tag_test_rval(unshared_tag(UnsharedTag), Rval) =
	binop(eq, unop(std_unop(tag), Rval),
		  unop(std_unop(mktag), const(int_const(UnsharedTag)))).
ml_gen_tag_test_rval(shared_remote_tag(Bits, Num), Rval) =
	binop(and,
		binop(eq,	unop(std_unop(tag), Rval),
				unop(std_unop(mktag), const(int_const(Bits)))), 
		binop(eq,	lval(field(yes(Bits), Rval,
					offset(const(int_const(0))))),
				const(int_const(Num)))).
ml_gen_tag_test_rval(shared_local_tag(Bits, Num), Rval) =
	binop(eq, Rval,
		  mkword(Bits, unop(std_unop(mkbody), const(int_const(Num))))).

%-----------------------------------------------------------------------------%
%
% Code for handling success and failure
%

	% Generate code to succeed in the given code_model.
	%
:- pred ml_gen_success(code_model, prog_context, mlds__statements,
			ml_gen_info, ml_gen_info).
:- mode ml_gen_success(in, in, out, in, out) is det.

ml_gen_success(model_det, _, MLDS_Statements) -->
	%
	% det succeed:
	%	<do true>
	% ===>
	%	/* just fall through */
	%
	{ MLDS_Statements = [] }.
ml_gen_success(model_semi, Context, [SetSuccessTrue]) -->
	%
	% semidet succeed:
	%	<do true>
	% ===>
	%	succeeded = TRUE;
	%
	ml_gen_set_success(const(true), Context, SetSuccessTrue).
ml_gen_success(model_non, Context, [CallCont]) -->
	%
	% nondet succeed:
	%	<true && SUCCEED()>
	% ===>
	%	SUCCEED()
	%
	ml_gen_call_current_success_cont(Context, CallCont).

	% Generate code to fail in the given code_model.
	%
:- pred ml_gen_failure(code_model, prog_context, mlds__statements,
			ml_gen_info, ml_gen_info).
:- mode ml_gen_failure(in, in, out, in, out) is det.

ml_gen_failure(model_det, _, _) -->
	% this should never happen
	{ error("ml_code_gen: `fail' has determinism `det'") }.
ml_gen_failure(model_semi, Context, [SetSuccessFalse]) -->
	%
	% semidet fail:
	%	<do fail>
	% ===>
	%	succeeded = FALSE;
	%
	ml_gen_set_success(const(false), Context, SetSuccessFalse).
ml_gen_failure(model_non, _, MLDS_Statements) -->
	%
	% nondet fail:
	%	<fail && SUCCEED()>
	% ===>
	%	/* just fall through */
	%
	{ MLDS_Statements = [] }.

	% Return the lval for the `succeeded' flag.
	% (`succeeded' is a boolean variable used to record
	% the success or failure of model_semi procedures.)
	%
:- pred ml_success_lval(mlds__lval, ml_gen_info, ml_gen_info).
:- mode ml_success_lval(out, in, out) is det.
ml_success_lval(SucceededLval) -->
	=(MLDSGenInfo),
	{ ml_gen_info_get_module_name(MLDSGenInfo, ModuleName) },
	{ MLDS_Module = mercury_module_name_to_mlds(ModuleName) },
	{ SucceededLval = var(qual(MLDS_Module, "succeeded")) }.

	% Return an rval which will test the value of the `succeeded' flag.
	% (`succeeded' is a boolean variable used to record
	% the success or failure of model_semi procedures.)
	%
:- pred ml_gen_test_success(mlds__rval, ml_gen_info, ml_gen_info).
:- mode ml_gen_test_success(out, in, out) is det.
ml_gen_test_success(SucceededRval) -->
	ml_success_lval(SucceededLval),
	{ SucceededRval = lval(SucceededLval) }.
	
	% Generate code to set the `succeeded' flag to the
	% specified truth value.
	%
:- pred ml_gen_set_success(mlds__rval, prog_context, mlds__statement,
				ml_gen_info, ml_gen_info).
:- mode ml_gen_set_success(in, in, out, in, out) is det.
ml_gen_set_success(Value, Context, MLDS_Statement) -->
	ml_success_lval(Succeeded),
	{ Assign = assign(Succeeded, Value) },
	{ MLDS_Stmt = atomic(Assign) },
	{ MLDS_Statement = mlds__statement(MLDS_Stmt,
		mlds__make_context(Context)) }.

	% Return an rval for a pointer to the current environment
	% (the set of local variables in the containing procedure).
	% Note that we generate this as a dangling reference.
	% The ml_elim_nested pass will insert the declaration
	% of the env_ptr variable.
:- pred ml_get_env_ptr(mlds__rval, ml_gen_info, ml_gen_info).
:- mode ml_get_env_ptr(out, in, out) is det.
ml_get_env_ptr(EnvPtrRval) -->
	=(MLDSGenInfo),
	{ ml_gen_info_get_module_name(MLDSGenInfo, ModuleName) },
	{ MLDS_Module = mercury_module_name_to_mlds(ModuleName) },
	{ EnvPtrRval = lval(var(qual(MLDS_Module, "env_ptr"))) }.

	% Return an rval for a pointer to the current environment
	% (the set of local variables in the containing procedure).
:- pred ml_declare_env_ptr_arg(pair(mlds__entity_name, mlds__type),
		ml_gen_info, ml_gen_info).
:- mode ml_declare_env_ptr_arg(out, in, out) is det.
ml_declare_env_ptr_arg(Name - mlds__generic_env_ptr_type) -->
	{ Name = data(var("env_ptr_arg")) }.

	% Return rvals for the success continuation that was
	% passed as the current function's argument(s).
	% The success continuation consists of two parts, the
	% `cont' argument, and the `cont_env' argument.
	% The `cont' argument is a continuation function that
	% will be called when a model_non goal succeeds.
	% The `cont_env' argument is a pointer to the environment (set
	% of local variables in the containing procedure) for the continuation
	% function.  (If we're using gcc nested function, the `cont_env'
	% is not used.)
	%
:- pred ml_initial_cont(success_cont, ml_gen_info, ml_gen_info).
:- mode ml_initial_cont(out, in, out) is det.
ml_initial_cont(Cont) -->
	=(MLDSGenInfo),
	{ ml_gen_info_get_module_name(MLDSGenInfo, ModuleName) },
	{ MLDS_Module = mercury_module_name_to_mlds(ModuleName) },
	{ ContRval = lval(var(qual(MLDS_Module, "cont"))) },
	{ ContEnvRval = lval(var(qual(MLDS_Module, "cont_env_ptr"))) },
	{ Cont = success_cont(ContRval, ContEnvRval) }.

	% Generate code to call the current success continuation.
	% This is used for generating success when in a model_non context.
	%
:- pred ml_gen_call_current_success_cont(prog_context, mlds__statement,
			ml_gen_info, ml_gen_info).
:- mode ml_gen_call_current_success_cont(in, out, in, out) is det.

ml_gen_call_current_success_cont(Context, MLDS_Statement) -->
	ml_gen_info_current_success_cont(SuccCont),
	{ SuccCont = success_cont(FuncRval, EnvPtrRval) },
	ml_gen_info_use_gcc_nested_functions(UseNestedFuncs),
	( { UseNestedFuncs = yes } ->
		{ ArgTypes = [] }
	;
		{ ArgTypes = [mlds__generic_env_ptr_type] }
	),
	{ RetTypes = [] },
	{ Signature = mlds__func_signature(ArgTypes, RetTypes) },
	{ ObjectRval = no },
	( { UseNestedFuncs = yes } ->
		{ ArgRvals = [] }
	;
		{ ArgRvals = [EnvPtrRval] }
	),
	{ RetLvals = [] },
	{ CallOrTailcall = call },
	{ MLDS_Stmt = call(Signature, FuncRval, ObjectRval, ArgRvals, RetLvals,
			CallOrTailcall) },
	{ MLDS_Statement = mlds__statement(MLDS_Stmt,
			mlds__make_context(Context)) }.

:- pred ml_gen_info_use_gcc_nested_functions(bool, ml_gen_info, ml_gen_info).
:- mode ml_gen_info_use_gcc_nested_functions(out, in, out) is det.

ml_gen_info_use_gcc_nested_functions(UseNestedFuncs) -->
	=(Info),
	{ ml_gen_info_get_module_info(Info, ModuleInfo) },
	{ module_info_globals(ModuleInfo, Globals) },
	{ globals__lookup_bool_option(Globals, gcc_nested_functions,
		UseNestedFuncs) }.

%-----------------------------------------------------------------------------%
%
% Code for generating mlds__entity_names.
%

:- type ml_label_func == mlds__func_sequence_num.
				% A number corresponding to an MLDS
				% nested function which serves as a label
				% (i.e. a continuation function).

	% Generate the mlds__entity_name for the entry point function
	% corresponding to a given procedure.
	%
:- func ml_gen_proc_label(module_info, pred_id, proc_id) = mlds__entity_name.
ml_gen_proc_label(ModuleInfo, PredId, ProcId) = 
	ml_gen_func_label(ModuleInfo, PredId, ProcId, no).

	% Generate an mlds__entity_name for a continuation function
	% with the given sequence number.  The pred_id and proc_id
	% specify the procedure that this continuation function
	% is part of.
	%
:- func ml_gen_nondet_label(module_info, pred_id, proc_id, ml_label_func)
		= mlds__entity_name.
ml_gen_nondet_label(ModuleInfo, PredId, ProcId, SeqNum) =
	ml_gen_func_label(ModuleInfo, PredId, ProcId, yes(SeqNum)).

:- func ml_gen_func_label(module_info, pred_id, proc_id,
		maybe(ml_label_func)) = mlds__entity_name.
ml_gen_func_label(ModuleInfo, PredId, ProcId, MaybeSeqNum) = MLDS_Name :-
	ml_gen_pred_label(ModuleInfo, PredId, ProcId, MLDS_PredLabel, _),
	MLDS_Name = function(MLDS_PredLabel, ProcId, MaybeSeqNum, PredId).

ml_gen_pred_label(ModuleInfo, PredId, ProcId, MLDS_PredLabel, MLDS_Module) :-
	module_info_pred_info(ModuleInfo, PredId, PredInfo),
	pred_info_module(PredInfo, PredModule),
	pred_info_name(PredInfo, PredName),
	module_info_name(ModuleInfo, ThisModule),
	(
		code_util__compiler_generated(PredInfo)
	->
		pred_info_arg_types(PredInfo, ArgTypes),
		(
			special_pred_get_type(PredName, ArgTypes, Type),
			type_to_type_id(Type, TypeId, _),
			% All type_ids here should be module qualified,
			% since builtin types are handled separately in
			% polymorphism.m.
			TypeId = qualified(TypeModule, TypeName) - Arity
		->
			(
				ThisModule \= TypeModule,
				PredName = "__Unify__",
				\+ hlds_pred__in_in_unification_proc_id(ProcId)
			->
				DeclaringModule = yes(TypeModule)
			;
				% the module declaring the type is the same as
				% the module defining this special pred
				DeclaringModule = no
			),
			MLDS_PredLabel = special_pred(PredName,
				DeclaringModule, TypeName, Arity),
			MLDS_Module = mercury_module_name_to_mlds(TypeModule)
		;
			string__append_list(["ml_gen_pred_label:\n",
				"cannot make label for special pred `",
				PredName, "'"], ErrorMessage),
			error(ErrorMessage)
		)
	;
		(
			% Work out which module supplies the code for
			% the predicate.
			ThisModule \= PredModule,
			\+ pred_info_is_imported(PredInfo)
		->
			% This predicate is a specialized version of 
			% a pred from a `.opt' file.
			MaybeDeclaringModule = yes(PredModule)
		;	
			% The predicate was declared in the same module
			% that it is defined in
			MaybeDeclaringModule = no
		),
		pred_info_get_is_pred_or_func(PredInfo, PredOrFunc),
		pred_info_arity(PredInfo, Arity),
		MLDS_PredLabel = pred(PredOrFunc, MaybeDeclaringModule,
				PredName, Arity),
		MLDS_Module = mercury_module_name_to_mlds(PredModule)
	).

%-----------------------------------------------------------------------------%
%
% Code for generating function declarations (i.e. mlds__func_params).
%

	% Generate the function prototype for a procedure.
	%
ml_gen_proc_params(ModuleInfo, PredId, ProcId) = FuncParams :-
	module_info_pred_proc_info(ModuleInfo, PredId, ProcId,
		PredInfo, ProcInfo),
	proc_info_varset(ProcInfo, VarSet),
	proc_info_headvars(ProcInfo, HeadVars),
	pred_info_arg_types(PredInfo, HeadTypes),
	proc_info_argmodes(ProcInfo, Modes),
	proc_info_interface_code_model(ProcInfo, CodeModel),
	HeadVarNames = ml_gen_var_names(VarSet, HeadVars),
	Modes = argument_modes(InstTable, HeadModes),
	% The argument modes do not depend on the alias substitutions in
	% the instmap.
	instmap__init_reachable(InstMap),
	FuncParams = ml_gen_params(ModuleInfo, HeadVarNames, HeadTypes,
		HeadModes, InstMap, InstTable, CodeModel).

:- func ml_gen_params(module_info, list(string), list(prog_data__type),
	list(mode), instmap, inst_table, code_model) = mlds__func_params.

ml_gen_params(ModuleInfo, HeadVarNames, HeadTypes, HeadModes, InstMap,
		InstTable, CodeModel) = FuncParams :-
	( CodeModel = model_semi ->
		RetTypes = [mlds__bool_type]
	;
		RetTypes = []
	),
	ml_gen_arg_decls(InstMap, InstTable, ModuleInfo, HeadVarNames,
		HeadTypes, HeadModes, FuncArgs0),
	( CodeModel = model_non ->
		ContType = mlds__cont_type,
		ContName = data(var("cont")),
		ContArg = ContName - ContType,
		ContEnvType = mlds__generic_env_ptr_type,
		ContEnvName = data(var("cont_env_ptr")),
		ContEnvArg = ContEnvName - ContEnvType,
		(
			module_info_globals(ModuleInfo, Globals),
			globals__lookup_bool_option(Globals,
				gcc_nested_functions, yes)
		->
			FuncArgs = list__append(FuncArgs0, [ContArg])
		;
			FuncArgs = list__append(FuncArgs0,
				[ContArg, ContEnvArg])
		)
	;
		FuncArgs = FuncArgs0
	),
	FuncParams = mlds__func_params(FuncArgs, RetTypes).

	% Given the argument variable names, and corresponding lists of their
	% types and modes, generate the MLDS argument list declaration.
	%
:- pred ml_gen_arg_decls(instmap, inst_table, module_info,
		list(mlds__var_name), list(prog_type),
		list(mode), mlds__arguments).
:- mode ml_gen_arg_decls(in, in, in, in, in, in, out) is det.

ml_gen_arg_decls(InstMap, InstTable, ModuleInfo, HeadVars, HeadTypes,
		HeadModes, FuncArgs) :-
	(
		HeadVars = [], HeadTypes = [], HeadModes = []
	->
		FuncArgs = []
	;	
		HeadVars = [Var | Vars],
		HeadTypes = [Type | Types],
		HeadModes = [Mode | Modes]
	->
		ml_gen_arg_decls(InstMap, InstTable, ModuleInfo,
			Vars, Types, Modes, FuncArgs0),
		% exclude types such as io__state, etc.
		( type_util__is_dummy_argument_type(Type) ->
			FuncArgs = FuncArgs0
		;
			ml_gen_arg_decl(InstMap, InstTable,
				ModuleInfo, Var, Type, Mode, FuncArg),
			FuncArgs = [FuncArg | FuncArgs0]
		)
	;
		error("ml_gen_arg_decls: length mismatch")
	).

	% Given an argument variable, and its type and mode,
	% generate an MLDS argument declaration for it.
	%
:- pred ml_gen_arg_decl(instmap, inst_table, module_info, var_name, prog_type,
			mode, pair(mlds__entity_name, mlds__type)).
:- mode ml_gen_arg_decl(in, in, in, in, in, in, out) is det.

ml_gen_arg_decl(InstMap, InstTable, ModuleInfo, Var, Type, Mode, FuncArg) :-
	MLDS_Type = mercury_type_to_mlds_type(Type),
	(
		\+ mode_to_arg_mode(InstMap, InstTable,
			ModuleInfo, Mode, Type, top_in)
	->
		MLDS_ArgType = mlds__ptr_type(MLDS_Type)
	;
		MLDS_ArgType = MLDS_Type
	),
	Name = data(var(Var)),
	FuncArg = Name - MLDS_ArgType.

	% Given a list of variables and their corresponding modes,
	% return a list containing only those variables which have
	% an output mode.
	%
:- func select_output_vars(instmap, inst_table, module_info, list(prog_var),
		list(mode), map(prog_var, prog_type)) = list(prog_var).

select_output_vars(InstMap, InstTable, ModuleInfo, HeadVars,
		HeadModes, VarTypes) = OutputVars :-
	( HeadVars = [], HeadModes = [] ->
		OutputVars = []
	; HeadVars = [Var|Vars], HeadModes = [Mode|Modes] ->
		map__lookup(VarTypes, Var, Type),
		(
			\+ mode_to_arg_mode(InstMap, InstTable,
				ModuleInfo, Mode, Type, top_in)
		->
			OutputVars1 = select_output_vars(InstMap, InstTable,
					ModuleInfo, Vars, Modes, VarTypes),
			OutputVars = [Var | OutputVars1]
		;
			OutputVars = select_output_vars(InstMap, InstTable,
					ModuleInfo, Vars, Modes, VarTypes)
		)
	;
		error("select_output_vars: length mismatch")
	).

%-----------------------------------------------------------------------------%
%
% miscellaneous helper routines
%

	% Generate a block statement, i.e. `{ <Decls>; <Statements>; }'.
	% But if the block consists only of a single statement with no
	% declarations, then just return that statement.
	%
:- func ml_gen_block(mlds__defns, mlds__statements, prog_context) =
		mlds__statement.

ml_gen_block(VarDecls, Statements, Context) =
	(if VarDecls = [], Statements = [SingleStatement] then
		SingleStatement
	else
		mlds__statement(block(VarDecls, Statements),
			mlds__make_context(Context))
	).
		
	% Generate a list of the mlds__lvals corresponding to a
	% given list of prog_vars.
	%
:- pred ml_gen_var_list(list(prog_var), list(mlds__lval),
		ml_gen_info, ml_gen_info).
:- mode ml_gen_var_list(in, out, in, out) is det.

ml_gen_var_list([], []) --> [].
ml_gen_var_list([Var | Vars], [Lval | Lvals]) -->
	ml_gen_var(Var, Lval),
	ml_gen_var_list(Vars, Lvals).

	% Generate the mlds__lval corresponding to a given prog_var.
	%
:- pred ml_gen_var(prog_var, mlds__lval, ml_gen_info, ml_gen_info).
:- mode ml_gen_var(in, out, in, out) is det.

ml_gen_var(Var, Lval) -->
	ml_variable_type(Var, Type),
	( { type_util__is_dummy_argument_type(Type) } ->
		%
		% The variable won't have been declared, so
		% we need to generate a dummy lval for this variable.
		%
		{ mercury_private_builtin_module(PrivateBuiltin) },
		{ MLDS_Module = mercury_module_name_to_mlds(PrivateBuiltin) },
		{ Lval = var(qual(MLDS_Module, "dummy_var")) }
	;
		=(MLDSGenInfo),
		{ ml_gen_info_get_output_vars(MLDSGenInfo, OutputVars) },
		{ ml_gen_info_get_varset(MLDSGenInfo, VarSet) },
		{ ml_gen_info_get_module_name(MLDSGenInfo, ModuleName) },
		{ MLDS_Module = mercury_module_name_to_mlds(ModuleName) },
		{ VarName = ml_gen_var_name(VarSet, Var) },
		{ VarLval = var(qual(MLDS_Module, VarName)) },
		% output variables are passed by reference...
		{ list__member(Var, OutputVars) ->
			Lval = mem_ref(lval(VarLval))
		;
			Lval = VarLval
		}
	).

	% Lookup the types of a list of variables.
	%
:- pred ml_variable_types(list(prog_var), list(prog_type),
		ml_gen_info, ml_gen_info).
:- mode ml_variable_types(in, out, in, out) is det.

ml_variable_types([], []) --> [].
ml_variable_types([Var | Vars], [Type | Types]) -->
	ml_variable_type(Var, Type),
	ml_variable_types(Vars, Types).

	% Lookup the type of a variable.
	%
:- pred ml_variable_type(prog_var, prog_type, ml_gen_info, ml_gen_info).
:- mode ml_variable_type(in, out, in, out) is det.

ml_variable_type(Var, Type) -->
	=(MLDSGenInfo),
	{ ml_gen_info_get_var_types(MLDSGenInfo, VarTypes) },
	{ map__lookup(VarTypes, Var, Type) }.

%-----------------------------------------------------------------------------%

	% Call error/1 with a "Sorry, not implemented" message.
	%
:- pred sorry(string::in) is erroneous.

sorry(What) :-
	string__format("ml_code_gen.m: Sorry, not implemented: %s",
		[s(What)], ErrorMessage),
	error(ErrorMessage).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- type prog_type == prog_data__type.

%
% The `ml_gen_info' type holds information used during MLDS code generation
% for a given procedure.
%
% Only the `func_sequence_num', `commit_sequence_num', and
% `stack(success_cont)' fields are mutable; the others are set
% when the `ml_gen_info' is created and then never modified.
% 

:- type ml_gen_info
	--->	ml_gen_info(
			%
			% these fields remain constant for each procedure
			%

			module_info,
			pred_id,
			proc_id,
			prog_varset,
			map(prog_var, prog_type),
			list(prog_var),			% output arguments

			%
			% these fields get updated as we traverse
			% each procedure
			%

			mlds__func_sequence_num,
			commit_sequence_num,
			stack(success_cont),
				% definitions of functions or global
				% constants which should be inserted
				% before the definition of the function
				% for the current procedure
			mlds__defns,
			instmap,
			inst_table
		).

:- type commit_sequence_num == int.

:- func ml_gen_info_init(module_info, pred_id, proc_id) = ml_gen_info.

ml_gen_info_init(ModuleInfo, PredId, ProcId) = MLDSGenInfo :-
	module_info_pred_proc_info(ModuleInfo, PredId, ProcId,
			_PredInfo, ProcInfo),
	proc_info_headvars(ProcInfo, HeadVars),
	proc_info_varset(ProcInfo, VarSet),
	proc_info_vartypes(ProcInfo, VarTypes),
	proc_info_argmodes(ProcInfo, Modes),
	Modes = argument_modes(_, HeadModes),
	proc_info_get_initial_instmap(ProcInfo, ModuleInfo, InstMap),
	proc_info_inst_table(ProcInfo, InstTable),
	OutputVars = select_output_vars(InstMap, InstTable,
			ModuleInfo, HeadVars, HeadModes, VarTypes),
	FuncLabelCounter = 0,
	CommitLabelCounter = 0,
	stack__init(SuccContStack),
	ExtraDefns = [],
	MLDSGenInfo = ml_gen_info(
			ModuleInfo,
			PredId,
			ProcId,
			VarSet,
			VarTypes,
			OutputVars,
			FuncLabelCounter,
			CommitLabelCounter,
			SuccContStack,
			ExtraDefns,
			InstMap,
			InstTable
		).

:- pred ml_gen_info_get_module_info(ml_gen_info, module_info).
:- mode ml_gen_info_get_module_info(in, out) is det.

ml_gen_info_get_module_info(
	ml_gen_info(ModuleInfo, _, _, _, _, _, _, _, _, _, _, _),
	ModuleInfo).

:- pred ml_gen_info_get_module_name(ml_gen_info, mercury_module_name).
:- mode ml_gen_info_get_module_name(in, out) is det.

ml_gen_info_get_module_name(MLDSGenInfo, ModuleName) :-
	ml_gen_info_get_module_info(MLDSGenInfo, ModuleInfo),
	module_info_name(ModuleInfo, ModuleName).

:- pred ml_gen_info_get_pred_id(ml_gen_info, pred_id).
:- mode ml_gen_info_get_pred_id(in, out) is det.

ml_gen_info_get_pred_id(ml_gen_info(_, PredId, _, _, _, _, _, _, _, _, _, _),
		PredId).

:- pred ml_gen_info_get_proc_id(ml_gen_info, proc_id).
:- mode ml_gen_info_get_proc_id(in, out) is det.

ml_gen_info_get_proc_id(ml_gen_info(_, _, ProcId, _, _, _, _, _, _, _, _, _),
		ProcId).

:- pred ml_gen_info_get_varset(ml_gen_info, prog_varset).
:- mode ml_gen_info_get_varset(in, out) is det.

ml_gen_info_get_varset(ml_gen_info(_, _, _, VarSet, _, _, _, _, _, _, _, _),
		VarSet).

:- pred ml_gen_info_get_var_types(ml_gen_info, map(prog_var, prog_type)).
:- mode ml_gen_info_get_var_types(in, out) is det.

ml_gen_info_get_var_types(
	ml_gen_info(_, _, _, _, VarTypes, _, _, _, _, _, _, _),
	VarTypes).

:- pred ml_gen_info_get_output_vars(ml_gen_info, list(prog_var)).
:- mode ml_gen_info_get_output_vars(in, out) is det.

ml_gen_info_get_output_vars(
	ml_gen_info(_, _, _, _, _, OutputVars, _, _, _, _, _, _),
	OutputVars).

:- pred ml_gen_info_new_func_label(ml_label_func, ml_gen_info, ml_gen_info).
:- mode ml_gen_info_new_func_label(out, in, out) is det.

ml_gen_info_new_func_label(Label,
		ml_gen_info(A, B, C, D, E, F, Label0, H, I, J, K, L),
		ml_gen_info(A, B, C, D, E, F, Label, H, I, J, K, L)) :-
	Label is Label0 + 1.

:- pred ml_gen_info_new_commit_label(commit_sequence_num,
		ml_gen_info, ml_gen_info).
:- mode ml_gen_info_new_commit_label(out, in, out) is det.

ml_gen_info_new_commit_label(CommitLabel,
		ml_gen_info(A, B, C, D, E, F, G, CommitLabel0, I, J, K, L),
		ml_gen_info(A, B, C, D, E, F, G, CommitLabel, I, J, K, L)) :-
	CommitLabel is CommitLabel0 + 1.

:- type success_cont 
	--->	success_cont(
			mlds__rval,	% function pointer
			mlds__rval	% environment pointer
				% note that if we're using nested
				% functions then the environment
				% pointer will not be used
		).

/******
:- pred ml_gen_info_get_success_cont_stack(ml_gen_info,
			stack(success_cont)).
:- mode ml_gen_info_get_success_cont_stack(in, out) is det.

ml_gen_info_get_success_cont_stack(
	ml_gen_info(_, _, _, _, _, _, _, _, SuccContStack, _, _, _),
	SuccContStack).

:- pred ml_gen_info_set_success_cont_stack(stack(success_cont),
			ml_gen_info, ml_gen_info).
:- mode ml_gen_info_set_success_cont_stack(in, in, out) is det.

ml_gen_info_set_success_cont_stack(SuccContStack,
		ml_gen_info(A, B, C, D, E, F, G, H, _, J, K, L),
		ml_gen_info(A, B, C, D, E, F, G, H, SuccContStack, J, K, L)).
********/

:- pred ml_gen_info_push_success_cont(success_cont,
			ml_gen_info, ml_gen_info).
:- mode ml_gen_info_push_success_cont(in, in, out) is det.

ml_gen_info_push_success_cont(SuccCont,
		ml_gen_info(A, B, C, D, E, F, G, H, Stack0, J, K, L),
		ml_gen_info(A, B, C, D, E, F, G, H, Stack, J, K, L)) :-
	stack__push(Stack0, SuccCont, Stack).

:- pred ml_gen_info_pop_success_cont(ml_gen_info, ml_gen_info).
:- mode ml_gen_info_pop_success_cont(in, out) is det.

ml_gen_info_pop_success_cont(
		ml_gen_info(A, B, C, D, E, F, G, H, Stack0, J, K, L),
		ml_gen_info(A, B, C, D, E, F, G, H, Stack, J, K, L)) :-
	stack__pop_det(Stack0, _SuccCont, Stack).

:- pred ml_gen_info_current_success_cont(success_cont,
			ml_gen_info, ml_gen_info).
:- mode ml_gen_info_current_success_cont(out, in, out) is det.

ml_gen_info_current_success_cont(SuccCont,
		ml_gen_info(A, B, C, D, E, F, G, H, Stack, J, K, L),
		ml_gen_info(A, B, C, D, E, F, G, H, Stack, J, K, L)) :-
	stack__top_det(Stack, SuccCont).

:- pred ml_gen_info_add_extra_defn(mlds__defn,
			ml_gen_info, ml_gen_info).
:- mode ml_gen_info_add_extra_defn(in, in, out) is det.

ml_gen_info_add_extra_defn(ExtraDefn,
		ml_gen_info(A, B, C, D, E, F, G, H, I, ExtraDefns0, K, L),
		ml_gen_info(A, B, C, D, E, F, G, H, I, ExtraDefns, K, L)) :-
	ExtraDefns = [ExtraDefn | ExtraDefns0].

:- pred ml_gen_info_get_extra_defns(ml_gen_info, mlds__defns).
:- mode ml_gen_info_get_extra_defns(in, out) is det.

ml_gen_info_get_extra_defns(
	ml_gen_info(_, _, _, _, _, _, _, _, _, ExtraDefns, _, _),
	ExtraDefns).

:- pred ml_gen_info_get_instmap(ml_gen_info, instmap).
:- mode ml_gen_info_get_instmap(in, out) is det.

ml_gen_info_get_instmap(
	ml_gen_info(_, _, _, _, _, _, _, _, _, _, InstMap, _),
	InstMap).

:- pred ml_gen_info_set_instmap(instmap, ml_gen_info, ml_gen_info).
:- mode ml_gen_info_set_instmap(in, in, out) is det.

ml_gen_info_set_instmap(InstMap,
		ml_gen_info(A, B, C, D, E, F, G, H, I, J, _, L),
		ml_gen_info(A, B, C, D, E, F, G, H, I, J, InstMap, L)).

:- pred ml_gen_info_update_instmap(hlds_goal,
		ml_gen_info, ml_gen_info).
:- mode ml_gen_info_update_instmap(in, in, out) is det.

ml_gen_info_update_instmap(_ - GoalInfo, Info0, Info) :-
	goal_info_get_instmap_delta(GoalInfo, InstMapDelta),
	ml_gen_info_get_instmap(Info0, InstMap0),
	instmap__apply_instmap_delta(InstMap0, InstMapDelta, InstMap),
	ml_gen_info_set_instmap(InstMap, Info0, Info).

:- pred ml_gen_info_get_inst_table(ml_gen_info, inst_table).
:- mode ml_gen_info_get_inst_table(in, out) is det.

ml_gen_info_get_inst_table(
	ml_gen_info(_, _, _, _, _, _, _, _, _, _, _, InstTable),
	InstTable).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
