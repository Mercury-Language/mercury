%-----------------------------------------------------------------------------%
% Copyright (C) 1999-2000 The University of Melbourne.
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

:- import_module hlds_module, mlds.
:- import_module io.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Generate MLDS code for an entire module.
	%
:- pred ml_code_gen(module_info, mlds, io__state, io__state).
:- mode ml_code_gen(in, out, di, uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module ml_base_type_info, ml_call_gen, ml_unify_gen, ml_code_util.
:- import_module llds. % XXX needed for `code_model'.
:- import_module export, llds_out. % XXX needed for pragma C code
:- import_module hlds_pred, hlds_goal, hlds_data, prog_data.
:- import_module goal_util, type_util, mode_util, builtin_ops.
:- import_module passes_aux.

:- import_module bool, string, list, map, set, require, std_util.

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
	Defns1 = list__append(ExtraDefns, [MLDS_ProcDefn | Defns0]),
	ml_gen_maybe_add_table_var(ModuleInfo, PredId, ProcId, ProcInfo,
		Defns1, Defns).

:- pred ml_gen_maybe_add_table_var(module_info, pred_id, proc_id, proc_info,
		mlds__defns, mlds__defns).
:- mode ml_gen_maybe_add_table_var(in, in, in, in, in, out) is det.

ml_gen_maybe_add_table_var(ModuleInfo, PredId, ProcId, ProcInfo,
		Defns0, Defns) :-
	proc_info_eval_method(ProcInfo, EvalMethod),
	(
		eval_method_uses_table(EvalMethod) = yes
	->
		ml_gen_pred_label(ModuleInfo, PredId, ProcId,
			MLDS_PredLabel, _MLDS_PredModule),
		Var = tabling_pointer(MLDS_PredLabel - ProcId),
		proc_info_context(ProcInfo, Context),
		TablePointerVarDefn = ml_gen_mlds_var_decl(
			Var, mlds__generic_type,
			mlds__make_context(Context)),
		Defns = [TablePointerVarDefn | Defns0]
	;
		Defns = Defns0
	).

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
	ml_gen_mlds_var_decl(var(VarName), mlds__commit_type, Context).

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
		pred_id, proc_id, list(prog_var), list(maybe(pair(string, mode))),
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
		PredId, _ProcId, ArgVars, ArgDatas, OrigArgTypes,
		C_Code, Context, MLDS_Decls, MLDS_Statements) -->
	%
	% Combine all the information about the each arg
	%
	{ ml_make_c_arg_list(ArgVars, ArgDatas, OrigArgTypes,
		ArgList) },

	%
	% Generate <declaration of one local variable for each arg>
	%
	{ ml_gen_pragma_c_decls(ArgList, ArgDeclsList) },

	%
	% Generate code to set the values of the input variables.
	%
	list__map_foldl(ml_gen_pragma_c_input_arg, ArgList, AssignInputsList),

	%
	% Generate code to assign the values of the output variables.
	%
	list__map_foldl(ml_gen_pragma_c_output_arg, ArgList, AssignOutputsList),

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
				"\n\t\t;}\n",
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
:- pred ml_gen_pragma_c_input_arg(ml_c_arg::in, string::out,
		ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_pragma_c_input_arg(ml_c_arg(Var, MaybeNameAndMode, OrigType),
		AssignInputString) -->
	=(MLDSGenInfo),
	{ ml_gen_info_get_module_info(MLDSGenInfo, ModuleInfo) },
	(
		{ MaybeNameAndMode = yes(ArgName - Mode) },
		{ \+ var_is_singleton(ArgName) },
		{ mode_to_arg_mode(ModuleInfo, Mode, OrigType, top_in) }
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
:- pred ml_gen_pragma_c_output_arg(ml_c_arg::in, string::out,
		ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_pragma_c_output_arg(ml_c_arg(Var, MaybeNameAndMode, OrigType),
		AssignOutputString) -->
	=(MLDSGenInfo),
	{ ml_gen_info_get_module_info(MLDSGenInfo, ModuleInfo) },
	(
		{ MaybeNameAndMode = yes(ArgName - Mode) },
		{ \+ var_is_singleton(ArgName) },
		{ \+ type_util__is_dummy_argument_type(OrigType) },
		{ mode_to_arg_mode(ModuleInfo, Mode, OrigType, top_out) }
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
% Code for switches
%

	% Generate MLDS code for a switch.
	%
:- pred ml_gen_switch(prog_var, can_fail, list(case), code_model, prog_context,
			mlds__defns, mlds__statements,
			ml_gen_info, ml_gen_info).
:- mode ml_gen_switch(in, in, in, in, in, out, out, in, out) is det.


:- type extended_case ---> case(int, cons_tag, cons_id, hlds_goal).
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
	{ Case = case(ConsId, Goal) },
	ml_variable_type(Var, Type),
	ml_cons_id_to_tag(ConsId, Type, Tag),
	{ ml_switch_priority(Tag, Priority) },
	{ TaggedCase = case(Priority, Tag, ConsId, Goal) },
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
	{ Case = case(_, _Tag, ConsId, Goal) },
	(
		{ Cases = [], CanFail = cannot_fail }
	->
		ml_gen_goal(CodeModel, Goal, MLDS_Decls, MLDS_Statements)
	;
		ml_gen_tag_test(Var, ConsId, TagTestDecls, TagTestStatements,
			TagTestExpression),
		ml_gen_goal(CodeModel, Goal, GoalStatement),
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
		ml_gen_goal(CodeModel, Then, ThenStatement),
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
		ml_gen_goal(model_non, Cond, CondDecls, CondStatements),
		ml_gen_info_pop_success_cont,
		ml_gen_test_success(Succeeded),
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
	{ First = _ - FirstGoalInfo },
	{ goal_info_get_code_model(FirstGoalInfo, FirstCodeModel) },
	{ DoGenFirst = ml_gen_goal(FirstCodeModel, First) },
	{ DoGenRest = ml_gen_conj(Rest, CodeModel, Context) },
	ml_combine_conj(FirstCodeModel, Context, DoGenFirst, DoGenRest,
		MLDS_Decls, MLDS_Statements).

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

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
