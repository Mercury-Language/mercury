%-----------------------------------------------------------------------------%
% Copyright (C) 1999-2002 The University of Melbourne.
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

% One of the aims of the MLDS is to be able to generated human-readable
% code in languages like C or Java.  This means that unlike the LLDS back-end,
% we do not want to rely on macros or conditional compilation.  If the
% final code is going to depend on the setting of some compilation option,
% our philosophy is to reflect that change in the generated MLDS and C code
% where possible, rather than generating C code which calls macros that do
% different things in different grades.  This is important both for
% readability of the generated code, and to make sure that we can easily
% adapt the MLDS code generator to target languages like Java that don't
% support macros or conditional compilation.

% A big challenge in generating MLDS code is handling nondeterminism.
% For nondeterministic procedures, we generate code using an explicit
% continuation passing style.  Each nondeterministic procedures gets
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
% In each procedure, we declare a local variable `MR_bool succeeded'.
% This is used to hold the success status of semidet sub-goals.
% Note that the comments below show local declarations for the
% `succeeded' variable in all the places where they would be
% needed if we were generating them locally, but currently
% we actually just generate a single `succeeded' variable for
% each procedure.
%
% The calling convention for sub-goals is as follows.
%
%	model_det goal:
%		On success, fall through.
%		(May clobber `succeeded'.)
%	model_semi goal:
%		On success, set `succeeded' to MR_TRUE and fall through.
%		On failure, set `succeeded' to MR_FALSE and fall through.
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
%			MR_TRUE if the goal succeeds and MR_FALSE if it fails.
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

% There is one other important thing which can be considered part of the
% calling convention for the code that we generate for each goal.
% If static ground term optimization is enabled, then for the terms
% marked as static by mark_static_terms.m, we will generate static consts.
% These static consts can refer to other static consts defined earlier.
% We need to be careful about the scopes of variables to ensure that
% for any term that mark_static_terms.m marks as static, the C constants
% representing the arguments of that term are in scope at the point
% where that term is constructed.  Basically this means that
% all the static consts generated inside a goal must be hoist out to
% the top level scope for that goal, except for goal types where
% goal_expr_mark_static_terms (in mark_static_terms.m) returns the
% same static_info unchanged, i.e. branched goals and negations.
%
% Handling static constants also requires that the calls to ml_gen_goal
% for each subgoal must be done in the right order, so that the
% const_num_map in the ml_gen_info holds the right sequence numbers
% for the constants in scope.

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
%		<do Goal>
%		succeeded = MR_TRUE;

%	det goal in nondet context:
%		<Goal && SUCCEED()>
%	===>
%		<do Goal>
%		SUCCEED();

%	semi goal in nondet context:
%		<Goal && SUCCEED()>
%	===>
%		MR_bool succeeded;
%	
%		<succeeded = Goal>
%		if (succeeded) SUCCEED();

%-----------------------------------------------------------------------------%
%
% Code for commits
%

% There's several different ways of handling commits:
%	- using catch/throw
%	- using setjmp/longjmp
%	- using GCC's __builtin_setjmp/__builtin_longjmp
%	- exiting nested functions via gotos to
%	  their containing functions
%
% The MLDS data structure abstracts away these differences
% using the `try_commit' and `do_commit' instructions.
% The comments below show the MLDS try_commit/do_commit version first,
% but for clarity I've also included sample code using each of the three
% different techniques.  This shows how the MLDS->target back-end can map
% mlds__commit_type, do_commit and try_commit into target language
% constructs.
%
% Note that if we're using GCC's __builtin_longjmp(),
% then it is important that the call to __builtin_longjmp() be
% put in its own function, to ensure that it is not in the same
% function as the __builtin_setjmp().
% The code generation schema below does that automatically.
% We will need to be careful with MLDS optimizations to
% ensure that we preserve that invariant, though.
% (Alternatively, we could just call a function that
% calls __builtin_longjmp() rather than calling it directly.
% But that would be a little less efficient.)
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
%		MR_COMMIT_TYPE ref;
%		void success() {
%			MR_DO_COMMIT(ref);
%		}
%		MR_TRY_COMMIT(ref, {
%			<Goal && success()>
%			succeeded = MR_FALSE;
%		}, {
%			succeeded = MR_TRUE;
%		})

%	model_non in semi context: (using catch/throw)
%		<succeeded = Goal>
% 	===>
%		void success() {
%			throw COMMIT();
%		}
%		try {
%			<Goal && success()>
%			succeeded = MR_FALSE;
%		} catch (COMMIT) {
%			succeeded = MR_TRUE;
%		}

%	The above is using C++ syntax. Here COMMIT is an exception type,
%	which can be defined trivially (e.g. "class COMMIT {};").
%	Note that when using catch/throw, we don't need the "ref" argument
%	at all; the target language's exception handling implementation
%	keeps track of all the information needed to unwind the stack.

%	model_non in semi context: (using setjmp/longjmp)
%		<succeeded = Goal>
% 	===>
%		jmp_buf ref;
%		void success() {
%			longjmp(ref, 1);
%		}
%		if (setjmp(ref)) {
%			succeeded = MR_TRUE;
%		} else {
%			<Goal && success()>
%			succeeded = MR_FALSE;
%		}

%	model_non in semi context: (using GNU C nested functions,
%				GNU C local labels, and exiting
%				the nested function by a goto
%				to a label in the containing function)
%		<succeeded = Goal>
% 	===>
%		__label__ commit;
%		void success() {
%			goto commit;
%		}
%		<Goal && success()>
%		succeeded = MR_FALSE;
%		goto commit_done;
%	commit:
%		succeeded = MR_TRUE;
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
%		<Goal && success()>
%	done:	;

%	model_non in det context (using catch/throw):
%		<do Goal>
%	===>
%		void success() {
%			throw COMMIT();
%		}
%		try {
%			<Goal && success()>
%		} catch (COMMIT) {}

%	model_non in det context (using setjmp/longjmp):
%		<do Goal>
% 	===>
%		jmp_buf ref;
%		void success() {
%			longjmp(ref, 1);
%		}
%		if (setjmp(ref) == 0) {
%			<Goal && success()>
%		}

% Note that for all of these versions, we must hoist any static declarations
% generated for <Goal> out to the top level; this is needed so that such
% declarations remain in scope for any following goals.

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
%		succceeded = MR_TRUE;

%	model_non goal
%		<true && CONT()>
%	===>
%		CONT();

%-----------------------------------------------------------------------------%
%
% Code for non-empty conjunctions
%

% We need to handle the case where the first goal cannot succeed
% specially:
%
%	at_most_zero Goal:
%		<Goal, Goals>
%	===>
%		<Goal>
%
% The remaining cases for conjunction all assume that the first
% goal's determinism is not `erroneous' or `failure'.

% If the first goal is model_det, it is straight-forward:
%
%	model_det Goal:
%		<Goal, Goals>
% 	===>
%		<do Goal>
%		<Goals>

% If the first goal is model_semidet, then there are two cases:
% if the conj as a whole is semidet, things are simple, and
% if the conj as a whole is model_non, then we do the same as
% for the semidet case, except that we also (ought to) declare
% a local `succeeded' variable.
%
%	model_semi Goal in model_semi conj:
%		<succeeded = (Goal, Goals)>
% 	===>
%		<succeeded = Goal>;
%		if (succeeded) {
%			<Goals>;
%		}
%
%	model_semi Goal in model_non conj:
%		<Goal && Goals>
% 	===>
%		MR_bool succeeded;
%
%		<succeeded = Goal>;
%		if (succeeded) {
%			<Goals>;
%		}
%
% The actual code generation scheme we use is slightly
% different to that: we hoist any declarations generated
% for <Goals> to the outer scope, rather than keeping
% them inside the `if', so that they remain in
% scope for any later goals which follow this.
% This is needed for declarations of static consts.

% For model_non goals, there are a couple of different
% ways that we could generate code, depending on whether
% we are aiming to maximize readability, or whether we
% prefer to generate code that may be more efficient
% but is a little less readable.  The more readable method
% puts the generated goals in the same order that
% they occur in the source code:
%
%	model_non Goal (optimized for readability)
%		<Goal, Goals>
% 	===>
%		entry_func() {
%			<Goal && succ_func()>;
%		}
%		succ_func() {
%			<Goals && SUCCEED()>;
%		}
%
%		entry_func();
%
% The more efficient method generates the goals in
% reverse order, so it's less readable, but it has fewer
% function calls and can make it easier for the C compiler
% to inline things:
%
%	model_non Goal (optimized for efficiency):
%		<Goal, Goals>
% 	===>
%		succ_func() {
%			<Goals && SUCCEED()>;
%		}
%
%		<Goal && succ_func()>;
%
% The more efficient method is the one we actually use.
%
% Here's how those two methods look on longer
% conjunctions of nondet goals:
%
%	model_non goals (optimized for readability):
%		<Goal1, Goal2, Goal3, Goals>
% 	===>
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
%
%	model_non goals (optimized for efficiency):
%		<Goal1, Goal2, Goal3, Goals>
% 	===>
%		label1_func() {
%			label2_func() {
%				label3_func() {
%					<Goals && SUCCEED()>;
%				}
%				<Goal3 && label3_func()>;
%			}
%			<Goal2 && label2_func()>;
%		}
%		<Goal1 && label1_func()>;
%
% Note that it might actually make more sense to generate
% conjunctions of nondet goals like this:
%
%	model_non goals (optimized for efficiency, alternative version):
%		<Goal1, Goal2, Goal3, Goals>
% 	===>
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
%
% This would avoid the undesirable deep nesting that we sometimes get
% with our current scheme.  However, if we're eliminating nested
% functions, as is normally the case, then after the ml_elim_nested
% transformation all the functions and variables have been hoisted
% to the top level, so there is no difference between these two.
%
% As with semidet conjunctions, we hoist declarations
% out so that they remain in scope for any following goals.
% This is needed for declarations of static consts.
% However, we want to keep the declarations of non-static
% variables local, since accessing local variables is more
% efficient that accessing variables in the environment
% from a nested function.  So we only hoist declarations
% of static constants.

%-----------------------------------------------------------------------------%
%
% Code for empty disjunctions (`fail')
%

%	model_semi goal:
%		<succeeded = fail>
%	===>
%		succeeded = MR_FALSE;

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
%		MR_bool succeeded;
%	
%		<succeeded = Goal>;
%		if (!succeeded) {
%			<do Goals>;
%		}

% model_semi disj:

%	model_det Goal:
%		<succeeded = (Goal ; Goals)>
%	===>
%		MR_bool succeeded;
%
%		<do Goal>
%		succeeded = MR_TRUE
%		/* <Goals> will never be reached */

%	model_semi Goal:
%		<succeeded = (Goal ; Goals)>
%	===>
%		MR_bool succeeded;
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
%		MR_bool succeeded;
%	
%		<succeeded = Goal>
%		if (succeeded) SUCCEED();
%		<Goals && SUCCEED()>
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

%	model_det Cond:
%		<(Cond -> Then ; Else)>
%	===>
%		<Cond>
%		<Then>

%	model_semi Cond:
%		<(Cond -> Then ; Else)>
%	===>
%		MR_bool succeeded;
%	
%		<succeeded = Cond>
%		if (succeeded) {
%			<Then>
%		} else {
%			<Else>
%		}

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
%		MR_bool cond_<N>;
%
%		void then_func() {
%			cond_<N> = MR_TRUE;
%			<Then>
%		}
%
%		cond_<N> = MR_FALSE;
%		<Cond && then_func()>
%		if (!cond_<N>) {
%			<Else>
%		}
%	except that we hoist any declarations generated
%	for <Cond> to the top of the scope, so that they
%	are in scope for the <Then> goal
%	(this is needed for declarations of static consts)

%-----------------------------------------------------------------------------%
%
% Code for negation
%

% model_det negation
%		<not(Goal)>
%	===>
%		MR_bool succeeded;
%		<succeeded = Goal>
%		/* now ignore the value of succeeded,
%		   which we know will be MR_FALSE */

% model_semi negation, model_det Goal:
%		<succeeded = not(Goal)>
%	===>
%		<do Goal>
%		succeeded = MR_FALSE;

% model_semi negation, model_semi Goal:
%		<succeeded = not(Goal)>
%	===>
%		<succeeded = Goal>
%		succeeded = !succeeded;

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


% This back-end is still not yet 100% complete.
%
% Done:
%	- function prototypes
%	- code generation for det, semidet, and nondet predicates/functions:
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
%		- `pragma c_code'
%	- RTTI
%	- high level data representation
%	  (i.e. generate MLDS type declarations for user-defined types)
%	- support trailing
%
% BUGS:
%	- XXX parameter passing problem for abstract equivalence types
%         that are defined as float (or anything which doesn't map to `Word')
%
% TODO:
%	- XXX define compare & unify preds for RTTI types
%	- XXX need to generate correct layout information for closures
%	      so that tests/hard_coded/copy_pred works.
%	- XXX fix ANSI/ISO C conformance of the generated code (i.e. port to lcc)
%
% UNIMPLEMENTED FEATURES:
%	- test --det-copy-out
%	- fix --gcc-nested-functions (need forward declarations for
%	  nested functions)
%	- support debugging (with mdb)
%	- support genuine parallel conjunction
%	- support fact tables
%	- support --split-c-files
%	- support aditi
%	- support accurate GC
%
% POTENTIAL EFFICIENCY IMPROVEMENTS:
%	- optimize unboxed float on DEC Alphas.
%	- generate better code for switches:
%		- optimize switches so that the recursive case comes first
%		  (see switch_gen.m).
%		- apply the reverse tag test optimization
%		  for types with two functors (see unify_gen.m)
%		- binary search switches
%		- lookup switches
%	- generate local declarations for the `succeeded' variable;
%	  this would help in nondet code, because it would avoid
%	  the need to access the outermost function's `succeeded'
%	  variable via the environment pointer
%	  (be careful about the interaction with setjmp(), though)

%-----------------------------------------------------------------------------%

:- module ml_backend__ml_code_gen.

:- interface.

:- import_module parse_tree__prog_data.
:- import_module hlds__hlds_module, hlds__hlds_goal.
:- import_module backend_libs__code_model.
:- import_module ml_backend__mlds, ml_backend__ml_code_util.
:- import_module io, map.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Generate MLDS code for an entire module.
	%
:- pred ml_code_gen(module_info, mlds, io__state, io__state).
:- mode ml_code_gen(in, out, di, uo) is det.

	% Generate MLDS code for the specified goal in the
	% specified code model.  Return the result as a single statement
	% (which may be a block statement containing nested declarations).
	%
:- pred ml_gen_goal(code_model, hlds_goal, mlds__statement,
			ml_gen_info, ml_gen_info).
:- mode ml_gen_goal(in, in, out, in, out) is det.

	% Generate MLDS code for the specified goal in the
	% specified code model.  Return the result as two lists,
	% one containing the necessary declarations and the other
	% containing the generated statements.
	%
:- pred ml_gen_goal(code_model, hlds_goal, mlds__defns, mlds__statements,
			ml_gen_info, ml_gen_info).
:- mode ml_gen_goal(in, in, out, out, in, out) is det.

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

	% Generate declarations for a list of local variables.
	%
:- pred ml_gen_local_var_decls(prog_varset::in, map(prog_var, prog_type)::in,
		prog_context::in, prog_vars::in, mlds__defns::out,
		ml_gen_info::in, ml_gen_info::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module ml_backend__ml_type_gen, ml_backend__ml_call_gen.
:- import_module ml_backend__ml_unify_gen, ml_backend__ml_switch_gen.
:- import_module ml_backend__ml_code_util.
:- import_module ll_backend__llds. % XXX needed for pragma backend_libs__foreign code
:- import_module backend_libs__export.
:- import_module backend_libs__foreign. % XXX needed for pragma foreign code
:- import_module hlds__hlds_pred, hlds__hlds_data.
:- import_module hlds__goal_util, check_hlds__type_util.
:- import_module check_hlds__mode_util, backend_libs__builtin_ops.
:- import_module hlds__error_util.
:- import_module backend_libs__c_util.
:- import_module hlds__passes_aux, parse_tree__modules.
:- import_module libs__globals, libs__options.

:- import_module assoc_list, bool, string, list.
:- import_module int, set, term, require, std_util.

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

:- pred ml_gen_foreign_code(module_info, map(foreign_language,
		mlds__foreign_code), io__state, io__state).
:- mode ml_gen_foreign_code(in, out, di, uo) is det.

ml_gen_foreign_code(ModuleInfo, All_MLDS_ForeignCode) -->
	{ module_info_get_foreign_decl(ModuleInfo, ForeignDecls) },
	{ module_info_get_foreign_import_module(ModuleInfo, ForeignImports) },
	{ module_info_get_foreign_body_code(ModuleInfo, ForeignBodys) },
	globals__io_get_backend_foreign_languages(BackendForeignLanguages),
	
	{ list__foldl((pred(Lang::in, Map0::in, Map::out) is det :-
			foreign__filter_decls(Lang,
				ForeignDecls, WantedForeignDecls, 
				_OtherForeignDecls),
			foreign__filter_imports(Lang,
				ForeignImports, WantedForeignImports, 
				_OtherForeignImports),
			foreign__filter_bodys(Lang,
				ForeignBodys, WantedForeignBodys,
				_OtherForeignBodys),
			ConvBody = (func(foreign_body_code(L, S, C)) = 
				user_foreign_code(L, S, C)),
			MLDSWantedForeignBodys = list__map(ConvBody, 
				WantedForeignBodys),
			 	% XXX exports are only implemented for
				% C and IL at the moment
			( ( Lang = c ; Lang = il ) ->
				ml_gen_pragma_export(ModuleInfo,
					MLDS_PragmaExports)
			;
				MLDS_PragmaExports = []
			),
			MLDS_ForeignCode = mlds__foreign_code(
				WantedForeignDecls, WantedForeignImports,
				MLDSWantedForeignBodys, MLDS_PragmaExports),
			map__det_insert(Map0, Lang, 
				MLDS_ForeignCode, Map)
		), BackendForeignLanguages, map__init, All_MLDS_ForeignCode) }.


:- pred ml_gen_imports(module_info, mlds__imports).
:- mode ml_gen_imports(in, out) is det.

ml_gen_imports(ModuleInfo, MLDS_ImportList) :-
		% Determine all the mercury imports.
	module_info_globals(ModuleInfo, Globals),
	globals__get_target(Globals, Target),
	module_info_get_all_deps(ModuleInfo, AllImports),
	P = (func(Name) = mercury_import(mercury_module_name_to_mlds(Name))),

		% For every foreign type determine the import needed to
		% find the declaration for that type.
	module_info_types(ModuleInfo, Types),
	ForeignTypeImports = list__condense(list__map(
				foreign_type_required_imports(Target),
				map__values(Types))),

	MLDS_ImportList = ForeignTypeImports ++ 
			list__map(P, set__to_sorted_list(AllImports)).

:- func foreign_type_required_imports(compilation_target, hlds_type_defn)
		= list(mlds__import).

foreign_type_required_imports(c, _) = [].
foreign_type_required_imports(il, TypeDefn) = Imports :-
	hlds_data__get_type_defn_body(TypeDefn, Body),
	( Body = foreign_type(MaybeIL, _MaybeC) ->
		( MaybeIL = yes(il(_, Location, _)) ->
			Name = il_assembly_name(mercury_module_name_to_mlds(
					unqualified(Location))),
			Imports = [foreign_import(Name)]
			
		;
			unexpected(this_file, "no IL type")
		)
	;
		Imports = []
	).
foreign_type_required_imports(java, _) = [].
foreign_type_required_imports(asm, _) = [].

:- pred ml_gen_defns(module_info, mlds__defns, io__state, io__state).
:- mode ml_gen_defns(in, out, di, uo) is det.

ml_gen_defns(ModuleInfo, MLDS_Defns) -->
	ml_gen_types(ModuleInfo, MLDS_TypeDefns),
	ml_gen_preds(ModuleInfo, MLDS_PredDefns),
	{ MLDS_Defns = list__append(MLDS_TypeDefns, MLDS_PredDefns) }.

%-----------------------------------------------------------------------------%
%
% For each pragma export declaration we associate with it the 
% information used to generate the function prototype for the MLDS
% entity.
%

:- pred ml_gen_pragma_export(module_info, list(mlds__pragma_export)).
:- mode ml_gen_pragma_export(in, out) is det.

ml_gen_pragma_export(ModuleInfo, MLDS_PragmaExports) :-
	module_info_get_pragma_exported_procs(ModuleInfo, PragmaExports),
	list__map(ml_gen_pragma_export_proc(ModuleInfo),
			PragmaExports, MLDS_PragmaExports).

:- pred ml_gen_pragma_export_proc(module_info::in,
		pragma_exported_proc::in, mlds__pragma_export::out) is det.

ml_gen_pragma_export_proc(ModuleInfo,
		pragma_exported_proc(PredId, ProcId, C_Name, ProgContext),
		ML_Defn) :-

	ml_gen_proc_label(ModuleInfo, PredId, ProcId,
		MLDS_Name, MLDS_ModuleName),
	MLDS_FuncParams = ml_gen_proc_params(ModuleInfo, PredId, ProcId),
	MLDS_Context = mlds__make_context(ProgContext),
	ML_Defn = ml_pragma_export(C_Name, qual(MLDS_ModuleName, MLDS_Name),
			MLDS_FuncParams, MLDS_Context).


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
		{ pred_info_import_status(PredInfo, ImportStatus) },
		( { ImportStatus = imported(_) } ->
			{ MLDS_Defns1 = MLDS_Defns0 }
		;
			ml_gen_pred(ModuleInfo, PredId, PredInfo, ImportStatus,
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
:- pred ml_gen_pred(module_info, pred_id, pred_info, import_status,
			mlds__defns, mlds__defns, io__state, io__state).
:- mode ml_gen_pred(in, in, in, in, in, out, di, uo) is det.

ml_gen_pred(ModuleInfo, PredId, PredInfo, ImportStatus,
		MLDS_Defns0, MLDS_Defns) -->
	( { ImportStatus = external(_) } ->
		{ pred_info_procids(PredInfo, ProcIds) }
	;
		{ pred_info_non_imported_procids(PredInfo, ProcIds) }
	),
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

	ml_gen_proc_label(ModuleInfo, PredId, ProcId, MLDS_Name, _ModuleName),
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
		eval_method_has_per_proc_tabling_pointer(EvalMethod) = yes
	->
		ml_gen_pred_label(ModuleInfo, PredId, ProcId,
			MLDS_PredLabel, _MLDS_PredModule),
		Var = tabling_pointer(MLDS_PredLabel - ProcId),
		Type = mlds__generic_type,
		Initializer = init_obj(const(null(Type))),
		proc_info_context(ProcInfo, Context),
		(
			module_info_globals(ModuleInfo, Globals),
			globals__get_gc_method(Globals, GC_Method),
			GC_Method = accurate
		->
			% XXX To handle this case properly, the GC would
			% need to trace through the global variable
			% that we generate for the table pointer.
			% Support for this is not yet implemented.
			sorry(this_file, "tabling and `--gc accurate'")
		;
			GC_TraceCode = no
		),
		TablePointerVarDefn = ml_gen_mlds_var_decl(
			Var, Type, Initializer, GC_TraceCode,
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
	PerInstance = one_copy,
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
			PredInfo, ProcInfo),
	pred_info_import_status(PredInfo, ImportStatus),
	pred_info_arg_types(PredInfo, ArgTypes),
	proc_info_interface_code_model(ProcInfo, CodeModel),
	proc_info_headvars(ProcInfo, HeadVars),
	proc_info_goal(ProcInfo, Goal0),

	%
	% The HLDS front-end sometimes over-estimates
	% the set of non-locals.  We need to restrict
	% the set of non-locals for the top-level goal
	% to just the headvars, because otherwise variables
	% which occur in the top-level non-locals but which
	% are not really non-local will not be declared.
	%
	Goal0 = GoalExpr - GoalInfo0,
	goal_info_get_code_gen_nonlocals(GoalInfo0, NonLocals0),
	set__list_to_set(HeadVars, HeadVarsSet),
	set__intersect(HeadVarsSet, NonLocals0, NonLocals),
	goal_info_set_code_gen_nonlocals(GoalInfo0, NonLocals, GoalInfo),
	Goal = GoalExpr - GoalInfo,

	goal_info_get_context(GoalInfo, Context),

	MLDSGenInfo0 = ml_gen_info_init(ModuleInfo, PredId, ProcId),

	( ImportStatus = external(_) ->
		%
		% For Mercury procedures declared `:- external', we generate 
		% an MLDS definition for them with no function body.
		% The MLDS -> target code pass can treat this accordingly,
		% e.g. for C it outputs a function declaration with no
		% corresponding definition, making sure that the function
		% is declared as `extern' rather than `static'.
		%
		FunctionBody = external,
		ExtraDefns = [],
		ml_gen_proc_params(PredId, ProcId, MLDS_Params,
			MLDSGenInfo0, _MLDSGenInfo)
	;
		% Set up the initial success continuation, if any.
		% Also figure out which output variables are returned by
		% value (rather than being passed by reference) and remove
		% them from the byref_output_vars field in the ml_gen_info.
		( CodeModel = model_non ->
		
 			ml_set_up_initial_succ_cont(ModuleInfo, 
				CopiedOutputVars, MLDSGenInfo0, MLDSGenInfo1)
 		;
			ml_det_copy_out_vars(ModuleInfo,
			 	CopiedOutputVars, MLDSGenInfo0, MLDSGenInfo1)
		),

		% This would generate all the local variables at the top of
		% the function:
		%	ml_gen_all_local_var_decls(Goal,
		% 		VarSet, VarTypes, HeadVars, MLDS_LocalVars,
		%		MLDSGenInfo1, MLDSGenInfo2)
		% But instead we now generate them locally for each goal.
		% We just declare the `succeeded' var here, plus locals
		% for any output arguments that are returned by value
		% (e.g. if --nondet-copy-out is enabled, or for det function
		% return values).
		( CopiedOutputVars = [] ->
			% optimize common case
			OutputVarLocals = [],
			MLDSGenInfo2 = MLDSGenInfo1
		;
			proc_info_varset(ProcInfo, VarSet),
			proc_info_vartypes(ProcInfo, VarTypes),
			% note that for headvars we must use the types from
			% the procedure interface, not from the procedure body
			HeadVarTypes = map__from_corresponding_lists(HeadVars,
				ArgTypes),
			ml_gen_local_var_decls(VarSet,
				map__overlay(VarTypes, HeadVarTypes),
				Context, CopiedOutputVars, OutputVarLocals,
				MLDSGenInfo1, MLDSGenInfo2)
		),
		MLDS_Context = mlds__make_context(Context),
		MLDS_LocalVars = [ml_gen_succeeded_var_decl(MLDS_Context) |
				OutputVarLocals],
		ml_gen_proc_body(CodeModel, HeadVars, ArgTypes,
				CopiedOutputVars, Goal,
				MLDS_Decls0, MLDS_Statements,
				MLDSGenInfo2, MLDSGenInfo3),
		ml_gen_proc_params(PredId, ProcId, MLDS_Params,
			MLDSGenInfo3, MLDSGenInfo),
		ml_gen_info_get_extra_defns(MLDSGenInfo, ExtraDefns),
		MLDS_Decls = list__append(MLDS_LocalVars, MLDS_Decls0),
		MLDS_Statement = ml_gen_block(MLDS_Decls, MLDS_Statements,
			Context),
		FunctionBody = defined_here(MLDS_Statement)
	),

	pred_info_get_attributes(PredInfo, Attributes),
	attributes_to_attribute_list(Attributes, AttributeList),

	MLDSAttributes = attributes_to_mlds_attributes(ModuleInfo,
		AttributeList),
	
	MLDS_ProcDefnBody = mlds__function(yes(proc(PredId, ProcId)),
			MLDS_Params, FunctionBody, MLDSAttributes).

	% for model_det and model_semi procedures,
	% figure out which output variables are returned by
	% value (rather than being passed by reference) and remove
	% them from the byref_output_vars field in the ml_gen_info.
:- pred ml_det_copy_out_vars(module_info, list(prog_var),
		ml_gen_info, ml_gen_info).
:- mode ml_det_copy_out_vars(in, out, in, out) is det.

ml_det_copy_out_vars(ModuleInfo, CopiedOutputVars,
		MLDSGenInfo0, MLDSGenInfo) :-
	ml_gen_info_get_byref_output_vars(MLDSGenInfo0, OutputVars),
	module_info_globals(ModuleInfo, Globals),
	globals__lookup_bool_option(Globals, det_copy_out, DetCopyOut),
	(
		% if --det-copy-out is enabled, all output variables
		% are returned by value, rather than passing
		% them by reference.
		DetCopyOut = yes
	->
		ByRefOutputVars = [],
		CopiedOutputVars = OutputVars
	;
		% for det functions, the function result variable
		% is returned by value, and any remaining output
		% variables are passed by reference
		ml_gen_info_get_pred_id(MLDSGenInfo0, PredId),
		ml_gen_info_get_proc_id(MLDSGenInfo0, ProcId),
		ml_is_output_det_function(ModuleInfo, PredId,
			ProcId, ResultVar)
	->
		CopiedOutputVars = [ResultVar],
		list__delete_all(OutputVars, ResultVar, ByRefOutputVars)
	;
		% otherwise, all output vars are passed by reference
		CopiedOutputVars = [],
		ByRefOutputVars = OutputVars
	),
	ml_gen_info_set_byref_output_vars(ByRefOutputVars,
		MLDSGenInfo0, MLDSGenInfo1),
	ml_gen_info_set_value_output_vars(CopiedOutputVars,
		MLDSGenInfo1, MLDSGenInfo).

	% for model_non procedures,
	% figure out which output variables are returned by
	% value (rather than being passed by reference) and remove
	% them from the byref_output_vars field in the ml_gen_info,
	% and construct the initial success continuation.
:- pred ml_set_up_initial_succ_cont(module_info, list(prog_var),
		ml_gen_info, ml_gen_info).
:- mode ml_set_up_initial_succ_cont(in, out, in, out) is det.

ml_set_up_initial_succ_cont(ModuleInfo, NondetCopiedOutputVars) -->
	{ module_info_globals(ModuleInfo, Globals) },
	{ globals__lookup_bool_option(Globals, nondet_copy_out,
		NondetCopyOut) },
	( { NondetCopyOut = yes } ->
		% for --nondet-copy-out, we generate local variables
		% for the output variables and then pass them to the
		% continuation, rather than passing them by reference.
		=(MLDSGenInfo0),
		{ ml_gen_info_get_byref_output_vars(MLDSGenInfo0,
			NondetCopiedOutputVars) },
		ml_gen_info_set_byref_output_vars([])
	;
		{ NondetCopiedOutputVars = [] }
	),
	ml_gen_info_set_value_output_vars(NondetCopiedOutputVars),
	ml_gen_var_list(NondetCopiedOutputVars, OutputVarLvals),
	ml_variable_types(NondetCopiedOutputVars, OutputVarTypes),
	ml_initial_cont(OutputVarLvals, OutputVarTypes, InitialCont),
	ml_gen_info_push_success_cont(InitialCont).

	% Generate MLDS definitions for all the local variables in a function.
	%
	% Note that this function generates all the local variables at the
	% top of the function.  It might be a better idea to instead
	% generate local declarations for all the variables used in
	% each sub-goal.
	%
:- pred ml_gen_all_local_var_decls(hlds_goal::in, prog_varset::in,
		map(prog_var, prog_type)::in, list(prog_var)::in,
		mlds__defns::out, ml_gen_info::in, ml_gen_info::out) is det.
ml_gen_all_local_var_decls(Goal, VarSet, VarTypes, HeadVars, MLDS_LocalVars) -->
	{ Goal = _ - GoalInfo },
	{ goal_info_get_context(GoalInfo, Context) },
	{ goal_util__goal_vars(Goal, AllVarsSet) },
	{ set__delete_list(AllVarsSet, HeadVars, LocalVarsSet) },
	{ set__to_sorted_list(LocalVarsSet, LocalVars) },
	ml_gen_local_var_decls(VarSet, VarTypes, Context, LocalVars,
				MLDS_LocalVars0),
	{ MLDS_Context = mlds__make_context(Context) },
	{ MLDS_SucceededVar = ml_gen_succeeded_var_decl(MLDS_Context) },
	{ MLDS_LocalVars = [MLDS_SucceededVar | MLDS_LocalVars0] }.

	% Generate declarations for a list of local variables.
	%
ml_gen_local_var_decls(_VarSet, _VarTypes, _Context, [], []) --> [].
ml_gen_local_var_decls(VarSet, VarTypes, Context, [Var|Vars], MLDS_Defns) -->
	{ map__lookup(VarTypes, Var, Type) },
	( { type_util__is_dummy_argument_type(Type) } ->
		% no declaration needed for this variable
		ml_gen_local_var_decls(VarSet, VarTypes, Context, Vars,
			MLDS_Defns)
	;
		{ VarName = ml_gen_var_name(VarSet, Var) },
		ml_gen_var_decl(VarName, Type, Context, MLDS_Defn),
		ml_gen_local_var_decls(VarSet, VarTypes, Context, Vars,
			MLDS_Defns0),
		{ MLDS_Defns = [MLDS_Defn | MLDS_Defns0] }
	).

	% Generate the code for a procedure body.
	%
:- pred ml_gen_proc_body(code_model, list(prog_var), list(prog_type),
		list(prog_var), hlds_goal, mlds__defns, mlds__statements,
		ml_gen_info, ml_gen_info).
:- mode ml_gen_proc_body(in, in, in, in, in, out, out, in, out) is det.

ml_gen_proc_body(CodeModel, HeadVars, ArgTypes, CopiedOutputVars, Goal,
		MLDS_Decls, MLDS_Statements) -->
	{ Goal = _ - GoalInfo },
	{ goal_info_get_context(GoalInfo, Context) },

	%
	% First just generate the code for the procedure's goal.
	%
	{ DoGenGoal = ml_gen_goal(CodeModel, Goal) },

	%
	% In certain cases -- for example existentially typed procedures,
	% or unification/compare procedures for equivalence types --
	% the parameters types may not match the types of the head variables.
	% In such cases, we need to box/unbox/cast them to the right type.
	% We also grab the original (uncast) lvals for the copied output
	% variables (if any) here, since for the return statement that
	% we append below, we want the original vars, not their cast versions.
	%
	ml_gen_var_list(CopiedOutputVars, CopiedOutputVarOriginalLvals),
	ml_gen_convert_headvars(HeadVars, ArgTypes, CopiedOutputVars, Context,
		ConvDecls, ConvInputStatements, ConvOutputStatements),
	(
		{ ConvDecls = [] },
		{ ConvInputStatements = [] },
		{ ConvOutputStatements = [] }
	->
		% No boxing/unboxing/casting required.
		DoGenGoal(MLDS_Decls, MLDS_Statements1)
	;
		% Boxing/unboxing/casting required.
		% We need to convert the input arguments,
		% generate the goal, convert the output arguments,
		% and then succeeed.
		{ DoConvOutputs = (pred(Decls::out, Statements::out, in, out)
					is det -->
			ml_gen_success(CodeModel, Context, SuccStatements),
			{ Decls = [] },
			{ Statements = list__append(ConvOutputStatements,
				SuccStatements) }
		) },
		ml_combine_conj(CodeModel, Context,
			DoGenGoal, DoConvOutputs,
			MLDS_Decls0, MLDS_Statements0),
		{ MLDS_Statements1 = list__append(ConvInputStatements,
			MLDS_Statements0) },
		{ MLDS_Decls = list__append(ConvDecls, MLDS_Decls0) }
	),

	%
	% Finally append an appropriate `return' statement, if needed.
	%
	ml_append_return_statement(CodeModel, CopiedOutputVarOriginalLvals,
		Context, MLDS_Statements1, MLDS_Statements).

%
% In certain cases -- for example existentially typed procedures,
% or unification/compare procedures for equivalence types --
% the parameter types may not match the types of the head variables.
% In such cases, we need to box/unbox/cast them to the right type.
% This procedure handles that.
%
:- pred ml_gen_convert_headvars(list(prog_var), list(prog_type),
		list(prog_var), prog_context,
		mlds__defns, mlds__statements, mlds__statements,
		ml_gen_info, ml_gen_info).
:- mode ml_gen_convert_headvars(in, in, in, in, out, out, out, in, out) is det.

ml_gen_convert_headvars([], [], _, _, [], [], []) --> [].
ml_gen_convert_headvars([Var|Vars], [HeadType|HeadTypes], CopiedOutputVars,
		Context, Decls, InputStatements, OutputStatements) -->
	ml_variable_type(Var, BodyType),
	(
		%
		% Check whether HeadType is the same as BodyType
		% (modulo the term__contexts)
		%
		{ map__init(Subst0) },
		{ type_unify(HeadType, BodyType, [], Subst0, Subst) },
		{ map__is_empty(Subst) }
	->
		% just recursively process the remaining arguments
		ml_gen_convert_headvars(Vars, HeadTypes, CopiedOutputVars,
			Context, Decls, InputStatements, OutputStatements)
	;
		%
		% generate the lval for the head variable
		%
		ml_gen_var_with_type(Var, HeadType, HeadVarLval),

		%
		% generate code to box or unbox that head variable,
		% to convert its type from HeadType to BodyType
		%
		=(MLDSGenInfo),
		{ ml_gen_info_get_varset(MLDSGenInfo, VarSet) },
		{ VarName = ml_gen_var_name(VarSet, Var) },
		ml_gen_box_or_unbox_lval(HeadType, BodyType, HeadVarLval,
			VarName, Context, BodyLval, ConvDecls,
			ConvInputStatements, ConvOutputStatements),

		%
		% Ensure that for any uses of this variable in the procedure
		% body, we use the BodyLval (which has type BodyType)
		% rather than the HeadVarLval (which has type HeadType).
		%
		ml_gen_info_set_var_lval(Var, BodyLval),

		%
		% Recursively process the remaining arguments
		%
		ml_gen_convert_headvars(Vars, HeadTypes, CopiedOutputVars,
			Context, Decls1, InputStatements1, OutputStatements1),

		%
		% Add the code to convert this input or output.
		%
		=(MLDSGenInfo2),
		{ ml_gen_info_get_byref_output_vars(MLDSGenInfo2,
			ByRefOutputVars) },
		{
			( list__member(Var, ByRefOutputVars)
			; list__member(Var, CopiedOutputVars)
			)
		->
			InputStatements = InputStatements1,
			OutputStatements = list__append(OutputStatements1,
				ConvOutputStatements)
		;
			InputStatements = list__append(ConvInputStatements,
				InputStatements1),
			OutputStatements = OutputStatements1
		},
		{ list__append(ConvDecls, Decls1, Decls) }
	).
ml_gen_convert_headvars([], [_|_], _, _, _, _, _) -->
	{ error("ml_gen_convert_headvars: length mismatch") }.
ml_gen_convert_headvars([_|_], [], _, _, _, _, _) -->
	{ error("ml_gen_convert_headvars: length mismatch") }.

%-----------------------------------------------------------------------------%
%
% Stuff to generate code for goals.
%

	% Generate MLDS code for the specified goal in the
	% specified code model.  Return the result as a single statement
	% (which may be a block statement containing nested declarations).
	%
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
ml_gen_goal(CodeModel, Goal, MLDS_Decls, MLDS_Statements) -->
	{ Goal = GoalExpr - GoalInfo },
	{ goal_info_get_context(GoalInfo, Context) },
	%
	% Generate the local variables for this goal.
	% We need to declare any variables which
	% are local to this goal (including its subgoals),
	% but which are not local to a subgoal.
	% (If they're local to a subgoal, they'll be declared
	% when we generate code for that subgoal.)
	%
	% We need to make sure that we declare any type_info or
	% type_classinfo variables *before* any other variables,
	% since the GC tracing code for the other variables may
	% refer to the type_info variables, so they need to be in scope.

	{ Locals = goal_local_vars(Goal) },
	{ SubGoalLocals = union_of_direct_subgoal_locals(Goal) },
	{ set__difference(Locals, SubGoalLocals, VarsToDeclareHere) },
	{ set__to_sorted_list(VarsToDeclareHere, VarsList0) },
	=(MLDSGenInfo),
	{ ml_gen_info_get_varset(MLDSGenInfo, VarSet) },
	{ ml_gen_info_get_var_types(MLDSGenInfo, VarTypes) },
	{ VarsList = put_typeinfo_vars_first(VarsList0, VarTypes) },
	ml_gen_local_var_decls(VarSet, VarTypes, Context, VarsList, VarDecls),

	%
	% Generate code for the goal in its own code model.
	%
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
	goal_info_get_code_gen_nonlocals(GoalInfo, NonLocalVars),
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
	%	<do Goal>
	%	succeeded = MR_TRUE
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
	%	MR_bool succeeded;
	%
	%	<succeeded = Goal>
	%	if (succeeded) SUCCEED()
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
	{ goal_info_get_context(GoalInfo, GoalContext) },

	( { GoalCodeModel = model_non, CodeModel = model_semi } ->

		%	model_non in semi context: (using try_commit/do_commit)
		%		<succeeded = Goal>
		% 	===>
		%		MR_bool succeeded;
		%	#ifdef NONDET_COPY_OUT
		%		<local var decls>
		%	#endif
		%	#ifdef PUT_COMMIT_IN_OWN_FUNC
		%	    /*
		%	    ** to avoid problems with setjmp() and non-volatile
		%	    ** local variables, we need to put the call to
		%	    ** setjmp() in its own nested function
		%	    */
		%	    void commit_func()
		%	    {
		%       #endif
		%		MR_COMMIT_TYPE ref;
		%
		%		void success() {
		%			MR_DO_COMMIT(ref);
		%		}
		%
		%		MR_TRY_COMMIT(ref, {
		%			<Goal && success()>
		%			succeeded = MR_FALSE;
		%		}, {
		%	#ifdef NONDET_COPY_OUT
		%			<copy local vars to output args>
		%	#endif
		%			succeeded = MR_TRUE;
		%		})
		%	#ifdef PUT_COMMIT_IN_OWN_FUNC
		%	    }
		%	    commit_func();
		%       #endif

		ml_gen_maybe_make_locals_for_output_args(GoalInfo,
			LocalVarDecls, CopyLocalsToOutputArgs,
			OrigVarLvalMap),

		% generate the `success()' function
		ml_gen_new_func_label(no, SuccessFuncLabel, 
			SuccessFuncLabelRval),
		/* push nesting level */
		{ MLDS_Context = mlds__make_context(Context) },
		ml_gen_info_new_commit_label(CommitLabelNum),
		{ CommitRef = mlds__var_name(string__format("commit_%d",
			[i(CommitLabelNum)]), no) },
		ml_gen_var_lval(CommitRef, mlds__commit_type,
			CommitRefLval),
		{ CommitRefDecl = ml_gen_commit_var_decl(MLDS_Context,
			CommitRef) },
		{ DoCommitStmt = do_commit(lval(CommitRefLval)) },
		{ DoCommitStatement = 
			mlds__statement(DoCommitStmt, MLDS_Context) },
		/* pop nesting level */
		ml_gen_nondet_label_func(SuccessFuncLabel, Context,
			DoCommitStatement, SuccessFunc),

		ml_get_env_ptr(EnvPtrRval),
		{ SuccessCont = success_cont(SuccessFuncLabelRval,
			EnvPtrRval, [], []) },
		ml_gen_info_push_success_cont(SuccessCont),
		ml_gen_goal(model_non, Goal, GoalDecls, GoalStatements),
		% hoist any static constant declarations for Goal
		% out to the top level
		{ list__filter(ml_decl_is_static_const, GoalDecls,
			GoalStaticDecls, GoalOtherDecls) },
		{ GoalStatement = ml_gen_block(GoalOtherDecls,
			GoalStatements, GoalContext) },
		ml_gen_info_pop_success_cont,
		ml_gen_set_success(const(false), Context, SetSuccessFalse),
		ml_gen_set_success(const(true), Context, SetSuccessTrue),
		{ TryCommitStmt = try_commit(CommitRefLval,
			ml_gen_block([], [GoalStatement, SetSuccessFalse],
				Context),
			ml_gen_block([], list__append(CopyLocalsToOutputArgs,
				[SetSuccessTrue]), Context)) },
		{ TryCommitStatement = mlds__statement(TryCommitStmt,
			MLDS_Context) },
		{ CommitFuncLocalDecls = [CommitRefDecl, SuccessFunc |
			GoalStaticDecls] },
		maybe_put_commit_in_own_func(CommitFuncLocalDecls,
			[TryCommitStatement], Context,
			CommitFuncDecls, MLDS_Statements),
		{ MLDS_Decls = LocalVarDecls ++ CommitFuncDecls },

		ml_gen_info_set_var_lvals(OrigVarLvalMap)

	; { GoalCodeModel = model_non, CodeModel = model_det } ->

		%	model_non in det context: (using try_commit/do_commit)
		%		<do Goal>
		%	===>
		%	#ifdef NONDET_COPY_OUT
		%		<local var decls>
		%	#endif
		%	#ifdef PUT_COMMIT_IN_NESTED_FUNC
		%	    /*
		%	    ** to avoid problems with setjmp() and non-volatile
		%	    ** local variables, we need to put the call to
		%	    ** setjmp() in its own nested functions
		%	    */
		%	    void commit_func()
		%	    {
		%       #endif
		%		MR_COMMIT_TYPE ref;
		%		void success() {
		%			MR_DO_COMMIT(ref);
		%		}
		%		MR_TRY_COMMIT(ref, {
		%			<Goal && success()>
		%		}, {
		%	#ifdef NONDET_COPY_OUT
		%			<copy local vars to output args>
		%	#endif
		%		})
		%	#ifdef PUT_COMMIT_IN_NESTED_FUNC
		%	    }
		%	    commit_func();
		%       #endif

		ml_gen_maybe_make_locals_for_output_args(GoalInfo,
			LocalVarDecls, CopyLocalsToOutputArgs,
			OrigVarLvalMap),

		% generate the `success()' function
		ml_gen_new_func_label(no,
			SuccessFuncLabel, SuccessFuncLabelRval),
		/* push nesting level */
		{ MLDS_Context = mlds__make_context(Context) },
		ml_gen_info_new_commit_label(CommitLabelNum),
		{ CommitRef = mlds__var_name(
			string__format("commit_%d", [i(CommitLabelNum)]),
			no) },
		ml_gen_var_lval(CommitRef, mlds__commit_type, CommitRefLval),
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
			EnvPtrRval, [], []) },
		ml_gen_info_push_success_cont(SuccessCont),
		ml_gen_goal(model_non, Goal, GoalDecls, GoalStatements),
		% hoist any static constant declarations for Goal
		% out to the top level
		{ list__filter(ml_decl_is_static_const, GoalDecls,
			GoalStaticDecls, GoalOtherDecls) },
		{ GoalStatement = ml_gen_block(GoalOtherDecls,
			GoalStatements, GoalContext) },
		ml_gen_info_pop_success_cont,

		{ TryCommitStmt = try_commit(CommitRefLval, GoalStatement,
			ml_gen_block([], CopyLocalsToOutputArgs, Context)) },
		{ TryCommitStatement = mlds__statement(TryCommitStmt,
			MLDS_Context) },
		{ CommitFuncLocalDecls = [CommitRefDecl, SuccessFunc |
			GoalStaticDecls] },
		maybe_put_commit_in_own_func(CommitFuncLocalDecls,
			[TryCommitStatement], Context,
			CommitFuncDecls, MLDS_Statements),
		{ MLDS_Decls = LocalVarDecls ++ CommitFuncDecls },
		ml_gen_info_set_var_lvals(OrigVarLvalMap)
	;
		% no commit required
		ml_gen_goal(CodeModel, Goal, MLDS_Decls, MLDS_Statements)
	).

	% maybe_put_commit_in_own_func(Defns0, Stmts0, Defns, Stmts):
	% if the --put-commit-in-own-func option is set, put
	% the commit in its own function.  This is needed for
	% the high-level C back-end, to handle problems with
	% setjmp()/longjmp() clobbering non-volatile local variables.
	%
	% Detailed explanation:
	%   For the high-level C back-end, we implement commits using
	%   setjmp()/longjmp().  Unfortunately for us, ANSI/ISO C says
	%   that longjmp() is allowed to clobber the values of any
	%   non-volatile local variables in the function that called
	%   setjmp() which have been modified between the setjmp()
	%   and the longjmp().
	%
	%   To avoid this, whenever we generate a commit, we put
	%   it in its own nested function, with the local variables
	%   (e.g. `succeeded', plus any outputs from the goal that
	%   we're committing over) remaining in the containing function.
	%   This ensures that none of the variables which get modified
	%   between the setjmp() and the longjmp() and which get
	%   referenced after the longjmp() are local variables in the
	%   function containing the setjmp().
	%
	%   [The obvious alternative of declaring the local variables in
	%   the function containing setjmp() as `volatile' doesn't work,
	%   since the assignments to those output variables may be deep
	%   in some function called indirectly from the goal that we're
	%   committing across, and assigning to a volatile-qualified
	%   variable via a non-volatile pointer is undefined behaviour.
	%   The only way to make it work would be to be to declare
	%   *every* output argument that we pass by reference as
	%   `volatile T *'.  But that would impose distributed fat and
	%   would make interoperability difficult.]
	%
:- pred maybe_put_commit_in_own_func(mlds__defns, mlds__statements,
		prog_context, mlds__defns, mlds__statements,
		ml_gen_info, ml_gen_info).
:- mode maybe_put_commit_in_own_func(in, in, in, out, out, in, out) is det.

maybe_put_commit_in_own_func(CommitFuncLocalDecls, TryCommitStatements,
		Context, MLDS_Decls, MLDS_Statements) -->
	ml_gen_info_put_commit_in_own_func(PutCommitInOwnFunc),
	( { PutCommitInOwnFunc = yes } ->
		%
		% Generate the `void commit_func() { ... }' wrapper
		% around the main body that we generated above
		%
		ml_gen_new_func_label(no, CommitFuncLabel, 
			CommitFuncLabelRval),
		/* push nesting level */
		{ CommitFuncBody = ml_gen_block(CommitFuncLocalDecls,
			TryCommitStatements, Context) },
		/* pop nesting level */
		ml_gen_nondet_label_func(CommitFuncLabel, Context,
			CommitFuncBody, CommitFunc),
		%
		% Generate the call to `commit_func();'
		%
		ml_gen_info_use_gcc_nested_functions(UseNestedFuncs),
		( { UseNestedFuncs = yes } ->
			{ ArgRvals = [] },
			{ ArgTypes = [] }
		;
			ml_get_env_ptr(EnvPtrRval),
			{ ArgRvals = [EnvPtrRval] },
			{ ArgTypes = [mlds__generic_env_ptr_type] }
		),
		{ RetTypes = [] },
		{ Signature = mlds__func_signature(ArgTypes, RetTypes) },
		{ CallKind = ordinary_call },
		{ CallStmt = call(Signature, CommitFuncLabelRval, no,
			ArgRvals, [], CallKind) },
		{ CallStatement = mlds__statement(CallStmt,
			mlds__make_context(Context)) },
		% Package it all up
		{ MLDS_Statements = [CallStatement] },
		{ MLDS_Decls = [CommitFunc] }
	;
		{ MLDS_Statements = TryCommitStatements },
		{ MLDS_Decls = CommitFuncLocalDecls }
	).

	%
	% In commits, you have model_non code called from a model_det or
	% model_semi context.  With --nondet-copy-out, when generating code
	% for commits, if the context is a model_det or model_semi procedure
	% with output arguments passed by reference, then we need to introduce
	% local variables corresponding to those output arguments,
	% and at the end of the commit we'll copy the local variables into
	% the output arguments.
	%
:- pred ml_gen_maybe_make_locals_for_output_args(hlds_goal_info, mlds__defns,
		mlds__statements, map(prog_var, mlds__lval),
		ml_gen_info, ml_gen_info).
:- mode ml_gen_maybe_make_locals_for_output_args(in, out, out, out, in, out)
		is det.

ml_gen_maybe_make_locals_for_output_args(GoalInfo,
		LocalVarDecls, CopyLocalsToOutputArgs, OrigVarLvalMap) -->
	=(MLDSGenInfo0),
	{ ml_gen_info_get_var_lvals(MLDSGenInfo0, OrigVarLvalMap) },
	ml_gen_info_get_globals(Globals),
	{ globals__lookup_bool_option(Globals, nondet_copy_out,
		NondetCopyOut) },
	( { NondetCopyOut = yes } ->
		{ goal_info_get_context(GoalInfo, Context) },
		{ goal_info_get_nonlocals(GoalInfo, NonLocals) },
		{ ml_gen_info_get_byref_output_vars(MLDSGenInfo0,
			ByRefOutputVars) },
		{ VarsToCopy = set__intersect(set__list_to_set(ByRefOutputVars),
			NonLocals) },
		ml_gen_make_locals_for_output_args(
			set__to_sorted_list(VarsToCopy), Context,
			LocalVarDecls, CopyLocalsToOutputArgs)
	;
		{ LocalVarDecls = [] },
		{ CopyLocalsToOutputArgs = [] }
	).

:- pred ml_gen_make_locals_for_output_args(list(prog_var), prog_context,
		mlds__defns, mlds__statements, ml_gen_info, ml_gen_info).
:- mode ml_gen_make_locals_for_output_args(in, in, out, out, in, out) is det.

ml_gen_make_locals_for_output_args([], _, [], []) --> [].
ml_gen_make_locals_for_output_args([Var | Vars], Context,
		LocalDefns, Assigns) -->
	ml_gen_make_locals_for_output_args(Vars, Context,
		LocalDefns0, Assigns0),
	ml_variable_type(Var, Type),
	( { type_util__is_dummy_argument_type(Type) } ->
		{ LocalDefns = LocalDefns0 },
		{ Assigns = Assigns0 }
	;
		ml_gen_make_local_for_output_arg(Var, Type, Context,
			LocalDefn, Assign),
		{ LocalDefns = [LocalDefn | LocalDefns0] },
		{ Assigns = [Assign | Assigns0] }
	).

:- pred ml_gen_make_local_for_output_arg(prog_var, prog_type, prog_context,
		mlds__defn, mlds__statement, ml_gen_info, ml_gen_info).
:- mode ml_gen_make_local_for_output_arg(in, in, in, out, out, in, out) is det.

ml_gen_make_local_for_output_arg(OutputVar, Type, Context,
		LocalVarDefn, Assign) -->
	%
	% Look up the name of the output variable
	%
	=(MLDSGenInfo),
	{ ml_gen_info_get_varset(MLDSGenInfo, VarSet) },
	{ OutputVarName = ml_gen_var_name(VarSet, OutputVar) },

	%
	% Generate a declaration for a corresponding local variable.
	{ OutputVarName = mlds__var_name(OutputVarNameStr, MaybeNum) },
	{ LocalVarName = mlds__var_name(
		string__append("local_", OutputVarNameStr), MaybeNum) },
	ml_gen_type(Type, MLDS_Type),
	ml_gen_maybe_gc_trace_code(LocalVarName, Type, Context, GC_TraceCode),
	{ LocalVarDefn = ml_gen_mlds_var_decl(var(LocalVarName), MLDS_Type,
		GC_TraceCode, mlds__make_context(Context)) },
	
	%
	% Generate code to assign from the local var to the output var
	%
	ml_gen_var(OutputVar, OutputVarLval),
	ml_gen_var_lval(LocalVarName, MLDS_Type, LocalVarLval),
	{ Assign = ml_gen_assign(OutputVarLval, lval(LocalVarLval), Context) },

	%
	% Update the lval for this variable so that any references to it
	% inside the commit refer to the local variable rather than
	% to the output argument.
	% (Note that we reset all the var lvals at the end of the commit.)
	%
	ml_gen_info_set_var_lval(OutputVar, LocalVarLval).

	% Generate the declaration for the `commit' variable.
	%
:- func ml_gen_commit_var_decl(mlds__context, mlds__var_name) = mlds__defn.
ml_gen_commit_var_decl(Context, VarName) =
	ml_gen_mlds_var_decl(var(VarName), mlds__commit_type, no, Context).

	% Generate MLDS code for the different kinds of HLDS goals.
	%
:- pred ml_gen_goal_expr(hlds_goal_expr, code_model, prog_context,
			mlds__defns, mlds__statements,
			ml_gen_info, ml_gen_info).
:- mode ml_gen_goal_expr(in, in, in, out, out, in, out) is det.

ml_gen_goal_expr(switch(Var, CanFail, CasesList), CodeModel, Context,
		MLDS_Decls, MLDS_Statements) -->
	ml_gen_switch(Var, CanFail, CasesList, CodeModel, Context,
		MLDS_Decls, MLDS_Statements).

ml_gen_goal_expr(some(_Vars, _CanRemove, Goal), CodeModel, Context,
		MLDS_Decls, MLDS_Statements) -->
	ml_gen_commit(Goal, CodeModel, Context, MLDS_Decls, MLDS_Statements).

ml_gen_goal_expr(if_then_else(_Vars, Cond, Then, Else),
		CodeModel, Context, MLDS_Decls, MLDS_Statements) -->
	ml_gen_ite(CodeModel, Cond, Then, Else, Context,
			MLDS_Decls, MLDS_Statements).

ml_gen_goal_expr(not(Goal), CodeModel, Context,
		MLDS_Decls, MLDS_Statements) -->
	ml_gen_negation(Goal, CodeModel, Context, MLDS_Decls, MLDS_Statements).

ml_gen_goal_expr(conj(Goals), CodeModel, Context,
		MLDS_Decls, MLDS_Statements) -->
	ml_gen_conj(Goals, CodeModel, Context, MLDS_Decls, MLDS_Statements).

ml_gen_goal_expr(disj(Goals), CodeModel, Context,
		MLDS_Decls, MLDS_Statements) -->
	ml_gen_disj(Goals, CodeModel, Context, MLDS_Decls, MLDS_Statements).

ml_gen_goal_expr(par_conj(Goals), CodeModel, Context,
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
	ml_gen_generic_call(GenericCall, Vars, Modes, Detism, Context,
		MLDS_Decls, MLDS_Statements).

ml_gen_goal_expr(call(PredId, ProcId, ArgVars, BuiltinState, _, PredName),
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
		% For the MLDS back-end, we can't treat
		% private_builtin:unsafe_type_cast as an
		% ordinary builtin, since the code that
		% builtin_ops__translate_builtin generates
		% for it is not type-correct.  Instead,
		% we handle it separately here.
		{ PredName = qualified(_, "unsafe_type_cast") }
	->
		ml_gen_var_list(ArgVars, ArgLvals),
		ml_variable_types(ArgVars, ArgTypes),
		(
			{ ArgLvals = [SrcLval, DestLval] },
			{ ArgTypes = [SrcType, DestType] }
		->
			ml_gen_box_or_unbox_rval(SrcType, DestType,
				lval(SrcLval), CastRval),
			{ Assign = ml_gen_assign(DestLval, CastRval,
				Context) },
			{ MLDS_Statements = [Assign] },
			{ MLDS_Decls = [] }
		;
			{ error("wrong number of args for unsafe_type_cast") }
		)
	;
		ml_gen_builtin(PredId, ProcId, ArgVars, CodeModel, Context,
			MLDS_Decls, MLDS_Statements)
	).

ml_gen_goal_expr(unify(_A, _B, _, Unification, _), CodeModel, Context,
		MLDS_Decls, MLDS_Statements) -->
	ml_gen_unification(Unification, CodeModel, Context,
		MLDS_Decls, MLDS_Statements).

ml_gen_goal_expr(foreign_proc(Attributes,
                PredId, ProcId, ArgVars, ArgDatas, OrigArgTypes, PragmaImpl),
		CodeModel, OuterContext, MLDS_Decls, MLDS_Statements) -->
        (
                { PragmaImpl = ordinary(Foreign_Code, _MaybeContext) },
                ml_gen_ordinary_pragma_foreign_proc(CodeModel, Attributes,
                        PredId, ProcId, ArgVars, ArgDatas, OrigArgTypes,
                        Foreign_Code, OuterContext, MLDS_Decls,
			MLDS_Statements)
        ;
                { PragmaImpl = nondet(
                        LocalVarsDecls, LocalVarsContext,
			FirstCode, FirstContext, LaterCode, LaterContext,
			_Treatment, SharedCode, SharedContext) },
                ml_gen_nondet_pragma_foreign_proc(CodeModel, Attributes,
                        PredId, ProcId, ArgVars, ArgDatas, OrigArgTypes,
			OuterContext, LocalVarsDecls, LocalVarsContext,
			FirstCode, FirstContext, LaterCode, LaterContext,
			SharedCode, SharedContext, MLDS_Decls, MLDS_Statements)
	;
		{ PragmaImpl = import(Name, HandleReturn, Vars, _Context) },
		{ ForeignCode = string__append_list([HandleReturn, " ",
				Name, "(", Vars, ");"]) },
                ml_gen_ordinary_pragma_foreign_proc(CodeModel, Attributes,
                        PredId, ProcId, ArgVars, ArgDatas, OrigArgTypes,
                        ForeignCode, OuterContext, MLDS_Decls, MLDS_Statements)
        ).

ml_gen_goal_expr(shorthand(_), _, _, _, _) -->
	% these should have been expanded out by now
	{ error("ml_gen_goal_expr: unexpected shorthand") }.

% :- module ml_foreign.
%
% ml_foreign creates MLDS code to execute foreign language code.
%
%

:- pred ml_gen_nondet_pragma_foreign_proc(code_model,
		pragma_foreign_proc_attributes,
		pred_id, proc_id, list(prog_var),
		list(maybe(pair(string, mode))), list(prog_type), prog_context,
		string, maybe(prog_context), string, maybe(prog_context),
		string, maybe(prog_context), string, maybe(prog_context),
		mlds__defns, mlds__statements, ml_gen_info, ml_gen_info).
:- mode ml_gen_nondet_pragma_foreign_proc(in, in, in, in, in, in, in, in,
		in, in, in, in, in, in, in, in, out, out, in, out) is det.

	% For model_non pragma c_code,
	% we generate code of the following form:
	%
	%	#define MR_PROC_LABEL <procedure name>
	% 	<declaration of locals needed for boxing/unboxing>
	%	{
	% 		<declaration of one local variable for each arg>
	%		struct {
	%			<user's local_vars decls>
	%		} MR_locals;
	%		MR_bool MR_done = MR_FALSE;
	%		MR_bool MR_succeeded = MR_FALSE;
	%
	%		#define FAIL		(MR_done = MR_TRUE)
	%		#define SUCCEED		(MR_succeeded = MR_TRUE)
	%		#define SUCCEED_LAST	(MR_succeeded = MR_TRUE, \
	%					 MR_done = MR_TRUE)
	%		#define LOCALS		(&MR_locals)
	%
	%		<assign input args>
	%		<obtain global lock>
	%		<user's first_code C code>
	%		while (true) {
	%			<user's shared_code C code>
	%			<release global lock>
	%			if (MR_succeeded) {
	%				<assign output args>
	% 				<boxing/unboxing of outputs>
	%				CONT();
	%			}
	%			if (MR_done) break;
	%			<obtain global lock>
	%			<user's later_code C code>
	%		}
	%
	%		#undef FAIL
	%		#undef SUCCEED
	%		#undef SUCCEED_LAST
	%		#undef LOCALS
	%	}
	%	#undef MR_PROC_LABEL
	%
	% We insert a #define for MR_PROC_LABEL, so that the C code in
	% the Mercury standard library that allocates memory manually
	% can use MR_PROC_LABEL as the procname argument to
	% incr_hp_msg(), for memory profiling.  Hard-coding the procname
	% argument in the C code would be wrong, since it wouldn't
	% handle the case where the original pragma c_code procedure
	% gets inlined and optimized away.  Of course we also need to
	% #undef it afterwards.
	%		
ml_gen_nondet_pragma_foreign_proc(CodeModel, Attributes,
		PredId, _ProcId, ArgVars, ArgDatas, OrigArgTypes, Context,
		LocalVarsDecls, LocalVarsContext, FirstCode, FirstContext,
		LaterCode, LaterContext, SharedCode, SharedContext,
		MLDS_Decls, MLDS_Statements) -->

	{ foreign_language(Attributes, Lang) },
	( { Lang = csharp } ->
		{ sorry(this_file, "nondet pragma foreign_proc for C#") }
	;
		[]
	),
	%
	% Combine all the information about the each arg
	%
	{ ml_make_c_arg_list(ArgVars, ArgDatas, OrigArgTypes,
		ArgList) },

	%
	% Generate <declaration of one local variable for each arg>
	%
	ml_gen_pragma_c_decls(Lang, ArgList, ArgDeclsList),

	%
	% Generate definitions of the FAIL, SUCCEED, SUCCEED_LAST,
	% and LOCALS macros
	%
	{ string__append_list([
"	#define FAIL		(MR_done = MR_TRUE)\n",
"	#define SUCCEED		(MR_succeeded = MR_TRUE)\n",
"	#define SUCCEED_LAST	(MR_succeeded = MR_TRUE, MR_done = MR_TRUE)\n",
"	#define LOCALS		(&MR_locals)\n"
		], HashDefines) },
	{ string__append_list([
			"	#undef	FAIL\n",
			"	#undef	SUCCEED\n",
			"	#undef	SUCCEED_LAST\n",
			"	#undef	LOCALS\n"
		], HashUndefs) },

	%
	% Generate code to set the values of the input variables.
	%
	ml_gen_pragma_c_input_arg_list(Lang, ArgList, AssignInputsList),

	%
	% Generate code to assign the values of the output variables.
	%
	ml_gen_pragma_c_output_arg_list(Lang, ArgList, Context,
		AssignOutputsList, ConvDecls, ConvStatements),

	%
	% Generate code fragments to obtain and release the global lock
	%
	{ thread_safe(Attributes, ThreadSafe) },
	ml_gen_obtain_release_global_lock(ThreadSafe, PredId,
		ObtainLock, ReleaseLock),

	%
	% Generate the MR_PROC_LABEL #define
	%
	ml_gen_hash_define_mr_proc_label(HashDefine),

	%
	% Put it all together
	%
	{ Starting_C_Code = list__condense([
			[raw_target_code("{\n", [])],
			HashDefine,
			ArgDeclsList,
			[raw_target_code("\tstruct {\n", []),
			user_target_code(LocalVarsDecls, LocalVarsContext, []),
			raw_target_code("\n", []),
			raw_target_code("\t} MR_locals;\n", []),
			raw_target_code("\tMR_bool MR_succeeded = MR_FALSE;\n",
				[]),
			raw_target_code("\tMR_bool MR_done = MR_FALSE;\n", []),
			raw_target_code("\n", []),
			raw_target_code(HashDefines, []),
			raw_target_code("\n", [])],
			AssignInputsList,
			[raw_target_code(ObtainLock, []),
			raw_target_code("\t{\n", []),
			user_target_code(FirstCode, FirstContext, []),
			raw_target_code("\n\t;}\n", []),
			raw_target_code("\twhile (1) {\n", []),
			raw_target_code("\t\t{\n", []),
			user_target_code(SharedCode, SharedContext, []),
			raw_target_code("\n\t\t;}\n", []),
			raw_target_code("#undef MR_PROC_LABEL\n", []),
			raw_target_code(ReleaseLock, []),
			raw_target_code("\t\tif (MR_succeeded) {\n", [])],
			AssignOutputsList
	]) },
	=(MLDSGenInfo),
	{ ml_gen_info_get_module_info(MLDSGenInfo, ModuleInfo) },
	{ module_info_globals(ModuleInfo, Globals) },
	{ globals__get_target(Globals, Target) },
	( { CodeModel = model_non } ->

		% For IL code, we can't call continutations because
		% there is no syntax for calling managed function
		% pointers in managed C++.  Instead we
		% have to call back into IL and make the continuation
		% call in IL.  This is called an "indirect" success
		% continuation call.
		
		(
			{ Target = il }
		->
			ml_gen_call_current_success_cont_indirectly(Context,
				CallCont)
		;
			ml_gen_call_current_success_cont(Context, CallCont)
		)
	;
		{ error("ml_gen_nondet_pragma_c_code: unexpected code model") }
	),
	{ Ending_C_Code = [
			raw_target_code("\t\t}\n", []),
			raw_target_code("\t\tif (MR_done) break;\n", []),
			raw_target_code(ObtainLock, []),
			raw_target_code("\t\t{\n", []),
			user_target_code(LaterCode, LaterContext, []),
			raw_target_code("\n\t\t;}\n", []),
			raw_target_code("\t}\n", []),
			raw_target_code("\n", []),
			raw_target_code(HashUndefs, []),
			raw_target_code("}\n", [])
	] },
	{ Starting_C_Code_Stmt = inline_target_code(lang_C, Starting_C_Code) },
	{ Starting_C_Code_Statement = mlds__statement(
		atomic(Starting_C_Code_Stmt), mlds__make_context(Context)) },
	{ Ending_C_Code_Stmt = inline_target_code(lang_C, Ending_C_Code) },
	{ Ending_C_Code_Statement = mlds__statement(
		atomic(Ending_C_Code_Stmt), mlds__make_context(Context)) },
	{ MLDS_Statements = list__condense([
		[Starting_C_Code_Statement],
		ConvStatements,
		[CallCont,
		Ending_C_Code_Statement]
	]) },
	{ MLDS_Decls = ConvDecls }.

:- pred ml_gen_ordinary_pragma_foreign_proc(code_model, 
		pragma_foreign_proc_attributes,
		pred_id, proc_id, list(prog_var),
		list(maybe(pair(string, mode))), list(prog_type),
		string, prog_context,
		mlds__defns, mlds__statements, ml_gen_info, ml_gen_info).
:- mode ml_gen_ordinary_pragma_foreign_proc(in, in, in, in, in, in, 
		in, in, in, out, out, in, out) is det.

ml_gen_ordinary_pragma_foreign_proc(CodeModel, Attributes,
		PredId, ProcId, ArgVars, ArgDatas, OrigArgTypes,
		Foreign_Code, Context, MLDS_Decls, MLDS_Statements) -->
	{ foreign_language(Attributes, Lang) },
	( { Lang = c },
		ml_gen_ordinary_pragma_c_proc(CodeModel, Attributes,
			PredId, ProcId, ArgVars, ArgDatas, OrigArgTypes,
			Foreign_Code, Context, MLDS_Decls, MLDS_Statements)
	; { Lang = managed_cplusplus },
		ml_gen_ordinary_pragma_c_proc(CodeModel, Attributes,
			PredId, ProcId, ArgVars, ArgDatas, OrigArgTypes,
			Foreign_Code, Context, MLDS_Decls, MLDS_Statements)
	; { Lang = csharp },
		ml_gen_ordinary_pragma_csharp_proc(CodeModel, Attributes,
			PredId, ProcId, ArgVars, ArgDatas, OrigArgTypes,
			Foreign_Code, Context, MLDS_Decls, MLDS_Statements)
	; { Lang = il },
		ml_gen_ordinary_pragma_il_proc(CodeModel, Attributes,
			PredId, ProcId, ArgVars, ArgDatas, OrigArgTypes,
			Foreign_Code, Context, MLDS_Decls, MLDS_Statements)
	).

:- pred ml_gen_ordinary_pragma_csharp_proc(code_model, 
		pragma_foreign_proc_attributes,
		pred_id, proc_id, list(prog_var),
		list(maybe(pair(string, mode))), list(prog_type),
		string, prog_context,
		mlds__defns, mlds__statements, ml_gen_info, ml_gen_info).
:- mode ml_gen_ordinary_pragma_csharp_proc(in, in, in, in, in, in, 
		in, in, in, out, out, in, out) is det.

	% For ordinary (not model_non) pragma foreign_code in C#,
	% we generate a call to an out-of-line procedure that contains
	% the user's code.

ml_gen_ordinary_pragma_csharp_proc(CodeModel, Attributes,
		_PredId, _ProcId, _ArgVars, _ArgDatas, _OrigArgTypes,
		ForeignCode, Context, MLDS_Decls, MLDS_Statements) -->
	{ foreign_language(Attributes, ForeignLang) },
	{ MLDSContext = mlds__make_context(Context) },
	=(MLDSGenInfo),
	{ ml_gen_info_get_value_output_vars(MLDSGenInfo, OutputVars) },
	ml_gen_var_list(OutputVars, OutputVarLvals),
	{ OutlineStmt = outline_foreign_proc(ForeignLang, OutputVarLvals,
		ForeignCode) },

	{ ml_gen_info_get_module_info(MLDSGenInfo, ModuleInfo) },
	{ module_info_name(ModuleInfo, ModuleName) },
	{ MLDSModuleName = mercury_module_name_to_mlds(ModuleName) },

		% If the code is semidet, we should copy SUCCESS_INDICATOR 
		% out into "suceess".
	
	ml_success_lval(SucceededLval),

	{ CodeModel = model_semi ->
		SuccessIndicatorVarName = var_name("SUCCESS_INDICATOR", no),
		SuccessIndicatorDecl = ml_gen_mlds_var_decl(
			var(SuccessIndicatorVarName),
			mlds__native_bool_type,
			no_initializer, no, MLDSContext),
		SuccessIndicatorLval = var(qual(MLDSModuleName,
			SuccessIndicatorVarName), mlds__native_bool_type),
		SuccessIndicatorStatement = ml_gen_assign(SucceededLval,
			lval(SuccessIndicatorLval), Context),
		SuccessVarLocals = [SuccessIndicatorDecl],
		SuccessIndicatorStatements = [SuccessIndicatorStatement]
	;
		SuccessVarLocals = [],
		SuccessIndicatorStatements = []
	},

	{ MLDS_Statements = [
		mlds__statement(atomic(OutlineStmt), MLDSContext) |
		SuccessIndicatorStatements
		] },
	{ MLDS_Decls = SuccessVarLocals }.

:- pred ml_gen_ordinary_pragma_il_proc(code_model, 
	pragma_foreign_proc_attributes, pred_id, proc_id, list(prog_var),
	list(maybe(pair(string, mode))), list(prog_type), string, prog_context,
	mlds__defns, mlds__statements, ml_gen_info, ml_gen_info).
:- mode ml_gen_ordinary_pragma_il_proc(in, in, in, in, in, in, in, in, in,
	out, out, in, out) is det.

ml_gen_ordinary_pragma_il_proc(_CodeModel, Attributes,
	PredId, ProcId, ArgVars, _ArgDatas, OrigArgTypes,
	ForeignCode, Context, MLDS_Decls, MLDS_Statements) -->

	{ MLDSContext = mlds__make_context(Context) },

	=(MLDSGenInfo),
	{ ml_gen_info_get_module_info(MLDSGenInfo, ModuleInfo) },
	{ module_info_pred_proc_info(ModuleInfo, PredId, ProcId,
			_PredInfo, ProcInfo) },
	{ proc_info_varset(ProcInfo, VarSet) },
%	{ proc_info_vartypes(ProcInfo, VarTypes) },
	% note that for headvars we must use the types from
	% the procedure interface, not from the procedure body
	{ HeadVarTypes = map__from_corresponding_lists(ArgVars,
		OrigArgTypes) },
	{ ml_gen_info_get_byref_output_vars(MLDSGenInfo, ByRefOutputVars) },
	{ ml_gen_info_get_value_output_vars(MLDSGenInfo, CopiedOutputVars) },
	{ module_info_name(ModuleInfo, ModuleName) },
	{ MLDSModuleName = mercury_module_name_to_mlds(ModuleName) },


	% XXX in the code to marshall parameters, fjh says:
	% we need to handle the case where the types in the procedure interface
	% are polymorphic, but the types of the vars in the `foreign_proc' HLDS
	% goal are concrete instances of those types, which can happen when the
	% procedure is inlined or specialized.  The assignment that you
	% generate here with ml_gen_assign won't be type-correct.  In general
	% you may need to box/unbox the arguments.

		% Generate statements to assign by-ref output arguments
	{ list__filter_map(	
		(pred(Var::in, Statement::out) is semidet :- 
			map__lookup(HeadVarTypes, Var, Type),
			not type_util__is_dummy_argument_type(Type),
			VarName = mlds__var_name(VarNameString, _MangleInt),
			MLDSType = mercury_type_to_mlds_type(ModuleInfo, Type),

			VarName = ml_gen_var_name(VarSet, Var),
			QualVarName = qual(MLDSModuleName, VarName),
			OutputVarLval = mem_ref(lval(
				var(QualVarName, MLDSType)), MLDSType),

			NonMangledVarName = mlds__var_name(VarNameString, no),
			QualLocalVarName= qual(MLDSModuleName,
				NonMangledVarName),
			LocalVarLval = var(QualLocalVarName, MLDSType),

			Statement = ml_gen_assign(OutputVarLval,
				lval(LocalVarLval), Context)
		), ByRefOutputVars, ByRefAssignStatements) },

		% Generate statements to assign copied output arguments
	{ list__filter_map(	
		(pred(Var::in, Statement::out) is semidet :- 
			map__lookup(HeadVarTypes, Var, Type),
			not type_util__is_dummy_argument_type(Type),
			VarName = mlds__var_name(VarNameString, _MangleInt),
			MLDSType = mercury_type_to_mlds_type(ModuleInfo, Type),

			VarName = ml_gen_var_name(VarSet, Var),
			QualVarName = qual(MLDSModuleName, VarName),
				% this line differs from above
			OutputVarLval = var(QualVarName, MLDSType),

			NonMangledVarName = mlds__var_name(VarNameString, no),
			QualLocalVarName= qual(MLDSModuleName,
				NonMangledVarName),
			LocalVarLval = var(QualLocalVarName, MLDSType),

			Statement = ml_gen_assign(OutputVarLval,
				lval(LocalVarLval), Context)
		), CopiedOutputVars, CopiedOutputStatements) },


		% Generate declarations for all the variables, and
		% initializers for input variables.
	{ list__map_foldl(
		(pred(Var::in, MLDS_Defn::out, Box0::in, Box::out) is det :- 
			map__lookup(HeadVarTypes, Var, Type),
			VarName = ml_gen_var_name(VarSet, Var),
			VarName = mlds__var_name(VarNameString, _MangleInt),
			NonMangledVarName = mlds__var_name(VarNameString, no),
				% Dummy arguments are just mapped to integers,
				% since they shouldn't be used in any
				% way that requires them to have a real value.
			( type_util__is_dummy_argument_type(Type) ->
				Initializer = no_initializer,
				MLDSType = mlds__native_int_type,
				Box0 = Box
			; list__member(Var, ByRefOutputVars) ->
				Initializer = no_initializer,
				MLDSType = mercury_type_to_mlds_type(
					ModuleInfo, Type),
				Box0 = Box
			; list__member(Var, CopiedOutputVars) ->
				Initializer = no_initializer,
				MLDSType = mercury_type_to_mlds_type(
					ModuleInfo, Type),
				Box0 = Box
			;
				MLDSType = mercury_type_to_mlds_type(
					ModuleInfo, Type),
				QualVarName = qual(MLDSModuleName, VarName),
				Initializer = init_obj(
					lval(var(QualVarName, MLDSType))),
				Box0 = Box
			),
			% XXX Accurate GC is not supported for IL foreign code;
			% this would only be useful if interfacing to
			% IL when compiling to C, which is not yet supported.
			GC_TraceCode = no,
			MLDS_Defn = ml_gen_mlds_var_decl(
				var(NonMangledVarName), MLDSType,
				Initializer, GC_TraceCode, MLDSContext) 
		), ArgVars, VarLocals, [], BoxStatements) },

	{ OutlineStmt = inline_target_code(lang_il, [
		user_target_code(ForeignCode, yes(Context),
			get_target_code_attributes(il,
				Attributes ^ extra_attributes)) 
		]) },

	{ ILCodeFragment = mlds__statement(atomic(OutlineStmt), MLDSContext) },
	{ MLDS_Statements = [statement(block(VarLocals, 
		BoxStatements ++ [ILCodeFragment] ++ ByRefAssignStatements
			++ CopiedOutputStatements),
		mlds__make_context(Context))] },
	{ MLDS_Decls = [] }.


:- pred ml_gen_ordinary_pragma_c_proc(code_model, 
		pragma_foreign_proc_attributes,
		pred_id, proc_id, list(prog_var),
		list(maybe(pair(string, mode))), list(prog_type),
		string, prog_context,
		mlds__defns, mlds__statements, ml_gen_info, ml_gen_info).
:- mode ml_gen_ordinary_pragma_c_proc(in, in, in, in, in, in, 
		in, in, in, out, out, in, out) is det.

	% For ordinary (not model_non) pragma c_proc,
	% we generate code of the following form:
	%
	% model_det pragma_c_proc:
	%
	%	#define MR_PROC_LABEL <procedure name>
	% 	<declaration of locals needed for boxing/unboxing>
	%	{
	% 		<declaration of one local variable for each arg>
	%
	%		<assign input args>
	%		<obtain global lock>
	%		<c code>
	% 		<boxing/unboxing of outputs>
	%		<release global lock>
	%		<assign output args>
	%	}
	%	#undef MR_PROC_LABEL
	%		
	% model_semi pragma_c_proc:
	%
	%	#define MR_PROC_LABEL <procedure name>
	% 	<declaration of locals needed for boxing/unboxing>
	%	{
	% 		<declaration of one local variable for each arg>
	%		MR_bool SUCCESS_INDICATOR;
	%
	%		<assign input args>
	%		<obtain global lock>
	%		<c code>
	%		<release global lock>
	%		if (SUCCESS_INDICATOR) {
	%			<assign output args>
	% 			<boxing/unboxing of outputs>
	%		}
	%		
	%		<succeeded> = SUCCESS_INDICATOR;
	%	}
	%	#undef MR_PROC_LABEL
	%
	% We insert a #define for MR_PROC_LABEL, so that the C code in
	% the Mercury standard library that allocates memory manually
	% can use MR_PROC_LABEL as the procname argument to
	% incr_hp_msg(), for memory profiling.  Hard-coding the procname
	% argument in the C code would be wrong, since it wouldn't
	% handle the case where the original pragma c_code procedure
	% gets inlined and optimized away.  Of course we also need to
	% #undef it afterwards.
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
ml_gen_ordinary_pragma_c_proc(CodeModel, Attributes,
		PredId, _ProcId, ArgVars, ArgDatas, OrigArgTypes,
		C_Code, Context, MLDS_Decls, MLDS_Statements) -->

	{ foreign_language(Attributes, Lang) },

	%
	% Combine all the information about the each arg
	%
	{ ml_make_c_arg_list(ArgVars, ArgDatas, OrigArgTypes,
		ArgList) },

	%
	% Generate <declaration of one local variable for each arg>
	%
	ml_gen_pragma_c_decls(Lang, ArgList, ArgDeclsList),

	%
	% Generate code to set the values of the input variables.
	%
	ml_gen_pragma_c_input_arg_list(Lang, ArgList, AssignInputsList),

	%
	% Generate code to assign the values of the output variables.
	%
	ml_gen_pragma_c_output_arg_list(Lang, ArgList, Context,
		AssignOutputsList, ConvDecls, ConvStatements),

	%
	% Generate code fragments to obtain and release the global lock
	%
	{ thread_safe(Attributes, ThreadSafe) },
	ml_gen_obtain_release_global_lock(ThreadSafe, PredId,
		ObtainLock, ReleaseLock),

	%
	% Generate the MR_PROC_LABEL #define
	%
	ml_gen_hash_define_mr_proc_label(HashDefine),

	%
	% Put it all together
	%
	( { CodeModel = model_det } ->
		{ Starting_C_Code = list__condense([
			[raw_target_code("{\n", [])],
			HashDefine,
			ArgDeclsList,
			[raw_target_code("\n", [])],
			AssignInputsList,
			[raw_target_code(ObtainLock, []),
			raw_target_code("\t\t{\n", []),
			user_target_code(C_Code, yes(Context), []),
			raw_target_code("\n\t\t;}\n", []),
			raw_target_code("#undef MR_PROC_LABEL\n", []),
			raw_target_code(ReleaseLock, [])],
			AssignOutputsList
		]) },
		{ Ending_C_Code = [raw_target_code("}\n", [])] }
	; { CodeModel = model_semi } ->
		ml_success_lval(SucceededLval),
		{ Starting_C_Code = list__condense([
			[raw_target_code("{\n", [])],
			HashDefine,
			ArgDeclsList,
			[raw_target_code("\tMR_bool SUCCESS_INDICATOR;\n", []),
			raw_target_code("\n", [])],
			AssignInputsList,
			[raw_target_code(ObtainLock, []),
			raw_target_code("\t\t{\n", []),
			user_target_code(C_Code, yes(Context), []),
			raw_target_code("\n\t\t;}\n", []),
			raw_target_code("#undef MR_PROC_LABEL\n", []),
			raw_target_code(ReleaseLock, []),
			raw_target_code("\tif (SUCCESS_INDICATOR) {\n", [])],
			AssignOutputsList
		]) },
		{ Ending_C_Code = [
			raw_target_code("\t}\n", []),
			target_code_output(SucceededLval),
			raw_target_code(" = SUCCESS_INDICATOR;\n", []),
			raw_target_code("}\n", [])
		] }
	;
		{ error("ml_gen_ordinary_pragma_c_code: unexpected code model") }
	),
	{ Starting_C_Code_Stmt = inline_target_code(lang_C, Starting_C_Code) },
	{ Ending_C_Code_Stmt = inline_target_code(lang_C, Ending_C_Code) },
	{ Starting_C_Code_Statement = mlds__statement(
		atomic(Starting_C_Code_Stmt), mlds__make_context(Context)) },
	{ Ending_C_Code_Statement = mlds__statement(atomic(Ending_C_Code_Stmt),
		mlds__make_context(Context)) },
	{ MLDS_Statements = list__condense([
		[Starting_C_Code_Statement],
		ConvStatements,
		[Ending_C_Code_Statement]
	]) },
	{ MLDS_Decls = ConvDecls }.

	% Generate code fragments to obtain and release the global lock
	% (this is used for ensuring thread safety in a concurrent
	% implementation)
	%
:- pred ml_gen_obtain_release_global_lock(thread_safe, pred_id,
		string, string, ml_gen_info, ml_gen_info).
:- mode ml_gen_obtain_release_global_lock(in, in, out, out, in, out) is det.

ml_gen_obtain_release_global_lock(ThreadSafe, PredId,
		ObtainLock, ReleaseLock) -->
	=(MLDSGenInfo),
	{ ml_gen_info_get_module_info(MLDSGenInfo, ModuleInfo) },
	{ module_info_globals(ModuleInfo, Globals) },
	{ globals__lookup_bool_option(Globals, parallel, Parallel) },
	{
		Parallel = yes,
		ThreadSafe = not_thread_safe
	->
		module_info_pred_info(ModuleInfo, PredId, PredInfo),
		pred_info_name(PredInfo, Name),
		c_util__quote_string(Name, MangledName),
		string__append_list(["\tMR_OBTAIN_GLOBAL_LOCK(""",
			MangledName, """);\n"], ObtainLock),
		string__append_list(["\tMR_RELEASE_GLOBAL_LOCK(""",
			MangledName, """);\n"], ReleaseLock)
	;
		ObtainLock = "",
		ReleaseLock = ""
	}.

:- pred ml_gen_hash_define_mr_proc_label(list(target_code_component)::out,
		ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_hash_define_mr_proc_label(HashDefine) -->
	=(MLDSGenInfo),
	{ ml_gen_info_get_module_info(MLDSGenInfo, ModuleInfo) },
	% Note that we use the pred_id and proc_id of the current procedure,
	% not the one that the pragma foreign_code originally came from.
	% There may not be any function address for the latter, e.g. if it
	% has been inlined and the original definition optimized away.
	{ ml_gen_info_get_pred_id(MLDSGenInfo, PredId) },
	{ ml_gen_info_get_proc_id(MLDSGenInfo, ProcId) },
	{ ml_gen_proc_label(ModuleInfo, PredId, ProcId, MLDS_Name,
			MLDS_Module) },
	{ HashDefine = [raw_target_code("#define MR_PROC_LABEL ", []),
			name(qual(MLDS_Module, MLDS_Name)),
			raw_target_code("\n", [])] }.


:- func get_target_code_attributes(foreign_language,
		pragma_foreign_proc_extra_attributes) = target_code_attributes.
get_target_code_attributes(_, []) = [].
get_target_code_attributes(Lang, [max_stack_size(N) | Xs]) = 
	( Lang = il ->
		[max_stack_size(N) | get_target_code_attributes(Lang, Xs)]
	;
		[]
	).


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
% for a `pragma foreign_proc' declaration.
%
:- pred ml_gen_pragma_c_decls(foreign_language::in, list(ml_c_arg)::in,
		list(target_code_component)::out,
		ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_pragma_c_decls(_, [], []) --> [].
ml_gen_pragma_c_decls(Lang, [Arg|Args], [Decl|Decls]) -->
	ml_gen_pragma_c_decl(Lang, Arg, Decl),
	ml_gen_pragma_c_decls(Lang, Args, Decls).

% ml_gen_pragma_c_decl generates C code to declare an argument
% of a `pragma foreign_proc' declaration.
%
:- pred ml_gen_pragma_c_decl(foreign_language::in,
		ml_c_arg::in, target_code_component::out,
		ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_pragma_c_decl(Lang, ml_c_arg(_Var, MaybeNameAndMode, Type),
		Decl) -->
	=(MLDSGenInfo),
	{ ml_gen_info_get_module_info(MLDSGenInfo, ModuleInfo) },
	{
		MaybeNameAndMode = yes(ArgName - _Mode),
		\+ var_is_singleton(ArgName)
	->
		TypeString = to_type_string(Lang, ModuleInfo, Type),
		string__format("\t%s %s;\n", [s(TypeString), s(ArgName)],
			DeclString)
	;
		% if the variable doesn't occur in the ArgNames list,
		% it can't be used, so we just ignore it
		DeclString = ""
	},
	{ Decl = raw_target_code(DeclString, []) }.

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

:- pred ml_gen_pragma_c_input_arg_list(foreign_language::in,
		list(ml_c_arg)::in, list(target_code_component)::out,
		ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_pragma_c_input_arg_list(Lang, ArgList, AssignInputs) -->
	list__map_foldl(ml_gen_pragma_c_input_arg(Lang), ArgList,
		AssignInputsList),
	{ list__condense(AssignInputsList, AssignInputs) }.

% ml_gen_pragma_c_input_arg generates C code to assign the value of an
% input arg for a `pragma foreign_proc' declaration.
%
:- pred ml_gen_pragma_c_input_arg(foreign_language::in, ml_c_arg::in,
		list(target_code_component)::out,
		ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_pragma_c_input_arg(Lang, ml_c_arg(Var, MaybeNameAndMode, OrigType),
		AssignInput) -->
	=(MLDSGenInfo),
	{ ml_gen_info_get_module_info(MLDSGenInfo, ModuleInfo) },
	(
		{ MaybeNameAndMode = yes(ArgName - Mode) },
		{ \+ var_is_singleton(ArgName) },
		{ mode_to_arg_mode(ModuleInfo, Mode, OrigType, top_in) }
	->
		ml_variable_type(Var, VarType),
		ml_gen_var(Var, VarLval),
		( { type_util__is_dummy_argument_type(VarType) } ->
			% The variable may not have been declared,
			% so we need to generate a dummy value for it.
			% Using `0' here is more efficient than
			% using private_builtin__dummy_var, which is
			% what ml_gen_var will have generated for this
			% variable.
			{ ArgRval = const(int_const(0)) }
		;
			ml_gen_box_or_unbox_rval(VarType, OrigType,
				lval(VarLval), ArgRval)
		),
		{ module_info_globals(ModuleInfo, Globals) },
		{ globals__lookup_bool_option(Globals, highlevel_data,
			HighLevelData) },
		{ HighLevelData = yes ->
			% In general, the types used for the C interface
			% are not the same as the types used by
			% --high-level-data, so we always use a cast here.
			% (Strictly speaking the cast is not needed for
			% a few cases like `int', but it doesn't do any harm.)
			TypeString = to_type_string(Lang, ModuleInfo, OrigType),
			string__format("(%s)", [s(TypeString)], Cast)
		;
			% For --no-high-level-data, we only need to use
			% a cast is for polymorphic types, which are
			% `Word' in the C interface but `MR_Box' in the
			% MLDS back-end.
			% Except for MC++, where polymorphic types
			% are MR_Box.
			( 
				type_util__var(OrigType, _),
				Lang \= managed_cplusplus
			->
				Cast = "(MR_Word) "
			;
				Cast = ""
			)
		},
		{ string__format("\t%s = %s\n",
			[s(ArgName), s(Cast)],
			AssignToArgName) },
		{ AssignInput = [
			raw_target_code(AssignToArgName, []),
			target_code_input(ArgRval),
			raw_target_code(";\n", [])
		] }
	;
		% if the variable doesn't occur in the ArgNames list,
		% it can't be used, so we just ignore it
		{ AssignInput = [] }
	).

:- pred ml_gen_pragma_c_output_arg_list(foreign_language::in,
		list(ml_c_arg)::in, prog_context::in,
		list(target_code_component)::out,
		mlds__defns::out, mlds__statements::out,
		ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_pragma_c_output_arg_list(_, [], _, [], [], []) --> [].
ml_gen_pragma_c_output_arg_list(Lang, [C_Arg | C_Args], Context,
		Components, ConvDecls, ConvStatements) -->
	ml_gen_pragma_c_output_arg(Lang, C_Arg, Context, Components1,
			ConvDecls1, ConvStatements1),
	ml_gen_pragma_c_output_arg_list(Lang, C_Args, Context,
			Components2, ConvDecls2, ConvStatements2),
	{ Components = list__append(Components1, Components2) },
	{ ConvDecls = list__append(ConvDecls1, ConvDecls2) },
	{ ConvStatements = list__append(ConvStatements1, ConvStatements2) }.

% ml_gen_pragma_c_output_arg generates C code to assign the value of an output
% arg for a `pragma foreign_proc' declaration.
%
:- pred ml_gen_pragma_c_output_arg(foreign_language::in,
		ml_c_arg::in, prog_context::in,
		list(target_code_component)::out,
		mlds__defns::out, mlds__statements::out,
		ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_pragma_c_output_arg(Lang, ml_c_arg(Var, MaybeNameAndMode, OrigType),
		Context, AssignOutput, ConvDecls, ConvOutputStatements) -->
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
		ml_gen_box_or_unbox_lval(VarType, OrigType, VarLval,
			mlds__var_name(ArgName, no),
			Context, ArgLval, ConvDecls, _ConvInputStatements,
			ConvOutputStatements),
		{ module_info_globals(ModuleInfo, Globals) },
		{ globals__lookup_bool_option(Globals, highlevel_data,
			HighLevelData) },
		{ HighLevelData = yes ->
			% In general, the types used for the C interface
			% are not the same as the types used by
			% --high-level-data, so we always use a cast here.
			% (Strictly speaking the cast is not needed for
			% a few cases like `int', but it doesn't do any harm.)
			% Note that we can't easily obtain the type string
			% for the RHS of the assignment, so instead we
			% cast the LHS.
			TypeString = to_type_string(Lang, ModuleInfo, OrigType),
			string__format("*(%s *)&", [s(TypeString)], LHS_Cast),
			RHS_Cast = ""
		;
			% For --no-high-level-data, we only need to use
			% a cast is for polymorphic types, which are
			% `Word' in the C interface but `MR_Box' in the
			% MLDS back-end.
			( type_util__var(OrigType, _) ->
				RHS_Cast = "(MR_Box) "
			;
				RHS_Cast = ""
			),
			LHS_Cast = ""
		},
		{ string__format(" = %s%s;\n", [s(RHS_Cast), s(ArgName)],
			AssignFromArgName) },
		{ string__format("\t%s\n", [s(LHS_Cast)], AssignTo) },
		{ AssignOutput = [
			raw_target_code(AssignTo, []),
			target_code_output(ArgLval),
			raw_target_code(AssignFromArgName, [])
		] }
	;
		% if the variable doesn't occur in the ArgNames list,
		% it can't be used, so we just ignore it
		{ AssignOutput = [] },
		{ ConvDecls = [] },
		{ ConvOutputStatements = [] }
	).

% :- end_module ml_foreign.

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
		%	model_det Cond:
		%		<(Cond -> Then ; Else)>
		%	===>
		%		<Cond>
		%		<Then>
		{ CondCodeModel = model_det },
		ml_gen_goal(model_det, Cond, CondStatement),
		ml_gen_goal(CodeModel, Then, ThenStatement),
		{ MLDS_Decls = [] },
		{ MLDS_Statements = [CondStatement, ThenStatement] }
	;
		%	model_semi cond:
		%		<(Cond -> Then ; Else)>
		%	===>
		%		MR_bool succeeded;
		%	
		%		<succeeded = Cond>
		%		if (succeeded) {
		%			<Then>
		%		} else {
		%			<Else>
		%		}
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
		%	model_non cond:
		%		<(Cond -> Then ; Else)>
		%	===>
		%		MR_bool cond_<N>;
		%
		%		void then_func() {
		%			cond_<N> = MR_TRUE;
		%			<Then>
		%		}
		%
		%		cond_<N> = MR_FALSE;
		%		<Cond && then_func()>
		%		if (!cond_<N>) {
		%			<Else>
		%		}
		%	except that we hoist any declarations generated
		%	for <Cond> to the top of the scope, so that they
		%	are in scope for the <Then> goal
		%	(this is needed for declarations of static consts)


		{ CondCodeModel = model_non },

		% generate the `cond_<N>' var and the code to initialize it to false
		ml_gen_info_new_cond_var(CondVar),
		{ MLDS_Context = mlds__make_context(Context) },
		{ CondVarDecl = ml_gen_cond_var_decl(CondVar, MLDS_Context) },
		ml_gen_set_cond_var(CondVar, const(false), Context,
			SetCondFalse),

		% allocate a name for the `then_func'
		ml_gen_new_func_label(no, ThenFuncLabel, ThenFuncLabelRval),

		% generate <Cond && then_func()>
		ml_get_env_ptr(EnvPtrRval),
		{ SuccessCont = success_cont(ThenFuncLabelRval, EnvPtrRval,
			[], []) },
		ml_gen_info_push_success_cont(SuccessCont),
		ml_gen_goal(model_non, Cond, CondDecls, CondStatements),
		ml_gen_info_pop_success_cont,

		% generate the `then_func'
		/* push nesting level */
		{ Then = _ - ThenGoalInfo },
		{ goal_info_get_context(ThenGoalInfo, ThenContext) },
		ml_gen_set_cond_var(CondVar, const(true), ThenContext,
			SetCondTrue),
		ml_gen_goal(CodeModel, Then, ThenStatement),
		{ ThenFuncBody = ml_gen_block([],
			[SetCondTrue, ThenStatement], ThenContext) },
		/* pop nesting level */
		ml_gen_nondet_label_func(ThenFuncLabel, ThenContext,
			ThenFuncBody, ThenFunc),

		% generate `if (!cond_<N>) { <Else> }'
		ml_gen_test_cond_var(CondVar, CondSucceeded),
		ml_gen_goal(CodeModel, Else, ElseStatement),
		{ IfStmt = if_then_else(unop(std_unop(not), CondSucceeded),
			ElseStatement, no) },
		{ IfStatement = mlds__statement(IfStmt, MLDS_Context) },

		% package it all up in the right order
		{ MLDS_Decls = list__append([CondVarDecl | CondDecls], [ThenFunc]) },
		{ MLDS_Statements = list__append(
			[SetCondFalse | CondStatements], [IfStatement]) }
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
		%		MR_bool succeeded;
		%		<succeeded = Goal>
		%		/* now ignore the value of succeeded,
		%		   which we know will be MR_FALSE */
		%	}
		{ CodeModel = model_det },
		ml_gen_goal(model_semi, Cond, MLDS_Decls, MLDS_Statements)
	;
		% model_semi negation, model_det goal:
		%		<succeeded = not(Goal)>
		%	===>
		%		<do Goal>
		%		succeeded = MR_FALSE;
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
		%		<succeeded = Goal>
		%		succeeded = !succeeded;
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
	{ goal_info_get_determinism(FirstGoalInfo, FirstDeterminism) },
	( { determinism_components(FirstDeterminism, _, at_most_zero) } ->
		% the `Rest' code is unreachable
		ml_gen_goal(CodeModel, First, MLDS_Decls, MLDS_Statements)
	;
		{ determinism_to_code_model(FirstDeterminism, FirstCodeModel) },
		{ DoGenFirst = ml_gen_goal(FirstCodeModel, First) },
		{ DoGenRest = ml_gen_conj(Rest, CodeModel, Context) },
		ml_combine_conj(FirstCodeModel, Context,
			DoGenFirst, DoGenRest, MLDS_Decls, MLDS_Statements)
	).

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
	% Note that each arm of the model_non disjunction is placed into
	% a block.  This avoids a problem where ml_join_decls can create
	% block nesting proportional to the size of the disjunction.
	% The nesting can hit fixed limit problems in some C compilers.
	%
ml_gen_disj([SingleGoal], CodeModel, Context, [], [MLDS_Statement]) -->
	ml_gen_goal(CodeModel, SingleGoal, Goal_Decls, Goal_Statements),
	{ MLDS_Statement = ml_gen_block(Goal_Decls, Goal_Statements,
			Context) }.

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

		(
			{ RestDecls = [] }
		->
			{ FirstBlock = ml_gen_block(FirstDecls,
					FirstStatements, Context) },
			{ MLDS_Decls = [] },
			{ MLDS_Statements = [FirstBlock | RestStatements] }
		;
			{ error("ml_gen_disj: RestDecls not empty.") }
		)

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
		%		MR_bool succeeded;
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
%
% Code for handling attributes
%

:- func attributes_to_mlds_attributes(module_info, list(hlds_pred__attribute))
		= list(mlds__attribute).
attributes_to_mlds_attributes(ModuleInfo, Attrs) =
	list__map(attribute_to_mlds_attribute(ModuleInfo), Attrs).

:- func attribute_to_mlds_attribute(module_info, hlds_pred__attribute)
	= mlds__attribute.
attribute_to_mlds_attribute(ModuleInfo, custom(Type)) = 
	custom(mercury_type_to_mlds_type(ModuleInfo, Type)).

%-----------------------------------------------------------------------------%

:- func this_file = string.
this_file = "ml_code_gen.m".

:- end_module ml_code_gen.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
