%-----------------------------------------------------------------------------%
% Copyright (C) 2000-2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: ml_optimize.m
% Main author: trd, fjh

% This module runs various optimizations on the MLDS.
%
% Currently the optimizations we do here are
%	- turning tailcalls into loops;
%	- converting assignments to local variables into variable initializers.
%	- eliminating initialized local variables entirely,
%	  by replacing occurrences of such variables with their initializer
%
% Note that tailcall detection is done in ml_tailcall.m.
% It might be nice to move the detection here, and do both the
% loop transformation (in the case of self-tailcalls) and marking
% tailcalls at the same time.
%
% Ultimately this module should just consist of a skeleton to traverse
% the MLDS, and should call various optimization modules along the way.
%
% It would probably be a good idea to make each transformation optional.
% Previously the tailcall transformation depended on emit_c_loops, but
% this is a bit misleading given the documentation of emit_c_loops.

%-----------------------------------------------------------------------------%

:- module ml_backend__ml_optimize.
:- interface.

:- import_module ml_backend__mlds, io.

:- pred optimize(mlds, mlds, io__state, io__state).
:- mode optimize(in, out, di, uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module ml_backend__ml_util, ml_backend__ml_code_util.
:- import_module parse_tree__prog_data, parse_tree__prog_util.
:- import_module backend_libs__builtin_ops, libs__globals, libs__options.
:- import_module hlds__error_util.

:- import_module bool, int, list, require, std_util, string.

:- type opt_info --->
	opt_info(
		globals		:: globals,
		module_name 	:: mlds_module_name,
		entity_name 	:: mlds__entity_name,
		func_params 	:: mlds__func_params,
		context 	:: mlds__context
	).

optimize(MLDS0, MLDS) -->
	globals__io_get_globals(Globals),
	{ MLDS0 = mlds(ModuleName, ForeignCode, Imports, Defns0) },
	{ Defns = optimize_in_defns(Defns0, Globals, 
		mercury_module_name_to_mlds(ModuleName)) },
	{ MLDS = mlds(ModuleName, ForeignCode, Imports, Defns) }.

:- func optimize_in_defns(mlds__defns, globals, mlds_module_name) 
	= mlds__defns.
optimize_in_defns(Defns, Globals, ModuleName) = 
	list__map(optimize_in_defn(ModuleName, Globals), Defns).

:- func optimize_in_defn(mlds_module_name, globals, mlds__defn) = mlds__defn.
optimize_in_defn(ModuleName, Globals, Defn0) = Defn :-
	Defn0 = mlds__defn(Name, Context, Flags, DefnBody0),
	(
		DefnBody0 = mlds__function(PredProcId, Params, FuncBody0,
			Attributes),
		OptInfo = opt_info(Globals, ModuleName, Name, Params, Context),

		FuncBody1 = optimize_func(OptInfo, FuncBody0),
		FuncBody = optimize_in_function_body(OptInfo, FuncBody1),

		DefnBody = mlds__function(PredProcId, Params, FuncBody,
			Attributes),
		Defn = mlds__defn(Name, Context, Flags, DefnBody)
	;
		DefnBody0 = mlds__data(_, _, _),
		Defn = Defn0
	;
		DefnBody0 = mlds__class(ClassDefn0),
		ClassDefn0 = class_defn(Kind, Imports, BaseClasses, Implements,
		                CtorDefns0, MemberDefns0),
		MemberDefns = optimize_in_defns(MemberDefns0, Globals,
			ModuleName),
		CtorDefns = optimize_in_defns(CtorDefns0, Globals,
			ModuleName),
		ClassDefn = class_defn(Kind, Imports, BaseClasses, Implements,
		                CtorDefns, MemberDefns),
		DefnBody = mlds__class(ClassDefn),
		Defn = mlds__defn(Name, Context, Flags, DefnBody)
	).

:- func optimize_in_function_body(opt_info, function_body) = function_body.

optimize_in_function_body(_, external) = external.
optimize_in_function_body(OptInfo, defined_here(Statement0)) =
		defined_here(Statement) :-
	Statement = optimize_in_statement(OptInfo, Statement0).

:- func optimize_in_maybe_statement(opt_info, 
		maybe(mlds__statement)) = maybe(mlds__statement).

optimize_in_maybe_statement(_, no) = no.
optimize_in_maybe_statement(OptInfo, yes(Statement0)) = yes(Statement) :-
	Statement = optimize_in_statement(OptInfo, Statement0).

:- func optimize_in_statements(opt_info, list(mlds__statement)) = 
	list(mlds__statement).

optimize_in_statements(OptInfo, Statements) = 
	list__map(optimize_in_statement(OptInfo), Statements).

:- func optimize_in_statement(opt_info, mlds__statement) =
	 mlds__statement.

optimize_in_statement(OptInfo, statement(Stmt, Context)) = 
	statement(optimize_in_stmt(OptInfo ^ context := Context, Stmt),
	Context).

:- func optimize_in_stmt(opt_info, mlds__stmt) = mlds__stmt.

optimize_in_stmt(OptInfo, Stmt0) = Stmt :-
	(
		Stmt0 = call(_, _, _, _, _, _),
		Stmt = optimize_in_call_stmt(OptInfo, Stmt0)
	;
		Stmt0 = block(Defns0, Statements0),
		maybe_convert_assignments_into_initializers(Defns0, Statements0,
			OptInfo, Defns1, Statements1),
		maybe_eliminate_locals(Defns1, Statements1,
			OptInfo, Defns, Statements2),
		Statements = optimize_in_statements(OptInfo, Statements2),
		Stmt = block(Defns, Statements)
	;
		Stmt0 = while(Rval, Statement0, Once),
		Stmt = while(Rval, optimize_in_statement(OptInfo, 
			Statement0), Once)
	;
		Stmt0 = if_then_else(Rval, Then, MaybeElse),
		Stmt = if_then_else(Rval, 
			optimize_in_statement(OptInfo, Then), 
			maybe_apply(optimize_in_statement(OptInfo), MaybeElse))
	;
		Stmt0 = switch(Type, Rval, Range, Cases0, Default0),
		Stmt = switch(Type, Rval, Range,
			list__map(optimize_in_case(OptInfo), Cases0), 
			optimize_in_default(OptInfo, Default0))
	;
		Stmt0 = do_commit(_),
		Stmt = Stmt0
	;
		Stmt0 = return(_),
		Stmt = Stmt0
	;
		Stmt0 = try_commit(Ref, TryGoal, HandlerGoal),
		Stmt = try_commit(Ref, 
			optimize_in_statement(OptInfo, TryGoal), 
			optimize_in_statement(OptInfo, HandlerGoal))
	;
		Stmt0 = label(_Label),
		Stmt = Stmt0
	;
		Stmt0 = goto(_Label),
		Stmt = Stmt0
	;
		Stmt0 = computed_goto(_Rval, _Label),
		Stmt = Stmt0
	;
		Stmt0 = atomic(_Atomic),
		Stmt = Stmt0
	).

:- func optimize_in_case(opt_info, mlds__switch_case) = mlds__switch_case.

optimize_in_case(OptInfo, Conds - Statement0) = Conds - Statement :-
	Statement = optimize_in_statement(OptInfo, Statement0).

:- func optimize_in_default(opt_info, mlds__switch_default) =
	mlds__switch_default.

optimize_in_default(_OptInfo, default_is_unreachable) = default_is_unreachable.
optimize_in_default(_OptInfo, default_do_nothing) = default_do_nothing.
optimize_in_default(OptInfo, default_case(Statement0)) =
		default_case(Statement) :-
	Statement = optimize_in_statement(OptInfo, Statement0).

%-----------------------------------------------------------------------------%

:- inst g == ground.
:- inst call ---> ml_backend__mlds__call(g, g, g, g, g, g).

:- func optimize_in_call_stmt(opt_info, mlds__stmt) = mlds__stmt.
:- mode optimize_in_call_stmt(in, in(call)) = out is det.

optimize_in_call_stmt(OptInfo, Stmt0) = Stmt :-
	Stmt0 = call(_Signature, FuncRval, _MaybeObject, CallArgs,
		_Results, _IsTailCall),
		% If we have a self-tailcall, assign to the arguments and
		% then goto the top of the tailcall loop.
	(
		globals__lookup_bool_option(OptInfo ^ globals,
			optimize_tailcalls, yes),
		can_optimize_tailcall(qual(OptInfo ^ module_name, 
			OptInfo ^ entity_name), Stmt0)
	->
		CommentStatement = statement(
			atomic(comment("direct tailcall eliminated")),
			OptInfo ^ context),
		GotoStatement = statement(
			goto(tailcall_loop_top(OptInfo ^ globals)),
			OptInfo ^ context),
		OptInfo ^ func_params = mlds__func_params(FuncArgs, _RetTypes),
		generate_assign_args(OptInfo, FuncArgs, CallArgs,
			AssignStatements, AssignDefns),
		AssignVarsStatement = statement(block(AssignDefns, 
			AssignStatements), OptInfo ^ context),

		CallReplaceStatements = [
			CommentStatement,
			AssignVarsStatement,
			GotoStatement
			],
		Stmt = block([], CallReplaceStatements)
	;
		% Convert calls to `mark_hp' and `restore_hp' to the
		% corresponding MLDS instructions.  This ensures that
		% they get generated as inline code.  (Without this
		% they won't, since HLDS inlining doesn't get run again
		% after the add_heap_ops pass that adds these calls.)
		% This approach is better than running HLDS inlining
		% again, both because it cheaper in compilation time
		% and because inlining the C code doesn't help with
		% the --target asm back-end, whereas generating the
		% appropriate MLDS instructions does.
		%
		FuncRval = const(code_addr_const(proc(qual(ModName, 
                        pred(predicate, _DefnModName, PredName, _Arity,
				_CodeModel, _NonOutputFunc) - _ProcId),
			_FuncSignature))), 
		(
			PredName = "mark_hp",
			CallArgs = [mem_addr(Lval)],
			AtomicStmt = mark_hp(Lval)
		;
			PredName = "restore_hp",
			CallArgs = [Rval],
			AtomicStmt = restore_hp(Rval)
		),
		mercury_private_builtin_module(PrivateBuiltin),
		ModName = mercury_module_name_to_mlds(PrivateBuiltin)
	->
		Stmt = atomic(AtomicStmt)
	;
		Stmt = Stmt0
	).

	% This specifies how we should branch to the top of the loop
	% introduced by tailcall opptimization.
:- func tailcall_loop_top(globals) = mlds__goto_target.
tailcall_loop_top(Globals) =
	( target_supports_break_and_continue(Globals) ->
		% the function body has been wrapped inside
		% `while (true) { ... break; }', and so to
		% branch to the top of the function, we just do
		% a `continue' which will continue the next iteration
		% of the loop
		continue
	;
		% a label has been inserted at the start of the function,
		% and so to branch to the top of the function, we just
		% branch to that label
		label(tailcall_loop_label_name)
	).

	% The label name we use for the top of the loop introduced by
	% tailcall optimization, when we're doing it with labels & gotos.
:- func tailcall_loop_label_name = string.
tailcall_loop_label_name = "loop_top".

%----------------------------------------------------------------------------

	% Assign the specified list of rvals to the arguments.
	% This is used as part of tail recursion optimization (see above).

:- pred generate_assign_args(opt_info, mlds__arguments, list(mlds__rval),
	list(mlds__statement), list(mlds__defn)).
:- mode generate_assign_args(in, in, in, out, out) is det.

generate_assign_args(_, [_|_], [], [], []) :-
	error("generate_assign_args: length mismatch").
generate_assign_args(_, [], [_|_], [], []) :-
	error("generate_assign_args: length mismatch").
generate_assign_args(_, [], [], [], []).
generate_assign_args(OptInfo, [Arg | Args], [ArgRval | ArgRvals],
		Statements, TempDefns) :-
	Arg = mlds__argument(Name, Type, _ArgGCTraceCode),
	(
		%
		% extract the variable name
		%
		Name = data(var(VarName))
	->
		QualVarName = qual(OptInfo ^ module_name, VarName),
		(
			% 
			% don't bother assigning a variable to itself
			%
			ArgRval = lval(var(QualVarName, _VarType))
		->
			generate_assign_args(OptInfo, Args, ArgRvals, 
				Statements, TempDefns)
		;

			% Declare a temporary variable, initialized it
			% to the arg, recursively process the remaining
			% args, and then assign the temporary to the
			% parameter:
			%
			%	SomeType argN__tmp_copy;
			%	argN__tmp_copy = new_argN_value;
			%	...
			%	argN = argN_tmp_copy;
			%
			% The temporaries are needed for the case where
			% we are e.g. assigning v1, v2 to v2, v1;
			% they ensure that we don't try to reference the old
			% value of a parameter after it has already been
			% clobbered by the new value.
			%
			% Note that we have to use an assignment rather
			% than an initializer to initialize the temp,
			% because this pass comes before ml_elem_nested.m,
			% and ml_elim_nested.m doesn't handle code containing
			% initializers.

			VarName = mlds__var_name(VarNameStr, MaybeNum),
			TempName = mlds__var_name(VarNameStr ++ "__tmp_copy",
				MaybeNum),
			QualTempName = qual(OptInfo ^ module_name, 
				TempName),
			Initializer = no_initializer,
			% We don't need to trace the temporary variables
			% for GC, since they are not live across a call
			% or a heap allocation
			GC_TraceCode = no,
			TempDefn = ml_gen_mlds_var_decl(var(TempName),
				Type, Initializer, GC_TraceCode,
				OptInfo ^ context),
			TempInitStatement = statement(
				atomic(assign(var(QualTempName, Type),
					ArgRval)), 
				OptInfo ^ context),
			AssignStatement = statement(
				atomic(assign(
					var(QualVarName, Type),
					lval(var(QualTempName, Type)))), 
				OptInfo ^ context),
			generate_assign_args(OptInfo, Args, ArgRvals,
				Statements0, TempDefns0),
			Statements = [TempInitStatement | Statements0] ++
				[AssignStatement],
			TempDefns = [TempDefn | TempDefns0]
		)
	;
		error("generate_assign_args: function param is not a var")
	).

%----------------------------------------------------------------------------

:- func optimize_func(opt_info, function_body) = function_body.

optimize_func(_, external) = external.
optimize_func(OptInfo, defined_here(Statement)) =
	defined_here(optimize_func_stmt(OptInfo, Statement)).

:- func optimize_func_stmt(opt_info, mlds__statement) = (mlds__statement).

optimize_func_stmt(OptInfo, mlds__statement(Stmt0, Context)) = 
		mlds__statement(Stmt, Context) :-

		% Tailcall optimization -- if we do a self tailcall, we
		% can turn it into a loop.
	(
		globals__lookup_bool_option(OptInfo ^ globals,
			optimize_tailcalls, yes),
		stmt_contains_statement(Stmt0, Call),
		Call = mlds__statement(CallStmt, _),
		can_optimize_tailcall(
			qual(OptInfo ^ module_name, OptInfo ^ entity_name), 
			CallStmt)
	->
		Comment = atomic(comment("tailcall optimized into a loop")),
		CommentStmt = statement(Comment, Context),
		% The loop can be defined either using while, break, and
		% continue, or using a label and goto.  We prefer to
		% use the former, if possible, since it is a higher-level
		% construct that may help the back-end compiler's optimizer.
		( target_supports_break_and_continue(OptInfo ^ globals) ->
			% Wrap a while loop around the function body:
			%	while (true) {
			%		/* tailcall optimized into a loop */
			%		<function body goes here>
			%		break;
			%	}
			% Any tail calls in the function body will have
			% been replaced with `continue' statements.
			Stmt = while(const(true),
				statement(block([],
					[CommentStmt,
					statement(Stmt0, Context),
					statement(goto(break), Context)]),
				Context), no)
		;
			% Add a loop_top label at the start of the function
			% body:
			%	{
			%	loop_top:
			%		/* tailcall optimized into a loop */
			%		<function body goes here>
			%	}
			% Any tail calls in the function body will have
			% been replaced with `goto loop_top' statements.
			Label = label(tailcall_loop_label_name),
			Stmt = block([], [CommentStmt,
				statement(Label, Context),
				statement(Stmt0, Context)])
		)
	;
		Stmt = Stmt0
	).

:- pred target_supports_break_and_continue(globals::in) is semidet.

target_supports_break_and_continue(Globals) :-
	globals__get_target(Globals, Target),
	target_supports_break_and_continue_2(Target) = yes.

:- func target_supports_break_and_continue_2(compilation_target) = bool.

target_supports_break_and_continue_2(c) = yes.
target_supports_break_and_continue_2(asm) = no. % asm means via gnu back-end
target_supports_break_and_continue_2(il) = no.
target_supports_break_and_continue_2(java) = yes.
% target_supports_break_and_continue_2(c_sharp) = yes.

%-----------------------------------------------------------------------------%

%
% This code implements the --optimize-initializations option.
% It converts MLDS code using assignments, e.g.
%
%	{
%		int v1;	 // or any other type -- it doesn't have to be int
%		int v2;
%		int v3;
%		int v4;
%		int v5;
%
%		v1 = 1;
%		v2 = 2;
%		v3 = 3;
%		foo();
%		v4 = 4;
%		...
%	}
%
% into code that instead uses initializers, e.g.
%
%	{
%		int v1 = 1;
%		int v2 = 2;
%		int v3 = 3;
%		int v4;
%	
%		foo();
%		v4 = 4;
%		...
%	}
%
% Note that if there are multiple initializations of the same
% variable, then we'll apply the optimization successively,
% replacing the existing initializers as we go, and keeping
% only the last, e.g.
%
%		int v = 1;
%		v = 2;
%		v = 3;
%		...
%
% will get replaced with
%
%		int v = 3;
%		...
%
% We need to watch out for some tricky cases that can't be safely optimized.
% If the RHS of the assignment refers to a variable which was declared after
% the variable whose initialization we're optimizing, e.g.
%
%		int v1 = 1;
%		int v2 = 0;
%		v1 = v2 + 1;	// RHS refers to variable declared after v1
%
% then we can't do the optimization because it would cause a forward reference
%
%		int v1 = v2 + 1;	// error -- v2 not declared yet!
%		int v2 = 0;
%
% Likewise if the RHS refers to the variable itself
%
%		int v1 = 1;
%		v1 = v1 + 1;
%
% then we can't optimize it, because that would be bogus:
%
%		int v1 = v1 + 1;	// error -- v1 not initialized yet!
%
% Similarly, if the initializers of the variables that follow
% the one we're trying to optimize refer to it, e.g.
%
%		int v1 = 1;
%		int v2 = v1 + 1;	// here v2 == 2
%		v1 = 0;
%		...
%
% then we can't eliminate the assignment, because that would produce
% different results:
%
%		int v1 = 0;
%		int v2 = v1 + 1;	// wrong -- v2 == 1
%		...

:- pred maybe_convert_assignments_into_initializers(
		mlds__defns, mlds__statements, opt_info,
		mlds__defns, mlds__statements).
:- mode maybe_convert_assignments_into_initializers(in, in, in, out, out)
		is det.

maybe_convert_assignments_into_initializers(Defns0, Statements0, OptInfo,
		Defns, Statements) :-
	(
		% Check if --optimize-initializations is enabled
		globals__lookup_bool_option(OptInfo ^ globals,
			optimize_initializations, yes)
	->
		convert_assignments_into_initializers(Defns0, Statements0,
			OptInfo, Defns, Statements)
	;
		Defns = Defns0,
		Statements = Statements0
	).

:- pred convert_assignments_into_initializers(mlds__defns, mlds__statements,
		opt_info, mlds__defns, mlds__statements).
:- mode convert_assignments_into_initializers(in, in, in, out, out) is det.

convert_assignments_into_initializers(Defns0, Statements0, OptInfo,
		Defns, Statements) :-
	(
		% Check if the first statement in the block is
		% an assignment to one of the variables declared in
		% the block.
		Statements0 = [AssignStatement | Statements1],
		AssignStatement = statement(atomic(assign(LHS, RHS)), _),
		LHS = var(ThisVar, _ThisType),
		ThisVar = qual(Qualifier, VarName),
		ThisData = qual(Qualifier, var(VarName)),
		Qualifier = OptInfo ^ module_name,
		list__takewhile(isnt(var_defn(VarName)), Defns0, 
			_PrecedingDefns, [_VarDefn | FollowingDefns]),

		% We must check that the value being assigned
		% doesn't refer to the variable itself, or to any
		% of the variables which are declared after this one.
		% We must also check that the initializers (if any)
		% of the variables that follow this one don't
		% refer to this variable.
		\+ rval_contains_var(RHS, ThisData),
		\+ (
			list__member(OtherDefn, FollowingDefns),
			OtherDefn = mlds__defn(data(OtherVarName),
				_, _, data(_Type, OtherInitializer, _GC)),
			( rval_contains_var(RHS, qual(Qualifier, OtherVarName))
			; initializer_contains_var(OtherInitializer, ThisData)
			)
		)
	->
		% Replace the assignment statement with an initializer
		% on the variable declaration.
		set_initializer(Defns0, VarName, RHS, Defns1),

		% Now try to apply the same optimization again
		convert_assignments_into_initializers(Defns1, Statements1,
			OptInfo, Defns, Statements)
	;
		% No optimization possible -- leave the block unchanged.
		Defns = Defns0,
		Statements = Statements0
	).

:- pred var_defn(var_name::in, mlds__defn::in) is semidet.
var_defn(VarName, Defn) :-
	Defn = mlds__defn(data(var(VarName)), _, _, _).

	% set_initializer(Defns0, VarName, Rval, Defns):
	%	Finds the first definition of the specified variable
	%	in Defns0, and replaces the initializer of that
	%	definition with init_obj(Rval).
	%
:- pred set_initializer(mlds__defns, mlds__var_name, mlds__rval, mlds__defns).
:- mode set_initializer(in, in, in, out) is det.

set_initializer([], _, _, _) :-
	unexpected(this_file, "set_initializer: var not found!").
set_initializer([Defn0 | Defns0], VarName, Rval, [Defn | Defns]) :-
	Defn0 = mlds__defn(Name, Context, Flags, DefnBody0),
	(
		Name = data(var(VarName)), 
		DefnBody0 = mlds__data(Type, _OldInitializer, GC_TraceCode)
	->
		DefnBody = mlds__data(Type, init_obj(Rval), GC_TraceCode),
		Defn = mlds__defn(Name, Context, Flags, DefnBody),
		Defns = Defns0
	;
		Defn = Defn0,
		set_initializer(Defns0, VarName, Rval, Defns)
	).

%-----------------------------------------------------------------------------%

%
% This is a pass to eliminate initialized local variable definitions,
% by substituting the value of the initializer for occurrences
% of the variable.
%

% XXX This is quadratic in the number of variable definitions,
% since we done one pass over the block per variable definition.
% A more efficient algorithm would be to do one pass to figure
% out which variables could be eliminated, and then do another
% pass to actually eliminate them.

:- pred maybe_eliminate_locals(mlds__defns, mlds__statements,
		opt_info, mlds__defns, mlds__statements).
:- mode maybe_eliminate_locals(in, in, in, out, out) is det.

maybe_eliminate_locals(Defns0, Statements0, OptInfo, Defns, Statements) :-
	globals__lookup_bool_option(OptInfo ^ globals, eliminate_local_vars,
		EliminateLocalVars),
	( EliminateLocalVars = yes ->
		eliminate_locals(Defns0, Statements0, OptInfo,
			Defns, Statements)
	;
		Defns = Defns0,
		Statements = Statements0
	).

:- pred eliminate_locals(mlds__defns, mlds__statements,
		opt_info, mlds__defns, mlds__statements).
:- mode eliminate_locals(in, in, in, out, out) is det.

eliminate_locals([], Statements, _OptInfo, [], Statements).
eliminate_locals([Defn0 | Defns0], Statements0, OptInfo, Defns, Statements) :-
	(
		try_to_eliminate_defn(Defn0, Defns0, Statements0, OptInfo,
			Defns1, Statements1)
	->
		eliminate_locals(Defns1, Statements1, OptInfo,
			Defns, Statements)
	;
		eliminate_locals(Defns0, Statements0, OptInfo,
			Defns2, Statements),
		Defns = [Defn0 | Defns2]
	).

	% This data structure holds information that we use
	% in this pass to eliminate initialized local variable definitions.
:- type var_elim_info
	--->	var_elim_info(
			%
			% these fields remain constant
			%
			var_name	:: mlds__var,
				% the name of the variable to eliminate
			var_value	:: mlds__rval,
				% the value to replace the
				% eliminated variable with
			%
			% these get updated as we go along
			%
			replace_count	:: int,
				% the number of occurrences of the variable
			invalidated	:: bool
				% `yes' if the optimization can't be applied,
				% e.g. because the variable was assigned to,
				% or because its address was taken
		).

	% Check if this definition is a variable that we can eliminate.
	% If so, replace uses of this variable with the variable's value.
	% This will fail if the definition is not a variable definition,
	% or if any of the statements or definitions take the address
	% of the variable, or assign to it.
:- pred try_to_eliminate_defn(mlds__defn::in, mlds__defns::in,
		mlds__statements::in, opt_info::in,
		mlds__defns::out, mlds__statements::out) is semidet.

try_to_eliminate_defn(Defn0, Defns0, Statements0, OptInfo,
		Defns, Statements) :-
	Defn0 = mlds__defn(Name, _Context, Flags, DefnBody),

	% Check if this definition is a local variable definition...
	Name = data(var(VarName)),
	Flags = ml_gen_local_var_decl_flags,
	DefnBody = mlds__data(_Type, Initializer, _MaybeGCTraceCode),

	% ... with a known initial value.
	QualVarName = qual(OptInfo ^ module_name, VarName),
	(
		Initializer = init_obj(Rval),
		Statements1 = Statements0
	;
		Initializer = no_initializer,
		find_initial_val_in_statements(QualVarName, Statements0,
			Rval, Statements1)
	),

	% It's only safe to do this transformation if the
	% variable's value is constant, otherwise we might
	% end up moving the rvalue across a statement which
	% modifies it.
	rval_will_not_change(Rval),

	% This transformation moves evaluation of the rvalue later in
	% the computation.  If the rvalue is something which might
	% loop, throw an exception, or abort (e.g. for division by zero),
	% then this might change the behaviour of the program.
	% In such cases, we can only do the transformation
	% if reordering of both conjunctions and disjunctions
	% (we can't tell here whether this MLDS code came from a
	% conjunction or a disjunction) is allowed.
	(
		rval_cannot_throw(Rval)
	;
		globals__lookup_bool_option(OptInfo ^ globals,
			reorder_conj, yes),
		globals__lookup_bool_option(OptInfo ^ globals,
			reorder_disj, yes)
	),

	% Replace uses of this variable with the variable's value,
	% checking that none of the statements or definitions took the
	% address of the variable, or assigned to it.
	eliminate_var(QualVarName, Rval, Defns0, Defns,
		Statements1, Statements, Count, Invalidated),
	Invalidated = no,

	% Make sure that we didn't duplicate the rval,
	% unless it is just a constant or a variable,
	% because duplicating any real operation would be
	% a pessimization
	( Count =< 1
	; rval_is_cheap_enough_to_duplicate(Rval)
	).

:- pred rval_is_cheap_enough_to_duplicate(mlds__rval::in) is semidet.
rval_is_cheap_enough_to_duplicate(Rval) :-
	( Rval = const(_)
	; Rval = lval(var(_, _))
	; Rval = mem_addr(_)
	; Rval = self(_)
	).

	% Succeed only if the specified rval definitely won't change
	% in value.
:- pred rval_will_not_change(mlds__rval::in) is semidet.
rval_will_not_change(const(_)).
rval_will_not_change(mkword(_Tag, Rval)) :-
	rval_will_not_change(Rval).
rval_will_not_change(unop(_Op, Rval)) :-
	rval_will_not_change(Rval).
rval_will_not_change(binop(_Op, Rval1, Rval2)) :-
	rval_will_not_change(Rval1),
	rval_will_not_change(Rval2).
rval_will_not_change(mem_addr(var(_, _))).
rval_will_not_change(mem_addr(mem_ref(Address, _Type))) :-
	rval_will_not_change(Address).
rval_will_not_change(mem_addr(field(_, Address, _, _, _))) :-
	rval_will_not_change(Address).

	% Succeed only if the given rval definitely can't loop,
	% throw an exception, or abort.
	% We use a pretty conservative approximation...
:- pred rval_cannot_throw(mlds__rval::in) is semidet.
rval_cannot_throw(const(_)).
rval_cannot_throw(mkword(_Tag, Rval)) :-
	rval_cannot_throw(Rval).
rval_cannot_throw(mem_addr(_)).
rval_cannot_throw(self(_)).

% Search through a list of statements,
% trying to find the first assignment to the specified variable.
% Return the initial value, and a modified list of statements
% with the initial assignment deleted.
% Fail if the first value can't be determined.
:- pred find_initial_val_in_statements(mlds__var::in, mlds__statements::in,
		mlds__rval::out, mlds__statements::out) is semidet.
find_initial_val_in_statements(VarName, [Statement0 | Statements0],
		Rval, Statements) :-
	(
		find_initial_val_in_statement(VarName, Statement0,
			Rval1, Statement1)
	->
		Rval = Rval1,
		( Statement1 = mlds__statement(block([], []), _) ->
			Statements = Statements0
		;
			Statements = [Statement1 | Statements0]
		)
	;
		% Check that Statement0 doesn't modify the value of the
		% variable -- this includes checking that there are no
		% labels via which code could branch into the middle of
		% Statement0.  Only if we are sure that Statement0
		% can't modify the variable's value is it safe to go
		% on and look for the initial value in Statements0.
		VarName = qual(Mod, UnqualVarName),
		DataName = qual(Mod, var(UnqualVarName)),
		\+ statement_contains_var(Statement0, DataName),
		\+ (
			statement_contains_statement(Statement0, Label),
			Label = mlds__statement(label(_), _)
		),
		find_initial_val_in_statements(VarName, Statements0,
			Rval, Statements1),
		Statements = [Statement0 | Statements1]
	).

:- pred find_initial_val_in_statement(mlds__var::in, mlds__statement::in,
		mlds__rval::out, mlds__statement::out) is semidet.

find_initial_val_in_statement(Var, Statement0, Rval, Statement) :-
	Statement0 = mlds__statement(Stmt0, Context),
	Statement = mlds__statement(Stmt, Context),
	( Stmt0 = atomic(assign(var(Var, _Type), Rval0)) ->
		Rval = Rval0,
		% delete the assignment, by replacing it with an empty block
		Stmt = block([], [])
	; Stmt0 = block(Defns0, SubStatements0) ->
		Var = qual(Mod, UnqualVarName),
		Data = qual(Mod, var(UnqualVarName)),
		\+ defns_contains_var(Defns0, Data),
		find_initial_val_in_statements(Var, SubStatements0,
			Rval, SubStatements),
		Stmt = block(Defns0, SubStatements)
	;
		fail
	).

	% Replace uses of this variable with the variable's value
	% in the specified definitions and statements.
	% This will return a count of how many occurrences
	% of the variable there were.
	% It will also return Invalidated = yes if 
	% any of the statements or definitions take the address
	% of the variable, or assign to it; in that case,
	% the transformation should not be performed.
:- pred eliminate_var(mlds__var::in, mlds__rval::in,
		mlds__defns::in, mlds__defns::out,
		mlds__statements::in, mlds__statements::out,
		int::out, bool::out) is det.

eliminate_var(QualVarName, VarRval, Defns0, Defns1,
		Statements0, Statements, Count, Invalidated) :-
	Count0 = 0,
	Invalidated0 = no,
	VarElimInfo0 = var_elim_info(QualVarName, VarRval, Count0,
		Invalidated0),
	eliminate_var_in_block(Defns0, Defns1,
		Statements0, Statements, VarElimInfo0, VarElimInfo),
	Count = VarElimInfo ^ replace_count,
	Invalidated = VarElimInfo ^ invalidated.

% eliminate_var_in_*:
%	process the specified construct, replacing all rvalue occurences
%	of the variable (^var_name) with its value (^var_value),
%	incrementing the ^replace_count field for each occurrence
%	as an rvalue, and setting ^invalidated to yes if the variable
%	occurs as an lvalue.

:- pred eliminate_var_in_block(mlds__defns::in, mlds__defns::out,
		mlds__statements::in, mlds__statements::out,
		var_elim_info::in, var_elim_info::out) is det.

eliminate_var_in_block(Defns0, Defns, Statements0, Statements) -->
	eliminate_var_in_defns(Defns0, Defns),
	eliminate_var_in_statements(Statements0, Statements).

:- pred eliminate_var_in_defns(mlds__defns::in, mlds__defns::out,
		var_elim_info::in, var_elim_info::out) is det.
	
eliminate_var_in_defns(Defns0, Defns) -->
	list__map_foldl(eliminate_var_in_defn, Defns0, Defns).

:- pred eliminate_var_in_defn(mlds__defn::in, mlds__defn::out,
		var_elim_info::in, var_elim_info::out) is det.
	
eliminate_var_in_defn(Defn0, Defn) -->
	{ Defn0 = mlds__defn(Name, Context, Flags, DefnBody0) },
	(
		{ DefnBody0 = mlds__data(Type, Initializer0,
			MaybeGCTraceCode) },
		eliminate_var_in_initializer(Initializer0, Initializer),
		{ DefnBody = mlds__data(Type, Initializer, MaybeGCTraceCode) }
	;
		{ DefnBody0 = mlds__class(_) },
		% We assume that nested classes don't refer to local variables
		% in the containing scope
		{ DefnBody = DefnBody0 }
	;
		{ DefnBody0 = mlds__function(Id, Params, Body0, Attributes) },
		eliminate_var_in_function_body(Body0, Body),
		{ DefnBody = mlds__function(Id, Params, Body, Attributes) }
	),
	{ Defn = mlds__defn(Name, Context, Flags, DefnBody) }.

:- pred eliminate_var_in_function_body(
		mlds__function_body::in, mlds__function_body::out,
		var_elim_info::in, var_elim_info::out) is det.

eliminate_var_in_function_body(external, external) --> [].
eliminate_var_in_function_body(defined_here(Stmt0), defined_here(Stmt)) -->
	eliminate_var_in_statement(Stmt0, Stmt).

:- pred eliminate_var_in_initializer(
		mlds__initializer::in, mlds__initializer::out,
		var_elim_info::in, var_elim_info::out) is det.

eliminate_var_in_initializer(no_initializer, no_initializer) --> [].
eliminate_var_in_initializer(init_obj(Rval0), init_obj(Rval)) -->
	eliminate_var_in_rval(Rval0, Rval).
eliminate_var_in_initializer(init_array(Elements0), init_array(Elements)) -->
	list__map_foldl(eliminate_var_in_initializer, Elements0, Elements).
eliminate_var_in_initializer(init_struct(Members0), init_struct(Members)) -->
	list__map_foldl(eliminate_var_in_initializer, Members0, Members).

:- pred eliminate_var_in_rvals(
		list(mlds__rval)::in, list(mlds__rval)::out,
		var_elim_info::in, var_elim_info::out) is det.

eliminate_var_in_rvals(Rvals0, Rvals) -->
	list__map_foldl(eliminate_var_in_rval, Rvals0, Rvals).

:- pred eliminate_var_in_maybe_rval(
		maybe(mlds__rval)::in, maybe(mlds__rval)::out,
		var_elim_info::in, var_elim_info::out) is det.

eliminate_var_in_maybe_rval(no, no) --> [].
eliminate_var_in_maybe_rval(yes(Rval0), yes(Rval)) -->
	eliminate_var_in_rval(Rval0, Rval).

:- pred eliminate_var_in_rval(mlds__rval::in, mlds__rval::out,
		var_elim_info::in, var_elim_info::out) is det.
	
eliminate_var_in_rval(Rval0, Rval) -->
	(
		{ Rval0 = lval(Lval0) },
		VarName =^ var_name,
		( { Lval0 = var(VarName, _) } ->
			% we found an rvalue occurrence of the variable --
			% replace it with the rval for the variable's value,
			% and increment the counter for the number of
			% occurrences that we have replaced.
			Rval =^ var_value,
			Count0 =^ replace_count,
			^replace_count := Count0 + 1
		;
			eliminate_var_in_lval(Lval0, Lval),
			{ Rval = lval(Lval) }
		)
	;
		{ Rval0 = mkword(Tag, ArgRval0) },
		eliminate_var_in_rval(ArgRval0, ArgRval),
		{ Rval = mkword(Tag, ArgRval) }
	;
		{ Rval0 = const(_) },
		{ Rval = Rval0 }
	;
		{ Rval0 = unop(Op, ArgRval0) },
		eliminate_var_in_rval(ArgRval0, ArgRval),
		{ Rval = unop(Op, ArgRval) }
	;
		{ Rval0 = binop(Op, Arg1Rval0, Arg2Rval0) },
		eliminate_var_in_rval(Arg1Rval0, Arg1Rval),
		eliminate_var_in_rval(Arg2Rval0, Arg2Rval),
		{ Rval = binop(Op, Arg1Rval, Arg2Rval) }
	;
		{ Rval0 = mem_addr(Lval0) },
		eliminate_var_in_lval(Lval0, Lval),
		{ Rval = mem_addr(Lval) }
	;
		{ Rval0 = self(_Type) },
		{ Rval = Rval0 }
	).

:- pred eliminate_var_in_lvals(
		list(mlds__lval)::in, list(mlds__lval)::out,
		var_elim_info::in, var_elim_info::out) is det.

eliminate_var_in_lvals(Lvals0, Lvals) -->
	list__map_foldl(eliminate_var_in_lval, Lvals0, Lvals).

:- pred eliminate_var_in_lval(mlds__lval::in, mlds__lval::out,
		var_elim_info::in, var_elim_info::out) is det.
	
eliminate_var_in_lval(Lval0, Lval) -->
	(
		{ Lval0 = field(MaybeTag, Rval0, FieldId, FieldType, PtrType) },
		eliminate_var_in_rval(Rval0, Rval),
		{ Lval = field(MaybeTag, Rval, FieldId, FieldType, PtrType) }
	;
		{ Lval0 = mem_ref(Rval0, Type) },
		eliminate_var_in_rval(Rval0, Rval),
		{ Lval = mem_ref(Rval, Type) }
	;
		{ Lval0 = var(VarName, _Type) },
		( VarName =^ var_name ->
			% we found an lvalue occurrence of the variable --
			% if the variable that we are trying to eliminate
			% has its address is taken, or is assigned to,
			% or in general if it is used as an lvalue,
			% then it's not safe to eliminate it
			^invalidated := yes
		;
			[]
		),
		{ Lval = Lval0 }
	).

:- pred eliminate_var_in_statements(
		mlds__statements::in, mlds__statements::out,
		var_elim_info::in, var_elim_info::out) is det.
	
eliminate_var_in_statements(Statements0, Statements) -->
	list__map_foldl(eliminate_var_in_statement, Statements0, Statements).

:- pred eliminate_var_in_maybe_statement(
		maybe(mlds__statement)::in, maybe(mlds__statement)::out,
		var_elim_info::in, var_elim_info::out) is det.

eliminate_var_in_maybe_statement(no, no) --> [].
eliminate_var_in_maybe_statement(yes(Statement0), yes(Statement)) -->
	eliminate_var_in_statement(Statement0, Statement).

:- pred eliminate_var_in_statement(mlds__statement::in, mlds__statement::out,
		var_elim_info::in, var_elim_info::out) is det.

eliminate_var_in_statement(Statement0, Statement) -->
	{ Statement0 = mlds__statement(Stmt0, Context) },
	eliminate_var_in_stmt(Stmt0, Stmt),
	{ Statement = mlds__statement(Stmt, Context) }.

:- pred eliminate_var_in_stmt(mlds__stmt::in, mlds__stmt::out,
		var_elim_info::in, var_elim_info::out) is det.

eliminate_var_in_stmt(Stmt0, Stmt) -->
	(
		{ Stmt0 = block(Defns0, Statements0) },
		eliminate_var_in_block(Defns0, Defns, Statements0, Statements),
		{ Stmt = block(Defns, Statements) }
	;
		{ Stmt0 = while(Rval0, Statement0, Once) },
		eliminate_var_in_rval(Rval0, Rval),
		eliminate_var_in_statement(Statement0, Statement),
		{ Stmt = while(Rval, Statement, Once) }
	;
		{ Stmt0 = if_then_else(Cond0, Then0, MaybeElse0) },
		eliminate_var_in_rval(Cond0, Cond),
		eliminate_var_in_statement(Then0, Then),
		eliminate_var_in_maybe_statement(MaybeElse0, MaybeElse),
		{ Stmt = if_then_else(Cond, Then, MaybeElse) }
	;
		{ Stmt0 = switch(Type, Val0, Range, Cases0, Default0) },
		eliminate_var_in_rval(Val0, Val),
		list__map_foldl(eliminate_var_in_case, Cases0, Cases),
		eliminate_var_in_default(Default0, Default),
		{ Stmt = switch(Type, Val, Range, Cases, Default) }
	;
		{ Stmt0 = label(_) },
		{ Stmt = Stmt0 }
	;
		{ Stmt0 = goto(_) },
		{ Stmt = Stmt0 }
	;
		{ Stmt0 = computed_goto(Rval0, Labels) },
		eliminate_var_in_rval(Rval0, Rval),
		{ Stmt = computed_goto(Rval, Labels) }
	;
		{ Stmt0 = call(Sig, Func0, Obj0, Args0, RetLvals0, TailCall) },
		eliminate_var_in_rval(Func0, Func),
		eliminate_var_in_maybe_rval(Obj0, Obj),
		eliminate_var_in_rvals(Args0, Args),
		eliminate_var_in_lvals(RetLvals0, RetLvals),
		{ Stmt = call(Sig, Func, Obj, Args, RetLvals, TailCall) }
	;
		{ Stmt0 = return(Rvals0) },
		eliminate_var_in_rvals(Rvals0, Rvals),
		{ Stmt = return(Rvals) }
	;
		{ Stmt0 = do_commit(Ref0) },
		eliminate_var_in_rval(Ref0, Ref),
		{ Stmt = do_commit(Ref) }
	;
		{ Stmt0 = try_commit(Ref0, Statement0, Handler0) },
		eliminate_var_in_lval(Ref0, Ref),
		eliminate_var_in_statement(Statement0, Statement),
		eliminate_var_in_statement(Handler0, Handler),
		{ Stmt = try_commit(Ref, Statement, Handler) }
	;
		{ Stmt0 = atomic(AtomicStmt0) },
		eliminate_var_in_atomic_stmt(AtomicStmt0, AtomicStmt),
		{ Stmt = atomic(AtomicStmt) }
	).

:- pred eliminate_var_in_case(mlds__switch_case::in, mlds__switch_case::out,
		var_elim_info::in, var_elim_info::out) is det.

eliminate_var_in_case(Conds0 - Statement0, Conds - Statement) -->
	list__map_foldl(eliminate_var_in_case_cond, Conds0, Conds),
	eliminate_var_in_statement(Statement0, Statement).

:- pred eliminate_var_in_default(
		mlds__switch_default::in, mlds__switch_default::out,
		var_elim_info::in, var_elim_info::out) is det.

eliminate_var_in_default(default_is_unreachable, default_is_unreachable) --> [].
eliminate_var_in_default(default_do_nothing, default_do_nothing) --> [].
eliminate_var_in_default(default_case(Statement0), default_case(Statement)) -->
	eliminate_var_in_statement(Statement0, Statement).
	
:- pred eliminate_var_in_atomic_stmt(
		mlds__atomic_statement::in, mlds__atomic_statement::out,
		var_elim_info::in, var_elim_info::out) is det.

eliminate_var_in_atomic_stmt(comment(C), comment(C)) --> [].
eliminate_var_in_atomic_stmt(assign(Lval0, Rval0), assign(Lval, Rval)) -->
	eliminate_var_in_lval(Lval0, Lval),
	eliminate_var_in_rval(Rval0, Rval).
eliminate_var_in_atomic_stmt(delete_object(Lval0), delete_object(Lval)) -->
	eliminate_var_in_lval(Lval0, Lval).
eliminate_var_in_atomic_stmt(new_object(Target0, MaybeTag, HasSecTag, Type,
			MaybeSize, MaybeCtorName, Args0, ArgTypes),
		new_object(Target, MaybeTag, HasSecTag, Type,
			MaybeSize, MaybeCtorName, Args, ArgTypes)) -->
	eliminate_var_in_lval(Target0, Target),
	eliminate_var_in_rvals(Args0, Args).
eliminate_var_in_atomic_stmt(gc_check, gc_check) --> [].
eliminate_var_in_atomic_stmt(mark_hp(Lval0), mark_hp(Lval)) -->
	eliminate_var_in_lval(Lval0, Lval).
eliminate_var_in_atomic_stmt(restore_hp(Rval0), restore_hp(Rval)) -->
	eliminate_var_in_rval(Rval0, Rval).
eliminate_var_in_atomic_stmt(trail_op(TrailOp0), trail_op(TrailOp)) -->
	eliminate_var_in_trail_op(TrailOp0, TrailOp).
eliminate_var_in_atomic_stmt(inline_target_code(Lang, Components0),
		inline_target_code(Lang, Components)) -->
	list__map_foldl(eliminate_var_in_target_code_component,
		Components0, Components).
eliminate_var_in_atomic_stmt(outline_foreign_proc(Lang, Vs, Lvals0, Code),
		outline_foreign_proc(Lang, Vs, Lvals, Code)) -->
	eliminate_var_in_lvals(Lvals0, Lvals).

:- pred eliminate_var_in_case_cond(
		mlds__case_match_cond::in, mlds__case_match_cond::out,
		var_elim_info::in, var_elim_info::out) is det.

eliminate_var_in_case_cond(match_value(Rval0), match_value(Rval)) -->
	eliminate_var_in_rval(Rval0, Rval).
eliminate_var_in_case_cond(match_range(Low0, High0), match_range(Low, High)) -->
	eliminate_var_in_rval(Low0, Low),
	eliminate_var_in_rval(High0, High).

:- pred eliminate_var_in_target_code_component(
		target_code_component::in, target_code_component::out,
		var_elim_info::in, var_elim_info::out) is det.

eliminate_var_in_target_code_component(raw_target_code(Code, Attrs),
		raw_target_code(Code, Attrs)) --> [].
eliminate_var_in_target_code_component(user_target_code(Code, Context, Attrs),
		user_target_code(Code, Context, Attrs)) --> [].
eliminate_var_in_target_code_component(target_code_input(Rval0),
		target_code_input(Rval)) -->
	eliminate_var_in_rval(Rval0, Rval).
eliminate_var_in_target_code_component(target_code_output(Lval0),
		target_code_output(Lval)) -->
	eliminate_var_in_lval(Lval0, Lval).
eliminate_var_in_target_code_component(name(Name), name(Name)) --> [].

:- pred eliminate_var_in_trail_op(trail_op::in, trail_op::out,
		var_elim_info::in, var_elim_info::out) is det.

eliminate_var_in_trail_op(store_ticket(Lval0), store_ticket(Lval)) -->
	eliminate_var_in_lval(Lval0, Lval).
eliminate_var_in_trail_op(reset_ticket(Rval0, Reason),
		reset_ticket(Rval, Reason)) -->
	eliminate_var_in_rval(Rval0, Rval).
eliminate_var_in_trail_op(discard_ticket, discard_ticket) --> [].
eliminate_var_in_trail_op(prune_ticket, prune_ticket) --> [].
eliminate_var_in_trail_op(mark_ticket_stack(Lval0), mark_ticket_stack(Lval)) -->
	eliminate_var_in_lval(Lval0, Lval).
eliminate_var_in_trail_op(prune_tickets_to(Rval0), prune_tickets_to(Rval)) -->
	eliminate_var_in_rval(Rval0, Rval).

%-----------------------------------------------------------------------------%

        % Maps T into V, inside a maybe .  
:- func maybe_apply(func(T) = V, maybe(T)) = maybe(V).

maybe_apply(_, no) = no.
maybe_apply(F, yes(T)) = yes(F(T)).

%-----------------------------------------------------------------------------%

:- func this_file = string.
this_file = "ml_optimize.m".

:- end_module ml_optimize.

%-----------------------------------------------------------------------------%
