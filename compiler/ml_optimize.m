%-----------------------------------------------------------------------------%
% Copyright (C) 2000-2001 The University of Melbourne.
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

:- module ml_optimize.
:- interface.

:- import_module mlds, io.

:- pred optimize(mlds, mlds, io__state, io__state).
:- mode optimize(in, out, di, uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module ml_util, ml_code_util.
:- import_module builtin_ops, globals, options, error_util.

:- import_module bool, list, require, std_util, string.

:- type opt_info --->
	opt_info(
		globals		:: globals,
		module_name 	:: mlds_module_name,
		entity_name 	:: mlds__entity_name,
		func_params 	:: mlds__func_params,
		context 	:: mlds__context
	).

	% The label name we use for the top of the loop introduced by
	% tailcall optimization.
:- func tailcall_loop_label_name = string.
tailcall_loop_label_name = "loop_top".

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
		DefnBody0 = mlds__data(_, _),
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
		convert_assignments_into_initializers(Defns0, Statements0,
			OptInfo, Defns, Statements1),
		Statements = optimize_in_statements(OptInfo, Statements1),
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

:- func optimize_in_call_stmt(opt_info, mlds__stmt) = mlds__stmt.

optimize_in_call_stmt(OptInfo, Stmt0) = Stmt :-
		% If we have a self-tailcall, assign to the arguments and
		% then goto the top of the tailcall loop.
	(
		Stmt0 = call(_Signature, _FuncRval, _MaybeObject, CallArgs,
			_Results, _IsTailCall),
		can_optimize_tailcall(qual(OptInfo ^ module_name, 
			OptInfo ^ entity_name), Stmt0)
	->
		CommentStatement = statement(
			atomic(comment("direct tailcall eliminated")),
			OptInfo ^ context),
		GotoStatement = statement(goto(tailcall_loop_label_name),
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
		Stmt = Stmt0
	).

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
generate_assign_args(OptInfo, 
	[Name - Type | Rest], [Arg | Args], Statements, TempDefns) :-
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
			Arg = lval(var(QualVarName, _VarType))
		->
			generate_assign_args(OptInfo, Rest, Args, 
				Statements, TempDefns)
		;

			% Declare a temporary variable, initialized it
			% to the arg, recursively process the remaining
			% args, and then assign the temporary to the
			% parameter:
			%
			%	SomeType argN__tmp_copy = new_argN_value;
			%	...
			%	new_argN_value = argN_tmp_copy;
			%
			% The temporaries are needed for the case where
			% we are e.g. assigning v1, v2 to v2, v1;
			% they ensure that we don't try to reference the old
			% value of a parameter after it has already been
			% clobbered by the new value.

			VarName = mlds__var_name(VarNameStr, MaybeNum),
			TempName = mlds__var_name(VarNameStr ++ "__tmp_copy",
				MaybeNum),
			QualTempName = qual(OptInfo ^ module_name, 
				TempName),
			Initializer = init_obj(Arg),
			TempDefn = ml_gen_mlds_var_decl(var(TempName),
				Type, Initializer, OptInfo ^ context),

			Statement = statement(
				atomic(assign(
					var(QualVarName, Type),
					lval(var(QualTempName, Type)))), 
				OptInfo ^ context),
			generate_assign_args(OptInfo, Rest, Args, Statements0,
				TempDefns0),
			Statements = [Statement | Statements0],
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
		stmt_contains_statement(Stmt0, Call),
		Call = mlds__statement(CallStmt, _),
		can_optimize_tailcall(
			qual(OptInfo ^ module_name, OptInfo ^ entity_name), 
			CallStmt)
	->
		Comment = atomic(comment("tailcall optimized into a loop")),
		Label = label(tailcall_loop_label_name),
		Stmt = block([], [statement(Comment, Context),
			statement(Label, Context),
			statement(Stmt0, Context)])
	;
		Stmt = Stmt0
	).

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

:- pred convert_assignments_into_initializers(mlds__defns, mlds__statements,
		opt_info, mlds__defns, mlds__statements).
:- mode convert_assignments_into_initializers(in, in, in, out, out) is det.

convert_assignments_into_initializers(Defns0, Statements0, OptInfo,
		Defns, Statements) :-
	(
		% Check if --optimize-initializations is enabled
		globals__lookup_bool_option(OptInfo ^ globals,
			optimize_initializations, yes),

		% Check if the first statement in the block is
		% an assignment to one of the variables declared in
		% the block.
		Statements0 = [AssignStatement | Statements1],
		AssignStatement = statement(atomic(assign(LHS, RHS)), _),
		LHS = var(ThisVar, _ThisType),
		ThisVar = qual(Qualifier, VarName),
		Qualifier = OptInfo ^ module_name,
		list__takewhile(isnt(var_defn(VarName)), Defns0, 
			_PrecedingDefns, [_VarDefn | FollowingDefns]),

		% We must check that the value being assigned
		% doesn't refer to the variable itself, or to any
		% of the variables which are declared after this one.
		% We must also check that the initializers (if any)
		% of the variables that follow this one don't
		% refer to this variable.
		\+ rval_contains_var(RHS, ThisVar),
		\+ (
			list__member(OtherDefn, FollowingDefns),
			OtherDefn = mlds__defn(data(var(OtherVarName)),
				_, _, data(_Type, OtherInitializer)),
			( rval_contains_var(RHS, qual(Qualifier, OtherVarName))
			; initializer_contains_var(OtherInitializer, ThisVar)
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
		DefnBody0 = mlds__data(Type, _OldInitializer)
	->
		DefnBody = mlds__data(Type, init_obj(Rval)),
		Defn = mlds__defn(Name, Context, Flags, DefnBody),
		Defns = Defns0
	;
		Defn = Defn0,
		set_initializer(Defns0, VarName, Rval, Defns)
	).

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
