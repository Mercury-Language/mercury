%-----------------------------------------------------------------------------%
% Copyright (C) 1999 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: ml_call_gen.m
% Main author: fjh

% This module is part of the MLDS code generator.
% It handles code generation of procedures calls,
% calls to builtins, and other closely related stuff.

%-----------------------------------------------------------------------------%

:- module ml_call_gen.
:- interface.

:- import_module prog_data.
:- import_module hlds_pred, hlds_goal.
:- import_module mlds, ml_code_util.
:- import_module llds. % XXX for `code_model'

:- import_module list.

	% Generate MLDS code for an HLDS generic_call goal.
	% This includes boxing/unboxing the arguments if necessary.
:- pred ml_gen_generic_call(generic_call, list(prog_var), list(mode),
		code_model, prog_context, mlds__defns, mlds__statements,
		ml_gen_info, ml_gen_info).
:- mode ml_gen_generic_call(in, in, in, in, in, out, out, in, out) is det.

	%
	% Generate MLDS code for an HLDS procedure call, making sure to
	% box/unbox the arguments if necessary.
	%
:- pred ml_gen_call(pred_id, proc_id, list(var_name), list(mlds__lval),
		list(prog_data__type), code_model, prog_context,
		mlds__defns, mlds__statements, ml_gen_info, ml_gen_info).
:- mode ml_gen_call(in, in, in, in, in, in, in, out, out, in, out) is det.

	%
	% Generate MLDS code for a call to a builtin procedure.
	%
:- pred ml_gen_builtin(pred_id, proc_id, list(prog_var), code_model,
		prog_context, mlds__defns, mlds__statements,
		ml_gen_info, ml_gen_info).
:- mode ml_gen_builtin(in, in, in, in, in, out, out, in, out) is det.

	%
	% Generate an rval containing the address of the specified procedure.
	%
:- pred ml_gen_proc_addr_rval(pred_id, proc_id, mlds__rval,
		ml_gen_info, ml_gen_info).
:- mode ml_gen_proc_addr_rval(in, in, out, in, out) is det.

	% Given a source type and a destination type,
	% and given an source rval holding a value of the source type,
	% produce an rval that converts the source rval to the destination type.
	%
:- pred ml_gen_box_or_unbox_rval(prog_type, prog_type, mlds__rval, mlds__rval).
:- mode ml_gen_box_or_unbox_rval(in, in, in, out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds_module.
:- import_module builtin_ops.
:- import_module type_util, mode_util.

:- import_module bool, string, std_util, term, varset, require.

%-----------------------------------------------------------------------------%
%
% Code for procedure calls
%

	%
	% Generate MLDS code for an HLDS generic_call goal.
	% This includes boxing/unboxing the arguments if necessary.
	%
ml_gen_generic_call(GenericCall, ArgVars, ArgModes, CodeModel, Context,
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
	{ ArgNames = ml_gen_var_names(VarSet, ArgVars) },
	{ Params0 = ml_gen_params(ModuleInfo, ArgNames,
		BoxedArgTypes, ArgModes, CodeModel) },

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
		ArgModes, Context, InputRvals, OutputLvals, ConvArgDecls,
		ConvOutputStatements),
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
	{ proc_info_argmodes(ProcInfo, ArgModes) },

	%
	% Generate code to box/unbox the arguments
	% and compute the list of properly converted rvals/lvals
	% to pass as the function call's arguments and return values
	%
	ml_gen_arg_list(ArgNames, ArgLvals, ActualArgTypes, PredArgTypes,
		ArgModes, Context, InputRvals, OutputLvals, ConvArgDecls,
		ConvOutputStatements),

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
		list(prog_type), list(mode), prog_context, list(mlds__rval),
		list(mlds__lval), mlds__defns, mlds__statements,
		ml_gen_info, ml_gen_info).
:- mode ml_gen_arg_list(in, in, in, in, in, in, out, out, out, out,
		in, out) is det.

ml_gen_arg_list(VarNames, VarLvals, CallerTypes, CalleeTypes, Modes, Context,
		InputRvals, OutputLvals, ConvDecls, ConvOutputStatements) -->
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
			CallerTypes1, CalleeTypes1, Modes1, Context,
			InputRvals1, OutputLvals1,
			ConvDecls1, ConvOutputStatements1),
		=(MLDSGenInfo),
		{ ml_gen_info_get_module_info(MLDSGenInfo, ModuleInfo) },
		( { type_util__is_dummy_argument_type(CalleeType) } ->
			%
			% exclude arguments of type io__state etc.
			%
			{ InputRvals = InputRvals1 },
			{ OutputLvals = OutputLvals1 },
			{ ConvDecls = ConvDecls1 },
			{ ConvOutputStatements = ConvOutputStatements1 }
		; { mode_to_arg_mode(ModuleInfo, Mode, CalleeType, top_in) } ->
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

	% Convert VarRval, of type SourceType,
	% to ArgRval, of type DestType.
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

	%
	% Generate MLDS code for a call to a builtin procedure.
	%
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

