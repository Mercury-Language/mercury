%-----------------------------------------------------------------------------%
% Copyright (C) 1999-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: ml_call_gen.m
% Main author: fjh

% This module is part of the MLDS code generator.
% It handles code generation of procedures calls,
% calls to builtins, and other closely related stuff.

%-----------------------------------------------------------------------------%

:- module ml_backend__ml_call_gen.
:- interface.

:- import_module hlds__code_model.
:- import_module hlds__hlds_goal.
:- import_module hlds__hlds_pred.
:- import_module ml_backend__ml_code_util.
:- import_module ml_backend__mlds.
:- import_module parse_tree__prog_data.

:- import_module list, bool.

	% Generate MLDS code for an HLDS generic_call goal.
	% This includes boxing/unboxing the arguments if necessary.
:- pred ml_gen_generic_call(generic_call::in, list(prog_var)::in,
	list(mode)::in, determinism::in, prog_context::in,
	mlds__defns::out, mlds__statements::out,
	ml_gen_info::in, ml_gen_info::out) is det.

	%
	% ml_gen_call(PredId, ProcId, ArgNames, ArgLvals, ArgTypes,
	%	CodeModel, Context, ForClosureWrapper,
	%	Defns, Statements):
	%
	% Generate MLDS code for an HLDS procedure call, making sure to
	% box/unbox the arguments if necessary.
	%
	% If ForClosureWrapper = yes, then the type_info for type variables
	% in CallerType may not be available in the current procedure, so
	% the GC tracing code for temps introduced for boxing/unboxing (if any)
	% should obtain the type_info from the corresponding entry in the
	% `type_params' local.
	%
:- pred ml_gen_call(pred_id::in, proc_id::in, list(var_name)::in,
	list(mlds__lval)::in, list(prog_data__type)::in, code_model::in,
	prog_context::in, bool::in, mlds__defns::out, mlds__statements::out,
	ml_gen_info::in, ml_gen_info::out) is det.

	%
	% Generate MLDS code for a call to a builtin procedure.
	%
:- pred ml_gen_builtin(pred_id::in, proc_id::in, list(prog_var)::in,
	code_model::in, prog_context::in,
	mlds__defns::out, mlds__statements::out,
	ml_gen_info::in, ml_gen_info::out) is det.

	%
	% Generate MLDS code for a cast. The list of argument variables
	% must have only two elements, the input and the output.
	%
:- pred ml_gen_cast(prog_context::in, list(prog_var)::in,
	mlds__defns::out, mlds__statements::out,
	ml_gen_info::in, ml_gen_info::out) is det.

	%
	% Generate an rval containing the address of the specified procedure.
	%
:- pred ml_gen_proc_addr_rval(pred_id::in, proc_id::in, mlds__rval::out,
	ml_gen_info::in, ml_gen_info::out) is det.

	% Given a source type and a destination type,
	% and given an source rval holding a value of the source type,
	% produce an rval that converts the source rval to the destination type.
	%
:- pred ml_gen_box_or_unbox_rval(prog_type::in, prog_type::in,
	mlds__rval::in, mlds__rval::out,
	ml_gen_info::in, ml_gen_info::out) is det.

	% ml_gen_box_or_unbox_lval(CallerType, CalleeType, VarLval, VarName,
	%	Context, ForClosureWrapper, ArgNum,
	%	ArgLval, ConvDecls, ConvInputStatements, ConvOutputStatements):
	%
	% This is like `ml_gen_box_or_unbox_rval', except that it
	% works on lvals rather than rvals.
	% Given a source type and a destination type,
	% a source lval holding a value of the source type,
	% and a name to base the name of the local temporary variable on,
	% this procedure produces an lval of the destination type,
	% the declaration for the local temporary used (if any),
	% code to assign from the source lval (suitable converted)
	% to the destination lval, and code to assign from the
	% destination lval (suitable converted) to the source lval.
	%
	% If ForClosureWrapper = yes, then the type_info for type variables
	% in CallerType may not be available in the current procedure, so
	% the GC tracing code for the ConvDecls (if any) should obtain the
	% type_info from the ArgNum-th entry in the `type_params' local.
	% (If ForClosureWrapper = no, then ArgNum is unused.)
	%
:- pred ml_gen_box_or_unbox_lval(prog_type::in, prog_type::in, mlds__lval::in,
	var_name::in, prog_context::in, bool::in, int::in, mlds__lval::out,
	mlds__defns::out, mlds__statements::out, mlds__statements::out,
	ml_gen_info::in, ml_gen_info::out) is det.

        % Generate the appropriate MLDS type for a continuation function
        % for a nondet procedure whose output arguments have the
        % specified types.
	%
	% WARNING: this does not fill in the maybe_gc_trace_code for the
	% function parameters.  It is the caller's responsibility to fill
	% these in properly if needed.
        %
:- pred ml_gen_cont_params(list(mlds__type)::in, mlds__func_params::out,
	ml_gen_info::in, ml_gen_info::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs__builtin_ops.
:- import_module check_hlds__mode_util.
:- import_module check_hlds__type_util.
:- import_module hlds__hlds_data.
:- import_module hlds__hlds_module.
:- import_module libs__globals.
:- import_module libs__options.
:- import_module mdbcomp__prim_data.
:- import_module ml_backend__ml_closure_gen.
:- import_module parse_tree__error_util.
:- import_module parse_tree__prog_type.

:- import_module bool, int, string, std_util, term, varset, require, map.

%-----------------------------------------------------------------------------%
%
% Code for procedure calls
%

	%
	% Generate MLDS code for an HLDS generic_call goal.
	% This includes boxing/unboxing the arguments if necessary.
	%
	% XXX For typeclass method calls, we do some unnecessary
	% boxing/unboxing of the arguments.
	%
ml_gen_generic_call(higher_order(_, _, _, _) @ GenericCall, ArgVars, ArgModes,
		Determinism, Context, Decls, Statements, !Info) :-
	ml_gen_generic_call_2(GenericCall, ArgVars, ArgModes, Determinism,
		Context, Decls, Statements, !Info).
ml_gen_generic_call(class_method(_, _, _, _) @ GenericCall, ArgVars, ArgModes,
		Determinism, Context, Decls, Statements, !Info) :-
	ml_gen_generic_call_2(GenericCall, ArgVars, ArgModes, Determinism,
		Context, Decls, Statements, !Info).
ml_gen_generic_call(unsafe_cast, ArgVars, _ArgModes, _Determinism, Context,
		Decls, Statements, !Info) :-
	ml_gen_cast(Context, ArgVars, Decls, Statements, !Info).
ml_gen_generic_call(aditi_builtin(_, _), _, _, _, _, _, _, !Info) :-
	error("ml_gen_generic_call: aditi_builtin").

:- pred ml_gen_generic_call_2(generic_call::in, list(prog_var)::in,
	list(mode)::in, determinism::in, prog_context::in,
	mlds__defns::out, mlds__statements::out,
	ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_generic_call_2(GenericCall, ArgVars, ArgModes, Determinism, Context,
		Decls, Statements, !Info) :-
	%
	% allocate some fresh type variables to use as the Mercury types
	% of the boxed arguments
	%
	NumArgs = list__length(ArgVars),
	BoxedArgTypes = ml_make_boxed_types(NumArgs),

	%
	% create the boxed parameter types for the called function
	%
	ml_gen_info_get_module_info(!.Info, ModuleInfo),
	ml_gen_info_get_varset(!.Info, VarSet),
	ArgNames = ml_gen_var_names(VarSet, ArgVars),
	PredOrFunc = generic_call_pred_or_func(GenericCall),
	determinism_to_code_model(Determinism, CodeModel),
	Params0 = ml_gen_params(ModuleInfo, ArgNames,
		BoxedArgTypes, ArgModes, PredOrFunc, CodeModel),

	%
	% insert the `closure_arg' parameter
	%
	% The GC_TraceCode for `closure_arg' here is wrong,
	% but it doesn't matter, since `closure_arg' is only part
	% of a type (a function parameter in the function type).
	% We won't use the GC tracing code generated here, since we don't
	% generate any actual local variable or parameter for `closure_arg'.
	%
	GC_TraceCode = no,
	ClosureArgType = mlds__generic_type,
	ClosureArg = mlds__argument(
		data(var(var_name("closure_arg", no))),
		ClosureArgType, GC_TraceCode),
	Params0 = mlds__func_params(ArgParams0, RetParam),
	Params = mlds__func_params([ClosureArg | ArgParams0], RetParam),
	Signature = mlds__get_func_signature(Params),

	%
	% compute the function address
	%
	(
		GenericCall = higher_order(ClosureVar, _Purity, _PredOrFunc,
			_Arity),
		ml_gen_var(!.Info, ClosureVar, ClosureLval),
		FieldId = offset(const(int_const(1))),
			% XXX are these types right?
		FuncLval = field(yes(0), lval(ClosureLval), FieldId,
			mlds__generic_type, ClosureArgType),
		FuncType = mlds__func_type(Params),
		FuncRval = unop(unbox(FuncType), lval(FuncLval))
	;
		GenericCall = class_method(TypeClassInfoVar, MethodNum,
			_ClassId, _PredName),
		%
		% create the lval for the typeclass_info,
		% which is also the closure in this case
		%
		ml_gen_var(!.Info, TypeClassInfoVar, TypeClassInfoLval),
		ClosureLval = TypeClassInfoLval,
		%
		% extract the base_typeclass_info from the typeclass_info
		%
		BaseTypeclassInfoFieldId =
			offset(const(int_const(0))),
		BaseTypeclassInfoLval = field(yes(0),
			lval(TypeClassInfoLval), BaseTypeclassInfoFieldId,
			mlds__generic_type, ClosureArgType),
		%
		% extract the method address from the base_typeclass_info
		%
		Offset = ml_base_typeclass_info_method_offset,
		MethodFieldNum = MethodNum + Offset,
		MethodFieldId = offset(const(int_const(MethodFieldNum))),
		FuncLval = field(yes(0), lval(BaseTypeclassInfoLval),
			MethodFieldId,
			mlds__generic_type, mlds__generic_type),
		FuncType = mlds__func_type(Params),
		FuncRval = unop(unbox(FuncType), lval(FuncLval))
	;
		GenericCall = unsafe_cast,
		error("ml_gen_generic_call_2: unsafe_cast")
	;
		GenericCall = aditi_builtin(_, _),
		error("ml_gen_generic_call_2: aditi_builtin")
	),

	%
	% Assign the function address rval to a new local variable.
	% This makes the generated code slightly more readable.
	% More importantly, this is also necessary when using a
	% non-standard calling convention with GNU C, since GNU C
	% (2.95.2) ignores the function attributes on function
	% pointer types in casts.
	%
	ml_gen_info_new_conv_var(ConvVarNum, !Info),
	FuncVarName = var_name(
		string__format("func_%d", [i(ConvVarNum)]), no),
	% the function address is always a pointer to code,
	% not to the heap, so the GC doesn't need to trace it
	GC_TraceCode = no,
	FuncVarDecl = ml_gen_mlds_var_decl(var(FuncVarName),
		FuncType, GC_TraceCode, mlds__make_context(Context)),
	ml_gen_var_lval(!.Info, FuncVarName, FuncType, FuncVarLval),
	AssignFuncVar = ml_gen_assign(FuncVarLval, FuncRval, Context),
	FuncVarRval = lval(FuncVarLval),

	%
	% Generate code to box/unbox the arguments
	% and compute the list of properly converted rvals/lvals
	% to pass as the function call's arguments and return values
	%
	ml_gen_var_list(!.Info, ArgVars, ArgLvals),
	ml_variable_types(!.Info, ArgVars, ActualArgTypes),
	ml_gen_arg_list(ArgNames, ArgLvals, ActualArgTypes, BoxedArgTypes,
		ArgModes, PredOrFunc, CodeModel, Context, no, 1,
		InputRvals, OutputLvals, OutputTypes,
		ConvArgDecls, ConvOutputStatements, !Info),
	ClosureRval = unop(unbox(ClosureArgType), lval(ClosureLval)),

	%
	% Prepare to generate the call, passing the closure as the first
	% argument.
	% (We can't actually generate the call yet, since it might be nondet,
	% and we don't yet know what its success continuation will be;
	% instead for now we just construct a higher-order term `DoGenCall',
	% which when called will generate it.)
	%
	ObjectRval = no,
	DoGenCall = ml_gen_mlds_call(Signature, ObjectRval, FuncVarRval,
		[ClosureRval | InputRvals], OutputLvals, OutputTypes,
		Determinism, Context),

	( ConvArgDecls = [], ConvOutputStatements = [] ->
		DoGenCall(Decls0, Statements0, !Info)
	;
		%
		% Construct a closure to generate code to
		% convert the output arguments and then succeed
		%
		DoGenConvOutputAndSucceed = (
			pred(COAS_Decls::out, COAS_Statements::out,
					Info0::in, Info::out) is det :-
				COAS_Decls = [],
				ml_gen_success(CodeModel, Context,
					SucceedStmts, Info0, Info),
				COAS_Statements = list__append(
					ConvOutputStatements, SucceedStmts)
		),

		%
		% Conjoin the code generated by the two closures that we
		% computed above.  `ml_combine_conj' will generate whatever
		% kind of sequence is necessary for this code model.
		%
		ml_combine_conj(CodeModel, Context,
			DoGenCall, DoGenConvOutputAndSucceed,
			CallAndConvOutputDecls, CallAndConvOutputStatements,
			!Info),
		Decls0 = ConvArgDecls ++ CallAndConvOutputDecls,
		Statements0 = CallAndConvOutputStatements
	),
	Decls = [FuncVarDecl | Decls0],
	Statements = [AssignFuncVar | Statements0].

ml_gen_cast(Context, ArgVars, Decls, Statements, !Info) :-
	ml_gen_var_list(!.Info, ArgVars, ArgLvals),
	ml_variable_types(!.Info, ArgVars, ArgTypes),
	(
		ArgLvals = [SrcLval, DestLval],
		ArgTypes = [SrcType, DestType]
	->
		( type_util__is_dummy_argument_type(DestType) ->
			Statements = []
		;
			ml_gen_box_or_unbox_rval(SrcType, DestType,
				lval(SrcLval), CastRval, !Info),
			Assign = ml_gen_assign(DestLval, CastRval, Context),
			Statements = [Assign]
		),
		Decls = []
	;
		error("ml_gen_cast: wrong number of args for cast")
	).

%-----------------------------------------------------------------------------%

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
	%	func(unbox(Arg1), box(Arg2), &unboxed_Arg3, &boxed_Arg4);
	%
	%	/* code to box/unbox the output arguments */
	%	*Arg3 = box(unboxed_Arg3);
	%	*Arg4 = unbox(boxed_Arg4);
	%	...
	%
	% Note that of course in general not every argument will need
	% to be boxed/unboxed; for those where no conversion is required,
	% we just pass the original argument unchanged.
	%
ml_gen_call(PredId, ProcId, ArgNames, ArgLvals, ActualArgTypes, CodeModel,
		Context, ForClosureWrapper, Decls, Statements, !Info) :-
	%
	% Compute the function signature
	%
	ml_gen_info_get_module_info(!.Info, ModuleInfo),
	Params = ml_gen_proc_params(ModuleInfo, PredId, ProcId),
	Signature = mlds__get_func_signature(Params),

	%
	% Compute the function address
	%
	ml_gen_proc_addr_rval(PredId, ProcId, FuncRval, !Info),

	%
	% Compute the callee's Mercury argument types and modes
	%
	module_info_pred_proc_info(ModuleInfo, PredId, ProcId,
		PredInfo, ProcInfo),
	PredOrFunc = pred_info_is_pred_or_func(PredInfo),
	pred_info_arg_types(PredInfo, PredArgTypes),
	proc_info_argmodes(ProcInfo, ArgModes),

	%
	% Generate code to box/unbox the arguments
	% and compute the list of properly converted rvals/lvals
	% to pass as the function call's arguments and return values
	%
	ml_gen_arg_list(ArgNames, ArgLvals, ActualArgTypes, PredArgTypes,
		ArgModes, PredOrFunc, CodeModel, Context, ForClosureWrapper, 1,
		InputRvals, OutputLvals, OutputTypes,
		ConvArgDecls, ConvOutputStatements, !Info),

	%
	% Construct a closure to generate the call
	% (We can't actually generate the call yet, since it might be nondet,
	% and we don't yet know what its success continuation will be;
	% that's why for now we just construct a closure `DoGenCall'
	% to generate it.)
	%
	ObjectRval = no,
	proc_info_interface_determinism(ProcInfo, Detism),
	DoGenCall = ml_gen_mlds_call(Signature, ObjectRval, FuncRval,
		InputRvals, OutputLvals, OutputTypes, Detism, Context),

	( ConvArgDecls = [], ConvOutputStatements = [] ->
		DoGenCall(Decls, Statements, !Info)
	;
		%
		% Construct a closure to generate code to
		% convert the output arguments and then succeed
		%
		DoGenConvOutputAndSucceed = (
			pred(COAS_Decls::out, COAS_Statements::out,
					Info0::in, Info::out) is det :-
				COAS_Decls = [],
				ml_gen_success(CodeModel, Context,
					SucceedStmts, Info0, Info),
				COAS_Statements = list__append(
					ConvOutputStatements, SucceedStmts)
		),

		%
		% Conjoin the code generated by the two closures that we
		% computed above.  `ml_combine_conj' will generate whatever
		% kind of sequence is necessary for this code model.
		%
		ml_combine_conj(CodeModel, Context, DoGenCall,
			DoGenConvOutputAndSucceed, CallAndConvOutputDecls,
			CallAndConvOutputStatements, !Info),
		Decls = list__append(ConvArgDecls, CallAndConvOutputDecls),
		Statements = CallAndConvOutputStatements
	).

	%
	% This generates a call in the specified code model.
	% This is a lower-level routine called by both ml_gen_call
	% and ml_gen_generic_call.
	%
:- pred ml_gen_mlds_call(mlds__func_signature::in, maybe(mlds__rval)::in,
	mlds__rval::in, list(mlds__rval)::in, list(mlds__lval)::in,
	list(mlds__type)::in, determinism::in, prog_context::in,
	mlds__defns::out, mlds__statements::out,
	ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_mlds_call(Signature, ObjectRval, FuncRval, ArgRvals0, RetLvals0,
		RetTypes0, Detism, Context, Decls, Statements, !Info) :-
	%
	% append the extra arguments or return val for this code_model
	%
	determinism_to_code_model(Detism, CodeModel),
	(
		CodeModel = model_non,
		% create a new success continuation, if necessary
		ml_gen_success_cont(RetTypes0, RetLvals0, Context,
			Cont, ContDecls, !Info),
		% append the success continuation to the ordinary arguments
		Cont = success_cont(FuncPtrRval, EnvPtrRval, _, _),
		ml_gen_info_use_gcc_nested_functions(!.Info, UseNestedFuncs),
		( UseNestedFuncs = yes ->
			ArgRvals = list__append(ArgRvals0, [FuncPtrRval])
		;
			ArgRvals = list__append(ArgRvals0,
				[FuncPtrRval, EnvPtrRval])
		),
		% for --nondet-copy-out, the output arguments will be
		% passed to the continuation rather than being returned
		ml_gen_info_get_globals(!.Info, Globals),
		globals__lookup_bool_option(Globals, nondet_copy_out,
			NondetCopyOut),
		( NondetCopyOut = yes ->
			RetLvals = []
		;
			RetLvals = RetLvals0
		),
		Decls = ContDecls
	;
		CodeModel = model_semi,
		% return a bool indicating whether or not it succeeded
		ml_success_lval(!.Info, Success),
		ArgRvals = ArgRvals0,
		RetLvals = list__append([Success], RetLvals0),
		Decls = []
	;
		CodeModel = model_det,
		ArgRvals = ArgRvals0,
		RetLvals = RetLvals0,
		Decls = []
	),

	%
	% build the MLDS call statement
	%
	% if the called procedure has determinism `erroneous'
	% then mark it as never returning
	% (this will ensure that it gets treated as a tail call)
	( Detism = erroneous ->
		CallKind = no_return_call
	;
		CallKind = ordinary_call
	),
	Stmt = call(Signature, FuncRval, ObjectRval, ArgRvals, RetLvals,
			CallKind),
	Statement = mlds__statement(Stmt, mlds__make_context(Context)),
	Statements = [Statement].

:- pred ml_gen_success_cont(list(mlds__type)::in, list(mlds__lval)::in,
	prog_context::in, success_cont::out, mlds__defns::out,
	ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_success_cont(OutputArgTypes, OutputArgLvals, Context,
		Cont, ContDecls, !Info) :-
	ml_gen_info_current_success_cont(CurrentCont, !Info),
	CurrentCont = success_cont(_FuncPtrRval, _EnvPtrRval,
		CurrentContArgTypes, CurrentContArgLvals),
	(
		%
		% As an optimization, check if the parameters expected by
		% the current continuation are the same as the ones
		% expected by the new continuation that we're generating;
		% if so, we can just use the current continuation rather
		% than creating a new one.
		%
		CurrentContArgTypes = OutputArgTypes,
		CurrentContArgLvals = OutputArgLvals
	->
		Cont = CurrentCont,
		ContDecls = []
	;
		%
		% Create a new continuation function
		% that just copies the outputs to locals
		% and then calls the original current continuation.
		%
		% Note that ml_gen_cont_params does not fill in the
		% maybe_gc_trace_code for the parameters.  This is OK,
		% because the parameters of the continuation function
		% will not be live across any heap allocations or
		% procedure calls.
		%
		ml_gen_cont_params(OutputArgTypes, Params, !Info),
		ml_gen_new_func_label(yes(Params),
			ContFuncLabel, ContFuncLabelRval, !Info),
		/* push nesting level */
		ml_gen_copy_args_to_locals(!.Info, OutputArgLvals,
			OutputArgTypes, Context, CopyDecls, CopyStatements),
		ml_gen_call_current_success_cont(Context, CallCont, !Info),
		CopyStatement = ml_gen_block(CopyDecls,
			list__append(CopyStatements, [CallCont]), Context),
		/* pop nesting level */
		ml_gen_label_func(!.Info, ContFuncLabel, Params, Context,
			CopyStatement, ContFuncDefn),
		ContDecls = [ContFuncDefn],

		ml_get_env_ptr(!.Info, EnvPtrRval),
		NewSuccessCont = success_cont(ContFuncLabelRval,
			EnvPtrRval, OutputArgTypes, OutputArgLvals),
		Cont = NewSuccessCont
	).

ml_gen_cont_params(OutputArgTypes, Params, !Info) :-
	ml_gen_cont_params_2(OutputArgTypes, 1, Args0),
	ml_gen_info_use_gcc_nested_functions(!.Info, UseNestedFuncs),
	( UseNestedFuncs = yes ->
		Args = Args0
	;
		ml_declare_env_ptr_arg(EnvPtrArg),
		Args = list__append(Args0, [EnvPtrArg])
	),
	Params = mlds__func_params(Args, []).

:- pred ml_gen_cont_params_2(list(mlds__type)::in, int::in,
	mlds__arguments::out) is det.

ml_gen_cont_params_2([], _, []).
ml_gen_cont_params_2([Type | Types], ArgNum, [Argument | Arguments]) :-
	ArgName = ml_gen_arg_name(ArgNum),
	% Figuring out the correct GC code here is difficult,
	% since doing that requires knowing the HLDS types, but
	% here we only have the MLDS types.  So here we just
	% leave it blank.  The caller of ml_gen_cont_param has the
	% reponsibility of fillling this in properly if needed.
	Maybe_GC_TraceCode = no,
	Argument = mlds__argument(data(var(ArgName)), Type,
		Maybe_GC_TraceCode),
	ml_gen_cont_params_2(Types, ArgNum + 1, Arguments).

:- pred ml_gen_copy_args_to_locals(ml_gen_info::in, list(mlds__lval)::in,
	list(mlds__type)::in, prog_context::in, mlds__defns::out,
	mlds__statements::out) is det.

ml_gen_copy_args_to_locals(Info, ArgLvals, ArgTypes, Context,
		CopyDecls, CopyStatements) :-
	CopyDecls = [],
	ml_gen_copy_args_to_locals_2(Info, ArgLvals, ArgTypes, 1, Context,
		CopyStatements).

:- pred ml_gen_copy_args_to_locals_2(ml_gen_info::in, list(mlds__lval)::in,
	list(mlds__type)::in, int::in, prog_context::in, mlds__statements::out)
	is det.

ml_gen_copy_args_to_locals_2(_Info, [], [], _, _, []).
ml_gen_copy_args_to_locals_2(Info, [LocalLval | LocalLvals], [Type | Types],
		ArgNum, Context, [Statement | Statements]) :-
	ArgName = ml_gen_arg_name(ArgNum),
	ml_gen_var_lval(Info, ArgName, Type, ArgLval),
	Statement = ml_gen_assign(LocalLval, lval(ArgLval), Context),
	ml_gen_copy_args_to_locals_2(Info, LocalLvals, Types, ArgNum + 1,
		Context, Statements).
ml_gen_copy_args_to_locals_2(_Info, [], [_ | _], _, _, _) :-
	error("ml_gen_copy_args_to_locals_2: list length mismatch").
ml_gen_copy_args_to_locals_2(_Info, [_ | _], [], _, _, _) :-
	error("ml_gen_copy_args_to_locals_2: list length mismatch").

:- func ml_gen_arg_name(int) = mlds__var_name.

ml_gen_arg_name(ArgNum) = mlds__var_name(ArgName, no) :-
	string__format("arg%d", [i(ArgNum)], ArgName).

%
% Generate an rval containing the address of the specified procedure
%
ml_gen_proc_addr_rval(PredId, ProcId, CodeAddrRval, !Info) :-
	ml_gen_info_get_module_info(!.Info, ModuleInfo),
	ml_gen_pred_label(ModuleInfo, PredId, ProcId, PredLabel, PredModule),
	ml_gen_proc_params(PredId, ProcId, Params, !Info),
	Signature = mlds__get_func_signature(Params),
	QualifiedProcLabel = qual(PredModule, module_qual, PredLabel - ProcId),
	CodeAddrRval = const(code_addr_const(proc(QualifiedProcLabel,
		Signature))).

%
% Generate rvals and lvals for the arguments of a procedure call
%
:- pred ml_gen_arg_list(list(var_name)::in, list(mlds__lval)::in,
	list(prog_type)::in, list(prog_type)::in, list(mode)::in,
	pred_or_func::in, code_model::in, prog_context::in, bool::in, int::in,
	list(mlds__rval)::out, list(mlds__lval)::out, list(mlds__type)::out,
	mlds__defns::out, mlds__statements::out,
	ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_arg_list(VarNames, VarLvals, CallerTypes, CalleeTypes, Modes,
		PredOrFunc, CodeModel, Context, ForClosureWrapper, ArgNum,
		InputRvals, OutputLvals, OutputTypes,
		ConvDecls, ConvOutputStatements, !Info) :-
	(
		VarNames = [],
		VarLvals = [],
		CallerTypes = [],
		CalleeTypes = [],
		Modes = []
	->
		InputRvals = [],
		OutputLvals = [],
		OutputTypes = [],
		ConvDecls = [],
		ConvOutputStatements = []
	;
		VarNames = [VarName | VarNames1],
		VarLvals = [VarLval | VarLvals1],
		CallerTypes = [CallerType | CallerTypes1],
		CalleeTypes = [CalleeType | CalleeTypes1],
		Modes = [Mode | Modes1]
	->
		ml_gen_arg_list(VarNames1, VarLvals1,
			CallerTypes1, CalleeTypes1, Modes1,
			PredOrFunc, CodeModel, Context, ForClosureWrapper,
			ArgNum + 1, InputRvals1, OutputLvals1, OutputTypes1,
			ConvDecls1, ConvOutputStatements1, !Info),
		ml_gen_info_get_module_info(!.Info, ModuleInfo),
		mode_to_arg_mode(ModuleInfo, Mode, CalleeType, ArgMode),
		(
			( type_util__is_dummy_argument_type(CalleeType)
			; ArgMode = top_unused
			)
		->
			%
			% Exclude arguments of type io__state etc.
			% Also exclude those with arg_mode `top_unused'.
			%
			InputRvals = InputRvals1,
			OutputLvals = OutputLvals1,
			OutputTypes = OutputTypes1,
			ConvDecls = ConvDecls1,
			ConvOutputStatements = ConvOutputStatements1
		;
			ArgMode = top_in
		->
			%
			% it's an input argument
			%
			( type_util__is_dummy_argument_type(CallerType) ->
				% The variable may not have been declared,
				% so we need to generate a dummy value for it.
				% Using `0' here is more efficient than
				% using private_builtin__dummy_var, which is
				% what ml_gen_var will have generated for this
				% variable.
				VarRval = const(int_const(0))
			;
				VarRval = lval(VarLval)
			),
			ml_gen_box_or_unbox_rval(CallerType, CalleeType,
				VarRval, ArgRval, !Info),
			InputRvals = [ArgRval | InputRvals1],
			OutputLvals = OutputLvals1,
			OutputTypes = OutputTypes1,
			ConvDecls = ConvDecls1,
			ConvOutputStatements = ConvOutputStatements1
		;
			%
			% it's an output argument
			%
			ml_gen_box_or_unbox_lval(CallerType, CalleeType,
				VarLval, VarName, Context, ForClosureWrapper,
				ArgNum, ArgLval, ThisArgConvDecls,
				_ThisArgConvInput, ThisArgConvOutput, !Info),
			ConvDecls = ThisArgConvDecls ++ ConvDecls1,
			ConvOutputStatements = ThisArgConvOutput
				++ ConvOutputStatements1,

			ml_gen_info_get_globals(!.Info, Globals),
			CopyOut = get_copy_out_option(Globals, CodeModel),
			(
				(
					%
					% if the target language allows
					% multiple return values, then use them
					%
					CopyOut = yes
				;
					%
					% if this is the result argument
					% of a model_det function, and it has
					% an output mode, then return it as a
					% value
					%
					VarNames1 = [],
					CodeModel = model_det,
					PredOrFunc = function,
					ArgMode = top_out
				)
			->
				InputRvals = InputRvals1,
				OutputLvals = [ArgLval | OutputLvals1],
				ml_gen_type(!.Info, CalleeType, OutputType),
				OutputTypes = [OutputType | OutputTypes1]
			;
				%
				% otherwise use the traditional C style
				% of passing the address of the output value
				%
				InputRvals = [ml_gen_mem_addr(ArgLval)
					| InputRvals1],
				OutputLvals = OutputLvals1,
				OutputTypes = OutputTypes1
			)
		)
	;
		error("ml_gen_arg_list: length mismatch")
	).

	% ml_gen_mem_addr(Lval) returns a value equal to &Lval.
	% For the case where Lval = *Rval, for some Rval,
	% we optimize &*Rval to just Rval.
:- func ml_gen_mem_addr(mlds__lval) = mlds__rval.

ml_gen_mem_addr(Lval) =
	(if Lval = mem_ref(Rval, _) then Rval else mem_addr(Lval)).

	% Convert VarRval, of type SourceType,
	% to ArgRval, of type DestType.
ml_gen_box_or_unbox_rval(SourceType, DestType, VarRval, ArgRval, !Info) :-
	(
		%
		% if converting from polymorphic type to concrete type,
		% then unbox
		%
		SourceType = term__variable(_),
		DestType = term__functor(_, _, _)
	->
		ml_gen_type(!.Info, DestType, MLDS_DestType),
		ArgRval = unop(unbox(MLDS_DestType), VarRval)
	;
		%
		% if converting from concrete type to polymorphic type,
		% then box
		%
		SourceType = term__functor(_, _, _),
		DestType = term__variable(_)
	->
		ml_gen_type(!.Info, SourceType, MLDS_SourceType),
		ArgRval = unop(box(MLDS_SourceType), VarRval)
	;
		%
		% if converting to float, cast to mlds__generic_type
		% and then unbox
		%
		DestType = term__functor(term__atom("float"), [], _),
		SourceType \= term__functor(term__atom("float"), [], _)
	->
		ml_gen_type(!.Info, DestType, MLDS_DestType),
		ArgRval = unop(unbox(MLDS_DestType),
			unop(cast(mlds__generic_type), VarRval))
	;
		%
		% if converting from float, box and then cast the result
		%
		SourceType = term__functor(term__atom("float"), [], _),
		DestType \= term__functor(term__atom("float"), [], _)
	->
		ml_gen_type(!.Info, SourceType, MLDS_SourceType),
		ml_gen_type(!.Info, DestType, MLDS_DestType),
		ArgRval = unop(cast(MLDS_DestType),
			unop(box(MLDS_SourceType), VarRval))
	;
		%
		% if converting from an array(T) to array(X) where
		% X is a concrete instance, we should insert a cast
		% to the concrete instance.  Also when converting to
		% array(T) from array(X) we should cast to array(T).
		%
		type_to_ctor_and_args(SourceType, SourceTypeCtor,
			SourceTypeArgs),
		type_to_ctor_and_args(DestType, DestTypeCtor, DestTypeArgs),
		(
			type_ctor_is_array(SourceTypeCtor),
			SourceTypeArgs = [term__variable(_)]
		;
			type_ctor_is_array(DestTypeCtor),
			DestTypeArgs = [term__variable(_)]
		)
	->
		ml_gen_type(!.Info, DestType, MLDS_DestType),
		ArgRval = unop(cast(MLDS_DestType), VarRval)
	;
		%
		% if converting from one concrete type to a different
		% one, then cast
		%
		% This is needed to handle construction/deconstruction
		% unifications for no_tag types.
		%
		\+ type_util__type_unify(SourceType, DestType,
			[], map__init, _)
	->
		ml_gen_type(!.Info, DestType, MLDS_DestType),
		ArgRval = unop(cast(MLDS_DestType), VarRval)
	;
		%
		% otherwise leave unchanged
		%
		ArgRval = VarRval
	).

ml_gen_box_or_unbox_lval(CallerType, CalleeType, VarLval, VarName, Context,
		ForClosureWrapper, ArgNum, ArgLval, ConvDecls,
		ConvInputStatements, ConvOutputStatements, !Info) :-
	%
	% First see if we can just convert the lval as an rval;
	% if no boxing/unboxing is required, then ml_box_or_unbox_rval
	% will return its argument unchanged, and so we're done.
	%
	ml_gen_box_or_unbox_rval(CalleeType, CallerType, lval(VarLval),
		BoxedRval, !Info),
	( BoxedRval = lval(VarLval) ->
		ArgLval = VarLval,
		ConvDecls = [],
		ConvInputStatements = [],
		ConvOutputStatements = []
	;
		%
		% If that didn't work, then we need to declare a fresh variable
		% to use as the arg, and to generate statements to box/unbox
		% that fresh arg variable and assign it to/from the output
		% argument whose address we were passed.
		%

		% generate a declaration for the fresh variable
		%
		% Note that generating accurate GC tracing code for this
		% variable requires some care, because CalleeType might be a
		% type variable from the callee, not from the caller,
		% and we can't generate type_infos for type variables
		% from the callee.  Hence we need to call the version of
		% ml_gen_maybe_gc_trace_code which takes two types:
		% the CalleeType is used to determine the type for the
		% temporary variable declaration, but the CallerType is
		% used to construct the type_info.

		ml_gen_info_new_conv_var(ConvVarNum, !Info),
		VarName = mlds__var_name(VarNameStr, MaybeNum),
		ArgVarName = mlds__var_name(string__format(
			"conv%d_%s", [i(ConvVarNum), s(VarNameStr)]),
			MaybeNum),
		ml_gen_type(!.Info, CalleeType, MLDS_CalleeType),
		( ForClosureWrapper = yes ->
			% For closure wrappers, the argument type_infos are
			% stored in the `type_params' local, so we need to
			% handle the GC tracing code specially
			( prog_type__var(CallerType, _TypeVar) ->
				ml_gen_local_for_output_arg(ArgVarName,
					CalleeType, ArgNum, Context,
					ArgVarDecl, !Info)
			;
				unexpected(this_file, "invalid CalleeType " ++
					"for closure wrapper")
			)
		;
			ml_gen_maybe_gc_trace_code(ArgVarName, CalleeType,
				CallerType, Context, GC_TraceCode, !Info),
			ArgVarDecl = ml_gen_mlds_var_decl(var(ArgVarName),
				MLDS_CalleeType, GC_TraceCode,
				mlds__make_context(Context))
		),
		ConvDecls = [ArgVarDecl],

		% create the lval for the variable and use it for the
		% argument lval
		ml_gen_var_lval(!.Info, ArgVarName, MLDS_CalleeType, ArgLval),

		( type_util__is_dummy_argument_type(CallerType) ->
			% if it is a dummy argument type (e.g. io__state),
			% then we don't need to bother assigning it
			ConvInputStatements = [],
			ConvOutputStatements = []
		;
			%
			% generate statements to box/unbox the fresh variable
			% and assign it to/from the output argument whose
			% address we were passed.
			%

			% assign to the freshly generated arg variable
			ml_gen_box_or_unbox_rval(CallerType, CalleeType,
				lval(VarLval), ConvertedVarRval, !Info),
			AssignInputStatement = ml_gen_assign(ArgLval,
				ConvertedVarRval, Context),
			ConvInputStatements = [AssignInputStatement],

			% assign from the freshly generated arg variable
			ml_gen_box_or_unbox_rval(CalleeType, CallerType,
				lval(ArgLval), ConvertedArgRval, !Info),
			AssignOutputStatement = ml_gen_assign(VarLval,
				ConvertedArgRval, Context),
			ConvOutputStatements = [AssignOutputStatement]
		)
	).

%-----------------------------------------------------------------------------%
%
% Code for builtins
%

	%
	% Generate MLDS code for a call to a builtin procedure.
	%
ml_gen_builtin(PredId, ProcId, ArgVars, CodeModel, Context, Decls, Statements,
		!Info) :-
	ml_gen_var_list(!.Info, ArgVars, ArgLvals),
	ml_gen_info_get_module_info(!.Info, ModuleInfo),
	predicate_module(ModuleInfo, PredId, ModuleName),
	predicate_name(ModuleInfo, PredId, PredName),
	(
		builtin_ops__translate_builtin(ModuleName, PredName,
			ProcId, ArgLvals, SimpleCode0)
	->
		SimpleCode = SimpleCode0
	;
		error("ml_gen_builtin: unknown builtin predicate")
	),
	(
		CodeModel = model_det,
		( SimpleCode = assign(Lval, SimpleExpr) ->
			(
				% we need to avoid generating assignments to
				% dummy variables introduced for types such
				% as io__state
				Lval = var(_VarName, VarType),
				VarType = mercury_type(ProgDataType, _, _),
				type_util__is_dummy_argument_type(ProgDataType)
			->
				Statements = []
			;
				Rval = ml_gen_simple_expr(SimpleExpr),
				Statement = ml_gen_assign(Lval, Rval, Context),
				Statements = [Statement]
			)
		;
			error("Malformed det builtin predicate")
		)
	;
		CodeModel = model_semi,
		( SimpleCode = test(SimpleTest) ->
			TestRval = ml_gen_simple_expr(SimpleTest),
			ml_gen_set_success(!.Info, TestRval, Context,
				Statement),
			Statements = [Statement]
		;
			error("Malformed semi builtin predicate")
		)
	;
		CodeModel = model_non,
		error("Nondet builtin predicate")
	),
	Decls = [].

:- func ml_gen_simple_expr(simple_expr(mlds__lval)) = mlds__rval.

ml_gen_simple_expr(leaf(Lval)) = lval(Lval).
ml_gen_simple_expr(int_const(Int)) = const(int_const(Int)).
ml_gen_simple_expr(float_const(Float)) = const(float_const(Float)).
ml_gen_simple_expr(unary(Op, Expr)) =
	unop(std_unop(Op), ml_gen_simple_expr(Expr)).
ml_gen_simple_expr(binary(Op, Expr1, Expr2)) =
	binop(Op, ml_gen_simple_expr(Expr1), ml_gen_simple_expr(Expr2)).

:- func this_file = string.

this_file = "ml_call_gen.m".
