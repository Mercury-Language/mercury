%-----------------------------------------------------------------------------%
% Copyright (C) 1999-2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: ml_unify_gen.m
% Main author: fjh

% This module is part of the MLDS code generator.
% It handles MLDS code generation for unifications.

%-----------------------------------------------------------------------------%

:- module ml_unify_gen.
:- interface.

:- import_module prog_data.
:- import_module hlds_module, hlds_pred, hlds_data, hlds_goal.
:- import_module code_model.
:- import_module mlds, ml_code_util.

%-----------------------------------------------------------------------------%

	% Generate MLDS code for a unification.
	%
:- pred ml_gen_unification(unification, code_model, prog_context,
		mlds__defns, mlds__statements, ml_gen_info, ml_gen_info).
:- mode ml_gen_unification(in, in, in, out, out, in, out) is det.

	% Convert a cons_id for a given type to a cons_tag.
	%
:- pred ml_cons_id_to_tag(cons_id, prog_type, cons_tag,
		ml_gen_info, ml_gen_info).
:- mode ml_cons_id_to_tag(in, in, out, in, out) is det.

	% ml_gen_tag_test(Var, ConsId, Defns, Statements, Expression):
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

	% ml_gen_secondary_tag_rval(PrimaryTag, VarType, ModuleInfo, VarRval):
	%	Return the rval for the secondary tag field of VarRval,
	%	assuming that VarRval has the specified VarType and PrimaryTag.
:- func ml_gen_secondary_tag_rval(tag_bits, prog_type, module_info, mlds__rval)
	= mlds__rval.

	%
	% ml_gen_closure_wrapper(PredId, ProcId, Offset, NumClosureArgs,
	%	Context, WrapperFuncRval, WrapperFuncType):
	%
	% Generates a wrapper function which unboxes the input arguments,
	% calls the specified procedure, passing it some extra arguments
	% from the closure, and then boxes the output arguments.
	% It adds the definition of this wrapper function to the extra_defns
	% field in the ml_gen_info, and return the wrapper function's
	% rval and type.
	%
	% The NumClosuresArgs parameter specifies how many arguments
	% to extract from the closure.  The Offset parameter specifies
	% the offset to add to the argument number to get the field
	% number within the closure.  (Argument numbers start from 1,
	% and field numbers start from 0.)
	%
:- pred ml_gen_closure_wrapper(pred_id, proc_id, int, int, prog_context,
		mlds__rval, mlds__type, ml_gen_info, ml_gen_info).
:- mode ml_gen_closure_wrapper(in, in, in, in, in, out, out,
		in, out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds_out, builtin_ops.
:- import_module ml_code_gen, ml_call_gen, ml_type_gen.
:- import_module prog_util, type_util, mode_util.
:- import_module rtti, error_util.
:- import_module code_util. % XXX needed for `code_util__cons_id_to_tag'.
:- import_module globals, options.

:- import_module bool, int, string, list, map, require, std_util, term, varset.

%-----------------------------------------------------------------------------%

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
		HowToConstruct, _CellIsUnique, MaybeAditiRLExprnID),
		CodeModel, Context, MLDS_Decls, MLDS_Statements) -->
	{ require(unify(CodeModel, model_det),
		"ml_code_gen: construct not det") },
	{ MaybeAditiRLExprnID = yes(_) ->
		sorry(this_file, "Aditi closures")
	;
		true
	},
	ml_gen_construct(Var, ConsId, Args, ArgModes, HowToConstruct, Context,
		MLDS_Decls, MLDS_Statements).

ml_gen_unification(deconstruct(Var, ConsId, Args, ArgModes, CanFail, CanCGC),
		CodeModel, Context, MLDS_Decls, MLDS_Statements) -->
	(
		{ CanFail = can_fail },
		{ ExpectedCodeModel = model_semi },
		ml_gen_semi_deconstruct(Var, ConsId, Args, ArgModes, Context,
			MLDS_Decls, MLDS_Unif_Statements)
	;
		{ CanFail = cannot_fail },
		{ ExpectedCodeModel = model_det },
		ml_gen_det_deconstruct(Var, ConsId, Args, ArgModes, Context,
			MLDS_Decls, MLDS_Unif_Statements)
	),
	(
		%
		% Note that we can deallocate a cell even if the
		% unification fails, it is the responsibility of the
		% structure reuse phase to ensure that this is safe.
		%
		{ CanCGC = yes },
		ml_gen_var(Var, VarLval),
		{ MLDS_Stmt = atomic(delete_object(VarLval)) },
		{ MLDS_CGC_Statements = [mlds__statement(MLDS_Stmt,
				mlds__make_context(Context)) ] }
	;
		{ CanCGC = no },
		{ MLDS_CGC_Statements = [] }
	),
	{ MLDS_Statements0 = MLDS_Unif_Statements `list__append`
			MLDS_CGC_Statements },
	%
	% We used to require that CodeModel = ExpectedCodeModel.
	% But the determinism field in the goal_info is allowed to
	% be a conservative approximation, so we need to handle
	% the case were CodeModel is less precise than
	% ExpectedCodeModel.
	%
	ml_gen_wrap_goal(CodeModel, ExpectedCodeModel, Context,
		MLDS_Statements0, MLDS_Statements).

ml_gen_unification(complicated_unify(_, _, _), _, _, [], []) -->
	% simplify.m should convert these into procedure calls
	{ error("ml_code_gen: complicated unify") }.

	% ml_gen_construct generations code for a construction unification.
	%
	% Note that the code for ml_gen_static_const_arg is very similar to
	% the code here, and any changes may need to be done in both places.
	%
:- pred ml_gen_construct(prog_var, cons_id, prog_vars, list(uni_mode),
		how_to_construct, prog_context, mlds__defns, mlds__statements,
		ml_gen_info, ml_gen_info).
:- mode ml_gen_construct(in, in, in, in, in, in, out, out, in, out) is det.

ml_gen_construct(Var, ConsId, Args, ArgModes, HowToConstruct, Context,
		MLDS_Decls, MLDS_Statements) -->
	%
	% figure out how this cons_id is represented
	%
	ml_variable_type(Var, Type),
	ml_cons_id_to_tag(ConsId, Type, Tag),

	(
		%
		% no_tag types
		%
		{ Tag = no_tag }
	->
		( { Args = [Arg], ArgModes = [ArgMode] } ->
			ml_variable_type(Arg, ArgType),
			ml_variable_type(Var, VarType),
			ml_gen_var(Arg, ArgLval),
			ml_gen_var(Var, VarLval),
			ml_gen_sub_unify(ArgMode, ArgLval, ArgType, VarLval,
				VarType, Context, [], MLDS_Statements),
			{ MLDS_Decls = [] }
		;
			{ error("ml_code_gen: no_tag: arity != 1") }
		)
	;
		%
		% lambda expressions
		%
		{ Tag = pred_closure_tag(PredId, ProcId, EvalMethod) }
	->
		ml_gen_closure(PredId, ProcId, EvalMethod, Var, Args,
				ArgModes, HowToConstruct, Context,
				MLDS_Decls, MLDS_Statements)
	;
		%
		% ordinary compound terms
		%
		{ Tag = unshared_tag(TagVal),
		  MaybeSecondaryTag = no
		; Tag = shared_remote_tag(TagVal, SecondaryTag),
		  MaybeSecondaryTag = yes(SecondaryTag)
		}
	->
		ml_gen_compound(TagVal, MaybeSecondaryTag, ConsId, Var, Args,
			ArgModes, HowToConstruct, Context,
			MLDS_Decls, MLDS_Statements)
	;
		%
		% constants
		%
		{ Args = [] }
	->
		ml_gen_var(Var, VarLval),
		ml_gen_constant(Tag, Type, Rval),
		{ MLDS_Statement = ml_gen_assign(VarLval, Rval, Context) },
		{ MLDS_Decls = [] },
		{ MLDS_Statements = [MLDS_Statement] }
	;
		{ error("ml_gen_construct: unknown compound term") }
	).

	% ml_gen_static_const_arg is similar to ml_gen_construct
	% with HowToConstruct = construct_statically(_),
	% except that for compound terms, rather than generating
	% a new static constant, it just generates a reference
	% to one that has already been defined.
	%
	% Note that any changes here may require similar changes to
	% ml_gen_construct.
	%
:- pred ml_gen_static_const_arg(prog_var, static_cons, mlds__rval,
	ml_gen_info, ml_gen_info).
:- mode ml_gen_static_const_arg(in, in, out, in, out) is det.

ml_gen_static_const_arg(Var, static_cons(ConsId, ArgVars, StaticArgs), Rval) -->
	%
	% figure out how this argument is represented
	%
	ml_variable_type(Var, VarType),
	ml_cons_id_to_tag(ConsId, VarType, Tag),

	(
		%
		% no_tag types
		%
		{ Tag = no_tag }
	->
		( { ArgVars = [Arg], StaticArgs = [StaticArg] } ->
			% construct (statically) the argument,
			% and then convert it to the appropriate type
			ml_gen_static_const_arg(Arg, StaticArg, ArgRval),
			ml_variable_type(Arg, ArgType),
			ml_gen_box_or_unbox_rval(ArgType, VarType,
				ArgRval, Rval)
		;
			{ error("ml_code_gen: no_tag: arity != 1") }
		)
	;
		%
		% compound terms, including lambda expressions
		%
		{ Tag = pred_closure_tag(_, _, _), TagVal = 0
		; Tag = unshared_tag(TagVal)
		; Tag = shared_remote_tag(TagVal, _SecondaryTag)
		}
	->
		%
		% If this argument is something that would normally be allocated
		% on the heap, just generate a reference to the static constant
		% that we must have already generated for it.
		% XXX Using mlds__array_type(mlds__generic_type) is probably 
		% wrong when `--high-level-data' is enabled.
		%
		{ ConstType = mlds__array_type(mlds__generic_type) },	
		ml_gen_static_const_addr(Var, ConstType, ConstAddrRval),
		{ TagVal = 0 ->
			TaggedRval = ConstAddrRval
		;
			TaggedRval = mkword(TagVal, ConstAddrRval)
		},
		ml_gen_type(VarType, MLDS_VarType),
		{ Rval = unop(cast(MLDS_VarType), TaggedRval) }
	;
		%
		% If this argument is just a constant,
		% then generate the rval for the constant
		%
		{ StaticArgs = [] }
	->
		ml_gen_constant(Tag, VarType, Rval)
	;
		{ error("ml_gen_static_const_arg: unknown compound term") }
	).

	%
	% generate the rval for a given constant
	%
:- pred ml_gen_constant(cons_tag, prog_type, mlds__rval,
		ml_gen_info, ml_gen_info).
:- mode ml_gen_constant(in, in, out, in, out) is det.

ml_gen_constant(string_constant(String), _, const(string_const(String)))
	--> [].

ml_gen_constant(int_constant(Int), _, const(int_const(Int))) --> [].

ml_gen_constant(float_constant(Float), _, const(float_const(Float))) --> [].

ml_gen_constant(shared_local_tag(Bits1, Num1), VarType, Rval) -->
	ml_gen_type(VarType, MLDS_Type),
	{ Rval = unop(cast(MLDS_Type), mkword(Bits1,
		unop(std_unop(mkbody), const(int_const(Num1))))) }.

ml_gen_constant(type_ctor_info_constant(ModuleName0, TypeName, TypeArity),
		VarType, Rval) -->
	ml_gen_type(VarType, MLDS_VarType),
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
	{ RttiTypeId = rtti_type_id(ModuleName, TypeName, TypeArity) },
	{ DataAddr = data_addr(MLDS_Module,
		rtti(RttiTypeId, type_ctor_info)) },
	{ Rval = unop(cast(MLDS_VarType),
			const(data_addr_const(DataAddr))) }.

ml_gen_constant(base_typeclass_info_constant(ModuleName, ClassId,
			Instance), VarType, Rval) -->
	ml_gen_type(VarType, MLDS_VarType),
	{ MLDS_Module = mercury_module_name_to_mlds(ModuleName) },
	{ DataAddr = data_addr(MLDS_Module,
		base_typeclass_info(ClassId, Instance)) },
	{ Rval = unop(cast(MLDS_VarType),
			const(data_addr_const(DataAddr))) }.

ml_gen_constant(tabling_pointer_constant(PredId, ProcId), VarType, Rval) -->
	ml_gen_type(VarType, MLDS_VarType),
	=(Info),
	{ ml_gen_info_get_module_info(Info, ModuleInfo) },
	{ ml_gen_pred_label(ModuleInfo, PredId, ProcId,
		PredLabel, PredModule) },
	{ DataAddr = data_addr(PredModule,
			tabling_pointer(PredLabel - ProcId)) },
	{ Rval = unop(cast(MLDS_VarType),
			const(data_addr_const(DataAddr))) }.

ml_gen_constant(deep_profiling_proc_static_tag(_), _, _) -->
	{ error("ml_gen_constant: deep_profiling_proc_static_tag not yet supported") }.

ml_gen_constant(code_addr_constant(PredId, ProcId), _, ProcAddrRval) -->
	ml_gen_proc_addr_rval(PredId, ProcId, ProcAddrRval).

% tags which are not (necessarily) constants are handled
% in ml_gen_construct and ml_gen_static_const_arg,
% so we don't need to handle them here.
ml_gen_constant(no_tag, _, _) -->
	{ error("ml_gen_constant: no_tag") }.
ml_gen_constant(unshared_tag(_), _, _) -->
	{ error("ml_gen_constant: unshared_tag") }.
ml_gen_constant(shared_remote_tag(_, _), _, _) -->
	{ error("ml_gen_constant: shared_remote_tag") }.
ml_gen_constant(pred_closure_tag(_, _, _), _, _) -->
	{ error("ml_gen_constant: pred_closure_tag") }.

%-----------------------------------------------------------------------------%

:- pred ml_gen_closure(pred_id, proc_id, lambda_eval_method, prog_var,
		prog_vars, list(uni_mode), how_to_construct, prog_context,
		mlds__defns, mlds__statements, ml_gen_info, ml_gen_info).
:- mode ml_gen_closure(in, in, in, in, in, in, in, in, out, out, in, out)
		is det.

ml_gen_closure(PredId, ProcId, EvalMethod, Var, ArgVars, ArgModes,
		HowToConstruct, Context, MLDS_Decls, MLDS_Statements) -->
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
		{ sorry(this_file, "`aditi_bottom_up' closures") }
	;
		{ EvalMethod = (aditi_top_down) },
		% XXX not yet implemented
		{ sorry(this_file, "`aditi_top_down' closures") }
	),

	%
	% Generate a dummy value for the closure layout
	% (we do this just to match the structure used
	% by the LLDS closure representation)
	%
	{ mercury_private_builtin_module(PrivateBuiltinModule) },
	{ MLDS_PrivateBuiltinModule = mercury_module_name_to_mlds(
		PrivateBuiltinModule) },
	{ ClosureLayoutType = mlds__class_type(qual(MLDS_PrivateBuiltinModule,
			"closure_layout"), 0, mlds__class) },
	{ ClosureLayoutRval = const(null(ClosureLayoutType)) },

	%
	% Generate a wrapper function which just unboxes the
	% arguments and then calls the specified procedure,
	% and put the address of the wrapper function in the closure.
	%
	% ml_gen_closure_wrapper will insert the wrapper function in the
	% extra_defns field in the ml_gen_info; ml_gen_proc will extract
	% it and will insert it before the mlds__defn for the current
	% procedure.
	%
	{ Offset = ml_closure_arg_offset },
	{ list__length(ArgVars, NumArgs) },
	ml_gen_closure_wrapper(PredId, ProcId, Offset, NumArgs,
		Context, WrapperFuncRval, WrapperFuncType),

	%
	% Compute the rval which holds the number of arguments
	%
	{ NumArgsRval = const(int_const(NumArgs)) },
	{ NumArgsType = mlds__native_int_type },

	%
	% the pointer will not be tagged (i.e. the tag will be zero)
	%
	{ Tag = 0 },
	{ CtorDefn = ctor_id("<closure>", 0) },
	{ QualifiedCtorId = qual(MLDS_PrivateBuiltinModule, CtorDefn) },

	%
	% put all the extra arguments of the closure together
	%
	{ ExtraArgRvals = [ClosureLayoutRval, WrapperFuncRval, NumArgsRval] },
	{ ExtraArgTypes = [ClosureLayoutType, WrapperFuncType, NumArgsType] },

	%
	% generate a `new_object' statement (or static constant)
	% for the closure
	%
	ml_gen_new_object(no, Tag, QualifiedCtorId, Var, ExtraArgRvals, 
		ExtraArgTypes, ArgVars, ArgModes, HowToConstruct, Context,
		MLDS_Decls, MLDS_Statements).

	%
	% ml_gen_closure_wrapper:
	% 	see comment in interface section for details.
	% 
	% This is used to create wrappers both for ordinary closures and
	% also for type class methods.
	%
	% The generated function will be of the following form:
	%
	%	foo_wrapper(void *closure_arg,
	%			MR_Box wrapper_arg1, MR_Box *wrapper_arg2,
	%			..., MR_Box wrapper_argn)
	%	{
	%		FooClosure *closure;
	%		...
	%		/* declarations needed for converting output args */
	%		Arg2Type conv_arg2;
	%		...
	% #if MODEL_SEMI
	%		bool succeeded;
	% #endif
	%		
	%		closure = closure_arg; 	/* XXX should add cast */
	%
	%	    CONJ(code_model, 
	%		/* call function, boxing/unboxing inputs if needed */
	%		foo(closure->f1, unbox(closure->f2), ...,
	%			unbox(wrapper_arg1), &conv_arg2,
	%			wrapper_arg3, ...);
	%	    ,
	%		/* box output arguments */
	%		*wrapper_arg2 = box(conv_arg2);
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
	%			unbox(wrapper_arg1), &conv_arg2,
	%			wrapper_arg3, ...);
	%
	%		/* box output arguments */
	%		*wrapper_arg2 = box(conv_arg2);
	%		...
	% #elif MODEL_SEMI
	%		/* call function, boxing/unboxing inputs if needed */
	%		succeeded = foo(closure->f1, unbox(closure->f2), ...,
	%			unbox(wrapper_arg1), &conv_arg2,
	%			wrapper_arg3, ...);
	%		
	%		if (succeeded) {
	%			/* box output arguments */
	%			*wrapper_arg2 = box(conv_arg2);
	%			...
	%		}
	%
	%		return succeeded;
	%	}
	% #else /* MODEL_NON */
	%		foo_1() {
	%			/* box output arguments */
	%			*wrapper_arg2 = box(conv_arg2);
	%			...
	%			(*succ_cont)();
	%		}
	%			
	%		/* call function, boxing/unboxing inputs if needed */
	%		foo(closure->f1, unbox(closure->f2), ...,
	%			unbox(wrapper_arg1), &conv_arg2,
	%			wrapper_arg3, ...,
	%			foo_1);
	% #endif
	%
ml_gen_closure_wrapper(PredId, ProcId, Offset, NumClosureArgs,
		Context, WrapperFuncRval, WrapperFuncType) -->
	%
	% grab the relevant information about the called procedure
	%
	=(Info),
	{ ml_gen_info_get_module_info(Info, ModuleInfo) },
	{ module_info_pred_proc_info(ModuleInfo, PredId, ProcId,
		PredInfo, ProcInfo) },
	{ pred_info_get_is_pred_or_func(PredInfo, PredOrFunc) },
	{ proc_info_headvars(ProcInfo, ProcHeadVars) },
	{ proc_info_argmodes(ProcInfo, ProcArgModes) },
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
	%	MR_Box wrapper_arg1, MR_Box *wrapper_arg2, ...,
	%	MR_Box wrapper_argn)
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
	{ WrapperParams0 = ml_gen_params(ModuleInfo, WrapperHeadVarNames,
		WrapperBoxedArgTypes, WrapperArgModes, PredOrFunc, CodeModel) },

	% then insert the `closure_arg' parameter
	{ ClosureArgType = mlds__generic_type },
	{ ClosureArg = data(var(
		var_name("closure_arg", no))) - ClosureArgType },
	{ WrapperParams0 = mlds__func_params(WrapperArgs0, WrapperRetType) },
	{ WrapperParams = mlds__func_params([ClosureArg | WrapperArgs0],
		WrapperRetType) },

	% also compute the lvals for the parameters,
	% and local declarations for any --copy-out output parameters
	ml_gen_wrapper_arg_lvals(WrapperHeadVarNames, WrapperBoxedArgTypes,
		WrapperArgModes, PredOrFunc, CodeModel, Context,
		WrapperHeadVarDecls, WrapperHeadVarLvals, WrapperCopyOutLvals),

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
	{ ClosureName = mlds__var_name("closure", no) },
	{ ClosureArgName = mlds__var_name("closure_arg", no) },
	{ MLDS_Context = mlds__make_context(Context) },
	{ ClosureType = mlds__generic_type },
	{ ClosureDecl = ml_gen_mlds_var_decl(var(ClosureName),
		ClosureType, MLDS_Context) },
	ml_gen_var_lval(ClosureName, ClosureType, ClosureLval),
	ml_gen_var_lval(ClosureArgName, ClosureArgType, ClosureArgLval),
	{ InitClosure = ml_gen_assign(ClosureLval, lval(ClosureArgLval),
		Context) },

	%
	% if the wrapper function is model_non, then
	% set up the initial success continuation;
	% this is needed by ml_gen_call which we call below
	%
	( { CodeModel = model_non } ->
		{ module_info_globals(ModuleInfo, Globals) },
		{ globals__lookup_bool_option(Globals, nondet_copy_out,
			NondetCopyOut) },
		( { NondetCopyOut = yes } ->
			{ map__from_corresponding_lists(WrapperHeadVarLvals,
				WrapperBoxedArgTypes, WrapperBoxedVarTypes) },
			{ WrapperOutputLvals = select_output_vars(ModuleInfo,
				WrapperHeadVarLvals, WrapperArgModes,
				WrapperBoxedVarTypes) },
			{ WrapperOutputTypes = map__apply_to_list(
				WrapperOutputLvals, WrapperBoxedVarTypes) },
			ml_initial_cont(WrapperOutputLvals, WrapperOutputTypes,
				InitialCont)
		;
			ml_initial_cont([], [], InitialCont)
		),
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
	%		unbox(wrapper_arg1), &conv_arg2, wrapper_arg3, ...
	%	);
	%
	ml_gen_closure_field_lvals(ClosureLval, Offset, 1, NumClosureArgs,
		ClosureArgLvals),
	{ CallLvals = list__append(ClosureArgLvals, WrapperHeadVarLvals) },
	ml_gen_call(PredId, ProcId, ProcHeadVarNames, CallLvals,
		ProcBoxedArgTypes, CodeModel, Context, Decls0, Statements0),

	% insert the stuff to declare and initialize the closure
	{ Decls1 = [ClosureDecl | Decls0] },
	{ Statements1 = [InitClosure | Statements0] },

	%
	% For semidet code, add the declaration `bool succeeded;'
	%
	( { CodeModel = model_semi } ->
		{ SucceededVarDecl = ml_gen_succeeded_var_decl(MLDS_Context) },
		{ Decls2 = [SucceededVarDecl | Decls1] }
	;
		{ Decls2 = Decls1 }
	),

	% Add an appropriate `return' statement
	ml_append_return_statement(CodeModel, WrapperCopyOutLvals, Context,
		Statements1, Statements),

	%
	% Insert the local declarations of the wrapper's output arguments,
	% if any (this is needed for `--nondet-copy-out')
	%
	{ Decls = list__append(WrapperHeadVarDecls, Decls2) },

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
	ml_gen_new_func_label(yes(WrapperParams), WrapperFuncName,
		WrapperFuncRval),
	ml_gen_wrapper_func(WrapperFuncName, WrapperParams, Context,
		WrapperFuncBody, WrapperFunc),
	{ WrapperFuncType = mlds__func_type(WrapperParams) },
	ml_gen_info_add_extra_defn(WrapperFunc).

:- pred ml_gen_wrapper_func(ml_label_func, mlds__func_params, prog_context,
		mlds__statement, mlds__defn, ml_gen_info, ml_gen_info).
:- mode ml_gen_wrapper_func(in, in, in, in, out, in, out) is det.

ml_gen_wrapper_func(FuncLabel, FuncParams, Context, Statement, Func) -->
	ml_gen_label_func(FuncLabel, FuncParams, Context, Statement, Func0),
	{ Func0 = mlds__defn(Name, Ctxt, DeclFlags0, Defn) },
	{ DeclFlags1 = set_per_instance(DeclFlags0, one_copy) },
	{ DeclFlags = set_access(DeclFlags1, private) },
	{ Func = mlds__defn(Name, Ctxt, DeclFlags, Defn) }.

:- func ml_gen_wrapper_head_var_names(int, int) = list(mlds__var_name).
ml_gen_wrapper_head_var_names(Num, Max) = Names :-
	( Num > Max ->
		Names = []
	;
		Name = string__format("wrapper_arg_%d", [i(Num)]),
		Names1 = ml_gen_wrapper_head_var_names(Num + 1, Max),
		Names = [mlds__var_name(Name, no) | Names1]
	).

	% ml_gen_wrapper_arg_lvals(HeadVarNames, Types, ArgModes,
	%		PredOrFunc, CodeModel, LocalVarDefns, HeadVarLvals):
	%	Generate lvals for the specified head variables
	%	passed in the specified modes.
	%	Also generate local definitions for output variables,
	%	if those output variables will be copied out,
	%	rather than passed by reference.
	%
:- pred ml_gen_wrapper_arg_lvals(list(var_name), list(prog_type), list(mode),
		pred_or_func, code_model, prog_context,
		list(mlds__defn), list(mlds__lval), list(mlds__lval),
		ml_gen_info, ml_gen_info).
:- mode ml_gen_wrapper_arg_lvals(in, in, in, in, in, in, out, out, out, in, out)
		is det.

ml_gen_wrapper_arg_lvals(Names, Types, Modes, PredOrFunc, CodeModel, Context,
		Defns, Lvals, CopyOutLvals) -->
	(
		{ Names = [], Types = [], Modes = [] }
	->
		{ Lvals = [] },
		{ CopyOutLvals = [] },
		{ Defns = [] }
	;
		{ Names = [Name | Names1] },
		{ Types = [Type | Types1] },
		{ Modes = [Mode | Modes1] }
	->
		ml_gen_wrapper_arg_lvals(Names1, Types1, Modes1,
			PredOrFunc, CodeModel, Context,
			Defns1, Lvals1, CopyOutLvals1),
		ml_gen_type(Type, MLDS_Type),
		ml_gen_var_lval(Name, MLDS_Type, VarLval),
		=(Info),
		{ ml_gen_info_get_module_info(Info, ModuleInfo) },
		{ mode_to_arg_mode(ModuleInfo, Mode, Type, ArgMode) },
		( { ArgMode = top_in } ->
			{ Lval = VarLval },
			{ CopyOutLvals = CopyOutLvals1 },
			{ Defns = Defns1 }
		;
			%
			% handle output variables
			%
			ml_gen_info_get_globals(Globals),
			{ CopyOut = get_copy_out_option(Globals, CodeModel) },
			(
				{
					CopyOut = yes
				;
					% for model_det functions,
					% output mode function results
					% are mapped to MLDS return values
					PredOrFunc = function,
					CodeModel = model_det,
					ArgMode = top_out,
					Types1 = [],
					\+ type_util__is_dummy_argument_type(
						Type)
				}
			->
				%
				% output arguments are copied out,
				% so we need to generate a local declaration
				% for them here
				%
				{ Lval = VarLval },
				( { type_util__is_dummy_argument_type(Type) } ->
					{ CopyOutLvals = CopyOutLvals1 },
					{ Defns = Defns1 }
				;
					{ CopyOutLvals = [Lval |
						CopyOutLvals1] },
					ml_gen_local_for_output_arg(Name, Type,
						Context, Defn),
					{ Defns = [Defn | Defns1] }
				)
			;
				%
				% output arguments are passed by reference,
				% so we need to dereference them
				%
				{ Lval = mem_ref(lval(VarLval), MLDS_Type) },
				{ CopyOutLvals = CopyOutLvals1 },
				{ Defns = Defns1 }
			)
		),
		{ Lvals = [Lval | Lvals1] }
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
			% XXX these types might not be right
		{ FieldLval = field(yes(0), lval(ClosureLval), FieldId,
			mlds__generic_type, mlds__generic_type) },
		%
		% recursively handle the remaining fields
		%
		ml_gen_closure_field_lvals(ClosureLval, Offset, ArgNum + 1,
			NumClosureArgs, ClosureArgLvals0),
		{ ClosureArgLvals = [FieldLval | ClosureArgLvals0] }
	).

:- pred ml_gen_local_for_output_arg(var_name, prog_type, prog_context,
		mlds__defn, ml_gen_info, ml_gen_info).
:- mode ml_gen_local_for_output_arg(in, in, in, out, in, out) is det.

ml_gen_local_for_output_arg(VarName, Type, Context, LocalVarDefn) -->
	%
	% Generate a declaration for a corresponding local variable.
	%
	=(MLDSGenInfo),
	{ ml_gen_info_get_module_info(MLDSGenInfo, ModuleInfo) },
	{ LocalVarDefn = ml_gen_var_decl(VarName, Type,
		mlds__make_context(Context), ModuleInfo) }.

%-----------------------------------------------------------------------------%
		
	% convert a cons_id for a given type to a cons_tag
ml_cons_id_to_tag(ConsId, Type, Tag) -->
	=(Info),
	{ ml_gen_info_get_module_info(Info, ModuleInfo) },
	{ code_util__cons_id_to_tag(ConsId, Type, ModuleInfo, Tag) }.

	% generate code to construct a new object
:- pred ml_gen_compound(mlds__tag, maybe(int), cons_id, prog_var, prog_vars,
		list(uni_mode), how_to_construct, prog_context,
		mlds__defns, mlds__statements, ml_gen_info, ml_gen_info).
:- mode ml_gen_compound(in, in, in, in, in, in, in, in, out, out, in, out)
		is det.

ml_gen_compound(Tag, MaybeSecondaryTag, ConsId, Var, ArgVars, ArgModes,
		HowToConstruct, Context, MLDS_Decls, MLDS_Statements) -->
	ml_cons_name(ConsId, CtorName),
	% 
	% If there is a secondary tag, it goes in the first field
	%
	{ MaybeSecondaryTag = yes(SecondaryTag) ->
		SecondaryTagRval = const(int_const(SecondaryTag)),
		SecondaryTagType = mlds__native_int_type,
		ExtraRvals = [SecondaryTagRval],
		ExtraArgTypes = [SecondaryTagType]
	;
		ExtraRvals = [],
		ExtraArgTypes = []
	},
	ml_gen_new_object(yes(ConsId), Tag, CtorName, Var,
			ExtraRvals, ExtraArgTypes, ArgVars, ArgModes,
			HowToConstruct, Context, MLDS_Decls, MLDS_Statements).

	%
	% ml_gen_new_object:
	%	Generate a `new_object' statement, or a static constant,
	%	depending on the value of the how_to_construct argument.
	%	The `ExtraRvals' and `ExtraTypes' arguments specify
	%	additional constants to insert at the start of the
	%	argument list.
	%
:- pred ml_gen_new_object(maybe(cons_id), mlds__tag, ctor_name, prog_var,
		list(mlds__rval), list(mlds__type), prog_vars,
		list(uni_mode), how_to_construct,
		prog_context, mlds__defns, mlds__statements,
		ml_gen_info, ml_gen_info).
:- mode ml_gen_new_object(in, in, in, in, in, in, in, in, in, in, out, out,
		in, out) is det.

ml_gen_new_object(MaybeConsId, Tag, CtorName, Var, ExtraRvals, ExtraTypes,
		ArgVars, ArgModes, HowToConstruct, Context,
		MLDS_Decls, MLDS_Statements) -->
	%
	% Determine the variable's type and lval,
	% the tag to use, and the types of the argument vars.
	%
	ml_variable_type(Var, Type),
	ml_gen_type(Type, MLDS_Type),
	ml_gen_var(Var, VarLval),
	{ Tag = 0 ->
		MaybeTag = no
	;
		MaybeTag = yes(Tag)
	},
	ml_variable_types(ArgVars, ArgTypes),
	list__map_foldl(ml_gen_type, ArgTypes, MLDS_ArgTypes0),

	(
		{ HowToConstruct = construct_dynamically },

		%
		% Generate rvals for the arguments
		%
		ml_gen_var_list(ArgVars, ArgLvals),
		=(Info),
		{ ml_gen_info_get_module_info(Info, ModuleInfo) },
		{ ml_gen_cons_args(ArgLvals, ArgTypes, ArgModes, ModuleInfo,
			ArgRvals0) },

		%
		% Insert the extra rvals at the start
		%
		{ list__append(ExtraRvals, ArgRvals0, ArgRvals) },
		{ list__append(ExtraTypes, MLDS_ArgTypes0, MLDS_ArgTypes) },

		%
		% Compute the number of bytes to allocate
		%
		{ list__length(ArgRvals, NumArgs) },
		{ SizeInWordsRval = const(int_const(NumArgs)) },
		
		%
		% Generate a `new_object' statement to dynamically allocate
		% the memory for this term from the heap.  The `new_object'
		% statement will also initialize the fields of this term
		% with boxed versions of the specified arguments.
		%
		{ MakeNewObject = new_object(VarLval, MaybeTag, MLDS_Type,
			yes(SizeInWordsRval), yes(CtorName), ArgRvals,
			MLDS_ArgTypes) },
		{ MLDS_Stmt = atomic(MakeNewObject) },
		{ MLDS_Statement = mlds__statement(MLDS_Stmt,
			mlds__make_context(Context)) },
		{ MLDS_Statements = [MLDS_Statement] },
		{ MLDS_Decls = [] }
	;
		{ HowToConstruct = construct_statically(StaticArgs) },

		%
		% Generate rvals for the arguments
		%
		ml_gen_static_const_arg_list(ArgVars, StaticArgs, ArgRvals0),

		%
		% Insert the extra rvals at the start
		%
		{ list__append(ExtraRvals, ArgRvals0, ArgRvals1) },
		{ list__append(ExtraTypes, MLDS_ArgTypes0, MLDS_ArgTypes) },

		%
		% Box all the arguments
		%
		ml_gen_box_const_rval_list(MLDS_ArgTypes, ArgRvals1,
			Context, BoxConstDefns, ArgRvals),

		%
		% Generate a local static constant for this term.
		% XXX Using mlds__array_type(mlds__generic_type) is probably 
		% wrong when `--high-level-data' is enabled.
		%
		ml_gen_static_const_name(Var, ConstName),
		{ ConstType = mlds__array_type(mlds__generic_type) },
		{ ArgInits = list__map(func(X) = init_obj(X), ArgRvals) },
		{ Initializer = init_array(ArgInits) },
		{ ConstDefn = ml_gen_static_const_defn(ConstName, ConstType,
			Initializer, Context) },

		%
		% Assign the address of the local static constant to
		% the variable.
		%
		ml_gen_static_const_addr(Var, ConstType, ConstAddrRval),
		{ MaybeTag = no ->
			TaggedRval = ConstAddrRval
		;
			TaggedRval = mkword(Tag, ConstAddrRval)
		},
		{ Rval = unop(cast(MLDS_Type), TaggedRval) },
		{ AssignStatement = ml_gen_assign(VarLval, Rval, Context) },
		{ MLDS_Decls = list__append(BoxConstDefns, [ConstDefn]) },
		{ MLDS_Statements = [AssignStatement] }
	;
		{ HowToConstruct = reuse_cell(CellToReuse) },
		{ CellToReuse = cell_to_reuse(ReuseVar, ReuseConsIds, _) },

		{ MaybeConsId = yes(ConsId0) ->
			ConsId = ConsId0
		;
			error("ml_gen_new_object: unknown cons id")
		},

		list__map_foldl(
			(pred(ReuseConsId::in, ReusePrimTag::out,
					in, out) is det -->
				ml_variable_type(ReuseVar, ReuseType),
				ml_cons_id_to_tag(ReuseConsId, ReuseType,
						ReuseConsIdTag),
				{ ml_tag_offset_and_argnum(ReuseConsIdTag,
						ReusePrimTag,
						_ReuseOffSet, _ReuseArgNum) }
			), ReuseConsIds, ReusePrimaryTags0),
		{ list__remove_dups(ReusePrimaryTags0, ReusePrimaryTags) },

		ml_cons_id_to_tag(ConsId, Type, ConsIdTag),
		ml_field_names_and_types(Type, ConsId, ArgTypes, Fields),
		{ ml_tag_offset_and_argnum(ConsIdTag,
				PrimaryTag, OffSet, ArgNum) },

		ml_gen_var(Var, Var1Lval),
		ml_gen_var(ReuseVar, Var2Lval),

		{ list__filter((pred(ReuseTag::in) is semidet :-
				ReuseTag \= PrimaryTag
			), ReusePrimaryTags, DifferentTags) },
		{ DifferentTags = [] ->
			Var2Rval = lval(Var2Lval)
		; DifferentTags = [ReusePrimaryTag] ->
				% The body operator is slightly more
				% efficient than the strip_tag operator so
				% we use it when the old tag is known.
			Var2Rval = mkword(PrimaryTag,
					binop(body, lval(Var2Lval),
					ml_gen_mktag(ReusePrimaryTag)))
		;
			Var2Rval = mkword(PrimaryTag,
					unop(std_unop(strip_tag),
					lval(Var2Lval)))
		},

		{ MLDS_Statement = ml_gen_assign(Var1Lval, Var2Rval, Context) },

		%
		% For each field in the construction unification we need
		% to generate an rval.
		% XXX we do more work then we need to here, as some of
		% the cells may already contain the correct values.
		%
		ml_gen_unify_args(ConsId, ArgVars, ArgModes, ArgTypes,
				Fields, Type, VarLval, OffSet,
				ArgNum, PrimaryTag, Context, MLDS_Statements0),

		{ MLDS_Decls = [] },
		{ MLDS_Statements = [MLDS_Statement | MLDS_Statements0] }
	).

:- func ml_gen_mktag(int) = mlds__rval.
ml_gen_mktag(Tag) = unop(std_unop(mktag), const(int_const(Tag))).

:- pred ml_gen_box_const_rval_list(list(mlds__type), list(mlds__rval),
		prog_context, mlds__defns, list(mlds__rval),
		ml_gen_info, ml_gen_info).
:- mode ml_gen_box_const_rval_list(in, in, in, out, out, in, out) is det.

ml_gen_box_const_rval_list([], [], _, [], []) --> [].
ml_gen_box_const_rval_list([Type | Types], [Rval | Rvals], Context,
		ConstDefns, [BoxedRval | BoxedRvals]) -->
	ml_gen_box_const_rval(Type, Rval, Context, ConstDefns1, BoxedRval),
	ml_gen_box_const_rval_list(Types, Rvals, Context, ConstDefns2,
		BoxedRvals),
	{ ConstDefns = list__append(ConstDefns1, ConstDefns2) }.
ml_gen_box_const_rval_list([], [_|_], _, _, _) -->
	{ error("ml_gen_box_const_rval_list: length mismatch") }.
ml_gen_box_const_rval_list([_|_], [], _, _, _) -->
	{ error("ml_gen_box_const_rval_list: length mismatch") }.

:- pred ml_gen_box_const_rval(mlds__type, mlds__rval, prog_context,
		mlds__defns, mlds__rval, ml_gen_info, ml_gen_info).
:- mode ml_gen_box_const_rval(in, in, in, out, out, in, out) is det.

ml_gen_box_const_rval(Type, Rval, Context, ConstDefns, BoxedRval) -->
	(
		{ Type = mercury_type(term__variable(_), _)
		; Type = mlds__generic_type
		}
	->
		{ BoxedRval = Rval },
		{ ConstDefns = [] }
	;
		%
		% We need to handle floats specially,
		% since boxed floats normally get heap allocated,
		% whereas for other types boxing is just a cast
		% (casts are OK in static initializers,
		% but calls to malloc() are not).
		%
		{ Type = mercury_type(term__functor(term__atom("float"),
				[], _), _)
		; Type = mlds__native_float_type
		}
	->
		%
		% Generate a local static constant for this float
		%
		ml_gen_info_new_const(SequenceNum),
		=(MLDSGenInfo),
		{ ml_gen_info_get_pred_id(MLDSGenInfo, PredId) },
		{ ml_gen_info_get_proc_id(MLDSGenInfo, ProcId) },
		{ pred_id_to_int(PredId, PredIdNum) },
		{ proc_id_to_int(ProcId, ProcIdNum) },
		{ ConstName = mlds__var_name(string__format("float_%d_%d_%d",
			[i(PredIdNum), i(ProcIdNum), i(SequenceNum)]), no) },
		{ Initializer = init_obj(Rval) },
		{ ConstDefn = ml_gen_static_const_defn(ConstName, Type,
			Initializer, Context) },
		{ ConstDefns = [ConstDefn] },
		%
		% Return as the boxed rval the address of that constant,
		% cast to mlds__generic_type
		%
		ml_gen_var_lval(ConstName, Type, ConstLval),
		{ ConstAddrRval = mem_addr(ConstLval) },
		{ BoxedRval = unop(cast(mlds__generic_type), ConstAddrRval) }
	;
		{ BoxedRval = unop(box(Type), Rval) },
		{ ConstDefns = [] }
	).
	
:- pred ml_gen_static_const_arg_list(list(prog_var), list(static_cons),
		list(mlds__rval), ml_gen_info, ml_gen_info).
:- mode ml_gen_static_const_arg_list(in, in, out, in, out) is det.

ml_gen_static_const_arg_list([], [], []) --> [].
ml_gen_static_const_arg_list([Var | Vars], [StaticCons | StaticConses],
		[Rval | Rvals]) -->
	ml_gen_static_const_arg(Var, StaticCons, Rval),
	ml_gen_static_const_arg_list(Vars, StaticConses, Rvals).
ml_gen_static_const_arg_list([_|_], [], _) -->
	{ error("ml_gen_static_const_arg_list: length mismatch") }.
ml_gen_static_const_arg_list([], [_|_], _) -->
	{ error("ml_gen_static_const_arg_list: length mismatch") }.

	% Generate the name of the local static constant
	% for a given variable. 
	%
:- pred ml_gen_static_const_name(prog_var, mlds__var_name,
		ml_gen_info, ml_gen_info).
:- mode ml_gen_static_const_name(in, out, in, out) is det.
ml_gen_static_const_name(Var, ConstName) -->
	ml_gen_info_new_const(SequenceNum),
	ml_gen_info_set_const_num(Var, SequenceNum),
	=(MLDSGenInfo),
	{ ml_gen_info_get_varset(MLDSGenInfo, VarSet) },
	{ VarName = ml_gen_var_name(VarSet, Var) },
	ml_format_static_const_name(ml_var_name_to_string(VarName),
		SequenceNum, ConstName).

:- pred ml_lookup_static_const_name(prog_var, mlds__var_name,
		ml_gen_info, ml_gen_info).
:- mode ml_lookup_static_const_name(in, out, in, out) is det.
ml_lookup_static_const_name(Var, ConstName) -->
	ml_gen_info_lookup_const_num(Var, SequenceNum),
	=(MLDSGenInfo),
	{ ml_gen_info_get_varset(MLDSGenInfo, VarSet) },
	{ VarName = ml_gen_var_name(VarSet, Var) },
	ml_format_static_const_name(ml_var_name_to_string(VarName),
		SequenceNum, ConstName).

	% Generate an rval containing the address of the local static constant
	% for a given variable.
	%
:- pred ml_gen_static_const_addr(prog_var, mlds__type, mlds__rval,
		ml_gen_info, ml_gen_info).
:- mode ml_gen_static_const_addr(in, in, out, in, out) is det.
ml_gen_static_const_addr(Var, Type, ConstAddrRval) -->
	ml_lookup_static_const_name(Var, ConstName),
	ml_gen_var_lval(ConstName, Type, ConstLval),
	{ ConstAddrRval = mem_addr(ConstLval) }.

:- pred ml_cons_name(cons_id, ctor_name, ml_gen_info, ml_gen_info).
:- mode ml_cons_name(in, out, in, out) is det.

ml_cons_name(HLDS_ConsId, QualifiedConsId) -->
	( 
		{ HLDS_ConsId = cons(SymName, Arity),
	    	SymName = qualified(SymModuleName, ConsName) } 
	->
		{ ConsId = ctor_id(ConsName, Arity) },
		{ ModuleName = mercury_module_name_to_mlds(SymModuleName) }
	;
		{ hlds_out__cons_id_to_string(HLDS_ConsId, ConsName) },
		{ ConsId = ctor_id(ConsName, 0) },
		{ ModuleName = mercury_module_name_to_mlds(unqualified("")) }
	),
	{ QualifiedConsId = qual(ModuleName, ConsId) }.

:- pred ml_gen_cons_args(list(mlds__lval), list(prog_type),
		list(uni_mode), module_info, list(mlds__rval)).
:- mode ml_gen_cons_args(in, in, in, in, out) is det.

ml_gen_cons_args(Lvals, Types, Modes, ModuleInfo, Rvals) :-
	( ml_gen_cons_args_2(Lvals, Types, Modes, ModuleInfo, Rvals0) ->
		Rvals = Rvals0
	;
		error("ml_gen_cons_args: length mismatch")
	).

	% Create a list of rvals for the arguments
	% for a construction unification.  For each argument which
	% is input to the construction unification, we produce the
	% corresponding lval, but if the argument is free,
	% we produce a null value.

:- pred ml_gen_cons_args_2(list(mlds__lval), list(prog_type),
		list(uni_mode), module_info, list(mlds__rval)).
:- mode ml_gen_cons_args_2(in, in, in, in, out) is semidet.

ml_gen_cons_args_2([], [], [], _, []).
ml_gen_cons_args_2([Lval|Lvals], [Type|Types], [UniMode|UniModes],
			ModuleInfo, [Rval|Rvals]) :-
	UniMode = ((_LI - RI) -> (_LF - RF)),
	( mode_to_arg_mode(ModuleInfo, (RI -> RF), Type, top_in) ->
		Rval = lval(Lval)
	;
		Rval = const(null(mercury_type_to_mlds_type(ModuleInfo, Type)))
	),
	ml_gen_cons_args_2(Lvals, Types, UniModes, ModuleInfo, Rvals).

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
%		<do (X => f(A1, A2, ...))>
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
		{ Tag = deep_profiling_proc_static_tag(_) },
		{ MLDS_Statements = [] }
	;
		{ Tag = no_tag },
		( { Args = [Arg], Modes = [Mode] } ->
			ml_variable_type(Arg, ArgType),
			ml_gen_var(Arg, ArgLval),
			ml_gen_var(Var, VarLval),
			ml_gen_sub_unify(Mode, ArgLval, ArgType, VarLval, Type,
				Context, [], MLDS_Statements)
		;
			{ error("ml_code_gen: no_tag: arity != 1") }
		)
	;
		{ Tag = unshared_tag(UnsharedTag) },
		ml_gen_var(Var, VarLval),
		ml_variable_types(Args, ArgTypes),
		ml_field_names_and_types(Type, ConsId, ArgTypes, Fields),
		{ ml_tag_offset_and_argnum(Tag, _, OffSet, ArgNum) },
		ml_gen_unify_args(ConsId, Args, Modes, ArgTypes, Fields, Type,
				VarLval, OffSet, ArgNum,
				UnsharedTag, Context, MLDS_Statements)
	;
		{ Tag = shared_remote_tag(PrimaryTag, _SecondaryTag) },
		ml_gen_var(Var, VarLval),
		ml_variable_types(Args, ArgTypes),
		ml_field_names_and_types(Type, ConsId, ArgTypes, Fields),
		{ ml_tag_offset_and_argnum(Tag, _, OffSet, ArgNum) },
		ml_gen_unify_args(ConsId, Args, Modes, ArgTypes, Fields, Type,
				VarLval, OffSet, ArgNum,
				PrimaryTag, Context, MLDS_Statements)
	;
		{ Tag = shared_local_tag(_Bits1, _Num1) },
		{ MLDS_Statements = [] } % if this is det, then nothing happens
	).

	% Calculate the integer offset used to reference the first field
	% of a structure for lowlevel data or the first argument number
	% to access the field using the highlevel data representation.
	% Abort if the tag indicates that the data doesn't have any
	% fields.
:- pred ml_tag_offset_and_argnum(cons_tag::in, tag_bits::out,
		int::out, int::out) is det.

ml_tag_offset_and_argnum(Tag, TagBits, OffSet, ArgNum) :-
	(
		Tag = unshared_tag(UnsharedTag),
		TagBits = UnsharedTag,
		OffSet = 0,
		ArgNum = 1
	;
		Tag = shared_remote_tag(PrimaryTag, _SecondaryTag),
		TagBits = PrimaryTag,
		OffSet = 1,
		ArgNum = 1
	;
		Tag = string_constant(_String),
		error("ml_tag_offset_and_argnum")
	;
		Tag = int_constant(_Int),
		error("ml_tag_offset_and_argnum")
	;
		Tag = float_constant(_Float),
		error("ml_tag_offset_and_argnum")
	;
		Tag = pred_closure_tag(_, _, _),
		error("ml_tag_offset_and_argnum")
	;
		Tag = code_addr_constant(_, _),
		error("ml_tag_offset_and_argnum")
	;
		Tag = type_ctor_info_constant(_, _, _),
		error("ml_tag_offset_and_argnum")
	;
		Tag = base_typeclass_info_constant(_, _, _),
		error("ml_tag_offset_and_argnum")
	;
		Tag = tabling_pointer_constant(_, _),
		error("ml_tag_offset_and_argnum")
	;
		Tag = deep_profiling_proc_static_tag(_),
		error("ml_tag_offset_and_argnum")
	;
		Tag = no_tag,
		error("ml_tag_offset_and_argnum")
	;
		Tag = shared_local_tag(_Bits1, _Num1),
		error("ml_tag_offset_and_argnum")
	).


	% Given a type and a cons_id, and also the types of the actual
	% arguments of that cons_id in some particular use of it,
	% look up the original types of the fields of that cons_id from
	% the type definition.  Note that the field types need not be
	% the same as the actual argument types; for polymorphic types,
	% the types of the actual arguments can be an instance of the
	% field types.
	%
:- pred ml_field_names_and_types(prog_type, cons_id, list(prog_type),
		list(constructor_arg), ml_gen_info, ml_gen_info).
:- mode ml_field_names_and_types(in, in, in, out, in, out) is det.

ml_field_names_and_types(Type, ConsId, ArgTypes, Fields) -->
	%
	% Lookup the field types for the arguments of this cons_id
	%
	{ MakeUnnamedField = (func(FieldType) = no - FieldType) },
	(
		{ type_is_tuple(Type, _) },
		{ list__length(ArgTypes, TupleArity) }
	->
		% The argument types for tuples are unbound type variables.
		{ varset__init(TypeVarSet0) },
		{ varset__new_vars(TypeVarSet0, TupleArity,
			TVars, _TypeVarSet) },
		{ term__var_list_to_term_list(TVars, FieldTypes) },
		{ Fields = list__map(MakeUnnamedField, FieldTypes) }
	;
		=(Info),
		{ ml_gen_info_get_module_info(Info, ModuleInfo) },
		{ type_util__get_type_and_cons_defn(ModuleInfo, Type, ConsId,
				_TypeDefn, ConsDefn) },
		{ ConsDefn = hlds_cons_defn(_, _, Fields0, _, _) },
		%
		% Add the fields for any type_infos and/or typeclass_infos
		% inserted for existentially quantified data types.
		% For these, we just copy the types from the ArgTypes.
		%
		{ NumArgs = list__length(ArgTypes) },
		{ NumFieldTypes0 = list__length(Fields0) },
		{ NumExtraTypes = NumArgs - NumFieldTypes0 },
		{ ExtraFieldTypes = list__take_upto(NumExtraTypes, ArgTypes) },
		{ ExtraFields = list__map(MakeUnnamedField, ExtraFieldTypes) },
		{ Fields = list__append(ExtraFields, Fields0) }
	).

:- pred ml_gen_unify_args(cons_id, prog_vars, list(uni_mode), list(prog_type),
		list(constructor_arg), prog_type, mlds__lval, int, int,
		mlds__tag, prog_context, mlds__statements,
		ml_gen_info, ml_gen_info).
:- mode ml_gen_unify_args(in, in, in, in, in, in, in, in, in, in, in, out,
		in, out) is det.

ml_gen_unify_args(ConsId, Args, Modes, ArgTypes, Fields, VarType, VarLval,
		Offset, ArgNum, PrimaryTag, Context, MLDS_Statements) -->
	(
		ml_gen_unify_args_2(ConsId, Args, Modes, ArgTypes, Fields,
			VarType, VarLval, Offset, ArgNum, PrimaryTag, Context,
			[], MLDS_Statements0)
	->
		{ MLDS_Statements = MLDS_Statements0 }
	;
		{ error("ml_gen_unify_args: length mismatch") }
	).

:- pred ml_gen_unify_args_2(cons_id, prog_vars, list(uni_mode), list(prog_type),
		list(constructor_arg), prog_type, mlds__lval, int, int,
		mlds__tag, prog_context, mlds__statements, mlds__statements,
		ml_gen_info, ml_gen_info).
:- mode ml_gen_unify_args_2(in, in, in, in, in, in, in, in, in, in, in, in, out,
		in, out) is semidet.

ml_gen_unify_args_2(_, [], [], [], _, _, _, _, _, _, _, Statements, Statements)
		--> [].
ml_gen_unify_args_2(ConsId, [Arg|Args], [Mode|Modes], [ArgType|ArgTypes],
		[Field|Fields], VarType, VarLval, Offset, ArgNum, PrimaryTag,
		Context, MLDS_Statements0, MLDS_Statements) -->
	{ Offset1 = Offset + 1 },
	{ ArgNum1 = ArgNum + 1 },
	ml_gen_unify_args_2(ConsId, Args, Modes, ArgTypes, Fields, VarType,
		VarLval, Offset1, ArgNum1, PrimaryTag, Context,
		MLDS_Statements0, MLDS_Statements1),
	ml_gen_unify_arg(ConsId, Arg, Mode, ArgType, Field, VarType, VarLval,
		Offset, ArgNum, PrimaryTag, Context,
		MLDS_Statements1, MLDS_Statements).

:- pred ml_gen_unify_arg(cons_id, prog_var, uni_mode, prog_type,
		constructor_arg, prog_type, mlds__lval, int, int, mlds__tag,
		prog_context, mlds__statements, mlds__statements,
		ml_gen_info, ml_gen_info).
:- mode ml_gen_unify_arg(in, in, in, in, in, in, in, in, in, in, in, in, out,
		in, out) is det.

ml_gen_unify_arg(ConsId, Arg, Mode, ArgType, Field, VarType, VarLval,
		Offset, ArgNum, PrimaryTag, Context,
		MLDS_Statements0, MLDS_Statements) -->
	{ Field = MaybeFieldName - FieldType },
	=(Info),
	{ ml_gen_info_get_module_info(Info, ModuleInfo) },
	{ module_info_globals(ModuleInfo, Globals) },
	{ globals__lookup_bool_option(Globals, highlevel_data,
		HighLevelData) },
	{
		%
		% With the low-level data representation,
		% we access all fields using offsets.
		%
		HighLevelData = no,
		FieldId = offset(const(int_const(Offset)))
	;
		%
		% With the high-level data representation,
		% we always used named fields, except for
		% tuple types.
		% 
		HighLevelData = yes,
		( type_is_tuple(VarType, _) ->
			FieldId = offset(const(int_const(Offset)))
		;
			FieldName = ml_gen_field_name(MaybeFieldName, ArgNum),
			(
				ConsId = cons(ConsName, ConsArity)
			->
				unqualify_name(ConsName, UnqualConsName),
				FieldId = ml_gen_field_id(VarType,
					UnqualConsName, ConsArity, FieldName)
			;
				error("ml_gen_unify_args: invalid cons_id")
			)
		)
	},
	{
		%
		% With the low-level data representation,
		% we store all fields as boxed, so we ignore the field
		% type from `Field' and instead generate a polymorphic
		% type BoxedFieldType which we use for the type of the field.
		% This type is used in the calls to
		% ml_gen_box_or_unbox_rval below to ensure that we
		% box values when storing them into fields and
		% unbox them when extracting them from fields.
		%
		% With the high-level data representation,
		% we don't box everything, but we still need
		% to box floating point fields.
		%
		(
			HighLevelData = no
		;
			HighLevelData = yes,
			ml_must_box_field_type(FieldType, ModuleInfo)
		)
	->
		varset__init(TypeVarSet0),
		varset__new_var(TypeVarSet0, TypeVar, _TypeVarSet),
		type_util__var(BoxedFieldType, TypeVar)
	;
		BoxedFieldType = FieldType
	},

		%
		% Generate lvals for the LHS and the RHS
		%
	ml_gen_type(VarType, MLDS_VarType),
	ml_gen_type(BoxedFieldType, MLDS_BoxedFieldType),
	{ FieldLval = field(yes(PrimaryTag), lval(VarLval), FieldId,
		MLDS_BoxedFieldType, MLDS_VarType) },
	ml_gen_var(Arg, ArgLval),

	%
	% Now generate code to unify them
	%
	ml_gen_sub_unify(Mode, ArgLval, ArgType, FieldLval, BoxedFieldType,
		Context, MLDS_Statements0, MLDS_Statements).

:- pred ml_gen_sub_unify(uni_mode, mlds__lval, prog_type, mlds__lval, prog_type,
		prog_context, mlds__statements, mlds__statements,
		ml_gen_info, ml_gen_info).
:- mode ml_gen_sub_unify(in, in, in, in, in, in, in, out, in, out) is det.

ml_gen_sub_unify(Mode, ArgLval, ArgType, FieldLval, FieldType, Context,
		MLDS_Statements0, MLDS_Statements) -->
	%
	% Figure out the direction of data-flow from the mode,
	% and generate code accordingly
	%
	{ Mode = ((LI - RI) -> (LF - RF)) },
	=(Info),
	{ ml_gen_info_get_module_info(Info, ModuleInfo) },
	{ mode_to_arg_mode(ModuleInfo, (LI -> LF), ArgType, LeftMode) },
	{ mode_to_arg_mode(ModuleInfo, (RI -> RF), ArgType, RightMode) },
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
		ml_gen_box_or_unbox_rval(FieldType, ArgType,
			lval(FieldLval), FieldRval),
		{ MLDS_Statement = ml_gen_assign(ArgLval, FieldRval,
			Context) },
		{ MLDS_Statements = [MLDS_Statement | MLDS_Statements0] }
	;
		% output - input: it's an assignment to the LHS
		{ LeftMode = top_out },
		{ RightMode = top_in }
	->
		ml_gen_box_or_unbox_rval(ArgType, FieldType,
			lval(ArgLval), ArgRval),
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
%		<succeeded = (X => f(A1, A2, ...))>
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

	% ml_gen_tag_test(Var, ConsId, Defns, Statements, Expression):
	%	Generate code to perform a tag test.
	%
	%	The test checks whether Var has the functor specified by
	%	ConsId.  The generated code may contain Defns, Statements
	%	and an Expression.  The Expression is a boolean rval.
	%	After execution of the Statements, Expression will evaluate
	%	to true iff the Var has the functor specified by ConsId.
	%
	% TODO: apply the reverse tag test optimization
	% for types with two functors (see unify_gen.m).

ml_gen_tag_test(Var, ConsId, TagTestDecls, TagTestStatements,
		TagTestExpression) -->
	ml_gen_var(Var, VarLval),
	ml_variable_type(Var, Type),
	ml_cons_id_to_tag(ConsId, Type, Tag),
	=(Info),
	{ ml_gen_info_get_module_info(Info, ModuleInfo) },
	{ TagTestExpression = ml_gen_tag_test_rval(Tag, Type, ModuleInfo,
		lval(VarLval)) },
	{ TagTestDecls = [] },
	{ TagTestStatements = [] }.

	% ml_gen_tag_test_rval(Tag, VarType, ModuleInfo, VarRval) = TestRval:
	%	TestRval is a Rval of type bool which evaluates to
	%	true if VarRval has the specified Tag and false otherwise.
	%	VarType is the type of VarRval. 
	%
:- func ml_gen_tag_test_rval(cons_tag, prog_type, module_info, mlds__rval)
	= mlds__rval.

ml_gen_tag_test_rval(string_constant(String), _, _, Rval) =
	binop(str_eq, Rval, const(string_const(String))).
ml_gen_tag_test_rval(float_constant(Float), _, _, Rval) =
	binop(float_eq, Rval, const(float_const(Float))).
ml_gen_tag_test_rval(int_constant(Int), _, _, Rval) =
	binop(eq, Rval, const(int_const(Int))).
ml_gen_tag_test_rval(pred_closure_tag(_, _, _), _, _, _Rval) = _TestRval :-
	% This should never happen, since the error will be detected
	% during mode checking.
	error("Attempted higher-order unification").
ml_gen_tag_test_rval(code_addr_constant(_, _), _, _, _Rval) = _TestRval :-
	% This should never happen
	error("Attempted code_addr unification").
ml_gen_tag_test_rval(type_ctor_info_constant(_, _, _), _, _, _) = _ :-
	% This should never happen
	error("Attempted type_ctor_info unification").
ml_gen_tag_test_rval(base_typeclass_info_constant(_, _, _), _, _, _) = _ :-
	% This should never happen
	error("Attempted base_typeclass_info unification").
ml_gen_tag_test_rval(tabling_pointer_constant(_, _), _, _, _) = _ :-
	% This should never happen
	error("Attempted tabling_pointer unification").
ml_gen_tag_test_rval(deep_profiling_proc_static_tag(_), _, _, _) = _ :-
	% This should never happen
	error("Attempted deep_profiling_proc_static unification").
ml_gen_tag_test_rval(no_tag, _, _, _Rval) = const(true).
ml_gen_tag_test_rval(unshared_tag(UnsharedTag), _, _, Rval) =
	binop(eq, unop(std_unop(tag), Rval),
		  unop(std_unop(mktag), const(int_const(UnsharedTag)))).
ml_gen_tag_test_rval(shared_remote_tag(PrimaryTagVal, SecondaryTagVal),
		VarType, ModuleInfo, Rval) = TagTest :-
	SecondaryTagField = ml_gen_secondary_tag_rval(PrimaryTagVal,
		VarType, ModuleInfo, Rval),
	SecondaryTagTest = binop(eq, SecondaryTagField,
		const(int_const(SecondaryTagVal))),
	module_info_globals(ModuleInfo, Globals),
	globals__lookup_int_option(Globals, num_tag_bits, NumTagBits),
	( NumTagBits = 0 ->
		% no need to test the primary tag
		TagTest = SecondaryTagTest
	;
		PrimaryTagTest = binop(eq,
			unop(std_unop(tag), Rval),
			unop(std_unop(mktag),
				const(int_const(PrimaryTagVal)))), 
		TagTest = binop(and, PrimaryTagTest, SecondaryTagTest)
	).
ml_gen_tag_test_rval(shared_local_tag(Bits, Num), VarType, ModuleInfo, Rval) =
		TestRval :-
	MLDS_VarType = mercury_type_to_mlds_type(ModuleInfo, VarType),
	TestRval = binop(eq, Rval,
		  unop(cast(MLDS_VarType), mkword(Bits,
		  	unop(std_unop(mkbody), const(int_const(Num)))))).

	% ml_gen_secondary_tag_rval(PrimaryTag, VarType, ModuleInfo, VarRval):
	%	Return the rval for the secondary tag field of VarRval,
	%	assuming that VarRval has the specified VarType and PrimaryTag.
ml_gen_secondary_tag_rval(PrimaryTagVal, VarType, ModuleInfo, Rval) =
		SecondaryTagField :-
	MLDS_VarType = mercury_type_to_mlds_type(ModuleInfo, VarType),
	module_info_globals(ModuleInfo, Globals),
	globals__lookup_bool_option(Globals, highlevel_data, HighLevelData),
	( HighLevelData = no ->
		% Note: with the low-level data representation,
		% all fields -- even the secondary tag -- are boxed,
		% and so we need to unbox (i.e. cast) it back to the
		% right type here.
		SecondaryTagField = 
			unop(unbox(mlds__native_int_type),
				lval(field(yes(PrimaryTagVal), Rval,
				offset(const(int_const(0))),
				mlds__generic_type, MLDS_VarType)))
	;
		FieldId = ml_gen_hl_tag_field_id(VarType, ModuleInfo),
		SecondaryTagField = lval(field(yes(PrimaryTagVal), Rval,
			FieldId, mlds__native_int_type, MLDS_VarType))
	).

	% Return the field_id for the "data_tag" field of the specified
	% Mercury type, which holds the secondary tag.
	%
:- func ml_gen_hl_tag_field_id(prog_type, module_info) = mlds__field_id.

ml_gen_hl_tag_field_id(Type, ModuleInfo) = FieldId :-
	FieldName = "data_tag",

	% Figure out the type name and arity
	( type_to_type_id(Type, TypeId0, _) ->
		TypeId = TypeId0
	;
		error("ml_gen_hl_tag_field_id: invalid type")
	),
	ml_gen_type_name(TypeId, qual(MLDS_Module, TypeName), TypeArity),

	% Figure out whether this type has constructors both
	% with and without secondary tags.  If so, then the
	% secondary tag field is in a class "tag_type" that is
	% derived from the base class for this type,
	% rather than in the base class itself.
	module_info_types(ModuleInfo, TypeTable),
	TypeDefn = map__lookup(TypeTable, TypeId),
	hlds_data__get_type_defn_body(TypeDefn, TypeDefnBody),
	( TypeDefnBody = du_type(Ctors, TagValues, _, _) ->
		(
			(some [Ctor] (
				list__member(Ctor, Ctors),
				ml_uses_secondary_tag(TagValues, Ctor, _)
			)),
			(some [Ctor] (
				list__member(Ctor, Ctors),
				\+ ml_uses_secondary_tag(TagValues, Ctor, _)
			))
		->
			ClassQualifier = mlds__append_class_qualifier(
				MLDS_Module, TypeName, TypeArity),
			ClassName = "tag_type",
			ClassArity = 0
		;
			ClassQualifier = MLDS_Module,
			ClassName = TypeName,
			ClassArity = TypeArity
		)
	;
		error("ml_gen_hl_tag_field_id: non-du type")
	),

	% Put it all together
	QualClassName = qual(ClassQualifier, ClassName),
	ClassPtrType = mlds__ptr_type(mlds__class_type(
		QualClassName, ClassArity, mlds__class)),
	FieldQualifier = mlds__append_class_qualifier(
		ClassQualifier, ClassName, ClassArity),
	QualifiedFieldName = qual(FieldQualifier, FieldName),
	FieldId = named_field(QualifiedFieldName, ClassPtrType).

:- func ml_gen_field_id(prog_type, mlds__class_name, arity, mlds__field_name) =
	mlds__field_id.

ml_gen_field_id(Type, ClassName, ClassArity, FieldName) = FieldId :-
	(
		type_to_type_id(Type, TypeId, _)
	->
		ml_gen_type_name(TypeId, qual(MLDS_Module, TypeName), TypeArity),
		ClassQualifier = mlds__append_class_qualifier(
			MLDS_Module, TypeName, TypeArity),
		QualClassName = qual(ClassQualifier, ClassName),
		ClassPtrType = mlds__ptr_type(mlds__class_type(
			QualClassName, ClassArity, mlds__class)),
		FieldQualifier = mlds__append_class_qualifier(
			ClassQualifier, ClassName, ClassArity),
		QualifiedFieldName = qual(FieldQualifier, FieldName),
		FieldId = named_field(QualifiedFieldName, ClassPtrType)
	;
		error("ml_gen_field_id: invalid type")
	).


:- func this_file = string.
this_file = "ml_unify_gen.m".

:- end_module ml_unify_gen.
