%-----------------------------------------------------------------------------%
% Copyright (C) 1999-2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: ml_closure_gen.m
% Main author: fjh

% This module is part of the MLDS code generator.
% It handles generation of MLDS code to construct closures.

%-----------------------------------------------------------------------------%

:- module ml_backend__ml_closure_gen.
:- interface.

:- import_module parse_tree__prog_data.
:- import_module hlds__hlds_pred, hlds__hlds_goal.
:- import_module ml_backend__mlds, ml_backend__ml_code_util.

:- import_module list.

	%
	% ml_gen_closure(PredId, ProcId, EvalMethod, Var, ArgVars, ArgModes,
	% 		HowToConstruct, Context, MLDS_Decls, MLDS_Statements):
	%
	% 	Generate code to construct a closure for the procedure
	%	specified by PredId and ProcId, with the partially applied
	%	arguments specified by ArgVars (and ArgModes),
	%	and to store the pointer to the resulting closure in Var.
:- pred ml_gen_closure(pred_id, proc_id, lambda_eval_method, prog_var,
		prog_vars, list(uni_mode), how_to_construct, prog_context,
		mlds__defns, mlds__statements, ml_gen_info, ml_gen_info).
:- mode ml_gen_closure(in, in, in, in, in, in, in, in, out, out, in, out)
		is det.

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

:- implementation.

:- import_module hlds__hlds_module.
:- import_module backend_libs__code_model, backend_libs__pseudo_type_info.
:- import_module backend_libs__rtti.
:- import_module ml_backend__ml_unify_gen, ml_backend__ml_call_gen.
:- import_module ml_backend__rtti_to_mlds.
:- import_module check_hlds__type_util, check_hlds__mode_util.
:- import_module hlds__error_util.
:- import_module libs__options, libs__globals.

% XXX The following modules depend on the LLDS,
% so ideally they should not be used here.
:- import_module ll_backend__continuation_info. % needed for
					   % `generate_closure_layout'
:- import_module ll_backend__stack_layout. % needed for `represent_locn_as_int'
:- import_module ll_backend__llds.         % needed for `layout_locn'

:- import_module assoc_list, bool, int, map, set, std_util, string, term.

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
	% Generate a value for the closure layout;
	% this is a static constant that holds information
	% about how the structure of this closure.
	%
	ml_gen_closure_layout(PredId, ProcId, Context,
		ClosureLayoutRval, ClosureLayoutType,
		ClosureLayoutDecls),

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
		Context, WrapperFuncRval0, WrapperFuncType0),

	%
	% Compute the rval which holds the number of arguments
	%
	{ NumArgsRval0 = const(int_const(NumArgs)) },
	{ NumArgsType0 = mlds__native_int_type },

	%
	% put all the extra arguments of the closure together
	% Note that we need to box these arguments, except for
	% the closure layout, which is already a reference type.
	%
	{ NumArgsRval = unop(box(NumArgsType0), NumArgsRval0) },
	{ NumArgsType = mlds__generic_type },
	{ WrapperFuncRval = unop(box(WrapperFuncType0), WrapperFuncRval0) },
	{ WrapperFuncType = mlds__generic_type },
	{ ExtraArgRvals = [ClosureLayoutRval, WrapperFuncRval, NumArgsRval] },
	{ ExtraArgTypes = [ClosureLayoutType, WrapperFuncType, NumArgsType] },

	%
	% the pointer will not be tagged (i.e. the tag will be zero)
	%
	{ MaybeConsId = no },
	{ MaybeConsName = no },
	{ PrimaryTag = 0 },
	{ MaybeSecondaryTag = no },

	%
	% generate a `new_object' statement (or static constant)
	% for the closure
	%
	ml_gen_new_object(MaybeConsId, PrimaryTag, MaybeSecondaryTag,
		MaybeConsName, Var, ExtraArgRvals, ExtraArgTypes, ArgVars,
		ArgModes, HowToConstruct, Context,
		MLDS_Decls0, MLDS_Statements),
	{ MLDS_Decls1 = ClosureLayoutDecls ++ MLDS_Decls0 },
	% We sometimes generates two definitions of the same RTTI constant
	% in ml_gen_closure_layout (e.g. two definitions of the same
	% pseudo_type_info).  To avoid generating invalid MLDS code,
	% we need to check for and eliminate any duplicate definitions here.
	{ MLDS_Decls = list__remove_dups(MLDS_Decls1) }.

	%
	% Generate a value for the closure layout struct.
	% See MR_Closure_Layout in ../runtime/mercury_ho_call.h.
	%
	% Note that the code here is similar to code in stack_layout.m;
	% any changes here may need to be reflected there, and vice versa.
	%
:- pred ml_gen_closure_layout(pred_id::in, proc_id::in, prog_context::in,
		mlds__rval::out, mlds__type::out, mlds__defns::out,
		ml_gen_info::in, ml_gen_info::out) is det.
ml_gen_closure_layout(PredId, ProcId, Context,
		ClosureLayoutRval, ClosureLayoutType,
		ClosureLayoutDefns) -->
	=(Info),
	{ ml_gen_info_get_module_info(Info, ModuleInfo) },
	{ continuation_info__generate_closure_layout(
		ModuleInfo, PredId, ProcId, ClosureLayoutInfo) },

	{ ml_gen_closure_proc_id(ModuleInfo, Context,
		InitProcId, ProcIdType, ClosureProcIdDefns) },

	{ ClosureLayoutInfo = closure_layout_info(ClosureArgs, TVarLocnMap) },
	{ ml_stack_layout_construct_closure_args(ModuleInfo, ClosureArgs,
		InitClosureArgs, ClosureArgTypes, ClosureArgDefns) },
	ml_gen_info_new_const(TvarVectorSeqNum),
	ml_format_static_const_name("typevar_vector", TvarVectorSeqNum,
		TvarVectorName),
	{ ml_stack_layout_construct_tvar_vector(ModuleInfo, TvarVectorName,
		Context, TVarLocnMap, TVarVectorRval, TVarVectorType,
		TVarDefns) },
	{ InitTVarVector = init_obj(unop(box(TVarVectorType),
		TVarVectorRval)) },
	{ Inits = [InitProcId, InitTVarVector | InitClosureArgs] },
	{ _ArgTypes = [ProcIdType, TVarVectorType | ClosureArgTypes] },

	ml_gen_info_new_const(LayoutSeqNum),
	ml_format_static_const_name("closure_layout", LayoutSeqNum, Name),
	{ Access = local },
	{ Initializer = init_array(Inits) },
	% XXX there's no way in C to properly represent this type,
	% since it is a struct that ends with a variable-length array.
	% For now we just treat the whole struct as an array.
	{ ClosureLayoutType = mlds__array_type(mlds__generic_type) },
	{ ClosureLayoutDefn = ml_gen_static_const_defn(Name, ClosureLayoutType,
		Access, Initializer, Context) },
	{ ClosureLayoutDefns = ClosureProcIdDefns ++ TVarDefns ++
		ClosureArgDefns ++ [ClosureLayoutDefn] },
	{ module_info_name(ModuleInfo, ModuleName) },
	{ MLDS_ModuleName = mercury_module_name_to_mlds(ModuleName) },
	{ ClosureLayoutRval = lval(var(qual(MLDS_ModuleName, Name),
		ClosureLayoutType)) }.

:- pred ml_gen_closure_proc_id(module_info::in, prog_context::in,
		mlds__initializer::out, mlds__type::out,
		mlds__defns::out) is det.
ml_gen_closure_proc_id(_ModuleInfo, _Context, InitProcId, ProcIdType,
		ClosureProcIdDefns) :-
	% XXX currently we don't fill in the ProcId field!
	InitProcId = init_obj(const(null(ProcIdType))),
	ProcIdType = mlds__generic_type,
	ClosureProcIdDefns = [].
/*
	{ module_info_name(ModuleInfo, ModuleName) },
	{ term__context_file(Context, FileName) },
	{ term__context_line(Context, LineNumber) },
	% XXX We don't have the GoalInfo here,
	%     so we can't compute the goal path correctly
	%	{ goal_info_get_goal_path(GoalInfo, GoalPath) },
	%	{ trace__path_to_string(GoalPath, GoalPathStr) },
	{ GoalPathStr = "" },
	% DataAddr = layout_addr(
	% 	closure_proc_id(CallerProcLabel, SeqNo, ClosureProcLabel)),
	% Data = layout_data(closure_proc_id_data(CallerProcLabel, SeqNo,
	% 	ClosureProcLabel, ModuleName, FileName, LineNumber, GoalPath)),
	% InitProcId = init_obj(const(data_addr_const(DataAddr))),
	% ProcIdType = ...
*/

:- pred ml_stack_layout_construct_closure_args(module_info::in,
	list(closure_arg_info)::in, list(mlds__initializer)::out,
	list(mlds__type)::out, mlds__defns::out) is det.

ml_stack_layout_construct_closure_args(ModuleInfo, ClosureArgs,
		ClosureArgInits, ClosureArgTypes, MLDS_Defns) :-
	list__map_foldl(ml_stack_layout_construct_closure_arg_rval(ModuleInfo),
		ClosureArgs, ArgInitsAndTypes, [], MLDS_Defns),
	assoc_list__keys(ArgInitsAndTypes, ArgInits),
	assoc_list__values(ArgInitsAndTypes, ArgTypes),
	Length = list__length(ArgInits),
	LengthRval = const(int_const(Length)),
	LengthType = mlds__native_int_type,
	CastLengthRval = unop(box(LengthType), LengthRval),
	ClosureArgInits = [init_obj(CastLengthRval) | ArgInits],
	ClosureArgTypes = [LengthType | ArgTypes].

:- pred ml_stack_layout_construct_closure_arg_rval(module_info::in,
	closure_arg_info::in, pair(mlds__initializer, mlds__type)::out,
	mlds__defns::in, mlds__defns::out) is det.

ml_stack_layout_construct_closure_arg_rval(ModuleInfo, ClosureArg,
		ArgInit - ArgType, MLDS_Defns0, MLDS_Defns) :-
	ClosureArg = closure_arg_info(Type, _Inst),

		% For a stack layout, we can treat all type variables as
		% universally quantified. This is not the argument of a
		% constructor, so we do not need to distinguish between type
		% variables that are and aren't in scope; we can take the
		% variable number directly from the procedure's tvar set.
	ExistQTvars = [],
	NumUnivQTvars = -1,

	pseudo_type_info__construct_pseudo_type_info(Type, NumUnivQTvars,
			ExistQTvars, PseudoTypeInfo),
	ml_gen_pseudo_type_info(ModuleInfo, PseudoTypeInfo, ArgRval, ArgType,
			MLDS_Defns0, MLDS_Defns),
	CastArgRval = unop(box(ArgType), ArgRval),
	ArgInit = init_obj(CastArgRval).

:- pred ml_gen_pseudo_type_info_defn(module_info::in, pseudo_type_info::in,
		mlds__defns::in, mlds__defns::out) is det.

ml_gen_pseudo_type_info_defn(ModuleInfo, Pseudo, Defns0, Defns) :-
	ml_gen_pseudo_type_info(ModuleInfo, Pseudo, _Rval, _Type,
		Defns0, Defns).

:- pred ml_gen_pseudo_type_info(module_info::in, pseudo_type_info::in,
		mlds__rval::out, mlds__type::out,
		mlds__defns::in, mlds__defns::out) is det.

ml_gen_pseudo_type_info(ModuleInfo, Pseudo, Rval, Type,
		MLDS_Defns0, MLDS_Defns) :-
	( Pseudo = type_var(N) ->
		% type variables are represented just as integers
		Rval = const(int_const(N)),
		Type = mlds__native_int_type,
		MLDS_Defns = MLDS_Defns0
	;
		( Pseudo = type_ctor_info(RttiTypeId0) ->
			% for zero-arity types, we just generate a
			% reference to the already-existing type_ctor_info
			RttiName = type_ctor_info,
			RttiTypeId0 = rtti_type_ctor(ModuleName0, _, _),
			ModuleName = fixup_builtin_module(ModuleName0),
			RttiTypeId = RttiTypeId0,
			MLDS_Defns = MLDS_Defns0
		;
			% for other types, we need to generate a definition
			% of the pseudo_type_info for that type,
			% in the the current module
			module_info_name(ModuleInfo, ModuleName),
			RttiData = pseudo_type_info(Pseudo),
			rtti_data_to_name(RttiData, RttiTypeId, RttiName),
			RttiDefns0 = rtti_data_list_to_mlds(ModuleInfo,
				[RttiData]),
			% rtti_data_list_to_mlds assumes that the result
			% will be at file scope, but here we're generating it
			% as a local, so we need to convert the access
			% to `local'
			RttiDefns = list__map(convert_to_local, RttiDefns0),
			MLDS_Defns1 = RttiDefns ++ MLDS_Defns0,
			% Generate definitions of any pseudo_type_infos
			% referenced by this pseudotypeinfo.
			list__foldl(ml_gen_pseudo_type_info_defn(ModuleInfo),
				arg_pseudo_type_infos(Pseudo),
				MLDS_Defns1, MLDS_Defns)
		),
		MLDS_ModuleName = mercury_module_name_to_mlds(ModuleName),
		Rval = const(data_addr_const(data_addr(MLDS_ModuleName,
			rtti(RttiTypeId, RttiName)))),
		Type = mlds__rtti_type(RttiName)
	).

:- func arg_pseudo_type_infos(pseudo_type_info) = list(pseudo_type_info).
arg_pseudo_type_infos(type_var(_)) = [].
arg_pseudo_type_infos(type_ctor_info(_)) = [].
arg_pseudo_type_infos(type_info(_TypeId, ArgPTIs)) = ArgPTIs.
arg_pseudo_type_infos(higher_order_type_info(_TypeId, _Arity, ArgPTIs)) =
	ArgPTIs.

:- func convert_to_local(mlds__defn) = mlds__defn.
convert_to_local(mlds__defn(Name, Context, Flags0, Body)) =
		mlds__defn(Name, Context, Flags, Body) :-
	Flags = set_access(Flags0, local).

:- pred ml_stack_layout_construct_tvar_vector(module_info::in,
	mlds__var_name::in, prog_context::in, map(tvar, set(layout_locn))::in,
	mlds__rval::out, mlds__type::out, mlds__defns::out) is det.

ml_stack_layout_construct_tvar_vector(ModuleInfo, TvarVectorName, Context,
		TVarLocnMap, MLDS_Rval, ArrayType, MLDS_Defns) :-
	ArrayType = mlds__array_type(mlds__native_int_type),
	( map__is_empty(TVarLocnMap) ->
		MLDS_Rval = const(null(ArrayType)),
		MLDS_Defns = []
	;
		Access = local,
		ml_stack_layout_construct_tvar_rvals(TVarLocnMap,
			Vector, _VectorTypes),
		Initializer = init_array(Vector),
		MLDS_Defn = ml_gen_static_const_defn(TvarVectorName, ArrayType,
			Access, Initializer, Context),
		MLDS_Defns = [MLDS_Defn],
		module_info_name(ModuleInfo, ModuleName),
		MLDS_ModuleName = mercury_module_name_to_mlds(ModuleName),
		MLDS_Rval = lval(var(qual(MLDS_ModuleName, TvarVectorName),
			ArrayType))
	).

:- pred ml_stack_layout_construct_tvar_rvals(map(tvar, set(layout_locn))::in,
	list(mlds__initializer)::out, list(mlds__type)::out) is det.

ml_stack_layout_construct_tvar_rvals(TVarLocnMap, Vector, VectorTypes) :-
	map__to_assoc_list(TVarLocnMap, TVarLocns),
	ml_stack_layout_construct_type_param_locn_vector(TVarLocns, 1,
		TypeParamLocs),
	list__length(TypeParamLocs, TypeParamsLength),
	LengthRval = const(int_const(TypeParamsLength)),
	Vector = [init_obj(LengthRval) | TypeParamLocs],
	VectorTypes = list__duplicate(TypeParamsLength + 1,
			mlds__native_int_type).

	% Given a association list of type variables and their locations
	% sorted on the type variables, represent them in an array of
	% location descriptions indexed by the type variable. The next
	% slot to fill is given by the second argument.

:- pred ml_stack_layout_construct_type_param_locn_vector(
	assoc_list(tvar, set(layout_locn))::in,
	int::in, list(mlds__initializer)::out) is det.

ml_stack_layout_construct_type_param_locn_vector([], _, []).
ml_stack_layout_construct_type_param_locn_vector([TVar - Locns | TVarLocns],
		CurSlot, Vector) :-
	term__var_to_int(TVar, TVarNum),
	NextSlot is CurSlot + 1,
	( TVarNum = CurSlot ->
		( set__remove_least(Locns, LeastLocn, _) ->
			Locn = LeastLocn
		;
			unexpected(this_file,
				"tvar has empty set of locations")
		),
		stack_layout__represent_locn_as_int(Locn, LocnAsInt),
		Rval = const(int_const(LocnAsInt)),
		ml_stack_layout_construct_type_param_locn_vector(TVarLocns,
			NextSlot, VectorTail),
		Vector = [init_obj(Rval) | VectorTail]
	; TVarNum > CurSlot ->
		% This slot will never be referred to.
		ml_stack_layout_construct_type_param_locn_vector(
			[TVar - Locns | TVarLocns], NextSlot, VectorTail),
		Vector = [init_obj(const(int_const(0))) | VectorTail]
	;
		unexpected(this_file,
			"unsorted tvars in construct_type_param_locn_vector")
	).

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
	%		MR_bool succeeded;
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
	% XXX The accurate GC handling for closures arguments is wrong
	%
	{ ProcBoxedArgTypes = ml_make_boxed_types(ProcArity) },

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
		unexpected(this_file,
			"ml_gen_closure_wrapper: list__drop failed")
	},
	{ WrapperHeadVarNames = ml_gen_wrapper_head_var_names(1,
		list__length(WrapperHeadVars)) },
	ml_gen_params(WrapperHeadVarNames, WrapperBoxedArgTypes,
		WrapperArgModes, PredOrFunc, CodeModel, WrapperParams0),

	% then insert the `closure_arg' parameter
	{ ClosureArgType = mlds__generic_type },
	% XXX FIXME The GC handling for closures is wrong
	{ GC_TraceCode = no },
	{ ClosureArg = mlds__argument(
		data(var(var_name("closure_arg", no))),
		ClosureArgType,
		GC_TraceCode) },
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
	% XXX FIXME The GC handling for closures is wrong
	{ GC_TraceCode = no },
	{ ClosureDecl = ml_gen_mlds_var_decl(var(ClosureName),
		ClosureType, GC_TraceCode, MLDS_Context) },
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
	% For semidet code, add the declaration `MR_bool succeeded;'
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
		{ sorry(this_file,
			"ml_gen_wrapper_arg_lvals: length mismatch") }
	).

:- pred ml_gen_local_for_output_arg(var_name, prog_type, prog_context,
		mlds__defn, ml_gen_info, ml_gen_info).
:- mode ml_gen_local_for_output_arg(in, in, in, out, in, out) is det.

ml_gen_local_for_output_arg(VarName, Type, Context, LocalVarDefn) -->
	%
	% Generate a declaration for a corresponding local variable.
	%
	ml_gen_var_decl(VarName, Type, Context, LocalVarDefn).

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

%-----------------------------------------------------------------------------%

:- func this_file = string.
this_file = "ml_closure_gen.m".

:- end_module ml_closure_gen.

%-----------------------------------------------------------------------------%
