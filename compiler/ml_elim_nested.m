%-----------------------------------------------------------------------------%
% Copyright (C) 1999-2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: ml_elim_nested.m
% Main author: fjh

% This module is an MLDS-to-MLDS transformation
% that eliminates nested functions.

% Note that this module does not attempt to handle arbitrary MLDS
% as input; it will only work with the output of the current MLDS
% code generator.  In particular, it assumes that local variables
% in nested functions can be hoisted into the outermost function's
% environment.  That's not true in general (e.g. if the nested
% functions are recursive), but it's true for the code that ml_code_gen
% generates.

% As well as eliminating nested functions, this transformation
% also has the effect of fixing up the dangling `env_ptr' references
% that ml_code_gen.m leaves in the code.

%-----------------------------------------------------------------------------%
% TRANSFORMATION SUMMARY
%-----------------------------------------------------------------------------%
%
% We transform code of the form e.g.
%
%	<OuterRet> outer(<OuterArgs>) {
%		<OuterLocals>
%
%		<Inner1Ret> inner(<Inner1Args>, void *env_ptr_arg) {
%			<Inner1Locals>
%
%			<NestedInnerRet> nested_inner(<NestedInnerArgs>,
%						void *env_ptr_arg)
%			{
%				<NestedInnerLocals>
%
%				<NestedInnerCode>
%			}
%
%			<Inner1Code>
%		}
%
%		<Inner2Ret> inner(<Inner2Args>, void *env_ptr_arg) {
%			<Inner2Locals>
%
%			<Inner2Code>
%		}
%
%		<OuterCode>
%	}
%
% into
%
%	struct OuterLocals_struct {
%		<OuterArgs>
%		<OuterLocals>
%		<Inner1Locals>
%	};
%
%	<NestedInnerRet> nested_inner(<NestedInnerArgs>, void *env_ptr_arg) {
%		OuterLocals *env_ptr = env_ptr_arg;
%		<NestedInnerLocals>
%
%		<NestedInnerCode'>
%	}
%
%	<Inner1Ret> inner(<Inner1Args>, void *env_ptr_arg) {
%		OuterLocals *env_ptr = env_ptr_arg;
%		
%		<Inner1Code'>
%	}
%
%	<Inner2Ret> inner(<Inner2Args>, void *env_ptr_arg) {
%		OuterLocals *env_ptr = env_ptr_arg;
%		<Inner2Locals>
%
%		<Inner2Code'>
%	}
%
%	<OuterRet> outer(<OuterArgs>) {
%		OuterLocals env;
%		OuterLocals *env_ptr = &env;
%
%		env_ptr-><OuterArgs> = <OuterArgs>;
%		<OuterCode'>
%	}
%
% where <Inner1Code'>, <Inner2Code'> and <NestedInnerCode'> are the
% same as <Inner1Code>, <Inner2Code> and <NestedInnerCode> (respectively)
% except that any references to a local variable <Var> declared in
% outer() are replaced with `env_ptr -> <Var>',
% and likewise <OuterCode'> is the same as <OuterCode> with references to
% local variables replaced with `env_ptr->foo'.  In the latter
% case it could (depending on how smart the C compiler is) potentially
% be more efficient to generate `env.foo', but currently we don't do that.
%
% Actually the description above is slightly over-simplified: not all local
% variables need to be put in the environment struct.  Only those local
% variables which are referenced by nested functions need to be
% put in the environment struct.  Also, if none of the nested functions
% refer to the locals in the outer function, we don't need to create
% an environment struct at all, we just need to hoist the definitions
% of the nested functions out to the top level.
%
% The `env_ptr' variables generated here serve as definitions for
% the (previously dangling) references to such variables that
% ml_code_gen puts in calls to the nested functions.

%-----------------------------------------------------------------------------%

:- module ml_elim_nested.

:- interface.

:- import_module mlds.
:- import_module io.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Eliminated nested functions for the whole MLDS.
	%
:- pred ml_elim_nested(mlds, mlds, io__state, io__state).
:- mode ml_elim_nested(in, out, di, uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module bool, int, list, std_util, string, require.

:- import_module ml_code_util, ml_util.

% the following imports are needed for mangling pred names
:- import_module hlds_pred, prog_data, prog_out.

:- import_module globals, options.

	% Eliminated nested functions for the whole MLDS.
	%
ml_elim_nested(MLDS0, MLDS) -->
	globals__io_get_globals(Globals),
	{ MLDS0 = mlds(ModuleName, ForeignCode, Imports, Defns0) },
	{ MLDS = mlds(ModuleName, ForeignCode, Imports, Defns) },
	{ MLDS_ModuleName = mercury_module_name_to_mlds(ModuleName) },
	{ OuterVars = [] },
	{ DefnsList = list__map(
		ml_elim_nested_defns(MLDS_ModuleName, Globals, OuterVars),
		Defns0) },
	{ Defns = list__condense(DefnsList) }.

	% Hoist out any nested function occurring in a single mlds__defn.
	% Return a list of mlds__defns that contains no nested functions.
	%
:- func ml_elim_nested_defns(mlds_module_name, globals, outervars,
		mlds__defn) = list(mlds__defn).
ml_elim_nested_defns(ModuleName, Globals, OuterVars, Defn0) = FlatDefns :-
	Defn0 = mlds__defn(Name, Context, Flags, DefnBody0),
	( DefnBody0 = mlds__function(PredProcId, Params,
			defined_here(FuncBody0), Attributes) ->
		EnvName = ml_env_name(Name),
			% XXX this should be optimized to generate 
			% EnvTypeName from just EnvName
		ml_create_env(EnvName, [], Context, ModuleName, Globals,
			_EnvTypeDefn, EnvTypeName, _EnvDecls, _InitEnv),
		
		EnvPtrTypeName = ml_make_env_ptr_type(Globals, EnvTypeName),

		%
		% traverse the function body, finding (and removing)
		% any nested functions, and fixing up any references
		% to the arguments or to local variables or local
		% static constants which occur in nested functions
		%
		ElimInfo0 = elim_info_init(ModuleName, OuterVars, EnvTypeName,
			EnvPtrTypeName),
		Params = mlds__func_params(Arguments, _RetValues),
		ml_maybe_add_args(Arguments, FuncBody0, ModuleName,
			Context, ElimInfo0, ElimInfo1),
		flatten_statement(FuncBody0, FuncBody1, ElimInfo1, ElimInfo),
		elim_info_finish(ElimInfo, NestedFuncs0, Locals),

		%
		% Split the locals that we need to process
		% into local variables and local static constants
		%
		list__filter(ml_decl_is_static_const, Locals,
			LocalStatics, LocalVars),
		%
		% Fix up access flags on the statics that we're going to hoist:
		% convert "local" to "private"
		%
		HoistedStatics = list__map(convert_local_to_global, LocalStatics),


		%
		% if there were no nested functions, then we just
		% hoist the local static constants
		%
		( NestedFuncs0 = [] ->
			FuncBody = FuncBody1,
			HoistedDefns = HoistedStatics
		;
			%
			% Create a struct to hold the local variables,
			% and initialize the environment pointers for
			% both the containing function and the nested
			% functions
			%
			ml_create_env(EnvName, LocalVars, Context, ModuleName,
				Globals, EnvTypeDefn, _EnvTypeName, EnvDecls,
				InitEnv),
			list__map_foldl(
				ml_insert_init_env(EnvTypeName, ModuleName,
					Globals), NestedFuncs0, NestedFuncs,
					no, InsertedEnv),

			% Hoist out the local statics and the nested functions
			HoistedDefns0 = list__append(HoistedStatics, NestedFuncs),

			% 
			% It's possible that none of the nested
			% functions reference the arguments or locals of
			% the parent function.  In that case, there's no
			% need to create an environment, we just need to 
			% flatten the functions.
			%
			% Note that we don't generate the
			% env_ptr_args in this module (instead they are
			% generated when the nested functions are
			% generated).  This means that we don't avoid
			% generating these arguments.  This is not
			% really a big problem, since the code
			% that generates these arguments needs them.
			%
			( InsertedEnv = yes ->
				%
				% If the function's arguments are
				% referenced by nested functions, then
				% we need to copy them to local
				% variables in the environment
				% structure.
				%
				ml_maybe_copy_args(Arguments, FuncBody0,
					ModuleName, EnvTypeName, EnvPtrTypeName,
					Context, _ArgsToCopy, CodeToCopyArgs),

				%
				% insert the definition and
				% initialization of the environment
				% struct variable at the start of the
				% top-level function's body
				%
				FuncBody = ml_block(EnvDecls,
					list__append(
						[InitEnv | CodeToCopyArgs], 
						[FuncBody1]), Context),
				%
				% insert the environment struct type
				% at the start of the list of hoisted definitions
				% (preceding the previously nested functions
				% and static constants in HoistedDefns0),
				%
				HoistedDefns = [EnvTypeDefn | HoistedDefns0]
			;
				FuncBody = FuncBody1,
				HoistedDefns = HoistedDefns0
			)
		),
		DefnBody = mlds__function(PredProcId, Params,
			defined_here(FuncBody), Attributes),
		Defn = mlds__defn(Name, Context, Flags, DefnBody),
		FlatDefns = list__append(HoistedDefns, [Defn])
	;
		% leave definitions of things other than functions unchanged
		FlatDefns = [Defn0]
	).

	%
	% Add any arguments which are used in nested functions
	% to the local_data field in the elim_info.
	%
:- pred ml_maybe_add_args(mlds__arguments, mlds__statement,
		mlds_module_name, mlds__context, elim_info, elim_info).
:- mode ml_maybe_add_args(in, in, in, in, in, out) is det.

ml_maybe_add_args([], _, _, _) --> [].
ml_maybe_add_args([Arg|Args], FuncBody, ModuleName, Context) -->
	(
		{ Arg = data(var(VarName)) - _Type },
		{ ml_should_add_local_data(ModuleName, VarName, [], [FuncBody]) }
	->
		{ ml_conv_arg_to_var(Context, Arg, ArgToCopy) },
		elim_info_add_local_data(ArgToCopy)
	;
		[]
	),
	ml_maybe_add_args(Args, FuncBody, ModuleName, Context).

	%
	% Generate code to copy any arguments which are used in nested functions
	% to the environment struct.
	%
:- pred ml_maybe_copy_args(mlds__arguments, mlds__statement,
		mlds_module_name, mlds__type, mlds__type, mlds__context, 
		mlds__defns, mlds__statements).
:- mode ml_maybe_copy_args(in, in, in, in, in, in, out, out) is det.

ml_maybe_copy_args([], _, _, _, _, _, [], []).
ml_maybe_copy_args([Arg|Args], FuncBody, ModuleName, ClassType, EnvPtrTypeName,
		Context, ArgsToCopy, CodeToCopyArgs) :-
	ml_maybe_copy_args(Args, FuncBody, ModuleName, ClassType,
		EnvPtrTypeName,	Context, ArgsToCopy0, CodeToCopyArgs0),
	(
		Arg = data(var(VarName)) - FieldType,
		ml_should_add_local_data(ModuleName, VarName, [], [FuncBody])
	->
		ml_conv_arg_to_var(Context, Arg, ArgToCopy),

		%
		% Generate code to copy this arg to the environment
		% struct:
		%	env_ptr->foo = foo;
		%
		QualVarName = qual(ModuleName, VarName),
		EnvModuleName = ml_env_module_name(ClassType),
		FieldNameString = ml_var_name_to_string(VarName),
		FieldName = named_field(qual(EnvModuleName, FieldNameString),
			EnvPtrTypeName),
		Tag = yes(0),
		EnvPtr = lval(var(qual(ModuleName,
				mlds__var_name("env_ptr", no)),
			EnvPtrTypeName)),
		EnvArgLval = field(Tag, EnvPtr, FieldName, FieldType, 
			EnvPtrTypeName),
		ArgRval = lval(var(QualVarName, FieldType)),
		AssignToEnv = assign(EnvArgLval, ArgRval),
		CodeToCopyArg = mlds__statement(atomic(AssignToEnv), Context),

		ArgsToCopy = [ArgToCopy | ArgsToCopy0],
		CodeToCopyArgs = [CodeToCopyArg | CodeToCopyArgs0]
	;
		ArgsToCopy = ArgsToCopy0,
		CodeToCopyArgs = CodeToCopyArgs0
	).

	% Create the environment struct type,
	% the declaration of the environment variable,
	% and the declaration and initializer for the environment
	% pointer variable:
	%
	%	struct <EnvClassName> {
	%		<LocalVars>
	%	};
	%	struct <EnvClassName> env;
	%	struct <EnvClassName> *env_ptr;
	%	env_ptr = &env;
	%
:- pred ml_create_env(mlds__class_name, list(mlds__defn), mlds__context,
		mlds_module_name, globals, mlds__defn, mlds__type,
		list(mlds__defn), mlds__statement).
:- mode ml_create_env(in, in, in, in, in, out, out, out, out) is det.

ml_create_env(EnvClassName, LocalVars, Context, ModuleName, Globals,
		EnvTypeDefn, EnvTypeName, EnvDecls, InitEnv) :-
	%
	% generate the following type:
	%
	%	struct <EnvClassName> {
	%		<LocalVars>
	%	};
	%
		% If we're allocating it on the heap, then we need to use 
		% a class type rather than a struct (value type).
		% This is needed for verifiable code on the IL back-end.
	globals__lookup_bool_option(Globals, put_nondet_env_on_heap, OnHeap),
	( OnHeap = yes ->
		EnvTypeKind = mlds__class,
		BaseClasses = [mlds__generic_env_ptr_type]
	;
		EnvTypeKind = mlds__struct,
		BaseClasses = []
	),
	EnvTypeName = class_type(qual(ModuleName, EnvClassName), 0,
		EnvTypeKind),
	EnvTypeEntityName = type(EnvClassName, 0),
	EnvTypeFlags = env_type_decl_flags,
	Fields = list__map(convert_local_to_field, LocalVars),
	Imports = [],
	Ctors = [], % mlds_to_il.m will add an empty constructor if needed
	Interfaces = [],
	EnvTypeDefnBody = mlds__class(mlds__class_defn(EnvTypeKind, Imports, 
		BaseClasses, Interfaces, Ctors, Fields)),
	EnvTypeDefn = mlds__defn(EnvTypeEntityName, Context, EnvTypeFlags,
		EnvTypeDefnBody),

	%
	% generate the following variable declaration:
	%
	%	struct <EnvClassName> env;
	%
	EnvVarName = data(var(var_name("env", no))),
	EnvVarFlags = ml_gen_local_var_decl_flags,
	EnvVarDefnBody = mlds__data(EnvTypeName, no_initializer),
	EnvVarDecl = mlds__defn(EnvVarName, Context, EnvVarFlags,
		EnvVarDefnBody),

	%
	% declare the `env_ptr' var, and
	% initialize the `env_ptr' with the address of `env'
	%
	EnvVar = qual(ModuleName, mlds__var_name("env", no)),

	%
	% generate code to initialize the environment pointer,
	% either by allocating an object on the heap, or by
	% just taking the address of the struct we put on the stack
	%
	( OnHeap = yes ->
		EnvVarAddr = lval(var(EnvVar, EnvTypeName)),
		ml_init_env(EnvTypeName, EnvVarAddr, Context, ModuleName,
			 Globals, EnvPtrVarDecl, InitEnv0),
		
		NewObj = mlds__statement(
				atomic(new_object(
					var(EnvVar, EnvTypeName), 
					no, no, EnvTypeName, no, no, [], [])),
				Context),
		InitEnv = mlds__statement(block([], 
			[NewObj, InitEnv0]), Context),
		EnvDecls = [EnvVarDecl, EnvPtrVarDecl]
	;
		EnvVarAddr = mem_addr(var(EnvVar, EnvTypeName)),
		ml_init_env(EnvTypeName, EnvVarAddr, Context, ModuleName,
			Globals, EnvPtrVarDecl, InitEnv),
		EnvDecls = [EnvVarDecl, EnvPtrVarDecl]
	).

	% When converting local variables into fields of the
	% environment struct, we need to change `local' access
	% into something else, since `local' is only supposed to be
	% used for entities that are local to a function or block,
	% not for fields.  Currently we change it to `public'.
	% (Perhaps changing it to `default' might be better?)
	% 
:- func convert_local_to_field(mlds__defn) = mlds__defn.
convert_local_to_field(mlds__defn(Name, Context, Flags0, Body)) =
		mlds__defn(Name, Context, Flags, Body) :-
	( access(Flags0) = local ->
		Flags = set_access(Flags0, public)
	;
		Flags = Flags0
	).

	% Similarly, when converting local statics into
	% global statics, we need to change `local' access
	% to something else -- we use `private'.
:- func convert_local_to_global(mlds__defn) = mlds__defn.
convert_local_to_global(mlds__defn(Name, Context, Flags0, Body)) =
		mlds__defn(Name, Context, Flags, Body) :-
	( access(Flags0) = local ->
		Flags = set_access(Flags0, private)
	;
		Flags = Flags0
	).

	% ml_insert_init_env:
	%	If the definition is a nested function definition, and it's
	%	body makes use of the environment pointer (`env_ptr'), then
	%	insert code to declare and initialize the environment pointer.
	%
	% We transform code of the form
	%	<Ret> <Func>(<Args>) {
	%		<Body>
	%	}
	% to
	%	<Ret> <Func>(<Args>) {
	%		struct <EnvClassName> *env_ptr;
	%		env_ptr = (<EnvClassName> *) env_ptr_arg;
	%		<Body>
	%	}
	%
	% If we perform this transformation, set Init to "yes",
	% otherwise leave it unchanged.
	%
:- pred ml_insert_init_env(mlds__type, mlds_module_name, globals,
		mlds__defn, mlds__defn, bool, bool).
:- mode ml_insert_init_env(in, in, in, in, out, in, out) is det.
ml_insert_init_env(TypeName, ModuleName, Globals, Defn0, Defn, Init0, Init) :-
	Defn0 = mlds__defn(Name, Context, Flags, DefnBody0),
	(
		DefnBody0 = mlds__function(PredProcId, Params,
			defined_here(FuncBody0), Attributes),
		statement_contains_var(FuncBody0, qual(ModuleName,
			mlds__var_name("env_ptr", no)))
	->
		EnvPtrVal = lval(var(qual(ModuleName,
				mlds__var_name("env_ptr_arg", no)),
				mlds__generic_env_ptr_type)),
		EnvPtrVarType = ml_make_env_ptr_type(Globals, TypeName),

			% Insert a cast, to downcast from
			% mlds__generic_env_ptr_type to the specific
			% environment type for this procedure.
		CastEnvPtrVal = unop(cast(EnvPtrVarType), EnvPtrVal),

		ml_init_env(TypeName, CastEnvPtrVal, Context, ModuleName,
			Globals, EnvPtrDecl, InitEnvPtr),
		FuncBody = mlds__statement(block([EnvPtrDecl],
				[InitEnvPtr, FuncBody0]), Context),
		DefnBody = mlds__function(PredProcId, Params,
			defined_here(FuncBody), Attributes),
		Defn = mlds__defn(Name, Context, Flags, DefnBody),
		Init = yes
	;
		Defn = Defn0,
		Init = Init0
	).

:- func ml_make_env_ptr_type(globals, mlds__type) = mlds__type.
ml_make_env_ptr_type(Globals, EnvType) = EnvPtrType :-
	globals__lookup_bool_option(Globals, put_nondet_env_on_heap, OnHeap),
	globals__get_target(Globals, Target),
	( Target = il, OnHeap = yes ->
		% For IL, a class type is already a pointer (object reference).
		EnvPtrType = EnvType
	;
		EnvPtrType = mlds__ptr_type(EnvType)
	).

	% Create the environment pointer and initialize it:
	%
	%	struct <EnvClassName> *env_ptr;
	%	env_ptr = <EnvPtrVal>;
	%
:- pred ml_init_env(mlds__type, mlds__rval,
		mlds__context, mlds_module_name, globals,
		mlds__defn, mlds__statement).
:- mode ml_init_env(in, in, in, in, in, out, out) is det.

ml_init_env(EnvTypeName, EnvPtrVal, Context, ModuleName, Globals,
		EnvPtrVarDecl, InitEnvPtr) :-
	%
	% generate the following variable declaration:
	%
	%	<EnvTypeName> *env_ptr;
	%
	EnvPtrVarName = data(var(mlds__var_name("env_ptr", no))),
	EnvPtrVarFlags = ml_gen_local_var_decl_flags,
	EnvPtrVarType = ml_make_env_ptr_type(Globals, EnvTypeName),
	EnvPtrVarDefnBody = mlds__data(EnvPtrVarType, no_initializer),
	EnvPtrVarDecl = mlds__defn(EnvPtrVarName, Context, EnvPtrVarFlags,
		EnvPtrVarDefnBody),

	%
	% generate the following statement:
	%
	%	env_ptr = <EnvPtrVal>;
	%
	% (note that the caller of this routine is responsible
	% for inserting a cast in <EnvPtrVal> if needed).
	%
	EnvPtrVar = qual(ModuleName, mlds__var_name("env_ptr", no)),
	AssignEnvPtr = assign(var(EnvPtrVar, EnvPtrVarType), EnvPtrVal),
	InitEnvPtr = mlds__statement(atomic(AssignEnvPtr), Context).

	% Given the declaration for a function parameter, produce a
	% declaration for a corresponding local variable or environment
	% struct field.  We need to do this so as to include function
	% parameter in the environment struct.
	%
:- pred ml_conv_arg_to_var(mlds__context, pair(entity_name, mlds__type),
		mlds__defn).
:- mode ml_conv_arg_to_var(in, in, out) is det.

ml_conv_arg_to_var(Context, Name - Type, LocalVar) :-
	Flags = ml_gen_local_var_decl_flags,
	DefnBody = mlds__data(Type, no_initializer),
	LocalVar = mlds__defn(Name, Context, Flags, DefnBody).

	% Return the declaration flags appropriate for an environment struct
	% type declaration.
:- func env_type_decl_flags = mlds__decl_flags.
env_type_decl_flags = MLDS_DeclFlags :-
	Access = private,
	PerInstance = one_copy,
	Virtuality = non_virtual,
	Finality = overridable,
	Constness = modifiable,
	Abstractness = concrete,
	MLDS_DeclFlags = init_decl_flags(Access, PerInstance,
		Virtuality, Finality, Constness, Abstractness).

	% Generate a block statement, i.e. `{ <Decls>; <Statements>; }'.
	% But if the block consists only of a single statement with no
	% declarations, then just return that statement.
	%
:- func ml_block(mlds__defns, mlds__statements, mlds__context) =
		mlds__statement.

ml_block(VarDecls, Statements, Context) =
	(if VarDecls = [], Statements = [SingleStatement] then
		SingleStatement
	else
		mlds__statement(block(VarDecls, Statements), Context)
	).
		
%-----------------------------------------------------------------------------%
%
% This code does some name mangling.
% It essentially duplicates the functionality in mlds_output_name.
% 
% Doing name mangling here is probably a bad idea;
% it might be better to change the MLDS data structure
% to allow structured type names, so that we don't have to
% do any name mangling at this point.
%

	% Compute the name to use for the environment struct
	% for the specified function.
:- func ml_env_name(mlds__entity_name) = mlds__class_name.

ml_env_name(type(_, _)) = _ :-
	error("ml_env_name: expected function, got type").
ml_env_name(data(_)) = _ :-
	error("ml_env_name: expected function, got data").
ml_env_name(function(PredLabel, ProcId, MaybeSeqNum, _PredId)) = ClassName :-
	PredLabelString = ml_pred_label_name(PredLabel),
	proc_id_to_int(ProcId, ModeNum),
	( MaybeSeqNum = yes(SeqNum) ->
		string__format("%s_%d_%d_env",
			[s(PredLabelString), i(ModeNum), i(SeqNum)],
			ClassName)
	;
		string__format("%s_%d_env",
			[s(PredLabelString), i(ModeNum)],
			ClassName)
	).
ml_env_name(export(_)) = _ :-
	error("ml_env_name: expected function, got export").

:- func ml_pred_label_name(mlds__pred_label) = string.

ml_pred_label_name(pred(PredOrFunc, MaybeDefiningModule, Name, Arity,
		_CodeModel, _NonOutputFunc)) = LabelName :-
	( PredOrFunc = predicate, Suffix = "p"
	; PredOrFunc = function, Suffix = "f"
	),
	( MaybeDefiningModule = yes(DefiningModule) ->
		ModuleNameString = ml_module_name_string(DefiningModule),
		string__format("%s_%d_%s_in__%s",
			[s(Name), i(Arity), s(Suffix), s(ModuleNameString)],
			LabelName)
	;
		string__format("%s_%d_%s",
			[s(Name), i(Arity), s(Suffix)],
			LabelName)
	).
ml_pred_label_name(special_pred(PredName, MaybeTypeModule,
		TypeName, TypeArity)) = LabelName :-
	( MaybeTypeModule = yes(TypeModule) ->
		TypeModuleString = ml_module_name_string(TypeModule),
		string__format("%s__%s__%s_%d",
			[s(PredName), s(TypeModuleString),
				s(TypeName), i(TypeArity)],
			LabelName)
	;
		string__format("%s__%s_%d",
			[s(PredName), s(TypeName), i(TypeArity)],
			LabelName)
	).

:- func ml_module_name_string(mercury_module_name) = string.
ml_module_name_string(ModuleName) = ModuleNameString :-
	Separator = "__",
	prog_out__sym_name_to_string(ModuleName, Separator, ModuleNameString).

%-----------------------------------------------------------------------------%

%
% flatten_maybe_statement:
% flatten_function_body:
% flatten_statements:
% flatten_statement:
%	Recursively process the statement(s), calling fixup_var on every
%	use of a variable inside them, and calling flatten_nested_defns
%	for every definition they contain (e.g. definitions of local
%	variables and nested functions).
%

:- pred flatten_function_body(function_body, function_body,
		elim_info, elim_info).
:- mode flatten_function_body(in, out, in, out) is det.

flatten_function_body(external, external) --> [].
flatten_function_body(defined_here(Statement0), defined_here(Statement)) -->
	flatten_statement(Statement0, Statement).

:- pred flatten_maybe_statement(maybe(mlds__statement), maybe(mlds__statement),
		elim_info, elim_info).
:- mode flatten_maybe_statement(in, out, in, out) is det.

flatten_maybe_statement(no, no) --> [].
flatten_maybe_statement(yes(Statement0), yes(Statement)) -->
	flatten_statement(Statement0, Statement).
	
:- pred flatten_statements(mlds__statements, mlds__statements,
		elim_info, elim_info).
:- mode flatten_statements(in, out, in, out) is det.

flatten_statements(Statements0, Statements) -->
	list__map_foldl(flatten_statement, Statements0, Statements).

:- pred flatten_statement(mlds__statement, mlds__statement,
		elim_info, elim_info).
:- mode flatten_statement(in, out, in, out) is det.

flatten_statement(Statement0, Statement) -->
	{ Statement0 = mlds__statement(Stmt0, Context) },
	flatten_stmt(Stmt0, Stmt),
	{ Statement = mlds__statement(Stmt, Context) }.

:- pred flatten_stmt(mlds__stmt, mlds__stmt, elim_info, elim_info).
:- mode flatten_stmt(in, out, in, out) is det.

flatten_stmt(Stmt0, Stmt) -->
	(
		{ Stmt0 = block(Defns0, Statements0) },
		flatten_nested_defns(Defns0, Statements0, Defns),
		flatten_statements(Statements0, Statements),
		{ Stmt = block(Defns, Statements) }
	;
		{ Stmt0 = while(Rval0, Statement0, Once) },
		fixup_rval(Rval0, Rval),
		flatten_statement(Statement0, Statement),
		{ Stmt = while(Rval, Statement, Once) }
	;
		{ Stmt0 = if_then_else(Cond0, Then0, MaybeElse0) },
		fixup_rval(Cond0, Cond),
		flatten_statement(Then0, Then),
		flatten_maybe_statement(MaybeElse0, MaybeElse),
		{ Stmt = if_then_else(Cond, Then, MaybeElse) }
	;
		{ Stmt0 = switch(Type, Val0, Range, Cases0, Default0) },
		fixup_rval(Val0, Val),
		list__map_foldl(flatten_case, Cases0, Cases),
		flatten_default(Default0, Default),
		{ Stmt = switch(Type, Val, Range, Cases, Default) }
	;
		{ Stmt0 = label(_) },
		{ Stmt = Stmt0 }
	;
		{ Stmt0 = goto(_) },
		{ Stmt = Stmt0 }
	;
		{ Stmt0 = computed_goto(Rval0, Labels) },
		fixup_rval(Rval0, Rval),
		{ Stmt = computed_goto(Rval, Labels) }
	;
		{ Stmt0 = call(Sig, Func0, Obj0, Args0, RetLvals0, TailCall) },
		fixup_rval(Func0, Func),
		fixup_maybe_rval(Obj0, Obj),
		fixup_rvals(Args0, Args),
		fixup_lvals(RetLvals0, RetLvals),
		{ Stmt = call(Sig, Func, Obj, Args, RetLvals, TailCall) }
	;
		{ Stmt0 = return(Rvals0) },
		fixup_rvals(Rvals0, Rvals),
		{ Stmt = return(Rvals) }
	;
		{ Stmt0 = do_commit(Ref0) },
		fixup_rval(Ref0, Ref),
		{ Stmt = do_commit(Ref) }
	;
		{ Stmt0 = try_commit(Ref0, Statement0, Handler0) },
		fixup_lval(Ref0, Ref),
		flatten_statement(Statement0, Statement),
		flatten_statement(Handler0, Handler),
		{ Stmt = try_commit(Ref, Statement, Handler) }
	;
		{ Stmt0 = atomic(AtomicStmt0) },
		fixup_atomic_stmt(AtomicStmt0, AtomicStmt),
		{ Stmt = atomic(AtomicStmt) }
	).

:- pred flatten_case(mlds__switch_case, mlds__switch_case,
		elim_info, elim_info).
:- mode flatten_case(in, out, in, out) is det.

flatten_case(Conds0 - Statement0, Conds - Statement) -->
	list__map_foldl(fixup_case_cond, Conds0, Conds),
	flatten_statement(Statement0, Statement).

:- pred flatten_default(mlds__switch_default, mlds__switch_default,
		elim_info, elim_info).
:- mode flatten_default(in, out, in, out) is det.

flatten_default(default_is_unreachable, default_is_unreachable) --> [].
flatten_default(default_do_nothing, default_do_nothing) --> [].
flatten_default(default_case(Statement0), default_case(Statement)) -->
	flatten_statement(Statement0, Statement).
	
%-----------------------------------------------------------------------------%

%
% flatten_nested_defns:
% flatten_nested_defn:
%	Hoist out nested function definitions and local variables
%	referenced by nested functions, storing them both in the elim_info.
%

:- pred flatten_nested_defns(mlds__defns, mlds__statements, mlds__defns,
		elim_info, elim_info).
:- mode flatten_nested_defns(in, in, out, in, out) is det.

flatten_nested_defns([], _, []) --> [].
flatten_nested_defns([Defn0 | Defns0], FollowingStatements, Defns) -->
	flatten_nested_defn(Defn0, Defns0, FollowingStatements, Defns1),
	flatten_nested_defns(Defns0, FollowingStatements, Defns2),
	{ Defns = list__append(Defns1, Defns2) }.

:- pred flatten_nested_defn(mlds__defn, mlds__defns, mlds__statements,
		mlds__defns, elim_info, elim_info).
:- mode flatten_nested_defn(in, in, in, out, in, out) is det.

flatten_nested_defn(Defn0, FollowingDefns, FollowingStatements, Defns) -->
	{ Defn0 = mlds__defn(Name, Context, Flags0, DefnBody0) },
	(
		{ DefnBody0 = mlds__function(PredProcId, Params, FuncBody0,
			Attributes) },
		%
		% recursively flatten the nested function
		%
		flatten_function_body(FuncBody0, FuncBody),

		%
		% mark the function as private / one_copy,
		% rather than as local / per_instance,
		% since we're about to hoist it out to the top level
		%
		{ Flags1 = set_access(Flags0, private) },
		{ Flags = set_per_instance(Flags1, one_copy) },

		{ DefnBody = mlds__function(PredProcId, Params, FuncBody,
			Attributes) },
		{ Defn = mlds__defn(Name, Context, Flags, DefnBody) },

		% Note that we assume that we can safely hoist stuff
		% inside nested functions into the containing function.
		% If that wasn't the case, we'd need code something
		% like this:
		/***************
		{ LocalVars = elim_info_get_local_data(ElimInfo) },
		{ OuterVars0 = elim_info_get_outer_vars(ElimInfo) },
		{ OuterVars = [LocalVars | OuterVars0] },
		{ FlattenedDefns = ml_elim_nested_defns(ModuleName,
			OuterVars, Defn0) },
		list__foldl(elim_info_add_nested_func, FlattenedDefns),
		***************/

		%
		% strip out the now flattened nested function,
		% and store it in the elim_info
		%
		elim_info_add_nested_func(Defn),
		{ Defns = [] }
	;
		{ DefnBody0 = mlds__data(_, _) },
		%
		% for local variable definitions, if they are
		% referenced by any nested functions, then
		% strip them out and store them in the elim_info
		%
		=(ElimInfo),
		{ ModuleName = elim_info_get_module_name(ElimInfo) },
		(
			(
				% For IL and Java, we need to hoist all
				% static constants out to the top level,
				% so that they can be initialized in the
				% class constructor.
				% To keep things consistent (and reduce
				% the testing burden), we do the same for
				% the other back-ends too.
				{ ml_decl_is_static_const(Defn0) }
			;
				{ Name = data(var(VarName)) },
				{ ml_should_add_local_data(ModuleName, VarName,
					FollowingDefns, FollowingStatements) }
			)
		->
			elim_info_add_local_data(Defn0),
			{ Defns = [] }
		;
			{ Defns = [Defn0] }
		)
	;
		{ DefnBody0 = mlds__class(_) },
		%
		% leave nested class declarations alone
		%
		% XXX that might not be the right thing to do,
		% but currently ml_code_gen.m doesn't generate
		% any of these, so it doesn't matter what we do
		%
		{ Defns = [Defn0] }
	).

	%
	% Succeed iff we should add the definition of this variable
	% to the local_data field of the ml_elim_info, meaning that
	% it should be added to the environment struct
	% (if it's a variable) or hoisted out to the top level
	% (if it's a static const).
	%
	% This checks for a nested function definition
	% or static initializer that references the variable.
	% This is conservative; for the MLDS->C and MLDS->GCC
	% back-ends, we only need to hoist out
	% static variables if they are referenced by
	% static initializers which themselves need to be
	% hoisted because they are referenced from a nested
	% function.  But checking the last part of that
	% is tricky, and for the Java and IL back-ends we
	% need to hoist out all static constants anyway,
	% so to keep things simple we do the same for the
	% C back-end to, i.e. we always hoist all static constants.
	%
:- pred ml_should_add_local_data(mlds_module_name, mlds__var_name,
		mlds__defns, mlds__statements).
:- mode ml_should_add_local_data(in, in, in, in) is semidet.

ml_should_add_local_data(ModuleName, VarName,
		FollowingDefns, FollowingStatements) :-
	QualVarName = qual(ModuleName, VarName),
	(
		list__member(FollowingDefn, FollowingDefns)
	;
		statements_contains_defn(FollowingStatements,
			FollowingDefn)
	),
	(
		FollowingDefn = mlds__defn(_, _, _,
			mlds__function(_, _, _, _)),
		defn_contains_var(FollowingDefn, QualVarName)
	;
		FollowingDefn = mlds__defn(_, _, _,
			mlds__data(_, Initializer)),
		ml_decl_is_static_const(FollowingDefn),
		initializer_contains_var(Initializer, QualVarName)
	).

%-----------------------------------------------------------------------------%

%
% fixup_atomic_stmt:
% fixup_case_cond:
% fixup_trail_op:
% fixup_rvals:
% fixup_maybe_rval:
% fixup_rval:
% fixup_lvals:
% fixup_lval:
%	Recursively process the specified construct, calling fixup_var on
%	every variable inside it.
%

:- pred fixup_atomic_stmt(mlds__atomic_statement, mlds__atomic_statement,
		elim_info, elim_info).
:- mode fixup_atomic_stmt(in, out, in, out) is det.

fixup_atomic_stmt(comment(C), comment(C)) --> [].
fixup_atomic_stmt(assign(Lval0, Rval0), assign(Lval, Rval)) -->
	fixup_lval(Lval0, Lval),
	fixup_rval(Rval0, Rval).
fixup_atomic_stmt(delete_object(Lval0), delete_object(Lval)) -->
	fixup_lval(Lval0, Lval).
fixup_atomic_stmt(new_object(Target0, MaybeTag, HasSecTag, Type, MaybeSize,
			MaybeCtorName, Args0, ArgTypes),
		new_object(Target, MaybeTag, HasSecTag, Type, MaybeSize,
			MaybeCtorName, Args, ArgTypes)) -->
	fixup_lval(Target0, Target),
	fixup_rvals(Args0, Args).
fixup_atomic_stmt(mark_hp(Lval0), mark_hp(Lval)) -->
	fixup_lval(Lval0, Lval).
fixup_atomic_stmt(restore_hp(Rval0), restore_hp(Rval)) -->
	fixup_rval(Rval0, Rval).
fixup_atomic_stmt(trail_op(TrailOp0), trail_op(TrailOp)) -->
	fixup_trail_op(TrailOp0, TrailOp).
fixup_atomic_stmt(inline_target_code(Lang, Components0),
		inline_target_code(Lang, Components)) -->
	list__map_foldl(fixup_target_code_component,
		Components0, Components).
fixup_atomic_stmt(outline_foreign_proc(Lang, Lvals, Code),
		outline_foreign_proc(Lang, Lvals, Code)) --> [].

:- pred fixup_case_cond(mlds__case_match_cond, mlds__case_match_cond,
		elim_info, elim_info).
:- mode fixup_case_cond(in, out, in, out) is det.

fixup_case_cond(match_value(Rval0), match_value(Rval)) -->
	fixup_rval(Rval0, Rval).
fixup_case_cond(match_range(Low0, High0), match_range(Low, High)) -->
	fixup_rval(Low0, Low),
	fixup_rval(High0, High).

:- pred fixup_target_code_component(target_code_component,
		target_code_component, elim_info, elim_info).
:- mode fixup_target_code_component(in, out, in, out) is det.

fixup_target_code_component(raw_target_code(Code, Attrs),
		raw_target_code(Code, Attrs)) --> [].
fixup_target_code_component(user_target_code(Code, Context, Attrs),
		user_target_code(Code, Context, Attrs)) --> [].
fixup_target_code_component(target_code_input(Rval0),
		target_code_input(Rval)) -->
	fixup_rval(Rval0, Rval).
fixup_target_code_component(target_code_output(Lval0),
		target_code_output(Lval)) -->
	fixup_lval(Lval0, Lval).
fixup_target_code_component(name(Name), name(Name)) --> [].

:- pred fixup_trail_op(trail_op, trail_op, elim_info, elim_info).
:- mode fixup_trail_op(in, out, in, out) is det.

fixup_trail_op(store_ticket(Lval0), store_ticket(Lval)) -->
	fixup_lval(Lval0, Lval).
fixup_trail_op(reset_ticket(Rval0, Reason), reset_ticket(Rval, Reason)) -->
	fixup_rval(Rval0, Rval).
fixup_trail_op(discard_ticket, discard_ticket) --> [].
fixup_trail_op(prune_ticket, prune_ticket) --> [].
fixup_trail_op(mark_ticket_stack(Lval0), mark_ticket_stack(Lval)) -->
	fixup_lval(Lval0, Lval).
fixup_trail_op(prune_tickets_to(Rval0), prune_tickets_to(Rval)) -->
	fixup_rval(Rval0, Rval).

:- pred fixup_rvals(list(mlds__rval), list(mlds__rval), elim_info, elim_info).
:- mode fixup_rvals(in, out, in, out) is det.

fixup_rvals([], []) --> [].
fixup_rvals([X0|Xs0], [X|Xs]) -->
	fixup_rval(X0, X),
	fixup_rvals(Xs0, Xs).

:- pred fixup_maybe_rval(maybe(mlds__rval), maybe(mlds__rval),
		elim_info, elim_info).
:- mode fixup_maybe_rval(in, out, in, out) is det.

fixup_maybe_rval(no, no) --> [].
fixup_maybe_rval(yes(Rval0), yes(Rval)) -->
	fixup_rval(Rval0, Rval).

:- pred fixup_rval(mlds__rval, mlds__rval, elim_info, elim_info).
:- mode fixup_rval(in, out, in, out) is det.

fixup_rval(lval(Lval0), lval(Lval)) -->
	fixup_lval(Lval0, Lval).
fixup_rval(mkword(Tag, Rval0), mkword(Tag, Rval)) -->
	fixup_rval(Rval0, Rval).
fixup_rval(const(Const), const(Const)) --> [].
fixup_rval(unop(Op, Rval0), unop(Op, Rval)) -->
	fixup_rval(Rval0, Rval).
fixup_rval(binop(Op, X0, Y0), binop(Op, X, Y)) -->
	fixup_rval(X0, X),
	fixup_rval(Y0, Y).
fixup_rval(mem_addr(Lval0), mem_addr(Lval)) -->
	fixup_lval(Lval0, Lval).
fixup_rval(self(T), self(T)) --> [].

:- pred fixup_lvals(list(mlds__lval), list(mlds__lval), elim_info, elim_info).
:- mode fixup_lvals(in, out, in, out) is det.

fixup_lvals([], []) --> [].
fixup_lvals([X0|Xs0], [X|Xs]) -->
	fixup_lval(X0, X),
	fixup_lvals(Xs0, Xs).

:- pred fixup_lval(mlds__lval, mlds__lval, elim_info, elim_info).
:- mode fixup_lval(in, out, in, out) is det.

fixup_lval(field(MaybeTag, Rval0, FieldId, FieldType, PtrType), 
		field(MaybeTag, Rval, FieldId, FieldType, PtrType)) --> 
	fixup_rval(Rval0, Rval).
fixup_lval(mem_ref(Rval0, Type), mem_ref(Rval, Type)) --> 
	fixup_rval(Rval0, Rval).
fixup_lval(var(Var0, VarType), VarLval) --> 
	fixup_var(Var0, VarType, VarLval).

%-----------------------------------------------------------------------------%

%
% fixup_var:
%	change up any references to local vars in the
%	containing function to go via the environment pointer
%

:- pred fixup_var(mlds__var, mlds__type, mlds__lval, elim_info, elim_info).
:- mode fixup_var(in, in, out, in, out) is det.

fixup_var(ThisVar, ThisVarType, Lval, ElimInfo, ElimInfo) :-
	ThisVar = qual(ThisVarModuleName, ThisVarName),
	ModuleName = elim_info_get_module_name(ElimInfo),
	Locals = elim_info_get_local_data(ElimInfo),
	ClassType = elim_info_get_env_type_name(ElimInfo),
	EnvPtrVarType = elim_info_get_env_ptr_type_name(ElimInfo),
	(
		%
		% Check for references to local variables
		% that are used by nested functions,
		% and replace them with `env_ptr->foo'.
		%
		ThisVarModuleName = ModuleName,
		IsLocalVar = (pred(VarType::out) is nondet :-
			list__member(Var, Locals),
			Var = mlds__defn(data(var(ThisVarName)), _, _, 
				data(VarType, _)),
			\+ ml_decl_is_static_const(Var)
			),
		solutions(IsLocalVar, [FieldType])
	->
		EnvPtr = lval(var(qual(ModuleName,
			mlds__var_name("env_ptr", no)),
			EnvPtrVarType)),
		EnvModuleName = ml_env_module_name(ClassType),
		ThisVarFieldName = ml_var_name_to_string(ThisVarName),
		FieldName = named_field(qual(EnvModuleName, ThisVarFieldName),
			EnvPtrVarType),
		Tag = yes(0),
		Lval = field(Tag, EnvPtr, FieldName, FieldType, EnvPtrVarType)
	;
		% Check for references to the env_ptr itself.
		% For those, the code generator will have left the
		% type as mlds__unknown_type, and we need to fill
		% it in here.
		ThisVarName = mlds__var_name("env_ptr", no),
		ThisVarType = mlds__unknown_type
	->
		Lval = var(ThisVar, EnvPtrVarType)
	;
		%
		% leave everything else unchanged
		%
		Lval = var(ThisVar, ThisVarType)
	).
/*****************************
The following code is what we would have to use if we couldn't
just hoist all local variables out to the outermost function.
	(
		%
		% Check for references to local variables
		% that are used by nested functions,
		% and replace them with `(&env)->foo'.
		% (The MLDS doesn't have any representation
		% for `env.foo'.)
		%
		ThisVarModuleName = ModuleName,
		list__member(Var, Locals),
		Var = mlds__defn(data(var(ThisVarName)), _, _, _)
	->
		Env = var(qual(ModuleName, "env")),
		FieldName = named_field(ThisVar),
		Tag = yes(0),
		Lval = field(Tag, mem_addr(Env), FieldName)
	;
		%
		% Check for references to variables in the
		% containing function(s), and replace them
		% with envptr->foo, envptr->envptr->foo, etc.
		% depending on the depth of nesting.
		%
		ThisVarModuleName = ModuleName,
		outervar_member(ThisVarName, OuterVars, 1, Depth)
	->
		EnvPtrName = qual(ModuleName, "env_ptr"),
		EnvPtr = lval(var(EnvPtrName)),
		Lval = make_envptr_ref(Depth, EnvPtr, EnvPtrName, ThisVar)
	;
		%
		% leave everything else unchanged
		%
		Lval = var(ThisVar, ThisVarType)
	).

	% check if the specified variable is contained in the
	% outervars, and if so, return the depth of nesting
	%
:- pred outervar_member(mlds__var_name, outervars, int, int).
:- mode outervar_member(in, in, in, out) is semidet.

outervar_member(ThisVarName, [OuterVars | OtherOuterVars], Depth0, Depth) :-
	(	
		list__member(Var, OuterVars),
		Var = mlds__defn(data(var(ThisVarName)), _, _, _)
	->
		Depth = Depth0
	;
		outervar_member(ThisVarName, OtherOuterVars, Depth0 + 1, Depth)
	).

	% Produce a reference to a variable via `Depth' levels
	% of `envptr->' indirections.
	%
:- func make_envptr_ref(int, mlds__rval, mlds__var, mlds__var) = lval.

make_envptr_ref(Depth, CurEnvPtr, EnvPtrVar, Var) = Lval :-
	( Depth = 1 ->
		Tag = yes(0),
		Lval = field(Tag, CurEnvPtr, named_field(Var))
	;
		Tag = yes(0),
		NewEnvPtr = lval(field(Tag, CurEnvPtr, named_field(EnvPtrVar))),
		Lval = make_envptr_ref(Depth - 1, NewEnvPtr, EnvPtrVar, Var)
	).
*********/

:- func ml_env_module_name(mlds__type) = mlds_module_name.
ml_env_module_name(ClassType) = EnvModuleName :-
	( ClassType = class_type(qual(ClassModule, ClassName), Arity, _Kind) ->
		EnvModuleName = mlds__append_class_qualifier(ClassModule,
			ClassName, Arity)
	;
		error("ml_env_module_name: ClassType is not a class")
	).

%-----------------------------------------------------------------------------%
%
% defns_contains_defn:
% defn_contains_defn:
% defn_body_contains_defn:
% maybe_statement_contains_defn:
% function_body_contains_defn:
% statements_contains_defn:
% statement_contains_defn:
%	Nondeterministically return all the definitions contained
%	in the specified construct.
%

:- pred defns_contains_defn(mlds__defns, mlds__defn).
:- mode defns_contains_defn(in, out) is nondet.

defns_contains_defn(Defns, Name) :-
	list__member(Defn, Defns),
	defn_contains_defn(Defn, Name).

:- pred defn_contains_defn(mlds__defn, mlds__defn).
:- mode defn_contains_defn(in, out) is multi.

defn_contains_defn(Defn, Defn).	/* this is where we succeed! */
defn_contains_defn(mlds__defn(_Name, _Context, _Flags, DefnBody), Defn) :-
	defn_body_contains_defn(DefnBody, Defn).

:- pred defn_body_contains_defn(mlds__entity_defn, mlds__defn).
:- mode defn_body_contains_defn(in, out) is nondet.

% defn_body_contains_defn(mlds__data(_Type, _Initializer), _Defn) :- fail.
defn_body_contains_defn(mlds__function(_PredProcId, _Params, FunctionBody,
		_Attrs), Name) :-
	function_body_contains_defn(FunctionBody, Name).
defn_body_contains_defn(mlds__class(ClassDefn), Name) :-
	ClassDefn = mlds__class_defn(_Kind, _Imports, _Inherits, _Implements,
		CtorDefns, FieldDefns),
	( defns_contains_defn(FieldDefns, Name)
	; defns_contains_defn(CtorDefns, Name)
	).

:- pred statements_contains_defn(mlds__statements, mlds__defn).
:- mode statements_contains_defn(in, out) is nondet.

statements_contains_defn(Statements, Defn) :-
	list__member(Statement, Statements),
	statement_contains_defn(Statement, Defn).

:- pred maybe_statement_contains_defn(maybe(mlds__statement), mlds__defn).
:- mode maybe_statement_contains_defn(in, out) is nondet.

% maybe_statement_contains_defn(no, _Defn) :- fail.
maybe_statement_contains_defn(yes(Statement), Defn) :-
	statement_contains_defn(Statement, Defn).

:- pred function_body_contains_defn(function_body, mlds__defn).
:- mode function_body_contains_defn(in, out) is nondet.

% function_body_contains_defn(external, _Defn) :- fail.
function_body_contains_defn(defined_here(Statement), Defn) :-
	statement_contains_defn(Statement, Defn).

:- pred statement_contains_defn(mlds__statement, mlds__defn).
:- mode statement_contains_defn(in, out) is nondet.

statement_contains_defn(Statement, Defn) :-
	Statement = mlds__statement(Stmt, _Context),
	stmt_contains_defn(Stmt, Defn).

:- pred stmt_contains_defn(mlds__stmt, mlds__defn).
:- mode stmt_contains_defn(in, out) is nondet.

stmt_contains_defn(Stmt, Defn) :-
	(
		Stmt = block(Defns, Statements),
		( defns_contains_defn(Defns, Defn)
		; statements_contains_defn(Statements, Defn)
		)
	;
		Stmt = while(_Rval, Statement, _Once),
		statement_contains_defn(Statement, Defn)
	;
		Stmt = if_then_else(_Cond, Then, MaybeElse),
		( statement_contains_defn(Then, Defn)
		; maybe_statement_contains_defn(MaybeElse, Defn)
		)
	;
		Stmt = switch(_Type, _Val, _Range, Cases, Default),
		( cases_contains_defn(Cases, Defn)
		; default_contains_defn(Default, Defn)
		)
	;
		Stmt = label(_Label),
		fail
	;
		Stmt = goto(_),
		fail
	;
		Stmt = computed_goto(_Rval, _Labels),
		fail
	;
		Stmt = call(_Sig, _Func, _Obj, _Args, _RetLvals, _TailCall),
		fail
	;
		Stmt = return(_Rvals),
		fail
	;
		Stmt = do_commit(_Ref),
		fail
	;
		Stmt = try_commit(_Ref, Statement, Handler),
		( statement_contains_defn(Statement, Defn)
		; statement_contains_defn(Handler, Defn)
		)
	;
		Stmt = atomic(_AtomicStmt),
		fail
	).

:- pred cases_contains_defn(list(mlds__switch_case), mlds__defn).
:- mode cases_contains_defn(in, out) is nondet.

cases_contains_defn(Cases, Defn) :-
	list__member(Case, Cases),
	Case = _MatchConds - Statement,
	statement_contains_defn(Statement, Defn).

:- pred default_contains_defn(mlds__switch_default, mlds__defn).
:- mode default_contains_defn(in, out) is nondet.

% default_contains_defn(default_do_nothing, _) :- fail.
% default_contains_defn(default_is_unreachable, _) :- fail.
default_contains_defn(default_case(Statement), Defn) :-
	statement_contains_defn(Statement, Defn).

%-----------------------------------------------------------------------------%

%
% defns_contains_var:
% defn_contains_var:
% defn_body_contains_var:
% function_body_contains_var:
% statements_contains_var:
% statement_contains_var:
% trail_op_contains_var:
% atomic_stmt_contains_var:
%	Succeeds iff the specified construct contains a reference to
%	the specified variable.
%

:- pred defns_contains_var(mlds__defns, mlds__var).
:- mode defns_contains_var(in, in) is semidet.

defns_contains_var(Defns, Name) :-
	list__member(Defn, Defns),
	defn_contains_var(Defn, Name).

:- pred defn_contains_var(mlds__defn, mlds__var).
:- mode defn_contains_var(in, in) is semidet.

defn_contains_var(mlds__defn(_Name, _Context, _Flags, DefnBody), Name) :-
	defn_body_contains_var(DefnBody, Name).

:- pred defn_body_contains_var(mlds__entity_defn, mlds__var).
:- mode defn_body_contains_var(in, in) is semidet.

defn_body_contains_var(mlds__data(_Type, Initializer), Name) :-
	initializer_contains_var(Initializer, Name).
defn_body_contains_var(mlds__function(_PredProcId, _Params, FunctionBody,
		_Attrs), Name) :-
	function_body_contains_var(FunctionBody, Name).
defn_body_contains_var(mlds__class(ClassDefn), Name) :-
	ClassDefn = mlds__class_defn(_Kind, _Imports, _Inherits, _Implements,
		CtorDefns, FieldDefns),
	( defns_contains_var(FieldDefns, Name)
	; defns_contains_var(CtorDefns, Name)
	).

:- pred maybe_statement_contains_var(maybe(mlds__statement), mlds__var).
:- mode maybe_statement_contains_var(in, in) is semidet.

% maybe_statement_contains_var(no, _) :- fail.
maybe_statement_contains_var(yes(Statement), Name) :-
	statement_contains_var(Statement, Name).

:- pred function_body_contains_var(function_body, mlds__var).
:- mode function_body_contains_var(in, in) is semidet.

% function_body_contains_var(external, _) :- fail.
function_body_contains_var(defined_here(Statement), Name) :-
	statement_contains_var(Statement, Name).
	
:- pred statements_contains_var(mlds__statements, mlds__var).
:- mode statements_contains_var(in, in) is semidet.

statements_contains_var(Statements, Name) :-
	list__member(Statement, Statements),
	statement_contains_var(Statement, Name).

:- pred statement_contains_var(mlds__statement, mlds__var).
:- mode statement_contains_var(in, in) is semidet.

statement_contains_var(Statement, Name) :-
	Statement = mlds__statement(Stmt, _Context),
	stmt_contains_var(Stmt, Name).

:- pred stmt_contains_var(mlds__stmt, mlds__var).
:- mode stmt_contains_var(in, in) is semidet.

stmt_contains_var(Stmt, Name) :-
	(
		Stmt = block(Defns, Statements),
		( defns_contains_var(Defns, Name)
		; statements_contains_var(Statements, Name)
		)
	;
		Stmt = while(Rval, Statement, _Once),
		( rval_contains_var(Rval, Name)
		; statement_contains_var(Statement, Name)
		)
	;
		Stmt = if_then_else(Cond, Then, MaybeElse),
		( rval_contains_var(Cond, Name)
		; statement_contains_var(Then, Name)
		; maybe_statement_contains_var(MaybeElse, Name)
		)
	;
		Stmt = switch(_Type, Val, _Range, Cases, Default),
		( rval_contains_var(Val, Name)
		; cases_contains_var(Cases, Name)
		; default_contains_var(Default, Name)
		)
	;
		Stmt = label(_Label),
		fail
	;
		Stmt = goto(_),
		fail
	;
		Stmt = computed_goto(Rval, _Labels),
		rval_contains_var(Rval, Name)
	;
		Stmt = call(_Sig, Func, Obj, Args, RetLvals, _TailCall),
		( rval_contains_var(Func, Name)
		; maybe_rval_contains_var(Obj, Name)
		; rvals_contains_var(Args, Name)
		; lvals_contains_var(RetLvals, Name)
		)
	;
		Stmt = return(Rvals),
		rvals_contains_var(Rvals, Name)
	;
		Stmt = do_commit(Ref),
		rval_contains_var(Ref, Name)
	;
		Stmt = try_commit(Ref, Statement, Handler),
		( lval_contains_var(Ref, Name)
		; statement_contains_var(Statement, Name)
		; statement_contains_var(Handler, Name)
		)
	;
		Stmt = atomic(AtomicStmt),
		atomic_stmt_contains_var(AtomicStmt, Name)
	).

:- pred cases_contains_var(list(mlds__switch_case), mlds__var).
:- mode cases_contains_var(in, in) is semidet.

cases_contains_var(Cases, Name) :-
	list__member(Case, Cases),
	Case = _MatchConds - Statement,
	statement_contains_var(Statement, Name).

:- pred default_contains_var(mlds__switch_default, mlds__var).
:- mode default_contains_var(in, in) is semidet.

% default_contains_var(default_do_nothing, _) :- fail.
% default_contains_var(default_is_unreachable, _) :- fail.
default_contains_var(default_case(Statement), Name) :-
	statement_contains_var(Statement, Name).

:- pred atomic_stmt_contains_var(mlds__atomic_statement, mlds__var).
:- mode atomic_stmt_contains_var(in, in) is semidet.

% atomic_stmt_contains_var(comment(_), _Name) :- fail.
atomic_stmt_contains_var(assign(Lval, Rval), Name) :-
	( lval_contains_var(Lval, Name)
	; rval_contains_var(Rval, Name)
	).
atomic_stmt_contains_var(new_object(Target, _MaybeTag, _HasSecTag, _Type,
		_MaybeSize, _MaybeCtorName, Args, _ArgTypes), Name) :-
	( lval_contains_var(Target, Name)
	; rvals_contains_var(Args, Name)
	).
atomic_stmt_contains_var(mark_hp(Lval), Name) :-
	lval_contains_var(Lval, Name).
atomic_stmt_contains_var(restore_hp(Rval), Name) :-
	rval_contains_var(Rval, Name).
atomic_stmt_contains_var(trail_op(TrailOp), Name) :-
	trail_op_contains_var(TrailOp, Name).
atomic_stmt_contains_var(inline_target_code(_Lang, Components), Name) :-
	list__member(Component, Components),
	target_code_component_contains_var(Component, Name).

:- pred trail_op_contains_var(trail_op, mlds__var).
:- mode trail_op_contains_var(in, in) is semidet.

trail_op_contains_var(store_ticket(Lval), Name) :-
	lval_contains_var(Lval, Name).
trail_op_contains_var(reset_ticket(Rval, _Reason), Name) :-
	rval_contains_var(Rval, Name).
% trail_op_contains_var(discard_ticket, _Name) :- fail.
% trail_op_contains_var(prune_ticket, _Name) :- fail.
trail_op_contains_var(mark_ticket_stack(Lval), Name) :-
	lval_contains_var(Lval, Name).
trail_op_contains_var(prune_tickets_to(Rval), Name) :-
	rval_contains_var(Rval, Name).

:- pred target_code_component_contains_var(target_code_component, mlds__var).
:- mode target_code_component_contains_var(in, in) is semidet.

%target_code_component_contains_var(raw_target_code(_Code), _Name) :-
%	fail.
%target_code_component_contains_var(user_target_code(_Code, _Context), _Name) :- 
%	fail.
target_code_component_contains_var(target_code_input(Rval), Name) :-
	rval_contains_var(Rval, Name).
target_code_component_contains_var(target_code_output(Lval), Name) :-
	lval_contains_var(Lval, Name).
target_code_component_contains_var(name(EntityName), VarName) :-
	EntityName = qual(ModuleName, data(var(UnqualVarName))),
	VarName = qual(ModuleName, UnqualVarName).

%-----------------------------------------------------------------------------%

%
% The elim_info type holds information that we use or accumulate
% as we traverse through the function body.
%

:- type elim_info
	--->	elim_info(
				% The name of the current module.
			module_name :: mlds_module_name,

				% The lists of local variables for
				% each of the containing functions,
				% innermost first
				% XXX this is not used.
				% It would be needed if we want to
				% handle arbitrary nesting.
				% Currently we assume that any variables
				% can safely be hoisted to the outermost
				% function, so this field is not needed.
			outer_vars :: outervars,

				% The list of nested function definitions
				% that we must hoist out.
				% This list is stored in reverse order.
			nested_funcs :: list(mlds__defn),

				% The list of local variables that we must
				% put in the environment structure
				% This list is stored in reverse order.
			local_data :: list(mlds__defn),
				
				% Type of the introduced environment struct
			env_type_name :: mlds__type,

				% Type of the introduced environment struct
				% pointer.  This might not just be just
				% a pointer to the env_type_name (in the
				% IL backend we don't necessarily use a
				% pointer).
			env_ptr_type_name :: mlds__type
	).

	% The lists of local variables for
	% each of the containing functions,
	% innermost first
:- type outervars == list(list(mlds__defn)).

:- func elim_info_init(mlds_module_name, outervars, mlds__type, mlds__type)
	= elim_info.
elim_info_init(ModuleName, OuterVars, EnvTypeName, EnvPtrTypeName) =
	elim_info(ModuleName, OuterVars, [], [], EnvTypeName, EnvPtrTypeName).

:- func elim_info_get_module_name(elim_info) = mlds_module_name.
elim_info_get_module_name(ElimInfo) = ElimInfo ^ module_name.

:- func elim_info_get_outer_vars(elim_info) = outervars.
elim_info_get_outer_vars(ElimInfo) = ElimInfo ^ outer_vars.

:- func elim_info_get_local_data(elim_info) = list(mlds__defn).
elim_info_get_local_data(ElimInfo) = ElimInfo ^ local_data.

:- func elim_info_get_env_type_name(elim_info) = mlds__type.
elim_info_get_env_type_name(ElimInfo) = ElimInfo ^ env_type_name.

:- func elim_info_get_env_ptr_type_name(elim_info) = mlds__type.
elim_info_get_env_ptr_type_name(ElimInfo) = ElimInfo ^ env_ptr_type_name.

:- pred elim_info_add_nested_func(mlds__defn, elim_info, elim_info).
:- mode elim_info_add_nested_func(in, in, out) is det.
elim_info_add_nested_func(NestedFunc, ElimInfo, 
	ElimInfo ^ nested_funcs := [NestedFunc | ElimInfo ^ nested_funcs]).

:- pred elim_info_add_local_data(mlds__defn, elim_info, elim_info).
:- mode elim_info_add_local_data(in, in, out) is det.
elim_info_add_local_data(LocalVar, ElimInfo,
	ElimInfo ^ local_data := [LocalVar | ElimInfo ^ local_data]).

:- pred elim_info_finish(elim_info, list(mlds__defn), list(mlds__defn)).
:- mode elim_info_finish(in, out, out) is det.
elim_info_finish(ElimInfo, Funcs, Locals) :-
	Funcs = list__reverse(ElimInfo ^ nested_funcs),
	Locals = list__reverse(ElimInfo ^ local_data).

%-----------------------------------------------------------------------------%
