%-----------------------------------------------------------------------------%
% Copyright (C) 1999-2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: ml_elim_nested.m
% Main author: fjh

% This module is an MLDS-to-MLDS transformation
% that has two functions:
% (1) eliminating nested functions
% (2) putting local variables that might contain pointers into
%     structs, and chaining these structs together,
%     for use with accurate garbage collection.
%
% The two transformations are quite similar,
% so they're both handled by the same code;
% a flag is passed to say which transformation
% should be done.
%
% The word "environment" (as in "environment struct" or "environment pointer")
% is used to refer to both the environment structs used when eliminating
% nested functions and also to the frame structs used for accurate GC.

% XXX Would it be possible to do both in a single pass?

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
% (1) eliminating nested functions
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

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
%-----------------------------------------------------------------------------%
% (2) accurate GC
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% SUMMARY
%
% This is an MLDS-to-MLDS transformation that transforms the
% MLDS code to add the information needed to do accurate GC
% when compiling to C (or to assembler).
%
% Basically what we do is to put all local variables that might
% contain pointers in structs, with one struct for each stack frame,
% and chain these structs together.  At GC time, we traverse the chain
% of structs.  This allows us to accurately scan the C stack.
%
% XXX Accurate GC is still not yet fully implemented.
% TODO:
%	- fix problem with undeclared local vars for some test cases
%	  (this seems to be due to the problems with higher-order code)
%	- handle `pragma export'
%	- support higher-order code: fix problems with tracing closures
%	  (the MLDS back-end doesn't generate closure layouts)
%	  and with tracing the wrapper functions generated for
%	  higher-order code.
%	- support type classes: same issues as for higher-order code, I think
%	- support --nondet-copy-out (see comment in flatten_nested_defn)
%	- support --high-level-data (fixup_newobj_in_atomic_statement
%	  gets the types wrong; see comment in ml_code_util.m)
%
% There are also some things that could be done to improve efficiency,
% e.g.
%	- optimize away temporary variables
%	- put stack_chain and/or heap pointer in global register variables
%	- avoid linking stack chain unnecessarily?
%
%-----------------------------------------------------------------------------%
%
% DETAILED DESCRIPTION
%
% For each function, we generate a struct for that function.
% Each such struct starts with a sub-struct containing a couple of
% fixed fields, which allow the GC to traverse the chain:
%
%	struct <function_name>_frame {
%		struct MR_StackChain fixed_fields;
%		...
%	};
%
% The fixed fields are as follows:
%
%	struct MR_StackChain {
%		struct MR_StackChain *prev;
%		void (*trace)(void *this_frame);
%	};
%		
% Actually, rather than using a nested structure,
% we just put these fields directly in the <function_name>_frame struct.
% (This turned out to be a little easier.)
%
% The prev field holds a link to the entry for this function's caller.
% The trace field is the address of a function to
% trace everything pointed to by this stack frame.
%
% To ensure that we don't try to traverse uninitialized fields,
% we zero-initialize each struct before inserting it into the chain.
%
% We need to keep a link to the topmost frame on the stack.
% There's two possible ways that we could handle this.
% One way is to pass it down as an parameter.
% Each function would get an extra parameter `stack_chain' which
% points to the caller's struct.
% An alternative approach is to just have a global variable
% `stack_chain' that points to the top of the stack.  We need extra code
% to set this pointer when entering and returning from functions.
% To make this approach thread-safe, the variable would actually
% need to be thread-local rather than global.
% This approach would probably work best if the variable is
% a GNU C global register variable, which would make it both
% efficient and thread-safe.
% XXX Currently, for simplicity, we're using a global variable.
%
% As an optimization, we ought to not bother allocating a struct for
% functions that don't have any variables that might contain pointers.
% We also ought to not bother allocating a struct for leaf functions that
% don't contain any functions calls or memory allocations.
% XXX These optimizations are not yet implemented!
%
%-----------------------------------------------------------------------------%
%
% EXAMPLE
%
% If we have a function
%
%	RetType
%	foo(Arg1Type arg1, Arg2Type arg2, ...)
%	{
%		Local1Type local1;
%		Local2Type local2;
%		...
%		local1 = MR_new_object(...);
%		...
%		bar(arg1, arg2, local1, &local2);
%		...
%	}
%
% where say Arg1Type and Local1Type might contain pointers,
% but Arg2Type and Local2Type don't, then we would transform it as follows:
%
%	struct foo_frame {
%		MR_StackChain fixed_fields;
%		Arg1Type arg1;
%		Local1Type local1;
%		...
%	};
%
%	static void
%	foo_trace(void *this_frame) {
%		struct foo_frame *frame = (struct foo_frame *)this_frame;
%
%		... code to construct TypeInfo for type of arg1 ...
%		mercury__private_builtin__gc_trace_1_p_0(
%			<TypeInfo for type of arg1>, &frame->arg1);
%
%		... code to construct TypeInfo for type of local1 ...
%		mercury__private_builtin__gc_trace_1_p_0(
%			<TypeInfo for type of local1>, &frame->local1);
%
%		...
%	}
%
%	RetType
%	foo(Arg1Type arg1, Arg2Type arg2, ...)
%	{
%		struct foo_frame this_frame;
%		Local2Type local2;
%		
%		this_frame.fixed_fields.prev = stack_chain;
%		this_frame.fixed_fields.trace = foo_trace;
%		this_frame.arg1 = arg1;
%		this_frame.local1 = NULL;
%		stack_chain = &this_frame;
%
%		GC_check();
%		
%		...
%		this_frame.local1 = MR_new_object(...);
%		...
%		bar(this_frame.arg1, arg2, this_frame.local1, &local2);
%		...
%		stack_chain = stack_chain->prev;
%	}
%
% Alternatively, if we were passing stack_chain as an argument,
% rather than treating it as a global variable, then the generated
% code for foo() would look like this:
%
%	RetType
%	foo(struct MR_StackChain *stack_chain,
%		Arg1Type arg1, Arg2Type arg2, ...)
%	{
%		struct foo_frame this_frame;
%		Local2Type local2;
%		
%		this_frame.fixed_fields.prev = stack_chain;
%		this_frame.fixed_fields.trace = foo_trace;
%		this_frame.arg1 = arg1;
%		this_frame.local1 = NULL;
%
%		GC_check();
%		
%		...
%		this_frame.local1 = MR_new_object(&this_frame, ...);
%		...
%		bar(&this_frame, this_frame.arg1, arg2,
%			this_frame.local1, &local2);
%		...
%		/* no need to explicitly unchain the stack frame here */
%	}
%
% Currently, rather than initializing the fields of `this_frame'
% using a sequence of assignment statements, we actually just use
% an initializer:
%		struct foo_frame this_frame = { stack_chain };
% This implicitly zeros out the remaining fields.
% Only the non-null fields, i.e. the arguments and the trace
% field, need to be explicitly assigned using assignment statements.
%
% The code in the Mercury runtime to traverse the stack frames would
% look something like this:
%
%	void
%	MR_traverse_stack(struct MR_StackChain *stack_chain)
%	{
%		while (stack_chain != NULL) {
%			(*stack_chain->trace)(stack_chain);
%			stack_chain = stack_chain->prev;
%		}
%	}
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module ml_elim_nested.

:- interface.

:- import_module mlds.
:- import_module io.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- type action
	--->	hoist_nested_funcs	% Eliminate nested functions
	;	chain_gc_stack_frames.  % Add shadow stack for supporting
					% accurate GC.

	% Process the whole MLDS, performing the indicated action.
:- pred ml_elim_nested(action, mlds, mlds, io__state, io__state).
:- mode ml_elim_nested(in, in, out, di, uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module bool, counter, int, list, std_util, string, require.

:- import_module ml_code_util, ml_util.
:- import_module prog_util, type_util.

% the following imports are needed for mangling pred names
:- import_module hlds_pred, prog_data, prog_out.

:- import_module globals, options.

	% Perform the specified action on the whole MLDS.
	%
ml_elim_nested(Action, MLDS0, MLDS) -->
	globals__io_get_globals(Globals),
	{ MLDS0 = mlds(ModuleName, ForeignCode, Imports, Defns0) },
	{ MLDS = mlds(ModuleName, ForeignCode, Imports, Defns) },
	{ MLDS_ModuleName = mercury_module_name_to_mlds(ModuleName) },
	{ OuterVars = [] },
	{ DefnsList = list__map(
		ml_elim_nested_defns(Action, MLDS_ModuleName, Globals,
			OuterVars),
		Defns0) },
	{ Defns = list__condense(DefnsList) }.

	% Either eliminated nested functions:
	% Hoist out any nested function occurring in a single mlds__defn.
	% Return a list of mlds__defns that contains no nested functions.
	%
	% Or handle accurate GC:
	% put all variables that might contain pointers in structs
	% and chain these structs together into a "shadow stack".
	% Extract out the code to trace these variables,
	% putting it in a function whose address is stored in
	% the shadow stack frame.
	%
:- func ml_elim_nested_defns(action, mlds_module_name, globals, outervars,
		mlds__defn) = list(mlds__defn).
ml_elim_nested_defns(Action, ModuleName, Globals, OuterVars, Defn0)
		= Defns :-
	Defn0 = mlds__defn(Name, Context, Flags, DefnBody0),
	(
		DefnBody0 = mlds__function(PredProcId, Params0,
			defined_here(FuncBody0), Attributes)
	->
		EnvName = ml_env_name(Name, Action),
		EnvTypeName = ml_create_env_type_name(EnvName,
			 ModuleName, Globals),
		EnvPtrTypeName = ml_make_env_ptr_type(Globals, EnvTypeName),

		%
		% traverse the function body, finding (and removing)
		% any nested functions, and fixing up any references
		% to the arguments or to local variables or local
		% static constants that need to be put in the environment
		% structure (e.g. because they occur in nested functions,
		% or to make them visible to the garbage collector)
		%
		% Also, for accurate GC, add code to save and restore the
		% stack chain pointer at any `try_commit' statements.
		%
		ElimInfo0 = elim_info_init(Action, ModuleName,
			OuterVars, EnvTypeName, EnvPtrTypeName),
		Params0 = mlds__func_params(Arguments0, RetValues),
		ml_maybe_add_args(Arguments0, FuncBody0, ModuleName,
			Context, ElimInfo0, ElimInfo1),
		flatten_arguments(Arguments0, Arguments1, ElimInfo1, ElimInfo2),
		flatten_statement(FuncBody0, FuncBody1, ElimInfo2, ElimInfo),
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
		HoistedStatics = list__map(convert_local_to_global,
			LocalStatics),


		%
		% When hoisting nested functions,
		% if there were no nested functions, then we just
		% hoist the local static constants
		%
		(
			Action = hoist_nested_funcs,
			NestedFuncs0 = []
		->
			FuncBody = FuncBody1,
			HoistedDefns = HoistedStatics
		;
			%
			% Create a struct to hold the local variables,
			% and initialize the environment pointers for
			% both the containing function and the nested
			% functions.  Also generate the GC tracing function,
			% if Action = chain_gc_stack_frames.
			%
			ml_create_env(Action, EnvName, EnvTypeName, LocalVars,
				Context, ModuleName, Name, Globals,
				EnvTypeDefn, EnvDecls, InitEnv,
				GCTraceFuncDefns),
			list__map_foldl(
				ml_insert_init_env(Action, EnvTypeName,
					ModuleName, Globals),
					NestedFuncs0, NestedFuncs,
					no, InsertedEnv),

			% Hoist out the local statics and the nested functions
			HoistedDefns0 = HoistedStatics ++ GCTraceFuncDefns ++
				NestedFuncs,

			% 
			% When hoisting nested functions,
			% it's possible that none of the nested
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
			(
				Action = hoist_nested_funcs,
				InsertedEnv = no
			->
				FuncBody = FuncBody1,
				HoistedDefns = HoistedDefns0
			;
				%
				% If the function's arguments are
				% referenced by nested functions,
				% or (for accurate GC) may contain pointers,
				% then we need to copy them to local
				% variables in the environment
				% structure.
				%
				ml_maybe_copy_args(Arguments1, FuncBody0,
					ElimInfo, EnvTypeName, EnvPtrTypeName,
					Context, _ArgsToCopy, CodeToCopyArgs),

				%
				% for accurate GC,
				% add a call to GC_check() at start of every
				% function that might allocate memory
				%
				% (XXX we could perhaps reduce the overhead of
				% this slightly by only doing it for
				% possibly-recursive functions, i.e.
				% by not doing it for leaf functions).
				% 
				(
					Action = chain_gc_stack_frames,
					statement_contains_statement(FuncBody2,
						NewObject),
					NewObject = mlds__statement(atomic(
					    new_object(_, _, _, _, _, _, _, _)
					    ), _)
				->
					GC_Check = [mlds__statement(
						atomic(gc_check), Context)]
				;
					GC_Check = []
				),

				%
				% Insert code to unlink this stack frame
				% before doing any tail calls or returning
				% from the function, either explicitly
				% or implicitly.
				%
					% add unlink statements before
					% any explicit returns or tail calls
				( Action = chain_gc_stack_frames ->
					add_unchain_stack_to_statement(
						FuncBody1, FuncBody2,
						ElimInfo, _ElimInfo)
				;
					FuncBody2 = FuncBody1
				),
					% add a final unlink statement
					% at the end of the function,
					% if needed.  This is only needed if
					% the function has no return values --
					% if there is a return value, then the
					% function must exit with an explicit
					% return statement.
				(
					Action = chain_gc_stack_frames,
					RetValues = []
				->
					UnchainFrame = [ml_gen_unchain_frame(
						Context, ElimInfo)]
				;
					UnchainFrame = []
				),

				%
				% insert the definition and
				% initialization of the environment
				% struct variable at the start of the
				% top-level function's body,
				% and append the final unlink statement
				% (if any) at the end
				%
				FuncBody = ml_block(EnvDecls,
					InitEnv ++ CodeToCopyArgs ++ GC_Check ++
					[FuncBody2] ++ UnchainFrame,
					Context),
				%
				% insert the environment struct type at
				% the start of the list of hoisted definitions
				% (preceding the previously nested functions
				% and static constants in HoistedDefns0),
				%
				HoistedDefns = [EnvTypeDefn | HoistedDefns0]
			)
		),
		(	
			Action = chain_gc_stack_frames,
			% This pass will have put the GC tracing code for the
			% arguments in the GC tracing function.  So we don't
			% need the GC tracing code annotation on the arguments
			% anymore.  We delete them here, because otherwise
			% the `#if 0 ... #endif' blocks output for the
			% annotations clutter up the generated C files.
			Arguments = list__map(strip_gc_trace_code, Arguments1)
		;
			Action = hoist_nested_funcs,
			Arguments = Arguments1
		),
		Params = mlds__func_params(Arguments, RetValues),
		DefnBody = mlds__function(PredProcId, Params,
			defined_here(FuncBody), Attributes),
		Defn = mlds__defn(Name, Context, Flags, DefnBody),
		Defns = list__append(HoistedDefns, [Defn])
	;
		% leave definitions of things other than functions unchanged
		Defns = [Defn0]
	).

:- func strip_gc_trace_code(mlds__argument) = mlds__argument.
strip_gc_trace_code(Argument0) = Argument :-
	Argument0 = mlds__argument(Name, Type, _MaybeGCTraceCode),
	Argument = mlds__argument(Name, Type, no).

	%
	% Add any arguments which are used in nested functions
	% to the local_data field in the elim_info.
	%
:- pred ml_maybe_add_args(mlds__arguments, mlds__statement,
		mlds_module_name, mlds__context, elim_info, elim_info).
:- mode ml_maybe_add_args(in, in, in, in, in, out) is det.

ml_maybe_add_args([], _, _, _) --> [].
ml_maybe_add_args([Arg|Args], FuncBody, ModuleName, Context) -->
	=(ElimInfo),
	(
		{ Arg = mlds__argument(data(var(VarName)), _Type,
			GC_TraceCode) },
		{ ml_should_add_local_data(ElimInfo, VarName, GC_TraceCode,
			[], [FuncBody]) }
	->
		{ ml_conv_arg_to_var(Context, Arg, ArgToCopy) },
		elim_info_add_and_flatten_local_data(ArgToCopy)
	;
		[]
	),
	ml_maybe_add_args(Args, FuncBody, ModuleName, Context).

	%
	% Generate code to copy any arguments which are used in nested functions
	% to the environment struct.
	%
:- pred ml_maybe_copy_args(mlds__arguments, mlds__statement,
		elim_info, mlds__type, mlds__type, mlds__context, 
		mlds__defns, mlds__statements).
:- mode ml_maybe_copy_args(in, in, in, in, in, in, out, out) is det.

ml_maybe_copy_args([], _, _, _, _, _, [], []).
ml_maybe_copy_args([Arg|Args], FuncBody, ElimInfo, ClassType, EnvPtrTypeName,
		Context, ArgsToCopy, CodeToCopyArgs) :-
	ml_maybe_copy_args(Args, FuncBody, ElimInfo, ClassType,
		EnvPtrTypeName,	Context, ArgsToCopy0, CodeToCopyArgs0),
	ModuleName = elim_info_get_module_name(ElimInfo),
	(
		Arg = mlds__argument(data(var(VarName)), FieldType,
			GC_TraceCode),
		ml_should_add_local_data(ElimInfo, VarName, GC_TraceCode,
			[], [FuncBody])
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
		EnvPtrName = env_name_base(ElimInfo ^ action) ++ "_ptr",
		EnvPtr = lval(var(qual(ModuleName,
				mlds__var_name(EnvPtrName, no)),
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

	% Create the environment struct type.
:- func ml_create_env_type_name(mlds__class_name, mlds_module_name, globals) =
	mlds__type.
ml_create_env_type_name(EnvClassName, ModuleName, Globals) = EnvTypeName :-
		% If we're allocating it on the heap, then we need to use 
		% a class type rather than a struct (value type).
		% This is needed for verifiable code on the IL back-end.
	globals__lookup_bool_option(Globals, put_nondet_env_on_heap, OnHeap),
	( OnHeap = yes ->
		EnvTypeKind = mlds__class
	;
		EnvTypeKind = mlds__struct
	),
	EnvTypeName = class_type(qual(ModuleName, EnvClassName), 0,
		EnvTypeKind).

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
:- pred ml_create_env(action, mlds__class_name, mlds__type, list(mlds__defn),
		mlds__context, mlds_module_name, mlds__entity_name, globals,
		mlds__defn, list(mlds__defn), list(mlds__statement),
		list(mlds__defn)).
:- mode ml_create_env(in, in, in, in, in, in, in, in, out, out, out, out)
		is det.

ml_create_env(Action, EnvClassName, EnvTypeName, LocalVars, Context,
		ModuleName, FuncName, Globals, EnvTypeDefn, EnvDecls, InitEnv,
		GCTraceFuncDefns) :-
	%
	% generate the following type:
	%
	%	struct <EnvClassName> {
	%	  #ifdef ACCURATE_GC
	%		/* these fixed fields match `struct MR_StackChain' */
	%		void *prev;
	%		void (*trace)(...);
	%	  #endif
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
	EnvTypeEntityName = type(EnvClassName, 0),
	EnvTypeFlags = env_type_decl_flags,
	Fields0 = list__map(convert_local_to_field, LocalVars),

	%
	% Extract the GC tracing code from the fields
	%
	list__map2(extract_gc_trace_code, Fields0, Fields1,
		GC_TraceStatements0),
	GC_TraceStatements = list__condense(GC_TraceStatements0),

	( Action = chain_gc_stack_frames ->
		ml_chain_stack_frames(Fields1, GC_TraceStatements,
			EnvTypeName, Context, FuncName, ModuleName, Globals,
			Fields, EnvInitializer, LinkStackChain,
			GCTraceFuncDefns),
		GC_TraceEnv = no
	;
		( GC_TraceStatements = [] ->
			GC_TraceEnv = no
		;
			GC_TraceEnv = yes(
				ml_block([], GC_TraceStatements, Context)
			)
		),
		Fields = Fields1,
		EnvInitializer = no_initializer,
		LinkStackChain = [],
		GCTraceFuncDefns = []
	),

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
	%	struct <EnvClassName> env; // = { ... }
	%
	EnvVarName = var_name(env_name_base(Action), no),
	EnvVarEntityName = data(var(EnvVarName)),
	EnvVarFlags = ml_gen_local_var_decl_flags,
	EnvVarDefnBody = mlds__data(EnvTypeName, EnvInitializer, GC_TraceEnv),
	EnvVarDecl = mlds__defn(EnvVarEntityName, Context, EnvVarFlags,
		EnvVarDefnBody),

	%
	% declare the `env_ptr' var, and
	% initialize the `env_ptr' with the address of `env'
	%
	EnvVar = qual(ModuleName, EnvVarName),

	%
	% generate code to initialize the environment pointer,
	% either by allocating an object on the heap, or by
	% just taking the address of the struct we put on the stack
	%
	( OnHeap = yes ->
		EnvVarAddr = lval(var(EnvVar, EnvTypeName)),
		NewObj = [mlds__statement(
				atomic(new_object(
					var(EnvVar, EnvTypeName), 
					no, no, EnvTypeName, no, no, [], [])),
				Context)]
	;
		EnvVarAddr = mem_addr(var(EnvVar, EnvTypeName)),
		NewObj = []
	),
	ml_init_env(Action, EnvTypeName, EnvVarAddr, Context, ModuleName,
		Globals, EnvPtrVarDecl, InitEnv0),
	EnvDecls = [EnvVarDecl, EnvPtrVarDecl],
	InitEnv = NewObj ++ [InitEnv0] ++ LinkStackChain.

:- pred ml_chain_stack_frames(mlds__defns, mlds__statements, mlds__type,
		mlds__context, mlds__entity_name, mlds_module_name, globals,
		mlds__defns, mlds__initializer, mlds__statements, mlds__defns).
:- mode ml_chain_stack_frames(in, in, in, in, in, in, in, out, out, out, out)
		is det.

ml_chain_stack_frames(Fields0, GCTraceStatements, EnvTypeName, Context,
		FuncName, ModuleName, Globals, Fields,
		EnvInitializer, LinkStackChain, GCTraceFuncDefns) :-
	%
	% Generate code to declare and initialize the
	% environment pointer for the GC trace function
	% from that function's `this_frame' parameter:
	%
	%	struct foo_frame *frame;
	%	frame = (struct foo_frame *) this_frame;
	%
	ThisFrameName = qual(ModuleName, var_name("this_frame", no)),
	ThisFrameRval = lval(var(ThisFrameName,
		mlds__generic_type)),
	CastThisFrameRval = unop(cast(mlds__ptr_type(EnvTypeName)),
		ThisFrameRval),
	ml_init_env(chain_gc_stack_frames, EnvTypeName, CastThisFrameRval,
		Context, ModuleName, Globals, FramePtrDecl, InitFramePtr),

	%
	% Put the environment pointer declaration and initialization
	% and the GC tracing code in a function:
	%
	%	void foo_trace(void *this_frame) {
	%		struct foo_frame *frame;
	%		frame = (struct foo_frame *) this_frame;
	%		<GCTraceStatements>
	%	}
	%
	gen_gc_trace_func(FuncName, ModuleName,
		FramePtrDecl, [InitFramePtr | GCTraceStatements],
		Context, GCTraceFuncAddr, GCTraceFuncParams,
		GCTraceFuncDefn),
	GCTraceFuncDefns = [GCTraceFuncDefn],

	%
	% insert the fixed fields in the struct <EnvClassName>:
	%	void *prev;
	%	void (*trace)(...);
	%
	PrevFieldName = data(var(var_name("prev", no))),
	PrevFieldFlags = ml_gen_public_field_decl_flags,
	PrevFieldType = ml_stack_chain_type,
	PrevFieldDefnBody = mlds__data(PrevFieldType,
		no_initializer, no),
	PrevFieldDecl = mlds__defn(PrevFieldName, Context,
			PrevFieldFlags, PrevFieldDefnBody),

	TraceFieldName = data(var(var_name("trace", no))),
	TraceFieldFlags = ml_gen_public_field_decl_flags,
	TraceFieldType = mlds__func_type(GCTraceFuncParams),
	TraceFieldDefnBody = mlds__data(TraceFieldType,
		no_initializer, no),
	TraceFieldDecl = mlds__defn(TraceFieldName, Context,
			TraceFieldFlags, TraceFieldDefnBody),

	Fields = [PrevFieldDecl, TraceFieldDecl | Fields0],

	%
	% Set the initializer so that the `prev' field
	% is initialized to the global stack chain,
	% and the `trace' field is initialized to the
	% address of the GC tracing function:
	%
	%	... = { stack_chain, foo_trace };
	%
	% Since there no values for the remaining fields in the
	% initializer, this means the remaining fields will get
	% initialized to zero (C99 6.7.8 #21).
	%
	% XXX This uses a non-const initializer, which is a
	% feature that is only supported in C99 and GNU C;
	% it won't work in C89.  We should just generate
	% a bunch of assignments to all the fields,
	% rather than relying on initializers like this.
	%
	StackChain = ml_stack_chain_var,
	EnvInitializer = init_struct([
		init_obj(lval(StackChain)),
		init_obj(const(code_addr_const(GCTraceFuncAddr)))
	]),

	%
	% Generate code to set the global stack chain
	% to point to the current environment:
	%
	%	 stack_chain = frame_ptr;
	%
	EnvPtrTypeName = ml_make_env_ptr_type(Globals, EnvTypeName),
	EnvPtr = lval(var(qual(ModuleName,
			mlds__var_name("frame_ptr", no)),
		EnvPtrTypeName)),
	AssignToStackChain = assign(StackChain, EnvPtr),
	LinkStackChain = [mlds__statement(atomic(AssignToStackChain),
		Context)].

:- pred gen_gc_trace_func(mlds__entity_name, mlds_module_name,
		mlds__defn, list(mlds__statement), mlds__context,
		mlds__code_addr, mlds__func_params, mlds__defn).
:- mode gen_gc_trace_func(in, in, in, in, in, out, out, out) is det.

gen_gc_trace_func(FuncName, PredModule, FramePointerDecl, GCTraceStatements,
		Context, GCTraceFuncAddr, FuncParams, GCTraceFuncDefn) :-
	%
	% Compute the signature of the GC tracing function
	%
	ArgName = data(var(var_name("this_frame", no))),
	ArgType = mlds__generic_type,
	Argument = mlds__argument(ArgName, ArgType, no),
	FuncParams = mlds__func_params([Argument], []),
	Signature = mlds__get_func_signature(FuncParams),
	%
	% Compute the name of the GC tracing function
	%
	% To compute the name, we just take the name of the original function
	% and add 100000 to the original function's sequence number.
	% XXX This is a bit of a hack; maybe we should add
	% another field to the `function' ctor for mlds__entity_name.
	%
	( FuncName = function(PredLabel, ProcId, MaybeSeqNum, PredId) ->
		( MaybeSeqNum = yes(SeqNum)
		; MaybeSeqNum = no, SeqNum = 0
		),
		NewSeqNum = SeqNum + 100000,
		GCTraceFuncName = function(PredLabel, ProcId, yes(NewSeqNum),
			PredId),
		ProcLabel = qual(PredModule, PredLabel - ProcId),
		GCTraceFuncAddr = internal(ProcLabel, NewSeqNum, Signature)
	;
		error("gen_gc_trace_func: not a function")
	),
	%
	% Construct the function definition
	%
	Statement = mlds__statement(
		block([FramePointerDecl], GCTraceStatements),
		Context),
	DeclFlags = ml_gen_gc_trace_func_decl_flags,
	MaybePredProcId = no,
	Attributes = [],
	FuncDefn = function(MaybePredProcId, FuncParams, 
		defined_here(Statement), Attributes),
	GCTraceFuncDefn = mlds__defn(GCTraceFuncName, Context, DeclFlags,
		FuncDefn).

	% Return the declaration flags appropriate for a procedure definition.
	%
:- func ml_gen_gc_trace_func_decl_flags = mlds__decl_flags.
ml_gen_gc_trace_func_decl_flags = MLDS_DeclFlags :-
	Access = private,
	PerInstance = one_copy,
	Virtuality = non_virtual,
	Finality = overridable,
	Constness = modifiable,
	Abstractness = concrete,
	MLDS_DeclFlags = init_decl_flags(Access, PerInstance,
		Virtuality, Finality, Constness, Abstractness).

:- pred extract_gc_trace_code(mlds__defn, mlds__defn, mlds__statements).
:- mode extract_gc_trace_code(in, out, out) is det.

extract_gc_trace_code(mlds__defn(Name, Context, Flags, Body0),
		mlds__defn(Name, Context, Flags, Body), GCTraceStmts) :-
	( Body0 = data(Type, Init, yes(GCTraceStmt)) ->
		Body = data(Type, Init, no),
		GCTraceStmts = [GCTraceStmt]
	;
		Body = Body0,
		GCTraceStmts = []
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
:- pred ml_insert_init_env(action, mlds__type, mlds_module_name, globals,
		mlds__defn, mlds__defn, bool, bool).
:- mode ml_insert_init_env(in, in, in, in, in, out, in, out) is det.
ml_insert_init_env(Action, TypeName, ModuleName, Globals, Defn0, Defn,
		Init0, Init) :-
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

		ml_init_env(Action, TypeName, CastEnvPtrVal, Context,
			ModuleName, Globals, EnvPtrDecl, InitEnvPtr),
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
:- pred ml_init_env(action, mlds__type, mlds__rval,
		mlds__context, mlds_module_name, globals,
		mlds__defn, mlds__statement).
:- mode ml_init_env(in, in, in, in, in, in, out, out) is det.

ml_init_env(Action, EnvTypeName, EnvPtrVal, Context, ModuleName, Globals,
		EnvPtrVarDecl, InitEnvPtr) :-
	%
	% generate the following variable declaration:
	%
	%	<EnvTypeName> *env_ptr;
	%
	EnvPtrVarName = mlds__var_name(env_name_base(Action) ++ "_ptr", no),
	EnvPtrVarEntityName = data(var(EnvPtrVarName)),
	EnvPtrVarFlags = ml_gen_local_var_decl_flags,
	EnvPtrVarType = ml_make_env_ptr_type(Globals, EnvTypeName),
	% The env_ptr never needs to be traced by the GC,
	% since the environment that it points to will always
	% be on the stack, not into the heap.
	GC_TraceCode = no,
	EnvPtrVarDefnBody = mlds__data(EnvPtrVarType, no_initializer,
		GC_TraceCode),
	EnvPtrVarDecl = mlds__defn(EnvPtrVarEntityName, Context,
		EnvPtrVarFlags, EnvPtrVarDefnBody),

	%
	% generate the following statement:
	%
	%	env_ptr = <EnvPtrVal>;
	%
	% (note that the caller of this routine is responsible
	% for inserting a cast in <EnvPtrVal> if needed).
	%
	EnvPtrVar = qual(ModuleName, EnvPtrVarName),
	AssignEnvPtr = assign(var(EnvPtrVar, EnvPtrVarType), EnvPtrVal),
	InitEnvPtr = mlds__statement(atomic(AssignEnvPtr), Context).

	% Given the declaration for a function parameter, produce a
	% declaration for a corresponding local variable or environment
	% struct field.  We need to do this so as to include function
	% parameter in the environment struct.
	%
:- pred ml_conv_arg_to_var(mlds__context, mlds__argument, mlds__defn).
:- mode ml_conv_arg_to_var(in, in, out) is det.

ml_conv_arg_to_var(Context, Arg, LocalVar) :-
	Arg = mlds__argument(Name, Type, GC_TraceCode),
	Flags = ml_gen_local_var_decl_flags,
	DefnBody = mlds__data(Type, no_initializer, GC_TraceCode),
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
		
:- func ml_stack_chain_var = mlds__lval.
ml_stack_chain_var = StackChain :-
	mercury_private_builtin_module(PrivateBuiltin),
	MLDS_Module = mercury_module_name_to_mlds(PrivateBuiltin),
	StackChain = var(qual(MLDS_Module, var_name("stack_chain", no)),
		ml_stack_chain_type).

	% the type of the `stack_chain' pointer, i.e. `void *'.
:- func ml_stack_chain_type = mlds__type.
ml_stack_chain_type = mlds__generic_env_ptr_type.

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
:- func ml_env_name(mlds__entity_name, action) = mlds__class_name.

ml_env_name(type(_, _), _) = _ :-
	error("ml_env_name: expected function, got type").
ml_env_name(data(_), _) = _ :-
	error("ml_env_name: expected function, got data").
ml_env_name(function(PredLabel, ProcId, MaybeSeqNum, _PredId),
		Action) = ClassName :-
	Base = env_name_base(Action),
	PredLabelString = ml_pred_label_name(PredLabel),
	proc_id_to_int(ProcId, ModeNum),
	( MaybeSeqNum = yes(SeqNum) ->
		string__format("%s_%d_%d_%s",
			[s(PredLabelString), i(ModeNum), i(SeqNum), s(Base)],
			ClassName)
	;
		string__format("%s_%d_%s",
			[s(PredLabelString), i(ModeNum), s(Base)],
			ClassName)
	).
ml_env_name(export(_), _) = _ :-
	error("ml_env_name: expected function, got export").

:- func env_name_base(action) = string.
env_name_base(chain_gc_stack_frames) = "frame".
env_name_base(hoist_nested_funcs) = "env".

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
% flatten_arguments:
% flatten_argument:
% flatten_function_body:
% flatten_maybe_statement:
% flatten_statements:
% flatten_statement:
%	Recursively process the statement(s), calling fixup_var on every
%	use of a variable inside them, and calling flatten_nested_defns
%	for every definition they contain (e.g. definitions of local
%	variables and nested functions).
%
%	Also, for Action = chain_gc_stack_frames, add code to save and
%	restore the stack chain pointer at any `try_commit' statements.
%

:- pred flatten_arguments(mlds__arguments, mlds__arguments,
		elim_info, elim_info).
:- mode flatten_arguments(in, out, in, out) is det.

flatten_arguments(Arguments0, Arguments) -->
	list__map_foldl(flatten_argument, Arguments0, Arguments).

:- pred flatten_argument(mlds__argument, mlds__argument,
		elim_info, elim_info).
:- mode flatten_argument(in, out, in, out) is det.

flatten_argument(Argument0, Argument) -->
	{ Argument0 = mlds__argument(Name, Type, MaybeGCTraceCode0) },
	{ Argument = mlds__argument(Name, Type, MaybeGCTraceCode) },
	flatten_maybe_statement(MaybeGCTraceCode0, MaybeGCTraceCode).

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
		flatten_statement(Statement0, Statement1),
		flatten_statement(Handler0, Handler1),
		{ Stmt1 = try_commit(Ref, Statement1, Handler1) },
		Action =^ action,
		( { Action = chain_gc_stack_frames } ->
			save_and_restore_stack_chain(Stmt1, Stmt)
		;
			{ Stmt = Stmt1 }
		)
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
	% add code to save/restore the stack chain pointer:
	% convert
	%	try {
	%	    Statement
	%	} commit {
	%	    Handler
	%	}
	% into
	%	{
	%	    void *saved_stack_chain;
	%	    try {
	%		saved_stack_chain = stack_chain;
	%		Statement
	%	    } commit {
	%		stack_chain = saved_stack_chain;
	%		Handler
	%	    }
	%	}
	%
:- inst try_commit ---> try_commit(ground, ground, ground).

:- pred save_and_restore_stack_chain(mlds__stmt, mlds__stmt,
		elim_info, elim_info).
:- mode save_and_restore_stack_chain(in(try_commit), out, in, out) is det.

save_and_restore_stack_chain(Stmt0, Stmt, ElimInfo0, ElimInfo) :-
	ModuleName = ElimInfo0 ^ module_name,
	counter__allocate(Id, ElimInfo0 ^ saved_stack_chain_counter, NextId),
	ElimInfo = (ElimInfo0 ^ saved_stack_chain_counter := NextId),
	Stmt0 = try_commit(Ref, Statement0, Handler0),
	Statement0 = mlds__statement(_, StatementContext),
	Handler0 = mlds__statement(_, HandlerContext),
	SavedVarDecl = gen_saved_stack_chain_var(Id, StatementContext),
	SaveStatement = gen_save_stack_chain_var(ModuleName, Id,
		StatementContext),
	RestoreStatement = gen_restore_stack_chain_var(ModuleName, Id,
		HandlerContext),
	Statement = mlds__statement(
		block([], [SaveStatement, Statement0]),
		HandlerContext),
	Handler = mlds__statement(
		block([], [RestoreStatement, Handler0]),
		HandlerContext),
	TryCommit = try_commit(Ref, Statement, Handler),
	Stmt = block(
		[SavedVarDecl],
		[mlds__statement(TryCommit, StatementContext)]
	).

%-----------------------------------------------------------------------------%

%
% flatten_nested_defns:
% flatten_nested_defn:
%	Hoist out nested function definitions, and any local variables
%	that need to go in the environment struct (e.g. because they are
%	referenced by nested functions), storing them both in the elim_info.
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
		flatten_function_body(FuncBody0, FuncBody1),

		%
		% for accurate GC,
		% add a call to GC_check() at start of every
		% nested function that might allocate memory
		%
		% (XXX we could perhaps reduce the overhead of
		% this slightly by only doing it for
		% possibly-recursive functions, i.e.
		% by not doing it for leaf functions).
		%
		% XXX This won't work properly with --nondet-copy-out:
		% we'd need to check out to come after
		% we copy the arguments to the GC frame struct.
		% In fact, do we even copy the arguments of
		% nested functions to the GC frame struct?
		% 
		{
			FuncBody1 = defined_here(FuncBody2),
			Action = chain_gc_stack_frames,
			some [NewObjectStmt] (
				statement_contains_statement(FuncBody2,
					NewObjectStmt),
				NewObjectStmt = mlds__statement(atomic(
				    new_object(_, _, _, _, _, _, _, _)
				    ), _)
			)
		->
			GC_Check = mlds__statement(atomic(gc_check), Context),
			FuncBody = defined_here(mlds__statement(
				block([], [GC_Check, FuncBody2]), Context))
		;
			FuncBody = FuncBody1
		},

		%
		% mark the function as private / one_copy,
		% rather than as local / per_instance,
		% if we're about to hoist it out to the top level
		%
		=(ElimInfo),
		{ Action = ElimInfo ^ action },
		( { Action = hoist_nested_funcs } ->
			{ Flags1 = set_access(Flags0, private) },
			{ Flags = set_per_instance(Flags1, one_copy) }
		;
			{ Flags = Flags0 }
		),
		{ DefnBody = mlds__function(PredProcId, Params,
			FuncBody, Attributes) },
		{ Defn = mlds__defn(Name, Context, Flags, DefnBody) },
		( { Action = hoist_nested_funcs } ->
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
			{ Defns = [Defn] }
		)
	;
		{ DefnBody0 = mlds__data(Type, Init, MaybeGCTraceCode0) },
		%
		% for local variable definitions, if they are
		% referenced by any nested functions, then
		% strip them out and store them in the elim_info
		%
		=(ElimInfo),
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
				{ ml_should_add_local_data(ElimInfo,
					VarName, MaybeGCTraceCode0,
					FollowingDefns, FollowingStatements) }
			)
		->
			elim_info_add_and_flatten_local_data(Defn0),
			{ Defns = [] }
		;
			flatten_maybe_statement(MaybeGCTraceCode0,
				MaybeGCTraceCode),
			{ DefnBody = mlds__data(Type, Init, MaybeGCTraceCode) },
			{ Defn = mlds__defn(Name, Context, Flags0, DefnBody) },

			{ Defns = [Defn] }
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
	% to the local_data field of the elim_info, meaning that
	% it should be added to the environment struct
	% (if it's a variable) or hoisted out to the top level
	% (if it's a static const).
:- pred ml_should_add_local_data(elim_info, mlds__var_name,
		mlds__maybe_gc_trace_code, mlds__defns, mlds__statements).
:- mode ml_should_add_local_data(in, in, in, in, in) is semidet.

ml_should_add_local_data(ElimInfo, VarName, MaybeGCTraceCode,
		FollowingDefns, FollowingStatements) :-
	Action = ElimInfo ^ action,
	(
		Action = chain_gc_stack_frames,
		MaybeGCTraceCode = yes(_)
	;
		Action = hoist_nested_funcs,
		ml_need_to_hoist(ElimInfo ^ module_name, VarName,
			FollowingDefns, FollowingStatements)
	).

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
	% XXX Do we need to check for references from the GC_TraceCode
	% fields here?
	%
:- pred ml_need_to_hoist(mlds_module_name, mlds__var_name,
		mlds__defns, mlds__statements).
:- mode ml_need_to_hoist(in, in, in, in) is semidet.

ml_need_to_hoist(ModuleName, VarName,
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
			mlds__data(_, Initializer, _)),
		ml_decl_is_static_const(FollowingDefn),
		initializer_contains_var(Initializer, QualVarName)
	).

	%
	% Add the variable definition to the
	% local_data field of the elim_info,
	% fix up any references inside the GC tracing code,
	% and then update the GC tracing code in the
	% local_data.
	%
	% Note that we need to add the variable definition
	% to the local_data *before* we fix up the GC tracing
	% code, otherwise references to the variable itself
	% in the GC tracing code won't get fixed.
	%
:- pred elim_info_add_and_flatten_local_data(mlds__defn::in,
		elim_info::in, elim_info::out) is det.

elim_info_add_and_flatten_local_data(Defn0) -->
	(
		{ Defn0 = mlds__defn(Name, Context, Flags, DefnBody0) },
		{ DefnBody0 = mlds__data(Type, Init, MaybeGCTraceCode0) }
	->
		elim_info_add_local_data(Defn0),
		flatten_maybe_statement(MaybeGCTraceCode0, MaybeGCTraceCode),
		{ DefnBody = mlds__data(Type, Init, MaybeGCTraceCode) },
		{ Defn = mlds__defn(Name, Context, Flags, DefnBody) },
		elim_info_remove_local_data(Defn0),
		elim_info_add_local_data(Defn)
	;
		elim_info_add_local_data(Defn0)
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
fixup_atomic_stmt(gc_check, gc_check) --> [].
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
	Action = ElimInfo ^ action,
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
				data(VarType, _, _)),
			\+ ml_decl_is_static_const(Var)
			),
		solutions(IsLocalVar, [FieldType])
	->
		EnvPtr = lval(var(qual(ModuleName,
			mlds__var_name(env_name_base(Action) ++ "_ptr", no)),
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
		Action = hoist_nested_funcs,
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

% defn_body_contains_defn(mlds__data(_Type, _Initializer, _), _Defn) :- fail.
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

	% XXX Should we include variables in the GC_TraceCode field here?
defn_body_contains_var(mlds__data(_Type, Initializer, _GC_TraceCode), Name) :-
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
% atomic_stmt_contains_var(gc_check, _) :- fail.
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

% Add code to unlink the stack chain before any explicit returns or tail calls.

:- pred add_unchain_stack_to_maybe_statement(maybe(mlds__statement),
		maybe(mlds__statement), elim_info, elim_info).
:- mode add_unchain_stack_to_maybe_statement(in, out, in, out) is det.

add_unchain_stack_to_maybe_statement(no, no) --> [].
add_unchain_stack_to_maybe_statement(yes(Statement0), yes(Statement)) -->
	add_unchain_stack_to_statement(Statement0, Statement).
	
:- pred add_unchain_stack_to_statements(mlds__statements, mlds__statements,
		elim_info, elim_info).
:- mode add_unchain_stack_to_statements(in, out, in, out) is det.

add_unchain_stack_to_statements(Statements0, Statements) -->
	list__map_foldl(add_unchain_stack_to_statement,
		Statements0, Statements).

:- pred add_unchain_stack_to_statement(mlds__statement, mlds__statement,
		elim_info, elim_info).
:- mode add_unchain_stack_to_statement(in, out, in, out) is det.

add_unchain_stack_to_statement(Statement0, Statement) -->
	{ Statement0 = mlds__statement(Stmt0, Context) },
	add_unchain_stack_to_stmt(Stmt0, Context, Stmt),
	{ Statement = mlds__statement(Stmt, Context) }.

:- pred add_unchain_stack_to_stmt(mlds__stmt, mlds__context, mlds__stmt,
		elim_info, elim_info).
:- mode add_unchain_stack_to_stmt(in, in, out, in, out) is det.

add_unchain_stack_to_stmt(Stmt0, Context, Stmt) -->
	(
		{ Stmt0 = block(Defns, Statements0) },
		add_unchain_stack_to_statements(Statements0, Statements),
		{ Stmt = block(Defns, Statements) }
	;
		{ Stmt0 = while(Rval, Statement0, Once) },
		add_unchain_stack_to_statement(Statement0, Statement),
		{ Stmt = while(Rval, Statement, Once) }
	;
		{ Stmt0 = if_then_else(Cond, Then0, MaybeElse0) },
		add_unchain_stack_to_statement(Then0, Then),
		add_unchain_stack_to_maybe_statement(MaybeElse0, MaybeElse),
		{ Stmt = if_then_else(Cond, Then, MaybeElse) }
	;
		{ Stmt0 = switch(Type, Val, Range, Cases0, Default0) },
		list__map_foldl(add_unchain_stack_to_case, Cases0, Cases),
		add_unchain_stack_to_default(Default0, Default),
		{ Stmt = switch(Type, Val, Range, Cases, Default) }
	;
		{ Stmt0 = label(_) },
		{ Stmt = Stmt0 }
	;
		{ Stmt0 = goto(_) },
		{ Stmt = Stmt0 }
	;
		{ Stmt0 = computed_goto(_Rval, _Labels) },
		{ Stmt = Stmt0 }
	;
		{ Stmt0 = call(_Sig, _Func, _Obj, _Args, _RetLvals, TailCall) },
		( { TailCall = tail_call } ->
			=(ElimInfo),
			{ Stmt = prepend_unchain_frame(Stmt0, Context,
				ElimInfo) }
		;
			{ Stmt = Stmt0 }
		)
	;
		{ Stmt0 = return(_Rvals) },
		=(ElimInfo),
		{ Stmt = prepend_unchain_frame(Stmt0, Context, ElimInfo) }
	;
		{ Stmt0 = do_commit(_Ref) },
		{ Stmt = Stmt0 }
	;
		{ Stmt0 = try_commit(Ref, Statement0, Handler0) },
		add_unchain_stack_to_statement(Statement0, Statement),
		add_unchain_stack_to_statement(Handler0, Handler),
		{ Stmt = try_commit(Ref, Statement, Handler) }
	;
		{ Stmt0 = atomic(_AtomicStmt0) },
		{ Stmt = Stmt0 }
	).

:- pred add_unchain_stack_to_case(mlds__switch_case, mlds__switch_case,
		elim_info, elim_info).
:- mode add_unchain_stack_to_case(in, out, in, out) is det.

add_unchain_stack_to_case(Conds0 - Statement0, Conds - Statement) -->
	list__map_foldl(fixup_case_cond, Conds0, Conds),
	add_unchain_stack_to_statement(Statement0, Statement).

:- pred add_unchain_stack_to_default(mlds__switch_default, mlds__switch_default,
		elim_info, elim_info).
:- mode add_unchain_stack_to_default(in, out, in, out) is det.

add_unchain_stack_to_default(default_is_unreachable, default_is_unreachable)
		--> [].
add_unchain_stack_to_default(default_do_nothing, default_do_nothing) --> [].
add_unchain_stack_to_default(default_case(Statement0), default_case(Statement))
		-->
	add_unchain_stack_to_statement(Statement0, Statement).
	
:- func prepend_unchain_frame(mlds__stmt, mlds__context, elim_info) =
	mlds__stmt.
prepend_unchain_frame(Stmt0, Context, ElimInfo) = Stmt :-
	UnchainFrame = ml_gen_unchain_frame(Context, ElimInfo),
	Statement0 = mlds__statement(Stmt0, Context),
	Stmt = block([], [UnchainFrame, Statement0]).

:- func append_unchain_frame(mlds__stmt, mlds__context, elim_info) =
	mlds__stmt.
append_unchain_frame(Stmt0, Context, ElimInfo) = Stmt :-
	UnchainFrame = ml_gen_unchain_frame(Context, ElimInfo),
	Statement0 = mlds__statement(Stmt0, Context),
	Stmt = block([], [Statement0, UnchainFrame]).

:- func ml_gen_unchain_frame(mlds__context, elim_info) = mlds__statement.
ml_gen_unchain_frame(Context, ElimInfo) = UnchainFrame :-
	EnvPtrTypeName = ElimInfo ^ env_ptr_type_name,
	%
	% Generate code to remove this frame from the stack chain:
	%
	%	stack_chain = stack_chain->prev;
	%
	% Actually, it's not quite as simple as that.  The global
	% `stack_chain' has type `void *', rather than `MR_StackChain *', and
	% the MLDS has no way of representing the `struct MR_StackChain' type
	% (which we'd need to cast it to) or of accessing an unqualified
	% field name like `prev' (rather than `modulename__prev').
	%
	% So we do this in a slightly lower-level fashion, using
	% a field offset rather than a field name:
	%
	%	stack_chain = MR_hl_field(stack_chain, 0);
	%
	StackChain = ml_stack_chain_var,
	Tag = yes(0),
	PrevFieldId = offset(const(int_const(0))),
	PrevFieldType = mlds__generic_type,
	PrevFieldRval = lval(field(Tag, lval(StackChain), PrevFieldId,
		PrevFieldType, EnvPtrTypeName)),
	Assignment = assign(StackChain, PrevFieldRval),
	UnchainFrame = mlds__statement(atomic(Assignment), Context).

	% Generate a local variable declaration
	% to hold the saved stack chain pointer:
	%	void *saved_stack_chain;
:- func gen_saved_stack_chain_var(int, mlds__context) = mlds__defn.
gen_saved_stack_chain_var(Id, Context) = Defn :-
	Name = data(var(ml_saved_stack_chain_name(Id))),
	Flags = ml_gen_local_var_decl_flags,
	Type = ml_stack_chain_type,
	Initializer = no_initializer,
	% The saved stack chain never needs to be traced by the GC,
	% since it will always point to the stack, not into the heap.
	GCTraceCode = no,
	DefnBody = mlds__data(Type, Initializer, GCTraceCode),
	Defn = mlds__defn(Name, Context, Flags, DefnBody).

	% Generate a statement to save the stack chain pointer:
	%	saved_stack_chain = stack_chain;
:- func gen_save_stack_chain_var(mlds_module_name, int, mlds__context) =
	mlds__statement.
gen_save_stack_chain_var(MLDS_Module, Id, Context) = SaveStatement :-
	SavedStackChain = var(qual(MLDS_Module,
		ml_saved_stack_chain_name(Id)), ml_stack_chain_type),
	Assignment = assign(SavedStackChain, lval(ml_stack_chain_var)),
	SaveStatement = mlds__statement(atomic(Assignment), Context).

	% Generate a statement to restore the stack chain pointer:
	%	stack_chain = saved_stack_chain;
:- func gen_restore_stack_chain_var(mlds_module_name, int, mlds__context) =
	mlds__statement.
gen_restore_stack_chain_var(MLDS_Module, Id, Context) = RestoreStatement :-
	SavedStackChain = var(qual(MLDS_Module,
		ml_saved_stack_chain_name(Id)), ml_stack_chain_type),
	Assignment = assign(ml_stack_chain_var, lval(SavedStackChain)),
	RestoreStatement = mlds__statement(atomic(Assignment), Context).

:- func ml_saved_stack_chain_name(int) = mlds__var_name.
ml_saved_stack_chain_name(Id) = var_name("saved_stack_chain", yes(Id)).

%-----------------------------------------------------------------------------%

%
% The elim_info type holds information that we use or accumulate
% as we traverse through the function body.
%

:- type elim_info
	--->	elim_info(
				% Specify whether we're eliminating nested
				% functions, or doing the transformation
				% needed for accurate GC.
			action :: action,

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
			env_ptr_type_name :: mlds__type,

				% A counter used to number the local variables
				% used to save the stack chain
			saved_stack_chain_counter :: counter
	).

	% The lists of local variables for
	% each of the containing functions,
	% innermost first
:- type outervars == list(list(mlds__defn)).

:- func elim_info_init(action, mlds_module_name, outervars,
		mlds__type, mlds__type) = elim_info.
elim_info_init(Action, ModuleName, OuterVars, EnvTypeName, EnvPtrTypeName) =
	elim_info(Action, ModuleName, OuterVars, [], [],
		EnvTypeName, EnvPtrTypeName, counter__init(0)).

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

:- pred elim_info_remove_local_data(mlds__defn, elim_info, elim_info).
:- mode elim_info_remove_local_data(in, in, out) is det.
elim_info_remove_local_data(LocalVar, ElimInfo0, ElimInfo) :-
	( list__delete_first(ElimInfo0 ^ local_data, LocalVar, LocalData) ->
		ElimInfo = ElimInfo0 ^ local_data := LocalData
	;
		error("elim_info_remove_local_data: not found")
	).

:- pred elim_info_finish(elim_info, list(mlds__defn), list(mlds__defn)).
:- mode elim_info_finish(in, out, out) is det.
elim_info_finish(ElimInfo, Funcs, Locals) :-
	Funcs = list__reverse(ElimInfo ^ nested_funcs),
	Locals = list__reverse(ElimInfo ^ local_data).

%-----------------------------------------------------------------------------%
