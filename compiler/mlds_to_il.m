%-----------------------------------------------------------------------------%
% Copyright (C) 2000-2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% mlds_to_il - Convert MLDS to IL.
% Main author: trd, petdr
%
% This module generates IL from MLDS.  Currently it's pretty tuned
% towards generating assembler -- to generate code using
% Reflection::Emit it is likely some changes will need to be made.
%
% Currently non-det environments are represented using a high-level data
% representation (classes with typed fields), while all other data structures 
% are represented using a low-level data representation (arrays of
% System.Object).  This is for historical reasons -- the MLDS high-level-data
% support wasn't available when it was needed.  Eventually we should
% move to a completely high-level data representation as the current
% representation is pretty inefficient.
%
% The IL backend TO-DO list:
%
% [ ] advanced name mangling: 
%	- optionally only mangle names when it is absolutely necessary
%	(Partly done; we now mangle names less often than we used to.
%	The only way to mangle less would be to use a context-sensitive
%	name mangling algorithm, which may not be a good idea.)
% [ ] Type classes
%	- now work, but...
%	- type class hierarchies don't work due to unimplemented pragma
%	  foreign code.
%	- should be implemented as interfaces
% [ ] RTTI (io__write -- about half the work required for this is done)
% [ ] High-level RTTI data
% [ ] Test unused mode (we seem to create a byref for it)
% [ ] Char (test unicode support)
% [ ] auto dependency generation for IL and assembler
% [ ] build environment improvements (support
% 	libraries/packages/namespaces better)
% [ ] verifiable code
% 	[ ] verifiable function pointers
% [ ] omit empty cctors
% [ ] Convert to "high-level data"
% [ ] Computed gotos need testing.
% [ ] :- extern doesn't work -- it needs to be treated like pragma c code.
% [ ] nested modules need testing
% [ ] Fix issues with abstract types so that we can implement C
%     pointers as MR_Box rather than MR_Word.
% [ ] When generating target_code, sometimes we output more calls than
%     we should (this can occur in nondet C code). 
% [ ] ml_gen_call_current_success_cont_indirectly should be merged with
% 	similar code for doing copy-in/copy-out.
% [ ] Add an option to do overflow checking.
% [ ] Should replace hard-coded of int32 with a more abstract name such
%     as `mercury_int_il_type'.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module ml_backend__mlds_to_il.
:- interface.

:- import_module ml_backend__mlds, ml_backend__ilasm, ml_backend__ilds.
:- import_module io, list, bool, std_util, set.
:- import_module hlds__hlds_pred. % for `pred_proc_id'.
:- import_module libs__globals. % for `foreign_language'.

%-----------------------------------------------------------------------------%

	%
	% Generate IL assembly from MLDS.
	%
	% This is where all the action is for the IL backend.
	%
:- pred generate_il(mlds, list(ilasm__decl), set(foreign_language),
		io__state, io__state).
:- mode generate_il(in, out, out, di, uo) is det.


%-----------------------------------------------------------------------------%

	%
	% The following predicates are exported so that we can get type
	% conversions and name mangling consistent between the managed
	% C++ output (currently in mlds_to_ilasm.m) and IL output (in
	% this file).
	%
	% XXX we should reduce the dependencies here to a bare minimum.
	%
:- func params_to_il_signature(il_data_rep, mlds_module_name,
	mlds__func_params) = signature.

	% Generate an IL identifier for a pred label.
:- pred predlabel_to_id(mlds__pred_label, proc_id,
	maybe(mlds__func_sequence_num), ilds__id).
:- mode predlabel_to_id(in, in, in, out) is det.

	% Generate an IL identifier for a MLDS var.
:- pred mangle_mlds_var(mlds__var, ilds__id).
:- mode mangle_mlds_var(in, out) is det.

	% This type stores information affecting our IL data representation.
:- type il_data_rep ---> il_data_rep(
		highlevel_data	:: bool,	% do we use high-level data?
		il_envptr_type :: ilds__type	% what IL type do we use for
						% mlds__generic_env_ptr_type?
	).
:- pred get_il_data_rep(il_data_rep::out, io__state::di, io__state::uo) is det.

	% Get the corresponding ILDS type for an MLDS type 
	% (this depends on which representation you happen to be using).
:- func mlds_type_to_ilds_type(il_data_rep, mlds__type) = ilds__type.

	% Get the corresponding ILDS class name for an MLDS type
	% (this depends on which representation you happen to be using).

:- func mlds_type_to_ilds_class_name(il_data_rep, mlds__type) =
	ilds__class_name.

	% Turn a proc name into an IL class_name and a method name.
:- pred mangle_mlds_proc_label(mlds__qualified_proc_label, 
	maybe(mlds__func_sequence_num), ilds__class_name, ilds__id).
:- mode mangle_mlds_proc_label(in, in, out, out) is det.

	% class_name(Module, Name) returns a class name representing
	% Name in the module Module.
:- func class_name(mlds_module_name, string) = ilds__class_name.

	% Return the class_name for the generic class.
:- func il_generic_class_name = ilds__class_name.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module libs__globals, libs__options, hlds__passes_aux.
:- import_module backend_libs__builtin_ops, backend_libs__c_util.
:- import_module parse_tree__modules, libs__tree.
:- import_module parse_tree__prog_data, parse_tree__prog_out.
:- import_module parse_tree__prog_util, ll_backend__llds_out.
:- import_module backend_libs__pseudo_type_info, backend_libs__rtti.
:- import_module check_hlds__type_util, backend_libs__code_model.
:- import_module backend_libs__foreign.

:- import_module ml_backend__il_peephole.
:- import_module ml_backend__ml_util, ml_backend__ml_code_util.
:- import_module hlds__error_util.
:- import_module ml_backend__ml_type_gen.
:- import_module backend_libs__foreign.
:- use_module ll_backend__llds. /* for user_foreign_code */

:- import_module bool, int, map, string, set, list, assoc_list, term.
:- import_module library, require, counter.

	% We build up lists of instructions using a tree to make
	% insertion easy.
:- type instr_tree == tree(list(instr)).

	% The state of the il code generator.
:- type il_info ---> il_info(
		% file-wide attributes (all static)
	module_name 	:: mlds_module_name,	% the module name
	assembly_name 	:: ilds__id,		% the assembly name
	imports 	:: mlds__imports,	% the imports
	file_foreign_langs :: set(foreign_language), % file foreign code
	il_data_rep	:: il_data_rep,		% data representation.
	debug_il_asm	:: bool,		% --debug-il-asm
	verifiable_code	:: bool,		% --verifiable-code
	il_byref_tailcalls :: bool,		% --il-byref-tailcalls
	support_ms_clr	:: bool,		% --support-ms-clr
		% class-wide attributes (all accumulate)
	alloc_instrs	:: instr_tree,		% .cctor allocation instructions
	init_instrs	:: instr_tree,		% .cctor init instructions
	class_members	:: list(class_member),	% class methods and fields 
	has_main	:: bool,		% class contains main
	class_foreign_langs :: set(foreign_language),% class foreign code
	field_names	:: field_names_set,	% field names
		% method-wide attributes (accumulating)
	locals 		:: locals_map,		% The current locals
	instr_tree 	:: instr_tree,		% The instruction tree (unused)
	label_counter 	:: counter,		% the label counter
	block_counter 	:: counter,		% the block counter
	method_foreign_lang :: maybe(foreign_language),
						% method contains foreign code
		% method-wide attributes (static)
	arguments 	:: arguments_map, 	% The arguments 
	method_name	:: member_name,		% current method name
	signature	:: signature		% current return type 
	).

:- type locals_map == map(ilds__id, mlds__type).
:- type arguments_map == assoc_list(ilds__id, mlds__type). 
:- type mlds_vartypes == map(ilds__id, mlds__type).
:- type field_names_set == set(string).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

generate_il(MLDS, ILAsm, ForeignLangs) -->
	maybe_get_dotnet_library_version(MaybeVersion),
	( { MaybeVersion = yes(Version) },
		generate_il(MLDS, Version, ILAsm, ForeignLangs)
	; { MaybeVersion = no },
		{ ILAsm = [] },
		{ ForeignLangs = set__init }
	).

:- pred maybe_get_dotnet_library_version(maybe(assembly_decl)::out,
		io::di, io::uo) is det.

maybe_get_dotnet_library_version(MaybeVersion) -->
	io_lookup_string_option(dotnet_library_version, VersionStr),
	{ IsSep = (pred(('.')::in) is semidet) },
	( 
		{ string__words(IsSep, VersionStr) = [Mj, Mn, Bu, Rv] },
		{ string__to_int(Mj, Major) },
		{ string__to_int(Mn, Minor) },
		{ string__to_int(Bu, Build) },
		{ string__to_int(Rv, Revision) }
	->
		{ Version = version(Major, Minor, Build, Revision) },
		{ MaybeVersion = yes(Version) }
	;
		{ MaybeVersion = no },
		write_error_pieces_maybe_with_context(no, 0, [
				words("Error: invalid version string"),
				words("`" ++ VersionStr ++ "'"),
				words("passed to `--dotnet-library-version'.")
				]),
		io__set_exit_status(1)
	).

%-----------------------------------------------------------------------------%

:- pred generate_il(mlds, assembly_decl,
		list(ilasm__decl), set(foreign_language),
		io__state, io__state).
:- mode generate_il(in, in, out, out, di, uo) is det.

generate_il(MLDS, Version, ILAsm, ForeignLangs, IO0, IO) :-

	mlds(MercuryModuleName, _ForeignCode, Imports, Defns) =
		transform_mlds(MLDS),

	ModuleName = mercury_module_name_to_mlds(MercuryModuleName),
	prog_out__sym_name_to_string(mlds_module_name_to_sym_name(ModuleName),
			".", AssemblyName),
	get_il_data_rep(ILDataRep, IO0, IO1),
	globals__io_lookup_bool_option(debug_il_asm, DebugIlAsm, IO1, IO2),
	globals__io_lookup_bool_option(verifiable_code,
			VerifiableCode, IO2, IO3),
	globals__io_lookup_bool_option(il_byref_tailcalls, ByRefTailCalls,
			IO3, IO4),
	globals__io_lookup_bool_option(sign_assembly, SignAssembly,
			IO4, IO5),
	globals__io_lookup_bool_option(separate_assemblies, SeparateAssemblies,
			IO5, IO6),
	globals__io_lookup_bool_option(support_ms_clr, MsCLR,
			IO6, IO),

	IlInfo0 = il_info_init(ModuleName, AssemblyName, Imports,
			ILDataRep, DebugIlAsm, VerifiableCode, ByRefTailCalls,
			MsCLR),

		% Generate code for all the methods.
	list__map_foldl(mlds_defn_to_ilasm_decl, Defns, ILDecls,
			IlInfo0, IlInfo),

	ForeignLangs = IlInfo ^ file_foreign_langs,

	ClassName = mlds_module_name_to_class_name(ModuleName),
	ClassName = structured_name(_, NamespaceName, _),

		% Make this module an assembly unless it is in the standard
		% library.  Standard library modules all go in the one
		% assembly in a separate step during the build (using
		% AL.EXE).  
	PackageName = mlds_module_name_to_package_name(ModuleName),
	(
		PackageName = qualified(unqualified("mercury"), _)
	->
		ThisAssembly = [],
		AssemblerRefs = Imports
	;
			% If the package name is qualified then the
			% we have a sub-module which shouldn't be placed
			% in its own assembly provided we have
			% --no-separate-assemblies
		(
			PackageName = qualified(_, _),
			SeparateAssemblies = no
		->
			ThisAssembly = []
		;
			ThisAssembly = [assembly(AssemblyName)]
		),

			% XXX at a later date we should make foreign
			% code behave like a submodule.
			%
			% If not in the library, but we have foreign code,
			% declare the foreign module as an assembly we
			% reference
		list__map((pred(F::in, I::out) is det :-
				mangle_foreign_code_module(ModuleName, F, N),
				I = mercury_import(N)
			),
			set__to_sorted_list(ForeignLangs),
			ForeignCodeAssemblerRefs),
		AssemblerRefs = list__append(ForeignCodeAssemblerRefs, Imports)
	),
	generate_extern_assembly(AssemblyName, Version, SignAssembly,
			SeparateAssemblies, AssemblerRefs, ExternAssemblies),
	Namespace = [namespace(NamespaceName, ILDecls)],
	ILAsm = list__condense([ThisAssembly, ExternAssemblies, Namespace]).

get_il_data_rep(ILDataRep, IO0, IO) :-
	globals__io_get_globals(Globals, IO0, IO),
	globals__lookup_bool_option(Globals, highlevel_data, HighLevelData),
	ILEnvPtrType = choose_il_envptr_type(Globals),
	ILDataRep = il_data_rep(HighLevelData, ILEnvPtrType).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Move all the top level methods and data definitions into the
	% wrapper class, and then fix all the references so that
	% they refer to their new names.
:- func transform_mlds(mlds) = mlds.

transform_mlds(MLDS0) = MLDS :-
	AllExports = list__condense(
		list__map(
			(func(mlds__foreign_code(_, _, _, Exports)) = Exports),
			map__values(MLDS0 ^ foreign_code))
		),

		% Generate the exports for this file, they will be placed
		% into class methods inside the wrapper class.
	list__map(mlds_export_to_mlds_defn, AllExports, ExportDefns),

	list__filter((pred(D::in) is semidet :-
			( D = mlds__defn(_, _, _, mlds__function(_, _, _, _))
			; D = mlds__defn(_, _, _, mlds__data(_, _, _))
			)
		), MLDS0 ^ defns ++ ExportDefns, MercuryCodeMembers, Others),
	WrapperClass = wrapper_class(list__map(rename_defn, MercuryCodeMembers)),
		% Note that ILASM requires that the type definitions in Others
		% must precede the references to those types in WrapperClass.
	MLDS = MLDS0 ^ defns := list__map(rename_defn, Others) ++ [WrapperClass].


:- func wrapper_class(mlds__defns) = mlds__defn.

wrapper_class(Members)
	= mlds__defn(
		export(wrapper_class_name),
		mlds__make_context(term__context_init),
		ml_gen_type_decl_flags,
		mlds__class(
			mlds__class_defn(mlds__package, [], [], [], [], Members)
		)
	).

:- func rename_defn(mlds__defn) = mlds__defn.

rename_defn(defn(Name, Context, Flags, Entity0))
	= defn(Name, Context, Flags, Entity) :-
	( Entity0 = data(Type, Initializer, GC_TraceCode),
		Entity = data(Type, rename_initializer(Initializer),
			rename_maybe_statement(GC_TraceCode))
	; Entity0 = function(MaybePredProcId, Params, FunctionBody0,
			Attributes),
		( FunctionBody0 = defined_here(Stmt),
			FunctionBody = defined_here(rename_statement(Stmt))
		; FunctionBody0 = external,
			FunctionBody = external
		),
		Entity = function(MaybePredProcId, Params, FunctionBody,
			Attributes)
	; Entity0 = class(ClassDefn),
		ClassDefn = class_defn(Kind, Imports, Inherits, Implements,
				Ctors, Members),
		Entity = class(class_defn(Kind, Imports, Inherits, Implements,
				list__map(rename_defn, Ctors),
				list__map(rename_defn, Members)))
	).

:- func rename_maybe_statement(maybe(mlds__statement)) = maybe(mlds__statement).

rename_maybe_statement(no) = no.
rename_maybe_statement(yes(Stmt)) = yes(rename_statement(Stmt)).

:- func rename_statement(mlds__statement) = mlds__statement.

rename_statement(statement(block(Defns, Stmts), Context))
	= statement(block(list__map(rename_defn, Defns),
			list__map(rename_statement, Stmts)),
			Context).
rename_statement(statement(while(Rval, Loop, IterateOnce), Context))
	= statement(while(rename_rval(Rval),
			rename_statement(Loop), IterateOnce), Context).
rename_statement(statement(if_then_else(Rval, Then, MaybeElse), Context))
	= statement(if_then_else(rename_rval(Rval),
			rename_statement(Then),
			rename_maybe_statement(MaybeElse)), Context).
rename_statement(statement(switch(Type, Rval, Range, Cases, Default0), Context))
	= statement(switch(Type, rename_rval(Rval), Range,
			list__map(rename_switch_case, Cases), Default),
			Context) :-
	( Default0 = default_is_unreachable,
		Default = default_is_unreachable
	; Default0 = default_do_nothing,
		Default = default_do_nothing
	; Default0 = default_case(Stmt),
		Default = default_case(rename_statement(Stmt))
	).
rename_statement(statement(label(Label), Context))
	= statement(label(Label), Context).
rename_statement(statement(goto(Label), Context))
	= statement(goto(Label), Context).
rename_statement(statement(computed_goto(Rval, Labels), Context))
	= statement(computed_goto(rename_rval(Rval), Labels), Context).

rename_statement(statement(
		call(Signature, Rval, MaybeThis0, Args, Results, TailCall),
		Context))
	= statement(call(Signature, rename_rval(Rval),
			MaybeThis, list__map(rename_rval, Args),
			list__map(rename_lval, Results), TailCall), Context) :-
	( MaybeThis0 = yes(Self),
		MaybeThis = yes(rename_rval(Self))
	; MaybeThis0 = no,
		MaybeThis = no
	).

rename_statement(statement(return(Vals), Context))
	= statement(return(Vals), Context).
rename_statement(statement(try_commit(Lval, Try, Handler), Context))
	= statement(try_commit(rename_lval(Lval), rename_statement(Try),
			rename_statement(Handler)), Context).
rename_statement(statement(do_commit(Rval), Context))
	= statement(do_commit(rename_rval(Rval)), Context).
rename_statement(statement(atomic(Stmt), Context))
	= statement(atomic(rename_atomic(Stmt)), Context).

:- func rename_switch_case(switch_case) = switch_case.

rename_switch_case(Conds - Stmt)
	= list__map(rename_cond, Conds) - rename_statement(Stmt).

:- func rename_cond(case_match_cond) = case_match_cond.

rename_cond(match_value(Rval)) = match_value(rename_rval(Rval)).
rename_cond(match_range(RvalA, RvalB))
	= match_range(rename_rval(RvalA), rename_rval(RvalB)).

:- func rename_atomic(atomic_statement) = atomic_statement.

rename_atomic(comment(S)) = comment(S).
rename_atomic(assign(L, R)) = assign(rename_lval(L), rename_rval(R)).
rename_atomic(delete_object(O)) = delete_object(rename_lval(O)).
rename_atomic(new_object(L, Tag, HasSecTag, Type, MaybeSize, Ctxt, Args, Types))
	= new_object(rename_lval(L), Tag, HasSecTag, Type, MaybeSize,
			Ctxt, list__map(rename_rval, Args), Types).
rename_atomic(gc_check) = gc_check.
rename_atomic(mark_hp(L)) = mark_hp(rename_lval(L)).
rename_atomic(restore_hp(R)) = restore_hp(rename_rval(R)).
rename_atomic(trail_op(T)) = trail_op(T).
rename_atomic(inline_target_code(L, Cs)) = inline_target_code(L, Cs).
rename_atomic(outline_foreign_proc(F, Ls, S)) = outline_foreign_proc(F, Ls, S).

:- func rename_rval(mlds__rval) = mlds__rval.

rename_rval(lval(Lval)) = lval(rename_lval(Lval)).
rename_rval(mkword(Tag, Rval)) = mkword(Tag, rename_rval(Rval)).
rename_rval(const(Const)) = const(rename_const(Const)).
rename_rval(unop(Op, Rval)) = unop(Op, rename_rval(Rval)).
rename_rval(binop(Op, RvalA, RvalB))
	= binop(Op, rename_rval(RvalA), rename_rval(RvalB)).
rename_rval(mem_addr(Lval)) = mem_addr(rename_lval(Lval)).
rename_rval(self(Type)) = self(Type).

:- func rename_const(mlds__rval_const) = mlds__rval_const.

rename_const(true) = true.
rename_const(false) = false.
rename_const(int_const(I)) = int_const(I).
rename_const(float_const(F)) = float_const(F).
rename_const(string_const(S)) = string_const(S).
rename_const(multi_string_const(I, S)) = multi_string_const(I, S).
rename_const(code_addr_const(C)) = code_addr_const(rename_code_addr(C)).
rename_const(data_addr_const(A)) = data_addr_const(rename_data_addr(A)).
rename_const(null(T)) = null(T).

:- func rename_code_addr(mlds__code_addr) = mlds__code_addr.

rename_code_addr(proc(Label, Signature))
	= proc(rename_proc_label(Label), Signature).
rename_code_addr(internal(Label, Seq, Signature))
	= internal(rename_proc_label(Label), Seq, Signature).

rename_proc_label(qual(Module, Name))
	= qual(append_wrapper_class(Module), Name).

:- func rename_lval(mlds__lval) = mlds__lval.

rename_lval(field(Tag, Address, FieldName, FieldType, PtrType))
	= field(Tag, rename_rval(Address),
			rename_field_id(FieldName), FieldType, PtrType).
rename_lval(mem_ref(Rval, Type)) = mem_ref(rename_rval(Rval), Type).
rename_lval(var(Var, Type)) = var(rename_var(Var, Type), Type).

:- func rename_field_id(field_id) = field_id.

rename_field_id(offset(Rval)) = offset(rename_rval(Rval)).
rename_field_id(named_field(Name, Type)) = named_field(Name, Type).

:- func rename_initializer(mlds__initializer) = mlds__initializer.

rename_initializer(init_obj(Rval)) = init_obj(rename_rval(Rval)).
rename_initializer(init_struct(Inits))
	= init_struct(list__map(rename_initializer, Inits)).
rename_initializer(init_array(Inits))
	= init_array(list__map(rename_initializer, Inits)).
rename_initializer(no_initializer) = no_initializer.

	% We need to append a wrapper class qualifier so that we access
	% the RTTI fields correctly.
:- func rename_data_addr(data_addr) = data_addr.

rename_data_addr(data_addr(ModuleName, Name))
	= data_addr(append_wrapper_class(ModuleName), Name).

	% We need to append a wrapper class qualifier so that we refer to the
	% methods of the wrapper class.
:- func rename_proc_label(mlds__qualified_proc_label) =
		mlds__qualified_proc_label.

	% Again append a wrapper class qualifier to the var name.
:- func rename_var(mlds__var, mlds__type) = mlds__var.

rename_var(qual(ModuleName, Name), _Type)
	= qual(append_wrapper_class(ModuleName), Name).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred mlds_defn_to_ilasm_decl(mlds__defn::in, ilasm__decl::out,
		il_info::in, il_info::out) is det.

	% IL supports top-level (i.e. "global") function definitions and
	% data definitions, but they're not part of the CLS.
	% Since they are not part of the CLS, we don't generate them,
	% and so there's no need to handle them here.
mlds_defn_to_ilasm_decl(defn(_Name, _Context, _Flags, data(_Type, _Init, _GC)),
		_Decl, Info, Info) :-
	sorry(this_file, "top level data definition!").
mlds_defn_to_ilasm_decl(defn(_Name, _Context, _Flags,
		function(_MaybePredProcId, _Params, _MaybeStmts, _Attrs)),
		_Decl, Info, Info) :-
	sorry(this_file, "top level function definition!").
mlds_defn_to_ilasm_decl(defn(Name, Context, Flags0, class(ClassDefn)),
		Decl, Info0, Info) :-
	il_info_new_class(ClassDefn, Info0, Info1),

	generate_class_body(Name, Context, ClassDefn, ClassName, EntityName,
		Extends, Interfaces, MethodsAndFieldsAndCtors, Info1, Info2),

		% Only the wrapper class needs to have the
		% initialization instructions executed by the class
		% constructor.
	( EntityName = wrapper_class_name ->
		Imports = Info2 ^ imports,
		InitInstrs = list__condense(tree__flatten(Info2 ^ init_instrs)),
		AllocInstrs = list__condense(
				tree__flatten(Info2 ^ alloc_instrs)),

			% Generate a field that records whether we have
			% finished RTTI initialization.
		generate_rtti_initialization_field(ClassName, 
				AllocDoneFieldRef, AllocDoneField),

			% Generate a class constructor.
		make_class_constructor_class_member(AllocDoneFieldRef,
				Imports, AllocInstrs, InitInstrs, CCtor,
				Info2, Info),

			% The declarations in this class.
		MethodDecls = [AllocDoneField, CCtor | MethodsAndFieldsAndCtors]
	;
		MethodDecls = MethodsAndFieldsAndCtors,
		Info = Info2
	),
		% XXX Needed to work around a bug where private classes
		% aren't accessible from classes in the same assembly
		% when that assembly is created by al.exe.
		% This occurs for nondet environment classes in the
		% mercury std library.
	( ClassName = structured_name(assembly("mercury"), _, _) ->
		Flags = set_access(Flags0, public)
	;
		Flags = Flags0
	),
	Decl = class(decl_flags_to_classattrs(Flags), EntityName, Extends,
			Interfaces, MethodDecls).

:- pred generate_class_body(mlds__entity_name::in, mlds__context::in,
		mlds__class_defn::in,
		ilds__class_name::out, ilds__id::out, extends::out,
		implements::out, list(class_member)::out,
		il_info::in, il_info::out) is det.

generate_class_body(Name, Context, ClassDefn,
		ClassName, EntityName, Extends, Interfaces, ClassMembers,
		Info0, Info) :-
	EntityName = entity_name_to_ilds_id(Name),
	ClassDefn = class_defn(Kind, _Imports, Inherits, Implements,
			Ctors0, Members),
	Parent - Extends = generate_parent_and_extends(Info0 ^ il_data_rep,
			Kind, Inherits),
	Interfaces = implements(
			list__map(interface_id_to_class_name, Implements)),
	ClassName = class_name(Info0 ^ module_name, EntityName),
	list__map_foldl(generate_method(ClassName, no), Members,
			MethodsAndFields, Info0, Info1),
	Ctors = maybe_add_empty_ctor(Ctors0, Kind, Context),
	list__map_foldl(generate_method(ClassName, yes(Parent)), Ctors,
			IlCtors, Info1, Info),
	ClassMembers = IlCtors ++ MethodsAndFields.

	% For IL, every class needs a constructor,
	% otherwise you can't use the newobj instruction to
	% allocate instances of the class.
	% So if a class doesn't already have one, we add an empty one.
:- func maybe_add_empty_ctor(mlds__defns, mlds__class_kind, mlds__context) =
	mlds__defns.
maybe_add_empty_ctor(Ctors0, Kind, Context) = Ctors :-
	(
		Kind = mlds__class,
		Ctors0 = [] 
	->
		% Generate an empty block for the body of the constructor.
		Stmt = mlds__statement(block([], []), Context),

		Attributes = [],
		Ctor = mlds__function(no, func_params([], []),
				defined_here(Stmt), Attributes),
		CtorFlags = init_decl_flags(public, per_instance, non_virtual,
				overridable, modifiable, concrete),

		CtorDefn = mlds__defn(export(".ctor"), Context, CtorFlags,
				Ctor),
		Ctors = [CtorDefn]
	;
		Ctors = Ctors0
	).

:- func generate_parent_and_extends(il_data_rep, mlds__class_kind,
		list(mlds__class_id)) = pair(ilds__class_name, extends).

generate_parent_and_extends(DataRep, Kind, Inherits) = Parent - Extends :-
	( Inherits = [],
		( Kind = mlds__struct ->
			Parent = il_generic_valuetype_name,
			Extends = extends(Parent)
		; Kind = mlds__enum ->
			Parent = il_generic_enum_name,
			Extends = extends(Parent)
		; % Kind = mlds__class, mlds__package, or mlds__interface
			Parent = il_generic_class_name,
			Extends = extends_nothing
		)
	; Inherits = [Parent0 | Rest],
		( Rest = [] ->
			Parent = mlds_type_to_ilds_class_name(DataRep, Parent0),
			Extends = extends(Parent)
		;
			error(this_file ++ 
				": multiple inheritance not supported.")
		)
	).

class_name(Module, Name)
	= append_toplevel_class_name(mlds_module_name_to_class_name(Module),
		Name).

:- func sym_name_to_list(sym_name) = list(string).

sym_name_to_list(unqualified(Name)) = [Name].
sym_name_to_list(qualified(Module, Name))
	= sym_name_to_list(Module) ++ [Name].


:- func decl_flags_to_classattrs(mlds__decl_flags) = list(ilasm__classattr).

decl_flags_to_classattrs(Flags)
	= list__condense([Access, decl_flags_to_classattrs_2(Flags)]) :-
	AccessFlag = access(Flags),
	( AccessFlag = public,
		Access = [public]
	; AccessFlag = protected,
		error("decl_flags_to_classattrs: protected access flag")
	; AccessFlag = private,
		Access = [private]
	; AccessFlag = default,
			% To make members of the private class
			% accessible to other types in the assembly, set
			% their access to be default or public.
		Access = [private]
	; AccessFlag = local,
		error("decl_flags_to_classattrs: local access flag")
	).

:- func decl_flags_to_nestedclassattrs(mlds__decl_flags) =
		list(ilasm__classattr).

decl_flags_to_nestedclassattrs(Flags)
	= list__condense([Access, decl_flags_to_classattrs_2(Flags)]) :-
	AccessFlag = access(Flags),
	( AccessFlag = public,
		Access = [nestedpublic]
	; AccessFlag = protected,
		Access = [nestedfamily]
	; AccessFlag = private,
		Access = [nestedprivate]
	; AccessFlag = default,
		Access = [nestedassembly]
	; AccessFlag = local,
		error("decl_flags_to_classattrs: local access flag")
	).

:- func decl_flags_to_classattrs_2(mlds__decl_flags) = list(ilasm__classattr).

decl_flags_to_classattrs_2(Flags)
	= list__condense([Finality, Abstractness]) :-
	FinalityFlag = finality(Flags),
	( FinalityFlag = overridable,
		Finality = []
	; FinalityFlag = final,
		Finality = [sealed]
	),
	AbstractnessFlag = abstractness(Flags),
	( AbstractnessFlag = concrete,
		Abstractness = []
	; AbstractnessFlag = abstract,
		Abstractness = [abstract]
	).

:- func decl_flags_to_methattrs(mlds__decl_flags) = list(ilasm__methattr).

decl_flags_to_methattrs(Flags)
	= list__condense([Access, PerInstance, Virtuality,
			Finality, Abstractness]) :-
	AccessFlag = access(Flags),
	( AccessFlag = public,
		Access = [public]
	; AccessFlag = protected,
		Access = [family]
	; AccessFlag = private,
		Access = [private]
	; AccessFlag = default,
		Access = [assembly]
	; AccessFlag = local,
		error("decl_flags_to_methattrs: local access flag")
	),
	PerInstanceFlag = per_instance(Flags),
	( PerInstanceFlag = one_copy,
		PerInstance = [static]
	; PerInstanceFlag = per_instance,
		PerInstance = []
	),
	VirtualityFlag = virtuality(Flags),
	( VirtualityFlag = non_virtual,
		Virtuality = []
	; VirtualityFlag = virtual,
		Virtuality = [virtual]
	),
	FinalityFlag = finality(Flags),
	( FinalityFlag = overridable,
		Finality = []
	; FinalityFlag = final,
		Finality = [final]
	),
	AbstractnessFlag = abstractness(Flags),
	( AbstractnessFlag = concrete,
		Abstractness = []
	; AbstractnessFlag = abstract,
		Abstractness = [abstract]
	).


:- func decl_flags_to_fieldattrs(mlds__decl_flags) = list(ilasm__fieldattr).

decl_flags_to_fieldattrs(Flags)
	= list__condense([Access, PerInstance, Constness]) :-
	AccessFlag = access(Flags),
	( AccessFlag = public,
		Access = [public]
	; AccessFlag = protected,
		Access = [family]
	; AccessFlag = private,
		Access = [private]
	; AccessFlag = default,
		Access = [assembly]
	; AccessFlag = local,
		% Access = [private]
		error("decl_flags_to_fieldattrs: local access flag")
	),
	PerInstanceFlag = per_instance(Flags),
	( PerInstanceFlag = one_copy,
		PerInstance = [static]
	; PerInstanceFlag = per_instance,
		PerInstance = []
	),
	ConstnessFlag = constness(Flags),
	( ConstnessFlag = modifiable,
		Constness = []
	; ConstnessFlag = const,
		Constness = [initonly]
	).


:- func entity_name_to_ilds_id(mlds__entity_name) = ilds__id.

entity_name_to_ilds_id(export(Name)) = Name.
entity_name_to_ilds_id(function(PredLabel, ProcId, MaybeSeqNum, _))
	= Name :-
	predlabel_to_id(PredLabel, ProcId, MaybeSeqNum, Name).
entity_name_to_ilds_id(type(Name, Arity))
	= string__format("%s_%d", [s(Name), i(Arity)]).
entity_name_to_ilds_id(data(DataName))
	= mangle_dataname(DataName).

:- func interface_id_to_class_name(mlds__interface_id) = ilds__class_name.

interface_id_to_class_name(_) = Result :-
		% XXX
	( semidet_succeed ->
		sorry(this_file, "interface_id_to_class_name NYI")
	;
		Result = structured_name(assembly("XXX"), [], [])
		
	).

%-----------------------------------------------------------------------------%

:- pred generate_method(ilds__class_name::in, maybe(ilds__class_name)::in,
		mlds__defn::in, class_member::out,
		il_info::in, il_info::out) is det.

generate_method(ClassName, _, defn(Name, Context, Flags, Entity),
		ClassMember) -->
	{ Entity = data(Type, DataInitializer, _GC_TraceCode) },

	{ FieldName = entity_name_to_ilds_id(Name) },

	{ Attrs = decl_flags_to_fieldattrs(Flags) },

		% Generate instructions to initialize this data.
		% There are two sorts of instructions,
		% instructions to allocate the data structure,
		% and instructions to initialize it.
		% See the comments about class constructors to
		% find out why we do this.
	data_initializer_to_instrs(DataInitializer, Type, AllocInstrsTree,
			InitInstrTree),

		% Make a field reference for the field
	DataRep =^ il_data_rep,
	{ ILType = mlds_type_to_ilds_type(DataRep, Type) },
	{ FieldRef = make_fieldref(ILType, ClassName, FieldName) },

		% If we had to allocate memory, the code
		% we generate looks like this:
		%
		%	// allocation for foo
		%	... allocation instructions ...
		%	stsfld thisclass::foo
		%
		%
		%	// initializer for foo
		%	ldsfld thisclass::foo
		%	... initialization code ...
		%	pop
		%
		% The final pop is necessary because the init
		% code will leave the field on the stack, but we
		% don't need it anymore (and we already set the
		% field when we allocated it).
		%
		% If no memory had to be allocated, the code is
		% a bit simpler.
		%
		%	// allocation for foo
		%	nothing here! 
		%	
		%	// initializer for foo
		%	... initialization code ...
		%	stsfld thisclass::foo
		%
		% Note that here we have to set the field.

	{ AllocInstrsTree = node([]) ->
		StoreAllocTree = node([]),
		StoreInitTree = node([stsfld(FieldRef)]),
		LoadTree = node([])
	;
		StoreAllocTree = node([stsfld(FieldRef)]),
		StoreInitTree = node([pop]),
		LoadTree = node([ldsfld(FieldRef)])
	},

		% Add a store after the alloc instrs (if necessary)
	{ AllocInstrs = list__condense(tree__flatten(
		tree__list([
			context_node(Context),
			comment_node(string__append("allocation for ",
				FieldName)),
			AllocInstrsTree, 
			StoreAllocTree]))) },

		% Add a load before the init instrs (if necessary)
	{ InitInstrs = list__condense(tree__flatten(
		tree__list([
			context_node(Context),
			comment_node(string__append("initializer for ",
				FieldName)),
			LoadTree,
			InitInstrTree,
			StoreInitTree]))) },
	
		% Add these instructions to the lists of
		% allocation/initialization instructions.
		% They will be put into the class constructor
		% later.
	il_info_add_alloc_instructions(AllocInstrs),
	il_info_add_init_instructions(InitInstrs),

	{ MaybeOffset = no },
	{ Initializer = none },

	{ ClassMember = field(Attrs, ILType, FieldName,
			MaybeOffset, Initializer) }.

generate_method(_, IsCons, defn(Name, Context, Flags, Entity), ClassMember) -->
	{ Entity = function(_MaybePredProcId, Params, MaybeStatement,
		Attributes) },

	il_info_get_module_name(ModuleName),

	/* XXX We formerly returned a list of definitions, so we could put
	 * this term in a comment term, so we cannot currently do this.

		% Generate a term (we use it to emit the complete
		% method definition as a comment, which is nice
		% for debugging).
	{ term__type_to_term(defn(Name, Context, Flags, Entity),
			_MLDSDefnTerm) },
	*/

		% Generate the signature
	{ Params = mlds__func_params(Args, Returns) },
	{ ILArgs = list__map(mlds_arg_to_il_arg, Args) },
	DataRep =^ il_data_rep,
	{ ILSignature = params_to_il_signature(DataRep, ModuleName, Params) },

		% Generate the name
	{ IsCons = yes(ParentClass),
		MemberName = ctor,
		CtorInstrs = [load_this,
			call(methoddef(call_conv(yes, default), void, 
			class_member_name(ParentClass, ctor), []))]
	; IsCons = no,
		MemberName = id(entity_name_to_ilds_id(Name)),
		CtorInstrs = []
	},

	{ Attrs = decl_flags_to_methattrs(Flags) },

		% Initialize the IL info with this method info.
	il_info_new_method(ILArgs, ILSignature, MemberName),

		% Start a new block, which we will use to wrap
		% up the entire method.
	il_info_get_next_block_id(BlockId),

		% Generate the code of the statement.
	( 
		{ MaybeStatement = defined_here(Statement) },
		statement_to_il(Statement, InstrsTree1)
	; 
		{ MaybeStatement = external },
			% If there is no function body, generate
			% forwarding code instead.  This can happen with
			% :- external
		atomic_statement_to_il(inline_target_code(lang_C, []),
				InstrsTree0),

			% The code might reference locals...
		il_info_add_locals(["succeeded" - mlds__native_bool_type]),
		( { Returns = [_] } ->
			% XXX Bug!
			% We assume that if there is a return value,
			% then it must be a semidet procedure, so
			% we return `succeeded'.
			% This is wrong for functions!
			{ InstrsTree1 = tree__list([
				InstrsTree0,
				instr_node(ldloc(name("succeeded"))),
				instr_node(ret)
			]) }
		;
			{ InstrsTree1 = InstrsTree0 }
		)
	),

		% Need to insert a ret for functions returning
		% void (MLDS doesn't).
	{ Returns = [] ->
		MaybeRet = instr_node(ret)
	;
		MaybeRet = empty
	},

		% Retrieve the locals, put them in the enclosing
		% scope.
	il_info_get_locals_list(Locals),
	{ InstrsTree2 = tree__list([
		context_node(Context),
		node(CtorInstrs),
		context_node(Context),
		instr_node(start_block(scope(Locals), BlockId)),
		InstrsTree1, 
		MaybeRet,
		instr_node(end_block(scope(Locals), BlockId))
		])
	},

		% If this is main, add the entrypoint, set a flag,
		% wrap the code in an exception handler and call the
		% initialization instructions in the cctor of this
		% module.
	(
		{ Name = function(PredLabel, _ProcId, MaybeSeqNum, _PredId) },
		{ PredLabel = pred(predicate, no, "main", 2, model_det, no) },
		{ MaybeSeqNum = no }
	->
		{ EntryPoint = [entrypoint] },
		il_info_add_init_instructions(runtime_initialization_instrs),
		^ has_main := yes,

		il_info_get_next_block_id(InnerTryBlockId),
		il_info_get_next_block_id(OuterTryBlockId),
		il_info_get_next_block_id(InnerCatchBlockId),
		il_info_get_next_block_id(OuterCatchBlockId),
		il_info_make_next_label(DoneLabel),

			% Replace all the returns with leave instructions;
			% as a side effect, this means that
			% we can no longer have any tail calls,
			% so replace them with nops.
		{ RenameRets = (func(I) = 
			(if (I = ret) then
				leave(label_target(DoneLabel))
			else if (I = tailcall) then
				nop
			else
				I
			)
		)},

		{ construct_qualified_term(
			qualified(unqualified("std_util"), "univ"),
			[], UnivMercuryType) },	
		{ UnivMLDSType = mercury_type(UnivMercuryType,
				user_type, non_foreign_type(UnivMercuryType)) },
		%
		% XXX Nasty hack alert!
		%
		% Currently the library doesn't build with --high-level-data.
		% So here we explicitly set --high-level-data to `no'
		% to reflect the fact that we're linking against the
		% version of the library compiled with --low-level-data.
		%
		{ XXX_LibraryDataRep = DataRep ^ highlevel_data := no },
		{ UnivType = mlds_type_to_ilds_type(XXX_LibraryDataRep,
			UnivMLDSType) },

		{ RenameNode = (func(N) = list__map(RenameRets, N)) },

		{ MercuryExceptionClassName = 
			mercury_runtime_name(["Exception"]) },
		
		{ ExceptionClassName = structured_name(il_system_assembly_name,
				["System", "Exception"], []) },

		{ FieldRef = make_fieldref(UnivType, MercuryExceptionClassName,
			"mercury_exception") },

		{ ConsoleWriteName = class_member_name(
				structured_name(il_system_assembly_name,
					["System", "Console"], []),
				id("Write")) },

		{ UncaughtExceptionName = class_member_name(
			mercury_library_wrapper_class_name(["exception"]),
				id("ML_report_uncaught_exception")) },

		{ WriteString = methoddef(call_conv(no, default),
					void, ConsoleWriteName,
					[il_string_type]) },
		{ WriteUncaughtException = methoddef(call_conv(no, default),
					void, UncaughtExceptionName,
					[UnivType]) },
		{ WriteObject = methoddef(call_conv(no, default),
					void, ConsoleWriteName,
					[il_generic_type]) },


			% A code block to catch any exception at all.
	
		{ CatchAnyException = tree__list([
			instr_node(start_block(
				catch(ExceptionClassName),
				OuterCatchBlockId)),
			instr_node(ldstr("\nUncaught system exception: \n")),
			instr_node(call(WriteString)),
			instr_node(call(WriteObject)),
			instr_node(leave(label_target(DoneLabel))),
			instr_node(end_block(catch(ExceptionClassName),
				OuterCatchBlockId))
			])
		},

			% Code to catch Mercury exceptions.
		{ CatchUserException = tree__list([
			instr_node(start_block(
				catch(MercuryExceptionClassName),
				InnerCatchBlockId)),
			instr_node(ldfld(FieldRef)),

			instr_node(call(WriteUncaughtException)),

			instr_node(leave(label_target(DoneLabel))),
			instr_node(end_block(
				catch(MercuryExceptionClassName),
				InnerCatchBlockId))
			])
		},

			% Wrap an exception handler around the main
			% code.  This allows us to debug programs
			% remotely without a window popping up asking
			% how you wish to debug.  Pressing the cancel
			% button on this window is a bit difficult
			% remotely.
			%
			% Inside this exception handler, we catch any 
			% exceptions and print them.
			%
			% We nest the Mercury exception handler so that any
			% exceptions thrown in ML_report_uncaught_exception
			% will be caught by the outer (more general) exception
			% handler.
			%
			% try {
			%	try {
			%		... main instructions ...
			%	}
			%	catch (mercury.runtime.Exception me) {
			%		ML_report_uncaught_exception(me);
			%	}
			% } 
			% catch (System.Exception e) {
			%	System.Console.Write(e);
			% }

		{ InstrsTree = tree__list([

					% outer try block
				instr_node(start_block(try, OuterTryBlockId)),

					% inner try block
				instr_node(start_block(try, InnerTryBlockId)),
				tree__map(RenameNode, InstrsTree2),
				instr_node(leave(label_target(DoneLabel))),
				instr_node(end_block(try, InnerTryBlockId)),
				
					% inner catch block
				CatchUserException,

				instr_node(leave(label_target(DoneLabel))),
				instr_node(end_block(try, OuterTryBlockId)),

					% outer catch block
				CatchAnyException,

				instr_node(label(DoneLabel)),
				instr_node(ret)
			]) }
	;
		{ EntryPoint = [] },
		{ InstrsTree = InstrsTree2 }
	),

		% Generate the entire method contents.
	DebugIlAsm =^ debug_il_asm,
	VerifiableCode =^ verifiable_code,
	{ MethodBody = make_method_defn(DebugIlAsm, VerifiableCode,
		InstrsTree) },
	{ CustomAttributes = attributes_to_custom_attributes(DataRep,
		Attributes) },
	{ list__condense([EntryPoint, CustomAttributes, MethodBody],
		MethodContents) },

	{ ClassMember = ilasm__method(methodhead(Attrs, MemberName,
			ILSignature, []), MethodContents)}.

generate_method(_, _, defn(Name, Context, Flags, Entity), ClassMember) -->
	{ Entity = class(ClassDefn) },
	generate_class_body(Name, Context, ClassDefn, _ClassName, EntityName,
			Extends, Interfaces, ClassMembers),
	{ ClassMember = nested_class(decl_flags_to_nestedclassattrs(Flags),
			EntityName, Extends, Interfaces, ClassMembers) }.

%-----------------------------------------------------------------------------%

:- func attributes_to_custom_attributes(il_data_rep, list(mlds__attribute))
		= list(method_body_decl).
attributes_to_custom_attributes(DataRep, Attrs) = 
	list__map(attribute_to_custom_attribute(DataRep), Attrs).

:- func attribute_to_custom_attribute(il_data_rep, mlds__attribute)
		= method_body_decl.
attribute_to_custom_attribute(DataRep, custom(MLDSType)) = custom(CustomDecl) :-
	ClassName = mlds_type_to_ilds_class_name(DataRep, MLDSType),
	MethodRef = get_constructor_methoddef(ClassName, []),
	CustomDecl = custom_decl(methodref(MethodRef), no, no_initalizer).

%-----------------------------------------------------------------------------%

:- func mangle_dataname(mlds__data_name) = string.

mangle_dataname(var(MLDSVarName))
	= mangle_mlds_var_name(MLDSVarName).
mangle_dataname(common(Int))
	= string__format("common_%s", [i(Int)]).
mangle_dataname(rtti(RttiTypeCtor, RttiName)) = MangledName :-
	rtti__addr_to_string(RttiTypeCtor, RttiName, MangledName).
mangle_dataname(base_typeclass_info(ClassId, InstanceStr)) = MangledName :-
        llds_out__make_base_typeclass_info_name(ClassId, InstanceStr,
		MangledName).
mangle_dataname(module_layout) = _MangledName :-
	error("unimplemented: mangling module_layout").
mangle_dataname(proc_layout(_)) = _MangledName :-
	error("unimplemented: mangling proc_layout").
mangle_dataname(internal_layout(_, _)) = _MangledName :-
	error("unimplemented: mangling internal_layout").
mangle_dataname(tabling_pointer(_)) = _MangledName :-
	error("unimplemented: mangling tabling_pointer").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% MLDS exports are converted into forwarding functions, which are
	% marked as public, are given the specified name, and simply call to
	% the "exported" function.
	%
	% They will be placed inside the "mercury_code" wrapper class with
	% all the other procedures.
	%
	% XXX much of this code should be generalized and turned into a
	% more general routine for generating MLDS forwarding functions.
	% We could use almost the same approach for outline_foreign_code
	% to generate the forwarding function.

:- pred mlds_export_to_mlds_defn(mlds__pragma_export::in, mlds__defn::out)
	is det.

mlds_export_to_mlds_defn(
	ml_pragma_export(ExportName, EntityName, Params, Context), Defn) :- 
	EntityName = qual(ModuleName, UnqualName),

	Params = mlds__func_params(Inputs, RetTypes),
	list__map_foldl(
		(pred(RT::in, RV - Lval::out, N0::in, N0 + 1::out) is det :-
			VN = var_name("returnval" ++ int_to_string(N0), no),
			% We don't need to worry about tracing variables for
			% accurate GC in the IL back-end -- the .NET runtime
			% system itself provides accurate GC.
			GC_TraceCode = no,
			RV = ml_gen_mlds_var_decl(
				var(VN), RT, no_initializer, GC_TraceCode,
				Context),
			Lval = var(qual(ModuleName, VN), RT)
		), RetTypes, ReturnVars, 0, _),

	EntNameToVarName = (func(EntName) = VarName :-
		( EntName = data(var(VarName0)) ->
			VarName = qual(ModuleName, VarName0)
		;
			error("exported method has argument without var name")
		)
	),
	ArgTypes = mlds__get_arg_types(Inputs),
	ArgRvals = list__map(
		(func(mlds__argument(EntName, Type, _GC_TraceCode)) =
				lval(var(VarName, Type)) :-
			VarName = EntNameToVarName(EntName)
		), Inputs),
	ReturnVarDecls = assoc_list__keys(ReturnVars),
	ReturnLvals = assoc_list__values(ReturnVars),
	ReturnRvals = list__map((func(X) = lval(X)), ReturnLvals),

	Signature = func_signature(ArgTypes, RetTypes),
	( 
		UnqualName = function(PredLabel, ProcId, _MaybeSeq, _PredId)
	->
		CodeRval = const(code_addr_const(proc(
			qual(ModuleName, PredLabel - ProcId),
			Signature)))
	;
		error("exported entity is not a function")
	),

		% XXX should we look for tail calls?
	CallStatement = statement(
		call(Signature, CodeRval, no, ArgRvals, ReturnLvals,
			call), Context),
	ReturnStatement = statement(return(ReturnRvals), Context),

	Statement = statement(mlds__block(ReturnVarDecls,
		( ReturnRvals = [] ->
			[CallStatement]
		;
			[CallStatement, ReturnStatement]
		)
		), Context),
	
	Attributes = [],
	DefnEntity = function(no, Params, defined_here(Statement),
		Attributes),

	Flags = init_decl_flags(public, one_copy, non_virtual, overridable,
		const, concrete),
	Defn = defn(export(ExportName), Context, Flags, DefnEntity).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	%
	% Code for generating initializers.
	%

	% Generate initializer code from an MLDS defn.  We are only expecting
	% data defns at this point (local vars), not functions or classes.
:- pred generate_defn_initializer(mlds__defn, instr_tree, instr_tree, 
	il_info, il_info).
:- mode generate_defn_initializer(in, in, out, in, out) is det.
generate_defn_initializer(defn(Name, Context, _DeclFlags, Entity),
		Tree0, Tree) --> 
	( 
		{ Name = data(DataName) },
		{ Entity = mlds__data(MLDSType, Initializer, _GC_TraceCode) }
	->
		( { Initializer = no_initializer } ->
			{ Tree = Tree0 }
		;
			( { DataName = var(VarName) } ->
				il_info_get_module_name(ModuleName),
				{ Lval = var(qual(ModuleName, VarName), 
					MLDSType) },
				get_load_store_lval_instrs(Lval,
					LoadMemRefInstrs, StoreLvalInstrs),
				{ NameString = mangle_mlds_var_name(VarName) }
			;
				{ LoadMemRefInstrs = throw_unimplemented(
					"initializer_for_non_var_data_name") },
				{ StoreLvalInstrs = node([]) },
				{ NameString = "unknown" }
			),
			data_initializer_to_instrs(Initializer, MLDSType,
				AllocInstrs, InitInstrs),
			{ string__append("initializer for ", NameString, 
				Comment) },
			{ Tree = tree__list([
				Tree0,
				context_node(Context),
				comment_node(Comment),
				LoadMemRefInstrs,
				AllocInstrs,
				InitInstrs,
				StoreLvalInstrs
				]) }
		)
	;
		{ unexpected(this_file, "defn not data(...) in block") }
	).

	% initialize this value, leave it on the stack.
	% XXX the code generator doesn't box these values
	% we need to look ahead at them and box them appropriately.
:- pred data_initializer_to_instrs(mlds__initializer::in, mlds__type::in,
	instr_tree::out, instr_tree::out, il_info::in, il_info::out) is det.
data_initializer_to_instrs(init_obj(Rval), _Type, node([]), InitInstrs) --> 
	load(Rval, InitInstrs).

	% MLDS structures initializers are assumed to be initialized like
	% structures in C, which means nested elements are actually laid out
	% flat in the structure.
	%
	% So we flatten structures, and then process them as arrays
	% (this may have to be re-visited if used to initialise high-level
	% data).

data_initializer_to_instrs(init_struct(InitList0), Type,
		AllocInstrs, InitInstrs) --> 

	
	{ InitList = flatten_inits(InitList0) },
	data_initializer_to_instrs(init_array(InitList), Type,
		AllocInstrs, InitInstrs).

	% Put the array allocation in AllocInstrs.
	% For sub-initializations, we don't worry about keeping AllocInstrs
	% and InitInstrs apart, since we are only interested in top level
	% allocations.
data_initializer_to_instrs(init_array(InitList), Type,
		AllocInstrs, InitInstrs) -->

		%
		% figure out the array element type
		%
	DataRep =^ il_data_rep,
	( { Type = mlds__array_type(ElemType0) } ->
		{ ElemType = ElemType0 },
		{ ILElemType = mlds_type_to_ilds_type(DataRep, ElemType) }
	;
		% XXX we assume struct fields have type mlds__generic_type
		% This is probably wrong for --high-level-data
		{ ElemType = mlds__generic_type },
		{ ILElemType = il_generic_type }
	),
	{ ILElemType = ilds__type(_, ILElemSimpleType) },

		% To initialize an array, we generate the following
		% code:
		% 	ldc <length of array>
		% 	newarr <array element type>
		%	
		% Then, for each element in the array:
		%	dup
		%	ldc <index of this element in the array>
		%	... allocation instructions ...
		%	... initialization instructions ...
		%	box the value (if necessary)
		%	stelem <array element type>
		%
		% The initialization will leave the array on the stack.
		%	
	{ AllocInstrs = node([
		ldc(int32, i(list__length(InitList))), 
		newarr(ILElemType)]) },
	{ AddInitializer = 
		(pred(Init0::in, X0 - Tree0::in, (X0 + 1) - Tree::out,
				in, out) is det -->
			% we may need to box the arguments
			% XXX is this right?
			( { ElemType = mlds__generic_type } ->
				maybe_box_initializer(Init0, Init)
			;
				{ Init = Init0 }
			),
			data_initializer_to_instrs(Init, ElemType,
				ATree1, ITree1),
			{ Tree = tree(tree(Tree0, node(
					[dup, ldc(int32, i(X0))])), 
				tree(tree(ATree1, ITree1), 
					node([stelem(ILElemSimpleType)]
				))) }
		) },
	list__foldl2(AddInitializer, InitList, 0 - empty, _ - InitInstrs).
data_initializer_to_instrs(no_initializer, _, node([]), node([])) --> [].

	% If we are initializing an array or struct, we need to box
	% all the things inside it.
:- pred maybe_box_initializer(mlds__initializer, mlds__initializer, 
	il_info, il_info).
:- mode maybe_box_initializer(in, out, in, out) is det.

	% nothing to do
maybe_box_initializer(no_initializer, no_initializer) --> [].
	% array already boxed
maybe_box_initializer(init_array(X), init_array(X)) --> [].
	% struct already boxed
maybe_box_initializer(init_struct(X), init_struct(X)) --> [].
	% single items need to be boxed
maybe_box_initializer(init_obj(Rval), init_obj(NewRval)) -->
	{ rval_to_type(Rval, BoxType) },
	{ NewRval = unop(box(BoxType), Rval) }.


	% Code to flatten nested intializers.

:- func flatten_inits(list(mlds__initializer)) = list(mlds__initializer).
flatten_inits(Inits) = list__condense(list__map(flatten_init, Inits)).

:- func flatten_init(mlds__initializer) = list(mlds__initializer).
flatten_init(I) = Inits :-
	( I = init_struct(Inits0) ->
		Inits = flatten_inits(Inits0)
	; I = init_array(Inits0) ->
		Inits = flatten_inits(Inits0)
	;
		Inits = [I]
	).
	


%-----------------------------------------------------------------------------%
%
% Convert basic MLDS statements into IL.
%

:- pred statements_to_il(list(mlds__statement), instr_tree, il_info, il_info).
:- mode statements_to_il(in, out, in, out) is det.
statements_to_il([], empty) --> [].
statements_to_il([ S | Statements], tree(Instrs0, Instrs1)) -->
	statement_to_il(S, Instrs0),
	statements_to_il(Statements, Instrs1).


:- pred statement_to_il(mlds__statement, instr_tree, il_info, il_info).
:- mode statement_to_il(in, out, in, out) is det.

statement_to_il(statement(block(Defns, Statements), Context),
		Instrs) -->
	il_info_get_module_name(ModuleName),
	il_info_get_next_block_id(BlockId),
	{ list__map(defn_to_local(ModuleName), Defns, Locals) },
	il_info_add_locals(Locals),
	list__foldl2(generate_defn_initializer, Defns, empty,
		InitInstrsTree),
	statements_to_il(Statements, BlockInstrs),
	DataRep =^ il_data_rep,
	{ list__map((pred((K - V)::in, (K - W)::out) is det :- 
		W = mlds_type_to_ilds_type(DataRep, V)), Locals, ILLocals) },
	{ Scope = scope(ILLocals) },
	{ Instrs = tree__list([
			context_node(Context),
			instr_node(start_block(Scope, BlockId)),
			InitInstrsTree,
			comment_node("block body"),
			BlockInstrs,
			node([end_block(Scope, BlockId)])
			]) },
	il_info_remove_locals(Locals).

statement_to_il(statement(atomic(Atomic), Context), Instrs) -->
	atomic_statement_to_il(Atomic, AtomicInstrs),
	{ Instrs = tree(context_node(Context), AtomicInstrs) }.

statement_to_il(statement(call(Sig, Function, _This, Args, Returns, IsTail), 
		Context), Instrs) -->
	VerifiableCode =^ verifiable_code,
	ByRefTailCalls =^ il_byref_tailcalls,
	MsCLR =^ support_ms_clr,
	DataRep =^ il_data_rep,
	{ TypeParams = mlds_signature_to_ilds_type_params(DataRep, Sig) },
	{ ReturnParam = mlds_signature_to_il_return_param(DataRep, Sig) },
	CallerSig =^ signature,
	{ CallerSig = signature(_, CallerReturnParam, _) },
	(
		{ IsTail = tail_call },
		% if --verifiable-code is enabled,
		% and the arguments contain one or more byrefs,
		% then don't emit the "tail." prefix,
		% unless --il-byref-tailcalls is set
		\+ (
			{ VerifiableCode = yes },
			some [Ref] (
				{ list__member(Ref, TypeParams) },
				{ Ref = ilds__type(_, '&'(_)) 
				; Ref = ilds__type(_, '*'(_)) 
				; Ref = ilds__type(_, refany) 
				}
			),
			{ ByRefTailCalls = no }
		),
		% if --verifiable-code is enabled, then we must not output
		% the "tail." prefix unless the callee return type is
		% compatible with the caller return type
		\+ (
			{ VerifiableCode = yes },
			{ ReturnParam \= CallerReturnParam }
		),
		% In the MS CLR implementation the callee and caller return
		% type of a tail call must be compatible even when we are
		% using unverifiable code.
		\+ (
			{ MsCLR = yes },
			{ ReturnParam \= CallerReturnParam }
		)
	->
		{ TailCallInstrs = [tailcall] },
		% For calls marked with "tail.", we need a `ret'
		% instruction immediately after the call (this is in fact
		% needed for correct IL, not just for verifiability)
		{ RetInstrs = [ret] },
		{ ReturnsStoredInstrs = empty },
		{ LoadMemRefInstrs = empty }
	;
		% For non-tail calls, we might have to load a memory
		% reference before the call so we can store the result
		% into the memory reference after the call.
		{ TailCallInstrs = [] },
		{ RetInstrs = [] },
		get_all_load_store_lval_instrs(Returns,
			LoadMemRefInstrs, ReturnsStoredInstrs)
	),
	list__map_foldl(load, Args, ArgsLoadInstrsTrees),
	{ ArgsLoadInstrs = tree__list(ArgsLoadInstrsTrees) },
	( { Function = const(_) } ->
		{ FunctionLoadInstrs = empty },
		{ rval_to_function(Function, MemberName) },
		{ Instrs0 = [call(methoddef(call_conv(no, default),
			ReturnParam, MemberName, TypeParams))] }
	;
		load(Function, FunctionLoadInstrs),
		{ list__length(TypeParams, Length) },
		{ list__duplicate(Length, no, NoList) },
		{ assoc_list__from_corresponding_lists(
			TypeParams, NoList, ParamsList) },
		{ Instrs0 = [calli(signature(call_conv(no, default),
			ReturnParam, ParamsList))] }
	),		
	{ Instrs = tree__list([
			context_node(Context),
			comment_node("call"), 
			LoadMemRefInstrs,
			ArgsLoadInstrs,
			FunctionLoadInstrs,
			node(TailCallInstrs),
			node(Instrs0), 
			node(RetInstrs),
			ReturnsStoredInstrs
			]) }.

statement_to_il(statement(if_then_else(Condition, ThenCase, ElseCase), 
		Context), Instrs) -->
	generate_condition(Condition, ConditionInstrs, ElseLabel),
	il_info_make_next_label(DoneLabel),
	statement_to_il(ThenCase, ThenInstrs),
	maybe_map_fold(statement_to_il, ElseCase, empty, ElseInstrs),
	{ Instrs = tree__list([
		context_node(Context),
		comment_node("if then else"),
		ConditionInstrs,
		comment_node("then case"),
		ThenInstrs,
		instr_node(br(label_target(DoneLabel))),
		instr_node(label(ElseLabel)),
		comment_node("else case"),
		ElseInstrs,
		comment_node("end if then else"),
		instr_node(label(DoneLabel))
		]) }.

statement_to_il(statement(switch(_Type, _Val, _Range, _Cases, _Default),
		_Context), _Instrs) -->
	% The IL back-end only supports computed_gotos and if-then-else chains;
	% the MLDS code generator should either avoid generating MLDS switches,
	% or should transform them into computed_gotos or if-then-else chains.
	{ error("mlds_to_il.m: `switch' not supported") }.

statement_to_il(statement(while(Condition, Body, AtLeastOnce), 
		Context), Instrs) -->
	generate_condition(Condition, ConditionInstrs, EndLabel),
	il_info_make_next_label(StartLabel),
	statement_to_il(Body, BodyInstrs),
	{ AtLeastOnce = no,
		Instrs = tree__list([
			context_node(Context),
			comment_node("while"),
			instr_node(label(StartLabel)),
			ConditionInstrs,
			BodyInstrs,
			instr_node(br(label_target(StartLabel))),
			instr_node(label(EndLabel))
		])
	; AtLeastOnce = yes, 
			% XXX this generates a branch over branch which
			% is suboptimal.
		Instrs = tree__list([
			context_node(Context),
			comment_node("while (actually do ... while)"),
			instr_node(label(StartLabel)),
			BodyInstrs,
			ConditionInstrs,
			instr_node(br(label_target(StartLabel))),
			instr_node(label(EndLabel))
		])

	}.

statement_to_il(statement(return(Rvals), Context), Instrs) -->
	( { Rvals = [Rval] } ->
		load(Rval, LoadInstrs),
		{ Instrs = tree__list([
			context_node(Context),
			LoadInstrs,
			instr_node(ret)]) }
	; { Rvals = [] } ->
		{ unexpected(this_file, "empty list of return values") }
	;
		% MS IL doesn't support multiple return values
		{ sorry(this_file, "multiple return values") }
	).

statement_to_il(statement(label(Label), Context), Instrs) -->
	{ string__format("label %s", [s(Label)], Comment) },
	{ Instrs = node([
			comment(Comment),
			context_instr(Context),
			label(Label)
		]) }.

statement_to_il(statement(goto(label(Label)), Context), Instrs) -->
	{ string__format("goto %s", [s(Label)], Comment) },
	{ Instrs = node([
			comment(Comment),
			context_instr(Context),
			br(label_target(Label))
		]) }.

statement_to_il(statement(goto(break), _Context), _Instrs) -->
	{ sorry(this_file, "break") }.

statement_to_il(statement(goto(continue), _Context), _Instrs) -->
	{ sorry(this_file, "continue") }.

statement_to_il(statement(do_commit(_Ref), Context), Instrs) -->

	% For commits, we use exception handling.
	%
	% For a do_commit instruction, we generate code equivalent 
	% to the following C++/C#/Java code:
	%
	%	throw new mercury::runtime::Commit();
	%
	% In IL the code looks like this:
	% 
	%	newobj  instance void
	%		['mercury']'mercury'.'runtime'.'Commit'::.ctor()
	% 	throw
	% 

	{ NewObjInstr = newobj_constructor(il_commit_class_name, []) },
	{ Instrs = tree__list([
			context_node(Context),
			comment_node("do_commit/1"),
			instr_node(NewObjInstr),
			instr_node(throw)
		]) }.

statement_to_il(statement(try_commit(_Ref, GoalToTry, CommitHandlerGoal), 
		Context), Instrs) -->

	% For commits, we use exception handling.
	%
	% For try_commit instructions, we generate IL code
	% of the following form:
	%
	% 	.try {	
	%		<GoalToTry>
	%		leave label1
	% 	} catch commit_type {
	%		pop	// discard the exception object
	% 		<CommitHandlerGoal>
	%		leave label1
	% 	}
	% 	label1:
	% 

	il_info_get_next_block_id(TryBlockId),
	statement_to_il(GoalToTry, GoalInstrsTree),
	il_info_get_next_block_id(CatchBlockId),
	statement_to_il(CommitHandlerGoal, HandlerInstrsTree),
	il_info_make_next_label(DoneLabel),

	{ ClassName = il_commit_class_name },
	{ Instrs = tree__list([
		context_node(Context),
		comment_node("try_commit/3"),

		instr_node(start_block(try, TryBlockId)),
		GoalInstrsTree,
		instr_node(leave(label_target(DoneLabel))),
		instr_node(end_block(try, TryBlockId)),

		instr_node(start_block(catch(ClassName), CatchBlockId)),
		comment_node("discard the exception object"),
		instr_node(pop),
		HandlerInstrsTree,
		instr_node(leave(label_target(DoneLabel))),
		instr_node(end_block(catch(ClassName), CatchBlockId)),
		instr_node(label(DoneLabel))

		]) }.

statement_to_il(statement(computed_goto(Rval, MLDSLabels), Context), 
		Instrs) -->
	load(Rval, RvalLoadInstrs),
	{ Targets = list__map(func(L) = label_target(L), MLDSLabels) },
	{ Instrs = tree__list([
		context_node(Context),
		comment_node("computed goto"),
		RvalLoadInstrs,
		instr_node(switch(Targets))
		]) }.


	
:- pred atomic_statement_to_il(mlds__atomic_statement, instr_tree, 
	il_info, il_info).
:- mode atomic_statement_to_il(in, out, in, out) is det.

atomic_statement_to_il(gc_check, node(Instrs)) --> 
	{ Instrs = [comment(
		"gc check -- not relevant for this backend")] }.
atomic_statement_to_il(mark_hp(_), node(Instrs)) --> 
	{ Instrs = [comment(
		"mark hp -- not relevant for this backend")] }.
atomic_statement_to_il(restore_hp(_), node(Instrs)) --> 
	{ Instrs = [comment(
		"restore hp -- not relevant for this backend")] }.

atomic_statement_to_il(outline_foreign_proc(Lang, ReturnLvals, _Code),
		Instrs) --> 
	il_info_get_module_name(ModuleName),
	( no =^ method_foreign_lang  ->
		=(Info),
		^ method_foreign_lang := yes(Lang),
		^ file_foreign_langs := 
			set__insert(Info ^ file_foreign_langs, Lang),
		{ mangle_foreign_code_module(ModuleName, Lang,
			OutlineLangModuleName) },
		{ ClassName = mlds_module_name_to_class_name(
			OutlineLangModuleName) },
		signature(_, RetType, Params) =^ signature, 

		( { ReturnLvals = [] } ->
			% If there is a return type, but no return value, it
			% must be a semidet predicate so put it in 
			% SUCCESS_INDICATOR.
			% XXX it would be better to get the code generator
			% to tell us this is the case directly
			{ LoadInstrs = empty },
			{ RetType = void ->
				StoreInstrs = empty
			;
				StoreInstrs = instr_node(
					stloc(name("SUCCESS_INDICATOR")))
			}
		; { ReturnLvals = [ReturnLval] } ->
			get_load_store_lval_instrs(ReturnLval,
				LoadInstrs, StoreInstrs)
		;
			{ sorry(this_file, "multiple return values") }
		),
		MethodName =^ method_name,
		{ assoc_list__keys(Params, TypeParams) },
		{ list__map_foldl((pred(_::in, Instr::out,
			Num::in, Num + 1::out) is det :-
				Instr = ldarg(index(Num))),
			TypeParams, LoadArgInstrs, 0, _) },
		{ Instrs = tree__list([
			comment_node(
 			    "outline foreign proc -- call handwritten version"),
			LoadInstrs,
			node(LoadArgInstrs),
			instr_node(call(get_static_methodref(
				ClassName, MethodName, RetType, TypeParams))),
			StoreInstrs
			]) }
	;
		{ Instrs = comment_node(
			"outline foreign proc -- already called") }
	).

	% XXX we assume lang_C is MC++
atomic_statement_to_il(inline_target_code(lang_C, _Code), Instrs) --> 
	il_info_get_module_name(ModuleName),
	( no =^ method_foreign_lang  ->
			% XXX we hardcode managed C++ here
		=(Info),
		^ method_foreign_lang := yes(managed_cplusplus),
		^ file_foreign_langs := 
			set__insert(Info ^ file_foreign_langs,
			managed_cplusplus),
		{ mangle_dataname_module(no, ModuleName, NewModuleName) },
		{ ClassName = mlds_module_name_to_class_name(NewModuleName) },
		signature(_, RetType, Params) =^ signature, 
			% If there is a return value, put it in succeeded.
			% XXX this is incorrect for functions, which might
			% return a useful value.
		{ RetType = void ->
			StoreReturnInstr = empty
		; RetType = simple_type(bool) ->
			StoreReturnInstr = instr_node(stloc(name("succeeded")))
		;
			sorry(this_file, "functions in MC++")
		},
		MethodName =^ method_name,
		{ assoc_list__keys(Params, TypeParams) },
		{ list__map_foldl((pred(_::in, Instr::out,
			Num::in, Num + 1::out) is det :-
				Instr = ldarg(index(Num))),
			TypeParams, LoadInstrs, 0, _) },
		{ Instrs = tree__list([
			comment_node("inline target code -- call handwritten version"),
			node(LoadInstrs),
			instr_node(call(get_static_methodref(ClassName,
				MethodName, RetType, TypeParams))),
			StoreReturnInstr
			]) }
	;
		{ Instrs = comment_node("inline target code -- already called") }
	).
atomic_statement_to_il(inline_target_code(lang_il, Code), Instrs) --> 
	{ Instrs = inline_code_to_il_asm(Code) }.
atomic_statement_to_il(inline_target_code(lang_java_bytecode, _), _) --> 
	{ unexpected(this_file, "lang_java_bytecode") }.
atomic_statement_to_il(inline_target_code(lang_java_asm, _), _) --> 
	{ unexpected(this_file, "lang_java_asm") }.
atomic_statement_to_il(inline_target_code(lang_asm, _), _) --> 
	{ unexpected(this_file, "lang_asm") }.
atomic_statement_to_il(inline_target_code(lang_GNU_C, _), _) --> 
	{ unexpected(this_file, "lang_GNU_C") }.
atomic_statement_to_il(inline_target_code(lang_C_minus_minus, _), _) --> 
	{ unexpected(this_file, "lang_C_minus_minus") }.


atomic_statement_to_il(trail_op(_), node(Instrs)) --> 
	{ Instrs = [comment(
		"... some trail operation ... (unimplemented)")] }.

atomic_statement_to_il(assign(Lval, Rval), Instrs) -->
	% do assignments by loading the rval and storing
	% to the lval
	load(Rval, LoadRvalInstrs),
	get_load_store_lval_instrs(Lval, LoadMemRefInstrs, StoreLvalInstrs),
	{ Instrs = tree__list([
		comment_node("assign"),
		LoadMemRefInstrs,
		LoadRvalInstrs,
		StoreLvalInstrs
		]) }.
atomic_statement_to_il(comment(Comment), Instrs) -->
	{ Instrs = node([comment(Comment)]) }.

atomic_statement_to_il(delete_object(Target), Instrs) -->
		% XXX we assume the code generator knows what it is
		% doing and is only going to delete real objects (e.g.
		% reference types).  It would perhaps be prudent to
		% check the type of delete_object (if it had one) to
		% make sure.
	
		% We implement delete_object by storing null in the
		% lval, which hopefully gives the garbage collector a good
		% solid hint that this storage is no longer required.
	get_load_store_lval_instrs(Target, LoadInstrs, StoreInstrs),
	{ Instrs = tree__list([LoadInstrs, instr_node(ldnull), StoreInstrs]) }.

atomic_statement_to_il(new_object(Target, _MaybeTag, HasSecTag, Type, Size,
		MaybeCtorName, Args0, ArgTypes0), Instrs) -->
	DataRep =^ il_data_rep,
	( 
		{ 
			Type = mlds__generic_env_ptr_type
		;
			Type = mlds__class_type(_, _, mlds__class) 
		;
			DataRep ^ highlevel_data = yes,
			Type = mlds__mercury_type(_, user_type, _)
		}
	->
			% If this is a class, we should call the
			% constructor.  (This is needed for nondet environment
			% classes, and also for high-level data.)
			% We generate code of the form:
			%
			% 	... load memory reference ...
			%	// new object (call constructor)
			%	... load each argument ...
			%	call ClassName::.ctor
			%	... store to memory reference ...
			%
		{ ClassName0 = mlds_type_to_ilds_class_name(DataRep, Type) },
		( { MaybeCtorName = yes(QualifiedCtorName) } ->
			{ QualifiedCtorName = qual(_,
				ctor_id(CtorName, CtorArity)) },
			{ CtorType = entity_name_to_ilds_id(
				type(CtorName, CtorArity)) },
		 	{ ClassName = append_nested_class_name(ClassName0,
				[CtorType]) }
		;
		 	{ ClassName = ClassName0 }
		),
			% Skip the secondary tag, if any
		{ HasSecTag = yes ->
			(
				ArgTypes0 = [_SecondaryTag | ArgTypes1],
				Args0 = [_SecondaryTagVal | Args1]
			->
				Args = Args1,
				ArgTypes = ArgTypes1
			;
				unexpected(this_file,
					"newobj without secondary tag")
			)
		;
			ArgTypes = ArgTypes0,
			Args = Args0
		},
		{ ILArgTypes = list__map(mlds_type_to_ilds_type(DataRep),
					ArgTypes) },
		list__map_foldl(load, Args, ArgsLoadInstrsTrees),
		{ ArgsLoadInstrs = tree__list(ArgsLoadInstrsTrees) },
		get_load_store_lval_instrs(Target, LoadMemRefInstrs,
			StoreLvalInstrs),
		{ CallCtor = newobj_constructor(ClassName, ILArgTypes) },
		{ Instrs = tree__list([
			LoadMemRefInstrs, 
			comment_node("new object (call constructor)"),
			ArgsLoadInstrs,
			instr_node(CallCtor),
			StoreLvalInstrs
			]) }
	;
			% Otherwise this is a generic mercury object -- we 
			% use an array of System::Object to represent
			% it.
			%
			% 	... load memory reference ...
			%	// new object 
			%	ldc <size of array>
			%	newarr
			%
			% And then for each array element:
			%
			%	dup
			%	ldc <array index>
			%	... load rval ...
			%	stelem System::Object
			%
			% Finally, after all the array elements have
			% been set:
			%
			%	... store to memory reference ...
			%
			% Note that the MLDS code generator is
			% responsible for boxing/unboxing the
			% arguments if needed.

			% Load each rval 
			% (XXX we do almost exactly the same code when
			% initializing array data structures -- we
			% should reuse that code.
		{ LoadInArray = (pred(Rval::in, I::out, Arg0::in, 
				Arg::out) is det :- 
			Arg0 = Index - S0,
			I0 = instr_node(dup),
			load(const(int_const(Index)), I1, S0, S1),
			load(Rval, I2, S1, S), 
			I3 = instr_node(stelem(il_generic_simple_type)),
			I = tree__list([I0, I1, I2, I3]),
			Arg = (Index + 1) - S
		) },
		=(State0),
		{ list__map_foldl(LoadInArray, Args0, ArgsLoadInstrsTrees,
			0 - State0, _ - State) },
		{ ArgsLoadInstrs = tree__list(ArgsLoadInstrsTrees) },
		dcg_set(State),

			% Get the instructions to load and store the
			% target.
		get_load_store_lval_instrs(Target, LoadMemRefInstrs,
			StoreLvalInstrs),

		{ Size = yes(SizeInWordsRval0) ->
			SizeInWordsRval = SizeInWordsRval0
		;
			% XXX do we need to handle this case?
			% I think it's needed for --high-level-data
			error("unknown size in MLDS new_object")
		},
		load(SizeInWordsRval, LoadSizeInstrs),

		{ Instrs = tree__list([
			LoadMemRefInstrs,
			comment_node("new object"),
			LoadSizeInstrs,
			instr_node(newarr(il_generic_type)),
			ArgsLoadInstrs,
			StoreLvalInstrs
			]) }
	).

:- func inline_code_to_il_asm(list(target_code_component)) = instr_tree.
inline_code_to_il_asm([]) = empty.
inline_code_to_il_asm([T | Ts]) = tree(Instrs, Rest) :-
	( 
		T = user_target_code(Code, MaybeContext, Attrs),
		( yes(max_stack_size(N)) = get_max_stack_attribute(Attrs) ->
			Instrs = tree__list([
				( MaybeContext = yes(Context) ->
					context_node(mlds__make_context(
						Context))
				;
					empty
				),
				instr_node(il_asm_code(Code, N))
				])
		;
			error(this_file ++ ": max_stack_size not set")
		)
	;
		T = raw_target_code(Code, Attrs),
		( yes(max_stack_size(N)) = get_max_stack_attribute(Attrs) ->
			Instrs = instr_node(il_asm_code(Code, N))
		;
			error(this_file ++ ": max_stack_size not set")
		)
	;
		T = target_code_input(_),
		Instrs = empty
	;
		T = target_code_output(_),
		Instrs = empty
	;
		T = name(_),
		Instrs = empty
	),
	Rest = inline_code_to_il_asm(Ts).

:- func get_max_stack_attribute(target_code_attributes) =
		maybe(target_code_attribute).
get_max_stack_attribute([]) = no.
get_max_stack_attribute([X | _Xs]) = yes(X) :- X = max_stack_size(_).

	
:- pred get_all_load_store_lval_instrs(list(lval), instr_tree, instr_tree,
		il_info, il_info).
:- mode get_all_load_store_lval_instrs(in, out, out, in, out) is det.
get_all_load_store_lval_instrs([], empty, empty) --> [].
get_all_load_store_lval_instrs([Lval | Lvals], 
		tree(LoadMemRefNode, LoadMemRefTree),
		tree(StoreLvalNode, StoreLvalTree)) -->
	get_load_store_lval_instrs(Lval, LoadMemRefNode, StoreLvalNode),
	get_all_load_store_lval_instrs(Lvals, LoadMemRefTree, StoreLvalTree).

	% Some lvals need to be loaded before you load the rval.
	% XXX It would be much better if this took the lval and the rval and
	% just gave you a single tree.  Instead it gives you the
	% "before" tree and the "after" tree and asks you to sandwich
	% the rval in between.
	% The predicate `store' should probably take the lval and the
	% rval and do all of this at once.
:- pred get_load_store_lval_instrs(lval, instr_tree, instr_tree, il_info,
		il_info).
:- mode get_load_store_lval_instrs(in, out, out, in, out) is det.
get_load_store_lval_instrs(Lval, LoadMemRefInstrs,
		StoreLvalInstrs) -->
	DataRep =^ il_data_rep,
	( { Lval = mem_ref(Rval0, MLDS_Type) } ->
		load(Rval0, LoadMemRefInstrs),
		{ SimpleType = mlds_type_to_ilds_simple_type(DataRep,
			MLDS_Type) },
		{ StoreLvalInstrs = instr_node(stind(SimpleType)) } 
	; { Lval = field(_MaybeTag, FieldRval, FieldNum, FieldType, 
			ClassType) } -> 
		{ get_fieldref(DataRep, FieldNum, FieldType, ClassType,
			FieldRef, CastClassInstrs) },
		load(FieldRval, LoadMemRefInstrs0),
		{ LoadMemRefInstrs = tree__list([
			LoadMemRefInstrs0,
			CastClassInstrs]) },
		{ StoreLvalInstrs = instr_node(stfld(FieldRef)) } 
	;
		{ LoadMemRefInstrs = empty },
		store(Lval, StoreLvalInstrs)
	).

%-----------------------------------------------------------------------------%
%
% Load and store.
%
% NOTE: Be very careful calling store directly.  You probably want to
% call get_load_store_lval_instrs to generate the prelude part (which
% will load any memory reference that need to be loaded) and the store
% part (while will store the rval into the pre-loaded lval), and then
% sandwich the calculation of the rval in between the two.
%

:- pred load(mlds__rval, instr_tree, il_info, il_info) is det.
:- mode load(in, out, in, out) is det.

load(lval(Lval), Instrs) -->
	DataRep =^ il_data_rep,
	( { Lval = var(Var, VarType) },
		{ mangle_mlds_var(Var, MangledVarStr) },
		=(Info),
		{ is_local(MangledVarStr, Info) ->
			Instrs = instr_node(ldloc(name(MangledVarStr)))
		; is_argument(MangledVarStr, Info) ->
			Instrs = instr_node(ldarg(name(MangledVarStr)))
		; is_local_field(Var, VarType, Info, FieldRef) ->
			Instrs = instr_node(ldsfld(FieldRef))
		;
			FieldRef = make_static_fieldref(DataRep, Var, VarType),
			Instrs = instr_node(ldsfld(FieldRef))
		}
	; { Lval = field(_MaybeTag, Rval, FieldNum, FieldType, ClassType) },
		load(Rval, RvalLoadInstrs),
		( { FieldNum = offset(OffSet) } ->
			{ SimpleFieldType = mlds_type_to_ilds_simple_type(
				DataRep, FieldType) },
			load(OffSet, OffSetLoadInstrs),
			{ CastClassInstrs = empty },
			{ LoadInstruction = ldelem(SimpleFieldType) }
		;
			{ get_fieldref(DataRep, FieldNum, FieldType, ClassType,
				FieldRef, CastClassInstrs) },
			{ LoadInstruction = ldfld(FieldRef) },
			{ OffSetLoadInstrs = empty }
		),
		{ Instrs = tree__list([
				RvalLoadInstrs, 
				CastClassInstrs,
				OffSetLoadInstrs, 
				instr_node(LoadInstruction)
				]) }
	; { Lval = mem_ref(Rval, MLDS_Type) }, 
		{ SimpleType = mlds_type_to_ilds_simple_type(DataRep,
			MLDS_Type) },
		load(Rval, RvalLoadInstrs),
		{ Instrs = tree__list([
			RvalLoadInstrs,
			instr_node(ldind(SimpleType))
			]) }
	).

load(mkword(_Tag, _Rval), Instrs) -->
	{ Instrs = comment_node("unimplemented load rval mkword") }.

	% XXX check these, what should we do about multi strings, 
	% characters, etc.
load(const(Const), Instrs) -->
	DataRep =^ il_data_rep,
		% true and false are just the integers 1 and 0
	{ Const = true,
		Instrs = instr_node(ldc(bool, i(1)))
	; Const = false,
		Instrs = instr_node(ldc(bool, i(0)))
	; Const = string_const(Str),
		Instrs = instr_node(ldstr(Str))
	; Const = int_const(Int),
		Instrs = instr_node(ldc(int32, i(Int)))
	; Const = float_const(Float),
		Instrs = instr_node(ldc(float64, f(Float)))
	; Const = multi_string_const(_Length, _MultiString),
		Instrs = throw_unimplemented("load multi_string_const")
	; Const = code_addr_const(CodeAddr),
		MethodRef = code_addr_constant_to_methodref(DataRep, CodeAddr),
		Instrs = instr_node(ldftn(MethodRef))
	; Const = data_addr_const(DataAddr),
		data_addr_constant_to_fieldref(DataAddr, FieldRef),
		Instrs = instr_node(ldsfld(FieldRef))
	; Const = null(_MLDSType),
			% We might consider loading an integer for 
			% null function types.
		Instrs = instr_node(ldnull)
	}.

load(unop(Unop, Rval), Instrs) -->
	load(Rval, RvalLoadInstrs),
	unaryop_to_il(Unop, Rval, UnOpInstrs),
	{ Instrs = tree__list([RvalLoadInstrs, UnOpInstrs]) }.

load(binop(BinOp, R1, R2), Instrs) -->
	load(R1, R1LoadInstrs),
	load(R2, R2LoadInstrs),
	binaryop_to_il(BinOp, BinaryOpInstrs),
	{ Instrs = tree__list([R1LoadInstrs, R2LoadInstrs, BinaryOpInstrs]) }.

load(mem_addr(Lval), Instrs) -->
	DataRep =^ il_data_rep,
	( { Lval = var(Var, VarType) },
		{ mangle_mlds_var(Var, MangledVarStr) },
		=(Info),
		{ is_local(MangledVarStr, Info) ->
			Instrs = instr_node(ldloca(name(MangledVarStr)))
		; is_argument(MangledVarStr, Info) ->
			Instrs = instr_node(ldarga(name(MangledVarStr)))
		; is_local_field(Var, VarType, Info, FieldRef) ->
			Instrs = instr_node(ldsfld(FieldRef))
		;
			FieldRef = make_static_fieldref(DataRep, Var, VarType),
			Instrs = instr_node(ldsfld(FieldRef))
		}
	; { Lval = field(_MaybeTag, Rval, FieldNum, FieldType, ClassType) },
		{ get_fieldref(DataRep, FieldNum, FieldType, ClassType,
			FieldRef, CastClassInstrs) },
		load(Rval, RvalLoadInstrs),
		{ Instrs = tree__list([
			RvalLoadInstrs, 
			CastClassInstrs,
			instr_node(ldflda(FieldRef))
			]) }
	; { Lval = mem_ref(_, _) },
			% XXX implement this
		{ Instrs = throw_unimplemented("load mem_addr lval mem_ref") }
	).

load(self(_), tree__list([instr_node(ldarg(index(0)))])) --> [].

:- pred store(mlds__lval, instr_tree, il_info, il_info) is det.
:- mode store(in, out, in, out) is det.

store(field(_MaybeTag, Rval, FieldNum, FieldType, ClassType), Instrs) -->
	DataRep =^ il_data_rep,
	{ get_fieldref(DataRep, FieldNum, FieldType, ClassType,
		FieldRef, CastClassInstrs) },
	load(Rval, RvalLoadInstrs),
	{ Instrs = tree__list([
		CastClassInstrs,
		RvalLoadInstrs,
		instr_node(stfld(FieldRef))]) }.

store(mem_ref(_Rval, _Type), _Instrs, Info, Info) :- 
		% you always need load the reference first, then
		% the value, then stind it.  There's no swap
		% instruction.  Annoying, eh?
	unexpected(this_file, "store into mem_ref").

store(var(Var, VarType), Instrs) --> 
	DataRep =^ il_data_rep,
	{ mangle_mlds_var(Var, MangledVarStr) },
	=(Info),
	{ is_local(MangledVarStr, Info) ->
		Instrs = instr_node(stloc(name(MangledVarStr)))
	; is_argument(MangledVarStr, Info) ->
		Instrs = instr_node(starg(name(MangledVarStr)))
	;
		FieldRef = make_static_fieldref(DataRep, Var, VarType),
		Instrs = instr_node(stsfld(FieldRef))
	}.

%-----------------------------------------------------------------------------%
%
% Convert binary and unary operations to IL
%


:- pred unaryop_to_il(mlds__unary_op, mlds__rval, instr_tree, il_info,
	il_info) is det.
:- mode unaryop_to_il(in, in, out, in, out) is det.

	% Once upon a time the MLDS code generator generated primary tag tests
	% (but we don't use primary tags).
	% If we make mktag return its operand (since it will always be
	% called with 0 as its operand), and we make tag return 0, it will
	% always succeed in the tag test (which is good, with tagbits = 0
	% we want to always succeed all primary tag tests).

unaryop_to_il(std_unop(mktag), _, comment_node("mktag (a no-op)")) --> [].
unaryop_to_il(std_unop(tag), _, Instrs) --> 
	load(const(int_const(0)), Instrs).
unaryop_to_il(std_unop(unmktag), _, comment_node("unmktag (a no-op)")) --> [].
unaryop_to_il(std_unop(strip_tag),_,comment_node("strip_tag (a no-op)")) --> [].
unaryop_to_il(std_unop(mkbody),	_, comment_node("mkbody (a no-op)")) --> [].
unaryop_to_il(std_unop(unmkbody), _, comment_node("unmkbody (a no-op)")) --> [].

unaryop_to_il(std_unop(hash_string), _, node([call(il_mercury_string_hash)]))
		--> [].
unaryop_to_il(std_unop(bitwise_complement), _, node([not])) --> [].

		% might want to revisit this and define not to be only
		% valid on 1 or 0, then we can use ldc.i4.1 and xor,
		% which might be more efficient.
unaryop_to_il(std_unop((not)), _,
	node([ldc(int32, i(1)), clt(unsigned)])) --> [].

	% XXX should detect casts to System.Array from
	% array types and ignore them, as they are not
	% necessary.
unaryop_to_il(cast(DestType), SrcRval, Instrs) -->
	DataRep =^ il_data_rep,
	{ DestILType = mlds_type_to_ilds_type(DataRep, DestType) },
	{ rval_to_type(SrcRval, SrcType) },
	{ SrcILType = mlds_type_to_ilds_type(DataRep, SrcType) },

	%
	% we need to handle casts to/from "refany" specially --
	% IL has special instructions for those
	%
	{
		% is it a cast to refany?
		DestILType = ilds__type(_, refany)
	->
		(
			% is it from refany?
			SrcILType = ilds__type(_, refany)
		->
			% cast from refany to refany is a NOP
			Instrs = empty
		;
			% cast to refany: use "mkrefany" instruction
			( SrcILType = ilds__type(_Qual, '&'(ReferencedType)) ->
				Instrs = node([mkrefany(ReferencedType)])
			;
				unexpected(this_file,
					"cast from non-ref type to refany")
			)
		)
	;
		% is it a cast from refany?
		SrcRval = lval(_),
		rval_to_type(SrcRval, SrcType),
		SrcILType = mlds_type_to_ilds_type(DataRep, SrcType),
		SrcILType = ilds__type(_, refany)
	->
		% cast from refany: use "refanyval" instruction
		( DestILType = ilds__type(_Qual, '&'(ReferencedType)) ->
			Instrs = node([refanyval(ReferencedType)])
		;
			unexpected(this_file,
				"cast from non-ref type to refany")
		)
	;
	%
	% we need to handle casts to/from unmanaged pointers specially --
	% .castclass doesn't work for those.  These casts are generated
	% by ml_elim_nested.m for the environment pointers.  If we're
	% using unmanaged pointers, then this must be unverifiable code.
	% We don't need to use any explicit conversion in the IL
	%
	% XXX Currently ilds uses `native_uint' for unmanaged pointers,
	% because that's what IL does, but we should probably define a
	% separate ilds type for this.
	%
		( DestILType = ilds__type(_, native_uint)
		; SrcILType = ilds__type(_, native_uint)
		)
	->
		Instrs = empty
	;
	%
	% if we are casting from an unboxed type to a boxed type,
	% we should box it first, and then cast.
	%
		already_boxed(DestILType)
	->
		( already_boxed(SrcILType) ->
			( SrcType = DestType ->
				Instrs = empty
			;
				% cast one boxed type to another boxed type
				Instrs = node([castclass(DestILType)])
			)
		;
			% convert an unboxed type to a boxed type:
			% box it first, then cast
			Instrs = tree__list([
				convert_to_object(SrcILType),
				instr_node(castclass(DestILType))
			])
		)
	;
		( already_boxed(SrcILType) ->
			( SrcType = mercury_type(_, user_type, _) ->
				% XXX we should look into a nicer way to
				% generate MLDS so we don't need to do this
				% XXX This looks wrong for --high-level-data. -fjh.
				Instrs = tree__list([
					comment_node(
						"loading out of an MR_Word"),
					instr_node(ldc(int32, i(0))),
					instr_node(ldelem(
						il_generic_simple_type)),
					comment_node(
						"turning a cast into an unbox"),
					convert_from_object(DestILType)
				])
			;
				% XXX It would be nicer if the MLDS used an
				% unbox to do this.
				Instrs = tree__list([
					comment_node(
					"turning a cast into an unbox"),
					convert_from_object(DestILType)
				])
			)
		;
			DestILType = ilds__type(_, DestSimpleType),
			Instrs = tree__list([
				comment_node("cast between value types"),
				instr_node(conv(DestSimpleType))
			])
		)
	}.

unaryop_to_il(box(UnboxedType), _, Instrs) -->
	DataRep =^ il_data_rep,
	{ UnboxedILType = mlds_type_to_ilds_type(DataRep, UnboxedType) },
	{ already_boxed(UnboxedILType) ->
			% It is already boxed, so we don't need
			% to do anything.
		Instrs = empty
	;
		Instrs = convert_to_object(UnboxedILType)
	}.

unaryop_to_il(unbox(UnboxedType), Rval, Instrs) -->
	DataRep =^ il_data_rep,
	{ rval_to_type(Rval, RvalType) },
	{ UnboxedILType = mlds_type_to_ilds_type(DataRep, UnboxedType) },
	{ already_boxed(UnboxedILType) ->
		( RvalType = UnboxedType ->
				% We already have the correct type
			Instrs = empty
		;
				% We have a different boxed type
			Instrs = instr_node(castclass(UnboxedILType))
		)
	;
		Instrs = convert_from_object(UnboxedILType)
	}.

:- pred already_boxed(ilds__type::in) is semidet.
already_boxed(ilds__type(_, object)).
already_boxed(ilds__type(_, string)).
already_boxed(ilds__type(_, refany)).
already_boxed(ilds__type(_, class(_))).
already_boxed(ilds__type(_, interface(_))).
already_boxed(ilds__type(_, '[]'(_, _))).
already_boxed(ilds__type(_, '&'(_))).
already_boxed(ilds__type(_, '*'(_))).

:- pred binaryop_to_il(binary_op, instr_tree, il_info,
	il_info) is det.
:- mode binaryop_to_il(in, out, in, out) is det.

binaryop_to_il((+), instr_node(I)) -->
	{ I = add(nocheckoverflow, signed) }.

binaryop_to_il((-), instr_node(I)) -->
	{ I = sub(nocheckoverflow, signed) }.

binaryop_to_il((*), instr_node(I)) -->
	{ I = mul(nocheckoverflow, signed) }.

binaryop_to_il((/), instr_node(I)) -->
	{ I = div(signed) }.

binaryop_to_il((mod), instr_node(I)) -->
	{ I = rem(signed) }.

binaryop_to_il((<<), instr_node(I)) -->
	{ I = shl }.

binaryop_to_il((>>), instr_node(I)) -->
	{ I = shr(signed) }.

binaryop_to_il((&), instr_node(I)) -->
	{ I = (and) }.

binaryop_to_il(('|'), instr_node(I)) -->
	{ I = (or) }.

binaryop_to_il(('^'), instr_node(I)) -->
	{ I = (xor) }.

binaryop_to_il((and), instr_node(I)) --> % This is logical and
	{ I = (and) }.

binaryop_to_il((or), instr_node(I)) --> % This is logical or
	{ I = (or) }.

binaryop_to_il(eq, instr_node(I)) -->
	{ I = ceq }.

binaryop_to_il(ne, node(Instrs)) --> 
	{ Instrs = [
		ceq, 
		ldc(int32, i(0)),
		ceq
	] }.

binaryop_to_il(body, _) -->
	{ unexpected(this_file, "binop: body") }.


binaryop_to_il(array_index(ElemType), instr_node(I)) -->
	DataRep =^ il_data_rep,
	{ MLDS_Type = ml_gen_array_elem_type(ElemType) },
	{ ILSimpleType = mlds_type_to_ilds_simple_type(DataRep, MLDS_Type) },
	{ I = ldelem(ILSimpleType) }.

	% String operations.
binaryop_to_il(str_eq, node([
		call(il_string_equals)
		])) --> [].
binaryop_to_il(str_ne, node([
		call(il_string_equals),
		ldc(int32, i(0)),
		ceq
		])) --> [].
binaryop_to_il(str_lt, node([
		call(il_string_compare),
		ldc(int32, i(0)),
		clt(signed)
		])) --> [].
binaryop_to_il(str_gt, node([
		call(il_string_compare),
		ldc(int32, i(0)),
		cgt(signed)
		])) --> [].
binaryop_to_il(str_le, node([
		call(il_string_compare),
		ldc(int32, i(1)), clt(signed)
		])) --> [].
binaryop_to_il(str_ge, node([
		call(il_string_compare),
		ldc(int32, i(-1)),
		cgt(signed)
		])) --> [].

	% Integer comparison
binaryop_to_il((<), node([clt(signed)])) --> [].
binaryop_to_il((>), node([cgt(signed)])) --> [].
binaryop_to_il((<=), node([cgt(signed), ldc(int32, i(0)), ceq])) --> [].
binaryop_to_il((>=), node([clt(signed), ldc(int32, i(0)), ceq])) --> [].
binaryop_to_il(unsigned_le, node([cgt(unsigned), ldc(int32, i(0)), ceq])) -->
	[].

	% Floating pointer operations.
binaryop_to_il(float_plus, instr_node(I)) -->
	{ I = add(nocheckoverflow, signed) }.
binaryop_to_il(float_minus, instr_node(I)) -->
	{ I = sub(nocheckoverflow, signed) }.
binaryop_to_il(float_times, instr_node(I)) -->
	{ I = mul(nocheckoverflow, signed) }.
binaryop_to_il(float_divide, instr_node(I)) -->
	{ I = div(signed) }.
binaryop_to_il(float_eq, instr_node(I)) -->
	{ I = ceq }.
binaryop_to_il(float_ne, node(Instrs)) --> 
	{ Instrs = [
		ceq, 
		ldc(int32, i(0)),
		ceq
	] }.
binaryop_to_il(float_lt, node([clt(signed)])) --> [].
binaryop_to_il(float_gt, node([cgt(signed)])) --> [].
binaryop_to_il(float_le, node([cgt(signed), ldc(int32, i(0)), ceq])) --> [].
binaryop_to_il(float_ge, node([clt(signed), ldc(int32, i(0)), ceq])) --> [].

%-----------------------------------------------------------------------------%
%
% Generate code for conditional statements
%
% For most conditionals, we simply load the rval and branch to the else
% case if it is false.
%
%	load rval
%	brfalse elselabel
%
% For eq and ne binops, this will generate something a bit wasteful, e.g.
%
%	load operand1
%	load operand2
%	ceq
%	brfalse elselabel
%
% We try to avoid generating a comparison result on the stack and then
% comparing it to false.  Instead we load the operands and
% branch/compare all at once.  E.g.
%
%	load operand1
%	load operand2
%	bne.unsigned elselabel
%
% Perhaps it would be better to just generate the default code and let
% the peephole optimizer pick this one up.  Since it's pretty easy
% to detect I've left it here for now.

:- pred generate_condition(rval, instr_tree, string, 
		il_info, il_info).
:- mode generate_condition(in, out, out, in, out) is det.

generate_condition(Rval, Instrs, ElseLabel) -->
	il_info_make_next_label(ElseLabel),
	( 
		{ Rval = binop(eq, Operand1, Operand2) }
	->
		load(Operand1, Op1Instr),
		load(Operand2, Op2Instr),
		{ OpInstr = instr_node(
			bne(unsigned, label_target(ElseLabel))) },
		{ Instrs = tree__list([Op1Instr, Op2Instr, OpInstr]) }
	; 
		{ Rval = binop(ne, Operand1, Operand2) }
	->
		load(Operand1, Op1Instr),
		load(Operand2, Op2Instr),
		{ OpInstr = instr_node(beq(label_target(ElseLabel))) },
		{ Instrs = tree__list([Op1Instr, Op2Instr, OpInstr]) }
	;
		load(Rval, RvalLoadInstrs),
		{ ExtraInstrs = instr_node(brfalse(label_target(ElseLabel))) },
		{ Instrs = tree__list([RvalLoadInstrs, ExtraInstrs]) }
	).

%-----------------------------------------------------------------------------%
%
% Get a function name for a code_addr_const rval.
%
% XXX This predicate should be narrowed down to the cases that actually
% make sense.


	% Convert an rval into a function we can call.
:- pred rval_to_function(rval, class_member_name).
:- mode rval_to_function(in, out) is det.
rval_to_function(Rval, MemberName) :-
	( Rval = const(Const),
		( Const = code_addr_const(CodeConst) ->
			( CodeConst = proc(ProcLabel, _Sig),
				mangle_mlds_proc_label(ProcLabel, no, 
					ClassName, ProcLabelStr),
				MemberName = class_member_name(ClassName, 
					id(ProcLabelStr))
			; CodeConst = internal(ProcLabel, SeqNum, _Sig),
				mangle_mlds_proc_label(ProcLabel, yes(SeqNum),
					ClassName, ProcLabelStr),
				MemberName = class_member_name(ClassName, 
					id(ProcLabelStr))
			)
		;
			unexpected(this_file,
				"rval_to_function: const is not a code address")
		)
	; Rval = mkword(_, _),
		unexpected(this_file, "mkword_function_name")
	; Rval = lval(_),
		unexpected(this_file, "lval_function_name")
	; Rval = unop(_, _),
		unexpected(this_file, "unop_function_name")
	; Rval = binop(_, _, _),
		unexpected(this_file, "binop_function_name")
	; Rval = mem_addr(_),
		unexpected(this_file, "mem_addr_function_name")
	; Rval = self(_),
		unexpected(this_file, "self_function_name")
	).

%-----------------------------------------------------------------------------
%
% Class constructors (.cctors) are used to fill in the RTTI information
% needed for any types defined in the module.  The RTTI is stored in
% static fields of the class.

	% .cctors can be called at practically any time by the runtime
	% system, but must be called before a static field is loaded
	% (the runtime will ensure this happens).
	% Since all the static fields in RTTI reference other RTTI static
	% fields, we could run into problems if we load a field from another
	% class before we initialize it.  Often the RTTI in one module will
	% refer to another, creating exactly this cross-referencing problem.
	% To avoid problems, we initialize them in 3 passes.
	%
	% 1. We allocate all the RTTI data structures but leave them blank.
	% When this is complete we set a flag to say we have completed this
	% pass.  After this pass is complete, it is safe for any other module
	% to reference our data structures.
	%
	% 2. We call all the .cctors for RTTI data structures that we
	% import.  We do this because we can't load fields from them until we
	% know they have been allocated.
	%
	% 3. We fill in the RTTI info in the already allocated structures.
	%
	% To ensure that pass 2 doesn't cause looping, the first thing done
	% in all .cctors is a check to see if the flag is set.  If it is, we
	% return immediately (we have already been called and our
	% initialization is either complete or at pass 2).
	%
	% 	// if (rtti_initialized) return;
	% 	ldsfld rtti_initialized
	%       brfalse done_label
	% 	ret
	% 	done_label:
	% 
	% 	// rtti_initialized = true
	% 	ldc.i4.1
	% 	stsfld rtti_initialized
	% 
	% 	// allocate RTTI data structures.
	% 	<allocation instructions generated by field initializers>
	% 
	% 	// call .cctors
	% 	call	someclass::.cctor
	% 	call	someotherclass::.cctor
	% 	... etc ...
	% 
	% 	// fill in fields of RTTI data structures
	% 	<initialization instructions generated by field initializers>
	%

:- pred make_class_constructor_class_member(fieldref, mlds__imports,
	list(instr), list(instr), class_member, il_info, il_info).
:- mode make_class_constructor_class_member(in, in, in, in,
	out, in, out) is det.
make_class_constructor_class_member(DoneFieldRef, Imports, AllocInstrs, 
		InitInstrs, Method) -->
	{ Method = method(methodhead([public, static], cctor, 
		signature(call_conv(no, default), void, []), []),
		MethodDecls) },
	test_rtti_initialization_field(DoneFieldRef, TestInstrs),
	set_rtti_initialization_field(DoneFieldRef, SetInstrs),
	{ CCtorCalls = list__filter_map(
		(func(I::in) = (C::out) is semidet :-
			I = mercury_import(ImportName),
			C = call_class_constructor(
				class_name(ImportName, wrapper_class_name))
		), Imports) },
	{ AllInstrs = list__condense([TestInstrs, AllocInstrs, SetInstrs,
		CCtorCalls, InitInstrs, [ret]]) },
	{ MethodDecls = [instrs(AllInstrs)] }.

:- pred test_rtti_initialization_field(fieldref, list(instr),
		il_info, il_info).
:- mode test_rtti_initialization_field(in, out, in, out) is det.
test_rtti_initialization_field(FieldRef, Instrs) -->
	il_info_make_next_label(DoneLabel),
	{ Instrs = [ldsfld(FieldRef), brfalse(label_target(DoneLabel)),
		ret, label(DoneLabel)] }.

:- pred set_rtti_initialization_field(fieldref, list(instr),
		il_info, il_info).
:- mode set_rtti_initialization_field(in, out, in, out) is det.
set_rtti_initialization_field(FieldRef, Instrs) -->
	{ Instrs = [ldc(int32, i(1)), stsfld(FieldRef)] }.


:- pred generate_rtti_initialization_field(ilds__class_name, 
		fieldref, class_member).
:- mode generate_rtti_initialization_field(in, out, out) is det.
generate_rtti_initialization_field(ClassName, AllocDoneFieldRef,
		AllocDoneField) :-
	AllocDoneFieldName = "rtti_initialized",
	AllocDoneField = field([public, static], ilds__type([], bool),
				AllocDoneFieldName, no, none),
	AllocDoneFieldRef = make_fieldref(ilds__type([], bool),
		ClassName, AllocDoneFieldName).



%-----------------------------------------------------------------------------
%
% Conversion of MLDS types to IL types.

:- func mlds_inherits_to_ilds_inherits(il_data_rep, list(mlds__type))
	= ilasm__extends.
mlds_inherits_to_ilds_inherits(DataRep, Inherits) = Extends :-
	( Inherits = [],
		Extends = extends_nothing
	; Inherits = [InheritType],
		Extends = extends(mlds_type_to_ilds_class_name(DataRep,
			InheritType))
	; Inherits = [_, _ | _],
		error("multiple inheritance not supported")
	).

:- pred mlds_signature_to_ilds_type_params(il_data_rep, mlds__func_signature,
	list(ilds__type)).
:- mode mlds_signature_to_ilds_type_params(in, in, out) is det.
mlds_signature_to_ilds_type_params(DataRep, 
		func_signature(Args, _Returns), Params) :-
	Params = list__map(mlds_type_to_ilds_type(DataRep), Args).

:- func mlds_arg_to_il_arg(mlds__argument) = pair(ilds__id, mlds__type).
mlds_arg_to_il_arg(mlds__argument(EntityName, Type, _GC_TraceCode)) =
		Id - Type :-
	mangle_entity_name(EntityName, Id).


:- func mlds_signature_to_ilds_type_params(il_data_rep, mlds__func_signature)
	= list(ilds__type).
mlds_signature_to_ilds_type_params(DataRep, func_signature(Args, _Returns)) = 
	list__map(mlds_type_to_ilds_type(DataRep), Args).

:- func mlds_signature_to_il_return_param(il_data_rep, mlds__func_signature)
	= ret_type.
mlds_signature_to_il_return_param(DataRep, func_signature(_, Returns))
		= Param :-
	( Returns = [] ->
		Param = void
	; Returns = [ReturnType] ->
		SimpleType = mlds_type_to_ilds_simple_type(DataRep, ReturnType),
		Param = simple_type(SimpleType)
	;
		% IL doesn't support multiple return values
		sorry(this_file, "multiple return values")
	).

params_to_il_signature(DataRep, ModuleName, FuncParams) = ILSignature :-
	ILInputTypes = list__map(input_param_to_ilds_type(DataRep, ModuleName),
		Inputs),
	FuncParams = mlds__func_params(Inputs, Outputs),
	( Outputs = [] ->
		Param = void
	; Outputs = [ReturnType] ->
		SimpleType = mlds_type_to_ilds_simple_type(DataRep, ReturnType),
		Param = simple_type(SimpleType)
	;
		% IL doesn't support multiple return values
		sorry(this_file, "multiple return values")
	),
	ILSignature = signature(call_conv(no, default), Param, ILInputTypes).

:- func input_param_to_ilds_type(il_data_rep, mlds_module_name, mlds__argument)
	= ilds__param.
input_param_to_ilds_type(DataRep, _ModuleName, Arg) = ILType - yes(Id) :-
	Arg = mlds__argument(EntityName, MldsType, _GC_TraceCode),
	mangle_entity_name(EntityName, Id),
	ILType = mlds_type_to_ilds_type(DataRep, MldsType).

:- func mlds_type_to_ilds_simple_type(il_data_rep, mlds__type) =
	 ilds__simple_type.
mlds_type_to_ilds_simple_type(DataRep, MLDSType) = SimpleType :-
	ilds__type(_, SimpleType) = mlds_type_to_ilds_type(DataRep, MLDSType).


	% XXX make sure all the types are converted correctly

mlds_type_to_ilds_type(_, mlds__rtti_type(_RttiName)) = il_object_array_type.

mlds_type_to_ilds_type(DataRep, mlds__mercury_array_type(ElementType)) = 
	( ElementType = mlds__mercury_type(_, polymorphic_type, _) ->
		il_generic_array_type
	;
		ilds__type([], '[]'(mlds_type_to_ilds_type(DataRep,
			ElementType), []))
	).

mlds_type_to_ilds_type(DataRep, mlds__array_type(ElementType)) = 
	ilds__type([], '[]'(mlds_type_to_ilds_type(DataRep, ElementType), [])).

	% This is tricky.  It could be an integer, or it could be
	% a System.Array.
mlds_type_to_ilds_type(_, mlds__pseudo_type_info_type) = il_generic_type.

	% IL has a pretty fuzzy idea about function types.
	% We treat them as integers for now
	% XXX This means the code is not verifiable.
mlds_type_to_ilds_type(_, mlds__func_type(_)) = ilds__type([], int32).

mlds_type_to_ilds_type(_, mlds__generic_type) = il_generic_type.

	% XXX Using int32 here means the code is not verifiable
	% see comments about function types above.
mlds_type_to_ilds_type(_, mlds__cont_type(_ArgTypes)) = ilds__type([], int32).

mlds_type_to_ilds_type(_, mlds__class_type(Class, Arity, Kind)) =
		ilds__type([], SimpleType) :-
	ClassName = mlds_class_name_to_ilds_class_name(Class, Arity),
	SimpleType = mlds_class_to_ilds_simple_type(Kind, ClassName).

mlds_type_to_ilds_type(_, mlds__commit_type) = il_commit_type.

mlds_type_to_ilds_type(ILDataRep, mlds__generic_env_ptr_type) =
	ILDataRep^il_envptr_type.

mlds_type_to_ilds_type(_, mlds__native_bool_type) = ilds__type([], bool).

mlds_type_to_ilds_type(_, mlds__native_char_type) = ilds__type([], char).

	% These two following choices are arbitrary -- IL has native
	% integer and float types too.  It's not clear whether there is
	% any benefit in mapping to them instead -- it all depends what
	% the indended use of mlds__native_int_type and
	% mlds__native_float_type is.
	% Any mapping other than int32 would have to be examined to see
	% whether it is going to be compatible with int32.
mlds_type_to_ilds_type(_, mlds__native_int_type) = ilds__type([], int32).

mlds_type_to_ilds_type(_, mlds__native_float_type) = ilds__type([], float64).

mlds_type_to_ilds_type(_, mlds__foreign_type(IsBoxed, ForeignType, Assembly))
	= ilds__type([], Class) :-
	sym_name_to_class_name(ForeignType, ForeignClassName),
	( IsBoxed = yes,
		Class = class(structured_name(assembly(Assembly),
				ForeignClassName, []))
	; IsBoxed = no,
		Class = valuetype(structured_name(assembly(Assembly),
				ForeignClassName, []))
	).

mlds_type_to_ilds_type(ILDataRep, mlds__ptr_type(MLDSType)) =
	ilds__type([], '&'(mlds_type_to_ilds_type(ILDataRep, MLDSType))).

mlds_type_to_ilds_type(_, mercury_type(_, int_type, _)) =
	ilds__type([], int32).
mlds_type_to_ilds_type(_, mercury_type(_, char_type, _)) =
	ilds__type([], char).
mlds_type_to_ilds_type(_, mercury_type(_, float_type, _)) =
	ilds__type([], float64).
mlds_type_to_ilds_type(_, mercury_type(_, str_type, _)) = il_string_type.
mlds_type_to_ilds_type(_, mercury_type(_, pred_type, _)) = il_object_array_type.
mlds_type_to_ilds_type(_, mercury_type(_, tuple_type, _)) =
	il_object_array_type.
mlds_type_to_ilds_type(_, mercury_type(_, enum_type, _)) = il_object_array_type.
mlds_type_to_ilds_type(_, mercury_type(_, polymorphic_type, _)) =
	il_generic_type.
mlds_type_to_ilds_type(DataRep, mercury_type(MercuryType, user_type, _)) = 
	( DataRep ^ highlevel_data = yes ->
		mercury_type_to_highlevel_class_type(MercuryType)
	;
		il_object_array_type
	).
mlds_type_to_ilds_type(_, mlds__unknown_type) = _ :-
	unexpected(this_file, "mlds_type_to_ilds_type: unknown_type").

:- func mlds_class_to_ilds_simple_type(mlds__class_kind, ilds__class_name) =
	ilds__simple_type.
mlds_class_to_ilds_simple_type(Kind, ClassName) = SimpleType :-
	( Kind = mlds__package,		SimpleType = class(ClassName)
	; Kind = mlds__class,		SimpleType = class(ClassName)
	; Kind = mlds__interface,	SimpleType = class(ClassName)
	; Kind = mlds__struct,		SimpleType = valuetype(ClassName)
	; Kind = mlds__enum,		SimpleType = valuetype(ClassName)
	).

:- func mercury_type_to_highlevel_class_type(mercury_type) = ilds__type.
mercury_type_to_highlevel_class_type(MercuryType) = ILType :-
	( type_to_ctor_and_args(MercuryType, TypeCtor, _Args) ->
		ml_gen_type_name(TypeCtor, ClassName, Arity),
		ILType = ilds__type([], class(
			mlds_class_name_to_ilds_class_name(ClassName, Arity)
			))
	;
		unexpected(this_file, "type_to_ctor_and_args failed")
	).

:- func mlds_class_name_to_ilds_class_name(mlds__class, arity) =
	ilds__class_name.

mlds_class_name_to_ilds_class_name(
		qual(MldsModuleName, MldsClassName0), Arity) = IldsClassName :-
	MldsClassName = string__format("%s_%d", [s(MldsClassName0), i(Arity)]),
	IldsClassName = append_toplevel_class_name(
		mlds_module_name_to_class_name(MldsModuleName), MldsClassName).

mlds_type_to_ilds_class_name(DataRep, MldsType) = 
	get_ilds_type_class_name(mlds_type_to_ilds_type(DataRep, MldsType)).

:- func get_ilds_type_class_name(ilds__type) = ilds__class_name.
get_ilds_type_class_name(ILType) = ClassName :-
	( 
		( ILType = ilds__type(_, class(ClassName0))
		; ILType = ilds__type(_, valuetype(ClassName0))
		)
	->
		ClassName = ClassName0
	;
		unexpected(this_file,
			"get_ilds_type_class_name: type not a class")
	).	

%-----------------------------------------------------------------------------
%
% Name mangling.


	% XXX We may need to do different name mangling for CLS compliance
	% than we would otherwise need.
	%
	% We mangle as follows:
	%	- Problem:
	%	  Two preds or funcs with different arities in Mercury
	%	  end up having the same types and arities in IL, e.g.
	%	  because one of them takes io__state arguments which
	%	  get omitted in IL.                                       
	%
	%	  To avoid this we append _<arity> to every procedure
	%	  name.
	%
	%	- Problem:
	%	  A semidet pred returns its success value, and so has
	%	  the same return type (bool) as a function.
	%
	%	  To avoid this, we mangle all semidet predicates
	%	  to indicate that they are a pred by appending _p.
	%
	%	- Problem:
	%	  A function with modes other than the default (in, in,
	%	  in = out) may clash with a predicate which has the
	%	  same types and modes.
	%
	%	  To avoid this, we mangle all functions with
	%	  non-default modes by adding _f to the procedure name.
	%
	%	- Problem:
	%	  A predicate or function with more than one mode.
	%
	%	  To avoid this, we mangle all modes > 0 by adding
	%	  _m<modenum> to the procedure name.
	%
	%	- We append the sequence number (if there is one) as
	%	  _i<seqnum>.
	%
	%	- We prepend the module name (if there is one) as
	%	  <modulename>_.
	%	
	% So the mangled name is:
	% (<modulename>_)<procname>_<arity>(_f|_p)(_m<modenum>)(_i<seqnum>)
	%
	% Where parentheses indicate optional components.
	%
	% Since each optional component (except the modulename) is after
	% the mandatory arity, and the components have unique prefixes,
	% it isn't possible to generate names that conflict with user
	% names. 
	%
	% XXX I think that it may be possible to have conflicts with
	% user names in the case where there is a <modulename>. - fjh
	%
predlabel_to_id(pred(PredOrFunc, MaybeModuleName, Name, Arity, CodeModel,
	NonOutputFunc), ProcId, MaybeSeqNum, Id) :-
		( MaybeModuleName = yes(ModuleName) ->
			mlds_to_il__sym_name_to_string(ModuleName, MStr),
			string__format("%s_", [s(MStr)], MaybeModuleStr)
		;
			MaybeModuleStr = ""
		),
		( 
			CodeModel = model_semi,
			PredOrFunc = predicate
		->
			PredOrFuncStr = "_p" 
		;
			PredOrFunc = function,
			NonOutputFunc = yes
		->
			PredOrFuncStr = "_f" 
		;
			PredOrFuncStr = ""
		),
		proc_id_to_int(ProcId, ProcIdInt),
		( ProcIdInt = 0 ->
			MaybeProcIdInt = ""
		;
			string__format("_m%d", [i(ProcIdInt)], MaybeProcIdInt)
		),
		( MaybeSeqNum = yes(SeqNum) ->
			string__format("_i%d", [i(SeqNum)], MaybeSeqNumStr)
		;
			MaybeSeqNumStr = ""
		),
		string__format("%s%s_%d%s%s%s", [
			s(MaybeModuleStr), s(Name),
			i(Arity), s(PredOrFuncStr), s(MaybeProcIdInt),
			s(MaybeSeqNumStr)], UnMangledId),
		Id = UnMangledId.
		% llds_out__name_mangle(UnMangledId, Id).

predlabel_to_id(special_pred(PredName, MaybeModuleName, TypeName, Arity),
			ProcId, MaybeSeqNum, Id) :-
		proc_id_to_int(ProcId, ProcIdInt),
		( MaybeModuleName = yes(ModuleName) ->
			mlds_to_il__sym_name_to_string(ModuleName, MStr),
			string__format("%s_", [s(MStr)], MaybeModuleStr)
		;
			MaybeModuleStr = ""
		),
		( MaybeSeqNum = yes(SeqNum) ->
			string__format("_%d", [i(SeqNum)], MaybeSeqNumStr)
		;
			MaybeSeqNumStr = ""
		),
		string__format("special_%s%s_%s_%d_%d%s", 
			[s(MaybeModuleStr), s(PredName), s(TypeName), i(Arity),
				i(ProcIdInt), s(MaybeSeqNumStr)], UnMangledId),
		Id = UnMangledId.
		% llds_out__name_mangle(UnMangledId, Id).


	% If an mlds__var is not an argument or a local, what is it?
	% We assume the given variable is a static field;
	% either a compiler-generated static,
	% or possibly a handwritten RTTI reference or a
	% reference to some hand-written code in the
	% modulename__cpp_code class.

:- func make_static_fieldref(il_data_rep, mlds__var, mlds__type)
	 = fieldref.
make_static_fieldref(DataRep, Var, VarType) = FieldRef :-
	Var = qual(ModuleName, VarName),
	mangle_mlds_var(Var, MangledVarStr),
	mangle_dataname_module(yes(var(VarName)), ModuleName, NewModuleName),
	ClassName = mlds_module_name_to_class_name(NewModuleName),
	FieldRef = make_fieldref(
		mlds_type_to_ilds_type(DataRep, VarType), ClassName,
		MangledVarStr).

:- pred mangle_foreign_code_module(mlds_module_name, foreign_language, 
	mlds_module_name).
:- mode mangle_foreign_code_module(in, in, out) is det.

mangle_foreign_code_module(ModuleName0, Lang, ModuleName) :-
	LangStr = simple_foreign_language_string(Lang),
	PackageName0 = mlds_module_name_to_package_name(ModuleName0),
	( 
		PackageName0 = qualified(Q, M0),
		M = string__format("%s__%s_code", [s(M0), s(LangStr)]),
		PackageName = qualified(Q, M)
	; 
		PackageName0 = unqualified(M0),
		M = string__format("%s__%s_code", [s(M0), s(LangStr)]),
		PackageName = unqualified(M)
	),
	SymName0 = mlds_module_name_to_sym_name(ModuleName0),
		% Check to see whether or not the name has already been
		% qualified with the wrapper class.  If not qualify it.
	( SymName0 = qualified(SymName1, wrapper_class_name) ->
		( 
			SymName1 = qualified(SQ, SM0),
			SM = string__format("%s__%s_code",
				[s(SM0), s(LangStr)]),
			SymName2 = qualified(SQ, SM)
		; 
			SymName1 = unqualified(SM0),
			SM = string__format("%s__%s_code",
					[s(SM0), s(LangStr)]),
			SymName2 = unqualified(SM)
		),
		SymName = qualified(SymName2, wrapper_class_name)
	;
		( 
			SymName0 = qualified(SQ, SM0),
			SM = string__format("%s__%s_code",
					[s(SM0), s(LangStr)]),
			SymName = qualified(qualified(SQ, SM),
					wrapper_class_name)
		; 
			SymName0 = unqualified(SM0),
			SM = string__format("%s__%s_code",
					[s(SM0), s(LangStr)]),
			SymName = qualified(unqualified(SM),
					wrapper_class_name)
		)
	),
	ModuleName = mercury_module_and_package_name_to_mlds(
			PackageName, SymName).

	% When generating references to RTTI, we need to mangle the
	% module name if the RTTI is defined in C code by hand.
	% If no data_name is provided, always do the mangling.
:- pred mangle_dataname_module(maybe(mlds__data_name), mlds_module_name,
	mlds_module_name).
:- mode mangle_dataname_module(in, in, out) is det.

mangle_dataname_module(no, ModuleName0, ModuleName) :-
	mangle_foreign_code_module(ModuleName0, managed_cplusplus, ModuleName).

mangle_dataname_module(yes(DataName), ModuleName0, ModuleName) :-
	( 
		SymName = mlds_module_name_to_sym_name(ModuleName0),
		SymName = qualified(qualified(unqualified("mercury"),
			LibModuleName0), wrapper_class_name),
		(
			DataName = rtti(RttiTypeCtor, RttiName),
			RttiTypeCtor = rtti_type_ctor(_, Name, Arity),

			% Only the type_ctor_infos for the following
			% RTTI names are defined in MC++.
			(
				RttiName = type_ctor_info
			;
				RttiName = pseudo_type_info(PseudoTypeInfo),
				PseudoTypeInfo = type_ctor_info(RttiTypeCtor)
			),
			( LibModuleName0 = "builtin",
				( 
				  Name = "int", Arity = 0 
				; Name = "string", Arity = 0
				; Name = "float", Arity = 0
				; Name = "character", Arity = 0
				; Name = "void", Arity = 0
				; Name = "c_pointer", Arity = 0
				; Name = "pred", Arity = 0
				; Name = "func", Arity = 0
				; Name = "tuple", Arity = 0
				)
			; LibModuleName0 = "array", 
				(
				  Name = "array", Arity = 1
				)
			; LibModuleName0 = "type_desc",
				( 
				  Name = "type_desc", Arity = 0
				)
			; LibModuleName0 = "private_builtin",
				( 
				  Name = "type_ctor_info", Arity = 1
				; Name = "type_info", Arity = 1
				; Name = "base_typeclass_info", Arity = 1
				; Name = "typeclass_info", Arity = 1
				)
			)		  
		;
			DataName = var(_),
			LibModuleName0 = "private_builtin"
		)
	->
		string__append(LibModuleName0, "__cpp_code",
			LibModuleName),
		ModuleName = mercury_module_name_to_mlds(
			qualified(qualified(unqualified("mercury"),
			LibModuleName), wrapper_class_name))
	;
		ModuleName = ModuleName0
	).



:- pred mangle_dataname(mlds__data_name, string).
:- mode mangle_dataname(in, out) is det.

mangle_dataname(var(MLDSVarName), Name) :-
	Name = mangle_mlds_var_name(MLDSVarName).
mangle_dataname(common(Int), MangledName) :-
	string__format("common_%s", [i(Int)], MangledName).
mangle_dataname(rtti(RttiTypeCtor, RttiName), MangledName) :-
	rtti__addr_to_string(RttiTypeCtor, RttiName, MangledName).
mangle_dataname(base_typeclass_info(ClassId, InstanceStr), MangledName) :-
        llds_out__make_base_typeclass_info_name(ClassId, InstanceStr,
		MangledName).
mangle_dataname(module_layout, _MangledName) :-
	error("unimplemented: mangling module_layout").
mangle_dataname(proc_layout(_), _MangledName) :-
	error("unimplemented: mangling proc_layout").
mangle_dataname(internal_layout(_, _), _MangledName) :-
	error("unimplemented: mangling internal_layout").
mangle_dataname(tabling_pointer(_), _MangledName) :-
	error("unimplemented: mangling tabling_pointer").

	% We turn procedures into methods of classes.
mangle_mlds_proc_label(qual(ModuleName, PredLabel - ProcId), MaybeSeqNum,
		ClassName, PredStr) :-
	ClassName = mlds_module_name_to_class_name(ModuleName),
	predlabel_to_id(PredLabel, ProcId, MaybeSeqNum, PredStr).

:- pred mangle_entity_name(mlds__entity_name, string).
:- mode mangle_entity_name(in, out) is det.
mangle_entity_name(type(_TypeName, _), _MangledName) :-
	error("can't mangle type names").
mangle_entity_name(data(DataName), MangledName) :-
	mangle_dataname(DataName, MangledName).
mangle_entity_name(function(_, _, _, _), _MangledName) :-
	error("can't mangle function names").
mangle_entity_name(export(_), _MangledName) :-
	error("can't mangle export names").

	% Any valid Mercury identifier will be fine here too.
	% We quote all identifiers before we output them, so
	% even funny characters should be fine.
mangle_mlds_var(qual(_ModuleName, VarName), Str) :-
	Str = mangle_mlds_var_name(VarName).

:- func mangle_mlds_var_name(mlds__var_name) = string.
mangle_mlds_var_name(mlds__var_name(Name, yes(Num))) = 
	string__format("%s_%d", [s(Name), i(Num)]).
mangle_mlds_var_name(mlds__var_name(Name, no)) = Name.


:- pred mlds_to_il__sym_name_to_string(sym_name, string).
:- mode mlds_to_il__sym_name_to_string(in, out) is det.
mlds_to_il__sym_name_to_string(SymName, String) :-
        mlds_to_il__sym_name_to_string(SymName, ".", String).

:- pred mlds_to_il__sym_name_to_string(sym_name, string, string).
:- mode mlds_to_il__sym_name_to_string(in, in, out) is det.
mlds_to_il__sym_name_to_string(SymName, Separator, String) :-
        mlds_to_il__sym_name_to_string_2(SymName, Separator, Parts, []),
        string__append_list(Parts, String).

:- pred mlds_to_il__sym_name_to_string_2(sym_name, string, list(string),
	 list(string)).
:- mode mlds_to_il__sym_name_to_string_2(in, in, out, in) is det.

mlds_to_il__sym_name_to_string_2(qualified(ModuleSpec,Name), Separator) -->
        mlds_to_il__sym_name_to_string_2(ModuleSpec, Separator),
        [Separator, Name].
mlds_to_il__sym_name_to_string_2(unqualified(Name), _) -->
        [Name].

	% Turn an MLDS module name into a class_name name.
:- func mlds_module_name_to_class_name(mlds_module_name) = ilds__class_name.

mlds_module_name_to_class_name(MldsModuleName) = 
		structured_name(AssemblyName, ClassName, []) :-
	SymName = mlds_module_name_to_sym_name(MldsModuleName),
	sym_name_to_class_name(SymName, ClassName),
	AssemblyName = mlds_module_name_to_assembly_name(MldsModuleName).

:- func mlds_module_name_to_assembly_name(mlds_module_name) = assembly_name.

mlds_module_name_to_assembly_name(MldsModuleName) = AssemblyName :-
	SymName = mlds_module_name_to_sym_name(MldsModuleName),
	PackageSymName = mlds_module_name_to_package_name(MldsModuleName),
	sym_name_to_class_name(SymName, ClassName),
	( 
		ClassName = ["mercury" | _]
	->
		AssemblyName = assembly("mercury")
	;
			% Foreign code currently resides in it's own
			% assembly even if it is in a sub-module.
		PackageSymName = qualified(_, Name),
		( string__remove_suffix(Name, "__csharp_code", _)
		; string__remove_suffix(Name, "__cpp_code", _)
		)
	->
		mlds_to_il__sym_name_to_string(PackageSymName, PackageString),
		AssemblyName = assembly(PackageString)
	;
		mlds_to_il__sym_name_to_string(PackageSymName, PackageString),
		( PackageSymName = unqualified(_),
			AssemblyName = assembly(PackageString)
		; PackageSymName = qualified(_, _),
			AssemblyName = module(PackageString,
					outermost_qualifier(PackageSymName))
		)
	).
	

:- pred sym_name_to_class_name(sym_name, list(ilds__id)).
:- mode sym_name_to_class_name(in, out) is det.
sym_name_to_class_name(SymName, Ids) :-
	sym_name_to_class_name_2(SymName, Ids0),
	list__reverse(Ids0, Ids).

:- pred sym_name_to_class_name_2(sym_name, list(ilds__id)).
:- mode sym_name_to_class_name_2(in, out) is det.
sym_name_to_class_name_2(qualified(ModuleSpec, Name), [Name | Modules]) :-
	sym_name_to_class_name_2(ModuleSpec, Modules).
sym_name_to_class_name_2(unqualified(Name), [Name]).



%-----------------------------------------------------------------------------%
%
% Predicates for checking various attributes of variables.
%


:- pred is_argument(ilds__id, il_info).
:- mode is_argument(in, in) is semidet.
is_argument(VarName, Info) :-
	list__member(VarName - _, Info ^ arguments).

:- pred is_local(ilds__id, il_info).
:- mode is_local(in, in) is semidet.
is_local(VarName, Info) :-
	map__contains(Info ^ locals, VarName).

:- pred is_local_field(mlds__var, mlds__type, il_info, fieldref).
:- mode is_local_field(in, in, in, out) is semidet.
is_local_field(Var, VarType, Info, FieldRef) :-
	mangle_mlds_var(Var, VarName),
	set__member(VarName, Info ^ field_names),
	Var = qual(ModuleName, _),
	ClassName = mlds_module_name_to_class_name(ModuleName),
	FieldRef = make_fieldref(
			mlds_type_to_ilds_type(Info ^ il_data_rep, VarType),
			ClassName, VarName).

%-----------------------------------------------------------------------------%
%
% Preds and funcs to find the types of rvals.
%

	% This gives us the type of an rval. 
	% This type is an MLDS type, but is with respect to the IL
	% representation (that is, we map code address and data address
	% constants to the MLDS version of their IL representation).
	% This is so you can generate appropriate box rvals for
	% rval_consts.

:- pred rval_to_type(mlds__rval::in, mlds__type::out) is det.

rval_to_type(lval(var(_, Type)), Type).
rval_to_type(lval(field(_, _, _, Type, _)), Type).
rval_to_type(lval(mem_ref(_, Type)), Type).

rval_to_type(mkword(_, _), _) :-
	unexpected(this_file, "rval_to_type: mkword").

rval_to_type(unop(Unop, _), Type) :- 
	( 
		Unop = box(_),
		Type = mlds__generic_type
	; 
		Unop = unbox(UnboxType),
		Type = UnboxType
	; 
		Unop = cast(CastType),
		Type = CastType
	; 
		Unop = std_unop(StdUnop),
		functor(StdUnop, StdUnopStr, _Arity),
		sorry(this_file, "rval_to_type: unop: " ++ StdUnopStr)
	).

rval_to_type(binop(_, _, _), _) :- 
	sorry(this_file, "rval_to_type: binop").

rval_to_type(mem_addr(_), _) :-
	sorry(this_file, "rval_to_type: mem_addr").

rval_to_type(self(Type), Type).

rval_to_type(const(Const), Type) :- 
	Type = rval_const_to_type(Const).

:- func rval_const_to_type(mlds__rval_const) = mlds__type.
rval_const_to_type(data_addr_const(_)) =
	mlds__array_type(mlds__generic_type).
rval_const_to_type(code_addr_const(_)) = mlds__func_type(
		mlds__func_params([], [])).
rval_const_to_type(int_const(_)) 
	= mercury_type(IntType, int_type, non_foreign_type(IntType)) :-
	IntType = term__functor(term__atom("int"), [], context("", 0)).
rval_const_to_type(float_const(_)) 
	= mercury_type(FloatType, float_type, non_foreign_type(FloatType)) :-
	FloatType = term__functor(term__atom("float"), [], context("", 0)).
rval_const_to_type(false) = mlds__native_bool_type.
rval_const_to_type(true) = mlds__native_bool_type.
rval_const_to_type(string_const(_)) 
	= mercury_type(StrType, str_type, non_foreign_type(StrType)) :-
	StrType = term__functor(term__atom("string"), [], context("", 0)).
rval_const_to_type(multi_string_const(_, _)) 
	= mercury_type(StrType, str_type, non_foreign_type(StrType)) :-
	StrType = term__functor(term__atom("string"), [], context("", 0)).
rval_const_to_type(null(MldsType)) = MldsType.

%-----------------------------------------------------------------------------%

:- func code_addr_constant_to_methodref(il_data_rep, mlds__code_addr) =
	methodref.

code_addr_constant_to_methodref(DataRep, proc(ProcLabel, Sig)) = MethodRef :-
	mangle_mlds_proc_label(ProcLabel, no, ClassName, ProcLabelStr),
	ReturnParam = mlds_signature_to_il_return_param(DataRep, Sig),
	TypeParams = mlds_signature_to_ilds_type_params(DataRep, Sig),
	MemberName = class_member_name(ClassName, id(ProcLabelStr)),
	MethodRef = methoddef(call_conv(no, default), ReturnParam, 
		MemberName, TypeParams).

code_addr_constant_to_methodref(DataRep, 
		internal(ProcLabel, SeqNum, Sig)) = MethodRef :-
	mangle_mlds_proc_label(ProcLabel, yes(SeqNum), ClassName, 
		ProcLabelStr),
	TypeParams = mlds_signature_to_ilds_type_params(DataRep, Sig),
	ReturnParam = mlds_signature_to_il_return_param(DataRep, Sig),
	MemberName = class_member_name(ClassName, id(ProcLabelStr)),
	MethodRef = methoddef(call_conv(no, default), ReturnParam, 
		MemberName, TypeParams).


	% Assumed to be a field of a class
:- pred data_addr_constant_to_fieldref(mlds__data_addr, fieldref).
:- mode data_addr_constant_to_fieldref(in, out) is det.

data_addr_constant_to_fieldref(data_addr(ModuleName, DataName), FieldRef) :-
	mangle_dataname(DataName, FieldName),
	mangle_dataname_module(yes(DataName), ModuleName, NewModuleName),
	ClassName = mlds_module_name_to_class_name(NewModuleName),
	FieldRef = make_fieldref(il_object_array_type, ClassName, FieldName).


%-----------------------------------------------------------------------------%

	% when we generate mercury terms using classes, we should use
	% this to reference the fields of the class.
	% note this pred will handle named or offsets.  It assumes that
	% an offset is transformed into "f<num>".
	% XXX should move towards using this code for *all* field name
	% creation and referencing
	% XXX we remove byrefs from fields here.  Perhaps we ought to do
	% this in a separate pass.   See defn_to_class_decl which does
	% the same thing when creating the fields.
:- pred get_fieldref(il_data_rep, field_id, mlds__type, mlds__type,
		fieldref, instr_tree).
:- mode get_fieldref(in, in, in, in, out, out) is det.

get_fieldref(DataRep, FieldNum, FieldType, ClassType0,
		FieldRef, CastClassInstrs) :-
	( ClassType0 = mlds__ptr_type(ClassType1) ->
		ClassType = ClassType1
	;
		ClassType = ClassType0
	),
	FieldILType0 = mlds_type_to_ilds_type(DataRep,
		FieldType),
	( FieldILType0 = ilds__type(_, '&'(FieldILType1)) ->
		FieldILType = FieldILType1
	;
		FieldILType = FieldILType0
	),
	( 
		FieldNum = offset(OffsetRval),
		ClassName = mlds_type_to_ilds_class_name(DataRep, ClassType),
		( OffsetRval = const(int_const(Num)) ->
			string__format("f%d", [i(Num)], FieldId)
		;
			sorry(this_file, 
				"offsets for non-int_const rvals")
		),
		CastClassInstrs = empty
	; 
		FieldNum = named_field(qual(ModuleName, FieldId), _CtorType),
		% The MLDS doesn't record which qualifiers are class qualifiers
		% and which are namespace qualifiers... we first generate
		% a name for the CtorClass as if it wasn't nested, and then
		% we call fixup_class_qualifiers to make it correct.
		% XXX This is a bit of a hack.  It would be nicer for the
		% MLDS to keep the information around.
		CtorClassName = mlds_module_name_to_class_name(ModuleName),
		PtrClassName = mlds_type_to_ilds_class_name(DataRep, ClassType),
		ClassName = fixup_class_qualifiers(CtorClassName, PtrClassName),
		(
			PtrClassName = CtorClassName
		->
			CastClassInstrs = empty
		;
			CastClassInstrs = instr_node(
				castclass(ilds__type([], class(ClassName))))
		)
	),
	FieldRef = make_fieldref(FieldILType, ClassName, FieldId).

	% The CtorClass will be nested inside the base class.
	% But when we initially generate the name, we don't
	% know that it is nested.  This routine fixes up the
	% CtorClassName by moving the nested parts into the
	% third field of the structured_name.
:- func fixup_class_qualifiers(ilds__class_name, ilds__class_name) =
	ilds__class_name.
fixup_class_qualifiers(CtorClassName0, PtrClassName) = CtorClassName :-
	PtrClassName = structured_name(PtrAssembly, PtrClass, PtrNested),
	CtorClassName0 = structured_name(CtorAssembly, CtorClass, CtorNested),
	(
		% some sanity checks
		PtrAssembly = CtorAssembly,
		PtrNested = [],
		CtorNested = []
	->
		% The part of the prefix which CtorClass shares with PtrClass
		% will be the outermost class name; the remainder of CtorClass,
		% if any, will be a nested class within.
		% (XXX This relies on the way that ml_type_gen.m generates
		% the nested MLDS classes for discriminated unions.)
		common_prefix(CtorClass, PtrClass, OuterClass, NestedClasses, _),
		CtorClassName = structured_name(CtorAssembly, OuterClass,
			NestedClasses)
	;
		unexpected(this_file, "fixup_class_qualifiers")
	).

	% common_prefix(List1, List2, Prefix, Tail1, Tail2):
	%	List1 = Prefix ++ Tail1,
	%	List2 = Prefix ++ Tail2.
:- pred common_prefix(list(T), list(T), list(T), list(T), list(T)).
:- mode common_prefix(in, in, out, out, out) is det.
common_prefix([],     Ys,     [],     [],     Ys).
common_prefix([X|Xs], [],     [],     [X|Xs], []).
common_prefix([X|Xs], [Y|Ys], Prefix, TailXs, TailYs) :-
	(if X = Y then
		common_prefix(Xs, Ys, Prefix1, TailXs, TailYs),
		Prefix = [X|Prefix1]
	else
		TailXs = [X|Xs],
		TailYs = [Y|Ys],
		Prefix = []
	).

%-----------------------------------------------------------------------------%

:- pred defn_to_local(mlds_module_name, mlds__defn, 
	pair(ilds__id, mlds__type)).
:- mode defn_to_local(in, in, out) is det.

defn_to_local(ModuleName, 
	mlds__defn(Name, _Context, _DeclFlags, Entity), Id - MLDSType) :-
	(
		Name = data(DataName),
		Entity = mlds__data(MLDSType0, _Initializer, _GC_TraceCode)
	->
		mangle_dataname(DataName, MangledDataName),
		mangle_mlds_var(qual(ModuleName,
			var_name(MangledDataName, no)), Id),
		MLDSType0 = MLDSType
	;
		error("definition name was not data/1")
	).

%-----------------------------------------------------------------------------%
%
% These functions are for converting to/from generic objects.
%

:- func convert_to_object(ilds__type) = instr_tree.

convert_to_object(Type) = instr_node(box(ValueType)) :-
	Type = ilds__type(_, SimpleType),
	ValueType = simple_type_to_valuetype(SimpleType).

:- func convert_from_object(ilds__type) = instr_tree.

convert_from_object(Type) = node([unbox(Type), ldobj(Type)]).

:- func simple_type_to_valuetype(simple_type) = ilds__type.
simple_type_to_valuetype(int8) = 
	ilds__type([], valuetype(il_system_name(["SByte"]))).
simple_type_to_valuetype(int16) =
	ilds__type([], valuetype(il_system_name(["Int16"]))).
simple_type_to_valuetype(int32) =
	ilds__type([], valuetype(il_system_name(["Int32"]))).
simple_type_to_valuetype(int64) =
	ilds__type([], valuetype(il_system_name(["Int64"]))).
simple_type_to_valuetype(uint8) = 
	ilds__type([], valuetype(il_system_name(["Byte"]))).
simple_type_to_valuetype(uint16) =
	ilds__type([], valuetype(il_system_name(["UInt16"]))).
simple_type_to_valuetype(uint32) =
	ilds__type([], valuetype(il_system_name(["UInt32"]))).
simple_type_to_valuetype(uint64) = 
	ilds__type([], valuetype(il_system_name(["UInt64"]))).
simple_type_to_valuetype(float32) = 
	ilds__type([], valuetype(il_system_name(["Single"]))).
simple_type_to_valuetype(float64) = 
	ilds__type([], valuetype(il_system_name(["Double"]))).
simple_type_to_valuetype(bool) = 
	ilds__type([], valuetype(il_system_name(["Boolean"]))).
simple_type_to_valuetype(char) = 
	ilds__type([], valuetype(il_system_name(["Char"]))).
simple_type_to_valuetype(object) = _ :-
	% ilds__type([], valuetype(il_system_name(["Object"]))).
	error("no value class for System.Object").
simple_type_to_valuetype(string) = _ :-
	% ilds__type([], valuetype(il_system_name(["String"]))).
	error("no value class for System.String").
simple_type_to_valuetype(refany) = _ :-
	error("no value class for refany").
simple_type_to_valuetype(class(_)) = _ :-
	error("no value class for class").
simple_type_to_valuetype(valuetype(Name)) =
	ilds__type([], valuetype(Name)).
simple_type_to_valuetype(interface(_)) = _ :-
	error("no value class for interface").
simple_type_to_valuetype('[]'(_, _)) = _ :-
	error("no value class for array").
simple_type_to_valuetype('&'( _)) = _ :-
	error("no value class for '&'").
simple_type_to_valuetype('*'(_)) = _ :-
	error("no value class for '*'").
simple_type_to_valuetype(native_float) = _ :-
	error("no value class for native float").
simple_type_to_valuetype(native_int) = _ :-
	error("no value class for native int").
simple_type_to_valuetype(native_uint) = _ :-
	error("no value class for native uint").

%-----------------------------------------------------------------------------%
%
% The mapping of the string type.
%

:- func il_string_equals = methodref.
il_string_equals = get_static_methodref(il_string_class_name, id("Equals"), 
	simple_type(bool), [il_string_type, il_string_type]).

:- func il_string_compare = methodref.
il_string_compare = get_static_methodref(il_string_class_name, id("Compare"), 
	simple_type(int32), [il_string_type, il_string_type]).

	% Note that we need to use the hash function from the Mercury
	% standard library, rather than the one from the .NET BCL
	% (Base Class Library), because it must match the one used by
	% the Mercury compiler when computing the hash tables for
	% string switches.
:- func il_mercury_string_hash = methodref.
il_mercury_string_hash = get_static_methodref(mercury_string_class_name,
	id("hash_2"), simple_type(int32), [il_string_type]).

:- func il_string_class_name = ilds__class_name.
il_string_class_name = il_system_name(["String"]).

:- func il_string_simple_type = simple_type.
il_string_simple_type = class(il_string_class_name).

:- func il_string_type = ilds__type.
il_string_type = ilds__type([], il_string_simple_type).

:- func mercury_string_class_name = ilds__class_name.
mercury_string_class_name = mercury_library_name(StringClass) :-
	sym_name_to_class_name(qualified(unqualified("string"),
			wrapper_class_name), StringClass).

%-----------------------------------------------------------------------------%
%
% The mapping of the generic type (used like MR_Box).
%

:- func il_generic_type = ilds__type.
il_generic_type = ilds__type([], il_generic_simple_type).

:- func il_generic_simple_type = simple_type.
il_generic_simple_type = class(il_generic_class_name).

il_generic_class_name = il_system_name(["Object"]).

	% Return the class name for System.ValueType.
:- func il_generic_valuetype_name = ilds__class_name.
il_generic_valuetype_name = il_system_name(["ValueType"]).

	% Return the class name for System.Enum
:- func il_generic_enum_name = ilds__class_name.
il_generic_enum_name = il_system_name(["Enum"]).

%-----------------------------------------------------------------------------%
%
% The mapping of the object array type (used like MR_Word).
%
	% il_object_array_type means array of System.Object.
:- func il_object_array_type = ilds__type.
il_object_array_type = ilds__type([], '[]'(il_generic_type, [])).

%-----------------------------------------------------------------------------%
%
% The mapping of the library array type (array(T))
%

	% il_generic_array_type means array of System.Object.
:- func il_generic_array_type = ilds__type.
il_generic_array_type = ilds__type([], class(il_system_name(["Array"]))).

%-----------------------------------------------------------------------------%
%
% The class that performs conversion operations
%

:- func il_conversion_class_name = ilds__class_name.
il_conversion_class_name = mercury_runtime_name(["Convert"]).

%-----------------------------------------------------------------------------%
%
% The mapping of the exception type.
%

:- func il_exception_type = ilds__type.
il_exception_type = ilds__type([], il_exception_simple_type).

:- func il_exception_simple_type = simple_type.
il_exception_simple_type = class(il_exception_class_name).

:- func il_exception_class_name = ilds__class_name.
il_exception_class_name = mercury_runtime_name(["Exception"]).

%-----------------------------------------------------------------------------%
%
% The mapping of the generic environment pointer type.
%

% Unfortunately the .NET CLR doesn't have any verifiable way of creating a
% generic pointer to an environment, unless you allocate them on the heap.
% Using "refany" (a.k.a. "typedref") *almost* works, except that we need
% to be able to put these pointers in environment structs, and the CLR
% doesn't allow that (see ECMA CLI Partition 1, 8.6.1.3 "Local Signatures").
% So we only do that if the --il-refany-fields option is set.
% If it is not set, then handle_options.m will ensure that we allocate
% the environments on the heap if verifiable code is requested.

% For unverifiable code we allocate environments on the stack and use
% unmanaged pointers.

:- func choose_il_envptr_type(globals) = ilds__type.
choose_il_envptr_type(Globals) = ILType :-
	globals__lookup_bool_option(Globals, put_nondet_env_on_heap, OnHeap),
	globals__lookup_bool_option(Globals, verifiable_code, Verifiable),
	( OnHeap = yes ->
		% Use an object reference type
		ILType = il_heap_envptr_type
	; Verifiable = yes ->
		% Use "refany", the generic managed pointer type
		ILType = ilds__type([], refany)
	;
		% Use unmanaged pointers
		ILType = ilds__type([], native_uint)
		% XXX we should introduce an ILDS type for unmanaged pointers,
		%     rather than using native_uint; that's what IL does, but
		%     it sucks -- we should delay the loss of type information
		%     to the last possible moment, i.e. when writing out IL.
	).

:- func il_heap_envptr_type = ilds__type.
il_heap_envptr_type = ilds__type([], il_heap_envptr_simple_type).

:- func il_heap_envptr_simple_type = simple_type.
il_heap_envptr_simple_type = class(il_heap_envptr_class_name).
 
:- func il_heap_envptr_class_name = ilds__class_name.
il_heap_envptr_class_name = mercury_runtime_name(["Environment"]).

%-----------------------------------------------------------------------------%
%
% The mapping of the commit type.
%

:- func il_commit_type = ilds__type.
il_commit_type = ilds__type([], il_commit_simple_type).

:- func il_commit_simple_type = simple_type.
il_commit_simple_type = class(il_commit_class_name).

:- func il_commit_class_name = ilds__class_name.
il_commit_class_name = mercury_runtime_name(["Commit"]).

%-----------------------------------------------------------------------------

	% qualify a name with "[mercury]mercury."
:- func mercury_library_name(ilds__namespace_qual_name) = ilds__class_name.
mercury_library_name(Name) = 
	structured_name(assembly("mercury"), ["mercury" | Name], []).

	% qualify a name with "[mercury]mercury." and add the wrapper class
	% name on the end.
:- func mercury_library_wrapper_class_name(ilds__namespace_qual_name) = 
		ilds__class_name.
mercury_library_wrapper_class_name(Name) = 
	structured_name(assembly("mercury"),
		["mercury" | Name] ++ [wrapper_class_name], []).

%-----------------------------------------------------------------------------

	% qualifiy a name with "[mercury]mercury.runtime."
:- func mercury_runtime_name(ilds__namespace_qual_name) = ilds__class_name.
mercury_runtime_name(Name) = 
	structured_name(assembly("mercury"), ["mercury", "runtime" | Name], []).

%-----------------------------------------------------------------------------

	% qualifiy a name with "[mscorlib]System."
:- func il_system_name(ilds__namespace_qual_name) = ilds__class_name.
il_system_name(Name) = structured_name(il_system_assembly_name, 
		[il_system_namespace_name | Name], []).

:- func il_system_assembly_name = assembly_name.
il_system_assembly_name = assembly("mscorlib").

:- func il_system_namespace_name = string.
il_system_namespace_name = "System".

%-----------------------------------------------------------------------------

	% Generate extern decls for any assembly we reference.
:- pred mlds_to_il__generate_extern_assembly(string::in, assembly_decl::in,
		bool::in, bool::in, mlds__imports::in, list(decl)::out) is det.

mlds_to_il__generate_extern_assembly(CurrentAssembly, Version, SignAssembly,
		SeparateAssemblies, Imports, AllDecls) :-
	Gen = (pred(Import::in, Decl::out) is semidet :-
		( Import = mercury_import(ImportName),
			( SignAssembly = yes,
				AsmDecls = mercury_strong_name_assembly_decls
			; SignAssembly = no,
				AsmDecls = []
			)
		; Import = foreign_import(ForeignImportName),
			ForeignImportName = il_assembly_name(ImportName),
			PackageName = mlds_module_name_to_package_name(
					ImportName),
			prog_out__sym_name_to_string(PackageName,
					ForeignPackageStr),
			( string__prefix(ForeignPackageStr, "System") ->
				AsmDecls = dotnet_system_assembly_decls(Version)
			;
				AsmDecls = []
			)
		),
		AsmName = mlds_module_name_to_assembly_name(ImportName),
		( AsmName = assembly(Assembly),
			Assembly \= "mercury",
			Decl = [extern_assembly(Assembly, AsmDecls)]
		; AsmName = module(ModuleName, Assembly),
			( SeparateAssemblies = no,
				( Assembly = CurrentAssembly ->
					ModuleStr = ModuleName ++ ".dll",
					Decl = [file(ModuleStr),
						extern_module(ModuleStr)]
				;
					Assembly \= "mercury",
					Decl = [extern_assembly(Assembly,
							AsmDecls)]
				)
			; SeparateAssemblies = yes,
				Decl = [extern_assembly(ModuleName, AsmDecls)]
			)
		)
	),
	list__filter_map(Gen, Imports, Decls0),
	list__sort_and_remove_dups(list__condense(Decls0), Decls),
	AllDecls = [
		extern_assembly("mercury", [
			version(0, 0, 0, 0),
			public_key_token([
				int8(0x22), int8(0x8C), int8(0x16), int8(0x7D),
				int8(0x12), int8(0xAA), int8(0x0B), int8(0x0B)
			])
		]),
		extern_assembly("mscorlib",
			dotnet_system_assembly_decls(Version)) | Decls].

:- func dotnet_system_assembly_decls(assembly_decl) = list(assembly_decl).

dotnet_system_assembly_decls(Version)
	= [
		Version,
		public_key_token([
			int8(0xb7), int8(0x7a), int8(0x5c), int8(0x56),
			int8(0x19), int8(0x34), int8(0xE0), int8(0x89)
		])
	].

:- func mercury_strong_name_assembly_decls = list(assembly_decl).

mercury_strong_name_assembly_decls
	= [
		version(0, 0, 0, 0),
		public_key_token([
			int8(0x22), int8(0x8C), int8(0x16), int8(0x7D),
			int8(0x12), int8(0xAA), int8(0x0B), int8(0x0B)
		])
	].

%-----------------------------------------------------------------------------

:- func make_method_defn(bool, bool, instr_tree) = method_defn.
make_method_defn(DebugIlAsm, VerifiableCode, InstrTree) = MethodDecls :-
	( DebugIlAsm = yes,
		Add = 1
	; DebugIlAsm = no,
		Add = 0
	),
	Instrs = list__condense(tree__flatten(InstrTree)),
	MaxStack = maxstack(int32(calculate_max_stack(Instrs) + Add)),
		% .zeroinit (which initializes all variables to zero)
		% is required for verifiable code.  But if we're generating
		% non-verifiable code, then we can skip it.  The code that
		% the Mercury compiler generates doesn't require it, and
		% omitting it may lead to slightly faster code.
	( VerifiableCode = yes ->
		MethodDecls = [MaxStack, zeroinit, instrs(Instrs)]
	;
		MethodDecls = [MaxStack, instrs(Instrs)]
	).

%-----------------------------------------------------------------------------
% Some useful functions for generating IL fragments.
		
:- func load_this = instr.
load_this = ldarg(index(0)).

:- func call_class_constructor(ilds__class_name) = instr.
call_class_constructor(CtorMemberName) = 
	call(get_static_methodref(CtorMemberName, cctor, void, [])).

:- func call_constructor(ilds__class_name) = instr.
call_constructor(CtorMemberName) = 
	call(get_constructor_methoddef(CtorMemberName, [])).

:- func throw_unimplemented(string) = instr_tree.
throw_unimplemented(String) = 
	node([
		ldstr(String),
		newobj(get_instance_methodref(il_exception_class_name,
			ctor, void, [il_string_type])),
		throw]
	).

:- func newobj_constructor(ilds__class_name, list(ilds__type)) = instr.
newobj_constructor(CtorMemberName, ArgTypes) = 
	newobj(get_constructor_methoddef(CtorMemberName, ArgTypes)).

:- func get_constructor_methoddef(ilds__class_name, list(ilds__type)) = methodref.
get_constructor_methoddef(CtorMemberName, ArgTypes) = 
	get_instance_methodref(CtorMemberName, ctor, void, ArgTypes).

:- func get_instance_methodref(ilds__class_name, member_name, ret_type,
		list(ilds__type)) = methodref.
get_instance_methodref(ClassName, MethodName, RetType, TypeParams) = 
	methoddef(call_conv(yes, default), RetType,
		class_member_name(ClassName, MethodName), TypeParams).

:- func get_static_methodref(ilds__class_name, member_name, ret_type,
		list(ilds__type)) = methodref.
get_static_methodref(ClassName, MethodName, RetType, TypeParams) = 
	methoddef(call_conv(no, default), RetType,
		class_member_name(ClassName, MethodName), TypeParams).

:- func make_constructor_class_member(method_defn) = class_member.
make_constructor_class_member(MethodDecls) = method(
	methodhead([], ctor, signature(call_conv(no, default), 
		void, []), []), MethodDecls).

:- func make_fieldref(ilds__type, ilds__class_name, ilds__id) = fieldref.
make_fieldref(ILType, ClassName, Id) = 
	fieldref(ILType, class_member_name(ClassName, id(Id))).



:- func runtime_initialization_instrs = list(instr).
runtime_initialization_instrs = [
	call(get_static_methodref(runtime_init_module_name, 
			runtime_init_method_name, void, []))
	].

:- func runtime_init_module_name = ilds__class_name.
runtime_init_module_name = 
	structured_name(assembly("mercury"),
		["mercury", "private_builtin__cpp_code", wrapper_class_name], []).

:- func runtime_init_method_name = ilds__member_name.
runtime_init_method_name = id("init_runtime").

%-----------------------------------------------------------------------------%
%
% Predicates for manipulating il_info.
%

:- func il_info_init(mlds_module_name, ilds__id, mlds__imports,
		il_data_rep, bool, bool, bool, bool) = il_info.

il_info_init(ModuleName, AssemblyName, Imports, ILDataRep,
		DebugIlAsm, VerifiableCode, ByRefTailCalls, MsCLR) =
	il_info(ModuleName, AssemblyName, Imports, set__init, ILDataRep,
		DebugIlAsm, VerifiableCode, ByRefTailCalls, MsCLR,
		empty, empty, [], no, set__init, set__init,
		map__init, empty, counter__init(1), counter__init(1), no,
		Args, MethodName, DefaultSignature) :-
	Args = [],
	DefaultSignature = signature(call_conv(no, default), void, []),
	MethodName = id("").

:- pred il_info_new_class(class_defn::in, il_info::in, il_info::out) is det.

il_info_new_class(ClassDefn) -->
	{ ClassDefn = class_defn(_, _, _, _, _, Members) },
	{ list__filter_map((pred(M::in, S::out) is semidet :-
			M = mlds__defn(Name, _, _, data(_, _, _)),
			S = entity_name_to_ilds_id(Name)
		), Members, FieldNames)
	},
	^ alloc_instrs := empty,
	^ init_instrs := empty,
	^ class_members := [],
	^ has_main := no,
	^ class_foreign_langs := set__init,
	^ field_names := set__list_to_set(FieldNames).
	
	% reset the il_info for processing a new method
:- pred il_info_new_method(arguments_map, signature, member_name, 
	il_info, il_info).
:- mode il_info_new_method(in, in, in, in, out) is det.


il_info_new_method(ILArgs, ILSignature, MethodName) -->
	=(Info),
	( yes(SomeLang) =^ method_foreign_lang ->
		^ file_foreign_langs := 
			set__insert(Info ^ file_foreign_langs, SomeLang),
		^ class_foreign_langs := 
			set__insert(Info ^ class_foreign_langs, SomeLang)
	;
		[]
	),
	^ locals := map__init,
	^ instr_tree := empty,
	^ label_counter := counter__init(1),
	^ block_counter := counter__init(1),
	^ method_foreign_lang := no,
	^ arguments := ILArgs,
	^ method_name := MethodName,
	^ signature := ILSignature.


:- pred il_info_set_arguments(assoc_list(ilds__id, mlds__type), 
	il_info, il_info).
:- mode il_info_set_arguments(in, in, out) is det.
il_info_set_arguments(Arguments, Info0, Info) :- 
	Info = Info0 ^ arguments := Arguments.

:- pred il_info_get_arguments(arguments_map, il_info, il_info).
:- mode il_info_get_arguments(out, in, out) is det.
il_info_get_arguments(Arguments, Info0, Info0) :- 
	Arguments = Info0 ^ arguments.

:- pred il_info_get_mlds_type(ilds__id, mlds__type, il_info, il_info).
:- mode il_info_get_mlds_type(in, out, in, out) is det.
il_info_get_mlds_type(Id, Type, Info0, Info0) :- 
	( 
		map__search(Info0 ^ locals, Id, Type0)
	->
		Type = Type0
	;
		assoc_list__search(Info0 ^ arguments, Id, Type0)
	->
		Type = Type0
	;
		% XXX If it isn't a local or an argument, it can only be a
		% "global variable" -- used by RTTI.  
		Type = mlds_type_for_rtti_global
	).

	% RTTI creates global variables -- these all happen to be of
	% type mlds__native_int_type.
:- func mlds_type_for_rtti_global = mlds__type.
mlds_type_for_rtti_global = native_int_type.
		
:- pred il_info_set_modulename(mlds_module_name, il_info, il_info).
:- mode il_info_set_modulename(in, in, out) is det.
il_info_set_modulename(ModuleName, Info0, Info) :- 
	Info = Info0 ^ module_name := ModuleName.

:- pred il_info_add_locals(assoc_list(ilds__id, mlds__type), il_info, il_info).
:- mode il_info_add_locals(in, in, out) is det.
il_info_add_locals(NewLocals, Info0, Info) :- 
	Info = Info0 ^ locals := 
		map__det_insert_from_assoc_list(Info0 ^ locals, NewLocals).

:- pred il_info_remove_locals(assoc_list(ilds__id, mlds__type), 
	il_info, il_info).
:- mode il_info_remove_locals(in, in, out) is det.
il_info_remove_locals(RemoveLocals, Info0, Info) :- 
	assoc_list__keys(RemoveLocals, Keys),
	map__delete_list(Info0 ^ locals, Keys, NewLocals),
	Info = Info0 ^ locals := NewLocals.

:- pred il_info_add_class_member(list(class_member), il_info, il_info).
:- mode il_info_add_class_member(in, in, out) is det.
il_info_add_class_member(ClassMembers, Info0, Info) :- 
	Info = Info0 ^ class_members := 
		list__append(ClassMembers, Info0 ^ class_members).

:- pred il_info_add_instructions(list(instr), il_info, il_info).
:- mode il_info_add_instructions(in, in, out) is det.
il_info_add_instructions(NewInstrs, Info0, Info) :- 
	Info = Info0 ^ instr_tree := tree(Info0 ^ instr_tree, node(NewInstrs)).

:- pred il_info_add_init_instructions(list(instr), il_info, il_info).
:- mode il_info_add_init_instructions(in, in, out) is det.
il_info_add_init_instructions(NewInstrs, Info0, Info) :- 
	Info = Info0 ^ init_instrs := tree(Info0 ^ init_instrs,
		node(NewInstrs)).

:- pred il_info_add_alloc_instructions(list(instr), il_info, il_info).
:- mode il_info_add_alloc_instructions(in, in, out) is det.
il_info_add_alloc_instructions(NewInstrs, Info0, Info) :- 
	Info = Info0 ^ alloc_instrs := tree(Info0 ^ alloc_instrs,
		node(NewInstrs)).

:- pred il_info_get_instructions(tree(list(instr)), il_info, il_info).
:- mode il_info_get_instructions(out, in, out) is det.
il_info_get_instructions(Instrs, Info, Info) :- 
	Instrs = Info ^ instr_tree.

:- pred il_info_get_locals_list(assoc_list(ilds__id, ilds__type), 
	il_info, il_info).
:- mode il_info_get_locals_list(out, in, out) is det.
il_info_get_locals_list(Locals, Info, Info) :- 
	DataRep = Info ^ il_data_rep,
	map__map_values((pred(_K::in, V::in, W::out) is det :- 
		W = mlds_type_to_ilds_type(DataRep, V)), 
		Info ^ locals, LocalsMap),
	map__to_assoc_list(LocalsMap, Locals).

:- pred il_info_get_module_name(mlds_module_name, il_info, il_info).
:- mode il_info_get_module_name(out, in, out) is det.
il_info_get_module_name(ModuleName, Info, Info) :- 
	ModuleName = Info ^ module_name.

:- pred il_info_get_next_block_id(blockid, il_info, il_info).
:- mode il_info_get_next_block_id(out, in, out) is det.
il_info_get_next_block_id(N, Info0, Info) :- 
	counter__allocate(N, Info0 ^ block_counter, NewCounter),
	Info = Info0 ^ block_counter := NewCounter.

:- pred il_info_get_next_label_num(int, il_info, il_info).
:- mode il_info_get_next_label_num(out, in, out) is det.
il_info_get_next_label_num(N, Info0, Info) :- 
	counter__allocate(N, Info0 ^ label_counter, NewCounter),
	Info = Info0 ^ label_counter := NewCounter.

:- pred il_info_make_next_label(ilds__label, il_info, il_info).
:- mode il_info_make_next_label(out, in, out) is det.
il_info_make_next_label(Label, Info0, Info) :- 
	il_info_get_next_label_num(LabelNnum, Info0, Info),
	string__format("l%d", [i(LabelNnum)], Label).

%-----------------------------------------------------------------------------%
%
% General utility predicates.
%

:- pred dcg_set(T::in, T::unused, T::out) is det.
dcg_set(T, _, T).

%-----------------------------------------------------------------------------%

	% Use this to make comments into trees easily.
:- func comment_node(string) = instr_tree.
comment_node(S) = node([comment(S)]).

	% Use this to make contexts into trees easily.
:- func context_node(mlds__context) = instr_tree.
context_node(Context) = node([context_instr(Context)]).

:- func context_instr(mlds__context) = instr.
context_instr(Context) = context(FileName, LineNumber) :-
	ProgContext = mlds__get_prog_context(Context),
	term__context_file(ProgContext, FileName),
	term__context_line(ProgContext, LineNumber).


	% Use this to make instructions into trees easily.
:- func instr_node(instr) = instr_tree.
instr_node(I) = node([I]).

	% Maybe fold T into U, and map it to V.  
	% U remains untouched if T is `no'.
:- pred maybe_map_fold(pred(T, V, U, U), maybe(T), V, V, U, U).
:- mode maybe_map_fold(pred(in, out, in, out) is det, in, in, out, in, out)
		 is det.

maybe_map_fold(_, no, V, V, U, U).
maybe_map_fold(P, yes(T), _, V, U0, U) :-
	P(T, V, U0, U).

%-----------------------------------------------------------------------------%

:- func this_file = string.
this_file = "mlds_to_il.m".

:- end_module mlds_to_il.

