%-----------------------------------------------------------------------------%
% Copyright (C) 2000-2001 The University of Melbourne.
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
% [ ] solutions
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
% [ ] We generate too many castclasses, it would be good to check if we
%     really to do it before generating it.  Same with isinst.
% [ ] Implement pragma export.
% [ ] Fix issues with abstract types so that we can implement C
%     pointers as MR_Box rather than MR_Word.
% [ ] When generating target_code, sometimes we output more calls than
%     we should (this can occur in nondet C code). 
% [ ] ml_gen_call_current_success_cont_indirectly should be merged with
% 	similar code for doing copy-in/copy-out.
% [ ] Try to use the IL bool type for the true/false rvals.
% [ ] Add an option to do overflow checking.
% [ ] Should replace hard-coded of int32 with a more abstract name such
%     as `mercury_int_il_type'.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module mlds_to_il.
:- interface.

:- import_module mlds, ilasm, ilds.
:- import_module io, list, bool, std_util, set.
:- import_module hlds_pred. % for `pred_proc_id'.
:- import_module prog_data. % for `foreign_language'.

%-----------------------------------------------------------------------------%

	%
	% Generate IL assembly from MLDS.
	%
	% This is where all the action is for the IL backend.
	%
:- pred generate_il(mlds, list(ilasm:decl), set(foreign_language),
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

:- type il_data_rep ---> il_data_rep(
	highlevel_data	:: bool		% do we use high level data?
	).

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

:- import_module globals, options, passes_aux.
:- import_module builtin_ops, c_util, modules, tree.
:- import_module prog_data, prog_out, llds_out.
:- import_module rtti, type_util, code_model.

:- import_module ilasm, il_peephole.
:- import_module ml_util, ml_code_util, error_util.
:- import_module ml_type_gen.
:- use_module llds. /* for user_foreign_code */

:- import_module bool, int, map, string, set, list, assoc_list, term.
:- import_module library, require, counter.

	% We build up lists of instructions using a tree to make
	% insertion easy.
:- type instr_tree == tree(list(instr)).

	% The state of the il code generator.
:- type il_info ---> il_info(
		% file-wide attributes (all static)
	module_name 	:: mlds_module_name,	% the module name
	assembly_name 	:: assembly_name,	% the assembly name
	imports 	:: mlds__imports,	% the imports
	file_foreign_langs :: set(foreign_language), % file foreign code
	il_data_rep	:: il_data_rep,		% data representation.
	debug_il_asm	:: bool,		% --debug-il-asm
		% class-wide attributes (all accumulate)
	alloc_instrs	:: instr_tree,		% .cctor allocation instructions
	init_instrs	:: instr_tree,		% .cctor init instructions
	classdecls	:: list(classdecl),	% class methods and fields 
	has_main	:: bool,		% class contains main
	class_foreign_langs :: set(foreign_language),% class foreign code
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

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

generate_il(MLDS, AssemblyDecls ++ [assembly(IlInfo ^ assembly_name),
		namespace(NamespaceName, ILDecls)], set__init, IO0, IO) :-
	mlds(MercuryModuleName, _, Imports, Defns)= transform_mlds(MLDS),

	ModuleName = mercury_module_name_to_mlds(MercuryModuleName),
	prog_out__sym_name_to_string(mlds_module_name_to_sym_name(ModuleName),
			".", AssemblyName),
	globals__io_lookup_bool_option(highlevel_data, HighLevelData, IO0, IO1),
	globals__io_lookup_bool_option(debug_il_asm, DebugIlAsm, IO1, IO),

	IlInfo = il_info_init(ModuleName, AssemblyName, Imports,
			il_data_rep(HighLevelData), DebugIlAsm),

	ILDecls = list__map(mlds_defn_to_ilasm_decl(IlInfo), Defns),

	ClassName = mlds_module_name_to_class_name(ModuleName),
	ClassName = structured_name(_, NamespaceName),

	generate_extern_assembly(Imports, AssemblyDecls).


%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Move all the top level methods and data definitions into the
	% mercury_code class, and then rename all the definitions as
	% necessary to reflect this new hierachy.
:- func transform_mlds(mlds) = mlds.

transform_mlds(mlds(MercuryModuleName, ForeignCode, Imports, Defns))
	= mlds(
		MercuryModuleName,
		ForeignCode,
		Imports,
		[mercury_code_class(list__map(rename_defn, Members)) | Others]
	) :-
	list__filter((pred(D::in) is semidet :-
			( D = mlds__defn(_, _, _, mlds__function(_, _, _))
				% XXX we need to place all the RTTI
				% datastructures inside this class, so
				% they are generated as fields.
				% Maybe what we should do is make all
				% the RTTI data structures nested
				% classes.  I think that would work
				% better.
			; D = mlds__defn(_, _, _, mlds__data(_, _))
			)
		), Defns, Members, Others).

:- func mercury_code_class(mlds__defns) = mlds__defn.

mercury_code_class(Members)
	= mlds__defn(
		export("mercury_code"),
		mlds__make_context(term__context_init),
		init_decl_flags(public, per_instance, non_virtual,
				final, const, concrete),
		mlds__class(
			mlds__class_defn(mlds__package, [], [], [], [], Members)
		)
	).

:- func rename_defn(mlds__defn) = mlds__defn.

rename_defn(defn(Name, Context, Flags, Entity0))
	= defn(Name, Context, Flags, Entity) :-
	( Entity0 = data(Type, Initializer),
		Entity = data(Type, rename_initializer(Initializer))
	; Entity0 = function(MaybePredProcId, Params, MaybeStmt0),
		( MaybeStmt0 = yes(Stmt),
			MaybeStmt = yes(rename_statement(Stmt))
		; MaybeStmt0 = no,
			MaybeStmt = no
		),
		Entity = function(MaybePredProcId, Params, MaybeStmt)
	; Entity0 = class(_),
		sorry(this_file, "renaming nested classes")
	).

:- func rename_statement(mlds__statement) = mlds__statement.

rename_statement(statement(block(Defns, Stmts), Context))
	= statement(block(list__map(rename_defn, Defns),
			list__map(rename_statement, Stmts)), Context).
rename_statement(statement(while(Rval, Loop, IterateOnce), Context))
	= statement(while(rename_rval(Rval),
			rename_statement(Loop), IterateOnce), Context).
rename_statement(statement(if_then_else(Rval, Then, MaybeElse0), Context))
	= statement(if_then_else(rename_rval(Rval),
			rename_statement(Then), MaybeElse), Context) :-
	( MaybeElse0 = no,
		MaybeElse = no
	; MaybeElse0 = yes(Else),
		MaybeElse = yes(rename_statement(Else))
	).
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
rename_atomic(new_object(L, T, Type, MaybeSize, C, Args, Types))
	= new_object(rename_lval(L), T, Type, MaybeSize,
			C, list__map(rename_rval, Args), Types).
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
	= proc(rename_label(Label), Signature).
rename_code_addr(internal(Label, Seq, Signature))
	= internal(rename_label(Label), Seq, Signature).

:- func rename_data_addr(data_addr) = data_addr.

rename_data_addr(data_addr(ModuleName, Name))
	= data_addr(append_mercury_code(ModuleName), Name).

:- func rename_label(mlds__qualified_proc_label) = mlds__qualified_proc_label.

rename_label(qual(Module, Name)) = qual(append_mercury_code(Module), Name).

:- func rename_lval(mlds__lval) = mlds__lval.

rename_lval(field(Tag, Address, FieldName, FieldType, PtrType))
	= field(Tag, rename_rval(Address),
			rename_field_id(FieldName), FieldType, PtrType).
rename_lval(mem_ref(Rval, Type)) = mem_ref(rename_rval(Rval), Type).
rename_lval(var(Var, Type)) = var(rename_var(Var, Type), Type).

:- func rename_field_id(field_id) = field_id.

rename_field_id(offset(Rval)) = offset(rename_rval(Rval)).
rename_field_id(named_field(Name, Type)) = named_field(Name, Type).

:- func rename_var(mlds__var, mlds__type) = mlds__var.

rename_var(qual(ModuleName, Name), _Type)
	= qual(append_mercury_code(ModuleName), Name).

:- func rename_initializer(mlds__initializer) = mlds__initializer.

rename_initializer(init_obj(Rval)) = init_obj(rename_rval(Rval)).
rename_initializer(init_struct(Inits))
	= init_struct(list__map(rename_initializer, Inits)).
rename_initializer(init_array(Inits))
	= init_array(list__map(rename_initializer, Inits)).
rename_initializer(no_initializer) = no_initializer.


%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- func mlds_defn_to_ilasm_decl(il_info, mlds__defn) = ilasm__decl.

mlds_defn_to_ilasm_decl(_, defn(_Name, _Context, _Flags, data(_Type, _Init)))
	= _ :- sorry(this_file, "top level data definition!").
mlds_defn_to_ilasm_decl(_, defn(_Name, _Context, _Flags,
		function(_MaybePredProcId, _Params, _MaybeStmts)))
	= _ :- sorry(this_file, "top level function definition!").
mlds_defn_to_ilasm_decl(Info0, defn(Name, _Context, Flags, class(ClassDefn)))
	= class(
		decl_flags_to_classattrs(Flags),
		EntityName,
		Extends,
		Interfaces,
		MethodDecls
	) :-
	EntityName = entity_name_to_ilds_id(Name),
	ClassDefn = class_defn(_Kind, _Imports, Inherits, Implements,
			Ctors, Members),
	( Inherits = [],
		Extends = extends_nothing,
		Parent = il_generic_class_name
	; Inherits = [Parent0 | Rest],
		( Rest = [] ->
			Parent = mlds_type_to_ilds_class_name(
					Info0 ^ il_data_rep, Parent0),
			Extends = extends(Parent)
		;
			error(this_file ++ 
				": multiple inheritance not supported.")
		)
	),

	Interfaces = implements(
			list__map(interface_id_to_class_name, Implements)),

	ClassName = class_name(Info0 ^ module_name, EntityName),
	list__map_foldl(generate_method(ClassName, no), Members,
			MethodsAndFields, Info0, Info1),
	list__map_foldl(generate_method(ClassName, yes(Parent)), Ctors,
			IlCtors, Info1, Info),
	MethodsAndFieldsAndCtors = IlCtors ++ MethodsAndFields,

		% XXX Maybe it would be better to just check to see
		% whether or not there are any init instructions than
		% explicitly checking for the name mercury_code.
	( EntityName = "mercury_code" ->
		Imports = Info ^ imports,
		InitInstrs = list__condense(tree__flatten(Info ^ init_instrs)),
		AllocInstrs = list__condense(
				tree__flatten(Info ^ alloc_instrs)),

			% Generate a field that records whether we have
			% finished RTTI initialization.
		generate_rtti_initialization_field(ClassName, 
				AllocDoneFieldRef, AllocDoneField),

			% Generate a class constructor.
		make_class_constructor_classdecl(AllocDoneFieldRef,
				Imports, AllocInstrs, InitInstrs, CCtor,
				Info, _InfoX),

			% The declarations in this class.
		MethodDecls = [AllocDoneField, CCtor | MethodsAndFieldsAndCtors]
	;
		MethodDecls = MethodsAndFieldsAndCtors
	).

class_name(Module, Name) = structured_name(Assembly, ClassName ++ [Name]) :-
	ClassName = sym_name_to_list(mlds_module_name_to_sym_name(Module)),
	( ClassName = ["mercury" | _] ->
		Assembly = "mercury"
	;
		prog_out__sym_name_to_string(
				mlds_module_name_to_package_name(Module),
				".", Assembly)
	).

:- func sym_name_to_list(sym_name) = list(string).

sym_name_to_list(unqualified(Name)) = [Name].
sym_name_to_list(qualified(Module, Name))
	= sym_name_to_list(Module) ++ [Name].


:- func decl_flags_to_classattrs(mlds__decl_flags) = list(ilasm__classattr).

decl_flags_to_classattrs(Flags)
	= list__condense([Access, Finality, Abstractness]) :-
	AccessFlag = access(Flags),
	( AccessFlag = public,
		Access = [public]
	; AccessFlag = protected,
		Access = []
	; AccessFlag = private,
		Access = []
	; AccessFlag = default,
		error("decl_flags_to_classattrs: default access flag")
	; AccessFlag = local,
		error("decl_flags_to_classattrs: local access flag")
	),
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
		error("decl_flags_to_methattrs: default access flag")
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
		error("decl_flags_to_fieldattrs: default access flag")
	; AccessFlag = local,
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
		Result = structured_name("XXX", [])
		
	).

%-----------------------------------------------------------------------------%

:- pred generate_method(ilds__class_name::in, maybe(ilds__class_name)::in,
		mlds__defn::in, classdecl::out,
		il_info::in, il_info::out) is det.

generate_method(ClassName, _, defn(Name, Context, Flags, Entity), ClassDecl) -->
	{ Entity = data(Type, DataInitializer) },

	{ FieldName = entity_name_to_ilds_id(Name) },

	{ Attrs = decl_flags_to_fieldattrs(Flags) },

		% Generate instructions to initialize this data.
		% There are two sorts of instructions,
		% instructions to allocate the data structure,
		% and instructions to initialize it.
		% See the comments about class constructors to
		% find out why we do this.
	data_initializer_to_instrs(DataInitializer, AllocInstrsTree,
			InitInstrTree),

		% Make a field reference for the field
	{ FieldRef = make_fieldref(il_array_type, ClassName, FieldName) },

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

	DataRep =^ il_data_rep,
	{ IlType = mlds_type_to_ilds_type(DataRep, Type) },
	{ MaybeOffset = no },
	{ Initializer = none },

	{ ClassDecl = field(Attrs, IlType, FieldName,
			MaybeOffset, Initializer) }.

generate_method(_, IsCons, defn(Name, Context, Flags, Entity), ClassDecl) -->
	{ Entity = function(_MaybePredProcId, Params, MaybeStatement) },

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
	( { MaybeStatement = yes(Statement) } -> 
		statement_to_il(Statement, InstrsTree1)
	;
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

		% If this is main, add the entrypoint, set a flag, and
		% call the initialization instructions in the cctor of
		% this module.
	(
		{ Name = function(PredLabel, _ProcId, MaybeSeqNum, _PredId) },
		{ PredLabel = pred(predicate, no, "main", 2, model_det, no) },
		{ MaybeSeqNum = no }
	->
		{ EntryPoint = [entrypoint] },
		il_info_add_init_instructions(runtime_initialization_instrs),
		^ has_main := yes
	;
		{ EntryPoint = [] }
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
	{ InstrsTree = tree__list([
		context_node(Context),
		node(CtorInstrs),
		context_node(Context),
		instr_node(start_block(scope(Locals), BlockId)),
		InstrsTree1, 
		MaybeRet,
		instr_node(end_block(scope(Locals), BlockId))
		])
	},

		% Generate the entire method contents.
	DebugIlAsm =^ debug_il_asm,
	{ MethodBody = make_method_defn(DebugIlAsm, InstrsTree) },
	{ list__append(EntryPoint, MethodBody, MethodContents) },

	{ ClassDecl = ilasm__method(methodhead(Attrs, MemberName,
			ILSignature, []), MethodContents)}.

generate_method(_, _, defn(_Name, _Context, _Flags, Entity), _ClassDecl) -->
	{ Entity = class(_ClassDefn) },
	{ sorry(this_file, "nested classes") }.

%-----------------------------------------------------------------------------%

:- func mangle_dataname(mlds__data_name) = string.

mangle_dataname(var(MLDSVarName))
	= mangle_mlds_var_name(MLDSVarName).
mangle_dataname(common(Int))
	= string__format("common_%s", [i(Int)]).
mangle_dataname(rtti(RttiTypeId, RttiName)) = MangledName :-
	rtti__addr_to_string(RttiTypeId, RttiName, MangledName).
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
		{ Entity = mlds__data(MLDSType, Initializer) }
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
			data_initializer_to_instrs(Initializer, AllocInstrs,
				InitInstrs),
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
:- pred data_initializer_to_instrs(mlds__initializer::in,
	instr_tree::out, instr_tree::out, il_info::in, il_info::out) is det.
data_initializer_to_instrs(init_obj(Rval), node([]), InitInstrs) --> 
	load(Rval, InitInstrs).

	% Currently, structs are the same as arrays.
data_initializer_to_instrs(init_struct(InitList), AllocInstrs, InitInstrs) --> 
	data_initializer_to_instrs(init_array(InitList), AllocInstrs, 
		InitInstrs).

	% Put the array allocation in AllocInstrs.
	% For sub-initializations, we don't worry about keeping AllocInstrs
	% and InitInstrs apart, since we are only interested in top level
	% allocations.
data_initializer_to_instrs(init_array(InitList), AllocInstrs, InitInstrs) -->

		% To initialize an array, we generate the following
		% code:
		% 	ldc <length of array>
		% 	newarr System::Object
		%	
		% Then, for each element in the array:
		%	dup
		%	ldc <index of this element in the array>
		%	... allocation instructions ...
		%	... initialization instructions ...
		%	box the value (if necessary)
		%	stelem System::Object
		%
		% The initialization will leave the array on the stack.
		%	
	{ AllocInstrs = node([ldc(int32, i(list__length(InitList))), 
		newarr(il_generic_type)]) },
	{ AddInitializer = 
		(pred(Init0::in, X0 - Tree0::in, (X0 + 1) - Tree::out,
				in, out) is det -->
			maybe_box_initializer(Init0, Init),
			data_initializer_to_instrs(Init, ATree1, ITree1),
			{ Tree = tree(tree(Tree0, node(
					[dup, ldc(int32, i(X0))])), 
				tree(tree(ATree1, ITree1), 
					node([stelem(il_generic_simple_type)]
				))) }
		) },
	list__foldl2(AddInitializer, InitList, 0 - empty, _ - InitInstrs).
data_initializer_to_instrs(no_initializer, node([]), node([])) --> [].

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
	rval_to_type(Rval, BoxType),
	{ NewRval = unop(box(BoxType), Rval) }.


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

statement_to_il(statement(block(Defns, Statements), Context), Instrs) -->
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
	{ Instrs = tree__list([
			context_node(Context),
			instr_node(start_block(scope(ILLocals), BlockId)),
			InitInstrsTree,
			comment_node("block body"),
			BlockInstrs,
			node([end_block(scope(ILLocals), BlockId)])
			]) },
	il_info_remove_locals(Locals).

statement_to_il(statement(atomic(Atomic), Context), Instrs) -->
	atomic_statement_to_il(Atomic, AtomicInstrs),
	{ Instrs = tree(context_node(Context), AtomicInstrs) }.

statement_to_il(statement(call(Sig, Function, _This, Args, Returns, IsTail), 
		Context), Instrs) -->
	( { IsTail = tail_call } ->
		% For tail calls, to make the code verifiable, 
		% we need a `ret' instruction immediately after
		% the call.
		{ TailCallInstrs = [tailcall] },
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
	DataRep =^ il_data_rep,
	{ TypeParams = mlds_signature_to_ilds_type_params(DataRep, Sig) },
	{ ReturnParam = mlds_signature_to_il_return_param(DataRep, Sig) },
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



statement_to_il(statement(goto(Label), Context), Instrs) -->
	{ string__format("goto %s", [s(Label)], Comment) },
	{ Instrs = node([
			comment(Comment),
			context_instr(Context),
			br(label_target(Label))
		]) }.

statement_to_il(statement(do_commit(Ref), Context), Instrs) -->

	% For commits, we use exception handling.
	%
	% We generate code of the following form:
	% 
	% 	<load exception rval -- should be of a special commit type>
	% 	throw
	%
	% 

	load(Ref, RefLoadInstrs),
	{ Instrs = tree__list([
			context_node(Context),
			comment_node("do_commit/1"),
			RefLoadInstrs,
			instr_node(throw)
		]) }.

statement_to_il(statement(try_commit(Ref, GoalToTry, CommitHandlerGoal), 
		Context), Instrs) -->

	% For commits, we use exception handling.
	%
	% We generate code of the following form:
	%
	% 	.try {	
	%		GoalToTry
	%		leave label1
	% 	} catch commit_type {
	%		pop	// discard the exception object
	% 		CommitHandlerGoal
	%		leave label1
	% 	}
	% 	label1:
	% 

	il_info_get_next_block_id(TryBlockId),
	statement_to_il(GoalToTry, GoalInstrsTree),
	il_info_get_next_block_id(CatchBlockId),
	statement_to_il(CommitHandlerGoal, HandlerInstrsTree),
	il_info_make_next_label(DoneLabel),

	rval_to_type(lval(Ref), MLDSRefType),
	DataRep =^ il_data_rep,
	{ ClassName = mlds_type_to_ilds_class_name(DataRep, MLDSRefType) },
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
		^ method_foreign_lang := yes(Lang),
		{ mangle_foreign_code_module(ModuleName, Lang,
			OutlineLangModuleName) },
		{ ClassName = mlds_module_name_to_class_name(
			OutlineLangModuleName) },
		signature(_, RetType, Params) =^ signature, 

		( { ReturnLvals = [] } ->
			% If there is a return type, but no return value, it
			% must be a semidet predicate so put it in succeeded.
			% XXX it would be better to get the code generator
			% to tell us this is the case directly
			{ LoadInstrs = empty },
			{ RetType = void ->
				StoreInstrs = empty
			;
				StoreInstrs = instr_node(
					stloc(name("succeeded")))
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

atomic_statement_to_il(inline_target_code(_Lang, _Code), node(Instrs)) --> 
	il_info_get_module_name(ModuleName),
	( no =^ method_foreign_lang  ->
			% XXX we hardcode managed C++ here
		^ method_foreign_lang := yes(managed_cplusplus),
		{ mangle_dataname_module(no, ModuleName, NewModuleName) },
		{ ClassName = mlds_module_name_to_class_name(NewModuleName) },
		signature(_, RetType, Params) =^ signature, 
			% If there is a return value, put it in succeeded.
			% XXX this is incorrect for functions, which might
			% return a useful value.
		{ RetType = void ->
			StoreReturnInstr = []
		;
			StoreReturnInstr = [stloc(name("succeeded"))]
		},
		MethodName =^ method_name,
		{ assoc_list__keys(Params, TypeParams) },
		{ list__map_foldl((pred(_::in, Instr::out,
			Num::in, Num + 1::out) is det :-
				Instr = ldarg(index(Num))),
			TypeParams, LoadInstrs, 0, _) },
		{ list__condense(
			[[comment("inline target code -- call handwritten version")],
			LoadInstrs,
			[call(get_static_methodref(ClassName, MethodName, 
				RetType, TypeParams))],
			StoreReturnInstr	
			], Instrs) }
	;
		{ Instrs = [comment("inline target code -- already called")] }
	).


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

atomic_statement_to_il(new_object(Target, _MaybeTag, Type, Size, _CtorName,
		Args, ArgTypes), Instrs) -->
	DataRep =^ il_data_rep,
	( 
		{ 
			Type = mlds__generic_env_ptr_type 
		; 
			Type = mlds__class_type(_, _, _) 
		;
			Type = mlds__commit_type
		; 
			DataRep ^ highlevel_data = yes,
			Type = mlds__mercury_type(_, user_type)
		}
	->
			% If this is an env_ptr we should call the
			% constructor.  
			% (This is also how we will handle high-level data).
			% We generate code of the form:
			%
			% 	... load memory reference ...
			%	// new object (call constructor)
			%	... load each argument ...
			%	call ClassName::.ctor
			%	... store to memory reference ...
			%
		{ ClassName = mlds_type_to_ilds_class_name(DataRep, Type) },
		list__map_foldl(load, Args, ArgsLoadInstrsTrees),
		{ ArgsLoadInstrs = tree__list(ArgsLoadInstrsTrees) },
		get_load_store_lval_instrs(Target, LoadMemRefInstrs,
			StoreLvalInstrs),
		{ CallCtor = newobj_constructor(ClassName) },
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
			%	... load and box rval ...
			%	stelem System::Object
			%
			% Finally, after all the array elements have
			% been set:
			%
			%	... store to memory reference ...
			
			% We need to do the boxing ourselves because
			% MLDS hasn't done it.  We add boxing unops to
			% the rvals.
		{ Box = (pred(A - T::in, B::out) is det :- 
			B = unop(box(T), A)   
		) },
		{ assoc_list__from_corresponding_lists(Args, ArgTypes,
			ArgsAndTypes) },
		{ list__map(Box, ArgsAndTypes, BoxedArgs) },
	
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
		{ list__map_foldl(LoadInArray, BoxedArgs, ArgsLoadInstrsTrees,
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
		{ FieldRef = get_fieldref(DataRep, FieldNum, FieldType,
			ClassType) },
		load(FieldRval, LoadMemRefInstrs),
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
		;
			FieldRef = make_fieldref_for_handdefined_var(DataRep,
				Var, VarType),
			Instrs = instr_node(ldsfld(FieldRef))
		}
	; { Lval = field(_MaybeTag, Rval, FieldNum, FieldType, ClassType) },
		load(Rval, RvalLoadInstrs),
		( { FieldNum = offset(OffSet) } ->
			{ SimpleFieldType = mlds_type_to_ilds_simple_type(
				DataRep, FieldType) },
			load(OffSet, OffSetLoadInstrs),
			{ LoadInstruction = ldelem(SimpleFieldType) }
		;
			{ FieldRef = get_fieldref(DataRep, FieldNum, FieldType,
				ClassType) },
			{ LoadInstruction = ldfld(FieldRef) },
			{ OffSetLoadInstrs = empty }
		),
		{ Instrs = tree__list([
				RvalLoadInstrs, 
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
		% XXX is there a better way to handle true and false
		% using IL's bool type?
	{ Const = true,
		Instrs = instr_node(ldc(int32, i(1)))
	; Const = false,
		Instrs = instr_node(ldc(int32, i(0)))
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
		;
			FieldRef = make_fieldref_for_handdefined_var(DataRep,
				Var, VarType),
			Instrs = instr_node(ldsfld(FieldRef))
		}
	; { Lval = field(_MaybeTag, Rval, FieldNum, FieldType, ClassType) },
		{ FieldRef = get_fieldref(DataRep, FieldNum, FieldType,
			ClassType) },
		load(Rval, RvalLoadInstrs),
		{ Instrs = tree__list([
			RvalLoadInstrs, 
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
	{ FieldRef = get_fieldref(DataRep, FieldNum, FieldType, ClassType) },
	load(Rval, RvalLoadInstrs),
	{ Instrs = tree__list([RvalLoadInstrs, instr_node(stfld(FieldRef))]) }.

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
		FieldRef = make_fieldref_for_handdefined_var(DataRep, Var,
			VarType),
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

		% if we are casting from an unboxed type, we should box
		% it first.
		% XXX should also test the cast-to type, to handle the
		% cases where it is unboxed.
unaryop_to_il(cast(Type), Rval, Instrs) -->
	DataRep =^ il_data_rep,
	{ ILType = mlds_type_to_ilds_type(DataRep, Type) },
	{ 
		Rval = const(Const),
		RvalType = rval_const_to_type(Const),
		RvalILType = mlds_type_to_ilds_type(DataRep, RvalType),
		not already_boxed(RvalILType)
	->
		Instrs = node([call(convert_to_object(RvalILType)),
			castclass(ILType)])
	;
		Instrs = node([castclass(ILType)])
	}.


	% XXX boxing and unboxing should be fixed.
	% currently for boxing and unboxing we call some conversion
	% methods that written by hand. 
	% We should do a small MLDS->MLDS transformation to introduce
	% locals so we can box the address of the locals.
	% then unboxing should just be castclass(System.Int32 or whatever),
	% then unbox.
unaryop_to_il(box(Type), _, Instrs) -->
	DataRep =^ il_data_rep,
	{ ILType = mlds_type_to_ilds_type(DataRep, Type) },
	{ already_boxed(ILType) ->
		Instrs = node([isinst(il_generic_type)])
	;
		Instrs = node([call(convert_to_object(ILType))])
		% XXX can't just use box, because it requires a pointer to
		% the object, so it's useless for anything that isn't
		% addressable
		% Instrs = [box(ILType)]  
	}.

unaryop_to_il(unbox(Type), _, Instrs) -->
	DataRep =^ il_data_rep,
	{ ILType = mlds_type_to_ilds_type(DataRep, Type) },
	{ ILType = ilds__type(_, class(_)) ->
		Instrs = node([castclass(ILType)])
	;
		Instrs = node([call(convert_from_object(ILType))])
		% since we can't use box, we can't use unbox
		% Instrs = [unbox(ILType)]
	}.

:- pred already_boxed(ilds__type::in) is semidet.
already_boxed(ilds__type(_, class(_))).
already_boxed(ilds__type(_, '[]'(_, _))).

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

:- pred make_class_constructor_classdecl(fieldref, mlds__imports,
	list(instr), list(instr), classdecl, il_info, il_info).
:- mode make_class_constructor_classdecl(in, in, in, in, out, in, out) is det.
make_class_constructor_classdecl(DoneFieldRef, Imports, AllocInstrs, 
		InitInstrs, Method) -->
	{ Method = method(methodhead([public, static], cctor, 
		signature(call_conv(no, default), void, []), []),
		MethodDecls) },
	test_rtti_initialization_field(DoneFieldRef, TestInstrs),
	set_rtti_initialization_field(DoneFieldRef, SetInstrs),
	{ CCtorCalls = list__map((func(X) = call_class_constructor(
		class_name(X, "mercury_code"))), Imports) },
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
		fieldref, classdecl).
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

:- func mlds_arg_to_il_arg(pair(mlds__entity_name, mlds__type)) = 
		pair(ilds__id, mlds__type).
mlds_arg_to_il_arg(EntityName - Type) = Id - Type :-
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

:- func input_param_to_ilds_type(il_data_rep, mlds_module_name, 
		pair(entity_name, mlds__type)) = ilds__param.
input_param_to_ilds_type(DataRep, _ModuleName, EntityName - MldsType) 
		= ILType - yes(Id) :-
	mangle_entity_name(EntityName, Id),
	ILType = mlds_type_to_ilds_type(DataRep, MldsType).

:- func mlds_type_to_ilds_simple_type(il_data_rep, mlds__type) =
	 ilds__simple_type.
mlds_type_to_ilds_simple_type(DataRep, MLDSType) = SimpleType :-
	ilds__type(_, SimpleType) = mlds_type_to_ilds_type(DataRep, MLDSType).


	% XXX make sure all the types are converted correctly

mlds_type_to_ilds_type(_, mlds__rtti_type(_RttiName)) = il_array_type.

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

mlds_type_to_ilds_type(_, mlds__class_type(Class, Arity, _Kind)) = 
	ilds__type([], class(
		mlds_class_name_to_ilds_class_name(Class, Arity))).

mlds_type_to_ilds_type(_, mlds__commit_type) = il_commit_type.

mlds_type_to_ilds_type(_, mlds__generic_env_ptr_type) = il_envptr_type.

	% XXX we ought to use the IL bool type
mlds_type_to_ilds_type(_, mlds__native_bool_type) = ilds__type([], int32).

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

mlds_type_to_ilds_type(ILDataRep, mlds__ptr_type(MLDSType)) =
	ilds__type([], '&'(mlds_type_to_ilds_type(ILDataRep, MLDSType))).

mlds_type_to_ilds_type(_, mercury_type(_, int_type)) = ilds__type([], int32).
mlds_type_to_ilds_type(_, mercury_type(_, char_type)) = ilds__type([], char).
mlds_type_to_ilds_type(_, mercury_type(_, float_type)) =
	ilds__type([], float64).
mlds_type_to_ilds_type(_, mercury_type(_, str_type)) = il_string_type.
mlds_type_to_ilds_type(_, mercury_type(_, pred_type)) = il_array_type.
mlds_type_to_ilds_type(_, mercury_type(_, tuple_type)) = il_array_type.
mlds_type_to_ilds_type(_, mercury_type(_, enum_type)) = il_array_type.
mlds_type_to_ilds_type(_, mercury_type(_, polymorphic_type)) = il_generic_type.
mlds_type_to_ilds_type(DataRep, mercury_type(MercuryType, user_type)) = 
	( DataRep ^ highlevel_data = yes ->
		mercury_type_to_highlevel_class_type(MercuryType)
	;
		il_array_type
	).

:- func mercury_type_to_highlevel_class_type(mercury_type) = ilds__type.
mercury_type_to_highlevel_class_type(MercuryType) = ILType :-
	( type_to_type_id(MercuryType, TypeId, _Args) ->
		(
			type_id_is_array(TypeId)
		->
			ILType = il_array_type
		;
			ml_gen_type_name(TypeId, ClassName, Arity),
			ILType = ilds__type([], class(
				mlds_class_name_to_ilds_class_name(
					ClassName, Arity)))
		)
	;
		unexpected(this_file, "type_to_type_id failed")
	).



mlds_type_to_ilds_type(_, mlds__unknown_type) = _ :-
	unexpected(this_file, "mlds_type_to_ilds_type: unknown_type").


:- func mlds_class_name_to_ilds_class_name(mlds__class, arity) =
	ilds__class_name.

mlds_class_name_to_ilds_class_name(
		qual(MldsModuleName, MldsClassName0), Arity) = IldsClassName :-
	MldsClassName = string__format("%s_%d", [s(MldsClassName0), i(Arity)]),
	IldsClassName = append_class_name(
		mlds_module_name_to_class_name(MldsModuleName),
		[MldsClassName]).

mlds_type_to_ilds_class_name(DataRep, MldsType) = 
	get_ilds_type_class_name(mlds_type_to_ilds_type(DataRep, MldsType)).

:- func get_ilds_type_class_name(ilds__type) = ilds__class_name.
get_ilds_type_class_name(ILType) = ClassName :-
	( 
		ILType = ilds__type(_, class(ClassName0))
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
		llds_out__name_mangle(UnMangledId, Id).

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
		llds_out__name_mangle(UnMangledId, Id).


	% If an mlds__var is not an argument or a local, what is it?
	% We assume the given variable is a handwritten RTTI reference or a
	% reference to some hand-written code in the
	% modulename__cpp_code class.  This is OK so long as the
	% code generator uses real 'field' lvals to reference
	% fields in the modulename class.

:- func make_fieldref_for_handdefined_var(il_data_rep, mlds__var, mlds__type)
	 = fieldref.
make_fieldref_for_handdefined_var(DataRep, Var, VarType) = FieldRef :-
	Var = qual(ModuleName, _),
	mangle_mlds_var(Var, MangledVarStr),
	mangle_dataname_module(no, ModuleName, NewModuleName),
	ClassName = mlds_module_name_to_class_name(NewModuleName),
	FieldRef = make_fieldref(
		mlds_type_to_ilds_type(DataRep, VarType), ClassName,
		MangledVarStr).

:- pred mangle_foreign_code_module(mlds_module_name, foreign_language, 
	mlds_module_name).
:- mode mangle_foreign_code_module(in, in, out) is det.

mangle_foreign_code_module(ModuleName0, Lang, ModuleName) :-
	LangStr = globals__simple_foreign_language_string(Lang),
	SymName0 = mlds_module_name_to_sym_name(ModuleName0),
	( SymName0 = qualified(SymName1, Name) ->
		( 
			SymName1 = qualified(Q, M0),
			M = string__format("%s__%s_code", [s(M0), s(LangStr)]),
			SymName = qualified(Q, M)
		; 
			SymName1 = unqualified(M0),
			M = string__format("%s__%s_code", [s(M0), s(LangStr)]),
			SymName = unqualified(M)
		),
		ModuleName = mercury_module_name_to_mlds(
				qualified(SymName, Name))
	;
		error("should never occur.")
	).

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
			LibModuleName0), "mercury_code"),
		DataName = rtti(rtti_type_id(_, Name, Arity),
			_RttiName),
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
			)
		; LibModuleName0 = "array", 
			(
			  Name = "array", Arity = 1
			)
		; LibModuleName0 = "std_util",
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
	->
		string__append(LibModuleName0, "__cpp_code",
			LibModuleName),
		ModuleName = mercury_module_name_to_mlds(
			qualified(qualified(unqualified("mercury"),
			LibModuleName), "mercury_code"))
	;
		ModuleName = ModuleName0
	).



:- pred mangle_dataname(mlds__data_name, string).
:- mode mangle_dataname(in, out) is det.

mangle_dataname(var(MLDSVarName), Name) :-
	Name = mangle_mlds_var_name(MLDSVarName).
mangle_dataname(common(Int), MangledName) :-
	string__format("common_%s", [i(Int)], MangledName).
mangle_dataname(rtti(RttiTypeId, RttiName), MangledName) :-
	rtti__addr_to_string(RttiTypeId, RttiName, MangledName).
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
		structured_name(AssemblyName, ClassName) :-
	SymName = mlds_module_name_to_sym_name(MldsModuleName),
	PackageSymName = mlds_module_name_to_package_name(MldsModuleName),
	sym_name_to_class_name(SymName, ClassName),
	( 
		ClassName = ["mercury" | _]
	->
		AssemblyName = "mercury"
	;
		mlds_to_il__sym_name_to_string(PackageSymName, AssemblyName)
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

:- pred rval_to_type(mlds__rval::in, mlds__type::out,
		il_info::in, il_info::out) is det.

rval_to_type(lval(Lval), Type, Info0, Info) :- 
	( Lval = var(Var, _VarType),
		mangle_mlds_var(Var, MangledVarStr),
		il_info_get_mlds_type(MangledVarStr, Type, Info0, Info)
	; Lval = field(_, _, _, Type, _),
		Info = Info0
	; Lval = mem_ref(_Rval, Type),
		Info = Info0
	).

	% The following five conversions should never occur or be boxed
	% anyway, but just in case they are we make them reference
	% mercury.invalid which is a non-exisitant class.   If we try to
	% run this code, we'll get a runtime error.
	% XXX can we just call error?
rval_to_type(mkword(_Tag, _Rval), Type, I, I) :- 
	ModuleName = mercury_module_name_to_mlds(unqualified("mercury")),
	Type = mlds__class_type(qual(ModuleName, "invalid"),
		0, mlds__class).
rval_to_type(unop(_, _), Type, I, I) :- 
	ModuleName = mercury_module_name_to_mlds(unqualified("mercury")),
	Type = mlds__class_type(qual(ModuleName, "invalid"),
		0, mlds__class).
rval_to_type(binop(_, _, _), Type, I, I) :- 
	ModuleName = mercury_module_name_to_mlds(unqualified("mercury")),
	Type = mlds__class_type(qual(ModuleName, "invalid"),
		0, mlds__class).
rval_to_type(mem_addr(_), Type, I, I) :-
	ModuleName = mercury_module_name_to_mlds(unqualified("mercury")),
	Type = mlds__class_type(qual(ModuleName, "invalid"),
		0, mlds__class).
rval_to_type(self(_), Type, I, I) :-
	ModuleName = mercury_module_name_to_mlds(unqualified("mercury")),
	Type = mlds__class_type(qual(ModuleName, "invalid"),
		0, mlds__class).
rval_to_type(const(Const), Type, I, I) :- 
	Type = rval_const_to_type(Const).

:- func rval_const_to_type(mlds__rval_const) = mlds__type.
rval_const_to_type(data_addr_const(_)) =
	mlds__array_type(mlds__generic_type).
rval_const_to_type(code_addr_const(_)) = mlds__func_type(
		mlds__func_params([], [])).
rval_const_to_type(int_const(_)) = mercury_type(
	term__functor(term__atom("int"), [], context("", 0)), int_type).
rval_const_to_type(float_const(_)) = mercury_type(
	term__functor(term__atom("float"), [], context("", 0)), float_type).
rval_const_to_type(false) = mlds__native_bool_type.
rval_const_to_type(true) = mlds__native_bool_type.
rval_const_to_type(string_const(_)) = mercury_type(
	term__functor(term__atom("string"), [], context("", 0)), str_type).
rval_const_to_type(multi_string_const(_, _)) = mercury_type(
	term__functor(term__atom("string"), [], context("", 0)), str_type).
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
	FieldRef = make_fieldref(il_array_type, ClassName, FieldName).


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
:- func get_fieldref(il_data_rep, field_id, mlds__type, mlds__type) = fieldref.
get_fieldref(DataRep, FieldNum, FieldType, ClassType) = FieldRef :-
		FieldILType0 = mlds_type_to_ilds_type(DataRep,
			FieldType),
		( FieldILType0 = ilds__type(_, '&'(FieldILType1)) ->
			FieldILType = FieldILType1
		;
			FieldILType = FieldILType0
		),
		( 
			FieldNum = offset(OffsetRval),
			ClassName = mlds_type_to_ilds_class_name(DataRep,
				ClassType),
			( OffsetRval = const(int_const(Num)) ->
				string__format("f%d", [i(Num)], FieldId)
			;
				sorry(this_file, 
					"offsets for non-int_const rvals")
			)
		; 
			FieldNum = named_field(qual(ModuleName, FieldId),
				_Type),
			ClassName = mlds_module_name_to_class_name(ModuleName)
		),
		FieldRef = make_fieldref(FieldILType, ClassName, FieldId).


%-----------------------------------------------------------------------------%

:- pred defn_to_local(mlds_module_name, mlds__defn, 
	pair(ilds__id, mlds__type)).
:- mode defn_to_local(in, in, out) is det.

defn_to_local(ModuleName, 
	mlds__defn(Name, _Context, _DeclFlags, Entity), Id - MLDSType) :-
	( Name = data(DataName),
	  Entity = mlds__data(MLDSType0, _Initializer) ->
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

:- func convert_to_object(ilds__type) = methodref.

convert_to_object(Type) = methoddef(call_conv(no, default), 
		simple_type(il_generic_simple_type),
		class_member_name(il_conversion_class_name, id("ToObject")),
		[Type]).

:- func convert_from_object(ilds__type) = methodref.

convert_from_object(Type) = 
	methoddef(call_conv(no, default), simple_type(SimpleType),
		class_member_name(il_conversion_class_name, id(Id)),
			[il_generic_type]) :-
	Type = ilds__type(_, SimpleType),
	ValueClassName = simple_type_to_value_class_name(SimpleType),
	string__append("To", ValueClassName, Id).


	% XXX String and Array should be converted to/from Object using a
	% cast, not a call to runtime convert.  When that is done they can be
	% removed from this list
:- func simple_type_to_value_class_name(simple_type) = string.
simple_type_to_value_class_name(int8) = "Int8".
simple_type_to_value_class_name(int16) = "Int16".
simple_type_to_value_class_name(int32) = "Int32".
simple_type_to_value_class_name(int64) = "Int64".
simple_type_to_value_class_name(uint8) = "Int8".
simple_type_to_value_class_name(uint16) = "UInt16".
simple_type_to_value_class_name(uint32) = "UInt32".
simple_type_to_value_class_name(uint64) = "UInt64".
simple_type_to_value_class_name(float32) = "Single".
simple_type_to_value_class_name(float64) = "Double".
simple_type_to_value_class_name(bool) = "Bool".
simple_type_to_value_class_name(char) = "Char".
simple_type_to_value_class_name(refany) = _ :-
	error("no value class name for refany").
simple_type_to_value_class_name(class(Name)) = VCName :-
	( Name = il_string_class_name ->
		VCName = "String"
	;
		error("unknown class name")
	).
simple_type_to_value_class_name(value_class(_)) = _ :-
	error("no value class name for value_class").
simple_type_to_value_class_name(interface(_)) = _ :-
	error("no value class name for interface").
simple_type_to_value_class_name('[]'(_, _)) = "Array".
simple_type_to_value_class_name('&'( _)) = _ :-
	error("no value class name for '&'").
simple_type_to_value_class_name('*'(_)) = _ :-
	error("no value class name for '*'").
simple_type_to_value_class_name(native_float) = _ :-
	error("no value class name for native float").
simple_type_to_value_class_name(native_int) = _ :-
	error("no value class name for native int").
simple_type_to_value_class_name(native_uint) = _ :-
	error("no value class name for native uint").

%-----------------------------------------------------------------------------%
%
% The mapping to the string type.
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
	sym_name_to_class_name(qualified(unqualified("string"), "mercury_code"),
			StringClass).

%-----------------------------------------------------------------------------%
%
% The mapping to the generic type (used like MR_Box).
%

:- func il_generic_type = ilds__type.
il_generic_type = ilds__type([], il_generic_simple_type).

:- func il_generic_simple_type = simple_type.
il_generic_simple_type = class(il_generic_class_name).

il_generic_class_name = il_system_name(["Object"]).

%-----------------------------------------------------------------------------%
%
% The mapping to the array type (used like MR_Word).
%

	% il_array_type means array of System.Object.
:- func il_array_type = ilds__type.
il_array_type = ilds__type([], '[]'(il_generic_type, [])).

%-----------------------------------------------------------------------------%
%
% The class that performs conversion operations
%

:- func il_conversion_class_name = ilds__class_name.
il_conversion_class_name = mercury_runtime_name(["Convert"]).

%-----------------------------------------------------------------------------%
%
% The mapping to the exception type.
%

:- func il_exception_type = ilds__type.
il_exception_type = ilds__type([], il_exception_simple_type).

:- func il_exception_simple_type = simple_type.
il_exception_simple_type = class(il_exception_class_name).

:- func il_exception_class_name = ilds__class_name.
il_exception_class_name = mercury_runtime_name(["Exception"]).

%-----------------------------------------------------------------------------%
%
% The mapping to the environment type.
%

:- func il_envptr_type = ilds__type.
il_envptr_type = ilds__type([], il_envptr_simple_type).

:- func il_envptr_simple_type = simple_type.
il_envptr_simple_type = class(il_envptr_class_name).

:- func il_envptr_class_name = ilds__class_name.
il_envptr_class_name = mercury_runtime_name(["Environment"]).


%-----------------------------------------------------------------------------%
%
% The mapping to the commit type.
%

:- func il_commit_type = ilds__type.
il_commit_type = ilds__type([], il_commit_simple_type).

:- func il_commit_simple_type = simple_type.
il_commit_simple_type = class(il_commit_class_name).

:- func il_commit_class_name = ilds__class_name.
il_commit_class_name = mercury_runtime_name(["Commit"]).

%-----------------------------------------------------------------------------

	% qualifiy a name with "[mercury]mercury."
:- func mercury_library_name(ilds__namespace_qual_name) = ilds__class_name.
mercury_library_name(Name) = 
	append_class_name(mercury_library_namespace_name, Name).

:- func mercury_library_namespace_name = ilds__class_name.
mercury_library_namespace_name = structured_name("mercury", ["mercury"]).

%-----------------------------------------------------------------------------

	% qualifiy a name with "[mercury]mercury.runtime."
:- func mercury_runtime_name(ilds__namespace_qual_name) = ilds__class_name.
mercury_runtime_name(Name) = 
	append_class_name(mercury_runtime_class_name, Name).

:- func mercury_runtime_class_name = ilds__class_name.
mercury_runtime_class_name = structured_name("mercury",
	["mercury", "runtime"]).

%-----------------------------------------------------------------------------

	% qualifiy a name with "[mscorlib]System."
:- func il_system_name(ilds__namespace_qual_name) = ilds__class_name.
il_system_name(Name) = structured_name(il_system_assembly_name, 
		[il_system_namespace_name | Name]).

:- func il_system_assembly_name = string.
il_system_assembly_name = "mscorlib".

:- func il_system_namespace_name = string.
il_system_namespace_name = "System".

%-----------------------------------------------------------------------------

	% Generate extern decls for any assembly we reference.
:- pred mlds_to_il__generate_extern_assembly(mlds__imports, list(decl)).
:- mode mlds_to_il__generate_extern_assembly(in, out) is det.

mlds_to_il__generate_extern_assembly(Imports, AllDecls) :-
	Gen = (pred(Import::in, Decl::out) is semidet :-
		ClassName = mlds_module_name_to_class_name(Import),
		ClassName = structured_name(Assembly, _),
		not (Assembly = "mercury"),
		Decl = extern_assembly(Assembly, [])
	),
	list__filter_map(Gen, Imports, Decls0),
	list__sort_and_remove_dups(Decls0, Decls),
	AllDecls = [
		extern_assembly("mercury", [
			version(0, 0, 0, 0),
			public_key_token([
				int8(0x22), int8(0x8C), int8(0x16), int8(0x7D),
				int8(0x12), int8(0xAA), int8(0x0B), int8(0x0B)
			])
		]),
		extern_assembly("mscorlib", [
			version(1, 0, 2411, 0),
			public_key_token([
				int8(0xb7), int8(0x7a), int8(0x5c), int8(0x56),
				int8(0x19), int8(0x34), int8(0xE0), int8(0x89)
			]),
			hash([
				int8(0xb0), int8(0x73), int8(0xf2), int8(0x4c),
				int8(0x14), int8(0x39), int8(0x0a), int8(0x35),
				int8(0x25), int8(0xea), int8(0x45), int8(0x0f),
				int8(0x60), int8(0x58), int8(0xc3), int8(0x84),
				int8(0xe0), int8(0x3b), int8(0xe0), int8(0x95)
			])
		]) | Decls].

%-----------------------------------------------------------------------------

:- func make_method_defn(bool, instr_tree) = method_defn.
make_method_defn(DebugIlAsm, InstrTree) = MethodDecls :-
	( DebugIlAsm = yes,
		Add = 1
	; DebugIlAsm = no,
		Add = 0
	),
	Instrs = list__condense(tree__flatten(InstrTree)),
	MethodDecls = [
		maxstack(int32(calculate_max_stack(Instrs) + Add)),
			% note that we only need .zeroinit to ensure
			% verifiability; for nonverifiable code,
			% we could omit that (it ensures that all
			% variables are initialized to zero).
		zeroinit,
		instrs(Instrs)
		].

%-----------------------------------------------------------------------------
% Some useful functions for generating IL fragments.
		
:- func load_this = instr.
load_this = ldarg(index(0)).

:- func call_class_constructor(ilds__class_name) = instr.
call_class_constructor(CtorMemberName) = 
	call(get_static_methodref(CtorMemberName, cctor, void, [])).

:- func call_constructor(ilds__class_name) = instr.
call_constructor(CtorMemberName) = 
	call(get_constructor_methoddef(CtorMemberName)).

:- func throw_unimplemented(string) = instr_tree.
throw_unimplemented(String) = 
	node([
		ldstr(String),
		newobj(get_instance_methodref(il_exception_class_name,
			ctor, void, [il_string_type])),
		throw]
	).

:- func newobj_constructor(ilds__class_name) = instr.
newobj_constructor(CtorMemberName) = 
	newobj(get_constructor_methoddef(CtorMemberName)).

:- func get_constructor_methoddef(ilds__class_name) = methodref.
get_constructor_methoddef(CtorMemberName) = 
	get_instance_methodref(CtorMemberName, ctor, void, []).

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

:- func make_constructor_classdecl(method_defn) = classdecl.
make_constructor_classdecl(MethodDecls) = method(
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
	structured_name("mercury",
		["mercury", "private_builtin__cpp_code", "mercury_code"]).

:- func runtime_init_method_name = ilds__member_name.
runtime_init_method_name = id("init_runtime").

%-----------------------------------------------------------------------------%
%
% Predicates for manipulating il_info.
%

:- func il_info_init(mlds_module_name, assembly_name, mlds__imports,
		il_data_rep, bool) = il_info.

il_info_init(ModuleName, AssemblyName, Imports, ILDataRep, DebugIlAsm) =
	il_info(ModuleName, AssemblyName, Imports, set__init, ILDataRep,
		DebugIlAsm, empty, empty, [], no, set__init,
		map__init, empty, counter__init(1), counter__init(1), no,
		Args, MethodName, DefaultSignature) :-
	Args = [],
	DefaultSignature = signature(call_conv(no, default), void, []),
	MethodName = id("").

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

:- pred il_info_add_classdecls(list(classdecl), il_info, il_info).
:- mode il_info_add_classdecls(in, in, out) is det.
il_info_add_classdecls(ClassDecls, Info0, Info) :- 
	Info = Info0 ^ classdecls := 
		list__append(ClassDecls, Info0 ^ classdecls).

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

