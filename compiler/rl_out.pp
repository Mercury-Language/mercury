%-----------------------------------------------------------------------------%
% Copyright (C) 1998-1999 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% File: rl_out.m
% Main author: stayl
%
% Generate RL bytecodes.
%
% See $ADITI_ROOT/src/rosi/rlo_spec.tex for a partial specification 
% of the bytecodes. (copy in ~stayl/aditi/src/rosi/rlo_spec.tex)
%
% The conditional compilation in this module is done to avoid 
% major efficiency problems when compiling the large disjunctions
% in rl_code.m using the alias branch mode checker.
%
%-----------------------------------------------------------------------------%
:- module rl_out.

:- interface.

:- import_module rl, rl_file, hlds_module.
#if INCLUDE_ADITI_OUTPUT	% See ../Mmake.common.in.
:- import_module rl_code, tree.
#else
#endif

:- import_module list, io, std_util.

	% Output schemas for locally defined base and derived relations to
	% <module>.base_schema and <module>.derived_schema respectively.
:- pred rl_out__generate_schema_file(module_info::in,
		io__state::di, io__state::uo) is det.

	% Output bytecode to `<module>.rlo' if --aditi-only was set and a text
	% representation to `<module>.rla' if --dump-rl-bytecode was specified. 
	% Output schema information for derived relations to 
	% `<module>.derived_schema' if --generate-schemas was set.
	% If --aditi-only is not set, return the rl_file containing
	% bytecodes to be output as constant data in the C file.
:- pred rl_out__generate_rl_bytecode(module_info::in, list(rl_proc)::in, 
		maybe(rl_file)::out, io__state::di, io__state::uo) is det.

#if INCLUDE_ADITI_OUTPUT	% See ../Mmake.common.in.
	% Given a predicate to update the labels in a bytecode, update
	% all the labels in a tree of bytecodes.
:- pred rl_out__resolve_addresses(pred(bytecode, bytecode),
		byte_tree, byte_tree). 
:- mode rl_out__resolve_addresses(pred(in, out) is det, in, out) is det.

:- type byte_tree == tree(list(bytecode)).
#else
#endif

%-----------------------------------------------------------------------------%
:- implementation.

:- import_module code_util, hlds_data, hlds_pred, prog_data, prog_out.
:- import_module llds, globals, options, tree, type_util, passes_aux.
:- import_module rl_file, getopt, modules, prog_util, magic_util.

#if INCLUDE_ADITI_OUTPUT	% See ../Mmake.common.in.
:- import_module rl_exprn.
#else
#endif

:- import_module assoc_list, bool, char, int, map, multi_map, require, set.
:- import_module string, term, tree, varset.

%-----------------------------------------------------------------------------%

rl_out__generate_schema_file(ModuleInfo) -->
	{ module_info_name(ModuleInfo, ModuleName) },
	module_name_to_file_name(ModuleName, ".base_schema", yes, FileName),
	io__open_output(FileName, Res),
	( { Res = ok(Stream) } ->
		io__set_output_stream(Stream, OldStream),
		{ module_info_predids(ModuleInfo, PredIds) },
		list__foldl(rl_out__generate_schema_file_2(ModuleInfo),
			PredIds),
		io__set_output_stream(OldStream, _)
	;
		{ string__append_list(["Error: cannot open ", FileName,
			" for output.\n"], Msg) },
		{ error(Msg) }
	).

:- pred rl_out__generate_schema_file_2(module_info::in, pred_id::in, 
		io__state::di, io__state::uo) is det.

rl_out__generate_schema_file_2(ModuleInfo, PredId) -->
	{ module_info_pred_info(ModuleInfo, PredId, PredInfo) },
	{ pred_info_get_markers(PredInfo, Markers) },
	{ module_info_name(ModuleInfo, Module) },
	{ pred_info_module(PredInfo, PredModule) },
	(
		{ Module = PredModule },
		{ check_marker(Markers, base_relation) }
	->
		{ rl__get_permanent_relation_info(ModuleInfo, PredId,
			Owner, ModuleName, PredName, PredArity0,
			RelName, RelSchema) },
		{ string__int_to_string(PredArity0, PredArity) },
		io__write_strings([ModuleName, ":", PredName, "/", PredArity,
			"\t", Owner, "/", ModuleName, "/", RelName,
			"\t", RelSchema, "\n"])
	;
		[]
	).

%-----------------------------------------------------------------------------%

	% If the RL procedure is callable from the query shell or Mercury,
	% i.e. it has one entry point, generate a description of the
	% procedure to the `<module>.derived_schema' file.
:- pred rl_out__generate_derived_schema(module_info::in, rl_proc::in,
		io__state::di, io__state::uo) is det.

rl_out__generate_derived_schema(ModuleInfo, Proc) -->
	{ Proc = rl_proc(ProcName, Inputs, Outputs, _,
			RelInfo, _, EntryPoints) },
	(
		{ EntryPoints = [proc(PredId, _)] }, 
		{ Inputs = [InputRel] },
		{ Outputs = [OutputRel] }
	->
		{ module_info_pred_info(ModuleInfo, PredId, PredInfo) },
		{ pred_info_module(PredInfo, PredModule0) }, 
		{ prog_out__sym_name_to_string(PredModule0, PredModule) },
		{ pred_info_name(PredInfo, PredName) },
		{ pred_info_arity(PredInfo, PredArity0) },
		{ string__int_to_string(PredArity0, PredArity) },
		{ rl_out__get_proc_schema(ModuleInfo, RelInfo,
			[InputRel, OutputRel], SchemaString) },
		{ rl__proc_name_to_string(ProcName, ProcNameStr) },
		io__write_strings([PredModule, ":", PredName, "/", PredArity,
			"\t", ProcNameStr, "\t", SchemaString, "\n"])
	;
		[]
	).

:- pred rl_out__get_proc_schema(module_info::in, relation_info_map::in,
		list(relation_id)::in, string::out) is det.

rl_out__get_proc_schema(ModuleInfo, Relations, Args, SchemaString) :- 
	list__map(
		(pred(Arg::in, ArgSchema::out) is det :-
			map__lookup(Relations, Arg, ArgInfo),
			ArgInfo = relation_info(_, ArgSchema, _, _)
		), Args, ArgSchemas),
	rl__schemas_to_strings(ModuleInfo, ArgSchemas,
		TypeDecls, ArgSchemaStrings),
	list__map_foldl(
		(pred(ArgSchemaString::in, ArgSchemaDecl::out,
				Index::in, (Index + 1)::out) is det :-
			ArgPrefix = "__arg_",
			string__int_to_string(Index, ArgString),
			string__append_list(
				[":", ArgPrefix, ArgString, "=",
				ArgPrefix, ArgString, "(",
				ArgSchemaString, ") "],
				ArgSchemaDecl)
		), ArgSchemaStrings, ArgSchemaDeclList, 1, _),
	rl_out__get_proc_schema_2(1, Args, "", SchemaString0),
	list__condense([[TypeDecls | ArgSchemaDeclList], ["("],
		[SchemaString0, ")"]], SchemaStrings),
	string__append_list(SchemaStrings, SchemaString).

:- pred rl_out__get_proc_schema_2(int::in, list(T)::in,
		string::in, string::out) is det.

rl_out__get_proc_schema_2(_, [], SchemaList, SchemaList). 
rl_out__get_proc_schema_2(ArgNo, [_ | Args], SchemaList0, SchemaList) :-
	ArgPrefix = "__arg_",
	( Args = [] ->
		Comma = ""
	;
		Comma = ","
	),
	string__int_to_string(ArgNo, ArgString),
	string__append_list([SchemaList0, ":T", ArgPrefix, ArgString, Comma],
		SchemaList1),
	rl_out__get_proc_schema_2(ArgNo + 1, Args, SchemaList1, SchemaList).

%-----------------------------------------------------------------------------%

#if INCLUDE_ADITI_OUTPUT	% See ../Mmake.common.in,

rl_out__generate_rl_bytecode(ModuleInfo, Procs, MaybeRLFile) -->
	{ module_info_name(ModuleInfo, ModuleName0) },
	module_name_to_file_name(ModuleName0, ".rlo", yes, RLOName),
	module_name_to_file_name(ModuleName0, ".rla", yes, RLAName),
	globals__io_lookup_bool_option(verbose, Verbose),
	maybe_write_string(Verbose, "% Writing RL bytecode to `"),
	maybe_write_string(Verbose, RLOName),
	maybe_write_string(Verbose, "'..."),
	maybe_flush_output(Verbose),

	{ rl_out_info_init(ModuleInfo, RLInfo0) },
	{ list__foldl(rl_out__generate_proc_bytecode, Procs, 
		RLInfo0, RLInfo1) },

	globals__io_lookup_string_option(aditi_user, Owner),
	{ rl_out_info_assign_const(string(Owner), OwnerIndex,
		RLInfo1, RLInfo2) },
	{ prog_out__sym_name_to_string(ModuleName0, ModuleName) },
	module_name_to_file_name(ModuleName0, ".m", no, SourceFileName),
	module_name_to_file_name(ModuleName0, ".int", no, IntFileName),
	{ rl_out_info_assign_const(string(ModuleName), ModuleIndex, 
		RLInfo2, RLInfo3) },
	{ rl_out_info_assign_const(string(IntFileName), IntIndex, 
		RLInfo3, RLInfo4) },
	{ rl_out_info_assign_const(string(SourceFileName), 
		SourceIndex, RLInfo4, RLInfo5) },
	{ rl_out_info_get_procs(RLProcs, RLInfo5, RLInfo6) },
	{ rl_out_info_get_consts(Consts, RLInfo6, RLInfo7) },
	{ rl_out_info_get_permanent_relations(PermRelsSet, 
		RLInfo7, RLInfo8) },
	{ rl_out_info_get_relation_variables(RelVars, RLInfo8, _) },

	{ map__to_assoc_list(Consts, ConstsAL) },
	{ assoc_list__reverse_members(ConstsAL, ConstsLA0) },
	{ list__sort(ConstsLA0, ConstsLA) },
	{ list__length(ConstsLA, ConstTableSize0) },
	{ ConstTableSize is ConstTableSize0 + 1 },
	{ set__to_sorted_list(PermRelsSet, PermRels) },
	{ list__length(PermRels, NumPermRels) },
	{ list__length(RLProcs, NumProcs) },

	{ list__length(RelVars, NumVars) },

	{ rl_code__version(MinorVersion, MajorVersion) },
	{ File = rl_file(
			MinorVersion,
			MajorVersion,
			ConstTableSize,
			ConstsLA,
			NumPermRels,
			PermRels,
			NumVars,
			RelVars,
			NumProcs,
			RLProcs,
			OwnerIndex,
			ModuleIndex,
			SourceIndex,
			IntIndex
		) },

	%
	% Dump the binary representation to `<module>.rlo', if --aditi-only
	% was specified, otherwise return the rl_file for output into the
	% C file.
	%
	globals__io_lookup_bool_option(aditi_only, AditiOnly),
	( { AditiOnly = yes } ->
		io__open_binary_output(RLOName, RLOResult),

		( { RLOResult = ok(RLOStream) } ->
			io__set_binary_output_stream(RLOStream, OldBinStream),
			rl_file__write_binary(io__write_byte, File, _),
			io__close_binary_output(RLOStream),
			io__set_binary_output_stream(OldBinStream, _)
		; 
			{ string__append_list([
				"cannot open `", RLOName, "' for output.\n"], 
				RLOError) },
			report_error(RLOError)
		),
		{ MaybeRLFile = no }
	;
		{ MaybeRLFile = yes(File) }
	),

	%
	% Dump the text representation to `<module>.rla'
	%
	globals__io_lookup_bool_option(dump_rl_bytecode, DumpToRLA),
	( { DumpToRLA = yes } ->
		io__open_output(RLAName, RLAResult),
		( { RLAResult = ok(RLAStream) } ->
			io__set_output_stream(RLAStream, OldStdOut),
			rl_file__write_text(File),
			io__close_output(RLAStream),
			io__set_output_stream(OldStdOut, _)
		;
			{ string__append_list([
				"cannot open `", RLAName, "' for output.\n"], 
				RLAError) },
			report_error(RLAError)
		)
	;
		[]
	),

	%
	% Dump the schema information for derived relations to
	% `<module>.derived_schema'.
	%
	globals__io_lookup_bool_option(generate_schemas, GenSchemas),
	( { GenSchemas = yes } ->
		module_name_to_file_name(ModuleName0, ".derived_schema",
			no, SchemaFileName),
		io__open_output(SchemaFileName, SchemaResult),
		( { SchemaResult = ok(SchemaStream) } ->
			io__set_output_stream(SchemaStream, OldStream),
			list__foldl(
				rl_out__generate_derived_schema(ModuleInfo),
				Procs),
			io__set_output_stream(OldStream, SchemaStream1),
			io__close_output(SchemaStream1)
		;
			{ string__append_list([
				"cannot open `", SchemaFileName,
				"' for output.\n"], SchemaError) },
			report_error(SchemaError)
		)
	;
		[]
	),
	maybe_write_string(Verbose, "done\n").
#else
rl_out__generate_rl_bytecode(_, _, MaybeRLFile) -->
	{ semidet_succeed ->
		error("rl_out.pp: `--aditi' requires `INCLUDE_ADITI_OUTPUT'")
	;
		MaybeRLFile = no
	}.
#endif
	
#if INCLUDE_ADITI_OUTPUT
:- pred rl_out__generate_proc_bytecode(rl_proc::in, 
		rl_out_info::in, rl_out_info::out) is det.

rl_out__generate_proc_bytecode(Proc) -->
	{ Proc = rl_proc(Name, Inputs, Outputs, MemoedRels,
			Relations, RLInstrs, _) },

	{ Name = rl_proc_name(Owner, Module, ProcName, _) },

	{ list__append(Inputs, Outputs, Args) },
	rl_out_info_init_proc(Relations, Args),
	rl_out__generate_instr_list(RLInstrs, RLInstrCodeTree0),
	
	{ set__to_sorted_list(MemoedRels, MemoedList) },
	( { MemoedList = [] } ->
		{ CollectCode = [] },
		{ NameCode = [] },
		{ GroupCode = empty }
	;
		% If one memoed relation is dropped, all must be 
		% dropped for correctness. We could possibly be a
		% little smarter about this.
		rl_out__collect_memoed_relations(Owner, Name, MemoedList, 0,
			CollectCode, NameCode),
		rl_out__get_rel_var_list(MemoedList, RelVarCodes),
		{ GroupCode = tree(node([rl_PROC_grouprels]), RelVarCodes) }
	),

	rl_out_info_get_relation_addrs(Addrs),
	{ map__to_assoc_list(Addrs, AddrsAL) },
	rl_out__collect_permanent_relations(AddrsAL, [], PermRelCodes),

	rl_out_info_get_proc_expressions(Exprns),
	{ list__length(Exprns, NumExprns) },

	rl_out__resolve_proc_addresses(RLInstrCodeTree0, RLInstrCodeTree1),

	{ RLInstrCodeTree = 
		tree(node(PermRelCodes),
		tree(node(CollectCode),
		tree(RLInstrCodeTree1,
		tree(node(NameCode),
		tree(GroupCode,
		node([rl_PROC_ret])	
	))))) },
	{ tree__flatten(RLInstrCodeTree, CodeLists) },
	{ list__condense(CodeLists, Codes) },

	rl_out_info_assign_const(string(Owner), OwnerConst),
	rl_out_info_assign_const(string(Module), ModuleConst),
	rl_out_info_assign_const(string(ProcName), NameConst),
	{ rl_out__instr_code_size(node(Codes), CodeLength) },

	{ list__length(Args, NumArgs) },
	list__map_foldl(rl_out_info_get_relation_addr, Args, ArgLocs),
	rl_out__generate_proc_schema(Args, SchemaConst),

	{ RLProc = procedure(OwnerConst, ModuleConst, NameConst, SchemaConst,
			NumArgs, ArgLocs, NumExprns, Exprns,
			CodeLength, Codes) },
	rl_out_info_add_proc(RLProc).

%-----------------------------------------------------------------------------%

	% Temporaries in Aditi are reference counted. If the count on a
	% temporary goes to zero, it may be garbage collected. For relations
	% which are memoed, we do not inhibit garbage collection by
	% holding a reference to them. Instead we just give them a name
	% by which we can retrieve the relation later. If the system does
	% not need to garbage collect the relation between calls, it
	% will be used, otherwise it will be reinitialised. If one
	% memoed relation in a procedure is dropped, all must be dropped
	% to maintain correctness. Aditi should prefer to drop unnamed 
	% temporaries to named ones, since unnamed temporaries cannot
	% possibly be used later.
:- pred rl_out__collect_memoed_relations(string::in, rl_proc_name::in,
		list(relation_id)::in, int::in, list(bytecode)::out,
		list(bytecode)::out, rl_out_info::in,
		rl_out_info::out) is det.

rl_out__collect_memoed_relations(_, _, [], _, [], []) --> [].
rl_out__collect_memoed_relations(Owner, ProcName, [Rel | Rels], Counter0,
		[GetCode | GetCodes], [NameCode, DropCode | NameCodes]) -->

	rl_out_info_get_relation_addr(Rel, Addr),
	rl_out_info_get_relation_schema_offset(Rel, SchemaOffset),

	{ rl__proc_name_to_string(ProcName, ProcNameStr) },
	{ string__to_char_list(ProcNameStr, ProcNameList0) },
	% Slashes are significant in relation names, so convert them to colons.
	{ RemoveSlashes =
		lambda([Char0::in, Char::out] is det, (
			( Char0 = ('/') ->
				Char = (':')
			;
				Char = Char0
			)
		)) },
	{ list__map(RemoveSlashes, ProcNameList0, ProcNameList) },
	{ string__from_char_list(ProcNameList, ProcNameStr1) },
	rl_out_info_get_module_info(ModuleInfo),
	{ module_info_name(ModuleInfo, ModuleName0) },
	{ prog_out__sym_name_to_string(ModuleName0, ModuleName) },
	{ string__format("%s/%s/Memoed__%s__%i", 
		[s(Owner), s(ModuleName), s(ProcNameStr1), i(Counter0)],
		UniqueName) },
	rl_out_info_assign_const(string(UniqueName), NameOffset),

		% Get the memoed relation, if it exists, at the start of
		% the procedure. If it does not exist it will be created.
	{ GetCode = rl_PROC_settemprel(Addr, NameOffset, SchemaOffset) },

		% Make sure the relation variable has the correct name
		% at the end of the procedure so that the settemprel can 
		% find it at the start of the next call.
	{ NameCode = rl_PROC_nametemprel(Addr, NameOffset) },

		% Drop the pointer - this should already have been done
		% for non-memoed relations, but we need to name memoed 
		% relations before dropping the pointers to them.
	{ DropCode = rl_PROC_unsetrel(Addr) },

	{ Counter is Counter0 + 1 },
	rl_out__collect_memoed_relations(Owner, ProcName, Rels, Counter,
		GetCodes, NameCodes).

	% Put pointers to all the permanent relations
	% used by the procedure into variables.
:- pred rl_out__collect_permanent_relations(assoc_list(relation_id, int)::in,
		list(bytecode)::in, list(bytecode)::out,
		rl_out_info::in, rl_out_info::out) is det.

rl_out__collect_permanent_relations([], Codes, Codes) --> [].
rl_out__collect_permanent_relations([RelationId - Addr | Rels],
		Codes0, Codes) -->
	rl_out_info_get_relations(Relations),
	{ map__lookup(Relations, RelationId, RelInfo) },
	{ RelInfo = relation_info(RelType, _Schema, _Index, _) },
	(
		{ RelType = permanent(proc(PredId, _)) }
	->
		rl_out_info_get_module_info(ModuleInfo),

		{ rl__get_permanent_relation_info(ModuleInfo, PredId,
			Owner, PredModule, _, _, RelName, SchemaString) },

		rl_out_info_assign_const(string(Owner), OwnerConst), 
		rl_out_info_assign_const(string(PredModule), PredModuleConst),
		rl_out_info_assign_const(string(SchemaString), SchemaOffset),
		rl_out_info_assign_const(string(RelName), RelNameConst),

		rl_out_info_get_permanent_relations(PermRels0),
		{ set__insert(PermRels0, 
			relation(OwnerConst, PredModuleConst, 
				RelNameConst, SchemaOffset), 
			PermRels) },
		rl_out_info_set_permanent_relations(PermRels),

		{ string__format("%s/%s/%s", 
			[s(Owner), s(PredModule), s(RelName)], Name) },
		rl_out_info_assign_const(string(Name), RelNameOffset),
		{ SetCode = rl_PROC_openpermrel(Addr, RelNameOffset, 
				SchemaOffset) },
		{ Codes1 = [SetCode | Codes0] }
	;
		{ Codes1 = Codes0 }
	),
	rl_out__collect_permanent_relations(Rels, Codes1, Codes).

%-----------------------------------------------------------------------------%

:- pred rl_out__get_rel_var_list(list(relation_id)::in, byte_tree::out,
		rl_out_info::in, rl_out_info::out) is det.

rl_out__get_rel_var_list(Rels, Code) -->
	list__map_foldl(rl_out_info_get_relation_addr, Rels, Addrs),
	{ ConsElems = lambda([Addr::in, Cons::out] is det, (
			LockSpec = 0,
			Cons = rl_PROC_var_list_cons(Addr, LockSpec)
		)) },
	{ list__map(ConsElems, Addrs, Code1) },
	{ Code = tree(node(Code1), node([rl_PROC_var_list_nil])) }.

%-----------------------------------------------------------------------------%

	% Generate the schema string for a procedure.
:- pred rl_out__generate_proc_schema(list(relation_id)::in, int::out,
		rl_out_info::in, rl_out_info::out) is det.

rl_out__generate_proc_schema(Args, SchemaOffset) -->
	rl_out_info_get_module_info(ModuleInfo),
	rl_out_info_get_relations(Relations),
	{ rl_out__get_proc_schema(ModuleInfo, Relations, Args, SchemaString) },
	rl_out_info_assign_const(string(SchemaString), SchemaOffset).

%-----------------------------------------------------------------------------%

	% Convert a schema for use in the bytecode. A schema string is
	% a list of type definitions followed by a bracketed list
	% of types.
:- pred rl_out__schema_to_string(list(type)::in, int::out,
		rl_out_info::in, rl_out_info::out) is det.

rl_out__schema_to_string(Types, SchemaOffset) -->
	rl_out_info_get_module_info(ModuleInfo),
	{ rl__schema_to_string(ModuleInfo, Types, SchemaString) },
	rl_out_info_assign_const(string(SchemaString), SchemaOffset).

%-----------------------------------------------------------------------------%

:- pred rl_out__generate_instr_list(list(rl_instruction)::in, byte_tree::out,
		rl_out_info::in, rl_out_info::out) is det.

rl_out__generate_instr_list([], empty) --> [].
rl_out__generate_instr_list([RLInstr | RLInstrs], Code) -->
	rl_out__generate_instr(RLInstr, Code1),
	{ rl_out__instr_code_size(Code1, Size) },
	rl_out_info_incr_pc(Size),
	rl_out__generate_instr_list(RLInstrs, Code2),
	{ Code = tree(Code1, Code2) }.

%-----------------------------------------------------------------------------%

:- pred rl_out__generate_instr(rl_instruction::in, byte_tree::out,
		rl_out_info::in, rl_out_info::out) is det.

rl_out__generate_instr(join(Output, Input1, Input2, Type, Cond) - _, Code) -->
	(
		{ Type = nested_loop },
		rl_out__generate_join(rl_PROC_join_nl, Output,
			Input1, Input2, Cond, Code)
	;
		{ Type = sort_merge(_, _) },
		rl_out__generate_join(rl_PROC_join_sm, Output,
			Input1, Input2, Cond, Code)
	;
		{ Type = index(IndexSpec, Range) },
		{ rl_out__index_spec_to_string(IndexSpec, IndexStr) },
		rl_out_info_assign_const(string(IndexStr), IndexConst),
		rl_out__generate_stream(Input1, Stream1Code),
		rl_out_info_get_relation_addr(Input2, Input2Addr),
		rl_out__generate_key_range(Range, RangeExprn),
		rl_out_info_get_output_relation_schema_offset(Output,
			OutputSchemaOffset),
		rl_out__generate_exprn(Cond, OutputSchemaOffset, CondExprn),
		{ Code =
			tree(node([rl_PROC_join_index_simple]),
			tree(Stream1Code,
			node([
				rl_PROC_indexed_var(Input2Addr, 0, IndexConst),
				rl_PROC_expr(RangeExprn),
				rl_PROC_expr(CondExprn)
			])
		)) }
	;
		{ Type = cross },
		rl_out__generate_join(rl_PROC_join_cross, Output,
			Input1, Input2, Cond, Code)
	;
		{ Type = semi },
		%
		% Optimize a common case here - if the output does not depend
		% on the second relation, we generate this as:
		% if (empty(rel2)) {
		% 	init(output);
		% } else {
		%	output = rel1;
		% }
		%
		% This happens for joins with zero-arity input relations.
		(
			{ rl__goal_is_independent_of_input(two,
				Cond, _Cond1) }
		->
			rl_out__generate_stream(Input1, Stream1Code),
			rl_out__generate_stream(Input2, Stream2Code),
			{ CondCode =
				tree(node([rl_PROC_empty]),
				Stream2Code) },
			rl_out__generate_instr(init(Output) - "", ThenCode),
			rl_out__generate_stream_instruction(Output,
				Stream1Code, ElseCode),
			rl_out__generate_ite(CondCode, ThenCode, ElseCode,
				Code)
		;
			rl_out__generate_join(rl_PROC_join_sm, Output,
				Input1, Input2, Cond, Code)
		)
	).

rl_out__generate_instr(subtract(Output, Input1, Input2, Type, Cond) - _, 
		Code) -->
	rl_out__generate_stream(Input1, Stream1Code),
	rl_out__generate_stream(Input2, Stream2Code),
	rl_out_info_get_output_relation_schema_offset(Output,
		OutputSchemaOffset),
	rl_out__generate_exprn(Cond, OutputSchemaOffset, CondExprn),
	(
		{ Type = nested_loop },
		{ SubtractCode = rl_PROC_subtract_nl }
	;
		{ Type = semi },
		{ SubtractCode = rl_PROC_semisubtract_nl }
	;
		{ Type = sort_merge(_, _) },
		{ SubtractCode = rl_PROC_subtract_sm }
	;
		{ Type = index(_IndexSpec, _) },
		{ error(
		"rl_out__generate_instr: subtract_index not yet implemented") }
	),
	{ InstrCode =
		tree(node([SubtractCode]), 
		tree(Stream1Code, 
		tree(Stream2Code,
		node([rl_PROC_expr(CondExprn)])
	))) },
	rl_out__generate_stream_instruction(Output, InstrCode, Code).
rl_out__generate_instr(difference(Output, Input1, Input2, Type) - _,
		Code) -->
	rl_out__generate_stream(Input1, Stream1Code),
	rl_out__generate_stream(Input2, Stream2Code),
	{ Type = sort_merge(Spec) },
	rl_out_info_get_output_relation_schema(Output, OutputSchema),
	rl_out__generate_compare_exprn(Spec, OutputSchema, CompareExprn),
	{ InstrCode = 
		tree(node([rl_PROC_difference]),
		tree(Stream1Code,
		tree(Stream2Code,
		node([rl_PROC_expr(CompareExprn)])
	))) },
	rl_out__generate_stream_instruction(Output, InstrCode, Code).	
rl_out__generate_instr(project(Output, Input, Cond0,
		OtherOutputs, ProjectType) - _, Code) -->
	rl_out_info_get_output_relation_schema_offset(Output,
		OutputSchemaOffset),

	% If the produced tuple is independent of the input tuple,
	% generate:
	% if (empty(Input)) {
	% 	init(Output);
	% } else
	% 	init(Output);
	%	insert_tuple(Output, Tuple);
	% } 
	%
	% This can happen for tables of facts.
	%
	% Projections of this type are never combined with
	% other projections of the same input relation in the
	% one instruction by rl_block_opt.m.
	(
		{ OtherOutputs = [] },
		{ rl__goal_is_independent_of_input(one, Cond0, Cond) }
	->
		rl_out__generate_exprn(Cond, OutputSchemaOffset, CondExprn),
		rl_out__generate_stream(Input, StreamCode),
		{ CondCode = tree(node([rl_PROC_empty]), StreamCode) },
		rl_out__generate_instr(init(Output) - "", ThenCode),
		{ TupleCode = node([
			rl_PROC_insert_tuple_stream,
			rl_PROC_stream,
			rl_PROC_empty_stream(OutputSchemaOffset),
			rl_PROC_stream_end,
			rl_PROC_expr(CondExprn)
		]) },
		rl_out__generate_stream_instruction(Output, TupleCode,
			ElseCode),
		rl_out__generate_ite(CondCode, ThenCode, ElseCode, Code)
	;
		(
			{ ProjectType = filter },
			rl_out__generate_stream(Input, StreamCode)
		;
			% For an indexed project/select we do a btree_scan
			% to select out the range of tuples we're interested
			% in, then proceed as normal.
			{ ProjectType = index(IndexSpec, Range) },
			{ rl_out__index_spec_to_string(IndexSpec, IndexStr) },
			rl_out_info_get_relation_addr(Input, InputAddr),
			rl_out_info_assign_const(string(IndexStr), IndexConst),
			rl_out__generate_key_range(Range, RangeExprn),
			{ StreamCode = node([
				rl_PROC_stream,
				rl_PROC_btree_scan,
				rl_PROC_indexed_var(InputAddr, 0, IndexConst),
				rl_PROC_expr(RangeExprn),
				rl_PROC_stream_end
			]) }
		),

		rl_out__generate_exprn(Cond0, OutputSchemaOffset, CondExprn),

		%
		% Initialise the other output relations.
		%
		{ assoc_list__keys(OtherOutputs, OtherOutputRels) },
		list__map_foldl(
		    (pred(TheOutput::in, RelInitCode::out, in, out) is det -->
			rl_out__generate_instr(init(TheOutput) - "",
				RelInitCode)
		    ),
		    OtherOutputRels, OtherOutputInitCodeList),
		{ list__foldl(
		    (pred(InitCode::in, Tree0::in, Tree::out) is det :-
			Tree = tree(Tree0, InitCode)
		    ),
		    OtherOutputInitCodeList, empty, OtherOutputInitCode) },

		{ list__map(rl__output_rel_relation,
			OtherOutputRels, OtherOutputRelations) },
		rl_out__get_rel_var_list(OtherOutputRelations, VarListCode),
		list__foldl2(rl_out__generate_project_exprn, OtherOutputs,
			empty, ExprnListCode),
		{ InstrCode = 
			tree(node([rl_PROC_project_tee]), 
			tree(StreamCode, 
			tree(node([rl_PROC_expr(CondExprn)]),
			tree(VarListCode,
			tree(ExprnListCode,
			node([rl_PROC_expr_list_nil])
		))))) },
		rl_out__generate_stream_instruction(Output, InstrCode, Code0),
		{ Code = tree(OtherOutputInitCode, Code0) }
	).
rl_out__generate_instr(union(Output, Inputs, Type) - _, Code) -->
	{ UnionCode = rl_PROC_union_sm },
	{ Type = sort_merge(Spec) },
	rl_out_info_get_output_relation_schema(Output, OutputSchema),
	rl_out__generate_compare_exprn(Spec, OutputSchema, CompareExprn),
	rl_out__generate_union(UnionCode, CompareExprn, Inputs, InstrCode),
	rl_out__generate_stream_instruction(Output, InstrCode, Code).
rl_out__generate_instr(insert(_, _, _, _, _) - _, _) -->
	{ error("rl_out__generate_instr: insert not yet implemented") }.
rl_out__generate_instr(
	    union_diff(UoOutput, DiInput, Input, Diff, Index, CopyInfo) - _,
	    Code) -->
	{ CopyInfo = yes(_) ->
		% This should be removed by rl_liveness.m.
		error("rl_out__generate_instr: copy info on union_diff")
	;
		true
	},
	{ rl_out__index_spec_to_string(Index, IndexStr) },
	rl_out_info_assign_const(string(IndexStr), IndexConst),
	rl_out__generate_stream(Input, StreamCode),
	rl_out_info_get_relation_addr(DiInput, DiInputAddr), 
	rl_out_info_get_relation_addr(UoOutput, UoOutputAddr), 
	{ InstrCode =
		tree(node([
			rl_PROC_uniondiff_btree,
			rl_PROC_indexed_var(DiInputAddr, 0, IndexConst)
		]),
		StreamCode
	) },
	rl_out__generate_stream_instruction(Diff, InstrCode, Code0),
	{ Code =
		tree(Code0,
		node([rl_PROC_setrel(UoOutputAddr, DiInputAddr)])
	) }.
rl_out__generate_instr(sort(Output, Input, Attrs) - _, Code) -->
	rl_out__generate_stream(Input, StreamCode),
	rl_out_info_get_output_relation_schema(Output, OutputSchema),
	rl_out__generate_compare_exprn(attributes(Attrs),
		OutputSchema, CompareExprn),

	% If we are sorting on all attributes we do duplicate removal
	% as well. We shouldn't do this when sorting on a subset of the
	% attributes because the duplicate removal only takes the compared
	% attributes into account.
	{ list__sort_and_remove_dups(Attrs, SortedAttrs) },
	{ list__length(SortedAttrs, NumAttrs) },
	{ list__length(OutputSchema, NumAttrs) ->
		Filter = 1
	;
		Filter = 0
	},
	{ InstrCode = 
		tree(node([rl_PROC_sort(Filter)]), 
		tree(StreamCode, 
		node([rl_PROC_expr(CompareExprn)])
	)) },
	rl_out__generate_stream_instruction(Output, InstrCode, Code).
rl_out__generate_instr(add_index(output_rel(Rel, Indexes)) - _, Code) -->
	rl_out__add_indexes_to_rel(may_have_index, Rel, Indexes, Code).
rl_out__generate_instr(clear(Rel) - _, Code) -->
	rl_out_info_get_relation_addr(Rel, Addr),
	{ Code = node([rl_PROC_clear(Addr)]) }.
rl_out__generate_instr(init(output_rel(Rel, Indexes)) - _, Code) -->
	rl_out_info_get_relation_addr(Rel, Addr),
	rl_out_info_get_relation_schema_offset(Rel, SchemaOffset),
	rl_out__add_indexes_to_rel(does_not_have_index,
		Rel, Indexes, IndexCodes),
	{ Code = 
		tree(node([
			rl_PROC_unsetrel(Addr),
			rl_PROC_createtemprel(Addr, SchemaOffset)
		]),
		IndexCodes
	) }.
rl_out__generate_instr(insert_tuple(Output, Input, Exprn) - _, Code) -->
	rl_out__generate_stream(Input, InputStream),
	rl_out_info_get_output_relation_schema_offset(Output,
		OutputSchemaOffset),
	rl_out__generate_exprn(Exprn, OutputSchemaOffset, ExprnNo),
	{ InstrCode =
		tree(node([rl_PROC_insert_tuple_stream]),
		tree(InputStream,
		node([rl_PROC_expr(ExprnNo)])
	)) },
	rl_out__generate_stream_instruction(Output, InstrCode, Code).
rl_out__generate_instr(unset(Rel) - _, Code) -->
	rl_out_info_get_relation_addr(Rel, Addr),
	{ Code = node([rl_PROC_unsetrel(Addr)]) }.
rl_out__generate_instr(conditional_goto(Cond, Label) - _, Code) -->
	rl_out__generate_goto_cond(Cond, CondCode),
	{ Code = tree(node([rl_PROC_conditional_goto_label(Label)]), 
			CondCode) }.
rl_out__generate_instr(goto(Label) - _, node([rl_PROC_goto_label(Label)])) --> 
	[].
rl_out__generate_instr(label(Label) - _, node([rl_PROC_label(LabelNo)])) -->
	rl_out_info_add_label(Label, LabelNo).
rl_out__generate_instr(ref(OutputRel, InputRel) - _, Code) -->
	rl_out_info_get_relation_type(InputRel, InputType),
	rl_out_info_get_relation_type(OutputRel, OutputType),
	(
		{ InputType = temporary(stream) },
		{ OutputType = temporary(materialised) }
	->
		rl_out__generate_instr(
			copy(output_rel(OutputRel, []), InputRel) - "",
			Code)
	;
		rl_out_info_get_relation_addr(InputRel, InputAddr),
		rl_out_info_get_relation_addr(OutputRel, OutputAddr),
		{ Code = node([rl_PROC_setrel(OutputAddr, InputAddr)]) }
	).
rl_out__generate_instr(copy(OutputRel, InputRel) - _, Code) -->
	% Unfortunately there are internal Aditi reasons why copy
	% must be done as a materialise of each tuple into the new
	% relation rather than just as a copy of the files.
	rl_out_info_get_relation_addr(InputRel, InputAddr),
	{ OutputRel = output_rel(Output, _) },
	rl_out_info_get_relation_addr(Output, OutputAddr),

	% The code for the `init' instruction
	% will also add any necessary indexes.
	rl_out__generate_instr(init(OutputRel) - "", InitCode),

	rl_out_info_get_next_materialise_id(Id),
	{ Code = 
		tree(InitCode,
		node([
			rl_PROC_materialise(Id),
			rl_PROC_stream,
			rl_PROC_var(InputAddr, 0),
			rl_PROC_stream_end,
			rl_PROC_var_list_cons(OutputAddr, 0),
			rl_PROC_var_list_nil
		])
	) }.
rl_out__generate_instr(make_unique(OutputRel, Input) - Comment, Code) -->
	% 	if (one_reference(InputRel)) {
	% 		OutputRel = ref(InputRel)
	% 	} else {
	% 		OutputRel = copy(InputRel)
	%	}
	rl_out_info_get_relation_addr(Input, InputAddr),
	{ CondCode = node([rl_PROC_one_reference(InputAddr)]) },

	{ OutputRel = output_rel(Output, _) },
	rl_out__generate_instr(ref(Output, Input) - Comment, ThenCode0),
	% We may not need to generate this instruction - rl_sort.m
	% has enough information to work out whether this is actually needed.
	rl_out__generate_instr(add_index(OutputRel) - Comment, ThenCode1),
	{ ThenCode = tree(ThenCode0, ThenCode1) },

	rl_out__generate_instr(copy(OutputRel, Input) - Comment, ElseCode),

	rl_out__generate_ite(CondCode, ThenCode, ElseCode, Code).
rl_out__generate_instr(call(ProcName, Inputs, OutputRels, SaveRels) - _,
		Code) -->

	rl_out__save_input_args(Inputs, NewInputs, SaveRels,
		SaveTmpVars, SaveCode),
	{ list__map(rl__output_rel_relation, OutputRels, Outputs) },
	rl_out__handle_overlapping_args(Outputs, NewOutputs, Inputs,
		OverlapTmpVars, OverlapCode),

	{ list__append(NewInputs, NewOutputs, CallArgs) },
	{ list__map(lambda([Arg::in, ArgCode::out] is det, (
		ArgCode = rl_PROC_var_list_cons(Arg, 0)
	)), CallArgs, CallArgCodes) },
	{ rl__proc_name_to_string(ProcName, ProcNameStr) },
	rl_out_info_assign_const(string(ProcNameStr), ProcNameConst),
	rl_out_info_return_tmp_vars(SaveTmpVars, SaveClearCode),
	rl_out_info_return_tmp_vars(OverlapTmpVars, OverlapClearCode),
	rl_out__add_indexes_to_rels(does_not_have_index,
		OutputRels, IndexCode),
	{ Code =
		tree(SaveCode,
		tree(node([rl_PROC_call(ProcNameConst)]), 
		tree(node(CallArgCodes), 
		tree(node([rl_PROC_var_list_nil]),
		tree(OverlapCode,
		tree(OverlapClearCode,
		tree(SaveClearCode,
		IndexCode
	))))))) }.
rl_out__generate_instr(aggregate(Output, Input,
		ComputeInitial, UpdateAcc) - _, Code) -->
	rl_out__generate_stream(Input, InputCode),
	rl_out__generate_aggregate_exprn(ComputeInitial, UpdateAcc,
		Input, Output, AggExprn),
	{ InstrCode = 
		tree(node([rl_PROC_aggregate]), 
		tree(InputCode, 
		node([rl_PROC_expr(AggExprn)])
	)) },
	rl_out__generate_stream_instruction(Output, InstrCode, Code).
rl_out__generate_instr(comment - _, empty) --> [].

%-----------------------------------------------------------------------------%

:- pred rl_out__generate_join(bytecode::in, output_rel::in,
		relation_id::in, relation_id::in, rl_goal::in,
		byte_tree::out, rl_out_info::in, rl_out_info::out) is det.

rl_out__generate_join(JoinCode, Output, Input1, Input2, Cond, Code) -->
	rl_out_info_get_output_relation_schema_offset(Output,
		OutputSchemaOffset),
	rl_out__generate_stream(Input1, Stream1Code),
	rl_out__generate_stream(Input2, Stream2Code),
	rl_out__generate_exprn(Cond, OutputSchemaOffset, CondExprn),
	{ InstrCode =
		tree(node([JoinCode]), 
		tree(Stream1Code, 
		tree(Stream2Code, 
		node([rl_PROC_expr(CondExprn)])
	))) },
	rl_out__generate_stream_instruction(Output, InstrCode, Code).

%-----------------------------------------------------------------------------%

	% Copy any arguments which are needed again later to a temporary
	% location. The called procedure can then do what it likes to
	% the new variable (except changing the contents of the relation
	% it points to on entry).
:- pred rl_out__save_input_args(list(relation_id)::in, list(int)::out,
	set(relation_id)::in, assoc_list(int, int)::out, byte_tree::out,
	rl_out_info::in, rl_out_info::out) is det.

rl_out__save_input_args([], [], _, [], empty) --> [].
rl_out__save_input_args([Input | Inputs], [InputAddr | InputAddrs],
		SavedInputs, TmpVars, SaveCode) -->
	rl_out__save_input_args(Inputs, InputAddrs, SavedInputs,
		TmpVars1, SaveCode1),
	rl_out_info_get_relation_schema_offset(Input, InputSchemaOffset),
	rl_out_info_get_relation_addr(Input, OldInputAddr),
	( { set__member(Input, SavedInputs) } ->
		rl_out_info_get_tmp_var(InputSchemaOffset, InputAddr),
		{ SaveCode =
			tree(node([rl_PROC_setrel(InputAddr, OldInputAddr)]),
			SaveCode1
		) },
		{ TmpVars = [InputSchemaOffset - InputAddr | TmpVars1] }
	;
		{ InputAddr = OldInputAddr },
		{ SaveCode = SaveCode1 },
		{ TmpVars = TmpVars1 }
	).

	% Where input and output relations overlap, put the overlapping
	% outputs in new temporaries, then copy them over the inputs 
	% after the call. This should be very rarely needed, if at all.
:- pred rl_out__handle_overlapping_args(list(relation_id)::in, list(int)::out,
	list(relation_id)::in, assoc_list(int, int)::out, 
	byte_tree::out, rl_out_info::in, rl_out_info::out) is det.

rl_out__handle_overlapping_args([], [], _, [], empty) --> [].
rl_out__handle_overlapping_args([Output | Outputs], [NewOutput | NewOutputs],
		Inputs, TmpVars, OverlapCode) -->
	rl_out__handle_overlapping_args(Outputs, NewOutputs, Inputs, 
		TmpVars0, OverlapCode0),
	rl_out_info_get_relation_addr(Output, OutputAddr),
	( { list__member(Output, Inputs) } ->
		rl_out_info_get_relation_schema_offset(Output,
			OutputSchemaOffset),
		rl_out_info_add_relation_variable(OutputSchemaOffset,
			NewOutput),
		{ OverlapCode1 = 
			node([
				rl_PROC_setrel(OutputAddr, NewOutput),
				rl_PROC_unsetrel(NewOutput)
			]) },
		{ OverlapCode = tree(OverlapCode0, OverlapCode1) },
		{ TmpVars = [OutputSchemaOffset - NewOutput | TmpVars0] }
	;
		{ NewOutput = OutputAddr },
		{ OverlapCode = OverlapCode0 },
		{ TmpVars = TmpVars0 }
	).

%-----------------------------------------------------------------------------%

:- pred rl_out__generate_project_exprn(pair(output_rel, rl_goal)::in,
		byte_tree::in, byte_tree::out,
		rl_out_info::in, rl_out_info::out) is det.

rl_out__generate_project_exprn(Output - Exprn,
		ExprnListCode0, ExprnListCode) -->
	rl_out_info_get_output_relation_schema_offset(Output,
		OutputSchemaOffset),
	rl_out__generate_exprn(Exprn, OutputSchemaOffset, ExprnNum),
	{ ExprnListCode = tree(ExprnListCode0,
				node([rl_PROC_expr_list_cons(ExprnNum)])) }.
	
%-----------------------------------------------------------------------------%

:- pred rl_out__generate_goto_cond(goto_cond::in, byte_tree::out,
	rl_out_info::in, rl_out_info::out) is det.

rl_out__generate_goto_cond(empty(RelationId), Code) -->
	rl_out_info_get_relation_addr(RelationId, RelationAddr),
	{ LockSpec = 0 },	% get default lock spec.
	{ Code = node([
		rl_PROC_empty, 
		rl_PROC_stream, 
		rl_PROC_var(RelationAddr, LockSpec), 
		rl_PROC_stream_end
	]) }.
rl_out__generate_goto_cond(and(Cond1, Cond2), Code) -->
	rl_out__generate_goto_cond(Cond1, Code1),
	rl_out__generate_goto_cond(Cond2, Code2),
	{ Code = 
		tree(node([rl_PROC_and]), 
		tree(Code1, 
		Code2)
	) }.
rl_out__generate_goto_cond(or(Cond1, Cond2), Code) -->
	rl_out__generate_goto_cond(Cond1, Code1),
	rl_out__generate_goto_cond(Cond2, Code2),
	{ Code = 
		tree(node([rl_PROC_or]), 
		tree(Code1, 
		Code2
	)) }.
rl_out__generate_goto_cond(not(Cond), Code) -->
	rl_out__generate_goto_cond(Cond, Code1),
	{ Code = 
		tree(node([rl_PROC_not]), 
		Code1
	) }.
			
%-----------------------------------------------------------------------------%

	% Generate an if-then-else in the bytecode. This is used to handle
	% some trivial cases where we didn't want to introduce the extra
	% branching in the code earlier to avoid inhibiting other
	% optimizations.
:- pred rl_out__generate_ite(byte_tree::in, byte_tree::in, byte_tree::in,
		byte_tree::out, rl_out_info::in, rl_out_info::out) is det.

rl_out__generate_ite(CondCode, ThenCode, ElseCode, IteCode) -->
	rl_out_info_add_label(GotoLabel),
	rl_out_info_add_label(EndLabel),
	{ IteCode =
		tree(node([
			rl_PROC_conditional_goto(GotoLabel)
		]),
		tree(CondCode,
		tree(ElseCode,
		tree(node([
			rl_PROC_goto(EndLabel),
			rl_PROC_label(GotoLabel)
		]),
		tree(ThenCode,
		node([rl_PROC_label(EndLabel)])
	))))) }.

%-----------------------------------------------------------------------------%

:- pred rl_out__index_spec_to_string(index_spec::in, string::out) is det.

rl_out__index_spec_to_string(index_spec(Type, Attrs), IndexString) :-
	( 
		Type = unique_B_tree,
		TypeStr = "bu"
	;
		Type = non_unique_B_tree,
		TypeStr = "bn"
	),
	( Attrs = [FirstAttr | OtherAttrs] ->
		rl_out__index_attrs_to_string(FirstAttr,
			OtherAttrs, "", AttrString)
	;
		error("rl_out__index_spec_to_string: no indexed attributes")
	),
	string__append_list([TypeStr, "(", AttrString, ")"], IndexString).

:- pred rl_out__index_attrs_to_string(int::in, list(int)::in,
		string::in, string::out) is det.
	
rl_out__index_attrs_to_string(Attr, [], Str0, Str) :-
	rl_out__index_attr_to_string(Attr, Str1),
	string__append(Str0, Str1, Str).
rl_out__index_attrs_to_string(Attr1, [Attr2 | Attrs], Str0, Str) :-
	rl_out__index_attr_to_string(Attr1, Str1),
	string__append_list([Str0, Str1, ", "], Str2),
	rl_out__index_attrs_to_string(Attr2, Attrs, Str2, Str).

:- pred rl_out__index_attr_to_string(int::in, string::out) is det.

rl_out__index_attr_to_string(Attr, Str) :-
	string__int_to_string(Attr, AttrStr),
	string__append("#:", AttrStr, Str).

%-----------------------------------------------------------------------------%

:- type check_index
	--->	may_have_index
	;	does_not_have_index
	.

:- pred rl_out__add_indexes_to_rels(check_index::in,
		list(output_rel)::in, byte_tree::out,
		rl_out_info::in, rl_out_info::out) is det.
	
rl_out__add_indexes_to_rels(_, [], empty) --> [].
rl_out__add_indexes_to_rels(CheckIndex,
		[output_rel(Output, Indexes) | Outputs], IndexCode) -->
	rl_out__add_indexes_to_rel(CheckIndex, Output, Indexes, IndexCode0),
	rl_out__add_indexes_to_rels(CheckIndex, Outputs, IndexCode1),
	{ IndexCode = tree(IndexCode0, IndexCode1) }.

:- pred rl_out__add_indexes_to_rel(check_index::in, relation_id::in,
		list(index_spec)::in, byte_tree::out,
		rl_out_info::in, rl_out_info::out) is det.

rl_out__add_indexes_to_rel(_, _, [], empty) --> [].
rl_out__add_indexes_to_rel(CheckIndex, Output,
		[Index | Indexes], IndexCode) -->
	rl_out_info_get_relation_addr(Output, OutputAddr),
	{ rl_out__index_spec_to_string(Index, IndexStr) },
	rl_out_info_assign_const(string(IndexStr), IndexConst),

	(
		{ CheckIndex = may_have_index },
		% Generate code to test whether the index already exists
		% before adding it.
		{ CondCode = node([
			rl_PROC_has_index(OutputAddr, IndexConst)
		]) },
		{ ThenCode = empty },
		{ ElseCode = node([
			rl_PROC_addindextorel(OutputAddr, IndexConst)
		]) },
		rl_out__generate_ite(CondCode, ThenCode, ElseCode, IndexCode0)
	;
		{ CheckIndex = does_not_have_index },
		{ IndexCode0 = node([
			rl_PROC_addindextorel(OutputAddr, IndexConst)
		]) }
	),
	rl_out__add_indexes_to_rel(CheckIndex,
		OutputAddr, Indexes, IndexCode1),
	{ IndexCode = tree(IndexCode0, IndexCode1) }.

%-----------------------------------------------------------------------------%

	% Generate code to handle an instruction that could return a
	% stream, either by binding the stream to a variable to be
	% evaulated later or materialising it into a relation variable. 
:- pred rl_out__generate_stream_instruction(output_rel::in, byte_tree::in,
		byte_tree::out, rl_out_info::in, rl_out_info::out) is det.

rl_out__generate_stream_instruction(output_rel(Output, Indexes),
		InstrCode, Code) -->
	rl_out_info_get_relation_addr(Output, OutputAddr),
	rl_out_info_get_relation_type(Output, RelType),
	{ Stream =
		tree(node([rl_PROC_stream]),
		tree(InstrCode,
		node([rl_PROC_stream_end])
	)) },

	( { RelType = temporary(stream) } ->
		{ Code = 
			tree(node([
				rl_PROC_unsetrel(OutputAddr),
				rl_PROC_bind_handle(OutputAddr)
			]),
			Stream
		) }
	;
		rl_out_info_get_relation_schema_offset(Output, SchemaOffset),
		rl_out_info_get_tmp_var(SchemaOffset, TmpVar),
		rl_out_info_return_tmp_var(SchemaOffset,
			TmpVar, TmpClearCode),

		{ LockSpec = 0 },	% default lock spec
		rl_out__add_indexes_to_rel(does_not_have_index,
			Output, Indexes, IndexInstrs),
		rl_out_info_get_next_materialise_id(Id),
		{ Code = 
			tree(node([
				rl_PROC_createtemprel(TmpVar, SchemaOffset)
			]),
			tree(IndexInstrs,
			tree(node([
				rl_PROC_materialise(Id)
			]),
			tree(Stream,
			tree(node([
				rl_PROC_var_list_cons(TmpVar, LockSpec),
				rl_PROC_var_list_nil,

				% This unsetrel must come after the code
				% to materialise the stream, since the stream
				% may depend on the variable.
				rl_PROC_unsetrel(OutputAddr),
				rl_PROC_setrel(OutputAddr, TmpVar)
			]),
			TmpClearCode
		))))) }
	).

%-----------------------------------------------------------------------------%

:- pred rl_out__generate_stream_list(list(relation_id)::in, byte_tree::out,
		rl_out_info::in, rl_out_info::out) is det.

rl_out__generate_stream_list(Rels, Code) -->
	rl_out__generate_stream_list_2(Rels, Code1),
	{ Code = 
		tree(node([rl_PROC_stream_list_cons]), 
		tree(Code1, 
		node([rl_PROC_stream_list_nil])
	)) }.

:- pred rl_out__generate_stream_list_2(list(relation_id)::in, byte_tree::out,
		rl_out_info::in, rl_out_info::out) is det.

rl_out__generate_stream_list_2([], empty) --> [].
rl_out__generate_stream_list_2([Rel | Rels], Code) -->
	rl_out__generate_stream(Rel, Code1),
	rl_out__generate_stream_list_2(Rels, Code2),
	{ Code = tree(Code1, Code2) }.

:- pred rl_out__generate_stream(relation_id::in, byte_tree::out,
		rl_out_info::in, rl_out_info::out) is det.

rl_out__generate_stream(Rel, StreamCode) -->
	rl_out_info_get_relation_addr(Rel, Addr),
	{ LockSpec = 0 },	% get default lock spec
	{ StreamCode = node([
		rl_PROC_stream, 
		rl_PROC_var(Addr, LockSpec),
		rl_PROC_stream_end
	]) }.

%-----------------------------------------------------------------------------%

	% Generate a binary tree of unions. 
:- pred rl_out__generate_union(bytecode::in, int::in,
		list(relation_id)::in, byte_tree::out, 
		rl_out_info::in, rl_out_info::out) is det.

rl_out__generate_union(UnionCode, Exprn, Inputs, InstrCode) -->
	( { Inputs = [] } ->
		{ error("rl_out__generate_union: no inputs") }
	; { Inputs = [Input] } ->
		rl_out_info_get_relation_addr(Input, Addr),
		{ LockSpec = 0 },	% get default lock spec
		{ InstrCode = node([rl_PROC_var(Addr, LockSpec)]) }
	;		
		{ list__length(Inputs, NumInputs) },
		{ SplitPoint is NumInputs // 2 },
		( { list__split_list(SplitPoint, Inputs, Inputs1, Inputs2) } ->
			rl_out__generate_union(UnionCode, Exprn, 
				Inputs1, StreamCode1),
			rl_out__generate_union(UnionCode, Exprn, 
				Inputs2, StreamCode2)
		;
			{ error("rl_out__generate_union: list__split_list failed") }
		),
		{ InstrCode = 
			tree(node([UnionCode, rl_PROC_stream]), 
			tree(StreamCode1,
			tree(node([rl_PROC_stream_end, rl_PROC_stream]),
			tree(StreamCode2,
			node([rl_PROC_stream_end, rl_PROC_expr(Exprn)])
		)))) }
	).

%-----------------------------------------------------------------------------%

:- pred rl_out__generate_arg_list(list(int)::in, byte_tree::out) is det.

rl_out__generate_arg_list(List, Code) :-
	ConsElem = lambda([Elem::in, ArgCode::out] is det, (
			LockSpec = 0, % default lock spec.
			ArgCode = rl_PROC_var(Elem, LockSpec)
		)),
	list__map(ConsElem, List, Codes),
	Code = node(Codes).

%-----------------------------------------------------------------------------%

:- pred rl_out__generate_int_list(list(int)::in, byte_tree::out) is det.

rl_out__generate_int_list(List, Code) :-
	ConsElem = lambda([Elem::in, Cons::out] is det, (
			Cons = rl_PROC_int_list_cons(Elem)
		)),
	list__map(ConsElem, List, Codes0),
	list__append(Codes0, [rl_PROC_int_list_nil], Codes),
	Code = node(Codes).

%-----------------------------------------------------------------------------%

:- pred rl_out__resolve_proc_addresses(byte_tree::in, byte_tree::out,
		rl_out_info::in, rl_out_info::out) is det.

rl_out__resolve_proc_addresses(ByteTree0, ByteTree) -->
	rl_out_info_get_labels(Labels),
	{ ResolveAddr = 
		lambda([Code0::in, Code::out] is det, (
			%
			% The actual code addresses of rl_PROC_goto_label
			% are resolved at runtime, we possibly could resolve
			% them here and use rl_PROC_goto instead.
			%
			( Code0 = rl_PROC_goto_label(Label0) ->
				maybe_lookup(Labels, Label0, Label),
				Code = rl_PROC_goto_label(Label)
			; Code0 = rl_PROC_conditional_goto_label(Label0) ->
				maybe_lookup(Labels, Label0, Label),
				Code = rl_PROC_conditional_goto_label(Label)
			%
			% rl_PROC_goto and rl_PROC_conditional_goto are
			% used by Aditi for resolved label addresses. We
			% use them here for labels which don't need renaming.
			%
			; Code0 = rl_PROC_goto(Label0) ->
				Code = rl_PROC_goto_label(Label0)
			; Code0 = rl_PROC_conditional_goto(Label0) ->
				Code = rl_PROC_conditional_goto_label(Label0)
			;
				Code = Code0
			)
		)) },
	{ rl_out__resolve_addresses(ResolveAddr, ByteTree0, ByteTree) }.

	% Labels introduced as optimizations in rl_out.m don't
	% need to be resolved.
:- pred maybe_lookup(map(K, K)::in, K::in, K::out) is det.

maybe_lookup(Map, K0, K) :-
	( map__search(Map, K0, K1) ->
		K = K1
	;
		K = K0
	).

rl_out__resolve_addresses(_, empty, empty).
rl_out__resolve_addresses(ResolveAddr, node(InstrList0), node(InstrList)) :-
	list__map(ResolveAddr, InstrList0, InstrList).
rl_out__resolve_addresses(ResolveAddr, tree(CodeA0, CodeB0), 
		tree(CodeA, CodeB)) :-
	rl_out__resolve_addresses(ResolveAddr, CodeA0, CodeA),
	rl_out__resolve_addresses(ResolveAddr, CodeB0, CodeB).

%-----------------------------------------------------------------------------%

:- pred rl_out__instr_code_size(byte_tree::in, int::out) is det.

rl_out__instr_code_size(empty, 0).
rl_out__instr_code_size(node(Instrs), Size) :-
	AddSize = lambda([Instr::in, S0::in, S::out] is det, (
			bytecode_to_intlist(Instr, IntList),
			list__length(IntList, S1),
			S is S0 + S1
		)),
	list__foldl(AddSize, Instrs, 0, Size).
rl_out__instr_code_size(tree(CodeA, CodeB), Size) :-
	rl_out__instr_code_size(CodeA, SizeA),
	rl_out__instr_code_size(CodeB, SizeB),
	Size is SizeA + SizeB.

%-----------------------------------------------------------------------------%

	% Generate a general join/project condition.
:- pred rl_out__generate_exprn(rl_goal::in, int::in, int::out,
		rl_out_info::in, rl_out_info::out) is det.

rl_out__generate_exprn(RLGoal, OutputSchemaOffset, ExprnNum) -->

	rl_out_info_get_module_info(ModuleInfo),
	{ rl_exprn__generate(ModuleInfo, RLGoal, ExprnCode,
		NumParams, ExprnMode, Decls) },

	rl_out__schema_to_string([], EmptySchemaOffset),
	% Nothing is built on the stack, so this will be enough.
	{ StackSize = 10 },
	rl_out__package_exprn(ExprnCode, NumParams, ExprnMode,
		OutputSchemaOffset, EmptySchemaOffset, StackSize,
		Decls, ExprnNum).

:- pred rl_out__generate_aggregate_exprn(pred_proc_id::in,
		pred_proc_id::in, relation_id::in, output_rel::in,
		int::out, rl_out_info::in, rl_out_info::out) is det.

rl_out__generate_aggregate_exprn(ComputeInitial, UpdateAcc,
		Input, Output, ExprnNum) -->
	rl_out_info_get_relation_schema(Input, InputSchema),
	rl_out_info_get_output_relation_schema(Output, OutputSchema),
	rl_out__schema_to_string(OutputSchema, OutputSchemaOffset),
	(
		{ InputSchema = [GrpByType, NonGrpByType] },
		{ OutputSchema = [_, AccType] }
	->
		rl_out_info_get_module_info(ModuleInfo),
		{ rl_exprn__aggregate(ModuleInfo, ComputeInitial, UpdateAcc,
			GrpByType, NonGrpByType, AccType, AggCode, Decls) },
		rl_out__schema_to_string([], EmptySchemaOffset),

		% Nothing is built on the stack, so this will be enough.
		{ StackSize = 10 },
		{ NumParams = 1 },
		rl_out__package_exprn(AggCode, NumParams, generate,
			OutputSchemaOffset, EmptySchemaOffset,
			StackSize, Decls, ExprnNum)
	;
		{ error(
		"rl_out__generate_aggregate_exprn: invalid relation schemas") }
	).

	% Generate an expression to compare tuples with the
	% given schema on the given attributes.
:- pred rl_out__generate_compare_exprn(sort_spec::in, list(type)::in,
		int::out, rl_out_info::in, rl_out_info::out) is det.

rl_out__generate_compare_exprn(Spec, Schema, ExprnNum) -->
	rl_out_info_get_compare_exprns(CompareExprns0),
	rl_out__schema_to_string(Schema, InputSchemaOffset),
	(
		{ map__search(CompareExprns0, Spec - InputSchemaOffset,
			ExprnNum0) }
	->
		{ ExprnNum = ExprnNum0 }
	;
		rl_out_info_get_module_info(ModuleInfo),
		{ rl_exprn__generate_compare_exprn(ModuleInfo, Spec,
			Schema, Instrs) },

		% Comparison expressions don't use any variables
		% or create an output tuple.
		rl_out__schema_to_string([], EmptySchemaOffset),

		% Nothing is built on the stack, so this will be enough.
		{ StackSize = 10 },
		{ Decls = [] },
		rl_out__package_exprn(Instrs, 2, test, EmptySchemaOffset,
			EmptySchemaOffset, StackSize, Decls, ExprnNum),

		{ map__det_insert(CompareExprns0, Spec - InputSchemaOffset,
			ExprnNum, CompareExprns) },
		rl_out_info_set_compare_exprns(CompareExprns)
	).

:- pred rl_out__generate_key_range(key_range::in, int::out,
		rl_out_info::in, rl_out_info::out) is det.

rl_out__generate_key_range(Range, RangeExprn) -->
	rl_out_info_get_module_info(ModuleInfo),
	{ rl_exprn__generate_key_range(ModuleInfo, Range, ExprnCode,
		NumParams, Output1Schema, Output2Schema, TermDepth, Decls) },
	rl_out__schema_to_string(Output1Schema, Output1SchemaOffset),
	rl_out__schema_to_string(Output2Schema, Output2SchemaOffset),

	% Terms take 2 slots in the stack, so to be safe we
	% multiply the depth by 2. The +10 is for temporary storage
	% and probably isn't used.
	{ StackSize is TermDepth * 2 + 10 },
	rl_out__package_exprn(ExprnCode, NumParams, generate2,
		Output1SchemaOffset, Output2SchemaOffset, StackSize,
		Decls, RangeExprn).
	
:- pred rl_out__package_exprn(list(bytecode)::in, int::in, exprn_mode::in,
		int::in, int::in, int::in, list(type)::in, int::out,
		rl_out_info::in, rl_out_info::out) is det.

rl_out__package_exprn(ExprnCode, NumParams, ExprnMode, OutputSchemaOffset,
		Schema2Offset, StackSize, Decls, ExprnNum) -->
	rl_out__schema_to_string(Decls, VarSchemaOffset),

	% Note that this field is for the benefit of ROSI which counts
	% everything in bytes, so we don't use rl_out__exprn_code_size.
	{ rl_out__instr_code_size(node(ExprnCode), CodeSize) },

	{ Exprnession = expression(OutputSchemaOffset, Schema2Offset, 
		VarSchemaOffset, StackSize, NumParams, ExprnMode,
		CodeSize, ExprnCode) },

	rl_out_info_add_expression(Exprnession, ExprnNum).

%-----------------------------------------------------------------------------%

:- type rl_out_info
	---> rl_out_info(
		int,				% PC
		compare_exprns,
		map(relation_id, int),		% relation vars
		int,				% next relation address
		map(relation_id, relation_info),
		map(label_id, int),		% proc label offsets
		unit,
		module_info,
		int,				% expression PC
		map(rl_const, int),		% procedure consts
		int,				% next proc const address
		int,				% next materialise number -
						% used for debugging the
						% generated code.
		unit,
		unit,
		unit,
		int,				% next proc label.
		list(procedure),		% procedure bytecodes
						% in reverse order.
		unit,
		unit,
		unit,
		set(relation),			% permanent relations.
		list(variable),			% variables used in
						% reverse order.
		list(expression),		% expressions for the current
						% procedure in reverse order.
		int,				% next expression.
		multi_map(int, int)		% temporary relation variables:
						% map from schema constant
						% to list of variables.
						% These must only be used 
						% within one rl.m instruction.
	).

	% We only want to generate a single comparison expression for
	% each combination of attributes and types.
	% Key:
	% 	The int gives the offset of the schema of the input relation
	% 	in the constant table.
	% Value:
	% 	The number of the expression.
:- type compare_exprns == map(pair(sort_spec, int), int).

%-----------------------------------------------------------------------------%

:- pred rl_out_info_init(module_info::in, rl_out_info::out) is det.

rl_out_info_init(ModuleInfo, Info0) :-
	map__init(CompareExprns),
	map__init(Relations),
	map__init(RelationAddrs),
	map__init(Consts),
	map__init(Labels),
	set__init(PermRels),
	map__init(TmpVars),
	PC = 0,
	FirstRelAddr = 0,
	FirstConst = 1,
	FirstMaterialise = 1,
	Label = 0,
	NextExprn = 0,
	Info0 = rl_out_info(PC, CompareExprns, RelationAddrs, FirstRelAddr, 
		Relations, Labels, unit, ModuleInfo, PC, Consts, 
		FirstConst, FirstMaterialise, unit, unit, unit, Label, 
		[], unit, unit, unit, PermRels, [], [], 
		NextExprn, TmpVars).

:- pred rl_out_info_init_proc(map(relation_id, relation_info)::in,
	list(relation_id)::in, rl_out_info::in, rl_out_info::out) is det.

rl_out_info_init_proc(Relations, _Args, Info0, Info) :-
	map__init(Labels),
	map__init(RelationAddrs),
	map__init(CompareExprns),
	PC = 0,
	Label = 0,
	NextExprn = 0,
	map__init(TmpVars),
	Info0 = rl_out_info(_, _, _, NextAddr, _, _, _, 
		ModuleInfo, _, ProcConsts, NextConst, Materialise, _, _,
		_, _, Procs, _, _, _, PermRelations, Variables, _, _, _),
	Info = rl_out_info(PC, CompareExprns, RelationAddrs, NextAddr,
		Relations, Labels, unit, ModuleInfo, PC, ProcConsts,
		NextConst, Materialise, unit, unit, unit, Label, Procs,
		unit, unit, unit, PermRelations, Variables, [], 
		NextExprn, TmpVars).

%-----------------------------------------------------------------------------%

:- pred rl_out_info_get_compare_exprns(compare_exprns::out,
		rl_out_info::in, rl_out_info::out) is det.

rl_out_info_get_compare_exprns(Exprns, Info, Info) :-
	Info = rl_out_info(_,Exprns,_,_,_,_,_,_,_,_,_,_,_,_,_,_,
			_,_,_,_,_,_,_,_,_).

:- pred rl_out_info_set_compare_exprns(compare_exprns::in,
		rl_out_info::in, rl_out_info::out) is det.

rl_out_info_set_compare_exprns(Exprns, Info0, Info) :-
	Info0 = rl_out_info(A,_,C,D,E,F,G,H,I,J,K,L,M,
			N,O,P,Q,R,S,T,U,V,W,X,Y),
	Info = rl_out_info(A,Exprns,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,
			V,W,X,Y).

%-----------------------------------------------------------------------------%

:- pred rl_out_info_get_relation_addr(relation_id::in, int::out,
		rl_out_info::in, rl_out_info::out) is det.

rl_out_info_get_relation_addr(RelationId, Addr) -->
	rl_out_info_get_relation_addrs(Addrs0),
	( { map__search(Addrs0, RelationId, Addr0) } ->
		{ Addr = Addr0 }
	;
		rl_out_info_get_relation_schema_offset(RelationId,
			SchemaAddr),
		rl_out_info_add_relation_variable(SchemaAddr, Addr),
		{ map__det_insert(Addrs0, RelationId, Addr, Addrs) },
		rl_out_info_set_relation_addrs(Addrs)
	).

:- pred rl_out_info_get_relation_addrs(map(relation_id, int)::out,
		rl_out_info::in, rl_out_info::out) is det.

rl_out_info_get_relation_addrs(Addrs, Info, Info) :-
	Info = rl_out_info(_,_,Addrs,_,_,_,_,_,_,_,_,_,_,_,_,_,
			_,_,_,_,_,_,_,_,_).

:- pred rl_out_info_set_relation_addrs(map(relation_id, int)::in, 
		rl_out_info::in, rl_out_info::out) is det.

rl_out_info_set_relation_addrs(Addrs, Info0, Info) :-
	Info0 = rl_out_info(A,B,_,D,E,F,G,H,I,J,K,L,M,
			N,O,P,Q,R,S,T,U,V,W,X,Y),
	Info = rl_out_info(A,B,Addrs,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,
			V,W,X,Y).

:- pred rl_out_info_add_relation_variable(int::in, int::out,
		rl_out_info::in, rl_out_info::out) is det.
		
rl_out_info_add_relation_variable(Schema, Addr) -->
	rl_out_info_get_next_relation_addr(Addr),
	{ string__int_to_string(Addr, AddrStr) },
	{ string__append("v_", AddrStr, VarName) },
	rl_out_info_assign_const(string(VarName), VarNameConst),
	rl_out_info_add_relation_variable_2(VarNameConst, Schema).

:- pred rl_out_info_add_relation_variable_2(int::in, int::in, 
		rl_out_info::in, rl_out_info::out) is det.

rl_out_info_add_relation_variable_2(Name, Schema, Info0, Info) :-
	Info0 = rl_out_info(A,B,C,D,E,F,G,H,I,J,K,L,M,
			N,O,P,Q,R,S,T,U,Vars0,W,X,Y),
	Info = rl_out_info(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,
			[variable(Name, Schema) | Vars0], W,X,Y).

:- pred rl_out_info_get_next_relation_addr(int::out, 
		rl_out_info::in, rl_out_info::out) is det.

rl_out_info_get_next_relation_addr(NextAddr0, Info0, Info) :-
	Info0 = rl_out_info(A,B,C,NextAddr0,E,F,G,H,I,J,K,L,M,
			N,O,P,Q,R,S,T,U,V,W,X,Y),
	NextAddr is NextAddr0 + 1,
	Info = rl_out_info(A,B,C,NextAddr,E,F,G,H,
			I,J,K,L,M, N,O,P,Q,R,S,T,U,V,W,X,Y).

:- pred rl_out_info_get_relation_variables(list(variable)::out,
		rl_out_info::in, rl_out_info::out) is det.

rl_out_info_get_relation_variables(Vars, Info, Info) :-
	Info = rl_out_info(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,
			Vars0,_,_,_),
	list__reverse(Vars0, Vars).

%-----------------------------------------------------------------------------%

:- pred rl_out_info_add_label(label_id::in, int::out,
		rl_out_info::in, rl_out_info::out) is det.

rl_out_info_add_label(LabelId, NextLabel, Info0, Info) :-
	Info0 = rl_out_info(A,B,C,D,E,Labels0,G,H,I,J,K,L,M,N,O,NextLabel,
			Q,R,S,T,U,V,W,X,Y),
	map__det_insert(Labels0, LabelId, NextLabel, Labels),
	NextLabel1 is NextLabel + 1,
	Info = rl_out_info(A,B,C,D,E,Labels,G,H,I,J,K,L,M,N,O,NextLabel1,
			Q,R,S,T,U,V,W,X,Y).

:- pred rl_out_info_add_label(int::out,
		rl_out_info::in, rl_out_info::out) is det.

rl_out_info_add_label(NextLabel, Info0, Info) :-
	Info0 = rl_out_info(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,NextLabel,
			Q,R,S,T,U,V,W,X,Y),
	NextLabel1 is NextLabel + 1,
	Info = rl_out_info(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,NextLabel1,
			Q,R,S,T,U,V,W,X,Y).

:- pred rl_out_info_get_labels(map(label_id, int)::out,
		rl_out_info::in, rl_out_info::out) is det.

rl_out_info_get_labels(Labels, Info, Info) :-
	Info = rl_out_info(_,_,_,_,_,Labels,_,_,_,_,_,_,_,_,_,_,_,_,
			_,_,_,_,_,_,_).

%-----------------------------------------------------------------------------%

:- pred rl_out_info_get_module_info(module_info::out, rl_out_info::in,
		rl_out_info::out) is det.

rl_out_info_get_module_info(ModuleInfo, Info, Info) :-
	Info = rl_out_info(_,_,_,_,_,_,_,ModuleInfo,
			_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_).

%-----------------------------------------------------------------------------%

:- pred rl_out_info_assign_const(rl_const::in, int::out,
		rl_out_info::in, rl_out_info::out) is det.

rl_out_info_assign_const(Const, ConstOffset, Info0, Info) :-
	Info0 = rl_out_info(A,B,C,D,E,F,G,H,I,Consts0,NextAddr0,
			L,M,N,O,P,Q,R,S,T,U,V,W,X,Y),
	( map__search(Consts0, Const, Addr1) ->
		ConstOffset = Addr1,
		NextAddr = NextAddr0,
		Consts = Consts0
	;
		map__det_insert(Consts0, Const, NextAddr0, Consts),
		ConstOffset = NextAddr0,
		NextAddr is NextAddr0 + 1
	),
	Info = rl_out_info(A,B,C,D,E,F,G,H,I,Consts,NextAddr,
			L,M,N,O,P,Q,R,S,T,U,V,W,X,Y).

:- pred rl_out_info_get_consts(map(rl_const, int)::out, 
		rl_out_info::in,
		rl_out_info::out) is det.

rl_out_info_get_consts(Consts, Info, Info) :-
	Info = rl_out_info(_,_,_,_,_,_,_,_,_,Consts,
			_,_,_,_,_,_,_,_,_,_,_,_,_,_,_).

%-----------------------------------------------------------------------------%

:- pred rl_out_info_get_next_materialise_id(int::out, 
		rl_out_info::in, rl_out_info::out) is det.

rl_out_info_get_next_materialise_id(MaterialiseId, Info0, Info) :-
	Info0 = rl_out_info(A,B,C,D,E,F,G,H,I,J,K,
			MaterialiseId, M,N,O,P,Q,R,S,T,U,V,W,X,Y),
	Info = rl_out_info(A,B,C,D,E,F,G,H,I,J,K,
			MaterialiseId + 1, M,N,O,P,Q,R,S,T,U,V,W,X,Y).

%-----------------------------------------------------------------------------%

:- pred rl_out_info_get_relations(map(relation_id, relation_info)::out,
		rl_out_info::in, rl_out_info::out) is det.

rl_out_info_get_relations(Relations, Info, Info) :-
	Info = rl_out_info(_,_,_,_,Relations,
			_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_).

%-----------------------------------------------------------------------------%

:- pred rl_out_info_get_output_relation_schema(output_rel::in, list(type)::out,
		rl_out_info::in, rl_out_info::out) is det.

rl_out_info_get_output_relation_schema(output_rel(RelId, _), Schema) -->
	rl_out_info_get_relation_schema(RelId, Schema).

:- pred rl_out_info_get_relation_schema(relation_id::in, list(type)::out,
		rl_out_info::in, rl_out_info::out) is det.

rl_out_info_get_relation_schema(RelId, Schema, Info0, Info) :-
	rl_out_info_get_relations(Relations, Info0, Info),
	map__lookup(Relations, RelId, RelInfo),
	RelInfo = relation_info(_, Schema, _, _).

:- pred rl_out_info_get_relation_indexes(relation_id::in, 
	list(index_spec)::out, rl_out_info::in, rl_out_info::out) is det.

rl_out_info_get_relation_indexes(RelId, Indexes, Info0, Info) :-
	rl_out_info_get_relations(Relations, Info0, Info),
	map__lookup(Relations, RelId, RelInfo),
	RelInfo = relation_info(_, _, Indexes, _).

:- pred rl_out_info_get_relation_type(relation_id::in, 
	relation_type::out, rl_out_info::in, rl_out_info::out) is det.

rl_out_info_get_relation_type(RelId, Type, Info0, Info) :-
	rl_out_info_get_relations(Relations, Info0, Info),
	map__lookup(Relations, RelId, RelInfo),
	RelInfo = relation_info(Type, _, _, _).

:- pred rl_out_info_get_output_relation_schema_offset(output_rel::in, int::out,
		rl_out_info::in, rl_out_info::out) is det.

rl_out_info_get_output_relation_schema_offset(output_rel(RelId, _),
		SchemaOffset) -->
	rl_out_info_get_relation_schema_offset(RelId, SchemaOffset).

:- pred rl_out_info_get_relation_schema_offset(relation_id::in, int::out,
		rl_out_info::in, rl_out_info::out) is det.

rl_out_info_get_relation_schema_offset(RelId, SchemaOffset) -->
	rl_out_info_get_relation_schema(RelId, Schema),
	rl_out__schema_to_string(Schema, SchemaOffset).

%-----------------------------------------------------------------------------%

:- pred rl_out_info_incr_pc(int::in, rl_out_info::in,
		rl_out_info::out) is det.
	
rl_out_info_incr_pc(Incr, Info0, Info) :-
	Info0 = rl_out_info(PC0,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,
			S,T,U,V,W,X,Y),
	PC = PC0 + Incr,
	Info = rl_out_info(PC,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,
			T,U,V,W,X,Y).

:- pred rl_out_info_get_pc(int::out, rl_out_info::in,
		rl_out_info::out) is det.

rl_out_info_get_pc(PC0, Info, Info) :-
	Info = rl_out_info(PC0,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,
			_,_,_,_,_,_,_).

%-----------------------------------------------------------------------------%

:- pred rl_out_info_add_proc(procedure::in,
		rl_out_info::in, rl_out_info::out) is det.

rl_out_info_add_proc(Proc, Info0, Info) :-
	Info0 = rl_out_info(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Procs0,
			R,S,T,U,V,W,X,Y),
	Info = rl_out_info(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,[Proc | Procs0],
			R,S,T,U,V,W,X,Y).

:- pred rl_out_info_get_procs(list(procedure)::out,
		rl_out_info::in, rl_out_info::out) is det.

rl_out_info_get_procs(Procs, Info, Info) :-
	Info = rl_out_info(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,Procs0,
			_,_,_,_,_,_,_,_),
	list__reverse(Procs0, Procs).

%-----------------------------------------------------------------------------%

:- pred rl_out_info_get_permanent_relations(set(relation)::out,
		rl_out_info::in, rl_out_info::out) is det.

rl_out_info_get_permanent_relations(Rels, Info, Info) :-
	Info = rl_out_info(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,
			Rels,_,_,_,_).

:- pred rl_out_info_set_permanent_relations(set(relation)::in,
		rl_out_info::in, rl_out_info::out) is det.

rl_out_info_set_permanent_relations(Rels, Info0, Info) :-
	Info0 = rl_out_info(A,B,C,D,E,F,G,H,I,J,K,L,M,
			N,O,P,Q,R,S,T,_,V,W,X,Y),
	Info = rl_out_info(A,B,C,D,E,F,G,H,I,J,K,L,M,
			N,O,P,Q,R,S,T,Rels, V,W,X,Y).

%-----------------------------------------------------------------------------%

:- pred rl_out_info_get_proc_expressions(list(expression)::out,
		rl_out_info::in, rl_out_info::out) is det.

rl_out_info_get_proc_expressions(Exprns, Info, Info) :-
	Info = rl_out_info(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,
			_,_,Exprns0,_,_),
	list__reverse(Exprns0, Exprns).

:- pred rl_out_info_add_expression(expression::in, int::out,
		rl_out_info::in, rl_out_info::out) is det.

rl_out_info_add_expression(Exprn, NextExprn0, Info0, Info) :-
	Info0 = rl_out_info(A,B,C,D,E,F,G,H,I,J,K,L,M,
			N,O,P,Q,R,S,T,U,V,Exprns0,NextExprn0,Y),
	NextExprn is NextExprn0 + 1,
	Info = rl_out_info(A,B,C,D,E,F,G,H,I,J,K,L,M,
			N,O,P,Q,R,S,T,U,V,[Exprn | Exprns0], NextExprn,Y).

%-----------------------------------------------------------------------------%

:- pred rl_out_info_get_tmp_var(int::in, int::out, 
		rl_out_info::in, rl_out_info::out) is det.

rl_out_info_get_tmp_var(Schema, Var) -->
	rl_out_info_get_tmp_vars(TmpVars0),
	( { multi_map__search(TmpVars0, Schema, [Var0 | Vars]) } ->
		{ Var = Var0 },
		{ multi_map__det_replace(TmpVars0, Schema, Vars, TmpVars) },
		rl_out_info_set_tmp_vars(TmpVars)
	;
		rl_out_info_add_relation_variable(Schema, Var)
	).

:- pred rl_out_info_return_tmp_var(int::in, int::in, byte_tree::out,
		rl_out_info::in, rl_out_info::out) is det.
	
rl_out_info_return_tmp_var(Schema, Var, TmpClearCode) -->
	rl_out_info_get_tmp_vars(TmpVars0),
	( { multi_map__search(TmpVars0, Schema, Vars) } ->
		{ multi_map__det_replace(TmpVars0, Schema,
			[Var | Vars], TmpVars) }
	;
		{ multi_map__det_insert(TmpVars0, Schema, Var, TmpVars) }
	),
	{ TmpClearCode = node([rl_PROC_unsetrel(Var)]) },
	rl_out_info_set_tmp_vars(TmpVars).

:- pred rl_out_info_return_tmp_vars(assoc_list(int, int)::in, byte_tree::out,
		rl_out_info::in, rl_out_info::out) is det.

rl_out_info_return_tmp_vars([], empty) --> [].
rl_out_info_return_tmp_vars([Schema - Var | Vars], tree(Clear0, Clear1)) -->
	rl_out_info_return_tmp_var(Schema, Var, Clear0),
	rl_out_info_return_tmp_vars(Vars, Clear1).

:- pred rl_out_info_get_tmp_vars(multi_map(int, int)::out, 
		rl_out_info::in, rl_out_info::out) is det.

rl_out_info_get_tmp_vars(TmpVars, Info, Info) :-
	Info = rl_out_info(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,
			_,_,_,_, TmpVars).

:- pred rl_out_info_set_tmp_vars(multi_map(int, int)::in, 
		rl_out_info::in, rl_out_info::out) is det.
		
rl_out_info_set_tmp_vars(TmpVars, Info0, Info) :-
	Info0 = rl_out_info(A,B,C,D,E,F,G,H,I,J,K,L,M,
			N,O,P,Q,R,S,T,U,V,W,X,_),
	Info = rl_out_info(A,B,C,D,E,F,G,H,I,J,K,L,M,
			N,O,P,Q,R,S,T,U,V,W,X,TmpVars).

#else	% !INCLUDE_ADITI_OUTPUT
#endif

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
