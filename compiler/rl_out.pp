%-----------------------------------------------------------------------------%
% Copyright (C) 1998-2001 University of Melbourne.
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
:- import_module rl_file, getopt, modules, prog_util, magic_util, hlds_goal.
:- import_module code_aux, det_analysis, instmap.

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
	rl_out__get_proc_schema(ModuleInfo, ArgSchemas, SchemaString).

:- pred rl_out__get_proc_schema(module_info::in, list(list(type))::in,
		string::out) is det.

rl_out__get_proc_schema(ModuleInfo, ArgSchemas, SchemaString) :-
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
	rl_out__get_proc_schema_2(1, ArgSchemaDeclList, "", SchemaString0),
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

	{ module_info_predids(ModuleInfo, PredIds) },
	{ list__foldl(rl_out__generate_update_procs, PredIds,
		RLInfo1, RLInfo2) },

	globals__io_lookup_string_option(aditi_user, Owner),
	{ rl_out_info_assign_const(string(Owner), OwnerIndex,
		RLInfo2, RLInfo3) },
	{ prog_out__sym_name_to_string(ModuleName0, ModuleName) },
	module_name_to_file_name(ModuleName0, ".m", no, SourceFileName),
	module_name_to_file_name(ModuleName0, ".int", no, IntFileName),
	{ rl_out_info_assign_const(string(ModuleName), ModuleIndex, 
		RLInfo3, RLInfo4) },
	{ rl_out_info_assign_const(string(IntFileName), IntIndex, 
		RLInfo4, RLInfo5) },
	{ rl_out_info_assign_const(string(SourceFileName), 
		SourceIndex, RLInfo5, RLInfo6) },
	{ rl_out_info_get_procs(RLProcs, RLInfo6, RLInfo7) },
	{ rl_out_info_get_consts(Consts, RLInfo7, RLInfo8) },
	{ rl_out_info_get_permanent_relations(PermRelsSet, 
		RLInfo8, RLInfo9) },
	{ rl_out_info_get_relation_variables(RelVars, RLInfo9, _) },

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
			yes, SchemaFileName),
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

	% For each base relation defined in this module, generate
	% a procedure to be used by aditi_bulk_modify to update
	% the relation.
	%
	% In the procedure below, UpdateRel is the relation returned
	% by the closure passed to aditi_bulk_modify. Each tuple returned
	% by that closure contains two sets of arguments -- the tuple
	% to delete, and the tuple to insert.
	%
	% DummyOutput is not actually used -- it is there just so
	% that the procedure matches the usual convention for calling
	% Aditi procedures from Mercury.
	%
	% ModifyProcFor__p_3(UpdateRel, DummyOutput)
	% {
	%	delete(p/3, project(UpdateRel, FirstTuple);
	%	insert(p/3, project(UpdateRel, SecondTuple),
	%	init(DummyOutput).
	% }
:- pred rl_out__generate_update_procs(pred_id::in, rl_out_info::in,
		rl_out_info::out) is det.

rl_out__generate_update_procs(PredId) -->
	rl_out_info_get_module_info(ModuleInfo),
	{ module_info_pred_info(ModuleInfo, PredId, PredInfo) },
	{ module_info_name(ModuleInfo, ModuleName) },
	{ pred_info_module(PredInfo, PredModule) },
	(
		{ ModuleName = PredModule },
		{ hlds_pred__pred_info_is_base_relation(PredInfo) }
	->
		rl_out__generate_update_proc(insert, PredId, PredInfo),
		rl_out__generate_update_proc(delete, PredId, PredInfo),
		rl_out__generate_update_proc(modify, PredId, PredInfo)
	;
		[]
	).

:- type update_type
	--->	insert
	;	delete
	;	modify
	.

:- pred rl_out__generate_update_proc(update_type::in, pred_id::in,
		pred_info::in, rl_out_info::in, rl_out_info::out) is det.

rl_out__generate_update_proc(UpdateType, PredId, PredInfo) -->
	{ map__init(Relations) },
	rl_out_info_init_proc(Relations),

	{ pred_info_arg_types(PredInfo, ArgTypes) },
	{ pred_info_get_indexes(PredInfo, Indexes) },

	rl_out__schema_to_string(ArgTypes, PermSchemaOffset),

	rl_out_info_add_relation_variable(PermSchemaOffset,
		PermanentAddr),
	rl_out__collect_permanent_relation(PredId,
		PermanentAddr, OpenPermanentCode, UnsetPermanentCode),

	{ 
		UpdateType = insert,
		InputRelTypes = ArgTypes
	;
		UpdateType = delete,
		InputRelTypes = ArgTypes
	;
		UpdateType = modify,
		list__append(ArgTypes, ArgTypes, InputRelTypes)
	},

	rl_out__schema_to_string(InputRelTypes, InputRelSchemaOffset),
	rl_out_info_add_relation_variable(InputRelSchemaOffset,
		InputRelAddr),

	rl_out__schema_to_string([], NullSchemaOffset),
	rl_out_info_add_relation_variable(NullSchemaOffset,
		DummyOutputAddr),

	rl_out_info_get_module_info(ModuleInfo),

	{ LockSpec = 0 }, % default lock spec
	(
		{ UpdateType = insert },
		{ rl__get_insert_proc_name(ModuleInfo, PredId, ProcName) },
		rl_out_info_get_next_materialise_id(MaterialiseId),
		{ InsertCode =
			node([
				rl_PROC_materialise(MaterialiseId),
				rl_PROC_stream,
				rl_PROC_var(InputRelAddr, LockSpec),
				rl_PROC_stream_end,

				rl_PROC_var_list_cons(PermanentAddr, LockSpec),
				rl_PROC_var_list_nil
			]) },
		{ DeleteCode = empty }
	;
		{ UpdateType = delete },
		{ rl__get_delete_proc_name(ModuleInfo, PredId, ProcName) },
		{ DeleteInputStream =
			node([
				rl_PROC_stream,
				rl_PROC_var(InputRelAddr, LockSpec),
				rl_PROC_stream_end
			]) },
		{ InsertCode = empty },
		rl_out__generate_delete_code(PermanentAddr, Indexes, ArgTypes,
			PermSchemaOffset, DeleteInputStream, DeleteCode)
	;
		{ UpdateType = modify },
		{ rl__get_modify_proc_name(ModuleInfo, PredId, ProcName) },
		rl_out__generate_modify_project_exprn(ArgTypes,
			PermSchemaOffset, one, DeleteProjectExpr),
		rl_out__generate_modify_project_exprn(ArgTypes,
			PermSchemaOffset, two, InsertProjectExpr),

		%
		% Project the input relation onto
		% the first half of its attributes,
		% deleting the result from the base
		% relation.
		% 

		{ DeleteInputStream =
			node([
				rl_PROC_stream,

				rl_PROC_project_tee,
				rl_PROC_stream,
				rl_PROC_var(InputRelAddr, LockSpec),
				rl_PROC_stream_end,
				rl_PROC_expr(DeleteProjectExpr),
				rl_PROC_var_list_nil,
				rl_PROC_expr_list_nil,

				rl_PROC_stream_end
		]) },
		rl_out__generate_delete_code(PermanentAddr, Indexes, ArgTypes,
			PermSchemaOffset, DeleteInputStream, DeleteCode),

		rl_out_info_get_next_materialise_id(MaterialiseId),
		{ InsertCode =
			node([
				%
				% Project the input relation onto
				% the second half of its attributes,
				% inserting the result into the base
				% relation.
				% 
				rl_PROC_materialise(MaterialiseId),
				rl_PROC_stream,

				rl_PROC_project_tee,
				rl_PROC_stream,
				rl_PROC_var(InputRelAddr, LockSpec),
				rl_PROC_stream_end,
				rl_PROC_expr(InsertProjectExpr),
				rl_PROC_var_list_nil,
				rl_PROC_expr_list_nil,

				rl_PROC_stream_end,

				rl_PROC_var_list_cons(PermanentAddr, LockSpec),
				rl_PROC_var_list_nil
			]) }
	),


	{ Codes = tree(
		%
		% Open the permanent relation.
		%
		node([OpenPermanentCode]),

		%
		% Do the deletion.
		% 
		tree(DeleteCode,

		%
		% Do the insertion for an `aditi_bulk_modify'
		% or `aditi_bulk_insert' goal.
		%
		tree(InsertCode,	

		node([
			%
			% Clean up.
			%
			UnsetPermanentCode,
			rl_PROC_unsetrel(InputRelAddr),

			%
			% Create the dummy output variable.
			%
			rl_PROC_createtemprel(DummyOutputAddr,
				NullSchemaOffset),

			rl_PROC_ret
		])
	))) },
	{ tree__flatten(Codes, CodeList) },
	{ list__condense(CodeList, Code) },

	{ ArgSchemas = [InputRelTypes, []] },
	{ rl_out__get_proc_schema(ModuleInfo, ArgSchemas, ProcSchemaString) },
	rl_out_info_assign_const(string(ProcSchemaString),
		ProcSchemaConst),

	{ Args = [InputRelAddr, DummyOutputAddr] },
	rl_out__package_proc(ProcName, Args, Code, ProcSchemaConst).


:- pred rl_out__generate_delete_code(int::in, list(index_spec)::in,
		list(type)::in, int::in, byte_tree::in, byte_tree::out,
		rl_out_info::in, rl_out_info::out) is det.

rl_out__generate_delete_code(PermanentAddr, _Indexes, ArgTypes, SchemaOffset,
		DeleteInputStream, DeleteCode) -->
	(
		{ ArgTypes = [] },
		{ CondCode = 
			tree(node([rl_PROC_empty]),
			DeleteInputStream
		) },
		{ ThenCode = empty },

		% We use clear here because otherwise the relation manager
		% may complain about deleting a tuple from an empty relation.
		{ ElseCode = node([rl_PROC_clear(PermanentAddr)]) },
		rl_out__generate_ite(CondCode, ThenCode, ElseCode, DeleteCode)
	;
		{ ArgTypes = [_ | _] },

		%
		% The tuples to delete must come from the relation to
		% delete from -- Aditi does the deletion by tuple-id,
		% not tuple contents. To get the correct tuples, we must
		% do a sem-join of the tuples to delete against the relation
		% to delete from.
		%
		% Note that the indexed semi-join won't work because it returns
		% tuples from the non-indexed relation, which are no good for
		% deleting from the indexed relation.
		%
		% XXX For a permanent relation with a unique B-tree index
		% on all attributes, we may be able to use a sort-merge
		% semi-join to collect the tuples to delete.
		%
		{ list__length(ArgTypes, Arity) },
		{ list__foldl2(
			(pred(_::in, L0::in, L::out, N0::in, N::out) is det :-
				L = [N0 | L0],
				N = N0 - 1
			),
			ArgTypes, [], Attrs, Arity, _) },
		rl_out__do_generate_hash_exprn(ArgTypes, SchemaOffset,
			Attrs, HashExprn),
		rl_out__do_generate_equijoin_exprn(ArgTypes, Attrs, JoinCond),
		{ LockSpec = 0 }, % default lock spec
		{ DeleteCode =
			tree(node([
				rl_PROC_delete(PermanentAddr),
				rl_PROC_stream,
				rl_PROC_semijoin_hj,
				rl_PROC_stream,
				rl_PROC_var(PermanentAddr, LockSpec),
				rl_PROC_stream_end
			]),
			tree(DeleteInputStream,
			node([
				% Both relations can use the same
				% hash expression.
				rl_PROC_expr(HashExprn),
				rl_PROC_expr(HashExprn),
				rl_PROC_expr(JoinCond),

				rl_PROC_stream_end
			])
		)) }
	).

:- pred rl_out__generate_proc_bytecode(rl_proc::in, 
		rl_out_info::in, rl_out_info::out) is det.

rl_out__generate_proc_bytecode(Proc) -->
	{ Proc = rl_proc(Name, Inputs, Outputs, MemoedRels,
			Relations, RLInstrs, _) },

	{ list__append(Inputs, Outputs, Args) },
	rl_out_info_init_proc(Relations),
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
		{ Name = rl_proc_name(Owner, _, _, _) },
		rl_out__collect_memoed_relations(Owner, Name, MemoedList, 0,
			CollectCode, NameCode),
		rl_out__get_rel_var_list(MemoedList, RelVarCodes),
		{ GroupCode = tree(node([rl_PROC_grouprels]), RelVarCodes) }
	),

	rl_out_info_get_relation_addrs(Addrs),
	{ map__to_assoc_list(Addrs, AddrsAL) },
	rl_out__collect_permanent_relations(AddrsAL, [],
		PermRelCodes, [], PermUnsetCodes),

	rl_out__resolve_proc_addresses(RLInstrCodeTree0, RLInstrCodeTree1),

	{ RLInstrCodeTree = 
		tree(node(PermRelCodes),
		tree(node(CollectCode),
		tree(RLInstrCodeTree1,
		tree(node(NameCode),
		tree(GroupCode,
		tree(node(PermUnsetCodes),
		node([rl_PROC_ret])	
	)))))) },
	{ tree__flatten(RLInstrCodeTree, CodeLists) },
	{ list__condense(CodeLists, Codes) },

	list__map_foldl(rl_out_info_get_relation_addr, Args, ArgLocs),
	rl_out__generate_proc_schema(Args, ProcSchemaConst),

	rl_out__package_proc(Name, ArgLocs, Codes, ProcSchemaConst).

:- pred rl_out__package_proc(rl_proc_name::in, list(int)::in,
		list(bytecode)::in, int::in,
		rl_out_info::in, rl_out_info::out) is det.

rl_out__package_proc(Name, ArgLocs, Codes, ProcSchemaConst) -->

	{ Name = rl_proc_name(Owner, Module, ProcName, _) },

	rl_out_info_get_proc_expressions(Exprns),
	{ list__length(Exprns, NumExprns) },

	rl_out_info_assign_const(string(Owner), OwnerConst),
	rl_out_info_assign_const(string(Module), ModuleConst),
	rl_out_info_assign_const(string(ProcName), NameConst),
	{ rl_out__instr_code_size(node(Codes), CodeLength) },

	{ list__length(ArgLocs, NumArgs) },

	{ RLProc = procedure(OwnerConst, ModuleConst, NameConst,
			ProcSchemaConst, NumArgs, ArgLocs, NumExprns, Exprns,
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
		list(bytecode)::in, list(bytecode)::out, list(bytecode)::in,
		list(bytecode)::out, rl_out_info::in, rl_out_info::out) is det.

rl_out__collect_permanent_relations([], Codes, Codes,
		UnsetCodes, UnsetCodes) --> [].
rl_out__collect_permanent_relations([RelationId - Addr | Rels],
		Codes0, Codes, UnsetCodes0, UnsetCodes) -->
	rl_out_info_get_relations(Relations),
	{ map__lookup(Relations, RelationId, RelInfo) },
	{ RelInfo = relation_info(RelType, _Schema, _Index, _) },
	(
		{ RelType = permanent(proc(PredId, _)) }
	->
		rl_out__collect_permanent_relation(PredId, Addr,
			SetCode, UnsetCode),
		{ UnsetCodes1 = [UnsetCode | UnsetCodes0] },
		{ Codes1 = [SetCode | Codes0] }
	;
		{ UnsetCodes1 = UnsetCodes0 },
		{ Codes1 = Codes0 }
	),
	rl_out__collect_permanent_relations(Rels, Codes1, Codes,
		UnsetCodes1, UnsetCodes).

:- pred rl_out__collect_permanent_relation(pred_id::in, int::in,
		bytecode::out, bytecode::out,
		rl_out_info::in, rl_out_info::out) is det.

rl_out__collect_permanent_relation(PredId, Addr, SetCode, UnsetCode) -->
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
	{ SetCode = rl_PROC_openpermrel(Addr, RelNameOffset, SchemaOffset) },
	{ UnsetCode = rl_PROC_unsetrel(Addr) }.

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

rl_out__generate_instr(join(Output, Input1, Input2, Type, Cond,
		SemiJoin, TrivialJoin) - _, Code) -->
	rl_out__generate_join(Output, Input1, Input2, Type,
		Cond, SemiJoin, TrivialJoin, Code).
rl_out__generate_instr(
		subtract(Output, Input1, Input2,
			Type, Cond, TrivialSubtract) - _, 
		Code) -->
	rl_out__generate_subtract(Output, Input1, Input2, Type,
		Cond, TrivialSubtract, Code).
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
	rl_out__generate_project(Output, Input, Cond0, OtherOutputs,
		ProjectType, Code).
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
rl_out__generate_instr(add_index(output_rel(Rel, Indexes), Input) - _,
		Code) -->
	% Generated as 
	% if (is_permanent(Input) and !has_index(Input, Indexes)) {
	% 	copy(Output, Input);
	% } else {
	%	ref(Output, Input);
	% 	if (has_index(Input, Indexes)) {
	%		;
	%	} else {
	%		rl_PROC_add_index(Output, Indexes);
	%	}
	% }
	%
	rl_out__generate_test_for_non_indexed_permanent(Input,
		Indexes, CondCode),
	rl_out__generate_instr(copy(output_rel(Rel, Indexes), Input) - "",
		ThenCode),
	rl_out__generate_instr(ref(Rel, Input) - "", RefCode),
	rl_out__add_indexes_to_rel(may_have_index, Rel, Indexes, IndexCode),
	{ ElseCode = tree(RefCode, IndexCode) },
	rl_out__generate_ite(CondCode, ThenCode, ElseCode, Code).
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
	% 		OutputRel = add_index(InputRel)
	% 	} else {
	% 		OutputRel = copy(InputRel)
	%	}
	rl_out_info_get_relation_addr(Input, InputAddr),
	{ CondCode = node([rl_PROC_one_reference(InputAddr)]) },

	% We may not need to generate this instruction - rl_sort.m
	% has enough information to work out whether this is actually needed.
	rl_out__generate_instr(add_index(OutputRel, Input) - Comment,
		ThenCode),

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
	rl_out__add_indexes_to_rels_copy_permanents(may_have_index,
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

:- pred rl_out__generate_join(output_rel::in, relation_id::in,
	relation_id::in, join_type::in, rl_goal::in, maybe(semi_join_info)::in,
	maybe(trivial_join_info)::in, byte_tree::out,
	rl_out_info::in, rl_out_info::out) is det.

rl_out__generate_join(Output, Input1a, Input2a, JoinType0, Cond0,
		MaybeSemiJoin, MaybeTrivialJoin, Code) -->
	%
	% Work out the bytecode to use for the join, and whether the
	% join is actually a semi-join.
	%
	{ rl_out__compute_join_bytecode(JoinType0, MaybeSemiJoin,
		JoinBytecode, SwapInputs) },
		
	{
		MaybeSemiJoin = yes(_),
		rl__strip_goal_outputs(Cond0, Cond1)
	;
		MaybeSemiJoin = no,
		Cond1 = Cond0
	},

	{
		SwapInputs = yes,
		Input1 = Input2a,
		Input2 = Input1a,
		rl__swap_goal_inputs(Cond1, Cond),
		rl__swap_join_type_inputs(JoinType0, JoinType)
	;
		SwapInputs = no,
		Input1 = Input1a,
		Input2 = Input2a,
		Cond = Cond1,
		JoinType = JoinType0
	},

	%
	% Optimize a common case here - if the join condition does not
	% depend on the second relation, we generate this as:
	% if (empty(rel2)) {
	% 	init(output);
	% } else {
	%	output = project(rel1);
	% }
	%
	% This happens often for joins with zero-arity input relations.
	%
	% The projection will be unnecessary for a semi-join where
	% the condition is deterministic.
	%
	(
		{ MaybeTrivialJoin = yes(TrivialJoinInfo) },
		{ TrivialJoinInfo = trivial_join_or_subtract_info(
					ProjectTupleNum0, MaybeProjectType) },
		{
			SwapInputs = yes,
			rl__swap_tuple_num(ProjectTupleNum0, ProjectTupleNum)
		;
			SwapInputs = no,
			ProjectTupleNum = ProjectTupleNum0
		},

		{
			ProjectTupleNum = one,
			ProjectInput = Input1,
			TestRel = Input2
		;
			ProjectTupleNum = two,
			ProjectInput = Input2,
			TestRel = Input1
		},

		(
			{ MaybeProjectType = yes(ProjectType) },
			{ rl__swap_tuple_num(ProjectTupleNum, InputToRemove) },
			{ rl__remove_goal_input(InputToRemove,
				Cond, ProjectCond) },
			{ ProjectInstr = project(Output, ProjectInput,
					ProjectCond, [], ProjectType) - "" },
			rl_out__generate_instr(ProjectInstr, ElseCode)
		;
			{ MaybeProjectType = no },
			rl_out__maybe_materialise(Output,
				ProjectInput, ElseCode)
		),

		rl_out__generate_stream(TestRel, TestStreamCode),
		{ CondCode = tree(node([rl_PROC_empty]), TestStreamCode) },	
		rl_out__generate_instr(init(Output) - "", ThenCode),
		rl_out__generate_ite(CondCode, ThenCode, ElseCode, Code)
	;
		{ MaybeTrivialJoin = no },
		rl_out__generate_join_or_subtract(Output, Input1, Input2,
			JoinType, JoinBytecode, Cond, Code)
	).

:- pred rl_out__compute_join_bytecode(join_type::in, maybe(semi_join_info)::in,
		bytecode::out, bool::out) is det.

rl_out__compute_join_bytecode(nested_loop, no, rl_PROC_join_nl, no).
rl_out__compute_join_bytecode(nested_loop, yes(Tuple),
		rl_PROC_semijoin_nl, Swap) :-
	rl_out__should_swap_inputs(Tuple, Swap).

rl_out__compute_join_bytecode(sort_merge(_, _), no, rl_PROC_join_sm, no).
rl_out__compute_join_bytecode(sort_merge(_, _), yes(Tuple),
		rl_PROC_semijoin_sm, Swap) :-
	rl_out__should_swap_inputs(Tuple, Swap),
	error(
"rl_out__compute_join_bytecode: sort-merge semi-joins not yet implemented in Aditi").

rl_out__compute_join_bytecode(hash(_, _), no, rl_PROC_join_hj, no).
rl_out__compute_join_bytecode(hash(_, _), yes(Tuple),
		rl_PROC_semijoin_hj, Swap) :-
	rl_out__should_swap_inputs(Tuple, Swap).

rl_out__compute_join_bytecode(index(_, _), no,
		rl_PROC_join_index_simple(IndexRangeTypes), no) :-

		% both ends of the key range are closed.
		% XXX we should detect and use open ranges
	IndexRangeTypes = 0.

rl_out__compute_join_bytecode(index(_, _), yes(Tuple),
		rl_PROC_semijoin_index(Output, IndexRangeTypes), no) :-

		% The tuple from the non-indexed relation is returned --
		% returning the tuple from the index relation is not
		% yet implemented in Aditi.
	Output = 0,

		% both ends of the key range are closed.
		% XXX we should detect and use open ranges
	IndexRangeTypes = 0,
	require(unify(Tuple, one),
		"indexed semi_join doesn't return first tuple").

	% For semi_joins, the first input tuple is the one returned.
:- pred rl_out__should_swap_inputs(tuple_num::in, bool::out) is det.

rl_out__should_swap_inputs(one, no).
rl_out__should_swap_inputs(two, yes).

%-----------------------------------------------------------------------------%

:- pred rl_out__generate_subtract(output_rel::in, relation_id::in,
		relation_id::in, subtract_type::in, rl_goal::in,
		maybe(trivial_subtract_info)::in, byte_tree::out,
		rl_out_info::in, rl_out_info::out) is det.

rl_out__generate_subtract(Output, Input1, Input2, Type,
		Cond, TrivialSubtract, Code) -->
	(
	    { TrivialSubtract = yes(
		trivial_join_or_subtract_info(TupleNum, MaybeProject)) },
	    (
		{ TupleNum = one },
		% Output = subtract(Input1, Input2, Cond),
		% 	where Cond is independent of input two,
		%	is generated as:
		%  
		% if (empty(Input2)) {
		%	Output = Input1	
		% else {
		% 	Output = select(Input1, not(Cond))
		% }
		(
			{ MaybeProject = yes(ProjectType) },
			{ rl__remove_goal_input(two, Cond, ProjectCond0) },
			{ Goals0 = ProjectCond0 ^ goal },
			{ goal_list_nonlocals(Goals0, NonLocals) },
			{ goal_list_determinism(Goals0, Detism0) },
			{ det_negation_det(Detism0, MaybeDetism) },
			{ MaybeDetism = yes(NegDetism0) ->
				NegDetism = NegDetism0
			;
				% This should probably never happen,
				% but semidet is a safe approximation.
				NegDetism = semidet
			},
			{ instmap_delta_init_reachable(IMDelta) },
			{ goal_info_init(NonLocals, IMDelta, Detism0,
				GoalInfo) },
			{ conj_list_to_goal(Goals0, GoalInfo, Conj) },
			{ goal_info_init(NonLocals, IMDelta, NegDetism,
				NegGoalInfo) },
			{ NegGoal = not(Conj) - NegGoalInfo },
			{ ProjectCond = ProjectCond0 ^ goal := [NegGoal] },
			{ ProjectInstr = project(Output, Input1,
				ProjectCond, [], ProjectType) - "" },
			rl_out__generate_instr(ProjectInstr, ElseCode)
		;
			{ MaybeProject = no },
			% The selection is not removed by
			% rl__is_trivial_subtract in this case.
			{ error(
	"rl_out__generate_subtract: trivial subtract without select") }
		),

		rl_out__generate_stream(Input2, InputStream2Code),
		{ CondCode = tree(node([rl_PROC_empty]), InputStream2Code) },	
		rl_out__generate_instr(init(Output) - "", ThenCode),
		rl_out__generate_ite(CondCode, ThenCode, ElseCode, Code)
	    ;
		{ TupleNum = two },
		% Output = subtract(Input1, Input2, Cond),
		% 	where Cond is independent of input one,
		%	is generated as:
		%  
		% if (empty(select(Input2, Cond))) {
		%	Output = Input1	
		% else {
		% 	init(Output)
		% }

		(
			{ MaybeProject = yes(ProjectType) },
			{ rl__remove_goal_input(one, Cond, ProjectCond) },

			rl_out_info_get_relation_schema(Input2, Input2Schema),
			rl_out_info_add_temporary_relation(Input2Schema,
				stream, TmpRel),
			{ ProjectInstr = project(output_rel(TmpRel, []),
				Input2, ProjectCond, [], ProjectType) - "" },
			rl_out__generate_instr(ProjectInstr, ProjectCode),
			{ ProjectRel = TmpRel }
		;
			{ MaybeProject = no },
			{ ProjectRel = Input2 },
			{ ProjectCode = empty }
		),
		rl_out__generate_stream(ProjectRel, ProjectStreamCode),
		{ CondCode = tree(node([rl_PROC_empty]), ProjectStreamCode) },
		rl_out__maybe_materialise(Output, Input1, ThenCode),
		rl_out__generate_instr(init(Output) - "", ElseCode),

		rl_out__generate_ite(CondCode, ThenCode, ElseCode, ITECode),
		{ Code = tree(ProjectCode, ITECode) }
	    )
	;
		{ TrivialSubtract = no },
		{ rl_out__compute_subtract_bytecode(Type, SubtractBytecode,
			JoinType) },
		rl_out__generate_join_or_subtract(Output, Input1, Input2,
			JoinType, SubtractBytecode, Cond, Code)
	).

	% Work out which bytecode to use, and which join type
	% uses the same instruction format.
:- pred rl_out__compute_subtract_bytecode(subtract_type::in, bytecode::out,
		join_type::out) is det.

rl_out__compute_subtract_bytecode(semi_nested_loop, rl_PROC_semisubtract_nl,
		nested_loop).
rl_out__compute_subtract_bytecode(semi_sort_merge(_, _), _, _) :-
	error(
	"rl_out__compute_subtract_bytecode: subtract_sm not yet implemented").
rl_out__compute_subtract_bytecode(semi_hash(Attrs1, Attrs2),
		rl_PROC_semisubtract_hj, hash(Attrs1, Attrs2)).
rl_out__compute_subtract_bytecode(semi_index(IndexSpec, KeyRange),
		rl_PROC_semisubtract_index(IndexRangeTypes),
		index(IndexSpec, KeyRange)) :-

		% both ends of the key range are closed.
		% XXX we should detect and use open ranges
	IndexRangeTypes = 0.

%-----------------------------------------------------------------------------%

:- pred rl_out__generate_join_or_subtract(output_rel::in, relation_id::in,
	relation_id::in, join_type::in, bytecode::in,
	rl_goal::in, byte_tree::out, rl_out_info::in, rl_out_info::out) is det.

rl_out__generate_join_or_subtract(Output, Input1, Input2, JoinType,
		JoinBytecode, Cond, Code) -->
	(
		{ JoinType = nested_loop },
		rl_out__generate_join_or_subtract_2(JoinBytecode, Output,
			Input1, Input2, [], Cond, Code)
	;
		{ JoinType = hash(Attrs1, Attrs2) },
		%
		% Hash values are not necessarily unique, so
		% the entire join condition must run when
		% performing the nested loop join on tuples
		% with the same hash value.
		%
		rl_out__generate_hash_exprn(Input1, Attrs1, HashExprn1),
		rl_out__generate_hash_exprn(Input2, Attrs2, HashExprn2),
		rl_out__generate_join_or_subtract_2(JoinBytecode, Output,
			Input1, Input2, [HashExprn1, HashExprn2], Cond, Code)
	;
		{ JoinType = sort_merge(Spec1, Spec2) },
		rl_out_info_get_relation_schema(Input1, Schema1),
		rl_out_info_get_relation_schema(Input1, Schema2),
		rl_out__generate_sort_merge_compare_exprn(Spec1, Schema1,
			Spec2, Schema2, CompareExprn),

		%
		% XXX We should strip out the parts of the join
		% condition which are already tested by the CompareExprns.
		%
		rl_out__generate_join_or_subtract_2(JoinBytecode, Output,
			Input1, Input2, [CompareExprn],
			Cond, Code)
	;
		{ JoinType = index(IndexSpec, Range) },
		{ rl_out__index_spec_to_string(IndexSpec, IndexStr) },
		rl_out_info_assign_const(string(IndexStr), IndexConst),
		rl_out__generate_stream(Input1, Stream1Code),
		rl_out_info_get_relation_addr(Input2, Input2Addr),
		rl_out__generate_key_range(Range, RangeExprn),
		rl_out_info_get_output_relation_schema_offset(Output,
			OutputSchemaOffset),
		%
		% XXX We should strip out the parts of the join
		% condition which are already tested by the comparison
		% against the ends of the key range.
		%
		rl_out__generate_exprn(Cond, OutputSchemaOffset, CondExprn),
		{ InstrCode =
			tree(node([JoinBytecode]),
			tree(Stream1Code,
			node([
				rl_PROC_indexed_var(Input2Addr, 0, IndexConst),
				rl_PROC_expr(RangeExprn),
				rl_PROC_expr(CondExprn)
			])
		)) },
		rl_out__generate_stream_instruction(Output, InstrCode, Code)
	).

:- pred rl_out__generate_join_or_subtract_2(bytecode::in, output_rel::in,
		relation_id::in, relation_id::in,
		list(int)::in, rl_goal::in, byte_tree::out,
		rl_out_info::in, rl_out_info::out) is det.

rl_out__generate_join_or_subtract_2(JoinCode, Output, Input1, Input2,
		ExtraExprns, Cond, Code) -->
	rl_out_info_get_output_relation_schema_offset(Output,
		OutputSchemaOffset),
	rl_out__generate_stream(Input1, Stream1Code),
	rl_out__generate_stream(Input2, Stream2Code),
	rl_out__generate_exprn(Cond, OutputSchemaOffset, CondExprn),
	{ list__append(ExtraExprns, [CondExprn], Exprns) },
	{ list__map(pred(Exprn::in, rl_PROC_expr(Exprn)::out) is det,
		Exprns, ExprnInstrs) },
	{ InstrCode =
		tree(node([JoinCode]), 
		tree(Stream1Code, 
		tree(Stream2Code, 
		node(ExprnInstrs)
	))) },
	rl_out__generate_stream_instruction(Output, InstrCode, Code).

%-----------------------------------------------------------------------------%

:- pred rl_out__generate_project(output_rel::in, relation_id::in,
		rl_goal::in, assoc_list(output_rel, rl_goal)::in,
		project_type::in, byte_tree::out, rl_out_info::in,
		rl_out_info::out) is det.

rl_out__generate_project(Output, Input, Cond0, OtherOutputs,
		ProjectType, Code) -->
	rl_out_info_get_output_relation_schema_offset(Output,
		OutputSchemaOffset),

	%
	% If the goal passes the input tuple through unmodified,
	% the projection is actually a selection.
	%
	{ rl__goal_returns_input_tuple(Cond0, one) ->
		rl__strip_goal_outputs(Cond0, Cond)
	;
		Cond = Cond0
	},

	(
		{ OtherOutputs = [] },
		{ rl__goal_is_independent_of_input(one, Cond) }
	->
		%
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
		%
		{ rl__remove_goal_input(one, Cond, TupleGoal) },
		rl_out__generate_exprn(TupleGoal,
			OutputSchemaOffset, CondExprn),
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
			%
			% For an indexed project/select we do a btree_scan
			% to select out the range of tuples we're interested
			% in, then proceed as normal.
			%
			% XXX We should strip out the parts of the join
			% condition which are already tested by the
			% CompareExprns. The project_tee operation
			% may not be necessary.
			%

			{ ProjectType = index(IndexSpec, Range) },
			{ rl_out__index_spec_to_string(IndexSpec, IndexStr) },
			rl_out_info_get_relation_addr(Input, InputAddr),
			rl_out_info_assign_const(string(IndexStr), IndexConst),
			rl_out__generate_key_range(Range, RangeExprn),

				% both ends of the key range are closed.
				% XXX we should detect and use open ranges
			{ IndexRangeTypes = 0 },

			{ StreamCode = node([
				rl_PROC_stream,
				rl_PROC_btree_scan(IndexRangeTypes),
				rl_PROC_indexed_var(InputAddr, 0, IndexConst),
				rl_PROC_expr(RangeExprn),
				rl_PROC_stream_end
			]) }
		),

		rl_out__generate_exprn(Cond, OutputSchemaOffset, CondExprn),

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
	% Aditi counts attributes starting from 0.
	string__int_to_string(Attr - 1, AttrStr),
	string__append("#:", AttrStr, Str).

%-----------------------------------------------------------------------------%

:- type check_index
	--->	may_have_index
	;	does_not_have_index
	.

:- pred rl_out__add_indexes_to_rels_copy_permanents(check_index::in, 
		list(output_rel)::in, byte_tree::out,
		rl_out_info::in, rl_out_info::out) is det.

rl_out__add_indexes_to_rels_copy_permanents(CheckIndex,
		OutputRels, IndexCode) -->
	list__foldl2(rl_out__add_indexes_to_rel_copy_permanent(CheckIndex),
		OutputRels, empty, IndexCode).

:- pred rl_out__add_indexes_to_rel_copy_permanent(check_index::in,
		output_rel::in, byte_tree::in, byte_tree::out,
		rl_out_info::in, rl_out_info::out) is det.

rl_out__add_indexes_to_rel_copy_permanent(CheckIndex,
		OutputRel, Code0, Code) -->
	{ OutputRel = output_rel(Output, Indexes) },
	(
		{ Indexes = [] },
		{ Code = Code0 }
	;
		{ Indexes = [_ | _] },
		rl_out__generate_test_for_non_indexed_permanent(Output,
			Indexes, CondCode),
	
		% Copy the base relation, because queries can't add
		% indexes to a base relation.
		rl_out__generate_instr(copy(OutputRel, Output) - "",
			CopyCode),	

		rl_out__add_indexes_to_rel(CheckIndex, Output,
			Indexes, IndexCode),
		rl_out__generate_ite(CondCode, CopyCode, IndexCode, Code)
	).

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

	% Test whether the input relation is a base relation which does
	% not have one of the given indexes. If we are going to add
	% the indexes, the base relation needs to be copied (queries should
	% never add indexes to base relations).
:- pred rl_out__generate_test_for_non_indexed_permanent(relation_id::in,
		list(index_spec)::in, byte_tree::out,
		rl_out_info::in, rl_out_info::out) is det.

rl_out__generate_test_for_non_indexed_permanent(Input, Indexes, CondCode) -->
	rl_out_info_get_relation_addr(Input, InputAddr),
	{ GenerateHasIndexCode =
	    (pred(Index::in, HasIndexCode::out, Info0::in, Info::out) is det :-
			rl_out__index_spec_to_string(Index, IndexStr),
			rl_out_info_assign_const(string(IndexStr), IndexConst,
				Info0, Info),
			HasIndexCode = node([
				rl_PROC_not,
				rl_PROC_has_index(InputAddr, IndexConst)
			])
	    ) },
	list__map_foldl(GenerateHasIndexCode, Indexes, IndexTests),

	{
		IndexTests = [IndexTest | IndexTests1],
		CombineTest =
			(pred(Test1::in, CombinedCode0::in,
					CombinedCode::out) is det :-
				CombinedCode =
					tree(node([rl_PROC_or]),
					tree(Test1,
					CombinedCode0
				))
			),
		list__foldl(CombineTest, IndexTests1,
			IndexTest, IndexTestCode),
		CondCode =
			tree(node([
				rl_PROC_and,
				rl_PROC_is_permanent(InputAddr)
			]),
			IndexTestCode
		)
	;
		IndexTests = [],
		error(
	"rl_out__generate_test_for_non_indexed_permanent: no indexes")
	}.

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

		{ LockSpec = 0 },	% default lock spec
		rl_out__add_indexes_to_rel(does_not_have_index,
			Output, Indexes, IndexInstrs),
		rl_out_info_get_next_materialise_id(Id),
		rl_out_info_return_tmp_var(SchemaOffset,
			TmpVar, TmpClearCode),
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

:- pred rl_out__maybe_materialise(output_rel::in, relation_id::in,
		byte_tree::out, rl_out_info::in, rl_out_info::out) is det.

rl_out__maybe_materialise(OutputRel, Input, Code) -->
	{ OutputRel = output_rel(Output, Indexes) },
	rl_out_info_get_relation_type(Input, InputType),
	rl_out_info_get_relation_type(Output, OutputType),

	(
		{ InputType = temporary(stream) },
		{ OutputType = temporary(materialised) }
	->
		%
		% Materialise the input into the output.
		%
		rl_out__generate_instr(copy(OutputRel, Input) - "", Code)
	;
		( { Indexes = [] } ->
			rl_out__generate_instr(ref(Output, Input) - "", Code)
		;
			rl_out__generate_instr(
				add_index(OutputRel, Input) - "",
				Code)
		)
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

	% Generate an expression to compare the join attributes 
	% in a sort-merge equi-join.
:- pred rl_out__generate_sort_merge_compare_exprn(sort_spec::in,
	list(type)::in, sort_spec::in, list(type)::in, int::out,
	rl_out_info::in, rl_out_info::out) is det.

rl_out__generate_sort_merge_compare_exprn(Attrs1, Schema1,
		Attrs2, Schema2, ExprnNum) -->
	rl_out_info_get_sort_merge_compare_exprns(CompareExprns0),
	rl_out__schema_to_string(Schema1, Schema1Offset),
	rl_out__schema_to_string(Schema2, Schema2Offset),

	{ CompareExprnId = (Attrs1 - Schema1Offset)
				- (Attrs2 - Schema2Offset) },

	( { map__search(CompareExprns0, CompareExprnId, ExprnNum0) } ->
		{ ExprnNum = ExprnNum0 }
	;
		rl_out_info_get_module_info(ModuleInfo),
		{ rl_exprn__generate_sort_merge_compare_exprn(ModuleInfo,
			Attrs1, Schema1, Attrs2, Schema2, Instrs) },

		% Comparison expressions don't use any variables
		% or create an output tuple.
		rl_out__schema_to_string([], EmptySchemaOffset),

		% Nothing is built on the stack, so this will be enough.
		{ StackSize = 10 },
		{ Decls = [] },
		rl_out__package_exprn(Instrs, 2, test, EmptySchemaOffset,
			EmptySchemaOffset, StackSize, Decls, ExprnNum),

		{ map__det_insert(CompareExprns0, CompareExprnId,
			ExprnNum, CompareExprns) },
		rl_out_info_set_sort_merge_compare_exprns(CompareExprns)
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
	
:- pred rl_out__generate_hash_exprn(relation_id::in, list(int)::in, int::out,
		rl_out_info::in, rl_out_info::out) is det.

rl_out__generate_hash_exprn(Input, Attrs, ExprnNum) -->
	rl_out_info_get_relation_schema(Input, InputSchema),
	rl_out__schema_to_string(InputSchema, InputSchemaOffset),
	rl_out__do_generate_hash_exprn(InputSchema,
		InputSchemaOffset, Attrs, ExprnNum).

:- pred rl_out__do_generate_hash_exprn(list(type)::in, int::in,
		list(int)::in, int::out,
		rl_out_info::in, rl_out_info::out) is det.

rl_out__do_generate_hash_exprn(InputSchema, InputSchemaOffset,
		Attrs, ExprnNum) -->
	rl_out_info_get_hash_exprns(HashExprns0),
	( { map__search(HashExprns0, Attrs - InputSchemaOffset, ExprnNum0) } ->
		{ ExprnNum = ExprnNum0 }
	;
		rl_out_info_get_module_info(ModuleInfo),
		{ rl_exprn__generate_hash_function(ModuleInfo,
			Attrs, InputSchema, ExprnCode) },
		rl_out__schema_to_string([], EmptySchemaOffset),

		% Nothing is built on the stack, so this will be enough.
		{ StackSize = 10 },
		{ NumParams = 1 },
		{ Decls = [] },
		rl_out__package_exprn(ExprnCode, NumParams, test,
			EmptySchemaOffset, EmptySchemaOffset, StackSize,
			Decls, ExprnNum),
		{ map__det_insert(HashExprns0, Attrs - InputSchemaOffset,
			ExprnNum, HashExprns) },
		rl_out_info_set_hash_exprns(HashExprns)
	).

	% This is only used by the code to generate the modification
	% and deletion procedures for base relations, so avoiding
	% generating multiple copies of one of these is pointless --
	% only one will ever be generated for each procedure.
:- pred rl_out__do_generate_equijoin_exprn(list(type)::in, list(int)::in,
		int::out, rl_out_info::in, rl_out_info::out) is det.

rl_out__do_generate_equijoin_exprn(InputSchema, Attrs, ExprnNum) -->
	rl_out_info_get_module_info(ModuleInfo),
	{ rl_exprn__generate_equijoin_exprn(ModuleInfo,
		Attrs, InputSchema, ExprnCode) },

	% Nothing is built on the stack, so this will be enough.
	{ StackSize = 10 },
	{ NumParams = 2 },
	{ Decls = [] },
	rl_out__schema_to_string([], EmptySchemaOffset),
	rl_out__package_exprn(ExprnCode, NumParams, test,
		EmptySchemaOffset, EmptySchemaOffset, StackSize,
		Decls, ExprnNum).

	% This is only used by the code to generate the modification
	% procedures for base relations, so avoiding generating multiple
	% copies of one of these is pointless -- only one will ever be
	% generated for each procedure.
:- pred rl_out__generate_modify_project_exprn(list(type)::in, int::in,
		tuple_num::in, int::out,
		rl_out_info::in, rl_out_info::out) is det.

rl_out__generate_modify_project_exprn(Schema, SchemaOffset,
		TupleNum, ExprnNum) -->
	rl_out_info_get_module_info(ModuleInfo),
	{ rl_exprn__generate_modify_project_exprn(ModuleInfo,
		TupleNum, Schema, Code) },

	% Nothing is built on the stack, so this will be enough.
	{ StackSize = 10 },
	{ NumParams = 1 },
	{ ExprnMode = generate },
	{ Decls = [] },
	rl_out__schema_to_string([], EmptySchemaOffset),
	rl_out__package_exprn(Code, NumParams, ExprnMode, SchemaOffset,
		EmptySchemaOffset, StackSize, Decls, ExprnNum).

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
		module_info :: module_info,

		pc :: int,

		procs :: list(procedure),	% bytecodes for each procedure,
						% in reverse order.


			%
			% Tables used to avoid generating multiple
			% copies of the expressions used to
			% compare tuples and compute hash values.
			% 
		compare_exprns :: compare_exprns,
		sort_merge_compare_exprns :: sort_merge_compare_exprns,
		hash_exprns :: hash_exprns,

		permanent_relations :: set(relation),

		relation_addrs :: map(relation_id, int), % relation vars
		next_relation_addr :: int,		% next relation address

		relation_variables :: list(variable),		
						% variables used in
						% reverse order.

		relations :: map(relation_id, relation_info),

		proc_labels :: map(label_id, int), % proc label offsets
		next_proc_label :: int,

		consts :: map(rl_const, int),	% procedure consts
		next_const :: int,		% next proc const address

		next_materialise :: int,	% next materialise number -
						% used for debugging the
						% generated code.

		exprns :: list(expression),	% expressions for the current
						% procedure in reverse order.
		next_exprn :: int,		% next expression.

		tmp_vars :: multi_map(int, int)	
						% temporary relation variables:
						% map from schema constant
						% to list of variables.
						% These must only be used 
						% within one rl.m instruction.
	).

	% We only want to generate a single comparison or hash expression for
	% each combination of attributes and types.
	% Key:
	% 	The int gives the offset of the schema of the input relation
	% 	in the constant table.
	% Value:
	% 	The number of the expression.
:- type compare_exprns == map(pair(sort_spec, int), int).

	% The comparison in a sort-merge join takes for each relation
	% the list of attributes being joined on and the schema offset.
:- type sort_merge_compare == pair(pair(sort_spec, int)).
:- type sort_merge_compare_exprns == map(sort_merge_compare, int).

:- type hash_exprns == map(pair(list(int), int), int).

%-----------------------------------------------------------------------------%

:- pred rl_out_info_init(module_info::in, rl_out_info::out) is det.

rl_out_info_init(ModuleInfo, Info0) :-
	map__init(CompareExprns),
	map__init(SortMergeCompareExprns),
	map__init(HashExprns),
	map__init(Relations),
	map__init(RelationAddrs),
	map__init(Consts),
	map__init(Labels),
	map__init(TmpVars),
	PC = 0,
	FirstRelAddr = 0,
	FirstConst = 1,
	FirstMaterialise = 1,
	FirstLabel = 0,
	Exprns = [],
	FirstExprn = 0,
	Procs = [],
	set__init(PermanentRelations),
	RelationVariables = [],

	Info0 = rl_out_info(ModuleInfo, PC, Procs,
			CompareExprns, SortMergeCompareExprns, HashExprns, 
			PermanentRelations, RelationAddrs, FirstRelAddr, 
			RelationVariables, Relations, Labels, FirstLabel,
			Consts, FirstConst, FirstMaterialise, Exprns,
			FirstExprn, TmpVars).

:- pred rl_out_info_init_proc(map(relation_id, relation_info)::in,
	rl_out_info::in, rl_out_info::out) is det.

rl_out_info_init_proc(Relations) -->
	^ relations := Relations,

	{ map__init(Labels) },
	^ proc_labels := Labels,
	^ next_proc_label := 0,

	{ map__init(RelationAddrs) },
	^ relation_addrs := RelationAddrs,

	{ map__init(CompareExprns) },
	^ compare_exprns := CompareExprns,

	{ map__init(HashExprns) },
	^ hash_exprns := HashExprns,

	^ pc := 0,

	^ exprns := [],
	^ next_exprn := 0,

	{ map__init(TmpVars) },
	^ tmp_vars := TmpVars.

%-----------------------------------------------------------------------------%

:- pred rl_out_info_get_compare_exprns(compare_exprns::out,
		rl_out_info::in, rl_out_info::out) is det.

rl_out_info_get_compare_exprns(Exprns) --> Exprns =^ compare_exprns.

:- pred rl_out_info_set_compare_exprns(compare_exprns::in,
		rl_out_info::in, rl_out_info::out) is det.

rl_out_info_set_compare_exprns(Exprns) --> ^ compare_exprns := Exprns.

%-----------------------------------------------------------------------------%

:- pred rl_out_info_get_sort_merge_compare_exprns(
		sort_merge_compare_exprns::out,
		rl_out_info::in, rl_out_info::out) is det.

rl_out_info_get_sort_merge_compare_exprns(Exprns) -->
		Exprns =^ sort_merge_compare_exprns.

:- pred rl_out_info_set_sort_merge_compare_exprns(
		sort_merge_compare_exprns::in,
		rl_out_info::in, rl_out_info::out) is det.

rl_out_info_set_sort_merge_compare_exprns(Exprns) -->
		^ sort_merge_compare_exprns := Exprns.

%-----------------------------------------------------------------------------%

:- pred rl_out_info_get_hash_exprns(hash_exprns::out, rl_out_info::in,
		rl_out_info::out) is det.

rl_out_info_get_hash_exprns(HashExprns) --> HashExprns =^ hash_exprns.

:- pred rl_out_info_set_hash_exprns(hash_exprns::in, rl_out_info::in,
		rl_out_info::out) is det.

rl_out_info_set_hash_exprns(HashExprns) --> ^ hash_exprns := HashExprns.

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

rl_out_info_get_relation_addrs(Addrs) -->
	Addrs =^ relation_addrs.

:- pred rl_out_info_set_relation_addrs(map(relation_id, int)::in, 
		rl_out_info::in, rl_out_info::out) is det.

rl_out_info_set_relation_addrs(Addrs) --> ^ relation_addrs := Addrs.

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

rl_out_info_add_relation_variable_2(Name, Schema) -->
	Vars0 =^ relation_variables,
	^ relation_variables := [variable(Name, Schema) | Vars0].

:- pred rl_out_info_get_next_relation_addr(int::out, 
		rl_out_info::in, rl_out_info::out) is det.

rl_out_info_get_next_relation_addr(NextAddr0) -->
	NextAddr0 =^ next_relation_addr,
	^ next_relation_addr := NextAddr0 + 1.

:- pred rl_out_info_get_relation_variables(list(variable)::out,
		rl_out_info::in, rl_out_info::out) is det.

rl_out_info_get_relation_variables(Vars, Info, Info) :-
	list__reverse(Info ^ relation_variables, Vars).

%-----------------------------------------------------------------------------%

:- pred rl_out_info_add_label(label_id::in, int::out,
		rl_out_info::in, rl_out_info::out) is det.

rl_out_info_add_label(LabelId, NextLabel) -->
	rl_out_info_add_label(NextLabel),
	Labels0 =^ proc_labels,
	{ map__det_insert(Labels0, LabelId, NextLabel, Labels) },
	^ proc_labels := Labels.

:- pred rl_out_info_add_label(int::out,
		rl_out_info::in, rl_out_info::out) is det.

rl_out_info_add_label(NextLabel) -->
	NextLabel =^ next_proc_label,
	^ next_proc_label := NextLabel + 1.

:- pred rl_out_info_get_labels(map(label_id, int)::out,
		rl_out_info::in, rl_out_info::out) is det.

rl_out_info_get_labels(Labels) --> Labels =^ proc_labels.

%-----------------------------------------------------------------------------%

:- pred rl_out_info_get_module_info(module_info::out, rl_out_info::in,
		rl_out_info::out) is det.

rl_out_info_get_module_info(ModuleInfo) --> ModuleInfo =^ module_info.

%-----------------------------------------------------------------------------%

:- pred rl_out_info_assign_const(rl_const::in, int::out,
		rl_out_info::in, rl_out_info::out) is det.

rl_out_info_assign_const(Const, ConstOffset) -->
	Consts0 =^ consts,
	( { map__search(Consts0, Const, ConstOffset0) } ->
		{ ConstOffset = ConstOffset0 }
	;
		ConstOffset =^ next_const,
		{ map__det_insert(Consts0, Const, ConstOffset, Consts) },
		^ consts := Consts,
		^ next_const := ConstOffset + 1
	).

:- pred rl_out_info_get_consts(map(rl_const, int)::out, 
		rl_out_info::in, rl_out_info::out) is det.

rl_out_info_get_consts(Consts) --> Consts =^ consts.

%-----------------------------------------------------------------------------%

:- pred rl_out_info_get_next_materialise_id(int::out, 
		rl_out_info::in, rl_out_info::out) is det.

rl_out_info_get_next_materialise_id(MaterialiseId) -->
	MaterialiseId =^ next_materialise,
	^ next_materialise := MaterialiseId + 1.

%-----------------------------------------------------------------------------%

:- pred rl_out_info_get_relations(map(relation_id, relation_info)::out,
		rl_out_info::in, rl_out_info::out) is det.

rl_out_info_get_relations(Relations) --> Relations =^ relations.

:- pred rl_out_info_add_temporary_relation(list(type)::in, relation_state::in,
		relation_id::out, rl_out_info::in, rl_out_info::out) is det.
	
rl_out_info_add_temporary_relation(Schema, State, RelationId) -->
	Relations0 =^ relations,

	% This should be pretty rare (this predicate is currently only
	% used in optimizing one type of trivial subtract), so efficiency
	% isn't a concern.
	{ map__sorted_keys(Relations0, RelationIds0) },
	{ list__last(RelationIds0, HighestRelationId) ->
		RelationId = HighestRelationId + 1
	;
		RelationId = 0
	},

	{ rl__relation_id_to_string(RelationId, RelName) },
	{ RelationInfo = relation_info(temporary(State),
				Schema, [], RelName) },

	{ map__det_insert(Relations0, RelationId, RelationInfo, Relations) },
	^ relations := Relations.

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
	
rl_out_info_incr_pc(Incr) -->
	PC0 =^ pc,
	^ pc := PC0 + Incr.

:- pred rl_out_info_get_pc(int::out, rl_out_info::in,
		rl_out_info::out) is det.

rl_out_info_get_pc(PC) --> PC =^ pc.

%-----------------------------------------------------------------------------%

:- pred rl_out_info_add_proc(procedure::in,
		rl_out_info::in, rl_out_info::out) is det.

rl_out_info_add_proc(Proc) -->
	Procs0 =^ procs,
	^ procs := [Proc | Procs0].

:- pred rl_out_info_get_procs(list(procedure)::out,
		rl_out_info::in, rl_out_info::out) is det.

rl_out_info_get_procs(Procs) -->
	Procs0 =^ procs,
	{ list__reverse(Procs0, Procs) }.

%-----------------------------------------------------------------------------%

:- pred rl_out_info_get_permanent_relations(set(relation)::out,
		rl_out_info::in, rl_out_info::out) is det.

rl_out_info_get_permanent_relations(Rels) --> Rels =^ permanent_relations.

:- pred rl_out_info_set_permanent_relations(set(relation)::in,
		rl_out_info::in, rl_out_info::out) is det.

rl_out_info_set_permanent_relations(Rels) --> ^ permanent_relations := Rels.

%-----------------------------------------------------------------------------%

:- pred rl_out_info_get_proc_expressions(list(expression)::out,
		rl_out_info::in, rl_out_info::out) is det.

rl_out_info_get_proc_expressions(Exprns) -->
	Exprns0 =^ exprns,
	{ list__reverse(Exprns0, Exprns) }.

:- pred rl_out_info_add_expression(expression::in, int::out,
		rl_out_info::in, rl_out_info::out) is det.

rl_out_info_add_expression(Exprn, NextExprn0) -->
	Exprns0 =^ exprns,
	NextExprn0 =^ next_exprn,

	^ exprns := [Exprn | Exprns0],
	^ next_exprn := NextExprn0 + 1.

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

rl_out_info_get_tmp_vars(TmpVars) --> TmpVars =^ tmp_vars.

:- pred rl_out_info_set_tmp_vars(multi_map(int, int)::in, 
		rl_out_info::in, rl_out_info::out) is det.
		
rl_out_info_set_tmp_vars(TmpVars) --> ^ tmp_vars := TmpVars.

#else	% !INCLUDE_ADITI_OUTPUT
#endif

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
