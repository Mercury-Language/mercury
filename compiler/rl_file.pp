%-----------------------------------------------------------------------------%
% Copyright (C) 1998-2000, 2003-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% File: rl_file.m
% Main author: stayl.
%
% Definitions for the RLO file format.
%
%-----------------------------------------------------------------------------%
:- module aditi_backend__rl_file.

:- interface.

:- import_module io.

:- type byte_writer == (pred(int, io__state, io__state)).
:- mode byte_writer == (pred(in, di, uo) is det).

	% Given a predicate to write a byte, write an RL file
	% to the current binary output stream, returning the number
	% of bytes written.
:- pred rl_file__write_binary(byte_writer::byte_writer, rl_file::in, 
		int::out, io__state::di, io__state::uo) is det. 

#if INCLUDE_ADITI_OUTPUT	% See ../Mmake.common.in.
:- import_module aditi_backend__rl_code.
:- import_module assoc_list, list.

	% Write a text representation of an RL file to the current
	% text stream output.
:- pred rl_file__write_text(rl_file::in, io__state::di, io__state::uo) is det.

:- type rl_file
	---> rl_file(
		int,		% minor version
		int,		% major version
		int,		% constant pool count
		constant_pool,
		int,		% relations count
		list(relation),	% permanent relations
		int,		% variables count
		list(variable),
		int,		% procedures count
		list(procedure),
		int,		% owner name index in constant pool
		int,		% module name index in constant pool
		int,		% source file name index
		int		% interface file name index
	).

:- type constant_pool == assoc_list(int, rl_const).

:- type rl_const
	--->	int(int)
	;	float(float)
	;	string(string).

:- type relations == assoc_list(int, relation).

:- type relation
	---> relation(
		int,	% user
		int,	% module
		int,	% predicate
		int	% schema
	).

:- type variable
	---> variable(
		int,	% name
		int	% schema
	).

:- type procedure
	---> procedure(
		int,		% user
		int,		% module
		int,		% predicate
		int,		% schema
		int,		% number of arguments
		list(int),	% indices of arguments in the variable table
		int,		% number of expressions
		list(expression),
				% expressions
		int,		% code length
		list(bytecode)	% codes
	).

:- type expression
	---> expression(
		int,		% output schema
		int,		% output schema 2 (key range expressions only)
		int,		% variable array schema (variables are stored
				% in a tuple)
		int,		% stack size
		int,		% number of parameters
		exprn_mode,	
		int,		% code length
		list(bytecode)	% codes
	).

:- type exprn_mode
	--->	void
	;	test		% apply test to input tuples
	;	generate	% generates one output tuple
	;	generate2	% generates two output tuples - used for
				% B-tree key ranges.
	;	generate_nondet % generates one output tuple.
				% The `result' expression fragment
				% can be called multiple times to
				% return all the solutions.
	.
#else
:- type rl_file ---> rl_file.
#endif
%-----------------------------------------------------------------------------%
:- implementation.

:- import_module char, int, require, string, std_util.

:- type rl_state == pair(int, io__state).

:- type writer == (pred(int, rl_state, rl_state)).
:- mode writer == (pred(in, di, uo) is det).

	% 
	% Write the binary representation of the file to <module>.rlo.
	%
#if INCLUDE_ADITI_OUTPUT	% See ../Mmake.common.in.
rl_file__write_binary(ByteWriter, RLFile, Length, IO0, IO) :-
	Writer = (pred(Byte::in, Pair0::di, Pair::uo) is det :-
		Pair0 = Len0 - IOState0,
		Len = Len0 + 1,			
		call(ByteWriter, Byte, IOState0, IOState),
	    	Pair = unsafe_promise_unique(Len) - IOState
	),
	State0 = 0 - IO0,
	rl_file__write_binary_2(Writer, RLFile, State0, State),
	State = Length - IO.
#else
rl_file__write_binary(_ByteWriter, _RLFile, Length, IO0, IO) :-
	( semidet_succeed ->
		error("rl_file.pp: `--aditi' requires `INCLUDE_ADITI_OUTPUT'")
	;
		Length = 0,
		IO = IO0
	).
#endif

#if INCLUDE_ADITI_OUTPUT	% See ../Mmake.common.in.
:- pred rl_file__write_binary_2(writer::writer, rl_file::in, 
		rl_state::di, rl_state::uo) is det. 

rl_file__write_binary_2(Writer, RLFile) -->
       { RLFile = rl_file(
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
		ModuleNameIndex,
		SourceIndex,
		IntIndex
	) },

	string__foldl(rl_file__write_char(Writer), ".rlo"),

	{ int16_to_bytecode(MinorVersion, MinorCode) },
	{ int16_to_bytecode(MajorVersion, MajorCode) },
	list__foldl(Writer, MinorCode),
	list__foldl(Writer, MajorCode),

	% Write out the constant pool.
	{ int16_to_bytecode(ConstTableSize, ConstSizeCode) },
	list__foldl(Writer, ConstSizeCode),
	rl_file__write_char(Writer, 'V'),
	list__foldl(rl_file__generate_const_table_entry(Writer), ConstsLA),

	% Write out the permanent relations used.
	{ int16_to_bytecode(NumPermRels, NumPermRelsCode) },
	list__foldl(Writer, NumPermRelsCode),
	list__foldl(rl_file__write_relation(Writer), PermRels),

	% Write out the variable schema information.
	{ int16_to_bytecode(NumVars, NumVarsCode) },
	list__foldl(Writer, NumVarsCode),
	list__foldl(rl_file__write_variable(Writer), RelVars),	

	% Write out the procedure.
	{ int16_to_bytecode(NumProcs, NumProcsCode) },
	list__foldl(Writer, NumProcsCode),
	list__foldl(rl_file__write_proc_bytecode(Writer), RLProcs),

	{ int16_to_bytecode(OwnerIndex, OwnerCode) },
	list__foldl(Writer, OwnerCode),

	{ int16_to_bytecode(ModuleNameIndex, ModuleNameCode) },
	list__foldl(Writer, ModuleNameCode),

	{ int16_to_bytecode(SourceIndex, SourceCode) },
	list__foldl(Writer, SourceCode),

	{ int16_to_bytecode(0, SourceRelIndex) },
	list__foldl(Writer, SourceRelIndex),

	{ int16_to_bytecode(IntIndex, IntCode) },
	list__foldl(Writer, IntCode).

:- pred rl_file__write_char(writer::writer, char::in,
		rl_state::di, rl_state::uo) is det.

rl_file__write_char(Writer, Char) -->
	% XXX what about Unicode characters?
	%     The value returned by char__to_int might not fit in a byte.
	{ char__to_int(Char, Int) },
	call(Writer, Int).

%-----------------------------------------------------------------------------%

:- pred rl_file__write_relation(writer::writer, relation::in,
		rl_state::di, rl_state::uo) is det.

rl_file__write_relation(Writer, relation(User, Module, Predicate, Schema)) -->
	{ list__map(int16_to_bytecode, [User, Module, Predicate, Schema],
		Codes) },	
	{ list__condense(Codes, CodeList) },
	list__foldl(Writer, CodeList).

%-----------------------------------------------------------------------------%

:- pred rl_file__write_variable(writer::writer, variable::in,
		rl_state::di, rl_state::uo) is det.

rl_file__write_variable(Writer, variable(Name, Schema)) -->
	{ int16_to_bytecode(Name, NameCode) },
	{ int16_to_bytecode(Schema, SchemaCode) },
	list__foldl(Writer, NameCode),
	list__foldl(Writer, SchemaCode).

:- pred rl_file__generate_const_table_entry(writer::writer,
		pair(int, rl_const)::in, rl_state::di, rl_state::uo) is det.

rl_file__generate_const_table_entry(Writer, _Addr - Const) -->
	( 
		{ Const = int(Int) },
		rl_file__write_char(Writer, 'I'),
		{ aInt_to_bytecode(Int, Int64) },
		list__foldl(Writer, Int64)
	;
		{ Const = float(Float) },
		rl_file__write_char(Writer, 'D'),
		{ aDouble_to_bytecode(Float, Float64) },
		list__foldl(Writer, Float64)
	;
		{ Const = string(Str) },
		rl_file__write_char(Writer, 'S'),
		string__foldl(rl_file__write_char(Writer), Str),
		call(Writer, 0)
	).

%-----------------------------------------------------------------------------%

:- pred rl_file__write_proc_bytecode(writer::writer, procedure::in,
		rl_state::di, rl_state::uo) is det.

rl_file__write_proc_bytecode(Writer, Proc) -->
	{ Proc = procedure(User, Module, Name, Schema, NArgs, 
			Args, NExprns, Exprns, CodeLength, Code) },
		
	{ int16_to_bytecode(User, UserCode) },
	{ int16_to_bytecode(Module, ModuleCode) },
	{ int16_to_bytecode(Name, NameCode) },
	{ int16_to_bytecode(Schema, SchemaCode) },
	{ int16_to_bytecode(NArgs, NArgsCode) },
	{ int16_to_bytecode(NExprns, NExprnsCode) },
	{ int32_to_bytecode(CodeLength, CodeLengthCode) },
	{ list__map(int16_to_bytecode, Args, ArgCodeLists) },
	{ list__condense(ArgCodeLists, ArgCodes) },

	list__foldl(Writer, UserCode),
	list__foldl(Writer, ModuleCode),
	list__foldl(Writer, NameCode),
	list__foldl(Writer, SchemaCode),
	list__foldl(Writer, NArgsCode),
	list__foldl(Writer, ArgCodes),

	list__foldl(Writer, NExprnsCode),
	list__foldl(rl_file__write_exprn_bytecode(Writer), Exprns),

	list__foldl(Writer, CodeLengthCode),

	rl_file__output_bytecodes(Writer, Code),

	% Line number table length (NYI).
	{ int16_to_bytecode(0, LineNumCode) },
	list__foldl(Writer, LineNumCode).

:- pred rl_file__write_exprn_bytecode(writer::writer, expression::in, 
		rl_state::di, rl_state::uo) is det.

rl_file__write_exprn_bytecode(Writer, Exprn) -->
	{ Exprn = expression(OutputSchema, OutputSchema2, VarSchema,
			Stack, NumParams, ExprnMode, CodeLength, Code) },
	{ int16_to_bytecode(OutputSchema, OutputSchemaCode) },
	{ int16_to_bytecode(OutputSchema2, OutputSchema2Code) },
	{ int16_to_bytecode(VarSchema, VarSchemaCode) },
	{ int32_to_bytecode(Stack, StackCode) },
	{ int32_to_bytecode(NumParams, NumParamsCode) },

	{ rl_file__exprn_mode_to_int(ExprnMode, Mode) },

	{ int16_to_bytecode(Mode, ModeCode) },
	{ int32_to_bytecode(CodeLength, CodeLengthCode) },

	list__foldl(Writer, OutputSchemaCode),
	list__foldl(Writer, OutputSchema2Code),
	list__foldl(Writer, VarSchemaCode),
	list__foldl(Writer, StackCode),
	list__foldl(Writer, NumParamsCode),
	list__foldl(Writer, ModeCode),
	list__foldl(Writer, CodeLengthCode),
	rl_file__output_bytecodes(Writer, Code).

:- pred rl_file__exprn_mode_to_int(exprn_mode, int).
:- mode rl_file__exprn_mode_to_int(in, out) is det.
:- mode rl_file__exprn_mode_to_int(out, in) is semidet.

rl_file__exprn_mode_to_int(ExprnMode, Mode) :-
	(
		ExprnMode = void,
		Mode = 0
	;
		ExprnMode = test,
		Mode = 1
	;
		ExprnMode = generate,
		Mode = 2
	;
		ExprnMode = generate2,
		Mode = 3
	;
		ExprnMode = generate_nondet,
		Mode = 4
	).

:- pred rl_file__output_bytecodes(writer::writer, list(bytecode)::in, 
		rl_state::di, rl_state::uo) is det.

rl_file__output_bytecodes(Writer, Code) -->
	{ OutputByteCode = (pred(Instr::in, IO0::di, IO::uo) is det :-
		bytecode_to_intlist(Instr, IntList),
		list__foldl(Writer, IntList, IO0, IO)
	) },
	list__foldl(OutputByteCode, Code).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

rl_file__write_text(RLFile) -->
       { RLFile = rl_file(
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
	% Write a text representation of the file to <module>.rla.
	% This should be readably formatted, but still parsable by io__read.
	%

	io__write_string("rl_file(\n"),
	io__write_int(MinorVersion),	
	io__write_string(",\t% minor version\n"),
	io__write_int(MajorVersion),	
	io__write_string(",\t% major version\n"),

	io__write_string("%==============const table====================================================%\n"),
	io__write_int(ConstTableSize),	
	io__write_string(",\t% const table size\n"),
	io__write_string("\t[\n\t"),
	io__write_list(ConstsLA, ",\n\t", io__write),
	io__write_string("\n\t],\n"),

	io__write_string("%==============permanent relations============================================%\n"),
	io__write_int(NumPermRels),	
	io__write_string(",\t% number of permanent relations\n"),
	io__write_string("\t[\n\t"),
	io__write_list(PermRels, ",\n\t", io__write),
	io__write_string("\n\t],\n"),
	
	io__write_string("%==============var table======================================================%\n"),
	io__write_int(NumVars),	
	io__write_string(",\t% number of variables\n"),
	io__write_string("\t[\n\t"),
	io__write_list(RelVars, ",\n\t", io__write),
	io__write_string("\n\t],\n"),

	io__write_string("%==============proc table=====================================================%\n"),
	io__write_int(NumProcs),	
	io__write_string(",\t% number of procedures\n"),
	io__write_string("\t[\n\t"),
	io__write_list(RLProcs, ",\n\t", rl_file__write_proc(ConstsLA)),
	io__write_string("\n]\n"),
	io__write_string("%=============================================================================%\n"),
	io__write_string(", "),
	io__write_int(OwnerIndex),
	io__write_string(", "),
	io__write_int(ModuleIndex),
	io__write_string(", "),
	io__write_int(SourceIndex),
	io__write_string(", "),
	io__write_int(IntIndex),
	io__write_string(").\n").
	
%-----------------------------------------------------------------------------%

:- pred rl_file__write_proc(constant_pool::in, procedure::in, 
		io__state::di, io__state::uo) is det.

rl_file__write_proc(Consts, Proc) -->
	{ Proc = procedure(User, Module, Name, Schema, NArgs, 
			Args, NExprns, Exprns, CodeLength, Code) },
	{ list__index1_det(Consts, Name, _ - NameConst) },
	{ NameConst = string(ProcName0) ->
		ProcName = ProcName0
	;
		error("rl_file__write_proc")
	},
	io__write_string("procedure(\t% "),
	io__write_string(ProcName),
	io__write_string("\n\t"),
	io__write_int(User),
	io__write_string(",\t% user\n\t"),
	io__write_int(Module),
	io__write_string(",\t% module\n\t"),
	io__write_int(Name),
	io__write_string(",\t% name\n\t"),
	io__write_int(Schema),
	io__write_string(",\t% schema\n\t"),
	io__write_int(NArgs),
	io__write_string(",\t% number of args\n\t["),
	io__write_list(Args, ", ", io__write_int),
	io__write_string("],\t% arguments\n\t"),
	io__write_int(NExprns),
	io__write_string(",\t% number of expressions\n\t[\n\t"),
	( { Exprns = [Exprn | Exprns1] } ->
		rl_file__write_exprn_2(Exprn, 0),
		list__foldl2(rl_file__write_exprn, Exprns1, 1, _)
	;
		[]
	),
	io__write_string("],\n\n\t"),
	io__write_int(CodeLength),
	io__write_string(",\t% code length\n\t[\n"),
	io__write_list(Code, ",\n", rl_file__write_instruction),
	io__write_string("\n\t]\n)").

:- pred rl_file__write_instruction(bytecode::in,
		io__state::di, io__state::uo) is det.

rl_file__write_instruction(Bytecode) -->
	(
		% Make the code easier to read by not indenting
		% certain instructions.
		{ Bytecode \= rl_PROC_materialise(_) },
		{ Bytecode \= rl_PROC_bind_handle(_) },
		{ Bytecode \= rl_PROC_bind_code(_) },
		{ Bytecode \= rl_PROC_label(_) },
		{ Bytecode \= rl_PROC_conditional_goto(_) },
		{ Bytecode \= rl_PROC_conditional_goto_label(_) },
		{ Bytecode \= rl_PROC_goto(_) },
		{ Bytecode \= rl_PROC_goto_label(_) },
		{ Bytecode \= rl_PROC_call(_) }
	->
		io__write_string("\t")
	;
		[]
	),
	io__write(Bytecode),
	(
		% Make it easier to pick out the end of a
		% materialise instruction.
		{ Bytecode = rl_PROC_var_list_nil }
	->
		io__write_string("\n\t")
	;
		[]
	).

%-----------------------------------------------------------------------------%

:- pred rl_file__write_exprn(expression::in, int::in, int::out,
		io__state::di, io__state::uo) is det.

rl_file__write_exprn(Exprn, ExprnNum0, ExprnNum) -->
	{ ExprnNum = ExprnNum0 + 1 },
	io__write_string(",\n\n\t"),
	rl_file__write_exprn_2(Exprn, ExprnNum0).

:- pred rl_file__write_exprn_2(expression::in, int::in,
		io__state::di, io__state::uo) is det.

rl_file__write_exprn_2(Exprn, ExprnNum) -->
	{ Exprn = expression(OutputSchema, OutputSchema2, VarSchema,
			Stack, NumParams, Mode, CodeLength, Code) },

	io__write_string("% expression "),
	io__write_int(ExprnNum),
	io__nl,
	io__write_string("expression(\n"),
	io__write_string("\t\t"),
	io__write_int(OutputSchema),
	io__write_string(", "),
	io__write_int(OutputSchema2),
	io__write_string(", "),
	io__write_int(VarSchema),
	io__write_string(", "),
	io__write_int(Stack),
	io__write_string(", "),
	io__write_int(NumParams),
	io__write_string(", "),
	io__write(Mode),
	io__write_string(", "),
	io__write_int(CodeLength),
	io__write_string(",\n\t[\n\t"),
	io__write_list(Code, ",\n\t", io__write),
	io__write_string("\n\t]\n)").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
#else	/* !INCLUDE_ADITI_OUTPUT */
#endif
