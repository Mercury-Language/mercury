%-----------------------------------------------------------------------------%
% Copyright (C) 1999 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% mlds_to_c - Convert MLDS to C/C++ code.
% Main author: fjh.

%-----------------------------------------------------------------------------%

:- module mlds_to_c.
:- interface.

:- import_module mlds.
:- import_module io.

:- pred mlds_to_c__output_mlds(mlds, io__state, io__state).
:- mode mlds_to_c__output_mlds(in, di, uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module globals, options, passes_aux.
:- import_module builtin_ops, c_util, modules.
:- import_module hlds_pred. % for `pred_proc_id'.
:- import_module prog_data, prog_out.

:- import_module bool, int, string, list, assoc_list, term, std_util, require.

%-----------------------------------------------------------------------------%

mlds_to_c__output_mlds(MLDS) -->
	{ ModuleName = mlds__get_module_name(MLDS) },
	module_name_to_file_name(ModuleName, ".h", yes, HeaderFile),
	module_name_to_file_name(ModuleName, ".c", yes, SourceFile),
	{ Indent = 0 },
	mlds_output_to_file(HeaderFile, mlds_output_hdr_file(Indent, MLDS)),
	mlds_output_to_file(SourceFile, mlds_output_src_file(Indent, MLDS)).
	%
	% XXX at some point we should also handle output of any non-C
	%     foreign code (Ada, Fortran, etc.) to appropriate files.

:- pred mlds_output_to_file(string, pred(io__state, io__state),
				io__state, io__state).
:- mode mlds_output_to_file(in, pred(di, uo) is det, di, uo) is det.

mlds_output_to_file(FileName, Action) -->
	globals__io_lookup_bool_option(verbose, Verbose),
	globals__io_lookup_bool_option(statistics, Stats),
	maybe_write_string(Verbose, "% Writing to file `"),
	maybe_write_string(Verbose, FileName),
	maybe_write_string(Verbose, "'...\n"),
	maybe_flush_output(Verbose),
	io__tell(FileName, Res),
	( { Res = ok } ->
		Action,
		io__told,
		maybe_write_string(Verbose, "% done.\n"),
		maybe_report_stats(Stats)
	;
		maybe_write_string(Verbose, "\n"),
		{ string__append_list(["can't open file `",
			FileName, "' for output."], ErrorMessage) },
		report_error(ErrorMessage)
	).

	%
	% Generate the header file
	%
:- pred mlds_output_hdr_file(int, mlds, io__state, io__state).
:- mode mlds_output_hdr_file(in, in, di, uo) is det.

mlds_output_hdr_file(Indent, MLDS) -->
	{ MLDS = mlds(ModuleName, ForeignCode, Imports, Defns) },
	{ list__filter(defn_is_public, Defns, PublicDefns) },
	mlds_output_hdr_start(Indent, ModuleName), io__nl,
	mlds_output_hdr_imports(Indent, Imports), io__nl,
	mlds_output_c_hdr_decls(Indent, ForeignCode), io__nl,
	mlds_output_decls(Indent, PublicDefns), io__nl,
	mlds_output_hdr_end(Indent, ModuleName).

:- pred defn_is_public(mlds__defn).
:- mode defn_is_public(in) is semidet.

defn_is_public(Defn) :-
	Defn = mlds__defn(_Name, _Context, Flags, _Body),
	access(Flags) \= private.

:- pred mlds_output_hdr_imports(int, mlds__imports, io__state, io__state).
:- mode mlds_output_hdr_imports(in, in, di, uo) is det.

mlds_output_hdr_imports(Indent, Imports) -->
	list__foldl(mlds_output_hdr_import(Indent), Imports).

:- pred mlds_output_src_imports(int, mlds__imports, io__state, io__state).
:- mode mlds_output_src_imports(in, in, di, uo) is det.

% XXX currently we assume all imports are header imports
mlds_output_src_imports(_Indent, _Imports) --> [].

:- pred mlds_output_hdr_import(int, mlds__import, io__state, io__state).
:- mode mlds_output_hdr_import(in, in, di, uo) is det.

mlds_output_hdr_import(_Indent, Import) -->
	{ SymName = mlds_module_name_to_sym_name(Import) },
	module_name_to_file_name(SymName, ".h", no, HeaderFile),
	io__write_strings(["#include """, HeaderFile, """\n"]).


	%
	% Generate the `.c' file
	%
	% (Calling it the "source" file is a bit of a misnomer,
	% since in our case it is actually the target file,
	% but there's no obvious alternative term to use which
	% also has a clear and concise abbreviation, so never mind...)
	%
:- pred mlds_output_src_file(int, mlds, io__state, io__state).
:- mode mlds_output_src_file(in, in, di, uo) is det.

mlds_output_src_file(Indent, MLDS) -->
	{ MLDS = mlds(ModuleName, ForeignCode, Imports, Defns) },
	{ list__filter(defn_is_public, Defns, _PublicDefns, PrivateDefns) },
	mlds_output_src_start(Indent, ModuleName), io__nl,
	mlds_output_src_imports(Indent, Imports), io__nl,
	mlds_output_c_decls(Indent, ForeignCode), io__nl,
	mlds_output_c_defns(Indent, ForeignCode), io__nl,
	mlds_output_decls(Indent, PrivateDefns), io__nl,
	mlds_output_defns(Indent, Defns), io__nl,
	mlds_output_src_end(Indent, ModuleName).

:- pred mlds_output_hdr_start(int, mercury_module_name, io__state, io__state).
:- mode mlds_output_hdr_start(in, in, di, uo) is det.

mlds_output_hdr_start(Indent, ModuleName) -->
	% XXX should spit out an "automatically generated by ..." comment
	mlds_indent(Indent),
	io__write_string("/* :- module "),
	prog_out__write_sym_name(ModuleName),
	io__write_string(". */\n"),
	mlds_indent(Indent),
	io__write_string("/* :- interface. */\n"),
	io__nl,
	mlds_indent(Indent),
	io__write_string("#include ""mercury_imp.h""\n\n").

:- pred mlds_output_src_start(int, mercury_module_name, io__state, io__state).
:- mode mlds_output_src_start(in, in, di, uo) is det.

mlds_output_src_start(Indent, ModuleName) -->
	% XXX should spit out an "automatically generated by ..." comment
	mlds_indent(Indent),
	io__write_string("/* :- module "),
	prog_out__write_sym_name(ModuleName),
	io__write_string(". */\n\n"),
	mlds_indent(Indent),
	io__write_string("/* :- implementation. */\n"),
	io__nl,
	module_name_to_file_name(ModuleName, ".h", no, HeaderFile),
	io__write_string("#include """),
	io__write_string(HeaderFile),
	io__write_string("""\n"),
	io__nl.

:- pred mlds_output_hdr_end(int, mercury_module_name, io__state, io__state).
:- mode mlds_output_hdr_end(in, in, di, uo) is det.

mlds_output_hdr_end(Indent, ModuleName) -->
	mlds_indent(Indent),
	io__write_string("/* :- end_interface "),
	prog_out__write_sym_name(ModuleName),
	io__write_string(". */\n").

:- pred mlds_output_src_end(int, mercury_module_name, io__state, io__state).
:- mode mlds_output_src_end(in, in, di, uo) is det.

mlds_output_src_end(Indent, ModuleName) -->
	mlds_indent(Indent),
	io__write_string("/* :- end_module "),
	prog_out__write_sym_name(ModuleName),
	io__write_string(". */\n").

%-----------------------------------------------------------------------------%
%
% C interface stuff
%

:- pred mlds_output_c_hdr_decls(int, mlds__foreign_code, io__state, io__state).
:- mode mlds_output_c_hdr_decls(in, in, di, uo) is det.

% XXX not yet implemented
mlds_output_c_hdr_decls(_, _) --> [].

:- pred mlds_output_c_decls(int, mlds__foreign_code, io__state, io__state).
:- mode mlds_output_c_decls(in, in, di, uo) is det.

% XXX not yet implemented
mlds_output_c_decls(_, _) --> [].

:- pred mlds_output_c_defns(int, mlds__foreign_code, io__state, io__state).
:- mode mlds_output_c_defns(in, in, di, uo) is det.

% XXX not yet implemented
mlds_output_c_defns(_, _) --> [].

%-----------------------------------------------------------------------------%
%
% Code to output declarations and definitions
%


:- pred mlds_output_decls(int, mlds__defns, io__state, io__state).
:- mode mlds_output_decls(in, in, di, uo) is det.

mlds_output_decls(Indent, Defns) -->
	list__foldl(mlds_output_decl(Indent), Defns).

:- pred mlds_output_defns(int, mlds__defns, io__state, io__state).
:- mode mlds_output_defns(in, in, di, uo) is det.

mlds_output_defns(Indent, Defns) -->
	list__foldl(mlds_output_defn(Indent), Defns).


:- pred mlds_output_decl(int, mlds__defn, io__state, io__state).
:- mode mlds_output_decl(in, in, di, uo) is det.

mlds_output_decl(Indent, Defn) -->
	{ Defn = mlds__defn(Name, Context, Flags, DefnBody) },
	mlds_output_context(Context),
	mlds_indent(Indent),
	mlds_output_decl_flags(Flags),
	mlds_output_decl_body(Indent, Name, DefnBody).

:- pred mlds_output_defn(int, mlds__defn, io__state, io__state).
:- mode mlds_output_defn(in, in, di, uo) is det.

mlds_output_defn(Indent, Defn) -->
	{ Defn = mlds__defn(Name, Context, Flags, DefnBody) },
	mlds_output_context(Context),
	mlds_indent(Indent),
	mlds_output_decl_flags(Flags),
	mlds_output_defn_body(Indent, Name, DefnBody).


:- pred mlds_output_decl_body(int, mlds__entity_name, mlds__entity_defn,
			io__state, io__state).
:- mode mlds_output_decl_body(in, in, in, di, uo) is det.

mlds_output_decl_body(Indent, Name, DefnBody) -->
	(
		{ DefnBody = mlds__data(Type, _MaybeInitializer) },
		mlds_output_data_decl(Name, Type)
	;
		{ DefnBody = mlds__function(MaybePredProcId, Signature,
			_MaybeBody) },
		mlds_output_maybe(MaybePredProcId, mlds_output_pred_proc_id),
		mlds_output_func_decl(Indent, Name, Signature)
	;
		{ DefnBody = mlds__class(ClassDefn) },
		mlds_output_class_decl(Indent, Name, ClassDefn)
	),
	io__write_string(";\n").

:- pred mlds_output_defn_body(int, mlds__entity_name, mlds__entity_defn,
			io__state, io__state).
:- mode mlds_output_defn_body(in, in, in, di, uo) is det.

mlds_output_defn_body(Indent, Name, DefnBody) -->
	(
		{ DefnBody = mlds__data(Type, MaybeInitializer) },
		mlds_output_data_defn(Name, Type, MaybeInitializer)
	;
		{ DefnBody = mlds__function(MaybePredProcId, Signature,
			MaybeBody) },
		mlds_output_maybe(MaybePredProcId, mlds_output_pred_proc_id),
		mlds_output_func(Indent, Name, Signature, MaybeBody)
	;
		{ DefnBody = mlds__class(ClassDefn) },
		mlds_output_class(Indent, Name, ClassDefn),
		io__write_string(";\n")
	).


:- pred mlds_output_context(mlds__context, io__state, io__state).
:- mode mlds_output_context(in, di, uo) is det.

mlds_output_context(Context) -->
	{ ProgContext = mlds__get_prog_context(Context) },
	{ term__context_file(ProgContext, FileName) },
	{ term__context_line(ProgContext, LineNumber) },
	c_util__set_line_num(FileName, LineNumber).

%-----------------------------------------------------------------------------%
%
% Code to output type declarations/definitions
%

:- pred mlds_output_class(int, mlds__entity_name, mlds__class,
		io__state, io__state).
:- mode mlds_output_class(in, in, in, di, uo) is erroneous.

mlds_output_class(_Indent, _Name, _ClassDefn) -->
	{ error("NYI 3") }.

:- pred mlds_output_class_decl(int, mlds__entity_name, mlds__class,
		io__state, io__state).
:- mode mlds_output_class_decl(in, in, in, di, uo) is erroneous.

mlds_output_class_decl(_Indent, _Name, _ClassDefn) -->
	{ error("NYI 3b") }.

%-----------------------------------------------------------------------------%
%
% Code to output data declarations/definitions
%

:- pred mlds_output_data_decl(mlds__entity_name, mlds__type,
			io__state, io__state).
:- mode mlds_output_data_decl(in, in, di, uo) is det.

mlds_output_data_decl(Name, Type) -->
	mlds_output_type(Type),
	io__write_char(' '),
	mlds_output_name(Name).

:- pred mlds_output_data_defn(mlds__entity_name, mlds__type,
			maybe(mlds__initializer), io__state, io__state).
:- mode mlds_output_data_defn(in, in, in, di, uo) is det.

mlds_output_data_defn(Name, Type, MaybeInitializer) -->
	mlds_output_data_decl(Name, Type),
	mlds_output_maybe(MaybeInitializer,
		mlds_output_initializer(Type)),
	io__write_string(";\n").

:- pred mlds_output_maybe(maybe(T), pred(T, io__state, io__state),
		io__state, io__state).
:- mode mlds_output_maybe(in, pred(in, di, uo) is det, di, uo) is det.

mlds_output_maybe(MaybeValue, OutputAction) -->
	( { MaybeValue = yes(Value) } ->
		OutputAction(Value)
	;
		[]
	).

:- pred mlds_output_initializer(mlds__type, mlds__initializer,
		io__state, io__state).
:- mode mlds_output_initializer(in, in, di, uo) is det.

mlds_output_initializer(_Type, Initializer) -->
	( { Initializer = [SingleValue] } ->
		io__write_string(" = "),
		mlds_output_rval(SingleValue)
	;
		% XXX we should eventually handle these...
		{ error("sorry, complex initializers not yet implemented") }
	).

%-----------------------------------------------------------------------------%
%
% Code to output function declarations/definitions
%

:- pred mlds_output_pred_proc_id(pred_proc_id, io__state, io__state).
:- mode mlds_output_pred_proc_id(in, di, uo) is det.

mlds_output_pred_proc_id(proc(PredId, ProcId)) -->
	io__write_string("/* pred_id: "),
	{ pred_id_to_int(PredId, PredIdNum) },
	io__write_int(PredIdNum),
	io__write_string(", proc_id: "),
	{ proc_id_to_int(ProcId, ProcIdNum) },
	io__write_int(ProcIdNum),
	io__write_string(" */\n").

:- pred mlds_output_func(int, entity_name, func_params, maybe(statement),
			io__state, io__state).
:- mode mlds_output_func(in, in, in, in, di, uo) is det.

mlds_output_func(Indent, Name, Signature, MaybeBody) -->
	mlds_output_func_decl(Indent, Name, Signature),
	(
		{ MaybeBody = no },
		io__write_string(";\n")
	;
		{ MaybeBody = yes(Body) },
		io__write_string("\n"),
		% require Body0 = statement(block(_, _), _)
		mlds_output_statement(Indent, Body)
	).

:- pred mlds_output_func_decl(int, entity_name, func_params, 
			io__state, io__state).
:- mode mlds_output_func_decl(in, in, in, di, uo) is det.

mlds_output_func_decl(Indent, Name, Signature) -->
	{ Signature = mlds__func_params(Parameters, RetTypes) },
	( { RetTypes = [] } ->
		io__write_string("void")
	; { RetTypes = [RetType] } ->
		mlds_output_type(RetType)
	;
		{ error("mlds_output_func: multiple return types") }
	),
	io__write_char(' '),
	mlds_output_name(Name),
	mlds_output_params(Indent, Parameters).

:- pred mlds_output_params(int, assoc_list(entity_name, mlds__type),
				io__state, io__state).
:- mode mlds_output_params(in, in, di, uo) is det.

mlds_output_params(Indent, Parameters) -->
	io__write_char('('),
	io__write_list(Parameters, ", ", mlds_output_param(Indent)),
	io__write_char(')').

:- pred mlds_output_param(int, pair(mlds__entity_name, mlds__type),
		io__state, io__state).
:- mode mlds_output_param(in, in, di, uo) is det.

mlds_output_param(_Indent, Name - Type) -->
	mlds_output_type(Type),
	io__write_char(' '),
	mlds_output_name(Name).

%-----------------------------------------------------------------------------%
%
% Code to output names of various entities
%

:- pred mlds_output_fully_qualified_name(mlds__fully_qualified_name(T),
		pred(T, io__state, io__state), io__state, io__state).
:- mode mlds_output_fully_qualified_name(in, pred(in, di, uo) is det,
		di, uo) is det.

mlds_output_fully_qualified_name(qual(ModuleName, Name), OutputFunc) -->
	{ SymName = mlds_module_name_to_sym_name(ModuleName) },
	{ Separator = "__" },
	{ sym_name_to_string(SymName, Separator, ModuleNameString) },
	io__write_string(ModuleNameString),
	io__write_string(Separator),
	OutputFunc(Name).

:- pred mlds_output_module_name(mercury_module_name, io__state, io__state).
:- mode mlds_output_module_name(in, di, uo) is det.

mlds_output_module_name(ModuleName) -->
	{ Separator = "__" },
	{ sym_name_to_string(ModuleName, Separator, ModuleNameString) },
	io__write_string(ModuleNameString).

:- pred mlds_output_name(mlds__entity_name, io__state, io__state).
:- mode mlds_output_name(in, di, uo) is det.

% XXX FIXME!
% XXX we should escape special characters
% XXX we should avoid appending the arity, modenum, and seqnum
%     if they are not needed.

mlds_output_name(type(Name, Arity)) -->
	io__format("%s_%d", [s(Name), i(Arity)]).
mlds_output_name(data(DataName)) -->
	mlds_output_data_name(DataName).
mlds_output_name(function(PredLabel, ProcId, MaybeSeqNum, _PredId)) -->
	mlds_output_pred_label(PredLabel),
	{ proc_id_to_int(ProcId, ModeNum) },
	io__format("_%d", [i(ModeNum)]),
	( { MaybeSeqNum = yes(SeqNum) } ->
		io__format("_%d", [i(SeqNum)])
	;
		[]
	).

:- pred mlds_output_pred_label(mlds__pred_label, io__state, io__state).
:- mode mlds_output_pred_label(in, di, uo) is det.

mlds_output_pred_label(pred(PredOrFunc, MaybeDefiningModule, Name, Arity)) -->
	( { PredOrFunc = predicate, Suffix = "p" }
	; { PredOrFunc = function, Suffix = "f" }
	),
	io__format("%s_%d_%s", [s(Name), i(Arity), s(Suffix)]),
	( { MaybeDefiningModule = yes(DefiningModule) } ->
		io__write_string("_in__"),
		mlds_output_module_name(DefiningModule)
	;
		[]
	).
mlds_output_pred_label(special_pred(PredName, MaybeTypeModule,
		TypeName, TypeArity)) -->
	io__write_string(PredName),
	io__write_string("__"),
	( { MaybeTypeModule = yes(TypeModule) } ->
		mlds_output_module_name(TypeModule),
		io__write_string("__")
	;
		[]
	),
	io__write_string(TypeName),
	io__write_string("_"),
	io__write_int(TypeArity).

:- pred mlds_output_data_name(mlds__data_name, io__state, io__state).
:- mode mlds_output_data_name(in, di, uo) is det.

% XX some of these should probably be escaped

mlds_output_data_name(var(Name)) -->
	io__write_string(Name).
mlds_output_data_name(common(Num)) -->
	io__write_string("common_"),
	io__write_int(Num).
mlds_output_data_name(type_ctor(_BaseData, _Name, _Arity)) -->
	{ error("mlds_to_c.m: NYI: type_ctor") }.
mlds_output_data_name(base_typeclass_info(_ClassId, _InstanceId)) -->
	{ error("mlds_to_c.m: NYI: basetypeclass_info") }.
mlds_output_data_name(module_layout) -->
	{ error("mlds_to_c.m: NYI: module_layout") }.
mlds_output_data_name(proc_layout(_ProcLabel)) -->
	{ error("mlds_to_c.m: NYI: proc_layout") }.
mlds_output_data_name(internal_layout(_ProcLabel, _FuncSeqNum)) -->
	{ error("mlds_to_c.m: NYI: internal_layout") }.
mlds_output_data_name(tabling_pointer(_ProcLabel)) -->
	{ error("mlds_to_c.m: NYI: tabling_pointer") }.

%-----------------------------------------------------------------------------%
%
% Code to output types
%

:- pred mlds_output_type(mlds__type, io__state, io__state).
:- mode mlds_output_type(in, di, uo) is det.

mlds_output_type(mlds__type(Type)) -->
	( { Type = term__functor(term__atom("character"), [], _) } ->
		io__write_string("char")
	; { Type = term__functor(term__atom("int"), [], _) } ->
		io__write_string("int")
	; { Type = term__functor(term__atom("string"), [], _) } ->
		io__write_string("String")
	; { Type = term__functor(term__atom("float"), [], _) } ->
		io__write_string("Float")
	;
		io__write_string("Word")
	).

%-----------------------------------------------------------------------------%
%
% Code to output declaration specifiers
%

:- pred mlds_output_decl_flags(mlds__decl_flags, io__state, io__state).
:- mode mlds_output_decl_flags(in, di, uo) is det.

mlds_output_decl_flags(Flags) -->
	mlds_output_access(access(Flags)),
	mlds_output_per_instance(per_instance(Flags)),
	mlds_output_virtuality(virtuality(Flags)),
	mlds_output_finality(finality(Flags)),
	mlds_output_constness(constness(Flags)),
	mlds_output_abstractness(abstractness(Flags)).

:- pred mlds_output_access(access, io__state, io__state).
:- mode mlds_output_access(in, di, uo) is det.

mlds_output_access(public)    --> io__write_string("/* public: */ ").
mlds_output_access(private)   --> io__write_string("/* private: */ ").
mlds_output_access(protected) --> io__write_string("/* protected: */ ").
mlds_output_access(default)   --> [].

:- pred mlds_output_per_instance(per_instance, io__state, io__state).
:- mode mlds_output_per_instance(in, di, uo) is det.

mlds_output_per_instance(one_copy)     --> io__write_string("static ").
mlds_output_per_instance(per_instance) --> [].

:- pred mlds_output_virtuality(virtuality, io__state, io__state).
:- mode mlds_output_virtuality(in, di, uo) is det.

mlds_output_virtuality(virtual)     --> io__write_string("virtual ").
mlds_output_virtuality(non_virtual) --> [].

:- pred mlds_output_finality(finality, io__state, io__state).
:- mode mlds_output_finality(in, di, uo) is det.

mlds_output_finality(final)       --> io__write_string("final ").
mlds_output_finality(overridable) --> [].

:- pred mlds_output_constness(constness, io__state, io__state).
:- mode mlds_output_constness(in, di, uo) is det.

mlds_output_constness(const)      --> io__write_string("const ").
mlds_output_constness(modifiable) --> [].

:- pred mlds_output_abstractness(abstractness, io__state, io__state).
:- mode mlds_output_abstractness(in, di, uo) is det.

mlds_output_abstractness(abstract) --> io__write_string("/* abstract */ ").
mlds_output_abstractness(concrete) --> [].

%-----------------------------------------------------------------------------%
%
% Code to output statements
%

:- pred mlds_output_statements(int, list(mlds__statement),
				io__state, io__state).
:- mode mlds_output_statements(in, in, di, uo) is det.

mlds_output_statements(Indent, Statements) -->
	list__foldl(mlds_output_statement(Indent), Statements).

:- pred mlds_output_statement(int, mlds__statement, io__state, io__state).
:- mode mlds_output_statement(in, in, di, uo) is det.

mlds_output_statement(Indent, mlds__statement(Statement, Context)) -->
	mlds_output_context(Context),
	mlds_output_stmt(Indent, Statement).

:- pred mlds_output_stmt(int, mlds__stmt, io__state, io__state).
:- mode mlds_output_stmt(in, in, di, uo) is det.

	%
	% sequence
	%
mlds_output_stmt(Indent, block(Defns, Statements)) -->
	mlds_indent(Indent),
	io__write_string("{\n"),
	( { Defns \= [] } ->
		mlds_output_defns(Indent + 1, Defns),
		io__write_string("\n")
	;
		[]
	),
	mlds_output_statements(Indent + 1, Statements),
	mlds_indent(Indent),
	io__write_string("}\n").

	%
	% iteration
	%
mlds_output_stmt(Indent, while(Cond, Statement, no)) -->
	mlds_indent(Indent),
	io__write_string("while ("),
	mlds_output_rval(Cond),
	io__write_string(")\n"),
	mlds_output_statement(Indent + 1, Statement).
mlds_output_stmt(Indent, while(Cond, Statement, yes)) -->
	mlds_indent(Indent),
	io__write_string("do {\n"),
	mlds_output_statement(Indent + 1, Statement),
	mlds_indent(Indent),
	io__write_string("} while ("),
	mlds_output_rval(Cond),
	io__write_string(");\n").

	%
	% selection (see also computed_goto)
	%
mlds_output_stmt(Indent, if_then_else(Cond, Then0, MaybeElse)) -->
	%
	% we need to take care to avoid problems caused by the
	% dangling else ambiguity
	%
	{
		MaybeElse = yes(_),
		Then0 = statement(if_then_else(_, _, no), Context)
	->
		Then = statement(block([], [Then0]), Context)
	;
		Then = Then0
	},
	mlds_indent(Indent),
	io__write_string("if ("),
	mlds_output_rval(Cond),
	io__write_string(")\n"),
	mlds_output_statement(Indent + 1, Then),
	( { MaybeElse = yes(Else) } ->
		mlds_indent(Indent),
		io__write_string("else\n"),
		mlds_output_statement(Indent + 1, Else)
	;
		[]
	).

	%
	% transfer of control
	%
mlds_output_stmt(Indent, label(LabelName)) -->
	%
	% Note: MLDS allows labels at the end of blocks.
	% C doesn't.  Hence we need to insert a semi-colon after the colon
	% to ensure that there is a statement to attach the label to.
	%
	mlds_indent(Indent - 1),
	mlds_output_label_name(LabelName),
	io__write_string(":;\n").
mlds_output_stmt(Indent, goto(LabelName)) -->
	mlds_indent(Indent),
	io__write_string("goto "),
	mlds_output_label_name(LabelName),
	io__write_string(";\n").
mlds_output_stmt(Indent, computed_goto(Expr, Labels)) -->
	% XXX for GNU C, we could output potentially more efficient code
	% by using an array of labels; this would tell the compiler that
	% it didn't need to do any range check.
	mlds_indent(Indent),
	io__write_string("switch ("),
	mlds_output_rval(Expr),
	io__write_string(") {"),
	{ OutputLabel =
	    (pred(Label::in, Count0::in, Count::out, di, uo) is det -->
		mlds_indent(Indent + 1),
		io__write_string("case "),
		io__write_int(Count0),
		io__write_string(": goto "),
		mlds_output_label_name(Label),
		io__write_string(";\n"),
		{ Count = Count0 + 1 }
	) },
	list__foldl2(OutputLabel, Labels, 0, _FinalCount),
	mlds_indent(Indent + 1),
	io__write_string("default: /*NOTREACHED*/ assert(0);\n"),
	mlds_indent(Indent),
	io__write_string("}\n").

	%
	% function call/return
	%
mlds_output_stmt(Indent, call(_Signature, Func, MaybeObject, Args, Results,
				IsTailCall)) -->
	mlds_indent(Indent),
	( { IsTailCall = tail_call } ->
		io__write_string("return ")
	;
		[]
	),
	( { MaybeObject = yes(Object) } ->
		mlds_output_bracketed_rval(Object),
		io__write_string(".")
	;
		[]
	),
	( { Results = [] } ->
		[]
	; { Results = [Lval] } ->
		mlds_output_lval(Lval),
		io__write_string(" = ")
	;
		{ error("mld_output_stmt: multiple return values") }
	),
	mlds_output_bracketed_rval(Func),
	io__write_string("("),
	io__write_list(Args, ", ", mlds_output_rval),
	io__write_string(");\n").

mlds_output_stmt(Indent, return(Results)) -->
	mlds_indent(Indent),
	io__write_string("return"),
	( { Results = [] } ->
		[]
	; { Results = [Rval] } ->
		io__write_char(' '),
		mlds_output_rval(Rval)
	;
		{ error("mld_output_stmt: multiple return values") }
	),
	io__write_string(";\n").
	
	%
	% exception handling
	%

	/* XXX not yet implemented */

	%
	% atomic statements
	%
mlds_output_stmt(Indent, atomic(AtomicStatement)) -->
	mlds_output_atomic_stmt(Indent, AtomicStatement).

:- pred mlds_output_label_name(mlds__label, io__state, io__state).
:- mode mlds_output_label_name(in, di, uo) is det.

mlds_output_label_name(LabelName) -->
	io__write_string(LabelName).

:- pred mlds_output_atomic_stmt(int, mlds__atomic_statement,
				io__state, io__state).
:- mode mlds_output_atomic_stmt(in, in, di, uo) is det.

	%
	% comments
	%
mlds_output_atomic_stmt(Indent, comment(Comment)) -->
	% XXX we should escape any "*/"'s in the Comment.
	%     we should also split the comment into lines and indent
	%     each line appropriately.
	mlds_indent(Indent),
	io__write_string("/* "),
	io__write_string(Comment),
	io__write_string(" */").

	%
	% assignment
	%
mlds_output_atomic_stmt(Indent, assign(Lval, Rval)) -->
	mlds_indent(Indent),
	mlds_output_lval(Lval),
	io__write_string(" = "),
	mlds_output_rval(Rval),
	io__write_string(";\n").

	%
	% heap management
	%
mlds_output_atomic_stmt(Indent, new_object(Target,
		MaybeTag, Type, MaybeSize, MaybeCtorName, Args, ArgTypes)) -->
	mlds_indent(Indent),
	mlds_output_lval(Target),
	io__write_string(" = "),
	( { MaybeTag = yes(Tag) } ->
		io__write_string("MR_mkword("),
		mlds_output_tag(Tag),
		io__write_string(", "),
		{ EndMkword = ")" }
	;
		{ EndMkword = "" }
	),
	io__write_string("MR_new_object("),
	mlds_output_type(Type),
	io__write_string(", "),
	( { MaybeSize = yes(Size) } ->
		mlds_output_rval(Size)
	;
		% XXX what should we do here?
		io__write_int(-1)
	),
	io__write_string(", "),
	( { MaybeCtorName = yes(CtorName) } ->
		io__write_string(CtorName)
	;
		io__write_string("NULL")
	),
	io__write_string(")"),
	io__write_string(EndMkword),
	io__write_string(";\n"),
	%
	% XXX we should handle the constructor arguments / initializer
	%
	( { Args = [], ArgTypes = [] } ->
		[]
	;
		{ error("mlds_output_atomic_stmt: new_object initializer") }
	).

mlds_output_atomic_stmt(Indent, mark_hp(Lval)) -->
	mlds_indent(Indent),
	io__write_string("MR_mark_hp("),
	mlds_output_lval(Lval),
	io__write_string(");\n").

mlds_output_atomic_stmt(Indent, restore_hp(Rval)) -->
	mlds_indent(Indent),
	io__write_string("MR_mark_hp("),
	mlds_output_rval(Rval),
	io__write_string(");\n").

	%
	% trail management
	%
mlds_output_atomic_stmt(_Indent, trail_op(_TrailOp)) -->
	{ error("mlds_to_c.m: sorry, trail_ops not implemented") }.

	%
	% foreign language interfacing
	%
mlds_output_atomic_stmt(_Indent, target_code(_TargetLang, _CodeString)) -->
	{ error("mlds_to_c.m: sorry, target_code not implemented") }.
/*
		target_code(target_lang, string)
			% Do whatever is specified by the string,
			% which can be any piece of code in the specified
			% target language (C, assembler, or whatever)
			% that does not have any non-local flow of control.
*/

%-----------------------------------------------------------------------------%
%
% Code to output expressions
%

:- pred mlds_output_lval(mlds__lval, io__state, io__state).
:- mode mlds_output_lval(in, di, uo) is det.

mlds_output_lval(field(_MaybeTag, _Rval, _FieldId)) -->
	{ error("mlds.m: sorry, not yet implemented: field") }.
mlds_output_lval(mem_ref(Rval)) -->
	io__write_string("*"),
	mlds_output_bracketed_rval(Rval).
mlds_output_lval(var(VarName)) -->
	mlds_output_fully_qualified_name(VarName, io__write_string).

:- pred mlds_output_bracketed_rval(mlds__rval, io__state, io__state).
:- mode mlds_output_bracketed_rval(in, di, uo) is det.

mlds_output_bracketed_rval(Rval) -->
	io__write_char('('),
	mlds_output_rval(Rval),
	io__write_char(')').

:- pred mlds_output_rval(mlds__rval, io__state, io__state).
:- mode mlds_output_rval(in, di, uo) is det.

mlds_output_rval(lval(Lval)) -->
	mlds_output_lval(Lval).
/**** XXX do we need this?
mlds_output_rval(lval(Lval)) -->
	% if a field is used as an rval, then we need to use
	% the MR_const_field() macro, not the MR_field() macro,
	% to avoid warnings about discarding const,
	% and similarly for MR_mask_field.
	( { Lval = field(MaybeTag, Rval, FieldNum) } ->
		( { MaybeTag = yes(Tag) } ->
			io__write_string("MR_const_field("),
			mlds_output_tag(Tag),
			io__write_string(", ")
		;
			io__write_string("MR_const_mask_field(")
		),
		mlds_output_rval(Rval),
		io__write_string(", "),
		mlds_output_rval(FieldNum),
		io__write_string(")")
	;
		mlds_output_lval(Lval)
	).
****/

mlds_output_rval(mkword(Tag, Rval)) -->
	io__write_string("MR_mkword("),
	mlds_output_tag(Tag),
	io__write_string(", "),
	mlds_output_rval(Rval),
	io__write_string(")").

mlds_output_rval(const(Const)) -->
	mlds_output_rval_const(Const).

mlds_output_rval(unop(Op, Rval)) -->
	mlds_output_unop(Op, Rval).

mlds_output_rval(binop(Op, Rval1, Rval2)) -->
	mlds_output_binop(Op, Rval1, Rval2).

mlds_output_rval(mem_addr(Lval)) -->
	% XXX are parentheses needed?
	io__write_string("&"),
	mlds_output_lval(Lval).

:- pred mlds_output_unop(unary_op, mlds__rval, io__state, io__state).
:- mode mlds_output_unop(in, in, di, uo) is det.
	
mlds_output_unop(UnaryOp, Exprn) -->
	{ c_util__unary_prefix_op(UnaryOp, UnaryOpString) },
	io__write_string(UnaryOpString),
	io__write_string("("),
	mlds_output_rval(Exprn),
	io__write_string(")").

:- pred mlds_output_binop(binary_op, mlds__rval, mlds__rval,
			io__state, io__state).
:- mode mlds_output_binop(in, in, in, di, uo) is det.
	
mlds_output_binop(Op, X, Y) -->
	(
		{ Op = array_index }
	->
		mlds_output_bracketed_rval(X),
		io__write_string("["),
		mlds_output_rval(Y),
		io__write_string("]")
	;
		{ c_util__string_compare_op(Op, OpStr) }
	->
		io__write_string("(strcmp("),
		mlds_output_rval(X),
		io__write_string(", "),
		mlds_output_rval(Y),
		io__write_string(")"),
		io__write_string(" "),
		io__write_string(OpStr),
		io__write_string(" "),
		io__write_string("0)")
	;
		( { c_util__float_compare_op(Op, OpStr1) } ->
			{ OpStr = OpStr1 }
		; { c_util__float_op(Op, OpStr2) } ->
			{ OpStr = OpStr2 }
		;
			{ fail }
		)
	->
		io__write_string("("),
		mlds_output_bracketed_rval(X), % XXX as float
		io__write_string(" "),
		io__write_string(OpStr),
		io__write_string(" "),
		mlds_output_bracketed_rval(Y), % XXX as float
		io__write_string(")")
	;
/****
XXX broken for C == minint
(since `NewC is 0 - C' overflows)
		{ Op = (+) },
		{ Y = const(int_const(C)) },
		{ C < 0 }
	->
		{ NewOp = (-) },
		{ NewC is 0 - C },
		{ NewY = const(int_const(NewC)) },
		io__write_string("("),
		mlds_output_rval(X),
		io__write_string(" "),
		mlds_output_binary_op(NewOp),
		io__write_string(" "),
		mlds_output_rval(NewY),
		io__write_string(")")
	;
******/
		io__write_string("("),
		mlds_output_rval(X),
		io__write_string(" "),
		mlds_output_binary_op(Op),
		io__write_string(" "),
		mlds_output_rval(Y),
		io__write_string(")")
	).

:- pred mlds_output_binary_op(binary_op, io__state, io__state).
:- mode mlds_output_binary_op(in, di, uo) is erroneous.

mlds_output_binary_op(_Op) -->
	{ error("NYI 7") }.
	% XXX
	% { c_util__binary_op(Op, OpStr) },
	% io__write_string(OpStr).

:- pred mlds_output_rval_const(mlds__rval_const, io__state, io__state).
:- mode mlds_output_rval_const(in, di, uo) is det.

mlds_output_rval_const(true) -->
	io__write_string("TRUE").	% XXX should we use `MR_TRUE'?
mlds_output_rval_const(false) -->
	io__write_string("FALSE").	% XXX should we use `MR_FALSE'?
mlds_output_rval_const(int_const(N)) -->
	% we need to cast to (Integer) to ensure
	% things like 1 << 32 work when `Integer' is 64 bits
	% but `int' is 32 bits.
	io__write_string("(Integer) "),
	io__write_int(N).
mlds_output_rval_const(float_const(FloatVal)) -->
	% the cast to (Float) here lets the C compiler
	% do arithmetic in `float' rather than `double'
	% if `Float' is `float' not `double'.
	io__write_string("(Float) "),
	io__write_float(FloatVal).
mlds_output_rval_const(string_const(String)) -->
	io__write_string(""""),
	c_util__output_quoted_string(String),
	io__write_string("""").
mlds_output_rval_const(multi_string_const(Length, String)) -->
	io__write_string(""""),
	c_util__output_quoted_multi_string(Length, String),
	io__write_string("""").
mlds_output_rval_const(code_addr_const(CodeAddr)) -->
	mlds_output_code_addr(CodeAddr).
mlds_output_rval_const(data_addr_const(DataAddr)) -->
	mlds_output_data_addr(DataAddr).

%-----------------------------------------------------------------------------%

:- pred mlds_output_tag(tag, io__state, io__state).
:- mode mlds_output_tag(in, di, uo) is det.

mlds_output_tag(Tag) -->
	io__write_string("MR_mktag("),
	io__write_int(Tag),
	io__write_string(")").

%-----------------------------------------------------------------------------%

:- pred mlds_output_code_addr(mlds__code_addr, io__state, io__state).
:- mode mlds_output_code_addr(in, di, uo) is det.

mlds_output_code_addr(proc(Label)) -->
	mlds_output_fully_qualified_name(Label, mlds_output_proc_label).
mlds_output_code_addr(internal(Label, SeqNum)) -->
	mlds_output_fully_qualified_name(Label, mlds_output_proc_label),
	io__write_string("_"),
	io__write_int(SeqNum).

:- pred mlds_output_proc_label(mlds__proc_label, io__state, io__state).
:- mode mlds_output_proc_label(in, di, uo) is det.

mlds_output_proc_label(PredLabel - ProcId) -->
	mlds_output_pred_label(PredLabel),
	{ proc_id_to_int(ProcId, ModeNum) },
	io__format("_%d", [i(ModeNum)]).

:- pred mlds_output_data_addr(mlds__data_addr, io__state, io__state).
:- mode mlds_output_data_addr(in, di, uo) is det.

mlds_output_data_addr(data_addr(_ModuleName, DataName)) -->
	io__write_string("/* XXX ModuleName */"),
	mlds_output_data_name(DataName).

%-----------------------------------------------------------------------------%
%
% Miscellaneous
%

:- pred mlds_indent(int, io__state, io__state).
:- mode mlds_indent(in, di, uo) is det.

mlds_indent(N) -->
	( { N =< 0 } ->
		[]
	;
		io__write_string("  "),
		mlds_indent(N - 1)
	).

%-----------------------------------------------------------------------------%

/*****

:- type base_data
	--->	info
	;	functors
	;	layout.

	% see runtime/mercury_trail.h
:- type reset_trail_reason
	--->	undo
	;	commit
	;	solve
	;	exception
	;	gc
	.

:- type mlds__qualified_proc_label
	==	mlds__fully_qualified_name(mlds__proc_label).
:- type mlds__proc_label
	==	pair(mlds__pred_label, proc_id).

:- type mlds__qualified_pred_label
	==	mlds__fully_qualified_name(mlds__pred_label).

:- type field_id == mlds__fully_qualified_name(field_name).
:- type field_name == string.

:- type mlds__var == mlds__fully_qualified_name(mlds__var_name).
:- type mlds__var_name == string.

*****/

/**************************
% An mlds__module_name specifies the name of an mlds package or class.
:- type mlds__module_name.

% An mlds__package_name specifies the name of an mlds package.
:- type mlds__package_name == mlds__module_name.

% Given the name of a Mercury module, return the name of the corresponding
% MLDS package.
:- func mercury_module_name_to_mlds(mercury_module_name) = mlds__package_name.

:- type mlds__qualified_entity_name
	==	mlds__fully_qualified_name(mlds__entity_name).

:- type mlds__class_kind
	--->	mlds__class		% A generic class:
					% can inherit other classes and
					% interfaces
					% (but most targets will only support
					% single inheritence, so usually there
					% will be at most one class).
	;	mlds__package		% A class with only static members
					% (can only inherit other packages).
					% Unlike other kinds of classes,
					% packages should not be used as types.
	;	mlds__interface		% A class with no variable data members
					% (can only inherit other interfaces)
	;	mlds__struct		% A value class
					% (can only inherit other structs).
	;	mlds__enum		% A class with one integer member and
					% a bunch of static consts
					% (cannot inherit anything).
	.

:- type mlds__class
	---> mlds__class(
		mlds__class_kind,
		mlds__imports,			% imports these classes (or
						% modules, packages, ...)
		list(mlds__class_id),		% inherits these base classes
		list(mlds__interface_id),	% implements these interfaces
		mlds__defns			% contains these members
	).

:- type mlds__type.
:- type mercury_type == prog_data__type.

:- func mercury_type_to_mlds_type(mercury_type) = mlds__type.

% Hmm... this is tentative.
:- type mlds__class_id == mlds__type.
:- type mlds__interface_id == mlds__type.

%-----------------------------------------------------------------------------%

	%
	% C code required for the C interface.
	% When compiling to a language other than C,
	% this part still needs to be generated as C code
	% and compiled with a C compiler.
	%
:- type mlds__foreign_code
	---> mlds__foreign_code(
		c_header_info,
		list(user_c_code),
		list(c_export)		% XXX we will need to modify
					% export.m to handle different
					% target languages
	).

%-----------------------------------------------------------------------------%

**************************/
