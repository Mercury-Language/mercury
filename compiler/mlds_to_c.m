%-----------------------------------------------------------------------------%
% Copyright (C) 1999-2000 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% mlds_to_c - Convert MLDS to C/C++ code.
% Main author: fjh.

% TODO:
%	- RTTI (base_type_layout, base_type_functors,
%		module_layout, proc_layout)
%	- type classes (base_typeclass_info)
%	- trail ops
%	- foreign language interfacing and inline target code
%	- packages, classes and inheritance
%	  (currently we just generate all classes as structs)

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

:- import_module llds.		% XXX needed for C interface types
:- import_module llds_out.	% XXX needed for llds_out__name_mangle.
:- import_module rtti.		% for rtti__addr_to_string.
:- import_module rtti_to_mlds.	% for mlds_rtti_type_name.
:- import_module hlds_pred.	% for `pred_proc_id'.
:- import_module globals, options, passes_aux.
:- import_module builtin_ops, c_util, modules.
:- import_module prog_data, prog_out.

:- import_module bool, int, string, list, term, std_util, require.

%-----------------------------------------------------------------------------%

mlds_to_c__output_mlds(MLDS) -->
	%
	% We need to use the MLDS package name to compute the header file
	% names, giving e.g. `mercury.io.h', `mercury.time.h' etc.,
	% rather than using just the Mercury module name, which would give
	% just `io.h', `time.h', etc.  The reason for this is that if we
	% don't, then we get name clashes with the standard C header files.
	% For example, `time.h' clashes with the standard <time.h> header.
	%
	% But to keep the Mmake auto-dependencies working (or at least
	% _mostly_ working!), we still want to name the `.c' file based
	% on just the Mercury module name, giving e.g. `time.c', not
	% `mercury.time.c'.
	%
	% Hence the different treatment of SourceFile and HeaderFile below.
	%
	{ ModuleName = mlds__get_module_name(MLDS) },
	module_name_to_file_name(ModuleName, ".c", yes, SourceFile),
	{ MLDS_ModuleName = mercury_module_name_to_mlds(ModuleName) },
	{ ModuleSymName = mlds_module_name_to_sym_name(MLDS_ModuleName) },
	module_name_to_file_name(ModuleSymName, ".h", yes, HeaderFile),
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
:- pred mlds_output_hdr_file(indent, mlds, io__state, io__state).
:- mode mlds_output_hdr_file(in, in, di, uo) is det.

mlds_output_hdr_file(Indent, MLDS) -->
	{ MLDS = mlds(ModuleName, ForeignCode, Imports, Defns) },
	{ list__filter(defn_is_public, Defns, PublicDefns) },
	mlds_output_hdr_start(Indent, ModuleName), io__nl,
	mlds_output_hdr_imports(Indent, Imports), io__nl,
	mlds_output_c_hdr_decls(Indent, ForeignCode), io__nl,
	{ MLDS_ModuleName = mercury_module_name_to_mlds(ModuleName) },
	mlds_output_decls(Indent, MLDS_ModuleName, PublicDefns), io__nl,
	mlds_output_hdr_end(Indent, ModuleName).

:- pred defn_is_public(mlds__defn).
:- mode defn_is_public(in) is semidet.

defn_is_public(Defn) :-
	Defn = mlds__defn(_Name, _Context, Flags, _Body),
	access(Flags) \= private.

:- pred defn_is_commit_type_var(mlds__defn).
:- mode defn_is_commit_type_var(in) is semidet.

defn_is_commit_type_var(Defn) :-
	Defn = mlds__defn(_Name, _Context, _Flags, Body),
	Body = mlds__data(Type, _),
	Type = mlds__commit_type.

:- pred mlds_output_hdr_imports(indent, mlds__imports, io__state, io__state).
:- mode mlds_output_hdr_imports(in, in, di, uo) is det.

% XXX currently we assume all imports are source imports,
% i.e. that the header file does not depend on any types
% defined in other header files.
mlds_output_hdr_imports(_Indent, _Imports) --> [].

:- pred mlds_output_src_imports(indent, mlds__imports, io__state, io__state).
:- mode mlds_output_src_imports(in, in, di, uo) is det.

mlds_output_src_imports(Indent, Imports) -->
	list__foldl(mlds_output_src_import(Indent), Imports).

:- pred mlds_output_src_import(indent, mlds__import, io__state, io__state).
:- mode mlds_output_src_import(in, in, di, uo) is det.

mlds_output_src_import(_Indent, Import) -->
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
:- pred mlds_output_src_file(indent, mlds, io__state, io__state).
:- mode mlds_output_src_file(in, in, di, uo) is det.

mlds_output_src_file(Indent, MLDS) -->
	{ MLDS = mlds(ModuleName, ForeignCode, Imports, Defns) },
	{ list__filter(defn_is_public, Defns, _PublicDefns, PrivateDefns) },
	mlds_output_src_start(Indent, ModuleName), io__nl,
	mlds_output_src_imports(Indent, Imports), io__nl,
	mlds_output_c_decls(Indent, ForeignCode), io__nl,
	mlds_output_c_defns(Indent, ForeignCode), io__nl,
	{ MLDS_ModuleName = mercury_module_name_to_mlds(ModuleName) },
	mlds_output_decls(Indent, MLDS_ModuleName, PrivateDefns), io__nl,
	mlds_output_defns(Indent, MLDS_ModuleName, Defns), io__nl,
	mlds_output_src_end(Indent, ModuleName).

:- pred mlds_output_hdr_start(indent, mercury_module_name,
		io__state, io__state).
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
	io__write_string("#ifndef MR_HEADER_GUARD_"),
	{ llds_out__sym_name_mangle(ModuleName, MangledModuleName) },
	io__write_string(MangledModuleName),
	io__nl,
	mlds_indent(Indent),
	io__write_string("#define MR_HEADER_GUARD_"),
	io__write_string(MangledModuleName),
	io__nl,
	io__nl,
	mlds_indent(Indent),
	io__write_string("#include ""mercury.h""\n").

:- pred mlds_output_src_start(indent, mercury_module_name,
		io__state, io__state).
:- mode mlds_output_src_start(in, in, di, uo) is det.

mlds_output_src_start(Indent, ModuleName) -->
	% XXX should spit out an "automatically generated by ..." comment
	mlds_indent(Indent),
	io__write_string("/* :- module "),
	prog_out__write_sym_name(ModuleName),
	io__write_string(". */\n"),
	mlds_indent(Indent),
	io__write_string("/* :- implementation. */\n"),
	io__nl,
	mlds_output_src_import(Indent,
		mercury_module_name_to_mlds(ModuleName)),
	io__nl.

:- pred mlds_output_hdr_end(indent, mercury_module_name,
		io__state, io__state).
:- mode mlds_output_hdr_end(in, in, di, uo) is det.

mlds_output_hdr_end(Indent, ModuleName) -->
	mlds_indent(Indent),
	io__write_string("#endif /* MR_HEADER_GUARD_"),
	prog_out__write_sym_name(ModuleName),
	io__write_string(" */\n"),
	io__nl,
	mlds_indent(Indent),
	io__write_string("/* :- end_interface "),
	prog_out__write_sym_name(ModuleName),
	io__write_string(". */\n").

:- pred mlds_output_src_end(indent, mercury_module_name,
		io__state, io__state).
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

:- pred mlds_output_c_hdr_decls(indent, mlds__foreign_code,
		io__state, io__state).
:- mode mlds_output_c_hdr_decls(in, in, di, uo) is det.

mlds_output_c_hdr_decls(Indent, ForeignCode) -->
	% XXX we don't yet handle `pragma export' decls
	{ ForeignCode = mlds__foreign_code(RevHeaderCode, _RevBodyCode,
		_ExportDefns) },
	{ HeaderCode = list__reverse(RevHeaderCode) },
	io__write_list(HeaderCode, "\n", mlds_output_c_hdr_decl(Indent)).

:- pred mlds_output_c_hdr_decl(indent, c_header_code, io__state, io__state).
:- mode mlds_output_c_hdr_decl(in, in, di, uo) is det.

mlds_output_c_hdr_decl(_Indent, Code - Context) -->
	mlds_output_context(mlds__make_context(Context)),
	io__write_string(Code).

:- pred mlds_output_c_decls(indent, mlds__foreign_code, io__state, io__state).
:- mode mlds_output_c_decls(in, in, di, uo) is det.

% all of the declarations go in the header file or as c_code
mlds_output_c_decls(_, _) --> [].

:- pred mlds_output_c_defns(indent, mlds__foreign_code, io__state, io__state).
:- mode mlds_output_c_defns(in, in, di, uo) is det.

mlds_output_c_defns(Indent, ForeignCode) -->
	% XXX export decls
	{ ForeignCode = mlds__foreign_code(_RevHeaderCode, RevBodyCode,
		_ExportDefns) },
	{ BodyCode = list__reverse(RevBodyCode) },
	io__write_list(BodyCode, "\n", mlds_output_c_defn(Indent)).

:- pred mlds_output_c_defn(indent, user_c_code, io__state, io__state).
:- mode mlds_output_c_defn(in, in, di, uo) is det.

mlds_output_c_defn(_Indent, user_c_code(Code, Context)) -->
	mlds_output_context(mlds__make_context(Context)),
	io__write_string(Code).

%-----------------------------------------------------------------------------%
%
% Code to output declarations and definitions
%


:- pred mlds_output_decls(indent, mlds_module_name, mlds__defns,
		io__state, io__state).
:- mode mlds_output_decls(in, in, in, di, uo) is det.

mlds_output_decls(Indent, ModuleName, Defns) -->
	list__foldl(mlds_output_decl(Indent, ModuleName), Defns).

:- pred mlds_output_defns(indent, mlds_module_name, mlds__defns,
		io__state, io__state).
:- mode mlds_output_defns(in, in, in, di, uo) is det.

mlds_output_defns(Indent, ModuleName, Defns) -->
	{ OutputDefn = mlds_output_defn(Indent, ModuleName) },
	globals__io_lookup_bool_option(gcc_local_labels, GCC_LocalLabels),
	( { GCC_LocalLabels = yes } ->
		%
		% GNU C __label__ declarations must precede
		% ordinary variable declarations.
		%
		{ list__filter(defn_is_commit_type_var, Defns, LabelDecls,
			OtherDefns) },
		list__foldl(OutputDefn, LabelDecls),
		list__foldl(OutputDefn, OtherDefns)
	;
		list__foldl(OutputDefn, Defns)
	).


:- pred mlds_output_decl(indent, mlds_module_name, mlds__defn,
		io__state, io__state).
:- mode mlds_output_decl(in, in, in, di, uo) is det.

mlds_output_decl(Indent, ModuleName, Defn) -->
	{ Defn = mlds__defn(Name, Context, Flags, DefnBody) },
	mlds_indent(Context, Indent),
	( { Name = data(_) } ->
		% XXX for private data and private functions,
		% we should use "static"
		io__write_string("extern ")
	;
		[]
	),
	mlds_output_decl_flags(Flags),
	mlds_output_decl_body(Indent, qual(ModuleName, Name), DefnBody).

:- pred mlds_output_defn(indent, mlds_module_name, mlds__defn,
		io__state, io__state).
:- mode mlds_output_defn(in, in, in, di, uo) is det.

mlds_output_defn(Indent, ModuleName, Defn) -->
	{ Defn = mlds__defn(Name, Context, Flags, DefnBody) },
	( { DefnBody \= mlds__data(_, _) } ->
		io__nl
	;
		[]
	),
	mlds_indent(Context, Indent),
	mlds_output_decl_flags(Flags),
	mlds_output_defn_body(Indent, qual(ModuleName, Name), Context,
			DefnBody).

:- pred mlds_output_decl_body(indent, mlds__qualified_entity_name,
		mlds__entity_defn, io__state, io__state).
:- mode mlds_output_decl_body(in, in, in, di, uo) is det.

mlds_output_decl_body(Indent, Name, DefnBody) -->
	(
		{ DefnBody = mlds__data(Type, _Initializer) },
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

:- pred mlds_output_defn_body(indent, mlds__qualified_entity_name,
		mlds__context, mlds__entity_defn, io__state, io__state).
:- mode mlds_output_defn_body(in, in, in, in, di, uo) is det.

mlds_output_defn_body(Indent, Name, Context, DefnBody) -->
	(
		{ DefnBody = mlds__data(Type, Initializer) },
		mlds_output_data_defn(Name, Type, Initializer)
	;
		{ DefnBody = mlds__function(MaybePredProcId, Signature,
			MaybeBody) },
		mlds_output_maybe(MaybePredProcId, mlds_output_pred_proc_id),
		mlds_output_func(Indent, Name, Context, Signature, MaybeBody)
	;
		{ DefnBody = mlds__class(ClassDefn) },
		mlds_output_class(Indent, Name, Context, ClassDefn),
		io__write_string(";\n")
	).


%-----------------------------------------------------------------------------%
%
% Code to output type declarations/definitions
%

:- pred mlds_output_class_decl(indent, mlds__qualified_entity_name,
		mlds__class_defn, io__state, io__state).
:- mode mlds_output_class_decl(in, in, in, di, uo) is det.

mlds_output_class_decl(_Indent, Name, _ClassDefn) -->
	io__write_string("struct "),
	mlds_output_fully_qualified_name(Name).

:- pred mlds_output_class(indent, mlds__qualified_entity_name, mlds__context,
		mlds__class_defn, io__state, io__state).
:- mode mlds_output_class(in, in, in, in, di, uo) is det.

mlds_output_class(Indent, Name, Context, ClassDefn) -->
	mlds_output_class_decl(Indent, Name, ClassDefn),
	io__write_string(" {\n"),
	{ ClassDefn = class_defn(_Kind, _Imports, _BaseClasses, _Implements,
		Defns) },
	{ Name = qual(ModuleName, _) },
	mlds_output_defns(Indent + 1, ModuleName, Defns),
	mlds_indent(Context, Indent),
	io__write_string("}").

%-----------------------------------------------------------------------------%
%
% Code to output data declarations/definitions
%

:- pred mlds_output_data_decl(mlds__qualified_entity_name, mlds__type,
			io__state, io__state).
:- mode mlds_output_data_decl(in, in, di, uo) is det.

mlds_output_data_decl(Name, Type) -->
	mlds_output_type(Type),
	io__write_char(' '),
	mlds_output_fully_qualified_name(Name).

:- pred mlds_output_data_defn(mlds__qualified_entity_name, mlds__type,
			mlds__initializer, io__state, io__state).
:- mode mlds_output_data_defn(in, in, in, di, uo) is det.

mlds_output_data_defn(Name, Type, Initializer) -->
	mlds_output_data_decl(Name, Type),
	mlds_output_initializer(Type, Initializer),
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
	( { Initializer = no_initializer } ->
		[]
	;
		io__write_string(" = "),
		mlds_output_initializer_body(Initializer)
	).

:- pred mlds_output_initializer_body(mlds__initializer, io__state, io__state).
:- mode mlds_output_initializer_body(in, di, uo) is det.

mlds_output_initializer_body(no_initializer) --> [].
mlds_output_initializer_body(init_obj(Rval)) -->
	mlds_output_rval(Rval).
mlds_output_initializer_body(init_struct(FieldInits)) -->
	io__write_string("{\n\t\t"),
	io__write_list(FieldInits, ",\n\t\t", mlds_output_initializer_body),
	io__write_string("}").
mlds_output_initializer_body(init_array(ElementInits)) -->
	io__write_string("{\n\t\t"),
	io__write_list(ElementInits, ",\n\t\t", mlds_output_initializer_body),
	io__write_string("}").

%-----------------------------------------------------------------------------%
%
% Code to output function declarations/definitions
%

:- pred mlds_output_pred_proc_id(pred_proc_id, io__state, io__state).
:- mode mlds_output_pred_proc_id(in, di, uo) is det.

mlds_output_pred_proc_id(proc(PredId, ProcId)) -->
	globals__io_lookup_bool_option(auto_comments, AddComments),
	( { AddComments = yes } ->
		io__write_string("/* pred_id: "),
		{ pred_id_to_int(PredId, PredIdNum) },
		io__write_int(PredIdNum),
		io__write_string(", proc_id: "),
		{ proc_id_to_int(ProcId, ProcIdNum) },
		io__write_int(ProcIdNum),
		io__write_string(" */\n")
	;
		[]
	).

:- pred mlds_output_func(indent, qualified_entity_name, mlds__context,
		func_params, maybe(statement), io__state, io__state).
:- mode mlds_output_func(in, in, in, in, in, di, uo) is det.

mlds_output_func(Indent, Name, Context, Signature, MaybeBody) -->
	mlds_output_func_decl(Indent, Name, Signature),
	(
		{ MaybeBody = no },
		io__write_string(";\n")
	;
		{ MaybeBody = yes(Body) },
		io__write_string("\n"),

		mlds_indent(Context, Indent),
		io__write_string("{\n"),

		{ FuncInfo = func_info(Name, Signature) },

		%
		% If the procedure body contains any optimizable tailcalls,
		% we wrap the function body inside a `for(;;)' loop
		% so that we can use `continue;' inside the function body
		% to optimize tail recursive calls.
		%
		globals__io_lookup_bool_option(emit_c_loops, Emit_C_Loops),
		(
			{ Emit_C_Loops = yes },
			{ statement_contains_statement(Body, Call) },
			{ Call = mlds__statement(CallStmt, _) },
			{ can_optimize_tailcall(FuncInfo, CallStmt) }
		->
			mlds_indent(Context, Indent + 1),
			io__write_string("for(;;)\n"),
			mlds_indent(Context, Indent + 2),
			io__write_string("{\n"),
			{ Indent2 = Indent + 2 }
		;
			{ Indent2 = Indent }
		),

		mlds_output_statement(Indent2 + 1, FuncInfo, Body),

		%
		% Output a `return' statement to terminate the `for(;;)' loop.
		% This should only be necessary for functions with no
		% return values; for functions with return values,
		% the function body should never just fall through,
		% instead it must always return via a `return' statement.
		%
		{ Signature = mlds__func_params(_Parameters, RetTypes) },
		( { RetTypes = [] } ->
			mlds_output_stmt(Indent2 + 1, FuncInfo, return([]),
				Context)
		;
			globals__io_lookup_bool_option(auto_comments, Comments),
			( { Comments = yes } ->
				mlds_indent(Context, Indent2 + 1),
				io__write_string("/*NOTREACHED*/\n")
			;
				[]
			)
		),

		( { Indent2 = Indent + 2 } ->
			% end the `for(;;)'
			mlds_indent(Context, Indent2),
			io__write_string("}\n")
		;
			[]
		),

		mlds_indent(Context, Indent),
		io__write_string("}\n")	% end the function
	).

:- pred mlds_output_func_decl(indent, qualified_entity_name, func_params, 
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
	mlds_output_fully_qualified_name(Name),
	mlds_output_params(Indent, Name, Parameters).

:- pred mlds_output_params(indent, qualified_entity_name, mlds__arguments,
		io__state, io__state).
:- mode mlds_output_params(in, in, in, di, uo) is det.

mlds_output_params(Indent, FuncName, Parameters) -->
	io__write_char('('),
	( { Parameters = [] } ->
		io__write_string("void")
	;
		io__write_list(Parameters, ", ",
			mlds_output_param(Indent, FuncName))
	),
	io__write_char(')').

:- pred mlds_output_param(indent, qualified_entity_name,
		pair(mlds__entity_name, mlds__type), io__state, io__state).
:- mode mlds_output_param(in, in, in, di, uo) is det.

mlds_output_param(_Indent, qual(ModuleName, _FuncName), Name - Type) -->
	mlds_output_type(Type),
	io__write_char(' '),
	mlds_output_fully_qualified_name(qual(ModuleName, Name)).

:- pred mlds_output_func_type(func_params, io__state, io__state).
:- mode mlds_output_func_type(in, di, uo) is det.

mlds_output_func_type(Params) -->
	{ Params = mlds__func_params(Parameters, RetTypes) },
	( { RetTypes = [] } ->
		io__write_string("void")
	; { RetTypes = [RetType] } ->
		mlds_output_type(RetType)
	;
		{ error("mlds_output_func_type: multiple return types") }
	),
	io__write_string(" (*)"),
	mlds_output_param_types(Parameters).

:- pred mlds_output_param_types(mlds__arguments, io__state, io__state).
:- mode mlds_output_param_types(in, di, uo) is det.

mlds_output_param_types(Parameters) -->
	io__write_char('('),
	( { Parameters = [] } ->
		io__write_string("void")
	;
		io__write_list(Parameters, ", ", mlds_output_param_type)
	),
	io__write_char(')').

:- pred mlds_output_param_type(pair(mlds__entity_name, mlds__type),
		io__state, io__state).
:- mode mlds_output_param_type(in, di, uo) is det.

mlds_output_param_type(_Name - Type) -->
	mlds_output_type(Type).

%-----------------------------------------------------------------------------%
%
% Code to output names of various entities
%

:- pred mlds_output_fully_qualified_name(mlds__qualified_entity_name,
		io__state, io__state).
:- mode mlds_output_fully_qualified_name(in, di, uo) is det.

mlds_output_fully_qualified_name(QualifiedName) -->
	(
		%
		% don't module-qualify main/2
		%
		{ QualifiedName = qual(_ModuleName, Name) },
		{ Name = function(PredLabel, _, _, _) },
		{ PredLabel = pred(predicate, no, "main", 2) }
	->
		mlds_output_name(Name)
	;
		mlds_output_fully_qualified(QualifiedName, mlds_output_name)
	).

:- pred mlds_output_fully_qualified_proc_label(mlds__qualified_proc_label,
		io__state, io__state).
:- mode mlds_output_fully_qualified_proc_label(in, di, uo) is det.

mlds_output_fully_qualified_proc_label(QualifiedName) -->
	(
		%
		% don't module-qualify main/2
		%
		{ QualifiedName = qual(_ModuleName, Name) },
		{ Name = PredLabel - _ProcId },
		{ PredLabel = pred(predicate, no, "main", 2) }
	->
		mlds_output_proc_label(Name)
	;
		mlds_output_fully_qualified(QualifiedName,
			mlds_output_proc_label)
	).

:- pred mlds_output_fully_qualified(mlds__fully_qualified_name(T),
		pred(T, io__state, io__state), io__state, io__state).
:- mode mlds_output_fully_qualified(in, pred(in, di, uo) is det,
		di, uo) is det.

mlds_output_fully_qualified(qual(ModuleName, Name), OutputFunc) -->
	{ SymName = mlds_module_name_to_sym_name(ModuleName) },
	{ llds_out__sym_name_mangle(SymName, MangledModuleName) },
	io__write_string(MangledModuleName),
	io__write_string("__"),
	OutputFunc(Name).

:- pred mlds_output_module_name(mercury_module_name, io__state, io__state).
:- mode mlds_output_module_name(in, di, uo) is det.

mlds_output_module_name(ModuleName) -->
	{ llds_out__sym_name_mangle(ModuleName, MangledModuleName) },
	io__write_string(MangledModuleName).

:- pred mlds_output_name(mlds__entity_name, io__state, io__state).
:- mode mlds_output_name(in, di, uo) is det.

% XXX we should avoid appending the arity, modenum, and seqnum
%     if they are not needed.

mlds_output_name(type(Name, Arity)) -->
	{ llds_out__name_mangle(Name, MangledName) },
	io__format("%s_%d", [s(MangledName), i(Arity)]).
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
	{ llds_out__name_mangle(Name, MangledName) },
	io__format("%s_%d_%s", [s(MangledName), i(Arity), s(Suffix)]),
	( { MaybeDefiningModule = yes(DefiningModule) } ->
		io__write_string("_in__"),
		mlds_output_module_name(DefiningModule)
	;
		[]
	).
mlds_output_pred_label(special_pred(PredName, MaybeTypeModule,
		TypeName, TypeArity)) -->
	{ llds_out__name_mangle(PredName, MangledPredName) },
	{ llds_out__name_mangle(TypeName, MangledTypeName) },
	io__write_string(MangledPredName),
	io__write_string("__"),
	( { MaybeTypeModule = yes(TypeModule) } ->
		mlds_output_module_name(TypeModule),
		io__write_string("__")
	;
		[]
	),
	io__write_string(MangledTypeName),
	io__write_string("_"),
	io__write_int(TypeArity).

:- pred mlds_output_data_name(mlds__data_name, io__state, io__state).
:- mode mlds_output_data_name(in, di, uo) is det.

mlds_output_data_name(var(Name)) -->
	{ llds_out__name_mangle(Name, MangledName) },
	io__write_string(MangledName).
mlds_output_data_name(common(Num)) -->
	io__write_string("common_"),
	io__write_int(Num).
mlds_output_data_name(rtti(RttiTypeId, RttiName)) -->
	{ rtti__addr_to_string(RttiTypeId, RttiName, RttiAddrName) },
	io__write_string(RttiAddrName).
mlds_output_data_name(base_typeclass_info(_ClassId, _InstanceId)) -->
	{ error("mlds_to_c.m: NYI: basetypeclass_info") }.
mlds_output_data_name(module_layout) -->
	{ error("mlds_to_c.m: NYI: module_layout") }.
mlds_output_data_name(proc_layout(_ProcLabel)) -->
	{ error("mlds_to_c.m: NYI: proc_layout") }.
mlds_output_data_name(internal_layout(_ProcLabel, _FuncSeqNum)) -->
	{ error("mlds_to_c.m: NYI: internal_layout") }.
mlds_output_data_name(tabling_pointer(ProcLabel)) -->
	io__write_string("table_for_"),
	mlds_output_proc_label(ProcLabel).

%-----------------------------------------------------------------------------%
%
% Code to output types
%

:- pred mlds_output_type(mlds__type, io__state, io__state).
:- mode mlds_output_type(in, di, uo) is det.

mlds_output_type(mercury_type(Type)) -->
	( { Type = term__functor(term__atom("character"), [], _) } ->
		io__write_string("Char")
	; { Type = term__functor(term__atom("int"), [], _) } ->
		io__write_string("Integer")
	; { Type = term__functor(term__atom("string"), [], _) } ->
		io__write_string("String")
	; { Type = term__functor(term__atom("float"), [], _) } ->
		io__write_string("Float")
	; { Type = term__variable(_) } ->
		io__write_string("MR_Box")
	;
		% XXX we ought to use pointers to struct types here,
		% so that distinct Mercury types map to distinct C types
		io__write_string("MR_Word")
	).
mlds_output_type(mlds__native_int_type)   --> io__write_string("int").
mlds_output_type(mlds__native_float_type) --> io__write_string("float").
mlds_output_type(mlds__native_bool_type)  --> io__write_string("bool").
mlds_output_type(mlds__native_char_type)  --> io__write_string("char").
mlds_output_type(mlds__class_type(Name, Arity)) -->
	io__write_string("struct "),
	mlds_output_fully_qualified(Name, io__write_string),
	io__format("_%d", [i(Arity)]).
mlds_output_type(mlds__ptr_type(Type)) -->
	mlds_output_type(Type),
	io__write_string(" *").
mlds_output_type(mlds__func_type(FuncParams)) -->
	% XXX C syntax sucks, there's no easy way of
	% writing these types that will work in all
	% situations.  Currently we rely on the MLDS code
	% generator only using function types in certain situations.
	mlds_output_func_type(FuncParams).
mlds_output_type(mlds__generic_type) -->
	io__write_string("MR_Box").
mlds_output_type(mlds__generic_env_ptr_type) -->
	io__write_string("void *").
mlds_output_type(mlds__pseudo_type_info_type) -->
	io__write_string("MR_PseudoTypeInfo").
mlds_output_type(mlds__cont_type) -->
	globals__io_lookup_bool_option(gcc_nested_functions, GCC_NestedFuncs),
	( { GCC_NestedFuncs = yes } ->
		io__write_string("MR_NestedCont")
	;
		io__write_string("MR_Cont")
	).
mlds_output_type(mlds__commit_type) -->
	globals__io_lookup_bool_option(gcc_local_labels, GCC_LocalLabels),
	( { GCC_LocalLabels = yes } ->
		io__write_string("__label__")
	;
		io__write_string("jmp_buf")
	).
mlds_output_type(mlds__rtti_type(RttiName)) -->
	io__write_string("MR_"),
	io__write_string(mlds_rtti_type_name(RttiName)).

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

mlds_output_access(Access) -->
	globals__io_lookup_bool_option(auto_comments, Comments),
	( { Comments = yes } ->
		mlds_output_access_2(Access)
	;
		[]
	).

:- pred mlds_output_access_2(access, io__state, io__state).
:- mode mlds_output_access_2(in, di, uo) is det.

mlds_output_access_2(public)    --> [].
mlds_output_access_2(private)   --> io__write_string("/* private: */ ").
mlds_output_access_2(protected) --> io__write_string("/* protected: */ ").
mlds_output_access_2(default)   --> io__write_string("/* default access */ ").

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

:- type func_info
	--->	func_info(mlds__qualified_entity_name, mlds__func_params).

:- pred mlds_output_statements(indent, func_info, list(mlds__statement),
		io__state, io__state).
:- mode mlds_output_statements(in, in, in, di, uo) is det.

mlds_output_statements(Indent, FuncInfo, Statements) -->
	list__foldl(mlds_output_statement(Indent, FuncInfo), Statements).

:- pred mlds_output_statement(indent, func_info, mlds__statement,
		io__state, io__state).
:- mode mlds_output_statement(in, in, in, di, uo) is det.

mlds_output_statement(Indent, FuncInfo, mlds__statement(Statement, Context)) -->
	mlds_output_context(Context),
	mlds_output_stmt(Indent, FuncInfo, Statement, Context).

:- pred mlds_output_stmt(indent, func_info, mlds__stmt, mlds__context,
		io__state, io__state).
:- mode mlds_output_stmt(in, in, in, in, di, uo) is det.

	%
	% sequence
	%
mlds_output_stmt(Indent, FuncInfo, block(Defns, Statements), Context) -->
	mlds_indent(Indent),
	io__write_string("{\n"),
	( { Defns \= [] } ->
		{ FuncInfo = func_info(FuncName, _) },
		{ FuncName = qual(ModuleName, _) },
		mlds_output_defns(Indent + 1, ModuleName, Defns),
		io__write_string("\n")
	;
		[]
	),
	mlds_output_statements(Indent + 1, FuncInfo, Statements),
	mlds_indent(Context, Indent),
	io__write_string("}\n").

	%
	% iteration
	%
mlds_output_stmt(Indent, FuncInfo, while(Cond, Statement, no), _) -->
	mlds_indent(Indent),
	io__write_string("while ("),
	mlds_output_rval(Cond),
	io__write_string(")\n"),
	mlds_output_statement(Indent + 1, FuncInfo, Statement).
mlds_output_stmt(Indent, FuncInfo, while(Cond, Statement, yes), Context) -->
	mlds_indent(Indent),
	io__write_string("do\n"),
	mlds_output_statement(Indent + 1, FuncInfo, Statement),
	mlds_indent(Context, Indent),
	io__write_string("while ("),
	mlds_output_rval(Cond),
	io__write_string(");\n").

	%
	% selection (see also computed_goto)
	%
mlds_output_stmt(Indent, FuncInfo, if_then_else(Cond, Then0, MaybeElse),
		Context) -->
	%
	% we need to take care to avoid problems caused by the
	% dangling else ambiguity
	%
	{
		%
		% For examples of the form
		%
		%	if (...)
		%		if (...)
		%			...
		%	else
		%		...
		%
		% we need braces around the inner `if', otherwise
		% they wouldn't parse they way we want them to:
		% C would match the `else' with the inner `if'
		% rather than the outer `if'.
		%
		MaybeElse = yes(_),
		Then0 = statement(if_then_else(_, _, no), ThenContext)
	->
		Then = statement(block([], [Then0]), ThenContext)
	;
		%
		% For examples of the form
		%
		%	if (...)
		%		if (...)
		%			...
		%		else
		%			...
		%
		% we don't _need_ braces around the inner `if',
		% since C will match the else with the inner `if',
		% but we add braces anyway, to avoid a warning from gcc.
		%
		MaybeElse = no,
		Then0 = statement(if_then_else(_, _, yes(_)), ThenContext)
	->
		Then = statement(block([], [Then0]), ThenContext)
	;
		Then = Then0
	},

	mlds_indent(Indent),
	io__write_string("if ("),
	mlds_output_rval(Cond),
	io__write_string(")\n"),
	mlds_output_statement(Indent + 1, FuncInfo, Then),
	( { MaybeElse = yes(Else) } ->
		mlds_indent(Context, Indent),
		io__write_string("else\n"),
		mlds_output_statement(Indent + 1, FuncInfo, Else)
	;
		[]
	).

	%
	% transfer of control
	%
mlds_output_stmt(Indent, _FuncInfo, label(LabelName), _) -->
	%
	% Note: MLDS allows labels at the end of blocks.
	% C doesn't.  Hence we need to insert a semi-colon after the colon
	% to ensure that there is a statement to attach the label to.
	%
	mlds_indent(Indent - 1),
	mlds_output_label_name(LabelName),
	io__write_string(":;\n").
mlds_output_stmt(Indent, _FuncInfo, goto(LabelName), _) -->
	mlds_indent(Indent),
	io__write_string("goto "),
	mlds_output_label_name(LabelName),
	io__write_string(";\n").
mlds_output_stmt(Indent, _FuncInfo, computed_goto(Expr, Labels), Context) -->
	% XXX for GNU C, we could output potentially more efficient code
	% by using an array of labels; this would tell the compiler that
	% it didn't need to do any range check.
	mlds_indent(Indent),
	io__write_string("switch ("),
	mlds_output_rval(Expr),
	io__write_string(") {"),
	{ OutputLabel =
	    (pred(Label::in, Count0::in, Count::out, di, uo) is det -->
		mlds_indent(Context, Indent + 1),
		io__write_string("case "),
		io__write_int(Count0),
		io__write_string(": goto "),
		mlds_output_label_name(Label),
		io__write_string(";\n"),
		{ Count = Count0 + 1 }
	) },
	list__foldl2(OutputLabel, Labels, 0, _FinalCount),
	mlds_indent(Context, Indent + 1),
	io__write_string("default: /*NOTREACHED*/ assert(0);\n"),
	mlds_indent(Context, Indent),
	io__write_string("}\n").

	%
	% function call/return
	%
mlds_output_stmt(Indent, CallerFuncInfo, Call, Context) -->
	{ Call = call(_Signature, FuncRval, MaybeObject, CallArgs,
		Results, IsTailCall) },
	%
	% Optimize directly-recursive tail calls
	%
	{ CallerFuncInfo = func_info(Name, Params) },
	globals__io_lookup_bool_option(emit_c_loops, Emit_C_Loops),
	(
		{ Emit_C_Loops = yes },
		{ can_optimize_tailcall(CallerFuncInfo, Call) }
	->
		mlds_indent(Indent),
		io__write_string("{\n"),
		globals__io_lookup_bool_option(auto_comments, Comments),
		( { Comments = yes } ->
			mlds_indent(Context, Indent + 1),
			io__write_string("/* tail recursive call */\n")
		;
			[]
		),
		{ Name = qual(ModuleName, _) },
		{ Params = mlds__func_params(FuncArgs, _RetTypes) },
		mlds_output_assign_args(Indent + 1, ModuleName, Context,
			FuncArgs, CallArgs),
		mlds_indent(Context, Indent + 1),
		( { Comments = yes } ->
			io__write_string("continue; /* go to start of function */\n")
		;
			io__write_string("continue;\n")
		),
		mlds_indent(Context, Indent),
		io__write_string("}\n")
	;
		%
		% Optimize general tail calls.
		% We can't really do much here except to insert `return'
		% as an extra hint to the C compiler.
		% XXX these optimizations should be disable-able
		%
		% If Results = [], i.e. the function has `void' return type,
		% then this would result in code that is not legal ANSI C
		% (although it _is_ legal in GNU C and in C++),
		% so for that case, we put the return statement after
		% the call -- see below.  We need to enclose it inside
		% an extra pair of curly braces in case this `call'
		% is e.g. inside an if-then-else.
		%
		mlds_indent(Indent),
		( { IsTailCall = tail_call } ->
			( { Results \= [] } ->
				io__write_string("return ")
			;
				io__write_string("{\n"),
				mlds_indent(Context, Indent + 1)
			)
		;
			[]
		),
		( { MaybeObject = yes(Object) } ->
			mlds_output_bracketed_rval(Object),
			io__write_string(".") % XXX should this be "->"?
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
		mlds_output_bracketed_rval(FuncRval),
		io__write_string("("),
		io__write_list(CallArgs, ", ", mlds_output_rval),
		io__write_string(");\n"),

		( { IsTailCall = tail_call, Results = [] } ->
			mlds_indent(Context, Indent + 1),
			io__write_string("return;\n"),
			mlds_indent(Context, Indent),
			io__write_string("}\n")
		;
			[]
		)
	).

mlds_output_stmt(Indent, _FuncInfo, return(Results), _) -->
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
	% commits
	%
mlds_output_stmt(Indent, _FuncInfo, do_commit(Ref), _) -->
	mlds_indent(Indent),
	globals__io_lookup_bool_option(gcc_local_labels, GCC_LocalLabels),
	( { GCC_LocalLabels = yes } ->
		% output "goto <Ref>"
		io__write_string("goto "),
		mlds_output_rval(Ref)
	;
		% output "longjmp(<Ref>, 1)"
		io__write_string("longjmp("),
		mlds_output_rval(Ref),
		io__write_string(", 1)")
	),
	io__write_string(";\n").
mlds_output_stmt(Indent, FuncInfo, try_commit(Ref, Stmt0, Handler), Context) -->
	globals__io_lookup_bool_option(gcc_local_labels, GCC_LocalLabels),
	(
		{ GCC_LocalLabels = yes },
	
		% Output the following:
		%
		%               <Stmt>
		%               goto <Ref>_done;
		%       <Ref>:
		%               <Handler>
		%       <Ref>_done:
		%               ;

		% Note that <Ref> should be just variable name,
		% not a complicated expression.  If not, the
		% C compiler will catch it.

		mlds_output_statement(Indent, FuncInfo, Stmt0),

		mlds_indent(Context, Indent),
		io__write_string("goto "),
		mlds_output_lval(Ref),
		io__write_string("_done;\n"),

		mlds_indent(Context, Indent - 1),
		mlds_output_lval(Ref),
		io__write_string(":\n"),

		mlds_output_statement(Indent, FuncInfo, Handler),

		mlds_indent(Context, Indent - 1),
		mlds_output_lval(Ref),
		io__write_string("_done:\t;\n")

	;
		{ GCC_LocalLabels = no },

		% Output the following:
		%
		%	if (setjmp(<Ref>) == 0)
		%               <Stmt>
		%       else
		%               <Handler>

		%
		% XXX do we need to declare the local variables as volatile,
		% because of the setjmp()?
		%

		%
		% we need to take care to avoid problems caused by the
		% dangling else ambiguity
		%
		{
			Stmt0 = statement(if_then_else(_, _, no), Context)
		->
			Stmt = statement(block([], [Stmt0]), Context)
		;
			Stmt = Stmt0
		},

		mlds_indent(Indent),
		io__write_string("if (setjmp("),
		mlds_output_lval(Ref),
		io__write_string(") == 0)\n"),

		mlds_output_statement(Indent + 1, FuncInfo, Stmt),

		mlds_indent(Context, Indent),
		io__write_string("else\n"),

		mlds_output_statement(Indent + 1, FuncInfo, Handler)
	).

	% return `true' if the statement is a tail call which
	% can be optimized into a jump back to the start of the
	% function
:- pred can_optimize_tailcall(func_info, mlds__stmt).
:- mode can_optimize_tailcall(in, in) is semidet.
can_optimize_tailcall(CallerFuncInfo, Call) :-
	Call = call(_Signature, FuncRval, MaybeObject, _CallArgs,
		_Results, IsTailCall),
	CallerFuncInfo = func_info(Name, _Params),
	%
	% check if this call can be optimized as a tail call
	%
	IsTailCall = tail_call,

	%
	% check if the callee adddress is the same as
	% the caller
	%
	FuncRval = const(code_addr_const(CodeAddr)),
	(	
		CodeAddr = proc(QualifiedProcLabel, _Sig),
		MaybeSeqNum = no
	;
		CodeAddr = internal(QualifiedProcLabel, SeqNum, _Sig),
		MaybeSeqNum = yes(SeqNum)
	),
	QualifiedProcLabel = qual(ModuleName, PredLabel - ProcId),
	% check that the module name matches
	Name = qual(ModuleName, FuncName),
	% check that the PredLabel, ProcId, and MaybeSeqNum match
	FuncName = function(PredLabel, ProcId, MaybeSeqNum, _),

	%
	% In C++, `this' is a constant, so our usual technique
	% of assigning the arguments won't work if it is a
	% member function.  Thus we don't do this optimization
	% if we're optimizing a member function call
	%
	MaybeObject = no.


	% Assign the specified list of rvals to the arguments.
	% This is used as part of tail recursion optimization (see above).
:- pred mlds_output_assign_args(indent, mlds_module_name, mlds__context,
		mlds__arguments, list(mlds__rval), io__state, io__state).
:- mode mlds_output_assign_args(in, in, in, in, in, di, uo) is det.

mlds_output_assign_args(_, _, _, [_|_], []) -->
	{ error("mlds_output_assign_args: length mismatch") }.
mlds_output_assign_args(_, _, _, [], [_|_]) -->
	{ error("mlds_output_assign_args: length mismatch") }.
mlds_output_assign_args(_, _, _, [], []) --> [].
mlds_output_assign_args(Indent, ModuleName, Context,
		[Name - _Type | Rest], [Arg | Args]) -->
	(
		%
		% don't bother assigning a variable to itself
		%
		{ Name = data(var(VarName)) },
		{ QualVarName = qual(ModuleName, VarName) },
		{ Arg = lval(var(QualVarName)) }
	->
		[]
	;
		mlds_indent(Context, Indent),
		mlds_output_fully_qualified_name(qual(ModuleName, Name)),
		io__write_string(" = "),
		mlds_output_rval(Arg),
		io__write_string(";\n")
	),
	mlds_output_assign_args(Indent, ModuleName, Context, Rest, Args).

	%
	% exception handling
	%

	/* XXX not yet implemented */


	%
	% atomic statements
	%
mlds_output_stmt(Indent, _FuncInfo, atomic(AtomicStatement), Context) -->
	mlds_output_atomic_stmt(Indent, AtomicStatement, Context).

:- pred mlds_output_label_name(mlds__label, io__state, io__state).
:- mode mlds_output_label_name(in, di, uo) is det.

mlds_output_label_name(LabelName) -->
	io__write_string(LabelName).

:- pred mlds_output_atomic_stmt(indent, mlds__atomic_statement, mlds__context,
				io__state, io__state).
:- mode mlds_output_atomic_stmt(in, in, in, di, uo) is det.

	%
	% comments
	%
mlds_output_atomic_stmt(Indent, comment(Comment), _) -->
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
mlds_output_atomic_stmt(Indent, assign(Lval, Rval), _) -->
	mlds_indent(Indent),
	mlds_output_lval(Lval),
	io__write_string(" = "),
	mlds_output_rval(Rval),
	io__write_string(";\n").

	%
	% heap management
	%
mlds_output_atomic_stmt(Indent, NewObject, Context) -->
	{ NewObject = new_object(Target, MaybeTag, Type, MaybeSize,
		MaybeCtorName, Args, ArgTypes) },
	mlds_indent(Indent),
	io__write_string("{\n"),
	mlds_indent(Indent + 1),
	mlds_output_lval(Target),
	io__write_string(" = "),
	( { MaybeTag = yes(Tag0) } ->
		{ Tag = Tag0 },
		io__write_string("(MR_Word) MR_mkword("),
		mlds_output_tag(Tag),
		io__write_string(", "),
		{ EndMkword = ")" }
	;
		{ Tag = 0 },
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
		io__write_char('"'),
		c_util__output_quoted_string(CtorName),
		io__write_char('"')
	;
		io__write_string("NULL")
	),
	io__write_string(")"),
	io__write_string(EndMkword),
	io__write_string(";\n"),
	mlds_output_init_args(Args, ArgTypes, Context, 0, Target, Tag,
		Indent + 1),
	mlds_indent(Indent),
	io__write_string("}\n").

mlds_output_atomic_stmt(Indent, mark_hp(Lval), _) -->
	mlds_indent(Indent),
	io__write_string("MR_mark_hp("),
	mlds_output_lval(Lval),
	io__write_string(");\n").

mlds_output_atomic_stmt(Indent, restore_hp(Rval), _) -->
	mlds_indent(Indent),
	io__write_string("MR_mark_hp("),
	mlds_output_rval(Rval),
	io__write_string(");\n").

	%
	% trail management
	%
mlds_output_atomic_stmt(_Indent, trail_op(_TrailOp), _) -->
	{ error("mlds_to_c.m: sorry, trail_ops not implemented") }.

	%
	% foreign language interfacing
	%
mlds_output_atomic_stmt(_Indent, target_code(TargetLang, CodeString), Context) -->
	( { TargetLang = lang_C } ->
		mlds_output_context(Context),
		io__write_string(CodeString)
	;
		{ error("mlds_to_c.m: sorry, target_code only works for lang_C") }
	).

:- pred mlds_output_init_args(list(mlds__rval), list(mlds__type), mlds__context,
		int, mlds__lval, mlds__tag, indent, io__state, io__state).
:- mode mlds_output_init_args(in, in, in, in, in, in, in, di, uo) is det.

mlds_output_init_args([_|_], [], _, _, _, _, _) -->
	{ error("mlds_output_init_args: length mismatch") }.
mlds_output_init_args([], [_|_], _, _, _, _, _) -->
	{ error("mlds_output_init_args: length mismatch") }.
mlds_output_init_args([], [], _, _, _, _, _) --> [].
mlds_output_init_args([Arg|Args], [ArgType|ArgTypes], Context,
		ArgNum, Target, Tag, Indent) -->
	%
	% Currently all fields of new_object instructions are
	% represented as MR_Box, so we need to box them if necessary.
	%
	mlds_indent(Context, Indent),
	io__write_string("MR_field("),
	mlds_output_tag(Tag),
	io__write_string(", "),
	mlds_output_lval(Target),
	io__write_string(", "),
	io__write_int(ArgNum),
	io__write_string(") = (MR_Word) "),
	mlds_output_boxed_rval(ArgType, Arg),
	io__write_string(";\n"),
	mlds_output_init_args(Args, ArgTypes, Context,
		ArgNum + 1, Target, Tag, Indent).

%-----------------------------------------------------------------------------%
%
% Code to output expressions
%

:- pred mlds_output_lval(mlds__lval, io__state, io__state).
:- mode mlds_output_lval(in, di, uo) is det.

mlds_output_lval(field(MaybeTag, Rval, offset(OffsetRval),
		FieldType, _ClassType)) -->
	(
		{ FieldType = mlds__generic_type
		; FieldType = mlds__mercury_type(term__variable(_))
		}
	->
		% XXX this generated code is ugly;
		% it would be nicer to use a different macro
		% than MR_field(), one which had type `MR_Box'
		% rather than `Word'.
		io__write_string("(* (MR_Box *) &")
	;
		% The field type for field(_, _, offset(_), _, _) lvals
		% must be something that maps to MR_Box.
		{ error("unexpected field type") }
	),
	( { MaybeTag = yes(Tag) } ->
		io__write_string("MR_field("),
		mlds_output_tag(Tag),
		io__write_string(", ")
	;
		io__write_string("MR_mask_field(")
	),
	io__write_string("(MR_Word) "),
	mlds_output_rval(Rval),
	io__write_string(", "),
	mlds_output_rval(OffsetRval),
	io__write_string("))").
mlds_output_lval(field(MaybeTag, PtrRval, named_field(FieldId), _, _)) -->
	( { MaybeTag = yes(0) } ->
		( { PtrRval = mem_addr(Lval) } ->
			mlds_output_bracketed_lval(Lval),
			io__write_string(".")
		;
			mlds_output_bracketed_rval(PtrRval),
			io__write_string("->")
		)
	;
		( { MaybeTag = yes(Tag) } ->
			io__write_string("MR_body("),
			mlds_output_tag(Tag),
			io__write_string(", ")
		;
			io__write_string("MR_strip_tag(")
		),
		mlds_output_rval(PtrRval),
		io__write_string(")"),
		io__write_string("->")
	),
	mlds_output_fully_qualified(FieldId, io__write_string).
mlds_output_lval(mem_ref(Rval, _Type)) -->
	io__write_string("*"),
	mlds_output_bracketed_rval(Rval).
mlds_output_lval(var(VarName)) -->
	mlds_output_var(VarName).

:- pred mlds_output_var(mlds__var, io__state, io__state).
:- mode mlds_output_var(in, di, uo) is det.

mlds_output_var(VarName) -->
	mlds_output_fully_qualified(VarName, io__write_string).

:- pred mlds_output_bracketed_lval(mlds__lval, io__state, io__state).
:- mode mlds_output_bracketed_lval(in, di, uo) is det.

mlds_output_bracketed_lval(Lval) -->
	(
		% if it's just a variable name, then we don't need parentheses
		{ Lval = var(_) }
	->
		mlds_output_lval(Lval)
	;
		io__write_char('('),
		mlds_output_lval(Lval),
		io__write_char(')')
	).

:- pred mlds_output_bracketed_rval(mlds__rval, io__state, io__state).
:- mode mlds_output_bracketed_rval(in, di, uo) is det.

mlds_output_bracketed_rval(Rval) -->
	(
		% if it's just a variable name, then we don't need parentheses
		{ Rval = lval(var(_))
		; Rval = const(code_addr_const(_))
		}
	->
		mlds_output_rval(Rval)
	;
		io__write_char('('),
		mlds_output_rval(Rval),
		io__write_char(')')
	).

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
	( { Lval = field(MaybeTag, Rval, FieldNum, _, _) } ->
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
	io__write_string("(MR_Word) MR_mkword("),
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

:- pred mlds_output_unop(mlds__unary_op, mlds__rval, io__state, io__state).
:- mode mlds_output_unop(in, in, di, uo) is det.
	
mlds_output_unop(cast(Type), Exprn) -->
	mlds_output_cast_rval(Type, Exprn).
mlds_output_unop(box(Type), Exprn) -->
	mlds_output_boxed_rval(Type, Exprn).
mlds_output_unop(unbox(Type), Exprn) -->
	mlds_output_unboxed_rval(Type, Exprn).
mlds_output_unop(std_unop(Unop), Exprn) -->
	mlds_output_std_unop(Unop, Exprn).

:- pred mlds_output_cast_rval(mlds__type, mlds__rval, io__state, io__state).
:- mode mlds_output_cast_rval(in, in, di, uo) is det.
	
mlds_output_cast_rval(Type, Exprn) -->
	io__write_string("("),
	mlds_output_type(Type),
	io__write_string(") "),
	mlds_output_rval(Exprn).

:- pred mlds_output_boxed_rval(mlds__type, mlds__rval, io__state, io__state).
:- mode mlds_output_boxed_rval(in, in, di, uo) is det.
	
mlds_output_boxed_rval(Type, Exprn) -->
	(
		{ Type = mlds__mercury_type(term__functor(term__atom("float"),
				[], _))
		; Type = mlds__native_float_type
		}
	->
		io__write_string("MR_box_float("),
		mlds_output_rval(Exprn),
		io__write_string(")")
	;
		% We cast first to MR_Word, and then to MR_Box.
		% This is done to avoid spurious warnings about "cast from
		% pointer to integer of different size" from gcc.
		% XXX The generated code would be more readable if we
		%     only did this for the cases where it was necessary.
		io__write_string("((MR_Box) (MR_Word) ("),
		mlds_output_rval(Exprn),
		io__write_string("))")
	).

:- pred mlds_output_unboxed_rval(mlds__type, mlds__rval, io__state, io__state).
:- mode mlds_output_unboxed_rval(in, in, di, uo) is det.
	
mlds_output_unboxed_rval(Type, Exprn) -->
	(
		{ Type = mlds__mercury_type(term__functor(term__atom("float"),
				[], _))
		; Type = mlds__native_float_type
		}
	->
		io__write_string("MR_unbox_float("),
		mlds_output_rval(Exprn),
		io__write_string(")")
	;
		% We cast first to MR_Word, and then to the desired type.
		% This is done to avoid spurious warnings about "cast from
		% pointer to integer of different size" from gcc.
		% XXX The generated code would be more readable if we
		%     only did this for the cases where it was necessary.
		io__write_string("(("),
		mlds_output_type(Type),
		io__write_string(") (MR_Word) "),
		mlds_output_rval(Exprn),
		io__write_string(")")
	).

:- pred mlds_output_std_unop(builtin_ops__unary_op, mlds__rval,
		io__state, io__state).
:- mode mlds_output_std_unop(in, in, di, uo) is det.
	
mlds_output_std_unop(UnaryOp, Exprn) -->
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
:- mode mlds_output_binary_op(in, di, uo) is det.

mlds_output_binary_op(Op) -->
	( { c_util__binary_infix_op(Op, OpStr) } ->
		io__write_string(OpStr)
	;
		{ error("mlds_output_binary_op: invalid binary operator") }
	).

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

:- pred mlds_output_tag(mlds__tag, io__state, io__state).
:- mode mlds_output_tag(in, di, uo) is det.

mlds_output_tag(Tag) -->
	io__write_string("MR_mktag("),
	io__write_int(Tag),
	io__write_string(")").

%-----------------------------------------------------------------------------%

:- pred mlds_output_code_addr(mlds__code_addr, io__state, io__state).
:- mode mlds_output_code_addr(in, di, uo) is det.

mlds_output_code_addr(proc(Label, _Sig)) -->
	mlds_output_fully_qualified_proc_label(Label).
mlds_output_code_addr(internal(Label, SeqNum, _Sig)) -->
	mlds_output_fully_qualified_proc_label(Label),
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

mlds_output_data_addr(data_addr(ModuleName, DataName)) -->
	(
		% if its an array type, then we just use the name,
		% otherwise we must prefix the name with `&'.
		{ DataName = rtti(_, RttiName) },
		{ rtti_name_has_array_type(RttiName) = yes }
	->
		mlds_output_data_var_name(ModuleName, DataName)
	;
		io__write_string("(&"),
		mlds_output_data_var_name(ModuleName, DataName),
		io__write_string(")")
	).

:- pred mlds_output_data_var_name(mlds_module_name, mlds__data_name,
		io__state, io__state).
:- mode mlds_output_data_var_name(in, in, di, uo) is det.

mlds_output_data_var_name(ModuleName, DataName) -->
	mlds_output_module_name(mlds_module_name_to_sym_name(ModuleName)),
	io__write_string("__"),
	mlds_output_data_name(DataName).

%-----------------------------------------------------------------------------%
%
% Miscellaneous stuff to handle indentation and generation of
% source context annotations (#line directives).
%

:- pred mlds_output_context(mlds__context, io__state, io__state).
:- mode mlds_output_context(in, di, uo) is det.

mlds_output_context(Context) -->
	{ ProgContext = mlds__get_prog_context(Context) },
	{ term__context_file(ProgContext, FileName) },
	{ term__context_line(ProgContext, LineNumber) },
	c_util__set_line_num(FileName, LineNumber).

:- pred mlds_indent(mlds__context, indent, io__state, io__state).
:- mode mlds_indent(in, in, di, uo) is det.

mlds_indent(Context, N) -->
	mlds_output_context(Context),
	mlds_indent(N).

% A value of type `indent' records the number of levels
% of indentation to indent the next piece of code.
% Currently we output two spaces for each level of indentation.
:- type indent == int.

:- pred mlds_indent(indent, io__state, io__state).
:- mode mlds_indent(in, di, uo) is det.

mlds_indent(N) -->
	( { N =< 0 } ->
		[]
	;
		io__write_string("  "),
		mlds_indent(N - 1)
	).

%-----------------------------------------------------------------------------%

:- pred statements_contains_statement(mlds__statements, mlds__statement).
:- mode statements_contains_statement(in, out) is nondet.

statements_contains_statement(Statements, SubStatement) :-
	list__member(Statement, Statements),
	statement_contains_statement(Statement, SubStatement).

:- pred maybe_statement_contains_statement(maybe(mlds__statement), mlds__statement).
:- mode maybe_statement_contains_statement(in, out) is nondet.

maybe_statement_contains_statement(no, _Statement) :- fail.
maybe_statement_contains_statement(yes(Statement), SubStatement) :-
	statement_contains_statement(Statement, SubStatement).

:- pred statement_contains_statement(mlds__statement, mlds__statement).
:- mode statement_contains_statement(in, out) is multi.

statement_contains_statement(Statement, Statement).
statement_contains_statement(Statement, SubStatement) :-
	Statement = mlds__statement(Stmt, _Context),
	stmt_contains_statement(Stmt, SubStatement).

:- pred stmt_contains_statement(mlds__stmt, mlds__statement).
:- mode stmt_contains_statement(in, out) is nondet.

stmt_contains_statement(Stmt, SubStatement) :-
	(
		Stmt = block(_Defns, Statements),
		statements_contains_statement(Statements, SubStatement)
	;
		Stmt = while(_Rval, Statement, _Once),
		statement_contains_statement(Statement, SubStatement)
	;
		Stmt = if_then_else(_Cond, Then, MaybeElse),
		( statement_contains_statement(Then, SubStatement)
		; maybe_statement_contains_statement(MaybeElse, SubStatement)
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
		( statement_contains_statement(Statement, SubStatement)
		; statement_contains_statement(Handler, SubStatement)
		)
	;
		Stmt = atomic(_AtomicStmt),
		fail
	).

%-----------------------------------------------------------------------------%
