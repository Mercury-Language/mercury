%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% LLDS - The Low-Level Data Structure.

% This module defines the routines for printing out LLDS,
% the Low Level Data Structure.

% Main authors: conway, fjh, zs.

%-----------------------------------------------------------------------------%

:- module llds_out.

:- interface.

:- import_module llds.
:- import_module io.

	% Given a 'c_file' structure, open the appropriate .mod file
	% and output the code into that file.

:- pred output_c_file(c_file, io__state, io__state).
:- mode output_c_file(in, di, uo) is det.

	% Convert an lval to a string description of that lval.

:- pred llds_out__lval_to_string(lval, string).
:- mode llds_out__lval_to_string(in, out) is semidet.

:- pred llds_out__reg_to_string(reg_type, int, string).
:- mode llds_out__reg_to_string(in, in, out) is det.

:- pred llds_out__binary_op_to_string(binary_op, string).
:- mode llds_out__binary_op_to_string(in, out) is det.

	% Output an instruction (used for debugging).

:- pred output_instruction(instr, io__state, io__state).
:- mode output_instruction(in, di, uo) is det.

	% Output a label (used by garbage collection).

:- pred output_label(label, io__state, io__state).
:- mode output_label(in, di, uo) is det.

	% Output a proc label (used for building static call graph for prof).

:- pred output_proc_label(proc_label, io__state, io__state).
:- mode output_proc_label(in, di, uo) is det.

	% Get a proc label string (used by procs which are exported to C).

:- pred get_proc_label(proc_label, string).
:- mode get_proc_label(in, out) is det.

	% Mangle an arbitrary name into a C identifier

:- pred llds_out__name_mangle(string, string).
:- mode llds_out__name_mangle(in, out) is det.

	% Produces a string of the form Module__Name, unless Module__
	% is already a prefix of Name.

:- pred llds_out__maybe_qualify_name(string, string, string).
:- mode llds_out__maybe_qualify_name(in, in, out) is det.

:- pred output_c_quoted_string(string, io__state, io__state).
:- mode output_c_quoted_string(in, di, uo) is det.

	% Create a name for base_type_*

:- pred llds_out__make_base_type_name(base_data, string, arity, string).
:- mode llds_out__make_base_type_name(in, in, in, out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module export.
:- import_module exprn_aux, prog_data, prog_out, hlds_pred, mercury_to_mercury.
:- import_module bool, int, list, char, string, set, std_util, term, varset.
:- import_module assoc_list, require, globals, options.
:- import_module library.	% for the version number.

%-----------------------------------------------------------------------------%

% Every time we emit a declaration for a symbol, we insert it into the
% set of symbols we've already declared.  That way, we avoid generating
% the same symbol twice, which would cause an error in the C code.

:- type decl_set ==	set(decl_id).

:- type decl_id --->	create_label(int)
		;	float_label(string)
		;	code_addr(code_addr)
		;	data_addr(data_addr).

output_c_file(C_File) -->
	globals__io_lookup_bool_option(split_c_files, SplitFiles),
	( { SplitFiles = yes } ->
		{ C_File = c_file(BaseName, C_HeaderInfo, Modules) },
		{ string__append(BaseName, ".dir", ObjDirName) },
		make_directory(ObjDirName),
		output_c_file_init(BaseName, Modules),
		output_c_file_list(Modules, 1, BaseName, C_HeaderInfo)
	;
		output_single_c_file(C_File, no)
	).

:- pred make_directory(string, io__state, io__state).
:- mode make_directory(in, di, uo) is det.

make_directory(DirName) -->
	{ string__format("[ -d %s ] || mkdir -p %s", [s(DirName), s(DirName)],
		Command) },
	io__call_system(Command, _Result).

:- pred output_c_file_list(list(c_module), int, string, list(c_header_code),
				io__state, io__state).
:- mode output_c_file_list(in, in, in, in, di, uo) is det.

output_c_file_list([], _, _, _) --> [].
output_c_file_list([Module|Modules], Num, BaseName, C_HeaderLines) -->
	output_single_c_file(c_file(BaseName, C_HeaderLines, [Module]),
		yes(Num)),
	{ Num1 is Num + 1 },
	output_c_file_list(Modules, Num1, BaseName, C_HeaderLines).

:- pred output_c_file_init(string, list(c_module), io__state, io__state).
:- mode output_c_file_init(in, in, di, uo) is det.

output_c_file_init(BaseName, Modules) -->
	{ string__format("%s.dir/%s_%03d", [s(BaseName), s(BaseName), i(0)],
		NewName) },
	{ string__append(NewName, ".c", FileName) },
	io__tell(FileName, Result),
	(
		{ Result = ok }
	->
		{ library__version(Version) },
		io__write_strings(
			["/*\n** Automatically generated from `", BaseName,
			".m' by the\n** Mercury compiler, version ", Version,
			".  Do not edit.\n*/\n"]),
		io__write_string("/*\n"),
		io__write_string("INIT "),
		output_init_name(BaseName),
		io__write_string("\n"),
		io__write_string("ENDINIT\n"),
		io__write_string("*/\n\n"),
		io__write_string("#include ""imp.h""\n"),
		output_c_module_init_list(BaseName, Modules),
		io__told
	;
		io__progname_base("llds.m", ProgName),
		io__write_string("\n"),
		io__write_string(ProgName),
		io__write_string(": can't open `"),
		io__write_string(FileName),
		io__write_string("' for output\n"),
		io__set_exit_status(1)
	).

:- pred output_single_c_file(c_file, maybe(int), io__state, io__state).
:- mode output_single_c_file(in, in, di, uo) is det.

output_single_c_file(c_file(BaseName, C_HeaderLines, Modules), SplitFiles) 
		-->
	( { SplitFiles = yes(Num) } ->
		{ string__format("%s.dir/%s_%03d.c", [s(BaseName), s(BaseName), 
			i(Num)], FileName) }
	;
		{ string__append(BaseName, ".c", FileName) }
	),
	io__tell(FileName, Result),
	(
		{ Result = ok }
	->
		{ library__version(Version) },
		io__write_strings(
			["/*\n** Automatically generated from `", BaseName,
			".m' by the\n** Mercury compiler, version ", Version,
			".  Do not edit.\n*/\n"]),
		( { SplitFiles = yes(_) } ->
			[]
		;
			io__write_string("/*\n"),
			io__write_string("INIT "),
			output_init_name(BaseName),
			io__write_string("\n"),
			io__write_string("ENDINIT\n"),
			io__write_string("*/\n\n")
		),
		io__write_string("#include ""imp.h""\n"),
		output_c_header_include_lines(C_HeaderLines),
		io__write_string("\n"),
		{ gather_c_file_labels(Modules, Labels) },
		{ set__init(DeclSet0) },
		output_c_label_decl_list(Labels, DeclSet0, DeclSet),
		output_c_module_list(Modules, DeclSet, BaseName),
		( { SplitFiles = yes(_) } ->
			[]
		;
			io__write_string("\n"),
			output_c_module_init_list(BaseName, Modules)
		),
		io__told
	;
		io__progname_base("llds.m", ProgName),
		io__write_string("\n"),
		io__write_string(ProgName),
		io__write_string(": can't open `"),
		io__write_string(FileName),
		io__write_string("' for output\n"),
		io__set_exit_status(1)
	).

:- pred output_c_module_init_list(string, list(c_module), io__state, io__state).
:- mode output_c_module_init_list(in, in, di, uo) is det.

output_c_module_init_list(BaseName, Modules) -->

		% Output initialization functions, bunched into groups
		% of 40.
	io__write_string("#if (defined(USE_GCC_NONLOCAL_GOTOS) && "),
	io__write_string("!defined(USE_ASM_LABELS)) \\\n\t|| "),
	io__write_string("defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \\\n"),
	io__write_string("\t|| defined(DEBUG_LABELS) || !defined(SPEED) \\\n"),
	io__write_string("\t|| defined(NATIVE_GC) \n\n"),
	io__write_string("static void "),
	output_bunch_name(BaseName, 0),
	io__write_string("(void)\n"),
	io__write_string("{\n"),
	output_c_module_init_list_2(Modules, BaseName, 0, 40, 0, InitFuncs),
	io__write_string("}\n\n#endif\n\n"),

		% Output code to call each of the init functions created
		% above.
	io__write_string("void "),
	output_init_name(BaseName),
	io__write_string("(void);"),
	io__write_string("/* suppress gcc -Wmissing-decls warning */\n"),
	io__write_string("void "),
	output_init_name(BaseName),
	io__write_string("(void)\n"),
	io__write_string("{\n"),
	io__write_string("#if (defined(USE_GCC_NONLOCAL_GOTOS) && "),
	io__write_string("!defined(USE_ASM_LABELS)) \\\n\t|| "),
	io__write_string("defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \\\n"),
	io__write_string("\t|| defined(DEBUG_LABELS) || !defined(SPEED) \\\n"),
	io__write_string("\t|| defined(NATIVE_GC) \n\n"),
	io__write_string("\tstatic bool done = FALSE;\n"),
	io__write_string("\tif (!done) {\n"),
	io__write_string("\t\tdone = TRUE;\n"),
	output_c_module_init_list_3(0, BaseName, InitFuncs),
	io__write_string("\t}\n"),
	io__write_string("#endif\n"),
	output_c_data_init_list(Modules),
	io__write_string("}\n").

:- pred output_c_module_init_list_2(list(c_module), string, int, int, int, int,
	io__state, io__state).
:- mode output_c_module_init_list_2(in, in, in, in, in, out, di, uo) is det.

output_c_module_init_list_2([], _, _, _, InitFunc, InitFunc) --> [].
output_c_module_init_list_2([c_data(_, _, _, _, _) | Ms], A, B, C, D, E) -->
	output_c_module_init_list_2(Ms, A, B, C, D, E).
output_c_module_init_list_2([c_export(_) | Ms], A, B, C, D, E) -->
	output_c_module_init_list_2(Ms, A, B, C, D, E).
output_c_module_init_list_2([c_code(_, _) | Ms], A, B, C, D, E) -->
	output_c_module_init_list_2(Ms, A, B, C, D, E).
output_c_module_init_list_2([c_module(ModuleName, _) | Ms], BaseName,
		Calls0, MaxCalls, InitFunc0, InitFunc) -->
	( { Calls0 > MaxCalls } ->
		io__write_string("}\n\n"),
		{ InitFunc1 is InitFunc0 + 1 },
		io__write_string("static void "),
		output_bunch_name(BaseName, InitFunc1),
		io__write_string("(void)\n"),
		io__write_string("{\n"),
		{ Calls1 = 1 }
	;
		{ InitFunc1 = InitFunc0 },
		{ Calls1 is Calls0 + 1 }
	),
	globals__io_lookup_bool_option(split_c_files, SplitFiles),
	( { SplitFiles = yes } ->
		io__write_string("\t{ extern ModuleFunc "),
		output_module_name(ModuleName),
		io__write_string(";\n"),
		io__write_string("\t  "),
		output_module_name(ModuleName),
		io__write_string("(); }\n")
	;
		io__write_string("\t"),
		output_module_name(ModuleName),
		io__write_string("();\n")
	),
	output_c_module_init_list_2(Ms, BaseName,
		Calls1, MaxCalls, InitFunc1, InitFunc).

	% Output calls to all the bunched initialization functions.

:- pred output_c_module_init_list_3(int, string, int, io__state, io__state).
:- mode output_c_module_init_list_3(in, in, in, di, uo) is det.

output_c_module_init_list_3(InitFunc0, BaseName, MaxInitFunc) -->
	( { InitFunc0 > MaxInitFunc } ->
		[]
	;
		io__write_string("\t\t"),
		output_bunch_name(BaseName, InitFunc0),
		io__write_string("();\n"),
		{ InitFunc1 is InitFunc0 + 1},
		output_c_module_init_list_3(InitFunc1, BaseName, MaxInitFunc)
	).


	% Output MR_INIT_BASE_TYPE_INFO(BaseTypeInfo, TypeId);
	% for each base_type_info defined in this module.

:- pred output_c_data_init_list(list(c_module), io__state, io__state).
:- mode output_c_data_init_list(in, di, uo) is det.

output_c_data_init_list([]) --> [].
output_c_data_init_list([c_export(_) | Ms]) -->
	output_c_data_init_list(Ms).
output_c_data_init_list([c_code(_, _) | Ms]) -->
	output_c_data_init_list(Ms).
output_c_data_init_list([c_module(_, _) | Ms]) -->
	output_c_data_init_list(Ms).
output_c_data_init_list([c_data(BaseName, DataName, _, _, _) | Ms])  -->
	(
		{ DataName = base_type(info, TypeName, Arity) }
	->
		io__write_string("\tMR_INIT_BASE_TYPE_INFO(\n\t\t"),
		output_data_addr(BaseName, DataName),
		io__write_string(",\n\t\t"),
		{ string__append(BaseName, "__", UnderscoresModule) },
		( 
			{ string__append(UnderscoresModule, _, TypeName) } 
		->
			[]
		;
			io__write_string(UnderscoresModule)
		),
		{ llds_out__name_mangle(TypeName, MangledTypeName) },
		io__write_string(MangledTypeName),
		io__write_string("_"),
		io__write_int(Arity),
		io__write_string("_0);\n")
	;
		[]
	),
	output_c_data_init_list(Ms).

:- pred output_init_name(string, io__state, io__state).
:- mode output_init_name(in, di, uo) is det.

output_init_name(BaseName0) -->
	io__write_string("mercury__"),
	{ llds_out__name_mangle(BaseName0, BaseName) },
	io__write_string(BaseName),
	io__write_string("__init").

:- pred output_bunch_name(string, int, io__state, io__state).
:- mode output_bunch_name(in, in, di, uo) is det.

output_bunch_name(BaseName0, Number) -->
	io__write_string("mercury__"),
	{ llds_out__name_mangle(BaseName0, BaseName) },
	io__write_string(BaseName),
	io__write_string("_bunch_"),
	io__write_int(Number).

:- pred output_module_name(string, io__state, io__state).
:- mode output_module_name(in, di, uo) is det.

output_module_name(ModuleName0) -->
	io__write_string("mercury__"),
	{ llds_out__name_mangle(ModuleName0, ModuleName) },
	io__write_string(ModuleName).

:- pred output_c_module_list(list(c_module), decl_set, string,
	io__state, io__state).
:- mode output_c_module_list(in, in, in, di, uo) is det.

output_c_module_list([], _, _) --> [].
output_c_module_list([M | Ms], DeclSet0, BaseName) -->
	output_c_module(M, DeclSet0, DeclSet, BaseName),
	output_c_module_list(Ms, DeclSet, BaseName).

:- pred output_c_module(c_module, decl_set, decl_set, string,
	io__state, io__state).
:- mode output_c_module(in, in, out, in, di, uo) is det.

output_c_module(c_module(ModuleName, Procedures), DeclSet0, DeclSet, _) -->
	io__write_string("\n"),
	output_c_procedure_list_decls(Procedures, DeclSet0, DeclSet),
	io__write_string("\n"),
	io__write_string("BEGIN_MODULE("),
	output_module_name(ModuleName),
	io__write_string(")\n"),
	{ gather_c_module_labels(Procedures, Labels) },
	output_c_label_init_list(Labels),
	io__write_string("BEGIN_CODE\n"),
	io__write_string("\n"),
	globals__io_lookup_bool_option(auto_comments, PrintComments),
	globals__io_lookup_bool_option(emit_c_loops, EmitCLoops),
	output_c_procedure_list(Procedures, PrintComments, EmitCLoops),
	io__write_string("END_MODULE\n").

output_c_module(c_data(BaseName, VarName, ExportedFromModule, ArgVals, _Refs),
		DeclSet0, DeclSet, _) -->
	io__write_string("\n"),
	{ DataAddr = data_addr(data_addr(BaseName, VarName)) },
	output_cons_arg_decls(ArgVals, "", "", 0, _, DeclSet0, DeclSet1),
		% The code for data local to a Mercury module
		% should normally be visible only within the C file
		% generated for that module. However, if we generate
		% multiple C files, the code in each C file must be
		% visible to the other C files for that Mercury module.
	( { ExportedFromModule = yes } ->
		{ ExportedFromFile = yes }
	;
		globals__io_lookup_bool_option(split_c_files, SplitFiles),
		{ ExportedFromFile = SplitFiles }
	),
	output_const_term_decl(ArgVals, DataAddr, ExportedFromFile, "", "",
		0, _),
	{ set__insert(DeclSet1, DataAddr, DeclSet) }.

output_c_module(c_code(C_Code, Context), DeclSet, DeclSet, _) -->
	globals__io_lookup_bool_option(auto_comments, PrintComments),
	( { PrintComments = yes } ->
		io__write_string("/* "),
		prog_out__write_context(Context),
		io__write_string(" pragma(c_code) */\n")
	;
		[]
	),
	io__write_string(C_Code),
	io__write_string("\n").

output_c_module(c_export(PragmaExports), DeclSet, DeclSet, BaseName) -->
	(
		{ PragmaExports = [_|_] }
	->
		globals__io_lookup_bool_option(split_c_files, SplitFiles),
		( { SplitFiles = yes } ->
			io__write_strings(
				["#include ""../", BaseName, ".h""\n"])
		;
			io__write_strings(["#include """, BaseName, ".h""\n"])
		),
		output_exported_c_functions(PragmaExports)
	;
		% Don't spit out a #include if there are no pragma exports
		[]	
	).

	% output_c_header_include_lines reverses the list of c header lines
	% and passes them to output_c_header_include_lines_2 which outputs them.
	% The list must be reversed since they are inserted in reverse order
:- pred output_c_header_include_lines(list(c_header_code),
					io__state, io__state).
:- mode output_c_header_include_lines(in, di, uo) is det.

output_c_header_include_lines(Headers) -->
	{ list__reverse(Headers, RevHeaders) },
	output_c_header_include_lines_2(RevHeaders).

:- pred output_c_header_include_lines_2(list(c_header_code),
					io__state, io__state).
:- mode output_c_header_include_lines_2(in, di, uo) is det.

output_c_header_include_lines_2([]) --> 
	[].
output_c_header_include_lines_2([Code - Context|Hs]) -->
	globals__io_lookup_bool_option(auto_comments, PrintComments),
	( { PrintComments = yes } ->
		io__write_string("/* "),
		prog_out__write_context(Context),
		io__write_string(" pragma(c_code) */\n")
	;
		[]
	),
	io__write_string(Code),
	io__write_string("\n"),
	output_c_header_include_lines_2(Hs).

:- pred output_exported_c_functions(list(string), io__state, io__state).
:- mode output_exported_c_functions(in, di, uo) is det.
output_exported_c_functions([]) --> [].
output_exported_c_functions([F | Fs]) -->
	io__write_string(F),
	output_exported_c_functions(Fs).

:- pred output_c_label_decl_list(list(label), decl_set, decl_set,
	io__state, io__state).
:- mode output_c_label_decl_list(in, in, out, di, uo) is det.

output_c_label_decl_list([], DeclSet, DeclSet) --> [].
output_c_label_decl_list([Label | Labels], DeclSet0, DeclSet) -->
	output_c_label_decl(Label, DeclSet0, DeclSet1),
	output_c_label_decl_list(Labels, DeclSet1, DeclSet).

:- pred output_c_label_decl(label, decl_set, decl_set, io__state, io__state).
:- mode output_c_label_decl(in, in, out, di, uo) is det.

output_c_label_decl(Label, DeclSet0, DeclSet) -->
	(
		{ Label = exported(_) },
		io__write_string("Define_extern_entry(")
	;
		{ Label = local(_) },
		% The code for procedures local to a Mercury module
		% should normally be visible only within the C file
		% generated for that module. However, if we generate
		% multiple C files, the code in each C file must be
		% visible to the other C files for that Mercury module.
		globals__io_lookup_bool_option(split_c_files,
			SplitFiles),
		( { SplitFiles = no } ->
			io__write_string("Declare_static(")
		;
			io__write_string("Define_extern_entry(")
		)
	;
		{ Label = c_local(_) },
		io__write_string("Declare_local(")
	;
		{ Label = local(_, _) },
		io__write_string("Declare_label(")
	),
	{ set__insert(DeclSet0, code_addr(label(Label)), DeclSet) },
	output_label(Label),
	io__write_string(");\n").

:- pred output_c_label_init_list(list(label), io__state, io__state).
:- mode output_c_label_init_list(in, di, uo) is det.

output_c_label_init_list([]) --> [].
output_c_label_init_list([Label | Labels]) -->
	output_c_label_init(Label),
	output_c_label_init_list(Labels).

:- pred output_c_label_init(label, io__state, io__state).
:- mode output_c_label_init(in, di, uo) is det.

output_c_label_init(Label) -->
	(
		{ Label = exported(_) },
		io__write_string("\tinit_entry("),
		output_label(Label),
		io__write_string(");\n")
	;
		{ Label = local(_) },
		io__write_string("\tinit_entry("),
		output_label(Label),
		io__write_string(");\n")
	;
		{ Label = c_local(_) },
		io__write_string("\tinit_local("),
		output_label(Label),
		io__write_string(");\n")
	;
		{ Label = local(_, _) },
		io__write_string("\tinit_label("),
		output_label(Label),
		io__write_string(");\n")
	).

:- pred output_c_procedure_list_decls(list(c_procedure), decl_set, decl_set,
	io__state, io__state).
:- mode output_c_procedure_list_decls(in, in, out, di, uo) is det.

output_c_procedure_list_decls([], DeclSet, DeclSet) --> [].
output_c_procedure_list_decls([Proc | Procs], DeclSet0, DeclSet) -->
	output_c_procedure_decls(Proc, DeclSet0, DeclSet1),
	output_c_procedure_list_decls(Procs, DeclSet1, DeclSet).

:- pred output_c_procedure_list(list(c_procedure), bool, bool,
				io__state, io__state).
:- mode output_c_procedure_list(in, in, in, di, uo) is det.

output_c_procedure_list([], _, _) --> [].
output_c_procedure_list([Proc | Procs], PrintComments, EmitCLoops) -->
	output_c_procedure(Proc, PrintComments, EmitCLoops),
	output_c_procedure_list(Procs, PrintComments, EmitCLoops).

:- pred output_c_procedure_decls(c_procedure, decl_set, decl_set,
				io__state, io__state).
:- mode output_c_procedure_decls(in, in, out, di, uo) is det.

output_c_procedure_decls(Proc, DeclSet0, DeclSet) -->
	{ Proc = c_procedure(_Name, _Arity, _ModeNum0, Instrs) },
	output_instruction_list_decls(Instrs, DeclSet0, DeclSet).

:- pred output_c_procedure(c_procedure, bool, bool,
	io__state, io__state).
:- mode output_c_procedure(in, in, in, di, uo) is det.

output_c_procedure(Proc, PrintComments, EmitCLoops) -->
	{ Proc = c_procedure(Name, Arity, ModeNum0, Instrs) },
	( { PrintComments = yes } ->
		io__write_string("\n/*-------------------------------------"),
		io__write_string("------------------------------------*/\n")
	;
		[]
	),
	io__write_string("/* code for predicate '"),
		% Now that we have unused_args.m mangling predicate names,
		% we should probably demangle them here.
	io__write_string(Name),
	io__write_string("'/"),
	io__write_int(Arity),
	io__write_string(" in mode "),
	{ ModeNum is ModeNum0 mod 10000 },	% strip off the priority
	io__write_int(ModeNum),
	io__write_string(" */\n"),
	{ llds_out__find_caller_label(Instrs, CallerLabel) },
	{ set__init(ContLabelSet0) },
	{ llds_out__find_cont_labels(Instrs, ContLabelSet0, ContLabelSet) },
	{ set__init(WhileSet0) },
	( { EmitCLoops = yes } ->
		{ llds_out__find_while_labels(Instrs, WhileSet0, WhileSet) }
	;
		{ WhileSet = WhileSet0 }
	),
	output_instruction_list(Instrs, PrintComments,
		CallerLabel - ContLabelSet, WhileSet).

	% Find the entry label for the procedure, 
	% for use as the profiling "caller label"
	% field in calls within this procedure.

:- pred llds_out__find_caller_label(list(instruction), label).
:- mode llds_out__find_caller_label(in, out) is det.

llds_out__find_caller_label([], _) :-
	error("cannot find caller label").
llds_out__find_caller_label([Instr0 - _ | Instrs], CallerLabel) :-
	( Instr0 = label(Label) ->
		( Label = local(_, _) ->
			error("caller label is internal label")
		;
			CallerLabel = Label
		)
	;
		llds_out__find_caller_label(Instrs, CallerLabel)
	).

	% Locate all the labels which are the continutation labels for calls
	% or nondet disjunctions, and store them in ContLabelSet.

:- pred llds_out__find_cont_labels(list(instruction), set(label), set(label)).
:- mode llds_out__find_cont_labels(in, in, out) is det.

llds_out__find_cont_labels([], ContLabelSet, ContLabelSet).
llds_out__find_cont_labels([Instr - _ | Instrs], ContLabelSet0, ContLabelSet)
		:-
	(
		(
			Instr = call(_, label(ContLabel), _, _)
		;
			Instr = mkframe(_Comment2, _SlotCount, label(ContLabel))
		;
			Instr = modframe(label(ContLabel))
		;
			Instr = assign(redoip(lval(maxfr)), 
				const(code_addr_const(label(ContLabel))))
		)
	->
		set__insert(ContLabelSet0, ContLabel, ContLabelSet1)
	;
		Instr = block(_, _, Block)
	->
		llds_out__find_cont_labels(Block, ContLabelSet0, ContLabelSet1)
	;
		ContLabelSet1 = ContLabelSet0
	),
	llds_out__find_cont_labels(Instrs, ContLabelSet1, ContLabelSet).

	% Locate all the labels which can be profitably turned into
	% labels starting while loops. The idea is to do this transform:
	%
	% L1:				L1:
	%				     while (1) {
	%	...				...
	%	if (...) goto L1		if (...) continue
	%	...		   =>		...
	%	if (...) goto L?		if (...) goto L?
	%	...				...
	%	if (...) goto L1		if (...) continue
	%	...				...
	%					break;
	%				     }
	% L2:				L2:
	%
	% The second of these is better if we don't have fast jumps.

:- pred llds_out__find_while_labels(list(instruction), set(label), set(label)).
:- mode llds_out__find_while_labels(in, in, out) is det.

llds_out__find_while_labels([], WhileSet, WhileSet).
llds_out__find_while_labels([Instr0 - _ | Instrs0], WhileSet0, WhileSet) :-
	(
		Instr0 = label(Label),
		llds_out__is_while_label(Label, Instrs0, Instrs1, 0, UseCount),
		UseCount > 0
	->
		set__insert(WhileSet0, Label, WhileSet1),
		llds_out__find_while_labels(Instrs1, WhileSet1, WhileSet)
	;
		llds_out__find_while_labels(Instrs0, WhileSet0, WhileSet)
	).

:- pred llds_out__is_while_label(label, list(instruction), list(instruction),
	int, int).
:- mode llds_out__is_while_label(in, in, out, in, out) is det.

llds_out__is_while_label(_, [], [], Count, Count).
llds_out__is_while_label(Label, [Instr0 - Comment0 | Instrs0], Instrs,
		Count0, Count) :-
	( Instr0 = label(_) ->
		Count = Count0,
		Instrs = [Instr0 - Comment0 | Instrs0]
	; Instr0 = goto(label(Label)) ->
		Count1 is Count0 + 1,
		llds_out__is_while_label(Label, Instrs0, Instrs, Count1, Count)
	; Instr0 = if_val(_, label(Label)) ->
		Count1 is Count0 + 1,
		llds_out__is_while_label(Label, Instrs0, Instrs, Count1, Count)
	;
		llds_out__is_while_label(Label, Instrs0, Instrs, Count0, Count)
	).

%-----------------------------------------------------------------------------%

:- pred output_instruction_list_decls(list(instruction), decl_set, decl_set,
					io__state, io__state).
:- mode output_instruction_list_decls(in, in, out, di, uo) is det.

output_instruction_list_decls([], DeclSet, DeclSet) --> [].
output_instruction_list_decls([Instr0 - _Comment0 | Instrs],
		DeclSet0, DeclSet) -->
	output_instruction_decls(Instr0, DeclSet0, DeclSet1),
	output_instruction_list_decls(Instrs, DeclSet1, DeclSet).

:- pred output_instruction_decls(instr, decl_set, decl_set,
	io__state, io__state).
:- mode output_instruction_decls(in, in, out, di, uo) is det.

output_instruction_decls(comment(_), DeclSet, DeclSet) --> [].
output_instruction_decls(livevals(_), DeclSet, DeclSet) --> [].
output_instruction_decls(block(_TempR, _TempF, Instrs),
		DeclSet0, DeclSet) --> 
	output_instruction_list_decls(Instrs, DeclSet0, DeclSet).
output_instruction_decls(assign(Lval, Rval), DeclSet0, DeclSet) -->
	output_lval_decls(Lval, "", "", 0, _, DeclSet0, DeclSet1),
	output_rval_decls(Rval, "", "", 0, _, DeclSet1, DeclSet).
output_instruction_decls(call(Target, ContLabel, _, _), DeclSet0, DeclSet) -->
	output_code_addr_decls(Target, "", "", 0, _, DeclSet0, DeclSet1),
	output_code_addr_decls(ContLabel, "", "", 0, _, DeclSet1, DeclSet).
output_instruction_decls(c_code(_), DeclSet, DeclSet) --> [].
output_instruction_decls(mkframe(_, _, FailureContinuation),
		DeclSet0, DeclSet) -->
	output_code_addr_decls(FailureContinuation, "", "", 0, _,
		DeclSet0, DeclSet).
output_instruction_decls(modframe(FailureContinuation), DeclSet0, DeclSet) -->
	output_code_addr_decls(FailureContinuation, "", "", 0, _,
		DeclSet0, DeclSet).
output_instruction_decls(label(_), DeclSet, DeclSet) --> [].
output_instruction_decls(goto(CodeAddr), DeclSet0, DeclSet) -->
	output_code_addr_decls(CodeAddr, "", "", 0, _, DeclSet0, DeclSet).
output_instruction_decls(computed_goto(Rval, _Labels), DeclSet0, DeclSet) -->
	output_rval_decls(Rval, "", "", 0, _, DeclSet0, DeclSet).
output_instruction_decls(if_val(Rval, Target), DeclSet0, DeclSet) -->
	output_rval_decls(Rval, "", "", 0, _, DeclSet0, DeclSet1),
	output_code_addr_decls(Target, "", "", 0, _, DeclSet1, DeclSet).
output_instruction_decls(incr_hp(Lval, _Tag, Rval), DeclSet0, DeclSet) -->
	output_lval_decls(Lval, "", "", 0, _, DeclSet0, DeclSet1),
	output_rval_decls(Rval, "", "", 0, _, DeclSet1, DeclSet).
output_instruction_decls(mark_hp(Lval), DeclSet0, DeclSet) -->
	output_lval_decls(Lval, "", "", 0, _, DeclSet0, DeclSet).
output_instruction_decls(restore_hp(Rval), DeclSet0, DeclSet) -->
	output_rval_decls(Rval, "", "", 0, _, DeclSet0, DeclSet).
output_instruction_decls(store_ticket(Lval), DeclSet0, DeclSet) -->
	output_lval_decls(Lval, "", "", 0, _, DeclSet0, DeclSet).
output_instruction_decls(restore_ticket(Rval), DeclSet0, DeclSet) -->
	output_rval_decls(Rval, "", "", 0, _, DeclSet0, DeclSet).
output_instruction_decls(discard_ticket, DeclSet, DeclSet) --> [].
output_instruction_decls(incr_sp(_, _), DeclSet, DeclSet) --> [].
output_instruction_decls(decr_sp(_), DeclSet, DeclSet) --> [].
output_instruction_decls(pragma_c(_Decls, Inputs, _C_Code, Outputs, _Context),
		DeclSet0, DeclSet) -->
	output_pragma_input_rval_decls(Inputs, DeclSet0, DeclSet1),
	output_pragma_output_lval_decls(Outputs, DeclSet1, DeclSet).

%-----------------------------------------------------------------------------%

:- pred output_instruction_list(list(instruction), bool,
	pair(label, set(label)), set(label), io__state, io__state).
:- mode output_instruction_list(in, in, in, in, di, uo) is det.

output_instruction_list([], _, _, _) --> [].
output_instruction_list([Instr0 - Comment0 | Instrs], PrintComments, ProfInfo,
		WhileSet) -->
	output_instruction_and_comment(Instr0, Comment0,
		PrintComments, ProfInfo),
	( { Instr0 = label(Label), set__member(Label, WhileSet) } ->
		io__write_string("\twhile (1) {\n"),
		output_instruction_list_while(Instrs, Label,
			PrintComments, ProfInfo, WhileSet)
	;
		output_instruction_list(Instrs, PrintComments, ProfInfo,
			WhileSet)
	).

:- pred output_instruction_list_while(list(instruction), label,
	bool, pair(label, set(label)), set(label), io__state, io__state).
:- mode output_instruction_list_while(in, in, in, in, in, di, uo) is det.

output_instruction_list_while([], _, _, _, _) -->
	io__write_string("\tbreak; } /* end while */\n").
output_instruction_list_while([Instr0 - Comment0 | Instrs], Label,
		PrintComments, ProfInfo, WhileSet) -->
	( { Instr0 = label(_) } ->
		io__write_string("\tbreak; } /* end while */\n"),
		output_instruction_list([Instr0 - Comment0 | Instrs],
			PrintComments, ProfInfo, WhileSet)
	; { Instr0 = goto(label(Label)) } ->
		io__write_string("\t/* continue */ } /* end while */\n"),
		output_instruction_list(Instrs, PrintComments, ProfInfo,
			WhileSet)
	; { Instr0 = if_val(Rval, label(Label)) } ->
		io__write_string("\tif ("),
		output_rval(Rval),
		io__write_string(")\n\t\tcontinue;\n"),
		( { PrintComments = yes, Comment0 \= "" } ->
			io__write_string("\t\t/* "),
			io__write_string(Comment0),
			io__write_string(" */\n")
		;
			[]
		),
		output_instruction_list_while(Instrs, Label,
			PrintComments, ProfInfo, WhileSet)
	;
		output_instruction_and_comment(Instr0, Comment0,
			PrintComments, ProfInfo),
		output_instruction_list_while(Instrs, Label,
			PrintComments, ProfInfo, WhileSet)
	).

:- pred output_instruction_and_comment(instr, string, bool,
	pair(label, set(label)), io__state, io__state).
:- mode output_instruction_and_comment(in, in, in, in, di, uo) is det.

output_instruction_and_comment(Instr, Comment, PrintComments,
		ProfInfo) -->
	(
		{ PrintComments = no },
		( { Instr = comment(_) ; Instr = livevals(_) } ->
			[]
		;
			output_instruction(Instr, ProfInfo)
		)
	;
		{ PrintComments = yes },
		output_instruction(Instr, ProfInfo),
		( { Comment = "" } ->
			[]
		;
			io__write_string("\t\t/* "),
			io__write_string(Comment),
			io__write_string(" */\n")
		)
	).

	% output_instruction/2 is only for debugging.
	% Normally we use output_instruction/4.

output_instruction(Instr) -->
	{ set__init(ContLabelSet) },
	{ hlds_pred__initial_proc_id(ProcId) },
	{ ProfInfo = local(proc("DEBUG", predicate, "DEBUG", "DEBUG", 0,
			ProcId)) - ContLabelSet },
	output_instruction(Instr, ProfInfo).

:- pred output_instruction(instr, pair(label, set(label)),
				io__state, io__state).
:- mode output_instruction(in, in, di, uo) is det.

output_instruction(comment(Comment), _) -->
	io__write_strings(["/* ", Comment, " */\n"]).

output_instruction(livevals(LiveVals), _) -->
	io__write_string("/*\n * Live lvalues:\n"),
	{ set__to_sorted_list(LiveVals, LiveValsList) },
	output_livevals(LiveValsList),
	io__write_string(" */\n").

output_instruction(block(TempR, TempF, Instrs), ProfInfo) -->
	io__write_string("\t{\n"),
	( { TempR > 0 } ->
		io__write_string("\tWord "),
		output_temp_decls(TempR, "r"),
		io__write_string(";\n")
	;
		[]
	),
	( { TempF > 0 } ->
		io__write_string("\tFloat "),
		output_temp_decls(TempF, "f"),
		io__write_string(";\n")
	;
		[]
	),
	globals__io_lookup_bool_option(auto_comments, PrintComments),
	{ set__init(WhileSet0) },
	output_instruction_list(Instrs, PrintComments, ProfInfo,
		WhileSet0),
	io__write_string("\t}\n").

output_instruction(assign(Lval, Rval), _) -->
	io__write_string("\t"),
	output_lval(Lval),
	io__write_string(" = "),
	{ llds__lval_type(Lval, Type) },
	output_rval_as_type(Rval, Type),
	io__write_string(";\n").

output_instruction(call(Target, ContLabel, LiveVals, _), ProfInfo) -->
	{ ProfInfo = CallerLabel - _ },
	output_call(Target, ContLabel, CallerLabel),
	output_gc_livevals(LiveVals).

output_instruction(c_code(C_Code_String), _) -->
	io__write_string("\t"),
	io__write_string(C_Code_String).

output_instruction(mkframe(Str, Num, FailureContinuation), _) -->
	io__write_string("\tmkframe("""),
	io__write_string(Str),
	io__write_string(""", "),
	io__write_int(Num),
	io__write_string(", "),
	output_code_addr(FailureContinuation),
	io__write_string(");\n").

output_instruction(modframe(FailureContinuation), _) -->
	io__write_string("\tmodframe("),
	output_code_addr(FailureContinuation),
	io__write_string(");\n").

output_instruction(label(Label), ProfInfo) -->
	output_label_defn(Label),
	maybe_output_update_prof_counter(Label, ProfInfo).

output_instruction(goto(CodeAddr), ProfInfo) -->
	{ ProfInfo = CallerLabel - _ },
	io__write_string("\t"),
	output_goto(CodeAddr, CallerLabel).

output_instruction(computed_goto(Rval, Labels), _) -->
	io__write_string("\tCOMPUTED_GOTO("),
	output_rval_as_type(Rval, unsigned),
	io__write_string(",\n\t\t"),
	output_label_list(Labels),
	io__write_string(");\n").

output_instruction(if_val(Rval, Target), ProfInfo) -->
	{ ProfInfo = CallerLabel - _ },
	io__write_string("\tif ("),
	output_rval_as_type(Rval, bool),
	io__write_string(")\n\t\t"),
	output_goto(Target, CallerLabel).

output_instruction(incr_hp(Lval, MaybeTag, Rval), _) -->
	(
		{ MaybeTag = no },
		io__write_string("\tincr_hp("),
		output_lval_as_word(Lval)
	;
		{ MaybeTag = yes(Tag) },
		io__write_string("\ttag_incr_hp("),
		output_lval_as_word(Lval),
		io__write_string(", "),
		output_tag(Tag)
	),
	io__write_string(", "),
	output_rval_as_type(Rval, word),
	io__write_string(");\n").

output_instruction(mark_hp(Lval), _) -->
	io__write_string("\tmark_hp("),
	output_lval_as_word(Lval),
	io__write_string(");\n").

output_instruction(restore_hp(Rval), _) -->
	io__write_string("\trestore_hp("),
	output_rval_as_type(Rval, word),
	io__write_string(");\n").

output_instruction(store_ticket(Lval), _) -->
	io__write_string("\tstore_ticket("),
	output_lval_as_word(Lval),
	io__write_string(");\n").

output_instruction(restore_ticket(Rval), _) -->
	io__write_string("\trestore_ticket("),
	output_rval_as_type(Rval, word),
	io__write_string(");\n").

output_instruction(discard_ticket, _) -->
	io__write_string("\tdiscard_ticket();\n").

output_instruction(incr_sp(N, Msg), _) -->
	io__write_string("\tincr_sp_push_msg("),
	io__write_int(N),
	io__write_string(", """),
	io__write_string(Msg),
	io__write_string(""");\n").

output_instruction(decr_sp(N), _) -->
	io__write_string("\tdecr_sp_pop_msg("),
	io__write_int(N),
	io__write_string(");\n").

	% The code we produce for pragma(c_code, ...) is in the form
	% {
	%	<declaration of one local variable for each one in the proc>
	%	<declarations for any rvals and lvals used, if needed>
	%	<assignment of the input regs to the corresponding locals>
	%	<the C code itself>
	%	<assignment to the output regs of the corresponding locals>
	% }
	%
	% The printing of the #line directives is currently disabled;
	% they are printed as comments instead.
output_instruction(pragma_c(Decls, Inputs, C_Code, Outputs, Context), _) -->
	{ term__context_file(Context, File) },
	{ term__context_line(Context, Line) },
	% The context is unfortunately bogus for pragma_c_codes inlined
	% from a .opt file.
	(
		{ Line > 0 },
		{ File \= "" }
	->
		io__write_string("/* #line "),
		io__write_int(Line),
		io__write_string(" """),
		io__write_string(File),
		io__write_string(""" */\n")
	;
		[]
	),
	io__write_string("\t{\n"),
	output_pragma_decls(Decls),
	output_pragma_inputs(Inputs),
	io__write_string("\t\t"),
	io__write_string(C_Code),
	io__write_string("\n"),
	output_pragma_outputs(Outputs),
	io__write_string("\n\t}\n").
%	% We want to generate another #line directive to reset the C compiler's
%	% idea of what it is processing back to the file we are generating.
%	% However, that would require us to pass down here the filename and
%	% line count. This current fudge depends on the code *we* generate
%	% never getting any errors or warnings.
%	(
%		{ Line > 0 },
%		{ File \= "" }
%	->
%		io__write_string("#line 1 ""xxx.c""\n")
%	;
%		[]
%	).

	% Output the local variable declarations at the top of the 
	% pragma_c_code code.
:- pred output_pragma_decls(list(pragma_c_decl), io__state, io__state).
:- mode output_pragma_decls(in, di, uo) is det.

output_pragma_decls([]) --> [].
output_pragma_decls([D|Decls]) -->
	{ D = pragma_c_decl(Type, VarName) },
		% Apart from special cases, the local variables are Words
	{ export__term_to_type_string(Type, VarType) },
	io__write_string("\t\t"),
	io__write_string(VarType),
	io__write_string("\t"),
	io__write_string(VarName),
	io__write_string(";\n"),
	output_pragma_decls(Decls).

	% Output declarations for any rvals used to initialize the inputs
:- pred output_pragma_input_rval_decls(list(pragma_c_input), decl_set, decl_set,
					io__state, io__state).
:- mode output_pragma_input_rval_decls(in, in, out, di, uo) is det.

output_pragma_input_rval_decls([], DeclSet, DeclSet) --> [].
output_pragma_input_rval_decls([I | Inputs], DeclSet0, DeclSet) -->
	{ I = pragma_c_input(_VarName, _Type, Rval) },
	output_rval_decls(Rval, "\t", "\t", 0, _N, DeclSet0, DeclSet1),
	output_pragma_input_rval_decls(Inputs, DeclSet1, DeclSet).

	% Output the input variable assignments at the top of the 
	% pragma_c_code code.
:- pred output_pragma_inputs(list(pragma_c_input), io__state, io__state).
:- mode output_pragma_inputs(in, di, uo) is det.

output_pragma_inputs([]) --> [].
output_pragma_inputs([I|Inputs]) -->
	{ I = pragma_c_input(VarName, Type, Rval) },
	io__write_string("\t\t"),
	io__write_string(VarName),
	io__write_string(" = "),
	(
        	{ Type = term__functor(term__atom("string"), [], _) }
	->
		io__write_string("(String) "),
		output_rval_as_type(Rval, word)
	;
        	{ Type = term__functor(term__atom("float"), [], _) }
	->
		output_rval_as_type(Rval, float)
	;
		output_rval_as_type(Rval, word)
	),
	io__write_string(";\n"),
	output_pragma_inputs(Inputs).

	% Output declarations for any lvals used for the outputs
:- pred output_pragma_output_lval_decls(list(pragma_c_output),
				decl_set, decl_set, io__state, io__state).
:- mode output_pragma_output_lval_decls(in, in, out, di, uo) is det.

output_pragma_output_lval_decls([], DeclSet, DeclSet) --> [].
output_pragma_output_lval_decls([O | Outputs], DeclSet0, DeclSet) -->
	{ O = pragma_c_output(Lval, _Type, _VarName) },
	output_lval_decls(Lval, "\t", "\t", 0, _N, DeclSet0, DeclSet1),
	output_pragma_output_lval_decls(Outputs, DeclSet1, DeclSet).

	% Output the output variable assignments at the bottom of the
	% pragma_c_code
:- pred output_pragma_outputs(list(pragma_c_output), io__state, io__state).
:- mode output_pragma_outputs(in, di, uo) is det.

output_pragma_outputs([]) --> [].
output_pragma_outputs([O|Outputs]) --> 
	{ O = pragma_c_output(Lval, Type, VarName) },
	io__write_string("\t\t"),
	output_lval_as_word(Lval),
	io__write_string(" = "),
	(
        	{ Type = term__functor(term__atom("string"), [], _) }
	->
		io__write_string("(Word) "),
		io__write_string(VarName)
	;
        	{ Type = term__functor(term__atom("float"), [], _) }
	->
		io__write_string("float_to_word("),
		io__write_string(VarName),
		io__write_string(")")
	;
		io__write_string(VarName)
	),
	io__write_string(";\n"),
	output_pragma_outputs(Outputs).

:- pred output_livevals(list(lval), io__state, io__state).
:- mode output_livevals(in, di, uo) is det.

output_livevals([]) --> [].
output_livevals([Lval|Lvals]) -->
	io__write_string(" *\t"),
	output_lval(Lval),
	io__write_string("\n"),
	output_livevals(Lvals).

:- pred output_gc_livevals(list(liveinfo), io__state, io__state).
:- mode output_gc_livevals(in, di, uo) is det.

output_gc_livevals(LiveVals) -->
	globals__io_lookup_bool_option(auto_comments, PrintAutoComments),
	( { PrintAutoComments = yes } ->
		io__write_string("/*\n"),
		io__write_string(" * Garbage collection livevals info\n"),
		output_gc_livevals_2(LiveVals),
		io__write_string(" */\n")
	;
		[]
	).

:- pred output_gc_livevals_2(list(liveinfo), io__state, io__state).
:- mode output_gc_livevals_2(in, di, uo) is det.

output_gc_livevals_2([]) --> [].
output_gc_livevals_2([live_lvalue(Lval, LiveValueType, TypeParams)|Lvals]) -->
	io__write_string(" *\t"),
	output_lval(Lval),
	io__write_string("\t"),
	output_live_value_type(LiveValueType),
	io__write_string("\t"),
	output_gc_livevals_params(TypeParams),
	io__write_string("\n"),
	output_gc_livevals_2(Lvals).

:- pred output_gc_livevals_params(assoc_list(var, lval), io__state, io__state).
:- mode output_gc_livevals_params(in, di, uo) is det.
output_gc_livevals_params([]) --> [].
output_gc_livevals_params([Var - Lval | Lvals]) -->
	{ term__var_to_int(Var, VarInt) },
	io__write_int(VarInt),
	io__write_string(" - "),
	output_lval(Lval),
	io__write_string("  "),
	output_gc_livevals_params(Lvals).

:- pred output_live_value_type(live_value_type, io__state, io__state).
:- mode output_live_value_type(in, di, uo) is det.
output_live_value_type(succip) --> io__write_string("succip").
output_live_value_type(curfr) --> io__write_string("curfr").
output_live_value_type(maxfr) --> io__write_string("maxfr").
output_live_value_type(redoip) --> io__write_string("redoip").
output_live_value_type(hp) --> io__write_string("hp").
output_live_value_type(unwanted) --> io__write_string("unwanted").
output_live_value_type(var(Type, Inst)) --> 
	io__write_string("var("),
	{ varset__init(NewVarset) },
	mercury_output_term(Type, NewVarset, no),
	io__write_string(", "),
	mercury_output_inst(Inst, NewVarset),
	io__write_string(")").

:- pred output_temp_decls(int, string, io__state, io__state).
:- mode output_temp_decls(in, in, di, uo) is det.

output_temp_decls(N, Type) --> 
	output_temp_decls_2(1, N, Type).

:- pred output_temp_decls_2(int, int, string, io__state, io__state).
:- mode output_temp_decls_2(in, in, in, di, uo) is det.

output_temp_decls_2(Next, Max, Type) --> 
	( { Next =< Max } ->
		( { Next > 1 } ->
			io__write_string(", ")
		;
			[]
		),
		io__write_string("temp"),
		io__write_string(Type),
		io__write_int(Next),
		{ Next1 is Next + 1 },
		output_temp_decls_2(Next1, Max, Type)
	;
		[]
	).

% output_rval_decls(Rval, ...) outputs the declarations of any
% static constants, etc. that need to be declared before
% output_rval(Rval) is called.

% Every time we emit a declaration for a symbol, we insert it into the
% set of symbols we've already declared. That way, we avoid generating
% the same symbol twice, which would cause an error in the C code.

:- pred output_rval_decls(rval, string, string, int, int, decl_set, decl_set,
	io__state, io__state).
:- mode output_rval_decls(in, in, in, in, out, in, out, di, uo) is det.

output_rval_decls(lval(Lval), FirstIndent, LaterIndent, N0, N,
		DeclSet0, DeclSet) -->
	output_lval_decls(Lval, FirstIndent, LaterIndent, N0, N,
		DeclSet0, DeclSet).
output_rval_decls(var(_), _, _, _, _, _, _) --> 
	{ error("output_rval_decls: unexpected var") }.
output_rval_decls(mkword(_, Rval), FirstIndent, LaterIndent, N0, N,
		DeclSet0, DeclSet) --> 
	output_rval_decls(Rval, FirstIndent, LaterIndent, N0, N,
		DeclSet0, DeclSet).
output_rval_decls(const(Const), FirstIndent, LaterIndent, N0, N,
		DeclSet0, DeclSet) -->
	( { Const = code_addr_const(CodeAddress) } ->
		output_code_addr_decls(CodeAddress, FirstIndent, LaterIndent,
			N0, N, DeclSet0, DeclSet)
	; { Const = data_addr_const(DataAddr) } ->
		( { set__member(data_addr(DataAddr), DeclSet0) } ->
			{ N = N0 },
			{ DeclSet = DeclSet0 }
		;
			{ set__insert(DeclSet0, data_addr(DataAddr), DeclSet) },
			output_data_addr_decls(DataAddr,
				FirstIndent, LaterIndent, N0, N)
		)
	; { Const = float_const(FloatVal) } ->
		%
		% If floats are boxed, and the static ground terms
		% option is enabled, then for each float constant
		% which we might want to box we declare a static const
		% variable holding that constant. 
		%
		globals__io_lookup_bool_option(unboxed_float, UnboxedFloat),
		globals__io_lookup_bool_option(static_ground_terms,
			StaticGroundTerms),
		( { UnboxedFloat = no, StaticGroundTerms = yes } ->
			{ llds_out__float_literal_name(FloatVal, FloatName) },
			{ FloatLabel = float_label(FloatName) },
			( { set__member(FloatLabel, DeclSet0) } ->
				{ N = N0 },
				{ DeclSet = DeclSet0 }
			;
				{ set__insert(DeclSet0, FloatLabel, DeclSet) },
				{ string__float_to_string(FloatVal,
					FloatString) },
				output_indent(FirstIndent, LaterIndent, N0),
				{ N is N0 + 1 },
				io__write_strings([
					"static const Float ",
					"mercury_float_const_", FloatName,
					" = ", FloatString, ";\n"
				])
			)
		;
			{ N = N0 },
			{ DeclSet = DeclSet0 }
		)
	;
		{ N = N0 },
		{ DeclSet = DeclSet0 }
	).
output_rval_decls(unop(_, Rval), FirstIndent, LaterIndent, N0, N,
		DeclSet0, DeclSet) -->
	output_rval_decls(Rval, FirstIndent, LaterIndent, N0, N,
		DeclSet0, DeclSet).
output_rval_decls(binop(Op, Rval1, Rval2), FirstIndent, LaterIndent, N0, N,
		DeclSet0, DeclSet) -->
	output_rval_decls(Rval1, FirstIndent, LaterIndent, N0, N1,
		DeclSet0, DeclSet1),
	output_rval_decls(Rval2, FirstIndent, LaterIndent, N1, N2,
		DeclSet1, DeclSet2),
		%
		% If floats are boxed, and the static ground terms
		% option is enabled, then for each float constant
		% which we might want to box we declare a static const
		% variable holding that constant. 
		%
	( { llds_out__float_op(Op, OpStr) } ->
	    globals__io_lookup_bool_option(unboxed_float, UnboxFloat),
	    globals__io_lookup_bool_option(static_ground_terms,
					StaticGroundTerms),
	    (
		{ UnboxFloat = no, StaticGroundTerms = yes },
		{ llds_out__float_const_binop_expr_name(Op, Rval1, Rval2,
			FloatName) }
	    ->
		{ FloatLabel = float_label(FloatName) },
		( { set__member(FloatLabel, DeclSet2) } ->
			{ N = N2 },
			{ DeclSet = DeclSet2 }
		;
			{ set__insert(DeclSet2, FloatLabel, DeclSet) },
			output_indent(FirstIndent, LaterIndent, N2),
			{ N is N2 + 1 },
			io__write_string(
				"static const Float mercury_float_const_"),
			io__write_string(FloatName),
			io__write_string(" = "),
				% note that we just output the expression
				% here, and let the C compiler evaluate it,
				% rather than evaluating it ourselves;
				% this avoids having to deal with some nasty
				% issues regarding floating point accuracy
				% when doing cross-compilation.
			output_rval_as_type(Rval1, float),
			io__write_string(" "),
			io__write_string(OpStr),
			io__write_string(" "),
			output_rval_as_type(Rval2, float),
			io__write_string(";\n")
		)
	    ;
		{ N = N2 },
		{ DeclSet = DeclSet2 }
	    )
	;
	    { N = N2 },
	    { DeclSet = DeclSet2 }
	).
output_rval_decls(create(_Tag, ArgVals, _, Label), FirstIndent, LaterIndent,
		N0, N, DeclSet0, DeclSet) -->
	{ CreateLabel = create_label(Label) },
	( { set__member(CreateLabel, DeclSet0) } ->
		{ N = N0 },
		{ DeclSet = DeclSet0 }
	;
		{ set__insert(DeclSet0, CreateLabel, DeclSet1) },
		output_cons_arg_decls(ArgVals, FirstIndent, LaterIndent, N0, N1,
			DeclSet1, DeclSet),
		output_const_term_decl(ArgVals, CreateLabel, no, FirstIndent,
			LaterIndent, N1, N)
	).
output_rval_decls(mem_addr(MemRef), FirstIndent, LaterIndent,
		N0, N, DeclSet0, DeclSet) -->
	output_mem_ref_decls(MemRef, FirstIndent, LaterIndent,
		N0, N, DeclSet0, DeclSet).

:- pred output_mem_ref_decls(mem_ref, string, string, int, int,
	decl_set, decl_set, io__state, io__state).
:- mode output_mem_ref_decls(in, in, in, in, out, in, out, di, uo) is det.

output_mem_ref_decls(stackvar_ref(_), _, _, N, N, DeclSet, DeclSet) --> [].
output_mem_ref_decls(framevar_ref(_), _, _, N, N, DeclSet, DeclSet) --> [].
output_mem_ref_decls(heap_ref(Rval, _, _), FirstIndent, LaterIndent, N0, N,
		DeclSet0, DeclSet) -->
	output_rval_decls(Rval, FirstIndent, LaterIndent, N0, N,
		DeclSet0, DeclSet).

%-----------------------------------------------------------------------------%

% The following predicates are used to compute the names used for
% floating point static constants.

:- pred llds_out__float_const_expr_name(rval::in, string::out) is semidet.

% Given an rval, succeed iff it is a floating point constant expression;
% if so, return a name for that rval that is suitable for use in a C identifier.
% Different rvals must be given different names.

llds_out__float_const_expr_name(Expr, Name) :-
	( Expr = const(float_const(Float)) ->
		llds_out__float_literal_name(Float, Name)
	; Expr = binop(Op, Arg1, Arg2) ->
		llds_out__float_const_binop_expr_name(Op, Arg1, Arg2, Name)
	;
		fail
	).

:- pred llds_out__float_const_binop_expr_name(binary_op::in, rval::in, rval::in,
				string::out) is semidet.

% Given a binop rval, succeed iff that rval is a floating point constant
% expression; if so, return a name for that rval that is suitable for use in
% a C identifier.  Different rvals must be given different names.

llds_out__float_const_binop_expr_name(Op, Arg1, Arg2, Name) :-
	llds_out__float_op_name(Op, OpName),
	llds_out__float_const_expr_name(Arg1, Arg1Name),
	llds_out__float_const_expr_name(Arg2, Arg2Name),
	% we use prefix notation (operator, argument, argument)
	% rather than infix, to ensure that different rvals get
	% different names
	string__append_list([OpName, "_", Arg1Name, "_", Arg2Name],
		Name).

:- pred llds_out__float_literal_name(float::in, string::out) is det.

% Given an rval which is a floating point literal, return
% a name for that rval that is suitable for use in a C identifier.
% Different rvals must be given different names.

llds_out__float_literal_name(Float, FloatName) :-
	%
	% The name of the variable is based on the
	% value of the float const, with "pt" instead
	% of ".", "plus" instead of "+", and "neg" instead of "-".
	%
	string__float_to_string(Float, FloatName0),
	string__replace_all(FloatName0, ".", "pt", FloatName1),
	string__replace_all(FloatName1, "+", "plus", FloatName2),
	string__replace_all(FloatName2, "-", "neg", FloatName).

:- pred llds_out__float_op_name(binary_op, string).
:- mode llds_out__float_op_name(in, out) is semidet.
	
% succeed iff the binary operator is an operator whose return
% type is float; bind the output string to a name for that operator
% that is suitable for use in a C identifier

llds_out__float_op_name(float_plus, "plus").
llds_out__float_op_name(float_minus, "minus").
llds_out__float_op_name(float_times, "times").
llds_out__float_op_name(float_divide, "divide").

%-----------------------------------------------------------------------------%

	% We output constant terms as follows:
	%
	%	static const struct <foo>_struct {
	%		Word field1;
	%		Float field2;
	%		Word * field3;
	%		...
	%	} <foo> = {
	%		...
	%	};
	%
	% Unless the term contains code addresses, and we don't have
	% static code addresses available, in which case we'll have
	% to initialize them dynamically, so we must omit `const'
	% from the above structure.

:- pred output_const_term_decl(list(maybe(rval)), decl_id, bool, string, string,
	int, int, io__state, io__state).
:- mode output_const_term_decl(in, in, in, in, in, in, out, di, uo) is det.

output_const_term_decl(ArgVals, DeclId, Exported, FirstIndent, LaterIndent,
		N1, N) -->
	output_indent(FirstIndent, LaterIndent, N1),
	{ N is N1 + 1 },
	( { Exported = yes } ->
		[]
	;
		io__write_string("static ")
	),
	globals__io_get_globals(Globals),
	{ globals__have_static_code_addresses(Globals, StaticCode) },
	(
		{ StaticCode = no },
		{ DeclId = data_addr(data_addr(_, base_type(info, _, _))) }
	->
		[]
	;
		io__write_string("const ")
	),
	io__write_string("struct "),
	output_decl_id(DeclId),
	io__write_string("_struct {\n"),
	output_cons_arg_types(ArgVals, "\t", 1),
	io__write_string("} "),
	output_decl_id(DeclId),
	io__write_string(" = {\n"),
	output_cons_args(ArgVals, "\t"),
	io__write_string(LaterIndent),
	io__write_string("};\n").

:- pred output_decl_id(decl_id, io__state, io__state).
:- mode output_decl_id(in, di, uo) is det.

output_decl_id(create_label(N)) -->
	io__write_string("mercury_const_"),
	io__write_int(N).
output_decl_id(data_addr(data_addr(ModuleName, VarName))) -->
	output_data_addr(ModuleName, VarName).
output_decl_id(code_addr(_CodeAddress)) -->
	{ error("output_decl_id: code_addr unexpected") }.
output_decl_id(float_label(_Label)) -->
	{ error("output_decl_id: float_label unexpected") }.

:- pred output_cons_arg_types(list(maybe(rval)), string, int, 
				io__state, io__state).
:- mode output_cons_arg_types(in, in, in, di, uo) is det.

output_cons_arg_types([], _, _) --> [].
output_cons_arg_types([Arg | Args], Indent, ArgNum) -->
	( { Arg = yes(Rval) } ->
		io__write_string(Indent),
		llds_out__rval_type_as_arg(Rval, Type),
		output_llds_type(Type),
		io__write_string(" f"),
		io__write_int(ArgNum),
		io__write_string(";\n")
	;
		{ error("output_cons_arg_types: missing arg") }
	),
	{ ArgNum1 is ArgNum + 1 },
	output_cons_arg_types(Args, Indent, ArgNum1).

	% Given an rval, figure out the type it would have as
	% an argument.  Normally that's the same as its usual type;
	% the exception is that for boxed floats, the type is data_ptr
	% (i.e. the type of the boxed value) rather than float
	% (the type of the unboxed value).
	%
:- pred llds_out__rval_type_as_arg(rval, llds_type, io__state, io__state).
:- mode llds_out__rval_type_as_arg(in, out, di, uo) is det.

llds_out__rval_type_as_arg(Rval, ArgType) -->
	{ llds__rval_type(Rval, Type) },
	globals__io_lookup_bool_option(unboxed_float, UnboxFloat),
	( { Type = float, UnboxFloat = no } ->
		{ ArgType = data_ptr }
	;
		{ ArgType = Type }
	).

:- pred output_llds_type(llds_type, io__state, io__state).
:- mode output_llds_type(in, di, uo) is det.

output_llds_type(bool)     --> io__write_string("Integer").
output_llds_type(integer)  --> io__write_string("Integer").
output_llds_type(unsigned) --> io__write_string("Unsigned").
output_llds_type(float)    --> io__write_string("Float").
output_llds_type(word)     --> io__write_string("Word").
output_llds_type(data_ptr) --> io__write_string("const Word *").
output_llds_type(code_ptr) --> io__write_string("Code *").

:- pred output_cons_arg_decls(list(maybe(rval)), string, string, int, int,
	decl_set, decl_set, io__state, io__state).
:- mode output_cons_arg_decls(in, in, in, in, out, in, out, di, uo) is det.

output_cons_arg_decls([], _, _, N, N, DeclSet, DeclSet) --> [].
output_cons_arg_decls([Arg | Args], FirstIndent, LaterIndent, N0, N,
		DeclSet0, DeclSet) -->
	( { Arg = yes(Rval) } ->
		output_rval_decls(Rval, FirstIndent, LaterIndent, N0, N1,
			DeclSet0, DeclSet1)
	;
		{ N1 = N0 },
		{ DeclSet1 = DeclSet0 }
	),
	output_cons_arg_decls(Args, FirstIndent, LaterIndent, N1, N,
		DeclSet1, DeclSet).

:- pred output_cons_args(list(maybe(rval)), string, io__state, io__state).
:- mode output_cons_args(in, in, di, uo) is det.
% 	output_cons_args(Args, Indent):
%	output the arguments, each on its own line prefixing with Indent.

output_cons_args([], _) --> [].
output_cons_args([Arg | Args], Indent) -->
	( { Arg = yes(Rval) } ->
		io__write_string(Indent),
		llds_out__rval_type_as_arg(Rval, TypeAsArg),
		output_rval_as_type(Rval, TypeAsArg)
	;
		% `Arg = no' means the argument is uninitialized,
		% but that would mean the term isn't ground
		{ error("output_cons_args: missing argument") }
	),
	( { Args \= [] } ->
		io__write_string(",\n"),
		output_cons_args(Args, Indent)
	;
		io__write_string("\n")
	).

%-----------------------------------------------------------------------------%

% output_lval_decls(Lval, ...) outputs the declarations of any
% static constants, etc. that need to be declared before
% output_lval(Lval) is called.

:- pred output_lval_decls(lval, string, string, int, int, decl_set, decl_set,
	io__state, io__state).
:- mode output_lval_decls(in, in, in, in, out, in, out, di, uo) is det.

output_lval_decls(field(_, Rval, FieldNum), FirstIndent, LaterIndent, N0, N,
		DeclSet0, DeclSet) -->
	output_rval_decls(Rval, FirstIndent, LaterIndent, N0, N1,
		DeclSet0, DeclSet1),
	output_rval_decls(FieldNum, FirstIndent, LaterIndent, N1, N,
		DeclSet1, DeclSet).
output_lval_decls(reg(_, _), _, _, N, N, DeclSet, DeclSet) --> [].
output_lval_decls(stackvar(_), _, _, N, N, DeclSet, DeclSet) --> [].
output_lval_decls(framevar(_), _, _, N, N, DeclSet, DeclSet) --> [].
output_lval_decls(succip, _, _, N, N, DeclSet, DeclSet) --> [].
output_lval_decls(maxfr, _, _, N, N, DeclSet, DeclSet) --> [].
output_lval_decls(curfr, _, _, N, N, DeclSet, DeclSet) --> [].
output_lval_decls(succfr(Rval), FirstIndent, LaterIndent, N0, N,
		DeclSet0, DeclSet) -->
	output_rval_decls(Rval, FirstIndent, LaterIndent, N0, N,
		DeclSet0, DeclSet).
output_lval_decls(prevfr(Rval), FirstIndent, LaterIndent, N0, N,
		DeclSet0, DeclSet) -->
	output_rval_decls(Rval, FirstIndent, LaterIndent, N0, N,
		DeclSet0, DeclSet).
output_lval_decls(redoip(Rval), FirstIndent, LaterIndent, N0, N,
		DeclSet0, DeclSet) -->
	output_rval_decls(Rval, FirstIndent, LaterIndent, N0, N,
		DeclSet0, DeclSet).
output_lval_decls(succip(Rval), FirstIndent, LaterIndent, N0, N,
		DeclSet0, DeclSet) -->
	output_rval_decls(Rval, FirstIndent, LaterIndent, N0, N,
		DeclSet0, DeclSet).
output_lval_decls(hp, _, _, N, N, DeclSet, DeclSet) --> [].
output_lval_decls(sp, _, _, N, N, DeclSet, DeclSet) --> [].
output_lval_decls(lvar(_), _, _, N, N, DeclSet, DeclSet) --> [].
output_lval_decls(temp(_, _), _, _, N, N, DeclSet, DeclSet) --> [].
output_lval_decls(mem_ref(Rval), FirstIndent, LaterIndent, N0, N,
		DeclSet0, DeclSet) -->
	output_rval_decls(Rval, FirstIndent, LaterIndent, N0, N,
		DeclSet0, DeclSet).

% output_code_addr_decls(CodeAddr, ...) outputs the declarations of any
% extern symbols, etc. that need to be declared before
% output_code_addr(CodeAddr) is called.

:- pred output_code_addr_decls(code_addr, string, string, int, int,
	decl_set, decl_set, io__state, io__state).
:- mode output_code_addr_decls(in, in, in, in, out, in, out, di, uo) is det.

output_code_addr_decls(CodeAddress, FirstIndent, LaterIndent, N0, N,
		DeclSet0, DeclSet) -->
	( { set__member(code_addr(CodeAddress), DeclSet0) } ->
		{ N = N0 },
		{ DeclSet = DeclSet0 }
	;
		{ set__insert(DeclSet0, code_addr(CodeAddress), DeclSet) },
		need_code_addr_decls(CodeAddress, NeedDecl),
		( { NeedDecl = yes } ->
			output_indent(FirstIndent, LaterIndent, N0),
			{ N is N0 + 1 },
			output_code_addr_decls(CodeAddress)
		;
			{ N = N0 }
		)
	).

:- pred need_code_addr_decls(code_addr, bool, io__state, io__state).
:- mode need_code_addr_decls(in, out, di, uo) is det.

need_code_addr_decls(label(Label), Need) -->
	{ 
		Label = exported(_),
		Need = yes
	;
		Label = local(_),
		Need = yes
	;
		Label = c_local(_),
		Need = no
	;
		Label = local(_, _),
		Need = no
	}.
need_code_addr_decls(imported(_), yes) --> [].
need_code_addr_decls(succip, no) --> [].
need_code_addr_decls(do_succeed(_), no) --> [].
need_code_addr_decls(do_redo, NeedDecl) -->
	globals__io_lookup_bool_option(use_macro_for_redo_fail, UseMacro),
	(
		{ UseMacro = yes },
		{ NeedDecl = no }
	;
		{ UseMacro = no },
		{ NeedDecl = yes }
	).
need_code_addr_decls(do_fail, NeedDecl) -->
	globals__io_lookup_bool_option(use_macro_for_redo_fail, UseMacro),
	(
		{ UseMacro = yes },
		{ NeedDecl = no }
	;
		{ UseMacro = no },
		{ NeedDecl = yes }
	).
need_code_addr_decls(do_det_closure, yes) --> [].
need_code_addr_decls(do_semidet_closure, yes) --> [].
need_code_addr_decls(do_nondet_closure, yes) --> [].
need_code_addr_decls(do_not_reached, yes) --> [].

:- pred output_code_addr_decls(code_addr, io__state, io__state).
:- mode output_code_addr_decls(in, di, uo) is det.

output_code_addr_decls(label(Label)) -->
	output_label_as_code_addr_decls(Label).
output_code_addr_decls(imported(ProcLabel)) -->
	io__write_string("Declare_entry("),
	output_proc_label(ProcLabel),
	io__write_string(");\n").
output_code_addr_decls(succip) --> [].
output_code_addr_decls(do_succeed(_)) --> [].
output_code_addr_decls(do_redo) -->
	globals__io_lookup_bool_option(use_macro_for_redo_fail, UseMacro),
	(
		{ UseMacro = yes }
	;
		{ UseMacro = no },
		io__write_string("Declare_entry("),
		io__write_string("do_redo"),
		io__write_string(");\n")
	).
output_code_addr_decls(do_fail) -->
	globals__io_lookup_bool_option(use_macro_for_redo_fail, UseMacro),
	(
		{ UseMacro = yes }
	;
		{ UseMacro = no },
		io__write_string("Declare_entry("),
		io__write_string("do_fail"),
		io__write_string(");\n")
	).
output_code_addr_decls(do_det_closure) -->
	io__write_string("Declare_entry(do_call_det_closure);\n").
output_code_addr_decls(do_semidet_closure) -->
	io__write_string("Declare_entry(do_call_semidet_closure);\n").
output_code_addr_decls(do_nondet_closure) -->
	io__write_string("Declare_entry(do_call_nondet_closure);\n").
output_code_addr_decls(do_not_reached) -->
	io__write_string("Declare_entry(do_not_reached);\n").

:- pred output_label_as_code_addr_decls(label, io__state, io__state).
:- mode output_label_as_code_addr_decls(in, di, uo) is det.

output_label_as_code_addr_decls(exported(ProcLabel)) -->
	io__write_string("Declare_entry("),
	output_label(exported(ProcLabel)),
	io__write_string(");\n").
output_label_as_code_addr_decls(local(ProcLabel)) -->
	globals__io_lookup_bool_option(split_c_files, SplitFiles),
	( { SplitFiles = no } ->
		[]
	;
		io__write_string("Declare_entry("),
		output_label(local(ProcLabel)),
		io__write_string(");\n")
	).
output_label_as_code_addr_decls(c_local(_)) --> [].
output_label_as_code_addr_decls(local(_, _)) --> [].

:- pred output_data_addr_decls(data_addr, string, string, int, int,
	io__state, io__state).
:- mode output_data_addr_decls(in, in, in, in, out, di, uo) is det.

output_data_addr_decls(data_addr(BaseName, VarName),
		FirstIndent, LaterIndent, N0, N) -->
	output_indent(FirstIndent, LaterIndent, N0),
	{ N is N0 + 1 },
	io__write_string("extern "),
	globals__io_get_globals(Globals),

		% Don't make decls of base_type_infos `const' if we
		% don't have static code addresses.
	(
		{ VarName = base_type(info, _, _) },
		{ globals__have_static_code_addresses(Globals, no) }
	->
		[]
	;
		io__write_string("const ")
	),
	io__write_string("struct "),
	output_data_addr(BaseName, VarName), 
	io__write_string("_struct\n"),
	io__write_string(LaterIndent),
	io__write_string("\t"),
	output_data_addr(BaseName, VarName), 
	io__write_string(";\n").

%-----------------------------------------------------------------------------%

:- pred output_indent(string, string, int, io__state, io__state).
:- mode output_indent(in, in, in, di, uo) is det.

output_indent(FirstIndent, LaterIndent, N0) -->
	( { N0 > 0 } ->
		io__write_string(LaterIndent)
	;
		io__write_string(FirstIndent)
	).

%-----------------------------------------------------------------------------%

:- pred maybe_output_update_prof_counter(label, pair(label, set(label)),
	io__state, io__state).
:- mode maybe_output_update_prof_counter(in, in, di, uo) is det.

maybe_output_update_prof_counter(Label, CallerLabel - ContLabelSet) -->
	(
		{ set__member(Label, ContLabelSet) }
	->
		io__write_string("\tupdate_prof_current_proc(LABEL("),
		output_label(CallerLabel),
		io__write_string("));\n")
	;
		[]
	).

%-----------------------------------------------------------------------------%

:- pred output_goto(code_addr, label, io__state, io__state).
:- mode output_goto(in, in, di, uo) is det.

	% Note that we do some optimization here:
	% instead of always outputting `GOTO(<label>)', we
	% output different things for each different kind of label.

output_goto(label(Label), CallerLabel) -->
	(
		{ Label = exported(_) },
		io__write_string("tailcall("),
		output_label_as_code_addr(Label),
		io__write_string(",\n\t\t"),
		output_label_as_code_addr(CallerLabel),
		io__write_string(");\n")
	;
		{ Label = local(_) },
		io__write_string("tailcall("),
		output_label_as_code_addr(Label),
		io__write_string(",\n\t\t"),
		output_label_as_code_addr(CallerLabel),
		io__write_string(");\n")
	;
		{ Label = c_local(_) },
		io__write_string("localtailcall("),
		output_label(Label),
		io__write_string(",\n\t\t"),
		output_label_as_code_addr(CallerLabel),
		io__write_string(");\n")
	;
		{ Label = local(_, _) },
		io__write_string("GOTO_LABEL("),
		output_label(Label),
		io__write_string(");\n")
	).
output_goto(imported(ProcLabel), CallerLabel) -->
	io__write_string("tailcall(ENTRY("),
	output_proc_label(ProcLabel),
	io__write_string("),\n\t\t"),
	output_label_as_code_addr(CallerLabel),
	io__write_string(");\n").
output_goto(succip, _) -->
	io__write_string("proceed();\n").
output_goto(do_succeed(Last), _) -->
	(
		{ Last = no },
		io__write_string("succeed();\n")
	;
		{ Last = yes },
		io__write_string("succeed_discard();\n")
	).
output_goto(do_redo, _) -->
	globals__io_lookup_bool_option(use_macro_for_redo_fail, UseMacro),
	(
		{ UseMacro = yes },
		io__write_string("redo();\n")
	;
		{ UseMacro = no },
		io__write_string("GOTO(ENTRY(do_redo));\n")
	).
output_goto(do_fail, _) -->
	globals__io_lookup_bool_option(use_macro_for_redo_fail, UseMacro),
	(
		{ UseMacro = yes },
		io__write_string("fail();\n")
	;
		{ UseMacro = no },
		io__write_string("GOTO(ENTRY(do_fail));\n")
	).
output_goto(do_det_closure, CallerLabel) -->
	io__write_string("tailcall(ENTRY(do_call_det_closure),\n\t\t"),
	output_label_as_code_addr(CallerLabel),
	io__write_string(");\n").
output_goto(do_semidet_closure, CallerLabel) -->
	io__write_string("tailcall(ENTRY(do_call_semidet_closure),\n\t\t"),
	output_label_as_code_addr(CallerLabel),
	io__write_string(");\n").
output_goto(do_nondet_closure, CallerLabel) -->
	io__write_string("tailcall(ENTRY(do_call_nondet_closure),\n\t\t"),
	output_label_as_code_addr(CallerLabel),
	io__write_string(");\n").
output_goto(do_not_reached, CallerLabel) -->
	io__write_string("tailcall(ENTRY(do_not_reached),\n\t\t"),
	output_label_as_code_addr(CallerLabel),
	io__write_string(");\n").


	% Note that we also do some optimization here by
	% outputting `localcall' rather than `call' for
	% calls to local labels, or `call_localret' for
	% calls which return to local labels (i.e. most of them).

:- pred output_call(code_addr, code_addr, label, io__state, io__state).
:- mode output_call(in, in, in, di, uo) is det.

output_call(Target, Continuation, CallerLabel) -->
	(
		{ Target = label(Label) },
		% We really shouldn't be calling internal labels ...
		{ Label = c_local(_) ; Label = local(_, _) }
	->
		io__write_string("\tlocalcall("),
		output_label(Label),
		io__write_string(",\n\t\t"),
		output_code_addr(Continuation)
	;
		{ Continuation = label(ContLabel) },
		{ ContLabel = c_local(_) ; ContLabel = local(_, _) }
	->
		io__write_string("\tcall_localret("),
		output_code_addr(Target),
		io__write_string(",\n\t\t"),
		output_label(ContLabel)
	;
		io__write_string("\tcall("),
		output_code_addr(Target),
		io__write_string(",\n\t\t"),
		output_code_addr(Continuation)
	),
	io__write_string(",\n\t\t"),
	output_label_as_code_addr(CallerLabel),
	io__write_string(");\n").

:- pred output_code_addr(code_addr, io__state, io__state).
:- mode output_code_addr(in, di, uo) is det.

output_code_addr(label(Label)) -->
	output_label_as_code_addr(Label).
output_code_addr(imported(ProcLabel)) -->
	io__write_string("ENTRY("),
	output_proc_label(ProcLabel),
	io__write_string(")").
output_code_addr(succip) -->
	io__write_string("succip").
output_code_addr(do_succeed(Last)) -->
	(
		{ Last = no },
		io__write_string("ENTRY(do_succeed)")
	;
		{ Last = yes },
		io__write_string("ENTRY(do_last_succeed)")
	).
output_code_addr(do_redo) -->
	io__write_string("ENTRY(do_redo)").
output_code_addr(do_fail) -->
	io__write_string("ENTRY(do_fail)").
output_code_addr(do_det_closure) -->
	io__write_string("ENTRY(do_call_det_closure)").
output_code_addr(do_semidet_closure) -->
	io__write_string("ENTRY(do_call_semidet_closure)").
output_code_addr(do_nondet_closure) -->
	io__write_string("ENTRY(do_call_nondet_closure)").
output_code_addr(do_not_reached) -->
	io__write_string("ENTRY(do_not_reached)").

:- pred output_data_addr(string, data_name, io__state, io__state).
:- mode output_data_addr(in, in, di, uo) is det.

output_data_addr(BaseName0, VarName) -->
	{ llds_out__name_mangle(BaseName0, BaseName) },
	io__write_string("mercury_data_"),
	io__write_string(BaseName),
	(
		{ VarName = common(N) },
		io__write_string("__common_"),
		{ string__int_to_string(N, NStr) },
		io__write_string(NStr)
	;
		{ VarName = base_type(BaseData, TypeName0, TypeArity) },
		{ llds_out__make_base_type_name(BaseData, TypeName0, TypeArity,
			Str) },
		io__write_string("__"),
		io__write_string(Str)
	).

:- pred output_label_as_code_addr(label, io__state, io__state).
:- mode output_label_as_code_addr(in, di, uo) is det.

output_label_as_code_addr(exported(ProcLabel)) -->
	io__write_string("ENTRY("),
	output_label(exported(ProcLabel)),
	io__write_string(")").
output_label_as_code_addr(local(ProcLabel)) -->
	globals__io_lookup_bool_option(split_c_files, SplitFiles),
	( { SplitFiles = no } ->
		io__write_string("STATIC("),
		output_label(local(ProcLabel)),
		io__write_string(")")
	;
		io__write_string("ENTRY("),
		output_label(local(ProcLabel)),
		io__write_string(")")
	).
output_label_as_code_addr(c_local(ProcLabel)) -->
	io__write_string("LABEL("),
	output_label(c_local(ProcLabel)),
	io__write_string(")").
output_label_as_code_addr(local(ProcLabel, N)) -->
	io__write_string("LABEL("),
	output_label(local(ProcLabel, N)),
	io__write_string(")").

:- pred output_label_list(list(label), io__state, io__state).
:- mode output_label_list(in, di, uo) is det.

output_label_list([]) --> [].
output_label_list([Label | Labels]) -->
	io__write_string("LABEL("),
	output_label(Label),
	io__write_string(")"),
	output_label_list_2(Labels).

:- pred output_label_list_2(list(label), io__state, io__state).
:- mode output_label_list_2(in, di, uo) is det.

output_label_list_2([]) --> [].
output_label_list_2([Label | Labels]) -->
	io__write_string(" AND\n\t\t"),
	io__write_string("LABEL("),
	output_label(Label),
	io__write_string(")"),
	output_label_list_2(Labels).

:- pred output_label_defn(label, io__state, io__state).
:- mode output_label_defn(in, di, uo) is det.

output_label_defn(exported(ProcLabel)) -->
	io__write_string("Define_entry("),
	output_label(exported(ProcLabel)),
	io__write_string(");\n").
output_label_defn(local(ProcLabel)) -->
	% The code for procedures local to a Mercury module
	% should normally be visible only within the C file
	% generated for that module. However, if we generate
	% multiple C files, the code in each C file must be
	% visible to the other C files for that Mercury module.
	globals__io_lookup_bool_option(split_c_files, SplitFiles),
	( { SplitFiles = no } ->
		io__write_string("Define_static("),
		output_label(local(ProcLabel)),
		io__write_string(");\n")
	;
		io__write_string("Define_entry("),
		output_label(local(ProcLabel)),
		io__write_string(");\n")
	).
output_label_defn(c_local(ProcLabel)) -->
	io__write_string("Define_local("),
	output_label(c_local(ProcLabel)),
	io__write_string(");\n").
output_label_defn(local(ProcLabel, Num)) -->
	io__write_string("Define_label("),
	output_label(local(ProcLabel, Num)),
	io__write_string(");\n").

% Note that the suffixes _l and _iN used to be interpreted by mod2c,
% which generated different code depending on the suffix.
% We don't generate the _l suffix anymore, since it interferes
% with referring to a label both as local(_) and c_local(_).
% For example, the entry label of a recursive unification predicate
% is referred to as local(_) in type_info structures and as c_local(_)
% in the recursive call.

output_label(exported(ProcLabel)) -->
	output_proc_label(ProcLabel).
output_label(local(ProcLabel)) -->
	output_proc_label(ProcLabel).
output_label(c_local(ProcLabel)) -->
	output_proc_label(ProcLabel).
output_label(local(ProcLabel, Num)) -->
	output_proc_label(ProcLabel),
	io__write_string("_i"),		% i for "internal" (not Intel ;-)
	io__write_int(Num).

output_proc_label(ProcLabel) -->
	{ get_proc_label(ProcLabel, ProcLabelString) },
	io__write_string(ProcLabelString).

get_proc_label(proc(DefiningModule, PredOrFunc, PredModule,
		PredName, Arity, ModeNum0), ProcLabelString) :-
	get_label_name(DefiningModule, PredOrFunc, PredModule,
		PredName, Arity, LabelName),
	( PredOrFunc = function ->
		OrigArity is Arity - 1
	;
		OrigArity = Arity
	),
	string__int_to_string(OrigArity, ArityString),
	proc_id_to_int(ModeNum0, ModeInt),
	ModeNum is ModeInt mod 10000,		% strip off the priority
	string__int_to_string(ModeNum, ModeNumString),
	string__append_list([LabelName, "_", ArityString, "_", ModeNumString], 
		ProcLabelString).

	% For a special proc, output a label of the form:
	% mercury____<PredName>___<TypeModule>__<TypeName>_<TypeArity>_<Mode>
get_proc_label(special_proc(Module, PredName, TypeModule, TypeName, TypeArity,
				ModeNum0), ProcLabelString) :-
	% figure out the LabelName
	DummyArity = -1,	% not used by get_label_name.
	get_label_name("", predicate, "", PredName, DummyArity, LabelName),

	% figure out the ModeNumString
	string__int_to_string(TypeArity, TypeArityString),
	proc_id_to_int(ModeNum0, ModeInt),
	ModeNum is ModeInt mod 10000,		% strip off the priority
	string__int_to_string(ModeNum, ModeNumString),

	% mangle all the relevent names
	llds_out__name_mangle(Module, MangledModule),
	llds_out__name_mangle(TypeModule, MangledTypeModule),
	llds_out__name_mangle(TypeName, MangledTypeName),

	% Module-qualify the type name.
	% To handle locally produced unification preds for imported types,
	% we need to qualify it with both the module name of the
	% type, and also (if it is different) the module name of the
	% current module.
	llds_out__maybe_qualify_name(MangledTypeModule, MangledTypeName,
		QualifiedMangledTypeName),
	llds_out__maybe_qualify_name(MangledModule, QualifiedMangledTypeName,
		FullyQualifiedMangledTypeName),

	% join it all together
	string__append_list( [LabelName, "_", FullyQualifiedMangledTypeName, 
		"_", TypeArityString, "_", ModeNumString], 
		ProcLabelString).

:- pred get_label_name(string, pred_or_func, string, string, int, string).
:- mode get_label_name(in, in, in, in, in, out) is det.

get_label_name(DefiningModule, PredOrFunc, DeclaringModule,
		Name0, Arity, LabelName) :-
	(
		( 
			DeclaringModule = "mercury_builtin"
		;
			Name0 = "main",
			Arity = 2
		;
			string__prefix(Name0, "__")
		)
		% The conditions above define which labels are printed without
		% module qualification.  XXX Changes to runtime/* are necessary
		% to allow `mercury_builtin' labels to be qualified/
		% overloaded.
	->
		LabelName0 = Name0
	;
		llds_out__maybe_qualify_name(DeclaringModule, Name0,
			LabelName0)
	),
	(
		% if this is a specialized version of a predicate
		% defined in some other module, then it needs both
		% module prefixes
		DefiningModule \= DeclaringModule,
		% but we don't do that for "mercury_builtin",
		% because that would give the wrong results
		% for the definitions of term__context_init etc.
		% in mercury_builtin.m.
		DefiningModule \= "mercury_builtin"
	->
		string__append_list([DefiningModule, "__", LabelName0],
			LabelName1)
	;
		LabelName1 = LabelName0
	),
	llds_out__name_mangle(LabelName1, LabelName2),
	(
		PredOrFunc = function,
		string__append("fn__", LabelName2, LabelName3)
	;
		PredOrFunc = predicate,
		LabelName3 = LabelName2
	),
	get_label_prefix(Prefix),
	string__append(Prefix, LabelName3, LabelName).

	% To ensure that Mercury labels don't clash with C symbols, we
	% prefix them with `mercury__'.

:- pred get_label_prefix(string).
:- mode get_label_prefix(out) is det.

get_label_prefix("mercury__"). 

:- pred output_reg(reg_type, int, io__state, io__state).
:- mode output_reg(in, in, di, uo) is det.

output_reg(r, N) -->
	{ llds_out__reg_to_string(r, N, RegName) },
	io__write_string(RegName).
output_reg(f, _) -->
	{ error("Floating point registers not implemented") }.

:- pred output_tag(tag, io__state, io__state).
:- mode output_tag(in, di, uo) is det.

output_tag(Tag) -->
	io__write_string("mktag("),
	io__write_int(Tag),
	io__write_string(")").

	% output an rval, converted to the specified type
	%
:- pred output_rval_as_type(rval, llds_type, io__state, io__state).
:- mode output_rval_as_type(in, in, di, uo) is det.

output_rval_as_type(Rval, DesiredType) -->
	{ llds__rval_type(Rval, ActualType) },
	( { types_match(DesiredType, ActualType) } ->
		% no casting needed
		output_rval(Rval)
	; { Rval = unop(cast_to_unsigned, _) } ->
		% cast_to_unsigned overrides the ordinary type
		% XXX this is a bit of a hack; we should probably
		% eliminate cast_to_unsigned and instead use
		% special unsigned operators
		output_rval(Rval)
	;
		% We need to convert to the right type first.
		% Convertions to/from float must be treated specially;
		% for the others, we can just use a cast.
		( { DesiredType = float } ->
			io__write_string("word_to_float("),
			output_rval(Rval),
			io__write_string(")")
		; { ActualType = float } ->
			( { DesiredType = word } ->
				output_float_rval_as_word(Rval)
			; { DesiredType = data_ptr } ->
				output_float_rval_as_data_ptr(Rval)
			;
				{ error("output_rval_as_type: type error") }
			)
		;
			% cast value to desired type
			io__write_string("("),
			output_llds_type(DesiredType),
			io__write_string(") "),
			output_rval(Rval)
		)
	).

	
	% types_match(DesiredType, ActualType) is true iff
	% a value of type ActualType can be used as a value of
	% type DesiredType without casting.
	%
:- pred types_match(llds_type, llds_type).
:- mode types_match(in, in) is semidet.

types_match(Type, Type).
types_match(word, unsigned).
types_match(word, integer).
types_match(word, bool).
types_match(bool, integer).
types_match(bool, unsigned).
types_match(bool, word).
types_match(integer, bool).

	% output a float rval, converted to type `const Word *'
	%
:- pred output_float_rval_as_data_ptr(rval, io__state, io__state).
:- mode output_float_rval_as_data_ptr(in, di, uo) is det.

output_float_rval_as_data_ptr(Rval) -->
	%
	% for float constant expressions, if we're using boxed
	% boxed floats and --static-ground-terms is enabled,
	% we just refer to the static const which we declared
	% earlier
	%
	globals__io_lookup_bool_option(unboxed_float, UnboxFloat),
	globals__io_lookup_bool_option(static_ground_terms, StaticGroundTerms),
	(
		{ UnboxFloat = no, StaticGroundTerms = yes },
		{ llds_out__float_const_expr_name(Rval, FloatName) }
	->
		io__write_string("(const Word *) &mercury_float_const_"),
		io__write_string(FloatName)
	;
		io__write_string("(const Word *) float_to_word("),
		output_rval(Rval),
		io__write_string(")")
	).

	% output a float rval, converted to type `Word'
	%
:- pred output_float_rval_as_word(rval, io__state, io__state).
:- mode output_float_rval_as_word(in, di, uo) is det.

output_float_rval_as_word(Rval) -->
	%
	% for float constant expressions, if we're using boxed
	% boxed floats and --static-ground-terms is enabled,
	% we just refer to the static const which we declared
	% earlier
	%
	globals__io_lookup_bool_option(unboxed_float, UnboxFloat),
	globals__io_lookup_bool_option(static_ground_terms, StaticGroundTerms),
	(
		{ UnboxFloat = no, StaticGroundTerms = yes },
		{ llds_out__float_const_expr_name(Rval, FloatName) }
	->
		io__write_string("(Word) &mercury_float_const_"),
		io__write_string(FloatName)
	;
		io__write_string("float_to_word("),
		output_rval(Rval),
		io__write_string(")")
	).

	% output an rval (not converted to any particular type,
	% but instead output as its "natural" type)
	%
:- pred output_rval(rval, io__state, io__state).
:- mode output_rval(in, di, uo) is det.

output_rval(const(Const)) -->
	output_rval_const(Const).
output_rval(unop(UnaryOp, Exprn)) -->
	output_unary_op(UnaryOp),
	io__write_string("("),
	{ llds__unop_arg_type(UnaryOp, ArgType) },
	output_rval_as_type(Exprn, ArgType),
	io__write_string(")").
output_rval(binop(Op, X, Y)) -->
	(
		{ Op = array_index }
	->
		io__write_string("("),
		output_rval_as_type(X, data_ptr),
		io__write_string(")["),
		output_rval_as_type(Y, integer),
		io__write_string("]")
	;
		{ llds_out__string_op(Op, OpStr) }
	->
		io__write_string("(strcmp((char *)"),
		output_rval_as_type(X, word),
		io__write_string(", (char *)"),
		output_rval_as_type(Y, word),
		io__write_string(")"),
		io__write_string(" "),
		io__write_string(OpStr),
		io__write_string(" "),
		io__write_string("0)")
	;
		( { llds_out__float_compare_op(Op, OpStr1) } ->
			{ OpStr = OpStr1 }
		; { llds_out__float_op(Op, OpStr2) } ->
			{ OpStr = OpStr2 }
		;
			{ fail }
		)
	->
		io__write_string("("),
		output_rval_as_type(X, float),
		io__write_string(" "),
		io__write_string(OpStr),
		io__write_string(" "),
		output_rval_as_type(Y, float),
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
		output_rval(X),
		io__write_string(" "),
		output_binary_op(NewOp),
		io__write_string(" "),
		output_rval(NewY),
		io__write_string(")")
	;
******/
		% special-case equality ops to avoid some unnecessary
		% casts -- there's no difference between signed and
		% unsigned equality, so if both args are unsigned, we
		% don't need to cast them to (Integer)
		{ Op = eq ; Op = ne },
		{ llds__rval_type(X, XType) },
		{ XType = word ; XType = unsigned },
		{ llds__rval_type(Y, YType) },
		{ YType = word ; YType = unsigned }
	->
		io__write_string("("),
		output_rval(X),
		io__write_string(" "),
		output_binary_op(Op),
		io__write_string(" "),
		output_rval(Y),
		io__write_string(")")
	;
		io__write_string("("),
		output_rval_as_type(X, integer),
		io__write_string(" "),
		output_binary_op(Op),
		io__write_string(" "),
		output_rval_as_type(Y, integer),
		io__write_string(")")
	).
output_rval(mkword(Tag, Exprn)) -->
	% XXX we should change the definition of mkword()
	% so that this cast is not needed
	io__write_string("(const Word *) mkword("),
	output_tag(Tag),
	io__write_string(", "),
	output_rval_as_type(Exprn, word),
	io__write_string(")").
output_rval(lval(Lval)) -->
	% if a field is used as an rval, then we need to use
	% the const_field() macro, not the field() macro,
	% to avoid warnings about discarding const.
	( { Lval = field(Tag, Rval, FieldNum) } ->
		io__write_string("const_field("),
		output_tag(Tag),
		io__write_string(", "),
		output_rval(Rval),
		io__write_string(", "),
		output_rval(FieldNum),
		io__write_string(")")
	;
		output_lval(Lval)
	).
output_rval(create(Tag, _Args, _Unique, CellNum)) -->
		% emit a reference to the static constant which we
		% declared in output_rval_decls.
	% XXX we should change the definition of mkword()
	% so that this cast is not needed
	io__write_string("(const Word *) mkword(mktag("),
	io__write_int(Tag),
	io__write_string("), "),
	io__write_string("&mercury_const_"),
	io__write_int(CellNum),
	io__write_string(")").
output_rval(var(_)) -->
	{ error("Cannot output a var(_) expression in code") }.
output_rval(mem_addr(MemRef)) -->
	(
		{ MemRef = stackvar_ref(N) },
		io__write_string("(const Word *) &detstackvar("),
		io__write_int(N),
		io__write_string(")")
	;
		{ MemRef = framevar_ref(N) },
		io__write_string("(const Word *) &framevar("),
		io__write_int(N),
		io__write_string(")")
	;
		{ MemRef = heap_ref(Rval, Tag, FieldNum) },
		io__write_string("(const Word *) &field("),
		output_tag(Tag),
		io__write_string(", "),
		output_rval(Rval),
		io__write_string(", "),
		io__write_int(FieldNum),
		io__write_string(")")
	).

:- pred output_unary_op(unary_op, io__state, io__state).
:- mode output_unary_op(in, di, uo) is det.

output_unary_op(mktag) -->
	io__write_string("mktag").
output_unary_op(tag) -->
	io__write_string("tag").
output_unary_op(unmktag) -->
	io__write_string("unmktag").
output_unary_op(mkbody) -->
	io__write_string("mkbody").
output_unary_op(body) -->
	io__write_string("body").
output_unary_op(unmkbody) -->
	io__write_string("unmkbody").
output_unary_op(hash_string) -->
	io__write_string("hash_string").
output_unary_op(bitwise_complement) -->
	io__write_string("~").
output_unary_op(not) -->
	io__write_string("!").
output_unary_op(cast_to_unsigned) -->
	io__write_string("(Unsigned)").

:- pred llds_out__string_op(binary_op, string).
:- mode llds_out__string_op(in, out) is semidet.

llds_out__string_op(str_eq, "==").
llds_out__string_op(str_ne, "!=").
llds_out__string_op(str_le, "<=").
llds_out__string_op(str_ge, ">=").
llds_out__string_op(str_lt, "<").
llds_out__string_op(str_gt, ">").

:- pred llds_out__float_op(binary_op, string).
:- mode llds_out__float_op(in, out) is semidet.

llds_out__float_op(float_plus, "+").
llds_out__float_op(float_minus, "-").
llds_out__float_op(float_times, "*").
llds_out__float_op(float_divide, "/").

:- pred llds_out__float_compare_op(binary_op, string).
:- mode llds_out__float_compare_op(in, out) is semidet.

llds_out__float_compare_op(float_eq, "==").
llds_out__float_compare_op(float_ne, "!=").
llds_out__float_compare_op(float_le, "<=").
llds_out__float_compare_op(float_ge, ">=").
llds_out__float_compare_op(float_lt, "<").
llds_out__float_compare_op(float_gt, ">").

:- pred output_rval_const(rval_const, io__state, io__state).
:- mode output_rval_const(in, di, uo) is det.

output_rval_const(int_const(N)) -->
	% we need to cast to (Integer) to ensure
	% things like 1 << 32 work when `Integer' is 64 bits
	% but `int' is 32 bits.
	io__write_string("(Integer) "),
	io__write_int(N).
output_rval_const(float_const(FloatVal)) -->
	% the cast to (Float) here lets the C compiler
	% do arithmetic in `float' rather than `double'
	% if `Float' is `float' not `double'.
	io__write_string("(Float) "),
	io__write_float(FloatVal).
output_rval_const(string_const(String)) -->
		% XXX we should change the definition of `string_const'
		% so that this cast is not necessary
	io__write_string("(const Word *) string_const("""),
	output_c_quoted_string(String),
	{ string__length(String, StringLength) },
	io__write_string(""", "),
	io__write_int(StringLength),
	io__write_string(")").
output_rval_const(true) -->
	io__write_string("TRUE").
output_rval_const(false) -->
	io__write_string("FALSE").
output_rval_const(code_addr_const(CodeAddress)) -->
	output_code_addr(CodeAddress).
output_rval_const(data_addr_const(data_addr(BaseName, VarName))) -->
	% data addresses are all assumed to be of type `Word *';
	% we need to cast them here to avoid type errors
	io__write_string("(const Word *) &"),
	output_data_addr(BaseName, VarName).

:- pred output_lval_as_word(lval, io__state, io__state).
:- mode output_lval_as_word(in, di, uo) is det.

output_lval_as_word(Lval) -->
	{ llds__lval_type(Lval, ActualType) },
	( { types_match(word, ActualType) } ->
		output_lval(Lval)
	; { ActualType = float } ->
		% sanity check -- if this happens, the llds is ill-typed
		{ error("output_lval_as_word: got float") }
	;
		io__write_string("LVALUE_CAST(Word,"),
		output_lval(Lval),
		io__write_string(")")
	).

:- pred output_lval(lval, io__state, io__state).
:- mode output_lval(in, di, uo) is det.

output_lval(reg(Type, Num)) -->
	output_reg(Type, Num).
output_lval(stackvar(N)) -->
	{ (N < 0) ->
		error("stack var out of range")
	;
		true
	},
	io__write_string("detstackvar("),
	io__write_int(N),
	io__write_string(")").
output_lval(framevar(N)) -->
	{ (N < 0) ->
		error("frame var out of range")
	;
		true
	},
	io__write_string("framevar("),
	io__write_int(N),
	io__write_string(")").
output_lval(succip) -->
	io__write_string("succip").
output_lval(sp) -->
	io__write_string("sp").
output_lval(hp) -->
	io__write_string("hp").
output_lval(maxfr) -->
	io__write_string("maxfr").
output_lval(curfr) -->
	io__write_string("curfr").
output_lval(succfr(Rval)) -->
	io__write_string("bt_succfr("),
	output_rval(Rval),
	io__write_string(")").
output_lval(prevfr(Rval)) -->
	io__write_string("bt_prevfr("),
	output_rval(Rval),
	io__write_string(")").
output_lval(redoip(Rval)) -->
	io__write_string("bt_redoip("),
	output_rval(Rval),
	io__write_string(")").
output_lval(succip(Rval)) -->
	io__write_string("bt_succip("),
	output_rval(Rval),
	io__write_string(")").
output_lval(field(Tag, Rval, FieldNum)) -->
	io__write_string("field("),
	output_tag(Tag),
	io__write_string(", "),
	output_rval(Rval),
	io__write_string(", "),
	output_rval(FieldNum),
	io__write_string(")").
output_lval(lvar(_)) -->
	{ error("Illegal to output an lvar") }.
output_lval(temp(Type, Num)) -->
	(
		{ Type = r },
		io__write_string("tempr"),
		io__write_int(Num)
	;
		{ Type = f },
		io__write_string("tempf"),
		io__write_int(Num)
	).
output_lval(mem_ref(Rval)) -->
	io__write_string("XXX("),
	output_rval(Rval),
	io__write_string(")").

%-----------------------------------------------------------------------------%

output_c_quoted_string(S0) -->
	( { string__first_char(S0, Char, S1) } ->
		( { quote_c_char(Char, QuoteChar) } ->
			io__write_char('\\'),
			io__write_char(QuoteChar)
		;
			io__write_char(Char)
		),
		output_c_quoted_string(S1)
	;
		[]
	).

:- pred quote_c_char(char, char).
:- mode quote_c_char(in, out) is semidet.

quote_c_char('"', '"').
quote_c_char('\\', '\\').
quote_c_char('\n', 'n').
quote_c_char('\t', 't').
quote_c_char('\b', 'b').

%-----------------------------------------------------------------------------%

:- pred output_binary_op(binary_op, io__state, io__state).
:- mode output_binary_op(in, di, uo) is det.

output_binary_op(Op) -->
	{ llds_out__binary_op_to_string(Op, String) },
	io__write_string(String).

llds_out__binary_op_to_string(+, "+").
llds_out__binary_op_to_string(-, "-").
llds_out__binary_op_to_string(*, "*").
llds_out__binary_op_to_string(/, "/").
llds_out__binary_op_to_string(<<, "<<").
llds_out__binary_op_to_string(>>, ">>").
llds_out__binary_op_to_string(&, "&").
llds_out__binary_op_to_string('|', "|").
llds_out__binary_op_to_string(^, "^").
llds_out__binary_op_to_string(mod, "%").
llds_out__binary_op_to_string(eq, "==").
llds_out__binary_op_to_string(ne, "!=").
llds_out__binary_op_to_string(and, "&&").
llds_out__binary_op_to_string(or, "||").
llds_out__binary_op_to_string(<, "<").
llds_out__binary_op_to_string(>, ">").
llds_out__binary_op_to_string(<=, "<=").
llds_out__binary_op_to_string(>=, ">=").
	% The following is just for debugging purposes -
	% string operators are not output as `str_eq', etc.
llds_out__binary_op_to_string(array_index, "array_index").
llds_out__binary_op_to_string(str_eq, "str_eq").
llds_out__binary_op_to_string(str_ne, "str_ne").
llds_out__binary_op_to_string(str_lt, "str_lt").
llds_out__binary_op_to_string(str_gt, "str_gt").
llds_out__binary_op_to_string(str_le, "str_le").
llds_out__binary_op_to_string(str_ge, "str_ge").
llds_out__binary_op_to_string(float_eq, "float_eq").
llds_out__binary_op_to_string(float_ne, "float_ne").
llds_out__binary_op_to_string(float_lt, "float_lt").
llds_out__binary_op_to_string(float_gt, "float_gt").
llds_out__binary_op_to_string(float_le, "float_le").
llds_out__binary_op_to_string(float_ge, "float_ge").
llds_out__binary_op_to_string(float_plus, "float_plus").
llds_out__binary_op_to_string(float_minus, "float_minus").
llds_out__binary_op_to_string(float_times, "float_times").
llds_out__binary_op_to_string(float_divide, "float_divide").

%-----------------------------------------------------------------------------%

llds_out__lval_to_string(framevar(N), Description) :-
	string__int_to_string(N, N_String),
	string__append("framevar(", N_String, Tmp),
	string__append(Tmp, ")", Description).
llds_out__lval_to_string(stackvar(N), Description) :-
	string__int_to_string(N, N_String),
	string__append("stackvar(", N_String, Tmp),
	string__append(Tmp, ")", Description).
llds_out__lval_to_string(reg(RegType, RegNum), Description) :-
	llds_out__reg_to_string(RegType, RegNum, Reg_String),
	string__append("reg(", Reg_String, Tmp),
	string__append(Tmp, ")", Description).

llds_out__reg_to_string(r, N, Description) :-
	( N > 32 ->
		Template = "r(%d)"
	;
		Template = "r%d"
	),
	string__format(Template, [i(N)], Description).
llds_out__reg_to_string(f, N, Description) :-
	string__int_to_string(N, N_String),
	string__append("f(", N_String, Tmp),
	string__append(Tmp, ")", Description).

%-----------------------------------------------------------------------------%

	% Convert a Mercury predicate name into something that can form
	% part of a C identifier.  This predicate is necessary because
	% quoted names such as 'name with embedded spaces' are valid
	% predicate names in Mercury.

llds_out__name_mangle(Name, MangledName) :-
	(
		string__is_alnum_or_underscore(Name)
	->
		% any names that start with `f_' are changed so that
		% they start with `f__', so that we can use names starting
		% with `f_' (followed by anything except an underscore)
		% without fear of name collisions
		(
			string__append("f_", Suffix, Name)
		->
			string__append("f__", Suffix, MangledName)
		;
			MangledName = Name
		)
	;
		llds_out__convert_to_valid_c_identifier(Name, MangledName)
	).

:- pred llds_out__convert_to_valid_c_identifier(string, string).
:- mode llds_out__convert_to_valid_c_identifier(in, out) is det.

llds_out__convert_to_valid_c_identifier(String, Name) :-	
	(
		llds_out__name_conversion_table(String, Name0)
	->
		Name = Name0
	;
		llds_out__convert_to_valid_c_identifier_2(String, Name0),
		string__append("f", Name0, Name)
	).

llds_out__maybe_qualify_name(Module0, Name0, Name) :-
	string__append(Module0, "__", UnderscoresModule),
	( string__append(UnderscoresModule, _, Name0) ->
		Name = Name0
	;
		string__append(UnderscoresModule, Name0, Name)
	).

	% A table used to convert Mercury functors into
	% C identifiers.  Feel free to add any new translations you want.
	% The C identifiers should start with "f_",
	% to avoid introducing name clashes.
	% If the functor name is not found in the table, then
	% we use a fall-back method which produces ugly names.

:- pred llds_out__name_conversion_table(string, string).
:- mode llds_out__name_conversion_table(in, out) is semidet.

llds_out__name_conversion_table("\\=", "f_not_equal").
llds_out__name_conversion_table(">=", "f_greater_or_equal").
llds_out__name_conversion_table("=<", "f_less_or_equal").
llds_out__name_conversion_table("=", "f_equal").
llds_out__name_conversion_table("<", "f_less_than").
llds_out__name_conversion_table(">", "f_greater_than").
llds_out__name_conversion_table("-", "f_minus").
llds_out__name_conversion_table("+", "f_plus").
llds_out__name_conversion_table("*", "f_times").
llds_out__name_conversion_table("/", "f_slash").
llds_out__name_conversion_table(",", "f_comma").
llds_out__name_conversion_table(";", "f_semicolon").
llds_out__name_conversion_table("!", "f_cut").

	% This is the fall-back method.
	% Given a string, produce a C identifier
	% for that string by concatenating the decimal
	% expansions of the character codes in the string,
	% separated by underlines.
	% The C identifier will start with "f_"; this predicate
	% constructs everything except the initial "f".
	%
	% For example, given the input "\n\t" we return "_10_8".

:- pred llds_out__convert_to_valid_c_identifier_2(string, string).
:- mode llds_out__convert_to_valid_c_identifier_2(in, out) is det.

llds_out__convert_to_valid_c_identifier_2(String, Name) :-	
	(
		string__first_char(String, Char, Rest)
	->
		char__to_int(Char, Code),
		string__int_to_string(Code, CodeString),
		string__append("_", CodeString, ThisCharString),
		llds_out__convert_to_valid_c_identifier_2(Rest, Name0),
		string__append(ThisCharString, Name0, Name)
	;
		% String is the empty string
		Name = String
	).

%-----------------------------------------------------------------------------%

llds_out__make_base_type_name(BaseData, TypeName0, TypeArity, Str) :-
	(
		BaseData = info,
		BaseString = "info"
	;
		BaseData = layout,
		BaseString = "layout"
	;
		BaseData = functors,
		BaseString = "functors"
	),
	llds_out__name_mangle(TypeName0, TypeName),
	string__int_to_string(TypeArity, A_str),
        string__append_list(["base_type_", BaseString, "_", TypeName, "_", 
		A_str], Str).


%-----------------------------------------------------------------------------%

:- pred gather_c_file_labels(list(c_module), list(label)).
:- mode gather_c_file_labels(in, out) is det.

gather_c_file_labels(Modules, Labels) :-
	gather_labels_from_c_modules(Modules, [], Labels1),
	list__reverse(Labels1, Labels).

:- pred gather_c_module_labels(list(c_procedure), list(label)).
:- mode gather_c_module_labels(in, out) is det.

gather_c_module_labels(Procs, Labels) :-
	gather_labels_from_c_procs(Procs, [], Labels1),
	list__reverse(Labels1, Labels).

:- pred gather_labels_from_c_modules(list(c_module), list(label), list(label)).
:- mode gather_labels_from_c_modules(in, in, out) is det.

gather_labels_from_c_modules([], Labels, Labels).
gather_labels_from_c_modules([Module | Modules], Labels0, Labels) :-
	gather_labels_from_c_module(Module, Labels0, Labels1),
	gather_labels_from_c_modules(Modules, Labels1, Labels).

:- pred gather_labels_from_c_module(c_module, list(label), list(label)).
:- mode gather_labels_from_c_module(in, in, out) is det.

gather_labels_from_c_module(c_module(_, Procs), Labels0, Labels) :-
	gather_labels_from_c_procs(Procs, Labels0, Labels).
gather_labels_from_c_module(c_data(_, _, _, _, _), Labels, Labels).
gather_labels_from_c_module(c_code(_, _), Labels, Labels).
gather_labels_from_c_module(c_export(_), Labels, Labels).

:- pred gather_labels_from_c_procs(list(c_procedure), list(label), list(label)).
:- mode gather_labels_from_c_procs(in, in, out) is det.

gather_labels_from_c_procs([], Labels, Labels).
gather_labels_from_c_procs([c_procedure(_, _, _, Instrs) | Procs],
		Labels0, Labels) :-
	gather_labels_from_instrs(Instrs, Labels0, Labels1),
	gather_labels_from_c_procs(Procs, Labels1, Labels).

:- pred gather_labels_from_instrs(list(instruction), list(label), list(label)).
:- mode gather_labels_from_instrs(in, in, out) is det.

gather_labels_from_instrs([], Labels, Labels).
gather_labels_from_instrs([Instr | Instrs], Labels0, Labels) :-
	( Instr = label(Label) - _ ->
		Labels1 = [Label | Labels0]
	;
		Labels1 = Labels0
	),
	gather_labels_from_instrs(Instrs, Labels1, Labels).

%-----------------------------------------------------------------------------%
