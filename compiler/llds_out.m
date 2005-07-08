%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% LLDS - The Low-Level Data Structure.

% This module defines the routines for printing out LLDS,
% the Low Level Data Structure.

% Main authors: conway, fjh, zs.

%-----------------------------------------------------------------------------%

:- module ll_backend__llds_out.

:- interface.

:- import_module aditi_backend__rl_file.
:- import_module backend_libs__builtin_ops.
:- import_module hlds__hlds_module.
:- import_module libs__globals.
:- import_module ll_backend__llds.
:- import_module mdbcomp__prim_data.

:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module map.
:- import_module std_util.

	% Given a 'c_file' structure, output the LLDS code inside it
	% into one or more .c files, depending on the setting of the
	% --split-c-files option. The second argument gives the set of
	% labels that have layout structures. The third gives the Aditi-RL
	% code for the module.

:- pred output_llds(c_file::in, list(complexity_proc_info)::in,
	map(label, data_addr)::in, maybe(rl_file)::in, io::di, io::uo) is det.

	% output_rval_decls(Rval, DeclSet0, DeclSet) outputs the declarations
	% of any static constants, etc. that need to be declared before
	% output_rval(Rval) is called.

:- pred output_rval_decls(rval::in, decl_set::in, decl_set::out,
	io::di, io::uo) is det.

	% output an rval (not converted to any particular type,
	% but instead output as its "natural" type)

:- pred output_rval(rval::in, io::di, io::uo) is det.

	% output_code_addr_decls(CodeAddr, ...) outputs the declarations of any
	% extern symbols, etc. that need to be declared before
	% output_code_addr(CodeAddr) is called.

:- pred output_code_addr_decls(code_addr::in, decl_set::in, decl_set::out,
	io::di, io::uo) is det.

:- pred output_code_addr(code_addr::in, io::di, io::uo) is det.

	% output_data_addr_decls(DataAddr, ...) outputs the declarations of
	% any static constants, etc. that need to be declared before
	% output_data_addr(DataAddr) is called.

:- pred output_data_addr_decls(data_addr::in, string::in, string::in,
	int::in, int::out, decl_set::in, decl_set::out, io::di, io::uo) is det.

:- pred output_data_addr_decls(data_addr::in, decl_set::in, decl_set::out,
	io::di, io::uo) is det.

:- pred output_data_addrs_decls(list(data_addr)::in, string::in, string::in,
	int::in, int::out, decl_set::in, decl_set::out, io::di, io::uo) is det.

:- pred output_data_addr(data_addr::in, io::di, io::uo) is det.

	% c_data_linkage_string(Globals, DefaultLinkage, StaticEvenIfSplit,
	%	BeingDefined):
	% Return a C string that gives the storage class appropriate for the
	% definition of a global variable with the specified properties.

:- func c_data_linkage_string(globals, linkage, bool, bool) = string.

	% Given a boolean that states whether a data item includes code
	% addresses or not, return a C string that gives its "const-ness".

:- pred c_data_const_string(globals::in, bool::in, string::out) is det.

	% Convert an lval to a string description of that lval.

:- pred llds_out__lval_to_string(lval::in, string::out) is semidet.

	% Convert a register to a string description of that register.

:- pred llds_out__reg_to_string(reg_type::in, int::in, string::out) is det.

	% Convert a binary operator to a string description of that operator.

:- pred llds_out__binary_op_to_string(binary_op::in, string::out) is det.

	% Output an instruction and (if the third arg is yes) the comment.
	% This predicate is provided for debugging use only.

:- pred output_instruction_and_comment(instr::in, string::in, bool::in,
	io::di, io::uo) is det.

	% Output an instruction.
	% This predicate is provided for debugging use only.

:- pred output_instruction(instr::in, io::di, io::uo) is det.

	% Output a label (used by garbage collection).

:- pred output_label(label::in, io::di, io::uo) is det.

	% Output a label without the standard mercury__ prefix.

:- pred output_label(label::in, bool::in, io::di, io::uo) is det.

	% Convert a label to a C string. The boolean controls whether
	% a prefix ("mercury__") is added to the string.

:- func llds_out__label_to_c_string(label, bool) = string.

	% The following are exported to rtti_out. It may be worthwhile
	% to put these in a new module (maybe llds_out_util).

:- type decl_id
	--->	common_type(module_name, int)
	;	float_label(string)
	;	code_addr(code_addr)
	;	data_addr(data_addr)
	;	pragma_c_struct(string)
	;	type_info_like_struct(int)
	;	typeclass_constraint_struct(int).

:- type decl_set.

% Every time we emit a declaration for a symbol, we insert it into the
% set of symbols we've already declared.  That way, we avoid generating
% the same symbol twice, which would cause an error in the C code.

:- pred decl_set_init(decl_set::out) is det.

:- pred decl_set_insert(decl_id::in, decl_set::in, decl_set::out) is det.

:- pred decl_set_is_member(decl_id::in, decl_set::in) is semidet.

%-----------------------------------------------------------------------------%

%
% Note that we need to know the linkage not just at the definition,
% but also at every use, because if the use is prior to the definition,
% then we need to declare the name first, and the linkage used in that
% declaration must be consistent with the linkage in the definition.
% For this reason, the field in c_data (which holds the information about
% the definition) which says whether or not a data name is exported
% is not useful.  Instead, we need to determine whether or not something
% is exported from its `data_name'.
%

:- type linkage ---> extern ; static.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs__c_util.
:- import_module backend_libs__compile_target_code.
:- import_module backend_libs__export.
:- import_module backend_libs__foreign.
:- import_module backend_libs__name_mangle.
:- import_module backend_libs__proc_label.
:- import_module backend_libs__rtti.
:- import_module check_hlds__type_util.
:- import_module hlds__hlds_pred.
:- import_module hlds__passes_aux.
:- import_module libs__options.
:- import_module libs__trace_params.
:- import_module ll_backend__exprn_aux.
:- import_module ll_backend__layout.
:- import_module ll_backend__layout_out.
:- import_module ll_backend__pragma_c_gen.
:- import_module ll_backend__rtti_out.
:- import_module parse_tree__mercury_to_mercury.
:- import_module parse_tree__modules.
:- import_module parse_tree__prog_data.
:- import_module parse_tree__prog_foreign.
:- import_module parse_tree__prog_out.
:- import_module parse_tree__prog_util.

:- import_module assoc_list.
:- import_module bintree_set.
:- import_module char.
:- import_module dir.
:- import_module int.
:- import_module library.	% for the version number.
:- import_module multi_map.
:- import_module require.
:- import_module set.
:- import_module set_tree234.
:- import_module std_util.
:- import_module string.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

:- type decl_set ==	set_tree234(decl_id).

decl_set_init(DeclSet) :-
	DeclSet = set_tree234__init.

decl_set_insert(DeclId, DeclSet0, DeclSet) :-
	set_tree234__insert(DeclId, DeclSet0, DeclSet).

decl_set_is_member(DeclId, DeclSet) :-
	set_tree234__contains(DeclSet, DeclId).

%-----------------------------------------------------------------------------%

output_llds(C_File, ComplexityProcs, StackLayoutLabels, MaybeRLFile, !IO) :-
	globals__io_lookup_bool_option(split_c_files, SplitFiles, !IO),
	(
		SplitFiles = yes,
		C_File = c_file(ModuleName, C_HeaderInfo,
			UserForeignCodes, Exports, Vars, Datas, Modules),
		module_name_to_file_name(ModuleName, ".dir", yes, ObjDirName,
			!IO),
		dir__make_directory(ObjDirName, _, !IO),

		output_split_c_file_init(ModuleName, Modules, Datas, Vars,
			ComplexityProcs, StackLayoutLabels, MaybeRLFile, !IO),
		output_split_user_foreign_codes(UserForeignCodes, ModuleName,
			C_HeaderInfo, ComplexityProcs, StackLayoutLabels,
			1, Num1, !IO),
		output_split_c_exports(Exports, ModuleName,
			C_HeaderInfo, ComplexityProcs, StackLayoutLabels,
			Num1, Num2, !IO),
		output_split_comp_gen_c_vars(Vars, ModuleName,
			C_HeaderInfo, ComplexityProcs, StackLayoutLabels,
			Num2, Num3, !IO),
		output_split_comp_gen_c_datas(Datas, ModuleName,
			C_HeaderInfo, ComplexityProcs, StackLayoutLabels,
			Num3, Num4, !IO),
		output_split_comp_gen_c_modules(Modules, ModuleName,
			C_HeaderInfo, ComplexityProcs, StackLayoutLabels,
			Num4, Num, !IO),

		compile_target_code__write_num_split_c_files(ModuleName,
			Num, Succeeded, !IO),
		( Succeeded = no ->
			compile_target_code__remove_split_c_output_files(
				ModuleName, Num, !IO)
		;
			true
		)
	;
		SplitFiles = no,
		output_single_c_file(C_File, no, ComplexityProcs,
			StackLayoutLabels, MaybeRLFile, !IO)
	).

:- pred output_split_user_foreign_codes(list(user_foreign_code)::in,
	module_name::in, list(foreign_decl_code)::in,
	list(complexity_proc_info)::in, map(label, data_addr)::in,
	int::in, int::out, io::di, io::uo) is det.

output_split_user_foreign_codes([], _, _, _, _, !Num, !IO).
output_split_user_foreign_codes([UserForeignCode | UserForeignCodes],
		ModuleName, C_HeaderLines, ComplexityProcs, StackLayoutLabels,
		!Num, !IO) :-
	CFile = c_file(ModuleName, C_HeaderLines, [UserForeignCode],
		[], [], [], []),
	output_single_c_file(CFile, yes(!.Num), ComplexityProcs,
		StackLayoutLabels, no, !IO),
	!:Num = !.Num + 1,
	output_split_user_foreign_codes(UserForeignCodes, ModuleName,
		C_HeaderLines, ComplexityProcs, StackLayoutLabels, !Num, !IO).

:- pred output_split_c_exports(list(foreign_export)::in, module_name::in,
	list(foreign_decl_code)::in, list(complexity_proc_info)::in,
	map(label, data_addr)::in, int::in, int::out, io::di, io::uo) is det.

output_split_c_exports([], _, _, _, _, !Num, !IO).
output_split_c_exports([Export | Exports], ModuleName, C_HeaderLines,
		ComplexityProcs, StackLayoutLabels, !Num, !IO) :-
	CFile = c_file(ModuleName, C_HeaderLines, [], [Export], [], [], []),
	output_single_c_file(CFile, yes(!.Num), ComplexityProcs,
		StackLayoutLabels, no, !IO),
	!:Num = !.Num + 1,
	output_split_c_exports(Exports, ModuleName, C_HeaderLines,
		ComplexityProcs, StackLayoutLabels, !Num, !IO).

:- pred output_split_comp_gen_c_vars(list(comp_gen_c_var)::in,
	module_name::in, list(foreign_decl_code)::in,
	list(complexity_proc_info)::in, map(label, data_addr)::in,
	int::in, int::out, io::di, io::uo) is det.

output_split_comp_gen_c_vars([], _, _, _, _, !Num, !IO).
output_split_comp_gen_c_vars([Var | Vars], ModuleName, C_HeaderLines,
		ComplexityProcs, StackLayoutLabels, !Num, !IO) :-
	CFile = c_file(ModuleName, C_HeaderLines, [], [], [Var], [], []),
	output_single_c_file(CFile, yes(!.Num), ComplexityProcs,
		StackLayoutLabels, no, !IO),
	!:Num = !.Num + 1,
	output_split_comp_gen_c_vars(Vars, ModuleName, C_HeaderLines,
		ComplexityProcs, StackLayoutLabels, !Num, !IO).

:- pred output_split_comp_gen_c_datas(list(comp_gen_c_data)::in,
	module_name::in, list(foreign_decl_code)::in,
	list(complexity_proc_info)::in, map(label, data_addr)::in,
	int::in, int::out, io::di, io::uo) is det.

output_split_comp_gen_c_datas([], _, _, _, _, !Num, !IO).
output_split_comp_gen_c_datas([Data | Datas], ModuleName, C_HeaderLines,
		ComplexityProcs, StackLayoutLabels, !Num, !IO) :-
	CFile = c_file(ModuleName, C_HeaderLines, [], [], [], [Data], []),
	output_single_c_file(CFile, yes(!.Num), ComplexityProcs,
		StackLayoutLabels, no, !IO),
	!:Num = !.Num + 1,
	output_split_comp_gen_c_datas(Datas, ModuleName, C_HeaderLines,
		ComplexityProcs, StackLayoutLabels, !Num, !IO).

:- pred output_split_comp_gen_c_modules(list(comp_gen_c_module)::in,
	module_name::in, list(foreign_decl_code)::in,
	list(complexity_proc_info)::in, map(label, data_addr)::in,
	int::in, int::out, io::di, io::uo) is det.

output_split_comp_gen_c_modules([], _, _, _, _, !Num, !IO).
output_split_comp_gen_c_modules([Module | Modules], ModuleName, C_HeaderLines,
		ComplexityProcs, StackLayoutLabels, !Num, !IO) :-
	CFile = c_file(ModuleName, C_HeaderLines, [], [], [], [], [Module]),
	output_single_c_file(CFile, yes(!.Num), ComplexityProcs,
		StackLayoutLabels, no, !IO),
	!:Num = !.Num + 1,
	output_split_comp_gen_c_modules(Modules, ModuleName, C_HeaderLines,
		ComplexityProcs, StackLayoutLabels, !Num, !IO).

:- pred output_split_c_file_init(module_name::in, list(comp_gen_c_module)::in,
	list(comp_gen_c_data)::in, list(comp_gen_c_var)::in,
	list(complexity_proc_info)::in, map(label, data_addr)::in,
	maybe(rl_file)::in, io::di, io::uo) is det.

output_split_c_file_init(ModuleName, Modules, Datas, Vars, ComplexityProcs,
		StackLayoutLabels, MaybeRLFile, !IO) :-
	module_name_to_file_name(ModuleName, ".m", no, SourceFileName, !IO),
	module_name_to_split_c_file_name(ModuleName, 0, ".c", FileName, !IO),

	io__open_output(FileName, Result, !IO),
	(
		Result = ok(FileStream),
		library__version(Version),
		io__set_output_stream(FileStream, OutputStream, !IO),
		output_c_file_intro_and_grade(SourceFileName, Version, !IO),
		output_init_comment(ModuleName, !IO),
		output_c_file_mercury_headers(!IO),
		io__write_string("\n", !IO),
		decl_set_init(DeclSet0),
		output_c_module_init_list(ModuleName, Modules, Datas, Vars,
			ComplexityProcs, StackLayoutLabels,
			DeclSet0, _DeclSet, !IO),
		c_util__output_rl_file(ModuleName, MaybeRLFile, !IO),
		io__set_output_stream(OutputStream, _, !IO),
		io__close_output(FileStream, !IO)
	;
		Result = error(Error),
		io__progname_base("llds.m", ProgName, !IO),
		io__write_string("\n", !IO),
		io__write_string(ProgName, !IO),
		io__write_string(": can't open `", !IO),
		io__write_string(FileName, !IO),
		io__write_string("' for output:\n", !IO),
		io__write_string(io__error_message(Error), !IO),
		io__write_string("\n", !IO),
		io__set_exit_status(1, !IO)
	).

:- pred output_c_file_mercury_headers(io::di, io::uo) is det.

output_c_file_mercury_headers(!IO) :-
	globals__io_get_trace_level(TraceLevel, !IO),
	( given_trace_level_is_none(TraceLevel) = no ->
		io__write_string("#include ""mercury_imp.h""\n", !IO),
		io__write_string("#include ""mercury_trace_base.h""\n", !IO)
	;
		io__write_string("#include ""mercury_imp.h""\n", !IO)
	),
	globals__io_lookup_bool_option(profile_deep, DeepProfile, !IO),
	(
		DeepProfile = yes,
		io__write_string("#include ""mercury_deep_profiling.h""\n", !IO)
	;
		DeepProfile = no
	),
	globals__io_lookup_bool_option(generate_bytecode, GenBytecode, !IO),
	(
		GenBytecode = yes,
		io__write_string("#include ""mb_interface_stub.h""\n", !IO)
	;
		GenBytecode = no
	).

:- pred output_single_c_file(c_file::in, maybe(int)::in,
	list(complexity_proc_info)::in, map(label, data_addr)::in,
	maybe(rl_file)::in, io::di, io::uo) is det.

output_single_c_file(CFile, SplitFiles, ComplexityProcs, StackLayoutLabels,
		MaybeRLFile, !IO) :-
	CFile = c_file(ModuleName, _, _, _, _, _, _),
	( SplitFiles = yes(Num) ->
		module_name_to_split_c_file_name(ModuleName, Num, ".c",
			FileName, !IO)
	;
		module_name_to_file_name(ModuleName, ".c", yes, FileName, !IO)
	),
	io__open_output(FileName, Result, !IO),
	(
		Result = ok(FileStream),
		decl_set_init(DeclSet0),
		do_output_single_c_file(CFile, SplitFiles, ComplexityProcs,
			StackLayoutLabels, MaybeRLFile, FileStream,
			DeclSet0, _, !IO),
		io__close_output(FileStream, !IO)
	;
		Result = error(Error),
		io__progname_base("llds.m", ProgName, !IO),
		io__write_string("\n", !IO),
		io__write_string(ProgName, !IO),
		io__write_string(": can't open `", !IO),
		io__write_string(FileName, !IO),
		io__write_string("' for output:\n", !IO),
		io__write_string(io__error_message(Error), !IO),
		io__write_string("\n", !IO),
		io__set_exit_status(1, !IO)
	).

:- pred do_output_single_c_file(c_file::in, maybe(int)::in,
	list(complexity_proc_info)::in, map(label, data_addr)::in,
	maybe(rl_file)::in, io__output_stream::in,
	decl_set::in, decl_set::out, io::di, io::uo) is det.

do_output_single_c_file(CFile, SplitFiles, ComplexityProcs, StackLayoutLabels,
		MaybeRLFile, FileStream, !DeclSet, !IO) :-
	CFile = c_file(ModuleName, C_HeaderLines,
		UserForeignCode, Exports, Vars, Datas, Modules),
	library__version(Version),
	io__set_output_stream(FileStream, OutputStream, !IO),
	module_name_to_file_name(ModuleName, ".m", no, SourceFileName,
		!IO),
	output_c_file_intro_and_grade(SourceFileName, Version, !IO),
	(
		SplitFiles = yes(_)
	;
		SplitFiles = no,
		output_init_comment(ModuleName, !IO)
	),
	output_c_file_mercury_headers(!IO),

	output_foreign_header_include_lines(C_HeaderLines, !IO),
	io__write_string("\n", !IO),

	gather_c_file_labels(Modules, Labels),
	classify_comp_gen_c_data(Datas, multi_map__init, CommonMap,
		[], CommonDatas0, [], RttiDatas, [], LayoutDatas),
	multi_map__to_assoc_list(CommonMap, CommonAssocList),
	list__foldl2(output_common_decl_group, CommonAssocList, !DeclSet, !IO),
	output_rtti_data_decl_list(RttiDatas, !DeclSet, !IO),
	output_c_label_decls(StackLayoutLabels, Labels, !DeclSet, !IO),
	list__foldl2(output_comp_gen_c_var, Vars, !DeclSet, !IO),
	list__reverse(CommonDatas0, CommonDatas),
	list__foldl2(output_common_data_defn, CommonDatas, !DeclSet, !IO),
	list__foldl2(output_rtti_data_defn, RttiDatas, !DeclSet, !IO),
	order_layout_datas(LayoutDatas, OrderedLayoutDatas),
	list__foldl2(output_layout_data_defn, OrderedLayoutDatas,
		!DeclSet, !IO),

	list__foldl2(output_comp_gen_c_module(StackLayoutLabels), Modules,
		!DeclSet, !IO),
	list__foldl(output_user_foreign_code, UserForeignCode, !IO),
	list__foldl(io__write_string, Exports, !IO),

	(
		SplitFiles = yes(_)
	;
		SplitFiles = no,
		io__write_string("\n", !IO),
		output_c_module_init_list(ModuleName, Modules, Datas, Vars,
			ComplexityProcs, StackLayoutLabels, !DeclSet, !IO)
	),
	c_util__output_rl_file(ModuleName, MaybeRLFile, !IO),
	io__set_output_stream(OutputStream, _, !IO).

:- pred order_layout_datas(list(layout_data)::in, list(layout_data)::out)
	is det.

order_layout_datas(LayoutDatas0, LayoutDatas) :-
	order_layout_datas_2(LayoutDatas0, [], ProcLayouts,
		[], LabelLayouts, [], OtherLayouts),
	% list__reverse(RevProcLayouts, ProcLayouts),
	% list__reverse(RevLabelLayouts, LabelLayouts),
	% list__reverse(RevOtherLayouts, OtherLayouts),
	list__condense([ProcLayouts, LabelLayouts, OtherLayouts], LayoutDatas).

:- pred order_layout_datas_2(list(layout_data)::in,
	list(layout_data)::in, list(layout_data)::out,
	list(layout_data)::in, list(layout_data)::out,
	list(layout_data)::in, list(layout_data)::out) is det.

order_layout_datas_2([], !ProcLayouts, !LabelLayouts, !OtherLayouts).
order_layout_datas_2([Layout | Layouts], !ProcLayouts, !LabelLayouts,
		!OtherLayouts) :-
	( Layout = proc_layout_data(_, _, _) ->
		!:ProcLayouts = [Layout | !.ProcLayouts]
	; Layout = label_layout_data(_, _, _, _, _, _, _, _) ->
		!:LabelLayouts = [Layout | !.LabelLayouts]
	;
		!:OtherLayouts = [Layout | !.OtherLayouts]
	),
	order_layout_datas_2(Layouts, !ProcLayouts, !LabelLayouts,
		!OtherLayouts).

:- pred output_c_module_init_list(module_name::in, list(comp_gen_c_module)::in,
	list(comp_gen_c_data)::in, list(comp_gen_c_var)::in,
	list(complexity_proc_info)::in, map(label, data_addr)::in,
	decl_set::in, decl_set::out, io::di, io::uo) is det.

output_c_module_init_list(ModuleName, Modules, Datas, Vars, ComplexityProcs,
		StackLayoutLabels, !DeclSet, !IO) :-
	MustInit = (pred(Module::in) is semidet :-
		module_defines_label_with_layout(Module, StackLayoutLabels)
	),
	list__filter(MustInit, Modules,
		AlwaysInitModules, MaybeInitModules),
	list__chunk(AlwaysInitModules, 40, AlwaysInitModuleBunches),
	list__chunk(MaybeInitModules, 40, MaybeInitModuleBunches),
	globals__io_lookup_bool_option(split_c_files, SplitFiles, !IO),

	output_init_bunch_defs(AlwaysInitModuleBunches, ModuleName,
		"always", 0, SplitFiles, !IO),

	(
		MaybeInitModuleBunches = []
	;
		MaybeInitModuleBunches = [_ | _],
		output_init_bunch_defs(MaybeInitModuleBunches, ModuleName,
			"maybe", 0, SplitFiles, !IO)
	),

	io__write_string("/* suppress gcc -Wmissing-decls warnings */\n", !IO),
	io__write_string("void ", !IO),
	output_init_name(ModuleName, !IO),
	io__write_string("init(void);\n", !IO),

	io__write_string("void ", !IO),
	output_init_name(ModuleName, !IO),
	io__write_string("init_type_tables(void);\n", !IO),
	io__write_string("void ", !IO),
	output_init_name(ModuleName, !IO),
	io__write_string("init_debugger(void);\n", !IO),

	io__write_string("#ifdef MR_DEEP_PROFILING\n", !IO),
	io__write_string("void ", !IO),
	output_init_name(ModuleName, !IO),
	io__write_string("write_out_proc_statics(FILE *fp);\n", !IO),
	io__write_string("#endif\n", !IO),

	io__write_string("#ifdef MR_RECORD_TERM_SIZES\n", !IO),
	io__write_string("void ", !IO),
	output_init_name(ModuleName, !IO),
	io__write_string("init_complexity_procs(void);\n", !IO),
	io__write_string("#endif\n", !IO),

	globals__io_lookup_bool_option(allow_table_reset, TableReset, !IO),
	(
		TableReset = yes,
		io__write_string("void ", !IO),
		output_init_name(ModuleName, !IO),
		io__write_string("reset_tables(void);\n", !IO)
	;
		TableReset = no
	),

	io__write_string("\n", !IO),

	io__write_string("void ", !IO),
	output_init_name(ModuleName, !IO),
	io__write_string("init(void)\n", !IO),
	io__write_string("{\n", !IO),
	io__write_string("\tstatic MR_bool done = MR_FALSE;\n", !IO),
	io__write_string("\tif (done) {\n", !IO),
	io__write_string("\t\treturn;\n", !IO),
	io__write_string("\t}\n", !IO),
	io__write_string("\tdone = MR_TRUE;\n", !IO),

	output_init_bunch_calls(AlwaysInitModuleBunches, ModuleName,
		"always", 0, !IO),

	(
		MaybeInitModuleBunches = []
	;
		MaybeInitModuleBunches = [_ | _],
		output_init_bunch_calls(MaybeInitModuleBunches, ModuleName,
			"maybe", 0, !IO)
	),

	output_c_data_init_list(Datas, !IO),
		% The call to the debugger initialization function
		% is for bootstrapping; once the debugger has been modified
		% to call do_init_modules_debugger() and all debuggable
		% object files created before this change have been
		% overwritten, it can be deleted.
	io__write_string("\t", !IO),
	output_init_name(ModuleName, !IO),
	io__write_string("init_debugger();\n", !IO),
	io__write_string("}\n\n", !IO),

	io__write_string("void ", !IO),
	output_init_name(ModuleName, !IO),
	io__write_string("init_type_tables(void)\n", !IO),
	io__write_string("{\n", !IO),
	io__write_string("\tstatic MR_bool done = MR_FALSE;\n", !IO),
	io__write_string("\tif (done) {\n", !IO),
	io__write_string("\t\treturn;\n", !IO),
	io__write_string("\t}\n", !IO),
	io__write_string("\tdone = MR_TRUE;\n", !IO),
	output_type_tables_init_list(Datas, SplitFiles, !IO),
	io__write_string("}\n\n", !IO),

	output_debugger_init_list_decls(Datas, !DeclSet, !IO),
	io__write_string("\n", !IO),
	io__write_string("void ", !IO),
	output_init_name(ModuleName, !IO),
	io__write_string("init_debugger(void)\n", !IO),
	io__write_string("{\n", !IO),
	io__write_string("\tstatic MR_bool done = MR_FALSE;\n", !IO),
	io__write_string("\tif (done) {\n", !IO),
	io__write_string("\t\treturn;\n", !IO),
	io__write_string("\t}\n", !IO),
	io__write_string("\tdone = MR_TRUE;\n", !IO),
	output_debugger_init_list(Datas, !IO),
	io__write_string("}\n\n", !IO),

	io__write_string("#ifdef MR_DEEP_PROFILING\n", !IO),
	output_write_proc_static_list_decls(Datas, !DeclSet, !IO),
	io__write_string("\nvoid ", !IO),
	output_init_name(ModuleName, !IO),
	io__write_string("write_out_proc_statics(FILE *fp)\n", !IO),
	io__write_string("{\n", !IO),
	output_write_proc_static_list(Datas, !IO),
	io__write_string("}\n", !IO),
	io__write_string("\n#endif\n\n", !IO),

	io__write_string("#ifdef MR_RECORD_TERM_SIZES\n", !IO),
	output_complexity_arg_info_arrays(ComplexityProcs, !IO),
	io__write_string("\nvoid ", !IO),
	output_init_name(ModuleName, !IO),
	io__write_string("init_complexity_procs(void)\n", !IO),
	io__write_string("{\n", !IO),
	output_init_complexity_proc_list(ComplexityProcs, !IO),
	io__write_string("}\n", !IO),
	io__write_string("\n#endif\n\n", !IO),

	(
		TableReset = yes,
		io__write_string("void ", !IO),
		output_init_name(ModuleName, !IO),
		io__write_string("reset_tables(void)\n", !IO),
		io__write_string("{\n", !IO),
		list__foldl(output_init_reset_table, Vars, !IO),
		io__write_string("}\n\n", !IO)
	;
		TableReset = no
	),

	io__write_string(
		"/* ensure everything is compiled with the same grade */\n",
		!IO),
	io__write_string(
		"static const void *const MR_grade = &MR_GRADE_VAR;\n", !IO).

:- pred module_defines_label_with_layout(comp_gen_c_module::in,
	map(label, data_addr)::in) is semidet.

module_defines_label_with_layout(Module, StackLayoutLabels) :-
		% Checking whether the set is empty or not
		% allows us to avoid calling gather_c_module_labels.
	\+ map__is_empty(StackLayoutLabels),
	Module = comp_gen_c_module(_, Procedures),
	gather_c_module_labels(Procedures, Labels),
	list__member(Label, Labels),
	map__search(StackLayoutLabels, Label, _).

:- pred output_init_bunch_defs(list(list(comp_gen_c_module))::in,
	module_name::in, string::in, int::in, bool::in,
	io::di, io::uo) is det.

output_init_bunch_defs([], _, _, _, _, !IO).
output_init_bunch_defs([Bunch | Bunches], ModuleName, InitStatus, Seq,
		SplitFiles, !IO) :-
	io__write_string("static void ", !IO),
	output_bunch_name(ModuleName, InitStatus, Seq, !IO),
	io__write_string("(void)\n", !IO),
	io__write_string("{\n", !IO),
	output_init_bunch_def(Bunch, ModuleName, SplitFiles, !IO),
	io__write_string("}\n\n", !IO),
	NextSeq = Seq + 1,
	output_init_bunch_defs(Bunches, ModuleName, InitStatus, NextSeq,
		SplitFiles, !IO).

:- pred output_init_bunch_def(list(comp_gen_c_module)::in, module_name::in,
	bool::in, io::di, io::uo) is det.

output_init_bunch_def([], _, _, !IO).
output_init_bunch_def([Module | Modules], ModuleName, SplitFiles, !IO) :-
	Module = comp_gen_c_module(C_ModuleName, _),
	(
		SplitFiles = yes,
		io__write_string("\t{ extern MR_ModuleFunc ", !IO),
		io__write_string(C_ModuleName, !IO),
		io__write_string(";\n", !IO),
		io__write_string("\t  ", !IO),
		io__write_string(C_ModuleName, !IO),
		io__write_string("(); }\n", !IO)
	;
		SplitFiles = no,
		io__write_string("\t", !IO),
		io__write_string(C_ModuleName, !IO),
		io__write_string("();\n", !IO)
	),
	output_init_bunch_def(Modules, ModuleName, SplitFiles, !IO).

:- pred output_init_bunch_calls(list(list(comp_gen_c_module))::in,
	module_name::in, string::in, int::in, io::di, io::uo)
	is det.

output_init_bunch_calls([], _, _, _, !IO).
output_init_bunch_calls([_ | Bunches], ModuleName, InitStatus, Seq, !IO) :-
	io__write_string("\t", !IO),
	output_bunch_name(ModuleName, InitStatus, Seq, !IO),
	io__write_string("();\n", !IO),
	NextSeq = Seq + 1,
	output_init_bunch_calls(Bunches, ModuleName, InitStatus, NextSeq, !IO).

	% Output MR_INIT_TYPE_CTOR_INFO(TypeCtorInfo, Typector);
	% for each type_ctor_info defined in this module.

:- pred output_c_data_init_list(list(comp_gen_c_data)::in, io::di, io::uo)
	is det.

output_c_data_init_list([], !IO).
output_c_data_init_list([Data | Datas], !IO) :-
	( Data = rtti_data(RttiData) ->
		rtti_out__init_rtti_data_if_nec(RttiData, !IO)
	;
		true
	),
	output_c_data_init_list(Datas, !IO).

	% Output code to register each type_ctor_info defined in this module.

:- pred output_type_tables_init_list(list(comp_gen_c_data)::in,
	bool::in, io::di, io::uo) is det.

output_type_tables_init_list([], _, !IO).
output_type_tables_init_list([Data | Datas], SplitFiles, !IO) :-
	( Data = rtti_data(RttiData) ->
		rtti_out__register_rtti_data_if_nec(RttiData, SplitFiles, !IO)
	;
		true
	),
	output_type_tables_init_list(Datas, SplitFiles, !IO).

	% Output declarations for each module layout defined in this module
	% (there should only be one, of course).
:- pred output_debugger_init_list_decls(list(comp_gen_c_data)::in,
	decl_set::in, decl_set::out, io::di, io::uo) is det.

output_debugger_init_list_decls([], !DeclSet, !IO).
output_debugger_init_list_decls([Data | Datas], !DeclSet, !IO) :-
	(
		Data = layout_data(LayoutData),
		LayoutData = module_layout_data(ModuleName, _,_,_,_,_,_,_)
	->
		output_data_addr_decls(layout_addr(module_layout(ModuleName)),
			!DeclSet, !IO)
	;
		true
	),
	output_debugger_init_list_decls(Datas, !DeclSet, !IO).

	% Output calls to MR_register_module_layout()
	% for each module layout defined in this module
	% (there should only be one, of course).

:- pred output_debugger_init_list(list(comp_gen_c_data)::in, io::di, io::uo)
	is det.

output_debugger_init_list([], !IO).
output_debugger_init_list([Data | Datas], !IO) :-
	(
		Data = layout_data(LayoutData),
		LayoutData = module_layout_data(ModuleName, _,_,_,_,_,_,_)
	->
		io__write_string(
			"\tif (MR_register_module_layout != NULL) {\n",
			!IO),
		io__write_string("\t\t(*MR_register_module_layout)(", !IO),
		io__write_string("\n\t\t\t&", !IO),
		output_layout_name(module_layout(ModuleName), !IO),
		io__write_string(");\n\t}\n", !IO)
	;
		true
	),
	output_debugger_init_list(Datas, !IO).

:- pred output_write_proc_static_list_decls(list(comp_gen_c_data)::in,
	decl_set::in, decl_set::out, io::di, io::uo) is det.

output_write_proc_static_list_decls([], !DeclSet, !IO).
output_write_proc_static_list_decls([Data | Datas], !DeclSet, !IO) :-
	(
		Data = layout_data(LayoutData),
		LayoutData = proc_layout_data(_, _, MaybeRest),
		MaybeRest = proc_id(yes(_), _)
	->
		output_maybe_layout_data_decl(LayoutData, !DeclSet, !IO)
	;
		true
	),
	output_write_proc_static_list_decls(Datas, !DeclSet, !IO).

:- pred output_write_proc_static_list(list(comp_gen_c_data)::in,
	io::di, io::uo) is det.

output_write_proc_static_list([], !IO).
output_write_proc_static_list([Data | Datas], !IO) :-
	(
		Data = layout_data(LayoutData),
		LayoutData = proc_layout_data(RttiProcLabel, _, MaybeRest),
		MaybeRest = proc_id(yes(_), _)
	->
		ProcLabel = make_proc_label_from_rtti(RttiProcLabel),
		UserOrUCI = proc_label_user_or_uci(ProcLabel),
		Kind = proc_layout_proc_id(UserOrUCI),
		(
			UserOrUCI = user,
			io__write_string(
				"\tMR_write_out_user_proc_static(fp,\n\t\t&",
				!IO)
		;
			UserOrUCI = uci,
			io__write_string(
				"\tMR_write_out_uci_proc_static(fp,\n\t\t&",
				!IO)
		),
		output_layout_name(proc_layout(RttiProcLabel, Kind), !IO),
		io__write_string(");\n", !IO)
	;
		true
	),
	output_write_proc_static_list(Datas, !IO).

:- func complexity_arg_info_array_name(int) = string.

complexity_arg_info_array_name(ProcNum) =
	"MR_complexity_arg_info_" ++ int_to_string(ProcNum).

:- pred output_complexity_arg_info_arrays(list(complexity_proc_info)::in,
	io::di, io::uo) is det.

output_complexity_arg_info_arrays([], !IO).
output_complexity_arg_info_arrays([Info | Infos], !IO) :-
	Info = complexity_proc_info(ProcNum, _, Args),
	io__write_string("\nMR_ComplexityArgInfo ", !IO),
	io__write_string(complexity_arg_info_array_name(ProcNum), !IO),
	io__write_string("[", !IO),
	io__write_int(list__length(Args), !IO),
	io__write_string("] = {\n", !IO),
	output_complexity_arg_info_array(Args, !IO),
	io__write_string("};\n", !IO),
	output_complexity_arg_info_arrays(Infos, !IO).

:- pred output_complexity_arg_info_array(list(complexity_arg_info)::in,
	io::di, io::uo) is det.

output_complexity_arg_info_array([], !IO).
output_complexity_arg_info_array([Arg | Args], !IO) :-
	Arg = complexity_arg_info(MaybeName, Kind),
	io__write_string("{ ", !IO),
	(
		MaybeName = yes(Name),
		io__write_string("""", !IO),
		io__write_string(Name, !IO),
		io__write_string(""", ", !IO)
	;
		MaybeName = no,
		io__write_string("NULL, ", !IO)
	),
	(
		Kind = complexity_input_variable_size,
		io__write_string("MR_COMPLEXITY_INPUT_VAR_SIZE", !IO)
	;
		Kind = complexity_input_fixed_size,
		io__write_string("MR_COMPLEXITY_INPUT_FIX_SIZE", !IO)
	;
		Kind = complexity_output,
		io__write_string("MR_COMPLEXITY_OUTPUT", !IO)
	),
	io__write_string(" },\n", !IO),
	output_complexity_arg_info_array(Args, !IO).

:- pred output_init_complexity_proc_list(list(complexity_proc_info)::in,
	io::di, io::uo) is det.

output_init_complexity_proc_list([], !IO).
output_init_complexity_proc_list([Info | Infos], !IO) :-
	Info = complexity_proc_info(ProcNum, FullProcName, ArgInfos),
	io__write_string("\tMR_init_complexity_proc(", !IO),
	io__write_int(ProcNum, !IO),
	io__write_string(", """, !IO),
	c_util__output_quoted_string(FullProcName, !IO),
	io__write_string(""", ", !IO),
	list__filter(complexity_arg_is_profiled, ArgInfos, ProfiledArgInfos),
	io__write_int(list__length(ProfiledArgInfos), !IO),
	io__write_string(", ", !IO),
	io__write_int(list__length(ArgInfos), !IO),
	io__write_string(", ", !IO),
	io__write_string(complexity_arg_info_array_name(ProcNum), !IO),
	io__write_string(");\n", !IO),
	output_init_complexity_proc_list(Infos, !IO).

:- pred complexity_arg_is_profiled(complexity_arg_info::in) is semidet.

complexity_arg_is_profiled(complexity_arg_info(_, Kind)) :-
	Kind = complexity_input_variable_size.

:- pred output_init_reset_table(comp_gen_c_var::in, io::di, io::uo) is det.

output_init_reset_table(Var, !IO) :-
	Var = tabling_pointer_var(_Module, ProcLabel),
	io__write_string("\t", !IO),
	output_tabling_pointer_var_name(ProcLabel, !IO),
	io__write_string(".MR_integer = 0;\n", !IO).

	% Output a comment to tell mkinit what functions to
	% call from <module>_init.c.
:- pred output_init_comment(module_name::in, io::di, io::uo) is det.

output_init_comment(ModuleName, !IO) :-
	io__write_string("/*\n", !IO),
	io__write_string("INIT ", !IO),
	output_init_name(ModuleName, !IO),
	io__write_string("init\n", !IO),
	globals__io_lookup_bool_option(aditi, Aditi, !IO),
	(
		Aditi = yes,
		RLName = make_rl_data_name(ModuleName),
		io__write_string("ADITI_DATA ", !IO),
		io__write_string(RLName, !IO),
		io__write_string("\n", !IO)
	;
		Aditi = no
	),
	io__write_string("ENDINIT\n", !IO),
	io__write_string("*/\n\n", !IO).

:- pred output_bunch_name(module_name::in, string::in, int::in, io::di, io::uo)
	is det.

output_bunch_name(ModuleName, InitStatus, Number, !IO) :-
	io__write_string("mercury__", !IO),
	MangledModuleName = sym_name_mangle(ModuleName),
	io__write_string(MangledModuleName, !IO),
	io__write_string("_", !IO),
	io__write_string(InitStatus, !IO),
	io__write_string("_bunch_", !IO),
	io__write_int(Number, !IO).

:- pred classify_comp_gen_c_data(list(comp_gen_c_data)::in,
	multi_map(int, common_data)::in,
	multi_map(int, common_data)::out,
	list(common_data)::in, list(common_data)::out,
	list(rtti_data)::in, list(rtti_data)::out,
	list(layout_data)::in, list(layout_data)::out) is det.

classify_comp_gen_c_data([], !CommonMap, !CommonList, !RttiList, !LayoutList).
classify_comp_gen_c_data([Data | Datas], !CommonMap, !CommonList,
		!RttiList, !LayoutList) :-
	(
		Data = common_data(CommonData),
		CommonData = common_data(_ModuleName, _CellNum, TypeAndValue),
		TypeNum = common_cell_get_type_num(TypeAndValue),
		multi_map__set(!.CommonMap, TypeNum, CommonData, !:CommonMap),
		!:CommonList = [CommonData | !.CommonList]
	;
		Data = rtti_data(Rtti),
		!:RttiList = [Rtti | !.RttiList]
	;
		Data = layout_data(Layout),
		!:LayoutList = [Layout | !.LayoutList]
	),
	classify_comp_gen_c_data(Datas, !CommonMap, !CommonList,
		!RttiList, !LayoutList).

:- pred output_common_decl_group(pair(int, list(common_data))::in,
	decl_set::in, decl_set::out, io::di, io::uo) is det.

output_common_decl_group(TypeNum - CommonDatas, !DeclSet, !IO) :-
	io__write_string("\n", !IO),
	globals__io_lookup_bool_option(split_c_files, SplitFiles, !IO),
	ExportedFromFile = SplitFiles,
	(
		CommonDatas = [CommonData | _],
		CommonData = common_data(ModuleName, _, TypeAndValue)
	;
		CommonDatas = [],
		error("output_common_decl_chunk: empty list")
	),
	TypeDeclId = common_type(ModuleName, TypeNum),
	( decl_set_is_member(TypeDeclId, !.DeclSet) ->
		true
	;
		output_const_term_type(TypeAndValue, ModuleName,
			"", "", 0, _, !IO),
		io__write_string("\n", !IO),
		decl_set_insert(TypeDeclId, !DeclSet)
	),
	(
		ExportedFromFile = no,
		% There should be a macro MR_DEF_COMMON<n> for every n up to
		% ChunkSize.
		ChunkSize = 10,
		list__chunk(list__reverse(CommonDatas), ChunkSize,
			CommonDataChunks),
		list__foldl2(output_common_decl_shorthand_chunk(TypeNum),
			CommonDataChunks, !DeclSet, !IO)
	;
		ExportedFromFile = yes,
		% ChunkSize should be as large as possible to reduce the size
		% of the file being generated, but small enough not to overload
		% the fixed limits of our target C compilers.
		ChunkSize = 20,
		% The process of creating the multi_map reverses the order of
		% CommonDatas, we now undo this reversal.
		list__chunk(list__reverse(CommonDatas), ChunkSize,
			CommonDataChunks),
		list__foldl2(output_common_decl_chunk(ModuleName, TypeNum),
			CommonDataChunks, !DeclSet, !IO)
	).

:- pred output_common_decl_shorthand_chunk(int::in, list(common_data)::in,
	decl_set::in, decl_set::out, io::di, io::uo) is det.

output_common_decl_shorthand_chunk(TypeNum, CommonDatas, !DeclSet, !IO) :-
	io__write_string("MR_DEF_COMMON", !IO),
	io__write_int(list__length(CommonDatas), !IO),
	io__write_string("(", !IO),
	io__write_int(TypeNum, !IO),
	io__write_string(",", !IO),
	output_common_decl_shorthand_chunk_entries(CommonDatas, !DeclSet, !IO),
	io__write_string(")\n", !IO).

:- pred output_common_decl_shorthand_chunk_entries(list(common_data)::in,
	decl_set::in, decl_set::out, io::di, io::uo) is det.

output_common_decl_shorthand_chunk_entries([], !DeclSet, !IO) :-
	error("output_common_decl_shorthand_chunk_entries: empty list").
output_common_decl_shorthand_chunk_entries([CommonData | CommonDatas],
		!DeclSet, !IO) :-
	CommonData = common_data(ModuleName, CellNum, TypeAndValue),
	TypeNum = common_cell_get_type_num(TypeAndValue),
	VarName = common(CellNum, TypeNum),
	VarDeclId = data_addr(data_addr(ModuleName, VarName)),
	decl_set_insert(VarDeclId, !DeclSet),
	io__write_int(CellNum, !IO),
	(
		CommonDatas = [_ | _],
		io__write_string(",", !IO),
		output_common_decl_shorthand_chunk_entries(CommonDatas,
			!DeclSet, !IO)
	;
		CommonDatas = []
	).

:- pred output_common_decl_chunk(module_name::in, int::in,
	list(common_data)::in, decl_set::in, decl_set::out, io::di, io::uo)
	is det.

output_common_decl_chunk(ModuleName, TypeNum, CommonDatas, !DeclSet, !IO) :-
	io__write_string("const struct ", !IO),
	output_common_cell_type_name(ModuleName, TypeNum, !IO),
	io__nl(!IO),
	output_common_decl_chunk_entries(CommonDatas, !DeclSet, !IO).

:- pred output_common_decl_chunk_entries(list(common_data)::in,
	decl_set::in, decl_set::out, io::di, io::uo) is det.

output_common_decl_chunk_entries([], !DeclSet, !IO) :-
	error("output_common_decl_chunk_entries: empty list").
output_common_decl_chunk_entries([CommonData | CommonDatas], !DeclSet, !IO) :-
	CommonData = common_data(ModuleName, CellNum, TypeAndValue),
	TypeNum = common_cell_get_type_num(TypeAndValue),
	VarName = common(CellNum, TypeNum),
	VarDeclId = data_addr(data_addr(ModuleName, VarName)),
	output_decl_id(VarDeclId, !IO),
	decl_set_insert(VarDeclId, !DeclSet),
	(
		CommonDatas = [_ | _],
		io__write_string(",\n", !IO),
		output_common_decl_chunk_entries(CommonDatas, !DeclSet, !IO)
	;
		CommonDatas = [],
		io__write_string(";\n", !IO)
	).

	%
	% output_c_data_type_def outputs the given the type definition.
	% This is needed because some compilers need the type definition
	% to appear before any use of the type in forward declarations
	% of static constants.
	%

:- pred output_c_data_type_def(comp_gen_c_data::in,
	decl_set::in, decl_set::out, io::di, io::uo) is det.

output_c_data_type_def(common_data(CommonData), !DeclSet, !IO) :-
	output_common_data_decl(CommonData, !DeclSet, !IO).
output_c_data_type_def(rtti_data(RttiData), !DeclSet, !IO) :-
	output_rtti_data_decl(RttiData, !DeclSet, !IO).
output_c_data_type_def(layout_data(LayoutData), !DeclSet, !IO) :-
	output_maybe_layout_data_decl(LayoutData, !DeclSet, !IO).

:- pred output_common_data_decl(common_data::in,
	decl_set::in, decl_set::out, io::di, io::uo) is det.

output_common_data_decl(common_data(ModuleName, CellNum, TypeAndValue),
		!DeclSet, !IO) :-
	io__write_string("\n", !IO),

		% The code for data local to a Mercury module
		% should normally be visible only within the C file
		% generated for that module. However, if we generate
		% multiple C files, the code in each C file must be
		% visible to the other C files for that Mercury module.
	globals__io_lookup_bool_option(split_c_files, SplitFiles, !IO),
	ExportedFromFile = SplitFiles,
	TypeNum = common_cell_get_type_num(TypeAndValue),
	TypeDeclId = common_type(ModuleName, TypeNum),
	( decl_set_is_member(TypeDeclId, !.DeclSet) ->
		true
	;
		output_const_term_type(TypeAndValue, ModuleName,
			"", "", 0, _, !IO),
		io__write_string("\n", !IO),
		decl_set_insert(TypeDeclId, !DeclSet)
	),
	VarName = common(CellNum, TypeNum),
	VarDeclId = data_addr(data_addr(ModuleName, VarName)),
	output_const_term_decl_or_defn(TypeAndValue, ModuleName, CellNum,
		ExportedFromFile, no, "", "", 0, _, !IO),
	decl_set_insert(VarDeclId, !DeclSet).

:- pred output_comp_gen_c_module(map(label, data_addr)::in,
	comp_gen_c_module::in, decl_set::in, decl_set::out, io::di, io::uo)
	is det.

output_comp_gen_c_module(StackLayoutLabels,
		comp_gen_c_module(ModuleName, Procedures),
		!DeclSet, !IO) :-
	io__write_string("\n", !IO),
	list__foldl2(output_c_procedure_decls(StackLayoutLabels),
		Procedures, !DeclSet, !IO),
	io__write_string("\n", !IO),
	io__write_string("MR_BEGIN_MODULE(", !IO),
	io__write_string(ModuleName, !IO),
	io__write_string(")\n", !IO),
	gather_c_module_labels(Procedures, Labels),
	output_c_label_inits(StackLayoutLabels, Labels, !IO),
	io__write_string("MR_BEGIN_CODE\n", !IO),
	io__write_string("\n", !IO),
	globals__io_lookup_bool_option(auto_comments, PrintComments, !IO),
	globals__io_lookup_bool_option(emit_c_loops, EmitCLoops, !IO),
	list__foldl(output_c_procedure(PrintComments, EmitCLoops), Procedures,
		!IO),
	io__write_string("MR_END_MODULE\n", !IO).

:- pred output_comp_gen_c_var(comp_gen_c_var::in,
	decl_set::in, decl_set::out, io::di, io::uo) is det.

output_comp_gen_c_var(tabling_pointer_var(ModuleName, ProcLabel),
		!DeclSet, !IO) :-
	io__write_string("\nMR_TableNode ", !IO),
	output_tabling_pointer_var_name(ProcLabel, !IO),
	io__write_string(" = { 0 };\n", !IO),
	DataAddr = data_addr(ModuleName, tabling_pointer(ProcLabel)),
	decl_set_insert(data_addr(DataAddr), !DeclSet).

:- pred output_comp_gen_c_data(comp_gen_c_data::in,
	decl_set::in, decl_set::out, io::di, io::uo) is det.

output_comp_gen_c_data(common_data(CommonData), !DeclSet, !IO) :-
	output_common_data_defn(CommonData, !DeclSet, !IO).
output_comp_gen_c_data(rtti_data(RttiData), !DeclSet, !IO) :-
	output_rtti_data_defn(RttiData, !DeclSet, !IO).
output_comp_gen_c_data(layout_data(LayoutData), !DeclSet, !IO) :-
	output_layout_data_defn(LayoutData, !DeclSet, !IO).

:- pred output_common_data_defn(common_data::in,
	decl_set::in, decl_set::out, io::di, io::uo) is det.

output_common_data_defn(common_data(ModuleName, CellNum, TypeAndValue),
		!DeclSet, !IO) :-
	io__write_string("\n", !IO),
	Args = common_cell_get_rvals(TypeAndValue),
	output_rvals_decls(Args, !DeclSet, !IO),

		% The code for data local to a Mercury module
		% should normally be visible only within the C file
		% generated for that module. However, if we generate
		% multiple C files, the code in each C file must be
		% visible to the other C files for that Mercury module.
	globals__io_lookup_bool_option(split_c_files, SplitFiles, !IO),
	ExportedFromFile = SplitFiles,

	TypeNum = common_cell_get_type_num(TypeAndValue),
	VarName = common(CellNum, TypeNum),
	VarDeclId = data_addr(data_addr(ModuleName, VarName)),
	output_const_term_decl_or_defn(TypeAndValue, ModuleName, CellNum,
		ExportedFromFile, yes, "", "", 0, _, !IO),
	decl_set_insert(VarDeclId, !DeclSet).

:- pred output_user_foreign_code(user_foreign_code::in, io::di, io::uo) is det.

output_user_foreign_code(user_foreign_code(Lang, Foreign_Code, Context),
		!IO) :-
	( Lang = c ->
		globals__io_lookup_bool_option(auto_comments, PrintComments,
			!IO),
		( PrintComments = yes ->
			io__write_string("/* ", !IO),
			prog_out__write_context(Context, !IO),
			io__write_string(" pragma foreign_code */\n", !IO)
		;
			true
		),
		output_set_line_num(Context, !IO),
		io__write_string(Foreign_Code, !IO),
		io__write_string("\n", !IO),
		output_reset_line_num(!IO)
	;
		error("llds_out__output_user_foreign_code: unimplemented: " ++
			"foreign code other than C")
	).

:- pred output_foreign_header_include_lines(list(foreign_decl_code)::in,
	io::di, io::uo) is det.

output_foreign_header_include_lines(Decls, !IO) :-
	list__foldl(output_foreign_header_include_line, Decls, !IO).

:- pred output_foreign_header_include_line(foreign_decl_code::in,
	io::di, io::uo) is det.

output_foreign_header_include_line(Decl, !IO) :-
	Decl = foreign_decl_code(Lang, _IsLocal, Code, Context),
	( Lang = c ->
		globals__io_lookup_bool_option(auto_comments, PrintComments,
			!IO),
		( PrintComments = yes ->
			io__write_string("/* ", !IO),
			prog_out__write_context(Context, !IO),
			io__write_string(" pragma foreign_decl_code( ", !IO),
			io__write(Lang, !IO),
			io__write_string(" */\n", !IO)
		;
			true
		),
		output_set_line_num(Context, !IO),
		io__write_string(Code, !IO),
		io__write_string("\n", !IO),
		output_reset_line_num(!IO)
	;
		error("llds_out__output_user_foreign_code: unexpected: " ++
			"foreign code other than C")
	).

:- pred output_c_label_decls(map(label, data_addr)::in, list(label)::in,
	decl_set::in, decl_set::out, io::di, io::uo) is det.

output_c_label_decls(StackLayoutLabels, Labels, !DeclSet, !IO) :-
	group_c_labels_with_layouts(StackLayoutLabels, Labels,
		multi_map__init, DeclLLMap, multi_map__init, LocalLabels,
		[], RevAddrsToDecl, [], RevOtherLabels),
	multi_map__to_assoc_list(DeclLLMap, DeclLLList),
	list__foldl2(output_label_layout_decls, DeclLLList, !DeclSet, !IO),
	multi_map__to_assoc_list(LocalLabels, LocalLabelList),
	list__foldl2(output_local_label_decls, LocalLabelList, !DeclSet, !IO),
	list__reverse(RevAddrsToDecl, AddrsToDecl),
	list__foldl2(output_stack_layout_decl, AddrsToDecl, !DeclSet, !IO),
	list__reverse(RevOtherLabels, OtherLabels),
	list__foldl2(output_c_label_decl(StackLayoutLabels), OtherLabels,
		!DeclSet, !IO).

:- pred group_c_labels_with_layouts(map(label, data_addr)::in, list(label)::in,
	multi_map(proc_label, int)::in, multi_map(proc_label, int)::out,
	multi_map(proc_label, int)::in, multi_map(proc_label, int)::out,
	list(data_addr)::in, list(data_addr)::out,
	list(label)::in, list(label)::out) is det.

group_c_labels_with_layouts(_StackLayoutLabels, [],
		!DeclLLMap, !OtherLocalMap, !RevAddrsToDecl, !RevOthers).
group_c_labels_with_layouts(StackLayoutLabels, [Label | Labels],
		!DeclLLMap, !OtherLocalMap, !RevAddrsToDecl, !RevOthers) :-
	(
		Label = internal(LabelNum, ProcLabel),
		( map__search(StackLayoutLabels, Label, DataAddr) ->
			(
				DataAddr = layout_addr(LayoutName),
				LayoutName = label_layout(ProcLabel, LabelNum,
					LabelVars),
				LabelVars = label_has_var_info
			->
				multi_map__set(!.DeclLLMap, ProcLabel,
					LabelNum, !:DeclLLMap)
			;
				multi_map__set(!.OtherLocalMap, ProcLabel,
					LabelNum, !:OtherLocalMap),
				!:RevAddrsToDecl = [DataAddr
					| !.RevAddrsToDecl]
			)
		;
			multi_map__set(!.OtherLocalMap, ProcLabel, LabelNum,
				!:OtherLocalMap)
		)
	;
		Label = entry(_, _),
		!:RevOthers = [Label | !.RevOthers]
	),
	group_c_labels_with_layouts(StackLayoutLabels, Labels,
		!DeclLLMap, !OtherLocalMap, !RevAddrsToDecl, !RevOthers).

:- pred output_label_layout_decls(pair(proc_label, list(int))::in,
	decl_set::in, decl_set::out, io::di, io::uo) is det.

output_label_layout_decls(ProcLabel - LabelNums0, !DeclSet, !IO) :-
	% There must be a macro of the form MR_DECL_LL<n> for every <n>
	% up to MaxChunkSize.
	list__reverse(LabelNums0, LabelNums),
	MaxChunkSize = 10,
	list__chunk(LabelNums, MaxChunkSize, LabelNumChunks),
	list__foldl2(output_label_layout_decl_group(ProcLabel), LabelNumChunks,
		!DeclSet, !IO).

:- pred output_label_layout_decl_group(proc_label::in, list(int)::in,
	decl_set::in, decl_set::out, io::di, io::uo) is det.

output_label_layout_decl_group(ProcLabel, LabelNums, !DeclSet, !IO) :-
	io__write_string("MR_DECL_LL", !IO),
	io__write_int(list__length(LabelNums), !IO),
	io__write_string("(", !IO),
	output_proc_label(ProcLabel, no, !IO),
	io__write_string(", ", !IO),
	io__write_list(LabelNums, ",", io__write_int, !IO),
	io__write_string(")\n", !IO).

:- pred output_local_label_decls(pair(proc_label, list(int))::in,
	decl_set::in, decl_set::out, io::di, io::uo) is det.

output_local_label_decls(ProcLabel - LabelNums0, !DeclSet, !IO) :-
	% There must be a macro of the form MR_decl_label<n> for every <n>
	% up to MaxChunkSize.
	list__reverse(LabelNums0, LabelNums),
	MaxChunkSize = 8,
	list__chunk(LabelNums, MaxChunkSize, LabelNumChunks),
	list__foldl2(output_local_label_decl_group(ProcLabel), LabelNumChunks,
		!DeclSet, !IO),
	list__foldl(insert_var_info_label_layout_decl(ProcLabel), LabelNums,
		!DeclSet),
	list__foldl(insert_code_addr_decl(ProcLabel), LabelNums, !DeclSet).

:- pred output_local_label_decl_group(proc_label::in, list(int)::in,
	decl_set::in, decl_set::out, io::di, io::uo) is det.

output_local_label_decl_group(ProcLabel, LabelNums, !DeclSet, !IO) :-
	io__write_string("MR_decl_label", !IO),
	io__write_int(list__length(LabelNums), !IO),
	io__write_string("(", !IO),
	output_proc_label(ProcLabel, no, !IO),
	io__write_string(", ", !IO),
	io__write_list(LabelNums, ",", io__write_int, !IO),
	io__write_string(")\n", !IO),
	list__foldl(insert_code_addr_decl(ProcLabel), LabelNums, !DeclSet).

:- pred insert_var_info_label_layout_decl(proc_label::in, int::in,
	decl_set::in, decl_set::out) is det.

insert_var_info_label_layout_decl(ProcLabel, LabelNum, !DeclSet) :-
	LayoutName = label_layout(ProcLabel, LabelNum, label_has_var_info),
	DataAddr = layout_addr(LayoutName),
	DeclId = data_addr(DataAddr),
	decl_set_insert(DeclId, !DeclSet).

:- pred insert_code_addr_decl(proc_label::in, int::in,
	decl_set::in, decl_set::out) is det.

insert_code_addr_decl(ProcLabel, LabelNum, !DeclSet) :-
	DeclId = code_addr(label(internal(LabelNum, ProcLabel))),
	decl_set_insert(DeclId, !DeclSet).

:- pred output_c_label_decl(map(label, data_addr)::in, label::in,
	decl_set::in, decl_set::out, io::di, io::uo) is det.

output_c_label_decl(StackLayoutLabels, Label, !DeclSet, !IO) :-
	%
	% Declare the stack layout entry for this label, if needed.
	%
	( map__search(StackLayoutLabels, Label, DataAddr) ->
		(
			Label = internal(LabelNum, ProcLabel),
			DataAddr = layout_addr(LayoutName),
			LayoutName = label_layout(ProcLabel, LabelNum,
				LabelVars)
		->
			(
				LabelVars = label_has_var_info,
				Macro = "MR_DECL_LL"
			;
				LabelVars = label_has_no_var_info,
				Macro = "MR_DECL_LLNVI"
			),
			io__write_string(Macro, !IO),
			io__write_string("(", !IO),
			output_proc_label(ProcLabel, no, !IO),
			io__write_string(", ", !IO),
			io__write_int(LabelNum, !IO),
			% The final semicolon is in the macro definition.
			io__write_string(")\n", !IO),
			% The macro declares both the label layout structure
			% and the label.
			AlreadyDeclaredLabel = yes
		;
			output_stack_layout_decl(DataAddr, !DeclSet, !IO),
			AlreadyDeclaredLabel = no
		)
	;
		AlreadyDeclaredLabel = no
	),
	(
		AlreadyDeclaredLabel = no,
		%
		% Declare the label itself.
		%
		(
			Label = entry(exported, _),
			DeclMacro = "MR_def_extern_entry("
		;
			Label = entry(local, _),
			% The code for procedures local to a Mercury module
			% should normally be visible only within the C file
			% generated for that module. However, if we generate
			% multiple C files, the code in each C file must be
			% visible to the other C files for that Mercury module.
			globals__io_lookup_bool_option(split_c_files,
				SplitFiles, !IO),
			( SplitFiles = no ->
				DeclMacro = "MR_decl_static("
			;
				DeclMacro = "MR_def_extern_entry("
			)
		;
			Label = entry(c_local, _),
			DeclMacro = "MR_decl_local("
		;
			Label = internal(_, _),
			DeclMacro = "MR_decl_label("
		),
		io__write_string(DeclMacro, !IO),
		io__write_string("", !IO),
		output_label(Label, no, !IO),
		io__write_string(")\n", !IO)
	;
		AlreadyDeclaredLabel = yes
	),
	decl_set_insert(code_addr(label(Label)), !DeclSet).

:- pred output_stack_layout_decl(data_addr::in, decl_set::in, decl_set::out,
	io::di, io::uo) is det.

output_stack_layout_decl(DataAddr, !DeclSet, !IO) :-
	output_data_addr_decls(DataAddr, !DeclSet, !IO).

:- pred output_c_label_inits(map(label, data_addr)::in, list(label)::in,
	io::di, io::uo) is det.

output_c_label_inits(StackLayoutLabels, Labels, !IO) :-
	group_c_labels(StackLayoutLabels, Labels, multi_map__init, NoLayoutMap,
		multi_map__init, LayoutMap, [], RevOtherLabels),
	list__reverse(RevOtherLabels, OtherLabels),
	list__foldl(output_c_label_init(StackLayoutLabels), OtherLabels, !IO),
	multi_map__to_assoc_list(NoLayoutMap, NoLayoutList),
	multi_map__to_assoc_list(LayoutMap, LayoutList),
	list__foldl(output_c_label_init_group(""), NoLayoutList, !IO),
	list__foldl(output_c_label_init_group("_sl"), LayoutList, !IO).

:- pred group_c_labels(map(label, data_addr)::in, list(label)::in,
	multi_map(proc_label, int)::in, multi_map(proc_label, int)::out,
	multi_map(proc_label, int)::in, multi_map(proc_label, int)::out,
	list(label)::in, list(label)::out) is det.

group_c_labels(_StackLayoutLabels, [], !NoLayoutMap, !LayoutMap, !RevOthers).
group_c_labels(StackLayoutLabels, [Label | Labels], !NoLayoutMap, !LayoutMap,
		!RevOthers) :-
	(
		Label = internal(LabelNum, ProcLabel),
		( map__search(StackLayoutLabels, Label, _DataAddr) ->
			multi_map__set(!.LayoutMap, ProcLabel, LabelNum,
				!:LayoutMap)
		;
			multi_map__set(!.NoLayoutMap, ProcLabel, LabelNum,
				!:NoLayoutMap)
		)
	;
		Label = entry(_, _),
		!:RevOthers = [Label | !.RevOthers]
	),
	group_c_labels(StackLayoutLabels, Labels, !NoLayoutMap, !LayoutMap,
		!RevOthers).

:- pred output_c_label_init_group(string::in,
	pair(proc_label, list(int))::in, io::di, io::uo) is det.

output_c_label_init_group(Suffix, ProcLabel - RevLabelNums, !IO) :-
	list__reverse(RevLabelNums, LabelNums),
	% There must be macros of the form MR_init_label<n> and
	% MR_init_label_sl<n> for every <n> up to MaxChunkSize.
	MaxChunkSize = 8,
	list__chunk(LabelNums, MaxChunkSize, LabelNumChunks),
	list__foldl(output_c_label_init_chunk(Suffix, ProcLabel),
		LabelNumChunks, !IO).

:- pred output_c_label_init_chunk(string::in,
	proc_label::in, list(int)::in, io::di, io::uo) is det.

output_c_label_init_chunk(Suffix, ProcLabel, LabelNums, !IO) :-
	io__write_string("\tMR_init_label", !IO),
	io__write_string(Suffix, !IO),
	io__write_int(list__length(LabelNums), !IO),
	io__write_string("(", !IO),
	output_proc_label(ProcLabel, no, !IO),
	io__write_string(",", !IO),
	io__write_list(LabelNums, ",", io__write_int, !IO),
	io__write_string(")\n", !IO).

:- pred output_c_label_init(map(label, data_addr)::in, label::in,
	io::di, io::uo) is det.

output_c_label_init(StackLayoutLabels, Label, !IO) :-
	( map__search(StackLayoutLabels, Label, DataAddr) ->
		SuffixOpen = "_sl(",
		( DataAddr = layout_addr(proc_layout(_, _)) ->
			% Labels whose stack layouts are proc layouts may need
			% to have the code address in that layout initialized
			% at run time (if code addresses are not static).
			InitProcLayout = yes
		;
			% Labels whose stack layouts are internal layouts
			% do not have code addresses in their layouts.
			InitProcLayout = no
		)
	;
		SuffixOpen = "(",
		% This label has no stack layout to initialize.
		InitProcLayout = no
	),
	(
		Label = entry(exported, ProcLabel),
		TabInitMacro = "\tMR_init_entry1"
	;
		Label = entry(local, ProcLabel),
		TabInitMacro = "\tMR_init_entry1"
	;
		Label = entry(c_local, ProcLabel),
		TabInitMacro = "\tMR_init_local1"
	;
		Label = internal(_, _),
		% These should have been separated out by group_c_labels.
		error("output_c_label_init: internal/2")
	),
	io__write_string(TabInitMacro, !IO),
	io__write_string(SuffixOpen, !IO),
	output_proc_label(ProcLabel, no, !IO),
	io__write_string(");\n", !IO),
	( InitProcLayout = yes ->
		io__write_string("\tMR_INIT_PROC_LAYOUT_ADDR(", !IO),
		output_label(Label, !IO),
		io__write_string(");\n", !IO)
	;
		true
	).

:- pred label_is_proc_entry(label::in, bool::out) is det.

label_is_proc_entry(internal(_, _), no).
label_is_proc_entry(entry(_, _), yes).

:- pred output_c_procedure_decls(map(label, data_addr)::in, c_procedure::in,
	decl_set::in, decl_set::out, io::di, io::uo) is det.

output_c_procedure_decls(StackLayoutLabels, Proc, !DeclSet, !IO) :-
	Proc = c_procedure(_Name, _Arity, _PredProcId, Instrs, _, _, _),
	list__foldl2(output_instruction_decls(StackLayoutLabels), Instrs,
		!DeclSet, !IO).

:- pred output_c_procedure(bool::in, bool::in, c_procedure::in,
	io::di, io::uo) is det.

output_c_procedure(PrintComments, EmitCLoops, Proc, !IO) :-
	Proc = c_procedure(Name, Arity, proc(_, ProcId), Instrs, _, _, _),
	proc_id_to_int(ProcId, ModeNum),
	( PrintComments = yes ->
		io__write_string("\n/*-------------------------------------",
			!IO),
		io__write_string("------------------------------------*/\n",
			!IO)
	;
		true
	),
	(
		PrintComments = yes,
		io__write_string("/* code for predicate '", !IO),
			% Now that we have unused_args.m mangling predicate
			% names, we should probably demangle them here.
		io__write_string(Name, !IO),
		io__write_string("'/", !IO),
		io__write_int(Arity, !IO),
		io__write_string(" in mode ", !IO),
		io__write_int(ModeNum, !IO),
		io__write_string(" */\n", !IO)
	;
		PrintComments = no
	),

	llds_out__find_caller_label(Instrs, CallerLabel),
	llds_out__find_cont_labels(Instrs, bintree_set__init, ContLabelSet),
	( EmitCLoops = yes ->
		llds_out__find_while_labels(Instrs,
			bintree_set__init, WhileSet)
	;
		WhileSet = bintree_set__init
	),
	output_instruction_list(Instrs, PrintComments,
		CallerLabel - ContLabelSet, WhileSet, !IO).

	% Find the entry label for the procedure,
	% for use as the profiling "caller label"
	% field in calls within this procedure.

:- pred llds_out__find_caller_label(list(instruction)::in, label::out) is det.

llds_out__find_caller_label([], _) :-
	error("cannot find caller label").
llds_out__find_caller_label([Instr0 - _ | Instrs], CallerLabel) :-
	( Instr0 = label(Label) ->
		(
			Label = internal(_, _),
			error("caller label is internal label")
		;
			Label = entry(_, _),
			CallerLabel = Label
		)
	;
		llds_out__find_caller_label(Instrs, CallerLabel)
	).

	% Locate all the labels which are the continuation labels for calls,
	% nondet disjunctions, forks or joins, and store them in ContLabelSet.

:- pred llds_out__find_cont_labels(list(instruction)::in,
	bintree_set(label)::in, bintree_set(label)::out) is det.

llds_out__find_cont_labels([], !ContLabelSet).
llds_out__find_cont_labels([Instr - _ | Instrs], !ContLabelSet) :-
	(
		(
			Instr = call(_, label(ContLabel), _, _, _, _)
		;
			Instr = mkframe(_, yes(label(ContLabel)))
		;
			Instr = join_and_continue(_, ContLabel)
		;
			Instr = assign(redoip(_), const(Const)),
			Const = code_addr_const(label(ContLabel))
		)
	->
		bintree_set__insert(!.ContLabelSet, ContLabel, !:ContLabelSet)
	;
		Instr = fork(Label1, Label2, _)
	->
		bintree_set__insert_list(!.ContLabelSet, [Label1, Label2],
			!:ContLabelSet)
	;
		Instr = block(_, _, Block)
	->
		llds_out__find_cont_labels(Block, !ContLabelSet)
	;
		true
	),
	llds_out__find_cont_labels(Instrs, !ContLabelSet).

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

:- pred llds_out__find_while_labels(list(instruction)::in,
	bintree_set(label)::in, bintree_set(label)::out) is det.

llds_out__find_while_labels([], !WhileSet).
llds_out__find_while_labels([Instr0 - _ | Instrs0], !WhileSet) :-
	(
		Instr0 = label(Label),
		llds_out__is_while_label(Label, Instrs0, Instrs1, 0, UseCount),
		UseCount > 0
	->
		bintree_set__insert(!.WhileSet, Label, !:WhileSet),
		llds_out__find_while_labels(Instrs1, !WhileSet)
	;
		llds_out__find_while_labels(Instrs0, !WhileSet)
	).

:- pred llds_out__is_while_label(label::in,
	list(instruction)::in, list(instruction)::out, int::in, int::out)
	is det.

llds_out__is_while_label(_, [], [], !Count).
llds_out__is_while_label(Label, [Instr0 - Comment0 | Instrs0], Instrs,
		!Count) :-
	( Instr0 = label(_) ->
		Instrs = [Instr0 - Comment0 | Instrs0]
	; Instr0 = goto(label(Label)) ->
		!:Count = !.Count + 1,
		llds_out__is_while_label(Label, Instrs0, Instrs, !Count)
	; Instr0 = if_val(_, label(Label)) ->
		!:Count = !.Count + 1,
		llds_out__is_while_label(Label, Instrs0, Instrs, !Count)
	;
		llds_out__is_while_label(Label, Instrs0, Instrs, !Count)
	).

%-----------------------------------------------------------------------------%

:- pred output_instruction_decls(map(label, data_addr)::in, instruction::in,
	decl_set::in, decl_set::out, io::di, io::uo) is det.

output_instruction_decls(StackLayoutLabels, Instr - _Comment, !DeclSet, !IO) :-
	output_instr_decls(StackLayoutLabels, Instr, !DeclSet, !IO).

:- pred output_instr_decls(map(label, data_addr)::in, instr::in,
	decl_set::in, decl_set::out, io::di, io::uo) is det.

output_instr_decls(_, comment(_), !DeclSet, !IO).
output_instr_decls(_, livevals(_), !DeclSet, !IO).
output_instr_decls( StackLayoutLabels, block(_TempR, _TempF, Instrs),
		!DeclSet, !IO) :-
	list__foldl2(output_instruction_decls(StackLayoutLabels), Instrs,
		!DeclSet, !IO).
output_instr_decls(_, assign(Lval, Rval), !DeclSet, !IO) :-
	output_lval_decls(Lval, !DeclSet, !IO),
	output_rval_decls(Rval, !DeclSet, !IO).
output_instr_decls(_, call(Target, ContLabel, _, _, _, _),
		!DeclSet, !IO) :-
	output_code_addr_decls(Target, !DeclSet, !IO),
	output_code_addr_decls(ContLabel, !DeclSet, !IO).
output_instr_decls(_, c_code(_, _), !DeclSet, !IO).
output_instr_decls(_, mkframe(FrameInfo, MaybeFailureContinuation),
		!DeclSet, !IO) :-
	(
		FrameInfo = ordinary_frame(_, _, yes(Struct)),
		Struct = pragma_c_struct(StructName, StructFields,
			MaybeStructFieldsContext)
	->
		(
			decl_set_is_member(pragma_c_struct(StructName),
				!.DeclSet)
		->
			string__append_list(["struct ", StructName,
				" has been declared already"], Msg),
			error(Msg)
		;
			true
		),
		io__write_string("struct ", !IO),
		io__write_string(StructName, !IO),
		io__write_string(" {\n", !IO),
		(
			MaybeStructFieldsContext = yes(StructFieldsContext),
			output_set_line_num(StructFieldsContext, !IO),
			io__write_string(StructFields, !IO),
			output_reset_line_num(!IO)
		;
			MaybeStructFieldsContext = no,
			io__write_string(StructFields, !IO)
		),
		io__write_string("\n};\n", !IO),
		decl_set_insert(pragma_c_struct(StructName), !DeclSet)
	;
		true
	),
	(
		MaybeFailureContinuation = yes(FailureContinuation),
		output_code_addr_decls(FailureContinuation, !DeclSet, !IO)
	;
		MaybeFailureContinuation = no
	).
output_instr_decls(_, label(_), !DeclSet, !IO).
output_instr_decls(_, goto(CodeAddr), !DeclSet, !IO) :-
	output_code_addr_decls(CodeAddr, !DeclSet, !IO).
output_instr_decls(_, computed_goto(Rval, _Labels), !DeclSet, !IO) :-
	output_rval_decls(Rval, !DeclSet, !IO).
output_instr_decls(_, if_val(Rval, Target), !DeclSet, !IO) :-
	output_rval_decls(Rval, !DeclSet, !IO),
	output_code_addr_decls(Target, !DeclSet, !IO).
output_instr_decls(_, incr_hp(Lval, _Tag, _, Rval, _), !DeclSet, !IO) :-
	output_lval_decls(Lval, !DeclSet, !IO),
	output_rval_decls(Rval, !DeclSet, !IO).
output_instr_decls(_, mark_hp(Lval), !DeclSet, !IO) :-
	output_lval_decls(Lval, !DeclSet, !IO).
output_instr_decls(_, restore_hp(Rval), !DeclSet, !IO) :-
	output_rval_decls(Rval, !DeclSet, !IO).
output_instr_decls(_, free_heap(Rval), !DeclSet, !IO) :-
	output_rval_decls(Rval, !DeclSet, !IO).
output_instr_decls(_, store_ticket(Lval), !DeclSet, !IO) :-
	output_lval_decls(Lval, !DeclSet, !IO).
output_instr_decls(_, reset_ticket(Rval, _Reason), !DeclSet, !IO) :-
	output_rval_decls(Rval, !DeclSet, !IO).
output_instr_decls(_, discard_ticket, !DeclSet, !IO).
output_instr_decls(_, prune_ticket, !DeclSet, !IO).
output_instr_decls(_, mark_ticket_stack(Lval), !DeclSet, !IO) :-
	output_lval_decls(Lval, !DeclSet, !IO).
output_instr_decls(_, prune_tickets_to(Rval), !DeclSet, !IO) :-
	output_rval_decls(Rval, !DeclSet, !IO).
output_instr_decls(_, incr_sp(_, _), !DeclSet, !IO).
output_instr_decls(_, decr_sp(_), !DeclSet, !IO).
output_instr_decls(StackLayoutLabels, pragma_c(_, Comps, _, _,
		MaybeLayoutLabel, MaybeOnlyLayoutLabel, _, _, _),
		!DeclSet, !IO) :-
	( MaybeLayoutLabel = yes(Label) ->
		map__lookup(StackLayoutLabels, Label, DataAddr),
		output_stack_layout_decl(DataAddr, !DeclSet, !IO)
	;
		true
	),
	( MaybeOnlyLayoutLabel = yes(OnlyLabel) ->
		map__lookup(StackLayoutLabels, OnlyLabel, OnlyDataAddr),
		output_stack_layout_decl(OnlyDataAddr, !DeclSet, !IO)
	;
		true
	),
	list__foldl2(output_pragma_c_component_decls, Comps, !DeclSet, !IO).
output_instr_decls(_, init_sync_term(Lval, _), !DeclSet, !IO) :-
	output_lval_decls(Lval, !DeclSet, !IO).
output_instr_decls(_, fork(Child, Parent, _), !DeclSet, !IO) :-
	output_code_addr_decls(label(Child), !DeclSet, !IO),
	output_code_addr_decls(label(Parent), !DeclSet, !IO).
output_instr_decls(_, join_and_terminate(Lval), !DeclSet, !IO) :-
	output_lval_decls(Lval, !DeclSet, !IO).
output_instr_decls(_, join_and_continue(Lval, Label), !DeclSet, !IO) :-
	output_lval_decls(Lval, !DeclSet, !IO),
	output_code_addr_decls(label(Label), !DeclSet, !IO).

:- pred output_pragma_c_component_decls(pragma_c_component::in,
	decl_set::in, decl_set::out, io::di, io::uo) is det.

output_pragma_c_component_decls(pragma_c_inputs(Inputs), !DeclSet, !IO) :-
	output_pragma_input_rval_decls(Inputs, !DeclSet, !IO).
output_pragma_c_component_decls(pragma_c_outputs(Outputs), !DeclSet, !IO) :-
	output_pragma_output_lval_decls(Outputs, !DeclSet, !IO).
output_pragma_c_component_decls(pragma_c_raw_code(_, _), !DeclSet, !IO).
output_pragma_c_component_decls(pragma_c_user_code(_, _), !DeclSet, !IO).
output_pragma_c_component_decls(pragma_c_fail_to(_), !DeclSet, !IO).
output_pragma_c_component_decls(pragma_c_noop, !DeclSet, !IO).

%-----------------------------------------------------------------------------%

:- pred output_instruction_list(list(instruction)::in, bool::in,
	pair(label, bintree_set(label))::in, bintree_set(label)::in,
	io::di, io::uo) is det.

output_instruction_list([], _, _, _, !IO).
output_instruction_list([Instr0 - Comment0 | Instrs], PrintComments, ProfInfo,
		WhileSet, !IO) :-
	output_instruction_and_comment(Instr0, Comment0,
		PrintComments, ProfInfo, !IO),
	(
		Instr0 = label(Label),
		bintree_set__is_member(Label, WhileSet)
	->
		io__write_string("\twhile (1) {\n", !IO),
		output_instruction_list_while(Instrs, Label,
			PrintComments, ProfInfo, WhileSet, !IO)
	;
		output_instruction_list(Instrs, PrintComments, ProfInfo,
			WhileSet, !IO)
	).

:- pred output_instruction_list_while(list(instruction)::in, label::in,
	bool::in, pair(label, bintree_set(label))::in, bintree_set(label)::in,
	io::di, io::uo) is det.

output_instruction_list_while([], _, _, _, _, !IO) :-
	io__write_string("\tbreak; } /* end while */\n", !IO).
output_instruction_list_while([Instr0 - Comment0 | Instrs], Label,
		PrintComments, ProfInfo, WhileSet, !IO) :-
	( Instr0 = label(_) ->
		io__write_string("\tbreak; } /* end while */\n", !IO),
		output_instruction_list([Instr0 - Comment0 | Instrs],
			PrintComments, ProfInfo, WhileSet, !IO)
	; Instr0 = goto(label(Label)) ->
		io__write_string("\t/* continue */ } /* end while */\n", !IO),
		output_instruction_list(Instrs, PrintComments, ProfInfo,
			WhileSet, !IO)
	; Instr0 = if_val(Rval, label(Label)) ->
		io__write_string("\tif (", !IO),
		output_test_rval(Rval, !IO),
		io__write_string(")\n\t\tcontinue;\n", !IO),
		(
			PrintComments = yes,
			Comment0 \= ""
		->
			io__write_string("\t\t/* ", !IO),
			io__write_string(Comment0, !IO),
			io__write_string(" */\n", !IO)
		;
			true
		),
		output_instruction_list_while(Instrs, Label,
			PrintComments, ProfInfo, WhileSet, !IO)
	;
		output_instruction_and_comment(Instr0, Comment0,
			PrintComments, ProfInfo, !IO),
		output_instruction_list_while(Instrs, Label,
			PrintComments, ProfInfo, WhileSet, !IO)
	).

:- pred output_instruction_and_comment(instr::in, string::in, bool::in,
	pair(label, bintree_set(label))::in, io::di, io::uo) is det.

output_instruction_and_comment(Instr, Comment, PrintComments, ProfInfo, !IO) :-
	(
		PrintComments = no,
		(
			( Instr = comment(_)
			; Instr = livevals(_)
			)
		->
			true
		;
			output_instruction(Instr, ProfInfo, !IO)
		)
	;
		PrintComments = yes,
		output_instruction(Instr, ProfInfo, !IO),
		( Comment = "" ->
			true
		;
			io__write_string("\t\t/* ", !IO),
			io__write_string(Comment, !IO),
			io__write_string(" */\n", !IO)
		)
	).

	% output_instruction_and_comment/5 is only for debugging.
	% Normally we use output_instruction_and_comment/6.

output_instruction_and_comment(Instr, Comment, PrintComments, !IO) :-
	bintree_set__init(ContLabelSet),
	DummyModule = unqualified("DEBUG"),
	DummyPredName = "DEBUG",
	proc_id_to_int(hlds_pred__initial_proc_id, InitialProcIdInt),
	ProcLabel = proc(DummyModule, predicate, DummyModule,
		DummyPredName, 0, InitialProcIdInt),
	ProfInfo = entry(local, ProcLabel) - ContLabelSet,
	output_instruction_and_comment(Instr, Comment, PrintComments,
		ProfInfo, !IO).

	% output_instruction/3 is only for debugging.
	% Normally we use output_instruction/4.

output_instruction(Instr, !IO) :-
	bintree_set__init(ContLabelSet),
	DummyModule = unqualified("DEBUG"),
	DummyPredName = "DEBUG",
	proc_id_to_int(hlds_pred__initial_proc_id, InitialProcIdInt),
	ProcLabel = proc(DummyModule, predicate, DummyModule,
		DummyPredName, 0, InitialProcIdInt),
	ProfInfo = entry(local, ProcLabel) - ContLabelSet,
	output_instruction(Instr, ProfInfo, !IO).

:- pred output_instruction(instr::in, pair(label, bintree_set(label))::in,
	io::di, io::uo) is det.

output_instruction(comment(Comment), _, !IO) :-
	io__write_strings(["/* ", Comment, " */\n"], !IO).

output_instruction(livevals(LiveVals), _, !IO) :-
	io__write_string("/*\n * Live lvalues:\n", !IO),
	set__to_sorted_list(LiveVals, LiveValsList),
	output_livevals(LiveValsList, !IO),
	io__write_string(" */\n", !IO).

output_instruction(block(TempR, TempF, Instrs), ProfInfo, !IO) :-
	io__write_string("\t{\n", !IO),
	( TempR > 0 ->
		io__write_string("\tMR_Word ", !IO),
		output_temp_decls(TempR, "r", !IO),
		io__write_string(";\n", !IO)
	;
		true
	),
	( TempF > 0 ->
		io__write_string("\tMR_Float ", !IO),
		output_temp_decls(TempF, "f", !IO),
		io__write_string(";\n", !IO)
	;
		true
	),
	globals__io_lookup_bool_option(auto_comments, PrintComments, !IO),
	output_instruction_list(Instrs, PrintComments, ProfInfo,
		bintree_set__init, !IO),
	io__write_string("\t}\n", !IO).

output_instruction(assign(Lval, Rval), _, !IO) :-
	io__write_string("\t", !IO),
	output_lval_for_assign(Lval, Type, !IO),
	io__write_string(" = ", !IO),
	output_rval_as_type(Rval, Type, !IO),
	io__write_string(";\n", !IO).

output_instruction(call(Target, ContLabel, LiveVals, _, _, _), ProfInfo,			!IO) :-
	ProfInfo = CallerLabel - _,
	output_call(Target, ContLabel, CallerLabel, !IO),
	output_gc_livevals(LiveVals, !IO).

output_instruction(c_code(C_Code_String, _), _, !IO) :-
	io__write_string("\t", !IO),
	io__write_string(C_Code_String, !IO).

output_instruction(mkframe(FrameInfo, MaybeFailCont), _, !IO) :-
	(
		FrameInfo = ordinary_frame(Msg, Num, MaybeStruct),
		(
			MaybeStruct = yes(pragma_c_struct(StructName, _, _)),
			(
				MaybeFailCont = yes(FailCont),
				io__write_string("\tMR_mkpragmaframe(""", !IO),
				c_util__output_quoted_string(Msg, !IO),
				io__write_string(""", ", !IO),
				io__write_int(Num, !IO),
				io__write_string(", ", !IO),
				io__write_string(StructName, !IO),
				io__write_string(", ", !IO),
				output_code_addr(FailCont, !IO),
				io__write_string(");\n", !IO)
			;
				MaybeFailCont = no,
				io__write_string(
					"\tMR_mkpragmaframe_no_redoip(""",
					!IO),
				c_util__output_quoted_string(Msg, !IO),
				io__write_string(""", ", !IO),
				io__write_int(Num, !IO),
				io__write_string(", ", !IO),
				io__write_string(StructName, !IO),
				io__write_string(");\n", !IO)
			)
		;
			MaybeStruct = no,
			(
				MaybeFailCont = yes(FailCont),
				io__write_string("\tMR_mkframe(""", !IO),
				c_util__output_quoted_string(Msg, !IO),
				io__write_string(""", ", !IO),
				io__write_int(Num, !IO),
				io__write_string(", ", !IO),
				output_code_addr(FailCont, !IO),
				io__write_string(");\n", !IO)
			;
				MaybeFailCont = no,
				io__write_string("\tMR_mkframe_no_redoip(""",
					!IO),
				c_util__output_quoted_string(Msg, !IO),
				io__write_string(""", ", !IO),
				io__write_int(Num, !IO),
				io__write_string(");\n", !IO)
			)
		)
	;
		FrameInfo = temp_frame(Kind),
		(
			Kind = det_stack_proc,
			io__write_string("\tMR_mkdettempframe(", !IO),
			(
				MaybeFailCont = yes(FailCont),
				output_code_addr(FailCont, !IO)
			;
				MaybeFailCont = no,
				error("output_instruction: no failcont")
			),
			io__write_string(");\n", !IO)
		;
			Kind = nondet_stack_proc,
			io__write_string("\tMR_mktempframe(", !IO),
			(
				MaybeFailCont = yes(FailCont),
				output_code_addr(FailCont, !IO)
			;
				MaybeFailCont = no,
				error("output_instruction: no failcont")
			),
			io__write_string(");\n", !IO)
		)
	).

output_instruction(label(Label), ProfInfo, !IO) :-
	output_label_defn(Label, !IO),
	maybe_output_update_prof_counter(Label, ProfInfo, !IO).

output_instruction(goto(CodeAddr), ProfInfo, !IO) :-
	ProfInfo = CallerLabel - _,
	io__write_string("\t", !IO),
	output_goto(CodeAddr, CallerLabel, !IO).

output_instruction(computed_goto(Rval, Labels), _, !IO) :-
	io__write_string("\tMR_COMPUTED_GOTO(", !IO),
	output_rval_as_type(Rval, unsigned, !IO),
	io__write_string(",\n\t\t", !IO),
	output_label_list(Labels, !IO),
	io__write_string(");\n", !IO).

output_instruction(if_val(Rval, Target), ProfInfo, !IO) :-
	ProfInfo = CallerLabel - _,
	io__write_string("\tif (", !IO),
	output_test_rval(Rval, !IO),
	io__write_string(") {\n\t\t", !IO),
	output_goto(Target, CallerLabel, !IO),
	io__write_string("\t}\n", !IO).

output_instruction(incr_hp(Lval, MaybeTag, MaybeOffset, Rval, TypeMsg),
		ProfInfo, !IO) :-
	globals__io_lookup_bool_option(profile_memory, ProfMem, !IO),
	(
		ProfMem = yes,
		(
			MaybeTag = no,
			io__write_string("\tMR_offset_incr_hp_msg(", !IO),
			output_lval_as_word(Lval, !IO)
		;
			MaybeTag = yes(Tag),
			io__write_string("\tMR_tag_offset_incr_hp_msg(", !IO),
			output_lval_as_word(Lval, !IO),
			io__write_string(", ", !IO),
			output_tag(Tag, !IO)
		),
		io__write_string(", ", !IO),
		(
			MaybeOffset = no,
			io__write_string("0, ", !IO)
		;
			MaybeOffset = yes(Offset),
			io__write_int(Offset, !IO),
			io__write_string(", ", !IO)
		),
		output_rval_as_type(Rval, word, !IO),
		io__write_string(", ", !IO),
		ProfInfo = CallerLabel - _,
		output_label(CallerLabel, !IO),
		io__write_string(", """, !IO),
		c_util__output_quoted_string(TypeMsg, !IO),
		io__write_string(""");\n", !IO)
	;
		ProfMem = no,
		(
			MaybeTag = no,
			(
				MaybeOffset = yes(_),
				io__write_string("\tMR_offset_incr_hp(", !IO)
			;
				MaybeOffset = no,
				io__write_string("\tMR_alloc_heap(", !IO)
			),
			output_lval_as_word(Lval, !IO)
		;
			MaybeTag = yes(Tag),
			(
				MaybeOffset = yes(_),
				io__write_string("\tMR_tag_offset_incr_hp(",
					!IO),
				output_lval_as_word(Lval, !IO),
				io__write_string(", ", !IO),
				output_tag(Tag, !IO)
			;
				MaybeOffset = no,
				io__write_string("\tMR_tag_alloc_heap(", !IO),
				output_lval_as_word(Lval, !IO),
				io__write_string(", ", !IO),
				io__write_int(Tag, !IO)
			)
		),
		io__write_string(", ", !IO),
		(
			MaybeOffset = yes(Offset),
			io__write_int(Offset, !IO),
			io__write_string(", ", !IO)
		;
			MaybeOffset = no
		),
		output_rval_as_type(Rval, word, !IO),
		io__write_string(");\n", !IO)
	).

output_instruction(mark_hp(Lval), _, !IO) :-
	io__write_string("\tMR_mark_hp(", !IO),
	output_lval_as_word(Lval, !IO),
	io__write_string(");\n", !IO).

output_instruction(restore_hp(Rval), _, !IO) :-
	io__write_string("\tMR_restore_hp(", !IO),
	output_rval_as_type(Rval, word, !IO),
	io__write_string(");\n", !IO).

output_instruction(free_heap(Rval), _, !IO) :-
	io__write_string("\tMR_free_heap(", !IO),
	output_rval_as_type(Rval, data_ptr, !IO),
	io__write_string(");\n", !IO).

output_instruction(store_ticket(Lval), _, !IO) :-
	io__write_string("\tMR_store_ticket(", !IO),
	output_lval_as_word(Lval, !IO),
	io__write_string(");\n", !IO).

output_instruction(reset_ticket(Rval, Reason), _, !IO) :-
	io__write_string("\tMR_reset_ticket(", !IO),
	output_rval_as_type(Rval, word, !IO),
	io__write_string(", ", !IO),
	output_reset_trail_reason(Reason, !IO),
	io__write_string(");\n", !IO).

output_instruction(discard_ticket, _, !IO) :-
	io__write_string("\tMR_discard_ticket();\n", !IO).

output_instruction(prune_ticket, _, !IO) :-
	io__write_string("\tMR_prune_ticket();\n", !IO).

output_instruction(mark_ticket_stack(Lval), _, !IO) :-
	io__write_string("\tMR_mark_ticket_stack(", !IO),
	output_lval_as_word(Lval, !IO),
	io__write_string(");\n", !IO).

output_instruction(prune_tickets_to(Rval), _, !IO) :-
	io__write_string("\tMR_prune_tickets_to(", !IO),
	output_rval_as_type(Rval, word, !IO),
	io__write_string(");\n", !IO).

output_instruction(incr_sp(N, _Msg), _, !IO) :-
	io__write_string("\tMR_incr_sp(", !IO),
	io__write_int(N, !IO),
	io__write_string(");\n", !IO).
	% Use the code below instead of the code above if you want to run
	% tools/framesize on the output of the compiler.
	% io__write_string("\tMR_incr_sp_push_msg(", !IO),
	% io__write_int(N, !IO),
	% io__write_string(", """, !IO),
	% c_util__output_quoted_string(Msg, !IO),
	% io__write_string(""");\n", !IO).

output_instruction(decr_sp(N), _, !IO) :-
	io__write_string("\tMR_decr_sp(", !IO),
	io__write_int(N, !IO),
	io__write_string(");\n", !IO).

output_instruction(pragma_c(Decls, Components, _, _, _, _, _, _, _), _, !IO) :-
	io__write_string("\t{\n", !IO),
	output_pragma_decls(Decls, !IO),
	list__foldl(output_pragma_c_component, Components, !IO),
	io__write_string("\t}\n", !IO).

output_instruction(init_sync_term(Lval, N), _, !IO) :-
	io__write_string("\tMR_init_sync_term(", !IO),
	output_lval_as_word(Lval, !IO),
	io__write_string(", ", !IO),
	io__write_int(N, !IO),
	io__write_string(");\n", !IO).

output_instruction(fork(Child, Parent, Lval), _, !IO) :-
	io__write_string("\tMR_fork_new_context(", !IO),
	output_label_as_code_addr(Child, !IO),
	io__write_string(", ", !IO),
	output_label_as_code_addr(Parent, !IO),
	io__write_string(", ", !IO),
	io__write_int(Lval, !IO),
	io__write_string(");\n", !IO).

output_instruction(join_and_terminate(Lval), _, !IO) :-
	io__write_string("\tMR_join_and_terminate(", !IO),
	output_lval(Lval, !IO),
	io__write_string(");\n", !IO).

output_instruction(join_and_continue(Lval, Label), _, !IO) :-
	io__write_string("\tMR_join_and_continue(", !IO),
	output_lval(Lval, !IO),
	io__write_string(", ", !IO),
	output_label_as_code_addr(Label, !IO),
	io__write_string(");\n", !IO).

:- pred output_pragma_c_component(pragma_c_component::in, io::di, io::uo)
	is det.

output_pragma_c_component(pragma_c_inputs(Inputs), !IO) :-
	output_pragma_inputs(Inputs, !IO).
output_pragma_c_component(pragma_c_outputs(Outputs), !IO) :-
	output_pragma_outputs(Outputs, !IO).
output_pragma_c_component(pragma_c_user_code(MaybeContext, C_Code), !IO) :-
	( C_Code = "" ->
		true
	;
			% We should start the C_Code on a new line,
			% just in case it starts with a proprocessor directive.
		(
			MaybeContext = yes(Context),
			io__write_string("{\n", !IO),
			output_set_line_num(Context, !IO),
			io__write_string(C_Code, !IO),
			io__write_string(";}\n", !IO),
			output_reset_line_num(!IO)
		;
			MaybeContext = no,
			io__write_string("{\n", !IO),
			io__write_string(C_Code, !IO),
			io__write_string(";}\n", !IO)
		)
	).
output_pragma_c_component(pragma_c_raw_code(C_Code, _), !IO) :-
	io__write_string(C_Code, !IO).
output_pragma_c_component(pragma_c_fail_to(Label), !IO) :-
	io__write_string("if (!" ++ pragma_succ_ind_name ++
		") MR_GOTO_LAB(", !IO),
	output_label(Label, no, !IO),
	io__write_string(");\n", !IO).
output_pragma_c_component(pragma_c_noop, !IO).

	% Output the local variable declarations at the top of the
	% pragma_foreign code for C.
:- pred output_pragma_decls(list(pragma_c_decl)::in, io::di, io::uo) is det.

output_pragma_decls([], !IO).
output_pragma_decls([Decl | Decls], !IO) :-
	(
		% Apart from special cases, the local variables are MR_Words
		Decl = pragma_c_arg_decl(_Type, TypeString, VarName),
		io__write_string("\t", !IO),
		io__write_string(TypeString, !IO),
		io__write_string("\t", !IO),
		io__write_string(VarName, !IO),
		io__write_string(";\n", !IO)
	;
		Decl = pragma_c_struct_ptr_decl(StructTag, VarName),
		io__write_string("\tstruct ", !IO),
		io__write_string(StructTag, !IO),
		io__write_string("\t*", !IO),
		io__write_string(VarName, !IO),
		io__write_string(";\n", !IO)
	),
	output_pragma_decls(Decls, !IO).

	% Output declarations for any rvals used to initialize the inputs
:- pred output_pragma_input_rval_decls(list(pragma_c_input)::in,
	decl_set::in, decl_set::out, io::di, io::uo) is det.

output_pragma_input_rval_decls([], !DeclSet, !IO).
output_pragma_input_rval_decls([Input | Inputs], !DeclSet, !IO) :-
	Input = pragma_c_input(_VarName, _VarType, _OrigType, Rval, _),
	output_rval_decls(Rval, "\t", "\t", 0, _N, !DeclSet, !IO),
	output_pragma_input_rval_decls(Inputs, !DeclSet, !IO).

	% Output the input variable assignments at the top of the
	% pragma foreign_code code for C.
:- pred output_pragma_inputs(list(pragma_c_input)::in, io::di, io::uo) is det.

output_pragma_inputs([], !IO).
output_pragma_inputs([Input | Inputs], !IO) :-
	Input = pragma_c_input(_VarName, VarType, _OrigType, _Rval,
		_MaybeForeignTypeInfo),
	( is_dummy_argument_type(VarType) ->
		true
	;
		output_pragma_input(Input, !IO)
	),
	output_pragma_inputs(Inputs, !IO).

	% Output the input variable assignments at the top of the
	% pragma foreign_code code for C.
:- pred output_pragma_input(pragma_c_input::in, io::di, io::uo) is det.

output_pragma_input(Input, !IO) :-
	Input = pragma_c_input(VarName, _VarType, OrigType, Rval,
		MaybeForeignTypeInfo),
	io__write_string("\t", !IO),
	(
		MaybeForeignTypeInfo = yes(ForeignTypeInfo),
		ForeignTypeInfo = pragma_c_foreign_type(ForeignType,
			Assertions),
		% For foreign types for which c_type_is_word_sized_int_or_ptr
		% succeeds, the code in the else branch is not only correct,
		% it also generates faster code than would be generated by
		% the then branch, because MR_MAYBE_UNBOX_FOREIGN_TYPE
		% invokes memcpy when given a word-sized type.
		(
			( c_type_is_word_sized_int_or_ptr(ForeignType)
			; list__member(can_pass_as_mercury_type, Assertions)
			)
		->
			% Note that for this cast to be correct the foreign
			% type must be a word sized integer or pointer type.
			io__write_string(VarName, !IO),
			io__write_string(" = ", !IO),
			io__write_string("(" ++ ForeignType ++ ") ", !IO),
			output_rval_as_type(Rval, word, !IO)
		;
			io__write_string("MR_MAYBE_UNBOX_FOREIGN_TYPE(", !IO),
			io__write_string(ForeignType, !IO),
			io__write_string(", ", !IO),
			output_rval_as_type(Rval, word, !IO),
			io__write_string(", ", !IO),
			io__write_string(VarName, !IO),
			io__write_string(")", !IO)
		)
	;
		MaybeForeignTypeInfo = no,
		io__write_string(VarName, !IO),
		io__write_string(" = ", !IO),
		( OrigType = term__functor(term__atom("string"), [], _) ->
			output_llds_type_cast(string, !IO),
			output_rval_as_type(Rval, word, !IO)
		; OrigType = term__functor(term__atom("float"), [], _) ->
			output_rval_as_type(Rval, float, !IO)
		;
			output_rval_as_type(Rval, word, !IO)
		)
	),
	io__write_string(";\n", !IO).

	% Output declarations for any lvals used for the outputs
:- pred output_pragma_output_lval_decls(list(pragma_c_output)::in,
	decl_set::in, decl_set::out, io::di, io::uo) is det.

output_pragma_output_lval_decls([], !DeclSet, !IO).
output_pragma_output_lval_decls([O | Outputs], !DeclSet, !IO) :-
	O = pragma_c_output(Lval, _VarType, _OrigType, _VarName, _),
	output_lval_decls(Lval, "\t", "\t", 0, _N, !DeclSet, !IO),
	output_pragma_output_lval_decls(Outputs, !DeclSet, !IO).

	% Output the output variable assignments at the bottom of the
	% pragma foreign code for C
:- pred output_pragma_outputs(list(pragma_c_output)::in, io::di, io::uo)
	is det.

output_pragma_outputs([], !IO).
output_pragma_outputs([Output | Outputs], !IO) :-
	Output = pragma_c_output(_Lval, VarType, _OrigType, _VarName,
		_MaybeForeignType),
	( is_dummy_argument_type(VarType) ->
		true
	;
		output_pragma_output(Output, !IO)
	),
	output_pragma_outputs(Outputs, !IO).

	% Output the output variable assignments at the bottom of the
	% pragma foreign code for C
:- pred output_pragma_output(pragma_c_output::in, io::di, io::uo) is det.

output_pragma_output(Output, !IO) :-
	Output = pragma_c_output(Lval, _VarType, OrigType, VarName,
		MaybeForeignType),
	io__write_string("\t", !IO),
	(
		MaybeForeignType = yes(ForeignTypeInfo),
		ForeignTypeInfo = pragma_c_foreign_type(ForeignType,
			Assertions),
		( list__member(can_pass_as_mercury_type, Assertions) ->
			output_lval_as_word(Lval, !IO),
			io__write_string(" = ", !IO),
			output_llds_type_cast(word, !IO),
			io__write_string(VarName, !IO)
		;
			io__write_string("MR_MAYBE_BOX_FOREIGN_TYPE(", !IO),
			io__write_string(ForeignType, !IO),
			io__write_string(", ", !IO),
			io__write_string(VarName, !IO),
			io__write_string(", ", !IO),
			output_lval_as_word(Lval, !IO),
			io__write_string(")", !IO)
		)
	;
		MaybeForeignType = no,
		output_lval_as_word(Lval, !IO),
		io__write_string(" = ", !IO),
		(
			OrigType = term__functor(term__atom("string"), [], _)
		->
			output_llds_type_cast(word, !IO),
			io__write_string(VarName, !IO)
		;
			OrigType = term__functor(term__atom("float"), [], _)
		->
			io__write_string("MR_float_to_word(", !IO),
			io__write_string(VarName, !IO),
			io__write_string(")", !IO)
		;
			io__write_string(VarName, !IO)
		)
	),
	io__write_string(";\n", !IO).

:- pred output_reset_trail_reason(reset_trail_reason::in, io::di, io::uo)
	is det.

output_reset_trail_reason(undo, !IO) :-
	io__write_string("MR_undo", !IO).
output_reset_trail_reason(commit, !IO) :-
	io__write_string("MR_commit", !IO).
output_reset_trail_reason(solve, !IO) :-
	io__write_string("MR_solve", !IO).
output_reset_trail_reason(exception, !IO) :-
	io__write_string("MR_exception", !IO).
output_reset_trail_reason(retry, !IO) :-
	io__write_string("MR_retry", !IO).
output_reset_trail_reason(gc, !IO) :-
	io__write_string("MR_gc", !IO).

:- pred output_livevals(list(lval)::in, io::di, io::uo) is det.

output_livevals([], !IO).
output_livevals([Lval | Lvals], !IO) :-
	io__write_string(" *\t", !IO),
	output_lval(Lval, !IO),
	io__write_string("\n", !IO),
	output_livevals(Lvals, !IO).

:- pred output_gc_livevals(list(liveinfo)::in, io::di, io::uo) is det.

output_gc_livevals(LiveVals, !IO) :-
	globals__io_lookup_bool_option(auto_comments, PrintAutoComments, !IO),
	( PrintAutoComments = yes ->
		io__write_string("/*\n", !IO),
		io__write_string(" * Garbage collection livevals info\n", !IO),
		output_gc_livevals_2(LiveVals, !IO),
		io__write_string(" */\n", !IO)
	;
		true
	).

:- pred output_gc_livevals_2(list(liveinfo)::in, io::di, io::uo) is det.

output_gc_livevals_2([], !IO).
output_gc_livevals_2([LiveInfo | LiveInfos], !IO) :-
	LiveInfo = live_lvalue(Locn, LiveValueType, TypeParams),
	io__write_string(" *\t", !IO),
	output_layout_locn(Locn, !IO),
	io__write_string("\t", !IO),
	output_live_value_type(LiveValueType, !IO),
	io__write_string("\t", !IO),
	map__to_assoc_list(TypeParams, TypeParamList),
	output_gc_livevals_params(TypeParamList, !IO),
	io__write_string("\n", !IO),
	output_gc_livevals_2(LiveInfos, !IO).

:- pred output_gc_livevals_params(assoc_list(tvar, set(layout_locn))::in,
	io::di, io::uo) is det.

output_gc_livevals_params([], !IO).
output_gc_livevals_params([Var - LocnSet | Locns], !IO) :-
	term__var_to_int(Var, VarInt),
	io__write_int(VarInt, !IO),
	io__write_string(" - ", !IO),
	set__to_sorted_list(LocnSet, LocnList),
	output_layout_locns(LocnList, !IO),
	io__write_string("  ", !IO),
	output_gc_livevals_params(Locns, !IO).

:- pred output_layout_locns(list(layout_locn)::in, io::di, io::uo) is det.

output_layout_locns([], !IO).
output_layout_locns([Locn | Locns], !IO) :-
	output_layout_locn(Locn, !IO),
	( Locns = [] ->
		true
	;
		io__write_string(" and ", !IO),
		output_layout_locns(Locns, !IO)
	).

:- pred output_layout_locn(layout_locn::in, io::di, io::uo) is det.

output_layout_locn(Locn, !IO) :-
	(
		Locn = direct(Lval),
		output_lval(Lval, !IO)
	;
		Locn = indirect(Lval, Offset),
		io__write_string("offset ", !IO),
		io__write_int(Offset, !IO),
		io__write_string(" from ", !IO),
		output_lval(Lval, !IO)
	).

:- pred output_live_value_type(live_value_type::in, io::di, io::uo) is det.

output_live_value_type(succip, !IO) :-
	io__write_string("type succip", !IO).
output_live_value_type(curfr, !IO) :-
	io__write_string("type curfr", !IO).
output_live_value_type(maxfr, !IO) :-
	io__write_string("type maxfr", !IO).
output_live_value_type(redofr, !IO) :-
	io__write_string("type redofr", !IO).
output_live_value_type(redoip, !IO) :-
	io__write_string("type redoip", !IO).
output_live_value_type(hp, !IO) :-
	io__write_string("type hp", !IO).
output_live_value_type(trail_ptr, !IO) :-
	io__write_string("type trail_ptr", !IO).
output_live_value_type(ticket, !IO) :-
	io__write_string("type ticket", !IO).
output_live_value_type(unwanted, !IO) :-
	io__write_string("unwanted", !IO).
output_live_value_type(var(Var, Name, Type, LldsInst), !IO) :-
	io__write_string("var(", !IO),
	term__var_to_int(Var, VarInt),
	io__write_int(VarInt, !IO),
	io__write_string(", ", !IO),
	io__write_string(Name, !IO),
	io__write_string(", ", !IO),
		% XXX Fake type varset
	varset__init(NewTVarset),
	mercury_output_term(Type, NewTVarset, no, !IO),
	io__write_string(", ", !IO),
	(
		LldsInst = ground,
		io__write_string("ground", !IO)
	;
		LldsInst = partial(Inst),
			% XXX Fake inst varset
		varset__init(NewIVarset),
		mercury_output_inst(Inst, NewIVarset, !IO)
	),
	io__write_string(")", !IO).

:- pred output_temp_decls(int::in, string::in, io::di, io::uo) is det.

output_temp_decls(N, Type, !IO) :-
	output_temp_decls_2(1, N, Type, !IO).

:- pred output_temp_decls_2(int::in, int::in, string::in, io::di, io::uo)
	is det.

output_temp_decls_2(Next, Max, Type, !IO) :-
	( Next =< Max ->
		( Next > 1 ->
			io__write_string(", ", !IO)
		;
			true
		),
		io__write_string("MR_temp", !IO),
		io__write_string(Type, !IO),
		io__write_int(Next, !IO),
		output_temp_decls_2(Next + 1, Max, Type, !IO)
	;
		true
	).

output_rval_decls(Lval, !DeclSet, !IO) :-
	output_rval_decls(Lval, "", "", 0, _, !DeclSet, !IO).

	% output_rval_decls(Rval, FirstIndent, LaterIndent, N0, N,
	% DeclSet0, DeclSet) outputs the declarations of any static constants,
	% etc. that need to be declared before output_rval(Rval) is called.
	% FirstIndent is output before the first declaration, while
	% LaterIndent is output before all later declaration; N0 and N
	% give the number of declarations output before and after this call.
	%
	% Every time we emit a declaration for a symbol, we insert it into the
	% set of symbols we've already declared. That way, we avoid generating
	% the same symbol twice, which would cause an error in the C code.

:- pred output_rval_decls(rval::in, string::in, string::in, int::in, int::out,
	decl_set::in, decl_set::out, io::di, io::uo) is det.

output_rval_decls(lval(Lval), FirstIndent, LaterIndent, !N, !DeclSet, !IO) :-
	output_lval_decls(Lval, FirstIndent, LaterIndent, !N, !DeclSet, !IO).
output_rval_decls(var(_), _, _, _, _, _, _, !IO) :-
	error("output_rval_decls: unexpected var").
output_rval_decls(mkword(_, Rval), FirstIndent, LaterIndent,
		!N, !DeclSet, !IO) :-
	output_rval_decls(Rval, FirstIndent, LaterIndent, !N, !DeclSet, !IO).
output_rval_decls(const(Const), FirstIndent, LaterIndent, !N, !DeclSet, !IO) :-
	( Const = code_addr_const(CodeAddress) ->
		output_code_addr_decls(CodeAddress, FirstIndent, LaterIndent,
			!N, !DeclSet, !IO)
	; Const = data_addr_const(DataAddr, _) ->
		output_data_addr_decls(DataAddr, FirstIndent, LaterIndent,
			!N, !DeclSet, !IO)
	; Const = float_const(FloatVal) ->
		%
		% If floats are boxed, and the static ground terms
		% option is enabled, then for each float constant
		% which we might want to box we declare a static const
		% variable holding that constant.
		%
		globals__io_lookup_bool_option(unboxed_float,
			UnboxedFloat, !IO),
		globals__io_lookup_bool_option(static_ground_terms,
			StaticGroundTerms, !IO),
		(
			UnboxedFloat = no,
			StaticGroundTerms = yes
		->
			llds_out__float_literal_name(FloatVal, FloatName),
			FloatLabel = float_label(FloatName),
			( decl_set_is_member(FloatLabel, !.DeclSet) ->
				true
			;
				decl_set_insert(FloatLabel, !DeclSet),
				FloatString = c_util__make_float_literal(
					FloatVal),
				output_indent(FirstIndent, LaterIndent,
					!.N, !IO),
				!:N = !.N + 1,
				io__write_strings([
					"static const MR_Float ",
					"mercury_float_const_", FloatName,
					" = ", FloatString, ";\n"
				], !IO)
			)
		;
			true
		)
	;
		true
	).
output_rval_decls(unop(_, Rval), FirstIndent, LaterIndent,
		!N, !DeclSet, !IO) :-
	output_rval_decls(Rval, FirstIndent, LaterIndent, !N, !DeclSet, !IO).
output_rval_decls(binop(Op, Rval1, Rval2), FirstIndent, LaterIndent,
		!N, !DeclSet, !IO) :-
	output_rval_decls(Rval1, FirstIndent, LaterIndent, !N, !DeclSet, !IO),
	output_rval_decls(Rval2, FirstIndent, LaterIndent, !N, !DeclSet, !IO),
		%
		% If floats are boxed, and the static ground terms
		% option is enabled, then for each float constant
		% which we might want to box we declare a static const
		% variable holding that constant.
		%
	( c_util__float_op(Op, OpStr) ->
		globals__io_lookup_bool_option(unboxed_float, UnboxFloat, !IO),
		globals__io_lookup_bool_option(static_ground_terms,
			StaticGroundTerms, !IO),
		(
			UnboxFloat = no,
			StaticGroundTerms = yes,
			llds_out__float_const_binop_expr_name(Op, Rval1, Rval2,
				FloatName)
		->
			FloatLabel = float_label(FloatName),
			( decl_set_is_member(FloatLabel, !.DeclSet) ->
				true
			;
				decl_set_insert(FloatLabel, !DeclSet),
				output_indent(FirstIndent, LaterIndent, !.N,
					!IO),
				!:N = !.N + 1,
				io__write_string("static const ", !IO),
				output_llds_type(float, !IO),
				io__write_string(" mercury_float_const_", !IO),
				io__write_string(FloatName, !IO),
				io__write_string(" = ", !IO),
					% note that we just output the
					% expression here, and let the C
					% compiler evaluate it, rather than
					% evaluating it ourselves;
					% this avoids having to deal with some
					% nasty issues regarding floating point
					% accuracy when doing
					% cross-compilation.
				output_rval_as_type(Rval1, float, !IO),
				io__write_string(" ", !IO),
				io__write_string(OpStr, !IO),
				io__write_string(" ", !IO),
				output_rval_as_type(Rval2, float, !IO),
				io__write_string(";\n", !IO)
			)
		;
			true
		)
	;
		true
	).
output_rval_decls(mem_addr(MemRef), FirstIndent, LaterIndent,
		!N, !DeclSet, !IO) :-
	output_mem_ref_decls(MemRef, FirstIndent, LaterIndent,
		!N, !DeclSet, !IO).

:- pred output_rvals_decls(list(rval)::in, decl_set::in, decl_set::out,
	io::di, io::uo) is det.

output_rvals_decls(Rvals, !DeclSet, !IO) :-
	output_rvals_decls(Rvals, "", "", 0, _, !DeclSet, !IO).

:- pred output_rvals_decls(list(rval)::in, string::in, string::in,
	int::in, int::out, decl_set::in, decl_set::out, io::di, io::uo) is det.

output_rvals_decls([], _FirstIndent, _LaterIndent, !N, !DeclSet, !IO).
output_rvals_decls([Rval | Rvals], FirstIndent, LaterIndent,
		!N, !DeclSet, !IO) :-
	output_rval_decls(Rval, FirstIndent, LaterIndent, !N, !DeclSet, !IO),
	output_rvals_decls(Rvals, FirstIndent, LaterIndent, !N, !DeclSet, !IO).

:- pred output_mem_ref_decls(mem_ref::in, string::in, string::in,
	int::in, int::out, decl_set::in, decl_set::out, io::di, io::uo) is det.

output_mem_ref_decls(stackvar_ref(_), _, _, !N, !DeclSet, !IO).
output_mem_ref_decls(framevar_ref(_), _, _, !N, !DeclSet, !IO).
output_mem_ref_decls(heap_ref(Rval, _, _), FirstIndent, LaterIndent,
		!N, !DeclSet, !IO) :-
	output_rval_decls(Rval, FirstIndent, LaterIndent, !N, !DeclSet, !IO).

%-----------------------------------------------------------------------------%

% The following predicates are used to compute the names used for
% floating point static constants.

:- pred llds_out__float_const_expr_name(rval::in, string::out) is semidet.

% Given an rval, succeed iff it is a floating point constant expression;
% if so, return a name for that rval that is suitable for use in a C
% identifier. Different rvals must be given different names.

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
	FloatName0 = c_util__make_float_literal(Float),
	string__replace_all(FloatName0, ".", "pt", FloatName1),
	string__replace_all(FloatName1, "+", "plus", FloatName2),
	string__replace_all(FloatName2, "-", "neg", FloatName).

:- pred llds_out__float_op_name(binary_op::in, string::out) is semidet.

% succeed iff the binary operator is an operator whose return
% type is float; bind the output string to a name for that operator
% that is suitable for use in a C identifier

llds_out__float_op_name(float_plus, "plus").
llds_out__float_op_name(float_minus, "minus").
llds_out__float_op_name(float_times, "times").
llds_out__float_op_name(float_divide, "divide").

%-----------------------------------------------------------------------------%

:- func common_cell_get_type_num(common_cell_type_and_value) = int.

common_cell_get_type_num(TypeAndValue) = TypeNum :-
	(
		TypeAndValue = plain_type_and_value(TypeNum, _)
	;
		TypeAndValue = grouped_type_and_value(TypeNum, _)
	).

:- func common_cell_get_rvals(common_cell_type_and_value) = list(rval).

common_cell_get_rvals(TypeAndValue) = Rvals :-
	(
		TypeAndValue = plain_type_and_value(_, RvalsTypes),
		assoc_list__keys(RvalsTypes, Rvals)
	;
		TypeAndValue = grouped_type_and_value(_, Groups),
		RvalLists = list__map(common_group_get_rvals, Groups),
		list__condense(RvalLists, Rvals)
	).

:- func common_group_get_rvals(common_cell_arg_group) = list(rval).

common_group_get_rvals(common_cell_grouped_args(_, _, Rvals)) = Rvals.
common_group_get_rvals(common_cell_ungrouped_arg(_, Rval)) = [Rval].

%-----------------------------------------------------------------------------%

	% We output constant terms as follows:
	%
	%	struct <prefix>_common_type_<TypeNum> {		// Type
	%		...
	%	};
	%
	%	static const <prefix>_common_type_<TypeNum>
	%		<prefix>_common_<CellNum>;		// Decl
	%
	%	static const <prefix>_common_type_<TypeNum>
	%		<prefix>_common_<CellNum> = {		// Init
	%		...
	%	};
	%
	% Unless the term contains code addresses, and we don't have
	% static code addresses available, in which case we'll have
	% to initialize them dynamically, so we must omit both `const's
	% above.
	%
	% output_const_term_type outputs the first part above. The second
	% and third parts are output by output_const_term_decl_or_defn.

:- pred output_const_term_type(common_cell_type_and_value::in, module_name::in,
	string::in, string::in, int::in, int::out, io::di, io::uo) is det.

output_const_term_type(TypeAndValue, ModuleName, FirstIndent, LaterIndent,
		!N, !IO) :-
	output_indent(FirstIndent, LaterIndent, !.N, !IO),
	!:N = !.N + 1,
	io__write_string("struct ", !IO),
	TypeNum = common_cell_get_type_num(TypeAndValue),
	output_common_cell_type_name(ModuleName, TypeNum, !IO),
	io__write_string(" {\n", !IO),
	(
		TypeAndValue = plain_type_and_value(_, ArgsTypes),
		assoc_list__values(ArgsTypes, Types),
		output_cons_arg_types(Types, "\t", 1, !IO)
	;
		TypeAndValue = grouped_type_and_value(_, ArgGroups),
		output_cons_arg_group_types(ArgGroups, "\t", 1, !IO)
	),
	io__write_string("};\n", !IO).

:- pred output_const_term_decl_or_defn(common_cell_type_and_value::in,
	module_name::in, int::in, bool::in, bool::in,
	string::in, string::in, int::in, int::out, io::di, io::uo) is det.

output_const_term_decl_or_defn(TypeAndValue, ModuleName, CellNum, Exported,
		IsDefn, FirstIndent, LaterIndent, !N, !IO) :-
	output_indent(FirstIndent, LaterIndent, !.N, !IO),
	!:N = !.N + 1,
	(
		Exported = yes,
		io__write_string("const struct ", !IO)
	;
		Exported = no,
		io__write_string("static const struct ", !IO)
	),
	TypeNum = common_cell_get_type_num(TypeAndValue),
	output_common_cell_type_name(ModuleName, TypeNum, !IO),
	io__write_string(" ", !IO),
	VarDeclId = data_addr(ModuleName, common(CellNum, TypeNum)),
	output_decl_id(data_addr(VarDeclId), !IO),
	(
		IsDefn = no,
		io__write_string(";\n", !IO)
	;
		IsDefn = yes,
		io__write_string(" =\n{\n", !IO),
		(
			TypeAndValue = plain_type_and_value(_, ArgsTypes),
			output_cons_args(ArgsTypes, !IO)
		;
			TypeAndValue = grouped_type_and_value(_, ArgGroups),
			output_cons_arg_groups(ArgGroups, !IO)
		),
		io__write_string(LaterIndent, !IO),
		io__write_string("};\n", !IO)
	).

	% Return true if a data structure of the given type will eventually
	% have code addresses filled in inside it. Note that we can't just
	% test the data structure itself, since in the absence of static
	% code addresses the earlier passes will have replaced any code
	% addresses with dummy values that will have to be overridden with
	% the real code address at initialization time.

:- func data_addr_may_include_non_static_code_address(data_addr) = bool.

data_addr_may_include_non_static_code_address(data_addr(_, DataName)) =
	data_name_may_include_non_static_code_address(DataName).
data_addr_may_include_non_static_code_address(rtti_addr(RttiId)) =
	rtti_id_would_include_code_addr(RttiId).
data_addr_may_include_non_static_code_address(layout_addr(LayoutName)) =
	layout_name_would_include_code_addr(LayoutName).

:- func data_name_may_include_non_static_code_address(data_name) = bool.

% Common structures can include code addresses, but only in grades with
% static code addresses.
data_name_may_include_non_static_code_address(common(_, _)) =  no.
data_name_may_include_non_static_code_address(tabling_pointer(_)) = no.

:- pred output_decl_id(decl_id::in, io::di, io::uo) is det.

output_decl_id(common_type(ModuleName, TypeNum), !IO) :-
	output_common_cell_type_name(ModuleName, TypeNum, !IO).
output_decl_id(data_addr(DataAddr), !IO) :-
	output_data_addr(DataAddr, !IO).
output_decl_id(code_addr(_CodeAddress), !IO) :-
	error("output_decl_id: code_addr unexpected").
output_decl_id(float_label(_Label), !IO) :-
	error("output_decl_id: float_label unexpected").
output_decl_id(pragma_c_struct(_Name), !IO) :-
	error("output_decl_id: pragma_c_struct unexpected").
output_decl_id(type_info_like_struct(_Name), !IO) :-
	error("output_decl_id: type_info_like_struct unexpected").
output_decl_id(typeclass_constraint_struct(_Name), !IO) :-
	error("output_decl_id: class_constraint_struct unexpected").

:- pred output_cons_arg_types(list(llds_type)::in, string::in, int::in,
	io::di, io::uo) is det.

output_cons_arg_types([], _, _, !IO).
output_cons_arg_types([Type | Types], Indent, ArgNum, !IO) :-
	io__write_string(Indent, !IO),
	output_llds_type(Type, !IO),
	io__write_string(" f", !IO),
	io__write_int(ArgNum, !IO),
	io__write_string(";\n", !IO),
	output_cons_arg_types(Types, Indent, ArgNum + 1, !IO).

:- pred output_cons_arg_group_types(list(common_cell_arg_group)::in,
	string::in, int::in, io::di, io::uo) is det.

output_cons_arg_group_types([], _, _, !IO).
output_cons_arg_group_types([Group | Groups], Indent, ArgNum, !IO) :-
	io__write_string(Indent, !IO),
	(
		Group = common_cell_grouped_args(Type, ArraySize, _),
		output_llds_type(Type, !IO),
		io__write_string(" f", !IO),
		io__write_int(ArgNum, !IO),
		io__write_string("[", !IO),
		io__write_int(ArraySize, !IO),
		io__write_string("];\n", !IO)
	;
		Group = common_cell_ungrouped_arg(Type, _),
		output_llds_type(Type, !IO),
		io__write_string(" f", !IO),
		io__write_int(ArgNum, !IO),
		io__write_string(";\n", !IO)
	),
	output_cons_arg_group_types(Groups, Indent, ArgNum + 1, !IO).

	% Given an rval, figure out the type it would have as
	% an argument.  Normally that's the same as its usual type;
	% the exception is that for boxed floats, the type is data_ptr
	% (i.e. the type of the boxed value) rather than float
	% (the type of the unboxed value).

:- pred llds_out__rval_type_as_arg(rval::in, llds_type::out, io::di, io::uo)
	is det.

llds_out__rval_type_as_arg(Rval, ArgType, !IO) :-
	llds__rval_type(Rval, Type),
	globals__io_lookup_bool_option(unboxed_float, UnboxFloat, !IO),
	(
		Type = float,
		UnboxFloat = no
	->
		ArgType = data_ptr
	;
		ArgType = Type
	).

	% Same as output_llds_type, but will put parentheses
	% around the llds_type.
:- pred output_llds_type_cast(llds_type::in, io::di, io::uo) is det.

output_llds_type_cast(LLDSType, !IO) :-
	io__write_string("(", !IO),
	output_llds_type(LLDSType, !IO),
	io__write_string(") ", !IO).

:- pred output_llds_type(llds_type::in, io::di, io::uo) is det.

output_llds_type(int_least8, !IO) :-
	io__write_string("MR_int_least8_t", !IO).
output_llds_type(uint_least8, !IO) :-
	io__write_string("MR_uint_least8_t", !IO).
output_llds_type(int_least16, !IO) :-
	io__write_string("MR_int_least16_t", !IO).
output_llds_type(uint_least16, !IO) :-
	io__write_string("MR_uint_least16_t", !IO).
output_llds_type(int_least32, !IO) :-
	io__write_string("MR_int_least32_t", !IO).
output_llds_type(uint_least32, !IO) :-
	io__write_string("MR_uint_least32_t", !IO).
output_llds_type(bool, !IO) :-
	io__write_string("MR_Integer", !IO).
output_llds_type(integer, !IO) :-
	io__write_string("MR_Integer", !IO).
output_llds_type(unsigned, !IO) :-
	io__write_string("MR_Unsigned", !IO).
output_llds_type(float, !IO) :-
	io__write_string("MR_Float", !IO).
output_llds_type(word, !IO) :-
	io__write_string("MR_Word", !IO).
output_llds_type(string, !IO) :-
	io__write_string("MR_String", !IO).
output_llds_type(data_ptr, !IO) :-
	io__write_string("MR_Word *", !IO).
output_llds_type(code_ptr, !IO) :-
	io__write_string("MR_Code *", !IO).

	% Output the arguments, each on its own line prefixing with Indent,
	% and with a cast appropriate to its type if necessary.

:- pred output_cons_args(assoc_list(rval, llds_type)::in, io::di, io::uo)
	is det.

output_cons_args([], !IO).
output_cons_args([Rval - Type | RvalsTypes], !IO) :-
	(
		direct_field_int_constant(Type) = yes,
		Rval = const(int_const(N))
	->
		output_int_const(N, Type, !IO)
	;
		output_rval_as_type(Rval, Type, !IO)
	),
	( RvalsTypes \= [] ->
		io__write_string(",\n", !IO),
		output_cons_args(RvalsTypes, !IO)
	;
		io__write_string("\n", !IO)
	).

:- pred output_cons_arg_groups(list(common_cell_arg_group)::in, io::di, io::uo)
	is det.

output_cons_arg_groups([], !IO).
output_cons_arg_groups([Group | Groups], !IO) :-
	(
		Group = common_cell_grouped_args(Type, _, Rvals),
		io__write_string("{\n", !IO),
		(
			direct_field_int_constant(Type) = yes,
			list__map(project_int_constant, Rvals, Ints)
		->
			Check = check_int_const_sizes,
			(
				Check = no,
				output_cons_arg_group_ints(Ints, !IO)
			;
				Check = yes,
				output_cons_arg_group_ints_check(Ints, Type,
					!IO)
			)
		;
			output_cons_arg_group_elements(Type, Rvals, !IO)
		),
		io__write_string("}", !IO)
	;
		Group = common_cell_ungrouped_arg(Type, Rval),
		(
			direct_field_int_constant(Type) = yes,
			project_int_constant(Rval, Int)
		->
			output_int_const(Int, Type, !IO)
		;
			output_rval_as_type(Rval, Type, !IO)
		)
	),
	( Groups \= [] ->
		io__write_string(",\n", !IO),
		output_cons_arg_groups(Groups, !IO)
	;
		io__write_string("\n", !IO)
	).

:- pred output_cons_arg_group_elements(llds_type::in, list(rval)::in,
	io::di, io::uo) is det.

output_cons_arg_group_elements(_, [], !IO).
output_cons_arg_group_elements(Type, [Rval | Rvals], !IO) :-
	output_rval_as_type(Rval, Type, !IO),
	( Rvals \= [] ->
		io__write_string(",\n", !IO),
		output_cons_arg_group_elements(Type, Rvals, !IO)
	;
		io__write_string("\n", !IO)
	).

:- pred output_cons_arg_group_ints(list(int)::in, io::di, io::uo) is det.

output_cons_arg_group_ints([], !IO).
output_cons_arg_group_ints([Int | Ints], !IO) :-
	io__write_int(Int, !IO),
	( Ints \= [] ->
		io__write_string(",\n", !IO),
		output_cons_arg_group_ints(Ints, !IO)
	;
		io__write_string("\n", !IO)
	).

:- pred output_cons_arg_group_ints_check(list(int)::in, llds_type::in,
	io::di, io::uo) is det.

output_cons_arg_group_ints_check([], _, !IO).
output_cons_arg_group_ints_check([Int | Ints], Type, !IO) :-
	output_int_const(Int, Type, !IO),
	( Ints \= [] ->
		io__write_string(",\n", !IO),
		output_cons_arg_group_ints_check(Ints, Type, !IO)
	;
		io__write_string("\n", !IO)
	).

:- pred project_int_constant(rval::in, int::out) is semidet.

project_int_constant(const(int_const(N)), N).

:- func check_int_const_sizes = bool.
:- pragma inline(check_int_const_sizes/0).

% If you this to `yes', we will test all integer constants places into static
% data structures to see if they fit into the space allocated for them.

check_int_const_sizes = no.

:- pred output_int_const(int::in, llds_type::in, io::di, io::uo) is det.
:- pragma inline(output_int_const/4).

output_int_const(N, Type, !IO) :-
	Check = check_int_const_sizes,
	(
		Check = yes,
		( ok_int_const(N, Type) ->
			io__write_int(N, !IO)
		;
			error("output_int_const: constant does not fit in type")
		)
	;
		Check = no,
		io__write_int(N, !IO)
	).

:- pred ok_int_const(int::in, llds_type::in) is semidet.
:- pragma inline(ok_int_const/2).

ok_int_const(N, int_least8) :-
	-128 =< N, N < 128.
ok_int_const(N, uint_least8) :-
	0 =< N, N < 256.
ok_int_const(N, int_least16) :-
	-32768 =< N, N < 32768.
ok_int_const(N, uint_least16) :-
	0 =< N, N < 65536.
ok_int_const(_N, int_least32).
ok_int_const(_N, uint_least32).
ok_int_const(_N, bool) :-
	error("ok_int_const: not integer constant").
ok_int_const(_N, integer).
ok_int_const(_N, unsigned).
ok_int_const(_, float) :-
	error("ok_int_const: not integer constant").
ok_int_const(_, word) :-
	error("ok_int_const: not integer constant").
ok_int_const(_, string) :-
	error("ok_int_const: not integer constant").
ok_int_const(_, data_ptr) :-
	error("ok_int_const: not integer constant").
ok_int_const(_, code_ptr) :-
	error("ok_int_const: not integer constant").

%-----------------------------------------------------------------------------%

% output_lval_decls(Lval, ...) outputs the declarations of any
% static constants, etc. that need to be declared before
% output_lval(Lval) is called.

:- pred output_lval_decls(lval::in, decl_set::in, decl_set::out,
	io::di, io::uo) is det.

output_lval_decls(Lval, !DeclSet, !IO) :-
	output_lval_decls(Lval, "", "", 0, _, !DeclSet, !IO).

:- pred output_lval_decls(lval::in, string::in, string::in, int::in, int::out,
	decl_set::in, decl_set::out, io::di, io::uo) is det.

output_lval_decls(field(_, Rval, FieldNum), FirstIndent, LaterIndent,
		!N, !DeclSet, !IO) :-
	output_rval_decls(Rval, FirstIndent, LaterIndent, !N, !DeclSet, !IO),
	output_rval_decls(FieldNum, FirstIndent, LaterIndent,
		!N, !DeclSet, !IO).
output_lval_decls(reg(_, _), _, _, !N, !DeclSet, !IO).
output_lval_decls(stackvar(_), _, _, !N, !DeclSet, !IO).
output_lval_decls(framevar(_), _, _, !N, !DeclSet, !IO).
output_lval_decls(succip, _, _, !N, !DeclSet, !IO).
output_lval_decls(maxfr, _, _, !N, !DeclSet, !IO).
output_lval_decls(curfr, _, _, !N, !DeclSet, !IO).
output_lval_decls(succfr(Rval), FirstIndent, LaterIndent, !N, !DeclSet, !IO) :-
	output_rval_decls(Rval, FirstIndent, LaterIndent, !N, !DeclSet, !IO).
output_lval_decls(prevfr(Rval), FirstIndent, LaterIndent, !N, !DeclSet, !IO) :-
	output_rval_decls(Rval, FirstIndent, LaterIndent, !N, !DeclSet, !IO).
output_lval_decls(redofr(Rval), FirstIndent, LaterIndent, !N, !DeclSet, !IO) :-
	output_rval_decls(Rval, FirstIndent, LaterIndent, !N, !DeclSet, !IO).
output_lval_decls(redoip(Rval), FirstIndent, LaterIndent, !N, !DeclSet, !IO) :-
	output_rval_decls(Rval, FirstIndent, LaterIndent, !N, !DeclSet, !IO).
output_lval_decls(succip(Rval), FirstIndent, LaterIndent, !N, !DeclSet, !IO) :-
	output_rval_decls(Rval, FirstIndent, LaterIndent, !N, !DeclSet, !IO).
output_lval_decls(hp, _, _, !N, !DeclSet, !IO).
output_lval_decls(sp, _, _, !N, !DeclSet, !IO).
output_lval_decls(lvar(_), _, _, !N, !DeclSet, !IO).
output_lval_decls(temp(_, _), _, _, !N, !DeclSet, !IO).
output_lval_decls(mem_ref(Rval), FirstIndent, LaterIndent,
		!N, !DeclSet, !IO) :-
	output_rval_decls(Rval, FirstIndent, LaterIndent, !N, !DeclSet, !IO).

output_code_addr_decls(CodeAddress, !DeclSet, !IO) :-
	output_code_addr_decls(CodeAddress, "", "", 0, _, !DeclSet, !IO).

:- pred output_code_addr_decls(code_addr::in, string::in, string::in,
	int::in, int::out, decl_set::in, decl_set::out, io::di, io::uo) is det.

output_code_addr_decls(CodeAddress, FirstIndent, LaterIndent, !N, !DeclSet,
		!IO) :-
	( decl_set_is_member(code_addr(CodeAddress), !.DeclSet) ->
		true
	;
		decl_set_insert(code_addr(CodeAddress), !DeclSet),
		need_code_addr_decls(CodeAddress, NeedDecl, !IO),
		( NeedDecl = yes ->
			output_indent(FirstIndent, LaterIndent, !.N, !IO),
			!:N = !.N + 1,
			output_code_addr_decls(CodeAddress, !IO)
		;
			true
		)
	).

:- pred need_code_addr_decls(code_addr::in, bool::out, io::di, io::uo) is det.

need_code_addr_decls(label(Label), Need, !IO) :-
	(
		Label = entry(exported, _),
		Need = yes
	;
		Label = entry(local, _),
		Need = yes
	;
		Label = entry(c_local, _),
		Need = no
	;
		Label = internal(_, _),
		Need = no
	).
need_code_addr_decls(imported(_), yes, !IO).
need_code_addr_decls(succip, no, !IO).
need_code_addr_decls(do_succeed(_), no, !IO).
need_code_addr_decls(do_redo, NeedDecl, !IO) :-
	globals__io_lookup_bool_option(use_macro_for_redo_fail, UseMacro, !IO),
	(
		UseMacro = yes,
		NeedDecl = no
	;
		UseMacro = no,
		NeedDecl = yes
	).
need_code_addr_decls(do_fail, NeedDecl, !IO) :-
	globals__io_lookup_bool_option(use_macro_for_redo_fail, UseMacro, !IO),
	(
		UseMacro = yes,
		NeedDecl = no
	;
		UseMacro = no,
		NeedDecl = yes
	).
need_code_addr_decls(do_trace_redo_fail_shallow, yes, !IO).
need_code_addr_decls(do_trace_redo_fail_deep, yes, !IO).
need_code_addr_decls(do_call_closure, yes, !IO).
need_code_addr_decls(do_call_class_method, yes, !IO).
need_code_addr_decls(do_not_reached, yes, !IO).

:- pred output_code_addr_decls(code_addr::in, io::di, io::uo) is det.

output_code_addr_decls(label(Label), !IO) :-
	output_label_as_code_addr_decls(Label, !IO).
output_code_addr_decls(imported(ProcLabel), !IO) :-
	io__write_string("MR_decl_entry(", !IO),
	output_proc_label(ProcLabel, no, !IO),
	io__write_string(");\n", !IO).
output_code_addr_decls(succip, !IO).
output_code_addr_decls(do_succeed(_), !IO).
output_code_addr_decls(do_redo, !IO) :-
	globals__io_lookup_bool_option(use_macro_for_redo_fail, UseMacro, !IO),
	(
		UseMacro = yes
	;
		UseMacro = no,
		io__write_string("MR_declare_entry(", !IO),
		io__write_string("MR_do_redo", !IO),
		io__write_string(");\n", !IO)
	).
output_code_addr_decls(do_fail, !IO) :-
	globals__io_lookup_bool_option(use_macro_for_redo_fail, UseMacro, !IO),
	(
		UseMacro = yes
	;
		UseMacro = no,
		io__write_string("MR_declare_entry(", !IO),
		io__write_string("MR_do_fail", !IO),
		io__write_string(");\n", !IO)
	).
output_code_addr_decls(do_trace_redo_fail_shallow, !IO) :-
	io__write_string("MR_declare_entry(MR_do_trace_redo_fail_shallow);\n",
		!IO).
output_code_addr_decls(do_trace_redo_fail_deep, !IO) :-
	io__write_string("MR_declare_entry(MR_do_trace_redo_fail_deep);\n",
		!IO).
output_code_addr_decls(do_call_closure, !IO) :-
	io__write_string(
		"MR_declare_entry(mercury__do_call_closure_compact);\n", !IO).
output_code_addr_decls(do_call_class_method, !IO) :-
	io__write_string(
		"MR_declare_entry(mercury__do_call_class_method_compact);\n",
		!IO).
output_code_addr_decls(do_not_reached, !IO) :-
	io__write_string("MR_declare_entry(MR_do_not_reached);\n", !IO).

:- pred output_label_as_code_addr_decls(label::in, io::di, io::uo) is det.

output_label_as_code_addr_decls(entry(exported, ProcLabel), !IO) :-
	io__write_string("MR_decl_entry(", !IO),
	output_label(entry(exported, ProcLabel), no, !IO),
	io__write_string(");\n", !IO).
output_label_as_code_addr_decls(entry(local, ProcLabel), !IO) :-
	globals__io_lookup_bool_option(split_c_files, SplitFiles, !IO),
	(
		SplitFiles = no
	;
		SplitFiles = yes,
		io__write_string("MR_decl_entry(", !IO),
		output_label(entry(local, ProcLabel), no, !IO),
		io__write_string(");\n", !IO)
	).
output_label_as_code_addr_decls(entry(c_local, _), !IO).
output_label_as_code_addr_decls(internal(_, _), !IO).

output_data_addr_decls(DataAddr, !DeclSet, !IO) :-
	output_data_addr_decls(DataAddr, "", "", 0, _, !DeclSet, !IO).

output_data_addr_decls(DataAddr, FirstIndent, LaterIndent, !N, !DeclSet,
		!IO) :-
	( decl_set_is_member(data_addr(DataAddr), !.DeclSet) ->
		true
	;
		decl_set_insert(data_addr(DataAddr), !DeclSet),
		output_data_addr_decls_2(DataAddr, FirstIndent, LaterIndent,
			!N, !IO)
	).

:- pred output_data_addr_decls_2(data_addr::in, string::in, string::in,
	int::in, int::out, io::di, io::uo) is det.

output_data_addr_decls_2(DataAddr, FirstIndent, LaterIndent, !N, !IO) :-
	output_indent(FirstIndent, LaterIndent, !.N, !IO),
	!:N = !.N + 1,
	(
		DataAddr = data_addr(ModuleName, DataVarName),
		output_data_addr_storage_type_name(ModuleName, DataVarName, no,
			LaterIndent, !IO),
		io__write_string(";\n", !IO)
	;
		DataAddr = rtti_addr(RttiId),
		output_rtti_id_storage_type_name_no_decl(RttiId, no, !IO),
		io__write_string(";\n", !IO)
	;
		DataAddr = layout_addr(LayoutName),
		output_layout_name_storage_type_name(LayoutName, no, !IO),
		io__write_string(";\n", !IO)
	).

output_data_addrs_decls([], _, _, !N, !DeclSet, !IO).
output_data_addrs_decls([DataAddr | DataAddrs], FirstIndent, LaterIndent, !N,
		!DeclSet, !IO) :-
	output_data_addr_decls(DataAddr, FirstIndent, LaterIndent, !N,
		!DeclSet, !IO),
	output_data_addrs_decls(DataAddrs, FirstIndent, LaterIndent, !N,
		!DeclSet, !IO).

c_data_linkage_string(Globals, DefaultLinkage, StaticEvenIfSplit, BeingDefined)
		= LinkageStr :-
	globals__lookup_bool_option(Globals, split_c_files, SplitFiles),
	(
		( DefaultLinkage = extern
		; SplitFiles = yes, StaticEvenIfSplit = no
		)
	->
		(
			BeingDefined = yes,
			LinkageStr = ""
		;
			BeingDefined = no,
			LinkageStr = "extern "
		)
	;
		%
		% Previously we used to always write `extern' here, but
		% declaring something `extern' and then later defining it as
		% `static' causes undefined behavior -- on many systems, it
		% works, but on some systems such as RS/6000s running AIX
		% it results in link errors.
		%
		LinkageStr = "static "
	).

c_data_const_string(Globals, InclCodeAddr, ConstStr) :-
	(
		InclCodeAddr = yes,
		globals__have_static_code_addresses(Globals, no)
	->
		ConstStr = ""
	;
		ConstStr = "const "
	).

	% This predicate outputs the storage class, type and name
	% of the variable specified by the first two arguments.
	% The third argument should be true if the variable is being
	% defined, and false if it is only being declared (since the
	% storage class "extern" is needed only on declarations).

:- pred output_data_addr_storage_type_name(module_name::in, data_name::in,
	bool::in, string::in, io::di, io::uo) is det.

output_data_addr_storage_type_name(ModuleName, DataVarName, BeingDefined,
		LaterIndent, !IO) :-
	data_name_linkage(DataVarName, Linkage),
	globals__io_get_globals(Globals, !IO),
	LinkageStr = c_data_linkage_string(Globals, Linkage, no, BeingDefined),
	io__write_string(LinkageStr, !IO),

	InclCodeAddr = data_name_may_include_non_static_code_address(
		DataVarName),
	c_data_const_string(Globals, InclCodeAddr, ConstStr),
	io__write_string(ConstStr, !IO),

	io__write_string("struct ", !IO),
	output_data_addr(ModuleName, DataVarName, !IO),
	io__write_string("_struct\n", !IO),
	io__write_string(LaterIndent, !IO),
	io__write_string("\t", !IO),
	output_data_addr(ModuleName, DataVarName, !IO).

:- pred data_name_linkage(data_name::in, linkage::out) is det.

data_name_linkage(common(_, _),       static).
data_name_linkage(tabling_pointer(_), static).

%-----------------------------------------------------------------------------%

:- pred output_indent(string::in, string::in, int::in, io::di, io::uo) is det.

output_indent(FirstIndent, LaterIndent, N0, !IO) :-
	( N0 > 0 ->
		io__write_string(LaterIndent, !IO)
	;
		io__write_string(FirstIndent, !IO)
	).

%-----------------------------------------------------------------------------%

:- pred maybe_output_update_prof_counter(label::in,
	pair(label, bintree_set(label))::in, io::di, io::uo) is det.

maybe_output_update_prof_counter(Label, CallerLabel - ContLabelSet, !IO) :-
	% If ProfileTime is no, the definition of MR_update_prof_current_proc
	% is empty anyway.
	globals__io_lookup_bool_option(profile_time, ProfileTime, !IO),
	(
		bintree_set__is_member(Label, ContLabelSet),
		ProfileTime = yes
	->
		io__write_string("\tMR_update_prof_current_proc(MR_LABEL_AP(",
			!IO),
		output_label(CallerLabel, no, !IO),
		io__write_string("));\n", !IO)
	;
		true
	).

%-----------------------------------------------------------------------------%

:- pred output_goto(code_addr::in, label::in, io::di, io::uo) is det.

	% Note that we do some optimization here:
	% instead of always outputting `MR_GOTO(<label>)', we
	% output different things for each different kind of label.

output_goto(label(Label), CallerLabel, !IO) :-
	(
		Label = entry(exported, _),
		globals__io_lookup_bool_option(profile_calls, ProfileCalls,
			!IO),
		(
			ProfileCalls = yes,
			io__write_string("MR_tailcall(", !IO),
			output_label_as_code_addr(Label, !IO),
			io__write_string(",\n\t\t", !IO),
			output_label_as_code_addr(CallerLabel, !IO),
			io__write_string(");\n", !IO)
		;
			ProfileCalls = no,
			io__write_string("MR_np_tailcall_ent(", !IO),
			output_label(Label, no, !IO),
			io__write_string(");\n", !IO)
		)
	;
		Label = entry(local, _),
		globals__io_lookup_bool_option(profile_calls, ProfileCalls,
			!IO),
		(
			ProfileCalls = yes,
			io__write_string("MR_tailcall(", !IO),
			output_label_as_code_addr(Label, !IO),
			io__write_string(",\n\t\t", !IO),
			output_label_as_code_addr(CallerLabel, !IO),
			io__write_string(");\n", !IO)
		;
			ProfileCalls = no,
			io__write_string("MR_np_tailcall_ent(", !IO),
			output_label(Label, no, !IO),
			io__write_string(");\n", !IO)
		)
	;
		Label = entry(c_local, _),
		globals__io_lookup_bool_option(profile_calls, ProfileCalls,
			!IO),
		(
			ProfileCalls = yes,
			io__write_string("MR_localtailcall(", !IO),
			output_label(Label, !IO),
			io__write_string(",\n\t\t", !IO),
			output_label_as_code_addr(CallerLabel, !IO),
			io__write_string(");\n", !IO)
		;
			ProfileCalls = no,
			io__write_string("MR_np_localtailcall(", !IO),
			output_label(Label, no, !IO),
			io__write_string(");\n", !IO)
		)
	;
		Label = internal(_, _),
		io__write_string("MR_GOTO_LAB(", !IO),
		output_label(Label, no, !IO),
		io__write_string(");\n", !IO)
	).
output_goto(imported(ProcLabel), CallerLabel, !IO) :-
	globals__io_lookup_bool_option(profile_calls, ProfileCalls, !IO),
	(
		ProfileCalls = yes,
		io__write_string("MR_tailcall(MR_ENTRY(", !IO),
		output_proc_label(ProcLabel, !IO),
		io__write_string("),\n\t\t", !IO),
		output_label_as_code_addr(CallerLabel, !IO),
		io__write_string(");\n", !IO)
	;
		ProfileCalls = no,
		io__write_string("MR_np_tailcall_ent(", !IO),
		output_proc_label(ProcLabel, no, !IO),
		io__write_string(");\n", !IO)
	).
output_goto(succip, _, !IO) :-
	io__write_string("MR_proceed();\n", !IO).
output_goto(do_succeed(Last), _, !IO) :-
	(
		Last = no,
		io__write_string("MR_succeed();\n", !IO)
	;
		Last = yes,
		io__write_string("MR_succeed_discard();\n", !IO)
	).
output_goto(do_redo, _, !IO) :-
	globals__io_lookup_bool_option(use_macro_for_redo_fail, UseMacro, !IO),
	(
		UseMacro = yes,
		io__write_string("MR_redo();\n", !IO)
	;
		UseMacro = no,
		io__write_string("MR_GOTO(MR_ENTRY(MR_do_redo));\n", !IO)
	).
output_goto(do_fail, _, !IO) :-
	globals__io_lookup_bool_option(use_macro_for_redo_fail, UseMacro, !IO),
	(
		UseMacro = yes,
		io__write_string("MR_fail();\n", !IO)
	;
		UseMacro = no,
		io__write_string("MR_GOTO(MR_ENTRY(MR_do_fail));\n", !IO)
	).
output_goto(do_trace_redo_fail_shallow, _, !IO) :-
	io__write_string("MR_GOTO(MR_ENTRY(MR_do_trace_redo_fail_shallow));\n",
		!IO).
output_goto(do_trace_redo_fail_deep, _, !IO) :-
	io__write_string("MR_GOTO(MR_ENTRY(MR_do_trace_redo_fail_deep));\n",
		!IO).
output_goto(do_call_closure, CallerLabel, !IO) :-
	% see comment in output_call for why we use `noprof_' etc. here
	io__write_string("MR_set_prof_ho_caller_proc(", !IO),
	output_label_as_code_addr(CallerLabel, !IO),
	io__write_string(");\n\t", !IO),
	io__write_string("MR_np_tailcall_ent(do_call_closure_compact);\n",
		!IO).
output_goto(do_call_class_method, CallerLabel, !IO) :-
	% see comment in output_call for why we use `noprof_' etc. here
	io__write_string("MR_set_prof_ho_caller_proc(", !IO),
	output_label_as_code_addr(CallerLabel, !IO),
	io__write_string(");\n\t", !IO),
	io__write_string("MR_np_tailcall_ent(do_call_class_method_compact);\n",
		!IO).
output_goto(do_not_reached, CallerLabel, !IO) :-
	io__write_string("MR_tailcall(MR_ENTRY(MR_do_not_reached),\n\t\t",
		!IO),
	output_label_as_code_addr(CallerLabel, !IO),
	io__write_string(");\n", !IO).

	% Note that we also do some optimization here by outputting `localcall'
	% rather than `call' for calls to local labels, or `call_localret' for
	% calls which return to local labels (i.e. most of them).
	%
	% We also reduce the size of the output by emitting shorthand forms of
	% the relevant macros when possible, allowing those shorthand macros
	% to apply mercury__ prefixes and possible MR_ENTRY() wrappers.

:- pred output_call(code_addr::in, code_addr::in, label::in, io::di, io::uo)
	is det.

output_call(Target, Continuation, CallerLabel, !IO) :-
	io__write_string("\t", !IO),
	% For profiling, we ignore calls to do_call_closure
	% and do_call_class_method, because in general they
	% lead to cycles in the call graph that screw up the
	% profile.  By generating a `noprof_call' rather than
	% a `call', we ensure that time spent inside those
	% routines is credited to the caller, rather than to
	% do_call_closure or do_call_class_method itself.
	% But if we do use a noprof_call, we need to set
	% MR_prof_ho_caller_proc, so that the callee knows
	% which proc it has been called from.
	(
		( Target = do_call_closure
		; Target = do_call_class_method
		)
	->
		ProfileCall = no,
		io__write_string("MR_set_prof_ho_caller_proc(", !IO),
		output_label_as_code_addr(CallerLabel, !IO),
		io__write_string(");\n\t", !IO)
	;
		globals__io_lookup_bool_option(profile_calls, ProfileCall,
			!IO)
	),
	(
		Target = label(Label),
		% We really shouldn't be calling internal labels ...
		label_is_external_to_c_module(Label) = no
	->
		(
			ProfileCall = yes,
			io__write_string("MR_localcall(", !IO),
			output_label(Label, !IO),
			io__write_string(",\n\t\t", !IO),
			output_code_addr(Continuation, !IO)
		;
			ProfileCall = no,
			code_addr_to_string_base(Continuation, BaseStr,
				NeedsPrefix, Wrapper),
			(
				NeedsPrefix = no,
				io__write_string("MR_noprof_localcall(", !IO),
				output_label(Label, no, !IO),
				io__write_string(",\n\t\t", !IO),
				io__write_string(BaseStr, !IO),
				output_code_addr_from_pieces(BaseStr,
					NeedsPrefix, Wrapper, !IO)
			;
				NeedsPrefix = yes,
				Wrapper = entry,
				io__write_string("MR_np_localcall_ent(", !IO),
				output_label(Label, no, !IO),
				io__write_string(",\n\t\t", !IO),
				io__write_string(BaseStr, !IO)
			;
				NeedsPrefix = yes,
				Wrapper = label,
				io__write_string("MR_np_localcall_lab(", !IO),
				output_label(Label, no, !IO),
				io__write_string(",\n\t\t", !IO),
				io__write_string(BaseStr, !IO)
			;
				NeedsPrefix = yes,
				Wrapper = none,
				io__write_string("MR_np_localcall(", !IO),
				output_label(Label, no, !IO),
				io__write_string(",\n\t\t", !IO),
				output_code_addr_from_pieces(BaseStr,
					NeedsPrefix, Wrapper, !IO)
			)
		)
	;
		Continuation = label(ContLabel),
		label_is_external_to_c_module(ContLabel) = no
	->
		(
			ProfileCall = yes,
			io__write_string("MR_call_localret(", !IO),
			output_code_addr(Target, !IO),
			io__write_string(",\n\t\t", !IO),
			output_label(ContLabel, !IO)
		;
			ProfileCall = no,
			code_addr_to_string_base(Target, BaseStr,
				NeedsPrefix, Wrapper),
			(
				NeedsPrefix = no,
				io__write_string("MR_noprof_call_localret(",
					!IO),
				output_code_addr_from_pieces(BaseStr,
					NeedsPrefix, Wrapper, !IO),
				io__write_string(",\n\t\t", !IO),
				output_label(ContLabel, !IO)
			;
				NeedsPrefix = yes,
				Wrapper = entry,
				io__write_string("MR_np_call_localret_ent(",
					!IO),
				io__write_string(BaseStr, !IO),
				io__write_string(",\n\t\t", !IO),
				output_label(ContLabel, no, !IO)
			;
				NeedsPrefix = yes,
				Wrapper = label,
				% We should never get here; the conditions
				% that lead here in this switch should have
				% been caught by the first if-then-else
				% condition that tests Target.
				error("output_call: calling label")
			;
				NeedsPrefix = yes,
				Wrapper = none,
				io__write_string("MR_np_call_localret(",
					!IO),
				output_code_addr_from_pieces(BaseStr,
					NeedsPrefix, Wrapper, !IO),
				io__write_string(",\n\t\t", !IO),
				output_label(ContLabel, no, !IO)
			)
		)
	;
		(
			ProfileCall = yes,
			io__write_string("MR_call(", !IO)
		;
			ProfileCall = no,
			io__write_string("MR_noprof_call(", !IO)
		),
		output_code_addr(Target, !IO),
		io__write_string(",\n\t\t", !IO),
		output_code_addr(Continuation, !IO)
	),
	( ProfileCall = yes ->
		io__write_string(",\n\t\t", !IO),
		output_label_as_code_addr(CallerLabel, !IO)
	;
		true
	),
	io__write_string(");\n", !IO).

output_code_addr(CodeAddr, !IO) :-
	code_addr_to_string_base(CodeAddr, BaseStr, NeedsPrefix, Wrapper),
	output_code_addr_from_pieces(BaseStr, NeedsPrefix, Wrapper, !IO).

:- type wrapper --->	entry ; label ; none.

:- pred output_code_addr_from_pieces(string::in, bool::in, wrapper::in,
	io::di, io::uo) is det.

output_code_addr_from_pieces(BaseStr, NeedsPrefix, Wrapper, !IO) :-
	(
		Wrapper = none,
		(
			NeedsPrefix = yes,
			io__write_string(mercury_label_prefix, !IO)
		;
			NeedsPrefix = no
		),
		io__write_string(BaseStr, !IO)
	;
		Wrapper = entry,
		(
			NeedsPrefix = yes,
			% The _AP version of the macro adds the prefix.
			io__write_string("MR_ENTRY_AP(", !IO),
			io__write_string(BaseStr, !IO),
			io__write_string(")", !IO)
		;
			NeedsPrefix = no,
			io__write_string("MR_ENTRY(", !IO),
			io__write_string(BaseStr, !IO),
			io__write_string(")", !IO)
		)
	;
		Wrapper = label,
		(
			NeedsPrefix = yes,
			% The _AP version of the macro adds the prefix.
			io__write_string("MR_LABEL_AP(", !IO),
			io__write_string(BaseStr, !IO),
			io__write_string(")", !IO)
		;
			NeedsPrefix = no,
			io__write_string("MR_LABEL(", !IO),
			io__write_string(BaseStr, !IO),
			io__write_string(")", !IO)
		)
	).

:- pred code_addr_to_string_base(code_addr::in, string::out,
	bool::out, wrapper::out) is det.

code_addr_to_string_base(label(Label), BaseStr, yes, Wrapper) :-
	BaseStr = label_to_c_string(Label, no),
	IsExternal = label_is_external_to_c_module(Label),
	(
		IsExternal = yes,
		Wrapper = entry
	;
		IsExternal = no,
		Wrapper = label
	).
code_addr_to_string_base(imported(ProcLabel), BaseStr, yes, entry) :-
	BaseStr = proc_label_to_c_string(ProcLabel, no).
code_addr_to_string_base(succip, "MR_succip", no, none).
code_addr_to_string_base(do_succeed(Last), BaseStr, no, entry) :-
	(
		Last = no,
		BaseStr = "MR_do_succeed"
	;
		Last = yes,
		BaseStr = "MR_do_last_succeed"
	).
code_addr_to_string_base(do_redo, "MR_do_redo", no, entry).
code_addr_to_string_base(do_fail, "MR_do_fail", no, entry).
code_addr_to_string_base(do_trace_redo_fail_shallow, BaseStr, no, entry) :-
	BaseStr = "MR_do_trace_redo_fail_shallow".
code_addr_to_string_base(do_trace_redo_fail_deep, BaseStr, no, entry) :-
	BaseStr = "MR_do_trace_redo_fail_deep".
code_addr_to_string_base(do_call_closure, BaseStr, no, entry) :-
	BaseStr = "mercury__do_call_closure_compact".
code_addr_to_string_base(do_call_class_method, BaseStr, no, entry) :-
	BaseStr = "mercury__do_call_class_method_compact".
code_addr_to_string_base(do_not_reached, BaseStr, no, entry) :-
	BaseStr = "MR_do_not_reached".

	% Output a maybe data address, with a `no' meaning NULL.

:- pred output_maybe_data_addr(maybe(data_addr)::in, io::di, io::uo) is det.

output_maybe_data_addr(MaybeDataAddr, !IO) :-
	(
		MaybeDataAddr = yes(DataAddr),
		output_data_addr(DataAddr, !IO)
	;
		MaybeDataAddr = no,
		io__write_string("NULL", !IO)
	).

	% Output a list of maybe data addresses, with a `no' meaning NULL.

:- pred output_maybe_data_addrs(list(maybe(data_addr))::in, io::di, io::uo)
	is det.

output_maybe_data_addrs([], !IO).
output_maybe_data_addrs([MaybeDataAddr | MaybeDataAddrs], !IO) :-
	io__write_string("\t", !IO),
	io__write_list([MaybeDataAddr | MaybeDataAddrs], ",\n\t",
		output_maybe_data_addr, !IO),
	io__write_string("\n", !IO).

	% Output a list of data addresses.

:- pred output_data_addrs(list(data_addr)::in, io::di, io::uo) is det.

output_data_addrs([], !IO).
output_data_addrs([DataAddr | DataAddrs], !IO) :-
	io__write_string("\t", !IO),
	io__write_list([DataAddr | DataAddrs], ",\n\t", output_data_addr, !IO),
	io__write_string("\n", !IO).

	% Output a data address.

output_data_addr(data_addr(ModuleName, DataName), !IO) :-
	output_data_addr(ModuleName, DataName, !IO).
output_data_addr(rtti_addr(RttiId), !IO) :-
	output_rtti_id(RttiId, !IO).
output_data_addr(layout_addr(LayoutName), !IO) :-
	output_layout_name(LayoutName, !IO).

:- pred output_data_addr(module_name::in, data_name::in, io::di, io::uo)
	is det.

output_data_addr(ModuleName, VarName, !IO) :-
	(
		VarName = common(CellNum, _TypeNum),
		output_common_prefix(ModuleName, common_prefix_var, !IO),
		io__write_int(CellNum, !IO)
	;
		VarName = tabling_pointer(ProcLabel),
		output_tabling_pointer_var_name(ProcLabel, !IO)
	).

:- pred output_common_cell_type_name(module_name::in, int::in, io::di, io::uo)
	is det.

output_common_cell_type_name(ModuleName, TypeNum, !IO) :-
	output_common_prefix(ModuleName, common_prefix_type, !IO),
	io__write_int(TypeNum, !IO).

:- type common_prefix
	--->	common_prefix_var
	;	common_prefix_type.

:- pred output_common_prefix(module_name::in, common_prefix::in,
	io::di, io::uo) is det.

output_common_prefix(ModuleName, Prefix, !IO) :-
	(
		Prefix = common_prefix_var,
		io__write_string(mercury_common_prefix, !IO)
	;
		Prefix = common_prefix_type,
		io__write_string(mercury_common_type_prefix, !IO)
	),
	globals__io_lookup_bool_option(split_c_files, SplitFiles, !IO),
	(
		SplitFiles = no
		% In the absence of split_c_files, common cells are always
		% local to a C file, so we don't have to module qualify them,
		% or the names of their types, and omitting the module
		% qualification makes the generated C file significantly
		% smaller in debugging grades.
	;
		SplitFiles = yes,
		MangledModuleName = sym_name_mangle(ModuleName),
		io__write_string(MangledModuleName, !IO),
		io__write_string("__", !IO)
	).

:- pred output_label_as_code_addr(label::in, io::di, io::uo) is det.

output_label_as_code_addr(Label, !IO) :-
	label_as_code_addr_to_string(Label, Str),
	io__write_string(Str, !IO).

:- func label_is_external_to_c_module(label) = bool.

label_is_external_to_c_module(entry(exported, _)) = yes.
label_is_external_to_c_module(entry(local, _)) = yes.
label_is_external_to_c_module(entry(c_local, _)) = no.
label_is_external_to_c_module(internal(_, _)) = no.

:- pred label_as_code_addr_to_string(label::in, string::out) is det.

label_as_code_addr_to_string(Label, Str) :-
	LabelStr = llds_out__label_to_c_string(Label, no),
	IsEntry = label_is_external_to_c_module(Label),
	(
		IsEntry = yes,
		Str = "MR_ENTRY_AP(" ++ LabelStr ++ ")"
	;
		IsEntry = no,
		Str = "MR_LABEL_AP(" ++ LabelStr ++ ")"
	).

:- pred output_label_list(list(label)::in, io::di, io::uo) is det.

output_label_list([], !IO).
output_label_list([Label | Labels], !IO) :-
	io__write_string("MR_LABEL_AP(", !IO),
	output_label(Label, no, !IO),
	io__write_string(")", !IO),
	output_label_list_2(Labels, !IO).

:- pred output_label_list_2(list(label)::in, io::di, io::uo) is det.

output_label_list_2([], !IO).
output_label_list_2([Label | Labels], !IO) :-
	io__write_string(" MR_AND\n\t\t", !IO),
	io__write_string("MR_LABEL_AP(", !IO),
	output_label(Label, no, !IO),
	io__write_string(")", !IO),
	output_label_list_2(Labels, !IO).

:- pred output_label_defn(label::in, io::di, io::uo) is det.

output_label_defn(entry(exported, ProcLabel), !IO) :-
	io__write_string("MR_define_entry(", !IO),
	output_label(entry(exported, ProcLabel), !IO),
	io__write_string(");\n", !IO).
output_label_defn(entry(local, ProcLabel), !IO) :-
	% The code for procedures local to a Mercury module
	% should normally be visible only within the C file
	% generated for that module. However, if we generate
	% multiple C files, the code in each C file must be
	% visible to the other C files for that Mercury module.
	globals__io_lookup_bool_option(split_c_files, SplitFiles, !IO),
	(
		SplitFiles = no,
		io__write_string("MR_def_static(", !IO),
		output_proc_label(ProcLabel, no, !IO),
		io__write_string(")\n", !IO)
	;
		SplitFiles = yes,
		io__write_string("MR_def_entry(", !IO),
		output_proc_label(ProcLabel, no, !IO),
		io__write_string(")\n", !IO)
	).
output_label_defn(entry(c_local, ProcLabel), !IO) :-
	io__write_string("MR_def_local(", !IO),
	output_proc_label(ProcLabel, no, !IO),
	io__write_string(")\n", !IO).
output_label_defn(internal(Num, ProcLabel), !IO) :-
	io__write_string("MR_def_label(", !IO),
	output_proc_label(ProcLabel, no, !IO),
	io__write_string(",", !IO),
	io__write_int(Num, !IO),
	io__write_string(")\n", !IO).

% Entry labels should generate the same code, regardless of the entry label
% type, because we may refer to an entry label via different entry label types
% in different circumstances.
% For example, the entry label of a recursive unification predicate
% is referred to as local in type_info structures and as c_local
% in the recursive call, since the c_local is special cased in some
% circumstances, leading to better code.

output_label(Label, !IO) :-
	LabelStr = llds_out__label_to_c_string(Label, yes),
	io__write_string(LabelStr, !IO).

output_label(Label, AddPrefix, !IO) :-
	LabelStr = llds_out__label_to_c_string(Label, AddPrefix),
	io__write_string(LabelStr, !IO).

llds_out__label_to_c_string(entry(_, ProcLabel), AddPrefix) =
	proc_label_to_c_string(ProcLabel, AddPrefix).
llds_out__label_to_c_string(internal(Num, ProcLabel), AddPrefix) = LabelStr :-
	ProcLabelStr = proc_label_to_c_string(ProcLabel, AddPrefix),
	string__int_to_string(Num, NumStr),
	string__append("_i", NumStr, NumSuffix),
	string__append(ProcLabelStr, NumSuffix, LabelStr).

:- pred output_reg(reg_type::in, int::in, io::di, io::uo) is det.

output_reg(r, N, !IO) :-
	llds_out__reg_to_string(r, N, RegName),
	io__write_string(RegName, !IO).
output_reg(f, _, !IO) :-
	error("Floating point registers not implemented").

:- pred output_tag(tag::in, io::di, io::uo) is det.

output_tag(Tag, !IO) :-
	io__write_string("MR_mktag(", !IO),
	io__write_int(Tag, !IO),
	io__write_string(")", !IO).

	% output an rval, converted to the specified type
	%
:- pred output_rval_as_type(rval::in, llds_type::in, io::di, io::uo) is det.

output_rval_as_type(Rval, DesiredType, !IO) :-
	llds__rval_type(Rval, ActualType),
	( types_match(DesiredType, ActualType) ->
		% no casting needed
		output_rval(Rval, !IO)
	;
		% We need to convert to the right type first.
		% Convertions to/from float must be treated specially;
		% for the others, we can just use a cast.
		( DesiredType = float ->
			io__write_string("MR_word_to_float(", !IO),
			output_rval(Rval, !IO),
			io__write_string(")", !IO)
		; ActualType = float ->
			( DesiredType = word ->
				output_float_rval_as_word(Rval, !IO)
			; DesiredType = data_ptr ->
				output_float_rval_as_data_ptr(Rval, !IO)
			;
				error("output_rval_as_type: type error")
			)
		;
			(
				Rval = const(int_const(N)),
				direct_field_int_constant(DesiredType) = yes
			->
				% The condition above increases the runtime of
				% the compiler very slightly. The elimination
				% of the unnecessary casts reduces the size
				% of the generated C source file, which has
				% a considerably longer lifetime. In debugging
				% grades, the file size difference can be
				% very substantial (in the range of megabytes).
				output_int_const(N, DesiredType, !IO)
			;
				% cast value to desired type
				output_llds_type_cast(DesiredType, !IO),
				output_rval(Rval, !IO)
			)
		)
	).

	% types_match(DesiredType, ActualType) is true iff
	% a value of type ActualType can be used as a value of
	% type DesiredType without casting.
	%
:- pred types_match(llds_type::in, llds_type::in) is semidet.

types_match(Type, Type).
types_match(word, unsigned).
types_match(word, integer).
types_match(word, bool).
types_match(bool, integer).
types_match(bool, unsigned).
types_match(bool, word).
types_match(integer, bool).

	% Return true iff an integer constant can be used directly as a value
	% in a structure field of the given type, instead of being cast to
	% MR_Integer first and then to the type. The answer can be
	% conservative: it is always ok to return `no'.
	%
	% Only the compiler generates values of the uint_leastN types,
	% and for these the constant will never be negative.
	%
:- func direct_field_int_constant(llds_type) = bool.

direct_field_int_constant(bool) = no.
direct_field_int_constant(int_least8) = yes.
direct_field_int_constant(uint_least8) = yes.
direct_field_int_constant(int_least16) = yes.
direct_field_int_constant(uint_least16) = yes.
direct_field_int_constant(int_least32) = yes.
direct_field_int_constant(uint_least32) = yes.
direct_field_int_constant(integer) = yes.
direct_field_int_constant(unsigned) = yes.
direct_field_int_constant(float) = no.
direct_field_int_constant(string) = no.
direct_field_int_constant(data_ptr) = no.
direct_field_int_constant(code_ptr) = no.
direct_field_int_constant(word) = no.

	% output a float rval, converted to type `MR_Word *'
	%
:- pred output_float_rval_as_data_ptr(rval::in, io::di, io::uo) is det.

output_float_rval_as_data_ptr(Rval, !IO) :-
	output_float_rval(Rval, yes, !IO).

	% output a float rval, converted to type `MR_Word'
	%
:- pred output_float_rval_as_word(rval::in, io::di, io::uo) is det.

output_float_rval_as_word(Rval, !IO) :-
	output_float_rval(Rval, no, !IO).

	% output a float rval, converted to type `MR_Word' or `MR_Word *'
	%
:- pred output_float_rval(rval::in, bool::in, io::di, io::uo) is det.

output_float_rval(Rval, IsPtr, !IO) :-
	%
	% for float constant expressions, if we're using boxed
	% boxed floats and --static-ground-terms is enabled,
	% we just refer to the static const which we declared
	% earlier
	%
	globals__io_lookup_bool_option(unboxed_float, UnboxFloat, !IO),
	globals__io_lookup_bool_option(static_ground_terms, StaticGroundTerms,
		!IO),
	(
		UnboxFloat = no,
		StaticGroundTerms = yes,
		llds_out__float_const_expr_name(Rval, FloatName)
	->
		(
			IsPtr = yes,
			Cast = data_ptr
		;
			IsPtr = no,
			Cast = word
		),
		output_llds_type_cast(Cast, !IO),
		io__write_string("&mercury_float_const_", !IO),
		io__write_string(FloatName, !IO)
	;
		(
			IsPtr = yes,
			output_llds_type_cast(data_ptr, !IO)
		;
			IsPtr = no
		),
		io__write_string("MR_float_to_word(", !IO),
		output_rval(Rval, !IO),
		io__write_string(")", !IO)
	).

:- pred output_test_rval(rval::in, io::di, io::uo) is det.

output_test_rval(Test, !IO) :-
	(
		is_int_cmp(Test, Left, RightConst, OpStr, _)
	->
		io__write_string(OpStr, !IO),
		io__write_string("(", !IO),
		output_rval(Left, !IO),
		io__write_string(",", !IO),
		io__write_int(RightConst, !IO),
		io__write_string(")", !IO)
	;
		Test = unop(not, InnerTest),
		is_int_cmp(InnerTest, Left, RightConst, _, NegOpStr)
	->
		io__write_string(NegOpStr, !IO),
		io__write_string("(", !IO),
		output_rval(Left, !IO),
		io__write_string(",", !IO),
		io__write_int(RightConst, !IO),
		io__write_string(")", !IO)
	;
		is_ptag_test(Test, Rval, Ptag, Negated)
	->
		(
			Negated = no,
			io__write_string("MR_PTAG_TEST(", !IO)
		;
			Negated = yes,
			io__write_string("MR_PTAG_TESTR(", !IO)
		),
		output_rval(Rval, !IO),
		io__write_string(",", !IO),
		io__write_int(Ptag, !IO),
		io__write_string(")", !IO)
	;
		Test = unop(not, InnerTest),
		is_ptag_test(InnerTest, Rval, Ptag, Negated)
	->
		(
			Negated = no,
			io__write_string("MR_PTAG_TESTR(", !IO)
		;
			Negated = yes,
			io__write_string("MR_PTAG_TEST(", !IO)
		),
		output_rval(Rval, !IO),
		io__write_string(",", !IO),
		io__write_int(Ptag, !IO),
		io__write_string(")", !IO)
	;
		Test = binop(and, Left, Right),
		is_ptag_test(Left, Rval, Ptag, no),
		is_remote_stag_test(Right, Rval, Ptag, Stag)
	->
		io__write_string("MR_RTAGS_TEST(", !IO),
		output_rval(Rval, !IO),
		io__write_string(",", !IO),
		io__write_int(Ptag, !IO),
		io__write_string(",", !IO),
		io__write_int(Stag, !IO),
		io__write_string(")", !IO)
	;
		Test = unop(not, InnerTest),
		InnerTest = binop(and, Left, Right),
		is_ptag_test(Left, Rval, Ptag, no),
		is_remote_stag_test(Right, Rval, Ptag, Stag)
	->
		io__write_string("MR_RTAGS_TESTR(", !IO),
		output_rval(Rval, !IO),
		io__write_string(",", !IO),
		io__write_int(Ptag, !IO),
		io__write_string(",", !IO),
		io__write_int(Stag, !IO),
		io__write_string(")", !IO)
	;
		is_local_stag_test(Test, Rval, Ptag, Stag, Negated)
	->
		(
			Negated = no,
			io__write_string("MR_LTAGS_TEST(", !IO)
		;
			Negated = yes,
			io__write_string("MR_LTAGS_TESTR(", !IO)
		),
		output_rval(Rval, !IO),
		io__write_string(",", !IO),
		io__write_int(Ptag, !IO),
		io__write_string(",", !IO),
		io__write_int(Stag, !IO),
		io__write_string(")", !IO)
	;
		Test = unop(not, InnerTest),
		is_local_stag_test(InnerTest, Rval, Ptag, Stag, Negated)
	->
		(
			Negated = no,
			io__write_string("MR_LTAGS_TESTR(", !IO)
		;
			Negated = yes,
			io__write_string("MR_LTAGS_TEST(", !IO)
		),
		output_rval(Rval, !IO),
		io__write_string(",", !IO),
		io__write_int(Ptag, !IO),
		io__write_string(",", !IO),
		io__write_int(Stag, !IO),
		io__write_string(")", !IO)
	;
		output_rval_as_type(Test, bool, !IO)
	).

:- pred is_int_cmp(rval::in, rval::out, int::out, string::out, string::out)
	is semidet.

is_int_cmp(Test, Left, RightConst, OpStr, NegOpStr) :-
	Test = binop(Op, Left, Right),
	Right = const(int_const(RightConst)),
	(
		Op = eq,
		OpStr = "MR_INT_EQ",
		NegOpStr = "MR_INT_NE"
	;
		Op = ne,
		OpStr = "MR_INT_NE",
		NegOpStr = "MR_INT_EQ"
	;
		Op = (<),
		OpStr = "MR_INT_LT",
		NegOpStr = "MR_INT_GE"
	;
		Op = (>),
		OpStr = "MR_INT_GT",
		NegOpStr = "MR_INT_LT"
	;
		Op = (<=),
		OpStr = "MR_INT_LE",
		NegOpStr = "MR_INT_GT"
	;
		Op = (>=),
		OpStr = "MR_INT_GE",
		NegOpStr = "MR_INT_LT"
	).

:- pred is_ptag_test(rval::in, rval::out, int::out, bool::out) is semidet.

is_ptag_test(Test, Rval, Ptag, Negated) :-
	Test = binop(Op, Left, Right),
	Left = unop(tag, Rval),
	Right = unop(mktag, const(int_const(Ptag))),
	(
		Op = eq,
		Negated = no
	;
		Op = ne,
		Negated = yes
	).

:- pred is_remote_stag_test(rval::in, rval::in, int::in, int::out) is semidet.

is_remote_stag_test(Test, Rval, Ptag, Stag) :-
	Test = binop(eq, Left, Right),
	Left = lval(field(yes(Ptag), Rval, Zero)),
	Zero = const(int_const(0)),
	Right = const(int_const(Stag)).

:- pred is_local_stag_test(rval::in, rval::out, int::out, int::out, bool::out)
	is semidet.

is_local_stag_test(Test, Rval, Ptag, Stag, Negated) :-
	Test = binop(Op, Rval, Right),
	Right = mkword(Ptag, unop(mkbody, const(int_const(Stag)))),
	(
		Op = eq,
		Negated = no
	;
		Op = ne,
		Negated = yes
	).

output_rval(const(Const), !IO) :-
	output_rval_const(Const, !IO).
output_rval(unop(UnaryOp, Exprn), !IO) :-
	output_unary_op(UnaryOp, !IO),
	io__write_string("(", !IO),
	llds__unop_arg_type(UnaryOp, ArgType),
	output_rval_as_type(Exprn, ArgType, !IO),
	io__write_string(")", !IO).
output_rval(binop(Op, X, Y), !IO) :-
	(
		Op = array_index(_Type)
	->
		io__write_string("(", !IO),
		output_rval_as_type(X, data_ptr, !IO),
		io__write_string(")[", !IO),
		output_rval_as_type(Y, integer, !IO),
		io__write_string("]", !IO)
	;
		c_util__string_compare_op(Op, OpStr)
	->
		io__write_string("(strcmp((char *)", !IO),
		output_rval_as_type(X, word, !IO),
		io__write_string(", (char *)", !IO),
		output_rval_as_type(Y, word, !IO),
		io__write_string(")", !IO),
		io__write_string(" ", !IO),
		io__write_string(OpStr, !IO),
		io__write_string(" ", !IO),
		io__write_string("0)", !IO)
	;
		( c_util__float_compare_op(Op, OpStr1) ->
			OpStr = OpStr1
		; c_util__float_op(Op, OpStr2) ->
			OpStr = OpStr2
		;
			fail
		)
	->
		io__write_string("(", !IO),
		output_rval_as_type(X, float, !IO),
		io__write_string(" ", !IO),
		io__write_string(OpStr, !IO),
		io__write_string(" ", !IO),
		output_rval_as_type(Y, float, !IO),
		io__write_string(")", !IO)
	;
% XXX broken for C == minint
% (since `NewC is 0 - C' overflows)
% 		Op = (+),
% 		Y = const(int_const(C)),
% 		C < 0
% 	->
% 		NewOp = (-),
% 		NewC is 0 - C,
% 		NewY = const(int_const(NewC)),
% 		io__write_string("("),
% 		output_rval(X),
% 		io__write_string(" "),
% 		output_binary_op(NewOp),
% 		io__write_string(" "),
% 		output_rval(NewY),
% 		io__write_string(")")
% 	;
		% special-case equality ops to avoid some unnecessary
		% casts -- there's no difference between signed and
		% unsigned equality, so if both args are unsigned, we
		% don't need to cast them to (Integer)
		( Op = eq ; Op = ne ),
		( llds__rval_type(X, XType) ),
		( XType = word ; XType = unsigned ),
		( llds__rval_type(Y, YType) ),
		( YType = word ; YType = unsigned )
	->
		io__write_string("(", !IO),
		output_rval(X, !IO),
		io__write_string(" ", !IO),
		output_binary_op(Op, !IO),
		io__write_string(" ", !IO),
		output_rval(Y, !IO),
		io__write_string(")", !IO)
	;
		c_util__unsigned_compare_op(Op, OpStr)
	->
		io__write_string("(", !IO),
		output_rval_as_type(X, unsigned, !IO),
		io__write_string(" ", !IO),
		io__write_string(OpStr, !IO),
		io__write_string(" ", !IO),
		output_rval_as_type(Y, unsigned, !IO),
		io__write_string(")", !IO)
	;
		io__write_string("(", !IO),
		output_rval_as_type(X, integer, !IO),
		io__write_string(" ", !IO),
		output_binary_op(Op, !IO),
		io__write_string(" ", !IO),
		output_rval_as_type(Y, integer, !IO),
		io__write_string(")", !IO)
	).
output_rval(mkword(Tag, Exprn), !IO) :-
	globals__io_lookup_bool_option(split_c_files, SplitFiles, !IO),
	(
		SplitFiles = no,
		Exprn = const(data_addr_const(DataAddr, no)),
		DataAddr = data_addr(_, DataName),
		DataName = common(CellNum, _TypeNum)
	->
		io__write_string("MR_TAG_COMMON(", !IO),
		io__write_int(Tag, !IO),
		io__write_string(",", !IO),
		io__write_int(CellNum, !IO),
		io__write_string(")", !IO)
	;
		Exprn = unop(mkbody, const(int_const(Body)))
	->
		io__write_string("MR_tbmkword(", !IO),
		io__write_int(Tag, !IO),
		io__write_string(", ", !IO),
		io__write_int(Body, !IO),
		io__write_string(")", !IO)
	;
		io__write_string("MR_tmkword(", !IO),
		io__write_int(Tag, !IO),
		io__write_string(", ", !IO),
		output_rval_as_type(Exprn, data_ptr, !IO),
		io__write_string(")", !IO)
	).
output_rval(lval(Lval), !IO) :-
	% if a field is used as an rval, then we need to use
	% the MR_const_field() macro or its variants, not the MR_field() macro
	% or its variants, to avoid warnings about discarding const.
	( Lval = field(MaybeTag, Rval, FieldNumRval) ->
		( MaybeTag = yes(Tag) ->
			io__write_string("MR_ctfield(", !IO),
			io__write_int(Tag, !IO),
			io__write_string(", ", !IO)
		;
			io__write_string("MR_const_mask_field(", !IO)
		),
		output_rval(Rval, !IO),
		io__write_string(", ", !IO),
		( FieldNumRval = const(int_const(FieldNum)) ->
			% Avoid emitting the (MR_Integer) cast.
			io__write_int(FieldNum, !IO)
		;
			output_rval(FieldNumRval, !IO)
		),
		io__write_string(")", !IO)
	;
		output_lval(Lval, !IO)
	).
output_rval(var(_), !IO) :-
	error("Cannot output a var(_) expression in code").
output_rval(mem_addr(MemRef), !IO) :-
	(
		MemRef = stackvar_ref(N),
		output_llds_type_cast(data_ptr, !IO),
		io__write_string("&MR_sv(", !IO),
		io__write_int(N, !IO),
		io__write_string(")", !IO)
	;
		MemRef = framevar_ref(N),
		output_llds_type_cast(data_ptr, !IO),
		io__write_string("&MR_fv(", !IO),
		io__write_int(N, !IO),
		io__write_string(")", !IO)
	;
		MemRef = heap_ref(Rval, Tag, FieldNum),
		output_llds_type_cast(data_ptr, !IO),
		io__write_string("&MR_tfield(", !IO),
		io__write_int(Tag, !IO),
		io__write_string(", ", !IO),
		output_rval(Rval, !IO),
		io__write_string(", ", !IO),
		io__write_int(FieldNum, !IO),
		io__write_string(")", !IO)
	).

:- pred output_unary_op(unary_op::in, io::di, io::uo) is det.

output_unary_op(Op, !IO) :-
	c_util__unary_prefix_op(Op, OpString),
	io__write_string(OpString, !IO).

:- pred output_rval_const(rval_const::in, io::di, io::uo) is det.

output_rval_const(int_const(N), !IO) :-
	% we need to cast to (Integer) to ensure
	% things like 1 << 32 work when `Integer' is 64 bits
	% but `int' is 32 bits.
	output_llds_type_cast(integer, !IO),
	io__write_int(N, !IO).
output_rval_const(float_const(FloatVal), !IO) :-
	% the cast to (Float) here lets the C compiler
	% do arithmetic in `float' rather than `double'
	% if `Float' is `float' not `double'.
	output_llds_type_cast(float, !IO),
	c_util__output_float_literal(FloatVal, !IO).
output_rval_const(string_const(String), !IO) :-
	io__write_string("MR_string_const(""", !IO),
	c_util__output_quoted_string(String, !IO),
	string__length(String, StringLength),
	io__write_string(""", ", !IO),
	io__write_int(StringLength, !IO),
	io__write_string(")", !IO).
output_rval_const(multi_string_const(Length, String), !IO) :-
	io__write_string("MR_string_const(""", !IO),
	c_util__output_quoted_multi_string(Length, String, !IO),
	io__write_string(""", ", !IO),
	io__write_int(Length, !IO),
	io__write_string(")", !IO).
output_rval_const(true, !IO) :-
	io__write_string("MR_TRUE", !IO).
output_rval_const(false, !IO) :-
	io__write_string("MR_FALSE", !IO).
output_rval_const(code_addr_const(CodeAddress), !IO) :-
	output_code_addr(CodeAddress, !IO).
output_rval_const(data_addr_const(DataAddr, MaybeOffset), !IO) :-
	% Data addresses are all assumed to be of type `MR_Word *';
	% we need to cast them here to avoid type errors. The offset
	% is also in MR_Words.
	(
		MaybeOffset = no,
		% The tests for special cases below increase the runtime
		% of the compiler very slightly, but the use of shorter names
		% reduces the size of the generated C source file, which has
		% a considerably longer lifetime. In debugging grades, the
		% file size difference can be very substantial.
		globals__io_lookup_bool_option(split_c_files, SplitFiles, !IO),
		(
			SplitFiles = no,
			DataAddr = data_addr(_, DataName),
			DataName = common(CellNum, _TypeNum)
		->
			io__write_string("MR_COMMON(", !IO),
			io__write_int(CellNum, !IO),
			io__write_string(")", !IO)
		;
			DataAddr = rtti_addr(RttiId),
			rtti_id_emits_type_ctor_info(RttiId, Ctor),
			Ctor = rtti_type_ctor(Module, Name, Arity),
			sym_name_doesnt_need_mangling(Module),
			name_doesnt_need_mangling(Name)
		->
			output_type_ctor_addr(Module, Name, Arity, !IO)
		;
			output_llds_type_cast(data_ptr, !IO),
			io__write_string("&", !IO),
			output_data_addr(DataAddr, !IO)
		)
	;
		MaybeOffset = yes(Offset),
		io__write_string("((", !IO),
		output_llds_type_cast(data_ptr, !IO),
		io__write_string("&", !IO),
		output_data_addr(DataAddr, !IO),
		io__write_string(") + ", !IO),
		io__write_int(Offset, !IO),
		io__write_string(")", !IO)
	).

:- pred output_type_ctor_addr(module_name::in, string::in, int::in,
	io::di, io::uo) is det.

output_type_ctor_addr(Module0, Name, Arity, !IO) :-
	( Module0 = unqualified("") ->
		Module = mercury_public_builtin_module
	;
		Module = Module0
	),
	% We don't need to mangle the module name, but we do need to
	% convert it to a C identifier in the standard fashion.
	ModuleStr = sym_name_mangle(Module),
	( Arity = 0 ->
		(
			ModuleStr = "builtin",
			( Name = "int" ->
				Macro = "MR_INT_CTOR_ADDR"
			; Name = "float" ->
				Macro = "MR_FLOAT_CTOR_ADDR"
			; Name = "string" ->
				Macro = "MR_STRING_CTOR_ADDR"
			; Name = "character" ->
				Macro = "MR_CHAR_CTOR_ADDR"
			;
				fail
			)
		->
			io__write_string(Macro, !IO)
		;
			ModuleStr = "io",
			Name = "state"
		->
			io__write_string("MR_IO_CTOR_ADDR", !IO)
		;
			ModuleStr = "bool",
			Name = "bool"
		->
			io__write_string("MR_BOOL_CTOR_ADDR", !IO)
		;
			io__write_strings(["MR_CTOR0_ADDR(", ModuleStr, ", ",
				Name, ")"], !IO)
		)
	; Arity = 1 ->
		(
			Name = "list",
			ModuleStr = "list"
		->
			io__write_string("MR_LIST_CTOR_ADDR", !IO)
		;
			Name = "private_builtin",
			ModuleStr = "type_info"
		->
			io__write_string("MR_TYPE_INFO_CTOR_ADDR", !IO)
		;
			io__write_strings(["MR_CTOR1_ADDR(", ModuleStr, ", ",
				Name, ")"], !IO)
		)
	;
		io__write_strings(["MR_CTOR_ADDR(", ModuleStr, ", ", Name,
			", ", int_to_string(Arity), ")"], !IO)
	).

:- pred output_lval_as_word(lval::in, io::di, io::uo) is det.

output_lval_as_word(Lval, !IO) :-
	llds__lval_type(Lval, ActualType),
	( types_match(word, ActualType) ->
		output_lval(Lval, !IO)
	; ActualType = float ->
		% sanity check -- if this happens, the llds is ill-typed
		error("output_lval_as_word: got float")
	;
		io__write_string("MR_LVALUE_CAST(MR_Word,", !IO),
		output_lval(Lval, !IO),
		io__write_string(")", !IO)
	).

:- pred output_lval(lval::in, io::di, io::uo) is det.

output_lval(reg(Type, Num), !IO) :-
	output_reg(Type, Num, !IO).
output_lval(stackvar(N), !IO) :-
	( N =< 0 ->
		error("stack var out of range")
	;
		true
	),
	io__write_string("MR_sv(", !IO),
	io__write_int(N, !IO),
	io__write_string(")", !IO).
output_lval(framevar(N), !IO) :-
	( N =< 0 ->
		error("frame var out of range")
	;
		true
	),
	io__write_string("MR_fv(", !IO),
	io__write_int(N, !IO),
	io__write_string(")", !IO).
output_lval(succip, !IO) :-
	io__write_string("MR_succip", !IO).
output_lval(sp, !IO) :-
	io__write_string("MR_sp", !IO).
output_lval(hp, !IO) :-
	io__write_string("MR_hp", !IO).
output_lval(maxfr, !IO) :-
	io__write_string("MR_maxfr", !IO).
output_lval(curfr, !IO) :-
	io__write_string("MR_curfr", !IO).
output_lval(succfr(Rval), !IO) :-
	io__write_string("MR_succfr_slot(", !IO),
	output_rval(Rval, !IO),
	io__write_string(")", !IO).
output_lval(prevfr(Rval), !IO) :-
	io__write_string("MR_prevfr_slot(", !IO),
	output_rval(Rval, !IO),
	io__write_string(")", !IO).
output_lval(redofr(Rval), !IO) :-
	io__write_string("MR_redofr_slot(", !IO),
	output_rval(Rval, !IO),
	io__write_string(")", !IO).
output_lval(redoip(Rval), !IO) :-
	io__write_string("MR_redoip_slot(", !IO),
	output_rval(Rval, !IO),
	io__write_string(")", !IO).
output_lval(succip(Rval), !IO) :-
	io__write_string("MR_succip_slot(", !IO),
	output_rval(Rval, !IO),
	io__write_string(")", !IO).
output_lval(field(MaybeTag, Rval, FieldNumRval), !IO) :-
	(
		MaybeTag = yes(Tag),
		io__write_string("MR_tfield(", !IO),
		io__write_int(Tag, !IO),
		io__write_string(", ", !IO)
	;
		MaybeTag = no,
		io__write_string("MR_mask_field(", !IO)
	),
	output_rval(Rval, !IO),
	io__write_string(", ", !IO),
	( FieldNumRval = const(int_const(FieldNum)) ->
		% Avoid emitting the (MR_Integer) cast.
		io__write_int(FieldNum, !IO)
	;
		output_rval(FieldNumRval, !IO)
	),
	io__write_string(")", !IO).
output_lval(lvar(_), !IO) :-
	error("Illegal to output an lvar").
output_lval(temp(Type, Num), !IO) :-
	(
		Type = r,
		io__write_string("MR_tempr", !IO),
		io__write_int(Num, !IO)
	;
		Type = f,
		io__write_string("MR_tempf", !IO),
		io__write_int(Num, !IO)
	).
output_lval(mem_ref(Rval), !IO) :-
	io__write_string("XXX(", !IO),
	output_rval(Rval, !IO),
	io__write_string(")", !IO).

:- pred output_lval_for_assign(lval::in, llds_type::out, io::di, io::uo) is det.

output_lval_for_assign(reg(RegType, Num), word, !IO) :-
	require(unify(RegType, r), "output_lval_for_assign: float reg"),
	output_reg(RegType, Num, !IO).
output_lval_for_assign(stackvar(N), word, !IO) :-
	( N < 0 ->
		error("stack var out of range")
	;
		true
	),
	io__write_string("MR_sv(", !IO),
	io__write_int(N, !IO),
	io__write_string(")", !IO).
output_lval_for_assign(framevar(N), word, !IO) :-
	( N =< 0 ->
		error("frame var out of range")
	;
		true
	),
	io__write_string("MR_fv(", !IO),
	io__write_int(N, !IO),
	io__write_string(")", !IO).
output_lval_for_assign(succip, word, !IO) :-
	io__write_string("MR_succip_word", !IO).
output_lval_for_assign(sp, word, !IO) :-
	io__write_string("MR_sp_word", !IO).
output_lval_for_assign(hp, word, !IO) :-
	io__write_string("MR_hp_word", !IO).
output_lval_for_assign(maxfr, word, !IO) :-
	io__write_string("MR_maxfr_word", !IO).
output_lval_for_assign(curfr, word, !IO) :-
	io__write_string("MR_curfr_word", !IO).
output_lval_for_assign(succfr(Rval), word, !IO) :-
	io__write_string("MR_succfr_slot_word(", !IO),
	output_rval(Rval, !IO),
	io__write_string(")", !IO).
output_lval_for_assign(prevfr(Rval), word, !IO) :-
	io__write_string("MR_prevfr_slot_word(", !IO),
	output_rval(Rval, !IO),
	io__write_string(")", !IO).
output_lval_for_assign(redofr(Rval), word, !IO) :-
	io__write_string("MR_redofr_slot_word(", !IO),
	output_rval(Rval, !IO),
	io__write_string(")", !IO).
output_lval_for_assign(redoip(Rval), word, !IO) :-
	io__write_string("MR_redoip_slot_word(", !IO),
	output_rval(Rval, !IO),
	io__write_string(")", !IO).
output_lval_for_assign(succip(Rval), word, !IO) :-
	io__write_string("MR_succip_slot_word(", !IO),
	output_rval(Rval, !IO),
	io__write_string(")", !IO).
output_lval_for_assign(field(MaybeTag, Rval, FieldNumRval), word, !IO) :-
	(
		MaybeTag = yes(Tag),
		io__write_string("MR_tfield(", !IO),
		io__write_int(Tag, !IO),
		io__write_string(", ", !IO)
	;
		MaybeTag = no,
		io__write_string("MR_mask_field(", !IO)
	),
	output_rval(Rval, !IO),
	io__write_string(", ", !IO),
	( FieldNumRval = const(int_const(FieldNum)) ->
		% Avoid emitting the (MR_Integer) cast.
		io__write_int(FieldNum, !IO)
	;
		output_rval(FieldNumRval, !IO)
	),
	io__write_string(")", !IO).
output_lval_for_assign(lvar(_), _, !IO) :-
	error("output_lval_for_assign: lvar").
output_lval_for_assign(temp(RegType, Num), Type, !IO) :-
	(
		RegType = r,
		Type = word,
		io__write_string("MR_tempr", !IO),
		io__write_int(Num, !IO)
	;
		RegType = f,
		Type = float,
		io__write_string("MR_tempf", !IO),
		io__write_int(Num, !IO)
	).
output_lval_for_assign(mem_ref(_), _, !IO) :-
	error("output_lval_for_assign: mem_ref").

%-----------------------------------------------------------------------------%

:- pred output_set_line_num(prog_context::in, io::di, io::uo) is det.

output_set_line_num(Context, !IO) :-
	term__context_file(Context, File),
	term__context_line(Context, Line),
	c_util__set_line_num(File, Line, !IO).

:- pred output_reset_line_num(io::di, io::uo) is det.

output_reset_line_num(!IO) :-
	c_util__reset_line_num(!IO).

%-----------------------------------------------------------------------------%

:- pred output_binary_op(binary_op::in, io::di, io::uo) is det.

output_binary_op(Op, !IO) :-
	( c_util__binary_infix_op(Op, String) ->
		io__write_string(String, !IO)
	;
		error("llds_out.m: invalid binary operator")
	).

llds_out__binary_op_to_string(Op, Name) :-
	( c_util__binary_infix_op(Op, Name0) ->
		Name = Name0
	;
		% The following is just for debugging purposes -
		% string operators are not output as `str_eq', etc.
		functor(Op, Name, _)
	).

%-----------------------------------------------------------------------------%

llds_out__lval_to_string(framevar(N), Description) :-
	string__int_to_string(N, N_String),
	string__append("MR_fv(", N_String, Tmp),
	string__append(Tmp, ")", Description).
llds_out__lval_to_string(stackvar(N), Description) :-
	string__int_to_string(N, N_String),
	string__append("MR_sv(", N_String, Tmp),
	string__append(Tmp, ")", Description).
llds_out__lval_to_string(reg(RegType, RegNum), Description) :-
	llds_out__reg_to_string(RegType, RegNum, Reg_String),
	string__append("reg(", Reg_String, Tmp),
	string__append(Tmp, ")", Description).

llds_out__reg_to_string(r, N, Description) :-
	( N =< max_real_r_reg ->
		Template = "MR_r%d"
	; N =< max_virtual_r_reg ->
		Template = "MR_r(%d)"
	;
		error("llds_out__reg_to_string: register number too large")
	),
	string__format(Template, [i(N)], Description).
llds_out__reg_to_string(f, N, Description) :-
	string__int_to_string(N, N_String),
	string__append("MR_f(", N_String, Tmp),
	string__append(Tmp, ")", Description).

:- func max_real_r_reg = int.
:- func max_virtual_r_reg = int.

max_real_r_reg = 32.
max_virtual_r_reg = 1024.

%-----------------------------------------------------------------------------%

:- pred gather_c_file_labels(list(comp_gen_c_module)::in, list(label)::out)
	is det.

gather_c_file_labels(Modules, Labels) :-
	gather_labels_from_c_modules(Modules, [], Labels1),
	list__reverse(Labels1, Labels).

:- pred gather_c_module_labels(list(c_procedure)::in, list(label)::out) is det.

gather_c_module_labels(Procs, Labels) :-
	gather_labels_from_c_procs(Procs, [], Labels1),
	list__reverse(Labels1, Labels).

%-----------------------------------------------------------------------------%

:- pred gather_labels_from_c_modules(list(comp_gen_c_module)::in,
	list(label)::in, list(label)::out) is det.

gather_labels_from_c_modules([], Labels, Labels).
gather_labels_from_c_modules([Module | Modules], Labels0, Labels) :-
	gather_labels_from_c_module(Module, Labels0, Labels1),
	gather_labels_from_c_modules(Modules, Labels1, Labels).

:- pred gather_labels_from_c_module(comp_gen_c_module::in,
	list(label)::in, list(label)::out) is det.

gather_labels_from_c_module(comp_gen_c_module(_, Procs), Labels0, Labels) :-
	gather_labels_from_c_procs(Procs, Labels0, Labels).

:- pred gather_labels_from_c_procs(list(c_procedure)::in,
	list(label)::in, list(label)::out) is det.

gather_labels_from_c_procs([], Labels, Labels).
gather_labels_from_c_procs([c_procedure(_, _, _, Instrs, _, _, _) | Procs],
		Labels0, Labels) :-
	gather_labels_from_instrs(Instrs, Labels0, Labels1),
	gather_labels_from_c_procs(Procs, Labels1, Labels).

:- pred gather_labels_from_instrs(list(instruction)::in,
	list(label)::in, list(label)::out) is det.

gather_labels_from_instrs([], Labels, Labels).
gather_labels_from_instrs([Instr | Instrs], Labels0, Labels) :-
	( Instr = label(Label) - _ ->
		Labels1 = [Label | Labels0]
	;
		Labels1 = Labels0
	),
	gather_labels_from_instrs(Instrs, Labels1, Labels).

%-----------------------------------------------------------------------------%
