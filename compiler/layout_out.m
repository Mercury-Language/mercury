%-----------------------------------------------------------------------------%
% Copyright (C) 2001-2004 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% This structure converts layout structures from the representation used
% within the compiler to the representation used by the runtime system.
% The types of the inputs are defined in layout.m. The types of the outputs
% are defined in runtime/mercury_stack_layout.h, where the documentation
% of the semantics of the various kinds of layout structures can also be found.
%
% This module should be, but as yet isn't, independent of whether we are
% compiling to LLDS or MLDS.
%
% Author: zs.

%-----------------------------------------------------------------------------%

:- module ll_backend__layout_out.

:- interface.

:- import_module backend_libs__proc_label.
:- import_module ll_backend__layout.
:- import_module ll_backend__llds.
:- import_module ll_backend__llds_out.
:- import_module parse_tree__prog_data.

:- import_module bool, io.

	% Given a Mercury representation of a layout structure, output its
	% definition in the appropriate C global variable.
:- pred output_layout_data_defn(layout_data::in, decl_set::in, decl_set::out,
	io::di, io::uo) is det.

	% Given the name of a layout structure, output the declaration
	% of the C global variable which will hold it.
:- pred output_layout_name_decl(layout_name::in, io::di, io::uo) is det.

	% Given the name of a layout structure, output the declaration
	% of the C global variable which will hold it, if it has
	% not already been declared.
:- pred output_maybe_layout_name_decl(layout_name::in,
	decl_set::in, decl_set::out, io::di, io::uo) is det.

	% Given a Mercury representation of a layout structure, output the
	% declaration of the C global variable which will hold it, if it has
	% not already been declared.
:- pred output_maybe_layout_data_decl(layout_data::in,
	decl_set::in, decl_set::out, io::di, io::uo) is det.

	% Given a reference to a layout structure, output the storage class
	% (e.g. static), type and name of the global variable that will
	% hold it. The bool says whether the output is part of the definition
	% of that variable (this influences e.g. whether we output "extern"
	% or not).
:- pred output_layout_name_storage_type_name(layout_name::in, bool::in,
	io::di, io::uo) is det.

	% Given a reference to a layout structure, output the name of the
	% global variable that will hold it.
:- pred output_layout_name(layout_name::in, io::di, io::uo) is det.

	% Given a reference to a layout structure, return a bool that is true
	% iff the layout structure contains code addresses.
:- func layout_name_would_include_code_addr(layout_name) = bool.

	% Given a label, return a string giving the name of the global variable
	% containing the label layout structure that would be associated with
	% it. Make_label_layout_name does not guarantee that the label *has*
	% an associated label layout structure.
:- func make_label_layout_name(label) = string.

	% For a given procedure label, return whether the procedure is
	% user-defined or part of a compiler-generated unify, compare or index
	% predicate.
:- func proc_label_user_or_uci(proc_label) = proc_layout_user_or_uci.

	% Output a value of C type MR_PredFunc corrresponding to the argument.
:- pred output_pred_or_func(pred_or_func::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs__c_util.
:- import_module backend_libs__name_mangle.
:- import_module backend_libs__rtti.
:- import_module hlds__hlds_goal.
:- import_module hlds__hlds_pred.
:- import_module hlds__special_pred.
:- import_module libs__globals.
:- import_module libs__options.
:- import_module libs__trace_params.
:- import_module ll_backend__code_util.
:- import_module parse_tree__prog_out.

:- import_module int, char, string, require, std_util, list.

output_layout_data_defn(label_layout_data(ProcLabel, LabelNum, ProcLayoutAddr,
		MaybePort, MaybeIsHidden, MaybeGoalPath, MaybeVarInfo),
		!DeclSet, !IO) :-
	output_label_layout_data_defn(ProcLabel, LabelNum, ProcLayoutAddr,
		MaybePort, MaybeIsHidden, MaybeGoalPath, MaybeVarInfo,
		!DeclSet, !IO).
output_layout_data_defn(proc_layout_data(ProcLabel, Traversal, MaybeRest),
		!DeclSet, !IO) :-
	output_proc_layout_data_defn(ProcLabel, Traversal, MaybeRest,
		!DeclSet, !IO).
output_layout_data_defn(closure_proc_id_data(CallerProcLabel, SeqNo,
		ProcLabel, ModuleName, FileName, LineNumber, GoalPath),
		!DeclSet, !IO) :-
	output_closure_proc_id_data_defn(CallerProcLabel, SeqNo, ProcLabel,
		ModuleName, FileName, LineNumber, GoalPath, !DeclSet, !IO).
output_layout_data_defn(module_layout_data(ModuleName, StringTableSize,
		StringTable, ProcLayoutNames, FileLayouts, TraceLevel,
		SuppressedEvents), !DeclSet, !IO) :-
	output_module_layout_data_defn(ModuleName, StringTableSize,
		StringTable, ProcLayoutNames, FileLayouts, TraceLevel,
		SuppressedEvents, !DeclSet, !IO).
output_layout_data_defn(table_io_decl_data(RttiProcLabel, Kind, NumPTIs,
		PTIVectorRval, TypeParamsRval), !DeclSet, !IO) :-
	output_table_io_decl(RttiProcLabel, Kind, NumPTIs,
		PTIVectorRval, TypeParamsRval, !DeclSet, !IO).
output_layout_data_defn(table_gen_data(RttiProcLabel, NumInputs, NumOutputs,
		Steps, PTIVectorRval, TypeParamsRval), !DeclSet, !IO) :-
	output_table_gen(RttiProcLabel, NumInputs, NumOutputs, Steps,
		PTIVectorRval, TypeParamsRval, !DeclSet, !IO).

%-----------------------------------------------------------------------------%

output_layout_name_decl(LayoutName, !IO) :-
	output_layout_name_storage_type_name(LayoutName, no, !IO),
	io__write_string(";\n", !IO).

output_maybe_layout_name_decl(LayoutName, !DeclSet, !IO) :-
	( decl_set_is_member(data_addr(layout_addr(LayoutName)), !.DeclSet) ->
		true
	;
		output_layout_name_decl(LayoutName, !IO),
		decl_set_insert(data_addr(layout_addr(LayoutName)), !DeclSet)
	).

output_maybe_layout_data_decl(LayoutData, !DeclSet, !IO) :-
	extract_layout_name(LayoutData, LayoutName),
	output_maybe_layout_name_decl(LayoutName, !DeclSet, !IO).

:- pred extract_layout_name(layout_data::in, layout_name::out) is det.

extract_layout_name(label_layout_data(ProcLabel, LabelNum, _, _, _, _, yes(_)),
		LayoutName) :-
	LayoutName = label_layout(ProcLabel, LabelNum, label_has_var_info).
extract_layout_name(label_layout_data(ProcLabel, LabelNum, _, _, _, _, no),
		LayoutName) :-
	LayoutName = label_layout(ProcLabel, LabelNum, label_has_no_var_info).
extract_layout_name(proc_layout_data(RttiProcLabel, _, MaybeRest),
		LayoutName) :-
	ProcLabel = make_proc_label_from_rtti(RttiProcLabel),
	Kind = maybe_proc_layout_and_more_kind(MaybeRest, ProcLabel),
	LayoutName = proc_layout(RttiProcLabel, Kind).
extract_layout_name(closure_proc_id_data(CallerProcLabel, SeqNo,
		ClosureProcLabel, _, _, _, _),
		closure_proc_id(CallerProcLabel, SeqNo, ClosureProcLabel)).
extract_layout_name(module_layout_data(ModuleName, _,_,_,_,_,_), LayoutName) :-
	LayoutName = module_layout(ModuleName).
extract_layout_name(table_io_decl_data(RttiProcLabel, _, _, _, _),
		LayoutName) :-
	LayoutName = table_io_decl(RttiProcLabel).
extract_layout_name(table_gen_data(RttiProcLabel, _, _, _, _, _),
		LayoutName) :-
	LayoutName = table_gen_info(RttiProcLabel).

:- pred output_layout_decl(layout_name::in, decl_set::in, decl_set::out,
	io::di, io::uo) is det.

output_layout_decl(LayoutName, !DeclSet, !IO) :-
	( decl_set_is_member(data_addr(layout_addr(LayoutName)), !.DeclSet) ->
		true
	;
		output_layout_name_storage_type_name(LayoutName, no, !IO),
		io__write_string(";\n", !IO),
		decl_set_insert(data_addr(layout_addr(LayoutName)), !DeclSet)
	).

	% This code should be kept in sync with output_layout_name/3 below.
make_label_layout_name(Label) = Name :-
	% We can't omit the mercury_ prefix on LabelName, even though the
	% mercury_data_prefix duplicates it, because there is no simply way
	% to make the MR_init_label_sl macro delete that prefix from the
	% label's name to get the name of its layout structure.
	LabelName = label_to_c_string(Label, yes),
	string__append_list([
		mercury_data_prefix,
		"_label_layout__",
		LabelName
	], Name).

output_layout_name(label_layout(ProcLabel, LabelNum, _)) -->
	% This code should be kept in sync with make_label_layout_name/1 above.
	io__write_string(mercury_data_prefix),
	io__write_string("_label_layout__"),
	io__write_string(label_to_c_string(internal(LabelNum, ProcLabel),
		yes)).
output_layout_name(proc_layout(RttiProcLabel, _)) -->
	io__write_string(mercury_data_prefix),
	io__write_string("_proc_layout__"),
	% We can't omit the mercury_ prefix on ProcLabel, even though the
	% mercury_data_prefix duplicates it, because there is no simply way
	% to make the MR_init_entryl_sl macro delete that prefix from the
	% entry label's name to get the name of its layout structure.
	output_proc_label(make_proc_label_from_rtti(RttiProcLabel), yes).
output_layout_name(proc_layout_exec_trace(RttiProcLabel)) -->
	io__write_string(mercury_data_prefix),
	io__write_string("_proc_layout_exec_trace__"),
	output_proc_label(make_proc_label_from_rtti(RttiProcLabel), no).
output_layout_name(proc_layout_head_var_nums(RttiProcLabel)) -->
	io__write_string(mercury_data_prefix),
	io__write_string("_head_var_nums__"),
	output_proc_label(make_proc_label_from_rtti(RttiProcLabel), no).
output_layout_name(proc_layout_var_names(RttiProcLabel)) -->
	io__write_string(mercury_data_prefix),
	io__write_string("_var_names__"),
	output_proc_label(make_proc_label_from_rtti(RttiProcLabel), no).
output_layout_name(closure_proc_id(CallerProcLabel, SeqNo, _)) -->
	io__write_string(mercury_data_prefix),
	io__write_string("_closure_layout__"),
	output_proc_label(CallerProcLabel, no),
	io__write_string("_"),
	io__write_int(SeqNo).
output_layout_name(file_layout(ModuleName, FileNum)) -->
	io__write_string(mercury_data_prefix),
	io__write_string("_file_layout__"),
	{ ModuleNameStr = sym_name_mangle(ModuleName) },
	io__write_string(ModuleNameStr),
	io__write_string("_"),
	io__write_int(FileNum).
output_layout_name(file_layout_line_number_vector(ModuleName, FileNum)) -->
	io__write_string(mercury_data_prefix),
	io__write_string("_file_lines__"),
	{ ModuleNameStr = sym_name_mangle(ModuleName) },
	io__write_string(ModuleNameStr),
	io__write_string("_"),
	io__write_int(FileNum).
output_layout_name(file_layout_label_layout_vector(ModuleName, FileNum)) -->
	io__write_string(mercury_data_prefix),
	io__write_string("_file_label_layouts__"),
	{ ModuleNameStr = sym_name_mangle(ModuleName) },
	io__write_string(ModuleNameStr),
	io__write_string("_"),
	io__write_int(FileNum).
output_layout_name(module_layout_string_table(ModuleName)) -->
	io__write_string(mercury_data_prefix),
	io__write_string("_module_strings__"),
	{ ModuleNameStr = sym_name_mangle(ModuleName) },
	io__write_string(ModuleNameStr).
output_layout_name(module_layout_file_vector(ModuleName)) -->
	io__write_string(mercury_data_prefix),
	io__write_string("_module_files__"),
	{ ModuleNameStr = sym_name_mangle(ModuleName) },
	io__write_string(ModuleNameStr).
output_layout_name(module_layout_proc_vector(ModuleName)) -->
	io__write_string(mercury_data_prefix),
	io__write_string("_module_procs__"),
	{ ModuleNameStr = sym_name_mangle(ModuleName) },
	io__write_string(ModuleNameStr).
output_layout_name(module_layout(ModuleName)) -->
	io__write_string(mercury_data_prefix),
	io__write_string("_module_layout__"),
	{ ModuleNameStr = sym_name_mangle(ModuleName) },
	io__write_string(ModuleNameStr).
output_layout_name(proc_static(RttiProcLabel)) -->
	io__write_string(mercury_data_prefix),
	io__write_string("_proc_static__"),
	{ ProcLabel = make_proc_label_from_rtti(RttiProcLabel) },
	output_proc_label(ProcLabel, no).
output_layout_name(proc_static_call_sites(RttiProcLabel)) -->
	io__write_string(mercury_data_prefix),
	io__write_string("_proc_static_call_sites__"),
	{ ProcLabel = make_proc_label_from_rtti(RttiProcLabel) },
	output_proc_label(ProcLabel, no).
output_layout_name(table_io_decl(RttiProcLabel)) -->
	io__write_string(mercury_data_prefix),
	io__write_string("_table_io_decl__"),
	{ ProcLabel = make_proc_label_from_rtti(RttiProcLabel) },
	output_proc_label(ProcLabel, no).
output_layout_name(table_gen_info(RttiProcLabel)) -->
	io__write_string(mercury_data_prefix),
	io__write_string("_table_gen__"),
	{ ProcLabel = make_proc_label_from_rtti(RttiProcLabel) },
	output_proc_label(ProcLabel, no).
output_layout_name(table_gen_enum_params(RttiProcLabel)) -->
	io__write_string(mercury_data_prefix),
	io__write_string("_table_enum_params__"),
	{ ProcLabel = make_proc_label_from_rtti(RttiProcLabel) },
	output_proc_label(ProcLabel, no).
output_layout_name(table_gen_steps(RttiProcLabel)) -->
	io__write_string(mercury_data_prefix),
	io__write_string("_table_steps__"),
	{ ProcLabel = make_proc_label_from_rtti(RttiProcLabel) },
	output_proc_label(ProcLabel, no).

output_layout_name_storage_type_name(
		label_layout(ProcLabel, LabelNum, LabelVars),
		_BeingDefined) -->
	io__write_string("static const "),
	io__write_string(label_vars_to_type(LabelVars)),
	io__write_string(" "),
	output_layout_name(label_layout(ProcLabel, LabelNum, LabelVars)).
output_layout_name_storage_type_name(proc_layout(ProcLabel, Kind),
		BeingDefined) -->
	{ ProcIsImported = ProcLabel ^ proc_is_imported },
	{ ProcIsExported = ProcLabel ^ proc_is_exported },
	(
		{ ProcIsImported = no },
		{ ProcIsExported = no }
	->
		io__write_string("static ")
	;
		(
			{ BeingDefined = yes }
		;
			{ BeingDefined = no },
			io__write_string("extern ")
		)
	),
	io__write_string("const "),
	io__write_string(proc_layout_kind_to_type(Kind)),
	io__write_string(" "),
	output_layout_name(proc_layout(ProcLabel, Kind)).
output_layout_name_storage_type_name(proc_layout_exec_trace(ProcLabel),
		_BeingDefined) -->
	io__write_string("static MR_STATIC_CODE_CONST MR_Exec_Trace\n\t"),
	output_layout_name(proc_layout_exec_trace(ProcLabel)).
output_layout_name_storage_type_name(proc_layout_head_var_nums(ProcLabel),
		_BeingDefined) -->
	io__write_string("static const "),
	io__write_string("MR_uint_least16_t "),
	output_layout_name(proc_layout_head_var_nums(ProcLabel)),
	io__write_string("[]").
output_layout_name_storage_type_name(proc_layout_var_names(ProcLabel),
		_BeingDefined) -->
	io__write_string("static const "),
	io__write_string("MR_uint_least32_t "),
	output_layout_name(proc_layout_var_names(ProcLabel)),
	io__write_string("[]").
output_layout_name_storage_type_name(closure_proc_id(CallerProcLabel, SeqNo,
		ClosureProcLabel), _BeingDefined) -->
	io__write_string("static const "),
	(
		{ ClosureProcLabel = proc(_, _, _, _, _, _) },
		io__write_string("MR_User_Closure_Id\n")
	;
		{ ClosureProcLabel = special_proc(_, _, _, _, _, _) },
		io__write_string("MR_UCI_Closure_Id\n")
	),
	output_layout_name(closure_proc_id(CallerProcLabel, SeqNo,
		ClosureProcLabel)).
output_layout_name_storage_type_name(file_layout(ModuleName, FileNum),
		_BeingDefined) -->
	io__write_string("static const MR_Module_File_Layout "),
	output_layout_name(file_layout(ModuleName, FileNum)).
output_layout_name_storage_type_name(file_layout_line_number_vector(
		ModuleName, FileNum), _BeingDefined) -->
	io__write_string("static const MR_int_least16_t "),
	output_layout_name(
		file_layout_line_number_vector(ModuleName, FileNum)),
	io__write_string("[]").
output_layout_name_storage_type_name(file_layout_label_layout_vector(
		ModuleName, FileNum), _BeingDefined) -->
	io__write_string("static const MR_Label_Layout *"),
	output_layout_name(
		file_layout_label_layout_vector(ModuleName, FileNum)),
	io__write_string("[]").
output_layout_name_storage_type_name(module_layout_string_table(ModuleName),
		_BeingDefined) -->
	io__write_string("static const char "),
	output_layout_name(module_layout_string_table(ModuleName)),
	io__write_string("[]").
output_layout_name_storage_type_name(module_layout_file_vector(ModuleName),
		_BeingDefined) -->
	io__write_string("static const MR_Module_File_Layout *"),
	output_layout_name(module_layout_file_vector(ModuleName)),
	io__write_string("[]").
output_layout_name_storage_type_name(module_layout_proc_vector(ModuleName),
		_BeingDefined) -->
	io__write_string("static const MR_Proc_Layout *"),
	output_layout_name(module_layout_proc_vector(ModuleName)),
	io__write_string("[]").
output_layout_name_storage_type_name(module_layout(ModuleName),
		_BeingDefined) -->
	io__write_string("static const MR_Module_Layout "),
	output_layout_name(module_layout(ModuleName)).
output_layout_name_storage_type_name(proc_static(RttiProcLabel),
		_BeingDefined) -->
	io__write_string("static MR_ProcStatic "),
	output_layout_name(proc_static(RttiProcLabel)).
output_layout_name_storage_type_name(proc_static_call_sites(RttiProcLabel),
		_BeingDefined) -->
	io__write_string("static const MR_CallSiteStatic "),
	output_layout_name(proc_static_call_sites(RttiProcLabel)),
	io__write_string("[]").
output_layout_name_storage_type_name(table_io_decl(RttiProcLabel),
		_BeingDefined) -->
	io__write_string("static const MR_Table_Io_Decl "),
	output_layout_name(table_io_decl(RttiProcLabel)).
output_layout_name_storage_type_name(table_gen_info(RttiProcLabel),
		_BeingDefined) -->
	io__write_string("static const MR_Table_Gen "),
	output_layout_name(table_gen_info(RttiProcLabel)).
output_layout_name_storage_type_name(table_gen_enum_params(RttiProcLabel),
		_BeingDefined) -->
	io__write_string("static const MR_Integer "),
	output_layout_name(table_gen_enum_params(RttiProcLabel)),
	io__write_string("[]").
output_layout_name_storage_type_name(table_gen_steps(RttiProcLabel),
		_BeingDefined) -->
	io__write_string("static const MR_Table_Trie_Step "),
	output_layout_name(table_gen_steps(RttiProcLabel)),
	io__write_string("[]").

layout_name_would_include_code_addr(label_layout(_, _, _)) = no.
layout_name_would_include_code_addr(proc_layout(_, _)) = no.
layout_name_would_include_code_addr(proc_layout_exec_trace(_)) = yes.
layout_name_would_include_code_addr(proc_layout_head_var_nums(_)) = no.
layout_name_would_include_code_addr(proc_layout_var_names(_)) = no.
layout_name_would_include_code_addr(closure_proc_id(_, _, _)) = no.
layout_name_would_include_code_addr(file_layout(_, _)) = no.
layout_name_would_include_code_addr(file_layout_line_number_vector(_, _)) = no.
layout_name_would_include_code_addr(file_layout_label_layout_vector(_, _)) = no.
layout_name_would_include_code_addr(module_layout_string_table(_)) = no.
layout_name_would_include_code_addr(module_layout_file_vector(_)) = no.
layout_name_would_include_code_addr(module_layout_proc_vector(_)) = no.
layout_name_would_include_code_addr(module_layout(_)) = no.
layout_name_would_include_code_addr(proc_static(_)) = no.
layout_name_would_include_code_addr(proc_static_call_sites(_)) = no.
layout_name_would_include_code_addr(table_io_decl(_)) = no.
layout_name_would_include_code_addr(table_gen_info(_)) = no.
layout_name_would_include_code_addr(table_gen_enum_params(_)) = no.
layout_name_would_include_code_addr(table_gen_steps(_)) = no.

:- func label_vars_to_type(label_vars) = string.

label_vars_to_type(label_has_var_info) =    "MR_Label_Layout".
label_vars_to_type(label_has_no_var_info) = "MR_Label_Layout_No_Var_Info".

:- func proc_layout_kind_to_type(proc_layout_kind) = string.

proc_layout_kind_to_type(proc_layout_traversal) =
	"MR_Proc_Layout_Traversal".
proc_layout_kind_to_type(proc_layout_proc_id(user)) =
	"MR_Proc_Layout_User".
proc_layout_kind_to_type(proc_layout_proc_id(uci)) =
	"MR_Proc_Layout_UCI".

%-----------------------------------------------------------------------------%

:- type rval_or_num_or_none
	--->	rval(rval)
	;	num(int)
	;	none.

:- pred output_rval_or_num_or_none(rval_or_num_or_none::in,
	io::di, io::uo) is det.

output_rval_or_num_or_none(rval(Rval), !IO) :-
	io__write_string(", ", !IO),
	output_rval_as_addr(Rval, !IO).
output_rval_or_num_or_none(num(Num), !IO) :-
	io__write_string(", ", !IO),
	io__write_int(Num, !IO).
output_rval_or_num_or_none(none, !IO).

:- pred output_label_layout_data_defn(proc_label::in, int::in, layout_name::in,
	maybe(trace_port)::in, maybe(bool)::in, maybe(int)::in,
	maybe(label_var_info)::in, decl_set::in, decl_set::out,
	io::di, io::uo) is det.

output_label_layout_data_defn(ProcLabel, LabelNum, ProcLayoutAddr, MaybePort,
		MaybeIsHidden, MaybeGoalPath, MaybeVarInfo, !DeclSet, !IO) :-
	output_layout_decl(ProcLayoutAddr, !DeclSet, !IO),
	(
		MaybeIsHidden = yes(yes),
		HiddenChars = "T"
	;
		MaybeIsHidden = yes(no),
		HiddenChars = ""
	;
		MaybeIsHidden = no,
		% The value of the hidden field shouldn't matter here.
		HiddenChars = ""
	),
	(
		MaybeVarInfo = yes(VarInfo0),
		VarInfo0 = label_var_info(EncodedVarCount1,
			LocnsTypes0, VarNums0, TypeParams0),
		output_rval_decls(LocnsTypes0, !DeclSet, !IO),
		output_rval_decls(VarNums0, !DeclSet, !IO),
		output_rval_decls(TypeParams0, !DeclSet, !IO),
		LabelVars = label_has_var_info,
		globals__io_lookup_bool_option(split_c_files, Split, !IO),
		(
			% With --split-c-files, the names of common cells
			% can't be of the form mercury_common_<n> (they have to
			% be module qualified), which contradicts the
			% assumptions of the CCC and CC0 variants of the
			% MR_DEF_LL macro.
			Split = no,
			LocnsTypes0 = const(data_addr_const(LTDataAddr, no)),
			LTDataAddr = data_addr(_, common(LTCellNum, _)),
			VarNums0 = const(data_addr_const(VNDataAddr, no)),
			VNDataAddr = data_addr(_, common(VNCellNum, _))
		->
			(
				TypeParams0 =
					const(data_addr_const(TPDataAddr, no)),
				TPDataAddr = data_addr(_, common(TPCellNum, _))
			->
				CommonChars = "CCC",
				LocnsTypes1 = num(LTCellNum),
				VarNums1 = num(VNCellNum),
				TypeParams1 = num(TPCellNum)
			;
				TypeParams0 = const(int_const(0))
			->
				CommonChars = "CC0",
				LocnsTypes1 = num(LTCellNum),
				VarNums1 = num(VNCellNum),
				TypeParams1 = none
			;
				CommonChars = "",
				LocnsTypes1 = rval(LocnsTypes0),
				VarNums1 = rval(VarNums0),
				TypeParams1 = rval(TypeParams0)
			)
		;
			CommonChars = "",
			LocnsTypes1 = rval(LocnsTypes0),
			VarNums1 = rval(VarNums0),
			TypeParams1 = rval(TypeParams0)
		),
		Macro = "MR_DEF_LL" ++ HiddenChars ++ CommonChars,
		MaybeVarInfoTuple = yes({EncodedVarCount1,
			LocnsTypes1, VarNums1, TypeParams1})
	;
		MaybeVarInfo = no,
		LabelVars = label_has_no_var_info,
		Macro = "MR_DEF_LLNVI" ++ HiddenChars,
		MaybeVarInfoTuple = no
	),
	LayoutName = label_layout(ProcLabel, LabelNum, LabelVars),
	io__write_string("\n", !IO),
	io__write_string(Macro, !IO),
	io__write_string("(", !IO),
	output_proc_label(ProcLabel, no, !IO),
	io__write_string(",\n", !IO),
	io__write_int(LabelNum, !IO),
	io__write_string(", ", !IO),
	(
		MaybePort = yes(Port),
		io__write_string(trace_port_to_string(Port), !IO)
	;
		MaybePort = no,
		io__write_string("NONE", !IO)
	),
	io__write_string(", ", !IO),
	(
		MaybeGoalPath = yes(GoalPath),
		io__write_int(GoalPath, !IO)
	;
		MaybeGoalPath = no,
		io__write_string("0", !IO)
	),
	(
		MaybeVarInfoTuple = yes({EncodedVarCount,
			LocnsTypes, VarNums, TypeParams}),
		io__write_string(", ", !IO),
		io__write_int(EncodedVarCount, !IO),
		output_rval_or_num_or_none(LocnsTypes, !IO),
		output_rval_or_num_or_none(VarNums, !IO),
		output_rval_or_num_or_none(TypeParams, !IO)
	;
		MaybeVarInfoTuple = no
	),
	io__write_string(");\n", !IO),
	decl_set_insert(data_addr(layout_addr(LayoutName)), !DeclSet).

	% Output the rval in a context in which it is immediately cast to an
	% address.
:- pred output_rval_as_addr(rval::in, io::di, io::uo) is det.

output_rval_as_addr(Rval, !IO) :-
	( Rval = const(int_const(0)) ->
		io__write_string(" 0", !IO)
	; Rval = const(data_addr_const(DataAddr, no)) ->
		io__write_string(" &", !IO),
		output_data_addr(DataAddr, !IO)
	;
		io__write_string("\n", !IO),
		output_rval(Rval, !IO)
	).

:- func trace_port_to_string(trace_port) = string.

trace_port_to_string(call) =	 	    "CALL".
trace_port_to_string(exit) = 		    "EXIT".
trace_port_to_string(redo) = 		    "REDO".
trace_port_to_string(fail) = 		    "FAIL".
trace_port_to_string(exception) = 	    "EXCEPTION".
trace_port_to_string(ite_cond) = 	    "COND".
trace_port_to_string(ite_then) = 	    "THEN".
trace_port_to_string(ite_else) = 	    "ELSE".
trace_port_to_string(neg_enter) =   	    "NEG_ENTER".
trace_port_to_string(neg_success) = 	    "NEG_SUCCESS".
trace_port_to_string(neg_failure) = 	    "NEG_FAILURE".
trace_port_to_string(disj) =   	            "DISJ".
trace_port_to_string(switch) = 	            "SWITCH".
trace_port_to_string(nondet_pragma_first) = "PRAGMA_FIRST".
trace_port_to_string(nondet_pragma_later) = "PRAGMA_LATER".

%-----------------------------------------------------------------------------%

:- pred output_proc_layout_data_defn(rtti_proc_label::in,
	proc_layout_stack_traversal::in, maybe_proc_id_and_more::in,
	decl_set::in, decl_set::out, io::di, io::uo) is det.

output_proc_layout_data_defn(RttiProcLabel, Traversal, MaybeRest,
		!DeclSet, !IO) :-
	output_layout_traversal_decls(Traversal, !DeclSet, !IO),
	ProcLabel = make_proc_label_from_rtti(RttiProcLabel),
	Kind = maybe_proc_layout_and_more_kind(MaybeRest, ProcLabel),
	(
		MaybeRest = no_proc_id,
		output_proc_layout_data_defn_start(RttiProcLabel, Kind,
			Traversal, !IO),
		output_layout_no_proc_id_group(!IO),
		output_proc_layout_data_defn_end(!IO)
	;
		MaybeRest = proc_id(MaybeProcStatic, MaybeExecTrace),
		(
			MaybeProcStatic = yes(ProcStatic),
			output_proc_static_data_defn(RttiProcLabel, ProcStatic,
				!DeclSet, !IO)
		;
			MaybeProcStatic = no
		),
		(
			MaybeExecTrace = yes(ExecTrace),
			HeadVarNums = ExecTrace ^ head_var_nums,
			output_proc_layout_head_var_nums(RttiProcLabel,
				HeadVarNums, !DeclSet, !IO),
			VarNames = ExecTrace ^ var_names,
			MaxVarNum = ExecTrace ^ max_var_num,
			output_proc_layout_var_names(RttiProcLabel, VarNames,
				MaxVarNum, !DeclSet, !IO),
			output_layout_exec_trace_decls(RttiProcLabel,
				ExecTrace, !DeclSet, !IO),
			output_layout_exec_trace(RttiProcLabel, ExecTrace,
				!DeclSet, !IO)
		;
			MaybeExecTrace = no
		),

		output_proc_layout_data_defn_start(RttiProcLabel, Kind,
			Traversal, !IO),
		output_layout_proc_id_group(ProcLabel, !IO),
		(
			MaybeExecTrace = no,
			io__write_string("NULL,\n", !IO)
		;
			MaybeExecTrace = yes(_),
			io__write_string("&", !IO),
			output_layout_name(
				proc_layout_exec_trace(RttiProcLabel), !IO),
			io__write_string(",\n", !IO)
		),
		(
			MaybeProcStatic = no,
			io__write_string("NULL\n", !IO)
		;
			MaybeProcStatic = yes(_),
			io__write_string("&", !IO),
			output_layout_name(proc_static(RttiProcLabel), !IO),
			io__write_string("\n", !IO)
		),
		output_proc_layout_data_defn_end(!IO)
	),
	decl_set_insert(data_addr(
		layout_addr(proc_layout(RttiProcLabel, Kind))), !DeclSet).

:- func maybe_proc_layout_and_more_kind(maybe_proc_id_and_more,
	proc_label) = proc_layout_kind.

maybe_proc_layout_and_more_kind(MaybeRest, ProcLabel) = Kind :-
	(
		MaybeRest = no_proc_id,
		Kind = proc_layout_traversal
	;
		MaybeRest = proc_id(_, _),
		Kind = proc_layout_proc_id(proc_label_user_or_uci(ProcLabel))
	).

proc_label_user_or_uci(proc(_, _, _, _, _, _)) = user.
proc_label_user_or_uci(special_proc(_, _, _, _, _, _)) = uci.

:- pred output_proc_layout_data_defn_start(rtti_proc_label::in,
	proc_layout_kind::in, proc_layout_stack_traversal::in,
	io::di, io::uo) is det.

output_proc_layout_data_defn_start(RttiProcLabel, Kind, Traversal) -->
	io__write_string("\n"),
	output_layout_name_storage_type_name(proc_layout(RttiProcLabel, Kind),
		yes),
	io__write_string(" = {\n"),
	output_layout_traversal_group(Traversal).

:- pred output_proc_layout_data_defn_end(io::di, io::uo) is det.

output_proc_layout_data_defn_end -->
	io__write_string("};\n").

:- pred output_layout_traversal_decls(proc_layout_stack_traversal::in,
	decl_set::in, decl_set::out, io::di, io::uo) is det.

output_layout_traversal_decls(Traversal, !DeclSet, !IO) :-
	Traversal = proc_layout_stack_traversal(MaybeEntryLabel,
		_MaybeSuccipSlot, _StackSlotCount, _Detism),
	(
		MaybeEntryLabel = yes(EntryLabel),
		output_code_addr_decls(label(EntryLabel), !DeclSet, !IO)
	;
		MaybeEntryLabel = no
	).

:- pred output_layout_traversal_group(proc_layout_stack_traversal::in,
	io::di, io::uo) is det.

output_layout_traversal_group(Traversal, !IO) :-
	Traversal = proc_layout_stack_traversal(MaybeEntryLabel,
		MaybeSuccipSlot, StackSlotCount, Detism),
	io__write_string("{\n", !IO),
	(
		MaybeEntryLabel = yes(EntryLabel),
		output_code_addr(label(EntryLabel), !IO)
	;
		MaybeEntryLabel = no,
		% The actual code address will be put into the structure
		% by module initialization code.
		io__write_string("NULL", !IO)
	),
	io__write_string(",\n", !IO),
	(
		MaybeSuccipSlot = yes(SuccipSlot),
		io__write_int(SuccipSlot, !IO)
	;
		MaybeSuccipSlot = no,
		io__write_int(-1, !IO)
	),
	io__write_string(",\n", !IO),
	io__write_int(StackSlotCount, !IO),
	io__write_string(",\n", !IO),
	io__write_string(detism_to_c_detism(Detism), !IO),
	io__write_string("\n},\n", !IO).

:- func detism_to_c_detism(determinism) = string.

detism_to_c_detism(det) =	  "MR_DETISM_DET".
detism_to_c_detism(semidet) =	  "MR_DETISM_SEMI".
detism_to_c_detism(nondet) =	  "MR_DETISM_NON".
detism_to_c_detism(multidet) =	  "MR_DETISM_MULTI".
detism_to_c_detism(erroneous) =	  "MR_DETISM_ERRONEOUS".
detism_to_c_detism(failure) =	  "MR_DETISM_FAILURE".
detism_to_c_detism(cc_nondet) =	  "MR_DETISM_CCNON".
detism_to_c_detism(cc_multidet) = "MR_DETISM_CCMULTI".

:- pred output_layout_proc_id_group(proc_label::in, io::di, io::uo) is det.

output_layout_proc_id_group(ProcLabel) -->
	io__write_string("{\n"),
	output_proc_id(ProcLabel),
	io__write_string("},\n").

:- pred output_layout_no_proc_id_group(io::di, io::uo) is det.

output_layout_no_proc_id_group -->
	io__write_string("-1\n").

:- pred output_layout_exec_trace_decls(rtti_proc_label::in,
	proc_layout_exec_trace::in, decl_set::in, decl_set::out,
	io::di, io::uo) is det.

output_layout_exec_trace_decls(RttiProcLabel, ExecTrace, !DeclSet, !IO) :-
	ExecTrace = proc_layout_exec_trace(CallLabelLayout, MaybeProcBody,
		MaybeTableInfo, _HeadVarNums, _VarNames, _MaxVarNum,
		_MaxRegNum, _MaybeFromFullSlot, _MaybeIoSeqSlot,
		_MaybeTrailSlot, _MaybeMaxfrSlot, _EvalMethod,
		_MaybeCallTableSlot),
	ProcLabel = make_proc_label_from_rtti(RttiProcLabel),
	ModuleName = get_defining_module_name(ProcLabel) ,
	output_layout_decl(CallLabelLayout, !DeclSet, !IO),
	output_layout_decl(module_layout(ModuleName), !DeclSet, !IO),
	(
		MaybeProcBody = yes(ProcBody),
		output_rval_decls(ProcBody, !DeclSet, !IO)
	;
		MaybeProcBody = no
	),
	(
		MaybeTableInfo = yes(TableInfo),
		output_layout_decl(TableInfo, !DeclSet, !IO)
	;
		MaybeTableInfo = no
	).

:- pred output_layout_exec_trace(rtti_proc_label::in,
	proc_layout_exec_trace::in, decl_set::in, decl_set::out,
	io::di, io::uo) is det.

output_layout_exec_trace(RttiProcLabel, ExecTrace, !DeclSet, !IO) :-
	ExecTrace = proc_layout_exec_trace(CallLabelLayout, MaybeProcBody,
		MaybeTableInfo, HeadVarNums, _VarNames, MaxVarNum,
		MaxRegNum, MaybeFromFullSlot, MaybeIoSeqSlot, MaybeTrailSlot,
		MaybeMaxfrSlot, EvalMethod, MaybeCallTableSlot),
	io__write_string("\n", !IO),
	output_layout_name_storage_type_name(
		proc_layout_exec_trace(RttiProcLabel), yes, !IO),
	io__write_string(" = {\nMR_LABEL_LAYOUT_REF(", !IO),
	( CallLabelLayout = label_layout(CallProcLabel, CallLabelNum, _) ->
		output_label(internal(CallLabelNum, CallProcLabel), no, !IO)
	;
		error("output_layout_exec_trace: bad call layout")
	),
	io__write_string("),\n(const MR_Module_Layout *) &", !IO),
	ProcLabel = make_proc_label_from_rtti(RttiProcLabel),
	ModuleName = get_defining_module_name(ProcLabel),
	output_layout_name(module_layout(ModuleName), !IO),
	io__write_string(",\n", !IO),
	(
		MaybeProcBody = yes(ProcBody),
		output_rval(ProcBody, !IO)
	;
		MaybeProcBody = no,
		io__write_int(0, !IO)
	),
	io__write_string(",\n", !IO),
	(
		MaybeCallTableSlot = yes(_),
		io__write_string("&", !IO),
		output_tabling_pointer_var_name(ProcLabel, !IO)
	;
		MaybeCallTableSlot = no,
		io__write_string("NULL", !IO)
	),
	io__write_string(",\n{ ", !IO),
	(
		MaybeTableInfo = yes(TableInfo),
		io__write_string("(const void *) &", !IO),
		output_layout_name(TableInfo, !IO)
	;
		MaybeTableInfo = no,
		io__write_string("NULL", !IO)
	),
	io__write_string(" },\n", !IO),
	output_layout_name(proc_layout_head_var_nums(RttiProcLabel), !IO),
	io__write_string(",\n", !IO),
	output_layout_name(proc_layout_var_names(RttiProcLabel), !IO),
	io__write_string(",\n", !IO),
	io__write_int(list__length(HeadVarNums), !IO),
	io__write_string(",\n", !IO),
	io__write_int(MaxVarNum, !IO),
	io__write_string(",\n", !IO),
	io__write_int(MaxRegNum, !IO),
	io__write_string(",\n", !IO),
	write_maybe_slot_num(MaybeFromFullSlot, !IO),
	io__write_string(",\n", !IO),
	write_maybe_slot_num(MaybeIoSeqSlot, !IO),
	io__write_string(",\n", !IO),
	write_maybe_slot_num(MaybeTrailSlot, !IO),
	io__write_string(",\n", !IO),
	write_maybe_slot_num(MaybeMaxfrSlot, !IO),
	io__write_string(",\n", !IO),
	io__write_string(eval_method_to_c_string(EvalMethod), !IO),
	io__write_string(",\n", !IO),
	write_maybe_slot_num(MaybeCallTableSlot, !IO),
	io__write_string("\n};\n", !IO).

:- pred write_maybe_slot_num(maybe(int)::in, io::di, io::uo) is det.

write_maybe_slot_num(yes(SlotNum)) -->
	io__write_int(SlotNum).
write_maybe_slot_num(no) -->
	io__write_int(-1).

:- func eval_method_to_c_string(eval_method) = string.

eval_method_to_c_string(eval_normal) =	      "MR_EVAL_METHOD_NORMAL".
eval_method_to_c_string(eval_loop_check) =    "MR_EVAL_METHOD_LOOP_CHECK".
eval_method_to_c_string(eval_memo) =          "MR_EVAL_METHOD_MEMO".
eval_method_to_c_string(eval_minimal) =	      "MR_EVAL_METHOD_MINIMAL".
eval_method_to_c_string(eval_table_io(Decl, Unitize)) = Str :-
	(
		Decl = table_io_proc,
		Unitize = table_io_alone,
		Str = "MR_EVAL_METHOD_TABLE_IO"
	;
		Decl = table_io_proc,
		Unitize = table_io_unitize,
		Str = "MR_EVAL_METHOD_TABLE_IO_UNITIZE"
	;
		Decl = table_io_decl,
		Unitize = table_io_alone,
		Str = "MR_EVAL_METHOD_TABLE_IO_DECL"
	;
		Decl = table_io_decl,
		Unitize = table_io_unitize,
		Str = "MR_EVAL_METHOD_TABLE_IO_UNITIZE_DECL"
	).

:- pred output_proc_layout_head_var_nums(rtti_proc_label::in, list(int)::in,
	decl_set::in, decl_set::out, io::di, io::uo) is det.

output_proc_layout_head_var_nums(ProcLabel, HeadVarNums, !DeclSet, !IO) :-
	io__write_string("\n", !IO),
	output_layout_name_storage_type_name(
		proc_layout_head_var_nums(ProcLabel), yes, !IO),
	io__write_string(" = {\n", !IO),
	(
		HeadVarNums = [],
			% ANSI/ISO C doesn't allow empty arrays, so
			% place a dummy value in the array.
		io__write_string("0\n", !IO)
	;
		HeadVarNums = [_ | _],
		list__foldl(output_number_in_vector, HeadVarNums, !IO)
	),
	io__write_string("};\n", !IO),
	decl_set_insert(data_addr(
		layout_addr(proc_layout_head_var_nums(ProcLabel))), !DeclSet).

:- pred output_proc_layout_var_names(rtti_proc_label::in, list(int)::in,
	int::in, decl_set::in, decl_set::out, io::di, io::uo) is det.

output_proc_layout_var_names(ProcLabel, VarNames, MaxVarNum, !DeclSet, !IO) :-
	list__length(VarNames, VarNameCount),
	require(unify(VarNameCount, MaxVarNum),
		"output_proc_layout_var_names: VarNameCount != MaxVarNum"),
	io__write_string("\n", !IO),
	output_layout_name_storage_type_name(proc_layout_var_names(ProcLabel),
		yes, !IO),
	io__write_string(" = {\n", !IO),
	(
		VarNames = [],
			% ANSI/ISO C doesn't allow empty arrays, so
			% place a dummy value in the array.
		io__write_string("0\n", !IO)
	;
		VarNames = [_ | _],
		list__foldl(output_number_in_vector, VarNames, !IO)
	),
	io__write_string("};\n", !IO),
	decl_set_insert(data_addr(
		layout_addr(proc_layout_var_names(ProcLabel))), !DeclSet).

%-----------------------------------------------------------------------------%

:- pred output_closure_proc_id_data_defn(proc_label::in, int::in,
	proc_label::in, module_name::in, string::in, int::in, string::in,
	decl_set::in, decl_set::out, io::di, io::uo) is det.

output_closure_proc_id_data_defn(CallerProcLabel, SeqNo, ClosureProcLabel,
		ModuleName, FileName, LineNumber, GoalPath,
		!DeclSet) -->
	io__write_string("\n"),
	{ LayoutName = closure_proc_id(CallerProcLabel, SeqNo,
		ClosureProcLabel) },
	output_layout_name_storage_type_name(LayoutName, yes),
	io__write_string(" = {\n{\n"),
	output_proc_id(ClosureProcLabel),
	io__write_string("},\n"),
	{ prog_out__sym_name_to_string(ModuleName, ModuleNameStr) },
	quote_and_write_string(ModuleNameStr),
	io__write_string(",\n"),
	quote_and_write_string(FileName),
	io__write_string(",\n"),
	io__write_int(LineNumber),
	io__write_string(",\n"),
	quote_and_write_string(GoalPath),
	io__write_string("\n};\n"),
	{ decl_set_insert(data_addr(layout_addr(LayoutName)),
		!DeclSet) }.

:- pred output_proc_id(proc_label::in, io::di, io::uo) is det.

output_proc_id(ProcLabel) -->
	(
		{ ProcLabel = proc(DefiningModule, PredOrFunc, DeclaringModule,
			Name, Arity, Mode) },
		{ prog_out__sym_name_to_string(DefiningModule,
			DefiningModuleStr) },
		{ prog_out__sym_name_to_string(DeclaringModule,
			DeclaringModuleStr) },
		{ proc_id_to_int(Mode, ModeInt) },
		output_pred_or_func(PredOrFunc),
		io__write_string(",\n"),
		quote_and_write_string(DeclaringModuleStr),
		io__write_string(",\n"),
		quote_and_write_string(DefiningModuleStr),
		io__write_string(",\n"),
		quote_and_write_string(Name),
		io__write_string(",\n"),
		io__write_int(Arity),
		io__write_string(",\n"),
		io__write_int(ModeInt),
		io__write_string("\n")
	;
		{ ProcLabel = special_proc(DefiningModule, SpecialPredId,
			TypeModule, TypeName, TypeArity, Mode) },
		{ prog_out__sym_name_to_string(DefiningModule,
			DefiningModuleStr) },
		{ prog_out__sym_name_to_string(TypeModule, TypeModuleStr) },
		{ proc_id_to_int(Mode, ModeInt) },
		quote_and_write_string(TypeName),
		io__write_string(",\n"),
		quote_and_write_string(TypeModuleStr),
		io__write_string(",\n"),
		quote_and_write_string(DefiningModuleStr),
		io__write_string(",\n"),
		{ TypeCtor = qualified(TypeModule, TypeName) - TypeArity },
		{ PredName = special_pred_name(SpecialPredId, TypeCtor) },
		quote_and_write_string(PredName),
		io__write_string(",\n"),
		io__write_int(TypeArity),
		io__write_string(",\n"),
		io__write_int(ModeInt),
		io__write_string("\n")
	).

%-----------------------------------------------------------------------------%

:- pred output_module_layout_data_defn(module_name::in, int::in,
	string_with_0s::in, list(layout_name)::in, list(file_layout_data)::in,
	trace_level::in, int::in, decl_set::in, decl_set::out,
	io::di, io::uo) is det.

output_module_layout_data_defn(ModuleName, StringTableSize, StringTable,
		ProcLayoutNames, FileLayouts, TraceLevel, SuppressedEvents,
		!DeclSet, !IO) :-
	output_module_string_table(ModuleName, StringTableSize, StringTable,
		!DeclSet, !IO),
	output_module_layout_proc_vector_defn(ModuleName, ProcLayoutNames,
		ProcVectorName, !DeclSet, !IO),
	output_file_layout_data_defns(ModuleName, 0, FileLayouts,
		FileLayoutNames, !DeclSet, !IO),
	output_file_layout_vector_data_defn(ModuleName, FileLayoutNames,
		FileVectorName, !DeclSet, !IO),

	ModuleLayoutName = module_layout(ModuleName),
	io__write_string("\n", !IO),
	output_layout_name_storage_type_name(ModuleLayoutName, yes, !IO),
	io__write_string(" = {\n", !IO),
	prog_out__sym_name_to_string(ModuleName, ModuleNameStr),
	quote_and_write_string(ModuleNameStr, !IO),
	io__write_string(",\n", !IO),
	io__write_int(StringTableSize, !IO),
	io__write_string(",\n", !IO),
	ModuleStringTableName = module_layout_string_table(ModuleName),
	output_layout_name(ModuleStringTableName, !IO),
	io__write_string(",\n", !IO),
	list__length(ProcLayoutNames, ProcLayoutVectorLength),
	io__write_int(ProcLayoutVectorLength, !IO),
	io__write_string(",\n", !IO),
	output_layout_name(ProcVectorName, !IO),
	io__write_string(",\n", !IO),
	list__length(FileLayouts, FileLayoutVectorLength),
	io__write_int(FileLayoutVectorLength, !IO),
	io__write_string(",\n", !IO),
	output_layout_name(FileVectorName, !IO),
	io__write_string(",\n", !IO),
	io__write_string(trace_level_rep(TraceLevel), !IO),
	io__write_string(",\n", !IO),
	io__write_int(SuppressedEvents, !IO),
	io__write_string("\n};\n", !IO),
	decl_set_insert(data_addr(layout_addr(ModuleLayoutName)), !DeclSet).

:- pred output_module_layout_proc_vector_defn(module_name::in,
	list(layout_name)::in, layout_name::out, decl_set::in, decl_set::out,
	io::di, io::uo) is det.

output_module_layout_proc_vector_defn(ModuleName, ProcLayoutNames,
		VectorName, !DeclSet, !IO) :-
	list__foldl2(output_layout_decl, ProcLayoutNames, !DeclSet, !IO),
	VectorName = module_layout_proc_vector(ModuleName),
	io__write_string("\n", !IO),
	output_layout_name_storage_type_name(VectorName, yes, !IO),
	io__write_string(" = {\n", !IO),
	( ProcLayoutNames = [] ->
			% ANSI/ISO C doesn't allow empty arrays, so
			% place a dummy value in the array.
		io__write_string("NULL\n", !IO)
	;
		list__foldl(output_proc_layout_name_in_vector, ProcLayoutNames,
			!IO)
	),
	io__write_string("};\n", !IO),
	decl_set_insert(data_addr(layout_addr(VectorName)), !DeclSet).

:- pred output_proc_layout_name_in_vector(layout_name::in, io::di, io::uo)
	is det.

output_proc_layout_name_in_vector(LayoutName, !IO) :-
	( LayoutName = proc_layout(RttiProcLabel, _) ->
		ProcLabel = make_proc_label_from_rtti(RttiProcLabel),
		io__write_string("MR_PROC_LAYOUT1(", !IO),
		output_proc_label(ProcLabel, no, !IO),
		io__write_string(")\n", !IO)
	;
		error("output_proc_layout_name_in_vector: not proc layout")
	).

%-----------------------------------------------------------------------------%

	% The string table cannot be zero size; it must contain at least an
	% empty string.

:- pred output_module_string_table(module_name::in,
	int::in, string_with_0s::in, decl_set::in, decl_set::out,
	io::di, io::uo) is det.

output_module_string_table(ModuleName, StringTableSize, StringTable,
		!DeclSet) -->
	{ TableName = module_layout_string_table(ModuleName) },
	io__write_string("\n"),
	output_layout_name_storage_type_name(TableName, yes),
	io__write_string(" = {"),
	output_module_string_table_chars_driver(0, StringTableSize - 1,
		StringTable),
	io__write_string("};\n"),
	{ decl_set_insert(data_addr(layout_addr(TableName)),
		!DeclSet) }.

% The jobs of this predicate is to minimize stack space consumption in
% grades that do not allow output_module_string_table_chars to be tail
% recursive. The maximum observed size of the module string so far has
% been just short of 64 kilobytes; writing that out in 256 batches of 256
% characters minimizes maximum total stack requirements.

:- pred output_module_string_table_chars_driver(int::in, int::in,
	string_with_0s::in, io::di, io::uo) is det.

output_module_string_table_chars_driver(CurIndex, MaxIndex, StringWithNulls) -->
	( { CurIndex < MaxIndex } ->
		{ SubMaxIndex = int__min(MaxIndex, CurIndex + 255) },
		output_module_string_table_chars(CurIndex, SubMaxIndex,
			StringWithNulls),
		output_module_string_table_chars_driver(SubMaxIndex + 1,
			MaxIndex, StringWithNulls)
	;
		[]
	).

:- pred output_module_string_table_chars(int::in, int::in, string_with_0s::in,
	io::di, io::uo) is det.

output_module_string_table_chars(CurIndex, MaxIndex, StringWithNulls) -->
	( { CurIndex mod 10 = 0 } ->
		io__write_string("\n")
	;
		[]
	),
	{ StringWithNulls = string_with_0s(String) },
	{ string__unsafe_index(String, CurIndex, Char) },
	io__write_char(''''),
	c_util__output_quoted_char(Char),
	io__write_char(''''),
	io__write_string(", "),
	( { CurIndex < MaxIndex } ->
		output_module_string_table_chars(CurIndex + 1, MaxIndex,
			StringWithNulls)
	;
		[]
	).

%-----------------------------------------------------------------------------%

:- pred output_file_layout_vector_data_defn(module_name::in,
	list(layout_name)::in, layout_name::out, decl_set::in, decl_set::out,
	io::di, io::uo) is det.

output_file_layout_vector_data_defn(ModuleName, FileLayoutNames, VectorName,
		!DeclSet, !IO) :-
	list__foldl2(output_layout_decl, FileLayoutNames, !DeclSet, !IO),
	VectorName = module_layout_file_vector(ModuleName),
	io__write_string("\n", !IO),
	output_layout_name_storage_type_name(VectorName, yes, !IO),
	io__write_string(" = {\n", !IO),
	( FileLayoutNames = [] ->
			% ANSI/ISO C doesn't allow empty arrays, so
			% place a dummy value in the array.
		io__write_string("NULL\n", !IO)
	;
		list__foldl(output_layout_name_in_vector("&"), FileLayoutNames,
			!IO)
	),
	io__write_string("};\n", !IO),
	decl_set_insert(data_addr(layout_addr(VectorName)), !DeclSet).

:- pred output_file_layout_data_defns(module_name::in, int::in,
	list(file_layout_data)::in, list(layout_name)::out,
	decl_set::in, decl_set::out, io::di, io::uo) is det.

output_file_layout_data_defns(_, _, [], [], !DeclSet) --> [].
output_file_layout_data_defns(ModuleName, FileNum, [FileLayout | FileLayouts],
		[FileLayoutName | FileLayoutNames], !DeclSet) -->
	output_file_layout_data_defn(ModuleName, FileNum, FileLayout,
		FileLayoutName, !DeclSet),
	output_file_layout_data_defns(ModuleName, FileNum + 1, FileLayouts,
		FileLayoutNames, !DeclSet).

:- pred output_file_layout_data_defn(module_name::in, int::in,
	file_layout_data::in, layout_name::out,
	decl_set::in, decl_set::out, io::di, io::uo) is det.

output_file_layout_data_defn(ModuleName, FileNum, FileLayout, FileLayoutName,
		!DeclSet, !IO) :-
	FileLayout = file_layout_data(FileName, LineNoLabelList),
	list__map2(line_no_label_to_label_layout_addr, LineNoLabelList,
		LineNos, LabelLayoutAddrs),
	list__foldl2(output_data_addr_decls, LabelLayoutAddrs, !DeclSet, !IO),

	list__length(LineNoLabelList, VectorLengths),
	output_file_layout_line_number_vector_defn(ModuleName, FileNum,
		LineNos, LineNumberVectorName, !DeclSet, !IO),
	output_file_layout_label_layout_vector_defn(ModuleName, FileNum,
		LabelLayoutAddrs, LabelVectorName, !DeclSet, !IO),

	FileLayoutName = file_layout(ModuleName, FileNum),
	io__write_string("\n", !IO),
	output_layout_name_storage_type_name(FileLayoutName, yes, !IO),
	io__write_string(" = {\n", !IO),
	quote_and_write_string(FileName, !IO),
	io__write_string(",\n", !IO),
	io__write_int(VectorLengths, !IO),
	io__write_string(",\n", !IO),
	output_layout_name(LineNumberVectorName, !IO),
	io__write_string(",\n", !IO),
	output_layout_name(LabelVectorName, !IO),
	io__write_string("\n};\n", !IO),
	decl_set_insert(data_addr(layout_addr(FileLayoutName)), !DeclSet).

:- pred output_file_layout_line_number_vector_defn(module_name::in, int::in,
	list(int)::in, layout_name::out, decl_set::in, decl_set::out,
	io::di, io::uo) is det.

output_file_layout_line_number_vector_defn(ModuleName, FileNum, LineNumbers,
		LayoutName, !DeclSet, !IO) :-
	LayoutName = file_layout_line_number_vector(ModuleName, FileNum),
	io__write_string("\n", !IO),
	output_layout_name_storage_type_name(LayoutName, yes, !IO),
	io__write_string(" = {\n", !IO),
	( LineNumbers = [] ->
			% ANSI/ISO C doesn't allow empty arrays, so
			% place a dummy value in the array.
		io__write_string("0\n", !IO)
	;
		list__foldl(output_number_in_vector, LineNumbers, !IO)
	),
	io__write_string("};\n", !IO),
	decl_set_insert(data_addr(layout_addr(LayoutName)), !DeclSet).

:- pred output_file_layout_label_layout_vector_defn(module_name::in, int::in,
	list(data_addr)::in, layout_name::out, decl_set::in, decl_set::out,
	io::di, io::uo) is det.

output_file_layout_label_layout_vector_defn(ModuleName, FileNum, LabelAddrs,
		LayoutName, !DeclSet, !IO) :-
	LayoutName = file_layout_label_layout_vector(ModuleName, FileNum),
	io__write_string("\n", !IO),
	output_layout_name_storage_type_name(LayoutName, yes, !IO),
	io__write_string(" = {\n", !IO),
	( LabelAddrs = [] ->
			% ANSI/ISO C doesn't allow empty arrays, so
			% place a dummy value in the array.
		io__write_string("NULL\n", !IO)
	;
		list__map(project_label_layout, LabelAddrs, Labels),
		output_label_layout_addrs_in_vector(Labels, !IO)
	),
	io__write_string("};\n", !IO),
	decl_set_insert(data_addr(layout_addr(LayoutName)), !DeclSet).

:- pred project_label_layout(data_addr::in, label::out) is det.

project_label_layout(DataAddr, Label) :-
	(
		DataAddr = layout_addr(LayoutName),
		LayoutName = label_layout(ProcLabel, LabelNum, _)
	->
		Label = internal(LabelNum, ProcLabel)
	;
		error("project_label_layout: not label layout")
	).

:- pred output_label_layout_addrs_in_vector(list(label)::in, io::di, io::uo)
	is det.

output_label_layout_addrs_in_vector([], !IO).
output_label_layout_addrs_in_vector([Label | Labels], !IO) :-
	(
		Label = internal(LabelNum, ProcLabel),
		groupable_labels(ProcLabel, 1, N, [LabelNum], RevLabelNums,
			Labels, RemainingLabels),
		N > 1
	->
		list__reverse(RevLabelNums, LabelNums),
		io__write_string("MR_LABEL_LAYOUT", !IO),
		io__write_int(list__length(LabelNums), !IO),
		io__write_string("(", !IO),
		output_proc_label(ProcLabel, no, !IO),
		io__write_string(",", !IO),
		io__write_list(LabelNums, ",", io__write_int, !IO),
		io__write_string(")\n", !IO),
		output_label_layout_addrs_in_vector(RemainingLabels, !IO)
	;
		io__write_string("MR_LABEL_LAYOUT(", !IO),
		output_label(Label, !IO),
		io__write_string("),\n", !IO),
		output_label_layout_addrs_in_vector(Labels, !IO)
	).

:- pred groupable_labels(proc_label::in, int::in, int::out,
	list(int)::in, list(int)::out, list(label)::in, list(label)::out)
	is det.

groupable_labels(ProcLabel, !Count, !RevLabelsNums, !Labels) :-
	(
		% There must be a macro of the form MR_LABEL_LAYOUT<n>
		% for every <n> up to MaxChunkSize.
		!.Labels = [Label | !:Labels],
		MaxChunkSize = 9,
		!.Count < MaxChunkSize,	% leave room for the one we're adding
		Label = internal(LabelNum, ProcLabel)
	->
		!:Count = !.Count + 1,
		!:RevLabelsNums = [LabelNum | !.RevLabelsNums],
		groupable_labels(ProcLabel, !Count, !RevLabelsNums, !Labels)
	;
		true
	).

%-----------------------------------------------------------------------------%

:- pred line_no_label_to_label_layout_addr(pair(int, layout_name)::in,
	int::out, data_addr::out) is det.

line_no_label_to_label_layout_addr(LineNo - LabelLayout, LineNo, DataAddr) :-
	DataAddr = layout_addr(LabelLayout).

:- pred quote_and_write_string(string::in, io::di, io::uo) is det.

quote_and_write_string(String) -->
	io__write_string(""""),
	c_util__output_quoted_string(String),
	io__write_string("""").

:- pred output_number_in_vector(int::in, io::di, io::uo) is det.

output_number_in_vector(Num) -->
	io__write_int(Num),
	io__write_string(",\n").

:- pred output_layout_name_in_vector(string::in, layout_name::in,
	io::di, io::uo) is det.

output_layout_name_in_vector(Prefix, Name) -->
	io__write_string(Prefix),
	output_layout_name(Name),
	io__write_string(",\n").

:- pred output_data_addr_in_vector(string::in, data_addr::in,
	io::di, io::uo) is det.

output_data_addr_in_vector(Prefix, DataAddr) -->
	io__write_string(Prefix),
	output_data_addr(DataAddr),
	io__write_string(",\n").

%-----------------------------------------------------------------------------%

:- pred output_proc_static_data_defn(rtti_proc_label::in,
	proc_layout_proc_static::in, decl_set::in, decl_set::out,
	io::di, io::uo) is det.

output_proc_static_data_defn(RttiProcLabel, ProcLayoutProcStatic,
		!DeclSet, !IO) :-
	ProcLayoutProcStatic = proc_layout_proc_static(HLDSProcStatic,
		DeepExcpVars),
	HLDSProcStatic = hlds_proc_static(FileName, LineNumber, IsInInterface,
		CallSites),
	list__foldl2(output_call_site_static_decl, CallSites, !DeclSet, !IO),
	output_call_site_static_array(RttiProcLabel, CallSites, !DeclSet, !IO),
	LayoutName = proc_static(RttiProcLabel),
	io__write_string("\n", !IO),
	output_layout_name_storage_type_name(LayoutName, yes, !IO),
	io__write_string(" = {\n", !IO),
	quote_and_write_string(FileName, !IO),
	io__write_string(",\n", !IO),
	io__write_int(LineNumber, !IO),
	io__write_string(",\n", !IO),
	(
		IsInInterface = yes,
		io__write_string("MR_TRUE", !IO)
	;
		IsInInterface = no,
		io__write_string("MR_FALSE", !IO)
	),
	io__write_string(",\n", !IO),
	io__write_int(list__length(CallSites), !IO),
	io__write_string(",\n", !IO),
	CallSitesLayoutName = proc_static_call_sites(RttiProcLabel),
	output_layout_name(CallSitesLayoutName, !IO),
	io__write_string(",\n#ifdef MR_USE_ACTIVATION_COUNTS\n", !IO),
	io__write_string("0,\n", !IO),
	io__write_string("#endif\n", !IO),
	io__write_string("NULL,\n", !IO),
	DeepExcpVars = deep_excp_slots(TopCSDSlot, MiddleCSDSlot,
		OldOutermostSlot),
	io__write_int(TopCSDSlot, !IO),
	io__write_string(",\n\t", !IO),
	io__write_int(MiddleCSDSlot, !IO),
	io__write_string(",\n\t", !IO),
	io__write_int(OldOutermostSlot, !IO),
	io__write_string("\n};\n", !IO),
	decl_set_insert(data_addr(layout_addr(LayoutName)), !DeclSet).

:- pred output_call_site_static_array(rtti_proc_label::in,
	list(call_site_static_data)::in, decl_set::in, decl_set::out,
	io::di, io::uo) is det.

output_call_site_static_array(RttiProcLabel, CallSites, !DeclSet, !IO) :-
	LayoutName = proc_static_call_sites(RttiProcLabel),
	io__write_string("\n", !IO),
	output_layout_name_storage_type_name(LayoutName, yes, !IO),
	io__write_string(" = {\n", !IO),
	list__foldl2(output_call_site_static, CallSites, 0, _, !IO),
	io__write_string("};\n", !IO),
	decl_set_insert(data_addr(layout_addr(LayoutName)), !DeclSet).

:- pred output_call_site_static(call_site_static_data::in, int::in, int::out,
	io::di, io::uo) is det.

output_call_site_static(CallSiteStatic, Index, Index + 1, !IO) :-
	io__write_string("{ /* ", !IO),
	io__write_int(Index, !IO),
	io__write_string(" */ ", !IO),
	(
		CallSiteStatic = normal_call(Callee, TypeSubst,
			FileName, LineNumber, GoalPath),
		io__write_string("MR_normal_call, (MR_Proc_Layout *)\n&",
			!IO),
		CalleeProcLabel = make_proc_label_from_rtti(Callee),
		CalleeUserOrUci = proc_label_user_or_uci(CalleeProcLabel),
		output_layout_name(proc_layout(Callee,
			proc_layout_proc_id(CalleeUserOrUci)), !IO),
		( TypeSubst = "" ->
			io__write_string(", NULL, ", !IO)
		;
			io__write_string(",\n""", !IO),
			io__write_string(TypeSubst, !IO),
			io__write_string(""", ", !IO)
		)
	;
		CallSiteStatic = special_call(FileName, LineNumber,
			GoalPath),
		io__write_string("MR_special_call, NULL, NULL, ", !IO)
	;
		CallSiteStatic = higher_order_call(FileName, LineNumber,
			GoalPath),
		io__write_string("MR_higher_order_call, NULL, NULL, ", !IO)
	;
		CallSiteStatic = method_call(FileName, LineNumber, GoalPath),
		io__write_string("MR_method_call, NULL, NULL, ", !IO)
	;
		CallSiteStatic = callback(FileName, LineNumber, GoalPath),
		io__write_string("MR_callback, NULL, NULL, ", !IO)
	),
	io__write_string("""", !IO),
	io__write_string(FileName, !IO),
	io__write_string(""", ", !IO),
	io__write_int(LineNumber, !IO),
	io__write_string(", """, !IO),
	goal_path_to_string(GoalPath, GoalPathStr),
	io__write_string(GoalPathStr, !IO),
	io__write_string(""" },\n", !IO).

:- pred output_call_site_static_decl(call_site_static_data::in,
	decl_set::in, decl_set::out, io::di, io::uo) is det.

output_call_site_static_decl(CallSiteStatic, !DeclSet, !IO) :-
	(
		CallSiteStatic = normal_call(Callee, _, _, _, _),
		CalleeProcLabel = make_proc_label_from_rtti(Callee),
		CalleeUserOrUci = proc_label_user_or_uci(CalleeProcLabel),
		output_maybe_layout_name_decl(proc_layout(Callee,
			proc_layout_proc_id(CalleeUserOrUci)),
			!DeclSet, !IO)
	;
		CallSiteStatic = special_call(_, _, _)
	;
		CallSiteStatic = higher_order_call(_, _, _)
	;
		CallSiteStatic = method_call(_, _, _)
	;
		CallSiteStatic = callback(_, _, _)
	).

%-----------------------------------------------------------------------------%

:- pred output_table_io_decl(rtti_proc_label::in, proc_layout_kind::in,
	int::in, rval::in, rval::in, decl_set::in, decl_set::out,
	io::di, io::uo) is det.

output_table_io_decl(RttiProcLabel, ProcLayoutKind, NumPTIs,
		PTIVectorRval, TypeParamsRval, !DeclSet, !IO) :-
	output_rval_decls(PTIVectorRval, !DeclSet, !IO),
	LayoutName = table_io_decl(RttiProcLabel),
	ProcLayoutName = proc_layout(RttiProcLabel, ProcLayoutKind),
	output_layout_decl(ProcLayoutName, !DeclSet, !IO),

	io__write_string("\n", !IO),
	output_layout_name_storage_type_name(LayoutName, yes, !IO),
	io__write_string(" = {\n(const MR_Proc_Layout *) &", !IO),
	output_layout_name(ProcLayoutName, !IO),
	io__write_string(",\n", !IO),
	io__write_int(NumPTIs, !IO),
	io__write_string(",\n(const MR_PseudoTypeInfo *) ", !IO),
	output_rval(PTIVectorRval, !IO),
	io__write_string(",\n(const MR_Type_Param_Locns *) ", !IO),
	output_rval(TypeParamsRval, !IO),
	io__write_string("\n};\n", !IO),
	decl_set_insert(data_addr(layout_addr(LayoutName)), !DeclSet).

:- pred output_table_gen(rtti_proc_label::in, int::in, int::in,
	list(table_trie_step)::in, rval::in, rval::in,
	decl_set::in, decl_set::out, io::di, io::uo) is det.

output_table_gen(RttiProcLabel, NumInputs, NumOutputs, Steps,
		PTIVectorRval, TypeParamsRval, !DeclSet, !IO) :-
	output_table_gen_steps_table(RttiProcLabel, Steps, MaybeEnumParams,
		!DeclSet, !IO),
	output_table_gen_enum_params_table(RttiProcLabel, MaybeEnumParams,
		!DeclSet, !IO),
	output_rval_decls(PTIVectorRval, !DeclSet, !IO),
	LayoutName = table_gen_info(RttiProcLabel),
	io__write_string("\n", !IO),
	output_layout_name_storage_type_name(LayoutName, yes, !IO),
	io__write_string(" = {\n", !IO),
	io__write_int(NumInputs, !IO),
	io__write_string(",\n", !IO),
	io__write_int(NumOutputs, !IO),
	io__write_string(",\n", !IO),
	output_layout_name(table_gen_steps(RttiProcLabel), !IO),
	io__write_string(",\n", !IO),
	output_layout_name(table_gen_enum_params(RttiProcLabel), !IO),
	io__write_string(",\n(const MR_PseudoTypeInfo *)\n", !IO),
	output_rval(PTIVectorRval, !IO),
	io__write_string(",\n(const MR_Type_Param_Locns *)\n", !IO),
	output_rval(TypeParamsRval, !IO),
	io__write_string("\n};\n", !IO),
	decl_set_insert(data_addr(layout_addr(LayoutName)), !DeclSet).

:- pred output_table_gen_steps_table(rtti_proc_label::in,
	list(table_trie_step)::in, list(maybe(int))::out,
	decl_set::in, decl_set::out, io::di, io::uo) is det.

output_table_gen_steps_table(RttiProcLabel, Steps, MaybeEnumParams,
		!DeclSet, !IO) :-
	LayoutName = table_gen_steps(RttiProcLabel),
	io__write_string("\n", !IO),
	output_layout_name_storage_type_name(LayoutName, yes, !IO),
	io__write_string(" = {\n", !IO),
	output_table_gen_steps(Steps, MaybeEnumParams, !IO),
	io__write_string("};\n", !IO),
	decl_set_insert(data_addr(layout_addr(LayoutName)), !DeclSet).

:- pred output_table_gen_steps(list(table_trie_step)::in,
	list(maybe(int))::out, io::di, io::uo) is det.

output_table_gen_steps([], [], !IO).
output_table_gen_steps([Step | Steps], [MaybeEnumParam | MaybeEnumParams],
		!IO) :-
	(
		Step = table_trie_step_int,
		StepType = "MR_TABLE_STEP_INT",
		MaybeEnumParam = no
	;
		Step = table_trie_step_char,
		StepType = "MR_TABLE_STEP_CHAR",
		MaybeEnumParam = no
	;
		Step = table_trie_step_string,
		StepType = "MR_TABLE_STEP_STRING",
		MaybeEnumParam = no
	;
		Step = table_trie_step_float,
		StepType = "MR_TABLE_STEP_FLOAT",
		MaybeEnumParam = no
	;
		Step = table_trie_step_enum(EnumRange),
		StepType = "MR_TABLE_STEP_ENUM",
		MaybeEnumParam = yes(EnumRange)
	;
		Step = table_trie_step_user(_),
		StepType = "MR_TABLE_STEP_USER",
		MaybeEnumParam = no
	;
		Step = table_trie_step_poly,
		StepType = "MR_TABLE_STEP_POLY",
		MaybeEnumParam = no
	;
		Step = table_trie_step_typeinfo,
		StepType = "MR_TABLE_STEP_TYPEINFO",
		MaybeEnumParam = no
	;
		Step = table_trie_step_typeclassinfo,
		StepType = "MR_TABLE_STEP_TYPECLASSINFO",
		MaybeEnumParam = no
	),
	io__write_string(StepType, !IO),
	io__write_string(",\n", !IO),
	output_table_gen_steps(Steps, MaybeEnumParams, !IO).

:- pred output_table_gen_enum_params_table(rtti_proc_label::in,
	list(maybe(int))::in, decl_set::in, decl_set::out,
	io::di, io::uo) is det.

output_table_gen_enum_params_table(RttiProcLabel, MaybeEnumParams,
		!DeclSet, !IO) :-
	LayoutName = table_gen_enum_params(RttiProcLabel),
	io__write_string("\n", !IO),
	output_layout_name_storage_type_name(LayoutName, yes, !IO),
	io__write_string(" = {\n", !IO),
	output_table_gen_enum_params(MaybeEnumParams, !IO),
	io__write_string("};\n", !IO),
	decl_set_insert(data_addr(layout_addr(LayoutName)), !DeclSet).

:- pred output_table_gen_enum_params(list(maybe(int))::in,
	io::di, io::uo) is det.

output_table_gen_enum_params([], !IO).
output_table_gen_enum_params([MaybeEnumParam | MaybeEnumParams], !IO) :-
	(
		MaybeEnumParam = no,
		io__write_int(-1, !IO)
	;
		MaybeEnumParam = yes(EnumRange),
		io__write_int(EnumRange, !IO)
	),
	io__write_string(",\n", !IO),
	output_table_gen_enum_params(MaybeEnumParams, !IO).

%-----------------------------------------------------------------------------%

output_pred_or_func(PredOrFunc, !IO) :-
	(
		PredOrFunc = predicate,
		io__write_string("MR_PREDICATE", !IO)
	;
		PredOrFunc = function,
		io__write_string("MR_FUNCTION", !IO)
	).

%-----------------------------------------------------------------------------%
