%-----------------------------------------------------------------------------%
% Copyright (C) 2001-2002 The University of Melbourne.
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

:- import_module ll_backend__layout, ll_backend__llds, ll_backend__llds_out.
:- import_module bool, io.

	% Given a Mercury representation of a layout structure, output its
	% definition in the appropriate C global variable.
:- pred output_layout_data_defn(layout_data::in, decl_set::in, decl_set::out,
	io__state::di, io__state::uo) is det.

	% Given the name of a layout structure, output the declaration
	% of the C global variable which will hold it.
:- pred output_layout_name_decl(layout_name::in, io__state::di, io__state::uo)
	is det.

	% Given the name of a layout structure, output the declaration
	% of the C global variable which will hold it, if it has
	% not already been declared.
:- pred output_maybe_layout_name_decl(layout_name::in,
	decl_set::in, decl_set::out, io__state::di, io__state::uo) is det.

	% Given a Mercury representation of a layout structure, output the
	% declaration of the C global variable which will hold it, if it has
	% not already been declared.
:- pred output_maybe_layout_data_decl(layout_data::in,
	decl_set::in, decl_set::out, io__state::di, io__state::uo) is det.

	% Given a reference to a layout structure, output the storage class
	% (e.g. static), type and name of the global variable that will
	% hold it. The bool says whether the output is part of the definition
	% of that variable (this influences e.g. whether we output "extern"
	% or not).
:- pred output_layout_name_storage_type_name(layout_name::in, bool::in,
	io__state::di, io__state::uo) is det.

	% Given a reference to a layout structure, output the name of the
	% global variable that will hold it.
:- pred output_layout_name(layout_name::in,
	io__state::di, io__state::uo) is det.

	% Given a reference to a layout structure, return a bool that is true
	% iff the layout structure contains code addresses.
:- func layout_name_would_include_code_addr(layout_name) = bool.

	% Given a label, return a string giving the name of the global variable
	% containing the label layout structure that would be associated with
	% it. Make_label_layout_name does not guarantee that the label *has*
	% an associated label layout structure.
:- func make_label_layout_name(label) = string.

	% For a given procedure label, return whether the procedure is
	% user-defined or compiler-generated.
:- func proc_label_user_or_compiler(proc_label) = proc_layout_user_or_compiler.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree__prog_data, parse_tree__prog_out, hlds__hlds_pred.
:- import_module libs__trace_params, backend_libs__c_util.
:- import_module backend_libs__rtti, ll_backend__trace, ll_backend__code_util.
:- import_module int, char, string, require, std_util, list.

output_layout_data_defn(label_layout_data(Label, ProcLayoutAddr,
		MaybePort, MaybeGoalPath, MaybeVarInfo), DeclSet0, DeclSet) -->
	output_label_layout_data_defn(Label, ProcLayoutAddr,
		MaybePort, MaybeGoalPath, MaybeVarInfo, DeclSet0, DeclSet).
output_layout_data_defn(proc_layout_data(ProcLabel, Traversal, MaybeRest),
		DeclSet0, DeclSet) -->
	output_proc_layout_data_defn(ProcLabel, Traversal, MaybeRest,
		DeclSet0, DeclSet).
output_layout_data_defn(closure_proc_id_data(CallerProcLabel, SeqNo,
		ProcLabel, ModuleName, FileName, LineNumber, GoalPath),
		DeclSet0, DeclSet) -->
	output_closure_proc_id_data_defn(CallerProcLabel, SeqNo, ProcLabel,
		ModuleName, FileName, LineNumber, GoalPath, DeclSet0, DeclSet).
output_layout_data_defn(module_layout_data(ModuleName, StringTableSize,
		StringTable, ProcLayoutNames, FileLayouts, TraceLevel),
		DeclSet0, DeclSet) -->
	output_module_layout_data_defn(ModuleName, StringTableSize,
		StringTable, ProcLayoutNames, FileLayouts, TraceLevel,
		DeclSet0, DeclSet).
output_layout_data_defn(proc_static_data(RttiProcLabel, FileName, LineNumber,
		IsInInterface, CallSites), DeclSet0, DeclSet) -->
	output_proc_static_data_defn(RttiProcLabel, FileName, LineNumber,
		IsInInterface, CallSites, DeclSet0, DeclSet).
output_layout_data_defn(table_io_decl_data(RttiProcLabel, Kind, NumPTIs,
		PTIVectorRval, TypeParamsRval), DeclSet0, DeclSet) -->
	output_table_io_decl(RttiProcLabel, Kind, NumPTIs,
		PTIVectorRval, TypeParamsRval, DeclSet0, DeclSet).

%-----------------------------------------------------------------------------%

output_layout_name_decl(LayoutName) -->
	output_layout_name_storage_type_name(LayoutName, no),
	io__write_string(";\n").

output_maybe_layout_name_decl(LayoutName, DeclSet0, DeclSet) -->
	(
		{ decl_set_is_member(data_addr(layout_addr(LayoutName)),
			DeclSet0) }
	->
		{ DeclSet = DeclSet0 }
	;
		output_layout_name_decl(LayoutName),
		{ decl_set_insert(DeclSet0, data_addr(layout_addr(LayoutName)),
			DeclSet) }
	).

output_maybe_layout_data_decl(LayoutData, DeclSet0, DeclSet) -->
	{ extract_layout_name(LayoutData, LayoutName) },
	output_maybe_layout_name_decl(LayoutName, DeclSet0, DeclSet).

:- pred extract_layout_name(layout_data::in, layout_name::out) is det.

extract_layout_name(label_layout_data(Label, _, _, _, yes(_)), LayoutName) :-
	LayoutName = label_layout(Label, label_has_var_info).
extract_layout_name(label_layout_data(Label, _, _, _, no), LayoutName) :-
	LayoutName = label_layout(Label, label_has_no_var_info).
extract_layout_name(proc_layout_data(ProcLabel, _, MaybeRest), LayoutName) :-
	Kind = maybe_proc_layout_and_exec_trace_kind(MaybeRest, ProcLabel),
	LayoutName = proc_layout(ProcLabel, Kind).
extract_layout_name(closure_proc_id_data(CallerProcLabel, SeqNo,
		ClosureProcLabel, _, _, _, _),
		closure_proc_id(CallerProcLabel, SeqNo, ClosureProcLabel)).
extract_layout_name(module_layout_data(ModuleName, _,_,_,_,_), LayoutName) :-
	LayoutName = module_layout(ModuleName).
extract_layout_name(proc_static_data(RttiProcLabel, _, _, _, _), LayoutName) :-
	LayoutName = proc_static(RttiProcLabel).
extract_layout_name(table_io_decl_data(RttiProcLabel, _, _, _, _),
		LayoutName) :-
	LayoutName = table_io_decl(RttiProcLabel).

:- pred output_layout_decls(list(layout_name)::in, decl_set::in, decl_set::out,
	io__state::di, io__state::uo) is det.

output_layout_decls([], DeclSet, DeclSet) --> [].
output_layout_decls([LayoutName | LayoutNames], DeclSet0, DeclSet) -->
	output_layout_decl(LayoutName, DeclSet0, DeclSet1),
	output_layout_decls(LayoutNames, DeclSet1, DeclSet).

:- pred output_layout_decl(layout_name::in, decl_set::in, decl_set::out,
	io__state::di, io__state::uo) is det.

output_layout_decl(LayoutName, DeclSet0, DeclSet) -->
	(
		{ decl_set_is_member(data_addr(layout_addr(LayoutName)),
			DeclSet0) }
	->
		{ DeclSet = DeclSet0 }
	;
		output_layout_name_storage_type_name(LayoutName, no),
		io__write_string(";\n"),
		{ decl_set_insert(DeclSet0, data_addr(layout_addr(LayoutName)),
			DeclSet) }
	).

	% This code should be kept in sync with output_layout_name/3 below.
make_label_layout_name(Label) = Name :-
	llds_out__get_label(Label, yes, LabelName),
	string__append_list([
		mercury_data_prefix,
		"_label_layout__",
		LabelName
	], Name).

output_layout_name(label_layout(Label, _)) -->
	% This code should be kept in sync with make_label_layout_name/1 above.
	io__write_string(mercury_data_prefix),
	io__write_string("_label_layout__"),
	{ llds_out__get_label(Label, yes, LabelName) },
	io__write_string(LabelName).
output_layout_name(proc_layout(ProcLabel, _)) -->
	io__write_string(mercury_data_prefix),
	io__write_string("_proc_layout__"),
	output_proc_label(ProcLabel).
output_layout_name(proc_layout_head_var_nums(ProcLabel)) -->
	io__write_string(mercury_data_prefix),
	io__write_string("_head_var_nums__"),
	output_proc_label(ProcLabel).
output_layout_name(proc_layout_var_names(ProcLabel)) -->
	io__write_string(mercury_data_prefix),
	io__write_string("_var_names__"),
	output_proc_label(ProcLabel).
output_layout_name(closure_proc_id(CallerProcLabel, SeqNo, _)) -->
	io__write_string(mercury_data_prefix),
	io__write_string("_closure_layout__"),
	output_proc_label(CallerProcLabel),
	io__write_string("_"),
	io__write_int(SeqNo).
output_layout_name(file_layout(ModuleName, FileNum)) -->
	io__write_string(mercury_data_prefix),
	io__write_string("_file_layout__"),
	{ llds_out__sym_name_mangle(ModuleName, ModuleNameStr) },
	io__write_string(ModuleNameStr),
	io__write_string("_"),
	io__write_int(FileNum).
output_layout_name(file_layout_line_number_vector(ModuleName, FileNum)) -->
	io__write_string(mercury_data_prefix),
	io__write_string("_file_lines__"),
	{ llds_out__sym_name_mangle(ModuleName, ModuleNameStr) },
	io__write_string(ModuleNameStr),
	io__write_string("_"),
	io__write_int(FileNum).
output_layout_name(file_layout_label_layout_vector(ModuleName, FileNum)) -->
	io__write_string(mercury_data_prefix),
	io__write_string("_file_label_layouts__"),
	{ llds_out__sym_name_mangle(ModuleName, ModuleNameStr) },
	io__write_string(ModuleNameStr),
	io__write_string("_"),
	io__write_int(FileNum).
output_layout_name(module_layout_string_table(ModuleName)) -->
	io__write_string(mercury_data_prefix),
	io__write_string("_module_strings__"),
	{ llds_out__sym_name_mangle(ModuleName, ModuleNameStr) },
	io__write_string(ModuleNameStr).
output_layout_name(module_layout_file_vector(ModuleName)) -->
	io__write_string(mercury_data_prefix),
	io__write_string("_module_files__"),
	{ llds_out__sym_name_mangle(ModuleName, ModuleNameStr) },
	io__write_string(ModuleNameStr).
output_layout_name(module_layout_proc_vector(ModuleName)) -->
	io__write_string(mercury_data_prefix),
	io__write_string("_module_procs__"),
	{ llds_out__sym_name_mangle(ModuleName, ModuleNameStr) },
	io__write_string(ModuleNameStr).
output_layout_name(module_layout(ModuleName)) -->
	io__write_string(mercury_data_prefix),
	io__write_string("_module_layout__"),
	{ llds_out__sym_name_mangle(ModuleName, ModuleNameStr) },
	io__write_string(ModuleNameStr).
output_layout_name(proc_static(RttiProcLabel)) -->
	io__write_string(mercury_data_prefix),
	io__write_string("_proc_static__"),
	{ ProcLabel = code_util__make_proc_label_from_rtti(RttiProcLabel) },
	output_proc_label(ProcLabel).
output_layout_name(proc_static_call_sites(RttiProcLabel)) -->
	io__write_string(mercury_data_prefix),
	io__write_string("_proc_static_call_sites__"),
	{ ProcLabel = code_util__make_proc_label_from_rtti(RttiProcLabel) },
	output_proc_label(ProcLabel).
output_layout_name(table_io_decl(RttiProcLabel)) -->
	io__write_string(mercury_data_prefix),
	io__write_string("_table_io_decl__"),
	{ ProcLabel = code_util__make_proc_label_from_rtti(RttiProcLabel) },
	output_proc_label(ProcLabel).

output_layout_name_storage_type_name(label_layout(Label, LabelVars),
		_BeingDefined) -->
	io__write_string("static const "),
	io__write_string(label_vars_to_type(LabelVars)),
	io__write_string(" "),
	output_layout_name(label_layout(Label, LabelVars)).
output_layout_name_storage_type_name(proc_layout(ProcLabel, Kind),
		_BeingDefined) -->
	io__write_string("static MR_STATIC_CODE_CONST "),
	io__write_string(kind_to_type(Kind)),
	io__write_string(" "),
	output_layout_name(proc_layout(ProcLabel, Kind)).
output_layout_name_storage_type_name(proc_layout_head_var_nums(ProcLabel),
		_BeingDefined) -->
	io__write_string("static const "),
	io__write_string("MR_int_least16_t "),
	output_layout_name(proc_layout_head_var_nums(ProcLabel)),
	io__write_string("[]").
output_layout_name_storage_type_name(proc_layout_var_names(ProcLabel),
		_BeingDefined) -->
	io__write_string("static const "),
	io__write_string("MR_int_least16_t "),
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
		io__write_string("MR_Compiler_Closure_Id\n")
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
		BeingDefined) -->
	(
		{ BeingDefined = no },
		io__write_string("extern ")
	;
		{ BeingDefined = yes }
	),
	(
		{ RttiProcLabel ^ is_special_pred_instance = yes },
		io__write_string("MR_Compiler_ProcStatic ")
	;
		{ RttiProcLabel ^ is_special_pred_instance = no },
		io__write_string("MR_User_ProcStatic ")
	),
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

layout_name_would_include_code_addr(label_layout(_, _)) = no.
layout_name_would_include_code_addr(proc_layout(_, _)) = yes.
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

:- func label_vars_to_type(label_vars) = string.

label_vars_to_type(label_has_var_info) =    "MR_Label_Layout".
label_vars_to_type(label_has_no_var_info) = "MR_Label_Layout_No_Var_Info".

:- func kind_to_type(proc_layout_kind) = string.

kind_to_type(proc_layout_traversal) =            "MR_Proc_Layout_Traversal".
kind_to_type(proc_layout_proc_id(user)) =        "MR_Proc_Layout_User".
kind_to_type(proc_layout_proc_id(compiler)) =    "MR_Proc_Layout_Compiler".
kind_to_type(proc_layout_exec_trace(user)) =     "MR_Proc_Layout_User_Exec".
kind_to_type(proc_layout_exec_trace(compiler)) = "MR_Proc_Layout_Compiler_Exec".

%-----------------------------------------------------------------------------%

:- pred output_label_layout_data_defn(label::in, layout_name::in,
	maybe(trace_port)::in, maybe(int)::in, maybe(label_var_info)::in,
	decl_set::in, decl_set::out, io__state::di, io__state::uo) is det.

output_label_layout_data_defn(Label, ProcLayoutAddr, MaybePort, MaybeGoalPath,
		MaybeVarInfo, DeclSet0, DeclSet) -->
	output_layout_decl(ProcLayoutAddr, DeclSet0, DeclSet1),
	(
		{ MaybeVarInfo = yes(VarInfo0) },
		{ VarInfo0 = label_var_info(_,
			LocnsTypes0, VarNums0, TypeParams0) },
		output_rval_decls(LocnsTypes0, "", "", 0, _,
			DeclSet1, DeclSet2),
		output_rval_decls(VarNums0, "", "", 0, _,
			DeclSet2, DeclSet3),
		output_rval_decls(TypeParams0, "", "", 0, _,
			DeclSet3, DeclSet4),
		{ LabelVars = label_has_var_info }
	;
		{ MaybeVarInfo = no },
		{ DeclSet4 = DeclSet0 },
		{ LabelVars = label_has_no_var_info }
	),
	io__write_string("\n"),
	{ LayoutName = label_layout(Label, LabelVars) },
	output_layout_name_storage_type_name(LayoutName, yes),
	io__write_string(" = {\n"),
	io__write_string("\t(const MR_Proc_Layout *) &"),
	output_layout_name(ProcLayoutAddr),
	io__write_string(",\n\t"),
	(
		{ MaybePort = yes(Port) },
		io__write_string(trace_port_to_string(Port))
	;
		{ MaybePort = no },
		io__write_string("MR_PORT_NONE")
	),
	io__write_string(",\n\t"),
	(
		{ MaybeGoalPath = yes(GoalPath) },
		io__write_int(GoalPath)
	;
		{ MaybeGoalPath = no },
		io__write_string("0")
	),
	io__write_string(",\n\t"),
	(
		{ MaybeVarInfo = yes(VarInfo) },
		{ VarInfo = label_var_info(EncodedVarCount,
			LocnsTypes, VarNums, TypeParams) },
		io__write_int(EncodedVarCount),
		io__write_string(",\n\t(const void *) "),
		output_rval(LocnsTypes),
		io__write_string(",\n\t(const MR_uint_least16_t *) "),
		output_rval(VarNums),
		io__write_string(",\n\t(const MR_Type_Param_Locns *) "),
		output_rval(TypeParams)
	;
		{ MaybeVarInfo = no },
		io__write_int(-1)
	),
	io__write_string("\n};\n"),
	{ decl_set_insert(DeclSet4, data_addr(layout_addr(LayoutName)),
		DeclSet) }.

:- func trace_port_to_string(trace_port) = string.

trace_port_to_string(call) =	 	    "MR_PORT_CALL".
trace_port_to_string(exit) = 		    "MR_PORT_EXIT".
trace_port_to_string(redo) = 		    "MR_PORT_REDO".
trace_port_to_string(fail) = 		    "MR_PORT_FAIL".
trace_port_to_string(exception) = 	    "MR_PORT_EXCEPTION".
trace_port_to_string(ite_cond) = 	    "MR_PORT_COND".
trace_port_to_string(ite_then) = 	    "MR_PORT_THEN".
trace_port_to_string(ite_else) = 	    "MR_PORT_ELSE".
trace_port_to_string(neg_enter) =   	    "MR_PORT_NEG_ENTER".
trace_port_to_string(neg_success) = 	    "MR_PORT_NEG_SUCCESS".
trace_port_to_string(neg_failure) = 	    "MR_PORT_NEG_FAILURE".
trace_port_to_string(disj) =   	            "MR_PORT_DISJ".
trace_port_to_string(switch) = 	            "MR_PORT_SWITCH".
trace_port_to_string(nondet_pragma_first) = "MR_PORT_PRAGMA_FIRST".
trace_port_to_string(nondet_pragma_later) = "MR_PORT_PRAGMA_LATER".

%-----------------------------------------------------------------------------%

:- pred output_proc_layout_data_defn(proc_label::in,
	proc_layout_stack_traversal::in, maybe_proc_id_and_exec_trace::in,
	decl_set::in, decl_set::out, io__state::di, io__state::uo) is det.

output_proc_layout_data_defn(ProcLabel, Traversal, MaybeRest,
		DeclSet0, DeclSet) -->
	{ Kind = maybe_proc_layout_and_exec_trace_kind(MaybeRest, ProcLabel) },
	(
		{ MaybeRest = no_proc_id },
		output_layout_traversal_decls(Traversal, DeclSet0, DeclSet4),
		output_proc_layout_data_defn_start(ProcLabel, Kind, Traversal),
		output_layout_no_proc_id_group,
		output_proc_layout_data_defn_end
	;
		{ MaybeRest = proc_id_only },
		output_layout_traversal_decls(Traversal, DeclSet0, DeclSet4),
		output_proc_layout_data_defn_start(ProcLabel, Kind, Traversal),
		output_layout_proc_id_group(ProcLabel),
		output_layout_no_exec_trace_group,
		output_proc_layout_data_defn_end
	;
		{ MaybeRest = proc_id_and_exec_trace(ExecTrace) },
		{ HeadVarNums = ExecTrace ^ head_var_nums },
		output_proc_layout_head_var_nums(ProcLabel, HeadVarNums,
			DeclSet0, DeclSet1),
		{ VarNames = ExecTrace ^ var_names },
		{ MaxVarNum = ExecTrace ^ max_var_num },
		output_proc_layout_var_names(ProcLabel, VarNames, MaxVarNum,
			DeclSet1, DeclSet2),
		output_layout_traversal_decls(Traversal, DeclSet2, DeclSet3),
		output_layout_exec_trace_decls(ProcLabel, ExecTrace,
			DeclSet3, DeclSet4),

		output_proc_layout_data_defn_start(ProcLabel, Kind, Traversal),
		output_layout_proc_id_group(ProcLabel),
		output_layout_exec_trace_group(ProcLabel, ExecTrace),
		output_proc_layout_data_defn_end
	),
	{ decl_set_insert(DeclSet4, data_addr(
		layout_addr(proc_layout(ProcLabel, Kind))), DeclSet) }.

:- func maybe_proc_layout_and_exec_trace_kind(maybe_proc_id_and_exec_trace,
	proc_label) = proc_layout_kind.

maybe_proc_layout_and_exec_trace_kind(MaybeRest, ProcLabel) = Kind :-
	(
		MaybeRest = no_proc_id,
		Kind = proc_layout_traversal
	;
		MaybeRest = proc_id_only,
		Kind = proc_layout_proc_id(
			proc_label_user_or_compiler(ProcLabel))
	;
		MaybeRest = proc_id_and_exec_trace(_),
		Kind = proc_layout_exec_trace(
			proc_label_user_or_compiler(ProcLabel))
	).

proc_label_user_or_compiler(proc(_, _, _, _, _, _)) = user.
proc_label_user_or_compiler(special_proc(_, _, _, _, _, _)) = compiler.

:- pred output_proc_layout_data_defn_start(proc_label::in,
	proc_layout_kind::in, proc_layout_stack_traversal::in,
	io__state::di, io__state::uo) is det.

output_proc_layout_data_defn_start(ProcLabel, Kind, Traversal) -->
	io__write_string("\n"),
	output_layout_name_storage_type_name(proc_layout(ProcLabel, Kind),
		yes),
	io__write_string(" = {\n"),
	output_layout_traversal_group(Traversal).

:- pred output_proc_layout_data_defn_end(io__state::di, io__state::uo) is det.

output_proc_layout_data_defn_end -->
	io__write_string("};\n").

:- pred output_layout_traversal_decls(proc_layout_stack_traversal::in,
	decl_set::in, decl_set::out, io__state::di, io__state::uo) is det.

output_layout_traversal_decls(Traversal, DeclSet0, DeclSet) -->
	{ Traversal = proc_layout_stack_traversal(MaybeEntryLabel,
		_MaybeSuccipSlot, _StackSlotCount, _Detism) },
	(
		{ MaybeEntryLabel = yes(EntryLabel) },
		output_code_addr_decls(label(EntryLabel), "", "", 0, _,
			DeclSet0, DeclSet)
	;
		{ MaybeEntryLabel = no },
		{ DeclSet = DeclSet0 }
	).

:- pred output_layout_traversal_group(proc_layout_stack_traversal::in,
	io__state::di, io__state::uo) is det.

output_layout_traversal_group(Traversal) -->
	{ Traversal = proc_layout_stack_traversal(MaybeEntryLabel,
		MaybeSuccipSlot, StackSlotCount, Detism) },
	io__write_string("\t{\n\t"),
	(
		{ MaybeEntryLabel = yes(EntryLabel) },
		output_code_addr(label(EntryLabel))
	;
		{ MaybeEntryLabel = no },
		% The actual code address will be put into the structure
		% by module initialization code.
		io__write_string("NULL")
	),
	io__write_string(",\n\t"),
	(
		{ MaybeSuccipSlot = yes(SuccipSlot) },
		io__write_int(SuccipSlot)
	;
		{ MaybeSuccipSlot = no },
		io__write_int(-1)
	),
	io__write_string(",\n\t"),
	io__write_int(StackSlotCount),
	io__write_string(",\n\t"),
	io__write_string(detism_to_c_detism(Detism)),
	io__write_string("\n\t},\n").

:- func detism_to_c_detism(determinism) = string.

detism_to_c_detism(det) =	  "MR_DETISM_DET".
detism_to_c_detism(semidet) =	  "MR_DETISM_SEMI".
detism_to_c_detism(nondet) =	  "MR_DETISM_NON".
detism_to_c_detism(multidet) =	  "MR_DETISM_MULTI".
detism_to_c_detism(erroneous) =	  "MR_DETISM_ERRONEOUS".
detism_to_c_detism(failure) =	  "MR_DETISM_FAILURE".
detism_to_c_detism(cc_nondet) =	  "MR_DETISM_CCNON".
detism_to_c_detism(cc_multidet) = "MR_DETISM_CCMULTI".

:- pred output_layout_proc_id_group(proc_label::in,
	io__state::di, io__state::uo) is det.

output_layout_proc_id_group(ProcLabel) -->
	io__write_string("\t{\n"),
	output_proc_id(ProcLabel),
	io__write_string("\t},\n").

:- pred output_layout_no_proc_id_group(io__state::di, io__state::uo) is det.

output_layout_no_proc_id_group -->
	io__write_string("\t-1\n").

:- pred output_layout_exec_trace_decls(proc_label::in,
	proc_layout_exec_trace::in, decl_set::in, decl_set::out,
	io__state::di, io__state::uo) is det.

output_layout_exec_trace_decls(ProcLabel, ExecTrace, DeclSet0, DeclSet) -->
	{ ExecTrace = proc_layout_exec_trace(CallLabelLayout, MaybeProcBody,
		MaybeTableIoDecl, _HeadVarNums, _VarNames, _MaxVarNum,
		_MaxRegNum, _MaybeFromFullSlot, _MaybeIoSeqSlot,
		_MaybeTrailSlot, _MaybeMaxfrSlot, _EvalMethod,
		_MaybeCallTableSlot) },
	{ ModuleName = get_defining_module_name(ProcLabel) },
	output_layout_decl(CallLabelLayout, DeclSet0, DeclSet1),
	output_layout_decl(module_layout(ModuleName), DeclSet1, DeclSet2),
	(
		{ MaybeProcBody = yes(ProcBody) },
		output_rval_decls(ProcBody, "", "", 0, _, DeclSet2, DeclSet3)
	;
		{ MaybeProcBody = no },
		{ DeclSet3 = DeclSet2 }
	),
	(
		{ MaybeTableIoDecl = yes(TableIoDeclName) },
		output_layout_decl(TableIoDeclName, DeclSet3, DeclSet)
	;
		{ MaybeTableIoDecl = no },
		{ DeclSet = DeclSet3 }
	).

:- pred output_layout_exec_trace_group(proc_label::in,
	proc_layout_exec_trace::in, io__state::di, io__state::uo) is det.

output_layout_exec_trace_group(ProcLabel, ExecTrace) -->
	{ ExecTrace = proc_layout_exec_trace(CallLabelLayout, MaybeProcBody,
		MaybeTableIoDecl, HeadVarNums, _VarNames, MaxVarNum,
		MaxRegNum, MaybeFromFullSlot, MaybeIoSeqSlot, MaybeTrailSlot,
		MaybeMaxfrSlot, EvalMethod, MaybeCallTableSlot) },
	io__write_string("\t{\n\t(const MR_Label_Layout *) &"),
	output_layout_name(CallLabelLayout),
	io__write_string(",\n\t(const MR_Module_Layout *) &"),
	{ ModuleName = get_defining_module_name(ProcLabel) },
	output_layout_name(module_layout(ModuleName)),
	io__write_string(",\n\t"),
	(
		{ MaybeProcBody = yes(ProcBody) },
		output_rval(ProcBody)
	;
		{ MaybeProcBody = no },
		io__write_int(0)
	),
	io__write_string(",\n\t"),
	(
		{ MaybeTableIoDecl = yes(TableIoDecl) },
		io__write_string("&"),
		output_layout_name(TableIoDecl)
	;
		{ MaybeTableIoDecl = no },
		io__write_string("NULL")
	),
	io__write_string(",\n\t"),
	output_layout_name(proc_layout_head_var_nums(ProcLabel)),
	io__write_string(",\n\t"),
	output_layout_name(proc_layout_var_names(ProcLabel)),
	io__write_string(",\n\t"),
	io__write_int(list__length(HeadVarNums)),
	io__write_string(",\n\t"),
	io__write_int(MaxVarNum),
	io__write_string(",\n\t"),
	io__write_int(MaxRegNum),
	io__write_string(",\n\t"),
	write_maybe_slot_num(MaybeFromFullSlot),
	io__write_string(",\n\t"),
	write_maybe_slot_num(MaybeIoSeqSlot),
	io__write_string(",\n\t"),
	write_maybe_slot_num(MaybeTrailSlot),
	io__write_string(",\n\t"),
	write_maybe_slot_num(MaybeMaxfrSlot),
	io__write_string(",\n\t"),
	io__write_string(eval_method_to_c_string(EvalMethod)),
	io__write_string(",\n\t"),
	write_maybe_slot_num(MaybeCallTableSlot),
	io__write_string("\n\t}\n").

:- pred write_maybe_slot_num(maybe(int)::in, io__state::di, io__state::uo)
	is det.

write_maybe_slot_num(yes(SlotNum)) -->
	io__write_int(SlotNum).
write_maybe_slot_num(no) -->
	io__write_int(-1).

:- func eval_method_to_c_string(eval_method) = string.

eval_method_to_c_string(eval_normal) =	      "MR_EVAL_METHOD_NORMAL".
eval_method_to_c_string(eval_loop_check) =    "MR_EVAL_METHOD_LOOP_CHECK".
eval_method_to_c_string(eval_memo) =          "MR_EVAL_METHOD_MEMO".
eval_method_to_c_string(eval_table_io) =      "MR_EVAL_METHOD_TABLE_IO".
eval_method_to_c_string(eval_table_io_decl) = "MR_EVAL_METHOD_TABLE_IO_DECL".
eval_method_to_c_string(eval_minimal) =	      "MR_EVAL_METHOD_MINIMAL".

:- pred output_proc_layout_head_var_nums(proc_label::in, list(int)::in,
	decl_set::in, decl_set::out, io__state::di, io__state::uo) is det.

output_proc_layout_head_var_nums(ProcLabel, HeadVarNums, DeclSet0, DeclSet) -->
	io__write_string("\n"),
	output_layout_name_storage_type_name(
		proc_layout_head_var_nums(ProcLabel), yes),
	io__write_string(" = {\n"),
	( { HeadVarNums = [] } ->
			% ANSI/ISO C doesn't allow empty arrays, so
			% place a dummy value in the array.
		io__write_string("\t0\n")
	;
		list__foldl(output_number_in_vector, HeadVarNums)
	),
	io__write_string("};\n"),
	{ decl_set_insert(DeclSet0, data_addr(
		layout_addr(proc_layout_head_var_nums(ProcLabel))), DeclSet) }.

:- pred output_proc_layout_var_names(proc_label::in, list(int)::in, int::in,
	decl_set::in, decl_set::out, io__state::di, io__state::uo) is det.

output_proc_layout_var_names(ProcLabel, VarNames, MaxVarNum,
		DeclSet0, DeclSet) -->
	{ list__length(VarNames, VarNameCount) },
	{ require(unify(VarNameCount, MaxVarNum),
		"output_proc_layout_var_names: VarNameCount != MaxVarNum") },
	io__write_string("\n"),
	output_layout_name_storage_type_name(proc_layout_var_names(ProcLabel),
		yes),
	io__write_string(" = {\n"),
	( { VarNames = [] } ->
			% ANSI/ISO C doesn't allow empty arrays, so
			% place a dummy value in the array.
		io__write_string("\t0\n")
	;
		list__foldl(output_number_in_vector, VarNames)
	),
	io__write_string("};\n"),
	{ decl_set_insert(DeclSet0, data_addr(
		layout_addr(proc_layout_var_names(ProcLabel))), DeclSet) }.

:- pred output_layout_no_exec_trace_group(io__state::di, io__state::uo) is det.

output_layout_no_exec_trace_group -->
	io__write_string("\t0\n").

%-----------------------------------------------------------------------------%

:- pred output_closure_proc_id_data_defn(proc_label::in, int::in,
	proc_label::in, module_name::in, string::in, int::in, string::in,
	decl_set::in, decl_set::out, io__state::di, io__state::uo) is det.

output_closure_proc_id_data_defn(CallerProcLabel, SeqNo, ClosureProcLabel,
		ModuleName, FileName, LineNumber, GoalPath,
		DeclSet0, DeclSet) -->
	io__write_string("\n"),
	{ LayoutName = closure_proc_id(CallerProcLabel, SeqNo,
		ClosureProcLabel) },
	output_layout_name_storage_type_name(LayoutName, yes),
	io__write_string(" = {\n\t{\n"),
	output_proc_id(ClosureProcLabel),
	io__write_string("\t},\n\t"),
	{ prog_out__sym_name_to_string(ModuleName, ModuleNameStr) },
	quote_and_write_string(ModuleNameStr),
	io__write_string(",\n\t"),
	quote_and_write_string(FileName),
	io__write_string(",\n\t"),
	io__write_int(LineNumber),
	io__write_string(",\n\t"),
	quote_and_write_string(GoalPath),
	io__write_string("\n};\n"),
	{ decl_set_insert(DeclSet0,
		data_addr(layout_addr(LayoutName)), DeclSet) }.

:- pred output_proc_id(proc_label::in, io__state::di, io__state::uo) is det.

output_proc_id(ProcLabel) -->
	(
		{ ProcLabel = proc(DefiningModule, PredOrFunc, DeclaringModule,
			Name, Arity, Mode) },
		{ prog_out__sym_name_to_string(DefiningModule,
			DefiningModuleStr) },
		{ prog_out__sym_name_to_string(DeclaringModule,
			DeclaringModuleStr) },
		{ proc_id_to_int(Mode, ModeInt) },
		(
			{ PredOrFunc = predicate },
			io__write_string("\tMR_PREDICATE,\n\t")
		;
			{ PredOrFunc = function },
			io__write_string("\tMR_FUNCTION,\n\t")
		),
		quote_and_write_string(DeclaringModuleStr),
		io__write_string(",\n\t"),
		quote_and_write_string(DefiningModuleStr),
		io__write_string(",\n\t"),
		quote_and_write_string(Name),
		io__write_string(",\n\t"),
		io__write_int(Arity),
		io__write_string(",\n\t"),
		io__write_int(ModeInt),
		io__write_string("\n")
	;
		{ ProcLabel = special_proc(DefiningModule, PredName,
			TypeModule, TypeName, TypeArity, Mode) },
		{ prog_out__sym_name_to_string(DefiningModule,
			DefiningModuleStr) },
		{ prog_out__sym_name_to_string(TypeModule, TypeModuleStr) },
		{ proc_id_to_int(Mode, ModeInt) },
		io__write_string("\t"),
		quote_and_write_string(TypeName),
		io__write_string(",\n\t"),
		quote_and_write_string(TypeModuleStr),
		io__write_string(",\n\t"),
		quote_and_write_string(DefiningModuleStr),
		io__write_string(",\n\t"),
		quote_and_write_string(PredName),
		io__write_string(",\n\t"),
		io__write_int(TypeArity),
		io__write_string(",\n\t"),
		io__write_int(ModeInt),
		io__write_string("\n")
	).

%-----------------------------------------------------------------------------%

:- pred output_module_layout_data_defn(module_name::in, int::in,
	string::in, list(layout_name)::in, list(file_layout_data)::in,
	trace_level::in, decl_set::in, decl_set::out,
	io__state::di, io__state::uo) is det.

output_module_layout_data_defn(ModuleName, StringTableSize, StringTable,
		ProcLayoutNames, FileLayouts, TraceLevel, DeclSet0, DeclSet)
		-->
	output_module_string_table(ModuleName, StringTableSize, StringTable,
		DeclSet0, DeclSet1),
	output_module_layout_proc_vector_defn(ModuleName, ProcLayoutNames,
		ProcVectorName, DeclSet1, DeclSet2),
	output_file_layout_data_defns(ModuleName, 0, FileLayouts,
		FileLayoutNames, DeclSet2, DeclSet3),
	output_file_layout_vector_data_defn(ModuleName, FileLayoutNames,
		FileVectorName, DeclSet3, DeclSet4),

	{ ModuleLayoutName = module_layout(ModuleName) },
	io__write_string("\n"),
	output_layout_name_storage_type_name(ModuleLayoutName, yes),
	io__write_string(" = {\n\t"),
	{ prog_out__sym_name_to_string(ModuleName, ModuleNameStr) },
	quote_and_write_string(ModuleNameStr),
	io__write_string(",\n\t"),
	io__write_int(StringTableSize),
	io__write_string(",\n\t"),
	{ ModuleStringTableName = module_layout_string_table(ModuleName) },
	output_layout_name(ModuleStringTableName),
	io__write_string(",\n\t"),
	{ list__length(ProcLayoutNames, ProcLayoutVectorLength) },
	io__write_int(ProcLayoutVectorLength),
	io__write_string(",\n\t"),
	output_layout_name(ProcVectorName),
	io__write_string(",\n\t"),
	{ list__length(FileLayouts, FileLayoutVectorLength) },
	io__write_int(FileLayoutVectorLength),
	io__write_string(",\n\t"),
	output_layout_name(FileVectorName),
	io__write_string(",\n\t"),
	io__write_string(trace_level_rep(TraceLevel)),
	io__write_string("\n};\n"),
	{ decl_set_insert(DeclSet4, data_addr(layout_addr(ModuleLayoutName)),
		DeclSet) }.

:- pred output_module_layout_proc_vector_defn(module_name::in,
	list(layout_name)::in, layout_name::out, decl_set::in, decl_set::out,
	io__state::di, io__state::uo) is det.

output_module_layout_proc_vector_defn(ModuleName, ProcLayoutNames,
		VectorName, DeclSet0, DeclSet) -->
	output_layout_decls(ProcLayoutNames, DeclSet0, DeclSet1),
	{ VectorName = module_layout_proc_vector(ModuleName) },
	io__write_string("\n"),
	output_layout_name_storage_type_name(VectorName, yes),
	io__write_string(" = {\n"),
	( { ProcLayoutNames = [] } ->
			% ANSI/ISO C doesn't allow empty arrays, so
			% place a dummy value in the array.
		io__write_string("\tNULL\n")
	;
		list__foldl(output_layout_name_in_vector(
					"(const MR_Proc_Layout *)\n\t&"),
				ProcLayoutNames)
	),
	io__write_string("};\n"),
	{ decl_set_insert(DeclSet1, data_addr(layout_addr(VectorName)),
		DeclSet) }.

%-----------------------------------------------------------------------------%

	% The string table cannot be zero size; it must contain at least an
	% empty string.

:- pred output_module_string_table(module_name::in,
	int::in, string::in, decl_set::in, decl_set::out,
	io__state::di, io__state::uo) is det.

output_module_string_table(ModuleName, StringTableSize, StringTable,
		DeclSet0, DeclSet) -->
	{ TableName = module_layout_string_table(ModuleName) },
	io__write_string("\n"),
	output_layout_name_storage_type_name(TableName, yes),
	io__write_string(" = {"),
	output_module_string_table_chars(0, StringTableSize - 1, StringTable),
	io__write_string("};\n"),
	{ decl_set_insert(DeclSet0, data_addr(layout_addr(TableName)),
		DeclSet) }.

:- pred output_module_string_table_chars(int::in, int::in, string::in,
	io__state::di, io__state::uo) is det.

output_module_string_table_chars(CurIndex, MaxIndex, String) -->
	( { CurIndex mod 16 = 0 } ->
		io__write_string("\n\t")
	;
		[]
	),
	{ string__unsafe_index(String, CurIndex, Char) },
	io__write_char(''''),
	( { char__to_int(Char, 0) } ->
		io__write_string("\\0")
	; { c_util__quote_char(Char, QuoteChar) } ->
		io__write_char('\\'),
		io__write_char(QuoteChar)
	;
		io__write_char(Char)
	),
	io__write_char(''''),
	( { CurIndex < MaxIndex } ->
		io__write_string(", "),
		output_module_string_table_chars(CurIndex + 1, MaxIndex,
			String)
	;
		[]
	).

%-----------------------------------------------------------------------------%

:- pred output_file_layout_vector_data_defn(module_name::in,
	list(layout_name)::in, layout_name::out, decl_set::in, decl_set::out,
	io__state::di, io__state::uo) is det.

output_file_layout_vector_data_defn(ModuleName, FileLayoutNames, VectorName,
		DeclSet0, DeclSet) -->
	output_layout_decls(FileLayoutNames, DeclSet0, DeclSet1),
	{ VectorName = module_layout_file_vector(ModuleName) },
	io__write_string("\n"),
	output_layout_name_storage_type_name(VectorName, yes),
	io__write_string(" = {\n"),
	( { FileLayoutNames = [] } ->
			% ANSI/ISO C doesn't allow empty arrays, so
			% place a dummy value in the array.
		io__write_string("\tNULL\n")
	;
		list__foldl(output_layout_name_in_vector("&"), FileLayoutNames)
	),
	io__write_string("};\n"),
	{ decl_set_insert(DeclSet1, data_addr(layout_addr(VectorName)),
		DeclSet) }.

:- pred output_file_layout_data_defns(module_name::in, int::in,
	list(file_layout_data)::in, list(layout_name)::out,
	decl_set::in, decl_set::out, io__state::di, io__state::uo) is det.

output_file_layout_data_defns(_, _, [], [], DeclSet, DeclSet) --> [].
output_file_layout_data_defns(ModuleName, FileNum, [FileLayout | FileLayouts],
		[FileLayoutName | FileLayoutNames], DeclSet0, DeclSet) -->
	output_file_layout_data_defn(ModuleName, FileNum, FileLayout,
		FileLayoutName, DeclSet0, DeclSet1),
	output_file_layout_data_defns(ModuleName, FileNum + 1, FileLayouts,
		FileLayoutNames, DeclSet1, DeclSet).

:- pred output_file_layout_data_defn(module_name::in, int::in,
	file_layout_data::in, layout_name::out,
	decl_set::in, decl_set::out, io__state::di, io__state::uo) is det.

output_file_layout_data_defn(ModuleName, FileNum, FileLayout, FileLayoutName,
		DeclSet0, DeclSet) -->
	{ FileLayout = file_layout_data(FileName, LineNoLabelList) },
	{ list__map2(line_no_label_to_label_layout_addr, LineNoLabelList,
		LineNos, LabelLayoutAddrs) },
	output_data_addrs_decls(LabelLayoutAddrs, "", "", 0, _,
		DeclSet0, DeclSet1),

	{ list__length(LineNoLabelList, VectorLengths) },
	output_file_layout_line_number_vector_defn(ModuleName, FileNum,
		LineNos, LineNumberVectorName, DeclSet1, DeclSet2),
	output_file_layout_label_layout_vector_defn(ModuleName, FileNum,
		LabelLayoutAddrs, LabelVectorName, DeclSet2, DeclSet3),

	{ FileLayoutName = file_layout(ModuleName, FileNum) },
	io__write_string("\n"),
	output_layout_name_storage_type_name(FileLayoutName, yes),
	io__write_string(" = {\n\t"),
	quote_and_write_string(FileName),
	io__write_string(",\n\t"),
	io__write_int(VectorLengths),
	io__write_string(",\n\t"),
	output_layout_name(LineNumberVectorName),
	io__write_string(",\n\t"),
	output_layout_name(LabelVectorName),
	io__write_string("\n};\n"),
	{ decl_set_insert(DeclSet3, data_addr(layout_addr(FileLayoutName)),
		DeclSet) }.

:- pred output_file_layout_line_number_vector_defn(module_name::in, int::in,
	list(int)::in, layout_name::out, decl_set::in, decl_set::out,
	io__state::di, io__state::uo) is det.

output_file_layout_line_number_vector_defn(ModuleName, FileNum, LineNumbers,
		LayoutName, DeclSet0, DeclSet) -->
	{ LayoutName = file_layout_line_number_vector(ModuleName, FileNum) },
	io__write_string("\n"),
	output_layout_name_storage_type_name(LayoutName, yes),
	io__write_string(" = {\n"),
	( { LineNumbers = [] } ->
			% ANSI/ISO C doesn't allow empty arrays, so
			% place a dummy value in the array.
		io__write_string("\t0\n")
	;
		list__foldl(output_number_in_vector, LineNumbers)
	),
	io__write_string("};\n"),
	{ decl_set_insert(DeclSet0, data_addr(layout_addr(LayoutName)),
		DeclSet) }.

:- pred output_file_layout_label_layout_vector_defn(module_name::in, int::in,
	list(data_addr)::in, layout_name::out, decl_set::in, decl_set::out,
	io__state::di, io__state::uo) is det.

output_file_layout_label_layout_vector_defn(ModuleName, FileNum, LabelAddrs,
		LayoutName, DeclSet0, DeclSet) -->
	{ LayoutName = file_layout_label_layout_vector(ModuleName, FileNum) },
	io__write_string("\n"),
	output_layout_name_storage_type_name(LayoutName, yes),
	io__write_string(" = {\n"),
	( { LabelAddrs = [] } ->
			% ANSI/ISO C doesn't allow empty arrays, so
			% place a dummy value in the array.
		io__write_string("\tNULL\n")
	;
		list__foldl(output_data_addr_in_vector(
				"(const MR_Label_Layout *)\n\t&"),
			LabelAddrs)
	),
	io__write_string("};\n"),
	{ decl_set_insert(DeclSet0, data_addr(layout_addr(LayoutName)),
		DeclSet) }.

%-----------------------------------------------------------------------------%

:- pred line_no_label_to_label_layout_addr(pair(int, layout_name)::in,
	int::out, data_addr::out) is det.

line_no_label_to_label_layout_addr(LineNo - LabelLayout, LineNo, DataAddr) :-
	DataAddr = layout_addr(LabelLayout).

:- pred quote_and_write_string(string::in, io__state::di, io__state::uo)
	is det.

quote_and_write_string(String) -->
	io__write_string(""""),
	c_util__output_quoted_string(String),
	io__write_string("""").

:- pred output_number_in_vector(int::in, io__state::di, io__state::uo) is det.

output_number_in_vector(Num) -->
	io__write_string("\t"),
	io__write_int(Num),
	io__write_string(",\n").

:- pred output_layout_name_in_vector(string::in, layout_name::in,
	io__state::di, io__state::uo) is det.

output_layout_name_in_vector(Prefix, Name) -->
	io__write_string("\t"),
	io__write_string(Prefix),
	output_layout_name(Name),
	io__write_string(",\n").

:- pred output_data_addr_in_vector(string::in, data_addr::in,
	io__state::di, io__state::uo) is det.

output_data_addr_in_vector(Prefix, DataAddr) -->
	io__write_string("\t"),
	io__write_string(Prefix),
	output_data_addr(DataAddr),
	io__write_string(",\n").

%-----------------------------------------------------------------------------%

:- pred output_proc_static_data_defn(rtti_proc_label::in, string::in,
	int::in, bool::in, list(call_site_static_data)::in,
	decl_set::in, decl_set::out, io__state::di, io__state::uo) is det.

output_proc_static_data_defn(RttiProcLabel, FileName, LineNumber,
		IsInInterface, CallSites, DeclSet0, DeclSet) -->
	list__foldl2(output_call_site_static_decl, CallSites,
		DeclSet0, DeclSet1),
	output_call_site_static_array(RttiProcLabel, CallSites,
		DeclSet1, DeclSet2),
	{ LayoutName = proc_static(RttiProcLabel) },
	io__write_string("\n"),
	output_layout_name_storage_type_name(LayoutName, yes),
	io__write_string(" = {\n"),
	{ ProcLabel = code_util__make_proc_label_from_rtti(RttiProcLabel) },
	output_layout_proc_id_group(ProcLabel),
	io__write_string("\t"),
	quote_and_write_string(FileName),
	io__write_string(",\n\t"),
	io__write_int(LineNumber),
	io__write_string(",\n\t"),
	(
		{ IsInInterface = yes },
		io__write_string("MR_TRUE")
	;
		{ IsInInterface = no },
		io__write_string("MR_FALSE")
	),
	io__write_string(",\n\t"),
	io__write_int(list__length(CallSites)),
	io__write_string(",\n\t"),
	{ CallSitesLayoutName = proc_static_call_sites(RttiProcLabel) },
	output_layout_name(CallSitesLayoutName),
	io__write_string(",\n#ifdef MR_USE_ACTIVATION_COUNTS\n"),
	io__write_string("\t0,\n"),
	io__write_string("#endif\n"),
	io__write_string("\tNULL\n};\n"),
	{ decl_set_insert(DeclSet2, data_addr(layout_addr(LayoutName)),
		DeclSet) }.

:- pred output_call_site_static_array(rtti_proc_label::in,
	list(call_site_static_data)::in, decl_set::in, decl_set::out,
	io__state::di, io__state::uo) is det.

output_call_site_static_array(RttiProcLabel, CallSites, DeclSet0, DeclSet) -->
	{ LayoutName = proc_static_call_sites(RttiProcLabel) },
	io__write_string("\n"),
	output_layout_name_storage_type_name(LayoutName, yes),
	io__write_string(" = {\n"),
	list__foldl(output_call_site_static, CallSites),
	io__write_string("};\n"),
	{ decl_set_insert(DeclSet0, data_addr(layout_addr(LayoutName)),
		DeclSet) }.

:- pred output_call_site_static(call_site_static_data::in,
	io__state::di, io__state::uo) is det.

output_call_site_static(CallSiteStatic) -->
	io__write_string("\t{ "),
	(
		{ CallSiteStatic = normal_call(Callee, TypeSubst,
			FileName, LineNumber, GoalPath) },
		io__write_string("MR_normal_call, (MR_ProcStatic *)\n\t  &"),
		output_layout_name(proc_static(Callee)),
		( { TypeSubst = "" } ->
			io__write_string(",\n\t  NULL, ")
		;
			io__write_string(",\n\t  """),
			io__write_string(TypeSubst),
			io__write_string(""", ")
		)
	;
		{ CallSiteStatic = special_call(FileName, LineNumber,
			GoalPath) },
		io__write_string("MR_special_call, NULL,\n\t  NULL, ")
	;
		{ CallSiteStatic = higher_order_call(FileName, LineNumber,
			GoalPath) },
		io__write_string("MR_higher_order_call, NULL,\n\t  NULL, ")
	;
		{ CallSiteStatic = method_call(FileName, LineNumber,
			GoalPath) },
		io__write_string("MR_method_call, NULL,\n\t  NULL, ")
	;
		{ CallSiteStatic = callback(FileName, LineNumber, GoalPath) },
		io__write_string("MR_callback, NULL,\n\t  NULL, ")
	),
	io__write_string(""""),
	io__write_string(FileName),
	io__write_string(""", "),
	io__write_int(LineNumber),
	io__write_string(", """),
	{ trace__path_to_string(GoalPath, GoalPathStr) },
	io__write_string(GoalPathStr),
	io__write_string(""" },\n").

:- pred output_call_site_static_decl(call_site_static_data::in,
	decl_set::in, decl_set::out, io__state::di, io__state::uo) is det.

output_call_site_static_decl(CallSiteStatic, DeclSet0, DeclSet) -->
	(
		{ CallSiteStatic = normal_call(Callee, _, _, _, _) },
		output_maybe_layout_name_decl(proc_static(Callee),
			DeclSet0, DeclSet)
	;
		{ CallSiteStatic = special_call(_, _, _) },
		{ DeclSet = DeclSet0 }
	;
		{ CallSiteStatic = higher_order_call(_, _, _) },
		{ DeclSet = DeclSet0 }
	;
		{ CallSiteStatic = method_call(_, _, _) },
		{ DeclSet = DeclSet0 }
	;
		{ CallSiteStatic = callback(_, _, _) },
		{ DeclSet = DeclSet0 }
	).

%-----------------------------------------------------------------------------%

:- pred output_table_io_decl(rtti_proc_label::in, proc_layout_kind::in,
	int::in, rval::in, rval::in, decl_set::in, decl_set::out,
	io__state::di, io__state::uo) is det.

output_table_io_decl(RttiProcLabel, ProcLayoutKind, NumPTIs,
		PTIVectorRval, TypeParamRval, DeclSet0, DeclSet) -->
	output_rval_decls(PTIVectorRval, "", "", 0, _, DeclSet0, DeclSet1),
	{ LayoutName = table_io_decl(RttiProcLabel) },
	{ ProcLabel = code_util__make_proc_label_from_rtti(RttiProcLabel) },
	{ ProcLayoutName = proc_layout(ProcLabel, ProcLayoutKind) },
	output_layout_decl(ProcLayoutName, DeclSet1, DeclSet2),

	io__write_string("\n"),
	output_layout_name_storage_type_name(LayoutName, yes),
	io__write_string(" = {\n\t(const MR_Proc_Layout *) &"),
	output_layout_name(ProcLayoutName),
	io__write_string(",\n\t"),
	io__write_int(NumPTIs),
	io__write_string(",\n\t(const MR_PseudoTypeInfo *) "),
	output_rval(PTIVectorRval),
	io__write_string(",\n\t(const MR_Type_Param_Locns *) "),
	output_rval(TypeParamRval),
	io__write_string("\n};\n"),
	{ decl_set_insert(DeclSet2, data_addr(layout_addr(LayoutName)),
		DeclSet) }.

%-----------------------------------------------------------------------------%
