%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% Debugging support for LLDS to LLDS peephole optimization.

% Main author: zs.

%-----------------------------------------------------------------------------%

:- module opt_debug.

:- interface.

:- import_module llds, livemap.
:- import_module code_model, rtti, layout, builtin_ops.

:- import_module io, bool, list, assoc_list, std_util.

:- pred opt_debug__msg(bool, int, string, io__state, io__state).
:- mode opt_debug__msg(in, in, in, di, uo) is det.

:- pred opt_debug__dump_instrs(bool, list(instruction), io__state, io__state).
:- mode opt_debug__dump_instrs(in, in, di, uo) is det.

:- pred opt_debug__dump_intlist(list(int), string).
:- mode opt_debug__dump_intlist(in, out) is det.

:- pred opt_debug__dump_livemap(livemap, string).
:- mode opt_debug__dump_livemap(in, out) is det.

:- pred opt_debug__dump_livemaplist(assoc_list(label, lvalset), string).
:- mode opt_debug__dump_livemaplist(in, out) is det.

:- pred opt_debug__dump_livevals(lvalset, string).
:- mode opt_debug__dump_livevals(in, out) is det.

:- pred opt_debug__dump_livelist(list(lval), string).
:- mode opt_debug__dump_livelist(in, out) is det.

:- pred opt_debug__dump_reg(reg_type, int, string).
:- mode opt_debug__dump_reg(in, in, out) is det.

:- pred opt_debug__dump_lval(lval, string).
:- mode opt_debug__dump_lval(in, out) is det.

:- pred opt_debug__dump_rval(rval, string).
:- mode opt_debug__dump_rval(in, out) is det.

:- pred opt_debug__dump_rvals(list(rval), string).
:- mode opt_debug__dump_rvals(in, out) is det.

:- pred opt_debug__dump_mem_ref(mem_ref, string).
:- mode opt_debug__dump_mem_ref(in, out) is det.

:- pred opt_debug__dump_const(rval_const, string).
:- mode opt_debug__dump_const(in, out) is det.

:- pred opt_debug__dump_data_addr(data_addr, string).
:- mode opt_debug__dump_data_addr(in, out) is det.

:- pred opt_debug__dump_data_name(data_name, string).
:- mode opt_debug__dump_data_name(in, out) is det.

:- pred opt_debug__dump_rtti_type_id(rtti_type_id, string).
:- mode opt_debug__dump_rtti_type_id(in, out) is det.

:- pred opt_debug__dump_rtti_name(rtti_name, string).
:- mode opt_debug__dump_rtti_name(in, out) is det.

:- pred opt_debug__dump_layout_name(layout_name, string).
:- mode opt_debug__dump_layout_name(in, out) is det.

:- pred opt_debug__dump_unop(unary_op, string).
:- mode opt_debug__dump_unop(in, out) is det.

:- pred opt_debug__dump_binop(binary_op, string).
:- mode opt_debug__dump_binop(in, out) is det.

:- pred opt_debug__dump_label(label, string).
:- mode opt_debug__dump_label(in, out) is det.

:- pred opt_debug__dump_labels(list(label), string).
:- mode opt_debug__dump_labels(in, out) is det.

:- pred opt_debug__dump_label_pairs(list(pair(label)), string).
:- mode opt_debug__dump_label_pairs(in, out) is det.

:- pred opt_debug__dump_proclabel(proc_label, string).
:- mode opt_debug__dump_proclabel(in, out) is det.

:- pred opt_debug__dump_maybe_rvals(list(maybe(rval)), int, string).
:- mode opt_debug__dump_maybe_rvals(in, in, out) is det.

:- pred opt_debug__dump_code_addr(code_addr, string).
:- mode opt_debug__dump_code_addr(in, out) is det.

:- pred opt_debug__dump_code_addrs(list(code_addr), string).
:- mode opt_debug__dump_code_addrs(in, out) is det.

:- pred opt_debug__dump_bool(bool, string).
:- mode opt_debug__dump_bool(in, out) is det.

:- pred opt_debug__dump_instr(instr, string).
:- mode opt_debug__dump_instr(in, out) is det.

:- pred opt_debug__dump_fullinstr(instruction, string).
:- mode opt_debug__dump_fullinstr(in, out) is det.

:- pred opt_debug__dump_fullinstrs(list(instruction), string).
:- mode opt_debug__dump_fullinstrs(in, out) is det.

:- pred opt_debug__dump_code_model(code_model, string).
:- mode opt_debug__dump_code_model(in, out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module prog_out.
:- import_module hlds_pred.
:- import_module llds_out, code_util, opt_util.
:- import_module globals, options.

:- import_module int, set, map, string.

opt_debug__msg(OptDebug, LabelNo, Msg) -->
	(
		{ OptDebug = yes },
		io__write_string("\n"),
		io__write_string(Msg),
		( { LabelNo >= 0 } ->
			io__write_string(", next label no: "),
			io__write_int(LabelNo)
		;
			[]
		),
		io__write_string("\n")
	;
		{ OptDebug = no }
	).

opt_debug__dump_instrs(OptDebug, Instrs) -->
	(
		{ OptDebug = yes },
		globals__io_lookup_bool_option(auto_comments, PrintComments),
		opt_debug__dump_instrs_2(Instrs, PrintComments)
	;
		{ OptDebug = no }
	).

:- pred opt_debug__dump_instrs_2(list(instruction), bool, io__state, io__state).
:- mode opt_debug__dump_instrs_2(in, in, di, uo) is det.

opt_debug__dump_instrs_2([], _PrintComments) --> [].
opt_debug__dump_instrs_2([Uinstr - Comment | Instrs], PrintComments) -->
	output_instruction_and_comment(Uinstr, Comment, PrintComments),
	opt_debug__dump_instrs_2(Instrs, PrintComments).

opt_debug__dump_intlist([], "").
opt_debug__dump_intlist([H | T], Str) :-
	string__int_to_string(H, H_str),
	opt_debug__dump_intlist(T, T_str),
	string__append_list([" ", H_str, T_str], Str).

opt_debug__dump_livemap(Livemap, Str) :-
	map__to_assoc_list(Livemap, Livemaplist),
	opt_debug__dump_livemaplist(Livemaplist, Str).

opt_debug__dump_livemaplist([], "").
opt_debug__dump_livemaplist([Label - Lvalset | Livemaplist], Str) :-
	opt_debug__dump_label(Label, L_str),
	opt_debug__dump_livevals(Lvalset, S_str),
	opt_debug__dump_livemaplist(Livemaplist, Str2),
	string__append_list([L_str, " ->", S_str, "\n", Str2], Str).

opt_debug__dump_livevals(Lvalset, Str) :-
	set__to_sorted_list(Lvalset, Lvallist),
	opt_debug__dump_livelist(Lvallist, Str).

opt_debug__dump_livelist([], "").
opt_debug__dump_livelist([Lval | Lvallist], Str) :-
	opt_debug__dump_lval(Lval, L_str),
	opt_debug__dump_livelist(Lvallist, L2_str),
	string__append_list([" ", L_str, L2_str], Str).

opt_debug__dump_reg(r, N, Str) :-
	string__int_to_string(N, N_str),
	string__append_list(["r(", N_str, ")"], Str).
opt_debug__dump_reg(f, N, Str) :-
	string__int_to_string(N, N_str),
	string__append_list(["f(", N_str, ")"], Str).

opt_debug__dump_lval(reg(Type, Num), Str) :-
	opt_debug__dump_reg(Type, Num, R_str),
	string__append_list(["reg(", R_str, ")"], Str).
opt_debug__dump_lval(stackvar(N), Str) :-
	string__int_to_string(N, N_str),
	string__append_list(["stackvar(", N_str, ")"], Str).
opt_debug__dump_lval(framevar(N), Str) :-
	string__int_to_string(N, N_str),
	string__append_list(["framevar(", N_str, ")"], Str).
opt_debug__dump_lval(succip, Str) :-
	string__append_list(["succip"], Str).
opt_debug__dump_lval(maxfr, Str) :-
	string__append_list(["maxfr"], Str).
opt_debug__dump_lval(curfr, Str) :-
	string__append_list(["curfr"], Str).
opt_debug__dump_lval(succfr(R), Str) :-
	opt_debug__dump_rval(R, R_str),
	string__append_list(["succfr(", R_str, ")"], Str).
opt_debug__dump_lval(prevfr(R), Str) :-
	opt_debug__dump_rval(R, R_str),
	string__append_list(["prevfr(", R_str, ")"], Str).
opt_debug__dump_lval(redofr(R), Str) :-
	opt_debug__dump_rval(R, R_str),
	string__append_list(["redofr(", R_str, ")"], Str).
opt_debug__dump_lval(redoip(R), Str) :-
	opt_debug__dump_rval(R, R_str),
	string__append_list(["redoip(", R_str, ")"], Str).
opt_debug__dump_lval(succip(R), Str) :-
	opt_debug__dump_rval(R, R_str),
	string__append_list(["succip(", R_str, ")"], Str).
opt_debug__dump_lval(hp, Str) :-
	string__append_list(["hp"], Str).
opt_debug__dump_lval(sp, Str) :-
	string__append_list(["sp"], Str).
opt_debug__dump_lval(field(MT, N, F), Str) :-
	( MT = yes(T) ->
		string__int_to_string(T, T_str)
	;
		T_str = "no"
	),
	opt_debug__dump_rval(N, N_str),
	opt_debug__dump_rval(F, F_str),
	string__append_list(["field(", T_str, ", ", N_str, ", ",
		F_str, ")"], Str).
opt_debug__dump_lval(lvar(_), Str) :-
	string__append_list(["lvar(_)"], Str).
opt_debug__dump_lval(temp(Type, Num), Str) :-
	opt_debug__dump_reg(Type, Num, R_str),
	string__append_list(["temp(", R_str, ")"], Str).
opt_debug__dump_lval(mem_ref(R), Str) :-
	opt_debug__dump_rval(R, R_str),
	string__append_list(["mem_ref(", R_str, ")"], Str).

opt_debug__dump_rval(lval(Lval), Str) :-
	opt_debug__dump_lval(Lval, Lval_str),
	string__append_list(["lval(", Lval_str, ")"], Str).
opt_debug__dump_rval(var(_), Str) :-
	string__append_list(["var(_)"], Str).
opt_debug__dump_rval(mkword(T, N), Str) :-
	string__int_to_string(T, T_str),
	opt_debug__dump_rval(N, N_str),
	string__append_list(["mkword(", T_str, ", ", N_str, ")"], Str).
opt_debug__dump_rval(const(C), Str) :-
	opt_debug__dump_const(C, C_str),
	string__append_list(["const(", C_str, ")"], Str).
opt_debug__dump_rval(create(T, MA, _, U, L, _, _), Str) :-
	string__int_to_string(T, T_str),
	opt_debug__dump_maybe_rvals(MA, 3, MA_str),
	(
		U = must_be_static,
		U_str = "static"
	;
		U = can_be_either,
		U_str = "either"
	;
		U = must_be_dynamic,
		U_str = "dynamic"
	),
	string__int_to_string(L, L_str),
	string__append_list(["create(", T_str, ", ", MA_str, ", ",
		U_str, ", ", L_str, ")"], Str).
opt_debug__dump_rval(unop(O, N), Str) :-
	opt_debug__dump_unop(O, O_str),
	opt_debug__dump_rval(N, N_str),
	string__append_list(["unop(", O_str, ", ", N_str, ")"], Str).
opt_debug__dump_rval(binop(O, N1, N2), Str) :-
	opt_debug__dump_binop(O, O_str),
	opt_debug__dump_rval(N1, N1_str),
	opt_debug__dump_rval(N2, N2_str),
	string__append_list(["binop(", O_str, ", ", N1_str, ", ",
		N2_str, ")"], Str).
opt_debug__dump_rval(mem_addr(M), Str) :-
	opt_debug__dump_mem_ref(M, M_str),
	string__append_list(["mem_addr(", M_str, ")"], Str).

opt_debug__dump_rvals([], "").
opt_debug__dump_rvals([Rval | Rvals], Str) :-
	opt_debug__dump_rval(Rval, R_str),
	opt_debug__dump_rvals(Rvals, S_str),
	string__append_list([R_str, ", ", S_str], Str).

opt_debug__dump_mem_ref(stackvar_ref(N), Str) :-
	string__int_to_string(N, N_str),
	string__append_list(["stackvar_ref(", N_str, ")"], Str).
opt_debug__dump_mem_ref(framevar_ref(N), Str) :-
	string__int_to_string(N, N_str),
	string__append_list(["framevar_ref(", N_str, ")"], Str).
opt_debug__dump_mem_ref(heap_ref(R, T, N), Str) :-
	opt_debug__dump_rval(R, R_str),
	string__int_to_string(T, T_str),
	string__int_to_string(N, N_str),
	string__append_list(["heap_ref(", R_str, ", ", T_str, ", ",
		N_str, ")"], Str).

opt_debug__dump_const(true, "true").
opt_debug__dump_const(false, "false").
opt_debug__dump_const(int_const(I), Str) :-
	string__int_to_string(I, Str).
opt_debug__dump_const(float_const(F), Str) :-
	string__float_to_string(F, Str).
opt_debug__dump_const(string_const(S), Str) :-
	string__append_list(["""", S, """"], Str).
opt_debug__dump_const(multi_string_const(L, _S), Str) :-
	string__int_to_string(L, L_str),
	string__append_list(["multi_string(", L_str, ")"], Str).
opt_debug__dump_const(code_addr_const(CodeAddr), Str) :-
	opt_debug__dump_code_addr(CodeAddr, C_str),
	string__append_list(["code_addr_const(", C_str, ")"], Str).
opt_debug__dump_const(data_addr_const(DataAddr), Str) :-
	opt_debug__dump_data_addr(DataAddr, DataAddr_str),
	string__append_list(
		["data_addr_const(", DataAddr_str, ")"], Str).
opt_debug__dump_const(label_entry(Label), Str) :-
	opt_debug__dump_label(Label, LabelStr),
	string__append_list(["label_entry(", LabelStr, ")"], Str).

opt_debug__dump_data_addr(data_addr(ModuleName, DataName), Str) :-
	prog_out__sym_name_to_string(ModuleName, ModuleName_str),
	opt_debug__dump_data_name(DataName, DataName_str),
	string__append_list(
		["data_addr(", ModuleName_str, ", ", DataName_str, ")"], Str).
opt_debug__dump_data_addr(rtti_addr(RttiTypeId, DataName), Str) :-
	opt_debug__dump_rtti_type_id(RttiTypeId, RttiTypeId_str),
	opt_debug__dump_rtti_name(DataName, DataName_str),
	string__append_list(
		["rtti_addr(", RttiTypeId_str, ", ", DataName_str, ")"], Str).
opt_debug__dump_data_addr(layout_addr(LayoutName), Str) :-
	opt_debug__dump_layout_name(LayoutName, LayoutName_str),
	string__append_list(["layout_addr(", LayoutName_str, ")"], Str).

opt_debug__dump_data_name(common(N), Str) :-
	string__int_to_string(N, N_str),
	string__append("common", N_str, Str).
opt_debug__dump_data_name(base_typeclass_info(ClassId, InstanceNum), Str) :-
	llds_out__make_base_typeclass_info_name(ClassId, InstanceNum, Str).
opt_debug__dump_data_name(tabling_pointer(ProcLabel), Str) :-
	opt_debug__dump_proclabel(ProcLabel, ProcLabelStr),
	string__append_list(["tabling_pointer(", ProcLabelStr, ")"], Str).
opt_debug__dump_data_name(deep_profiling_procedure_data(ProcLabel), Str) :-
	opt_debug__dump_proclabel(ProcLabel, ProcLabelStr),
	string__append_list(["deep_profiling_procedure_data(",
				ProcLabelStr, ")"], Str).

opt_debug__dump_rtti_type_id(rtti_type_id(ModuleName, TypeName, Arity), Str) :-
	llds_out__sym_name_mangle(ModuleName, ModuleName_str),
	llds_out__name_mangle(TypeName, TypeName_str),
	string__int_to_string(Arity, Arity_str),
	string__append_list(["rtti_type_id(", ModuleName_str, ", ",
		TypeName_str, Arity_str, ")"], Str).

opt_debug__dump_rtti_name(exist_locns(Ordinal), Str) :-
	string__int_to_string(Ordinal, Ordinal_str),
	string__append("exist_locns_", Ordinal_str, Str).
opt_debug__dump_rtti_name(exist_info(Ordinal), Str) :-
	string__int_to_string(Ordinal, Ordinal_str),
	string__append("exist_info_", Ordinal_str, Str).
opt_debug__dump_rtti_name(field_names(Ordinal), Str) :-
	string__int_to_string(Ordinal, Ordinal_str),
	string__append("field_names_", Ordinal_str, Str).
opt_debug__dump_rtti_name(field_types(Ordinal), Str) :-
	string__int_to_string(Ordinal, Ordinal_str),
	string__append("field_types_", Ordinal_str, Str).
opt_debug__dump_rtti_name(reserved_addrs, Str) :-
	Str = "reserved_addrs".
opt_debug__dump_rtti_name(reserved_addr_functors, Str) :-
	Str = "reserved_addr_functors".
opt_debug__dump_rtti_name(enum_functor_desc(Ordinal), Str) :-
	string__int_to_string(Ordinal, Ordinal_str),
	string__append("enum_functor_desc_", Ordinal_str, Str).
opt_debug__dump_rtti_name(notag_functor_desc, Str) :-
	Str = "notag_functor_desc_".
opt_debug__dump_rtti_name(du_functor_desc(Ordinal), Str) :-
	string__int_to_string(Ordinal, Ordinal_str),
	string__append("du_functor_desc_", Ordinal_str, Str).
opt_debug__dump_rtti_name(reserved_addr_functor_desc(Ordinal), Str) :-
	string__int_to_string(Ordinal, Ordinal_str),
	string__append("reserved_addr_functor_desc_", Ordinal_str, Str).
opt_debug__dump_rtti_name(enum_name_ordered_table, Str) :-
	Str = "enum_name_ordered_table".
opt_debug__dump_rtti_name(enum_value_ordered_table, Str) :-
	Str = "enum_value_ordered_table".
opt_debug__dump_rtti_name(du_name_ordered_table, Str) :-
	Str = "du_name_ordered_table".
opt_debug__dump_rtti_name(du_stag_ordered_table(Ptag), Str) :-
	string__int_to_string(Ptag, Ptag_str),
	string__append("du_stag_ordered_table_", Ptag_str, Str).
opt_debug__dump_rtti_name(du_ptag_ordered_table, Str) :-
	Str = "du_ptag_ordered_table".
opt_debug__dump_rtti_name(reserved_addr_table, Str) :-
	Str = "reserved_addr_table".
opt_debug__dump_rtti_name(type_ctor_info, Str) :-
	Str = "type_ctor_info".
opt_debug__dump_rtti_name(base_typeclass_info(_ModuleName, ClassId,
		InstanceStr), Str) :-
	llds_out__make_base_typeclass_info_name(ClassId, InstanceStr, Str).
opt_debug__dump_rtti_name(pseudo_type_info(_Pseudo), Str) :-
	% XXX should give more info than this
	Str = "pseudo_type_info".
opt_debug__dump_rtti_name(type_hashcons_pointer, Str) :-
	Str = "type_hashcons_pointer".

opt_debug__dump_layout_name(label_layout(Label, LabelVars), Str) :-
	opt_debug__dump_label(Label, LabelStr),
	(
		LabelVars = label_has_var_info,
		LabelVarsStr = "label_has_var_info"
	;
		LabelVars = label_has_no_var_info,
		LabelVarsStr = "label_has_no_var_info"
	),
	string__append_list(["label_layout(", LabelStr, ", ",
		LabelVarsStr, ")"], Str).
opt_debug__dump_layout_name(proc_layout(ProcLabel, _), Str) :-
	opt_debug__dump_proclabel(ProcLabel, ProcLabelStr),
	string__append_list(["proc_layout(", ProcLabelStr, ")"], Str).
opt_debug__dump_layout_name(proc_layout_var_names(ProcLabel), Str) :-
	opt_debug__dump_proclabel(ProcLabel, ProcLabelStr),
	string__append_list(["proc_layout_var_names(", ProcLabelStr, ")"],
		Str).
opt_debug__dump_layout_name(closure_proc_id(ProcLabel, SeqNo, _), Str) :-
	opt_debug__dump_proclabel(ProcLabel, ProcLabelStr),
	string__int_to_string(SeqNo, SeqNoStr),
	string__append_list(["closure_proc_id(", ProcLabelStr, ", ",
		SeqNoStr, ")"], Str).
opt_debug__dump_layout_name(file_layout(ModuleName, FileNum), Str) :-
	llds_out__sym_name_mangle(ModuleName, ModuleNameStr),
	string__int_to_string(FileNum, FileNumStr),
	string__append_list(["file_layout(", ModuleNameStr, ", ",
		FileNumStr, ")"], Str).
opt_debug__dump_layout_name(file_layout_line_number_vector(ModuleName,
		FileNum), Str) :-
	llds_out__sym_name_mangle(ModuleName, ModuleNameStr),
	string__int_to_string(FileNum, FileNumStr),
	string__append_list(["file_layout_line_number_vector(", ModuleNameStr,
		", ", FileNumStr, ")"], Str).
opt_debug__dump_layout_name(file_layout_label_layout_vector(ModuleName,
		FileNum), Str) :-
	llds_out__sym_name_mangle(ModuleName, ModuleNameStr),
	string__int_to_string(FileNum, FileNumStr),
	string__append_list(["file_layout_label_layout_vector(", ModuleNameStr,
		", ", FileNumStr, ")"], Str).
opt_debug__dump_layout_name(module_layout_string_table(ModuleName), Str) :-
	llds_out__sym_name_mangle(ModuleName, ModuleNameStr),
	string__append_list(["module_layout_string_table(", ModuleNameStr,
		")"], Str).
opt_debug__dump_layout_name(module_layout_file_vector(ModuleName), Str) :-
	llds_out__sym_name_mangle(ModuleName, ModuleNameStr),
	string__append_list(["module_layout_file_vector(", ModuleNameStr, ")"],
		Str).
opt_debug__dump_layout_name(module_layout_proc_vector(ModuleName), Str) :-
	llds_out__sym_name_mangle(ModuleName, ModuleNameStr),
	string__append_list(["module_layout_proc_vector(", ModuleNameStr, ")"],
		Str).
opt_debug__dump_layout_name(module_layout(ModuleName), Str) :-
	llds_out__sym_name_mangle(ModuleName, ModuleNameStr),
	string__append_list(["module_layout(", ModuleNameStr, ")"], Str).
opt_debug__dump_layout_name(proc_static(RttiProcLabel), Str) :-
	ProcLabel = code_util__make_proc_label_from_rtti(RttiProcLabel),
	opt_debug__dump_proclabel(ProcLabel, ProcLabelStr),
	string__append_list(["proc_static(", ProcLabelStr, ")"], Str).
opt_debug__dump_layout_name(proc_static_call_sites(RttiProcLabel), Str) :-
	ProcLabel = code_util__make_proc_label_from_rtti(RttiProcLabel),
	opt_debug__dump_proclabel(ProcLabel, ProcLabelStr),
	string__append_list(["proc_static_call_sites(", ProcLabelStr, ")"],
		Str).
opt_debug__dump_layout_name(table_io_decl(RttiProcLabel), Str) :-
	ProcLabel = code_util__make_proc_label_from_rtti(RttiProcLabel),
	opt_debug__dump_proclabel(ProcLabel, ProcLabelStr),
	string__append_list(["table_io_decl(", ProcLabelStr, ")"], Str).

opt_debug__dump_unop(mktag, "mktag").
opt_debug__dump_unop(tag, "tag").
opt_debug__dump_unop(unmktag, "unmktag").
opt_debug__dump_unop(strip_tag, "strip_tag").
opt_debug__dump_unop(mkbody, "mkbody").
opt_debug__dump_unop(unmkbody, "unmkbody").
opt_debug__dump_unop(not, "not").
opt_debug__dump_unop(hash_string, "hash_string").
opt_debug__dump_unop(bitwise_complement, "bitwise_complement").

opt_debug__dump_binop(Op, String) :-
	llds_out__binary_op_to_string(Op, String).

opt_debug__dump_maybe_rvals([], _, "").
opt_debug__dump_maybe_rvals([MR | MRs], N, Str) :-
	( N > 0 ->
		( MR = yes(R) ->
			opt_debug__dump_rval(R, MR_str)
		;
			MR_str = "no"
		),
		N1 is N - 1,
		opt_debug__dump_maybe_rvals(MRs, N1, MRs_str),
		string__append_list([MR_str, ", ", MRs_str], Str)
	;
		Str = "truncated"
	).

opt_debug__dump_code_addr(label(Label), Str) :-
	opt_debug__dump_label(Label, Str).
opt_debug__dump_code_addr(imported(ProcLabel), Str) :-
	opt_debug__dump_proclabel(ProcLabel, Str).
opt_debug__dump_code_addr(succip, "succip").
opt_debug__dump_code_addr(do_succeed(Last), Str) :-
	(
		Last = no,
		Str = "do_succeed"
	;
		Last = yes,
		Str = "do_last_succeed"
	).
opt_debug__dump_code_addr(do_redo, "do_redo").
opt_debug__dump_code_addr(do_fail, "do_fail").
opt_debug__dump_code_addr(do_trace_redo_fail_shallow,
	"do_trace_redo_fail_shallow").
opt_debug__dump_code_addr(do_trace_redo_fail_deep, "do_trace_redo_fail_deep").
opt_debug__dump_code_addr(do_call_closure, "do_nondet_closure").
opt_debug__dump_code_addr(do_call_class_method, "do_nondet_class_method").
opt_debug__dump_code_addr(do_det_aditi_call, "do_det_aditi_call").
opt_debug__dump_code_addr(do_semidet_aditi_call, "do_semidet_aditi_call").
opt_debug__dump_code_addr(do_nondet_aditi_call, "do_nondet_aditi_call").
opt_debug__dump_code_addr(do_aditi_insert, "do_aditi_insert").
opt_debug__dump_code_addr(do_aditi_delete, "do_aditi_delete").
opt_debug__dump_code_addr(do_aditi_bulk_insert, "do_aditi_bulk_insert").
opt_debug__dump_code_addr(do_aditi_bulk_delete, "do_aditi_bulk_delete").
opt_debug__dump_code_addr(do_aditi_bulk_modify, "do_aditi_bulk_modify").
opt_debug__dump_code_addr(do_not_reached, "do_not_reached").

opt_debug__dump_code_addrs([], "").
opt_debug__dump_code_addrs([Addr | Addrs], Str) :-
	opt_debug__dump_code_addr(Addr, A_str),
	opt_debug__dump_code_addrs(Addrs, A2_str),
	string__append_list([" ", A_str, A2_str], Str).

opt_debug__dump_label(local(N, ProcLabel), Str) :-
	opt_debug__dump_proclabel(ProcLabel, P_str),
	string__int_to_string(N, N_str),
	string__append_list([P_str, "_", N_str], Str).
opt_debug__dump_label(c_local(ProcLabel), Str) :-
	opt_debug__dump_proclabel(ProcLabel, Str).
opt_debug__dump_label(local(ProcLabel), Str) :-
	opt_debug__dump_proclabel(ProcLabel, Str).
opt_debug__dump_label(exported(ProcLabel), Str) :-
	opt_debug__dump_proclabel(ProcLabel, Str).

opt_debug__dump_labels([], "").
opt_debug__dump_labels([Label | Labels], Str) :-
	opt_debug__dump_label(Label, L_str),
	opt_debug__dump_labels(Labels, L2_str),
	string__append_list([" ", L_str, L2_str], Str).

opt_debug__dump_label_pairs([], "").
opt_debug__dump_label_pairs([L1 - L2 | Labels], Str) :-
	opt_debug__dump_label(L1, L1_str),
	opt_debug__dump_label(L2, L2_str),
	opt_debug__dump_label_pairs(Labels, L_str),
	string__append_list([" ", L1_str, "-", L2_str, L_str], Str).

opt_debug__dump_proclabel(proc(Module, _PredOrFunc, PredModule,
		PredName, Arity, ProcId), Str) :-
	( Module = PredModule ->
		ExtraModule = ""
	;
		llds_out__sym_name_mangle(PredModule, PredModuleName),
		string__append(PredModuleName, "_", ExtraModule)
	),
	llds_out__sym_name_mangle(Module, ModuleName),
	string__int_to_string(Arity, A_str),
	proc_id_to_int(ProcId, Mode),
	string__int_to_string(Mode, M_str),
	string__append_list([ExtraModule, ModuleName, "_", PredName,
		"_", A_str, "_", M_str], Str).
opt_debug__dump_proclabel(special_proc(Module, Pred, TypeModule,
		Type, Arity, ProcId), Str) :-
	llds_out__sym_name_mangle(Module, ModuleName),
	llds_out__sym_name_mangle(TypeModule, TypeModuleName),
	llds_out__qualify_name(TypeModuleName, Type, TypeName),
	string__int_to_string(Arity, A_str),
	proc_id_to_int(ProcId, Mode),
	string__int_to_string(Mode, M_str),
	string__append_list([ModuleName, "_", Pred, "_",
		TypeName, "_", A_str, "_", M_str], Str).

opt_debug__dump_bool(yes, "yes").
opt_debug__dump_bool(no, "no").

opt_debug__dump_code_model(model_det, "model_det").
opt_debug__dump_code_model(model_semi, "model_semi").
opt_debug__dump_code_model(model_non, "model_non").

opt_debug__dump_instr(comment(Comment), Str) :-
	string__append_list(["comment(", Comment, ")"], Str).
opt_debug__dump_instr(livevals(Livevals), Str) :-
	opt_debug__dump_livevals(Livevals, L_str),
	string__append_list(["livevals(", L_str, ")"], Str).
opt_debug__dump_instr(block(RTemps, FTemps, _), Str) :-
	string__int_to_string(RTemps, R_str),
	string__int_to_string(FTemps, F_str),
	string__append_list(["block(", R_str, ", ", F_str, ", ...)"], Str).
opt_debug__dump_instr(assign(Lval, Rval), Str) :-
	opt_debug__dump_lval(Lval, L_str),
	opt_debug__dump_rval(Rval, R_str),
	string__append_list(["assign(", L_str, ", ", R_str, ")"], Str).
opt_debug__dump_instr(call(Proc, Ret, _, _, _, _), Str) :-
	opt_debug__dump_code_addr(Proc, P_str),
	opt_debug__dump_code_addr(Ret, R_str),
	string__append_list(["call(", P_str, ", ", R_str, ", ...)"], Str).
opt_debug__dump_instr(mkframe(FrameInfo, Redoip), Str) :-
	opt_debug__dump_code_addr(Redoip, R_str),
	(
		FrameInfo = ordinary_frame(Name, Size, MaybePragma),
		string__int_to_string(Size, S_str),
		( MaybePragma = yes(pragma_c_struct(StructName, Fields, _)) ->
			string__append_list(["yes(", StructName, ", ",
				Fields, ")"], P_str)
		;
			P_str = "no"
		),
		string__append_list(["mkframe(", Name, ", ", S_str, ", ",
			P_str, ", ", R_str, ")"], Str)
	;
		FrameInfo = temp_frame(Kind),
		(
			Kind = nondet_stack_proc,
			string__append_list(["mktempframe(", R_str, ")"], Str)
		;
			Kind = det_stack_proc,
			string__append_list(["mkdettempframe(", R_str, ")"],
				Str)
		)
	).
opt_debug__dump_instr(label(Label), Str) :-
	opt_debug__dump_label(Label, L_str),
	string__append_list(["label(", L_str, ")"], Str).
opt_debug__dump_instr(goto(CodeAddr), Str) :-
	opt_debug__dump_code_addr(CodeAddr, C_str),
	string__append_list(["goto(", C_str, ")"], Str).
opt_debug__dump_instr(computed_goto(Rval, Labels), Str) :-
	opt_debug__dump_rval(Rval, R_str),
	opt_debug__dump_labels(Labels, L_str),
	string__append_list(["computed_goto(", R_str, ", ", L_str, ")"], Str).
opt_debug__dump_instr(c_code(Code, _), Str) :-
	string__append_list(["c_code(", Code, ")"], Str).
opt_debug__dump_instr(if_val(Rval, CodeAddr), Str) :-
	opt_debug__dump_rval(Rval, R_str),
	opt_debug__dump_code_addr(CodeAddr, C_str),
	string__append_list(["if_val(", R_str, ", ", C_str, ")"], Str).
opt_debug__dump_instr(incr_hp(Lval, MaybeTag, Size, _), Str) :-
	opt_debug__dump_lval(Lval, L_str),
	(
		MaybeTag = no,
		T_str = "no"
	;
		MaybeTag = yes(Tag),
		string__int_to_string(Tag, T_str)
	),
	opt_debug__dump_rval(Size, S_str),
	string__append_list(["incr_hp(", L_str, ", ", T_str, ", ", S_str, ")"],
		Str).
opt_debug__dump_instr(mark_hp(Lval), Str) :-
	opt_debug__dump_lval(Lval, L_str),
	string__append_list(["mark_hp(", L_str, ")"], Str).
opt_debug__dump_instr(restore_hp(Rval), Str) :-
	opt_debug__dump_rval(Rval, R_str),
	string__append_list(["restore_hp(", R_str, ")"], Str).
opt_debug__dump_instr(free_heap(Rval), Str) :-
	opt_debug__dump_rval(Rval, R_str),
	string__append_list(["free_heap(", R_str, ")"], Str).
opt_debug__dump_instr(store_ticket(Lval), Str) :-
	opt_debug__dump_lval(Lval, L_str),
	string__append_list(["store_ticket(", L_str, ")"], Str).
opt_debug__dump_instr(reset_ticket(Rval, _Reason), Str) :-
	opt_debug__dump_rval(Rval, R_str),
	string__append_list(["reset_ticket(", R_str, ", _)"], Str).
opt_debug__dump_instr(discard_ticket, "discard_ticket").
opt_debug__dump_instr(prune_ticket, "prune_ticket").
opt_debug__dump_instr(mark_ticket_stack(Lval), Str) :-
	opt_debug__dump_lval(Lval, L_str),
	string__append_list(["mark_ticket_stack(", L_str, ")"], Str).
opt_debug__dump_instr(prune_tickets_to(Rval), Str) :-
	opt_debug__dump_rval(Rval, R_str),
	string__append_list(["prune_tickets_to(", R_str, ")"], Str).
opt_debug__dump_instr(incr_sp(Size, _), Str) :-
	string__int_to_string(Size, S_str),
	string__append_list(["incr_sp(", S_str, ")"], Str).
opt_debug__dump_instr(decr_sp(Size), Str) :-
	string__int_to_string(Size, S_str),
	string__append_list(["decr_sp(", S_str, ")"], Str).
opt_debug__dump_instr(init_sync_term(Lval, N), Str) :-
	opt_debug__dump_lval(Lval, L_str),
	string__int_to_string(N, N_str),
	string__append_list(["init_sync_term(", L_str, ", ", N_str, ")"], Str).
opt_debug__dump_instr(fork(Child, Parent, Lval), Str) :-
	opt_debug__dump_label(Child, ChildStr),
	opt_debug__dump_label(Parent, ParentStr),
	string__int_to_string(Lval, LvalStr),
	string__append_list(["fork(", ChildStr, ", ", ParentStr, ", ",
		LvalStr, ")"], Str).
opt_debug__dump_instr(join_and_terminate(Lval), Str) :-
	opt_debug__dump_lval(Lval, LvalStr),
	string__append_list(["join_and_terminate(", LvalStr, ")"], Str).
opt_debug__dump_instr(join_and_continue(Lval, Label), Str) :-
	opt_debug__dump_lval(Lval, LvalStr),
	opt_debug__dump_label(Label, LabelStr),
	string__append_list(["join(", LvalStr, ", ", LabelStr, ")"], Str).
% XXX  should probably give more info than this
opt_debug__dump_instr(pragma_c(_, Comps, _, _, _, _, _, _), Str) :-
	opt_debug__dump_components(Comps, C_str),
	string__append_list(["pragma_c(", C_str, ")"], Str).

:- pred opt_debug__dump_components(list(pragma_c_component), string).
:- mode opt_debug__dump_components(in, out) is det.

opt_debug__dump_components([], "").
opt_debug__dump_components([Comp | Comps], Str) :-
	opt_debug__dump_component(Comp, Str1),
	opt_debug__dump_components(Comps, Str2),
	string__append(Str1, Str2, Str).

:- pred opt_debug__dump_component(pragma_c_component, string).
:- mode opt_debug__dump_component(in, out) is det.

opt_debug__dump_component(pragma_c_inputs(_), "").
opt_debug__dump_component(pragma_c_outputs(_), "").
opt_debug__dump_component(pragma_c_user_code(_, Code), Code).
opt_debug__dump_component(pragma_c_raw_code(Code, _), Code).
opt_debug__dump_component(pragma_c_fail_to(Label), Code) :-
	opt_debug__dump_label(Label, LabelStr),
	string__append_list(["fail to ", LabelStr], Code).
opt_debug__dump_component(pragma_c_noop, "").

opt_debug__dump_fullinstr(Uinstr - Comment, Str) :-
	opt_debug__dump_instr(Uinstr, U_str),
	string__append_list([U_str, " - ", Comment, "\n"], Str).

opt_debug__dump_fullinstrs([], "").
opt_debug__dump_fullinstrs([Instr | Instrs], Str) :-
	opt_debug__dump_fullinstr(Instr, S1_str),
	opt_debug__dump_fullinstrs(Instrs, S2_str),
	string__append_list([S1_str, S2_str], Str).
