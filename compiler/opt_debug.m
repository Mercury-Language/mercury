%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% Debugging support for LLDS to LLDS peephole optimization.

% Main author: zs.

%-----------------------------------------------------------------------------%

:- module ll_backend__opt_debug.

:- interface.

:- import_module backend_libs__builtin_ops.
:- import_module backend_libs__rtti.
:- import_module hlds__code_model.
:- import_module ll_backend__layout.
:- import_module ll_backend__livemap.
:- import_module ll_backend__llds.
:- import_module mdbcomp__prim_data.

:- import_module assoc_list.
:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module std_util.

:- pred msg(bool::in, int::in, string::in, io::di, io::uo) is det.

:- pred dump_instrs(bool::in, list(instruction)::in, io::di, io::uo) is det.

:- pred dump_intlist(list(int)::in, string::out) is det.

:- pred dump_livemap(livemap::in, string::out) is det.

:- pred dump_livemaplist(assoc_list(label, lvalset)::in, string::out) is det.

:- pred dump_livevals(lvalset::in, string::out) is det.

:- pred dump_livelist(list(lval)::in, string::out) is det.

:- pred dump_reg(reg_type::in, int::in, string::out) is det.

:- pred dump_lval(lval::in, string::out) is det.

:- pred dump_rval(rval::in, string::out) is det.

:- pred dump_rvals(list(rval)::in, string::out) is det.

:- pred dump_mem_ref(mem_ref::in, string::out) is det.

:- pred dump_const(rval_const::in, string::out) is det.

:- pred dump_data_addr(data_addr::in, string::out) is det.

:- pred dump_data_name(data_name::in, string::out) is det.

:- pred dump_rtti_type_ctor(rtti_type_ctor::in, string::out) is det.

:- pred dump_rtti_type_class_name(tc_name::in, string::out) is det.

:- pred dump_rtti_type_class_instance_types(list(tc_type)::in,
	string::out) is det.

:- pred dump_rtti_name(ctor_rtti_name::in, string::out) is det.

:- pred dump_tc_rtti_name(tc_rtti_name::in, string::out) is det.

:- pred dump_layout_name(layout_name::in, string::out) is det.

:- pred dump_unop(unary_op::in, string::out) is det.

:- pred dump_binop(binary_op::in, string::out) is det.

:- pred dump_label(label::in, string::out) is det.

:- pred dump_labels(list(label)::in, string::out) is det.

:- pred dump_label_pairs(list(pair(label))::in, string::out) is det.

:- pred dump_proclabel(proc_label::in, string::out) is det.

:- pred dump_maybe_rvals(list(maybe(rval))::in, int::in, string::out) is det.

:- pred dump_code_addr(code_addr::in, string::out) is det.

:- pred dump_code_addrs(list(code_addr)::in, string::out) is det.

:- pred dump_bool(bool::in, string::out) is det.

:- pred dump_instr(instr::in, string::out) is det.

:- pred dump_fullinstr(instruction::in, string::out) is det.

:- pred dump_fullinstrs(list(instruction)::in, string::out) is det.

:- pred dump_code_model(code_model::in, string::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs__name_mangle.
:- import_module backend_libs__proc_label.
:- import_module hlds__hlds_pred.
:- import_module hlds__special_pred.
:- import_module libs__globals.
:- import_module libs__options.
:- import_module ll_backend__code_util.
:- import_module ll_backend__llds_out.
:- import_module ll_backend__opt_util.
:- import_module parse_tree__prog_data.
:- import_module parse_tree__prog_foreign.
:- import_module parse_tree__prog_out.

:- import_module int.
:- import_module map.
:- import_module set.
:- import_module string.

msg(OptDebug, LabelNo, Msg, !IO) :-
	(
		OptDebug = yes,
		io__write_string("\n", !IO),
		io__write_string(Msg, !IO),
		( LabelNo >= 0 ->
			io__write_string(", next label no: ", !IO),
			io__write_int(LabelNo, !IO)
		;
			true
		),
		io__write_string("\n", !IO)
	;
		OptDebug = no
	).

dump_instrs(OptDebug, Instrs, !IO) :-
	(
		OptDebug = yes,
		globals__io_lookup_bool_option(auto_comments, PrintComments,
			!IO),
		dump_instrs_2(Instrs, PrintComments, !IO)
	;
		OptDebug = no
	).

:- pred dump_instrs_2(list(instruction)::in, bool::in,
	io::di, io::uo) is det.

dump_instrs_2([], _PrintComments, !IO).
dump_instrs_2([Uinstr - Comment | Instrs], PrintComments, !IO) :-
	output_instruction_and_comment(Uinstr, Comment, PrintComments, !IO),
	dump_instrs_2(Instrs, PrintComments, !IO).

dump_intlist([], "").
dump_intlist([H | T], Str) :-
	string__int_to_string(H, H_str),
	dump_intlist(T, T_str),
	string__append_list([" ", H_str, T_str], Str).

dump_livemap(Livemap, Str) :-
	map__to_assoc_list(Livemap, Livemaplist),
	dump_livemaplist(Livemaplist, Str).

dump_livemaplist([], "").
dump_livemaplist([Label - Lvalset | Livemaplist], Str) :-
	dump_label(Label, L_str),
	dump_livevals(Lvalset, S_str),
	dump_livemaplist(Livemaplist, Str2),
	string__append_list([L_str, " ->", S_str, "\n", Str2], Str).

dump_livevals(Lvalset, Str) :-
	set__to_sorted_list(Lvalset, Lvallist),
	dump_livelist(Lvallist, Str).

dump_livelist([], "").
dump_livelist([Lval | Lvallist], Str) :-
	dump_lval(Lval, L_str),
	dump_livelist(Lvallist, L2_str),
	string__append_list([" ", L_str, L2_str], Str).

dump_reg(r, N, Str) :-
	string__int_to_string(N, N_str),
	string__append_list(["r(", N_str, ")"], Str).
dump_reg(f, N, Str) :-
	string__int_to_string(N, N_str),
	string__append_list(["f(", N_str, ")"], Str).

dump_lval(reg(Type, Num), Str) :-
	dump_reg(Type, Num, R_str),
	string__append_list(["reg(", R_str, ")"], Str).
dump_lval(stackvar(N), Str) :-
	string__int_to_string(N, N_str),
	string__append_list(["stackvar(", N_str, ")"], Str).
dump_lval(framevar(N), Str) :-
	string__int_to_string(N, N_str),
	string__append_list(["framevar(", N_str, ")"], Str).
dump_lval(succip, Str) :-
	string__append_list(["succip"], Str).
dump_lval(maxfr, Str) :-
	string__append_list(["maxfr"], Str).
dump_lval(curfr, Str) :-
	string__append_list(["curfr"], Str).
dump_lval(succfr(R), Str) :-
	dump_rval(R, R_str),
	string__append_list(["succfr(", R_str, ")"], Str).
dump_lval(prevfr(R), Str) :-
	dump_rval(R, R_str),
	string__append_list(["prevfr(", R_str, ")"], Str).
dump_lval(redofr(R), Str) :-
	dump_rval(R, R_str),
	string__append_list(["redofr(", R_str, ")"], Str).
dump_lval(redoip(R), Str) :-
	dump_rval(R, R_str),
	string__append_list(["redoip(", R_str, ")"], Str).
dump_lval(succip(R), Str) :-
	dump_rval(R, R_str),
	string__append_list(["succip(", R_str, ")"], Str).
dump_lval(hp, Str) :-
	string__append_list(["hp"], Str).
dump_lval(sp, Str) :-
	string__append_list(["sp"], Str).
dump_lval(field(MT, N, F), Str) :-
	( MT = yes(T) ->
		string__int_to_string(T, T_str)
	;
		T_str = "no"
	),
	dump_rval(N, N_str),
	dump_rval(F, F_str),
	string__append_list(["field(", T_str, ", ", N_str, ", ",
		F_str, ")"], Str).
dump_lval(lvar(_), Str) :-
	string__append_list(["lvar(_)"], Str).
dump_lval(temp(Type, Num), Str) :-
	dump_reg(Type, Num, R_str),
	string__append_list(["temp(", R_str, ")"], Str).
dump_lval(mem_ref(R), Str) :-
	dump_rval(R, R_str),
	string__append_list(["mem_ref(", R_str, ")"], Str).

dump_rval(lval(Lval), Str) :-
	dump_lval(Lval, Lval_str),
	string__append_list(["lval(", Lval_str, ")"], Str).
dump_rval(var(_), Str) :-
	string__append_list(["var(_)"], Str).
dump_rval(mkword(T, N), Str) :-
	string__int_to_string(T, T_str),
	dump_rval(N, N_str),
	string__append_list(["mkword(", T_str, ", ", N_str, ")"], Str).
dump_rval(const(C), Str) :-
	dump_const(C, C_str),
	string__append_list(["const(", C_str, ")"], Str).
dump_rval(unop(O, N), Str) :-
	dump_unop(O, O_str),
	dump_rval(N, N_str),
	string__append_list(["unop(", O_str, ", ", N_str, ")"], Str).
dump_rval(binop(O, N1, N2), Str) :-
	dump_binop(O, O_str),
	dump_rval(N1, N1_str),
	dump_rval(N2, N2_str),
	string__append_list(["binop(", O_str, ", ", N1_str, ", ",
		N2_str, ")"], Str).
dump_rval(mem_addr(M), Str) :-
	dump_mem_ref(M, M_str),
	string__append_list(["mem_addr(", M_str, ")"], Str).

dump_rvals([], "").
dump_rvals([Rval | Rvals], Str) :-
	dump_rval(Rval, R_str),
	dump_rvals(Rvals, S_str),
	string__append_list([R_str, ", ", S_str], Str).

dump_mem_ref(stackvar_ref(N), Str) :-
	string__int_to_string(N, N_str),
	string__append_list(["stackvar_ref(", N_str, ")"], Str).
dump_mem_ref(framevar_ref(N), Str) :-
	string__int_to_string(N, N_str),
	string__append_list(["framevar_ref(", N_str, ")"], Str).
dump_mem_ref(heap_ref(R, T, N), Str) :-
	dump_rval(R, R_str),
	string__int_to_string(T, T_str),
	string__int_to_string(N, N_str),
	string__append_list(["heap_ref(", R_str, ", ", T_str, ", ",
		N_str, ")"], Str).

dump_const(true, "true").
dump_const(false, "false").
dump_const(int_const(I), Str) :-
	string__int_to_string(I, Str).
dump_const(float_const(F), Str) :-
	string__float_to_string(F, Str).
dump_const(string_const(S), Str) :-
	string__append_list(["""", S, """"], Str).
dump_const(multi_string_const(L, _S), Str) :-
	string__int_to_string(L, L_str),
	string__append_list(["multi_string(", L_str, ")"], Str).
dump_const(code_addr_const(CodeAddr), Str) :-
	dump_code_addr(CodeAddr, C_str),
	string__append_list(["code_addr_const(", C_str, ")"], Str).
dump_const(data_addr_const(DataAddr, MaybeOffset), Str) :-
	dump_data_addr(DataAddr, DataAddr_str),
	(
		MaybeOffset = no,
		string__append_list(
			["data_addr_const(", DataAddr_str, ")"], Str)
	;
		MaybeOffset = yes(Offset),
		string__int_to_string(Offset, Offset_str),
		string__append_list(
			["data_addr_const(", DataAddr_str, ", ",
			Offset_str, ")"], Str)
	).
dump_const(label_entry(Label), Str) :-
	dump_label(Label, LabelStr),
	string__append_list(["label_entry(", LabelStr, ")"], Str).

dump_data_addr(data_addr(ModuleName, DataName), Str) :-
	mdbcomp__prim_data__sym_name_to_string(ModuleName, ModuleName_str),
	dump_data_name(DataName, DataName_str),
	string__append_list(
		["data_addr(", ModuleName_str, ", ", DataName_str, ")"], Str).
dump_data_addr(rtti_addr(ctor_rtti_id(RttiTypeCtor, DataName)),
		Str) :-
	dump_rtti_type_ctor(RttiTypeCtor, RttiTypeCtor_str),
	dump_rtti_name(DataName, DataName_str),
	string__append_list(
		["rtti_addr(", RttiTypeCtor_str, ", ", DataName_str, ")"],
		Str).
dump_data_addr(rtti_addr(tc_rtti_id(TCName, TCDataName)), Str) :-
	dump_rtti_type_class_name(TCName, TCNameStr),
	dump_tc_rtti_name(TCDataName, TCDataName_str),
	string__append_list(
		["tc_rtti_addr(", TCNameStr, ", ", TCDataName_str, ")"],
		Str).
dump_data_addr(rtti_addr(aditi_rtti_id(ProcLabel)), Str) :-
	string__append_list(
		["aditi_rtti_addr(",
			sym_name_to_string(qualified(ProcLabel ^ proc_module,
				ProcLabel ^ proc_name)),
			")"],
		Str).
dump_data_addr(layout_addr(LayoutName), Str) :-
	dump_layout_name(LayoutName, LayoutName_str),
	string__append_list(["layout_addr(", LayoutName_str, ")"], Str).

dump_data_name(common(CellNum, TypeNum), Str) :-
	string__int_to_string(CellNum, C_str),
	string__int_to_string(TypeNum, T_str),
	string__append_list(["common(", C_str, ", ", T_str, ")"], Str).
dump_data_name(tabling_pointer(ProcLabel), Str) :-
	dump_proclabel(ProcLabel, ProcLabelStr),
	string__append_list(["tabling_pointer(", ProcLabelStr, ")"], Str).

dump_rtti_type_ctor(rtti_type_ctor(ModuleName, TypeName, Arity),
		Str) :-
	ModuleName_str = sym_name_mangle(ModuleName),
	TypeName_str = name_mangle(TypeName),
	string__int_to_string(Arity, Arity_str),
	string__append_list(["rtti_type_ctor(", ModuleName_str, ", ",
		TypeName_str, Arity_str, ")"], Str).

dump_rtti_name(exist_locns(Ordinal), Str) :-
	string__int_to_string(Ordinal, Ordinal_str),
	string__append("exist_locns_", Ordinal_str, Str).
dump_rtti_name(exist_locn, Str) :-
	Str = "exist_loc".
dump_rtti_name(exist_tc_constr(Ordinal, TCCNum, Arity), Str) :-
	string__int_to_string(Ordinal, Ordinal_str),
	string__int_to_string(TCCNum, TCCNum_str),
	string__int_to_string(Arity, Arity_str),
	string__append_list(["exist_tc_constr_", Ordinal_str, "_", TCCNum_str,
		"_", Arity_str], Str).
dump_rtti_name(exist_tc_constrs(Ordinal), Str) :-
	string__int_to_string(Ordinal, Ordinal_str),
	string__append("exist_tc_constrs_", Ordinal_str, Str).
dump_rtti_name(exist_info(Ordinal), Str) :-
	string__int_to_string(Ordinal, Ordinal_str),
	string__append("exist_info_", Ordinal_str, Str).
dump_rtti_name(field_names(Ordinal), Str) :-
	string__int_to_string(Ordinal, Ordinal_str),
	string__append("field_names_", Ordinal_str, Str).
dump_rtti_name(field_types(Ordinal), Str) :-
	string__int_to_string(Ordinal, Ordinal_str),
	string__append("field_types_", Ordinal_str, Str).
dump_rtti_name(res_addrs, Str) :-
	Str = "res_addrs".
dump_rtti_name(res_addr_functors, Str) :-
	Str = "res_addr_functors".
dump_rtti_name(enum_functor_desc(Ordinal), Str) :-
	string__int_to_string(Ordinal, Ordinal_str),
	string__append("enum_functor_desc_", Ordinal_str, Str).
dump_rtti_name(notag_functor_desc, Str) :-
	Str = "notag_functor_desc_".
dump_rtti_name(du_functor_desc(Ordinal), Str) :-
	string__int_to_string(Ordinal, Ordinal_str),
	string__append("du_functor_desc_", Ordinal_str, Str).
dump_rtti_name(res_functor_desc(Ordinal), Str) :-
	string__int_to_string(Ordinal, Ordinal_str),
	string__append("res_functor_desc_", Ordinal_str, Str).
dump_rtti_name(enum_name_ordered_table, Str) :-
	Str = "enum_name_ordered_table".
dump_rtti_name(enum_value_ordered_table, Str) :-
	Str = "enum_value_ordered_table".
dump_rtti_name(du_name_ordered_table, Str) :-
	Str = "du_name_ordered_table".
dump_rtti_name(du_stag_ordered_table(Ptag), Str) :-
	string__int_to_string(Ptag, Ptag_str),
	string__append("du_stag_ordered_table_", Ptag_str, Str).
dump_rtti_name(du_ptag_ordered_table, Str) :-
	Str = "du_ptag_ordered_table".
dump_rtti_name(du_ptag_layout(Ptag), Str) :-
	string__int_to_string(Ptag, Ptag_str),
	string__append("du_ptag_layout", Ptag_str, Str).
dump_rtti_name(res_value_ordered_table, Str) :-
	Str = "res_value_ordered_table".
dump_rtti_name(res_name_ordered_table, Str) :-
	Str = "res_name_ordered_table".
dump_rtti_name(maybe_res_addr_functor_desc, Str) :-
	Str = "maybe_res_addr_functor_desc".
dump_rtti_name(type_layout, Str) :-
	Str = "type_layout".
dump_rtti_name(type_functors, Str) :-
	Str = "type_functors".
dump_rtti_name(type_ctor_info, Str) :-
	Str = "type_ctor_info".
dump_rtti_name(type_info(_TypeInfo), Str) :-
	% XXX should give more info than this
	Str = "type_info".
dump_rtti_name(pseudo_type_info(_PseudoTypeInfo), Str) :-
	% XXX should give more info than this
	Str = "pseudo_type_info".
dump_rtti_name(type_hashcons_pointer, Str) :-
	Str = "type_hashcons_pointer".

dump_tc_rtti_name(base_typeclass_info(_ModuleName, InstanceStr),
		Str) :-
	string__append_list(["base_typeclass_info(", InstanceStr, ")"], Str).
dump_tc_rtti_name(type_class_id, "type_class_id").
dump_tc_rtti_name(type_class_decl, "type_class_decl").
dump_tc_rtti_name(type_class_decl_super(Ordinal, _), Str) :-
	string__int_to_string(Ordinal, OrdinalStr),
	string__append_list(["type_class_decl_super(", OrdinalStr, ")"], Str).
dump_tc_rtti_name(type_class_decl_supers, "type_class_decl_supers").
dump_tc_rtti_name(type_class_id_method_ids,
		"type_class_id_method_ids").
dump_tc_rtti_name(type_class_id_var_names,
		"type_class_id_var_names").
dump_tc_rtti_name(type_class_instance(TCTypes), Str) :-
	dump_rtti_type_class_instance_types(TCTypes, InstanceStr),
	string__append_list(["type_class_instance(", InstanceStr, ")"], Str).
dump_tc_rtti_name(type_class_instance_tc_type_vector(TCTypes),
		Str) :-
	dump_rtti_type_class_instance_types(TCTypes, InstanceStr),
	string__append_list(["type_class_instance_tc_types_vector(",
		InstanceStr, ")"], Str).
dump_tc_rtti_name(type_class_instance_constraints(TCTypes), Str) :-
	dump_rtti_type_class_instance_types(TCTypes, InstanceStr),
	string__append_list(["type_class_instance_constraints(",
		InstanceStr, ")"], Str).
dump_tc_rtti_name(type_class_instance_constraint(TCTypes,
		Ordinal, _), Str) :-
	dump_rtti_type_class_instance_types(TCTypes, InstanceStr),
	string__int_to_string(Ordinal, OrdinalStr),
	string__append_list(["type_class_instance_constraint(",
		InstanceStr, ", ", OrdinalStr, ")"], Str).
dump_tc_rtti_name(type_class_instance_methods(TCTypes), Str) :-
	dump_rtti_type_class_instance_types(TCTypes, InstanceStr),
	string__append_list(["type_class_instance_methods(",
		InstanceStr, ")"], Str).

dump_rtti_type_class_name(tc_name(ModuleName, ClassName, Arity),
		Str) :-
	ModuleNameStr = sym_name_mangle(ModuleName),
	ClassNameStr = name_mangle(ClassName),
	string__int_to_string(Arity, ArityStr),
	string__append_list(["tc_name(", ModuleNameStr, ", ",
		ClassNameStr, ArityStr, ")"], Str).

dump_rtti_type_class_instance_types(TCTypes, Str) :-
	EncodedTCTypes = list__map(rtti__encode_tc_instance_type, TCTypes),
	string__append_list(EncodedTCTypes, TypesStr),
	string__append_list(["tc_instance(", TypesStr, ")"],
		Str).

dump_layout_name(label_layout(ProcLabel, LabelNum, LabelVars),
		Str) :-
	dump_label(internal(LabelNum, ProcLabel), LabelStr),
	(
		LabelVars = label_has_var_info,
		LabelVarsStr = "label_has_var_info"
	;
		LabelVars = label_has_no_var_info,
		LabelVarsStr = "label_has_no_var_info"
	),
	string__append_list(["label_layout(", LabelStr, ", ",
		LabelVarsStr, ")"], Str).
dump_layout_name(proc_layout(RttiProcLabel, _), Str) :-
	dump_rttiproclabel(RttiProcLabel, ProcLabelStr),
	string__append_list(["proc_layout(", ProcLabelStr, ")"], Str).
dump_layout_name(proc_layout_exec_trace(RttiProcLabel), Str) :-
	dump_rttiproclabel(RttiProcLabel, ProcLabelStr),
	string__append_list(["proc_layout_exec_trace(", ProcLabelStr, ")"],
		Str).
dump_layout_name(proc_layout_head_var_nums(RttiProcLabel), Str) :-
	dump_rttiproclabel(RttiProcLabel, ProcLabelStr),
	string__append_list(["proc_layout_head_var_nums(", ProcLabelStr, ")"],
		Str).
dump_layout_name(proc_layout_var_names(RttiProcLabel), Str) :-
	dump_rttiproclabel(RttiProcLabel, ProcLabelStr),
	string__append_list(["proc_layout_var_names(", ProcLabelStr, ")"],
		Str).
dump_layout_name(proc_layout_body_bytecode(RttiProcLabel), Str) :-
	dump_rttiproclabel(RttiProcLabel, ProcLabelStr),
	string__append_list(["proc_layout_body_bytecode(", ProcLabelStr, ")"],
		Str).
dump_layout_name(closure_proc_id(ProcLabel, SeqNo, _), Str) :-
	dump_proclabel(ProcLabel, ProcLabelStr),
	string__int_to_string(SeqNo, SeqNoStr),
	string__append_list(["closure_proc_id(", ProcLabelStr, ", ",
		SeqNoStr, ")"], Str).
dump_layout_name(file_layout(ModuleName, FileNum), Str) :-
	ModuleNameStr = sym_name_mangle(ModuleName),
	string__int_to_string(FileNum, FileNumStr),
	string__append_list(["file_layout(", ModuleNameStr, ", ",
		FileNumStr, ")"], Str).
dump_layout_name(file_layout_line_number_vector(ModuleName,
		FileNum), Str) :-
	ModuleNameStr = sym_name_mangle(ModuleName),
	string__int_to_string(FileNum, FileNumStr),
	string__append_list(["file_layout_line_number_vector(", ModuleNameStr,
		", ", FileNumStr, ")"], Str).
dump_layout_name(file_layout_label_layout_vector(ModuleName,
		FileNum), Str) :-
	ModuleNameStr = sym_name_mangle(ModuleName),
	string__int_to_string(FileNum, FileNumStr),
	string__append_list(["file_layout_label_layout_vector(", ModuleNameStr,
		", ", FileNumStr, ")"], Str).
dump_layout_name(module_layout_string_table(ModuleName), Str) :-
	ModuleNameStr = sym_name_mangle(ModuleName),
	string__append_list(["module_layout_string_table(", ModuleNameStr,
		")"], Str).
dump_layout_name(module_layout_file_vector(ModuleName), Str) :-
	ModuleNameStr = sym_name_mangle(ModuleName),
	string__append_list(["module_layout_file_vector(", ModuleNameStr, ")"],
		Str).
dump_layout_name(module_layout_proc_vector(ModuleName), Str) :-
	ModuleNameStr = sym_name_mangle(ModuleName),
	string__append_list(["module_layout_proc_vector(", ModuleNameStr, ")"],
		Str).
dump_layout_name(module_layout_label_exec_count(ModuleName, NumLabels), Str) :-
	ModuleNameStr = sym_name_mangle(ModuleName),
	NumLabelsStr = int_to_string(NumLabels),
	string__append_list(["module_layout_label_exec_count(",
		ModuleNameStr, ", ", NumLabelsStr, ")"], Str).
dump_layout_name(module_layout(ModuleName), Str) :-
	ModuleNameStr = sym_name_mangle(ModuleName),
	string__append_list(["module_layout(", ModuleNameStr, ")"], Str).
dump_layout_name(proc_static(RttiProcLabel), Str) :-
	ProcLabel = make_proc_label_from_rtti(RttiProcLabel),
	dump_proclabel(ProcLabel, ProcLabelStr),
	string__append_list(["proc_static(", ProcLabelStr, ")"], Str).
dump_layout_name(proc_static_call_sites(RttiProcLabel), Str) :-
	ProcLabel = make_proc_label_from_rtti(RttiProcLabel),
	dump_proclabel(ProcLabel, ProcLabelStr),
	string__append_list(["proc_static_call_sites(", ProcLabelStr, ")"],
		Str).
dump_layout_name(table_io_decl(RttiProcLabel), Str) :-
	ProcLabel = make_proc_label_from_rtti(RttiProcLabel),
	dump_proclabel(ProcLabel, ProcLabelStr),
	string__append_list(["table_io_decl(", ProcLabelStr, ")"], Str).
dump_layout_name(table_gen_info(RttiProcLabel), Str) :-
	ProcLabel = make_proc_label_from_rtti(RttiProcLabel),
	dump_proclabel(ProcLabel, ProcLabelStr),
	string__append_list(["table_gen_info(", ProcLabelStr, ")"], Str).
dump_layout_name(table_gen_enum_params(RttiProcLabel), Str) :-
	ProcLabel = make_proc_label_from_rtti(RttiProcLabel),
	dump_proclabel(ProcLabel, ProcLabelStr),
	string__append_list(["table_gen_enum_params(", ProcLabelStr, ")"], Str).
dump_layout_name(table_gen_steps(RttiProcLabel), Str) :-
	ProcLabel = make_proc_label_from_rtti(RttiProcLabel),
	dump_proclabel(ProcLabel, ProcLabelStr),
	string__append_list(["table_gen_steps(", ProcLabelStr, ")"], Str).

dump_unop(mktag, "mktag").
dump_unop(tag, "tag").
dump_unop(unmktag, "unmktag").
dump_unop(strip_tag, "strip_tag").
dump_unop(mkbody, "mkbody").
dump_unop(unmkbody, "unmkbody").
dump_unop(not, "not").
dump_unop(hash_string, "hash_string").
dump_unop(bitwise_complement, "bitwise_complement").

dump_binop(Op, String) :-
	llds_out__binary_op_to_string(Op, String).

dump_maybe_rvals([], _, "").
dump_maybe_rvals([MR | MRs], N, Str) :-
	( N > 0 ->
		( MR = yes(R) ->
			dump_rval(R, MR_str)
		;
			MR_str = "no"
		),
		N1 = N - 1,
		dump_maybe_rvals(MRs, N1, MRs_str),
		string__append_list([MR_str, ", ", MRs_str], Str)
	;
		Str = "truncated"
	).

dump_code_addr(label(Label), Str) :-
	dump_label(Label, Str).
dump_code_addr(imported(ProcLabel), Str) :-
	dump_proclabel(ProcLabel, Str).
dump_code_addr(succip, "succip").
dump_code_addr(do_succeed(Last), Str) :-
	(
		Last = no,
		Str = "do_succeed"
	;
		Last = yes,
		Str = "do_last_succeed"
	).
dump_code_addr(do_redo, "do_redo").
dump_code_addr(do_fail, "do_fail").
dump_code_addr(do_trace_redo_fail_shallow,
	"do_trace_redo_fail_shallow").
dump_code_addr(do_trace_redo_fail_deep, "do_trace_redo_fail_deep").
dump_code_addr(do_call_closure, "do_nondet_closure").
dump_code_addr(do_call_class_method, "do_nondet_class_method").
dump_code_addr(do_not_reached, "do_not_reached").

dump_code_addrs([], "").
dump_code_addrs([Addr | Addrs], Str) :-
	dump_code_addr(Addr, A_str),
	dump_code_addrs(Addrs, A2_str),
	string__append_list([" ", A_str, A2_str], Str).

dump_label(internal(N, ProcLabel), Str) :-
	dump_proclabel(ProcLabel, P_str),
	string__int_to_string(N, N_str),
	string__append_list([P_str, "_", N_str], Str).
dump_label(entry(_, ProcLabel), Str) :-
	dump_proclabel(ProcLabel, Str).

dump_labels([], "").
dump_labels([Label | Labels], Str) :-
	dump_label(Label, L_str),
	dump_labels(Labels, L2_str),
	string__append_list([" ", L_str, L2_str], Str).

dump_label_pairs([], "").
dump_label_pairs([L1 - L2 | Labels], Str) :-
	dump_label(L1, L1_str),
	dump_label(L2, L2_str),
	dump_label_pairs(Labels, L_str),
	string__append_list([" ", L1_str, "-", L2_str, L_str], Str).

:- pred dump_rttiproclabel(rtti_proc_label::in, string::out) is det.

dump_rttiproclabel(RttiProcLabel, Str) :-
	ProcLabel = make_proc_label_from_rtti(RttiProcLabel),
	dump_proclabel(ProcLabel, Str).

dump_proclabel(proc(Module, _PredOrFunc, PredModule,
		PredName, Arity, Mode), Str) :-
	( Module = PredModule ->
		ExtraModule = ""
	;
		PredModuleName = sym_name_mangle(PredModule),
		string__append(PredModuleName, "_", ExtraModule)
	),
	ModuleName = sym_name_mangle(Module),
	string__int_to_string(Arity, A_str),
	string__int_to_string(Mode, M_str),
	string__append_list([ExtraModule, ModuleName, "_", PredName,
		"_", A_str, "_", M_str], Str).
dump_proclabel(special_proc(Module, SpecialPredId, TypeModule,
		TypeName, TypeArity, Mode), Str) :-
	ModuleName = sym_name_mangle(Module),
	TypeModuleName = sym_name_mangle(TypeModule),
	QualTypeName = qualify_name(TypeModuleName, TypeName),
	string__int_to_string(TypeArity, A_str),
	string__int_to_string(Mode, M_str),
	TypeCtor = qualified(TypeModule, TypeName) - TypeArity,
	SpecialPredStr = special_pred_name(SpecialPredId, TypeCtor),
	string__append_list([ModuleName, "_", SpecialPredStr, "_",
		QualTypeName, "_", A_str, "_", M_str], Str).

dump_bool(yes, "yes").
dump_bool(no, "no").

dump_code_model(model_det, "model_det").
dump_code_model(model_semi, "model_semi").
dump_code_model(model_non, "model_non").

dump_instr(comment(Comment), Str) :-
	string__append_list(["comment(", Comment, ")"], Str).
dump_instr(livevals(Livevals), Str) :-
	dump_livevals(Livevals, L_str),
	string__append_list(["livevals(", L_str, ")"], Str).
dump_instr(block(RTemps, FTemps, _), Str) :-
	string__int_to_string(RTemps, R_str),
	string__int_to_string(FTemps, F_str),
	string__append_list(["block(", R_str, ", ", F_str, ", ...)"], Str).
dump_instr(assign(Lval, Rval), Str) :-
	dump_lval(Lval, L_str),
	dump_rval(Rval, R_str),
	string__append_list(["assign(", L_str, ", ", R_str, ")"], Str).
dump_instr(call(Proc, Ret, _, _, _, _), Str) :-
	dump_code_addr(Proc, P_str),
	dump_code_addr(Ret, R_str),
	string__append_list(["call(", P_str, ", ", R_str, ", ...)"], Str).
dump_instr(mkframe(FrameInfo, MaybeRedoip), Str) :-
	(
		MaybeRedoip = yes(Redoip),
		dump_code_addr(Redoip, R_str)
	;
		MaybeRedoip = no,
		R_str = "no_redoip"
	),
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
dump_instr(label(Label), Str) :-
	dump_label(Label, L_str),
	string__append_list(["label(", L_str, ")"], Str).
dump_instr(goto(CodeAddr), Str) :-
	dump_code_addr(CodeAddr, C_str),
	string__append_list(["goto(", C_str, ")"], Str).
dump_instr(computed_goto(Rval, Labels), Str) :-
	dump_rval(Rval, R_str),
	dump_labels(Labels, L_str),
	string__append_list(["computed_goto(", R_str, ", ", L_str, ")"], Str).
dump_instr(c_code(Code, _), Str) :-
	string__append_list(["c_code(", Code, ")"], Str).
dump_instr(if_val(Rval, CodeAddr), Str) :-
	dump_rval(Rval, R_str),
	dump_code_addr(CodeAddr, C_str),
	string__append_list(["if_val(", R_str, ", ", C_str, ")"], Str).
dump_instr(incr_hp(Lval, MaybeTag, MaybeOffset, Size, _), Str) :-
	dump_lval(Lval, L_str),
	(
		MaybeTag = no,
		T_str = "no"
	;
		MaybeTag = yes(Tag),
		string__int_to_string(Tag, T_str)
	),
	(
		MaybeOffset = no,
		O_str = "no"
	;
		MaybeOffset = yes(Offset),
		string__int_to_string(Offset, O_str)
	),
	dump_rval(Size, S_str),
	string__append_list(["incr_hp(", L_str, ", ", T_str, ", ", O_str,
		", ", S_str, ")"], Str).
dump_instr(mark_hp(Lval), Str) :-
	dump_lval(Lval, L_str),
	string__append_list(["mark_hp(", L_str, ")"], Str).
dump_instr(restore_hp(Rval), Str) :-
	dump_rval(Rval, R_str),
	string__append_list(["restore_hp(", R_str, ")"], Str).
dump_instr(free_heap(Rval), Str) :-
	dump_rval(Rval, R_str),
	string__append_list(["free_heap(", R_str, ")"], Str).
dump_instr(store_ticket(Lval), Str) :-
	dump_lval(Lval, L_str),
	string__append_list(["store_ticket(", L_str, ")"], Str).
dump_instr(reset_ticket(Rval, _Reason), Str) :-
	dump_rval(Rval, R_str),
	string__append_list(["reset_ticket(", R_str, ", _)"], Str).
dump_instr(discard_ticket, "discard_ticket").
dump_instr(prune_ticket, "prune_ticket").
dump_instr(mark_ticket_stack(Lval), Str) :-
	dump_lval(Lval, L_str),
	string__append_list(["mark_ticket_stack(", L_str, ")"], Str).
dump_instr(prune_tickets_to(Rval), Str) :-
	dump_rval(Rval, R_str),
	string__append_list(["prune_tickets_to(", R_str, ")"], Str).
dump_instr(incr_sp(Size, _), Str) :-
	string__int_to_string(Size, S_str),
	string__append_list(["incr_sp(", S_str, ")"], Str).
dump_instr(decr_sp(Size), Str) :-
	string__int_to_string(Size, S_str),
	string__append_list(["decr_sp(", S_str, ")"], Str).
dump_instr(init_sync_term(Lval, N), Str) :-
	dump_lval(Lval, L_str),
	string__int_to_string(N, N_str),
	string__append_list(["init_sync_term(", L_str, ", ", N_str, ")"], Str).
dump_instr(fork(Child, Parent, Lval), Str) :-
	dump_label(Child, ChildStr),
	dump_label(Parent, ParentStr),
	string__int_to_string(Lval, LvalStr),
	string__append_list(["fork(", ChildStr, ", ", ParentStr, ", ",
		LvalStr, ")"], Str).
dump_instr(join_and_terminate(Lval), Str) :-
	dump_lval(Lval, LvalStr),
	string__append_list(["join_and_terminate(", LvalStr, ")"], Str).
dump_instr(join_and_continue(Lval, Label), Str) :-
	dump_lval(Lval, LvalStr),
	dump_label(Label, LabelStr),
	string__append_list(["join(", LvalStr, ", ", LabelStr, ")"], Str).
% XXX  should probably give more info than this
dump_instr(pragma_c(_, Comps, _, _, _, _, _, _, _), Str) :-
	dump_components(Comps, C_str),
	string__append_list(["pragma_c(", C_str, ")"], Str).

:- pred dump_components(list(pragma_c_component)::in, string::out) is det.

dump_components([], "").
dump_components([Comp | Comps], Str) :-
	dump_component(Comp, Str1),
	dump_components(Comps, Str2),
	string__append(Str1, Str2, Str).

:- pred dump_component(pragma_c_component::in, string::out) is det.

dump_component(pragma_c_inputs(_), "").
dump_component(pragma_c_outputs(_), "").
dump_component(pragma_c_user_code(_, Code), Code).
dump_component(pragma_c_raw_code(Code, _), Code).
dump_component(pragma_c_fail_to(Label), Code) :-
	dump_label(Label, LabelStr),
	string__append_list(["fail to ", LabelStr], Code).
dump_component(pragma_c_noop, "").

dump_fullinstr(Uinstr - Comment, Str) :-
	dump_instr(Uinstr, U_str),
	string__append_list([U_str, " - ", Comment, "\n"], Str).

dump_fullinstrs([], "").
dump_fullinstrs([Instr | Instrs], Str) :-
	dump_fullinstr(Instr, S1_str),
	dump_fullinstrs(Instrs, S2_str),
	string__append_list([S1_str, S2_str], Str).
