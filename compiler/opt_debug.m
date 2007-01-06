%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2007 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: opt_debug.m.
% Main author: zs.
%
% Debugging support for LLDS to LLDS peephole optimization.
%
%-----------------------------------------------------------------------------%

:- module ll_backend.opt_debug.
:- interface.

:- import_module backend_libs.builtin_ops.
:- import_module backend_libs.rtti.
:- import_module hlds.code_model.
:- import_module ll_backend.layout.
:- import_module ll_backend.livemap.
:- import_module ll_backend.llds.
:- import_module mdbcomp.prim_data.

:- import_module assoc_list.
:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module maybe.
:- import_module pair.

%-----------------------------------------------------------------------------%

:- pred msg(bool::in, int::in, string::in, io::di, io::uo) is det.

:- pred maybe_dump_instrs(bool::in, proc_label::in, list(instruction)::in,
    io::di, io::uo) is det.

:- func dump_intlist(list(int)) = string.

:- func dump_livemap(maybe(proc_label), livemap) = string.

:- func dump_livemaplist(maybe(proc_label), assoc_list(label, lvalset))
    = string.

:- func dump_livevals(maybe(proc_label), lvalset) = string.

:- func dump_livelist(maybe(proc_label), list(lval)) = string.

:- func dump_reg(reg_type, int) = string.

:- func dump_lval(maybe(proc_label), lval) = string.

:- func dump_rval(maybe(proc_label), rval) = string.

:- func dump_rvals(maybe(proc_label), list(rval)) = string.

:- func dump_mem_ref(maybe(proc_label), mem_ref) = string.

:- func dump_const(maybe(proc_label), rval_const) = string.

:- func dump_data_addr(data_addr) = string.

:- func dump_data_name(data_name) = string.

:- func dump_rtti_type_ctor(rtti_type_ctor) = string.

:- func dump_rtti_type_class_name(tc_name) = string.

:- func dump_rtti_type_class_instance_types(list(tc_type)) = string.

:- func dump_rtti_name(ctor_rtti_name) = string.

:- func dump_tc_rtti_name(tc_rtti_name) = string.

:- func dump_layout_name(layout_name) = string.

:- func dump_unop(unary_op) = string.

:- func dump_binop(binary_op) = string.

:- func dump_label(maybe(proc_label), label) = string.

:- func dump_labels(maybe(proc_label), list(label)) = string.

:- func dump_label_pairs(maybe(proc_label), list(pair(label))) = string.

:- func dump_proclabel(proc_label) = string.

:- func dump_maybe_rvals(maybe(proc_label), list(maybe(rval)), int) = string.

:- func dump_code_addr(maybe(proc_label), code_addr) = string.

:- func dump_code_addrs(maybe(proc_label), list(code_addr)) = string.

:- func dump_bool(bool) = string.

:- func dump_code_model(code_model) = string.

:- func dump_stack_incr_kind(stack_incr_kind) = string.

:- func dump_instr(proc_label, bool, instr) = string.

:- func dump_fullinstr(proc_label, bool, instruction) = string.

:- func dump_fullinstrs(proc_label, bool, list(instruction)) = string.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.proc_label.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_rtti.
:- import_module hlds.special_pred.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module ll_backend.llds_out.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_foreign.

:- import_module char.
:- import_module int.
:- import_module map.
:- import_module set.
:- import_module string.
:- import_module term.

msg(OptDebug, LabelNo, Msg, !IO) :-
    (
        OptDebug = yes,
        io.write_string("\n", !IO),
        io.write_string(Msg, !IO),
        ( LabelNo >= 0 ->
            io.write_string(", next label no: ", !IO),
            io.write_int(LabelNo, !IO)
        ;
            true
        ),
        io.write_string("\n", !IO)
    ;
        OptDebug = no
    ).

maybe_dump_instrs(OptDebug, ProcLabel, Instrs, !IO) :-
    (
        OptDebug = yes,
        globals.io_lookup_bool_option(auto_comments, PrintComments,
            !IO),
        dump_instrs_2(Instrs, ProcLabel, PrintComments, !IO)
    ;
        OptDebug = no
    ).

:- pred dump_instrs_2(list(instruction)::in, proc_label::in, bool::in,
    io::di, io::uo) is det.

dump_instrs_2([], _ProcLabel, _PrintComments, !IO).
dump_instrs_2([Instr | Instrs], ProcLabel, PrintComments, !IO) :-
    Instr = llds_instr(Uinstr, Comment),
    ( Uinstr = label(_) ->
        io.write_string(dump_instr(ProcLabel, PrintComments, Uinstr), !IO)
    ; Uinstr = comment(InstrComment) ->
        io.write_string("\t% ", !IO),
        string.foldl(print_comment_char, InstrComment, !IO)
    ;
        io.write_string("\t", !IO),
        io.write_string(dump_instr(ProcLabel, PrintComments, Uinstr), !IO)
    ),
    (
        PrintComments = yes,
        Comment \= ""
    ->
        io.write_string("\n\t\t" ++ Comment, !IO)
    ;
        true
    ),
    io.nl(!IO),
    dump_instrs_2(Instrs, ProcLabel, PrintComments, !IO).

:- pred print_comment_char(char::in, io::di, io::uo) is det.

print_comment_char(C, !IO) :-
    ( C = '\n' ->
        io.write_string("\n\t% ", !IO)
    ;
        io.write_char(C, !IO)
    ).

dump_intlist([]) = "".
dump_intlist([H | T]) =
    " " ++ int_to_string(H) ++ dump_intlist(T).

dump_livemap(MaybeProcLabel, Livemap) =
    dump_livemaplist(MaybeProcLabel, map.to_assoc_list(Livemap)).

dump_livemaplist(_, []) = "".
dump_livemaplist(MaybeProcLabel, [Label - Lvalset | Livemaplist]) =
    dump_label(MaybeProcLabel, Label) ++ " ->" ++
        dump_livevals(MaybeProcLabel, Lvalset) ++ "\n" ++
        dump_livemaplist(MaybeProcLabel, Livemaplist).

dump_livevals(MaybeProcLabel, Lvalset) =
    dump_livelist(MaybeProcLabel, set.to_sorted_list(Lvalset)).

dump_livelist(MaybeProcLabel, Lvals) =
    dump_livelist_2(MaybeProcLabel, Lvals, "").

:- func dump_livelist_2(maybe(proc_label), list(lval), string) = string.

dump_livelist_2(_, [], _) = "".
dump_livelist_2(MaybeProcLabel, [Lval | Lvallist], Prefix) =
    Prefix ++ dump_lval(MaybeProcLabel, Lval) ++
        dump_livelist_2(MaybeProcLabel, Lvallist, " ").

dump_reg(reg_r, N) =
    "r" ++ int_to_string(N).
dump_reg(reg_f, N) =
    "f" ++ int_to_string(N).

dump_lval(_, reg(Type, Num)) =
    dump_reg(Type, Num).
dump_lval(_, stackvar(N)) =
    "sv" ++ int_to_string(N).
dump_lval(_, parent_stackvar(N)) =
    "parent_sv" ++ int_to_string(N).
dump_lval(_, framevar(N)) =
    "fv" ++ int_to_string(N).
dump_lval(_, succip) = "succip".
dump_lval(_, maxfr) = "maxfr".
dump_lval(_, curfr) = "curfr".
dump_lval(MaybeProcLabel, succfr_slot(R)) =
    "succfr_slot(" ++ dump_rval(MaybeProcLabel, R) ++ ")".
dump_lval(MaybeProcLabel, prevfr_slot(R)) =
    "prevfr_slot(" ++ dump_rval(MaybeProcLabel, R) ++ ")".
dump_lval(MaybeProcLabel, redofr_slot(R)) =
    "redofr_slot(" ++ dump_rval(MaybeProcLabel, R) ++ ")".
dump_lval(MaybeProcLabel, redoip_slot(R)) =
    "redoip_slot(" ++ dump_rval(MaybeProcLabel, R) ++ ")".
dump_lval(MaybeProcLabel, succip_slot(R)) =
    "succip_slot(" ++ dump_rval(MaybeProcLabel, R) ++ ")".
dump_lval(_, hp) = "hp".
dump_lval(_, sp) = "sp".
dump_lval(_, parent_sp) = "parent_sp".
dump_lval(MaybeProcLabel, field(MT, N, F)) = Str :-
    (
        MT = yes(T),
        string.int_to_string(T, T_str)
    ;
        MT = no,
        T_str = "no"
    ),
    Str = "field(" ++ T_str ++ ", " ++ dump_rval(MaybeProcLabel, N) ++ ", "
        ++ dump_rval(MaybeProcLabel, F) ++ ")".
dump_lval(_, lvar(_)) = "lvar(_)".
dump_lval(_, temp(Type, Num)) =
    "temp_" ++ dump_reg(Type, Num).
dump_lval(MaybeProcLabel, mem_ref(R)) =
    "mem_ref(" ++ dump_rval(MaybeProcLabel, R) ++ ")".
dump_lval(_, global_var_ref(env_var_ref(VarName))) =
    "global_var_ref(env_var_ref(" ++ VarName ++ "))".

dump_rval(MaybeProcLabel, lval(Lval)) =
    dump_lval(MaybeProcLabel, Lval).
dump_rval(_, var(Var)) =
    "var(" ++ int_to_string(term.var_to_int(Var)) ++ ")".
dump_rval(MaybeProcLabel, mkword(T, N)) =
    "mkword(" ++ int_to_string(T) ++ ", " ++
        dump_rval(MaybeProcLabel, N) ++ ")".
dump_rval(MaybeProcLabel, const(C)) =
    dump_const(MaybeProcLabel, C).
dump_rval(MaybeProcLabel, unop(O, N)) =
    dump_unop(O) ++ "(" ++ dump_rval(MaybeProcLabel, N) ++ ")".
dump_rval(MaybeProcLabel, binop(O, N1, N2)) =
    (
        ( N1 = binop(_, _, _)
        ; N2 = binop(_, _, _)
        )
    ->
        "binop(" ++ dump_binop(O) ++ ", "
            ++ dump_rval(MaybeProcLabel, N1) ++ ", " ++
            dump_rval(MaybeProcLabel, N2) ++ ")"
    ;
        dump_rval(MaybeProcLabel, N1) ++ " " ++ dump_binop(O) ++ " " ++
        dump_rval(MaybeProcLabel, N2)
    ).
dump_rval(MaybeProcLabel, mem_addr(M)) =
    "mem_addr(" ++ dump_mem_ref(MaybeProcLabel, M) ++ ")".

dump_rvals(_, []) = "".
dump_rvals(MaybeProcLabel, [Rval | Rvals]) =
    dump_rval(MaybeProcLabel, Rval) ++ ", " ++
        dump_rvals(MaybeProcLabel, Rvals).

dump_mem_ref(MaybeProcLabel, stackvar_ref(N)) =
    "stackvar_ref(" ++ dump_rval(MaybeProcLabel, N) ++ ")".
dump_mem_ref(MaybeProcLabel, framevar_ref(N)) =
    "framevar_ref(" ++ dump_rval(MaybeProcLabel, N) ++ ")".
dump_mem_ref(MaybeProcLabel, heap_ref(R, T, N)) =
    "heap_ref(" ++ dump_rval(MaybeProcLabel, R) ++ ", " ++ int_to_string(T) ++ ", "
        ++ dump_rval(MaybeProcLabel, N) ++ ")".

dump_const(_, llconst_true) = "true".
dump_const(_, llconst_false) = "false".
dump_const(_, llconst_int(I)) =
    int_to_string(I).
dump_const(_, llconst_float(F)) =
    float_to_string(F).
dump_const(_, llconst_string(S)) =
    """" ++ S ++ """".
dump_const(_, llconst_multi_string(L, _S)) =
    "multi_string(" ++ int_to_string(L) ++ ")".
dump_const(MaybeProcLabel, llconst_code_addr(CodeAddr)) =
    "code_addr_const(" ++ dump_code_addr(MaybeProcLabel, CodeAddr) ++ ")".
dump_const(_, llconst_data_addr(DataAddr, MaybeOffset)) = Str :-
    DataAddrStr = dump_data_addr(DataAddr),
    (
        MaybeOffset = no,
        Str = "data_addr_const(" ++ DataAddrStr ++ ")"
    ;
        MaybeOffset = yes(Offset),
        Str = "data_addr_const(" ++ DataAddrStr ++ ", "
            ++ int_to_string(Offset) ++ ")"
    ).

dump_data_addr(data_addr(ModuleName, DataName)) =
    "data_addr(" ++ sym_name_to_string(ModuleName) ++ ", "
        ++ dump_data_name(DataName) ++ ")".
dump_data_addr(rtti_addr(ctor_rtti_id(RttiTypeCtor, DataName))) =
    "rtti_addr(" ++ dump_rtti_type_ctor(RttiTypeCtor) ++ ", "
        ++ dump_rtti_name(DataName) ++ ")".
dump_data_addr(rtti_addr(tc_rtti_id(TCName, TCDataName))) =
    "tc_rtti_addr(" ++ dump_rtti_type_class_name(TCName) ++ ", "
        ++ dump_tc_rtti_name(TCDataName) ++ ")".
dump_data_addr(layout_addr(LayoutName)) =
    "layout_addr(" ++ dump_layout_name(LayoutName) ++ ")".

dump_data_name(scalar_common_ref(type_num(TypeNum), Offset)) =
    "scalar_common_ref(" ++ int_to_string(TypeNum) ++ ", "
        ++ int_to_string(Offset) ++ ")".
dump_data_name(vector_common_ref(type_num(TypeNum), Offset)) =
    "vector_common_ref(" ++ int_to_string(TypeNum) ++ ", "
        ++ int_to_string(Offset) ++ ")".
dump_data_name(proc_tabling_ref(ProcLabel, Id)) =
    tabling_info_id_str(Id) ++ "(" ++ dump_proclabel(ProcLabel) ++ ")".

dump_rtti_type_ctor(rtti_type_ctor(ModuleName, TypeName, Arity)) =
    "rtti_type_ctor(" ++ sym_name_mangle(ModuleName) ++ ", "
        ++ name_mangle(TypeName) ++ int_to_string(Arity) ++ ")".

dump_rtti_name(type_ctor_exist_locns(Ordinal)) =
    "exist_locns_" ++ int_to_string(Ordinal).
dump_rtti_name(type_ctor_exist_locn) = "exist_loc".
dump_rtti_name(type_ctor_exist_tc_constr(Ordinal, TCCNum, Arity)) =
    "exist_tc_constr_" ++ int_to_string(Ordinal) ++ "_"
        ++ int_to_string(TCCNum) ++ "_" ++ int_to_string(Arity).
dump_rtti_name(type_ctor_exist_tc_constrs(Ordinal)) =
    "exist_tc_constrs_" ++ int_to_string(Ordinal).
dump_rtti_name(type_ctor_exist_info(Ordinal)) =
    "exist_info_" ++ int_to_string(Ordinal).
dump_rtti_name(type_ctor_field_names(Ordinal)) =
    "field_names_" ++ int_to_string(Ordinal).
dump_rtti_name(type_ctor_field_types(Ordinal)) =
    "field_types_" ++ int_to_string(Ordinal).
dump_rtti_name(type_ctor_res_addrs) = "res_addrs".
dump_rtti_name(type_ctor_res_addr_functors) = "res_addr_functors".
dump_rtti_name(type_ctor_enum_functor_desc(Ordinal)) =
    "enum_functor_desc_" ++ int_to_string(Ordinal).
dump_rtti_name(type_ctor_notag_functor_desc) = "notag_functor_desc_".
dump_rtti_name(type_ctor_du_functor_desc(Ordinal)) =
    "du_functor_desc_" ++ int_to_string(Ordinal).
dump_rtti_name(type_ctor_res_functor_desc(Ordinal)) =
    "res_functor_desc_" ++ int_to_string(Ordinal).
dump_rtti_name(type_ctor_enum_name_ordered_table) = "enum_name_ordered_table".
dump_rtti_name(type_ctor_enum_value_ordered_table) =
    "enum_value_ordered_table".
dump_rtti_name(type_ctor_du_name_ordered_table) = "du_name_ordered_table".
dump_rtti_name(type_ctor_du_stag_ordered_table(Ptag)) =
    "du_stag_ordered_table_" ++ int_to_string(Ptag).
dump_rtti_name(type_ctor_du_ptag_ordered_table) = "du_ptag_ordered_table".
dump_rtti_name(type_ctor_du_ptag_layout(Ptag)) =
    "du_ptag_layout" ++ int_to_string(Ptag).
dump_rtti_name(type_ctor_res_value_ordered_table) = "res_value_ordered_table".
dump_rtti_name(type_ctor_res_name_ordered_table) = "res_name_ordered_table".
dump_rtti_name(type_ctor_maybe_res_addr_functor_desc) =
    "maybe_res_addr_functor_desc".
dump_rtti_name(type_ctor_type_layout) = "type_layout".
dump_rtti_name(type_ctor_type_functors) = "type_functors".
dump_rtti_name(type_ctor_functor_number_map) = "functor_number_map".
dump_rtti_name(type_ctor_type_ctor_info) = "type_ctor_info".
dump_rtti_name(type_ctor_type_info(_TypeInfo)) = "type_info".
    % XXX Should give more info than this for _TypeInfo.
dump_rtti_name(type_ctor_pseudo_type_info(_PseudoTypeInfo)) =
    "pseudo_type_info".
    % XXX Should give more info than this for _PseudoTypeInfo.
dump_rtti_name(type_ctor_type_hashcons_pointer) = "type_hashcons_pointer".

dump_tc_rtti_name(type_class_base_typeclass_info(_ModuleName, InstanceStr)) =
    "base_typeclass_info(" ++ InstanceStr ++ ")".
dump_tc_rtti_name(type_class_id) = "type_class_id".
dump_tc_rtti_name(type_class_decl) = "type_class_decl".
dump_tc_rtti_name(type_class_decl_super(Ordinal, _)) =
    "type_class_decl_super(" ++ int_to_string(Ordinal) ++ ")".
dump_tc_rtti_name(type_class_decl_supers) = "type_class_decl_supers".
dump_tc_rtti_name(type_class_id_method_ids) = "type_class_id_method_ids".
dump_tc_rtti_name(type_class_id_var_names) = "type_class_id_var_names".
dump_tc_rtti_name(type_class_instance(TCTypes)) =
    "type_class_instance("
        ++ dump_rtti_type_class_instance_types(TCTypes) ++ ")".
dump_tc_rtti_name(type_class_instance_tc_type_vector(TCTypes)) =
    "type_class_instance_tc_types_vector("
    ++ dump_rtti_type_class_instance_types(TCTypes) ++ ")".
dump_tc_rtti_name(type_class_instance_constraints(TCTypes)) =
    "type_class_instance_constraints("
        ++ dump_rtti_type_class_instance_types(TCTypes) ++ ")".
dump_tc_rtti_name(type_class_instance_constraint(TCTypes, Ordinal, _)) =
    "type_class_instance_constraint("
        ++ dump_rtti_type_class_instance_types(TCTypes) ++ ", "
        ++ int_to_string(Ordinal) ++ ")".
dump_tc_rtti_name(type_class_instance_methods(TCTypes)) =
    "type_class_instance_methods("
        ++ dump_rtti_type_class_instance_types(TCTypes) ++ ")".

dump_rtti_type_class_name(tc_name(ModuleName, ClassName, Arity)) = Str :-
    Str = "tc_name(" ++ sym_name_mangle(ModuleName) ++ ", "
        ++ name_mangle(ClassName) ++ int_to_string(Arity) ++ ")".

dump_rtti_type_class_instance_types(TCTypes) = Str :-
    EncodedTCTypes = list.map(rtti.encode_tc_instance_type, TCTypes),
    string.append_list(EncodedTCTypes, TypesStr),
    Str = "tc_instance(" ++ TypesStr ++ ")".

dump_layout_name(label_layout(ProcLabel, LabelNum, LabelVars)) = Str :-
    LabelStr = dump_label(no, internal_label(LabelNum, ProcLabel)),
    (
        LabelVars = label_has_var_info,
        LabelVarsStr = "label_has_var_info"
    ;
        LabelVars = label_has_no_var_info,
        LabelVarsStr = "label_has_no_var_info"
    ),
    Str = "label_layout(" ++ LabelStr ++ ", " ++ LabelVarsStr ++ ")".
dump_layout_name(user_event_layout(ProcLabel, LabelNum)) = Str :-
    LabelStr = dump_label(no, internal_label(LabelNum, ProcLabel)),
    Str = "user_event_layout(" ++ LabelStr ++ ")".
dump_layout_name(user_event_attr_var_nums(ProcLabel, LabelNum)) = Str :-
    LabelStr = dump_label(no, internal_label(LabelNum, ProcLabel)),
    Str = "user_event_attr_var_nums(" ++ LabelStr ++ ")".
dump_layout_name(proc_layout(RttiProcLabel, _)) =
    "proc_layout(" ++ dump_rttiproclabel(RttiProcLabel) ++ ")".
dump_layout_name(proc_layout_exec_trace(RttiProcLabel)) =
    "proc_layout_exec_trace(" ++ dump_rttiproclabel(RttiProcLabel) ++ ")".
dump_layout_name(proc_layout_head_var_nums(RttiProcLabel)) =
    "proc_layout_head_var_nums(" ++ dump_rttiproclabel(RttiProcLabel) ++ ")".
dump_layout_name(proc_layout_var_names(RttiProcLabel)) =
    "proc_layout_var_names(" ++ dump_rttiproclabel(RttiProcLabel) ++ ")".
dump_layout_name(proc_layout_body_bytecode(RttiProcLabel)) =
    "proc_layout_body_bytecode(" ++ dump_rttiproclabel(RttiProcLabel) ++ ")".
dump_layout_name(closure_proc_id(ProcLabel, SeqNo, _)) =
    "closure_proc_id(" ++ dump_proclabel(ProcLabel)
        ++ int_to_string(SeqNo) ++ ")".
dump_layout_name(file_layout(ModuleName, FileNum)) =
    "file_layout(" ++ sym_name_mangle(ModuleName)
        ++ int_to_string(FileNum) ++ ")".
dump_layout_name(file_layout_line_number_vector(ModuleName, FileNum)) =
    "file_layout_line_number_vector(" ++ sym_name_mangle(ModuleName)
        ++ int_to_string(FileNum) ++ ")".
dump_layout_name(file_layout_label_layout_vector(ModuleName, FileNum)) =
    "file_layout_label_layout_vector(" ++ sym_name_mangle(ModuleName)
        ++ int_to_string(FileNum) ++ ")".
dump_layout_name(module_layout_string_table(ModuleName)) =
    "module_layout_string_table(" ++ sym_name_mangle(ModuleName) ++ ")".
dump_layout_name(module_layout_file_vector(ModuleName)) =
    "module_layout_file_vector(" ++ sym_name_mangle(ModuleName) ++ ")".
dump_layout_name(module_layout_proc_vector(ModuleName)) =
    "module_layout_proc_vector(" ++ sym_name_mangle(ModuleName) ++ ")".
dump_layout_name(module_layout_label_exec_count(ModuleName, NumLabels)) =
    "module_layout_label_exec_count(" ++ sym_name_mangle(ModuleName)
        ++ ", " ++ int_to_string(NumLabels) ++ ")".
dump_layout_name(module_layout_event_set_desc(ModuleName)) =
    "module_layout_event_set_desc(" ++ sym_name_mangle(ModuleName) ++ ")".
dump_layout_name(module_layout_event_specs(ModuleName)) =
    "module_layout_event_specs(" ++ sym_name_mangle(ModuleName) ++ ")".
dump_layout_name(module_layout_event_arg_names(ModuleName, EventNum)) =
    "module_layout_event_arg_names(" ++ sym_name_mangle(ModuleName) ++
        ", " ++ int_to_string(EventNum) ++ ")".
dump_layout_name(module_layout_event_synth_attrs(ModuleName, EventNum)) =
    "module_layout_event_synth_attrs(" ++ sym_name_mangle(ModuleName) ++
        ", " ++ int_to_string(EventNum) ++ ")".
dump_layout_name(module_layout_event_synth_attr_args(ModuleName,
        EventNum, ArgNum)) =
    "module_layout_event_synth_attr_args(" ++ sym_name_mangle(ModuleName) ++
        ", " ++ int_to_string(EventNum) ++
        ", " ++ int_to_string(ArgNum) ++ ")".
dump_layout_name(module_layout_event_synth_attr_order(ModuleName,
        EventNum, ArgNum)) =
    "module_layout_event_synth_attr_order(" ++ sym_name_mangle(ModuleName) ++
        ", " ++ int_to_string(EventNum) ++
        ", " ++ int_to_string(ArgNum) ++ ")".
dump_layout_name(module_layout_event_synth_order(ModuleName, EventNum)) =
    "module_layout_event_synth_order(" ++ sym_name_mangle(ModuleName) ++
        ", " ++ int_to_string(EventNum) ++ ")".
dump_layout_name(module_layout(ModuleName)) =
    "module_layout(" ++ sym_name_mangle(ModuleName) ++ ")".
dump_layout_name(proc_static(RttiProcLabel)) =
    "proc_static(" ++ dump_rttiproclabel(RttiProcLabel) ++ ")".
dump_layout_name(proc_static_call_sites(RttiProcLabel)) =
    "proc_static_call_sites(" ++ dump_rttiproclabel(RttiProcLabel) ++ ")".
dump_layout_name(table_io_decl(RttiProcLabel)) =
    "table_io_decl(" ++ dump_rttiproclabel(RttiProcLabel) ++ ")".

dump_unop(mktag) = "mktag".
dump_unop(tag) = "tag".
dump_unop(unmktag) = "unmktag".
dump_unop(strip_tag) = "strip_tag".
dump_unop(mkbody) = "mkbody".
dump_unop(unmkbody) = "unmkbody".
dump_unop(logical_not) = "not".
dump_unop(hash_string) = "hash_string".
dump_unop(bitwise_complement) = "bitwise_complement".

dump_binop(Op) =
    llds_out.binary_op_to_string(Op).

dump_maybe_rvals(_, [], _) = "".
dump_maybe_rvals(MaybeProcLabel, [MR | MRs], N) = Str :-
    ( N > 0 ->
        (
            MR = yes(R),
            MR_str = dump_rval(MaybeProcLabel, R)
        ;
            MR = no,
            MR_str = "no"
        ),
        Str = MR_str ++ ", " ++ dump_maybe_rvals(MaybeProcLabel, MRs, N - 1)
    ;
        Str = "truncated"
    ).

dump_code_addr(MaybeProcLabel, code_label(Label)) =
    dump_label(MaybeProcLabel, Label).
dump_code_addr(_, code_imported_proc(ProcLabel)) = dump_proclabel(ProcLabel).
dump_code_addr(_, code_succip) = "succip".
dump_code_addr(_, do_succeed(no)) = "do_succeed".
dump_code_addr(_, do_succeed(yes)) = "do_last_succeed".
dump_code_addr(_, do_redo) = "do_redo".
dump_code_addr(_, do_fail) = "do_fail".
dump_code_addr(_, do_trace_redo_fail_shallow) = "do_trace_redo_fail_shallow".
dump_code_addr(_, do_trace_redo_fail_deep) = "do_trace_redo_fail_deep".
dump_code_addr(_, do_call_closure(Variant)) =
    "do_call_closure_" ++ ho_call_variant_to_string(Variant).
dump_code_addr(_, do_call_class_method(Variant)) =
    "do_call_class_method_" ++ ho_call_variant_to_string(Variant).
dump_code_addr(_, do_not_reached) = "do_not_reached".

dump_code_addrs(_, []) = "".
dump_code_addrs(MaybeProcLabel, [Addr | Addrs]) =
    " " ++ dump_code_addr(MaybeProcLabel, Addr) ++
        dump_code_addrs(MaybeProcLabel, Addrs).

dump_label(no, internal_label(N, ProcLabel)) =
    dump_proclabel(ProcLabel) ++ "_i" ++ int_to_string(N).
dump_label(no, entry_label(_, ProcLabel)) =
    dump_proclabel(ProcLabel).
dump_label(yes(CurProcLabel), internal_label(N, ProcLabel)) = Str :-
    string.int_to_string(N, N_str),
    ( CurProcLabel = ProcLabel ->
        Str = "local_" ++ N_str
    ;
        Str = dump_proclabel(ProcLabel) ++ "_" ++ N_str
    ).
dump_label(yes(CurProcLabel), entry_label(_, ProcLabel)) = Str :-
    ( CurProcLabel = ProcLabel ->
        Str = "CUR_PROC_ENTRY"
    ;
        Str = dump_proclabel(ProcLabel)
    ).

dump_labels(_, []) = "".
dump_labels(MaybeProcLabel, [Label | Labels]) =
    " " ++ dump_label(MaybeProcLabel, Label) ++
        dump_labels(MaybeProcLabel, Labels).

dump_label_pairs(_, []) = "".
dump_label_pairs(MaybeProcLabel, [L1 - L2 | Labels]) =
    " " ++ dump_label(MaybeProcLabel, L1) ++ "-" ++
        dump_label(MaybeProcLabel, L2) ++
        dump_label_pairs(MaybeProcLabel, Labels).

:- func dump_rttiproclabel(rtti_proc_label) = string.

dump_rttiproclabel(RttiProcLabel) =
    dump_proclabel(make_proc_label_from_rtti(RttiProcLabel)).

dump_proclabel(ProcLabel) = Str :-
    (
        ProcLabel = ordinary_proc_label(Module, _PredOrFunc, PredModule,
            PredName, Arity, Mode),
        ( Module = PredModule ->
            ExtraModule = ""
        ;
            PredModuleName = sym_name_mangle(PredModule),
            ExtraModule = PredModuleName ++ "_"
        ),
        Str = ExtraModule ++ sym_name_mangle(Module) ++ "_" ++ PredName ++ "_"
            ++ int_to_string(Arity) ++ "_" ++ int_to_string(Mode)

    ;
        ProcLabel = special_proc_label(Module, SpecialPredId, TypeModule,
            TypeName, TypeArity, Mode),
        TypeCtor = type_ctor(qualified(TypeModule, TypeName), TypeArity),
        Str = sym_name_mangle(Module) ++ "_"
            ++ special_pred_name(SpecialPredId, TypeCtor) ++ "_"
            ++ qualify_name(sym_name_mangle(TypeModule), TypeName) ++ "_"
            ++ int_to_string(TypeArity) ++ "_" ++ int_to_string(Mode)
    ).

dump_bool(yes) = "yes".
dump_bool(no) = "no".

dump_code_model(model_det) = "model_det".
dump_code_model(model_semi) = "model_semi".
dump_code_model(model_non) = "model_non".

dump_stack_incr_kind(stack_incr_leaf) = "leaf".
dump_stack_incr_kind(stack_incr_nonleaf) = "nonleaf".

dump_instr(ProcLabel, PrintComments, Instr) = Str :-
    (
        Instr = comment(Comment),
        Str = "comment(" ++ Comment ++ ")"
    ;
        Instr = livevals(Livevals),
        Str = "livevals(" ++ dump_livevals(yes(ProcLabel), Livevals) ++ ")"
    ;
        Instr = block(RTemps, FTemps, Instrs),
        Str = "block(" ++ int_to_string(RTemps) ++ ", "
            ++ int_to_string(FTemps) ++ ",\n"
            ++ dump_fullinstrs(ProcLabel, PrintComments, Instrs)
            ++ ")"
    ;
        Instr = assign(Lval, Rval),
        Str = dump_lval(yes(ProcLabel), Lval) ++ " := " ++
            dump_rval(yes(ProcLabel), Rval)
    ;
        Instr = llcall(Callee, ReturnLabel, _LiveInfo, _Context, _GoalPath,
            CallModel),
        (
            CallModel = call_model_det,
            CallModelStr = "det"
        ;
            CallModel = call_model_semidet,
            CallModelStr = "semidet"
        ;
            CallModel = call_model_nondet(no_tail_call),
            CallModelStr = "nondet no_tail_call"
        ;
            CallModel = call_model_nondet(checked_tail_call),
            CallModelStr = "nondet checked_tail_call"
        ;
            CallModel = call_model_nondet(unchecked_tail_call),
            CallModelStr = "nondet unchecked_tail_call"
        ),
        Str = "call(" ++ dump_code_addr(yes(ProcLabel), Callee) ++ ", "
            ++ dump_code_addr(yes(ProcLabel), ReturnLabel) ++ ", ..., "
            ++ CallModelStr ++ ")"
    ;
        Instr = mkframe(FrameInfo, MaybeRedoip),
        (
            MaybeRedoip = yes(Redoip),
            R_str = dump_code_addr(yes(ProcLabel), Redoip)
        ;
            MaybeRedoip = no,
            R_str = "no_redoip"
        ),
        (
            FrameInfo = ordinary_frame(Name, Size, MaybePragma),
            (
                MaybePragma = yes(foreign_proc_struct(StructName, Fields, _)),
                P_str = "yes(" ++ StructName ++ ", " ++ Fields ++ ")"
            ;
                MaybePragma = no,
                P_str = "no"
            ),
            Str = "mkframe(" ++ Name ++ ", " ++ int_to_string(Size) ++ ", "
                ++ P_str ++ ", " ++ R_str ++ ")"
        ;
            FrameInfo = temp_frame(Kind),
            (
                Kind = nondet_stack_proc,
                Str = "mktempframe(" ++ R_str ++ ")"
            ;
                Kind = det_stack_proc,
                Str = "mkdettempframe(" ++ R_str ++ ")"
            )
        )
    ;
        Instr = label(Label),
        Str = dump_label(yes(ProcLabel), Label) ++ ":"
    ;
        Instr = goto(CodeAddr),
        Str = "goto " ++ dump_code_addr(yes(ProcLabel), CodeAddr)
    ;
        Instr = computed_goto(Rval, Labels),
        Str = "computed_goto " ++ dump_rval(yes(ProcLabel), Rval) ++ ":"
            ++ dump_labels(yes(ProcLabel), Labels)
    ;
        Instr = arbitrary_c_code(AL, _, Code),
        Str = "arbitrary_c_code(" ++ dump_affects_liveness(AL) ++ "\n" ++
            Code ++ ")"
    ;
        Instr = if_val(Rval, CodeAddr),
        Str = "if (" ++ dump_rval(yes(ProcLabel), Rval) ++ ") goto "
            ++ dump_code_addr(yes(ProcLabel), CodeAddr)
    ;
        Instr = save_maxfr(Lval),
        Str = "save_maxfr(" ++ dump_lval(yes(ProcLabel), Lval) ++ ")"
    ;
        Instr = restore_maxfr(Lval),
        Str = "restore_maxfr(" ++ dump_lval(yes(ProcLabel), Lval) ++ ")"
    ;
        Instr = incr_hp(Lval, MaybeTag, MaybeOffset, Size, _, MayUseAtomic),
        (
            MaybeTag = no,
            T_str = "no"
        ;
            MaybeTag = yes(Tag),
            string.int_to_string(Tag, T_str)
        ),
        (
            MaybeOffset = no,
            O_str = "no"
        ;
            MaybeOffset = yes(Offset),
            string.int_to_string(Offset, O_str)
        ),
        Str = "incr_hp(" ++ dump_lval(yes(ProcLabel), Lval) ++ ", " ++
            T_str ++ ", " ++ O_str ++ ", " ++
            dump_rval(yes(ProcLabel), Size) ++ ", " ++
            dump_may_use_atomic(MayUseAtomic) ++ ")"
    ;
        Instr = mark_hp(Lval),
        Str = "mark_hp(" ++ dump_lval(yes(ProcLabel), Lval) ++ ")"
    ;
        Instr = restore_hp(Rval),
        Str = "restore_hp(" ++ dump_rval(yes(ProcLabel), Rval) ++ ")"
    ;
        Instr = free_heap(Rval),
        Str = "free_heap(" ++ dump_rval(yes(ProcLabel), Rval) ++ ")"
    ;
        Instr = store_ticket(Lval),
        Str = "store_ticket(" ++ dump_lval(yes(ProcLabel), Lval) ++ ")"
    ;
        Instr = reset_ticket(Rval, _Reason),
        Str = "reset_ticket(" ++ dump_rval(yes(ProcLabel), Rval) ++ ", _)"
    ;
        Instr = discard_ticket,
        Str = "discard_ticket"
    ;
        Instr = prune_ticket,
        Str = "prune_ticket"
    ;
        Instr = mark_ticket_stack(Lval),
        Str = "mark_ticket_stack(" ++ dump_lval(yes(ProcLabel), Lval) ++ ")"
    ;
        Instr = prune_tickets_to(Rval),
        Str = "prune_tickets_to(" ++ dump_rval(yes(ProcLabel), Rval) ++ ")"
    ;
        Instr = incr_sp(Size, _, Kind),
        Str = "incr_sp(" ++ int_to_string(Size) ++ ", " ++
            dump_stack_incr_kind(Kind) ++ ")"
    ;
        Instr = decr_sp(Size),
        Str = "decr_sp(" ++ int_to_string(Size) ++ ")"
    ;
        Instr = decr_sp_and_return(Size),
        Str = "decr_sp_and_return(" ++ int_to_string(Size) ++ ")"
    ;
        Instr = init_sync_term(Lval, N),
        Str = "init_sync_term(" ++ dump_lval(yes(ProcLabel), Lval) ++ ", "
            ++ int_to_string(N) ++ ")"
    ;
        Instr = fork(Child),
        Str = "fork(" ++ dump_label(yes(ProcLabel), Child) ++ ")"
    ;
        Instr = join_and_continue(Lval, Label),
        Str = "join(" ++ dump_lval(yes(ProcLabel), Lval) ++ ", "
            ++ dump_label(yes(ProcLabel), Label) ++ ")"
    ;
        Instr = foreign_proc_code(Decls, Comps, MCM, MFNL, MFL, MFOL, MNF,
            SSR, MD),
        MaybeProcLabel = yes(ProcLabel),
        Str = "foreign_proc_code(\n"
            ++ "declarations:\n" ++ dump_decls(Decls)
            ++ "components:\n" ++ dump_components(MaybeProcLabel, Comps)
            ++ dump_may_call_mercury(MCM) ++ "\n"
            ++ dump_maybe_label("fix nolayout:", MaybeProcLabel, MFNL)
            ++ dump_maybe_label("fix layout:", MaybeProcLabel, MFL)
            ++ dump_maybe_label("fix onlylayout:", MaybeProcLabel, MFOL)
            ++ dump_maybe_label("nofix:", MaybeProcLabel, MNF)
            ++ dump_bool_msg("stack slot ref:", SSR)
            ++ dump_bool_msg("may duplicate:", MD)
            ++ ")"
    ).

:- func dump_may_call_mercury(proc_may_call_mercury) = string.

dump_may_call_mercury(proc_may_call_mercury) = "may_call_mercury".
dump_may_call_mercury(proc_will_not_call_mercury) = "will_not_call_mercury".

:- func dump_maybe_label(string, maybe(proc_label), maybe(label)) = string.

dump_maybe_label(_Msg, _MaybeProcLabel, no) = "".
dump_maybe_label(Msg, MaybeProcLabel, yes(Label)) =
    Msg ++ " " ++ dump_label(MaybeProcLabel, Label) ++ "\n".

:- func dump_bool_msg(string, bool) = string.

dump_bool_msg(Msg, no)  = Msg ++ " no\n".
dump_bool_msg(Msg, yes) = Msg ++ " yes\n".

:- func dump_may_use_atomic(may_use_atomic_alloc) = string.

dump_may_use_atomic(may_use_atomic_alloc) = "may_use_atomic_alloc".
dump_may_use_atomic(may_not_use_atomic_alloc) = "may_not_use_atomic_alloc".

:- func dump_decls(list(foreign_proc_decl)) = string.

dump_decls([]) = "".
dump_decls([Decl | Decls]) =
    dump_decl(Decl) ++ dump_decls(Decls).

:- func dump_decl(foreign_proc_decl) = string.

dump_decl(foreign_proc_arg_decl(_MerType, TypeStr, VarName)) =
    "decl " ++ TypeStr ++ " " ++ VarName ++ "\n".
dump_decl(foreign_proc_struct_ptr_decl(StructTag, VarName)) =
    "decl struct" ++ StructTag ++ " " ++ VarName ++ "\n".

:- func dump_components(maybe(proc_label), list(foreign_proc_component))
    = string.

dump_components(_, []) = "".
dump_components(MaybeProcLabel, [Comp | Comps]) =
    dump_component(MaybeProcLabel, Comp) ++
        dump_components(MaybeProcLabel, Comps).

:- func dump_component(maybe(proc_label), foreign_proc_component) = string.

dump_component(MaybeProcLabel, foreign_proc_inputs(Inputs)) =
    dump_input_components(MaybeProcLabel, Inputs).
dump_component(MaybeProcLabel, foreign_proc_outputs(Outputs)) =
    dump_output_components(MaybeProcLabel, Outputs).
dump_component(_, foreign_proc_user_code(_, AL, Code)) =
    dump_affects_liveness(AL) ++ "\n" ++ Code ++ "\n".
dump_component(_, foreign_proc_raw_code(_, AL, _, Code)) =
    dump_affects_liveness(AL) ++ "\n" ++ Code ++ "\n".
dump_component(MaybeProcLabel, foreign_proc_fail_to(Label)) =
    "fail to " ++ dump_label(MaybeProcLabel, Label) ++ "\n".
dump_component(_, foreign_proc_noop) = "".

:- func dump_affects_liveness(affects_liveness) = string.

dump_affects_liveness(affects_liveness) = "affects_liveness".
dump_affects_liveness(does_not_affect_liveness) = "does_not_affect_liveness".
dump_affects_liveness(default_affects_liveness) = "default_affects_liveness".

:- func dump_input_components(maybe(proc_label), list(foreign_proc_input))
    = string.

dump_input_components(_, []) = "".
dump_input_components(MaybeProcLabel, [Input | Inputs]) =
    dump_input_component(MaybeProcLabel, Input) ++ "\n" ++
    dump_input_components(MaybeProcLabel, Inputs).

:- func dump_output_components(maybe(proc_label), list(foreign_proc_output))
    = string.

dump_output_components(_, []) = "".
dump_output_components(MaybeProcLabel, [Input | Inputs]) =
    dump_output_component(MaybeProcLabel, Input) ++ "\n" ++
    dump_output_components(MaybeProcLabel, Inputs).

:- func dump_input_component(maybe(proc_label), foreign_proc_input) = string.

dump_input_component(MaybeProcLabel,
        foreign_proc_input(Var, _, Dummy, _, Rval, _, _)) =
    Var ++ dump_maybe_dummy(Dummy) ++ " := " ++
        dump_rval(MaybeProcLabel, Rval).

:- func dump_output_component(maybe(proc_label), foreign_proc_output) = string.

dump_output_component(MaybeProcLabel,
        foreign_proc_output(Lval, _, Dummy, _, Var, _, _)) =
    dump_lval(MaybeProcLabel, Lval) ++ " := " ++ Var ++
        dump_maybe_dummy(Dummy).

:- func dump_maybe_dummy(bool) = string.

dump_maybe_dummy(no) = "".
dump_maybe_dummy(yes) = " (dummy)".

dump_fullinstr(ProcLabel, PrintComments, llds_instr(Uinstr, Comment)) = Str :-
    (
        PrintComments = no,
        Str = dump_instr(ProcLabel, PrintComments, Uinstr) ++ "\n"
    ;
        PrintComments = yes,
        Str = dump_instr(ProcLabel, PrintComments, Uinstr) ++
            " - " ++ Comment ++ "\n"
    ).

dump_fullinstrs(_ProcLabel, _PrintComments, []) = "".
dump_fullinstrs(ProcLabel, PrintComments, [Instr | Instrs]) =
    dump_fullinstr(ProcLabel, PrintComments, Instr)
    ++ dump_fullinstrs(ProcLabel, PrintComments, Instrs).
