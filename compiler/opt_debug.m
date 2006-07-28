%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% Debugging support for LLDS to LLDS peephole optimization.

% Main author: zs.

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

:- func dump_livemap(livemap) = string.

:- func dump_livemap(proc_label, livemap) = string.

:- func dump_livemaplist(assoc_list(label, lvalset)) = string.

:- func dump_livemaplist(proc_label, assoc_list(label, lvalset)) = string.

:- func dump_livevals(lvalset) = string.

:- func dump_livelist(list(lval)) = string.

:- func dump_reg(reg_type, int) = string.

:- func dump_lval(lval) = string.

:- func dump_rval(rval) = string.

:- func dump_rvals(list(rval)) = string.

:- func dump_mem_ref(mem_ref) = string.

:- func dump_const(rval_const) = string.

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

:- func dump_label(label) = string.

:- func dump_label(proc_label, label) = string.

:- func dump_labels(list(label)) = string.

:- func dump_labels(proc_label, list(label)) = string.

:- func dump_label_pairs(list(pair(label))) = string.

:- func dump_proclabel(proc_label) = string.

:- func dump_maybe_rvals(list(maybe(rval)), int) = string.

:- func dump_code_addr(code_addr) = string.

:- func dump_code_addr(proc_label, code_addr) = string.

:- func dump_code_addrs(list(code_addr)) = string.

:- func dump_code_addrs(proc_label, list(code_addr)) = string.

:- func dump_bool(bool) = string.

:- func dump_instr(proc_label, bool, instr) = string.

:- func dump_fullinstr(proc_label, bool, instruction) = string.

:- func dump_fullinstrs(proc_label, bool, list(instruction)) = string.

:- func dump_code_model(code_model) = string.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.name_mangle.
:- import_module backend_libs.proc_label.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_rtti.
:- import_module hlds.special_pred.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module ll_backend.code_util.
:- import_module ll_backend.llds_out.
:- import_module ll_backend.opt_util.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_foreign.
:- import_module parse_tree.prog_out.

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
dump_instrs_2([Uinstr - Comment | Instrs], ProcLabel, PrintComments, !IO) :-
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

dump_livemap(Livemap) =
    dump_livemaplist(map.to_assoc_list(Livemap)).

dump_livemap(ProcLabel, Livemap) =
    dump_livemaplist(ProcLabel, map.to_assoc_list(Livemap)).

dump_livemaplist([]) = "".
dump_livemaplist([Label - Lvalset | Livemaplist]) =
    dump_label(Label) ++ " ->" ++ dump_livevals(Lvalset) ++ "\n"
        ++ dump_livemaplist(Livemaplist).

dump_livemaplist(_ProcLabel, []) = "".
dump_livemaplist(ProcLabel, [Label - Lvalset | Livemaplist]) =
    dump_label(ProcLabel, Label) ++ " ->" ++ dump_livevals(Lvalset) ++ "\n"
        ++ dump_livemaplist(ProcLabel, Livemaplist).

dump_livevals(Lvalset) =
    dump_livelist(set.to_sorted_list(Lvalset)).

dump_livelist(Lvals) =
    dump_livelist_2(Lvals, "").

:- func dump_livelist_2(list(lval), string) = string.

dump_livelist_2([], _) = "".
dump_livelist_2([Lval | Lvallist], Prefix) =
    Prefix ++ dump_lval(Lval) ++ dump_livelist_2(Lvallist, " ").

dump_reg(r, N) =
    "r" ++ int_to_string(N).
dump_reg(f, N) =
    "f" ++ int_to_string(N).

dump_lval(reg(Type, Num)) =
    dump_reg(Type, Num).
dump_lval(stackvar(N)) =
    "sv" ++ int_to_string(N).
dump_lval(framevar(N)) =
    "fv" ++ int_to_string(N).
dump_lval(succip) = "succip".
dump_lval(maxfr) = "maxfr".
dump_lval(curfr) = "curfr".
dump_lval(succfr_slot(R)) =
    "succfr_slot(" ++ dump_rval(R) ++ ")".
dump_lval(prevfr_slot(R)) =
    "prevfr_slot(" ++ dump_rval(R) ++ ")".
dump_lval(redofr_slot(R)) =
    "redofr_slot(" ++ dump_rval(R) ++ ")".
dump_lval(redoip_slot(R)) =
    "redoip_slot(" ++ dump_rval(R) ++ ")".
dump_lval(succip_slot(R)) =
    "succip_slot(" ++ dump_rval(R) ++ ")".
dump_lval(hp) = "hp".
dump_lval(sp) = "sp".
dump_lval(field(MT, N, F)) = Str :-
    (
        MT = yes(T),
        string.int_to_string(T, T_str)
    ;
        MT = no,
        T_str = "no"
    ),
    Str = "field(" ++ T_str ++ ", " ++ dump_rval(N) ++ ", "
        ++ dump_rval(F) ++ ")".
dump_lval(lvar(_)) = "lvar(_)".
dump_lval(temp(Type, Num)) =
    "temp_" ++ dump_reg(Type, Num).
dump_lval(mem_ref(R)) =
    "mem_ref(" ++ dump_rval(R) ++ ")".
dump_lval(global_var_ref(env_var_ref(VarName))) =
    "global_var_ref(env_var_ref(" ++ VarName ++ "))".

dump_rval(lval(Lval)) =
    dump_lval(Lval).
dump_rval(var(Var)) =
    "var(" ++ int_to_string(term.var_to_int(Var)) ++ ")".
dump_rval(mkword(T, N)) =
    "mkword(" ++ int_to_string(T) ++ ", " ++ dump_rval(N) ++ ")".
dump_rval(const(C)) =
    dump_const(C).
dump_rval(unop(O, N)) =
    dump_unop(O) ++ "(" ++ dump_rval(N) ++ ")".
dump_rval(binop(O, N1, N2)) =
    (
        ( N1 = binop(_, _, _)
        ; N2 = binop(_, _, _)
        )
    ->
        "binop(" ++ dump_binop(O) ++ ", "
            ++ dump_rval(N1) ++ ", " ++ dump_rval(N2) ++ ")"
    ;
        dump_rval(N1) ++ " " ++ dump_binop(O) ++ " " ++ dump_rval(N2)
    ).
dump_rval(mem_addr(M)) =
    "mem_addr(" ++ dump_mem_ref(M) ++ ")".

dump_rvals([]) = "".
dump_rvals([Rval | Rvals]) =
    dump_rval(Rval) ++ ", " ++ dump_rvals(Rvals).

dump_mem_ref(stackvar_ref(N)) =
    "stackvar_ref(" ++ dump_rval(N) ++ ")".
dump_mem_ref(framevar_ref(N)) =
    "framevar_ref(" ++ dump_rval(N) ++ ")".
dump_mem_ref(heap_ref(R, T, N)) =
    "heap_ref(" ++ dump_rval(R) ++ ", " ++ int_to_string(T) ++ ", "
        ++ dump_rval(N) ++ ")".

dump_const(true) = "true".
dump_const(false) = "false".
dump_const(int_const(I)) =
    int_to_string(I).
dump_const(float_const(F)) =
    float_to_string(F).
dump_const(string_const(S)) =
    """" ++ S ++ """".
dump_const(multi_string_const(L, _S)) =
    "multi_string(" ++ int_to_string(L) ++ ")".
dump_const(code_addr_const(CodeAddr)) =
    "code_addr_const(" ++ dump_code_addr(CodeAddr) ++ ")".
dump_const(data_addr_const(DataAddr, MaybeOffset)) = Str :-
    DataAddr_str = dump_data_addr(DataAddr),
    (
        MaybeOffset = no,
        Str = "data_addr_const(" ++ DataAddr_str ++ ")"
    ;
        MaybeOffset = yes(Offset),
        Str = "data_addr_const(" ++ DataAddr_str ++ ", "
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

dump_data_name(scalar_common_ref(TypeNum, Offset)) =
    "scalar_common_ref(" ++ int_to_string(TypeNum) ++ ", "
        ++ int_to_string(Offset) ++ ")".
dump_data_name(vector_common_ref(TypeNum, Offset)) =
    "vector_common_ref(" ++ int_to_string(TypeNum) ++ ", "
        ++ int_to_string(Offset) ++ ")".
dump_data_name(proc_tabling_ref(ProcLabel, Id)) =
    tabling_info_id_str(Id) ++ "(" ++ dump_proclabel(ProcLabel) ++ ")".

dump_rtti_type_ctor(rtti_type_ctor(ModuleName, TypeName, Arity)) =
    "rtti_type_ctor(" ++ sym_name_mangle(ModuleName) ++ ", "
        ++ name_mangle(TypeName) ++ int_to_string(Arity) ++ ")".

dump_rtti_name(exist_locns(Ordinal)) =
    "exist_locns_" ++ int_to_string(Ordinal).
dump_rtti_name(exist_locn) = "exist_loc".
dump_rtti_name(exist_tc_constr(Ordinal, TCCNum, Arity)) =
    "exist_tc_constr_" ++ int_to_string(Ordinal) ++ "_"
        ++ int_to_string(TCCNum) ++ "_" ++ int_to_string(Arity).
dump_rtti_name(exist_tc_constrs(Ordinal)) =
    "exist_tc_constrs_" ++ int_to_string(Ordinal).
dump_rtti_name(exist_info(Ordinal)) =
    "exist_info_" ++ int_to_string(Ordinal).
dump_rtti_name(field_names(Ordinal)) =
    "field_names_" ++ int_to_string(Ordinal).
dump_rtti_name(field_types(Ordinal)) =
    "field_types_" ++ int_to_string(Ordinal).
dump_rtti_name(res_addrs) = "res_addrs".
dump_rtti_name(res_addr_functors) = "res_addr_functors".
dump_rtti_name(enum_functor_desc(Ordinal)) =
    "enum_functor_desc_" ++ int_to_string(Ordinal).
dump_rtti_name(notag_functor_desc) = "notag_functor_desc_".
dump_rtti_name(du_functor_desc(Ordinal)) =
    "du_functor_desc_" ++ int_to_string(Ordinal).
dump_rtti_name(res_functor_desc(Ordinal)) =
    "res_functor_desc_" ++ int_to_string(Ordinal).
dump_rtti_name(enum_name_ordered_table) = "enum_name_ordered_table".
dump_rtti_name(enum_value_ordered_table) = "enum_value_ordered_table".
dump_rtti_name(du_name_ordered_table) = "du_name_ordered_table".
dump_rtti_name(du_stag_ordered_table(Ptag)) =
    "du_stag_ordered_table_" ++ int_to_string(Ptag).
dump_rtti_name(du_ptag_ordered_table) = "du_ptag_ordered_table".
dump_rtti_name(du_ptag_layout(Ptag)) =
    "du_ptag_layout" ++ int_to_string(Ptag).
dump_rtti_name(res_value_ordered_table) = "res_value_ordered_table".
dump_rtti_name(res_name_ordered_table) = "res_name_ordered_table".
dump_rtti_name(maybe_res_addr_functor_desc) = "maybe_res_addr_functor_desc".
dump_rtti_name(type_layout) = "type_layout".
dump_rtti_name(type_functors) = "type_functors".
dump_rtti_name(type_ctor_info) = "type_ctor_info".
dump_rtti_name(type_info(_TypeInfo)) = "type_info".
    % XXX Should give more info than this for _TypeInfo.
dump_rtti_name(pseudo_type_info(_PseudoTypeInfo)) = "pseudo_type_info".
    % XXX Should give more info than this for _PseudoTypeInfo.
dump_rtti_name(type_hashcons_pointer) = "type_hashcons_pointer".

dump_tc_rtti_name(base_typeclass_info(_ModuleName, InstanceStr)) =
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
    LabelStr = dump_label(internal(LabelNum, ProcLabel)),
    (
        LabelVars = label_has_var_info,
        LabelVarsStr = "label_has_var_info"
    ;
        LabelVars = label_has_no_var_info,
        LabelVarsStr = "label_has_no_var_info"
    ),
    Str = "label_layout(" ++ LabelStr ++ ", " ++ LabelVarsStr ++ ")".
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

dump_maybe_rvals([], _) = "".
dump_maybe_rvals([MR | MRs], N) = Str :-
    ( N > 0 ->
        (
            MR = yes(R),
            MR_str = dump_rval(R)
        ;
            MR = no,
            MR_str = "no"
        ),
        Str = MR_str ++ ", " ++ dump_maybe_rvals(MRs, N - 1)
    ;
        Str = "truncated"
    ).

dump_code_addr(label(Label)) = dump_label(Label).
dump_code_addr(imported(ProcLabel)) = dump_proclabel(ProcLabel).
dump_code_addr(succip) = "succip".
dump_code_addr(do_succeed(Last)) = Str :-
    (
        Last = no,
        Str = "do_succeed"
    ;
        Last = yes,
        Str = "do_last_succeed"
    ).
dump_code_addr(do_redo) = "do_redo".
dump_code_addr(do_fail) = "do_fail".
dump_code_addr(do_trace_redo_fail_shallow) =
    "do_trace_redo_fail_shallow".
dump_code_addr(do_trace_redo_fail_deep) = "do_trace_redo_fail_deep".
dump_code_addr(do_call_closure(Variant)) =
    "do_call_closure_" ++ ho_call_variant_to_string(Variant).
dump_code_addr(do_call_class_method(Variant)) =
    "do_call_class_method_" ++ ho_call_variant_to_string(Variant).
dump_code_addr(do_not_reached) = "do_not_reached".

dump_code_addr(ProcLabel, CodeAddr) =
    ( CodeAddr = label(Label) ->
        dump_label(ProcLabel, Label)
    ;
        dump_code_addr(CodeAddr)
    ).

dump_code_addrs([]) = "".
dump_code_addrs([Addr | Addrs]) =
    " " ++ dump_code_addr(Addr) ++ dump_code_addrs(Addrs).

dump_code_addrs(_, []) = "".
dump_code_addrs(ProcLabel, [Addr | Addrs]) =
    " " ++ dump_code_addr(ProcLabel, Addr)
        ++ dump_code_addrs(ProcLabel, Addrs).

dump_label(internal(N, ProcLabel)) =
    dump_proclabel(ProcLabel) ++ "_i" ++ int_to_string(N).
dump_label(entry(_, ProcLabel)) =
    dump_proclabel(ProcLabel).

dump_label(CurProcLabel, internal(N, ProcLabel)) = Str :-
    string.int_to_string(N, N_str),
    ( CurProcLabel = ProcLabel ->
        Str = "local_" ++ N_str
    ;
        Str = dump_proclabel(ProcLabel) ++ "_" ++ N_str
    ).
dump_label(CurProcLabel, entry(_, ProcLabel)) = Str :-
    ( CurProcLabel = ProcLabel ->
        Str = "CUR_PROC_ENTRY"
    ;
        Str = dump_proclabel(ProcLabel)
    ).

dump_labels([]) = "".
dump_labels([Label | Labels]) =
    " " ++ dump_label(Label) ++ dump_labels(Labels).

dump_labels(_, []) = "".
dump_labels(ProcLabel, [Label | Labels]) =
    " " ++ dump_label(ProcLabel, Label) ++ dump_labels(ProcLabel, Labels).

dump_label_pairs([]) = "".
dump_label_pairs([L1 - L2 | Labels]) =
    " " ++ dump_label(L1) ++ "-" ++ dump_label(L2) ++ dump_label_pairs(Labels).

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

dump_instr(ProcLabel, PrintComments, Instr) = Str :-
    (
        Instr = comment(Comment),
        Str = "comment(" ++ Comment ++ ")"
    ;
        Instr = livevals(Livevals),
        Str = "livevals(" ++ dump_livevals(Livevals) ++ ")"
    ;
        Instr = block(RTemps, FTemps, Instrs),
        Str = "block(" ++ int_to_string(RTemps) ++ ", "
            ++ int_to_string(FTemps) ++ ",\n"
            ++ dump_fullinstrs(ProcLabel, PrintComments, Instrs)
            ++ ")"
    ;
        Instr = assign(Lval, Rval),
        Str = dump_lval(Lval) ++ " := " ++ dump_rval(Rval)
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
        Str = "call(" ++ dump_code_addr(ProcLabel, Callee) ++ ", "
            ++ dump_code_addr(ProcLabel, ReturnLabel) ++ ", ..., "
            ++ CallModelStr ++ ")"
    ;
        Instr = mkframe(FrameInfo, MaybeRedoip),
        (
            MaybeRedoip = yes(Redoip),
            R_str = dump_code_addr(ProcLabel, Redoip)
        ;
            MaybeRedoip = no,
            R_str = "no_redoip"
        ),
        (
            FrameInfo = ordinary_frame(Name, Size, MaybePragma),
            (
                MaybePragma = yes(pragma_c_struct(StructName, Fields, _)),
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
        Str = dump_label(ProcLabel, Label) ++ ":"
    ;
        Instr = goto(CodeAddr),
        Str = "goto " ++ dump_code_addr(ProcLabel, CodeAddr)
    ;
        Instr = computed_goto(Rval, Labels),
        Str = "computed_goto " ++ dump_rval(Rval) ++ ":"
            ++ dump_labels(ProcLabel, Labels)
    ;
        Instr = arbitrary_c_code(Code, _),
        Str = "arbitrary_c_code(" ++ Code ++ ")"
    ;
        Instr = if_val(Rval, CodeAddr),
        Str = "if (" ++ dump_rval(Rval) ++ ") goto "
            ++ dump_code_addr(ProcLabel, CodeAddr)
    ;
        Instr = save_maxfr(Lval),
        Str = "save_maxfr(" ++ dump_lval(Lval) ++ ")"
    ;
        Instr = restore_maxfr(Lval),
        Str = "restore_maxfr(" ++ dump_lval(Lval) ++ ")"
    ;
        Instr = incr_hp(Lval, MaybeTag, MaybeOffset, Size, _),
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
        Str = "incr_hp(" ++ dump_lval(Lval) ++ ", " ++ T_str ++ ", " ++ O_str
            ++ ", " ++ dump_rval(Size) ++ ")"
    ;
        Instr = mark_hp(Lval),
        Str = "mark_hp(" ++ dump_lval(Lval) ++ ")"
    ;
        Instr = restore_hp(Rval),
        Str = "restore_hp(" ++ dump_rval(Rval) ++ ")"
    ;
        Instr = free_heap(Rval),
        Str = "free_heap(" ++ dump_rval(Rval) ++ ")"
    ;
        Instr = store_ticket(Lval),
        Str = "store_ticket(" ++ dump_lval(Lval) ++ ")"
    ;
        Instr = reset_ticket(Rval, _Reason),
        Str = "reset_ticket(" ++ dump_rval(Rval) ++ ", _)"
    ;
        Instr = discard_ticket,
        Str = "discard_ticket"
    ;
        Instr = prune_ticket,
        Str = "prune_ticket"
    ;
        Instr = mark_ticket_stack(Lval),
        Str = "mark_ticket_stack(" ++ dump_lval(Lval) ++ ")"
    ;
        Instr = prune_tickets_to(Rval),
        Str = "prune_tickets_to(" ++ dump_rval(Rval) ++ ")"
    ;
        Instr = incr_sp(Size, _),
        Str = "incr_sp(" ++ int_to_string(Size) ++ ")"
    ;
        Instr = decr_sp(Size),
        Str = "decr_sp(" ++ int_to_string(Size) ++ ")"
    ;
        Instr = decr_sp_and_return(Size),
        Str = "decr_sp_and_return(" ++ int_to_string(Size) ++ ")"
    ;
        Instr = init_sync_term(Lval, N),
        Str = "init_sync_term(" ++ dump_lval(Lval) ++ ", "
            ++ int_to_string(N) ++")"
    ;
        Instr = fork(Child, Parent, NumSlots),
        Str = "fork(" ++ dump_label(ProcLabel, Child) ++ ", "
            ++ dump_label(ProcLabel, Parent) ++ ", "
            ++ int_to_string(NumSlots) ++ ")"
    ;
        Instr = join_and_terminate(Lval),
        Str = "join_and_terminate(" ++ dump_lval(Lval) ++ ")"
    ;
        Instr = join_and_continue(Lval, Label),
        Str = "join(" ++ dump_lval(Lval) ++ ", "
            ++ dump_label(ProcLabel, Label) ++ ")"
    ;
        Instr = pragma_c(Decls, Comps, MCM, MFNL, MFL, MFOL, MNF, SSR, MD),
        Str = "pragma_c(\n"
            ++ "declarations:\n" ++ dump_decls(Decls)
            ++ "components:\n" ++ dump_components(ProcLabel, Comps)
            ++ dump_may_call_mercury(MCM) ++ "\n"
            ++ dump_maybe_label("fix nolayout:", ProcLabel, MFNL)
            ++ dump_maybe_label("fix layout:", ProcLabel, MFL)
            ++ dump_maybe_label("fix onlylayout:", ProcLabel, MFOL)
            ++ dump_maybe_label("nofix:", ProcLabel, MNF)
            ++ dump_bool("stack slot ref:", SSR)
            ++ dump_bool("may duplicate:", MD)
            ++ ")"
    ).

:- func dump_may_call_mercury(may_call_mercury) = string.

dump_may_call_mercury(may_call_mercury) = "may_call_mercury".
dump_may_call_mercury(will_not_call_mercury) = "will_not_call_mercury".

:- func dump_maybe_label(string, proc_label, maybe(label)) = string.

dump_maybe_label(_Msg, _ProcLabel, no) = "".
dump_maybe_label(Msg, ProcLabel, yes(Label)) =
    Msg ++ " " ++ dump_label(ProcLabel, Label) ++ "\n".

:- func dump_bool(string, bool) = string.

dump_bool(Msg, no)  = Msg ++ " no\n".
dump_bool(Msg, yes) = Msg ++ " yes\n".

:- func dump_decls(list(pragma_c_decl)) = string.

dump_decls([]) = "".
dump_decls([Decl | Decls]) =
    dump_decl(Decl) ++ dump_decls(Decls).

:- func dump_decl(pragma_c_decl) = string.

dump_decl(pragma_c_arg_decl(_MerType, TypeStr, VarName)) =
    "decl " ++ TypeStr ++ " " ++ VarName ++ "\n".
dump_decl(pragma_c_struct_ptr_decl(StructTag, VarName)) =
    "decl struct" ++ StructTag ++ " " ++ VarName ++ "\n".

:- func dump_components(proc_label, list(pragma_c_component)) = string.

dump_components(_, []) = "".
dump_components(ProcLabel, [Comp | Comps]) =
    dump_component(ProcLabel, Comp) ++ dump_components(ProcLabel, Comps).

:- func dump_component(proc_label, pragma_c_component) = string.

dump_component(_, pragma_c_inputs(Inputs)) = dump_input_components(Inputs).
dump_component(_, pragma_c_outputs(Outputs)) = dump_output_components(Outputs).
dump_component(_, pragma_c_user_code(_, Code)) = Code ++ "\n".
dump_component(_, pragma_c_raw_code(Code, _, _)) = Code ++ "\n".
dump_component(ProcLabel, pragma_c_fail_to(Label)) =
    "fail to " ++ dump_label(ProcLabel, Label) ++ "\n".
dump_component(_, pragma_c_noop) = "".

:- func dump_input_components(list(pragma_c_input)) = string.

dump_input_components([]) = "".
dump_input_components([Input | Inputs]) =
    dump_input_component(Input) ++ "\n" ++
    dump_input_components(Inputs).

:- func dump_output_components(list(pragma_c_output)) = string.

dump_output_components([]) = "".
dump_output_components([Input | Inputs]) =
    dump_output_component(Input) ++ "\n" ++
    dump_output_components(Inputs).

:- func dump_input_component(pragma_c_input) = string.

dump_input_component(pragma_c_input(Var, _, Dummy, _, Rval, _, _)) =
    Var ++ dump_maybe_dummy(Dummy) ++ " := " ++ dump_rval(Rval).

:- func dump_output_component(pragma_c_output) = string.

dump_output_component(pragma_c_output(Lval, _, Dummy, _, Var, _, _)) =
    dump_lval(Lval) ++ " := " ++ Var ++ dump_maybe_dummy(Dummy).

:- func dump_maybe_dummy(bool) = string.

dump_maybe_dummy(no) = "".
dump_maybe_dummy(yes) = " (dummy)".

dump_fullinstr(ProcLabel, PrintComments, Uinstr - Comment) = Str :-
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
