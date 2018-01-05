%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2007, 2009-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: bytecode.m.
% Author: zs.
%
% This module defines the bytecode used by the debugger.
%
%---------------------------------------------------------------------------%

:- module bytecode_backend.bytecode.
:- interface.

:- import_module backend_libs.
:- import_module backend_libs.builtin_ops.
:- import_module hlds.
:- import_module hlds.hlds_data.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module char.
:- import_module cord.
:- import_module io.
:- import_module list.
:- import_module pair.

%---------------------------------------------------------------------------%

:- type byte_tree   ==  cord(byte_code).

:- type byte_code
    --->    byte_enter_pred(byte_pred_id, int, byte_is_func, int)
    ;       byte_endof_pred
    ;       byte_enter_proc(byte_proc_id, determinism, int, int, int,
                list(byte_var_info))
    ;       byte_endof_proc
    ;       byte_label(byte_label_id)
    ;       byte_enter_disjunction(byte_label_id)
    ;       byte_endof_disjunction
    ;       byte_enter_disjunct(byte_label_id)
    ;       byte_endof_disjunct(byte_label_id)
    ;       byte_enter_switch(byte_var, byte_label_id)
    ;       byte_endof_switch
    ;       byte_enter_switch_arm(byte_cons_id, list(byte_cons_id),
                byte_label_id)
    ;       byte_endof_switch_arm(byte_label_id)
    ;       byte_enter_if(byte_label_id, byte_label_id, byte_temp)
    ;       byte_enter_then(byte_temp)
    ;       byte_endof_then(byte_label_id)
    ;       byte_enter_else(byte_temp)
    ;       byte_endof_if
    ;       byte_enter_negation(byte_temp, byte_label_id)
    ;       byte_endof_negation_goal(byte_temp)
    ;       byte_endof_negation
    ;       byte_enter_commit(byte_temp)
    ;       byte_endof_commit(byte_temp)
    ;       byte_assign(byte_var, byte_var)
    ;       byte_test(byte_var, byte_var, byte_test_id)
    ;       byte_construct(byte_var, byte_cons_id, list(byte_var))
    ;       byte_deconstruct(byte_var, byte_cons_id, list(byte_var))
    ;       byte_complex_construct(byte_var, byte_cons_id,
                list(pair(byte_var, byte_dir)))
    ;       byte_complex_deconstruct(byte_var, byte_cons_id,
                list(pair(byte_var, byte_dir)))
    ;       byte_place_arg(byte_reg_type, int, byte_var)
    ;       byte_pickup_arg(byte_reg_type, int, byte_var)
    ;       byte_call(byte_module_id, byte_pred_id, arity, byte_is_func,
                byte_proc_id)
    ;       byte_higher_order_call(byte_var, arity, arity, determinism)
    ;       byte_builtin_binop(binary_op, byte_arg, byte_arg, byte_var)
    ;       byte_builtin_unop(unary_op, byte_arg, byte_var)
    ;       byte_builtin_bintest(binary_op, byte_arg, byte_arg)
    ;       byte_builtin_untest(unary_op, byte_arg)
    ;       byte_semidet_succeed
    ;       byte_semidet_success_check
    ;       byte_fail
    ;       byte_context(int)
    ;       byte_not_supported.

    % Currently we only support integer registers.
    % This might one day be extended to support separate
    % floating-point registers.
:- type byte_reg_type
    --->    byte_reg_r.  % general-purpose (integer) register.

:- type byte_cons_id
    --->    byte_cons(byte_module_id, string, arity, byte_cons_tag)
    ;       byte_int_const(int)
    ;       byte_string_const(string)
    ;       byte_float_const(float)
    ;       byte_char_const(char)
    ;       byte_pred_const(byte_module_id, byte_pred_id, arity, byte_is_func,
                byte_proc_id)
    ;       byte_type_ctor_info_const(byte_module_id, string, int)
    ;       byte_base_typeclass_info_const(byte_module_id, class_id, string)
    ;       byte_type_info_cell_constructor
    ;       byte_typeclass_info_cell_constructor.

:- type byte_var_info
    --->    var_info(string, mer_type).

:- type byte_cons_tag
    --->    byte_no_tag
    ;       byte_unshared_tag(tag_bits)
    ;       byte_shared_remote_tag(tag_bits, int)
    ;       byte_shared_local_tag(tag_bits, int)
    ;       byte_enum_tag(int).

:- type byte_arg
    --->    byte_arg_var(byte_var)
    ;       byte_arg_int_const(int)
    ;       byte_arg_float_const(float).

:- type byte_dir
    --->    to_arg
    ;       to_var
    ;       to_none.

:- type byte_test_id
    --->    int_test
    ;       char_test
    ;       string_test
    ;       float_test
    ;       enum_test
    ;       dummy_test.

:- type byte_module_id  ==  module_name.
:- type byte_pred_id    ==  string.
:- type byte_proc_id    ==  int.
:- type byte_label_id   ==  int.
:- type byte_var    ==  int.
:- type byte_temp   ==  int.
:- type byte_is_func    ==  int.    % 0 if a predicate, 1 if a function

:- pred output_bytecode_file(string::in, list(byte_code)::in,
    io::di, io::uo) is det.

:- pred debug_bytecode_file(string::in, list(byte_code)::in,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.bytecode_data.
:- import_module backend_libs.c_util.

:- import_module assoc_list.
:- import_module require.
:- import_module string.

%---------------------------------------------------------------------------%

:- pred bytecode.version(int::out) is det.

bytecode.version(9).

output_bytecode_file(FileName, ByteCodes, !IO) :-
    io.open_binary_output(FileName, Result, !IO),
    (
        Result = ok(FileStream),
        io.set_binary_output_stream(FileStream, OutputStream, !IO),
        bytecode.version(Version),
        output_short(Version, !IO),
        output_bytecode_list(ByteCodes, !IO),
        io.set_binary_output_stream(OutputStream, _, !IO),
        io.close_binary_output(FileStream, !IO)
    ;
        Result = error(_),
        io.progname_base("byte.m", ProgName, !IO),
        io.write_string("\n", !IO),
        io.write_string(ProgName, !IO),
        io.write_string(": can't open `", !IO),
        io.write_string(FileName, !IO),
        io.write_string("' for output\n", !IO),
        io.set_exit_status(1, !IO)
    ).

debug_bytecode_file(FileName, ByteCodes, !IO) :-
    io.open_output(FileName, Result, !IO),
    (
        Result = ok(FileStream),
        io.set_output_stream(FileStream, OutputStream, !IO),
        bytecode.version(Version),
        io.write_string("bytecode_version ", !IO),
        io.write_int(Version, !IO),
        io.write_string("\n", !IO),
        debug_bytecode_list(ByteCodes, !IO),
        io.set_output_stream(OutputStream, _, !IO),
        io.close_output(FileStream, !IO)
    ;
        Result = error(_),
        io.progname_base("byte.m", ProgName, !IO),
        io.write_string("\n", !IO),
        io.write_string(ProgName, !IO),
        io.write_string(": can't open `", !IO),
        io.write_string(FileName, !IO),
        io.write_string("' for output\n", !IO),
        io.set_exit_status(1, !IO)
    ).

:- pred output_bytecode_list(list(byte_code)::in, io::di, io::uo) is det.

output_bytecode_list([], !IO).
output_bytecode_list([ByteCode | ByteCodes], !IO) :-
    byte_code(ByteCode, Byte),
    io.write_byte(Byte, !IO),
    output_args(ByteCode, !IO),
    output_bytecode_list(ByteCodes, !IO).

:- pred debug_bytecode_list(list(byte_code)::in, io::di, io::uo) is det.

debug_bytecode_list([], !IO).
debug_bytecode_list([ByteCode | ByteCodes], !IO) :-
    byte_debug(ByteCode, Debug),
    debug_string(Debug, !IO),
    debug_args(ByteCode, !IO),
    io.write_char('\n', !IO),
    debug_bytecode_list(ByteCodes, !IO).

:- pred output_args(byte_code::in, io::di, io::uo) is det.

output_args(byte_enter_pred(PredId, PredArity, IsFunc, ProcCount), !IO) :-
    output_pred_id(PredId, !IO),
    output_length(PredArity, !IO),
    output_is_func(IsFunc, !IO),
    output_length(ProcCount, !IO).
output_args(byte_endof_pred, !IO).
output_args(byte_enter_proc(ProcId, Detism, LabelCount, LabelId, TempCount,
        Vars), !IO) :-
    output_proc_id(ProcId, !IO),
    output_determinism(Detism, !IO),
    output_length(LabelCount, !IO),
    output_label_id(LabelId, !IO),
    output_length(TempCount, !IO),
    list.length(Vars, VarCount),
    output_length(VarCount, !IO),
    output_var_infos(Vars, !IO).
output_args(byte_endof_proc, !IO).
output_args(byte_label(LabelId), !IO) :-
    output_label_id(LabelId, !IO).
output_args(byte_enter_disjunction(LabelId), !IO) :-
    output_label_id(LabelId, !IO).
output_args(byte_endof_disjunction, !IO).
output_args(byte_enter_disjunct(LabelId), !IO) :-
    output_label_id(LabelId, !IO).
output_args(byte_endof_disjunct(LabelId), !IO) :-
    output_label_id(LabelId, !IO).
output_args(byte_enter_switch(Var, LabelId), !IO) :-
    output_var(Var, !IO),
    output_label_id(LabelId, !IO).
output_args(byte_endof_switch, !IO).
output_args(byte_enter_switch_arm(MainConsId, OtherConsIds, NextLabelId),
        !IO) :-
    output_cons_id(MainConsId, !IO),
    % The interpreter doesn't yet implement switch arms with more than one
    % function symbol.
    expect(unify(OtherConsIds, []), $module, $pred, "OtherConsIds"),
    output_label_id(NextLabelId, !IO).
output_args(byte_endof_switch_arm(LabelId), !IO) :-
    output_label_id(LabelId, !IO).
output_args(byte_enter_if(ElseLabelId, FollowLabelId, FramePtrTemp), !IO) :-
    output_label_id(ElseLabelId, !IO),
    output_label_id(FollowLabelId, !IO),
    output_temp(FramePtrTemp, !IO).
output_args(byte_enter_then(FramePtrTemp), !IO) :-
    output_temp(FramePtrTemp, !IO).
output_args(byte_endof_then(FollowLabelId), !IO) :-
    output_label_id(FollowLabelId, !IO).
output_args(byte_enter_else(FramePtrTemp), !IO) :-
    output_temp(FramePtrTemp, !IO).
output_args(byte_endof_if, !IO).
output_args(byte_enter_negation(FramePtrTemp, LabelId), !IO) :-
    output_temp(FramePtrTemp, !IO),
    output_label_id(LabelId, !IO).
output_args(byte_endof_negation_goal(FramePtrTemp), !IO) :-
    output_temp(FramePtrTemp, !IO).
output_args(byte_endof_negation, !IO).
output_args(byte_enter_commit(Temp), !IO) :-
    output_temp(Temp, !IO).
output_args(byte_endof_commit(Temp), !IO) :-
    output_temp(Temp, !IO).
output_args(byte_assign(Var1, Var2), !IO) :-
    output_var(Var1, !IO),
    output_var(Var2, !IO).
output_args(byte_test(Var1, Var2, TestId), !IO) :-
    output_var(Var1, !IO),
    output_var(Var2, !IO),
    output_test_id(TestId, !IO).
output_args(byte_construct(Var, ConsId, Vars), !IO) :-
    output_var(Var, !IO),
    output_cons_id(ConsId, !IO),
    list.length(Vars, Length),
    output_length(Length, !IO),
    output_vars(Vars, !IO).
output_args(byte_deconstruct(Var, ConsId, Vars), !IO) :-
    output_var(Var, !IO),
    output_cons_id(ConsId, !IO),
    list.length(Vars, Length),
    output_length(Length, !IO),
    output_vars(Vars, !IO).
output_args(byte_complex_construct(Var, ConsId, VarDirs), !IO) :-
    output_var(Var, !IO),
    output_cons_id(ConsId, !IO),
    list.length(VarDirs, Length),
    output_length(Length, !IO),
    output_var_dirs(VarDirs, !IO).
output_args(byte_complex_deconstruct(Var, ConsId, VarDirs), !IO) :-
    output_var(Var, !IO),
    output_cons_id(ConsId, !IO),
    list.length(VarDirs, Length),
    output_length(Length, !IO),
    output_var_dirs(VarDirs, !IO).
output_args(byte_place_arg(RegType, RegNum, Var), !IO) :-
    output_reg(RegType, RegNum, !IO),
    output_var(Var, !IO).
output_args(byte_pickup_arg(RegType, RegNum, Var), !IO) :-
    output_reg(RegType, RegNum, !IO),
    output_var(Var, !IO).
output_args(byte_call(ModuleId, PredId, Arity, IsFunc, ProcId), !IO) :-
    output_module_id(ModuleId, !IO),
    output_pred_id(PredId, !IO),
    output_length(Arity, !IO),
    output_is_func(IsFunc, !IO),
    output_proc_id(ProcId, !IO).
output_args(byte_higher_order_call(PredVar, InVarCount, OutVarCount, Detism),
        !IO) :-
    output_var(PredVar, !IO),
    output_length(InVarCount, !IO),
    output_length(OutVarCount, !IO),
    output_determinism(Detism, !IO).
output_args(byte_builtin_binop(Binop, Var1, Var2, Var3), !IO) :-
    output_binop(Binop, !IO),
    output_arg(Var1, !IO),
    output_arg(Var2, !IO),
    output_var(Var3, !IO).
output_args(byte_builtin_unop(Unop, Var1, Var2), !IO) :-
    output_unop(Unop, !IO),
    output_arg(Var1, !IO),
    output_var(Var2, !IO).
output_args(byte_builtin_bintest(Binop, Var1, Var2), !IO) :-
    output_binop(Binop, !IO),
    output_arg(Var1, !IO),
    output_arg(Var2, !IO).
output_args(byte_builtin_untest(Unop, Var1), !IO) :-
    output_unop(Unop, !IO),
    output_arg(Var1, !IO).
output_args(byte_semidet_succeed, !IO).
output_args(byte_semidet_success_check, !IO).
output_args(byte_fail, !IO).
output_args(byte_context(Line), !IO) :-
    output_short(Line, !IO).
output_args(byte_not_supported, !IO).

:- pred debug_args(byte_code::in, io::di, io::uo) is det.

debug_args(byte_enter_pred(PredId, PredArity, IsFunc, ProcsCount), !IO) :-
    debug_pred_id(PredId, !IO),
    debug_length(PredArity, !IO),
    debug_is_func(IsFunc, !IO),
    debug_length(ProcsCount, !IO).
debug_args(byte_endof_pred, !IO).
debug_args(byte_enter_proc(ProcId, Detism, LabelCount, LabelId, TempCount,
        Vars), !IO) :-
    debug_proc_id(ProcId, !IO),
    debug_determinism(Detism, !IO),
    debug_length(LabelCount, !IO),
    debug_label_id(LabelId, !IO),
    debug_length(TempCount, !IO),
    list.length(Vars, VarCount),
    debug_length(VarCount, !IO),
    debug_var_infos(Vars, !IO).
debug_args(byte_endof_proc, !IO).
debug_args(byte_label(LabelId), !IO) :-
    debug_label_id(LabelId, !IO).
debug_args(byte_enter_disjunction(LabelId), !IO) :-
    debug_label_id(LabelId, !IO).
debug_args(byte_endof_disjunction, !IO).
debug_args(byte_enter_disjunct(LabelId), !IO) :-
    debug_label_id(LabelId, !IO).
debug_args(byte_endof_disjunct(LabelId), !IO) :-
    debug_label_id(LabelId, !IO).
debug_args(byte_enter_switch(Var, LabelId), !IO) :-
    debug_var(Var, !IO),
    debug_label_id(LabelId, !IO).
debug_args(byte_endof_switch, !IO).
debug_args(byte_enter_switch_arm(MainConsId, OtherConsIds,
        NextLabelId), !IO) :-
    debug_cons_id(MainConsId, !IO),
    list.foldl(debug_cons_id, OtherConsIds, !IO),
    debug_label_id(NextLabelId, !IO).
debug_args(byte_endof_switch_arm(LabelId), !IO) :-
    debug_label_id(LabelId, !IO).
debug_args(byte_enter_if(ElseLabelId, FollowLabelId, FramePtrTemp), !IO) :-
    debug_label_id(ElseLabelId, !IO),
    debug_label_id(FollowLabelId, !IO),
    debug_temp(FramePtrTemp, !IO).
debug_args(byte_enter_then(FramePtrTemp), !IO) :-
    debug_temp(FramePtrTemp, !IO).
debug_args(byte_endof_then(FollowLabelId), !IO) :-
    debug_label_id(FollowLabelId, !IO).
debug_args(byte_enter_else(FramePtrTemp), !IO) :-
    debug_temp(FramePtrTemp, !IO).
debug_args(byte_endof_if, !IO).
debug_args(byte_enter_negation(FramePtrTemp, LabelId), !IO) :-
    debug_temp(FramePtrTemp, !IO),
    debug_label_id(LabelId, !IO).
debug_args(byte_endof_negation_goal(FramePtrTemp), !IO) :-
    debug_temp(FramePtrTemp, !IO).
debug_args(byte_endof_negation, !IO).
debug_args(byte_enter_commit(Temp), !IO) :-
    debug_temp(Temp, !IO).
debug_args(byte_endof_commit(Temp), !IO) :-
    debug_temp(Temp, !IO).
debug_args(byte_assign(Var1, Var2), !IO) :-
    debug_var(Var1, !IO),
    debug_var(Var2, !IO).
debug_args(byte_test(Var1, Var2, TestId), !IO) :-
    debug_var(Var1, !IO),
    debug_var(Var2, !IO),
    debug_test_id(TestId, !IO).
debug_args(byte_construct(Var, ConsId, Vars), !IO) :-
    debug_var(Var, !IO),
    debug_cons_id(ConsId, !IO),
    list.length(Vars, Length),
    debug_length(Length, !IO),
    debug_vars(Vars, !IO).
debug_args(byte_deconstruct(Var, ConsId, Vars), !IO) :-
    debug_var(Var, !IO),
    debug_cons_id(ConsId, !IO),
    list.length(Vars, Length),
    debug_length(Length, !IO),
    debug_vars(Vars, !IO).
debug_args(byte_complex_construct(Var, ConsId, VarDirs), !IO) :-
    debug_var(Var, !IO),
    debug_cons_id(ConsId, !IO),
    list.length(VarDirs, Length),
    debug_length(Length, !IO),
    debug_var_dirs(VarDirs, !IO).
debug_args(byte_complex_deconstruct(Var, ConsId, VarDirs), !IO) :-
    debug_var(Var, !IO),
    debug_cons_id(ConsId, !IO),
    list.length(VarDirs, Length),
    debug_length(Length, !IO),
    debug_var_dirs(VarDirs, !IO).
debug_args(byte_place_arg(RegType, RegNum, Var), !IO) :-
    debug_reg(RegType, RegNum, !IO),
    debug_var(Var, !IO).
debug_args(byte_pickup_arg(RegType, RegNum, Var), !IO) :-
    debug_reg(RegType, RegNum, !IO),
    debug_var(Var, !IO).
debug_args(byte_call(ModuleId, PredId, Arity, IsFunc, ProcId), !IO) :-
    debug_module_id(ModuleId, !IO),
    debug_pred_id(PredId, !IO),
    debug_length(Arity, !IO),
    debug_is_func(IsFunc, !IO),
    debug_proc_id(ProcId, !IO).
debug_args(byte_higher_order_call(PredVar, InVarCount, OutVarCount, Detism),
        !IO) :-
    debug_var(PredVar, !IO),
    debug_length(InVarCount, !IO),
    debug_length(OutVarCount, !IO),
    debug_determinism(Detism, !IO).
debug_args(byte_builtin_binop(Binop, Var1, Var2, Var3), !IO) :-
    debug_binop(Binop, !IO),
    debug_arg(Var1, !IO),
    debug_arg(Var2, !IO),
    debug_var(Var3, !IO).
debug_args(byte_builtin_unop(Unop, Var1, Var2), !IO) :-
    debug_unop(Unop, !IO),
    debug_arg(Var1, !IO),
    debug_var(Var2, !IO).
debug_args(byte_builtin_bintest(Binop, Var1, Var2), !IO) :-
    debug_binop(Binop, !IO),
    debug_arg(Var1, !IO),
    debug_arg(Var2, !IO).
debug_args(byte_builtin_untest(Unop, Var1), !IO) :-
    debug_unop(Unop, !IO),
    debug_arg(Var1, !IO).
debug_args(byte_semidet_succeed, !IO).
debug_args(byte_semidet_success_check, !IO).
debug_args(byte_fail, !IO).
debug_args(byte_context(Line), !IO) :-
    debug_int(Line, !IO).
debug_args(byte_not_supported, !IO).

%---------------------------------------------------------------------------%

:- pred output_var_infos(list(byte_var_info)::in, io::di, io::uo) is det.

output_var_infos([], !IO).
output_var_infos([Var | Vars], !IO) :-
    output_var_info(Var, !IO),
    output_var_infos(Vars, !IO).

:- pred output_var_info(byte_var_info::in, io::di, io::uo) is det.

output_var_info(var_info(Name, _), !IO) :-
    output_string(Name, !IO).

:- pred debug_var_infos(list(byte_var_info)::in, io::di, io::uo) is det.

debug_var_infos([], !IO).
debug_var_infos([Var | Vars], !IO) :-
    debug_var_info(Var, !IO),
    debug_var_infos(Vars, !IO).

:- pred debug_var_info(byte_var_info::in, io::di, io::uo) is det.

debug_var_info(var_info(Name, _), !IO) :-
    debug_string(Name, !IO).

%---------------------------------------------------------------------------%

:- pred output_determinism(determinism::in, io::di, io::uo) is det.

output_determinism(Detism, !IO) :-
    determinism_code(Detism, Code),
    output_byte(Code, !IO).

:- pred debug_determinism(determinism::in, io::di, io::uo) is det.

debug_determinism(Detism, !IO) :-
    determinism_debug(Detism, Debug),
    debug_string(Debug, !IO).

%---------------------------------------------------------------------------%

:- pred output_reg(byte_reg_type::in, int::in, io::di, io::uo) is det.

output_reg(byte_reg_r, N, !IO) :-
    output_byte(N, !IO).

:- pred debug_reg(byte_reg_type::in, int::in, io::di, io::uo) is det.

debug_reg(byte_reg_r, N, !IO) :-
    debug_int(N, !IO).

%---------------------------------------------------------------------------%

:- pred output_is_func(byte_is_func::in, io::di, io::uo) is det.

output_is_func(IsFunc, !IO) :-
    ( ( IsFunc = 1 ; IsFunc = 0 ) ->
        output_byte(IsFunc, !IO)
    ;
        unexpected($module, $pred,
            "invalid predicate or function specified in bytecode")
    ).

:- pred debug_is_func(byte_is_func::in, io::di, io::uo) is det.

debug_is_func(IsFunc, !IO) :-
    ( IsFunc = 1 ->
        debug_string("func", !IO)
    ; IsFunc = 0 ->
        debug_string("pred", !IO)
    ;
        unexpected($module, $pred,
            "invalid predicate or function specifier in bytecode.")
    ).

%---------------------------------------------------------------------------%

:- pred output_length(int::in, io::di, io::uo) is det.

output_length(Length, !IO) :-
    output_short(Length, !IO).

:- pred debug_length(int::in, io::di, io::uo) is det.

debug_length(Length, !IO) :-
    debug_int(Length, !IO).

%---------------------------------------------------------------------------%

:- pred output_arg(byte_arg::in, io::di, io::uo) is det.

output_arg(byte_arg_var(Var), !IO) :-
    output_byte(0, !IO),
    output_var(Var, !IO).
output_arg(byte_arg_int_const(IntVal), !IO) :-
    output_byte(1, !IO),
    output_int(IntVal, !IO).
output_arg(byte_arg_float_const(FloatVal), !IO) :-
    output_byte(2, !IO),
    output_float(FloatVal, !IO).

:- pred debug_arg(byte_arg::in, io::di, io::uo) is det.

debug_arg(byte_arg_var(Var), !IO) :-
    debug_string("var", !IO),
    debug_var(Var, !IO).
debug_arg(byte_arg_int_const(IntVal), !IO) :-
    debug_string("int", !IO),
    debug_int(IntVal, !IO).
debug_arg(byte_arg_float_const(FloatVal), !IO) :-
    debug_string("float", !IO),
    debug_float(FloatVal, !IO).

%---------------------------------------------------------------------------%

:- pred output_var(byte_var::in, io::di, io::uo) is det.

output_var(Var, !IO) :-
    output_short(Var, !IO).

:- pred output_vars(list(byte_var)::in, io::di, io::uo) is det.

output_vars([], !IO).
output_vars([Var | Vars], !IO) :-
    output_var(Var, !IO),
    output_vars(Vars, !IO).

:- pred debug_var(byte_var::in, io::di, io::uo) is det.

debug_var(Var, !IO) :-
    debug_int(Var, !IO).

:- pred debug_vars(list(byte_var)::in, io::di, io::uo) is det.

debug_vars([], !IO).
debug_vars([Var | Vars], !IO) :-
    debug_var(Var, !IO),
    debug_vars(Vars, !IO).

%---------------------------------------------------------------------------%

:- pred output_temp(byte_temp::in, io::di, io::uo) is det.

output_temp(Var, !IO) :-
    output_short(Var, !IO).

:- pred debug_temp(byte_temp::in, io::di, io::uo) is det.

debug_temp(Var, !IO) :-
    debug_int(Var, !IO).

%---------------------------------------------------------------------------%

:- pred output_dir(byte_dir::in, io::di, io::uo) is det.

output_dir(to_arg, !IO) :-
    output_byte(0, !IO).
output_dir(to_var, !IO) :-
    output_byte(1, !IO).
output_dir(to_none, !IO) :-
    output_byte(2, !IO).

:- pred output_var_dirs(assoc_list(byte_var, byte_dir)::in,
    io::di, io::uo) is det.

output_var_dirs([], !IO).
output_var_dirs([Var - Dir | VarDirs], !IO) :-
    output_var(Var, !IO),
    output_dir(Dir, !IO),
    output_var_dirs(VarDirs, !IO).

:- pred debug_dir(byte_dir::in, io::di, io::uo) is det.

debug_dir(to_arg, !IO) :-
    debug_string("to_arg", !IO).
debug_dir(to_var, !IO) :-
    debug_string("to_var", !IO).
debug_dir(to_none, !IO) :-
    debug_string("to_none", !IO).

:- pred debug_var_dirs(assoc_list(byte_var, byte_dir)::in,
    io::di, io::uo) is det.

debug_var_dirs([], !IO).
debug_var_dirs([Var - Dir | VarDirs], !IO) :-
    debug_var(Var, !IO),
    debug_dir(Dir, !IO),
    debug_var_dirs(VarDirs, !IO).

%---------------------------------------------------------------------------%

:- pred output_test_id(byte_test_id::in, io::di, io::uo) is det.

output_test_id(int_test, !IO)    :- output_byte(0, !IO).
output_test_id(char_test, !IO)   :- output_byte(1, !IO).
output_test_id(string_test, !IO) :- output_byte(2, !IO).
output_test_id(float_test, !IO)  :- output_byte(3, !IO).
output_test_id(enum_test, !IO)   :- output_byte(4, !IO).
output_test_id(dummy_test, !IO)  :- output_byte(5, !IO).

:- pred debug_test_id(byte_test_id::in, io::di, io::uo) is det.

debug_test_id(int_test, !IO)     :- debug_string("int", !IO).
debug_test_id(char_test, !IO)    :- debug_string("char", !IO).
debug_test_id(string_test, !IO)  :- debug_string("string", !IO).
debug_test_id(float_test, !IO)   :- debug_string("float", !IO).
debug_test_id(enum_test, !IO)    :- debug_string("enum", !IO).
debug_test_id(dummy_test, !IO)   :- debug_string("dummy", !IO).

%---------------------------------------------------------------------------%

:- pred output_module_id(byte_module_id::in, io::di, io::uo) is det.

output_module_id(ModuleId, !IO) :-
    output_string(sym_name_to_string(ModuleId), !IO).

:- pred debug_module_id(byte_module_id::in, io::di, io::uo) is det.

debug_module_id(ModuleId, !IO) :-
    debug_sym_name(ModuleId, !IO).

%---------------------------------------------------------------------------%

:- pred output_pred_id(byte_pred_id::in, io::di, io::uo) is det.

output_pred_id(PredId, !IO) :-
    output_string(PredId, !IO).

:- pred debug_pred_id(byte_pred_id::in, io::di, io::uo) is det.

debug_pred_id(PredId, !IO) :-
    debug_string(PredId, !IO).

%---------------------------------------------------------------------------%

:- pred output_proc_id(byte_proc_id::in, io::di, io::uo) is det.

output_proc_id(ProcId, !IO) :-
    output_byte(ProcId, !IO).

:- pred debug_proc_id(byte_proc_id::in, io::di, io::uo) is det.

debug_proc_id(ProcId, !IO) :-
    debug_int(ProcId, !IO).

%---------------------------------------------------------------------------%

:- pred output_label_id(int::in, io::di, io::uo) is det.

output_label_id(LabelId, !IO) :-
    output_short(LabelId, !IO).

:- pred debug_label_id(int::in, io::di, io::uo) is det.

debug_label_id(LabelId, !IO) :-
    debug_int(LabelId, !IO).

%---------------------------------------------------------------------------%

:- pred output_cons_id(byte_cons_id::in, io::di, io::uo) is det.

output_cons_id(byte_cons(ModuleId, Functor, Arity, Tag), !IO) :-
    output_byte(0, !IO),
    output_module_id(ModuleId, !IO),
    output_string(Functor, !IO),
    output_short(Arity, !IO),
    output_tag(Tag, !IO).
output_cons_id(byte_int_const(IntVal), !IO) :-
    output_byte(1, !IO),
    output_int(IntVal, !IO).
output_cons_id(byte_string_const(StringVal), !IO) :-
    output_byte(2, !IO),
    output_string(StringVal, !IO).
output_cons_id(byte_float_const(FloatVal), !IO) :-
    output_byte(3, !IO),
    output_float(FloatVal, !IO).
output_cons_id(byte_pred_const(ModuleId, PredId, Arity, IsFunc, ProcId),
        !IO) :-
    output_byte(4, !IO),
    output_module_id(ModuleId, !IO),
    output_pred_id(PredId, !IO),
    output_length(Arity, !IO),
    output_is_func(IsFunc, !IO),
    output_proc_id(ProcId, !IO).
output_cons_id(byte_type_ctor_info_const(ModuleId, TypeName, TypeArity),
        !IO) :-
    output_byte(6, !IO),
    output_module_id(ModuleId, !IO),
    output_string(TypeName, !IO),
    output_byte(TypeArity, !IO).
output_cons_id(byte_char_const(Char), !IO) :-
    output_byte(7, !IO),
    char.to_int(Char, Byte),
    output_byte(Byte, !IO).
    % XXX
output_cons_id(byte_base_typeclass_info_const(_, _, _), !IO) :-
    sorry($module, $pred, "bytecode for typeclass not yet implemented."),
    output_byte(8, !IO).
output_cons_id(byte_type_info_cell_constructor, !IO) :-
    sorry($module, $pred, "bytecode for type_info_cell_constructor " ++
        "not yet implemented."),
    output_byte(9, !IO).
output_cons_id(byte_typeclass_info_cell_constructor, !IO) :-
    sorry($module, $pred, "bytecode for typeclass_info_cell_constructor " ++
        "not yet implemented."),
    output_byte(10, !IO).

:- pred debug_cons_id(byte_cons_id::in, io::di, io::uo) is det.

debug_cons_id(byte_cons(ModuleId, Functor, Arity, Tag), !IO) :-
    debug_string("functor", !IO),
    debug_sym_name(ModuleId, !IO),
    debug_string(Functor, !IO),
    debug_int(Arity, !IO),
    debug_tag(Tag, !IO).
debug_cons_id(byte_int_const(IntVal), !IO) :-
    debug_string("int_const", !IO),
    debug_int(IntVal, !IO).
debug_cons_id(byte_string_const(StringVal), !IO) :-
    debug_string("string_const", !IO),
    debug_cstring(StringVal, !IO).
debug_cons_id(byte_float_const(FloatVal), !IO) :-
    debug_string("float_const", !IO),
    debug_float(FloatVal, !IO).
debug_cons_id(byte_pred_const(ModuleId, PredId, Arity, IsFunc, ProcId), !IO) :-
    debug_string("pred_const", !IO),
    debug_module_id(ModuleId, !IO),
    debug_pred_id(PredId, !IO),
    debug_length(Arity, !IO),
    debug_is_func(IsFunc, !IO),
    debug_proc_id(ProcId, !IO).
debug_cons_id(byte_type_ctor_info_const(ModuleId, TypeName, TypeArity), !IO) :-
    debug_string("type_ctor_info_const", !IO),
    debug_module_id(ModuleId, !IO),
    debug_string(TypeName, !IO),
    debug_int(TypeArity, !IO).
debug_cons_id(byte_base_typeclass_info_const(ModuleId,
        class_id(ClassName, ClassArity), Instance), !IO) :-
    debug_string("base_typeclass_info_const", !IO),
    debug_module_id(ModuleId, !IO),
    debug_string("class_id", !IO),
    debug_sym_name(ClassName, !IO),
    debug_string("/", !IO),
    debug_int(ClassArity, !IO),
    debug_string(Instance, !IO).
debug_cons_id(byte_char_const(Char), !IO) :-
    debug_string("char_const", !IO),
    string.from_char_list([Char], String),
    debug_string(String, !IO).
debug_cons_id(byte_type_info_cell_constructor, !IO) :-
    debug_string("type_info_cell_constructor", !IO).
debug_cons_id(byte_typeclass_info_cell_constructor, !IO) :-
    debug_string("typeclass_info_cell_constructor", !IO).

%---------------------------------------------------------------------------%

:- pred output_tag(byte_cons_tag::in, io::di, io::uo) is det.

output_tag(byte_unshared_tag(Primary), !IO) :-
    output_byte(0, !IO),
    output_byte(Primary, !IO).
output_tag(byte_shared_remote_tag(Primary, Secondary), !IO) :-
    output_byte(1, !IO),
    output_byte(Primary, !IO),
    output_int(Secondary, !IO).
output_tag(byte_shared_local_tag(Primary, Secondary), !IO) :-
    output_byte(2, !IO),
    output_byte(Primary, !IO),
    output_int(Secondary, !IO).
output_tag(byte_enum_tag(Enum), !IO) :-
    output_byte(3, !IO),
    output_byte(Enum, !IO).
output_tag(byte_no_tag, !IO) :-
    output_byte(4, !IO).

:- pred debug_tag(byte_cons_tag::in, io::di, io::uo) is det.

debug_tag(byte_unshared_tag(Primary), !IO) :-
    debug_string("unshared_tag", !IO),
    debug_int(Primary, !IO).
debug_tag(byte_shared_remote_tag(Primary, Secondary), !IO) :-
    debug_string("shared_remote_tag", !IO),
    debug_int(Primary, !IO),
    debug_int(Secondary, !IO).
debug_tag(byte_shared_local_tag(Primary, Secondary), !IO) :-
    debug_string("shared_local_tag", !IO),
    debug_int(Primary, !IO),
    debug_int(Secondary, !IO).
debug_tag(byte_enum_tag(Enum), !IO) :-
    debug_string("enum_tag", !IO),
    debug_int(Enum, !IO).
debug_tag(byte_no_tag, !IO) :-
    debug_string("no_tag", !IO).

%---------------------------------------------------------------------------%

:- pred output_binop(binary_op::in, io::di, io::uo) is det.

output_binop(Binop, !IO) :-
    binop_code(Binop, Code),
    output_byte(Code, !IO).

:- pred debug_binop(binary_op::in, io::di, io::uo) is det.

debug_binop(Binop, !IO) :-
    binop_debug(Binop, Debug),
    debug_string(Debug, !IO).

%---------------------------------------------------------------------------%

:- pred output_unop(unary_op::in, io::di, io::uo) is det.

output_unop(Unop, !IO) :-
    unop_code(Unop, Code),
    output_byte(Code, !IO).

:- pred debug_unop(unary_op::in, io::di, io::uo) is det.

debug_unop(Unop, !IO) :-
    unop_debug(Unop, Debug),
    debug_string(Debug, !IO).

%---------------------------------------------------------------------------%

:- pred byte_code(byte_code::in, int::out) is det.

byte_code(byte_enter_pred(_, _, _, _),            0).
byte_code(byte_endof_pred,                        1).
byte_code(byte_enter_proc(_, _, _, _, _, _),      2).
byte_code(byte_endof_proc,                        3).
byte_code(byte_label(_),                          4).
byte_code(byte_enter_disjunction(_),              5).
byte_code(byte_endof_disjunction,                 6).
byte_code(byte_enter_disjunct(_),                 7).
byte_code(byte_endof_disjunct(_),                 8).
byte_code(byte_enter_switch(_, _),                9).
byte_code(byte_endof_switch,                     10).
byte_code(byte_enter_switch_arm(_, _, _),        11).
byte_code(byte_endof_switch_arm(_),              12).
byte_code(byte_enter_if(_, _, _),                13).
byte_code(byte_enter_then(_),                    14).
byte_code(byte_endof_then(_),                    15).
byte_code(byte_endof_if,                         16).
byte_code(byte_enter_negation(_, _),             17).
byte_code(byte_endof_negation,                   18).
byte_code(byte_enter_commit(_),                  19).
byte_code(byte_endof_commit(_),                  20).
byte_code(byte_assign(_, _),                     21).
byte_code(byte_test(_, _, _),                    22).
byte_code(byte_construct(_, _, _),               23).
byte_code(byte_deconstruct(_, _, _),             24).
byte_code(byte_complex_construct(_, _, _),       25).
byte_code(byte_complex_deconstruct(_, _, _),     26).
byte_code(byte_place_arg(_, _, _),               27).
byte_code(byte_pickup_arg(_, _, _),              28).
byte_code(byte_call(_, _, _, _, _),              29).
byte_code(byte_higher_order_call(_, _, _, _),    30).
byte_code(byte_builtin_binop(_, _, _, _),        31).
byte_code(byte_builtin_unop(_, _, _),            32).
byte_code(byte_builtin_bintest(_, _, _),         33).
byte_code(byte_builtin_untest(_, _),             34).
byte_code(byte_semidet_succeed,                  35).
byte_code(byte_semidet_success_check,            36).
byte_code(byte_fail,                             37).
byte_code(byte_context(_),                       38).
byte_code(byte_not_supported,                    39).
byte_code(byte_enter_else(_),                    40).
byte_code(byte_endof_negation_goal(_),           41).

:- pred byte_debug(byte_code::in, string::out) is det.

byte_debug(byte_enter_pred(_, _, _, _),          "enter_pred").
byte_debug(byte_endof_pred,                      "endof_pred").
byte_debug(byte_enter_proc(_, _, _, _, _, _),    "enter_proc").
byte_debug(byte_endof_proc,                      "endof_proc").
byte_debug(byte_label(_),                        "label").
byte_debug(byte_enter_disjunction(_),            "enter_disjunction").
byte_debug(byte_endof_disjunction,               "endof_disjunction").
byte_debug(byte_enter_disjunct(_),               "enter_disjunct").
byte_debug(byte_endof_disjunct(_),               "endof_disjunct").
byte_debug(byte_enter_switch(_, _),              "enter_switch").
byte_debug(byte_endof_switch,                    "endof_switch").
byte_debug(byte_enter_switch_arm(_, _, _),       "enter_switch_arm").
byte_debug(byte_endof_switch_arm(_),             "endof_switch_arm").
byte_debug(byte_enter_if(_, _, _),               "enter_if").
byte_debug(byte_enter_then(_),                   "enter_then").
byte_debug(byte_endof_then(_),                   "endof_then").
byte_debug(byte_enter_else(_),                   "enter_else").
byte_debug(byte_endof_if,                        "endof_if").
byte_debug(byte_enter_negation(_,_),             "enter_negation").
byte_debug(byte_endof_negation_goal(_),          "endof_negation_goal").
byte_debug(byte_endof_negation,                  "endof_negation").
byte_debug(byte_enter_commit(_),                 "enter_commit").
byte_debug(byte_endof_commit(_),                 "endof_commit").
byte_debug(byte_assign(_, _),                    "assign").
byte_debug(byte_test(_, _, _),                   "test").
byte_debug(byte_construct(_, _, _),              "construct").
byte_debug(byte_deconstruct(_, _, _),            "deconstruct").
byte_debug(byte_complex_construct(_, _, _),      "complex_construct").
byte_debug(byte_complex_deconstruct(_, _, _),    "complex_deconstruct").
byte_debug(byte_place_arg(_, _, _),              "place_arg").
byte_debug(byte_pickup_arg(_, _, _),             "pickup_arg").
byte_debug(byte_call(_, _, _, _, _),             "call").
byte_debug(byte_higher_order_call(_, _, _, _),   "higher_order_call").
byte_debug(byte_builtin_binop(_, _, _, _),       "builtin_binop").
byte_debug(byte_builtin_unop(_, _, _),           "builtin_unop").
byte_debug(byte_builtin_bintest(_, _, _),        "builtin_bintest").
byte_debug(byte_builtin_untest(_, _),            "builtin_untest").
byte_debug(byte_semidet_succeed,                 "semidet_succeed").
byte_debug(byte_semidet_success_check,           "semidet_success_check").
byte_debug(byte_fail,                            "fail").
byte_debug(byte_context(_),                      "context").
byte_debug(byte_not_supported,                   "not_supported").

:- pred determinism_code(determinism::in, int::out) is det.

determinism_code(detism_det,           0).
determinism_code(detism_semi,          1).
determinism_code(detism_multi,         2).
determinism_code(detism_non,           3).
determinism_code(detism_cc_multi,      4).
determinism_code(detism_cc_non,        5).
determinism_code(detism_erroneous,     6).
determinism_code(detism_failure,       7).

:- pred determinism_debug(determinism::in, string::out) is det.

determinism_debug(detism_det,          "det").
determinism_debug(detism_semi,         "semidet").
determinism_debug(detism_multi,        "multidet").
determinism_debug(detism_non,          "nondet").
determinism_debug(detism_cc_multi,     "cc_multidet").
determinism_debug(detism_cc_non,       "cc_nondet").
determinism_debug(detism_erroneous,    "erroneous").
determinism_debug(detism_failure,      "failure").

:- pred binop_code(binary_op::in, int::out) is det.

binop_code(int_add(int_type_int),    0).
binop_code(int_sub(int_type_int),    1).
binop_code(int_mul(int_type_int),    2).
binop_code(int_div(int_type_int),    3).
binop_code(int_mod(int_type_int),    4).
binop_code(unchecked_left_shift(int_type_int),  5).
binop_code(unchecked_right_shift(int_type_int), 6).
binop_code(bitwise_and(int_type_int), 7).
binop_code(bitwise_or(int_type_int),  8).
binop_code(bitwise_xor(int_type_int), 9).
binop_code(logical_and,             10).
binop_code(logical_or,              11).
binop_code(eq(int_type_int),        12).
binop_code(ne(int_type_int),        13).
binop_code(array_index(_Type),      14).
binop_code(str_eq,                  15).
binop_code(str_ne,                  16).
binop_code(str_lt,                  17).
binop_code(str_gt,                  18).
binop_code(str_le,                  19).
binop_code(str_ge,                  20).
binop_code(int_lt(int_type_int),    21).
binop_code(int_gt(int_type_int),    22).
binop_code(int_le(int_type_int),    23).
binop_code(int_ge(int_type_int),    24).
binop_code(float_plus,              25).
binop_code(float_minus,             26).
binop_code(float_times,             27).
binop_code(float_divide,            28).
binop_code(float_eq,                29).
binop_code(float_ne,                30).
binop_code(float_lt,                31).
binop_code(float_gt,                32).
binop_code(float_le,                33).
binop_code(float_ge,                34).
binop_code(body,                    35).
binop_code(unsigned_le,             36).
binop_code(compound_eq,             37).
binop_code(compound_lt,             38).
binop_code(str_cmp,                 39).
binop_code(float_word_bits,         40).
binop_code(float_from_dword,        41).
binop_code(pointer_equal_conservative, 42).
binop_code(offset_str_eq(_),        43).
binop_code(string_unsafe_index_code_unit, 44).
binop_code(eq(int_type_uint),       45).
binop_code(ne(int_type_uint),       46).
binop_code(int_lt(int_type_uint),   47).
binop_code(int_gt(int_type_uint),   48).
binop_code(int_le(int_type_uint),   49).
binop_code(int_ge(int_type_uint),   50).
binop_code(int_add(int_type_uint),  51).
binop_code(int_sub(int_type_uint),  52).
binop_code(int_mul(int_type_uint),  53).
binop_code(int_div(int_type_uint),                54).
binop_code(int_mod(int_type_uint),                55).
binop_code(bitwise_and(int_type_uint),  56).
binop_code(bitwise_or(int_type_uint), 57).
binop_code(bitwise_xor(int_type_uint), 58).
binop_code(unchecked_left_shift(int_type_uint), 59).
binop_code(unchecked_right_shift(int_type_uint), 60).
binop_code(eq(int_type_int8),       61).
binop_code(ne(int_type_int8),       62).
binop_code(int_lt(int_type_int8),   63).
binop_code(int_gt(int_type_int8),   64).
binop_code(int_le(int_type_int8),   65).
binop_code(int_ge(int_type_int8),   66).
binop_code(int_add(int_type_int8),  67).
binop_code(int_sub(int_type_int8),  68).
binop_code(int_mul(int_type_int8),  69).
binop_code(int_div(int_type_int8),  70).
binop_code(int_mod(int_type_int8),  71).
binop_code(bitwise_and(int_type_int8), 72).
binop_code(bitwise_or(int_type_int8), 73).
binop_code(bitwise_xor(int_type_int8), 74).
binop_code(unchecked_left_shift(int_type_int8), 75).
binop_code(unchecked_right_shift(int_type_int8), 76).
binop_code(eq(int_type_uint8),       77).
binop_code(ne(int_type_uint8),       78).
binop_code(int_lt(int_type_uint8),   79).
binop_code(int_gt(int_type_uint8),   80).
binop_code(int_le(int_type_uint8),   81).
binop_code(int_ge(int_type_uint8),   82).
binop_code(int_add(int_type_uint8),  83).
binop_code(int_sub(int_type_uint8),  84).
binop_code(int_mul(int_type_uint8),  85).
binop_code(int_div(int_type_uint8),  86).
binop_code(int_mod(int_type_uint8),  87).
binop_code(bitwise_and(int_type_uint8), 88).
binop_code(bitwise_or(int_type_uint8),  89).
binop_code(bitwise_xor(int_type_uint8), 90).
binop_code(unchecked_left_shift(int_type_uint8), 91).
binop_code(unchecked_right_shift(int_type_uint8), 92).
binop_code(eq(int_type_int16),       93).
binop_code(ne(int_type_int16),       94).
binop_code(int_lt(int_type_int16),   95).
binop_code(int_gt(int_type_int16),   96).
binop_code(int_le(int_type_int16),   97).
binop_code(int_ge(int_type_int16),   98).
binop_code(int_add(int_type_int16),  99).
binop_code(int_sub(int_type_int16), 100).
binop_code(int_mul(int_type_int16), 101).
binop_code(int_div(int_type_int16), 102).
binop_code(int_mod(int_type_int16), 103).
binop_code(bitwise_and(int_type_int16), 104).
binop_code(bitwise_or(int_type_int16),  105).
binop_code(bitwise_xor(int_type_int16), 106).
binop_code(unchecked_left_shift(int_type_int16),  107).
binop_code(unchecked_right_shift(int_type_int16), 108).
binop_code(eq(int_type_uint16),       109).
binop_code(ne(int_type_uint16),       110).
binop_code(int_lt(int_type_uint16),   111).
binop_code(int_gt(int_type_uint16),   112).
binop_code(int_le(int_type_uint16),   113).
binop_code(int_ge(int_type_uint16),   114).
binop_code(int_add(int_type_uint16),  115).
binop_code(int_sub(int_type_uint16),  116).
binop_code(int_mul(int_type_uint16),  117).
binop_code(int_div(int_type_uint16),  118).
binop_code(int_mod(int_type_uint16),  119).
binop_code(bitwise_and(int_type_uint16), 120).
binop_code(bitwise_or(int_type_uint16),  121).
binop_code(bitwise_xor(int_type_uint16), 122).
binop_code(unchecked_left_shift(int_type_uint16), 123).
binop_code(unchecked_right_shift(int_type_uint16), 124).
binop_code(eq(int_type_int32),       125).
binop_code(ne(int_type_int32),       126).
binop_code(int_lt(int_type_int32),   127).
binop_code(int_gt(int_type_int32),   128).
binop_code(int_le(int_type_int32),   129).
binop_code(int_ge(int_type_int32),   130).
binop_code(int_add(int_type_int32),  131).
binop_code(int_sub(int_type_int32),  132).
binop_code(int_mul(int_type_int32),  133).
binop_code(int_div(int_type_int32),  134).
binop_code(int_mod(int_type_int32),                135).
binop_code(bitwise_and(int_type_int32), 136).
binop_code(bitwise_or(int_type_int32),  137).
binop_code(bitwise_xor(int_type_int32), 138).
binop_code(unchecked_left_shift(int_type_int32), 139).
binop_code(unchecked_right_shift(int_type_int32), 140).
binop_code(eq(int_type_uint32),       141).
binop_code(ne(int_type_uint32),       142).
binop_code(int_lt(int_type_uint32),   143).
binop_code(int_gt(int_type_uint32),   144).
binop_code(int_le(int_type_uint32),   145).
binop_code(int_ge(int_type_uint32),   146).
binop_code(int_add(int_type_uint32),  147).
binop_code(int_sub(int_type_uint32),  148).
binop_code(int_mul(int_type_uint32),  149).
binop_code(int_div(int_type_uint32),  150).
binop_code(int_mod(int_type_uint32),  151).
binop_code(bitwise_and(int_type_uint32), 152).
binop_code(bitwise_or(int_type_uint32),  153).
binop_code(bitwise_xor(int_type_uint32), 154).
binop_code(unchecked_left_shift(int_type_uint32),  155).
binop_code(unchecked_right_shift(int_type_uint32), 156).
binop_code(eq(int_type_int64),       157).
binop_code(ne(int_type_int64),       158).
binop_code(int_lt(int_type_int64),   159).
binop_code(int_gt(int_type_int64),   160).
binop_code(int_le(int_type_int64),   161).
binop_code(int_ge(int_type_int64),   162).
binop_code(int_add(int_type_int64),  163).
binop_code(int_sub(int_type_int64),  164).
binop_code(int_mul(int_type_int64),  165).
binop_code(int_div(int_type_int64),  166).
binop_code(int_mod(int_type_int64),  167).
binop_code(bitwise_and(int_type_int64), 168).
binop_code(bitwise_or(int_type_int64),  169).
binop_code(bitwise_xor(int_type_int64), 170).
binop_code(unchecked_left_shift(int_type_int64), 171).
binop_code(unchecked_right_shift(int_type_int64), 172).
binop_code(eq(int_type_uint64),       173).
binop_code(ne(int_type_uint64),       174).
binop_code(int_lt(int_type_uint64),   175).
binop_code(int_gt(int_type_uint64),   176).
binop_code(int_le(int_type_uint64),   177).
binop_code(int_ge(int_type_uint64),   178).
binop_code(int_add(int_type_uint64),  179).
binop_code(int_sub(int_type_uint64),  180).
binop_code(int_mul(int_type_uint64),  181).
binop_code(int_div(int_type_uint64),  182).
binop_code(int_mod(int_type_uint64),  183).
binop_code(bitwise_and(int_type_uint64), 184).
binop_code(bitwise_or(int_type_uint64),  185).
binop_code(bitwise_xor(int_type_uint64), 186).
binop_code(unchecked_left_shift(int_type_uint64),  187).
binop_code(unchecked_right_shift(int_type_uint64), 188).

:- pred binop_debug(binary_op::in, string::out) is det.

binop_debug(int_add(int_type_int),  "+").
binop_debug(int_sub(int_type_int),  "-").
binop_debug(int_mul(int_type_int),  "*").
binop_debug(int_div(int_type_int),  "/").
binop_debug(int_mod(int_type_int),  "mod").
binop_debug(unchecked_left_shift(int_type_int),   "<<").
binop_debug(unchecked_right_shift(int_type_int),  ">>").
binop_debug(bitwise_and(int_type_int), "&").
binop_debug(bitwise_or(int_type_int), "|").
binop_debug(bitwise_xor(int_type_int), "^").
binop_debug(logical_and,            "and").
binop_debug(logical_or,             "or").
binop_debug(eq(int_type_int),       "eq").
binop_debug(ne(int_type_int),       "ne").
binop_debug(array_index(_Type),     "array_index").
binop_debug(str_eq,                 "str_eq").
binop_debug(str_ne,                 "str_ne").
binop_debug(str_lt,                 "str_lt").
binop_debug(str_gt,                 "str_gt").
binop_debug(str_le,                 "str_le").
binop_debug(str_ge,                 "str_ge").
binop_debug(int_lt(int_type_int),   "<").
binop_debug(int_gt(int_type_int),   ">").
binop_debug(int_le(int_type_int),   "<=").
binop_debug(int_ge(int_type_int),   ">=").
binop_debug(float_plus,             "float_plus").
binop_debug(float_minus,            "float_minus").
binop_debug(float_times,            "float_times").
binop_debug(float_divide,           "float_divide").
binop_debug(float_eq,               "float_eq").
binop_debug(float_ne,               "float_ne").
binop_debug(float_lt,               "float_lt").
binop_debug(float_gt,               "float_gt").
binop_debug(float_le,               "float_le").
binop_debug(float_ge,               "float_ge").
binop_debug(body,                   "body").
binop_debug(unsigned_le,            "unsigned_le").
binop_debug(compound_eq,            "compound_eq").
binop_debug(compound_lt,            "compound_lt").
binop_debug(str_cmp,                "strcmp").
binop_debug(float_word_bits,        "float_word_bits").
binop_debug(float_from_dword,       "float_from_dword").
binop_debug(pointer_equal_conservative, "pointer_equal_conservative").
binop_debug(offset_str_eq(_),       "offset_str_eq").
binop_debug(string_unsafe_index_code_unit, "string_unsafe_index_code_unit").
binop_debug(eq(int_type_uint),      "==(uint)").
binop_debug(ne(int_type_uint),      "!=(uint)").
binop_debug(int_lt(int_type_uint),  "<(uint)").
binop_debug(int_gt(int_type_uint),  ">(uint)").
binop_debug(int_le(int_type_uint),  "<=(uint)").
binop_debug(int_ge(int_type_uint),  ">=(uint)").
binop_debug(int_add(int_type_uint), "+(uint)").
binop_debug(int_sub(int_type_uint), "-(uint)").
binop_debug(int_mul(int_type_uint), "*(uint)").
binop_debug(int_div(int_type_uint), "/(uint)").
binop_debug(int_mod(int_type_uint), "mod(uint)").
binop_debug(bitwise_and(int_type_uint), "&(uint)").
binop_debug(bitwise_or(int_type_uint), "|(uint)").
binop_debug(bitwise_xor(int_type_uint), "^(uint)").
binop_debug(unchecked_left_shift(int_type_uint), "<<(uint)").
binop_debug(unchecked_right_shift(int_type_uint), ">>(uint)").
binop_debug(eq(int_type_int8),      "==(int8)").
binop_debug(ne(int_type_int8),      "!=(int8)").
binop_debug(int_lt(int_type_int8),  "<(int8)").
binop_debug(int_gt(int_type_int8),  ">(int8)").
binop_debug(int_le(int_type_int8),  "<=(int8)").
binop_debug(int_ge(int_type_int8),  ">=(int8)").
binop_debug(int_add(int_type_int8), "+(int8)").
binop_debug(int_sub(int_type_int8), "-(int8)").
binop_debug(int_mul(int_type_int8), "*(int8)").
binop_debug(int_div(int_type_int8), "/(int8)").
binop_debug(int_mod(int_type_int8), "mod(int8)").
binop_debug(bitwise_and(int_type_int8), "&(int8)").
binop_debug(bitwise_or(int_type_int8), "|(int8)").
binop_debug(bitwise_xor(int_type_int8), "^(int8)").
binop_debug(unchecked_left_shift(int_type_int8), "<<(int8)").
binop_debug(unchecked_right_shift(int_type_int8), ">>(int8)").
binop_debug(eq(int_type_uint8),      "==(uint8)").
binop_debug(ne(int_type_uint8),      "!=(uint8)").
binop_debug(int_lt(int_type_uint8),  "<(uint8)").
binop_debug(int_gt(int_type_uint8),  ">(uint8)").
binop_debug(int_le(int_type_uint8),  "<=(uint8)").
binop_debug(int_ge(int_type_uint8),  ">=(uint8)").
binop_debug(int_add(int_type_uint8), "+(uint8)").
binop_debug(int_sub(int_type_uint8), "-(uint8)").
binop_debug(int_mul(int_type_uint8), "*(uint8)").
binop_debug(int_div(int_type_uint8), "/(uint8)").
binop_debug(int_mod(int_type_uint8), "mod(uint8)").
binop_debug(bitwise_and(int_type_uint8), "&(uint8)").
binop_debug(bitwise_or(int_type_uint8), "|(uint8)").
binop_debug(bitwise_xor(int_type_uint8), "^(uint8)").
binop_debug(unchecked_left_shift(int_type_uint8), "<<(uint8)").
binop_debug(unchecked_right_shift(int_type_uint8), ">>(uint8)").
binop_debug(eq(int_type_int16),      "==(int6)").
binop_debug(ne(int_type_int16),      "!=(int16)").
binop_debug(int_lt(int_type_int16),  "<(int16)").
binop_debug(int_gt(int_type_int16),  ">(int16)").
binop_debug(int_le(int_type_int16),  "<=(int16)").
binop_debug(int_ge(int_type_int16),  ">=(int16)").
binop_debug(int_add(int_type_int16), "+(int16)").
binop_debug(int_sub(int_type_int16), "-(int16)").
binop_debug(int_mul(int_type_int16), "*(int16)").
binop_debug(int_div(int_type_int16), "/(int16)").
binop_debug(int_mod(int_type_int16), "mod(int16)").
binop_debug(bitwise_and(int_type_int16), "&(int16)").
binop_debug(bitwise_or(int_type_int16), "|(int16)").
binop_debug(bitwise_xor(int_type_int16), "^(int16)").
binop_debug(unchecked_left_shift(int_type_int16), "<<(int16)").
binop_debug(unchecked_right_shift(int_type_int16), ">>(int16)").
binop_debug(eq(int_type_uint16),      "==(uint16)").
binop_debug(ne(int_type_uint16),      "!=(uint16)").
binop_debug(int_lt(int_type_uint16),  "<(uint16)").
binop_debug(int_gt(int_type_uint16),  ">(uint16)").
binop_debug(int_le(int_type_uint16),  "<=(uint16)").
binop_debug(int_ge(int_type_uint16),  ">=(uint16)").
binop_debug(int_add(int_type_uint16), "+(uint16)").
binop_debug(int_sub(int_type_uint16), "-(uint16)").
binop_debug(int_mul(int_type_uint16), "*(uint16)").
binop_debug(int_div(int_type_uint16), "/(uint16)").
binop_debug(int_mod(int_type_uint16), "mod(uint16)").
binop_debug(bitwise_and(int_type_uint16), "&(uint16)").
binop_debug(bitwise_or(int_type_uint16), "|(uint16)").
binop_debug(bitwise_xor(int_type_uint16), "^(uint16)").
binop_debug(unchecked_left_shift(int_type_uint16), "<<(uint16)").
binop_debug(unchecked_right_shift(int_type_uint16), ">>(uint16)").
binop_debug(eq(int_type_int32),      "==(int32)").
binop_debug(ne(int_type_int32),      "!=(int32)").
binop_debug(int_lt(int_type_int32),  "<(int32)").
binop_debug(int_gt(int_type_int32),  ">(int32)").
binop_debug(int_le(int_type_int32),  "<=(int32)").
binop_debug(int_ge(int_type_int32),  ">=(int32)").
binop_debug(int_add(int_type_int32), "+(int32)").
binop_debug(int_sub(int_type_int32), "-(int32)").
binop_debug(int_mul(int_type_int32), "*(int32)").
binop_debug(int_div(int_type_int32), "/(int32)").
binop_debug(int_mod(int_type_int32), "mod(int32)").
binop_debug(bitwise_and(int_type_int32), "&(int32)").
binop_debug(bitwise_or(int_type_int32), "|(int32)").
binop_debug(bitwise_xor(int_type_int32), "^(int32)").
binop_debug(unchecked_left_shift(int_type_int32), "<<(int32)").
binop_debug(unchecked_right_shift(int_type_int32), ">>(int32)").
binop_debug(eq(int_type_uint32),      "==(uint32)").
binop_debug(ne(int_type_uint32),      "!=(uint32)").
binop_debug(int_lt(int_type_uint32),  "<(uint32)").
binop_debug(int_gt(int_type_uint32),  ">(uint32)").
binop_debug(int_le(int_type_uint32),  "<=(uint32)").
binop_debug(int_ge(int_type_uint32),  ">=(uint32)").
binop_debug(int_add(int_type_uint32), "+(uint32)").
binop_debug(int_sub(int_type_uint32), "-(uint32)").
binop_debug(int_mul(int_type_uint32), "*(uint32)").
binop_debug(int_div(int_type_uint32), "/(uint32)").
binop_debug(int_mod(int_type_uint32), "mod(uint32").
binop_debug(bitwise_and(int_type_uint32), "&(uint32)").
binop_debug(bitwise_or(int_type_uint32),  "|(uint32)").
binop_debug(bitwise_xor(int_type_uint32), "^(uint32)").
binop_debug(unchecked_left_shift(int_type_uint32), "<<(uint32)").
binop_debug(unchecked_right_shift(int_type_uint32), ">>(uint32)").
binop_debug(eq(int_type_int64),      "==(int64)").
binop_debug(ne(int_type_int64),      "!=(int64)").
binop_debug(int_lt(int_type_int64),  "<(int64)").
binop_debug(int_gt(int_type_int64),  ">(int64)").
binop_debug(int_le(int_type_int64),  "<=(int64)").
binop_debug(int_ge(int_type_int64),  ">=(int64)").
binop_debug(int_add(int_type_int64), "+(int64)").
binop_debug(int_sub(int_type_int64), "-(int64)").
binop_debug(int_mul(int_type_int64), "*(int64)").
binop_debug(int_div(int_type_int64), "/(int64)").
binop_debug(int_mod(int_type_int64), "mod(int64)").
binop_debug(bitwise_and(int_type_int64), "&(int64)").
binop_debug(bitwise_or(int_type_int64), "|(int64)").
binop_debug(bitwise_xor(int_type_int64), "^(int64)").
binop_debug(unchecked_left_shift(int_type_int64), "<<(int64)").
binop_debug(unchecked_right_shift(int_type_int64), ">>(int64)").
binop_debug(eq(int_type_uint64),      "==(uint64)").
binop_debug(ne(int_type_uint64),      "!=(uint64)").
binop_debug(int_lt(int_type_uint64),  "<(uint64)").
binop_debug(int_gt(int_type_uint64),  ">(uint64)").
binop_debug(int_le(int_type_uint64),  "<=(uint64)").
binop_debug(int_ge(int_type_uint64),  ">=(uint64)").
binop_debug(int_add(int_type_uint64), "+(uint64)").
binop_debug(int_sub(int_type_uint64), "-(uint64)").
binop_debug(int_mul(int_type_uint64), "*(uint64)").
binop_debug(int_div(int_type_uint64), "/(uint64)").
binop_debug(int_mod(int_type_uint64), "mod(uint64").
binop_debug(bitwise_and(int_type_uint64), "&(uint64)").
binop_debug(bitwise_or(int_type_uint64),  "|(uint64)").
binop_debug(bitwise_xor(int_type_uint64), "^(uint64)").
binop_debug(unchecked_left_shift(int_type_uint64), "<<(uint64)").
binop_debug(unchecked_right_shift(int_type_uint64), ">>(uint64)").

:- pred unop_code(unary_op::in, int::out) is det.

unop_code(mktag,                0).
unop_code(tag,                  1).
unop_code(unmktag,              2).
unop_code(mkbody,               3).
unop_code(unmkbody,             4).
unop_code(strip_tag,            5).
unop_code(bitwise_complement(int_type_int), 6).
unop_code(logical_not,          7).
unop_code(hash_string,          8).
unop_code(hash_string2,         9).
unop_code(hash_string3,        10).
unop_code(hash_string4,        11).
unop_code(hash_string5,        12).
unop_code(hash_string6,        13).
unop_code(bitwise_complement(int_type_uint), 14).
unop_code(bitwise_complement(int_type_int8), 15).
unop_code(bitwise_complement(int_type_uint8), 16).
unop_code(bitwise_complement(int_type_int16), 17).
unop_code(bitwise_complement(int_type_uint16), 18).
unop_code(bitwise_complement(int_type_int32), 19).
unop_code(bitwise_complement(int_type_uint32), 20).
unop_code(bitwise_complement(int_type_int64), 21).
unop_code(bitwise_complement(int_type_uint64), 22).

:- pred unop_debug(unary_op::in, string::out) is det.

unop_debug(mktag,               "mktag").
unop_debug(tag,                 "tag").
unop_debug(unmktag,             "unmktag").
unop_debug(mkbody,              "mkbody").
unop_debug(unmkbody,            "unmkbody").
unop_debug(strip_tag,           "strip_tag").
unop_debug(bitwise_complement(int_type_int),  "bitwise_complement(int)").
unop_debug(logical_not,         "not").
unop_debug(hash_string,         "hash_string").
unop_debug(hash_string2,        "hash_string2").
unop_debug(hash_string3,        "hash_string3").
unop_debug(hash_string4,        "hash_string4").
unop_debug(hash_string5,        "hash_string5").
unop_debug(hash_string6,        "hash_string6").
unop_debug(bitwise_complement(int_type_uint), "bitwise_complement(uint)").
unop_debug(bitwise_complement(int_type_int8), "bitwise_complement(int8)").
unop_debug(bitwise_complement(int_type_uint8), "bitwise_complement(uint8)").
unop_debug(bitwise_complement(int_type_int16), "bitwise_complement(int16)").
unop_debug(bitwise_complement(int_type_uint16), "bitwise_complement(uint16)").
unop_debug(bitwise_complement(int_type_int32), "bitwise_complement(int32)").
unop_debug(bitwise_complement(int_type_uint32), "bitwise_complement(uint32)").
unop_debug(bitwise_complement(int_type_int64), "bitwise_complement(int64)").
unop_debug(bitwise_complement(int_type_uint64), "bitwise_complement(uint64)").

%---------------------------------------------------------------------------%

    % debug_cstring prints a string quoted in the manner of C.
    %
:- pred debug_cstring(string::in, io::di, io::uo) is det.

debug_cstring(Str, !IO) :-
    io.output_stream(Stream, !IO),
    io.write_char(Stream, '"', !IO),
    c_util.output_quoted_string(Stream, Str, !IO),
    io.write_char(Stream, '"', !IO),
    % XXX: We need the trailing space in case something follows
    % the string as a bytecode argument. This is not very elegant.
    io.write_char(Stream, ' ', !IO).

:- pred debug_string(string::in, io::di, io::uo) is det.

debug_string(Val, !IO) :-
    io.write_string(Val, !IO),
    io.write_char(' ', !IO).

:- pred debug_int(int::in, io::di, io::uo) is det.

debug_int(Val, !IO) :-
    io.write_int(Val, !IO),
    io.write_char(' ', !IO).

:- pred debug_float(float::in, io::di, io::uo) is det.

debug_float(Val, !IO) :-
    io.write_float(Val, !IO),
    io.write_char(' ', !IO).

:- pred debug_sym_name(sym_name::in, io::di, io::uo) is det.

debug_sym_name(unqualified(Val), !IO) :-
    io.write_string(Val, !IO),
    io.write_char(' ', !IO).
debug_sym_name(qualified(Module, Val), !IO) :-
    debug_sym_name(Module, !IO),
    io.write_char(':', !IO),
    io.write_string(Val, !IO),
    io.write_char(' ', !IO).

%---------------------------------------------------------------------------%
:- end_module bytecode_backend.bytecode.
%---------------------------------------------------------------------------%
