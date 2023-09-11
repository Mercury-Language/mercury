%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2007, 2009-2012 The University of Melbourne.
% Copyright (C) 2014-2018 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: bytecode.m.
% Author: zs.
%
% This module defines the bytecode used by a possible future
% bytecode-based debugger.
%
%---------------------------------------------------------------------------%

:- module bytecode_backend.bytecode.
:- interface.

:- import_module backend_libs.
:- import_module backend_libs.builtin_ops.
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

:- type byte_tree == cord(byte_code).

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
    ;       byte_unshared_tag(int)
    ;       byte_shared_remote_tag(int, int)
    ;       byte_shared_local_tag(int, int)
    ;       byte_enum_tag(int).

:- type byte_arg
    --->    byte_arg_var(byte_var)
    ;       byte_arg_int_const(int)
    ;       byte_arg_float_const(float)
    ;       byte_arg_uint_const(uint)
    ;       byte_arg_int8_const(int8)
    ;       byte_arg_uint8_const(uint8)
    ;       byte_arg_int16_const(int16)
    ;       byte_arg_uint16_const(uint16)
    ;       byte_arg_int32_const(int32)
    ;       byte_arg_uint32_const(uint32)
    ;       byte_arg_int64_const(int64)
    ;       byte_arg_uint64_const(uint64).

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
:- type byte_var        ==  int.
:- type byte_temp       ==  int.
:- type byte_is_func    ==  int.    % 0 if a predicate, 1 if a function

:- pred output_bytecode_file(io.text_output_stream::in, string::in,
    list(byte_code)::in, io::di, io::uo) is det.

:- pred debug_bytecode_file(io.text_output_stream::in, string::in,
    list(byte_code)::in, io::di, io::uo) is det.

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

output_bytecode_file(ProgressStream, FileName, ByteCodes, !IO) :-
    io.open_binary_output(FileName, Result, !IO),
    (
        Result = ok(FileStream),
        bytecode.version(Version),
        output_short(FileStream, Version, !IO),
        output_bytecode_list(FileStream, ByteCodes, !IO),
        io.close_binary_output(FileStream, !IO)
    ;
        Result = error(_),
        io.progname_base("byte.m", ProgName, !IO),
        io.format(ProgressStream, "\n%s: can't open `%s' for output\n",
            [s(ProgName), s(FileName)], !IO),
        io.set_exit_status(1, !IO)
    ).

debug_bytecode_file(ProgressStream, FileName, ByteCodes, !IO) :-
    io.open_output(FileName, Result, !IO),
    (
        Result = ok(FileStream),
        bytecode.version(Version),
        io.format(FileStream, "bytecode_version %d\n", [i(Version)], !IO),
        debug_bytecode_list(FileStream, ByteCodes, !IO),
        io.close_output(FileStream, !IO)
    ;
        Result = error(_),
        io.progname_base("byte.m", ProgName, !IO),
        io.format(ProgressStream, "\n%s: can't open `%s' for output\n",
            [s(ProgName), s(FileName)], !IO),
        io.set_exit_status(1, !IO)
    ).

:- pred output_bytecode_list(io.binary_output_stream::in, list(byte_code)::in,
    io::di, io::uo) is det.

output_bytecode_list(_BinaryOutputStream, [], !IO).
output_bytecode_list(BinaryOutputStream, [ByteCode | ByteCodes], !IO) :-
    byte_code(ByteCode, Byte),
    io.write_byte(BinaryOutputStream, Byte, !IO),
    output_args(BinaryOutputStream, ByteCode, !IO),
    output_bytecode_list(BinaryOutputStream, ByteCodes, !IO).

:- pred debug_bytecode_list(io.text_output_stream::in, list(byte_code)::in,
    io::di, io::uo) is det.

debug_bytecode_list(_OutputStream, [], !IO).
debug_bytecode_list(OutputStream, [ByteCode | ByteCodes], !IO) :-
    byte_debug(ByteCode, Debug),
    debug_string(OutputStream, Debug, !IO),
    debug_args(OutputStream, ByteCode, !IO),
    io.write_char(OutputStream, '\n', !IO),
    debug_bytecode_list(OutputStream, ByteCodes, !IO).

:- pred output_args(io.binary_output_stream::in, byte_code::in,
    io::di, io::uo) is det.

output_args(BinaryOutputStream, ByteCode, !IO) :-
    (
        ByteCode = byte_enter_pred(PredId, PredArity, IsFunc, ProcCount),
        output_pred_id(BinaryOutputStream, PredId, !IO),
        output_length(BinaryOutputStream, PredArity, !IO),
        output_is_func(BinaryOutputStream, IsFunc, !IO),
        output_length(BinaryOutputStream, ProcCount, !IO)
    ;
        ByteCode = byte_endof_pred
    ;
        ByteCode = byte_enter_proc(ProcId, Detism, LabelCount, LabelId,
            TempCount, Vars),
        list.length(Vars, NumVars),
        output_proc_id(BinaryOutputStream, ProcId, !IO),
        output_determinism(BinaryOutputStream, Detism, !IO),
        output_length(BinaryOutputStream, LabelCount, !IO),
        output_label_id(BinaryOutputStream, LabelId, !IO),
        output_length(BinaryOutputStream, TempCount, !IO),
        output_length(BinaryOutputStream, NumVars, !IO),
        output_var_infos(BinaryOutputStream, Vars, !IO)
    ;
        ByteCode = byte_endof_proc
    ;
        ByteCode = byte_label(LabelId),
        output_label_id(BinaryOutputStream, LabelId, !IO)
    ;
        ByteCode = byte_enter_disjunction(LabelId),
        output_label_id(BinaryOutputStream, LabelId, !IO)
    ;
        ByteCode = byte_endof_disjunction
    ;
        ByteCode = byte_enter_disjunct(LabelId),
        output_label_id(BinaryOutputStream, LabelId, !IO)
    ;
        ByteCode = byte_endof_disjunct(LabelId),
        output_label_id(BinaryOutputStream, LabelId, !IO)
    ;
        ByteCode = byte_enter_switch(Var, LabelId),
        output_var(BinaryOutputStream, Var, !IO),
        output_label_id(BinaryOutputStream, LabelId, !IO)
    ;
        ByteCode = byte_endof_switch
    ;
        ByteCode = byte_enter_switch_arm(MainConsId, OtherConsIds,
            NextLabelId),
        output_cons_id(BinaryOutputStream, MainConsId, !IO),
        % The interpreter doesn't yet implement switch arms with more than one
        % function symbol.
        expect(unify(OtherConsIds, []), $pred, "OtherConsIds"),
        output_label_id(BinaryOutputStream, NextLabelId, !IO)
    ;
        ByteCode = byte_endof_switch_arm(LabelId),
        output_label_id(BinaryOutputStream, LabelId, !IO)
    ;
        ByteCode = byte_enter_if(ElseLabelId, FollowLabelId, FramePtrTemp),
        output_label_id(BinaryOutputStream, ElseLabelId, !IO),
        output_label_id(BinaryOutputStream, FollowLabelId, !IO),
        output_temp(BinaryOutputStream, FramePtrTemp, !IO)
    ;
        ByteCode = byte_enter_then(FramePtrTemp),
        output_temp(BinaryOutputStream, FramePtrTemp, !IO)
    ;
        ByteCode = byte_endof_then(FollowLabelId),
        output_label_id(BinaryOutputStream, FollowLabelId, !IO)
    ;
        ByteCode = byte_enter_else(FramePtrTemp),
        output_temp(BinaryOutputStream, FramePtrTemp, !IO)
    ;
        ByteCode = byte_endof_if
    ;
        ByteCode = byte_enter_negation(FramePtrTemp, LabelId),
        output_temp(BinaryOutputStream, FramePtrTemp, !IO),
        output_label_id(BinaryOutputStream, LabelId, !IO)
    ;
        ByteCode = byte_endof_negation_goal(FramePtrTemp),
        output_temp(BinaryOutputStream, FramePtrTemp, !IO)
    ;
        ByteCode = byte_endof_negation
    ;
        ByteCode = byte_enter_commit(Temp),
        output_temp(BinaryOutputStream, Temp, !IO)
    ;
        ByteCode = byte_endof_commit(Temp),
        output_temp(BinaryOutputStream, Temp, !IO)
    ;
        ByteCode = byte_assign(Var1, Var2),
        output_var(BinaryOutputStream, Var1, !IO),
        output_var(BinaryOutputStream, Var2, !IO)
    ;
        ByteCode = byte_test(Var1, Var2, TestId),
        output_var(BinaryOutputStream, Var1, !IO),
        output_var(BinaryOutputStream, Var2, !IO),
        output_test_id(BinaryOutputStream, TestId, !IO)
    ;
        ByteCode = byte_construct(Var, ConsId, Vars),
        list.length(Vars, NumVars),
        output_var(BinaryOutputStream, Var, !IO),
        output_cons_id(BinaryOutputStream, ConsId, !IO),
        output_length(BinaryOutputStream, NumVars, !IO),
        output_vars(BinaryOutputStream, Vars, !IO)
    ;
        ByteCode = byte_deconstruct(Var, ConsId, Vars),
        list.length(Vars, NumVars),
        output_var(BinaryOutputStream, Var, !IO),
        output_cons_id(BinaryOutputStream, ConsId, !IO),
        output_length(BinaryOutputStream, NumVars, !IO),
        output_vars(BinaryOutputStream, Vars, !IO)
    ;
        ByteCode = byte_complex_construct(Var, ConsId, VarDirs),
        list.length(VarDirs, NumVarsDirs),
        output_var(BinaryOutputStream, Var, !IO),
        output_cons_id(BinaryOutputStream, ConsId, !IO),
        output_length(BinaryOutputStream, NumVarsDirs, !IO),
        output_var_dirs(BinaryOutputStream, VarDirs, !IO)
    ;
        ByteCode = byte_complex_deconstruct(Var, ConsId, VarDirs),
        list.length(VarDirs, NumVarsDirs),
        output_var(BinaryOutputStream, Var, !IO),
        output_cons_id(BinaryOutputStream, ConsId, !IO),
        output_length(BinaryOutputStream, NumVarsDirs, !IO),
        output_var_dirs(BinaryOutputStream, VarDirs, !IO)
    ;
        ByteCode = byte_place_arg(RegType, RegNum, Var),
        output_reg(BinaryOutputStream, RegType, RegNum, !IO),
        output_var(BinaryOutputStream, Var, !IO)
    ;
        ByteCode = byte_pickup_arg(RegType, RegNum, Var),
        output_reg(BinaryOutputStream, RegType, RegNum, !IO),
        output_var(BinaryOutputStream, Var, !IO)
    ;
        ByteCode = byte_call(ModuleId, PredId, Arity, IsFunc, ProcId),
        output_module_id(BinaryOutputStream, ModuleId, !IO),
        output_pred_id(BinaryOutputStream, PredId, !IO),
        output_length(BinaryOutputStream, Arity, !IO),
        output_is_func(BinaryOutputStream, IsFunc, !IO),
        output_proc_id(BinaryOutputStream, ProcId, !IO)
    ;
        ByteCode = byte_higher_order_call(PredVar, InVarCount, OutVarCount,
            Detism),
        output_var(BinaryOutputStream, PredVar, !IO),
        output_length(BinaryOutputStream, InVarCount, !IO),
        output_length(BinaryOutputStream, OutVarCount, !IO),
        output_determinism(BinaryOutputStream, Detism, !IO)
    ;
        ByteCode = byte_builtin_binop(Binop, Var1, Var2, Var3),
        output_binop(BinaryOutputStream, Binop, !IO),
        output_arg(BinaryOutputStream, Var1, !IO),
        output_arg(BinaryOutputStream, Var2, !IO),
        output_var(BinaryOutputStream, Var3, !IO)
    ;
        ByteCode = byte_builtin_unop(Unop, Var1, Var2),
        output_unop(BinaryOutputStream, Unop, !IO),
        output_arg(BinaryOutputStream, Var1, !IO),
        output_var(BinaryOutputStream, Var2, !IO)
    ;
        ByteCode = byte_builtin_bintest(Binop, Var1, Var2),
        output_binop(BinaryOutputStream, Binop, !IO),
        output_arg(BinaryOutputStream, Var1, !IO),
        output_arg(BinaryOutputStream, Var2, !IO)
    ;
        ByteCode = byte_builtin_untest(Unop, Var1),
        output_unop(BinaryOutputStream, Unop, !IO),
        output_arg(BinaryOutputStream, Var1, !IO)
    ;
        ByteCode = byte_semidet_succeed
    ;
        ByteCode = byte_semidet_success_check
    ;
        ByteCode = byte_fail
    ;
        ByteCode = byte_context(Line),
        output_short(BinaryOutputStream, Line, !IO)
    ;
        ByteCode = byte_not_supported
    ).

:- pred debug_args(io.text_output_stream::in, byte_code::in,
    io::di, io::uo) is det.

debug_args(OutputStream, ByteCode, !IO) :-
    (
        ByteCode = byte_enter_pred(PredId, PredArity, IsFunc, ProcsCount),
        debug_pred_id(OutputStream, PredId, !IO),
        debug_length(OutputStream, PredArity, !IO),
        debug_is_func(OutputStream, IsFunc, !IO),
        debug_length(OutputStream, ProcsCount, !IO)
    ;
        ByteCode = byte_endof_pred
    ;
        ByteCode = byte_enter_proc(ProcId, Detism, LabelCount, LabelId,
            TempCount, Vars),
        list.length(Vars, VarCount),
        debug_proc_id(OutputStream, ProcId, !IO),
        debug_determinism(OutputStream, Detism, !IO),
        debug_length(OutputStream, LabelCount, !IO),
        debug_label_id(OutputStream, LabelId, !IO),
        debug_length(OutputStream, TempCount, !IO),
        debug_length(OutputStream, VarCount, !IO),
        debug_var_infos(OutputStream, Vars, !IO)
    ;
        ByteCode = byte_endof_proc
    ;
        ByteCode = byte_label(LabelId),
        debug_label_id(OutputStream, LabelId, !IO)
    ;
        ByteCode = byte_enter_disjunction(LabelId),
        debug_label_id(OutputStream, LabelId, !IO)
    ;
        ByteCode = byte_endof_disjunction
    ;
        ByteCode = byte_enter_disjunct(LabelId),
        debug_label_id(OutputStream, LabelId, !IO)
    ;
        ByteCode = byte_endof_disjunct(LabelId),
        debug_label_id(OutputStream, LabelId, !IO)
    ;
        ByteCode = byte_enter_switch(Var, LabelId),
        debug_var(OutputStream, Var, !IO),
        debug_label_id(OutputStream, LabelId, !IO)
    ;
        ByteCode = byte_endof_switch
    ;
        ByteCode = byte_enter_switch_arm(MainConsId, OtherConsIds,
            NextLabelId),
        debug_cons_id(OutputStream, MainConsId, !IO),
        list.foldl(debug_cons_id(OutputStream), OtherConsIds, !IO),
        debug_label_id(OutputStream, NextLabelId, !IO)
    ;
        ByteCode = byte_endof_switch_arm(LabelId),
        debug_label_id(OutputStream, LabelId, !IO)
    ;
        ByteCode = byte_enter_if(ElseLabelId, FollowLabelId, FramePtrTemp),
        debug_label_id(OutputStream, ElseLabelId, !IO),
        debug_label_id(OutputStream, FollowLabelId, !IO),
        debug_temp(OutputStream, FramePtrTemp, !IO)
    ;
        ByteCode = byte_enter_then(FramePtrTemp),
        debug_temp(OutputStream, FramePtrTemp, !IO)
    ;
        ByteCode = byte_endof_then(FollowLabelId),
        debug_label_id(OutputStream, FollowLabelId, !IO)
    ;
        ByteCode = byte_enter_else(FramePtrTemp),
        debug_temp(OutputStream, FramePtrTemp, !IO)
    ;
        ByteCode = byte_endof_if
    ;
        ByteCode = byte_enter_negation(FramePtrTemp, LabelId),
        debug_temp(OutputStream, FramePtrTemp, !IO),
        debug_label_id(OutputStream, LabelId, !IO)
    ;
        ByteCode = byte_endof_negation_goal(FramePtrTemp),
        debug_temp(OutputStream, FramePtrTemp, !IO)
    ;
        ByteCode = byte_endof_negation
    ;
        ByteCode = byte_enter_commit(Temp),
        debug_temp(OutputStream, Temp, !IO)
    ;
        ByteCode = byte_endof_commit(Temp),
        debug_temp(OutputStream, Temp, !IO)
    ;
        ByteCode = byte_assign(Var1, Var2),
        debug_var(OutputStream, Var1, !IO),
        debug_var(OutputStream, Var2, !IO)
    ;
        ByteCode = byte_test(Var1, Var2, TestId),
        debug_var(OutputStream, Var1, !IO),
        debug_var(OutputStream, Var2, !IO),
        debug_test_id(OutputStream, TestId, !IO)
    ;
        ByteCode = byte_construct(Var, ConsId, Vars),
        list.length(Vars, NumVars),
        debug_var(OutputStream, Var, !IO),
        debug_cons_id(OutputStream, ConsId, !IO),
        debug_length(OutputStream, NumVars, !IO),
        debug_vars(OutputStream, Vars, !IO)
    ;
        ByteCode = byte_deconstruct(Var, ConsId, Vars),
        list.length(Vars, NumVars),
        debug_var(OutputStream, Var, !IO),
        debug_cons_id(OutputStream, ConsId, !IO),
        debug_length(OutputStream, NumVars, !IO),
        debug_vars(OutputStream, Vars, !IO)
    ;
        ByteCode = byte_complex_construct(Var, ConsId, VarDirs),
        list.length(VarDirs, Length),
        debug_var(OutputStream, Var, !IO),
        debug_cons_id(OutputStream, ConsId, !IO),
        debug_length(OutputStream, Length, !IO),
        debug_var_dirs(OutputStream, VarDirs, !IO)
    ;
        ByteCode = byte_complex_deconstruct(Var, ConsId, VarDirs),
        list.length(VarDirs, NumVarDirs),
        debug_var(OutputStream, Var, !IO),
        debug_cons_id(OutputStream, ConsId, !IO),
        debug_length(OutputStream, NumVarDirs, !IO),
        debug_var_dirs(OutputStream, VarDirs, !IO)
    ;
        ByteCode = byte_place_arg(RegType, RegNum, Var),
        debug_reg(OutputStream, RegType, RegNum, !IO),
        debug_var(OutputStream, Var, !IO)
    ;
        ByteCode = byte_pickup_arg(RegType, RegNum, Var),
        debug_reg(OutputStream, RegType, RegNum, !IO),
        debug_var(OutputStream, Var, !IO)
    ;
        ByteCode = byte_call(ModuleId, PredId, Arity, IsFunc, ProcId),
        debug_module_id(OutputStream, ModuleId, !IO),
        debug_pred_id(OutputStream, PredId, !IO),
        debug_length(OutputStream, Arity, !IO),
        debug_is_func(OutputStream, IsFunc, !IO),
        debug_proc_id(OutputStream, ProcId, !IO)
    ;
        ByteCode = byte_higher_order_call(PredVar, InVarCount, OutVarCount,
            Detism),
        debug_var(OutputStream, PredVar, !IO),
        debug_length(OutputStream, InVarCount, !IO),
        debug_length(OutputStream, OutVarCount, !IO),
        debug_determinism(OutputStream, Detism, !IO)
    ;
        ByteCode = byte_builtin_binop(Binop, Var1, Var2, Var3),
        debug_binop(OutputStream, Binop, !IO),
        debug_arg(OutputStream, Var1, !IO),
        debug_arg(OutputStream, Var2, !IO),
        debug_var(OutputStream, Var3, !IO)
    ;
        ByteCode = byte_builtin_unop(Unop, Var1, Var2),
        debug_unop(OutputStream, Unop, !IO),
        debug_arg(OutputStream, Var1, !IO),
        debug_var(OutputStream, Var2, !IO)
    ;
        ByteCode = byte_builtin_bintest(Binop, Var1, Var2),
        debug_binop(OutputStream, Binop, !IO),
        debug_arg(OutputStream, Var1, !IO),
        debug_arg(OutputStream, Var2, !IO)
    ;
        ByteCode = byte_builtin_untest(Unop, Var1),
        debug_unop(OutputStream, Unop, !IO),
        debug_arg(OutputStream, Var1, !IO)
    ;
        ByteCode = byte_semidet_succeed
    ;
        ByteCode = byte_semidet_success_check
    ;
        ByteCode = byte_fail
    ;
        ByteCode = byte_context(Line),
        debug_int(OutputStream, Line, !IO)
    ;
        ByteCode = byte_not_supported
    ).

%---------------------------------------------------------------------------%

:- pred output_var_infos(io.binary_output_stream::in, list(byte_var_info)::in,
    io::di, io::uo) is det.

output_var_infos(_BinaryOutputStream, [], !IO).
output_var_infos(BinaryOutputStream, [Var | Vars], !IO) :-
    output_var_info(BinaryOutputStream, Var, !IO),
    output_var_infos(BinaryOutputStream, Vars, !IO).

:- pred output_var_info(io.binary_output_stream::in, byte_var_info::in,
    io::di, io::uo) is det.

output_var_info(BinaryOutputStream, var_info(Name, _), !IO) :-
    output_string(BinaryOutputStream, Name, !IO).

:- pred debug_var_infos(io.text_output_stream::in, list(byte_var_info)::in,
    io::di, io::uo) is det.

debug_var_infos(_OutputStream, [], !IO).
debug_var_infos(OutputStream, [Var | Vars], !IO) :-
    debug_var_info(OutputStream, Var, !IO),
    debug_var_infos(OutputStream, Vars, !IO).

:- pred debug_var_info(io.text_output_stream::in, byte_var_info::in,
    io::di, io::uo) is det.

debug_var_info(OutputStream, var_info(Name, _), !IO) :-
    debug_string(OutputStream, Name, !IO).

%---------------------------------------------------------------------------%

:- pred output_determinism(io.binary_output_stream::in, determinism::in,
    io::di, io::uo) is det.

output_determinism(BinaryOutputStream, Detism, !IO) :-
    determinism_code(Detism, Code),
    output_byte(BinaryOutputStream, Code, !IO).

:- pred debug_determinism(io.text_output_stream::in, determinism::in,
    io::di, io::uo) is det.

debug_determinism(OutputStream, Detism, !IO) :-
    determinism_debug(Detism, Debug),
    debug_string(OutputStream, Debug, !IO).

%---------------------------------------------------------------------------%

:- pred output_reg(io.binary_output_stream::in, byte_reg_type::in, int::in,
    io::di, io::uo) is det.

output_reg(BinaryOutputStream, byte_reg_r, N, !IO) :-
    output_byte(BinaryOutputStream, N, !IO).

:- pred debug_reg(io.text_output_stream::in, byte_reg_type::in, int::in,
    io::di, io::uo) is det.

debug_reg(OutputStream, byte_reg_r, N, !IO) :-
    debug_int(OutputStream, N, !IO).

%---------------------------------------------------------------------------%

:- pred output_is_func(io.binary_output_stream::in, byte_is_func::in,
    io::di, io::uo) is det.

output_is_func(BinaryOutputStream, IsFunc, !IO) :-
    ( if ( IsFunc = 1 ; IsFunc = 0 ) then
        output_byte(BinaryOutputStream, IsFunc, !IO)
    else
        unexpected($pred,
            "invalid predicate or function specified in bytecode")
    ).

:- pred debug_is_func(io.text_output_stream::in, byte_is_func::in,
    io::di, io::uo) is det.

debug_is_func(OutputStream, IsFunc, !IO) :-
    ( if IsFunc = 1 then
        debug_string(OutputStream, "func", !IO)
    else if IsFunc = 0 then
        debug_string(OutputStream, "pred", !IO)
    else
        unexpected($pred,
            "invalid predicate or function specifier in bytecode.")
    ).

%---------------------------------------------------------------------------%

:- pred output_length(io.binary_output_stream::in, int::in,
    io::di, io::uo) is det.

output_length(BinaryOutputStream, Length, !IO) :-
    output_short(BinaryOutputStream, Length, !IO).

:- pred debug_length(io.text_output_stream::in, int::in,
    io::di, io::uo) is det.

debug_length(OutputStream, Length, !IO) :-
    debug_int(OutputStream, Length, !IO).

%---------------------------------------------------------------------------%

:- pred output_arg(io.binary_output_stream::in, byte_arg::in,
    io::di, io::uo) is det.

output_arg(BinaryOutputStream, byte_arg_var(Var), !IO) :-
    output_byte(BinaryOutputStream, 0, !IO),
    output_var(BinaryOutputStream, Var, !IO).
output_arg(BinaryOutputStream, byte_arg_int_const(IntVal), !IO) :-
    output_byte(BinaryOutputStream, 1, !IO),
    output_int(BinaryOutputStream, IntVal, !IO).
output_arg(BinaryOutputStream, byte_arg_float_const(FloatVal), !IO) :-
    output_byte(BinaryOutputStream, 2, !IO),
    output_float(BinaryOutputStream, FloatVal, !IO).
output_arg(_, byte_arg_uint_const(_), _, _) :-
    unexpected($pred, "NYI uint constants in bytecode").
output_arg(_, byte_arg_int8_const(_), _, _) :-
    unexpected($pred, "NYI int8 constants in bytecode").
output_arg(_, byte_arg_uint8_const(_), _, _) :-
    unexpected($pred, "NYI uint8 constants in bytecode").
output_arg(_, byte_arg_int16_const(_), _, _) :-
    unexpected($pred, "NYI int16 constants in bytecode").
output_arg(_, byte_arg_uint16_const(_), _, _) :-
    unexpected($pred, "NYI uint16 constants in bytecode").
output_arg(_, byte_arg_int32_const(_), _, _) :-
    unexpected($pred, "NYI int32 constants in bytecode").
output_arg(_, byte_arg_uint32_const(_), _, _) :-
    unexpected($pred, "NYI uint32 constants in bytecode").
output_arg(_, byte_arg_int64_const(_), _, _) :-
    unexpected($pred, "NYI int64 constants in bytecode").
output_arg(_, byte_arg_uint64_const(_), _, _) :-
    unexpected($pred, "NYI uint64 constants in bytecode").

:- pred debug_arg(io.text_output_stream::in, byte_arg::in,
    io::di, io::uo) is det.

debug_arg(OutputStream, byte_arg_var(Var), !IO) :-
    debug_string(OutputStream, "var", !IO),
    debug_var(OutputStream, Var, !IO).
debug_arg(OutputStream, byte_arg_int_const(IntVal), !IO) :-
    debug_string(OutputStream, "int", !IO),
    debug_int(OutputStream, IntVal, !IO).
debug_arg(OutputStream, byte_arg_uint_const(UIntVal), !IO) :-
    debug_string(OutputStream, "uint", !IO),
    debug_uint(OutputStream, UIntVal, !IO).
debug_arg(OutputStream, byte_arg_int8_const(Int8Val), !IO) :-
    debug_string(OutputStream, "int8", !IO),
    debug_int8(OutputStream, Int8Val, !IO).
debug_arg(OutputStream, byte_arg_uint8_const(UInt8Val), !IO) :-
    debug_string(OutputStream, "uint8", !IO),
    debug_uint8(OutputStream, UInt8Val, !IO).
debug_arg(OutputStream, byte_arg_int16_const(Int16Val), !IO) :-
    debug_string(OutputStream, "int16", !IO),
    debug_int16(OutputStream, Int16Val, !IO).
debug_arg(OutputStream, byte_arg_uint16_const(UInt16Val), !IO) :-
    debug_string(OutputStream, "uint16", !IO),
    debug_uint16(OutputStream, UInt16Val, !IO).
debug_arg(OutputStream, byte_arg_int32_const(Int32Val), !IO) :-
    debug_string(OutputStream, "int32", !IO),
    debug_int32(OutputStream, Int32Val, !IO).
debug_arg(OutputStream, byte_arg_uint32_const(UInt32Val), !IO) :-
    debug_string(OutputStream, "uint32", !IO),
    debug_uint32(OutputStream, UInt32Val, !IO).
debug_arg(OutputStream, byte_arg_int64_const(Int64Val), !IO) :-
    debug_string(OutputStream, "int64", !IO),
    debug_int64(OutputStream, Int64Val, !IO).
debug_arg(OutputStream, byte_arg_uint64_const(UInt64Val), !IO) :-
    debug_string(OutputStream, "uint64", !IO),
    debug_uint64(OutputStream, UInt64Val, !IO).
debug_arg(OutputStream, byte_arg_float_const(FloatVal), !IO) :-
    debug_string(OutputStream, "float", !IO),
    debug_float(OutputStream, FloatVal, !IO).

%---------------------------------------------------------------------------%

:- pred output_var(io.binary_output_stream::in, byte_var::in,
    io::di, io::uo) is det.

output_var(BinaryOutputStream, Var, !IO) :-
    output_short(BinaryOutputStream, Var, !IO).

:- pred output_vars(io.binary_output_stream::in, list(byte_var)::in,
    io::di, io::uo) is det.

output_vars(_BinaryOutputStream, [], !IO).
output_vars(BinaryOutputStream, [Var | Vars], !IO) :-
    output_var(BinaryOutputStream, Var, !IO),
    output_vars(BinaryOutputStream, Vars, !IO).

:- pred debug_var(io.text_output_stream::in, byte_var::in,
    io::di, io::uo) is det.

debug_var(OutputStream, Var, !IO) :-
    debug_int(OutputStream, Var, !IO).

:- pred debug_vars(io.text_output_stream::in, list(byte_var)::in,
    io::di, io::uo) is det.

debug_vars(_OutputStream, [], !IO).
debug_vars(OutputStream, [Var | Vars], !IO) :-
    debug_var(OutputStream, Var, !IO),
    debug_vars(OutputStream, Vars, !IO).

%---------------------------------------------------------------------------%

:- pred output_temp(io.binary_output_stream::in, byte_temp::in,
    io::di, io::uo) is det.

output_temp(BinaryOutputStream, Var, !IO) :-
    output_short(BinaryOutputStream, Var, !IO).

:- pred debug_temp(io.text_output_stream::in, byte_temp::in,
    io::di, io::uo) is det.

debug_temp(OutputStream, Var, !IO) :-
    debug_int(OutputStream, Var, !IO).

%---------------------------------------------------------------------------%

:- pred output_dir(io.binary_output_stream::in, byte_dir::in,
    io::di, io::uo) is det.

output_dir(BinaryOutputStream, to_arg, !IO) :-
    output_byte(BinaryOutputStream, 0, !IO).
output_dir(BinaryOutputStream, to_var, !IO) :-
    output_byte(BinaryOutputStream, 1, !IO).
output_dir(BinaryOutputStream, to_none, !IO) :-
    output_byte(BinaryOutputStream, 2, !IO).

:- pred output_var_dirs(io.binary_output_stream::in,
    assoc_list(byte_var, byte_dir)::in, io::di, io::uo) is det.

output_var_dirs(_BinaryOutputStream, [], !IO).
output_var_dirs(BinaryOutputStream, [Var - Dir | VarDirs], !IO) :-
    output_var(BinaryOutputStream, Var, !IO),
    output_dir(BinaryOutputStream, Dir, !IO),
    output_var_dirs(BinaryOutputStream, VarDirs, !IO).

:- pred debug_dir(io.text_output_stream::in, byte_dir::in,
    io::di, io::uo) is det.

debug_dir(OutputStream, to_arg, !IO) :-
    debug_string(OutputStream, "to_arg", !IO).
debug_dir(OutputStream, to_var, !IO) :-
    debug_string(OutputStream, "to_var", !IO).
debug_dir(OutputStream, to_none, !IO) :-
    debug_string(OutputStream, "to_none", !IO).

:- pred debug_var_dirs(io.text_output_stream::in,
    assoc_list(byte_var, byte_dir)::in, io::di, io::uo) is det.

debug_var_dirs(_OutputStream, [], !IO).
debug_var_dirs(OutputStream, [Var - Dir | VarDirs], !IO) :-
    debug_var(OutputStream, Var, !IO),
    debug_dir(OutputStream, Dir, !IO),
    debug_var_dirs(OutputStream, VarDirs, !IO).

%---------------------------------------------------------------------------%

:- pred output_test_id(io.binary_output_stream::in, byte_test_id::in,
    io::di, io::uo) is det.

output_test_id(BinaryOutputStream, Test, !IO) :-
    ( Test = int_test,      TestId = 0
    ; Test = char_test,     TestId = 1
    ; Test = string_test,   TestId = 2
    ; Test = float_test,    TestId = 3
    ; Test = enum_test,     TestId = 4
    ; Test = dummy_test,    TestId = 5
    ),
    output_byte(BinaryOutputStream, TestId, !IO).

:- pred debug_test_id(io.text_output_stream::in, byte_test_id::in,
    io::di, io::uo) is det.

debug_test_id(OutputStream, Test, !IO) :-
    ( Test = int_test,      TestStr = "int"
    ; Test = char_test,     TestStr = "char"
    ; Test = string_test,   TestStr = "string"
    ; Test = float_test,    TestStr = "float"
    ; Test = enum_test,     TestStr = "enum"
    ; Test = dummy_test,    TestStr = "dummy"
    ),
    debug_string(OutputStream, TestStr, !IO).

%---------------------------------------------------------------------------%

:- pred output_module_id(io.binary_output_stream::in, byte_module_id::in,
    io::di, io::uo) is det.

output_module_id(BinaryOutputStream, ModuleId, !IO) :-
    output_string(BinaryOutputStream, sym_name_to_string(ModuleId), !IO).

:- pred debug_module_id(io.text_output_stream::in, byte_module_id::in,
    io::di, io::uo) is det.

debug_module_id(OutputStream, ModuleId, !IO) :-
    debug_sym_name(OutputStream, ModuleId, !IO).

%---------------------------------------------------------------------------%

:- pred output_pred_id(io.binary_output_stream::in, byte_pred_id::in,
    io::di, io::uo) is det.

output_pred_id(BinaryOutputStream, PredId, !IO) :-
    output_string(BinaryOutputStream, PredId, !IO).

:- pred debug_pred_id(io.text_output_stream::in, byte_pred_id::in,
    io::di, io::uo) is det.

debug_pred_id(OutputStream, PredId, !IO) :-
    debug_string(OutputStream, PredId, !IO).

%---------------------------------------------------------------------------%

:- pred output_proc_id(io.binary_output_stream::in, byte_proc_id::in,
    io::di, io::uo) is det.

output_proc_id(BinaryOutputStream, ProcId, !IO) :-
    output_byte(BinaryOutputStream, ProcId, !IO).

:- pred debug_proc_id(io.text_output_stream::in, byte_proc_id::in,
    io::di, io::uo) is det.

debug_proc_id(OutputStream, ProcId, !IO) :-
    debug_int(OutputStream, ProcId, !IO).

%---------------------------------------------------------------------------%

:- pred output_label_id(io.binary_output_stream::in, int::in,
    io::di, io::uo) is det.

output_label_id(BinaryOutputStream, LabelId, !IO) :-
    output_short(BinaryOutputStream, LabelId, !IO).

:- pred debug_label_id(io.text_output_stream::in, int::in,
    io::di, io::uo) is det.

debug_label_id(OutputStream, LabelId, !IO) :-
    debug_int(OutputStream, LabelId, !IO).

%---------------------------------------------------------------------------%

:- pred output_cons_id(io.binary_output_stream::in, byte_cons_id::in,
    io::di, io::uo) is det.

output_cons_id(BinaryOutputStream, ConsId, !IO) :-
    (
        ConsId = byte_cons(ModuleId, Functor, Arity, Tag),
        output_byte(BinaryOutputStream, 0, !IO),
        output_module_id(BinaryOutputStream, ModuleId, !IO),
        output_string(BinaryOutputStream, Functor, !IO),
        output_short(BinaryOutputStream, Arity, !IO),
        output_tag(BinaryOutputStream, Tag, !IO)
    ;
        ConsId = byte_int_const(IntVal),
        output_byte(BinaryOutputStream, 1, !IO),
        output_int(BinaryOutputStream, IntVal, !IO)
    ;
        ConsId = byte_string_const(StringVal),
        output_byte(BinaryOutputStream, 2, !IO),
        output_string(BinaryOutputStream, StringVal, !IO)
    ;
        ConsId = byte_float_const(FloatVal),
        output_byte(BinaryOutputStream, 3, !IO),
        output_float(BinaryOutputStream, FloatVal, !IO)
    ;
        ConsId = byte_pred_const(ModuleId, PredId, Arity, IsFunc, ProcId),
        output_byte(BinaryOutputStream, 4, !IO),
        output_module_id(BinaryOutputStream, ModuleId, !IO),
        output_pred_id(BinaryOutputStream, PredId, !IO),
        output_length(BinaryOutputStream, Arity, !IO),
        output_is_func(BinaryOutputStream, IsFunc, !IO),
        output_proc_id(BinaryOutputStream, ProcId, !IO)
    ;
        ConsId = byte_type_ctor_info_const(ModuleId, TypeName, TypeArity),
        output_byte(BinaryOutputStream, 6, !IO),
        output_module_id(BinaryOutputStream, ModuleId, !IO),
        output_string(BinaryOutputStream, TypeName, !IO),
        output_byte(BinaryOutputStream, TypeArity, !IO)
    ;
        ConsId = byte_char_const(Char),
        char.to_int(Char, Byte),
        output_byte(BinaryOutputStream, 7, !IO),
        output_byte(BinaryOutputStream, Byte, !IO)
    ;
        % XXX
        ConsId = byte_base_typeclass_info_const(_, _, _),
        output_byte(BinaryOutputStream, 8, !IO),
        sorry($pred, "bytecode for typeclass not yet implemented.")
    ;
        ConsId = byte_type_info_cell_constructor,
        output_byte(BinaryOutputStream, 9, !IO),
        sorry($pred, "bytecode for type_info_cell_constructor " ++
            "not yet implemented.")
    ;
        ConsId = byte_typeclass_info_cell_constructor,
        output_byte(BinaryOutputStream, 10, !IO),
        sorry($pred, "bytecode for typeclass_info_cell_constructor " ++
            "not yet implemented.")
    ).

:- pred debug_cons_id(io.text_output_stream::in, byte_cons_id::in,
    io::di, io::uo) is det.

debug_cons_id(OutputStream, ConsId, !IO) :-
    (
        ConsId = byte_cons(ModuleId, Functor, Arity, Tag),
        debug_string(OutputStream, "functor", !IO),
        debug_sym_name(OutputStream, ModuleId, !IO),
        debug_string(OutputStream, Functor, !IO),
        debug_int(OutputStream, Arity, !IO),
        debug_tag(OutputStream, Tag, !IO)
    ;
        ConsId = byte_int_const(IntVal),
        debug_string(OutputStream, "int_const", !IO),
        debug_int(OutputStream, IntVal, !IO)
    ;
        ConsId = byte_string_const(StringVal),
        debug_string(OutputStream, "string_const", !IO),
        debug_cstring(OutputStream, StringVal, !IO)
    ;
        ConsId = byte_float_const(FloatVal),
        debug_string(OutputStream, "float_const", !IO),
        debug_float(OutputStream, FloatVal, !IO)
    ;
        ConsId = byte_pred_const(ModuleId, PredId, Arity, IsFunc, ProcId),
        debug_string(OutputStream, "pred_const", !IO),
        debug_module_id(OutputStream, ModuleId, !IO),
        debug_pred_id(OutputStream, PredId, !IO),
        debug_length(OutputStream, Arity, !IO),
        debug_is_func(OutputStream, IsFunc, !IO),
        debug_proc_id(OutputStream, ProcId, !IO)
    ;
        ConsId = byte_type_ctor_info_const(ModuleId, TypeName, TypeArity),
        debug_string(OutputStream, "type_ctor_info_const", !IO),
        debug_module_id(OutputStream, ModuleId, !IO),
        debug_string(OutputStream, TypeName, !IO),
        debug_int(OutputStream, TypeArity, !IO)
    ;
        ConsId = byte_base_typeclass_info_const(ModuleId, ClassId, Instance),
        ClassId = class_id(ClassName, ClassArity),
        debug_string(OutputStream, "base_typeclass_info_const", !IO),
        debug_module_id(OutputStream, ModuleId, !IO),
        debug_string(OutputStream, "class_id", !IO),
        debug_sym_name(OutputStream, ClassName, !IO),
        debug_string(OutputStream, "/", !IO),
        debug_int(OutputStream, ClassArity, !IO),
        debug_string(OutputStream, Instance, !IO)
    ;
        ConsId = byte_char_const(Char),
        string.from_char_list([Char], String),
        debug_string(OutputStream, "char_const", !IO),
        debug_string(OutputStream, String, !IO)
    ;
        ConsId = byte_type_info_cell_constructor,
        debug_string(OutputStream, "type_info_cell_constructor", !IO)
    ;
        ConsId = byte_typeclass_info_cell_constructor,
        debug_string(OutputStream, "typeclass_info_cell_constructor", !IO)
    ).

%---------------------------------------------------------------------------%

:- pred output_tag(io.binary_output_stream::in, byte_cons_tag::in,
    io::di, io::uo) is det.

output_tag(OutputStream, ConsTag, !IO) :-
    (
        ConsTag = byte_unshared_tag(Primary),
        output_byte(OutputStream, 0, !IO),
        output_byte(OutputStream, Primary, !IO)
    ;
        ConsTag = byte_shared_remote_tag(Primary, Secondary),
        output_byte(OutputStream, 1, !IO),
        output_byte(OutputStream, Primary, !IO),
        output_int(OutputStream, Secondary, !IO)
    ;
        ConsTag = byte_shared_local_tag(Primary, Secondary),
        output_byte(OutputStream, 2, !IO),
        output_byte(OutputStream, Primary, !IO),
        output_int(OutputStream, Secondary, !IO)
    ;
        ConsTag = byte_enum_tag(Enum),
        output_byte(OutputStream, 3, !IO),
        output_byte(OutputStream, Enum, !IO)
    ;
        ConsTag = byte_no_tag,
        output_byte(OutputStream, 4, !IO)
    ).

:- pred debug_tag(io.text_output_stream::in, byte_cons_tag::in,
    io::di, io::uo) is det.

debug_tag(BinaryOutputStream, ConsTag, !IO) :-
    (
        ConsTag = byte_unshared_tag(Primary),
        debug_string(BinaryOutputStream, "unshared_tag", !IO),
        debug_int(BinaryOutputStream, Primary, !IO)
    ;
        ConsTag = byte_shared_remote_tag(Primary, Secondary),
        debug_string(BinaryOutputStream, "shared_remote_tag", !IO),
        debug_int(BinaryOutputStream, Primary, !IO),
        debug_int(BinaryOutputStream, Secondary, !IO)
    ;
        ConsTag = byte_shared_local_tag(Primary, Secondary),
        debug_string(BinaryOutputStream, "shared_local_tag", !IO),
        debug_int(BinaryOutputStream, Primary, !IO),
        debug_int(BinaryOutputStream, Secondary, !IO)
    ;
        ConsTag = byte_enum_tag(Enum),
        debug_string(BinaryOutputStream, "enum_tag", !IO),
        debug_int(BinaryOutputStream, Enum, !IO)
    ;
        ConsTag = byte_no_tag,
        debug_string(BinaryOutputStream, "no_tag", !IO)
    ).

%---------------------------------------------------------------------------%

:- pred output_binop(io.binary_output_stream::in, binary_op::in,
    io::di, io::uo) is det.

output_binop(OutputStream, Binop, !IO) :-
    binop_code(Binop, Code),
    output_byte(OutputStream, Code, !IO).

:- pred debug_binop(io.text_output_stream::in, binary_op::in,
    io::di, io::uo) is det.

debug_binop(BinaryOutputStream, Binop, !IO) :-
    binop_debug(Binop, Debug),
    debug_string(BinaryOutputStream, Debug, !IO).

%---------------------------------------------------------------------------%

:- pred output_unop(io.binary_output_stream::in, unary_op::in,
    io::di, io::uo) is det.

output_unop(BinaryOutputStream, Unop, !IO) :-
    unop_code(Unop, Code),
    output_byte(BinaryOutputStream, Code, !IO).

:- pred debug_unop(io.text_output_stream::in, unary_op::in,
    io::di, io::uo) is det.

debug_unop(OutputStream, Unop, !IO) :-
    unop_debug(Unop, Debug),
    debug_string(OutputStream, Debug, !IO).

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

:- pred binop_code(binary_op, int).
:- mode binop_code(in, out) is det.
% :- mode binop_code(out, in) is semidet.
% This mode would be a nice way to guard against accidentally assigning
% the same bytecode value to two different operators. Unfortunately,
% it would work only if we explicitly listed all possible integers
% as arguments of offset_str_eq, or if we used an output inst as big as
% this table.

% If we ever complete the bytecode backend, then this representation
% scheme should be simplified by putting related operations next
% to each other.
binop_code(int_add(int_type_int),    0).
binop_code(int_sub(int_type_int),    1).
binop_code(int_mul(int_type_int),    2).
binop_code(int_div(int_type_int),    3).
binop_code(int_mod(int_type_int),    4).
binop_code(unchecked_left_shift(int_type_int, shift_by_int),  5).
binop_code(unchecked_right_shift(int_type_int, shift_by_int), 6).
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
binop_code(float_add,               25).
binop_code(float_sub,               26).
binop_code(float_mul,               27).
binop_code(float_div,               28).
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
% was: binop_code(float_word_bits,         40).
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
binop_code(unchecked_left_shift(int_type_uint, shift_by_int), 59).
binop_code(unchecked_right_shift(int_type_uint, shift_by_int), 60).
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
binop_code(unchecked_left_shift(int_type_int8, shift_by_int), 75).
binop_code(unchecked_right_shift(int_type_int8, shift_by_int), 76).
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
binop_code(unchecked_left_shift(int_type_uint8, shift_by_int), 91).
binop_code(unchecked_right_shift(int_type_uint8, shift_by_int), 92).
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
binop_code(unchecked_left_shift(int_type_int16, shift_by_int),  107).
binop_code(unchecked_right_shift(int_type_int16, shift_by_int), 108).
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
binop_code(unchecked_left_shift(int_type_uint16, shift_by_int), 123).
binop_code(unchecked_right_shift(int_type_uint16, shift_by_int), 124).
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
binop_code(unchecked_left_shift(int_type_int32, shift_by_int), 139).
binop_code(unchecked_right_shift(int_type_int32, shift_by_int), 140).
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
binop_code(unchecked_left_shift(int_type_uint32, shift_by_int),  155).
binop_code(unchecked_right_shift(int_type_uint32, shift_by_int), 156).
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
binop_code(unchecked_left_shift(int_type_int64, shift_by_int), 171).
binop_code(unchecked_right_shift(int_type_int64, shift_by_int), 172).
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
binop_code(unchecked_left_shift(int_type_uint64, shift_by_int),  187).
binop_code(unchecked_right_shift(int_type_uint64, shift_by_int), 188).
binop_code(int64_from_dword,          189).
binop_code(uint64_from_dword,         190).
binop_code(unchecked_left_shift(int_type_int, shift_by_uint), 191).
binop_code(unchecked_right_shift(int_type_int, shift_by_uint), 192).
binop_code(unchecked_left_shift(int_type_uint, shift_by_uint), 193).
binop_code(unchecked_right_shift(int_type_uint, shift_by_uint), 194).
binop_code(unchecked_left_shift(int_type_int8, shift_by_uint), 195).
binop_code(unchecked_right_shift(int_type_int8, shift_by_uint), 196).
binop_code(unchecked_left_shift(int_type_uint8, shift_by_uint), 197).
binop_code(unchecked_right_shift(int_type_uint8, shift_by_uint), 198).
binop_code(unchecked_left_shift(int_type_int16, shift_by_uint), 199).
binop_code(unchecked_right_shift(int_type_int16, shift_by_uint), 200).
binop_code(unchecked_left_shift(int_type_uint16, shift_by_uint), 201).
binop_code(unchecked_right_shift(int_type_uint16, shift_by_uint), 202).
binop_code(unchecked_left_shift(int_type_int32, shift_by_uint), 203).
binop_code(unchecked_right_shift(int_type_int32, shift_by_uint), 204).
binop_code(unchecked_left_shift(int_type_uint32, shift_by_uint), 205).
binop_code(unchecked_right_shift(int_type_uint32, shift_by_uint), 206).
binop_code(unchecked_left_shift(int_type_int64, shift_by_uint), 207).
binop_code(unchecked_right_shift(int_type_int64, shift_by_uint), 208).
binop_code(unchecked_left_shift(int_type_uint64, shift_by_uint), 209).
binop_code(unchecked_right_shift(int_type_uint64, shift_by_uint), 210).
binop_code(unsigned_lt,               211).

:- pred binop_debug(binary_op, string).
:- mode binop_debug(in, out) is det.
% :- mode binop_debug(out, in) is semidet.
% This mode would be a nice way to guard against accidentally assigning
% the same debug representation to two different operators. Unfortunately,
% it would work only if we explicitly listed all possible integers
% as arguments of offset_str_eq, or if we used an output inst as big as
% this table.

binop_debug(int_add(int_type_int),  "+").
binop_debug(int_sub(int_type_int),  "-").
binop_debug(int_mul(int_type_int),  "*").
binop_debug(int_div(int_type_int),  "/").
binop_debug(int_mod(int_type_int),  "mod").
binop_debug(unchecked_left_shift(int_type_int, shift_by_int),   "<<").
binop_debug(unchecked_right_shift(int_type_int, shift_by_int),  ">>").
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
binop_debug(float_add,              "float_add").
binop_debug(float_sub,              "float_sub").
binop_debug(float_mul,              "float_mul").
binop_debug(float_div,              "float_div").
binop_debug(float_eq,               "float_eq").
binop_debug(float_ne,               "float_ne").
binop_debug(float_lt,               "float_lt").
binop_debug(float_gt,               "float_gt").
binop_debug(float_le,               "float_le").
binop_debug(float_ge,               "float_ge").
binop_debug(body,                   "body").
binop_debug(unsigned_lt,            "unsigned_lt").
binop_debug(unsigned_le,            "unsigned_le").
binop_debug(compound_eq,            "compound_eq").
binop_debug(compound_lt,            "compound_lt").
binop_debug(str_cmp,                "strcmp").
binop_debug(float_from_dword,       "float_from_dword").
binop_debug(int64_from_dword,       "int64_from_dword").
binop_debug(uint64_from_dword,      "uint64_from_dword").
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
binop_debug(unchecked_left_shift(int_type_uint, shift_by_int),
    "<<(uint)").
binop_debug(unchecked_right_shift(int_type_uint, shift_by_int),
    ">>(uint)").
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
binop_debug(unchecked_left_shift(int_type_int8, shift_by_int),
    "<<(int8)").
binop_debug(unchecked_right_shift(int_type_int8, shift_by_int),
    ">>(int8)").
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
binop_debug(unchecked_left_shift(int_type_uint8, shift_by_int),
    "<<(uint8)").
binop_debug(unchecked_right_shift(int_type_uint8, shift_by_int),
    ">>(uint8)").
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
binop_debug(unchecked_left_shift(int_type_int16, shift_by_int),
    "<<(int16)").
binop_debug(unchecked_right_shift(int_type_int16, shift_by_int),
    ">>(int16)").
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
binop_debug(unchecked_left_shift(int_type_uint16, shift_by_int),
    "<<(uint16)").
binop_debug(unchecked_right_shift(int_type_uint16, shift_by_int),
    ">>(uint16)").
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
binop_debug(unchecked_left_shift(int_type_int32, shift_by_int),
    "<<(int32)").
binop_debug(unchecked_right_shift(int_type_int32, shift_by_int),
    ">>(int32)").
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
binop_debug(unchecked_left_shift(int_type_uint32, shift_by_int),
    "<<(uint32)").
binop_debug(unchecked_right_shift(int_type_uint32, shift_by_int),
    ">>(uint32)").
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
binop_debug(unchecked_left_shift(int_type_int64, shift_by_int),
    "<<(int64)").
binop_debug(unchecked_right_shift(int_type_int64, shift_by_int),
    ">>(int64)").
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
binop_debug(unchecked_left_shift(int_type_uint64, shift_by_int),
    "<<(uint64)").
binop_debug(unchecked_right_shift(int_type_uint64, shift_by_int),
    ">>(uint64)").
binop_debug(unchecked_left_shift(int_type_int, shift_by_uint), "<<u").
binop_debug(unchecked_right_shift(int_type_int, shift_by_uint), ">>u").
binop_debug(unchecked_left_shift(int_type_uint, shift_by_uint), "<<u(uint)").
binop_debug(unchecked_right_shift(int_type_uint, shift_by_uint), ">>u(uint)").
binop_debug(unchecked_left_shift(int_type_int8, shift_by_uint), "<<u(int8)").
binop_debug(unchecked_right_shift(int_type_int8, shift_by_uint), ">>u(int8)").
binop_debug(unchecked_left_shift(int_type_uint8, shift_by_uint),
    "<<u(uint8)").
binop_debug(unchecked_right_shift(int_type_uint8, shift_by_uint),
    ">>u(uint8)").
binop_debug(unchecked_left_shift(int_type_int16, shift_by_uint),
    "<<u(int16)").
binop_debug(unchecked_right_shift(int_type_int16, shift_by_uint),
    ">>u(int16)").
binop_debug(unchecked_left_shift(int_type_uint16, shift_by_uint),
    "<<u(uint16)").
binop_debug(unchecked_right_shift(int_type_uint16, shift_by_uint),
    ">>u(uint16)").
binop_debug(unchecked_left_shift(int_type_int32, shift_by_uint),
    "<<u(int32)").
binop_debug(unchecked_right_shift(int_type_int32, shift_by_uint),
    ">>u(int32)").
binop_debug(unchecked_left_shift(int_type_uint32, shift_by_uint),
    "<<u(uint32)").
binop_debug(unchecked_right_shift(int_type_uint32, shift_by_uint),
    ">>u(uint32)").
binop_debug(unchecked_left_shift(int_type_int64, shift_by_uint),
    "<<u(int64)").
binop_debug(unchecked_right_shift(int_type_int64, shift_by_uint),
    ">>u(int64)").
binop_debug(unchecked_left_shift(int_type_uint64, shift_by_uint),
    "<<u(uint64)").
binop_debug(unchecked_right_shift(int_type_uint64, shift_by_uint),
    ">>u(uint64)").

:- pred unop_code(unary_op::in, int::out) is det.

% unop_code(mktag,              0).
unop_code(tag,                  1).
% unop_code(unmktag,            2).
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
unop_code(bitwise_complement(int_type_uint),    14).
unop_code(bitwise_complement(int_type_int8),    15).
unop_code(bitwise_complement(int_type_uint8),   16).
unop_code(bitwise_complement(int_type_int16),   17).
unop_code(bitwise_complement(int_type_uint16),  18).
unop_code(bitwise_complement(int_type_int32),   19).
unop_code(bitwise_complement(int_type_uint32),  20).
unop_code(bitwise_complement(int_type_int64),   21).
unop_code(bitwise_complement(int_type_uint64),  22).
unop_code(dword_float_get_word0,                23).
unop_code(dword_float_get_word1,                24).
unop_code(dword_int64_get_word0,                25).
unop_code(dword_int64_get_word1,                26).
unop_code(dword_uint64_get_word0,               27).
unop_code(dword_uint64_get_word1,               28).

:- pred unop_debug(unary_op::in, string::out) is det.

% unop_debug(mktag,             "mktag").
unop_debug(tag,                 "tag").
% unop_debug(unmktag,           "unmktag").
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
unop_debug(dword_float_get_word0,   "dword_float_get_word0").
unop_debug(dword_float_get_word1,   "dword_float_get_word1").
unop_debug(dword_int64_get_word0,   "dword_int64_get_word0").
unop_debug(dword_int64_get_word1,   "dword_int64_get_word1").
unop_debug(dword_uint64_get_word0,  "dword_uint64_get_word0").
unop_debug(dword_uint64_get_word1,  "dword_uint64_get_word1").
unop_debug(bitwise_complement(int_type_uint),   "bitwise_complement(uint)").
unop_debug(bitwise_complement(int_type_int8),   "bitwise_complement(int8)").
unop_debug(bitwise_complement(int_type_uint8),  "bitwise_complement(uint8)").
unop_debug(bitwise_complement(int_type_int16),  "bitwise_complement(int16)").
unop_debug(bitwise_complement(int_type_uint16), "bitwise_complement(uint16)").
unop_debug(bitwise_complement(int_type_int32),  "bitwise_complement(int32)").
unop_debug(bitwise_complement(int_type_uint32), "bitwise_complement(uint32)").
unop_debug(bitwise_complement(int_type_int64),  "bitwise_complement(int64)").
unop_debug(bitwise_complement(int_type_uint64), "bitwise_complement(uint64)").

%---------------------------------------------------------------------------%

    % debug_cstring prints a string quoted in the manner of C.
    %
:- pred debug_cstring(io.text_output_stream::in, string::in,
    io::di, io::uo) is det.

debug_cstring(OutputStream, Str, !IO) :-
    output_quoted_string_c(OutputStream, Str, !IO),
    % XXX: We need the trailing space in case something follows
    % the string as a bytecode argument. This is not very elegant.
    io.write_char(OutputStream, ' ', !IO).

:- pred debug_string(io.text_output_stream::in, string::in,
    io::di, io::uo) is det.

debug_string(OutputStream, Val, !IO) :-
    io.write_string(OutputStream, Val, !IO),
    io.write_char(OutputStream, ' ', !IO).

:- pred debug_int(io.text_output_stream::in, int::in,
    io::di, io::uo) is det.

debug_int(OutputStream, Val, !IO) :-
    io.write_int(OutputStream, Val, !IO),
    io.write_char(OutputStream, ' ', !IO).

:- pred debug_uint(io.text_output_stream::in, uint::in,
    io::di, io::uo) is det.

debug_uint(OutputStream, Val, !IO) :-
    io.write_uint(OutputStream, Val, !IO),
    io.write_char(OutputStream, ' ', !IO).

:- pred debug_int8(io.text_output_stream::in, int8::in,
    io::di, io::uo) is det.

debug_int8(OutputStream, Val, !IO) :-
    io.write_int8(OutputStream, Val, !IO),
    io.write_char(OutputStream, ' ', !IO).

:- pred debug_uint8(io.text_output_stream::in, uint8::in,
    io::di, io::uo) is det.

debug_uint8(OutputStream, Val, !IO) :-
    io.write_uint8(OutputStream, Val, !IO),
    io.write_char(OutputStream, ' ', !IO).

:- pred debug_int16(io.text_output_stream::in, int16::in,
    io::di, io::uo) is det.

debug_int16(OutputStream, Val, !IO) :-
    io.write_int16(OutputStream, Val, !IO),
    io.write_char(OutputStream, ' ', !IO).

:- pred debug_uint16(io.text_output_stream::in, uint16::in,
    io::di, io::uo) is det.

debug_uint16(OutputStream, Val, !IO) :-
    io.write_uint16(OutputStream, Val, !IO),
    io.write_char(OutputStream, ' ', !IO).

:- pred debug_int32(io.text_output_stream::in, int32::in,
    io::di, io::uo) is det.

debug_int32(OutputStream, Val, !IO) :-
    io.write_int32(OutputStream, Val, !IO),
    io.write_char(OutputStream, ' ', !IO).

:- pred debug_uint32(io.text_output_stream::in, uint32::in,
    io::di, io::uo) is det.

debug_uint32(OutputStream, Val, !IO) :-
    io.write_uint32(OutputStream, Val, !IO),
    io.write_char(OutputStream, ' ', !IO).

:- pred debug_int64(io.text_output_stream::in, int64::in,
    io::di, io::uo) is det.

debug_int64(OutputStream, Val, !IO) :-
    io.write_int64(OutputStream, Val, !IO),
    io.write_char(OutputStream, ' ', !IO).

:- pred debug_uint64(io.text_output_stream::in, uint64::in,
    io::di, io::uo) is det.

debug_uint64(OutputStream, Val, !IO) :-
    io.write_uint64(OutputStream, Val, !IO),
    io.write_char(OutputStream, ' ', !IO).

:- pred debug_float(io.text_output_stream::in, float::in,
    io::di, io::uo) is det.

debug_float(OutputStream, Val, !IO) :-
    io.write_float(OutputStream, Val, !IO),
    io.write_char(OutputStream, ' ', !IO).

:- pred debug_sym_name(io.text_output_stream::in, sym_name::in,
    io::di, io::uo) is det.

debug_sym_name(OutputStream, SymName, !IO) :-
    (
        SymName = unqualified(BaseName),
        io.write_string(OutputStream, BaseName, !IO),
        io.write_char(OutputStream, ' ', !IO)
    ;
        SymName = qualified(ModuleName, BaseName),
        debug_sym_name(OutputStream, ModuleName, !IO),
        io.write_char(OutputStream, '.', !IO),
        io.write_string(OutputStream, BaseName, !IO),
        io.write_char(OutputStream, ' ', !IO)
    ).

%---------------------------------------------------------------------------%
:- end_module bytecode_backend.bytecode.
%---------------------------------------------------------------------------%
