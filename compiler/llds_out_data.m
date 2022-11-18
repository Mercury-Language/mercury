%----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%----------------------------------------------------------------------------%
% Copyright (C) 2009-2012 The University of Melbourne.
% Copyright (C) 2013-2018 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%----------------------------------------------------------------------------%
%
% File: llds_out_data.m.
% Main authors: conway, fjh, zs.
%
% This module defines the routines for printing out LLDS lvals, rvals,
% and global variables.
%
%----------------------------------------------------------------------------%

:- module ll_backend.llds_out.llds_out_data.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_llds.
:- import_module ll_backend.llds.
:- import_module ll_backend.llds_out.llds_out_util.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module io.
:- import_module list.

%----------------------------------------------------------------------------%
%
% Lvals.
%

    % output_lval_decls(Lval, ...) outputs the declarations of any
    % static constants, etc. that need to be declared before
    % output_lval(Lval) is called.
    %
:- pred output_record_lval_decls(llds_out_info::in,
    io.text_output_stream::in, lval::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.
:- pred output_record_lval_decls_tab(llds_out_info::in,
    io.text_output_stream::in, lval::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

:- pred output_lval(llds_out_info::in, io.text_output_stream::in,
    lval::in, io::di, io::uo) is det.

:- pred output_lval_for_assign(llds_out_info::in, io.text_output_stream::in,
    lval::in, llds_type::out, io::di, io::uo) is det.

:- pred output_lval_as_word(llds_out_info::in, io.text_output_stream::in,
    lval::in, io::di, io::uo) is det.

    % Output the given llds_type with parentheses around it.
    %
:- pred output_llds_type_cast(io.text_output_stream::in, llds_type::in,
    io::di, io::uo) is det.

    % Output the given llds_type.
    %
:- pred output_llds_type(io.text_output_stream::in, llds_type::in,
    io::di, io::uo) is det.

    % Convert an lval to a string description of that lval.
    %
:- func lval_to_string(lval) = string is semidet.

    % Convert a register to a string description of that register.
    %
:- func reg_to_string(reg_type, int) = string.

:- func c_global_var_name(c_global_var_ref) = string.

%----------------------------------------------------------------------------%
%
% Rvals.
%

    % output_record_rval_decls(Info, Rval, !DeclSet) outputs the declarations
    % of any static constants, etc. that need to be declared before
    % output_rval(Rval) is called.
    %
:- pred output_record_rval_decls(llds_out_info::in,
    io.text_output_stream::in, rval::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.
:- pred output_record_rval_decls_tab(llds_out_info::in,
    io.text_output_stream::in, rval::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

:- pred output_record_rvals_decls(llds_out_info::in, io.text_output_stream::in,
    list(rval)::in, decl_set::in, decl_set::out, io::di, io::uo) is det.

    % Output an rval, not converted to any particular type,
    % but instead output as its "natural" type.
    %
:- pred output_rval(llds_out_info::in, rval::in, io.text_output_stream::in,
    io::di, io::uo) is det.

    % Output an rval, converted to the specified type
    %
:- pred output_rval_as_type(llds_out_info::in, rval::in, llds_type::in,
    io.text_output_stream::in, io::di, io::uo) is det.

:- pred output_test_rval(llds_out_info::in, rval::in,
    io.text_output_stream::in, io::di, io::uo) is det.

    % Write out the given ptag, wrapped up in MR_mktag(_).
    %
:- pred output_ptag(io.text_output_stream::in, ptag::in,
    io::di, io::uo) is det.

    % Write out the given ptag.
    %
:- pred write_ptag(io.text_output_stream::in, ptag::in, io::di, io::uo) is det.

    % Return true iff an integer constant can be used directly as a value
    % in a structure field of the given type, instead of being cast to
    % MR_Integer first and then to the type. The answer can be
    % conservative: it is always ok to return `no'.
    %
    % Only the compiler generates values of the uint_leastN types,
    % and for these the constant will never be negative.
    %
:- func direct_field_int_constant(llds_type) = bool.

%----------------------------------------------------------------------------%
%
% Global data.
%

    % Given an rval, succeed iff it is a floating point constant expression;
    % if so, return a name for that rval that is suitable for use in a C
    % identifier. Different rvals must be given different names.
    %
:- pred float_const_expr_name(rval::in, string::out) is semidet.

:- pred int64_const_expr_name(rval::in, string::out) is semidet.

:- pred uint64_const_expr_name(rval::in, string::out) is semidet.

    % output_record_data_addr_decls(Info, DataId, ...) outputs the
    % declarations of any static constants, etc. that need to be declared
    % before output_data_id(Info DataId, ...) is called.
    %
:- pred output_record_data_id_decls(llds_out_info::in,
    io.text_output_stream::in, data_id::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.
:- pred output_record_data_id_decls_format(llds_out_info::in,
    io.text_output_stream::in, data_id::in, string::in, string::in,
    int::in, int::out, decl_set::in, decl_set::out, io::di, io::uo) is det.

    % Output the name of the global variable identified by the data_id.
    %
:- pred output_data_id(llds_out_info::in, io.text_output_stream::in,
    data_id::in, io::di, io::uo) is det.

    % Output the address of the global variable identified by the data_id.
    %
:- pred output_data_id_addr(llds_out_info::in, io.text_output_stream::in,
    data_id::in, io::di, io::uo) is det.

:- pred output_common_scalar_cell_array_name(io.text_output_stream::in,
    type_num::in, io::di, io::uo) is det.

:- pred output_common_vector_cell_array_name(io.text_output_stream::in,
    type_num::in, int::in, io::di, io::uo) is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.builtin_ops.
:- import_module backend_libs.c_util.
:- import_module backend_libs.name_mangle.
:- import_module backend_libs.rtti.
:- import_module libs.
:- import_module libs.optimization_options.
:- import_module ll_backend.layout_out.
:- import_module ll_backend.llds_out.llds_out_code_addr.
:- import_module ll_backend.rtti_out.
:- import_module mdbcomp.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_foreign.

:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module string.
:- import_module uint.
:- import_module uint16.
:- import_module uint8.

%----------------------------------------------------------------------------%
%
% Declaring lvals.
%

output_record_lval_decls(Info, Stream, Lval, !DeclSet, !IO) :-
    output_record_lval_decls_format(Info, Stream, Lval,
        "", "", 0, _, !DeclSet, !IO).

output_record_lval_decls_tab(Info, Stream, Lval, !DeclSet, !IO) :-
    output_record_lval_decls_format(Info, Stream, Lval,
        "\t", "\t", 0, _, !DeclSet, !IO).

:- pred output_record_lval_decls_format(llds_out_info::in,
    io.text_output_stream::in, lval::in,
    string::in, string::in, int::in, int::out, decl_set::in, decl_set::out,
    io::di, io::uo) is det.

output_record_lval_decls_format(Info, Stream, Lval, FirstIndent, LaterIndent,
        !N, !DeclSet, !IO) :-
    (
        Lval = field(_, Rval, FieldNum),
        output_record_rval_decls_format(Info, Stream, Rval,
            FirstIndent, LaterIndent, !N, !DeclSet, !IO),
        output_record_rval_decls_format(Info, Stream, FieldNum,
            FirstIndent, LaterIndent, !N, !DeclSet, !IO)
    ;
        ( Lval = succfr_slot(Rval)
        ; Lval = prevfr_slot(Rval)
        ; Lval = redofr_slot(Rval)
        ; Lval = redoip_slot(Rval)
        ; Lval = succip_slot(Rval)
        ; Lval = mem_ref(Rval)
        ),
        output_record_rval_decls_format(Info, Stream, Rval,
            FirstIndent, LaterIndent, !N, !DeclSet, !IO)
    ;
        ( Lval = reg(_, _)
        ; Lval = stackvar(_)
        ; Lval = parent_stackvar(_)
        ; Lval = framevar(_)
        ; Lval = double_stackvar(_, _)
        ; Lval = succip
        ; Lval = maxfr
        ; Lval = curfr
        ; Lval = hp
        ; Lval = sp
        ; Lval = parent_sp
        ; Lval = lvar(_)
        ; Lval = temp(_, _)
        )
    ;
        Lval = global_var_ref(CGlobalVar),
        ( if decl_set_is_member(decl_c_global_var(CGlobalVar), !.DeclSet) then
            true
        else
            % All env_var_ref global_var_refs should have been output by
            % output_c_procedure_decls already, and as of now there are no
            % other global_var_refs.
            unexpected($file, $pred, "global_var_ref")
        )
    ).

%----------------------------------------------------------------------------%
%
% Writing lvals.
%

output_lval(Info, Stream, Lval, !IO) :-
    (
        Lval = reg(Type, Num),
        output_reg(Stream, Type, Num, !IO)
    ;
        Lval = stackvar(N),
        ( if N =< 0 then
            unexpected($file, $pred, "stack var out of range")
        else
            true
        ),
        io.format(Stream, "MR_sv(%d)", [i(N)], !IO)
    ;
        Lval = parent_stackvar(N),
        ( if N =< 0 then
            unexpected($file, $pred, "parent stack var out of range")
        else
            true
        ),
        io.format(Stream, "MR_parent_sv(%d)", [i(N)], !IO)
    ;
        Lval = framevar(N),
        ( if N =< 0 then
            unexpected($file, $pred, "frame var out of range")
        else
            true
        ),
        io.format(Stream, "MR_fv(%d)", [i(N)], !IO)
    ;
        Lval = double_stackvar(StackType, SlotNum),
        io.write_string(Stream, "MR_float_from_dword_ptr(", !IO),
        output_double_stackvar_ptr(Info, Stream, StackType, SlotNum, !IO),
        io.write_string(Stream, ")", !IO)
    ;
        Lval = succip,
        io.write_string(Stream, "MR_succip", !IO)
    ;
        Lval = sp,
        io.write_string(Stream, "MR_sp", !IO)
    ;
        Lval = parent_sp,
        io.write_string(Stream, "MR_parent_sp", !IO)
    ;
        Lval = hp,
        io.write_string(Stream, "MR_hp", !IO)
    ;
        Lval = maxfr,
        io.write_string(Stream, "MR_maxfr", !IO)
    ;
        Lval = curfr,
        io.write_string(Stream, "MR_curfr", !IO)
    ;
        Lval = succfr_slot(Rval),
        io.write_string(Stream, "MR_succfr_slot(", !IO),
        output_rval(Info, Rval, Stream, !IO),
        io.write_string(Stream, ")", !IO)
    ;
        Lval = prevfr_slot(Rval),
        io.write_string(Stream, "MR_prevfr_slot(", !IO),
        output_rval(Info, Rval, Stream, !IO),
        io.write_string(Stream, ")", !IO)
    ;
        Lval = redofr_slot(Rval),
        io.write_string(Stream, "MR_redofr_slot(", !IO),
        output_rval(Info, Rval, Stream, !IO),
        io.write_string(Stream, ")", !IO)
    ;
        Lval = redoip_slot(Rval),
        io.write_string(Stream, "MR_redoip_slot(", !IO),
        output_rval(Info, Rval, Stream, !IO),
        io.write_string(Stream, ")", !IO)
    ;
        Lval = succip_slot(Rval),
        io.write_string(Stream, "MR_succip_slot(", !IO),
        output_rval(Info, Rval, Stream, !IO),
        io.write_string(Stream, ")", !IO)
    ;
        Lval = field(MaybePtag, Rval, FieldNumRval),
        (
            MaybePtag = yes(ptag(PtagUInt8)),
            io.format(Stream, "MR_tfield(%u, ", [u8(PtagUInt8)], !IO)
        ;
            MaybePtag = no,
            io.write_string(Stream, "MR_mask_field(", !IO)
        ),
        output_rval(Info, Rval, Stream, !IO),
        io.write_string(Stream, ", ", !IO),
        ( if FieldNumRval = const(llconst_int(FieldNum)) then
            % Avoid emitting the (MR_Integer) cast.
            io.write_int(Stream, FieldNum, !IO)
        else
            output_rval(Info, FieldNumRval, Stream, !IO)
        ),
        io.write_string(Stream, ")", !IO)
    ;
        Lval = lvar(_),
        unexpected($file, $pred, "lvar")
    ;
        Lval = temp(Type, Num),
        (
            Type = reg_r,
            io.write_string(Stream, "MR_tempr", !IO),
            io.write_int(Stream, Num, !IO)
        ;
            Type = reg_f,
            io.write_string(Stream, "MR_tempf", !IO),
            io.write_int(Stream, Num, !IO)
        )
    ;
        Lval = mem_ref(Rval),
        io.write_string(Stream, "* (MR_Word *) (", !IO),
        output_rval(Info, Rval, Stream, !IO),
        io.write_string(Stream, ")", !IO)
    ;
        Lval = global_var_ref(GlobalVar),
        io.write_string(Stream, c_global_var_name(GlobalVar), !IO)
    ).

output_lval_for_assign(Info, Stream, Lval, Type, !IO) :-
    (
        Lval = reg(RegType, Num),
        (
            RegType = reg_r,
            Type = lt_word
        ;
            RegType = reg_f,
            Type = lt_float
        ),
        output_reg(Stream, RegType, Num, !IO)
    ;
        Lval = stackvar(N),
        Type = lt_word,
        ( if N < 0 then
            unexpected($file, $pred, "stack var out of range")
        else
            true
        ),
        io.format(Stream, "MR_sv(%d)", [i(N)], !IO)
    ;
        Lval = parent_stackvar(N),
        Type = lt_word,
        ( if N < 0 then
            unexpected($file, $pred, "parent stack var out of range")
        else
            true
        ),
        io.format(Stream, "MR_parent_sv(%d)", [i(N)], !IO)
    ;
        Lval = framevar(N),
        Type = lt_word,
        ( if N =< 0 then
            unexpected($file, $pred, "frame var out of range")
        else
            true
        ),
        io.format(Stream, "MR_fv(%d)", [i(N)], !IO)
    ;
        Lval = double_stackvar(StackType, SlotNum),
        Type = lt_float,
        io.write_string(Stream, "* (MR_Float *) ", !IO),
        output_double_stackvar_ptr(Info, Stream, StackType, SlotNum, !IO)
    ;
        Lval = succip,
        Type = lt_word,
        io.write_string(Stream, "MR_succip_word", !IO)
    ;
        Lval = sp,
        Type = lt_word,
        io.write_string(Stream, "MR_sp_word", !IO)
    ;
        Lval = parent_sp,
        Type = lt_data_ptr,
        io.write_string(Stream, "MR_parent_sp", !IO)
    ;
        Lval = hp,
        Type = lt_word,
        io.write_string(Stream, "MR_hp_word", !IO)
    ;
        Lval = maxfr,
        Type = lt_word,
        io.write_string(Stream, "MR_maxfr_word", !IO)
    ;
        Lval = curfr,
        Type = lt_word,
        io.write_string(Stream, "MR_curfr_word", !IO)
    ;
        Lval = succfr_slot(Rval),
        Type = lt_word,
        io.write_string(Stream, "MR_succfr_slot_word(", !IO),
        output_rval(Info, Rval, Stream, !IO),
        io.write_string(Stream, ")", !IO)
    ;
        Lval = prevfr_slot(Rval),
        Type = lt_word,
        io.write_string(Stream, "MR_prevfr_slot_word(", !IO),
        output_rval(Info, Rval, Stream, !IO),
        io.write_string(Stream, ")", !IO)
    ;
        Lval = redofr_slot(Rval),
        Type = lt_word,
        io.write_string(Stream, "MR_redofr_slot_word(", !IO),
        output_rval(Info, Rval, Stream, !IO),
        io.write_string(Stream, ")", !IO)
    ;
        Lval = redoip_slot(Rval),
        Type = lt_word,
        io.write_string(Stream, "MR_redoip_slot_word(", !IO),
        output_rval(Info, Rval, Stream, !IO),
        io.write_string(Stream, ")", !IO)
    ;
        Lval = succip_slot(Rval),
        Type = lt_word,
        io.write_string(Stream, "MR_succip_slot_word(", !IO),
        output_rval(Info, Rval, Stream, !IO),
        io.write_string(Stream, ")", !IO)
    ;
        Lval = field(MaybePtag, Rval, FieldNumRval),
        Type = lt_word,
        (
            MaybePtag = yes(ptag(PtagUInt8)),
            io.format(Stream, "MR_tfield(%u, ", [u8(PtagUInt8)], !IO)
        ;
            MaybePtag = no,
            io.write_string(Stream, "MR_mask_field(", !IO)
        ),
        output_rval(Info, Rval, Stream, !IO),
        io.write_string(Stream, ", ", !IO),
        ( if FieldNumRval = const(llconst_int(FieldNum)) then
            % Avoid emitting the (MR_Integer) cast.
            io.write_int(Stream, FieldNum, !IO)
        else
            output_rval(Info, FieldNumRval, Stream, !IO)
        ),
        io.write_string(Stream, ")", !IO)
    ;
        Lval = lvar(_),
        unexpected($file, $pred, "lvar")
    ;
        Lval = temp(RegType, Num),
        (
            RegType = reg_r,
            Type = lt_word,
            io.write_string(Stream, "MR_tempr", !IO),
            io.write_int(Stream, Num, !IO)
        ;
            RegType = reg_f,
            Type = lt_float,
            io.write_string(Stream, "MR_tempf", !IO),
            io.write_int(Stream, Num, !IO)
        )
    ;
        Lval = mem_ref(_MemRef),
        Type = lt_word,
        output_lval(Info, Stream, Lval, !IO)
    ;
        Lval = global_var_ref(GlobalVar),
        Type = lt_word,
        io.write_string(Stream, c_global_var_name(GlobalVar), !IO)
    ).

output_lval_as_word(Info, Stream, Lval, !IO) :-
    llds.lval_type(Lval, ActualType),
    ( if llds_types_match(lt_word, ActualType) then
        output_lval(Info, Stream, Lval, !IO)
    else if ActualType = lt_float then
        % Sanity check -- if this happens, the LLDS is ill-typed.
        unexpected($file, $pred, "float")
    else
        io.write_string(Stream, "MR_LVALUE_CAST(MR_Word,", !IO),
        output_lval(Info, Stream, Lval, !IO),
        io.write_string(Stream, ")", !IO)
    ).

:- pred output_double_stackvar_ptr(llds_out_info::in,
    io.text_output_stream::in, double_stack_type::in, int::in,
    io::di, io::uo) is det.

output_double_stackvar_ptr(Info, Stream, StackType, SlotNum, !IO) :-
    % The higher-numbered slot has the lower address because our stacks grow
    % downwards.
    (
        StackType = double_stackvar,
        Lval = stackvar(SlotNum + 1)
    ;
        StackType = double_parent_stackvar,
        Lval = parent_stackvar(SlotNum + 1)
    ),
    io.write_string(Stream, "MR_dword_ptr(&(", !IO),
    output_lval(Info, Stream, Lval, !IO),
    io.write_string(Stream, "))", !IO).

    % llds_types_match(DesiredType, ActualType) is true iff
    % a value of type ActualType can be used as a value of
    % type DesiredType without casting.
    %
:- pred llds_types_match(llds_type::in, llds_type::in) is semidet.

llds_types_match(Type, Type).
llds_types_match(lt_word, lt_int(int_type_int)).
llds_types_match(lt_word, lt_int(int_type_uint)).
llds_types_match(lt_word, lt_bool).
llds_types_match(lt_bool, lt_int(int_type_int)).
llds_types_match(lt_bool, lt_int(int_type_uint)).
llds_types_match(lt_bool, lt_word).
llds_types_match(lt_int(int_type_int), lt_bool).

output_llds_type_cast(Stream, LLDSType, !IO) :-
    io.write_string(Stream, "(", !IO),
    output_llds_type(Stream, LLDSType, !IO),
    io.write_string(Stream, ") ", !IO).

output_llds_type(Stream, lt_bool, !IO) :-
    io.write_string(Stream, "MR_Integer", !IO).
output_llds_type(Stream, lt_int_least(int_least8), !IO) :-
    io.write_string(Stream, "MR_int_least8_t", !IO).
output_llds_type(Stream, lt_int_least(uint_least8), !IO) :-
    io.write_string(Stream, "MR_uint_least8_t", !IO).
output_llds_type(Stream, lt_int_least(int_least16), !IO) :-
    io.write_string(Stream, "MR_int_least16_t", !IO).
output_llds_type(Stream, lt_int_least(uint_least16), !IO) :-
    io.write_string(Stream, "MR_uint_least16_t", !IO).
output_llds_type(Stream, lt_int_least(int_least32), !IO) :-
    io.write_string(Stream, "MR_int_least32_t", !IO).
output_llds_type(Stream, lt_int_least(uint_least32), !IO) :-
    io.write_string(Stream, "MR_uint_least32_t", !IO).
output_llds_type(Stream, lt_int(int_type_int), !IO) :-
    io.write_string(Stream, "MR_Integer", !IO).
output_llds_type(Stream, lt_int(int_type_uint), !IO) :-
    io.write_string(Stream, "MR_Unsigned", !IO).
output_llds_type(Stream, lt_int(int_type_int8), !IO) :-
    io.write_string(Stream, "int8_t", !IO).
output_llds_type(Stream, lt_int(int_type_uint8), !IO) :-
    io.write_string(Stream, "uint8_t", !IO).
output_llds_type(Stream, lt_int(int_type_int16), !IO) :-
    io.write_string(Stream, "int16_t", !IO).
output_llds_type(Stream, lt_int(int_type_uint16), !IO) :-
    io.write_string(Stream, "uint16_t", !IO).
output_llds_type(Stream, lt_int(int_type_int32), !IO) :-
    io.write_string(Stream, "int32_t", !IO).
output_llds_type(Stream, lt_int(int_type_uint32), !IO) :-
    io.write_string(Stream, "uint32_t", !IO).
output_llds_type(Stream, lt_int(int_type_int64), !IO) :-
    io.write_string(Stream, "int64_t", !IO).
output_llds_type(Stream, lt_int(int_type_uint64), !IO) :-
    io.write_string(Stream, "uint64_t", !IO).
output_llds_type(Stream, lt_float, !IO) :-
    io.write_string(Stream, "MR_Float", !IO).
output_llds_type(Stream, lt_word, !IO) :-
    io.write_string(Stream, "MR_Word", !IO).
output_llds_type(Stream, lt_string, !IO) :-
    io.write_string(Stream, "MR_String", !IO).
output_llds_type(Stream, lt_data_ptr, !IO) :-
    io.write_string(Stream, "MR_Word *", !IO).
output_llds_type(Stream, lt_code_ptr, !IO) :-
    io.write_string(Stream, "MR_Code *", !IO).

lval_to_string(reg(RegType, RegNum)) =
    reg_to_string(RegType, RegNum).
lval_to_string(framevar(N)) =
    "MR_fv(" ++ int_to_string(N) ++ ")".
lval_to_string(stackvar(N)) =
    "MR_sv(" ++ int_to_string(N) ++ ")".
lval_to_string(parent_stackvar(N)) =
    "MR_parent_sv(" ++ int_to_string(N) ++ ")".
lval_to_string(double_stackvar(Type, N)) = String :-
    (
        Type = double_stackvar,
        Macro = "MR_sv"
    ;
        Type = double_parent_stackvar,
        Macro = "MR_parent_sv"
    ),
    string.format("%s(%d,%d)", [s(Macro), i(N), i(N + 1)], String).

reg_to_string(reg_r, N) =
    ( if N =< max_real_r_reg then
        "MR_r" ++ int_to_string(N)
    else if N =< max_virtual_r_reg then
        "MR_r(" ++ int_to_string(N) ++ ")"
    else
        unexpected($file, $pred, "register number too large")
    ).
reg_to_string(reg_f, N) =
    ( if N =< max_virtual_f_reg then
        "MR_f(" ++ int_to_string(N) ++ ")"
    else
        unexpected($file, $pred, "register number too large")
    ).

:- func max_real_r_reg = int.
:- func max_virtual_r_reg = int.

max_real_r_reg = 32.
max_virtual_r_reg = 1024.

:- func max_virtual_f_reg = int.

max_virtual_f_reg = 1024.

:- pred output_reg(io.text_output_stream::in, reg_type::in, int::in,
    io::di, io::uo) is det.

output_reg(Stream, RegType, N, !IO) :-
    io.write_string(Stream, reg_to_string(RegType, N), !IO).

% The calls to env_var_is_acceptable_char in parse_goal.m ensure that
% EnvVarName is acceptable as part of a C identifier.
% The prefix must be identical to envvar_prefix in util/mkinit.c and
% global_var_name in mlds_to_c.m.
c_global_var_name(env_var_ref(EnvVarName)) = "mercury_envvar_" ++ EnvVarName.

%----------------------------------------------------------------------------%
%
% Declaring rvals.
%

output_record_rval_decls(Info, Stream, Rval, !DeclSet, !IO) :-
    output_record_rval_decls_format(Info, Stream, Rval,
        "", "", 0, _, !DeclSet, !IO).

output_record_rval_decls_tab(Info, Stream, Rval, !DeclSet, !IO) :-
    output_record_rval_decls_format(Info, Stream, Rval,
        "", "\t", 0, _, !DeclSet, !IO).

    % output_record_rval_decls_format(Info, Rval, FirstIndent, LaterIndent,
    %   !N, !DeclSet, !IO)
    %
    % Outputs the declarations of any static constants, etc. that need to be
    % declared before output_rval(Rval) is called. FirstIndent is output
    % before the first declaration, while LaterIndent is output before
    % all later declaration; N0 and N give the number of declarations output
    % before and after this call.
    %
    % Every time we emit a declaration for a symbol, we insert it into the
    % set of symbols we have already declared. That way, we avoid generating
    % the same symbol twice, which would cause an error in the C code.
    %
:- pred output_record_rval_decls_format(llds_out_info::in,
    io.text_output_stream::in, rval::in,
    string::in, string::in, int::in, int::out,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_record_rval_decls_format(Info, Stream, Rval, FirstIndent, LaterIndent,
        !N, !DeclSet, !IO) :-
    (
        Rval = lval(Lval),
        output_record_lval_decls_format(Info, Stream, Lval,
            FirstIndent, LaterIndent, !N, !DeclSet, !IO)
    ;
        Rval = var(_),
        unexpected($file, $pred, "var")
    ;
        Rval = mkword_hole(_)
    ;
        Rval = const(Const),
        (
            Const = llconst_code_addr(CodeAddress),
            output_record_code_addr_decls_format(Info, Stream, CodeAddress,
                FirstIndent, LaterIndent, !N, !DeclSet, !IO)
        ;
            Const = llconst_data_addr(DataId, _),
            output_record_data_id_decls_format(Info, Stream, DataId,
                FirstIndent, LaterIndent, !N, !DeclSet, !IO)
        ;
            Const = llconst_float(FloatVal),
            % If floats are boxed, but are allocated statically, then for each
            % float constant which we might want to box, we declare a static
            % const variable holding that constant.
            UnboxedFloat = Info ^ lout_unboxed_float,
            StaticGroundFloats = Info ^ lout_static_ground_floats,
            ( if
                UnboxedFloat = no,
                StaticGroundFloats = use_static_ground_floats,
                float_literal_name(FloatVal, FloatName),
                FloatLabel = decl_float_label(FloatName),
                decl_set_insert_new(FloatLabel, !DeclSet)
            then
                FloatValStr = c_util.make_float_literal(FloatVal),
                output_indent(Stream, FirstIndent, LaterIndent, !.N, !IO),
                !:N = !.N + 1,
                io.format(Stream,
                    "static const MR_Float mercury_float_const_%s = %s;\n",
                    [s(FloatName), s(FloatValStr)], !IO)
            else
                true
            )
        ;
            Const = llconst_int64(Int64Val),
            UnboxedInt64s = Info ^ lout_unboxed_int64s,
            StaticGroundInt64s = Info ^ lout_static_ground_int64s,
            ( if
                UnboxedInt64s = no,
                StaticGroundInt64s = use_static_ground_int64s,
                Int64Label = decl_int64_label(Int64Val),
                decl_set_insert_new(Int64Label, !DeclSet)
            then
                int64_literal_name(Int64Val, Int64Name),
                Int64ValStr = c_util.make_int64_literal(Int64Val),
                output_indent(Stream, FirstIndent, LaterIndent, !.N, !IO),
                !:N = !.N + 1,
                io.format(Stream,
                    "static const int64_t mercury_int64_const_%s = %s;\n",
                    [s(Int64Name), s(Int64ValStr)], !IO)
            else
                true
            )
        ;
            Const = llconst_uint64(UInt64Val),
            UnboxedInt64s = Info ^ lout_unboxed_int64s,
            StaticGroundInt64s = Info ^ lout_static_ground_int64s,
            ( if
                UnboxedInt64s = no,
                StaticGroundInt64s = use_static_ground_int64s,
                UInt64Label = decl_uint64_label(UInt64Val),
                decl_set_insert_new(UInt64Label, !DeclSet)
            then
                decl_set_insert(UInt64Label, !DeclSet),
                uint64_literal_name(UInt64Val, UInt64Name),
                UInt64ValStr = c_util.make_uint64_literal(UInt64Val),
                output_indent(Stream, FirstIndent, LaterIndent, !.N, !IO),
                !:N = !.N + 1,
                io.format(Stream,
                    "static const uint64_t mercury_uint64_const_%s = %s;\n",
                    [s(UInt64Name), s(UInt64ValStr)], !IO)
            else
                true
            )
        ;
            ( Const = llconst_true
            ; Const = llconst_false
            ; Const = llconst_int(_)
            ; Const = llconst_uint(_)
            ; Const = llconst_int8(_)
            ; Const = llconst_uint8(_)
            ; Const = llconst_int16(_)
            ; Const = llconst_uint16(_)
            ; Const = llconst_int32(_)
            ; Const = llconst_uint32(_)
            ; Const = llconst_foreign(_, _)
            ; Const = llconst_string(_)
            ; Const = llconst_multi_string(_)
            )
        )
    ;
        ( Rval = mkword(_, SubRval)
        ; Rval = cast(_, SubRval)
        ; Rval = unop(_, SubRval)
        ),
        output_record_rval_decls_format(Info, Stream, SubRval,
            FirstIndent, LaterIndent, !N, !DeclSet, !IO)
    ;
        Rval = binop(Op, SubRvalA, SubRvalB),
        output_record_rval_decls_format(Info, Stream, SubRvalA,
            FirstIndent, LaterIndent, !N, !DeclSet, !IO),
        output_record_rval_decls_format(Info, Stream, SubRvalB,
            FirstIndent, LaterIndent, !N, !DeclSet, !IO),

        % If floats are boxed, and the static ground terms option is enabled,
        % then for each float constant which we might want to box, we declare
        % a static const variable holding that constant.
        (
            ( Op = float_add, OpStr = "+"
            ; Op = float_sub, OpStr = "-"
            ; Op = float_mul, OpStr = "*"
            ; Op = float_div, OpStr = "/"
            ),
            UnboxFloat = Info ^ lout_unboxed_float,
            StaticGroundFloats = Info ^ lout_static_ground_floats,
            ( if
                UnboxFloat = no,
                StaticGroundFloats = use_static_ground_floats,
                float_const_binop_expr_name(Op, SubRvalA, SubRvalB, FloatName),
                FloatLabel = decl_float_label(FloatName),
                decl_set_insert_new(FloatLabel, !DeclSet)
            then
                output_indent(Stream, FirstIndent, LaterIndent, !.N, !IO),
                !:N = !.N + 1,
                io.write_string(Stream, "static const ", !IO),
                output_llds_type(Stream, lt_float, !IO),
                io.write_string(Stream, " mercury_float_const_", !IO),
                io.write_string(Stream, FloatName, !IO),
                io.write_string(Stream, " = ", !IO),
                % Note that we just output the expression here, and
                % let the C compiler evaluate it, rather than evaluating
                % it ourselves. This avoids having to deal with some nasty
                % issues regarding floating point accuracy when doing
                % cross-compilation.
                output_rval_as_type(Info, SubRvalA, lt_float, Stream, !IO),
                io.write_string(Stream, " ", !IO),
                io.write_string(Stream, OpStr, !IO),
                io.write_string(Stream, " ", !IO),
                output_rval_as_type(Info, SubRvalB, lt_float, Stream, !IO),
                io.write_string(Stream, ";\n", !IO)
            else
                true
            )
        ;
            ( Op = array_index(_)
            ; Op = string_unsafe_index_code_unit
            ; Op = pointer_equal_conservative
            ; Op = compound_lt
            ; Op = compound_eq
            ; Op = str_eq
            ; Op = str_ne
            ; Op = str_le
            ; Op = str_ge
            ; Op = str_lt
            ; Op = str_gt
            ; Op = unsigned_lt
            ; Op = unsigned_le
            ; Op = float_eq
            ; Op = float_ne
            ; Op = float_le
            ; Op = float_ge
            ; Op = float_lt
            ; Op = float_gt
            ; Op = int_add(_)
            ; Op = int_sub(_)
            ; Op = int_mul(_)
            ; Op = int_div(_)
            ; Op = unchecked_left_shift(_, _)
            ; Op = unchecked_right_shift(_, _)
            ; Op = bitwise_and(_)
            ; Op = bitwise_or(_)
            ; Op = bitwise_xor(_)
            ; Op = int_mod(_)
            ; Op = eq(_)
            ; Op = ne(_)
            ; Op = logical_and
            ; Op = logical_or
            ; Op = int_lt(_)
            ; Op = int_gt(_)
            ; Op = int_le(_)
            ; Op = int_ge(_)
            ; Op = str_cmp
            ; Op = offset_str_eq(_)
            ; Op = body
            ; Op = float_from_dword
            ; Op = int64_from_dword
            ; Op = uint64_from_dword
            )
        )
    ;
        Rval = mem_addr(MemRef),
        output_record_mem_ref_decls_format(Info, Stream, MemRef,
            FirstIndent, LaterIndent, !N, !DeclSet, !IO)
    ).

output_record_rvals_decls(Info, Stream, Rvals, !DeclSet, !IO) :-
    output_record_rvals_decls_format(Info, Stream, Rvals, "", "", 0, _,
        !DeclSet, !IO).

:- pred output_record_rvals_decls_format(llds_out_info::in,
    io.text_output_stream::in, list(rval)::in,
    string::in, string::in, int::in, int::out, decl_set::in,
    decl_set::out, io::di, io::uo) is det.

output_record_rvals_decls_format(_, _, [], _, _, !N, !DeclSet, !IO).
output_record_rvals_decls_format(Info, Stream, Rvals @ [_ | _],
        FirstIndent, LaterIndent, !N, !DeclSet, !IO) :-
    output_record_rvals_decls_format_count(Info, Stream, Rvals, LeftOverRvals,
        1000, FirstIndent, LaterIndent, !N, !DeclSet, !IO),
    output_record_rvals_decls_format(Info, Stream, LeftOverRvals,
        FirstIndent, LaterIndent, !N, !DeclSet, !IO).

    % We use this predicate to output the declarations of up to Count rvals.
    % It is separate from output_record_rvals_decls_format so that in grades
    % that do not permit tail recursion, we can free up the stack frames
    % occupied by a bunch of loop iterations before the declarations of *all*
    % the rvals have been output.
    %
:- pred output_record_rvals_decls_format_count(llds_out_info::in,
    io.text_output_stream::in, list(rval)::in, list(rval)::out, int::in,
    string::in, string::in, int::in, int::out, decl_set::in,
    decl_set::out, io::di, io::uo) is det.

output_record_rvals_decls_format_count(_, _, [], [], _, _, _,
        !N, !DeclSet, !IO).
output_record_rvals_decls_format_count(Info, Stream, [Rval | Rvals],
        LeftOverRvals, Count, FirstIndent, LaterIndent, !N, !DeclSet, !IO) :-
    ( if Count > 0 then
        output_record_rval_decls_format(Info, Stream, Rval,
            FirstIndent, LaterIndent, !N, !DeclSet, !IO),
        output_record_rvals_decls_format_count(Info, Stream, Rvals,
            LeftOverRvals, Count - 1, FirstIndent, LaterIndent,
            !N, !DeclSet, !IO)
    else
        LeftOverRvals = [Rval | Rvals]
    ).

:- pred output_record_mem_ref_decls_format(llds_out_info::in,
    io.text_output_stream::in, mem_ref::in, string::in, string::in,
    int::in, int::out, decl_set::in, decl_set::out, io::di, io::uo) is det.

output_record_mem_ref_decls_format(Info, Stream, MemRef,
        FirstIndent, LaterIndent, !N, !DeclSet, !IO) :-
    (
        ( MemRef = stackvar_ref(Rval)
        ; MemRef = framevar_ref(Rval)
        ),
        output_record_rval_decls_format(Info, Stream, Rval,
            FirstIndent, LaterIndent, !N, !DeclSet, !IO)
    ;
        MemRef = heap_ref(BaseRval, _, OffsetRval),
        output_record_rval_decls_format(Info, Stream, BaseRval,
            FirstIndent, LaterIndent, !N, !DeclSet, !IO),
        output_record_rval_decls_format(Info, Stream, OffsetRval,
            FirstIndent, LaterIndent, !N, !DeclSet, !IO)
    ).

%----------------------------------------------------------------------------%
%
% Writing rvals.
%

output_rval(Info, Rval, Stream, !IO) :-
    (
        Rval = const(Const),
        output_rval_const(Info, Const, Stream, !IO)
    ;
        Rval = cast(Type, SubRval),
        io.write_string(Stream, "((", !IO),
        output_llds_type(Stream, Type, !IO),
        io.write_string(Stream, ") ", !IO),
        output_rval(Info, SubRval, Stream, !IO),
        io.write_string(Stream, ")", !IO)
    ;
        Rval = unop(UnaryOp, SubRval),
        c_util.unary_prefix_op(UnaryOp, OpString),
        llds.unop_arg_type(UnaryOp, ArgType),
        io.write_string(Stream, OpString, !IO),
        io.write_string(Stream, "(", !IO),
        output_rval_as_type(Info, SubRval, ArgType, Stream, !IO),
        io.write_string(Stream, ")", !IO)
    ;
        Rval = binop(Op, SubRvalA, SubRvalB),
        (
            Op = array_index(_),
            io.write_string(Stream, "(", !IO),
            output_rval_as_type(Info, SubRvalA, lt_data_ptr, Stream, !IO),
            io.write_string(Stream, ")[", !IO),
            output_rval_as_type(Info, SubRvalB, lt_int(int_type_int),
                Stream, !IO),
            io.write_string(Stream, "]", !IO)
        ;
            Op = string_unsafe_index_code_unit,
            io.write_string(Stream, "MR_nth_code_unit(", !IO),
            output_rval_as_type(Info, SubRvalA, lt_data_ptr, Stream, !IO),
            io.write_string(Stream, ", ", !IO),
            output_rval_as_type(Info, SubRvalB, lt_int(int_type_int),
                Stream, !IO),
            io.write_string(Stream, ")", !IO)
        ;
            Op = pointer_equal_conservative,
            io.write_string(Stream, "(((MR_Word) ", !IO),
            output_rval(Info, SubRvalA, Stream, !IO),
            io.write_string(Stream, ") == ((MR_Word) ", !IO),
            output_rval(Info, SubRvalB, Stream, !IO),
            io.write_string(Stream, "))", !IO)
        ;
            ( Op = compound_lt
            ; Op = compound_eq
            ),
            % These operators were intended to be generated only when using
            % the now-deleted Erlang backend.
            unexpected($file, $pred, "compound_compare_binop")
        ;
            ( Op = str_eq, OpStr = "=="
            ; Op = str_ne, OpStr = "!="
            ; Op = str_le, OpStr = "<="
            ; Op = str_ge, OpStr = ">="
            ; Op = str_lt, OpStr = "<"
            ; Op = str_gt, OpStr = ">"
            ),
            io.write_string(Stream, "(strcmp(", !IO),
            ( if SubRvalA = const(llconst_string(SubRvalAConst)) then
                output_rval_const(Info, llconst_string(SubRvalAConst),
                    Stream, !IO)
            else
                io.write_string(Stream, "(char *) ", !IO),
                output_rval_as_type(Info, SubRvalA, lt_data_ptr, Stream, !IO)
            ),
            io.write_string(Stream, ", ", !IO),
            ( if SubRvalB = const(llconst_string(SubRvalBConst)) then
                output_rval_const(Info, llconst_string(SubRvalBConst),
                    Stream, !IO)
            else
                io.write_string(Stream, "(char *) ", !IO),
                output_rval_as_type(Info, SubRvalB, lt_data_ptr, Stream, !IO)
            ),
            io.write_string(Stream, ")", !IO),
            io.write_string(Stream, " ", !IO),
            io.write_string(Stream, OpStr, !IO),
            io.write_string(Stream, " ", !IO),
            io.write_string(Stream, "0)", !IO)
        ;
            ( Op = float_eq, OpStr = "=="
            ; Op = float_ne, OpStr = "!="
            ; Op = float_le, OpStr = "<="
            ; Op = float_ge, OpStr = ">="
            ; Op = float_lt, OpStr = "<"
            ; Op = float_gt, OpStr = ">"
            ; Op = float_add, OpStr = "+"
            ; Op = float_sub, OpStr = "-"
            ; Op = float_mul, OpStr = "*"
            ; Op = float_div, OpStr = "/"
            ),
            io.write_string(Stream, "(", !IO),
            output_rval_as_type(Info, SubRvalA, lt_float, Stream, !IO),
            io.write_string(Stream, " ", !IO),
            io.write_string(Stream, OpStr, !IO),
            io.write_string(Stream, " ", !IO),
            output_rval_as_type(Info, SubRvalB, lt_float, Stream, !IO),
            io.write_string(Stream, ")", !IO)
        ;
            Op = unsigned_lt,
            io.write_string(Stream, "(", !IO),
            output_rval_as_type(Info, SubRvalA, lt_int(int_type_uint),
                Stream, !IO),
            io.write_string(Stream, " < ", !IO),
            output_rval_as_type(Info, SubRvalB, lt_int(int_type_uint),
                Stream, !IO),
            io.write_string(Stream, ")", !IO)
        ;
            Op = unsigned_le,
            io.write_string(Stream, "(", !IO),
            output_rval_as_type(Info, SubRvalA, lt_int(int_type_uint),
                Stream, !IO),
            io.write_string(Stream, " <= ", !IO),
            output_rval_as_type(Info, SubRvalB, lt_int(int_type_uint),
                Stream, !IO),
            io.write_string(Stream, ")", !IO)
        ;
            ( Op = int_add(IntType), OpStr = "+"
            ; Op = int_sub(IntType), OpStr = "-"
            ; Op = int_mul(IntType), OpStr = "*"
            ),
            (
                (
                    IntType = int_type_int,
                    SignedType = "MR_Integer",
                    UnsignedType = "MR_Unsigned"
                ;
                    IntType = int_type_int8,
                    SignedType = "int8_t",
                    UnsignedType = "uint8_t"
                ;
                    IntType = int_type_int16,
                    SignedType = "int16_t",
                    UnsignedType = "uint16_t"
                ;
                    IntType = int_type_int32,
                    SignedType = "int32_t",
                    UnsignedType = "uint32_t"
                ;
                    IntType = int_type_int64,
                    SignedType = "int64_t",
                    UnsignedType = "uint64_t"
                ),
                % We used to handle X + (-C) (for constant C) specially, by
                % converting it to X - C, but we no longer do that since it
                % would overflow in the case where C == min_int.
                io.format(Stream, "(%s) ((%s) ",
                    [s(SignedType), s(UnsignedType)], !IO),
                output_rval_as_type(Info, SubRvalA, lt_int(IntType),
                    Stream, !IO),
                io.format(Stream, " %s (%s) ",
                    [s(OpStr), s(UnsignedType)], !IO),
                output_rval_as_type(Info, SubRvalB, lt_int(IntType),
                    Stream, !IO),
                io.write_string(Stream, ")", !IO)
            ;
                ( IntType = int_type_uint
                ; IntType = int_type_uint8
                ; IntType = int_type_uint16
                ; IntType = int_type_uint32
                ; IntType = int_type_uint64
                ),
                io.write_string(Stream, "(", !IO),
                output_rval_as_type(Info, SubRvalA, lt_int(IntType),
                    Stream, !IO),
                io.format(Stream, " %s ", [s(OpStr)], !IO),
                output_rval_as_type(Info, SubRvalB, lt_int(IntType),
                    Stream, !IO),
                io.write_string(Stream, ")", !IO)
            )
        ;
            ( Op = int_div(IntType), OpStr = "/"
            ; Op = int_mod(IntType), OpStr = "%"
            ; Op = eq(IntType), OpStr = "=="
            ; Op = ne(IntType), OpStr = "!="
            ; Op = int_lt(IntType), OpStr = "<"
            ; Op = int_gt(IntType), OpStr = ">"
            ; Op = int_le(IntType), OpStr = "<="
            ; Op = int_ge(IntType), OpStr = ">="
            ; Op = bitwise_and(IntType), OpStr = "&"
            ; Op = bitwise_or(IntType), OpStr = "|"
            ; Op = bitwise_xor(IntType), OpStr = "^"
            ),
            ( if
                % Special-case equality ops to avoid some unnecessary casts --
                % there is no difference between signed and unsigned equality,
                % so if both args are unsigned, we don't need to cast them to
                % MR_Integer.
                ( Op = eq(_) ; Op = ne(_) ),

                require_complete_switch [IntType]
                (
                    ( IntType = int_type_int
                    ; IntType = int_type_uint
                    )
                ;
                    % Don't apply this special case for sub-word-sized types,
                    % to avoid having any differences in the rest of the word
                    % convert an "equal" result to a "not equal" result.
                    %
                    % Don't apply this special case for 64-bit integer types,
                    % since they may be boxed.
                    ( IntType = int_type_int8
                    ; IntType = int_type_uint8
                    ; IntType = int_type_int16
                    ; IntType = int_type_uint16
                    ; IntType = int_type_int32
                    ; IntType = int_type_uint32
                    ; IntType = int_type_int64
                    ; IntType = int_type_uint64
                    ),
                    fail
                ),
                llds.rval_type(SubRvalA, SubRvalAType),
                ( SubRvalAType = lt_word
                ; SubRvalAType = lt_int(int_type_uint)
                ),
                llds.rval_type(SubRvalB, SubRvalBType),
                ( SubRvalBType = lt_word
                ; SubRvalBType = lt_int(int_type_uint)
                )
            then
                io.write_string(Stream, "(", !IO),
                output_rval(Info, SubRvalA, Stream, !IO),
                io.write_string(Stream, " ", !IO),
                io.write_string(Stream, OpStr, !IO),
                io.write_string(Stream, " ", !IO),
                output_rval(Info, SubRvalB, Stream, !IO),
                io.write_string(Stream, ")", !IO)
            else
                io.write_string(Stream, "(", !IO),
                output_rval_as_type(Info, SubRvalA, lt_int(IntType),
                    Stream, !IO),
                io.write_string(Stream, " ", !IO),
                io.write_string(Stream, OpStr, !IO),
                io.write_string(Stream, " ", !IO),
                output_rval_as_type(Info, SubRvalB, lt_int(IntType),
                    Stream, !IO),
                io.write_string(Stream, ")", !IO)
            )
        ;
            ( Op = logical_and, OpStr = "&&"
            ; Op = logical_or, OpStr = "||"
            ),
            io.write_string(Stream, "(", !IO),
            output_rval_as_type(Info, SubRvalA, lt_int(int_type_int),
                Stream, !IO),
            io.write_string(Stream, " ", !IO),
            io.write_string(Stream, OpStr, !IO),
            io.write_string(Stream, " ", !IO),
            output_rval_as_type(Info, SubRvalB, lt_int(int_type_int),
                Stream, !IO),
            io.write_string(Stream, ")", !IO)
        ;
            % The second operand of the shift operators always has type
            % `int' in C, but Mercury also allows it to be uint.
            ( Op = unchecked_left_shift(IntType, ShiftType), OpStr = "<<"
            ; Op = unchecked_right_shift(IntType, ShiftType), OpStr = ">>"
            ),
            io.write_string(Stream, "(", !IO),
            output_rval_as_type(Info, SubRvalA, lt_int(IntType), Stream, !IO),
            io.write_string(Stream, " ", !IO),
            io.write_string(Stream, OpStr, !IO),
            io.write_string(Stream, " ", !IO),
            (
                ShiftType = shift_by_int
            ;
                ShiftType = shift_by_uint,
                io.write_string(Stream, "(int) ", !IO)
            ),
            output_rval_as_type(Info, SubRvalB, lt_int(int_type_int),
                Stream, !IO),
            io.write_string(Stream, ")", !IO)
        ;
            Op = str_cmp,
            io.write_string(Stream, "MR_strcmp(", !IO),
            output_rval_as_type(Info, SubRvalA, lt_data_ptr, Stream, !IO),
            io.write_string(Stream, ", ", !IO),
            output_rval_as_type(Info, SubRvalB, lt_data_ptr, Stream, !IO),
            io.write_string(Stream, ")", !IO)
        ;
            Op = offset_str_eq(N),
            io.write_string(Stream, "MR_offset_streq(", !IO),
            io.write_int(Stream, N, !IO),
            io.write_string(Stream, ", ", !IO),
            output_rval_as_type(Info, SubRvalA, lt_data_ptr, Stream, !IO),
            io.write_string(Stream, ", ", !IO),
            output_rval_as_type(Info, SubRvalB, lt_data_ptr, Stream, !IO),
            io.write_string(Stream, ")", !IO)
        ;
            Op = body,
            io.write_string(Stream, "MR_body(", !IO),
            output_rval_as_type(Info, SubRvalA, lt_int(int_type_int),
                Stream, !IO),
            io.write_string(Stream, ", ", !IO),
            output_rval_as_type(Info, SubRvalB, lt_int(int_type_int),
                Stream, !IO),
            io.write_string(Stream, ")", !IO)
        ;
            ( Op = float_from_dword,  OpStr = "MR_float_from_dword"
            ; Op = int64_from_dword,  OpStr = "MR_int64_from_dword"
            ; Op = uint64_from_dword, OpStr = "MR_uint64_from_dword"
            ),
            io.write_string(Stream, OpStr, !IO),
            ( if is_aligned_dword_ptr(SubRvalA, SubRvalB, MemRef) then
                io.write_string(Stream, "_ptr(MR_dword_ptr(", !IO),
                output_rval(Info, mem_addr(MemRef), Stream, !IO),
                io.write_string(Stream, "))", !IO)
            else
                io.write_string(Stream, "(", !IO),
                output_rval(Info, SubRvalA, Stream, !IO),
                io.write_string(Stream, ", ", !IO),
                output_rval(Info, SubRvalB, Stream, !IO),
                io.write_string(Stream, ")", !IO)
            )
        )
    ;
        Rval = mkword(ptag(PtagUInt8), SubRval),
        ( if
            SubRval = const(llconst_data_addr(DataId, no)),
            DataId = scalar_common_data_id(type_num(TypeNum), CellNum)
        then
            io.format(Stream, "MR_TAG_COMMON(%u, %d, %d)",
                [u8(PtagUInt8), i(TypeNum), i(CellNum)], !IO)
        else if
            SubRval = unop(mkbody, const(llconst_int(Body)))
        then
            io.format(Stream, "MR_tbmkword(%u, %d)",
                [u8(PtagUInt8), i(Body)], !IO)
        else
            io.format(Stream, "MR_tmkword(%u, ", [u8(PtagUInt8)], !IO),
            output_rval_as_type(Info, SubRval, lt_data_ptr, Stream, !IO),
            io.write_string(Stream, ")", !IO)
        )
    ;
        Rval = mkword_hole(ptag(PtagUInt8)),
        io.format(Stream, "MR_tmkword(%u, 0)", [u8(PtagUInt8)], !IO)
    ;
        Rval = lval(Lval),
        % If a field is used as an rval, then we need to use the
        % MR_const_field() macro or its variants, not the MR_field() macro
        % or its variants, to avoid warnings about discarding const.
        ( if Lval = field(MaybePtag, Rval, FieldNumRval) then
            (
                MaybePtag = yes(ptag(PtagUInt8)),
                io.format(Stream, "MR_ctfield(%u, ", [u8(PtagUInt8)], !IO)
            ;
                MaybePtag = no,
                io.write_string(Stream, "MR_const_mask_field(", !IO)
            ),
            output_rval(Info, Rval, Stream, !IO),
            io.write_string(Stream, ", ", !IO),
            ( if FieldNumRval = const(llconst_int(FieldNum)) then
                % Avoid emitting the (MR_Integer) cast.
                io.write_int(Stream, FieldNum, !IO)
            else
                output_rval(Info, FieldNumRval, Stream, !IO)
            ),
            io.write_string(Stream, ")", !IO)
        else
            output_lval(Info, Stream, Lval, !IO)
        )
    ;
        Rval = var(_),
        unexpected($file, $pred, "cannot output a var(_) expression in code")
    ;
        Rval = mem_addr(MemRef),
        (
            MemRef = stackvar_ref(SubRval),
            io.write_string(Stream, "&MR_sv(", !IO),
            % Don't clutter the output with unnecessary casts.
            ( if SubRval = const(llconst_int(SlotNum)) then
                io.write_int(Stream, SlotNum, !IO)
            else
                output_rval_as_type(Info, SubRval, lt_int(int_type_int),
                    Stream, !IO)
            ),
            io.write_string(Stream, ")", !IO)
        ;
            MemRef = framevar_ref(SubRval),
            io.write_string(Stream, "&MR_fv(", !IO),
            % Don't clutter the output with unnecessary casts.
            ( if SubRval = const(llconst_int(SlotNum)) then
                io.write_int(Stream, SlotNum, !IO)
            else
                output_rval_as_type(Info, SubRval, lt_int(int_type_int),
                    Stream, !IO)
            ),
            io.write_string(Stream, ")", !IO)
        ;
            MemRef = heap_ref(BaseRval, MaybePtag, FieldNumRval),
            (
                MaybePtag = yes(ptag(PtagUInt8)),
                io.format(Stream, "&MR_tfield(%u, ", [u8(PtagUInt8)], !IO)
            ;
                MaybePtag = no,
                io.write_string(Stream, "&MR_mask_field(", !IO)
            ),
            output_rval(Info, BaseRval, Stream, !IO),
            io.write_string(Stream, ", ", !IO),
            % Don't clutter the output with unnecessary casts.
            ( if FieldNumRval = const(llconst_int(FieldNum)) then
                io.write_int(Stream, FieldNum, !IO)
            else
                output_rval_as_type(Info, FieldNumRval, lt_int(int_type_int),
                    Stream, !IO)
            ),
            io.write_string(Stream, ")", !IO)
        )
    ).

:- pred output_rval_const(llds_out_info::in, rval_const::in,
    io.text_output_stream::in, io::di, io::uo) is det.

output_rval_const(Info, Const, Stream, !IO) :-
    (
        Const = llconst_true,
        io.write_string(Stream, "MR_TRUE", !IO)
    ;
        Const = llconst_false,
        io.write_string(Stream, "MR_FALSE", !IO)
    ;
        Const = llconst_int(N),
        c_util.output_int_expr(Stream, N, !IO)
    ;
        Const = llconst_uint(N),
        c_util.output_uint_expr(Stream, N, !IO)
    ;
        Const = llconst_int8(N),
        c_util.output_int8_expr(Stream, N, !IO)
    ;
        Const = llconst_uint8(N),
        c_util.output_uint8_expr(Stream, N, !IO)
    ;
        Const = llconst_int16(N),
        c_util.output_int16_expr(Stream, N, !IO)
    ;
        Const = llconst_uint16(N),
        c_util.output_uint16_expr(Stream, N, !IO)
    ;
        Const = llconst_int32(N),
        c_util.output_int32_expr(Stream, N, !IO)
    ;
        Const = llconst_uint32(N),
        c_util.output_uint32_expr(Stream, N, !IO)
    ;
        Const = llconst_int64(N),
        c_util.output_int64_expr(Stream, N, !IO)
    ;
        Const = llconst_uint64(N),
        c_util.output_uint64_expr(Stream, N, !IO)
    ;
        Const = llconst_foreign(Value, Type),
        io.write_char(Stream, '(', !IO),
        output_llds_type_cast(Stream, Type, !IO),
        io.write_string(Stream, Value, !IO),
        io.write_char(Stream, ')', !IO)
    ;
        Const = llconst_float(FloatVal),
        % The cast to (MR_Float) here lets the C compiler do arithmetic in
        % `float' rather than `double' if `MR_Float' is `float' not `double'.
        output_llds_type_cast(Stream, lt_float, !IO),
        c_util.output_float_literal(Stream, FloatVal, !IO)
    ;
        Const = llconst_string(String),
        io.write_string(Stream, "MR_string_const(", !IO),
        output_quoted_string_c(Stream, String, !IO),
        io.write_string(Stream, ", ", !IO),
        io.write_int(Stream, string.count_utf8_code_units(String), !IO),
        io.write_string(Stream, ")", !IO)
    ;
        Const = llconst_multi_string(Strings),
        io.write_string(Stream, "MR_string_const(", !IO),
        output_quoted_multi_string_c(Stream, Strings, !IO),
        io.write_string(Stream, ", ", !IO),

        % The "+1" is for the NULL character.
        SumLengths = (func(S, L0) = L0 + string.count_utf8_code_units(S) + 1),
        Length = list.foldl(SumLengths, Strings, 0),
        io.write_int(Stream, Length, !IO),
        io.write_string(Stream, ")", !IO)
    ;
        Const = llconst_code_addr(CodeAddress),
        output_code_addr(Stream, CodeAddress, !IO)
    ;
        Const = llconst_data_addr(DataId, MaybeOffset),
        % Data addresses are all assumed to be of type `MR_Word *'; we need to
        % cast them here to avoid type errors. The offset is also in MR_Words.
        (
            MaybeOffset = no,
            % The tests for special cases below increase the runtime of the
            % compiler very slightly, but the use of shorter names reduces
            % the size of the generated C source file, which has a
            % considerably longer lifetime. In debugging grades, the
            % file size difference can be very substantial.
            ( if
                DataId = scalar_common_data_id(type_num(TypeNum), CellNum)
            then
                io.format(Stream, "MR_COMMON(%d, %d)",
                    [i(TypeNum), i(CellNum)], !IO)
            else if
                DataId = rtti_data_id(RttiId),
                rtti_id_emits_type_ctor_info(RttiId, Ctor),
                Ctor = rtti_type_ctor(Module, Name, Arity),
                sym_name_doesnt_need_mangling(Module),
                name_doesnt_need_mangling(Name)
            then
                output_type_ctor_addr(Stream, Module, Name, Arity, !IO)
            else
                output_llds_type_cast(Stream, lt_data_ptr, !IO),
                output_data_id_addr(Info, Stream, DataId, !IO)
            )
        ;
            MaybeOffset = yes(Offset),
            io.write_string(Stream, "((", !IO),
            output_llds_type_cast(Stream, lt_data_ptr, !IO),
            output_data_id_addr(Info, Stream, DataId, !IO),
            io.write_string(Stream, ") + ", !IO),
            io.write_int(Stream, Offset, !IO),
            io.write_string(Stream, ")", !IO)
        )
    ).

:- pred output_type_ctor_addr(io.text_output_stream::in,
    module_name::in, string::in, uint16::in, io::di, io::uo) is det.

output_type_ctor_addr(Stream, Module0, Name, Arity, !IO) :-
    ( if Module0 = unqualified("") then
        Module = mercury_public_builtin_module
    else
        Module = Module0
    ),
    % We don't need to mangle the module name, but we do need to convert it
    % to a C identifier in the standard fashion.
    ModuleStr = sym_name_mangle(Module),
    ( if Arity = 0u16 then
        ( if
            ModuleStr = "builtin",
            builtin_type_to_type_ctor_addr(Name, Macro)
        then
            io.write_string(Stream, Macro, !IO)
        else if
            ModuleStr = "io",
            Name = "state"
        then
            io.write_string(Stream, "MR_IO_CTOR_ADDR", !IO)
        else if
            ModuleStr = "bool",
            Name = "bool"
        then
            io.write_string(Stream, "MR_BOOL_CTOR_ADDR", !IO)
        else
            io.format(Stream, "MR_CTOR0_ADDR(%s, %s)",
                [s(ModuleStr), s(Name)], !IO)
        )
    else if Arity = 1u16 then
        ( if
            Name = "list",
            ModuleStr = "list"
        then
            io.write_string(Stream, "MR_LIST_CTOR_ADDR", !IO)
        else if
            Name = "private_builtin",
            ModuleStr = "type_info"
        then
            io.write_string(Stream, "MR_TYPE_INFO_CTOR_ADDR", !IO)
        else
            io.format(Stream, "MR_CTOR1_ADDR(%s, %s)",
                [s(ModuleStr), s(Name)], !IO)
        )
    else
        io.format(Stream, "MR_CTOR_ADDR(%s, %s, %d)",
            [s(ModuleStr), s(Name), i(uint16.to_int(Arity))], !IO)
    ).

:- pred builtin_type_to_type_ctor_addr(string::in, string::out) is semidet.

builtin_type_to_type_ctor_addr(Name, Macro) :-
    (
        Name = "int",
        Macro = "MR_INT_CTOR_ADDR"
    ;
        Name = "uint",
        Macro = "MR_UINT_CTOR_ADDR"
    ;
        Name = "int8",
        Macro = "MR_INT8_CTOR_ADDR"
    ;
        Name = "uint8",
        Macro = "MR_UINT8_CTOR_ADDR"
    ;
        Name = "int16",
        Macro = "MR_INT16_CTOR_ADDR"
    ;
        Name = "uint16",
        Macro = "MR_UINT16_CTOR_ADDR"
    ;
        Name = "int32",
        Macro = "MR_INT32_CTOR_ADDR"
    ;
        Name = "uint32",
        Macro = "MR_UINT32_CTOR_ADDR"
    ;
        Name = "int64",
        Macro = "MR_INT64_CTOR_ADDR"
    ;
        Name = "uint64",
        Macro = "MR_UINT64_CTOR_ADDR"
    ;
        Name = "float",
        Macro = "MR_FLOAT_CTOR_ADDR"
    ;
        Name = "string",
        Macro = "MR_STRING_CTOR_ADDR"
    ;
        Name = "character",
        Macro = "MR_CHAR_CTOR_ADDR"
    ).

output_rval_as_type(Info, Rval, DesiredType, Stream, !IO) :-
    llds.rval_type(Rval, ActualType),
    ( if llds_types_match(DesiredType, ActualType) then
        % No casting needed.
        output_rval(Info, Rval, Stream, !IO)
    else
        % We need to convert to the right type first.
        % Conversions to/from float, int64 and uint64 must be treated
        % specially; for the others, we can just use a cast.
        ( if DesiredType = lt_float then
            io.write_string(Stream, "MR_word_to_float(", !IO),
            output_rval(Info, Rval, Stream, !IO),
            io.write_string(Stream, ")", !IO)
        else if ActualType = lt_float then
            ( if DesiredType = lt_word then
                output_float_rval_as_word(Info, Stream, Rval, !IO)
            else if DesiredType = lt_data_ptr then
                output_float_rval_as_data_ptr(Info, Stream, Rval, !IO)
            else
                unexpected($file, $pred, "type error")
            )
        else if DesiredType = lt_int(int_type_int64) then
            io.write_string(Stream, "MR_word_to_int64(", !IO),
            output_rval(Info, Rval, Stream, !IO),
            io.write_string(Stream, ")", !IO)
        else if ActualType = lt_int(int_type_int64) then
            ( if DesiredType = lt_word then
                output_int64_rval_as_word(Info, Stream, Rval, !IO)
            else if DesiredType = lt_data_ptr then
                output_int64_rval_as_data_ptr(Info, Stream, Rval, !IO)
            else
                unexpected($file, $pred, "type error")
            )
        else if DesiredType = lt_int(int_type_uint64) then
            io.write_string(Stream, "MR_word_to_uint64(", !IO),
            output_rval(Info, Rval, Stream, !IO),
            io.write_string(Stream, ")", !IO)
        else if ActualType = lt_int(int_type_uint64) then
            ( if DesiredType = lt_word then
                output_uint64_rval_as_word(Info, Stream, Rval, !IO)
            else if DesiredType = lt_data_ptr then
                output_uint64_rval_as_data_ptr(Info, Stream, Rval, !IO)
            else
                unexpected($file, $pred, "type error")
            )
        else
            ( if
                Rval = const(llconst_int(N)),
                direct_field_int_constant(DesiredType) = yes
            then
                % The condition above increases the runtime of the compiler
                % very slightly. The elimination of the unnecessary casts
                % reduces the size of the generated C source file, which has
                % a considerably longer lifetime. In debugging grades,
                % the file size difference can be very substantial; it can be
                % in the range of megabytes.
                io.write_int(Stream, N, !IO)
            else
                % Cast value to desired type.
                io.write_string(Stream, "(", !IO),
                output_llds_type_cast(Stream, DesiredType, !IO),
                output_rval(Info, Rval, Stream, !IO),
                io.write_string(Stream, ")", !IO)
            )
        )
    ).

    % Output a float rval, converted to type `MR_Word *'.
    %
:- pred output_float_rval_as_data_ptr(llds_out_info::in,
    io.text_output_stream::in, rval::in, io::di, io::uo) is det.

output_float_rval_as_data_ptr(Stream, Info, Rval, !IO) :-
    output_float_rval(Stream, Info, Rval, yes, !IO).

    % Output a float rval, converted to type `MR_Word'.
    %
:- pred output_float_rval_as_word(llds_out_info::in, io.text_output_stream::in,
    rval::in, io::di, io::uo) is det.

output_float_rval_as_word(Info, Stream, Rval, !IO) :-
    output_float_rval(Info, Stream, Rval, no, !IO).

    % Output a float rval, converted to type `MR_Word' or `MR_Word *'.
    %
:- pred output_float_rval(llds_out_info::in, io.text_output_stream::in,
    rval::in, bool::in, io::di, io::uo) is det.

output_float_rval(Info, Stream, Rval, IsPtr, !IO) :-
    % For float constant expressions, if we are using boxed floats
    % and --static-ground-floats is enabled, we just refer to the static const
    % which we declared earlier.
    UnboxFloat = Info ^ lout_unboxed_float,
    StaticGroundFloats = Info ^ lout_static_ground_floats,
    ( if
        UnboxFloat = no,
        StaticGroundFloats = use_static_ground_floats,
        float_const_expr_name(Rval, FloatName)
    then
        (
            IsPtr = yes,
            Cast = lt_data_ptr
        ;
            IsPtr = no,
            Cast = lt_word
        ),
        output_llds_type_cast(Stream, Cast, !IO),
        io.write_string(Stream, "&mercury_float_const_", !IO),
        io.write_string(Stream, FloatName, !IO)
    else
        (
            IsPtr = yes,
            output_llds_type_cast(Stream, lt_data_ptr, !IO)
        ;
            IsPtr = no
        ),
        io.write_string(Stream, "MR_float_to_word(", !IO),
        output_rval(Info, Rval, Stream, !IO),
        io.write_string(Stream, ")", !IO)
    ).

    % Output a int64 rval, converted to type `MR_Word *'
    %
:- pred output_int64_rval_as_data_ptr(llds_out_info::in,
    io.text_output_stream::in, rval::in, io::di, io::uo) is det.

output_int64_rval_as_data_ptr(Info, Stream, Rval, !IO) :-
    output_int64_rval(Info, Stream, Rval, yes, !IO).

    % Output a int64 rval, converted to type `MR_Word'
    %
:- pred output_int64_rval_as_word(llds_out_info::in, io.text_output_stream::in,
    rval::in, io::di, io::uo) is det.

output_int64_rval_as_word(Info, Stream, Rval, !IO) :-
    output_int64_rval(Info, Stream, Rval, no, !IO).

    % Output a int64 rval, converted to type `MR_Word' or `MR_Word *'
    %
:- pred output_int64_rval(llds_out_info::in, io.text_output_stream::in,
    rval::in, bool::in, io::di, io::uo) is det.

output_int64_rval(Info, Stream, Rval, IsPtr, !IO) :-
    % For int64 constants, if we are using boxed 64-bit integers and
    % --static-ground-int64s is enabled, we just refer to the static const
    % which we declared earlier.
    UnboxInt64s = Info ^ lout_unboxed_int64s,
    StaticGroundInt64s = Info ^ lout_static_ground_int64s,
    ( if
        UnboxInt64s = no,
        StaticGroundInt64s = use_static_ground_int64s,
        int64_const_expr_name(Rval, Int64Name)
    then
        (
            IsPtr = yes,
            Cast = lt_data_ptr
        ;
            IsPtr = no,
            Cast = lt_word
        ),
        output_llds_type_cast(Stream, Cast, !IO),
        io.write_string(Stream, "&mercury_int64_const_", !IO),
        io.write_string(Stream, Int64Name, !IO)
    else
        (
            IsPtr = yes,
            output_llds_type_cast(Stream, lt_data_ptr, !IO)
        ;
            IsPtr = no
        ),
        io.write_string(Stream, "MR_int64_to_word(", !IO),
        output_rval(Info, Rval, Stream, !IO),
        io.write_string(Stream, ")", !IO)
    ).

    % Output a uint64 rval, converted to type `MR_Word *'.
    %
:- pred output_uint64_rval_as_data_ptr(llds_out_info::in,
    io.text_output_stream::in, rval::in, io::di, io::uo) is det.

output_uint64_rval_as_data_ptr(Info, Stream, Rval, !IO) :-
    output_uint64_rval(Info, Stream, Rval, yes, !IO).

    % Output a uint64 rval, converted to type `MR_Word'.
    %
:- pred output_uint64_rval_as_word(llds_out_info::in,
    io.text_output_stream::in, rval::in, io::di, io::uo) is det.

output_uint64_rval_as_word(Info, Stream, Rval, !IO) :-
    output_uint64_rval(Info, Stream, Rval, no, !IO).

    % Output a uint64 rval, converted to type `MR_Word' or `MR_Word *'.
    %
:- pred output_uint64_rval(llds_out_info::in, io.text_output_stream::in,
    rval::in, bool::in, io::di, io::uo) is det.

output_uint64_rval(Info, Stream, Rval, IsPtr, !IO) :-
    % For uint64 constants, if we are using boxed 64-bit integers and
    % --static-ground-int64s is enabled, we just refer to the static const
    % which we declared earlier.
    UnboxInt64s = Info ^ lout_unboxed_int64s,
    StaticGroundInt64s = Info ^ lout_static_ground_int64s,
    ( if
        UnboxInt64s = no,
        StaticGroundInt64s = use_static_ground_int64s,
        uint64_const_expr_name(Rval, UInt64Name)
    then
        (
            IsPtr = yes,
            Cast = lt_data_ptr
        ;
            IsPtr = no,
            Cast = lt_word
        ),
        output_llds_type_cast(Stream, Cast, !IO),
        io.write_string(Stream, "&mercury_uint64_const_", !IO),
        io.write_string(Stream, UInt64Name, !IO)
    else
        (
            IsPtr = yes,
            output_llds_type_cast(Stream, lt_data_ptr, !IO)
        ;
            IsPtr = no
        ),
        io.write_string(Stream, "MR_uint64_to_word(", !IO),
        output_rval(Info, Rval, Stream, !IO),
        io.write_string(Stream, ")", !IO)
    ).

:- pred is_aligned_dword_ptr(rval::in, rval::in, mem_ref::out) is semidet.

is_aligned_dword_ptr(lval(LvalA), lval(LvalB), MemRef) :-
    (
        LvalA = stackvar(N),
        LvalB = stackvar(N + 1),
        % Double-width variables on the det stack should have been aligned
        % by the allocator. In a downwards-growing stack the higher slot
        % number has the lower address.
        MemRef = stackvar_ref(const(llconst_int(N + 1)))
    ;
        LvalA = field(_MaybePtag, _Address, _Offset),
        % We cannot guarantee that the Address is dword aligned.
        fail
    ).

output_test_rval(Info, Test, Stream, !IO) :-
    ( if
        is_int_cmp(Test, Left, RightConst, OpStr, _)
    then
        io.write_string(Stream, OpStr, !IO),
        io.write_string(Stream, "(", !IO),
        output_rval(Info, Left, Stream, !IO),
        io.write_string(Stream, ",", !IO),
        io.write_int(Stream, RightConst, !IO),
        io.write_string(Stream, ")", !IO)
    else if
        Test = unop(logical_not, InnerTest),
        is_int_cmp(InnerTest, Left, RightConst, _, NegOpStr)
    then
        io.write_string(Stream, NegOpStr, !IO),
        io.write_string(Stream, "(", !IO),
        output_rval(Info, Left, Stream, !IO),
        io.write_string(Stream, ",", !IO),
        io.write_int(Stream, RightConst, !IO),
        io.write_string(Stream, ")", !IO)
    else if
        is_ptag_test(Test, Rval, Ptag, Negated)
    then
        (
            Negated = no,
            io.write_string(Stream, "MR_PTAG_TEST(", !IO)
        ;
            Negated = yes,
            io.write_string(Stream, "MR_PTAG_TESTR(", !IO)
        ),
        output_rval(Info, Rval, Stream, !IO),
        io.write_string(Stream, ",", !IO),
        write_ptag(Stream, Ptag, !IO),
        io.write_string(Stream, ")", !IO)
    else if
        Test = unop(logical_not, InnerTest),
        is_ptag_test(InnerTest, Rval, Ptag, Negated)
    then
        (
            Negated = no,
            io.write_string(Stream, "MR_PTAG_TESTR(", !IO)
        ;
            Negated = yes,
            io.write_string(Stream, "MR_PTAG_TEST(", !IO)
        ),
        output_rval(Info, Rval, Stream, !IO),
        io.write_string(Stream, ",", !IO),
        write_ptag(Stream, Ptag, !IO),
        io.write_string(Stream, ")", !IO)
    else if
        Test = binop(logical_and, Left, Right),
        is_ptag_test(Left, Rval, Ptag, no),
        is_remote_stag_test(Right, Rval, Ptag, Stag)
    then
        io.write_string(Stream, "MR_RTAGS_TEST(", !IO),
        output_rval(Info, Rval, Stream, !IO),
        io.write_string(Stream, ",", !IO),
        write_ptag(Stream, Ptag, !IO),
        io.write_string(Stream, ",", !IO),
        io.write_uint(Stream, Stag, !IO),
        io.write_string(Stream, ")", !IO)
    else if
        Test = unop(logical_not, InnerTest),
        InnerTest = binop(logical_and, Left, Right),
        is_ptag_test(Left, Rval, Ptag, no),
        is_remote_stag_test(Right, Rval, Ptag, Stag)
    then
        io.write_string(Stream, "MR_RTAGS_TESTR(", !IO),
        output_rval(Info, Rval, Stream, !IO),
        io.write_string(Stream, ",", !IO),
        write_ptag(Stream, Ptag, !IO),
        io.write_string(Stream, ",", !IO),
        io.write_uint(Stream, Stag, !IO),
        io.write_string(Stream, ")", !IO)
    else if
        is_local_stag_test(Test, Rval, Ptag, Stag, Negated)
    then
        (
            Negated = no,
            io.write_string(Stream, "MR_LTAGS_TEST(", !IO)
        ;
            Negated = yes,
            io.write_string(Stream, "MR_LTAGS_TESTR(", !IO)
        ),
        output_rval(Info, Rval, Stream, !IO),
        io.write_string(Stream, ",", !IO),
        write_ptag(Stream, Ptag, !IO),
        io.write_string(Stream, ",", !IO),
        io.write_uint(Stream, Stag, !IO),
        io.write_string(Stream, ")", !IO)
    else if
        Test = unop(logical_not, InnerTest),
        is_local_stag_test(InnerTest, Rval, Ptag, Stag, Negated)
    then
        (
            Negated = no,
            io.write_string(Stream, "MR_LTAGS_TESTR(", !IO)
        ;
            Negated = yes,
            io.write_string(Stream, "MR_LTAGS_TEST(", !IO)
        ),
        output_rval(Info, Rval, Stream, !IO),
        io.write_string(Stream, ",", !IO),
        write_ptag(Stream, Ptag, !IO),
        io.write_string(Stream, ",", !IO),
        io.write_uint(Stream, Stag, !IO),
        io.write_string(Stream, ")", !IO)
    else
        output_rval_as_type(Info, Test, lt_bool, Stream, !IO)
    ).

:- pred is_int_cmp(rval::in, rval::out, int::out, string::out, string::out)
    is semidet.

is_int_cmp(Test, Left, RightConst, OpStr, NegOpStr) :-
    Test = binop(Op, Left, Right),
    Right = const(llconst_int(RightConst)),
    (
        Op = eq(int_type_int),
        OpStr = "MR_INT_EQ",
        NegOpStr = "MR_INT_NE"
    ;
        Op = ne(int_type_int),
        OpStr = "MR_INT_NE",
        NegOpStr = "MR_INT_EQ"
    ;
        Op = int_lt(int_type_int),
        OpStr = "MR_INT_LT",
        NegOpStr = "MR_INT_GE"
    ;
        Op = int_gt(int_type_int),
        OpStr = "MR_INT_GT",
        NegOpStr = "MR_INT_LT"
    ;
        Op = int_le(int_type_int),
        OpStr = "MR_INT_LE",
        NegOpStr = "MR_INT_GT"
    ;
        Op = int_ge(int_type_int),
        OpStr = "MR_INT_GE",
        NegOpStr = "MR_INT_LT"
    ).

:- pred is_ptag_test(rval::in, rval::out, ptag::out, bool::out) is semidet.

is_ptag_test(Test, Rval, Ptag, Negated) :-
    Test = binop(Op, Left, Right),
    Left = unop(tag, Rval),
    Right = const(llconst_int(PtagInt)),
    uint8.from_int(PtagInt, PtagUint8),
    Ptag = ptag(PtagUint8),
    (
        Op = eq(_),
        Negated = no
    ;
        Op = ne(_),
        Negated = yes
    ).

:- pred is_remote_stag_test(rval::in, rval::in, ptag::in, uint::out)
    is semidet.

is_remote_stag_test(Test, Rval, Ptag, Stag) :-
    Test = binop(eq(int_type_int), Left, Right),
    Left = lval(field(yes(Ptag), Rval, Zero)),
    Zero = const(llconst_int(0)),
    Right = const(llconst_int(StagInt)),
    uint.from_int(StagInt, Stag).

:- pred is_local_stag_test(rval::in, rval::out, ptag::out, uint::out,
    bool::out) is semidet.

is_local_stag_test(Test, Rval, Ptag, Stag, Negated) :-
    Test = binop(Op, Rval, Right),
    Right = mkword(Ptag, unop(mkbody, const(llconst_int(StagInt)))),
    uint.from_int(StagInt, Stag),
    (
        Op = eq(_),
        Negated = no
    ;
        Op = ne(_),
        Negated = yes
    ).

output_ptag(Stream, ptag(PtagUInt8), !IO) :-
    io.format(Stream, "MR_mktag(%u)", [u8(PtagUInt8)], !IO).

write_ptag(Stream, ptag(PtagUInt8), !IO) :-
    io.write_uint8(Stream, PtagUInt8, !IO).

direct_field_int_constant(LLDSType) = DirectFieldIntConstant :-
    (
        ( LLDSType = lt_bool
        ; LLDSType = lt_float
        ; LLDSType = lt_string
        ; LLDSType = lt_data_ptr
        ; LLDSType = lt_code_ptr
        ; LLDSType = lt_word
        ),
        DirectFieldIntConstant = no
    ;
        LLDSType = lt_int_least(_),
        DirectFieldIntConstant = yes
    ;
        LLDSType = lt_int(IntType),
        (
            ( IntType = int_type_int
            ; IntType = int_type_uint
            ; IntType = int_type_int8
            ; IntType = int_type_uint8
            ; IntType = int_type_int16
            ; IntType = int_type_uint16
            ; IntType = int_type_int32
            ; IntType = int_type_uint32
            ),
            DirectFieldIntConstant = yes
        ;
            ( IntType = int_type_int64
            ; IntType = int_type_uint64
            ),
            DirectFieldIntConstant = no
        )
    ).

%----------------------------------------------------------------------------%
%
% Compute the names of the global variables that hold floating point constants.
%

float_const_expr_name(Expr, Name) :-
    ( if Expr = const(llconst_float(Float)) then
        float_literal_name(Float, Name)
    else if Expr = binop(Op, Arg1, Arg2) then
        float_const_binop_expr_name(Op, Arg1, Arg2, Name)
    else
        fail
    ).

    % Given a binop rval, succeed iff that rval is a floating point constant
    % expression; if so, return a name for that rval that is suitable for use
    % in a C identifier. Different rvals must be given different names.
    %
:- pred float_const_binop_expr_name(binary_op::in, rval::in, rval::in,
    string::out) is semidet.

float_const_binop_expr_name(Op, Arg1, Arg2, Name) :-
    float_op_name(Op, OpName),
    float_const_expr_name(Arg1, Arg1Name),
    float_const_expr_name(Arg2, Arg2Name),
    % We use prefix notation (operator, argument, argument) rather than infix,
    % to ensure that different rvals get different names.
    Name = OpName ++ "_" ++ Arg1Name ++ "_" ++ Arg2Name.

    % Given an rval which is a floating point literal, return a name for that
    % rval that is suitable for use as a suffix of a C identifier.
    % Different rvals must be given different names.
    %
:- pred float_literal_name(float::in, string::out) is det.

float_literal_name(Float, FloatName) :-
    % The name of the variable is based on the value of the float const, with
    % "pt" instead of ".", "plus" instead of "+", and "neg" instead of "-".
    FloatName0 = c_util.make_float_literal(Float),
    string.replace_all(FloatName0, ".", "pt", FloatName1),
    string.replace_all(FloatName1, "+", "plus", FloatName2),
    string.replace_all(FloatName2, "-", "neg", FloatName).

    % Succeed iff the binary operator is an operator whose return
    % type is float; bind the output string to a name for that operator
    % that is suitable for use in a C identifier
    %
:- pred float_op_name(binary_op::in, string::out) is semidet.

float_op_name(float_add, "fadd").
float_op_name(float_sub, "fsub").
float_op_name(float_mul, "fmul").
float_op_name(float_div, "fdiv").

%----------------------------------------------------------------------------%

    % Given an rval which is a signed 64-bit integer literal, return a name for
    % that rval that is suitable for use as a suffix of a C identifier.
    % Different rvals must be given different names.
    %
:- pred int64_literal_name(int64::in, string::out) is det.

int64_literal_name(Int64, Int64Name) :-
    Int64Name0 = int64_to_string(Int64),
    string.replace_all(Int64Name0, "-", "neg", Int64Name).

    % Given an rval which is an unsigned 64-bit integer literal, return a name
    % for that rval that is suitable for use as a suffix of a C identifier.
    % Different rvals must be given different names.
    %
:- pred uint64_literal_name(uint64::in, string::out) is det.

uint64_literal_name(UInt64, UInt64Name) :-
    UInt64Name = uint64_to_string(UInt64).

int64_const_expr_name(Expr, Name) :-
    Expr = const(llconst_int64(Int64)),
    int64_literal_name(Int64, Name).

uint64_const_expr_name(Expr, Name) :-
    Expr = const(llconst_uint64(UInt64)),
    uint64_literal_name(UInt64, Name).

%----------------------------------------------------------------------------%
%
% Declare the names of global variables.
%

output_record_data_id_decls(Info, Stream, DataId, !DeclSet, !IO) :-
    output_record_data_id_decls_format(Info, Stream, DataId, "", "",
        0, _, !DeclSet, !IO).

output_record_data_id_decls_format(Info, Stream, DataId,
        FirstIndent, LaterIndent, !N, !DeclSet, !IO) :-
    (
        ( DataId = scalar_common_data_id(_, _)
        ; DataId = vector_common_data_id(_, _)
        ; DataId = layout_slot_id(_, _)
        )
        % These are always declared at the top of the generated C source file.
    ;
        DataId = proc_tabling_data_id(_, _)
        % These are always defined (and therefore declared) before being used.
    ;
        DataId = rtti_data_id(RttiId),
        DeclId = decl_rtti_id(RttiId),
        ( if decl_set_insert_new(DeclId, !DeclSet) then
            output_indent(Stream, FirstIndent, LaterIndent, !.N, !IO),
            !:N = !.N + 1,
            output_rtti_id_storage_type_name_no_decl(Info, Stream, RttiId,
                no, !IO),
            io.write_string(Stream, ";\n", !IO)
        else
            true
        )
    ;
        DataId = layout_id(LayoutName),
        DeclId = decl_layout_id(LayoutName),
        ( if decl_set_insert_new(DeclId, !DeclSet) then
            output_indent(Stream, FirstIndent, LaterIndent, !.N, !IO),
            !:N = !.N + 1,
            output_layout_name_storage_type_name(Stream, LayoutName,
                not_being_defined, !IO),
            io.write_string(Stream, ";\n", !IO)
        else
            true
        )
    ).

%----------------------------------------------------------------------------%
%
% Output references to global variables.
%

output_data_id(Info, Stream, DataId, !IO) :-
    (
        DataId = rtti_data_id(RttiId),
        output_rtti_id(Stream, RttiId, !IO)
    ;
        DataId = proc_tabling_data_id(ProcLabel, TablingId),
        io.write_string(Stream,
            tabling_struct_data_addr_string(ProcLabel, TablingId), !IO)
    ;
        DataId = scalar_common_data_id(TypeNum, CellNum),
        output_common_scalar_cell_array_name(Stream, TypeNum, !IO),
        io.format(Stream, "[%d]", [i(CellNum)], !IO)
    ;
        DataId = vector_common_data_id(TypeNum, CellNum),
        output_common_vector_cell_array_name(Stream, TypeNum, CellNum, !IO)
    ;
        DataId = layout_id(LayoutName),
        output_layout_name(Stream, LayoutName, !IO)
    ;
        DataId = layout_slot_id(Kind, PredProcId),
        Kind = table_io_entry_id,
        TableIoEntryMap = Info ^ lout_table_io_entry_map,
        map.lookup(TableIoEntryMap, PredProcId, LayoutSlotName),
        MangledModuleName = Info ^ lout_mangled_module_name,
        output_layout_slot_id(Stream, use_layout_macro, MangledModuleName,
            LayoutSlotName, !IO)
    ).

output_data_id_addr(Info, Stream, DataId, !IO) :-
    io.write_string(Stream, "&", !IO),
    output_data_id(Info, Stream, DataId, !IO).

output_common_scalar_cell_array_name(Stream, type_num(TypeNum), !IO) :-
    io.format(Stream, "%s%d",
        [s(mercury_scalar_common_array_prefix), i(TypeNum)], !IO).

output_common_vector_cell_array_name(Stream, type_num(TypeNum),
        CellNum, !IO) :-
    io.format(Stream, "%s%d_%d",
        [s(mercury_vector_common_array_prefix), i(TypeNum), i(CellNum)], !IO).

%---------------------------------------------------------------------------%
:- end_module ll_backend.llds_out.llds_out_data.
%---------------------------------------------------------------------------%
