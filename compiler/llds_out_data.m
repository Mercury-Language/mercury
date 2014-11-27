%----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%----------------------------------------------------------------------------%
% Copyright (C) 2009-2012 The University of Melbourne.
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

:- import_module hlds.hlds_llds.
:- import_module ll_backend.llds.
:- import_module ll_backend.llds_out.llds_out_util.

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
:- pred output_record_lval_decls(llds_out_info::in, lval::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.
:- pred output_record_lval_decls_tab(llds_out_info::in, lval::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

:- pred output_lval(llds_out_info::in, lval::in, io::di, io::uo) is det.

:- pred output_lval_for_assign(llds_out_info::in, lval::in, llds_type::out,
    io::di, io::uo) is det.

:- pred output_lval_as_word(llds_out_info::in, lval::in,
    io::di, io::uo) is det.

    % Output the given llds_type with parentheses around it.
    %
:- pred output_llds_type_cast(llds_type::in, io::di, io::uo) is det.

    % Output the given llds_type.
    %
:- pred output_llds_type(llds_type::in, io::di, io::uo) is det.

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
:- pred output_record_rval_decls(llds_out_info::in, rval::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.
:- pred output_record_rval_decls_tab(llds_out_info::in, rval::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

:- pred output_record_rvals_decls(llds_out_info::in, list(rval)::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

    % Output an rval, not converted to any particular type,
    % but instead output as its "natural" type.
    %
:- pred output_rval(llds_out_info::in, rval::in, io::di, io::uo) is det.

    % Output an rval, converted to the specified type
    %
:- pred output_rval_as_type(llds_out_info::in, rval::in, llds_type::in,
    io::di, io::uo) is det.

:- pred output_test_rval(llds_out_info::in, rval::in, io::di, io::uo) is det.

:- pred output_tag(tag::in, io::di, io::uo) is det.

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

    % output_record_data_addr_decls(Info, DataId, ...) outputs the
    % declarations of any static constants, etc. that need to be declared
    % before output_data_id(Info DataId, ...) is called.
    %
:- pred output_record_data_id_decls(llds_out_info::in, data_id::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.
:- pred output_record_data_id_decls_format(llds_out_info::in, data_id::in,
    string::in, string::in, int::in, int::out, decl_set::in, decl_set::out,
    io::di, io::uo) is det.

    % Output the name of the global variable identified by the data_id.
    %
:- pred output_data_id(llds_out_info::in, data_id::in, io::di, io::uo)
    is det.

    % Output the address of the global variable identified by the data_id.
    %
:- pred output_data_id_addr(llds_out_info::in, data_id::in, io::di, io::uo)
    is det.

:- pred output_common_scalar_cell_array_name(type_num::in, io::di, io::uo)
    is det.

:- pred output_common_vector_cell_array_name(type_num::in, int::in,
    io::di, io::uo) is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.builtin_ops.
:- import_module backend_libs.c_util.
:- import_module backend_libs.name_mangle.
:- import_module backend_libs.rtti.
:- import_module ll_backend.layout_out.
:- import_module ll_backend.llds_out.llds_out_code_addr.
:- import_module ll_backend.rtti_out.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_foreign.

:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module string.

%----------------------------------------------------------------------------%
%
% Declaring lvals.
%

output_record_lval_decls(Info, Lval, !DeclSet, !IO) :-
    output_record_lval_decls_format(Info, Lval, "", "", 0, _, !DeclSet, !IO).

output_record_lval_decls_tab(Info, Lval, !DeclSet, !IO) :-
    output_record_lval_decls_format(Info, Lval, "\t", "\t", 0, _,
        !DeclSet, !IO).

:- pred output_record_lval_decls_format(llds_out_info::in, lval::in,
    string::in, string::in, int::in, int::out, decl_set::in, decl_set::out,
    io::di, io::uo) is det.

output_record_lval_decls_format(Info, Lval, FirstIndent, LaterIndent,
        !N, !DeclSet, !IO) :-
    (
        Lval = field(_, Rval, FieldNum),
        output_record_rval_decls_format(Info, Rval,
            FirstIndent, LaterIndent, !N, !DeclSet, !IO),
        output_record_rval_decls_format(Info, FieldNum,
            FirstIndent, LaterIndent, !N, !DeclSet, !IO)
    ;
        ( Lval = succfr_slot(Rval)
        ; Lval = prevfr_slot(Rval)
        ; Lval = redofr_slot(Rval)
        ; Lval = redoip_slot(Rval)
        ; Lval = succip_slot(Rval)
        ; Lval = mem_ref(Rval)
        ),
        output_record_rval_decls_format(Info, Rval,
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
        ( decl_set_is_member(decl_c_global_var(CGlobalVar), !.DeclSet) ->
            true
        ;
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

output_lval(Info, Lval, !IO) :-
    (
        Lval = reg(Type, Num),
        output_reg(Type, Num, !IO)
    ;
        Lval = stackvar(N),
        ( N =< 0 ->
            unexpected($file, $pred, "stack var out of range")
        ;
            true
        ),
        io.write_string("MR_sv(", !IO),
        io.write_int(N, !IO),
        io.write_string(")", !IO)
    ;
        Lval = parent_stackvar(N),
        ( N =< 0 ->
            unexpected($file, $pred, "parent stack var out of range")
        ;
            true
        ),
        io.write_string("MR_parent_sv(", !IO),
        io.write_int(N, !IO),
        io.write_string(")", !IO)
    ;
        Lval = framevar(N),
        ( N =< 0 ->
            unexpected($file, $pred, "frame var out of range")
        ;
            true
        ),
        io.write_string("MR_fv(", !IO),
        io.write_int(N, !IO),
        io.write_string(")", !IO)
    ;
        Lval = double_stackvar(StackType, SlotNum),
        io.write_string("MR_float_from_dword_ptr(", !IO),
        output_double_stackvar_ptr(Info, StackType, SlotNum, !IO),
        io.write_string(")", !IO)
    ;
        Lval = succip,
        io.write_string("MR_succip", !IO)
    ;
        Lval = sp,
        io.write_string("MR_sp", !IO)
    ;
        Lval = parent_sp,
        io.write_string("MR_parent_sp", !IO)
    ;
        Lval = hp,
        io.write_string("MR_hp", !IO)
    ;
        Lval = maxfr,
        io.write_string("MR_maxfr", !IO)
    ;
        Lval = curfr,
        io.write_string("MR_curfr", !IO)
    ;
        Lval = succfr_slot(Rval),
        io.write_string("MR_succfr_slot(", !IO),
        output_rval(Info, Rval, !IO),
        io.write_string(")", !IO)
    ;
        Lval = prevfr_slot(Rval),
        io.write_string("MR_prevfr_slot(", !IO),
        output_rval(Info, Rval, !IO),
        io.write_string(")", !IO)
    ;
        Lval = redofr_slot(Rval),
        io.write_string("MR_redofr_slot(", !IO),
        output_rval(Info, Rval, !IO),
        io.write_string(")", !IO)
    ;
        Lval = redoip_slot(Rval),
        io.write_string("MR_redoip_slot(", !IO),
        output_rval(Info, Rval, !IO),
        io.write_string(")", !IO)
    ;
        Lval = succip_slot(Rval),
        io.write_string("MR_succip_slot(", !IO),
        output_rval(Info, Rval, !IO),
        io.write_string(")", !IO)
    ;
        Lval = field(MaybeTag, Rval, FieldNumRval),
        (
            MaybeTag = yes(Tag),
            io.write_string("MR_tfield(", !IO),
            io.write_int(Tag, !IO),
            io.write_string(", ", !IO)
        ;
            MaybeTag = no,
            io.write_string("MR_mask_field(", !IO)
        ),
        output_rval(Info, Rval, !IO),
        io.write_string(", ", !IO),
        ( FieldNumRval = const(llconst_int(FieldNum)) ->
            % Avoid emitting the (MR_Integer) cast.
            io.write_int(FieldNum, !IO)
        ;
            output_rval(Info, FieldNumRval, !IO)
        ),
        io.write_string(")", !IO)
    ;
        Lval = lvar(_),
        unexpected($file, $pred, "lvar")
    ;
        Lval = temp(Type, Num),
        (
            Type = reg_r,
            io.write_string("MR_tempr", !IO),
            io.write_int(Num, !IO)
        ;
            Type = reg_f,
            io.write_string("MR_tempf", !IO),
            io.write_int(Num, !IO)
        )
    ;
        Lval = mem_ref(Rval),
        io.write_string("* (MR_Word *) (", !IO),
        output_rval(Info, Rval, !IO),
        io.write_string(")", !IO)
    ;
        Lval = global_var_ref(GlobalVar),
        io.write_string(c_global_var_name(GlobalVar), !IO)
    ).

output_lval_for_assign(Info, Lval, Type, !IO) :-
    (
        Lval = reg(RegType, Num),
        (
            RegType = reg_r,
            Type = lt_word
        ;
            RegType = reg_f,
            Type = lt_float
        ),
        output_reg(RegType, Num, !IO)
    ;
        Lval = stackvar(N),
        Type = lt_word,
        ( N < 0 ->
            unexpected($file, $pred, "stack var out of range")
        ;
            true
        ),
        io.write_string("MR_sv(", !IO),
        io.write_int(N, !IO),
        io.write_string(")", !IO)
    ;
        Lval = parent_stackvar(N),
        Type = lt_word,
        ( N < 0 ->
            unexpected($file, $pred, "parent stack var out of range")
        ;
            true
        ),
        io.write_string("MR_parent_sv(", !IO),
        io.write_int(N, !IO),
        io.write_string(")", !IO)
    ;
        Lval = framevar(N),
        Type = lt_word,
        ( N =< 0 ->
            unexpected($file, $pred, "frame var out of range")
        ;
            true
        ),
        io.write_string("MR_fv(", !IO),
        io.write_int(N, !IO),
        io.write_string(")", !IO)
    ;
        Lval = double_stackvar(StackType, SlotNum),
        Type = lt_float,
        io.write_string("* (MR_Float *) ", !IO),
        output_double_stackvar_ptr(Info, StackType, SlotNum, !IO)
    ;
        Lval = succip,
        Type = lt_word,
        io.write_string("MR_succip_word", !IO)
    ;
        Lval = sp,
        Type = lt_word,
        io.write_string("MR_sp_word", !IO)
    ;
        Lval = parent_sp,
        Type = lt_data_ptr,
        io.write_string("MR_parent_sp", !IO)
    ;
        Lval = hp,
        Type = lt_word,
        io.write_string("MR_hp_word", !IO)
    ;
        Lval = maxfr,
        Type = lt_word,
        io.write_string("MR_maxfr_word", !IO)
    ;
        Lval = curfr,
        Type = lt_word,
        io.write_string("MR_curfr_word", !IO)
    ;
        Lval = succfr_slot(Rval),
        Type = lt_word,
        io.write_string("MR_succfr_slot_word(", !IO),
        output_rval(Info, Rval, !IO),
        io.write_string(")", !IO)
    ;
        Lval = prevfr_slot(Rval),
        Type = lt_word,
        io.write_string("MR_prevfr_slot_word(", !IO),
        output_rval(Info, Rval, !IO),
        io.write_string(")", !IO)
    ;
        Lval = redofr_slot(Rval),
        Type = lt_word,
        io.write_string("MR_redofr_slot_word(", !IO),
        output_rval(Info, Rval, !IO),
        io.write_string(")", !IO)
    ;
        Lval = redoip_slot(Rval),
        Type = lt_word,
        io.write_string("MR_redoip_slot_word(", !IO),
        output_rval(Info, Rval, !IO),
        io.write_string(")", !IO)
    ;
        Lval = succip_slot(Rval),
        Type = lt_word,
        io.write_string("MR_succip_slot_word(", !IO),
        output_rval(Info, Rval, !IO),
        io.write_string(")", !IO)
    ;
        Lval = field(MaybeTag, Rval, FieldNumRval),
        Type = lt_word,
        (
            MaybeTag = yes(Tag),
            io.write_string("MR_tfield(", !IO),
            io.write_int(Tag, !IO),
            io.write_string(", ", !IO)
        ;
            MaybeTag = no,
            io.write_string("MR_mask_field(", !IO)
        ),
        output_rval(Info, Rval, !IO),
        io.write_string(", ", !IO),
        ( FieldNumRval = const(llconst_int(FieldNum)) ->
            % Avoid emitting the (MR_Integer) cast.
            io.write_int(FieldNum, !IO)
        ;
            output_rval(Info, FieldNumRval, !IO)
        ),
        io.write_string(")", !IO)
    ;
        Lval = lvar(_),
        unexpected($file, $pred, "lvar")
    ;
        Lval = temp(RegType, Num),
        (
            RegType = reg_r,
            Type = lt_word,
            io.write_string("MR_tempr", !IO),
            io.write_int(Num, !IO)
        ;
            RegType = reg_f,
            Type = lt_float,
            io.write_string("MR_tempf", !IO),
            io.write_int(Num, !IO)
        )
    ;
        Lval = mem_ref(_MemRef),
        Type = lt_word,
        output_lval(Info, Lval, !IO)
    ;
        Lval = global_var_ref(GlobalVar),
        Type = lt_word,
        io.write_string(c_global_var_name(GlobalVar), !IO)
    ).

output_lval_as_word(Info, Lval, !IO) :-
    llds.lval_type(Lval, ActualType),
    ( llds_types_match(lt_word, ActualType) ->
        output_lval(Info, Lval, !IO)
    ; ActualType = lt_float ->
        % Sanity check -- if this happens, the LLDS is ill-typed.
        unexpected($file, $pred, "float")
    ;
        io.write_string("MR_LVALUE_CAST(MR_Word,", !IO),
        output_lval(Info, Lval, !IO),
        io.write_string(")", !IO)
    ).

:- pred output_double_stackvar_ptr(llds_out_info::in,
    double_stack_type::in, int::in, io::di, io::uo) is det.

output_double_stackvar_ptr(Info, StackType, SlotNum, !IO) :-
    % The higher-numbered slot has the lower address because our stacks grow
    % downwards.
    (
        StackType = double_stackvar,
        Lval = stackvar(SlotNum + 1)
    ;
        StackType = double_parent_stackvar,
        Lval = parent_stackvar(SlotNum + 1)
    ),
    io.write_string("MR_dword_ptr(&(", !IO),
    output_lval(Info, Lval, !IO),
    io.write_string("))", !IO).

    % llds_types_match(DesiredType, ActualType) is true iff
    % a value of type ActualType can be used as a value of
    % type DesiredType without casting.
    %
:- pred llds_types_match(llds_type::in, llds_type::in) is semidet.

llds_types_match(Type, Type).
llds_types_match(lt_word, lt_unsigned).
llds_types_match(lt_word, lt_integer).
llds_types_match(lt_word, lt_bool).
llds_types_match(lt_bool, lt_integer).
llds_types_match(lt_bool, lt_unsigned).
llds_types_match(lt_bool, lt_word).
llds_types_match(lt_integer, lt_bool).

output_llds_type_cast(LLDSType, !IO) :-
    io.write_string("(", !IO),
    output_llds_type(LLDSType, !IO),
    io.write_string(") ", !IO).

output_llds_type(lt_int_least8, !IO) :-
    io.write_string("MR_int_least8_t", !IO).
output_llds_type(lt_uint_least8, !IO) :-
    io.write_string("MR_uint_least8_t", !IO).
output_llds_type(lt_int_least16, !IO) :-
    io.write_string("MR_int_least16_t", !IO).
output_llds_type(lt_uint_least16, !IO) :-
    io.write_string("MR_uint_least16_t", !IO).
output_llds_type(lt_int_least32, !IO) :-
    io.write_string("MR_int_least32_t", !IO).
output_llds_type(lt_uint_least32, !IO) :-
    io.write_string("MR_uint_least32_t", !IO).
output_llds_type(lt_bool, !IO) :-
    io.write_string("MR_Integer", !IO).
output_llds_type(lt_integer, !IO) :-
    io.write_string("MR_Integer", !IO).
output_llds_type(lt_unsigned, !IO) :-
    io.write_string("MR_Unsigned", !IO).
output_llds_type(lt_float, !IO) :-
    io.write_string("MR_Float", !IO).
output_llds_type(lt_word, !IO) :-
    io.write_string("MR_Word", !IO).
output_llds_type(lt_string, !IO) :-
    io.write_string("MR_String", !IO).
output_llds_type(lt_data_ptr, !IO) :-
    io.write_string("MR_Word *", !IO).
output_llds_type(lt_code_ptr, !IO) :-
    io.write_string("MR_Code *", !IO).

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
    ( N =< max_real_r_reg ->
        "MR_r" ++ int_to_string(N)
    ; N =< max_virtual_r_reg ->
        "MR_r(" ++ int_to_string(N) ++ ")"
    ;
        unexpected($file, $pred, "register number too large")
    ).
reg_to_string(reg_f, N) =
    ( N =< max_virtual_f_reg ->
        "MR_f(" ++ int_to_string(N) ++ ")"
    ;
        unexpected($file, $pred, "register number too large")
    ).

:- func max_real_r_reg = int.
:- func max_virtual_r_reg = int.

max_real_r_reg = 32.
max_virtual_r_reg = 1024.

:- func max_virtual_f_reg = int.

max_virtual_f_reg = 1024.

:- pred output_reg(reg_type::in, int::in, io::di, io::uo) is det.

output_reg(RegType, N, !IO) :-
    io.write_string(reg_to_string(RegType, N), !IO).

% The calls to env_var_is_acceptable_char in prog_io_goal.m ensure that
% EnvVarName is acceptable as part of a C identifier.
% The prefix must be identical to envvar_prefix in util/mkinit.c and
% global_var_name in mlds_to_c.m.
c_global_var_name(env_var_ref(EnvVarName)) = "mercury_envvar_" ++ EnvVarName.

%----------------------------------------------------------------------------%
%
% Declaring rvals.
%

output_record_rval_decls(Info, Rval, !DeclSet, !IO) :-
    output_record_rval_decls_format(Info, Rval, "", "", 0, _, !DeclSet, !IO).

output_record_rval_decls_tab(Info, Rval, !DeclSet, !IO) :-
    output_record_rval_decls_format(Info, Rval, "", "\t", 0, _, !DeclSet, !IO).

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
    % set of symbols we've already declared. That way, we avoid generating
    % the same symbol twice, which would cause an error in the C code.
    %
:- pred output_record_rval_decls_format(llds_out_info::in, rval::in,
    string::in, string::in, int::in, int::out,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_record_rval_decls_format(Info, Rval, FirstIndent, LaterIndent,
        !N, !DeclSet, !IO) :-
    (
        Rval = lval(Lval),
        output_record_lval_decls_format(Info, Lval,
            FirstIndent, LaterIndent, !N, !DeclSet, !IO)
    ;
        Rval = var(_),
        unexpected($file, $pred, "var")
    ;
        Rval = mkword(_, SubRval),
        output_record_rval_decls_format(Info, SubRval,
            FirstIndent, LaterIndent, !N, !DeclSet, !IO)
    ;
        Rval = mkword_hole(_)
    ;
        Rval = const(Const),
        ( Const = llconst_code_addr(CodeAddress) ->
            output_record_code_addr_decls_format(Info, CodeAddress,
                FirstIndent, LaterIndent, !N, !DeclSet, !IO)
        ; Const = llconst_data_addr(DataId, _) ->
            output_record_data_id_decls_format(Info, DataId,
                FirstIndent, LaterIndent, !N, !DeclSet, !IO)
        ; Const = llconst_float(FloatVal) ->
            % If floats are boxed, but are allocated statically, then for each
            % float constant which we might want to box we declare a static
            % const variable holding that constant.

            UnboxedFloat = Info ^ lout_unboxed_float,
            StaticGroundFloats = Info ^ lout_static_ground_floats,
            (
                UnboxedFloat = no,
                StaticGroundFloats = yes
            ->
                float_literal_name(FloatVal, FloatName),
                FloatLabel = decl_float_label(FloatName),
                ( decl_set_is_member(FloatLabel, !.DeclSet) ->
                    true
                ;
                    decl_set_insert(FloatLabel, !DeclSet),
                    FloatString = c_util.make_float_literal(FloatVal),
                    output_indent(FirstIndent, LaterIndent, !.N, !IO),
                    !:N = !.N + 1,
                    io.write_strings(["static const MR_Float ",
                        "mercury_float_const_", FloatName, " = ", FloatString,
                        ";\n"
                    ], !IO)
                )
            ;
                true
            )
        ;
            true
        )
    ;
        Rval = unop(_, SubRvalA),
        output_record_rval_decls_format(Info, SubRvalA,
            FirstIndent, LaterIndent, !N, !DeclSet, !IO)
    ;
        Rval = binop(Op, SubRvalA, SubRvalB),
        output_record_rval_decls_format(Info, SubRvalA,
            FirstIndent, LaterIndent, !N, !DeclSet, !IO),
        output_record_rval_decls_format(Info, SubRvalB,
            FirstIndent, LaterIndent, !N, !DeclSet, !IO),

        % If floats are boxed, and the static ground terms option is enabled,
        % then for each float constant which we might want to box we declare
        % a static const variable holding that constant.

        c_util.binop_category_string(Op, Category, OpStr),
        ( Category = float_arith_binop ->
            UnboxFloat = Info ^ lout_unboxed_float,
            StaticGroundFloats = Info ^ lout_static_ground_floats,
            (
                UnboxFloat = no,
                StaticGroundFloats = yes,
                float_const_binop_expr_name(Op, SubRvalA, SubRvalB, FloatName)
            ->
                FloatLabel = decl_float_label(FloatName),
                ( decl_set_is_member(FloatLabel, !.DeclSet) ->
                    true
                ;
                    decl_set_insert(FloatLabel, !DeclSet),
                    output_indent(FirstIndent, LaterIndent, !.N, !IO),
                    !:N = !.N + 1,
                    io.write_string("static const ", !IO),
                    output_llds_type(lt_float, !IO),
                    io.write_string(" mercury_float_const_", !IO),
                    io.write_string(FloatName, !IO),
                    io.write_string(" = ", !IO),
                    % Note that we just output the expression here, and
                    % let the C compiler evaluate it, rather than evaluating
                    % it ourselves. This avoids having to deal with some nasty
                    % issues regarding floating point accuracy when doing
                    % cross-compilation.
                    output_rval_as_type(Info, SubRvalA, lt_float, !IO),
                    io.write_string(" ", !IO),
                    io.write_string(OpStr, !IO),
                    io.write_string(" ", !IO),
                    output_rval_as_type(Info, SubRvalB, lt_float, !IO),
                    io.write_string(";\n", !IO)
                )
            ;
                true
            )
        ;
            true
        )
    ;
        Rval = mem_addr(MemRef),
        output_record_mem_ref_decls_format(Info, MemRef,
            FirstIndent, LaterIndent, !N, !DeclSet, !IO)
    ).

output_record_rvals_decls(Info, Rvals, !DeclSet, !IO) :-
    output_record_rvals_decls_format(Info, Rvals, "", "", 0, _,
        !DeclSet, !IO).

:- pred output_record_rvals_decls_format(llds_out_info::in, list(rval)::in,
    string::in, string::in, int::in, int::out, decl_set::in,
    decl_set::out, io::di, io::uo) is det.

output_record_rvals_decls_format(_, [], _, _, !N, !DeclSet, !IO).
output_record_rvals_decls_format(Info, Rvals @ [_ | _],
        FirstIndent, LaterIndent, !N, !DeclSet, !IO) :-
    output_record_rvals_decls_format_count(Info, Rvals, LeftOverRvals, 1000,
        FirstIndent, LaterIndent, !N, !DeclSet, !IO),
    output_record_rvals_decls_format(Info, LeftOverRvals,
        FirstIndent, LaterIndent, !N, !DeclSet, !IO).

    % We use this predicate to output the declarations of up to Count rvals.
    % It is separate from output_record_rvals_decls_format so that in grades
    % that do not permit tail recursion, we can free up the stack frames
    % occupied by a bunch of loop iterations before the declarations of *all*
    % the rvals have been output.
    %
:- pred output_record_rvals_decls_format_count(llds_out_info::in,
    list(rval)::in, list(rval)::out, int::in,
    string::in, string::in, int::in, int::out, decl_set::in,
    decl_set::out, io::di, io::uo) is det.

output_record_rvals_decls_format_count(_, [], [], _, _, _, !N, !DeclSet, !IO).
output_record_rvals_decls_format_count(Info, [Rval | Rvals], LeftOverRvals,
        Count, FirstIndent, LaterIndent, !N, !DeclSet, !IO) :-
    ( Count > 0 ->
        output_record_rval_decls_format(Info, Rval,
            FirstIndent, LaterIndent, !N, !DeclSet, !IO),
        output_record_rvals_decls_format_count(Info, Rvals, LeftOverRvals,
            Count - 1, FirstIndent, LaterIndent, !N, !DeclSet, !IO)
    ;
        LeftOverRvals = [Rval | Rvals]
    ).

:- pred output_record_mem_ref_decls_format(llds_out_info::in, mem_ref::in,
    string::in, string::in, int::in, int::out, decl_set::in, decl_set::out,
    io::di, io::uo) is det.

output_record_mem_ref_decls_format(Info, MemRef, FirstIndent, LaterIndent,
        !N, !DeclSet, !IO) :-
    (
        ( MemRef = stackvar_ref(Rval)
        ; MemRef = framevar_ref(Rval)
        ),
        output_record_rval_decls_format(Info, Rval,
            FirstIndent, LaterIndent, !N, !DeclSet, !IO)
    ;
        MemRef = heap_ref(BaseRval, _, OffsetRval),
        output_record_rval_decls_format(Info, BaseRval,
            FirstIndent, LaterIndent, !N, !DeclSet, !IO),
        output_record_rval_decls_format(Info, OffsetRval,
            FirstIndent, LaterIndent, !N, !DeclSet, !IO)
    ).

%----------------------------------------------------------------------------%
%
% Writing rvals.
%

output_rval(Info, Rval, !IO) :-
    (
        Rval = const(Const),
        output_rval_const(Info, Const, !IO)
    ;
        Rval = unop(UnaryOp, SubRvalA),
        c_util.unary_prefix_op(UnaryOp, OpString),
        io.write_string(OpString, !IO),
        io.write_string("(", !IO),
        llds.unop_arg_type(UnaryOp, ArgType),
        output_rval_as_type(Info, SubRvalA, ArgType, !IO),
        io.write_string(")", !IO)
    ;
        Rval = binop(Op, SubRvalA, SubRvalB),
        binop_category_string(Op, Category, OpStr),
        (
            Category = array_index_binop,
            io.write_string("(", !IO),
            output_rval_as_type(Info, SubRvalA, lt_data_ptr, !IO),
            io.write_string(")[", !IO),
            output_rval_as_type(Info, SubRvalB, lt_integer, !IO),
            io.write_string("]", !IO)
        ;
            Category = pointer_compare_binop,
            io.write_string("(((MR_Word) ", !IO),
            output_rval(Info, SubRvalA, !IO),
            io.write_string(") ", !IO),
            io.write_string(OpStr, !IO),
            io.write_string(" ((MR_Word) ", !IO),
            output_rval(Info, SubRvalB, !IO),
            io.write_string("))", !IO)
        ;
            Category = compound_compare_binop,
            % These operators are intended to be generated only when using
            % the Erlang backend.
            unexpected($file, $pred, "compound_compare_binop")
        ;
            Category = string_compare_binop,
            io.write_string("(strcmp(", !IO),
            ( SubRvalA = const(llconst_string(SubRvalAConst)) ->
                output_rval_const(Info, llconst_string(SubRvalAConst), !IO)
            ;
                io.write_string("(char *) ", !IO),
                output_rval_as_type(Info, SubRvalA, lt_data_ptr, !IO)
            ),
            io.write_string(", ", !IO),
            ( SubRvalB = const(llconst_string(SubRvalBConst)) ->
                output_rval_const(Info, llconst_string(SubRvalBConst), !IO)
            ;
                io.write_string("(char *) ", !IO),
                output_rval_as_type(Info, SubRvalB, lt_data_ptr, !IO)
            ),
            io.write_string(")", !IO),
            io.write_string(" ", !IO),
            io.write_string(OpStr, !IO),
            io.write_string(" ", !IO),
            io.write_string("0)", !IO)
        ;
            ( Category = float_compare_binop
            ; Category = float_arith_binop
            ),
            io.write_string("(", !IO),
            output_rval_as_type(Info, SubRvalA, lt_float, !IO),
            io.write_string(" ", !IO),
            io.write_string(OpStr, !IO),
            io.write_string(" ", !IO),
            output_rval_as_type(Info, SubRvalB, lt_float, !IO),
            io.write_string(")", !IO)
        ;
            Category = unsigned_compare_binop,
            io.write_string("(", !IO),
            output_rval_as_type(Info, SubRvalA, lt_unsigned, !IO),
            io.write_string(" ", !IO),
            io.write_string(OpStr, !IO),
            io.write_string(" ", !IO),
            output_rval_as_type(Info, SubRvalB, lt_unsigned, !IO),
            io.write_string(")", !IO)
        ;
            Category = int_or_bool_binary_infix_binop,
            (
                % Special-case equality ops to avoid some unnecessary casts --
                % there's no difference between signed and unsigned equality,
                % so if both args are unsigned, we don't need to cast them to
                % MR_Integer.
                ( Op = eq ; Op = ne ),
                llds.rval_type(SubRvalA, SubRvalAType),
                ( SubRvalAType = lt_word ; SubRvalAType = lt_unsigned ),
                llds.rval_type(SubRvalB, SubRvalBType),
                ( SubRvalBType = lt_word ; SubRvalBType = lt_unsigned )
            ->
                io.write_string("(", !IO),
                output_rval(Info, SubRvalA, !IO),
                io.write_string(" ", !IO),
                io.write_string(OpStr, !IO),
                io.write_string(" ", !IO),
                output_rval(Info, SubRvalB, !IO),
                io.write_string(")", !IO)
        %   ;
        %       XXX broken for C == minint
        %       (since `NewC = 0 - C' overflows)
        %       Op = (+),
        %       SubRvalB = const(llconst_int(C)),
        %       C < 0
        %   ->
        %       NewOp = (-),
        %       NewC = 0 - C,
        %       NewSubRvalB = const(llconst_int(NewC)),
        %       io.write_string("("),
        %       output_rval(SubRvalA),
        %       io.write_string(" "),
        %       io.write_string(NewOpStr),
        %       io.write_string(" "),
        %       output_rval(NewSubRvalB),
        %       io.write_string(")")
            ;
                io.write_string("(", !IO),
                output_rval_as_type(Info, SubRvalA, lt_integer, !IO),
                io.write_string(" ", !IO),
                io.write_string(OpStr, !IO),
                io.write_string(" ", !IO),
                output_rval_as_type(Info, SubRvalB, lt_integer, !IO),
                io.write_string(")", !IO)
            )
        ;
            Category = macro_binop,
            io.write_string(OpStr, !IO),
            io.write_string("(", !IO),
            output_rval_as_type(Info, SubRvalA, lt_integer, !IO),
            io.write_string(", ", !IO),
            output_rval_as_type(Info, SubRvalB, lt_integer, !IO),
            io.write_string(")", !IO)
        ;
            Category = float_macro_binop,
            (
                Op = float_word_bits
            ->
                io.write_string(OpStr, !IO),
                io.write_string("(", !IO),
                output_rval_as_type(Info, SubRvalA, lt_float, !IO),
                io.write_string(", ", !IO),
                output_rval_as_type(Info, SubRvalB, lt_integer, !IO),
                io.write_string(")", !IO)
            ;
                Op = float_from_dword
            ->
                ( is_aligned_dword_ptr(SubRvalA, SubRvalB, MemRef) ->
                    io.write_string("MR_float_from_dword_ptr(MR_dword_ptr(",
                        !IO),
                    output_rval(Info, mem_addr(MemRef), !IO),
                    io.write_string("))", !IO)
                ;
                    io.write_string("MR_float_from_dword(", !IO),
                    output_rval(Info, SubRvalA, !IO),
                    io.write_string(", ", !IO),
                    output_rval(Info, SubRvalB, !IO),
                    io.write_string(")", !IO)
                )
            ;
                sorry($file, $pred, "unknown float_macro_binop")
            )
        )
    ;
        Rval = mkword(Tag, SubRval),
        (
            SubRval = const(llconst_data_addr(DataId, no)),
            DataId = scalar_common_data_id(type_num(TypeNum), CellNum)
        ->
            io.write_string("MR_TAG_COMMON(", !IO),
            io.write_int(Tag, !IO),
            io.write_string(",", !IO),
            io.write_int(TypeNum, !IO),
            io.write_string(",", !IO),
            io.write_int(CellNum, !IO),
            io.write_string(")", !IO)
        ;
            SubRval = unop(mkbody, const(llconst_int(Body)))
        ->
            io.write_string("MR_tbmkword(", !IO),
            io.write_int(Tag, !IO),
            io.write_string(", ", !IO),
            io.write_int(Body, !IO),
            io.write_string(")", !IO)
        ;
            io.write_string("MR_tmkword(", !IO),
            io.write_int(Tag, !IO),
            io.write_string(", ", !IO),
            output_rval_as_type(Info, SubRval, lt_data_ptr, !IO),
            io.write_string(")", !IO)
        )
    ;
        Rval = mkword_hole(Tag),
        io.write_string("MR_tmkword(", !IO),
        io.write_int(Tag, !IO),
        io.write_string(", 0)", !IO)
    ;
        Rval = lval(Lval),
        % If a field is used as an rval, then we need to use the
        % MR_const_field() macro or its variants, not the MR_field() macro
        % or its variants, to avoid warnings about discarding const.
        ( Lval = field(MaybeTag, Rval, FieldNumRval) ->
            (
                MaybeTag = yes(Tag),
                io.write_string("MR_ctfield(", !IO),
                io.write_int(Tag, !IO),
                io.write_string(", ", !IO)
            ;
                MaybeTag = no,
                io.write_string("MR_const_mask_field(", !IO)
            ),
            output_rval(Info, Rval, !IO),
            io.write_string(", ", !IO),
            ( FieldNumRval = const(llconst_int(FieldNum)) ->
                % Avoid emitting the (MR_Integer) cast.
                io.write_int(FieldNum, !IO)
            ;
                output_rval(Info, FieldNumRval, !IO)
            ),
            io.write_string(")", !IO)
        ;
            output_lval(Info, Lval, !IO)
        )
    ;
        Rval = var(_),
        unexpected($file, $pred, "cannot output a var(_) expression in code")
    ;
        Rval = mem_addr(MemRef),
        (
            MemRef = stackvar_ref(SubRval),
            io.write_string("&MR_sv(", !IO),
            % Don't clutter the output with unnecessary casts.
            ( SubRval = const(llconst_int(SlotNum)) ->
                io.write_int(SlotNum, !IO)
            ;
                output_rval_as_type(Info, SubRval, lt_integer, !IO)
            ),
            io.write_string(")", !IO)
        ;
            MemRef = framevar_ref(SubRval),
            io.write_string("&MR_fv(", !IO),
            % Don't clutter the output with unnecessary casts.
            ( SubRval = const(llconst_int(SlotNum)) ->
                io.write_int(SlotNum, !IO)
            ;
                output_rval_as_type(Info, SubRval, lt_integer, !IO)
            ),
            io.write_string(")", !IO)
        ;
            MemRef = heap_ref(BaseRval, MaybeTag, FieldNumRval),
            (
                MaybeTag = yes(Tag),
                io.write_string("&MR_tfield(", !IO),
                io.write_int(Tag, !IO),
                io.write_string(", ", !IO)
            ;
                MaybeTag = no,
                io.write_string("&MR_mask_field(", !IO)
            ),
            output_rval(Info, BaseRval, !IO),
            io.write_string(", ", !IO),
            % Don't clutter the output with unnecessary casts.
            ( FieldNumRval = const(llconst_int(FieldNum)) ->
                io.write_int(FieldNum, !IO)
            ;
                output_rval_as_type(Info, FieldNumRval, lt_integer, !IO)
            ),
            io.write_string(")", !IO)
        )
    ).

:- pred output_rval_const(llds_out_info::in, rval_const::in,
    io::di, io::uo) is det.

output_rval_const(Info, Const, !IO) :-
    (
        Const = llconst_true,
        io.write_string("MR_TRUE", !IO)
    ;
        Const = llconst_false,
        io.write_string("MR_FALSE", !IO)
    ;
        Const = llconst_int(N),
        % We need to cast to (MR_Integer) to ensure things like 1 << 32 work
        % when `MR_Integer' is 64 bits but `int' is 32 bits.
        output_llds_type_cast(lt_integer, !IO),
        io.write_int(N, !IO)
    ;
        Const = llconst_foreign(Value, Type),
        io.write_char('(', !IO),
        output_llds_type_cast(Type, !IO),
        io.write_string(Value, !IO),
        io.write_char(')', !IO)
    ;
        Const = llconst_float(FloatVal),
        % The cast to (MR_Float) here lets the C compiler do arithmetic in
        % `float' rather than `double' if `MR_Float' is `float' not `double'.
        output_llds_type_cast(lt_float, !IO),
        c_util.output_float_literal(FloatVal, !IO)
    ;
        Const = llconst_string(String),
        io.write_string("MR_string_const(""", !IO),
        c_util.output_quoted_string(String, !IO),
        io.write_string(""", ", !IO),
        io.write_int(string.count_utf8_code_units(String), !IO),
        io.write_string(")", !IO)
    ;
        Const = llconst_multi_string(Strings),
        io.write_string("MR_string_const(""", !IO),
        c_util.output_quoted_multi_string(Strings, !IO),
        io.write_string(""", ", !IO),

        % The "+1" is for the NULL character.
        SumLengths = (func(S, L0) = L0 + string.count_utf8_code_units(S) + 1),
        Length = list.foldl(SumLengths, Strings, 0),
        io.write_int(Length, !IO),
        io.write_string(")", !IO)
    ;
        Const = llconst_code_addr(CodeAddress),
        output_code_addr(CodeAddress, !IO)
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
            (
                DataId = scalar_common_data_id(type_num(TypeNum), CellNum)
            ->
                io.write_string("MR_COMMON(", !IO),
                io.write_int(TypeNum, !IO),
                io.write_string(",", !IO),
                io.write_int(CellNum, !IO),
                io.write_string(")", !IO)
            ;
                DataId = rtti_data_id(RttiId),
                rtti_id_emits_type_ctor_info(RttiId, Ctor),
                Ctor = rtti_type_ctor(Module, Name, Arity),
                sym_name_doesnt_need_mangling(Module),
                name_doesnt_need_mangling(Name)
            ->
                output_type_ctor_addr(Module, Name, Arity, !IO)
            ;
                output_llds_type_cast(lt_data_ptr, !IO),
                output_data_id_addr(Info, DataId, !IO)
            )
        ;
            MaybeOffset = yes(Offset),
            io.write_string("((", !IO),
            output_llds_type_cast(lt_data_ptr, !IO),
            output_data_id_addr(Info, DataId, !IO),
            io.write_string(") + ", !IO),
            io.write_int(Offset, !IO),
            io.write_string(")", !IO)
        )
    ).

:- pred output_type_ctor_addr(module_name::in, string::in, int::in,
    io::di, io::uo) is det.

output_type_ctor_addr(Module0, Name, Arity, !IO) :-
    ( Module0 = unqualified("") ->
        Module = mercury_public_builtin_module
    ;
        Module = Module0
    ),
    % We don't need to mangle the module name, but we do need to convert it
    % to a C identifier in the standard fashion.
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
            io.write_string(Macro, !IO)
        ;
            ModuleStr = "io",
            Name = "state"
        ->
            io.write_string("MR_IO_CTOR_ADDR", !IO)
        ;
            ModuleStr = "bool",
            Name = "bool"
        ->
            io.write_string("MR_BOOL_CTOR_ADDR", !IO)
        ;
            io.format("MR_CTOR0_ADDR(%s, %s)", [s(ModuleStr), s(Name)], !IO)
        )
    ; Arity = 1 ->
        (
            Name = "list",
            ModuleStr = "list"
        ->
            io.write_string("MR_LIST_CTOR_ADDR", !IO)
        ;
            Name = "private_builtin",
            ModuleStr = "type_info"
        ->
            io.write_string("MR_TYPE_INFO_CTOR_ADDR", !IO)
        ;
            io.format("MR_CTOR1_ADDR(%s, %s)", [s(ModuleStr), s(Name)], !IO)
        )
    ;
        io.format("MR_CTOR_ADDR(%s, %s, %d)",
            [s(ModuleStr), s(Name), i(Arity)], !IO)
    ).

output_rval_as_type(Info, Rval, DesiredType, !IO) :-
    llds.rval_type(Rval, ActualType),
    ( llds_types_match(DesiredType, ActualType) ->
        % No casting needed.
        output_rval(Info, Rval, !IO)
    ;
        % We need to convert to the right type first.
        % Convertions to/from float must be treated specially;
        % for the others, we can just use a cast.
        ( DesiredType = lt_float ->
            io.write_string("MR_word_to_float(", !IO),
            output_rval(Info, Rval, !IO),
            io.write_string(")", !IO)
        ; ActualType = lt_float ->
            ( DesiredType = lt_word ->
                output_float_rval_as_word(Info, Rval, !IO)
            ; DesiredType = lt_data_ptr ->
                output_float_rval_as_data_ptr(Info, Rval, !IO)
            ;
                unexpected($file, $pred, "type error")
            )
        ;
            (
                Rval = const(llconst_int(N)),
                direct_field_int_constant(DesiredType) = yes
            ->
                % The condition above increases the runtime of the compiler
                % very slightly. The elimination of the unnecessary casts
                % reduces the size of the generated C source file, which has
                % a considerably longer lifetime. In debugging grades,
                % the file size difference can be very substantial; it can be
                % in the range of megabytes.
                io.write_int(N, !IO)
            ;
                % Cast value to desired type.
                output_llds_type_cast(DesiredType, !IO),
                output_rval(Info, Rval, !IO)
            )
        )
    ).

    % Output a float rval, converted to type `MR_Word *'
    %
:- pred output_float_rval_as_data_ptr(llds_out_info::in, rval::in,
    io::di, io::uo) is det.

output_float_rval_as_data_ptr(Info, Rval, !IO) :-
    output_float_rval(Info, Rval, yes, !IO).

    % Output a float rval, converted to type `MR_Word'
    %
:- pred output_float_rval_as_word(llds_out_info::in, rval::in,
    io::di, io::uo) is det.

output_float_rval_as_word(Info, Rval, !IO) :-
    output_float_rval(Info, Rval, no, !IO).

    % Output a float rval, converted to type `MR_Word' or `MR_Word *'
    %
:- pred output_float_rval(llds_out_info::in, rval::in, bool::in,
    io::di, io::uo) is det.

output_float_rval(Info, Rval, IsPtr, !IO) :-
    % For float constant expressions, if we're using boxed floats
    % and --static-ground-floats is enabled, we just refer to the static const
    % which we declared earlier.
    UnboxFloat = Info ^ lout_unboxed_float,
    StaticGroundFloats = Info ^ lout_static_ground_floats,
    (
        UnboxFloat = no,
        StaticGroundFloats = yes,
        float_const_expr_name(Rval, FloatName)
    ->
        (
            IsPtr = yes,
            Cast = lt_data_ptr
        ;
            IsPtr = no,
            Cast = lt_word
        ),
        output_llds_type_cast(Cast, !IO),
        io.write_string("&mercury_float_const_", !IO),
        io.write_string(FloatName, !IO)
    ;
        (
            IsPtr = yes,
            output_llds_type_cast(lt_data_ptr, !IO)
        ;
            IsPtr = no
        ),
        io.write_string("MR_float_to_word(", !IO),
        output_rval(Info, Rval, !IO),
        io.write_string(")", !IO)
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
        LvalA = field(_MaybeTag, _Address, _Offset),
        % We cannot guarantee that the Address is dword aligned.
        fail
    ).

output_test_rval(Info, Test, !IO) :-
    (
        is_int_cmp(Test, Left, RightConst, OpStr, _)
    ->
        io.write_string(OpStr, !IO),
        io.write_string("(", !IO),
        output_rval(Info, Left, !IO),
        io.write_string(",", !IO),
        io.write_int(RightConst, !IO),
        io.write_string(")", !IO)
    ;
        Test = unop(logical_not, InnerTest),
        is_int_cmp(InnerTest, Left, RightConst, _, NegOpStr)
    ->
        io.write_string(NegOpStr, !IO),
        io.write_string("(", !IO),
        output_rval(Info, Left, !IO),
        io.write_string(",", !IO),
        io.write_int(RightConst, !IO),
        io.write_string(")", !IO)
    ;
        is_ptag_test(Test, Rval, Ptag, Negated)
    ->
        (
            Negated = no,
            io.write_string("MR_PTAG_TEST(", !IO)
        ;
            Negated = yes,
            io.write_string("MR_PTAG_TESTR(", !IO)
        ),
        output_rval(Info, Rval, !IO),
        io.write_string(",", !IO),
        io.write_int(Ptag, !IO),
        io.write_string(")", !IO)
    ;
        Test = unop(logical_not, InnerTest),
        is_ptag_test(InnerTest, Rval, Ptag, Negated)
    ->
        (
            Negated = no,
            io.write_string("MR_PTAG_TESTR(", !IO)
        ;
            Negated = yes,
            io.write_string("MR_PTAG_TEST(", !IO)
        ),
        output_rval(Info, Rval, !IO),
        io.write_string(",", !IO),
        io.write_int(Ptag, !IO),
        io.write_string(")", !IO)
    ;
        Test = binop(logical_and, Left, Right),
        is_ptag_test(Left, Rval, Ptag, no),
        is_remote_stag_test(Right, Rval, Ptag, Stag)
    ->
        io.write_string("MR_RTAGS_TEST(", !IO),
        output_rval(Info, Rval, !IO),
        io.write_string(",", !IO),
        io.write_int(Ptag, !IO),
        io.write_string(",", !IO),
        io.write_int(Stag, !IO),
        io.write_string(")", !IO)
    ;
        Test = unop(logical_not, InnerTest),
        InnerTest = binop(logical_and, Left, Right),
        is_ptag_test(Left, Rval, Ptag, no),
        is_remote_stag_test(Right, Rval, Ptag, Stag)
    ->
        io.write_string("MR_RTAGS_TESTR(", !IO),
        output_rval(Info, Rval, !IO),
        io.write_string(",", !IO),
        io.write_int(Ptag, !IO),
        io.write_string(",", !IO),
        io.write_int(Stag, !IO),
        io.write_string(")", !IO)
    ;
        is_local_stag_test(Test, Rval, Ptag, Stag, Negated)
    ->
        (
            Negated = no,
            io.write_string("MR_LTAGS_TEST(", !IO)
        ;
            Negated = yes,
            io.write_string("MR_LTAGS_TESTR(", !IO)
        ),
        output_rval(Info, Rval, !IO),
        io.write_string(",", !IO),
        io.write_int(Ptag, !IO),
        io.write_string(",", !IO),
        io.write_int(Stag, !IO),
        io.write_string(")", !IO)
    ;
        Test = unop(logical_not, InnerTest),
        is_local_stag_test(InnerTest, Rval, Ptag, Stag, Negated)
    ->
        (
            Negated = no,
            io.write_string("MR_LTAGS_TESTR(", !IO)
        ;
            Negated = yes,
            io.write_string("MR_LTAGS_TEST(", !IO)
        ),
        output_rval(Info, Rval, !IO),
        io.write_string(",", !IO),
        io.write_int(Ptag, !IO),
        io.write_string(",", !IO),
        io.write_int(Stag, !IO),
        io.write_string(")", !IO)
    ;
        output_rval_as_type(Info, Test, lt_bool, !IO)
    ).

:- pred is_int_cmp(rval::in, rval::out, int::out, string::out, string::out)
    is semidet.

is_int_cmp(Test, Left, RightConst, OpStr, NegOpStr) :-
    Test = binop(Op, Left, Right),
    Right = const(llconst_int(RightConst)),
    (
        Op = eq,
        OpStr = "MR_INT_EQ",
        NegOpStr = "MR_INT_NE"
    ;
        Op = ne,
        OpStr = "MR_INT_NE",
        NegOpStr = "MR_INT_EQ"
    ;
        Op = int_lt,
        OpStr = "MR_INT_LT",
        NegOpStr = "MR_INT_GE"
    ;
        Op = int_gt,
        OpStr = "MR_INT_GT",
        NegOpStr = "MR_INT_LT"
    ;
        Op = int_le,
        OpStr = "MR_INT_LE",
        NegOpStr = "MR_INT_GT"
    ;
        Op = int_ge,
        OpStr = "MR_INT_GE",
        NegOpStr = "MR_INT_LT"
    ).

:- pred is_ptag_test(rval::in, rval::out, int::out, bool::out) is semidet.

is_ptag_test(Test, Rval, Ptag, Negated) :-
    Test = binop(Op, Left, Right),
    Left = unop(tag, Rval),
    Right = unop(mktag, const(llconst_int(Ptag))),
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
    Zero = const(llconst_int(0)),
    Right = const(llconst_int(Stag)).

:- pred is_local_stag_test(rval::in, rval::out, int::out, int::out, bool::out)
    is semidet.

is_local_stag_test(Test, Rval, Ptag, Stag, Negated) :-
    Test = binop(Op, Rval, Right),
    Right = mkword(Ptag, unop(mkbody, const(llconst_int(Stag)))),
    (
        Op = eq,
        Negated = no
    ;
        Op = ne,
        Negated = yes
    ).

output_tag(Tag, !IO) :-
    io.write_string("MR_mktag(", !IO),
    io.write_int(Tag, !IO),
    io.write_string(")", !IO).

direct_field_int_constant(lt_bool) = no.
direct_field_int_constant(lt_int_least8) = yes.
direct_field_int_constant(lt_uint_least8) = yes.
direct_field_int_constant(lt_int_least16) = yes.
direct_field_int_constant(lt_uint_least16) = yes.
direct_field_int_constant(lt_int_least32) = yes.
direct_field_int_constant(lt_uint_least32) = yes.
direct_field_int_constant(lt_integer) = yes.
direct_field_int_constant(lt_unsigned) = yes.
direct_field_int_constant(lt_float) = no.
direct_field_int_constant(lt_string) = no.
direct_field_int_constant(lt_data_ptr) = no.
direct_field_int_constant(lt_code_ptr) = no.
direct_field_int_constant(lt_word) = no.

%----------------------------------------------------------------------------%
%
% Compute the names of the global variables that hold floating point constants.
%

float_const_expr_name(Expr, Name) :-
    ( Expr = const(llconst_float(Float)) ->
        float_literal_name(Float, Name)
    ; Expr = binop(Op, Arg1, Arg2) ->
        float_const_binop_expr_name(Op, Arg1, Arg2, Name)
    ;
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

    % Given an rval which is a floating point literal, return
    % a name for that rval that is suitable for use in a C identifier.
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

float_op_name(float_plus, "plus").
float_op_name(float_minus, "minus").
float_op_name(float_times, "times").
float_op_name(float_divide, "divide").

%----------------------------------------------------------------------------%
%
% Declare the names of global variables.
%

output_record_data_id_decls(Info, DataId, !DeclSet, !IO) :-
    output_record_data_id_decls_format(Info, DataId, "", "",
        0, _, !DeclSet, !IO).

output_record_data_id_decls_format(Info, DataId, FirstIndent, LaterIndent,
        !N, !DeclSet, !IO) :-
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
        ( decl_set_is_member(DeclId, !.DeclSet) ->
            true
        ;
            decl_set_insert(DeclId, !DeclSet),
            output_indent(FirstIndent, LaterIndent, !.N, !IO),
            !:N = !.N + 1,
            output_rtti_id_storage_type_name_no_decl(Info, RttiId, no, !IO),
            io.write_string(";\n", !IO)
        )
    ;
        DataId = layout_id(LayoutName),
        DeclId = decl_layout_id(LayoutName),
        ( decl_set_is_member(DeclId, !.DeclSet) ->
            true
        ;
            decl_set_insert(DeclId, !DeclSet),
            output_indent(FirstIndent, LaterIndent, !.N, !IO),
            !:N = !.N + 1,
            output_layout_name_storage_type_name(LayoutName,
                not_being_defined, !IO),
            io.write_string(";\n", !IO)
        )
    ).

%----------------------------------------------------------------------------%
%
% Output references to global variables.
%

output_data_id_addr(Info, DataId, !IO) :-
    io.write_string("&", !IO),
    output_data_id(Info, DataId, !IO).

    % Output a data id.
    %
output_data_id(Info, DataId, !IO) :-
    (
        DataId = rtti_data_id(RttiId),
        output_rtti_id(RttiId, !IO)
    ;
        DataId = proc_tabling_data_id(ProcLabel, TablingId),
        io.write_string(tabling_struct_data_addr_string(ProcLabel, TablingId),
            !IO)
    ;
        DataId = scalar_common_data_id(TypeNum, CellNum),
        output_common_scalar_cell_array_name(TypeNum, !IO),
        io.write_string("[", !IO),
        io.write_int(CellNum, !IO),
        io.write_string("]", !IO)
    ;
        DataId = vector_common_data_id(TypeNum, CellNum),
        output_common_vector_cell_array_name(TypeNum, CellNum, !IO)
    ;
        DataId = layout_id(LayoutName),
        output_layout_name(LayoutName, !IO)
    ;
        DataId = layout_slot_id(Kind, PredProcId),
        Kind = table_io_entry_id,
        TableIoEntryMap = Info ^ lout_table_io_entry_map,
        map.lookup(TableIoEntryMap, PredProcId, LayoutSlotName),
        MangledModuleName = Info ^ lout_mangled_module_name,
        output_layout_slot_id(use_layout_macro, MangledModuleName,
            LayoutSlotName, !IO)
    ).

output_common_scalar_cell_array_name(type_num(TypeNum), !IO) :-
    io.write_string(mercury_scalar_common_array_prefix, !IO),
    io.write_int(TypeNum, !IO).

output_common_vector_cell_array_name(type_num(TypeNum), CellNum, !IO) :-
    io.write_string(mercury_vector_common_array_prefix, !IO),
    io.write_int(TypeNum, !IO),
    io.write_string("_", !IO),
    io.write_int(CellNum, !IO).

%---------------------------------------------------------------------------%
:- end_module ll_backend.llds_out.llds_out_data.
%---------------------------------------------------------------------------%
