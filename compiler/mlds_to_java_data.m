%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2000-2012 The University of Melbourne.
% Copyright (C) 2013-2018 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% Output MLDS lvals, rvals and initializers in Java.
%
%---------------------------------------------------------------------------%

:- module ml_backend.mlds_to_java_data.
:- interface.

:- import_module ml_backend.mlds.
:- import_module ml_backend.mlds_to_java_util.
:- import_module ml_backend.mlds_to_target_util.

:- import_module io.
:- import_module list.
:- import_module maybe.

%---------------------------------------------------------------------------%

:- pred output_lval_for_java(java_out_info::in, mlds_lval::in,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- pred output_call_rval_for_java(java_out_info::in, mlds_rval::in,
    io::di, io::uo) is det.

:- pred output_bracketed_rval_for_java(java_out_info::in, mlds_rval::in,
    io::di, io::uo) is det.

:- pred output_rval_for_java(java_out_info::in, mlds_rval::in,
    io::di, io::uo) is det.

:- pred output_boxed_rval_for_java(java_out_info::in, mlds_type::in,
    mlds_rval::in, io::di, io::uo) is det.

    % Output an Rval and if the Rval is an enumeration object append the string
    % ".MR_value", so we can access its value field.
    %
    % XXX Note that this is necessary in some places, but not in others.
    % For example, it is important to do so for switch statements, as the
    % argument of a switch _must_ be an integer in Java. However, adding
    % the .MR_value to assignments breaks some casting... At some point, we
    % need to go through all the places where output_rval and
    % output_rval_maybe_with_enum are called and make sure the correct one
    % is being used.
    %
:- pred output_rval_maybe_with_enum_for_java(java_out_info::in, mlds_rval::in,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- pred output_initializer_for_java(java_out_info::in, output_aux::in,
    mlds_type::in, mlds_initializer::in, io::di, io::uo) is det.

:- pred output_initializer_alloc_only_for_java(java_out_info::in,
    mlds_initializer::in, maybe(mlds_type)::in, io::di, io::uo) is det.

:- pred output_initializer_body_for_java(java_out_info::in,
    mlds_initializer::in, maybe(mlds_type)::in, io::di, io::uo) is det.

:- pred output_initializer_body_list_for_java(java_out_info::in,
    list(mlds_initializer)::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

    % We need to provide initializers for local variables to avoid problems
    % with Java's rules for definite assignment. This mirrors the default
    % Java initializers for class and instance variables.
    %
:- func get_default_initializer_for_java(mlds_type) = string.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.builtin_ops.
:- import_module backend_libs.c_util.
:- import_module backend_libs.rtti.
:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module libs.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module ml_backend.ml_util.
:- import_module ml_backend.mlds_to_java_name.
:- import_module ml_backend.mlds_to_java_type.
:- import_module parse_tree.
:- import_module parse_tree.java_names.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_type.

:- import_module bool.
:- import_module char.
:- import_module int.
:- import_module int8.
:- import_module int16.
:- import_module int32.
:- import_module int64.
:- import_module map.
:- import_module require.
:- import_module string.
:- import_module term.
:- import_module uint.

%---------------------------------------------------------------------------%

output_lval_for_java(Info, Lval, !IO) :-
    (
        Lval = ml_field(_MaybeTag, PtrRval, _PtrType, FieldId, FieldType),
        (
            FieldId = ml_field_offset(OffsetRval),
            ( if
                ( FieldType = mlds_generic_type
                ; FieldType = mercury_nb_type(type_variable(_, _), _)
                )
            then
                true
            else
                % The field type for field(_, _, offset(_), _, _) lvals
                % must be something that maps to MR_Box.
                unexpected($pred, "unexpected field type")
            ),
            % XXX We shouldn't need this cast here, but there are cases where
            % it is needed and the MLDS doesn't seem to generate it.
            io.write_string("((java.lang.Object[]) ", !IO),
            output_rval_for_java(Info, PtrRval, !IO),
            io.write_string(")[", !IO),
            output_rval_for_java(Info, OffsetRval, !IO),
            io.write_string("]", !IO)
        ;
            FieldId = ml_field_named(QualFieldVarName, CtorType),
            QualFieldVarName = qual_field_var_name(_, _, FieldVarName),
            ( if FieldVarName = fvn_data_tag then
                % If the field we are trying to access is just a `data_tag'
                % then it is a member of the base class.
                output_bracketed_rval_for_java(Info, PtrRval, !IO),
                io.write_string(".", !IO)
            else if PtrRval = ml_self(_) then
                % Suppress type cast on `this' keyword. This makes a difference
                % when assigning to `final' member variables in constructor
                % functions.
                output_rval_for_java(Info, PtrRval, !IO),
                io.write_string(".", !IO)
            else
                % Otherwise the field we are trying to access may be
                % in a derived class. Objects are manipulated as instances
                % of their base class, so we need to downcast to the derived
                % class to access some fields.
                io.write_string("((", !IO),
                output_type_for_java(Info, CtorType, !IO),
                io.write_string(") ", !IO),
                output_bracketed_rval_for_java(Info, PtrRval, !IO),
                io.write_string(").", !IO)
            ),
            output_field_var_name_for_java(FieldVarName, !IO)
        )
    ;
        Lval = ml_mem_ref(Rval, _Type),
        output_bracketed_rval_for_java(Info, Rval, !IO)
    ;
        Lval = ml_target_global_var_ref(GlobalVarRef),
        GlobalVarRef = env_var_ref(EnvVarName),
        io.write_string("mercury_envvar_", !IO),
        io.write_string(EnvVarName, !IO)
    ;
        Lval = ml_global_var(QualGlobalVarName, _),
        output_maybe_qualified_global_var_name_for_java(Info,
            QualGlobalVarName, !IO)
    ;
        Lval = ml_local_var(QualLocalVarName, _),
        output_local_var_name_for_java(QualLocalVarName, !IO)
    ).

%---------------------------------------------------------------------------%

output_call_rval_for_java(Info, Rval, !IO) :-
    ( if
        Rval = ml_const(Const),
        Const = mlconst_code_addr(CodeAddr)
    then
        IsCall = yes,
        mlds_output_code_addr_for_java(Info, CodeAddr, IsCall, !IO)
    else
        output_bracketed_rval_for_java(Info, Rval, !IO)
    ).

output_bracketed_rval_for_java(Info, Rval, !IO) :-
    ( if
        % If it is just a variable name, then we don't need parentheses.
        ( Rval = ml_lval(ml_local_var(_,_))
        ; Rval = ml_lval(ml_global_var(_,_))
        ; Rval = ml_const(mlconst_code_addr(_))
        )
    then
        output_rval_for_java(Info, Rval, !IO)
    else
        io.write_char('(', !IO),
        output_rval_for_java(Info, Rval, !IO),
        io.write_char(')', !IO)
    ).

output_rval_for_java(Info, Rval, !IO) :-
    (
        Rval = ml_lval(Lval),
        output_lval_for_java(Info, Lval, !IO)
    ;
        Rval = ml_mkword(_, _),
        unexpected($pred, "tags not supported in Java")
    ;
        Rval = ml_const(Const),
        output_rval_const_for_java(Info, Const, !IO)
    ;
        Rval = ml_cast(Type, SubRval),
        output_cast_rval_for_java(Info, Type, SubRval, !IO)
    ;
        Rval = ml_box(Type, SubRval),
        output_boxed_rval_for_java(Info, Type, SubRval, !IO)
    ;
        Rval = ml_unbox(Type, SubRval),
        output_unboxed_rval_for_java(Info, Type, SubRval, !IO)
    ;
        Rval = ml_unop(Unop, SubRval),
        output_unop_for_java(Info, Unop, SubRval, !IO)
    ;
        Rval = ml_binop(BinOp, RvalA, RvalB),
        output_binop_for_java(Info, BinOp, RvalA, RvalB, !IO)
    ;
        Rval = ml_mem_addr(_Lval),
        unexpected($pred, "mem_addr(_) not supported")
    ;
        Rval = ml_scalar_common(_),
        % This reference is not the same as a mlds_data_addr const.
        unexpected($pred, "ml_scalar_common")
    ;
        Rval = ml_scalar_common_addr(ScalarCommon),
        ScalarCommon = ml_scalar_common(ModuleName, _Type,
            ml_scalar_common_type_num(TypeNum), RowNum),
        ModuleSymName = mlds_module_name_to_sym_name(ModuleName),
        mangle_sym_name_for_java(ModuleSymName, module_qual, "__",
            MangledModuleName),
        io.format("%s.MR_scalar_common_%d[%d]",
            [s(MangledModuleName),i(TypeNum), i(RowNum)], !IO)
    ;
        Rval = ml_vector_common_row_addr(VectorCommon, RowRval),
        VectorCommon = ml_vector_common(_ModuleName, _Type,
            ml_vector_common_type_num(TypeNum), StartRowNum, _NumRows),
        % XXX Why do we print a "MangledModuleName." prefix for scalar common
        % addresses but not for vector common addresses?
        io.format("MR_vector_common_%d[%d + ",
            [i(TypeNum), i(StartRowNum)], !IO),
        output_rval_for_java(Info, RowRval, !IO),
        io.write_string("]", !IO)
    ;
        Rval = ml_self(_),
        io.write_string("this", !IO)
    ).

:- pred output_cast_rval_for_java(java_out_info::in, mlds_type::in,
    mlds_rval::in, io::di, io::uo) is det.

output_cast_rval_for_java(Info, Type, Expr, !IO) :-
    % rtti_to_mlds.m generates casts from int to
    % jmercury.runtime.PseudoTypeInfo, but for Java
    % we need to treat these as constructions, not casts.
    % Similarly for conversions from TypeCtorInfo to TypeInfo.
    ( if
        Type = mlds_pseudo_type_info_type,
        Expr = ml_const(mlconst_int(N))
    then
        maybe_output_comment_for_java(Info, "cast", !IO),
        ( if have_preallocated_pseudo_type_var_for_java(N) then
            io.write_string("jmercury.runtime.PseudoTypeInfo.K", !IO),
            io.write_int(N, !IO)
        else
            io.write_string("new jmercury.runtime.PseudoTypeInfo(", !IO),
            output_rval_for_java(Info, Expr, !IO),
            io.write_string(")", !IO)
        )
    else if
        ( Type = mercury_nb_type(_, ctor_cat_system(cat_system_type_info))
        ; Type = mlds_type_info_type
        )
    then
        % XXX We really should be able to tell if we are casting a
        % TypeCtorInfo or a TypeInfo. Julien says that's probably going to
        % be rather difficult as the compiler doesn't keep track of where
        % type_ctor_infos are acting as type_infos properly.
        maybe_output_comment_for_java(Info, "cast", !IO),
        io.write_string("jmercury.runtime.TypeInfo_Struct.maybe_new(", !IO),
        output_rval_for_java(Info, Expr, !IO),
        io.write_string(")", !IO)
    else if
        java_builtin_type(Type, "int", _, _)
    then
        io.write_string("(int) ", !IO),
        output_rval_maybe_with_enum_for_java(Info, Expr, !IO)
    else
        io.write_string("(", !IO),
        output_type_for_java(Info, Type, !IO),
        io.write_string(") ", !IO),
        output_rval_for_java(Info, Expr, !IO)
    ).

:- pred have_preallocated_pseudo_type_var_for_java(int::in) is semidet.

have_preallocated_pseudo_type_var_for_java(N) :-
    % Corresponds to static members in class PseudoTypeInfo.
    N >= 1,
    N =< 5.

output_boxed_rval_for_java(Info, Type, Expr, !IO) :-
    ( if java_builtin_type(Type, _JavaName, JavaBoxedName, _) then
        % valueOf may return cached instances instead of creating new objects.
        io.write_string(JavaBoxedName, !IO),
        io.write_string(".valueOf(", !IO),
        output_rval_for_java(Info, Expr, !IO),
        io.write_string(")", !IO)
    else
        io.write_string("((java.lang.Object) (", !IO),
        output_rval_for_java(Info, Expr, !IO),
        io.write_string("))", !IO)
    ).

:- pred output_unboxed_rval_for_java(java_out_info::in, mlds_type::in,
    mlds_rval::in, io::di, io::uo) is det.

output_unboxed_rval_for_java(Info, Type, Expr, !IO) :-
    ( if java_builtin_type(Type, _, JavaBoxedName, UnboxMethod) then
        io.write_string("((", !IO),
        io.write_string(JavaBoxedName, !IO),
        io.write_string(") ", !IO),
        output_bracketed_rval_for_java(Info, Expr, !IO),
        io.write_string(").", !IO),
        io.write_string(UnboxMethod, !IO),
        io.write_string("()", !IO)
    else
        io.write_string("((", !IO),
        output_type_for_java(Info, Type, !IO),
        io.write_string(") ", !IO),
        output_rval_for_java(Info, Expr, !IO),
        io.write_string(")", !IO)
    ).

:- pred output_unop_for_java(java_out_info::in, builtin_ops.unary_op::in,
    mlds_rval::in, io::di, io::uo) is det.

output_unop_for_java(Info, UnaryOp, Expr, !IO) :-
    % For the Java back-end, there are no tags, so all the tagging operators
    % are no-ops, except for `tag', which always returns zero (a tag of zero
    % means there is no tag).
    (
        UnaryOp = tag,
        io.write_string("/* tag */  0", !IO)
    ;
        ( UnaryOp = strip_tag, UnaryOpStr = "/* strip_tag */ "
        ; UnaryOp = mkbody,    UnaryOpStr = "/* mkbody */ "
        ; UnaryOp = unmkbody,  UnaryOpStr = "/* unmkbody */ "
        ; UnaryOp = bitwise_complement(int_type_int), UnaryOpStr = "~"
        ; UnaryOp = bitwise_complement(int_type_uint), UnaryOpStr = "~"
        ; UnaryOp = bitwise_complement(int_type_int32), UnaryOpStr = "~"
        ; UnaryOp = bitwise_complement(int_type_uint32), UnaryOpStr = "~"
        ; UnaryOp = bitwise_complement(int_type_int64), UnaryOpStr = "~"
        ; UnaryOp = bitwise_complement(int_type_uint64), UnaryOpStr = "~"
        ; UnaryOp = logical_not, UnaryOpStr = "!"
        ; UnaryOp = hash_string,  UnaryOpStr = "mercury.String.hash_1_f_0"
        ; UnaryOp = hash_string2, UnaryOpStr = "mercury.String.hash2_1_f_0"
        ; UnaryOp = hash_string3, UnaryOpStr = "mercury.String.hash3_1_f_0"
        ; UnaryOp = hash_string4, UnaryOpStr = "mercury.String.hash4_1_f_0"
        ; UnaryOp = hash_string5, UnaryOpStr = "mercury.String.hash5_1_f_0"
        ; UnaryOp = hash_string6, UnaryOpStr = "mercury.String.hash6_1_f_0"
        ),
        io.write_string(UnaryOpStr, !IO),
        io.write_string("(", !IO),
        output_rval_for_java(Info, Expr, !IO),
        io.write_string(")", !IO)
    ;
        ( UnaryOp = bitwise_complement(int_type_int8), UnaryOpStr = "~"
        ; UnaryOp = bitwise_complement(int_type_uint8), UnaryOpStr = "~"
        ),
        io.write_string("(byte)(", !IO),
        io.write_string(UnaryOpStr, !IO),
        io.write_string("(", !IO),
        output_rval_for_java(Info, Expr, !IO),
        io.write_string("))", !IO)
    ;
        ( UnaryOp = bitwise_complement(int_type_int16), UnaryOpStr = "~"
        ; UnaryOp = bitwise_complement(int_type_uint16), UnaryOpStr = "~"
        ),
        io.write_string("(short)(", !IO),
        io.write_string(UnaryOpStr, !IO),
        io.write_string("(", !IO),
        output_rval_for_java(Info, Expr, !IO),
        io.write_string("))", !IO)
    ;
        ( UnaryOp = dword_float_get_word0
        ; UnaryOp = dword_float_get_word1
        ; UnaryOp = dword_int64_get_word0
        ; UnaryOp = dword_int64_get_word1
        ; UnaryOp = dword_uint64_get_word0
        ; UnaryOp = dword_uint64_get_word1
        ),
        unexpected($pred, "invalid unary operator")
    ).

:- pred output_binop_for_java(java_out_info::in, binary_op::in, mlds_rval::in,
    mlds_rval::in, io::di, io::uo) is det.

output_binop_for_java(Info, Op, X, Y, !IO) :-
    (
        Op = array_index(_Type),
        output_bracketed_rval_for_java(Info, X, !IO),
        io.write_string("[", !IO),
        output_rval_for_java(Info, Y, !IO),
        io.write_string("]", !IO)
    ;
        Op = str_eq,
        output_rval_for_java(Info, X, !IO),
        io.write_string(".equals(", !IO),
        output_rval_for_java(Info, Y, !IO),
        io.write_string(")", !IO)
    ;
        ( Op = str_ne, OpStr = "!="
        ; Op = str_lt, OpStr = "<"
        ; Op = str_gt, OpStr = ">"
        ; Op = str_le, OpStr = "<="
        ; Op = str_ge, OpStr = ">="
        ),
        io.write_string("(", !IO),
        output_rval_for_java(Info, X, !IO),
        io.write_string(".compareTo(", !IO),
        output_rval_for_java(Info, Y, !IO),
        io.write_string(") ", !IO),
        io.write_string(OpStr, !IO),
        io.write_string(" 0)", !IO)
    ;
        Op = str_cmp,
        io.write_string("(", !IO),
        output_rval_for_java(Info, X, !IO),
        io.write_string(".compareTo(", !IO),
        output_rval_for_java(Info, Y, !IO),
        io.write_string(")) ", !IO)
    ;
        Op = pointer_equal_conservative,
        io.write_string("(", !IO),
        output_rval_for_java(Info, X, !IO),
        io.write_string(" == ", !IO),
        output_rval_for_java(Info, Y, !IO),
        io.write_string(") ", !IO)
    ;
        % XXX Should we abort for some of these?
        ( Op = int_add(int_type_int)
        ; Op = int_sub(int_type_int)
        ; Op = int_mul(int_type_int)
        ; Op = int_div(int_type_int)
        ; Op = int_mod(int_type_int)
        ; Op = unchecked_left_shift(int_type_int)
        ; Op = unchecked_right_shift(int_type_int)
        ; Op = bitwise_and(int_type_int)
        ; Op = bitwise_or(int_type_int)
        ; Op = bitwise_xor(int_type_int)
        ; Op = int_lt(int_type_int32)
        ; Op = int_gt(int_type_int32)
        ; Op = int_le(int_type_int32)
        ; Op = int_ge(int_type_int32)
        ; Op = int_add(int_type_int32)
        ; Op = int_sub(int_type_int32)
        ; Op = int_mul(int_type_int32)
        ; Op = int_div(int_type_int32)
        ; Op = int_mod(int_type_int32)
        ; Op = bitwise_and(int_type_int32)
        ; Op = bitwise_or(int_type_int32)
        ; Op = bitwise_xor(int_type_int32)
        ; Op = unchecked_left_shift(int_type_int32)
        ; Op = unchecked_right_shift(int_type_int32)
        ; Op = int_add(int_type_uint)
        ; Op = int_sub(int_type_uint)
        ; Op = int_mul(int_type_uint)
        ; Op = bitwise_and(int_type_uint)
        ; Op = bitwise_or(int_type_uint)
        ; Op = bitwise_xor(int_type_uint)
        ; Op = unchecked_left_shift(int_type_uint)
        ; Op = unchecked_right_shift(int_type_uint)
        ; Op = int_add(int_type_uint32)
        ; Op = int_sub(int_type_uint32)
        ; Op = int_mul(int_type_uint32)
        ; Op = bitwise_and(int_type_uint32)
        ; Op = bitwise_or(int_type_uint32)
        ; Op = bitwise_xor(int_type_uint32)
        ; Op = unchecked_left_shift(int_type_uint32)
        ; Op = unchecked_right_shift(int_type_uint32)
        ; Op = int_lt(int_type_int64)
        ; Op = int_gt(int_type_int64)
        ; Op = int_le(int_type_int64)
        ; Op = int_ge(int_type_int64)
        ; Op = int_add(int_type_int64)
        ; Op = int_sub(int_type_int64)
        ; Op = int_mul(int_type_int64)
        ; Op = int_div(int_type_int64)
        ; Op = int_mod(int_type_int64)
        ; Op = bitwise_and(int_type_int64)
        ; Op = bitwise_or(int_type_int64)
        ; Op = bitwise_xor(int_type_int64)
        ; Op = unchecked_left_shift(int_type_int64)
        ; Op = unchecked_right_shift(int_type_int64)
        ; Op = int_add(int_type_uint64)
        ; Op = int_sub(int_type_uint64)
        ; Op = int_mul(int_type_uint64)
        ; Op = bitwise_and(int_type_uint64)
        ; Op = bitwise_or(int_type_uint64)
        ; Op = bitwise_xor(int_type_uint64)
        ; Op = unchecked_left_shift(int_type_uint64)
        ; Op = unchecked_right_shift(int_type_uint64)
        ; Op = logical_and
        ; Op = logical_or
        ; Op = eq(_)
        ; Op = ne(_)
        ; Op = body
        ; Op = string_unsafe_index_code_unit
        ; Op = offset_str_eq(_)
        ; Op = int_lt(int_type_int)
        ; Op = int_gt(int_type_int)
        ; Op = int_le(int_type_int)
        ; Op = int_ge(int_type_int)
        ; Op = unsigned_le
        ; Op = float_plus
        ; Op = float_minus
        ; Op = float_times
        ; Op = float_divide
        ; Op = float_eq
        ; Op = float_ne
        ; Op = float_lt
        ; Op = float_gt
        ; Op = float_le
        ; Op = float_ge
        ; Op = float_from_dword
        ; Op = int64_from_dword
        ; Op = uint64_from_dword
        ; Op = compound_eq
        ; Op = compound_lt
        ; Op = int_lt(int_type_int8)
        ; Op = int_gt(int_type_int8)
        ; Op = int_le(int_type_int8)
        ; Op = int_ge(int_type_int8)
        ; Op = int_lt(int_type_int16)
        ; Op = int_gt(int_type_int16)
        ; Op = int_le(int_type_int16)
        ; Op = int_ge(int_type_int16)
        ),
        ( if rval_is_enum_object(X) then
            io.write_string("(", !IO),
            output_rval_for_java(Info, X, !IO),
            io.write_string(".MR_value ", !IO),
            output_binary_op_for_java(Op, !IO),
            io.write_string(" ", !IO),
            output_rval_for_java(Info, Y, !IO),
            io.write_string(".MR_value)", !IO)
        else
            io.write_string("(", !IO),
            output_rval_for_java(Info, X, !IO),
            io.write_string(" ", !IO),
            output_binary_op_for_java(Op, !IO),
            io.write_string(" ", !IO),
            output_rval_for_java(Info, Y, !IO),
            io.write_string(")", !IO)
        )
    ;
        ( Op = int_lt(int_type_uint)
        ; Op = int_gt(int_type_uint)
        ; Op = int_le(int_type_uint)
        ; Op = int_ge(int_type_uint)
        ; Op = int_lt(int_type_uint32)
        ; Op = int_gt(int_type_uint32)
        ; Op = int_le(int_type_uint32)
        ; Op = int_ge(int_type_uint32)
        ),
        io.write_string("(((", !IO),
        output_rval_for_java(Info, X, !IO),
        io.write_string(") & 0xffffffffL) ", !IO),
        output_binary_op_for_java(Op, !IO),
        io.write_string(" ((", !IO),
        output_rval_for_java(Info, Y, !IO),
        io.write_string(") & 0xffffffffL))", !IO)
    ;
        ( Op = int_lt(int_type_uint64), RelOpStr = "<"
        ; Op = int_gt(int_type_uint64), RelOpStr = ">"
        ; Op = int_le(int_type_uint64), RelOpStr = "<="
        ; Op = int_ge(int_type_uint64), RelOpStr = ">="
        ),
        io.write_string("(java.lang.Long.compareUnsigned(", !IO),
        output_rval_for_java(Info, X, !IO),
        io.write_string(", ", !IO),
        output_rval_for_java(Info, Y, !IO),
        io.write_string(") ", !IO),
        io.write_string(RelOpStr, !IO),
        io.write_string(" 0)", !IO)
    ;
        ( Op = int_div(int_type_uint)
        ; Op = int_mod(int_type_uint)
        ; Op = int_div(int_type_uint32)
        ; Op = int_mod(int_type_uint32)
        ),
        io.write_string("((int)(((", !IO),
        output_rval_for_java(Info, X, !IO),
        io.write_string(") & 0xffffffffL) ", !IO),
        output_binary_op_for_java(Op, !IO),
        io.write_string(" ((", !IO),
        output_rval_for_java(Info, Y, !IO),
        io.write_string(") & 0xffffffffL)))", !IO)
    ;
        Op = int_div(int_type_uint64),
        io.write_string("java.lang.Long.divideUnsigned(", !IO),
        output_rval_for_java(Info, X, !IO),
        io.write_string(", ", !IO),
        output_rval_for_java(Info, Y, !IO),
        io.write_string(")", !IO)
    ;
        Op = int_mod(int_type_uint64),
        io.write_string("java.lang.Long.remainderUnsigned(", !IO),
        output_rval_for_java(Info, X, !IO),
        io.write_string(", ", !IO),
        output_rval_for_java(Info, Y, !IO),
        io.write_string(")", !IO)
    ;
        ( Op = int_add(int_type_int8)
        ; Op = int_sub(int_type_int8)
        ; Op = int_mul(int_type_int8)
        ; Op = int_div(int_type_int8)
        ; Op = int_mod(int_type_int8)
        ; Op = bitwise_and(int_type_int8)
        ; Op = bitwise_or(int_type_int8)
        ; Op = bitwise_xor(int_type_int8)
        ; Op = unchecked_left_shift(int_type_int8)
        ; Op = unchecked_right_shift(int_type_int8)
        ; Op = int_add(int_type_uint8)
        ; Op = int_sub(int_type_uint8)
        ; Op = int_mul(int_type_uint8)
        ; Op = bitwise_and(int_type_uint8)
        ; Op = bitwise_or(int_type_uint8)
        ; Op = bitwise_xor(int_type_uint8)
        ; Op = unchecked_left_shift(int_type_uint8)
        ),
        io.write_string("(byte)(", !IO),
        output_rval_for_java(Info, X, !IO),
        io.write_string(" ", !IO),
        output_binary_op_for_java(Op, !IO),
        io.write_string(" ", !IO),
        output_rval_for_java(Info, Y, !IO),
        io.write_string(")", !IO)
    ;
        Op = unchecked_right_shift(int_type_uint8),
        io.write_string("(byte)(((", !IO),
        output_rval_for_java(Info, X, !IO),
        io.write_string(") & 0xff) ", !IO),
        output_binary_op_for_java(Op, !IO),
        io.write_string(" ", !IO),
        output_rval_for_java(Info, Y, !IO),
        io.write_string(")", !IO)
    ;
        ( Op = int_lt(int_type_uint8)
        ; Op = int_gt(int_type_uint8)
        ; Op = int_le(int_type_uint8)
        ; Op = int_ge(int_type_uint8)
        ),
        io.write_string("(((", !IO),
        output_rval_for_java(Info, X, !IO),
        io.write_string(") & 0xff) ", !IO),
        output_binary_op_for_java(Op, !IO),
        io.write_string(" ((", !IO),
        output_rval_for_java(Info, Y, !IO),
        io.write_string(") & 0xff))", !IO)
    ;
        ( Op = int_div(int_type_uint8)
        ; Op = int_mod(int_type_uint8)
        ),
        io.write_string("((byte)(((", !IO),
        output_rval_for_java(Info, X, !IO),
        io.write_string(") & 0xff) ", !IO),
        output_binary_op_for_java(Op, !IO),
        io.write_string(" ((", !IO),
        output_rval_for_java(Info, Y, !IO),
        io.write_string(") & 0xff)))", !IO)
    ;
        ( Op = int_add(int_type_int16)
        ; Op = int_sub(int_type_int16)
        ; Op = int_mul(int_type_int16)
        ; Op = int_div(int_type_int16)
        ; Op = int_mod(int_type_int16)
        ; Op = bitwise_and(int_type_int16)
        ; Op = bitwise_or(int_type_int16)
        ; Op = bitwise_xor(int_type_int16)
        ; Op = unchecked_left_shift(int_type_int16)
        ; Op = unchecked_right_shift(int_type_int16)
        ; Op = int_add(int_type_uint16)
        ; Op = int_sub(int_type_uint16)
        ; Op = int_mul(int_type_uint16)
        ; Op = bitwise_and(int_type_uint16)
        ; Op = bitwise_or(int_type_uint16)
        ; Op = bitwise_xor(int_type_uint16)
        ; Op = unchecked_left_shift(int_type_uint16)
        ),
        io.write_string("(short)(", !IO),
        output_rval_for_java(Info, X, !IO),
        io.write_string(" ", !IO),
        output_binary_op_for_java(Op, !IO),
        io.write_string(" ", !IO),
        output_rval_for_java(Info, Y, !IO),
        io.write_string(")", !IO)
    ;
        Op = unchecked_right_shift(int_type_uint16),
        io.write_string("(short)(((", !IO),
        output_rval_for_java(Info, X, !IO),
        io.write_string(") & 0xffff) ", !IO),
        output_binary_op_for_java(Op, !IO),
        io.write_string(" ", !IO),
        output_rval_for_java(Info, Y, !IO),
        io.write_string(")", !IO)
    ;
        ( Op = int_lt(int_type_uint16)
        ; Op = int_gt(int_type_uint16)
        ; Op = int_le(int_type_uint16)
        ; Op = int_ge(int_type_uint16)
        ),
        io.write_string("(((", !IO),
        output_rval_for_java(Info, X, !IO),
        io.write_string(") & 0xffff) ", !IO),
        output_binary_op_for_java(Op, !IO),
        io.write_string(" ((", !IO),
        output_rval_for_java(Info, Y, !IO),
        io.write_string(") & 0xffff))", !IO)
    ;
        ( Op = int_div(int_type_uint16)
        ; Op = int_mod(int_type_uint16)
        ),
        io.write_string("((short)(((", !IO),
        output_rval_for_java(Info, X, !IO),
        io.write_string(") & 0xffff) ", !IO),
        output_binary_op_for_java(Op, !IO),
        io.write_string(" ((", !IO),
        output_rval_for_java(Info, Y, !IO),
        io.write_string(") & 0xffff)))", !IO)
    ).

output_rval_maybe_with_enum_for_java(Info, Rval, !IO) :-
    output_rval_for_java(Info, Rval, !IO),
    ( if rval_is_enum_object(Rval) then
        io.write_string(".MR_value", !IO)
    else
        true
    ).

:- pred output_binary_op_for_java(binary_op::in, io::di, io::uo) is det.

output_binary_op_for_java(Op, !IO) :-
    (
        ( Op = int_add(_), OpStr = "+"
        ; Op = int_sub(_), OpStr = "-"
        ; Op = int_mul(_), OpStr = "*"
        % NOTE: unsigned div and mod require special handling in Java.
        % See output_binop/6 above.
        ; Op = int_div(_), OpStr = "/"
        ; Op = int_mod(_), OpStr = "%"
        ; Op = unchecked_left_shift(_), OpStr = "<<"
        ; Op = bitwise_and(_), OpStr = "&"
        ; Op = bitwise_or(_), OpStr = "|"
        ; Op = bitwise_xor(_), OpStr = "^"
        ; Op = logical_and, OpStr = "&&"
        ; Op = logical_or, OpStr = "||"
        % NOTE: unsigned comparisons require special handling in Java.
        % See output_binop/6 above.
        ; Op = eq(_), OpStr = "=="
        ; Op = ne(_), OpStr = "!="
        ; Op = int_lt(_), OpStr = "<"
        ; Op = int_gt(_), OpStr = ">"
        ; Op = int_le(_), OpStr = "<="
        ; Op = int_ge(_), OpStr = ">="

        ; Op = float_eq, OpStr = "=="
        ; Op = float_ne, OpStr = "!="
        ; Op = float_le, OpStr = "<="
        ; Op = float_ge, OpStr = ">="
        ; Op = float_lt, OpStr = "<"
        ; Op = float_gt, OpStr = ">"

        ; Op = float_plus, OpStr = "+"
        ; Op = float_minus, OpStr = "-"
        ; Op = float_times, OpStr = "*"
        ; Op = float_divide, OpStr = "/"
        ),
        io.write_string(OpStr, !IO)
    ;
        Op = unchecked_right_shift(IntType),
        (
            ( IntType = int_type_int
            ; IntType = int_type_int8
            ; IntType = int_type_int16
            ; IntType = int_type_int32
            ; IntType = int_type_int64
            ),
            OpStr = ">>"
        ;
            ( IntType = int_type_uint
            ; IntType = int_type_uint8
            ; IntType = int_type_uint16
            ; IntType = int_type_uint32
            ; IntType = int_type_uint64
            ),
            OpStr = ">>>"
        ),
        io.write_string(OpStr, !IO)
    ;
        ( Op = array_index(_)
        ; Op = body
        ; Op = float_from_dword
        ; Op = int64_from_dword
        ; Op = uint64_from_dword
        ; Op = offset_str_eq(_)
        ; Op = str_cmp
        ; Op = str_eq
        ; Op = str_ge
        ; Op = str_gt
        ; Op = str_le
        ; Op = str_lt
        ; Op = str_ne
        ; Op = string_unsafe_index_code_unit
        ; Op = pointer_equal_conservative
        ; Op = unsigned_le
        ; Op = compound_eq
        ; Op = compound_lt
        ),
        unexpected($pred, "invalid binary operator")
    ).

:- pred output_rval_const_for_java(java_out_info::in, mlds_rval_const::in,
    io::di, io::uo) is det.

output_rval_const_for_java(Info, Const, !IO) :-
    (
        Const = mlconst_true,
        io.write_string("true", !IO)
    ;
        Const = mlconst_false,
        io.write_string("false", !IO)
    ;
        Const = mlconst_int(N),
        output_int_const_for_java(N, !IO)
    ;
        Const = mlconst_uint(U),
        % Java does not have unsigned integer literals.
        % XXX perhaps we should output this in hexadecimal?
        output_int_const_for_java(uint.cast_to_int(U), !IO)
    ;
        Const = mlconst_int8(I8),
        io.write_string("(byte)", !IO),
        io.write_int8(I8, !IO)
    ;
        Const = mlconst_uint8(U8),
        io.write_string("(byte)", !IO),
        io.write_int8(int8.cast_from_uint8(U8), !IO)
    ;
        Const = mlconst_int16(I16),
        io.write_string("(short)", !IO),
        io.write_int16(I16, !IO)
    ;
        Const = mlconst_uint16(U16),
        io.write_string("(short)", !IO),
        io.write_int16(int16.cast_from_uint16(U16), !IO)
    ;
        Const = mlconst_int32(I32),
        io.write_int32(I32, !IO)
    ;
        Const = mlconst_uint32(U32),
        io.write_int32(int32.cast_from_uint32(U32), !IO)
    ;
        Const = mlconst_int64(I64),
        io.write_int64(I64, !IO),
        io.write_string("L", !IO)
    ;
        Const = mlconst_uint64(U64),
        io.write_int64(int64.cast_from_uint64(U64), !IO),
        io.write_string("L", !IO)
    ;
        Const = mlconst_char(N),
        io.write_string("(", !IO),
        output_int_const_for_java(N, !IO),
        io.write_string(")", !IO)
    ;
        Const = mlconst_enum(N, EnumType),
        output_type_for_java(Info, EnumType, !IO),
        io.write_string(".K", !IO),
        output_int_const_for_java(N, !IO)
    ;
        Const = mlconst_foreign(Lang, Value, _Type),
        expect(unify(Lang, lang_java), $pred, "language other than Java."),
        % XXX Should we parenthesize this?
        io.write_string(Value, !IO)
    ;
        Const = mlconst_float(FloatVal),
        c_util.output_float_literal_cur_stream(FloatVal, !IO)
    ;
        Const = mlconst_string(String),
        io.write_string("""", !IO),
        c_util.output_quoted_string_lang_cur_stream(literal_java,
            String, !IO),
        io.write_string("""", !IO)
    ;
        Const = mlconst_multi_string(String),
        io.write_string("""", !IO),
        c_util.output_quoted_multi_string_lang_cur_stream(literal_java,
            String, !IO),
        io.write_string("""", !IO)
    ;
        Const = mlconst_named_const(TargetPrefixes, NamedConst),
        io.write_string(TargetPrefixes ^ java_prefix, !IO),
        io.write_string(NamedConst, !IO)
    ;
        Const = mlconst_code_addr(CodeAddr),
        IsCall = no,
        mlds_output_code_addr_for_java(Info, CodeAddr, IsCall, !IO)
    ;
        Const = mlconst_data_addr_local_var(LocalVarName),
        output_local_var_name_for_java(LocalVarName, !IO)
    ;
        Const = mlconst_data_addr_global_var(ModuleName, GlobalVarName),
        SymName = mlds_module_name_to_sym_name(ModuleName),
        mangle_sym_name_for_java(SymName, module_qual, "__", ModuleNameStr),
        io.write_string(ModuleNameStr, !IO),
        io.write_string(".", !IO),
        output_global_var_name_for_java(GlobalVarName, !IO)
    ;
        Const = mlconst_data_addr_rtti(ModuleName, RttiId),
        SymName = mlds_module_name_to_sym_name(ModuleName),
        mangle_sym_name_for_java(SymName, module_qual, "__", ModuleNameStr),
        io.write_string(ModuleNameStr, !IO),
        io.write_string(".", !IO),
        rtti.id_to_c_identifier(RttiId, RttiAddrName),
        io.write_string(RttiAddrName, !IO)
    ;
        Const = mlconst_data_addr_tabling(QualProcLabel, TablingId),
        QualProcLabel = qual_proc_label(ModuleName, ProcLabel),
        SymName = mlds_module_name_to_sym_name(ModuleName),
        mangle_sym_name_for_java(SymName, module_qual, "__", ModuleNameStr),
        io.write_string(ModuleNameStr, !IO),
        io.write_string(".", !IO),
        TablingPrefix = tabling_info_id_str(TablingId) ++ "_",
        io.write_string(TablingPrefix, !IO),
        mlds_output_proc_label_for_java(mlds_std_tabling_proc_label(ProcLabel),
            !IO)
    ;
        Const = mlconst_null(Type),
        Initializer = get_default_initializer_for_java(Type),
        io.write_string(Initializer, !IO)
    ).

:- pred output_int_const_for_java(int::in, io::di, io::uo) is det.

output_int_const_for_java(N, !IO) :-
    % The Mercury compiler could be using 64-bit integers but Java has 32-bit
    % ints. A literal 0xffffffff in a source file would be interpreted by a
    % 64-bit Mercury compiler as 4294967295. If it is written out in decimal,
    % a Java compiler would rightly complain because the integer is too large
    % to fit in a 32-bit int. However, it won't complain if the literal is
    % expressed in hexadecimal (nor as the negative decimal -1).
    ( if
        N > 0,
        N >> 31 = 1
    then
        % The bit pattern fits in 32 bits, but is too large to write as a
        % positive decimal. This branch is unreachable on a 32-bit compiler.
        io.format("0x%x", [i(N /\ 0xffffffff)], !IO)
    else
        io.write_int(N, !IO)
    ).

:- pred mlds_output_code_addr_for_java(java_out_info::in, mlds_code_addr::in,
    bool::in, io::di, io::uo) is det.

mlds_output_code_addr_for_java(Info, CodeAddr, IsCall, !IO) :-
    (
        IsCall = no,
        % Not a function call, so we are taking the address of the
        % wrapper for that function (method).
        io.write_string("new ", !IO),
        AddrOfMap = Info ^ joi_addrof_map,
        map.lookup(AddrOfMap, CodeAddr, CodeAddrWrapper),
        CodeAddrWrapper = code_addr_wrapper(ClassName, MaybePtrNum),
        io.write_string(ClassName, !IO),
        io.write_string("_0(", !IO),
        (
            MaybePtrNum = yes(PtrNum),
            io.write_int(PtrNum, !IO)
        ;
            MaybePtrNum = no
        ),
        io.write_string(")", !IO)
    ;
        IsCall = yes,
        CodeAddr = mlds_code_addr(QualFuncLabel, _Signature),
        QualFuncLabel = qual_func_label(ModuleName, FuncLabel),
        FuncLabel = mlds_func_label(ProcLabel, MaybeAux),
        output_qual_name_prefix_java(ModuleName, module_qual, !IO),
        mlds_output_proc_label_for_java(ProcLabel, !IO),
        io.write_string(mlds_maybe_aux_func_id_to_suffix(MaybeAux), !IO)
    ).

%---------------------------------------------------------------------------%

    % Succeeds iff the Rval represents an enumeration object in the Java
    % backend. We need to check both Rvals that are variables and Rvals
    % that are casts. We need to know this in order to append the field name
    % to the object so we can access the value of the enumeration object.
    %
:- pred rval_is_enum_object(mlds_rval::in) is semidet.

rval_is_enum_object(Rval) :-
    Rval = ml_lval(Lval),
    (
        Lval = ml_local_var(_, Type)
    ;
        Lval = ml_global_var(_, Type)
    ;
        Lval = ml_field(_, _, _, _, Type)
    ),
    type_is_enum(Type).

    % Succeeds iff this type is a enumeration.
    %
:- pred type_is_enum(mlds_type::in) is semidet.

type_is_enum(Type) :-
    Type = mercury_nb_type(_, CtorCat),
    CtorCat = ctor_cat_enum(_).

%---------------------------------------------------------------------------%

output_initializer_for_java(Info, OutputAux, Type, Initializer, !IO) :-
    (
        ( Initializer = init_obj(_)
        ; Initializer = init_struct(_, _)
        ; Initializer = init_array(_)
        ),
        io.write_string(" = ", !IO),
        % Due to cyclic references, we need to separate the allocation and
        % initialisation steps of RTTI structures. If InitStyle is alloc_only
        % then we output an initializer to allocate a structure without filling
        % in the fields.
        (
            ( OutputAux = oa_none
            ; OutputAux = oa_cname(_, _)
            ; OutputAux = oa_force_init
            ),
            output_initializer_body_for_java(Info, Initializer,
                yes(Type), !IO)
        ;
            OutputAux = oa_alloc_only,
            output_initializer_alloc_only_for_java(Info, Initializer,
                yes(Type), !IO)
        )
    ;
        Initializer = no_initializer,
        (
            OutputAux = oa_force_init,
            % Local variables need to be initialised to avoid warnings.
            io.write_string(" = ", !IO),
            io.write_string(get_default_initializer_for_java(Type), !IO)
        ;
            ( OutputAux = oa_none
            ; OutputAux = oa_cname(_, _)
            ; OutputAux = oa_alloc_only
            )
        )
    ).

output_initializer_alloc_only_for_java(Info, Initializer, MaybeType, !IO) :-
    (
        Initializer = no_initializer,
        unexpected($pred, "no_initializer")
    ;
        Initializer = init_obj(_),
        unexpected($pred, "init_obj")
    ;
        Initializer = init_struct(StructType, FieldInits),
        io.write_string("new ", !IO),
        ( if
            StructType = mercury_nb_type(_, CtorCat),
            type_category_is_array(CtorCat) = is_array
        then
            Size = list.length(FieldInits),
            io.format("java.lang.Object[%d]", [i(Size)], !IO)
        else
            output_type_for_java(Info, StructType, !IO),
            io.write_string("()", !IO)
        )
    ;
        Initializer = init_array(ElementInits),
        Size = list.length(ElementInits),
        io.write_string("new ", !IO),
        (
            MaybeType = yes(Type),
            type_to_string_for_java(Info, Type, String, ArrayDims),
            io.write_string(String, !IO),
            % Replace the innermost array dimension by the known size.
            ( if list.split_last(ArrayDims, Heads, 0) then
                output_array_dimensions(Heads ++ [Size], !IO)
            else
                unexpected($pred, "missing array dimension")
            )
        ;
            MaybeType = no,
            % XXX we need to know the type here
            io.format("/* XXX init_array */ Object[%d]", [i(Size)], !IO)
        )
    ).

output_initializer_body_for_java(Info, Initializer, MaybeType, !IO) :-
    (
        Initializer = no_initializer,
        unexpected($pred, "no_initializer")
    ;
        Initializer = init_obj(Rval),
        output_rval_for_java(Info, Rval, !IO)
    ;
        Initializer = init_struct(StructType, FieldInits),
        io.write_string("new ", !IO),
        output_type_for_java(Info, StructType, !IO),
        IsArray = type_is_array_for_java(StructType),
        io.write_string(if IsArray = is_array then " {" else "(", !IO),
        output_initializer_body_list_for_java(Info, FieldInits, !IO),
        io.write_char(if IsArray = is_array then '}' else ')', !IO)
    ;
        Initializer = init_array(ElementInits),
        io.write_string("new ", !IO),
        (
            MaybeType = yes(Type),
            output_type_for_java(Info, Type, !IO)
        ;
            MaybeType = no,
            % XXX we need to know the type here
            io.write_string("/* XXX init_array */ Object[]", !IO)
        ),
        io.write_string(" {\n\t\t", !IO),
        output_initializer_body_list_for_java(Info, ElementInits, !IO),
        io.write_string("}", !IO)
    ).

output_initializer_body_list_for_java(Info, Inits, !IO) :-
    io.write_list(Inits, ",\n\t\t",
        ( pred(Init::in, !.IO::di, !:IO::uo) is det :-
            output_initializer_body_for_java(Info, Init, no, !IO)
        ), !IO).

%---------------------------------------------------------------------------%

get_default_initializer_for_java(Type) = Initializer :-
    (
        Type = mercury_nb_type(_, CtorCat),
        (
            CtorCat = ctor_cat_builtin(_),
            unexpected($pred, "mercury_nb_type but ctor_cat_builtin")
        ;
            ( CtorCat = ctor_cat_system(_)
            ; CtorCat = ctor_cat_higher_order
            ; CtorCat = ctor_cat_tuple
            ; CtorCat = ctor_cat_enum(_)
            ; CtorCat = ctor_cat_builtin_dummy
            ; CtorCat = ctor_cat_variable
            ; CtorCat = ctor_cat_void
            ; CtorCat = ctor_cat_user(_)
            ),
            Initializer = "null"
        )
    ;
        ( Type = mlds_builtin_type_int(_)
        ; Type = mlds_builtin_type_float
        ),
        Initializer = "0"
    ;
        Type = mlds_builtin_type_string,
        Initializer = "null"
    ;
        Type = mlds_builtin_type_char,
        Initializer = "'\\u0000'"
    ;
        Type = mlds_native_bool_type,
        Initializer = "false"
    ;
        Type = mlds_foreign_type(ForeignLangType),
        ( if
            java_primitive_foreign_language_type(ForeignLangType, _, _,
                _, Initializer0)
        then
            Initializer = Initializer0
        else
            Initializer = "null"
        )
    ;
        ( Type = mlds_mercury_array_type(_)
        ; Type = mlds_cont_type(_)
        ; Type = mlds_commit_type
        ; Type = mlds_class_type(_)
        ; Type = mlds_array_type(_)
        ; Type = mlds_mostly_generic_array_type(_)
        ; Type = mlds_ptr_type(_)
        ; Type = mlds_func_type(_)
        ; Type = mlds_generic_type
        ; Type = mlds_generic_env_ptr_type
        ; Type = mlds_type_info_type
        ; Type = mlds_pseudo_type_info_type
        ; Type = mlds_rtti_type(_)
        ; Type = mlds_tabling_type(_)
        ),
        Initializer = "null"
    ;
        Type = mlds_unknown_type,
        unexpected($pred, "variable has unknown_type")
    ).

%---------------------------------------------------------------------------%
:- end_module ml_backend.mlds_to_java_data.
%---------------------------------------------------------------------------%
