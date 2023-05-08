%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2010-2012 The University of Melbourne.
% Copyright (C) 2013-2018, 2020-2022 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% Output MLDS lvals, rvals and initializers in C#.
%
%---------------------------------------------------------------------------%

:- module ml_backend.mlds_to_cs_data.
:- interface.

:- import_module ml_backend.mlds.
:- import_module ml_backend.mlds_to_cs_util.
:- import_module ml_backend.mlds_to_target_util.

:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module maybe.

%---------------------------------------------------------------------------%

:- pred output_lval_for_csharp(csharp_out_info::in, mlds_lval::in,
    io.text_output_stream::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- pred output_call_rval_for_csharp(csharp_out_info::in, mlds_rval::in,
    io.text_output_stream::in, io::di, io::uo) is det.

:- pred output_rval_for_csharp(csharp_out_info::in, mlds_rval::in,
    io.text_output_stream::in, io::di, io::uo) is det.

:- pred mlds_output_code_addr_for_csharp(csharp_out_info::in,
    io.text_output_stream::in, mlds_code_addr::in, bool::in,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%

    % Output " = " followed by the given initializer, if any, followed
    % by the given suffix string and a newline.
    %
    % The initializer is printed using output_initializer_body_for_java with
    % not_at_start_of_line (see below).
    %
:- pred output_initializer_for_csharp(csharp_out_info::in,
    io.text_output_stream::in, output_aux::in, indent::in, mlds_type::in,
    mlds_initializer::in, string::in, io::di, io::uo) is det.

    % Output the allocation part of the given initializer on the rest
    % of the current line.
    %
:- pred output_initializer_alloc_only_for_csharp(csharp_out_info::in,
    io.text_output_stream::in, mlds_initializer::in, maybe(mlds_type)::in,
    string::in, io::di, io::uo) is det.

    % Output the given initializer. The formatting depends on whether
    % the caller tells us that it has printed something on the current line
    % already (not_at_start_of_line) or not (at_start_of_line).
    %
    % If the initializer is for a struct or an array, we put the initializer
    % on separate lines, each indented by the given indent level, regardless
    % of where we start.
    %
    % If the initializer is for a single object, then we put its initializer
    % immediately after the previous of the current line if there is one
    % (not_at_start_of_line); otherwise (at_start_of_line), we indent it by
    % the specified level.
    %
    % In either case, we end the initializer with the given suffix
    % (which will usually be a semicolon or a comma) and a newline.
    %
:- pred output_initializer_body_for_csharp(csharp_out_info::in,
    io.text_output_stream::in, initializer_starts::in, indent::in,
    mlds_initializer::in, maybe(mlds_type)::in, string::in,
    io::di, io::uo) is det.

    % Output the given list of initializers with commas between them,
    % putting each initializer on its own line with the given indent.
    % Put the given suffix after the last initializer.
    %
:- pred output_nonempty_initializer_body_list_for_csharp(csharp_out_info::in,
    io.text_output_stream::in, indent::in, list(mlds_initializer)::in,
    string::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.builtin_ops.
:- import_module backend_libs.c_util.       % for output_quoted_string*
:- import_module backend_libs.rtti.
:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module libs.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module ml_backend.ml_util.
:- import_module ml_backend.mlds_to_cs_name.
:- import_module ml_backend.mlds_to_cs_type.
:- import_module parse_tree.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_type.

:- import_module int.
:- import_module int32.
:- import_module map.
:- import_module require.
:- import_module string.
:- import_module term.
:- import_module uint32.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

output_lval_for_csharp(Info, Lval, Stream, !IO) :-
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
            io.write_string(Stream, "((object[]) ", !IO),
            output_rval_for_csharp(Info, PtrRval, Stream, !IO),
            io.write_string(Stream, ")[", !IO),
            output_rval_for_csharp(Info, OffsetRval, Stream, !IO),
            io.write_string(Stream, "]", !IO)
        ;
            FieldId = ml_field_named(QualFieldVarName, CtorType),
            QualFieldVarName = qual_field_var_name(_, _, FieldVarName),
            ( if FieldVarName = fvn_data_tag then
                % If the field we are trying to access is just a `data_tag'
                % then it is a member of the base class.
                output_bracketed_rval_for_csharp(Info, PtrRval, Stream, !IO),
                io.write_string(Stream, ".", !IO)
            else if PtrRval = ml_self(_) then
                % Suppress type cast on `this' keyword. This makes a difference
                % when assigning to `final' member variables in constructor
                % functions.
                output_rval_for_csharp(Info, PtrRval, Stream, !IO),
                io.write_string(Stream, ".", !IO)
            else
                % Otherwise the field we are trying to access may be
                % in a derived class. Objects are manipulated as instances
                % of their base class, so we need to downcast to the derived
                % class to access some fields.
                io.format(Stream, "((%s) ",
                    [s(type_to_string_for_csharp(Info, CtorType))], !IO),
                output_bracketed_rval_for_csharp(Info, PtrRval, Stream, !IO),
                io.write_string(Stream, ").", !IO)
            ),
            output_field_var_name_for_csharp(Stream, FieldVarName, !IO)
        )
    ;
        Lval = ml_mem_ref(Rval, _Type),
        output_bracketed_rval_for_csharp(Info, Rval, Stream, !IO)
    ;
        Lval = ml_target_global_var_ref(GlobalVarRef),
        GlobalVarRef = env_var_ref(EnvVarName),
        io.write_string(Stream, "mercury_envvar_", !IO),
        io.write_string(Stream, EnvVarName, !IO)
    ;
        Lval = ml_local_var(LocalVarName, _),
        output_local_var_name_for_csharp(Stream, LocalVarName, !IO)
    ;
        Lval = ml_global_var(QualGlobalVarName, _),
        output_maybe_qualified_global_var_name_for_csharp(Info, Stream,
            QualGlobalVarName, !IO)
    ).

%---------------------------------------------------------------------------%

output_call_rval_for_csharp(Info, Rval, Stream, !IO) :-
    ( if
        Rval = ml_const(Const),
        Const = mlconst_code_addr(CodeAddr)
    then
        IsCall = yes,
        mlds_output_code_addr_for_csharp(Info, Stream, CodeAddr, IsCall, !IO)
    else
        output_bracketed_rval_for_csharp(Info, Rval, Stream, !IO)
    ).

:- pred output_bracketed_rval_for_csharp(csharp_out_info::in, mlds_rval::in,
    io.text_output_stream::in, io::di, io::uo) is det.

output_bracketed_rval_for_csharp(Info, Rval, Stream, !IO) :-
    ( if
        % If it is just a variable name, then we don't need parentheses.
        ( Rval = ml_lval(ml_local_var(_,_))
        ; Rval = ml_lval(ml_global_var(_,_))
        ; Rval = ml_const(mlconst_code_addr(_))
        )
    then
        output_rval_for_csharp(Info, Rval, Stream, !IO)
    else
        io.write_char(Stream, '(', !IO),
        output_rval_for_csharp(Info, Rval, Stream, !IO),
        io.write_char(Stream, ')', !IO)
    ).

output_rval_for_csharp(Info, Rval, Stream, !IO) :-
    (
        Rval = ml_lval(Lval),
        output_lval_for_csharp(Info, Lval, Stream, !IO)
    ;
        Rval = ml_mkword(_, _),
        unexpected($pred, "tags not supported in C#")
    ;
        Rval = ml_const(Const),
        output_rval_const_for_csharp(Info, Stream, Const, !IO)
    ;
        Rval = ml_cast(Type, SubRval),
        output_cast_rval_for_csharp(Info, Type, SubRval, Stream, !IO)
    ;
        Rval = ml_box(Type, SubRval),
        ( if Type = mercury_nb_type(comparison_result_type, _) then
            io.write_string(Stream,
                "builtin.comparison_result_object[(int) ", !IO),
            output_rval_for_csharp(Info, SubRval, Stream, !IO),
            io.write_string(Stream, "]", !IO)
        else
            output_boxed_rval_for_csharp(Info, Type, SubRval, Stream, !IO)
        )
    ;
        Rval = ml_unbox(Type, SubRval),
        output_unboxed_rval_for_csharp(Info, Type, SubRval, Stream, !IO)
    ;
        Rval = ml_unop(Unop, SubRval),
        output_unop_for_csharp(Info, Stream, Unop, SubRval, !IO)
    ;
        Rval = ml_binop(Op, SubRvalA, SubRvalB),
        output_binop_for_csharp(Info, Stream, Op, SubRvalA, SubRvalB, !IO)
    ;
        Rval = ml_mem_addr(Lval),
        io.write_string(Stream, "out ", !IO),
        output_lval_for_csharp(Info, Lval, Stream, !IO)
    ;
        Rval = ml_scalar_common(_),
        unexpected($pred, "ml_scalar_common")
    ;
        Rval = ml_scalar_common_addr(ScalarCommon),
        ScalarCommon = mlds_scalar_common(ModuleName, _Type,
            ml_scalar_common_type_num(TypeNum), RowNum),
        ModuleSymName = mlds_module_name_to_sym_name(ModuleName),
        MangledModuleName =
            strip_mercury_and_mangle_sym_name_for_csharp(ModuleSymName),
        io.format(Stream, "%s.MR_scalar_common_%d[%d]",
            [s(MangledModuleName), i(TypeNum), i(RowNum)], !IO)
    ;
        Rval = ml_vector_common_row_addr(VectorCommon, RowRval),
        VectorCommon = mlds_vector_common(_ModuleName, _Type,
            ml_vector_common_type_num(TypeNum), StartRowNum, _NumRows),
        % XXX Why do we print a "MangledModuleName." prefix for scalar common
        % addresses but not for vector common addresses?
        io.format(Stream, "MR_vector_common_%d[%d + ",
            [i(TypeNum), i(StartRowNum)], !IO),
        output_rval_for_csharp(Info, RowRval, Stream, !IO),
        io.write_string(Stream, "]", !IO)
    ;
        Rval = ml_self(_),
        io.write_string(Stream, "this", !IO)
    ).

:- pred output_cast_rval_for_csharp(csharp_out_info::in, mlds_type::in,
    mlds_rval::in, io.text_output_stream::in, io::di, io::uo) is det.

output_cast_rval_for_csharp(Info, Type, Expr, Stream, !IO) :-
    % rtti_to_mlds.m generates casts from int to runtime.PseudoTypeInfo, but
    % for C# we need to treat these as constructions, not casts.
    % Similarly for conversions from TypeCtorInfo to TypeInfo.
    ( if
        Type = mlds_pseudo_type_info_type,
        Expr = ml_const(mlconst_int(N))
    then
        maybe_output_inline_comment_for_csharp(Info, Stream, "cast", !IO),
        ( if have_preallocated_pseudo_type_var_for_csharp(N) then
            io.write_string(Stream, "runtime.PseudoTypeInfo.K", !IO),
            io.write_int(Stream, N, !IO)
        else
            io.write_string(Stream, "new runtime.PseudoTypeInfo(", !IO),
            output_rval_for_csharp(Info, Expr, Stream, !IO),
            io.write_string(Stream, ")", !IO)
        )
    else if
        ( Type = mercury_nb_type(_, ctor_cat_system(cat_system_type_info))
        ; Type = mlds_type_info_type
        )
    then
        % XXX We really should be able to tell if we are casting a
        % TypeCtorInfo or a TypeInfo. Julien says that is probably going to
        % be rather difficult as the compiler doesn't keep track of where
        % type_ctor_infos are acting as type_infos properly. (zs agrees.)
        maybe_output_inline_comment_for_csharp(Info, Stream, "cast", !IO),
        io.write_string(Stream, "runtime.TypeInfo_Struct.maybe_new(", !IO),
        output_rval_for_csharp(Info, Expr, Stream, !IO),
        io.write_string(Stream, ")", !IO)
    else
        % While the Java backend represents Mercury enums as Java classes
        % with a value field, the C# backend represents them as C# enums.
        % We therefore have no need to get a value field.
        io.format(Stream, "(%s) ",
            [s(type_to_string_for_csharp(Info, Type))], !IO),
        output_rval_for_csharp(Info, Expr, Stream, !IO)
    ).

:- pred have_preallocated_pseudo_type_var_for_csharp(int::in) is semidet.

have_preallocated_pseudo_type_var_for_csharp(N) :-
    % Corresponds to static members in class PseudoTypeInfo.
    N >= 1,
    N =< 5.

:- pred output_boxed_rval_for_csharp(csharp_out_info::in, mlds_type::in,
    mlds_rval::in, io.text_output_stream::in, io::di, io::uo) is det.

output_boxed_rval_for_csharp(Info, _Type, Expr, Stream, !IO) :-
    % C# does implicit boxing.
    output_rval_for_csharp(Info, Expr, Stream, !IO).
%   ( if csharp_builtin_type(Type, _JavaName, JavaBoxedName, _) then
%       % valueOf may return cached instances instead of creating new objects.
%       io.write_string(Stream, JavaBoxedName, !IO),
%       io.write_string(Stream, ".valueOf(", !IO),
%       output_rval(Info, Expr, !IO),
%       io.write_string(Stream, ")", !IO)
%   else
%       io.write_string(Stream, "((object) (", !IO),
%       output_rval(Info, Expr, !IO),
%       io.write_string(Stream, "))", !IO)
%   ).

:- pred output_unboxed_rval_for_csharp(csharp_out_info::in, mlds_type::in,
    mlds_rval::in, io.text_output_stream::in, io::di, io::uo) is det.

output_unboxed_rval_for_csharp(Info, Type, Expr, Stream, !IO) :-
    io.format(Stream, "((%s) ",
        [s(type_to_string_for_csharp(Info, Type))], !IO),
    output_rval_for_csharp(Info, Expr, Stream, !IO),
    io.write_string(Stream, ")", !IO).

%---------------------------------------------------------------------------%

:- pred output_unop_for_csharp(csharp_out_info::in, io.text_output_stream::in,
    unary_op::in, mlds_rval::in, io::di, io::uo) is det.

output_unop_for_csharp(Info, Stream, UnaryOp, Expr, !IO) :-
    % For the C# backend, there are no tags, so all the tagging operators
    % are no-ops, except for `tag', which always returns zero (a tag of zero
    % means there is no tag).
    (
        UnaryOp = tag ,
        io.write_string(Stream, "/* tag */  0", !IO)
    ;
        ( UnaryOp = strip_tag, UnaryOpStr = "/* strip_tag */ "
        ; UnaryOp = mkbody,    UnaryOpStr = "/* mkbody */ "
        ; UnaryOp = unmkbody,  UnaryOpStr = "/* unmkbody */ "
        ; UnaryOp = logical_not, UnaryOpStr = "!"
        ; UnaryOp = hash_string,  UnaryOpStr = "mercury.String.hash_1_f_0"
        ; UnaryOp = hash_string2, UnaryOpStr = "mercury.String.hash2_1_f_0"
        ; UnaryOp = hash_string3, UnaryOpStr = "mercury.String.hash3_1_f_0"
        ; UnaryOp = hash_string4, UnaryOpStr = "mercury.String.hash4_1_f_0"
        ; UnaryOp = hash_string5, UnaryOpStr = "mercury.String.hash5_1_f_0"
        ; UnaryOp = hash_string6, UnaryOpStr = "mercury.String.hash6_1_f_0"
        ),
        io.write_string(Stream, UnaryOpStr, !IO),
        io.write_string(Stream, "(", !IO),
        output_rval_for_csharp(Info, Expr, Stream, !IO),
        io.write_string(Stream, ")", !IO)
    ;
        UnaryOp = bitwise_complement(IntType),
        (
            ( IntType = int_type_int
            ; IntType = int_type_uint
            ; IntType = int_type_int32
            ; IntType = int_type_uint32
            ; IntType = int_type_int64
            ; IntType = int_type_uint64
            ),
            io.write_string(Stream, "~(", !IO),
            output_rval_for_csharp(Info, Expr, Stream, !IO),
            io.write_string(Stream, ")", !IO)
        ;
            ( IntType = int_type_int8, CastStr = "(sbyte)"
            ; IntType = int_type_int16, CastStr = "(short)"
            ),
            io.write_string(Stream, CastStr, !IO),
            io.write_string(Stream, " ~(", !IO),
            output_rval_for_csharp(Info, Expr, Stream, !IO),
            io.write_string(Stream, ")", !IO)
        ;
            % The result of the bitwise complement of byte or ushort in C#
            % will be promoted to an int. Casting this back to the original
            % type may result in a CS0221 error from the C# compiler
            % due to the possible information loss involved in such a cast.
            % We need to wrap the whole expression in an unchecked context
            % in order to prevent the C# compiler from treating it as an error.
            ( IntType = int_type_uint8, CastStr = "(byte)"
            ; IntType = int_type_uint16, CastStr = "(ushort)"
            ),
            io.write_string(Stream, "unchecked (", !IO),
            io.write_string(Stream, CastStr, !IO),
            io.write_string(Stream, " ~(", !IO),
            output_rval_for_csharp(Info, Expr, Stream, !IO),
            io.write_string(Stream, "))", !IO)
        )
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

:- pred output_binop_for_csharp(csharp_out_info::in, io.text_output_stream::in,
    binary_op::in, mlds_rval::in, mlds_rval::in, io::di, io::uo) is det.

output_binop_for_csharp(Info, Stream, Op, X, Y, !IO) :-
    (
        Op = array_index(_Type),
        output_bracketed_rval_for_csharp(Info, X, Stream, !IO),
        io.write_string(Stream, "[", !IO),
        output_rval_for_csharp(Info, Y, Stream, !IO),
        io.write_string(Stream, "]", !IO)
    ;
        Op = str_eq,
        output_rval_for_csharp(Info, X, Stream, !IO),
        io.write_string(Stream, ".Equals(", !IO),
        output_rval_for_csharp(Info, Y, Stream, !IO),
        io.write_string(Stream, ")", !IO)
    ;
        ( Op = str_ne, OpStr = "!="
        ; Op = str_lt, OpStr = "<"
        ; Op = str_gt, OpStr = ">"
        ; Op = str_le, OpStr = "<="
        ; Op = str_ge, OpStr = ">="
        ),
        io.write_string(Stream, "(", !IO),
        output_rval_for_csharp(Info, X, Stream, !IO),
        io.write_string(Stream, ".CompareOrdinal(", !IO),
        output_rval_for_csharp(Info, Y, Stream, !IO),
        io.write_string(Stream, ") ", !IO),
        io.write_string(Stream, OpStr, !IO),
        io.write_string(Stream, " 0)", !IO)
    ;
        Op = str_cmp,
        io.write_string(Stream, "(", !IO),
        output_rval_for_csharp(Info, X, Stream, !IO),
        io.write_string(Stream, ".CompareOrdinal(", !IO),
        output_rval_for_csharp(Info, Y, Stream, !IO),
        io.write_string(Stream, "))", !IO)
    ;
        Op = pointer_equal_conservative,
        io.write_string(Stream, "System.Object.ReferenceEquals(", !IO),
        output_rval_for_csharp(Info, X, Stream, !IO),
        io.write_string(Stream, ", ", !IO),
        output_rval_for_csharp(Info, Y, Stream, !IO),
        io.write_string(Stream, ")", !IO)
    ;
        ( Op = int_add(_)
        ; Op = int_sub(_)
        ; Op = int_mul(_)
        ; Op = int_div(_)
        ; Op = int_mod(_)
        ; Op = unchecked_left_shift(_, _)
        ; Op = unchecked_right_shift(_, _)
        ; Op = bitwise_and(_)
        ; Op = bitwise_or(_)
        ; Op = bitwise_xor(_)
        ; Op = int_lt(_)
        ; Op = int_gt(_)
        ; Op = int_le(_)
        ; Op = int_ge(_)
        ),
        output_int_binop_for_csharp(Info, Stream, Op, X, Y, !IO)
    ;
        ( Op = unsigned_lt, OpStr = "<"
        ; Op = unsigned_le, OpStr = "<="
        ),
        io.write_string(Stream, "((uint) ", !IO),
        output_rval_for_csharp(Info, X, Stream, !IO),
        io.format(Stream, " %s (uint) ", [s(OpStr)], !IO),
        output_rval_for_csharp(Info, Y, Stream, !IO),
        io.write_string(Stream, ")", !IO)
    ;
        ( Op = logical_and, OpStr = "&&"
        ; Op = logical_or,  OpStr = "||"
        ; Op = eq(_),       OpStr = "=="
        ; Op = ne(_),       OpStr = "!="
        ; Op = float_add,   OpStr = "+"
        ; Op = float_sub,   OpStr = "-"
        ; Op = float_mul,   OpStr = "*"
        ; Op = float_div,   OpStr = "/"
        ; Op = float_eq,    OpStr = "=="
        ; Op = float_ne,    OpStr = "!="
        ; Op = float_lt,    OpStr = "<"
        ; Op = float_gt,    OpStr = ">"
        ; Op = float_le,    OpStr = "<="
        ; Op = float_ge,    OpStr = ">="
        ),
        output_basic_binop_for_csharp(Info, Stream, OpStr, X, Y, !IO)
    ;
        ( Op = body
        ; Op = string_unsafe_index_code_unit
        ; Op = offset_str_eq(_)
        ; Op = float_from_dword
        ; Op = int64_from_dword
        ; Op = uint64_from_dword
        ; Op = compound_eq
        ; Op = compound_lt
        ),
        unexpected($pred, "invalid binary operator")
    ).

:- pred output_int_binop_for_csharp(csharp_out_info::in,
    io.text_output_stream::in, binary_op::in(int_binary_op),
    mlds_rval::in, mlds_rval::in, io::di, io::uo) is det.
:- pragma no_inline(pred(output_int_binop_for_csharp/7)).

output_int_binop_for_csharp(Info, Stream, Op, X, Y, !IO) :-
    (
        ( Op = int_add(Type),       OpStr = "+"
        ; Op = int_sub(Type),       OpStr = "-"
        ; Op = int_mul(Type),       OpStr = "*"
        ; Op = int_div(Type),       OpStr = "/"
        ; Op = int_mod(Type),       OpStr = "%"
        ; Op = bitwise_and(Type),   OpStr = "&"
        ; Op = bitwise_xor(Type),   OpStr = "^"
        ),
        ( Type = int_type_int,      Cast = ""
        ; Type = int_type_int8,     Cast = "(sbyte) "
        ; Type = int_type_int16,    Cast = "(short) "
        ; Type = int_type_int32,    Cast = ""
        ; Type = int_type_int64,    Cast = ""
        ; Type = int_type_uint,     Cast = ""
        ; Type = int_type_uint8,    Cast = "(byte) "
        ; Type = int_type_uint16,   Cast = "(ushort) "
        ; Type = int_type_uint32,   Cast = ""
        ; Type = int_type_uint64,   Cast = ""
        ),
        io.write_string(Stream, Cast, !IO),
        output_basic_binop_for_csharp(Info, Stream, OpStr, X, Y, !IO)
    ;
        Op = bitwise_or(Type),
        OpStr = "|",
        (
            ( Type = int_type_int,      Cast = ""
            ; Type = int_type_int16,    Cast = "(short) "
            ; Type = int_type_int32,    Cast = ""
            ; Type = int_type_int64,    Cast = ""
            ; Type = int_type_uint,     Cast = ""
            ; Type = int_type_uint8,    Cast = "(byte) "
            ; Type = int_type_uint16,   Cast = "(ushort) "
            ; Type = int_type_uint32,   Cast = ""
            ; Type = int_type_uint64,   Cast = ""
            ),
            io.write_string(Stream, Cast, !IO),
            output_basic_binop_for_csharp(Info, Stream, OpStr, X, Y, !IO)
        ;
            Type = int_type_int8,
            % The special treatment of bitwise-or for int8 is necessary
            % to avoid warning CS0675 from the C# compiler.
            % XXX The Microsoft C# reference manual' section on compiler
            % messages says that this warning is for a "bitwise OR operator
            % used on a sign extended operand". I (zs) understand why this
            % deserves a warning, but do not understand why the same warning
            % does not apply to bitwise AND and bitwise XOR.
            % Unfortunately, the chance that a maintainer of the C# compiler
            % will read this comment is zero :-(
            io.write_string(Stream, "(sbyte) ((byte) ", !IO),
            output_rval_for_csharp(Info, X, Stream, !IO),
            io.write_string(Stream, " ", !IO),
            io.write_string(Stream, OpStr, !IO),
            io.write_string(Stream, " (byte) ", !IO),
            output_rval_for_csharp(Info, Y, Stream, !IO),
            io.write_string(Stream, ")", !IO)
        )
    ;
        ( Op = int_lt(_Type),   OpStr = "<"
        ; Op = int_gt(_Type),   OpStr = ">"
        ; Op = int_le(_Type),   OpStr = "<="
        ; Op = int_ge(_Type),   OpStr = ">="
        ),
        output_basic_binop_for_csharp(Info, Stream, OpStr, X, Y, !IO)
    ;
        ( Op = unchecked_left_shift(Type, ShiftType),  OpStr = "<<"
        ; Op = unchecked_right_shift(Type, ShiftType), OpStr = ">>"
        ),
        % C# does not automatically promote uints to ints, since
        % half of all uints are too big to be represented as ints.
        % However, the only valid shift amounts are very small,
        % so for valid shift amounts, the cast cannot lose information.
        (
            ShiftType = shift_by_int,
            CastY = ""
        ;
            ShiftType = shift_by_uint,
            CastY = "(int) "
        ),
        ( Type = int_type_int,      Cast = ""
        ; Type = int_type_uint,     Cast = ""
        ; Type = int_type_int32,    Cast = ""
        ; Type = int_type_uint32,   Cast = ""
        ; Type = int_type_int64,    Cast = ""
        ; Type = int_type_uint64,   Cast = ""
        ; Type = int_type_int8,     Cast = "(sbyte) "
        ; Type = int_type_uint8,    Cast = "(byte) "
        ; Type = int_type_int16,    Cast = "(short) "
        ; Type = int_type_uint16,   Cast = "(ushort) "
        ),
        io.write_string(Stream, Cast, !IO),
        io.write_string(Stream, "(", !IO),
        output_rval_for_csharp(Info, X, Stream, !IO),
        io.write_string(Stream, " ", !IO),
        io.write_string(Stream, OpStr, !IO),
        io.write_string(Stream, " ", !IO),
        io.write_string(Stream, CastY, !IO),
        output_rval_for_csharp(Info, Y, Stream, !IO),
        io.write_string(Stream, ")", !IO)
    ).

:- pred output_basic_binop_for_csharp(csharp_out_info::in,
    io.text_output_stream::in, string::in, mlds_rval::in, mlds_rval::in,
    io::di, io::uo) is det.
:- pragma no_inline(pred(output_basic_binop_for_csharp/7)).

output_basic_binop_for_csharp(Info, Stream, OpStr, X, Y, !IO) :-
    io.write_string(Stream, "(", !IO),
    output_rval_for_csharp(Info, X, Stream, !IO),
    io.write_string(Stream, " ", !IO),
    io.write_string(Stream, OpStr, !IO),
    io.write_string(Stream, " ", !IO),
    output_rval_for_csharp(Info, Y, Stream, !IO),
    io.write_string(Stream, ")", !IO).

%---------------------------------------------------------------------------%

:- pred output_rval_const_for_csharp(csharp_out_info::in,
    io.text_output_stream::in, mlds_rval_const::in, io::di, io::uo) is det.

output_rval_const_for_csharp(Info, Stream, Const, !IO) :-
    (
        Const = mlconst_true,
        io.write_string(Stream, "true", !IO)
    ;
        Const = mlconst_false,
        io.write_string(Stream, "false", !IO)
    ;
        Const = mlconst_int(N),
        output_int_const_for_csharp(Stream, N, !IO)
    ;
        Const = mlconst_uint(N),
        output_uint_const_for_csharp(Stream, N, !IO)
    ;
        Const = mlconst_int8(N),
        output_int8_const_for_csharp(Stream, N, !IO)
    ;
        Const = mlconst_uint8(N),
        output_uint8_const_for_csharp(Stream, N, !IO)
    ;
        Const = mlconst_int16(N),
        output_int16_const_for_csharp(Stream, N, !IO)
    ;
        Const = mlconst_uint16(N),
        output_uint16_const_for_csharp(Stream, N, !IO)
    ;
        Const = mlconst_int32(N),
        output_int32_const_for_csharp(Stream, N, !IO)
    ;
        Const = mlconst_uint32(N),
        output_uint32_const_for_csharp(Stream, N, !IO)
    ;
        Const = mlconst_int64(N),
        output_int64_const_for_csharp(Stream, N, !IO)
    ;
        Const = mlconst_uint64(N),
        output_uint64_const_for_csharp(Stream, N, !IO)
    ;
        Const = mlconst_char(C),
        io.write_string(Stream, "( ", !IO),
        output_int_const_for_csharp(Stream, C, !IO),
        io.write_string(Stream, ")", !IO)
    ;
        Const = mlconst_enum(N, EnumType),
        % Explicit cast required.
        output_cast_rval_for_csharp(Info, EnumType,
            ml_const(mlconst_int(N)), Stream, !IO)
    ;
        Const = mlconst_foreign(Lang, Value, Type),
        expect(unify(Lang, lang_csharp), $pred, "language other than C#."),
        % XXX Should we parenthesize this?
        io.format(Stream, "(%s) ",
            [s(type_to_string_for_csharp(Info, Type))], !IO),
        io.write_string(Stream, Value, !IO)
    ;
        Const = mlconst_float(FloatVal),
        c_util.output_float_literal(Stream, FloatVal, !IO)
    ;
        Const = mlconst_string(String),
        output_quoted_string_csharp(Stream, String, !IO)
    ;
        Const = mlconst_multi_string(String),
        output_quoted_multi_string_csharp(Stream, String, !IO)
    ;
        Const = mlconst_named_const(TargetPrefixes, NamedConst),
        io.write_string(Stream, TargetPrefixes ^ csharp_prefix, !IO),
        io.write_string(Stream, NamedConst, !IO)
    ;
        Const = mlconst_code_addr(CodeAddr),
        map.lookup(Info ^ csoi_code_addrs, CodeAddr, Name),
        io.write_string(Stream, Name, !IO)
    ;
        Const = mlconst_data_addr_local_var(VarName),
        local_var_name_to_string_for_csharp(VarName, VarNameStr),
        write_identifier_string_for_csharp(Stream, VarNameStr, !IO)
    ;
        Const = mlconst_data_addr_global_var(ModuleName, VarName),
        MangledModuleName = strip_mercury_and_mangle_sym_name_for_csharp(
            mlds_module_name_to_sym_name(ModuleName)),
        global_var_name_to_string_for_csharp(VarName, VarNameStr),
        io.write_string(Stream, MangledModuleName, !IO),
        io.write_string(Stream, ".", !IO),
        write_identifier_string_for_csharp(Stream, VarNameStr, !IO)
    ;
        Const = mlconst_data_addr_rtti(ModuleName, RttiId),
        MangledModuleName = strip_mercury_and_mangle_sym_name_for_csharp(
            mlds_module_name_to_sym_name(ModuleName)),
        rtti.id_to_c_identifier(RttiId, RttiAddrName),
        io.write_string(Stream, MangledModuleName, !IO),
        io.write_string(Stream, ".", !IO),
        write_identifier_string_for_csharp(Stream, RttiAddrName, !IO)
    ;
        Const = mlconst_data_addr_tabling(_QualProcLabel, _TablingId),
        unexpected($pred, "NYI: mlconst_data_addr_tabling")
    ;
        Const = mlconst_null(Type),
        Initializer = get_default_initializer_for_csharp(Info, Type),
        io.write_string(Stream, Initializer, !IO)
    ).

:- pred output_int_const_for_csharp(io.text_output_stream::in, int::in,
    io::di, io::uo) is det.

output_int_const_for_csharp(Stream, N, !IO) :-
    % You may wish to see the comment on output_int_const_for_java
    % in mlds_to_java_data.m.
    ( if
        N > 0,
        not int32.from_int(N, _I32),
        uint32.from_int(N, U32)
    then
        % The bit pattern fits in 32 bits, but is too big for a positive
        % integer. The C# compiler will report an error about this unless we
        % tell it otherwise.
        N32 = uint32.cast_to_int(U32),
        io.format(Stream, "unchecked((int) 0x%x)", [i(N32)], !IO)
    else
        io.write_int(Stream, N, !IO)
    ).

:- pred output_uint_const_for_csharp(io.text_output_stream::in, uint::in,
    io::di, io::uo) is det.

output_uint_const_for_csharp(Stream, U, !IO) :-
    io.write_uint(Stream, U, !IO),
    io.write_string(Stream, "U", !IO).

:- pred output_int8_const_for_csharp(io.text_output_stream::in, int8::in,
    io::di, io::uo) is det.

output_int8_const_for_csharp(Stream, I8, !IO) :-
    io.write_string(Stream, "(sbyte) ", !IO),
    io.write_int8(Stream, I8, !IO).

:- pred output_uint8_const_for_csharp(io.text_output_stream::in, uint8::in,
    io::di, io::uo) is det.

output_uint8_const_for_csharp(Stream, U8, !IO) :-
    io.write_string(Stream, "(byte) ", !IO),
    io.write_uint8(Stream, U8, !IO).

:- pred output_int16_const_for_csharp(io.text_output_stream::in, int16::in,
    io::di, io::uo) is det.

output_int16_const_for_csharp(Stream, I16, !IO) :-
    io.write_string(Stream, "(short) ", !IO),
    io.write_int16(Stream, I16, !IO).

:- pred output_uint16_const_for_csharp(io.text_output_stream::in, uint16::in,
    io::di, io::uo) is det.

output_uint16_const_for_csharp(Stream, U16, !IO) :-
    io.write_string(Stream, "(ushort) ", !IO),
    io.write_uint16(Stream, U16, !IO).

:- pred output_int32_const_for_csharp(io.text_output_stream::in, int32::in,
    io::di, io::uo) is det.

output_int32_const_for_csharp(Stream, I32, !IO) :-
    io.write_int32(Stream, I32, !IO).

:- pred output_uint32_const_for_csharp(io.text_output_stream::in, uint32::in,
    io::di, io::uo) is det.

output_uint32_const_for_csharp(Stream, U32, !IO) :-
    io.write_uint32(Stream, U32, !IO),
    io.write_string(Stream, "U", !IO).

:- pred output_int64_const_for_csharp(io.text_output_stream::in, int64::in,
    io::di, io::uo) is det.

output_int64_const_for_csharp(Stream, I64, !IO) :-
    io.write_int64(Stream, I64, !IO),
    io.write_string(Stream, "L", !IO).

:- pred output_uint64_const_for_csharp(io.text_output_stream::in, uint64::in,
    io::di, io::uo) is det.

output_uint64_const_for_csharp(Stream, U64, !IO) :-
    io.write_uint64(Stream, U64, !IO),
    io.write_string(Stream, "UL", !IO).

%---------------------------------------------------------------------------%

mlds_output_code_addr_for_csharp(Info, Stream, CodeAddr, IsCall, !IO) :-
    CodeAddr = mlds_code_addr(QualFuncLabel, Signature),
    Signature = mlds_func_signature(ArgTypes, RetTypes),
    (
        IsCall = no,
        % Not a function call, so we are taking the address of the
        % wrapper for that function (method).
        TypeString = method_ptr_type_to_string(Info, ArgTypes, RetTypes),
        io.format(Stream, "(%s) ", [s(TypeString)], !IO)
    ;
        IsCall = yes
    ),
    QualFuncLabel = qual_func_label(ModuleName, FuncLabel),
    FuncLabel = mlds_func_label(ProcLabel, MaybeAux),
    MaybeAuxSuffix = mlds_maybe_aux_func_id_to_suffix(MaybeAux),
    output_qual_name_prefix_cs(Stream, ModuleName, module_qual, !IO),
    mlds_output_proc_label(Stream, MaybeAuxSuffix, ProcLabel, !IO).

%---------------------------------------------------------------------------%

output_initializer_for_csharp(Info, Stream, OutputAux, Indent, Type,
        Initializer, Suffix, !IO) :-
    (
        ( Initializer = init_obj(_)
        ; Initializer = init_struct(_, _)
        ; Initializer = init_array(_)
        ),
        io.write_string(Stream, " = ", !IO),
        % Due to cyclic references, we need to separate the allocation and
        % initialisation steps of RTTI structures. If InitStyle is alloc_only,
        % then we output an initializer to allocate a structure without filling
        % in the fields.
        (
            ( OutputAux = oa_none
            ; OutputAux = oa_cname(_, _)
            ; OutputAux = oa_force_init
            ),
            output_initializer_body_for_csharp(Info, Stream,
                not_at_start_of_line, Indent + 1, Initializer, yes(Type),
                Suffix, !IO)
        ;
            OutputAux = oa_alloc_only,
            output_initializer_alloc_only_for_csharp(Info, Stream,
                Initializer, yes(Type), Suffix, !IO)
        )
    ;
        Initializer = no_initializer,
        (
            OutputAux = oa_force_init,
            % Local variables need to be initialised to avoid warnings.
            io.write_string(Stream, " = ", !IO),
            io.write_string(Stream,
                get_default_initializer_for_csharp(Info, Type), !IO)
        ;
            ( OutputAux = oa_none
            ; OutputAux = oa_cname(_, _)
            ; OutputAux = oa_alloc_only
            )
        ),
        io.format(Stream, "%s\n", [s(Suffix)], !IO)
    ).

%---------------------%

output_initializer_alloc_only_for_csharp(Info, Stream, Initializer,
        MaybeType, Suffix, !IO) :-
    (
        Initializer = no_initializer,
        unexpected($pred, "no_initializer")
    ;
        Initializer = init_obj(_),
        unexpected($pred, "init_obj")
    ;
        Initializer = init_struct(StructType, FieldInits),
        io.write_string(Stream, "new ", !IO),
        ( if
            StructType = mercury_nb_type(_, CtorCat),
            type_category_is_array(CtorCat) = is_array
        then
            Size = list.length(FieldInits),
            io.format(Stream, "object[%d]%s\n", [i(Size), s(Suffix)], !IO)
        else
            io.format(Stream, "%s()%s\n",
                [s(type_to_string_for_csharp(Info, StructType)), s(Suffix)],
                !IO)
        )
    ;
        Initializer = init_array(ElementInits),
        Size = list.length(ElementInits),
        io.write_string(Stream, "new ", !IO),
        (
            MaybeType = yes(Type),
            type_to_string_and_dims_for_csharp(Info, Type,
                BaseTypeName, ArrayDims0),
            make_last_dimension_known_size(ArrayDims0, Size, ArrayDims),
            DimsStr = array_dimensions_to_string(ArrayDims),
            io.format(Stream, "%s%s%s\n",
                [s(BaseTypeName), s(DimsStr), s(Suffix)], !IO)
        ;
            MaybeType = no,
            % XXX we need to know the type here
            io.format(Stream, "/* XXX init_array */ object[%d]%s\n",
                [i(Size), s(Suffix)], !IO)
        )
    ).

%---------------------%

output_initializer_body_for_csharp(Info, Stream, InitStart, Indent,
        Initializer, MaybeType, Suffix, !IO) :-
    (
        Initializer = no_initializer,
        unexpected($pred, "no_initializer")
    ;
        Initializer = init_obj(Rval),
        (
            InitStart = not_at_start_of_line
        ;
            InitStart = at_start_of_line,
            output_n_indents(Stream, Indent, !IO)
        ),
        output_rval_for_csharp(Info, Rval, Stream, !IO),
        io.format(Stream, "%s\n", [s(Suffix)], !IO)
    ;
        Initializer = init_struct(StructType, FieldInits),
        (
            InitStart = not_at_start_of_line,
            io.nl(Stream, !IO)
        ;
            InitStart = at_start_of_line
        ),
        output_n_indents(Stream, Indent, !IO),
        io.write_string(Stream, "new ", !IO),
        output_type_for_csharp(Info, Stream, StructType, ArrayDims, !IO),
        init_arg_wrappers_cs_java(ArrayDims, Start, End),
        (
            FieldInits = [],
            io.format(Stream, "%s%s%s", [s(Start), s(End), s(Suffix)], !IO)
        ;
            FieldInits = [HeadFieldInit | TailFieldInits],
            io.format(Stream, "%s\n", [s(Start)], !IO),
            output_initializer_body_list_for_csharp(Info, Stream, Indent + 1,
                HeadFieldInit, TailFieldInits, "", !IO),
            output_n_indents(Stream, Indent, !IO),
            io.format(Stream, "%s%s\n", [s(End), s(Suffix)], !IO)
        )
    ;
        Initializer = init_array(ElementInits),
        (
            InitStart = not_at_start_of_line,
            io.nl(Stream, !IO)
        ;
            InitStart = at_start_of_line
        ),
        output_n_indents(Stream, Indent, !IO),
        io.write_string(Stream, "new ", !IO),
        (
            MaybeType = yes(Type),
            output_type_for_csharp(Info, Stream, Type, !IO)
        ;
            MaybeType = no,
            % XXX We need to know the type here.
            io.write_string(Stream, "/* XXX init_array */ object[]", !IO)
        ),
        (
            ElementInits = [],
            io.format(Stream, " {}%s\n", [s(Suffix)], !IO)
        ;
            ElementInits = [HeadElementInit | TailElementInits],
            io.write_string(Stream, " {\n", !IO),
            output_initializer_body_list_for_csharp(Info, Stream, Indent + 1,
                HeadElementInit, TailElementInits, "", !IO),
            output_n_indents(Stream, Indent, !IO),
            io.format(Stream, "}%s\n", [s(Suffix)], !IO)
        )
    ).

%---------------------%

output_nonempty_initializer_body_list_for_csharp(Info, Stream, Indent, Inits,
        Suffix, !IO) :-
    list.det_head_tail(Inits, HeadInit, TailInits),
    output_initializer_body_list_for_csharp(Info, Stream, Indent,
        HeadInit, TailInits, Suffix, !IO).

:- pred output_initializer_body_list_for_csharp(csharp_out_info::in,
    io.text_output_stream::in, indent::in, mlds_initializer::in,
    list(mlds_initializer)::in, string::in, io::di, io::uo) is det.

output_initializer_body_list_for_csharp(Info, Stream, Indent,
        HeadInit, TailInits, Suffix, !IO) :-
    (
        TailInits = [],
        output_initializer_body_for_csharp(Info, Stream, at_start_of_line,
            Indent, HeadInit, no, Suffix, !IO)
    ;
        TailInits = [HeadTailInit | TailTailInits],
        output_initializer_body_for_csharp(Info, Stream, at_start_of_line,
            Indent, HeadInit, no, ",", !IO),
        output_initializer_body_list_for_csharp(Info, Stream,
            Indent, HeadTailInit, TailTailInits, Suffix, !IO)
    ).

%---------------------------------------------------------------------------%

    % We need to provide initializers for local variables to avoid problems
    % with undefined variables.
    %
:- func get_default_initializer_for_csharp(csharp_out_info, mlds_type)
    = string.

get_default_initializer_for_csharp(Info, Type) = Initializer :-
    (
        Type = mercury_nb_type(_, CtorCat),
        (
            CtorCat = ctor_cat_builtin(_),
            unexpected($pred, "mercury_nb_type but ctor_cat_builtin")
        ;
            ( CtorCat = ctor_cat_system(_)
            ; CtorCat = ctor_cat_higher_order
            ; CtorCat = ctor_cat_tuple
            ; CtorCat = ctor_cat_variable
            ; CtorCat = ctor_cat_void
            ),
            Initializer = "null"
        ;
            ( CtorCat = ctor_cat_enum(_)
            ; CtorCat = ctor_cat_user(_)
            ; CtorCat = ctor_cat_builtin_dummy
            ),
            TypeName = type_to_string_for_csharp(Info, Type),
            Initializer = "default(" ++ TypeName ++ ")"
        )
    ;
        ( Type = mlds_builtin_type_int(int_type_int)
        ; Type = mlds_builtin_type_int(int_type_int8)
        ; Type = mlds_builtin_type_int(int_type_int16)
        ; Type = mlds_builtin_type_int(int_type_int32)
        % C# byte and ushort literals don't have a suffix.
        ; Type = mlds_builtin_type_int(int_type_uint8)
        ; Type = mlds_builtin_type_int(int_type_uint16)
        ; Type = mlds_builtin_type_float
        ),
        Initializer = "0"
    ;
        ( Type = mlds_builtin_type_int(int_type_uint)
        ; Type = mlds_builtin_type_int(int_type_uint32)
        ),
        Initializer = "0U"
    ;
        Type = mlds_builtin_type_int(int_type_int64),
        Initializer = "0L"
    ;
        Type = mlds_builtin_type_int(int_type_uint64),
        Initializer = "0UL"
    ;
        Type = mlds_builtin_type_char,
        Initializer = "'\\u0000'"
    ;
        Type = mlds_builtin_type_string,
        Initializer = "null"
    ;
        Type = mlds_native_bool_type,
        Initializer = "false"
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
        Type = mlds_foreign_type(ForeignType),
        (
            ForeignType = csharp(csharp_type(CsharpType)),
            Initializer = "default(" ++ CsharpType ++ ")"
        ;
            ( ForeignType = c(_)
            ; ForeignType = java(_)
            ),
            unexpected($pred, "wrong foreign language type")
        )
    ;
        Type = mlds_unknown_type,
        unexpected($pred, "variable has unknown_type")
    ).

%---------------------------------------------------------------------------%
:- end_module ml_backend.mlds_to_cs_data.
%---------------------------------------------------------------------------%
