%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2018 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% Output MLDS lvals and rvals.
%
%---------------------------------------------------------------------------%

:- module ml_backend.mlds_to_c_data.
:- interface.

:- import_module ml_backend.mlds.
:- import_module ml_backend.mlds_to_c_util.

:- import_module io.

%---------------------------------------------------------------------------%

:- pred mlds_output_lval(mlds_to_c_opts::in, mlds_lval::in,
    io.text_output_stream::in, io::di, io::uo) is det.

:- pred mlds_output_rval(mlds_to_c_opts::in, mlds_rval::in,
    io.text_output_stream::in, io::di, io::uo) is det.

:- pred mlds_output_bracketed_rval(mlds_to_c_opts::in,
    io.text_output_stream::in, mlds_rval::in, io::di, io::uo) is det.

:- pred mlds_output_boxed_rval(mlds_to_c_opts::in, io.text_output_stream::in,
    mlds_type::in, mlds_rval::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- pred mlds_output_initializer(mlds_to_c_opts::in, io.text_output_stream::in,
    mlds_type::in, mlds_initializer::in, io::di, io::uo) is det.

:- pred mlds_output_initializer_body(mlds_to_c_opts::in,
    io.text_output_stream::in, int::in, mlds_initializer::in,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.builtin_ops.
:- import_module backend_libs.c_util.
:- import_module backend_libs.rtti.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.indent.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module ml_backend.ml_util.
:- import_module ml_backend.mlds_to_c_name.
:- import_module ml_backend.mlds_to_c_type.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_foreign.
:- import_module parse_tree.prog_type.

:- import_module bool.
:- import_module char.
:- import_module int.
:- import_module int16.
:- import_module int32.
:- import_module int8.
:- import_module list.
:- import_module maybe.
:- import_module require.
:- import_module string.
:- import_module term.
:- import_module uint.
:- import_module uint16.
:- import_module uint32.
:- import_module uint8.

%---------------------------------------------------------------------------%

mlds_output_lval(Opts, Lval, Stream, !IO) :-
    (
        Lval = ml_field(MaybePtag, PtrRval, PtrType, FieldId, FieldType),
        (
            FieldId = ml_field_offset(OffsetRval),
            ( if
                (
                    FieldType = mlds_generic_type
                ;
                    FieldType = mercury_nb_type(MercuryType, _),
                    MercuryType = type_variable(_, _)
                    % We could also accept other types that are the same size
                    % as MR_Box, such as builtin_type(builtin_type_int) and
                    % builtin_type(builtin_type_string).
                )
            then
                io.write_string(Stream, "(", !IO),
                (
                    MaybePtag = yes(ptag(PtagUInt8)),
                    io.format(Stream, "MR_hl_field(%u, ", [u8(PtagUInt8)], !IO)
                ;
                    MaybePtag = no,
                    io.write_string(Stream, "MR_hl_mask_field(", !IO),
                    io.write_string(Stream, "(MR_Word) ", !IO)
                ),
                mlds_output_rval(Opts, PtrRval, Stream, !IO),
                io.write_string(Stream, ", ", !IO),
                mlds_output_rval(Opts, OffsetRval, Stream, !IO),
                io.write_string(Stream, "))", !IO)
            else
                % The field type for ml_lval_field(_, _, ml_field_offset(_),
                % _, _) lvals must be something that maps to MR_Box.
                unexpected($pred, "unexpected field type")
            )
        ;
            FieldId = ml_field_named(QualFieldVarName, CtorType),
            QualFieldVarNameStr =
                qual_field_var_name_to_string_for_c(QualFieldVarName),
            ( if MaybePtag = yes(ptag(0u8)) then
                ( if PtrType = CtorType then
                    CastStr = ""
                else
                    CastStr = cast_to_prefix_string_for_c(Opts, CtorType)
                ),
                ( if PtrRval = ml_mem_addr(PtrAddrLval) then
                    io.format(Stream, "(%s", [s(CastStr)], !IO),
                    mlds_output_lval(Opts, PtrAddrLval, Stream, !IO),
                    io.format(Stream, ").%s", [s(QualFieldVarNameStr)], !IO)
                else
                    io.format(Stream, "(%s", [s(CastStr)], !IO),
                    mlds_output_bracketed_rval(Opts, Stream, PtrRval, !IO),
                    io.format(Stream, ")->%s", [s(QualFieldVarNameStr)], !IO)
                )
            else
                CastStr = cast_to_prefix_string_for_c(Opts, CtorType),
                (
                    MaybePtag = yes(ptag(PtagUInt8)),
                    io.format(Stream, "(%sMR_body(", [s(CastStr)], !IO),
                    mlds_output_rval(Opts, PtrRval, Stream, !IO),
                    io.format(Stream, ", %u))->%s",
                        [u8(PtagUInt8), s(QualFieldVarNameStr)], !IO)
                ;
                    MaybePtag = no,
                    io.format(Stream, "(%sMR_strip_tag(", [s(CastStr)], !IO),
                    mlds_output_rval(Opts, PtrRval, Stream, !IO),
                    io.format(Stream, "))->%s", [s(QualFieldVarNameStr)], !IO)
                )
            )
        )
    ;
        Lval = ml_mem_ref(Rval, _Type),
        io.write_string(Stream, "*", !IO),
        mlds_output_bracketed_rval(Opts, Stream, Rval, !IO)
    ;
        Lval = ml_target_global_var_ref(GlobalVarRef),
        io.write_string(Stream, global_var_ref_to_string(GlobalVarRef), !IO)
    ;
        Lval = ml_global_var(QualGlobalVarName, _VarType),
        QualGlobalVarName =
            qual_global_var_name(MLDS_ModuleName, GlobalVarName),
        QualGlobalVarNameStr = maybe_qual_global_var_name_to_string_for_c(
            MLDS_ModuleName, GlobalVarName),
        io.write_string(Stream, QualGlobalVarNameStr, !IO)
    ;
        Lval = ml_local_var(LocalVarName, _VarType),
        LocalVarNameStr = local_var_name_to_string_for_c(LocalVarName),
        io.write_string(Stream, LocalVarNameStr, !IO)
    ).

mlds_output_rval(Opts, Rval, Stream, !IO) :-
    (
        Rval = ml_lval(Lval),
        mlds_output_lval(Opts, Lval, Stream, !IO)
        % XXX Do we need the commented out code below?
        % if a field is used as an rval, then we need to use
        % the MR_hl_const_field() macro, not the MR_hl_field() macro,
        % to avoid warnings about discarding const,
        % and similarly for MR_mask_field.
        %   ( if Lval = ml_lval_field(MaybePtag, Rval, FieldNum, _, _) then
        %       (
        %           MaybePtag = yes(Ptag),
        %           io.write_string(Stream, "MR_hl_const_field(", !IO),
        %           mlds_output_ptag(Ptag, !IO),
        %           io.write_string(Stream, ", ", !IO)
        %       ;
        %           MaybePtag = no,
        %           io.write_string(Stream, "MR_hl_const_mask_field(", !IO)
        %       ),
        %       mlds_output_rval(Rval, !IO),
        %       io.write_string(Stream, ", ", !IO),
        %       mlds_output_rval(FieldNum, !IO),
        %       io.write_string(Stream, ")", !IO)
        %   else
        %       mlds_output_lval(Lval, !IO)
        %   ).
    ;
        Rval = ml_mkword(ptag(PtagUInt8), BaseRval),
        io.format(Stream, "MR_mkword(%u, ", [u8(PtagUInt8)], !IO),
        mlds_output_rval(Opts, BaseRval, Stream, !IO),
        io.write_string(Stream, ")", !IO)
    ;
        Rval = ml_const(Const),
        mlds_output_rval_const(Opts, Stream, Const, !IO)
    ;
        Rval = ml_cast(Type, SubRval),
        mlds_output_cast_rval(Opts, Stream, Type, SubRval, !IO)
    ;
        Rval = ml_box(Type, SubRval),
        mlds_output_boxed_rval(Opts, Stream, Type, SubRval, !IO)
    ;
        Rval = ml_unbox(Type, SubRval),
        mlds_output_unboxed_rval(Opts, Stream, Type, SubRval, !IO)
    ;
        Rval = ml_unop(Unop, SubRval),
        mlds_output_unop(Opts, Stream, Unop, SubRval, !IO)
    ;
        Rval = ml_binop(BinOp, RvalA, RvalB),
        mlds_output_binop(Opts, Stream, BinOp, RvalA, RvalB, !IO)
    ;
        Rval = ml_mem_addr(Lval),
        % XXX Are parentheses needed?
        io.write_string(Stream, "&", !IO),
        mlds_output_lval(Opts, Lval, Stream, !IO)
    ;
        (
            Rval = ml_scalar_common(ScalarCommon)
        ;
            Rval = ml_scalar_common_addr(ScalarCommon),
            io.write_string(Stream, "&", !IO)
        ),
        ScalarCommon = mlds_scalar_common(ModuleName, _Type,
            ml_scalar_common_type_num(TypeNum), RowNum),
        ModuleSymName = mlds_module_name_to_sym_name(ModuleName),
        MangledModuleName = sym_name_mangle(ModuleSymName),
        io.format(Stream, "%s_scalar_common_%d[%d]",
            [s(MangledModuleName), i(TypeNum), i(RowNum)], !IO)
    ;
        Rval = ml_vector_common_row_addr(VectorCommon, RowRval),
        VectorCommon = mlds_vector_common(ModuleName, _Type,
            ml_vector_common_type_num(TypeNum), StartRowNum, _NumRows),
        ModuleSymName = mlds_module_name_to_sym_name(ModuleName),
        MangledModuleName = sym_name_mangle(ModuleSymName),
        io.format(Stream, "&%s_vector_common_%d[%d + ",
            [s(MangledModuleName), i(TypeNum), i(StartRowNum)], !IO),
        mlds_output_rval(Opts, RowRval, Stream, !IO),
        io.write_string(Stream, "]", !IO)
    ;
        Rval = ml_self(_),
        io.write_string(Stream, "this", !IO)
    ).

mlds_output_bracketed_rval(Opts, Stream, Rval, !IO) :-
    ( if
        % If it is just a variable name, then we do not need parentheses.
        ( Rval = ml_lval(ml_local_var(_,_))
        ; Rval = ml_lval(ml_global_var(_,_))
        ; Rval = ml_const(mlconst_code_addr(_))
        )
    then
        mlds_output_rval(Opts, Rval, Stream, !IO)
    else
        io.write_char(Stream, '(', !IO),
        mlds_output_rval(Opts, Rval, Stream, !IO),
        io.write_char(Stream, ')', !IO)
    ).

:- pred mlds_output_cast_rval(mlds_to_c_opts::in, io.text_output_stream::in,
    mlds_type::in, mlds_rval::in, io::di, io::uo) is det.

mlds_output_cast_rval(Opts, Stream, Type, Rval, !IO) :-
    CastStr = cast_to_prefix_string_for_c(Opts, Type),
    % Cast the *whole* of Rval, not just an initial subrval.
    io.format(Stream, "%s(", [s(CastStr)], !IO),
    mlds_output_rval(Opts, Rval, Stream, !IO),
    io.write_char(Stream, ')', !IO).

%---------------------%

mlds_output_boxed_rval(Opts, Stream, Type, Rval, !IO) :-
    ( if
        Rval = ml_cast(OtherType, InnerRval),
        ( Type = OtherType
        ; is_an_address(InnerRval) = yes
        )
    then
        % Avoid unnecessary double-casting -- strip away the inner cast.
        % This is necessary for ANSI/ISO C conformance, to avoid casts
        % from pointers to integers in static initializers.
        mlds_output_boxed_rval(Opts, Stream, Type, InnerRval, !IO)
    else
        (
            Type = mlds_generic_type,
            mlds_output_boxed_rval_generic(Opts, Stream, Rval, !IO)
        ;
            Type = mlds_builtin_type_float,
            mlds_output_boxed_rval_float(Opts, Stream, Rval, !IO)
        ;
            ( Type = mlds_native_bool_type
            ; Type = mlds_builtin_type_char
            % ; Type = mlds_builtin_type_int(int_type_int)    % XXX ARG_PACK
            ),
            mlds_output_boxed_rval_smaller_than_word(Opts, Stream, Rval, !IO)
        ;
            ( Type = mlds_builtin_type_string
            ; Type = mlds_array_type(_)
            ; Type = mlds_mercury_array_type(_)
            ; Type = mlds_mostly_generic_array_type(_)
            ; Type = mlds_class_type(_)
            ; Type = mlds_enum_class_type(_)
            ; Type = mlds_env_type(_)
            ; Type = mlds_commit_type
            ; Type = mlds_cont_type(_)
            ; Type = mlds_foreign_type(_)
            ; Type = mlds_func_type(_)
            ; Type = mlds_generic_env_ptr_type
            ; Type = mlds_pseudo_type_info_type
            ; Type = mlds_ptr_type(_)
            ; Type = mlds_rtti_type(_)
            ; Type = mlds_tabling_type(_)
            ; Type = mlds_type_info_type
            ; Type = mlds_unknown_type
            ),
            mlds_output_boxed_rval_default(Opts, Stream, Rval, !IO)
        ;
            Type = mlds_builtin_type_int(IntType),
            (
                ( IntType = int_type_int
                ; IntType = int_type_uint
                ),
                mlds_output_boxed_rval_default(Opts, Stream, Rval, !IO)
            ;
                ( IntType = int_type_int8
                ; IntType = int_type_uint8
                ; IntType = int_type_int16
                ; IntType = int_type_uint16
                ; IntType = int_type_int32
                ; IntType = int_type_uint32
                ),
                mlds_output_boxed_rval_smaller_than_word(Opts, Stream,
                    Rval, !IO)
            ;
                IntType = int_type_int64,
                mlds_output_boxed_rval_int64(Opts, Stream, Rval, !IO)
            ;
                IntType = int_type_uint64,
                mlds_output_boxed_rval_uint64(Opts, Stream, Rval, !IO)
            )
        ;
            Type = mercury_nb_type(MercuryType, _),
            (
                MercuryType = builtin_type(_BuiltinType),
                unexpected($pred, "mercury_nb_type but builtin_type")
            ;
                MercuryType = type_variable(_, _),
                mlds_output_boxed_rval_generic(Opts, Stream, Rval, !IO)
            ;
                ( MercuryType = defined_type(_, _, _)
                ; MercuryType = tuple_type(_, _)
                ; MercuryType = higher_order_type(_, _, _, _, _)
                ; MercuryType = apply_n_type(_, _, _)
                ; MercuryType = kinded_type(_, _)
                ),
                mlds_output_boxed_rval_default(Opts, Stream, Rval, !IO)
            )
        )
    ).

:- pred mlds_output_boxed_rval_generic(mlds_to_c_opts::in,
    io.text_output_stream::in, mlds_rval::in, io::di, io::uo) is det.
:- pragma inline(pred(mlds_output_boxed_rval_generic/5)).

mlds_output_boxed_rval_generic(Opts, Stream, Rval, !IO) :-
    % Rval already has type MR_Box, so no cast is needed.
    mlds_output_rval(Opts, Rval, Stream, !IO).

:- pred mlds_output_boxed_rval_float(mlds_to_c_opts::in,
    io.text_output_stream::in, mlds_rval::in, io::di, io::uo) is det.
:- pragma inline(pred(mlds_output_boxed_rval_float/5)).

mlds_output_boxed_rval_float(Opts, Stream, Rval, !IO) :-
    io.write_string(Stream, "MR_box_float(", !IO),
    mlds_output_rval(Opts, Rval, Stream, !IO),
    io.write_string(Stream, ")", !IO).

:- pred mlds_output_boxed_rval_int64(mlds_to_c_opts::in,
    io.text_output_stream::in, mlds_rval::in, io::di, io::uo) is det.
:- pragma inline(pred(mlds_output_boxed_rval_int64/5)).

mlds_output_boxed_rval_int64(Opts, Stream, Rval, !IO) :-
    io.write_string(Stream, "MR_box_int64(", !IO),
    mlds_output_rval(Opts, Rval, Stream, !IO),
    io.write_string(Stream, ")", !IO).

:- pred mlds_output_boxed_rval_uint64(mlds_to_c_opts::in,
    io.text_output_stream::in, mlds_rval::in, io::di, io::uo) is det.
:- pragma inline(pred(mlds_output_boxed_rval_uint64/5)).

mlds_output_boxed_rval_uint64(Opts, Stream, Rval, !IO) :-
    io.write_string(Stream, "MR_box_uint64(", !IO),
    mlds_output_rval(Opts, Rval, Stream, !IO),
    io.write_string(Stream, ")", !IO).

:- pred mlds_output_boxed_rval_smaller_than_word(mlds_to_c_opts::in,
    io.text_output_stream::in, mlds_rval::in, io::di, io::uo) is det.
:- pragma inline(pred(mlds_output_boxed_rval_smaller_than_word/5)).

mlds_output_boxed_rval_smaller_than_word(Opts, Stream, Rval, !IO) :-
    % We cast first to MR_Word, and then to MR_Box.
    % We do this to avoid spurious warnings from gcc about
    % "cast from integer to pointer of different size".
    io.write_string(Stream, "((MR_Box) (MR_Word) (", !IO),
    mlds_output_rval(Opts, Rval, Stream, !IO),
    io.write_string(Stream, "))", !IO).

:- pred mlds_output_boxed_rval_default(mlds_to_c_opts::in,
    io.text_output_stream::in, mlds_rval::in, io::di, io::uo) is det.
:- pragma inline(pred(mlds_output_boxed_rval_default/5)).

mlds_output_boxed_rval_default(Opts, Stream, Rval, !IO) :-
    io.write_string(Stream, "((MR_Box) (", !IO),
    mlds_output_rval(Opts, Rval, Stream, !IO),
    io.write_string(Stream, "))", !IO).

    % Return `yes' if the specified rval is an address (possibly tagged and/or
    % cast to a different type).
    %
:- func is_an_address(mlds_rval) = bool.

is_an_address(Rval) = IsAddr :-
    (
        Rval = ml_mkword(_Ptag, SubRval),
        IsAddr = is_an_address(SubRval)
    ;
        Rval = ml_cast(_, SubRval),
        IsAddr = is_an_address(SubRval)
    ;
        ( Rval = ml_mem_addr(_)
        ; Rval = ml_scalar_common_addr(_)
        ; Rval = ml_vector_common_row_addr(_, _)
        ),
        IsAddr = yes
    ;
        ( Rval = ml_lval(_)
        ; Rval = ml_box(_, _)
        ; Rval = ml_unbox(_, _)
        ; Rval = ml_unop(_, _)
        ; Rval = ml_binop(_, _, _)
        ; Rval = ml_self(_)
        ; Rval = ml_scalar_common(_)
        ),
        IsAddr = no
    ;
        Rval = ml_const(Const),
        (
            ( Const = mlconst_null(_)
            ; Const = mlconst_code_addr(_)
            ; Const = mlconst_data_addr_local_var(_)
            ; Const = mlconst_data_addr_global_var(_, _)
            ; Const = mlconst_data_addr_rtti(_, _)
            ; Const = mlconst_data_addr_tabling(_, _)
            ),
            IsAddr = yes
        ;
            ( Const = mlconst_false
            ; Const = mlconst_true
            ; Const = mlconst_named_const(_, _)
            ; Const = mlconst_enum(_, _)
            ; Const = mlconst_char(_)
            ; Const = mlconst_string(_)
            ; Const = mlconst_multi_string(_)
            ; Const = mlconst_int(_)
            ; Const = mlconst_uint(_)
            ; Const = mlconst_int8(_)
            ; Const = mlconst_uint8(_)
            ; Const = mlconst_int16(_)
            ; Const = mlconst_uint16(_)
            ; Const = mlconst_int32(_)
            ; Const = mlconst_uint32(_)
            ; Const = mlconst_int64(_)
            ; Const = mlconst_uint64(_)
            ; Const = mlconst_float(_)
            ; Const = mlconst_foreign(_, _, _)
            ),
            IsAddr = no
        )
    ).

%---------------------%

:- pred mlds_output_unboxed_rval(mlds_to_c_opts::in, io.text_output_stream::in,
    mlds_type::in, mlds_rval::in, io::di, io::uo) is det.

mlds_output_unboxed_rval(Opts, Stream, Type, Rval, !IO) :-
    (
        Type = mlds_builtin_type_float,
        mlds_output_unboxed_rval_float(Opts, Stream, Rval, !IO)
    ;
        ( Type = mlds_native_bool_type
        ; Type = mlds_builtin_type_char
        % ; Type = mlds_builtin_type_int(int_type_int)    % XXX ARG_PACK
        ),
        mlds_output_unboxed_rval_smaller_than_word(Opts, Stream,
            Type, Rval, !IO)
    ;
        ( Type = mlds_builtin_type_string
        ; Type = mlds_array_type(_)
        ; Type = mlds_mercury_array_type(_)
        ; Type = mlds_mostly_generic_array_type(_)
        ; Type = mlds_class_type(_)
        ; Type = mlds_enum_class_type(_)
        ; Type = mlds_env_type(_)
        ; Type = mlds_commit_type
        ; Type = mlds_cont_type(_)
        ; Type = mlds_foreign_type(_)
        ; Type = mlds_func_type(_)
        ; Type = mlds_generic_type
        ; Type = mlds_generic_env_ptr_type
        ; Type = mlds_pseudo_type_info_type
        ; Type = mlds_ptr_type(_)
        ; Type = mlds_rtti_type(_)
        ; Type = mlds_tabling_type(_)
        ; Type = mlds_type_info_type
        ; Type = mlds_unknown_type
        ),
        mlds_output_unboxed_rval_default(Opts, Stream, Type, Rval, !IO)
    ;
        Type = mlds_builtin_type_int(IntType),
        (
            IntType = int_type_int64,
            mlds_output_unboxed_rval_int64(Opts, Stream, Rval, !IO)
        ;
            IntType = int_type_uint64,
            mlds_output_unboxed_rval_uint64(Opts, Stream, Rval, !IO)
        ;
            ( IntType = int_type_int
            ; IntType = int_type_uint
            ),
            mlds_output_unboxed_rval_default(Opts, Stream, Type, Rval, !IO)
        ;
            ( IntType = int_type_int8
            ; IntType = int_type_uint8
            ; IntType = int_type_int16
            ; IntType = int_type_uint16
            ; IntType = int_type_int32
            ; IntType = int_type_uint32
            ),
            % These integer types are all (potentially) smaller
            % than MR_Word.
            mlds_output_unboxed_rval_smaller_than_word(Opts, Stream,
                Type, Rval, !IO)
        )
    ;
        Type = mercury_nb_type(MercuryType, _),
        (
            MercuryType = builtin_type(_BuiltinType),
            unexpected($pred, "mercury_nb_type but builtin_type")
        ;
            ( MercuryType = type_variable(_, _)
            ; MercuryType = defined_type(_, _, _)
            ; MercuryType = tuple_type(_, _)
            ; MercuryType = higher_order_type(_, _, _, _, _)
            ; MercuryType = apply_n_type(_, _, _)
            ; MercuryType = kinded_type(_, _)
            ),
            mlds_output_unboxed_rval_default(Opts, Stream, Type, Rval, !IO)
        )
    ).

:- pred mlds_output_unboxed_rval_float(mlds_to_c_opts::in,
    io.text_output_stream::in, mlds_rval::in, io::di, io::uo) is det.
:- pragma inline(pred(mlds_output_unboxed_rval_float/5)).

mlds_output_unboxed_rval_float(Opts, Stream, Rval, !IO) :-
    io.write_string(Stream, "MR_unbox_float(", !IO),
    mlds_output_rval(Opts, Rval, Stream, !IO),
    io.write_string(Stream, ")", !IO).

:- pred mlds_output_unboxed_rval_int64(mlds_to_c_opts::in,
    io.text_output_stream::in, mlds_rval::in, io::di, io::uo) is det.
:- pragma inline(pred(mlds_output_unboxed_rval_int64/5)).

mlds_output_unboxed_rval_int64(Opts, Stream, Rval, !IO) :-
    io.write_string(Stream, "MR_unbox_int64(", !IO),
    mlds_output_rval(Opts, Rval, Stream, !IO),
    io.write_string(Stream, ")", !IO).

:- pred mlds_output_unboxed_rval_uint64(mlds_to_c_opts::in,
    io.text_output_stream::in, mlds_rval::in, io::di, io::uo) is det.
:- pragma inline(pred(mlds_output_unboxed_rval_uint64/5)).

mlds_output_unboxed_rval_uint64(Opts, Stream, Rval, !IO) :-
    io.write_string(Stream, "MR_unbox_uint64(", !IO),
    mlds_output_rval(Opts, Rval, Stream, !IO),
    io.write_string(Stream, ")", !IO).

:- pred mlds_output_unboxed_rval_smaller_than_word(mlds_to_c_opts::in,
    io.text_output_stream::in, mlds_type::in, mlds_rval::in,
    io::di, io::uo) is det.
:- pragma inline(pred(mlds_output_unboxed_rval_smaller_than_word/6)).

mlds_output_unboxed_rval_smaller_than_word(Opts, Stream, Type, Rval, !IO) :-
    % We cast first to MR_Word, and then to the desired type.
    % This is done to avoid spurious warnings from gcc about
    % "cast from pointer to integer of different size".
    % XXX ARG_PACK We call this predicate both too much and not enough.
    % - We call it for builtin types that *may* be smaller than a word
    %   on some platforms, whether or not it is smaller than a word
    %   on *this* platform.
    % - We do *not* call it values of dummy and enum types, which *are*
    %   smaller than a word.
    CastStr = cast_to_prefix_string_for_c(Opts, Type),
    io.format(Stream, "(%s", [s(CastStr)], !IO),
    io.write_string(Stream, "(MR_Word) ", !IO),
    mlds_output_rval(Opts, Rval, Stream, !IO),
    io.write_string(Stream, ")", !IO).

:- pred mlds_output_unboxed_rval_default(mlds_to_c_opts::in,
    io.text_output_stream::in, mlds_type::in, mlds_rval::in,
    io::di, io::uo) is det.
:- pragma inline(pred(mlds_output_unboxed_rval_default/6)).

mlds_output_unboxed_rval_default(Opts, Stream, Type, Rval, !IO) :-
    % XXX This does the same job as mlds_output_cast_rval, except for
    % writing out an outer pair of parentheses.
    CastStr = cast_to_prefix_string_for_c(Opts, Type),
    io.format(Stream, "(%s(", [s(CastStr)], !IO),
    mlds_output_rval(Opts, Rval, Stream, !IO),
    io.write_string(Stream, "))", !IO).

%---------------------%

:- pred mlds_output_unop(mlds_to_c_opts::in, io.text_output_stream::in,
    unary_op::in, mlds_rval::in, io::di, io::uo) is det.

mlds_output_unop(Opts, Stream, UnaryOp, Expr, !IO) :-
    c_util.unary_prefix_op(UnaryOp, UnaryOpString),
    io.format(Stream, "%s(", [s(UnaryOpString)], !IO),
    ( if UnaryOp = tag then
        % The MR_tag macro requires its argument to be of type `MR_Word'.
        % XXX Should we put this cast inside the definition of MR_tag?
        io.write_string(Stream, "(MR_Word) ", !IO)
    else
        true
    ),
    mlds_output_rval(Opts, Expr, Stream, !IO),
    io.write_string(Stream, ")", !IO).

:- pred mlds_output_binop(mlds_to_c_opts::in, io.text_output_stream::in,
    binary_op::in, mlds_rval::in, mlds_rval::in, io::di, io::uo) is det.

mlds_output_binop(Opts, Stream, Op, X, Y, !IO) :-
    (
        Op = array_index(_),
        mlds_output_bracketed_rval(Opts, Stream, X, !IO),
        io.write_string(Stream, "[", !IO),
        mlds_output_rval(Opts, Y, Stream, !IO),
        io.write_string(Stream, "]", !IO)
    ;
        Op = string_unsafe_index_code_unit,
        io.write_string(Stream, "MR_nth_code_unit(", !IO),
        mlds_output_bracketed_rval(Opts, Stream, X, !IO),
        io.write_string(Stream, ", ", !IO),
        ( if Y = ml_const(mlconst_int(YN)) then
            io.write_int(Stream, YN, !IO)
        else
            mlds_output_rval(Opts, Y, Stream, !IO)
        ),
        io.write_string(Stream, ")", !IO)
    ;
        ( Op = compound_lt
        ; Op = compound_eq
        ),
        % These operators were intended to be generated only when using
        % the now-deleted Erlang backend.
        unexpected($pred, "compound_compare_binop")
    ;
        Op = pointer_equal_conservative,
        io.write_string(Stream, "(((MR_Word) ", !IO),
        mlds_output_rval(Opts, X, Stream, !IO),
        io.write_string(Stream, ") == ((MR_Word) ", !IO),
        mlds_output_rval(Opts, Y, Stream, !IO),
        io.write_string(Stream, "))", !IO)
    ;
        ( Op = str_eq, OpStr = "=="
        ; Op = str_ne, OpStr = "!="
        ; Op = str_le, OpStr = "<="
        ; Op = str_ge, OpStr = ">="
        ; Op = str_lt, OpStr = "<"
        ; Op = str_gt, OpStr = ">"
        ),
        io.write_string(Stream, "(strcmp(", !IO),
        mlds_output_rval(Opts, X, Stream, !IO),
        io.write_string(Stream, ", ", !IO),
        mlds_output_rval(Opts, Y, Stream, !IO),
        io.format(Stream, ") %s 0)", [s(OpStr)], !IO)
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
        mlds_output_bracketed_rval(Opts, Stream, X, !IO),
        io.format(Stream, " %s ", [s(OpStr)], !IO),
        mlds_output_bracketed_rval(Opts, Stream, Y, !IO),
        io.write_string(Stream, ")", !IO)
    ;
        ( Op = unsigned_lt, OpStr = "<"
        ; Op = unsigned_le, OpStr = "<="
        ),
        io.write_string(Stream, "(((MR_Unsigned) ", !IO),
        mlds_output_rval_as_unsigned_op_arg(Opts, Stream, 2147483647, X, !IO),
        io.format(Stream, ") %s ((MR_Unsigned) ", [s(OpStr)], !IO),
        mlds_output_rval_as_unsigned_op_arg(Opts, Stream, 2147483647, Y, !IO),
        io.write_string(Stream, "))", !IO)
    ;
        ( Op = int_add(IntType), OpStr = "+"
        ; Op = int_sub(IntType), OpStr = "-"
        ; Op = int_mul(IntType), OpStr = "*"
        ),
        (
            % Max is the maximum signed integer of the given size
            % that can be converted to the target unsigned type
            % on all platforms.
            (
                IntType = int_type_int,
                SignedType = "MR_Integer",
                UnsignedType = "MR_Unsigned",
                Max = 2147483647
            ;
                IntType = int_type_int8,
                SignedType = "int8_t",
                UnsignedType = "uint8_t",
                Max = 127
            ;
                IntType = int_type_int16,
                SignedType = "int16_t",
                UnsignedType = "uint16_t",
                Max = 32767
            ;
                IntType = int_type_int32,
                SignedType = "int32_t",
                UnsignedType = "uint32_t",
                Max = 2147483647
            ;
                IntType = int_type_int64,
                SignedType = "int64_t",
                UnsignedType = "uint64_t",
                Max = 2147483647            % for 32 bit platforms
            ),
            io.format(Stream, "(%s) ((%s) ",
                [s(SignedType), s(UnsignedType)], !IO),
            mlds_output_rval_as_unsigned_op_arg(Opts, Stream, Max, X, !IO),
            io.format(Stream, " %s (%s) ", [s(OpStr), s(UnsignedType)], !IO),
            mlds_output_rval_as_unsigned_op_arg(Opts, Stream, Max, Y, !IO),
            io.write_string(Stream, ")", !IO)
        ;
            ( IntType = int_type_uint
            ; IntType = int_type_uint8
            ; IntType = int_type_uint16
            ; IntType = int_type_uint32
            ; IntType = int_type_uint64
            ),
            % We could treat X + (-const) specially, but we do not.
            % The reason is documented in the equivalent code in
            % llds_out_data.m.
            io.write_string(Stream, "(", !IO),
            mlds_output_rval_as_op_arg(Opts, Stream, X, !IO),
            io.format(Stream, " %s ", [s(OpStr)], !IO),
            mlds_output_rval_as_op_arg(Opts, Stream, Y, !IO),
            io.write_string(Stream, ")", !IO)
        )
    ;
        ( Op = int_div(_), OpStr = "/"
        ; Op = int_mod(_), OpStr = "%"
        ; Op = eq(_), OpStr = "=="
        ; Op = ne(_), OpStr = "!="
        ; Op = int_lt(_), OpStr = "<"
        ; Op = int_gt(_), OpStr = ">"
        ; Op = int_le(_), OpStr = "<="
        ; Op = int_ge(_), OpStr = ">="
        ; Op = bitwise_and(_), OpStr = "&"
        ; Op = bitwise_or(_), OpStr = "|"
        ; Op = bitwise_xor(_), OpStr = "^"
        ; Op = logical_and, OpStr = "&&"
        ; Op = logical_or, OpStr = "||"
        ),
        % We could treat X + (-const) specially, but we do not.
        % The reason is documented in the equivalent code in llds_out_data.m.
        io.write_string(Stream, "(", !IO),
        mlds_output_rval_as_op_arg(Opts, Stream, X, !IO),
        io.format(Stream, " %s ", [s(OpStr)], !IO),
        mlds_output_rval_as_op_arg(Opts, Stream, Y, !IO),
        io.write_string(Stream, ")", !IO)
    ;
        ( Op = unchecked_left_shift(_, ShiftType), OpStr = "<<"
        ; Op = unchecked_right_shift(_, ShiftType), OpStr = ">>"
        ),
        io.write_string(Stream, "(", !IO),
        mlds_output_rval_as_op_arg(Opts, Stream, X, !IO),
        io.format(Stream, " %s ", [s(OpStr)], !IO),
        % Avoid clutter in the usual case that the shift amount
        % is a small constant.
        ( if Y = ml_const(mlconst_int(YInt)) then
            io.write_int(Stream, YInt, !IO)
        else if Y = ml_const(mlconst_uint(YUInt)) then
            io.write_uint(Stream, YUInt, !IO)
        else
            (
                ShiftType = shift_by_int
            ;
                ShiftType = shift_by_uint,
                io.write_string(Stream, "(int) ", !IO)
            ),
            mlds_output_rval_as_op_arg(Opts, Stream, Y, !IO)
        ),
        io.write_string(Stream, ")", !IO)
    ;
        Op = str_cmp,
        io.write_string(Stream, "MR_strcmp(", !IO),
        mlds_output_rval_as_op_arg(Opts, Stream, X, !IO),
        io.write_string(Stream, ", ", !IO),
        mlds_output_rval_as_op_arg(Opts, Stream, Y, !IO),
        io.write_string(Stream, ")", !IO)
    ;
        Op = offset_str_eq(N),
        io.format(Stream, "MR_offset_streq(%d, ", [i(N)], !IO),
        mlds_output_rval_as_op_arg(Opts, Stream, X, !IO),
        io.write_string(Stream, ", ", !IO),
        mlds_output_rval_as_op_arg(Opts, Stream, Y, !IO),
        io.write_string(Stream, ")", !IO)
    ;
        Op = body,
        io.write_string(Stream, "MR_body(", !IO),
        mlds_output_rval_as_op_arg(Opts, Stream, X, !IO),
        io.write_string(Stream, ", ", !IO),
        mlds_output_rval_as_op_arg(Opts, Stream, Y, !IO),
        io.write_string(Stream, ")", !IO)
    ;
        ( Op = float_from_dword,  OpStr = "MR_float_from_dword"
        ; Op = int64_from_dword,  OpStr = "MR_int64_from_dword"
        ; Op = uint64_from_dword, OpStr = "MR_uint64_from_dword"
        ),
        ( if is_aligned_dword_field(X, Y, PtrRval) then
            % gcc produces faster code in this case.
            io.format(Stream, "%s_ptr(MR_dword_ptr(", [s(OpStr)], !IO),
            mlds_output_rval(Opts, PtrRval, Stream, !IO),
            io.write_string(Stream, "))", !IO)
        else
            io.format(Stream, "%s(", [s(OpStr)], !IO),
            mlds_output_rval_as_op_arg(Opts, Stream, X, !IO),
            io.write_string(Stream, ", ", !IO),
            mlds_output_rval_as_op_arg(Opts, Stream, Y, !IO),
            io.write_string(Stream, ")", !IO)
        )
    ).

    % mlds_output_rval_as_unsigned_op_arg(Opts, Stream, Max, Rval, !IO):
    %
    % If Rval is a signed or unsigned integer constant in [0 .. Max],
    % the write out its value without casting it; it will be cast
    % to the right unsigned type by our caller. Otherwise, write it out
    % with whatever intermediate casts our general rules require for it.
    % These intermediate casts will usually be redundant in a context
    % in which the Rval is immediately cast to a different type.
    % The avoidance of these redundant casts when possible is the
    % purpose of this predicate.
    %
:- pred mlds_output_rval_as_unsigned_op_arg(mlds_to_c_opts::in,
    io.text_output_stream::in, int::in, mlds_rval::in, io::di, io::uo) is det.

mlds_output_rval_as_unsigned_op_arg(Opts, Stream, Max, Rval, !IO) :-
    ( if Rval = ml_const(Const) then
        ( if
            % The argument of an mlconst_int64 may not fit inside an int.
            (
                Const = mlconst_int(Int),
                Int >= 0
            ;
                Const = mlconst_int8(Int8),
                Int8 >= 0i8,
                Int = int8.cast_to_int(Int8)
            ;
                Const = mlconst_int16(Int16),
                Int16 >= 0i16,
                Int = int16.cast_to_int(Int16)
            ;
                Const = mlconst_int32(Int32),
                Int32 >= 0i32,
                Int = int32.cast_to_int(Int32)
            ),
            Int =< Max
        then
            io.write_int(Stream, Int, !IO)
        else if
            % The argument of an mlconst_uint64 may not fit in an uint.
            % We do the tests separately from the ints because a uint
            % may not fit in an int either.
            (
                Const = mlconst_uint(Uint)
            ;
                Const = mlconst_uint8(Uint8),
                Uint = uint8.cast_to_uint(Uint8)
            ;
                Const = mlconst_uint16(Uint16),
                Uint = uint16.cast_to_uint(Uint16)
            ;
                Const = mlconst_uint32(Uint32),
                Uint = uint32.cast_to_uint(Uint32)
            ),
            Uint =< uint.cast_from_int(Max)
        then
            io.write_uint(Stream, Uint, !IO)
        else
            mlds_output_rval_as_op_arg(Opts, Stream, Rval, !IO)
        )
    else
        mlds_output_rval_as_op_arg(Opts, Stream, Rval, !IO)
    ).

:- pred mlds_output_rval_as_op_arg(mlds_to_c_opts::in,
    io.text_output_stream::in, mlds_rval::in, io::di, io::uo) is det.

mlds_output_rval_as_op_arg(Opts, Stream, Rval, !IO) :-
    ( if
        ( Rval = ml_unop(_, _)
        ; Rval = ml_binop(_, _, _)
        )
    then
        io.write_string(Stream, "(", !IO),
        mlds_output_rval(Opts, Rval, Stream, !IO),
        io.write_string(Stream, ")", !IO)
    else
        mlds_output_rval(Opts, Rval, Stream, !IO)
    ).

%---------------------%

:- pred mlds_output_rval_const(mlds_to_c_opts::in, io.text_output_stream::in,
    mlds_rval_const::in, io::di, io::uo) is det.

mlds_output_rval_const(_Opts, Stream, Const, !IO) :-
    (
        Const = mlconst_true,
        io.write_string(Stream, "MR_TRUE", !IO)
    ;
        Const = mlconst_false,
        io.write_string(Stream, "MR_FALSE", !IO)
    ;
        ( Const = mlconst_int(N)
        ; Const = mlconst_enum(N, _)
        ),
        c_util.output_int_expr(Stream, N, !IO)
    ;
        Const = mlconst_uint(U),
        c_util.output_uint_expr(Stream, U, !IO)
    ;
        Const = mlconst_int8(N),
        c_util.output_int8_expr(Stream, N, !IO)
    ;
        Const = mlconst_uint8(N),
        c_util.output_uint8_expr(Stream, N, !IO)
    ;
        Const = mlconst_int16(N),
        c_util.output_int16_expr(Stream, N, !IO)
    ;
        Const = mlconst_uint16(N),
        c_util.output_uint16_expr(Stream, N, !IO)
    ;
        Const = mlconst_int32(N),
        c_util.output_int32_expr(Stream, N, !IO)
    ;
        Const = mlconst_uint32(N),
        c_util.output_uint32_expr(Stream, N, !IO)
    ;
        Const = mlconst_int64(N),
        c_util.output_int64_expr(Stream, N, !IO)
    ;
        Const = mlconst_uint64(N),
        c_util.output_uint64_expr(Stream, N, !IO)
    ;
        Const = mlconst_char(C),
        io.format(Stream, "(MR_Char) %d", [i(C)], !IO)
    ;
        Const = mlconst_foreign(Lang, Value, _Type),
        expect(unify(Lang, lang_c), $pred,
            "mlconst_foreign for language other than C"),
        io.format(Stream, "((int) %s)", [s(Value)], !IO)
    ;
        Const = mlconst_float(FloatVal),
        % The cast to (MR_Float) here lets the C compiler do arithmetic in
        % `float' rather than `double' if `MR_Float' is `float' not `double'.
        io.write_string(Stream, "(MR_Float) ", !IO),
        c_util.output_float_literal(Stream, FloatVal, !IO)
    ;
        Const = mlconst_string(String),
        % The cast avoids the following gcc warning
        % "assignment discards qualifiers from pointer target type".
        io.write_string(Stream, "(MR_String) ", !IO),
        output_quoted_string_c(Stream, String, !IO)
    ;
        Const = mlconst_multi_string(String),
        output_quoted_multi_string_c(Stream, String, !IO)
    ;
        Const = mlconst_named_const(_TargetPrefixes, NamedConst),
        % The target prefix for C is implicitly always empty.
        io.write_string(Stream, NamedConst, !IO)
    ;
        Const = mlconst_code_addr(CodeAddr),
        mlds_output_code_addr(Stream, CodeAddr, !IO)
    ;
        Const = mlconst_data_addr_local_var(LocalVarName),
        MangledLocalVarName =
            name_mangle(ml_local_var_name_to_string(LocalVarName)),
        io.format(Stream, "&%s", [s(MangledLocalVarName)], !IO)
    ;
        (
            Const =
                mlconst_data_addr_global_var(MLDS_ModuleName, GlobalVarName),
            IsArray = not_array
        ;
            Const = mlconst_data_addr_rtti(MLDS_ModuleName, RttiId),
            GlobalVarName = gvn_rtti_var(RttiId),
            IsArray = rtti_id_has_array_type(RttiId)
        ;
            Const = mlconst_data_addr_tabling(QualProcLabel, TablingId),
            QualProcLabel = qual_proc_label(MLDS_ModuleName, ProcLabel),
            GlobalVarName = gvn_tabling_var(ProcLabel, TablingId),
            IsArray = tabling_id_has_array_type(TablingId)
        ),
        % If it is an array type, then we just use the name;
        % otherwise, we must prefix the name with `&'.
        QualGlobalVarNameStr = maybe_qual_global_var_name_to_string_for_c(
            MLDS_ModuleName, GlobalVarName),
        (
            IsArray = is_array,
            io.format(Stream, "%s", [s(QualGlobalVarNameStr)], !IO)
        ;
            IsArray = not_array,
            io.format(Stream, "&%s", [s(QualGlobalVarNameStr)], !IO)
        )
    ;
        Const = mlconst_null(MLDS_Type),
        ( if MLDS_Type = mlds_builtin_type_float then
            io.write_string(Stream, "0.0", !IO)
        else
            io.write_string(Stream, "NULL", !IO)
        )
    ).

:- pred is_aligned_dword_field(mlds_rval::in, mlds_rval::in, mlds_rval::out)
    is semidet.

is_aligned_dword_field(RvalX, RvalY, ml_mem_addr(LvalX)) :-
    RvalX = ml_lval(LvalX),
    RvalY = ml_lval(LvalY),
    LvalX = ml_field(Ptag, PtrRval, PtrType, FieldIdX, FieldType),
    LvalY = ml_field(Ptag, PtrRval, PtrType, FieldIdY, FieldType),
    FieldIdX = ml_field_offset(ml_const(mlconst_int(Offset))),
    FieldIdY = ml_field_offset(ml_const(mlconst_int(Offset + 1))),
    int.even(Offset).

:- pred mlds_output_code_addr(io.text_output_stream::in, mlds_code_addr::in,
    io::di, io::uo) is det.

mlds_output_code_addr(Stream, CodeAddr, !IO) :-
    CodeAddr = mlds_code_addr(QualFuncLabel, _Signature),
    QualFuncLabel = qual_func_label(ModuleName, FuncLabel),
    FuncLabel = mlds_func_label(ProcLabel, MaybeAux),
    QualProcLabel = qual_proc_label(ModuleName, ProcLabel),
    QualProcLabelStr = qual_proc_label_to_string_for_c(QualProcLabel),
    MaybeAuxSuffix = mlds_maybe_aux_func_id_to_suffix(MaybeAux),
    io.format(Stream, "%s%s", [s(QualProcLabelStr), s(MaybeAuxSuffix)], !IO).

%---------------------------------------------------------------------------%

mlds_output_initializer(Opts, Stream, _Type, Initializer, !IO) :-
    NeedsInit = mlds_needs_initialization(Initializer),
    (
        NeedsInit = yes,
        io.write_string(Stream, " = ", !IO),
        mlds_output_initializer_body(Opts, Stream, 0, Initializer, !IO)
    ;
        NeedsInit = no
    ).

:- func mlds_needs_initialization(mlds_initializer) = bool.

mlds_needs_initialization(no_initializer) = no.
mlds_needs_initialization(init_obj(_)) = yes.
mlds_needs_initialization(init_struct(_Type, [])) = no.
mlds_needs_initialization(init_struct(_Type, [_|_])) = yes.
mlds_needs_initialization(init_array(_)) = yes.

mlds_output_initializer_body(Opts, Stream, Indent, Initializer, !IO) :-
    (
        Initializer = no_initializer
    ;
        Initializer = init_obj(Rval),
        write_indent2(Stream, Indent, !IO),
        mlds_output_rval(Opts, Rval, Stream, !IO)
    ;
        ( Initializer = init_struct(_Type, Inits)
        ; Initializer = init_array(Inits)
        ),
        IndentStr = indent2_string(Indent),
        (
            Inits = [],
            (
                Initializer = init_struct(_, _),
                % Note that standard ANSI/ISO C does not allow empty structs,
                % and it is the responsibility of the MLDS code generator
                % to not generate any such structs.
                unexpected($pred, "field initializers = []")
            ;
                Initializer = init_array(_),
                % Standard ANSI/ISO C does not allow empty arrays, but
                % the MLDS does. To keep the C compiler happy, we therefore
                % convert zero-element MLDS arrays into one-element C arrays.
                % (The extra element is a minor waste of space, but it will
                % otherwise be ignored.) So if the initializer list here
                % is empty, we need to output a single initializer.
                % We can initialize the extra element with any value.
                % We use "0", since that is a valid initializer for any type.
                io.format(Stream, "%s{ 0 }\n", [s(IndentStr)], !IO)
            )
        ;
            Inits = [HeadInit | TailInits],
            (
                TailInits = [],
                % We write the single init on a single line, if we can.
                (
                    ( HeadInit = no_initializer
                    ; HeadInit = init_obj(_)
                    ),
                    % We can.
                    io.format(Stream, "%s{ ", [s(IndentStr)], !IO),
                    mlds_output_initializer_body(Opts, Stream, 0,
                        HeadInit, !IO),
                    io.write_string(Stream, " }", !IO)
                ;
                    ( HeadInit = init_struct(_, _)
                    ; HeadInit = init_array(_)
                    ),
                    % We probably can't: printing HeadInit by itself may need
                    % more than one line.
                    io.format(Stream, "%s{\n", [s(IndentStr)], !IO),
                    mlds_output_initializer_body(Opts, Stream, Indent + 1,
                        HeadInit, !IO),
                    io.format(Stream, "\n%s}", [s(IndentStr)], !IO)
                )
            ;
                TailInits = [_ | _],
                % We write the N inits on N+2 lines.
                io.format(Stream, "%s{\n", [s(IndentStr)], !IO),
                mlds_output_initializer_bodies(Opts, Stream, Indent + 1,
                    HeadInit, TailInits, !IO),
                io.format(Stream, "%s}", [s(IndentStr)], !IO)
            )
        )
    ).

:- pred mlds_output_initializer_bodies(mlds_to_c_opts::in,
    io.text_output_stream::in, int::in,
    mlds_initializer::in, list(mlds_initializer)::in, io::di, io::uo) is det.

mlds_output_initializer_bodies(Opts, Stream, Indent,
        HeadInit, TailInits, !IO) :-
    mlds_output_initializer_body(Opts, Stream, Indent, HeadInit, !IO),
    (
        TailInits = [],
        io.write_string(Stream, "\n", !IO)
    ;
        TailInits = [HeadTailInit | TailTailInits],
        io.write_string(Stream, ",\n", !IO),
        mlds_output_initializer_bodies(Opts, Stream, Indent,
            HeadTailInit, TailTailInits, !IO)
    ).

%---------------------------------------------------------------------------%
:- end_module ml_backend.mlds_to_c_data.
%---------------------------------------------------------------------------%
