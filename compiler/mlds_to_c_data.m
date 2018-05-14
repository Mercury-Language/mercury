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

:- import_module hlds.
:- import_module hlds.hlds_data.
:- import_module ml_backend.mlds.
:- import_module ml_backend.mlds_to_c_util.

:- import_module io.

%---------------------------------------------------------------------------%

:- pred mlds_output_ptag(ptag::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- pred mlds_output_lval(mlds_to_c_opts::in, mlds_lval::in,
    io::di, io::uo) is det.

:- pred mlds_output_rval(mlds_to_c_opts::in, mlds_rval::in,
    io::di, io::uo) is det.

:- pred mlds_output_bracketed_rval(mlds_to_c_opts::in, mlds_rval::in,
    io::di, io::uo) is det.

:- pred mlds_output_boxed_rval(mlds_to_c_opts::in,
    mlds_type::in, mlds_rval::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- pred mlds_output_initializer(mlds_to_c_opts::in, mlds_type::in,
    mlds_initializer::in, io::di, io::uo) is det.

:- pred mlds_output_initializer_body(mlds_to_c_opts::in, int::in,
    mlds_initializer::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.builtin_ops.
:- import_module backend_libs.c_util.
:- import_module backend_libs.foreign.
:- import_module backend_libs.rtti.
:- import_module libs.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module ml_backend.ml_util.
:- import_module ml_backend.mlds_to_c_name.
:- import_module ml_backend.mlds_to_c_type.
:- import_module ml_backend.mlds_to_target_util.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_foreign.
:- import_module parse_tree.prog_type.

:- import_module bool.
:- import_module char.
:- import_module float.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module require.
:- import_module string.
:- import_module term.

%---------------------------------------------------------------------------%

mlds_output_ptag(Ptag, !IO) :-
    io.write_string("MR_mktag(", !IO),
    io.write_int(Ptag, !IO),
    io.write_string(")", !IO).

%---------------------------------------------------------------------------%

mlds_output_lval(Opts, Lval, !IO) :-
    (
        Lval = ml_field(MaybePtag, PtrRval, FieldId, FieldType, PtrType),
        (
            FieldId = ml_field_offset(OffsetRval),
            ( if
                (
                    FieldType = mlds_generic_type
                ;
                    FieldType = mercury_type(MercuryType, _, _),
                    MercuryType = type_variable(_, _)
                    % We could also accept other types that are the same size
                    % as MR_Box, such as builtin_type(builtin_type_int) and
                    % builtin_type(builtin_type_string).
                )
            then
                io.write_string("(", !IO),
                (
                    MaybePtag = yes(Ptag),
                    io.write_string("MR_hl_field(", !IO),
                    mlds_output_ptag(Ptag, !IO),
                    io.write_string(", ", !IO)
                ;
                    MaybePtag = no,
                    io.write_string("MR_hl_mask_field(", !IO),
                    io.write_string("(MR_Word) ", !IO)
                ),
                mlds_output_rval(Opts, PtrRval, !IO),
                io.write_string(", ", !IO),
                mlds_output_rval(Opts, OffsetRval, !IO),
                io.write_string("))", !IO)
            else
                % The field type for ml_lval_field(_, _, ml_field_offset(_),
                % _, _) lvals must be something that maps to MR_Box.
                unexpected($pred, "unexpected field type")
            )
        ;
            FieldId = ml_field_named(QualFieldVarName, CtorType),
            io.write_string("(", !IO),
            ( if MaybePtag = yes(0) then
                ( if PtrType = CtorType then
                    true
                else
                    mlds_output_cast(Opts, CtorType, !IO)
                ),
                ( if PtrRval = ml_mem_addr(PtrAddrLval) then
                    mlds_output_lval(Opts, PtrAddrLval, !IO),
                    io.write_string(").", !IO)
                else
                    mlds_output_bracketed_rval(Opts, PtrRval, !IO),
                    io.write_string(")->", !IO)
                )
            else
                mlds_output_cast(Opts, CtorType, !IO),
                (
                    MaybePtag = yes(Ptag),
                    io.write_string("MR_body(", !IO),
                    mlds_output_rval(Opts, PtrRval, !IO),
                    io.write_string(", ", !IO),
                    mlds_output_ptag(Ptag, !IO)
                ;
                    MaybePtag = no,
                    io.write_string("MR_strip_tag(", !IO),
                    mlds_output_rval(Opts, PtrRval, !IO)
                ),
                io.write_string("))->", !IO)
            ),
            mlds_output_fully_qualified_field_var_name(QualFieldVarName, !IO)
        )
    ;
        Lval = ml_mem_ref(Rval, _Type),
        io.write_string("*", !IO),
        mlds_output_bracketed_rval(Opts, Rval, !IO)
    ;
        Lval = ml_target_global_var_ref(GlobalVar),
        io.write_string(global_var_name(GlobalVar), !IO)
    ;
        Lval = ml_global_var(QualGlobalVarName, _VarType),
        QualGlobalVarName =
            qual_global_var_name(MLDS_ModuleName, GlobalVarName),
        mlds_output_maybe_qualified_global_var_name(MLDS_ModuleName,
            GlobalVarName, !IO)
    ;
        Lval = ml_local_var(LocalVarName, _VarType),
        mlds_output_local_var_name(LocalVarName, !IO)
    ).

mlds_output_rval(Opts, Rval, !IO) :-
    (
        Rval = ml_lval(Lval),
        mlds_output_lval(Opts, Lval, !IO)
        % XXX Do we need the commented out code below?
        % if a field is used as an rval, then we need to use
        % the MR_hl_const_field() macro, not the MR_hl_field() macro,
        % to avoid warnings about discarding const,
        % and similarly for MR_mask_field.
        %   ( if Lval = ml_lval_field(MaybePtag, Rval, FieldNum, _, _) then
        %       (
        %           MaybePtag = yes(Ptag),
        %           io.write_string("MR_hl_const_field(", !IO),
        %           mlds_output_ptag(Ptag, !IO),
        %           io.write_string(", ", !IO)
        %       ;
        %           MaybePtag = no,
        %           io.write_string("MR_hl_const_mask_field(", !IO)
        %       ),
        %       mlds_output_rval(Rval, !IO),
        %       io.write_string(", ", !IO),
        %       mlds_output_rval(FieldNum, !IO),
        %       io.write_string(")", !IO)
        %   else
        %       mlds_output_lval(Lval, !IO)
        %   ).
    ;
        Rval = ml_mkword(Ptag, BaseRval),
        io.write_string("MR_mkword(", !IO),
        mlds_output_ptag(Ptag, !IO),
        io.write_string(", ", !IO),
        mlds_output_rval(Opts, BaseRval, !IO),
        io.write_string(")", !IO)
    ;
        Rval = ml_const(Const),
        mlds_output_rval_const(Opts, Const, !IO)
    ;
        Rval = ml_cast(Type, SubRval),
        mlds_output_cast_rval(Opts, Type, SubRval, !IO)
    ;
        Rval = ml_box(Type, SubRval),
        mlds_output_boxed_rval(Opts, Type, SubRval, !IO)
    ;
        Rval = ml_unbox(Type, SubRval),
        mlds_output_unboxed_rval(Opts, Type, SubRval, !IO)
    ;
        Rval = ml_unop(Unop, SubRval),
        mlds_output_unop(Opts, Unop, SubRval, !IO)
    ;
        Rval = ml_binop(BinOp, RvalA, RvalB),
        mlds_output_binop(Opts, BinOp, RvalA, RvalB, !IO)
    ;
        Rval = ml_mem_addr(Lval),
        % XXX Are parentheses needed?
        io.write_string("&", !IO),
        mlds_output_lval(Opts, Lval, !IO)
    ;
        (
            Rval = ml_scalar_common(ScalarCommon)
        ;
            Rval = ml_scalar_common_addr(ScalarCommon),
            io.write_string("&", !IO)
        ),
        ScalarCommon = ml_scalar_common(ModuleName, _Type,
            ml_scalar_common_type_num(TypeNum), RowNum),
        ModuleSymName = mlds_module_name_to_sym_name(ModuleName),
        MangledModuleName = sym_name_mangle(ModuleSymName),
        io.format("%s_scalar_common_%d[%d]",
            [s(MangledModuleName), i(TypeNum), i(RowNum)], !IO)
    ;
        Rval = ml_vector_common_row_addr(VectorCommon, RowRval),
        VectorCommon = ml_vector_common(ModuleName, _Type,
            ml_vector_common_type_num(TypeNum), StartRowNum, _NumRows),
        ModuleSymName = mlds_module_name_to_sym_name(ModuleName),
        MangledModuleName = sym_name_mangle(ModuleSymName),
        io.format("&%s_vector_common_%d[%d + ",
            [s(MangledModuleName), i(TypeNum), i(StartRowNum)], !IO),
        mlds_output_rval(Opts, RowRval, !IO),
        io.write_string("]", !IO)
    ;
        Rval = ml_self(_),
        io.write_string("this", !IO)
    ).

mlds_output_bracketed_rval(Opts, Rval, !IO) :-
    ( if
        % If it is just a variable name, then we do not need parentheses.
        ( Rval = ml_lval(ml_local_var(_,_))
        ; Rval = ml_lval(ml_global_var(_,_))
        ; Rval = ml_const(mlconst_code_addr(_))
        )
    then
        mlds_output_rval(Opts, Rval, !IO)
    else
        io.write_char('(', !IO),
        mlds_output_rval(Opts, Rval, !IO),
        io.write_char(')', !IO)
    ).

:- pred mlds_output_cast_rval(mlds_to_c_opts::in, mlds_type::in, mlds_rval::in,
    io::di, io::uo) is det.

mlds_output_cast_rval(Opts, Type, Rval, !IO) :-
    mlds_output_cast(Opts, Type, !IO),
    ( if
        Opts ^ m2co_highlevel_data = yes,
        Rval = ml_const(mlconst_float(Float))
    then
        mlds_output_float_bits(Opts, Float, !IO)
    else
        mlds_output_rval(Opts, Rval, !IO)
    ).

%---------------------%

mlds_output_boxed_rval(Opts, Type, Rval, !IO) :-
    ( if
        Rval = ml_cast(OtherType, InnerRval),
        ( Type = OtherType
        ; is_an_address(InnerRval) = yes
        )
    then
        % Avoid unnecessary double-casting -- strip away the inner cast.
        % This is necessary for ANSI/ISO C conformance, to avoid casts
        % from pointers to integers in static initializers.
        mlds_output_boxed_rval(Opts, Type, InnerRval, !IO)
    else
        (
            Type = mlds_generic_type,
            mlds_output_boxed_rval_generic(Opts, Rval, !IO)
        ;
            Type = mlds_native_float_type,
            mlds_output_boxed_rval_float(Opts, Rval, !IO)
        ;
            ( Type = mlds_native_char_type
            ; Type = mlds_native_bool_type
            ; Type = mlds_native_int_type       % XXX ARG_PACK
            ),
            mlds_output_boxed_rval_smaller_than_word(Opts, Rval, !IO)
        ;
            ( Type = mlds_native_uint_type
            ; Type = mlds_array_type(_)
            ; Type = mlds_mercury_array_type(_)
            ; Type = mlds_mostly_generic_array_type(_)
            ; Type = mlds_class_type(_)
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
            mlds_output_boxed_rval_default(Opts, Rval, !IO)
        ;
            Type = mercury_type(MercuryType, _, _),
            (
                MercuryType = builtin_type(BuiltinType),
                (
                    BuiltinType = builtin_type_float,
                    mlds_output_boxed_rval_float(Opts, Rval, !IO)
                ;
                    BuiltinType = builtin_type_int(IntType),
                    (
                        IntType = int_type_int64,
                        mlds_output_boxed_rval_int64(Opts, Rval, !IO)
                    ;
                        IntType = int_type_uint64,
                        mlds_output_boxed_rval_uint64(Opts, Rval, !IO)
                    ;
                        ( IntType = int_type_int
                        ; IntType = int_type_uint
                        ),
                        mlds_output_boxed_rval_default(Opts, Rval, !IO)
                    ;
                        ( IntType = int_type_int8
                        ; IntType = int_type_uint8
                        ; IntType = int_type_int16
                        ; IntType = int_type_uint16
                        ; IntType = int_type_int32
                        ; IntType = int_type_uint32
                        ),
                        mlds_output_boxed_rval_smaller_than_word(Opts, Rval,
                            !IO)
                    )
                ;
                    BuiltinType = builtin_type_char,
                    mlds_output_boxed_rval_smaller_than_word(Opts, Rval, !IO)
                ;
                    BuiltinType = builtin_type_string,
                    mlds_output_boxed_rval_default(Opts, Rval, !IO)
                )
            ;
                MercuryType = type_variable(_, _),
                mlds_output_boxed_rval_generic(Opts, Rval, !IO)
            ;
                ( MercuryType = defined_type(_, _, _)
                ; MercuryType = tuple_type(_, _)
                ; MercuryType = higher_order_type(_, _, _, _, _)
                ; MercuryType = apply_n_type(_, _, _)
                ; MercuryType = kinded_type(_, _)
                ),
                mlds_output_boxed_rval_default(Opts, Rval, !IO)
            )
        )
    ).

:- pred mlds_output_boxed_rval_generic(mlds_to_c_opts::in,
    mlds_rval::in, io::di, io::uo) is det.
:- pragma inline(mlds_output_boxed_rval_generic/4).

mlds_output_boxed_rval_generic(Opts, Rval, !IO) :-
    % Rval already has type MR_Box, so no cast is needed.
    mlds_output_rval(Opts, Rval, !IO).

:- pred mlds_output_boxed_rval_float(mlds_to_c_opts::in,
    mlds_rval::in, io::di, io::uo) is det.
:- pragma inline(mlds_output_boxed_rval_float/4).

mlds_output_boxed_rval_float(Opts, Rval, !IO) :-
    ( if
        Rval = ml_const(mlconst_float(Float)),
        Opts ^ m2co_highlevel_data = yes
    then
        mlds_output_float_bits(Opts, Float, !IO)
    else
        io.write_string("MR_box_float(", !IO),
        mlds_output_rval(Opts, Rval, !IO),
        io.write_string(")", !IO)
    ).

    % Output the bit layout of a floating point literal as an integer, so that
    % it can be cast to a pointer type. We manage to avoid this in all but one
    % situation: in high-level data grades, when the program contains a ground
    % term, of which a sub-term is a no-tag wrapper around float.
    %
    % Technically we should avoid doing this when --cross-compiling is
    % enabled.
    %
    % XXX the problem is the field type in the C struct which is generated for
    % the type which has the no-tag argument. The generated field type is a
    % pointer to the struct for the no-tag type, yet the no-tag optimisation is
    % used, so the field type should either be the struct for the no-tag type
    % (not a pointer) or the type which the no-tag type wraps (which itself may
    % be a no-tag type, etc.)
    %
:- pred mlds_output_float_bits(mlds_to_c_opts::in, float::in, io::di, io::uo)
    is det.

mlds_output_float_bits(Opts, Float, !IO) :-
    expect(unify(Opts ^ m2co_highlevel_data, yes), $pred,
        "should only be required with --high-level-data"),
    SinglePrecFloat = Opts ^ m2co_single_prec_float,
    (
        SinglePrecFloat = yes,
        String = float32_bits_string(Float)
    ;
        SinglePrecFloat = no,
        String = float64_bits_string(Float)
    ),
    io.format("%s /* float-bits: %g */", [s(String), f(Float)], !IO).

:- pred mlds_output_boxed_rval_int64(mlds_to_c_opts::in,
    mlds_rval::in, io::di, io::uo) is det.
:- pragma inline(mlds_output_boxed_rval_int64/4).

mlds_output_boxed_rval_int64(Opts, Rval, !IO) :-
    io.write_string("MR_box_int64(", !IO),
    mlds_output_rval(Opts, Rval, !IO),
    io.write_string(")", !IO).

:- pred mlds_output_boxed_rval_uint64(mlds_to_c_opts::in,
    mlds_rval::in, io::di, io::uo) is det.
:- pragma inline(mlds_output_boxed_rval_uint64/4).

mlds_output_boxed_rval_uint64(Opts, Rval, !IO) :-
    io.write_string("MR_box_uint64(", !IO),
    mlds_output_rval(Opts, Rval, !IO),
    io.write_string(")", !IO).

:- pred mlds_output_boxed_rval_smaller_than_word(mlds_to_c_opts::in,
    mlds_rval::in, io::di, io::uo) is det.
:- pragma inline(mlds_output_boxed_rval_smaller_than_word/4).

mlds_output_boxed_rval_smaller_than_word(Opts, Rval, !IO) :-
    % We cast first to MR_Word, and then to MR_Box.
    % We do this to avoid spurious warnings from gcc about
    % "cast from integer to pointer of different size".
    io.write_string("((MR_Box) (MR_Word) (", !IO),
    mlds_output_rval(Opts, Rval, !IO),
    io.write_string("))", !IO).

:- pred mlds_output_boxed_rval_default(mlds_to_c_opts::in,
    mlds_rval::in, io::di, io::uo) is det.
:- pragma inline(mlds_output_boxed_rval_default/4).

mlds_output_boxed_rval_default(Opts, Rval, !IO) :-
    io.write_string("((MR_Box) (", !IO),
    mlds_output_rval(Opts, Rval, !IO),
    io.write_string("))", !IO).

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

:- pred mlds_output_unboxed_rval(mlds_to_c_opts::in,
    mlds_type::in, mlds_rval::in, io::di, io::uo) is det.

mlds_output_unboxed_rval(Opts, Type, Rval, !IO) :-
    (
        Type = mlds_native_float_type,
        mlds_output_unboxed_rval_float(Opts, Rval, !IO)
    ;
        ( Type = mlds_native_char_type
        ; Type = mlds_native_bool_type
        ; Type = mlds_native_int_type   % XXX ARG_PACK
        ),
        mlds_output_unboxed_rval_smaller_than_word(Opts, Type, Rval, !IO)
    ;
        ( Type = mlds_native_uint_type
        ; Type = mlds_array_type(_)
        ; Type = mlds_mercury_array_type(_)
        ; Type = mlds_mostly_generic_array_type(_)
        ; Type = mlds_class_type(_)
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
        mlds_output_unboxed_rval_default(Opts, Type, Rval, !IO)
    ;
        Type = mercury_type(MercuryType, _, _),
        (
            MercuryType = builtin_type(BuiltinType),
            (
                BuiltinType = builtin_type_float,
                mlds_output_unboxed_rval_float(Opts, Rval, !IO)
            ;
                BuiltinType = builtin_type_int(IntType),
                (
                    IntType = int_type_int64,
                    mlds_output_unboxed_rval_int64(Opts, Rval, !IO)
                ;
                    IntType = int_type_uint64,
                    mlds_output_unboxed_rval_uint64(Opts, Rval, !IO)
                ;
                    ( IntType = int_type_int
                    ; IntType = int_type_uint
                    ),
                    mlds_output_unboxed_rval_default(Opts, Type, Rval, !IO)
                ;
                    % The following integer types are all (potentially) smaller
                    % than MR_Word.
                    ( IntType = int_type_int8
                    ; IntType = int_type_uint8
                    ; IntType = int_type_int16
                    ; IntType = int_type_uint16
                    ; IntType = int_type_int32
                    ; IntType = int_type_uint32
                    ),
                    mlds_output_unboxed_rval_smaller_than_word(Opts,
                        Type, Rval, !IO)
                )
            ;
                BuiltinType = builtin_type_char,
                mlds_output_unboxed_rval_smaller_than_word(Opts,
                    Type, Rval, !IO)
            ;
                BuiltinType = builtin_type_string,
                mlds_output_unboxed_rval_default(Opts, Type, Rval, !IO)
            )
        ;
            ( MercuryType = type_variable(_, _)
            ; MercuryType = defined_type(_, _, _)
            ; MercuryType = tuple_type(_, _)
            ; MercuryType = higher_order_type(_, _, _, _, _)
            ; MercuryType = apply_n_type(_, _, _)
            ; MercuryType = kinded_type(_, _)
            ),
            mlds_output_unboxed_rval_default(Opts, Type, Rval, !IO)
        )
    ).

:- pred mlds_output_unboxed_rval_float(mlds_to_c_opts::in,
    mlds_rval::in, io::di, io::uo) is det.
:- pragma inline(mlds_output_unboxed_rval_float/4).

mlds_output_unboxed_rval_float(Opts, Rval, !IO) :-
    io.write_string("MR_unbox_float(", !IO),
    mlds_output_rval(Opts, Rval, !IO),
    io.write_string(")", !IO).

:- pred mlds_output_unboxed_rval_int64(mlds_to_c_opts::in,
    mlds_rval::in, io::di, io::uo) is det.
:- pragma inline(mlds_output_unboxed_rval_int64/4).

mlds_output_unboxed_rval_int64(Opts, Rval, !IO) :-
    io.write_string("MR_unbox_int64(", !IO),
    mlds_output_rval(Opts, Rval, !IO),
    io.write_string(")", !IO).

:- pred mlds_output_unboxed_rval_uint64(mlds_to_c_opts::in,
    mlds_rval::in, io::di, io::uo) is det.
:- pragma inline(mlds_output_unboxed_rval_uint64/4).

mlds_output_unboxed_rval_uint64(Opts, Rval, !IO) :-
    io.write_string("MR_unbox_uint64(", !IO),
    mlds_output_rval(Opts, Rval, !IO),
    io.write_string(")", !IO).

:- pred mlds_output_unboxed_rval_smaller_than_word(mlds_to_c_opts::in,
    mlds_type::in, mlds_rval::in, io::di, io::uo) is det.
:- pragma inline(mlds_output_unboxed_rval_smaller_than_word/5).

mlds_output_unboxed_rval_smaller_than_word(Opts, Type, Rval, !IO) :-
    % We cast first to MR_Word, and then to the desired type.
    % This is done to avoid spurious warnings from gcc about
    % "cast from pointer to integer of different size".
    % XXX ARG_PACK We call this predicate both too much and not enough.
    % - We call it for builtin types that *may* be smaller than a word
    %   on some platforms, whether or not it is smaller than a word
    %   on *this* platform.
    % - We do *not* call it values of dummy and enum types, which *are*
    %   smaller than a word.
    io.write_string("(", !IO),
    mlds_output_cast(Opts, Type, !IO),
    io.write_string("(MR_Word) ", !IO),
    mlds_output_rval(Opts, Rval, !IO),
    io.write_string(")", !IO).

:- pred mlds_output_unboxed_rval_default(mlds_to_c_opts::in,
    mlds_type::in, mlds_rval::in, io::di, io::uo) is det.
:- pragma inline(mlds_output_unboxed_rval_default/5).

mlds_output_unboxed_rval_default(Opts, Type, Rval, !IO) :-
    io.write_string("(", !IO),
    mlds_output_cast(Opts, Type, !IO),
    mlds_output_rval(Opts, Rval, !IO),
    io.write_string(")", !IO).

%---------------------%

:- pred mlds_output_unop(mlds_to_c_opts::in, unary_op::in, mlds_rval::in,
    io::di, io::uo) is det.

mlds_output_unop(Opts, UnaryOp, Expr, !IO) :-
    c_util.unary_prefix_op(UnaryOp, UnaryOpString),
    io.write_string(UnaryOpString, !IO),
    io.write_string("(", !IO),
    ( if UnaryOp = tag then
        % The MR_tag macro requires its argument to be of type `MR_Word'.
        % XXX Should we put this cast inside the definition of MR_tag?
        io.write_string("(MR_Word) ", !IO)
    else
        true
    ),
    mlds_output_rval(Opts, Expr, !IO),
    io.write_string(")", !IO).

:- pred mlds_output_binop(mlds_to_c_opts::in, binary_op::in,
    mlds_rval::in, mlds_rval::in, io::di, io::uo) is det.

mlds_output_binop(Opts, Op, X, Y, !IO) :-
    (
        Op = array_index(_),
        mlds_output_bracketed_rval(Opts, X, !IO),
        io.write_string("[", !IO),
        mlds_output_rval(Opts, Y, !IO),
        io.write_string("]", !IO)
    ;
        Op = string_unsafe_index_code_unit,
        io.write_string("MR_nth_code_unit(", !IO),
        mlds_output_bracketed_rval(Opts, X, !IO),
        io.write_string(", ", !IO),
        ( if Y = ml_const(mlconst_int(YN)) then
            io.write_int(YN, !IO)
        else
            mlds_output_rval(Opts, Y, !IO)
        ),
        io.write_string(")", !IO)
    ;
        ( Op = compound_lt
        ; Op = compound_eq
        ),
        % These operators are intended to be generated only when using
        % the Erlang backend.
        unexpected($pred, "compound_compare_binop")
    ;
        Op = pointer_equal_conservative,
        io.write_string("(((MR_Word) ", !IO),
        mlds_output_rval(Opts, X, !IO),
        io.write_string(") == ((MR_Word) ", !IO),
        mlds_output_rval(Opts, Y, !IO),
        io.write_string("))", !IO)
    ;
        ( Op = str_eq, OpStr = "=="
        ; Op = str_ne, OpStr = "!="
        ; Op = str_le, OpStr = "<="
        ; Op = str_ge, OpStr = ">="
        ; Op = str_lt, OpStr = "<"
        ; Op = str_gt, OpStr = ">"
        ),
        io.write_string("(strcmp(", !IO),
        mlds_output_rval(Opts, X, !IO),
        io.write_string(", ", !IO),
        mlds_output_rval(Opts, Y, !IO),
        io.write_string(")", !IO),
        io.write_string(" ", !IO),
        io.write_string(OpStr, !IO),
        io.write_string(" ", !IO),
        io.write_string("0)", !IO)
    ;
        ( Op = float_eq, OpStr = "=="
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
        io.write_string("(", !IO),
        mlds_output_bracketed_rval(Opts, X, !IO),
        io.write_string(" ", !IO),
        io.write_string(OpStr, !IO),
        io.write_string(" ", !IO),
        mlds_output_bracketed_rval(Opts, Y, !IO),
        io.write_string(")", !IO)
    ;
        Op = unsigned_le,
        io.write_string("(((MR_Unsigned) ", !IO),
        mlds_output_rval(Opts, X, !IO),
        io.write_string(") <= ((MR_Unsigned) ", !IO),
        mlds_output_rval(Opts, Y, !IO),
        io.write_string("))", !IO)
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
            io.format("(%s) ((%s) ", [s(SignedType), s(UnsignedType)], !IO),
            mlds_output_rval_as_op_arg(Opts, X, !IO),
            io.format(" %s (%s) ", [s(OpStr), s(UnsignedType)], !IO),
            mlds_output_rval_as_op_arg(Opts, Y, !IO),
            io.write_string(")", !IO)
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
            io.write_string("(", !IO),
            mlds_output_rval_as_op_arg(Opts, X, !IO),
            io.format(" %s ", [s(OpStr)], !IO),
            mlds_output_rval_as_op_arg(Opts, Y, !IO),
            io.write_string(")", !IO)
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
        ; Op = unchecked_left_shift(_), OpStr = "<<"
        ; Op = unchecked_right_shift(_), OpStr = ">>"
        ; Op = bitwise_and(_), OpStr = "&"
        ; Op = bitwise_or(_), OpStr = "|"
        ; Op = bitwise_xor(_), OpStr = "^"
        ; Op = logical_and, OpStr = "&&"
        ; Op = logical_or, OpStr = "||"
        ),
        % We could treat X + (-const) specially, but we do not.
        % The reason is documented in the equivalent code in llds_out_data.m.
        io.write_string("(", !IO),
        mlds_output_rval_as_op_arg(Opts, X, !IO),
        io.write_string(" ", !IO),
        io.write_string(OpStr, !IO),
        io.write_string(" ", !IO),
        mlds_output_rval_as_op_arg(Opts, Y, !IO),
        io.write_string(")", !IO)
    ;
        Op = str_cmp,
        io.write_string("MR_strcmp(", !IO),
        mlds_output_rval_as_op_arg(Opts, X, !IO),
        io.write_string(", ", !IO),
        mlds_output_rval_as_op_arg(Opts, Y, !IO),
        io.write_string(")", !IO)
    ;
        Op = offset_str_eq(N),
        io.write_string("MR_offset_streq(", !IO),
        io.write_int(N, !IO),
        io.write_string(", ", !IO),
        mlds_output_rval_as_op_arg(Opts, X, !IO),
        io.write_string(", ", !IO),
        mlds_output_rval_as_op_arg(Opts, Y, !IO),
        io.write_string(")", !IO)
    ;
        Op = body,
        io.write_string("MR_body(", !IO),
        mlds_output_rval_as_op_arg(Opts, X, !IO),
        io.write_string(", ", !IO),
        mlds_output_rval_as_op_arg(Opts, Y, !IO),
        io.write_string(")", !IO)
    ;
        ( Op = float_from_dword,  OpStr = "MR_float_from_dword"
        ; Op = int64_from_dword,  OpStr = "MR_int64_from_dword"
        ; Op = uint64_from_dword, OpStr = "MR_uint64_from_dword"
        ),
        io.write_string(OpStr, !IO),
        ( if is_aligned_dword_field(X, Y, PtrRval) then
            % gcc produces faster code in this case.
            io.write_string("_ptr(MR_dword_ptr(", !IO),
            mlds_output_rval(Opts, PtrRval, !IO),
            io.write_string("))", !IO)
        else
            io.write_string("(", !IO),
            mlds_output_rval_as_op_arg(Opts, X, !IO),
            io.write_string(", ", !IO),
            mlds_output_rval_as_op_arg(Opts, Y, !IO),
            io.write_string(")", !IO)
        )
    ).

:- pred mlds_output_rval_as_op_arg(mlds_to_c_opts::in, mlds_rval::in,
    io::di, io::uo) is det.

mlds_output_rval_as_op_arg(Opts, Rval, !IO) :-
    ( if
        ( Rval = ml_unop(_, _)
        ; Rval = ml_binop(_, _, _)
        )
    then
        io.write_string("(", !IO),
        mlds_output_rval(Opts, Rval, !IO),
        io.write_string(")", !IO)
    else
        mlds_output_rval(Opts, Rval, !IO)
    ).

%---------------------%

:- pred mlds_output_rval_const(mlds_to_c_opts::in, mlds_rval_const::in,
    io::di, io::uo) is det.

mlds_output_rval_const(_Opts, Const, !IO) :-
    (
        Const = mlconst_true,
        io.write_string("MR_TRUE", !IO)
    ;
        Const = mlconst_false,
        io.write_string("MR_FALSE", !IO)
    ;
        ( Const = mlconst_int(N)
        ; Const = mlconst_enum(N, _)
        ),
        c_util.output_int_expr_cur_stream(N, !IO)
    ;
        Const = mlconst_uint(U),
        c_util.output_uint_expr_cur_stream(U, !IO)
    ;
        Const = mlconst_int8(N),
        c_util.output_int8_expr_cur_stream(N, !IO)
    ;
        Const = mlconst_uint8(N),
        c_util.output_uint8_expr_cur_stream(N, !IO)
    ;
        Const = mlconst_int16(N),
        c_util.output_int16_expr_cur_stream(N, !IO)
    ;
        Const = mlconst_uint16(N),
        c_util.output_uint16_expr_cur_stream(N, !IO)
    ;
        Const = mlconst_int32(N),
        c_util.output_int32_expr_cur_stream(N, !IO)
    ;
        Const = mlconst_uint32(N),
        c_util.output_uint32_expr_cur_stream(N, !IO)
    ;
        Const = mlconst_int64(N),
        c_util.output_int64_expr_cur_stream(N, !IO)
    ;
        Const = mlconst_uint64(N),
        c_util.output_uint64_expr_cur_stream(N, !IO)
    ;
        Const = mlconst_char(C),
        io.write_string("(MR_Char) ", !IO),
        io.write_int(C, !IO)
    ;
        Const = mlconst_foreign(Lang, Value, _Type),
        expect(unify(Lang, lang_c), $pred,
            "mlconst_foreign for language other than C"),
        io.write_string("((int) ", !IO),
        io.write_string(Value, !IO),
        io.write_string(")", !IO)
    ;
        Const = mlconst_float(FloatVal),
        % The cast to (MR_Float) here lets the C compiler do arithmetic in
        % `float' rather than `double' if `MR_Float' is `float' not `double'.
        io.write_string("(MR_Float) ", !IO),
        c_util.output_float_literal_cur_stream(FloatVal, !IO)
    ;
        Const = mlconst_string(String),
        % The cast avoids the following gcc warning
        % "assignment discards qualifiers from pointer target type".
        io.write_string("(MR_String) ", !IO),
        io.write_string("""", !IO),
        c_util.output_quoted_string_cur_stream(String, !IO),
        io.write_string("""", !IO)
    ;
        Const = mlconst_multi_string(String),
        io.write_string("""", !IO),
        c_util.output_quoted_multi_string_cur_stream(String, !IO),
        io.write_string("""", !IO)
    ;
        Const = mlconst_named_const(_TargetPrefixes, NamedConst),
        % The target prefix for C is implicitly always empty.
        io.write_string(NamedConst, !IO)
    ;
        Const = mlconst_code_addr(CodeAddr),
        mlds_output_code_addr(CodeAddr, !IO)
    ;
        Const = mlconst_data_addr_local_var(LocalVarName),
        MangledLocalVarName =
            name_mangle(ml_local_var_name_to_string(LocalVarName)),
        io.write_string("&", !IO),
        io.write_string(MangledLocalVarName, !IO)
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
        (
            IsArray = is_array
        ;
            IsArray = not_array,
            io.write_string("&", !IO)
        ),
        mlds_output_maybe_qualified_global_var_name(MLDS_ModuleName,
            GlobalVarName, !IO)
    ;
        Const = mlconst_null(_),
        io.write_string("NULL", !IO)
    ).

:- pred is_aligned_dword_field(mlds_rval::in, mlds_rval::in, mlds_rval::out)
    is semidet.

is_aligned_dword_field(X, Y, ml_mem_addr(Lval)) :-
    X = ml_lval(ml_field(Ptag, Addr, FieldIdX, Type, PtrType) @ Lval),
    Y = ml_lval(ml_field(Ptag, Addr, FieldIdY, Type, PtrType)),
    FieldIdX = ml_field_offset(ml_const(mlconst_int(Offset))),
    FieldIdY = ml_field_offset(ml_const(mlconst_int(Offset + 1))),
    int.even(Offset).

:- pred mlds_output_code_addr(mlds_code_addr::in, io::di, io::uo) is det.

mlds_output_code_addr(CodeAddr, !IO) :-
    CodeAddr = mlds_code_addr(QualFuncLabel, _Signature),
    QualFuncLabel = qual_func_label(ModuleName, FuncLabel),
    FuncLabel = mlds_func_label(ProcLabel, MaybeAux),
    QualProcLabel = qual_proc_label(ModuleName, ProcLabel),
    mlds_output_fully_qualified_proc_label(QualProcLabel, !IO),
    io.write_string(mlds_maybe_aux_func_id_to_suffix(MaybeAux), !IO).

%---------------------------------------------------------------------------%

mlds_output_initializer(Opts, _Type, Initializer, !IO) :-
    NeedsInit = mlds_needs_initialization(Initializer),
    (
        NeedsInit = yes,
        io.write_string(" = ", !IO),
        mlds_output_initializer_body(Opts, 0, Initializer, !IO)
    ;
        NeedsInit = no
    ).

:- func mlds_needs_initialization(mlds_initializer) = bool.

mlds_needs_initialization(no_initializer) = no.
mlds_needs_initialization(init_obj(_)) = yes.
mlds_needs_initialization(init_struct(_Type, [])) = no.
mlds_needs_initialization(init_struct(_Type, [_|_])) = yes.
mlds_needs_initialization(init_array(_)) = yes.

mlds_output_initializer_body(Opts, Indent, Initializer, !IO) :-
    (
        Initializer = no_initializer
    ;
        Initializer = init_obj(Rval),
        output_n_indents(Indent, !IO),
        mlds_output_rval(Opts, Rval, !IO)
    ;
        Initializer = init_struct(_Type, FieldInitializers),
        % Note that standard ANSI/ISO C does not allow empty structs, and
        % it is the responsibility of the MLDS code generator to not generate
        % any such structs.
        (
            FieldInitializers = [],
            unexpected($pred, "FieldInitializers = []")
        ;
            FieldInitializers = [FieldInitializer],
            output_n_indents(Indent, !IO),
            io.write_string("{ ", !IO),
            mlds_output_initializer_body(Opts, Indent + 1,
                FieldInitializer, !IO),
            io.write_string(" }", !IO)
        ;
            FieldInitializers = [_, _ | _],
            output_n_indents(Indent, !IO),
            io.write_string("{\n", !IO),
            io.write_list(FieldInitializers, ",\n",
                mlds_output_initializer_body(Opts, Indent + 1), !IO),
            io.write_string("\n", !IO),
            output_n_indents(Indent, !IO),
            io.write_string("}", !IO)
        )
    ;
        Initializer = init_array(ElementInitializers),
        % Standard ANSI/ISO C does not allow empty arrays. But the MLDS does.
        % To keep the C compiler happy, we therefore convert zero-element MLDS
        % arrays into one-element C arrays. (The extra element is a minor waste
        % of space, but it will otherwise be ignored.) So if the initializer
        % list here is empty, we need to output a single initializer.
        % We can initialize the extra element with any value. We use "0",
        % since that is a valid initializer for any type.
        (
            ElementInitializers = [],
            output_n_indents(Indent, !IO),
            io.write_string("{ 0 }\n", !IO)
        ;
            ElementInitializers = [_ | _],
            output_n_indents(Indent, !IO),
            io.write_string("{\n", !IO),
            io.write_list(ElementInitializers, ",\n",
                mlds_output_initializer_body(Opts, Indent + 1), !IO),
            io.write_string("\n", !IO),
            output_n_indents(Indent, !IO),
            io.write_string("}", !IO)
        )
    ).

%---------------------------------------------------------------------------%
:- end_module ml_backend.mlds_to_c_data.
%---------------------------------------------------------------------------%
