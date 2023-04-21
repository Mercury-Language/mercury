%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2018 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% Output types.
%
%---------------------------------------------------------------------------%

:- module ml_backend.mlds_to_c_type.
:- interface.

:- import_module ml_backend.mlds.
:- import_module ml_backend.mlds_to_c_util.

:- import_module io.
:- import_module list.

%---------------------%

:- pred mlds_output_type(mlds_to_c_opts::in, mlds_type::in,
    io.text_output_stream::in, io::di, io::uo) is det.

    % This type/inst pair is not *used* in this module, but it is used
    % to describe the signature of the predicates in the following block
    % of declarations.
    %
:- type output_type == pred(mlds_to_c_opts, io.text_output_stream,
    mlds_type, io, io).
:- inst output_type == (pred(in, in, in, di, uo) is det).

    % Because of the joys of C syntax, the code for outputting types
    % needs to be split into two parts; first the prefix, i.e. the part
    % of the type name that goes before the variable name in a variable
    % declaration, and then the suffix, i.e. the part which goes after
    % the variable name, e.g. the "[]" for array types.
    %
:- pred mlds_output_type_prefix(mlds_to_c_opts::in,
    io.text_output_stream::in, mlds_type::in, io::di, io::uo) is det.
:- pred mlds_output_type_suffix_no_size(mlds_to_c_opts::in,
    io.text_output_stream::in, mlds_type::in, io::di, io::uo) is det.
:- pred mlds_output_type_suffix(mlds_to_c_opts::in,
    io.text_output_stream::in, mlds_type::in, initializer_array_size::in,
    io::di, io::uo) is det.

%---------------------%

    % For any given MLDS initializer, this function returns an equivalent
    % initializer that is more "standard". This means that it can replace
    % some of the types in the initializer with simpler types that we
    % nevertheless translate to the same C type.
    %
    % The idea is to let ml_global_data.m discover that two MLDS initializers 
    % that are different would nevertheless generate the same row data
    % in a scalar common array, and thus can be translated to the same row,
    % instead of different rows with identical contents.
    %
    % The name contains *semi* in front of canonicalize, because we do *not*
    % guarantee that we map all MLDS initializers that generate the same
    % C output to the same result.
    % 
:- func semicanonicalize_types_in_initializer_for_c(mlds_initializer)
    = mlds_initializer.

%---------------------%

    % mlds_output_return_list(List, OutputPred, !IO) outputs
    % a List of return types/values using OutputPred.
    %
:- pred mlds_output_return_list(io.text_output_stream::in,
    pred(T, io.text_output_stream, io, io)::in(pred(in, in, di, uo) is det),
    list(T)::in, io::di, io::uo) is det.

%---------------------%

:- pred mlds_output_cast(mlds_to_c_opts::in, io.text_output_stream::in,
    mlds_type::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.rtti.
:- import_module libs.
:- import_module libs.maybe_util.
:- import_module ml_backend.mlds_to_c_name.
:- import_module parse_tree.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_foreign.
:- import_module parse_tree.prog_type.

:- import_module int.
:- import_module require.
:- import_module string.

%---------------------------------------------------------------------------%

mlds_output_type(Opts, Type, Stream, !IO) :-
    % In the declaration of a type, as opposed to the declaration of a
    % variable, the variable name is not there, so we have just the prefix and
    % the suffix.
    mlds_output_type_prefix(Opts, Stream, Type, !IO),
    mlds_output_type_suffix(Opts, Stream, Type, no_size, !IO).

%---------------------%

mlds_output_type_prefix(Opts, Stream, MLDS_Type, !IO) :-
    % semicanonicalize_types_in_initializer_for_c relies on knowing
    % which MLDS_Types generate the same C types, so if this predicate
    % is updated so that two MLDS_Types that used to generate the same
    % C type no longer do so, you will need to update that function.
    (
        MLDS_Type = mercury_nb_type(_Type, TypeCategory),
        mlds_output_mercury_type_prefix(Stream, TypeCategory, !IO)
    ;
        MLDS_Type = mlds_mercury_array_type(_ElemType),
        io.write_string(Stream, "MR_ArrayPtr", !IO)
    ;
        MLDS_Type = mlds_builtin_type_int(IntType),
        (
            IntType = int_type_int,
            io.write_string(Stream, "MR_Integer", !IO)
        ;
            IntType = int_type_uint,
            io.write_string(Stream, "MR_Unsigned", !IO)
        ;
            IntType = int_type_int8,
            io.write_string(Stream, "int8_t", !IO)
        ;
            IntType = int_type_uint8,
            io.write_string(Stream, "uint8_t", !IO)
        ;
            IntType = int_type_int16,
            io.write_string(Stream, "int16_t", !IO)
        ;
            IntType = int_type_uint16,
            io.write_string(Stream, "uint16_t", !IO)
        ;
            IntType = int_type_int32,
            io.write_string(Stream, "int32_t", !IO)
        ;
            IntType = int_type_uint32,
            io.write_string(Stream, "uint32_t", !IO)
        ;
            IntType = int_type_int64,
            io.write_string(Stream, "int64_t", !IO)
        ;
            IntType = int_type_uint64,
            io.write_string(Stream, "uint64_t", !IO)
        )
    ;
        MLDS_Type = mlds_builtin_type_float,
        io.write_string(Stream, "MR_Float", !IO)
    ;
        MLDS_Type = mlds_builtin_type_char,
        io.write_string(Stream, "MR_Char", !IO)
    ;
        MLDS_Type = mlds_builtin_type_string,
        io.write_string(Stream, "MR_String", !IO)
    ;
        MLDS_Type = mlds_native_bool_type,
        io.write_string(Stream, "MR_bool", !IO)
    ;
        MLDS_Type = mlds_foreign_type(_ForeignType),
        % For binary compatibility with the --target asm back-end,
        % we need to output these as a generic type, rather than making
        % use of the C type name
        % XXX target asm no longer exists, so no longer need to do this.
        io.write_string(Stream, "MR_Box", !IO)
    ;
        MLDS_Type = mlds_class_type(ClassId),
        ClassId = mlds_class_id(QualClassName, Arity, ClassKind),
        QualClassName = qual_class_name(ModuleName, _QualKind, ClassName),
        (
            ClassKind = mlds_enum,
            % We cannot just use the enumeration type, since the enumeration
            % type's definition is not guaranteed to be in scope at this point.
            % (Fixing that would be somewhat complicated; it would require
            % writing enum definitions to a separate header file.) Also,
            % the enumeration might not be word-sized, which would cause
            % problems for e.g. `std_util.arg/2'. So we just use `MR_Integer',
            % and output the actual enumeration type as a comment.
            io.format(Stream, "MR_Integer /* actually `enum %s%s_%d_e' */",
                [s(qual_name_prefix_c(ModuleName)), s(name_mangle(ClassName)),
                i(Arity)], !IO)
        ;
            ( ClassKind = mlds_class
            ; ClassKind = mlds_interface
            ; ClassKind = mlds_struct
            ),
            % For struct types, it is OK to output an incomplete type, since
            % we do not use these types directly; we only use pointers to them.
            io.format(Stream, "struct %s%s_%d_s",
                [s(qual_name_prefix_c(ModuleName)), s(name_mangle(ClassName)),
                i(Arity)], !IO)
        )
    ;
        MLDS_Type = mlds_ptr_type(Type),
        mlds_output_type(Opts, Type, Stream, !IO),
        io.write_string(Stream, " *", !IO)
    ;
        MLDS_Type = mlds_array_type(Type),
        % Here we just output the element type. The "[]" goes in the type
        % suffix.
        mlds_output_type(Opts, Type, Stream, !IO)
    ;
        MLDS_Type = mlds_mostly_generic_array_type(_),
        Type = mlds_generic_type,
        mlds_output_type(Opts, Type, Stream, !IO)
    ;
        MLDS_Type = mlds_func_type(FuncParams),
        mlds_output_func_type_prefix(Opts, Stream, FuncParams, !IO)
    ;
        MLDS_Type = mlds_generic_type,
        io.write_string(Stream, "MR_Box", !IO)
    ;
        MLDS_Type = mlds_generic_env_ptr_type,
        io.write_string(Stream, "void *", !IO)
    ;
        MLDS_Type = mlds_type_info_type,
        io.write_string(Stream, "MR_TypeInfo", !IO)
    ;
        MLDS_Type = mlds_pseudo_type_info_type,
        io.write_string(Stream, "MR_PseudoTypeInfo", !IO)
    ;
        MLDS_Type = mlds_cont_type(ArgTypes),
        (
            ArgTypes = [],
            io.write_string(Stream, "MR_Cont", !IO)
        ;
            ArgTypes = [_ | _],
            % This case only happens for --nondet-copy-out.
            io.write_string(Stream, "void MR_CALL (*", !IO)
        )
    ;
        MLDS_Type = mlds_commit_type,
        io.write_string(Stream, "jmp_buf", !IO)
    ;
        MLDS_Type = mlds_rtti_type(RttiIdMaybeElement),
        rtti_id_maybe_element_c_type(RttiIdMaybeElement, CType, _IsArray),
        io.write_string(Stream, CType, !IO)
    ;
        MLDS_Type = mlds_tabling_type(TablingId),
        tabling_id_c_type(TablingId, CType, _IsArray),
        io.write_string(Stream, CType, !IO)
    ;
        MLDS_Type = mlds_unknown_type,
        unexpected($pred, "prefix has unknown type")
    ).

:- pred mlds_output_mercury_type_prefix(io.text_output_stream::in,
    type_ctor_category::in, io::di, io::uo) is det.

mlds_output_mercury_type_prefix(Stream, CtorCat, !IO) :-
    % semicanonicalize_types_in_initializer_for_c relies on knowing
    % which MLDS_Types generate the same C types, so if this predicate
    % is updated so that two MLDS_Types that used to generate the same
    % C type no longer do so, you will need to update that function.
    (
        CtorCat = ctor_cat_builtin(_),
        unexpected($pred, "ctor_cat_builtin")
    ;
        CtorCat = ctor_cat_variable,
        io.write_string(Stream, "MR_Box", !IO)
    ;
        CtorCat = ctor_cat_tuple,
        io.write_string(Stream, "MR_Tuple", !IO)
    ;
        % runtime/mercury_hlc_types requires typeinfos, typeclass_infos etc
        % to be treated as user defined types.
        ( CtorCat = ctor_cat_higher_order
        ; CtorCat = ctor_cat_void
        ; CtorCat = ctor_cat_builtin_dummy
        ; CtorCat = ctor_cat_enum(_)
        ; CtorCat = ctor_cat_user(_)
        ; CtorCat = ctor_cat_system(_)
        ),
        io.write_string(Stream, "MR_Word", !IO)
    ).

:- pred mlds_output_func_type_prefix(mlds_to_c_opts::in,
    io.text_output_stream::in, mlds_func_params::in, io::di, io::uo) is det.

mlds_output_func_type_prefix(Opts, Stream, Params, !IO) :-
    Params = mlds_func_params(_Parameters, RetTypes),
    (
        RetTypes = [],
        io.write_string(Stream, "void", !IO)
    ;
        RetTypes = [RetType],
        mlds_output_type(Opts, RetType, Stream, !IO)
    ;
        RetTypes = [_, _ | _],
        mlds_output_return_list(Stream, mlds_output_type(Opts), RetTypes, !IO)
    ),
    % Note that mlds_func_type actually corresponds to a function _pointer_
    % type in C. This is necessary because function types in C are not first
    % class.
    io.write_string(Stream, " MR_CALL (*", !IO).

%---------------------%

mlds_output_type_suffix_no_size(Opts, Stream, Type, !IO) :-
    mlds_output_type_suffix(Opts, Stream, Type, no_size, !IO).

mlds_output_type_suffix(Opts, Stream, MLDS_Type, ArraySize, !IO) :-
    % semicanonicalize_types_in_initializer_for_c relies on knowing
    % which MLDS_Types generate the same C types, so if this predicate
    % is updated so that two MLDS_Types that used to generate the same
    % C type no longer do so, you will need to update that function.
    (
        MLDS_Type = mlds_array_type(_),
        mlds_output_array_type_suffix(Stream, ArraySize, !IO)
    ;
        MLDS_Type = mlds_mostly_generic_array_type(_)
    ;
        MLDS_Type = mlds_func_type(FuncParams),
        mlds_output_func_type_suffix(Opts, Stream, FuncParams, !IO)
    ;
        MLDS_Type = mlds_cont_type(ArgTypes),
        (
            ArgTypes = []
        ;
            ArgTypes = [_ | _],
            % This case only happens for --nondet-copy-out.
            io.write_string(Stream, ")(", !IO),
            write_out_list(mlds_output_type(Opts), ", ", ArgTypes,
                Stream, !IO),
            % add the type for the environment parameter, if needed
            io.write_string(Stream, ", void *)", !IO)
        )
    ;
        MLDS_Type = mlds_rtti_type(RttiIdMaybeElement),
        IsArrayType = rtti_id_maybe_element_has_array_type(RttiIdMaybeElement),
        (
            IsArrayType = is_array,
            mlds_output_array_type_suffix(Stream, ArraySize, !IO)
        ;
            IsArrayType = not_array
        )
    ;
        MLDS_Type = mlds_tabling_type(TablingId),
        IsArrayType = tabling_id_has_array_type(TablingId),
        (
            IsArrayType = is_array,
            mlds_output_array_type_suffix(Stream, ArraySize, !IO)
        ;
            IsArrayType = not_array
        )
    ;
        MLDS_Type = mlds_unknown_type,
        unexpected($pred, "unknown_type")
    ;
        ( MLDS_Type = mercury_nb_type(_, _)
        ; MLDS_Type = mlds_mercury_array_type(_)
        ; MLDS_Type = mlds_builtin_type_int(_)
        ; MLDS_Type = mlds_builtin_type_float
        ; MLDS_Type = mlds_builtin_type_string
        ; MLDS_Type = mlds_builtin_type_char
        ; MLDS_Type = mlds_native_bool_type
        % XXX Currently we cannot output a type suffix.
        ; MLDS_Type = mlds_foreign_type(_)
        ; MLDS_Type = mlds_class_type(_)
        ; MLDS_Type = mlds_ptr_type(_)
        ; MLDS_Type = mlds_generic_type
        ; MLDS_Type = mlds_generic_env_ptr_type
        ; MLDS_Type = mlds_type_info_type
        ; MLDS_Type = mlds_pseudo_type_info_type
        ; MLDS_Type = mlds_commit_type
        )
    ).

:- pred mlds_output_func_type_suffix(mlds_to_c_opts::in,
    io.text_output_stream::in, mlds_func_params::in, io::di, io::uo) is det.

mlds_output_func_type_suffix(Opts, Stream, Params, !IO) :-
    Params = mlds_func_params(Parameters, _RetTypes),
    io.write_string(Stream, ")", !IO),
    mlds_output_param_types(Opts, Stream, Parameters, !IO).

:- pred mlds_output_param_types(mlds_to_c_opts::in, io.text_output_stream::in,
    list(mlds_argument)::in, io::di, io::uo) is det.

mlds_output_param_types(Opts, Stream, Parameters, !IO) :-
    io.write_char(Stream, '(', !IO),
    (
        Parameters = [],
        io.write_string(Stream, "void", !IO)
    ;
        Parameters = [_ | _],
        write_out_list(mlds_output_param_type(Opts), ", ", Parameters,
            Stream, !IO)
    ),
    io.write_char(Stream, ')', !IO).

:- pred mlds_output_param_type(mlds_to_c_opts::in, mlds_argument::in,
    io.text_output_stream::in, io::di, io::uo) is det.

mlds_output_param_type(Opts, Arg, Stream, !IO) :-
    Arg = mlds_argument(_Name, Type, _GCStmt),
    mlds_output_type(Opts, Type, Stream, !IO).

:- pred mlds_output_array_type_suffix(io.text_output_stream::in,
    initializer_array_size::in, io::di, io::uo) is det.

mlds_output_array_type_suffix(Stream, no_size, !IO) :-
    io.write_string(Stream, "[]", !IO).
mlds_output_array_type_suffix(Stream, array_size(Size0), !IO) :-
    % Standard ANSI/ISO C does not allow arrays of size 0. But the MLDS does.
    % To keep the C compiler happy, we therefore convert zero-element MLDS
    % arrays into one-element C arrays.
    int.max(Size0, 1, Size),
    io.format(Stream, "[%d]", [i(Size)], !IO).

%---------------------------------------------------------------------------%

semicanonicalize_types_in_initializer_for_c(Init0) = Init :-
    semicanonicalize_types_in_initializer_for_c(Init0, Init, _).

:- pred semicanonicalize_types_in_initializer_for_c(
    mlds_initializer::in, mlds_initializer::out, maybe_changed::out) is det.

semicanonicalize_types_in_initializer_for_c(Init0, Init, Changed) :-
    (
        Init0 = init_obj(Rval0),
        semicanonicalize_types_in_rval_for_c(Rval0, Rval, Changed),
        ( Changed = unchanged, Init = Init0
        ; Changed = changed, Init = init_obj(Rval)
        )
    ;
        Init0 = init_struct(StructType, Inits0),
        semicanonicalize_types_in_initializers_for_c(Inits0, Inits,
            unchanged, Changed),
        ( Changed = unchanged, Init = Init0
        ; Changed = changed, Init = init_struct(StructType, Inits)
        )
    ;
        Init0 = init_array(Inits0),
        semicanonicalize_types_in_initializers_for_c(Inits0, Inits,
            unchanged, Changed),
        ( Changed = unchanged, Init = Init0
        ; Changed = changed, Init = init_array(Inits)
        )
    ;
        Init0 = no_initializer,
        Init = Init0,
        Changed = unchanged
    ).

:- pred semicanonicalize_types_in_initializers_for_c(
    list(mlds_initializer)::in, list(mlds_initializer)::out,
    maybe_changed::in, maybe_changed::out) is det.

semicanonicalize_types_in_initializers_for_c([], [], !Changed).
semicanonicalize_types_in_initializers_for_c([Init0 | Inits0], [Init | Inits],
        !Changed) :-
    semicanonicalize_types_in_initializer_for_c(Init0, Init, InitChanged),
    ( InitChanged = unchanged
    ; InitChanged = changed, !:Changed = changed
    ),
    semicanonicalize_types_in_initializers_for_c(Inits0, Inits, !Changed).

:- pred semicanonicalize_types_in_rval_for_c(mlds_rval::in, mlds_rval::out,
    maybe_changed::out) is det.

semicanonicalize_types_in_rval_for_c(Rval0, Rval, Changed) :-
    (
        ( Rval0 = ml_lval(_)
        ; Rval0 = ml_const(_)
        ; Rval0 = ml_unbox(_, _)
        ; Rval0 = ml_mem_addr(_)
        ; Rval0 = ml_scalar_common(_)
        ; Rval0 = ml_scalar_common_addr(_)
        ; Rval0 = ml_vector_common_row_addr(_, _)
        ; Rval0 = ml_self(_)
        ),
        % Some of these rvals have no types that can be replaced
        % with other types, the others occur too rarely to be worth the bother.
        Rval = Rval0,
        Changed = unchanged
    ;
        Rval0 = ml_box(Type0, SubRvalA0),
        semicanonicalize_types_in_type_for_c(Type0, Type, ChangedT),
        semicanonicalize_types_in_rval_for_c(SubRvalA0, SubRvalA, ChangedA),
        ( if ChangedT = unchanged, ChangedA = unchanged then
            Changed = unchanged,
            Rval = Rval0
        else
            Changed = changed,
            Rval = ml_box(Type, SubRvalA)
        )
    ;
        Rval0 = ml_cast(Type0, SubRvalA0),
        semicanonicalize_types_in_type_for_c(Type0, Type, ChangedT),
        semicanonicalize_types_in_rval_for_c(SubRvalA0, SubRvalA, ChangedA),
        ( if ChangedT = unchanged, ChangedA = unchanged then
            Changed = unchanged,
            Rval = Rval0
        else
            Changed = changed,
            Rval = ml_cast(Type, SubRvalA)
        )
    ;
        Rval0 = ml_mkword(Tag, SubRvalA0),
        semicanonicalize_types_in_rval_for_c(SubRvalA0, SubRvalA, Changed),
        ( Changed = unchanged, Rval = Rval0
        ; Changed = changed,   Rval = ml_mkword(Tag, SubRvalA)
        )
    ;
        Rval0 = ml_unop(UnOp, SubRvalA0),
        semicanonicalize_types_in_rval_for_c(SubRvalA0, SubRvalA, Changed),
        ( Changed = unchanged, Rval = Rval0
        ; Changed = changed,   Rval = ml_unop(UnOp, SubRvalA)
        )
    ;
        Rval0 = ml_binop(BinOp, SubRvalA0, SubRvalB0),
        semicanonicalize_types_in_rval_for_c(SubRvalA0, SubRvalA, ChangedA),
        semicanonicalize_types_in_rval_for_c(SubRvalB0, SubRvalB, ChangedB),
        ( if ChangedA = unchanged, ChangedB = unchanged then
            Changed = unchanged,
            Rval = Rval0
        else
            Changed = changed,
            Rval = ml_binop(BinOp, SubRvalA, SubRvalB)
        )
    ).

:- pred semicanonicalize_types_in_type_for_c(mlds_type::in, mlds_type::out,
    maybe_changed::out) is det.

semicanonicalize_types_in_type_for_c(Type0, Type, Changed) :-
    (
        ( Type0 = mlds_mercury_array_type(_)
        ; Type0 = mlds_builtin_type_int(_)
        ; Type0 = mlds_builtin_type_float
        ; Type0 = mlds_builtin_type_char
        ; Type0 = mlds_builtin_type_string
        ; Type0 = mlds_native_bool_type
        ; Type0 = mlds_class_type(_)
        ; Type0 = mlds_mostly_generic_array_type(_)
        ; Type0 = mlds_func_type(_)
        ; Type0 = mlds_generic_type
        ; Type0 = mlds_generic_env_ptr_type
        ; Type0 = mlds_type_info_type
        ; Type0 = mlds_pseudo_type_info_type
        ; Type0 = mlds_cont_type(_)
        ; Type0 = mlds_commit_type
        ; Type0 = mlds_rtti_type(_)
        ; Type0 = mlds_tabling_type(_)
        ; Type0 = mlds_unknown_type
        ),
        Type = Type0,
        Changed = unchanged
    ;
        Type0 = mlds_ptr_type(SubType0),
        semicanonicalize_types_in_type_for_c(SubType0, SubType, Changed),
        ( Changed = unchanged, Type = Type0
        ; Changed = changed,   Type = mlds_ptr_type(SubType)
        )
    ;
        Type0 = mlds_array_type(SubType0),
        semicanonicalize_types_in_type_for_c(SubType0, SubType, Changed),
        ( Changed = unchanged, Type = Type0
        ; Changed = changed,   Type = mlds_array_type(SubType)
        )
    ;
        Type0 = mercury_nb_type(MerType0, CtorCat0),
        (
            CtorCat0 = ctor_cat_builtin(_),
            unexpected($pred, "ctor_cat_builtin")
        ;
            CtorCat0 = ctor_cat_variable,
            % The C type we generate is "MR_Box" regardless of MerType.
            Type = mlds_generic_type,
            Changed = changed
        ;
            CtorCat0 = ctor_cat_tuple,
            % The C type we generate is "MR_Tuple" regardless of MerType.
            MerType = void_type,
            ( if MerType = MerType0 then
                Type = Type0,
                Changed = unchanged
            else
                Type = mercury_nb_type(MerType, CtorCat0),
                Changed = changed
            )
        ;
            ( CtorCat0 = ctor_cat_higher_order
            ; CtorCat0 = ctor_cat_void
            ; CtorCat0 = ctor_cat_builtin_dummy
            ; CtorCat0 = ctor_cat_enum(_)
            ; CtorCat0 = ctor_cat_user(_)
            ; CtorCat0 = ctor_cat_system(_)
            ),
            % These all generate the C type "MR_Word" regardless of MerType.
            MerType = void_type,
            CtorCat = ctor_cat_void,
            ( if
                MerType = MerType0,
                CtorCat = CtorCat0
            then
                Type = Type0,
                Changed = unchanged
            else
                Type = mercury_nb_type(MerType, CtorCat),
                Changed = changed
            )
        )
    ;
        Type0 = mlds_foreign_type(_),
        % The C type we generate is "MR_Box" for both all foreign types
        % and for mlds_generic_type.
        Type = mlds_generic_type,
        Changed = changed
    ).

%---------------------------------------------------------------------------%

mlds_output_return_list(Stream, OutputPred, List, !IO) :-
    % Even though C does not support multiple return types, this case needs
    % to be handled for e.g. MLDS dumps when compiling to Java. We generate
    % an "#error" directive to make the error message clearer, but then we go
    % ahead and generate C-like pseudo-code for the purposes of MLDS dumps.
    io.write_string(Stream, "\n#error multiple return values\n", !IO),
    io.write_string(Stream, "\t{", !IO),
    write_out_list(OutputPred, ", ", List, Stream, !IO),
    io.write_string(Stream, "}", !IO).

%---------------------------------------------------------------------------%

mlds_output_cast(Opts, Stream, Type, !IO) :-
    io.write_string(Stream, "(", !IO),
    mlds_output_type(Opts, Type, Stream, !IO),
    io.write_string(Stream, ") ", !IO).

%---------------------------------------------------------------------------%
:- end_module ml_backend.mlds_to_c_type.
%---------------------------------------------------------------------------%
