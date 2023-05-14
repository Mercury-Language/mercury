%_---------------------------------------------------------------------------%
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

:- func type_to_string_for_c(mlds_to_c_opts, mlds_type) = string.
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
    % ZZZ to prefix_suffix
:- func type_prefix_for_c(mlds_to_c_opts, mlds_type) = string.
:- pred mlds_output_type_prefix(mlds_to_c_opts::in,
    io.text_output_stream::in, mlds_type::in, io::di, io::uo) is det.
:- func type_suffix_for_c_no_size(mlds_to_c_opts, mlds_type) = string.
:- pred mlds_output_type_suffix_no_size(mlds_to_c_opts::in,
    io.text_output_stream::in, mlds_type::in, io::di, io::uo) is det.
:- func type_suffix_for_c(mlds_to_c_opts, mlds_type,
    initializer_array_size) = string.
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
:- func return_list_to_string_for_c(list(string)) = string.
:- pred mlds_output_return_list(io.text_output_stream::in,
    pred(T, io.text_output_stream, io, io)::in(pred(in, in, di, uo) is det),
    list(T)::in, io::di, io::uo) is det.

%---------------------%

:- func cast_to_prefix_string_for_c(mlds_to_c_opts, mlds_type) = string.
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

type_to_string_for_c(Opts, Type) = TypeStr :-
    TypePrefix = type_prefix_for_c(Opts, Type),
    TypeSuffix = type_suffix_for_c(Opts, Type, no_size),
    % In the declaration of a type, as opposed to the declaration of a
    % variable, the variable name is not there, so we have just the prefix and
    % the suffix.
    TypeStr = TypePrefix ++ TypeSuffix.

mlds_output_type(Opts, Type, Stream, !IO) :-
    TypeStr = type_to_string_for_c(Opts, Type),
    io.write_string(Stream, TypeStr, !IO).

%---------------------%

/*
mlds_output_type_prefix(Opts, Stream, MLDS_Type, !IO) :-
*/
type_prefix_for_c(Opts, MLDS_Type) = TypePrefix :-
    % semicanonicalize_types_in_initializer_for_c relies on knowing
    % which MLDS_Types generate the same C types, so if this predicate
    % is updated so that two MLDS_Types that used to generate the same
    % C type no longer do so, you will need to update that function.
    (
        MLDS_Type = mercury_nb_type(_Type, TypeCategory),
        TypePrefix = mercury_type_prefix_for_c(TypeCategory)
    ;
        MLDS_Type = mlds_mercury_array_type(_ElemType),
        TypePrefix = "MR_ArrayPtr"
    ;
        MLDS_Type = mlds_builtin_type_int(IntType),
        (
            IntType = int_type_int,
            TypePrefix = "MR_Integer"
        ;
            IntType = int_type_uint,
            TypePrefix = "MR_Unsigned"
        ;
            IntType = int_type_int8,
            TypePrefix = "int8_t"
        ;
            IntType = int_type_uint8,
            TypePrefix = "uint8_t"
        ;
            IntType = int_type_int16,
            TypePrefix = "int16_t"
        ;
            IntType = int_type_uint16,
            TypePrefix = "uint16_t"
        ;
            IntType = int_type_int32,
            TypePrefix = "int32_t"
        ;
            IntType = int_type_uint32,
            TypePrefix = "uint32_t"
        ;
            IntType = int_type_int64,
            TypePrefix = "int64_t"
        ;
            IntType = int_type_uint64,
            TypePrefix = "uint64_t"
        )
    ;
        MLDS_Type = mlds_builtin_type_float,
        TypePrefix = "MR_Float"
    ;
        MLDS_Type = mlds_builtin_type_char,
        TypePrefix = "MR_Char"
    ;
        MLDS_Type = mlds_builtin_type_string,
        TypePrefix = "MR_String"
    ;
        MLDS_Type = mlds_native_bool_type,
        TypePrefix = "MR_bool"
    ;
        MLDS_Type = mlds_foreign_type(_ForeignType),
        % For binary compatibility with the --target asm back-end,
        % we need to output these as a generic type, rather than making
        % use of the C type name
        % XXX target asm no longer exists, so no longer need to do this.
        TypePrefix = "MR_Box"
    ;
        MLDS_Type = mlds_class_type(ClassId),
        ClassId = mlds_class_id(QualClassName, Arity, ClassKind),
        QualClassName = qual_class_name(ModuleName, _QualKind, ClassName),
        Qualifier = qualifier_to_string_for_c(ModuleName),
        MangledClassName = name_mangle(ClassName),
        (
            ClassKind = mlds_enum,
            % We cannot just use the enumeration type, since the enumeration
            % type's definition is not guaranteed to be in scope at this point.
            % (Fixing that would be somewhat complicated; it would require
            % writing enum definitions to a separate header file.) Also,
            % the enumeration might not be word-sized, which would cause
            % problems for e.g. `std_util.arg/2'. So we just use `MR_Integer',
            % and output the actual enumeration type as a comment.
            string.format("MR_Integer /* actually `enum %s__%s_%d_e' */",
                [s(Qualifier), s(MangledClassName), i(Arity)], TypePrefix)
        ;
            ( ClassKind = mlds_class
            ; ClassKind = mlds_interface
            ; ClassKind = mlds_struct
            ),
            % For struct types, it is OK to output an incomplete type, since
            % we do not use these types directly; we only use pointers to them.
            string.format("struct %s__%s_%d_s",
                [s(Qualifier), s(MangledClassName), i(Arity)], TypePrefix)
        )
    ;
        MLDS_Type = mlds_ptr_type(BaseType),
        BaseTypeStr = type_to_string_for_c(Opts, BaseType),
        string.format("%s *", [s(BaseTypeStr)], TypePrefix)
    ;
        MLDS_Type = mlds_array_type(BaseType),
        % Here we just output the element type. The "[]" goes in the type
        % suffix.
        TypePrefix = type_to_string_for_c(Opts, BaseType)
    ;
        MLDS_Type = mlds_mostly_generic_array_type(_),
        TypePrefix = type_to_string_for_c(Opts, mlds_generic_type)
    ;
        MLDS_Type = mlds_func_type(FuncParams),
        TypePrefix = func_type_prefix_for_c(Opts, FuncParams)
    ;
        MLDS_Type = mlds_generic_type,
        TypePrefix = "MR_Box"
    ;
        MLDS_Type = mlds_generic_env_ptr_type,
        TypePrefix = "void *"
    ;
        MLDS_Type = mlds_type_info_type,
        TypePrefix = "MR_TypeInfo"
    ;
        MLDS_Type = mlds_pseudo_type_info_type,
        TypePrefix = "MR_PseudoTypeInfo"
    ;
        MLDS_Type = mlds_cont_type(ArgTypes),
        (
            ArgTypes = [],
            TypePrefix = "MR_Cont"
        ;
            ArgTypes = [_ | _],
            % This case only happens for --nondet-copy-out.
            TypePrefix = "void MR_CALL (*"
        )
    ;
        MLDS_Type = mlds_commit_type,
        TypePrefix = "jmp_buf"
    ;
        MLDS_Type = mlds_rtti_type(RttiIdMaybeElement),
        rtti_id_maybe_element_c_type(RttiIdMaybeElement, TypePrefix, _IsArray)
    ;
        MLDS_Type = mlds_tabling_type(TablingId),
        tabling_id_c_type(TablingId, TypePrefix, _IsArray)
    ;
        MLDS_Type = mlds_unknown_type,
        unexpected($pred, "prefix has unknown type")
    ).

:- func mercury_type_prefix_for_c(type_ctor_category) = string.

/*
mlds_output_mercury_type_prefix(Stream, CtorCat, !IO) :-
*/
mercury_type_prefix_for_c(CtorCat) = TypePrefix :-
    % semicanonicalize_types_in_initializer_for_c relies on knowing
    % which MLDS_Types generate the same C types, so if this predicate
    % is updated so that two MLDS_Types that used to generate the same
    % C type no longer do so, you will need to update that function.
    (
        CtorCat = ctor_cat_builtin(_),
        unexpected($pred, "ctor_cat_builtin")
    ;
        CtorCat = ctor_cat_variable,
        TypePrefix = "MR_Box"
    ;
        CtorCat = ctor_cat_tuple,
        TypePrefix = "MR_Tuple"
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
        TypePrefix = "MR_Word"
    ).

:- func func_type_prefix_for_c(mlds_to_c_opts, mlds_func_params) = string.

/*
mlds_output_func_type_prefix(Opts, Stream, Params, !IO) :-
*/
func_type_prefix_for_c(Opts, Params) = TypePrefix :-
    Params = mlds_func_params(_Parameters, ReturnTypes),
    (
        ReturnTypes = [],
        ReturnTypeStr = "void"
    ;
        ReturnTypes = [ReturnType],
        ReturnTypeStr = type_to_string_for_c(Opts, ReturnType)
    ;
        ReturnTypes = [_, _ | _],
        ReturnTypeStrs = list.map(type_to_string_for_c(Opts), ReturnTypes),
        ReturnTypeStr = return_list_to_string_for_c(ReturnTypeStrs)
    ),
    % Note that mlds_func_type actually corresponds to a function _pointer_
    % type in C. This is necessary because function types in C are not first
    % class.
    string.format("%s MR_CALL (*", [s(ReturnTypeStr)], TypePrefix).

mlds_output_type_prefix(Opts, Stream, MLDS_Type, !IO) :-
    TypePrefix = type_prefix_for_c(Opts, MLDS_Type),
    io.write_string(Stream, TypePrefix, !IO).

%---------------------%

type_suffix_for_c_no_size(Opts, Type) = TypeStr :-
    TypeStr = type_suffix_for_c(Opts, Type, no_size).

mlds_output_type_suffix_no_size(Opts, Stream, Type, !IO) :-
    mlds_output_type_suffix(Opts, Stream, Type, no_size, !IO).

type_suffix_for_c(Opts, MLDS_Type, InitSize) = TypeSuffix :-
    % semicanonicalize_types_in_initializer_for_c relies on knowing
    % which MLDS_Types generate the same C types, so if this predicate
    % is updated so that two MLDS_Types that used to generate the same
    % C type no longer do so, you will need to update that function.
    (
        MLDS_Type = mlds_array_type(_),
        TypeSuffix = array_type_suffix_for_c(InitSize)
    ;
        MLDS_Type = mlds_mostly_generic_array_type(_),
        TypeSuffix = ""
    ;
        MLDS_Type = mlds_func_type(FuncParams),
        TypeSuffix = func_type_suffix_for_c(Opts, FuncParams)
    ;
        MLDS_Type = mlds_cont_type(ArgTypes),
        (
            ArgTypes = [],
            TypeSuffix = ""
        ;
            ArgTypes = [_ | _],
            % This case only happens for --nondet-copy-out.
            ArgTypeStrs = list.map(type_to_string_for_c(Opts), ArgTypes),
            ArgTypesStr = string.join_list(", ", ArgTypeStrs),
            % Add the type for the environment parameter, if needed.
            string.format(")(%s, void *)", [s(ArgTypesStr)], TypeSuffix)
        )
    ;
        MLDS_Type = mlds_rtti_type(RttiIdMaybeElement),
        IsArrayType = rtti_id_maybe_element_has_array_type(RttiIdMaybeElement),
        (
            IsArrayType = is_array,
            TypeSuffix = array_type_suffix_for_c(InitSize)
        ;
            IsArrayType = not_array,
            TypeSuffix = ""
        )
    ;
        MLDS_Type = mlds_tabling_type(TablingId),
        IsArrayType = tabling_id_has_array_type(TablingId),
        (
            IsArrayType = is_array,
            TypeSuffix = array_type_suffix_for_c(InitSize)
        ;
            IsArrayType = not_array,
            TypeSuffix = ""
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
        ),
        TypeSuffix = ""
    ).

mlds_output_type_suffix(Opts, Stream, MLDS_Type, ArraySize, !IO) :-
    TypeStr = type_suffix_for_c(Opts, MLDS_Type, ArraySize),
    io.write_string(Stream, TypeStr, !IO).

:- func func_type_suffix_for_c(mlds_to_c_opts, mlds_func_params) = string.

/*
mlds_output_func_type_suffix(Opts, Stream, Params, !IO) :-
*/
func_type_suffix_for_c(Opts, Params) = TypeSuffix :-
    Params = mlds_func_params(Args, _RetTypes),
    TypeSuffix = ")" ++ param_types_to_c(Opts, Args).

:- func param_types_to_c(mlds_to_c_opts, list(mlds_argument)) = string.

/*
mlds_output_param_types(Opts, Stream, Parameters, !IO) :-
*/
param_types_to_c(Opts, Args) = ArgsStr :-
    (
        Args = [],
        ArgsStr = "(void)"
    ;
        Args = [_ | _],
        ArgTypeStrs = list.map(param_type_to_string_for_c(Opts), Args),
        ArgTypesStr = string.join_list(", ", ArgTypeStrs),
        string.format("(%s)", [s(ArgTypesStr)], ArgsStr)
    ).

:- func param_type_to_string_for_c(mlds_to_c_opts, mlds_argument) = string.

param_type_to_string_for_c(Opts, Arg) = TypeStr :-
    Arg = mlds_argument(_Name, Type, _GCStmt),
    TypeStr = type_to_string_for_c(Opts, Type).

/* ZZZ
:- pred mlds_output_param_type(mlds_to_c_opts::in, mlds_argument::in,
    io.text_output_stream::in, io::di, io::uo) is det.

mlds_output_param_type(Opts, Arg, Stream, !IO) :-
    Arg = mlds_argument(_Name, Type, _GCStmt),
    mlds_output_type(Opts, Type, Stream, !IO).
*/

:- func array_type_suffix_for_c(initializer_array_size) = string.

array_type_suffix_for_c(InitSize) = ArraySuffix :-
    (
        InitSize = no_size,
        ArraySuffix = "[]"
    ;
        InitSize = array_size(Size0),
        % Standard ANSI/ISO C does not allow arrays of size 0.
        % But the MLDS does. To keep the C compiler happy, we therefore
        % convert zero-element MLDS arrays into one-element C arrays.
        int.max(Size0, 1, Size),
        string.format("[%d]", [i(Size)], ArraySuffix)
    ).

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

% ZZZ take an indent arg
return_list_to_string_for_c(ReturnTypeStrs) = ListStr :-
    ReturnTypesStr = string.join_list(", ", ReturnTypeStrs),
    string.format("\n#error multiple return values\n\t{%s}",
        [s(ReturnTypesStr)], ListStr).

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

cast_to_prefix_string_for_c(Opts, Type) = CastStr :-
    TypeStr = type_to_string_for_c(Opts, Type),
    string.format("(%s) ", [s(TypeStr)], CastStr).

mlds_output_cast(Opts, Stream, Type, !IO) :-
    CastStr = cast_to_prefix_string_for_c(Opts, Type),
    io.write_string(Stream, CastStr, !IO).

%---------------------------------------------------------------------------%
:- end_module ml_backend.mlds_to_c_type.
%---------------------------------------------------------------------------%
