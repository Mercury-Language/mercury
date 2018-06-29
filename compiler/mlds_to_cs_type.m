%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2010-2012 The University of Melbourne.
% Copyright (C) 2013-2018 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% Output MLDS types in C#.
%
%---------------------------------------------------------------------------%

:- module ml_backend.mlds_to_cs_type.
:- interface.

:- import_module backend_libs.
:- import_module backend_libs.rtti.
:- import_module ml_backend.mlds.
:- import_module ml_backend.mlds_to_cs_util.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_type.

:- import_module io.
:- import_module list.

%---------------------------------------------------------------------------%

:- pred output_type_for_csharp(csharp_out_info::in, mlds_type::in,
    io::di, io::uo) is det.

    % type_to_string_for_csharp(Info, MLDS_Type, String, ArrayDims)
    %
    % Generate the Java name for a type. ArrayDims are the array dimensions to
    % be written after the type name, if any, in reverse order to that of Java
    % syntax where a non-zero integer represents a known array size and zero
    % represents an unknown array size.
    %
    % e.g. ArrayDims = [0, 3] represents the Java array `Object[3][]',
    % which should be read as `(Object[])[3]'.
    %
    % XXX yet to check this for C#
    %
:- pred type_to_string_for_csharp(csharp_out_info::in, mlds_type::in,
    string::out, list(int)::out) is det.

:- pred boxed_type_to_string_for_csharp(csharp_out_info::in, mlds_type::in,
    string::out) is det.

:- func method_ptr_type_to_string(csharp_out_info, mlds_arg_types,
    mlds_return_types) = string.

:- pred csharp_builtin_type(mlds_type::in, string::out) is semidet.

:- func int_type_to_csharp_type(int_type) = string.

%---------------------------------------------------------------------------%

    % Return is_array if the corresponding C# type is an array type.
    %
:- func type_is_array_for_csharp(mlds_type) = is_array.

    % hand_defined_type_for_csharp(Type, CtorCat, SubstituteName, ArrayDims):
    %
    % We need to handle type_info (etc.) types specially -- they get mapped
    % to types in the runtime rather than in private_builtin.
    %
:- pred hand_defined_type_for_csharp(mer_type::in, type_ctor_category::in,
    string::out, list(int)::out) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module ml_backend.ml_type_gen.    % for ml_gen_type_name
:- import_module ml_backend.mlds_to_cs_name.
:- import_module ml_backend.mlds_to_target_util.
:- import_module parse_tree.builtin_lib_types.

:- import_module bool.
:- import_module maybe.
:- import_module require.
:- import_module string.
:- import_module term.

%---------------------------------------------------------------------------%

output_type_for_csharp(Info, MLDS_Type, !IO) :-
    output_type_for_csharp_dims(Info, MLDS_Type, [], !IO).

:- pred output_type_for_csharp_dims(csharp_out_info::in, mlds_type::in,
    list(int)::in, io::di, io::uo) is det.

output_type_for_csharp_dims(Info, MLDS_Type, ArrayDims0, !IO) :-
    type_to_string_for_csharp(Info, MLDS_Type, String, ArrayDims),
    io.write_string(String, !IO),
    output_array_dimensions(ArrayDims ++ ArrayDims0, !IO).

type_to_string_for_csharp(Info, MLDS_Type, String, ArrayDims) :-
    (
        MLDS_Type = mercury_type(Type, _, CtorCat),
        ( if
            % We need to handle type_info (etc.) types specially --
            % they get mapped to types in the runtime rather than
            % in private_builtin.
            hand_defined_type_for_csharp(Type, CtorCat,
                SubstituteName, ArrayDims0)
        then
            String = SubstituteName,
            ArrayDims = ArrayDims0
        else if
            % io.state and store.store
            CtorCat = ctor_cat_builtin_dummy
        then
            String = "/* builtin_dummy */ object",
            ArrayDims = []
        else if
            Type = c_pointer_type
        then
            % The c_pointer type is used in the C back-end as a generic way
            % to pass foreign types to automatically generated Compare and
            % Unify code. When compiling to C# we must instead use object.
            String = "/* c_pointer */ object",
            ArrayDims = []
        else
            mercury_type_to_string_for_csharp(Info, Type, CtorCat, String,
                ArrayDims)
        )
    ;
        MLDS_Type = mlds_mercury_array_type(ElementType),
        ( if ElementType = mercury_type(_, _, ctor_cat_variable) then
            String = "System.Array",
            ArrayDims = []
        else
            % For primitive element types we use arrays of the primitive type.
            % For non-primitive element types, we just use
            % `object []'. We used to use more specific types,
            % but then to create an array of the right type we need to use
            % reflection to determine the class of a representative element.
            % That doesn't work if the representative element is of a foreign
            % type, and has the value null.
            ( if csharp_builtin_type(ElementType, _) then
                type_to_string_for_csharp(Info, ElementType, String,
                    ArrayDims0),
                ArrayDims = [0 | ArrayDims0]
            else
                String = "object",
                ArrayDims = [0]
            )
        )
    ;
        MLDS_Type = mlds_native_int_type,
        String = "int",
        ArrayDims = []
    ;
        MLDS_Type = mlds_native_uint_type,
        String = "uint",
        ArrayDims = []
    ;
        MLDS_Type = mlds_native_float_type,
        String = "double",
        ArrayDims = []
    ;
        MLDS_Type = mlds_native_bool_type,
        String = "bool",
        ArrayDims = []
    ;
        MLDS_Type = mlds_native_char_type,
        % C# `char' not large enough for code points so we must use `int'.
        String = "int",
        ArrayDims = []
    ;
        MLDS_Type = mlds_foreign_type(ForeignType),
        (
            ForeignType = csharp(csharp_type(String)),
            ArrayDims = []
        ;
            ForeignType = c(_),
            unexpected($pred, "c foreign_type")
        ;
            ForeignType = java(_),
            unexpected($pred, "java foreign_type")
        ;
            ForeignType = erlang(_),
            unexpected($pred, "erlang foreign_type")
        )
    ;
        MLDS_Type = mlds_class_type(ClassId),
        ClassId = mlds_class_id(Name, Arity, _ClassKind),
        qual_class_name_to_string_for_csharp(Name, Arity, String),
        ArrayDims = []
    ;
        MLDS_Type = mlds_ptr_type(Type),
        % XXX Should we report an error here, if the type pointed to
        % is not a class type?
        type_to_string_for_csharp(Info, Type, String, ArrayDims)
    ;
        MLDS_Type = mlds_array_type(Type),
        type_to_string_for_csharp(Info, Type, String, ArrayDims0),
        ArrayDims = [0 | ArrayDims0]
    ;
        MLDS_Type = mlds_mostly_generic_array_type(_),
        Type = mlds_generic_type,
        type_to_string_for_csharp(Info, Type, String, ArrayDims0),
        ArrayDims = [0 | ArrayDims0]
    ;
        MLDS_Type = mlds_func_type(mlds_func_params(Args, RetTypes)),
        ArgTypes = list.map(func(mlds_argument(_, Type, _)) = Type, Args),
        String = method_ptr_type_to_string(Info, ArgTypes, RetTypes),
        ArrayDims = []
    ;
        MLDS_Type = mlds_generic_type,
        String = "/* generic_type */ object",
        ArrayDims = []
    ;
        MLDS_Type = mlds_generic_env_ptr_type,
        String = "/* env_ptr */ object",
        ArrayDims = []
    ;
        MLDS_Type = mlds_type_info_type,
        String = "runtime.TypeInfo",
        ArrayDims = []
    ;
        MLDS_Type = mlds_pseudo_type_info_type,
        String = "runtime.PseudoTypeInfo",
        ArrayDims = []
    ;
        MLDS_Type = mlds_cont_type(_),
        % XXX can we do better than this for C#?
        String = "/* cont_type */ object",
        ArrayDims = []
    ;
        MLDS_Type = mlds_commit_type,
        String = "runtime.Commit",
        ArrayDims = []
    ;
        MLDS_Type = mlds_rtti_type(RttiIdMaybeElement),
        rtti_id_maybe_element_csharp_type(RttiIdMaybeElement, String, IsArray),
        (
            IsArray = is_array,
            ArrayDims = [0]
        ;
            IsArray = not_array,
            ArrayDims = []
        )
    ;
        MLDS_Type = mlds_tabling_type(TablingId),
        % XXX C# is not Java
        tabling_id_java_type(TablingId, String, IsArray),
        (
            IsArray = is_array,
            ArrayDims = [0]
        ;
            IsArray = not_array,
            ArrayDims = []
        )
    ;
        MLDS_Type = mlds_unknown_type,
        unexpected($pred, "unknown type")
    ).

boxed_type_to_string_for_csharp(Info, Type, String) :-
    type_to_string_for_csharp(Info, Type, String0, ArrayDims),
    list.map(array_dimension_to_string, ArrayDims, RevBrackets),
    list.reverse(RevBrackets, Brackets),
    string.append_list([String0 | Brackets], String).

method_ptr_type_to_string(Info, ArgTypes, RetTypes) = String :-
    Arity = list.length(ArgTypes),
    NumRets = list.length(RetTypes),
    list.map(boxed_type_to_string_for_csharp(Info), ArgTypes, ArgTypesStrings),
    list.map(boxed_type_to_string_for_csharp(Info), RetTypes, RetTypesStrings),
    TypesString = string.join_list(", ", ArgTypesStrings ++ RetTypesStrings),
    string.format("runtime.MethodPtr%d_r%d<%s>",
        [i(Arity), i(NumRets), s(TypesString)], String).

:- pred mercury_type_to_string_for_csharp(csharp_out_info::in, mer_type::in,
    type_ctor_category::in, string::out, list(int)::out) is det.

mercury_type_to_string_for_csharp(Info, Type, CtorCat, String, ArrayDims) :-
    (
        CtorCat = ctor_cat_builtin(cat_builtin_char),
        % C# `char' not large enough for code points so we must use `int'.
        String = "int",
        ArrayDims = []
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_int(IntType)),
        String = int_type_to_csharp_type(IntType),
        ArrayDims = []
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_string),
        String = "string",
        ArrayDims = []
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_float),
        String = "double",
        ArrayDims = []
    ;
        CtorCat = ctor_cat_void,
        String = "builtin.Void_0",
        ArrayDims = []
    ;
        CtorCat = ctor_cat_variable,
        % XXX C# has generics
        String = "object",
        ArrayDims = []
    ;
        CtorCat = ctor_cat_tuple,
        String = "/* tuple */ object",
        ArrayDims = [0]
    ;
        CtorCat = ctor_cat_higher_order,
        String = "/* closure */ object",
        ArrayDims = [0]
    ;
        CtorCat = ctor_cat_system(_),
        mercury_type_to_string_for_csharp(Info, Type,
            ctor_cat_user(cat_user_general), String, ArrayDims)
    ;
        ( CtorCat = ctor_cat_enum(_)
        ; CtorCat = ctor_cat_user(_)
        ; CtorCat = ctor_cat_builtin_dummy
        ),
        mercury_user_type_to_string_csharp(Info, Type, CtorCat, String,
            ArrayDims)
    ).

%---------------------------------------------------------------------------%

csharp_builtin_type(Type, TargetType) :-
    require_complete_switch [Type]
    (
        Type = mlds_native_bool_type,
        TargetType = "bool"
    ;
        % C# `char' is not large enough for code points so we must use `int'.
        ( Type = mlds_native_int_type
        ; Type = mlds_native_char_type
        ),
        TargetType = "int"
    ;
        Type = mlds_native_uint_type,
        TargetType = "uint"
    ;
        Type = mlds_native_float_type,
        TargetType = "double"
    ;
        Type = mercury_type(MerType, _, TypeCtorCat),
        require_complete_switch [MerType] (
            MerType = builtin_type(BuiltinType),
            require_complete_switch [BuiltinType] (
                BuiltinType = builtin_type_char,
                TargetType = "int"
            ;
                BuiltinType = builtin_type_int(IntType),
                TargetType = int_type_to_csharp_type(IntType)
            ;
                BuiltinType = builtin_type_float,
                TargetType = "double"
            ;
                BuiltinType = builtin_type_string,
                fail
            )
    ;
            MerType = defined_type(_, _, _),
            require_complete_switch [TypeCtorCat] (
                % io.state and store.store(S) are dummy variables for which we
                % pass an arbitrary integer. For this reason they should have
                % the C# type `int'.
                TypeCtorCat = ctor_cat_builtin_dummy,
                TargetType = "int"
            ;
                ( TypeCtorCat = ctor_cat_builtin(_)
                ; TypeCtorCat = ctor_cat_higher_order
                ; TypeCtorCat = ctor_cat_tuple
                ; TypeCtorCat = ctor_cat_enum(_)
                ; TypeCtorCat = ctor_cat_variable
                ; TypeCtorCat = ctor_cat_system(_)
                ; TypeCtorCat = ctor_cat_void
                ; TypeCtorCat = ctor_cat_user(_)
                ),
                fail
            )
        ;
            ( MerType = type_variable(_, _)
            ; MerType = tuple_type(_, _)
            ; MerType = higher_order_type(_, _, _, _, _)
            ; MerType = apply_n_type(_, _, _)
            ; MerType = kinded_type(_, _)
            ),
            fail
        )
    ;
        ( Type = mlds_foreign_type(_)
        ; Type = mlds_mercury_array_type(_)
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
        fail
    ;
        Type = mlds_unknown_type,
        unexpected($file, $pred, "unknown typed")
    ).

int_type_to_csharp_type(int_type_int) = "int".
int_type_to_csharp_type(int_type_uint) = "uint".
int_type_to_csharp_type(int_type_int8) = "sbyte".
int_type_to_csharp_type(int_type_uint8) = "byte".
int_type_to_csharp_type(int_type_int16) = "short".
int_type_to_csharp_type(int_type_uint16) = "ushort".
int_type_to_csharp_type(int_type_int32) = "int".
int_type_to_csharp_type(int_type_uint32) = "uint".
int_type_to_csharp_type(int_type_int64) = "long".
int_type_to_csharp_type(int_type_uint64) = "ulong".

:- pred mercury_user_type_to_string_csharp(csharp_out_info::in, mer_type::in,
    type_ctor_category::in, string::out, list(int)::out) is det.

mercury_user_type_to_string_csharp(Info, Type, CtorCat, String, ArrayDims) :-
    type_to_ctor_and_args_det(Type, TypeCtor, ArgsTypes),
    ml_gen_type_name(TypeCtor, ClassName, ClassArity),
    ( if CtorCat = ctor_cat_enum(_) then
        ClassKind = mlds_enum
    else
        ClassKind = mlds_class
    ),
    ClassId = mlds_class_id(ClassName, ClassArity, ClassKind),
    MLDS_Type = mlds_class_type(ClassId),
    type_to_string_for_csharp(Info, MLDS_Type, TypeString, ArrayDims),
    OutputGenerics = Info ^ csoi_output_generics,
    (
        OutputGenerics = do_output_generics,
        generic_args_types_to_string_for_csharp(Info, ArgsTypes,
            GenericsString),
        String = TypeString ++ GenericsString
    ;
        OutputGenerics = do_not_output_generics,
        String = TypeString
    ).

:- pred generic_args_types_to_string_for_csharp(csharp_out_info::in,
    list(mer_type)::in, string::out) is det.

generic_args_types_to_string_for_csharp(Info, ArgsTypes, String) :-
    (
        ArgsTypes = [],
        String = ""
    ;
        ArgsTypes = [_ | _],
        ToString =
            ( pred(ArgType::in, ArgTypeString::out) is det :-
                ModuleInfo = Info ^ csoi_module_info,
                MLDS_ArgType = mercury_type_to_mlds_type(ModuleInfo, ArgType),
                boxed_type_to_string_for_csharp(Info, MLDS_ArgType,
                    ArgTypeString)
            ),
        list.map(ToString, ArgsTypes, ArgsTypesStrings),
        ArgsTypesString = string.join_list(", ", ArgsTypesStrings),
        String = "<" ++ ArgsTypesString ++ ">"
    ).

%---------------------------------------------------------------------------%

type_is_array_for_csharp(Type) = IsArray :-
    ( if Type = mlds_array_type(_) then
        IsArray = is_array
    else if Type = mlds_mostly_generic_array_type(_) then
        IsArray = is_array
    else if Type = mlds_mercury_array_type(_) then
        IsArray = is_array
    else if Type = mercury_type(_, _, CtorCat) then
        IsArray = type_category_is_array(CtorCat)
    else if Type = mlds_rtti_type(RttiIdMaybeElement) then
        rtti_id_maybe_element_csharp_type(RttiIdMaybeElement,
            _TypeName, IsArray)
    else
        IsArray = not_array
    ).

hand_defined_type_for_csharp(Type, CtorCat, SubstituteName, ArrayDims) :-
    require_complete_switch [CtorCat]
    (
        CtorCat = ctor_cat_system(CtorCatSystem),
        (
            CtorCatSystem = cat_system_type_info,
            SubstituteName = "runtime.TypeInfo_Struct",
            ArrayDims = []
        ;
            CtorCatSystem = cat_system_type_ctor_info,
            SubstituteName = "runtime.TypeCtorInfo_Struct",
            ArrayDims = []
        ;
            CtorCatSystem = cat_system_typeclass_info,
            SubstituteName = "/* typeclass_info */ object",
            ArrayDims = [0]
        ;
            CtorCatSystem = cat_system_base_typeclass_info,
            SubstituteName = "/* base_typeclass_info */ object",
            ArrayDims = [0]
        )
    ;
        CtorCat = ctor_cat_user(CtorCatUser),
        require_complete_switch [CtorCatUser]
        (
            CtorCatUser = cat_user_general,
            ( if Type = type_desc_type then
                SubstituteName = "runtime.TypeInfo_Struct"
            else if Type = pseudo_type_desc_type then
                SubstituteName = "runtime.PseudoTypeInfo"
            else if Type = type_ctor_desc_type then
                SubstituteName = "runtime.TypeCtorInfo_Struct"
            else
                fail
            ),
            ArrayDims = []
        ;
            ( CtorCatUser = cat_user_direct_dummy
            ; CtorCatUser = cat_user_abstract_dummy
            ; CtorCatUser = cat_user_notag
            ; CtorCatUser = cat_user_abstract_notag
            ),
            fail
        )
    ;
        ( CtorCat = ctor_cat_builtin(_)
        ; CtorCat = ctor_cat_builtin_dummy
        ; CtorCat = ctor_cat_enum(_)
        ; CtorCat = ctor_cat_higher_order
        ; CtorCat = ctor_cat_tuple
        ; CtorCat = ctor_cat_variable
        ; CtorCat = ctor_cat_void
        ),
        fail
    ).

%---------------------------------------------------------------------------%
:- end_module ml_backend.mlds_to_cs_type.
%---------------------------------------------------------------------------%
