%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2000-2012 The University of Melbourne.
% Copyright (C) 2013-2018 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% Output MLDS types in Java.
%
%---------------------------------------------------------------------------%

:- module ml_backend.mlds_to_java_type.
:- interface.

:- import_module backend_libs.
:- import_module backend_libs.rtti.
:- import_module ml_backend.mlds.
:- import_module ml_backend.mlds_to_java_util.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_type.

:- import_module io.
:- import_module list.

%---------------------------------------------------------------------------%

:- pred output_type_for_java(java_out_info::in, mlds_type::in,
    io::di, io::uo) is det.

    % type_to_string_for_java(Info, MLDS_Type, String, ArrayDims)
    %
    % Generate the Java name for a type. ArrayDims are the array dimensions to
    % be written after the type name, if any, in reverse order to that of Java
    % syntax where a non-zero integer represents a known array size and zero
    % represents an unknown array size.
    %
    % e.g. ArrayDims = [0, 3] represents the Java array `Object[3][]',
    % which should be read as `(Object[])[3]'.
    %
:- pred type_to_string_for_java(java_out_info::in, mlds_type::in,
    string::out, list(int)::out) is det.

    % Return is_array if the corresponding Java type is an array type.
    %
:- func type_is_array_for_java(mlds_type) = is_array.

    % hand_defined_type_for_java(Type, CtorCat, SubstituteName, ArrayDims):
    %
    % We need to handle type_info (etc.) types specially -- they get mapped
    % to types in the runtime rather than in private_builtin.
    %
:- pred hand_defined_type_for_java(mer_type::in, type_ctor_category::in,
    string::out, list(int)::out) is semidet.

:- pred boxed_type_to_string_for_java(java_out_info::in, mlds_type::in,
    string::out) is det.

%---------------------------------------------------------------------------%

    % java_builtin_type(MLDS_Type, JavaUnboxedType, JavaBoxedType,
    %   UnboxMethod):
    %
    % For a given Mercury type, check if this corresponds to a Java type
    % which has both unboxed (builtin) and boxed (class) versions, and if so,
    % return their names, and the name of the method to get the unboxed value
    % from the boxed type.
    %
:- pred java_builtin_type(mlds_type::in, string::out, string::out, string::out)
    is semidet.

:- pred java_primitive_foreign_language_type(generic_language_foreign_type::in,
    string::out, string::out, string::out, string::out) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module ml_backend.ml_type_gen.   % for ml_gen_type_name
:- import_module ml_backend.mlds_to_target_util.
:- import_module ml_backend.mlds_to_java_name.
:- import_module parse_tree.builtin_lib_types.

:- import_module bool.
:- import_module require.
:- import_module string.
:- import_module term.

%---------------------------------------------------------------------------%

output_type_for_java(Info, MLDS_Type, !IO) :-
    output_type_for_java_dims(Info, MLDS_Type, [], !IO).

:- pred output_type_for_java_dims(java_out_info::in, mlds_type::in,
    list(int)::in, io::di, io::uo) is det.

output_type_for_java_dims(Info, MLDS_Type, ArrayDims0, !IO) :-
    type_to_string_for_java(Info, MLDS_Type, String, ArrayDims),
    io.write_string(String, !IO),
    output_array_dimensions(ArrayDims ++ ArrayDims0, !IO).

type_to_string_for_java(Info, MLDS_Type, String, ArrayDims) :-
    (
        MLDS_Type = mercury_nb_type(Type, CtorCat),
        ( if
            % We need to handle type_info (etc.) types specially --
            % they get mapped to types in the runtime rather than
            % in private_builtin.
            hand_defined_type_for_java(Type, CtorCat,
                SubstituteName, ArrayDims0)
        then
            String = SubstituteName,
            ArrayDims = ArrayDims0
        else if
            % io.state and store.store
            CtorCat = ctor_cat_builtin_dummy
        then
            String = "/* builtin_dummy */ java.lang.Object",
            ArrayDims = []
        else if
            Type = c_pointer_type
        then
            % The c_pointer type is used in the c back-end as a generic way
            % to pass foreign types to automatically generated Compare and
            % Unify code. When compiling to Java we must instead use
            % java.lang.Object.
            String = "/* c_pointer */ java.lang.Object",
            ArrayDims = []
        else
            mercury_type_to_string_for_java(Info, Type, CtorCat, String,
                ArrayDims)
        )
    ;
        MLDS_Type = mlds_builtin_type_int(IntType),
        % Java lacks unsigned integers.
        (
            ( IntType = int_type_int
            ; IntType = int_type_uint
            ; IntType = int_type_int32
            ; IntType = int_type_uint32
            ),
            String = "int"
        ;
            ( IntType = int_type_int8
            ; IntType = int_type_uint8
            ),
            String = "byte"
        ;
            ( IntType = int_type_int16
            ; IntType = int_type_uint16
            ),
            String = "short"
        ;
            ( IntType = int_type_int64
            ; IntType = int_type_uint64
            ),
            String = "long"
        ),
        ArrayDims = []
    ;
        MLDS_Type = mlds_builtin_type_float,
        String = "double",
        ArrayDims = []
    ;
        MLDS_Type = mlds_builtin_type_string,
        String = "java.lang.String",
        ArrayDims = []
    ;
        MLDS_Type = mlds_builtin_type_char,
        % Java `char' is not large enough for code points,
        % so we must use `int'.
        String = "int",
        ArrayDims = []
    ;
        MLDS_Type = mlds_mercury_array_type(ElementType),
        ( if ElementType = mercury_nb_type(_, ctor_cat_variable) then
            % We can't use `java.lang.Object []', since we want a generic type
            % that is capable of holding any kind of array, including e.g.
            % `int []'. Java doesn't have any equivalent of .NET's System.Array
            % class, so we just use the universal base `java.lang.Object'.
            String = "/* Array */ java.lang.Object",
            ArrayDims = []
        else
            % For primitive element types we use arrays of the primitive type.
            % For non-primitive element types, we just use
            % `java.lang.Object []'. We used to use more specific types,
            % but then to create an array of the right type we need to use
            % reflection to determine the class of a representative element.
            % That doesn't work if the representative element is of a foreign
            % type, and has the value null.
            ( if java_builtin_type(ElementType, _, _, _) then
                type_to_string_for_java(Info, ElementType, String, ArrayDims0),
                ArrayDims = [0 | ArrayDims0]
            else
                String = "java.lang.Object",
                ArrayDims = [0]
            )
        )
    ;
        MLDS_Type = mlds_native_bool_type,
        String = "boolean",
        ArrayDims = []
    ;
        MLDS_Type = mlds_foreign_type(ForeignType),
        (
            ForeignType = java(java_type(Name)),
            String = Name,
            ArrayDims = []
        ;
            ForeignType = c(_),
            unexpected($pred, "c foreign_type")
        ;
            ForeignType = csharp(_),
            unexpected($pred, "csharp foreign_type")
        ;
            ForeignType = erlang(_),
            unexpected($pred, "erlang foreign_type")
        )
    ;
        MLDS_Type = mlds_class_type(mlds_class_id(Name, Arity, _ClassKind)),
        qual_class_name_to_string_for_java(Name, Arity, String),
        ArrayDims = []
    ;
        MLDS_Type = mlds_ptr_type(Type),
        % XXX Should we report an error here, if the type pointed to
        % is not a class type?
        type_to_string_for_java(Info, Type, String, ArrayDims)
    ;
        MLDS_Type = mlds_array_type(Type),
        type_to_string_for_java(Info, Type, String, ArrayDims0),
        ArrayDims = [0 | ArrayDims0]
    ;
        MLDS_Type = mlds_mostly_generic_array_type(_Type),
        Type = mlds_generic_type,
        type_to_string_for_java(Info, Type, String, ArrayDims0),
        ArrayDims = [0 | ArrayDims0]
    ;
        MLDS_Type = mlds_func_type(_FuncParams),
        String = "jmercury.runtime.MethodPtr",
        ArrayDims = []
    ;
        MLDS_Type = mlds_generic_type,
        String = "/* generic_type */ java.lang.Object",
        ArrayDims = []
    ;
        MLDS_Type = mlds_generic_env_ptr_type,
        String = "/* env_ptr */ java.lang.Object",
        ArrayDims = []
    ;
        MLDS_Type = mlds_type_info_type,
        String = "jmercury.runtime.TypeInfo",
        ArrayDims = []
    ;
        MLDS_Type = mlds_pseudo_type_info_type,
        String = "jmercury.runtime.PseudoTypeInfo",
        ArrayDims = []
    ;
        MLDS_Type = mlds_cont_type(_),
        % XXX Should this actually be a class that extends MethodPtr?
        String = "jmercury.runtime.MethodPtr",
        ArrayDims = []
    ;
        MLDS_Type = mlds_commit_type,
        String = "jmercury.runtime.Commit",
        ArrayDims = []
    ;
        MLDS_Type = mlds_rtti_type(RttiIdMaybeElement),
        rtti_id_maybe_element_java_type(RttiIdMaybeElement, String, IsArray),
        (
            IsArray = is_array,
            ArrayDims = [0]
        ;
            IsArray = not_array,
            ArrayDims = []
        )
    ;
        MLDS_Type = mlds_tabling_type(TablingId),
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

:- pred mercury_type_to_string_for_java(java_out_info::in, mer_type::in,
    type_ctor_category::in, string::out, list(int)::out) is det.

mercury_type_to_string_for_java(Info, Type, CtorCat, String, ArrayDims) :-
    (
        CtorCat = ctor_cat_builtin(_),
        unexpected($pred, "ctor_cat_builtin")
    ;
        CtorCat = ctor_cat_void,
        String = "builtin.Void_0",
        ArrayDims = []
    ;
        CtorCat = ctor_cat_variable,
        ( if
            Info ^ joi_output_generics = do_output_generics,
            Type = type_variable(TVar, kind_star),
            list.member(TVar, Info ^ joi_univ_tvars)
        then
            generic_tvar_to_string(TVar, String)
        else
            String = "java.lang.Object"
        ),
        ArrayDims = []
    ;
        CtorCat = ctor_cat_tuple,
        String = "/* tuple */ java.lang.Object",
        ArrayDims = [0]
    ;
        CtorCat = ctor_cat_higher_order,
        String = "/* closure */ java.lang.Object",
        ArrayDims = [0]
    ;
        CtorCat = ctor_cat_system(_),
        mercury_type_to_string_for_java(Info, Type,
            ctor_cat_user(cat_user_general), String, ArrayDims)
    ;
        ( CtorCat = ctor_cat_enum(_)
        ; CtorCat = ctor_cat_user(_)
        ; CtorCat = ctor_cat_builtin_dummy
        ),
        mercury_user_type_to_string_for_java(Info, Type, CtorCat, String,
            ArrayDims)
    ).

:- pred mercury_user_type_to_string_for_java(java_out_info::in, mer_type::in,
    type_ctor_category::in, string::out, list(int)::out) is det.

mercury_user_type_to_string_for_java(Info, Type, CtorCat, String, ArrayDims) :-
    type_to_ctor_and_args_det(Type, TypeCtor, ArgsTypes),
    ml_gen_type_name(TypeCtor, ClassName, ClassArity),
    ( if CtorCat = ctor_cat_enum(_) then
        ClassKind = mlds_enum
    else
        ClassKind = mlds_class
    ),
    MLDS_Type =
        mlds_class_type(mlds_class_id(ClassName, ClassArity, ClassKind)),
    type_to_string_for_java(Info, MLDS_Type, TypeString, ArrayDims),
    OutputGenerics = Info ^ joi_output_generics,
    (
        OutputGenerics = do_output_generics,
        generic_args_types_to_string_for_java(Info, ArgsTypes, GenericsString),
        String = TypeString ++ GenericsString
    ;
        OutputGenerics = do_not_output_generics,
        String = TypeString
    ).

:- pred generic_args_types_to_string_for_java(java_out_info::in,
    list(mer_type)::in, string::out) is det.

generic_args_types_to_string_for_java(Info, ArgsTypes, String) :-
    (
        ArgsTypes = [],
        String = ""
    ;
        ArgsTypes = [_ | _],
        ToString =
            ( pred(ArgType::in, ArgTypeString::out) is det :-
                ModuleInfo = Info ^ joi_module_info,
                MLDS_ArgType = mercury_type_to_mlds_type(ModuleInfo, ArgType),
                boxed_type_to_string_for_java(Info, MLDS_ArgType,
                    ArgTypeString)
            ),
        list.map(ToString, ArgsTypes, ArgsTypesStrings),
        ArgsTypesString = string.join_list(", ", ArgsTypesStrings),
        String = "<" ++ ArgsTypesString ++ ">"
    ).

%---------------------------------------------------------------------------%

type_is_array_for_java(Type) = IsArray :-
    ( if Type = mlds_array_type(_) then
        IsArray = is_array
    else if Type = mlds_mercury_array_type(_) then
        IsArray = is_array
    else if Type = mercury_nb_type(_, CtorCat) then
        IsArray = type_category_is_array(CtorCat)
    else if Type = mlds_rtti_type(RttiIdMaybeElement) then
        rtti_id_maybe_element_java_type(RttiIdMaybeElement,
            _JavaTypeName, IsArray)
    else
        IsArray = not_array
    ).

%---------------------------------------------------------------------------%

hand_defined_type_for_java(Type, CtorCat, SubstituteName, ArrayDims) :-
    require_complete_switch [CtorCat]
    (
        CtorCat = ctor_cat_system(CtorCatSystem),
        require_complete_switch [CtorCatSystem]
        (
            CtorCatSystem = cat_system_type_info,
            SubstituteName = "jmercury.runtime.TypeInfo_Struct",
            ArrayDims = []
        ;
            CtorCatSystem = cat_system_type_ctor_info,
            SubstituteName = "jmercury.runtime.TypeCtorInfo_Struct",
            ArrayDims = []
        ;
            CtorCatSystem = cat_system_typeclass_info,
            SubstituteName = "/* typeclass_info */ java.lang.Object",
            ArrayDims = [0]
        ;
            CtorCatSystem = cat_system_base_typeclass_info,
            SubstituteName = "/* base_typeclass_info */ java.lang.Object",
            ArrayDims = [0]
        )
    ;
        CtorCat = ctor_cat_user(CtorCatUser),
        require_complete_switch [CtorCatUser]
        (
            CtorCatUser = cat_user_general,
            ( if Type = type_desc_type then
                SubstituteName = "jmercury.runtime.TypeInfo_Struct"
            else if Type = pseudo_type_desc_type then
                SubstituteName = "jmercury.runtime.PseudoTypeInfo"
            else if Type = type_ctor_desc_type then
                SubstituteName = "jmercury.runtime.TypeCtorInfo_Struct"
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

boxed_type_to_string_for_java(Info, Type, String) :-
    ( if java_builtin_type(Type, _, JavaBoxedName, _) then
        String = JavaBoxedName
    else
        type_to_string_for_java(Info, Type, String0, ArrayDims),
        list.map(array_dimension_to_string, ArrayDims, RevBrackets),
        list.reverse(RevBrackets, Brackets),
        string.append_list([String0 | Brackets], String)
    ).

%---------------------------------------------------------------------------%

java_builtin_type(MLDS_Type, JavaUnboxedType, JavaBoxedType, UnboxMethod) :-
    require_complete_switch [MLDS_Type]
    (
        % NOTE: Java's `char' type is not large enough for code points so we
        % must use `int'.  Java has no unsigned types so we represent them
        % as `int'.
        ( MLDS_Type = mlds_builtin_type_char
        ; MLDS_Type = mlds_builtin_type_int(int_type_int)
        ; MLDS_Type = mlds_builtin_type_int(int_type_uint)
        ; MLDS_Type = mlds_builtin_type_int(int_type_int32)
        ; MLDS_Type = mlds_builtin_type_int(int_type_uint32)
        ),
        JavaUnboxedType = "int",
        JavaBoxedType = "java.lang.Integer",
        UnboxMethod = "intValue"
    ;
        ( MLDS_Type = mlds_builtin_type_int(int_type_int8)
        ; MLDS_Type = mlds_builtin_type_int(int_type_uint8)
        ),
        JavaUnboxedType = "byte",
        JavaBoxedType = "java.lang.Byte",
        UnboxMethod = "byteValue"
    ;
        ( MLDS_Type = mlds_builtin_type_int(int_type_int16)
        ; MLDS_Type = mlds_builtin_type_int(int_type_uint16)
        ),
        JavaUnboxedType = "short",
        JavaBoxedType = "java.lang.Short",
        UnboxMethod = "shortValue"
    ;
        ( MLDS_Type = mlds_builtin_type_int(int_type_int64)
        ; MLDS_Type = mlds_builtin_type_int(int_type_uint64)
        ),
        JavaUnboxedType = "long",
        JavaBoxedType = "java.lang.Long",
        UnboxMethod = "longValue"
    ;
        MLDS_Type = mlds_builtin_type_float,
        JavaUnboxedType = "double",
        JavaBoxedType = "java.lang.Double",
        UnboxMethod = "doubleValue"
    ;
        MLDS_Type = mlds_builtin_type_string,
        fail
    ;
        MLDS_Type = mlds_native_bool_type,
        JavaUnboxedType = "boolean",
        JavaBoxedType = "java.lang.Boolean",
        UnboxMethod = "booleanValue"
    ;
        MLDS_Type = mercury_nb_type(MerType, TypeCtorCat),
        require_complete_switch [MerType]
        (
            MerType = builtin_type(_),
            unexpected($pred, "mercury_nb_type but builtin_type")
        ;
            MerType = defined_type(_, _, _),
            require_complete_switch [TypeCtorCat] (
                % io.state and store.store(S) are dummy variables for which we
                % pass an arbitrary integer. For this reason they should have
                % the Java type `int'.
                TypeCtorCat = ctor_cat_builtin_dummy,
                JavaUnboxedType = "int",
                JavaBoxedType = "java.lang.Integer",
                UnboxMethod = "intValue"
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
        % Handle foreign types that map on to Java's primitive types specially
        % since we want to avoid boxing them where possible for performance
        % reasons.
        MLDS_Type = mlds_foreign_type(ForeignLangType),
        java_primitive_foreign_language_type(ForeignLangType, JavaUnboxedType,
            JavaBoxedType, UnboxMethod, _DefaultValue)
    ;
        ( MLDS_Type = mlds_mercury_array_type(_)
        ; MLDS_Type = mlds_cont_type(_)
        ; MLDS_Type = mlds_commit_type
        ; MLDS_Type = mlds_class_type(_)
        ; MLDS_Type = mlds_array_type(_)
        ; MLDS_Type = mlds_mostly_generic_array_type(_)
        ; MLDS_Type = mlds_ptr_type(_)
        ; MLDS_Type = mlds_func_type(_)
        ; MLDS_Type = mlds_generic_type
        ; MLDS_Type = mlds_generic_env_ptr_type
        ; MLDS_Type = mlds_type_info_type
        ; MLDS_Type = mlds_pseudo_type_info_type
        ; MLDS_Type = mlds_rtti_type(_)
        ; MLDS_Type = mlds_tabling_type(_)
        ),
        fail
    ;
        MLDS_Type = mlds_unknown_type,
        unexpected($file, $pred, "unknown typed")
    ).

java_primitive_foreign_language_type(ForeignLangType, PrimitiveType,
        BoxedType, UnboxMethod, DefaultValue) :-
    require_complete_switch [ForeignLangType]
    (
        ForeignLangType = java(java_type(JavaForeignType))
    ;
        ForeignLangType = c(_),
        unexpected($file, $pred, "foreign_type for C")
    ;
        ForeignLangType = csharp(_),
        unexpected($file, $pred, "foreign_type for C#")
    ;
        ForeignLangType = erlang(_),
        unexpected($file, $pred, "foreign_type for Erlang")
    ),
    PrimitiveType = string.strip(JavaForeignType),
    (
        PrimitiveType = "byte",
        BoxedType = "java.lang.Byte",
        UnboxMethod = "byteValue",
        DefaultValue = "0"
    ;
        PrimitiveType = "short",
        BoxedType = "java.lang.Short",
        UnboxMethod = "shortValue",
        DefaultValue = "0"
    ;
        PrimitiveType = "int",
        BoxedType = "java.lang.Integer",
        UnboxMethod = "intValue",
        DefaultValue = "0"
    ;
        PrimitiveType = "long",
        BoxedType = "java.lang.Long",
        UnboxMethod = "longValue",
        DefaultValue = "0"
    ;
        PrimitiveType = "float",
        BoxedType = "java.lang.Float",
        UnboxMethod = "floatValue",
        DefaultValue = "0"
    ;
        PrimitiveType = "double",
        BoxedType = "java.lang.Double",
        UnboxMethod = "doubleValue",
        DefaultValue = "0"
    ;
        PrimitiveType = "boolean",
        BoxedType = "java.lang.Boolean",
        UnboxMethod = "booleanValue",
        DefaultValue = "false"
    ;
        PrimitiveType = "char",
        BoxedType = "java.lang.Character",
        UnboxMethod = "charValue",
        DefaultValue = "'\\u0000'"
    ).


%---------------------------------------------------------------------------%
:- end_module ml_backend.mlds_to_java_type.
%---------------------------------------------------------------------------%
