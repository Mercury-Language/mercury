%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright (C) 2002-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: type_desc.m.
% Main author: fjh, zs.
% Stability: low.
% 
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module type_desc.
:- interface.

:- import_module list.

    % The `type_desc', `pseudo_type_desc' and `type_ctor_desc' types
    % provide access to type information.
    % A type_desc represents a type, e.g. `list(int)'.
    % A pseudo_type_desc represents a type that possibly contains type
    % variables, e.g. `list(T)'.
    % A type_ctor_desc represents a type constructor, e.g. `list/1'.
    %
:- type type_desc.
:- type pseudo_type_desc.
:- type type_ctor_desc.

    % The possibly nonground type represented by a pseudo_type_desc
    % is either a type constructor applied to zero or more
    % pseudo_type_descs, or a type variable. If the latter, the
    % type variable may be either universally or existentially quantified.
    % In either case, the type is identified by an integer, which has no
    % meaning beyond the fact that two type variables will be represented
    % by identical integers if and only if they are the same type variable.
    % Existentially quantified type variables may have type class
    % constraints placed on them, but for now we can't return these.
    %
:- type pseudo_type_rep
    --->    bound(type_ctor_desc, list(pseudo_type_desc))
    ;       univ_tvar(int)
    ;       exist_tvar(int).

:- pred pseudo_type_desc_is_ground(pseudo_type_desc::in) is semidet.

    % This function allows the caller to look into the structure
    % of the given pseudo_type_desc.
    %
:- func pseudo_type_desc_to_rep(pseudo_type_desc) = pseudo_type_rep.

    % Convert a type_desc, which by definition describes a ground type,
    % to a pseudo_type_desc.
    %
:- func type_desc_to_pseudo_type_desc(type_desc) = pseudo_type_desc.

    % Convert a pseudo_type_desc describing a ground type to a type_desc.
    % If the pseudo_type_desc describes a non-ground type, fail.
    %
:- func ground_pseudo_type_desc_to_type_desc(pseudo_type_desc) = type_desc
    is semidet.

    % Convert a pseudo_type_desc describing a ground type to a type_desc.
    % If the pseudo_type_desc describes a non-ground type, abort.
    %
:- func ground_pseudo_type_desc_to_type_desc_det(pseudo_type_desc) = type_desc
    is det.

    % The function type_of/1 returns a representation of the type
    % of its argument.
    %
    % (Note: it is not possible for the type of a variable to be an unbound
    % type variable; if there are no constraints on a type variable, then the
    % typechecker will use the type `void'. `void' is a special (builtin) type
    % that has no constructors. There is no way of creating an object of
    % type `void'. `void' is not considered to be a discriminated union, so
    % get_functor/5 and construct/3 will fail if used upon a value of
    % this type.)
    %
:- func type_of(T::unused) = (type_desc.type_desc::out) is det.

    % The predicate has_type/2 is basically an existentially typed inverse
    % to the function type_of/1. It constrains the type of the first argument
    % to be the type represented by the second argument.
    %
:- some [T] pred has_type(T::unused, type_desc.type_desc::in) is det.

    % type_name(Type) returns the name of the specified type
    % (e.g. type_name(type_of([2,3])) = "list:list(int)").
    % Any equivalence types will be fully expanded.
    % Builtin types (those defined in builtin.m) will not have
    % a module qualifier.
    %
:- func type_name(type_desc.type_desc) = string.

    % type_ctor_and_args(Type, TypeCtor, TypeArgs):
    %
    % True iff `TypeCtor' is a representation of the top-level type constructor
    % for `Type', and `TypeArgs' is a list of the corresponding type arguments
    % to `TypeCtor', and `TypeCtor' is not an equivalence type.
    %
    % For example, type_ctor_and_args(type_of([2,3]), TypeCtor, TypeArgs)
    % will bind `TypeCtor' to a representation of the type constructor list/1,
    % and will bind `TypeArgs' to the list `[Int]', where `Int' is a
    % representation of the type `int'.
    %
    % Note that the requirement that `TypeCtor' not be an equivalence type
    % is fulfilled by fully expanding any equivalence types. For example,
    % if you have a declaration `:- type foo == bar.', then
    % type_ctor_and_args/3 will always return a representation of type
    % constructor `bar/0', not `foo/0'. (If you don't want them expanded,
    %% you can use the reverse mode of make_type/2 instead.)
    %
:- pred type_ctor_and_args(type_desc.type_desc::in,
    type_desc.type_ctor_desc::out, list(type_desc.type_desc)::out) is det.

    % pseudo_type_ctor_and_args(Type, TypeCtor, TypeArgs):
    %
    % True iff `TypeCtor' is a representation of the top-level type constructor
    % for `Type', and `TypeArgs' is a list of the corresponding type arguments
    % to `TypeCtor', and `TypeCtor' is not an equivalence type.
    %
    % Similar to type_ctor_and_args, but works on pseudo_type_infos.
    % Fails if the input pseudo_type_info is a variable.
    %
:- pred pseudo_type_ctor_and_args(type_desc.pseudo_type_desc::in,
    type_desc.type_ctor_desc::out, list(type_desc.pseudo_type_desc)::out)
    is semidet.

    % type_ctor(Type) = TypeCtor :-
    %   type_ctor_and_args(Type, TypeCtor, _).
    %
:- func type_ctor(type_desc.type_desc) = type_desc.type_ctor_desc.

    % pseudo_type_ctor(Type) = TypeCtor :-
    %   pseudo_type_ctor_and_args(Type, TypeCtor, _).
    %
:- func pseudo_type_ctor(type_desc.pseudo_type_desc) =
    type_desc.type_ctor_desc is semidet.

    % type_args(Type) = TypeArgs :-
    %   type_ctor_and_args(Type, _, TypeArgs).
    %
:- func type_args(type_desc.type_desc) = list(type_desc.type_desc).

    % pseudo_type_args(Type) = TypeArgs :-
    %   pseudo_type_ctor_and_args(Type, _, TypeArgs).
    %
:- func pseudo_type_args(type_desc.pseudo_type_desc) =
    list(type_desc.pseudo_type_desc) is semidet.

    % type_ctor_name(TypeCtor) returns the name of specified type constructor.
    % (e.g. type_ctor_name(type_ctor(type_of([2,3]))) = "list").
    %
:- func type_ctor_name(type_desc.type_ctor_desc) = string.

    % type_ctor_module_name(TypeCtor) returns the module name of specified
    % type constructor.
    % (e.g. type_ctor_module_name(type_ctor(type_of(2))) = "builtin").
    %
:- func type_ctor_module_name(type_desc.type_ctor_desc) = string.

    % type_ctor_arity(TypeCtor) returns the arity of specified
    % type constructor.
    % (e.g. type_ctor_arity(type_ctor(type_of([2,3]))) = 1).
    %
:- func type_ctor_arity(type_desc.type_ctor_desc) = int.

    % type_ctor_name_and_arity(TypeCtor, ModuleName, TypeName, Arity) :-
    %   Name = type_ctor_name(TypeCtor),
    %   ModuleName = type_ctor_module_name(TypeCtor),
    %   Arity = type_ctor_arity(TypeCtor).
    %
:- pred type_ctor_name_and_arity(type_desc.type_ctor_desc::in,
    string::out, string::out, int::out) is det.

    % make_type(TypeCtor, TypeArgs) = Type:
    %
    % True iff `Type' is a type constructed by applying the type constructor
    % `TypeCtor' to the type arguments `TypeArgs'.
    %
    % Operationally, the forwards mode returns the type formed by applying
    % the specified type constructor to the specified argument types, or fails
    % if the length of TypeArgs is not the same as the arity of TypeCtor.
    % The reverse mode returns a type constructor and its argument types,
    % given a type_desc; the type constructor returned may be an equivalence
    % type (and hence this reverse mode of make_type/2 may be more useful
    % for some purposes than the type_ctor/1 function).
    %
:- func make_type(type_desc.type_ctor_desc, list(type_desc.type_desc)) =
    type_desc.type_desc.
:- mode make_type(in, in) = out is semidet.
:- mode make_type(out, out) = in is cc_multi.

    % det_make_type(TypeCtor, TypeArgs):
    %
    % Returns the type formed by applying the specified type constructor
    % to the specified argument types. Aborts if the length of `TypeArgs'
    % is not the same as the arity of `TypeCtor'.
    %
:- func det_make_type(type_desc.type_ctor_desc, list(type_desc.type_desc)) =
    type_desc.type_desc.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module require.
:- import_module string.

:- use_module    rtti_implementation.

:- pragma foreign_decl("C", "
#include ""mercury_heap.h"" /* for MR_incr_hp_msg() etc. */
#include ""mercury_misc.h"" /* for MR_fatal_error() */
#include ""mercury_string.h""   /* for MR_make_aligned_string() */
#include ""mercury_type_desc.h""
").

    % We need to call the rtti_implementation module -- so that we get the
    % dependencies right it's easiest to do it from Mercury.

:- pragma export(call_rtti_compare_type_infos(out, in, in),
    "ML_call_rtti_compare_type_infos").

:- pred call_rtti_compare_type_infos(comparison_result::out,
    rtti_implementation.type_info::in, rtti_implementation.type_info::in)
    is det.

call_rtti_compare_type_infos(Res, T1, T2) :-
    rtti_implementation.compare_type_infos(Res, T1, T2).

:- pragma foreign_code("C#", "

public static int MR_compare_type_info(object[] t1, object[] t2) {
    object[] res = null;

    mercury.type_desc.mercury_code.ML_call_rtti_compare_type_infos(
        ref res, t1, t2);
// currently comparison_results are always built using low-level data.
//#ifdef MR_HIGHLEVEL_DATA
//  return res.data_tag;
//#else
    return System.Convert.ToInt32(res[0]);
// #endif
}

public static void
special___Compare___type_ctor_desc_0_0(
    ref object[] result, object[] x, object[] y)
{
    mercury.runtime.Errors.SORRY(
        ""foreign code for comparing type_ctor_descs"");
}

public static bool
special___Unify___type_ctor_desc_0_0(object[] x, object[] y)
{
    mercury.runtime.Errors.SORRY(
        ""foreign code for unifying type_ctor_descs"");
    return false;
}

public static void
special___Compare___type_desc_0_0(
    ref object[] result, object[] x, object[] y)
{
    mercury.type_desc.mercury_code.ML_call_rtti_compare_type_infos(
        ref result, x, y);
}

public static bool
special___Unify___type_desc_0_0(object[] x, object[] y)
{
    return (MR_compare_type_info(x, y) == 0);
}

public static void
special___Compare___pseudo_type_desc_0_0(
    ref object[] result, object[] x, object[] y)
{
    mercury.runtime.Errors.SORRY(
        ""foreign code for comparing pseudo_type_desc"");
}

public static bool
special___Unify___pseudo_type_desc_0_0(object[] x, object[] y)
{
    mercury.runtime.Errors.SORRY(
        ""foreign code for unifying pseudo_type_desc"");
    return false;
}

").

%-----------------------------------------------------------------------------%

% Code for type manipulation.

pseudo_type_desc_is_ground(PseudoTypeDesc) :-
    pseudo_type_ctor_and_args(PseudoTypeDesc, _TypeCtor, ArgPseudos),
    list.all_true(pseudo_type_desc_is_ground, ArgPseudos).

pseudo_type_desc_to_rep(PseudoTypeDesc) = PseudoTypeRep :-
    ( pseudo_type_ctor_and_args(PseudoTypeDesc, TypeCtor, ArgPseudos) ->
        PseudoTypeRep = bound(TypeCtor, ArgPseudos)
    ; is_exist_pseudo_type_desc(PseudoTypeDesc, UnivNum) ->
        PseudoTypeRep = exist_tvar(UnivNum)
    ; is_univ_pseudo_type_desc(PseudoTypeDesc, UnivNum) ->
        PseudoTypeRep = univ_tvar(UnivNum)
    ;
        error("pseudo_type_desc_to_rep: internal error")
    ).

:- pred is_univ_pseudo_type_desc(pseudo_type_desc::in, int::out) is semidet.

:- pragma foreign_proc("C",
    is_univ_pseudo_type_desc(PseudoTypeDesc::in, TypeVarNum::out),
    [will_not_call_mercury, thread_safe, promise_pure, will_not_modify_trail],
"
    MR_PseudoTypeInfo   pseudo_type_info;

    pseudo_type_info = (MR_PseudoTypeInfo) PseudoTypeDesc;
    if (MR_PSEUDO_TYPEINFO_IS_VARIABLE(pseudo_type_info) &&
        MR_TYPE_VARIABLE_IS_UNIV_QUANT(pseudo_type_info))
    {
        TypeVarNum = (MR_Integer) pseudo_type_info;
        SUCCESS_INDICATOR = MR_TRUE;
    } else {
        SUCCESS_INDICATOR = MR_FALSE;
    }
").

is_univ_pseudo_type_desc(_PseudoTypeDesc, -1) :-
    % The backends in which we use this definition of this predicate
    % don't yet support pseudo_type_descs.
    semidet_fail.

:- pred is_exist_pseudo_type_desc(pseudo_type_desc::in, int::out) is semidet.

:- pragma foreign_proc("C",
    is_exist_pseudo_type_desc(PseudoTypeDesc::in, TypeVarNum::out),
    [will_not_call_mercury, thread_safe, promise_pure, will_not_modify_trail],
"
    MR_PseudoTypeInfo   pseudo_type_info;

    pseudo_type_info = (MR_PseudoTypeInfo) PseudoTypeDesc;
    if (MR_PSEUDO_TYPEINFO_IS_VARIABLE(pseudo_type_info) &&
        MR_TYPE_VARIABLE_IS_EXIST_QUANT(pseudo_type_info))
    {
        TypeVarNum = (MR_Integer) pseudo_type_info;
        SUCCESS_INDICATOR = MR_TRUE;
    } else {
        SUCCESS_INDICATOR = MR_FALSE;
    }
").

is_exist_pseudo_type_desc(_PseudoTypeDesc, -1) :-
    % The backends in which we use this definition of this predicate
    % don't yet support pseudo_type_descs.
    semidet_fail.

:- pragma foreign_proc("C",
    type_desc_to_pseudo_type_desc(TypeDesc::in) = (PseudoTypeDesc::out),
    [will_not_call_mercury, thread_safe, promise_pure, will_not_modify_trail],
"
    PseudoTypeDesc = TypeDesc;
").

type_desc_to_pseudo_type_desc(_TypeDesc) = _PseudoTypeDesc :-
    % The backends in which we use this definition of this predicate
    % don't yet support pseudo_type_descs.
    private_builtin.sorry("type_desc_to_pseudo_type_desc").

ground_pseudo_type_desc_to_type_desc(PseudoTypeDesc) = TypeDesc :-
    ( pseudo_type_desc_is_ground(PseudoTypeDesc) ->
        private_builtin.unsafe_type_cast(PseudoTypeDesc, TypeDesc)
    ;
        fail
    ).

ground_pseudo_type_desc_to_type_desc_det(PseudoTypeDesc) = TypeDesc :-
    ( pseudo_type_desc_is_ground(PseudoTypeDesc) ->
        private_builtin.unsafe_type_cast(PseudoTypeDesc, TypeDesc)
    ;
        error("ground_pseudo_type_desc_to_type_desc_det: not ground")
    ).

:- pragma foreign_proc("C",
    type_of(_Value::unused) = (TypeInfo::out),
    [will_not_call_mercury, thread_safe, promise_pure, will_not_modify_trail],
"{
    TypeInfo = TypeInfo_for_T;

    /*
    ** We used to collapse equivalences for efficiency here, but that's not
    ** always desirable, due to the reverse mode of make_type/2, and efficiency
    ** of type_infos probably isn't very important anyway.
    */
#if 0
    MR_save_transient_registers();
    TypeInfo = (MR_Word) MR_collapse_equivalences(
        (MR_TypeInfo) TypeInfo_for_T);
    MR_restore_transient_registers();
#endif

}").

:- pragma foreign_proc("C#",
    type_of(_Value::unused) = (TypeInfo::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    TypeInfo = TypeInfo_for_T;
").

:- pragma foreign_proc("Java",
    type_of(_Value::unused) = (TypeInfo::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    TypeInfo = new mercury.type_desc.Type_desc_0(
        (mercury.runtime.TypeInfo_Struct) TypeInfo_for_T);
").

:- pragma foreign_proc("C",
    has_type(_Arg::unused, TypeInfo::in),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    TypeInfo_for_T = TypeInfo;
").

:- pragma foreign_proc("C#",
    has_type(_Arg::unused, TypeInfo::in),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    TypeInfo_for_T = TypeInfo;
").

:- pragma foreign_proc("Java",
    has_type(_Arg::unused, TypeInfo::in),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    TypeInfo_for_T = ((mercury.type_desc.Type_desc_0) TypeInfo).struct;
").


% Export this function in order to use it in runtime/mercury_trace_external.c
:- pragma export(type_name(in) = out, "ML_type_name").

type_name(Type) = TypeName :-
    type_ctor_and_args(Type, TypeCtor, ArgTypes),
    type_ctor_name_and_arity(TypeCtor, ModuleName, Name, Arity),
    ( Arity = 0 ->
        UnqualifiedTypeName = Name
    ;
        ( ModuleName = "builtin", Name = "func" ->
            IsFunc = yes
        ;
            IsFunc = no
        ),
        (
            ModuleName = "builtin", Name = "{}"
        ->
            type_arg_names(ArgTypes, IsFunc, ArgTypeNames),
            list.append(ArgTypeNames, ["}"], TypeStrings0),
            TypeStrings = ["{" | TypeStrings0],
            string.append_list(TypeStrings, UnqualifiedTypeName)
        ;
            IsFunc = yes,
            ArgTypes = [FuncRetType]
        ->
            FuncRetTypeName = type_name(FuncRetType),
            string.append_list(["((func) = ", FuncRetTypeName, ")"],
                UnqualifiedTypeName)
        ;
            type_arg_names(ArgTypes, IsFunc, ArgTypeNames),
            (
                IsFunc = no,
                list.append(ArgTypeNames, [")"], TypeStrings0)
            ;
                IsFunc = yes,
                TypeStrings0 = ArgTypeNames
            ),
            TypeNameStrings = [Name, "(" | TypeStrings0],
            string.append_list(TypeNameStrings, UnqualifiedTypeName)
        )
    ),
    ( ModuleName = "builtin" ->
        TypeName = UnqualifiedTypeName
    ;
        string.append_list([ModuleName, ".", UnqualifiedTypeName], TypeName)
    ).

    % Turn the types into a list of strings representing an argument list,
    % adding commas as separators as required.  For example:
    %   ["TypeName1", ",", "TypeName2"]
    % If formatting a function type, we close the parentheses around
    % the function's input parameters, e.g.
    %   ["TypeName1", ",", "TypeName2", ") = ", "ReturnTypeName"]
    % It is the caller's reponsibility to add matching parentheses.
    %
:- pred type_arg_names(list(type_desc.type_desc)::in, bool::in,
    list(string)::out) is det.

type_arg_names([], _, []).
type_arg_names([Type | Types], IsFunc, ArgNames) :-
    Name = type_name(Type),
    ( Types = [] ->
        ArgNames = [Name]
    ; IsFunc = yes, Types = [FuncReturnType] ->
        FuncReturnName = type_name(FuncReturnType),
        ArgNames = [Name, ") = ", FuncReturnName]
    ;
        type_arg_names(Types, IsFunc, Names),
        ArgNames = [Name, ", " | Names]
    ).

type_args(Type) = ArgTypes :-
    type_ctor_and_args(Type, _TypeCtor, ArgTypes).

pseudo_type_args(PseudoType) = ArgPseudoTypes :-
    pseudo_type_ctor_and_args(PseudoType, _TypeCtor, ArgPseudoTypes).

type_ctor_name(TypeCtor) = Name :-
    type_ctor_name_and_arity(TypeCtor, _ModuleName, Name, _Arity).

type_ctor_module_name(TypeCtor) = ModuleName :-
    type_ctor_name_and_arity(TypeCtor, ModuleName, _Name, _Arity).

type_ctor_arity(TypeCtor) = Arity :-
    type_ctor_name_and_arity(TypeCtor, _ModuleName, _Name, Arity).

det_make_type(TypeCtor, ArgTypes) = Type :-
    ( make_type(TypeCtor, ArgTypes) = NewType ->
        Type = NewType
    ;
        error("det_make_type/2: make_type/2 failed (wrong arity)")
    ).

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    type_ctor(TypeInfo::in) = (TypeCtor::out),
    [will_not_call_mercury, thread_safe, promise_pure, will_not_modify_trail],
"{
    MR_TypeCtorInfo type_ctor_info;
    MR_TypeInfo     type_info;

    MR_save_transient_registers();
    type_info = MR_collapse_equivalences((MR_TypeInfo) TypeInfo);
    MR_restore_transient_registers();

    type_ctor_info = MR_TYPEINFO_GET_TYPE_CTOR_INFO(type_info);

    TypeCtor = (MR_Word) MR_make_type_ctor_desc(type_info, type_ctor_info);
}").

:- pragma foreign_proc("C",
    pseudo_type_ctor(PseudoTypeInfo::in) = (TypeCtor::out),
    [will_not_call_mercury, thread_safe, promise_pure, will_not_modify_trail],
"{
    MR_TypeCtorInfo     type_ctor_info;
    MR_PseudoTypeInfo   pseudo_type_info;

    MR_save_transient_registers();
    pseudo_type_info = MR_collapse_equivalences_pseudo(
        (MR_PseudoTypeInfo) PseudoTypeInfo);
    MR_restore_transient_registers();

    if (MR_PSEUDO_TYPEINFO_IS_VARIABLE(pseudo_type_info)) {
        SUCCESS_INDICATOR = MR_FALSE;
    } else {
        type_ctor_info = MR_PSEUDO_TYPEINFO_GET_TYPE_CTOR_INFO(
            pseudo_type_info);
        TypeCtor = (MR_Word) MR_make_type_ctor_desc_pseudo(pseudo_type_info,
            type_ctor_info);
        SUCCESS_INDICATOR = MR_TRUE;
    }
}").

:- pragma foreign_proc("C",
    type_ctor_and_args(TypeDesc::in, TypeCtorDesc::out, ArgTypes::out),
    [will_not_call_mercury, thread_safe, promise_pure, will_not_modify_trail],
"{
    MR_TypeCtorDesc type_ctor_desc;
    MR_TypeInfo     type_info;

    MR_save_transient_registers();

    type_info = (MR_TypeInfo) TypeDesc;
    MR_type_ctor_and_args(type_info, MR_TRUE, &type_ctor_desc, &ArgTypes);
    TypeCtorDesc = (MR_Word) type_ctor_desc;

    MR_restore_transient_registers();
}").

:- pragma foreign_proc("Java",
    type_ctor_and_args(TypeDesc::in, TypeCtorDesc::out, ArgTypes::out),
    [may_call_mercury, thread_safe, promise_pure, terminates],
"
    java.lang.Object [] result =
        mercury.rtti_implementation.type_ctor_and_args_3_p_0(
        ((mercury.type_desc.Type_desc_0) TypeDesc).struct);

    TypeCtorDesc = new Type_ctor_desc_0((TypeCtorInfo_Struct) result[0]);
    ArgTypes = result[1];

    // Convert list from TypeInfo_Struct to type_desc_0
    mercury.list.List_1 type_list = (mercury.list.List_1) ArgTypes;
    while (type_list.data_tag == 1) {
        ((mercury.list.List_1.F_cons_2) type_list).F1 =
            new mercury.type_desc.Type_desc_0(
            (TypeInfo_Struct) ((mercury.list.List_1.F_cons_2) type_list).F1);
        type_list = (mercury.list.List_1)
            ((mercury.list.List_1.F_cons_2) type_list).F2;
    }
").

type_ctor_and_args(TypeDesc::in, TypeCtorDesc::out, ArgTypes::out) :-
    rtti_implementation.type_ctor_and_args(
        rtti_implementation.unsafe_cast(TypeDesc),
        TypeCtorDesc0, ArgTypes0),
    TypeCtorDesc = rtti_implementation.unsafe_cast(TypeCtorDesc0),
    ArgTypes = rtti_implementation.unsafe_cast(ArgTypes0).

:- pragma foreign_proc("C",
    pseudo_type_ctor_and_args(PseudoTypeDesc::in, TypeCtorDesc::out,
        ArgPseudoTypeInfos::out),
    [will_not_call_mercury, thread_safe, promise_pure, will_not_modify_trail],
"{
    MR_TypeCtorDesc     type_ctor_desc;
    MR_PseudoTypeInfo   pseudo_type_info;
    MR_bool             success;

    pseudo_type_info = (MR_PseudoTypeInfo) PseudoTypeDesc;
    MR_save_transient_registers();
    success = MR_pseudo_type_ctor_and_args(pseudo_type_info, MR_TRUE,
        &type_ctor_desc, &ArgPseudoTypeInfos);
    TypeCtorDesc = (MR_Word) type_ctor_desc;
    MR_restore_transient_registers();
    SUCCESS_INDICATOR = success;
}").

pseudo_type_ctor_and_args(_, _, _) :-
    % The non-C backends can't (yet) handle pseudo_type_infos.
    private_builtin.sorry("pseudo_type_ctor_and_args").

% This is the forwards mode of make_type/2: given a type constructor and
% a list of argument types, check that the length of the argument types
% matches the arity of the type constructor, and if so, use the type
% constructor to construct a new type with the specified arguments.

:- pragma promise_pure(make_type/2).
:- pragma foreign_proc("C",
    make_type(TypeCtorDesc::in, ArgTypes::in) = (TypeDesc::out),
    [will_not_call_mercury, thread_safe, will_not_modify_trail],
"{
    MR_TypeCtorDesc type_ctor_desc;
    MR_TypeCtorInfo type_ctor_info;
    MR_Word     arg_type;
    int     list_length;
    int     arity;

    type_ctor_desc = (MR_TypeCtorDesc) TypeCtorDesc;

    if (MR_TYPECTOR_DESC_IS_VARIABLE_ARITY(type_ctor_desc)) {
        arity = MR_TYPECTOR_DESC_GET_VA_ARITY(type_ctor_desc);
    } else {
        type_ctor_info =
            MR_TYPECTOR_DESC_GET_FIXED_ARITY_TYPE_CTOR_INFO(type_ctor_desc);
        arity = type_ctor_info->MR_type_ctor_arity;
    }

    arg_type = ArgTypes;
    for (list_length = 0; ! MR_list_is_empty(arg_type); list_length++) {
        arg_type = MR_list_tail(arg_type);
    }

    if (list_length != arity) {
        SUCCESS_INDICATOR = MR_FALSE;
    } else {
        MR_save_transient_registers();
        TypeDesc = (MR_Word) MR_make_type(arity, type_ctor_desc, ArgTypes);
        MR_restore_transient_registers();
        SUCCESS_INDICATOR = MR_TRUE;
    }
}").

    /*
    ** This is the reverse mode of make_type: given a type,
    ** split it up into a type constructor and a list of
    ** arguments.
    */

:- pragma foreign_proc("C",
    make_type(TypeCtorDesc::out, ArgTypes::out) = (TypeDesc::in),
    [will_not_call_mercury, thread_safe, will_not_modify_trail],
"{
    MR_TypeCtorDesc type_ctor_desc;
    MR_TypeInfo     type_info;

    MR_save_transient_registers();

    type_info = (MR_TypeInfo) TypeDesc;
    MR_type_ctor_and_args(type_info, MR_FALSE, &type_ctor_desc, &ArgTypes);
    TypeCtorDesc = (MR_Word) type_ctor_desc;

    MR_restore_transient_registers();
}").

:- pragma foreign_proc("C",
    type_ctor_name_and_arity(TypeCtorDesc::in, TypeCtorModuleName::out,
        TypeCtorName::out, TypeCtorArity::out),
    [will_not_call_mercury, thread_safe, promise_pure, will_not_modify_trail],
"{
    MR_TypeCtorDesc type_ctor_desc;

    type_ctor_desc = (MR_TypeCtorDesc) TypeCtorDesc;

    if (MR_TYPECTOR_DESC_IS_VARIABLE_ARITY(type_ctor_desc)) {
        TypeCtorModuleName = (MR_String) (MR_Word)
            MR_TYPECTOR_DESC_GET_VA_MODULE_NAME(type_ctor_desc);
        TypeCtorName = (MR_String) (MR_Word)
            MR_TYPECTOR_DESC_GET_VA_NAME(type_ctor_desc);
        TypeCtorArity = MR_TYPECTOR_DESC_GET_VA_ARITY(type_ctor_desc);
    } else {
        MR_TypeCtorInfo type_ctor_info;

        type_ctor_info =
            MR_TYPECTOR_DESC_GET_FIXED_ARITY_TYPE_CTOR_INFO(type_ctor_desc);

        /*
        ** We cast away the const-ness of the module and type names,
        ** because MR_String is defined as char *, not const char *.
        */

        TypeCtorModuleName = (MR_String) (MR_Integer)
            MR_type_ctor_module_name(type_ctor_info);
        TypeCtorName = (MR_String) (MR_Integer)
            MR_type_ctor_name(type_ctor_info);
        TypeCtorArity = type_ctor_info->MR_type_ctor_arity;
    }
}").

:- pragma foreign_proc("Java",
    type_ctor_name_and_arity(TypeCtorDesc::in, TypeCtorModuleName::out,
        TypeCtorName::out, TypeCtorArity::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    Object[] result = mercury.rtti_implementation.
        type_ctor_name_and_arity_4_p_0(
            ((Type_ctor_desc_0) TypeCtorDesc).struct);

    TypeCtorModuleName = (java.lang.String) result[0];
    TypeCtorName = (java.lang.String) result[1];
    TypeCtorArity = ((java.lang.Integer) result[2]).intValue();
").

type_ctor_name_and_arity(TypeCtorDesc::in, ModuleName::out,
        TypeCtorName::out, TypeCtorArity::out) :-
    rtti_implementation.type_ctor_name_and_arity(
        rtti_implementation.unsafe_cast(TypeCtorDesc),
        ModuleName, TypeCtorName, TypeCtorArity).

%-----------------------------------------------------------------------------%

    % This function returns the type_info for the builtin type "typeinfo"
    % itself. It is intended for use from C code, since Mercury code can access
    % this type_info easily enough even without this predicate.
    %
    % XXX This code relies on the type "type_desc.type_desc" being the
    % same type as the builtin type "typeinfo".
    %
:- func get_type_info_for_type_info = type_desc.type_desc.

:- pragma export(get_type_info_for_type_info = out,
    "ML_get_type_info_for_type_info").

get_type_info_for_type_info = TypeDesc :-
    Type = type_of(1),
    TypeDesc = type_of(Type).

%-----------------------------------------------------------------------------%

:- pragma foreign_code("Java", "
    public static class Type_desc_0 {
        public mercury.runtime.TypeInfo_Struct struct;

        public Type_desc_0(mercury.runtime.TypeInfo_Struct init) {
            struct = init;
        }
    }
    public static class Type_ctor_desc_0 {
        public mercury.runtime.TypeCtorInfo_Struct struct;

        public Type_ctor_desc_0(
            mercury.runtime.TypeCtorInfo_Struct init)
        {
            struct = init;
        }
    }

    public static boolean
    __Unify____type_desc_0_0(mercury.type_desc.Type_desc_0 x,
        mercury.type_desc.Type_desc_0 y)
    {
        return x.struct.unify(y.struct);
    }

    public static boolean
    __Unify____type_ctor_desc_0_0(mercury.type_desc.Type_ctor_desc_0 x,
        mercury.type_desc.Type_ctor_desc_0 y)
    {
        // stub only
        throw new java.lang.Error
            (""unify/2 for type_ctor_desc type not implemented"");
    }

    public static mercury.builtin.Comparison_result_0
    __Compare____type_desc_0_0(mercury.type_desc.Type_desc_0 x,
        mercury.type_desc.Type_desc_0 y)
    {
        // stub only
        throw new java.lang.Error
            (""compare/3 for type_desc type implemented"");
    }

    public static mercury.builtin.Comparison_result_0
    __Compare____type_ctor_desc_0_0(mercury.type_desc.Type_ctor_desc_0 x,
        mercury.type_desc.Type_ctor_desc_0 y)
    {
        // stub only
        throw new java.lang.Error
            (""compare/3 for type_ctor_desc type implemented"");
    }

").

%-----------------------------------------------------------------------------%
