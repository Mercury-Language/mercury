%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2002-2007, 2009-2012 The University of Melbourne.
% Copyright (C) 2013-2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: type_desc.m.
% Main author: fjh, zs.
% Stability: low.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module type_desc.
:- interface.

:- import_module list.

%---------------------------------------------------------------------------%

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
:- pred ground_pseudo_type_desc_to_type_desc(pseudo_type_desc::in,
    type_desc::out) is semidet.

    % Convert a pseudo_type_desc describing a ground type to a type_desc.
    % Throw an exception if the pseudo_type_desc describes a non-ground type.
    %
:- func det_ground_pseudo_type_desc_to_type_desc(pseudo_type_desc) = type_desc.

%---------------------------------------------------------------------------%

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
:- func type_of(T::unused) = (type_desc::out) is det.

    % The predicate has_type/2 is basically an existentially typed inverse
    % to the function type_of/1. It constrains the type of the first argument
    % to be the type represented by the second argument.
    %
:- some [T] pred has_type(T::unused, type_desc::in) is det.

    % The predicate same_type/2 ensures type identity of the two arguments.
    %
:- pred same_type(T::unused, T::unused) is det.

    % type_name(Type) returns the name of the specified type
    % (e.g. type_name(type_of([2,3])) = "list.list(int)").
    % Any equivalence types will be fully expanded.
    % Builtin types (those defined in builtin.m) will not have
    % a module qualifier.
    %
:- func type_name(type_desc) = string.

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
    % you can use the reverse mode of make_type/2 instead.)
    %
:- pred type_ctor_and_args(type_desc::in,
    type_ctor_desc::out, list(type_desc)::out) is det.

    % pseudo_type_ctor_and_args(Type, TypeCtor, TypeArgs):
    %
    % True iff `TypeCtor' is a representation of the top-level type constructor
    % for `Type', and `TypeArgs' is a list of the corresponding type arguments
    % to `TypeCtor', and `TypeCtor' is not an equivalence type.
    %
    % Similar to type_ctor_and_args, but works on pseudo_type_infos.
    % Fails if the input pseudo_type_info is a variable.
    %
:- pred pseudo_type_ctor_and_args(pseudo_type_desc::in,
    type_ctor_desc::out, list(pseudo_type_desc)::out) is semidet.

    % type_ctor(Type) = TypeCtor :-
    %   type_ctor_and_args(Type, TypeCtor, _).
    %
:- func type_ctor(type_desc) = type_ctor_desc.

    % pseudo_type_ctor(Type) = TypeCtor :-
    %   pseudo_type_ctor_and_args(Type, TypeCtor, _).
    %
:- func pseudo_type_ctor(pseudo_type_desc) = type_ctor_desc is semidet.

    % type_args(Type) = TypeArgs :-
    %   type_ctor_and_args(Type, _, TypeArgs).
    %
:- func type_args(type_desc) = list(type_desc).

    % pseudo_type_args(Type) = TypeArgs :-
    %   pseudo_type_ctor_and_args(Type, _, TypeArgs).
    %
:- func pseudo_type_args(pseudo_type_desc) = list(pseudo_type_desc) is semidet.

    % type_ctor_name(TypeCtor) returns the name of specified type constructor.
    % (e.g. type_ctor_name(type_ctor(type_of([2,3]))) = "list").
    %
:- func type_ctor_name(type_ctor_desc) = string.

    % type_ctor_module_name(TypeCtor) returns the module name of specified
    % type constructor.
    % (e.g. type_ctor_module_name(type_ctor(type_of(2))) = "builtin").
    %
:- func type_ctor_module_name(type_ctor_desc) = string.

    % type_ctor_arity(TypeCtor) returns the arity of specified
    % type constructor.
    % (e.g. type_ctor_arity(type_ctor(type_of([2,3]))) = 1).
    %
:- func type_ctor_arity(type_ctor_desc) = int.

    % type_ctor_name_and_arity(TypeCtor, ModuleName, TypeName, Arity) :-
    %   Name = type_ctor_name(TypeCtor),
    %   ModuleName = type_ctor_module_name(TypeCtor),
    %   Arity = type_ctor_arity(TypeCtor).
    %
:- pred type_ctor_name_and_arity(type_ctor_desc::in,
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
:- func make_type(type_ctor_desc, list(type_desc)) = type_desc.
:- mode make_type(in, in) = out is semidet.
:- mode make_type(out, out) = in is cc_multi.

    % det_make_type(TypeCtor, TypeArgs):
    %
    % Returns the type formed by applying the specified type constructor
    % to the specified argument types. Throws an exception if the length of
    % `TypeArgs' is not the same as the arity of `TypeCtor'.
    %
:- func det_make_type(type_ctor_desc, list(type_desc)) = type_desc.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

% Everything below here is not intended to be part of the public interface,
% and will not be included in the Mercury library reference manual.

:- interface.

:- use_module rtti_implementation.

% The following predicates are exported for construct.m.

:- pred type_desc_to_type_info(type_desc::in,
    rtti_implementation.type_info::out) is det.

:- pred type_info_to_type_desc(rtti_implementation.type_info::in,
    type_desc::out) is det.

:- pred type_info_list_to_type_desc_list(
    list(rtti_implementation.type_info)::in, list(type_desc)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module require.
:- import_module string.

:- pragma foreign_decl("C", "
#include ""mercury_heap.h""         // for MR_incr_hp_msg() etc.
#include ""mercury_misc.h""         // for MR_fatal_error()
#include ""mercury_string.h""       // for MR_make_aligned_string()
#include ""mercury_type_desc.h""
").

% The Java backend substitutes:
%
%   type_desc        == jmercury.runtime.TypeInfo_Struct
%   pseudo_type_desc == jmercury.runtime.PseudoTypeDesc
%   type_ctor_desc   == jmercury.runtime.TypeCtorInfo_Struct
%
% We can't use `:- pragma foreign_type' because the compiler will complain
% that non-Java grades are missing type definitions.

:- pragma foreign_decl("Java", local, "
// Any foreign_procs which use the unqualified names should be marked
// `may_not_duplicate' so as not to be written to .opt files.

import jmercury.runtime.PseudoTypeInfo;
import jmercury.runtime.TypeCtorInfo_Struct;
import jmercury.runtime.TypeInfo_Struct;
").

%---------------------------------------------------------------------------%

pseudo_type_desc_is_ground(PseudoTypeDesc) :-
    pseudo_type_ctor_and_args(PseudoTypeDesc, _TypeCtor, ArgPseudos),
    list.all_true(pseudo_type_desc_is_ground, ArgPseudos).

pseudo_type_desc_to_rep(PseudoTypeDesc) = PseudoTypeRep :-
    ( if pseudo_type_ctor_and_args(PseudoTypeDesc, TypeCtor, ArgPseudos) then
        PseudoTypeRep = bound(TypeCtor, ArgPseudos)
    else if is_exist_pseudo_type_desc(PseudoTypeDesc, UnivNum) then
        PseudoTypeRep = exist_tvar(UnivNum)
    else if is_univ_pseudo_type_desc(PseudoTypeDesc, UnivNum) then
        PseudoTypeRep = univ_tvar(UnivNum)
    else
        error($pred, "internal error")
    ).

:- pred is_univ_pseudo_type_desc(pseudo_type_desc::in, int::out) is semidet.

:- pragma foreign_proc("C",
    is_univ_pseudo_type_desc(PseudoTypeDesc::in, TypeVarNum::out),
    [will_not_call_mercury, thread_safe, promise_pure, will_not_modify_trail,
        no_sharing],
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

is_univ_pseudo_type_desc(PTD, N) :-
    pseudo_type_desc_to_pseudo_type_info(PTD, PTI),
    rtti_implementation.is_univ_pseudo_type_info(PTI, N).

:- pred is_exist_pseudo_type_desc(pseudo_type_desc::in, int::out) is semidet.

:- pragma foreign_proc("C",
    is_exist_pseudo_type_desc(PseudoTypeDesc::in, TypeVarNum::out),
    [will_not_call_mercury, thread_safe, promise_pure, will_not_modify_trail,
        no_sharing],
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

is_exist_pseudo_type_desc(PTD, N) :-
    pseudo_type_desc_to_pseudo_type_info(PTD, PTI),
    rtti_implementation.is_exist_pseudo_type_info(PTI, N).

%---------------------------------------------------------------------------%

:- pred pseudo_type_desc_to_pseudo_type_info(pseudo_type_desc::in,
    rtti_implementation.pseudo_type_info::out) is det.
:- pragma consider_used(pseudo_type_desc_to_pseudo_type_info/2).

pseudo_type_desc_to_pseudo_type_info(PseudoTypeDesc, PseudoTypeInfo) :-
    ( if type_info_desc_same_representation then
        private_builtin.unsafe_type_cast(PseudoTypeDesc, PseudoTypeInfo)
    else
        error("pseudo_type_desc_to_pseudo_type_info/2")
    ).

:- pred type_info_desc_same_representation is semidet.

type_info_desc_same_representation :-
    semidet_true.

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    type_desc_to_pseudo_type_desc(TypeDesc::in) = (PseudoTypeDesc::out),
    [will_not_call_mercury, thread_safe, promise_pure, will_not_modify_trail],
"
    PseudoTypeDesc = TypeDesc;
").

:- pragma foreign_proc("C#",
    type_desc_to_pseudo_type_desc(TypeDesc::in) = (PseudoTypeDesc::out),
    [will_not_call_mercury, thread_safe, promise_pure, will_not_modify_trail],
"
    PseudoTypeDesc = TypeDesc;
").

:- pragma foreign_proc("Java",
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
    ground_pseudo_type_desc_to_type_desc(PseudoTypeDesc, TypeDesc).

ground_pseudo_type_desc_to_type_desc(PseudoTypeDesc, TypeDesc) :-
    ( if pseudo_type_desc_is_ground(PseudoTypeDesc) then
        private_builtin.unsafe_type_cast(PseudoTypeDesc, TypeDesc)
    else
        fail
    ).

det_ground_pseudo_type_desc_to_type_desc(PseudoTypeDesc) = TypeDesc :-
    ( if pseudo_type_desc_is_ground(PseudoTypeDesc) then
        private_builtin.unsafe_type_cast(PseudoTypeDesc, TypeDesc)
    else
        error($pred, "not ground")
    ).

%---------------------------------------------------------------------------%
%
% Code for type manipulation.
%

:- pragma foreign_proc("C",
    type_of(_Value::unused) = (TypeInfo::out),
    [will_not_call_mercury, thread_safe, promise_pure, will_not_modify_trail,
        no_sharing],
"{
    TypeInfo = TypeInfo_for_T;

    // We used to collapse equivalences for efficiency here, but that is not
    // always desirable, due to the reverse mode of make_type/2, and efficiency
    // of type_infos probably isn't very important anyway.
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
    TypeInfo = TypeInfo_for_T;
").

:- pragma foreign_proc("C",
    has_type(_Arg::unused, TypeInfo::in),
    [will_not_call_mercury, thread_safe, promise_pure, no_sharing],
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
    TypeInfo_for_T = TypeInfo;
").

same_type(_, _).

% Export this function in order to use it in runtime/mercury_trace_external.c
:- pragma foreign_export("C", type_name(in) = out, "ML_type_name").

type_name(Type) = TypeName :-
    type_ctor_and_args(Type, TypeCtor, ArgTypes),
    type_ctor_name_and_arity(TypeCtor, ModuleName, Name, Arity),
    ( if Arity = 0 then
        UnqualifiedTypeName = Name
    else
        ( if ModuleName = "builtin", Name = "func" then
            IsFunc = yes
        else
            IsFunc = no
        ),
        ( if
            ModuleName = "builtin", Name = "{}"
        then
            type_arg_names(ArgTypes, IsFunc, ArgTypeNames),
            list.append(ArgTypeNames, ["}"], TypeStrings0),
            TypeStrings = ["{" | TypeStrings0],
            string.append_list(TypeStrings, UnqualifiedTypeName)
        else if
            IsFunc = yes,
            ArgTypes = [FuncRetType]
        then
            FuncRetTypeName = type_name(FuncRetType),
            string.append_list(["((func) = ", FuncRetTypeName, ")"],
                UnqualifiedTypeName)
        else
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
    ( if ModuleName = "builtin" then
        TypeName = UnqualifiedTypeName
    else
        string.append_list([ModuleName, ".", UnqualifiedTypeName], TypeName)
    ).

    % Turn the types into a list of strings representing an argument list,
    % adding commas as separators as required. For example:
    %   ["TypeName1", ",", "TypeName2"]
    % If formatting a function type, we close the parentheses around
    % the function's input parameters, e.g.
    %   ["TypeName1", ",", "TypeName2", ") = ", "ReturnTypeName"]
    % It is the caller's responsibility to add matching parentheses.
    %
:- pred type_arg_names(list(type_desc)::in, bool::in, list(string)::out)
    is det.

type_arg_names([], _, []).
type_arg_names([Type | Types], IsFunc, ArgNames) :-
    Name = type_name(Type),
    (
        Types = [],
        ArgNames = [Name]
    ;
        Types = [_ | _],
        ( if
            IsFunc = yes,
            Types = [FuncReturnType]
        then
            FuncReturnName = type_name(FuncReturnType),
            ArgNames = [Name, ") = ", FuncReturnName]
        else
            type_arg_names(Types, IsFunc, Names),
            ArgNames = [Name, ", " | Names]
        )
    ).

%---------------------------------------------------------------------------%

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

type_ctor_and_args(TypeDesc, TypeCtorDesc, ArgTypeDescs) :-
    type_desc_to_type_info(TypeDesc, TypeInfo),
    rtti_implementation.type_ctor_and_args(TypeInfo, TypeCtorInfo,
        ArgTypeInfos),
    make_type_ctor_desc(TypeInfo, TypeCtorInfo, TypeCtorDesc),
    type_info_list_to_type_desc_list(ArgTypeInfos, ArgTypeDescs).

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

pseudo_type_ctor_and_args(PseudoTypeDesc, TypeCtorDesc, ArgPseudoTypeDescs) :-
    pseudo_type_desc_to_pseudo_type_info(PseudoTypeDesc, PseudoTypeInfo),
    rtti_implementation.pseudo_type_ctor_and_args(PseudoTypeInfo,
        TypeCtorInfo, ArgPseudoTypeInfos),
    Arity = list.length(ArgPseudoTypeInfos),
    make_type_ctor_desc_with_arity(Arity, TypeCtorInfo, TypeCtorDesc),
    private_builtin.unsafe_type_cast(ArgPseudoTypeInfos, ArgPseudoTypeDescs).

%---------------------------------------------------------------------------%

:- pragma no_determinism_warning(pseudo_type_ctor/1).

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

type_ctor(TypeDesc) = TypeCtorDesc :-
    type_desc_to_type_info(TypeDesc, TypeInfo),
    TypeCtorInfo = rtti_implementation.get_type_ctor_info(TypeInfo),
    make_type_ctor_desc(TypeInfo, TypeCtorInfo, TypeCtorDesc).

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

pseudo_type_ctor(_) = _ :-
    private_builtin.sorry("pseudo_type_ctor/1").

%---------------------------------------------------------------------------%

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

%---------------------------------------------------------------------------%

    % Make a type_info_desc from a type_ctor_info. A type_info_desc is
    % different to a type_ctor_info in the case of variable arity types,
    % i.e. predicates, functions and tuples.
    %
    % The C implementation uses small integers to encode variable arity
    % type_ctor_infos (see mercury_type_desc.h). In the Java backend we simply
    % allocate new TypeCtorInfo_Struct objects and set the `arity' field.
    % Two equivalent type_ctor_descs may have different addresses.
    %
:- pred make_type_ctor_desc(rtti_implementation.type_info::in,
    rtti_implementation.type_ctor_info::in, type_ctor_desc::out) is det.
:- pragma consider_used(make_type_ctor_desc/3).

:- pragma foreign_proc("C#",
    make_type_ctor_desc(TypeInfo::in, TypeCtorInfo::in, TypeCtorDesc::out),
    [will_not_call_mercury, promise_pure, thread_safe,
        may_not_duplicate],
"
    runtime.TypeCtorInfo_Struct tci = TypeCtorInfo;

    // Handle variable arity types.
    switch (tci.type_ctor_rep) {
        case runtime.TypeCtorRep.MR_TYPECTOR_REP_PRED:
        case runtime.TypeCtorRep.MR_TYPECTOR_REP_FUNC:
        case runtime.TypeCtorRep.MR_TYPECTOR_REP_TUPLE:
            tci = new runtime.TypeCtorInfo_Struct(tci, TypeInfo.args.Length);
            break;
        default:
            break;
    }

    TypeCtorDesc = tci;
").

:- pragma foreign_proc("Java",
    make_type_ctor_desc(TypeInfo::in, TypeCtorInfo::in, TypeCtorDesc::out),
    [will_not_call_mercury, promise_pure, thread_safe,
        may_not_duplicate],
"
    TypeCtorInfo_Struct tci = TypeCtorInfo;

    // Handle variable arity types.
    switch (tci.type_ctor_rep.value) {
        case jmercury.runtime.TypeCtorRep.MR_TYPECTOR_REP_PRED:
        case jmercury.runtime.TypeCtorRep.MR_TYPECTOR_REP_FUNC:
        case jmercury.runtime.TypeCtorRep.MR_TYPECTOR_REP_TUPLE:
            tci = new TypeCtorInfo_Struct(tci, TypeInfo.args.length);
            break;
        default:
            break;
    }

    TypeCtorDesc = tci;
").

make_type_ctor_desc(_, _, _) :-
    private_builtin.sorry("make_type_ctor_desc/3").

:- pred make_type_ctor_desc_with_arity(int::in,
    rtti_implementation.type_ctor_info::in, type_ctor_desc::out) is det.
:- pragma consider_used(make_type_ctor_desc_with_arity/3).

:- pragma foreign_proc("C#",
    make_type_ctor_desc_with_arity(Arity::in, TypeCtorInfo::in,
        TypeCtorDesc::out),
    [will_not_call_mercury, promise_pure, thread_safe,
        may_not_duplicate],
"
    runtime.TypeCtorInfo_Struct tci = TypeCtorInfo;

    // Handle variable arity types.
    switch (tci.type_ctor_rep) {
        case runtime.TypeCtorRep.MR_TYPECTOR_REP_PRED:
        case runtime.TypeCtorRep.MR_TYPECTOR_REP_FUNC:
        case runtime.TypeCtorRep.MR_TYPECTOR_REP_TUPLE:
            tci = new runtime.TypeCtorInfo_Struct(tci, Arity);
            break;
        default:
            break;
    }

    TypeCtorDesc = tci;
").

:- pragma foreign_proc("Java",
    make_type_ctor_desc_with_arity(Arity::in, TypeCtorInfo::in,
        TypeCtorDesc::out),
    [will_not_call_mercury, promise_pure, thread_safe,
        may_not_duplicate],
"
    TypeCtorInfo_Struct tci = TypeCtorInfo;

    // Handle variable arity types.
    switch (tci.type_ctor_rep.value) {
        case jmercury.runtime.TypeCtorRep.MR_TYPECTOR_REP_PRED:
        case jmercury.runtime.TypeCtorRep.MR_TYPECTOR_REP_FUNC:
        case jmercury.runtime.TypeCtorRep.MR_TYPECTOR_REP_TUPLE:
            tci = new TypeCtorInfo_Struct(tci, Arity);
            break;
        default:
            break;
    }

    TypeCtorDesc = tci;
").

make_type_ctor_desc_with_arity(_, _, _) :-
    private_builtin.sorry("make_type_ctor_desc_with_arity/3").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    type_ctor_name_and_arity(TypeCtorDesc::in, TypeCtorModuleName::out,
        TypeCtorName::out, TypeCtorArity::out),
    [will_not_call_mercury, thread_safe, promise_pure,
        will_not_modify_trail],
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

        // We cast away the const-ness of the module and type names,
        // because MR_String is defined as char *, not const char *.

        TypeCtorModuleName = (MR_String) (MR_Integer)
            MR_type_ctor_module_name(type_ctor_info);
        TypeCtorName = (MR_String) (MR_Integer)
            MR_type_ctor_name(type_ctor_info);
        TypeCtorArity = type_ctor_info->MR_type_ctor_arity;
    }
}").

type_ctor_name_and_arity(TypeCtorDesc, ModuleName, TypeCtorName,
        TypeCtorArity) :-
    type_ctor_desc_to_type_ctor_info(TypeCtorDesc, TypeCtorInfo),
    rtti_implementation.type_ctor_name_and_arity(TypeCtorInfo,
        ModuleName, TypeCtorName, TypeCtorArity).

:- pred type_ctor_desc_to_type_ctor_info(type_ctor_desc::in,
    rtti_implementation.type_ctor_info::out) is det.
:- pragma consider_used(type_ctor_desc_to_type_ctor_info/2).

type_ctor_desc_to_type_ctor_info(TypeCtorDesc, TypeCtorInfo) :-
    ( if type_info_desc_same_representation then
        private_builtin.unsafe_type_cast(TypeCtorDesc, TypeCtorInfo)
    else
        error("type_ctor_desc_to_type_ctor_info/2")
    ).

%---------------------------------------------------------------------------%

:- pragma promise_equivalent_clauses(make_type/2).
:- pragma no_determinism_warning(make_type/2).

make_type(_TypeCtorDesc::in, _ArgTypes::in) = (_TypeDesc::out) :-
    private_builtin.sorry("make_type(in, in) = out").
make_type(_TypeCtorDesc::out, _ArgTypes::out) = (_TypeDesc::in) :-
    private_builtin.sorry("make_type(out, out) = in").

% This is the forwards mode of make_type/2: given a type constructor and
% a list of argument types, check that the length of the argument types
% matches the arity of the type constructor, and if so, use the type
% constructor to construct a new type with the specified arguments.

:- pragma foreign_proc("C",
    make_type(TypeCtorDesc::in, ArgTypes::in) = (TypeDesc::out),
    [promise_pure, will_not_call_mercury, thread_safe, will_not_modify_trail],
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

:- pragma foreign_proc("C#",
    make_type(TypeCtorDesc::in, ArgTypes::in) = (TypeDesc::out),
    [promise_pure, will_not_call_mercury, thread_safe, will_not_modify_trail,
        may_not_duplicate],
"{
    runtime.PseudoTypeInfo[] args =
        new runtime.PseudoTypeInfo[TypeCtorDesc.arity];

    SUCCESS_INDICATOR = true;
    list.List_1 arg_types = ArgTypes;
    for (int i = 0; i < TypeCtorDesc.arity; i++) {
        if (list.is_empty(arg_types)) {
            SUCCESS_INDICATOR = false;
            break;
        }
        args[i] = (runtime.PseudoTypeInfo) list.det_head(arg_types);
        arg_types = list.det_tail(arg_types);
    }

    if (SUCCESS_INDICATOR) {
        TypeDesc = new runtime.TypeInfo_Struct();
        TypeDesc.init(TypeCtorDesc, args);
    } else {
        TypeDesc = null;
    }
}").

:- pragma foreign_proc("Java",
    make_type(TypeCtorDesc::in, ArgTypes::in) = (TypeDesc::out),
    [promise_pure, will_not_call_mercury, thread_safe, will_not_modify_trail,
        may_not_duplicate],
"{
    PseudoTypeInfo[] as = new PseudoTypeInfo[TypeCtorDesc.arity];

    SUCCESS_INDICATOR = true;
    list.List_1<TypeInfo_Struct> arg_types = ArgTypes;
    for (int i = 0; i < TypeCtorDesc.arity; i++) {
        if (list.is_empty(arg_types)) {
            SUCCESS_INDICATOR = false;
            break;
        }
        as[i] = list.det_head(arg_types);
        arg_types = list.det_tail(arg_types);
    }

    if (SUCCESS_INDICATOR) {
        TypeDesc = new TypeInfo_Struct();
        TypeDesc.init(TypeCtorDesc, as);
    } else {
        TypeDesc = null;
    }
}").

% This is the reverse mode of make_type: given a type,
% split it up into a type constructor and a list of arguments.

:- pragma foreign_proc("C",
    make_type(TypeCtorDesc::out, ArgTypes::out) = (TypeDesc::in),
    [promise_pure, will_not_call_mercury, thread_safe, will_not_modify_trail],
"{
    MR_TypeCtorDesc type_ctor_desc;
    MR_TypeInfo     type_info;

    MR_save_transient_registers();

    type_info = (MR_TypeInfo) TypeDesc;
    MR_type_ctor_and_args(type_info, MR_FALSE, &type_ctor_desc, &ArgTypes);
    TypeCtorDesc = (MR_Word) type_ctor_desc;

    MR_restore_transient_registers();
}").

det_make_type(TypeCtor, ArgTypes) = Type :-
    ( if make_type(TypeCtor, ArgTypes) = NewType then
        Type = NewType
    else
        error($pred, "make_type/2 failed (wrong arity)")
    ).

%---------------------------------------------------------------------------%

    % This function returns the type_info for the builtin type "typeinfo"
    % itself. It is intended for use from C code, since Mercury code can access
    % this type_info easily enough even without this predicate.
    %
    % XXX This code relies on the type "type_desc" being the same type
    % as the builtin type "typeinfo".
    %
:- func get_type_info_for_type_info = type_desc.

:- pragma foreign_export("C", get_type_info_for_type_info = out,
    "ML_get_type_info_for_type_info").

get_type_info_for_type_info = TypeDesc :-
    Type = type_of(1),
    TypeDesc = type_of(Type).

%---------------------------------------------------------------------------%

:- pragma foreign_code("C#", "
    public static bool
    __Unify____type_desc_0_0(
        runtime.TypeInfo_Struct x,
        runtime.TypeInfo_Struct y)
    {
        return x.Equals(y);
    }

    public static bool
    __Unify____type_ctor_desc_0_0(
        runtime.TypeCtorInfo_Struct x,
        runtime.TypeCtorInfo_Struct y)
    {
        return x.Equals(y);
    }

    public static builtin.Comparison_result_0
    __Compare____type_desc_0_0(
        runtime.TypeInfo_Struct x,
        runtime.TypeInfo_Struct y)
    {
        return rtti_implementation.ML_compare_type_infos(x, y);
    }

    public static builtin.Comparison_result_0
    __Compare____type_ctor_desc_0_0(
        runtime.TypeCtorInfo_Struct x,
        runtime.TypeCtorInfo_Struct y)
    {
        return rtti_implementation.ML_compare_type_ctor_infos(x, y);
    }

    public static bool
    __Unify____pseudo_type_desc_0_0(
        runtime.PseudoTypeInfo x,
        runtime.PseudoTypeInfo y)
    {
        return x.Equals(y);
    }

    public static builtin.Comparison_result_0
    __Compare____pseudo_type_desc_0_0(
        runtime.PseudoTypeInfo x,
        runtime.PseudoTypeInfo y)
    {
        return rtti_implementation.ML_compare_pseudo_type_infos(x, y);
    }
").

:- pragma foreign_code("Java", "
    public static boolean
    __Unify____type_desc_0_0(TypeInfo_Struct x, TypeInfo_Struct y)
    {
        return x.unify(y);
    }

    public static boolean
    __Unify____type_ctor_desc_0_0(TypeCtorInfo_Struct x, TypeCtorInfo_Struct y)
    {
        return x.unify(y);
    }

    public static builtin.Comparison_result_0
    __Compare____type_desc_0_0(TypeInfo_Struct x, TypeInfo_Struct y)
    {
        return rtti_implementation.ML_compare_type_infos(x, y);
    }

    public static builtin.Comparison_result_0
    __Compare____type_ctor_desc_0_0(TypeCtorInfo_Struct x,
        TypeCtorInfo_Struct y)
    {
        return rtti_implementation.ML_compare_type_ctor_infos(x, y);
    }

    public static boolean
    __Unify____pseudo_type_desc_0_0(PseudoTypeInfo x, PseudoTypeInfo y)
    {
        return x.unify(y);
    }

    public static builtin.Comparison_result_0
    __Compare____pseudo_type_desc_0_0(PseudoTypeInfo x, PseudoTypeInfo y)
    {
        return rtti_implementation.ML_compare_pseudo_type_infos(x, y);
    }
").

%---------------------------------------------------------------------------%

type_desc_to_type_info(TypeDesc, TypeInfo) :-
    ( if type_info_desc_same_representation then
        private_builtin.unsafe_type_cast(TypeDesc, TypeInfo)
    else
        error("type_desc_to_type_info/2")
    ).

type_info_to_type_desc(TypeInfo, TypeDesc) :-
    ( if type_info_desc_same_representation then
        private_builtin.unsafe_type_cast(TypeInfo, TypeDesc)
    else
        error("type_info_to_type_desc/2")
    ).

type_info_list_to_type_desc_list(TypeInfoList, TypeDescList) :-
    ( if type_info_desc_same_representation then
        private_builtin.unsafe_type_cast(TypeInfoList, TypeDescList)
    else
        list.map(type_info_to_type_desc, TypeInfoList, TypeDescList)
    ).

%---------------------------------------------------------------------------%
:- end_module type_desc.
%---------------------------------------------------------------------------%
