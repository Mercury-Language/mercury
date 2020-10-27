%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2007, 2012 The University of Melbourne.
% Copyright (C) 2013-2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: private_builtin.m.
% Main authors: fjh, zs.
% Stability: medium.
%
% This file is automatically imported, as if via `use_module', into every
% module. It is intended for builtins that are just implementation details,
% such as procedures that the compiler generates implicit calls to when
% implementing polymorphism, unification, compare/3, etc.
% Note that the builtins that are needed only for the implementation of
% some specific constructs and/or in some specific grades, such as
% tabling, deep profiling and parallelism are in other modules named
% xxx_builtin.m in this directory.
%
% This module is a private part of the Mercury implementation; user modules
% should never explicitly import this module. The interface for this module
% does not get included in the Mercury library reference manual.
%
% Many of the predicates defined in this module are builtin: they do not
% have definitions, because the compiler generates code for them inline.
% Some others are implemented in the runtime. A third group are implemented
% normally in this module.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module private_builtin.

%---------------------------------------------------------------------------%
%
% This section of the module contains predicates that are used by the
% compiler to implement polymorphism. Changes here may also require changes
% in compiler/polymorphism.m, compiler/unify_proc.m, compiler/higher_order.m
% and runtime/mercury_type_info.{c,h}.
%
% These predicates should not be used by user programs directly.
%

:- interface.

:- pred builtin_unify_int(int::in, int::in) is semidet.
:- pred builtin_compare_int(comparison_result::uo, int::in, int::in) is det.

:- pred builtin_unify_uint(uint::in, uint::in) is semidet.
:- pred builtin_compare_uint(comparison_result::uo, uint::in, uint::in) is det.

:- pred builtin_unify_int8(int8::in, int8::in) is semidet.
:- pred builtin_compare_int8(comparison_result::uo, int8::in, int8::in) is det.

:- pred builtin_unify_uint8(uint8::in, uint8::in) is semidet.
:- pred builtin_compare_uint8(comparison_result::uo, uint8::in, uint8::in)
    is det.

:- pred builtin_unify_int16(int16::in, int16::in) is semidet.
:- pred builtin_compare_int16(comparison_result::uo, int16::in, int16::in)
    is det.

:- pred builtin_unify_uint16(uint16::in, uint16::in) is semidet.
:- pred builtin_compare_uint16(comparison_result::uo, uint16::in, uint16::in)
    is det.

:- pred builtin_unify_int32(int32::in, int32::in) is semidet.
:- pred builtin_compare_int32(comparison_result::uo, int32::in, int32::in)
    is det.

:- pred builtin_unify_uint32(uint32::in, uint32::in) is semidet.
:- pred builtin_compare_uint32(comparison_result::uo, uint32::in, uint32::in)
    is det.

:- pred builtin_unify_int64(int64::in, int64::in) is semidet.
:- pred builtin_compare_int64(comparison_result::uo, int64::in, int64::in)
    is det.

:- pred builtin_unify_uint64(uint64::in, uint64::in) is semidet.
:- pred builtin_compare_uint64(comparison_result::uo, uint64::in, uint64::in)
    is det.

:- pred builtin_unify_character(character::in, character::in) is semidet.
:- pred builtin_compare_character(comparison_result::uo, character::in,
    character::in) is det.

:- pred builtin_unify_string(string::in, string::in) is semidet.
:- pred builtin_compare_string(comparison_result::uo, string::in, string::in)
    is det.

:- pred builtin_unify_float(float::in, float::in) is semidet.
:- pred builtin_compare_float(comparison_result::uo, float::in, float::in)
    is det.

:- pred builtin_unify_pred((pred)::in, (pred)::in) is semidet.
:- pred builtin_compare_pred(comparison_result::uo, (pred)::in, (pred)::in)
    is det.

    % These should never be called -- the compiler never specializes
    % comparison on these types because the generic compare is just as good
    % as anything we could put here.
    %
:- pred builtin_unify_tuple(T::in, T::in) is semidet.
:- pred builtin_compare_tuple(comparison_result::uo, T::in, T::in) is det.

    % The following pred is used for compare/3 on non-canonical types
    % (types for which there is a `where equality is ...' declaration).
    %
:- pred builtin_compare_non_canonical_type(comparison_result::uo,
    T::in, T::in) is det.

    % The following predicates are used for unify/2 (compare/3) on
    % solver types when the equality (comparison) attribute is omitted
    % from the solver type definition.
    %
:- pred builtin_unify_solver_type(T::in, T::in) is semidet.
:- pred builtin_compare_solver_type(comparison_result::uo,
    T::in, T::in) is det.

    % Compare_error is used in the code generated for compare/3 preds.
    %
:- pred compare_error is erroneous.

    % The builtin < and > operators on integers of all sizes and both
    % signednesses, used in the code generated for compare/3 preds.
    %
:- pred builtin_int_lt(int::in, int::in) is semidet.
:- pred builtin_int_gt(int::in, int::in) is semidet.
:- pred builtin_int8_lt(int8::in, int8::in) is semidet.
:- pred builtin_int8_gt(int8::in, int8::in) is semidet.
:- pred builtin_int16_lt(int16::in, int16::in) is semidet.
:- pred builtin_int16_gt(int16::in, int16::in) is semidet.
:- pred builtin_int32_lt(int32::in, int32::in) is semidet.
:- pred builtin_int32_gt(int32::in, int32::in) is semidet.
:- pred builtin_int64_lt(int64::in, int64::in) is semidet.
:- pred builtin_int64_gt(int64::in, int64::in) is semidet.
:- pred builtin_uint_lt(int::in, int::in) is semidet.
:- pred builtin_uint_gt(int::in, int::in) is semidet.
:- pred builtin_uint8_lt(uint8::in, uint8::in) is semidet.
:- pred builtin_uint8_gt(uint8::in, uint8::in) is semidet.
:- pred builtin_uint16_lt(uint16::in, uint16::in) is semidet.
:- pred builtin_uint16_gt(uint16::in, uint16::in) is semidet.
:- pred builtin_uint32_lt(uint32::in, uint32::in) is semidet.
:- pred builtin_uint32_gt(uint32::in, uint32::in) is semidet.
:- pred builtin_uint64_lt(uint64::in, uint64::in) is semidet.
:- pred builtin_uint64_gt(uint64::in, uint64::in) is semidet.

:- pred unsigned_lt(int::in, int::in) is semidet.
:- pred unsigned_le(int::in, int::in) is semidet.

    % These should never be called -- the compiler replaces calls to these
    % predicates with inline code. These predicates are declared not to take
    % type_infos.
    %
:- pred builtin_compound_eq(T::in, T::in) is semidet.
:- pred builtin_compound_lt(T::in, T::in) is semidet.

    % A "typed" version of unify/2 -- i.e. one that can handle arguments
    % of different types. It first unifies their types, and then (if the types
    % are equal) it unifies the values.
    %
:- pred typed_unify(T1, T2).
:- mode typed_unify(in, in) is semidet.
:- mode typed_unify(in, out) is semidet.

    % A "typed" version of compare/3 -- i.e. one that can handle arguments
    % of different types. It first compares the types, and then (if the
    % types are equal) it compares the values.
    %
:- pred typed_compare(comparison_result::uo, T1::in, T2::in) is det.

    % True iff the two terms occupy the same address in memory.
    % This is useful as a cheap but incomplete test of equality
    % when implementing user-defined equality.
    %
:- pred pointer_equal(T::in, T::in) is semidet.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module float.
:- import_module int.
:- import_module int8.
:- import_module int16.
:- import_module int32.
:- import_module int64.
:- import_module uint.
:- import_module uint8.
:- import_module uint16.
:- import_module uint32.
:- import_module uint64.
:- import_module require.
:- import_module string.
:- import_module type_desc.

:- pragma inline(builtin_compare_int/3).
:- pragma inline(builtin_compare_uint/3).
:- pragma inline(builtin_compare_int8/3).
:- pragma inline(builtin_compare_uint8/3).
:- pragma inline(builtin_compare_int16/3).
:- pragma inline(builtin_compare_uint16/3).
:- pragma inline(builtin_compare_int32/3).
:- pragma inline(builtin_compare_uint32/3).
:- pragma inline(builtin_compare_int64/3).
:- pragma inline(builtin_compare_uint64/3).
:- pragma inline(builtin_compare_character/3).
:- pragma inline(builtin_compare_string/3).
:- pragma inline(builtin_compare_float/3).

builtin_unify_int(X, X).

builtin_compare_int(R, X, Y) :-
    ( if X < Y then
        R = (<)
    else if X = Y then
        R = (=)
    else
        R = (>)
    ).

builtin_unify_uint(X, X).

builtin_compare_uint(R, X, Y) :-
    ( if X < Y then
        R = (<)
    else if X = Y then
        R = (=)
    else
        R = (>)
    ).

builtin_unify_int8(X, X).

builtin_compare_int8(R, X, Y) :-
    ( if X < Y then
        R = (<)
    else if X = Y then
        R = (=)
    else
        R = (>)
    ).

builtin_unify_uint8(X, X).

builtin_compare_uint8(R, X, Y) :-
    ( if X < Y then
        R = (<)
    else if X = Y then
        R = (=)
    else
        R = (>)
    ).

builtin_unify_int16(X, X).

builtin_compare_int16(R, X, Y) :-
    ( if X < Y then
        R = (<)
    else if X = Y then
        R = (=)
    else
        R = (>)
    ).

builtin_unify_uint16(X, X).

builtin_compare_uint16(R, X, Y) :-
    ( if X < Y then
        R = (<)
    else if X = Y then
        R = (=)
    else
        R = (>)
    ).

builtin_unify_int32(X, X).

builtin_compare_int32(R, X, Y) :-
    ( if X < Y then
        R = (<)
    else if X = Y then
        R = (=)
    else
        R = (>)
    ).

builtin_unify_uint32(X, X).

builtin_compare_uint32(R, X, Y) :-
    ( if X < Y then
        R = (<)
    else if X = Y then
        R = (=)
    else
        R = (>)
    ).

builtin_unify_int64(X, X).

builtin_compare_int64(R, X, Y) :-
    ( if X < Y then
        R = (<)
    else if X = Y then
        R = (=)
    else
        R = (>)
    ).

builtin_unify_uint64(X, X).

builtin_compare_uint64(R, X, Y) :-
    ( if X < Y then
        R = (<)
    else if X = Y then
        R = (=)
    else
        R = (>)
    ).

builtin_unify_character(C, C).

builtin_compare_character(R, X, Y) :-
    char.to_int(X, XI),
    char.to_int(Y, YI),
    ( if XI < YI then
        R = (<)
    else if XI = YI then
        R = (=)
    else
        R = (>)
    ).

builtin_unify_string(S, S).

builtin_compare_string(R, S1, S2) :-
    builtin_strcmp(Res, S1, S2),
    ( if Res < 0 then
        R = (<)
    else if Res = 0 then
        R = (=)
    else
        R = (>)
    ).

:- pred builtin_strcmp(int::out, string::in, string::in) is det.

:- pragma foreign_proc("C",
    builtin_strcmp(Res::out, S1::in, S2::in),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    Res = strcmp(S1, S2);
").

:- pragma foreign_proc("C#",
    builtin_strcmp(Res::out, S1::in, S2::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Res = System.String.CompareOrdinal(S1, S2);
").
:- pragma foreign_proc("Java",
    builtin_strcmp(Res::out, S1::in, S2::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Res = S1.compareTo(S2);
").

builtin_unify_float(F, F).

builtin_compare_float(R, F1, F2) :-
    ( if F1 < F2 then
        R = (<)
    else if F1 > F2 then
        R = (>)
    else
        R = (=)
    ).

:- pragma no_inline(builtin_unify_pred/2).
builtin_unify_pred(_X, _Y) :-
    ( if semidet_succeed then
        error("attempted higher-order unification")
    else
        % The following is never executed.
        semidet_succeed
    ).

:- pragma no_inline(builtin_compare_pred/3).
builtin_compare_pred(Result, _X, _Y) :-
    ( if semidet_succeed then
        error("attempted higher-order comparison")
    else
        % The following is never executed.
        Result = (<)
    ).

builtin_unify_tuple(_, _) :-
    ( if semidet_succeed then
        % The generic unification function in the runtime
        % should handle this itself.
        error("builtin_unify_tuple called")
    else
        % The following is never executed.
        semidet_succeed
    ).

builtin_compare_tuple(Res, _, _) :-
    ( if semidet_succeed then
        % The generic comparison function in the runtime
        % should handle this itself.
        error("builtin_compare_tuple called")
    else
        % The following is never executed.
        Res = (<)
    ).

:- pragma no_inline(builtin_compare_non_canonical_type/3).
builtin_compare_non_canonical_type(Res, X, _Y) :-
    % Suppress determinism warning.
    ( if semidet_succeed then
        Message = "call to compare/3 for non-canonical type `"
            ++ type_name(type_of(X)) ++ "'",
        error(Message)
    else
        % The following is never executed.
        Res = (<)
    ).

:- pragma no_inline(builtin_unify_solver_type/2).
builtin_unify_solver_type(_X, _Y) :-
    % Suppress determinism warning.
    ( if semidet_succeed then
        % XXX ideally we should use the commented out code but looking up
        % the name of the solver type in RTTI currently gives us the name of
        % the representation type - reporting the name of the latter is likely
        % to be confusing since representation types will nearly always have
        % equality defined on them.
        %Message = "call to unify/2 for solver type `"
        %    ++ type_name(type_of(X)) ++ "'",
        Message = "call to generated unify/2 for solver type",
        error(Message)
    else
        % This is never executed.
        semidet_fail
    ).

:- pragma no_inline(builtin_compare_solver_type/3).
builtin_compare_solver_type(Res, _X, _Y) :-
    % Suppress determinism warning.
    ( if semidet_succeed then
        % XXX see the comment above regarding RTTI.
        %Message = "call to compare/3 for solver type `"
        %    ++ type_name(type_of(X)) ++ "'",
        Message = "call to generated compare/3 for solver type",
        error(Message)
    else
        % This is never executed.
        Res = (<)
    ).

:- pragma no_inline(compare_error/0).
compare_error :-
    error("internal error in compare/3").

%---------------------------------------------------------------------------%

typed_unify(X, Y) :-
    ( if type_of(X) = type_of(Y) then
        unsafe_type_cast(X, Y)
    else
        fail
    ).

typed_compare(R, X, Y) :-
    compare(R0, type_of(X), type_of(Y)),
    ( if R0 = (=) then
        unsafe_type_cast(X, Z),
        compare(R, Z, Y)
    else
        R = R0
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% This section of the module handles the runtime representation of
% type information.
%

:- interface.

:- type type_info.
:- type type_ctor_info.

:- type typeclass_info.
:- type base_typeclass_info.

    % The following types are used by compiler/ml_code_util.m as the types
    % used for copying type_info/0 and typeclass_info/0 types.
    % XXX Document me better
    %
:- type sample_type_info
    ---> sample_type_info(type_info).
:- type sample_typeclass_info
    ---> sample_typeclass_info(typeclass_info).

    % type_info_from_typeclass_info(TypeClassInfo, Index, TypeInfo):
    %
    % Extracts TypeInfo from TypeClassInfo, where TypeInfo is the Index'th
    % type_info in the typeclass_info.
    %
    % Note: Index must be equal to the number of the desired type_info
    % plus the number of superclasses for this class.
    %
:- pred type_info_from_typeclass_info(typeclass_info::in, int::in,
    type_info::out) is det.

    % unconstrained_type_info_from_typeclass_info(TypeClassInfo,
    %   Index, TypeInfo):
    %
    % Extracts the TypeInfo for the Indexth unconstrained type variable
    % from the instance represented by TypeClassInfo.
    %
:- pred unconstrained_type_info_from_typeclass_info(typeclass_info::in,
    int::in, type_info::out) is det.

    % superclass_from_typeclass_info(TypeClassInfo, Index, SuperClass):
    %
    % Extracts SuperClass from TypeClassInfo where SuperClass is the
    % Indexth superclass of the class.
    %
:- pred superclass_from_typeclass_info(typeclass_info::in,
    int::in, typeclass_info::out) is det.

    % instance_constraint_from_typeclass_info(TypeClassInfo, Index,
    %   InstanceConstraintTypeClassInfo):
    %
    % Extracts the typeclass_info for the Indexth typeclass constraint
    % of the instance described by TypeClassInfo.
    %
    % Note: Index must be equal to the number of the desired constraint
    % plus the number of unconstrained type variables for this instance.
    %
:- pred instance_constraint_from_typeclass_info(typeclass_info::in,
    int::in, typeclass_info::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

    % The definitions for type_ctor_info/0 and type_info/0.

:- pragma foreign_code("C#", "

public static runtime.TypeInfo_Struct
MR_typeclass_info_param_type_info(object[] tcinfo, int index)
{
    object[] tmp;
    int t1;

    tmp = (object[]) tcinfo[0];
    t1 = System.Convert.ToInt32(tmp[0]) + index;
    return (runtime.TypeInfo_Struct) tcinfo[t1];
}

public static runtime.TypeInfo_Struct
MR_typeclass_info_instance_tvar_type_info(
    object[] tcinfo, int index)
{
    return (runtime.TypeInfo_Struct) tcinfo[index];
}

public static object[] MR_typeclass_info_superclass_info(
    object[] tcinfo, int index)
{
    object[] tmp;
    int t1;

    tmp = (object[]) tcinfo[0];
    t1 = System.Convert.ToInt32(tmp[0]) + index;
    return (object[]) tcinfo[t1];
}

public static object[] MR_typeclass_info_arg_typeclass_info(
    object[] tcinfo, int index)
{
    return (object[]) tcinfo[index];
}

").

:- pragma foreign_code("Java", "

public static jmercury.runtime.TypeInfo_Struct
MR_typeclass_info_param_type_info(/* typeclass_info */ Object[] tcinfo,
    int index)
{
    /* typeclass_info */ Object[] base_tcinfo;
    int t1;

    base_tcinfo = (Object[]) tcinfo[0];
    t1 = ((Integer) base_tcinfo[0]).intValue() + index;
    return (jmercury.runtime.TypeInfo_Struct) tcinfo[t1];
}

public static jmercury.runtime.TypeInfo_Struct
MR_typeclass_info_instance_tvar_type_info(
    /* typeclass_info */ Object[] tcinfo, int index)
{
    return (jmercury.runtime.TypeInfo_Struct) tcinfo[index];
}

public static /* typeclass_info */ Object[] MR_typeclass_info_superclass_info(
    /* typeclass_info */ Object[] tcinfo, int index)
{
    /* typeclass_info */ Object[] base_tcinfo;
    int t1;

    // The zeroth argument is num_extra_instance_args.
    base_tcinfo = (Object[]) tcinfo[0];
    t1 = ((Integer) base_tcinfo[0]).intValue() + index;
    return (/* typeclass_info */ Object[]) tcinfo[t1];
}

public static /* typeclass_info */ Object[]
MR_typeclass_info_arg_typeclass_info(
    /* typeclass_info */ Object[] tcinfo, int index)
{
    return (/* typeclass_info */ Object[]) tcinfo[index];
}

").

:- pragma foreign_code("C#", "

public class Ref_1
{
    // XXX stub only
}

public class Heap_pointer_0
{
    // XXX stub only
}

public static bool
__Unify____ref_1_0(runtime.TypeInfo_Struct ti,
    private_builtin.Ref_1 x, private_builtin.Ref_1 y)
{
    runtime.Errors.SORRY(""unify for ref"");
    return false;
}

public static builtin.Comparison_result_0
__Compare____ref_1_0(runtime.TypeInfo_Struct ti,
    private_builtin.Ref_1 x, private_builtin.Ref_1 y)
{
    runtime.Errors.SORRY(""compare for ref"");
    return builtin.Comparison_result_0.f_equal;
}

public static bool
__Unify____heap_pointer_0_0(
    private_builtin.Heap_pointer_0 x, private_builtin.Heap_pointer_0 y)
{
    runtime.Errors.SORRY(""unify for heap_pointer"");
    return false;
}

public static builtin.Comparison_result_0
__Compare____heap_pointer_0_0(
    private_builtin.Heap_pointer_0 x, private_builtin.Heap_pointer_0 y)
{
    runtime.Errors.SORRY(""unify for heap_pointer"");
    return builtin.Comparison_result_0.f_equal;
}

public static bool
__Unify____type_info_0_0(
    runtime.TypeInfo_Struct x,
    runtime.TypeInfo_Struct y)
{
    runtime.Errors.SORRY(""unify for type_info"");
    return false;
}

public static bool
__Unify____type_info_1_0(
    object[] type_info,
    runtime.TypeInfo_Struct x,
    runtime.TypeInfo_Struct y)
{
    runtime.Errors.SORRY(""unify for type_info"");
    return false;
}

public static bool
__Unify____typeclass_info_0_0(
    object[] x, object[] y)
{
    runtime.Errors.SORRY(""unify for typeclass_info"");
    return false;
}

public static bool
__Unify____typeclass_info_1_0(
    object[] type_info, object[] x, object[] y)
{
    runtime.Errors.SORRY(""unify for typeclass_info"");
    return false;
}

public static bool
__Unify____base_typeclass_info_0_0(
    object[] x, object[] y)
{
    runtime.Errors.SORRY(""unify for base_typeclass_info"");
    return false;
}

public static bool
__Unify____base_typeclass_info_1_0(
    object[] type_info, object[] x, object[] y)
{
    runtime.Errors.SORRY(""unify for base_typeclass_info"");
    return false;
}

public static bool
__Unify____type_ctor_info_0_0(
    runtime.TypeCtorInfo_Struct x,
    runtime.TypeCtorInfo_Struct y)
{
    runtime.Errors.SORRY(""unify for type_ctor_info"");
    return false;
}

public static bool
__Unify____type_ctor_info_1_0(
    object[] type_info, object[] x, object[] y)
{
    runtime.Errors.SORRY(""unify for type_ctor_info"");
    return false;
}

public static builtin.Comparison_result_0
__Compare____type_ctor_info_0_0(
    runtime.TypeCtorInfo_Struct x,
    runtime.TypeCtorInfo_Struct y)
{
    runtime.Errors.SORRY(""compare for type_ctor_info"");
    return builtin.Comparison_result_0.f_equal;
}

public static builtin.Comparison_result_0
__Compare____type_ctor_info_1_0(
    object[] type_info, object[] x, object[] y)
{
    runtime.Errors.SORRY(""compare for type_ctor_info"");
    return builtin.Comparison_result_0.f_equal;
}

public static builtin.Comparison_result_0
__Compare____type_info_0_0(
    runtime.TypeInfo_Struct x,
    runtime.TypeInfo_Struct y)
{
    runtime.Errors.SORRY(""compare for type_info"");
    return builtin.Comparison_result_0.f_equal;
}

public static builtin.Comparison_result_0
__Compare____type_info_1_0(
    object[] type_info,
    runtime.TypeInfo_Struct x,
    runtime.TypeInfo_Struct y)
{
    runtime.Errors.SORRY(""compare for type_info"");
    return builtin.Comparison_result_0.f_equal;
}

public static builtin.Comparison_result_0
__Compare____typeclass_info_0_0(
    object[] x, object[] y)
{
    runtime.Errors.SORRY(""compare for typeclass_info"");
    return builtin.Comparison_result_0.f_equal;
}

public static builtin.Comparison_result_0
__Compare____typeclass_info_1_0(
    object[] type_info, object[] x, object[] y)
{
    runtime.Errors.SORRY(""compare for typeclass_info"");
    return builtin.Comparison_result_0.f_equal;
}

public static builtin.Comparison_result_0
__Compare____base_typeclass_info_0_0(
    object[] x, object[] y)
{
    runtime.Errors.SORRY(""compare for base_typeclass_info"");
    return builtin.Comparison_result_0.f_equal;
}

public static builtin.Comparison_result_0
__Compare____base_typeclass_info_1_0(
    object[] type_info, object[] x, object[] y)
{
    runtime.Errors.SORRY(""compare for base_typeclass_info"");
    return builtin.Comparison_result_0.f_equal;
}

").

%---------------------%

:- pragma foreign_proc("C",
    type_info_from_typeclass_info(TypeClassInfo::in, Index::in,
        TypeInfo::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    TypeInfo = MR_typeclass_info_param_type_info(TypeClassInfo, Index);
").
:- pragma foreign_proc("C#",
    type_info_from_typeclass_info(TypeClassInfo::in, Index::in,
        TypeInfo::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    TypeInfo = private_builtin.MR_typeclass_info_param_type_info(
        TypeClassInfo, Index);
").
:- pragma foreign_proc("Java",
    type_info_from_typeclass_info(TypeClassInfo::in, Index::in,
        TypeInfo::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    TypeInfo = jmercury.private_builtin.
        MR_typeclass_info_param_type_info(TypeClassInfo, Index);
").

%---------------------%

:- pragma foreign_proc("C",
    unconstrained_type_info_from_typeclass_info(TypeClassInfo::in,
        Index::in, TypeInfo::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    TypeInfo = MR_typeclass_info_instance_tvar_type_info(TypeClassInfo, Index);
").
:- pragma foreign_proc("C#",
    unconstrained_type_info_from_typeclass_info(TypeClassInfo::in,
        Index::in, TypeInfo::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    TypeInfo = private_builtin.MR_typeclass_info_instance_tvar_type_info(
        TypeClassInfo, Index);
").
:- pragma foreign_proc("Java",
    unconstrained_type_info_from_typeclass_info(TypeClassInfo::in,
        Index::in, TypeInfo::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    TypeInfo = jmercury.private_builtin.
        MR_typeclass_info_instance_tvar_type_info(TypeClassInfo, Index);
").

%---------------------%

:- pragma foreign_proc("C",
    superclass_from_typeclass_info(TypeClassInfo0::in, Index::in,
        TypeClassInfo::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    TypeClassInfo =
        MR_typeclass_info_superclass_info(TypeClassInfo0, Index);
").
:- pragma foreign_proc("C#",
    superclass_from_typeclass_info(TypeClassInfo0::in, Index::in,
        TypeClassInfo::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    TypeClassInfo = private_builtin.MR_typeclass_info_superclass_info(
        TypeClassInfo0, Index);
").
:- pragma foreign_proc("Java",
    superclass_from_typeclass_info(TypeClassInfo0::in, Index::in,
        TypeClassInfo::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    TypeClassInfo = jmercury.private_builtin.
        MR_typeclass_info_superclass_info(TypeClassInfo0, Index);
").

%---------------------%

:- pragma foreign_proc("C",
    instance_constraint_from_typeclass_info(TypeClassInfo0::in,
        Index::in, TypeClassInfo::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    TypeClassInfo =
        MR_typeclass_info_arg_typeclass_info(TypeClassInfo0, Index);
").
:- pragma foreign_proc("C#",
    instance_constraint_from_typeclass_info(TypeClassInfo0::in,
        Index::in, TypeClassInfo::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    TypeClassInfo = private_builtin.MR_typeclass_info_arg_typeclass_info(
        TypeClassInfo0, Index);
").
:- pragma foreign_proc("Java",
    instance_constraint_from_typeclass_info(TypeClassInfo0::in,
        Index::in, TypeClassInfo::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    TypeClassInfo = jmercury.private_builtin.
        MR_typeclass_info_arg_typeclass_info(TypeClassInfo0, Index);
").

%---------------------------------------------------------------------------%
%
% In LLDS grades with float registers, we require a type_ctor_info in
% closure layouts to represent hidden float values which are passed via
% regular registers. The standard type_ctor_info represents hidden float
% arguments passed via float registers.
%

:- interface.

:- type float_box
    --->    float_box(float).

%---------------------------------------------------------------------------%
%
% This section of the module contains predicates that are used by the
% MLDS back-end to implement trailing. (The LLDS back-end does not use these;
% instead it handles the corresponding tasks directly during code generation.)
%
% These predicates should not be used by user programs directly.
%

:- interface.

:- type ticket == c_pointer.
:- type ticket_counter == c_pointer.

    % For documentation, see the corresponding LLDS instructions
    % in compiler/llds.m. See also compiler/notes/trailing.html.

:- impure pred store_ticket(ticket::out) is det.
:- impure pred reset_ticket_undo(ticket::in) is det.
:- impure pred reset_ticket_commit(ticket::in) is det.
:- impure pred reset_ticket_solve(ticket::in) is det.
:- impure pred discard_ticket is det.
:- impure pred prune_ticket is det.
:- impure pred mark_ticket_stack(ticket_counter::out) is det.
:- impure pred prune_tickets_to(ticket_counter::in) is det.

:- implementation.

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    store_ticket(Ticket::out),
    [will_not_call_mercury, thread_safe],
"
#ifdef MR_USE_TRAIL
    MR_store_ticket(Ticket);
#else
    Ticket = 0;
#endif
").
:- pragma foreign_proc("C#",
    store_ticket(Ticket::out),
    [will_not_call_mercury, thread_safe],
"
#if MR_USE_TRAIL
    runtime.Errors.SORRY(""foreign code for this function"");
    // MR_store_ticket(Ticket);
#else
    Ticket = null;
#endif
").
:- pragma foreign_proc("Java",
    store_ticket(Ticket::out),
    [will_not_call_mercury, thread_safe],
"
    // XXX No trailing for the Java back-end, so take no action.
    Ticket = null;
").

%---------------------%

:- pragma foreign_proc("C",
    reset_ticket_undo(Ticket::in),
    [will_not_call_mercury, thread_safe, does_not_affect_liveness],
"
#ifdef MR_USE_TRAIL
    MR_reset_ticket(Ticket, MR_undo);
#endif
").
:- pragma foreign_proc("C#",
    reset_ticket_undo(Ticket::in),
    [will_not_call_mercury, thread_safe],
"
#if MR_USE_TRAIL
    runtime.Errors.SORRY(""foreign code for this function"");
    // MR_reset_ticket(Ticket, MR_undo);
#endif
").
:- pragma foreign_proc("Java",
    reset_ticket_undo(_Ticket::in),
    [will_not_call_mercury, thread_safe],
"
    // XXX No trailing for the Java back-end, so take no action.
").

%---------------------%

:- pragma foreign_proc("C",
    reset_ticket_commit(Ticket::in),
    [will_not_call_mercury, thread_safe, does_not_affect_liveness],
"
#ifdef MR_USE_TRAIL
    MR_reset_ticket(Ticket, MR_commit);
#endif
").
:- pragma foreign_proc("C#",
    reset_ticket_commit(Ticket::in),
    [will_not_call_mercury, thread_safe],
"
#if MR_USE_TRAIL
    runtime.Errors.SORRY(""foreign code for this function"");
    // MR_reset_ticket(Ticket, MR_commit);
#endif
").
:- pragma foreign_proc("Java",
    reset_ticket_commit(_Ticket::in),
    [will_not_call_mercury, thread_safe],
"
    // XXX No trailing for the Java back-end, so take no action.
").

%---------------------%

:- pragma foreign_proc("C",
    reset_ticket_solve(Ticket::in),
    [will_not_call_mercury, thread_safe, does_not_affect_liveness],
"
#ifdef MR_USE_TRAIL
    MR_reset_ticket(Ticket, MR_solve);
#endif
").
:- pragma foreign_proc("C#",
    reset_ticket_solve(Ticket::in),
    [will_not_call_mercury, thread_safe],
"
#if MR_USE_TRAIL
    runtime.Errors.SORRY(""foreign code for this function"");
    // MR_reset_ticket(Ticket, MR_solve);
#endif
").
:- pragma foreign_proc("Java",
    reset_ticket_solve(_Ticket::in),
    [will_not_call_mercury, thread_safe],
"
    // XXX No trailing for the Java back-end, so take no action.
").

%---------------------%

:- pragma foreign_proc("C",
    discard_ticket,
    [will_not_call_mercury, thread_safe, does_not_affect_liveness],
"
#ifdef MR_USE_TRAIL
    MR_discard_ticket();
#endif
").
:- pragma foreign_proc("C#",
    discard_ticket,
    [will_not_call_mercury, thread_safe],
"
#if MR_USE_TRAIL
    runtime.Errors.SORRY(""foreign code for this function"");
    // MR_discard_ticket();
#endif
").
:- pragma foreign_proc("Java",
    discard_ticket,
    [will_not_call_mercury, thread_safe],
"
    // XXX No trailing for the Java back-end, so take no action.
").

%---------------------%

:- pragma foreign_proc("C",
    prune_ticket,
    [will_not_call_mercury, thread_safe, does_not_affect_liveness],
"
#ifdef MR_USE_TRAIL
    MR_prune_ticket();
#endif
").
:- pragma foreign_proc("C#",
    prune_ticket,
    [will_not_call_mercury, thread_safe],
"
#if MR_USE_TRAIL
    runtime.Errors.SORRY(""foreign code for this function"");
    // MR_prune_ticket();
#endif
").
:- pragma foreign_proc("Java",
    prune_ticket,
    [will_not_call_mercury, thread_safe],
"
    // XXX No trailing for the Java back-end, so take no action.
").

%---------------------%

:- pragma foreign_proc("C",
    mark_ticket_stack(TicketCounter::out),
    [will_not_call_mercury, thread_safe, does_not_affect_liveness],
"
#ifdef MR_USE_TRAIL
    MR_mark_ticket_stack(TicketCounter);
#else
    TicketCounter = 0;
#endif
").
:- pragma foreign_proc("C#",
    mark_ticket_stack(TicketCounter::out),
    [will_not_call_mercury, thread_safe],
"
#if MR_USE_TRAIL
    runtime.Errors.SORRY(""foreign code for this function"");
    // MR_mark_ticket_stack(TicketCounter);
#else
    TicketCounter = null;
#endif
").
:- pragma foreign_proc("Java",
    mark_ticket_stack(TicketCounter::out),
    [will_not_call_mercury, thread_safe],
"
    // XXX No trailing for the Java back-end, so take no action.
    TicketCounter = null;
").

%---------------------%

:- pragma foreign_proc("C",
    prune_tickets_to(TicketCounter::in),
    [will_not_call_mercury, thread_safe, does_not_affect_liveness],
"
#ifdef MR_USE_TRAIL
    MR_prune_tickets_to(TicketCounter);
#endif
").
:- pragma foreign_proc("C#",
    prune_tickets_to(TicketCounter::in),
    [will_not_call_mercury, thread_safe],
"
#if MR_USE_TRAIL
    runtime.Errors.SORRY(""foreign code for this function"");
    // MR_prune_tickets_to(TicketCounter);
#endif
").
:- pragma foreign_proc("Java",
    prune_tickets_to(_TicketCounter::in),
    [will_not_call_mercury, thread_safe],
"
    // XXX No trailing for the Java back-end, so take no action.
").

%---------------------------------------------------------------------------%
%
% This section of the module contains predicates and types that are used
% internally by the compiler for manipulating the heap.
%
% These predicates should not be used by user programs directly.
%

:- interface.

    % free_heap/1 is used internally by the compiler to implement compile-time
    % garbage collection. (Note that currently compile-time garbage collection
    % is not yet fully implemented.)
    %
    % free_heap/1 explicitly deallocates a cell on the heap. It works by
    % calling GC_free(), which will put the cell on the appropriate free list.
    % It can only be used when doing conservative GC, since with `--gc none'
    % or `--gc accurate', allocation does not use a free list. The `di' mode
    % on the argument is overly conservative -- only the top-level cell is
    % clobbered. This is handled correctly by recompute_instmap_delta in
    % mode_util.
    %
:- impure pred free_heap(T::di) is det.

:- type mutvar(T)
    --->    mutvar(c_pointer).
            % A no_tag type, i.e. the representation is just a c_pointer.

    % gc_trace/1 is used for accurate garbage collection in the MLDS->C
    % backend. It takes as parameters a pointer to a variable (normally on
    % the stack) and, implicitly, a type_info which describes the type of
    % that variable. It traverses the heap object(s) pointed to by that
    % variable, copying them to the new heap area, and updating the variable
    % to point to the new copy. This is done by calling MR_agc_deep_copy()
    % (from runtime/mercury_deep_copy*).
    %
:- impure pred gc_trace(mutvar(T)::in) is det.

    % mark_hp/1 and restore_hp/1 are used by the MLDS back-end, to implement
    % heap reclamation on failure. (The LLDS back-end does not use these;
    % instead it inserts the corresponding LLDS instructions directly during
    % code generation.) For documentation, see the corresponding LLDS
    % instructions in compiler/llds.m. See also compiler/notes/trailing.html.

:- type heap_pointer.

:- impure pred mark_hp(heap_pointer::out) is det.
:- impure pred restore_hp(heap_pointer::in) is det.

    % The following is a built-in reference type. It is used to define the
    % types store.generic_ref/2, store.generic_mutvar/2, solutions.mutvar/1,
    % benchmarking.int_ref/0, etc.
:- type ref(T).

:- implementation.

% These routines are defined in C in ways which may make it not obvious
% to the Mercury compiler that they are worth inlining.
%
% (Note: it is probably not worth inlining gc_trace/1...)

:- pragma inline(free_heap/1).
:- pragma inline(mark_hp/1).
:- pragma inline(restore_hp/1).

:- pragma foreign_decl("C", "
    #include ""mercury_heap.h"" // for MR_free_heap()
").

%---------------------%

:- pragma foreign_proc("C",
    gc_trace(Pointer::in),
    [will_not_call_mercury, thread_safe, will_not_modify_trail],
"
#ifdef MR_NATIVE_GC
    * (MR_Word *) Pointer =
        MR_agc_deep_copy(* (MR_Word *) Pointer, (MR_TypeInfo) TypeInfo_for_T,
            MR_ENGINE(MR_eng_heap_zone2->MR_zone_min),
            MR_ENGINE(MR_eng_heap_zone2->MR_zone_hardmax));
#else
    MR_fatal_error(""private_builtin.gc_trace/2: ""
        ""called when accurate GC not enabled"");
#endif
").
:- pragma foreign_proc("Java",
    gc_trace(_Pointer::in),
    [will_not_call_mercury, thread_safe],
"
    // For the Java back-end, we use the Java garbage collector,
    // so we take no action here.
").

%---------------------%

:- pragma foreign_proc("C",
    free_heap(Val::di),
    [will_not_call_mercury, thread_safe, will_not_modify_trail],
"
    MR_free_heap((void *) Val);
").
:- pragma foreign_proc("Java",
    free_heap(_Val::di),
    [will_not_call_mercury, thread_safe],
"
    // For the Java back-end, we don't define our own heaps.
    // So take no action here.
").

%---------------------%

:- pragma foreign_proc("C",
    mark_hp(SavedHeapPointer::out),
    [will_not_call_mercury, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
#ifndef MR_CONSERVATIVE_GC
    MR_mark_hp(SavedHeapPointer);
#else
    // We can't do heap reclamation with conservative GC.
    SavedHeapPointer = 0;
#endif
").
:- pragma foreign_proc("C#",
    mark_hp(SavedHeapPointer::out),
    [will_not_call_mercury, thread_safe],
"
    // We can't do heap reclamation on failure in the .NET back-end.
    SavedHeapPointer = null;
").
:- pragma foreign_proc("Java",
    mark_hp(SavedHeapPointer::out),
    [will_not_call_mercury, thread_safe],
"
    // We can't do heap reclamation on failure in the Java back-end.
    SavedHeapPointer = null;
").

%---------------------%

:- pragma foreign_proc("C",
    restore_hp(SavedHeapPointer::in),
    [will_not_call_mercury, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
#ifndef MR_CONSERVATIVE_GC
    MR_restore_hp(SavedHeapPointer);
#endif
").
:- pragma foreign_proc("C#",
    restore_hp(_SavedHeapPointer::in),
    [will_not_call_mercury, thread_safe],
"
    // We can't do heap reclamation on failure in the .NET back-end.
").
:- pragma foreign_proc("Java",
    restore_hp(_SavedHeapPointer::in),
    [will_not_call_mercury, thread_safe],
"
    // We can't do heap reclamation on failure in the Java back-end.
").

%---------------------------------------------------------------------------%

% Code to define the `heap_pointer' and `ref' types for the .NET back-end.
% (For the C back-ends, they're defined in runtime/mercury_builtin_types.[ch].)

:- pragma foreign_code("C#", "

public static bool
__Unify__private_builtin__heap_pointer_0_0(object[] x, object[] y)
{
    runtime.Errors.fatal_error(
        ""called unify for type `private_builtin:heap_pointer'"");
    return false;
}

public static void
__Compare__private_builtin__heap_pointer_0_0(
    ref object[] result, object[] x, object[] y)
{
    runtime.Errors.fatal_error(
        ""called compare/3 for type `private_builtin:heap_pointer'"");
}

public static bool
__Unify__private_builtin__ref_1_0(
    object[] type_info, object[] x, object[] y)
{
    return x == y;
}

public static void
__Compare__private_builtin__ref_1_0(
    object[] type_info, ref object[] result, object[] x, object[] y)
{
    runtime.Errors.fatal_error(
        ""called compare/3 for type `private_builtin.ref'"");
}

").

%---------------------------------------------------------------------------%
%
% This section of the module is for miscellaneous predicates
% that sometimes have calls to them emitted by the compiler.
%

:- interface.

    % unsafe_type_cast/2 is used internally by the compiler.
    % Bad things will happen if this is used in programs.
    % With the LLDS back-end, it has no definition,
    % since for efficiency the code generator treats it as a builtin.
    % With the MLDS back-end, it is defined in runtime/mercury.h.
    %
:- pred unsafe_type_cast(T1::in, T2::out) is det.

    % store_at_ref_impure/2 is used internally by the compiler.
    % Bad things will happen if this is used in programs.
    %
:- impure pred store_at_ref_impure(store_at_ref_type(T)::in, T::in) is det.

    % This type should be used only by the program transformation that
    % introduces calls to store_at_ref_impure. Any other use is will cause
    % bad things to happen.
:- type store_at_ref_type(T)
    --->    store_at_ref_type(int).

    % unused/0 should never be called.
    % The compiler sometimes generates references to this procedure,
    % but they should never get executed.
:- pred unused is det.

:- pred nyi_foreign_type_unify(T::in, T::in) is semidet.
:- pred nyi_foreign_type_compare(comparison_result::uo, T::in, T::in) is det.

:- semipure pred trace_evaluate_runtime_condition is semidet.

    % unify_remote_arg_words(TermVarX, TermVarY, Ptag, CellOffsetVar):
    %
    % Succeed iff the argument words at the given offset are the same
    % in TermVarX and TermVarY.
    %
:- pred unify_remote_arg_words(T::in, T::in, int::in, int::in) is semidet.

    % compare_remote_uint_words(TermVarX, TermVarY,
    %   Ptag, CellOffsetVar, ResultVar):
    %
    % Set ResultVar to the result of the unsigned comparison between
    % two bitfields in the memory cells of TermVarX and TermVarY.
    % The bitfields occupy the entirety of the words at offset
    % CellOffsetVar.
    %
:- pred compare_remote_uint_words(T::in, T::in, int::in, int::in,
    comparison_result::uo) is det.

    % compare_remote_uint_bitfields(TermVarX, TermVarY,
    %   Ptag, CellOffsetVar, ShiftVar, NumBitsVar, ResultVar):
    %
    % Set ResultVar to the result of the unsigned comparison between
    % two bitfields in the memory cells of TermVarX and TermVarY.
    % The bitfields are in the word at offset CellOffsetVar, with the
    % LSBs of the bitfields being ShiftVar bits from the LSB of the word.
    % Their size is NumBitsVar bits.
    %
:- pred compare_remote_uint_bitfields(T::in, T::in, int::in, int::in,
    int::in, int::in, comparison_result::uo) is det.

    % compare_remote_int{8,16,32}_bitfields(TermVarX, TermVarY,
    %   Ptag, CellOffsetVar, ShiftVar, ResultVar):
    %
    % Set ResultVar to the result of the signed comparison between two
    % {8,16,32} bit bitfields in the memory cells of TermVarX and TermVarY.
    % The bitfields are in the word at offset CellOffsetVar, with the
    % LSBs of the bitfields being ShiftVar bits from the LSB of the word.
    %
:- pred compare_remote_int8_bitfields(T::in, T::in, int::in, int::in, int::in,
    comparison_result::uo) is det.
:- pred compare_remote_int16_bitfields(T::in, T::in, int::in, int::in, int::in,
    comparison_result::uo) is det.
:- pred compare_remote_int32_bitfields(T::in, T::in, int::in, int::in, int::in,
    comparison_result::uo) is det.

    % compare_local_uint_words(TermVarX, TermVarY, ResultVar):
    %
    % Set ResultVar to the result of the unsigned comparison between
    % two bitfields in TermVarX and TermVarY.
    % The bitfields occupy the entirety of the words containing
    % the terms themselves.
    %
:- pred compare_local_uint_words(T::in, T::in,
    comparison_result::uo) is det.

    % compare_local_uint_bitfields(TermVarX, TermVarY,
    %   ShiftVar, NumBitsVar, ResultVar):
    %
    % Set ResultVar to the result of the unsigned comparison between
    % two bitfields in TermVarX and TermVarY.
    % The bitfields are in the term words themselves, with the LSBs
    % of the bitfields being ShiftVar bits from the LSB of the word.
    % Their size is NumBitsVar bits.
    %
:- pred compare_local_uint_bitfields(T::in, T::in, int::in, int::in,
    comparison_result::uo) is det.

    % compare_local_int{8,16,32}_bitfields(TermVarX, TermVarY,
    %   ShiftVar, ResultVar):
    %
    % Set ResultVar to the result of the signed comparison between two
    % {8,16,32} bit bitfields in TermVarX and TermVarY.
    % The bitfields are in the term words themselves, with the LSBs
    % of the bitfields being ShiftVar bits from the LSB of the word.
    %
:- pred compare_local_int8_bitfields(T::in, T::in, int::in,
    comparison_result::uo) is det.
:- pred compare_local_int16_bitfields(T::in, T::in, int::in,
    comparison_result::uo) is det.
:- pred compare_local_int32_bitfields(T::in, T::in, int::in,
    comparison_result::uo) is det.

:- implementation.

unused :-
    ( if semidet_succeed then
        error("attempted use of dead predicate")
    else
        % the following is never executed
        true
    ).

nyi_foreign_type_unify(_, _) :-
    ( if semidet_succeed then
        sorry("unify for foreign types")
    else
        semidet_succeed
    ).

nyi_foreign_type_compare(Result, _, _) :-
    ( if semidet_succeed then
        sorry("compare for foreign types")
    else
        Result = (=)
    ).

:- pragma foreign_proc("C",
    trace_evaluate_runtime_condition,
    [will_not_call_mercury, thread_safe, promise_semipure,
        does_not_affect_liveness],
"
    /* All uses of this predicate should override the body. */
    MR_fatal_error(""trace_evaluate_runtime_condition called"");
").
:- pragma foreign_proc("C#",
    trace_evaluate_runtime_condition,
    [will_not_call_mercury, thread_safe, promise_semipure,
        does_not_affect_liveness],
"
    // All uses of this predicate should override the body.
    throw new System.Exception(
        ""trace_evaluate_runtime_condition called"");
").
:- pragma foreign_proc("Java",
    trace_evaluate_runtime_condition,
    [will_not_call_mercury, thread_safe, promise_semipure,
        does_not_affect_liveness, may_not_duplicate],
"
    if (true) {
        /* All uses of this predicate should override the body. */
        throw new java.lang.RuntimeException(
            ""trace_evaluate_runtime_condition called"");
    }
").

%---------------------%

:- pragma foreign_proc("C",
    unify_remote_arg_words(TermVarX::in, TermVarY::in,
        Ptag::in, CellOffsetVar::in),
    [will_not_call_mercury, thread_safe, promise_pure,
        does_not_affect_liveness, may_not_duplicate],
"
    // All uses of this predicate should override the body,
    // but just in case they don't ...
    MR_Unsigned *cell_x;
    MR_Unsigned *cell_y;
    cell_x = (MR_Unsigned *) (((MR_Unsigned) TermVarX) - (MR_Unsigned) Ptag);
    cell_y = (MR_Unsigned *) (((MR_Unsigned) TermVarY) - (MR_Unsigned) Ptag);
    MR_Unsigned word_x = cell_x[CellOffsetVar];
    MR_Unsigned word_y = cell_y[CellOffsetVar];
    SUCCESS_INDICATOR = (word_x == word_y);
").

unify_remote_arg_words(_, _, _, _) :-
    semidet_fail.

%---------------------%

:- pragma foreign_proc("C",
    compare_remote_uint_words(TermVarX::in, TermVarY::in,
        Ptag::in, CellOffsetVar::in, ResultVar::uo),
    [will_not_call_mercury, thread_safe, promise_pure,
        does_not_affect_liveness, may_not_duplicate],
"
    // All uses of this predicate should override the body,
    // but just in case they don't ...
    MR_Unsigned *cell_x;
    MR_Unsigned *cell_y;
    cell_x = (MR_Unsigned *) (((MR_Unsigned) TermVarX) - (MR_Unsigned) Ptag);
    cell_y = (MR_Unsigned *) (((MR_Unsigned) TermVarY) - (MR_Unsigned) Ptag);
    MR_Unsigned word_x = cell_x[CellOffsetVar];
    MR_Unsigned word_y = cell_y[CellOffsetVar];
    if (word_x < word_y) {
        ResultVar = MR_COMPARE_LESS;
    } else if (word_x > word_y) {
        ResultVar = MR_COMPARE_GREATER;
    } else {
        ResultVar = MR_COMPARE_EQUAL;
    }
").

compare_remote_uint_words(_, _, _, _, Result) :-
    ( if semidet_fail then
        Result = (=)
    else
        error("compare_remote_int32_words called")
    ).

%---------------------%

:- pragma foreign_proc("C",
    compare_remote_uint_bitfields(TermVarX::in, TermVarY::in,
        Ptag::in, CellOffsetVar::in, ShiftVar::in, NumBitsVar::in,
        ResultVar::uo),
    [will_not_call_mercury, thread_safe, promise_pure,
        does_not_affect_liveness, may_not_duplicate],
"
    // All uses of this predicate should override the body,
    // but just in case they don't ...
    MR_Unsigned *cell_x;
    MR_Unsigned *cell_y;
    cell_x = (MR_Unsigned *) (((MR_Unsigned) TermVarX) - (MR_Unsigned) Ptag);
    cell_y = (MR_Unsigned *) (((MR_Unsigned) TermVarY) - (MR_Unsigned) Ptag);
    MR_Unsigned word_x = cell_x[CellOffsetVar];
    MR_Unsigned word_y = cell_y[CellOffsetVar];
    MR_Unsigned value_x = ((word_x >> ShiftVar) & ((1 << NumBitsVar) - 1));
    MR_Unsigned value_y = ((word_y >> ShiftVar) & ((1 << NumBitsVar) - 1));
    if (value_x < value_y) {
        ResultVar = MR_COMPARE_LESS;
    } else if (value_x > value_y) {
        ResultVar = MR_COMPARE_GREATER;
    } else {
        ResultVar = MR_COMPARE_EQUAL;
    }
").

compare_remote_uint_bitfields(_, _, _, _, _, _, Result) :-
    ( if semidet_fail then
        Result = (=)
    else
        error("compare_remote_int32_bitfields called")
    ).

%---------------------%

:- pragma foreign_proc("C",
    compare_remote_int8_bitfields(TermVarX::in, TermVarY::in,
        Ptag::in, CellOffsetVar::in, ShiftVar::in, ResultVar::uo),
    [will_not_call_mercury, thread_safe, promise_pure,
        does_not_affect_liveness, may_not_duplicate],
"
    // All uses of this predicate should override the body,
    // but just in case they don't ...
    MR_Unsigned *cell_x;
    MR_Unsigned *cell_y;
    cell_x = (MR_Unsigned *) (((MR_Unsigned) TermVarX) - (MR_Unsigned) Ptag);
    cell_y = (MR_Unsigned *) (((MR_Unsigned) TermVarY) - (MR_Unsigned) Ptag);
    MR_Unsigned word_x = cell_x[CellOffsetVar];
    MR_Unsigned word_y = cell_y[CellOffsetVar];
    int8_t value_x = (int8_t) ((word_x >> ShiftVar) & ((1 << 8) - 1));
    int8_t value_y = (int8_t) ((word_y >> ShiftVar) & ((1 << 8) - 1));
    if (value_x < value_y) {
        ResultVar = MR_COMPARE_LESS;
    } else if (value_x > value_y) {
        ResultVar = MR_COMPARE_GREATER;
    } else {
        ResultVar = MR_COMPARE_EQUAL;
    }
").

compare_remote_int8_bitfields(_, _, _, _, _, Result) :-
    ( if semidet_fail then
        Result = (=)
    else
        error("compare_remote_int8_bitfields called")
    ).

%---------------------%

:- pragma foreign_proc("C",
    compare_remote_int16_bitfields(TermVarX::in, TermVarY::in,
        Ptag::in, CellOffsetVar::in, ShiftVar::in, ResultVar::uo),
    [will_not_call_mercury, thread_safe, promise_pure,
        does_not_affect_liveness, may_not_duplicate],
"
    // All uses of this predicate should override the body,
    // but just in case they don't ...
    MR_Unsigned *cell_x;
    MR_Unsigned *cell_y;
    cell_x = (MR_Unsigned *) (((MR_Unsigned) TermVarX) - (MR_Unsigned) Ptag);
    cell_y = (MR_Unsigned *) (((MR_Unsigned) TermVarY) - (MR_Unsigned) Ptag);
    MR_Unsigned word_x = cell_x[CellOffsetVar];
    MR_Unsigned word_y = cell_y[CellOffsetVar];
    int16_t value_x = (int16_t) ((word_x >> ShiftVar) & ((1 << 16) - 1));
    int16_t value_y = (int16_t) ((word_y >> ShiftVar) & ((1 << 16) - 1));
    if (value_x < value_y) {
        ResultVar = MR_COMPARE_LESS;
    } else if (value_x > value_y) {
        ResultVar = MR_COMPARE_GREATER;
    } else {
        ResultVar = MR_COMPARE_EQUAL;
    }
").

compare_remote_int16_bitfields(_, _, _, _, _, Result) :-
    ( if semidet_fail then
        Result = (=)
    else
        error("compare_remote_int16_bitfields called")
    ).

%---------------------%

:- pragma foreign_proc("C",
    compare_remote_int32_bitfields(TermVarX::in, TermVarY::in,
        Ptag::in, CellOffsetVar::in, ShiftVar::in, ResultVar::uo),
    [will_not_call_mercury, thread_safe, promise_pure,
        does_not_affect_liveness, may_not_duplicate],
"
#ifdef MR_MERCURY_IS_64_BITS
    // All uses of this predicate should override the body,
    // but just in case they don't ...
    MR_Unsigned *cell_x;
    MR_Unsigned *cell_y;
    cell_x = (MR_Unsigned *) (((MR_Unsigned) TermVarX) - (MR_Unsigned) Ptag);
    cell_y = (MR_Unsigned *) (((MR_Unsigned) TermVarY) - (MR_Unsigned) Ptag);
    MR_Unsigned word_x = cell_x[CellOffsetVar];
    MR_Unsigned word_y = cell_y[CellOffsetVar];
    int32_t value_x =
        (int32_t) ((word_x >> ShiftVar) & ((INT64_C(1) << 32) - 1));
    int32_t value_y =
        (int32_t) ((word_y >> ShiftVar) & ((INT64_C(1) << 32) - 1));
    if (value_x < value_y) {
        ResultVar = MR_COMPARE_LESS;
    } else if (value_x > value_y) {
        ResultVar = MR_COMPARE_GREATER;
    } else {
        ResultVar = MR_COMPARE_EQUAL;
    }
#else
    MR_fatal_error(""compare_remote_int32_bitfields called on ""
        ""non-64-bit system"");
#endif
").

compare_remote_int32_bitfields(_, _, _, _, _, Result) :-
    ( if semidet_fail then
        Result = (=)
    else
        error("compare_remote_int32_bitfields called")
    ).

%---------------------%

:- pragma foreign_proc("C",
    compare_local_uint_words(TermVarX::in, TermVarY::in,
        ResultVar::uo),
    [will_not_call_mercury, thread_safe, promise_pure,
        does_not_affect_liveness, may_not_duplicate],
"
    // All uses of this predicate should override the body,
    // but just in case they don't ...
    MR_Unsigned value_x = (MR_Unsigned) TermVarX;
    MR_Unsigned value_y = (MR_Unsigned) TermVarY;
    if (value_x < value_y) {
        ResultVar = MR_COMPARE_LESS;
    } else if (value_x > value_y) {
        ResultVar = MR_COMPARE_GREATER;
    } else {
        ResultVar = MR_COMPARE_EQUAL;
    }
").

compare_local_uint_words(_, _, Result) :-
    ( if semidet_fail then
        Result = (=)
    else
        error("compare_local_uint_words called")
    ).

%---------------------%

:- pragma foreign_proc("C",
    compare_local_uint_bitfields(TermVarX::in, TermVarY::in,
        ShiftVar::in, NumBitsVar::in, ResultVar::uo),
    [will_not_call_mercury, thread_safe, promise_pure,
        does_not_affect_liveness, may_not_duplicate],
"
    // All uses of this predicate should override the body,
    // but just in case they don't ...
    MR_Unsigned word_x = (MR_Unsigned) TermVarX;
    MR_Unsigned word_y = (MR_Unsigned) TermVarY;
    MR_Unsigned value_x = ((word_x >> ShiftVar) & ((1 << NumBitsVar) - 1));
    MR_Unsigned value_y = ((word_y >> ShiftVar) & ((1 << NumBitsVar) - 1));
    if (value_x < value_y) {
        ResultVar = MR_COMPARE_LESS;
    } else if (value_x > value_y) {
        ResultVar = MR_COMPARE_GREATER;
    } else {
        ResultVar = MR_COMPARE_EQUAL;
    }
").

compare_local_uint_bitfields(_, _, _, _, Result) :-
    ( if semidet_fail then
        Result = (=)
    else
        error("compare_local_uint_bitfields called")
    ).

%---------------------%

:- pragma foreign_proc("C",
    compare_local_int8_bitfields(TermVarX::in, TermVarY::in,
        ShiftVar::in, ResultVar::uo),
    [will_not_call_mercury, thread_safe, promise_pure,
        does_not_affect_liveness, may_not_duplicate],
"
    // All uses of this predicate should override the body,
    // but just in case they don't ...
    MR_Unsigned word_x = (MR_Unsigned) TermVarX;
    MR_Unsigned word_y = (MR_Unsigned) TermVarY;
    int8_t value_x = (int8_t) ((word_x >> ShiftVar) & ((1 << 8) - 1));
    int8_t value_y = (int8_t) ((word_y >> ShiftVar) & ((1 << 8) - 1));
    if (value_x < value_y) {
        ResultVar = MR_COMPARE_LESS;
    } else if (value_x > value_y) {
        ResultVar = MR_COMPARE_GREATER;
    } else {
        ResultVar = MR_COMPARE_EQUAL;
    }
").

compare_local_int8_bitfields(_, _, _, Result) :-
    ( if semidet_fail then
        Result = (=)
    else
        error("compare_local_int8_bitfields called")
    ).

%---------------------%

:- pragma foreign_proc("C",
    compare_local_int16_bitfields(TermVarX::in, TermVarY::in,
        ShiftVar::in, ResultVar::uo),
    [will_not_call_mercury, thread_safe, promise_pure,
        does_not_affect_liveness, may_not_duplicate],
"
    // All uses of this predicate should override the body,
    // but just in case they don't ...
    MR_Unsigned word_x = (MR_Unsigned) TermVarX;
    MR_Unsigned word_y = (MR_Unsigned) TermVarY;
    int16_t value_x = (int16_t) ((word_x >> ShiftVar) & ((1 << 16) - 1));
    int16_t value_y = (int16_t) ((word_y >> ShiftVar) & ((1 << 16) - 1));
    if (value_x < value_y) {
        ResultVar = MR_COMPARE_LESS;
    } else if (value_x > value_y) {
        ResultVar = MR_COMPARE_GREATER;
    } else {
        ResultVar = MR_COMPARE_EQUAL;
    }
").

compare_local_int16_bitfields(_, _, _, Result) :-
    ( if semidet_fail then
        Result = (=)
    else
        error("compare_local_int16_bitfields called")
    ).

%---------------------%

:- pragma foreign_proc("C",
    compare_local_int32_bitfields(TermVarX::in, TermVarY::in,
        ShiftVar::in, ResultVar::uo),
    [will_not_call_mercury, thread_safe, promise_pure,
        does_not_affect_liveness, may_not_duplicate],
"
#ifdef MR_MERCURY_IS_64_BITS
    // All uses of this predicate should override the body,
    // but just in case they don't ...
    MR_Unsigned word_x = (MR_Unsigned) TermVarX;
    MR_Unsigned word_y = (MR_Unsigned) TermVarY;
    int32_t value_x =
        (int32_t) ((word_x >> ShiftVar) & ((INT64_C(1) << 32) - 1));
    int32_t value_y =
        (int32_t) ((word_y >> ShiftVar) & ((INT64_C(1) << 32) - 1));
    if (value_x < value_y) {
        ResultVar = MR_COMPARE_LESS;
    } else if (value_x > value_y) {
        ResultVar = MR_COMPARE_GREATER;
    } else {
        ResultVar = MR_COMPARE_EQUAL;
    }
#else
    MR_fatal_error(""compare_local_int32_bitfields called on ""
        ""non-64-bit system"");
#endif
").

compare_local_int32_bitfields(_, _, _, Result) :-
    ( if semidet_fail then
        Result = (=)
    else
        error("compare_local_int32_bitfields called")
    ).

%---------------------------------------------------------------------------%
%
% This section of the module is for miscellaneous predicates
% that are useful in other modules of the Mercury standard library.
%

:- interface.

    % var/1 is intended to make it possible to write code that effectively has
    % different implementations for different modes. It has to be impure
    % to ensure that reordering doesn't cause the wrong mode to be selected.
    %
:- impure pred var(T).
:-    mode var(ui) is failure.
:-    mode var(in) is failure.
:-    mode var(unused) is det.

:- impure pred nonvar(T).
:-    mode nonvar(ui) is det.
:-    mode nonvar(in) is det.
:-    mode nonvar(unused) is failure.

    % no_clauses/1 is used to report a run-time error when there is a call
    % to a procedure for which there are no clauses, and the procedure was
    % compiled with `--allow-stubs' and is not part of the Mercury standard
    % library. (If the procedure is part of the Mercury standard library,
    % the compiler will generate a call to sorry/1 instead of no_clauses/1.)
    %
:- pred no_clauses(string::in) is erroneous.

    % sorry/1 is used to apologize about the fact that we have not implemented
    % some predicate or function in the Mercury standard library for a given
    % back end. The argument should give the name of the predicate or function.
    %
:- pred sorry(string::in) is erroneous.

    % imp/0 is used to make pure predicates impure.
    %
:- impure pred imp is det.

    % semip/0 is used to make pure predicates semipure.
    %
:- semipure pred semip is det.

%---------------------------------------------------------------------------%

:- implementation.

var(_::ui) :- fail.
var(_::in) :- fail.
var(_::unused) :- true.

nonvar(_::ui) :- true.
nonvar(_::in) :- true.
nonvar(_::unused) :- fail.

no_clauses(PredName) :-
    error("no clauses for " ++ PredName).

sorry(PredName) :-
    error("sorry, " ++ PredName ++ " not implemented\n" ++
        "for this target language (or compiler back-end).").

:- pragma foreign_proc("C",
    imp,
    [will_not_call_mercury, thread_safe, will_not_modify_trail],
"").
:- pragma foreign_proc("C#",
    imp,
    [will_not_call_mercury, thread_safe],
"").
:- pragma foreign_proc("Java",
    imp,
    [will_not_call_mercury, thread_safe],
"").

:- pragma foreign_proc("C",
    semip,
    [will_not_call_mercury, thread_safe, promise_semipure,
        will_not_modify_trail],
"").
:- pragma foreign_proc("C#",
    semip,
    [will_not_call_mercury, thread_safe, promise_semipure],
"").
:- pragma foreign_proc("Java",
    semip,
    [will_not_call_mercury, thread_safe, promise_semipure],
"").

%---------------------------------------------------------------------------%
%
% Foreign language code defining miscellaneous stuff related to types.
% XXX The foreign_code pragmas below define different things for the
% different languages. All these should probably be somewhere else.
%

:- pragma foreign_decl("C", "

#include ""mercury_builtin_types.h""

MR_DECLARE_TYPE_CTOR_INFO_STRUCT(MR_TYPE_CTOR_INFO_NAME(list, list, 1));
MR_DECLARE_TYPE_CTOR_INFO_STRUCT(MR_TYPE_CTOR_INFO_NAME(univ, univ, 0));

").

:- pragma foreign_code("C", "

const MR_TypeCtorInfo ML_type_ctor_info_for_univ =
    &MR_TYPE_CTOR_INFO_NAME(univ, univ, 0);

const MR_TypeCtorInfo ML_type_info_for_type_info =
    &MR_TYPE_CTOR_INFO_NAME(private_builtin, type_info, 0);

const MR_TypeCtorInfo ML_type_info_for_pseudo_type_info =
    /*
    ** For the time being, we handle pseudo_type_infos the same way
    ** as we handle type_infos.
    */
    &MR_TYPE_CTOR_INFO_NAME(private_builtin, type_info, 0);

const MR_FA_TypeInfo_Struct1 ML_type_info_for_list_of_univ = {
    &MR_TYPE_CTOR_INFO_NAME(list, list, 1),
    { (MR_TypeInfo) &MR_TYPE_CTOR_INFO_NAME(univ, univ, 0) }
};

const MR_FA_TypeInfo_Struct1 ML_type_info_for_list_of_int = {
    &MR_TYPE_CTOR_INFO_NAME(list, list, 1),
    { (MR_TypeInfo) &MR_TYPE_CTOR_INFO_NAME(builtin, int, 0) }
};

const MR_FA_TypeInfo_Struct1 ML_type_info_for_list_of_char = {
    &MR_TYPE_CTOR_INFO_NAME(list, list, 1),
    { (MR_TypeInfo) &MR_TYPE_CTOR_INFO_NAME(builtin, character, 0) }
};

const MR_FA_TypeInfo_Struct1 ML_type_info_for_list_of_string = {
    &MR_TYPE_CTOR_INFO_NAME(list, list, 1),
    { (MR_TypeInfo) &MR_TYPE_CTOR_INFO_NAME(builtin, string, 0) }
};

const MR_FA_TypeInfo_Struct1 ML_type_info_for_list_of_type_info = {
    &MR_TYPE_CTOR_INFO_NAME(list, list, 1),
    { (MR_TypeInfo) &ML_type_info_for_type_info }
};

const MR_FA_TypeInfo_Struct1 ML_type_info_for_list_of_pseudo_type_info = {
    &MR_TYPE_CTOR_INFO_NAME(list, list, 1),
    /*
    ** For the time being, we handle pseudo_type_infos the same way
    ** as we handle type_infos.
    */
    { (MR_TypeInfo) &ML_type_info_for_type_info }
};

").

:- pragma foreign_code("Java", "
    public static class Ref_1
    {
        // XXX stub only
    }

    public static class Heap_pointer_0
    {
        // XXX stub only
    }

    public static final int MR_SECTAG_NONE               = 0;
    public static final int MR_SECTAG_NONE_DIRECT_ARG    = 1;
    public static final int MR_SECTAG_LOCAL              = 2;
    public static final int MR_SECTAG_LOCAL_REST_OF_WORD = 2;   // synonym
    public static final int MR_SECTAG_REMOTE             = 3;
    public static final int MR_SECTAG_REMOTE_FULL_WORD   = 3;   // synonym
    public static final int MR_SECTAG_VARIABLE           = 4;
    // These are never used in Java grades.
    // public static final int MR_SECTAG_LOCAL_BITS      = 5;
    // public static final int MR_SECTAG_REMOTE_BITS     = 6;

    public static final int MR_FUNCTOR_SUBTYPE_NONE      = 0;
    public static final int MR_FUNCTOR_SUBTYPE_EXISTS    = 1;

    public static final int MR_PREDICATE = 0;
    public static final int MR_FUNCTION  = 1;

    // The dummy_var is used to represent io.states and values of other types
    // (dummy types) that contain no information. Occasionally a dummy variable
    // will be used by the code generator as an lval, so we use
    // private_builtin.dummy_var as that lval.
    public static java.lang.Object dummy_var;
").

:- pragma foreign_code("C#", "
    // The dummy_var is used to represent io.states and values of other types
    // (dummy types) that contain no information. Occasionally a dummy variable
    // will be used by the code generator as an lval, so we use
    // private_builtin.dummy_var as that lval.
    public static object dummy_var;
").

:- pragma foreign_code("Java", "
    //
    // Type-specific unification and comparison routines.
    //

    public static boolean
    __Unify____ref_1_0(jmercury.runtime.TypeInfo_Struct ti,
        private_builtin.Ref_1 x, private_builtin.Ref_1 y)
    {
        // stub only
        throw new java.lang.Error
            (""unify/2 for type private_builtin.ref not implemented"");
    }

    public static boolean
    __Unify____heap_pointer_0_0 (private_builtin.Heap_pointer_0 x,
        private_builtin.Heap_pointer_0 y)
    {
        // stub only
        throw new java.lang.Error(""unify/2 for type heap_pointer/0"");
    }

    public static boolean
    __Unify____type_ctor_info_0_0(
        jmercury.runtime.TypeCtorInfo_Struct x,
        jmercury.runtime.TypeCtorInfo_Struct y)
    {
        // stub only
        throw new java.lang.Error
            (""unify/2 for type type_ctor_info/1"");
    }

    public static boolean
    __Unify____type_ctor_info_1_0(jmercury.runtime.TypeInfo_Struct ti,
        jmercury.runtime.TypeCtorInfo_Struct x,
        jmercury.runtime.TypeCtorInfo_Struct y)
    {
        // stub only
        throw new java.lang.Error
            (""unify/2 for type type_ctor_info/1"");
    }

    public static boolean
    __Unify____type_info_0_0(
        jmercury.runtime.TypeInfo_Struct x,
        jmercury.runtime.TypeInfo_Struct y)
    {
        // stub only
        throw new java.lang.Error
            (""unify/2 for type type_info/0"");
    }

    public static boolean
    __Unify____type_info_1_0(jmercury.runtime.TypeInfo_Struct ti,
        jmercury.runtime.TypeInfo_Struct x,
        jmercury.runtime.TypeInfo_Struct y)
    {
        // stub only
        throw new java.lang.Error
            (""unify/2 for type type_info/0"");
    }

    public static boolean
    __Unify____base_typeclass_info_0_0(
        java.lang.Object[] x, java.lang.Object[] y)
    {
        // stub only
        throw new java.lang.Error(""unify/2 for type typeclass_info/0"");
    }

    public static boolean
    __Unify____base_typeclass_info_1_0(jmercury.runtime.TypeInfo_Struct ti,
        java.lang.Object[] x, java.lang.Object[] y)
    {
        // stub only
        throw new java.lang.Error(""unify/2 for type typeclass_info/0"");
    }

    public static boolean
    __Unify____typeclass_info_0_0(java.lang.Object[] x, java.lang.Object[] y)
    {
        // stub only
        throw new java.lang.Error
            (""unify/2 for type typeclass_info/1"");
    }

    public static boolean
    __Unify____typeclass_info_1_0(jmercury.runtime.TypeInfo_Struct ti,
        java.lang.Object[] x, java.lang.Object[] y)
    {
        // stub only
        throw new java.lang.Error
            (""unify/2 for type typeclass_info/1"");
    }

    public static builtin.Comparison_result_0
    __Compare____ref_1_0(jmercury.runtime.TypeInfo_Struct ti,
        private_builtin.Ref_1 x, private_builtin.Ref_1 y)
    {
        // stub only
        throw new java.lang.Error
            (""called compare/3 for type private_builtin.ref"");
    }

    public static builtin.Comparison_result_0
    __Compare____heap_pointer_0_0 (
        private_builtin.Heap_pointer_0 x,
        private_builtin.Heap_pointer_0 y)
    {
        // stub only
        throw new java.lang.Error(""compare/2 for type heap_pointer/0"");
    }

    public static builtin.Comparison_result_0
    __Compare____type_ctor_info_0_0(
        jmercury.runtime.TypeCtorInfo_Struct x,
        jmercury.runtime.TypeCtorInfo_Struct y)
    {
        // stub only
        throw new java.lang.Error
            (""compare/2 for type type_ctor_info/1"");
    }

    public static builtin.Comparison_result_0
    __Compare____type_ctor_info_1_0(jmercury.runtime.TypeInfo_Struct ti,
        jmercury.runtime.TypeCtorInfo_Struct x,
        jmercury.runtime.TypeCtorInfo_Struct y)
    {
        // stub only
        throw new java.lang.Error
            (""compare/2 for type type_ctor_info/1"");
    }

    public static builtin.Comparison_result_0
    __Compare____type_info_0_0(
        jmercury.runtime.TypeInfo_Struct x,
        jmercury.runtime.TypeInfo_Struct y)
    {
        // stub only
        throw new java.lang.Error
            (""compare/2 for type type_info/0"");
    }

    public static builtin.Comparison_result_0
    __Compare____type_info_1_0(jmercury.runtime.TypeInfo_Struct ti,
        jmercury.runtime.TypeInfo_Struct x,
        jmercury.runtime.TypeInfo_Struct y)
    {
        // stub only
        throw new java.lang.Error
            (""compare/2 for type type_info/0"");
    }

    public static builtin.Comparison_result_0
    __Compare____base_typeclass_info_0_0(
        java.lang.Object[] x, java.lang.Object[] y)
    {
        // stub only
        throw new java.lang.Error(""compare/2 for type typeclass_info/1"");
    }

    public static builtin.Comparison_result_0
    __Compare____base_typeclass_info_1_0(jmercury.runtime.TypeInfo_Struct ti,
        java.lang.Object[] x, java.lang.Object[] y)
    {
        // stub only
        throw new java.lang.Error(""compare/2 for type typeclass_info/1"");
    }

    public static builtin.Comparison_result_0
    __Compare____typeclass_info_0_0(java.lang.Object[] x, java.lang.Object[] y)
    {
        // stub only
        throw new java.lang.Error
            (""compare/2 for type typeclass_info/0"");
    }

    public static builtin.Comparison_result_0
    __Compare____typeclass_info_1_0(jmercury.runtime.TypeInfo_Struct ti,
        java.lang.Object[] x, java.lang.Object[] y)
    {
        // stub only
        throw new java.lang.Error
            (""compare/2 for type typeclass_info/0"");
    }

").

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
