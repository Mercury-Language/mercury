%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%

% File: private_builtin.m.
% Main authors: fjh, zs.
% Stability: medium.

% This file is automatically imported, as if via `use_module', into every
% module. It is intended for builtins that are just implementation details,
% such as procedures that the compiler generates implicit calls to when
% implementing polymorphism, unification, compare/3, etc.
% Note that the builtins used for tabling and deep profiling are in separate
% modules (table_builtin.m and profiling_builtin.m).

% This module is a private part of the Mercury implementation; user modules
% should never explicitly import this module. The interface for this module
% does not get included in the Mercury library reference manual.

% Many of the predicates defined in this module are builtin - they do not have
% definitions because the compiler generates code for them inline. Some others
% are implemented in the runtime.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module private_builtin.

%-----------------------------------------------------------------------------%

:- interface.

    % This section of the module contains predicates that are used
    % by the compiler, to implement polymorphism. These predicates
    % should not be used by user programs directly.

    % Changes here may also require changes in compiler/polymorphism.m,
    % compiler/unify_proc.m, compiler/higher_order.m and
    % runtime/mercury_type_info.{c,h}.

:- pred builtin_unify_int(int::in, int::in) is semidet.
:- pred builtin_compare_int(comparison_result::uo, int::in, int::in) is det.

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

    % These should never be called -- the compiler never specializes them
    % because the generic compare is just as good as anything we could put
    % here.
    %
:- pred builtin_unify_tuple(T::in, T::in) is semidet.
:- pred builtin_compare_tuple(comparison_result::uo, T::in, T::in) is det.

    % The following pred is used for compare/3 on non-canonical types
    % (types for which there is a `where equality is ...' declaration).
    %
:- pred builtin_compare_non_canonical_type(comparison_result::uo,
        T::in, T::in) is det.

    % Compare_error is used in the code generated for compare/3 preds.
    %
:- pred compare_error is erroneous.

    % The builtin < operator on ints, used in the code generated
    % for compare/3 preds.
    %
:- pred builtin_int_lt(int::in, int::in) is semidet.

    % The builtin > operator on ints, used in the code generated
    % for compare/3 preds.
    %
:- pred builtin_int_gt(int::in, int::in) is semidet.

    % A "typed" version of unify/2 -- i.e. one that can handle arguments
    % of different types.  It first unifies their types, and then if
    % the types are equal it unifies the values.
    %
:- pred typed_unify(T1, T2).
:- mode typed_unify(in, in) is semidet.
:- mode typed_unify(in, out) is semidet.

    % A "typed" version of compare/3 -- i.e. one that can handle arguments
    % of different types.  It first compares the types, and then if the
    % types are equal it compares the values.
    %
:- pred typed_compare(comparison_result::uo, T1::in, T2::in) is det.

    % N.B. interface continued below.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module float.
:- import_module int.
:- import_module list.
:- import_module require.
:- import_module std_util.
:- import_module string.
:- import_module string.

:- pragma foreign_code("C#", "

// The dummy_var is used to represent io__states and other Mercury
// parameters that are not really passed around.  Occasionally a dummy variable
// will be used by the code generator as an lval, so we use
// private_builtin:dummy_var as that lval.

public static object[] dummy_var;

").

:- pragma inline(builtin_compare_int/3).
:- pragma inline(builtin_compare_character/3).
:- pragma inline(builtin_compare_string/3).
:- pragma inline(builtin_compare_float/3).

builtin_unify_int(X, X).

builtin_compare_int(R, X, Y) :-
    ( X < Y ->
        R = (<)
    ; X = Y ->
        R = (=)
    ;
        R = (>)
    ).

builtin_unify_character(C, C).

builtin_compare_character(R, X, Y) :-
    char__to_int(X, XI),
    char__to_int(Y, YI),
    ( XI < YI ->
        R = (<)
    ; XI = YI ->
        R = (=)
    ;
        R = (>)
    ).

builtin_unify_string(S, S).

builtin_compare_string(R, S1, S2) :-
    builtin_strcmp(Res, S1, S2),
    ( Res < 0 ->
        R = (<)
    ; Res = 0 ->
        R = (=)
    ;
        R = (>)
    ).

:- pred builtin_strcmp(int::out, string::in, string::in) is det.

:- pragma foreign_proc("C", builtin_strcmp(Res::out, S1::in, S2::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Res = strcmp(S1, S2);
").

:- pragma foreign_proc("C#", builtin_strcmp(Res::out, S1::in, S2::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Res = System.String.Compare(S1, S2);
").
:- pragma foreign_proc("Java", builtin_strcmp(Res::out, S1::in, S2::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Res = S1.compareTo(S2);
").

builtin_unify_float(F, F).

builtin_compare_float(R, F1, F2) :-
    ( F1 < F2 ->
        R = (<)
    ; F1 > F2 ->
        R = (>)
    ;
        R = (=)
    ).

builtin_unify_tuple(_, _) :-
    ( semidet_succeed ->
        % The generic unification function in the runtime
        % should handle this itself.
        error("builtin_unify_tuple called")
    ;
        % The following is never executed.
        semidet_succeed
    ).

builtin_compare_tuple(Res, _, _) :-
    ( semidet_succeed ->
        % The generic comparison function in the runtime
        % should handle this itself.
        error("builtin_compare_tuple called")
    ;
        % The following is never executed.
        Res = (<)
    ).

:- pragma no_inline(builtin_unify_pred/2).
builtin_unify_pred(_X, _Y) :-
    ( semidet_succeed ->
        error("attempted higher-order unification")
    ;
        % The following is never executed.
        semidet_succeed
    ).

:- pragma no_inline(builtin_compare_pred/3).
builtin_compare_pred(Result, _X, _Y) :-
    ( semidet_succeed ->
        error("attempted higher-order comparison")
    ;
        % The following is never executed.
        Result = (<)
    ).

:- pragma no_inline(builtin_compare_non_canonical_type/3).
builtin_compare_non_canonical_type(Res, X, _Y) :-
    % suppress determinism warning
    ( semidet_succeed ->
        Message = "call to compare/3 for non-canonical type `"
            ++ type_name(type_of(X)) ++ "'",
        error(Message)
    ;
        % The following is never executed.
        Res = (<)
    ).

:- pragma no_inline(compare_error/0).
compare_error :-
    error("internal error in compare/3").

%-----------------------------------------------------------------------------%

typed_unify(X, Y) :-
    ( type_of(X) = type_of(Y) ->
        unsafe_type_cast(X, Y)
    ;
        fail
    ).

typed_compare(R, X, Y) :-
    compare(R0, type_of(X), type_of(Y)),
    ( R0 = (=) ->
        unsafe_type_cast(X, Z),
        compare(R, Z, Y)
    ;
        R = R0
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- interface.

    % This section of the module handles the runtime representation of
    % type information.

    % The actual arities of these two function symbols are variable;
    % they depend on the number of type parameters of the type represented
    % by the type_info, and how many predicates we associate with each type.
    %
    % Note that, since these types look to the compiler as though they
    % are candidates to become no_tag types, special code is required
    % to handle them in type_util:type_is_no_tag_type/3.

:- type type_info(T) ---> type_info(type_ctor_info(T) /*, ... */).
:- type type_ctor_info(T) ---> type_ctor_info(int /*, ... */).

:- type zero_type_info.
:- type zero_type_ctor_info.

    % The type variable in these types isn't really a type variable,
    % it is a place for polymorphism.m to put a representation of the
    % class constraint about which the typeclass_info carries information.
    %
    % Note that, since these types look to the compiler as though they
    % are candidates to become no_tag types, special code is required
    % to handle them in type_util:type_is_no_tag_type/3.

:- type typeclass_info(T) ---> typeclass_info(base_typeclass_info(T)
                        /*, ... */).
:- type base_typeclass_info(_) ---> typeclass_info(int /*, ... */).

:- type zero_typeclass_info.
:- type zero_base_typeclass_info.

    % The following types are used by compiler/ml_code_util.m as the types
    % used for copying type_info/1 and typeclass_info/1 types.
    % XXX Document me better
    %
:- type sample_type_info ---> sample_type_info(type_info(int)).
:- type sample_typeclass_info ---> sample_typeclass_info(typeclass_info(int)).

    % type_info_from_typeclass_info(TypeClassInfo, Index, TypeInfo):
    %
    % Extracts TypeInfo from TypeClassInfo, where TypeInfo is the Indexth
    % type_info in the typeclass_info.
    %
    % Note: Index must be equal to the number of the desired type_info
    % plus the number of superclasses for this class.
    %
:- pred type_info_from_typeclass_info(typeclass_info(_)::in, int::in,
    type_info(T)::out) is det.
:- pred zero_type_info_from_typeclass_info(zero_typeclass_info::in, int::in,
    zero_type_info::out) is det.

    % unconstrained_type_info_from_typeclass_info(TypeClassInfo, 
    %   Index, TypeInfo):
    %
    % Extracts the TypeInfo for the Indexth unconstrained type variable
    % from the instance represented by TypeClassInfo.
    %
:- pred unconstrained_type_info_from_typeclass_info(typeclass_info(_)::in,
    int::in, type_info(_)::out) is det.
:- pred zero_unconstrained_type_info_from_typeclass_info(
    zero_typeclass_info::in, int::in, zero_type_info::out) is det.

    % superclass_from_typeclass_info(TypeClassInfo, Index, SuperClass):
    %
    % Extracts SuperClass from TypeClassInfo where SuperClass is the
    % Indexth superclass of the class.
    %
:- pred superclass_from_typeclass_info(typeclass_info(_)::in,
    int::in, typeclass_info(_)::out) is det.
:- pred zero_superclass_from_typeclass_info(zero_typeclass_info::in,
    int::in, zero_typeclass_info::out) is det.

    % instance_constraint_from_typeclass_info(TypeClassInfo, Index,
    %   InstanceConstraintTypeClassInfo):
    %
    % Extracts the typeclass_info for the Indexth typeclass constraint
    % of the instance described by TypeClassInfo.
    %
    % Note: Index must be equal to the number of the desired constraint
    % plus the number of unconstrained type variables for this instance.
    %
:- pred instance_constraint_from_typeclass_info(typeclass_info(_)::in,
    int::in, typeclass_info(_)::out) is det.
:- pred zero_instance_constraint_from_typeclass_info(zero_typeclass_info::in,
    int::in, zero_typeclass_info::out) is det.

    % N.B. interface continued below.

%-----------------------------------------------------------------------------%

:- implementation.

    % The definitions for type_ctor_info/1 and type_info/1.

:- pragma foreign_code("C#", "

public static object[] MR_typeclass_info_param_type_info(object[] tcinfo,
    int index)
{
    object[] tmp;
    int t1;

    tmp = (object[]) tcinfo[0];
    t1 = System.Convert.ToInt32(tmp[0]) + index;
    return (object[]) tcinfo[t1];
}
public static object[] MR_typeclass_info_instance_tvar_type_info(
    object[] tcinfo, int index) 
{
    return (object[]) tcinfo[index];
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

public static TypeInfo_Struct
MR_typeclass_info_param_type_info(/* typeclass_info */ Object[] tcinfo,
    int index)
{
    /* typeclass_info */ Object[] base_tcinfo;
    int t1;

    base_tcinfo = (Object[]) tcinfo[0];
    t1 = ((Integer) base_tcinfo[0]).intValue() + index;
    return (TypeInfo_Struct) tcinfo[t1];
}

public static TypeInfo_Struct MR_typeclass_info_instance_tvar_type_info(
    /* typeclass_info */ Object[] tcinfo, int index) 
{
    return (TypeInfo_Struct) tcinfo[index];
}

public static /* typeclass_info */ Object[] MR_typeclass_info_superclass_info(
    /* typeclass_info */ Object[] tcinfo, int index)
{
    /* typeclass_info */ Object[] base_tcinfo;
    int t1;

    base_tcinfo = (Object[]) tcinfo[0];
    t1 = ((Integer) base_tcinfo[0]).intValue() + index;
    return (/* typeclass_info */ Object[]) tcinfo[t1];
}

public static /* typeclass_info */ Object[] MR_typeclass_info_arg_typeclass_info(
    /* typeclass_info */ Object[] tcinfo, int index) 
{
    return (/* typeclass_info */ Object[]) tcinfo[index];
}

").

:- pragma foreign_code("C#", "

    // XXX These static constants are duplicated both here and in
    // mercury_dotnet.cs.in.

    // This is because other library modules reference them
    // from .NET code (so they depend on the versions in the runtime to
    // make the dependencies simple) whereas the compiler generates
    // references to the ones here. 

    // See runtime/mercury_dotnet.cs.in for discussion of why we aren't
    // using enums or const static ints here.

public static int MR_TYPECTOR_REP_ENUM                  = 0;
public static int MR_TYPECTOR_REP_ENUM_USEREQ           = 1;
public static int MR_TYPECTOR_REP_DU                    = 2;
public static int MR_TYPECTOR_REP_DU_USEREQ             = 3;
public static int MR_TYPECTOR_REP_NOTAG                 = 4;
public static int MR_TYPECTOR_REP_NOTAG_USEREQ          = 5;
public static int MR_TYPECTOR_REP_EQUIV                 = 6;
public static int MR_TYPECTOR_REP_FUNC                  = 7;
public static int MR_TYPECTOR_REP_INT                   = 8;
public static int MR_TYPECTOR_REP_CHAR                  = 9;
public static int MR_TYPECTOR_REP_FLOAT                 =10;
public static int MR_TYPECTOR_REP_STRING                =11;
public static int MR_TYPECTOR_REP_PRED                  =12;
public static int MR_TYPECTOR_REP_SUBGOAL               =13;
public static int MR_TYPECTOR_REP_VOID                  =14;
public static int MR_TYPECTOR_REP_C_POINTER             =15;
public static int MR_TYPECTOR_REP_TYPEINFO              =16;
public static int MR_TYPECTOR_REP_TYPECLASSINFO         =17;
public static int MR_TYPECTOR_REP_ARRAY                 =18;
public static int MR_TYPECTOR_REP_SUCCIP                =19;
public static int MR_TYPECTOR_REP_HP                    =20;
public static int MR_TYPECTOR_REP_CURFR                 =21;
public static int MR_TYPECTOR_REP_MAXFR                 =22;
public static int MR_TYPECTOR_REP_REDOFR                =23;
public static int MR_TYPECTOR_REP_REDOIP                =24;
public static int MR_TYPECTOR_REP_TRAIL_PTR             =25;
public static int MR_TYPECTOR_REP_TICKET                =26;
public static int MR_TYPECTOR_REP_NOTAG_GROUND          =27;
public static int MR_TYPECTOR_REP_NOTAG_GROUND_USEREQ   =28;
public static int MR_TYPECTOR_REP_EQUIV_GROUND          =29;
public static int MR_TYPECTOR_REP_TUPLE                 =30;
public static int MR_TYPECTOR_REP_RESERVED_ADDR         =31;
public static int MR_TYPECTOR_REP_RESERVED_ADDR_USEREQ  =32;
public static int MR_TYPECTOR_REP_TYPECTORINFO          =33;
public static int MR_TYPECTOR_REP_BASETYPECLASSINFO     =34;
public static int MR_TYPECTOR_REP_TYPEDESC              =35;
public static int MR_TYPECTOR_REP_TYPECTORDESC          =36;
public static int MR_TYPECTOR_REP_FOREIGN               =37;
public static int MR_TYPECTOR_REP_REFERENCE             =38;
public static int MR_TYPECTOR_REP_STABLE_C_POINTER      =39;
public static int MR_TYPECTOR_REP_PSEUDOTYPEDESC        =40;
public static int MR_TYPECTOR_REP_UNKNOWN               =41;

public static int MR_SECTAG_NONE                        = 0;
public static int MR_SECTAG_LOCAL                       = 1;
public static int MR_SECTAG_REMOTE                      = 2;
public static int MR_SECTAG_VARIABLE                    = 3;

public static bool
special__Unify____type_info_1_0(
    object[] type_info, object[] x, object[] y)
{
    mercury.runtime.Errors.SORRY(""unify for type_info"");
    return false;
}

public static bool
special__Unify____typeclass_info_1_0(
    object[] type_info, object[] x, object[] y)
{
    mercury.runtime.Errors.SORRY(""unify for typeclass_info"");
    return false;
}

public static bool
special__Unify____base_typeclass_info_1_0(
    object[] type_info, object[] x, object[] y)
{
    mercury.runtime.Errors.SORRY(""unify for base_typeclass_info"");
    return false;
}

public static bool
special__Unify____type_ctor_info_1_0(
    object[] type_info, object[] x, object[] y)
{
    mercury.runtime.Errors.SORRY(""unify for type_ctor_info"");
    return false;
}

public static void
special__Compare____type_ctor_info_1_0(
    object[] type_info, ref object[] result, object[] x, object[] y)
{
    mercury.runtime.Errors.SORRY(""compare for type_ctor_info"");
}

public static void
special__Compare____type_info_1_0(
    object[] type_info, ref object[] result, object[] x, object[] y)
{
    mercury.runtime.Errors.SORRY(""compare for type_info"");
}

public static void
special__Compare____typeclass_info_1_0(
    object[] type_info, ref object[] result, object[] x, object[] y)
{
    mercury.runtime.Errors.SORRY(""compare for typeclass_info"");
}

public static void
special__Compare____base_typeclass_info_1_0(
    object[] type_info, ref object[] result, object[] x, object[] y)
{
    mercury.runtime.Errors.SORRY(""compare for base_typeclass_info"");
}

").

:- pragma foreign_proc("C",
    type_info_from_typeclass_info(TypeClassInfo::in, Index::in,
        TypeInfo::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    TypeInfo = MR_typeclass_info_param_type_info(TypeClassInfo, Index);
").

:- pragma foreign_proc("C",
    zero_type_info_from_typeclass_info(TypeClassInfo::in, Index::in,
        TypeInfo::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    TypeInfo = MR_typeclass_info_param_type_info(TypeClassInfo, Index);
").

:- pragma foreign_proc("C",
    unconstrained_type_info_from_typeclass_info(TypeClassInfo::in,
        Index::in, TypeInfo::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    TypeInfo = MR_typeclass_info_instance_tvar_type_info(TypeClassInfo, Index);
").

:- pragma foreign_proc("C",
    zero_unconstrained_type_info_from_typeclass_info(TypeClassInfo::in,
        Index::in, TypeInfo::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    TypeInfo = MR_typeclass_info_instance_tvar_type_info(TypeClassInfo, Index);
").

:- pragma foreign_proc("C",
    superclass_from_typeclass_info(TypeClassInfo0::in, Index::in,
        TypeClassInfo::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    TypeClassInfo =
        MR_typeclass_info_superclass_info(TypeClassInfo0, Index);
").

:- pragma foreign_proc("C",
    zero_superclass_from_typeclass_info(TypeClassInfo0::in, Index::in,
        TypeClassInfo::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    TypeClassInfo =
        MR_typeclass_info_superclass_info(TypeClassInfo0, Index);
").

:- pragma foreign_proc("C",
    instance_constraint_from_typeclass_info(TypeClassInfo0::in,
        Index::in, TypeClassInfo::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    TypeClassInfo =
        MR_typeclass_info_arg_typeclass_info(TypeClassInfo0, Index);
").

:- pragma foreign_proc("C",
    zero_instance_constraint_from_typeclass_info(TypeClassInfo0::in,
        Index::in, TypeClassInfo::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    TypeClassInfo =
        MR_typeclass_info_arg_typeclass_info(TypeClassInfo0, Index);
").

:- pragma foreign_proc("C#",
    type_info_from_typeclass_info(TypeClassInfo::in, Index::in,
        TypeInfo::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    TypeInfo = MR_typeclass_info_param_type_info(TypeClassInfo, Index);
").

:- pragma foreign_proc("C#",
    unconstrained_type_info_from_typeclass_info(TypeClassInfo::in,
        Index::in, TypeInfo::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    TypeInfo = MR_typeclass_info_instance_tvar_type_info(TypeClassInfo, Index);
").

:- pragma foreign_proc("C#",
    superclass_from_typeclass_info(TypeClassInfo0::in, Index::in,
        TypeClassInfo::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    TypeClassInfo = MR_typeclass_info_superclass_info(TypeClassInfo0, Index);
").

:- pragma foreign_proc("C#",
    instance_constraint_from_typeclass_info(TypeClassInfo0::in,
        Index::in, TypeClassInfo::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    TypeClassInfo =
        MR_typeclass_info_arg_typeclass_info(TypeClassInfo0, Index);
").

:- pragma foreign_proc("Java",
    type_info_from_typeclass_info(TypeClassInfo::in, Index::in,
        TypeInfo::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    TypeInfo = MR_typeclass_info_param_type_info(
        (Object[]) TypeClassInfo, Index);
").

:- pragma foreign_proc("Java",
    unconstrained_type_info_from_typeclass_info(TypeClassInfo::in,
        Index::in, TypeInfo::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    TypeInfo = MR_typeclass_info_instance_tvar_type_info(
        (Object[]) TypeClassInfo, Index);
").

:- pragma foreign_proc("Java",
    superclass_from_typeclass_info(TypeClassInfo0::in, Index::in,
        TypeClassInfo::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    TypeClassInfo = MR_typeclass_info_superclass_info(
        (Object[]) TypeClassInfo0, Index);
").

:- pragma foreign_proc("Java",
    instance_constraint_from_typeclass_info(TypeClassInfo0::in,
        Index::in, TypeClassInfo::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    TypeClassInfo = MR_typeclass_info_arg_typeclass_info(
        (Object[]) TypeClassInfo0, Index);
").

%-----------------------------------------------------------------------------%

    % This section of the module contains predicates that are used
    % by the MLDS back-end, to implement trailing.
    % (The LLDS back-end does not use these; instead it inserts
    % the corresponding LLDS instructions directly during code
    % generation.)
    % These predicates should not be used by user programs directly.

:- interface.

:- type ticket == c_pointer.
:- type ticket_counter == c_pointer.

    % For documentation, see the corresponding LLDS instructions
    % in compiler/llds.m.  See also compiler/notes/trailing.html.

:- impure pred store_ticket(ticket::out) is det.
:- impure pred reset_ticket_undo(ticket::in) is det.
:- impure pred reset_ticket_commit(ticket::in) is det.
:- impure pred reset_ticket_solve(ticket::in) is det.
:- impure pred discard_ticket is det.
:- impure pred prune_ticket is det.
:- impure pred mark_ticket_stack(ticket_counter::out) is det.
:- impure pred prune_tickets_to(ticket_counter::in) is det.

    % XXX currently we don't support nondet pragma
    % foreign_code when trailing is enabled.
    % Instead we generate code which calls this procedure,
    % which will call error/1 with an appropriate message.
:- pred trailed_nondet_pragma_foreign_code is erroneous.

    % N.B. interface continued below.

:- implementation.

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

:- pragma foreign_proc("C",
    reset_ticket_undo(Ticket::in),
    [will_not_call_mercury, thread_safe],
"
#ifdef MR_USE_TRAIL
    MR_reset_ticket(Ticket, MR_undo);
#endif
").

:- pragma foreign_proc("C",
    reset_ticket_commit(Ticket::in),
    [will_not_call_mercury, thread_safe],
"
#ifdef MR_USE_TRAIL
    MR_reset_ticket(Ticket, MR_commit);
#endif
").

:- pragma foreign_proc("C",
    reset_ticket_solve(Ticket::in),
    [will_not_call_mercury, thread_safe],
"
#ifdef MR_USE_TRAIL
    MR_reset_ticket(Ticket, MR_solve);
#endif
").

:- pragma foreign_proc("C",
    discard_ticket,
    [will_not_call_mercury, thread_safe],
"
#ifdef MR_USE_TRAIL
    MR_discard_ticket();
#endif
").

:- pragma foreign_proc("C",
    prune_ticket,
    [will_not_call_mercury, thread_safe],
"
#ifdef MR_USE_TRAIL
    MR_prune_ticket();
#endif
").

:- pragma foreign_proc("C",
    mark_ticket_stack(TicketCounter::out),
    [will_not_call_mercury, thread_safe],
"
#ifdef MR_USE_TRAIL
    MR_mark_ticket_stack(TicketCounter);
#else
    TicketCounter = 0;
#endif
").

:- pragma foreign_proc("C",
    prune_tickets_to(TicketCounter::in),
    [will_not_call_mercury, thread_safe],
"
#ifdef MR_USE_TRAIL
    MR_prune_tickets_to(TicketCounter);
#endif
").

:- pragma foreign_proc("C#",
    store_ticket(Ticket::out),
    [will_not_call_mercury, thread_safe],
"
#if MR_USE_TRAIL
    mercury.runtime.Errors.SORRY(""foreign code for this function"");
    // MR_store_ticket(Ticket);
#else
    Ticket = null;
#endif
").

:- pragma foreign_proc("C#",
    reset_ticket_undo(Ticket::in),
    [will_not_call_mercury, thread_safe],
"
#if MR_USE_TRAIL
    mercury.runtime.Errors.SORRY(""foreign code for this function"");
    // MR_reset_ticket(Ticket, MR_undo);
#endif
").

:- pragma foreign_proc("C#",
    reset_ticket_commit(Ticket::in),
    [will_not_call_mercury, thread_safe],
"
#if MR_USE_TRAIL
    mercury.runtime.Errors.SORRY(""foreign code for this function"");
    // MR_reset_ticket(Ticket, MR_commit);
#endif
").

:- pragma foreign_proc("C#",
    reset_ticket_solve(Ticket::in),
    [will_not_call_mercury, thread_safe],
"
#if MR_USE_TRAIL
    mercury.runtime.Errors.SORRY(""foreign code for this function"");
    // MR_reset_ticket(Ticket, MR_solve);
#endif
").

:- pragma foreign_proc("C#",
    discard_ticket,
    [will_not_call_mercury, thread_safe],
"
#if MR_USE_TRAIL
    mercury.runtime.Errors.SORRY(""foreign code for this function"");
    // MR_discard_ticket();
#endif
").

:- pragma foreign_proc("C#",
    prune_ticket,
    [will_not_call_mercury, thread_safe],
"
#if MR_USE_TRAIL
    mercury.runtime.Errors.SORRY(""foreign code for this function"");
    // MR_prune_ticket();
#endif
").

:- pragma foreign_proc("C#",
    mark_ticket_stack(TicketCounter::out),
    [will_not_call_mercury, thread_safe],
"
#if MR_USE_TRAIL
    mercury.runtime.Errors.SORRY(""foreign code for this function"");
    // MR_mark_ticket_stack(TicketCounter);
#else
    TicketCounter = null;
#endif
").

:- pragma foreign_proc("C#",
    prune_tickets_to(TicketCounter::in),
    [will_not_call_mercury, thread_safe],
"
#if MR_USE_TRAIL
    mercury.runtime.Errors.SORRY(""foreign code for this function"");
    // MR_prune_tickets_to(TicketCounter);
#endif
").

:- pragma foreign_proc("Java",
    store_ticket(Ticket::out),
    [will_not_call_mercury, thread_safe],
"
    /* XXX No trailing for the Java back-end, so take no action. */
    Ticket = null;
").

:- pragma foreign_proc("Java",
    reset_ticket_undo(_Ticket::in),
    [will_not_call_mercury, thread_safe],
"
    /* XXX No trailing for the Java back-end, so take no action. */
").

:- pragma foreign_proc("Java",
    reset_ticket_commit(_Ticket::in),
    [will_not_call_mercury, thread_safe],
"
    /* XXX No trailing for the Java back-end, so take no action. */
").

:- pragma foreign_proc("Java",
    reset_ticket_solve(_Ticket::in),
    [will_not_call_mercury, thread_safe],
"
    /* XXX No trailing for the Java back-end, so take no action. */
").

:- pragma foreign_proc("Java",
    discard_ticket,
    [will_not_call_mercury, thread_safe],
"
    /* XXX No trailing for the Java back-end, so take no action. */
").

:- pragma foreign_proc("Java",
    prune_ticket,
    [will_not_call_mercury, thread_safe],
"
    /* XXX No trailing for the Java back-end, so take no action. */
").

:- pragma foreign_proc("Java",
    mark_ticket_stack(TicketCounter::out),
    [will_not_call_mercury, thread_safe],
"
    /* XXX No trailing for the Java back-end, so take no action. */
    TicketCounter = null;
").

:- pragma foreign_proc("Java",
    prune_tickets_to(_TicketCounter::in),
    [will_not_call_mercury, thread_safe],
"
    /* XXX No trailing for the Java back-end, so take no action. */
").

trailed_nondet_pragma_foreign_code :-
    Msg = string__append_list([
        "Sorry, not implemented:\n",
        "for the MLDS back-end (`--high-level-code')\n",
        "nondet `pragma c_code' or `pragma foreign_code'\n",
        "is not supported when trailing (`--use-trail') is enabled."
    ]),
    error(Msg).

%-----------------------------------------------------------------------------%

    % This section of the module contains predicates and types that are
    % used internally by the compiler for manipulating the heap.
    % These predicates should not be used by user programs directly.

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
:- impure pred free_heap(T::di) is det.

:- type mutvar(T) ---> mutvar(c_pointer).
    % a no_tag type, i.e. the representation is just a c_pointer.

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

    % XXX currently we don't support nondet foreign_procs when trailing
    % is enabled. Instead we generate code which calls this procedure,
    % which will call error/1 with an appropriate message.
    %
:- pred reclaim_heap_nondet_pragma_foreign_code is erroneous.

    % The following is a built-in reference type. It is used to define the
    % types store.generic_ref/2, store.generic_mutvar/2, std_util.mutvar/1,
    % benchmarking.int_ref/0, etc.
:- type ref(T).

    % N.B. interface continued below.

:- implementation.

% These routines are defined in C in ways which may make it not obvious
% to the Mercury compiler that they are worth inlining.
%
% (Note: it's probably not worth inlining gc_trace/1...)

:- pragma inline(free_heap/1).
:- pragma inline(mark_hp/1).
:- pragma inline(restore_hp/1).

:- pragma foreign_decl("C", "
    #include ""mercury_heap.h"" /* for MR_free_heap() */
").

:- pragma foreign_proc("C",
    gc_trace(Pointer::in),
    [will_not_call_mercury, thread_safe],
"
#ifdef MR_NATIVE_GC
    *(MR_Word *)Pointer =
        MR_agc_deep_copy(* (MR_Word *) Pointer, (MR_TypeInfo) TypeInfo_for_T,
            MR_ENGINE(MR_eng_heap_zone2->MR_zone_min),
            MR_ENGINE(MR_eng_heap_zone2->MR_zone_hardmax));
#else
    MR_fatal_error(""private_builtin__gc_trace/2: ""
        ""called when accurate GC not enabled"");
#endif
").

:- pragma foreign_proc("C",
    free_heap(Val::di),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    MR_free_heap((void *) Val);
").

:- pragma foreign_proc("C",
    mark_hp(SavedHeapPointer::out),
    [will_not_call_mercury, thread_safe],
"
#ifndef MR_CONSERVATIVE_GC
    MR_mark_hp(SavedHeapPointer);
#else
    /* We can't do heap reclamation with conservative GC. */
    SavedHeapPointer = 0;
#endif
").

:- pragma foreign_proc("C",
    restore_hp(SavedHeapPointer::in),
    [will_not_call_mercury, thread_safe],
"
#ifndef MR_CONSERVATIVE_GC
    MR_restore_hp(SavedHeapPointer);
#endif
").

:- pragma foreign_proc("C#",
    mark_hp(SavedHeapPointer::out),
    [will_not_call_mercury, thread_safe],
"
    /* We can't do heap reclamation on failure in the .NET back-end. */
    SavedHeapPointer = null;
").

:- pragma foreign_proc("C#",
    restore_hp(_SavedHeapPointer::in),
    [will_not_call_mercury, thread_safe],
"
    /* We can't do heap reclamation on failure in the .NET back-end. */
").

:- pragma foreign_proc("Java",
    gc_trace(_Pointer::in),
    [will_not_call_mercury, thread_safe],
"
    /*
    ** For the Java back-end, we use the Java garbage collector, so we
    ** take no action here.
    */
").

:- pragma foreign_proc("Java",
    free_heap(_Val::di),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    /*
    ** For the Java back-end, as for the .NET back-end, we don't define
    ** our own heaps.  So take no action here.
    */
").

:- pragma foreign_proc("Java",
    mark_hp(SavedHeapPointer::out),
    [will_not_call_mercury, thread_safe],
"
    /* We can't do heap reclamation on failure in the Java back-end. */
    SavedHeapPointer = null;
").

:- pragma foreign_proc("Java",
    restore_hp(_SavedHeapPointer::in),
    [will_not_call_mercury, thread_safe],
"
    /* We can't do heap reclamation on failure in the Java back-end. */
").

reclaim_heap_nondet_pragma_foreign_code :-
    Msg = string__append_list([
        "Sorry, not implemented:\n",
        "for the MLDS back-end (`--high-level-code')\n",
        "nondet `pragma c_code' or `pragma foreign_code'\n",
        "is not supported when `--reclaim-heap-on-failure' is enabled."
    ]),
    error(Msg).

%-----------------------------------------------------------------------------%

% Code to define the `heap_pointer' and `ref' types for the .NET back-end.
% (For the C back-ends, they're defined in runtime/mercury_builtin_types.[ch].)

:- pragma foreign_code("C#", "
    
public static bool
special__Unify__private_builtin__heap_pointer_0_0(object[] x, object[] y)
{
    mercury.runtime.Errors.fatal_error(
        ""called unify for type `private_builtin:heap_pointer'"");
    return false;
}

public static void
special__Compare__private_builtin__heap_pointer_0_0(
    ref object[] result, object[] x, object[] y)
{
    mercury.runtime.Errors.fatal_error(
        ""called compare/3 for type `private_builtin:heap_pointer'"");
}

public static bool
special__Unify__private_builtin__ref_1_0(
    object[] type_info, object[] x, object[] y)
{
    return x == y;
}

public static void
special__Compare__private_builtin__ref_1_0(
    object[] type_info, ref object[] result, object[] x, object[] y)
{
    mercury.runtime.Errors.fatal_error(
        ""called compare/3 for type `private_builtin.ref'"");
}

").

%-----------------------------------------------------------------------------%

:- pragma foreign_decl("C", "

#include ""mercury_builtin_types.h""

MR_DECLARE_TYPE_CTOR_INFO_STRUCT(MR_TYPE_CTOR_INFO_NAME(list, list, 1));
MR_DECLARE_TYPE_CTOR_INFO_STRUCT(MR_TYPE_CTOR_INFO_NAME(std_util, univ, 0));

").

:- pragma foreign_code("C", "

const MR_TypeCtorInfo ML_type_ctor_info_for_univ =
    &MR_TYPE_CTOR_INFO_NAME(std_util, univ, 0);

const MR_TypeCtorInfo ML_type_info_for_type_info =
    &MR_TYPE_CTOR_INFO_NAME(private_builtin, zero_type_info, 0);

const MR_TypeCtorInfo ML_type_info_for_pseudo_type_info =
    /*
    ** For the time being, we handle pseudo_type_infos the same way
    ** as we handle type_infos.
    */
    &MR_TYPE_CTOR_INFO_NAME(private_builtin, zero_type_info, 0);

const MR_FA_TypeInfo_Struct1 ML_type_info_for_list_of_univ = {
    &MR_TYPE_CTOR_INFO_NAME(list, list, 1),
    { (MR_TypeInfo) &MR_TYPE_CTOR_INFO_NAME(std_util, univ, 0) }
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

%-----------------------------------------------------------------------------%

:- interface.

    % This section of the module is for miscellaneous predicates
    % that sometimes have calls to them emitted by the compiler.

    % unsafe_type_cast/2 is used internally by the compiler. Bad things
    % will happen if this is used in programs.
    % With the LLDS back-end, it has no definition,
    % since for efficiency the code generator treats it as a builtin.
    % With the MLDS back-end, it is defined in runtime/mercury.h.

:- pred unsafe_type_cast(T1::in, T2::out) is det.

    % store_at_ref/2 is used internally by the compiler. Bad things
    % will happen if this is used in programs.
    %
:- pred store_at_ref(c_pointer::in, T::in) is det.

    % unused/0 should never be called.
    % The compiler sometimes generates references to this procedure,
    % but they should never get executed.
:- pred unused is det.

:- pred nyi_foreign_type_unify(T::in, T::in) is semidet.
:- pred nyi_foreign_type_compare(comparison_result::uo, T::in, T::in) is det.

    % N.B. interface continued below.

:- implementation.

% unsafe_type_cast is a builtin; the compiler generates inline code for it

store_at_ref(_X, _Y) :-
    true.

unused :-
    ( semidet_succeed ->
        error("attempted use of dead predicate")
    ;
        % the following is never executed
        true
    ).

nyi_foreign_type_unify(_, _) :-
    ( semidet_succeed ->
        sorry("unify for foreign types")
    ;
        semidet_succeed
    ).

nyi_foreign_type_compare(Result, _, _) :-
    ( semidet_succeed ->
        sorry("compare for foreign types")
    ;
        Result = (=)
    ).

%-----------------------------------------------------------------------------%

:- interface.

    % var/1 is intended to make it possible to write code that effectively
    % has different implementations for different modes (see type_to_univ
    % in std_util.m as an example). It has to be impure to ensure that
    % reordering doesn't cause the wrong mode to be selected.
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
    % library.  (If the procedure is part of the Mercury standard library,
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

%-----------------------------------------------------------------------------%

:- implementation.

var(_::ui) :- fail.
var(_::in) :- fail.
var(_::unused) :- true.

nonvar(_::ui) :- true.
nonvar(_::in) :- true.
nonvar(_::unused) :- fail.

sorry(PredName) :-
    error("sorry, " ++ PredName ++ " not implemented\n" ++
        "for this target language (or compiler back-end).").

no_clauses(PredName) :-
    error("no clauses for " ++ PredName).

:- pragma foreign_proc(c,
    imp,
    [will_not_call_mercury, thread_safe],
"").
:- pragma foreign_proc(il,  
    imp,
    [will_not_call_mercury, thread_safe, max_stack_size(0)],
"").
:- pragma foreign_proc("Java",
    imp,
    [will_not_call_mercury, thread_safe],
"").

:- pragma foreign_proc(c,
    semip,
    [will_not_call_mercury, thread_safe, promise_semipure],
"").
:- pragma foreign_proc(il,  
    semip,
    [will_not_call_mercury, thread_safe, promise_semipure, max_stack_size(0)],
"").
:- pragma foreign_proc("Java",
    semip,
    [will_not_call_mercury, thread_safe, promise_semipure],
"").

%-----------------------------------------------------------------------------%

:- pragma foreign_code("Java", "
    public static class Ref_1
    {
        // XXX stub only
    }

    public static class Heap_pointer_0
    {
        // XXX stub only
    }

    // TypeCtorRep constants
    public static final int MR_TYPECTOR_REP_ENUM = 0;
    public static final int MR_TYPECTOR_REP_ENUM_USEREQ = 1;
    public static final int MR_TYPECTOR_REP_DU = 2;
    public static final int MR_TYPECTOR_REP_DU_USEREQ               = 3;
    public static final int MR_TYPECTOR_REP_NOTAG                   = 4;
    public static final int MR_TYPECTOR_REP_NOTAG_USEREQ            = 5;
    public static final int MR_TYPECTOR_REP_EQUIV                   = 6;
    public static final int MR_TYPECTOR_REP_FUNC                    = 7;
    public static final int MR_TYPECTOR_REP_INT                     = 8;
    public static final int MR_TYPECTOR_REP_CHAR                    = 9;
    public static final int MR_TYPECTOR_REP_FLOAT                   = 10;
    public static final int MR_TYPECTOR_REP_STRING                  = 11;
    public static final int MR_TYPECTOR_REP_PRED                    = 12;
    public static final int MR_TYPECTOR_REP_SUBGOAL                 = 13;
    public static final int MR_TYPECTOR_REP_VOID                    = 14;
    public static final int MR_TYPECTOR_REP_C_POINTER               = 15;
    public static final int MR_TYPECTOR_REP_TYPEINFO                = 16;
    public static final int MR_TYPECTOR_REP_TYPECLASSINFO           = 17;
    public static final int MR_TYPECTOR_REP_ARRAY                   = 18;
    public static final int MR_TYPECTOR_REP_SUCCIP                  = 19;
    public static final int MR_TYPECTOR_REP_HP                      = 20;
    public static final int MR_TYPECTOR_REP_CURFR                   = 21;
    public static final int MR_TYPECTOR_REP_MAXFR                   = 22;
    public static final int MR_TYPECTOR_REP_REDOFR                  = 23;
    public static final int MR_TYPECTOR_REP_REDOIP                  = 24;
    public static final int MR_TYPECTOR_REP_TRAIL_PTR               = 25;
    public static final int MR_TYPECTOR_REP_TICKET                  = 26;
    public static final int MR_TYPECTOR_REP_NOTAG_GROUND            = 27;
    public static final int MR_TYPECTOR_REP_NOTAG_GROUND_USEREQ     = 28;
    public static final int MR_TYPECTOR_REP_EQUIV_GROUND            = 29;
    public static final int MR_TYPECTOR_REP_TUPLE                   = 30;
    public static final int MR_TYPECTOR_REP_RESERVED_ADDR           = 31;
    public static final int MR_TYPECTOR_REP_RESERVED_ADDR_USEREQ    = 32;
    public static final int MR_TYPECTOR_REP_TYPECTORINFO            = 33;
    public static final int MR_TYPECTOR_REP_BASETYPECLASSINFO       = 34;
    public static final int MR_TYPECTOR_REP_TYPEDESC                = 35;
    public static final int MR_TYPECTOR_REP_TYPECTORDESC            = 36;
    public static final int MR_TYPECTOR_REP_FOREIGN                 = 37;
    public static final int MR_TYPECTOR_REP_REFERENCE               = 38;
    public static final int MR_TYPECTOR_REP_STABLE_C_POINTER        = 39;
    public static final int MR_TYPECTOR_REP_PSEUDOTYPEDESC          = 40;
    public static final int MR_TYPECTOR_REP_UNKNOWN                 = 41;
    
    public static final int MR_SECTAG_NONE      = 0;
    public static final int MR_SECTAG_LOCAL     = 1;
    public static final int MR_SECTAG_REMOTE    = 2;
    public static final int MR_SECTAG_VARIABLE  = 3;

    public static final int MR_PREDICATE    = 0;
    public static final int MR_FUNCTION     = 1;

    // The dummy_var is used to represent io__states and other Mercury
    // parameters that are not really passed around.  Occasionally a dummy
    // variable will be used by the code generator as an lval, so we use
    // private_builtin:dummy_var as that lval.
    public static class Dummy {
        public java.lang.Object F1;
    };
    public static Dummy dummy_var = new Dummy();
").

:- pragma foreign_code("Java", "
    //
    // Type-specific unification and comparison routines
    //

    public static boolean
    __Unify____ref_1_0(mercury.runtime.TypeInfo_Struct ti,
        mercury.private_builtin.Ref_1 x,
        mercury.private_builtin.Ref_1 y)
    {
        // stub only
        throw new java.lang.Error
            (""unify/2 for type private_builtin.ref not implemented"");
    }

    public static boolean
    __Unify____heap_pointer_0_0 (mercury.private_builtin.Heap_pointer_0 x,
        mercury.private_builtin.Heap_pointer_0 y)
    {
        // stub only
        throw new java.lang.Error(""unify/2 for type heap_pointer/0"");
    }

    public static boolean
    __Unify____type_ctor_info_1_0(mercury.runtime.TypeInfo_Struct ti,
        mercury.runtime.TypeCtorInfo_Struct x,
        mercury.runtime.TypeCtorInfo_Struct y)
    {
        // stub only
        throw new java.lang.Error
            (""unify/2 for type type_ctor_info/1"");
    }

    public static boolean
    __Unify____type_info_1_0(mercury.runtime.TypeInfo_Struct ti,
        mercury.runtime.TypeInfo_Struct x,
        mercury.runtime.TypeInfo_Struct y)
    {
        // stub only
        throw new java.lang.Error
            (""unify/2 for type type_info/1"");
    }

    public static boolean
    __Unify____base_typeclass_info_1_0(mercury.runtime.TypeInfo_Struct ti,
        java.lang.Object[] x, java.lang.Object[] y)
    {
        // stub only
        throw new java.lang.Error(""unify/2 for type typeclass_info/1"");
    }

    public static boolean
    __Unify____typeclass_info_1_0(mercury.runtime.TypeInfo_Struct ti,
        java.lang.Object[] x, java.lang.Object[] y)
    {
        // stub only
        throw new java.lang.Error
            (""unify/2 for type typeclass_info/1"");
    }

    public static mercury.builtin.Comparison_result_0
    __Compare____ref_1_0(mercury.runtime.TypeInfo_Struct ti,
        mercury.private_builtin.Ref_1 x,
        mercury.private_builtin.Ref_1 y)
    {
        // stub only
        throw new java.lang.Error
            (""called compare/3 for type private_builtin.ref"");
    }

    public static mercury.builtin.Comparison_result_0
    __Compare____heap_pointer_0_0 (mercury.private_builtin.Heap_pointer_0 x,
        mercury.private_builtin.Heap_pointer_0 y)
    {
        // stub only
        throw new java.lang.Error(""compare/2 for type heap_pointer/0"");
    }

    public static mercury.builtin.Comparison_result_0
    __Compare____type_ctor_info_1_0(mercury.runtime.TypeInfo_Struct ti,
        mercury.runtime.TypeCtorInfo_Struct x,
        mercury.runtime.TypeCtorInfo_Struct y)
    {
        // stub only
        throw new java.lang.Error
            (""compare/2 for type type_ctor_info/1"");
    }

    public static mercury.builtin.Comparison_result_0
    __Compare____type_info_1_0(mercury.runtime.TypeInfo_Struct ti,
        mercury.runtime.TypeInfo_Struct x,
        mercury.runtime.TypeInfo_Struct y)
    {
        // stub only
        throw new java.lang.Error
            (""compare/2 for type type_info/1"");
    }

    public static mercury.builtin.Comparison_result_0
    __Compare____base_typeclass_info_1_0(mercury.runtime.TypeInfo_Struct ti,
        java.lang.Object[] x, java.lang.Object[] y)
    {
        // stub only
        throw new java.lang.Error(""compare/2 for type typeclass_info/1"");
    }

    public static mercury.builtin.Comparison_result_0
    __Compare____typeclass_info_1_0(mercury.runtime.TypeInfo_Struct ti,
        java.lang.Object[] x, java.lang.Object[] y)
    {
        // stub only
        throw new java.lang.Error
            (""compare/2 for type typeclass_info/1"");
    }

").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
