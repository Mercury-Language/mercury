%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2007, 2010-2012 The University of Melbourne.
% Copyright (C) 2014-2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: builtin.m.
% Main author: fjh.
% Stability: low.
%
% This file is automatically imported into every module.
% It is intended for things that are part of the language,
% but which are implemented just as normal user-level code
% rather than with special coding in the compiler.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module builtin.
:- interface.

%---------------------------------------------------------------------------%
%
% Types.
%

% The types `character', `int', `int8', `int16', `int32', `int64',
% `uint', `uint8', `uint16', `uint32', `uint64', `float', and `string',
% and tuple types `{}', `{T}', `{T1, T2}', ...
% and the types `pred', `pred(T)', `pred(T1, T2)', `pred(T1, T2, T3)', ...
% and `func(T1) = T2', `func(T1, T2) = T3', `func(T1, T2, T3) = T4', ...
% are builtin and are implemented using special code in the type-checker.

    % The type c_pointer can be used by predicates that use the C interface.
    %
    % NOTE: We *strongly* recommend using a `foreign_type' pragma instead
    % of using this type.
    %
:- type c_pointer.

%---------------------------------------------------------------------------%
%
% Insts.
%

% The standard insts `free', `ground', and `bound(...)' are builtin and are
% implemented using special code in the parser and mode-checker.
%
% So are the standard unique insts `unique', `unique(...)', `mostly_unique',
% `mostly_unique(...)', and `clobbered'.
%
% Higher-order predicate insts `pred(<modes>) is <detism>'
% and higher-order function insts `func(<modes>) = <mode> is <detism>'
% are also builtin.
%
% The `any' inst used for constraint solver interfaces is builtin and so are
% its higher-order variants: `any_pred(<modes>) is <detism>' and
% `any_func(<modes>) = <mode> is <detism>'.

    % The name `dead' is allowed as a synonym for `clobbered'.
    % Similarly, `mostly_dead' is a synonym for `mostly_clobbered'.
    %
:- inst dead == clobbered.
:- inst mostly_dead == mostly_clobbered.

%---------------------------------------------------------------------------%
%
% Standard modes.
%

:- mode unused == free >> free.

    % This mode is deprecated, use `out' instead.
    %
:- mode output == free >> ground.

    % This mode is deprecated, use `in' instead.
    %
:- mode input  == ground >> ground.

:- mode in  == ground >> ground.
:- mode out == free >> ground.

:- mode in(Inst)  == Inst >> Inst.
:- mode out(Inst) == free >> Inst.
:- mode di(Inst)  == Inst >> clobbered.
:- mode mdi(Inst) == Inst >> mostly_clobbered.

%---------------------------------------------------------------------------%
%
% Unique modes.
%

% XXX These are still not fully implemented.

    % unique output
    %
:- mode uo == free >> unique.

    % unique input
    %
:- mode ui == unique >> unique.

    % destructive input
    %
:- mode di == unique >> clobbered.

%---------------------------------------------------------------------------%
%
% "Mostly" unique modes.
%

% Unique except that they may be referenced again on backtracking.

    % mostly unique output
    %
:- mode muo == free >> mostly_unique.

    % mostly unique input
    %
:- mode mui == mostly_unique >> mostly_unique.

    % mostly destructive input
    %
:- mode mdi == mostly_unique >> mostly_clobbered.

%---------------------------------------------------------------------------%
%
% Dynamic modes.
%

    % Solver type modes.
    %
:- mode ia == any >> any.
:- mode oa == free >> any.

%---------------------------------------------------------------------------%
%
% Predicates.
%

    % copy/2 makes a deep copy of a data structure.
    % The resulting copy is a `unique' value, so you can use
    % destructive update on it.
    %
:- pred copy(T, T).
:- mode copy(ui, uo) is det.
:- mode copy(in, uo) is det.

    % unsafe_promise_unique/2 is used to promise the compiler that you
    % have a `unique' copy of a data structure, so that you can use
    % destructive update. It is used to work around limitations in
    % the current support for unique modes.
    % `unsafe_promise_unique(X, Y)' is the same as `Y = X' except that
    % the compiler will assume that Y is unique.
    %
    % Note that misuse of this predicate may lead to unsound results:
    % if there is more than one reference to the data in question,
    % i.e. it is not `unique', then the behaviour is undefined.
    % (If you lie to the compiler, the compiler will get its revenge!)
    %
:- func unsafe_promise_unique(T::in) = (T::uo) is det.
:- pred unsafe_promise_unique(T::in, T::uo) is det.

    % A synonym for fail/0; this name is more in keeping with Mercury's
    % declarative style rather than its Prolog heritage.
    %
:- pred false is failure.

%---------------------------------------------------------------------------%

    % This function is useful for converting polymorphic non-solver type
    % values with inst any to inst ground (the compiler recognises that
    % inst any is equivalent to ground for non-polymorphic non-solver
    % type values.)
    %
    % Do not call this on solver type values unless you are *absolutely sure*
    % that they are semantically ground.
    %
:- func unsafe_cast_any_to_ground(T::ia) = (T::out) is det.

%---------------------------------------------------------------------------%

    % unify(X, Y) is true iff X = Y.
    %
:- pred unify(T::in, T::in) is semidet.

    % For use in defining user-defined unification predicates.
    % The relation defined by a value of type `unify', must be an
    % equivalence relation; that is, it must be symmetric, reflexive,
    % and transitive.
    %
:- type unify(T) == pred(T, T).
:- inst unify == (pred(in, in) is semidet).

:- type comparison_result
    --->    (=)
    ;       (<)
    ;       (>).

    % compare(Res, X, Y) binds Res to =, <, or > depending on whether
    % X is =, <, or > Y in the standard ordering.
    %
:- pred compare(comparison_result, T, T).
    % NOTE_TO_IMPLEMENTORS The modes must appear in this order:
    % NOTE_TO_IMPLEMENTORS compiler/higher_order.m depends on it, as does
    % NOTE_TO_IMPLEMENTORS compiler/simplify.m (for the inequality
    % NOTE_TO_IMPLEMENTORS simplification.)
:- mode compare(uo, in, in) is det.
:- mode compare(uo, ui, ui) is det.
:- mode compare(uo, ui, in) is det.
:- mode compare(uo, in, ui) is det.

    % For use in defining user-defined comparison predicates.
    % For a value ComparePred of type `compare', the following
    % conditions must hold:
    %
    % - the relation
    %   compare_eq(X, Y) :- ComparePred((=), X, Y).
    %   must be an equivalence relation; that is, it must be symmetric,
    %   reflexive, and transitive.
    %
    % - the relations
    %   compare_leq(X, Y) :-
    %       ComparePred(R, X, Y), (R = (=) ; R = (<)).
    %   compare_geq(X, Y) :-
    %       ComparePred(R, X, Y), (R = (=) ; R = (>)).
    %   must be total order relations: that is they must be antisymmetric,
    %   reflexive and transitive.
    %
:- type compare(T) == pred(comparison_result, T, T).
:- inst compare == (pred(uo, in, in) is det).

    % ordering(X, Y) = R <=> compare(R, X, Y)
    %
:- func ordering(T, T) = comparison_result.

    % The standard inequalities defined in terms of compare/3.
    % XXX The ui modes are commented out because they don't yet work properly.
    %
:- pred T  @<  T.
:- mode in @< in is semidet.
% :- mode ui @< in is semidet.
% :- mode in @< ui is semidet.
% :- mode ui @< ui is semidet.

:- pred T  @=<  T.
:- mode in @=< in is semidet.
% :- mode ui @=< in is semidet.
% :- mode in @=< ui is semidet.
% :- mode ui @=< ui is semidet.

:- pred T  @>  T.
:- mode in @> in is semidet.
% :- mode ui @> in is semidet.
% :- mode in @> ui is semidet.
% :- mode ui @> ui is semidet.

:- pred T  @>=  T.
:- mode in @>= in is semidet.
% :- mode ui @>= in is semidet.
% :- mode in @>= ui is semidet.
% :- mode ui @>= ui is semidet.

    % Values of types comparison_pred/1 and comparison_func/1 are used
    % by predicates and functions which depend on an ordering on a given
    % type, where this ordering is not necessarily the standard ordering.
    % In addition to the type, mode and determinism constraints, a
    % comparison predicate C is expected to obey two other laws.
    % For all X, Y and Z of the appropriate type, and for all
    % comparison_results R:
    %   1) C(X, Y, (>)) if and only if C(Y, X, (<))
    %   2) C(X, Y, R) and C(Y, Z, R) implies C(X, Z, R).
    % Comparison functions are expected to obey analogous laws.
    %
    % Note that binary relations <, > and = can be defined from a
    % comparison predicate or function in an obvious way. The following
    % facts about these relations are entailed by the above constraints:
    % = is an equivalence relation (not necessarily the usual equality),
    % and the equivalence classes of this relation are totally ordered
    % with respect to < and >.
    %
:- type comparison_pred(T) == pred(T, T, comparison_result).
:- inst comparison_pred(I) == (pred(in(I), in(I), out) is det).
:- inst comparison_pred == comparison_pred(ground).

:- type comparison_func(T) == (func(T, T) = comparison_result).
:- inst comparison_func(I) == (func(in(I), in(I)) = out is det).
:- inst comparison_func == comparison_func(ground).

% In addition, the following predicate-like constructs are builtin:
%
%   :- pred (T = T).
%   :- pred (T \= T).
%   :- pred (pred , pred).
%   :- pred (pred ; pred).
%   :- pred (\+ pred).
%   :- pred (not pred).
%   :- pred (pred -> pred).
%   :- pred (if pred then pred).
%   :- pred (if pred then pred else pred).
%   :- pred (pred => pred).
%   :- pred (pred <= pred).
%   :- pred (pred <=> pred).
%
%   (pred -> pred ; pred).
%   some Vars pred
%   all Vars pred
%   call/N

%---------------------------------------------------------------------------%

    % `semidet_succeed' is exactly the same as `true', except that
    % the compiler thinks that it is semi-deterministic. You can use
    % calls to `semidet_succeed' to suppress warnings about determinism
    % declarations that could be stricter.
    %
:- pred semidet_succeed is semidet.

    % `semidet_fail' is like `fail' except that its determinism is semidet
    % rather than failure.
    %
:- pred semidet_fail is semidet.

    % A synonym for semidet_succeed/0.
    %
:- pred semidet_true is semidet.

    % A synonym for semidet_fail/0.
    %
:- pred semidet_false is semidet.

    % `cc_multi_equal(X, Y)' is the same as `X = Y' except that it
    % is cc_multi rather than det.
    %
:- pred cc_multi_equal(T, T).
:- mode cc_multi_equal(di, uo) is cc_multi.
:- mode cc_multi_equal(in, out) is cc_multi.

    % `impure_true' is like `true' except that it is impure.
    %
:- impure pred impure_true is det.

    % `semipure_true' is like `true' except that it is semipure.
    %
:- semipure pred semipure_true is det.

%---------------------------------------------------------------------------%

    % dynamic_cast(X, Y) succeeds with Y = X iff X has the same ground type
    % as Y (so this may succeed if Y is of type list(int), say, but not if
    % Y is of type list(T)).
    %
:- pred dynamic_cast(T1::in, T2::out) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

% Everything below here is not intended to be part of the public interface,
% and will not be included in the Mercury library reference manual.

:- interface.

    % compare_representation(Result, X, Y):
    %
    % compare_representation is similar to the builtin predicate compare/3,
    % except that it does not abort when asked to compare non-canonical terms.
    %
    % The declarative semantics of compare_representation for unequal
    % non-canonical terms is that the result is either (<) or (>).
    % For equal non-canonical terms the result can be anything.
    %
    % Operationally, the result of compare_representation for non-canonical
    % terms is the same as that for comparing the internal representations
    % of the terms, where the internal representation is that which would be
    % produced by deconstruct.cc.
    %
    % XXX This predicate is not yet implemented for highlevel code.
    % This is the reason it is not in the official part of the interface.
    %
:- pred compare_representation(comparison_result, T, T).
:- mode compare_representation(uo, in, in) is cc_multi.

    % Set up Mercury runtime to call special predicates implemented in this
    % module.
    %
:- impure pred init_runtime_hooks is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

    % This import is needed by the Mercury clause for semidet_fail/0,
    % and for e.g. {unify,compare}_tuple_pos.
    %
:- import_module int.

%---------------------------------------------------------------------------%

false :-
    fail.

%---------------------------------------------------------------------------%

% IMPORTANT: any changes or additions to external predicates should be
% reflected in the definition of pred_is_external in
% mdbcomp/program_representation.m. The debugger needs to know what predicates
% are defined externally, so that it knows not to expect events for those
% predicates.
%
:- pragma external_pred(unify/2).
:- pragma external_pred(compare/3).
:- pragma external_pred(compare_representation/3).

:- pragma foreign_export_enum("C#", comparison_result/0, [],
    [
        (=) - "COMPARE_EQUAL",
        (<) - "COMPARE_LESS",
        (>) - "COMPARE_GREATER"
    ]).

:- pragma foreign_export_enum("Java", comparison_result/0, [],
    [
        (=) - "COMPARE_EQUAL",
        (<) - "COMPARE_LESS",
        (>) - "COMPARE_GREATER"
    ]).

:- pragma foreign_code("C#", "
    public static readonly object[] comparison_result_object = new object[] {
        (Comparison_result_0) 0,
        (Comparison_result_0) 1,
        (Comparison_result_0) 2
    };
").

ordering(X, Y) = R :-
    compare(R, X, Y).

% simplify_goal_call.m automatically inlines these definitions.

X @< Y :-
    compare((<), X, Y).

X @=< Y :-
    not compare((>), X, Y).

X @>  Y :-
    compare((>), X, Y).

X @>= Y :-
    not compare((<), X, Y).

%---------------------------------------------------------------------------%
%
% Unify/compare of tuples.
%

% We implement these predicates in Mercury mainly to allow the compiler
% to perform the deep profiling transformation on them. init_runtime_hooks
% sets fields in MR_special_pred_hooks structure to point to the actual
% implementations, because we do not want the runtime to have unresolved
% references into the library when it is built.

:- pragma foreign_decl("C", "#include ""mercury_ho_call.h""").

:- pred unify_tuple(T::in, T::in) is semidet.

:- pragma foreign_export("C", unify_tuple(in, in), "ML_unify_tuple").

unify_tuple(TermA, TermB) :-
    tuple_arity(TermA, Arity),
    unify_tuple_pos(TermA, TermB, 0, Arity).

:- pred unify_tuple_pos(T::in, T::in, int::in, int::in) is semidet.

unify_tuple_pos(TermA, TermB, Index, Arity) :-
    ( if Index >= Arity then
        true
    else
        tuple_arg(TermA, Index, SubTermA),
        tuple_arg(TermB, Index, SubTermB),
        private_builtin.unsafe_type_cast(SubTermB, CastSubTermB),
        ( if builtin.unify(SubTermA, CastSubTermB) then
            unify_tuple_pos(TermA, TermB, Index + 1, Arity)
        else
            fail
        )
    ).

:- pred compare_tuple(comparison_result::uo, T::in, T::in) is det.

:- pragma foreign_export("C", compare_tuple(uo, in, in), "ML_compare_tuple").

compare_tuple(Result, TermA, TermB) :-
    tuple_arity(TermA, Arity),
    compare_tuple_pos(Result, TermA, TermB, 0, Arity).

:- pred compare_tuple_pos(comparison_result::uo, T::in, T::in,
    int::in, int::in) is det.

compare_tuple_pos(Result, TermA, TermB, Index, Arity) :-
    ( if Index >= Arity then
        Result = (=)
    else
        tuple_arg(TermA, Index, SubTermA),
        tuple_arg(TermB, Index, SubTermB),
        private_builtin.unsafe_type_cast(SubTermB, CastSubTermB),
        builtin.compare(SubResult, SubTermA, CastSubTermB),
        (
            SubResult = (=),
            compare_tuple_pos(Result, TermA, TermB, Index + 1, Arity)
        ;
            ( SubResult = (<)
            ; SubResult = (>)
            ),
            Result = SubResult
        )
    ).

:- pred compare_rep_tuple(comparison_result::uo, T::in, T::in) is cc_multi.

:- pragma foreign_export("C", compare_rep_tuple(uo, in, in),
    "ML_compare_rep_tuple").

compare_rep_tuple(Result, TermA, TermB) :-
    tuple_arity(TermA, Arity),
    compare_rep_tuple_pos(Result, TermA, TermB, 0, Arity).

:- pred compare_rep_tuple_pos(comparison_result::uo, T::in, T::in,
    int::in, int::in) is cc_multi.

compare_rep_tuple_pos(Result, TermA, TermB, Index, Arity) :-
    ( if Index >= Arity then
        Result = (=)
    else
        tuple_arg(TermA, Index, SubTermA),
        tuple_arg(TermB, Index, SubTermB),
        private_builtin.unsafe_type_cast(SubTermB, CastSubTermB),
        builtin.compare_representation(SubResult, SubTermA, CastSubTermB),
        (
            SubResult = (=),
            compare_rep_tuple_pos(Result, TermA, TermB, Index + 1, Arity)
        ;
            ( SubResult = (<)
            ; SubResult = (>)
            ),
            Result = SubResult
        )
    ).

:- pred tuple_arity(T::in, int::out) is det.
:- pragma no_determinism_warning(pred(tuple_arity/2)).

:- pragma foreign_proc("C",
    tuple_arity(_Term::in, Arity::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Arity = MR_TYPEINFO_GET_VAR_ARITY_ARITY((MR_TypeInfo) TypeInfo_for_T);
").

tuple_arity(_, _) :-
    private_builtin.sorry("tuple_arity/2").

:- some [ArgT] pred tuple_arg(T::in, int::in, ArgT::out) is det.
:- pragma no_determinism_warning(pred(tuple_arg/3)).

:- pragma foreign_proc("C",
    tuple_arg(Term::in, Index::in, Arg::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    MR_TypeInfo type_info = (MR_TypeInfo) TypeInfo_for_T;
    MR_Word     *arg_vector = (MR_Word *) Term;

    TypeInfo_for_ArgT =
        (MR_Word) MR_TYPEINFO_GET_VAR_ARITY_ARG_VECTOR(type_info)[1 + Index];
    Arg = arg_vector[Index];
").

tuple_arg(_, _, -1) :-
    private_builtin.sorry("tuple_arg/3").

%---------------------------------------------------------------------------%

:- pragma foreign_code("C#", "
    //
    // Generic unification/comparison routines
    //

    public static bool
    unify_2_p_0(runtime.TypeInfo_Struct ti,
        object x, object y)
    {
        return rtti_implementation.generic_unify_2_p_0(ti, x, y);
    }

    public static Comparison_result_0
    compare_3_p_0(runtime.TypeInfo_Struct ti,
        object x, object y)
    {
        return rtti_implementation.generic_compare_3_p_0(ti, x, y);
    }

    public static Comparison_result_0
    compare_3_p_1(runtime.TypeInfo_Struct ti,
        object x, object y)
    {
        return compare_3_p_0(ti, x, y);
    }

    public static Comparison_result_0
    compare_3_p_2(runtime.TypeInfo_Struct ti,
        object x, object y)
    {
        return compare_3_p_0(ti, x, y);
    }

    public static Comparison_result_0
    compare_3_p_3(runtime.TypeInfo_Struct ti,
        object x, object y)
    {
        return compare_3_p_0(ti, x, y);
    }

    public static Comparison_result_0
    compare_representation_3_p_0(runtime.TypeInfo_Struct ti,
        object x, object y)
    {
        // stub only
        runtime.Errors.SORRY(
            ""compare_representation_3_p_0/3 not implemented"");
        return Comparison_result_0.f_equal;
    }
").

:- pragma foreign_code("C#", "
public static object deep_copy(object o)
{
    if (o == null) {
        return null;
    }

    System.Type t = o.GetType();
    System.Array arr;

    if (t.IsValueType) {
        return o;
    } else if (t == typeof(string)) {
        // XXX For some reason we need to handle strings specially.
        // It is probably something to do with the fact that they
        // are a builtin type.
        string s;
        s = (string) o;
        return s;
    } else if ((arr = o as System.Array) != null) {
        return arr.Clone();
    } else {
        object n;

        // This will do a bitwise shallow copy of the object.
        n = t.InvokeMember(""MemberwiseClone"",
            System.Reflection.BindingFlags.Instance |
            System.Reflection.BindingFlags.NonPublic |
            System.Reflection.BindingFlags.InvokeMethod,
            null, o, new object[] {});

        // Set each of the fields to point to a deep copy of the
        // field.
        deep_copy_fields(t.GetFields(
            System.Reflection.BindingFlags.Public |
            System.Reflection.BindingFlags.Instance),
            n, o);

        // XXX This requires that mercury.dll have
        // System.Security.Permissions.ReflectionPermission
        // so that the non-public fields are accessible.
        deep_copy_fields(t.GetFields(
            System.Reflection.BindingFlags.NonPublic |
            System.Reflection.BindingFlags.Instance),
            n, o);

        return n;
    }
}

public static void deep_copy_fields(System.Reflection.FieldInfo[] fields,
    object dest, object src)
{
    // XXX We don't handle init-only fields, but I can't think of a way.
    foreach (System.Reflection.FieldInfo f in fields)
    {
        if (!f.IsNotSerialized) {
            f.SetValue(dest, deep_copy(f.GetValue(src)));
        }
    }
}
").

:- pragma foreign_code("C#", "

public static bool
__Unify____void_0_0(object x, object y)
{
    runtime.Errors.fatal_error(""called unify for type `void'"");
    return false;
}

public static bool
__Unify____c_pointer_0_0(object x, object y)
{
    runtime.Errors.fatal_error(""called unify for type `c_pointer'"");
    return false;
}

public static bool
__Unify____func_0_0(object[] x, object[] y)
{
    runtime.Errors.fatal_error(""called unify for `func' type"");
    return false;
}

public static bool
__Unify____tuple_0_0(object[] x, object[] y)
{
    runtime.Errors.fatal_error(""called unify for `tuple' type"");
    return false;
}

public static Comparison_result_0
__Compare____void_0_0(object x, object y)
{
    runtime.Errors.fatal_error(""called compare for type `void'"");
    return Comparison_result_0.f_equal;
}

public static Comparison_result_0
__Compare____c_pointer_0_0(object x, object y)
{
    runtime.Errors.fatal_error(
        ""called compare/3 for type `c_pointer'"");
    return Comparison_result_0.f_equal;
}

public static Comparison_result_0
__Compare____func_0_0(object x, object y)
{
    runtime.Errors.fatal_error(""called compare for `func' type"");
    return Comparison_result_0.f_equal;
}

public static Comparison_result_0
__Compare____tuple_0_0(object x, object y)
{
    runtime.Errors.fatal_error(""called compare for `tuple' type"");
    return Comparison_result_0.f_equal;
}
").

:- pragma foreign_code("Java",
"
    // Two other approaches to implement deep copy might be:
    //
    // 1. Get all mercury objects to implement the Serializable interface.
    //    Then this whole function could be replaced with code that writes
    //    the Object out via an ObjectOutputStream into a byte array (or
    //    something), then reads it back in again, thus creating a copy.
    //    This would copy non-Mercury objects as well, though.
    //
    // 2. Get all mercury objects to implement a clone() method (either the
    //    one in Object or our own). The MLDS doesn't have method calls
    //    and probably we wouldn't want to clutter it up just for this.

    public static <T> T
    deep_copy(final T original) throws
        java.lang.InstantiationException, java.lang.IllegalAccessException
    {
        if (original == null) {
            return null;
        }

        final java.lang.Class<T> cls =
            (java.lang.Class<T>) original.getClass();

        if (cls.isArray()) {
            int length = java.lang.reflect.Array.getLength(original);
            T clone = (T) java.lang.reflect.Array.newInstance(
                cls.getComponentType(), length);
            for (int i = 0; i < length; i++) {
                java.lang.Object X = java.lang.reflect.Array.get(original, i);
                java.lang.Object Y = deep_copy(X);
                java.lang.reflect.Array.set(clone, i, Y);
            }
            return clone;
        }

        // We'll only copy objects of Mercury-defined types. We could copy
        // more but that is what we do for C backends and it is enough.
        // We don't copy enumeration instances.
        if (!(original instanceof jmercury.runtime.MercuryType)) {
            return original;
        }
        if (original instanceof jmercury.runtime.MercuryEnum) {
            return original;
        }

        // Make a new instance of the class and fill in the fields.
        // This requires the class have a default constructor.
        // (alternatively, we could use the Objenesis library).
        T clone = cls.newInstance();
        java.lang.Class<?> c = cls;
        while (c != Object.class && c != null) {
            for (java.lang.reflect.Field field : c.getDeclaredFields()) {
                java.lang.Object X = field.get(original);
                java.lang.Object Y = deep_copy(X);
                field.set(clone, Y);
            }
            c = c.getSuperclass();
        }
        return clone;
    }
").

%---------------------------------------------------------------------------%

% unsafe_promise_unique is a compiler builtin.

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    copy(Value::ui, Copy::uo),
    [will_not_call_mercury, thread_safe, promise_pure, will_not_modify_trail,
        does_not_affect_liveness],
"
    MR_save_transient_registers();
    Copy = MR_deep_copy(Value, (MR_TypeInfo) TypeInfo_for_T, NULL, NULL);
    MR_restore_transient_registers();
").

:- pragma foreign_proc("C",
    copy(Value::in, Copy::uo),
    [will_not_call_mercury, thread_safe, promise_pure, will_not_modify_trail,
        does_not_affect_liveness],
"
    MR_save_transient_registers();
    Copy = MR_deep_copy(Value, (MR_TypeInfo) TypeInfo_for_T, NULL, NULL);
    MR_restore_transient_registers();
").

:- pragma foreign_proc("C#",
    copy(X::ui, Y::uo),
    [may_call_mercury, thread_safe, promise_pure, terminates],
"
    Y = builtin.deep_copy(X);
").

:- pragma foreign_proc("C#",
    copy(X::in, Y::uo),
    [may_call_mercury, thread_safe, promise_pure, terminates],
"
    Y = builtin.deep_copy(X);
").

:- pragma foreign_proc("Java",
    copy(X::ui, Y::uo),
    [may_call_mercury, thread_safe, promise_pure, terminates],
"
    try {
        Y = builtin.deep_copy(X);
    } catch (java.lang.InstantiationException E) {
        throw new RuntimeException(E);
    } catch (java.lang.IllegalAccessException E) {
        throw new RuntimeException(E);
    }
").

:- pragma foreign_proc("Java",
    copy(X::in, Y::uo),
    [may_call_mercury, thread_safe, promise_pure, terminates],
"
    try {
        Y = builtin.deep_copy(X);
    } catch (java.lang.InstantiationException E) {
        throw new RuntimeException(E);
    } catch (java.lang.IllegalAccessException E) {
        throw new RuntimeException(E);
    }
").

%---------------------------------------------------------------------------%

%
% A definition of the Mercury type void/0 is needed because we can generate
% references to it in code. See tests/hard_coded/nullary_ho_func.m for an
% example of code which does.
%
:- pragma foreign_code("C#", "
    [System.Serializable]
    public class Void_0
    {
        // Make the constructor private to ensure that we can
        // never create an instance of this class.
        private Void_0()
        {
        }
    }
").
:- pragma foreign_code("Java", "
    public static class Void_0 implements java.io.Serializable
    {
        // Make the constructor private to ensure that we can
        // never create an instance of this class.
        private Void_0()
        {
        }
    }
").

%---------------------------------------------------------------------------%

:- pragma foreign_code("Java", "

    //
    // Definitions of builtin types
    //

    public static class Tuple_0
    {
        // stub only
    }

    public static class Func_0
    {
        // stub only
    }

    public static class C_pointer_0
    {
        // stub only
    }

    //
    // Generic unification/comparison routines
    //

    public static boolean
    unify_2_p_0 (jmercury.runtime.TypeInfo_Struct ti,
        java.lang.Object x, java.lang.Object y)
    {
        return jmercury.rtti_implementation.generic_unify_2_p_0(ti, x, y);
    }

    public static Comparison_result_0
    compare_3_p_0 (jmercury.runtime.TypeInfo_Struct ti,
        java.lang.Object x, java.lang.Object y)
    {
        return jmercury.rtti_implementation.generic_compare_3_p_0(ti, x, y);
    }

    public static Comparison_result_0
    compare_3_p_1 (jmercury.runtime.TypeInfo_Struct ti,
        java.lang.Object x, java.lang.Object y)
    {
        return compare_3_p_0(ti, x, y);
    }

    public static Comparison_result_0
    compare_3_p_2 (jmercury.runtime.TypeInfo_Struct ti,
        java.lang.Object x, java.lang.Object y)
    {
        return compare_3_p_0(ti, x, y);
    }

    public static Comparison_result_0
    compare_3_p_3 (jmercury.runtime.TypeInfo_Struct ti,
        java.lang.Object x, java.lang.Object y)
    {
        return compare_3_p_0(ti, x, y);
    }

    public static Comparison_result_0
    compare_representation_3_p_0 (jmercury.runtime.TypeInfo_Struct ti,
        java.lang.Object x, java.lang.Object y)
    {
        // stub only
        throw new java.lang.Error (
            ""compare_representation_3_p_0/3 not implemented"");
    }

    //
    // Type-specific unification routines for builtin types
    //

    public static boolean
    __Unify____tuple_0_0(java.lang.Object[] x, java.lang.Object[] y)
    {
        // stub only
        throw new java.lang.Error (
            ""unify/2 for tuple types not implemented"");
    }

    public static boolean
    __Unify____func_0_0(java.lang.Object[] x, java.lang.Object[] y)
    {
        // stub only
        throw new java.lang.Error (
            ""unify/2 for tuple types not implemented"");
    }


    public static boolean
    __Unify____c_pointer_0_0(java.lang.Object x, java.lang.Object y)
    {
        // XXX should we try calling a Java comparison routine?
        throw new java.lang.Error (""unify/2 called for c_pointer type"");
    }

    public static boolean
    __Unify____void_0_0(builtin.Void_0 x, builtin.Void_0 y)
    {
        // there should never be any values of type void/0
        throw new java.lang.Error (""unify/2 called for void type"");
    }

    //
    // Type-specific comparison routines for builtin types
    //

    public static Comparison_result_0
    __Compare____tuple_0_0(java.lang.Object[] x, java.lang.Object[] y)
    {
        // stub only
        throw new java.lang.Error
            (""compare/3 for tuple types not implemented"");
    }

    public static Comparison_result_0
    __Compare____func_0_0(java.lang.Object[] x, java.lang.Object[] y)
    {
        // comparing values of higher-order types is a run-time error
        throw new java.lang.Error (""compare/3 called for func type"");
    }

    public static Comparison_result_0
    __Compare____c_pointer_0_0(java.lang.Object x, java.lang.Object y)
    {
        // XXX should we try calling a Java comparison routine?
        throw new java.lang.Error
            (""compare/3 called for c_pointer type"");
    }

    public static Comparison_result_0
    __Compare____void_0_0(builtin.Void_0 x, builtin.Void_0 y)
    {
        // there should never be any values of type void/0
        throw new java.lang.Error (""compare/3 called for void type"");
    }
").

%---------------------------------------------------------------------------%
%
% semidet_succeed and semidet_fail
%

% semidet_succeed and semidet_fail are implemented using the foreign language
% interface to make sure that the compiler doesn't issue any determinism
% warnings for them.

% We can't just use "true" and "fail" in the Mercury versions, because that
% provokes warnings from determinism analysis, and the library is compiled
% with --halt-at-warn. So instead we use 0+0 = (or \=) 0.
% This is guaranteed to succeed or fail (respectively),
% and with a bit of luck will even get optimized by constant propagation.
% But this optimization won't happen until after determinism analysis,
% which doesn't know anything about integer arithmetic,
% so this code won't provide a warning from determinism analysis.

:- pragma foreign_proc("C",
    semidet_succeed,
    [will_not_call_mercury, thread_safe, promise_pure,
        does_not_affect_liveness],
"
    SUCCESS_INDICATOR = MR_TRUE;
").
:- pragma foreign_proc("C#",
    semidet_succeed,
    [will_not_call_mercury, thread_safe, promise_pure],
"
    SUCCESS_INDICATOR = true;
").

semidet_succeed :-
    0 + 0 = 0.

:- pragma foreign_proc("C",
    semidet_fail,
    [will_not_call_mercury, thread_safe, promise_pure,
        does_not_affect_liveness],
"
    SUCCESS_INDICATOR = MR_FALSE;
").
:- pragma foreign_proc("C#",
    semidet_fail,
    [will_not_call_mercury, thread_safe, promise_pure],
"
    SUCCESS_INDICATOR = false;
").

semidet_fail :-
    0 + 0 \= 0.

semidet_true :-
    semidet_succeed.

semidet_false :-
    semidet_fail.

%---------------------------------------------------------------------------%
%
% cc_multi_equal
%

% NOTE: cc_multi_equal/2 is handled specially in browser/declarative_tree.m.
% Any changes here may need to be reflected there.

:- pragma promise_equivalent_clauses(pred(cc_multi_equal/2)).

:- pragma foreign_proc("C",
    cc_multi_equal(X::in, Y::out),
    [will_not_call_mercury, thread_safe, promise_pure,
        does_not_affect_liveness],
"
    Y = X;
").
:- pragma foreign_proc("C",
    cc_multi_equal(X::di, Y::uo),
    [will_not_call_mercury, thread_safe, promise_pure,
        does_not_affect_liveness],
"
    Y = X;
").

:- pragma foreign_proc("C#",
    cc_multi_equal(X::in, Y::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    Y = X;
").
:- pragma foreign_proc("C#",
    cc_multi_equal(X::di, Y::uo),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    Y = X;
").

:- pragma foreign_proc("Java",
    cc_multi_equal(X::in, Y::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    Y = X;
").
:- pragma foreign_proc("Java",
    cc_multi_equal(X::di, Y::uo),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    Y = X;
").

%---------------------------------------------------------------------------%

impure_true :-
    impure private_builtin.imp.

semipure_true :-
    semipure private_builtin.semip.

%---------------------------------------------------------------------------%

% NOTE: dynamic_cast/2 is handled specially compiler/const_prop.m.
% Any changes here may need to be reflected here.

dynamic_cast(X, Y) :-
    private_builtin.typed_unify(X, Y).

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    unsafe_cast_any_to_ground(X::ia) = (Y::out),
    [promise_pure, will_not_call_mercury, thread_safe, will_not_modify_trail],
"
    Y = X;
").

:- pragma foreign_proc("C#",
    unsafe_cast_any_to_ground(X::ia) = (Y::out),
    [promise_pure, will_not_call_mercury, thread_safe, will_not_modify_trail],
"
    Y = X;
").

:- pragma foreign_proc("Java",
    unsafe_cast_any_to_ground(X::ia) = (Y::out),
    [promise_pure, will_not_call_mercury, thread_safe, will_not_modify_trail],
"
    Y = X;
").

%---------------------------------------------------------------------------%

init_runtime_hooks.

:- pragma foreign_proc("C",
    init_runtime_hooks,
    [will_not_call_mercury, thread_safe, may_not_duplicate],
"
#ifdef MR_HIGHLEVEL_CODE
    MR_special_pred_hooks.MR_unify_tuple_pred = ML_unify_tuple;
    MR_special_pred_hooks.MR_compare_tuple_pred = ML_compare_tuple;
    MR_special_pred_hooks.MR_compare_rep_tuple_pred = ML_compare_rep_tuple;
#else
    MR_special_pred_hooks.MR_unify_tuple_pred =
        MR_ENTRY(mercury__builtin__unify_tuple_2_0);
    MR_special_pred_hooks.MR_compare_tuple_pred =
        MR_ENTRY(mercury__builtin__compare_tuple_3_0);
    MR_special_pred_hooks.MR_compare_rep_tuple_pred =
        MR_ENTRY(mercury__builtin__compare_rep_tuple_3_0);
#endif
").

%---------------------------------------------------------------------------%
:- end_module builtin.
%---------------------------------------------------------------------------%
