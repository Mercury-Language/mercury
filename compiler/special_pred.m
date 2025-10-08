%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1995-2000, 2002-2011 The University of Melbourne.
% Copyright (C) 2014-2022, 2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: special_pred.m.
% Main author: fjh.
%
% Certain predicates are implicitly defined for every type by the compiler.
% This module defines most of the characteristics of those predicates.
% (The actual code for these predicates is generated in unify_proc.m.)
%
%-----------------------------------------------------------------------------%

:- module hlds.special_pred.
:- interface.

:- import_module hlds.hlds_data.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.status.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module list.
:- import_module map.

:- type special_pred_maps
    --->    special_pred_maps(
                spm_unify_map           :: map(type_ctor, pred_id),
                spm_index_map           :: map(type_ctor, pred_id),
                spm_compare_map         :: map(type_ctor, pred_id)
            ).

:- pred search_special_pred_maps(special_pred_maps::in,
    special_pred_id::in, type_ctor::in, pred_id::out) is semidet.
:- pred lookup_special_pred_maps(special_pred_maps::in,
    special_pred_id::in, type_ctor::in, pred_id::out) is det.

%-----------------------------------------------------------------------------%

:- pred special_pred_list(list(special_pred_id)::out) is det.

:- pred special_pred_description(special_pred_id::in, string::out) is det.

%-----------------------------------------------------------------------------%

:- pred special_pred_mode_num(special_pred_id::in, int::out) is det.

    % This predicate always returns determinism `semidet' for unification
    % procedures. For types with only one value, the unification is actually
    % `det', however we need to pretend it is `semidet' so that it can be
    % called correctly from the polymorphic `unify' procedure.
    %
:- pred special_pred_interface(special_pred_id::in, mer_type::in,
    list(mer_type)::out, list(mer_mode)::out, determinism::out) is det.

    % Given a special pred id and the list of its arguments, work out which
    % argument specifies the type that this special predicate is for. Note that
    % this gets called after the polymorphism.m pass, so type_info arguments
    % may have been inserted at the start; hence we find the type at a known
    % position from the end of the list (by using list.reverse).
    %
    % Currently for most of the special predicates the type variable can be
    % found in the last type argument, except for index, for which it is the
    % second-last argument.
    %
:- pred special_pred_get_type(special_pred_id::in, list(prog_var)::in,
    prog_var::out) is semidet.

:- pred special_pred_get_type_det(special_pred_id::in, list(prog_var)::in,
    prog_var::out) is det.

%-----------------------------------------------------------------------------%

    % Succeeds if the declarations and clauses for the special predicates
    % for the given type generated only when required. This will succeed
    % for imported types for which the special predicates do not need
    % typechecking.
    %
:- pred special_pred_is_generated_lazily(module_info::in, type_ctor::in)
    is semidet.

    % XXX Document me, and the relationship to the /2 pred just above.
    %
:- pred special_pred_is_generated_lazily_for_defn(module_info::in,
    type_ctor::in, hlds_type_body::in, type_status::in) is semidet.

    % A compiler-generated predicate only needs type checking if
    % (a) it is a user-defined equality pred, or
    % (b) it is the unification or comparison predicate for an existentially
    %     quantified type, or
    % (c) it is the initialisation predicate for a solver type.
    %
:- pred special_pred_for_type_needs_typecheck(module_info::in,
    special_pred_id::in, hlds_type_body::in) is semidet.

    % Succeed if the type can have clauses generated for its special
    % predicates. This will fail for abstract types and types for which
    % the RTTI information is defined by hand.
    %
:- pred can_generate_special_pred_clauses_for_type(module_info::in,
    type_ctor::in, hlds_type_body::in) is semidet.

    % Is this a builtin type whose special predicates are defined in Mercury?
    % If yes, return the name of the type.
    %
:- pred is_builtin_type_special_preds_defined_in_mercury(type_ctor::in,
    string::out) is semidet.

%-----------------------------------------------------------------------------%

    % Does the compiler generate the RTTI for the builtin types, or is it
    % hand-coded?
    %
:- pred compiler_generated_rtti_for_builtins(module_info::in) is semidet.

%-----------------------------------------------------------------------------%

    % Look up the pred_id and proc_id for a type specific
    % unification/comparison/index predicate.
    %
:- pred get_special_proc(module_info::in, type_ctor::in,
    special_pred_id::in, sym_name::out, pred_id::out, proc_id::out) is semidet.
:- pred get_special_proc_det(module_info::in, type_ctor::in,
    special_pred_id::in, sym_name::out, pred_id::out, proc_id::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.pred_table.
:- import_module hlds.type_util.
:- import_module libs.
:- import_module libs.globals.
:- import_module mdbcomp.builtin_modules.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_type.

:- import_module bool.
:- import_module maybe.
:- import_module one_or_more.
:- import_module require.
:- import_module string.

%-----------------------------------------------------------------------------%

search_special_pred_maps(SpecMaps, SpecialPredId, TypeCtor, PredId) :-
    select_special_pred_map(SpecMaps, SpecialPredId, SpecMap),
    map.search(SpecMap, TypeCtor, PredId).

lookup_special_pred_maps(SpecMaps, SpecialPredId, TypeCtor, PredId) :-
    select_special_pred_map(SpecMaps, SpecialPredId, SpecMap),
    map.lookup(SpecMap, TypeCtor, PredId).

:- pred select_special_pred_map(special_pred_maps::in,
    special_pred_id::in, map(type_ctor, pred_id)::out) is det.
:- pragma inline(pred(select_special_pred_map/3)).

select_special_pred_map(SpecMaps, SpecialPredId, SpecMap) :-
    (
        SpecialPredId = spec_pred_unify,
        SpecMap = SpecMaps ^ spm_unify_map
    ;
        SpecialPredId = spec_pred_index,
        SpecMap = SpecMaps ^ spm_index_map
    ;
        SpecialPredId = spec_pred_compare,
        SpecMap = SpecMaps ^ spm_compare_map
    ).

%-----------------------------------------------------------------------------%

special_pred_list([spec_pred_unify, spec_pred_index, spec_pred_compare]).

special_pred_description(spec_pred_unify,   "unification predicate").
special_pred_description(spec_pred_compare, "comparison predicate").
special_pred_description(spec_pred_index,   "indexing predicate").

%-----------------------------------------------------------------------------%

special_pred_mode_num(_, 0).
    % Mode num for special procs is always 0 (the first mode).

%-----------------------------------------------------------------------------%

special_pred_interface(SpecialPredId, Type, ArgTypes, ArgModes, Detism) :-
    (
        SpecialPredId = spec_pred_unify,
        ArgTypes = [Type, Type],
        in_mode(In),
        ArgModes = [In, In],
        Detism = detism_semi
    ;
        SpecialPredId = spec_pred_index,
        ArgTypes = [Type, int_type],
        in_mode(In),
        out_mode(Out),
        ArgModes = [In, Out],
        Detism = detism_det
    ;
        SpecialPredId = spec_pred_compare,
        ArgTypes = [comparison_result_type, Type, Type],
        in_mode(In),
        uo_mode(Uo),
        ArgModes = [Uo, In, In],
        Detism = detism_det
    ).

special_pred_get_type(spec_pred_unify, Types, T) :-
    list.reverse(Types, [T | _]).
special_pred_get_type(spec_pred_index, Types, T) :-
    list.reverse(Types, [_, T | _]).
special_pred_get_type(spec_pred_compare, Types, T) :-
    list.reverse(Types, [T | _]).

special_pred_get_type_det(SpecialId, ArgTypes, Type) :-
    ( if special_pred_get_type(SpecialId, ArgTypes, TypePrime) then
        Type = TypePrime
    else
        unexpected($pred, "special_pred_get_type failed")
    ).

%-----------------------------------------------------------------------------%

special_pred_is_generated_lazily(ModuleInfo, TypeCtor) :-
    ( if classify_type_ctor_if_special(TypeCtor, CtorCat0) then
        MaybeCtorCat = yes(CtorCat0)
    else
        MaybeCtorCat = no
    ),
    (
        MaybeCtorCat = yes(ctor_cat_tuple)
    ;
        (
            MaybeCtorCat = no
        ;
            MaybeCtorCat = yes(CtorCat),
            is_introduced_type_info_type_category(CtorCat) = yes
        ),
        module_info_get_type_table(ModuleInfo, TypeTable),
        search_type_ctor_defn(TypeTable, TypeCtor, TypeDefn),
        hlds_data.get_type_defn_body(TypeDefn, TypeBody),
        hlds_data.get_type_defn_status(TypeDefn, TypeStatus),
        special_pred_is_generated_lazily_2(ModuleInfo, TypeBody, TypeStatus)
    ).

special_pred_is_generated_lazily_for_defn(ModuleInfo, TypeCtor,
        TypeBody, TypeStatus) :-
    % We don't want special preds for solver types to be generated lazily
    % because we have to insert calls to their initialisation preds during
    % mode analysis and we therefore require the appropriate names to
    % appear in the symbol table.

    TypeBody \= hlds_solver_type(_),
    TypeBody \= hlds_abstract_type(abstract_solver_type),

    ( if classify_type_ctor_if_special(TypeCtor, CtorCat0) then
        MaybeCtorCat = yes(CtorCat0)
    else
        MaybeCtorCat = no
    ),
    (
        MaybeCtorCat = yes(ctor_cat_tuple)
    ;
        (
            MaybeCtorCat = no
        ;
            MaybeCtorCat = yes(CtorCat),
            is_introduced_type_info_type_category(CtorCat) = yes
        ),
        special_pred_is_generated_lazily_2(ModuleInfo, TypeBody, TypeStatus)
    ).

:- pred special_pred_is_generated_lazily_2(module_info::in,
    hlds_type_body::in, type_status::in) is semidet.

special_pred_is_generated_lazily_2(ModuleInfo, TypeBody, TypeStatus) :-
    type_status_defined_in_this_module(TypeStatus) = no,

    % We can't generate clauses for unification predicates for foreign types
    % lazily because they call the polymorphic procedure
    % private_builtin.nyi_foreign_type_unify.
    % polymorphism.process_generated_pred can't handle calls to polymorphic
    % procedures after the initial polymorphism pass.
    TypeBody \= hlds_foreign_type(_),

    % The special predicates for types with user-defined equality or
    % existentially typed constructors are always generated immediately
    % by make_hlds.m.
    not special_pred_for_type_needs_typecheck(ModuleInfo, spec_pred_unify,
        TypeBody).

special_pred_for_type_needs_typecheck(ModuleInfo, SpecialPredId, TypeBody) :-
    (
        (
            SpecialPredId = spec_pred_unify,
            type_body_has_user_defined_equality_pred(ModuleInfo, TypeBody,
                NonCanonical),
            require_complete_switch [NonCanonical]
            (
                ( NonCanonical = noncanon_uni_cmp(_, _)
                ; NonCanonical = noncanon_uni_only(_)
                ; NonCanonical = noncanon_cmp_only(_)
                )
            ;
                ( NonCanonical = noncanon_abstract(_)
                ; NonCanonical = noncanon_subtype
                ),
                fail
            )
        ;
            SpecialPredId = spec_pred_compare,
            type_body_has_user_defined_equality_pred(ModuleInfo, TypeBody,
                NonCanonical),
            require_complete_switch [NonCanonical]
            (
                ( NonCanonical = noncanon_uni_cmp(_, _)
                ; NonCanonical = noncanon_cmp_only(_)
                )
            ;
                ( NonCanonical = noncanon_uni_only(_)
                ; NonCanonical = noncanon_abstract(_)
                ; NonCanonical = noncanon_subtype
                ),
                fail
            )
        )
    ;
        TypeBody = hlds_du_type(TypeBodyDu),
        one_or_more(HeadCtor, TailCtors) = TypeBodyDu ^ du_type_ctors,
        some [Ctor] (
            ( Ctor = HeadCtor
            ; list.member(Ctor, TailCtors)
            ),
            Ctor = ctor(_, MaybeExistConstraints, _, _, _, _),
            MaybeExistConstraints = exist_constraints(_)
        )
    ).

can_generate_special_pred_clauses_for_type(ModuleInfo, TypeCtor, TypeBody) :-
    (
        TypeBody \= hlds_abstract_type(_)
    ;
        % The types which have their unification and comparison
        % predicates defined in private_builtin.m.
        compiler_generated_rtti_for_builtins(ModuleInfo),
        is_builtin_type_special_preds_defined_in_mercury(TypeCtor, _)
    ),
    not type_ctor_has_hand_defined_rtti(TypeCtor, TypeBody),
    not (
        type_body_has_user_defined_equality_pred(ModuleInfo, TypeBody,
            NonCanonical),
        NonCanonical = noncanon_abstract(_IsSolverType)
    ).

is_builtin_type_special_preds_defined_in_mercury(TypeCtor, TypeName) :-
    Builtin = mercury_public_builtin_module,
    TypeCtor = type_ctor(qualified(Builtin, TypeName), 0),
    % XXX Treating "pred" as a builtin type without also treating "func"
    % the same way looks wrong to me. -zs
    ( is_builtin_type_name(TypeName)
    ; TypeName = "pred"
    ).

%-----------------------------------------------------------------------------%

compiler_generated_rtti_for_builtins(ModuleInfo) :-
    % The compiler generates the rtti for the builtins when we are on the
    % non-C backends. We don't generate the rtti on the C backends as the
    % C runtime contains references to this rtti, so the rtti must be defined
    % in the runtime, not the library.

    module_info_get_globals(ModuleInfo, Globals),
    globals.get_target(Globals, Target),
    ( Target = target_csharp
    ; Target = target_java
    ).

%-----------------------------------------------------------------------------%

get_special_proc(ModuleInfo, TypeCtor, SpecialPredId,
        PredName, PredId, ProcId) :-
    TypeCategory = classify_type_ctor(ModuleInfo, TypeCtor),
    get_ctor_cat_builtin_type_name(TypeCategory) = MaybeBuiltinTypeName,
    (
        MaybeBuiltinTypeName = no,
        module_info_get_special_pred_maps(ModuleInfo, SpecialPredMaps),
        search_special_pred_maps(SpecialPredMaps, SpecialPredId, TypeCtor,
            PredId),
        module_info_pred_info(ModuleInfo, PredId, PredInfo),
        Module = pred_info_module(PredInfo),
        Name = pred_info_name(PredInfo),
        PredName = qualified(Module, Name),
        special_pred_mode_num(SpecialPredId, ProcInt),
        proc_id_to_int(ProcId, ProcInt)
    ;
        MaybeBuiltinTypeName = yes(BuiltinTypeName),
        special_pred_name_arity(SpecialPredId, SpecialName, _, Arity),
        Name = "builtin_" ++ SpecialName ++ "_" ++ BuiltinTypeName,
        % None of the special preds are in fact functions, so for them,
        % user arity and pred form arity are the same.
        UserArity = user_arity(Arity),
        lookup_builtin_pred_proc_id(ModuleInfo, mercury_private_builtin_module,
            Name, pf_predicate, UserArity, only_mode, PredId, ProcId),
        PredName = qualified(mercury_private_builtin_module, Name)
    ).

get_special_proc_det(ModuleInfo, TypeCtor, SpecialPredId, PredName,
        PredId, ProcId) :-
    ( if
        get_special_proc(ModuleInfo, TypeCtor, SpecialPredId,
            PredNamePrime, PredIdPrime, ProcIdPrime)
    then
        PredName = PredNamePrime,
        PredId = PredIdPrime,
        ProcId = ProcIdPrime
    else
        unexpected($pred, "get_special_proc failed")
    ).

:- func get_ctor_cat_builtin_type_name(type_ctor_category) = maybe(string).

get_ctor_cat_builtin_type_name(CtorCat) = MaybeName :-
    (
        CtorCat = ctor_cat_builtin(cat_builtin_int(int_type_int)),
        MaybeName = yes("int")
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_int(int_type_uint)),
        MaybeName = yes("uint")
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_int(int_type_int8)),
        MaybeName = yes("int8")
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_int(int_type_uint8)),
        MaybeName = yes("uint8")
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_int(int_type_int16)),
        MaybeName = yes("int16")
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_int(int_type_uint16)),
        MaybeName = yes("uint16")
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_int(int_type_int32)),
        MaybeName = yes("int32")
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_int(int_type_uint32)),
        MaybeName = yes("uint32")
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_int(int_type_int64)),
        MaybeName = yes("int64")
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_int(int_type_uint64)),
        MaybeName = yes("uint64")
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_char),
        MaybeName = yes("character")
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_float),
        MaybeName = yes("float")
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_string),
        MaybeName = yes("string")
    ;
        CtorCat = ctor_cat_higher_order,
        MaybeName = yes("pred")
    ;
        CtorCat = ctor_cat_tuple,
        MaybeName = yes("tuple")
    ;
        ( CtorCat = ctor_cat_enum(_)
        ; CtorCat = ctor_cat_builtin_dummy
        ; CtorCat = ctor_cat_user(_)
        ; CtorCat = ctor_cat_system(_)
        ),
        MaybeName = no
    ;
        CtorCat = ctor_cat_variable,
        unexpected($pred, "variable type")
    ;
        CtorCat = ctor_cat_void,
        unexpected($pred, "void_type")
    ).

%-----------------------------------------------------------------------------%
:- end_module hlds.special_pred.
%-----------------------------------------------------------------------------%
