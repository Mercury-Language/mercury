%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1995-2000,2002-2011 The University of Melbourne.
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
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_data.

:- import_module list.
:- import_module map.
:- import_module pair.

:- type special_pred_map    ==  map(special_pred, pred_id).

:- type special_pred        ==  pair(special_pred_id, type_ctor).

    % Return the predicate name we should use for the given special_pred
    % for the given type constructor.
    %
:- func special_pred_name(special_pred_id, type_ctor) = string.

    % This predicate always returns determinism `semidet' for unification
    % procedures. For types with only one value, the unification is actually
    % `det', however we need to pretend it is `semidet' so that it can be
    % called correctly from the polymorphic `unify' procedure.
    %
:- pred special_pred_interface(special_pred_id::in, mer_type::in,
    list(mer_type)::out, list(mer_mode)::out, determinism::out) is det.

:- pred special_pred_mode_num(special_pred_id::in, int::out) is det.

:- pred special_pred_list(list(special_pred_id)::out) is det.

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

:- pred special_pred_description(special_pred_id::in, string::out) is det.

    % Succeeds if the declarations and clauses for the special predicates
    % for the given type generated only when required. This will succeed
    % for imported types for which the special predicates do not need
    % typechecking.
    %
:- pred special_pred_is_generated_lazily(module_info::in, type_ctor::in)
    is semidet.

:- pred special_pred_is_generated_lazily(module_info::in, type_ctor::in,
    hlds_type_body::in, import_status::in) is semidet.

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
    % If yes, return the name of the type.type
    %
:- pred is_builtin_type_special_preds_defined_in_mercury(type_ctor::in,
    string::out) is semidet.

    % Does the compiler generate the RTTI for the builtin types, or is
    % it hand-coded?
    %
:- pred compiler_generated_rtti_for_builtins(module_info::in) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.type_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_type.

:- import_module bool.
:- import_module maybe.
:- import_module require.
:- import_module string.

%-----------------------------------------------------------------------------%

special_pred_list([spec_pred_unify, spec_pred_index, spec_pred_compare]).

special_pred_mode_num(_, 0).
    % mode num for special procs is always 0 (the first mode)

special_pred_name(SpecialPred, type_ctor(SymName, Arity)) = Name :-
    BaseName = get_special_pred_id_target_name(SpecialPred),
    AppendTypeId = spec_pred_name_append_type_id,
    (
        AppendTypeId = yes,
        Name = BaseName ++ sym_name_to_string(SymName)
            ++ "/" ++ int_to_string(Arity)
    ;
        AppendTypeId = no,
        Name = BaseName
    ).

:- func spec_pred_name_append_type_id = bool.
:- pragma inline(spec_pred_name_append_type_id/0).

% XXX The name demanglers don't yet understand predicate names for special
% preds that have the type name and and arity appended, and the hand-written
% unify/compare predicates for builtin types such as typeinfo have the plain
% base names. Therefore returning "yes" here is useful only for debugging
% the compiler.
spec_pred_name_append_type_id = no.

special_pred_interface(spec_pred_unify, Type, [Type, Type], [In, In],
        detism_semi) :-
    in_mode(In).
special_pred_interface(spec_pred_index, Type, [Type, int_type], [In, Out],
        detism_det) :-
    in_mode(In),
    out_mode(Out).
special_pred_interface(spec_pred_compare, Type,
        [comparison_result_type, Type, Type], [Uo, In, In], detism_det) :-
    in_mode(In),
    uo_mode(Uo).
special_pred_interface(spec_pred_init, Type, [Type], [InAny], detism_det) :-
    InAny = out_any_mode.

special_pred_get_type(spec_pred_unify, Types, T) :-
    list.reverse(Types, [T | _]).
special_pred_get_type(spec_pred_index, Types, T) :-
    list.reverse(Types, [_, T | _]).
special_pred_get_type(spec_pred_compare, Types, T) :-
    list.reverse(Types, [T | _]).
special_pred_get_type(spec_pred_init, Types, T) :-
    list.reverse(Types, [T | _]).

special_pred_get_type_det(SpecialId, ArgTypes, Type) :-
    ( special_pred_get_type(SpecialId, ArgTypes, TypePrime) ->
        Type = TypePrime
    ;
        unexpected($module, $pred, "special_pred_get_type failed")
    ).

special_pred_description(spec_pred_unify,   "unification predicate").
special_pred_description(spec_pred_compare, "comparison predicate").
special_pred_description(spec_pred_index,   "indexing predicate").
special_pred_description(spec_pred_init,    "initialisation predicate").

special_pred_is_generated_lazily(ModuleInfo, TypeCtor) :-
    CtorCat = classify_type_ctor(ModuleInfo, TypeCtor),
    (
        CtorCat = ctor_cat_tuple
    ;
        ( CtorCat = ctor_cat_user(_)
        ; CtorCat = ctor_cat_enum(_)
        ; is_introduced_type_info_type_category(CtorCat) = yes
        ),
        module_info_get_type_table(ModuleInfo, TypeTable),
        search_type_ctor_defn(TypeTable, TypeCtor, TypeDefn),
        hlds_data.get_type_defn_body(TypeDefn, Body),
        hlds_data.get_type_defn_status(TypeDefn, Status),
        special_pred_is_generated_lazily_2(ModuleInfo, TypeCtor, Body, Status)
    ).

special_pred_is_generated_lazily(ModuleInfo, TypeCtor, Body, Status) :-
    % We don't want special preds for solver types to be generated lazily
    % because we have to insert calls to their initialisation preds during
    % mode analysis and we therefore require the appropriate names to
    % appear in the symbol table.

    Body \= hlds_solver_type(_, _),
    Body \= hlds_abstract_type(abstract_solver_type),

    CtorCat = classify_type_ctor(ModuleInfo, TypeCtor),
    (
        CtorCat = ctor_cat_tuple
    ;
        ( CtorCat = ctor_cat_user(_)
        ; CtorCat = ctor_cat_enum(_)
        ; is_introduced_type_info_type_category(CtorCat) = yes
        ),
        special_pred_is_generated_lazily_2(ModuleInfo, TypeCtor, Body, Status)
    ).

:- pred special_pred_is_generated_lazily_2(module_info::in,
    type_ctor::in, hlds_type_body::in, import_status::in) is semidet.

special_pred_is_generated_lazily_2(ModuleInfo, _TypeCtor, Body, Status) :-
    (
        status_defined_in_this_module(Status) = no
    ;
        module_info_get_globals(ModuleInfo, Globals),
        globals.lookup_bool_option(Globals, special_preds, no)
    ),

    % We can't generate clauses for unification predicates for foreign types
    % lazily because they call the polymorphic procedure
    % private_builtin.nyi_foreign_type_unify.
    % polymorphism.process_generated_pred can't handle calls to polymorphic
    % procedures after the initial polymorphism pass.
    %
    Body \= hlds_foreign_type(_),

    % The special predicates for types with user-defined equality or
    % existentially typed constructors are always generated immediately
    % by make_hlds.m.
    \+ special_pred_for_type_needs_typecheck(ModuleInfo, spec_pred_unify,
        Body).

special_pred_for_type_needs_typecheck(ModuleInfo, SpecialPredId, Body) :-
    (
        SpecialPredId = spec_pred_unify,
        type_body_has_user_defined_equality_pred(ModuleInfo, Body,
            unify_compare(_, _))
    ;
        SpecialPredId = spec_pred_compare,
        type_body_has_user_defined_equality_pred(ModuleInfo, Body,
            unify_compare(_, UserCmp)), UserCmp = yes(_)
    ;
        SpecialPredId \= spec_pred_init,
        Ctors = Body ^ du_type_ctors,
        list.member(Ctor, Ctors),
        Ctor = ctor(ExistQTVars, _, _, _, _),
        ExistQTVars = [_ | _]
    ;
        SpecialPredId = spec_pred_init,
        type_body_is_solver_type(ModuleInfo, Body)
    ).

can_generate_special_pred_clauses_for_type(ModuleInfo, TypeCtor, Body) :-
    (
        Body \= hlds_abstract_type(_)
    ;
        % The types which have their unification and comparison
        % predicates defined in private_builtin.m.
        compiler_generated_rtti_for_builtins(ModuleInfo),
        is_builtin_type_special_preds_defined_in_mercury(TypeCtor, _)
    ),
    \+ type_ctor_has_hand_defined_rtti(TypeCtor, Body),
    \+ type_body_has_user_defined_equality_pred(ModuleInfo, Body,
        abstract_noncanonical_type(_IsSolverType)).

is_builtin_type_special_preds_defined_in_mercury(TypeCtor, TypeName) :-
    Builtin = mercury_public_builtin_module,
    TypeCtor = type_ctor(qualified(Builtin, TypeName), 0),
    ( TypeName = "int"
    ; TypeName = "string"
    ; TypeName = "character"
    ; TypeName = "float"
    ; TypeName = "pred"
    ).

%-----------------------------------------------------------------------------%

    % The compiler generates the rtti for the builtins when we are on the
    % non-C backends. We don't generate the rtti on the C backends as the
    % runtime contains references to this rtti so the rtti must be defined
    % in the runtime not the library.
    %
compiler_generated_rtti_for_builtins(ModuleInfo) :-
    module_info_get_globals(ModuleInfo, Globals),
    globals.get_target(Globals, Target),
    ( Target = target_il
    ; Target = target_csharp
    ; Target = target_java
    ; Target = target_erlang
    ).

%-----------------------------------------------------------------------------%
:- end_module hlds.special_pred.
%-----------------------------------------------------------------------------%
