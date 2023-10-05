%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2005-2012 The University of Melbourne.
% Copyright (C) 2014-2021 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: prog_type.m.
% Main author: fjh.
%
% Utility predicates dealing with types in the parse tree. The predicates for
% doing type substitutions are in prog_type_subst.m, while utility predicates
% for dealing with types in the HLDS are in type_util.m.
%
%---------------------------------------------------------------------------%

:- module parse_tree.prog_type.
:- interface.

:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_util.

:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module term.

%---------------------------------------------------------------------------%

    % Given a non-variable type, return its type_ctor and argument types.
    % Fail if the type is a variable.
    %
:- pred type_to_ctor_and_args(mer_type::in, type_ctor::out,
    list(mer_type)::out) is semidet.

    % Given a non-variable type, return its type_ctor and argument types.
    % Abort if the type is a variable.
    %
:- pred type_to_ctor_and_args_det(mer_type::in, type_ctor::out,
    list(mer_type)::out) is det.

    % Given a non-variable type, return its type_ctor.
    % Fail if the type is a variable.
    %
:- pred type_to_ctor(mer_type::in, type_ctor::out) is semidet.

    % Given a non-variable type, return its type_ctor.
    % Abort if the type is a variable.
    %
:- pred type_to_ctor_det(mer_type::in, type_ctor::out) is det.

%---------------------------------------------------------------------------%

    % Convert a list of types to a list of vars. Fail if any of the type are
    % not variables.
    %
:- pred type_list_to_var_list(list(mer_type)::in, list(tvar)::out) is semidet.

    % Convert a var into a variable type.
    %
:- pred var_to_type(tvar_kind_map::in, tvar::in, mer_type::out) is det.

    % Convert a list of vars into a list of variable types.
    %
:- pred var_list_to_type_list(tvar_kind_map::in, list(tvar)::in,
    list(mer_type)::out) is det.

%---------------------------------------------------------------------------%

    % Make error messages more readable by removing some or all
    % module qualifiers from type and mode names contained in the given type
    % or types, regardless of how deeply they are nested.
    %
:- pred strip_module_names_from_type(strip_what_module_names::in,
    mer_type::in, mer_type::out) is det.
:- pred strip_module_names_from_type_list(strip_what_module_names::in,
    list(mer_type)::in, list(mer_type)::out) is det.

%---------------------------------------------------------------------------%

    % The list of type_ctors which are builtins which do not have a
    % hlds_type_defn.
    %
:- func builtin_type_ctors_with_no_hlds_type_defn = list(type_ctor).

%---------------------------------------------------------------------------%

:- type is_dummy_type
    --->    is_dummy_type
    ;       is_not_dummy_type.

:- type is_builtin_dummy_type_ctor
    --->    is_builtin_dummy_type_ctor
    ;       is_builtin_non_dummy_type_ctor
    ;       is_not_builtin_dummy_type_ctor.

:- type type_ctor_category
    --->    ctor_cat_builtin(type_ctor_cat_builtin)
    ;       ctor_cat_builtin_dummy
    ;       ctor_cat_void
    ;       ctor_cat_variable
    ;       ctor_cat_higher_order
    ;       ctor_cat_tuple
    ;       ctor_cat_enum(type_ctor_cat_enum)
    ;       ctor_cat_system(type_ctor_cat_system)
    ;       ctor_cat_user(type_ctor_cat_user).

:- type nb_type_ctor_category =< type_ctor_category
    --->    ctor_cat_builtin_dummy
    ;       ctor_cat_void
    ;       ctor_cat_variable
    ;       ctor_cat_higher_order
    ;       ctor_cat_tuple
    ;       ctor_cat_enum(type_ctor_cat_enum)
    ;       ctor_cat_system(type_ctor_cat_system)
    ;       ctor_cat_user(type_ctor_cat_user).

:- type type_ctor_cat_builtin
    --->    cat_builtin_int(int_type)
    ;       cat_builtin_float
    ;       cat_builtin_char
    ;       cat_builtin_string.

:- type type_ctor_cat_system
    --->    cat_system_type_info
    ;       cat_system_type_ctor_info
    ;       cat_system_typeclass_info
    ;       cat_system_base_typeclass_info.

:- type type_ctor_cat_enum
    --->    cat_enum_mercury
            % XXX TYPE_REPN Should we add an arg specifying
            % the number of bits needed to store the enum?
    ;       cat_enum_foreign.

:- type type_ctor_cat_user
    --->    cat_user_direct_dummy
    ;       cat_user_abstract_dummy
    ;       cat_user_notag
    ;       cat_user_abstract_notag
    ;       cat_user_general.

%---------------------------------------------------------------------------%

    % is_builtin_dummy_type_ctor(type_ctor):
    %
    % Is the given type constructor a dummy type irrespective
    % of its definition?
    %
:- func is_type_ctor_a_builtin_dummy(type_ctor) = is_builtin_dummy_type_ctor.

    % A test for type_info-related types that are introduced by
    % polymorphism.m.  These need to be handled specially in certain
    % places.  For example, mode inference never infers unique modes
    % for these types, since it would not be useful, and since we
    % want to minimize the number of different modes that we infer.
    %
:- pred is_introduced_type_info_type(mer_type::in) is semidet.

:- pred is_introduced_type_info_type_ctor(type_ctor::in) is semidet.

:- func is_introduced_type_info_type_category(type_ctor_category) = bool.

%---------------------------------------------------------------------------%

    % Check for a "new " prefix at the start of the functor name,
    % and remove it if present; if there is no such prefix, fail.
    % (These prefixes are used for construction unifications
    % with existentially typed functors.)
    %
:- pred remove_new_prefix(sym_name::in, sym_name::out) is semidet.

    % Prepend a "new " prefix at the start of the given functor name.
    % (These prefixes are used for construction unifications
    % with existentially typed functors.)
    %
:- pred add_new_prefix(sym_name::in, sym_name::out) is det.

%---------------------------------------------------------------------------%

:- type polymorphism_cell
    --->    type_info_cell(type_ctor)
    ;       typeclass_info_cell.

:- func cell_cons_id(polymorphism_cell) = cons_id.

:- func cell_inst_cons_id(polymorphism_cell, int) = cons_id.

    % Module-qualify the cons_id using module information from the type.
    % The second output value is the cons_id required for use in insts which
    % can be different from that used in types for typeclass_info and
    % type_info. The list(prog_var) is the list of arguments to the cons_id
    % and is just used for obtaining the arity for typeclass_info and type_info
    % cons_ids.
    %
:- pred qualify_cons_id(list(prog_var)::in, cons_id::in,
    cons_id::out, cons_id::out) is det.

%---------------------------------------------------------------------------%

    % Apply a renaming (partial map) to a list.
    % Useful for applying a variable renaming to a list of variables.
    %
:- pred apply_partial_map_to_list(map(T, T)::in, list(T)::in, list(T)::out)
    is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_mode.

:- import_module int.
:- import_module require.
:- import_module string.

%---------------------------------------------------------------------------%

type_to_ctor_and_args(Type, TypeCtor, ArgTypes) :-
    require_complete_switch [Type]
    (
        Type = type_variable(_, _),
        fail
    ;
        Type = defined_type(SymName, ArgTypes, _),
        Arity = list.length(ArgTypes),
        TypeCtor = type_ctor(SymName, Arity)
    ;
        Type = builtin_type(BuiltinType),
        builtin_type_name(BuiltinType, Name),
        SymName = unqualified(Name),
        Arity = 0,
        ArgTypes = [],
        TypeCtor = type_ctor(SymName, Arity)
    ;
        Type = higher_order_type(PorF, ArgTypes, _HO, Purity, _EvalMethod),
        list.length(ArgTypes, NumArgTypes),
        (
            PorF = pf_predicate,
            PorFStr = "pred",
            UserArity = NumArgTypes
        ;
            PorF = pf_function,
            PorFStr = "func",
            UserArity = NumArgTypes - 1
        ),
        SymName0 = unqualified(PorFStr),
        (
            Purity = purity_pure,
            SymName = SymName0
        ;
            Purity = purity_semipure,
            SymName = add_outermost_qualifier("semipure", SymName0)
        ;
            Purity = purity_impure,
            SymName = add_outermost_qualifier("impure", SymName0)
        ),
        TypeCtor = type_ctor(SymName, UserArity)
    ;
        Type = tuple_type(ArgTypes, _),
        SymName = unqualified("{}"),
        Arity = list.length(ArgTypes),
        TypeCtor = type_ctor(SymName, Arity)
    ;
        Type = apply_n_type(_, _, _),
        sorry($pred, "apply/N types")
    ;
        Type = kinded_type(SubType, _),
        type_to_ctor_and_args(SubType, TypeCtor, ArgTypes)
    ).

type_to_ctor_and_args_det(Type, TypeCtor, ArgTypes) :-
    ( if type_to_ctor_and_args(Type, TypeCtorPrime, ArgTypesPrime) then
        TypeCtor = TypeCtorPrime,
        ArgTypes = ArgTypesPrime
    else
        unexpected($pred, "type_to_ctor_and_args failed: " ++ string(Type))
    ).

type_to_ctor(Type, TypeCtor) :-
    % This should be subject to unused argument elimination.
    type_to_ctor_and_args(Type, TypeCtor, _ArgTypes).

type_to_ctor_det(Type, TypeCtor) :-
    % This should be subject to unused argument elimination.
    type_to_ctor_and_args_det(Type, TypeCtor, _ArgTypes).

%---------------------------------------------------------------------------%

type_list_to_var_list([], []).
type_list_to_var_list([Type | Types], [Var | Vars]) :-
    Type = type_variable(Var, _),
    type_list_to_var_list(Types, Vars).

var_to_type(KindMap, Var, Type) :-
    get_tvar_kind(KindMap, Var, Kind),
    Type = type_variable(Var, Kind).

var_list_to_type_list(_, [], []).
var_list_to_type_list(KindMap, [Var | Vars], [Type | Types]) :-
    var_to_type(KindMap, Var, Type),
    var_list_to_type_list(KindMap, Vars, Types).

%---------------------------------------------------------------------------%

strip_module_names_from_type(StripWhat, Type0, Type) :-
    (
        ( Type0 = type_variable(_, _)
        ; Type0 = builtin_type(_)
        ),
        Type = Type0
    ;
        Type0 = defined_type(SymName0, ArgTypes0, Kind),
        strip_module_names_from_sym_name(StripWhat, SymName0, SymName),
        strip_module_names_from_type_list(StripWhat, ArgTypes0, ArgTypes),
        Type = defined_type(SymName, ArgTypes, Kind)
    ;
        Type0 = higher_order_type(PorF, ArgTypes0, HOInstInfo0, Purity, EM),
        strip_module_names_from_type_list(StripWhat, ArgTypes0, ArgTypes),
        strip_module_names_from_ho_inst_info(StripWhat,
            HOInstInfo0, HOInstInfo),
        Type = higher_order_type(PorF, ArgTypes, HOInstInfo, Purity, EM)
    ;
        Type0 = tuple_type(ArgTypes0, Kind),
        strip_module_names_from_type_list(StripWhat, ArgTypes0, ArgTypes),
        Type = tuple_type(ArgTypes, Kind)
    ;
        Type0 = apply_n_type(Var, ArgTypes0, Kind),
        strip_module_names_from_type_list(StripWhat, ArgTypes0, ArgTypes),
        Type = apply_n_type(Var, ArgTypes, Kind)
    ;
        Type0 = kinded_type(SubType0, Kind),
        strip_module_names_from_type(StripWhat, SubType0, SubType),
        Type = kinded_type(SubType, Kind)
    ).

strip_module_names_from_type_list(StripWhat, Types0, Types) :-
    list.map(strip_module_names_from_type(StripWhat), Types0, Types).

%---------------------------------------------------------------------------%

builtin_type_ctors_with_no_hlds_type_defn =
    % Every element of this list must be reflected in the code of
    % builtin_type_ctor in type_ctor_info.m.
    [ type_ctor(qualified(mercury_public_builtin_module, "int"), 0),
      type_ctor(qualified(mercury_public_builtin_module, "int8"), 0),
      type_ctor(qualified(mercury_public_builtin_module, "int16"), 0),
      type_ctor(qualified(mercury_public_builtin_module, "int32"), 0),
      type_ctor(qualified(mercury_public_builtin_module, "int64"), 0),
      type_ctor(qualified(mercury_public_builtin_module, "uint"), 0),
      type_ctor(qualified(mercury_public_builtin_module, "uint8"), 0),
      type_ctor(qualified(mercury_public_builtin_module, "uint16"), 0),
      type_ctor(qualified(mercury_public_builtin_module, "uint32"), 0),
      type_ctor(qualified(mercury_public_builtin_module, "uint64"), 0),
      type_ctor(qualified(mercury_public_builtin_module, "string"), 0),
      type_ctor(qualified(mercury_public_builtin_module, "character"), 0),
      type_ctor(qualified(mercury_public_builtin_module, "float"), 0),
      type_ctor(qualified(mercury_public_builtin_module, "pred"), 0),
      type_ctor(qualified(mercury_public_builtin_module, "func"), 0),
      type_ctor(qualified(mercury_public_builtin_module, "void"), 0),
      type_ctor(qualified(mercury_public_builtin_module, "tuple"), 0)
    ].

%---------------------------------------------------------------------------%

is_type_ctor_a_builtin_dummy(TypeCtor) = IsBuiltinDummy :-
    % Please keep the set of type_ctors for which we return
    % is_builtin_dummy_type_ctor in sync with classify_type_ctor_if_special.
    TypeCtor = type_ctor(CtorSymName, TypeArity),
    (
        CtorSymName = qualified(ModuleName, TypeName),
        ( if
            (
                TypeName = "state",
                TypeArity = 0,
                ModuleName = mercury_io_module
            ;
                TypeName = "store",
                TypeArity = 1,
                ModuleName = mercury_std_lib_module_name(unqualified("store"))
            )
        then
            IsBuiltinDummy = is_builtin_dummy_type_ctor
        else if
            (
                TypeName = "store_at_ref_type",
                TypeArity = 1,
                ModuleName = mercury_private_builtin_module
            ;
                TypeName = "comparison_result",
                TypeArity = 0,
                ModuleName = mercury_public_builtin_module
            )
        then
            IsBuiltinDummy = is_builtin_non_dummy_type_ctor
        else
            IsBuiltinDummy = is_not_builtin_dummy_type_ctor
        )
    ;
        CtorSymName = unqualified(_TypeName),
        IsBuiltinDummy = is_not_builtin_dummy_type_ctor
    ).

is_introduced_type_info_type(Type) :-
    type_to_ctor(Type, TypeCtor),
    is_introduced_type_info_type_ctor(TypeCtor).

is_introduced_type_info_type_ctor(TypeCtor) :-
    TypeCtor = type_ctor(qualified(PrivateBuiltin, Name), 0),
    PrivateBuiltin = mercury_private_builtin_module,
    ( Name = "type_info"
    ; Name = "type_ctor_info"
    ; Name = "typeclass_info"
    ; Name = "base_typeclass_info"
    ).

is_introduced_type_info_type_category(TypeCtorCat) = IsIntroduced :-
    (
        ( TypeCtorCat = ctor_cat_builtin(_)
        ; TypeCtorCat = ctor_cat_higher_order
        ; TypeCtorCat = ctor_cat_tuple
        ; TypeCtorCat = ctor_cat_enum(_)
        ; TypeCtorCat = ctor_cat_builtin_dummy
        ; TypeCtorCat = ctor_cat_variable
        ; TypeCtorCat = ctor_cat_void
        ; TypeCtorCat = ctor_cat_user(_)
        ),
        IsIntroduced = no
    ;
        TypeCtorCat = ctor_cat_system(_),
        IsIntroduced = yes
    ).

%---------------------------------------------------------------------------%

remove_new_prefix(unqualified(Name0), unqualified(Name)) :-
    string.append("new ", Name, Name0).
remove_new_prefix(qualified(Module, Name0), qualified(Module, Name)) :-
    string.append("new ", Name, Name0).

add_new_prefix(unqualified(Name0), unqualified(Name)) :-
    string.append("new ", Name0, Name).
add_new_prefix(qualified(Module, Name0), qualified(Module, Name)) :-
    string.append("new ", Name0, Name).

%---------------------------------------------------------------------------%

cell_cons_id(type_info_cell(Ctor)) = type_info_cell_constructor(Ctor).
cell_cons_id(typeclass_info_cell) = typeclass_info_cell_constructor.

cell_inst_cons_id(Which, Arity) = InstConsId :-
    % Neither of these function symbols exist, even with fake arity,
    % but they do not need to.
    (
        Which = type_info_cell(_),
        Symbol = "type_info"
    ;
        Which = typeclass_info_cell,
        Symbol = "typeclass_info"
    ),
    PrivateBuiltin = mercury_private_builtin_module,
    TypeCtor = cons_id_dummy_type_ctor,
    InstConsId = cons(qualified(PrivateBuiltin, Symbol), Arity, TypeCtor).

%---------------------------------------------------------------------------%

qualify_cons_id(Args, ConsId0, ConsId, InstConsId) :-
    (
        ConsId0 = cons(Name0, OrigArity, TypeCtor),
        ( if TypeCtor = type_ctor(qualified(TypeModule, _), _) then
            UnqualName = unqualify_name(Name0),
            Name = qualified(TypeModule, UnqualName),
            ConsId = cons(Name, OrigArity, TypeCtor)
        else
            ConsId = ConsId0
        ),
        InstConsId = ConsId
    ;
        ConsId0 = type_info_cell_constructor(CellCtor),
        ConsId = ConsId0,
        InstConsId = cell_inst_cons_id(type_info_cell(CellCtor),
            list.length(Args))
    ;
        ConsId0 = typeclass_info_cell_constructor,
        ConsId = ConsId0,
        InstConsId = cell_inst_cons_id(typeclass_info_cell, list.length(Args))
    ;
        ( ConsId0 = tuple_cons(_)
        ; ConsId0 = closure_cons(_, _)
        ; ConsId0 = some_int_const(_)
        ; ConsId0 = float_const(_)
        ; ConsId0 = char_const(_)
        ; ConsId0 = string_const(_)
        ; ConsId0 = impl_defined_const(_)
        ; ConsId0 = type_ctor_info_const(_, _, _)
        ; ConsId0 = base_typeclass_info_const(_, _, _, _)
        ; ConsId0 = type_info_const(_)
        ; ConsId0 = typeclass_info_const(_)
        ; ConsId0 = ground_term_const(_, _)
        ; ConsId0 = table_io_entry_desc(_)
        ; ConsId0 = tabling_info_const(_)
        ; ConsId0 = deep_profiling_proc_layout(_)
        ),
        ConsId = ConsId0,
        InstConsId = ConsId
    ).

%---------------------------------------------------------------------------%

apply_partial_map_to_list(_PartialMap, [], []).
apply_partial_map_to_list(PartialMap, [X | Xs], [Y | Ys]) :-
    ( if map.search(PartialMap, X, Y0) then
        Y = Y0
    else
        Y = X
    ),
    apply_partial_map_to_list(PartialMap, Xs, Ys).

%---------------------------------------------------------------------------%
:- end_module parse_tree.prog_type.
%---------------------------------------------------------------------------%
