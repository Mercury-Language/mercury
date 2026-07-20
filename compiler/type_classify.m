%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2012 The University of Melbourne.
% Copyright (C) 2014-2026 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: type_classify.m.
%
% This file provides operations that classify types into categories.
%
%-----------------------------------------------------------------------------%

:- module hlds.type_classify.
:- interface.

:- import_module hlds.hlds_data.
:- import_module hlds.hlds_module.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_type.

%-----------------------------------------------------------------------------%

    % Given a type, determine what category its principal constructor
    % falls into.
    %
:- func classify_type(module_info, mer_type) = type_ctor_category.

    % Given a type_ctor, determine what sort it is.
    %
:- func classify_type_ctor(module_info, type_ctor) = type_ctor_category.

    % Given a type_ctor, determine what sort it is, *if* it is a special
    % kind of type_ctor. Unlike classify_type_ctor itself, it does not need
    % type representations to have been computed yet.
    %
:- pred classify_type_ctor_if_special(type_ctor::in, type_ctor_category::out)
    is semidet.

    % Given a type_ctor's type_ctor_defn's body, determine what sort it is.
    %
:- func classify_type_defn_body(hlds_type_body) = type_ctor_category.

%-----------------------------------------------------------------------------%

    % Succeed iff type is an "atomic" type - one which can be unified
    % using a simple_test rather than a complicated_unify.
    %
:- pred type_is_atomic(module_info::in, mer_type::in) is semidet.

:- pred type_ctor_is_atomic(module_info::in, type_ctor::in) is semidet.

%-----------------------------------------------------------------------------%

    % Report whether it is OK to include a value of the given time
    % in a heap cell allocated with GC_malloc_atomic.
    %
:- func type_may_use_atomic_alloc(module_info, mer_type) =
    may_use_atomic_alloc.

    % update_type_may_use_atomic_alloc(ModuleInfo, Type, !MaybeUseAtomic):
    %
    % Find out whether it is OK to include a value of the given time
    % in a heap cell allocated with GC_malloc_atomic. If yes, leave
    % !MaybeUseAtomic alone. If no, set !:MaybeUseAtomic to
    % may_not_use_atomic_alloc.
    %
:- pred update_type_may_use_atomic_alloc(module_info::in, mer_type::in,
    may_use_atomic_alloc::in, may_use_atomic_alloc::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.sym_name.

:- import_module bool.
:- import_module list.
:- import_module maybe.
:- import_module one_or_more.
:- import_module require.

%-----------------------------------------------------------------------------%

classify_type(ModuleInfo, Type) = TypeCategory :-
    ( if type_to_ctor(Type, TypeCtor) then
        TypeCategory = classify_type_ctor(ModuleInfo, TypeCtor)
    else
        TypeCategory = ctor_cat_variable
    ).

classify_type_ctor(ModuleInfo, TypeCtor) = TypeCategory :-
    ( if classify_type_ctor_if_special(TypeCtor, TypeCategoryPrime) then
        TypeCategory = TypeCategoryPrime
    else
        module_info_get_type_table(ModuleInfo, TypeTable),
        lookup_type_ctor_defn(TypeTable, TypeCtor, TypeDefn),
        hlds_data.get_type_defn_body(TypeDefn, TypeBody),
        TypeCategory = classify_type_defn_body(TypeBody)
    ).

classify_type_ctor_if_special(TypeCtor, TypeCategory) :-
    % Please keep the code of this predicate in sync with the code of
    % classify_type_defn_body.
    %
    % Please also keep the relevant parts of this code in sync with
    %
    % - builtin_type_to_string
    % - int_type_to_string
    % - type_ctor_is_higher_order
    % - type_ctor_is_tuple
    % - check_builtin_dummy_type_ctor
    %
    TypeCtor = type_ctor(TypeSymName, Arity),
    ( TypeSymName = unqualified(TypeName)
    ; TypeSymName = qualified(_ModuleSymName, TypeName)
    ),
    (
        (
            TypeName = "int",
            TypeCategory = ctor_cat_builtin(cat_builtin_int(int_type_int))
        ;
            TypeName = "uint",
            TypeCategory = ctor_cat_builtin(cat_builtin_int(int_type_uint))
        ;
            TypeName = "int8",
            TypeCategory = ctor_cat_builtin(cat_builtin_int(int_type_int8))
        ;
            TypeName = "uint8",
            TypeCategory = ctor_cat_builtin(cat_builtin_int(int_type_uint8))
        ;
            TypeName = "int16",
            TypeCategory = ctor_cat_builtin(cat_builtin_int(int_type_int16))
        ;
            TypeName = "uint16",
            TypeCategory = ctor_cat_builtin(cat_builtin_int(int_type_uint16))
        ;
            TypeName = "int32",
            TypeCategory = ctor_cat_builtin(cat_builtin_int(int_type_int32))
        ;
            TypeName = "uint32",
            TypeCategory = ctor_cat_builtin(cat_builtin_int(int_type_uint32))
        ;
            TypeName = "int64",
            TypeCategory = ctor_cat_builtin(cat_builtin_int(int_type_int64))
        ;
            TypeName = "uint64",
            TypeCategory = ctor_cat_builtin(cat_builtin_int(int_type_uint64))
        ;
            TypeName = "character",
            TypeCategory = ctor_cat_builtin(cat_builtin_char)
        ;
            TypeName = "float",
            TypeCategory = ctor_cat_builtin(cat_builtin_float)
        ;
            TypeName = "string",
            TypeCategory = ctor_cat_builtin(cat_builtin_string)
        ;
            TypeName = "void",
            TypeCategory = ctor_cat_void
        ),
        (
            TypeSymName = unqualified(_TypeName)
        ;
            TypeSymName = qualified(ModuleSymName, _TypeName),
            ModuleSymName = mercury_public_builtin_module
        ),
        Arity = 0
    ;
        (
            TypeName = "type_info",
            TypeCategory = ctor_cat_system(cat_system_type_info)
        ;
            TypeName = "type_ctor_info",
            TypeCategory = ctor_cat_system(cat_system_type_ctor_info)
        ;
            TypeName = "typeclass_info",
            TypeCategory = ctor_cat_system(cat_system_typeclass_info)
        ;
            TypeName = "base_typeclass_info",
            TypeCategory = ctor_cat_system(cat_system_base_typeclass_info)
        ),
        TypeSymName = qualified(ModuleSymName, _TypeName),
        ModuleSymName = mercury_private_builtin_module,
        Arity = 0
    ;
        (
            TypeName = "state",
            TypeSymName = qualified(ModuleSymName, _TypeName),
            ModuleSymName = mercury_io_module,
            Arity = 0
        ;
            TypeName = "store",
            TypeSymName = qualified(ModuleSymName, _TypeName),
            ModuleSymName = maybe_add_stdlib_wrapper(unqualified("store")),
            Arity = 1
        ),
        TypeCategory = ctor_cat_builtin_dummy
    ;
        (
            TypeName = "pred"
        ;
            TypeName = "func"
        ),
        % The previous version of classify_type_ctor was implemented
        % as a series of nested if-then-elses, with two conditions
        % that could recognize higher order type constructors.
        (
            % This was the first condition.
            TypeSymName = qualified(ModuleSymName, _TypeName),
            ModuleSymName = mercury_public_builtin_module,
            Arity = 0
        ;
            % This was the second condition.
            (
                TypeSymName = unqualified(_TypeName)
            ;
                TypeSymName = qualified(ModuleSymName, _TypeName),
                ModuleSymName = unqualified(Qualifier),
                ( Qualifier = "impure"
                ; Qualifier = "semipure"
                )
            )
            % The arity may be anything.
        ),
        % XXX zs: Having two conditions that look so different seems wrong.
        TypeCategory = ctor_cat_higher_order
    ;
        % XXX The compiler does not recognize any type named tuple/0 in
        % user code, but it nevertheless needs to know about this type,
        % because the compiler itself generates references to it. The
        % type_infos for tuples types (whose type name is "{}", not "tuple")
        % reference the hand-written type_ctor_info for the type named "tuple".
        % Since the name of the type is part of the name of the target language
        % variable holding the type_ctor_info, it helps if it does not contain
        % nonalphanumeric characters.
        TypeName = "tuple",
        TypeSymName = qualified(ModuleSymName, _TypeName),
        ModuleSymName = mercury_public_builtin_module,
        Arity = 0,
        TypeCategory = ctor_cat_tuple
    ;
        TypeName = "{}",
        TypeSymName = unqualified(_TypeName),
        % The arity may be anything.
        TypeCategory = ctor_cat_tuple
    ).

classify_type_defn_body(TypeBody) = TypeCategory :-
    % Unlike classify_type_ctor, we don't have to (a) test for types that do
    % not have definitions, or (b) look up the definition, since our caller has
    % already done that.

    % XXX Why don't we have a category for solver types?
    % XXX Why do we classify abstract_enum_types as general?
    (
        TypeBody = hlds_du_type(TypeBodyDu),
        TypeBodyDu = type_body_du(_, _, _, _, MaybeTypeRepn, _),
        (
            MaybeTypeRepn = no,
            unexpected($pred, "MaybeTypeRepn = no")
        ;
            MaybeTypeRepn = yes(Repn)
        ),
        DuTypeKind = Repn ^ dur_kind,
        (
            DuTypeKind = du_type_kind_mercury_enum,
            TypeCategory = ctor_cat_enum(cat_enum_mercury)
        ;
            DuTypeKind = du_type_kind_foreign_enum(_),
            TypeCategory = ctor_cat_enum(cat_enum_foreign)
        ;
            DuTypeKind = du_type_kind_direct_dummy,
            TypeCategory = ctor_cat_user(cat_user_direct_dummy)
        ;
            DuTypeKind = du_type_kind_notag(_, _, _),
            TypeCategory = ctor_cat_user(cat_user_notag)
        ;
            DuTypeKind = du_type_kind_general,
            TypeCategory = ctor_cat_user(cat_user_general)
        )
    ;
        TypeBody = hlds_abstract_type(AbstractDetails),
        (
            ( AbstractDetails = abstract_type_general
            ; AbstractDetails = abstract_type_fits_in_n_bits(_)
            ; AbstractDetails = abstract_subtype(_)
            ; AbstractDetails = abstract_solver_type
            ),
            TypeCategory = ctor_cat_user(cat_user_general)
        ;
            AbstractDetails = abstract_dummy_type,
            TypeCategory = ctor_cat_user(cat_user_abstract_dummy)
        ;
            AbstractDetails = abstract_notag_type,
            TypeCategory = ctor_cat_user(cat_user_abstract_notag)
        )
    ;
        % XXX We should be able to return more precise descriptions
        % than this.
        ( TypeBody = hlds_eqv_type(_)
        ; TypeBody = hlds_foreign_type(_)
        ; TypeBody = hlds_solver_type(_)
        ),
        TypeCategory = ctor_cat_user(cat_user_general)
    ).

%-----------------------------------------------------------------------------%

type_is_atomic(ModuleInfo, Type) :-
    type_to_ctor(Type, TypeCtor),
    type_ctor_is_atomic(ModuleInfo, TypeCtor).

type_ctor_is_atomic(ModuleInfo, TypeCtor) :-
    TypeCategory = classify_type_ctor(ModuleInfo, TypeCtor),
    type_ctor_category_is_atomic(TypeCategory) = yes.

:- func type_ctor_category_is_atomic(type_ctor_category) = bool.

type_ctor_category_is_atomic(CtorCat) = IsAtomic :-
    (
        ( CtorCat = ctor_cat_builtin(_)
        ; CtorCat = ctor_cat_enum(_)
        ; CtorCat = ctor_cat_void
        ; CtorCat = ctor_cat_builtin_dummy
        ; CtorCat = ctor_cat_user(cat_user_abstract_dummy)
        ; CtorCat = ctor_cat_user(cat_user_direct_dummy)
        ),
        IsAtomic = yes
    ;
        ( CtorCat = ctor_cat_higher_order
        ; CtorCat = ctor_cat_tuple
        ; CtorCat = ctor_cat_variable
        ; CtorCat = ctor_cat_system(_)
        ; CtorCat = ctor_cat_user(cat_user_notag)
        ; CtorCat = ctor_cat_user(cat_user_abstract_notag)
        ; CtorCat = ctor_cat_user(cat_user_general)
        ),
        IsAtomic = no
    ).

%-----------------------------------------------------------------------------%

type_may_use_atomic_alloc(ModuleInfo, Type) = TypeMayUseAtomic :-
    TypeCategory = classify_type(ModuleInfo, Type),
    (
        TypeCategory = ctor_cat_builtin(cat_builtin_int(IntType)),
        (
            ( IntType = int_type_int
            ; IntType = int_type_uint
            ; IntType = int_type_int8
            ; IntType = int_type_uint8
            ; IntType = int_type_int16
            ; IntType = int_type_uint16
            ; IntType = int_type_int32
            ; IntType = int_type_uint32
            ),
            TypeMayUseAtomic = may_use_atomic_alloc
        ;
            ( IntType = int_type_int64
            ; IntType = int_type_uint64
            ),
            module_info_get_globals(ModuleInfo, Globals),
            globals.lookup_bool_option(Globals, unboxed_int64s, UBI64),
            (
                UBI64 = yes,
                TypeMayUseAtomic = may_use_atomic_alloc
            ;
                UBI64 = no,
                TypeMayUseAtomic = may_not_use_atomic_alloc
            )
        )
    ;
        ( TypeCategory = ctor_cat_builtin(cat_builtin_char)
        ; TypeCategory = ctor_cat_enum(_)
        ; TypeCategory = ctor_cat_builtin_dummy
        ; TypeCategory = ctor_cat_system(cat_system_type_ctor_info)
        ),
        TypeMayUseAtomic = may_use_atomic_alloc
    ;
        TypeCategory = ctor_cat_builtin(cat_builtin_float),
        module_info_get_globals(ModuleInfo, Globals),
        globals.lookup_bool_option(Globals, unboxed_float, UBF),
        (
            UBF = yes,
            TypeMayUseAtomic = may_use_atomic_alloc
        ;
            UBF = no,
            TypeMayUseAtomic = may_not_use_atomic_alloc
        )
    ;
        ( TypeCategory = ctor_cat_builtin(cat_builtin_string)
        ; TypeCategory = ctor_cat_higher_order
        ; TypeCategory = ctor_cat_tuple
        ; TypeCategory = ctor_cat_variable
        ; TypeCategory = ctor_cat_system(cat_system_type_info)
        ; TypeCategory = ctor_cat_system(cat_system_typeclass_info)
        ; TypeCategory = ctor_cat_system(cat_system_base_typeclass_info)
        ; TypeCategory = ctor_cat_void
        ; TypeCategory = ctor_cat_user(_) % for direct_dummy, alloc is moot
        ),
        TypeMayUseAtomic = may_not_use_atomic_alloc
    ).

update_type_may_use_atomic_alloc(ModuleInfo, Type, !MayUseAtomic) :-
    (
        !.MayUseAtomic = may_not_use_atomic_alloc
        % There is no point in testing Type.
    ;
        !.MayUseAtomic = may_use_atomic_alloc,
        !:MayUseAtomic = type_may_use_atomic_alloc(ModuleInfo, Type)
    ).

%-----------------------------------------------------------------------------%
:- end_module hlds.type_classify.
%-----------------------------------------------------------------------------%
