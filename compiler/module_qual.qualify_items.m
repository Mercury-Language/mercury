%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This module implements the second pass of module_qual.m;
% it module qualifies everything that needs to be module qualified
% in the item blocks given to it. If something cannot be module qualified,
% either because it refers to an undefined entity (type, inst etc) or
% to an entity that has more than one matching definition, it calls
% module_qual.qual_error.m to generate an error message.
%

:- module parse_tree.module_qual.qualify_items.
:- interface.

:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_item.

:- import_module assoc_list.
:- import_module list.

    % Module qualify the src item blocks of an augmented compilation unit.
    %
:- pred module_qualify_items_in_src_item_blocks(
    list(src_item_block)::in, list(src_item_block)::out,
    mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

    % Module qualify the event specifications of an augmented compilation unit.
    %
:- pred qualify_event_specs(mq_in_interface::in, string::in,
    assoc_list(string, event_spec)::in, assoc_list(string, event_spec)::out,
    mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

    % Module qualify the items in an interface file.
    % (See the XXX near the calls to this predicate.)
    %
    % NOTE: Please update Mercury.options if this predicate is moved to another
    % module. It must be compiled with --optimize-constructor-last-call.
    %
:- pred module_qualify_items_loop(mq_in_interface::in,
    list(item)::in, list(item)::out, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

%---------------------%

    % Qualify a type and its argument types.
    %
:- pred qualify_type(mq_in_interface::in, mq_error_context::in,
    mer_type::in, mer_type::out, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred qualify_mode_list(mq_in_interface::in, mq_error_context::in,
    list(mer_mode)::in, list(mer_mode)::out, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred qualify_mode(mq_in_interface::in, mq_error_context::in,
    mer_mode::in, mer_mode::out, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.module_qual.id_set.
:- import_module parse_tree.module_qual.qual_errors.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_data_pragma.

:- import_module int.
:- import_module pair.
:- import_module require.

module_qualify_items_in_src_item_blocks([], [], !Info, !Specs).
module_qualify_items_in_src_item_blocks([SrcItemBlock0 | SrcItemBlocks0],
        [SrcItemBlock | SrcItemBlocks], !Info, !Specs) :-
    SrcItemBlock0 = item_block(ModuleName, SrcSection, Context,
        Incls, Avails, Items0),
    (
        SrcSection = sms_interface,
        InInt = mq_used_in_interface
    ;
        ( SrcSection = sms_implementation
        ; SrcSection = sms_impl_but_exported_to_submodules
        ),
        InInt = mq_not_used_in_interface
    ),
    (
        Incls = []
    ;
        Incls = [_ | _],
        % The submodule might make use of *any* of the imported modules.
        % There is no way for us to tell which ones. So we conservatively
        % assume that it uses *all* of them, *unless* we were given the option
        % that tells us that we should not make that assumption.
        mq_info_get_should_warn_unused_imports_in_parents(!.Info,
            ShouldWarnUnusedImportsInParents),
        (
            ShouldWarnUnusedImportsInParents =
                should_not_warn_unused_imports_in_parents,
            map.init(UnusedModules),
            mq_info_set_as_yet_unused_interface_modules(UnusedModules, !Info)
        ;
            ShouldWarnUnusedImportsInParents =
                should_warn_unused_imports_in_parents
        )
    ),
    module_qualify_items_loop(InInt, Items0, Items, !Info, !Specs),
    SrcItemBlock = item_block(ModuleName, SrcSection, Context,
        Incls, Avails, Items),
    module_qualify_items_in_src_item_blocks(SrcItemBlocks0, SrcItemBlocks,
        !Info, !Specs).

module_qualify_items_loop(_InInt, [], [], !Info, !Specs).
module_qualify_items_loop(InInt, [HeadItem0 | TailItems0], Items,
        !Info, !Specs) :-
    module_qualify_item(InInt, HeadItem0, HeadItem, !Info, !Specs),
    module_qualify_items_loop(InInt, TailItems0, TailItems, !Info, !Specs),
    Items = [HeadItem | TailItems]. % lcmc

    % Module qualify a single item.
    %
:- pred module_qualify_item(mq_in_interface::in, item::in, item::out,
    mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

module_qualify_item(InInt, Item0, Item, !Info, !Specs) :-
    (
        ( Item0 = item_clause(_)
        ; Item0 = item_initialise(_)
        ; Item0 = item_finalise(_)
        ; Item0 = item_promise(_)
        ; Item0 = item_foreign_import_module(_)
        ),
        Item = Item0
    ;
        Item0 = item_type_defn(ItemTypeDefn0),
        ItemTypeDefn0 = item_type_defn_info(SymName, Params, TypeDefn0,
            TVarSet, Context, SeqNum),
        list.length(Params, Arity),
        TypeCtor = type_ctor(SymName, Arity),
        qualify_type_defn(InInt, Context, TypeCtor, TypeDefn0, TypeDefn,
            !Info, !Specs),
        ItemTypeDefn = item_type_defn_info(SymName, Params, TypeDefn,
            TVarSet, Context, SeqNum),
        Item = item_type_defn(ItemTypeDefn)
    ;
        Item0 = item_inst_defn(ItemInstDefn0),
        ItemInstDefn0 = item_inst_defn_info(SymName, Params, MaybeForTypeCtor0,
            MaybeAbstractInstDefn0, InstVarSet, Context, SeqNum),
        list.length(Params, Arity),
        ErrorContext = mqec_inst(Context, mq_id(SymName, Arity)),
        (
            MaybeAbstractInstDefn0 = abstract_inst_defn,
            MaybeAbstractInstDefn = abstract_inst_defn
        ;
            MaybeAbstractInstDefn0 = nonabstract_inst_defn(InstDefn0),
            InstDefn0 = eqv_inst(Inst0),
            qualify_inst(InInt, ErrorContext, Inst0, Inst, !Info, !Specs),
            InstDefn = eqv_inst(Inst),
            MaybeAbstractInstDefn = nonabstract_inst_defn(InstDefn)
        ),
        (
            MaybeForTypeCtor0 = yes(ForTypeCtor0),
            qualify_type_ctor(InInt, ErrorContext, ForTypeCtor0, ForTypeCtor,
                !Info, !Specs),
            MaybeForTypeCtor = yes(ForTypeCtor)
        ;
            MaybeForTypeCtor0 = no,
            MaybeForTypeCtor = no
        ),
        ItemInstDefn = item_inst_defn_info(SymName, Params, MaybeForTypeCtor,
            MaybeAbstractInstDefn, InstVarSet, Context, SeqNum),
        Item = item_inst_defn(ItemInstDefn)
    ;
        Item0 = item_mode_defn(ItemModeDefn0),
        ItemModeDefn0 = item_mode_defn_info(SymName, Params,
            MaybeAbstractModeDefn0, InstVarSet, Context, SeqNum),
        (
            MaybeAbstractModeDefn0 = abstract_mode_defn,
            MaybeAbstractModeDefn = abstract_mode_defn
        ;
            MaybeAbstractModeDefn0 = nonabstract_mode_defn(ModeDefn0),
            list.length(Params, Arity),
            ErrorContext = mqec_mode(Context, mq_id(SymName, Arity)),
            ModeDefn0 = eqv_mode(Mode0),
            qualify_mode(InInt, ErrorContext, Mode0, Mode, !Info, !Specs),
            ModeDefn = eqv_mode(Mode),
            MaybeAbstractModeDefn = nonabstract_mode_defn(ModeDefn)
        ),
        ItemModeDefn = item_mode_defn_info(SymName, Params,
            MaybeAbstractModeDefn, InstVarSet, Context, SeqNum),
        Item = item_mode_defn(ItemModeDefn)
    ;
        Item0 = item_pred_decl(ItemPredDecl0),
        ItemPredDecl0 = item_pred_decl_info(SymName, PredOrFunc,
            TypesAndModes0, MaybeWithType0, MaybeWithInst0, MaybeDetism,
            Origin, TypeVarSet, InstVarSet, ExistQVars, Purity,
            Constraints0, Context, SeqNum),
        list.length(TypesAndModes0, Arity),
        ErrorContext = mqec_pred_or_func(Context, PredOrFunc,
            mq_id(SymName, Arity)),
        qualify_types_and_modes(InInt, ErrorContext,
            TypesAndModes0, TypesAndModes, !Info, !Specs),
        ConstraintErrorContext = mqcec_pred_decl(Context, PredOrFunc,
            SymName, Arity),
        qualify_prog_constraints(InInt, ConstraintErrorContext,
            Constraints0, Constraints, !Info, !Specs),
        (
            MaybeWithType0 = yes(WithType0),
            % XXX We could pass a more specific error context.
            qualify_type(InInt, ErrorContext, WithType0, WithType,
                !Info, !Specs),
            MaybeWithType = yes(WithType)
        ;
            MaybeWithType0 = no,
            MaybeWithType = no
        ),
        (
            MaybeWithInst0 = yes(WithInst0),
            % XXX We could pass a more specific error context.
            qualify_inst(InInt, ErrorContext, WithInst0, WithInst,
                !Info, !Specs),
            MaybeWithInst = yes(WithInst)
        ;
            MaybeWithInst0 = no,
            MaybeWithInst = no
        ),
        ItemPredDecl = item_pred_decl_info(SymName, PredOrFunc,
            TypesAndModes, MaybeWithType, MaybeWithInst, MaybeDetism,
            Origin, TypeVarSet, InstVarSet, ExistQVars, Purity,
            Constraints, Context, SeqNum),
        Item = item_pred_decl(ItemPredDecl)
    ;
        Item0 = item_mode_decl(ItemModeDecl0),
        ItemModeDecl0 = item_mode_decl_info(SymName, PredOrFunc, Modes0,
            MaybeWithInst0, MaybeDetism, InstVarSet, Context, SeqNum),
        list.length(Modes0, Arity),
        ErrorContext = mqec_pred_or_func_mode(Context, PredOrFunc,
            mq_id(SymName, Arity)),
        qualify_mode_list(InInt, ErrorContext, Modes0, Modes, !Info, !Specs),
        (
            MaybeWithInst0 = yes(WithInst0),
            % XXX We could pass a more specific error context.
            qualify_inst(InInt, ErrorContext, WithInst0, WithInst,
                !Info, !Specs),
            MaybeWithInst = yes(WithInst)
        ;
            MaybeWithInst0 = no,
            MaybeWithInst = no
        ),
        ItemModeDecl = item_mode_decl_info(SymName, PredOrFunc, Modes,
            MaybeWithInst, MaybeDetism, InstVarSet, Context, SeqNum),
        Item = item_mode_decl(ItemModeDecl)
    ;
        Item0 = item_typeclass(ItemTypeClass0),
        ItemTypeClass0 = item_typeclass_info(Name, Vars, Constraints0, FunDeps,
            Interface0, VarSet, Context, SeqNum),
        list.length(Vars, Arity),
        ConstraintErrorContext = mqcec_class_defn(Context, Name, Arity),
        qualify_prog_constraint_list(InInt, ConstraintErrorContext,
            Constraints0, Constraints, !Info, !Specs),
        (
            Interface0 = class_interface_abstract,
            Interface = class_interface_abstract
        ;
            Interface0 = class_interface_concrete(Methods0),
            ErrorContext = mqec_class(Context, mq_id(Name, Arity)),
            qualify_class_decls(InInt, ErrorContext, Methods0, Methods,
                !Info, !Specs),
            Interface = class_interface_concrete(Methods)
        ),
        ItemTypeClass = item_typeclass_info(Name, Vars, Constraints, FunDeps,
            Interface, VarSet, Context, SeqNum),
        Item = item_typeclass(ItemTypeClass)
    ;
        Item0 = item_instance(ItemInstance0),
        ItemInstance0 = item_instance_info(Name0, Types0, OriginalTypes0,
            Constraints0, Body0, VarSet, ModName, Context, SeqNum),
        list.length(Types0, Arity),
        Id0 = mq_id(Name0, Arity),
        ErrorContext = mqec_instance(Context, Id0),

        (
            InInt = mq_used_in_interface,
            mq_info_set_exported_instances_flag(yes, !Info)
        ;
            InInt = mq_not_used_in_interface
        ),

        % We don't qualify the implementation yet, since that requires
        % us to resolve overloading.
        ConstraintErrorContext = mqcec_instance_defn(Context, Name0,
            OriginalTypes0),
        qualify_prog_constraint_list(InInt, ConstraintErrorContext,
            Constraints0, Constraints, !Info, !Specs),
        qualify_class_name(InInt, ErrorContext, Id0, Name, !Info, !Specs),
        % XXX We don't want to keep the errors from the expansion of both
        % forms of the instance types, since printing two error messages about
        % one instance definition that make apparently contradictory
        % assumptions about whether the instance types are equiv-type-expanded
        % or not would be confusing. However, I (zs) cannot think of any
        % compelling reason right now for preferring the error messages
        % from one version of the types over the other.
        qualify_type_list(InInt, ErrorContext, Types0, Types,
            !Info, !Specs),
        qualify_type_list(InInt, ErrorContext, OriginalTypes0, OriginalTypes,
            !Info, !.Specs, _),
        qualify_instance_body(Name, Body0, Body),
        ItemInstance = item_instance_info(Name, Types, OriginalTypes,
            Constraints, Body, VarSet, ModName, Context, SeqNum),
        Item = item_instance(ItemInstance)
    ;
        Item0 = item_mutable(ItemMutable0),
        qualify_mutable(InInt, ItemMutable0, ItemMutable, !Info, !Specs),
        Item = item_mutable(ItemMutable)
    ;
        Item0 = item_type_repn(ItemTypeRepnInfo0),
        ItemTypeRepnInfo0 = item_type_repn_info(TypeCtorSymName, ArgTVars,
            RepInfo0, TVarSet, Context, SeqNum),
        (
            ( RepInfo0 = tcrepn_is_direct_dummy
            ; RepInfo0 = tcrepn_is_notag
            ; RepInfo0 = tcrepn_fits_in_n_bits(_, _)
            ; RepInfo0 = tcrepn_has_direct_arg_functors(_)
            ; RepInfo0 = tcrepn_maybe_foreign(_, _)
            ; RepInfo0 = tcrepn_du(_)
            ),
            RepInfo = RepInfo0
        ;
            RepInfo0 = tcrepn_is_eqv_to(EqvType0),
            list.length(ArgTVars, TypeCtorArity),
            TypeCtor = type_ctor(TypeCtorSymName, TypeCtorArity),
            ErrorContext = mqec_type_repn(Context, TypeCtor),
            qualify_type(InInt, ErrorContext, EqvType0, EqvType,
                !Info, !Specs),
            RepInfo = tcrepn_is_eqv_to(EqvType)
        ;
            RepInfo0 = tcrepn_is_word_aligned_ptr(WAP0),
            (
                WAP0 = wap_foreign_type_assertion,
                WAP = WAP0
            ;
                WAP0 = wap_mercury_type(WAPTypeSNA0),
                list.length(ArgTVars, TypeCtorArity),
                TypeCtor = type_ctor(TypeCtorSymName, TypeCtorArity),
                ErrorContext = mqec_type_repn(Context, TypeCtor),
                WAPTypeSNA0 = sym_name_arity(SymName0, Arity),
                TypeCtorId0 = mq_id(SymName0, Arity),
                mq_info_get_types(!.Info, Types),
                find_unique_match(InInt, ErrorContext, Types, type_id,
                    TypeCtorId0, SymName, !Info, !Specs),
                WAPTypeSNA = sym_name_arity(SymName, Arity),
                WAP = wap_mercury_type(WAPTypeSNA)
            ),
            RepInfo = tcrepn_is_word_aligned_ptr(WAP)
        ),
        ItemTypeRepnInfo = item_type_repn_info(TypeCtorSymName, ArgTVars,
            RepInfo, TVarSet, Context, SeqNum),
        Item = item_type_repn(ItemTypeRepnInfo)
    ;
        Item0 = item_pragma(ItemPragma0),
        ItemPragma0 = item_pragma_info(Pragma0, Origin, Context, SeqNum),
        qualify_pragma(InInt, Context, Pragma0, Pragma, !Info, !Specs),
        ItemPragma = item_pragma_info(Pragma, Origin, Context, SeqNum),
        Item = item_pragma(ItemPragma)
    ).

%---------------------------------------------------------------------------%
%
% Module qualify type definitions and types.
%

    % Qualify the constructors or other types in a type definition.
    %
:- pred qualify_type_defn(mq_in_interface::in,
    prog_context::in, type_ctor::in, type_defn::in, type_defn::out,
    mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_type_defn(InInt, Context, TypeCtor, TypeDefn0, TypeDefn,
        !Info, !Specs) :-
    (
        TypeDefn0 = parse_tree_du_type(DetailsDu0),
        DetailsDu0 = type_details_du(Ctors0, MaybeUserEqComp0,
            MaybeDirectArgCtors0),
        qualify_constructors(InInt, TypeCtor, Ctors0, Ctors, !Info, !Specs),
        % User-defined equality pred names will be converted into predicate
        % calls and then module-qualified after type analysis (during mode
        % analysis). That way, they get full type overloading resolution, etc.
        % Thus we don't module-qualify them here.
        MaybeUserEqComp = MaybeUserEqComp0,
        MaybeDirectArgCtors = MaybeDirectArgCtors0,
        DetailsDu = type_details_du(Ctors, MaybeUserEqComp,
            MaybeDirectArgCtors),
        TypeDefn = parse_tree_du_type(DetailsDu)
    ;
        TypeDefn0 = parse_tree_eqv_type(type_details_eqv(Type0)),
        ErrorContext = mqec_type_defn(Context, TypeCtor),
        qualify_type(InInt, ErrorContext, Type0, Type, !Info, !Specs),
        TypeDefn = parse_tree_eqv_type(type_details_eqv(Type))
    ;
        TypeDefn0 = parse_tree_abstract_type(_),
        TypeDefn = TypeDefn0
    ;
        TypeDefn0 = parse_tree_foreign_type(_),
        TypeDefn = TypeDefn0
    ;
        TypeDefn0 = parse_tree_solver_type(DetailsSolver0),
        DetailsSolver0 = type_details_solver(SolverTypeDetails0,
            MaybeUserEqComp),
        SolverTypeDetails0 = solver_type_details(RepnType0,
            GroundInst0, AnyInst0, MutableItems0),
        ErrorContext = mqec_type_defn(Context, TypeCtor),
        qualify_type(InInt, ErrorContext, RepnType0, RepnType,
            !Info, !Specs),
        qualify_inst(InInt, ErrorContext, GroundInst0, GroundInst,
            !Info, !Specs),
        qualify_inst(InInt, ErrorContext, AnyInst0, AnyInst,
            !Info, !Specs),
        qualify_constraint_stores(InInt, MutableItems0, MutableItems,
            !Info, !Specs),
        SolverTypeDetails  = solver_type_details(RepnType,
            GroundInst, AnyInst, MutableItems),
        DetailsSolver = type_details_solver(SolverTypeDetails,
            MaybeUserEqComp),
        TypeDefn = parse_tree_solver_type(DetailsSolver)
    ).

:- pred qualify_constraint_stores(mq_in_interface::in,
    list(item_mutable_info)::in, list(item_mutable_info)::out,
    mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_constraint_stores(_InInt, [], [], !Info, !Specs).
qualify_constraint_stores(InInt,
        [Mutable0 | Mutables0], [Mutable | Mutables], !Info, !Specs) :-
    qualify_mutable(InInt, Mutable0, Mutable, !Info, !Specs),
    qualify_constraint_stores(InInt, Mutables0, Mutables, !Info, !Specs).

:- pred qualify_constructors(mq_in_interface::in, type_ctor::in,
    list(constructor)::in, list(constructor)::out,
    mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_constructors(_InInt, _ContainingTypeCtor, [], [], !Info, !Specs).
qualify_constructors(InInt, ContainingTypeCtor,
        [Ctor0 | Ctors0], [Ctor | Ctors], !Info, !Specs) :-
    qualify_constructor(InInt, ContainingTypeCtor, Ctor0, Ctor,
        !Info, !Specs),
    qualify_constructors(InInt, ContainingTypeCtor, Ctors0, Ctors,
        !Info, !Specs).

:- pred qualify_constructor(mq_in_interface::in, type_ctor::in,
    constructor::in, constructor::out,
    mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_constructor(InInt, ContainingTypeCtor, Ctor0, Ctor, !Info, !Specs) :-
    Ctor0 = ctor(Ordinal, MaybeExistConstraints0, FunctionSymbolSymName, Args0,
        Arity, Context),
    FunctionSymbolName = unqualify_name(FunctionSymbolSymName),
    (
        MaybeExistConstraints0 = no_exist_constraints,
        MaybeExistConstraints = no_exist_constraints
    ;
        MaybeExistConstraints0 = exist_constraints(ExistConstraints0),
        ExistConstraints0 = cons_exist_constraints(ExistQVars, Constraints0,
            UnconstrainedExistQVars, ConstrainedExistQVars),
        ConstraintErrorContext = mqcec_type_defn_constructor(Context,
            ContainingTypeCtor, FunctionSymbolName),
        qualify_prog_constraint_list(InInt, ConstraintErrorContext,
            Constraints0, Constraints, !Info, !Specs),
        ExistConstraints = cons_exist_constraints(ExistQVars, Constraints,
            UnconstrainedExistQVars, ConstrainedExistQVars),
        MaybeExistConstraints = exist_constraints(ExistConstraints)
    ),
    qualify_constructor_args(InInt, ContainingTypeCtor, FunctionSymbolName,
        0, Args0, Args, !Info, !Specs),
    Ctor = ctor(Ordinal, MaybeExistConstraints, FunctionSymbolSymName, Args,
        Arity, Context).

:- pred qualify_constructor_args(mq_in_interface::in,
    type_ctor::in, string::in, int::in,
    list(constructor_arg)::in, list(constructor_arg)::out,
    mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_constructor_args(_InInt, _, _, _, [], [], !Info, !Specs).
qualify_constructor_args(InInt, ContainingTypeCtor, FunctionSymbol,
        LastArgNum, [Arg0 | Args0], [Arg | Args], !Info, !Specs) :-
    CurArgNum = LastArgNum + 1,
    qualify_constructor_arg(InInt, ContainingTypeCtor, FunctionSymbol,
        CurArgNum, Arg0, Arg, !Info, !Specs),
    qualify_constructor_args(InInt, ContainingTypeCtor, FunctionSymbol,
        CurArgNum, Args0, Args, !Info, !Specs).

:- pred qualify_constructor_arg(mq_in_interface::in,
    type_ctor::in, string::in, int::in,
    constructor_arg::in, constructor_arg::out, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_constructor_arg(InInt, ContainingTypeCtor, FunctionSymbol, ArgNum,
        Arg0, Arg, !Info, !Specs) :-
    Arg0 = ctor_arg(MaybeFieldName, Type0, Context),
    ErrorContext = mqec_constructor_arg(Context, ContainingTypeCtor,
        FunctionSymbol, ArgNum, MaybeFieldName),
    qualify_type(InInt, ErrorContext, Type0, Type, !Info, !Specs),
    Arg = ctor_arg(MaybeFieldName, Type, Context).

:- pred qualify_type_list(mq_in_interface::in, mq_error_context::in,
    list(mer_type)::in, list(mer_type)::out, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_type_list(_InInt, _ErrorContext, [], [], !Info, !Specs).
qualify_type_list(InInt, ErrorContext, [Type0 | Types0], [Type | Types],
        !Info, !Specs) :-
    qualify_type(InInt, ErrorContext, Type0, Type, !Info, !Specs),
    qualify_type_list(InInt, ErrorContext, Types0, Types, !Info, !Specs).

qualify_type(InInt, ErrorContext, Type0, Type, !Info, !Specs) :-
    (
        Type0 = type_variable(_Var, _Kind),
        Type = Type0
    ;
        Type0 = defined_type(SymName0, Args0, Kind),
        Arity = list.length(Args0),
        TypeCtorId0 = mq_id(SymName0, Arity),
        mq_info_get_types(!.Info, Types),
        find_unique_match(InInt, ErrorContext, Types, type_id,
            TypeCtorId0, SymName, !Info, !Specs),
        % XXX We could pass a more specific error context.
        qualify_type_list(InInt, ErrorContext, Args0, Args, !Info, !Specs),
        Type = defined_type(SymName, Args, Kind)
    ;
        Type0 = builtin_type(BuiltinType),
        % The types `int', `float', and `string' are builtin types,
        % defined by the compiler, but arguably they ought to be defined
        % in int.m, float.m, and string.m, and so if someone uses the type
        % `int' in the interface, then we don't want to warn about
        % `import_module int' in the interface. We don't do the same for
        % `character', since the corresponding library module `char'
        % will be flagged as used in the interface if the type `char' is used.
        (
            BuiltinType = builtin_type_int(int_type_int),
            mq_info_set_module_used(InInt, unqualified("int"), !Info)
        ;
            BuiltinType = builtin_type_int(int_type_uint),
            mq_info_set_module_used(InInt, unqualified("uint"), !Info)
        ;
            BuiltinType = builtin_type_int(int_type_int8),
            mq_info_set_module_used(InInt, unqualified("int8"), !Info)
        ;
            BuiltinType = builtin_type_int(int_type_uint8),
            mq_info_set_module_used(InInt, unqualified("uint8"), !Info)
        ;
            BuiltinType = builtin_type_int(int_type_int16),
            mq_info_set_module_used(InInt, unqualified("int16"), !Info)
        ;
            BuiltinType = builtin_type_int(int_type_uint16),
            mq_info_set_module_used(InInt, unqualified("uint16"), !Info)
        ;
            BuiltinType = builtin_type_int(int_type_int32),
            mq_info_set_module_used(InInt, unqualified("int32"), !Info)
        ;
            BuiltinType = builtin_type_int(int_type_uint32),
            mq_info_set_module_used(InInt, unqualified("uint32"), !Info)
        ;
            BuiltinType = builtin_type_int(int_type_int64),
            mq_info_set_module_used(InInt, unqualified("int64"), !Info)
        ;
            BuiltinType = builtin_type_int(int_type_uint64),
            mq_info_set_module_used(InInt, unqualified("uint64"), !Info)
        ;
            BuiltinType = builtin_type_float,
            mq_info_set_module_used(InInt, unqualified("float"), !Info)
        ;
            BuiltinType = builtin_type_string,
            mq_info_set_module_used(InInt, unqualified("string"), !Info)
        ;
            BuiltinType = builtin_type_char
        ),
        Type = Type0
    ;
        Type0 = higher_order_type(PorF, Args0, HOInstInfo0, Purity,
            EvalMethod),
        % XXX We could pass a more specific error context.
        qualify_type_list(InInt, ErrorContext, Args0, Args, !Info, !Specs),
        % XXX We could pass a more specific error context.
        qualify_ho_inst_info(InInt, ErrorContext, HOInstInfo0, HOInstInfo,
            !Info, !Specs),
        Type = higher_order_type(PorF, Args, HOInstInfo, Purity, EvalMethod)
    ;
        Type0 = tuple_type(Args0, Kind),
        % XXX We could pass a more specific error context.
        qualify_type_list(InInt, ErrorContext, Args0, Args, !Info, !Specs),
        Type = tuple_type(Args, Kind)
    ;
        Type0 = apply_n_type(Var, Args0, Kind),
        % XXX We could pass a more specific error context.
        qualify_type_list(InInt, ErrorContext, Args0, Args, !Info, !Specs),
        Type = apply_n_type(Var, Args, Kind)
    ;
        Type0 = kinded_type(SubType0, Kind),
        % XXX We could pass a more specific error context.
        qualify_type(InInt, ErrorContext, SubType0, SubType, !Info, !Specs),
        Type = kinded_type(SubType, Kind)
    ).

:- pred qualify_type_ctor(mq_in_interface::in, mq_error_context::in,
    type_ctor::in, type_ctor::out, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_type_ctor(InInt, ErrorContext, TypeCtor0, TypeCtor,
        !Info, !Specs) :-
    TypeCtor0 = type_ctor(SymName0, Arity),
    ( if is_builtin_atomic_type(TypeCtor0) then
        TypeCtor = TypeCtor0
    else
        TypeCtorId0 = mq_id(SymName0, Arity),
        mq_info_get_types(!.Info, Types),
        % XXX We could pass a more specific error context.
        find_unique_match(InInt, ErrorContext, Types, type_id,
            TypeCtorId0, SymName, !Info, !Specs),
        TypeCtor = type_ctor(SymName, Arity)
    ).

    % is_builtin_atomic_type(TypeCtor):
    %
    % Succeeds iff 'TypeCtor' is the type_ctor of a builtin atomic type.
    %
:- pred is_builtin_atomic_type(type_ctor::in) is semidet.

is_builtin_atomic_type(TypeCtor) :-
    TypeCtor = type_ctor(SymName, 0),
    is_builtin_type_sym_name(SymName).

%---------------------------------------------------------------------------%
%
% Module qualify insts.
%

    % Qualify a single inst.
    %
:- pred qualify_inst(mq_in_interface::in, mq_error_context::in,
    mer_inst::in, mer_inst::out, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_inst(InInt, ErrorContext, Inst0, Inst, !Info, !Specs) :-
    (
        Inst0 = any(Uniq, HOInstInfo0),
        qualify_ho_inst_info(InInt, ErrorContext, HOInstInfo0, HOInstInfo,
            !Info, !Specs),
        Inst = any(Uniq, HOInstInfo)
    ;
        ( Inst0 = free
        ; Inst0 = not_reached
        ; Inst0 = inst_var(_)
        ),
        Inst = Inst0
    ;
        Inst0 = free(_),
        unexpected($pred, "compiler generated inst not expected")
    ;
        Inst0 = bound(Uniq, InstResults0, BoundInsts0),
        (
            ( InstResults0 = inst_test_results_fgtc
            ; InstResults0 = inst_test_no_results
            )
        ;
            InstResults0 = inst_test_results(_, _, _, _, _, _),
            unexpected($pred, "compiler generated inst not expected")
        ),
        % XXX We could pass a more specific error context.
        qualify_bound_insts(InInt, ErrorContext, BoundInsts0, BoundInsts,
            !Info, !Specs),
        Inst = bound(Uniq, InstResults0, BoundInsts)
    ;
        Inst0 = ground(Uniq, HOInstInfo0),
        % XXX We could pass a more specific error context.
        qualify_ho_inst_info(InInt, ErrorContext, HOInstInfo0, HOInstInfo,
            !Info, !Specs),
        Inst = ground(Uniq, HOInstInfo)
    ;
        Inst0 = constrained_inst_vars(Vars, SubInst0),
        % XXX We could pass a more specific error context.
        qualify_inst(InInt, ErrorContext, SubInst0, SubInst, !Info, !Specs),
        Inst = constrained_inst_vars(Vars, SubInst)
    ;
        Inst0 = defined_inst(InstName0),
        % XXX We could pass a more specific error context.
        qualify_inst_name(InInt, ErrorContext, InstName0, InstName,
            !Info, !Specs),
        Inst = defined_inst(InstName)
    ;
        Inst0 = abstract_inst(Name, Args0),
        % XXX We could pass a more specific error context.
        qualify_inst_list(InInt, ErrorContext, Args0, Args, !Info, !Specs),
        Inst = abstract_inst(Name, Args)
    ).

:- pred qualify_inst_list(mq_in_interface::in, mq_error_context::in,
    list(mer_inst)::in, list(mer_inst)::out, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_inst_list(_InInt, _ErrorContext, [], [], !Info, !Specs).
qualify_inst_list(InInt, ErrorContext, [Inst0 | Insts0], [Inst | Insts],
        !Info, !Specs) :-
    qualify_inst(InInt, ErrorContext, Inst0, Inst, !Info, !Specs),
    qualify_inst_list(InInt, ErrorContext, Insts0, Insts, !Info, !Specs).

:- pred qualify_ho_inst_info(mq_in_interface::in, mq_error_context::in,
    ho_inst_info::in, ho_inst_info::out, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_ho_inst_info(InInt, ErrorContext, HOInstInfo0, HOInstInfo,
        !Info, !Specs) :-
    (
        HOInstInfo0 = higher_order(pred_inst_info(PredOrFunc, Modes0,
            MaybeArgRegs, Detism)),
        qualify_mode_list(InInt, ErrorContext, Modes0, Modes, !Info, !Specs),
        HOInstInfo = higher_order(pred_inst_info(PredOrFunc, Modes,
            MaybeArgRegs, Detism))
    ;
        HOInstInfo0 = none_or_default_func,
        HOInstInfo = none_or_default_func
    ).

    % Find the unique inst_id that matches this inst, and qualify
    % the argument insts.
    %
:- pred qualify_inst_name(mq_in_interface::in, mq_error_context::in,
    inst_name::in, inst_name::out, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_inst_name(InInt, ErrorContext, InstName0, InstName,
        !Info, !Specs) :-
    (
        InstName0 = user_inst(SymName0, Insts0),
        qualify_inst_list(InInt, ErrorContext, Insts0, Insts,
            !Info, !Specs),
        ( if
            % Check for a variable inst constructor.
            SymName0 = unqualified("")
        then
            report_invalid_user_inst(SymName0, Insts, ErrorContext, !Specs),
            mq_info_set_found_undef_inst(!Info),
            SymName = SymName0
        else
            list.length(Insts0, Arity),
            mq_info_get_insts(!.Info, InstIdSet),
            find_unique_match(InInt, ErrorContext, InstIdSet, inst_id,
                mq_id(SymName0, Arity), SymName, !Info, !Specs)
        ),
        InstName = user_inst(SymName, Insts)
    ;
        ( InstName0 = unify_inst(_, _, _, _)
        ; InstName0 = merge_inst(_, _)
        ; InstName0 = ground_inst(_, _, _, _)
        ; InstName0 = any_inst(_, _, _, _)
        ; InstName0 = shared_inst(_)
        ; InstName0 = mostly_uniq_inst(_)
        ; InstName0 = typed_ground(_, _)
        ; InstName0 = typed_inst(_, _)
        ),
        unexpected($pred, "unexpected compiler generated inst_name")
    ).

:- pred qualify_bound_insts(mq_in_interface::in, mq_error_context::in,
    list(bound_inst)::in, list(bound_inst)::out, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_bound_insts(_InInt, _ErrorContext, [], [], !Info, !Specs).
qualify_bound_insts(InInt, ErrorContext,
        [BoundInst0 | BoundInsts0], [BoundInst | BoundInsts],
        !Info, !Specs) :-
    qualify_bound_inst(InInt, ErrorContext, BoundInst0, BoundInst,
        !Info, !Specs),
    qualify_bound_insts(InInt, ErrorContext, BoundInsts0, BoundInsts,
        !Info, !Specs).

    % Qualify an inst of the form bound(functor(...)).
    %
:- pred qualify_bound_inst(mq_in_interface::in, mq_error_context::in,
    bound_inst::in, bound_inst::out, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_bound_inst(InInt, ErrorContext, BoundInst0, BoundInst,
        !Info, !Specs) :-
    BoundInst0 = bound_functor(ConsId, Insts0),
    (
        ConsId = cons(Name, Arity, _),
        Id = item_name(Name, Arity),
        update_recompilation_info(
            recompilation.record_used_item(functor_item, Id, Id), !Info)
    ;
        ( ConsId = tuple_cons(_)
        ; ConsId = closure_cons(_, _)
        ; ConsId = int_const(_)
        ; ConsId = uint_const(_)
        ; ConsId = int8_const(_)
        ; ConsId = uint8_const(_)
        ; ConsId = int16_const(_)
        ; ConsId = uint16_const(_)
        ; ConsId = int32_const(_)
        ; ConsId = uint32_const(_)
        ; ConsId = int64_const(_)
        ; ConsId = uint64_const(_)
        ; ConsId = float_const(_)
        ; ConsId = char_const(_)
        ; ConsId = string_const(_)
        ; ConsId = impl_defined_const(_)
        ; ConsId = type_ctor_info_const(_, _, _)
        ; ConsId = base_typeclass_info_const(_, _, _, _)
        ; ConsId = type_info_cell_constructor(_)
        ; ConsId = typeclass_info_cell_constructor
        ; ConsId = type_info_const(_)
        ; ConsId = typeclass_info_const(_)
        ; ConsId = ground_term_const(_, _)
        ; ConsId = tabling_info_const(_)
        ; ConsId = table_io_entry_desc(_)
        ; ConsId = deep_profiling_proc_layout(_)
        )
    ),
    qualify_inst_list(InInt, ErrorContext, Insts0, Insts, !Info, !Specs),
    BoundInst = bound_functor(ConsId, Insts).

%---------------------------------------------------------------------------%
%
% Module qualify modes.
%

qualify_mode(InInt, ErrorContext, Mode0, Mode, !Info, !Specs) :-
    (
        Mode0 = from_to_mode(InstA0, InstB0),
        qualify_inst(InInt, ErrorContext, InstA0, InstA, !Info, !Specs),
        qualify_inst(InInt, ErrorContext, InstB0, InstB, !Info, !Specs),
        Mode = from_to_mode(InstA, InstB)
    ;
        Mode0 = user_defined_mode(SymName0, Insts0),
        qualify_inst_list(InInt, ErrorContext, Insts0, Insts, !Info, !Specs),
        list.length(Insts, Arity),
        mq_info_get_modes(!.Info, Modes),
        find_unique_match(InInt, ErrorContext, Modes, mode_id,
            mq_id(SymName0, Arity), SymName, !Info, !Specs),
        Mode = user_defined_mode(SymName, Insts)
    ).

qualify_mode_list(_InInt, _ErrorContext, [], [], !Info, !Specs).
qualify_mode_list(InInt, ErrorContext, [Mode0 | Modes0], [Mode | Modes],
        !Info, !Specs) :-
    qualify_mode(InInt, ErrorContext, Mode0, Mode, !Info, !Specs),
    qualify_mode_list(InInt, ErrorContext, Modes0, Modes, !Info, !Specs).

%---------------------------------------------------------------------------%
%
% Module qualify the components of predicate declarations.
%

    % Qualify a list of items of the form Type::Mode, as in a
    % predicate declaration.
    %
:- pred qualify_types_and_modes(mq_in_interface::in, mq_error_context::in,
    list(type_and_mode)::in, list(type_and_mode)::out,
    mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_types_and_modes(_InInt, _ErrorContext, [], [], !Info, !Specs).
qualify_types_and_modes(InInt, ErrorContext,
        [TypeAndMode0 | TypesAndModes0], [TypeAndMode | TypesAndModes],
        !Info, !Specs) :-
    qualify_type_and_mode(InInt, ErrorContext,
        TypeAndMode0, TypeAndMode, !Info, !Specs),
    qualify_types_and_modes(InInt, ErrorContext,
        TypesAndModes0, TypesAndModes, !Info, !Specs).

:- pred qualify_type_and_mode(mq_in_interface::in, mq_error_context::in,
    type_and_mode::in, type_and_mode::out, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_type_and_mode(InInt, ErrorContext, TypeAndMode0, TypeAndMode,
        !Info, !Specs) :-
    (
        TypeAndMode0 = type_only(Type0),
        qualify_type(InInt, ErrorContext, Type0, Type, !Info, !Specs),
        TypeAndMode = type_only(Type)
    ;
        TypeAndMode0 = type_and_mode(Type0, Mode0),
        qualify_type(InInt, ErrorContext, Type0, Type, !Info, !Specs),
        qualify_mode(InInt, ErrorContext, Mode0, Mode, !Info, !Specs),
        TypeAndMode = type_and_mode(Type, Mode)
    ).

%---------------------------------------------------------------------------%
%
% Module qualify the components of typeclass declarations and typeclass
% constraints.
%

:- pred qualify_prog_constraints(mq_in_interface::in,
    mq_constraint_error_context::in,
    prog_constraints::in, prog_constraints::out, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_prog_constraints(InInt, ConstraintErrorContext,
        Constraints0, Constraints, !Info, !Specs) :-
    Constraints0 = constraints(UnivCs0, ExistCs0),
    % XXX We could pass a more specific error context.
    qualify_prog_constraint_list(InInt, ConstraintErrorContext,
        UnivCs0, UnivCs, !Info, !Specs),
    qualify_prog_constraint_list(InInt, ConstraintErrorContext,
        ExistCs0, ExistCs, !Info, !Specs),
    Constraints = constraints(UnivCs, ExistCs).

:- pred qualify_prog_constraint_list(mq_in_interface::in,
    mq_constraint_error_context::in,
    list(prog_constraint)::in, list(prog_constraint)::out,
    mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_prog_constraint_list(_InInt, _ConstraintErrorContext,
        [], [], !Info, !Specs).
qualify_prog_constraint_list(InInt, ConstraintErrorContext,
        [Constraint0 | Constraints0], [Constraint | Constraints],
        !Info, !Specs) :-
    qualify_prog_constraint(InInt, ConstraintErrorContext,
        Constraint0, Constraint, !Info, !Specs),
    qualify_prog_constraint_list(InInt, ConstraintErrorContext,
        Constraints0, Constraints, !Info, !Specs).

:- pred qualify_prog_constraint(mq_in_interface::in,
    mq_constraint_error_context::in,
    prog_constraint::in, prog_constraint::out, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_prog_constraint(InInt, ContainingErrorContext,
        Constraint0, Constraint, !Info, !Specs) :-
    Constraint0 = constraint(ClassName0, Types0),
    list.length(Types0, Arity),
    OutsideContext = mqec_typeclass_constraint_name(ContainingErrorContext),
    qualify_class_name(InInt, OutsideContext,
        mq_id(ClassName0, Arity), ClassName, !Info, !Specs),
    ErrorContext = mqec_typeclass_constraint(ClassName0, Arity,
        ContainingErrorContext),
    qualify_type_list(InInt, ErrorContext, Types0, Types, !Info, !Specs),
    Constraint = constraint(ClassName, Types).

:- pred qualify_class_name(mq_in_interface::in, mq_error_context::in,
    mq_id::in, sym_name::out, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_class_name(InInt, ErrorContext, Class0, Name, !Info, !Specs) :-
    mq_info_get_classes(!.Info, ClassIdSet),
    find_unique_match(InInt, ErrorContext, ClassIdSet, class_id, Class0, Name,
        !Info, !Specs).

%---------------------%

:- pred qualify_class_decls(mq_in_interface::in, mq_error_context::in,
    list(class_decl)::in, list(class_decl)::out,
    mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_class_decls(_InInt, _ErrorContext, [], [], !Info, !Specs).
qualify_class_decls(InInt, ErrorContext,
        [Decl0 | Decls0], [Decl | Decls], !Info, !Specs) :-
    % XXX We could pass a more specific error context.
    qualify_class_decl(InInt, ErrorContext, Decl0, Decl,
        !Info, !Specs),
    qualify_class_decls(InInt, ErrorContext, Decls0, Decls,
        !Info, !Specs).

:- pred qualify_class_decl(mq_in_interface::in, mq_error_context::in,
    class_decl::in, class_decl::out, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_class_decl(InInt, ErrorContext, Decl0, Decl, !Info, !Specs) :-
    % There is no need to qualify the method name, since that is done
    % when the item is parsed.
    (
        Decl0 = class_decl_pred_or_func(PredOrFuncInfo0),
        PredOrFuncInfo0 = class_pred_or_func_info(Name, PredOrFunc,
            TypesAndModes0, MaybeWithType0, MaybeWithInst0, MaybeDetism,
            TypeVarset, InstVarset, ExistQVars,
            Purity, Constraints0, Context),
        % XXX We could pass a more specific error context.
        qualify_types_and_modes(InInt, ErrorContext,
            TypesAndModes0, TypesAndModes, !Info, !Specs),
        ConstraintErrorContext = mqcec_class_method(Context, PredOrFunc,
            unqualify_name(Name)),
        qualify_prog_constraints(InInt, ConstraintErrorContext,
            Constraints0, Constraints, !Info, !Specs),
        (
            MaybeWithType0 = yes(WithType0),
            % XXX We could pass a more specific error context.
            qualify_type(InInt, ErrorContext, WithType0, WithType,
                !Info, !Specs),
            MaybeWithType = yes(WithType)
        ;
            MaybeWithType0 = no,
            MaybeWithType = no
        ),
        (
            MaybeWithInst0 = yes(WithInst0),
            % XXX We could pass a more specific error context.
            qualify_inst(InInt, ErrorContext, WithInst0, WithInst,
                !Info, !Specs),
            MaybeWithInst = yes(WithInst)
        ;
            MaybeWithInst0 = no,
            MaybeWithInst = no
        ),
        PredOrFuncInfo = class_pred_or_func_info(Name, PredOrFunc,
            TypesAndModes, MaybeWithType, MaybeWithInst, MaybeDetism,
            TypeVarset, InstVarset, ExistQVars,
            Purity, Constraints, Context),
        Decl = class_decl_pred_or_func(PredOrFuncInfo)
    ;
        Decl0 = class_decl_mode(ModeInfo0),
        ModeInfo0 = class_mode_info(PredOrFunc, Name, Modes0,
            MaybeWithInst0, MaybeDetism, Varset, Context),
        qualify_mode_list(InInt, ErrorContext, Modes0, Modes, !Info, !Specs),
        (
            MaybeWithInst0 = yes(WithInst0),
            % XXX We could pass a more specific error context.
            qualify_inst(InInt, ErrorContext, WithInst0, WithInst,
                !Info, !Specs),
            MaybeWithInst = yes(WithInst)
        ;
            MaybeWithInst0 = no,
            MaybeWithInst = no
        ),
        ModeInfo = class_mode_info(PredOrFunc, Name, Modes,
            MaybeWithInst, MaybeDetism, Varset, Context),
        Decl = class_decl_mode(ModeInfo)
    ).

%---------------------------------------------------------------------------%
%
% Module qualify instance definitions.
%

:- pred qualify_instance_body(sym_name::in,
    instance_body::in, instance_body::out) is det.

qualify_instance_body(ClassName, InstanceBody0, InstanceBody) :-
    (
        InstanceBody0 = instance_body_abstract,
        InstanceBody = instance_body_abstract
    ;
        InstanceBody0 = instance_body_concrete(Methods0),
        (
            ClassName = unqualified(_),
            InstanceBody = InstanceBody0
        ;
            ClassName = qualified(_, _),
            sym_name_get_module_name_default(ClassName, unqualified(""),
                DefaultModuleName),
            list.map(qualify_instance_method(DefaultModuleName),
                Methods0, Methods),
            InstanceBody = instance_body_concrete(Methods)
        )
    ).

:- pred qualify_instance_method(module_name::in,
    instance_method::in, instance_method::out) is det.

qualify_instance_method(DefaultModuleName, InstanceMethod0, InstanceMethod) :-
    % XXX InstanceProcDef may contain a list of clauses.
    % Why aren't those clauses module qualified?
    InstanceMethod0 = instance_method(PredOrFunc, MethodSymName0,
        InstanceProcDef, Arity, DeclContext),
    (
        MethodSymName0 = unqualified(Name),
        MethodSymName = qualified(DefaultModuleName, Name)
    ;
        MethodSymName0 = qualified(MethodModuleName, MethodBaseName),
        ( if
            partial_sym_name_matches_full(MethodModuleName, DefaultModuleName)
        then
            MethodSymName = qualified(DefaultModuleName, MethodBaseName)
        else
            % This case is an error. The user must have written something like
            %   :- instance foo.bar(some_type) where [
            %       pred(baz.p/1) is q
            %   ].
            % where the module qualifier on the pred or func in the instance
            % (baz) does not match the qualifier for the class name (foo).
            %
            % We don't report the error here, we just leave the original
            % module qualifier intact so that the error can be reported
            % later on. XXX Where is the code that does this reporting?
            MethodSymName = MethodSymName0
        )
    ),
    InstanceMethod = instance_method(PredOrFunc, MethodSymName,
        InstanceProcDef, Arity, DeclContext).

%---------------------------------------------------------------------------%
%
% Module qualify the definitions of mutables.
%

:- pred qualify_mutable(mq_in_interface::in,
    item_mutable_info::in, item_mutable_info::out, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_mutable(InInt, ItemMutable0, ItemMutable, !Info, !Specs) :-
    ItemMutable0 = item_mutable_info(Name, OrigType0, Type0, OrigInst0, Inst0,
        InitTerm, Attrs, Varset, Context, SeqNum),
    ErrorContext = mqec_mutable(Context, Name),
    qualify_type(InInt, ErrorContext, OrigType0, OrigType, !Info, [], _),
    qualify_type(InInt, ErrorContext, Type0, Type, !Info, !Specs),
    qualify_inst(InInt, ErrorContext, OrigInst0, OrigInst, !Info, [], _),
    qualify_inst(InInt, ErrorContext, Inst0, Inst, !Info, !Specs),
    ItemMutable = item_mutable_info(Name, OrigType, Type, OrigInst, Inst,
        InitTerm, Attrs, Varset, Context, SeqNum).

%---------------------------------------------------------------------------%
%
% Module qualify pragmas.
%

:- pred qualify_pragma(mq_in_interface::in, prog_context::in,
    pragma_type::in, pragma_type::out, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_pragma(InInt, Context, Pragma0, Pragma, !Info, !Specs) :-
    (
        ( Pragma0 = pragma_foreign_decl(_)
        ; Pragma0 = pragma_foreign_code(_)
        ; Pragma0 = pragma_external_proc(_)
          % The predicate name in the pragma_external_proc is constructed
          % already qualified.
        ; Pragma0 = pragma_inline(_)
        ; Pragma0 = pragma_no_inline(_)
        ; Pragma0 = pragma_consider_used(_)
        ; Pragma0 = pragma_obsolete(_)
        ; Pragma0 = pragma_no_detism_warning(_)
        ; Pragma0 = pragma_require_tail_recursion(_)
        ; Pragma0 = pragma_unused_args(_)
        ; Pragma0 = pragma_exceptions(_)
        ; Pragma0 = pragma_trailing_info(_)
        ; Pragma0 = pragma_mm_tabling_info(_)
        ; Pragma0 = pragma_fact_table(_)
        ; Pragma0 = pragma_promise_pure(_)
        ; Pragma0 = pragma_promise_semipure(_)
        ; Pragma0 = pragma_promise_eqv_clauses(_)
        ; Pragma0 = pragma_terminates(_)
        ; Pragma0 = pragma_does_not_terminate(_)
        ; Pragma0 = pragma_check_termination(_)
        ; Pragma0 = pragma_mode_check_clauses(_)
        ; Pragma0 = pragma_require_feature_set(_)
        ),
        Pragma = Pragma0
    ;
        Pragma0 = pragma_foreign_export_enum(FEEInfo0),
        FEEInfo0 = pragma_info_foreign_export_enum(Lang, TypeCtor0,
            Attributes, Overrides),
        ErrorContext = mqec_pragma(Context, Pragma0),
        mq_info_get_suppress_found_undef(!.Info, OldSuppressUndef),
        mq_info_set_suppress_found_undef(suppress_found_undef, !Info),
        qualify_type_ctor(InInt, ErrorContext, TypeCtor0, TypeCtor,
            !Info, !Specs),
        mq_info_set_suppress_found_undef(OldSuppressUndef, !Info),
        FEEInfo = pragma_info_foreign_export_enum(Lang, TypeCtor,
            Attributes, Overrides),
        Pragma = pragma_foreign_export_enum(FEEInfo)
    ;
        Pragma0 = pragma_foreign_enum(FEInfo0),
        FEInfo0 = pragma_info_foreign_enum(Lang, TypeCtor0, Values),
        ErrorContext = mqec_pragma(Context, Pragma0),
        qualify_type_ctor(InInt, ErrorContext, TypeCtor0, TypeCtor,
            !Info, !Specs),
        FEInfo = pragma_info_foreign_enum(Lang, TypeCtor, Values),
        Pragma = pragma_foreign_enum(FEInfo)
    ;
        Pragma0 = pragma_foreign_proc(FPInfo0),
        FPInfo0 = pragma_info_foreign_proc(Attrs0, Name, PredOrFunc,
            Vars0, Varset, InstVarset, Impl),
        ErrorContext = mqec_pragma(Context, Pragma0),
        qualify_pragma_vars(InInt, ErrorContext, Vars0, Vars, !Info, !Specs),
        UserSharing0 = get_user_annotated_sharing(Attrs0),
        qualify_user_sharing(InInt, ErrorContext, UserSharing0, UserSharing,
            !Info, !Specs),
        set_user_annotated_sharing(UserSharing, Attrs0, Attrs),
        FPInfo = pragma_info_foreign_proc(Attrs, Name, PredOrFunc,
            Vars, Varset, InstVarset, Impl),
        Pragma = pragma_foreign_proc(FPInfo)
    ;
        Pragma0 = pragma_oisu(OISUInfo0),
        OISUInfo0 = pragma_info_oisu(TypeCtor0, CreatorPreds,
            MutatorPreds, DestructorPreds),
        % XXX Preds
        ErrorContext = mqec_pragma(Context, Pragma0),
        qualify_type_ctor(InInt, ErrorContext, TypeCtor0, TypeCtor,
            !Info, !Specs),
        OISUInfo = pragma_info_oisu(TypeCtor, CreatorPreds,
            MutatorPreds, DestructorPreds),
        Pragma = pragma_oisu(OISUInfo)
    ;
        Pragma0 = pragma_tabled(TabledInfo0),
        TabledInfo0 = pragma_info_tabled(EvalMethod, PredNameArityPF,
            MModes0, Attrs),
        (
            MModes0 = yes(Modes0),
            ErrorContext = mqec_pragma(Context, Pragma0),
            qualify_mode_list(InInt, ErrorContext, Modes0, Modes,
                !Info, !Specs),
            MModes = yes(Modes)
        ;
            MModes0 = no,
            MModes = no
        ),
        TabledInfo = pragma_info_tabled(EvalMethod, PredNameArityPF,
            MModes, Attrs),
        Pragma = pragma_tabled(TabledInfo)
    ;
        Pragma0 = pragma_foreign_proc_export(FPEInfo0),
        FPEInfo0 = pragma_info_foreign_proc_export(Lang, PredNameModesPF0,
            CFunc),
        PredNameModesPF0 = pred_name_modes_pf(Name, Modes0, PredOrFunc),
        ErrorContext = mqec_pragma(Context, Pragma0),
        qualify_mode_list(InInt, ErrorContext, Modes0, Modes,
            !Info, !Specs),
        PredNameModesPF = pred_name_modes_pf(Name, Modes, PredOrFunc),
        FPEInfo = pragma_info_foreign_proc_export(Lang, PredNameModesPF,
            CFunc),
        Pragma = pragma_foreign_proc_export(FPEInfo)
    ;
        Pragma0 = pragma_type_spec(TypeSpecInfo0),
        TypeSpecInfo0 = pragma_info_type_spec(PredName, SpecializedPredName,
            Arity, PredOrFunc, MaybeModes0, Subst0, TVarSet, Items),
        ErrorContext = mqec_pragma(Context, Pragma0),
        (
            MaybeModes0 = yes(Modes0),
            qualify_mode_list(InInt, ErrorContext, Modes0, Modes,
                !Info, !Specs),
            MaybeModes = yes(Modes)
        ;
            MaybeModes0 = no,
            MaybeModes = no
        ),
        qualify_type_spec_subst(InInt, ErrorContext, Subst0, Subst,
            !Info, !Specs),
        TypeSpecInfo = pragma_info_type_spec(PredName, SpecializedPredName,
            Arity, PredOrFunc, MaybeModes, Subst, TVarSet, Items),
        Pragma = pragma_type_spec(TypeSpecInfo)
    ;
        Pragma0 = pragma_termination_info(TermInfo0),
        TermInfo0 = pragma_info_termination_info(PredNameModesPF0, Args, Term),
        PredNameModesPF0 = pred_name_modes_pf(SymName, ModeList0, PredOrFunc),
        ErrorContext = mqec_pragma(Context, Pragma0),
        qualify_mode_list(InInt, ErrorContext, ModeList0, ModeList,
            !Info, !Specs),
        PredNameModesPF = pred_name_modes_pf(SymName, ModeList, PredOrFunc),
        TermInfo = pragma_info_termination_info(PredNameModesPF, Args, Term),
        Pragma = pragma_termination_info(TermInfo)
    ;
        Pragma0 = pragma_structure_sharing(SharingInfo0),
        SharingInfo0 = pragma_info_structure_sharing(PredNameModesPF0,
            Vars, Types, Sharing),
        PredNameModesPF0 = pred_name_modes_pf(SymName, ModeList0, PredOrFunc),
        ErrorContext = mqec_pragma(Context, Pragma0),
        qualify_mode_list(InInt, ErrorContext, ModeList0, ModeList,
            !Info, !Specs),
        PredNameModesPF = pred_name_modes_pf(SymName, ModeList, PredOrFunc),
        SharingInfo = pragma_info_structure_sharing(PredNameModesPF,
            Vars, Types, Sharing),
        Pragma = pragma_structure_sharing(SharingInfo)
    ;
        Pragma0 = pragma_structure_reuse(ReuseInfo0),
        ReuseInfo0 = pragma_info_structure_reuse(PredNameModesPF0,
            Vars, Types, ReuseTuples),
        PredNameModesPF0 = pred_name_modes_pf(SymName, ModeList0, PredOrFunc),
        ErrorContext = mqec_pragma(Context, Pragma0),
        qualify_mode_list(InInt, ErrorContext, ModeList0, ModeList,
            !Info, !Specs),
        PredNameModesPF = pred_name_modes_pf(SymName, ModeList, PredOrFunc),
        ReuseInfo = pragma_info_structure_reuse(PredNameModesPF,
            Vars, Types, ReuseTuples),
        Pragma = pragma_structure_reuse(ReuseInfo)
    ;
        Pragma0 = pragma_termination2_info(Term2Info0),
        Term2Info0 = pragma_info_termination2_info(PredNameModesPF0,
            SuccessArgs, FailureArgs, Term),
        PredNameModesPF0 = pred_name_modes_pf(SymName, ModeList0, PredOrFunc),
        ErrorContext = mqec_pragma(Context, Pragma0),
        qualify_mode_list(InInt, ErrorContext, ModeList0, ModeList,
            !Info, !Specs),
        PredNameModesPF = pred_name_modes_pf(SymName, ModeList, PredOrFunc),
        Term2Info = pragma_info_termination2_info(PredNameModesPF,
            SuccessArgs, FailureArgs, Term),
        Pragma = pragma_termination2_info(Term2Info)
    ).

:- pred qualify_pragma_vars(mq_in_interface::in, mq_error_context::in,
    list(pragma_var)::in, list(pragma_var)::out, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_pragma_vars(_InInt, _ErrorContext, [], [], !Info, !Specs).
qualify_pragma_vars(InInt, ErrorContext,
        [PragmaVar0 | PragmaVars0], [PragmaVar | PragmaVars], !Info, !Specs) :-
    % XXX We could pass a more specific error context.
    qualify_pragma_var(InInt, ErrorContext, PragmaVar0, PragmaVar,
        !Info, !Specs),
    qualify_pragma_vars(InInt, ErrorContext, PragmaVars0, PragmaVars,
        !Info, !Specs).

:- pred qualify_pragma_var(mq_in_interface::in, mq_error_context::in,
    pragma_var::in, pragma_var::out, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_pragma_var(InInt, ErrorContext, PragmaVar0, PragmaVar,
        !Info, !Specs) :-
    PragmaVar0 = pragma_var(Var, Name, Mode0, Box),
    % XXX We could pass a more specific error context.
    qualify_mode(InInt, ErrorContext, Mode0, Mode, !Info, !Specs),
    PragmaVar = pragma_var(Var, Name, Mode, Box).

:- pred qualify_type_spec_subst(mq_in_interface::in, mq_error_context::in,
    assoc_list(tvar, mer_type)::in, assoc_list(tvar, mer_type)::out,
    mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_type_spec_subst(_InInt, _ErrorContext, [], [], !Info, !Specs).
qualify_type_spec_subst(InInt, ErrorContext,
        [Var - Type0 |  Subst0], [Var - Type | Subst], !Info, !Specs) :-
    % XXX We could pass a more specific error context.
    qualify_type(InInt, ErrorContext, Type0, Type, !Info, !Specs),
    qualify_type_spec_subst(InInt, ErrorContext, Subst0, Subst,
        !Info, !Specs).

:- pred qualify_user_sharing(mq_in_interface::in, mq_error_context::in,
    user_annotated_sharing::in, user_annotated_sharing::out,
    mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_user_sharing(InInt, ErrorContext, UserSharing0, UserSharing,
        !Info, !Specs) :-
    (
        UserSharing0 = no_user_annotated_sharing,
        UserSharing = UserSharing0
    ;
        UserSharing0 = user_sharing(Sharing, MaybeTypes0),
        (
            MaybeTypes0 = yes(user_type_info(Types0, TVarset)),
            qualify_type_list(InInt, ErrorContext, Types0, Types,
                !Info, !Specs),
            MaybeTypes = yes(user_type_info(Types, TVarset)),
            UserSharing = user_sharing(Sharing, MaybeTypes)
        ;
            MaybeTypes0 = no,
            UserSharing = UserSharing0
        )
    ).

%---------------------------------------------------------------------------%
%
% Module qualify event specifications.
%

qualify_event_specs(_InInt, _, [], [], !Info, !Specs).
qualify_event_specs(InInt, FileName,
        [Name - Spec0 | NameSpecs0], [Name - Spec | NameSpecs],
        !Info, !Specs) :-
    qualify_event_spec(InInt, FileName, Spec0, Spec, !Info, !Specs),
    qualify_event_specs(InInt, FileName, NameSpecs0, NameSpecs, !Info, !Specs).

:- pred qualify_event_spec(mq_in_interface::in, string::in,
    event_spec::in, event_spec::out, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_event_spec(InInt, FileName, EventSpec0, EventSpec,
        !Info, !Specs) :-
    EventSpec0 = event_spec(EventNumber, EventName, EventLineNumber,
        Attrs0, SynthAttrNumOrder),
    list.map_foldl2(
        qualify_event_attr(InInt, EventName, FileName, EventLineNumber),
        Attrs0, Attrs, !Info, !Specs),
    EventSpec = event_spec(EventNumber, EventName, EventLineNumber,
        Attrs, SynthAttrNumOrder).

:- pred qualify_event_attr(mq_in_interface::in,
    string::in, string::in, int::in,
    event_attribute::in, event_attribute::out, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_event_attr(InInt, EventName, FileName, LineNumber,
        Attr0, Attr, !Info, !Specs) :-
    Attr0 = event_attribute(AttrNum, AttrName, AttrType0, AttrMode0,
        MaybeSynthCall),
    Context = context(FileName, LineNumber),
    ErrorContext = mqec_event_spec_attr(Context, EventName, AttrName),
    qualify_type(InInt, ErrorContext, AttrType0, AttrType, !Info, !Specs),
    qualify_mode(InInt, ErrorContext, AttrMode0, AttrMode, !Info, !Specs),
    Attr = event_attribute(AttrNum, AttrName, AttrType, AttrMode,
        MaybeSynthCall).

%---------------------------------------------------------------------------%
:- end_module parse_tree.module_qual.qualify_items.
%---------------------------------------------------------------------------%
