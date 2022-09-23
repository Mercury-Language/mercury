%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2015-2021 The Mercury team.
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

%---------------------%

    % Module qualify the given parse tree.

:- pred module_qualify_parse_tree_module_src(
    parse_tree_module_src::in, parse_tree_module_src::out,
    mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred module_qualify_parse_tree_int3(
    parse_tree_int3::in, parse_tree_int3::out,
    mq_info::in, mq_info::out,
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

%---------------------%

    % Module qualify the event specifications of an augmented compilation unit.
    %
:- pred qualify_event_specs(mq_in_interface::in, string::in,
    assoc_list(string, event_spec)::in, assoc_list(string, event_spec)::out,
    mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.module_qual.id_set.
:- import_module parse_tree.module_qual.qual_errors.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_data_pragma.

:- import_module int.
:- import_module one_or_more.
:- import_module pair.
:- import_module require.
:- import_module term_context.

%---------------------------------------------------------------------------%

module_qualify_parse_tree_module_src(ParseTreeModuleSrc0, ParseTreeModuleSrc,
        !Info, !Specs) :-
    ParseTreeModuleSrc0 = parse_tree_module_src(ModuleName, ModuleNameContext,
        IntInclMap, ImpInclMap, InclMap,
        IntImportMap, IntUseMap, ImpImportMap, ImpUseMap, ImportUseMap,
        IntFIMSpecMap, ImpFIMSpecMap, IntSelfFIMLangs, ImpSelfFIMLangs,

        TypeCtorCheckedMap0, InstCtorCheckedMap0, ModeCtorCheckedMap0,
        TypeSpecs, InstModeSpecs,

        IntTypeClasses0, IntInstances0, IntPredDecls0, IntModeDecls0,
        IntDeclPragmas0, IntPromises0, IntBadPreds,

        ImpTypeClasses0, ImpInstances0, ImpPredDecls0, ImpModeDecls0,
        ImpClauses0, ImpForeignExportEnums0,
        ImpDeclPragmas0, ImpImplPragmas0, ImpPromises0,
        ImpInitialises0, ImpFinalises0, ImpMutables0),

    map.map_values_foldl2(module_qualify_type_ctor_checked_defn,
        TypeCtorCheckedMap0, TypeCtorCheckedMap, !Info, !Specs),
    map.map_values_foldl2(module_qualify_inst_ctor_checked_defn,
        InstCtorCheckedMap0, InstCtorCheckedMap, !Info, !Specs),
    map.map_values_foldl2(module_qualify_mode_ctor_checked_defn,
        ModeCtorCheckedMap0, ModeCtorCheckedMap, !Info, !Specs),

    InInt = mq_used_in_interface,
    list.map_foldl2(module_qualify_item_typeclass(InInt),
        IntTypeClasses0, IntTypeClasses, !Info, !Specs),
    list.map_foldl2(module_qualify_item_instance(InInt),
        IntInstances0, IntInstances, !Info, !Specs),
    list.map_foldl2(module_qualify_item_pred_decl(InInt),
        IntPredDecls0, IntPredDecls, !Info, !Specs),
    list.map_foldl2(module_qualify_item_mode_decl(InInt),
        IntModeDecls0, IntModeDecls, !Info, !Specs),
    list.map_foldl2(module_qualify_item_decl_pragma(InInt),
        IntDeclPragmas0, IntDeclPragmas, !Info, !Specs),
    % Promises don't need to be qualified.
    IntPromises = IntPromises0,

    InImp = mq_not_used_in_interface,
    list.map_foldl2(module_qualify_item_typeclass(InImp),
        ImpTypeClasses0, ImpTypeClasses, !Info, !Specs),
    list.map_foldl2(module_qualify_item_instance(InImp),
        ImpInstances0, ImpInstances, !Info, !Specs),
    list.map_foldl2(module_qualify_item_pred_decl(InImp),
        ImpPredDecls0, ImpPredDecls, !Info, !Specs),
    list.map_foldl2(module_qualify_item_mode_decl(InImp),
        ImpModeDecls0, ImpModeDecls, !Info, !Specs),
    % Clauses don't need to be qualified.
    ImpClauses = ImpClauses0,
    list.map_foldl2(module_qualify_item_foreign_export_enum(InImp),
        ImpForeignExportEnums0, ImpForeignExportEnums, !Info, !Specs),
    list.map_foldl2(module_qualify_item_decl_pragma(InImp),
        ImpDeclPragmas0, ImpDeclPragmas, !Info, !Specs),
    list.map_foldl2(module_qualify_item_impl_pragma(InImp),
        ImpImplPragmas0, ImpImplPragmas, !Info, !Specs),
    % Initialise and finalise items and promises don't need to be qualified.
    ImpInitialises = ImpInitialises0,
    ImpFinalises = ImpFinalises0,
    ImpPromises = ImpPromises0,
    list.map_foldl2(module_qualify_item_mutable(InImp),
        ImpMutables0, ImpMutables, !Info, !Specs),

    ParseTreeModuleSrc = parse_tree_module_src(ModuleName, ModuleNameContext,
        IntInclMap, ImpInclMap, InclMap,
        IntImportMap, IntUseMap, ImpImportMap, ImpUseMap, ImportUseMap,
        IntFIMSpecMap, ImpFIMSpecMap, IntSelfFIMLangs, ImpSelfFIMLangs,

        TypeCtorCheckedMap, InstCtorCheckedMap, ModeCtorCheckedMap,
        TypeSpecs, InstModeSpecs,

        IntTypeClasses, IntInstances, IntPredDecls, IntModeDecls,
        IntDeclPragmas, IntPromises, IntBadPreds,

        ImpTypeClasses, ImpInstances, ImpPredDecls, ImpModeDecls, ImpClauses,
        ImpForeignExportEnums, ImpDeclPragmas, ImpImplPragmas, ImpPromises,
        ImpInitialises, ImpFinalises, ImpMutables).

%---------------------------------------------------------------------------%

module_qualify_parse_tree_int3(OrigParseTreeInt3, ParseTreeInt3,
        !Info, !Specs) :-
    OrigParseTreeInt3 = parse_tree_int3(ModuleName, ModuleNameContext,
        InclMap, ImportUseMap,
        IntTypeDefnMap0, IntInstDefnMap0, IntModeDefnMap0,
        IntTypeClasses0, IntInstances0, IntTypeRepns0),

    InInt = mq_used_in_interface,
    map.map_values_foldl2(module_qualify_type_ctor_checked_defn,
        IntTypeDefnMap0, IntTypeDefnMap, !Info, !Specs),
    map.map_values_foldl2(module_qualify_inst_ctor_checked_defn,
        IntInstDefnMap0, IntInstDefnMap, !Info, !Specs),
    map.map_values_foldl2(module_qualify_mode_ctor_checked_defn,
        IntModeDefnMap0, IntModeDefnMap, !Info, !Specs),
    list.map_foldl2(module_qualify_item_typeclass(InInt),
        IntTypeClasses0, IntTypeClasses, !Info, !Specs),
    list.map_foldl2(module_qualify_item_instance(InInt),
        IntInstances0, IntInstances, !Info, !Specs),
    map.map_values_foldl2(module_qualify_item_type_repn(ModuleName, InInt),
        IntTypeRepns0, IntTypeRepns, !Info, !Specs),

    ParseTreeInt3 = parse_tree_int3(ModuleName, ModuleNameContext,
        InclMap, ImportUseMap,
        IntTypeDefnMap, IntInstDefnMap, IntModeDefnMap,
        IntTypeClasses, IntInstances, IntTypeRepns).

%---------------------------------------------------------------------------%
%
% Module qualify the checked type, inst and mode definition maps.
%
% Each entry in each of these checked maps contains
%
% - the actual definition of the type, inst or mode, together with its status,
%   and
%
% - the definition's source items (i.e. the items from which that definition
%   was derived) in the interface and the implementation sections.
%
% When we module qualify those items, we pass mq_not_used_in_interface
% or mq_used_in_interface as appropriate to the section, and we keep
% any error specs the qualification process generates. This is obviously
% the right thing to do.
%
% When we process the actual definitions, we *always* pass
% mq_not_used_in_interface, and we *always* throw away the resulting
% error specs. This is also the right thing to do, but the reason is
% not obvious.
%
% The reason for passing mq_not_used_in_interface when processing the
% definitions themselves is that the definitions are derived from
% *both interface and implementation items*. If we passed mq_used_in_interface,
% then we could record that mq_id was used in the interface just because
% we used it to resolve a reference to a part of a definition that came from
% the implementation section, which would be incorrect. Any mq_ids used by
% parts of the definition that came from the interface section will not be
% recorded as such when we process the definition itself, but it *will* be
% recorded as such when we process the interface source items.
%
% Likewise, any errors we generate while module qualifying the definitions
% but then throw away *will* be generated when we module qualify the
% source items. The difference is that the source items include the context
% we want for each error message, while the definitions themselves do not.
% We could add a context to the definitions, but, given the above, there is
% no point.
%

:- pred module_qualify_type_ctor_checked_defn(
    type_ctor_checked_defn::in, type_ctor_checked_defn::out,
    mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

module_qualify_type_ctor_checked_defn(CheckedDefn0, CheckedDefn,
        !Info, !Specs) :-
    (
        CheckedDefn0 = checked_defn_solver(SolverDefn0, SrcDefns0),
        (
            SolverDefn0 = solver_type_abstract(_AbsSolverStatus, _AbsDefn0),
            % Abstract definitions do not need qualification.
            SolverDefn = SolverDefn0
        ;
            SolverDefn0 = solver_type_full(MaybeAbsDefn0, FullDefn0),
            % Non-abstract definitions can occur only in the implementation
            % section.
            InInt = mq_not_used_in_interface,
            module_qualify_item_type_defn(qualify_type_defn_solver,
                InInt, FullDefn0, FullDefn, !Info, !Specs),
            SolverDefn = solver_type_full(MaybeAbsDefn0, FullDefn)
        ),
        SrcDefns0 = src_defns_solver(MaybeIntDefn0, MaybeImpDefn0),
        QualifyPred = module_qualify_item_type_defn(qualify_type_defn),
        maybe_qualify_defn(QualifyPred, mq_used_in_interface,
            MaybeIntDefn0, MaybeIntDefn, !Info, !Specs),
        maybe_qualify_defn(QualifyPred, mq_not_used_in_interface,
            MaybeImpDefn0, MaybeImpDefn, !Info, !Specs),
        SrcDefns = src_defns_solver(MaybeIntDefn, MaybeImpDefn),
        CheckedDefn = checked_defn_solver(SolverDefn, SrcDefns)
    ;
        CheckedDefn0 = checked_defn_std(StdDefn0, SrcDefns0),
        (
            StdDefn0 = std_mer_type_eqv(EqvStatus, EqvDefn0),
            InInt = std_eqv_status_section(EqvStatus),
            module_qualify_item_type_defn(qualify_type_defn_eqv,
                InInt, EqvDefn0, EqvDefn, !Info, !Specs),
            StdDefn = std_mer_type_eqv(EqvStatus, EqvDefn)
        ;
            StdDefn0 = std_mer_type_subtype(SubStatus, SubDefn0),
            InInt = std_sub_status_section(SubStatus),
            module_qualify_item_type_defn(qualify_type_defn_sub,
                InInt, SubDefn0, SubDefn, !Info, !Specs),
            StdDefn = std_mer_type_subtype(SubStatus, SubDefn)
        ;
            StdDefn0 = std_mer_type_du_all_plain_constants(DuStatus, DuDefn0,
                HeadCtor, TailCtors, CJCsDefnOrEnum),
            InInt = std_du_status_section(DuStatus),
            module_qualify_item_type_defn(qualify_type_defn_du,
                InInt, DuDefn0, DuDefn, !Info, !Specs),
            % Foreign types and foreign enums need no qualification.
            StdDefn = std_mer_type_du_all_plain_constants(DuStatus, DuDefn,
                HeadCtor, TailCtors, CJCsDefnOrEnum)
        ;
            StdDefn0 = std_mer_type_du_not_all_plain_constants(DuStatus,
                DuDefn0, CJCsDefnOrEnum),
            InInt = std_du_status_section(DuStatus),
            module_qualify_item_type_defn(qualify_type_defn_du,
                InInt, DuDefn0, DuDefn, !Info, !Specs),
            % Foreign types and foreign enums need no qualification.
            StdDefn = std_mer_type_du_not_all_plain_constants(DuStatus,
                DuDefn, CJCsDefnOrEnum)
        ;
            StdDefn0 = std_mer_type_abstract(_AbsStatus, _AbsDefn,
                _CJCsDefnOrEnum),
            % Abstract types, foreign types and foreign enums
            % need no qualification.
            StdDefn = StdDefn0
        ),
        SrcDefns0 = src_defns_std(IntDefns0, ImpDefns0, ImpForeignEnums0),
        list.map_foldl2(
            module_qualify_item_type_defn(qualify_type_defn,
                mq_used_in_interface),
            IntDefns0, IntDefns, !Info, !Specs),
        list.map_foldl2(
            module_qualify_item_type_defn(qualify_type_defn,
                mq_not_used_in_interface),
            ImpDefns0, ImpDefns, !Info, !Specs),
        list.map_foldl2(
            module_qualify_item_foreign_enum(mq_not_used_in_interface),
            ImpForeignEnums0, ImpForeignEnums, !Info, !Specs),
        SrcDefns = src_defns_std(IntDefns, ImpDefns, ImpForeignEnums),
        CheckedDefn = checked_defn_std(StdDefn, SrcDefns)
    ).

:- pred module_qualify_inst_ctor_checked_defn(
    inst_ctor_checked_defn::in, inst_ctor_checked_defn::out,
    mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

module_qualify_inst_ctor_checked_defn(CheckedDefn0, CheckedDefn,
        !Info, !Specs) :-
    CheckedDefn0 = checked_defn_inst(StdInstDefn0, SrcDefns0),
    StdInstDefn0 = std_inst_defn(Status, MaybeAbstractDefn0),
    % Because of the inst_for_type_constructor field,
    % even abstract inst definitions need qualifying.
    InInt = std_inst_status_section(Status),
    module_qualify_item_inst_defn(qualify_inst_defn, InInt,
        MaybeAbstractDefn0, MaybeAbstractDefn, !Info, !Specs),
    StdInstDefn = std_inst_defn(Status, MaybeAbstractDefn),
    SrcDefns0 = src_defns_inst(MaybeIntDefn0, MaybeImpDefn0),
    QualifyPred = module_qualify_item_inst_defn(qualify_inst_defn),
    maybe_qualify_defn(QualifyPred, mq_used_in_interface,
        MaybeIntDefn0, MaybeIntDefn, !Info, !Specs),
    maybe_qualify_defn(QualifyPred, mq_not_used_in_interface,
        MaybeImpDefn0, MaybeImpDefn, !Info, !Specs),
    SrcDefns = src_defns_inst(MaybeIntDefn, MaybeImpDefn),
    CheckedDefn = checked_defn_inst(StdInstDefn, SrcDefns).

:- pred module_qualify_mode_ctor_checked_defn(
    mode_ctor_checked_defn::in, mode_ctor_checked_defn::out,
    mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

module_qualify_mode_ctor_checked_defn(CheckedDefn0, CheckedDefn,
        !Info, !Specs) :-
    CheckedDefn0 = checked_defn_mode(StdModeDefn0, SrcDefns0),
    StdModeDefn0 = std_mode_defn(Status, MaybeAbstractDefn0),
    % Abstract mode definitions don't need qualifying NOW, but will
    % in the future when we add a new mode_for_type_constructor field.
    InInt = std_mode_status_section(Status),
    module_qualify_item_mode_defn(qualify_mode_defn, InInt,
        MaybeAbstractDefn0, MaybeAbstractDefn, !Info, !Specs),
    StdModeDefn = std_mode_defn(Status, MaybeAbstractDefn),
    SrcDefns0 = src_defns_mode(MaybeIntDefn0, MaybeImpDefn0),
    QualifyPred = module_qualify_item_mode_defn(qualify_mode_defn),
    maybe_qualify_defn(QualifyPred, mq_used_in_interface,
        MaybeIntDefn0, MaybeIntDefn, !Info, !Specs),
    maybe_qualify_defn(QualifyPred, mq_not_used_in_interface,
        MaybeImpDefn0, MaybeImpDefn, !Info, !Specs),
    SrcDefns = src_defns_mode(MaybeIntDefn, MaybeImpDefn),
    CheckedDefn = checked_defn_mode(StdModeDefn, SrcDefns).

:- pred maybe_qualify_defn(
    pred(mq_in_interface, T, T, mq_info, mq_info,
        list(error_spec), list(error_spec))
    :: in(pred(in, in, out, in, out, in, out) is det),
    mq_in_interface::in,
    maybe(T)::in, maybe(T)::out, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

maybe_qualify_defn(QualifyPred, InInt, MaybeDefn0, MaybeDefn, !Info, !Specs) :-
    (
        MaybeDefn0 = no,
        MaybeDefn = no
    ;
        MaybeDefn0 = yes(Defn0),
        QualifyPred(InInt, Defn0, Defn, !Info, !Specs),
        MaybeDefn = yes(Defn)
    ).

%---------------------------------------------------------------------------%

:- func std_eqv_status_section(std_eqv_type_status) = mq_in_interface.

std_eqv_status_section(std_eqv_type_mer_exported) = mq_used_in_interface.
std_eqv_status_section(std_eqv_type_abstract_exported) =
    mq_not_used_in_interface.
std_eqv_status_section(std_eqv_type_all_private) = mq_not_used_in_interface.

:- func std_du_status_section(std_du_type_status) = mq_in_interface.

std_du_status_section(std_du_type_mer_ft_exported) = mq_used_in_interface.
std_du_status_section(std_du_type_mer_exported) = mq_used_in_interface.
std_du_status_section(std_du_type_abstract_exported) =
    mq_not_used_in_interface.
std_du_status_section(std_du_type_all_private) = mq_not_used_in_interface.

:- func std_sub_status_section(std_subtype_status) = mq_in_interface.

std_sub_status_section(std_sub_type_mer_exported) = mq_used_in_interface.
std_sub_status_section(std_sub_type_abstract_exported) =
    mq_not_used_in_interface.
std_sub_status_section(std_sub_type_all_private) = mq_not_used_in_interface.

:- func std_inst_status_section(std_inst_status) = mq_in_interface.

std_inst_status_section(std_inst_exported) = mq_used_in_interface.
std_inst_status_section(std_inst_abstract_exported) = mq_not_used_in_interface.
std_inst_status_section(std_inst_all_private) = mq_not_used_in_interface.

:- func std_mode_status_section(std_mode_status) = mq_in_interface.

std_mode_status_section(std_mode_exported) = mq_used_in_interface.
std_mode_status_section(std_mode_abstract_exported) = mq_not_used_in_interface.
std_mode_status_section(std_mode_all_private) = mq_not_used_in_interface.

%---------------------------------------------------------------------------%

:- pred module_qualify_item_type_defn(
    pred(mq_in_interface, prog_context, type_ctor, T, T,
        mq_info, mq_info, list(error_spec), list(error_spec))
    :: in(pred(in, in, in, in, out, in, out, in, out) is det),
    mq_in_interface::in,
    item_type_defn_info_general(T)::in, item_type_defn_info_general(T)::out,
    mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

module_qualify_item_type_defn(QualDefn, InInt, ItemTypeDefn0, ItemTypeDefn,
        !Info, !Specs) :-
    ItemTypeDefn0 = item_type_defn_info(SymName, Params, TypeDefn0,
        TVarSet, Context, SeqNum),
    list.length(Params, Arity),
    TypeCtor = type_ctor(SymName, Arity),
    QualDefn(InInt, Context, TypeCtor, TypeDefn0, TypeDefn, !Info, !Specs),
    ItemTypeDefn = item_type_defn_info(SymName, Params, TypeDefn,
        TVarSet, Context, SeqNum).

:- pred module_qualify_item_inst_defn(
    pred(mq_in_interface, prog_context, inst_ctor, T, T,
        mq_info, mq_info, list(error_spec), list(error_spec))
    :: in(pred(in, in, in, in, out, in, out, in, out) is det),
    mq_in_interface::in,
    item_inst_defn_info_general(T)::in, item_inst_defn_info_general(T)::out,
    mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

module_qualify_item_inst_defn(QualDefn, InInt, ItemInstDefn0, ItemInstDefn,
        !Info, !Specs) :-
    ItemInstDefn0 = item_inst_defn_info(SymName, Params, MaybeForTypeCtor0,
        MaybeAbstractInstDefn0, InstVarSet, Context, SeqNum),
    list.length(Params, Arity),
    InstCtor = inst_ctor(SymName, Arity),
    ErrorContext = mqec_inst_defn(Context, InstCtor),
    QualDefn(InInt, Context, InstCtor,
        MaybeAbstractInstDefn0, MaybeAbstractInstDefn, !Info, !Specs),
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
        MaybeAbstractInstDefn, InstVarSet, Context, SeqNum).

:- pred module_qualify_item_mode_defn(
    pred(mq_in_interface, prog_context, mode_ctor, T, T,
        mq_info, mq_info, list(error_spec), list(error_spec))
    :: in(pred(in, in, in, in, out, in, out, in, out) is det),
    mq_in_interface::in,
    item_mode_defn_info_general(T)::in, item_mode_defn_info_general(T)::out,
    mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

module_qualify_item_mode_defn(QualDefn, InInt, ItemModeDefn0, ItemModeDefn,
        !Info, !Specs) :-
    ItemModeDefn0 = item_mode_defn_info(SymName, Params,
        MaybeAbstractModeDefn0, InstVarSet, Context, SeqNum),
    list.length(Params, Arity),
    ModeCtor = mode_ctor(SymName, Arity),
    QualDefn(InInt, Context, ModeCtor,
        MaybeAbstractModeDefn0, MaybeAbstractModeDefn, !Info, !Specs),
    ItemModeDefn = item_mode_defn_info(SymName, Params,
        MaybeAbstractModeDefn, InstVarSet, Context, SeqNum).

:- pred module_qualify_item_typeclass(mq_in_interface::in,
    item_typeclass_info::in, item_typeclass_info::out,
    mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

module_qualify_item_typeclass(InInt, ItemTypeClass0, ItemTypeClass,
        !Info, !Specs) :-
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
        ErrorContext = mqec_class(Context, class_id(Name, Arity)),
        qualify_class_decls(InInt, ErrorContext, Methods0, Methods,
            !Info, !Specs),
        Interface = class_interface_concrete(Methods)
    ),
    ItemTypeClass = item_typeclass_info(Name, Vars, Constraints, FunDeps,
        Interface, VarSet, Context, SeqNum).

:- pred module_qualify_item_instance(mq_in_interface::in,
    item_instance_info::in, item_instance_info::out,
    mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

module_qualify_item_instance(InInt, ItemInstance0, ItemInstance,
        !Info, !Specs) :-
    ItemInstance0 = item_instance_info(Name0, Types0, OriginalTypes0,
        Constraints0, Body0, VarSet, ModName, Context, SeqNum),
    list.length(Types0, Arity),
    ErrorContext = mqec_instance(Context, class_id(Name0, Arity)),

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
    Id0 = mq_id(Name0, Arity),
    qualify_class_name(InInt, ErrorContext, Id0, Name, !Info, !Specs),
    % XXX We don't want to keep the errors from the expansion of both
    % forms of the instance types, since printing two error messages about
    % one instance definition that make apparently contradictory
    % assumptions about whether the instance types are equiv-type-expanded
    % or not would be confusing. However, I (zs) cannot think of any
    % compelling reason right now for preferring the error messages
    % from one version of the types over the other.
    qualify_type_list(InInt, ErrorContext, Types0, Types, !Info, !Specs),
    qualify_type_list(InInt, ErrorContext, OriginalTypes0, OriginalTypes,
        !Info, !.Specs, _),
    qualify_instance_body(Name, Body0, Body),
    ItemInstance = item_instance_info(Name, Types, OriginalTypes,
        Constraints, Body, VarSet, ModName, Context, SeqNum).

:- pred module_qualify_item_pred_decl(mq_in_interface::in,
    item_pred_decl_info::in, item_pred_decl_info::out,
    mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

module_qualify_item_pred_decl(InInt, ItemPredDecl0, ItemPredDecl,
        !Info, !Specs) :-
    ItemPredDecl0 = item_pred_decl_info(SymName, PredOrFunc,
        TypesAndModes0, MaybeWithType0, MaybeWithInst0, MaybeDetism,
        Origin, TypeVarSet, InstVarSet, ExistQVars, Purity,
        Constraints0, Context, SeqNum),
    list.length(TypesAndModes0, Arity),
    ErrorContext = mqec_pred_or_func(Context, PredOrFunc,
        mq_id(SymName, Arity)),
    qualify_types_and_modes(InInt, ErrorContext, TypesAndModes0, TypesAndModes,
        !Info, !Specs),
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
        qualify_inst(InInt, ErrorContext, WithInst0, WithInst, !Info, !Specs),
        MaybeWithInst = yes(WithInst)
    ;
        MaybeWithInst0 = no,
        MaybeWithInst = no
    ),
    ItemPredDecl = item_pred_decl_info(SymName, PredOrFunc,
        TypesAndModes, MaybeWithType, MaybeWithInst, MaybeDetism,
        Origin, TypeVarSet, InstVarSet, ExistQVars, Purity,
        Constraints, Context, SeqNum).

:- pred module_qualify_item_mode_decl(mq_in_interface::in,
    item_mode_decl_info::in, item_mode_decl_info::out,
    mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

module_qualify_item_mode_decl(InInt, ItemModeDecl0, ItemModeDecl,
        !Info, !Specs) :-
    ItemModeDecl0 = item_mode_decl_info(SymName, PredOrFunc, Modes0,
        MaybeWithInst0, MaybeDetism, InstVarSet, Context, SeqNum),
    list.length(Modes0, Arity),
    ErrorContext = mqec_pred_or_func_mode(Context, PredOrFunc,
        mq_id(SymName, Arity)),
    qualify_mode_list(InInt, ErrorContext, Modes0, Modes, !Info, !Specs),
    (
        MaybeWithInst0 = yes(WithInst0),
        % XXX We could pass a more specific error context.
        qualify_inst(InInt, ErrorContext, WithInst0, WithInst, !Info, !Specs),
        MaybeWithInst = yes(WithInst)
    ;
        MaybeWithInst0 = no,
        MaybeWithInst = no
    ),
    ItemModeDecl = item_mode_decl_info(SymName, PredOrFunc, Modes,
        MaybeWithInst, MaybeDetism, InstVarSet, Context, SeqNum).

:- pred module_qualify_item_foreign_enum(mq_in_interface::in,
    item_foreign_enum_info::in, item_foreign_enum_info::out,
    mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

module_qualify_item_foreign_enum(InInt, ItemForeignEnum0, ItemForeignEnum,
        !Info, !Specs) :-
    ItemForeignEnum0 = item_foreign_enum_info(Lang, TypeCtor0, Values,
        Context, SeqNum),
    ErrorContext = mqec_foreign_enum(Context),
    qualify_type_ctor(InInt, ErrorContext, TypeCtor0, TypeCtor, !Info, !Specs),
    ItemForeignEnum = item_foreign_enum_info(Lang, TypeCtor, Values,
        Context, SeqNum).

:- pred module_qualify_item_foreign_export_enum(mq_in_interface::in,
    item_foreign_export_enum_info::in, item_foreign_export_enum_info::out,
    mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

module_qualify_item_foreign_export_enum(InInt, ItemFEE0, ItemFEE,
        !Info, !Specs) :-
    ItemFEE0 = item_foreign_export_enum_info(Lang, TypeCtor0, Attributes,
        Overrides, Context, SeqNum),
    ErrorContext = mqec_foreign_export_enum(Context),
    mq_info_get_suppress_found_undef(!.Info, OldSuppressUndef),
    mq_info_set_suppress_found_undef(suppress_found_undef, !Info),
    qualify_type_ctor(InInt, ErrorContext, TypeCtor0, TypeCtor, !Info, !Specs),
    mq_info_set_suppress_found_undef(OldSuppressUndef, !Info),
    ItemFEE = item_foreign_export_enum_info(Lang, TypeCtor, Attributes,
        Overrides, Context, SeqNum).

:- pred module_qualify_item_decl_pragma(mq_in_interface::in,
    item_decl_pragma_info::in, item_decl_pragma_info::out,
    mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

module_qualify_item_decl_pragma(InInt, ItemDeclPragma0, ItemDeclPragma,
        !Info, !Specs) :-
    ItemDeclPragma0 = item_pragma_info(Pragma0, Context, SeqNum),
    qualify_decl_pragma(InInt, Context, Pragma0, Pragma, !Info, !Specs),
    ItemDeclPragma = item_pragma_info(Pragma, Context, SeqNum).

:- pred module_qualify_item_impl_pragma(mq_in_interface::in,
    item_impl_pragma_info::in, item_impl_pragma_info::out,
    mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

module_qualify_item_impl_pragma(InInt, ItemImplPragma0, ItemImplPragma,
        !Info, !Specs) :-
    ItemImplPragma0 = item_pragma_info(Pragma0, Context, SeqNum),
    qualify_impl_pragma(InInt, Context, Pragma0, Pragma, !Info, !Specs),
    ItemImplPragma = item_pragma_info(Pragma, Context, SeqNum).

% Generated pragmas are always generated fully qualified.

:- pred module_qualify_item_type_repn(module_name::in, mq_in_interface::in,
    item_type_repn_info::in, item_type_repn_info::out,
    mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

module_qualify_item_type_repn(ModuleName, InInt,
        ItemTypeRepnInfo0, ItemTypeRepnInfo, !Info, !Specs) :-
    ItemTypeRepnInfo0 = item_type_repn_info(TypeCtorSymName0, ArgTVars,
        RepInfo0, TVarSet, Context, SeqNum),
    % We currently put type constructors names into type_repn items
    % in unqualified form because (a) the module name is implicit in the
    % identity of the module whose interface file the type_repn item
    % appears in, and therefore *can* be omitted; and (b) actually omitting
    % the module qualification makes the interface file smaller. However,
    % since we previously module qualified type constructors names, we accept
    % that form as well.
    ( TypeCtorSymName0 = qualified(_, TypeCtorName)
    ; TypeCtorSymName0 = unqualified(TypeCtorName)
    ),
    TypeCtorSymName = qualified(ModuleName, TypeCtorName),
    (
        ( RepInfo0 = tcrepn_is_word_aligned_ptr
        ; RepInfo0 = tcrepn_du(_)
        ; RepInfo0 = tcrepn_foreign(_)
        ),
        RepInfo = RepInfo0
    ;
        RepInfo0 = tcrepn_is_eqv_to(EqvType0),
        list.length(ArgTVars, TypeCtorArity),
        TypeCtor = type_ctor(TypeCtorSymName, TypeCtorArity),
        ErrorContext = mqec_type_repn(Context, TypeCtor),
        qualify_type(InInt, ErrorContext, EqvType0, EqvType, !Info, !Specs),
        RepInfo = tcrepn_is_eqv_to(EqvType)
    ;
        RepInfo0 = tcrepn_is_subtype_of(SuperTypeCtor0),
        list.length(ArgTVars, TypeCtorArity),
        TypeCtor = type_ctor(TypeCtorSymName, TypeCtorArity),
        ErrorContext = mqec_type_repn(Context, TypeCtor),
        qualify_type_ctor(InInt, ErrorContext, SuperTypeCtor0, SuperTypeCtor,
            !Info, !Specs),
        RepInfo = tcrepn_is_subtype_of(SuperTypeCtor)
    ),
    ItemTypeRepnInfo = item_type_repn_info(TypeCtorSymName, ArgTVars,
        RepInfo, TVarSet, Context, SeqNum).

%---------------------------------------------------------------------------%
%
% Module qualify type definitions and types.
%

    % Qualify the data constructors and/or arguments' types
    % in a type definition.
    %
:- pred qualify_type_defn(mq_in_interface::in, prog_context::in,
    type_ctor::in, type_defn::in, type_defn::out,
    mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_type_defn(InInt, Context, TypeCtor, TypeDefn0, TypeDefn,
        !Info, !Specs) :-
    (
        TypeDefn0 = parse_tree_solver_type(DetailsSolver0),
        qualify_type_defn_solver(InInt, Context, TypeCtor,
            DetailsSolver0, DetailsSolver, !Info, !Specs),
        TypeDefn = parse_tree_solver_type(DetailsSolver)
    ;
        TypeDefn0 = parse_tree_eqv_type(DetailsEqv0),
        qualify_type_defn_eqv(InInt, Context, TypeCtor,
            DetailsEqv0, DetailsEqv, !Info, !Specs),
        TypeDefn = parse_tree_eqv_type(DetailsEqv)
    ;
        TypeDefn0 = parse_tree_du_type(DetailsDu0),
        qualify_type_defn_du(InInt, Context, TypeCtor,
            DetailsDu0, DetailsDu, !Info, !Specs),
        TypeDefn = parse_tree_du_type(DetailsDu)
    ;
        TypeDefn0 = parse_tree_sub_type(DetailsSub0),
        qualify_type_defn_sub(InInt, Context, TypeCtor,
            DetailsSub0, DetailsSub, !Info, !Specs),
        TypeDefn = parse_tree_sub_type(DetailsSub)
    ;
        ( TypeDefn0 = parse_tree_abstract_type(_)
        ; TypeDefn0 = parse_tree_foreign_type(_)
        ),
        TypeDefn = TypeDefn0
    ).

:- pred qualify_type_defn_solver(mq_in_interface::in, prog_context::in,
    type_ctor::in, type_details_solver::in, type_details_solver::out,
    mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_type_defn_solver(InInt, Context, TypeCtor,
        DetailsSolver0, DetailsSolver, !Info, !Specs) :-
    DetailsSolver0 = type_details_solver(SolverTypeDetails0, MaybeUserEqComp),
    SolverTypeDetails0 = solver_type_details(RepnType0, GroundInst0, AnyInst0,
        Mutables0),
    ErrorContext = mqec_type_defn(Context, TypeCtor),
    qualify_type(InInt, ErrorContext, RepnType0, RepnType, !Info, !Specs),
    qualify_inst(InInt, ErrorContext, GroundInst0, GroundInst, !Info, !Specs),
    qualify_inst(InInt, ErrorContext, AnyInst0, AnyInst, !Info, !Specs),
    qualify_constraint_stores(InInt, Mutables0, Mutables, !Info, !Specs),
    SolverTypeDetails  = solver_type_details(RepnType, GroundInst, AnyInst,
        Mutables),
    DetailsSolver = type_details_solver(SolverTypeDetails, MaybeUserEqComp).

:- pred qualify_constraint_stores(mq_in_interface::in,
    list(item_mutable_info)::in, list(item_mutable_info)::out,
    mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_constraint_stores(_InInt, [], [], !Info, !Specs).
qualify_constraint_stores(InInt,
        [Mutable0 | Mutables0], [Mutable | Mutables], !Info, !Specs) :-
    module_qualify_item_mutable(InInt, Mutable0, Mutable, !Info, !Specs),
    qualify_constraint_stores(InInt, Mutables0, Mutables, !Info, !Specs).

:- pred qualify_type_defn_eqv(mq_in_interface::in, prog_context::in,
    type_ctor::in, type_details_eqv::in, type_details_eqv::out,
    mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_type_defn_eqv(InInt, Context, TypeCtor, DetailsEqv0, DetailsEqv,
        !Info, !Specs) :-
    DetailsEqv0 = type_details_eqv(Type0),
    ErrorContext = mqec_type_defn(Context, TypeCtor),
    qualify_type(InInt, ErrorContext, Type0, Type, !Info, !Specs),
    DetailsEqv = type_details_eqv(Type).

:- pred qualify_type_defn_du(mq_in_interface::in, prog_context::in,
    type_ctor::in, type_details_du::in, type_details_du::out,
    mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_type_defn_du(InInt, _Context, TypeCtor, DetailsDu0, DetailsDu,
        !Info, !Specs) :-
    DetailsDu0 = type_details_du(OoMCtors0, MaybeUserEqComp0,
        MaybeDirectArgCtors0),
    OoMCtors0 = one_or_more(HeadCtor0, TailCtors0),
    qualify_constructor(InInt, TypeCtor, HeadCtor0, HeadCtor, !Info, !Specs),
    qualify_constructors(InInt, TypeCtor, TailCtors0, TailCtors,
        !Info, !Specs),
    OoMCtors = one_or_more(HeadCtor, TailCtors),
    % User-defined equality pred names will be converted into predicate
    % calls and then module-qualified after type analysis (during mode
    % analysis). That way, they get full type overloading resolution, etc.
    % Thus we don't module-qualify them here.
    MaybeUserEqComp = MaybeUserEqComp0,
    MaybeDirectArgCtors = MaybeDirectArgCtors0,
    DetailsDu = type_details_du(OoMCtors, MaybeUserEqComp,
        MaybeDirectArgCtors).

:- pred qualify_type_defn_sub(mq_in_interface::in, prog_context::in,
    type_ctor::in, type_details_sub::in, type_details_sub::out,
    mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_type_defn_sub(InInt, Context, TypeCtor, DetailsSub0, DetailsSub,
        !Info, !Specs) :-
    DetailsSub0 = type_details_sub(SuperType0, OoMCtors0),
    ErrorContext = mqec_type_defn(Context, TypeCtor),
    % Note that this will not prevent a subtype defined in an interface
    % section from referring to an abstract type as its supertype.
    % That will be checked while checking other subtype conditions.
    qualify_type(InInt, ErrorContext, SuperType0, SuperType,
        !Info, !Specs),
    OoMCtors0 = one_or_more(HeadCtor0, TailCtors0),
    qualify_constructor(InInt, TypeCtor, HeadCtor0, HeadCtor, !Info, !Specs),
    qualify_constructors(InInt, TypeCtor, TailCtors0, TailCtors,
        !Info, !Specs),
    OoMCtors = one_or_more(HeadCtor, TailCtors),
    DetailsSub = type_details_sub(SuperType, OoMCtors).

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
% Module qualify inst definitions and insts.
%

    % Qualify the right hand side of an inst definition.
    %
:- pred qualify_inst_defn(mq_in_interface::in, prog_context::in,
    inst_ctor::in, maybe_abstract_inst_defn::in, maybe_abstract_inst_defn::out,
    mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_inst_defn(InInt, Context, InstCtor,
        MaybeAbstractInstDefn0, MaybeAbstractInstDefn,
        !Info, !Specs) :-
    (
        MaybeAbstractInstDefn0 = abstract_inst_defn,
        MaybeAbstractInstDefn = abstract_inst_defn
    ;
        MaybeAbstractInstDefn0 = nonabstract_inst_defn(InstDefn0),
        ErrorContext = mqec_inst_defn(Context, InstCtor),
        InstDefn0 = eqv_inst(Inst0),
        qualify_inst(InInt, ErrorContext, Inst0, Inst, !Info, !Specs),
        InstDefn = eqv_inst(Inst),
        MaybeAbstractInstDefn = nonabstract_inst_defn(InstDefn)
    ).

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
        Id = recomp_item_name(Name, Arity),
        update_recompilation_info(
            recompilation.record_used_item(used_functor, Id, Id), !Info)
    ;
        ( ConsId = tuple_cons(_)
        ; ConsId = closure_cons(_, _)
        ; ConsId = some_int_const(_)
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
% Module qualify mode definitions and modes.
%

    % Qualify the right hand side of a mode definition.
    %
:- pred qualify_mode_defn(mq_in_interface::in, prog_context::in,
    mode_ctor::in, maybe_abstract_mode_defn::in, maybe_abstract_mode_defn::out,
    mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_mode_defn(InInt, Context, ModeCtor,
        MaybeAbstractModeDefn0, MaybeAbstractModeDefn,
        !Info, !Specs) :-
    (
        MaybeAbstractModeDefn0 = abstract_mode_defn,
        MaybeAbstractModeDefn = abstract_mode_defn
    ;
        MaybeAbstractModeDefn0 = nonabstract_mode_defn(ModeDefn0),
        ErrorContext = mqec_mode_defn(Context, ModeCtor),
        ModeDefn0 = eqv_mode(Mode0),
        qualify_mode(InInt, ErrorContext, Mode0, Mode, !Info, !Specs),
        ModeDefn = eqv_mode(Mode),
        MaybeAbstractModeDefn = nonabstract_mode_defn(ModeDefn)
    ).

qualify_mode_list(_InInt, _ErrorContext, [], [], !Info, !Specs).
qualify_mode_list(InInt, ErrorContext, [Mode0 | Modes0], [Mode | Modes],
        !Info, !Specs) :-
    qualify_mode(InInt, ErrorContext, Mode0, Mode, !Info, !Specs),
    qualify_mode_list(InInt, ErrorContext, Modes0, Modes, !Info, !Specs).

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
% Module qualify pragmas.
%

:- pred qualify_decl_pragma(mq_in_interface::in, prog_context::in,
    decl_pragma::in, decl_pragma::out, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_decl_pragma(InInt, Context, Pragma0, Pragma, !Info, !Specs) :-
    (
        Pragma0 = decl_pragma_type_spec(TypeSpecInfo0),
        TypeSpecInfo0 = pragma_info_type_spec(PFUMM0, PredName, SpecPredName,
            Subst0, TVarSet, Items),
        ErrorContext = mqec_pragma_decl(Context, Pragma0),
        (
            PFUMM0 = pfumm_predicate(ModesOrArity0),
            (
                ModesOrArity0 = moa_modes(Modes0),
                qualify_mode_list(InInt, ErrorContext, Modes0, Modes,
                    !Info, !Specs),
                ModesOrArity = moa_modes(Modes),
                PFUMM = pfumm_predicate(ModesOrArity)
            ;
                ModesOrArity0 = moa_arity(_Arity),
                PFUMM = PFUMM0
            )
        ;
            PFUMM0 = pfumm_function(ModesOrArity0),
            (
                ModesOrArity0 = moa_modes(Modes0),
                qualify_mode_list(InInt, ErrorContext, Modes0, Modes,
                    !Info, !Specs),
                ModesOrArity = moa_modes(Modes),
                PFUMM = pfumm_function(ModesOrArity)
            ;
                ModesOrArity0 = moa_arity(_Arity),
                PFUMM = PFUMM0
            )
        ;
            PFUMM0 = pfumm_unknown(_Arity),
            PFUMM = PFUMM0
        ),
        Subst0 = one_or_more(HeadSubst0, TailSubsts0),
        qualify_type_spec_subst(InInt, ErrorContext,
            HeadSubst0, HeadSubst, TailSubsts0, TailSubsts, !Info, !Specs),
        Subst = one_or_more(HeadSubst, TailSubsts),
        TypeSpecInfo = pragma_info_type_spec(PFUMM, PredName, SpecPredName,
            Subst, TVarSet, Items),
        Pragma = decl_pragma_type_spec(TypeSpecInfo)
    ;
        Pragma0 = decl_pragma_oisu(OISUInfo0),
        OISUInfo0 = pragma_info_oisu(TypeCtor0, CreatorPreds,
            MutatorPreds, DestructorPreds),
        % XXX Preds
        ErrorContext = mqec_pragma_decl(Context, Pragma0),
        qualify_type_ctor(InInt, ErrorContext, TypeCtor0, TypeCtor,
            !Info, !Specs),
        OISUInfo = pragma_info_oisu(TypeCtor, CreatorPreds,
            MutatorPreds, DestructorPreds),
        Pragma = decl_pragma_oisu(OISUInfo)
    ;
        Pragma0 = decl_pragma_termination_info(TermInfo0),
        TermInfo0 = pragma_info_termination_info(PredNameModesPF0, Args, Term),
        PredNameModesPF0 = proc_pf_name_modes(PredOrFunc, SymName, Modes0),
        ErrorContext = mqec_pragma_decl(Context, Pragma0),
        qualify_mode_list(InInt, ErrorContext, Modes0, Modes, !Info, !Specs),
        PredNameModesPF = proc_pf_name_modes(PredOrFunc, SymName, Modes),
        TermInfo = pragma_info_termination_info(PredNameModesPF, Args, Term),
        Pragma = decl_pragma_termination_info(TermInfo)
    ;
        Pragma0 = decl_pragma_termination2_info(Term2Info0),
        Term2Info0 = pragma_info_termination2_info(PredNameModesPF0,
            SuccessArgs, FailureArgs, Term),
        PredNameModesPF0 = proc_pf_name_modes(PredOrFunc, SymName, Modes0),
        ErrorContext = mqec_pragma_decl(Context, Pragma0),
        qualify_mode_list(InInt, ErrorContext, Modes0, Modes, !Info, !Specs),
        PredNameModesPF = proc_pf_name_modes(PredOrFunc, SymName, Modes),
        Term2Info = pragma_info_termination2_info(PredNameModesPF,
            SuccessArgs, FailureArgs, Term),
        Pragma = decl_pragma_termination2_info(Term2Info)
    ;
        Pragma0 = decl_pragma_structure_sharing(SharingInfo0),
        SharingInfo0 = pragma_info_structure_sharing(PredNameModesPF0,
            HeadVars, HeadVarTypes, VarSet, TVarSet, MaybeSharing),
        PredNameModesPF0 = proc_pf_name_modes(PredOrFunc, SymName, Modes0),
        ErrorContext = mqec_pragma_decl(Context, Pragma0),
        qualify_mode_list(InInt, ErrorContext, Modes0, Modes, !Info, !Specs),
        PredNameModesPF = proc_pf_name_modes(PredOrFunc, SymName, Modes),
        SharingInfo = pragma_info_structure_sharing(PredNameModesPF,
            HeadVars, HeadVarTypes, VarSet, TVarSet, MaybeSharing),
        Pragma = decl_pragma_structure_sharing(SharingInfo)
    ;
        Pragma0 = decl_pragma_structure_reuse(ReuseInfo0),
        ReuseInfo0 = pragma_info_structure_reuse(PredNameModesPF0,
            HeadVars, HeadVarTypes, VarSet, TVarSet, MaybeReuse),
        PredNameModesPF0 = proc_pf_name_modes(PredOrFunc, SymName, Modes0),
        ErrorContext = mqec_pragma_decl(Context, Pragma0),
        qualify_mode_list(InInt, ErrorContext, Modes0, Modes, !Info, !Specs),
        PredNameModesPF = proc_pf_name_modes(PredOrFunc, SymName, Modes),
        ReuseInfo = pragma_info_structure_reuse(PredNameModesPF,
            HeadVars, HeadVarTypes, VarSet, TVarSet, MaybeReuse),
        Pragma = decl_pragma_structure_reuse(ReuseInfo)
    ;
        ( Pragma0 = decl_pragma_obsolete_pred(_)
        ; Pragma0 = decl_pragma_obsolete_proc(_)
        ; Pragma0 = decl_pragma_format_call(_)
        ; Pragma0 = decl_pragma_terminates(_)
        ; Pragma0 = decl_pragma_does_not_terminate(_)
        ; Pragma0 = decl_pragma_check_termination(_)
        ),
        Pragma = Pragma0
    ).

:- pred qualify_impl_pragma(mq_in_interface::in, prog_context::in,
    impl_pragma::in, impl_pragma::out, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_impl_pragma(InInt, Context, Pragma0, Pragma, !Info, !Specs) :-
    (
        ( Pragma0 = impl_pragma_foreign_decl(_)
        ; Pragma0 = impl_pragma_foreign_code(_)
        ; Pragma0 = impl_pragma_external_proc(_)
          % The predicate name in the pragma_external_proc is constructed
          % already qualified.
        ; Pragma0 = impl_pragma_inline(_)
        ; Pragma0 = impl_pragma_no_inline(_)
        ; Pragma0 = impl_pragma_consider_used(_)
        ; Pragma0 = impl_pragma_no_detism_warning(_)
        ; Pragma0 = impl_pragma_require_tail_rec(_)
        ; Pragma0 = impl_pragma_fact_table(_)
        ; Pragma0 = impl_pragma_promise_pure(_)
        ; Pragma0 = impl_pragma_promise_semipure(_)
        ; Pragma0 = impl_pragma_promise_eqv_clauses(_)
        ; Pragma0 = impl_pragma_mode_check_clauses(_)
        ; Pragma0 = impl_pragma_require_feature_set(_)
        ),
        Pragma = Pragma0
    ;
        Pragma0 = impl_pragma_foreign_proc(FPInfo0),
        FPInfo0 = pragma_info_foreign_proc(Attrs0, Name, PredOrFunc,
            Vars0, Varset, InstVarset, Impl),
        ErrorContext = mqec_pragma_impl(Context, Pragma0),
        qualify_pragma_vars(InInt, ErrorContext, Vars0, Vars, !Info, !Specs),
        UserSharing0 = get_user_annotated_sharing(Attrs0),
        qualify_user_sharing(InInt, ErrorContext, UserSharing0, UserSharing,
            !Info, !Specs),
        set_user_annotated_sharing(UserSharing, Attrs0, Attrs),
        FPInfo = pragma_info_foreign_proc(Attrs, Name, PredOrFunc,
            Vars, Varset, InstVarset, Impl),
        Pragma = impl_pragma_foreign_proc(FPInfo)
    ;
        Pragma0 = impl_pragma_tabled(TabledInfo0),
        TabledInfo0 = pragma_info_tabled(EvalMethod, PredOrProcSpec0, Attrs),
        PredOrProcSpec0 = pred_or_proc_pfumm_name(PFUMM0, PredSymName),
        (
            PFUMM0 = pfumm_predicate(ModesOrArity0),
            (
                ModesOrArity0 = moa_modes(Modes0),
                ErrorContext = mqec_pragma_impl(Context, Pragma0),
                qualify_mode_list(InInt, ErrorContext, Modes0, Modes,
                    !Info, !Specs),
                ModesOrArity = moa_modes(Modes),
                PFUMM = pfumm_predicate(ModesOrArity)
            ;
                ModesOrArity0 = moa_arity(_Arity),
                PFUMM = PFUMM0
            )
        ;
            PFUMM0 = pfumm_function(ModesOrArity0),
            (
                ModesOrArity0 = moa_modes(Modes0),
                ErrorContext = mqec_pragma_impl(Context, Pragma0),
                qualify_mode_list(InInt, ErrorContext, Modes0, Modes,
                    !Info, !Specs),
                ModesOrArity = moa_modes(Modes),
                PFUMM = pfumm_function(ModesOrArity)
            ;
                ModesOrArity0 = moa_arity(_Arity),
                PFUMM = PFUMM0
            )
        ;
            PFUMM0 = pfumm_unknown(_Arity),
            PFUMM = PFUMM0
        ),
        PredOrProcSpec = pred_or_proc_pfumm_name(PFUMM, PredSymName),
        TabledInfo = pragma_info_tabled(EvalMethod, PredOrProcSpec, Attrs),
        Pragma = impl_pragma_tabled(TabledInfo)
    ;
        Pragma0 = impl_pragma_foreign_proc_export(FPEInfo0),
        FPEInfo0 = pragma_info_foreign_proc_export(Origin, Lang,
            PredNameModesPF0, CFunc, VarSet),
        PredNameModesPF0 = proc_pf_name_modes(PredOrFunc, Name, Modes0),
        ErrorContext = mqec_pragma_impl(Context, Pragma0),
        qualify_mode_list(InInt, ErrorContext, Modes0, Modes,
            !Info, !Specs),
        PredNameModesPF = proc_pf_name_modes(PredOrFunc, Name, Modes),
        FPEInfo = pragma_info_foreign_proc_export(Origin, Lang,
            PredNameModesPF, CFunc, VarSet),
        Pragma = impl_pragma_foreign_proc_export(FPEInfo)
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
    pair(tvar, mer_type)::in, pair(tvar, mer_type)::out,
    assoc_list(tvar, mer_type)::in, assoc_list(tvar, mer_type)::out,
    mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_type_spec_subst(InInt, ErrorContext,
        HeadSubst0, HeadSubst, TailSubsts0, TailSubsts, !Info, !Specs) :-
    HeadSubst0 = Var - Type0,
    % XXX We could pass a more specific error context.
    qualify_type(InInt, ErrorContext, Type0, Type, !Info, !Specs),
    HeadSubst = Var - Type,
    (
        TailSubsts0 = [],
        TailSubsts = []
    ;
        TailSubsts0 = [HeadTailSubst0 | TailTailSubsts0],
        qualify_type_spec_subst(InInt, ErrorContext,
            HeadTailSubst0, HeadTailSubst, TailTailSubsts0, TailTailSubsts,
            !Info, !Specs),
        TailSubsts = [HeadTailSubst | TailTailSubsts]
    ).

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
% Module qualify the definitions of mutables.
%

:- pred module_qualify_item_mutable(mq_in_interface::in,
    item_mutable_info::in, item_mutable_info::out, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

module_qualify_item_mutable(InInt, ItemMutable0, ItemMutable, !Info, !Specs) :-
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
