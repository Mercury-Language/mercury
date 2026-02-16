%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2012 The University of Melbourne.
% Copyright (C) 2014-2026 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: build_eqv_maps.m.
%
% This module serves equiv_type.m by gathering the definitions
% of all the equivalence types and equivalence insts
% that may be used in the module being compiled.
%
%---------------------------------------------------------------------------%

:- module parse_tree.build_eqv_maps.
:- interface.

:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_parse_tree.

:- import_module list.
:- import_module map.

%---------------------------------------------------------------------------%

:- type eqv_type_body
    --->    eqv_type_body(
                tvarset,
                list(type_param),
                mer_type
            ).

:- type eqv_inst_body
    --->    eqv_inst_body(
                list(inst_var),
                mer_inst
            ).

:- type type_eqv_map == map(type_ctor, eqv_type_body).
:- type inst_eqv_map == map(inst_ctor, eqv_inst_body).

:- pred build_eqv_maps_in_aug_comp_unit(aug_compilation_unit::in,
    type_eqv_map::out, inst_eqv_map::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_item.
:- import_module recompilation.
:- import_module recompilation.item_types.

:- import_module maybe.
:- import_module one_or_more.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

build_eqv_maps_in_aug_comp_unit(AugCompUnit, !:TypeEqvMap, !:InstEqvMap) :-
    AugCompUnit = aug_compilation_unit(ParseTreeModuleSrc,
        AncestorIntSpecs, DirectInt1Specs, IndirectInt2Specs,
        PlainOpts, TransOpts, IntForOptSpecs, _TypeRepnSpecs,
        _ModuleVersionNumbers),
    map.init(!:TypeEqvMap),
    map.init(!:InstEqvMap),
    build_eqv_maps_in_parse_tree_module_src(ParseTreeModuleSrc,
        !TypeEqvMap, !InstEqvMap),
    map.foldl2_values(build_eqv_maps_in_ancestor_int_spec,
        AncestorIntSpecs,
        !TypeEqvMap, !InstEqvMap),
    map.foldl2_values(build_eqv_maps_in_direct_int1_spec,
        DirectInt1Specs,
        !TypeEqvMap, !InstEqvMap),
    map.foldl2_values(build_eqv_maps_in_indirect_int2_spec,
        IndirectInt2Specs,
        !TypeEqvMap, !InstEqvMap),
    map.foldl2_values(build_eqv_maps_in_parse_tree_plain_opt, PlainOpts,
        !TypeEqvMap, !InstEqvMap),
    map.foldl2_values(build_eqv_maps_in_parse_tree_trans_opt, TransOpts,
        !TypeEqvMap, !InstEqvMap),
    map.foldl2_values(build_eqv_maps_in_int_for_opt_spec, IntForOptSpecs,
        !TypeEqvMap, !InstEqvMap).

:- pred build_eqv_maps_in_parse_tree_module_src(parse_tree_module_src::in,
    type_eqv_map::in, type_eqv_map::out,
    inst_eqv_map::in, inst_eqv_map::out) is det.

build_eqv_maps_in_parse_tree_module_src(ParseTreeModuleSrc,
        !TypeEqvMap, !InstEqvMap) :-
    map.foldl(build_eqv_maps_in_type_ctor_checked_defns_int_imp,
        ParseTreeModuleSrc ^ ptms_type_defns, !TypeEqvMap),
    map.foldl(build_eqv_maps_in_inst_ctor_checked_defns_int_imp,
        ParseTreeModuleSrc ^ ptms_inst_defns, !InstEqvMap).

%---------------------%

:- pred build_eqv_maps_in_ancestor_int_spec(ancestor_int_spec::in,
    type_eqv_map::in, type_eqv_map::out,
    inst_eqv_map::in, inst_eqv_map::out) is det.

build_eqv_maps_in_ancestor_int_spec(AncestorIntSpec,
        !TypeEqvMap, !InstEqvMap) :-
    AncestorIntSpec = ancestor_int0(ParseTreeInt0, ReadWhy0),
    build_eqv_maps_in_parse_tree_int0(ReadWhy0, ParseTreeInt0,
        !TypeEqvMap, !InstEqvMap).

:- pred build_eqv_maps_in_direct_int1_spec(direct_int1_spec::in,
    type_eqv_map::in, type_eqv_map::out,
    inst_eqv_map::in, inst_eqv_map::out) is det.

build_eqv_maps_in_direct_int1_spec(DirectIntSpec,
        !TypeEqvMap, !InstEqvMap) :-
    DirectIntSpec = direct_int1(ParseTreeInt1, ReadWhy1),
    build_eqv_maps_in_parse_tree_int1(ReadWhy1, ParseTreeInt1,
        !TypeEqvMap, !InstEqvMap).

:- pred build_eqv_maps_in_indirect_int2_spec(indirect_int2_spec::in,
    type_eqv_map::in, type_eqv_map::out,
    inst_eqv_map::in, inst_eqv_map::out) is det.

build_eqv_maps_in_indirect_int2_spec(IndirectIntSpec,
        !TypeEqvMap, !InstEqvMap) :-
    IndirectIntSpec = indirect_int2(ParseTreeInt2, ReadWhy2),
    build_eqv_maps_in_parse_tree_int2(ReadWhy2, ParseTreeInt2,
        !TypeEqvMap, !InstEqvMap).

:- pred build_eqv_maps_in_int_for_opt_spec(int_for_opt_spec::in,
    type_eqv_map::in, type_eqv_map::out,
    inst_eqv_map::in, inst_eqv_map::out) is det.

build_eqv_maps_in_int_for_opt_spec(IntForOptSpec, !TypeEqvMap, !InstEqvMap) :-
    (
        IntForOptSpec = for_opt_int0(ParseTreeInt0, ReadWhy0),
        build_eqv_maps_in_parse_tree_int0(ReadWhy0, ParseTreeInt0,
            !TypeEqvMap, !InstEqvMap)
    ;
        IntForOptSpec = for_opt_int1(ParseTreeInt1, ReadWhy1),
        build_eqv_maps_in_parse_tree_int1(ReadWhy1, ParseTreeInt1,
            !TypeEqvMap, !InstEqvMap)
    ;
        IntForOptSpec = for_opt_int2(ParseTreeInt2, ReadWhy2),
        build_eqv_maps_in_parse_tree_int2(ReadWhy2, ParseTreeInt2,
            !TypeEqvMap, !InstEqvMap)
    ).

%---------------------%

:- pred build_eqv_maps_in_parse_tree_int0(read_why_int0::in,
    parse_tree_int0::in,
    type_eqv_map::in, type_eqv_map::out,
    inst_eqv_map::in, inst_eqv_map::out) is det.

build_eqv_maps_in_parse_tree_int0(_ReadWhy0, ParseTreeInt0,
        !TypeEqvMap, !InstEqvMap) :-
    % All possible values of _ReadWhy0 call for things in both
    % the interface and the implementation sections to be imported
    % in a non-abstract form.
    map.foldl(build_eqv_maps_in_type_ctor_checked_defns_int_imp,
        ParseTreeInt0 ^ pti0_type_defns, !TypeEqvMap),
    map.foldl(build_eqv_maps_in_inst_ctor_checked_defns_int_imp,
        ParseTreeInt0 ^ pti0_inst_defns, !InstEqvMap).

:- pred build_eqv_maps_in_parse_tree_int1(read_why_int1::in,
    parse_tree_int1::in,
    type_eqv_map::in, type_eqv_map::out,
    inst_eqv_map::in, inst_eqv_map::out) is det.

build_eqv_maps_in_parse_tree_int1(_ReadWhy1, ParseTreeInt1,
        !TypeEqvMap, !InstEqvMap) :-
    % All possible values of _ReadWhy1 call for things in the interface section
    % to be imported in a non-abstract form, and for things in the
    % implementation section to be imported in an abstract form.
    map.foldl(build_eqv_maps_in_type_ctor_checked_defns_int,
        ParseTreeInt1 ^ pti1_type_defns, !TypeEqvMap),
    % Do not allow the expansion of abstract-imported type definitions.
    % list.foldl(build_eqv_maps_in_type_ctor_all_defns,
    %     map.values(ParseTreeInt1 ^ pti1_imp_type_defns), !TypeEqvMap),
    map.foldl(build_eqv_maps_in_inst_ctor_checked_defns_int,
        ParseTreeInt1 ^ pti1_inst_defns, !InstEqvMap).

:- pred build_eqv_maps_in_parse_tree_int2(read_why_int2::in,
    parse_tree_int2::in,
    type_eqv_map::in, type_eqv_map::out,
    inst_eqv_map::in, inst_eqv_map::out) is det.

build_eqv_maps_in_parse_tree_int2(ReadWhy2, ParseTreeInt2,
        !TypeEqvMap, !InstEqvMap) :-
    % Some values of ReadWhy2 call for things in the interface section
    % to be imported in a non-abstract form, while others call for them
    % to be imported in an abstract form.
    %
    % All possible values of ReadWhy2 call for things in the implementation
    % section to be imported in an abstract form.
    (
        ReadWhy2 = rwi2_abstract
    ;
        ( ReadWhy2 = rwi2_int_use
        ; ReadWhy2 = rwi2_imp_use
        ; ReadWhy2 = rwi2_opt
        ),
        map.foldl(build_eqv_maps_in_type_ctor_checked_defns_int,
            ParseTreeInt2 ^ pti2_type_defns, !TypeEqvMap),
        map.foldl(build_eqv_maps_in_inst_ctor_checked_defns_int,
            ParseTreeInt2 ^ pti2_inst_defns, !InstEqvMap)
    ).

%---------------------%

:- pred build_eqv_maps_in_parse_tree_plain_opt(parse_tree_plain_opt::in,
    type_eqv_map::in, type_eqv_map::out,
    inst_eqv_map::in, inst_eqv_map::out) is det.

build_eqv_maps_in_parse_tree_plain_opt(ParseTreePlainOpt,
        !TypeEqvMap, !InstEqvMap) :-
    list.foldl(build_eqv_maps_in_type_defn,
        ParseTreePlainOpt ^ ptpo_type_defns, !TypeEqvMap),
    list.foldl(build_eqv_maps_in_inst_defn,
        ParseTreePlainOpt ^ ptpo_inst_defns, !InstEqvMap).

:- pred build_eqv_maps_in_parse_tree_trans_opt(parse_tree_trans_opt::in,
    type_eqv_map::in, type_eqv_map::out,
    inst_eqv_map::in, inst_eqv_map::out) is det.

build_eqv_maps_in_parse_tree_trans_opt(_ParseTreePlainOpt,
        TypeEqvMap, TypeEqvMap, InstEqvMap, InstEqvMap).
    % .trans_opt files can contain neither type nor inst definitions.

%---------------------%

:- pred build_eqv_maps_in_type_defn(item_type_defn_info::in,
    type_eqv_map::in, type_eqv_map::out) is det.

build_eqv_maps_in_type_defn(ItemTypeDefn, !TypeEqvMap) :-
    ItemTypeDefn = item_type_defn_info(Name, TypeParams, TypeDefn,
        TVarSet, _Context, _SeqNum),
    ( if TypeDefn = parse_tree_eqv_type(type_details_eqv(EqvType)) then
        list.length(TypeParams, Arity),
        TypeCtor = type_ctor(Name, Arity),
        map.set(TypeCtor, eqv_type_body(TVarSet, TypeParams, EqvType),
            !TypeEqvMap)
    else
        true
    ).

:- pred build_eqv_maps_in_type_ctor_checked_defns_int_imp(type_ctor::in,
    type_ctor_checked_defn::in,
    type_eqv_map::in, type_eqv_map::out) is det.

build_eqv_maps_in_type_ctor_checked_defns_int_imp(TypeCtor, CheckedDefn,
        !TypeEqvMap) :-
    (
        CheckedDefn = checked_defn_solver(_, _)
    ;
        CheckedDefn = checked_defn_std(StdTypeDefn, _SrcDefns),
        (
            StdTypeDefn = std_mer_type_eqv(_Status, ItemTypeDefnEqv),
            ItemTypeDefnEqv = item_type_defn_info(_Name, TypeParams,
                TypeDefn, TVarSet, _Context, _SeqNum),
            TypeDefn = type_details_eqv(EqvType),
            map.set(TypeCtor, eqv_type_body(TVarSet, TypeParams, EqvType),
                !TypeEqvMap)
        ;
            ( StdTypeDefn = std_mer_type_subtype(_, _)
            ; StdTypeDefn = std_mer_type_du_all_plain_constants(_, _, _, _, _)
            ; StdTypeDefn = std_mer_type_du_not_all_plain_constants(_, _, _)
            ; StdTypeDefn = std_mer_type_abstract(_, _, _)
            )
        )
    ).

:- pred build_eqv_maps_in_type_ctor_checked_defns_int(type_ctor::in,
    type_ctor_checked_defn::in,
    type_eqv_map::in, type_eqv_map::out) is det.

build_eqv_maps_in_type_ctor_checked_defns_int(TypeCtor, CheckedDefn,
        !TypeEqvMap) :-
    (
        CheckedDefn = checked_defn_solver(_, _)
    ;
        CheckedDefn = checked_defn_std(StdTypeDefn, _SrcDefns),
        (
            StdTypeDefn = std_mer_type_eqv(Status, ItemTypeDefnEqv),
            (
                Status = std_eqv_type_mer_exported,
                ItemTypeDefnEqv = item_type_defn_info(_Name, TypeParams,
                    TypeDefn, TVarSet, _Context, _SeqNum),
                TypeDefn = type_details_eqv(EqvType),
                map.set(TypeCtor, eqv_type_body(TVarSet, TypeParams, EqvType),
                    !TypeEqvMap)
            ;
                ( Status = std_eqv_type_abstract_exported
                ; Status = std_eqv_type_all_private
                )
            )
        ;
            ( StdTypeDefn = std_mer_type_subtype(_, _)
            ; StdTypeDefn = std_mer_type_du_all_plain_constants(_, _, _, _, _)
            ; StdTypeDefn = std_mer_type_du_not_all_plain_constants(_, _, _)
            ; StdTypeDefn = std_mer_type_abstract(_, _, _)
            )
        )
    ).

%---------------------%

:- pred build_eqv_maps_in_inst_defn(item_inst_defn_info::in,
    inst_eqv_map::in, inst_eqv_map::out) is det.

build_eqv_maps_in_inst_defn(ItemInstDefn, !InstEqvMap) :-
    ItemInstDefn = item_inst_defn_info(Name, InstParams, _IFTC,
        InstDefn, _InstVarSet, _Context, _SeqNum),
    ( if InstDefn = nonabstract_inst_defn(eqv_inst(EqvInst)) then
        list.length(InstParams, Arity),
        InstCtor = inst_ctor(Name, Arity),
        map.set(InstCtor, eqv_inst_body(InstParams, EqvInst), !InstEqvMap)
    else
        true
    ).

:- pred build_eqv_maps_in_inst_ctor_checked_defns_int_imp(inst_ctor::in,
    inst_ctor_checked_defn::in,
    inst_eqv_map::in, inst_eqv_map::out) is det.

build_eqv_maps_in_inst_ctor_checked_defns_int_imp(InstCtor, CheckedDefn,
        !InstEqvMap) :-
    CheckedDefn = checked_defn_inst(StdInstDefn, _SrcDefns),
    StdInstDefn = std_inst_defn(_Status, ItemInstDefn),
    ItemInstDefn = item_inst_defn_info(_Name, InstParams, _MaybeForType,
        MaybeAbstractInstDefn, _InstVarSet, _Context, _SeqNum),
    (
        MaybeAbstractInstDefn = abstract_inst_defn
    ;
        MaybeAbstractInstDefn = nonabstract_inst_defn(InstDefn),
        InstDefn = eqv_inst(EqvInst),
        map.set(InstCtor, eqv_inst_body(InstParams, EqvInst), !InstEqvMap)
    ).

:- pred build_eqv_maps_in_inst_ctor_checked_defns_int(inst_ctor::in,
    inst_ctor_checked_defn::in,
    inst_eqv_map::in, inst_eqv_map::out) is det.

build_eqv_maps_in_inst_ctor_checked_defns_int(InstCtor, CheckedDefn,
        !InstEqvMap) :-
    CheckedDefn = checked_defn_inst(StdInstDefn, _SrcDefns),
    StdInstDefn = std_inst_defn(Status, ItemInstDefn),
    ItemInstDefn = item_inst_defn_info(_Name, InstParams, _MaybeForType,
        MaybeAbstractInstDefn, _InstVarSet, _Context, _SeqNum),
    (
        MaybeAbstractInstDefn = abstract_inst_defn
    ;
        MaybeAbstractInstDefn = nonabstract_inst_defn(InstDefn),
        (
            Status = std_inst_exported,
            InstDefn = eqv_inst(EqvInst),
            map.set(InstCtor, eqv_inst_body(InstParams, EqvInst), !InstEqvMap)
        ;
            ( Status = std_inst_abstract_exported
            ; Status = std_inst_all_private
            )
        )
    ).

%---------------------------------------------------------------------------%
:- end_module parse_tree.build_eqv_maps.
%---------------------------------------------------------------------------%
