%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2012 The University of Melbourne.
% Copyright (C) 2014-2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: equiv_type.m.
% Main author: fjh.
%
% This module contains a parse-tree to parse-tree transformation
% that expands equivalence types. It also expands away `with_type`
% and `with_inst` annotations on predicate and function type declarations.
%
%---------------------------------------------------------------------------%
%
% XXX We do not currently expand out inst definitions.
%
% If inst i1's body contains inst i2, and i2 has been defined to be equivalent
% to some other inst i3, then we *could* replace i2 with i3 in i1's body.
% Instead of doing this once for this user-defined inst, we do it on every use
% of this inst. This is significantly less efficient, but if there is
% any error that involves this inst, the error message we generate will refer
% to the inst by the name the user gave it. If the user e.g. wrote an inst i1
% in a mode declaration, but an error message about that mode declaration
% referred to the expanded form of i1, this would be confusing to many
% programmers. Most likely, it would also be harder to read, since
% inst names are almost always shorter than the insts they are defined
% to be equivalent to.
%
% XXX INST_FOR_TYPE_CONSTRUCTOR
% If inst i1 is for type t2, and t2 has been defined to be equivalent
% to type t3, then we SHOULD record that i1 is really for t3.
% However, while t2 is required to be just a type_ctor and arity,
% t3 may be more complex. The obvious thing to do would be to record that
% i1 is for t3's top type_ctor and its arity. Whether that is good enough
% depends on what *exactly* we will do with the "inst for type ctor"
% information. We don't yet know the answer to that question.
% XXX This should allow us to fix Mantis bug #89.
%
% XXX We do not currently expand out mode definitions either,
% even though the first paragraph above definitely applies to them as well,
% and if we ever extend the language to allow (and maybe even require)
% programmers to record "mode for type constructor" information,
% the second paragraph will apply as well.
%
%---------------------------------------------------------------------------%
%
% XXX We do not currently expand out clauses.
% This will leave any with_type annotations in clauses unexpanded.
% XXX This applies both to clauses that define predicates and functions,
% and to clauses that define instance methods.
%
%---------------------------------------------------------------------------%
%
% XXX A big comment on an commented-out import of hlds.pred_table
% in hlds_out_module.m explores in detail a problem with the operation
% of this module.
%
%---------------------------------------------------------------------------%

:- module parse_tree.equiv_type.
:- interface.

:- import_module libs.
:- import_module libs.maybe_util.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_event.
:- import_module parse_tree.prog_data_used_modules.
:- import_module parse_tree.prog_item.
:- import_module parse_tree.prog_parse_tree.
:- import_module recompilation.
:- import_module recompilation.record_uses.

:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module one_or_more.

%---------------------------------------------------------------------------%

    % expand_eqv_types_insts(!AugCompUnit, !EventSpecMap,
    %   CircularTypes, TypeEqvMap, !MaybeRecompInfo, Specs):
    %
    % This predicate finds all type and inst declarations that define a type
    % or inst to be equivalent to another type or inst. It builds up two maps
    % of such declarations, and then traverses through all the items in the
    % given item blocks and through all the given event specs, expanding all
    % type and inst synonyms, which has the effect of eliminating all the
    % equivalence types and insts from the source code. We return the
    % equivalence map for types (our callers don't need the corresponding map
    % for insts).
    %
    % It also expands `with_type` and `with_inst` annotations on predicate and
    % function type declarations.
    %
    % It generates error messages for any circular equivalence types and insts
    % and for invalid `with_type` and `with_inst` annotations.
    %
    % For items not defined in the current module, the items expanded
    % while processing each item are recorded in the recompilation_info,
    % for use by smart recompilation.
    %
:- pred expand_eqv_types_insts(
    aug_compilation_unit::in, aug_compilation_unit::out,
    event_spec_map::in, event_spec_map::out,
    type_eqv_map::out, used_modules::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    list(error_spec)::out) is det.

    % Replace equivalence types in the representation of an equivalence type.
    % Generate an error message if the expansion reveals that the definition
    % is circular.
    %
:- pred replace_in_type_repn_eqv(type_eqv_map::in,
    item_type_repn_info_eqv::in, item_type_repn_info_eqv::out,
    list(error_spec)::in, list(error_spec)::out) is det.

    % Replace all equivalence types in a given type, reporting
    % any circularities, and whether the type has changed.
    %
:- pred replace_in_type_report_circular_eqvs(type_eqv_map::in, tvarset::in,
    prog_context::in, mer_type::in, mer_type::out, maybe_changed::out,
    list(error_spec)::in, list(error_spec)::out) is det.

    % Replace equivalence types in a given type.
    % The bool output is `yes' if anything changed.
    %
:- pred replace_in_type(type_eqv_map::in, mer_type::in, mer_type::out,
    maybe_changed::out, tvarset::in, tvarset::out,
    eqv_expand_info::in, eqv_expand_info::out) is det.

:- pred replace_in_type_list(type_eqv_map::in,
    list(mer_type)::in, list(mer_type)::out, maybe_changed::out,
    tvarset::in, tvarset::out,
    eqv_expand_info::in, eqv_expand_info::out) is det.

:- pred replace_in_univ_exist_constraints(type_eqv_map::in,
    univ_exist_constraints::in, univ_exist_constraints::out,
    tvarset::in, tvarset::out,
    eqv_expand_info::in, eqv_expand_info::out) is det.

:- pred replace_in_prog_constraint_list(type_eqv_map::in,
    list(prog_constraint)::in, list(prog_constraint)::out,
    tvarset::in, tvarset::out, eqv_expand_info::in, eqv_expand_info::out)
    is det.

:- pred replace_in_ctors(type_eqv_map::in,
    one_or_more(constructor)::in, one_or_more(constructor)::out,
    tvarset::in, tvarset::out,
    eqv_expand_info::in, eqv_expand_info::out) is det.

:- type eqv_type_body
    --->    eqv_type_body(
                tvarset,
                list(type_param),
                mer_type
            ).

:- type type_eqv_map == map(type_ctor, eqv_type_body).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.parse_tree_out_type.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_data_pragma.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_construct.
:- import_module parse_tree.prog_type_subst.
:- import_module parse_tree.prog_type_test.
:- import_module parse_tree.prog_util.
:- import_module recompilation.item_types.

:- import_module assoc_list.
:- import_module bool.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module term.
:- import_module varset.

:- type circ_types == set(type_ctor).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

expand_eqv_types_insts(AugCompUnit0, AugCompUnit, EventSpecMap0, EventSpecMap,
        TypeEqvMap, !:UsedModules, !RecompInfo, !:Specs) :-
    AugCompUnit0 = aug_compilation_unit(ParseTreeModuleSrc0,
        AncestorIntSpecs0, DirectInt1Specs0, IndirectInt2Specs0,
        PlainOpts0, TransOpts0, IntForOptSpecs0, TypeRepnSpecs0,
        ModuleVersionNumbers),
    ModuleName = ParseTreeModuleSrc0 ^ ptms_module_name,
    % First we build up a mapping which records the equivalence type
    % definitions, ...
    some [!TypeEqvMap, !InstEqvMap] (
        map.init(!:TypeEqvMap),
        map.init(!:InstEqvMap),
        build_eqv_maps_in_parse_tree_module_src(ParseTreeModuleSrc0,
            !TypeEqvMap, !InstEqvMap),
        map.foldl2_values(build_eqv_maps_in_ancestor_int_spec,
            AncestorIntSpecs0,
            !TypeEqvMap, !InstEqvMap),
        map.foldl2_values(build_eqv_maps_in_direct_int1_spec,
            DirectInt1Specs0,
            !TypeEqvMap, !InstEqvMap),
        map.foldl2_values(build_eqv_maps_in_indirect_int2_spec,
            IndirectInt2Specs0,
            !TypeEqvMap, !InstEqvMap),
        map.foldl2_values(build_eqv_maps_in_parse_tree_plain_opt, PlainOpts0,
            !TypeEqvMap, !InstEqvMap),
        map.foldl2_values(build_eqv_maps_in_parse_tree_trans_opt, TransOpts0,
            !TypeEqvMap, !InstEqvMap),
        map.foldl2_values(build_eqv_maps_in_int_for_opt_spec, IntForOptSpecs0,
            !TypeEqvMap, !InstEqvMap),
        TypeEqvMap = !.TypeEqvMap,
        InstEqvMap = !.InstEqvMap
    ),

    % .. and then we go through all the items in the relevant blocks
    % and in all the event specs, and replace all occurrences of
    % equivalence types and insts in them.
    !:UsedModules = used_modules_init,
    !:Specs = [],
    replace_in_parse_tree_module_src(TypeEqvMap, InstEqvMap,
        ParseTreeModuleSrc0, ParseTreeModuleSrc,
        !RecompInfo, !UsedModules, !Specs),
    map.map_values_foldl3(
        replace_in_ancestor_int_spec(ModuleName, TypeEqvMap, InstEqvMap),
        AncestorIntSpecs0, AncestorIntSpecs,
        !RecompInfo, !UsedModules, !Specs),
    map.map_values_foldl3(
        replace_in_direct_int1_spec(ModuleName, TypeEqvMap, InstEqvMap),
        DirectInt1Specs0, DirectInt1Specs, !RecompInfo, !UsedModules, !Specs),
    map.map_values_foldl3(
        replace_in_indirect_int2_spec(ModuleName, TypeEqvMap, InstEqvMap),
        IndirectInt2Specs0, IndirectInt2Specs,
        !RecompInfo, !UsedModules, !Specs),
    map.map_values_foldl3(
        replace_in_parse_tree_trans_opt(ModuleName, TypeEqvMap, InstEqvMap),
        TransOpts0, TransOpts, !RecompInfo, !UsedModules, !Specs),
    map.map_values_foldl3(
        replace_in_parse_tree_plain_opt(ModuleName, TypeEqvMap, InstEqvMap),
        PlainOpts0, PlainOpts, !RecompInfo, !UsedModules, !Specs),
    map.map_values_foldl3(
        replace_in_int_for_opt_spec(ModuleName, TypeEqvMap, InstEqvMap),
        IntForOptSpecs0, IntForOptSpecs, !RecompInfo, !UsedModules, !Specs),

    % XXX TYPE_REPN Type repns items should be generated fully eqv-expanded,
    % but it may be worth while checking whether this is really so.
    TypeRepnSpecs = TypeRepnSpecs0,
    AugCompUnit = aug_compilation_unit(ParseTreeModuleSrc,
        AncestorIntSpecs, DirectInt1Specs, IndirectInt2Specs,
        PlainOpts, TransOpts, IntForOptSpecs, TypeRepnSpecs,
        ModuleVersionNumbers),

    map.to_assoc_list(EventSpecMap0, EventSpecList0),
    replace_in_event_specs(TypeEqvMap, EventSpecList0, EventSpecList,
        !UsedModules),
    map.from_sorted_assoc_list(EventSpecList, EventSpecMap).

%---------------------------------------------------------------------------%

    % We need to expand equivalence insts in
    % `:- pred p `with_inst` i' declarations.
:- type eqv_inst_body
    --->    eqv_inst_body(
                list(inst_var),
                mer_inst
            ).

:- type inst_eqv_map == map(inst_ctor, eqv_inst_body).

%---------------------------------------------------------------------------%

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

:- pred replace_in_parse_tree_module_src(type_eqv_map::in, inst_eqv_map::in,
    parse_tree_module_src::in, parse_tree_module_src::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out,
    list(error_spec)::in, list(error_spec)::out) is det.

replace_in_parse_tree_module_src(TypeEqvMap, InstEqvMap,
        ParseTreeModuleSrc0, ParseTreeModuleSrc,
        !RecompInfo, !UsedModules, !Specs) :-
    MaybeRecordInt = record_sym_name_use(visibility_public),
    MaybeRecordImp = record_sym_name_use(visibility_private),

    ParseTreeModuleSrc0 = parse_tree_module_src(ModuleName, ModuleNameContext,
        InclMap, ImportUseMap,
        IntFIMSpecMap, ImpFIMSpecMap, IntSelfFIMLangs, ImpSelfFIMLangs,

        TypeCtorCheckedMap0, InstCtorCheckedMap0, ModeCtorCheckedMap0,
        TypeSpecs, InstModeSpecs,

        IntTypeClasses0, IntInstances0, IntPredDecls0, IntModeDecls0,
        IntDeclPragmas0, IntDeclMarkers, IntPromises, IntBadPreds,

        ImpTypeClasses0, ImpInstances0, ImpPredDecls0, ImpModeDecls0,
        ImpClauses0, ImpForeignProcs0, ImpForeignExportEnums,
        ImpDeclPragmas0, ImpDeclMarkers, ImpImplPragmas, ImpImplMarkers,
        ImpPromises, ImpInitialises, ImpFinalises, ImpMutables0),

    map.map_values_foldl3(
        replace_in_type_ctor_checked_defn(ModuleName,
            MaybeRecordInt, MaybeRecordImp, TypeEqvMap, InstEqvMap),
        TypeCtorCheckedMap0, TypeCtorCheckedMap,
        !RecompInfo, !UsedModules, !Specs),
    % XXX See the comment at module top.
    InstCtorCheckedMap = InstCtorCheckedMap0,
    ModeCtorCheckedMap = ModeCtorCheckedMap0,

    replace_in_list(ModuleName, MaybeRecordInt, TypeEqvMap, InstEqvMap,
        replace_in_typeclass_info, IntTypeClasses0, IntTypeClasses,
        !RecompInfo, !UsedModules, !Specs),
    replace_in_list(ModuleName, MaybeRecordInt, TypeEqvMap, InstEqvMap,
        replace_in_instance_info, IntInstances0, IntInstances,
        !RecompInfo, !UsedModules, !Specs),
    replace_in_list(ModuleName, MaybeRecordInt, TypeEqvMap, InstEqvMap,
        replace_in_pred_decl_info, IntPredDecls0, IntPredDecls,
        !RecompInfo, !UsedModules, !Specs),
    replace_in_list(ModuleName, MaybeRecordInt, TypeEqvMap, InstEqvMap,
        replace_in_mode_decl_info, IntModeDecls0, IntModeDecls,
        !RecompInfo, !UsedModules, !Specs),
    replace_in_list(ModuleName, MaybeRecordInt, TypeEqvMap, InstEqvMap,
        replace_in_decl_pragma_info, IntDeclPragmas0, IntDeclPragmas,
        !RecompInfo, !UsedModules, !Specs),

    replace_in_list(ModuleName, MaybeRecordImp, TypeEqvMap, InstEqvMap,
        replace_in_typeclass_info, ImpTypeClasses0, ImpTypeClasses,
        !RecompInfo, !UsedModules, !Specs),
    replace_in_list(ModuleName, MaybeRecordImp, TypeEqvMap, InstEqvMap,
        replace_in_instance_info, ImpInstances0, ImpInstances,
        !RecompInfo, !UsedModules, !Specs),
    replace_in_list(ModuleName, MaybeRecordImp, TypeEqvMap, InstEqvMap,
        replace_in_pred_decl_info, ImpPredDecls0, ImpPredDecls,
        !RecompInfo, !UsedModules, !Specs),
    replace_in_list(ModuleName, MaybeRecordImp, TypeEqvMap, InstEqvMap,
        replace_in_mode_decl_info, ImpModeDecls0, ImpModeDecls,
        !RecompInfo, !UsedModules, !Specs),
    ImpClauses = ImpClauses0, % XXX See the comment at module top.
    replace_in_list(ModuleName, MaybeRecordImp, TypeEqvMap, InstEqvMap,
        replace_in_decl_pragma_info, ImpDeclPragmas0, ImpDeclPragmas,
        !RecompInfo, !UsedModules, !Specs),
    replace_in_list(ModuleName, MaybeRecordImp, TypeEqvMap, InstEqvMap,
        replace_in_foreign_proc, ImpForeignProcs0, ImpForeignProcs,
        !RecompInfo, !UsedModules, !Specs),
    replace_in_list(ModuleName, MaybeRecordImp, TypeEqvMap, InstEqvMap,
        replace_in_mutable_info, ImpMutables0, ImpMutables,
        !RecompInfo, !UsedModules, !Specs),

    ParseTreeModuleSrc = parse_tree_module_src(ModuleName, ModuleNameContext,
        InclMap, ImportUseMap,
        IntFIMSpecMap, ImpFIMSpecMap, IntSelfFIMLangs, ImpSelfFIMLangs,

        TypeCtorCheckedMap, InstCtorCheckedMap, ModeCtorCheckedMap,
        TypeSpecs, InstModeSpecs,

        IntTypeClasses, IntInstances, IntPredDecls, IntModeDecls,
        IntDeclPragmas, IntDeclMarkers, IntPromises, IntBadPreds,

        ImpTypeClasses, ImpInstances, ImpPredDecls, ImpModeDecls,
        ImpClauses, ImpForeignProcs, ImpForeignExportEnums,
        ImpDeclPragmas, ImpDeclMarkers, ImpImplPragmas, ImpImplMarkers,
        ImpPromises, ImpInitialises, ImpFinalises, ImpMutables).

:- pred replace_in_ancestor_int_spec(module_name::in,
    type_eqv_map::in, inst_eqv_map::in,
    ancestor_int_spec::in, ancestor_int_spec::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out,
    list(error_spec)::in, list(error_spec)::out) is det.

replace_in_ancestor_int_spec(ModuleName, TypeEqvMap, InstEqvMap,
        AncestorIntSpec0, AncestorIntSpec, !RecompInfo, !UsedModules,
        !Specs) :-
    AncestorIntSpec0 = ancestor_int0(OrigParseTree0, ReadWhy0),
    replace_in_parse_tree_int0(ModuleName, TypeEqvMap, InstEqvMap,
        OrigParseTree0, ParseTree0, !RecompInfo, !UsedModules, !Specs),
    AncestorIntSpec = ancestor_int0(ParseTree0, ReadWhy0).

:- pred replace_in_direct_int1_spec(module_name::in,
    type_eqv_map::in, inst_eqv_map::in,
    direct_int1_spec::in, direct_int1_spec::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out,
    list(error_spec)::in, list(error_spec)::out) is det.

replace_in_direct_int1_spec(ModuleName, TypeEqvMap, InstEqvMap,
        DirectIntSpec0, DirectIntSpec, !RecompInfo, !UsedModules, !Specs) :-
    DirectIntSpec0 = direct_int1(OrigParseTree1, ReadWhy1),
    replace_in_parse_tree_int1(ModuleName, TypeEqvMap, InstEqvMap,
        OrigParseTree1, ParseTree1, !RecompInfo, !UsedModules, !Specs),
    DirectIntSpec = direct_int1(ParseTree1, ReadWhy1).

:- pred replace_in_indirect_int2_spec(module_name::in,
    type_eqv_map::in, inst_eqv_map::in,
    indirect_int2_spec::in, indirect_int2_spec::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out,
    list(error_spec)::in, list(error_spec)::out) is det.

replace_in_indirect_int2_spec(ModuleName, TypeEqvMap, InstEqvMap,
        IndirectIntSpec0, IndirectIntSpec,
        !RecompInfo, !UsedModules, !Specs) :-
    IndirectIntSpec0 = indirect_int2(OrigParseTree2, ReadWhy2),
    replace_in_parse_tree_int2(ModuleName, TypeEqvMap, InstEqvMap,
        OrigParseTree2, ParseTree2, !RecompInfo, !UsedModules, !Specs),
    IndirectIntSpec = indirect_int2(ParseTree2, ReadWhy2).

:- pred replace_in_int_for_opt_spec(module_name::in,
    type_eqv_map::in, inst_eqv_map::in,
    int_for_opt_spec::in, int_for_opt_spec::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out,
    list(error_spec)::in, list(error_spec)::out) is det.

replace_in_int_for_opt_spec(ModuleName, TypeEqvMap, InstEqvMap,
        IntForOptSpec0, IntForOptSpec, !RecompInfo, !UsedModules, !Specs) :-
    (
        IntForOptSpec0 = for_opt_int0(OrigParseTree0, ReadWhy0),
        replace_in_parse_tree_int0(ModuleName, TypeEqvMap, InstEqvMap,
            OrigParseTree0, ParseTree0, !RecompInfo, !UsedModules, !Specs),
        IntForOptSpec = for_opt_int0(ParseTree0, ReadWhy0)
    ;
        IntForOptSpec0 = for_opt_int1(OrigParseTree1, ReadWhy1),
        replace_in_parse_tree_int1(ModuleName, TypeEqvMap, InstEqvMap,
            OrigParseTree1, ParseTree1, !RecompInfo, !UsedModules, !Specs),
        IntForOptSpec = for_opt_int1(ParseTree1, ReadWhy1)
    ;
        IntForOptSpec0 = for_opt_int2(OrigParseTree2, ReadWhy2),
        replace_in_parse_tree_int2(ModuleName, TypeEqvMap, InstEqvMap,
            OrigParseTree2, ParseTree2, !RecompInfo, !UsedModules, !Specs),
        IntForOptSpec = for_opt_int2(ParseTree2, ReadWhy2)
    ).

%---------------------------------------------------------------------------%

:- pred replace_in_parse_tree_int0(module_name::in,
    type_eqv_map::in, inst_eqv_map::in,
    parse_tree_int0::in, parse_tree_int0::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out,
    list(error_spec)::in, list(error_spec)::out) is det.

replace_in_parse_tree_int0(ModuleName, TypeEqvMap, InstEqvMap,
        OrigParseTreeInt0, ParseTreeInt0, !RecompInfo, !UsedModules, !Specs) :-
    MaybeRecordInt = do_not_record_sym_name_use,
    MaybeRecordImp = do_not_record_sym_name_use,
    OrigParseTreeInt0 = parse_tree_int0(IntModuleName, IntModuleNameContext,
        MaybeVersionNumbers, InclMap, ImportUseMap, IntFIMSpecs, ImpFIMSpecs,
        TypeCtorCheckedMap0, InstCtorCheckedMap0, ModeCtorCheckedMap0,
        IntTypeClasses0, IntInstances0, IntPredDecls0, IntModeDecls0,
        IntDeclPragmas0, IntDeclMarkers, IntPromises,
        ImpTypeClasses0, ImpInstances0, ImpPredDecls0, ImpModeDecls0,
        ImpDeclPragmas0, ImpDeclMarkers, ImpPromises),

    map.map_values_foldl3(
        replace_in_type_ctor_checked_defn(ModuleName,
            MaybeRecordInt, MaybeRecordImp, TypeEqvMap, InstEqvMap),
        TypeCtorCheckedMap0, TypeCtorCheckedMap,
        !RecompInfo, !UsedModules, !Specs),
    % XXX See the comment at module top.
    InstCtorCheckedMap = InstCtorCheckedMap0,
    ModeCtorCheckedMap = ModeCtorCheckedMap0,

    replace_in_list(ModuleName, MaybeRecordInt, TypeEqvMap, InstEqvMap,
        replace_in_typeclass_info, IntTypeClasses0, IntTypeClasses,
        !RecompInfo, !UsedModules, !Specs),
    replace_in_list(ModuleName, MaybeRecordInt, TypeEqvMap, InstEqvMap,
        replace_in_abstract_instance_info, IntInstances0, IntInstances,
        !RecompInfo, !UsedModules, !Specs),
    replace_in_list(ModuleName, MaybeRecordInt, TypeEqvMap, InstEqvMap,
        replace_in_pred_decl_info, IntPredDecls0, IntPredDecls,
        !RecompInfo, !UsedModules, !Specs),
    replace_in_list(ModuleName, MaybeRecordInt, TypeEqvMap, InstEqvMap,
        replace_in_mode_decl_info, IntModeDecls0, IntModeDecls,
        !RecompInfo, !UsedModules, !Specs),
    replace_in_list(ModuleName, MaybeRecordInt, TypeEqvMap, InstEqvMap,
        replace_in_decl_pragma_info, IntDeclPragmas0, IntDeclPragmas,
        !RecompInfo, !UsedModules, !Specs),

    replace_in_list(ModuleName, MaybeRecordImp, TypeEqvMap, InstEqvMap,
        replace_in_typeclass_info, ImpTypeClasses0, ImpTypeClasses,
        !RecompInfo, !UsedModules, !Specs),
    replace_in_list(ModuleName, MaybeRecordImp, TypeEqvMap, InstEqvMap,
        replace_in_abstract_instance_info, ImpInstances0, ImpInstances,
        !RecompInfo, !UsedModules, !Specs),
    replace_in_list(ModuleName, MaybeRecordImp, TypeEqvMap, InstEqvMap,
        replace_in_pred_decl_info, ImpPredDecls0, ImpPredDecls,
        !RecompInfo, !UsedModules, !Specs),
    replace_in_list(ModuleName, MaybeRecordImp, TypeEqvMap, InstEqvMap,
        replace_in_mode_decl_info, ImpModeDecls0, ImpModeDecls,
        !RecompInfo, !UsedModules, !Specs),
    replace_in_list(ModuleName, MaybeRecordImp, TypeEqvMap, InstEqvMap,
        replace_in_decl_pragma_info, ImpDeclPragmas0, ImpDeclPragmas,
        !RecompInfo, !UsedModules, !Specs),

    ParseTreeInt0 = parse_tree_int0(IntModuleName, IntModuleNameContext,
        MaybeVersionNumbers, InclMap, ImportUseMap, IntFIMSpecs, ImpFIMSpecs,
        TypeCtorCheckedMap, InstCtorCheckedMap, ModeCtorCheckedMap,
        IntTypeClasses, IntInstances, IntPredDecls, IntModeDecls,
        IntDeclPragmas, IntDeclMarkers, IntPromises,
        ImpTypeClasses, ImpInstances, ImpPredDecls, ImpModeDecls,
        ImpDeclPragmas, ImpDeclMarkers, ImpPromises).

:- pred replace_in_parse_tree_int1(module_name::in,
    type_eqv_map::in, inst_eqv_map::in,
    parse_tree_int1::in, parse_tree_int1::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out,
    list(error_spec)::in, list(error_spec)::out) is det.

replace_in_parse_tree_int1(ModuleName, TypeEqvMap, InstEqvMap,
        OrigParseTreeInt1, ParseTreeInt1, !RecompInfo, !UsedModules, !Specs) :-
    MaybeRecordInt = do_not_record_sym_name_use,
    MaybeRecordImp = do_not_record_sym_name_use,
    OrigParseTreeInt1 = parse_tree_int1(IntModuleName, IntModuleNameContext,
        MaybeVersionNumbers, InclMap, ImportUseMap, IntFIMSpecs, ImpFIMSpecs,
        TypeCtorCheckedMap0, InstCtorCheckedMap0, ModeCtorCheckedMap0,
        IntTypeClasses0, IntInstances0, IntPredDecls0, IntModeDecls0,
        IntDeclPragmas0, IntDeclMarkers0, IntPromises, IntTypeRepnMap0,
        ImpTypeClasses0),

    map.map_values_foldl3(
        replace_in_type_ctor_checked_defn(ModuleName,
            MaybeRecordInt, MaybeRecordImp, TypeEqvMap, InstEqvMap),
        TypeCtorCheckedMap0, TypeCtorCheckedMap,
        !RecompInfo, !UsedModules, !Specs),
    % XXX See the comment at module top.
    InstCtorCheckedMap = InstCtorCheckedMap0,
    ModeCtorCheckedMap = ModeCtorCheckedMap0,

    replace_in_list(ModuleName, MaybeRecordInt, TypeEqvMap, InstEqvMap,
        replace_in_typeclass_info, IntTypeClasses0, IntTypeClasses,
        !RecompInfo, !UsedModules, !Specs),
    replace_in_list(ModuleName, MaybeRecordInt, TypeEqvMap, InstEqvMap,
        replace_in_abstract_instance_info, IntInstances0, IntInstances,
        !RecompInfo, !UsedModules, !Specs),
    replace_in_list(ModuleName, MaybeRecordInt, TypeEqvMap, InstEqvMap,
        replace_in_pred_decl_info, IntPredDecls0, IntPredDecls,
        !RecompInfo, !UsedModules, !Specs),
    replace_in_list(ModuleName, MaybeRecordInt, TypeEqvMap, InstEqvMap,
        replace_in_mode_decl_info, IntModeDecls0, IntModeDecls,
        !RecompInfo, !UsedModules, !Specs),
    replace_in_list(ModuleName, MaybeRecordInt, TypeEqvMap, InstEqvMap,
        replace_in_decl_pragma_info, IntDeclPragmas0, IntDeclPragmas,
        !RecompInfo, !UsedModules, !Specs),
    map.map_values_foldl3(
        replace_in_type_repn_info(ModuleName, MaybeRecordInt, TypeEqvMap),
        IntTypeRepnMap0, IntTypeRepnMap,
        !RecompInfo, !UsedModules, !Specs),

    replace_in_list(ModuleName, MaybeRecordImp, TypeEqvMap, InstEqvMap,
        replace_in_abstract_typeclass_info, ImpTypeClasses0, ImpTypeClasses,
        !RecompInfo, !UsedModules, !Specs),

    ParseTreeInt1 = parse_tree_int1(IntModuleName, IntModuleNameContext,
        MaybeVersionNumbers, InclMap, ImportUseMap, IntFIMSpecs, ImpFIMSpecs,
        TypeCtorCheckedMap, InstCtorCheckedMap, ModeCtorCheckedMap,
        IntTypeClasses, IntInstances, IntPredDecls, IntModeDecls,
        IntDeclPragmas, IntDeclMarkers0, IntPromises, IntTypeRepnMap,
        ImpTypeClasses).

:- pred replace_in_parse_tree_int2(module_name::in,
    type_eqv_map::in, inst_eqv_map::in,
    parse_tree_int2::in, parse_tree_int2::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out,
    list(error_spec)::in, list(error_spec)::out) is det.

replace_in_parse_tree_int2(ModuleName, TypeEqvMap, InstEqvMap,
        OrigParseTreeInt2, ParseTreeInt2, !RecompInfo, !UsedModules, !Specs) :-
    MaybeRecordInt = do_not_record_sym_name_use,
    MaybeRecordImp = do_not_record_sym_name_use,
    OrigParseTreeInt2 = parse_tree_int2(IntModuleName, IntModuleNameContext,
        MaybeVersionNumbers, InclMap, ImportUseMap, IntFIMSpecs, ImpFIMSpecs,
        TypeCtorCheckedMap0, InstCtorCheckedMap0, ModeCtorCheckedMap0,
        IntTypeClasses0, IntInstances0, IntTypeRepnMap0),

    map.map_values_foldl3(
        replace_in_type_ctor_checked_defn(ModuleName,
            MaybeRecordInt, MaybeRecordImp, TypeEqvMap, InstEqvMap),
        TypeCtorCheckedMap0, TypeCtorCheckedMap,
        !RecompInfo, !UsedModules, !Specs),
    % XXX See the comment at module top.
    InstCtorCheckedMap = InstCtorCheckedMap0,
    ModeCtorCheckedMap = ModeCtorCheckedMap0,

    replace_in_list(ModuleName, MaybeRecordInt, TypeEqvMap, InstEqvMap,
        replace_in_typeclass_info, IntTypeClasses0, IntTypeClasses,
        !RecompInfo, !UsedModules, !Specs),
    replace_in_list(ModuleName, MaybeRecordInt, TypeEqvMap, InstEqvMap,
        replace_in_abstract_instance_info, IntInstances0, IntInstances,
        !RecompInfo, !UsedModules, !Specs),
    map.map_values_foldl3(
        replace_in_type_repn_info(ModuleName, MaybeRecordInt, TypeEqvMap),
        IntTypeRepnMap0, IntTypeRepnMap,
        !RecompInfo, !UsedModules, !Specs),

    ParseTreeInt2 = parse_tree_int2(IntModuleName, IntModuleNameContext,
        MaybeVersionNumbers, InclMap, ImportUseMap, IntFIMSpecs, ImpFIMSpecs,
        TypeCtorCheckedMap, InstCtorCheckedMap, ModeCtorCheckedMap,
        IntTypeClasses, IntInstances, IntTypeRepnMap).

:- pred replace_in_parse_tree_plain_opt(module_name::in,
    type_eqv_map::in, inst_eqv_map::in,
    parse_tree_plain_opt::in, parse_tree_plain_opt::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out,
    list(error_spec)::in, list(error_spec)::out) is det.

replace_in_parse_tree_plain_opt(ModuleName, TypeEqvMap, InstEqvMap,
        OrigParseTreePlainOpt, ParseTreePlainOpt,
        !RecompInfo, !UsedModules, !Specs) :-
    MaybeRecord = do_not_record_sym_name_use,
    OrigParseTreePlainOpt = parse_tree_plain_opt(
        OptModuleName, OptModuleNameContext,
        UsedModuleNames, FIMSpecs, TypeDefns0, ForeignEnums,
        InstDefns0, ModeDefns0, TypeClasses0, Instances0,
        PredDecls0, ModeDecls0, Clauses, ForeignProcs, Promises,
        DeclMarkers, ImplMarkers,
        TypeSpecs0, UnusedArgs, TermInfos, Term2Infos,
        Exceptions, Trailings, MMTablings, Sharings, Reuses),

    InstDefns = InstDefns0, % XXX See the comment at module top.
    ModeDefns = ModeDefns0, % XXX See the comment at module top.
    replace_in_list(ModuleName, MaybeRecord, TypeEqvMap, InstEqvMap,
        replace_in_type_defn_info_general(replace_in_type_defn),
        TypeDefns0, TypeDefns, !RecompInfo, !UsedModules, !Specs),
    replace_in_list(ModuleName, MaybeRecord, TypeEqvMap, InstEqvMap,
        replace_in_typeclass_info, TypeClasses0, TypeClasses,
        !RecompInfo, !UsedModules, !Specs),
    replace_in_list(ModuleName, MaybeRecord, TypeEqvMap, InstEqvMap,
        replace_in_instance_info, Instances0, Instances,
        !RecompInfo, !UsedModules, !Specs),
    replace_in_list(ModuleName, MaybeRecord, TypeEqvMap, InstEqvMap,
        replace_in_pred_decl_info, PredDecls0, PredDecls,
        !RecompInfo, !UsedModules, !Specs),
    replace_in_list(ModuleName, MaybeRecord, TypeEqvMap, InstEqvMap,
        replace_in_mode_decl_info, ModeDecls0, ModeDecls,
        !RecompInfo, !UsedModules, !Specs),
    replace_in_list(ModuleName, MaybeRecord, TypeEqvMap, InstEqvMap,
        replace_in_decl_pragma_type_spec, TypeSpecs0, TypeSpecs,
        !RecompInfo, !UsedModules, !Specs),

    ParseTreePlainOpt = parse_tree_plain_opt(
        OptModuleName, OptModuleNameContext,
        UsedModuleNames, FIMSpecs, TypeDefns, ForeignEnums,
        InstDefns, ModeDefns, TypeClasses, Instances,
        PredDecls, ModeDecls, Clauses, ForeignProcs, Promises,
        DeclMarkers, ImplMarkers,
        TypeSpecs, UnusedArgs, TermInfos, Term2Infos,
        Exceptions, Trailings, MMTablings, Sharings, Reuses).

:- pred replace_in_parse_tree_trans_opt(module_name::in,
    type_eqv_map::in, inst_eqv_map::in,
    parse_tree_trans_opt::in, parse_tree_trans_opt::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out,
    list(error_spec)::in, list(error_spec)::out) is det.

replace_in_parse_tree_trans_opt(_ModuleName, _TypeEqvMap, _InstEqvMap,
        ParseTreeTransOpt, ParseTreeTransOpt, RecompInfo, RecompInfo,
        UsedModules, UsedModules, Specs, Specs).
    % No component that may appear in a parse_tree_trans_opt
    % needs any expansions.

%---------------------------------------------------------------------------%

:- type maybe_record_sym_name_use
    --->    do_not_record_sym_name_use
    ;       record_sym_name_use(item_visibility).

%---------------------------------------------------------------------------%

:- pred replace_in_maybe(module_name::in, maybe_record_sym_name_use::in,
    type_eqv_map::in, inst_eqv_map::in,
    pred(module_name, maybe_record_sym_name_use, type_eqv_map, inst_eqv_map,
        T, T, maybe(recompilation_info), maybe(recompilation_info),
        used_modules, used_modules, list(error_spec))
    :: in(pred(in, in,in, in, in, out, in, out, in, out, out) is det),
    maybe(T)::in, maybe(T)::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out,
    list(error_spec)::in, list(error_spec)::out) is det.

replace_in_maybe(ModuleName, MaybeRecord, TypeEqvMap, InstEqvMap,
        ReplaceInItem, MaybeItem0, MaybeItem,
        !RecompInfo, !UsedModules, !Specs) :-
    (
        MaybeItem0 = no,
        MaybeItem = no
    ;
        MaybeItem0 = yes(Item0),
        ReplaceInItem(ModuleName, MaybeRecord, TypeEqvMap, InstEqvMap,
            Item0, Item, !RecompInfo, !UsedModules, ItemSpecs),
        (
            ItemSpecs = [],
            MaybeItem = yes(Item)
        ;
            ItemSpecs = [_ | _],
            !:Specs = ItemSpecs ++ !.Specs,
            MaybeItem = no
        )
    ).

%---------------------%

:- pred replace_in_list(module_name::in, maybe_record_sym_name_use::in,
    type_eqv_map::in, inst_eqv_map::in,
    pred(module_name, maybe_record_sym_name_use, type_eqv_map, inst_eqv_map,
        T, T, maybe(recompilation_info), maybe(recompilation_info),
        used_modules, used_modules, list(error_spec))
    :: in(pred(in, in,in, in, in, out, in, out, in, out, out) is det),
    list(T)::in, list(T)::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out,
    list(error_spec)::in, list(error_spec)::out) is det.

replace_in_list(ModuleName, MaybeRecord, TypeEqvMap, InstEqvMap,
        ReplaceInItem, Items0, Items, !RecompInfo, !UsedModules, !Specs) :-
    replace_in_list_loop(ModuleName, MaybeRecord, TypeEqvMap, InstEqvMap,
        ReplaceInItem, Items0, [], RevItems,
        !RecompInfo, !UsedModules, !Specs),
    list.reverse(RevItems, Items).

:- pred replace_in_list_loop(module_name::in, maybe_record_sym_name_use::in,
    type_eqv_map::in, inst_eqv_map::in,
    pred(module_name, maybe_record_sym_name_use, type_eqv_map, inst_eqv_map,
        T, T, maybe(recompilation_info), maybe(recompilation_info),
        used_modules, used_modules, list(error_spec))
    :: in(pred(in, in,in, in, in, out, in, out, in, out, out) is det),
    list(T)::in, list(T)::in, list(T)::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out,
    list(error_spec)::in, list(error_spec)::out) is det.

replace_in_list_loop(_ModuleName, _MaybeRecord, _TypeEqvMap, _InstEqvMap,
        _ReplaceInItem, [], !RevItems, !RecompInfo, !UsedModules, !Specs).
replace_in_list_loop(ModuleName, MaybeRecord, TypeEqvMap, InstEqvMap,
        ReplaceInItem, [Item0 | Items0], !RevItems,
        !RecompInfo, !UsedModules, !Specs) :-
    ReplaceInItem(ModuleName, MaybeRecord, TypeEqvMap, InstEqvMap,
        Item0, Item, !RecompInfo, !UsedModules, ItemSpecs),
    % Discard the item if there were any errors.
    (
        ItemSpecs = [],
        !:RevItems = [Item | !.RevItems]
    ;
        ItemSpecs = [_ | _],
        !:Specs = ItemSpecs ++ !.Specs
    ),
    replace_in_list_loop(ModuleName, MaybeRecord, TypeEqvMap, InstEqvMap,
        ReplaceInItem, Items0, !RevItems, !RecompInfo, !UsedModules, !Specs).

%---------------------------------------------------------------------------%

:- pred replace_in_type_ctor_checked_defn(module_name::in,
    maybe_record_sym_name_use::in, maybe_record_sym_name_use::in,
    type_eqv_map::in, inst_eqv_map::in,
    type_ctor_checked_defn::in, type_ctor_checked_defn::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out,
    list(error_spec)::in, list(error_spec)::out) is det.

replace_in_type_ctor_checked_defn(ModuleName, MaybeRecordInt, MaybeRecordImp,
        TypeEqvMap, InstEqvMap, CheckedDefn0, CheckedDefn,
        !RecompInfo, !UsedModules, !Specs) :-
    (
        CheckedDefn0 = checked_defn_solver(SolverDefn0, SrcDefns0),
        (
            SolverDefn0 = solver_type_abstract(_, _),
            SolverDefn = SolverDefn0
        ;
            SolverDefn0 =
                solver_type_full(MaybeAbstractDefn0, ItemSolverDefn0),
            replace_in_type_defn_info_general(replace_in_type_defn_solver,
                ModuleName, MaybeRecordImp, TypeEqvMap, InstEqvMap,
                ItemSolverDefn0, ItemSolverDefn,
                !RecompInfo, !UsedModules, SolverSpecs),
            !:Specs = SolverSpecs ++ !.Specs,
            % Abstract type definitions have no equivalences to expand out.
            SolverDefn = solver_type_full(MaybeAbstractDefn0, ItemSolverDefn)
        ),
        SrcDefns0 = src_defns_solver(MaybeIntDefn0, MaybeImpDefn0),
        replace_in_maybe(ModuleName, MaybeRecordInt, TypeEqvMap, InstEqvMap,
            replace_in_type_defn_info_general(replace_in_type_defn),
            MaybeIntDefn0, MaybeIntDefn, !RecompInfo, !UsedModules, !Specs),
        replace_in_maybe(ModuleName, MaybeRecordImp, TypeEqvMap, InstEqvMap,
            replace_in_type_defn_info_general(replace_in_type_defn),
            MaybeImpDefn0, MaybeImpDefn, !RecompInfo, !UsedModules, !Specs),
        SrcDefns = src_defns_solver(MaybeIntDefn, MaybeImpDefn),
        CheckedDefn = checked_defn_solver(SolverDefn, SrcDefns)
    ;
        CheckedDefn0 = checked_defn_std(StdDefn0, SrcDefns0),
        (
            StdDefn0 = std_mer_type_eqv(Status, ItemEqvDefn0),
            replace_in_type_defn_info_general(replace_in_type_defn_eqv,
                ModuleName, MaybeRecordImp, TypeEqvMap, InstEqvMap,
                ItemEqvDefn0, ItemEqvDefn, !RecompInfo, !UsedModules,
                EqvSpecs),
            !:Specs = EqvSpecs ++ !.Specs,
            StdDefn = std_mer_type_eqv(Status, ItemEqvDefn)
        ;
            StdDefn0 = std_mer_type_subtype(Status, ItemSubDefn0),
            replace_in_type_defn_info_general(replace_in_type_defn_sub,
                ModuleName, MaybeRecordImp, TypeEqvMap, InstEqvMap,
                ItemSubDefn0, ItemSubDefn, !RecompInfo, !UsedModules,
                SubSpecs),
            !:Specs = SubSpecs ++ !.Specs,
            StdDefn = std_mer_type_subtype(Status, ItemSubDefn)
        ;
            StdDefn0 = std_mer_type_du_all_plain_constants(Status,
                ItemDuDefn0, HeadCtor, TailCtors, CJCsMaybeDefnOrEnum),
            replace_in_type_defn_info_general(replace_in_type_defn_du,
                ModuleName, MaybeRecordImp, TypeEqvMap, InstEqvMap,
                ItemDuDefn0, ItemDuDefn, !RecompInfo, !UsedModules, DuSpecs),
            !:Specs = DuSpecs ++ !.Specs,
            % Foreign type definitions and enums have no equivalences
            % to expand out.
            StdDefn = std_mer_type_du_all_plain_constants(Status,
                ItemDuDefn, HeadCtor, TailCtors, CJCsMaybeDefnOrEnum)
        ;
            StdDefn0 = std_mer_type_du_not_all_plain_constants(Status,
                ItemDuDefn0, CJCsMaybeDefn),
            replace_in_type_defn_info_general(replace_in_type_defn_du,
                ModuleName, MaybeRecordImp, TypeEqvMap, InstEqvMap,
                ItemDuDefn0, ItemDuDefn, !RecompInfo, !UsedModules, DuSpecs),
            !:Specs = DuSpecs ++ !.Specs,
            % Foreign type definitions have no equivalences to expand out.
            StdDefn = std_mer_type_du_not_all_plain_constants(Status,
                ItemDuDefn, CJCsMaybeDefn)
        ;
            StdDefn0 = std_mer_type_abstract(_Status,
                _ItemAbstractDefn, _CJCsMaybeDefn),
            % Abstract type definitions and foreign type definitions
            % have no equivalences to expand out.
            StdDefn = StdDefn0
        ),
        SrcDefns0 = src_defns_std(IntDefns0, ImpDefns0, ImpForeignEnums0),
        replace_in_list(ModuleName, MaybeRecordInt, TypeEqvMap, InstEqvMap,
            replace_in_type_defn_info_general(replace_in_type_defn),
            IntDefns0, IntDefns, !RecompInfo, !UsedModules, !Specs),
        replace_in_list(ModuleName, MaybeRecordImp, TypeEqvMap, InstEqvMap,
            replace_in_type_defn_info_general(replace_in_type_defn),
            ImpDefns0, ImpDefns, !RecompInfo, !UsedModules, !Specs),
        % Foreign enum infos have no equivalences to expand out.
        SrcDefns = src_defns_std(IntDefns, ImpDefns, ImpForeignEnums0),
        CheckedDefn = checked_defn_std(StdDefn, SrcDefns)
    ).

:- pred replace_in_type_defn_info_general(
    pred(maybe_record_sym_name_use, type_eqv_map, inst_eqv_map, type_ctor,
        prog_context, T, T, tvarset, tvarset,
        eqv_expand_info, eqv_expand_info,
        used_modules, used_modules, list(error_spec))
    :: in(pred(in, in, in, in, in, in, out, in, out, in, out, in, out, out)
        is det),
    module_name::in, maybe_record_sym_name_use::in,
    type_eqv_map::in, inst_eqv_map::in,
    item_type_defn_info_general(T)::in, item_type_defn_info_general(T)::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out, list(error_spec)::out) is det.

replace_in_type_defn_info_general(ReplaceInTypeDefn, ModuleName, MaybeRecord,
        TypeEqvMap, InstEqvMap, Info0, Info,
        !RecompInfo, !UsedModules, Specs) :-
    Info0 = item_type_defn_info(SymName, ArgTypeVars, TypeDefn0, TVarSet0,
        Context, SeqNum),
    list.length(ArgTypeVars, Arity),
    TypeCtor = type_ctor(SymName, Arity),
    maybe_start_recording_expanded_items(ModuleName, SymName, !.RecompInfo,
        UsedTypeCtors0),
    ReplaceInTypeDefn(MaybeRecord, TypeEqvMap, InstEqvMap, TypeCtor, Context,
        TypeDefn0, TypeDefn, TVarSet0, TVarSet,
        UsedTypeCtors0, UsedTypeCtors, !UsedModules, Specs),
    ItemName = recomp_item_name(SymName, Arity),
    ItemId = recomp_item_id(recomp_type_defn, ItemName),
    finish_recording_expanded_items(ItemId, UsedTypeCtors, !RecompInfo),
    Info = item_type_defn_info(SymName, ArgTypeVars, TypeDefn, TVarSet,
        Context, SeqNum).

:- pred replace_in_type_repn_info(module_name::in,
    maybe_record_sym_name_use::in, type_eqv_map::in,
    item_type_repn_info::in, item_type_repn_info::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out,
    list(error_spec)::in, list(error_spec)::out) is det.

replace_in_type_repn_info(ModuleName, MaybeRecord, TypeEqvMap,
        Info0, Info, !RecompInfo, !UsedModules, !Specs) :-
    Info0 = item_type_repn_info(SymName, ArgTypeVars, TypeRepn0, TVarSet0,
        Context, SeqNum),
    list.length(ArgTypeVars, Arity),
    maybe_start_recording_expanded_items(ModuleName, SymName, !.RecompInfo,
        UsedTypeCtors0),
    (
        TypeRepn0 = tcrepn_is_eqv_to(Type0),
        TypeCtor = type_ctor(SymName, Arity),
        replace_in_type_maybe_record_use_2(MaybeRecord, TypeEqvMap, [TypeCtor],
            Type0, Type, _, Circ, TVarSet0, TVarSet,
            UsedTypeCtors0, UsedTypeCtors, !UsedModules),
        set.to_sorted_list(Circ, CircTypes),
        (
            CircTypes = [_ | _],
            !:Specs = [report_circular_eqv_type(TypeCtor, Context) | !.Specs]
        ;
            CircTypes = []
        ),
        TypeRepn = tcrepn_is_eqv_to(Type)
    ;
        TypeRepn0 = tcrepn_is_subtype_of(SuperTypeCtor0),
        % Construct a type from the type ctor, substituting 'void' for any type
        % parameters, so that we can call replace_in_type_maybe_record_use_2.
        % We do not care about the type arguments so we can drop them again
        % afterwards.
        SuperTypeCtor0 = type_ctor(_, SuperTypeCtorArity),
        list.duplicate(SuperTypeCtorArity, void_type, VoidTypes),
        construct_type(SuperTypeCtor0, VoidTypes, SuperType0),
        TypeCtor = type_ctor(SymName, Arity),
        replace_in_type_maybe_record_use_2(MaybeRecord, TypeEqvMap, [TypeCtor],
            SuperType0, SuperType, _, Circ, TVarSet0, TVarSet,
            UsedTypeCtors0, UsedTypeCtors, !UsedModules),
        type_to_ctor_det(SuperType, SuperTypeCtor),
        set.to_sorted_list(Circ, CircTypes),
        (
            CircTypes = [_ | _],
            !:Specs = [report_circular_eqv_type(TypeCtor, Context) | !.Specs]
        ;
            CircTypes = []
        ),
        TypeRepn = tcrepn_is_subtype_of(SuperTypeCtor)
    ;
        ( TypeRepn0 = tcrepn_is_word_aligned_ptr
        ; TypeRepn0 = tcrepn_du(_)
        ; TypeRepn0 = tcrepn_foreign(_)
        ),
        TypeRepn = TypeRepn0,
        TVarSet = TVarSet0,
        UsedTypeCtors = UsedTypeCtors0
    ),
    ItemName = recomp_item_name(SymName, Arity),
    ItemId = recomp_item_id(recomp_type_defn, ItemName),
    finish_recording_expanded_items(ItemId, UsedTypeCtors, !RecompInfo),
    Info = item_type_repn_info(SymName, ArgTypeVars, TypeRepn, TVarSet,
        Context, SeqNum).

replace_in_type_repn_eqv(TypeEqvMap, Info0, Info, !Specs) :-
    Info0 = item_type_repn_info(SymName, ArgTypeVars, Type0, TVarSet0,
        Context, SeqNum),
    list.length(ArgTypeVars, Arity),
    TypeCtor = type_ctor(SymName, Arity),
    replace_in_type_maybe_record_use_2(do_not_record_sym_name_use, TypeEqvMap,
        [], Type0, Type, _Changed, Circ, TVarSet0, TVarSet,
        no_eqv_expand_info, _, used_modules_init, _),
    set.to_sorted_list(Circ, CircTypes),
    (
        CircTypes = [_ | _],
        !:Specs = [report_circular_eqv_type(TypeCtor, Context) | !.Specs]
    ;
        CircTypes = []
    ),
    Info = item_type_repn_info(SymName, ArgTypeVars, Type, TVarSet,
        Context, SeqNum).

:- pred replace_in_pred_decl_info(module_name::in,
    maybe_record_sym_name_use::in, type_eqv_map::in, inst_eqv_map::in,
    item_pred_decl_info::in, item_pred_decl_info::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out, list(error_spec)::out) is det.

replace_in_pred_decl_info(ModuleName, MaybeRecord, TypeEqvMap, InstEqvMap,
        Info0, Info, !RecompInfo, !UsedModules, Specs) :-
    Info0 = item_pred_decl_info(PredName, PredOrFunc, TypesAndMaybeModes0,
        MaybeWithType0, MaybeWithInst0, MaybeDetism0, Origin,
        TVarSet0, InstVarSet, ExistQVars, Purity, ClassContext0,
        Context, SeqNum),
    maybe_start_recording_expanded_items(ModuleName, PredName, !.RecompInfo,
        ExpandedItems0),
    replace_in_pred_types_and_maybe_modes(MaybeRecord, PredName, PredOrFunc,
        Context, TypeEqvMap, InstEqvMap, ClassContext0, ClassContext,
        TypesAndMaybeModes0, TypesAndMaybeModes, TVarSet0, TVarSet,
        MaybeWithType0, MaybeWithType, MaybeWithInst0, MaybeWithInst,
        MaybeDetism0, MaybeDetism, ExpandedItems0, ExpandedItems,
        !UsedModules, Specs),
    ItemType = pred_or_func_to_recomp_item_type(PredOrFunc),
    PredFormArity = types_and_maybe_modes_arity(TypesAndMaybeModes),
    user_arity_pred_form_arity(PredOrFunc, user_arity(Arity), PredFormArity),
    ItemName = recomp_item_name(PredName, Arity),
    ItemId = recomp_item_id(ItemType, ItemName),
    finish_recording_expanded_items(ItemId, ExpandedItems, !RecompInfo),
    Info = item_pred_decl_info(PredName, PredOrFunc, TypesAndMaybeModes,
        MaybeWithType, MaybeWithInst, MaybeDetism, Origin,
        TVarSet, InstVarSet, ExistQVars, Purity, ClassContext,
        Context, SeqNum).

%---------------------%

:- pred replace_in_mode_decl_info(module_name::in,
    maybe_record_sym_name_use::in, type_eqv_map::in, inst_eqv_map::in,
    item_mode_decl_info::in, item_mode_decl_info::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out, list(error_spec)::out) is det.

replace_in_mode_decl_info(ModuleName, MaybeRecord, _TypeEqvMap, InstEqvMap,
        Info0, Info, !RecompInfo, !UsedModules, Specs) :-
    Info0 = item_mode_decl_info(PredName, MaybePredOrFunc0, Modes0,
        WithInst0, MaybeDetism0, InstVarSet, Context, SeqNum),
    maybe_start_recording_expanded_items(ModuleName, PredName, !.RecompInfo,
        ExpandedItems0),
    PredFormArity = arg_list_arity(Modes0),
    replace_in_with_inst(MaybeRecord, InstEqvMap, PredName,
        PredFormArity, Context, mode_decl, MaybePredOrFunc0, MaybePredOrFunc,
        WithInst0, WithInst, ExtraModes, MaybeDetism0, MaybeDetism,
        ExpandedItems0, ExpandedItems, !UsedModules, Specs),
    (
        ExtraModes = [],
        Modes = Modes0
    ;
        ExtraModes = [_ | _],
        Modes = Modes0 ++ ExtraModes
    ),
    (
        MaybePredOrFunc = yes(PredOrFunc),
        ItemType = pred_or_func_to_recomp_item_type(PredOrFunc),
        list.length(Modes, Arity),
        adjust_func_arity(PredOrFunc, OrigArity, Arity),
        ItemName = recomp_item_name(PredName, OrigArity),
        ItemId = recomp_item_id(ItemType, ItemName),
        finish_recording_expanded_items(ItemId, ExpandedItems, !RecompInfo)
    ;
        MaybePredOrFunc = no
    ),
    Info = item_mode_decl_info(PredName, MaybePredOrFunc, Modes,
        WithInst, MaybeDetism, InstVarSet, Context, SeqNum).

%---------------------%
%
% The next two predicates have identical definitions except for the treatment
% of the class interfaces, but one is for item_typeclass_infos, while the other
% is for item_abstract_typeclass_infos.
% XXX Ideally, this should not be necessary.
%

:- pred replace_in_typeclass_info(module_name::in,
    maybe_record_sym_name_use::in, type_eqv_map::in, inst_eqv_map::in,
    item_typeclass_info::in, item_typeclass_info::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out, list(error_spec)::out) is det.

replace_in_typeclass_info(ModuleName, MaybeRecord, TypeEqvMap, InstEqvMap,
        Info0, Info, !RecompInfo, !UsedModules, Specs) :-
    Info0 = item_typeclass_info(ClassName, Vars, Constraints0, FunDeps,
        ClassInterface0, TVarSet0, Context, SeqNum),
    list.length(Vars, Arity),
    maybe_start_recording_expanded_items(ModuleName, ClassName, !.RecompInfo,
        ExpandedItems0),
    replace_in_prog_constraint_list_location(MaybeRecord, TypeEqvMap,
        Constraints0, Constraints, TVarSet0, TVarSet,
        ExpandedItems0, ExpandedItems1, !UsedModules),
    (
        ClassInterface0 = class_interface_abstract,
        ClassInterface = class_interface_abstract,
        ExpandedItems = ExpandedItems1,
        Specs = []
    ;
        ClassInterface0 = class_interface_concrete(Methods0),
        replace_in_class_interface(MaybeRecord, TypeEqvMap, InstEqvMap,
            Methods0, Methods, ExpandedItems1, ExpandedItems,
            !UsedModules, [], Specs),
        ClassInterface = class_interface_concrete(Methods)
    ),
    ItemName = recomp_item_name(ClassName, Arity),
    ItemId = recomp_item_id(recomp_typeclass, ItemName),
    finish_recording_expanded_items(ItemId, ExpandedItems, !RecompInfo),
    Info = item_typeclass_info(ClassName, Vars, Constraints, FunDeps,
        ClassInterface, TVarSet, Context, SeqNum).

:- pred replace_in_abstract_typeclass_info(module_name::in,
    maybe_record_sym_name_use::in, type_eqv_map::in, inst_eqv_map::in,
    item_abstract_typeclass_info::in, item_abstract_typeclass_info::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out, list(error_spec)::out) is det.

replace_in_abstract_typeclass_info(ModuleName, MaybeRecord, TypeEqvMap,
        _InstEqvMap, Info0, Info, !RecompInfo, !UsedModules, Specs) :-
    Info0 = item_typeclass_info(ClassName, Vars, Constraints0, FunDeps,
        ClassInterface, TVarSet0, Context, SeqNum),
    list.length(Vars, Arity),
    maybe_start_recording_expanded_items(ModuleName, ClassName, !.RecompInfo,
        ExpandedItems0),
    replace_in_prog_constraint_list_location(MaybeRecord, TypeEqvMap,
        Constraints0, Constraints, TVarSet0, TVarSet,
        ExpandedItems0, ExpandedItems, !UsedModules),
    Specs = [],
    ItemName = recomp_item_name(ClassName, Arity),
    ItemId = recomp_item_id(recomp_typeclass, ItemName),
    finish_recording_expanded_items(ItemId, ExpandedItems, !RecompInfo),
    Info = item_typeclass_info(ClassName, Vars, Constraints, FunDeps,
        ClassInterface, TVarSet, Context, SeqNum).

%---------------------%
%
% The next two predicates have identical definitions except for the treatment
% of the instance bodies, but one is for item_instance_infos, while the other
% is for item_abstract_instance_infos.
% XXX Ideally, this should not be necessary.
%

:- pred replace_in_instance_info(module_name::in,
    maybe_record_sym_name_use::in, type_eqv_map::in, inst_eqv_map::in,
    item_instance_info::in, item_instance_info::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out, list(error_spec)::out) is det.

replace_in_instance_info(ModuleName, MaybeRecord, TypeEqvMap, _InstEqvMap,
        InstanceInfo0, InstanceInfo, !RecompInfo, !UsedModules, []) :-
    InstanceInfo0 = item_instance_info(ClassName, Types0, OriginalTypes,
        Constraints0, InstanceBody0, TVarSet0, ContainingModuleName,
        Context, SeqNum),
    ( if
        ( !.RecompInfo = no
        ; ContainingModuleName = ModuleName
        )
    then
        UsedTypeCtors0 = no_eqv_expand_info
    else
        UsedTypeCtors0 = eqv_expand_info(ModuleName, set.init)
    ),
    replace_in_prog_constraint_list_location(MaybeRecord, TypeEqvMap,
        Constraints0, Constraints, TVarSet0, TVarSet1,
        UsedTypeCtors0, UsedTypeCtors1, !UsedModules),
    replace_in_type_list_location_circ(MaybeRecord, TypeEqvMap, Types0, Types,
        _, _, TVarSet1, TVarSet, UsedTypeCtors1, UsedTypeCtors, !UsedModules),
    (
        InstanceBody0 = instance_body_abstract,
        InstanceBody = instance_body_abstract
    ;
        InstanceBody0 = instance_body_concrete(_InstanceMethods0),
        InstanceBody = InstanceBody0
% We don't yet have code to expand out type equivalences in explicit
% type qualifications in clauses.
%
%       replace_in_list(ModuleName, MaybeRecord, TypeEqvMap, InstEqvMap,
%           replace_in_instance_method, InstanceMethods0, InstanceMethods,
%           !RecompInfo, !UsedModules, [], Specs),
%       InstanceBody = instance_body_concrete(InstanceMethods)
    ),
    % We specifically do NOT expand equivalence types in OriginalTypes.
    % If we did, that would defeat the purpose of the field.
    ItemName = recomp_item_name(ClassName, list.length(Types0)),
    ItemId = recomp_item_id(recomp_typeclass, ItemName),
    finish_recording_expanded_items(ItemId, UsedTypeCtors, !RecompInfo),
    InstanceInfo = item_instance_info(ClassName, Types, OriginalTypes,
        Constraints, InstanceBody, TVarSet, ContainingModuleName,
        Context, SeqNum).

:- pred replace_in_abstract_instance_info(module_name::in,
    maybe_record_sym_name_use::in, type_eqv_map::in, inst_eqv_map::in,
    item_abstract_instance_info::in, item_abstract_instance_info::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out, list(error_spec)::out) is det.

replace_in_abstract_instance_info(ModuleName, MaybeRecord, TypeEqvMap, _,
        InstanceInfo0, InstanceInfo, !RecompInfo, !UsedModules, []) :-
    InstanceInfo0 = item_instance_info(ClassName, Types0, OriginalTypes,
        Constraints0, InstanceBody, TVarSet0, ContainingModuleName,
        Context, SeqNum),
    ( if
        ( !.RecompInfo = no
        ; ContainingModuleName = ModuleName
        )
    then
        UsedTypeCtors0 = no_eqv_expand_info
    else
        UsedTypeCtors0 = eqv_expand_info(ModuleName, set.init)
    ),
    replace_in_prog_constraint_list_location(MaybeRecord, TypeEqvMap,
        Constraints0, Constraints, TVarSet0, TVarSet1,
        UsedTypeCtors0, UsedTypeCtors1, !UsedModules),
    replace_in_type_list_location_circ(MaybeRecord, TypeEqvMap, Types0, Types,
        _, _, TVarSet1, TVarSet, UsedTypeCtors1, UsedTypeCtors, !UsedModules),
    % We specifically do NOT expand equivalence types in OriginalTypes.
    % If we did, that would defeat the purpose of the field.
    ItemName = recomp_item_name(ClassName, list.length(Types0)),
    ItemId = recomp_item_id(recomp_typeclass, ItemName),
    finish_recording_expanded_items(ItemId, UsedTypeCtors, !RecompInfo),
    InstanceInfo = item_instance_info(ClassName, Types, OriginalTypes,
        Constraints, InstanceBody, TVarSet, ContainingModuleName,
        Context, SeqNum).

%---------------------%

% We don't yet have code to expand out type equivalences in explicit
% type qualifications in clauses.
%
% :- pred replace_in_instance_method(module_name::in,
%   maybe_record_sym_name_use::in, type_eqv_map::in, inst_eqv_map::in,
%   instance_method::in, instance_method::out,
%   maybe(recompilation_info)::in, maybe(recompilation_info)::out,
%   used_modules::in, used_modules::out, list(error_spec)::out) is det.

% replace_in_instance_method(ModuleName, MaybeRecord, TypeEqvMap, InstEqvMap,
%       InstanceMethod0, InstanceMethod, !RecompInfo, !UsedModules, Specs) :-
%   InstanceMethod0 = instance_method(_MethorNameArity, _ProcDef0, _Context),
%   (
%       ProcDef0 = instance_proc_def_name(_Name),
%       InstanceMethod = InstanceMethod0
%   ;
%       ProcDef0 = instance_proc_def_clauses(Clauses0),
%       replace_in_clauses(..., Clauses0, Clauses, ...),
%       ProcDef = instance_proc_def_clauses(Clauses),
%       InstanceMethod = instance_method(MethorNameArity, ProcDef, Context)
%   ).

%---------------------%

:- pred replace_in_decl_pragma_info(module_name::in,
    maybe_record_sym_name_use::in, type_eqv_map::in, inst_eqv_map::in,
    item_decl_pragma_info::in, item_decl_pragma_info::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out, list(error_spec)::out) is det.

replace_in_decl_pragma_info(ModuleName, MaybeRecord, TypeEqvMap, InstEqvMap,
        DeclPragma0, DeclPragma, !RecompInfo, !UsedModules, Specs) :-
    (
        DeclPragma0 = decl_pragma_type_spec_constr(TypeSpecConstr0),
        replace_in_decl_pragma_type_spec_constr(ModuleName, MaybeRecord,
            TypeEqvMap, InstEqvMap, TypeSpecConstr0, TypeSpecConstr,
            !UsedModules, Specs),
        DeclPragma = decl_pragma_type_spec_constr(TypeSpecConstr)
    ;
        DeclPragma0 = decl_pragma_type_spec(TypeSpec0),
        replace_in_decl_pragma_type_spec(ModuleName, MaybeRecord,
            TypeEqvMap, InstEqvMap, TypeSpec0, TypeSpec,
            !RecompInfo, !UsedModules, Specs),
        DeclPragma = decl_pragma_type_spec(TypeSpec)
    ;
        ( DeclPragma0 = decl_pragma_obsolete_pred(_)
        ; DeclPragma0 = decl_pragma_obsolete_proc(_)
        ; DeclPragma0 = decl_pragma_format_call(_)
        ; DeclPragma0 = decl_pragma_oisu(_)
        ; DeclPragma0 = decl_pragma_termination(_)
        ; DeclPragma0 = decl_pragma_termination2(_)
        ; DeclPragma0 = decl_pragma_struct_reuse(_)
        ; DeclPragma0 = decl_pragma_struct_sharing(_)
        ),
        DeclPragma = DeclPragma0,
        Specs = []
    ).

:- pred replace_in_decl_pragma_type_spec_constr(module_name::in,
    maybe_record_sym_name_use::in, type_eqv_map::in, inst_eqv_map::in,
    decl_pragma_type_spec_constr_info::in,
        decl_pragma_type_spec_constr_info::out,
    used_modules::in, used_modules::out, list(error_spec)::out) is det.

replace_in_decl_pragma_type_spec_constr(ModuleName, MaybeRecord,
        TypeEqvMap, _InstEqvMap, TypeSpecInfoConstr0, TypeSpecInfoConstr,
        !UsedModules, []) :-
    TypeSpecInfoConstr0 = decl_pragma_type_spec_constr_info(PragmaModuleName,
        OoMConstraints0, ApplyToSupers, OoMSubsts0, TVarSet0, ItemIds0,
        Context, SeqNum),
    % XXX I (zs) don't understand the purpose of the test in the code that
    % sets ExpandedItems0 in replace_in_decl_pragma_type_spec below.
    % The commit that added that code (the commit by Simon that added
    % the initial implementation of smart recompilation) does not mention
    % any rationale either. I cannot copy that test since there is no PredName
    % here. So this setting of ExpandedItems0 here is just a guess. Whether
    % it is a correct guess or not will matter only once smart recompilation
    % is completed, in the fullness of time.
    OoMConstraints0 = one_or_more(HeadConstraint0, TailConstraints0),
    ExpandedItems0 = eqv_expand_info(ModuleName, ItemIds0),
    replace_in_var_or_ground_constraint_location(MaybeRecord, TypeEqvMap,
        HeadConstraint0, HeadConstraint, TVarSet0, TVarSet1,
        ExpandedItems0, ExpandedItems1, !UsedModules),
    list.map_foldl3(
        replace_in_var_or_ground_constraint_location(MaybeRecord, TypeEqvMap),
        TailConstraints0, TailConstraints, TVarSet1, TVarSet2,
        ExpandedItems1, ExpandedItems2, !UsedModules),
    OoMConstraints = one_or_more(HeadConstraint, TailConstraints),
    OoMSubsts0 = one_or_more(HeadSubst0, TailSubsts0),
    replace_in_subst(MaybeRecord, TypeEqvMap,
        HeadSubst0, HeadSubst, TVarSet2, TVarSet3,
        ExpandedItems2, ExpandedItems3, !UsedModules),
    list.map_foldl3(replace_in_subst(MaybeRecord, TypeEqvMap),
        TailSubsts0, TailSubsts, TVarSet3, TVarSet,
        ExpandedItems3, ExpandedItems, !UsedModules),
    OoMSubsts = one_or_more(HeadSubst, TailSubsts),
    (
        ExpandedItems = no_eqv_expand_info,
        ItemIds = ItemIds0
    ;
        ExpandedItems = eqv_expand_info(_, ItemIds)
    ),
    TypeSpecInfoConstr = decl_pragma_type_spec_constr_info(PragmaModuleName,
        OoMConstraints, ApplyToSupers, OoMSubsts, TVarSet, ItemIds,
        Context, SeqNum).

:- pred replace_in_decl_pragma_type_spec(module_name::in,
    maybe_record_sym_name_use::in, type_eqv_map::in, inst_eqv_map::in,
    decl_pragma_type_spec_info::in, decl_pragma_type_spec_info::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out, list(error_spec)::out) is det.

replace_in_decl_pragma_type_spec(ModuleName, MaybeRecord,
        TypeEqvMap, _InstEqvMap, TypeSpecInfo0, TypeSpecInfo,
        RecompInfo, RecompInfo, !UsedModules, []) :-
    % Returning RecompInfo unchanged is required by the interface
    % of replace_in_list.
    TypeSpecInfo0 = decl_pragma_type_spec_info(PFUMM, PredName, NewName,
        Subst0, TVarSet0, ItemIds0, Context, SeqNum),
    ( if
        ( RecompInfo = no
        ; PredName = qualified(ModuleName, _)
        )
    then
        ExpandedItems0 = no_eqv_expand_info
    else
        ExpandedItems0 = eqv_expand_info(ModuleName, ItemIds0)
    ),
    replace_in_subst(MaybeRecord, TypeEqvMap, Subst0, Subst,
        TVarSet0, TVarSet, ExpandedItems0, ExpandedItems, !UsedModules),
    (
        ExpandedItems = no_eqv_expand_info,
        ItemIds = ItemIds0
    ;
        ExpandedItems = eqv_expand_info(_, ItemIds)
    ),
    TypeSpecInfo = decl_pragma_type_spec_info(PFUMM, PredName, NewName,
        Subst, TVarSet, ItemIds, Context, SeqNum).

%---------------------%

:- pred replace_in_foreign_proc(module_name::in,
    maybe_record_sym_name_use::in, type_eqv_map::in, inst_eqv_map::in,
    item_foreign_proc_info::in, item_foreign_proc_info::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out, list(error_spec)::out) is det.

replace_in_foreign_proc(ModuleName, MaybeRecord, TypeEqvMap, _InstEqvMap,
        FPInfo0, FPInfo, !RecompInfo, !UsedModules, []) :-
    FPInfo0 = item_foreign_proc_info(Attrs0, PName, PredOrFunc,
        ProcVars, ProcVarset, ProcInstVarset, ProcImpl, Context, SeqNum),
    some [!EquivTypeInfo] (
        maybe_start_recording_expanded_items(ModuleName, PName,
            !.RecompInfo, !:EquivTypeInfo),
        UserSharing0 = get_user_annotated_sharing(Attrs0),
        ( if
            UserSharing0 = user_sharing(Sharing0, MaybeTypes0),
            MaybeTypes0 = yes(user_type_info(Types0, TVarSet0))
        then
            replace_in_type_list_location(MaybeRecord,
                TypeEqvMap, Types0, Types, _AnythingChanged,
                TVarSet0, TVarSet, !EquivTypeInfo, !UsedModules),
            replace_in_structure_sharing_domain(MaybeRecord, TypeEqvMap,
                TVarSet0, Sharing0, Sharing, !EquivTypeInfo, !UsedModules),
            MaybeTypes = yes(user_type_info(Types, TVarSet)),
            UserSharing = user_sharing(Sharing, MaybeTypes),
            set_user_annotated_sharing(UserSharing, Attrs0, Attrs)
        else
            Attrs = Attrs0
        ),
        ItemName = recomp_item_name(PName, list.length(ProcVars)),
        ItemId = recomp_item_id(recomp_foreign_proc, ItemName),
        finish_recording_expanded_items(ItemId, !.EquivTypeInfo, !RecompInfo)
    ),
    FPInfo = item_foreign_proc_info(Attrs, PName, PredOrFunc,
        ProcVars, ProcVarset, ProcInstVarset, ProcImpl, Context, SeqNum).

%---------------------%

:- pred replace_in_mutable_info(module_name::in, maybe_record_sym_name_use::in,
    type_eqv_map::in, inst_eqv_map::in,
    item_mutable_info::in, item_mutable_info::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out, list(error_spec)::out) is det.

replace_in_mutable_info(ModuleName, MaybeRecord, TypeEqvMap, InstEqvMap,
        Info0, Info, !RecompInfo, !UsedModules, []) :-
    MutName = Info0 ^ mut_name,
    QualName = qualified(ModuleName, MutName),
    maybe_start_recording_expanded_items(ModuleName, QualName, !.RecompInfo,
        ExpandedItems0),
    replace_in_mutable_defn(MaybeRecord, TypeEqvMap, InstEqvMap, Info0, Info,
        ExpandedItems0, ExpandedItems, !UsedModules),
    ItemId = recomp_item_id(recomp_mutable, recomp_item_name(QualName, 0)),
    finish_recording_expanded_items(ItemId, ExpandedItems, !RecompInfo).

:- pred replace_in_mutable_defn(maybe_record_sym_name_use::in,
    type_eqv_map::in, inst_eqv_map::in,
    item_mutable_info::in, item_mutable_info::out,
    eqv_expand_info::in, eqv_expand_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_mutable_defn(MaybeRecord, TypeEqvMap, InstEqvMap, Info0, Info,
        !ExpandedItems, !UsedModules) :-
    Info0 = item_mutable_info(MutName, OrigType, Type0, OrigInst, Inst0,
        InitValue, Attrs, Varset, Context, SeqNum),
    TVarSet0 = varset.init,
    replace_in_type_maybe_record_use(MaybeRecord, TypeEqvMap, Type0, Type,
        _TypeChanged, TVarSet0, _TVarSet, !ExpandedItems, !UsedModules),
    replace_in_inst(MaybeRecord, InstEqvMap, Inst0, Inst, !ExpandedItems,
        !UsedModules),
    Info = item_mutable_info(MutName, OrigType, Type, OrigInst, Inst,
        InitValue, Attrs, Varset, Context, SeqNum).

%---------------------%

:- pred replace_in_event_specs(type_eqv_map::in,
    assoc_list(string, event_spec)::in, assoc_list(string, event_spec)::out,
    used_modules::in, used_modules::out) is det.

replace_in_event_specs(_, [], [], !UsedModules).
replace_in_event_specs(TypeEqvMap,
        [Name - EventSpec0 | NameSpecs0], [Name - EventSpec | NameSpecs],
        !UsedModules) :-
    replace_in_event_spec(TypeEqvMap, EventSpec0, EventSpec, !UsedModules),
    replace_in_event_specs(TypeEqvMap, NameSpecs0, NameSpecs, !UsedModules).

:- pred replace_in_event_spec(type_eqv_map::in,
    event_spec::in, event_spec::out,
    used_modules::in, used_modules::out) is det.

replace_in_event_spec(TypeEqvMap, EventSpec0, EventSpec, !UsedModules) :-
    EventSpec0 = event_spec(EventNumber, EventName, EventLineNumber,
        Attrs0, SyntAttrNumOrder),
    replace_in_event_attrs(TypeEqvMap, Attrs0, Attrs, !UsedModules),
    EventSpec = event_spec(EventNumber, EventName, EventLineNumber,
        Attrs, SyntAttrNumOrder).

:- pred replace_in_event_attrs(type_eqv_map::in,
    list(event_attribute)::in, list(event_attribute)::out,
    used_modules::in, used_modules::out) is det.

replace_in_event_attrs(_TypeEqvMap, [], [], !UsedModules).
replace_in_event_attrs(TypeEqvMap, [Attr0 | Attrs0], [Attr | Attrs],
        !UsedModules) :-
    replace_in_event_attr(TypeEqvMap, Attr0, Attr, !UsedModules),
    replace_in_event_attrs(TypeEqvMap, Attrs0, Attrs, !UsedModules).

:- pred replace_in_event_attr(type_eqv_map::in,
    event_attribute::in, event_attribute::out,
    used_modules::in, used_modules::out) is det.

replace_in_event_attr(TypeEqvMap, Attr0, Attr, !UsedModules) :-
    % We construct the attributes' modes ourselves in event_spec.m; they should
    % not contain type names.
    Attr0 = event_attribute(AttrNum, AttrName, AttrType0, AttrMode,
        MaybeSynthCall),
    TVarSet0 = varset.init,
    replace_in_type_maybe_record_use(do_not_record_sym_name_use, TypeEqvMap,
        AttrType0, AttrType, _Changed, TVarSet0, _TVarSet,
        no_eqv_expand_info, _EquivTypeInfo, !UsedModules),
    Attr = event_attribute(AttrNum, AttrName, AttrType, AttrMode,
        MaybeSynthCall).

%---------------------------------------------------------------------------%

:- pred replace_in_type_defn(maybe_record_sym_name_use::in,
    type_eqv_map::in, inst_eqv_map::in, type_ctor::in, prog_context::in,
    type_defn::in, type_defn::out,
    tvarset::in, tvarset::out, eqv_expand_info::in, eqv_expand_info::out,
    used_modules::in, used_modules::out, list(error_spec)::out) is det.

replace_in_type_defn(MaybeRecord, TypeEqvMap, InstEqvMap, TypeCtor, Context,
        TypeDefn0, TypeDefn, !TVarSet, !EquivTypeInfo, !UsedModules, Specs) :-
    (
        TypeDefn0 = parse_tree_eqv_type(DetailsEqv0),
        replace_in_type_defn_eqv(MaybeRecord, TypeEqvMap, InstEqvMap,
            TypeCtor, Context, DetailsEqv0, DetailsEqv,
            !TVarSet, !EquivTypeInfo, !UsedModules, Specs),
        TypeDefn = parse_tree_eqv_type(DetailsEqv)
    ;
        TypeDefn0 = parse_tree_du_type(DetailsDu0),
        replace_in_type_defn_du(MaybeRecord, TypeEqvMap, InstEqvMap,
            TypeCtor, Context, DetailsDu0, DetailsDu,
            !TVarSet, !EquivTypeInfo, !UsedModules, Specs),
        TypeDefn = parse_tree_du_type(DetailsDu)
    ;
        TypeDefn0 = parse_tree_sub_type(DetailsSub0),
        replace_in_type_defn_sub(MaybeRecord, TypeEqvMap, InstEqvMap,
            TypeCtor, Context, DetailsSub0, DetailsSub,
            !TVarSet, !EquivTypeInfo, !UsedModules, Specs),
        TypeDefn = parse_tree_sub_type(DetailsSub)
    ;
        TypeDefn0 = parse_tree_solver_type(DetailsSolver0),
        replace_in_type_defn_solver(MaybeRecord, TypeEqvMap, InstEqvMap,
            TypeCtor, Context, DetailsSolver0, DetailsSolver,
            !TVarSet, !EquivTypeInfo, !UsedModules, Specs),
        TypeDefn = parse_tree_solver_type(DetailsSolver)
    ;
        ( TypeDefn0 = parse_tree_abstract_type(_)
        ; TypeDefn0 = parse_tree_foreign_type(_)
        ),
        TypeDefn = TypeDefn0,
        Specs = []
    ).

:- pred replace_in_type_defn_eqv(maybe_record_sym_name_use::in,
    type_eqv_map::in, inst_eqv_map::in, type_ctor::in, prog_context::in,
    type_details_eqv::in, type_details_eqv::out,
    tvarset::in, tvarset::out, eqv_expand_info::in, eqv_expand_info::out,
    used_modules::in, used_modules::out, list(error_spec)::out) is det.

replace_in_type_defn_eqv(MaybeRecord, TypeEqvMap, _InstEqvMap, TypeCtor,
        Context, DetailsEqv0, DetailsEqv,
        !TVarSet, !EquivTypeInfo, !UsedModules, Specs) :-
    DetailsEqv0 = type_details_eqv(TypeBody0),
    replace_in_type_maybe_record_use_2(MaybeRecord, TypeEqvMap, [TypeCtor],
        TypeBody0, TypeBody, _, Circ, !TVarSet, !EquivTypeInfo, !UsedModules),
    set.to_sorted_list(Circ, CircTypes),
    (
        CircTypes = [_ | _],
        Specs = [report_circular_eqv_type(TypeCtor, Context)]
    ;
        CircTypes = [],
        Specs = []
    ),
    DetailsEqv = type_details_eqv(TypeBody).

:- func report_circular_eqv_type(type_ctor, prog_context) = error_spec.

report_circular_eqv_type(TypeCtor, Context) = Spec :-
    Pieces = [words("Error: equivalence type")] ++
        color_as_subject([qual_type_ctor(TypeCtor)]) ++
        [words("is")] ++
        color_as_incorrect([words("circular.")]) ++
        [nl],
    Spec = spec($pred, severity_error, phase_expand_types, Context, Pieces).

:- func report_contains_circular_eqv_type(tvarset, mer_type, prog_context,
    type_ctor, list(type_ctor)) = error_spec.

report_contains_circular_eqv_type(TVarSet, Type, Context,
        HeadTypeCtor, TailTypeCtors) = Spec :-
    TypeStr = mercury_type_to_string(TVarSet, print_name_only, Type),
    MainPieces = [words("Error: the type")] ++
        color_as_subject([quote(TypeStr)]) ++
        [words("cannot have its equivalences fully expanded,"),
        words("because its expansion contains the")],
    (
        TailTypeCtors = [],
        CircSpecs =
            color_as_incorrect([words("circular equivalence type")]) ++
            color_as_subject([qual_type_ctor(HeadTypeCtor), suffix(".")]) ++
            [nl]
    ;
        TailTypeCtors = [_ | _],
        TypeCtorPieces = list.map((func(TC) = qual_type_ctor(TC)),
            [HeadTypeCtor | TailTypeCtors]),
        CircSpecs =
            color_as_incorrect([words("circular equivalence types")]) ++
            piece_list_to_color_pieces(color_subject, "and", [suffix(".")],
                TypeCtorPieces) ++
            [nl]
    ),
    Pieces = MainPieces ++ CircSpecs,
    Spec = spec($pred, severity_error, phase_expand_types, Context, Pieces).

:- pred replace_in_type_defn_du(maybe_record_sym_name_use::in,
    type_eqv_map::in, inst_eqv_map::in, type_ctor::in, prog_context::in,
    type_details_du::in, type_details_du::out,
    tvarset::in, tvarset::out, eqv_expand_info::in, eqv_expand_info::out,
    used_modules::in, used_modules::out, list(error_spec)::out) is det.

replace_in_type_defn_du(MaybeRecord, TypeEqvMap, _InstEqvMap,
        _TypeCtor, _Context, DetailsDu0, DetailsDu,
        !TVarSet, !EquivTypeInfo, !UsedModules, Specs) :-
    DetailsDu0 = type_details_du(Ctors0, MaybeCanon, DirectArgFunctors),
    replace_in_ctors_location(MaybeRecord, TypeEqvMap, Ctors0, Ctors,
        !TVarSet, !EquivTypeInfo, !UsedModules),
    DetailsDu = type_details_du(Ctors, MaybeCanon, DirectArgFunctors),
    Specs = [].

:- pred replace_in_type_defn_sub(maybe_record_sym_name_use::in,
    type_eqv_map::in, inst_eqv_map::in, type_ctor::in, prog_context::in,
    type_details_sub::in, type_details_sub::out,
    tvarset::in, tvarset::out, eqv_expand_info::in, eqv_expand_info::out,
    used_modules::in, used_modules::out, list(error_spec)::out) is det.

replace_in_type_defn_sub(MaybeRecord, TypeEqvMap, _InstEqvMap,
        _TypeCtor, _Context, DetailsSub0, DetailsSub,
        !TVarSet, !EquivTypeInfo, !UsedModules, Specs) :-
    DetailsSub0 = type_details_sub(SuperType0, Ctors0),
    replace_in_type_maybe_record_use(MaybeRecord, TypeEqvMap,
        SuperType0, SuperType, _, !TVarSet, !EquivTypeInfo, !UsedModules),
    replace_in_ctors_location(MaybeRecord, TypeEqvMap, Ctors0, Ctors,
        !TVarSet, !EquivTypeInfo, !UsedModules),
    DetailsSub = type_details_sub(SuperType, Ctors),
    Specs = [].

:- pred replace_in_type_defn_solver(maybe_record_sym_name_use::in,
    type_eqv_map::in, inst_eqv_map::in, type_ctor::in, prog_context::in,
    type_details_solver::in, type_details_solver::out,
    tvarset::in, tvarset::out, eqv_expand_info::in, eqv_expand_info::out,
    used_modules::in, used_modules::out, list(error_spec)::out) is det.

replace_in_type_defn_solver(MaybeRecord, TypeEqvMap, InstEqvMap, TypeCtor,
        Context, DetailsSolver0, DetailsSolver,
        !TVarSet, !EquivTypeInfo, !UsedModules, Specs) :-
    DetailsSolver0 = type_details_solver(SolverDetails0, MaybeUserEqComp),
    SolverDetails0 = solver_type_details(RepresentationType0,
        GroundInst, AnyInst, MutableInfos0),
    replace_in_type_maybe_record_use_2(MaybeRecord, TypeEqvMap, [TypeCtor],
        RepresentationType0, RepresentationType,
        _Changed, Circ, !TVarSet, !EquivTypeInfo, !UsedModules),
    set.to_sorted_list(Circ, CircTypes),
    (
        CircTypes = [_ | _],
        % We used to abort the compiler if we found circular equivalence types
        % in any non-equivalence type definition. I (zs) don't remember
        % hearing about it ever being triggered in the wild, but it
        % *does* get triggered by code such as
        % ":- solver type foo where representation is foo, ...".
        %
        % XXX I (zs) don't know in what other scenarios we may find
        % circular equivalence types, so the wording of this message
        % is targeted towards the above scenario.
        Pieces = [words("Error: circular type expansion"),
            words("in the representation of solver type"),
            qual_type_ctor(TypeCtor), suffix("."), nl],
        Specs = [spec($pred, severity_error, phase_expand_types,
            Context, Pieces)]
    ;
        CircTypes = [],
        Specs = []
    ),
    replace_in_constraint_store(MaybeRecord, TypeEqvMap, InstEqvMap,
        MutableInfos0, MutableInfos, !EquivTypeInfo, !UsedModules),
    SolverDetails = solver_type_details(RepresentationType,
        GroundInst, AnyInst, MutableInfos),
    DetailsSolver = type_details_solver(SolverDetails, MaybeUserEqComp).

%---------------------------------------------------------------------------%

replace_in_type_report_circular_eqvs(TypeEqvMap, TVarSet0, Context,
        Type0, Type, Changed, !Specs) :-
    replace_in_type_maybe_record_use_2(do_not_record_sym_name_use,
        TypeEqvMap, [], Type0, Type, Changed, Circ,
        TVarSet0, _TVarSet, no_eqv_expand_info, _, used_modules_init, _),
    set.to_sorted_list(Circ, CircTypes),
    (
        CircTypes = [HeadCircTypeCtor | TailCircTypeCtors],
        Spec = report_contains_circular_eqv_type(TVarSet0, Type0, Context,
            HeadCircTypeCtor, TailCircTypeCtors),
        !:Specs = [Spec | !.Specs]
    ;
        CircTypes = []
    ).

replace_in_type(TypeEqvMap, Type0, Type, Changed, !TVarSet, !EquivTypeInfo) :-
    replace_in_type_maybe_record_use_2(do_not_record_sym_name_use,
        TypeEqvMap, [], Type0, Type, Changed, _Circ, !TVarSet,
        !EquivTypeInfo, used_modules_init, _).

:- pred replace_in_type_maybe_record_use(maybe_record_sym_name_use::in,
    type_eqv_map::in, mer_type::in, mer_type::out, maybe_changed::out,
    tvarset::in, tvarset::out, eqv_expand_info::in, eqv_expand_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_type_maybe_record_use(MaybeRecord, TypeEqvMap,
        Type0, Type, Changed, !TVarSet, !EquivTypeInfo, !UsedModules) :-
    replace_in_type_maybe_record_use_2(MaybeRecord, TypeEqvMap, [],
        Type0, Type, Changed, _, !TVarSet, !EquivTypeInfo, !UsedModules).

    % Replace all equivalence types in a given type, detecting
    % any circularities.
    %
:- pred replace_in_type_maybe_record_use_2(maybe_record_sym_name_use::in,
    type_eqv_map::in, list(type_ctor)::in, mer_type::in, mer_type::out,
    maybe_changed::out, circ_types::out, tvarset::in, tvarset::out,
    eqv_expand_info::in, eqv_expand_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_type_maybe_record_use_2(MaybeRecord, TypeEqvMap,
        TypeCtorsAlreadyExpanded, Type0, Type, Changed, Circ,
        !TVarSet, !EquivTypeInfo, !UsedModules) :-
    (
        Type0 = type_variable(Var, Kind),
        Type = type_variable(Var, Kind),
        Changed = unchanged,
        Circ = set.init
    ;
        Type0 = defined_type(SymName, TArgs0, Kind),
        replace_in_type_list_location_circ_2(MaybeRecord, TypeEqvMap,
            TypeCtorsAlreadyExpanded, TArgs0, TArgs, ArgsChanged,
            set.init, Circ0, !TVarSet, !EquivTypeInfo, !UsedModules),
        Arity = list.length(TArgs),
        TypeCtor = type_ctor(SymName, Arity),
        replace_type_ctor(MaybeRecord, TypeEqvMap, TypeCtorsAlreadyExpanded,
            Type0, TypeCtor, TArgs, Kind, Type, ArgsChanged, Changed,
            Circ0, Circ, !TVarSet, !EquivTypeInfo, !UsedModules)
    ;
        Type0 = builtin_type(_),
        Type = Type0,
        Changed = unchanged,
        Circ = set.init
    ;
        Type0 = higher_order_type(PorF, Args0, HOInstInfo, Purity),
        replace_in_type_list_location_circ_2(MaybeRecord, TypeEqvMap,
            TypeCtorsAlreadyExpanded, Args0, Args, Changed, set.init, Circ,
            !TVarSet, !EquivTypeInfo, !UsedModules),
        (
            Changed = changed,
            Type = higher_order_type(PorF, Args, HOInstInfo, Purity)
        ;
            Changed = unchanged,
            Type = Type0
        )
    ;
        Type0 = tuple_type(Args0, Kind),
        replace_in_type_list_location_circ_2(MaybeRecord, TypeEqvMap,
            TypeCtorsAlreadyExpanded, Args0, Args, Changed, set.init, Circ,
            !TVarSet, !EquivTypeInfo, !UsedModules),
        (
            Changed = changed,
            Type = tuple_type(Args, Kind)
        ;
            Changed = unchanged,
            Type = Type0
        )
    ;
        Type0 = apply_n_type(Var, Args0, Kind),
        replace_in_type_list_location_circ_2(MaybeRecord, TypeEqvMap,
            TypeCtorsAlreadyExpanded, Args0, Args, Changed, set.init, Circ,
            !TVarSet, !EquivTypeInfo, !UsedModules),
        (
            Changed = changed,
            Type = apply_n_type(Var, Args, Kind)
        ;
            Changed = unchanged,
            Type = Type0
        )
    ;
        Type0 = kinded_type(RawType0, Kind),
        replace_in_type_maybe_record_use_2(MaybeRecord, TypeEqvMap,
            TypeCtorsAlreadyExpanded, RawType0, RawType, Changed, Circ,
            !TVarSet, !EquivTypeInfo, !UsedModules),
        (
            Changed = changed,
            Type = kinded_type(RawType, Kind)
        ;
            Changed = unchanged,
            Type = Type0
        )
    ).

:- pred replace_type_ctor(maybe_record_sym_name_use::in, type_eqv_map::in,
    list(type_ctor)::in, mer_type::in, type_ctor::in, list(mer_type)::in,
    kind::in, mer_type::out, maybe_changed::in, maybe_changed::out,
    circ_types::in, circ_types::out, tvarset::in, tvarset::out,
    eqv_expand_info::in, eqv_expand_info::out,
    used_modules::in, used_modules::out) is det.

replace_type_ctor(MaybeRecord, TypeEqvMap, TypeCtorsAlreadyExpanded, Type0,
        TypeCtor, TArgs, Kind, Type, !Changed, !Circ, !TVarSet,
        !EquivTypeInfo, !UsedModules) :-
    ( if list.member(TypeCtor, TypeCtorsAlreadyExpanded) then
        AlreadyExpanded = yes,
        NewCirc = set.make_singleton_set(TypeCtor)
    else
        AlreadyExpanded = no,
        NewCirc = set.init
    ),
    ( if
        map.search(TypeEqvMap, TypeCtor, EqvTypeBody),
        EqvTypeBody = eqv_type_body(EqvTVarSet, Args0, Body0),

        % Don't merge in the variable names from the type declaration to avoid
        % creating multiple variables with the same name so that
        % `varset.create_name_var_map' can be used on the resulting tvarset.
        % make_hlds uses `varset.create_name_var_map' to match up type
        % variables in `:- pragma type_spec' declarations and explicit type
        % qualifications with the type variables in the predicate's
        % declaration.

        tvarset_merge_renaming_without_names(!.TVarSet, EqvTVarSet, !:TVarSet,
            Renaming),
        set.is_empty(!.Circ),
        AlreadyExpanded = no
    then
        maybe_record_type_ctor_sym_name_use(MaybeRecord, TypeCtor,
            !UsedModules),

        !:Changed = changed,
        map.apply_to_list(Args0, Renaming, Args),
        apply_variable_renaming_to_type(Renaming, Body0, Body1),
        TypeCtorItem = type_ctor_to_recomp_item_name(TypeCtor),
        record_expanded_item(recomp_item_id(recomp_type_name, TypeCtorItem),
            !EquivTypeInfo),
        map.from_corresponding_lists(Args, TArgs, Subst),
        apply_subst_to_type(Subst, Body1, Body),
        replace_in_type_maybe_record_use_2(MaybeRecord, TypeEqvMap,
            [TypeCtor | TypeCtorsAlreadyExpanded], Body,
            Type, _, !:Circ, !TVarSet, !EquivTypeInfo, !UsedModules)
    else
        (
            !.Changed = changed,
            TypeCtor = type_ctor(SymName, _Arity),
            Type = defined_type(SymName, TArgs, Kind)
        ;
            !.Changed = unchanged,
            Type = Type0
        ),
        set.union(NewCirc, !Circ)
    ).

%---------------------------------------------------------------------------%

replace_in_type_list(TypeEqvMap, !Types, Changed, !TVarSet, !EquivTypeInfo) :-
    replace_in_type_list_location(do_not_record_sym_name_use, TypeEqvMap,
        !Types, Changed, !TVarSet, !EquivTypeInfo, used_modules_init, _).

:- pred replace_in_type_list_location(maybe_record_sym_name_use::in,
    type_eqv_map::in, list(mer_type)::in, list(mer_type)::out,
    maybe_changed::out, tvarset::in, tvarset::out,
    eqv_expand_info::in, eqv_expand_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_type_list_location(MaybeRecord, TypeEqvMap, !Types,
        Changed, !TVarSet, !EquivTypeInfo, !UsedModules) :-
    replace_in_type_list_location_circ_2(MaybeRecord, TypeEqvMap, [], !Types,
        Changed, set.init, _, !TVarSet, !EquivTypeInfo, !UsedModules).

:- pred replace_in_type_list_location_circ(maybe_record_sym_name_use::in,
    type_eqv_map::in, list(mer_type)::in, list(mer_type)::out,
    maybe_changed::out, circ_types::out, tvarset::in, tvarset::out,
    eqv_expand_info::in, eqv_expand_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_type_list_location_circ(MaybeRecord, TypeEqvMap, !Types,
        Changed, ContainsCirc, !TVarSet, !EquivTypeInfo, !UsedModules) :-
    replace_in_type_list_location_circ_2(MaybeRecord, TypeEqvMap, [], !Types,
        Changed, set.init, ContainsCirc, !TVarSet,
        !EquivTypeInfo, !UsedModules).

:- pred replace_in_type_list_location_circ_2(maybe_record_sym_name_use::in,
    type_eqv_map::in, list(type_ctor)::in,
    list(mer_type)::in, list(mer_type)::out,
    maybe_changed::out, circ_types::in, circ_types::out,
    tvarset::in, tvarset::out, eqv_expand_info::in, eqv_expand_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_type_list_location_circ_2(_MaybeRecord, _TypeEqvMap, _Seen,
        [], [], unchanged, !ContainsCirc, !TVarSet,
        !EquivTypeInfo, !UsedModules).
replace_in_type_list_location_circ_2(MaybeRecord, TypeEqvMap, Seen,
        Types0 @ [HeadType0 | TailTypes0], Types, Changed, !Circ, !TVarSet,
        !EquivTypeInfo, !UsedModules) :-
    replace_in_type_maybe_record_use_2(MaybeRecord, TypeEqvMap, Seen,
        HeadType0, HeadType, HeadChanged, HeadCirc, !TVarSet,
        !EquivTypeInfo, !UsedModules),
    set.union(HeadCirc, !Circ),
    replace_in_type_list_location_circ_2(MaybeRecord, TypeEqvMap, Seen,
        TailTypes0, TailTypes, TailChanged, !Circ, !TVarSet,
        !EquivTypeInfo, !UsedModules),
    ( if
        ( HeadChanged = changed
        ; TailChanged = changed
        )
    then
        Changed = changed,
        Types = [HeadType | TailTypes]
    else
        Changed = unchanged,
        Types = Types0
    ).

%---------------------------------------------------------------------------%

:- pred replace_in_ctor_arg_list(maybe_record_sym_name_use::in,
    type_eqv_map::in, list(constructor_arg)::in, list(constructor_arg)::out,
    circ_types::out, tvarset::in, tvarset::out,
    eqv_expand_info::in, eqv_expand_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_ctor_arg_list(MaybeRecord, TypeEqvMap, !Args,
        ContainsCirc, !TVarSet, !EquivTypeInfo, !UsedModules) :-
    replace_in_ctor_arg_list_loop(MaybeRecord, TypeEqvMap, [], !Args,
        set.init, ContainsCirc, !TVarSet, !EquivTypeInfo, !UsedModules).

:- pred replace_in_ctor_arg_list_loop(maybe_record_sym_name_use::in,
    type_eqv_map::in, list(type_ctor)::in,
    list(constructor_arg)::in, list(constructor_arg)::out,
    circ_types::in, circ_types::out, tvarset::in, tvarset::out,
    eqv_expand_info::in, eqv_expand_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_ctor_arg_list_loop(_MaybeRecord, _TypeEqvMap, _Seen, [], [],
        !Circ, !TVarSet, !EquivTypeInfo, !UsedModules).
replace_in_ctor_arg_list_loop(MaybeRecord, TypeEqvMap, Seen,
        [Arg0 | Args0], [Arg | Args],
        !Circ, !TVarSet, !EquivTypeInfo, !UsedModules) :-
    Arg0 = ctor_arg(Name, Type0, Context),
    replace_in_type_maybe_record_use_2(MaybeRecord, TypeEqvMap, Seen,
        Type0, Type, _, TypeCirc, !TVarSet, !EquivTypeInfo, !UsedModules),
    Arg = ctor_arg(Name, Type, Context),
    set.union(TypeCirc, !Circ),
    replace_in_ctor_arg_list_loop(MaybeRecord, TypeEqvMap, Seen, Args0, Args,
        !Circ, !TVarSet, !EquivTypeInfo, !UsedModules).

%---------------------------------------------------------------------------%

:- pred replace_in_constraint_store(maybe_record_sym_name_use::in,
    type_eqv_map::in, inst_eqv_map::in,
    list(item_mutable_info)::in, list(item_mutable_info)::out,
    eqv_expand_info::in, eqv_expand_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_constraint_store(_, _, _, [], [], !EquivTypeInfo, !UsedModules).
replace_in_constraint_store(MaybeRecord, TypeEqvMap, InstEqvMap,
        [MutableInfo0 | MutableInfos0], [MutableInfo | MutableInfos],
        !EquivTypeInfo, !UsedModules) :-
    replace_in_mutable_defn(MaybeRecord, TypeEqvMap, InstEqvMap,
        MutableInfo0, MutableInfo, !EquivTypeInfo, !UsedModules),
    replace_in_constraint_store(MaybeRecord, TypeEqvMap, InstEqvMap,
        MutableInfos0, MutableInfos, !EquivTypeInfo, !UsedModules).

%---------------------------------------------------------------------------%

replace_in_univ_exist_constraints(TypeEqvMap, Cs0, Cs,
        !TVarSet, !EquivTypeInfo) :-
    replace_in_univ_exist_constraints_location(do_not_record_sym_name_use,
        TypeEqvMap, Cs0, Cs, !TVarSet, !EquivTypeInfo, used_modules_init, _).

:- pred replace_in_univ_exist_constraints_location(
    maybe_record_sym_name_use::in, type_eqv_map::in,
    univ_exist_constraints::in, univ_exist_constraints::out,
    tvarset::in, tvarset::out, eqv_expand_info::in, eqv_expand_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_univ_exist_constraints_location(MaybeRecord, TypeEqvMap, Cs0, Cs,
        !TVarSet, !EquivTypeInfo, !UsedModules) :-
    Cs0 = univ_exist_constraints(UnivCs0, ExistCs0),
    replace_in_prog_constraint_list_location(MaybeRecord, TypeEqvMap,
        UnivCs0, UnivCs, !TVarSet, !EquivTypeInfo, !UsedModules),
    replace_in_prog_constraint_list_location(MaybeRecord, TypeEqvMap,
        ExistCs0, ExistCs, !TVarSet, !EquivTypeInfo, !UsedModules),
    Cs = univ_exist_constraints(UnivCs, ExistCs).

replace_in_prog_constraint_list(TypeEqvMap,
        !Constraints, !TVarSet, !EquivTypeInfo) :-
    replace_in_prog_constraint_list_location(do_not_record_sym_name_use,
        TypeEqvMap, !Constraints,
        !TVarSet, !EquivTypeInfo, used_modules_init, _).

:- pred replace_in_prog_constraint_list_location(maybe_record_sym_name_use::in,
    type_eqv_map::in, list(prog_constraint)::in, list(prog_constraint)::out,
    tvarset::in, tvarset::out, eqv_expand_info::in, eqv_expand_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_prog_constraint_list_location(MaybeRecord, TypeEqvMap,
        !Constraints, !TVarSet, !EquivTypeInfo, !UsedModules) :-
    list.map_foldl3(
        replace_in_prog_constraint_location(MaybeRecord, TypeEqvMap),
        !Constraints, !TVarSet, !EquivTypeInfo, !UsedModules).

:- pred replace_in_prog_constraint_location(maybe_record_sym_name_use::in,
    type_eqv_map::in, prog_constraint::in, prog_constraint::out,
    tvarset::in, tvarset::out, eqv_expand_info::in, eqv_expand_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_prog_constraint_location(MaybeRecord, TypeEqvMap,
        Constraint0, Constraint, !TVarSet, !EquivTypeInfo, !UsedModules) :-
    Constraint0 = constraint(ClassName, ArgTypes0),
    replace_in_type_list_location_circ(MaybeRecord, TypeEqvMap,
        ArgTypes0, ArgTypes, _, _, !TVarSet, !EquivTypeInfo, !UsedModules),
    Constraint = constraint(ClassName, ArgTypes).

%---------------------%

:- pred replace_in_var_or_ground_constraint_location(
    maybe_record_sym_name_use::in, type_eqv_map::in,
    var_or_ground_constraint::in, var_or_ground_constraint::out,
    tvarset::in, tvarset::out, eqv_expand_info::in, eqv_expand_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_var_or_ground_constraint_location(MaybeRecord, TypeEqvMap,
        Constraint0, Constraint, !TVarSet, !EquivTypeInfo, !UsedModules) :-
    Constraint0 = var_or_ground_constraint(ClassName, Args0, Context),
    list.map_foldl3(
        replace_in_var_or_ground_type_location(MaybeRecord, TypeEqvMap),
        Args0, Args, !TVarSet, !EquivTypeInfo, !UsedModules),
    Constraint = var_or_ground_constraint(ClassName, Args, Context).

:- pred replace_in_var_or_ground_type_location(maybe_record_sym_name_use::in,
    type_eqv_map::in, var_or_ground_type::in, var_or_ground_type::out,
    tvarset::in, tvarset::out, eqv_expand_info::in, eqv_expand_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_var_or_ground_type_location(MaybeRecord, TypeEqvMap,
        Arg0, Arg, !TVarSet, !EquivTypeInfo, !UsedModules) :-
    (
        Arg0 = type_var_name(_, _),
        Arg = Arg0
    ;
        Arg0 = ground_type(GroundType0),
        Type0 = coerce(GroundType0),
        replace_in_type_maybe_record_use(MaybeRecord, TypeEqvMap,
            Type0, Type, _, !TVarSet, !EquivTypeInfo, !UsedModules),
        ( if type_is_ground(Type, GroundType) then
            Arg = ground_type(GroundType)
        else
            unexpected($pred, "expanded ground type is not ground")
        )
    ).

%---------------------------------------------------------------------------%

:- pred replace_in_class_interface(maybe_record_sym_name_use::in,
    type_eqv_map::in, inst_eqv_map::in,
    list(class_decl)::in, list(class_decl)::out,
    eqv_expand_info::in, eqv_expand_info::out,
    used_modules::in, used_modules::out,
    list(error_spec)::in, list(error_spec)::out) is det.

replace_in_class_interface(MaybeRecord, TypeEqvMap, InstEqvMap,
        ClassInterface0, ClassInterface, !EquivTypeInfo, !UsedModules,
        !Specs) :-
    list.map_foldl3(
        replace_in_class_decl(MaybeRecord, TypeEqvMap, InstEqvMap),
        ClassInterface0, ClassInterface, !EquivTypeInfo, !UsedModules, !Specs).

:- pred replace_in_class_decl(maybe_record_sym_name_use::in,
    type_eqv_map::in, inst_eqv_map::in, class_decl::in, class_decl::out,
    eqv_expand_info::in, eqv_expand_info::out,
    used_modules::in, used_modules::out,
    list(error_spec)::in, list(error_spec)::out) is det.

replace_in_class_decl(MaybeRecord, TypeEqvMap, InstEqvMap, Decl0, Decl,
        !EquivTypeInfo, !UsedModules, !Specs) :-
    (
        Decl0 = class_decl_pred_or_func(PredOrFuncInfo0),
        PredOrFuncInfo0 = class_pred_or_func_info(PredName, PredOrFunc,
            TypesAndModes0, WithType0, WithInst0, MaybeDetism0,
            TVarSet0, InstVarSet, ExistQVars, Purity,
            ClassContext0, Context),
        replace_in_pred_types_and_maybe_modes(MaybeRecord, PredName,
            PredOrFunc, Context, TypeEqvMap, InstEqvMap,
            ClassContext0, ClassContext, TypesAndModes0, TypesAndModes,
            TVarSet0, TVarSet, WithType0, WithType, WithInst0, WithInst,
            MaybeDetism0, MaybeDetism, !EquivTypeInfo, !UsedModules, NewSpecs),
        !:Specs = NewSpecs ++ !.Specs,
        PredOrFuncInfo = class_pred_or_func_info(PredName, PredOrFunc,
            TypesAndModes, WithType, WithInst, MaybeDetism,
            TVarSet, InstVarSet, ExistQVars, Purity,
            ClassContext, Context),
        Decl = class_decl_pred_or_func(PredOrFuncInfo)
    ;
        Decl0 = class_decl_mode(ModeInfo0),
        ModeInfo0 = class_mode_info(PredName, MaybePredOrFunc0, Modes0,
            WithInst0, MaybeDetism0, InstVarSet, Context),
        PredFormArity = arg_list_arity(Modes0),
        replace_in_with_inst(MaybeRecord, InstEqvMap,
            PredName, PredFormArity, Context, mode_decl,
            MaybePredOrFunc0, MaybePredOrFunc, WithInst0, WithInst, ExtraModes,
            MaybeDetism0, MaybeDetism, !EquivTypeInfo, !UsedModules, NewSpecs),
        (
            ExtraModes = [],
            Modes = Modes0
        ;
            ExtraModes = [_ | _],
            Modes = Modes0 ++ ExtraModes
        ),
        !:Specs = NewSpecs ++ !.Specs,
        ModeInfo = class_mode_info(PredName, MaybePredOrFunc, Modes,
            WithInst, MaybeDetism, InstVarSet, Context),
        Decl = class_decl_mode(ModeInfo)
    ).

%---------------------------------------------------------------------------%

:- pred replace_in_subst(maybe_record_sym_name_use::in, type_eqv_map::in,
    type_subst::in, type_subst::out,
    tvarset::in, tvarset::out, eqv_expand_info::in, eqv_expand_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_subst(MaybeRecord, TypeEqvMap, Subst0, Subst,
        !TVarSet, !ExpandedItems, !UsedModules) :-
    Subst0 = one_or_more(HeadSubst0, TailSubsts0),
    replace_in_tvar_substs(MaybeRecord, TypeEqvMap,
        HeadSubst0, HeadSubst, TailSubsts0, TailSubsts,
        !TVarSet, !ExpandedItems, !UsedModules),
    Subst = one_or_more(HeadSubst, TailSubsts).

:- pred replace_in_tvar_substs(maybe_record_sym_name_use::in, type_eqv_map::in,
    tvar_subst::in, tvar_subst::out,
    list(tvar_subst)::in, list(tvar_subst)::out,
    tvarset::in, tvarset::out, eqv_expand_info::in, eqv_expand_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_tvar_substs(MaybeRecord, TypeEqvMap,
        tvar_subst(HeadVar, HeadType0), tvar_subst(HeadVar, HeadType),
        TailVarsTypes0, TailVarsTypes,
        !TVarSet, !ExpandedItems, !UsedModules) :-
    replace_in_type_maybe_record_use(MaybeRecord, TypeEqvMap,
        HeadType0, HeadType, _, !TVarSet, !ExpandedItems, !UsedModules),
    (
        TailVarsTypes0 = [],
        TailVarsTypes = []
    ;
        TailVarsTypes0 = [HeadTailVarType0 | TailTailVarsTypes0],
        replace_in_tvar_substs(MaybeRecord, TypeEqvMap,
            HeadTailVarType0, HeadTailVarType,
            TailTailVarsTypes0, TailTailVarsTypes,
            !TVarSet, !ExpandedItems, !UsedModules),
        TailVarsTypes = [HeadTailVarType | TailTailVarsTypes]
    ).

%---------------------------------------------------------------------------%

replace_in_ctors(TypeEqvMap, !Ctors, !TVarSet, !EquivTypeInfo) :-
    replace_in_ctors_location(do_not_record_sym_name_use, TypeEqvMap,
        !Ctors, !TVarSet, !EquivTypeInfo, used_modules_init, _).

:- pred replace_in_ctors_location(maybe_record_sym_name_use::in,
    type_eqv_map::in,
    one_or_more(constructor)::in, one_or_more(constructor)::out,
    tvarset::in, tvarset::out, eqv_expand_info::in, eqv_expand_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_ctors_location(MaybeRecord, TypeEqvMap, Ctors0, Ctors, !TVarSet,
        !EquivTypeInfo, !UsedModules) :-
    Ctors0 = one_or_more(HeadCtor0, TailCtors0),
    replace_in_ctor(MaybeRecord, TypeEqvMap, HeadCtor0, HeadCtor,
        !TVarSet, !EquivTypeInfo, !UsedModules),
    list.map_foldl3(replace_in_ctor(MaybeRecord, TypeEqvMap),
        TailCtors0, TailCtors,
        !TVarSet, !EquivTypeInfo, !UsedModules),
    Ctors = one_or_more(HeadCtor, TailCtors).

:- pred replace_in_ctor(maybe_record_sym_name_use::in, type_eqv_map::in,
    constructor::in, constructor::out, tvarset::in, tvarset::out,
    eqv_expand_info::in, eqv_expand_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_ctor(MaybeRecord, TypeEqvMap, Ctor0, Ctor,
        !TVarSet, !EquivTypeInfo, !UsedModules) :-
    Ctor0 = ctor(Ordinal, MaybeExistConstraints0, CtorName, CtorArgs0, Arity,
        Ctxt),
    replace_in_ctor_arg_list(MaybeRecord, TypeEqvMap,
        CtorArgs0, CtorArgs, _, !TVarSet, !EquivTypeInfo, !UsedModules),
    (
        MaybeExistConstraints0 = no_exist_constraints,
        MaybeExistConstraints = no_exist_constraints
    ;
        MaybeExistConstraints0 = exist_constraints(ExistConstraints0),
        ExistConstraints0 = cons_exist_constraints(ExistQVars, Constraints0,
            UnconstrainedExistQTVars, ConstrainedExistQTVars),
        replace_in_prog_constraint_list_location(MaybeRecord, TypeEqvMap,
            Constraints0, Constraints, !TVarSet, !EquivTypeInfo, !UsedModules),
        ExistConstraints = cons_exist_constraints(ExistQVars, Constraints,
            UnconstrainedExistQTVars, ConstrainedExistQTVars),
        MaybeExistConstraints = exist_constraints(ExistConstraints)
    ),
    Ctor = ctor(Ordinal, MaybeExistConstraints, CtorName, CtorArgs, Arity,
        Ctxt).

%---------------------------------------------------------------------------%

:- pred replace_in_inst(maybe_record_sym_name_use::in, inst_eqv_map::in,
    mer_inst::in, mer_inst::out, eqv_expand_info::in, eqv_expand_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_inst(MaybeRecord, InstEqvMap, Inst0, Inst,
        !EquivTypeInfo, !UsedModules) :-
    replace_in_inst_location(MaybeRecord, InstEqvMap, set.init, Inst0, Inst,
        !EquivTypeInfo, !UsedModules).

:- pred replace_in_inst_location(maybe_record_sym_name_use::in,
    inst_eqv_map::in, set(inst_ctor)::in, mer_inst::in, mer_inst::out,
    eqv_expand_info::in, eqv_expand_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_inst_location(MaybeRecord, InstEqvMap, ExpandedInstCtors,
        Inst0, Inst, !EquivTypeInfo, !UsedModules) :-
    % XXX Need to record the used modules
    ( if Inst0 = defined_inst(user_inst(SymName, ArgInsts)) then
        InstCtor = inst_ctor(SymName, length(ArgInsts)),
        ( if
            set.member(InstCtor, ExpandedInstCtors)
        then
            Inst = Inst0
        else if
            map.search(InstEqvMap, InstCtor, EqvInstBody),
            EqvInstBody = eqv_inst_body(EqvInstParams, EqvInst)
        then
            inst_substitute_arg_list(EqvInstParams, ArgInsts, EqvInst, Inst1),
            InstCtorItem = inst_ctor_to_recomp_item_name(InstCtor),
            record_expanded_item(recomp_item_id(recomp_inst, InstCtorItem),
                !EquivTypeInfo),
            replace_in_inst_location(MaybeRecord, InstEqvMap,
                set.insert(ExpandedInstCtors, InstCtor), Inst1, Inst,
                !EquivTypeInfo, !UsedModules)
        else
            Inst = Inst0
        )
    else
        Inst = Inst0
    ).

%---------------------------------------------------------------------------%

:- pred replace_in_pred_types_and_maybe_modes(maybe_record_sym_name_use::in,
    sym_name::in, pred_or_func::in, prog_context::in,
    type_eqv_map::in, inst_eqv_map::in,
    univ_exist_constraints::in, univ_exist_constraints::out,
    types_and_maybe_modes::in, types_and_maybe_modes::out,
    tvarset::in, tvarset::out,
    maybe(mer_type)::in, maybe(mer_type)::out,
    maybe(mer_inst)::in, maybe(mer_inst)::out,
    maybe(determinism)::in, maybe(determinism)::out,
    eqv_expand_info::in, eqv_expand_info::out,
    used_modules::in, used_modules::out, list(error_spec)::out) is det.

replace_in_pred_types_and_maybe_modes(MaybeRecord, PredName, PredOrFunc,
        Context, TypeEqvMap, InstEqvMap, ClassContext0, ClassContext,
        TypesAndMaybeModes0, TypesAndMaybeModes, !TVarSet,
        MaybeWithType0, MaybeWithType, MaybeWithInst0, MaybeWithInst,
        !MaybeDetism, !EquivTypeInfo, !UsedModules, !:Specs) :-
    replace_in_univ_exist_constraints_location(MaybeRecord, TypeEqvMap,
        ClassContext0, ClassContext, !TVarSet, !EquivTypeInfo, !UsedModules),
    replace_in_types_and_maybe_modes(MaybeRecord, TypeEqvMap,
        TypesAndMaybeModes0, TypesAndMaybeModes1,
        !TVarSet, !EquivTypeInfo, !UsedModules),
    (
        MaybeWithType0 = yes(WithType0),
        replace_in_type_maybe_record_use(MaybeRecord, TypeEqvMap,
            WithType0, WithType, _, !TVarSet, !EquivTypeInfo, !UsedModules),
        ( if
            type_is_higher_order_details(WithType, _Purity, PredOrFunc,
                ExtraTypesPrime)
        then
            ExtraTypes = ExtraTypesPrime,
            !:Specs = []
        else
            ExtraTypes = [],
            ExtraTypePieces = [words("In type declaration for"),
                p_or_f(PredOrFunc), qual_sym_name(PredName), suffix(":"), nl,
                words("error: expected the type after"), quote("with_type"),
                words("to be a")] ++
                color_as_correct([words("higher order"), p_or_f(PredOrFunc),
                    words("type,")]) ++
                [words("but")] ++
                color_as_incorrect([words("it is not.")]) ++
                [nl],
            ExtraTypeSpec = spec($pred, severity_error, phase_expand_types,
                Context, ExtraTypePieces),
            !:Specs = [ExtraTypeSpec]
        )
    ;
        MaybeWithType0 = no,
        ExtraTypes = [],
        !:Specs = []
    ),

    PredFormArity = types_and_maybe_modes_arity(TypesAndMaybeModes0),
    replace_in_with_inst(MaybeRecord, InstEqvMap, PredName, PredFormArity,
        Context, type_decl, yes(PredOrFunc), _, MaybeWithInst0, _, ExtraModes,
        !MaybeDetism, !EquivTypeInfo, !UsedModules, ModeSpecs),
    !:Specs = !.Specs ++ ModeSpecs,

    (
        !.Specs = [_ | _],
        TypesAndMaybeModes = TypesAndMaybeModes1
    ;
        !.Specs = [],
        ( if
            ExtraTypes = [],
            ExtraModes = []
        then
            % Optimize this common path.
            TypesAndMaybeModes = TypesAndMaybeModes1
        else
            (
                TypesAndMaybeModes1 = no_types_arity_zero,
                (
                    ExtraModes = [],
                    % ExtraTypes must be nonempty, because otherwise,
                    % we wouldn't get here.
                    TypesAndMaybeModes = types_only(ExtraTypes)
                ;
                    ExtraModes = [_ | _],
                    % ExtraTypes may be empty if we get here, but if it is,
                    % that is an error.
                    try_to_pair_extra_types_and_modes(PredOrFunc, PredName,
                        Context, ExtraTypes, ExtraModes,
                        MaybeExtraTypesAndModes),
                    (
                        MaybeExtraTypesAndModes = ok1(ExtraTypesAndModes),
                        TypesAndMaybeModes =
                            types_and_modes(ExtraTypesAndModes)
                    ;
                        MaybeExtraTypesAndModes = error1(ExtraSpecs),
                        TypesAndMaybeModes = TypesAndMaybeModes1,
                        !:Specs = ExtraSpecs ++ !.Specs
                    )
                )
            ;
                TypesAndMaybeModes1 = types_only(Types1),
                expect_not(unify(Types1, []), $pred, "Types1 = []"),
                (
                    ExtraModes = [],
                    Types = Types1 ++ ExtraTypes,
                    TypesAndMaybeModes = types_only(Types)
                ;
                    ExtraModes = [_ | _],
                    TypesAndMaybeModes = TypesAndMaybeModes1,
                    Pieces = pred_decl_error_prefix(PredOrFunc, PredName) ++
                        [words("error: the declaration"),
                        words("has a `with_inst` annotation,"),
                        words("but the declaration")] ++
                        color_as_incorrect([words("does not specify")]) ++
                        [words("the mode of any of the other arguments."), nl],
                    Spec = spec($pred, severity_error, phase_expand_types,
                        Context, Pieces),
                    !:Specs = [Spec | !.Specs]
                )
            ;
                TypesAndMaybeModes1 = types_and_modes(TypesAndModes1),
                expect_not(unify(TypesAndModes1, []), $pred,
                    "TypesAndModes1 = []"),
                try_to_pair_extra_types_and_modes(PredOrFunc, PredName,
                    Context, ExtraTypes, ExtraModes, MaybeExtraTypesAndModes),
                (
                    MaybeExtraTypesAndModes = ok1(ExtraTypesAndModes),
                    TypesAndModes = TypesAndModes1 ++ ExtraTypesAndModes,
                    TypesAndMaybeModes = types_and_modes(TypesAndModes)
                ;
                    MaybeExtraTypesAndModes = error1(ExtraSpecs),
                    TypesAndMaybeModes = TypesAndMaybeModes1,
                    !:Specs = ExtraSpecs ++ !.Specs
                )
            )
        )
    ),
    (
        !.Specs = [],
        MaybeWithType = no,
        MaybeWithInst = no
    ;
        !.Specs = [_ | _],
        % Leave the `with_type` and `with_inst` fields so that make_hlds knows
        % to discard this declaration.
        MaybeWithType = MaybeWithType0,
        MaybeWithInst = MaybeWithInst0
    ),
    ( if
        ExtraTypes = [],
        ExtraModes = []
    then
        true
    else
        PredFormArity = pred_form_arity(Arity),
        OrigItemType = pred_or_func_to_recomp_item_type(PredOrFunc),
        OrigItemName = recomp_item_name(PredName, Arity),
        OrigItemId = recomp_item_id(OrigItemType, OrigItemName),
        record_expanded_item(OrigItemId, !EquivTypeInfo)
    ).

:- pred try_to_pair_extra_types_and_modes(pred_or_func::in, sym_name::in,
    prog_context::in, list(mer_type)::in, list(mer_mode)::in,
    maybe1(list(type_and_mode))::out) is det.

try_to_pair_extra_types_and_modes(PredOrFunc, PredName, Context,
        ExtraTypes, ExtraModes, MaybeExtraTypesAndModes) :-
    list.length(ExtraTypes, NumExtraTypes),
    list.length(ExtraModes, NumExtraModes),
    ( if NumExtraTypes = NumExtraModes then
        pair_extra_types_and_modes(ExtraTypes, ExtraModes, ExtraTypesAndModes),
        MaybeExtraTypesAndModes = ok1(ExtraTypesAndModes)
    else
        PrefixPieces = pred_decl_error_prefix(PredOrFunc, PredName),
        ( if ExtraTypes = [] then
            Pieces = PrefixPieces ++
                [words("error: the `with_inst` annotation must be"),
                words("accompanied by a `with_type` annotation."),
                words("However,")] ++
                color_as_incorrect([words("this `with_type` annotation"),
                    words("is missing.")]) ++
                [nl]
        else if ExtraModes = [] then
            Pieces = PrefixPieces ++
                [words("error: the declaration specifies"),
                words("the mode of each argument, so"),
                words("the `with_type` annotation must be"),
                words("accompanied by a `with_inst` annotation."),
                words("However,")] ++
                color_as_incorrect([words("this `with_inst` annotation"),
                    words("is missing.")]) ++
                [nl]
        else
            Pieces = PrefixPieces ++
                [words("error: the `with_type` and `with_inst`"),
                words("annotations are")] ++
                color_as_incorrect([words("incompatible,")]) ++
                [words("because they specify")] ++
                color_as_inconsistent([int_name(NumExtraTypes),
                    words(choose_number(ExtraTypes, "type", "types"))]) ++
                [words("but")] ++
                color_as_inconsistent([int_name(NumExtraModes),
                    words(choose_number(ExtraModes, "mode", "modes")),
                    suffix(".")]) ++
                [nl]
        ),
        Spec = spec($pred, severity_error, phase_expand_types,
            Context, Pieces),
        MaybeExtraTypesAndModes = error1([Spec])
    ).

:- func pred_decl_error_prefix(pred_or_func, sym_name) = list(format_piece).

pred_decl_error_prefix(PredOrFunc, PredName) = PrefixPieces :-
    PrefixPieces = [words("In the declaration of"),
        p_or_f(PredOrFunc), unqual_sym_name(PredName), suffix(":"), nl].

:- pred pair_extra_types_and_modes(list(mer_type)::in, list(mer_mode)::in,
    list(type_and_mode)::out) is det.

pair_extra_types_and_modes([], [], []).
pair_extra_types_and_modes([_ | _], [], _) :-
    unexpected($pred, "list length mismatch").
pair_extra_types_and_modes([], [_ | _], _) :-
    unexpected($pred, "list length mismatch").
pair_extra_types_and_modes([Type | Types], [Mode | Modes],
        [type_and_mode(Type, Mode) | TypesAndModes]) :-
    pair_extra_types_and_modes(Types, Modes, TypesAndModes).

:- type pred_or_func_decl_type
    --->    type_decl
    ;       mode_decl.

:- pred replace_in_with_inst(maybe_record_sym_name_use::in, inst_eqv_map::in,
    sym_name::in, pred_form_arity::in, prog_context::in,
    pred_or_func_decl_type::in,
    maybe(pred_or_func)::in, maybe(pred_or_func)::out,
    maybe(mer_inst)::in, maybe(mer_inst)::out, list(mer_mode)::out,
    maybe(determinism)::in, maybe(determinism)::out,
    eqv_expand_info::in, eqv_expand_info::out,
    used_modules::in, used_modules::out, list(error_spec)::out) is det.

replace_in_with_inst(MaybeRecord, InstEqvMap, PredName, PredFormArity, Context,
        DeclType, MaybePredOrFunc0, MaybePredOrFunc,
        MaybeWithInst0, MaybeWithInst, ExtraModes,
        !MaybeDetism, !EquivTypeInfo, !UsedModules, Specs) :-
    (
        MaybeWithInst0 = yes(WithInst0),
        replace_in_inst(MaybeRecord, InstEqvMap, WithInst0, WithInst,
            !EquivTypeInfo, !UsedModules),
        ( if
            WithInst = ground(_, GroundInstInfo),
            GroundInstInfo = higher_order(HOInst),
            HOInst = pred_inst_info(PredOrFunc, ExtraModes0, _, DetPrime),
            ( MaybePredOrFunc0 = no
            ; MaybePredOrFunc0 = yes(PredOrFunc)
            )
        then
            !:MaybeDetism = yes(DetPrime),
            MaybeWithInst = no,
            MaybePredOrFunc = yes(PredOrFunc),
            ExtraModes = ExtraModes0,
            (
                MaybePredOrFunc0 = no,
                RecordedPredOrFunc = pf_predicate
            ;
                MaybePredOrFunc0 = yes(RecordedPredOrFunc)
            ),
            ItemType = pred_or_func_to_recomp_item_type(RecordedPredOrFunc),
            PredFormArity = pred_form_arity(Arity),
            ItemName = recomp_item_name(PredName, Arity),
            OrigItemId = recomp_item_id(ItemType, ItemName),
            record_expanded_item(OrigItemId, !EquivTypeInfo),
            Specs = []
        else
            ExtraModes = [],
            MaybePredOrFunc = MaybePredOrFunc0,
            % Leave the `with_inst` fields so that make_hlds
            % knows to discard this declaration.
            MaybeWithInst = MaybeWithInst0,
            ( DeclType = type_decl, DeclStr = "declaration"
            ; DeclType = mode_decl, DeclStr = "mode declaration"
            ),
            (
                MaybePredOrFunc = no,
                PredOrFuncPieces = []
            ;
                MaybePredOrFunc = yes(PredOrFunc),
                PredOrFuncPieces = [p_or_f(PredOrFunc)]
            ),
            Pieces = [words("In"), words(DeclStr), words("for")] ++
                PredOrFuncPieces ++ [qual_sym_name(PredName), suffix(":"), nl,
                words("error: expected the inst after"), quote("with_inst"),
                words("to be a")] ++
                color_as_correct([words("higher order")] ++ PredOrFuncPieces ++
                    [words("inst,")]) ++
                [words("but")] ++
                color_as_incorrect([words("it is not.")]) ++
                [nl],
            Spec = spec($pred, severity_error, phase_expand_types,
                Context, Pieces),
            Specs = [Spec]
        )
    ;
        MaybeWithInst0 = no,
        MaybeWithInst = MaybeWithInst0,
        MaybePredOrFunc = MaybePredOrFunc0,
        ExtraModes = [],
        Specs = []
    ).

:- pred replace_in_types_and_maybe_modes(maybe_record_sym_name_use::in,
    type_eqv_map::in, types_and_maybe_modes::in, types_and_maybe_modes::out,
    tvarset::in, tvarset::out, eqv_expand_info::in, eqv_expand_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_types_and_maybe_modes(MaybeRecord, TypeEqvMap,
        !TypeAndMaybeModes, !TVarSet, !EquivTypeInfo, !UsedModules) :-
    (
        !.TypeAndMaybeModes = no_types_arity_zero
    ;
        !.TypeAndMaybeModes = types_only(Types0),
        list.map2_foldl3(
            replace_in_type_maybe_record_use(MaybeRecord, TypeEqvMap),
            Types0, Types, _, !TVarSet, !EquivTypeInfo, !UsedModules),
        !:TypeAndMaybeModes = types_only(Types)
    ;

        !.TypeAndMaybeModes = types_and_modes(TypesAndModes0),
        list.map_foldl3(
            replace_in_type_and_mode(MaybeRecord, TypeEqvMap),
            TypesAndModes0, TypesAndModes,
            !TVarSet, !EquivTypeInfo, !UsedModules),
        !:TypeAndMaybeModes = types_and_modes(TypesAndModes)
    ).

:- pred replace_in_type_and_mode(maybe_record_sym_name_use::in,
    type_eqv_map::in, type_and_mode::in, type_and_mode::out,
    tvarset::in, tvarset::out, eqv_expand_info::in, eqv_expand_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_type_and_mode(MaybeRecord, TypeEqvMap, TypeAndMode0, TypeAndMode,
        !TVarSet, !EquivTypeInfo, !UsedModules) :-
    TypeAndMode0 = type_and_mode(Type0, Mode),
    replace_in_type_maybe_record_use(MaybeRecord, TypeEqvMap,
        Type0, Type, _, !TVarSet, !EquivTypeInfo, !UsedModules),
    TypeAndMode = type_and_mode(Type, Mode).

%---------------------------------------------------------------------------%

:- pred replace_in_structure_sharing_domain(maybe_record_sym_name_use::in,
    type_eqv_map::in, tvarset::in,
    structure_sharing_domain::in, structure_sharing_domain::out,
    eqv_expand_info::in, eqv_expand_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_structure_sharing_domain(MaybeRecord, TypeEqvMap, TVarSet,
        SharingDomain0, SharingDomain, !EquivTypeInfo, !UsedModules) :-
    (
        ( SharingDomain0 = structure_sharing_bottom
        ; SharingDomain0 = structure_sharing_top(_)
        ),
        SharingDomain = SharingDomain0
    ;
        SharingDomain0 = structure_sharing_real(SharingPairs0),
        list.map_foldl2(
            replace_in_structure_sharing_pair(MaybeRecord, TypeEqvMap,
                TVarSet),
            SharingPairs0, SharingPairs, !EquivTypeInfo, !UsedModules),
        SharingDomain = structure_sharing_real(SharingPairs)
    ).

:- pred replace_in_structure_sharing_pair(maybe_record_sym_name_use::in,
    type_eqv_map::in, tvarset::in,
    structure_sharing_pair::in, structure_sharing_pair::out,
    eqv_expand_info::in, eqv_expand_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_structure_sharing_pair(MaybeRecord, TypeEqvMap, TVarSet,
        SSA0 - SSB0, SSA - SSB, !EquivTypeInfo, !UsedModules) :-
    replace_in_datastruct(MaybeRecord, TypeEqvMap, TVarSet, SSA0, SSA,
        !EquivTypeInfo, !UsedModules),
    replace_in_datastruct(MaybeRecord, TypeEqvMap, TVarSet, SSB0, SSB,
        !EquivTypeInfo, !UsedModules).

:- pred replace_in_datastruct(maybe_record_sym_name_use::in,
    type_eqv_map::in, tvarset::in, datastruct::in,
    datastruct::out, eqv_expand_info::in, eqv_expand_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_datastruct(MaybeRecord, TypeEqvMap, TVarSet, DS0, DS,
        !EquivTypeInfo, !UsedModules) :-
    DS0 = selected_cel(Var, Sel0),
    list.map_foldl2(replace_in_unit_selector(MaybeRecord, TypeEqvMap, TVarSet),
        Sel0, Sel, !EquivTypeInfo, !UsedModules),
    DS = selected_cel(Var, Sel).

:- pred replace_in_unit_selector(maybe_record_sym_name_use::in,
    type_eqv_map::in, tvarset::in, unit_selector::in,
    unit_selector::out, eqv_expand_info::in, eqv_expand_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_unit_selector(MaybeRecord, TypeEqvMap, TVarSet, Sel0, Sel,
        !EquivTypeInfo, !UsedModules) :-
    (
        Sel0 = termsel(_, _),
        Sel = Sel0
    ;
        Sel0 = typesel(Type0),
        replace_in_type_maybe_record_use(MaybeRecord, TypeEqvMap,
            Type0, Type, _, TVarSet, _, !EquivTypeInfo, !UsedModules),
        Sel = typesel(Type)
    ).

%---------------------------------------------------------------------------%

:- pred maybe_record_type_ctor_sym_name_use(maybe_record_sym_name_use::in,
    type_ctor::in, used_modules::in, used_modules::out) is det.

maybe_record_type_ctor_sym_name_use(MaybeRecord, TypeCtor, !UsedModules) :-
    (
        MaybeRecord = do_not_record_sym_name_use
    ;
        MaybeRecord = record_sym_name_use(Visibility),
        TypeCtor = type_ctor(TypeCtorSymName, _TypeCtorArity),
        record_sym_name_module_as_used(Visibility, TypeCtorSymName,
            !UsedModules)
    ).

%---------------------------------------------------------------------------%
:- end_module parse_tree.equiv_type.
%---------------------------------------------------------------------------%
