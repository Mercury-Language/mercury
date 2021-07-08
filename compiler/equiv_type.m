%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2012 The University of Melbourne.
% Copyright (C) 2014-2021 The Mercury team.
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
% If inst i1's body contains inst i2, and i2 has been defined
% to be equivalent to some other inst i3, then we *could* replace
% i2 with i3 in i1's body. Instead of doing this once for this
% user-defined inst, we do it on every use of this inst. This is
% significantly less efficient, but if there is any error that
% involves this inst, the error message we generate will refer to
% the inst by the name the user gave it. If the user e.g. wrote
% an inst i1 in a mode declaration, but an error message about
% that mode declaration referred to the expanded form of i1,
% this would be confusing to many programmers. Most likely,
% it would also be harder to read, since inst names are almost always
% shorter than the insts they are defined to be equivalent to.
%
% XXX INST_FOR_TYPE_CONSTRUCTOR
% If inst i1 is for type t2, and t2 has been defined to be equivalent
% to type t3, then we SHOULD record that i1 is really for t3.
% However, while t2 is required to be just a type_ctor and arity,
% t3 may be more complex. The obvious thing to do would be to record that
% i1 is for t3's top type_ctor and its arity. Whether that is good
% enough depends on what *exactly* we will do with the "inst for type ctor"
% information. We don't yet know the answer to that question.
% XXX This should allow us to fix Mantis bug #89.
%
% XXX We do not currently expand out mode definitions either,
% even though the first paragraph about definitely applies to them as well,
% and if we ever extend the language to allow (and maybe even require)
% programmers to record "mode for type constructor" information,
% the second paragraph will apply as well.
%
%---------------------------------------------------------------------------%
%
% XXX We do not currently expand out clauses.
% This will leave any with_type annotations in clauses unexpanded.
%
%---------------------------------------------------------------------------%

:- module parse_tree.equiv_type.
:- interface.

:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_event.
:- import_module parse_tree.prog_data_used_modules.
:- import_module parse_tree.prog_item.
:- import_module recompilation.

:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module one_or_more.

:- type maybe_changed
    --->    no_change
    ;       changed.

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
    eqv_expanded_info::in, eqv_expanded_info::out) is det.

:- pred replace_in_type_list(type_eqv_map::in,
    list(mer_type)::in, list(mer_type)::out, maybe_changed::out,
    tvarset::in, tvarset::out,
    eqv_expanded_info::in, eqv_expanded_info::out) is det.

:- pred replace_in_prog_constraints(type_eqv_map::in,
    prog_constraints::in, prog_constraints::out, tvarset::in, tvarset::out,
    eqv_expanded_info::in, eqv_expanded_info::out) is det.

:- pred replace_in_prog_constraint_list(type_eqv_map::in,
    list(prog_constraint)::in, list(prog_constraint)::out,
    tvarset::in, tvarset::out, eqv_expanded_info::in, eqv_expanded_info::out)
    is det.

:- pred replace_in_ctors(type_eqv_map::in,
    one_or_more(constructor)::in, one_or_more(constructor)::out,
    tvarset::in, tvarset::out,
    eqv_expanded_info::in, eqv_expanded_info::out) is det.

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
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_data_pragma.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_subst.
:- import_module parse_tree.prog_util.

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
    AugCompUnit0 = aug_compilation_unit(ModuleName, ModuleNameContext,
        ModuleVersionNumbers, ParseTreeModuleSrc0, AncestorIntSpecs0,
        DirectIntSpecs0, IndirectIntSpecs0,
        PlainOpts0, TransOpts0, IntForOptSpecs0, TypeRepnSpecs0),
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
        map.foldl2_values(build_eqv_maps_in_direct_int_spec, DirectIntSpecs0,
            !TypeEqvMap, !InstEqvMap),
        map.foldl2_values(build_eqv_maps_in_indirect_int_spec,
            IndirectIntSpecs0,
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
        replace_in_direct_int_spec(ModuleName, TypeEqvMap, InstEqvMap),
        DirectIntSpecs0, DirectIntSpecs, !RecompInfo, !UsedModules, !Specs),
    map.map_values_foldl3(
        replace_in_indirect_int_spec(ModuleName, TypeEqvMap, InstEqvMap),
        IndirectIntSpecs0, IndirectIntSpecs,
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
    AugCompUnit = aug_compilation_unit(ModuleName, ModuleNameContext,
        ModuleVersionNumbers, ParseTreeModuleSrc, AncestorIntSpecs,
        DirectIntSpecs, IndirectIntSpecs,
        PlainOpts, TransOpts, IntForOptSpecs, TypeRepnSpecs),

    map.to_assoc_list(EventSpecMap0, EventSpecList0),
    replace_in_event_specs(TypeEqvMap, EventSpecList0, EventSpecList,
        !RecompInfo, !UsedModules, !Specs),
    map.from_sorted_assoc_list(EventSpecList, EventSpecMap).

%---------------------------------------------------------------------------%

    % We need to expand equivalence insts in
    % `:- pred p `with_inst` i' declarations.
:- type eqv_inst_body
    --->    eqv_inst_body(
                inst_varset,
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
    list.foldl(build_eqv_maps_in_type_defn,
        ParseTreeModuleSrc ^ ptms_int_type_defns_mer, !TypeEqvMap),
    list.foldl(build_eqv_maps_in_type_defn,
        ParseTreeModuleSrc ^ ptms_imp_type_defns_mer, !TypeEqvMap),
    list.foldl(build_eqv_maps_in_inst_defn,
        ParseTreeModuleSrc ^ ptms_int_inst_defns, !InstEqvMap),
    list.foldl(build_eqv_maps_in_inst_defn,
        ParseTreeModuleSrc ^ ptms_imp_inst_defns, !InstEqvMap).

%---------------------%

:- pred build_eqv_maps_in_ancestor_int_spec(ancestor_int_spec::in,
    type_eqv_map::in, type_eqv_map::out,
    inst_eqv_map::in, inst_eqv_map::out) is det.

build_eqv_maps_in_ancestor_int_spec(AncestorIntSpec,
        !TypeEqvMap, !InstEqvMap) :-
    AncestorIntSpec = ancestor_int0(ParseTreeInt0, ReadWhy0),
    build_eqv_maps_in_parse_tree_int0(ReadWhy0, ParseTreeInt0,
        !TypeEqvMap, !InstEqvMap).

:- pred build_eqv_maps_in_direct_int_spec(direct_int_spec::in,
    type_eqv_map::in, type_eqv_map::out,
    inst_eqv_map::in, inst_eqv_map::out) is det.

build_eqv_maps_in_direct_int_spec(DirectIntSpec,
        !TypeEqvMap, !InstEqvMap) :-
    (
        DirectIntSpec = direct_int1(ParseTreeInt1, ReadWhy1),
        build_eqv_maps_in_parse_tree_int1(ReadWhy1, ParseTreeInt1,
            !TypeEqvMap, !InstEqvMap)
    ;
        DirectIntSpec = direct_int3(ParseTreeInt3, ReadWhy3),
        build_eqv_maps_in_parse_tree_int3(ReadWhy3, ParseTreeInt3,
            !TypeEqvMap, !InstEqvMap)
    ).

:- pred build_eqv_maps_in_indirect_int_spec(indirect_int_spec::in,
    type_eqv_map::in, type_eqv_map::out,
    inst_eqv_map::in, inst_eqv_map::out) is det.

build_eqv_maps_in_indirect_int_spec(IndirectIntSpec,
        !TypeEqvMap, !InstEqvMap) :-
    (
        IndirectIntSpec = indirect_int2(ParseTreeInt2, ReadWhy2),
        build_eqv_maps_in_parse_tree_int2(ReadWhy2, ParseTreeInt2,
            !TypeEqvMap, !InstEqvMap)
    ;
        IndirectIntSpec = indirect_int3(ParseTreeInt3, ReadWhy3),
        build_eqv_maps_in_parse_tree_int3(ReadWhy3, ParseTreeInt3,
            !TypeEqvMap, !InstEqvMap)
    ).

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
    list.foldl(build_eqv_maps_in_type_ctor_all_defns,
        map.values(ParseTreeInt0 ^ pti0_int_type_defns), !TypeEqvMap),
    list.foldl(build_eqv_maps_in_type_ctor_all_defns,
        map.values(ParseTreeInt0 ^ pti0_imp_type_defns), !TypeEqvMap),
    list.foldl(build_eqv_maps_in_inst_ctor_all_defns,
        map.values(ParseTreeInt0 ^ pti0_int_inst_defns), !InstEqvMap),
    list.foldl(build_eqv_maps_in_inst_ctor_all_defns,
        map.values(ParseTreeInt0 ^ pti0_imp_inst_defns), !InstEqvMap).

:- pred build_eqv_maps_in_parse_tree_int1(read_why_int1::in,
    parse_tree_int1::in,
    type_eqv_map::in, type_eqv_map::out,
    inst_eqv_map::in, inst_eqv_map::out) is det.

build_eqv_maps_in_parse_tree_int1(_ReadWhy1, ParseTreeInt1,
        !TypeEqvMap, !InstEqvMap) :-
    % All possible values of _ReadWhy1 call for things in the interface section
    % to be imported in a non-abstract form, and for things in the
    % implementation section to be imported in an abstract form.
    list.foldl(build_eqv_maps_in_type_ctor_all_defns,
        map.values(ParseTreeInt1 ^ pti1_int_type_defns), !TypeEqvMap),
    % Do not allow the expansion of abstract-imported type definitions.
    % list.foldl(build_eqv_maps_in_type_ctor_all_defns,
    %     map.values(ParseTreeInt1 ^ pti1_imp_type_defns), !TypeEqvMap),
    list.foldl(build_eqv_maps_in_inst_ctor_all_defns,
        map.values(ParseTreeInt1 ^ pti1_int_inst_defns), !InstEqvMap).

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
    % list.foldl(build_eqv_maps_in_type_ctor_all_defns,
    %     map.values(ParseTreeInt2 ^ pti2_imp_type_defns), !TypeEqvMap),
    (
        ReadWhy2 = rwi2_abstract
    ;
        ( ReadWhy2 = rwi2_int_use
        ; ReadWhy2 = rwi2_imp_use
        ; ReadWhy2 = rwi2_opt
        ),
        list.foldl(build_eqv_maps_in_type_ctor_all_defns,
            map.values(ParseTreeInt2 ^ pti2_int_type_defns), !TypeEqvMap),
        list.foldl(build_eqv_maps_in_inst_ctor_all_defns,
            map.values(ParseTreeInt2 ^ pti2_int_inst_defns), !InstEqvMap)
    ).

:- pred build_eqv_maps_in_parse_tree_int3(read_why_int3::in,
    parse_tree_int3::in,
    type_eqv_map::in, type_eqv_map::out,
    inst_eqv_map::in, inst_eqv_map::out) is det.

build_eqv_maps_in_parse_tree_int3(_ReadWhy3, ParseTreeInt3,
        !TypeEqvMap, !InstEqvMap) :-
    % All possible values of _ReadWhy3 call for things in the interface section
    % to be imported in a non-abstract form. There is no implementation
    % section.
    list.foldl(build_eqv_maps_in_type_ctor_all_defns,
        map.values(ParseTreeInt3 ^ pti3_int_type_defns), !TypeEqvMap),
    list.foldl(build_eqv_maps_in_inst_ctor_all_defns,
        map.values(ParseTreeInt3 ^ pti3_int_inst_defns), !InstEqvMap).

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
        !TypeEqvMap, !InstEqvMap).
    % .trans_opt files can contain neither type nor inst definitions.

%---------------------%

:- pred build_eqv_maps_in_type_ctor_all_defns(type_ctor_all_defns::in,
    type_eqv_map::in, type_eqv_map::out) is det.

build_eqv_maps_in_type_ctor_all_defns(TypeCtorAllDefns, !TypeEqvMap) :-
    EqvTypeDefns = TypeCtorAllDefns ^ tcad_eqv,
    (
        EqvTypeDefns = []
    ;
        EqvTypeDefns = [EqvTypeDefn | _],
        EqvTypeDefn = item_type_defn_info(Name, TypeParams, TypeDefnEqv,
            TVarSet, _Context, _SeqNum),
        TypeDefnEqv = type_details_eqv(EqvType),
        list.length(TypeParams, Arity),
        TypeCtor = type_ctor(Name, Arity),
        map.set(TypeCtor, eqv_type_body(TVarSet, TypeParams, EqvType),
            !TypeEqvMap)
    ).

:- pred build_eqv_maps_in_inst_ctor_all_defns(inst_ctor_all_defns::in,
    inst_eqv_map::in, inst_eqv_map::out) is det.

build_eqv_maps_in_inst_ctor_all_defns(InstCtorAllDefns, !InstEqvMap) :-
    InstCtorAllDefns = inst_ctor_all_defns(_AbstractInstDefns, EqvInstDefns),
    (
        EqvInstDefns = []
    ;
        EqvInstDefns = [EqvInstDefn | _],
        EqvInstDefn = item_inst_defn_info(Name, InstParams, _IFTC,
            InstDefn, InstVarSet, _Context, _SeqNum),
        ( if InstDefn = nonabstract_inst_defn(eqv_inst(EqvInst)) then
            list.length(InstParams, Arity),
            InstCtor = inst_ctor(Name, Arity),
            map.set(InstCtor, eqv_inst_body(InstVarSet, InstParams, EqvInst),
                !InstEqvMap)
        else
            unexpected($pred, "InstDefn != nonabstract_inst_defn")
        )
    ).

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

:- pred build_eqv_maps_in_inst_defn(item_inst_defn_info::in,
    inst_eqv_map::in, inst_eqv_map::out) is det.

build_eqv_maps_in_inst_defn(ItemInstDefn, !InstEqvMap) :-
    ItemInstDefn = item_inst_defn_info(Name, InstParams, _IFTC,
        InstDefn, TVarSet, _Context, _SeqNum),
    ( if InstDefn = nonabstract_inst_defn(eqv_inst(EqvInst)) then
        list.length(InstParams, Arity),
        InstCtor = inst_ctor(Name, Arity),
        map.set(InstCtor, eqv_inst_body(TVarSet, InstParams, EqvInst),
            !InstEqvMap)
    else
        true
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
        IntInclMap, ImpInclMap, InclMap,
        IntImportMap, IntUseMap, ImpImportMap, ImpUseMap, ImportUseMap,
        IntFIMSpecMap, ImpFIMSpecMap, MaybeImplicitFIMLangs,

        IntTypeDefnsAbs, IntTypeDefnsMer0, IntTypeDefnsForeign,
        IntInstDefns0, IntModeDefns0, IntTypeClasses0, IntInstances0,
        IntPredDecls0, IntModeDecls0,
        IntForeignExportEnums, IntDeclPragmas0, IntPromises, IntBadPreds,

        ImpTypeDefnsAbs, ImpTypeDefnsMer0, ImpTypeDefnsForeign,
        ImpInstDefns0, ImpModeDefns0, ImpTypeClasses0, ImpInstances0,
        ImpPredDecls0, ImpModeDecls0, ImpClauses0,
        ImpForeignEnums, ImpForeignExportEnums,
        ImpDeclPragmas0, ImpImplPragmas0, ImpPromises,
        ImpInitialises, ImpFinalises, ImpMutables0),

    replace_in_list(ModuleName, MaybeRecordInt, TypeEqvMap, InstEqvMap,
        replace_in_type_defn_info_general(replace_in_type_defn),
        IntTypeDefnsMer0, IntTypeDefnsMer, !RecompInfo, !UsedModules, !Specs),
    IntInstDefns = IntInstDefns0, % XXX See the comment at module top.
    IntModeDefns = IntModeDefns0, % XXX See the comment at module top.
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
        replace_in_type_defn_info_general(replace_in_type_defn),
        ImpTypeDefnsMer0, ImpTypeDefnsMer, !RecompInfo, !UsedModules, !Specs),
    ImpInstDefns = ImpInstDefns0, % XXX See the comment at module top.
    ImpModeDefns = ImpModeDefns0, % XXX See the comment at module top.
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
        replace_in_impl_pragma_info, ImpImplPragmas0, ImpImplPragmas,
        !RecompInfo, !UsedModules, !Specs),
    replace_in_list(ModuleName, MaybeRecordImp, TypeEqvMap, InstEqvMap,
        replace_in_mutable_info, ImpMutables0, ImpMutables,
        !RecompInfo, !UsedModules, !Specs),

    ParseTreeModuleSrc = parse_tree_module_src(ModuleName, ModuleNameContext,
        IntInclMap, ImpInclMap, InclMap,
        IntImportMap, IntUseMap, ImpImportMap, ImpUseMap, ImportUseMap,
        IntFIMSpecMap, ImpFIMSpecMap, MaybeImplicitFIMLangs,

        IntTypeDefnsAbs, IntTypeDefnsMer, IntTypeDefnsForeign,
        IntInstDefns, IntModeDefns, IntTypeClasses, IntInstances,
        IntPredDecls, IntModeDecls,
        IntForeignExportEnums, IntDeclPragmas, IntPromises, IntBadPreds,

        ImpTypeDefnsAbs, ImpTypeDefnsMer, ImpTypeDefnsForeign,
        ImpInstDefns, ImpModeDefns, ImpTypeClasses, ImpInstances,
        ImpPredDecls, ImpModeDecls, ImpClauses,
        ImpForeignEnums, ImpForeignExportEnums,
        ImpDeclPragmas, ImpImplPragmas, ImpPromises,
        ImpInitialises, ImpFinalises, ImpMutables).

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

:- pred replace_in_direct_int_spec(module_name::in,
    type_eqv_map::in, inst_eqv_map::in,
    direct_int_spec::in, direct_int_spec::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out,
    list(error_spec)::in, list(error_spec)::out) is det.

replace_in_direct_int_spec(ModuleName, TypeEqvMap, InstEqvMap,
        DirectIntSpec0, DirectIntSpec, !RecompInfo, !UsedModules, !Specs) :-
    (
        DirectIntSpec0 = direct_int1(OrigParseTree1, ReadWhy1),
        replace_in_parse_tree_int1(ModuleName, TypeEqvMap, InstEqvMap,
            OrigParseTree1, ParseTree1, !RecompInfo, !UsedModules, !Specs),
        DirectIntSpec = direct_int1(ParseTree1, ReadWhy1)
    ;
        DirectIntSpec0 = direct_int3(OrigParseTree3, ReadWhy3),
        replace_in_parse_tree_int3(ModuleName, TypeEqvMap, InstEqvMap,
            OrigParseTree3, ParseTree3, !RecompInfo, !UsedModules, !Specs),
        DirectIntSpec = direct_int3(ParseTree3, ReadWhy3)
    ).

:- pred replace_in_indirect_int_spec(module_name::in,
    type_eqv_map::in, inst_eqv_map::in,
    indirect_int_spec::in, indirect_int_spec::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out,
    list(error_spec)::in, list(error_spec)::out) is det.

replace_in_indirect_int_spec(ModuleName, TypeEqvMap, InstEqvMap,
        IndirectIntSpec0, IndirectIntSpec,
        !RecompInfo, !UsedModules, !Specs) :-
    (
        IndirectIntSpec0 = indirect_int2(OrigParseTree2, ReadWhy2),
        replace_in_parse_tree_int2(ModuleName, TypeEqvMap, InstEqvMap,
            OrigParseTree2, ParseTree2, !RecompInfo, !UsedModules, !Specs),
        IndirectIntSpec = indirect_int2(ParseTree2, ReadWhy2)
    ;
        IndirectIntSpec0 = indirect_int3(OrigParseTree3, ReadWhy3),
        replace_in_parse_tree_int3(ModuleName, TypeEqvMap, InstEqvMap,
            OrigParseTree3, ParseTree3, !RecompInfo, !UsedModules, !Specs),
        IndirectIntSpec = indirect_int3(ParseTree3, ReadWhy3)
    ).

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
    MaybeRecord = dont_record_sym_name_use,
    OrigParseTreeInt0 = parse_tree_int0(IntModuleName, IntModuleNameContext,
        MaybeVersionNumbers, IntInclMap, ImpInclMap, InclMap,
        IntImportMap, IntUseMap, ImpImportMap, ImpUseMap, ImportUseMap,
        IntFIMSpecs, ImpFIMSpecs,
        IntTypeDefnMap0, IntInstDefnMap0, IntModeDefnMap0,
        IntTypeClasses0, IntInstances0, IntPredDecls0, IntModeDecls0,
        IntForeignEnumMap, IntDeclPragmas0, IntPromises,
        ImpTypeDefnMap0, ImpInstDefnMap0, ImpModeDefnMap0,
        ImpTypeClasses0, ImpInstances0, ImpPredDecls0, ImpModeDecls0,
        ImpForeignEnumMap, ImpDeclPragmas0, ImpPromises),

    map.map_values_foldl3(
        replace_in_type_ctor_all_defns(ModuleName, MaybeRecord,
            TypeEqvMap, InstEqvMap),
        IntTypeDefnMap0, IntTypeDefnMap,
        !RecompInfo, !UsedModules, !Specs),
    IntInstDefnMap = IntInstDefnMap0, % XXX See the comment at module top.
    IntModeDefnMap = IntModeDefnMap0, % XXX See the comment at module top.
    replace_in_list(ModuleName, MaybeRecord, TypeEqvMap, InstEqvMap,
        replace_in_typeclass_info, IntTypeClasses0, IntTypeClasses,
        !RecompInfo, !UsedModules, !Specs),
    replace_in_list(ModuleName, MaybeRecord, TypeEqvMap, InstEqvMap,
        replace_in_instance_info, IntInstances0, IntInstances,
        !RecompInfo, !UsedModules, !Specs),
    replace_in_list(ModuleName, MaybeRecord, TypeEqvMap, InstEqvMap,
        replace_in_pred_decl_info, IntPredDecls0, IntPredDecls,
        !RecompInfo, !UsedModules, !Specs),
    replace_in_list(ModuleName, MaybeRecord, TypeEqvMap, InstEqvMap,
        replace_in_mode_decl_info, IntModeDecls0, IntModeDecls,
        !RecompInfo, !UsedModules, !Specs),
    replace_in_list(ModuleName, MaybeRecord, TypeEqvMap, InstEqvMap,
        replace_in_decl_pragma_info, IntDeclPragmas0, IntDeclPragmas,
        !RecompInfo, !UsedModules, !Specs),

    map.map_values_foldl3(
        replace_in_type_ctor_all_defns(ModuleName, MaybeRecord,
            TypeEqvMap, InstEqvMap),
        ImpTypeDefnMap0, ImpTypeDefnMap,
        !RecompInfo, !UsedModules, !Specs),
    ImpInstDefnMap = ImpInstDefnMap0, % XXX See the comment at module top.
    ImpModeDefnMap = ImpModeDefnMap0, % XXX See the comment at module top.
    replace_in_list(ModuleName, MaybeRecord, TypeEqvMap, InstEqvMap,
        replace_in_typeclass_info, ImpTypeClasses0, ImpTypeClasses,
        !RecompInfo, !UsedModules, !Specs),
    replace_in_list(ModuleName, MaybeRecord, TypeEqvMap, InstEqvMap,
        replace_in_instance_info, ImpInstances0, ImpInstances,
        !RecompInfo, !UsedModules, !Specs),
    replace_in_list(ModuleName, MaybeRecord, TypeEqvMap, InstEqvMap,
        replace_in_pred_decl_info, ImpPredDecls0, ImpPredDecls,
        !RecompInfo, !UsedModules, !Specs),
    replace_in_list(ModuleName, MaybeRecord, TypeEqvMap, InstEqvMap,
        replace_in_mode_decl_info, ImpModeDecls0, ImpModeDecls,
        !RecompInfo, !UsedModules, !Specs),
    replace_in_list(ModuleName, MaybeRecord, TypeEqvMap, InstEqvMap,
        replace_in_decl_pragma_info, ImpDeclPragmas0, ImpDeclPragmas,
        !RecompInfo, !UsedModules, !Specs),

    ParseTreeInt0 = parse_tree_int0(IntModuleName, IntModuleNameContext,
        MaybeVersionNumbers, IntInclMap, ImpInclMap, InclMap,
        IntImportMap, IntUseMap, ImpImportMap, ImpUseMap, ImportUseMap,
        IntFIMSpecs, ImpFIMSpecs,
        IntTypeDefnMap, IntInstDefnMap, IntModeDefnMap,
        IntTypeClasses, IntInstances, IntPredDecls, IntModeDecls,
        IntForeignEnumMap, IntDeclPragmas, IntPromises,
        ImpTypeDefnMap, ImpInstDefnMap, ImpModeDefnMap,
        ImpTypeClasses, ImpInstances, ImpPredDecls, ImpModeDecls,
        ImpForeignEnumMap, ImpDeclPragmas, ImpPromises).

:- pred replace_in_parse_tree_int1(module_name::in,
    type_eqv_map::in, inst_eqv_map::in,
    parse_tree_int1::in, parse_tree_int1::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out,
    list(error_spec)::in, list(error_spec)::out) is det.

replace_in_parse_tree_int1(ModuleName, TypeEqvMap, InstEqvMap,
        OrigParseTreeInt1, ParseTreeInt1, !RecompInfo, !UsedModules, !Specs) :-
    MaybeRecord = dont_record_sym_name_use,
    OrigParseTreeInt1 = parse_tree_int1(IntModuleName, IntModuleNameContext,
        MaybeVersionNumbers, IntInclMap, ImpInclMap, InclMap,
        IntUseMap, ImpUseMap, ImportUseMap, IntFIMSpecs, ImpFIMSpecs,
        IntTypeDefnMap0, IntInstDefnMap0, IntModeDefnMap0,
        IntTypeClasses0, IntInstances0, IntPredDecls0, IntModeDecls0,
        IntForeignEnumMap, IntDeclPragmas0, IntPromises, IntTypeRepnMap0,
        ImpTypeDefnMap0, ImpForeignEnumMap, ImpTypeClasses0),

    map.map_values_foldl3(
        replace_in_type_ctor_all_defns(ModuleName, MaybeRecord,
            TypeEqvMap, InstEqvMap),
        IntTypeDefnMap0, IntTypeDefnMap,
        !RecompInfo, !UsedModules, !Specs),
    IntInstDefnMap = IntInstDefnMap0, % XXX See the comment at module top.
    IntModeDefnMap = IntModeDefnMap0, % XXX See the comment at module top.
    replace_in_list(ModuleName, MaybeRecord, TypeEqvMap, InstEqvMap,
        replace_in_typeclass_info, IntTypeClasses0, IntTypeClasses,
        !RecompInfo, !UsedModules, !Specs),
    replace_in_list(ModuleName, MaybeRecord, TypeEqvMap, InstEqvMap,
        replace_in_instance_info, IntInstances0, IntInstances,
        !RecompInfo, !UsedModules, !Specs),
    replace_in_list(ModuleName, MaybeRecord, TypeEqvMap, InstEqvMap,
        replace_in_pred_decl_info, IntPredDecls0, IntPredDecls,
        !RecompInfo, !UsedModules, !Specs),
    replace_in_list(ModuleName, MaybeRecord, TypeEqvMap, InstEqvMap,
        replace_in_mode_decl_info, IntModeDecls0, IntModeDecls,
        !RecompInfo, !UsedModules, !Specs),
    replace_in_list(ModuleName, MaybeRecord, TypeEqvMap, InstEqvMap,
        replace_in_decl_pragma_info, IntDeclPragmas0, IntDeclPragmas,
        !RecompInfo, !UsedModules, !Specs),
    map.map_values_foldl3(
        replace_in_type_repn_info(ModuleName, MaybeRecord, TypeEqvMap),
        IntTypeRepnMap0, IntTypeRepnMap,
        !RecompInfo, !UsedModules, !Specs),

    map.map_values_foldl3(
        replace_in_type_ctor_all_defns(ModuleName, MaybeRecord,
            TypeEqvMap, InstEqvMap),
        ImpTypeDefnMap0, ImpTypeDefnMap,
        !RecompInfo, !UsedModules, !Specs),
    replace_in_list(ModuleName, MaybeRecord, TypeEqvMap, InstEqvMap,
        replace_in_typeclass_info, ImpTypeClasses0, ImpTypeClasses,
        !RecompInfo, !UsedModules, !Specs),

    ParseTreeInt1 = parse_tree_int1(IntModuleName, IntModuleNameContext,
        MaybeVersionNumbers, IntInclMap, ImpInclMap, InclMap,
        IntUseMap, ImpUseMap, ImportUseMap, IntFIMSpecs, ImpFIMSpecs,
        IntTypeDefnMap, IntInstDefnMap, IntModeDefnMap,
        IntTypeClasses, IntInstances, IntPredDecls, IntModeDecls,
        IntForeignEnumMap, IntDeclPragmas, IntPromises, IntTypeRepnMap,
        ImpTypeDefnMap, ImpForeignEnumMap, ImpTypeClasses).

:- pred replace_in_parse_tree_int2(module_name::in,
    type_eqv_map::in, inst_eqv_map::in,
    parse_tree_int2::in, parse_tree_int2::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out,
    list(error_spec)::in, list(error_spec)::out) is det.

replace_in_parse_tree_int2(ModuleName, TypeEqvMap, InstEqvMap,
        OrigParseTreeInt2, ParseTreeInt2, !RecompInfo, !UsedModules, !Specs) :-
    MaybeRecord = dont_record_sym_name_use,
    OrigParseTreeInt2 = parse_tree_int2(IntModuleName, IntModuleNameContext,
        MaybeVersionNumbers, IntInclMap, InclMap,
        IntUsedMap, ImportUseMap, IntFIMSpecs, ImpFIMSpecs,
        IntTypeDefnMap0, IntInstDefnMap0, IntModeDefnMap0,
        IntTypeClasses0, IntInstances0, IntTypeRepnMap0,
        ImpTypeDefnMap0),

    map.map_values_foldl3(
        replace_in_type_ctor_all_defns(ModuleName, MaybeRecord,
            TypeEqvMap, InstEqvMap),
        IntTypeDefnMap0, IntTypeDefnMap,
        !RecompInfo, !UsedModules, !Specs),
    IntInstDefnMap = IntInstDefnMap0, % XXX See the comment at module top.
    IntModeDefnMap = IntModeDefnMap0, % XXX See the comment at module top.
    replace_in_list(ModuleName, MaybeRecord, TypeEqvMap, InstEqvMap,
        replace_in_typeclass_info, IntTypeClasses0, IntTypeClasses,
        !RecompInfo, !UsedModules, !Specs),
    replace_in_list(ModuleName, MaybeRecord, TypeEqvMap, InstEqvMap,
        replace_in_instance_info, IntInstances0, IntInstances,
        !RecompInfo, !UsedModules, !Specs),
    map.map_values_foldl3(
        replace_in_type_repn_info(ModuleName, MaybeRecord, TypeEqvMap),
        IntTypeRepnMap0, IntTypeRepnMap,
        !RecompInfo, !UsedModules, !Specs),

    map.map_values_foldl3(
        replace_in_type_ctor_all_defns(ModuleName, MaybeRecord,
            TypeEqvMap, InstEqvMap),
        ImpTypeDefnMap0, ImpTypeDefnMap,
        !RecompInfo, !UsedModules, !Specs),

    ParseTreeInt2 = parse_tree_int2(IntModuleName, IntModuleNameContext,
        MaybeVersionNumbers, IntInclMap, InclMap,
        IntUsedMap, ImportUseMap, IntFIMSpecs, ImpFIMSpecs,
        IntTypeDefnMap, IntInstDefnMap, IntModeDefnMap,
        IntTypeClasses, IntInstances, IntTypeRepnMap,
        ImpTypeDefnMap).

:- pred replace_in_parse_tree_int3(module_name::in,
    type_eqv_map::in, inst_eqv_map::in,
    parse_tree_int3::in, parse_tree_int3::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out,
    list(error_spec)::in, list(error_spec)::out) is det.

replace_in_parse_tree_int3(ModuleName, TypeEqvMap, InstEqvMap,
        OrigParseTreeInt3, ParseTreeInt3, !RecompInfo, !UsedModules, !Specs) :-
    MaybeRecord = dont_record_sym_name_use,
    OrigParseTreeInt3 = parse_tree_int3(IntModuleName, IntModuleNameContext,
        IntInclMap, InclMap, IntImportMap, ImportUseMap,
        IntTypeDefnMap0, IntInstDefnMap0, IntModeDefnMap0,
        IntTypeClasses0, IntInstances0, IntTypeRepnMap0),

    map.map_values_foldl3(
        replace_in_type_ctor_all_defns(ModuleName, MaybeRecord,
            TypeEqvMap, InstEqvMap),
        IntTypeDefnMap0, IntTypeDefnMap,
        !RecompInfo, !UsedModules, !Specs),
    IntInstDefnMap = IntInstDefnMap0, % XXX See the comment at module top.
    IntModeDefnMap = IntModeDefnMap0, % XXX See the comment at module top.
    replace_in_list(ModuleName, MaybeRecord, TypeEqvMap, InstEqvMap,
        replace_in_typeclass_info, IntTypeClasses0, IntTypeClasses,
        !RecompInfo, !UsedModules, !Specs),
    replace_in_list(ModuleName, MaybeRecord, TypeEqvMap, InstEqvMap,
        replace_in_instance_info, IntInstances0, IntInstances,
        !RecompInfo, !UsedModules, !Specs),
    map.map_values_foldl3(
        replace_in_type_repn_info(ModuleName, MaybeRecord, TypeEqvMap),
        IntTypeRepnMap0, IntTypeRepnMap,
        !RecompInfo, !UsedModules, !Specs),

    ParseTreeInt3 = parse_tree_int3(IntModuleName, IntModuleNameContext,
        IntInclMap, InclMap, IntImportMap, ImportUseMap,
        IntTypeDefnMap, IntInstDefnMap, IntModeDefnMap,
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
    MaybeRecord = dont_record_sym_name_use,
    OrigParseTreePlainOpt = parse_tree_plain_opt(
        OptModuleName, OptModuleNameContext,
        UsedModuleNames, FIMSpecs, TypeDefns0, ForeignEnums,
        InstDefns0, ModeDefns0, TypeClasses0, Instances0,
        PredDecls0, ModeDecls0, Clauses, ForeignProcs, Promises,
        MarkerPragmas, TypeSpecs0, UnusedArgs, TermInfos, Term2Infos,
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
        replace_in_item_type_spec, TypeSpecs0, TypeSpecs,
        !RecompInfo, !UsedModules, !Specs),

    ParseTreePlainOpt = parse_tree_plain_opt(
        OptModuleName, OptModuleNameContext,
        UsedModuleNames, FIMSpecs, TypeDefns, ForeignEnums,
        InstDefns, ModeDefns, TypeClasses, Instances,
        PredDecls, ModeDecls, Clauses, ForeignProcs, Promises,
        MarkerPragmas, TypeSpecs, UnusedArgs, TermInfos, Term2Infos,
        Exceptions, Trailings, MMTablings, Sharings, Reuses).

:- pred replace_in_parse_tree_trans_opt(module_name::in,
    type_eqv_map::in, inst_eqv_map::in,
    parse_tree_trans_opt::in, parse_tree_trans_opt::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out,
    list(error_spec)::in, list(error_spec)::out) is det.

replace_in_parse_tree_trans_opt(_ModuleName, _TypeEqvMap, _InstEqvMap,
        !ParseTreeTransOpt, !RecompInfo, !UsedModules, !Specs).
    % No component that may appear in a parse_tree_trans_opt
    % needs any expansions.

%---------------------------------------------------------------------------%

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

:- type maybe_record_sym_name_use
    --->    dont_record_sym_name_use
    ;       record_sym_name_use(item_visibility).

:- pred replace_in_type_ctor_all_defns(module_name::in,
    maybe_record_sym_name_use::in, type_eqv_map::in, inst_eqv_map::in,
    type_ctor_all_defns::in, type_ctor_all_defns::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out,
    list(error_spec)::in, list(error_spec)::out) is det.

replace_in_type_ctor_all_defns(ModuleName, MaybeRecord, TypeEqvMap, InstEqvMap,
        AllDefns0, AllDefns, !RecompInfo, !UsedModules, !Specs) :-
    AllDefns0 = type_ctor_all_defns(TypeSolverAbs, TypeSolver0,
        TypeStdAbs, TypeStdEqv0, TypeStdDu0, TypeStdForeign),
    replace_in_list(ModuleName, MaybeRecord, TypeEqvMap, InstEqvMap,
        replace_in_type_defn_info_general(replace_in_type_defn_solver),
        TypeSolver0, TypeSolver, !RecompInfo, !UsedModules, !Specs),
    replace_in_list(ModuleName, MaybeRecord, TypeEqvMap, InstEqvMap,
        replace_in_type_defn_info_general(replace_in_type_defn_eqv),
        TypeStdEqv0, TypeStdEqv, !RecompInfo, !UsedModules, !Specs),
    replace_in_list(ModuleName, MaybeRecord, TypeEqvMap, InstEqvMap,
        replace_in_type_defn_info_general(replace_in_type_defn_du),
        TypeStdDu0, TypeStdDu, !RecompInfo, !UsedModules, !Specs),
    AllDefns = type_ctor_all_defns(TypeSolverAbs, TypeSolver,
        TypeStdAbs, TypeStdEqv, TypeStdDu, TypeStdForeign).

:- pred replace_in_type_defn_info_general(
    pred(maybe_record_sym_name_use, type_eqv_map, inst_eqv_map, type_ctor,
        prog_context, T, T, tvarset, tvarset,
        eqv_expanded_info, eqv_expanded_info,
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
    ItemId = item_id(type_body_item, item_name(SymName, Arity)),
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
    ItemId = item_id(type_body_item, item_name(SymName, Arity)),
    finish_recording_expanded_items(ItemId, UsedTypeCtors, !RecompInfo),
    Info = item_type_repn_info(SymName, ArgTypeVars, TypeRepn, TVarSet,
        Context, SeqNum).

replace_in_type_repn_eqv(TypeEqvMap, Info0, Info, !Specs) :-
    Info0 = item_type_repn_info(SymName, ArgTypeVars, Type0, TVarSet0,
        Context, SeqNum),
    list.length(ArgTypeVars, Arity),
    TypeCtor = type_ctor(SymName, Arity),
    replace_in_type_maybe_record_use_2(dont_record_sym_name_use, TypeEqvMap,
        [], Type0, Type, _Changed, Circ, TVarSet0, TVarSet,
        no, _, used_modules_init, _),
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
    Info0 = item_pred_decl_info(PredName, PredOrFunc, TypesAndModes0,
        MaybeWithType0, MaybeWithInst0, MaybeDetism0, Origin,
        TVarSet0, InstVarSet, ExistQVars, Purity, ClassContext0,
        Context, SeqNum),
    maybe_start_recording_expanded_items(ModuleName, PredName, !.RecompInfo,
        ExpandedItems0),
    replace_in_pred_type(MaybeRecord, PredName, PredOrFunc, Context,
        TypeEqvMap, InstEqvMap, ClassContext0, ClassContext,
        TypesAndModes0, TypesAndModes, TVarSet0, TVarSet,
        MaybeWithType0, MaybeWithType, MaybeWithInst0, MaybeWithInst,
        MaybeDetism0, MaybeDetism, ExpandedItems0, ExpandedItems,
        !UsedModules, Specs),
    ItemType = pred_or_func_to_item_type(PredOrFunc),
    list.length(TypesAndModes, Arity),
    adjust_func_arity(PredOrFunc, OrigArity, Arity),
    ItemId = item_id(ItemType, item_name(PredName, OrigArity)),
    finish_recording_expanded_items(ItemId, ExpandedItems, !RecompInfo),
    Info = item_pred_decl_info(PredName, PredOrFunc, TypesAndModes,
        MaybeWithType, MaybeWithInst, MaybeDetism, Origin,
        TVarSet, InstVarSet, ExistQVars, Purity, ClassContext,
        Context, SeqNum).

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
    replace_in_pred_mode(MaybeRecord, InstEqvMap, PredName,
        list.length(Modes0), Context, mode_decl, ExtraModes,
        MaybePredOrFunc0, MaybePredOrFunc, WithInst0, WithInst,
        MaybeDetism0, MaybeDetism, ExpandedItems0, ExpandedItems,
        !UsedModules, Specs),
    (
        ExtraModes = [],
        Modes = Modes0
    ;
        ExtraModes = [_ | _],
        Modes = Modes0 ++ ExtraModes
    ),
    (
        MaybePredOrFunc = yes(PredOrFunc),
        ItemType = pred_or_func_to_item_type(PredOrFunc),
        list.length(Modes, Arity),
        adjust_func_arity(PredOrFunc, OrigArity, Arity),
        ItemId = item_id(ItemType, item_name(PredName, OrigArity)),
        finish_recording_expanded_items(ItemId, ExpandedItems, !RecompInfo)
    ;
        MaybePredOrFunc = no
    ),
    Info = item_mode_decl_info(PredName, MaybePredOrFunc, Modes,
        WithInst, MaybeDetism, InstVarSet, Context, SeqNum).

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
    ItemId = item_id(typeclass_item, item_name(ClassName, Arity)),
    finish_recording_expanded_items(ItemId, ExpandedItems, !RecompInfo),
    Info = item_typeclass_info(ClassName, Vars, Constraints, FunDeps,
        ClassInterface, TVarSet, Context, SeqNum).

:- pred replace_in_instance_info(module_name::in,
    maybe_record_sym_name_use::in, type_eqv_map::in, inst_eqv_map::in,
    item_instance_info::in, item_instance_info::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out, list(error_spec)::out) is det.

replace_in_instance_info(ModuleName, MaybeRecord, TypeEqvMap, _InstEqvMap,
        InstanceInfo0, InstanceInfo, !RecompInfo, !UsedModules, []) :-
    InstanceInfo0 = item_instance_info(ClassName, Types0, OriginalTypes,
        Constraints0, InstanceBody, TVarSet0, ContainingModuleName,
        Context, SeqNum),
    ( if
        ( !.RecompInfo = no
        ; ContainingModuleName = ModuleName
        )
    then
        UsedTypeCtors0 = no
    else
        UsedTypeCtors0 = yes(eqv_expanded_item_set(ModuleName, set.init))
    ),
    replace_in_prog_constraint_list_location(MaybeRecord, TypeEqvMap,
        Constraints0, Constraints, TVarSet0, TVarSet1,
        UsedTypeCtors0, UsedTypeCtors1, !UsedModules),
    replace_in_type_list_location_circ(MaybeRecord, TypeEqvMap, Types0, Types,
        _, _, TVarSet1, TVarSet, UsedTypeCtors1, UsedTypeCtors, !UsedModules),
    % We specifically do NOT expand equivalence types in OriginalTypes.
    % If we did, that would defeat the purpose of the field.
    list.length(Types0, Arity),
    ItemId = item_id(typeclass_item, item_name(ClassName, Arity)),
    finish_recording_expanded_items(ItemId, UsedTypeCtors, !RecompInfo),
    InstanceInfo = item_instance_info(ClassName, Types, OriginalTypes,
        Constraints, InstanceBody, TVarSet, ContainingModuleName,
        Context, SeqNum).

:- pred replace_in_decl_pragma_info(module_name::in,
    maybe_record_sym_name_use::in, type_eqv_map::in, inst_eqv_map::in,
    item_decl_pragma_info::in, item_decl_pragma_info::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out, list(error_spec)::out) is det.

replace_in_decl_pragma_info(ModuleName, MaybeRecord, TypeEqvMap, InstEqvMap,
        Info0, Info, !RecompInfo, !UsedModules, Specs) :-
    Info0 = item_pragma_info(Pragma0, Context, SeqNum),
    (
        Pragma0 = decl_pragma_type_spec(TypeSpecInfo0),
        replace_in_pragma_info_type_spec(ModuleName, MaybeRecord,
            TypeEqvMap, InstEqvMap, TypeSpecInfo0, TypeSpecInfo,
            !RecompInfo, !UsedModules, Specs),
        Pragma = decl_pragma_type_spec(TypeSpecInfo),
        Info = item_pragma_info(Pragma, Context, SeqNum)
    ;
        ( Pragma0 = decl_pragma_obsolete_pred(_)
        ; Pragma0 = decl_pragma_obsolete_proc(_)
        ; Pragma0 = decl_pragma_oisu(_)
        ; Pragma0 = decl_pragma_terminates(_)
        ; Pragma0 = decl_pragma_does_not_terminate(_)
        ; Pragma0 = decl_pragma_check_termination(_)
        ; Pragma0 = decl_pragma_termination_info(_)
        ; Pragma0 = decl_pragma_termination2_info(_)
        ; Pragma0 = decl_pragma_structure_reuse(_)
        ; Pragma0 = decl_pragma_structure_sharing(_)
        ),
        Info = Info0,
        Specs = []
    ).

:- pred replace_in_item_type_spec(module_name::in,
    maybe_record_sym_name_use::in, type_eqv_map::in, inst_eqv_map::in,
    item_type_spec::in, item_type_spec::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out, list(error_spec)::out) is det.

replace_in_item_type_spec(ModuleName, MaybeRecord, TypeEqvMap, InstEqvMap,
        Item0, Item, !RecompInfo, !UsedModules, Specs) :-
    Item0 = item_pragma_info(TypeSpecInfo0, Context, SeqNum),
    replace_in_pragma_info_type_spec(ModuleName, MaybeRecord,
        TypeEqvMap, InstEqvMap, TypeSpecInfo0, TypeSpecInfo,
        !RecompInfo, !UsedModules, Specs),
    Item = item_pragma_info(TypeSpecInfo, Context, SeqNum).

:- pred replace_in_pragma_info_type_spec(module_name::in,
    maybe_record_sym_name_use::in, type_eqv_map::in, inst_eqv_map::in,
    pragma_info_type_spec::in, pragma_info_type_spec::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out, list(error_spec)::out) is det.

replace_in_pragma_info_type_spec(ModuleName, MaybeRecord,
        TypeEqvMap, _InstEqvMap, TypeSpecInfo0, TypeSpecInfo,
        !RecompInfo, !UsedModules, []) :-
    TypeSpecInfo0 = pragma_info_type_spec(PFUMM, PredName, NewName,
        Subst0, TVarSet0, ItemIds0),
    ( if
        ( !.RecompInfo = no
        ; PredName = qualified(ModuleName, _)
        )
    then
        ExpandedItems0 = no
    else
        ExpandedItems0 = yes(eqv_expanded_item_set(ModuleName, ItemIds0))
    ),
    replace_in_subst(MaybeRecord, TypeEqvMap, Subst0, Subst,
        TVarSet0, TVarSet, ExpandedItems0, ExpandedItems, !UsedModules),
    (
        ExpandedItems = no,
        ItemIds = ItemIds0
    ;
        ExpandedItems = yes(eqv_expanded_item_set(_, ItemIds))
    ),
    TypeSpecInfo = pragma_info_type_spec(PFUMM, PredName, NewName,
        Subst, TVarSet, ItemIds).

:- pred replace_in_impl_pragma_info(module_name::in,
    maybe_record_sym_name_use::in, type_eqv_map::in, inst_eqv_map::in,
    item_impl_pragma_info::in, item_impl_pragma_info::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out, list(error_spec)::out) is det.

replace_in_impl_pragma_info(ModuleName, MaybeRecord, TypeEqvMap, InstEqvMap,
        Info0, Info, !RecompInfo, !UsedModules, Specs) :-
    Info0 = item_pragma_info(Pragma0, Context, SeqNum),
    (
        Pragma0 = impl_pragma_foreign_proc(FPInfo0),
        replace_in_pragma_info_foreign_proc(ModuleName, MaybeRecord,
            TypeEqvMap, InstEqvMap, FPInfo0, FPInfo,
            !RecompInfo, !UsedModules, Specs),
        Pragma = impl_pragma_foreign_proc(FPInfo),
        Info = item_pragma_info(Pragma, Context, SeqNum)
    ;
        ( Pragma0 = impl_pragma_foreign_decl(_)
        ; Pragma0 = impl_pragma_foreign_code(_)
        ; Pragma0 = impl_pragma_foreign_proc_export(_)
        ; Pragma0 = impl_pragma_external_proc(_)
        ; Pragma0 = impl_pragma_fact_table(_)
        ; Pragma0 = impl_pragma_inline(_)
        ; Pragma0 = impl_pragma_no_inline(_)
        ; Pragma0 = impl_pragma_tabled(_)
        ; Pragma0 = impl_pragma_consider_used(_)
        ; Pragma0 = impl_pragma_no_detism_warning(_)
        ; Pragma0 = impl_pragma_mode_check_clauses(_)
        ; Pragma0 = impl_pragma_require_feature_set(_)
        ; Pragma0 = impl_pragma_promise_eqv_clauses(_)
        ; Pragma0 = impl_pragma_promise_pure(_)
        ; Pragma0 = impl_pragma_promise_semipure(_)
        ; Pragma0 = impl_pragma_require_tail_rec(_)
        ),
        Info = Info0,
        Specs = []
    ).

:- pred replace_in_pragma_info_foreign_proc(module_name::in,
    maybe_record_sym_name_use::in, type_eqv_map::in, inst_eqv_map::in,
    pragma_info_foreign_proc::in, pragma_info_foreign_proc::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out, list(error_spec)::out) is det.

replace_in_pragma_info_foreign_proc(ModuleName, MaybeRecord,
        TypeEqvMap, _InstEqvMap, FPInfo0, FPInfo,
        !RecompInfo, !UsedModules, []) :-
    FPInfo0 = pragma_info_foreign_proc(Attrs0, PName, PredOrFunc,
        ProcVars, ProcVarset, ProcInstVarset, ProcImpl),
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
        list.length(ProcVars, Arity),
        ItemId = item_id(foreign_proc_item, item_name(PName, Arity)),
        finish_recording_expanded_items(ItemId, !.EquivTypeInfo,
            !RecompInfo)
    ),
    FPInfo = pragma_info_foreign_proc(Attrs, PName, PredOrFunc,
        ProcVars, ProcVarset, ProcInstVarset, ProcImpl).

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
    ItemId = item_id(mutable_item, item_name(QualName, 0)),
    finish_recording_expanded_items(ItemId, ExpandedItems, !RecompInfo).

:- pred replace_in_mutable_defn(maybe_record_sym_name_use::in,
    type_eqv_map::in, inst_eqv_map::in,
    item_mutable_info::in, item_mutable_info::out,
    eqv_expanded_info::in, eqv_expanded_info::out,
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

:- pred replace_in_event_specs(type_eqv_map::in,
    assoc_list(string, event_spec)::in, assoc_list(string, event_spec)::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out,
    list(error_spec)::in, list(error_spec)::out) is det.

replace_in_event_specs(_, [], [], !RecompInfo, !UsedModules, !Specs).
replace_in_event_specs(TypeEqvMap,
        [Name - EventSpec0 | NameSpecs0], [Name - EventSpec | NameSpecs],
        !RecompInfo, !UsedModules, !Specs) :-
    replace_in_event_spec(TypeEqvMap, EventSpec0, EventSpec,
        !RecompInfo, !UsedModules, !Specs),
    replace_in_event_specs(TypeEqvMap, NameSpecs0, NameSpecs,
        !RecompInfo, !UsedModules, !Specs).

:- pred replace_in_event_spec(type_eqv_map::in,
    event_spec::in, event_spec::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out,
    list(error_spec)::in, list(error_spec)::out) is det.

replace_in_event_spec(TypeEqvMap, EventSpec0, EventSpec,
        !RecompInfo, !UsedModules, !Specs) :-
    EventSpec0 = event_spec(EventNumber, EventName, EventLineNumber,
        Attrs0, SyntAttrNumOrder),
    replace_in_event_attrs(TypeEqvMap, Attrs0, Attrs,
        !RecompInfo, !UsedModules, !Specs),
    EventSpec = event_spec(EventNumber, EventName, EventLineNumber,
        Attrs, SyntAttrNumOrder).

:- pred replace_in_event_attrs(type_eqv_map::in,
    list(event_attribute)::in, list(event_attribute)::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out,
    list(error_spec)::in, list(error_spec)::out) is det.

replace_in_event_attrs(_TypeEqvMap, [], [],
        !RecompInfo, !UsedModules, !Specs).
replace_in_event_attrs(TypeEqvMap, [Attr0 | Attrs0], [Attr | Attrs],
        !RecompInfo, !UsedModules, !Specs) :-
    replace_in_event_attr(TypeEqvMap, Attr0, Attr,
        !RecompInfo, !UsedModules, !Specs),
    replace_in_event_attrs(TypeEqvMap, Attrs0, Attrs,
        !RecompInfo, !UsedModules, !Specs).

:- pred replace_in_event_attr(type_eqv_map::in,
    event_attribute::in, event_attribute::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out,
    list(error_spec)::in, list(error_spec)::out) is det.

replace_in_event_attr(TypeEqvMap, Attr0, Attr,
        !RecompInfo, !UsedModules, !Specs) :-
    % We construct the attributes' modes ourselves in event_spec.m; they should
    % not contain type names.
    Attr0 = event_attribute(AttrNum, AttrName, AttrType0, AttrMode,
        MaybeSynthCall),
    TVarSet0 = varset.init,
    replace_in_type_maybe_record_use(dont_record_sym_name_use, TypeEqvMap,
        AttrType0, AttrType, _Changed, TVarSet0, _TVarSet, no, _EquivTypeInfo,
        !UsedModules),
    Attr = event_attribute(AttrNum, AttrName, AttrType, AttrMode,
        MaybeSynthCall).

%---------------------------------------------------------------------------%

:- pred replace_in_type_defn(maybe_record_sym_name_use::in,
    type_eqv_map::in, inst_eqv_map::in, type_ctor::in, prog_context::in,
    type_defn::in, type_defn::out,
    tvarset::in, tvarset::out, eqv_expanded_info::in, eqv_expanded_info::out,
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
    tvarset::in, tvarset::out, eqv_expanded_info::in, eqv_expanded_info::out,
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
    Pieces = [words("Error: circular equivalence type"),
        qual_type_ctor(TypeCtor), suffix("."), nl],
    Spec = simplest_spec($pred, severity_error, phase_expand_types,
        Context, Pieces).

:- func report_contains_circular_eqv_type(tvarset, mer_type, prog_context,
    type_ctor, list(type_ctor)) = error_spec.

report_contains_circular_eqv_type(TVarSet, Type, Context,
        HeadTypeCtor, TailTypeCtors) = Spec :-
    TypeStr = mercury_type_to_string(TVarSet, print_name_only, Type),
    MainPieces = [words("Error: the type"), quote(TypeStr),
        words("cannot have its equivalences fully expanded,"),
        words("because its expansion contains")],
    (
        TailTypeCtors = [],
        CircSpecs = [words("the circular equivalence type"),
            qual_type_ctor(HeadTypeCtor), suffix("."), nl]
    ;
        TailTypeCtors = [_ | _],
        TypeCtorPieces = list.map((func(TC) = qual_type_ctor(TC)),
            [HeadTypeCtor | TailTypeCtors]),
        CircSpecs = [words("the circular equivalence types")] ++
            component_list_to_pieces("and", TypeCtorPieces) ++
            [suffix("."), nl]
    ),
    Pieces = MainPieces ++ CircSpecs,
    Spec = simplest_spec($pred, severity_error, phase_expand_types,
        Context, Pieces).

:- pred replace_in_type_defn_du(maybe_record_sym_name_use::in,
    type_eqv_map::in, inst_eqv_map::in, type_ctor::in, prog_context::in,
    type_details_du::in, type_details_du::out,
    tvarset::in, tvarset::out, eqv_expanded_info::in, eqv_expanded_info::out,
    used_modules::in, used_modules::out, list(error_spec)::out) is det.

replace_in_type_defn_du(MaybeRecord, TypeEqvMap, _InstEqvMap,
        _TypeCtor, _Context, DetailsDu0, DetailsDu,
        !TVarSet, !EquivTypeInfo, !UsedModules, Specs) :-
    DetailsDu0 = type_details_du(MaybeSuperType0, Ctors0, EqPred,
        DirectArgFunctors),
    (
        MaybeSuperType0 = subtype_of(SuperType0),
        replace_in_type_maybe_record_use(MaybeRecord, TypeEqvMap,
            SuperType0, SuperType, _, !TVarSet, !EquivTypeInfo, !UsedModules),
        MaybeSuperType = subtype_of(SuperType)
    ;
        MaybeSuperType0 = not_a_subtype,
        MaybeSuperType = not_a_subtype
    ),
    replace_in_ctors_location(MaybeRecord, TypeEqvMap, Ctors0, Ctors,
        !TVarSet, !EquivTypeInfo, !UsedModules),
    DetailsDu = type_details_du(MaybeSuperType, Ctors, EqPred,
        DirectArgFunctors),
    Specs = [].

:- pred replace_in_type_defn_solver(maybe_record_sym_name_use::in,
    type_eqv_map::in, inst_eqv_map::in, type_ctor::in, prog_context::in,
    type_details_solver::in, type_details_solver::out,
    tvarset::in, tvarset::out, eqv_expanded_info::in, eqv_expanded_info::out,
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
        Specs = [simplest_spec($pred, severity_error, phase_expand_types,
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
    replace_in_type_maybe_record_use_2(dont_record_sym_name_use,
        TypeEqvMap, [], Type0, Type, Changed, Circ,
        TVarSet0, _TVarSet, no, _, used_modules_init, _),
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
    replace_in_type_maybe_record_use_2(dont_record_sym_name_use,
        TypeEqvMap, [], Type0, Type, Changed, _Circ, !TVarSet,
        !EquivTypeInfo, used_modules_init, _).

:- pred replace_in_type_maybe_record_use(maybe_record_sym_name_use::in,
    type_eqv_map::in, mer_type::in, mer_type::out, maybe_changed::out,
    tvarset::in, tvarset::out, eqv_expanded_info::in, eqv_expanded_info::out,
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
    eqv_expanded_info::in, eqv_expanded_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_type_maybe_record_use_2(MaybeRecord, TypeEqvMap,
        TypeCtorsAlreadyExpanded, Type0, Type, Changed, Circ,
        !TVarSet, !EquivTypeInfo, !UsedModules) :-
    (
        Type0 = type_variable(Var, Kind),
        Type = type_variable(Var, Kind),
        Changed = no_change,
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
        Changed = no_change,
        Circ = set.init
    ;
        Type0 = higher_order_type(PorF, Args0, HOInstInfo, Purity, EvalMethod),
        replace_in_type_list_location_circ_2(MaybeRecord, TypeEqvMap,
            TypeCtorsAlreadyExpanded, Args0, Args, Changed, set.init, Circ,
            !TVarSet, !EquivTypeInfo, !UsedModules),
        (
            Changed = changed,
            Type = higher_order_type(PorF, Args, HOInstInfo, Purity,
                EvalMethod)
        ;
            Changed = no_change,
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
            Changed = no_change,
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
            Changed = no_change,
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
            Changed = no_change,
            Type = Type0
        )
    ).

:- pred replace_type_ctor(maybe_record_sym_name_use::in, type_eqv_map::in,
    list(type_ctor)::in, mer_type::in, type_ctor::in, list(mer_type)::in,
    kind::in, mer_type::out, maybe_changed::in, maybe_changed::out,
    circ_types::in, circ_types::out, tvarset::in, tvarset::out,
    eqv_expanded_info::in, eqv_expanded_info::out,
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
        TypeCtorItem = type_ctor_to_item_name(TypeCtor),
        record_expanded_item(item_id(type_abstract_item, TypeCtorItem),
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
            !.Changed = no_change,
            Type = Type0
        ),
        set.union(NewCirc, !Circ)
    ).

%---------------------------------------------------------------------------%

replace_in_type_list(TypeEqvMap, !Types, Changed, !TVarSet, !EquivTypeInfo) :-
    replace_in_type_list_location(dont_record_sym_name_use, TypeEqvMap,
        !Types, Changed, !TVarSet, !EquivTypeInfo, used_modules_init, _).

:- pred replace_in_type_list_location(maybe_record_sym_name_use::in,
    type_eqv_map::in, list(mer_type)::in, list(mer_type)::out,
    maybe_changed::out, tvarset::in, tvarset::out,
    eqv_expanded_info::in, eqv_expanded_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_type_list_location(MaybeRecord, TypeEqvMap, !Types,
        Changed, !TVarSet, !EquivTypeInfo, !UsedModules) :-
    replace_in_type_list_location_circ_2(MaybeRecord, TypeEqvMap, [], !Types,
        Changed, set.init, _, !TVarSet, !EquivTypeInfo, !UsedModules).

:- pred replace_in_type_list_location_circ(maybe_record_sym_name_use::in,
    type_eqv_map::in, list(mer_type)::in, list(mer_type)::out,
    maybe_changed::out, circ_types::out, tvarset::in, tvarset::out,
    eqv_expanded_info::in, eqv_expanded_info::out,
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
    tvarset::in, tvarset::out, eqv_expanded_info::in, eqv_expanded_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_type_list_location_circ_2(_MaybeRecord, _TypeEqvMap, _Seen,
        [], [], no_change, !ContainsCirc, !TVarSet,
        !EquivTypeInfo, !UsedModules).
replace_in_type_list_location_circ_2(MaybeRecord, TypeEqvMap, Seen,
        List0 @ [Type0 | Types0], List, Changed, !Circ, !TVarSet,
        !EquivTypeInfo, !UsedModules) :-
    replace_in_type_maybe_record_use_2(MaybeRecord, TypeEqvMap, Seen,
        Type0, Type, HeadChanged, HeadCirc, !TVarSet,
        !EquivTypeInfo, !UsedModules),
    set.union(HeadCirc, !Circ),
    replace_in_type_list_location_circ_2(MaybeRecord, TypeEqvMap, Seen,
        Types0, Types, TailChanged, !Circ, !TVarSet,
        !EquivTypeInfo, !UsedModules),
    ( if
        ( HeadChanged = changed
        ; TailChanged = changed
        )
    then
        Changed = changed,
        List = [Type | Types]
    else
        Changed = no_change,
        List = List0
    ).

%---------------------------------------------------------------------------%

:- pred replace_in_ctor_arg_list(maybe_record_sym_name_use::in,
    type_eqv_map::in, list(constructor_arg)::in, list(constructor_arg)::out,
    circ_types::out, tvarset::in, tvarset::out,
    eqv_expanded_info::in, eqv_expanded_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_ctor_arg_list(MaybeRecord, TypeEqvMap, !Args,
        ContainsCirc, !TVarSet, !EquivTypeInfo, !UsedModules) :-
    replace_in_ctor_arg_list_loop(MaybeRecord, TypeEqvMap, [], !Args,
        set.init, ContainsCirc, !TVarSet, !EquivTypeInfo, !UsedModules).

:- pred replace_in_ctor_arg_list_loop(maybe_record_sym_name_use::in,
    type_eqv_map::in, list(type_ctor)::in,
    list(constructor_arg)::in, list(constructor_arg)::out,
    circ_types::in, circ_types::out, tvarset::in, tvarset::out,
    eqv_expanded_info::in, eqv_expanded_info::out,
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
    eqv_expanded_info::in, eqv_expanded_info::out,
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

replace_in_prog_constraints(TypeEqvMap, Cs0, Cs, !TVarSet, !EquivTypeInfo) :-
    replace_in_prog_constraints_location(dont_record_sym_name_use, TypeEqvMap,
        Cs0, Cs, !TVarSet, !EquivTypeInfo, used_modules_init, _).

:- pred replace_in_prog_constraints_location(maybe_record_sym_name_use::in,
    type_eqv_map::in, prog_constraints::in, prog_constraints::out,
    tvarset::in, tvarset::out, eqv_expanded_info::in, eqv_expanded_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_prog_constraints_location(MaybeRecord, TypeEqvMap, Cs0, Cs, !TVarSet,
        !EquivTypeInfo, !UsedModules) :-
    Cs0 = constraints(UnivCs0, ExistCs0),
    replace_in_prog_constraint_list_location(MaybeRecord, TypeEqvMap,
        UnivCs0, UnivCs, !TVarSet, !EquivTypeInfo, !UsedModules),
    replace_in_prog_constraint_list_location(MaybeRecord, TypeEqvMap,
        ExistCs0, ExistCs, !TVarSet, !EquivTypeInfo, !UsedModules),
    Cs = constraints(UnivCs, ExistCs).

replace_in_prog_constraint_list(TypeEqvMap,
        !Constraints, !TVarSet, !EquivTypeInfo) :-
    replace_in_prog_constraint_list_location(dont_record_sym_name_use,
        TypeEqvMap, !Constraints,
        !TVarSet, !EquivTypeInfo, used_modules_init, _).

:- pred replace_in_prog_constraint_list_location(maybe_record_sym_name_use::in,
    type_eqv_map::in, list(prog_constraint)::in, list(prog_constraint)::out,
    tvarset::in, tvarset::out, eqv_expanded_info::in, eqv_expanded_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_prog_constraint_list_location(MaybeRecord, TypeEqvMap,
        !Constraints, !TVarSet, !EquivTypeInfo, !UsedModules) :-
    list.map_foldl3(
        replace_in_prog_constraint_location(MaybeRecord, TypeEqvMap),
        !Constraints, !TVarSet, !EquivTypeInfo, !UsedModules).

:- pred replace_in_prog_constraint_location(maybe_record_sym_name_use::in,
    type_eqv_map::in, prog_constraint::in, prog_constraint::out,
    tvarset::in, tvarset::out, eqv_expanded_info::in, eqv_expanded_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_prog_constraint_location(MaybeRecord, TypeEqvMap,
        Constraint0, Constraint, !TVarSet, !EquivTypeInfo, !UsedModules) :-
    Constraint0 = constraint(ClassName, ArgTypes0),
    replace_in_type_list_location_circ(MaybeRecord, TypeEqvMap,
        ArgTypes0, ArgTypes, _, _, !TVarSet, !EquivTypeInfo, !UsedModules),
    Constraint = constraint(ClassName, ArgTypes).

%---------------------------------------------------------------------------%

:- pred replace_in_class_interface(maybe_record_sym_name_use::in,
    type_eqv_map::in, inst_eqv_map::in,
    list(class_decl)::in, list(class_decl)::out,
    eqv_expanded_info::in, eqv_expanded_info::out,
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
    eqv_expanded_info::in, eqv_expanded_info::out,
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
        replace_in_pred_type(MaybeRecord, PredName, PredOrFunc, Context,
            TypeEqvMap, InstEqvMap, ClassContext0, ClassContext,
            TypesAndModes0, TypesAndModes, TVarSet0, TVarSet,
            WithType0, WithType, WithInst0, WithInst,
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
        replace_in_pred_mode(MaybeRecord, InstEqvMap,
            PredName, list.length(Modes0), Context, mode_decl, ExtraModes,
            MaybePredOrFunc0, MaybePredOrFunc, WithInst0, WithInst,
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
    assoc_list(tvar, mer_type)::in, assoc_list(tvar, mer_type)::out,
    tvarset::in, tvarset::out, eqv_expanded_info::in, eqv_expanded_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_subst(_MaybeRecord, _TypeEqvMap, [], [], !TVarSet, !EquivTypeInfo,
        !UsedModules).
replace_in_subst(MaybeRecord, TypeEqvMap, [Var - Type0 | Subst0],
        [Var - Type | Subst], !TVarSet, !EquivTypeInfo, !UsedModules) :-
    replace_in_type_maybe_record_use(MaybeRecord, TypeEqvMap,
        Type0, Type, _, !TVarSet, !EquivTypeInfo, !UsedModules),
    replace_in_subst(MaybeRecord, TypeEqvMap, Subst0, Subst, !TVarSet,
        !EquivTypeInfo, !UsedModules).

%---------------------------------------------------------------------------%

replace_in_ctors(TypeEqvMap, !Ctors, !TVarSet, !EquivTypeInfo) :-
    replace_in_ctors_location(dont_record_sym_name_use, TypeEqvMap,
        !Ctors, !TVarSet, !EquivTypeInfo, used_modules_init, _).

:- pred replace_in_ctors_location(maybe_record_sym_name_use::in,
    type_eqv_map::in,
    one_or_more(constructor)::in, one_or_more(constructor)::out,
    tvarset::in, tvarset::out, eqv_expanded_info::in, eqv_expanded_info::out,
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
    eqv_expanded_info::in, eqv_expanded_info::out,
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
    mer_inst::in, mer_inst::out, eqv_expanded_info::in, eqv_expanded_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_inst(MaybeRecord, InstEqvMap, Inst0, Inst,
        !EquivTypeInfo, !UsedModules) :-
    replace_in_inst_location(MaybeRecord, InstEqvMap, set.init, Inst0, Inst,
        !EquivTypeInfo, !UsedModules).

:- pred replace_in_inst_location(maybe_record_sym_name_use::in,
    inst_eqv_map::in, set(inst_ctor)::in, mer_inst::in, mer_inst::out,
    eqv_expanded_info::in, eqv_expanded_info::out,
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
            EqvInstBody = eqv_inst_body(_, EqvInstParams, EqvInst)
        then
            inst_substitute_arg_list(EqvInstParams, ArgInsts, EqvInst, Inst1),
            InstCtorItem = inst_ctor_to_item_name(InstCtor),
            record_expanded_item(item_id(inst_item, InstCtorItem),
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

:- pred replace_in_pred_type(maybe_record_sym_name_use::in, sym_name::in,
    pred_or_func::in, prog_context::in, type_eqv_map::in, inst_eqv_map::in,
    prog_constraints::in, prog_constraints::out,
    list(type_and_mode)::in, list(type_and_mode)::out,
    tvarset::in, tvarset::out,
    maybe(mer_type)::in, maybe(mer_type)::out,
    maybe(mer_inst)::in, maybe(mer_inst)::out,
    maybe(determinism)::in, maybe(determinism)::out,
    eqv_expanded_info::in, eqv_expanded_info::out,
    used_modules::in, used_modules::out, list(error_spec)::out) is det.

replace_in_pred_type(MaybeRecord, PredName, PredOrFunc, Context,
        TypeEqvMap, InstEqvMap, ClassContext0, ClassContext,
        TypesAndModes0, TypesAndModes, !TVarSet,
        MaybeWithType0, MaybeWithType, MaybeWithInst0, MaybeWithInst,
        !MaybeDetism, !EquivTypeInfo, !UsedModules, !:Specs) :-
    replace_in_prog_constraints_location(MaybeRecord, TypeEqvMap,
        ClassContext0, ClassContext, !TVarSet,
        !EquivTypeInfo, !UsedModules),
    replace_in_types_and_modes(MaybeRecord, TypeEqvMap,
        TypesAndModes0, TypesAndModes1,
        !TVarSet, !EquivTypeInfo, !UsedModules),
    (
        MaybeWithType0 = yes(WithType0),
        replace_in_type_maybe_record_use(MaybeRecord, TypeEqvMap,
            WithType0, WithType, _, !TVarSet, !EquivTypeInfo, !UsedModules),
        ( if
            type_is_higher_order_details(WithType, _Purity, PredOrFunc,
                _EvalMethod, ExtraTypesPrime)
        then
            ExtraTypes = ExtraTypesPrime,
            !:Specs = []
        else
            ExtraTypes = [],
            Pieces1 = [words("In type declaration for"),
                p_or_f(PredOrFunc), qual_sym_name(PredName), suffix(":"), nl,
                words("error: expected higher order"), p_or_f(PredOrFunc),
                words("type after `with_type`."), nl],
            Spec1 = simplest_spec($pred, severity_error, phase_expand_types,
                Context, Pieces1),
            !:Specs = [Spec1]
        )
    ;
        MaybeWithType0 = no,
        ExtraTypes = [],
        !:Specs = []
    ),

    replace_in_pred_mode(MaybeRecord, InstEqvMap,
        PredName, list.length(TypesAndModes0), Context, type_decl,
        ExtraModes, yes(PredOrFunc), _, MaybeWithInst0, _, !MaybeDetism,
        !EquivTypeInfo, !UsedModules, ModeSpecs),
    !:Specs = !.Specs ++ ModeSpecs,

    (
        !.Specs = [_ | _],
        ExtraTypesAndModes = []
    ;
        !.Specs = [],
        (
            ExtraModes = [],
            ExtraTypesAndModes = list.map((func(Type) = type_only(Type)),
                ExtraTypes)
        ;
            ExtraModes = [_ | _],
            pair_extra_types_and_modes(ExtraTypes, ExtraModes,
                ExtraTypesAndModes, LeftOverExtraTypes, LeftOverExtraModes),
            (
                LeftOverExtraTypes = [],
                LeftOverExtraModes = []
            ;
                LeftOverExtraTypes = [],
                LeftOverExtraModes = [_ | _],
                list.length(ExtraTypes, NumExtraTypes),
                list.length(ExtraModes, NumExtraModes),
                Pieces2 = [words("In type declaration for"),
                    p_or_f(PredOrFunc), qual_sym_name(PredName),
                    suffix(":"), nl,
                    words("error: the `with_type` and `with_inst`"),
                    words("annotations are incompatible;"),
                    words("they specify"), int_fixed(NumExtraModes),
                    words(choose_number(ExtraModes, "mode", "modes")),
                    words("but only"), int_fixed(NumExtraTypes),
                    words(choose_number(ExtraTypes, "type.", "types.")), nl],
                Spec2 = simplest_spec($pred, severity_error,
                    phase_expand_types, Context, Pieces2),
                !:Specs = [Spec2 | !.Specs]
            ;
                LeftOverExtraTypes = [_ | _],
                LeftOverExtraModes = [],
                list.length(ExtraTypes, NumExtraTypes),
                list.length(ExtraModes, NumExtraModes),
                Pieces2 = [words("In type declaration for"),
                    p_or_f(PredOrFunc), qual_sym_name(PredName),
                    suffix(":"), nl,
                    words("error: the `with_type` and `with_inst`"),
                    words("annotations are incompatible;"),
                    words("they specify"), int_fixed(NumExtraTypes),
                    words(choose_number(ExtraTypes, "type", "types")),
                    words("but only"), int_fixed(NumExtraModes),
                    words(choose_number(ExtraModes, "mode.", "modes.")), nl],
                Spec2 = simplest_spec($pred, severity_error,
                    phase_expand_types, Context, Pieces2),
                !:Specs = [Spec2 | !.Specs]
            ;
                LeftOverExtraTypes = [_ | _],
                LeftOverExtraModes = [_ | _],
                % pair_extra_types_and_modes should have paired these up.
                unexpected($pred, "both types and modes left over")
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
    (
        ExtraTypesAndModes = [],
        TypesAndModes = TypesAndModes1
    ;
        ExtraTypesAndModes = [_ | _],
        OrigItemId = item_id(pred_or_func_to_item_type(PredOrFunc),
            item_name(PredName, list.length(TypesAndModes0))),
        record_expanded_item(OrigItemId, !EquivTypeInfo),
        TypesAndModes = TypesAndModes1 ++ ExtraTypesAndModes
    ).

:- pred pair_extra_types_and_modes(list(mer_type)::in, list(mer_mode)::in,
    list(type_and_mode)::out, list(mer_type)::out, list(mer_mode)::out) is det.

pair_extra_types_and_modes([], [], [], [], []).
pair_extra_types_and_modes(LeftOverTypes @ [_ | _], [], [], LeftOverTypes, []).
pair_extra_types_and_modes([], LeftOverModes @ [_ | _], [], [], LeftOverModes).
pair_extra_types_and_modes([Type | Types], [Mode | Modes],
        [type_and_mode(Type, Mode) | TypesAndModes],
        LeftOverTypes, LeftOverModes) :-
    pair_extra_types_and_modes(Types, Modes, TypesAndModes,
        LeftOverTypes, LeftOverModes).

:- type pred_or_func_decl_type
    --->    type_decl
    ;       mode_decl.

:- pred replace_in_pred_mode(maybe_record_sym_name_use::in, inst_eqv_map::in,
    sym_name::in, arity::in, prog_context::in, pred_or_func_decl_type::in,
    list(mer_mode)::out,
    maybe(pred_or_func)::in, maybe(pred_or_func)::out,
    maybe(mer_inst)::in, maybe(mer_inst)::out,
    maybe(determinism)::in, maybe(determinism)::out,
    eqv_expanded_info::in, eqv_expanded_info::out,
    used_modules::in, used_modules::out, list(error_spec)::out) is det.

replace_in_pred_mode(MaybeRecord, InstEqvMap, PredName, OrigArity, Context,
        DeclType, ExtraModes, MaybePredOrFunc0, MaybePredOrFunc,
        MaybeWithInst0, MaybeWithInst, !MaybeDetism,
        !EquivTypeInfo, !UsedModules, Specs) :-
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
            OrigItemId = item_id(pred_or_func_to_item_type(RecordedPredOrFunc),
                item_name(PredName, OrigArity)),
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
                words("error: expected higher order ")] ++ PredOrFuncPieces ++
                [words("inst after `with_inst`."), nl],
            Spec = simplest_spec($pred, severity_error, phase_expand_types,
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

:- pred replace_in_types_and_modes(maybe_record_sym_name_use::in,
    type_eqv_map::in, list(type_and_mode)::in, list(type_and_mode)::out,
    tvarset::in, tvarset::out, eqv_expanded_info::in, eqv_expanded_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_types_and_modes(MaybeRecord, TypeEqvMap,
        !TypeAndModes, !TVarSet, !EquivTypeInfo, !UsedModules) :-
    list.map_foldl3(replace_in_type_and_mode(MaybeRecord, TypeEqvMap),
        !TypeAndModes, !TVarSet, !EquivTypeInfo, !UsedModules).

:- pred replace_in_type_and_mode(maybe_record_sym_name_use::in,
    type_eqv_map::in, type_and_mode::in, type_and_mode::out,
    tvarset::in, tvarset::out, eqv_expanded_info::in, eqv_expanded_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_type_and_mode(MaybeRecord, TypeEqvMap, TypeAndMode0, TypeAndMode,
        !TVarSet, !EquivTypeInfo, !UsedModules) :-
    (
        TypeAndMode0 = type_only(Type0),
        replace_in_type_maybe_record_use(MaybeRecord, TypeEqvMap,
            Type0, Type, _, !TVarSet, !EquivTypeInfo, !UsedModules),
        TypeAndMode = type_only(Type)
    ;
        TypeAndMode0 = type_and_mode(Type0, Mode),
        replace_in_type_maybe_record_use(MaybeRecord, TypeEqvMap,
            Type0, Type, _, !TVarSet, !EquivTypeInfo, !UsedModules),
        TypeAndMode = type_and_mode(Type, Mode)
    ).

%---------------------------------------------------------------------------%

:- pred replace_in_structure_sharing_domain(maybe_record_sym_name_use::in,
    type_eqv_map::in, tvarset::in,
    structure_sharing_domain::in, structure_sharing_domain::out,
    eqv_expanded_info::in, eqv_expanded_info::out,
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
    eqv_expanded_info::in, eqv_expanded_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_structure_sharing_pair(MaybeRecord, TypeEqvMap, TVarSet,
        SSA0 - SSB0, SSA - SSB, !EquivTypeInfo, !UsedModules) :-
    replace_in_datastruct(MaybeRecord, TypeEqvMap, TVarSet, SSA0, SSA,
        !EquivTypeInfo, !UsedModules),
    replace_in_datastruct(MaybeRecord, TypeEqvMap, TVarSet, SSB0, SSB,
        !EquivTypeInfo, !UsedModules).

:- pred replace_in_datastruct(maybe_record_sym_name_use::in,
    type_eqv_map::in, tvarset::in, datastruct::in,
    datastruct::out, eqv_expanded_info::in, eqv_expanded_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_datastruct(MaybeRecord, TypeEqvMap, TVarSet, DS0, DS,
        !EquivTypeInfo, !UsedModules) :-
    DS0 = selected_cel(Var, Sel0),
    list.map_foldl2(replace_in_unit_selector(MaybeRecord, TypeEqvMap, TVarSet),
        Sel0, Sel, !EquivTypeInfo, !UsedModules),
    DS = selected_cel(Var, Sel).

:- pred replace_in_unit_selector(maybe_record_sym_name_use::in,
    type_eqv_map::in, tvarset::in, unit_selector::in,
    unit_selector::out, eqv_expanded_info::in, eqv_expanded_info::out,
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
        MaybeRecord = dont_record_sym_name_use
    ;
        MaybeRecord = record_sym_name_use(Visibility),
        TypeCtor = type_ctor(TypeCtorSymName, _TypeCtorArity),
        record_sym_name_module_as_used(Visibility, TypeCtorSymName,
            !UsedModules)
    ).

%---------------------------------------------------------------------------%
:- end_module parse_tree.equiv_type.
%---------------------------------------------------------------------------%
