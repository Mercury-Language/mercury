%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2012 The University of Melbourne.
% Copyright (C) 2014-2026 The Mercury team.
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
:- import_module parse_tree.build_eqv_maps.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_event.
:- import_module parse_tree.prog_data_used_modules.
:- import_module parse_tree.prog_item.
:- import_module parse_tree.prog_parse_tree.
:- import_module recompilation.
:- import_module recompilation.record_uses.

:- import_module list.
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

%---------------------------------------------------------------------------%

    % Replace equivalence types in the representation of an equivalence type.
    % Generate an error message if the expansion reveals that the definition
    % is circular.
    %
:- pred replace_in_type_repn_eqv(type_eqv_map::in,
    item_type_repn_info_eqv::in, item_type_repn_info_eqv::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred replace_in_ctors(type_eqv_map::in,
    one_or_more(constructor)::in, one_or_more(constructor)::out,
    tvarset::in, tvarset::out,
    item_recomp_deps::in, item_recomp_deps::out) is det.

:- pred replace_in_univ_exist_constraints(type_eqv_map::in,
    univ_exist_constraints::in, univ_exist_constraints::out,
    tvarset::in, tvarset::out,
    item_recomp_deps::in, item_recomp_deps::out) is det.

:- pred replace_in_prog_constraints(type_eqv_map::in,
    list(prog_constraint)::in, list(prog_constraint)::out,
    tvarset::in, tvarset::out, item_recomp_deps::in, item_recomp_deps::out)
    is det.

:- pred replace_in_type_list(type_eqv_map::in,
    list(mer_type)::in, list(mer_type)::out, maybe_changed::out,
    tvarset::in, tvarset::out,
    item_recomp_deps::in, item_recomp_deps::out) is det.

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
    item_recomp_deps::in, item_recomp_deps::out) is det.

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
:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- type circ_types == set(type_ctor).

:- type equiv_params
    --->    equiv_params(
                ep_module_name      :: module_name,
                ep_type_eqv_map     :: type_eqv_map,
                ep_inst_eqv_map     :: inst_eqv_map
            ).

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
    build_eqv_maps_in_aug_comp_unit(AugCompUnit0, TypeEqvMap, InstEqvMap),
    Params = equiv_params(ModuleName, TypeEqvMap, InstEqvMap),

    % .. and then we go through all the items in the relevant blocks
    % and in all the event specs, and replace all occurrences of
    % equivalence types and insts in them.
    !:UsedModules = used_modules_init,
    !:Specs = [],
    replace_in_parse_tree_module_src(Params,
        ParseTreeModuleSrc0, ParseTreeModuleSrc,
        !RecompInfo, !UsedModules, !Specs),
    map.map_values_foldl3(replace_in_ancestor_int_spec(Params),
        AncestorIntSpecs0, AncestorIntSpecs,
        !RecompInfo, !UsedModules, !Specs),
    map.map_values_foldl3(replace_in_direct_int1_spec(Params),
        DirectInt1Specs0, DirectInt1Specs, !RecompInfo, !UsedModules, !Specs),
    map.map_values_foldl3(replace_in_indirect_int2_spec(Params),
        IndirectInt2Specs0, IndirectInt2Specs,
        !RecompInfo, !UsedModules, !Specs),
    map.map_values_foldl3(replace_in_parse_tree_trans_opt(Params),
        TransOpts0, TransOpts, !RecompInfo, !UsedModules, !Specs),
    map.map_values_foldl3(replace_in_parse_tree_plain_opt(Params),
        PlainOpts0, PlainOpts, !RecompInfo, !UsedModules, !Specs),
    map.map_values_foldl3(replace_in_int_for_opt_spec(Params),
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

:- pred replace_in_parse_tree_module_src(equiv_params::in,
    parse_tree_module_src::in, parse_tree_module_src::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out,
    list(error_spec)::in, list(error_spec)::out) is det.

replace_in_parse_tree_module_src(Params,
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
        replace_in_type_ctor_checked_defn(Params,
            MaybeRecordInt, MaybeRecordImp),
        TypeCtorCheckedMap0, TypeCtorCheckedMap,
        !RecompInfo, !UsedModules, !Specs),
    % XXX See the comment at module top.
    InstCtorCheckedMap = InstCtorCheckedMap0,
    ModeCtorCheckedMap = ModeCtorCheckedMap0,

    replace_in_list(Params, MaybeRecordInt,
        replace_in_typeclass_info, IntTypeClasses0, IntTypeClasses,
        !RecompInfo, !UsedModules, !Specs),
    replace_in_list(Params, MaybeRecordInt,
        replace_in_instance_info, IntInstances0, IntInstances,
        !RecompInfo, !UsedModules, !Specs),
    replace_in_list(Params, MaybeRecordInt,
        replace_in_pred_decl_info, IntPredDecls0, IntPredDecls,
        !RecompInfo, !UsedModules, !Specs),
    replace_in_list(Params, MaybeRecordInt,
        replace_in_mode_decl_info, IntModeDecls0, IntModeDecls,
        !RecompInfo, !UsedModules, !Specs),
    replace_in_list(Params, MaybeRecordInt,
        replace_in_decl_pragma_info, IntDeclPragmas0, IntDeclPragmas,
        !RecompInfo, !UsedModules, !Specs),

    replace_in_list(Params, MaybeRecordImp,
        replace_in_typeclass_info, ImpTypeClasses0, ImpTypeClasses,
        !RecompInfo, !UsedModules, !Specs),
    replace_in_list(Params, MaybeRecordImp,
        replace_in_instance_info, ImpInstances0, ImpInstances,
        !RecompInfo, !UsedModules, !Specs),
    replace_in_list(Params, MaybeRecordImp,
        replace_in_pred_decl_info, ImpPredDecls0, ImpPredDecls,
        !RecompInfo, !UsedModules, !Specs),
    replace_in_list(Params, MaybeRecordImp,
        replace_in_mode_decl_info, ImpModeDecls0, ImpModeDecls,
        !RecompInfo, !UsedModules, !Specs),
    ImpClauses = ImpClauses0, % XXX See the comment at module top.
    replace_in_list(Params, MaybeRecordImp,
        replace_in_decl_pragma_info, ImpDeclPragmas0, ImpDeclPragmas,
        !RecompInfo, !UsedModules, !Specs),
    replace_in_list(Params, MaybeRecordImp,
        replace_in_foreign_proc, ImpForeignProcs0, ImpForeignProcs,
        !RecompInfo, !UsedModules, !Specs),
    replace_in_list(Params, MaybeRecordImp,
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

:- pred replace_in_ancestor_int_spec(equiv_params::in,
    ancestor_int_spec::in, ancestor_int_spec::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out,
    list(error_spec)::in, list(error_spec)::out) is det.

replace_in_ancestor_int_spec(Params, AncestorIntSpec0, AncestorIntSpec,
        !RecompInfo, !UsedModules, !Specs) :-
    AncestorIntSpec0 = ancestor_int0(OrigParseTree0, ReadWhy0),
    replace_in_parse_tree_int0(Params, OrigParseTree0, ParseTree0,
        !RecompInfo, !UsedModules, !Specs),
    AncestorIntSpec = ancestor_int0(ParseTree0, ReadWhy0).

:- pred replace_in_direct_int1_spec(equiv_params::in,
    direct_int1_spec::in, direct_int1_spec::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out,
    list(error_spec)::in, list(error_spec)::out) is det.

replace_in_direct_int1_spec(Params, DirectIntSpec0, DirectIntSpec,
        !RecompInfo, !UsedModules, !Specs) :-
    DirectIntSpec0 = direct_int1(OrigParseTree1, ReadWhy1),
    replace_in_parse_tree_int1(Params, OrigParseTree1, ParseTree1,
        !RecompInfo, !UsedModules, !Specs),
    DirectIntSpec = direct_int1(ParseTree1, ReadWhy1).

:- pred replace_in_indirect_int2_spec(equiv_params::in,
    indirect_int2_spec::in, indirect_int2_spec::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out,
    list(error_spec)::in, list(error_spec)::out) is det.

replace_in_indirect_int2_spec(Params, IndirectIntSpec0, IndirectIntSpec,
        !RecompInfo, !UsedModules, !Specs) :-
    IndirectIntSpec0 = indirect_int2(OrigParseTree2, ReadWhy2),
    replace_in_parse_tree_int2(Params, OrigParseTree2, ParseTree2,
        !RecompInfo, !UsedModules, !Specs),
    IndirectIntSpec = indirect_int2(ParseTree2, ReadWhy2).

:- pred replace_in_int_for_opt_spec(equiv_params::in,
    int_for_opt_spec::in, int_for_opt_spec::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out,
    list(error_spec)::in, list(error_spec)::out) is det.

replace_in_int_for_opt_spec(Params, IntForOptSpec0, IntForOptSpec,
        !RecompInfo, !UsedModules, !Specs) :-
    (
        IntForOptSpec0 = for_opt_int0(OrigParseTree0, ReadWhy0),
        replace_in_parse_tree_int0(Params, OrigParseTree0, ParseTree0,
            !RecompInfo, !UsedModules, !Specs),
        IntForOptSpec = for_opt_int0(ParseTree0, ReadWhy0)
    ;
        IntForOptSpec0 = for_opt_int1(OrigParseTree1, ReadWhy1),
        replace_in_parse_tree_int1(Params, OrigParseTree1, ParseTree1,
            !RecompInfo, !UsedModules, !Specs),
        IntForOptSpec = for_opt_int1(ParseTree1, ReadWhy1)
    ;
        IntForOptSpec0 = for_opt_int2(OrigParseTree2, ReadWhy2),
        replace_in_parse_tree_int2(Params, OrigParseTree2, ParseTree2,
            !RecompInfo, !UsedModules, !Specs),
        IntForOptSpec = for_opt_int2(ParseTree2, ReadWhy2)
    ).

%---------------------------------------------------------------------------%

:- pred replace_in_parse_tree_int0(equiv_params::in,
    parse_tree_int0::in, parse_tree_int0::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out,
    list(error_spec)::in, list(error_spec)::out) is det.

replace_in_parse_tree_int0(Params, OrigParseTreeInt0, ParseTreeInt0,
        !RecompInfo, !UsedModules, !Specs) :-
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
        replace_in_type_ctor_checked_defn(Params,
            MaybeRecordInt, MaybeRecordImp),
        TypeCtorCheckedMap0, TypeCtorCheckedMap,
        !RecompInfo, !UsedModules, !Specs),
    % XXX See the comment at module top.
    InstCtorCheckedMap = InstCtorCheckedMap0,
    ModeCtorCheckedMap = ModeCtorCheckedMap0,

    replace_in_list(Params, MaybeRecordInt,
        replace_in_typeclass_info, IntTypeClasses0, IntTypeClasses,
        !RecompInfo, !UsedModules, !Specs),
    replace_in_list(Params, MaybeRecordInt,
        replace_in_abstract_instance_info, IntInstances0, IntInstances,
        !RecompInfo, !UsedModules, !Specs),
    replace_in_list(Params, MaybeRecordInt,
        replace_in_pred_decl_info, IntPredDecls0, IntPredDecls,
        !RecompInfo, !UsedModules, !Specs),
    replace_in_list(Params, MaybeRecordInt,
        replace_in_mode_decl_info, IntModeDecls0, IntModeDecls,
        !RecompInfo, !UsedModules, !Specs),
    replace_in_list(Params, MaybeRecordInt,
        replace_in_decl_pragma_info, IntDeclPragmas0, IntDeclPragmas,
        !RecompInfo, !UsedModules, !Specs),

    replace_in_list(Params, MaybeRecordImp,
        replace_in_typeclass_info, ImpTypeClasses0, ImpTypeClasses,
        !RecompInfo, !UsedModules, !Specs),
    replace_in_list(Params, MaybeRecordImp,
        replace_in_abstract_instance_info, ImpInstances0, ImpInstances,
        !RecompInfo, !UsedModules, !Specs),
    replace_in_list(Params, MaybeRecordImp,
        replace_in_pred_decl_info, ImpPredDecls0, ImpPredDecls,
        !RecompInfo, !UsedModules, !Specs),
    replace_in_list(Params, MaybeRecordImp,
        replace_in_mode_decl_info, ImpModeDecls0, ImpModeDecls,
        !RecompInfo, !UsedModules, !Specs),
    replace_in_list(Params, MaybeRecordImp,
        replace_in_decl_pragma_info, ImpDeclPragmas0, ImpDeclPragmas,
        !RecompInfo, !UsedModules, !Specs),

    ParseTreeInt0 = parse_tree_int0(IntModuleName, IntModuleNameContext,
        MaybeVersionNumbers, InclMap, ImportUseMap, IntFIMSpecs, ImpFIMSpecs,
        TypeCtorCheckedMap, InstCtorCheckedMap, ModeCtorCheckedMap,
        IntTypeClasses, IntInstances, IntPredDecls, IntModeDecls,
        IntDeclPragmas, IntDeclMarkers, IntPromises,
        ImpTypeClasses, ImpInstances, ImpPredDecls, ImpModeDecls,
        ImpDeclPragmas, ImpDeclMarkers, ImpPromises).

:- pred replace_in_parse_tree_int1(equiv_params::in,
    parse_tree_int1::in, parse_tree_int1::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out,
    list(error_spec)::in, list(error_spec)::out) is det.

replace_in_parse_tree_int1(Params, OrigParseTreeInt1, ParseTreeInt1,
        !RecompInfo, !UsedModules, !Specs) :-
    MaybeRecordInt = do_not_record_sym_name_use,
    MaybeRecordImp = do_not_record_sym_name_use,
    OrigParseTreeInt1 = parse_tree_int1(IntModuleName, IntModuleNameContext,
        MaybeVersionNumbers, InclMap, ImportUseMap, IntFIMSpecs, ImpFIMSpecs,
        TypeCtorCheckedMap0, InstCtorCheckedMap0, ModeCtorCheckedMap0,
        IntTypeClasses0, IntInstances0, IntPredDecls0, IntModeDecls0,
        IntDeclPragmas0, IntDeclMarkers0, IntPromises, IntTypeRepnMap0,
        ImpTypeClasses0),

    map.map_values_foldl3(
        replace_in_type_ctor_checked_defn(Params,
            MaybeRecordInt, MaybeRecordImp),
        TypeCtorCheckedMap0, TypeCtorCheckedMap,
        !RecompInfo, !UsedModules, !Specs),
    % XXX See the comment at module top.
    InstCtorCheckedMap = InstCtorCheckedMap0,
    ModeCtorCheckedMap = ModeCtorCheckedMap0,

    replace_in_list(Params, MaybeRecordInt,
        replace_in_typeclass_info, IntTypeClasses0, IntTypeClasses,
        !RecompInfo, !UsedModules, !Specs),
    replace_in_list(Params, MaybeRecordInt,
        replace_in_abstract_instance_info, IntInstances0, IntInstances,
        !RecompInfo, !UsedModules, !Specs),
    replace_in_list(Params, MaybeRecordInt,
        replace_in_pred_decl_info, IntPredDecls0, IntPredDecls,
        !RecompInfo, !UsedModules, !Specs),
    replace_in_list(Params, MaybeRecordInt,
        replace_in_mode_decl_info, IntModeDecls0, IntModeDecls,
        !RecompInfo, !UsedModules, !Specs),
    replace_in_list(Params, MaybeRecordInt,
        replace_in_decl_pragma_info, IntDeclPragmas0, IntDeclPragmas,
        !RecompInfo, !UsedModules, !Specs),
    map.map_values_foldl3(
        replace_in_type_repn_info(Params, MaybeRecordInt),
        IntTypeRepnMap0, IntTypeRepnMap,
        !RecompInfo, !UsedModules, !Specs),

    replace_in_list(Params, MaybeRecordImp,
        replace_in_abstract_typeclass_info, ImpTypeClasses0, ImpTypeClasses,
        !RecompInfo, !UsedModules, !Specs),

    ParseTreeInt1 = parse_tree_int1(IntModuleName, IntModuleNameContext,
        MaybeVersionNumbers, InclMap, ImportUseMap, IntFIMSpecs, ImpFIMSpecs,
        TypeCtorCheckedMap, InstCtorCheckedMap, ModeCtorCheckedMap,
        IntTypeClasses, IntInstances, IntPredDecls, IntModeDecls,
        IntDeclPragmas, IntDeclMarkers0, IntPromises, IntTypeRepnMap,
        ImpTypeClasses).

:- pred replace_in_parse_tree_int2(equiv_params::in,
    parse_tree_int2::in, parse_tree_int2::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out,
    list(error_spec)::in, list(error_spec)::out) is det.

replace_in_parse_tree_int2(Params, OrigParseTreeInt2, ParseTreeInt2,
        !RecompInfo, !UsedModules, !Specs) :-
    MaybeRecordInt = do_not_record_sym_name_use,
    MaybeRecordImp = do_not_record_sym_name_use,
    OrigParseTreeInt2 = parse_tree_int2(IntModuleName, IntModuleNameContext,
        MaybeVersionNumbers, InclMap, ImportUseMap, IntFIMSpecs, ImpFIMSpecs,
        TypeCtorCheckedMap0, InstCtorCheckedMap0, ModeCtorCheckedMap0,
        IntTypeClasses0, IntInstances0, IntTypeRepnMap0),

    map.map_values_foldl3(
        replace_in_type_ctor_checked_defn(Params,
            MaybeRecordInt, MaybeRecordImp),
        TypeCtorCheckedMap0, TypeCtorCheckedMap,
        !RecompInfo, !UsedModules, !Specs),
    % XXX See the comment at module top.
    InstCtorCheckedMap = InstCtorCheckedMap0,
    ModeCtorCheckedMap = ModeCtorCheckedMap0,

    replace_in_list(Params, MaybeRecordInt,
        replace_in_typeclass_info, IntTypeClasses0, IntTypeClasses,
        !RecompInfo, !UsedModules, !Specs),
    replace_in_list(Params, MaybeRecordInt,
        replace_in_abstract_instance_info, IntInstances0, IntInstances,
        !RecompInfo, !UsedModules, !Specs),
    map.map_values_foldl3(
        replace_in_type_repn_info(Params, MaybeRecordInt),
        IntTypeRepnMap0, IntTypeRepnMap,
        !RecompInfo, !UsedModules, !Specs),

    ParseTreeInt2 = parse_tree_int2(IntModuleName, IntModuleNameContext,
        MaybeVersionNumbers, InclMap, ImportUseMap, IntFIMSpecs, ImpFIMSpecs,
        TypeCtorCheckedMap, InstCtorCheckedMap, ModeCtorCheckedMap,
        IntTypeClasses, IntInstances, IntTypeRepnMap).

:- pred replace_in_parse_tree_plain_opt(equiv_params::in,
    parse_tree_plain_opt::in, parse_tree_plain_opt::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out,
    list(error_spec)::in, list(error_spec)::out) is det.

replace_in_parse_tree_plain_opt(Params,
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
    replace_in_list(Params, MaybeRecord,
        replace_in_type_defn_info_general(replace_in_type_defn),
        TypeDefns0, TypeDefns, !RecompInfo, !UsedModules, !Specs),
    replace_in_list(Params, MaybeRecord,
        replace_in_typeclass_info, TypeClasses0, TypeClasses,
        !RecompInfo, !UsedModules, !Specs),
    replace_in_list(Params, MaybeRecord,
        replace_in_instance_info, Instances0, Instances,
        !RecompInfo, !UsedModules, !Specs),
    replace_in_list(Params, MaybeRecord,
        replace_in_pred_decl_info, PredDecls0, PredDecls,
        !RecompInfo, !UsedModules, !Specs),
    replace_in_list(Params, MaybeRecord,
        replace_in_mode_decl_info, ModeDecls0, ModeDecls,
        !RecompInfo, !UsedModules, !Specs),
    replace_in_list(Params, MaybeRecord,
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

:- pred replace_in_parse_tree_trans_opt(equiv_params::in,
    parse_tree_trans_opt::in, parse_tree_trans_opt::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out,
    list(error_spec)::in, list(error_spec)::out) is det.

replace_in_parse_tree_trans_opt(_Params, ParseTreeTransOpt, ParseTreeTransOpt,
        RecompInfo, RecompInfo, UsedModules, UsedModules, Specs, Specs).
    % No component that may appear in a parse_tree_trans_opt
    % needs any expansions.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- pred replace_in_type_ctor_checked_defn(equiv_params::in,
    maybe_record_sym_name_use::in, maybe_record_sym_name_use::in,
    type_ctor_checked_defn::in, type_ctor_checked_defn::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out,
    list(error_spec)::in, list(error_spec)::out) is det.

replace_in_type_ctor_checked_defn(Params, MaybeRecordInt, MaybeRecordImp,
        CheckedDefn0, CheckedDefn, !RecompInfo, !UsedModules, !Specs) :-
    (
        CheckedDefn0 = checked_defn_solver(SolverDefn0, SrcDefns0),
        (
            SolverDefn0 = solver_type_abstract(_, _),
            SolverDefn = SolverDefn0
        ;
            SolverDefn0 =
                solver_type_full(MaybeAbstractDefn0, ItemSolverDefn0),
            replace_in_type_defn_info_general(replace_in_type_defn_solver,
                Params, MaybeRecordImp, ItemSolverDefn0, ItemSolverDefn,
                !RecompInfo, !UsedModules, SolverSpecs),
            !:Specs = SolverSpecs ++ !.Specs,
            % Abstract type definitions have no equivalences to expand out.
            SolverDefn = solver_type_full(MaybeAbstractDefn0, ItemSolverDefn)
        ),
        SrcDefns0 = src_defns_solver(MaybeIntDefn0, MaybeImpDefn0),
        replace_in_maybe(Params, MaybeRecordInt,
            replace_in_type_defn_info_general(replace_in_type_defn),
            MaybeIntDefn0, MaybeIntDefn, !RecompInfo, !UsedModules, !Specs),
        replace_in_maybe(Params, MaybeRecordImp,
            replace_in_type_defn_info_general(replace_in_type_defn),
            MaybeImpDefn0, MaybeImpDefn, !RecompInfo, !UsedModules, !Specs),
        SrcDefns = src_defns_solver(MaybeIntDefn, MaybeImpDefn),
        CheckedDefn = checked_defn_solver(SolverDefn, SrcDefns)
    ;
        CheckedDefn0 = checked_defn_std(StdDefn0, SrcDefns0),
        (
            StdDefn0 = std_mer_type_eqv(Status, ItemEqvDefn0),
            replace_in_type_defn_info_general(replace_in_type_defn_eqv,
                Params, MaybeRecordImp, ItemEqvDefn0, ItemEqvDefn,
                !RecompInfo, !UsedModules, EqvSpecs),
            !:Specs = EqvSpecs ++ !.Specs,
            StdDefn = std_mer_type_eqv(Status, ItemEqvDefn)
        ;
            StdDefn0 = std_mer_type_subtype(Status, ItemSubDefn0),
            replace_in_type_defn_info_general(replace_in_type_defn_sub,
                Params, MaybeRecordImp, ItemSubDefn0, ItemSubDefn,
                !RecompInfo, !UsedModules, SubSpecs),
            !:Specs = SubSpecs ++ !.Specs,
            StdDefn = std_mer_type_subtype(Status, ItemSubDefn)
        ;
            StdDefn0 = std_mer_type_du_all_plain_constants(Status,
                ItemDuDefn0, HeadCtor, TailCtors, CJCsMaybeDefnOrEnum),
            replace_in_type_defn_info_general(replace_in_type_defn_du,
                Params, MaybeRecordImp, ItemDuDefn0, ItemDuDefn,
                !RecompInfo, !UsedModules, DuSpecs),
            !:Specs = DuSpecs ++ !.Specs,
            % Foreign type definitions and enums have no equivalences
            % to expand out.
            StdDefn = std_mer_type_du_all_plain_constants(Status,
                ItemDuDefn, HeadCtor, TailCtors, CJCsMaybeDefnOrEnum)
        ;
            StdDefn0 = std_mer_type_du_not_all_plain_constants(Status,
                ItemDuDefn0, CJCsMaybeDefn),
            replace_in_type_defn_info_general(replace_in_type_defn_du,
                Params, MaybeRecordImp, ItemDuDefn0, ItemDuDefn,
                !RecompInfo, !UsedModules, DuSpecs),
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
        replace_in_list(Params, MaybeRecordInt,
            replace_in_type_defn_info_general(replace_in_type_defn),
            IntDefns0, IntDefns, !RecompInfo, !UsedModules, !Specs),
        replace_in_list(Params, MaybeRecordImp,
            replace_in_type_defn_info_general(replace_in_type_defn),
            ImpDefns0, ImpDefns, !RecompInfo, !UsedModules, !Specs),
        % Foreign enum infos have no equivalences to expand out.
        SrcDefns = src_defns_std(IntDefns, ImpDefns, ImpForeignEnums0),
        CheckedDefn = checked_defn_std(StdDefn, SrcDefns)
    ).

    % NOTE: ReplaceInTypeDefn must be the first argument,
    % because some of our indirect callers curry it.
    %
:- pred replace_in_type_defn_info_general(
    pred(equiv_params, maybe_record_sym_name_use, type_ctor, prog_context,
        T, T, tvarset, tvarset, item_recomp_deps, item_recomp_deps,
        used_modules, used_modules, list(error_spec))
    :: in(pred(in, in, in, in, in, out, in, out, in, out, in, out, out)
        is det),
    equiv_params::in, maybe_record_sym_name_use::in,
    item_type_defn_info_general(T)::in, item_type_defn_info_general(T)::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out, list(error_spec)::out) is det.

replace_in_type_defn_info_general(ReplaceInTypeDefn, Params, MaybeRecord,
        TypeDefnInfo0, TypeDefnInfo, !RecompInfo, !UsedModules, Specs) :-
    TypeDefnInfo0 = item_type_defn_info(SymName, ArgTypeVars, TypeDefn0,
        TVarSet0, Context, SeqNum),
    ModuleName = Params ^ ep_module_name,
    list.length(ArgTypeVars, Arity),
    ItemName = recomp_item_name(SymName, Arity),
    ItemId = recomp_item_id(recomp_type_defn, ItemName),
    maybe_start_gathering_item_recomp_deps(ModuleName, ItemId, !.RecompInfo,
        ItemRecompDeps0),
    TypeCtor = type_ctor(SymName, Arity),
    ReplaceInTypeDefn(Params, MaybeRecord, TypeCtor, Context,
        TypeDefn0, TypeDefn, TVarSet0, TVarSet,
        ItemRecompDeps0, ItemRecompDeps, !UsedModules, Specs),
    finish_gathering_item_recomp_deps(ItemId, ItemRecompDeps, !RecompInfo),
    TypeDefnInfo = item_type_defn_info(SymName, ArgTypeVars, TypeDefn,
        TVarSet, Context, SeqNum).

:- pred replace_in_type_repn_info(equiv_params::in,
    maybe_record_sym_name_use::in,
    item_type_repn_info::in, item_type_repn_info::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out,
    list(error_spec)::in, list(error_spec)::out) is det.

replace_in_type_repn_info(Params, MaybeRecord, TypeRepnInfo0, TypeRepnInfo,
        !RecompInfo, !UsedModules, !Specs) :-
    TypeRepnInfo0 = item_type_repn_info(SymName, ArgTypeVars, TypeRepn0,
        TVarSet0, Context, SeqNum),
    ModuleName = Params ^ ep_module_name,
    list.length(ArgTypeVars, Arity),
    ItemName = recomp_item_name(SymName, Arity),
    ItemId = recomp_item_id(recomp_type_defn, ItemName),
    maybe_start_gathering_item_recomp_deps(ModuleName, ItemId, !.RecompInfo,
        ItemRecompDeps0),
    TypeEqvMap = Params ^ ep_type_eqv_map,
    (
        TypeRepn0 = tcrepn_is_eqv_to(Type0),
        TypeCtor = type_ctor(SymName, Arity),
        replace_in_type_maybe_record_use(TypeEqvMap, MaybeRecord, [TypeCtor],
            Type0, Type, _, Circ, TVarSet0, TVarSet,
            ItemRecompDeps0, ItemRecompDeps, !UsedModules),
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
        % parameters, so that we can call replace_in_type_maybe_record_use.
        % We do not care about the type arguments so we can drop them again
        % afterwards.
        SuperTypeCtor0 = type_ctor(_, SuperTypeCtorArity),
        list.duplicate(SuperTypeCtorArity, void_type, VoidTypes),
        construct_type(SuperTypeCtor0, VoidTypes, SuperType0),
        TypeCtor = type_ctor(SymName, Arity),
        replace_in_type_maybe_record_use(TypeEqvMap, MaybeRecord, [TypeCtor],
            SuperType0, SuperType, _, Circ, TVarSet0, TVarSet,
            ItemRecompDeps0, ItemRecompDeps, !UsedModules),
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
        ItemRecompDeps = ItemRecompDeps0
    ),
    finish_gathering_item_recomp_deps(ItemId, ItemRecompDeps, !RecompInfo),
    TypeRepnInfo = item_type_repn_info(SymName, ArgTypeVars, TypeRepn,
        TVarSet, Context, SeqNum).

replace_in_type_repn_eqv(TypeEqvMap, TypeRepnInfo0, TypeRepnInfo, !Specs) :-
    TypeRepnInfo0 = item_type_repn_info(SymName, ArgTypeVars, Type0, TVarSet0,
        Context, SeqNum),
    list.length(ArgTypeVars, Arity),
    TypeCtor = type_ctor(SymName, Arity),
    replace_in_type_maybe_record_use(TypeEqvMap, do_not_record_sym_name_use,
        [], Type0, Type, _Changed, Circ, TVarSet0, TVarSet,
        no_item_recomp_deps, _, used_modules_init, _),
    set.to_sorted_list(Circ, CircTypes),
    (
        CircTypes = [_ | _],
        !:Specs = [report_circular_eqv_type(TypeCtor, Context) | !.Specs]
    ;
        CircTypes = []
    ),
    TypeRepnInfo = item_type_repn_info(SymName, ArgTypeVars, Type, TVarSet,
        Context, SeqNum).

%---------------------------------------------------------------------------%

:- pred replace_in_type_defn(equiv_params::in, maybe_record_sym_name_use::in,
    type_ctor::in, prog_context::in,
    type_defn::in, type_defn::out,
    tvarset::in, tvarset::out, item_recomp_deps::in, item_recomp_deps::out,
    used_modules::in, used_modules::out, list(error_spec)::out) is det.

replace_in_type_defn(Params, MaybeRecord, TypeCtor, Context,
        TypeDefn0, TypeDefn, !TVarSet, !ItemRecompDeps, !UsedModules, Specs) :-
    (
        TypeDefn0 = parse_tree_eqv_type(DetailsEqv0),
        replace_in_type_defn_eqv(Params, MaybeRecord,
            TypeCtor, Context, DetailsEqv0, DetailsEqv,
            !TVarSet, !ItemRecompDeps, !UsedModules, Specs),
        TypeDefn = parse_tree_eqv_type(DetailsEqv)
    ;
        TypeDefn0 = parse_tree_du_type(DetailsDu0),
        replace_in_type_defn_du(Params, MaybeRecord,
            TypeCtor, Context, DetailsDu0, DetailsDu,
            !TVarSet, !ItemRecompDeps, !UsedModules, Specs),
        TypeDefn = parse_tree_du_type(DetailsDu)
    ;
        TypeDefn0 = parse_tree_sub_type(DetailsSub0),
        replace_in_type_defn_sub(Params, MaybeRecord,
            TypeCtor, Context, DetailsSub0, DetailsSub,
            !TVarSet, !ItemRecompDeps, !UsedModules, Specs),
        TypeDefn = parse_tree_sub_type(DetailsSub)
    ;
        TypeDefn0 = parse_tree_solver_type(DetailsSolver0),
        replace_in_type_defn_solver(Params, MaybeRecord,
            TypeCtor, Context, DetailsSolver0, DetailsSolver,
            !TVarSet, !ItemRecompDeps, !UsedModules, Specs),
        TypeDefn = parse_tree_solver_type(DetailsSolver)
    ;
        ( TypeDefn0 = parse_tree_abstract_type(_)
        ; TypeDefn0 = parse_tree_foreign_type(_)
        ),
        TypeDefn = TypeDefn0,
        Specs = []
    ).

:- pred replace_in_type_defn_eqv(equiv_params::in,
    maybe_record_sym_name_use::in, type_ctor::in, prog_context::in,
    type_details_eqv::in, type_details_eqv::out,
    tvarset::in, tvarset::out, item_recomp_deps::in, item_recomp_deps::out,
    used_modules::in, used_modules::out, list(error_spec)::out) is det.

replace_in_type_defn_eqv(Params, MaybeRecord, TypeCtor, Context,
        DetailsEqv0, DetailsEqv,
        !TVarSet, !ItemRecompDeps, !UsedModules, Specs) :-
    DetailsEqv0 = type_details_eqv(TypeBody0),
    TypeEqvMap = Params ^ ep_type_eqv_map,
    replace_in_type_maybe_record_use(TypeEqvMap, MaybeRecord, [TypeCtor],
        TypeBody0, TypeBody, _, Circ, !TVarSet, !ItemRecompDeps, !UsedModules),
    set.to_sorted_list(Circ, CircTypes),
    (
        CircTypes = [_ | _],
        Specs = [report_circular_eqv_type(TypeCtor, Context)]
    ;
        CircTypes = [],
        Specs = []
    ),
    DetailsEqv = type_details_eqv(TypeBody).

%---------------------%

:- pred replace_in_type_defn_du(equiv_params::in,
    maybe_record_sym_name_use::in, type_ctor::in, prog_context::in,
    type_details_du::in, type_details_du::out,
    tvarset::in, tvarset::out, item_recomp_deps::in, item_recomp_deps::out,
    used_modules::in, used_modules::out, list(error_spec)::out) is det.

replace_in_type_defn_du(Params, MaybeRecord, _TypeCtor, _Context,
        DetailsDu0, DetailsDu,
        !TVarSet, !ItemRecompDeps, !UsedModules, Specs) :-
    DetailsDu0 = type_details_du(Ctors0, MaybeCanon, DirectArgFunctors),
    TypeEqvMap = Params ^ ep_type_eqv_map,
    replace_in_ctors_location(TypeEqvMap, MaybeRecord, Ctors0, Ctors,
        !TVarSet, !ItemRecompDeps, !UsedModules),
    DetailsDu = type_details_du(Ctors, MaybeCanon, DirectArgFunctors),
    Specs = [].

:- pred replace_in_type_defn_sub(equiv_params::in,
    maybe_record_sym_name_use::in, type_ctor::in, prog_context::in,
    type_details_sub::in, type_details_sub::out,
    tvarset::in, tvarset::out, item_recomp_deps::in, item_recomp_deps::out,
    used_modules::in, used_modules::out, list(error_spec)::out) is det.

replace_in_type_defn_sub(Params, MaybeRecord, _TypeCtor, _Context,
        DetailsSub0, DetailsSub,
        !TVarSet, !ItemRecompDeps, !UsedModules, Specs) :-
    DetailsSub0 = type_details_sub(SuperType0, Ctors0),
    TypeEqvMap = Params ^ ep_type_eqv_map,
    replace_in_type_maybe_record_use_ignore_circ(TypeEqvMap, MaybeRecord,
        SuperType0, SuperType, _, !TVarSet, !ItemRecompDeps, !UsedModules),
    replace_in_ctors_location(TypeEqvMap, MaybeRecord, Ctors0, Ctors,
        !TVarSet, !ItemRecompDeps, !UsedModules),
    DetailsSub = type_details_sub(SuperType, Ctors),
    Specs = [].

%---------------------%

replace_in_ctors(TypeEqvMap, !Ctors, !TVarSet, !ItemRecompDeps) :-
    replace_in_ctors_location(TypeEqvMap, do_not_record_sym_name_use,
        !Ctors, !TVarSet, !ItemRecompDeps, used_modules_init, _).

:- pred replace_in_ctors_location(type_eqv_map::in,
    maybe_record_sym_name_use::in,
    one_or_more(constructor)::in, one_or_more(constructor)::out,
    tvarset::in, tvarset::out, item_recomp_deps::in, item_recomp_deps::out,
    used_modules::in, used_modules::out) is det.

replace_in_ctors_location(TypeEqvMap, MaybeRecord, Ctors0, Ctors, !TVarSet,
        !ItemRecompDeps, !UsedModules) :-
    Ctors0 = one_or_more(HeadCtor0, TailCtors0),
    replace_in_ctor(TypeEqvMap, MaybeRecord, HeadCtor0, HeadCtor,
        !TVarSet, !ItemRecompDeps, !UsedModules),
    list.map_foldl3(replace_in_ctor(TypeEqvMap, MaybeRecord),
        TailCtors0, TailCtors,
        !TVarSet, !ItemRecompDeps, !UsedModules),
    Ctors = one_or_more(HeadCtor, TailCtors).

:- pred replace_in_ctor(type_eqv_map::in, maybe_record_sym_name_use::in,
    constructor::in, constructor::out, tvarset::in, tvarset::out,
    item_recomp_deps::in, item_recomp_deps::out,
    used_modules::in, used_modules::out) is det.

replace_in_ctor(TypeEqvMap, MaybeRecord, Ctor0, Ctor,
        !TVarSet, !ItemRecompDeps, !UsedModules) :-
    Ctor0 = ctor(Ordinal, MaybeExistConstraints0, CtorName, CtorArgs0, Arity,
        Ctxt),
    replace_in_ctor_arg_list(TypeEqvMap, MaybeRecord,
        CtorArgs0, CtorArgs, _, !TVarSet, !ItemRecompDeps, !UsedModules),
    (
        MaybeExistConstraints0 = no_exist_constraints,
        MaybeExistConstraints = no_exist_constraints
    ;
        MaybeExistConstraints0 = exist_constraints(ExistConstraints0),
        ExistConstraints0 = cons_exist_constraints(ExistQVars, Constraints0,
            UnconstrainedExistQTVars, ConstrainedExistQTVars),
        replace_in_prog_constraints_location(TypeEqvMap, MaybeRecord,
            Constraints0, Constraints,
            !TVarSet, !ItemRecompDeps, !UsedModules),
        ExistConstraints = cons_exist_constraints(ExistQVars, Constraints,
            UnconstrainedExistQTVars, ConstrainedExistQTVars),
        MaybeExistConstraints = exist_constraints(ExistConstraints)
    ),
    Ctor = ctor(Ordinal, MaybeExistConstraints, CtorName, CtorArgs, Arity,
        Ctxt).

%---------------------%

:- pred replace_in_ctor_arg_list(type_eqv_map::in,
    maybe_record_sym_name_use::in,
    list(constructor_arg)::in, list(constructor_arg)::out,
    circ_types::out, tvarset::in, tvarset::out,
    item_recomp_deps::in, item_recomp_deps::out,
    used_modules::in, used_modules::out) is det.

replace_in_ctor_arg_list(TypeEqvMap, MaybeRecord, !CtorArgs,
        ContainsCirc, !TVarSet, !ItemRecompDeps, !UsedModules) :-
    replace_in_ctor_arg_list_loop(TypeEqvMap, MaybeRecord, [], !CtorArgs,
        set.init, ContainsCirc, !TVarSet, !ItemRecompDeps, !UsedModules).

:- pred replace_in_ctor_arg_list_loop(type_eqv_map::in,
    maybe_record_sym_name_use::in, list(type_ctor)::in,
    list(constructor_arg)::in, list(constructor_arg)::out,
    circ_types::in, circ_types::out, tvarset::in, tvarset::out,
    item_recomp_deps::in, item_recomp_deps::out,
    used_modules::in, used_modules::out) is det.

replace_in_ctor_arg_list_loop(_TypeEqvMap, _MaybeRecord, _Seen, [], [],
        !Circ, !TVarSet, !ItemRecompDeps, !UsedModules).
replace_in_ctor_arg_list_loop(TypeEqvMap, MaybeRecord, Seen,
        [CtorArg0 | CtorArgs0], [CtorArg | CtorArgs],
        !Circ, !TVarSet, !ItemRecompDeps, !UsedModules) :-
    CtorArg0 = ctor_arg(Name, Type0, Context),
    replace_in_type_maybe_record_use(TypeEqvMap, MaybeRecord, Seen,
        Type0, Type, _, TypeCirc, !TVarSet, !ItemRecompDeps, !UsedModules),
    CtorArg = ctor_arg(Name, Type, Context),
    set.union(TypeCirc, !Circ),
    replace_in_ctor_arg_list_loop(TypeEqvMap, MaybeRecord, Seen,
        CtorArgs0, CtorArgs, !Circ, !TVarSet, !ItemRecompDeps, !UsedModules).

%---------------------%

:- pred replace_in_type_defn_solver(equiv_params::in,
    maybe_record_sym_name_use::in, type_ctor::in, prog_context::in,
    type_details_solver::in, type_details_solver::out,
    tvarset::in, tvarset::out, item_recomp_deps::in, item_recomp_deps::out,
    used_modules::in, used_modules::out, list(error_spec)::out) is det.

replace_in_type_defn_solver(Params, MaybeRecord, TypeCtor, Context,
        DetailsSolver0, DetailsSolver,
        !TVarSet, !ItemRecompDeps, !UsedModules, Specs) :-
    DetailsSolver0 = type_details_solver(SolverDetails0, MaybeUserEqComp),
    SolverDetails0 = solver_type_details(RepresentationType0,
        GroundInst, AnyInst, MutableInfos0),
    TypeEqvMap = Params ^ ep_type_eqv_map,
    replace_in_type_maybe_record_use(TypeEqvMap, MaybeRecord, [TypeCtor],
        RepresentationType0, RepresentationType,
        _Changed, Circ, !TVarSet, !ItemRecompDeps, !UsedModules),
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
    replace_in_constraint_store(Params, MaybeRecord,
        MutableInfos0, MutableInfos, !ItemRecompDeps, !UsedModules),
    SolverDetails = solver_type_details(RepresentationType,
        GroundInst, AnyInst, MutableInfos),
    DetailsSolver = type_details_solver(SolverDetails, MaybeUserEqComp).

:- pred replace_in_constraint_store(equiv_params::in,
    maybe_record_sym_name_use::in,
    list(item_mutable_info)::in, list(item_mutable_info)::out,
    item_recomp_deps::in, item_recomp_deps::out,
    used_modules::in, used_modules::out) is det.

replace_in_constraint_store(_, _, [], [], !ItemRecompDeps, !UsedModules).
replace_in_constraint_store(Params, MaybeRecord,
        [MutableInfo0 | MutableInfos0], [MutableInfo | MutableInfos],
        !ItemRecompDeps, !UsedModules) :-
    replace_in_mutable_defn(Params, MaybeRecord,
        MutableInfo0, MutableInfo, !ItemRecompDeps, !UsedModules),
    replace_in_constraint_store(Params, MaybeRecord,
        MutableInfos0, MutableInfos, !ItemRecompDeps, !UsedModules).

%---------------------------------------------------------------------------%

:- pred replace_in_pred_decl_info(equiv_params::in,
    maybe_record_sym_name_use::in,
    item_pred_decl_info::in, item_pred_decl_info::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out, list(error_spec)::out) is det.

replace_in_pred_decl_info(Params, MaybeRecord,
        PredDeclInfo0, PredDeclInfo, !RecompInfo, !UsedModules, Specs) :-
    PredDeclInfo0 = item_pred_decl_info(PredSymName, PredOrFunc,
        TypesAndMaybeModes0, MaybeWithType0, MaybeWithInst0, MaybeDetism0,
        Origin, TVarSet0, InstVarSet, ExistQVars, Purity, ClassContext0,
        Context, SeqNum),
    ModuleName = Params ^ ep_module_name,
    % We do not yet know the item_id.
    maybe_start_gathering_item_recomp_deps_sym_name(ModuleName, PredSymName,
        !.RecompInfo, ItemRecompDeps0),
    replace_in_pred_types_and_maybe_modes(Params, MaybeRecord,
        PredSymName, PredOrFunc, Context, ClassContext0, ClassContext,
        TypesAndMaybeModes0, TypesAndMaybeModes, TVarSet0, TVarSet,
        MaybeWithType0, MaybeWithType, MaybeWithInst0, MaybeWithInst,
        MaybeDetism0, MaybeDetism, ItemRecompDeps0, ItemRecompDeps,
        !UsedModules, Specs),
    ItemType = pred_or_func_to_recomp_item_type(PredOrFunc),
    % NOTE TypesAndMaybeModes is NOT guaranteed to have the same arity
    % as TypesAndMaybeModes0, since replace_in_pred_types_and_maybe_modes
    % can add additional arguments from with_type annotations.
    PredFormArity = types_and_maybe_modes_arity(TypesAndMaybeModes),
    user_arity_pred_form_arity(PredOrFunc, user_arity(Arity), PredFormArity),
    ItemName = recomp_item_name(PredSymName, Arity),
    ItemId = recomp_item_id(ItemType, ItemName),
    finish_gathering_item_recomp_deps(ItemId, ItemRecompDeps, !RecompInfo),
    PredDeclInfo = item_pred_decl_info(PredSymName, PredOrFunc,
        TypesAndMaybeModes, MaybeWithType, MaybeWithInst, MaybeDetism,
        Origin, TVarSet, InstVarSet, ExistQVars, Purity, ClassContext,
        Context, SeqNum).

%---------------------%

:- pred replace_in_mode_decl_info(equiv_params::in,
    maybe_record_sym_name_use::in,
    item_mode_decl_info::in, item_mode_decl_info::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out, list(error_spec)::out) is det.

replace_in_mode_decl_info(Params, MaybeRecord,
        Info0, Info, !RecompInfo, !UsedModules, Specs) :-
    Info0 = item_mode_decl_info(PredSymName, MaybePredOrFunc0, Modes0,
        WithInst0, MaybeDetism0, InstVarSet, Context, SeqNum),
    ModuleName = Params ^ ep_module_name,
    % We do not yet know the item_id.
    maybe_start_gathering_item_recomp_deps_sym_name(ModuleName, PredSymName,
        !.RecompInfo, ItemRecompDeps0),
    PredFormArity = arg_list_arity(Modes0),
    replace_in_with_inst(Params, MaybeRecord, PredSymName, PredFormArity,
        Context, mode_decl, MaybePredOrFunc0, MaybePredOrFunc,
        WithInst0, WithInst, ExtraModes, MaybeDetism0, MaybeDetism,
        ItemRecompDeps0, ItemRecompDeps, !UsedModules, Specs),
    (
        ExtraModes = [],
        Modes = Modes0
    ;
        ExtraModes = [_ | _],
        Modes = Modes0 ++ ExtraModes
    ),
    (
        MaybePredOrFunc = yes(PredOrFunc),
        % NOTE Obviously from the code above, Modes is NOT guaranteed
        % to have the same arity as Modes, because ExtraModes can come from
        % with_inst annotations.
        ItemType = pred_or_func_to_recomp_item_type(PredOrFunc),
        list.length(Modes, Arity),
        adjust_func_arity(PredOrFunc, OrigArity, Arity),
        ItemName = recomp_item_name(PredSymName, OrigArity),
        ItemId = recomp_item_id(ItemType, ItemName),
        finish_gathering_item_recomp_deps(ItemId, ItemRecompDeps, !RecompInfo)
    ;
        MaybePredOrFunc = no
        % Since neither the mode declaration nor replace_in_with_inst
        % has provided a valid PredOrFunc value, the mode declaration
        % is invalid, which means that smart recompilation can do
        % nothing useful about it. We therefore ignore it by leaving
        % !RecompInfo unchanged.
    ),
    Info = item_mode_decl_info(PredSymName, MaybePredOrFunc, Modes,
        WithInst, MaybeDetism, InstVarSet, Context, SeqNum).

%---------------------------------------------------------------------------%
%
% The next two predicates have identical definitions except for the treatment
% of the class interfaces, but one is for item_typeclass_infos, while the other
% is for item_abstract_typeclass_infos.
% XXX Ideally, this should not be necessary.
%

:- pred replace_in_typeclass_info(equiv_params::in,
    maybe_record_sym_name_use::in,
    item_typeclass_info::in, item_typeclass_info::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out, list(error_spec)::out) is det.

replace_in_typeclass_info(Params, MaybeRecord, TypeClassInfo0, TypeClassInfo,
        !RecompInfo, !UsedModules, Specs) :-
    TypeClassInfo0 = item_typeclass_info(ClassName, Vars, Constraints0,
        FunDeps, ClassInterface0, TVarSet0, Context, SeqNum),
    ModuleName = Params ^ ep_module_name,
    list.length(Vars, Arity),
    ItemName = recomp_item_name(ClassName, Arity),
    ItemId = recomp_item_id(recomp_typeclass, ItemName),
    maybe_start_gathering_item_recomp_deps(ModuleName, ItemId, !.RecompInfo,
        ItemRecompDeps0),
    TypeEqvMap = Params ^ ep_type_eqv_map,
    replace_in_prog_constraints_location(TypeEqvMap, MaybeRecord,
        Constraints0, Constraints, TVarSet0, TVarSet,
        ItemRecompDeps0, ItemRecompDeps1, !UsedModules),
    (
        ClassInterface0 = class_interface_abstract,
        ClassInterface = class_interface_abstract,
        ItemRecompDeps = ItemRecompDeps1,
        Specs = []
    ;
        ClassInterface0 = class_interface_concrete(Methods0),
        replace_in_class_interface(Params, MaybeRecord, Methods0, Methods,
            ItemRecompDeps1, ItemRecompDeps, !UsedModules, [], Specs),
        ClassInterface = class_interface_concrete(Methods)
    ),
    finish_gathering_item_recomp_deps(ItemId, ItemRecompDeps, !RecompInfo),
    TypeClassInfo = item_typeclass_info(ClassName, Vars, Constraints,
        FunDeps, ClassInterface, TVarSet, Context, SeqNum).

:- pred replace_in_abstract_typeclass_info(equiv_params::in,
    maybe_record_sym_name_use::in,
    item_abstract_typeclass_info::in, item_abstract_typeclass_info::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out, list(error_spec)::out) is det.

replace_in_abstract_typeclass_info(Params, MaybeRecord,
        TypeClassInfo0, TypeClassInfo, !RecompInfo, !UsedModules, Specs) :-
    TypeClassInfo0 = item_typeclass_info(ClassName, Vars, Constraints0,
        FunDeps, ClassInterface, TVarSet0, Context, SeqNum),
    ModuleName = Params ^ ep_module_name,
    list.length(Vars, Arity),
    ItemName = recomp_item_name(ClassName, Arity),
    ItemId = recomp_item_id(recomp_typeclass, ItemName),
    maybe_start_gathering_item_recomp_deps(ModuleName, ItemId, !.RecompInfo,
        ItemRecompDeps0),
    TypeEqvMap = Params ^ ep_type_eqv_map,
    replace_in_prog_constraints_location(TypeEqvMap, MaybeRecord,
        Constraints0, Constraints, TVarSet0, TVarSet,
        ItemRecompDeps0, ItemRecompDeps, !UsedModules),
    finish_gathering_item_recomp_deps(ItemId, ItemRecompDeps, !RecompInfo),
    TypeClassInfo = item_typeclass_info(ClassName, Vars, Constraints,
        FunDeps, ClassInterface, TVarSet, Context, SeqNum),
    Specs = [].

%---------------------------------------------------------------------------%

:- pred replace_in_class_interface(equiv_params::in,
    maybe_record_sym_name_use::in,
    list(class_decl)::in, list(class_decl)::out,
    item_recomp_deps::in, item_recomp_deps::out,
    used_modules::in, used_modules::out,
    list(error_spec)::in, list(error_spec)::out) is det.

replace_in_class_interface(Params, MaybeRecord,
        ClassInterface0, ClassInterface, !ItemRecompDeps, !UsedModules,
        !Specs) :-
    list.map_foldl3(
        replace_in_class_decl(Params, MaybeRecord),
        ClassInterface0, ClassInterface,
        !ItemRecompDeps, !UsedModules, !Specs).

:- pred replace_in_class_decl(equiv_params::in, maybe_record_sym_name_use::in,
    class_decl::in, class_decl::out,
    item_recomp_deps::in, item_recomp_deps::out,
    used_modules::in, used_modules::out,
    list(error_spec)::in, list(error_spec)::out) is det.

replace_in_class_decl(Params, MaybeRecord, Decl0, Decl,
        !ItemRecompDeps, !UsedModules, !Specs) :-
    (
        Decl0 = class_decl_pred_or_func(PredOrFuncInfo0),
        PredOrFuncInfo0 = class_pred_or_func_info(PredName, PredOrFunc,
            TypesAndModes0, WithType0, WithInst0, MaybeDetism0,
            TVarSet0, InstVarSet, ExistQVars, Purity,
            ClassContext0, Context),
        replace_in_pred_types_and_maybe_modes(Params, MaybeRecord,
            PredName, PredOrFunc, Context,
            ClassContext0, ClassContext, TypesAndModes0, TypesAndModes,
            TVarSet0, TVarSet, WithType0, WithType, WithInst0, WithInst,
            MaybeDetism0, MaybeDetism, !ItemRecompDeps, !UsedModules,
            NewSpecs),
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
        replace_in_with_inst(Params, MaybeRecord,
            PredName, PredFormArity, Context, mode_decl,
            MaybePredOrFunc0, MaybePredOrFunc, WithInst0, WithInst, ExtraModes,
            MaybeDetism0, MaybeDetism, !ItemRecompDeps, !UsedModules,
            NewSpecs),
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
%
% The next two predicates have identical definitions except for the treatment
% of the instance bodies, but one is for item_instance_infos, while the other
% is for item_abstract_instance_infos.
% XXX Ideally, this should not be necessary.
%

:- pred replace_in_instance_info(equiv_params::in,
    maybe_record_sym_name_use::in,
    item_instance_info::in, item_instance_info::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out, list(error_spec)::out) is det.

replace_in_instance_info(Params, MaybeRecord, InstanceInfo0, InstanceInfo,
        !RecompInfo, !UsedModules, []) :-
    InstanceInfo0 = item_instance_info(ClassName, Types0, OriginalTypes,
        Constraints0, InstanceBody0, TVarSet0, ContainingModuleName,
        Context, SeqNum),
    ModuleName = Params ^ ep_module_name,
    ( if
        ( !.RecompInfo = no
        ; ContainingModuleName = ModuleName
        )
    then
        ItemRecompDeps0 = no_item_recomp_deps
    else
        ItemRecompDeps0 = item_recomp_deps(ModuleName, set.init)
    ),
    TypeEqvMap = Params ^ ep_type_eqv_map,
    replace_in_prog_constraints_location(TypeEqvMap, MaybeRecord,
        Constraints0, Constraints, TVarSet0, TVarSet1,
        ItemRecompDeps0, ItemRecompDeps1, !UsedModules),
    replace_in_type_list_location_circ(TypeEqvMap, MaybeRecord, Types0, Types,
        _, _, TVarSet1, TVarSet, ItemRecompDeps1, ItemRecompDeps,
        !UsedModules),
    (
        InstanceBody0 = instance_body_abstract,
        InstanceBody = instance_body_abstract
    ;
        InstanceBody0 = instance_body_concrete(_InstanceMethods0),
        InstanceBody = InstanceBody0
% We don't yet have code to expand out type equivalences in explicit
% type qualifications in clauses.
%
%       replace_in_list(Params, MaybeRecord, TypeEqvMap, InstEqvMap,
%           replace_in_instance_method, InstanceMethods0, InstanceMethods,
%           !RecompInfo, !UsedModules, [], Specs),
%       InstanceBody = instance_body_concrete(InstanceMethods)
    ),
    % We specifically do NOT expand equivalence types in OriginalTypes.
    % If we did, that would defeat the purpose of the field.
    ItemName = recomp_item_name(ClassName, list.length(Types0)),
    ItemId = recomp_item_id(recomp_typeclass, ItemName),
    finish_gathering_item_recomp_deps(ItemId, ItemRecompDeps, !RecompInfo),
    InstanceInfo = item_instance_info(ClassName, Types, OriginalTypes,
        Constraints, InstanceBody, TVarSet, ContainingModuleName,
        Context, SeqNum).

:- pred replace_in_abstract_instance_info(equiv_params::in,
    maybe_record_sym_name_use::in,
    item_abstract_instance_info::in, item_abstract_instance_info::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out, list(error_spec)::out) is det.

replace_in_abstract_instance_info(Params, MaybeRecord,
        InstanceInfo0, InstanceInfo, !RecompInfo, !UsedModules, []) :-
    InstanceInfo0 = item_instance_info(ClassName, Types0, OriginalTypes,
        Constraints0, InstanceBody, TVarSet0, ContainingModuleName,
        Context, SeqNum),
    ModuleName = Params ^ ep_module_name,
    ( if
        ( !.RecompInfo = no
        ; ContainingModuleName = ModuleName
        )
    then
        ItemRecompDeps0 = no_item_recomp_deps
    else
        ItemRecompDeps0 = item_recomp_deps(ModuleName, set.init)
    ),
    TypeEqvMap = Params ^ ep_type_eqv_map,
    replace_in_prog_constraints_location(TypeEqvMap, MaybeRecord,
        Constraints0, Constraints, TVarSet0, TVarSet1,
        ItemRecompDeps0, ItemRecompDeps1, !UsedModules),
    replace_in_type_list_location_circ(TypeEqvMap, MaybeRecord, Types0, Types,
        _, _, TVarSet1, TVarSet, ItemRecompDeps1, ItemRecompDeps,
        !UsedModules),
    % We specifically do NOT expand equivalence types in OriginalTypes.
    % If we did, that would defeat the purpose of the field.
    ItemName = recomp_item_name(ClassName, list.length(Types0)),
    ItemId = recomp_item_id(recomp_typeclass, ItemName),
    finish_gathering_item_recomp_deps(ItemId, ItemRecompDeps, !RecompInfo),
    InstanceInfo = item_instance_info(ClassName, Types, OriginalTypes,
        Constraints, InstanceBody, TVarSet, ContainingModuleName,
        Context, SeqNum).

%---------------------------------------------------------------------------%

% We don't yet have code to expand out type equivalences in explicit
% type qualifications in clauses.
%
% :- pred replace_in_instance_method(equiv_params::in,
%   maybe_record_sym_name_use::in,
%   instance_method::in, instance_method::out,
%   maybe(recompilation_info)::in, maybe(recompilation_info)::out,
%   used_modules::in, used_modules::out, list(error_spec)::out) is det.

% replace_in_instance_method(Params, MaybeRecord,
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

%---------------------------------------------------------------------------%

:- pred replace_in_decl_pragma_info(equiv_params::in,
    maybe_record_sym_name_use::in,
    item_decl_pragma_info::in, item_decl_pragma_info::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out, list(error_spec)::out) is det.

replace_in_decl_pragma_info(Params, MaybeRecord, DeclPragma0, DeclPragma,
        !RecompInfo, !UsedModules, Specs) :-
    (
        DeclPragma0 = decl_pragma_type_spec_constr(TypeSpecConstr0),
        replace_in_decl_pragma_type_spec_constr(Params, MaybeRecord,
            TypeSpecConstr0, TypeSpecConstr, !UsedModules, Specs),
        DeclPragma = decl_pragma_type_spec_constr(TypeSpecConstr)
    ;
        DeclPragma0 = decl_pragma_type_spec(TypeSpec0),
        replace_in_decl_pragma_type_spec(Params, MaybeRecord,
            TypeSpec0, TypeSpec, !RecompInfo, !UsedModules, Specs),
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

:- pred replace_in_decl_pragma_type_spec_constr(equiv_params::in,
    maybe_record_sym_name_use::in,
    decl_pragma_type_spec_constr_info::in,
        decl_pragma_type_spec_constr_info::out,
    used_modules::in, used_modules::out, list(error_spec)::out) is det.

replace_in_decl_pragma_type_spec_constr(Params, MaybeRecord,
        TypeSpecInfoConstr0, TypeSpecInfoConstr, !UsedModules, []) :-
    TypeSpecInfoConstr0 = decl_pragma_type_spec_constr_info(PragmaModuleName,
        OoMConstraints0, ApplyToSupers, OoMSubsts0, TVarSet0, ItemIds0,
        Context, SeqNum),
    % XXX I (zs) don't understand the purpose of the test in the code that
    % sets ItemRecompDeps0 in replace_in_decl_pragma_type_spec below.
    % The commit that added that code (the commit by Simon that added
    % the initial implementation of smart recompilation) does not mention
    % any rationale either. I cannot copy that test since there is no PredName
    % here. So this setting of ItemRecompDeps0 here is just a guess. Whether
    % it is a correct guess or not will matter only once smart recompilation
    % is completed, in the fullness of time.
    OoMConstraints0 = one_or_more(HeadConstraint0, TailConstraints0),
    ModuleName = Params ^ ep_module_name,
    ItemRecompDeps0 = item_recomp_deps(ModuleName, ItemIds0),
    replace_in_var_or_ground_constraint_location(TypeEqvMap, MaybeRecord,
        HeadConstraint0, HeadConstraint, TVarSet0, TVarSet1,
        ItemRecompDeps0, ItemRecompDeps1, !UsedModules),
    TypeEqvMap = Params ^ ep_type_eqv_map,
    list.map_foldl3(
        replace_in_var_or_ground_constraint_location(TypeEqvMap, MaybeRecord),
        TailConstraints0, TailConstraints, TVarSet1, TVarSet2,
        ItemRecompDeps1, ItemRecompDeps2, !UsedModules),
    OoMConstraints = one_or_more(HeadConstraint, TailConstraints),
    OoMSubsts0 = one_or_more(HeadSubst0, TailSubsts0),
    replace_in_subst(TypeEqvMap, MaybeRecord,
        HeadSubst0, HeadSubst, TVarSet2, TVarSet3,
        ItemRecompDeps2, ItemRecompDeps3, !UsedModules),
    list.map_foldl3(replace_in_subst(TypeEqvMap, MaybeRecord),
        TailSubsts0, TailSubsts, TVarSet3, TVarSet,
        ItemRecompDeps3, ItemRecompDeps, !UsedModules),
    OoMSubsts = one_or_more(HeadSubst, TailSubsts),
    (
        ItemRecompDeps = no_item_recomp_deps,
        ItemIds = ItemIds0
    ;
        ItemRecompDeps = item_recomp_deps(_, ItemIds)
    ),
    TypeSpecInfoConstr = decl_pragma_type_spec_constr_info(PragmaModuleName,
        OoMConstraints, ApplyToSupers, OoMSubsts, TVarSet, ItemIds,
        Context, SeqNum).

:- pred replace_in_decl_pragma_type_spec(equiv_params::in,
    maybe_record_sym_name_use::in,
    decl_pragma_type_spec_info::in, decl_pragma_type_spec_info::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out, list(error_spec)::out) is det.

replace_in_decl_pragma_type_spec(Params, MaybeRecord,
        TypeSpecInfo0, TypeSpecInfo,
        RecompInfo, RecompInfo, !UsedModules, []) :-
    % Unlike the code handling most other items, we do not directly
    % update RecompInfo. Instead, we store the information used for
    % that update in the modified TypeSpecInfo we return, as the
    % ItemIds field. The code of add_pragma_type_spec.m will then
    % do the update of RecompInfo, *provided* that the type spec pragma
    % has no errors.
    %
    % Note that we must nevertheless return a RecompInfo, since this is
    % required by the interface of replace_in_list.
    TypeSpecInfo0 = decl_pragma_type_spec_info(PFUMM, PredName, NewName,
        Subst0, TVarSet0, ItemIds0, Context, SeqNum),
    ModuleName = Params ^ ep_module_name,
    ( if
        ( RecompInfo = no
        ; PredName = qualified(ModuleName, _)
        )
    then
        ItemRecompDeps0 = no_item_recomp_deps
    else
        ItemRecompDeps0 = item_recomp_deps(ModuleName, ItemIds0)
    ),
    TypeEqvMap = Params ^ ep_type_eqv_map,
    replace_in_subst(TypeEqvMap, MaybeRecord, Subst0, Subst,
        TVarSet0, TVarSet, ItemRecompDeps0, ItemRecompDeps, !UsedModules),
    (
        ItemRecompDeps = no_item_recomp_deps,
        % The field in TypeSpecInfo0 holding ItemIds0 wasd initialized
        % to the empty set, and ItemIds0 should still be empty.
        ItemIds = ItemIds0
    ;
        ItemRecompDeps = item_recomp_deps(_, ItemIds)
    ),
    TypeSpecInfo = decl_pragma_type_spec_info(PFUMM, PredName, NewName,
        Subst, TVarSet, ItemIds, Context, SeqNum).

%---------------------%

:- pred replace_in_subst(type_eqv_map::in, maybe_record_sym_name_use::in,
    type_subst::in, type_subst::out,
    tvarset::in, tvarset::out, item_recomp_deps::in, item_recomp_deps::out,
    used_modules::in, used_modules::out) is det.

replace_in_subst(TypeEqvMap, MaybeRecord, Subst0, Subst,
        !TVarSet, !ItemRecompDeps, !UsedModules) :-
    Subst0 = one_or_more(HeadSubst0, TailSubsts0),
    replace_in_tvar_substs(TypeEqvMap, MaybeRecord,
        HeadSubst0, HeadSubst, TailSubsts0, TailSubsts,
        !TVarSet, !ItemRecompDeps, !UsedModules),
    Subst = one_or_more(HeadSubst, TailSubsts).

:- pred replace_in_tvar_substs(type_eqv_map::in, maybe_record_sym_name_use::in,
    tvar_subst::in, tvar_subst::out,
    list(tvar_subst)::in, list(tvar_subst)::out,
    tvarset::in, tvarset::out, item_recomp_deps::in, item_recomp_deps::out,
    used_modules::in, used_modules::out) is det.

replace_in_tvar_substs(TypeEqvMap, MaybeRecord, Subst0, Subst,
        TailVarsTypes0, TailVarsTypes,
        !TVarSet, !ItemRecompDeps, !UsedModules) :-
    Subst0 = tvar_subst(HeadVar, HeadType0),
    replace_in_type_maybe_record_use_ignore_circ(TypeEqvMap, MaybeRecord,
        HeadType0, HeadType, _, !TVarSet, !ItemRecompDeps, !UsedModules),
    (
        TailVarsTypes0 = [],
        TailVarsTypes = []
    ;
        TailVarsTypes0 = [HeadTailVarType0 | TailTailVarsTypes0],
        replace_in_tvar_substs(TypeEqvMap, MaybeRecord,
            HeadTailVarType0, HeadTailVarType,
            TailTailVarsTypes0, TailTailVarsTypes,
            !TVarSet, !ItemRecompDeps, !UsedModules),
        TailVarsTypes = [HeadTailVarType | TailTailVarsTypes]
    ),
    Subst = tvar_subst(HeadVar, HeadType).

%---------------------%

:- pred replace_in_var_or_ground_constraint_location(type_eqv_map::in,
    maybe_record_sym_name_use::in,
    var_or_ground_constraint::in, var_or_ground_constraint::out,
    tvarset::in, tvarset::out, item_recomp_deps::in, item_recomp_deps::out,
    used_modules::in, used_modules::out) is det.

replace_in_var_or_ground_constraint_location(TypeEqvMap, MaybeRecord,
        Constraint0, Constraint, !TVarSet, !ItemRecompDeps, !UsedModules) :-
    Constraint0 =
        var_or_ground_constraint(ClassName, ConstraintArgs0, Context),
    list.map_foldl3(
        replace_in_var_or_ground_type_location(TypeEqvMap, MaybeRecord),
        ConstraintArgs0, ConstraintArgs,
        !TVarSet, !ItemRecompDeps, !UsedModules),
    Constraint = var_or_ground_constraint(ClassName, ConstraintArgs, Context).

:- pred replace_in_var_or_ground_type_location(type_eqv_map::in,
    maybe_record_sym_name_use::in,
    var_or_ground_type::in, var_or_ground_type::out,
    tvarset::in, tvarset::out, item_recomp_deps::in, item_recomp_deps::out,
    used_modules::in, used_modules::out) is det.

replace_in_var_or_ground_type_location(TypeEqvMap, MaybeRecord,
        ConstraintArg0, ConstraintArg,
        !TVarSet, !ItemRecompDeps, !UsedModules) :-
    (
        ConstraintArg0 = type_var_name(_, _),
        ConstraintArg = ConstraintArg0
    ;
        ConstraintArg0 = ground_type(GroundType0),
        Type0 = coerce(GroundType0),
        replace_in_type_maybe_record_use_ignore_circ(TypeEqvMap, MaybeRecord,
            Type0, Type, _, !TVarSet, !ItemRecompDeps, !UsedModules),
        ( if type_is_ground(Type, GroundType) then
            ConstraintArg = ground_type(GroundType)
        else
            unexpected($pred, "expanded ground type is not ground")
        )
    ).

%---------------------------------------------------------------------------%

:- pred replace_in_foreign_proc(equiv_params::in,
    maybe_record_sym_name_use::in,
    item_foreign_proc_info::in, item_foreign_proc_info::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out, list(error_spec)::out) is det.

replace_in_foreign_proc(Params, MaybeRecord, FPInfo0, FPInfo,
        !RecompInfo, !UsedModules, []) :-
    FPInfo0 = item_foreign_proc_info(Attrs0, PredName, PredOrFunc,
        ProcVars, ProcVarset, ProcInstVarset, ProcImpl, Context, SeqNum),
    ModuleName = Params ^ ep_module_name,
    ItemName = recomp_item_name(PredName, list.length(ProcVars)),
    ItemId = recomp_item_id(recomp_foreign_proc, ItemName),
    maybe_start_gathering_item_recomp_deps(ModuleName, ItemId,
        !.RecompInfo, ItemRecompDeps0),
    UserSharing0 = get_user_annotated_sharing(Attrs0),
    ( if
        UserSharing0 = user_sharing(Sharing0, MaybeTypes0),
        MaybeTypes0 = yes(user_type_info(Types0, TVarSet0))
    then
        TypeEqvMap = Params ^ ep_type_eqv_map,
        replace_in_type_list_location(TypeEqvMap, MaybeRecord,
            Types0, Types, _AnythingChanged,
            TVarSet0, TVarSet, ItemRecompDeps0, ItemRecompDeps1, !UsedModules),
        replace_in_structure_sharing_domain(TypeEqvMap, MaybeRecord, TVarSet0,
            Sharing0, Sharing, ItemRecompDeps1, ItemRecompDeps, !UsedModules),
        MaybeTypes = yes(user_type_info(Types, TVarSet)),
        UserSharing = user_sharing(Sharing, MaybeTypes),
        set_user_annotated_sharing(UserSharing, Attrs0, Attrs)
    else
        Attrs = Attrs0,
        ItemRecompDeps = ItemRecompDeps0
    ),
    finish_gathering_item_recomp_deps(ItemId, ItemRecompDeps, !RecompInfo),
    FPInfo = item_foreign_proc_info(Attrs, PredName, PredOrFunc,
        ProcVars, ProcVarset, ProcInstVarset, ProcImpl, Context, SeqNum).

%---------------------%

:- pred replace_in_structure_sharing_domain(type_eqv_map::in,
    maybe_record_sym_name_use::in, tvarset::in,
    structure_sharing_domain::in, structure_sharing_domain::out,
    item_recomp_deps::in, item_recomp_deps::out,
    used_modules::in, used_modules::out) is det.

replace_in_structure_sharing_domain(TypeEqvMap, MaybeRecord, TVarSet,
        SharingDomain0, SharingDomain, !ItemRecompDeps, !UsedModules) :-
    (
        ( SharingDomain0 = structure_sharing_bottom
        ; SharingDomain0 = structure_sharing_top(_)
        ),
        SharingDomain = SharingDomain0
    ;
        SharingDomain0 = structure_sharing_real(SharingPairs0),
        list.map_foldl2(
            replace_in_structure_sharing_pair(TypeEqvMap, MaybeRecord,
                TVarSet),
            SharingPairs0, SharingPairs, !ItemRecompDeps, !UsedModules),
        SharingDomain = structure_sharing_real(SharingPairs)
    ).

:- pred replace_in_structure_sharing_pair(type_eqv_map::in,
    maybe_record_sym_name_use::in, tvarset::in,
    structure_sharing_pair::in, structure_sharing_pair::out,
    item_recomp_deps::in, item_recomp_deps::out,
    used_modules::in, used_modules::out) is det.

replace_in_structure_sharing_pair(TypeEqvMap, MaybeRecord, TVarSet,
        SSA0 - SSB0, SSA - SSB, !ItemRecompDeps, !UsedModules) :-
    replace_in_datastruct(TypeEqvMap, MaybeRecord, TVarSet, SSA0, SSA,
        !ItemRecompDeps, !UsedModules),
    replace_in_datastruct(TypeEqvMap, MaybeRecord, TVarSet, SSB0, SSB,
        !ItemRecompDeps, !UsedModules).

:- pred replace_in_datastruct(type_eqv_map::in, maybe_record_sym_name_use::in,
    tvarset::in, datastruct::in,
    datastruct::out, item_recomp_deps::in, item_recomp_deps::out,
    used_modules::in, used_modules::out) is det.

replace_in_datastruct(TypeEqvMap, MaybeRecord, TVarSet, DS0, DS,
        !ItemRecompDeps, !UsedModules) :-
    DS0 = selected_cel(Var, Sel0),
    list.map_foldl2(replace_in_unit_selector(TypeEqvMap, MaybeRecord, TVarSet),
        Sel0, Sel, !ItemRecompDeps, !UsedModules),
    DS = selected_cel(Var, Sel).

:- pred replace_in_unit_selector(type_eqv_map::in,
    maybe_record_sym_name_use::in, tvarset::in, unit_selector::in,
    unit_selector::out, item_recomp_deps::in, item_recomp_deps::out,
    used_modules::in, used_modules::out) is det.

replace_in_unit_selector(TypeEqvMap, MaybeRecord, TVarSet, Sel0, Sel,
        !ItemRecompDeps, !UsedModules) :-
    (
        Sel0 = termsel(_, _),
        Sel = Sel0
    ;
        Sel0 = typesel(Type0),
        replace_in_type_maybe_record_use_ignore_circ(TypeEqvMap, MaybeRecord,
            Type0, Type, _, TVarSet, _, !ItemRecompDeps, !UsedModules),
        Sel = typesel(Type)
    ).

%---------------------------------------------------------------------------%

:- pred replace_in_mutable_info(equiv_params::in,
    maybe_record_sym_name_use::in,
    item_mutable_info::in, item_mutable_info::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out, list(error_spec)::out) is det.

replace_in_mutable_info(Params, MaybeRecord,
        MutableInfo0, MutableInfo, !RecompInfo, !UsedModules, []) :-
    MutName = MutableInfo0 ^ mut_name,
    ModuleName = Params ^ ep_module_name,
    QualName = qualified(ModuleName, MutName),
    ItemId = recomp_item_id(recomp_mutable, recomp_item_name(QualName, 0)),
    maybe_start_gathering_item_recomp_deps(ModuleName, ItemId, !.RecompInfo,
        ItemRecompDeps0),
    replace_in_mutable_defn(Params, MaybeRecord, MutableInfo0, MutableInfo,
        ItemRecompDeps0, ItemRecompDeps, !UsedModules),
    finish_gathering_item_recomp_deps(ItemId, ItemRecompDeps, !RecompInfo).

:- pred replace_in_mutable_defn(equiv_params::in,
    maybe_record_sym_name_use::in,
    item_mutable_info::in, item_mutable_info::out,
    item_recomp_deps::in, item_recomp_deps::out,
    used_modules::in, used_modules::out) is det.

replace_in_mutable_defn(Params, MaybeRecord, MutableInfo0, MutableInfo,
        !ItemRecompDeps, !UsedModules) :-
    MutableInfo0 = item_mutable_info(MutName, OrigType, Type0, OrigInst, Inst0,
        InitValue, Attrs, Varset, Context, SeqNum),
    TypeEqvMap = Params ^ ep_type_eqv_map,
    TVarSet0 = varset.init,
    replace_in_type_maybe_record_use_ignore_circ(TypeEqvMap, MaybeRecord,
        Type0, Type, _TypeChanged, TVarSet0, _TVarSet,
        !ItemRecompDeps, !UsedModules),
    replace_in_inst(Params, MaybeRecord, Inst0, Inst,
        !ItemRecompDeps, !UsedModules),
    MutableInfo = item_mutable_info(MutName, OrigType, Type, OrigInst, Inst,
        InitValue, Attrs, Varset, Context, SeqNum).

%---------------------------------------------------------------------------%

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
    replace_in_type_maybe_record_use_ignore_circ(TypeEqvMap,
        do_not_record_sym_name_use, AttrType0, AttrType, _Changed,
        TVarSet0, _TVarSet, no_item_recomp_deps, _ItemRecompDeps,
        !UsedModules),
    Attr = event_attribute(AttrNum, AttrName, AttrType, AttrMode,
        MaybeSynthCall).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

replace_in_univ_exist_constraints(TypeEqvMap, Cs0, Cs,
        !TVarSet, !ItemRecompDeps) :-
    replace_in_univ_exist_constraints_location(TypeEqvMap,
        do_not_record_sym_name_use, Cs0, Cs,
        !TVarSet, !ItemRecompDeps, used_modules_init, _).

:- pred replace_in_univ_exist_constraints_location(type_eqv_map::in,
    maybe_record_sym_name_use::in,
    univ_exist_constraints::in, univ_exist_constraints::out,
    tvarset::in, tvarset::out, item_recomp_deps::in, item_recomp_deps::out,
    used_modules::in, used_modules::out) is det.

replace_in_univ_exist_constraints_location(TypeEqvMap, MaybeRecord, Cs0, Cs,
        !TVarSet, !ItemRecompDeps, !UsedModules) :-
    Cs0 = univ_exist_constraints(UnivCs0, ExistCs0),
    replace_in_prog_constraints_location(TypeEqvMap, MaybeRecord,
        UnivCs0, UnivCs, !TVarSet, !ItemRecompDeps, !UsedModules),
    replace_in_prog_constraints_location(TypeEqvMap, MaybeRecord,
        ExistCs0, ExistCs, !TVarSet, !ItemRecompDeps, !UsedModules),
    Cs = univ_exist_constraints(UnivCs, ExistCs).

replace_in_prog_constraints(TypeEqvMap,
        !Constraints, !TVarSet, !ItemRecompDeps) :-
    replace_in_prog_constraints_location(TypeEqvMap,
        do_not_record_sym_name_use, !Constraints,
        !TVarSet, !ItemRecompDeps, used_modules_init, _).

:- pred replace_in_prog_constraints_location(type_eqv_map::in,
    maybe_record_sym_name_use::in,
    list(prog_constraint)::in, list(prog_constraint)::out,
    tvarset::in, tvarset::out, item_recomp_deps::in, item_recomp_deps::out,
    used_modules::in, used_modules::out) is det.

replace_in_prog_constraints_location(TypeEqvMap, MaybeRecord,
        !Constraints, !TVarSet, !ItemRecompDeps, !UsedModules) :-
    list.map_foldl3(
        replace_in_prog_constraint_location(TypeEqvMap, MaybeRecord),
        !Constraints, !TVarSet, !ItemRecompDeps, !UsedModules).

:- pred replace_in_prog_constraint_location(type_eqv_map::in,
    maybe_record_sym_name_use::in, prog_constraint::in, prog_constraint::out,
    tvarset::in, tvarset::out, item_recomp_deps::in, item_recomp_deps::out,
    used_modules::in, used_modules::out) is det.

replace_in_prog_constraint_location(TypeEqvMap, MaybeRecord,
        Constraint0, Constraint, !TVarSet, !ItemRecompDeps, !UsedModules) :-
    Constraint0 = constraint(ClassName, ArgTypes0),
    replace_in_type_list_location_circ(TypeEqvMap, MaybeRecord,
        ArgTypes0, ArgTypes, _, _, !TVarSet, !ItemRecompDeps, !UsedModules),
    Constraint = constraint(ClassName, ArgTypes).

%---------------------------------------------------------------------------%

:- pred replace_in_pred_types_and_maybe_modes(equiv_params::in,
    maybe_record_sym_name_use::in,
    sym_name::in, pred_or_func::in, prog_context::in,
    univ_exist_constraints::in, univ_exist_constraints::out,
    types_and_maybe_modes::in, types_and_maybe_modes::out,
    tvarset::in, tvarset::out,
    maybe(mer_type)::in, maybe(mer_type)::out,
    maybe(mer_inst)::in, maybe(mer_inst)::out,
    maybe(determinism)::in, maybe(determinism)::out,
    item_recomp_deps::in, item_recomp_deps::out,
    used_modules::in, used_modules::out, list(error_spec)::out) is det.

replace_in_pred_types_and_maybe_modes(Params, MaybeRecord,
        PredName, PredOrFunc, Context, ClassContext0, ClassContext,
        TypesAndMaybeModes0, TypesAndMaybeModes, !TVarSet,
        MaybeWithType0, MaybeWithType, MaybeWithInst0, MaybeWithInst,
        !MaybeDetism, !ItemRecompDeps, !UsedModules, !:Specs) :-
    TypeEqvMap = Params ^ ep_type_eqv_map,
    replace_in_univ_exist_constraints_location(TypeEqvMap, MaybeRecord,
        ClassContext0, ClassContext, !TVarSet, !ItemRecompDeps, !UsedModules),
    replace_in_types_and_maybe_modes(TypeEqvMap, MaybeRecord,
        TypesAndMaybeModes0, TypesAndMaybeModes1,
        !TVarSet, !ItemRecompDeps, !UsedModules),
    (
        MaybeWithType0 = yes(WithType0),
        replace_in_type_maybe_record_use_ignore_circ(TypeEqvMap, MaybeRecord,
            WithType0, WithType, _, !TVarSet, !ItemRecompDeps, !UsedModules),
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
    replace_in_with_inst(Params, MaybeRecord, PredName, PredFormArity,
        Context, type_decl, yes(PredOrFunc), _, MaybeWithInst0, _, ExtraModes,
        !MaybeDetism, !ItemRecompDeps, !UsedModules, ModeSpecs),
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
        gather_item_recomp_dep(OrigItemId, !ItemRecompDeps)
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

:- pred replace_in_with_inst(equiv_params::in,
    maybe_record_sym_name_use::in,
    sym_name::in, pred_form_arity::in, prog_context::in,
    pred_or_func_decl_type::in,
    maybe(pred_or_func)::in, maybe(pred_or_func)::out,
    maybe(mer_inst)::in, maybe(mer_inst)::out, list(mer_mode)::out,
    maybe(determinism)::in, maybe(determinism)::out,
    item_recomp_deps::in, item_recomp_deps::out,
    used_modules::in, used_modules::out, list(error_spec)::out) is det.

replace_in_with_inst(Params, MaybeRecord, PredName, PredFormArity, Context,
        DeclType, MaybePredOrFunc0, MaybePredOrFunc,
        MaybeWithInst0, MaybeWithInst, ExtraModes,
        !MaybeDetism, !ItemRecompDeps, !UsedModules, Specs) :-
    (
        MaybeWithInst0 = yes(WithInst0),
        replace_in_inst(Params, MaybeRecord, WithInst0, WithInst,
            !ItemRecompDeps, !UsedModules),
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
            gather_item_recomp_dep(OrigItemId, !ItemRecompDeps),
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

:- pred replace_in_types_and_maybe_modes(type_eqv_map::in,
    maybe_record_sym_name_use::in,
    types_and_maybe_modes::in, types_and_maybe_modes::out,
    tvarset::in, tvarset::out, item_recomp_deps::in, item_recomp_deps::out,
    used_modules::in, used_modules::out) is det.

replace_in_types_and_maybe_modes(MaybeRecord, TypeEqvMap,
        !TypeAndMaybeModes, !TVarSet, !ItemRecompDeps, !UsedModules) :-
    (
        !.TypeAndMaybeModes = no_types_arity_zero
    ;
        !.TypeAndMaybeModes = types_only(Types0),
        list.map2_foldl3(
            replace_in_type_maybe_record_use_ignore_circ(MaybeRecord,
                TypeEqvMap),
            Types0, Types, _, !TVarSet, !ItemRecompDeps, !UsedModules),
        !:TypeAndMaybeModes = types_only(Types)
    ;

        !.TypeAndMaybeModes = types_and_modes(TypesAndModes0),
        list.map_foldl3(
            replace_in_type_and_mode(MaybeRecord, TypeEqvMap),
            TypesAndModes0, TypesAndModes,
            !TVarSet, !ItemRecompDeps, !UsedModules),
        !:TypeAndMaybeModes = types_and_modes(TypesAndModes)
    ).

:- pred replace_in_type_and_mode(type_eqv_map::in,
    maybe_record_sym_name_use::in, type_and_mode::in, type_and_mode::out,
    tvarset::in, tvarset::out, item_recomp_deps::in, item_recomp_deps::out,
    used_modules::in, used_modules::out) is det.

replace_in_type_and_mode(TypeEqvMap, MaybeRecord, TypeAndMode0, TypeAndMode,
        !TVarSet, !ItemRecompDeps, !UsedModules) :-
    TypeAndMode0 = type_and_mode(Type0, Mode),
    replace_in_type_maybe_record_use_ignore_circ(TypeEqvMap, MaybeRecord,
        Type0, Type, _, !TVarSet, !ItemRecompDeps, !UsedModules),
    TypeAndMode = type_and_mode(Type, Mode).

%---------------------------------------------------------------------------%

replace_in_type_list(TypeEqvMap, !Types, Changed, !TVarSet, !ItemRecompDeps) :-
    replace_in_type_list_location(TypeEqvMap, do_not_record_sym_name_use,
        !Types, Changed, !TVarSet, !ItemRecompDeps, used_modules_init, _).

:- pred replace_in_type_list_location(type_eqv_map::in,
    maybe_record_sym_name_use::in,
    list(mer_type)::in, list(mer_type)::out,
    maybe_changed::out, tvarset::in, tvarset::out,
    item_recomp_deps::in, item_recomp_deps::out,
    used_modules::in, used_modules::out) is det.

replace_in_type_list_location(TypeEqvMap, MaybeRecord, !Types,
        Changed, !TVarSet, !ItemRecompDeps, !UsedModules) :-
    replace_in_type_list_location_circ(TypeEqvMap, MaybeRecord, !Types,
        Changed, _, !TVarSet, !ItemRecompDeps, !UsedModules).

:- pred replace_in_type_list_location_circ(type_eqv_map::in,
    maybe_record_sym_name_use::in, list(mer_type)::in, list(mer_type)::out,
    maybe_changed::out, circ_types::out, tvarset::in, tvarset::out,
    item_recomp_deps::in, item_recomp_deps::out,
    used_modules::in, used_modules::out) is det.

replace_in_type_list_location_circ(TypeEqvMap, MaybeRecord, !Types,
        Changed, ContainsCirc, !TVarSet, !ItemRecompDeps, !UsedModules) :-
    replace_in_type_list_location_acc_circ(TypeEqvMap, MaybeRecord, [], !Types,
        Changed, set.init, ContainsCirc, !TVarSet,
        !ItemRecompDeps, !UsedModules).

:- pred replace_in_type_list_location_acc_circ(type_eqv_map::in,
    maybe_record_sym_name_use::in, list(type_ctor)::in,
    list(mer_type)::in, list(mer_type)::out,
    maybe_changed::out, circ_types::in, circ_types::out,
    tvarset::in, tvarset::out, item_recomp_deps::in, item_recomp_deps::out,
    used_modules::in, used_modules::out) is det.

replace_in_type_list_location_acc_circ(_TypeEqvMap, _MaybeRecord, _Seen,
        [], [], unchanged, !ContainsCirc, !TVarSet,
        !ItemRecompDeps, !UsedModules).
replace_in_type_list_location_acc_circ(TypeEqvMap, MaybeRecord, Seen,
        Types0 @ [HeadType0 | TailTypes0], Types, Changed, !Circ, !TVarSet,
        !ItemRecompDeps, !UsedModules) :-
    replace_in_type_maybe_record_use(TypeEqvMap, MaybeRecord, Seen,
        HeadType0, HeadType, HeadChanged, HeadCirc, !TVarSet,
        !ItemRecompDeps, !UsedModules),
    set.union(HeadCirc, !Circ),
    replace_in_type_list_location_acc_circ(TypeEqvMap, MaybeRecord, Seen,
        TailTypes0, TailTypes, TailChanged, !Circ, !TVarSet,
        !ItemRecompDeps, !UsedModules),
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

replace_in_type_report_circular_eqvs(TypeEqvMap, TVarSet0, Context,
        Type0, Type, Changed, !Specs) :-
    replace_in_type_maybe_record_use(TypeEqvMap, do_not_record_sym_name_use,
        [], Type0, Type, Changed, Circ,
        TVarSet0, _TVarSet, no_item_recomp_deps, _, used_modules_init, _),
    set.to_sorted_list(Circ, CircTypes),
    (
        CircTypes = [HeadCircTypeCtor | TailCircTypeCtors],
        Spec = report_contains_circular_eqv_type(TVarSet0, Type0, Context,
            HeadCircTypeCtor, TailCircTypeCtors),
        !:Specs = [Spec | !.Specs]
    ;
        CircTypes = []
    ).

replace_in_type(TypeEqvMap, Type0, Type, Changed, !TVarSet, !ItemRecompDeps) :-
    replace_in_type_maybe_record_use(TypeEqvMap, do_not_record_sym_name_use,
        [], Type0, Type, Changed, _Circ, !TVarSet,
        !ItemRecompDeps, used_modules_init, _).

:- pred replace_in_type_maybe_record_use_ignore_circ(type_eqv_map::in,
    maybe_record_sym_name_use::in,
    mer_type::in, mer_type::out, maybe_changed::out,
    tvarset::in, tvarset::out, item_recomp_deps::in, item_recomp_deps::out,
    used_modules::in, used_modules::out) is det.

replace_in_type_maybe_record_use_ignore_circ(TypeEqvMap, MaybeRecord,
        Type0, Type, Changed, !TVarSet, !ItemRecompDeps, !UsedModules) :-
    replace_in_type_maybe_record_use(TypeEqvMap, MaybeRecord, [],
        Type0, Type, Changed, _, !TVarSet, !ItemRecompDeps, !UsedModules).

    % Replace all equivalence types in a given type, detecting
    % any circularities.
    %
:- pred replace_in_type_maybe_record_use(type_eqv_map::in,
    maybe_record_sym_name_use::in, list(type_ctor)::in,
    mer_type::in, mer_type::out,
    maybe_changed::out, circ_types::out, tvarset::in, tvarset::out,
    item_recomp_deps::in, item_recomp_deps::out,
    used_modules::in, used_modules::out) is det.

replace_in_type_maybe_record_use(TypeEqvMap, MaybeRecord,
        TypeCtorsAlreadyExpanded, Type0, Type, Changed, Circ,
        !TVarSet, !ItemRecompDeps, !UsedModules) :-
    (
        Type0 = type_variable(Var, Kind),
        Type = type_variable(Var, Kind),
        Changed = unchanged,
        Circ = set.init
    ;
        Type0 = defined_type(SymName, ArgTypes0, Kind),
        replace_in_type_list_location_acc_circ(TypeEqvMap, MaybeRecord,
            TypeCtorsAlreadyExpanded, ArgTypes0, ArgTypes, ArgTypesChanged,
            set.init, Circ0, !TVarSet, !ItemRecompDeps, !UsedModules),
        Arity = list.length(ArgTypes),
        TypeCtor = type_ctor(SymName, Arity),
        replace_type_ctor(TypeEqvMap, MaybeRecord, TypeCtorsAlreadyExpanded,
            Type0, TypeCtor, ArgTypes, Kind, Type, ArgTypesChanged, Changed,
            Circ0, Circ, !TVarSet, !ItemRecompDeps, !UsedModules)
    ;
        Type0 = builtin_type(_),
        Type = Type0,
        Changed = unchanged,
        Circ = set.init
    ;
        Type0 = higher_order_type(PorF, HOArgTypes0, HOInstInfo, Purity),
        replace_in_type_list_location_acc_circ(TypeEqvMap, MaybeRecord,
            TypeCtorsAlreadyExpanded, HOArgTypes0, HOArgTypes, Changed,
            set.init, Circ, !TVarSet, !ItemRecompDeps, !UsedModules),
        (
            Changed = changed,
            Type = higher_order_type(PorF, HOArgTypes, HOInstInfo, Purity)
        ;
            Changed = unchanged,
            Type = Type0
        )
    ;
        Type0 = tuple_type(TupleArgTypes0, Kind),
        replace_in_type_list_location_acc_circ(TypeEqvMap, MaybeRecord,
            TypeCtorsAlreadyExpanded, TupleArgTypes0, TupleArgTypes, Changed,
            set.init, Circ, !TVarSet, !ItemRecompDeps, !UsedModules),
        (
            Changed = changed,
            Type = tuple_type(TupleArgTypes, Kind)
        ;
            Changed = unchanged,
            Type = Type0
        )
    ;
        Type0 = apply_n_type(Var, ApplyArgTypes0, Kind),
        replace_in_type_list_location_acc_circ(TypeEqvMap, MaybeRecord,
            TypeCtorsAlreadyExpanded, ApplyArgTypes0, ApplyArgTypes, Changed,
            set.init, Circ, !TVarSet, !ItemRecompDeps, !UsedModules),
        (
            Changed = changed,
            Type = apply_n_type(Var, ApplyArgTypes, Kind)
        ;
            Changed = unchanged,
            Type = Type0
        )
    ;
        Type0 = kinded_type(RawType0, Kind),
        replace_in_type_maybe_record_use(TypeEqvMap, MaybeRecord,
            TypeCtorsAlreadyExpanded, RawType0, RawType, Changed, Circ,
            !TVarSet, !ItemRecompDeps, !UsedModules),
        (
            Changed = changed,
            Type = kinded_type(RawType, Kind)
        ;
            Changed = unchanged,
            Type = Type0
        )
    ).

:- pred replace_type_ctor(type_eqv_map::in, maybe_record_sym_name_use::in,
    list(type_ctor)::in, mer_type::in, type_ctor::in, list(mer_type)::in,
    kind::in, mer_type::out, maybe_changed::in, maybe_changed::out,
    circ_types::in, circ_types::out, tvarset::in, tvarset::out,
    item_recomp_deps::in, item_recomp_deps::out,
    used_modules::in, used_modules::out) is det.

replace_type_ctor(TypeEqvMap, MaybeRecord, TypeCtorsAlreadyExpanded, Type0,
        TypeCtor, ArgTypes, Kind, Type, !Changed, !Circ, !TVarSet,
        !ItemRecompDeps, !UsedModules) :-
    ( if list.member(TypeCtor, TypeCtorsAlreadyExpanded) then
        AlreadyExpanded = yes,
        NewCirc = set.make_singleton_set(TypeCtor)
    else
        AlreadyExpanded = no,
        NewCirc = set.init
    ),
    ( if
        map.search(TypeEqvMap, TypeCtor, EqvTypeBody),
        EqvTypeBody = eqv_type_body(EqvTVarSet, EqvTypeParams0, Body0),

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
        map.apply_to_list(EqvTypeParams0, Renaming, EqvTypeParams),
        apply_renaming_to_type(Renaming, Body0, Body1),
        TypeCtorItem = type_ctor_to_recomp_item_name(TypeCtor),
        gather_item_recomp_dep(recomp_item_id(recomp_type_name, TypeCtorItem),
            !ItemRecompDeps),
        map.from_corresponding_lists(EqvTypeParams, ArgTypes, Subst),
        apply_subst_to_type(Subst, Body1, Body),
        replace_in_type_maybe_record_use(TypeEqvMap, MaybeRecord,
            [TypeCtor | TypeCtorsAlreadyExpanded], Body,
            Type, _, !:Circ, !TVarSet, !ItemRecompDeps, !UsedModules)
    else
        (
            !.Changed = changed,
            TypeCtor = type_ctor(SymName, _Arity),
            Type = defined_type(SymName, ArgTypes, Kind)
        ;
            !.Changed = unchanged,
            Type = Type0
        ),
        set.union(NewCirc, !Circ)
    ).

%---------------------------------------------------------------------------%

:- pred replace_in_inst(equiv_params::in, maybe_record_sym_name_use::in,
    mer_inst::in, mer_inst::out, item_recomp_deps::in, item_recomp_deps::out,
    used_modules::in, used_modules::out) is det.

replace_in_inst(Params, MaybeRecord, Inst0, Inst,
        !ItemRecompDeps, !UsedModules) :-
    InstEqvMap = Params ^ ep_inst_eqv_map,
    replace_in_inst_location(InstEqvMap, MaybeRecord, set.init, Inst0, Inst,
        !ItemRecompDeps, !UsedModules).

:- pred replace_in_inst_location(inst_eqv_map::in,
    maybe_record_sym_name_use::in, set(inst_ctor)::in,
    mer_inst::in, mer_inst::out,
    item_recomp_deps::in, item_recomp_deps::out,
    used_modules::in, used_modules::out) is det.

replace_in_inst_location(InstEqvMap, MaybeRecord,
        ExpandedInstCtors0, Inst0, Inst, !ItemRecompDeps, !UsedModules) :-
    % XXX Need to record the used modules
    ( if Inst0 = defined_inst(user_inst(SymName, ArgInsts)) then
        InstCtor = inst_ctor(SymName, length(ArgInsts)),
        ( if
            set.member(InstCtor, ExpandedInstCtors0)
        then
            Inst = Inst0
        else if
            map.search(InstEqvMap, InstCtor, EqvInstBody),
            EqvInstBody = eqv_inst_body(EqvInstParams, EqvInst)
        then
            inst_substitute_arg_list(EqvInstParams, ArgInsts, EqvInst, Inst1),
            InstCtorItem = inst_ctor_to_recomp_item_name(InstCtor),
            gather_item_recomp_dep(recomp_item_id(recomp_inst, InstCtorItem),
                !ItemRecompDeps),
            set.insert(InstCtor, ExpandedInstCtors0, ExpandedInstCtors),
            replace_in_inst_location(InstEqvMap, MaybeRecord,
                ExpandedInstCtors, Inst1, Inst, !ItemRecompDeps, !UsedModules)
        else
            Inst = Inst0
        )
    else
        Inst = Inst0
    ).

%---------------------------------------------------------------------------%
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

:- type maybe_record_sym_name_use
    --->    do_not_record_sym_name_use
    ;       record_sym_name_use(item_visibility).

:- pred replace_in_maybe(equiv_params::in, maybe_record_sym_name_use::in,
    pred(equiv_params, maybe_record_sym_name_use,
        T, T, maybe(recompilation_info), maybe(recompilation_info),
        used_modules, used_modules, list(error_spec))
    :: in(pred(in, in, in, out, in, out, in, out, out) is det),
    maybe(T)::in, maybe(T)::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out,
    list(error_spec)::in, list(error_spec)::out) is det.

replace_in_maybe(Params, MaybeRecord, ReplaceInItem, MaybeItem0, MaybeItem,
        !RecompInfo, !UsedModules, !Specs) :-
    (
        MaybeItem0 = no,
        MaybeItem = no
    ;
        MaybeItem0 = yes(Item0),
        ReplaceInItem(Params, MaybeRecord, Item0, Item,
            !RecompInfo, !UsedModules, ItemSpecs),
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

:- pred replace_in_list(equiv_params::in, maybe_record_sym_name_use::in,
    pred(equiv_params, maybe_record_sym_name_use,
        T, T, maybe(recompilation_info), maybe(recompilation_info),
        used_modules, used_modules, list(error_spec))
    :: in(pred(in, in, in, out, in, out, in, out, out) is det),
    list(T)::in, list(T)::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out,
    list(error_spec)::in, list(error_spec)::out) is det.

replace_in_list(Params, MaybeRecord, ReplaceInItem,
        Items0, Items, !RecompInfo, !UsedModules, !Specs) :-
    replace_in_list_loop(Params, MaybeRecord, ReplaceInItem,
        Items0, [], RevItems, !RecompInfo, !UsedModules, !Specs),
    list.reverse(RevItems, Items).

:- pred replace_in_list_loop(equiv_params::in, maybe_record_sym_name_use::in,
    pred(equiv_params, maybe_record_sym_name_use,
        T, T, maybe(recompilation_info), maybe(recompilation_info),
        used_modules, used_modules, list(error_spec))
    :: in(pred(in, in, in, out, in, out, in, out, out) is det),
    list(T)::in, list(T)::in, list(T)::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out,
    list(error_spec)::in, list(error_spec)::out) is det.

replace_in_list_loop(_Params, _MaybeRecord, _ReplaceInItem,
        [], !RevItems, !RecompInfo, !UsedModules, !Specs).
replace_in_list_loop(Params, MaybeRecord, ReplaceInItem,
        [Item0 | Items0], !RevItems, !RecompInfo, !UsedModules, !Specs) :-
    ReplaceInItem(Params, MaybeRecord,
        Item0, Item, !RecompInfo, !UsedModules, ItemSpecs),
    % Discard the item if there were any errors.
    (
        ItemSpecs = [],
        !:RevItems = [Item | !.RevItems]
    ;
        ItemSpecs = [_ | _],
        !:Specs = ItemSpecs ++ !.Specs
    ),
    replace_in_list_loop(Params, MaybeRecord, ReplaceInItem,
        Items0, !RevItems, !RecompInfo, !UsedModules, !Specs).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

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

%---------------------------------------------------------------------------%
:- end_module parse_tree.equiv_type.
%---------------------------------------------------------------------------%
