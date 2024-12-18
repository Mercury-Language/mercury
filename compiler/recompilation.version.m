%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2001-2012 The University of Melbourne.
% Copyright (C) 2014-2024 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: recompilation_version.m.
% Main author: stayl.
%
% Compute version numbers for program items in interface files.
%
%---------------------------------------------------------------------------%

:- module recompilation.version.
:- interface.

:- import_module libs.
:- import_module libs.timestamp.
:- import_module parse_tree.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.prog_parse_tree.
:- import_module recompilation.item_types.

:- import_module maybe.
:- import_module term.

%---------------------------------------------------------------------------%

    % compute_version_numbers_intN(MaybeOldParseTreeIntN,
    %   CurParseTreeIntNTimeStamp, CurParseTreeIntN, VersionNumbers).
    %
:- pred compute_version_numbers_int0(maybe(parse_tree_int0)::in,
    timestamp::in, parse_tree_int0::in, module_item_version_numbers::out)
    is det.
:- pred compute_version_numbers_int1(maybe(parse_tree_int1)::in,
    timestamp::in, parse_tree_int1::in, module_item_version_numbers::out)
    is det.
:- pred compute_version_numbers_int2(maybe(parse_tree_int2)::in,
    timestamp::in, parse_tree_int2::in, module_item_version_numbers::out)
    is det.

:- func module_item_version_numbers_to_string(module_item_version_numbers)
    = string.

    % The version number for the format of the version numbers
    % written to the interface files.
    %
:- func module_item_version_numbers_version_number = int.

    % Parse a term that maps item ids to timestamps. These terms
    % look like this:
    %
    % {
    %     type(
    %         state_mc/0 - "2015-10-16 08:51:02",
    %         state_no/0 - "2015-10-16 08:51:02",
    %         transition/0 - "2015-10-16 08:51:02",
    %         transitions/0 - "2015-10-16 08:51:02"
    %     ),
    %     type_body(
    %         state_mc/0 - "2015-10-16 08:51:02",
    %         state_no/0 - "2015-10-16 08:51:02",
    %         transition/0 - "2015-10-16 08:51:02",
    %         transitions/0 - "2015-10-16 08:51:02"
    %     ),
    %     inst(
    %         atom_transition/0 - "2015-10-16 08:51:02",
    %         atom_transitions/0 - "2015-10-16 08:51:02",
    %         null_transition/0 - "2015-10-16 08:51:02",
    %         null_transition_free_state_mc/0 - "2015-10-16 08:51:02",
    %         null_transitions/0 - "2015-10-16 08:51:02"
    %     )
    % }
    %
:- pred parse_module_item_version_numbers(term::in,
    maybe1(module_item_version_numbers)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.maybe_util.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.convert_parse_tree.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.item_util.
:- import_module parse_tree.parse_sym_name.
:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.parse_tree_out_sym_name.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.parse_tree_to_term.
:- import_module parse_tree.parse_util.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_pragma.
:- import_module parse_tree.prog_item.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_subst.
:- import_module parse_tree.prog_type_unify.
:- import_module parse_tree.prog_util.

:- import_module assoc_list.
:- import_module cord.
:- import_module list.
:- import_module map.
:- import_module multi_map.
:- import_module one_or_more.
:- import_module pair.
:- import_module require.
:- import_module string.
:- import_module term_context.
:- import_module term_int.
:- import_module term_subst.
:- import_module term_unify.
:- import_module varset.

%---------------------------------------------------------------------------%

compute_version_numbers_int0(MaybeOldParseTreeInt0,
        CurFileTime, CurParseTreeInt0, NewVersionNumbers) :-
    gather_items_in_parse_tree_int0(CurParseTreeInt0, CurGatherResults),
    ( if
        MaybeOldParseTreeInt0 = yes(OldParseTreeInt0),
        OldParseTreeInt0 ^ pti0_maybe_version_numbers
            = version_numbers(OldVersionNumbers)
    then
        gather_items_in_parse_tree_int0(OldParseTreeInt0, OldGatherResults),
        MaybeOldVersionNumbersGatherResults =
            yes({OldVersionNumbers, OldGatherResults})
    else
        MaybeOldVersionNumbersGatherResults = no
    ),
    update_version_numbers(MaybeOldVersionNumbersGatherResults,
        CurFileTime, CurGatherResults, NewVersionNumbers).

compute_version_numbers_int1(MaybeOldParseTreeInt1,
        CurFileTime, CurParseTreeInt1, NewVersionNumbers) :-
    gather_items_in_parse_tree_int1(CurParseTreeInt1, CurGatherResults),
    ( if
        MaybeOldParseTreeInt1 = yes(OldParseTreeInt1),
        OldParseTreeInt1 ^ pti1_maybe_version_numbers
            = version_numbers(OldVersionNumbers)
    then
        gather_items_in_parse_tree_int1(OldParseTreeInt1, OldGatherResults),
        MaybeOldVersionNumbersGatherResults =
            yes({OldVersionNumbers, OldGatherResults})
    else
        MaybeOldVersionNumbersGatherResults = no
    ),
    update_version_numbers(MaybeOldVersionNumbersGatherResults,
        CurFileTime, CurGatherResults, NewVersionNumbers).

compute_version_numbers_int2(MaybeOldParseTreeInt2,
        CurFileTime, CurParseTreeInt2, NewVersionNumbers) :-
    gather_items_in_parse_tree_int2(CurParseTreeInt2, CurGatherResults),
    ( if
        MaybeOldParseTreeInt2 = yes(OldParseTreeInt2),
        OldParseTreeInt2 ^ pti2_maybe_version_numbers
            = version_numbers(OldVersionNumbers)
    then
        gather_items_in_parse_tree_int2(OldParseTreeInt2, OldGatherResults),
        MaybeOldVersionNumbersGatherResults =
            yes({OldVersionNumbers, OldGatherResults})
    else
        MaybeOldVersionNumbersGatherResults = no
    ),
    update_version_numbers(MaybeOldVersionNumbersGatherResults,
        CurFileTime, CurGatherResults, NewVersionNumbers).

%---------------------%

:- pred update_version_numbers(
    maybe({module_item_version_numbers, gathered_items})::in,
    timestamp::in, gathered_items::in,
    module_item_version_numbers::out) is det.

update_version_numbers(MaybeOldVersionNumbersGatherResults,
        CurSourceFileTime, CurGatheredItems, NewVersionNumbers) :-
    (
        MaybeOldVersionNumbersGatherResults =
            yes({OldVersionNumbers, OldGatheredItems})
    ;
        MaybeOldVersionNumbersGatherResults = no,
        % There were no old version numbers, so every item gets
        % the same timestamp as the source module.
        % XXX ITEM_LIST In which case, the call to compute_item_version_numbers
        % below is mostly a waste of time, since we could get the same job done
        % more quickly without doing a lot of lookups in empty maps.
        OldVersionNumbers = module_item_version_numbers(map.init, map.init,
            map.init, map.init, map.init, map.init, map.init, map.init),
        OldGatheredItems = gathered_items(map.init, map.init,
            map.init, map.init, map.init, map.init, map.init, map.init)
    ),
    compute_item_version_numbers(CurSourceFileTime,
        OldGatheredItems, CurGatheredItems,
        OldVersionNumbers, NewVersionNumbers).

%---------------------%

:- pred compute_item_version_numbers(timestamp::in,
    gathered_items::in, gathered_items::in,
    module_item_version_numbers::in, module_item_version_numbers::out) is det.

compute_item_version_numbers(SourceFileTime,
        OldGatheredItems, CurGatheredItems,
        OldVersionNumbers, NewVersionNumbers) :-
    OldGatheredItems = gathered_items(OldTypeMap, OldTBodyMap,
        OldInstMap, OldModeMap, OldClassMap, OldInstanceMap,
        OldPredMap, OldFuncMap),
    CurGatheredItems = gathered_items(CurTypeMap, CurTBodyMap,
        CurInstMap, CurModeMap, CurClassMap, CurInstanceMap,
        CurPredMap, CurFuncMap),
    OldVersionNumbers = module_item_version_numbers(OldTypeVMap, OldTBodyVMap,
        OldInstVMap, OldModeVMap, OldClassVMap, OldInstanceVMap,
        OldPredVMap, OldFuncVMap),

    compute_name_arity_version_map(SourceFileTime,
        OldTypeMap, OldTypeVMap, CurTypeMap, NewTypeVMap),
    compute_name_arity_version_map(SourceFileTime,
        OldTBodyMap, OldTBodyVMap, CurTBodyMap, NewTBodyVMap),
    compute_name_arity_version_map(SourceFileTime,
        OldInstMap, OldInstVMap, CurInstMap, NewInstVMap),
    compute_name_arity_version_map(SourceFileTime,
        OldModeMap, OldModeVMap, CurModeMap, NewModeVMap),
    compute_name_arity_version_map(SourceFileTime,
        OldClassMap, OldClassVMap, CurClassMap, NewClassVMap),
    compute_item_name_version_map(SourceFileTime,
        OldInstanceMap, OldInstanceVMap, CurInstanceMap, NewInstanceVMap),
    compute_name_arity_version_map(SourceFileTime,
        OldPredMap, OldPredVMap, CurPredMap, NewPredVMap),
    compute_name_arity_version_map(SourceFileTime,
        OldFuncMap, OldFuncVMap, CurFuncMap, NewFuncVMap),

    NewVersionNumbers = module_item_version_numbers(NewTypeVMap, NewTBodyVMap,
        NewInstVMap, NewModeVMap, NewClassVMap, NewInstanceVMap,
        NewPredVMap, NewFuncVMap).

:- pred compute_name_arity_version_map(timestamp::in,
    gathered_item_multi_map_na::in, name_arity_version_map::in,
    gathered_item_multi_map_na::in, name_arity_version_map::out) is det.

compute_name_arity_version_map(SourceFileTime,
        OldGatheredMap, OldVersionMap, CurGatheredMap, NewVersionMap) :-
    map.map_values(
        compute_name_arity_version_map_entry(SourceFileTime,
            OldGatheredMap, OldVersionMap),
        CurGatheredMap, NewVersionMap).

:- pred compute_name_arity_version_map_entry(timestamp::in,
    gathered_item_multi_map_na::in, name_arity_version_map::in,
    name_arity::in, assoc_list(module_section, item)::in,
    version_number::out) is det.

compute_name_arity_version_map_entry(SourceFileTime,
        OldGatheredMap, OldVersionMap, NameArity, CurItems, TimeStamp) :-
    ( if
        map.search(OldGatheredMap, NameArity, OldItems),
        % We call order_items on the items in both the interface and the
        % implementation of the current parse tree, but doing the same for
        % the previous parse tree would be overkill. However, for some "item"
        % types, such as predicate_item, OldItems and CurItems may contain
        % more than one prog_item.item. We don't want this artificially-created
        % difference in the ORDER of those items to count as a difference
        % that requires a recompilation.
        list.sort(OldItems, SortedOldItems),
        list.sort(CurItems, SortedCurItems),
        are_items_changed(SortedOldItems, SortedCurItems, unchanged),
        map.search(OldVersionMap, NameArity, OldVersionNumber)
    then
        TimeStamp = OldVersionNumber
    else
        TimeStamp = SourceFileTime
    ).

:- pred compute_item_name_version_map(timestamp::in,
    gathered_item_multi_map_in::in, recomp_item_name_version_map::in,
    gathered_item_multi_map_in::in, recomp_item_name_version_map::out) is det.

compute_item_name_version_map(SourceFileTime,
        OldGatheredMap, OldVersionMap, CurGatheredMap, NewVersionMap) :-
    map.map_values(
        compute_item_name_version_map_entry(SourceFileTime,
            OldGatheredMap, OldVersionMap),
        CurGatheredMap, NewVersionMap).

:- pred compute_item_name_version_map_entry(timestamp::in,
    gathered_item_multi_map_in::in, recomp_item_name_version_map::in,
    recomp_item_name::in, assoc_list(module_section, item)::in,
    version_number::out) is det.

compute_item_name_version_map_entry(SourceFileTime,
        OldGatheredMap, OldVersionMap, ItemName, CurItems, TimeStamp) :-
    ( if
        map.search(OldGatheredMap, ItemName, OldItems),
        % We call order_items on the items in both the interface and the
        % implementation of the current parse tree, but doing the same for
        % the previous parse tree would be overkill. However, for some "item"
        % types, such as predicate_item, OldItems and CurItems may contain
        % more than one prog_item.item. We don't want this artificially-created
        % difference in the ORDER of those items to count as a difference
        % that requires a recompilation.
        list.sort(OldItems, SortedOldItems),
        list.sort(CurItems, SortedCurItems),
        are_items_changed(SortedOldItems, SortedCurItems, unchanged),
        map.search(OldVersionMap, ItemName, OldVersionNumber)
    then
        TimeStamp = OldVersionNumber
    else
        TimeStamp = SourceFileTime
    ).

%---------------------------------------------------------------------------%

:- pred gather_items_in_parse_tree_int0(parse_tree_int0::in,
    gathered_items::out) is det.

gather_items_in_parse_tree_int0(ParseTreeInt0, GatheredItems) :-
     ParseTreeInt0 = parse_tree_int0(_ModuleName, _ModuleNameContext,
        _MaybeVersionNumbers, _InclMap,
        _ImportUseMap, _IntFIMSpecs, _ImpFIMSpecs,
        TypeCtorCheckedMap, InstCtorCheckedMap, ModeCtorCheckedMap,
        IntTypeClasses, IntInstances, IntPredDecls, IntModeDecls,
        IntDeclPragmas, IntDeclMarkers,_IntPromises,
        ImpTypeClasses, ImpInstances, ImpPredDecls, ImpModeDecls,
        ImpDeclPragmas, ImpDeclMarkers, _ImpPromises),
    some [!TypeNameMap, !TypeDefnMap, !InstMap, !ModeMap,
        !ClassMap, !InstanceMap,
        !PredMap, !FuncMap, !DeclPragmaRecords]
    (
        map.init(!:TypeNameMap),
        map.init(!:TypeDefnMap),
        map.init(!:InstMap),
        map.init(!:ModeMap),
        map.init(!:ClassMap),
        map.init(!:InstanceMap),
        map.init(!:PredMap),
        map.init(!:FuncMap),
        !:DeclPragmaRecords = cord.init,

        type_ctor_checked_map_get_src_defns(TypeCtorCheckedMap,
            IntTypeDefns, ImpTypeDefns, _ImpForeignEnums),
        inst_ctor_checked_map_get_src_defns(InstCtorCheckedMap,
            IntInstDefns, ImpInstDefns),
        mode_ctor_checked_map_get_src_defns(ModeCtorCheckedMap,
            IntModeDefns, ImpModeDefns),

        list.foldl2(gather_in_type_defn(ms_interface),
            IntTypeDefns, !TypeNameMap, !TypeDefnMap),
        list.foldl(gather_in_inst_defn(ms_interface),
            IntInstDefns, !InstMap),
        list.foldl(gather_in_mode_defn(ms_interface),
            IntModeDefns, !ModeMap),
        list.foldl(gather_in_typeclass(ms_interface), IntTypeClasses,
            !ClassMap),
        list.foldl(gather_in_instance(ms_interface), coerce(IntInstances),
            !InstanceMap),
        list.foldl2(gather_in_pred_decl(ms_interface), IntPredDecls,
            !PredMap, !FuncMap),
        list.foldl2(gather_in_mode_decl(ms_interface), IntModeDecls,
            !PredMap, !FuncMap),
        list.foldl(gather_in_decl_pragma(ms_interface), IntDeclPragmas,
            !DeclPragmaRecords),
        list.foldl(gather_in_decl_marker(ms_interface), IntDeclMarkers,
            !DeclPragmaRecords),
        % XXX Not gathering promises is a bug.
        list.foldl2(gather_in_type_defn(ms_implementation),
            ImpTypeDefns, !TypeNameMap, !TypeDefnMap),
        list.foldl(gather_in_inst_defn(ms_implementation),
            ImpInstDefns, !InstMap),
        list.foldl(gather_in_mode_defn(ms_implementation),
            ImpModeDefns, !ModeMap),
        list.foldl(gather_in_typeclass(ms_implementation), ImpTypeClasses,
            !ClassMap),
        list.foldl(gather_in_instance(ms_implementation), coerce(ImpInstances),
            !InstanceMap),
        list.foldl2(gather_in_pred_decl(ms_implementation), ImpPredDecls,
            !PredMap, !FuncMap),
        list.foldl2(gather_in_mode_decl(ms_implementation), ImpModeDecls,
            !PredMap, !FuncMap),
        % We gather foreign enum info from the type_ctor_defn_maps.
        list.foldl(gather_in_decl_pragma(ms_implementation), ImpDeclPragmas,
            !DeclPragmaRecords),
        list.foldl(gather_in_decl_marker(ms_implementation), ImpDeclMarkers,
            !DeclPragmaRecords),
        % XXX Not gathering promises is a bug.
        cord.foldl3(apply_decl_pragma_record, !.DeclPragmaRecords,
            !PredMap, !FuncMap, !ClassMap),
        GatheredItems = gathered_items(!.TypeNameMap, !.TypeDefnMap,
            !.InstMap, !.ModeMap, !.ClassMap, !.InstanceMap,
            !.PredMap, !.FuncMap)
    ).

:- pred gather_items_in_parse_tree_int1(parse_tree_int1::in,
    gathered_items::out) is det.

gather_items_in_parse_tree_int1(ParseTreeInt1, GatheredItems) :-
    ParseTreeInt1 = parse_tree_int1(_ModuleName, _ModuleNameContext,
        _MaybeVersionNumbers, _InclMap,
        _ImportUseMap, _IntFIMSpecs, _ImpFIMSpecs,
        TypeCtorCheckedMap, InstCtorCheckedMap, ModeCtorCheckedMap,
        IntTypeClasses, IntInstances, IntPredDecls, IntModeDecls,
        IntDeclPragmas, IntDeclMarkers, _IntPromises, IntTypeRepnMap,
        ImpTypeClasses),
    some [!TypeNameMap, !TypeDefnMap, !InstMap, !ModeMap,
        !ClassMap, !InstanceMap,
        !PredMap, !FuncMap, !DeclPragmaRecords]
    (
        map.init(!:TypeNameMap),
        map.init(!:TypeDefnMap),
        map.init(!:InstMap),
        map.init(!:ModeMap),
        map.init(!:ClassMap),
        map.init(!:InstanceMap),
        map.init(!:PredMap),
        map.init(!:FuncMap),
        !:DeclPragmaRecords = cord.init,

        type_ctor_checked_map_get_src_defns(TypeCtorCheckedMap,
            IntTypeDefns, ImpTypeDefns, _ImpForeignEnums),
        inst_ctor_checked_map_get_src_defns(InstCtorCheckedMap,
            IntInstDefns, ImpInstDefns),
        mode_ctor_checked_map_get_src_defns(ModeCtorCheckedMap,
            IntModeDefns, ImpModeDefns),
        expect(unify(ImpInstDefns, []), $pred, "ImpInstDefns != []"),
        expect(unify(ImpModeDefns, []), $pred, "ImpModeDefns != []"),

        list.foldl2(gather_in_type_defn(ms_interface),
            IntTypeDefns, !TypeNameMap, !TypeDefnMap),
        list.foldl(gather_in_inst_defn(ms_interface),
            IntInstDefns, !InstMap),
        list.foldl(gather_in_mode_defn(ms_interface),
            IntModeDefns, !ModeMap),
        list.foldl(gather_in_typeclass(ms_interface), IntTypeClasses,
            !ClassMap),
        list.foldl(gather_in_instance(ms_interface), coerce(IntInstances),
            !InstanceMap),
        list.foldl2(gather_in_pred_decl(ms_interface), IntPredDecls,
            !PredMap, !FuncMap),
        list.foldl2(gather_in_mode_decl(ms_interface), IntModeDecls,
            !PredMap, !FuncMap),
        list.foldl(gather_in_decl_pragma(ms_interface), IntDeclPragmas,
            !DeclPragmaRecords),
        list.foldl(gather_in_decl_marker(ms_interface), IntDeclMarkers,
            !DeclPragmaRecords),
        % XXX Not gathering promises is a bug.
        list.foldl(gather_in_type_repn(ms_interface),
            type_ctor_repn_map_to_type_repns(IntTypeRepnMap), !TypeDefnMap),
        list.foldl2(gather_in_type_defn(ms_implementation),
            ImpTypeDefns, !TypeNameMap, !TypeDefnMap),
        % We gather foreign enum info from the type_ctor_defn_maps.
        list.foldl(gather_in_typeclass(ms_implementation),
            coerce(ImpTypeClasses), !ClassMap),

        cord.foldl3(apply_decl_pragma_record, !.DeclPragmaRecords,
            !PredMap, !FuncMap, !ClassMap),
        GatheredItems = gathered_items(!.TypeNameMap, !.TypeDefnMap,
            !.InstMap, !.ModeMap, !.ClassMap, !.InstanceMap,
            !.PredMap, !.FuncMap)
    ).

:- pred gather_items_in_parse_tree_int2(parse_tree_int2::in,
    gathered_items::out) is det.

gather_items_in_parse_tree_int2(ParseTreeInt2, GatheredItems) :-
    ParseTreeInt2 = parse_tree_int2(_ModuleName, _ModuleNameContext,
        _MaybeVersionNumbers, _InclMap, _ImportUseMap,
        _IntFIMSpecs, _ImpFIMSpecs,
        TypeCtorCheckedMap, InstCtorCheckedMap, ModeCtorCheckedMap,
        IntTypeClasses, IntInstances, IntTypeRepnMap),
    some [!TypeNameMap, !TypeDefnMap, !InstMap, !ModeMap,
        !ClassMap, !InstanceMap]
    (
        map.init(!:TypeNameMap),
        map.init(!:TypeDefnMap),
        map.init(!:InstMap),
        map.init(!:ModeMap),
        map.init(!:ClassMap),
        map.init(!:InstanceMap),

        type_ctor_checked_map_get_src_defns(TypeCtorCheckedMap,
            IntTypeDefns, ImpTypeDefns, _ImpForeignEnums),
        inst_ctor_checked_map_get_src_defns(InstCtorCheckedMap,
            IntInstDefns, ImpInstDefns),
        mode_ctor_checked_map_get_src_defns(ModeCtorCheckedMap,
            IntModeDefns, ImpModeDefns),
        expect(unify(ImpInstDefns, []), $pred, "ImpInstDefns != []"),
        expect(unify(ImpModeDefns, []), $pred, "ImpModeDefns != []"),

        list.foldl2(gather_in_type_defn(ms_interface),
            IntTypeDefns, !TypeNameMap, !TypeDefnMap),
        list.foldl(gather_in_inst_defn(ms_interface),
            IntInstDefns, !InstMap),
        list.foldl(gather_in_mode_defn(ms_interface),
            IntModeDefns, !ModeMap),
        list.foldl(gather_in_typeclass(ms_interface), IntTypeClasses,
            !ClassMap),
        list.foldl(gather_in_instance(ms_interface), coerce(IntInstances),
            !InstanceMap),
        % XXX Not gathering promises is a bug.
        list.foldl(gather_in_type_repn(ms_interface),
            type_ctor_repn_map_to_type_repns(IntTypeRepnMap), !TypeDefnMap),
        list.foldl2(gather_in_type_defn(ms_implementation),
            ImpTypeDefns, !TypeNameMap, !TypeDefnMap),

        map.init(PredMap0),
        map.init(FuncMap0),
        GatheredItems = gathered_items(!.TypeNameMap, !.TypeDefnMap,
            !.InstMap, !.ModeMap, !.ClassMap, !.InstanceMap,
            PredMap0, FuncMap0)
    ).

%---------------------------------------------------------------------------%

:- type gathered_item_multi_map_na ==
    multi_map(name_arity, pair(module_section, item)).
:- type gathered_item_multi_map_in ==
    multi_map(recomp_item_name, pair(module_section, item)).
    % XXX RECOMP The generic item type here should be replaced with
    % a different (set of) item-kind-specific types for each field.

:- type gathered_items
    --->    gathered_items(
                gi_type_names       :: gathered_item_multi_map_na,
                gi_type_defns       :: gathered_item_multi_map_na,
                gi_modes            :: gathered_item_multi_map_na,
                gi_insts            :: gathered_item_multi_map_na,
                gi_typeclasses      :: gathered_item_multi_map_na,
                gi_instances        :: gathered_item_multi_map_in,
                gi_predicates       :: gathered_item_multi_map_na,
                gi_functions        :: gathered_item_multi_map_na
            ).

:- type decl_pragma_record
    --->    decl_pragma_record(module_section, maybe_pred_or_func_id, item).
            % XXX RECOMP Item here should be a type that can store
            % either an item_decl_pragma_info or an item_decl_marker_info.

%---------------------%

:- pred gather_in_type_defn(module_section::in, item_type_defn_info::in,
    gathered_item_multi_map_na::in, gathered_item_multi_map_na::out,
    gathered_item_multi_map_na::in, gathered_item_multi_map_na::out) is det.

gather_in_type_defn(Section, ItemTypeDefn, !TypeMap, !TypeDefnMap) :-
    ItemTypeDefn = item_type_defn_info(SymName, Params, Body,
        VarSet, Context, SeqNum),
    Item = item_type_defn(ItemTypeDefn),
    (
        ( Body = parse_tree_du_type(_)
        ; Body = parse_tree_sub_type(_)
        ),
        % XXX Does the value of AbstractDetails matter here?
        % XXX TYPE_REPN zs: yes, it should, when it changes in a way that
        % affects decisions about the representations of other types
        % that include the abstract type. That means that *assuming*
        % this value for AbstractDetails is a BUG.
        AbstractDetails = abstract_type_general,
        AbstractBody = parse_tree_abstract_type(AbstractDetails),
        NameItemTypeDefn = item_type_defn_info(SymName, Params, AbstractBody,
            VarSet, Context, SeqNum),
        NameItem = item_type_defn(NameItemTypeDefn),
        BodyItem = Item
    ;
        Body = parse_tree_abstract_type(_),
        NameItem = Item,
        % The body of an abstract type can be recorded as used when
        % generating a call to the automatically generated unification
        % procedure.
        BodyItem = Item
    ;
        Body = parse_tree_eqv_type(_),
        % When we use an equivalence type we always use the body.
        NameItem = Item,
        BodyItem = Item
    ;
        Body = parse_tree_solver_type(_),
        NameItem = Item,
        BodyItem = Item
    ;
        Body = parse_tree_foreign_type(_),
        NameItem = Item,
        BodyItem = Item
    ),
    TypeCtorNA = name_arity(unqualify_name(SymName), list.length(Params)),
    multi_map.add(TypeCtorNA, Section - NameItem, !TypeMap),
    multi_map.add(TypeCtorNA, Section - BodyItem, !TypeDefnMap).

%---------------------%

:- pred gather_in_inst_defn(module_section::in, item_inst_defn_info::in,
    gathered_item_multi_map_na::in, gathered_item_multi_map_na::out) is det.

gather_in_inst_defn(Section, ItemInstDefn, !InstMap) :-
    ItemInstDefn = item_inst_defn_info(SymName, Params, _, _, _, _, _),
    Item = item_inst_defn(ItemInstDefn),
    InstCtorNA = name_arity(unqualify_name(SymName), list.length(Params)),
    multi_map.add(InstCtorNA, Section - Item, !InstMap).

%---------------------%

:- pred gather_in_mode_defn(module_section::in, item_mode_defn_info::in,
    gathered_item_multi_map_na::in, gathered_item_multi_map_na::out) is det.

gather_in_mode_defn(Section, ItemModeDefn, !ModeMap) :-
    ItemModeDefn = item_mode_defn_info(SymName, Params, _, _, _, _),
    Item = item_mode_defn(ItemModeDefn),
    ModeCtorNA = name_arity(unqualify_name(SymName), list.length(Params)),
    multi_map.add(ModeCtorNA, Section - Item, !ModeMap).

%---------------------%

:- pred gather_in_pred_decl(module_section::in, item_pred_decl_info::in,
    gathered_item_multi_map_na::in, gathered_item_multi_map_na::out,
    gathered_item_multi_map_na::in, gathered_item_multi_map_na::out) is det.

gather_in_pred_decl(Section, ItemPredDecl, !PredMap, !FuncMap) :-
    ItemPredDecl = item_pred_decl_info(PredSymName, PredOrFunc,
        TypesAndMaybeModes, WithType, WithInst, MaybeDetism, Origin,
        TypeVarSet, InstVarSet, ExistQVars, Purity, Constraints,
        Context, SeqNum),
    % For predicates or functions defined using `with_type` annotations
    % the arity here won't be correct, but equiv_type.m will record
    % the dependency on the version number with the `incorrect' arity,
    % so this will work.
    %
    % XXX This is an unreliable method of detecting changes.
    % Making it reliable would require at recording for the
    % predicate/function the type of the with_type annotation (if any),
    % which we don't currently do.
    %
    % XXX Likewise for with_inst annotations.
    PredFormArity = types_and_maybe_modes_arity(TypesAndMaybeModes),
    PredFormArity = pred_form_arity(Arity0),
    (
        WithType = no,
        adjust_func_arity(PredOrFunc, Arity, Arity0)
    ;
        WithType = yes(_),
        Arity = Arity0
    ),
    PredNA = name_arity(unqualify_name(PredSymName), Arity),

    % The code that generates interface files in parse_tree_out_pred_decl.m
    % splits combined pred and mode declarations. It does this to allow
    % the interface file to remain unchanged if/when the programmer
    % either does, or undoes, this splitting manually, without making
    % any other changes to the module's interface.
    %
    % The code here has to be prepared to compare the pred_decl/mode_decl pair
    % resulting from such as split against a still combined predmode_decl item
    % in the source file.
    get_declared_types_and_maybe_modes(TypesAndMaybeModes, WithInst,
        MaybeDetism, Types, MaybeModes),
    (
        MaybeModes = yes(Modes),
        TypesWithoutModes = types_only(Types),
        varset.init(EmptyInstVarSet),
        ItemPredOnlyDecl = item_pred_decl_info(PredSymName, PredOrFunc,
            TypesWithoutModes, WithType, no, no, Origin,
            TypeVarSet, EmptyInstVarSet, ExistQVars, Purity, Constraints,
            Context, SeqNum),
        PredOnlyItem = item_pred_decl(ItemPredOnlyDecl),
        (
            WithInst = yes(_),
            % MaybePredOrFunc needs to be `no' here because when the item
            % is read from the interface file, we won't know whether it is
            % a predicate or a function mode.
            MaybePredOrFunc = no
        ;
            WithInst = no,
            MaybePredOrFunc = yes(PredOrFunc)
        ),
        ModeItemModeDecl = item_mode_decl_info(PredSymName, MaybePredOrFunc,
            Modes, WithInst, MaybeDetism, InstVarSet, Context, SeqNum),
        ModeItem = item_mode_decl(ModeItemModeDecl),
        (
            PredOrFunc = pf_predicate,
            multi_map.add(PredNA, Section - PredOnlyItem, !PredMap),
            multi_map.add(PredNA, Section - ModeItem, !PredMap)
        ;
            PredOrFunc = pf_function,
            multi_map.add(PredNA, Section - PredOnlyItem, !FuncMap),
            multi_map.add(PredNA, Section - ModeItem, !FuncMap)
        )
    ;
        MaybeModes = no,
        PredItem = item_pred_decl(ItemPredDecl),
        (
            PredOrFunc = pf_predicate,
            multi_map.add(PredNA, Section - PredItem, !PredMap)
        ;
            PredOrFunc = pf_function,
            multi_map.add(PredNA, Section - PredItem, !FuncMap)
        )
    ).

%---------------------%

:- pred gather_in_mode_decl(module_section::in, item_mode_decl_info::in,
    gathered_item_multi_map_na::in, gathered_item_multi_map_na::out,
    gathered_item_multi_map_na::in, gathered_item_multi_map_na::out) is det.

gather_in_mode_decl(Section, ItemModeDecl, !PredMap, !FuncMap) :-
    % For predicates or functions defined using `with_inst` annotations
    % the pred_or_func and arity here won't be correct, but equiv_type.m
    % will record the dependency on the version number with the `incorrect'
    % pred_or_func and arity, so this will work.
    %
    % XXX See the comment about with_inst in gather_in_pred_decl.
    ItemModeDecl = item_mode_decl_info(SymName, MaybePredOrFunc, ArgModes,
        WithInst, _, _, _, _),
    Item = item_mode_decl(ItemModeDecl),
    ( if
        MaybePredOrFunc = no,
        WithInst = yes(_)
    then
        ModeNA = name_arity(unqualify_name(SymName), list.length(ArgModes)),
        multi_map.add(ModeNA, Section - Item, !PredMap),
        multi_map.add(ModeNA, Section - Item, !FuncMap)
    else
        (
            MaybePredOrFunc = yes(PredOrFunc),
            adjust_func_arity(PredOrFunc, Arity, list.length(ArgModes)),
            ModeNA = name_arity(unqualify_name(SymName), Arity),
            (
                PredOrFunc = pf_predicate,
                multi_map.add(ModeNA, Section - Item, !PredMap)
            ;
                PredOrFunc = pf_function,
                multi_map.add(ModeNA, Section - Item, !FuncMap)
            )
        ;
            MaybePredOrFunc = no
            % We don't have an recomp_item_id, so we cannot gather the item.
            % XXX This *will* lead to missing needed recompilations.
        )
    ).

%---------------------%

:- pred gather_in_typeclass(module_section::in, item_typeclass_info::in,
    gathered_item_multi_map_na::in, gathered_item_multi_map_na::out) is det.

gather_in_typeclass(Section, ItemTypeClass, !TypeClassMap) :-
    ItemTypeClass = item_typeclass_info(ClassName, ClassParams, _, _,
        _, _, _, _),
    ClassNA = name_arity(unqualify_name(ClassName), list.length(ClassParams)),
    Interface = ItemTypeClass ^ tc_class_methods,
    (
        Interface = class_interface_abstract,
        ItemToAdd = item_typeclass(ItemTypeClass)
    ;
        Interface = class_interface_concrete(Decls0),
        % See the comment in gather_in_pred_decl for why we split
        % any combined predmode declarations here.
        DeclsList = list.map(split_class_method_types_and_modes, Decls0),
        list.condense(DeclsList, Decls),
        SplitItemTypeClass = ItemTypeClass ^ tc_class_methods
            := class_interface_concrete(Decls),
        ItemToAdd = item_typeclass(SplitItemTypeClass)
    ),
    multi_map.add(ClassNA, Section - ItemToAdd, !TypeClassMap).

:- func split_class_method_types_and_modes(class_decl) = list(class_decl).

split_class_method_types_and_modes(Decl0) = Decls :-
    % Always strip the context from the item -- this is needed
    % so the items can be easily tested for equality.
    (
        Decl0 = class_decl_pred_or_func(PredOrFuncInfo0),
        PredOrFuncInfo0 = class_pred_or_func_info(SymName, PredOrFunc,
            TypesAndMaybeModes, WithType, WithInst, MaybeDetism,
            TypeVarSet, InstVarSet, ExistQVars, Purity, Constraints, _Context),
        % See the comment above the same call in gather_in_pred_decl
        % for the rationale behind this code.
        get_declared_types_and_maybe_modes(TypesAndMaybeModes, WithInst,
            MaybeDetism, Types, MaybeModes),
        (
            MaybeModes = yes(Modes),
            TypesWithoutModes = types_only(Types),
            (
                WithInst = yes(_),
                % MaybePredOrFunc needs to be `no' here because when the item
                % is read in from the interface file, we won't know whether
                % it is a mode for a predicate or a function.
                MaybePredOrFunc = no
            ;
                WithInst = no,
                MaybePredOrFunc = yes(PredOrFunc)
            ),
            ModeInfo = class_mode_info(SymName, MaybePredOrFunc,
                Modes, WithInst, MaybeDetism, InstVarSet, dummy_context),
            ModeDecl = class_decl_mode(ModeInfo),
            ModeDecls = [ModeDecl]
        ;
            MaybeModes = no,
            TypesWithoutModes = TypesAndMaybeModes,
            ModeDecls = []
        ),
        varset.init(EmptyInstVarSet),
        PredOrFuncInfo = class_pred_or_func_info(SymName, PredOrFunc,
            TypesWithoutModes, WithType, no, no, TypeVarSet, EmptyInstVarSet,
            ExistQVars, Purity, Constraints, dummy_context),
        PredOrFuncDecl = class_decl_pred_or_func(PredOrFuncInfo),
        Decls = [PredOrFuncDecl | ModeDecls]
    ;
        Decl0 = class_decl_mode(ModeInfo0),
        ModeInfo0 = class_mode_info(SymName, MaybePredOrFunc,
            Modes, WithInst, MaybeDetism, InstVarSet, _Context),
        ModeInfo = class_mode_info(SymName, MaybePredOrFunc,
            Modes, WithInst, MaybeDetism, InstVarSet, dummy_context),
        Decl = class_decl_mode(ModeInfo),
        Decls = [Decl]
    ).

%---------------------%

:- pred gather_in_instance(module_section::in, item_instance_info::in,
    gathered_item_multi_map_in::in, gathered_item_multi_map_in::out) is det.

gather_in_instance(Section, ItemInstance, !InstanceMap) :-
    ItemInstance = item_instance_info(ClassName, ClassParams,
        _, _, _, _, _, _, _),
    Item = item_instance(ItemInstance),
    ClassNA = recomp_item_name(ClassName, list.length(ClassParams)),
    multi_map.add(ClassNA, Section - Item, !InstanceMap).

%---------------------%

:- pred gather_in_decl_pragma(module_section::in, item_decl_pragma_info::in,
    cord(decl_pragma_record)::in, cord(decl_pragma_record)::out) is det.

gather_in_decl_pragma(Section, DeclPragma, !DeclPragmas) :-
    gather_decl_pragma_for_what_pf_id(DeclPragma, MaybePredOrFuncId),
    (
        MaybePredOrFuncId = yes(PredOrFuncId),
        Item = item_decl_pragma(DeclPragma),
        Record = decl_pragma_record(Section, PredOrFuncId, Item),
        cord.snoc(Record, !DeclPragmas)
    ;
        MaybePredOrFuncId = no
        % XXX Not doing anything here is probably a bug.
    ).

:- pred gather_in_decl_marker(module_section::in, item_decl_marker_info::in,
    cord(decl_pragma_record)::in, cord(decl_pragma_record)::out) is det.

gather_in_decl_marker(Section, DeclMarker, !DeclPragmas) :-
    gather_decl_marker_for_what_pf_id(DeclMarker, MaybePredOrFuncId),
    (
        MaybePredOrFuncId = yes(PredOrFuncId),
        Item = item_decl_marker(DeclMarker),
        Record = decl_pragma_record(Section, PredOrFuncId, Item),
        cord.snoc(Record, !DeclPragmas)
    ;
        MaybePredOrFuncId = no
        % XXX Not doing anything here is probably a bug.
    ).

%---------------------%

:- pred gather_in_type_repn(module_section::in, item_type_repn_info::in,
    gathered_item_multi_map_na::in, gathered_item_multi_map_na::out) is det.

gather_in_type_repn(Section, ItemTypeRepn, !TypeDefnMap) :-
    ItemTypeRepn = item_type_repn_info(SymName, Params, _, _, _, _),
    Item = item_type_repn(ItemTypeRepn),
    TypeCtorNA = name_arity(unqualify_name(SymName), list.length(Params)),
    % XXX We used to add these to the mivn_type_defns map.
    multi_map.add(TypeCtorNA, Section - Item, !TypeDefnMap).

%---------------------------------------------------------------------------%

:- pred apply_decl_pragma_record(decl_pragma_record::in,
    gathered_item_multi_map_na::in, gathered_item_multi_map_na::out,
    gathered_item_multi_map_na::in, gathered_item_multi_map_na::out,
    gathered_item_multi_map_na::in, gathered_item_multi_map_na::out) is det.

apply_decl_pragma_record(DeclPragma, !PredMap, !FuncMap, !TypeClassMap) :-
    DeclPragma = decl_pragma_record(Section, ItemId, Item),
    ItemId = MaybePredOrFunc - sym_name_arity(SymName, Arity),

    % For predicates defined using `with_type` annotations we don't know
    % the actual arity, so always we need to add entries for pragmas, even if
    % the pragma doesn't match any recorded predicate. For pragmas which don't
    % include enough information to work out whether they apply to a predicate
    % or a function, this will result in an extra entry in the version numbers.
    % Pragmas in the interface aren't common so this won't be too much of
    % a problem.
    NameArity = name_arity(unqualify_name(SymName), Arity),
    (
        MaybePredOrFunc = yes(PredOrFunc),
        (
            PredOrFunc = pf_predicate,
            multi_map.add(NameArity, Section - Item, !PredMap)
        ;
            PredOrFunc = pf_function,
            multi_map.add(NameArity, Section - Item, !FuncMap)
        )
    ;
        MaybePredOrFunc = no,
        multi_map.add(NameArity, Section - Item, !PredMap),
        multi_map.add(NameArity, Section - Item, !FuncMap)
    ),

    % Pragmas can apply to typeclass methods.
    % XXX I (zs) do not see why a decl_pragma *outside* a typeclass
    % declaration should be able to apply to a class method that is declared
    % *inside* the typeclass declaration. We should require that any
    % decl pragma that is intended to apply to a class method
    % should be declared next to it *inside the typeclass declaration itself*.
    map.map_values_only(
        distribute_pragma_items_class_items(MaybePredOrFunc,
            SymName, Arity, Item, Section),
        !TypeClassMap).

:- pred distribute_pragma_items_class_items(maybe(pred_or_func)::in,
    sym_name::in, arity::in, item::in, module_section::in,
    assoc_list(module_section, item)::in,
    assoc_list(module_section, item)::out) is det.

distribute_pragma_items_class_items(MaybePredOrFunc, SymName, Arity,
        Item, Section, !ClassItems) :-
    ( if
        % Does this pragma match any of the methods of this class.
        list.member(_ - ClassItem, !.ClassItems),
        ClassItem = item_typeclass(ClassItemTypeClass),
        ClassItemTypeClass ^ tc_class_methods =
            class_interface_concrete(Decls),
        list.member(Decl, Decls),
        Decl = class_decl_pred_or_func(PredOrFuncInfo),
        PredOrFuncInfo = class_pred_or_func_info(SymName, MethodPredOrFunc,
            TypesAndMaybeModes, WithType, _, _, _, _, _, _, _, _),
        ( MaybePredOrFunc = yes(MethodPredOrFunc)
        ; MaybePredOrFunc = no
        ),
        PredFormArity = types_and_maybe_modes_arity(TypesAndMaybeModes),
        PredFormArity = pred_form_arity(Arity0),
        (
            WithType = no,
            adjust_func_arity(MethodPredOrFunc, Arity, Arity0)
        ;
            WithType = yes(_)
            % We don't know the actual arity, so just match on the name
            % and pred_or_func.
        )
    then
        % XXX O(N^2), but shouldn't happen too often.
        !:ClassItems = !.ClassItems ++ [Section - Item]
    else
        true
    ).

%---------------------------------------------------------------------------%

:- type maybe_pred_or_func_id == pair(maybe(pred_or_func), sym_name_arity).

:- pred gather_decl_pragma_for_what_pf_id(item_decl_pragma_info::in,
    maybe(maybe_pred_or_func_id)::out) is det.

gather_decl_pragma_for_what_pf_id(DeclPragma, MaybePredOrFuncId) :-
    (
        DeclPragma = decl_pragma_type_spec_constr(_TypeSpecConstr),
        % XXX Unlike all the other decl_pragmas, the type_spec_constr
        % pragma is not about a single specified predicate or function,
        % but about all predicates and functions that have a specified
        % set of constraints in their signature. That set is computed later,
        % when we add this pragma to the HLDS.
        MaybePredOrFuncId = no
    ;
        DeclPragma = decl_pragma_type_spec(TypeSpec),
        TypeSpec = decl_pragma_type_spec_info(PFUMM, Name, _, _, _, _, _, _),
        pfumm_to_maybe_pf_arity_maybe_modes(PFUMM, MaybePredOrFunc,
            user_arity(Arity), _MaybeModes),
        MaybePredOrFuncId = yes(MaybePredOrFunc - sym_name_arity(Name, Arity))
    ;
        DeclPragma = decl_pragma_obsolete_proc(ObsoleteProc),
        ObsoleteProc =
            decl_pragma_obsolete_proc_info(PredNameModesPF, _, _, _),
        PredNameModesPF = proc_pf_name_modes(PredOrFunc, Name, Modes),
        adjust_func_arity(PredOrFunc, Arity, list.length(Modes)),
        MaybePredOrFuncId = yes(yes(PredOrFunc) - sym_name_arity(Name, Arity))
    ;
        DeclPragma = decl_pragma_obsolete_pred(ObsoletePred),
        ObsoletePred = decl_pragma_obsolete_pred_info(PredNameArity, _, _, _),
        PredNameArity = pred_pfu_name_arity(PFU, Name, user_arity(Arity)),
        MaybePredOrFunc = pfu_to_maybe_pred_or_func(PFU),
        MaybePredOrFuncId = yes(MaybePredOrFunc - sym_name_arity(Name, Arity))
    ;
        DeclPragma = decl_pragma_format_call(FormatCall),
        FormatCall = decl_pragma_format_call_info(PredNameArity, _, _, _),
        PredNameArity = pred_pf_name_arity(PF, Name, user_arity(Arity)),
        MaybePredOrFuncId = yes(yes(PF) - sym_name_arity(Name, Arity))
    ;
        DeclPragma = decl_pragma_oisu(_),
        % XXX Unlike all the other decl_pragmas, the oisu (order-independent
        % state update) pragma is about a type, not a predicate or function.
        %
        % We don't have to handle it here, because it is not yet implemented.
        % When it *is* implemented, we will need to tell our caller to record
        % this pragma for the type_ctor named in the pragma.
        MaybePredOrFuncId = no
    ;
        (
            DeclPragma = decl_pragma_termination(TermInfo),
            TermInfo = decl_pragma_termination_info(PredNameModesPF,
                _, _, _, _)
        ;
            DeclPragma = decl_pragma_termination2(Term2Info),
            Term2Info = decl_pragma_termination2_info(PredNameModesPF,
                _, _, _, _, _)
        ;
            DeclPragma = decl_pragma_struct_sharing(SharingInfo),
            SharingInfo = decl_pragma_struct_sharing_info(PredNameModesPF,
                _, _, _, _, _, _, _)
        ;
            DeclPragma = decl_pragma_struct_reuse(ReuseInfo),
            ReuseInfo = decl_pragma_struct_reuse_info(PredNameModesPF,
                _, _, _, _, _, _, _)
        ),
        PredNameModesPF = proc_pf_name_modes(PredOrFunc, Name, Modes),
        adjust_func_arity(PredOrFunc, Arity, list.length(Modes)),
        MaybePredOrFuncId = yes(yes(PredOrFunc) - sym_name_arity(Name, Arity))
    ).

:- pred gather_decl_marker_for_what_pf_id(item_decl_marker_info::in,
    maybe(maybe_pred_or_func_id)::out) is det.

gather_decl_marker_for_what_pf_id(DeclMarker, MaybePredOrFuncId) :-
    DeclMarker = item_decl_marker_info(_, PredNameArity, _, _),
    PredNameArity = pred_pfu_name_arity(PFU, Name, user_arity(Arity)),
    MaybePredOrFunc = pfu_to_maybe_pred_or_func(PFU),
    MaybePredOrFuncId = yes(MaybePredOrFunc - sym_name_arity(Name, Arity)).

%---------------------------------------------------------------------------%
%
% Check whether various things are unchanged.
%
% XXX This code is a bit brittle, because in some places the things being
% compared include term.contexts, which can change even if nothing we care
% about has been modified. For example, it won't work for clauses, which
% have lots of contexts inside them.
%
% However, the important thing is that these predicates will never succeed
% when they shouldn't, so they should never cause a necessary recompilation
% to be missed.
%

    % XXX This predicate is unused, which is likely to be a bug.
    %
:- pred is_item_include_changed(item_include::in, item_include::in,
    maybe_changed::out) is det.
:- pragma consider_used(pred(is_item_include_changed/3)).

is_item_include_changed(ItemInclude1, ItemInclude2, Changed) :-
    ItemInclude1 = item_include(ModuleName1, _, _),
    ItemInclude2 = item_include(ModuleName2, _, _),
    ( if ModuleName1 = ModuleName2 then
        Changed = unchanged
    else
        Changed = changed
    ).

    % XXX This predicate is unused, which is likely to be a bug.
    %
:- pred is_item_avail_changed(item_avail::in, item_avail::in,
    maybe_changed::out) is det.
:- pragma consider_used(pred(is_item_avail_changed/3)).

is_item_avail_changed(Avail1, Avail2, Changed) :-
    (
        Avail1 = avail_import(avail_import_info(ModuleName1, _, _)),
        ( if
            Avail2 = avail_import(avail_import_info(ModuleName2, _, _)),
            ModuleName1 = ModuleName2
        then
            Changed = unchanged
        else
            Changed = changed
        )
    ;
        Avail1 = avail_use(avail_use_info(ModuleName1, _, _)),
        ( if
            Avail2 = avail_use(avail_use_info(ModuleName2, _, _)),
            ModuleName1 = ModuleName2
        then
            Changed = unchanged
        else
            Changed = changed
        )
    ).

:- pred are_items_changed(assoc_list(module_section, item)::in,
    assoc_list(module_section, item)::in, maybe_changed::out) is det.

are_items_changed([], [], unchanged).
are_items_changed([], [_ | _], changed).
are_items_changed([_ | _], [], changed).
are_items_changed([Section1 - Item1 | Items1], [Section2 - Item2 | Items2],
        Changed) :-
    ( if Section1 = Section2 then
        is_item_changed(Item1, Item2, ItemChanged),
        (
            ItemChanged = changed,
            Changed = changed
        ;
            ItemChanged = unchanged,
            are_items_changed(Items1, Items2, Changed)
        )
    else
        Changed = changed
    ).

    % In most places here, we don't need to compare the varsets.
    % What matters is that the variable numbers in the arguments
    % and body are the same, the names are usually irrelevant.
    %
    % The only places where the names of variables affect the compilation
    % of the program are in explicit type qualifications and
    % `:- pragma type_spec' declarations. Explicit type qualifications
    % do not need to be considered here. This module only deals with items
    % in interface files (we don't yet write type qualifications to `.opt'
    % files). Variables in type qualifications are only matched with
    % the head type variables of the predicate by make_hlds.m.
    % For `:- pragma type_spec' declarations to work we need to consider
    % a predicate or function declaration to be changed if the names
    % of any of the type variables are changed.
    %
    % It is important not to compare the varsets for type and instance
    % declarations because the declarations we get here may be abstract
    % declarations produced from concrete declarations for use in an
    % interface file. The varsets may contain variables from the discarded
    % bodies which will not be present in the items read in from the
    % interface files for comparison.
    %
    % This code assumes that the variables in the head of a type or instance
    % declaration are added to the varset before those from the body, so that
    % the variable numbers in the head of the declaration match those from
    % an abstract declaration read from an interface file.
    %
:- pred is_item_changed(item::in, item::in, maybe_changed::out) is det.

is_item_changed(Item1, Item2, Changed) :-
    (
        Item1 = item_clause(ItemClause1),
        ItemClause1 = item_clause_info(PorF, SymName, Args, _, Goal, _, _),
        % XXX Need to compare the goals properly in clauses and assertions.
        % That is not necessary at the moment because smart recompilation
        % doesn't work with inter-module optimization yet.
        ( if
            Item2 = item_clause(ItemClause2),
            ItemClause2 = item_clause_info(PorF, SymName, Args, _, Goal, _, _)
        then
            Changed = unchanged
        else
            Changed = changed
        )
    ;
        Item1 = item_type_defn(ItemTypeDefn1),
        ItemTypeDefn1 = item_type_defn_info(_, Name, Args, Defn, _, _),
        ( if
            Item2 = item_type_defn(ItemTypeDefn2),
            ItemTypeDefn2 = item_type_defn_info(_, Name, Args, Defn, _, _)
        then
            Changed = unchanged
        else
            Changed = changed
        )
    ;
        Item1 = item_inst_defn(ItemInstDefn1),
        ItemInstDefn1 = item_inst_defn_info(_, Name, Args,
            MaybeForTypeCtor, Defn, _, _),
        ( if
            Item2 = item_inst_defn(ItemInstDefn2),
            ItemInstDefn2 = item_inst_defn_info(_, Name, Args,
                MaybeForTypeCtor, Defn, _, _)
        then
            Changed = unchanged
        else
            Changed = changed
        )
    ;
        Item1 = item_mode_defn(ItemModeDefn1),
        ItemModeDefn1 = item_mode_defn_info(_, Name, Args, Defn, _, _),
        ( if
            Item2 = item_mode_defn(ItemModeDefn2),
            ItemModeDefn2 = item_mode_defn_info(_, Name, Args, Defn, _, _)
        then
            Changed = unchanged
        else
            Changed = changed
        )
    ;
        Item1 = item_pred_decl(ItemPredDecl1),
        ItemPredDecl1 = item_pred_decl_info(Name, PredOrFunc,
            TypesAndMaybeModes1, WithType1, _, Det1, _, TVarSet1, _,
            ExistQVars1, Purity, Constraints1, _, _),
        ( if
            Item2 = item_pred_decl(ItemPredDecl2),
            ItemPredDecl2 = item_pred_decl_info(Name, PredOrFunc,
                TypesAndMaybeModes2, WithType2, _, Det2, _, TVarSet2, _,
                ExistQVars2, Purity, Constraints2, _, _),

            % For predicates, ignore the determinism -- the modes and
            % determinism should have been split into a separate declaration.
            % This case can only happen if this was not a combined predicate
            % and mode declaration (XXX We should warn about this somewhere).
            % For functions a determinism declaration but no modes implies
            % the default modes. The default modes are added later by
            % make_hlds.m, so they won't have been split into a separate
            % declaration here.
            (
                PredOrFunc = pf_function,
                Det1 = Det2
            ;
                PredOrFunc = pf_predicate
            ),

            pred_or_func_type_is_unchanged(TVarSet1, ExistQVars1,
                TypesAndMaybeModes1, WithType1, Constraints1, TVarSet2,
                ExistQVars2, TypesAndMaybeModes2, WithType2, Constraints2)
        then
            Changed = unchanged
        else
            Changed = changed
        )
    ;
        Item1 = item_mode_decl(ItemModeDecl1),
        ItemModeDecl1 = item_mode_decl_info(Name, PredOrFunc, Modes1,
            WithInst1, Det, InstVarSet1, _, _),
        ( if
            Item2 = item_mode_decl(ItemModeDecl2),
            ItemModeDecl2 = item_mode_decl_info(Name, PredOrFunc, Modes2,
                WithInst2, Det, InstVarSet2, _, _),
            pred_or_func_mode_is_unchanged(InstVarSet1, Modes1, WithInst1,
                InstVarSet2, Modes2, WithInst2)
        then
            Changed = unchanged
        else
            Changed = changed
        )
    ;
        Item1 = item_foreign_proc(ItemForeignProc1),
        ItemForeignProc1 = item_foreign_proc_info(Attrs, SyMname,
            PredOrFunc, Vars, VarSet, InstVarSet, Impl, _, _),
        ( if
            Item2 = item_foreign_proc(ItemForeignProc2),
            ItemForeignProc2 = item_foreign_proc_info(Attrs, SyMname,
                PredOrFunc, Vars, VarSet, InstVarSet, Impl, _, _)
        then
            Changed = unchanged
        else
            Changed = changed
        )
    ;
        Item1 = item_foreign_enum(ItemForeignEnum1),
        ItemForeignEnum1 =
            item_foreign_enum_info(Lang, TypeCtor, Values, _, _),
        ( if
            Item2 = item_foreign_enum(ItemForeignEnum2),
            ItemForeignEnum2 = item_foreign_enum_info(Lang, TypeCtor, Values,
                _, _)
        then
            Changed = unchanged
        else
            Changed = changed
        )
    ;
        Item1 = item_foreign_export_enum(ItemForeignEnum1),
        ItemForeignEnum1 = item_foreign_export_enum_info(Lang,
            TypeCtor, Attrs, Overrides, _, _),
        ( if
            Item2 = item_foreign_export_enum(ItemForeignEnum2),
            ItemForeignEnum2 = item_foreign_export_enum_info(Lang,
                TypeCtor, Attrs, Overrides, _, _)
        then
            Changed = unchanged
        else
            Changed = changed
        )
    ;
        Item1 = item_decl_pragma(DeclPragma1),
        % We do need to compare the variable names in `:- pragma type_spec'
        % declarations because the names of the variables are used to find
        % the corresponding variables in the predicate or function
        % type declaration.
        ( if
            Item2 = item_decl_pragma(DeclPragma2)
        then
            is_decl_pragma_changed(DeclPragma1, DeclPragma2, Changed)
        else
            Changed = changed
        )
    ;
        Item1 = item_decl_marker(DeclMarker1),
        DeclMarker1 = item_decl_marker_info(A, B, _, _),
        ( if
            Item2 = item_decl_marker(DeclMarker2),
            DeclMarker2 = item_decl_marker_info(A, B, _, _)
        then
            Changed = unchanged
        else
            Changed = changed
        )
    ;
        Item1 = item_impl_pragma(ImplPragma1),
        ( if
            Item2 = item_impl_pragma(ImplPragma2)
        then
            is_impl_pragma_changed(ImplPragma1, ImplPragma2, Changed)
        else
            Changed = changed
        )
    ;
        Item1 = item_impl_marker(ImplMarker1),
        ImplMarker1 = item_impl_marker_info(A, B, _, _),
        ( if
            Item2 = item_impl_marker(ImplMarker2),
            ImplMarker2 = item_impl_marker_info(A, B, _, _)
        then
            Changed = unchanged
        else
            Changed = changed
        )
    ;
        Item1 = item_generated_pragma(GenPragma1),
        ( if
            Item2 = item_generated_pragma(GenPragma2)
        then
            is_gen_pragma_changed(GenPragma1, GenPragma2, Changed)
        else
            Changed = changed
        )
    ;
        Item1 = item_promise(ItemPromiseInfo1),
        ItemPromiseInfo1 = item_promise_info(PromiseType, Goal, _,
            UnivVars, _, _),
        ( if
            Item2 = item_promise(ItemPromiseInfo2),
            ItemPromiseInfo2 = item_promise_info(PromiseType, Goal, _,
                UnivVars, _, _)
        then
            Changed = unchanged
        else
            Changed = changed
        )
    ;
        Item1 = item_initialise(ItemInitialise1),
        ItemInitialise1 = item_initialise_info(A, B, C, _, _),
        ( if
            Item2 = item_initialise(ItemInitialise2),
            ItemInitialise2 = item_initialise_info(A, B, C, _, _)
        then
            Changed = unchanged
        else
            Changed = changed
        )
    ;
        Item1 = item_finalise(ItemFinalise1),
        ItemFinalise1 = item_finalise_info(A, B, C, _, _),
        ( if
            Item2 = item_finalise(ItemFinalise2),
            ItemFinalise2 = item_finalise_info(A, B, C, _, _)
        then
            Changed = unchanged
        else
            Changed = changed
        )
    ;
        Item1 = item_mutable(ItemMutable1),
        ItemMutable1 = item_mutable_info(A, _, B, _, C, D, E, F, _, _),
        ( if
            Item2 = item_mutable(ItemMutable2),
            ItemMutable2 = item_mutable_info(A, _, B, _, C, D, E, F, _, _)
        then
            Changed = unchanged
        else
            Changed = changed
        )
    ;
        Item1 = item_typeclass(ItemTypeClass1),
        ItemTypeClass1 = item_typeclass_info(Constraints, FunDeps, Name,
            Vars, Interface1, _, _, _),
        ( if
            Item2 = item_typeclass(ItemTypeClass2),
            ItemTypeClass2 = item_typeclass_info(Constraints, FunDeps, Name,
                Vars, Interface2, _, _, _),
            class_interface_is_unchanged(Interface1, Interface2)
        then
            Changed = unchanged
        else
            Changed = changed
        )
    ;
        Item1 = item_instance(ItemInstance1),
        ItemInstance1 = item_instance_info(Constraints, Name,
            Types, OriginalTypes, Body, _, Module, _, _),
        ( if
            Item2 = item_instance(ItemInstance2),
            ItemInstance2 = item_instance_info(Constraints, Name,
                Types, OriginalTypes, Body, _, Module, _, _)
        then
            Changed = unchanged
        else
            Changed = changed
        )
    ;
        Item1 = item_type_repn(_),
        % Type representation items record information derived from
        % *other items*. They cannot change unless those other items change.
        Changed = unchanged
    ).

:- pred is_decl_pragma_changed(
    item_decl_pragma_info::in, item_decl_pragma_info::in,
    maybe_changed::out) is det.

is_decl_pragma_changed(DeclPragma1, DeclPragma2, Changed) :-
    ( if
        require_complete_switch [DeclPragma1]
        (
            DeclPragma1 = decl_pragma_obsolete_pred(ObsoletePred1),
            DeclPragma2 = decl_pragma_obsolete_pred(ObsoletePred2),
            ObsoletePred1 = decl_pragma_obsolete_pred_info(A, B, _, _),
            ObsoletePred2 = decl_pragma_obsolete_pred_info(A, B, _, _)
        ;
            DeclPragma1 = decl_pragma_obsolete_proc(ObsoleteProc1),
            DeclPragma2 = decl_pragma_obsolete_proc(ObsoleteProc2),
            ObsoleteProc1 = decl_pragma_obsolete_proc_info(A, B, _, _),
            ObsoleteProc2 = decl_pragma_obsolete_proc_info(A, B, _, _)
        ;
            DeclPragma1 = decl_pragma_format_call(FormatCall1),
            DeclPragma2 = decl_pragma_format_call(FormatCall2),
            FormatCall1 = decl_pragma_format_call_info(A, B, _, _),
            FormatCall2 = decl_pragma_format_call_info(A, B, _, _)
        ;
            DeclPragma1 = decl_pragma_type_spec_constr(TypeSpecConstr1),
            DeclPragma2 = decl_pragma_type_spec_constr(TypeSpecConstr2),
            TypeSpecConstr1 = decl_pragma_type_spec_constr_info(ModuleName,
                OoMConstraints1, ApplyToSupers1, OoMTypeSubsts1, TVarSet1,
                _, _, _),
            TypeSpecConstr2 = decl_pragma_type_spec_constr_info(ModuleName,
                OoMConstraints2, ApplyToSupers2, OoMTypeSubsts2, TVarSet2,
                _, _, _),
            ApplyToSupers1 = ApplyToSupers2,
            Constraints1 = one_or_more_to_list(OoMConstraints1),
            Constraints2 = one_or_more_to_list(OoMConstraints2),
            is_any_var_or_ground_constraint_changed(TVarSet1, TVarSet2,
                Constraints1, Constraints2, unchanged),
            TypeSubsts1 = one_or_more_to_list(OoMTypeSubsts1),
            TypeSubsts2 = one_or_more_to_list(OoMTypeSubsts2),
            is_any_type_subst_changed(TVarSet1, TVarSet2,
                TypeSubsts1, TypeSubsts2, unchanged)
        ;
            DeclPragma1 = decl_pragma_type_spec(TypeSpec1),
            DeclPragma2 = decl_pragma_type_spec(TypeSpec2),
            TypeSpec1 = decl_pragma_type_spec_info(PFUMM, Name, SpecName,
                TypeSubst1, TVarSet1, _, _, _),
            TypeSpec2 = decl_pragma_type_spec_info(PFUMM, Name, SpecName,
                TypeSubst2, TVarSet2, _, _, _),
            is_type_subst_changed(TVarSet1, TVarSet2, TypeSubst1, TypeSubst2,
                unchanged)
        ;
            DeclPragma1 = decl_pragma_oisu(OISU1),
            DeclPragma2 = decl_pragma_oisu(OISU2),
            OISU1 = decl_pragma_oisu_info(A, B, C, D, _, _),
            OISU2 = decl_pragma_oisu_info(A, B, C, D, _, _)
        ;
            DeclPragma1 = decl_pragma_termination(Term1),
            DeclPragma2 = decl_pragma_termination(Term2),
            Term1 = decl_pragma_termination_info(A, B, C, _, _),
            Term2 = decl_pragma_termination_info(A, B, C, _, _)
        ;
            DeclPragma1 = decl_pragma_termination2(Term1),
            DeclPragma2 = decl_pragma_termination2(Term2),
            Term1 = decl_pragma_termination2_info(A, B, C, D, _, _),
            Term2 = decl_pragma_termination2_info(A, B, C, D, _, _)
        ;
            DeclPragma1 = decl_pragma_struct_sharing(Sharing1),
            DeclPragma2 = decl_pragma_struct_sharing(Sharing2),
            Sharing1 = decl_pragma_struct_sharing_info(A, B, C, D, E, F, _, _),
            Sharing2 = decl_pragma_struct_sharing_info(A, B, C, D, E, F, _, _)
        ;
            DeclPragma1 = decl_pragma_struct_reuse(Reuse1),
            DeclPragma2 = decl_pragma_struct_reuse(Reuse2),
            Reuse1 = decl_pragma_struct_reuse_info(A, B, C, D, E, F, _, _),
            Reuse2 = decl_pragma_struct_reuse_info(A, B, C, D, E, F, _, _)
        )
    then
        Changed = unchanged
    else
        Changed = changed
    ).

:- pred is_impl_pragma_changed(
    item_impl_pragma_info::in, item_impl_pragma_info::in,
    maybe_changed::out) is det.

is_impl_pragma_changed(ImplPragma1, ImplPragma2, Changed) :-
    ( if
        require_complete_switch [ImplPragma1]
        (
            ImplPragma1 = impl_pragma_foreign_decl(Decl1),
            ImplPragma2 = impl_pragma_foreign_decl(Decl2),
            Decl1 = impl_pragma_foreign_decl_info(A, B, C, _, _),
            Decl2 = impl_pragma_foreign_decl_info(A, B, C, _, _)
        ;
            ImplPragma1 = impl_pragma_foreign_code(Code1),
            ImplPragma2 = impl_pragma_foreign_code(Code2),
            Code1 = impl_pragma_foreign_code_info(A, B, _, _),
            Code2 = impl_pragma_foreign_code_info(A, B, _, _)
        ;
            ImplPragma1 = impl_pragma_fproc_export(Export1),
            ImplPragma2 = impl_pragma_fproc_export(Export2),
            % XXX Comparing the names of inst vars seems excessive.
            Export1 = impl_pragma_fproc_export_info(A, B, C, D, E, _, _),
            Export2 = impl_pragma_fproc_export_info(A, B, C, D, E, _, _)
        ;
            ImplPragma1 = impl_pragma_external_proc(External1),
            ImplPragma2 = impl_pragma_external_proc(External2),
            External1 = impl_pragma_external_proc_info(A, B, _, _),
            External2 = impl_pragma_external_proc_info(A, B, _, _)
        ;
            ImplPragma1 = impl_pragma_fact_table(FactTable1),
            ImplPragma2 = impl_pragma_fact_table(FactTable2),
            FactTable1 = impl_pragma_fact_table_info(A, B, _, _),
            FactTable2 = impl_pragma_fact_table_info(A, B, _, _)
        ;
            ImplPragma1 = impl_pragma_tabled(Tabled1),
            ImplPragma2 = impl_pragma_tabled(Tabled2),
            Tabled1 = impl_pragma_tabled_info(A, B, C, _, _),
            Tabled2 = impl_pragma_tabled_info(A, B, C, _, _)
        ;
            ImplPragma1 = impl_pragma_req_tail_rec(TailRec1),
            ImplPragma2 = impl_pragma_req_tail_rec(TailRec2),
            TailRec1 = impl_pragma_req_tail_rec_info(A, B, _, _),
            TailRec2 = impl_pragma_req_tail_rec_info(A, B, _, _)
        ;
            ImplPragma1 = impl_pragma_req_feature_set(FeatureSet1),
            ImplPragma2 = impl_pragma_req_feature_set(FeatureSet2),
            FeatureSet1 = impl_pragma_req_feature_set_info(A, _, _),
            FeatureSet2 = impl_pragma_req_feature_set_info(A, _, _)
        )
    then
        Changed = unchanged
    else
        Changed = changed
    ).

:- pred is_gen_pragma_changed(
    item_generated_pragma_info::in, item_generated_pragma_info::in,
    maybe_changed::out) is det.

is_gen_pragma_changed(GenPragma1, GenPragma2, Changed) :-
    ( if
        require_complete_switch [GenPragma1]
        (
            GenPragma1 = gen_pragma_unused_args(UnusedArgs1),
            GenPragma2 = gen_pragma_unused_args(UnusedArgs2),
            UnusedArgs1 = gen_pragma_unused_args_info(A, B, _, _),
            UnusedArgs2 = gen_pragma_unused_args_info(A, B, _, _)
        ;
            GenPragma1 = gen_pragma_exceptions(Exceptions1),
            GenPragma2 = gen_pragma_exceptions(Exceptions2),
            Exceptions1 = gen_pragma_exceptions_info(A, B, _, _),
            Exceptions2 = gen_pragma_exceptions_info(A, B, _, _)
        ;
            GenPragma1 = gen_pragma_trailing(Trailing1),
            GenPragma2 = gen_pragma_trailing(Trailing2),
            Trailing1 = gen_pragma_trailing_info(A, B, _, _),
            Trailing2 = gen_pragma_trailing_info(A, B, _, _)
        ;
            GenPragma1 = gen_pragma_mm_tabling(MMTabling1),
            GenPragma2 = gen_pragma_mm_tabling(MMTabling2),
            MMTabling1 = gen_pragma_mm_tabling_info(A, B, _, _),
            MMTabling2 = gen_pragma_mm_tabling_info(A, B, _, _)
        )
    then
        Changed = unchanged
    else
        Changed = changed
    ).

    % Apply a substitution to the existq_tvars, types_and_modes, and
    % prog_constraints so that the type variables from both declarations
    % being checked are contained in the same tvarset, then check that
    % they are identical.
    %
    % We can't just assume that the varsets will be identical for
    % identical declarations because parse_tree_out_misc.m splits
    % combined type and mode declarations into separate declarations.
    % When they are read back in, the variable numbers will be different,
    % because the parser stores the type and inst variables for a combined
    % declaration in a single varset (it doesn't know which are which).
    %
:- pred pred_or_func_type_is_unchanged(tvarset::in, existq_tvars::in,
    types_and_maybe_modes::in, maybe(mer_type)::in, univ_exist_constraints::in,
    tvarset::in, existq_tvars::in, types_and_maybe_modes::in,
    maybe(mer_type)::in, univ_exist_constraints::in) is semidet.

pred_or_func_type_is_unchanged(TVarSet1, ExistQVars1, TypesAndMaybeModes1,
        MaybeWithType1, Constraints1, TVarSet2, ExistQVars2,
        TypesAndMaybeModes2, MaybeWithType2, Constraints2) :-
    % Any mode annotations on a predicate or function declaration
    % should have been split out into a separate mode declaration
    % by gather_items.
    (
        TypesAndMaybeModes1 = no_types_arity_zero,
        Types1 = []
    ;
        TypesAndMaybeModes1 = types_only(Types1)
    ;
        TypesAndMaybeModes1 = types_and_modes(_),
        unexpected($pred, "types_and_modes")
    ),
    (
        TypesAndMaybeModes2 = no_types_arity_zero,
        Types2 = []
    ;
        TypesAndMaybeModes2 = types_only(Types2)
    ;
        TypesAndMaybeModes2 = types_and_modes(_),
        unexpected($pred, "types_and_modes")
    ),
    (
        MaybeWithType1 = yes(WithType1),
        MaybeWithType2 = yes(WithType2),
        AllTypes1 = [WithType1 | Types1],
        AllTypes2 = [WithType2 | Types2]
    ;
        MaybeWithType1 = no,
        MaybeWithType2 = no,
        AllTypes1 = Types1,
        AllTypes2 = Types2
    ),

    type_list_is_unchanged(TVarSet1, TVarSet2, AllTypes1, AllTypes2,
        _TVarSet, Renaming, Types2ToTypes1Subst),

    % Check that the existentially quantified variables are equivalent.
    %
    % XXX kind inference: we assume all tvars have kind `star'.

    map.init(KindMap2),
    apply_variable_renaming_to_tvar_kind_map(Renaming, KindMap2,
        RenamedKindMap2),
    apply_variable_renaming_to_tvar_list(Renaming, ExistQVars2,
        RenamedExistQVars2),
    apply_rec_subst_to_tvar_list(RenamedKindMap2, Types2ToTypes1Subst,
        RenamedExistQVars2, SubstExistQTypes2),
    ( if
        prog_type.type_list_to_var_list(SubstExistQTypes2, SubstExistQVars2)
    then
        ExistQVars1 = SubstExistQVars2
    else
        unexpected($pred, "non-var")
    ),

    % Check that the class constraints are identical.
    apply_variable_renaming_to_univ_exist_constraints(Renaming,
        Constraints2, RenamedConstraints2),
    apply_rec_subst_to_univ_exist_constraints(Types2ToTypes1Subst,
        RenamedConstraints2, SubstConstraints2),
    Constraints1 = SubstConstraints2.

:- pred is_any_var_or_ground_constraint_changed(tvarset::in, tvarset::in,
    list(var_or_ground_constraint)::in, list(var_or_ground_constraint)::in,
    maybe_changed::out) is det.

is_any_var_or_ground_constraint_changed(_, _, [], [], unchanged).
is_any_var_or_ground_constraint_changed(_, _, [], [_ | _], changed).
is_any_var_or_ground_constraint_changed(_, _, [_ | _], [], changed).
is_any_var_or_ground_constraint_changed(TVarSet1, TVarSet2,
        [Constraint1 | Constraints1], [Constraint2 | Constraints2], Changed) :-
    is_var_or_ground_constraint_changed(TVarSet1, TVarSet2,
        Constraint1, Constraint2, HeadChanged),
    (
        HeadChanged = changed,
        Changed = changed
    ;
        HeadChanged = unchanged,
        is_any_var_or_ground_constraint_changed(TVarSet1, TVarSet2,
            Constraints1, Constraints2, Changed)
    ).

:- pred is_var_or_ground_constraint_changed(tvarset::in, tvarset::in,
    var_or_ground_constraint::in, var_or_ground_constraint::in,
    maybe_changed::out) is det.

is_var_or_ground_constraint_changed(TVarSet1, TVarSet2,
        Constraint1, Constraint2, Changed) :-
    Constraint1 = var_or_ground_constraint(ClassName1, Args1, _),
    Constraint2 = var_or_ground_constraint(ClassName2, Args2, _),
    VarOrGroundToType =
        ( pred(Arg::in, Type::out) is det :-
            (
                Arg = type_var_name(TVar, _),
                Type = type_variable(TVar, kind_star)
            ;
                Arg = ground_type(GroundType),
                Type = coerce(GroundType)
            )
        ),
    list.map(VarOrGroundToType, Args1, ArgTypes1),
    list.map(VarOrGroundToType, Args2, ArgTypes2),
    ( if
        ClassName1 = ClassName2,
        type_list_is_unchanged(TVarSet1, TVarSet2, ArgTypes1, ArgTypes2,
            _, _, _)
    then
        Changed = unchanged
    else
        Changed = changed
    ).

:- pred is_any_type_subst_changed(tvarset::in, tvarset::in,
    list(type_subst)::in, list(type_subst)::in, maybe_changed::out) is det.

is_any_type_subst_changed(_, _, [], [], unchanged).
is_any_type_subst_changed(_, _, [], [_ | _], changed).
is_any_type_subst_changed(_, _, [_ | _], [], changed).
is_any_type_subst_changed(TVarSet1, TVarSet2,
        [TypeSubst1 | TypeSubsts1], [TypeSubst2 | TypeSubsts2], Changed) :-
    is_type_subst_changed(TVarSet1, TVarSet2,
        TypeSubst1, TypeSubst2, HeadChanged),
    (
        HeadChanged = changed,
        Changed = changed
    ;
        HeadChanged = unchanged,
        is_any_type_subst_changed(TVarSet1, TVarSet2,
            TypeSubsts1, TypeSubsts2, Changed)
    ).

:- pred is_type_subst_changed(tvarset::in, tvarset::in,
    type_subst::in, type_subst::in, maybe_changed::out) is det.

is_type_subst_changed(TVarSet1, TVarSet2, TypeSubst1, TypeSubst2, Changed) :-
    GetVarType =
        ( pred(tvar_subst(TVar, Type)::in, TVar::out, Type::out) is det ),
    list.map2(GetVarType, one_or_more_to_list(TypeSubst1),
        TVars1, Types1),
    list.map2(GetVarType, one_or_more_to_list(TypeSubst2),
        TVars2, Types2),
    % XXX kind inference:
    % we assume vars have kind `star'.
    KindMap = map.init,
    prog_type.var_list_to_type_list(KindMap, TVars1, TVarTypes1),
    prog_type.var_list_to_type_list(KindMap, TVars2, TVarTypes2),
    ( if
        type_list_is_unchanged(TVarSet1, TVarSet2,
            TVarTypes1 ++ Types1, TVarTypes2 ++ Types2, _, _, _)
    then
        Changed = unchanged
    else
        Changed = changed
    ).

:- pred type_list_is_unchanged(tvarset::in, tvarset::in,
    list(mer_type)::in, list(mer_type)::in,
    tvarset::out, tvar_renaming::out, tsubst::out) is semidet.

type_list_is_unchanged(TVarSet1, TVarSet2, Types1, Types2,
        TVarSet, Renaming, Types2ToTypes1Subst) :-
    tvarset_merge_renaming(TVarSet1, TVarSet2, TVarSet, Renaming),
    apply_variable_renaming_to_type_list(Renaming, Types2, SubstTypes2),

    % Check that the types are equivalent.
    type_list_subsumes(SubstTypes2, Types1, Types2ToTypes1Subst),
    type_list_subsumes(Types1, SubstTypes2, _),

    % Check that the corresponding variables have the same names. This is
    % necessary because `:- pragma type_spec' declarations depend on the names
    % of the variables, so for example if two variable names are swapped,
    % the same `:- pragma type_spec' declaration will cause a different
    % specialized version to be created.

    ( all [VarInItem1, VarInItem2]
        (
            map.member(Types2ToTypes1Subst, VarInItem2, SubstTerm),
            % Note that since the type comes from a substitution,
            % it will not contain a kind annotation.
            SubstTerm = type_variable(VarInItem1, _)
        )
    =>
        (
            varset.lookup_name(TVarSet, VarInItem1, VarName1),
            varset.lookup_name(TVarSet, VarInItem2, VarName2),
            (
                VarName1 = VarName2
            ;
                % Variables written to interface files are always named,
                % even if the variable in the source code was not, so we can't
                % just use varset.search_name to check whether the variables
                % are named.
                VarIsNotNamed =
                    ( pred(VarName::in) is semidet :-
                        string.append("V_", VarNum, VarName),
                        string.to_int(VarNum, _)
                    ),
                VarIsNotNamed(VarName1),
                VarIsNotNamed(VarName2)
            )
        )
    ).

:- pred pred_or_func_mode_is_unchanged(inst_varset::in, list(mer_mode)::in,
    maybe(mer_inst)::in, inst_varset::in, list(mer_mode)::in,
    maybe(mer_inst)::in) is semidet.

pred_or_func_mode_is_unchanged(InstVarSet1, Modes1, MaybeWithInst1,
        InstVarSet2, Modes2, MaybeWithInst2) :-
    varset.coerce(InstVarSet1, VarSet1),
    varset.coerce(InstVarSet2, VarSet2),

    % Apply the substitution to the modes so that the inst variables
    % from both declarations being checked are contained in the same
    % inst_varset, then check that they are identical.
    varset.merge_renaming(VarSet1, VarSet2, _, InstRenaming),

    % Treat modes as terms here to use first_term_list_subsumes_second,
    % which does just what we want here.
    ModeTerms1 = list.map(mode_to_term(output_mercury), Modes1),
    ModeTerms2 = list.map(mode_to_term(output_mercury), Modes2),
    (
        MaybeWithInst1 = yes(Inst1),
        MaybeWithInst2 = yes(Inst2),
        WithInstTerm1 = mode_to_term(output_mercury,
            from_to_mode(free, Inst1)),
        WithInstTerm2 = mode_to_term(output_mercury,
            from_to_mode(free, Inst2)),
        AllModeTerms1 = [WithInstTerm1 | ModeTerms1],
        AllModeTerms2 = [WithInstTerm2 | ModeTerms2]
    ;
        MaybeWithInst1 = no,
        MaybeWithInst2 = no,
        AllModeTerms1 = ModeTerms1,
        AllModeTerms2 = ModeTerms2
    ),

    term_subst.apply_renaming_in_terms(InstRenaming,
        AllModeTerms2, SubstAllModeTerms2),
    first_term_list_subsumes_second(AllModeTerms1, SubstAllModeTerms2, _),
    first_term_list_subsumes_second(SubstAllModeTerms2, AllModeTerms1, _).

    % Combined typeclass method type and mode declarations are split as for
    % ordinary predicate declarations, so the varsets won't necessarily match
    % up if a typeclass declaration is read back from an interface file.
    %
:- pred class_interface_is_unchanged(class_interface::in, class_interface::in)
    is semidet.

class_interface_is_unchanged(Interface0, Interface) :-
    (
        Interface0 = class_interface_abstract,
        Interface = class_interface_abstract
    ;
        Interface0 = class_interface_concrete(Methods1),
        class_methods_are_unchanged(Methods1, Methods2),
        Interface = class_interface_concrete(Methods2)
    ).

:- pred class_methods_are_unchanged(list(class_decl)::in, list(class_decl)::in)
    is semidet.

class_methods_are_unchanged([], []).
class_methods_are_unchanged([Decl1 | Decls1], [Decl2 | Decls2]) :-
    (
        Decl1 = class_decl_pred_or_func(PredOrFuncInfo1),
        Decl2 = class_decl_pred_or_func(PredOrFuncInfo2),
        PredOrFuncInfo1 = class_pred_or_func_info(Name, PredOrFunc,
            TypesAndModes1, WithType1, _, Detism, TVarSet1, _, ExistQVars1,
            Purity, Constraints1, _),
        PredOrFuncInfo2 = class_pred_or_func_info(Name, PredOrFunc,
            TypesAndModes2, WithType2, _, Detism, TVarSet2, _, ExistQVars2,
            Purity, Constraints2, _),
        pred_or_func_type_is_unchanged(TVarSet1, ExistQVars1,
            TypesAndModes1, WithType1, Constraints1,
            TVarSet2, ExistQVars2,
            TypesAndModes2, WithType2, Constraints2)
    ;
        Decl1 = class_decl_mode(ModeInfo1),
        Decl2 = class_decl_mode(ModeInfo2),
        ModeInfo1 = class_mode_info(Name, PredOrFunc, Modes1,
            WithInst1, Det, InstVarSet1, _),
        ModeInfo2 = class_mode_info(Name, PredOrFunc, Modes2,
            WithInst2, Det, InstVarSet2, _),
        pred_or_func_mode_is_unchanged(InstVarSet1, Modes1, WithInst1,
            InstVarSet2, Modes2, WithInst2)
    ),
    class_methods_are_unchanged(Decls1, Decls2).

%---------------------------------------------------------------------------%

module_item_version_numbers_to_string(ModuleItemVersionNumbers) = Str :-
    ModuleItemVersionNumbers =
        module_item_version_numbers(TypeNameMap, TypeDefnMap,
            InstMap, ModeMap, ClassMap, InstanceMap, PredMap, FuncMap),
    ItemTypeMaybeStrs = [
        item_type_and_versions_to_string_na(recomp_type_name, TypeNameMap),
        item_type_and_versions_to_string_na(recomp_type_defn, TypeDefnMap),
        item_type_and_versions_to_string_na(recomp_inst, InstMap),
        item_type_and_versions_to_string_na(recomp_mode, ModeMap),
        item_type_and_versions_to_string_na(recomp_predicate, PredMap),
        item_type_and_versions_to_string_na(recomp_function, FuncMap),
        item_type_and_versions_to_string_na(recomp_typeclass, ClassMap),
        item_type_and_versions_to_string_in("instance", InstanceMap)
    ],
    list.filter_map(maybe_is_yes, ItemTypeMaybeStrs, ItemTypeStrs),
    ItemTypesStr = string.join_list(",\n", ItemTypeStrs),
    string.format("{\n%s\n}", [s(ItemTypesStr)], Str).

%---------------------%

:- func item_type_and_versions_to_string_na(recomp_item_type,
    map(name_arity, version_number)) = maybe(string).

item_type_and_versions_to_string_na(ItemType, VersionMap) = MaybeStr :-
    ( if map.is_empty(VersionMap) then
        MaybeStr = no
    else
        string_to_recomp_item_type(ItemTypeStr, ItemType),
        map.to_assoc_list(VersionMap, VersionsAL),
        ItemVersionStrs =
            list.map(name_arity_version_number_to_string, VersionsAL),
        ItemVersionsStr = string.join_list(",\n", ItemVersionStrs),
        string.format("\t%s(\n%s\n\t)",
            [s(ItemTypeStr), s(ItemVersionsStr)], Str),
        MaybeStr = yes(Str)
    ).

:- func item_type_and_versions_to_string_in(string,
    map(recomp_item_name, version_number)) = maybe(string).

item_type_and_versions_to_string_in(ItemTypeStr, VersionMap) = MaybeStr :-
    ( if map.is_empty(VersionMap) then
        MaybeStr = no
    else
        map.to_assoc_list(VersionMap, VersionsAL),
        ItemVersionStrs =
            list.map(recomp_item_name_version_number_to_string, VersionsAL),
        ItemVersionsStr = string.join_list(",\n", ItemVersionStrs),
        string.format("%s(\n%s\n\t)",
            [s(ItemTypeStr), s(ItemVersionsStr)], Str),
        MaybeStr = yes(Str)
    ).

%---------------------%

:- func name_arity_version_number_to_string(pair(name_arity, version_number))
    = string.

name_arity_version_number_to_string(NameArity - VersionNumber) = Str :-
    NameArity = name_arity(Name, Arity),
    SymNameStr = mercury_bracketed_sym_name_to_string_ngt(
        next_to_graphic_token, unqualified(Name)),
    VersionNumberStr = version_number_to_string(VersionNumber),
    string.format("\t\t%s/%i - %s",
        [s(SymNameStr), i(Arity), s(VersionNumberStr)], Str).

:- func recomp_item_name_version_number_to_string(
    pair(recomp_item_name, version_number)) = string.

recomp_item_name_version_number_to_string(ItemName - VersionNumber) = Str :-
    ItemName = recomp_item_name(SymName, Arity),
    SymNameStr = mercury_bracketed_sym_name_to_string_ngt(
        next_to_graphic_token, SymName),
    VersionNumberStr = version_number_to_string(VersionNumber),
    string.format("%s/%i - %s",
        [s(SymNameStr), i(Arity), s(VersionNumberStr)], Str).

%---------------------------------------------------------------------------%

module_item_version_numbers_version_number = 1.

%---------------------------------------------------------------------------%

parse_module_item_version_numbers(VersionNumbersTerm, Result) :-
    ( if
        VersionNumbersTerm = term.functor(term.atom("{}"),
            VersionNumbersTermList0, _)
    then
        VersionNumbersTermList = VersionNumbersTermList0
    else
        VersionNumbersTermList = [VersionNumbersTerm]
    ),
    map_parser(parse_item_type_version_numbers, VersionNumbersTermList,
        Result0),
    (
        Result0 = ok1(NamedFields),
        % NOTE This update could be done by parse_item_type_version_numbers.
        UpdateNamedField =
            ( pred(VNResult::in, VNs0::in, VNs::out) is det :-
                (
                    VNResult = items(ItemType, ItemVNs),
                    (
                        ItemType = recomp_type_name,
                        VNs = VNs0 ^ mivn_type_names := ItemVNs
                    ;
                        ItemType = recomp_type_defn,
                        VNs = VNs0 ^ mivn_type_defns := ItemVNs
                    ;
                        ItemType = recomp_inst,
                        VNs = VNs0 ^ mivn_insts := ItemVNs
                    ;
                        ItemType = recomp_mode,
                        VNs = VNs0 ^ mivn_modes := ItemVNs
                    ;
                        ItemType = recomp_typeclass,
                        VNs = VNs0 ^ mivn_typeclasses := ItemVNs
                    ;
                        ItemType = recomp_functor,
                        unexpected($pred, "recomp_functor")
                    ;
                        ItemType = recomp_predicate,
                        VNs = VNs0 ^ mivn_predicates := ItemVNs
                    ;
                        ItemType = recomp_function,
                        VNs = VNs0 ^ mivn_functions := ItemVNs
                    ;
                        ItemType = recomp_mutable,
                        unexpected($pred, "recomp_mutable")
                    ;
                        ItemType = recomp_foreign_proc,
                        unexpected($pred, "recomp_foreign_proc")
                    )
                ;
                    VNResult = instances(InstancesVNs),
                    VNs = VNs0 ^ mivn_instances := InstancesVNs
                )
            ),
        ModuleItemVersionNumbers0 = init_module_item_version_numbers,
        list.foldl(UpdateNamedField, NamedFields,
            ModuleItemVersionNumbers0, ModuleItemVersionNumbers),
        Result = ok1(ModuleItemVersionNumbers)
    ;
        Result0 = error1(Errors),
        Result = error1(Errors)
    ).

:- type item_version_numbers_result
    --->    items(recomp_item_type, name_arity_version_map)
    ;       instances(recomp_item_name_version_map).

:- pred parse_item_type_version_numbers(term::in,
    maybe1(item_version_numbers_result)::out) is det.

parse_item_type_version_numbers(Term, Result) :-
    ( if
        Term = term.functor(term.atom(ItemTypeStr), ItemsVNsTerms, _),
        string_to_recomp_item_type(ItemTypeStr, ItemType)
    then
        ParseName =
            ( pred(NameTerm::in, Name::out) is semidet :-
                NameTerm = term.functor(term.atom(Name), [], _)
            ),
        map_parser(parse_key_version_number(ParseName), ItemsVNsTerms,
            Result0),
        (
            Result0 = ok1(VNsAL),
            map.from_assoc_list(VNsAL, VNsMap),
            Result = ok1(items(ItemType, VNsMap))
        ;
            Result0 = error1(Specs),
            Result = error1(Specs)
        )
    else if
        Term = term.functor(term.atom("instance"), InstanceVNsTerms, _)
    then
        map_parser(parse_item_version_number(try_parse_sym_name_and_no_args),
            InstanceVNsTerms, Result1),
        (
            Result1 = ok1(VNsAL),
            map.from_assoc_list(VNsAL, VNsMap),
            Result = ok1(instances(VNsMap))
        ;
            Result1 = error1(Specs),
            Result = error1(Specs)
        )
    else
        % XXX This is an uninformative error message.
        Pieces = [words("Invalid item type version numbers."), nl],
        Spec = spec($pred, severity_error, phase_t2pt,
            get_term_context(Term), Pieces),
        Result = error1([Spec])
    ).

:- pred parse_key_version_number(
    pred(term, string)::in(pred(in, out) is semidet), term::in,
    maybe1(pair(name_arity, version_number))::out) is det.

parse_key_version_number(ParseName, Term, Result) :-
    ( if
        Term = term.functor(term.atom("-"),
            [ItemNameArityTerm, VersionNumberTerm], _),
        ItemNameArityTerm = term.functor(term.atom("/"),
            [NameTerm, ArityTerm], _),
        ParseName(NameTerm, Name),
        term_int.decimal_term_to_int(ArityTerm, Arity),
        parse_version_number_term(VersionNumberTerm, VersionNumber)
    then
        Result = ok1(name_arity(Name, Arity) - VersionNumber)
    else
        Pieces = [words("Error in item version number."), nl],
        Spec = spec($pred, severity_error, phase_t2pt,
            get_term_context(Term), Pieces),
        Result = error1([Spec])
    ).

:- pred parse_item_version_number(
    pred(term, sym_name)::in(pred(in, out) is semidet), term::in,
    maybe1(pair(recomp_item_name, version_number))::out) is det.

parse_item_version_number(ParseName, Term, Result) :-
    ( if
        Term = term.functor(term.atom("-"),
            [ItemNameArityTerm, VersionNumberTerm], _),
        ItemNameArityTerm = term.functor(term.atom("/"),
            [NameTerm, ArityTerm], _),
        ParseName(NameTerm, SymName),
        term_int.decimal_term_to_int(ArityTerm, Arity),
        parse_version_number_term(VersionNumberTerm, VersionNumber)
    then
        Result = ok1(recomp_item_name(SymName, Arity) - VersionNumber)
    else
        Pieces = [words("Error in item version number."), nl],
        Spec = spec($pred, severity_error, phase_t2pt,
            get_term_context(Term), Pieces),
        Result = error1([Spec])
    ).

%---------------------------------------------------------------------------%
:- end_module recompilation.version.
%---------------------------------------------------------------------------%
