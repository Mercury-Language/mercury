%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This module separates the different kinds of items in an augmented
% compilation unit.
%
%---------------------------------------------------------------------------%

:- module hlds.make_hlds.make_hlds_passes.make_hlds_separate_items.
:- interface.

:- import_module parse_tree.
:- import_module parse_tree.prog_item.
:- import_module parse_tree.prog_data.

:- import_module list.
:- import_module set.

%---------------------------------------------------------------------------%

:- type ims_tuple_list(T) == list(ims_tuple_item(T)).
:- type ims_tuple_item(T) == {item_mercury_status, T}.

    % The augmented compilation unit consists of the parse tree of
    % a source module, the parse trees of interface files, and the
    % parse trees of optimization files. For most of these kinds of item,
    % return all the items of that kind in these parse trees in a
    % two level list. The top level list, which is usually a sec_list
    % or an ims_list, is a plain list, whose elements are sec_sub_lists
    % and ims_sub_lists respectively. Each of these sublists contains
    % a list of the items of the given kind in one section of a parse tree,
    % wrapped up together with either a sec_info or an item_mercury_status,
    % both of which specify properties of the given section of the
    % given parse tree. The properties say e.g. whether the section is
    % in the current module, whether its contents are imported or exported,
    % whether references elsewhere in the code to the entities it defines
    % must be module qualified or not.
    %
    % XXX CLEANUP We should return several kinds of items as maps,
    % not as lists. Specifically,
    %
    % - we should return type definitions as a type_ctor_checked_map,
    %   or at least as a type_ctor_defn_map, in both cases with additional
    %   section information in the values;
    % - we should return inst definitions as an inst_ctor_defn_map
    %   extended with status information;
    % - we should return mode definitions as a mode_ctor_defn_map
    %   extended with status information;
    % - we should return type representation info as a map from type_ctor
    %   to item_type_repn_info.
    %
    % Several of the parse trees kinds that we process here already have
    % this information in the map form. We flatten these maps to lists
    % only to minimize the complexity of the diff that radically rewrites
    % this module and the associated code.
    %
:- pred separate_items_in_aug_comp_unit(aug_compilation_unit::in,
    ims_list(item_avail)::out,
    list(item_fim)::out,
    sec_list(item_type_defn_info)::out,
    sec_list(item_type_defn_info)::out,
    sec_list(item_type_defn_info)::out,
    ims_list(item_inst_defn_info)::out,
    ims_list(item_mode_defn_info)::out,
    sec_list(item_pred_decl_info)::out,
    ims_list(item_mode_decl_info)::out,
    ims_list(item_promise_info)::out,
    sec_list(item_typeclass_info)::out,
    ims_list(item_instance_info)::out,
    ims_list(item_initialise_info)::out,
    ims_list(item_finalise_info)::out,
    sec_list(item_mutable_info)::out,
    type_ctor_repn_map::out,
    ims_tuple_list(item_foreign_enum_info)::out,
    list(item_foreign_export_enum_info)::out,
    ims_list(item_decl_pragma_info)::out,
    ims_list(item_impl_pragma_info)::out,
    list(item_generated_pragma_info)::out,
    ims_list(item_clause_info)::out,
    set(pf_sym_name_arity)::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.convert_parse_tree. % XXX Undesirable dependency.
:- import_module parse_tree.file_kind.          % XXX Undesirable dependency.
:- import_module parse_tree.item_util.

:- import_module assoc_list.
:- import_module cord.
:- import_module int.
:- import_module pair.

%---------------------------------------------------------------------------%

:- type int_type_ctor_repns
    --->    int_type_ctor_repns(int_file_kind, type_ctor_repn_map).

:- type module_int_type_ctor_repns ==
    assoc_list(module_name, int_type_ctor_repns).

:- type ims_tuple_cord(T) == cord(ims_tuple_item(T)).

:- type item_accumulator
    --->    item_accumulator(
                ia_avails           :: ims_cord(item_avail),
                ia_fims             :: cord(item_fim),
                ia_type_defns_abs   :: sec_cord(item_type_defn_info),
                ia_type_defns_mer   :: sec_cord(item_type_defn_info),
                ia_type_defns_for   :: sec_cord(item_type_defn_info),
                ia_inst_defns       :: ims_cord(item_inst_defn_info),
                ia_mode_defns       :: ims_cord(item_mode_defn_info),
                ia_typeclasses      :: sec_cord(item_typeclass_info),
                ia_instances        :: ims_cord(item_instance_info),
                ia_pred_decls       :: sec_cord(item_pred_decl_info),
                ia_mode_decls       :: ims_cord(item_mode_decl_info),
                ia_clauses          :: ims_cord(item_clause_info),
                ia_foreign_enums    :: ims_tuple_cord(item_foreign_enum_info),
                ia_fees             :: cord(item_foreign_export_enum_info),
                ia_decl_pragmas     :: ims_cord(item_decl_pragma_info),
                ia_impl_pragmas     :: ims_cord(item_impl_pragma_info),
                ia_gen_pragmas      :: cord(item_generated_pragma_info),
                ia_promises         :: ims_cord(item_promise_info),
                ia_initialises      :: ims_cord(item_initialise_info),
                ia_finalises        :: ims_cord(item_finalise_info),
                ia_mutables         :: sec_cord(item_mutable_info),
                ia_type_repns       :: module_int_type_ctor_repns
            ).

separate_items_in_aug_comp_unit(AugCompUnit, Avails, FIMs,
        TypeDefnsAbstract, TypeDefnsMercury, TypeDefnsForeign,
        InstDefns, ModeDefns, PredDecls, ModeDecls,
        Promises, Typeclasses, Instances,
        Initialises, Finalises, Mutables,
        TypeRepnMap, ForeignEnums, ForeignExportEnums,
        PragmasDecl, PragmasImpl, PragmasGen, Clauses, IntBadPreds) :-
    AugCompUnit = aug_compilation_unit(_ModuleName, _ModuleNameContext,
        _ModuleVersionNumbers, ParseTreeModuleSrc,
        AncestorIntSpecs, DirectIntSpecs, IndirectIntSpecs,
        PlainOpts, TransOpts, IntForOptSpecs, TypeRepnSpecs),
    % We start with an empty cord for each kind of item.
    % Then for each file we have read in, we append to this cord
    % all the items of that kind in that file to the list,
    % with the items in the interface section preceding the items
    % in the implementation section. The final result should be a cord
    % of all the items of that kind in the original forward order.
    %
    % XXX ITEM_LIST PlainOpts and TransOpts have separate lists of
    % the different kinds of pragmas they may contain, but they have to
    % wrap them up as decl_pragmas or generated_pragmas before adding them
    % to the accumulator. This should not be necessary.
    some [!Acc] (
        !:Acc = item_accumulator(cord.init, cord.init,
            cord.init, cord.init, cord.init,
            cord.init, cord.init, cord.init, cord.init,
            cord.init, cord.init, cord.init,
            cord.init, cord.init,
            cord.init, cord.init, cord.init, cord.init,
            cord.init, cord.init, cord.init, []),
        acc_parse_tree_module_src(ParseTreeModuleSrc, !Acc),
        map.foldl_values(acc_ancestor_int_spec, AncestorIntSpecs, !Acc),
        map.foldl_values(acc_direct_int_spec, DirectIntSpecs, !Acc),
        map.foldl_values(acc_indirect_int_spec, IndirectIntSpecs, !Acc),
        map.foldl_values(acc_parse_tree_plain_opt, PlainOpts, !Acc),
        map.foldl_values(acc_parse_tree_trans_opt, TransOpts, !Acc),
        map.foldl_values(acc_int_for_opt_spec, IntForOptSpecs, !Acc),
        map.foldl_values(acc_type_repn_spec, TypeRepnSpecs, !Acc),
        !.Acc = item_accumulator(AvailsCord, FIMsCord,
            TypeDefnsAbstractCord, TypeDefnsMercuryCord, TypeDefnsForeignCord,
            InstDefnsCord, ModeDefnsCord, TypeclassesCord, InstancesCord,
            PredDeclsCord, ModeDeclsCord, ClausesCord,
            ForeignEnumsCord, ForeignExportEnumsCord,
            PragmasDeclCord, PragmasImplCord, PragmasGenCord, PromisesCord,
            InitialisesCord, FinalisesCord, MutablesCord, ModuleIntTypeRepns)
    ),

    Avails = cord.list(AvailsCord),
    FIMs = cord.list(FIMsCord),
    TypeDefnsAbstract = cord.list(TypeDefnsAbstractCord),
    TypeDefnsMercury = cord.list(TypeDefnsMercuryCord),
    TypeDefnsForeign = cord.list(TypeDefnsForeignCord),
    InstDefns = cord.list(InstDefnsCord),
    ModeDefns = cord.list(ModeDefnsCord),
    Typeclasses = cord.list(TypeclassesCord),
    Instances = cord.list(InstancesCord),
    PredDecls = cord.list(PredDeclsCord),
    ModeDecls = cord.list(ModeDeclsCord),
    Clauses = cord.list(ClausesCord),
    ForeignEnums = cord.list(ForeignEnumsCord),
    ForeignExportEnums = cord.list(ForeignExportEnumsCord),
    PragmasDecl = cord.list(PragmasDeclCord),
    PragmasImpl = cord.list(PragmasImplCord),
    PragmasGen = cord.list(PragmasGenCord),
    Promises = cord.list(PromisesCord),
    Initialises = cord.list(InitialisesCord),
    Finalises = cord.list(FinalisesCord),
    Mutables = cord.list(MutablesCord),

    list.foldl(acc_int_type_repn_map, ModuleIntTypeRepns,
        map.init, ModuleIntTypeRepnMap),
    map.foldl_values(acc_type_repn_map, ModuleIntTypeRepnMap,
        cord.init, TypeCtorRepnsCord),
    map.from_sorted_assoc_list(cord.list(TypeCtorRepnsCord), TypeRepnMap),
    IntBadPreds = ParseTreeModuleSrc ^ ptms_int_bad_clauses.

:- pred acc_int_type_repn_map(pair(module_name, int_type_ctor_repns)::in,
    map(module_name, int_type_ctor_repns)::in,
    map(module_name, int_type_ctor_repns)::out) is det.

acc_int_type_repn_map(ModuleName - IntTypeRepns, !ModuleMap) :-
    IntTypeRepns = int_type_ctor_repns(IntFileKind, _TypeCtorRepnMap),
    ( if map.search(!.ModuleMap, ModuleName, OldIntTypeRepns) then
        OldIntTypeRepns =
            int_type_ctor_repns(OldIntFileKind, _OldTypeCtorRepnMap),
        Content = type_repn_content(IntFileKind),
        OldContent = type_repn_content(OldIntFileKind),
        ( if Content > OldContent then
            map.det_update(ModuleName, IntTypeRepns, !ModuleMap)
        else
            true
        )
    else
        map.det_insert(ModuleName, IntTypeRepns, !ModuleMap)
    ).

    % How much type_repn information does each kind of interface file have,
    % relatively speaking? A higher number indicates more.
    %
    % .int0 files contain no type_repns at all. .int files contain a
    % type_repn item for every type defined in the module. .int2 and .int3
    % files both contain the same information: a type_repn item for only
    % (a) the simple types defined in the module (direct dummy, enum and
    % notag types) and (b) the non-simple types that have a property
    % (such as word alignment) that can useful in the representations
    % of other types. To make the choice deterministic, we break the tie
    % in favor of .int2 files.
    %
:- func type_repn_content(int_file_kind) = int.

type_repn_content(ifk_int0) = 0.
type_repn_content(ifk_int1) = 3.
type_repn_content(ifk_int2) = 2.
type_repn_content(ifk_int3) = 1.

:- pred acc_type_repn_map(int_type_ctor_repns::in,
    cord(pair(type_ctor, item_type_repn_info))::in,
    cord(pair(type_ctor, item_type_repn_info))::out) is det.

acc_type_repn_map(IntTypeRepns, !Cord) :-
    IntTypeRepns = int_type_ctor_repns(_IntFileKind, TypeCtorRepnMap),
    map.to_sorted_assoc_list(TypeCtorRepnMap, TypeCtorRepnPairs),
    !:Cord = !.Cord ++ cord.from_list(TypeCtorRepnPairs).

%---------------------------------------------------------------------------%

:- pred acc_ancestor_int_spec(ancestor_int_spec::in,
    item_accumulator::in, item_accumulator::out) is det.

acc_ancestor_int_spec(AncestorIntSpec, !Acc) :-
    AncestorIntSpec = ancestor_int0(ParseTreeInt0, ReadWhy0),
    acc_parse_tree_int0(ParseTreeInt0, ReadWhy0, !Acc).

:- pred acc_direct_int_spec(direct_int_spec::in,
    item_accumulator::in, item_accumulator::out) is det.

acc_direct_int_spec(DirectIntSpec, !Acc) :-
    (
        DirectIntSpec = direct_int1(ParseTreeInt1, ReadWhy1),
        acc_parse_tree_int1(ParseTreeInt1, ReadWhy1, !Acc)
    ;
        DirectIntSpec = direct_int3(ParseTreeInt3, ReadWhy3),
        acc_parse_tree_int3(ParseTreeInt3, ReadWhy3, !Acc)
    ).

:- pred acc_indirect_int_spec(indirect_int_spec::in,
    item_accumulator::in, item_accumulator::out) is det.

acc_indirect_int_spec(IndirectIntSpec, !Acc) :-
    (
        IndirectIntSpec = indirect_int2(ParseTreeInt2, ReadWhy2),
        acc_parse_tree_int2(ParseTreeInt2, ReadWhy2, !Acc)
    ;
        IndirectIntSpec = indirect_int3(ParseTreeInt3, ReadWhy3),
        acc_parse_tree_int3(ParseTreeInt3, ReadWhy3, !Acc)
    ).

:- pred acc_int_for_opt_spec(int_for_opt_spec::in,
    item_accumulator::in, item_accumulator::out) is det.

acc_int_for_opt_spec(IntForOptSpec, !Acc) :-
    (
        IntForOptSpec = for_opt_int0(ParseTreeInt0, ReadWhy0),
        acc_parse_tree_int0(ParseTreeInt0, ReadWhy0, !Acc)
    ;
        IntForOptSpec = for_opt_int1(ParseTreeInt1, ReadWhy1),
        acc_parse_tree_int1(ParseTreeInt1, ReadWhy1, !Acc)
    ;
        IntForOptSpec = for_opt_int2(ParseTreeInt2, ReadWhy2),
        acc_parse_tree_int2(ParseTreeInt2, ReadWhy2, !Acc)
    ).

:- pred acc_type_repn_spec(type_repn_spec::in,
    item_accumulator::in, item_accumulator::out) is det.

acc_type_repn_spec(TypeRepnSpec, !Acc) :-
    TypeRepnSpec = type_repn_spec_int1(ParseTreeInt1),
    ModuleName = ParseTreeInt1 ^ pti1_module_name,
    IntTypeRepnMap = ParseTreeInt1 ^ pti1_int_type_repns,
    AccTypeRepns0 = !.Acc ^ ia_type_repns,
    AccTypeRepns = [ModuleName - int_type_ctor_repns(ifk_int1, IntTypeRepnMap)
        | AccTypeRepns0],
    !Acc ^ ia_type_repns := AccTypeRepns.

%---------------------------------------------------------------------------%

:- pred acc_parse_tree_module_src(parse_tree_module_src::in,
    item_accumulator::in, item_accumulator::out) is det.

acc_parse_tree_module_src(ParseTreeModuleSrc, !Acc) :-
    % XXX CLEANUP *If* we could rely on _MaybeImplicitFIMLangs
    % having been filled in by now, which unfortunately we can't,
    % we could include the FIMs it corresponds to in AccFIMs,
    % which should allow us to delete some code elsewhere in the compiler
    % that now has the job of computing the set of FIMs implicitly needed
    % by the code of this module.
    ParseTreeModuleSrc = parse_tree_module_src(_ModuleName, _ModuleNameContext,
        _IntInclMap, _ImpInclMap, InclMap,
        _IntImportMap, _IntUseMap, _ImpImportMap, _ImpUseMap, ImportUseMap,
        IntFIMSpecMap, ImpFIMSpecMap, _MaybeImplicitFIMLangs,

        IntTypeDefnsAbs, IntTypeDefnsMer, IntTypeDefnsFor,
        IntInstDefns, IntModeDefns, IntTypeClasses, IntInstances,
        IntPredDecls, IntModeDecls,
        IntDeclPragmas, IntPromises, _IntBadPreds,

        SubTypeDefnsAbs, SubTypeDefnsMer, SubTypeDefnsFor,
        SubInstDefns, SubModeDefns, SubTypeClasses, SubInstances,
        SubPredDecls, SubModeDecls, ImpClauses,
        SubForeignEnums, ImpForeignExportEnums,
        SubDeclPragmas, ImpImplPragmas, SubPromises,
        ImpInitialises, ImpFinalises, SubMutables),

    IntItemMercuryStatus = item_defined_in_this_module(item_export_anywhere),
    ImpItemMercuryStatus = item_defined_in_this_module(item_export_nowhere),
    IntSectionInfo = sec_info(IntItemMercuryStatus, may_be_unqualified),
    ImpSectionInfo = sec_info(ImpItemMercuryStatus, may_be_unqualified),
    ( if map.is_empty(InclMap) then
        % There are no submodules to export stuff to.
        SubItemMercuryStatus = ImpItemMercuryStatus,
        SubSectionInfo = ImpSectionInfo
    else
        SubItemMercuryStatus = 
            item_defined_in_this_module(item_export_only_submodules),
        SubSectionInfo = sec_info(SubItemMercuryStatus, may_be_unqualified)
    ),

    AccAvails0 = !.Acc ^ ia_avails,
    AccFIMs0 = !.Acc ^ ia_fims,
    AccTypeDefnsAbs0 = !.Acc ^ ia_type_defns_abs,
    AccTypeDefnsMer0 = !.Acc ^ ia_type_defns_mer,
    AccTypeDefnsFor0 = !.Acc ^ ia_type_defns_for,
    AccInstDefns0 = !.Acc ^ ia_inst_defns,
    AccModeDefns0 = !.Acc ^ ia_mode_defns,
    AccTypeClasses0 = !.Acc ^ ia_typeclasses,
    AccInstances0 = !.Acc ^ ia_instances,
    AccPredDecls0 = !.Acc ^ ia_pred_decls,
    AccModeDecls0 = !.Acc ^ ia_mode_decls,
    AccClauses0 = !.Acc ^ ia_clauses,
    AccForeignEnums0 = !.Acc ^ ia_foreign_enums,
    AccForeignExportEnums0 = !.Acc ^ ia_fees,
    AccDeclPragmas0 = !.Acc ^ ia_decl_pragmas,
    AccImplPragmas0 = !.Acc ^ ia_impl_pragmas,
    AccPromises0 = !.Acc ^ ia_promises,
    AccInitialises0 = !.Acc ^ ia_initialises,
    AccFinalises0 = !.Acc ^ ia_finalises,
    AccMutables0 = !.Acc ^ ia_mutables,

    import_and_or_use_map_to_item_avails(do_not_include_implicit,
        ImportUseMap, IntAvails, ImpAvails),
    acc_ims_avails(IntItemMercuryStatus, IntAvails, AccAvails0, AccAvails1),
    acc_ims_avails(ImpItemMercuryStatus, ImpAvails, AccAvails1, AccAvails),
    IntFIMs = list.map(fim_spec_to_item, map.keys(IntFIMSpecMap)),
    ImpFIMs = list.map(fim_spec_to_item, map.keys(ImpFIMSpecMap)),
    AccFIMs = AccFIMs0 ++ cord.from_list(IntFIMs) ++ cord.from_list(ImpFIMs),
    acc_sec_list(IntSectionInfo, IntTypeDefnsAbs,
        AccTypeDefnsAbs0, AccTypeDefnsAbs1),
    acc_sec_list(SubSectionInfo, SubTypeDefnsAbs,
        AccTypeDefnsAbs1, AccTypeDefnsAbs),
    acc_sec_list(IntSectionInfo, IntTypeDefnsMer,
        AccTypeDefnsMer0, AccTypeDefnsMer1),
    acc_sec_list(SubSectionInfo, SubTypeDefnsMer,
        AccTypeDefnsMer1, AccTypeDefnsMer),
    acc_sec_list(IntSectionInfo, IntTypeDefnsFor,
        AccTypeDefnsFor0, AccTypeDefnsFor1),
    acc_sec_list(SubSectionInfo, SubTypeDefnsFor,
        AccTypeDefnsFor1, AccTypeDefnsFor),
    acc_ims_list(IntItemMercuryStatus, IntInstDefns,
        AccInstDefns0, AccInstDefns1),
    acc_ims_list(SubItemMercuryStatus, SubInstDefns,
        AccInstDefns1, AccInstDefns),
    acc_ims_list(IntItemMercuryStatus, IntModeDefns,
        AccModeDefns0, AccModeDefns1),
    acc_ims_list(SubItemMercuryStatus, SubModeDefns,
        AccModeDefns1, AccModeDefns),
    acc_sec_list(IntSectionInfo, IntTypeClasses,
        AccTypeClasses0, AccTypeClasses1),
    acc_sec_list(SubSectionInfo, SubTypeClasses,
        AccTypeClasses1, AccTypeClasses),
    acc_ims_list(IntItemMercuryStatus, IntInstances,
        AccInstances0, AccInstances1),
    acc_ims_list(SubItemMercuryStatus, SubInstances,
        AccInstances1, AccInstances),
    acc_sec_list(IntSectionInfo, IntPredDecls, AccPredDecls0, AccPredDecls1),
    acc_sec_list(SubSectionInfo, SubPredDecls, AccPredDecls1, AccPredDecls),
    acc_ims_list(IntItemMercuryStatus, IntModeDecls,
        AccModeDecls0, AccModeDecls1),
    acc_ims_list(SubItemMercuryStatus, SubModeDecls,
        AccModeDecls1, AccModeDecls),
    acc_ims_list(ImpItemMercuryStatus, ImpClauses,
        AccClauses0, AccClauses),
    acc_ims_tuple_list(SubItemMercuryStatus, SubForeignEnums,
        AccForeignEnums0, AccForeignEnums),
    AccForeignExportEnums = AccForeignExportEnums0 ++
        cord.from_list(ImpForeignExportEnums),
    acc_ims_list(IntItemMercuryStatus, IntDeclPragmas,
        AccDeclPragmas0, AccDeclPragmas1),
    acc_ims_list(SubItemMercuryStatus, SubDeclPragmas,
        AccDeclPragmas1, AccDeclPragmas),
    acc_ims_list(SubItemMercuryStatus, ImpImplPragmas,
        AccImplPragmas0, AccImplPragmas),
    acc_ims_list(IntItemMercuryStatus, IntPromises,
        AccPromises0, AccPromises1),
    acc_ims_list(SubItemMercuryStatus, SubPromises,
        AccPromises1, AccPromises),
    acc_ims_list(ImpItemMercuryStatus, ImpInitialises,
        AccInitialises0, AccInitialises),
    acc_ims_list(ImpItemMercuryStatus, ImpFinalises,
        AccFinalises0, AccFinalises),
    acc_sec_list(SubSectionInfo, SubMutables, AccMutables0, AccMutables),

    !Acc ^ ia_avails := AccAvails,
    !Acc ^ ia_fims := AccFIMs,
    !Acc ^ ia_type_defns_abs := AccTypeDefnsAbs,
    !Acc ^ ia_type_defns_mer := AccTypeDefnsMer,
    !Acc ^ ia_type_defns_for := AccTypeDefnsFor,
    !Acc ^ ia_inst_defns := AccInstDefns,
    !Acc ^ ia_mode_defns := AccModeDefns,
    !Acc ^ ia_typeclasses := AccTypeClasses,
    !Acc ^ ia_instances := AccInstances,
    !Acc ^ ia_pred_decls := AccPredDecls,
    !Acc ^ ia_mode_decls := AccModeDecls,
    !Acc ^ ia_clauses := AccClauses,
    !Acc ^ ia_foreign_enums := AccForeignEnums,
    !Acc ^ ia_fees := AccForeignExportEnums,
    !Acc ^ ia_decl_pragmas := AccDeclPragmas,
    !Acc ^ ia_impl_pragmas := AccImplPragmas,
    !Acc ^ ia_promises := AccPromises,
    !Acc ^ ia_initialises := AccInitialises,
    !Acc ^ ia_finalises := AccFinalises,
    !Acc ^ ia_mutables := AccMutables.

%---------------------%

:- pred acc_parse_tree_int0(parse_tree_int0::in,
    read_why_int0::in, item_accumulator::in, item_accumulator::out) is det.

acc_parse_tree_int0(ParseTreeInt0, ReadWhy0, !Acc) :-
    (
        ReadWhy0 = rwi0_section,
        IntImportLocn = import_locn_ancestor_int0_interface,
        ImpImportLocn = import_locn_ancestor_int0_implementation,
        IntItemImport = item_import_int_concrete(IntImportLocn),
        ImpItemImport = item_import_int_concrete(ImpImportLocn),
        IntNeedQual = may_be_unqualified,
        ImpNeedQual = may_be_unqualified
    ;
        ReadWhy0 = rwi0_opt,
        IntItemImport = item_import_opt_int,
        ImpItemImport = item_import_opt_int,
        IntNeedQual = must_be_qualified,
        ImpNeedQual = must_be_qualified
    ),
    IntItemMercuryStatus = item_defined_in_other_module(IntItemImport),
    ImpItemMercuryStatus = item_defined_in_other_module(ImpItemImport),
    IntSectionInfo = sec_info(IntItemMercuryStatus, IntNeedQual),
    ImpSectionInfo = sec_info(ImpItemMercuryStatus, ImpNeedQual),

    ParseTreeInt0 = parse_tree_int0(_ModuleName, _ModuleNameContext,
        _MaybeVersionNumbers, _IntInclMap, _ImpInclMap, _InclMap,
        _IntImportMap, _IntUseMap, _ImpImportMap, _ImpUseMap, ImportUseMap,
        IntFIMSpecs, ImpFIMSpecs,
        IntTypeDefnMap, IntInstDefnMap, IntModeDefnMap,
        IntTypeClasses, IntInstances, IntPredDecls, IntModeDecls,
        IntDeclPragmas, IntPromises,
        ImpTypeDefnMap, ImpInstDefnMap, ImpModeDefnMap,
        ImpTypeClasses, ImpInstances, ImpPredDecls, ImpModeDecls,
        ImpForeignEnumMap, ImpDeclPragmas, ImpPromises),

    AccAvails0 = !.Acc ^ ia_avails,
    AccFIMs0 = !.Acc ^ ia_fims,
    AccTypeDefnsAbs0 = !.Acc ^ ia_type_defns_abs,
    AccTypeDefnsMer0 = !.Acc ^ ia_type_defns_mer,
    AccTypeDefnsFor0 = !.Acc ^ ia_type_defns_for,
    AccInstDefns0 = !.Acc ^ ia_inst_defns,
    AccModeDefns0 = !.Acc ^ ia_mode_defns,
    AccTypeClasses0 = !.Acc ^ ia_typeclasses,
    AccInstances0 = !.Acc ^ ia_instances,
    AccPredDecls0 = !.Acc ^ ia_pred_decls,
    AccModeDecls0 = !.Acc ^ ia_mode_decls,
    AccForeignEnums0 = !.Acc ^ ia_foreign_enums,
    AccDeclPragmas0 = !.Acc ^ ia_decl_pragmas,
    AccPromises0 = !.Acc ^ ia_promises,

    import_and_or_use_map_to_item_avails(do_not_include_implicit,
        ImportUseMap, IntAvails, ImpAvails),
    acc_ims_avails(IntItemMercuryStatus, IntAvails, AccAvails0, AccAvails1),
    acc_ims_avails(ImpItemMercuryStatus, ImpAvails, AccAvails1, AccAvails),
    IntFIMs = list.map(fim_spec_to_item, set.to_sorted_list(IntFIMSpecs)),
    ImpFIMs = list.map(fim_spec_to_item, set.to_sorted_list(ImpFIMSpecs)),
    AccFIMs = AccFIMs0 ++ cord.from_list(IntFIMs) ++ cord.from_list(ImpFIMs),
    IntTypeDefns = type_ctor_defn_map_to_type_defns(IntTypeDefnMap),
    ImpTypeDefns = type_ctor_defn_map_to_type_defns(ImpTypeDefnMap),
    separate_type_defns_abs_mer_for(IntTypeDefns,
        [], IntTypeDefnsAbs, [], IntTypeDefnsMer, [], IntTypeDefnsFor),
    separate_type_defns_abs_mer_for(ImpTypeDefns,
        [], ImpTypeDefnsAbs, [], ImpTypeDefnsMer, [], ImpTypeDefnsFor),
    acc_sec_list(IntSectionInfo, IntTypeDefnsAbs,
        AccTypeDefnsAbs0, AccTypeDefnsAbs1),
    acc_sec_list(ImpSectionInfo, ImpTypeDefnsAbs,
        AccTypeDefnsAbs1, AccTypeDefnsAbs),
    acc_sec_list(IntSectionInfo, IntTypeDefnsMer,
        AccTypeDefnsMer0, AccTypeDefnsMer1),
    acc_sec_list(ImpSectionInfo, ImpTypeDefnsMer,
        AccTypeDefnsMer1, AccTypeDefnsMer),
    acc_sec_list(IntSectionInfo, IntTypeDefnsFor,
        AccTypeDefnsFor0, AccTypeDefnsFor1),
    acc_sec_list(ImpSectionInfo, ImpTypeDefnsFor,
        AccTypeDefnsFor1, AccTypeDefnsFor),
    IntInstDefns = inst_ctor_defn_map_to_inst_defns(IntInstDefnMap),
    ImpInstDefns = inst_ctor_defn_map_to_inst_defns(ImpInstDefnMap),
    acc_ims_list(IntItemMercuryStatus, IntInstDefns,
        AccInstDefns0, AccInstDefns1),
    acc_ims_list(ImpItemMercuryStatus, ImpInstDefns,
        AccInstDefns1, AccInstDefns),
    IntModeDefns = mode_ctor_defn_map_to_mode_defns(IntModeDefnMap),
    ImpModeDefns = mode_ctor_defn_map_to_mode_defns(ImpModeDefnMap),
    acc_ims_list(IntItemMercuryStatus, IntModeDefns,
        AccModeDefns0, AccModeDefns1),
    acc_ims_list(ImpItemMercuryStatus, ImpModeDefns,
        AccModeDefns1, AccModeDefns),
    acc_sec_list(IntSectionInfo, IntTypeClasses,
        AccTypeClasses0, AccTypeClasses1),
    acc_sec_list(ImpSectionInfo, ImpTypeClasses,
        AccTypeClasses1, AccTypeClasses),
    acc_ims_list(IntItemMercuryStatus, IntInstances,
        AccInstances0, AccInstances1),
    acc_ims_list(ImpItemMercuryStatus, ImpInstances,
        AccInstances1, AccInstances),
    acc_sec_list(IntSectionInfo, IntPredDecls, AccPredDecls0, AccPredDecls1),
    acc_sec_list(ImpSectionInfo, ImpPredDecls, AccPredDecls1, AccPredDecls),
    acc_ims_list(IntItemMercuryStatus, IntModeDecls,
        AccModeDecls0, AccModeDecls1),
    acc_ims_list(ImpItemMercuryStatus, ImpModeDecls,
        AccModeDecls1, AccModeDecls),
    acc_ims_tuple_list(ImpItemMercuryStatus,
        cjcs_map_to_list(ImpForeignEnumMap),
        AccForeignEnums0, AccForeignEnums),
    acc_ims_list(IntItemMercuryStatus, IntDeclPragmas,
        AccDeclPragmas0, AccDeclPragmas1),
    acc_ims_list(ImpItemMercuryStatus, ImpDeclPragmas,
        AccDeclPragmas1, AccDeclPragmas),
    acc_ims_list(IntItemMercuryStatus, IntPromises,
        AccPromises0, AccPromises1),
    acc_ims_list(ImpItemMercuryStatus, ImpPromises,
        AccPromises1, AccPromises),

    !Acc ^ ia_avails := AccAvails,
    !Acc ^ ia_fims := AccFIMs,
    !Acc ^ ia_type_defns_abs := AccTypeDefnsAbs,
    !Acc ^ ia_type_defns_mer := AccTypeDefnsMer,
    !Acc ^ ia_type_defns_for := AccTypeDefnsFor,
    !Acc ^ ia_inst_defns := AccInstDefns,
    !Acc ^ ia_mode_defns := AccModeDefns,
    !Acc ^ ia_typeclasses := AccTypeClasses,
    !Acc ^ ia_instances := AccInstances,
    !Acc ^ ia_pred_decls := AccPredDecls,
    !Acc ^ ia_mode_decls := AccModeDecls,
    !Acc ^ ia_foreign_enums := AccForeignEnums,
    !Acc ^ ia_decl_pragmas := AccDeclPragmas,
    !Acc ^ ia_promises := AccPromises.

%---------------------%

:- pred acc_parse_tree_int1(parse_tree_int1::in,
    read_why_int1::in, item_accumulator::in, item_accumulator::out) is det.

acc_parse_tree_int1(ParseTreeInt1, ReadWhy1, !Acc) :-
    (
        (
            ReadWhy1 = rwi1_int_import,
            IntImportLocn = import_locn_interface,
            IntNeedQual = may_be_unqualified
        ;
            ReadWhy1 = rwi1_int_use,
            IntImportLocn = import_locn_interface,
            IntNeedQual = must_be_qualified
        ;
            ReadWhy1 = rwi1_imp_import,
            IntImportLocn = import_locn_implementation,
            IntNeedQual = may_be_unqualified
        ;
            ReadWhy1 = rwi1_imp_use,
            IntImportLocn = import_locn_implementation,
            IntNeedQual = must_be_qualified
        ;
            ReadWhy1 = rwi1_int_use_imp_import,
            IntImportLocn = import_locn_interface,
            IntNeedQual = may_be_unqualified
            % XXX ITEM_LIST Either this should be something like
            % must_be_qualified_in_interface, or the job of IntNeedQual should
            % be split into, with IntNeedQualInInterface = must_be_qualified,
            % and IntNeedQualInImplementation = may_be_unqualified.
        ;
            ReadWhy1 = rwi1_type_repn,
            % This ReadWhy1 should occur only in type_repn_specs,
            % which are processed separately by acc_type_repn_spec.
            unexpected($pred, "rwi1_type_repn") 
        ),
        IntItemImport = item_import_int_concrete(IntImportLocn),
        ImpItemImport = item_import_int_abstract,
        ImpNeedQual = must_be_qualified
    ;
        ReadWhy1 = rwi1_opt,
        IntItemImport = item_import_opt_int,
        ImpItemImport = item_import_opt_int,
        IntNeedQual = must_be_qualified,
        ImpNeedQual = must_be_qualified
    ),
    IntItemMercuryStatus = item_defined_in_other_module(IntItemImport),
    ImpItemMercuryStatus = item_defined_in_other_module(ImpItemImport),
    IntSectionInfo = sec_info(IntItemMercuryStatus, IntNeedQual),
    ImpSectionInfo = sec_info(ImpItemMercuryStatus, ImpNeedQual),

    ParseTreeInt1 = parse_tree_int1(ModuleName, _ModuleNameContext,
        _MaybeVersionNumbers, _IntInclMap, _ImpInclMap, _InclMap,
        _IntUseMap, _ImpUseMap, ImportUseMap, IntFIMSpecs, ImpFIMSpecs,
        IntTypeDefnMap, IntInstDefnMap, IntModeDefnMap,
        IntTypeClasses, IntInstances, IntPredDecls, IntModeDecls,
        IntDeclPragmas, IntPromises, IntTypeRepnMap,
        ImpTypeDefnMap, ImpForeignEnumMap, ImpTypeClasses),

    AccAvails0 = !.Acc ^ ia_avails,
    AccFIMs0 = !.Acc ^ ia_fims,
    AccTypeDefnsAbs0 = !.Acc ^ ia_type_defns_abs,
    AccTypeDefnsMer0 = !.Acc ^ ia_type_defns_mer,
    AccTypeDefnsFor0 = !.Acc ^ ia_type_defns_for,
    AccInstDefns0 = !.Acc ^ ia_inst_defns,
    AccModeDefns0 = !.Acc ^ ia_mode_defns,
    AccTypeClasses0 = !.Acc ^ ia_typeclasses,
    AccInstances0 = !.Acc ^ ia_instances,
    AccPredDecls0 = !.Acc ^ ia_pred_decls,
    AccModeDecls0 = !.Acc ^ ia_mode_decls,
    AccForeignEnums0 = !.Acc ^ ia_foreign_enums,
    AccDeclPragmas0 = !.Acc ^ ia_decl_pragmas,
    AccPromises0 = !.Acc ^ ia_promises,
    AccTypeRepns0 = !.Acc ^ ia_type_repns,

    import_and_or_use_map_to_item_avails(do_not_include_implicit,
        ImportUseMap, IntAvails, ImpAvails),
    acc_ims_avails(IntItemMercuryStatus, IntAvails, AccAvails0, AccAvails1),
    acc_ims_avails(ImpItemMercuryStatus, ImpAvails, AccAvails1, AccAvails),
    IntFIMs = list.map(fim_spec_to_item, set.to_sorted_list(IntFIMSpecs)),
    ImpFIMs = list.map(fim_spec_to_item, set.to_sorted_list(ImpFIMSpecs)),
    AccFIMs = AccFIMs0 ++ cord.from_list(IntFIMs) ++ cord.from_list(ImpFIMs),
    IntTypeDefns = type_ctor_defn_map_to_type_defns(IntTypeDefnMap),
    ImpTypeDefns = type_ctor_defn_map_to_type_defns(ImpTypeDefnMap),
    separate_type_defns_abs_mer_for(IntTypeDefns,
        [], IntTypeDefnsAbs, [], IntTypeDefnsMer, [], IntTypeDefnsFor),
    separate_type_defns_abs_mer_for(ImpTypeDefns,
        [], ImpTypeDefnsAbs, [], ImpTypeDefnsMer, [], ImpTypeDefnsFor),
    acc_sec_list(IntSectionInfo, IntTypeDefnsAbs,
        AccTypeDefnsAbs0, AccTypeDefnsAbs1),
    acc_sec_list(ImpSectionInfo, ImpTypeDefnsAbs,
        AccTypeDefnsAbs1, AccTypeDefnsAbs),
    acc_sec_list(IntSectionInfo, IntTypeDefnsMer,
        AccTypeDefnsMer0, AccTypeDefnsMer1),
    acc_sec_list(ImpSectionInfo, ImpTypeDefnsMer,
        AccTypeDefnsMer1, AccTypeDefnsMer),
    acc_sec_list(IntSectionInfo, IntTypeDefnsFor,
        AccTypeDefnsFor0, AccTypeDefnsFor1),
    acc_sec_list(ImpSectionInfo, ImpTypeDefnsFor,
        AccTypeDefnsFor1, AccTypeDefnsFor),
    InstDefns = inst_ctor_defn_map_to_inst_defns(IntInstDefnMap),
    acc_ims_list(IntItemMercuryStatus, InstDefns, AccInstDefns0, AccInstDefns),
    ModeDefns = mode_ctor_defn_map_to_mode_defns(IntModeDefnMap),
    acc_ims_list(IntItemMercuryStatus, ModeDefns, AccModeDefns0, AccModeDefns),
    acc_sec_list(IntSectionInfo, IntTypeClasses,
        AccTypeClasses0, AccTypeClasses1),
    acc_sec_list(ImpSectionInfo, ImpTypeClasses,
        AccTypeClasses1, AccTypeClasses),
    acc_ims_list(IntItemMercuryStatus, IntInstances,
        AccInstances0, AccInstances),
    acc_sec_list(IntSectionInfo, IntPredDecls, AccPredDecls0, AccPredDecls),
    acc_ims_list(IntItemMercuryStatus, IntModeDecls,
        AccModeDecls0, AccModeDecls),
    acc_ims_tuple_list(ImpItemMercuryStatus,
        cjcs_map_to_list(ImpForeignEnumMap),
        AccForeignEnums0, AccForeignEnums),
    acc_ims_list(IntItemMercuryStatus, IntDeclPragmas,
        AccDeclPragmas0, AccDeclPragmas),
    acc_ims_list(IntItemMercuryStatus, IntPromises, AccPromises0, AccPromises),
    AccTypeRepns = [ModuleName - int_type_ctor_repns(ifk_int1, IntTypeRepnMap)
        | AccTypeRepns0],

    !Acc ^ ia_avails := AccAvails,
    !Acc ^ ia_fims := AccFIMs,
    !Acc ^ ia_type_defns_abs := AccTypeDefnsAbs,
    !Acc ^ ia_type_defns_mer := AccTypeDefnsMer,
    !Acc ^ ia_type_defns_for := AccTypeDefnsFor,
    !Acc ^ ia_inst_defns := AccInstDefns,
    !Acc ^ ia_mode_defns := AccModeDefns,
    !Acc ^ ia_typeclasses := AccTypeClasses,
    !Acc ^ ia_instances := AccInstances,
    !Acc ^ ia_pred_decls := AccPredDecls,
    !Acc ^ ia_mode_decls := AccModeDecls,
    !Acc ^ ia_foreign_enums := AccForeignEnums,
    !Acc ^ ia_decl_pragmas := AccDeclPragmas,
    !Acc ^ ia_promises := AccPromises,
    !Acc ^ ia_type_repns := AccTypeRepns.

%---------------------%

:- pred acc_parse_tree_int2(parse_tree_int2::in,
    read_why_int2::in, item_accumulator::in, item_accumulator::out) is det.

acc_parse_tree_int2(ParseTreeInt2, ReadWhy2, !Acc) :-
    (
        (
            ReadWhy2 = rwi2_int_use,
            IntImportLocn = import_locn_interface
        ;
            ReadWhy2 = rwi2_imp_use,
            IntImportLocn = import_locn_implementation
        ),
        IntItemImport = item_import_int_concrete(IntImportLocn),
        ImpItemImport = item_import_int_abstract
    ;
        ReadWhy2 = rwi2_abstract,
        IntItemImport = item_import_int_abstract,
        ImpItemImport = item_import_int_abstract
    ;
        ReadWhy2 = rwi2_opt,
        IntItemImport = item_import_opt_int,
        ImpItemImport = item_import_opt_int
    ),
    IntItemMercuryStatus = item_defined_in_other_module(IntItemImport),
    ImpItemMercuryStatus = item_defined_in_other_module(ImpItemImport),
    IntNeedQual = must_be_qualified,
    ImpNeedQual = must_be_qualified,
    IntSectionInfo = sec_info(IntItemMercuryStatus, IntNeedQual),
    ImpSectionInfo = sec_info(ImpItemMercuryStatus, ImpNeedQual),

    ParseTreeInt2 = parse_tree_int2(ModuleName, _ModuleNameContext,
        _MaybeVersionNumbers, _IntInclMap, _InclMap,
        _IntUseMap, ImportUseMap, IntFIMSpecs, ImpFIMSpecs,
        IntTypeDefnMap, IntInstDefnMap, IntModeDefnMap,
        IntTypeClasses, IntInstances, IntTypeRepnMap,
        ImpTypeDefnMap),

    AccAvails0 = !.Acc ^ ia_avails,
    AccFIMs0 = !.Acc ^ ia_fims,
    AccTypeDefnsAbs0 = !.Acc ^ ia_type_defns_abs,
    AccTypeDefnsMer0 = !.Acc ^ ia_type_defns_mer,
    AccTypeDefnsFor0 = !.Acc ^ ia_type_defns_for,
    AccInstDefns0 = !.Acc ^ ia_inst_defns,
    AccModeDefns0 = !.Acc ^ ia_mode_defns,
    AccTypeClasses0 = !.Acc ^ ia_typeclasses,
    AccInstances0 = !.Acc ^ ia_instances,
    AccTypeRepns0 = !.Acc ^ ia_type_repns,

    import_and_or_use_map_to_item_avails(do_not_include_implicit,
        ImportUseMap, IntAvails, ImpAvails),
    expect(unify(ImpAvails, []), $pred, "ImpAvails != []"),
    acc_ims_avails(IntItemMercuryStatus, IntAvails, AccAvails0, AccAvails),
    IntFIMs = list.map(fim_spec_to_item, set.to_sorted_list(IntFIMSpecs)),
    ImpFIMs = list.map(fim_spec_to_item, set.to_sorted_list(ImpFIMSpecs)),
    AccFIMs = AccFIMs0 ++ cord.from_list(IntFIMs) ++ cord.from_list(ImpFIMs),
    IntTypeDefns = type_ctor_defn_map_to_type_defns(IntTypeDefnMap),
    ImpTypeDefns = type_ctor_defn_map_to_type_defns(ImpTypeDefnMap),
    separate_type_defns_abs_mer_for(IntTypeDefns,
        [], IntTypeDefnsAbs, [], IntTypeDefnsMer, [], IntTypeDefnsFor),
    separate_type_defns_abs_mer_for(ImpTypeDefns,
        [], ImpTypeDefnsAbs, [], ImpTypeDefnsMer, [], ImpTypeDefnsFor),
    acc_sec_list(IntSectionInfo, IntTypeDefnsAbs,
        AccTypeDefnsAbs0, AccTypeDefnsAbs1),
    acc_sec_list(ImpSectionInfo, ImpTypeDefnsAbs,
        AccTypeDefnsAbs1, AccTypeDefnsAbs),
    acc_sec_list(IntSectionInfo, IntTypeDefnsMer,
        AccTypeDefnsMer0, AccTypeDefnsMer1),
    acc_sec_list(ImpSectionInfo, ImpTypeDefnsMer,
        AccTypeDefnsMer1, AccTypeDefnsMer),
    acc_sec_list(IntSectionInfo, IntTypeDefnsFor,
        AccTypeDefnsFor0, AccTypeDefnsFor1),
    acc_sec_list(ImpSectionInfo, ImpTypeDefnsFor,
        AccTypeDefnsFor1, AccTypeDefnsFor),
    InstDefns = inst_ctor_defn_map_to_inst_defns(IntInstDefnMap),
    acc_ims_list(IntItemMercuryStatus, InstDefns, AccInstDefns0, AccInstDefns),
    ModeDefns = mode_ctor_defn_map_to_mode_defns(IntModeDefnMap),
    acc_ims_list(IntItemMercuryStatus, ModeDefns, AccModeDefns0, AccModeDefns),
    acc_sec_list(IntSectionInfo, IntTypeClasses,
        AccTypeClasses0, AccTypeClasses),
    acc_ims_list(IntItemMercuryStatus, IntInstances,
        AccInstances0, AccInstances),
    AccTypeRepns = [ModuleName - int_type_ctor_repns(ifk_int2, IntTypeRepnMap)
        | AccTypeRepns0],

    !Acc ^ ia_avails := AccAvails,
    !Acc ^ ia_fims := AccFIMs,
    !Acc ^ ia_type_defns_abs := AccTypeDefnsAbs,
    !Acc ^ ia_type_defns_mer := AccTypeDefnsMer,
    !Acc ^ ia_type_defns_for := AccTypeDefnsFor,
    !Acc ^ ia_inst_defns := AccInstDefns,
    !Acc ^ ia_mode_defns := AccModeDefns,
    !Acc ^ ia_typeclasses := AccTypeClasses,
    !Acc ^ ia_instances := AccInstances,
    !Acc ^ ia_type_repns := AccTypeRepns.

%---------------------%

:- pred acc_parse_tree_int3(parse_tree_int3::in,
    read_why_int3::in, item_accumulator::in, item_accumulator::out) is det.

acc_parse_tree_int3(ParseTreeInt3, ReadWhy3, !Acc) :-
    (
        ReadWhy3 = rwi3_direct_ancestor_import,
        IntImportLocn = import_locn_import_by_ancestor,
        IntNeedQual = may_be_unqualified
    ;
        ReadWhy3 = rwi3_direct_int_import,
        IntImportLocn = import_locn_interface,
        IntNeedQual = may_be_unqualified
    ;
        ReadWhy3 = rwi3_direct_imp_import,
        IntImportLocn = import_locn_implementation,
        IntNeedQual = may_be_unqualified
    ;
        ReadWhy3 = rwi3_direct_ancestor_use,
        IntImportLocn = import_locn_import_by_ancestor,
        IntNeedQual = must_be_qualified
    ;
        ReadWhy3 = rwi3_direct_int_use,
        IntImportLocn = import_locn_interface,
        IntNeedQual = must_be_qualified
    ;
        ReadWhy3 = rwi3_direct_imp_use,
        IntImportLocn = import_locn_implementation,
        IntNeedQual = must_be_qualified
    ;
        ReadWhy3 = rwi3_direct_int_use_imp_import,
        IntImportLocn = import_locn_interface,
        IntNeedQual = may_be_unqualified
        % XXX ITEM_LIST Either this should be something like
        % must_be_qualified_in_interface, or the job of IntNeedQual should
        % be split into, with IntNeedQualInInterface = must_be_qualified,
        % and IntNeedQualInImplementation = may_be_unqualified.
    ;
        ReadWhy3 = rwi3_indirect_int_use,
        IntImportLocn = import_locn_interface,
        IntNeedQual = must_be_qualified
        % XXX ITEM_LIST We should use only the subset of the items in this file
        % that caused it to be indirectly imported.
    ;
        ReadWhy3 = rwi3_indirect_imp_use,
        IntImportLocn = import_locn_implementation,
        IntNeedQual = must_be_qualified
        % XXX ITEM_LIST We should use only the subset of the items in this file
        % that caused it to be indirectly imported.
    ),
    IntItemImport = item_import_int_concrete(IntImportLocn),
    IntItemMercuryStatus = item_defined_in_other_module(IntItemImport),
    IntSectionInfo = sec_info(IntItemMercuryStatus, IntNeedQual),

    ParseTreeInt3 = parse_tree_int3(ModuleName, _ModuleNameContext,
        _IntInclMap, _InclMap, _IntImportMap, ImportUseMap,
        IntTypeDefnMap, IntInstDefnMap, IntModeDefnMap,
        IntTypeClasses, IntInstances, IntTypeRepnMap),

    AccAvails0 = !.Acc ^ ia_avails,
    AccTypeDefnsAbs0 = !.Acc ^ ia_type_defns_abs,
    AccTypeDefnsMer0 = !.Acc ^ ia_type_defns_mer,
    AccTypeDefnsFor0 = !.Acc ^ ia_type_defns_for,
    AccInstDefns0 = !.Acc ^ ia_inst_defns,
    AccModeDefns0 = !.Acc ^ ia_mode_defns,
    AccTypeClasses0 = !.Acc ^ ia_typeclasses,
    AccInstances0 = !.Acc ^ ia_instances,
    AccTypeRepns0 = !.Acc ^ ia_type_repns,

    import_and_or_use_map_to_item_avails(do_not_include_implicit,
        ImportUseMap, IntAvails, ImpAvails),
    expect(unify(ImpAvails, []), $pred, "ImpAvails != []"),
    acc_ims_avails(IntItemMercuryStatus, IntAvails, AccAvails0, AccAvails),
    % XXX IntTypeDefnMap has the different kind of type definitions
    % already separated. However, taking advantage of that fact
    % to optimize the separation would be unwise, since the *proper* fix
    % is to merge IntTypeDefnMap into a single type_ctor_checked_map
    % extended with section information.
    TypeDefns = type_ctor_defn_map_to_type_defns(IntTypeDefnMap),
    separate_type_defns_abs_mer_for(TypeDefns,
        [], TypeDefnsAbs, [], TypeDefnsMer, [], TypeDefnsFor),
    acc_sec_list(IntSectionInfo, TypeDefnsAbs,
        AccTypeDefnsAbs0, AccTypeDefnsAbs),
    acc_sec_list(IntSectionInfo, TypeDefnsMer,
        AccTypeDefnsMer0, AccTypeDefnsMer),
    acc_sec_list(IntSectionInfo, TypeDefnsFor,
        AccTypeDefnsFor0, AccTypeDefnsFor),
    InstDefns = inst_ctor_defn_map_to_inst_defns(IntInstDefnMap),
    acc_ims_list(IntItemMercuryStatus, InstDefns, AccInstDefns0, AccInstDefns),
    ModeDefns = mode_ctor_defn_map_to_mode_defns(IntModeDefnMap),
    acc_ims_list(IntItemMercuryStatus, ModeDefns, AccModeDefns0, AccModeDefns),
    acc_sec_list(IntSectionInfo, IntTypeClasses,
        AccTypeClasses0, AccTypeClasses),
    acc_ims_list(IntItemMercuryStatus, IntInstances,
        AccInstances0, AccInstances),
    AccTypeRepns = [ModuleName - int_type_ctor_repns(ifk_int3, IntTypeRepnMap)
        | AccTypeRepns0],

    !Acc ^ ia_avails := AccAvails,
    !Acc ^ ia_type_defns_abs := AccTypeDefnsAbs,
    !Acc ^ ia_type_defns_mer := AccTypeDefnsMer,
    !Acc ^ ia_type_defns_for := AccTypeDefnsFor,
    !Acc ^ ia_inst_defns := AccInstDefns,
    !Acc ^ ia_mode_defns := AccModeDefns,
    !Acc ^ ia_typeclasses := AccTypeClasses,
    !Acc ^ ia_instances := AccInstances,
    !Acc ^ ia_type_repns := AccTypeRepns.

%---------------------%

:- pred acc_parse_tree_plain_opt(parse_tree_plain_opt::in,
    item_accumulator::in, item_accumulator::out) is det.

acc_parse_tree_plain_opt(ParseTreePlainOpt, !Acc) :-
    ItemMercuryStatus = item_defined_in_other_module(item_import_opt_int),
    SectionInfo = sec_info(ItemMercuryStatus, must_be_qualified),

    ParseTreePlainOpt = parse_tree_plain_opt(_ModuleName, _ModuleNameContext,
        UseMap, FIMSpecs, TypeDefns, ForeignEnums,
        InstDefns, ModeDefns, TypeClasses, Instances,
        PredDecls, ModeDecls, Clauses, ForeignProcs, Promises,
        MarkerPragmas, TypeSpecs, UnusedArgs, TermInfos, Term2Infos,
        Exceptions, Trailings, MMTablings, Sharings, Reuses),

    AccAvails0 = !.Acc ^ ia_avails,
    AccFIMs0 = !.Acc ^ ia_fims,
    AccTypeDefnsAbs0 = !.Acc ^ ia_type_defns_abs,
    AccTypeDefnsMer0 = !.Acc ^ ia_type_defns_mer,
    AccTypeDefnsFor0 = !.Acc ^ ia_type_defns_for,
    AccInstDefns0 = !.Acc ^ ia_inst_defns,
    AccModeDefns0 = !.Acc ^ ia_mode_defns,
    AccTypeClasses0 = !.Acc ^ ia_typeclasses,
    AccInstances0 = !.Acc ^ ia_instances,
    AccPredDecls0 = !.Acc ^ ia_pred_decls,
    AccModeDecls0 = !.Acc ^ ia_mode_decls,
    AccClauses0 = !.Acc ^ ia_clauses,
    AccForeignEnums0 = !.Acc ^ ia_foreign_enums,
    AccDeclPragmas0 = !.Acc ^ ia_decl_pragmas,
    AccImplPragmas0 = !.Acc ^ ia_impl_pragmas,
    AccGenPragmas0 = !.Acc ^ ia_gen_pragmas,
    AccPromises0 = !.Acc ^ ia_promises,

    OptAvails = use_map_to_item_avails(UseMap),
    acc_ims_avails(ItemMercuryStatus, OptAvails, AccAvails0, AccAvails),
    OptFIMs = list.map(fim_spec_to_item, set.to_sorted_list(FIMSpecs)),
    AccFIMs = AccFIMs0 ++ cord.from_list(OptFIMs),
    separate_type_defns_abs_mer_for(TypeDefns,
        [], TypeDefnsAbs, [], TypeDefnsMer, [], TypeDefnsFor),
    acc_sec_list(SectionInfo, TypeDefnsAbs,
        AccTypeDefnsAbs0, AccTypeDefnsAbs),
    acc_sec_list(SectionInfo, TypeDefnsMer,
        AccTypeDefnsMer0, AccTypeDefnsMer),
    acc_sec_list(SectionInfo, TypeDefnsFor,
        AccTypeDefnsFor0, AccTypeDefnsFor),
    acc_ims_list(ItemMercuryStatus, InstDefns, AccInstDefns0, AccInstDefns),
    acc_ims_list(ItemMercuryStatus, ModeDefns, AccModeDefns0, AccModeDefns),
    acc_sec_list(SectionInfo, TypeClasses, AccTypeClasses0, AccTypeClasses),
    acc_ims_list(ItemMercuryStatus, Instances, AccInstances0, AccInstances),
    acc_sec_list(SectionInfo, PredDecls, AccPredDecls0, AccPredDecls),
    acc_ims_list(ItemMercuryStatus, ModeDecls, AccModeDecls0, AccModeDecls),
    acc_ims_list(ItemMercuryStatus, Clauses, AccClauses0, AccClauses),
    acc_ims_tuple_list(ItemMercuryStatus, ForeignEnums,
        AccForeignEnums0, AccForeignEnums),
    acc_pred_marker_pragmas(MarkerPragmas,
        [], DeclMarkerPragmas, [], ImplMarkerPragmas),
    OptDeclPragmas =
        list.map(wrap_type_spec_pragma, TypeSpecs) ++
        list.map(wrap_termination_pragma, TermInfos) ++
        list.map(wrap_termination2_pragma, Term2Infos) ++
        list.map(wrap_struct_sharing_pragma, Sharings) ++
        list.map(wrap_struct_reuse_pragma, Reuses) ++
        DeclMarkerPragmas,
    acc_ims_list(ItemMercuryStatus, OptDeclPragmas,
        AccDeclPragmas0, AccDeclPragmas),
    OptImplPragmas =
        list.map(wrap_foreign_proc, ForeignProcs) ++
        ImplMarkerPragmas,
    acc_ims_list(ItemMercuryStatus, OptImplPragmas,
        AccImplPragmas0, AccImplPragmas),
    OptGenPragmas =
        list.map(wrap_unused_args_pragma, UnusedArgs) ++
        list.map(wrap_exceptions_pragma, Exceptions) ++
        list.map(wrap_trailing_pragma, Trailings) ++
        list.map(wrap_mm_tabling_pragma, MMTablings),
    AccGenPragmas = AccGenPragmas0 ++ cord.from_list(OptGenPragmas),
    acc_ims_list(ItemMercuryStatus, Promises, AccPromises0, AccPromises),

    !Acc ^ ia_avails := AccAvails,
    !Acc ^ ia_fims := AccFIMs,
    !Acc ^ ia_type_defns_abs := AccTypeDefnsAbs,
    !Acc ^ ia_type_defns_mer := AccTypeDefnsMer,
    !Acc ^ ia_type_defns_for := AccTypeDefnsFor,
    !Acc ^ ia_inst_defns := AccInstDefns,
    !Acc ^ ia_mode_defns := AccModeDefns,
    !Acc ^ ia_typeclasses := AccTypeClasses,
    !Acc ^ ia_instances := AccInstances,
    !Acc ^ ia_pred_decls := AccPredDecls,
    !Acc ^ ia_mode_decls := AccModeDecls,
    !Acc ^ ia_clauses := AccClauses,
    !Acc ^ ia_foreign_enums := AccForeignEnums,
    !Acc ^ ia_decl_pragmas := AccDeclPragmas,
    !Acc ^ ia_impl_pragmas := AccImplPragmas,
    !Acc ^ ia_gen_pragmas := AccGenPragmas,
    !Acc ^ ia_promises := AccPromises.

%---------------------%

:- pred acc_parse_tree_trans_opt(parse_tree_trans_opt::in,
    item_accumulator::in, item_accumulator::out) is det.

acc_parse_tree_trans_opt(ParseTreeTransOpt, !Acc) :-
    ParseTreeTransOpt = parse_tree_trans_opt(_ModuleName, _ModuleNameContext,
        TermInfos, Term2Infos, Exceptions, Trailings, MMTablings,
        Sharings, Reuses),
    ItemMercuryStatus = item_defined_in_other_module(item_import_opt_int),

    AccDeclPragmas0 = !.Acc ^ ia_decl_pragmas,
    AccGenPragmas0 = !.Acc ^ ia_gen_pragmas,

    OptDeclPragmas =
        list.map(wrap_termination_pragma, TermInfos) ++
        list.map(wrap_termination2_pragma, Term2Infos) ++
        list.map(wrap_struct_sharing_pragma, Sharings) ++
        list.map(wrap_struct_reuse_pragma, Reuses),
    acc_ims_list(ItemMercuryStatus, OptDeclPragmas,
        AccDeclPragmas0, AccDeclPragmas),
    OptGenPragmas =
        list.map(wrap_exceptions_pragma, Exceptions) ++
        list.map(wrap_trailing_pragma, Trailings) ++
        list.map(wrap_mm_tabling_pragma, MMTablings),
    AccGenPragmas = AccGenPragmas0 ++ cord.from_list(OptGenPragmas),

    !Acc ^ ia_decl_pragmas := AccDeclPragmas,
    !Acc ^ ia_gen_pragmas := AccGenPragmas.

%---------------------------------------------------------------------------%

:- pred acc_ims_avails(item_mercury_status::in, list(item_avail)::in,
    ims_cord(item_avail)::in, ims_cord(item_avail)::out) is det.

acc_ims_avails(ItemMercuryStatus, Avails, !AccAvails) :-
    (
        Avails = []
    ;
        Avails = [_ | _],
        ImsSubList = ims_sub_list(ItemMercuryStatus, Avails),
        cord.snoc(ImsSubList, !AccAvails)
    ).

%---------------------------------------------------------------------------%

:- pred separate_type_defns_abs_mer_for(list(item_type_defn_info)::in,
    list(item_type_defn_info)::in, list(item_type_defn_info)::out,
    list(item_type_defn_info)::in, list(item_type_defn_info)::out,
    list(item_type_defn_info)::in, list(item_type_defn_info)::out) is det.

separate_type_defns_abs_mer_for([], !Abs, !Mer, !For).
separate_type_defns_abs_mer_for([Item | Items], !Abs, !Mer, !For) :-
    separate_type_defns_abs_mer_for(Items, !Abs, !Mer, !For),
    Item = item_type_defn_info(_, _, TypeDefn, _, _, _),
    (
        TypeDefn = parse_tree_abstract_type(_),
        !:Abs = [Item | !.Abs]
    ;
        ( TypeDefn = parse_tree_du_type(_)
        ; TypeDefn = parse_tree_eqv_type(_)
        ; TypeDefn = parse_tree_solver_type(_)
        ),
        !:Mer = [Item | !.Mer]
    ;
        TypeDefn = parse_tree_foreign_type(_),
        !:For = [Item | !.For]
    ).

%---------------------------------------------------------------------------%

:- pred acc_pred_marker_pragmas(list(item_pred_marker)::in,
    list(item_decl_pragma_info)::in, list(item_decl_pragma_info)::out,
    list(item_impl_pragma_info)::in, list(item_impl_pragma_info)::out) is det.

acc_pred_marker_pragmas([], !DeclPragmas, !ImplPragmas).
acc_pred_marker_pragmas([ItemMarker | ItemMarkers],
        !DeclPragmas, !ImplPragmas) :-
    acc_pred_marker_pragmas(ItemMarkers, !DeclPragmas, !ImplPragmas),
    Item = wrap_marker_pragma_item(ItemMarker),
    (
        Item = item_decl_pragma(DeclPragma),
        !:DeclPragmas = [DeclPragma | !.DeclPragmas]
    ;
        Item = item_impl_pragma(ImplPragma),
        !:ImplPragmas = [ImplPragma | !.ImplPragmas]
    ).

%---------------------------------------------------------------------------%

    % These functions differ from similar functions in item_util.m in that
    % they yield an item_decl_pragma_info or item_generated_pragma_info;
    % they do not convert the resulting pragma into a general item.

:- func wrap_type_spec_pragma(item_type_spec) = item_decl_pragma_info.
:- func wrap_termination_pragma(item_termination) = item_decl_pragma_info.
:- func wrap_termination2_pragma(item_termination2) = item_decl_pragma_info.
:- func wrap_struct_sharing_pragma(item_struct_sharing)
    = item_decl_pragma_info.
:- func wrap_struct_reuse_pragma(item_struct_reuse) = item_decl_pragma_info.

wrap_type_spec_pragma(X) = Item :-
    X = item_pragma_info(Info, Context, SeqNum),
    Item = item_pragma_info(decl_pragma_type_spec(Info), Context, SeqNum).

wrap_termination_pragma(X) = Item :-
    X = item_pragma_info(Info, Context, SeqNum),
    Item = item_pragma_info(decl_pragma_termination_info(Info),
        Context, SeqNum).

wrap_termination2_pragma(X) = Item :-
    X = item_pragma_info(Info, Context, SeqNum),
    Item = item_pragma_info(decl_pragma_termination2_info(Info),
        Context, SeqNum).

wrap_struct_sharing_pragma(X) = Item :-
    X = item_pragma_info(Info, Context, SeqNum),
    Item = item_pragma_info(decl_pragma_structure_sharing(Info),
        Context, SeqNum).

wrap_struct_reuse_pragma(X) = Item :-
    X = item_pragma_info(Info, Context, SeqNum),
    Item = item_pragma_info(decl_pragma_structure_reuse(Info),
        Context, SeqNum).

:- func wrap_foreign_proc(item_foreign_proc) = item_impl_pragma_info.

wrap_foreign_proc(X) = Item :-
    X = item_pragma_info(Info, Context, SeqNum),
    Item = item_pragma_info(impl_pragma_foreign_proc(Info), Context, SeqNum).

:- func wrap_unused_args_pragma(item_unused_args) = item_generated_pragma_info.
:- func wrap_exceptions_pragma(item_exceptions) = item_generated_pragma_info.
:- func wrap_trailing_pragma(item_trailing) = item_generated_pragma_info.
:- func wrap_mm_tabling_pragma(item_mm_tabling) = item_generated_pragma_info.

wrap_unused_args_pragma(X) = Item :-
    X = item_pragma_info(Info, Context, SeqNum),
    Item = item_pragma_info(gen_pragma_unused_args(Info), Context, SeqNum).

wrap_exceptions_pragma(X) = Item :-
    X = item_pragma_info(Info, Context, SeqNum),
    Item = item_pragma_info(gen_pragma_exceptions(Info), Context, SeqNum).

wrap_trailing_pragma(X) = Item :-
    X = item_pragma_info(Info, Context, SeqNum),
    Item = item_pragma_info(gen_pragma_trailing_info(Info), Context, SeqNum).

wrap_mm_tabling_pragma(X) = Item :-
    X = item_pragma_info(Info, Context, SeqNum),
    Item = item_pragma_info(gen_pragma_mm_tabling_info(Info), Context, SeqNum).

%---------------------------------------------------------------------------%

:- pred acc_sec_list(sec_info::in, list(T)::in,
    sec_cord(T)::in, sec_cord(T)::out) is det.

acc_sec_list(SectionInfo, Items, !SecCord) :-
    (
        Items = []
    ;
        Items = [_ | _],
        SecSubList = sec_sub_list(SectionInfo, Items),
        cord.snoc(SecSubList, !SecCord)
    ).

%---------------------%

:- pred acc_ims_list(item_mercury_status::in, list(T)::in,
    ims_cord(T)::in, ims_cord(T)::out) is det.

acc_ims_list(ItemMercuryStatus, Items, !ImsCord) :-
    (
        Items = []
    ;
        Items = [_ | _],
        ImsSubList = ims_sub_list(ItemMercuryStatus, Items),
        cord.snoc(ImsSubList, !ImsCord)
    ).

%---------------------%

:- pred acc_ims_tuple_list(item_mercury_status::in, list(T)::in,
    ims_tuple_cord(T)::in, ims_tuple_cord(T)::out) is det.

acc_ims_tuple_list(_ItemMercuryStatus, [], !ImsItems).
acc_ims_tuple_list(ItemMercuryStatus, [Item | Items], !ImsItems) :-
    cord.snoc({ItemMercuryStatus, Item}, !ImsItems),
    acc_ims_tuple_list(ItemMercuryStatus, Items, !ImsItems).

%---------------------------------------------------------------------------%

:- func cjcs_map_to_list(map(_K, c_java_csharp(list(V)))) = list(V).

cjcs_map_to_list(Map) = List :-
    RevList0 = [],
    map.foldl_values(acc_cjcs_map_to_list, Map, RevList0, RevList),
    list.reverse(RevList, List).

:- pred acc_cjcs_map_to_list(c_java_csharp(list(V))::in,
    list(V)::in, list(V)::out) is det.

acc_cjcs_map_to_list(CJCsE, !RevList) :-
    CJCsE = c_java_csharp(Cs, Javas, Csharps),
    !:RevList = Cs ++ Javas ++ Csharps ++ !.RevList.

%---------------------------------------------------------------------------%
:- end_module hlds.make_hlds.make_hlds_passes.make_hlds_separate_items.
%---------------------------------------------------------------------------%
