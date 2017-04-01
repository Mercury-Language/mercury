%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: split_parse_tree_src.m.
% Main author: zs.

:- module parse_tree.split_parse_tree_src.
:- interface.

:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_item.

:- import_module list.

    % Given the parse tree of a source module that may contain submodules,
    % split it into a list of one or more compilation units; one for the
    % top level module, and one for each nested submodule. Return these
    % compilation units in top-down order of the submodule's inclusions.
    %
    % Also do some error checking:
    %
    % - report an error if the `implementation' section of a submodule
    %   is contained inside the `interface' section of its parent module;
    %
    % - check for modules declared as both nested and separate submodules;
    %
    % - check for non-abstract typeclass instance declarations in module
    %   interfaces.
    %
:- pred split_into_compilation_units_perform_checks(parse_tree_src::in,
    list(raw_compilation_unit)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module cord.
:- import_module map.
:- import_module maybe.
:- import_module require.

%---------------------------------------------------------------------------%

:- type parent_module_context
    --->    no_parent_top_level
    ;       in_parent_interface
    ;       in_parent_implementation.

:- type split_nested_info
    --->    split_nested_top_module(prog_context)
            % This module is the top level module, and this is the context
            % of its `:- module' declaration.

    ;       split_nested_only_int(prog_context)
            % This module is not the top level module, we have seen
            % only its interface section, and this is the context of the
            % `:- module' declaration of this interface.

    ;       split_nested_only_imp(prog_context)
            % This module is not the top level module, we have seen
            % only its implementation section, and this is the context of the
            % `:- module' declaration of this implementation.

    ;       split_nested_int_imp(prog_context, prog_context).
            % This module is not the top level module, and we have seen
            % both its interface and implementation section,
            % with the two contexts giving the locations of the `:- module'
            % declarations of the interface and implementation parts
            % respectively. If there was a single `:- module' declaration
            % which contained both interface and implementation sections,
            % these two will be the same context.

:- type split_module_entry
    --->    split_included(
                prog_context
                % The module was included by an `:- include_module'
                % declaration at this context.
            )
    ;       split_nested(
                % The module is either the top level module or a directly
                % or indirectly nested submodule. (NOT one referred to
                % by `:- include_module' declaration.)

                split_nested_info,

                cord(raw_item_block)
                % The contents of the module.
            ).

:- type split_module_map == map(module_name, split_module_entry).

:- func split_nested_info_get_context(split_nested_info) = prog_context.

split_nested_info_get_context(SplitNested) = Context :-
    ( SplitNested = split_nested_top_module(Context)
    ; SplitNested = split_nested_only_int(Context)
    ; SplitNested = split_nested_only_imp(Context)
    ; SplitNested = split_nested_int_imp(Context, _)
    ).

% Modules contains sections, and those sections may contain (sub)modules.
%
% A module may be a top-level module in a parse_tree_src, in which case
% it has no parents in the parse tree, so its context is ma_no_parent.
% If a module is not the top-level module in its parse_tree_src, then
% it appears in a particular section, which in turn is contained in another
% module. You can find the identity of the containing section in ma_parent,
% and the identity of the module containing that in sa_parent.

:- type module_ancestors
    --->    ma_no_parent
    ;       ma_parent(
                module_section,     % Which section of its parent module
                                    % does this module appear in?
                prog_context,       % The context of the section.
                section_ancestors
            ).

:- type section_ancestors
    --->    sa_parent(
                module_name,        % Which module does this section appear in?
                module_ancestors
            ).

    % Is this section in the interface section of some ancestor?
    % If yes, return the name of the closest such ancestor.
    %
:- pred section_has_some_ancestor_in_interface(section_ancestors::in,
    maybe(module_name)::out) is det.

section_has_some_ancestor_in_interface(SectionAncestors,
        MaybeProblemAncestor) :-
    SectionAncestors = sa_parent(_ModuleName, ModuleAncestors),
    (
        ModuleAncestors = ma_no_parent,
        MaybeProblemAncestor = no
    ;
        ModuleAncestors = ma_parent(SectionKind, _SectionContext,
            SectionParentAncestors),
        (
            SectionKind = ms_interface,
            SectionParentAncestors = sa_parent(ProblemAncestor, _),
            MaybeProblemAncestor = yes(ProblemAncestor)
        ;
            SectionKind = ms_implementation,
            section_has_some_ancestor_in_interface(SectionParentAncestors,
                MaybeProblemAncestor)
        )
    ).

    % Maps each module to the list of its submodules seen so far.
    % A submodule that is nested into its parent twice (because it has
    % its interface section and implementation inside separate `:- module'/
    % `:- end_module' pairs) will appear twice in the cord.
    %
:- type module_to_submodules_map == map(module_name, cord(module_name)).

:- pred add_new_module_maybe_submodule_to_map(module_ancestors::in,
    module_name::in,
    module_to_submodules_map::in, module_to_submodules_map::out) is det.

add_new_module_maybe_submodule_to_map(ModuleAncestors, ModuleName,
        !SubModulesMap) :-
    (
        ModuleAncestors = ma_no_parent
    ;
        ModuleAncestors =
            ma_parent(_SectionKind, _SectionContext, SectionAncestors),
        add_new_submodule_to_map(SectionAncestors, ModuleName, !SubModulesMap)
    ).

:- pred add_new_submodule_to_map(section_ancestors::in, module_name::in,
    module_to_submodules_map::in, module_to_submodules_map::out) is det.

add_new_submodule_to_map(SectionAncestors, ModuleName, !SubModulesMap) :-
    SectionAncestors = sa_parent(ParentModuleName, _),
    ( if
        map.search(!.SubModulesMap, ParentModuleName, SiblingModules0)
    then
        SiblingModules = cord.snoc(SiblingModules0, ModuleName),
        map.det_update(ParentModuleName, SiblingModules, !SubModulesMap)
    else
        SiblingModules = cord.singleton(ModuleName),
        map.det_insert(ParentModuleName, SiblingModules, !SubModulesMap)
    ).

%---------------------------------------------------------------------------%

split_into_compilation_units_perform_checks(ParseTreeSrc, RawCompUnits,
        !Specs) :-
    split_parse_tree_discover_submodules(ParseTreeSrc, ma_no_parent,
        map.init, SplitModuleMap, map.init, SubModulesMap, !Specs),
    ParseTreeSrc = parse_tree_src(TopModuleName, _, _),
    create_split_compilation_units_depth_first(TopModuleName,
        SplitModuleMap, LeftOverSplitModuleMap,
        SubModulesMap, LeftOverSubModulesMap,
        cord.init, RawCompUnitCord, !Specs),
    expect(unify(LeftOverSplitModuleMap, map.init), $module, $pred,
        "LeftOverSplitModuleMap != map.init"),
    expect(unify(LeftOverSubModulesMap, map.init), $module, $pred,
        "LeftOverSubModulesMap != map.init"),
    RawCompUnits = cord.list(RawCompUnitCord).

%---------------------------------------------------------------------------%

:- pred create_split_compilation_units_depth_first(module_name::in,
    split_module_map::in, split_module_map::out,
    module_to_submodules_map::in, module_to_submodules_map::out,
    cord(raw_compilation_unit)::in, cord(raw_compilation_unit)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

create_split_compilation_units_depth_first(ModuleName,
        !SplitModuleMap, !SubModulesMap, !RawCompUnitsCord, !Specs) :-
    map.det_remove(ModuleName, Entry, !SplitModuleMap),
    (
        Entry = split_included(_),
        map.delete(ModuleName, !SubModulesMap)
    ;
        Entry = split_nested(NestedInfo, RawItemBlockCord),
        (
            ( NestedInfo = split_nested_top_module(Context)
            ; NestedInfo = split_nested_only_int(Context)
            ; NestedInfo = split_nested_int_imp(Context, _)
            ),
            RawItemBlocks = cord.list(RawItemBlockCord),
            check_interface_blocks_for_abstract_instances(RawItemBlocks,
                !Specs),
            RawCompUnit = raw_compilation_unit(ModuleName, Context,
                RawItemBlocks),
            !:RawCompUnitsCord = cord.snoc(!.RawCompUnitsCord, RawCompUnit)
        ;
            NestedInfo = split_nested_only_imp(Context),
            Pieces = [words("Submodule"), qual_sym_name(ModuleName),
                words("is missing its interface section."), nl],
            Msg = simple_msg(Context, [always(Pieces)]),
            Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
            !:Specs = [Spec | !.Specs]
        ),
        ( if map.remove(ModuleName, SubModulesCord, !SubModulesMap) then
            list.sort_and_remove_dups(cord.list(SubModulesCord), SubModules),
            list.foldl4(create_split_compilation_units_depth_first, SubModules,
                !SplitModuleMap, !SubModulesMap, !RawCompUnitsCord, !Specs)
        else
            true
        )
    ).

    % Check to make sure that non-abstract instance declarations
    % do not occur in a module interface.
    %
:- pred check_interface_blocks_for_abstract_instances(list(raw_item_block)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

check_interface_blocks_for_abstract_instances([], !Specs).
check_interface_blocks_for_abstract_instances([RawItemBlock | RawItemBlocks],
        !Specs) :-
    RawItemBlock = item_block(Section, _Context, _Incls, _Avails, Items),
    (
        Section = ms_interface,
        check_interface_items_for_abstract_instances(Items, !Specs)
    ;
        Section = ms_implementation
    ),
    check_interface_blocks_for_abstract_instances(RawItemBlocks, !Specs).

:- pred check_interface_items_for_abstract_instances(list(item)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

check_interface_items_for_abstract_instances([], !Specs).
check_interface_items_for_abstract_instances([Item | Items], !Specs) :-
    ( if
        Item = item_instance(ItemInstance),
        ItemInstance ^ ci_method_instances \= instance_body_abstract
    then
        InstanceContext = ItemInstance ^ ci_context,
        report_non_abstract_instance_in_interface(InstanceContext, !Specs)
    else
        true
    ),
    check_interface_items_for_abstract_instances(Items, !Specs).

%---------------------------------------------------------------------------%

:- pred split_parse_tree_discover_submodules(parse_tree_src::in,
    module_ancestors::in, split_module_map::in, split_module_map::out,
    module_to_submodules_map::in, module_to_submodules_map::out,
    list(error_spec)::in, list(error_spec)::out) is det.

split_parse_tree_discover_submodules(ParseTree, ModuleAncestors,
        !SplitModuleMap, !SubModulesMap, !Specs) :-
    ParseTree = parse_tree_src(ModuleName, Context, ModuleComponentsCord),
    % If this module is a submodule, record its relationship to its parent.
    add_new_module_maybe_submodule_to_map(ModuleAncestors, ModuleName,
        !SubModulesMap),

    ModuleComponents = cord.list(ModuleComponentsCord),
    SubModuleSectionAncestors = sa_parent(ModuleName, ModuleAncestors),
    split_components_discover_submodules(ModuleComponents,
        SubModuleSectionAncestors, !SplitModuleMap, !SubModulesMap,
        cord.init, ItemBlockCord0, !Specs),
    (
        ModuleAncestors = ma_no_parent,
        ( if map.search(!.SplitModuleMap, ModuleName, OldEntry) then
            (
                OldEntry = split_included(OldContext),
                Pieces = [words("The top level module"),
                    qual_sym_name(ModuleName),
                    words("should not have an"), decl("include_module"),
                    words("declaration for itself."), nl],
                OldPieces = [words("This is the location of the"),
                    decl("include_module"), words("declaration."), nl]
            ;
                OldEntry = split_nested(SplitNested, _),
                OldContext = split_nested_info_get_context(SplitNested),
                Pieces = [words("The top level module"),
                    qual_sym_name(ModuleName),
                    words("should not have its name reused."), nl],
                OldPieces = [words("This is the location of the reuse."), nl]
            ),
            Msg = simple_msg(Context, [always(Pieces)]),
            OldMsg = simple_msg(OldContext, [always(OldPieces)]),
            Spec = error_spec(severity_error, phase_parse_tree_to_hlds,
                [Msg, OldMsg]),
            !:Specs = [Spec | !.Specs]
        else
            Entry = split_nested(split_nested_top_module(Context),
                ItemBlockCord0),
            map.det_insert(ModuleName, Entry, !SplitModuleMap)
        )
    ;
        ModuleAncestors = ma_parent(_SectionKind, _SectionContext,
            SectionAncestors),
        SectionAncestors = sa_parent(ParentModuleName, _),
        ItemBlocks = cord.list(ItemBlockCord0),
        get_raw_item_block_section_kinds(ItemBlocks, no, SeenInt, no, SeenImp),
        ItemBlockCord = cord.from_list(ItemBlocks),
        (
            SeenInt = no,
            SeenImp = no,
            warn_empty_submodule(ModuleName, Context, ParentModuleName,
                !Specs),
            ( if map.search(!.SplitModuleMap, ModuleName, OldEntry) then
                report_duplicate_submodule(ModuleName, Context,
                    dup_empty, ParentModuleName, OldEntry, !Specs)
            else
                true
            )
        ;
            SeenInt = yes,
            SeenImp = no,
            ( if map.search(!.SplitModuleMap, ModuleName, OldEntry) then
                ( if
                    OldEntry = split_nested(OldSplitNested, OldItemBlockCord),
                    OldSplitNested = split_nested_only_imp(ImpContext)
                then
                    NewSplitNested = split_nested_int_imp(Context, ImpContext),
                    NewItemBlockCord = ItemBlockCord ++ OldItemBlockCord,
                    NewEntry = split_nested(NewSplitNested, NewItemBlockCord),
                    map.det_update(ModuleName, NewEntry, !SplitModuleMap)
                else
                    report_duplicate_submodule(ModuleName, Context,
                        dup_int_only, ParentModuleName, OldEntry, !Specs)
                )
            else
                SplitNested = split_nested_only_int(Context),
                Entry = split_nested(SplitNested, ItemBlockCord),
                map.det_insert(ModuleName, Entry, !SplitModuleMap)
            )
        ;
            SeenInt = no,
            SeenImp = yes,
            ( if map.search(!.SplitModuleMap, ModuleName, OldEntry) then
                ( if
                    OldEntry = split_nested(OldSplitNested, OldItemBlockCord),
                    OldSplitNested = split_nested_only_int(IntContext)
                then
                    NewSplitNested = split_nested_int_imp(IntContext, Context),
                    NewItemBlockCord = OldItemBlockCord ++ ItemBlockCord,
                    NewEntry = split_nested(NewSplitNested, NewItemBlockCord),
                    map.det_update(ModuleName, NewEntry, !SplitModuleMap)
                else
                    report_duplicate_submodule(ModuleName, Context,
                        dup_imp_only, ParentModuleName, OldEntry, !Specs)
                )
            else
                SplitNested = split_nested_only_imp(Context),
                Entry = split_nested(SplitNested, ItemBlockCord),
                map.det_insert(ModuleName, Entry, !SplitModuleMap)
            )
        ;
            SeenInt = yes,
            SeenImp = yes,
            ( if map.search(!.SplitModuleMap, ModuleName, OldEntry) then
                report_duplicate_submodule(ModuleName, Context,
                    dup_int_imp, ParentModuleName, OldEntry, !Specs)
            else
                SplitNested = split_nested_int_imp(Context, Context),
                Entry = split_nested(SplitNested, ItemBlockCord),
                map.det_insert(ModuleName, Entry, !SplitModuleMap)
            )
        )
    ).

:- pred warn_empty_submodule(module_name::in, prog_context::in,
    module_name::in, list(error_spec)::in, list(error_spec)::out) is det.

warn_empty_submodule(ModuleName, Context, ParentModuleName, !Specs) :-
    Pieces = [words("Warning: submodule"), qual_sym_name(ModuleName),
        words("of"), words("module"), qual_sym_name(ParentModuleName),
        words("is empty."), nl],
    Msg = simple_msg(Context, [always(Pieces)]),
    Spec = error_spec(severity_warning, phase_parse_tree_to_hlds, [Msg]),
    !:Specs = [Spec | !.Specs].

:- type duplicated_section
    --->    dup_empty
    ;       dup_int_only
    ;       dup_imp_only
    ;       dup_int_imp.

:- pred report_duplicate_submodule(module_name::in, prog_context::in,
    duplicated_section::in, module_name::in, split_module_entry::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_duplicate_submodule(ModuleName, Context, DupSection,
        ParentModuleName, OldEntry, !Specs) :-
    (
        OldEntry = split_included(OldContext),
        Pieces = [words("In module"), qual_sym_name(ParentModuleName),
            suffix(":"), nl,
            words("error: submodule"), qual_sym_name(ModuleName), suffix(","),
            words("declared here as a nested submodule,"),
            words("was previously declared to be a separate submodule."), nl],
        OldPieces = [words("This is the location"),
            words("of that previous declaration."), nl],
        Msg = simple_msg(Context, [always(Pieces)]),
        OldMsg = simple_msg(OldContext, [always(OldPieces)]),
        Spec = error_spec(severity_error, phase_parse_tree_to_hlds,
            [Msg, OldMsg])
    ;
        OldEntry = split_nested(SplitNested, _),
        (
            DupSection = dup_empty,
            OldContext = split_nested_info_get_context(SplitNested),
            Pieces = [words("In module"), qual_sym_name(ParentModuleName),
                suffix(":"), nl,
                words("error: the empty nested submodule"),
                qual_sym_name(ModuleName), words("is a duplicate"),
                words("of a previous declaration of that module."), nl],
            OldPieces = [words("That previous declaration was here."), nl],
            Msg = simple_msg(Context, [always(Pieces)]),
            OldMsg = simple_msg(OldContext, [always(OldPieces)]),
            Spec = error_spec(severity_error, phase_parse_tree_to_hlds,
                [Msg, OldMsg])
        ;
            DupSection = dup_int_only,
            report_duplicate_submodule_one_section(ModuleName, Context,
                ms_interface, ParentModuleName, SplitNested, Spec)
        ;
            DupSection = dup_imp_only,
            report_duplicate_submodule_one_section(ModuleName, Context,
                ms_implementation, ParentModuleName, SplitNested, Spec)
        ;
            DupSection = dup_int_imp,
            (
                SplitNested = split_nested_top_module(_OldContext),
                report_duplicate_submodule_vs_top(ModuleName, Context,
                    ParentModuleName, Spec)
            ;
                SplitNested = split_nested_only_int(_OldContext),
                report_duplicate_submodule_one_section(ModuleName, Context,
                    ms_interface, ParentModuleName, SplitNested, Spec)
            ;
                SplitNested = split_nested_only_imp(_OldContext),
                report_duplicate_submodule_one_section(ModuleName, Context,
                    ms_implementation, ParentModuleName, SplitNested, Spec)
            ;
                SplitNested = split_nested_int_imp(IntContext, ImpContext),
                report_duplicate_submodule_both_sections(ModuleName, Context,
                    ParentModuleName, IntContext, ImpContext, Spec)
            )
        )
    ),
    !:Specs = [Spec | !.Specs].

:- pred report_duplicate_submodule_one_section(module_name::in,
    prog_context::in, module_section::in, module_name::in,
    split_nested_info::in, error_spec::out) is det.

report_duplicate_submodule_one_section(ModuleName, Context, Section,
        ParentModuleName, SplitNested, Spec) :-
    (
        SplitNested = split_nested_top_module(_OldContext),
        report_duplicate_submodule_vs_top(ModuleName, Context,
            ParentModuleName, Spec)
    ;
        (
            SplitNested = split_nested_only_int(IntContext),
            (
                Section = ms_interface,
                SectionWord = "interface",
                OldContext = IntContext
            ;
                Section = ms_implementation,
                unexpected($module, $pred, "duplicate int without duplication")
            ),
            report_duplicate_submodule_one_section_2(ModuleName, Context,
                SectionWord, ParentModuleName, OldContext, Spec)
        ;
            SplitNested = split_nested_only_imp(ImpContext),
            (
                Section = ms_interface,
                unexpected($module, $pred, "duplicate imp without duplication")
            ;
                Section = ms_implementation,
                SectionWord = "implementation",
                OldContext = ImpContext
            ),
            report_duplicate_submodule_one_section_2(ModuleName, Context,
                SectionWord, ParentModuleName, OldContext, Spec)
        ;
            SplitNested = split_nested_int_imp(IntContext, ImpContext),
            (
                Section = ms_interface,
                SectionWord = "interface",
                OldContext = IntContext
            ;
                Section = ms_implementation,
                SectionWord = "implementation",
                OldContext = ImpContext
            ),
            report_duplicate_submodule_one_section_2(ModuleName, Context,
                SectionWord, ParentModuleName, OldContext, Spec)
        )
    ).

:- pred report_duplicate_submodule_vs_top(module_name::in, prog_context::in,
    module_name::in, error_spec::out) is det.

report_duplicate_submodule_vs_top(ModuleName, Context, ParentModuleName,
        Spec) :-
    Pieces = [words("In module"), qual_sym_name(ParentModuleName),
        suffix(":"), nl,
        words("error: nested submodule"), qual_sym_name(ModuleName),
        words("has the same name as its ancestor module."), nl],
    Msg = simple_msg(Context, [always(Pieces)]),
    Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]).

:- pred report_duplicate_submodule_one_section_2(module_name::in,
    prog_context::in, string::in, module_name::in, prog_context::in,
    error_spec::out) is det.

report_duplicate_submodule_one_section_2(ModuleName, Context,
        SectionWord, ParentModuleName, OldContext, Spec) :-
    Pieces = [words("In module"), qual_sym_name(ParentModuleName),
        suffix(":"), nl,
        words("error: nested submodule"), qual_sym_name(ModuleName),
        words("has its"), fixed(SectionWord), words("declared here.")],
    OldPieces = [words("However, its"), fixed(SectionWord),
        words("was also declarated here."), nl],
    Msg = simple_msg(Context, [always(Pieces)]),
    OldMsg = simple_msg(OldContext, [always(OldPieces)]),
    Spec = error_spec(severity_error, phase_parse_tree_to_hlds,
        [Msg, OldMsg]).

:- pred report_duplicate_submodule_both_sections(module_name::in,
    prog_context::in, module_name::in, prog_context::in, prog_context::in,
    error_spec::out) is det.

report_duplicate_submodule_both_sections(ModuleName, Context,
        ParentModuleName, OldIntContext, OldImpContext, Spec) :-
    Pieces = [words("In module"), qual_sym_name(ParentModuleName),
        suffix(":"), nl,
        words("error: nested submodule"), qual_sym_name(ModuleName),
        words("has its both its interface and its implementation"),
        words("declared here."), nl],
    ( if OldIntContext = OldImpContext then
        OldPieces = [words("However, its interface and implementation"),
            words("were also declarated here."), nl],
        Msg = simple_msg(Context, [always(Pieces)]),
        OldMsg = simple_msg(OldIntContext, [always(OldPieces)]),
        Spec = error_spec(severity_error, phase_parse_tree_to_hlds,
            [Msg, OldMsg])
    else
        OldIntPieces = [words("However, its interface"),
            words("was also declarated here,"), nl],
        OldImpPieces = [words("and its implementation"),
            words("was also declarated here."), nl],
        Msg = simple_msg(Context, [always(Pieces)]),
        OldIntMsg = simple_msg(OldIntContext, [always(OldIntPieces)]),
        OldImpMsg = simple_msg(OldImpContext, [always(OldImpPieces)]),
        Spec = error_spec(severity_error, phase_parse_tree_to_hlds,
            [Msg, OldIntMsg, OldImpMsg])
    ).

:- pred get_raw_item_block_section_kinds(list(raw_item_block)::in,
    bool::in, bool::out, bool::in, bool::out) is det.

get_raw_item_block_section_kinds([], !SeenInt, !SeenImp).
get_raw_item_block_section_kinds([ItemBlock | ItemBlocks],
        !SeenInt, !SeenImp) :-
    ItemBlock = item_block(SectionKind, _, _, _, _),
    (
        SectionKind = ms_interface,
        !:SeenInt = yes
    ;
        SectionKind = ms_implementation,
        !:SeenImp = yes
    ),
    get_raw_item_block_section_kinds(ItemBlocks, !SeenInt, !SeenImp).

%---------------------------------------------------------------------------%

:- pred split_components_discover_submodules(list(module_component)::in,
    section_ancestors::in,
    split_module_map::in, split_module_map::out,
    module_to_submodules_map::in, module_to_submodules_map::out,
    cord(raw_item_block)::in, cord(raw_item_block)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

split_components_discover_submodules([], _,
        !SplitModuleMap, !SubModulesMap, !RawItemBlockCord, !Specs).
split_components_discover_submodules([Component | Components],
        SectionAncestors, !SplitModuleMap, !SubModulesMap,
        !RawItemBlockCord, !Specs) :-
    split_component_discover_submodules(Component, SectionAncestors,
        !SplitModuleMap, !SubModulesMap, !RawItemBlockCord, !Specs),
    split_components_discover_submodules(Components, SectionAncestors,
        !SplitModuleMap, !SubModulesMap, !RawItemBlockCord, !Specs).

:- pred split_component_discover_submodules(module_component::in,
    section_ancestors::in,
    split_module_map::in, split_module_map::out,
    module_to_submodules_map::in, module_to_submodules_map::out,
    cord(raw_item_block)::in, cord(raw_item_block)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

split_component_discover_submodules(Component, SectionAncestors,
        !SplitModuleMap, !SubModulesMap, !RawItemBlockCord, !Specs) :-
    (
        Component = mc_section(SectionKind, SectionContext,
            IncludesCord, AvailsCord, ItemsCord),
        Includes = cord.list(IncludesCord),
        Avails = cord.list(AvailsCord),
        Items = cord.list(ItemsCord),
        discover_included_submodules(Includes, SectionAncestors,
            cord.init, OKIncludesCord,
            !SplitModuleMap, !SubModulesMap, !Specs),
        OKIncludes = cord.list(OKIncludesCord),
        RawItemBlock = item_block(SectionKind, SectionContext,
            OKIncludes, Avails, Items),
        !:RawItemBlockCord = cord.snoc(!.RawItemBlockCord, RawItemBlock),
        (
            SectionKind = ms_interface
        ;
            SectionKind = ms_implementation,
            section_has_some_ancestor_in_interface(SectionAncestors,
                MaybeProblemAncestor),
            (
                MaybeProblemAncestor = no
            ;
                MaybeProblemAncestor = yes(ProblemAncestor),
                SectionAncestors = sa_parent(CurModuleName, ModuleAncestors),
                (
                    ModuleAncestors = ma_no_parent,
                    unexpected($module, $pred,
                        "in interface section of nonexistent ancestor")
                ;
                    ModuleAncestors = ma_parent(_, _, ModuleSectionAncestor),
                    ModuleSectionAncestor = sa_parent(ModuleParent, _),
                    ( if ModuleParent = ProblemAncestor then
                        PorA = "parent"
                    else
                        PorA = "ancestor"
                    )
                ),
                Pieces = [words("This implementation section for module"),
                    qual_sym_name(CurModuleName), words("occurs in"),
                    words("the interface section of"), words(PorA),
                    words("module"), qual_sym_name(ProblemAncestor),
                    suffix("."), nl],
                Msg = simple_msg(SectionContext, [always(Pieces)]),
                Spec = error_spec(severity_error, phase_parse_tree_to_hlds,
                    [Msg]),
                !:Specs = [Spec | !.Specs]
            )
        )
    ;
        Component = mc_nested_submodule(SectionKind, SectionContext,
            NestedModuleParseTree),
        % Replace the nested submodule with an `include_module' declaration.
        NestedModuleParseTree = parse_tree_src(NestedModuleName,
            NestedModuleContext, _NestedModuleComponents),
        NestedIncludeItem =
            item_include(NestedModuleName, NestedModuleContext, -1),
        RawItemBlock = item_block(SectionKind, SectionContext,
            [NestedIncludeItem], [], []),
        !:RawItemBlockCord = cord.snoc(!.RawItemBlockCord, RawItemBlock),

        % Discover any submodules nested inside NestedModuleParseTree.
        NestedModuleAncestors = ma_parent(SectionKind, SectionContext,
            SectionAncestors),
        split_parse_tree_discover_submodules(NestedModuleParseTree,
            NestedModuleAncestors, !SplitModuleMap, !SubModulesMap, !Specs)
    ).

:- pred discover_included_submodules(list(item_include)::in,
    section_ancestors::in, cord(item_include)::in, cord(item_include)::out,
    split_module_map::in, split_module_map::out,
    module_to_submodules_map::in, module_to_submodules_map::out,
    list(error_spec)::in, list(error_spec)::out) is det.

discover_included_submodules([], _,
        !OKIncludesCord, !SplitModuleMap, !SubModulesMap, !Specs).
discover_included_submodules([Include | Includes], SectionAncestors,
        !OKIncludesCord, !SplitModuleMap, !SubModulesMap, !Specs) :-
    Include = item_include(InclModuleName, Context, _SeqNum),
    ( if map.search(!.SplitModuleMap, InclModuleName, OldEntry) then
        SectionAncestors = sa_parent(ParentModuleName, _),
        Pieces1 = [words("In module"), qual_sym_name(ParentModuleName),
            suffix(":"), nl,
            words("error: submodule"), qual_sym_name(InclModuleName),
            suffix(","),
            words("included here as separate submodule,")],
        (
            OldEntry = split_nested(OldSplitNested, _),
            OldContext = split_nested_info_get_context(OldSplitNested),
            Pieces2 = [words("was previously declared to be"),
                words("a nested submodule."), nl]
        ;
            OldEntry = split_included(OldContext),
            Pieces2 = [words("has already been declared"),
                words("to be a separate submodule."), nl]
        ),

        OldPieces = [words("This is the location"),
            words("of that previous declaration."), nl],
        Msg = simple_msg(Context, [always(Pieces1 ++ Pieces2)]),
        OldMsg = simple_msg(OldContext, [always(OldPieces)]),
        Spec = error_spec(severity_error, phase_parse_tree_to_hlds,
            [Msg, OldMsg]),
        !:Specs = [Spec | !.Specs]
    else
        Entry = split_included(Context),
        map.det_insert(InclModuleName, Entry, !SplitModuleMap),
        add_new_submodule_to_map(SectionAncestors, InclModuleName,
            !SubModulesMap),
        !:OKIncludesCord = cord.snoc(!.OKIncludesCord, Include)
    ),
    discover_included_submodules(Includes, SectionAncestors,
        !OKIncludesCord, !SplitModuleMap, !SubModulesMap, !Specs).

%---------------------------------------------------------------------------%

    % XXX ITEM_LIST Unused predicate.
:- pred report_error_implementation_in_interface(module_name::in,
    prog_context::in, list(error_spec)::in, list(error_spec)::out) is det.

report_error_implementation_in_interface(ModuleName, Context, !Specs) :-
    (
        ModuleName = qualified(ParentModule0, ChildModule0),
        ParentModule = ParentModule0,
        ChildModule = ChildModule0
    ;
        ModuleName = unqualified(_),
        unexpected($module, $pred, "unqualified module name")
    ),
    Pieces = [words("In interface for module"), qual_sym_name(ParentModule),
        suffix(":"), nl, words("in definition of submodule"),
        quote(ChildModule), suffix(":"), nl,
        words("error:"), decl("implementation"),
        words("declaration for submodule"),
        words("occurs in interface section of parent module."), nl],
    Msg = simple_msg(Context, [always(Pieces)]),
    Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
    !:Specs = [Spec | !.Specs].

%---------------------------------------------------------------------------%

:- pred report_non_abstract_instance_in_interface(prog_context::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_non_abstract_instance_in_interface(Context, !Specs) :-
    Pieces = [words("Error: non-abstract instance declaration"),
        words("in module interface."), nl],
    Msg = simple_msg(Context, [always(Pieces)]),
    Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
    !:Specs = [Spec | !.Specs].

%---------------------------------------------------------------------------%
:- end_module parse_tree.split_parse_tree_src.
%---------------------------------------------------------------------------%
