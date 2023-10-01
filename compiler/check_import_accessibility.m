%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2023 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: check_import_accessibility.m.
% Main author: fjh (original), zs (current).
%
% Given the parse tree of a module, and information about the accessibility
% of included and/or imported modules gathered by our caller from either
% an aug_compilation_unit or an aug_make_int_unit, we look for and report
% two different but related kinds of errors.
%
% The first is when we see a reference to module x.y.z, but module x.y
% does not include a submodule named z.
%
% The second is when module m.n has an import_module or use_module
% declaration for module x.y.z, but there is some ancestor of x.y.z
% (either x or x.y) that neither m.n nor its ancestor m imports or uses.
%
% XXX ITEM_LIST We should either record in an updated AugCompUnit
% the set of imported modules that are inaccessible, or remove their
% imports from it, so that
%
% - when we report e.g. an undefined type, we don't tell the user that
%   the module that defines the type hasn't been imported, when in fact
%   it *was* imported, but the import was disregarded because the module
%   is inaccessible due to the missing import of an ancestor; and
%
% - we don't generate "unused module" warnings for them when
%   --warn-unused-imports is enabled.
%
%---------------------------------------------------------------------------%

:- module parse_tree.check_import_accessibility.
:- interface.

:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.prog_item.

:- import_module list.

%---------------------------------------------------------------------------%

:- type import_accessibility_info.

    % Generate the information that the main part of this module,
    % check_import_accessibility, needs.
    %
    % This part needs to be done slightly differently for
    % aug_compilation_units and aug_make_int_units, which is why
    % this part of the job is separated out.
    %
:- pred aug_comp_unit_get_import_accessibility_info(aug_compilation_unit::in,
    import_accessibility_info::out) is det.
:- pred aug_make_int_unit_get_import_accessibility_info(aug_make_int_unit::in,
    import_accessibility_info::out) is det.

%---------------------------------------------------------------------------%

    % check_import_accessibility(ParseTreeModuleSrc, ImportAccessibilityInfo,
    %   Specs):
    %
    % This predicate does the job described in the top-of-module comment.
    %
:- pred check_import_accessibility(parse_tree_module_src::in,
    import_accessibility_info::in, list(error_spec)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.
:- import_module libs.globals.
:- import_module parse_tree.item_util.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.

:- import_module map.
:- import_module maybe.
:- import_module one_or_more.
:- import_module require.
:- import_module set.
:- import_module term_context.

%---------------------------------------------------------------------------%

    % aug_{comp,make_int}_unit_get_import_accessibily_info both generate
    % a value of type import_accessibility_info by scanning the relevant kinds
    % of items in the input structures they are given.
    %
    % - The iai_read_modules field will contain the set of module names
    %   from whose files (source files, interface files, optimization files)
    %   the information in the rest of the fields originates.
    %
    % - The iai_inclusion_map field will map the name of each module
    %   that is named in an include_module declaration in the input
    %   data structure to the context of that declaration.
    %
    %   The iai_seen_includes field, supplied by the caller, specifies
    %   whether any include_module declarations in the read-in modules
    %   may be intentionally missing from this map. When the input is
    %   an aug_compilation_unit, all include_module declarations in the
    %   relevant modules will be in this map, but when the input is
    %   an aug_make_int_unit, the map will contain information only about
    %   include_module declarations in the interface sections of those modules
    %   (since the .int3 files we read in from them contain nothing at all
    %   from the implementation section).
    %
    % - The iai_src_int_import_use_map field will map the module names
    %   that occur in import_module or use_module declarations in the
    %   interface sections of the given parse_tree_module_src itself
    %   to the context(s) of those declarations.
    %
    % - The iai_src_imp_import_use_map field contains the same info
    %   for the implementation section.
    %
    % - The iai_ancestor_import_use_map field again contains the same info,
    %   but for import_module and use_module declarations read from
    %   (the .int0 interface files of) the ancestors of the
    %   parse_tree_module_src.
    %
    % NOTE By making the value in both the module_inclusion_map and the
    % module_import_or_use_map a (nonempty) list, we can represent situations
    % in which a module includes, imports or uses another module
    % more than once. This is an error, and we could and probably should
    % diagnose it here, but doing so would require disabling the code
    % we have elsewhere in the compiler that does that job. If we did that,
    % we could replace the nonempty lists of contexts with just one context,
    % and a message for every other context.
    %
:- type import_accessibility_info
    --->    import_accessibility_info(
                iai_read_modules                :: set(module_name),
                iai_seen_includes               :: seen_includes,
                iai_inclusion_map               :: module_inclusion_map,
                iai_src_int_import_use_map      :: module_import_or_use_map,
                iai_src_imp_import_use_map      :: module_import_or_use_map,
                iai_ancestor_import_use_map     :: module_import_or_use_map
            ).

:- type seen_includes
    --->    seen_only_int_includes
    ;       seen_all_includes.

% The module_inclusion_map and module_import_or_use_map are computed by
% record_includes_imports_uses, for use by find_any_missing_ancestor_imports.
% For their documentation, see those predicates below.

:- type maybe_abstract_section
    --->    non_abstract_section
    ;       abstract_section.

:- type include_context
    --->    include_context(maybe_abstract_section, term_context).

:- type module_inclusion_map ==
    map(module_name, one_or_more(include_context)).

:- type import_or_use_context
    --->    import_or_use_context(import_or_use, term_context).

:- type module_import_or_use_map ==
    map(module_name, one_or_more(import_or_use_context)).

%---------------------------------------------------------------------------%

aug_comp_unit_get_import_accessibility_info(AugCompUnit,
        ImportAccessibilityInfo) :-
    AugCompUnit = aug_compilation_unit(ParseTreeModuleSrc, AncestorIntSpecs,
        DirectIntSpecs, IndirectIntSpecs, PlainOpts, _TransOpts,
        IntForOptSpecs, _TypeRepnSpecs, _ModuleVersionNumbers),
    some [!ReadModules, !InclMap, !SrcIntImportUseMap, !SrcImpImportUseMap,
        !AncestorImportUseMap]
    (
        set.init(!:ReadModules),
        map.init(!:InclMap),
        map.init(!:SrcIntImportUseMap),
        map.init(!:SrcImpImportUseMap),
        map.init(!:AncestorImportUseMap),
        ModuleName = ParseTreeModuleSrc ^ ptms_module_name,
        Ancestors = get_ancestors_set(ModuleName),
        record_includes_imports_uses_in_parse_tree_module_src(
            ParseTreeModuleSrc,
            !ReadModules, !InclMap,
            !SrcIntImportUseMap, !SrcImpImportUseMap, !AncestorImportUseMap),
        map.foldl5_values(
            record_includes_imports_uses_in_ancestor_int_spec(Ancestors),
            AncestorIntSpecs, !ReadModules, !InclMap,
            !SrcIntImportUseMap, !SrcImpImportUseMap, !AncestorImportUseMap),
        map.foldl5_values(
            record_includes_imports_uses_in_direct_int1_spec(Ancestors),
            DirectIntSpecs, !ReadModules, !InclMap,
            !SrcIntImportUseMap, !SrcImpImportUseMap, !AncestorImportUseMap),
        map.foldl5_values(
            record_includes_imports_uses_in_indirect_int2_spec(Ancestors),
            IndirectIntSpecs, !ReadModules, !InclMap,
            !SrcIntImportUseMap, !SrcImpImportUseMap, !AncestorImportUseMap),
        map.foldl5_values(
            record_includes_imports_uses_in_parse_tree_plain_opt(Ancestors),
            PlainOpts, !ReadModules, !InclMap,
            !SrcIntImportUseMap, !SrcImpImportUseMap, !AncestorImportUseMap),
        % .trans_opt files may contain no include_module, import_module
        % or use_module declarations, so there is nothing to record for them.
        map.foldl5_values(
            record_includes_imports_uses_in_int_for_opt_spec(Ancestors),
            IntForOptSpecs, !ReadModules, !InclMap,
            !SrcIntImportUseMap, !SrcImpImportUseMap, !AncestorImportUseMap),
        ImportAccessibilityInfo = import_accessibility_info(!.ReadModules,
            seen_all_includes, !.InclMap,
            !.SrcIntImportUseMap, !.SrcImpImportUseMap, !.AncestorImportUseMap)
    ).

aug_make_int_unit_get_import_accessibility_info(AugMakeIntUnit,
        ImportAccessibilityInfo) :-
    AugMakeIntUnit = aug_make_int_unit(ParseTreeModuleSrc, AncestorIntSpecs,
        DirectIntSpecs, IndirectIntSpecs, _ModuleVersionNumbers),
    some [!ReadModules, !InclMap, !SrcIntImportUseMap, !SrcImpImportUseMap,
        !AncestorImportUseMap]
    (
        set.init(!:ReadModules),
        map.init(!:InclMap),
        map.init(!:SrcIntImportUseMap),
        map.init(!:SrcImpImportUseMap),
        map.init(!:AncestorImportUseMap),
        ModuleName = ParseTreeModuleSrc ^ ptms_module_name,
        Ancestors = get_ancestors_set(ModuleName),
        record_includes_imports_uses_in_parse_tree_module_src(
            ParseTreeModuleSrc,
            !ReadModules, !InclMap,
            !SrcIntImportUseMap, !SrcImpImportUseMap, !AncestorImportUseMap),
        map.foldl5_values(
            record_includes_imports_uses_in_parse_tree_int0(Ancestors),
            AncestorIntSpecs, !ReadModules, !InclMap,
            !SrcIntImportUseMap, !SrcImpImportUseMap, !AncestorImportUseMap),
        map.foldl5_values(
            record_includes_imports_uses_in_direct_int3_spec(Ancestors),
            DirectIntSpecs, !ReadModules, !InclMap,
            !SrcIntImportUseMap, !SrcImpImportUseMap, !AncestorImportUseMap),
        map.foldl5_values(
            record_includes_imports_uses_in_indirect_int3_spec(Ancestors),
            IndirectIntSpecs, !ReadModules, !InclMap,
            !SrcIntImportUseMap, !SrcImpImportUseMap, !AncestorImportUseMap),
        ImportAccessibilityInfo = import_accessibility_info(!.ReadModules,
            seen_only_int_includes, !.InclMap,
            !.SrcIntImportUseMap, !.SrcImpImportUseMap, !.AncestorImportUseMap)
    ).

%---------------------------------------------------------------------------%

:- pred record_includes_imports_uses_in_parse_tree_module_src(
    parse_tree_module_src::in,
    set(module_name)::in, set(module_name)::out,
    module_inclusion_map::in, module_inclusion_map::out,
    module_import_or_use_map::in, module_import_or_use_map::out,
    module_import_or_use_map::in, module_import_or_use_map::out,
    module_import_or_use_map::in, module_import_or_use_map::out) is det.

record_includes_imports_uses_in_parse_tree_module_src(ParseTreeModuleSrc,
        !ReadModules, !MaybeAbstractInclMap,
        !SrcIntImportUseMap, !SrcImpImportUseMap, !AncestorImportUseMap) :-
    ParseTreeModuleSrc = parse_tree_module_src(ModuleName, _,
        InclMap, ImportUseMap,
        _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _,
        _, _, _, _, _, _, _, _, _, _, _, _, _, _, _),
    set.insert(ModuleName, !ReadModules),
    map.foldl(record_include(non_abstract_section, yes(non_abstract_section)),
        InclMap, !MaybeAbstractInclMap),
    map.foldl2(record_avail_in_import_use_map_entry, ImportUseMap,
        !SrcIntImportUseMap, !SrcImpImportUseMap).

:- pred record_includes_imports_uses_in_ancestor_int_spec(set(module_name)::in,
    ancestor_int_spec::in,
    set(module_name)::in, set(module_name)::out,
    module_inclusion_map::in, module_inclusion_map::out,
    module_import_or_use_map::in, module_import_or_use_map::out,
    module_import_or_use_map::in, module_import_or_use_map::out,
    module_import_or_use_map::in, module_import_or_use_map::out) is det.

record_includes_imports_uses_in_ancestor_int_spec(Ancestors,
        AncestorSpec, !ReadModules, !InclMap,
        !SrcIntImportUseMap, !SrcImpImportUseMap, !AncestorImportUseMap) :-
    AncestorSpec = ancestor_int0(ParseTreeInt0, _ReadWhyInt0),
    record_includes_imports_uses_in_parse_tree_int0(Ancestors,
        ParseTreeInt0, !ReadModules, !InclMap,
        !SrcIntImportUseMap, !SrcImpImportUseMap, !AncestorImportUseMap).

:- pred record_includes_imports_uses_in_direct_int1_spec(
    set(module_name)::in, direct_int1_spec::in,
    set(module_name)::in, set(module_name)::out,
    module_inclusion_map::in, module_inclusion_map::out,
    module_import_or_use_map::in, module_import_or_use_map::out,
    module_import_or_use_map::in, module_import_or_use_map::out,
    module_import_or_use_map::in, module_import_or_use_map::out) is det.

record_includes_imports_uses_in_direct_int1_spec(Ancestors,
        DirectSpec, !ReadModules, !InclMap,
        !SrcIntImportUseMap, !SrcImpImportUseMap, !AncestorImportUseMap) :-
    DirectSpec = direct_int1(ParseTreeInt1, ReadWhyInt1),
    record_includes_imports_uses_in_parse_tree_int1(Ancestors,
        ParseTreeInt1, ReadWhyInt1, !ReadModules, !InclMap,
        !SrcIntImportUseMap, !SrcImpImportUseMap, !AncestorImportUseMap).

:- pred record_includes_imports_uses_in_indirect_int2_spec(
    set(module_name)::in, indirect_int2_spec::in,
    set(module_name)::in, set(module_name)::out,
    module_inclusion_map::in, module_inclusion_map::out,
    module_import_or_use_map::in, module_import_or_use_map::out,
    module_import_or_use_map::in, module_import_or_use_map::out,
    module_import_or_use_map::in, module_import_or_use_map::out) is det.

record_includes_imports_uses_in_indirect_int2_spec(Ancestors,
        IndirectSpec, !ReadModules, !InclMap,
        !SrcIntImportUseMap, !SrcImpImportUseMap, !AncestorImportUseMap) :-
    IndirectSpec = indirect_int2(ParseTreeInt2, ReadWhyInt2),
    record_includes_imports_uses_in_parse_tree_int2(Ancestors,
        ParseTreeInt2, ReadWhyInt2, !ReadModules, !InclMap,
        !SrcIntImportUseMap, !SrcImpImportUseMap, !AncestorImportUseMap).

:- pred record_includes_imports_uses_in_direct_int3_spec(
    set(module_name)::in, direct_int3_spec::in,
    set(module_name)::in, set(module_name)::out,
    module_inclusion_map::in, module_inclusion_map::out,
    module_import_or_use_map::in, module_import_or_use_map::out,
    module_import_or_use_map::in, module_import_or_use_map::out,
    module_import_or_use_map::in, module_import_or_use_map::out) is det.

record_includes_imports_uses_in_direct_int3_spec(Ancestors,
        IndirectSpec, !ReadModules, !InclMap,
        !SrcIntImportUseMap, !SrcImpImportUseMap, !AncestorImportUseMap) :-
    IndirectSpec = direct_int3(ParseTreeInt3, _ReadWhyInt3),
    record_includes_imports_uses_in_parse_tree_int3(Ancestors,
        ParseTreeInt3, non_abstract_section, !ReadModules, !InclMap,
        !SrcIntImportUseMap, !SrcImpImportUseMap, !AncestorImportUseMap).

:- pred record_includes_imports_uses_in_indirect_int3_spec(
    set(module_name)::in, indirect_int3_spec::in,
    set(module_name)::in, set(module_name)::out,
    module_inclusion_map::in, module_inclusion_map::out,
    module_import_or_use_map::in, module_import_or_use_map::out,
    module_import_or_use_map::in, module_import_or_use_map::out,
    module_import_or_use_map::in, module_import_or_use_map::out) is det.

record_includes_imports_uses_in_indirect_int3_spec(Ancestors,
        IndirectSpec, !ReadModules, !InclMap,
        !SrcIntImportUseMap, !SrcImpImportUseMap, !AncestorImportUseMap) :-
    IndirectSpec = indirect_int3(ParseTreeInt3, _ReadWhyInt3),
    record_includes_imports_uses_in_parse_tree_int3(Ancestors,
        ParseTreeInt3, abstract_section, !ReadModules, !InclMap,
        !SrcIntImportUseMap, !SrcImpImportUseMap, !AncestorImportUseMap).

:- pred record_includes_imports_uses_in_int_for_opt_spec(set(module_name)::in,
    int_for_opt_spec::in,
    set(module_name)::in, set(module_name)::out,
    module_inclusion_map::in, module_inclusion_map::out,
    module_import_or_use_map::in, module_import_or_use_map::out,
    module_import_or_use_map::in, module_import_or_use_map::out,
    module_import_or_use_map::in, module_import_or_use_map::out) is det.

record_includes_imports_uses_in_int_for_opt_spec(Ancestors,
        IntForOptSpec, !ReadModules, !InclMap,
        !SrcIntImportUseMap, !SrcImpImportUseMap, !AncestorImportUseMap) :-
    (
        IntForOptSpec = for_opt_int0(ParseTreeInt0, _ReadWhyInt0),
        record_includes_imports_uses_in_parse_tree_int0(Ancestors,
            ParseTreeInt0, !ReadModules, !InclMap,
            !SrcIntImportUseMap, !SrcImpImportUseMap, !AncestorImportUseMap)
    ;
        IntForOptSpec = for_opt_int1(ParseTreeInt1, ReadWhyInt1),
        record_includes_imports_uses_in_parse_tree_int1(Ancestors,
            ParseTreeInt1, ReadWhyInt1, !ReadModules, !InclMap,
            !SrcIntImportUseMap, !SrcImpImportUseMap, !AncestorImportUseMap)
    ;
        IntForOptSpec = for_opt_int2(ParseTreeInt2, ReadWhyInt2),
        record_includes_imports_uses_in_parse_tree_int2(Ancestors,
            ParseTreeInt2, ReadWhyInt2, !ReadModules, !InclMap,
            !SrcIntImportUseMap, !SrcImpImportUseMap, !AncestorImportUseMap)
    ).

%---------------------%

:- pred record_includes_imports_uses_in_parse_tree_int0(set(module_name)::in,
    parse_tree_int0::in,
    set(module_name)::in, set(module_name)::out,
    module_inclusion_map::in, module_inclusion_map::out,
    module_import_or_use_map::in, module_import_or_use_map::out,
    module_import_or_use_map::in, module_import_or_use_map::out,
    module_import_or_use_map::in, module_import_or_use_map::out) is det.

record_includes_imports_uses_in_parse_tree_int0(Ancestors,
        ParseTreeInt0, !ReadModules, !MaybeAbstractInclMap,
        !SrcIntImportUseMap, !SrcImpImportUseMap, !AncestorImportUseMap) :-
    ParseTreeInt0 = parse_tree_int0(ModuleName, _, _, InclMap, ImportUseMap,
        _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _),
    set.insert(ModuleName, !ReadModules),
    % Both possible values of ReadWhyInt0 call for treating the file contents
    % as non-abstract.
    map.foldl(record_include(non_abstract_section, yes(non_abstract_section)),
        InclMap, !MaybeAbstractInclMap),
    ( if set.contains(Ancestors, ModuleName) then
        % XXX CLEANUP This work could be done on ImportUseMap,
        % *without* constructing AllAvails.
        section_import_and_or_use_map_to_item_avails(ImportUseMap,
            IntAvails, ImpAvails),
        AllAvails = IntAvails ++ ImpAvails,
        record_avails_acc(AllAvails, !AncestorImportUseMap)
    else
        true
    ).

:- pred record_includes_imports_uses_in_parse_tree_int1(set(module_name)::in,
    parse_tree_int1::in, read_why_int1::in,
    set(module_name)::in, set(module_name)::out,
    module_inclusion_map::in, module_inclusion_map::out,
    module_import_or_use_map::in, module_import_or_use_map::out,
    module_import_or_use_map::in, module_import_or_use_map::out,
    module_import_or_use_map::in, module_import_or_use_map::out) is det.

record_includes_imports_uses_in_parse_tree_int1(Ancestors,
        ParseTreeInt1, ReadWhyInt1, !ReadModules, !InclMap,
        !SrcIntImportUseMap, !SrcImpImportUseMap, !AncestorImportUseMap) :-
    ParseTreeInt1 = parse_tree_int1(ModuleName, _, _, InclMap, _,
        _, _, _, _, _, _, _, _, _, _, _, _, _, _),
    set.insert(ModuleName, !ReadModules),
    (
        ( ReadWhyInt1 = rwi1_int_import
        ; ReadWhyInt1 = rwi1_imp_import
        ; ReadWhyInt1 = rwi1_int_use
        ; ReadWhyInt1 = rwi1_imp_use
        ; ReadWhyInt1 = rwi1_int_use_imp_import
        ),
        % All these values of ReadWhyInt1 call for treating
        % - the interface as non-abstract, and
        % - the implementation as abstract.
        map.foldl(
            record_include(non_abstract_section, yes(abstract_section)),
            InclMap, !InclMap)
    ;
        ReadWhyInt1 = rwi1_opt,
        map.foldl(
            record_include(non_abstract_section, yes(non_abstract_section)),
            InclMap, !InclMap)
    ;
        ReadWhyInt1 = rwi1_type_repn
    ),
    expect_not(set.contains(Ancestors, ModuleName), $pred,
        "processing the .int file of an ancestor").

:- pred record_includes_imports_uses_in_parse_tree_int2(set(module_name)::in,
    parse_tree_int2::in, read_why_int2::in,
    set(module_name)::in, set(module_name)::out,
    module_inclusion_map::in, module_inclusion_map::out,
    module_import_or_use_map::in, module_import_or_use_map::out,
    module_import_or_use_map::in, module_import_or_use_map::out,
    module_import_or_use_map::in, module_import_or_use_map::out) is det.

record_includes_imports_uses_in_parse_tree_int2(Ancestors,
        ParseTreeInt2, ReadWhyInt2, !ReadModules, !InclMap,
        !SrcIntImportUseMap, !SrcImpImportUseMap, !AncestorImportUseMap) :-
    ParseTreeInt2 = parse_tree_int2(ModuleName, _, _,
        IntInclMap, _, _, _, _, _, _, _, _, _),
    set.insert(ModuleName, !ReadModules),
    InclMap = coerce(IntInclMap),
    (
        ( ReadWhyInt2 = rwi2_int_use
        ; ReadWhyInt2 = rwi2_imp_use
        ; ReadWhyInt2 = rwi2_opt
        ),
        map.foldl(record_include(non_abstract_section, no), InclMap, !InclMap)
    ;
        ReadWhyInt2 = rwi2_abstract,
        map.foldl(record_include(abstract_section, no), InclMap, !InclMap)
    ),
    expect_not(set.contains(Ancestors, ModuleName), $pred,
        "processing the .int2 file of an ancestor").

:- pred record_includes_imports_uses_in_parse_tree_int3(set(module_name)::in,
    parse_tree_int3::in, maybe_abstract_section::in,
    set(module_name)::in, set(module_name)::out,
    module_inclusion_map::in, module_inclusion_map::out,
    module_import_or_use_map::in, module_import_or_use_map::out,
    module_import_or_use_map::in, module_import_or_use_map::out,
    module_import_or_use_map::in, module_import_or_use_map::out) is det.

record_includes_imports_uses_in_parse_tree_int3(Ancestors,
        ParseTreeInt3, MaybeAbstractSection, !ReadModules, !InclMap,
        !SrcIntImportUseMap, !SrcImpImportUseMap, !AncestorImportUseMap) :-
    ParseTreeInt3 = parse_tree_int3(ModuleName, _, IntInclMap,
        _, _, _, _, _, _, _),
    set.insert(ModuleName, !ReadModules),
    InclMap = coerce(IntInclMap),
    map.foldl(record_include(MaybeAbstractSection, no), InclMap, !InclMap),
    expect_not(set.contains(Ancestors, ModuleName), $pred,
        "processing the .int3 file of an ancestor").

:- pred record_includes_imports_uses_in_parse_tree_plain_opt(
    set(module_name)::in, parse_tree_plain_opt::in,
    set(module_name)::in, set(module_name)::out,
    module_inclusion_map::in, module_inclusion_map::out,
    module_import_or_use_map::in, module_import_or_use_map::out,
    module_import_or_use_map::in, module_import_or_use_map::out,
    module_import_or_use_map::in, module_import_or_use_map::out) is det.

record_includes_imports_uses_in_parse_tree_plain_opt(Ancestors,
        ParseTreePlainOpt, !ReadModules, !InclMap,
        !SrcIntImportUseMap, !SrcImpImportUseMap, !AncestorImportUseMap) :-
    ParseTreePlainOpt = parse_tree_plain_opt(ModuleName, _, UseMap,
        _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _),
    ( if set.contains(Ancestors, ModuleName) then
        Avails = use_map_to_item_avails(UseMap),
        record_avails_acc(Avails, !AncestorImportUseMap)
    else
        true
    ).

%---------------------------------------------------------------------------%

:- pred record_include(maybe_abstract_section::in,
    maybe(maybe_abstract_section)::in,
    module_name::in, include_module_info::in,
    module_inclusion_map::in, module_inclusion_map::out) is det.

record_include(MaybeAbsSectionInt, MaybeMaybeAbsSectionImp,
        ModuleName, InclInfo, !InclMap) :-
    InclInfo = include_module_info(Section, Context),
    (
        Section = ms_interface,
        IncludeContext0 = include_context(MaybeAbsSectionInt, Context),
        MaybeIncludeContext = yes(IncludeContext0)
    ;
        Section = ms_implementation,
        (
            MaybeMaybeAbsSectionImp = yes(MaybeAbsSectionImp),
            IncludeContext0 = include_context(MaybeAbsSectionImp, Context),
            MaybeIncludeContext = yes(IncludeContext0)
        ;
            MaybeMaybeAbsSectionImp = no,
            MaybeIncludeContext = no
        )
    ),
    (
        MaybeIncludeContext = yes(IncludeContext),
        ( if map.search(!.InclMap, ModuleName, OneOrMore0) then
            OneOrMore0 = one_or_more(HeadContext, TailContexts),
            OneOrMore =
                one_or_more(IncludeContext, [HeadContext | TailContexts]),
            map.det_update(ModuleName, OneOrMore, !InclMap)
        else
            OneOrMore = one_or_more(IncludeContext, []),
            map.det_insert(ModuleName, OneOrMore, !InclMap)
        )
    ;
        MaybeIncludeContext = no
    ).

%---------------------%

:- pred record_avail_in_import_use_map_entry(module_name::in,
    maybe_implicit_import_and_or_use::in,
    module_import_or_use_map::in, module_import_or_use_map::out,
    module_import_or_use_map::in, module_import_or_use_map::out) is det.

record_avail_in_import_use_map_entry(ModuleName, MaybeImplicit,
        !IntImportUseMap, !ImpImportUseMap) :-
    (
        MaybeImplicit = implicit_avail(_ImplicitImportOrUse,
            MaybeSectionImportOrUse),
        (
            MaybeSectionImportOrUse = no
        ;
            MaybeSectionImportOrUse = yes(SectionImportOrUse),
            record_avail_in_section(ModuleName, SectionImportOrUse,
                !IntImportUseMap, !ImpImportUseMap)
        )
    ;
        MaybeImplicit = explicit_avail(SectionImportOrUse),
        record_avail_in_section(ModuleName, SectionImportOrUse,
            !IntImportUseMap, !ImpImportUseMap)
    ).

:- pred record_avail_in_section(module_name::in, section_import_and_or_use::in,
    module_import_or_use_map::in, module_import_or_use_map::out,
    module_import_or_use_map::in, module_import_or_use_map::out) is det.

record_avail_in_section(ModuleName, SectionImportOrUse,
        !IntImportUseMap, !ImpImportUseMap) :-
    (
        ( SectionImportOrUse = int_import(Context), IoU = import_decl
        ; SectionImportOrUse = int_use(Context),    IoU = use_decl
        ),
        record_avail(ModuleName, IoU, Context, !IntImportUseMap)
    ;
        ( SectionImportOrUse = imp_import(Context), IoU = import_decl
        ; SectionImportOrUse = imp_use(Context),    IoU = use_decl
        ),
        record_avail(ModuleName, IoU, Context, !ImpImportUseMap)
    ;
        SectionImportOrUse = int_use_imp_import(IntContext, ImpContext),
        record_avail(ModuleName, use_decl, IntContext, !IntImportUseMap),
        record_avail(ModuleName, import_decl, ImpContext, !ImpImportUseMap)
    ).

:- pred record_avail(module_name::in, import_or_use::in, prog_context::in,
    module_import_or_use_map::in, module_import_or_use_map::out) is det.

record_avail(ModuleName, ImportOrUse, Context, !ImportUseMap) :-
    IoUC = import_or_use_context(ImportOrUse, Context),
    ( if map.search(!.ImportUseMap, ModuleName, OneOrMore0) then
        OneOrMore0 = one_or_more(HeadIoUC, TailIoUCs),
        OneOrMore = one_or_more(IoUC, [HeadIoUC | TailIoUCs]),
        map.det_update(ModuleName, OneOrMore, !ImportUseMap)
    else
        OneOrMore = one_or_more(IoUC, []),
        map.det_insert(ModuleName, OneOrMore, !ImportUseMap)
    ).

%---------------------%

:- pred record_avails_acc(list(item_avail)::in,
    module_import_or_use_map::in, module_import_or_use_map::out) is det.

record_avails_acc([], !ImportUseMap).
record_avails_acc([Avail | Avails], !ImportUseMap) :-
    (
        Avail = avail_import(avail_import_info(ModuleName, Context, _SeqNum)),
        ImportOrUse = import_decl
    ;
        Avail = avail_use(avail_use_info(ModuleName, Context, _SeqNum)),
        ImportOrUse = use_decl
    ),
    IoUC = import_or_use_context(ImportOrUse, Context),
    ( if map.search(!.ImportUseMap, ModuleName, OneOrMore0) then
        OneOrMore0 = one_or_more(HeadIoUC, TailIoUCs),
        OneOrMore = one_or_more(IoUC, [HeadIoUC | TailIoUCs]),
        map.det_update(ModuleName, OneOrMore, !ImportUseMap)
    else
        OneOrMore = one_or_more(IoUC, []),
        map.det_insert(ModuleName, OneOrMore, !ImportUseMap)
    ),
    record_avails_acc(Avails, !ImportUseMap).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

check_import_accessibility(ParseTreeModuleSrc, ImportAccessibilityInfo,
        !:Specs) :-
    ModuleName = ParseTreeModuleSrc ^ ptms_module_name,
    ModuleNameContext = ParseTreeModuleSrc ^ ptms_module_name_context,
    ImportAccessibilityInfo = import_accessibility_info(ReadModules,
        SeenIncludes, InclMap, SrcIntImportUseMap, SrcImpImportUseMap,
        AncestorImportUseMap),

    !:Specs = [],
    % The current module is not an import, but this is the obvious place
    % to check whether its purported parent module (if any) actually
    % includes it.
    report_any_missing_includes(ReadModules, SeenIncludes, InclMap,
        ModuleName, [ModuleNameContext], !Specs),
    map.foldl(
        report_any_missing_includes_for_imports(ReadModules,
            SeenIncludes, InclMap),
        SrcIntImportUseMap, !Specs),
    map.foldl(
        report_any_missing_includes_for_imports(ReadModules,
            SeenIncludes, InclMap),
        SrcImpImportUseMap, !Specs),

    % When checking whether avail declarations (i.e. import_module
    % and use_module declarations) in the interface section
    % have an accessible avail declaration for their ancestor modules,
    % the places where those declarations may occur include not just
    % the interface of the module itself, but also the contents of
    % the .int0 interface files of ancestor modules.
    map.union(append_one_or_more, SrcIntImportUseMap, AncestorImportUseMap,
        SrcIntAncImportUseMap),
    map.foldl(
        find_any_missing_ancestor_imports(ModuleName, poa_parent,
            SrcIntAncImportUseMap),
        SrcIntImportUseMap, map.init, SrcIntMissingAncestorMap),

    % When checking whether avail declarations in the implementation section
    % have an accessible avail declaration for their ancestor modules,
    % the places where those declarations may occur include not just
    % the implementation section of the module itself, but also every place
    % that the interface of the module has access to.
    map.union(append_one_or_more, SrcIntAncImportUseMap, SrcImpImportUseMap,
        SrcIntImpImportUseMap),
    map.foldl(
        find_any_missing_ancestor_imports(ModuleName, poa_parent,
            SrcIntImpImportUseMap),
        SrcImpImportUseMap, map.init, SrcImpMissingAncestorMap0),

    % If we generate a message about a missing import (or use) for a module
    % in the interface section, do not generate another message for it
    % also missing in the implementation section, because adding an
    % import_module or use_module declaration for it to the interface
    % will also cure the problem in the implementation section.
    map.keys(SrcIntMissingAncestorMap, SrcIntMissingAncestors),
    map.delete_list(SrcIntMissingAncestors,
        SrcImpMissingAncestorMap0, SrcImpMissingAncestorMap),

    map.foldl(
        report_missing_ancestor(ModuleName,
            missing_in_src_int(SrcImpImportUseMap)),
        SrcIntMissingAncestorMap, !Specs),
    map.foldl(
        report_missing_ancestor(ModuleName, missing_in_src_imp),
        SrcImpMissingAncestorMap, !Specs).

%---------------------------------------------------------------------------%

:- type parent_or_ancestor
    --->    poa_parent
    ;       poa_ancestor.

:- type import_and_or_use
    --->    import_only
    ;       use_only
    ;       import_and_use.

:- type missing_ancestor_info
    --->    missing_ancestor_info(
                mai_modules         :: set(module_name),
                mai_max_depth       :: parent_or_ancestor,
                mai_import_use      :: import_and_or_use,
                mai_least_context   :: term_context
            ).

:- type missing_ancestor_map == map(module_name, missing_ancestor_info).

    % find_any_missing_ancestor_imports(CurrentModule, ParentOrAncestor,
    %   ImportUseMap, ImportedModule, IoUCs, !MissingAncestorMap):
    %
    % If there are any ancestors of ImportedModule for which there is
    % neither an explicit import_module or use_module declaration in
    % ImportUseMap, nor an implicit declaration by virtue of that ancestor
    % module being an ancestor of CurrentModule as well, then record
    % the fact that we are missing an import or use of that ancestor.
    %
    % We don't generate an error message right here, so that if several
    % imported modules are missing the same ancestor, we can generate
    % just one message for that missing ancestor.
    %
    % The other inputs allow us to record information that will make
    % the eventual error message more informative.
    %
:- pred find_any_missing_ancestor_imports(module_name::in,
    parent_or_ancestor::in, module_import_or_use_map::in,
    module_name::in, one_or_more(import_or_use_context)::in,
    missing_ancestor_map::in, missing_ancestor_map::out) is det.

find_any_missing_ancestor_imports(CurrentModule, ParentOrAncestor,
        ImportUseMap, ImportedModule, IoUCs, !MissingAncestorMap) :-
    (
        ImportedModule = qualified(ParentModule, _SubModule),
        ( if
            (
                % Does CurrentModule import ParentModule explicitly?
                map.search(ImportUseMap, ParentModule, _ParentIoUCs)
            ;
                % Is ParentModule the same as CurrentModule, or a parent
                % or an ancestor of CurrentModule? If yes, then CurrentModule
                % imports it implicitly.
                is_submodule(CurrentModule, ParentModule)
            )
        then
            true
        else
            IoUCs = one_or_more(HeadIoUC, TailIoUCs),
            ( if
                map.search(!.MissingAncestorMap, ParentModule,
                    MissingAncestorInfo0)
            then
                MissingAncestorInfo0 = missing_ancestor_info(ChildModules0,
                    PoA0, ImportAndOrUse0, LeastContext0),
                set.insert(ImportedModule, ChildModules0, ChildModules),
                ( if
                    PoA0 = poa_parent,
                    ParentOrAncestor = poa_ancestor
                then
                    PoA = poa_ancestor
                else
                    PoA = PoA0
                ),
                update_iu_and_least_context(HeadIoUC,
                    ImportAndOrUse0, ImportAndOrUse1,
                    LeastContext0, LeastContext1),
                list.foldl2(update_iu_and_least_context, TailIoUCs,
                    ImportAndOrUse1, ImportAndOrUse,
                    LeastContext1, LeastContext),
                MissingAncestorInfo = missing_ancestor_info(ChildModules,
                    PoA, ImportAndOrUse, LeastContext),
                map.det_update(ParentModule, MissingAncestorInfo,
                    !MissingAncestorMap)
            else
                ChildModules = set.make_singleton_set(ImportedModule),
                HeadIoUC = import_or_use_context(HeadImportOrUse, HeadContext),
                (
                    HeadImportOrUse = import_decl,
                    ImportAndOrUse0 = import_only
                ;
                    HeadImportOrUse = use_decl,
                    ImportAndOrUse0 = use_only
                ),
                list.foldl2(update_iu_and_least_context, TailIoUCs,
                    ImportAndOrUse0, ImportAndOrUse,
                    HeadContext, LeastContext),
                MissingAncestorInfo = missing_ancestor_info(ChildModules,
                    ParentOrAncestor, ImportAndOrUse, LeastContext),
                map.det_insert(ParentModule, MissingAncestorInfo,
                    !MissingAncestorMap),
                find_any_missing_ancestor_imports(CurrentModule, poa_ancestor,
                    ImportUseMap, ParentModule, IoUCs, !MissingAncestorMap)
            )
        )
    ;
        ImportedModule = unqualified(_)
        % For modules without parent modules, accessibility is moot.
    ).

:- pred update_iu_and_least_context(import_or_use_context::in,
    import_and_or_use::in, import_and_or_use::out,
    term_context::in, term_context::out) is det.

update_iu_and_least_context(IoUC, !ImportAndOrUse, !LeastContext) :-
    IoUC = import_or_use_context(ImportOrUse, Context),
    (
        ImportOrUse = import_decl,
        (
            !.ImportAndOrUse = import_only
        ;
            ( !.ImportAndOrUse = use_only
            ; !.ImportAndOrUse = import_and_use
            ),
            !:ImportAndOrUse = import_and_use
        )
    ;
        ImportOrUse = use_decl,
        (
            !.ImportAndOrUse = use_only
        ;
            ( !.ImportAndOrUse = import_only
            ; !.ImportAndOrUse = import_and_use
            ),
            !:ImportAndOrUse = import_and_use
        )
    ),
    ( if
        compare((<), Context, !.LeastContext),
        not is_dummy_context(Context)
    then
        !:LeastContext = Context
    else
        true
    ).

%---------------------------------------------------------------------------%
%
% Error reporting.
%

:- type missing_where
    --->    missing_in_src_int(module_import_or_use_map)
    ;       missing_in_src_imp
    ;       missing_in_non_src.

:- pred report_missing_ancestor(module_name::in,
    missing_where::in, module_name::in, missing_ancestor_info::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_missing_ancestor(ModuleName, MissingWhere,
        MissingModuleName, SrcIntInfo, !Specs) :-
    SrcIntInfo = missing_ancestor_info(DescendantModuleNamesSet, MaxDepth,
        ImportAndOrUse, LeastContext),
    set.to_sorted_list(DescendantModuleNamesSet, DescendantModuleNames),
    ( MaxDepth = poa_parent, ChildOrDescendant = "child"
    ; MaxDepth = poa_ancestor, ChildOrDescendant = "descendant"
    ),
    (
        ImportAndOrUse = import_only,
        DeclPieces = [decl("import_module")]
    ;
        ImportAndOrUse = use_only,
        DeclPieces = [decl("use_module")]
    ;
        ImportAndOrUse = import_and_use,
        DeclPieces = [decl("import_module"), words("and"), decl("use_module")]
    ),
    (
        MissingWhere = missing_in_src_int(_),
        InTheInterface = [words("in the interface")]
    ;
        ( MissingWhere = missing_in_src_imp
        ; MissingWhere = missing_in_non_src
        ),
        InTheInterface = []
    ),
    DescendantPieces = list.map(wrap_module_name, DescendantModuleNames),
    ModuleS = choose_number(DescendantModuleNames, "module", "modules"),
    DeclarationS = choose_number(DescendantModuleNames,
        "declaration", "declarations"),
    MainPieces = [words("In module"), qual_sym_name(ModuleName),
        suffix(":"), words("error:"), nl,
        words("the absence of an"), decl("import_module"), words("or"),
        decl("use_module"), words("declaration for"),
        qual_sym_name(MissingModuleName)] ++ InTheInterface ++
        [words("prevents access to the")] ++
        DeclPieces ++ [words(DeclarationS)] ++ InTheInterface ++
        [words("for its"), words(ChildOrDescendant), words(ModuleS)] ++
        component_list_to_pieces("and", DescendantPieces) ++
        [suffix("."), nl],
    MainMsg = simplest_msg(LeastContext, MainPieces),
    ( if
        MissingWhere = missing_in_src_int(SrcImpImportUseMap),
        map.search(SrcImpImportUseMap, MissingModuleName, IoUCs)
    then
        % XXX _TailIoUCs
        IoUCs = one_or_more(HeadIoUC, _TailIoUCs),
        HeadIoUC = import_or_use_context(ImportOrUse, ImpContext),
        ( ImportOrUse = import_decl, ImportOrUseDecl = "import_module"
        ; ImportOrUse = use_decl, ImportOrUseDecl = "use_module"
        ),
        ImpPieces = [words("Adding such a declaration would obsolete"),
            words("this"), decl(ImportOrUseDecl), words("declaration"),
            words("in the implementation section."), nl],
        ImpMsg = simplest_msg(ImpContext, ImpPieces),
        Msgs = [MainMsg, ImpMsg]
    else
        Msgs = [MainMsg]
    ),
    Spec = error_spec($pred, severity_error, phase_parse_tree_to_hlds, Msgs),
    !:Specs = [Spec | !.Specs].

%---------------------%

:- pred report_any_missing_includes_for_imports(set(module_name)::in,
    seen_includes::in, module_inclusion_map::in,
    module_name::in, one_or_more(import_or_use_context)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_any_missing_includes_for_imports(ReadModules, SeenIncludes, InclMap,
        ModuleName, IoUCs, !Specs) :-
    IoUCs = one_or_more(HeadIoUC, TailIoUCs),
    Contexts = list.map(project_out_import_or_use, [HeadIoUC | TailIoUCs]),
    report_any_missing_includes(ReadModules, SeenIncludes, InclMap,
        ModuleName, Contexts, !Specs).

    % report_any_missing_includes(ReadModules, InclMap, Module, Contexts,
    %   !Specs):
    %
    % If Module is a submodule of ParentModule but we haven't seen
    % an include_module declaration for Module in ParentModule even though
    % we should have seen it is exists (because we have read an interface
    % file for ParentModule, which should contain all its include_module
    % declarations), then add an error message reporting this fact to !Specs.
    %
:- pred report_any_missing_includes(set(module_name)::in,
    seen_includes::in, module_inclusion_map::in,
    module_name::in, list(term_context)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_any_missing_includes(ReadModules, SeenIncludes, InclMap,
        Module, Contexts, !Specs) :-
    (
        Module = qualified(ParentModule, SubModule),
        ( if map.search(InclMap, Module, IncludeContexts) then
            % Module *has* its include in ParentModule, ...
            IncludeContexts =
                one_or_more(HeadIncludeContext, TailIncludeContexts),
            IncludeContextsList = [HeadIncludeContext | TailIncludeContexts],
            ( if any_true(is_non_abstract_include, IncludeContextsList) then
                % ... and it is visible here.
                true
            else
                % ... and it is NOT visible here.
                list.foldl(report_abstract_include(ParentModule, SubModule),
                    Contexts, !Specs)
            )
        else
            % We have not seen Module's include in ParentModule.
            ( if set.contains(ReadModules, ParentModule) then
                % We have read item blocks from ParentModule, and they
                % *should* have included its include_module declarations.
                list.foldl(
                    report_missing_include(SeenIncludes,
                        ParentModule, SubModule),
                    Contexts, !Specs)
            else
                % We have read not any item blocks from ParentModule.
                % For all we know, ParentModule *may* contain an include
                % for Module; we just don't know. Reporting an error
                % would be misleading.
                %
                % If we had imported ParentModule, we would have read
                % item blocks from one of its interface files. We will
                % report the missing import. If the include is truly missing
                % in ParentModule, we will discover and report that fact
                % when the missing import of ParentModule in the *current*
                % module is fixed by the programmer.
                true
            )
        ),
        report_any_missing_includes(ReadModules, SeenIncludes, InclMap,
            ParentModule, Contexts, !Specs)
    ;
        Module = unqualified(_)
        % For modules without parent modules, accessibility is moot.
    ).

:- pred report_abstract_include(module_name::in, string::in, term_context::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_abstract_include(ParentModule, SubModule, Context, !Specs) :-
    Pieces = [words("Error:"),
        words("module"), qual_sym_name(ParentModule),
        words("has a submodule named"), quote(SubModule), suffix(","),
        words("but it is visible only to its other submodules."), nl],
    Spec = simplest_spec($pred, severity_error, phase_parse_tree_to_hlds,
        Context, Pieces),
    !:Specs = [Spec | !.Specs].

:- pred report_missing_include(seen_includes::in, module_name::in, string::in,
    term_context::in, list(error_spec)::in, list(error_spec)::out) is det.

report_missing_include(SeenIncludes, ParentModule, SubModule, Context,
        !Specs) :-
    (
        SeenIncludes = seen_all_includes,
        SubmodulePieces = [words("a submodule")]
    ;
        SeenIncludes = seen_only_int_includes,
        SubmodulePieces = [words("a visible submodule")]
    ),
    Pieces = [words("Error:"), words("module"),
        qual_sym_name(ParentModule), words("does not have")] ++
        SubmodulePieces ++ [words("named"), quote(SubModule), suffix("."), nl],
    Spec = simplest_spec($pred, severity_error, phase_parse_tree_to_hlds,
        Context, Pieces),
    !:Specs = [Spec | !.Specs].

%---------------------------------------------------------------------------%
%
% Utility operations.
%

:- pred append_one_or_more(one_or_more(T)::in, one_or_more(T)::in,
    one_or_more(T)::out) is det.

append_one_or_more(A, B, AB) :-
    A = one_or_more(HeadA, TailA),
    B = one_or_more(HeadB, TailB),
    AB = one_or_more(HeadA, TailA ++ [HeadB | TailB]).

:- pred is_non_abstract_include(include_context::in) is semidet.

is_non_abstract_include(IncludeContext) :-
    IncludeContext = include_context(MaybeAbstractInclude, _Context),
    MaybeAbstractInclude = non_abstract_section.

:- func project_out_import_or_use(import_or_use_context) = term_context.

project_out_import_or_use(import_or_use_context(_, Context)) = Context.

:- func wrap_module_name(sym_name) = format_piece.

wrap_module_name(Module) = qual_sym_name(Module).

%---------------------------------------------------------------------------%
:- end_module parse_tree.check_import_accessibility.
%---------------------------------------------------------------------------%
