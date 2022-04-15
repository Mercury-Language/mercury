%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2022 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: module_baggage.m.
%
% This module contains the information we gather while reading in a module
% other than the module's parse tree.
%
%---------------------------------------------------------------------------%

:- module parse_tree.module_baggage.
:- interface.

:- import_module libs.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module libs.timestamp.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.error_util.
:- import_module parse_tree.file_kind.
:- import_module parse_tree.parse_error.
:- import_module parse_tree.prog_item.

:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module set.

%---------------------------------------------------------------------------%

    % The "baggage" of a module is the information the compiler may need
    % to know about the process of reading in both that module, and the other
    % modules needed to augment it.
    %
:- type module_baggage
    --->    module_baggage(
                % The name of the source file and directory
                % containing the module source.
                %
                % Currently, the source_file dir field is *always* set
                % to dir.this_directory, so strictly speaking, this field
                % is redundant. However, there are arguments for keeping it.
                %
                % 1. In the future, we may want to support reading in
                %    source files from places other than the current dir.
                %
                % 2. The source_file_dir field in the module_dep_summary
                %    structure is set by searching for a .module_dep summary
                %    file in a search path, so its contents need not be
                %    the current directory. Keeping it here as well
                %    is consistent with that.
                %
                % 3. A typical compiler execution creates few values
                %    of this type, so the cost of keeping this field
                %    is negligible.
                mb_source_file_name         :: file_name,
                mb_source_file_dir          :: dir_name,

                % The name of the top-level module in the above source file.
                mb_source_file_module_name  :: module_name,

                % The other modules included in the same source file,
                % if this module is the top-level module in its file.
                %
                % Invariant: this is top_module(...) if and only if
                % source_file_name = source_file_module_name.
                mb_maybe_top_module         :: maybe_top_module,

                % If we are doing smart recompilation, we need to keep
                % the timestamps of the modules read in.
                mb_maybe_timestamp_map      :: maybe(module_timestamp_map),

                mb_grabbed_file_map         :: grabbed_file_map,

                % Whether an error has been encountered when reading in
                % this module.
                mb_specs                    :: list(error_spec),
                mb_errors                   :: read_module_errors
            ).

    % A "burdened" augmented compilation unit contains, besides the augmented
    % compilation unit itself, the "baggage" of the module parse trees
    % (source, interface file, and optimization file) that the augmented
    % compilation unit consists of.
    %
:- type burdened_aug_comp_unit
    --->    burdened_aug_comp_unit(
                bacu_baggage    :: module_baggage,
                bacu_acu        :: aug_compilation_unit
            ).

:- type burdened_module
    --->    burdened_module(
                bm_baggage      :: module_baggage,
                bm_module       :: parse_tree_module_src
            ).

%---------------------------------------------------------------------------%
% The following sections define the types holding baggage components,
% and operations on those types.
%---------------------------------------------------------------------------%

:- type maybe_top_module
    --->    top_module(set(module_name))
            % This module is the top module in its source file,
            % and the argument gives the names of all its descendants
            % (i.e. its children, its children's children, and so on).
    ;       not_top_module.
            % This module is NOT the top module in its source file.

    % Return the module's nested childred IF it is a top module.
    % Otherwise, return the empty set or list.
    %
:- func get_nested_children_of_top_module(maybe_top_module) =
    set(module_name).
:- func get_nested_children_list_of_top_module(maybe_top_module) =
    list(module_name).

%---------------------------------------------------------------------------%

    % When doing smart recompilation, we record, for each module,
    % which of its versions (source .m file, generated .int0/.int3/.int2/.int
    % file, or generated .opt/.transopt file) we read, and the modification
    % time of the file.
    %
    % We also record whether code in the source module whose compilation
    % we are concerned with may refer to items in the read-in module
    % using names that are only partially qualified, or not.
    %
    % What follows in the next two paragraphs is conjecture by wangp,
    % but I (zs) agree that it is likely a correct description of
    % what we use this information for.
    %
    % Suppose a module msrc gets added to it a :- use_module declaration
    % for a module m1, when previously it did not import module m1 in any
    % shape or form. If msrc compiled cleanly until now, then a non-fully-
    % qualified reference (e.g. f) to a item (type, data constructor,
    % predicate etc) could not have been a reference to an item in m1,
    % even if m1 defines an item named f of the relevant kind.
    %
    % On the other hand, consider a version of the situation above
    % in which msrc gains access to m1 via an :- import_module declaration.
    % In that case, if m1 defines an item named f, then a non-fully-qualified
    % reference to that name *could* refer to that item in m1. If msrc
    % compiled cleanly before, this f must have been resolved to refer
    % to an item defined in some other module, but now that msrc imports m1,
    % the non-fully-qualified reference to f has become ambiguous.
    % This means that msrc must be recompiled, so that the recompilation
    % may detect and report any such ambiguities.
    %
    % XXX The handling of the map looks wrong to me (zs), because
    % when we read in e.g. mod1.int, we simply overwrite any existing entry
    % in the module_timestamp_map for mod1. I see no documented argument
    % anywhere for any of the following propositions, which could each make
    % the above the right thing to do.
    %
    % - Proposition 1: when we add an entry for a module, the map
    %   cannot contain any previous entry for that module.
    %   I (zs) think this is the correct answer, but have no proof.
    %
    % - Proposition 2a: when we add an entry for a module, the map
    %   *can* contain a previous entry for the module, but for
    %   a file of that module than contains at most as much information
    %   as the previously-read-in file (as e.g. a .int2 file cannot
    %   contain more information than a .int file for the same
    %   module).
    %
    % - Proposition 2b: when we add an entry for a module, the map
    %   *can* contain a previous entry for the module, the avail_kind
    %   field in the new entry is at least as restrictive as in
    %   the old entry.
    %
:- type module_timestamp_map == map(module_name, module_timestamp).
:- type module_timestamp
    --->    module_timestamp(
                mts_file_kind       :: file_kind,
                mts_timestamp       :: timestamp,
                mts_avail_kind      :: recomp_avail
            ).

:- type recomp_avail
    --->    recomp_avail_src
            % The module is the souurce module.
    ;       recomp_avail_int_import
            % There was an ":- import_module" in the interface section,
            % or in an ancestor module. In either case, references to
            % items defined by the read-in module may be made anywhere
            % in the source module.
    ;       recomp_avail_imp_import
            % There was an ":- import_module" in the implementation section,
            % or in an optimization file. In either case, references to
            % items defined by the read-in module may be made only in
            % in the implementation section of the source module.
    ;       recomp_avail_int_use
            % There was an ":- use_module" in the interface section,
            % or in an ancestor module. In either case, references to
            % items defined by the read-in module may be made anywhere
            % in the source module.
    ;       recomp_avail_imp_use
            % There was an ":- use_module" in the implementation section,
            % or in an optimization file. In either case, references to
            % items defined by the read-in module may be made only in
            % in the implementation section of the source module.
    ;       recomp_avail_int_use_imp_import.
            % There was an ":- use_module" in the interface section
            % and an ":- import_module" in the implementation section.

%---------------------------------------------------------------------------%

:- type grabbed_file
    --->    gf_src(parse_tree_module_src)
    ;       gf_int0(parse_tree_int0, read_why_int0)
    ;       gf_int1(parse_tree_int1, read_why_int1)
    ;       gf_int2(parse_tree_int2, read_why_int2)
    ;       gf_int3(parse_tree_int3, read_why_int3).

    % This maps each module to the interface file (or in one case,
    % the source file) that contains the most information about the module.
    % So for example, when compiling module A which imports module B,
    % and module B also imports module A (a circular dependency), then
    % the presence of a gf_src entry for module A will tell us that there is
    % no point in reading in A.int2.
    %
:- type grabbed_file_map == map(module_name, grabbed_file).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % This predicate is used by
    %
    %   deps_map.m
    %   generate_dep_d_files.m
    %   make.module_dep_file.m
    %
    % for building dependency maps between modules. The aug_compilation_units
    % it builds have only one field filled in, the one containing
    % the parse_tree_module_src.
    %
    % XXX Do the callers fill in the other fields, or do they need *only*
    % the parse_tree_module_src? If the latter, we should return only *that*.
    %
:- pred parse_tree_src_to_burdened_module_list(globals::in,
    file_name::in, parse_tree_src::in, read_module_errors::in,
    list(error_spec)::in, list(error_spec)::out,
    list(burdened_module)::out) is det.

%---------------------------------------------------------------------------%
%
% Predicates for getting information from parse_tree_module_srcs.
%

:- pred parse_tree_module_src_get_int_imp_deps(parse_tree_module_src::in,
    set(module_name)::out, set(module_name)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.item_util.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.split_parse_tree_src.

:- import_module cord.
:- import_module dir.
:- import_module one_or_more.
:- import_module term.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

get_nested_children_of_top_module(MaybeTopModule) = Modules :-
    (
        MaybeTopModule = top_module(Modules)
    ;
        MaybeTopModule = not_top_module,
        set.init(Modules)
    ).

get_nested_children_list_of_top_module(MaybeTopModule) = Modules :-
    (
        MaybeTopModule = top_module(ModulesSet),
        Modules = set.to_sorted_list(ModulesSet)
    ;
        MaybeTopModule = not_top_module,
        Modules = []
    ).

%---------------------------------------------------------------------------%

parse_tree_src_to_burdened_module_list(Globals, SourceFileName,
        ParseTreeSrc, ReadModuleErrors, !Specs, BurdenedModules) :-
    % XXX This predicate and augment_and_process_all_submodules
    % do very similar jobs. See whether we can unify the two.
    split_into_compilation_units_perform_checks(Globals, ParseTreeSrc,
        ParseTreeModuleSrcs, !Specs),
    ParseTreeSrc = parse_tree_src(TopModuleName, _, _),
    AllModuleNames = set.list_to_set(
        list.map(parse_tree_module_src_project_name, ParseTreeModuleSrcs)),
    BaggageSpecs0 = [],
    % XXX This will create a separate grabbed_file_map in each element
    % or BurdenedAugCompUnitList. It would be better for our caller
    % to thread a single grabbed_file_map through the augment processing
    % of all its aug_compilation_units.
    list.map(
        maybe_nested_init_burdened_module(SourceFileName,
            TopModuleName, AllModuleNames, BaggageSpecs0, ReadModuleErrors),
        ParseTreeModuleSrcs, BurdenedModules).

:- pred maybe_nested_init_burdened_module(file_name::in,
    module_name::in, set(module_name)::in,
    list(error_spec)::in, read_module_errors::in,
    parse_tree_module_src::in, burdened_module::out) is det.

maybe_nested_init_burdened_module(SourceFileName,
        SourceFileModuleName, AllModuleNames, Specs, Errors,
        ParseTreeModuleSrc, BurdenedModule) :-
    ModuleName = ParseTreeModuleSrc ^ ptms_module_name,
    ( if ModuleName = SourceFileModuleName then
        set.delete(ModuleName, AllModuleNames, NestedModuleNames),
        MaybeTopModule = top_module(NestedModuleNames)
    else
        MaybeTopModule = not_top_module
    ),
    MaybeTimestampMap = maybe.no,
    GrabbedFileMap = map.singleton(ModuleName, gf_src(ParseTreeModuleSrc)),
    Baggage = module_baggage(SourceFileName, dir.this_directory,
        SourceFileModuleName, MaybeTopModule, MaybeTimestampMap,
        GrabbedFileMap, Specs, Errors),
    BurdenedModule = burdened_module(Baggage, ParseTreeModuleSrc).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

parse_tree_module_src_get_int_imp_deps(ParseTreeModuleSrc, IntDeps, ImpDeps) :-
    ImportUseMap = ParseTreeModuleSrc ^ ptms_import_use_map,
    map.foldl2(add_module_dep, ImportUseMap,
        set.init, IntDeps, set.init, ImpDeps).

:- pred add_module_dep(module_name::in, maybe_implicit_import_and_or_use::in,
    set(module_name)::in, set(module_name)::out,
    set(module_name)::in, set(module_name)::out) is det.

add_module_dep(ModuleName, MaybeImplicit, !IntDeps, !ImpDeps) :-
    (
        MaybeImplicit = explicit_avail(SectionImportUse),
        Section = section_import_and_or_use_int_imp(SectionImportUse)
    ;
        MaybeImplicit = implicit_avail(Implicit, MaybeExplicit),
        (
            ( Implicit = implicit_int_import
            ; Implicit = implicit_int_use
            ),
            ImplicitSection = ms_interface
        ;
            Implicit = implicit_imp_use,
            ImplicitSection = ms_implementation
        ),
        (
            MaybeExplicit = no,
            Section = ImplicitSection
        ;
            MaybeExplicit = yes(SectionImportUse),
            ExplicitSection =
                section_import_and_or_use_int_imp(SectionImportUse),
            ( if
                ( ImplicitSection = ms_interface
                ; ExplicitSection = ms_interface
                )
            then
                Section = ms_interface
            else
                Section = ms_implementation
            )
        )
    ),
    (
        Section = ms_interface,
        set.insert(ModuleName, !IntDeps)
    ;
        Section = ms_implementation,
        set.insert(ModuleName, !ImpDeps)
    ).

:- func section_import_and_or_use_int_imp(section_import_and_or_use)
    = module_section.

section_import_and_or_use_int_imp(SectionImportUse) = Section :-
    (
        ( SectionImportUse = int_import(_)
        ; SectionImportUse = int_use(_)
        ; SectionImportUse = int_use_imp_import(_, _)
        ),
        Section = ms_interface
    ;
        ( SectionImportUse = imp_import(_)
        ; SectionImportUse = imp_use(_)
        ),
        Section = ms_implementation
    ).

%---------------------------------------------------------------------------%
:- end_module parse_tree.module_baggage.
%---------------------------------------------------------------------------%
