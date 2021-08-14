%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: module_imports.m.
% Original author: fjh.
% Author of current version: zs.
%
% This module contains the main data structure we use while augmenting
% a raw compilation unit. It records all the things that are imported,
% directly or indirectly, by the original raw compilation unit.
%
%---------------------------------------------------------------------------%

:- module parse_tree.module_imports.
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
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_item.

:- import_module io.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module set.

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
:- func get_nested_children_of_top_module(maybe_top_module) = set(module_name).
:- func get_nested_children_list_of_top_module(maybe_top_module) =
    list(module_name).

%---------------------------------------------------------------------------%

    % The `module_and_imports' structure holds information about
    % a module and the modules that it imports. We build this structure up
    % as we go along.
    %
:- type module_and_imports.

%---------------------------------------------------------------------------%

:- type module_baggage
    --->    module_baggage(
                % The name of the source file and directory
                % containing the module source.
                %
                % Currently, the source_file dir field is *always* set
                % to dir.this_directory, so strictly speaking, this field
                % is redundant. However, there arguments for keeping it.
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

%---------------------------------------------------------------------------%

:- type module_imports_and_baggage
    --->    module_imports_and_baggage(
                miab_baggage    :: module_baggage,
                miab_mai        :: module_and_imports
            ).

%---------------------------------------------------------------------------%
%
% The predicates that create module_and_imports structures.
%

    % This predicate is used by
    %
    %   deps_map.m
    %   generate_dep_d_files.m
    %   make.module_dep_file.m
    %
    % for building dependency maps between modules. The module_and_imports
    % structures it builds are not fully complete; only the fields
    % needed for that task are filled in.
    %
:- pred parse_tree_src_to_module_imports_and_baggage_list(globals::in,
    file_name::in, parse_tree_src::in, read_module_errors::in,
    list(error_spec)::in, list(error_spec)::out,
    list(module_imports_and_baggage)::out) is det.

:- pred rebuild_module_and_imports_for_dep_file(
    module_baggage::in, module_baggage::out,
    module_and_imports::in, module_and_imports::out) is det.

    % init_module_and_imports(ParseTreeModuleSrc, ModuleAndImports):
    %
    % Initialize a module_and_imports structure.
    %
    % We do this just after we have read in a parse_tree_module_src.
    % Later code, mostly in grab_modules.m but in some other modules as well,
    % then calls the module_and_imports_{add,set}_* predicates above
    % to record more information (mostly from read-in interface files)
    % to the module_and_imports structure. When all such modifications
    % are done, the module_and_imports_get_aug_comp_unit predicate
    % will extract the augmented compilation unit from the updated
    % module_and_imports structure.
    %
:- pred init_module_and_imports(parse_tree_module_src::in,
    module_and_imports::out) is det.

%---------------------------------------------------------------------------%
%
% Getter and setter predicates for the module_and_imports structure.
%

:- pred module_and_imports_get_parse_tree_module_src(module_and_imports::in,
    parse_tree_module_src::out) is det.
:- pred module_and_imports_get_ancestor_int_specs(module_and_imports::in,
    map(module_name, ancestor_int_spec)::out) is det.
:- pred module_and_imports_get_direct_int_specs(module_and_imports::in,
    map(module_name, direct_int_spec)::out) is det.
:- pred module_and_imports_get_indirect_int_specs(module_and_imports::in,
    map(module_name, indirect_int_spec)::out) is det.
:- pred module_and_imports_get_plain_opts(module_and_imports::in,
    map(module_name, parse_tree_plain_opt)::out) is det.
:- pred module_and_imports_get_trans_opts(module_and_imports::in,
    map(module_name, parse_tree_trans_opt)::out) is det.
:- pred module_and_imports_get_int_for_opt_specs(module_and_imports::in,
    map(module_name, int_for_opt_spec)::out) is det.
:- pred module_and_imports_get_type_repn_specs(module_and_imports::in,
    map(module_name, type_repn_spec)::out) is det.

%---------------------------------------------------------------------------%
%
% Predicates for getting information from module_and_imports structures.
%

:- pred module_and_imports_get_module_name(module_and_imports::in,
    module_name::out) is det.
:- pred module_and_imports_get_children(module_and_imports::in,
    list(module_name)::out) is det.
:- pred module_and_imports_get_children_set(module_and_imports::in,
    set(module_name)::out) is det.
:- pred module_and_imports_get_int_imp_deps(module_and_imports::in,
    set(module_name)::out, set(module_name)::out) is det.
:- pred module_and_imports_get_fact_tables(module_and_imports::in,
    set(string)::out) is det.
:- pred module_and_imports_get_fim_specs(module_and_imports::in,
    set(fim_spec)::out) is det.
:- pred module_and_imports_get_foreign_include_file_infos(
    module_and_imports::in, set(foreign_include_file_info)::out) is det.

%---------------------------------------------------------------------------%
%
% Predicates for adding information to module_and_imports structures.
%

:- pred module_and_imports_add_ancestor_int_spec(ancestor_int_spec::in,
    module_and_imports::in, module_and_imports::out) is det.
:- pred module_and_imports_add_direct_int_spec(direct_int_spec::in,
    module_and_imports::in, module_and_imports::out) is det.
:- pred module_and_imports_add_indirect_int_spec(indirect_int_spec::in,
    module_and_imports::in, module_and_imports::out) is det.
:- pred module_and_imports_add_plain_opt(parse_tree_plain_opt::in,
    module_and_imports::in, module_and_imports::out) is det.
:- pred module_and_imports_add_trans_opt(parse_tree_trans_opt::in,
    module_and_imports::in, module_and_imports::out) is det.
:- pred module_and_imports_add_int_for_opt_spec(int_for_opt_spec::in,
    module_and_imports::in, module_and_imports::out) is det.
:- pred module_and_imports_add_type_repn_spec(type_repn_spec::in,
    module_and_imports::in, module_and_imports::out) is det.

:- pred module_and_imports_maybe_add_module_version_numbers(
    module_name::in, maybe_version_numbers::in,
    module_and_imports::in, module_and_imports::out) is det.

%---------------------------------------------------------------------------%
%
% The predicates that return the contents of module_and_imports structures.
%

    % Return the results recorded in the module_and_imports structure.
    %
:- pred module_and_imports_get_aug_comp_unit(module_and_imports::in,
    aug_compilation_unit::out) is det.

%---------------------------------------------------------------------------%

:- type module_dep_info
    --->    module_dep_info_imports(module_imports_and_baggage)
    ;       module_dep_info_summary(module_dep_summary).

:- type module_dep_summary
    --->    module_dep_summary(
                mds_source_file_name        :: string,
                mds_source_file_dir         :: string,
                mds_source_file_module_name :: module_name,
                mds_module_name             :: module_name,
                mds_children                :: set(module_name),
                mds_maybe_top_module        :: maybe_top_module,
                mds_int_deps                :: set(module_name),
                mds_imp_deps                :: set(module_name),
                mds_fact_table_file_names   :: set(string),
                mds_fims                    :: set(fim_spec),
                mds_foreign_include_files   :: set(foreign_include_file_info),
                mds_contains_foreign_code   :: contains_foreign_code,
                mds_contains_foreign_export :: contains_foreign_export
            ).

:- pred module_dep_info_get_source_file_name(module_dep_info::in,
    string::out) is det.
:- pred module_dep_info_get_source_file_dir(module_dep_info::in,
    string::out) is det.
:- pred module_dep_info_get_source_file_module_name(module_dep_info::in,
    module_name::out) is det.
:- pred module_dep_info_get_module_name(module_dep_info::in,
    module_name::out) is det.
:- pred module_dep_info_get_children(module_dep_info::in,
    set(module_name)::out) is det.
:- pred module_dep_info_get_maybe_top_module(module_dep_info::in,
    maybe_top_module::out) is det.
:- pred module_dep_info_get_int_deps(module_dep_info::in,
    set(module_name)::out) is det.
:- pred module_dep_info_get_imp_deps(module_dep_info::in,
    set(module_name)::out) is det.
:- pred module_dep_info_get_fact_tables(module_dep_info::in,
    set(string)::out) is det.
:- pred module_dep_info_get_fims(module_dep_info::in,
    set(fim_spec)::out) is det.
:- pred module_dep_info_get_foreign_include_files(module_dep_info::in,
    set(foreign_include_file_info)::out) is det.

%---------------------------------------------------------------------------%

:- pred write_mai_stats(io.output_stream::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.get_dependencies.
:- import_module parse_tree.item_util.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.split_parse_tree_src.
:- import_module recompilation.

:- import_module cord.
:- import_module dir.
:- import_module one_or_more.
:- import_module string.
:- import_module term.

%---------------------------------------------------------------------------%

    % When generating the dependencies (for `--generate-dependencies'), the
    % two fields that hold the direct imports do not include the imports via
    % ancestors when the module is first read in; the ancestor imports are
    % added later, once all the modules have been read in. Similarly the
    % indirect imports field is initially set to the empty list and filled
    % in later.
    %
    % When compiling or when making interface files, the same sort of thing
    % applies: initially all the fields containing module names except the
    % public children field are set to contain no modules, and then
    % we add ancestor modules and imported modules to their respective fields
    % as we process the interface files for those imported or ancestor modules.
    %
    % XXX The above comment is very old, and almost certainly out of date.
    %
    % XXX This type now contains exactly the same info as an
    % aug_compilation_unit, and will be retired soon in favour of that type.
:- type module_and_imports
    --->    module_and_imports(
                % The contents of the module and its imports.
                mai_src                 :: parse_tree_module_src,
                mai_ancestor_int_specs  :: map(module_name, ancestor_int_spec),
                mai_direct_int_specs    :: map(module_name, direct_int_spec),
                mai_indirect_int_specs  :: map(module_name, indirect_int_spec),
                % Implicitly everything in both plain and trans opt files
                % is in a single section.
                mai_plain_opts          :: map(module_name,
                                            parse_tree_plain_opt),
                mai_trans_opts          :: map(module_name,
                                            parse_tree_trans_opt),
                % Implicitly everything in int_for_opt interface files
                % is treated the same whether it is in the interface or the
                % implementation section.
                mai_int_for_opt_specs   :: map(module_name, int_for_opt_spec),

                % The contents of the .int file of the module whose augmented
                % compilation unit we will build from this module_and_imports
                % structure. We will use *only* the type representation items
                % from this .int file.
                %
                % The type of this field is has the same structure as the
                % preceding fields so that we can use the same techniques
                % to fill it, but it has only two legal states: empty,
                % and containing one type_repn_spec containing this .int file,
                % before and after the reading in of that type_repn_spec.
                %
                % XXX TYPE_REPN Check that both mmake and mmc --make know
                % that they need to build this .int file before any compiler
                % invocation that does code generation.
                mai_type_repn_specs     :: map(module_name, type_repn_spec),

                mai_version_numbers_map :: module_version_numbers_map
            ).

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

parse_tree_src_to_module_imports_and_baggage_list(Globals, SourceFileName,
        ParseTreeSrc, ReadModuleErrors, !Specs, ModuleImportsAndBaggageList) :-
    split_into_compilation_units_perform_checks(Globals, ParseTreeSrc,
        ParseTreeModuleSrcs, !Specs),
    ParseTreeSrc = parse_tree_src(TopModuleName, _, _),
    AllModuleNames = set.list_to_set(
        list.map(parse_tree_module_src_project_name, ParseTreeModuleSrcs)),
    MAISpecs0 = [],
    list.map(
        maybe_nested_init_module_imports_and_baggage(SourceFileName,
            TopModuleName, AllModuleNames, MAISpecs0, ReadModuleErrors),
        ParseTreeModuleSrcs, ModuleImportsAndBaggageList).

rebuild_module_and_imports_for_dep_file(Baggage0, Baggage,
        ModuleAndImports0, ModuleAndImports) :-
    Baggage0 = module_baggage(SourceFileName, _SourceFileDir,
        SourceFileModuleName, MaybeTopModule, _MaybeTimestampMap,
        _GrabbedFileMap, Specs, _Errors),
    ModuleAndImports0 = module_and_imports(ParseTreeModuleSrc,
        _, _, _, _, _, _, _, _),

    MaybeTimestampMap = maybe.no,
    ModuleName = ParseTreeModuleSrc ^ ptms_module_name,
    GrabbedFileMap = map.singleton(ModuleName, gf_src(ParseTreeModuleSrc)),
    set.init(Errors),
    Baggage = module_baggage(SourceFileName, dir.this_directory,
        SourceFileModuleName, MaybeTopModule, MaybeTimestampMap,
        GrabbedFileMap, Specs, Errors),

    map.init(AncestorIntSpecs),
    map.init(DirectIntSpecs),
    map.init(IndirectIntSpecs),
    map.init(PlainOpts),
    map.init(TransOpts),
    map.init(IntForOptSpecs),
    map.init(TypeRepnSpecs),
    map.init(VersionNumbers),
    ModuleAndImports = module_and_imports(ParseTreeModuleSrc,
        AncestorIntSpecs, DirectIntSpecs, IndirectIntSpecs,
        PlainOpts, TransOpts, IntForOptSpecs, TypeRepnSpecs, VersionNumbers).

%---------------------------------------------------------------------------%

:- pred maybe_nested_init_module_imports_and_baggage(file_name::in,
    module_name::in, set(module_name)::in,
    list(error_spec)::in, read_module_errors::in,
    parse_tree_module_src::in, module_imports_and_baggage::out) is det.

maybe_nested_init_module_imports_and_baggage(SourceFileName,
        SourceFileModuleName, AllModuleNames, Specs, Errors,
        ParseTreeModuleSrc, ModuleImportsAndBaggage) :-
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
    init_module_and_imports(ParseTreeModuleSrc, ModuleAndImports),
    ModuleImportsAndBaggage =
        module_imports_and_baggage(Baggage, ModuleAndImports).

init_module_and_imports(ParseTreeModuleSrc, ModuleAndImports) :-
    map.init(AncestorIntSpecs),
    map.init(DirectIntSpecs),
    map.init(IndirectIntSpecs),
    map.init(PlainOpts),
    map.init(TransOpts),
    map.init(IntForOptSpecs),
    map.init(TypeRepnSpecs),
    map.init(VersionNumbers),
    ModuleAndImports = module_and_imports(ParseTreeModuleSrc,
        AncestorIntSpecs, DirectIntSpecs, IndirectIntSpecs,
        PlainOpts, TransOpts, IntForOptSpecs, TypeRepnSpecs, VersionNumbers).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- pred module_and_imports_get_version_numbers_map(module_and_imports::in,
    module_version_numbers_map::out) is det.

module_and_imports_get_parse_tree_module_src(ModuleAndImports, X) :-
    promise_pure (
        trace [compile_time(flag("mai-stats"))] (
            semipure get_accesses(Accesses0),
            Accesses = Accesses0 ^ mf_src := accessed,
            impure set_accesses(Accesses)
        ),
        X = ModuleAndImports ^ mai_src
    ).
module_and_imports_get_ancestor_int_specs(ModuleAndImports, X) :-
    promise_pure (
        trace [compile_time(flag("mai-stats"))] (
            semipure get_accesses(Accesses0),
            Accesses = Accesses0 ^ mf_ancestor_int_specs := accessed,
            impure set_accesses(Accesses)
        ),
        X = ModuleAndImports ^ mai_ancestor_int_specs
    ).
module_and_imports_get_direct_int_specs(ModuleAndImports, X) :-
    promise_pure (
        trace [compile_time(flag("mai-stats"))] (
            semipure get_accesses(Accesses0),
            Accesses = Accesses0 ^ mf_direct_int_specs := accessed,
            impure set_accesses(Accesses)
        ),
        X = ModuleAndImports ^ mai_direct_int_specs
    ).
module_and_imports_get_indirect_int_specs(ModuleAndImports, X) :-
    promise_pure (
        trace [compile_time(flag("mai-stats"))] (
            semipure get_accesses(Accesses0),
            Accesses = Accesses0 ^ mf_indirect_int_specs := accessed,
            impure set_accesses(Accesses)
        ),
        X = ModuleAndImports ^ mai_indirect_int_specs
    ).
module_and_imports_get_plain_opts(ModuleAndImports, X) :-
    promise_pure (
        trace [compile_time(flag("mai-stats"))] (
            semipure get_accesses(Accesses0),
            Accesses = Accesses0 ^ mf_plain_opts := accessed,
            impure set_accesses(Accesses)
        ),
        X = ModuleAndImports ^ mai_plain_opts
    ).
module_and_imports_get_trans_opts(ModuleAndImports, X) :-
    promise_pure (
        trace [compile_time(flag("mai-stats"))] (
            semipure get_accesses(Accesses0),
            Accesses = Accesses0 ^ mf_trans_opts := accessed,
            impure set_accesses(Accesses)
        ),
        X = ModuleAndImports ^ mai_trans_opts
    ).
module_and_imports_get_int_for_opt_specs(ModuleAndImports, X) :-
    promise_pure (
        trace [compile_time(flag("mai-stats"))] (
            semipure get_accesses(Accesses0),
            Accesses = Accesses0 ^ mf_int_for_opt_specs := accessed,
            impure set_accesses(Accesses)
        ),
        X = ModuleAndImports ^ mai_int_for_opt_specs
    ).
module_and_imports_get_type_repn_specs(ModuleAndImports, X) :-
    promise_pure (
        trace [compile_time(flag("mai-stats"))] (
            semipure get_accesses(Accesses0),
            Accesses = Accesses0 ^ mf_type_repn_specs := accessed,
            impure set_accesses(Accesses)
        ),
        X = ModuleAndImports ^ mai_type_repn_specs
    ).
module_and_imports_get_version_numbers_map(ModuleAndImports, X) :-
    promise_pure (
        trace [compile_time(flag("mai-stats"))] (
            semipure get_accesses(Accesses0),
            Accesses = Accesses0 ^ mf_version_numbers_map := accessed,
            impure set_accesses(Accesses)
        ),
        X = ModuleAndImports ^ mai_version_numbers_map
    ).

:- pred module_and_imports_set_ancestor_int_specs(
    map(module_name, ancestor_int_spec)::in,
    module_and_imports::in, module_and_imports::out) is det.
:- pred module_and_imports_set_direct_int_specs(
    map(module_name, direct_int_spec)::in,
    module_and_imports::in, module_and_imports::out) is det.
:- pred module_and_imports_set_indirect_int_specs(
    map(module_name, indirect_int_spec)::in,
    module_and_imports::in, module_and_imports::out) is det.
:- pred module_and_imports_set_plain_opts(
    map(module_name, parse_tree_plain_opt)::in,
    module_and_imports::in, module_and_imports::out) is det.
:- pred module_and_imports_set_trans_opts(
    map(module_name, parse_tree_trans_opt)::in,
    module_and_imports::in, module_and_imports::out) is det.
:- pred module_and_imports_set_int_for_opt_specs(
    map(module_name, int_for_opt_spec)::in,
    module_and_imports::in, module_and_imports::out) is det.
:- pred module_and_imports_set_type_repn_specs(
    map(module_name, type_repn_spec)::in,
    module_and_imports::in, module_and_imports::out) is det.
:- pred module_and_imports_set_version_numbers_map(
    module_version_numbers_map::in,
    module_and_imports::in, module_and_imports::out) is det.

module_and_imports_set_ancestor_int_specs(X, !ModuleAndImports) :-
    !ModuleAndImports ^ mai_ancestor_int_specs := X.
module_and_imports_set_direct_int_specs(X, !ModuleAndImports) :-
    !ModuleAndImports ^ mai_direct_int_specs := X.
module_and_imports_set_indirect_int_specs(X, !ModuleAndImports) :-
    !ModuleAndImports ^ mai_indirect_int_specs := X.
module_and_imports_set_plain_opts(X, !ModuleAndImports) :-
    !ModuleAndImports ^ mai_plain_opts := X.
module_and_imports_set_trans_opts(X, !ModuleAndImports) :-
    !ModuleAndImports ^ mai_trans_opts := X.
module_and_imports_set_int_for_opt_specs(X, !ModuleAndImports) :-
    !ModuleAndImports ^ mai_int_for_opt_specs := X.
module_and_imports_set_type_repn_specs(X, !ModuleAndImports) :-
    !ModuleAndImports ^ mai_type_repn_specs := X.
module_and_imports_set_version_numbers_map(X, !ModuleAndImports) :-
    !ModuleAndImports ^ mai_version_numbers_map := X.

%---------------------------------------------------------------------------%

module_and_imports_get_module_name(ModuleAndImports, ModuleName) :-
    module_and_imports_get_parse_tree_module_src(ModuleAndImports,
        ParseTreeModuleSrc),
    ModuleName = ParseTreeModuleSrc ^ ptms_module_name.

module_and_imports_get_children(ModuleAndImports, Children) :-
    module_and_imports_get_parse_tree_module_src(ModuleAndImports,
        ParseTreeModuleSrc),
    IncludeMap = ParseTreeModuleSrc ^ ptms_include_map,
    Children = map.keys(IncludeMap).

module_and_imports_get_children_set(ModuleAndImports, Children) :-
    module_and_imports_get_parse_tree_module_src(ModuleAndImports,
        ParseTreeModuleSrc),
    IncludeMap = ParseTreeModuleSrc ^ ptms_include_map,
    Children = map.keys_as_set(IncludeMap).

module_and_imports_get_int_imp_deps(ModuleAndImports, IntDeps, ImpDeps) :-
    module_and_imports_get_parse_tree_module_src(ModuleAndImports,
        ParseTreeModuleSrc),
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

module_and_imports_get_fact_tables(ModuleAndImports, FactTables) :-
    module_and_imports_get_parse_tree_module_src(ModuleAndImports,
        ParseTreeModuleSrc),
    get_fact_tables(ParseTreeModuleSrc, FactTables).

module_and_imports_get_fim_specs(ModuleAndImports, FIMSpecs) :-
    module_and_imports_get_parse_tree_module_src(ModuleAndImports,
        ParseTreeModuleSrc),
    get_fims(ParseTreeModuleSrc, FIMSpecs).

module_and_imports_get_foreign_include_file_infos(ModuleAndImports, FIFOs) :-
    module_and_imports_get_parse_tree_module_src(ModuleAndImports,
        ParseTreeModuleSrc),
    get_foreign_include_file_infos(ParseTreeModuleSrc, FIFOs).

%---------------------------------------------------------------------------%

module_and_imports_add_ancestor_int_spec(X, !ModuleAndImports) :-
    module_and_imports_get_ancestor_int_specs(!.ModuleAndImports, Map0),
    X = ancestor_int0(PT0, _),
    MN = PT0 ^ pti0_module_name,
    map.det_insert(MN, X, Map0, Map),
    module_and_imports_set_ancestor_int_specs(Map, !ModuleAndImports).

module_and_imports_add_direct_int_spec(X, !ModuleAndImports) :-
    module_and_imports_get_direct_int_specs(!.ModuleAndImports, Map0),
    ( X = direct_int1(PT1, _), MN = PT1 ^ pti1_module_name
    ; X = direct_int3(PT3, _), MN = PT3 ^ pti3_module_name
    ),
    map.det_insert(MN, X, Map0, Map),
    module_and_imports_set_direct_int_specs(Map, !ModuleAndImports).

module_and_imports_add_indirect_int_spec(X, !ModuleAndImports) :-
    module_and_imports_get_indirect_int_specs(!.ModuleAndImports, Map0),
    ( X = indirect_int2(PT2, _), MN = PT2 ^ pti2_module_name
    ; X = indirect_int3(PT3, _), MN = PT3 ^ pti3_module_name
    ),
    map.det_insert(MN, X, Map0, Map),
    module_and_imports_set_indirect_int_specs(Map, !ModuleAndImports).

module_and_imports_add_plain_opt(X, !ModuleAndImports) :-
    module_and_imports_get_plain_opts(!.ModuleAndImports, Map0),
    MN = X ^ ptpo_module_name,
    map.det_insert(MN, X, Map0, Map),
    module_and_imports_set_plain_opts(Map, !ModuleAndImports).

module_and_imports_add_trans_opt(X, !ModuleAndImports) :-
    module_and_imports_get_trans_opts(!.ModuleAndImports, Map0),
    MN = X ^ ptto_module_name,
    map.det_insert(MN, X, Map0, Map),
    module_and_imports_set_trans_opts(Map, !ModuleAndImports).

module_and_imports_add_int_for_opt_spec(X, !ModuleAndImports) :-
    module_and_imports_get_int_for_opt_specs(!.ModuleAndImports, Map0),
    ( X = for_opt_int0(PT0, _), MN = PT0 ^ pti0_module_name
    ; X = for_opt_int1(PT1, _), MN = PT1 ^ pti1_module_name
    ; X = for_opt_int2(PT2, _), MN = PT2 ^ pti2_module_name
    ),
    map.det_insert(MN, X, Map0, Map),
    module_and_imports_set_int_for_opt_specs(Map, !ModuleAndImports).

module_and_imports_add_type_repn_spec(X, !ModuleAndImports) :-
    module_and_imports_get_type_repn_specs(!.ModuleAndImports, Map0),
    X = type_repn_spec_int1(PT1), MN = PT1 ^ pti1_module_name,
    map.det_insert(MN, X, Map0, Map),
    module_and_imports_set_type_repn_specs(Map, !ModuleAndImports).

%---------------------%

module_and_imports_maybe_add_module_version_numbers(ModuleName,
        MaybeVersionNumbers, !ModuleAndImports) :-
    (
        MaybeVersionNumbers = no_version_numbers
    ;
        MaybeVersionNumbers = version_numbers(VersionNumbers),
        module_and_imports_get_version_numbers_map(!.ModuleAndImports,
            ModuleVersionNumbersMap0),
        map.det_insert(ModuleName, VersionNumbers,
            ModuleVersionNumbersMap0, ModuleVersionNumbersMap),
        module_and_imports_set_version_numbers_map(ModuleVersionNumbersMap,
            !ModuleAndImports)
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

module_and_imports_get_aug_comp_unit(ModuleAndImports, AugCompUnit) :-
    ModuleAndImports = module_and_imports(ParseTreeModuleSrc,
        AncestorIntSpecs, DirectIntSpecs, IndirectIntSpecs,
        PlainOpts, TransOpts, IntForOptSpecs, TypeRepnSpecs,
        VersionNumbersMap),
    AugCompUnit = aug_compilation_unit(VersionNumbersMap,
        ParseTreeModuleSrc, AncestorIntSpecs, DirectIntSpecs, IndirectIntSpecs,
        PlainOpts, TransOpts, IntForOptSpecs, TypeRepnSpecs).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

module_dep_info_get_source_file_name(ModuleDepInfo, X) :-
    (
        ModuleDepInfo = module_dep_info_imports(ModuleImportsAndBaggage),
        Baggage = ModuleImportsAndBaggage ^ miab_baggage,
        X = Baggage ^ mb_source_file_name
    ;
        ModuleDepInfo = module_dep_info_summary(Summary),
        X = Summary ^ mds_source_file_name
    ).

module_dep_info_get_source_file_dir(ModuleDepInfo, X) :-
    (
        ModuleDepInfo = module_dep_info_imports(ModuleImportsAndBaggage),
        Baggage = ModuleImportsAndBaggage ^ miab_baggage,
        X = Baggage ^ mb_source_file_dir
    ;
        ModuleDepInfo = module_dep_info_summary(Summary),
        X = Summary ^ mds_source_file_dir
    ).

module_dep_info_get_source_file_module_name(ModuleDepInfo, X) :-
    (
        ModuleDepInfo = module_dep_info_imports(ModuleImportsAndBaggage),
        Baggage = ModuleImportsAndBaggage ^ miab_baggage,
        X = Baggage ^ mb_source_file_module_name
    ;
        ModuleDepInfo = module_dep_info_summary(Summary),
        X = Summary ^ mds_source_file_module_name
    ).

module_dep_info_get_module_name(ModuleDepInfo, X) :-
    (
        ModuleDepInfo = module_dep_info_imports(ModuleImportsAndBaggage),
        ModuleAndImports = ModuleImportsAndBaggage ^ miab_mai,
        module_and_imports_get_module_name(ModuleAndImports, X)
    ;
        ModuleDepInfo = module_dep_info_summary(Summary),
        X = Summary ^ mds_module_name
    ).

module_dep_info_get_children(ModuleDepInfo, X) :-
    (
        ModuleDepInfo = module_dep_info_imports(ModuleImportsAndBaggage),
        ModuleAndImports = ModuleImportsAndBaggage ^ miab_mai,
        module_and_imports_get_children(ModuleAndImports, Xs),
        set.list_to_set(Xs, X)
    ;
        ModuleDepInfo = module_dep_info_summary(Summary),
        X = Summary ^ mds_children
    ).

module_dep_info_get_maybe_top_module(ModuleDepInfo, X) :-
    (
        ModuleDepInfo = module_dep_info_imports(ModuleImportsAndBaggage),
        Baggage = ModuleImportsAndBaggage ^ miab_baggage,
        X = Baggage ^ mb_maybe_top_module
    ;
        ModuleDepInfo = module_dep_info_summary(Summary),
        X = Summary ^ mds_maybe_top_module
    ).

module_dep_info_get_int_deps(ModuleDepInfo, X) :-
    (
        ModuleDepInfo = module_dep_info_imports(ModuleImportsAndBaggage),
        ModuleAndImports = ModuleImportsAndBaggage ^ miab_mai,
        module_and_imports_get_int_imp_deps(ModuleAndImports, X, _)
    ;
        ModuleDepInfo = module_dep_info_summary(Summary),
        X = Summary ^ mds_int_deps
    ).

module_dep_info_get_imp_deps(ModuleDepInfo, X) :-
    (
        ModuleDepInfo = module_dep_info_imports(ModuleImportsAndBaggage),
        ModuleAndImports = ModuleImportsAndBaggage ^ miab_mai,
        module_and_imports_get_int_imp_deps(ModuleAndImports, _, X)
    ;
        ModuleDepInfo = module_dep_info_summary(Summary),
        X = Summary ^ mds_imp_deps
    ).

module_dep_info_get_fact_tables(ModuleDepInfo, X) :-
    (
        ModuleDepInfo = module_dep_info_imports(ModuleImportsAndBaggage),
        ModuleAndImports = ModuleImportsAndBaggage ^ miab_mai,
        module_and_imports_get_fact_tables(ModuleAndImports, X)
    ;
        ModuleDepInfo = module_dep_info_summary(Summary),
        X = Summary ^ mds_fact_table_file_names
    ).

module_dep_info_get_fims(ModuleDepInfo, X) :-
    (
        ModuleDepInfo = module_dep_info_imports(ModuleImportsAndBaggage),
        ModuleAndImports = ModuleImportsAndBaggage ^ miab_mai,
        module_and_imports_get_fim_specs(ModuleAndImports, X)
    ;
        ModuleDepInfo = module_dep_info_summary(Summary),
        X = Summary ^ mds_fims
    ).

module_dep_info_get_foreign_include_files(ModuleDepInfo, X) :-
    (
        ModuleDepInfo = module_dep_info_imports(ModuleImportsAndBaggage),
        ModuleAndImports = ModuleImportsAndBaggage ^ miab_mai,
        module_and_imports_get_foreign_include_file_infos(ModuleAndImports, X)
    ;
        ModuleDepInfo = module_dep_info_summary(Summary),
        X = Summary ^ mds_foreign_include_files
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- type maybe_accessed
    --->    not_accessed
    ;       accessed.

:- type mai_fields
    --->    mai_fields(
                mf_source_file_name             :: maybe_accessed,
                mf_source_file_dir              :: maybe_accessed,
                mf_source_file_module_name      :: maybe_accessed,
                mf_maybe_top_module             :: maybe_accessed,

                mf_src                          :: maybe_accessed,
                mf_ancestor_int_specs           :: maybe_accessed,
                mf_direct_int_specs             :: maybe_accessed,
                mf_indirect_int_specs           :: maybe_accessed,
                mf_plain_opts                   :: maybe_accessed,
                mf_trans_opts                   :: maybe_accessed,
                mf_int_for_opt_specs            :: maybe_accessed,
                mf_type_repn_specs              :: maybe_accessed,

                mf_version_numbers_map          :: maybe_accessed,
                mf_maybe_timestamp_map          :: maybe_accessed,

                mf_specs                        :: maybe_accessed,
                mf_errors                       :: maybe_accessed
            ).

:- func init_mai_fields = mai_fields.

init_mai_fields =
    mai_fields(not_accessed, not_accessed, not_accessed, not_accessed,

        not_accessed, not_accessed, not_accessed, not_accessed,
        not_accessed, not_accessed, not_accessed, not_accessed,

        not_accessed, not_accessed, not_accessed, not_accessed).

:- mutable(accesses, mai_fields, init_mai_fields, ground, [untrailed]).

write_mai_stats(Stream, !IO) :-
    promise_pure (
        semipure get_accesses(Accesses),
        write_mai_fields_stats(Stream, Accesses, !IO)
    ).

:- pred write_mai_fields_stats(io.output_stream::in, mai_fields::in,
    io::di, io::uo) is det.

write_mai_fields_stats(Stream, Fields, !IO) :-
    Fields = mai_fields(SrcFileName, SrcFileDir, SrcFileModuleName,
        MaybeTopModule, ParseTreeModuleSrc,
        AncestorSpecs, DirectIntSpecs, IndirectIntSpecs,
        PlainOpts, TransOpts, IntForOptSpecs, TypeRepnSpecs,
        VersionNumbersMap, MaybeTimestampMap, Specs, Errors),
    io.format(Stream,
        "%s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s\n",
        [s(acc_str(SrcFileName)),
        s(acc_str(SrcFileDir)),
        s(acc_str(SrcFileModuleName)),
        s(acc_str(MaybeTopModule)),
        s(acc_str(ParseTreeModuleSrc)),
        s(acc_str(AncestorSpecs)),
        s(acc_str(DirectIntSpecs)),
        s(acc_str(IndirectIntSpecs)),
        s(acc_str(PlainOpts)),
        s(acc_str(TransOpts)),
        s(acc_str(IntForOptSpecs)),
        s(acc_str(TypeRepnSpecs)),
        s(acc_str(VersionNumbersMap)),
        s(acc_str(MaybeTimestampMap)),
        s(acc_str(Specs)),
        s(acc_str(Errors))],
        !IO).

:- func acc_str(maybe_accessed) = string.

acc_str(not_accessed) = "n".
acc_str(accessed) = "a".

%---------------------------------------------------------------------------%
:- end_module parse_tree.module_imports.
%---------------------------------------------------------------------------%
