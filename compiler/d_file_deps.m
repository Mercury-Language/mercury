%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2015-2017, 2019-2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: d_file_deps.m.
% Original author: fjh (when this code was in modules.m)
%
% This module is a subcontractor to write_deps_file.m. Its job
% is to figure out the what-depends-on-what information from which
% generate_mmake_fragments.m will generate mmake rules that
% write_deps_file.m then writes out to .d files.
%
%---------------------------------------------------------------------------%

:- module parse_tree.d_file_deps.
:- interface.

:- import_module libs.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.deps_map.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.generate_mmakefile_fragments.
:- import_module parse_tree.make_module_file_names.
:- import_module parse_tree.module_baggage.
:- import_module parse_tree.prog_parse_tree.

:- import_module digraph.
:- import_module io.
:- import_module list.
:- import_module set.

%---------------------------------------------------------------------------%

    % XXX This "documentation" should be made significantly more detailed.
    %
    % The int_deps_graph field gives the interface dependency graph.
    % The imp_deps_graph field gives the implementation dependency graph.
    % The indirect_deps_graph field gives the indirect dependency graph
    % (this includes dependencies on `*.int2' files).
    % The indirect_opt_deps_graph field gives the indirect optimization
    % dependencies (this includes dependencies via `.opt' and `.trans_opt'
    % files).
    % The trans_opt_deps_graph field gives the trans-opt dependency graph
    % for the purpose of making `.trans_opt' files.
    % The trans_opt_order field gives the ordering that is used to determine
    % which other modules the .trans_opt files may depend on.
:- type dep_graphs
    --->    dep_graphs(
                int_deps_graph          :: digraph(module_name),
                imp_deps_graph          :: digraph(module_name),
                indirect_deps_graph     :: digraph(module_name),
                indirect_opt_deps_graph :: digraph(module_name),
                trans_opt_deps_graph    :: digraph(module_name),
                trans_opt_order         :: list(module_name)
            ).

%---------------------------------------------------------------------------%

:- pred compute_deps_for_d_files_gendep(io.text_output_stream::in, globals::in,
    module_name::in, deps_map::in, dep_graphs::out,
    list(burdened_module)::out, list(error_spec)::in, list(error_spec)::out,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%

    % This function computes the whole d_file_deps structure for the gendep
    % context.
    %
:- pred construct_d_file_deps_gendep(globals::in, dep_graphs::in,
    burdened_aug_comp_unit::in, d_file_deps::out) is det.

    % The set of modules imported or used (i.e. made available)
    % by the current module by various means, categorized by those means.
    % (We use "import" as shorthand for "import and/or use', given that
    % for the purposes of the users of this type, which deal with dependencies
    % between files, the distinction does not matter.)
    %
    % Some of these fields contain module names that are straightforward
    % to convert into the names of the files that the current module
    % needs from them. For example, generating target language code
    % for the current module needs the ancestor modules' .opt0 files,
    % and the directly imported modules' .int files. However, this is
    % not so for some of the other fields. For example, for the modules
    % listed in the am_int_for_opt_imports field, we could need their
    % .int0, .int or .int2 file. This is because add_item_avail predicate
    % does not have the info it would need to make this distinction.
    % XXX D_FILE_DEPS This should be fixed.
:- type avail_module_sets
    --->    avail_module_sets(
                % The set of ancestors of the current module.
                % We import their .int0 files.
                am_ancestors                :: set(module_name),

                % The set of modules for which the current module
                % has an import_module or use_module declaration.
                % We will want to read the .int files of these modules.
                am_direct_imports           :: set(module_name),

                % The names of all the indirectly imported/used modules,
                % as computed by grab_qual_imported_modules_augment.
                %
                % This field is never used on its own; it is always used
                % together with the previous one, which records info about the
                % directly imported/used modules, to compute the set of modules
                % that are imported or used either directly or indirectly.
                %
                % This is used for purposes such as:
                %
                % - recording references to those modules' .int2 files
                %   in the current module's .d file;
                % - reading and writing the .analysis files of those modules;
                % - #including the .mh and .mih files of those modules.
                %
                % This field *should* be disjoint with am_direct_imports.
                am_indirect_imports         :: set(module_name),

                % The set of modules that have an import_module or use_module
                % declaration in an ancestor module.
                %
                % We mostly treat these modules as if they were imported
                % or used by the current module itself.
                am_imports_in_ancestors     :: set(module_name),

                % The set of files whose .int files we need to read
                % when compiling the current module in order to make sense
                % of all the .opt files we have read in.
                %
                % This field may overlap with the other fields.
                am_int_for_opt_imports      :: set(module_name),

                % The set of modules that are implicitly imported or used
                % by the current module. Examples include the public and
                % private builtin modules, the table_builtin module
                % in debug grades (for I/O tabling), and modules needed
                % to implement e.g. io.format and string.format.
                %
                % This field may overlap with the other fields.
                am_implicit_imports         :: set(module_name)
            ).

    % This function computes the d_file_deps structure in the hlds context
    % described above. We get the contents we puts into the second and third
    % fields of d_file_deps from the code that constructs the HLDS.
    %
:- pred construct_d_file_deps_hlds(globals::in, burdened_aug_comp_unit::in,
    avail_module_sets::in, maybe_include_trans_opt_rule::in,
    d_file_deps::out) is det.

%---------------------------------------------------------------------------%

:- pred construct_intermod_deps(globals::in, parse_tree_module_src::in,
    d_file_deps::in, intermod_deps::out,
    module_file_name_cache::in, module_file_name_cache::out,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.options.
:- import_module libs.timestamp.
:- import_module parse_tree.file_names.
:- import_module parse_tree.find_module.
:- import_module parse_tree.get_dependencies.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.module_dep_info.
:- import_module parse_tree.module_deps_graph.
:- import_module parse_tree.opt_deps_spec.
:- import_module parse_tree.output_imports_graph.
:- import_module parse_tree.parse_error.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_foreign.
:- import_module parse_tree.prog_item.

:- import_module bool.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module sparse_bitset.
:- import_module string.
:- import_module term_context.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

compute_deps_for_d_files_gendep(ProgressStream, Globals, ModuleName, DepsMap,
        DepGraphs, BurdenedModules, !Specs, !IO) :-
    % Compute the interface deps graph and the implementation deps graph
    % from the deps map.
    digraph.init(IntDepsGraph0),
    digraph.init(ImpDepsGraph0),
    map.values(DepsMap, DepsList),
    deps_list_to_deps_graph(DepsMap, DepsList, BurdenedModules,
        IntDepsGraph0, IntDepsGraph, ImpDepsGraph0, ImpDepsGraph),
    maybe_output_imports_graph(ProgressStream, Globals, ModuleName,
        IntDepsGraph, ImpDepsGraph, !IO),

    trace [compiletime(flag("deps_graph")), runtime(env("DEPS_GRAPH")),
        io(!TIO)]
    (
        io.format(ProgressStream, "compute_deps_for_d_files for %s\n",
            [s(sym_name_to_string(ModuleName))], !TIO),

        digraph.to_assoc_list(IntDepsGraph, IntDepsAL),
        io.write_string(ProgressStream, "IntDepsAL:\n", !TIO),
        list.foldl(io.write_line(ProgressStream), IntDepsAL, !TIO),

        digraph.to_assoc_list(ImpDepsGraph, ImpDepsAL),
        io.write_string(ProgressStream, "ImpDepsAL:\n", !TIO),
        list.foldl(io.write_line(ProgressStream), ImpDepsAL, !TIO)
    ),

    compute_opt_trans_opt_deps_graph(ProgressStream, Globals, ModuleName,
        ImpDepsGraph, IndirectOptDepsGraph,
        TransOptDepsGraph, TransOptDepsOrdering, !Specs, !IO),

    % Compute the indirect dependencies: they are equal to the composition
    % of the implementation dependencies with the transitive closure of the
    % implementation dependencies. (We used to take the transitive closure
    % of the interface dependencies, but we now include implementation
    % details in the interface files).
    digraph.tc(ImpDepsGraph, TransImpDepsGraph),
    digraph.compose(ImpDepsGraph, TransImpDepsGraph, IndirectDepsGraph),

    % XXX LEGACY
    ExtTransOpt =
        ext_cur_ngs_gs_max_ngs(ext_cur_ngs_gs_max_ngs_legacy_opt_trans),
    get_ext_opt_deps(Globals, look_for_src, ExtTransOpt,
        TransOptDepsOrdering, TransOptOrder, !IO),

    DepGraphs = dep_graphs(IntDepsGraph, ImpDepsGraph, IndirectDepsGraph,
        IndirectOptDepsGraph, TransOptDepsGraph, TransOptOrder).

    % Construct a pair of dependency graphs (the interface dependencies
    % and the implementation dependencies) for all the modules in the program.
    %
:- pred deps_list_to_deps_graph(deps_map::in,
    list(deps)::in, list(burdened_module)::out,
    deps_graph::in, deps_graph::out, deps_graph::in, deps_graph::out) is det.

deps_list_to_deps_graph(_, [], [], !IntDepsGraph, !ImpDepsGraph).
deps_list_to_deps_graph(DepsMap,
        [Deps | DepsList], [BurdenedModule | BurdenedModules],
        !IntDepsGraph, !ImpDepsGraph) :-
    Deps = deps(_, BurdenedModule),
    Baggage = BurdenedModule ^ bm_baggage,
    Errors = Baggage ^ mb_errors,
    FatalErrors = Errors ^ rm_fatal_errors,
    ( if set.is_empty(FatalErrors) then
        ModuleDepInfo = module_dep_info_full(BurdenedModule),
        add_module_dep_info_to_deps_graph(ModuleDepInfo,
            lookup_burdened_module_in_deps_map(DepsMap),
            !IntDepsGraph, !ImpDepsGraph)
    else
        true
    ),
    deps_list_to_deps_graph(DepsMap, DepsList, BurdenedModules,
        !IntDepsGraph, !ImpDepsGraph).

:- func lookup_burdened_module_in_deps_map(deps_map, module_name)
    = module_dep_info.

lookup_burdened_module_in_deps_map(DepsMap, ModuleName) = ModuleDepInfo :-
    map.lookup(DepsMap, ModuleName, deps(_, BurdenedModule)),
    ModuleDepInfo = module_dep_info_full(BurdenedModule).

%---------------------------------------------------------------------------%

construct_d_file_deps_gendep(Globals, DepGraphs, BurdenedAugCompUnit,
        DFileDeps) :-
    BurdenedAugCompUnit = burdened_aug_comp_unit(_Baggage, AugCompUnit),
    % XXX The reason why we ignore all the fields of AugCompUnit except
    % ParseTreeModuleSrc is that our caller gives us a BurdenedAugCompUnit
    % in which the AugCompUnit part contains nothing *but* the
    % ParseTreeModuleSrc. The info in DepGraphs is supposed to replace
    % this info, but it does not, and maybe cannot do so completely.
    AugCompUnit = aug_compilation_unit(ParseTreeModuleSrc,
        _, _, _, _, _, _, _, _),
    DepGraphs = dep_graphs(IntDepsGraph, ImpDepsGraph, IndirectDepsGraph,
        IndirectOptDepsGraph, TransOptDepsGraph, FullTransOptOrder),

    % Look up the interface/implementation/indirect dependencies
    % for this module from the respective dependency graphs.

    ModuleName = ParseTreeModuleSrc ^ ptms_module_name,
    get_dependencies_from_graph(IndirectOptDepsGraph, ModuleName,
        IndirectOptDeps),

    globals.lookup_bool_option(Globals, intermodule_optimization,
        Intermod),
    (
        Intermod = yes,
        % Be conservative with inter-module optimization -- assume a
        % module depends on the `.int', `.int2' and `.opt' files
        % for all transitively imported modules.
        DirectDeps = IndirectOptDeps,
        IndirectDeps = IndirectOptDeps
    ;
        Intermod = no,
        get_dependencies_from_graph(IntDepsGraph, ModuleName, IntDeps),
        get_dependencies_from_graph(ImpDepsGraph, ModuleName, ImpDeps),
        set.union(IntDeps, ImpDeps, DirectDeps),
        get_dependencies_from_graph(IndirectDepsGraph, ModuleName,
            IndirectDeps)
    ),

    get_dependencies_from_graph(TransOptDepsGraph, ModuleName, TransOptDeps0),
    set.delete(ModuleName, TransOptDeps0, TransOptDeps),

    % XXX DFILE The way IndirectOptDeps is computed seems to have nothing
    % to do with foreign_import_module declarations. This seems to me (zs)
    % to be a BUG.
    StdDeps = std_deps(DirectDeps, IndirectDeps, IndirectOptDeps,
        trans_opt_deps(TransOptDeps)),

    compute_allowable_trans_opt_deps(ModuleName,
        FullTransOptOrder, TransOptOrder),
    set.list_to_set(TransOptOrder, TransOptOrderSet),
    TransOptRuleInfo = trans_opt_deps_from_order(TransOptOrderSet),
    MaybeInclTransOptRule = include_trans_opt_rule(TransOptRuleInfo),

    DFileDeps = d_file_deps(StdDeps, IndirectOptDeps, MaybeInclTransOptRule).

:- pred get_dependencies_from_graph(deps_graph::in, module_name::in,
    set(module_name)::out) is det.

get_dependencies_from_graph(DepsGraph, ModuleName, Dependencies) :-
    ( if digraph.search_key(DepsGraph, ModuleName, ModuleKey) then
        digraph.lookup_key_set_from(DepsGraph, ModuleKey, DepsKeysSet),
        AddKeyDep =
            ( pred(Key::in, Deps0::in, Deps::out) is det :-
                digraph.lookup_vertex(DepsGraph, Key, Dep),
                Deps = [Dep | Deps0]
            ),
        sparse_bitset.foldr(AddKeyDep, DepsKeysSet, [], DependenciesList),
        set.list_to_set(DependenciesList, Dependencies)
    else
        set.init(Dependencies)
    ).

    % Compute the maximum allowable trans-opt dependencies for this module.
    % To avoid the possibility of cycles, each module is allowed to depend
    % only on modules that occur after it in FullTransOptOrder.
    %
:- pred compute_allowable_trans_opt_deps(module_name::in,
    list(module_name)::in, list(module_name)::out) is det.

compute_allowable_trans_opt_deps(_ModuleName, [], []).
compute_allowable_trans_opt_deps(ModuleName,
        [HeadModuleName | TailModuleNames], TransOptOrder) :-
    ( if HeadModuleName = ModuleName then
        TransOptOrder = TailModuleNames
    else
        compute_allowable_trans_opt_deps(ModuleName, TailModuleNames,
            TransOptOrder)
    ).

%---------------------------------------------------------------------------%

construct_d_file_deps_hlds(Globals, BurdenedAugCompUnit, AvailModuleSets,
        MaybeInclTransOptRule, DFileDeps) :-
    BurdenedAugCompUnit = burdened_aug_comp_unit(Baggage, AugCompUnit),
    % NOTE SourceFileTopModuleName will differ from the module name
    % in ParseTreeModuleSrc if ParseTreeModuleSrc is a nested submodule,
    % and if a file contains nested submodules, we do generate individual
    % .d files for each one of them.
    SourceFileTopModuleName = Baggage ^ mb_source_file_top_module_name,
    AugCompUnit = aug_compilation_unit(ParseTreeModuleSrc,
        AncestorIntSpecs, DirectInt1Specs, IndirectInt2Specs,
        PlainOpts, _TransOpts, IntForOptSpecs, _TypeRepnSpecs,
        _ModuleVersionNumber),
    map.keys_as_set(ParseTreeModuleSrc ^ ptms_import_use_map, DirectDeps),
    map.keys_as_set(IndirectInt2Specs, IndirectDeps),

    some [!FIMSpecs] (
        get_fim_specs(ParseTreeModuleSrc, !:FIMSpecs),
        map.foldl_values(gather_fim_specs_in_ancestor_int_spec,
            AncestorIntSpecs, !FIMSpecs),
        map.foldl_values(gather_fim_specs_in_direct_int1_spec,
            DirectInt1Specs, !FIMSpecs),
        map.foldl_values(gather_fim_specs_in_indirect_int2_spec,
            IndirectInt2Specs, !FIMSpecs),
        map.foldl_values(gather_fim_specs_in_parse_tree_plain_opt,
            PlainOpts, !FIMSpecs),
        % .trans_opt files cannot contain FIMs.
        map.foldl_values(gather_fim_specs_in_int_for_opt_spec,
            IntForOptSpecs, !FIMSpecs),
        % Any FIMs in type_repn_specs are ignored.

        % We restrict the set of FIMs to those that are valid
        % for the current backend. This preserves old behavior,
        % and makes sense in that the code below generates mmake rules
        % only for the current backend, but it would be nice if we
        % could generate dependency rules for *all* the backends.
        globals.get_backend_foreign_languages(Globals, BackendLangs),
        IsBackendFIM =
            ( pred(FIMSpec::in) is semidet :-
                list.member(FIMSpec ^ fimspec_lang, BackendLangs)
            ),
        set.filter(IsBackendFIM, !.FIMSpecs, FIMSpecs)
    ),
    set.filter_map(
        ( pred(ForeignImportMod::in, ImportModuleName::out) is semidet :-
            ImportModuleName = fim_spec_module_name_from_module(
                ForeignImportMod, SourceFileTopModuleName),
            % XXX We can't include mercury.dll as mmake can't find it,
            % but we know that it exists.
            ImportModuleName \= unqualified("mercury")
        ), FIMSpecs, ForeignImportedModuleNamesSet),

    StdDeps = std_deps(DirectDeps, IndirectDeps,
        ForeignImportedModuleNamesSet, no_trans_opt_deps),

    AvailModuleSets = avail_module_sets(Ancestors, DirectImports,
        IndirectImports, ImportedInAncestors, IntForOptImports,
        ImplicitlyImortedModules),
    DirectsIndirectsForOptsAncestors = set.union_list([Ancestors,
        DirectImports, IndirectImports, ImportedInAncestors,
        IntForOptImports, ImplicitlyImortedModules]),
    DFileDeps = d_file_deps(StdDeps, DirectsIndirectsForOptsAncestors,
        MaybeInclTransOptRule).

%---------------------%

:- pred gather_fim_specs_in_ancestor_int_spec(ancestor_int_spec::in,
    set(fim_spec)::in, set(fim_spec)::out) is det.

gather_fim_specs_in_ancestor_int_spec(AncestorIntSpec, !FIMSpecs) :-
    AncestorIntSpec = ancestor_int0(ParseTreeInt0, _ReadWhy0),
    gather_fim_specs_in_parse_tree_int0(ParseTreeInt0, !FIMSpecs).

:- pred gather_fim_specs_in_direct_int1_spec(direct_int1_spec::in,
    set(fim_spec)::in, set(fim_spec)::out) is det.

gather_fim_specs_in_direct_int1_spec(DirectInt1Spec, !FIMSpecs) :-
    DirectInt1Spec = direct_int1(ParseTreeInt1, _ReadWhy1),
    gather_fim_specs_in_parse_tree_int1(ParseTreeInt1, !FIMSpecs).

:- pred gather_fim_specs_in_indirect_int2_spec(indirect_int2_spec::in,
    set(fim_spec)::in, set(fim_spec)::out) is det.

gather_fim_specs_in_indirect_int2_spec(IndirectInt2Spec, !FIMSpecs) :-
    IndirectInt2Spec = indirect_int2(ParseTreeInt2, _ReadWhy2),
    gather_fim_specs_in_parse_tree_int2(ParseTreeInt2, !FIMSpecs).

:- pred gather_fim_specs_in_int_for_opt_spec(int_for_opt_spec::in,
    set(fim_spec)::in, set(fim_spec)::out) is det.

gather_fim_specs_in_int_for_opt_spec(IntForOptSpec, !FIMSpecs) :-
    (
        IntForOptSpec = for_opt_int0(ParseTreeInt0, _ReadWhy0),
        gather_fim_specs_in_parse_tree_int0(ParseTreeInt0, !FIMSpecs)
    ;
        IntForOptSpec = for_opt_int1(ParseTreeInt1, _ReadWhy1),
        gather_fim_specs_in_parse_tree_int1(ParseTreeInt1, !FIMSpecs)
    ;
        IntForOptSpec = for_opt_int2(ParseTreeInt2, _ReadWhy2),
        gather_fim_specs_in_parse_tree_int2(ParseTreeInt2, !FIMSpecs)
    ).

:- pred gather_fim_specs_in_parse_tree_int0(parse_tree_int0::in,
    set(fim_spec)::in, set(fim_spec)::out) is det.

gather_fim_specs_in_parse_tree_int0(ParseTreeInt0, !FIMSpecs) :-
    IntFIMS = ParseTreeInt0 ^ pti0_int_fims,
    ImpFIMS = ParseTreeInt0 ^ pti0_imp_fims,
    !:FIMSpecs = set.union_list([IntFIMS, ImpFIMS, !.FIMSpecs]).

:- pred gather_fim_specs_in_parse_tree_int1(parse_tree_int1::in,
    set(fim_spec)::in, set(fim_spec)::out) is det.

gather_fim_specs_in_parse_tree_int1(ParseTreeInt1, !FIMSpecs) :-
    IntFIMS = ParseTreeInt1 ^ pti1_int_fims,
    ImpFIMS = ParseTreeInt1 ^ pti1_imp_fims,
    !:FIMSpecs = set.union_list([IntFIMS, ImpFIMS, !.FIMSpecs]).

:- pred gather_fim_specs_in_parse_tree_int2(parse_tree_int2::in,
    set(fim_spec)::in, set(fim_spec)::out) is det.

gather_fim_specs_in_parse_tree_int2(ParseTreeInt2, !FIMSpecs) :-
    IntFIMS = ParseTreeInt2 ^ pti2_int_fims,
    ImpFIMS = ParseTreeInt2 ^ pti2_imp_fims,
    !:FIMSpecs = set.union_list([IntFIMS, ImpFIMS, !.FIMSpecs]).

:- pred gather_fim_specs_in_parse_tree_plain_opt(parse_tree_plain_opt::in,
    set(fim_spec)::in, set(fim_spec)::out) is det.

gather_fim_specs_in_parse_tree_plain_opt(ParseTreePlainOpt, !FIMSpecs) :-
    set.union(ParseTreePlainOpt ^ ptpo_fims, !FIMSpecs).

%---------------------------------------------------------------------------%

construct_intermod_deps(Globals, ParseTreeModuleSrc, DFileDeps, IntermodDeps,
        !Cache, !IO) :-
    % XXX Note that currently, due to a design problem, handle_options.m
    % *always* sets use_opt_files to no.
    globals.lookup_bool_option(Globals, use_opt_files, UsePlainOpt),
    globals.lookup_bool_option(Globals, intermodule_optimization, Intermod),
    (
        Intermod = yes,
        % If intermodule_optimization is enabled, then all the .mh files
        % must exist, because it is possible that the .c file imports them
        % directly or indirectly.
        MaybeMhDeps = intermod_mh_deps
    ;
        Intermod = no,
        MaybeMhDeps = no_intermod_mh_deps
    ),
    ( if
        ( Intermod = yes
        ; UsePlainOpt = yes
        )
    then
        globals.lookup_bool_option(Globals, use_trans_opt_files, UseTransOpt),
        globals.lookup_bool_option(Globals, transitive_optimization, TransOpt),
        ( UseTransOpt = no,  LookForSrc = look_for_src
        ; UseTransOpt = yes, LookForSrc = do_not_look_for_src
        ),
        DFileDeps = d_file_deps(StdDeps, _, _),
        StdDeps = std_deps(DirectDeps, _, _, _),
        ModuleName = ParseTreeModuleSrc ^ ptms_module_name,
        BaseDeps = [ModuleName | set.to_sorted_list(DirectDeps)],
        ( if
            ( TransOpt = yes
            ; UseTransOpt = yes
            )
        then
            get_plain_trans_opt_deps(Globals, LookForSrc,
                BaseDeps, PlainOptDeps, TransOptDeps, !Cache, !IO),
            MaybeTransOptDeps = yes(TransOptDeps)
        else
            % XXX LEGACY
            ExtOpt = ext_cur_ngs_gs_max_ngs(
                ext_cur_ngs_gs_max_ngs_legacy_opt_plain),
            get_ext_opt_deps(Globals, LookForSrc, ExtOpt,
                BaseDeps, PlainOptDeps, !IO),
            MaybeTransOptDeps = no
        ),
        MaybeOptFileDeps = opt_file_deps(PlainOptDeps, MaybeTransOptDeps)
    else
        MaybeOptFileDeps = no_opt_file_deps
    ),
    IntermodDeps = intermod_deps(MaybeMhDeps, MaybeOptFileDeps).

%---------------------%

    % The idea that code looking for .opt files and maybe .trans_opt files
    % can be told to accept .m files as acceptable substitutes for them
    % looks very strange. However, there is *some* logic behind this,
    % though that logic has been swamped by later changes.
    %
    % When we initially construct a program's .dep file, with a command
    % such as "mmake prog.depend", usually the only files associated
    % with that program that exist are just the source files: the .intN
    % files and the .*opt files either don't exist yet, or have just all
    % been deleted with something like "mmake prog.realclean". In that
    % situation, the compiler needs to decide what dependencies to put
    % into prog.dep, which requires deciding what .opt/.trans_opt files
    % the target code files of the program's modules, and through them
    % the program's executable, *should* depend on. In this case,
    % the *current* existence of a source file indicates that in the future,
    % there *should* exist a .opt and maybe a .trans_opt file for that module
    % (depending on which intermodule optimization options are enabled).
    %
    % There are two issues that this scheme does not handle well,
    % nested submodules, and --use-subdirs. This is not surprising,
    % since this code predates both those features :-(
    % (This code was added in early 1997, while nested submodules and
    % --use-subdirs were added about a year later.)
    %
    % This code mishandles nested modules in that it assumes that
    % the name of a module can be converted directly to a filename,
    % whose existence can then be tested. When the named module is
    % a nested, non-top submodule within a source file, this process
    % will fail: the second step will find that the file name constructed
    % by the first step does not exist. This means that prog.dep will
    % not record dependencies on nested submodules' .opt and .trans_opt files.
    % Given the mildness of the symptom, it is not surprising that this
    % flew under the radar. And given the rarity of nested submodules,
    % this is a problem that is probably not worth fixing.
    %
    % This code mishandles --use-subdirs in that it looks for .m files
    % in the same directories in which it looks for .opt/.trans_opt files,
    % i.e. in the directories named by --intermod-directory options,
    % even though .m files may not occur in many directories where
    % .opt/.trans_opt files may occur. The original code from which
    % the predicates are derived, the get_curr_dir_deps predicate added
    % by Simon on 1996 nov 6, as its name says, looked for source files
    % only in the current directory. Through the --intermod-directory
    % options, we now also look for .m files in
    %
    % - in Mercury subdirs in the current directory,
    % - in other directories in the same workspace,
    % - in the Mercury subdirs of other directories in the same workspace, and
    % - in installed library directories.
    %
    % Only the second of these is a place where .m files can legitimately
    % be found; looking in the others is wasted work.
    %
    % Fixing this waste is hard while using the LEGACY search options,
    % because the value of the intermod_directories option contains
    % all four of the above kinds of directory names without any indication
    % of which category they fall into. The PROPOSED search options
    % intermod_dirs_same_subdir_setting, intermod_dirs_indep_subdir_setting
    % and intermod_dirs_installed_library, *do* make those distinctions.
    % We should therefore fix this issue once we have switched over
    % to using them exclusively.
    % XXX LEGACY
    %
:- type maybe_look_for_src
    --->    do_not_look_for_src
    ;       look_for_src.

    % get_plain_trans_opt_deps(Globals, LookForSrc, IntermodDirs, Deps,
    %   OptDeps, TransOptDeps, !Cache, !IO):
    %
    % For each dependency, search intermod_directories for a .m file.
    % If it exists, add it to both output lists. Otherwise, if a .opt
    % file exists, add it to the OptDeps list, and if a .trans_opt
    % file exists, add it to the TransOptDeps list.
    % With do_not_look_for_src, don't look for `.m' files (since we are
    % not building `.opt' files, only using those which are available).
    %
    % XXX See the comment on the maybe_look_for_src type about this
    % strange arrangement.
    %
    % XXX This won't find nested submodules.
    % XXX Use `mmc --make' if that matters.
    %
:- pred get_plain_trans_opt_deps(globals::in, maybe_look_for_src::in,
    list(module_name)::in, list(module_name)::out, list(module_name)::out,
    module_file_name_cache::in, module_file_name_cache::out,
    io::di, io::uo) is det.

get_plain_trans_opt_deps(_, _, [], [], [], !Cache, !IO).
get_plain_trans_opt_deps(Globals, LookForSrc, [ModuleName | ModuleNames],
        !:OptDeps, !:TransOptDeps, !Cache, !IO) :-
    get_plain_trans_opt_deps(Globals, LookForSrc, ModuleNames,
        !:OptDeps, !:TransOptDeps, !Cache, !IO),
    (
        LookForSrc = look_for_src,
        SearchAuthDirsSrc = get_search_auth_intermod_dirs(ime_src, Globals),
        search_for_module_source(SearchAuthDirsSrc,
            ModuleName, _SearchDirsLook, MaybeFileName, !IO),
        (
            MaybeFileName = ok(_),
            !:OptDeps = [ModuleName | !.OptDeps],
            !:TransOptDeps = [ModuleName | !.TransOptDeps],
            Found = found
        ;
            MaybeFileName = error(_),
            Found = not_found
        )
    ;
        LookForSrc = do_not_look_for_src,
        Found = not_found
    ),
    (
        Found = not_found,
        % XXX LEGACY
        ExtOpt =
            ext_cur_ngs_gs_max_ngs(ext_cur_ngs_gs_max_ngs_legacy_opt_plain),
        make_module_file_name(Globals, $pred, ExtOpt,
            ModuleName, OptName, !Cache),
        SearchAuthDirsPlainOpt =
            get_search_auth_intermod_dirs(ime_opt_plain, Globals),
        search_for_file_returning_dir(SearchAuthDirsPlainOpt,
            OptName, _SearchDirsNotFoundOpt, MaybeOptDir, !IO),
        (
            MaybeOptDir = ok(_),
            !:OptDeps = [ModuleName | !.OptDeps]
        ;
            MaybeOptDir = error(_)
        ),
        % XXX LEGACY
        ExtTransOpt =
            ext_cur_ngs_gs_max_ngs(ext_cur_ngs_gs_max_ngs_legacy_opt_trans),
        make_module_file_name(Globals, $pred, ExtTransOpt,
            ModuleName, TransOptName, !Cache),
        SearchAuthDirsTransOpt =
            get_search_auth_intermod_dirs(ime_opt_trans, Globals),
        search_for_file_returning_dir(SearchAuthDirsTransOpt,
            TransOptName, _SearchDirsNotFoundTransOpt, MaybeTransOptDir, !IO),
        (
            MaybeTransOptDir = ok(_),
            !:TransOptDeps = [ModuleName | !.TransOptDeps]
        ;
            MaybeTransOptDir = error(_)
        )
    ;
        Found = found
    ).

    % For each dependency, search intermod_directories for a file with
    % the given extension, filtering out those for which the search fails.
    % With do_not_look_for_src, only look for files with the given extension,
    % not source files.
    %
    % XXX See the comment on the maybe_look_for_src type about this
    % strange arrangement.
    %
    % XXX This won't find nested submodules.
    % XXX Use `mmc --make' if that matters.
    %
    % This predicate must operate on lists, not sets, of module names,
    % because it needs to preserve the chosen trans_opt deps ordering,
    % which is derived from the dependency graph between modules,
    % and not just the modules' names.
    %
:- pred get_ext_opt_deps(globals::in, maybe_look_for_src::in, ext::in(ext_opt),
    list(module_name)::in, list(module_name)::out, io::di, io::uo) is det.

get_ext_opt_deps(_, _, _, [], [], !IO).
get_ext_opt_deps(Globals, LookForSrc, Ext, [ModuleName | ModuleNames],
        !:OptDeps, !IO) :-
    get_ext_opt_deps(Globals, LookForSrc, Ext, ModuleNames, !:OptDeps, !IO),
    (
        LookForSrc = look_for_src,
        SearchAuthDirsSrc = get_search_auth_intermod_dirs(ime_src, Globals),
        search_for_module_source(SearchAuthDirsSrc, ModuleName,
            _SearchDirsLook, Result1, !IO),
        (
            Result1 = ok(_),
            !:OptDeps = [ModuleName | !.OptDeps],
            Found = yes
        ;
            Result1 = error(_),
            Found = no
        )
    ;
        LookForSrc = do_not_look_for_src,
        Found = no
    ),
    (
        Found = no,
        SearchWhichDirsExt = search_intermod_dirs,
        % XXX LEGACY
        module_name_to_search_file_name(Globals, $pred, Ext, ModuleName,
            SearchWhichDirsExt, SearchAuthDirsExt, OptName, _OptNameProposed),
        search_for_file(SearchAuthDirsExt, OptName,
            _SearchDirsNotFound, MaybeOptDir, !IO),
        (
            MaybeOptDir = ok(_),
            !:OptDeps = [ModuleName | !.OptDeps]
        ;
            MaybeOptDir = error(_)
        )
    ;
        Found = yes
    ).

%---------------------------------------------------------------------------%
:- end_module parse_tree.d_file_deps.
%---------------------------------------------------------------------------%
