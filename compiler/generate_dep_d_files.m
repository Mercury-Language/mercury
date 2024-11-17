%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2015-2017, 2019-2024 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: generate_dep_d_files.m.
% Original author: fjh (when this code was in modules.m)
%
% This module figures out the what-depends-on-what information from which
% generate_mmake_fragments.m generate mmake rules that write_deps_file.m
% then writes out to .dep, .dv and .d files.
%
% We generate one .dep and one .dv file for each program, with those files
% being named prog.dep and prog.dv (if the name of the program is "prog").
% We generate one .d file for each module in the program, with the file
% being named mod.d (if the name of the module is "mod").
%
% The .dv file contains the definitions of all the mmake variable definitions
% relating to the program, while the .dep file contains all the rules
% relating to the program. The reason for this split is that we want mmake
% to glue all these mmakefile fragments together in the following order:
%
% - the program's .dv file
% - the Mmakefile in the current directory
% - the .d files of the program's modules
% - the program's .dep file
% - the standard Mmake.rules file
%
% This arrangement gives the Mmakefile access to the values of the
% variables defined in the program's .dv file, for example as lists
% of files on which a target depends. On the other hand, by including
% the automatically generated .dep file *after* the Mmakefile, we allow
% the rules in the .dep file to refer to variables defined in the Mmakefile
% (Usually the rules allow, but do not require, the Mmakefile to define
% these variables.)
%
%---------------------------------------------------------------------------%

:- module parse_tree.generate_dep_d_files.
:- interface.

:- import_module libs.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.deps_map.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.generate_mmakefile_fragments.
:- import_module parse_tree.make_module_file_names.
:- import_module parse_tree.module_baggage.
:- import_module parse_tree.prog_item.

:- import_module io.
:- import_module list.
:- import_module set.

%---------------------------------------------------------------------------%

    % generate_dep_file_for_module(ProgressStream, Globals, FileOrModule,
    %   Specs, !IO):
    %
    % Generate the per-program makefile dependencies file (`.dep' file)
    % for the program whose top-level module is specified by FileOrModule.
    % This involves first transitively reading in all imported or ancestor
    % modules. While we are at it, we also save the per-module makefile
    % dependency files (`.d' files) for all those modules. Return any errors
    % and/or warnings to be printed in Specs.
    %
:- pred generate_dep_file(io.text_output_stream::in, globals::in,
    file_or_module::in, deps_map::out, list(error_spec)::out,
    io::di, io::uo) is det.

    % generate_d_file_for_module(ProgressStream, Globals, FIleOrModule
    %   Specs, !IO):
    %
    % Generate the per-module makefile dependency file ('.d' file)
    % for the given module.
    %
:- pred generate_d_file(io.text_output_stream::in, globals::in,
    file_or_module::in, deps_map::out, list(error_spec)::out,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%

    % XXX This predicate should NOT take StdDeps as input; it should
    % compute StdDeps itself. Having our two callers construct StdDeps
    % using two separate algorithms that compute different results
    % is a old bug.
    %
:- pred generate_d_file_fragment(globals::in, burdened_aug_comp_unit::in,
    std_deps::in, set(module_name)::in, maybe_include_trans_opt_rule::in,
    file_name::out, string::out,
    module_file_name_cache::in, module_file_name_cache::out,
    io::di, io::uo) is det.

:- func construct_std_deps(globals, burdened_aug_comp_unit) = std_deps.

:- pred construct_intermod_deps(globals::in, parse_tree_module_src::in,
    std_deps::in, intermod_deps::out,
    module_file_name_cache::in, module_file_name_cache::out,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.mmakefiles.
:- import_module libs.options.
:- import_module libs.timestamp.
:- import_module parse_tree.file_names.
:- import_module parse_tree.find_module.
:- import_module parse_tree.get_dependencies.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.module_cmds.
:- import_module parse_tree.module_dep_info.
:- import_module parse_tree.module_deps_graph.
:- import_module parse_tree.opt_deps_spec.
:- import_module parse_tree.output_imports_graph.
:- import_module parse_tree.parse_error.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_foreign.
:- import_module parse_tree.warn_unread_modules.
:- import_module parse_tree.write_deps_file.

:- import_module bool.
:- import_module cord.
:- import_module digraph.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set_tree234.
:- import_module string.
:- import_module term_context.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

generate_dep_file(ProgressStream, Globals, FileOrModule,
        DepsMap, Specs, !IO) :-
    maybe_generate_dot_dx_files(ProgressStream, Globals,
        output_all_program_dot_dx_files, do_not_search,
        FileOrModule, DepsMap, Specs, !IO).

generate_d_file(ProgressStream, Globals, FileOrModule,
        DepsMap, Specs, !IO) :-
    maybe_generate_dot_dx_files(ProgressStream, Globals,
        output_module_dot_d_file, do_search,
        FileOrModule, DepsMap, Specs, !IO).

%---------------------------------------------------------------------------%

:- type which_dot_dx_files
    --->    output_module_dot_d_file
            % Output the given module's .d file.
    ;       output_all_program_dot_dx_files.
            % The given module is (or should be!) the main module of a program.
            % Output the program's .dep and .dv files, and the .d file
            % of every module in the program.

:- pred maybe_generate_dot_dx_files(io.text_output_stream::in, globals::in,
    which_dot_dx_files::in, maybe_search::in, file_or_module::in,
    deps_map::out, list(error_spec)::out, io::di, io::uo) is det.

maybe_generate_dot_dx_files(ProgressStream, Globals, Mode, Search,
        FileOrModule, DepsMap, !:Specs, !IO) :-
    % First, build up a map of the dependencies.
    generate_deps_map(ProgressStream, Globals, Search,
        FileOrModule, ModuleName, ReadModules, UnreadModules, DepsMap,
        !:Specs, !IO),
    warn_about_any_unread_modules_with_read_ancestors(ReadModules,
        UnreadModules, !Specs),

    trace [compiletime(flag("deps_graph")), runtime(env("DEPS_GRAPH")),
        io(!TIO)]
    (
        io.format(ProgressStream, "generate_dot_dx_files for %s\n",
            [s(sym_name_to_string(ModuleName))], !TIO),

        set_tree234.to_sorted_list(ReadModules, ReadModuleList),
        set_tree234.to_sorted_list(UnreadModules, UnreadModuleList),
        ReadStrs = list.map(sym_name_to_string, ReadModuleList),
        UnreadStrs = list.map(sym_name_to_string, UnreadModuleList),

        io.write_string(ProgressStream, "ReadModules\n", !TIO),
        io.write_line(ProgressStream, ReadStrs, !TIO),
        io.write_string(ProgressStream, "UnreadModules\n", !TIO),
        io.write_line(ProgressStream, UnreadStrs, !TIO)
    ),

    % Check whether we could read the main `.m' file.
    map.lookup(DepsMap, ModuleName, ModuleDep),
    ModuleDep = deps(_, _, BurdenedModule),
    BurdenedModule = burdened_module(Baggage, _ParseTreeModuleSrc),
    Errors = Baggage ^ mb_errors,
    FatalErrors = Errors ^ rm_fatal_errors,
    ( if set.is_non_empty(FatalErrors) then
        FatalErrorSpecs = Errors ^ rm_fatal_error_specs,
        (
            FatalErrorSpecs = [],
            string.format("FatalErrorSpecs = [], with FatalErrors = %s\n",
                [s(string.string(FatalErrors))], UnexpectedMsg),
            unexpected($pred, UnexpectedMsg)
        ;
            FatalErrorSpecs = [_ | _],
            % The error_specs in FatalErrorSpecs may already be in !.Specs,
            % but even if they are, they will be printed just once.
            !:Specs = FatalErrorSpecs ++ !.Specs
        )
    else
        (
            Mode = output_module_dot_d_file
        ;
            Mode = output_all_program_dot_dx_files,
            generate_dep_dv_files(ProgressStream, Globals, ModuleName, DepsMap,
                Baggage, !IO)
        ),
        compute_deps_for_d_files(ProgressStream, Globals, ModuleName, DepsMap,
            IntDepsGraph, ImpDepsGraph,
            IndirectDepsGraph, IndirectOptDepsGraph,
            TransOptDepsGraph, TransOptOrder, BurdenedModules, !Specs, !IO),
        (
            Mode = output_module_dot_d_file,
            DFilesToWrite = [BurdenedModule]
        ;
            Mode = output_all_program_dot_dx_files,
            DFilesToWrite = BurdenedModules
        ),
        generate_dependencies_write_d_files(ProgressStream, Globals,
            DFilesToWrite, IntDepsGraph, ImpDepsGraph,
            IndirectDepsGraph, IndirectOptDepsGraph,
            TransOptDepsGraph, TransOptOrder, !IO)
    ).

:- pred generate_dep_dv_files(io.text_output_stream::in, globals::in,
    module_name::in, deps_map::in, module_baggage::in, io::di, io::uo) is det.

generate_dep_dv_files(ProgressStream, Globals, ModuleName, DepsMap,
        Baggage, !IO) :-
    % First, build up a map of the dependencies.
    SourceFileName = Baggage ^ mb_source_file_name,

    map.init(Cache0),
    generate_dv_file(Globals, SourceFileName, ModuleName, DepsMap,
        MmakeFileDv, Cache0, _Cache, !IO),
    generate_dep_file(Globals, SourceFileName, ModuleName, DepsMap,
        MmakeFileDep, !IO),
    MmakeFileStrDv = mmakefile_to_string(MmakeFileDv),
    MmakeFileStrDep = mmakefile_to_string(MmakeFileDep),

    % XXX LEGACY
    module_name_to_file_name_create_dirs(Globals, $pred,
        ext_cur_ngs(ext_cur_ngs_mf_dv), ModuleName,
        FileNameDv, _FileNameDvProposed, !IO),
    module_name_to_file_name_create_dirs(Globals, $pred,
        ext_cur_ngs(ext_cur_ngs_mf_dep), ModuleName,
        FileNameDep, _FileNameDepProposed, !IO),

    write_string_to_file(ProgressStream, Globals,
        "Writing auto-dependency file", FileNameDv, MmakeFileStrDv,
        _SucceededDv, !IO),
    write_string_to_file(ProgressStream, Globals,
        "Writing auto-dependency file", FileNameDep, MmakeFileStrDep,
        _SucceededDep, !IO),

    % For Java, the main target is actually a shell script
    % which will set CLASSPATH appropriately, and then invoke java
    % on the appropriate .class file. Rather than generating
    % an Mmake rule to build this file when it is needed,
    % we just generate this file at "mmake depend" time, since
    % that is simpler and probably more efficient anyway.
    globals.get_target(Globals, Target),
    (
        Target = target_java,
        create_java_shell_script(ProgressStream, Globals, ModuleName,
            _Succeeded, !IO)
    ;
        ( Target = target_c
        ; Target = target_csharp
        )
    ).

:- pred compute_deps_for_d_files(io.text_output_stream::in, globals::in,
    module_name::in, deps_map::in,
    digraph(module_name)::out, digraph(module_name)::out,
    digraph(module_name)::out, digraph(module_name)::out,
    digraph(module_name)::out, list(module_name)::out,
    list(burdened_module)::out, list(error_spec)::in, list(error_spec)::out,
    io::di, io::uo) is det.

compute_deps_for_d_files(ProgressStream, Globals, ModuleName, DepsMap,
        IntDepsGraph, ImpDepsGraph, IndirectDepsGraph, IndirectOptDepsGraph,
        TransOptDepsGraph, TransOptOrder, BurdenedModules, !Specs, !IO) :-
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
        TransOptDepsOrdering, TransOptOrder, !IO).

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
    Deps = deps(_, _, BurdenedModule),
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
    map.lookup(DepsMap, ModuleName, deps(_, _, BurdenedModule)),
    ModuleDepInfo = module_dep_info_full(BurdenedModule).

%---------------------------------------------------------------------------%

generate_d_file_fragment(Globals, BurdenedAugCompUnit, StdDeps, AllDeps,
        MaybeInclTransOptRule, FileNameD, FileContentsStrD, !Cache, !IO) :-
    BurdenedAugCompUnit = burdened_aug_comp_unit(_, AugCompUnit),
    ParseTreeModuleSrc = AugCompUnit ^ acu_module_src,
    ModuleName = ParseTreeModuleSrc ^ ptms_module_name,
    % XXX LEGACY
    module_name_to_file_name_create_dirs(Globals, $pred,
        ext_cur_ngs(ext_cur_ngs_mf_d), ModuleName,
        FileNameD, _FileNameDProposed, !IO),
    construct_intermod_deps(Globals, ParseTreeModuleSrc, StdDeps, IntermodDeps,
        !Cache, !IO),
    generate_d_file(Globals, BurdenedAugCompUnit, StdDeps, IntermodDeps,
        AllDeps, MaybeInclTransOptRule, MmakeFileD, !Cache, !IO),
    FileContentsStrD = mmakefile_to_string(MmakeFileD).

%---------------------------------------------------------------------------%

construct_std_deps(Globals, BurdenedAugCompUnit) = StdDeps :-
    BurdenedAugCompUnit = burdened_aug_comp_unit(Baggage, AugCompUnit),
    SourceFileModuleName = Baggage ^ mb_source_file_module_name,
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
                ForeignImportMod, SourceFileModuleName),
            % XXX We can't include mercury.dll as mmake can't find it,
            % but we know that it exists.
            ImportModuleName \= unqualified("mercury")
        ), FIMSpecs, ForeignImportedModuleNamesSet),

    StdDeps = std_deps(DirectDeps, IndirectDeps,
        ForeignImportedModuleNamesSet, no_trans_opt_deps).

%-----------%

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

construct_intermod_deps(Globals, ParseTreeModuleSrc, StdDeps, IntermodDeps,
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

%---------------------------------------------------------------------------%

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
        SearchAuthDirsSrc = get_search_auth_intermod_dirs(ie_src, Globals),
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
            ModuleName, OptName, !Cache, !IO),
        SearchAuthDirsPlainOpt =
            get_search_auth_intermod_dirs(ie_opt_plain, Globals),
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
            ModuleName, TransOptName, !Cache, !IO),
        SearchAuthDirsTransOpt =
            get_search_auth_intermod_dirs(ie_opt_trans, Globals),
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
        % XXX Why are we looking for *source* files in intermod_dirs?
        % Neither of our caller call sites that pass look_for_src
        % have any documentation of their reasons, and the documentation
        % of --intermod-directory says:
        %
        % "Append dir to the list of directories to be search for .opt files".
        %
        % XXX It should mention .trans_opt files as well, since we do
        % search for them in the same places.
        SearchAuthDirsSrc = get_search_auth_intermod_dirs(ie_src, Globals),
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
:- end_module parse_tree.generate_dep_d_files.
%---------------------------------------------------------------------------%
