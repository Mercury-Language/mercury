%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2008-2012 The University of Melbourne.
% Copyright (C) 2013-2017, 2019-2024 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: write_deps_file.m.
%
%---------------------------------------------------------------------------%

:- module parse_tree.write_deps_file.
:- interface.

:- import_module libs.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.file_names.
:- import_module parse_tree.generate_mmakefile_fragments.
:- import_module parse_tree.module_baggage.
:- import_module parse_tree.module_deps_graph.

:- import_module io.
:- import_module list.
:- import_module set.

:- func construct_std_deps(globals, burdened_aug_comp_unit) = std_deps.

%---------------------------------------------------------------------------%

    % write_d_file(ProgressStream, Globals, BurdenedAugCompUnit,
    %   StdDeps, AllDeps, MaybeInclTransOptRule, !IO):
    %
    % Write out the per-module makefile dependencies (`.d') file for the
    % specified module. AllDeps is the set of all module names which the
    % generated code for this module might depend on, i.e. all that have been
    % used or imported, directly or indirectly, into this module, including
    % via .opt or .trans_opt files, and including parent modules of nested
    % modules. MaybeInclTransOptRule controls whether to include a
    % trans_opt_deps rule in the file, and if so, what the rule should say.
    %
    % XXX The StdDeps allows generate_dependencies_write_d_file
    % to supply some information derived from the overall dependency graph
    % that is intended to override the values of some of the fields in
    % BurdenedAugCompUnit. These used to be passed in a ModuleAndImports
    % argument itself (the predecessor of BurdenedAugCompUnit), but they
    % do not actually belong there, since the overridden fields are
    % supposed to be *solely* from the main module in BurdenedAugCompUnit.
    % As to *why* this overriding is desirable, I (zs) don't know, and
    % I am pretty sure that the original author (fjh) does not know anymore
    % either :-(
    %
:- pred write_d_file(io.text_output_stream::in, globals::in,
    burdened_aug_comp_unit::in, std_deps::in, set(module_name)::in,
    maybe_include_trans_opt_rule::in, io::di, io::uo) is det.

    % generate_dependencies_write_d_files(ProgressStream, Globals,
    %   BurdenedModules, IntDepsGraph, ImplDepsGraph,
    %   IndirectDepsGraph, IndirectOptDepsGraph,
    %   TransOptDepsGraph, TransOptOrder, !IO):
    %
    % This predicate writes out the .d files for all the modules in the
    % BurdenedModules list.
    % IntDepsGraph gives the interface dependency graph.
    % ImplDepsGraph gives the implementation dependency graph.
    % IndirectDepsGraph gives the indirect dependency graph
    % (this includes dependencies on `*.int2' files).
    % IndirectOptDepsGraph gives the indirect optimization dependencies
    % (this includes dependencies via `.opt' and `.trans_opt' files).
    %
    % TransOptDepsGraph gives the trans-opt dependency graph for the
    % purpose of making `.trans_opt' files.
    % TransOptOrder gives the ordering that is used to determine
    % which other modules the .trans_opt files may depend on.
    %
:- pred generate_dependencies_write_d_files(io.text_output_stream::in,
    globals::in, list(burdened_module)::in,
    deps_graph::in, deps_graph::in, deps_graph::in, deps_graph::in,
    deps_graph::in, list(module_name)::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

    % get_ext_opt_deps(Globals, Ext, ModuleNames, !:OptDeps, !IO):
    %
    % For each module in ModuleNames, search intermod_directories
    % for a file with the given extension, filtering out those for which
    % the search fails.
    % XXX This won't find nested submodules.
    % XXX Use `mmc --make' if that matters.
    %
    % This predicate must operate on lists, not sets, of module names,
    % because it needs to preserve the chosen trans_opt deps ordering,
    % which is derived from the dependency graph between modules,
    % and not just the modules' names.
    %
:- pred get_ext_opt_deps(globals::in, ext::in(ext_opt),
    list(module_name)::in, list(module_name)::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.file_util.
:- import_module libs.mmakefiles.
:- import_module libs.options.
:- import_module parse_tree.find_module.        % XXX undesirable dependency
:- import_module parse_tree.get_dependencies.
:- import_module parse_tree.make_module_file_names.
:- import_module parse_tree.parse_error.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_foreign.
:- import_module parse_tree.prog_item.

:- import_module bool.
:- import_module cord.
:- import_module digraph.
:- import_module dir.
:- import_module io.file.
:- import_module map.
:- import_module maybe.
:- import_module one_or_more.
:- import_module sparse_bitset.
:- import_module string.

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

%---------------------------------------------------------------------------%

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

:- pred construct_intermod_deps(globals::in, parse_tree_module_src::in,
    std_deps::in, set(module_name)::in, intermod_deps::out,
    module_file_name_cache::in, module_file_name_cache::out,
    io::di, io::uo) is det.

construct_intermod_deps(Globals, ParseTreeModuleSrc, StdDeps, AllDeps,
        IntermodDeps, !Cache, !IO) :-
    % XXX Note that currently, due to a design problem, handle_options.m
    % *always* sets use_opt_files to no.
    globals.lookup_bool_option(Globals, use_opt_files, UsePlainOpt),
    globals.lookup_bool_option(Globals, intermodule_optimization, Intermod),
    (
        Intermod = yes,
        % If intermodule_optimization is enabled, then all the .mh files
        % must exist, because it is possible that the .c file imports them
        % directly or indirectly.
        MaybeMhDeps = intermod_mh_deps(AllDeps)
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
        StdDeps = std_deps(DirectDeps, _, _, _),
        ModuleName = ParseTreeModuleSrc ^ ptms_module_name,
        BaseDeps = [ModuleName | set.to_sorted_list(DirectDeps)],
        ( if
            ( TransOpt = yes
            ; UseTransOpt = yes
            )
        then
            get_plain_trans_opt_deps(Globals, BaseDeps,
                PlainOptDeps, TransOptDeps, !Cache, !IO),
            MaybeTransOptDeps = yes(TransOptDeps)
        else
            % XXX LEGACY
            ExtOpt = ext_cur_ngs_gs_max_ngs(
                ext_cur_ngs_gs_max_ngs_legacy_opt_plain),
            get_ext_opt_deps(Globals, ExtOpt, BaseDeps, PlainOptDeps, !IO),
            MaybeTransOptDeps = no
        ),
        MaybeOptFileDeps = opt_file_deps(PlainOptDeps, MaybeTransOptDeps)
    else
        MaybeOptFileDeps = no_opt_file_deps
    ),
    IntermodDeps = intermod_deps(MaybeMhDeps, MaybeOptFileDeps).

%---------------------------------------------------------------------------%

write_d_file(ProgressStream, Globals, BurdenedAugCompUnit, StdDeps, AllDeps,
        MaybeInclTransOptRule, !IO) :-
    map.init(Cache0),
    generate_d_file_fragment(Globals, BurdenedAugCompUnit, StdDeps, AllDeps,
        MaybeInclTransOptRule, FileNameD, FileContentsStrD,
        Cache0, _Cache, !IO),
    write_out_d_file(ProgressStream, Globals, FileNameD,
        FileContentsStrD, !IO).

:- pred generate_d_file_fragment(globals::in, burdened_aug_comp_unit::in,
    std_deps::in, set(module_name)::in, maybe_include_trans_opt_rule::in,
    file_name::out, string::out,
    module_file_name_cache::in, module_file_name_cache::out,
    io::di, io::uo) is det.

generate_d_file_fragment(Globals, BurdenedAugCompUnit, StdDeps, AllDeps,
        MaybeInclTransOptRule, FileNameD, FileContentsStrD, !Cache, !IO) :-
    BurdenedAugCompUnit = burdened_aug_comp_unit(_, AugCompUnit),
    ParseTreeModuleSrc = AugCompUnit ^ acu_module_src,
    ModuleName = ParseTreeModuleSrc ^ ptms_module_name,
    % XXX LEGACY
    module_name_to_file_name_create_dirs(Globals, $pred,
        ext_cur_ngs(ext_cur_ngs_mf_d), ModuleName,
        FileNameD, _FileNameDProposed, !IO),

    construct_intermod_deps(Globals, ParseTreeModuleSrc, StdDeps, AllDeps,
        IntermodDeps, !Cache, !IO),
    generate_d_file(Globals, BurdenedAugCompUnit, StdDeps,
        IntermodDeps, AllDeps, MaybeInclTransOptRule, MmakeFileD, !Cache, !IO),
    FileContentsStrD = mmakefile_to_string(MmakeFileD).

:- pred write_out_d_file(io.text_output_stream::in, globals::in,
    file_name::in, string::in, io::di, io::uo) is det.

write_out_d_file(ProgressStream, Globals, FileNameD, FileContentsStrD, !IO) :-
    % To avoid problems with concurrent updates of `.d' files during
    % parallel makes, we first create the file with a temporary name,
    % and then rename it to the desired name when we have finished.
    % XXX I (zs) think that if two mmake actions both update the same .d file,
    % then having them executed in parallel is itself a bug, and that bug
    % should be fixed, instead of being accommodated, as we do here.
    % Therefore I think that this predicate should be deleted,
    % and its uses replaced by calls to write_string_to_file.
    io.file.make_temp_file(dir.dirname(FileNameD), "tmp_d", "",
        TmpFileNameDResult, !IO),
    (
        TmpFileNameDResult = error(Error),
        Message = "Could not create temporary file: " ++ error_message(Error),
        report_error(ProgressStream, Message, !IO)
    ;
        TmpFileNameDResult = ok(TmpFileNameD),
        globals.lookup_bool_option(Globals, verbose, Verbose),
        (
            Verbose = no
        ;
            Verbose = yes,
            io.format(ProgressStream,
                "%% Writing auto-dependency file `%s'...",
                [s(FileNameD)], !IO),
            io.flush_output(ProgressStream, !IO)
        ),
        io.open_output(TmpFileNameD, Result, !IO),
        (
            Result = error(IOError),
            maybe_write_string(ProgressStream, Verbose, " failed.\n", !IO),
            maybe_flush_output(ProgressStream, Verbose, !IO),
            io.error_message(IOError, IOErrorMessage),
            string.format("error opening temporary file `%s' for output: %s",
                [s(TmpFileNameD), s(IOErrorMessage)], Message),
            report_error(ProgressStream, Message, !IO)
        ;
            Result = ok(DepStream),
            io.write_string(DepStream, FileContentsStrD, !IO),
            io.close_output(DepStream, !IO),

            io.file.rename_file(TmpFileNameD, FileNameD,
                FirstRenameResult, !IO),
            (
                FirstRenameResult = error(_),
                % On some systems, we need to remove the existing file first,
                % if any. So try again that way.
                io.file.remove_file(FileNameD, RemoveResult, !IO),
                (
                    RemoveResult = error(Error4),
                    maybe_write_string(ProgressStream, Verbose,
                        " failed.\n", !IO),
                    maybe_flush_output(ProgressStream, Verbose, !IO),
                    io.error_message(Error4, ErrorMsg),
                    string.format("can't remove file `%s': %s",
                        [s(FileNameD), s(ErrorMsg)], Message),
                    report_error(ProgressStream, Message, !IO)
                ;
                    RemoveResult = ok,
                    io.file.rename_file(TmpFileNameD,
                        FileNameD, SecondRenameResult, !IO),
                    (
                        SecondRenameResult = error(Error5),
                        maybe_write_string(ProgressStream, Verbose,
                            " failed.\n", !IO),
                        maybe_flush_output(ProgressStream, Verbose, !IO),
                        io.error_message(Error5, ErrorMsg),
                        string.format("can't rename file `%s' as `%s': %s",
                            [s(TmpFileNameD), s(FileNameD),
                            s(ErrorMsg)], Message),
                        report_error(ProgressStream, Message, !IO)
                    ;
                        SecondRenameResult = ok,
                        maybe_write_string(ProgressStream, Verbose,
                            " done.\n", !IO)
                    )
                )
            ;
                FirstRenameResult = ok,
                maybe_write_string(ProgressStream, Verbose, " done.\n", !IO)
            )
        )
    ).

%---------------------------------------------------------------------------%

generate_dependencies_write_d_files(ProgressStream,
        Globals, BurdenedModules, IntDepsGraph, ImpDepsGraph,
        IndirectDepsGraph, IndirectOptDepsGraph,
        TransOptDepsGraph, TransOptOrder, !IO) :-
    map.init(Cache0),
    generate_dependencies_write_d_files_loop(ProgressStream,
        Globals, BurdenedModules, IntDepsGraph, ImpDepsGraph,
        IndirectDepsGraph, IndirectOptDepsGraph,
        TransOptDepsGraph, TransOptOrder, Cache0, _Cache, !IO).

:- pred generate_dependencies_write_d_files_loop(io.text_output_stream::in,
    globals::in, list(burdened_module)::in,
    deps_graph::in, deps_graph::in, deps_graph::in, deps_graph::in,
    deps_graph::in, list(module_name)::in,
    module_file_name_cache::in, module_file_name_cache::out,
    io::di, io::uo) is det.

generate_dependencies_write_d_files_loop(_, _, [], _, _, _, _, _, _,
        !Cache, !IO).
generate_dependencies_write_d_files_loop(ProgressStream, Globals,
        [BurdenedModule | BurdenedModules],
        IntDepsGraph, ImpDepsGraph, IndirectDepsGraph, IndirectOptDepsGraph,
        TransOptDepsGraph, TransOptOrder, !Cache, !IO) :-
    generate_dependencies_write_d_file(ProgressStream, Globals,
        BurdenedModule, IntDepsGraph, ImpDepsGraph,
        IndirectDepsGraph, IndirectOptDepsGraph,
        TransOptDepsGraph, TransOptOrder, !Cache, !IO),
    generate_dependencies_write_d_files_loop(ProgressStream, Globals,
        BurdenedModules, IntDepsGraph, ImpDepsGraph,
        IndirectDepsGraph, IndirectOptDepsGraph,
        TransOptDepsGraph, TransOptOrder, !Cache, !IO).

:- pred generate_dependencies_write_d_file(io.text_output_stream::in,
    globals::in, burdened_module::in, deps_graph::in, deps_graph::in,
    deps_graph::in, deps_graph::in, deps_graph::in, list(module_name)::in,
    module_file_name_cache::in, module_file_name_cache::out,
    io::di, io::uo) is det.

generate_dependencies_write_d_file(ProgressStream, Globals,
        BurdenedModule, IntDepsGraph, ImpDepsGraph,
        IndirectDepsGraph, IndirectOptDepsGraph,
        TransOptDepsGraph, FullTransOptOrder, !Cache, !IO) :-
    BurdenedModule = burdened_module(Baggage, ParseTreeModuleSrc),

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

    % Compute the maximum allowable trans-opt dependencies for this module.
    % To avoid the possibility of cycles, each module is only allowed to depend
    % on modules that occur after it in FullTransOptOrder.
    NotThisModule =
        ( pred(OtherModule::in) is semidet :-
            ModuleName \= OtherModule
        ),
    list.drop_while(NotThisModule, FullTransOptOrder, TailTransOptOrder),
    ( if TailTransOptOrder = [_ | TransOptOrderList] then
        % The module was found in the list.
        set.list_to_set(TransOptOrderList, TransOptOrder)
    else
        set.init(TransOptOrder)
    ),
    TransOptRuleInfo = trans_opt_deps_from_order(TransOptOrder),
    MaybeInclTransOptRule = include_trans_opt_rule(TransOptRuleInfo),

    % XXX DFILE Note that even if a fatal error occurred for one of the files
    % that the current Module depends on, a .d file is still produced,
    % even though it probably contains incorrect information.
    ModuleErrors = Baggage ^ mb_errors,
    FatalErrors = ModuleErrors ^ rm_fatal_errors,
    ( if set.is_empty(FatalErrors) then
        init_aug_compilation_unit(ParseTreeModuleSrc, AugCompUnit),
        BurdenedAugCompUnit = burdened_aug_comp_unit(Baggage, AugCompUnit),
        % XXX DFILE The way IndirectOptDeps is computed seems to have nothing
        % to do with the way the generate_d_file_fragment predicate's
        % corresponding argument is computed. This seems to me (zs)
        % to be a BUG.
        generate_d_file_fragment(Globals, BurdenedAugCompUnit,
            StdDeps, IndirectOptDeps, MaybeInclTransOptRule,
            FileNameD, FileContentsStrD, !Cache, !IO),
        write_out_d_file(ProgressStream, Globals, FileNameD,
            FileContentsStrD, !IO)
    else
        true
    ).

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

%---------------------------------------------------------------------------%

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
:- pred get_plain_trans_opt_deps(globals::in,
    list(module_name)::in, list(module_name)::out, list(module_name)::out,
    module_file_name_cache::in, module_file_name_cache::out,
    io::di, io::uo) is det.

get_plain_trans_opt_deps(_, [], [], [], !Cache, !IO).
get_plain_trans_opt_deps(Globals, [ModuleName | ModuleNames],
        !:OptDeps, !:TransOptDeps, !Cache, !IO) :-
    get_plain_trans_opt_deps(Globals, ModuleNames,
        !:OptDeps, !:TransOptDeps, !Cache, !IO),
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
    ).

get_ext_opt_deps(_, _, [], [], !IO).
get_ext_opt_deps(Globals, Ext, [ModuleName | ModuleNames], !:OptDeps, !IO) :-
    get_ext_opt_deps(Globals, Ext, ModuleNames, !:OptDeps, !IO),
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
    ).

%---------------------------------------------------------------------------%
:- end_module parse_tree.write_deps_file.
%---------------------------------------------------------------------------%
