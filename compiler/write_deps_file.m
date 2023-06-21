%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2008-2011 The University of Melbourne.
% Copyright (C) 2013-2017, 2019-2023 The Mercury team.
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
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.deps_map.
:- import_module parse_tree.file_names.
:- import_module parse_tree.module_baggage.
:- import_module parse_tree.module_deps_graph.

:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module set.

:- type maybe_intermod_deps
    --->    no_intermod_deps
    ;       intermod_deps(
                id_int_deps         :: set(module_name),
                id_imp_deps         :: set(module_name),
                id_indirect_deps    :: set(module_name),
                id_fim_deps         :: set(module_name),
                % id_trans_opt_deps is the set of modules whose .trans_opt
                % files that we may want to read when *making* this module's
                % .trans_opt file. However, it still may need to be reduced
                % further to prevent circularities in trans_opt_deps mmake
                % rules.
                id_trans_opt_deps   :: set(module_name)
            ).

:- type maybe_include_trans_opt_rule
    --->    do_not_include_trans_opt_rule
    ;       include_trans_opt_rule(trans_opt_rule_info).

    % The set of trans-opt dependencies can come from two sources:
    % - from a topological sort of the trans-opt dependency graph
    %   if we built the graph; or
    % - from a trans_opt_deps rule in a .d file.
    %
:- type trans_opt_rule_info
    --->    trans_opt_deps_from_order(set(module_name))
    ;       trans_opt_deps_from_d_file(set(module_name)).

    % write_dependency_file(Globals, BurdenedAugCompUnit, MaybeIntermodDeps,
    %   AllDeps, MaybeInclTransOptRule, !IO):
    %
    % Write out the per-module makefile dependencies (`.d') file for the
    % specified module. AllDeps is the set of all module names which the
    % generated code for this module might depend on, i.e. all that have been
    % used or imported, directly or indirectly, into this module, including
    % via .opt or .trans_opt files, and including parent modules of nested
    % modules. MaybeInclTransOptRule controls whether to include a
    % trans_opt_deps rule in the file, and if so, what the rule should say.
    %
    % XXX The MaybeIntermodDeps allows generate_dependencies_write_d_file
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
:- pred write_dependency_file(globals::in, burdened_aug_comp_unit::in,
    maybe_intermod_deps::in, set(module_name)::in,
    maybe_include_trans_opt_rule::in,
    io::di, io::uo) is det.

    % generate_dependencies_write_d_files(Globals, Modules,
    %   IntDepsGraph, ImplDepsGraph, IndirectDepsGraph, IndirectOptDepsGraph,
    %   TransOptDepsGraph, TransOptOrder, DepsMap, !IO):
    %
    % This predicate writes out the .d files for all the modules in the
    % Modules list.
    % IntDepsGraph gives the interface dependency graph.
    % ImplDepsGraph gives the implementation dependency graph.
    % IndirectDepsGraph gives the indirect dependency graph
    % (this includes dependencies on `*.int2' files).
    % IndirectOptDepsGraph gives the indirect optimization dependencies
    % (this includes dependencies via `.opt' and `.trans_opt' files).
    % These are all computed from the DepsMap.
    %
    % TransOptDepsGraph gives the trans-opt dependency graph for the
    % purpose of making `.trans_opt' files.
    % TransOptOrder gives the ordering that is used to determine
    % which other modules the .trans_opt files may depend on.
    %
:- pred generate_dependencies_write_d_files(globals::in, list(deps)::in,
    deps_graph::in, deps_graph::in, deps_graph::in, deps_graph::in,
    deps_graph::in, list(module_name)::in, deps_map::in, io::di, io::uo)
    is det.

    % Write out the `.dv' file, using the information collected in the
    % deps_map data structure.
    %
:- pred generate_dependencies_write_dv_file(globals::in, file_name::in,
    module_name::in, deps_map::in, io::di, io::uo) is det.

    % Write out the `.dep' file, using the information collected in the
    % deps_map data structure.
    %
:- pred generate_dependencies_write_dep_file(globals::in, file_name::in,
    module_name::in, deps_map::in, io::di, io::uo) is det.

:- pred output_module_order(globals::in, module_name::in,
    other_ext::in, newext::in, list(set(module_name))::in,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%

    % For each dependency, search intermod_directories for a file with
    % the given extension, filtering out those for which the search fails.
    % If --use-opt-files is set, only look for `.opt' files,
    % not `.m' files.
    % XXX This won't find nested submodules.
    % XXX Use `mmc --make' if that matters.
    %
    % This predicate must operate on lists, not sets, of module names,
    % because it needs to preserve the chosen trans_opt deps ordering,
    % which is derived from the dependency graph between modules,
    % and not just the modules' names.
    %
:- pred get_opt_deps(globals::in, bool::in, list(string)::in,
    other_ext::in, newext::in,
    list(module_name)::in, list(module_name)::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.mmakefiles.
:- import_module libs.options.
:- import_module parse_tree.find_module.        % XXX undesirable dependency
:- import_module parse_tree.get_dependencies.
:- import_module parse_tree.module_cmds.
:- import_module parse_tree.parse_error.
:- import_module parse_tree.parse_tree_out_sym_name.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_foreign.
:- import_module parse_tree.prog_item.
:- import_module parse_tree.source_file_map.

:- import_module cord.
:- import_module digraph.
:- import_module dir.
:- import_module io.file.
:- import_module library.
:- import_module map.
:- import_module maybe.
:- import_module one_or_more.
:- import_module one_or_more_map.
:- import_module pair.
:- import_module require.
:- import_module sparse_bitset.
:- import_module string.

:- type module_file_name_cache == map(module_name_and_ext, file_name).

:- type module_name_and_ext
    --->    module_name_and_ext(module_name, other_ext).

%---------------------------------------------------------------------------%

write_dependency_file(Globals, BurdenedAugCompUnit, IntermodDeps, AllDeps,
        MaybeInclTransOptRule, !IO) :-
    map.init(Cache0),
    write_dependency_file_fn_cache(Globals, BurdenedAugCompUnit, IntermodDeps,
        AllDeps, MaybeInclTransOptRule, Cache0, _Cache, !IO).

:- pred write_dependency_file_fn_cache(globals::in,
    burdened_aug_comp_unit::in, maybe_intermod_deps::in, set(module_name)::in,
    maybe_include_trans_opt_rule::in,
    module_file_name_cache::in, module_file_name_cache::out,
    io::di, io::uo) is det.

write_dependency_file_fn_cache(Globals, BurdenedAugCompUnit, IntermodDeps,
        AllDeps, MaybeInclTransOptRule, !Cache, !IO) :-
    % To avoid problems with concurrent updates of `.d' files during
    % parallel makes, we first create the file with a temporary name,
    % and then rename it to the desired name when we have finished.
    BurdenedAugCompUnit = burdened_aug_comp_unit(_, AugCompUnit),
    ParseTreeModuleSrc = AugCompUnit ^ acu_module_src,
    ModuleName = ParseTreeModuleSrc ^ ptms_module_name,
    module_name_to_file_name(Globals, $pred, do_create_dirs,
        ext_other(other_ext(".d")), newext_mmake_fragment(ext_mf_d),
        ModuleName, DependencyFileName, !IO),
    io.file.make_temp_file(dir.dirname(DependencyFileName), "tmp_d", "",
        TmpDependencyFileNameRes, !IO),
    get_error_output_stream(Globals, ModuleName, ErrorStream, !IO),
    get_progress_output_stream(Globals, ModuleName, ProgressStream, !IO),
    (
        TmpDependencyFileNameRes = error(Error),
        Message = "Could not create temporary file: " ++ error_message(Error),
        report_error(ErrorStream, Message, !IO)
    ;
        TmpDependencyFileNameRes = ok(TmpDependencyFileName),
        globals.lookup_bool_option(Globals, verbose, Verbose),
        (
            Verbose = no
        ;
            Verbose = yes,
            io.format(ProgressStream,
                "%% Writing auto-dependency file `%s'...",
                [s(DependencyFileName)], !IO),
            io.flush_output(ProgressStream, !IO)
        ),
        io.open_output(TmpDependencyFileName, Result, !IO),
        (
            Result = error(IOError),
            maybe_write_string(ProgressStream, Verbose, " failed.\n", !IO),
            maybe_flush_output(ProgressStream, Verbose, !IO),
            io.error_message(IOError, IOErrorMessage),
            string.format("error opening temporary file `%s' for output: %s",
                [s(TmpDependencyFileName), s(IOErrorMessage)], Message),
            report_error(ErrorStream, Message, !IO)
        ;
            Result = ok(DepStream),
            generate_d_file(Globals, BurdenedAugCompUnit, IntermodDeps,
                AllDeps, MaybeInclTransOptRule, MmakeFile, !Cache, !IO),
            write_mmakefile(DepStream, MmakeFile, !IO),
            io.close_output(DepStream, !IO),

            io.file.rename_file(TmpDependencyFileName, DependencyFileName,
                FirstRenameResult, !IO),
            (
                FirstRenameResult = error(_),
                % On some systems, we need to remove the existing file first,
                % if any. So try again that way.
                io.file.remove_file(DependencyFileName, RemoveResult, !IO),
                (
                    RemoveResult = error(Error4),
                    maybe_write_string(ProgressStream, Verbose,
                        " failed.\n", !IO),
                    maybe_flush_output(ProgressStream, Verbose, !IO),
                    io.error_message(Error4, ErrorMsg),
                    string.format("can't remove file `%s': %s",
                        [s(DependencyFileName), s(ErrorMsg)], Message),
                    report_error(ErrorStream, Message, !IO)
                ;
                    RemoveResult = ok,
                    io.file.rename_file(TmpDependencyFileName,
                        DependencyFileName, SecondRenameResult, !IO),
                    (
                        SecondRenameResult = error(Error5),
                        maybe_write_string(ProgressStream, Verbose,
                            " failed.\n", !IO),
                        maybe_flush_output(ProgressStream, Verbose, !IO),
                        io.error_message(Error5, ErrorMsg),
                        string.format("can't rename file `%s' as `%s': %s",
                            [s(TmpDependencyFileName), s(DependencyFileName),
                            s(ErrorMsg)], Message),
                        report_error(ErrorStream, Message, !IO)
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
%---------------------------------------------------------------------------%

    % Generate the contents of the module's .d file.
    %
    % The mmake rules we construct treat C differently from Java and C#.
    % The reason is that we support using mmake when targeting C, but require
    % the use of --use-mmc-make when targeting Java and C#.
    %
    % Initially, when the only target language was C, the only build system
    % we had was mmake, so the mmake rules we generate here can do everything
    % that one wants to do when targeting C. When we added the ability to
    % target C# and Erlang, we implemented it for --use-mmc-make only,
    % *not* for mmake, so the entries we generate for C# and Erlang
    % mostly just forward the work to --use-mmc-make. Java is in between;
    % there are more mmake rules for it than for C# or Erlang, but far from
    % enough for full functionality. In an email to m-rev on 2020 may 25,
    % Julien said: "IIRC, most of the mmake rules for Java that are not
    % required by --use-mmc-make are long obsolete". Unfortunately,
    % apparently there is no documentation of *which* mmake rules for Java
    % are required by --use-mmc-make.
    %
:- pred generate_d_file(globals::in, burdened_aug_comp_unit::in,
    maybe_intermod_deps::in,
    set(module_name)::in, maybe_include_trans_opt_rule::in,
    mmakefile::out, module_file_name_cache::in, module_file_name_cache::out,
    io::di, io::uo) is det.

generate_d_file(Globals, BurdenedAugCompUnit, IntermodDeps,
        AllDeps, MaybeInclTransOptRule, !:MmakeFile, !Cache, !IO) :-
    BurdenedAugCompUnit = burdened_aug_comp_unit(Baggage, AugCompUnit),
    SourceFileName = Baggage ^ mb_source_file_name,
    SourceFileModuleName = Baggage ^ mb_source_file_module_name,
    MaybeTopModule = Baggage ^ mb_maybe_top_module,
    ParseTreeModuleSrc = AugCompUnit ^ acu_module_src,
    ModuleName = ParseTreeModuleSrc ^ ptms_module_name,
    ModuleNameString = sym_name_to_string(ModuleName),
    Ancestors = get_ancestors_set(ModuleName),
    (
        IntermodDeps = no_intermod_deps,
        map.keys_as_set(ParseTreeModuleSrc ^ ptms_import_use_map, LongDeps0),
        IndirectIntSpecs = AugCompUnit ^ acu_indirect_int2_specs,
        map.keys_as_set(IndirectIntSpecs, IndirectDeps)
    ;
        IntermodDeps = intermod_deps(IntDeps, ImpDeps, IndirectDeps, _FIMDeps,
            _TransOptDeps),
        set.union(IntDeps, ImpDeps, LongDeps0)
    ),
    PublicChildrenMap = ParseTreeModuleSrc ^ ptms_int_includes,
    one_or_more_map.keys_as_set(PublicChildrenMap, PublicChildren),
    get_fact_tables(ParseTreeModuleSrc, FactTableFileNamesSet),
    get_foreign_include_file_infos(ParseTreeModuleSrc, ForeignIncludeFiles),

    library.version(Version, FullArch),

    MmakeStartComment = mmake_start_comment("module dependencies",
        ModuleNameString, SourceFileName, Version, FullArch),

    module_name_to_make_var_name(ModuleName, ModuleMakeVarName),

    set.delete(ModuleName, LongDeps0, LongDeps),
    ShortDeps0 = IndirectDeps,
    set.difference(ShortDeps0, LongDeps, ShortDeps1),
    set.delete(ModuleName, ShortDeps1, ShortDeps),

    make_module_file_name(Globals, $pred,
        ext_other(other_ext(".trans_opt_date")),
        newext_opt(ext_opt_date_trans),
        ModuleName, TransOptDateFileName, !Cache, !IO),
    construct_trans_opt_deps_rule(Globals, MaybeInclTransOptRule, IntermodDeps,
        TransOptDateFileName, MmakeRulesTransOpt, !Cache, !IO),

    construct_fact_tables_entries(ModuleMakeVarName,
        SourceFileName, ObjFileName, FactTableFileNamesSet,
        MmakeVarsFactTables, FactTableSourceGroups, MmakeRulesFactTables),

    ( if string.remove_suffix(SourceFileName, ".m", SourceFileBase) then
        ErrFileName = SourceFileBase ++ ".err"
    else
        unexpected($pred, "source file name doesn't end in `.m'")
    ),

    make_module_file_name(Globals, $pred,
        ext_other(other_ext(".optdate")), newext_opt(ext_opt_date_plain),
        ModuleName, OptDateFileName, !Cache, !IO),
    make_module_file_name(Globals, $pred,
        ext_other(other_ext(".c_date")), newext_target_date(ext_target_date_c),
        ModuleName, CDateFileName, !Cache, !IO),
    make_module_file_name(Globals, $pred,
        ext_other(other_ext(".$O")), newext_target_obj(ext_obj_dollar_o),
        ModuleName, ObjFileName, !Cache, !IO),
    make_module_file_name(Globals, $pred,
        ext_other(other_ext(".java_date")),
        newext_target_date(ext_target_date_java),
        ModuleName, JavaDateFileName, !Cache, !IO),
    % XXX Why is the extension hardcoded to .pic_o here?  That looks wrong.
    % It should probably be .$(EXT_FOR_PIC_OBJECT) - juliensf.
    make_module_file_name(Globals, $pred,
        ext_other(other_ext(".pic_o")), newext_target_obj(ext_obj_pic_o),
        ModuleName, PicObjFileName, !Cache, !IO),
    make_module_file_name(Globals, $pred,
        ext_other(other_ext(".int0")), newext_int(ext_int_int0),
        ModuleName, Int0FileName, !Cache, !IO),

    construct_date_file_deps_rule(Globals, ModuleName, SourceFileName,
        Ancestors, LongDeps, ShortDeps, PublicChildren, Int0FileName,
        OptDateFileName, TransOptDateFileName, ForeignIncludeFiles,
        CDateFileName, JavaDateFileName, ErrFileName,
        FactTableSourceGroups, MmakeRuleDateFileDeps, !Cache, !IO),

    construct_build_nested_children_first_rule(Globals,
        ModuleName, MaybeTopModule, MmakeRulesNestedDeps, !Cache, !IO),

    construct_intermod_rules(Globals, ModuleName, LongDeps, AllDeps,
        ErrFileName, TransOptDateFileName, CDateFileName, JavaDateFileName,
        ObjFileName, MmakeRulesIntermod, !Cache, !IO),

    make_module_file_name(Globals, $pred,
        ext_other(other_ext(".c")), newext_target_c_cs(ext_target_c),
        ModuleName, CFileName, !Cache, !IO),
    construct_c_header_rules(Globals, ModuleName, AllDeps,
        CFileName, ObjFileName, PicObjFileName, MmakeRulesCHeaders,
        !Cache, !IO),

    construct_module_dep_fragment(Globals, ModuleName, CFileName,
        MmakeFragmentModuleDep, !Cache, !IO),

    make_module_file_name(Globals, $pred,
        ext_other(other_ext(".date")), newext_int(ext_int_date_int12),
        ModuleName, DateFileName, !Cache, !IO),
    make_module_file_name(Globals, $pred,
        ext_other(other_ext(".date0")), newext_int(ext_int_date_int0),
        ModuleName, Date0FileName, !Cache, !IO),
    construct_self_and_parent_date_date0_rules(Globals, SourceFileName,
        Date0FileName, DateFileName, Ancestors, LongDeps, ShortDeps,
        MmakeRulesParentDates, !Cache, !IO),

    construct_foreign_import_rules(Globals, AugCompUnit, IntermodDeps,
        SourceFileModuleName, ObjFileName, PicObjFileName,
        MmakeRulesForeignImports, !Cache, !IO),

    make_module_file_name(Globals, $pred,
        ext_other(other_ext(".date3")), newext_int(ext_int_date_int3),
        ModuleName, Date3FileName,
        !Cache, !IO),
    construct_install_shadow_rules(Globals, ModuleName,
        Int0FileName, Date0FileName, DateFileName, Date3FileName,
        OptDateFileName, TransOptDateFileName,
        MmakeRulesInstallShadows, !Cache, !IO),

    construct_subdir_short_rules(Globals, ModuleName,
        MmakeRulesSubDirShorthand, !Cache, !IO),

    have_source_file_map(HaveMap, !IO),
    construct_any_needed_pattern_rules(HaveMap,
        ModuleName, SourceFileModuleName, SourceFileName,
        Date0FileName, DateFileName, Date3FileName,
        OptDateFileName, TransOptDateFileName, CDateFileName, JavaDateFileName,
        MmakeRulesPatterns),

    start_mmakefile(!:MmakeFile),
    add_mmake_entry(MmakeStartComment, !MmakeFile),
    add_mmake_entries(MmakeRulesTransOpt, !MmakeFile),
    add_mmake_entries(MmakeVarsFactTables, !MmakeFile),
    add_mmake_entry(MmakeRuleDateFileDeps, !MmakeFile),
    add_mmake_entries(MmakeRulesFactTables, !MmakeFile),
    add_mmake_entries(MmakeRulesNestedDeps, !MmakeFile),
    add_mmake_entries(MmakeRulesIntermod, !MmakeFile),
    add_mmake_entries(MmakeRulesCHeaders, !MmakeFile),
    add_mmake_fragment(MmakeFragmentModuleDep, !MmakeFile),
    add_mmake_entries(MmakeRulesParentDates, !MmakeFile),
    add_mmake_entries(MmakeRulesForeignImports, !MmakeFile),
    add_mmake_entries(MmakeRulesInstallShadows, !MmakeFile),
    add_mmake_entries(MmakeRulesSubDirShorthand, !MmakeFile),
    add_mmake_entries(MmakeRulesPatterns, !MmakeFile).

%---------------------%

:- pred construct_trans_opt_deps_rule(globals::in,
    maybe_include_trans_opt_rule::in, maybe_intermod_deps::in,
    string::in, list(mmake_entry)::out,
    module_file_name_cache::in, module_file_name_cache::out,
    io::di, io::uo) is det.

construct_trans_opt_deps_rule(Globals, MaybeInclTransOptRule, IntermodDeps,
        TransOptDateFileName, MmakeRulesTransOpt, !Cache, !IO) :-
    (
        MaybeInclTransOptRule = include_trans_opt_rule(TransOptRuleInfo),
        % There are two cases when we will write a trans_opt_deps rule.
        (
            TransOptRuleInfo = trans_opt_deps_from_order(TransOptOrder),
            % We reach this case when explicitly generating dependencies.
            %
            % TransOptDeps0 are the dependencies taken from the trans-opt
            % dependency graph (which may have edges deleted by the user).
            %
            % TransOptOrder contains the list of modules that occur later
            % than the current module in a topological ordering of the
            % trans-opt dependency graph.
            %
            % We take the intersection of TransOptOrder and TransOptDeps0
            % to eliminate any circularities that might arise in the
            % trans_opt_deps rules if we were to use TransOptDeps0 as-is.
            (
                IntermodDeps = intermod_deps(_, _, _, _, TransOptDeps0),
                set.intersect(TransOptOrder, TransOptDeps0, TransOptDeps)
            ;
                IntermodDeps = no_intermod_deps,
                unexpected($pred, "no_intermod_deps")
            )
        ;
            TransOptRuleInfo = trans_opt_deps_from_d_file(DFileTransOptDeps),
            % We reach this case when the `.d' file is being automatically
            % rewritten after producing target code, etc. We will not have
            % computed the trans-opt dependency graph, and we will not have
            % read the trans-opt-deps-spec file.
            %
            % What we can do is write the new `.d' file with the same trans-opt
            % dependencies as the old `.d' file. As source files are modified,
            % the trans-opt dependencies listed in the `.d' file may become out
            % of date, so the user will need to explicitly regenerate
            % dependencies.
            %
            % Note: we used to take the intersection with LongDeps (in the
            % caller), but this case was not separated from the previous case
            % and it greatly reduces the set of dependencies, so I'm not sure
            % if it was intentional. --pw
            TransOptDeps = DFileTransOptDeps
        ),
        % Note that maybe_read_dependency_file searches for
        % this exact pattern.
        make_module_file_names_with_suffix(Globals,
            ext_other(other_ext(".trans_opt")), newext_opt(ext_opt_trans),
            set.to_sorted_list(TransOptDeps), TransOptDepsFileNames,
            !Cache, !IO),
        MmakeRuleTransOpt = mmake_simple_rule("trans_opt_deps",
            mmake_rule_is_not_phony,
            TransOptDateFileName,
            TransOptDepsFileNames,
            []),
        MmakeRulesTransOpt = [MmakeRuleTransOpt]
    ;
        MaybeInclTransOptRule = do_not_include_trans_opt_rule,
        MmakeRulesTransOpt = []
    ).

%---------------------%

:- pred construct_fact_tables_entries(string::in, string::in, string::in,
    set(string)::in,
    list(mmake_entry)::out, list(mmake_file_name_group)::out,
    list(mmake_entry)::out) is det.

construct_fact_tables_entries(ModuleMakeVarName, SourceFileName, ObjFileName,
        FactTableFileNamesSet, MmakeVarsFactTables, FactTableSourceGroups,
        MmakeRulesFactTables) :-
    FactTableFileNames = set.to_sorted_list(FactTableFileNamesSet),
    (
        FactTableFileNames = [_ | _],
        MmakeVarFactTables = mmake_var_defn_list(
            ModuleMakeVarName ++ ".fact_tables",
            FactTableFileNames),
        MmakeVarFactTablesOs = mmake_var_defn(
            ModuleMakeVarName ++ ".fact_tables.os",
            "$(" ++ ModuleMakeVarName ++ ".fact_tables:%=$(os_subdir)%.$O)"),
        MmakeVarFactTablesAllOs = mmake_var_defn(
            ModuleMakeVarName ++ ".fact_tables.all_os",
            "$(" ++ ModuleMakeVarName ++ ".fact_tables:%=$(os_subdir)%.$O)"),
        MmakeVarFactTablesCs = mmake_var_defn(
            ModuleMakeVarName ++ ".fact_tables.cs",
            "$(" ++ ModuleMakeVarName ++ ".fact_tables:%=$(cs_subdir)%.c)"),
        MmakeVarFactTablesAllCs = mmake_var_defn(
            ModuleMakeVarName ++ ".fact_tables.all_cs",
            "$(" ++ ModuleMakeVarName ++ ".fact_tables:%=$(cs_subdir)%.c)"),
        MmakeVarsFactTables =
            [MmakeVarFactTables,
            MmakeVarFactTablesOs, MmakeVarFactTablesAllOs,
            MmakeVarFactTablesCs, MmakeVarFactTablesAllCs],

        FactTableSourceGroup = mmake_file_name_group("fact tables",
            one_or_more("$(" ++ ModuleMakeVarName ++ ".fact_tables)", [])),
        FactTableSourceGroups = [FactTableSourceGroup],

        % XXX These rules seem wrong to me. -zs
        MmakeRuleFactOs = mmake_simple_rule("fact_table_os",
            mmake_rule_is_not_phony,
            "$(" ++ ModuleMakeVarName ++ ".fact_tables.os)",
            ["$(" ++ ModuleMakeVarName ++  ".fact_tables)", SourceFileName],
            []),
        MmakeRuleFactCs = mmake_simple_rule("fact_table_cs",
            mmake_rule_is_not_phony,
            "$(" ++ ModuleMakeVarName ++ ".fact_tables.cs)",
            [ObjFileName],
            []),
        MmakeRulesFactTables = [MmakeRuleFactOs, MmakeRuleFactCs]
    ;
        FactTableFileNames = [],
        MmakeVarsFactTables = [],
        FactTableSourceGroups = [],
        MmakeRulesFactTables = []
    ).

%---------------------%

:- pred construct_date_file_deps_rule(globals::in,
    module_name::in, string::in,
    set(module_name)::in, set(module_name)::in, set(module_name)::in,
    set(module_name)::in, string::in, string::in, string::in,
    set(foreign_include_file_info)::in, string::in, string::in, string::in,
    list(mmake_file_name_group)::in,
    mmake_entry::out,
    module_file_name_cache::in, module_file_name_cache::out,
    io::di, io::uo) is det.

construct_date_file_deps_rule(Globals, ModuleName, SourceFileName,
        Ancestors, LongDeps, ShortDeps, PublicChildren, Int0FileName,
        OptDateFileName, TransOptDateFileName, ForeignIncludeFilesSet,
        CDateFileName, JavaDateFileName, ErrFileName,
        FactTableSourceGroups, MmakeRuleDateFileDeps, !Cache, !IO) :-
    % For the reason for why there is no mention of a date file for C# here,
    % see the comment at the top of generate_d_file.
    TargetGroup = mmake_file_name_group("dates_and_err",
        one_or_more(OptDateFileName,
            [TransOptDateFileName, ErrFileName,
            CDateFileName, JavaDateFileName])),
    TargetGroups = one_or_more(TargetGroup, []),

    SourceFileNameGroup = [make_singleton_file_name_group(SourceFileName)],
    % If the module contains nested submodules, then the `.int0' file
    % must first be built.
    ( if set.is_empty(PublicChildren) then
        Int0FileNameGroups = []
    else
        Int0FileNameGroups = [make_singleton_file_name_group(Int0FileName)]
    ),
    make_module_file_name_group_with_suffix(Globals,
        "ancestors",
        ext_other(other_ext(".int0")), newext_int(ext_int_int0),
        Ancestors, AncestorSourceGroups, !Cache, !IO),
    make_module_file_name_group_with_suffix(Globals,
        "long deps",
        ext_other(other_ext(".int")), newext_int(ext_int_int1),
        LongDeps, LongDepsSourceGroups, !Cache, !IO),
    make_module_file_name_group_with_suffix(Globals,
        "short deps",
        ext_other(other_ext(".int2")), newext_int(ext_int_int2),
        ShortDeps, ShortDepsSourceGroups, !Cache, !IO),
    make_module_file_name_group_with_suffix(Globals,
        "type_repn self dep",
        ext_other(other_ext(".int")), newext_int(ext_int_int1),
        set.make_singleton_set(ModuleName), TypeRepnSelfDepGroups,
        !Cache, !IO),
    make_module_file_name_group_with_suffix(Globals,
        "type_repn ancestor dep",
        ext_other(other_ext(".int")), newext_int(ext_int_int1),
        get_ancestors_set(ModuleName), TypeRepnAncestorsDepGroups,
        !Cache, !IO),
    ForeignIncludeFiles = set.to_sorted_list(ForeignIncludeFilesSet),
    % This is conservative: a target file for foreign language A
    % does not truly depend on a file included for foreign language B.
    ForeignImportFileNames =
        list.map(foreign_include_file_path_name(SourceFileName),
            ForeignIncludeFiles),
    ForeignImportFileNameGroup =
        make_file_name_group("foreign imports", ForeignImportFileNames),
    SourceGroups = SourceFileNameGroup ++
        Int0FileNameGroups ++ AncestorSourceGroups ++
        LongDepsSourceGroups ++ ShortDepsSourceGroups ++
        TypeRepnSelfDepGroups ++ TypeRepnAncestorsDepGroups ++
        ForeignImportFileNameGroup ++ FactTableSourceGroups,
    MmakeRuleDateFileDeps = mmake_general_rule("date_file_deps",
        mmake_rule_is_not_phony,
        TargetGroups,
        SourceGroups,
        []).

%---------------------%

    % If a module contains nested submodules, then we need to build
    % the nested children before attempting to build the parent module.
    % Build rules that enforce this.
    %
:- pred construct_build_nested_children_first_rule(globals::in,
    module_name::in, maybe_top_module::in, list(mmake_entry)::out,
    module_file_name_cache::in, module_file_name_cache::out,
    io::di, io::uo) is det.

construct_build_nested_children_first_rule(Globals, ModuleName, MaybeTopModule,
        MmakeRulesNestedDeps, !Cache, !IO) :-
    NestedModuleNames = get_nested_children_list_of_top_module(MaybeTopModule),
    (
        NestedModuleNames = [],
        MmakeRulesNestedDeps = []
    ;
        NestedModuleNames = [_ | _],
        % XXX EXT
        NestedOtherExts = [
            {other_ext(".optdate"), newext_opt(ext_opt_date_plain)},
            {other_ext(".trans_opt_date"), newext_opt(ext_opt_date_trans)},
            {other_ext(".c_date"), newext_target_date(ext_target_date_c)},
            {other_ext(".dir/*.$O"),
                newext_mmake_fragment(ext_mf_dir_sl_all_os)},
            {other_ext(".java_date"),
                newext_target_date(ext_target_date_java)}
            ],
        list.map_foldl2(
            gather_nested_deps(Globals, ModuleName, NestedModuleNames),
            NestedOtherExts, MmakeRulesNestedDeps, !Cache, !IO)
    ).

%---------------------%

:- pred construct_intermod_rules(globals::in, module_name::in,
    set(module_name)::in, set(module_name)::in,
    string::in, string::in, string::in, string::in, string::in,
    list(mmake_entry)::out,
    module_file_name_cache::in, module_file_name_cache::out,
    io::di, io::uo) is det.

construct_intermod_rules(Globals, ModuleName, LongDeps, AllDeps,
        ErrFileName, TransOptDateFileName, CDateFileName, JavaDateFileName,
        ObjFileName, MmakeRulesIntermod, !Cache, !IO) :-
    % XXX Note that currently, due to a design problem, handle_option.m
    % *always* sets use_opt_files to no.
    globals.lookup_bool_option(Globals, use_opt_files, UseOptFiles),
    globals.lookup_bool_option(Globals, intermodule_optimization,
        Intermod),
    globals.lookup_accumulating_option(Globals, intermod_directories,
        IntermodDirs),

    % If intermodule_optimization is enabled, then all the .mh files
    % must exist, because it is possible that the .c file imports them
    % directly or indirectly.
    (
        Intermod = yes,
        make_module_file_names_with_suffix(Globals,
            ext_other(other_ext(".mh")), newext_mh(ext_mh_mh),
            set.to_sorted_list(AllDeps), AllDepsFileNames, !Cache, !IO),
        MmakeRuleMhDeps = mmake_simple_rule("machine_dependent_header_deps",
            mmake_rule_is_not_phony,
            ObjFileName,
            AllDepsFileNames,
            []),
        MmakeRulesMhDeps = [MmakeRuleMhDeps]
    ;
        Intermod = no,
        MmakeRulesMhDeps = []
    ),
    ( if
        ( Intermod = yes
        ; UseOptFiles = yes
        )
    then
        Targets = one_or_more(TransOptDateFileName,
            [ErrFileName, CDateFileName, JavaDateFileName]),

        % The target (e.g. C) file only depends on the .opt files from the
        % current directory, so that inter-module optimization works when
        % the .opt files for the library are unavailable. This is only
        % necessary because make doesn't allow conditional dependencies.
        % The dependency on the current module's .opt file is to make sure
        % the module gets type-checked without having the definitions
        % of abstract types from other modules.
        %
        % XXX The code here doesn't correctly handle dependencies
        % on `.int' and `.int2' files needed by the `.opt' files.
        globals.lookup_bool_option(Globals, transitive_optimization, TransOpt),
        globals.lookup_bool_option(Globals, use_trans_opt_files,
            UseTransOpt),

        bool.not(UseTransOpt, BuildOptFiles),
        BaseDeps = [ModuleName | set.to_sorted_list(LongDeps)],
        ( if
            ( TransOpt = yes
            ; UseTransOpt = yes
            )
        then
            get_both_opt_deps(Globals, BuildOptFiles, IntermodDirs,
                BaseDeps, OptDeps, TransOptDeps1, !Cache, !IO),
            MaybeTransOptDeps1 = yes(TransOptDeps1)
        else
            get_opt_deps(Globals, BuildOptFiles, IntermodDirs,
                other_ext(".opt"), newext_opt(ext_opt_plain),
                BaseDeps, OptDeps, !IO),
            MaybeTransOptDeps1 = no
        ),

        OptInt0Deps = set.union_list(list.map(get_ancestors_set, OptDeps)),
        make_module_file_names_with_suffix(Globals,
            ext_other(other_ext(".opt")), newext_opt(ext_opt_plain),
            OptDeps, OptDepsFileNames, !Cache, !IO),
        make_module_file_names_with_suffix(Globals,
            ext_other(other_ext(".int0")), newext_int(ext_int_int0),
            set.to_sorted_list(OptInt0Deps), OptInt0DepsFileNames, !Cache, !IO),
        MmakeRuleDateOptInt0Deps = mmake_flat_rule("dates_on_opts_and_int0s",
            mmake_rule_is_not_phony,
            Targets,
            OptDepsFileNames ++ OptInt0DepsFileNames,
            []),

        (
            MaybeTransOptDeps1 = yes(TransOptDeps2),
            ErrDateTargets = one_or_more(ErrFileName,
                [CDateFileName, JavaDateFileName]),
            make_module_file_names_with_suffix(Globals,
                ext_other(other_ext(".trans_opt")), newext_opt(ext_opt_trans),
                TransOptDeps2, TransOptDepsOptFileNames, !Cache, !IO),
            MmakeRuleTransOptOpts = mmake_flat_rule("dates_on_trans_opts",
                mmake_rule_is_not_phony,
                ErrDateTargets,
                TransOptDepsOptFileNames,
                []),
            MmakeRulesIntermod = MmakeRulesMhDeps ++
                [MmakeRuleDateOptInt0Deps, MmakeRuleTransOptOpts]
        ;
            MaybeTransOptDeps1 = no,
            MmakeRulesIntermod = MmakeRulesMhDeps ++ [MmakeRuleDateOptInt0Deps]
        )
    else
        MmakeRulesIntermod = MmakeRulesMhDeps
    ).

%---------------------%

:- pred construct_c_header_rules(globals::in, module_name::in,
    set(module_name)::in, string::in, string::in, string::in,
    list(mmake_entry)::out,
    module_file_name_cache::in, module_file_name_cache::out,
    io::di, io::uo) is det.

construct_c_header_rules(Globals, ModuleName, AllDeps,
        CFileName, ObjFileName, PicObjFileName, MmakeRulesCHeaders,
        !Cache, !IO) :-
    globals.lookup_bool_option(Globals, highlevel_code, HighLevelCode),
    globals.get_target(Globals, CompilationTarget),
    ( if
        HighLevelCode = yes,
        CompilationTarget = target_c
    then
        % For --high-level-code with --target c, we need to make sure that
        % we generate the header files for imported modules before compiling
        % the C files, since the generated C files #include those header files.
        Targets = one_or_more(PicObjFileName, [ObjFileName]),
        make_module_file_names_with_suffix(Globals,
            ext_other(other_ext(".mih")), newext_mih(ext_mih_mih),
            set.to_sorted_list(AllDeps), AllDepsFileNames, !Cache, !IO),
        MmakeRuleObjOnMihs = mmake_flat_rule("objs_on_mihs",
            mmake_rule_is_not_phony,
            Targets,
            AllDepsFileNames,
            []),
        MmakeRulesObjOnMihs = [MmakeRuleObjOnMihs]
    else
        MmakeRulesObjOnMihs = []
    ),

    % We need to tell make how to make the header files. The header files
    % are actually built by the same command that creates the .c file,
    % so we just make them depend on the .c files. This is needed for the
    % --high-level-code rule above, and for the rules introduced for
    % `:- pragma foreign_import_module' declarations. In some grades the header
    % file won't actually be built (e.g. LLDS grades for modules not containing
    % `:- pragma export' declarations), but this rule won't do any harm.
    make_module_file_name(Globals, $pred,
        ext_other(other_ext(".mh")), newext_mh(ext_mh_mh),
        ModuleName, MhHeaderFileName, !Cache, !IO),
    make_module_file_name(Globals, $pred,
        ext_other(other_ext(".mih")), newext_mih(ext_mih_mih),
        ModuleName, MihHeaderFileName, !Cache, !IO),
    MmakeRuleMhMihOnC = mmake_flat_rule("mh_and_mih_on_c",
        mmake_rule_is_not_phony,
        one_or_more(MhHeaderFileName, [MihHeaderFileName]),
        [CFileName],
        []),
    MmakeRulesCHeaders = MmakeRulesObjOnMihs ++ [MmakeRuleMhMihOnC].

%---------------------%

    % The `.module_dep' file is made as a side effect of
    % creating the `.c' or `.java'.
    % XXX What about C#?
    % (See the main comment on generate_d_file above.
    %
:- pred construct_module_dep_fragment(globals::in, module_name::in,
    string::in, mmake_fragment::out,
    module_file_name_cache::in, module_file_name_cache::out,
    io::di, io::uo) is det.

construct_module_dep_fragment(Globals, ModuleName, CFileName,
        MmakeFragmentModuleDep, !Cache, !IO) :-
    make_module_file_name(Globals, $pred,
        ext_other(other_ext(".java")),
        newext_target_java(ext_target_java_java),
        ModuleName, JavaFileName, !Cache, !IO),
    make_module_file_name(Globals, $pred,
        ext_other(make_module_dep_file_extension),
        newext_misc_ngs(ext_misc_ngs_module_dep),
        ModuleName, ModuleDepFileName, !Cache, !IO),
    MmakeFragmentModuleDep = mmf_conditional_entry(
        mmake_cond_grade_has_component("java"),
        mmake_simple_rule("module_dep_on_java",
            mmake_rule_is_not_phony,
            ModuleDepFileName,
            [JavaFileName],
            []),
        mmake_simple_rule("module_dep_on_c",
            mmake_rule_is_not_phony,
            ModuleDepFileName,
            [CFileName],
            [])
    ).

%---------------------%

    % The .date and .date0 files depend on the .int0 files for the parent
    % modules, and the .int3 files for the directly and indirectly imported
    % modules.
    %
    % For nested submodules, the `.date' files for the parent modules
    % also depend on the same things as the `.date' files for this module,
    % since all the `.date' files will get produced by a single mmc command.
    % Similarly for `.date0' files, except these don't depend on the `.int0'
    % files, because when doing the `--make-private-interface' for nested
    % modules, mmc will process the modules in outermost to innermost order
    % so as to produce each `.int0' file before it is needed.
    %
:- pred construct_self_and_parent_date_date0_rules(globals::in,
    string::in, string::in, string::in,
    set(module_name)::in, set(module_name)::in, set(module_name)::in,
    list(mmake_entry)::out,
    module_file_name_cache::in, module_file_name_cache::out,
    io::di, io::uo) is det.

construct_self_and_parent_date_date0_rules(Globals, SourceFileName,
        Date0FileName, DateFileName, Ancestors, LongDeps, ShortDeps,
        MmakeRulesParentDates, !Cache, !IO) :-
    make_module_file_names_with_suffix(Globals,
        ext_other(other_ext(".date")), newext_int(ext_int_date_int12),
        set.to_sorted_list(Ancestors), AncestorDateFileNames, !Cache, !IO),
    make_module_file_names_with_suffix(Globals,
        ext_other(other_ext(".int0")), newext_int(ext_int_int0),
        set.to_sorted_list(Ancestors), AncestorInt0FileNames, !Cache, !IO),
    make_module_file_names_with_suffix(Globals,
        ext_other(other_ext(".int3")), newext_int(ext_int_int3),
        set.to_sorted_list(LongDeps), LongDepInt3FileNames, !Cache, !IO),
    make_module_file_names_with_suffix(Globals,
        ext_other(other_ext(".int3")), newext_int(ext_int_int3),
        set.to_sorted_list(ShortDeps), ShortDepInt3FileNames, !Cache, !IO),

    MmakeRuleParentDates = mmake_general_rule("self_and_parent_date_deps",
        mmake_rule_is_not_phony,
        one_or_more(
            mmake_file_name_group("",
                one_or_more(DateFileName,
                    [Date0FileName | AncestorDateFileNames])),
            []),
        [make_singleton_file_name_group(SourceFileName)] ++
            make_file_name_group("ancestor int0", AncestorInt0FileNames) ++
            make_file_name_group("long dep int3s", LongDepInt3FileNames) ++
            make_file_name_group("short dep int3s", ShortDepInt3FileNames),
        []),
    make_module_file_names_with_suffix(Globals,
        ext_other(other_ext(".date0")), newext_int(ext_int_date_int0),
        set.to_sorted_list(Ancestors), AncestorDate0FileNames, !Cache, !IO),
    MmakeRuleParentDate0s = mmake_general_rule("self_and_parent_date0_deps",
        mmake_rule_is_not_phony,
        one_or_more(
            mmake_file_name_group("date0s",
                one_or_more(Date0FileName, AncestorDate0FileNames)),
            []),
        [make_singleton_file_name_group(SourceFileName)] ++
            make_file_name_group("long dep int3s", LongDepInt3FileNames) ++
            make_file_name_group("short dep int3s", ShortDepInt3FileNames),
        []),
    MmakeRulesParentDates = [MmakeRuleParentDates, MmakeRuleParentDate0s].

%---------------------%

    % Handle dependencies introduced by
    % `:- pragma foreign_import_module' declarations.
    %
:- pred construct_foreign_import_rules(globals::in, aug_compilation_unit::in,
    maybe_intermod_deps::in, module_name::in, string::in, string::in,
    list(mmake_entry)::out,
    module_file_name_cache::in, module_file_name_cache::out,
    io::di, io::uo) is det.

construct_foreign_import_rules(Globals, AugCompUnit, IntermodDeps,
        SourceFileModuleName, ObjFileName, PicObjFileName,
        MmakeRulesForeignImports, !Cache, !IO) :-
    AugCompUnit = aug_compilation_unit(ParseTreeModuleSrc,
        AncestorIntSpecs, DirectInt1Specs, IndirectInt2Specs,
        PlainOpts, _TransOpts, IntForOptSpecs, _TypeRepnSpecs,
        _ModuleVersionNumber),
    ModuleName = ParseTreeModuleSrc ^ ptms_module_name,
    (
        IntermodDeps = intermod_deps(_, _, _, ForeignImportedModuleNamesSet, _)
    ;
        IntermodDeps = no_intermod_deps,
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
            ), FIMSpecs, ForeignImportedModuleNamesSet)
    ),

    ForeignImportedModuleNames =
        set.to_sorted_list(ForeignImportedModuleNamesSet),
    (
        ForeignImportedModuleNames = [],
        MmakeRulesForeignImports = []
    ;
        ForeignImportedModuleNames = [_ | _],
        globals.get_target(Globals, Target),
        (
            Target = target_c,
            % NOTE: for C the possible targets might be a .o file _or_ a
            % .pic_o file. We need to include dependencies for the latter
            % otherwise invoking mmake with a <module>.pic_o target will break.
            ForeignImportTargets = one_or_more(ObjFileName, [PicObjFileName]),
            ForeignImportOtherExt = other_ext(".mh"),
            ForeignImportNewExt = newext_mh(ext_mh_mh),
            gather_foreign_import_deps(Globals,
                ForeignImportOtherExt, ForeignImportNewExt,
                ForeignImportTargets, ForeignImportedModuleNames,
                MmakeRuleForeignImports, !Cache, !IO),
            MmakeRulesForeignImports = [MmakeRuleForeignImports]
        ;
            Target = target_java,
            make_module_file_name(Globals, $pred,
                ext_other(other_ext(".class")),
                newext_target_java(ext_target_java_class),
                ModuleName, ClassFileName, !Cache, !IO),
            ForeignImportTargets = one_or_more(ClassFileName, []),
            ForeignImportOtherExt = other_ext(".java"),
            ForeignImportNewExt = newext_target_java(ext_target_java_java),
            gather_foreign_import_deps(Globals,
                ForeignImportOtherExt, ForeignImportNewExt,
                ForeignImportTargets, ForeignImportedModuleNames,
                MmakeRuleForeignImports, !Cache, !IO),
            MmakeRulesForeignImports = [MmakeRuleForeignImports]
        ;
            Target = target_csharp,
            % XXX We don't implement mmake rules for C#.
            MmakeRulesForeignImports = []
        )
    ).

%---------------------%

    % We add some extra dependencies to the generated `.d' files, so that
    % local `.int', `.opt', etc. files shadow the installed versions properly
    % (e.g. for when you are trying to build a new version of an installed
    % library). This saves the user from having to add these explicitly
    % if they have multiple libraries installed in the same installation
    % hierarchy which aren't independent (e.g. one uses another). These extra
    % dependencies are necessary due to the way the combination of search paths
    % and pattern rules works in Make.
    %
:- pred construct_install_shadow_rules(globals::in, module_name::in,
    string::in, string::in, string::in, string::in, string::in, string::in,
    list(mmake_entry)::out,
    module_file_name_cache::in, module_file_name_cache::out,
    io::di, io::uo) is det.

construct_install_shadow_rules(Globals, ModuleName,
        Int0FileName, Date0FileName, DateFileName, Date3FileName,
        OptDateFileName, TransOptDateFileName,
        MmakeRulesInstallShadows, !Cache, !IO) :-
    make_module_file_name(Globals, $pred,
        ext_other(other_ext(".int")), newext_int(ext_int_int1),
        ModuleName, IntFileName, !Cache, !IO),
    make_module_file_name(Globals, $pred,
        ext_other(other_ext(".int2")), newext_int(ext_int_int2),
        ModuleName, Int2FileName, !Cache, !IO),
    make_module_file_name(Globals, $pred,
        ext_other(other_ext(".int3")), newext_int(ext_int_int3),
        ModuleName, Int3FileName, !Cache, !IO),
    make_module_file_name(Globals, $pred,
        ext_other(other_ext(".opt")), newext_opt(ext_opt_plain),
        ModuleName, OptFileName, !Cache, !IO),
    make_module_file_name(Globals, $pred,
        ext_other(other_ext(".trans_opt")), newext_opt(ext_opt_trans),
        ModuleName, TransOptFileName, !Cache, !IO),

    MmakeRulesInstallShadows = [
        mmake_simple_rule("int0_on_date0",
            mmake_rule_is_not_phony,
            Int0FileName, [Date0FileName], [silent_noop_action]),
        mmake_simple_rule("int_on_date",
            mmake_rule_is_not_phony,
            IntFileName, [DateFileName], [silent_noop_action]),
        mmake_simple_rule("int2_on_date",
            mmake_rule_is_not_phony,
            Int2FileName, [DateFileName], [silent_noop_action]),
        mmake_simple_rule("int3_on_date3",
            mmake_rule_is_not_phony,
            Int3FileName, [Date3FileName], [silent_noop_action]),
        mmake_simple_rule("opt_on_opt_date",
            mmake_rule_is_not_phony,
            OptFileName, [OptDateFileName], [silent_noop_action]),
        mmake_simple_rule("trans_opt_on_trans_opt_date",
            mmake_rule_is_not_phony,
            TransOptFileName, [TransOptDateFileName], [silent_noop_action])
    ].

%---------------------%

:- pred construct_subdir_short_rules(globals::in, module_name::in,
    list(mmake_entry)::out,
    module_file_name_cache::in, module_file_name_cache::out,
    io::di, io::uo) is det.

construct_subdir_short_rules(Globals, ModuleName,
        MmakeRulesSubDirShorthand, !Cache, !IO) :-
    globals.lookup_bool_option(Globals, use_subdirs, UseSubdirs),
    (
        UseSubdirs = yes,
        SubDirShorthandOtherExts =
            [{other_ext(".c"),      newext_target_c_cs(ext_target_c)},
            {other_ext(".$O"),      newext_target_obj(ext_obj_dollar_o)},
            {other_ext(".pic_o"),   newext_target_obj(ext_obj_pic_o)},
            {other_ext(".java"),    newext_target_java(ext_target_java_java)},
            {other_ext(".class"),   newext_target_java(ext_target_java_class)},
            {other_ext(".dll"),     newext_lib_gs(ext_lib_gs_dll)}],
        list.map_foldl2(
            construct_subdirs_shorthand_rule(Globals, ModuleName),
            SubDirShorthandOtherExts, MmakeRulesSubDirShorthand, !Cache, !IO)
    ;
        UseSubdirs = no,
        MmakeRulesSubDirShorthand = []
    ).

%---------------------%

:- pred construct_any_needed_pattern_rules(bool::in,
    module_name::in, module_name::in, string::in,
    string::in, string::in, string::in,
    string::in, string::in, string::in, string::in,
    list(mmake_entry)::out) is det.

construct_any_needed_pattern_rules(HaveMap,
        ModuleName, SourceFileModuleName, SourceFileName,
        Date0FileName, DateFileName, Date3FileName,
        OptDateFileName, TransOptDateFileName, CDateFileName, JavaDateFileName,
        MmakeRulesPatterns) :-
    % If we can pass the module name rather than the file name, then do so.
    % `--smart-recompilation' doesn't work if the file name is passed
    % and the module name doesn't match the file name.
    (
        HaveMap = yes,
        module_name_to_file_name_stem(SourceFileModuleName, ModuleArg)
    ;
        HaveMap = no,
        ModuleArg = SourceFileName
    ),
    ( if SourceFileName = default_source_file_name(ModuleName) then
        MmakeRulesPatterns = []
    else
        % The pattern rules in Mmake.rules won't work, since the source file
        % name doesn't match the expected source file name for this module
        % name. This can occur due to just the use of different source file
        % names, or it can be due to the use of nested modules. So we need
        % to output hard-coded rules in this case.
        %
        % The rules output below won't work in the case of nested modules
        % with parallel makes, because it will end up invoking the same command
        % twice (since it produces two output files) at the same time.
        %
        % Any changes here will require corresponding changes to
        % scripts/Mmake.rules. See that file for documentation on these rules.

        MmakeRulesPatterns = [
            mmake_simple_rule("date0_on_src",
                mmake_rule_is_not_phony,
                Date0FileName, [SourceFileName],
                ["$(MCPI) $(ALL_GRADEFLAGS) $(ALL_MCPIFLAGS) " ++ ModuleArg]),
            mmake_simple_rule("date_on_src",
                mmake_rule_is_not_phony,
                DateFileName, [SourceFileName],
                ["$(MCI) $(ALL_GRADEFLAGS) $(ALL_MCIFLAGS) " ++ ModuleArg]),
            mmake_simple_rule("date3_on_src",
                mmake_rule_is_not_phony,
                Date3FileName, [SourceFileName],
                ["$(MCSI) $(ALL_GRADEFLAGS) $(ALL_MCSIFLAGS) " ++ ModuleArg]),
            mmake_simple_rule("opt_date_on_src",
                mmake_rule_is_not_phony,
                OptDateFileName, [SourceFileName],
                ["$(MCOI) $(ALL_GRADEFLAGS) $(ALL_MCOIFLAGS) " ++ ModuleArg]),
            mmake_simple_rule("trans_opt_date_on_src",
                mmake_rule_is_not_phony,
                TransOptDateFileName, [SourceFileName],
                ["$(MCTOI) $(ALL_GRADEFLAGS) $(ALL_MCTOIFLAGS) " ++
                    ModuleArg]),
            mmake_simple_rule("c_date_on_src",
                mmake_rule_is_not_phony,
                CDateFileName, [SourceFileName],
                ["$(MCG) $(ALL_GRADEFLAGS) $(ALL_MCGFLAGS) " ++ ModuleArg ++
                    " $(ERR_REDIRECT)"]),
            mmake_simple_rule("java_date_on_src",
                mmake_rule_is_not_phony,
                JavaDateFileName, [SourceFileName],
                ["$(MCG) $(ALL_GRADEFLAGS) $(ALL_MCGFLAGS) --java-only " ++
                    ModuleArg ++ " $(ERR_REDIRECT)"])
        ]
    ).

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

:- pred gather_nested_deps(globals::in, module_name::in, list(module_name)::in,
    {other_ext, newext}::in, mmake_entry::out,
    module_file_name_cache::in, module_file_name_cache::out,
    io::di, io::uo) is det.

gather_nested_deps(Globals, ModuleName, NestedDeps, {OtherExt, NewExt},
        MmakeRule, !Cache, !IO) :-
    make_module_file_name(Globals, $pred, ext_other(OtherExt), NewExt,
        ModuleName, ModuleExtName, !Cache, !IO),
    make_module_file_names_with_suffix(Globals, ext_other(OtherExt), NewExt,
        NestedDeps, NestedDepsFileNames, !Cache, !IO),
    ExtStr = extension_to_string(Globals, ext_other(OtherExt), NewExt),
    MmakeRule = mmake_simple_rule("nested_deps_for_" ++ ExtStr,
        mmake_rule_is_not_phony,
        ModuleExtName,
        NestedDepsFileNames,
        []).

:- pred gather_foreign_import_deps(globals::in, other_ext::in, newext::in,
    one_or_more(string)::in, list(module_name)::in, mmake_entry::out,
    module_file_name_cache::in, module_file_name_cache::out,
    io::di, io::uo) is det.

gather_foreign_import_deps(Globals, ForeignImportOtherExt, ForeignImportNewExt,
        ForeignImportTargets, ForeignImportedModuleNames, MmakeRule,
        !Cache, !IO) :-
    make_module_file_names_with_suffix(Globals,
        ext_other(ForeignImportOtherExt), ForeignImportNewExt,
        ForeignImportedModuleNames, ForeignImportedFileNames, !Cache, !IO),
    ForeignImportExtStr = extension_to_string(Globals,
        ext_other(ForeignImportOtherExt), ForeignImportNewExt),
    RuleName = "foreign_deps_for_" ++
        string.remove_prefix_if_present(".", ForeignImportExtStr),
    MmakeRule = mmake_flat_rule(RuleName,
        mmake_rule_is_not_phony,
        ForeignImportTargets,
        ForeignImportedFileNames,
        []).

%---------------------------------------------------------------------------%

:- pred make_module_file_name(globals::in, string::in, ext::in, newext::in,
    module_name::in, file_name::out,
    module_file_name_cache::in, module_file_name_cache::out,
    io::di, io::uo) is det.

make_module_file_name(Globals, From, Ext, NewExt, ModuleName, FileName,
        !Cache, !IO) :-
    (
        Ext = ext_src,
        % No point caching these.
        module_name_to_file_name(Globals, From, do_not_create_dirs,
            Ext, NewExt, ModuleName, FileName, !IO)
    ;
        Ext = ext_other(OtherExt),
        ModuleNameExt = module_name_and_ext(ModuleName, OtherExt),
        ( if map.search(!.Cache, ModuleNameExt, FileName0) then
            FileName = FileName0
        else
            % The result of module_name_to_file_name is cached to save on
            % temporary string constructions.
            module_name_to_file_name(Globals, From, do_not_create_dirs,
                Ext, NewExt, ModuleName, FileName, !IO),
            map.det_insert(ModuleNameExt, FileName, !Cache)
        )
    ).

:- pred make_module_file_names_with_suffix(globals::in, ext::in, newext::in,
    list(module_name)::in, list(mmake_file_name)::out,
    module_file_name_cache::in, module_file_name_cache::out,
    io::di, io::uo) is det.

make_module_file_names_with_suffix(Globals, Ext, NewExt, Modules, FileNames,
        !Cache, !IO) :-
    list.map_foldl2(make_module_file_name(Globals, $pred, Ext, NewExt),
        Modules, FileNames, !Cache, !IO).

:- pred make_module_file_name_group_with_suffix(globals::in, string::in,
    ext::in, newext::in, set(module_name)::in, list(mmake_file_name_group)::out,
    module_file_name_cache::in, module_file_name_cache::out,
    io::di, io::uo) is det.

make_module_file_name_group_with_suffix(Globals, GroupName, Ext, NewExt,
        Modules, Groups, !Cache, !IO) :-
    list.map_foldl2(make_module_file_name(Globals, $pred, Ext, NewExt),
        set.to_sorted_list(Modules), FileNames, !Cache, !IO),
    Groups = make_file_name_group(GroupName, FileNames).

%---------------------------------------------------------------------------%

:- func foreign_include_file_path_name(file_name, foreign_include_file_info)
    = string.

foreign_include_file_path_name(SourceFileName, IncludeFile) = IncludePath :-
    IncludeFile = foreign_include_file_info(_Lang, IncludeFileName),
    make_include_file_path(SourceFileName, IncludeFileName, IncludePath).

:- pred get_fact_table_dependencies(globals::in, other_ext::in, newext::in,
    list(file_name)::in, list(string)::out, io::di, io::uo) is det.

get_fact_table_dependencies(_, _, _, [], [], !IO).
get_fact_table_dependencies(Globals, OtherExt, NewExt,
        [ExtraLink | ExtraLinks], [FileName | FileNames], !IO) :-
    fact_table_file_name(Globals, $pred, do_not_create_dirs, OtherExt, NewExt,
        ExtraLink, FileName, !IO),
    get_fact_table_dependencies(Globals, OtherExt, NewExt,
        ExtraLinks, FileNames, !IO).

    % With `--use-subdirs', allow users to type `mmake module.c'
    % rather than `mmake Mercury/cs/module.c'.
    %
:- pred construct_subdirs_shorthand_rule(globals::in, module_name::in,
    {other_ext, newext}::in, mmake_entry::out,
    module_file_name_cache::in, module_file_name_cache::out,
    io::di, io::uo) is det.

construct_subdirs_shorthand_rule(Globals, ModuleName, {OtherExt, NewExt},
        MmakeRule, !Cache, !IO) :-
    module_name_to_file_name_stem(ModuleName, ModuleStr),
    make_module_file_name(Globals, $pred, ext_other(OtherExt), NewExt,
        ModuleName, Target, !Cache, !IO),
    ExtStr = extension_to_string(Globals, ext_other(OtherExt), NewExt),
    ShorthandTarget = ModuleStr ++ ExtStr,
    MmakeRule = mmake_simple_rule("subdir_shorthand_for_" ++ ExtStr,
        mmake_rule_is_phony, ShorthandTarget, [Target], []).

%---------------------------------------------------------------------------%

generate_dependencies_write_d_files(Globals, Deps,
        IntDepsGraph, ImpDepsGraph, IndirectDepsGraph, IndirectOptDepsGraph,
        TransOptDepsGraph, TransOptOrder, DepsMap, !IO) :-
    map.init(Cache0),
    generate_dependencies_write_d_files_loop(Globals, Deps,
        IntDepsGraph, ImpDepsGraph, IndirectDepsGraph, IndirectOptDepsGraph,
        TransOptDepsGraph, TransOptOrder, DepsMap, Cache0, _Cache, !IO).

:- pred generate_dependencies_write_d_files_loop(globals::in, list(deps)::in,
    deps_graph::in, deps_graph::in, deps_graph::in, deps_graph::in,
    deps_graph::in, list(module_name)::in, deps_map::in,
    module_file_name_cache::in, module_file_name_cache::out,
    io::di, io::uo) is det.

generate_dependencies_write_d_files_loop(_, [], _, _, _, _, _, _, _,
        !Cache, !IO).
generate_dependencies_write_d_files_loop(Globals, [Dep | Deps],
        IntDepsGraph, ImpDepsGraph, IndirectDepsGraph, IndirectOptDepsGraph,
        TransOptDepsGraph, TransOptOrder, DepsMap, !Cache, !IO) :-
    generate_dependencies_write_d_file(Globals, Dep,
        IntDepsGraph, ImpDepsGraph, IndirectDepsGraph, IndirectOptDepsGraph,
        TransOptDepsGraph, TransOptOrder, DepsMap, !Cache, !IO),
    generate_dependencies_write_d_files_loop(Globals, Deps,
        IntDepsGraph, ImpDepsGraph, IndirectDepsGraph, IndirectOptDepsGraph,
        TransOptDepsGraph, TransOptOrder, DepsMap, !Cache, !IO).

:- pred generate_dependencies_write_d_file(globals::in, deps::in,
    deps_graph::in, deps_graph::in, deps_graph::in, deps_graph::in,
    deps_graph::in, list(module_name)::in, deps_map::in,
    module_file_name_cache::in, module_file_name_cache::out,
    io::di, io::uo) is det.

generate_dependencies_write_d_file(Globals, Dep,
        IntDepsGraph, ImpDepsGraph, IndirectDepsGraph, IndirectOptDepsGraph,
        TransOptDepsGraph, FullTransOptOrder, _DepsMap, !Cache, !IO) :-
    % XXX The fact that _DepsMap is unused here may be a bug.
    Dep = deps(_, BurdenedModule),
    BurdenedModule = burdened_module(Baggage, ParseTreeModuleSrc),

    % Look up the interface/implementation/indirect dependencies
    % for this module from the respective dependency graphs,
    % and save them in the module_and_imports structure.

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
        IntDeps = IndirectOptDeps,
        ImpDeps = IndirectOptDeps,
        IndirectDeps = IndirectOptDeps
    ;
        Intermod = no,
        get_dependencies_from_graph(IntDepsGraph, ModuleName, IntDeps),
        get_dependencies_from_graph(ImpDepsGraph, ModuleName, ImpDeps),
        get_dependencies_from_graph(IndirectDepsGraph, ModuleName,
            IndirectDeps)
    ),

    get_dependencies_from_graph(TransOptDepsGraph, ModuleName, TransOptDeps0),
    set.delete(ModuleName, TransOptDeps0, TransOptDeps),

    IntermodDeps = intermod_deps(IntDeps, ImpDeps, IndirectDeps,
        IndirectOptDeps, TransOptDeps),

    % Compute the maximum allowable trans-opt dependencies for this module.
    % To avoid the possibility of cycles, each module is only allowed to depend
    % on modules that occur after it in the FullTransOptOrder.

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

    % Note that even if a fatal error occurred for one of the files
    % that the current Module depends on, a .d file is still produced,
    % even though it probably contains incorrect information.
    ModuleErrors = Baggage ^ mb_errors,
    FatalErrors = ModuleErrors ^ rm_fatal_errors,
    ( if set.is_empty(FatalErrors) then
        init_aug_compilation_unit(ParseTreeModuleSrc, AugCompUnit),
        BurdenedAugCompUnit = burdened_aug_comp_unit(Baggage, AugCompUnit),
        write_dependency_file_fn_cache(Globals, BurdenedAugCompUnit,
            IntermodDeps, IndirectOptDeps, MaybeInclTransOptRule, !Cache, !IO)
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

generate_dependencies_write_dv_file(Globals, SourceFileName, ModuleName,
        DepsMap, !IO) :-
    globals.lookup_bool_option(Globals, verbose, Verbose),
    module_name_to_file_name(Globals, $pred, do_create_dirs,
        ext_other(other_ext(".dv")), newext_mmake_fragment(ext_mf_dv),
        ModuleName, DvFileName, !IO),
    get_progress_output_stream(Globals, ModuleName, ProgressStream, !IO),
    string.format("%% Creating auto-dependency file `%s'...\n",
        [s(DvFileName)], CreatingMsg),
    maybe_write_string(ProgressStream, Verbose, CreatingMsg, !IO),
    io.open_output(DvFileName, DvResult, !IO),
    (
        DvResult = ok(DvStream),
        map.init(Cache0),
        generate_dv_file(Globals, SourceFileName, ModuleName, DepsMap,
            MmakeFile, Cache0, _Cache, !IO),
        write_mmakefile(DvStream, MmakeFile, !IO),
        io.close_output(DvStream, !IO),
        maybe_write_string(ProgressStream, Verbose, "% done.\n", !IO)
    ;
        DvResult = error(IOError),
        maybe_write_string(ProgressStream, Verbose, " failed.\n", !IO),
        maybe_flush_output(ProgressStream, Verbose, !IO),
        get_error_output_stream(Globals, ModuleName, ErrorStream, !IO),
        io.error_message(IOError, IOErrorMessage),
        string.format("error opening file `%s' for output: %s",
            [s(DvFileName), s(IOErrorMessage)], DepMessage),
        report_error(ErrorStream, DepMessage, !IO)
    ).

%---------------------------------------------------------------------------%

:- pred generate_dv_file(globals::in, file_name::in, module_name::in,
    deps_map::in, mmakefile::out,
    module_file_name_cache::in, module_file_name_cache::out,
    io::di, io::uo) is det.

generate_dv_file(Globals, SourceFileName, ModuleName, DepsMap,
        MmakeFile, !Cache, !IO) :-
    ModuleNameString = sym_name_to_string(ModuleName),
    library.version(Version, FullArch),
    MmakeStartComment = mmake_start_comment("dependency variables",
        ModuleNameString, SourceFileName, Version, FullArch),

    map.keys(DepsMap, Modules0),
    select_ok_modules(DepsMap, Modules0, Modules1),
    list.sort(compare_module_names, Modules1, Modules),

    module_name_to_make_var_name(ModuleName, ModuleMakeVarName),
    list.map(get_source_file(DepsMap), Modules, SourceFiles0),
    list.sort_and_remove_dups(SourceFiles0, SourceFiles),

    MmakeVarModuleMs = mmake_var_defn_list(ModuleMakeVarName ++ ".ms",
        list.map(add_suffix(".m"), SourceFiles)),

    MmakeVarModuleErrs = mmake_var_defn_list(ModuleMakeVarName ++ ".errs",
        list.map(add_suffix(".err"), SourceFiles)),

    MmakeVarModuleDepErrs = mmake_var_defn_list(
        ModuleMakeVarName ++ ".dep_errs",
        list.map(add_suffix(".dep_err"), SourceFiles)),

    make_module_file_names_with_suffix(Globals,
        ext_other(other_ext("")), newext_exec_gs(ext_exec_gs_noext),
        Modules, ModulesSourceFileNames, !Cache, !IO),
    MmakeVarModuleMods = mmake_var_defn_list(ModuleMakeVarName ++ ".mods",
        ModulesSourceFileNames),

    % The modules for which we need to generate .int0 files.
    ModulesWithSubModules = list.filter(
        ( pred(Module::in) is semidet :-
            map.lookup(DepsMap, Module, deps(_, BurdenedModule)),
            ParseTreeModuleSrc = BurdenedModule ^ bm_module,
            IncludeMap = ParseTreeModuleSrc ^ ptms_include_map,
            not map.is_empty(IncludeMap)
        ), Modules),

    make_module_file_names_with_suffix(Globals,
        ext_other(other_ext("")), newext_exec_gs(ext_exec_gs_noext),
        ModulesWithSubModules, ModulesWithSubModulesSourceFileNames,
        !Cache, !IO),
    MmakeVarModuleParentMods = mmake_var_defn_list(
        ModuleMakeVarName ++ ".parent_mods",
        ModulesWithSubModulesSourceFileNames),

    globals.get_target(Globals, Target),
    (
        ( Target = target_c
        ; Target = target_csharp
        ; Target = target_java
        ),
        ForeignModulesAndExts = []
    ),
    ForeignModules = list.map((func({A, _B, _C}) = A), ForeignModulesAndExts),

    make_module_file_names_with_suffix(Globals,
        ext_other(other_ext("")), newext_exec_gs(ext_exec_gs_noext),
        ForeignModules, ForeignModulesFileNames, !Cache, !IO),
    MmakeVarForeignModules =
        mmake_var_defn_list(ModuleMakeVarName ++ ".foreign",
            ForeignModulesFileNames),

    MakeFileName =
        ( pred({M, E, NE}::in, F::out, IO0::di, IO::uo) is det :-
            module_name_to_file_name(Globals, $pred, do_create_dirs, E, NE,
                M, F0, IO0, IO),
            F = "$(os_subdir)" ++ F0
        ),
    list.map_foldl(MakeFileName, ForeignModulesAndExts, ForeignFileNames, !IO),

    % .foreign_cs are the source files which have had foreign code placed
    % in them.
    % XXX This rule looks wrong: why are we looking for (a) stuff with an
    % unknown suffix in (b) the os_subdir, when we (c) refer to it
    % using a make variable whose name ends in "_cs"?
    % Of course, since ForeignModulesAndExts is always zero with our current
    % set of target languages, this does not matter.
    MmakeVarForeignFileNames =
        mmake_var_defn_list(ModuleMakeVarName ++ ".foreign_cs",
            ForeignFileNames),

    % The dlls that contain the foreign_code.
    MmakeVarForeignDlls = mmake_var_defn(ModuleMakeVarName ++ ".foreign_dlls",
        string.format("$(%s.foreign:%%=$(dlls_subdir)%%.dll)",
            [s(ModuleMakeVarName)])),
    MmakeVarInitCs = mmake_var_defn(ModuleMakeVarName ++ ".init_cs",
        string.format("$(%s.mods:%%=$(cs_subdir)%%.c)",
            [s(ModuleMakeVarName)])),
    MmakeVarAllCs = mmake_var_defn(ModuleMakeVarName ++ ".all_cs",
        string.format("$(%s.mods:%%=$(cs_subdir)%%.c)",
            [s(ModuleMakeVarName)])),

    get_fact_table_file_names(DepsMap, Modules, FactTableFileNames),
    % XXX EXT
    % We should just be able to append ".c", ".$O" and the pic extension
    % to each string in FactTableFileNames.
    get_fact_table_dependencies(Globals,
        other_ext(".c"), newext_target_c_cs(ext_target_c),
        FactTableFileNames, FactTableFileNamesC, !IO),
    get_fact_table_dependencies(Globals,
        other_ext(".$O"), newext_target_obj(ext_obj_dollar_o),
        FactTableFileNames, FactTableFileNamesOs, !IO),
    get_fact_table_dependencies(Globals,
        other_ext(".$(EXT_FOR_PIC_OBJECTS)"),
        newext_target_obj(ext_obj_dollar_efpo),
        FactTableFileNames, FactTableFileNamesPicOs, !IO),

    MmakeVarCs = mmake_var_defn_list(ModuleMakeVarName ++ ".cs",
        ["$(" ++ ModuleMakeVarName ++ ".init_cs)" | FactTableFileNamesC]),
    MmakeVarDlls = mmake_var_defn(ModuleMakeVarName ++ ".dlls",
        string.format("$(%s.mods:%%=$(dlls_subdir)%%.dll)",
            [s(ModuleMakeVarName)])),
    MmakeVarAllOs = mmake_var_defn_list(ModuleMakeVarName ++ ".all_os",
        [string.format("$(%s.mods:%%=$(os_subdir)%%.$O)",
            [s(ModuleMakeVarName)]) |
        FactTableFileNamesOs]),
    MmakeVarAllPicOs = mmake_var_defn_list(ModuleMakeVarName ++ ".all_pic_os",
        [string.format("$(%s.mods:%%=$(os_subdir)%%.$(EXT_FOR_PIC_OBJECTS))",
            [s(ModuleMakeVarName)]) |
        FactTableFileNamesPicOs]),
    MmakeVarOs = mmake_var_defn(ModuleMakeVarName ++ ".os",
        string.format("$(%s.all_os)", [s(ModuleMakeVarName)])),
    MmakeVarPicOs = mmake_var_defn(ModuleMakeVarName ++ ".pic_os",
        string.format("$(%s.all_pic_os)", [s(ModuleMakeVarName)])),
    MmakeVarUseds = mmake_var_defn(ModuleMakeVarName ++ ".useds",
        string.format("$(%s.mods:%%=$(useds_subdir)%%.used)",
            [s(ModuleMakeVarName)])),
    MmakeVarJavas = mmake_var_defn(ModuleMakeVarName ++ ".javas",
        string.format("$(%s.mods:%%=$(javas_subdir)%%.java)",
            [s(ModuleMakeVarName)])),
    MmakeVarAllJavas = mmake_var_defn(ModuleMakeVarName ++ ".all_javas",
        string.format("$(%s.mods:%%=$(javas_subdir)%%.java)",
            [s(ModuleMakeVarName)])),

    % The Java compiler creates a .class file for each class within the
    % original .java file. The filenames of all these can be matched with
    % `module\$*.class', hence the "\\$$*.class" below.
    % If no such files exist, Make will use the pattern verbatim,
    % so we enclose the pattern in a `wildcard' function to prevent this.
    % Evaluating the .classes variable can be slow so we make it conditional
    % on the grade.
    MmakeVarClassesJava = mmake_var_defn_list(ModuleMakeVarName ++ ".classes",
        [string.format("$(%s.mods:%%=$(classes_subdir)%%.class)",
            [s(ModuleMakeVarName)]),
        string.format(
            "$(wildcard $(%s.mods:%%=$(classes_subdir)%%\\$$*.class))",
            [s(ModuleMakeVarName)])]),
    MmakeVarClassesNonJava = mmake_var_defn(ModuleMakeVarName ++ ".classes",
        ""),
    MmakeFragmentVarClasses = mmf_conditional_entry(
        mmake_cond_grade_has_component("java"),
        MmakeVarClassesJava, MmakeVarClassesNonJava),

    MmakeVarCss = mmake_var_defn(ModuleMakeVarName ++ ".css",
        string.format("$(%s.mods:%%=$(css_subdir)%%.cs)",
            [s(ModuleMakeVarName)])),
    MmakeVarAllCss = mmake_var_defn(ModuleMakeVarName ++ ".all_css",
        string.format("$(%s.mods:%%=$(css_subdir)%%.cs)",
            [s(ModuleMakeVarName)])),
    MmakeVarDirs = mmake_var_defn(ModuleMakeVarName ++ ".dirs",
        string.format("$(%s.mods:%%=$(dirs_subdir)%%.dir)",
            [s(ModuleMakeVarName)])),
    MmakeVarDirOs = mmake_var_defn(ModuleMakeVarName ++ ".dir_os",
        string.format("$(%s.mods:%%=$(dirs_subdir)%%.dir/*.$O)",
            [s(ModuleMakeVarName)])),
    MmakeVarDates = mmake_var_defn(ModuleMakeVarName ++ ".dates",
        string.format("$(%s.mods:%%=$(dates_subdir)%%.date)",
            [s(ModuleMakeVarName)])),
    MmakeVarDate0s = mmake_var_defn(ModuleMakeVarName ++ ".date0s",
        string.format("$(%s.mods:%%=$(date0s_subdir)%%.date0)",
            [s(ModuleMakeVarName)])),
    MmakeVarDate3s = mmake_var_defn(ModuleMakeVarName ++ ".date3s",
        string.format("$(%s.mods:%%=$(date3s_subdir)%%.date3)",
            [s(ModuleMakeVarName)])),
    MmakeVarOptDates = mmake_var_defn(ModuleMakeVarName ++ ".optdates",
        string.format("$(%s.mods:%%=$(optdates_subdir)%%.optdate)",
            [s(ModuleMakeVarName)])),
    MmakeVarTransOptDates =
        mmake_var_defn(ModuleMakeVarName ++ ".trans_opt_dates",
            string.format(
                "$(%s.mods:%%=$(trans_opt_dates_subdir)%%.trans_opt_date)",
                [s(ModuleMakeVarName)])),
    MmakeVarCDates = mmake_var_defn(ModuleMakeVarName ++ ".c_dates",
        string.format("$(%s.mods:%%=$(c_dates_subdir)%%.c_date)",
            [s(ModuleMakeVarName)])),
    MmakeVarJavaDates = mmake_var_defn(ModuleMakeVarName ++ ".java_dates",
        string.format("$(%s.mods:%%=$(java_dates_subdir)%%.java_date)",
            [s(ModuleMakeVarName)])),
    MmakeVarCsDates = mmake_var_defn(ModuleMakeVarName ++ ".cs_dates",
        string.format("$(%s.mods:%%=$(cs_dates_subdir)%%.cs_date)",
            [s(ModuleMakeVarName)])),
    MmakeVarDs = mmake_var_defn(ModuleMakeVarName ++ ".ds",
        string.format("$(%s.mods:%%=$(ds_subdir)%%.d)",
            [s(ModuleMakeVarName)])),

    % XXX Why is make_module_dep_file_extension a function?
    ModuleDepFileExt = make_module_dep_file_extension,
    ModuleDepFileExtStr = extension_to_string(Globals,
        ext_other(ModuleDepFileExt), newext_misc_ngs(ext_misc_ngs_module_dep)),
    MmakeVarModuleDeps = mmake_var_defn(ModuleMakeVarName ++ ".module_deps",
        string.format("$(%s.mods:%%=$(module_deps_subdir)%%%s)",
            [s(ModuleMakeVarName), s(ModuleDepFileExtStr)])),

    (
        Target = target_c,
        globals.lookup_bool_option(Globals, highlevel_code, HighLevelCode),
        (
            HighLevelCode = yes,
            % For the high level C back-end, we generate a `.mih' file
            % for every module.
            MihSources = [string.format("$(%s.mods:%%=$(mihs_subdir)%%.mih)",
                [s(ModuleMakeVarName)])]
        ;
            HighLevelCode = no,
            % For the LLDS back-end, we don't use `.mih' files at all.
            MihSources = []
        ),
        % We use `.mh' files for both low and high level C backends.
        MhSources =
            [string.format("$(%s.mods:%%=%%.mh)", [s(ModuleMakeVarName)])]
    ;
        % We don't generate C header files for non-C backends.
        ( Target = target_csharp
        ; Target = target_java
        ),
        MihSources = [],
        MhSources = []
    ),
    MmakeVarMihs =
        mmake_var_defn_list(ModuleMakeVarName ++ ".mihs", MihSources),
    MmakeVarMhs = mmake_var_defn_list(ModuleMakeVarName ++ ".mhs", MhSources),

    % The `<module>.all_mihs' variable is like `<module>.mihs' except that
    % it contains header files for all the modules, regardless of the grade
    % or --target option. It is used by the rule for `mmake realclean',
    % which should remove anything that could have been automatically
    % generated, even if the grade or --target option has changed.
    MmakeVarAllMihs = mmake_var_defn(ModuleMakeVarName ++ ".all_mihs",
        string.format("$(%s.mods:%%=$(mihs_subdir)%%.mih)",
            [s(ModuleMakeVarName)])),

    % The `<module>.all_mhs' variable is like `<module>.mhs' except that
    % it contains header files for all the modules, as for `<module>.all_mihs'
    % above.
    MmakeVarAllMhs = mmake_var_defn(ModuleMakeVarName ++ ".all_mhs",
        string.format("$(%s.mods:%%=%%.mh)",
            [s(ModuleMakeVarName)])),

    MmakeVarInts = mmake_var_defn_list(ModuleMakeVarName ++ ".ints",
        [string.format("$(%s.mods:%%=$(ints_subdir)%%.int)",
            [s(ModuleMakeVarName)]),
        string.format("$(%s.mods:%%=$(int2s_subdir)%%.int2)",
            [s(ModuleMakeVarName)])]),
    % `.int0' files are only generated for modules with submodules.
    % XXX ... or at least they should be. Currently we end up generating
    % .int0 files for nested submodules that don't have any children.
    % (We do the correct thing for separate submodules.)
    MmakeVarInt0s = mmake_var_defn(ModuleMakeVarName ++ ".int0s",
        string.format("$(%s.parent_mods:%%=$(int0s_subdir)%%.int0)",
            [s(ModuleMakeVarName)])),
    % XXX The `<module>.all_int0s' variables is like `<module>.int0s' except
    % that it contains .int0 files for all modules, regardless of whether
    % they should have been created or not. It is used by the rule for
    % `mmake realclean' to ensure that we clean up all the .int0 files,
    % including the ones that were accidentally created by the bug described
    % above.
    MmakeVarAllInt0s = mmake_var_defn(ModuleMakeVarName ++ ".all_int0s",
        string.format("$(%s.mods:%%=$(int0s_subdir)%%.int0)",
            [s(ModuleMakeVarName)])),
    MmakeVarInt3s = mmake_var_defn(ModuleMakeVarName ++ ".int3s",
        string.format("$(%s.mods:%%=$(int3s_subdir)%%.int3)",
            [s(ModuleMakeVarName)])),
    MmakeVarOpts = mmake_var_defn(ModuleMakeVarName ++ ".opts",
        string.format("$(%s.mods:%%=$(opts_subdir)%%.opt)",
            [s(ModuleMakeVarName)])),
    MmakeVarTransOpts = mmake_var_defn(ModuleMakeVarName ++ ".trans_opts",
        string.format("$(%s.mods:%%=$(trans_opts_subdir)%%.trans_opt)",
            [s(ModuleMakeVarName)])),
    MmakeVarAnalysiss = mmake_var_defn(ModuleMakeVarName ++ ".analysiss",
        string.format("$(%s.mods:%%=$(analysiss_subdir)%%.analysis)",
            [s(ModuleMakeVarName)])),
    MmakeVarRequests = mmake_var_defn(ModuleMakeVarName ++ ".requests",
        string.format("$(%s.mods:%%=$(requests_subdir)%%.request)",
            [s(ModuleMakeVarName)])),
    MmakeVarImdgs = mmake_var_defn(ModuleMakeVarName ++ ".imdgs",
        string.format("$(%s.mods:%%=$(imdgs_subdir)%%.imdg)",
            [s(ModuleMakeVarName)])),
    MmakeVarProfs = mmake_var_defn(ModuleMakeVarName ++ ".profs",
        string.format("$(%s.mods:%%=%%.prof)",
            [s(ModuleMakeVarName)])),

    MmakeFragmentsA = list.map(mmake_entry_to_fragment,
        [MmakeStartComment, MmakeVarModuleMs,
        MmakeVarModuleDepErrs, MmakeVarModuleErrs,
        MmakeVarModuleMods, MmakeVarModuleParentMods,
        MmakeVarForeignModules, MmakeVarForeignFileNames, MmakeVarForeignDlls,
        MmakeVarInitCs, MmakeVarAllCs, MmakeVarCs, MmakeVarDlls,
        MmakeVarAllOs, MmakeVarAllPicOs, MmakeVarOs, MmakeVarPicOs,
        MmakeVarUseds,
        MmakeVarJavas, MmakeVarAllJavas]),
    MmakeFragmentsB = list.map(mmake_entry_to_fragment,
        [MmakeVarCss, MmakeVarAllCss,
        MmakeVarDirs, MmakeVarDirOs,
        MmakeVarDates, MmakeVarDate0s, MmakeVarDate3s,
        MmakeVarOptDates, MmakeVarTransOptDates,
        MmakeVarCDates, MmakeVarJavaDates, MmakeVarCsDates,
        MmakeVarDs, MmakeVarModuleDeps, MmakeVarMihs,
        MmakeVarMhs, MmakeVarAllMihs, MmakeVarAllMhs,
        MmakeVarInts, MmakeVarInt0s, MmakeVarAllInt0s, MmakeVarInt3s,
        MmakeVarOpts, MmakeVarTransOpts,
        MmakeVarAnalysiss, MmakeVarRequests, MmakeVarImdgs, MmakeVarProfs]),
    MmakeFile =
        cord.from_list(MmakeFragmentsA) ++
        cord.singleton(MmakeFragmentVarClasses) ++
        cord.from_list(MmakeFragmentsB).

%---------------------%

:- pred select_ok_modules(deps_map::in, list(module_name)::in,
    list(module_name)::out) is det.

select_ok_modules(_, [], []).
select_ok_modules(DepsMap, [ModuleName | ModuleNames0], ModuleNames) :-
    select_ok_modules(DepsMap, ModuleNames0, ModuleNamesTail),
    map.lookup(DepsMap, ModuleName, deps(_, BurdenedModule)),
    Baggage = BurdenedModule ^ bm_baggage,
    ModuleErrors = Baggage ^ mb_errors,
    FatalErrors = ModuleErrors ^ rm_fatal_errors,
    ( if set.is_empty(FatalErrors) then
        ModuleNames = [ModuleName | ModuleNamesTail]
    else
        ModuleNames = ModuleNamesTail
    ).

%---------------------%

    % get_fact_table_file_names(DepsMap, Modules, ExtraLinkObjs):
    %
    % Find any extra .$O files that should be linked into the executable.
    % These include fact table object files and object files for foreign
    % code that can't be generated inline for this target.
    %
:- pred get_fact_table_file_names(deps_map::in, list(module_name)::in,
    list(file_name)::out) is det.

get_fact_table_file_names(DepsMap, Modules, FactTableFileNames) :-
    % It is possible, though very unlikely, that two or more modules
    % depend on the same fact table.
    get_fact_table_file_names(DepsMap, Modules,
        set.init, FactTableFileNamesSet),
    set.to_sorted_list(FactTableFileNamesSet, FactTableFileNames).

    % Gather file names of fact tables.
    %
:- pred get_fact_table_file_names(deps_map::in, list(module_name)::in,
    set(file_name)::in, set(file_name)::out) is det.

get_fact_table_file_names(_DepsMap, [], !FactTableFileNames).
get_fact_table_file_names(DepsMap, [Module | Modules], !FactTableFileNames) :-
    map.lookup(DepsMap, Module, deps(_, BurdenedModule)),
    ParseTreeModuleSrc = BurdenedModule ^ bm_module,
    get_fact_tables(ParseTreeModuleSrc, FactTableFileNames),
    % Handle object files for foreign code.
    % NOTE: currently none of the backends support foreign code
    % in a non target language.
    set.union(FactTableFileNames, !FactTableFileNames),
    get_fact_table_file_names(DepsMap, Modules, !FactTableFileNames).

%---------------------------------------------------------------------------%

generate_dependencies_write_dep_file(Globals, SourceFileName, ModuleName,
        DepsMap, !IO) :-
    globals.lookup_bool_option(Globals, verbose, Verbose),
    module_name_to_file_name(Globals, $pred, do_create_dirs,
        ext_other(other_ext(".dep")), newext_mmake_fragment(ext_mf_dep),
        ModuleName, DepFileName, !IO),
    get_progress_output_stream(Globals, ModuleName, ProgressStream, !IO),
    string.format("%% Creating auto-dependency file `%s'...\n",
        [s(DepFileName)], CreatingMsg),
    maybe_write_string(ProgressStream, Verbose, CreatingMsg, !IO),
    io.open_output(DepFileName, DepResult, !IO),
    (
        DepResult = ok(DepStream),
        generate_dep_file(Globals, SourceFileName, ModuleName, DepsMap,
            MmakeFile, !IO),
        write_mmakefile(DepStream, MmakeFile, !IO),
        io.close_output(DepStream, !IO),
        maybe_write_string(ProgressStream, Verbose, "% done.\n", !IO)
    ;
        DepResult = error(IOError),
        maybe_write_string(ProgressStream, Verbose, " failed.\n", !IO),
        maybe_flush_output(ProgressStream, Verbose, !IO),
        get_error_output_stream(Globals, ModuleName, ErrorStream, !IO),
        io.error_message(IOError, IOErrorMessage),
        string.format("error opening file `%s' for output: %s",
            [s(DepFileName), s(IOErrorMessage)], DepMessage),
        report_error(ErrorStream, DepMessage, !IO)
    ).

%---------------------------------------------------------------------------%

:- type maybe_mmake_var == pair(list(string), string).

:- pred generate_dep_file(globals::in, file_name::in, module_name::in,
    deps_map::in, mmakefile::out, io::di, io::uo) is det.

generate_dep_file(Globals, SourceFileName, ModuleName, DepsMap,
        !:MmakeFile, !IO) :-
    ModuleNameString = sym_name_to_string(ModuleName),
    library.version(Version, FullArch),

    MmakeStartComment = mmake_start_comment("program dependencies",
        ModuleNameString, SourceFileName, Version, FullArch),

    module_name_to_make_var_name(ModuleName, ModuleMakeVarName),

    module_name_to_file_name(Globals, $pred, do_create_dirs,
        ext_other(other_ext(".init")), newext_lib_gs(ext_lib_gs_init),
        ModuleName, InitFileName, !IO),
    module_name_to_file_name(Globals, $pred, do_create_dirs,
        ext_other(other_ext("_init.c")), newext_target_init_c(ext_init_c),
        ModuleName, InitCFileName, !IO),
    module_name_to_file_name(Globals, $pred, do_create_dirs,
        ext_other(other_ext("_init.$O")),
        newext_target_init_obj(ext_init_obj_dollar_o),
        ModuleName, InitObjFileName, !IO),
    module_name_to_file_name(Globals, $pred, do_create_dirs,
        ext_other(other_ext("_init.pic_o")),
        newext_target_init_obj(ext_init_obj_pic_o),
        ModuleName, InitPicObjFileName, !IO),

    globals.lookup_bool_option(Globals, generate_mmc_make_module_dependencies,
        MmcMakeDeps),
    globals.lookup_bool_option(Globals, intermodule_optimization, Intermod),
    globals.lookup_bool_option(Globals, transitive_optimization, TransOpt),
    (
        MmcMakeDeps = yes,
        ModuleDepsVar = "$(" ++ ModuleMakeVarName ++ ".module_deps)",
        MaybeModuleDepsVar = [ModuleDepsVar],
        MaybeModuleDepsVarSpace = ModuleDepsVar ++ " "
    ;
        MmcMakeDeps = no,
        MaybeModuleDepsVar = [],
        MaybeModuleDepsVarSpace = ""
    ),
    (
        Intermod = yes,
        OptsVar = "$(" ++ ModuleMakeVarName ++ ".opts)",
        MaybeOptsVar = [OptsVar],
        MaybeOptsVarSpace = OptsVar ++ " "
    ;
        Intermod = no,
        MaybeOptsVar = [],
        MaybeOptsVarSpace = ""
    ),
    (
        TransOpt = yes,
        TransOptsVar = "$(" ++ ModuleMakeVarName ++ ".trans_opts)",
        MaybeTransOptsVar = [TransOptsVar],
        MaybeTransOptsVarSpace = TransOptsVar ++ " "
    ;
        TransOpt = no,
        MaybeTransOptsVar = [],
        MaybeTransOptsVarSpace = ""
    ),
    MaybeModuleDepsVarPair = MaybeModuleDepsVar - MaybeModuleDepsVarSpace,
    MaybeOptsVarPair = MaybeOptsVar - MaybeOptsVarSpace,
    MaybeTransOptsVarPair = MaybeTransOptsVar - MaybeTransOptsVarSpace,

    start_mmakefile(!:MmakeFile),
    add_mmake_entry(MmakeStartComment, !MmakeFile),
    generate_dep_file_exec_library_targets(Globals, ModuleName,
        ModuleMakeVarName, InitFileName, InitObjFileName,
        MaybeOptsVar, MaybeTransOptsVar,
        ExeFileName, JarFileName, LibFileName, SharedLibFileName,
        !MmakeFile, !IO),
    generate_dep_file_init_targets(Globals, ModuleName, ModuleMakeVarName,
        InitCFileName, InitFileName, DepFileName, DvFileName, !MmakeFile, !IO),
    generate_dep_file_install_targets(Globals, ModuleName, DepsMap,
        ModuleMakeVarName, MmcMakeDeps, Intermod, TransOpt,
        MaybeModuleDepsVarPair, MaybeOptsVarPair, MaybeTransOptsVarPair,
        !MmakeFile, !IO),
    generate_dep_file_collective_targets(Globals, ModuleName,
        ModuleMakeVarName, !MmakeFile, !IO),
    generate_dep_file_clean_targets(Globals, ModuleName, ModuleMakeVarName,
        ExeFileName, InitCFileName, InitObjFileName, InitPicObjFileName,
        InitFileName, LibFileName, SharedLibFileName, JarFileName,
        DepFileName, DvFileName, !MmakeFile, !IO).

:- pred generate_dep_file_exec_library_targets(globals::in,
    module_name::in, string::in, string::in, string::in,
    list(string)::in, list(string)::in,
    string::out, string::out, string::out, string::out,
    mmakefile::in, mmakefile::out, io::di, io::uo) is det.

generate_dep_file_exec_library_targets(Globals, ModuleName,
        ModuleMakeVarName, InitFileName, InitObjFileName,
        MaybeOptsVar, MaybeTransOptsVar,
        ExeFileName, JarFileName, LibFileName, SharedLibFileName,
        !MmakeFile, !IO) :-
    module_name_to_file_name(Globals, $pred, do_not_create_dirs,
        ext_other(other_ext("")), newext_exec_gs(ext_exec_gs_noext),
        ModuleName, ExeFileName, !IO),
    MmakeRuleExtForExe = mmake_simple_rule("ext_for_exe",
        mmake_rule_is_phony,
        ExeFileName,
        [ExeFileName ++ "$(EXT_FOR_EXE)"],
        []),
    MmakeFragmentExtForExe = mmf_conditional_fragments(
        mmake_cond_strings_not_equal("$(EXT_FOR_EXE)", ""),
        [mmf_entry(MmakeRuleExtForExe)], []),

    % Note we have to do some ``interesting'' hacks to get
    % `$(ALL_MLLIBS_DEP)' to work in the dependency list,
    % without getting complaints about undefined variables.
    All_MLLibsDep =
        "$(foreach @," ++ ModuleMakeVarName ++ ",$(ALL_MLLIBS_DEP))",
    All_MLObjs =
        "$(foreach @," ++ ModuleMakeVarName ++ ",$(ALL_MLOBJS))",
    All_MLPicObjs =
        "$(patsubst %.o,%.$(EXT_FOR_PIC_OBJECTS)," ++
        "$(foreach @," ++ ModuleMakeVarName ++ ",$(ALL_MLOBJS)))",

    NL_All_MLObjs = "\\\n\t\t" ++ All_MLObjs,

    % When compiling to C, we want to include $(foo.cs) first in
    % the dependency list, before $(foo.os).
    % This is not strictly necessary, since the .$O files themselves depend
    % on the .c files, but want to do it to ensure that Make will try to
    % create all the C files first, thus detecting errors early,
    % rather than first spending time compiling C files to .$O,
    % which could be a waste of time if the program contains errors.

    ModuleMakeVarNameClasses = "$(" ++ ModuleMakeVarName ++ ".classes)",

    ModuleMakeVarNameOs = "$(" ++ ModuleMakeVarName ++ ".os)",
    NonJavaMainRuleAction1Line1 =
        "$(ML) $(ALL_GRADEFLAGS) $(ALL_MLFLAGS) -- $(ALL_LDFLAGS) " ++
            "$(EXEFILE_OPT)" ++ ExeFileName ++ "$(EXT_FOR_EXE) " ++
            InitObjFileName ++ " \\",
    NonJavaMainRuleAction1Line2 =
        "\t" ++ ModuleMakeVarNameOs ++ " " ++ NL_All_MLObjs ++
            " $(ALL_MLLIBS)",
    MmakeRuleExecutableJava = mmake_simple_rule("executable_java",
        mmake_rule_is_not_phony,
        ExeFileName,
        [ModuleMakeVarNameClasses],
        []),
    MmakeRuleExecutableNonJava = mmake_simple_rule("executable_non_java",
        mmake_rule_is_not_phony,
        ExeFileName ++ "$(EXT_FOR_EXE)",
        [ModuleMakeVarNameOs, InitObjFileName, All_MLObjs, All_MLLibsDep],
        [NonJavaMainRuleAction1Line1, NonJavaMainRuleAction1Line2]),
    MmakeFragmentExecutable = mmf_conditional_entry(
        mmake_cond_grade_has_component("java"),
        MmakeRuleExecutableJava, MmakeRuleExecutableNonJava),

    module_name_to_lib_file_name(Globals, $pred, do_not_create_dirs, "lib",
        other_ext(""), newext_exec_gs(ext_exec_gs_noext),
        ModuleName, LibTargetName, !IO),
    module_name_to_lib_file_name(Globals, $pred, do_create_dirs, "lib",
        other_ext(".$A"), newext_lib_gs(ext_lib_gs_dollar_a),
        ModuleName, LibFileName, !IO),
    module_name_to_lib_file_name(Globals, $pred, do_create_dirs, "lib",
        other_ext(".$(EXT_FOR_SHARED_LIB)"), newext_lib(ext_lib_dollar_efsl),
        ModuleName, SharedLibFileName, !IO),
    % XXX EXT What is the point of this call, given the call just above?
    module_name_to_lib_file_name(Globals, $pred, do_not_create_dirs, "lib",
        other_ext(".$(EXT_FOR_SHARED_LIB)"), newext_lib(ext_lib_dollar_efsl),
        ModuleName, MaybeSharedLibFileName, !IO),
    module_name_to_file_name(Globals, $pred, do_not_create_dirs,
        ext_other(other_ext(".jar")), newext_lib_gs(ext_lib_gs_jar),
        ModuleName, JarFileName, !IO),

    % Set up the installed name for shared libraries.

    globals.lookup_bool_option(Globals, shlib_linker_use_install_name,
        UseInstallName),
    (
        UseInstallName = yes,
        get_install_name_option(Globals, SharedLibFileName, InstallNameOpt)
    ;
        UseInstallName = no,
        InstallNameOpt = ""
    ),

    ModuleMakeVarNameInts = "$(" ++ ModuleMakeVarName ++ ".ints)",
    ModuleMakeVarNameInt3s = "$(" ++ ModuleMakeVarName ++ ".int3s)",
    AllIntSources = [ModuleMakeVarNameInts, ModuleMakeVarNameInt3s] ++
        MaybeOptsVar ++ MaybeTransOptsVar ++ [InitFileName],
    MmakeRuleLibTargetJava = mmake_simple_rule("lib_target_java",
        mmake_rule_is_phony,
        LibTargetName,
        [JarFileName | AllIntSources],
        []),
    MmakeRuleLibTargetNonJava = mmake_simple_rule("lib_target_non_java",
        mmake_rule_is_phony,
        LibTargetName,
        [LibFileName, MaybeSharedLibFileName | AllIntSources],
        []),
    MmakeFragmentLibTarget = mmf_conditional_entry(
        mmake_cond_grade_has_component("java"),
        MmakeRuleLibTargetJava, MmakeRuleLibTargetNonJava),

    ModuleMakeVarNamePicOs = "$(" ++ ModuleMakeVarName ++ ".pic_os)",
    SharedLibAction1Line1 =
        "$(ML) --make-shared-lib $(ALL_GRADEFLAGS) $(ALL_MLFLAGS) " ++
        "-- " ++ InstallNameOpt ++ " $(ALL_LD_LIBFLAGS) " ++
        "-o " ++ SharedLibFileName ++ " \\",
    SharedLibAction1Line2 = "\t" ++ ModuleMakeVarNamePicOs ++ " \\",
    SharedLibAction1Line3 = "\t" ++ All_MLPicObjs ++ " $(ALL_MLLIBS)",
    MmakeRuleSharedLib = mmake_simple_rule("shared_lib",
        mmake_rule_is_not_phony,
        SharedLibFileName,
        [ModuleMakeVarNamePicOs, All_MLPicObjs, All_MLLibsDep],
        [SharedLibAction1Line1, SharedLibAction1Line2, SharedLibAction1Line3]),
    MmakeFragmentSharedLib = mmf_conditional_fragments(
        mmake_cond_strings_not_equal("$(EXT_FOR_SHARED_LIB)", "$(A)"),
        [mmf_entry(MmakeRuleSharedLib)], []),

    LibAction1 = "rm -f " ++ LibFileName,
    LibAction2Line1 =
        "$(AR) $(ALL_ARFLAGS) $(AR_LIBFILE_OPT)" ++ LibFileName ++
            " " ++ ModuleMakeVarNameOs ++ " \\",
    LibAction2Line2 = "\t" ++ All_MLObjs,
    LibAction3 = "$(RANLIB) $(ALL_RANLIBFLAGS) " ++ LibFileName,
    MmakeRuleLib = mmake_simple_rule("lib",
        mmake_rule_is_not_phony,
        LibFileName,
        [ModuleMakeVarNameOs, All_MLObjs],
        [LibAction1, LibAction2Line1, LibAction2Line2, LibAction3]),

    list_class_files_for_jar_mmake(Globals, ModuleMakeVarNameClasses,
        ListClassFiles),
    JarAction1 = "$(JAR) $(JAR_CREATE_FLAGS) " ++ JarFileName ++ " " ++
        ListClassFiles,
    MmakeRuleJar = mmake_simple_rule("jar",
        mmake_rule_is_not_phony,
        JarFileName,
        [ModuleMakeVarNameClasses],
        [JarAction1]),

    add_mmake_fragment(MmakeFragmentExtForExe, !MmakeFile),
    add_mmake_fragment(MmakeFragmentExecutable, !MmakeFile),
    add_mmake_fragment(MmakeFragmentLibTarget, !MmakeFile),
    add_mmake_fragment(MmakeFragmentSharedLib, !MmakeFile),
    add_mmake_entries([MmakeRuleLib, MmakeRuleJar], !MmakeFile).

:- pred generate_dep_file_init_targets(globals::in,
    module_name::in, string::in, string::in, string::in,
    string::out, string::out,
    mmakefile::in, mmakefile::out, io::di, io::uo) is det.

generate_dep_file_init_targets(Globals, ModuleName, ModuleMakeVarName,
        InitCFileName, InitFileName, DepFileName, DvFileName,
        !MmakeFile, !IO) :-
    module_name_to_file_name(Globals, $pred, do_not_create_dirs,
        ext_other(other_ext(".dep")), newext_mmake_fragment(ext_mf_dep),
        ModuleName, DepFileName, !IO),
    module_name_to_file_name(Globals, $pred, do_not_create_dirs,
        ext_other(other_ext(".dv")), newext_mmake_fragment(ext_mf_dv),
        ModuleName, DvFileName, !IO),

    ModuleMakeVarNameCs = "$(" ++ ModuleMakeVarName ++ ".cs)",
    InitAction1 = "echo > " ++ InitFileName,
    InitAction2 = "$(MKLIBINIT) " ++ ModuleMakeVarNameCs ++
        " >> " ++ InitFileName,
    % $(EXTRA_INIT_COMMAND) should expand to a command to
    % generate extra entries in the `.init' file for a library.
    % It may expand to the empty string.
    InitAction3 = "$(EXTRA_INIT_COMMAND) >> " ++ InitFileName,
    MmakeRuleInitFile = mmake_simple_rule("init_file",
        mmake_rule_is_not_phony,
        InitFileName,
        [DepFileName, ModuleMakeVarNameCs],
        [InitAction1, InitAction2, InitAction3]),

    % The `force-module_init' dependency forces the commands for
    % the `module_init.c' rule to be run every time the rule
    % is considered.
    ModuleFileName = sym_name_to_string(ModuleName),
    ForceC2InitTarget = "force-" ++ ModuleFileName ++ "_init",
    MmakeRuleForceInitCFile = mmake_simple_rule("force_init_c_file",
        mmake_rule_is_not_phony,
        ForceC2InitTarget,
        [],
        []),

    TmpInitCFileName = InitCFileName ++ ".tmp",
    ModuleMakeVarNameInitCs = "$(" ++ ModuleMakeVarName ++ ".init_cs)",
    InitCAction1 =
        "@$(C2INIT) $(ALL_GRADEFLAGS) $(ALL_C2INITFLAGS) " ++
            "--init-c-file " ++ TmpInitCFileName ++ " " ++
            ModuleMakeVarNameInitCs ++ " $(ALL_C2INITARGS)",
    InitCAction2 = "@mercury_update_interface " ++ InitCFileName,
    MmakeRuleInitCFile = mmake_simple_rule("init_c_file",
        mmake_rule_is_not_phony,
        InitCFileName,
        [ForceC2InitTarget, ModuleMakeVarNameCs],
        [InitCAction1, InitCAction2]),

    add_mmake_entries(
        [MmakeRuleInitFile, MmakeRuleForceInitCFile, MmakeRuleInitCFile],
        !MmakeFile).

:- pred generate_dep_file_install_targets(globals::in, module_name::in,
    deps_map::in, string::in, bool::in, bool::in, bool::in,
    maybe_mmake_var::in, maybe_mmake_var::in, maybe_mmake_var::in,
    mmakefile::in, mmakefile::out, io::di, io::uo) is det.

generate_dep_file_install_targets(Globals, ModuleName, DepsMap,
        ModuleMakeVarName, MmcMakeDeps, Intermod, TransOpt,
        MaybeModuleDepsVarPair, MaybeOptsVarPair, MaybeTransOptsVarPair,
        !MmakeFile, !IO) :-
    % XXX  Note that we install the `.opt' and `.trans_opt' files
    % in two places: in the `lib/$(GRADE)/opts' directory, so
    % that mmc will find them, and also in the `ints' directory,
    % so that Mmake will find them. That is not ideal, but it works.

    MaybeOptsVarPair = MaybeOptsVar - MaybeOptsVarSpace,
    MaybeTransOptsVarPair = MaybeTransOptsVar - MaybeTransOptsVarSpace,
    MaybeModuleDepsVarPair = MaybeModuleDepsVar - MaybeModuleDepsVarSpace,

    module_name_to_lib_file_name(Globals, $pred, do_not_create_dirs, "lib",
        other_ext(".install_ints"), newext_mmake_target(ext_mt_install_ints),
        ModuleName, LibInstallIntsTargetName, !IO),
    module_name_to_lib_file_name(Globals, $pred, do_not_create_dirs, "lib",
        other_ext(".install_opts"), newext_mmake_target(ext_mt_install_opts),
        ModuleName, LibInstallOptsTargetName, !IO),
    module_name_to_lib_file_name(Globals, $pred, do_not_create_dirs, "lib",
        other_ext(".install_hdrs"), newext_mmake_target(ext_mt_install_hdrs),
        ModuleName, LibInstallHdrsTargetName, !IO),
    module_name_to_lib_file_name(Globals, $pred, do_not_create_dirs, "lib",
        other_ext(".install_grade_hdrs"),
        newext_mmake_target(ext_mt_install_grade_hdrs),
        ModuleName, LibInstallGradeHdrsTargetName, !IO),

    ModuleMakeVarNameInts = "$(" ++ ModuleMakeVarName ++ ".ints)",
    ModuleMakeVarNameInt3s = "$(" ++ ModuleMakeVarName ++ ".int3s)",

    (
        Intermod = yes,
        MaybeSpaceOptStr = " opt"
    ;
        Intermod = no,
        MaybeSpaceOptStr = ""
    ),
    ( if
        some [BurdenedModule] (
            map.member(DepsMap, _, deps(_, BurdenedModule)),
            ParseTreeModuleSrc = BurdenedModule ^ bm_module,
            IncludeMap = ParseTreeModuleSrc ^ ptms_include_map,
            not map.is_empty(IncludeMap)
        )
    then
        % We always install `.int0' files; see the comment in the body of
        % make.program_target.install_ints_and_headers/8 for the reason why.
        SpaceInt0Str = " int0",
        ModuleVarNameInt0s = "$(" ++ ModuleMakeVarName ++ ".int0s)",
        MaybeModuleVarNameInt0sSpace = ModuleVarNameInt0s ++ " ",
        MaybeModuleVarNameInt0s = [ModuleVarNameInt0s]
    else
        SpaceInt0Str = "",
        MaybeModuleVarNameInt0sSpace = "",
        MaybeModuleVarNameInt0s = []
    ),
    (
        TransOpt = yes,
        MaybeSpaceTransOptStr = " trans_opt"
    ;
        TransOpt = no,
        MaybeSpaceTransOptStr = ""
    ),
    (
        MmcMakeDeps = yes,
        MaybeSpaceDepStr = " module_dep"
    ;
        MmcMakeDeps = no,
        MaybeSpaceDepStr = ""
    ),

    LibInstallIntsFiles = """" ++
        ModuleMakeVarNameInts ++ " " ++ ModuleMakeVarNameInt3s ++ " " ++
        MaybeModuleVarNameInt0sSpace ++ MaybeOptsVarSpace ++
        MaybeTransOptsVarSpace ++ MaybeModuleDepsVarSpace ++ """",

    MmakeRuleLibInstallInts = mmake_simple_rule("lib_install_ints",
        mmake_rule_is_phony,
        LibInstallIntsTargetName,
        [ModuleMakeVarNameInts, ModuleMakeVarNameInt3s] ++
            MaybeModuleVarNameInt0s ++ MaybeOptsVar ++ MaybeTransOptsVar ++
            MaybeModuleDepsVar ++ ["install_lib_dirs"],
        ["files=" ++ LibInstallIntsFiles ++ "; \\",
        "for file in $$files; do \\",
        "\ttarget=""$(INSTALL_INT_DIR)/`basename $$file`""; \\",
        "\tif cmp -s ""$$file"" ""$$target""; then \\",
        "\t\techo \"$$target unchanged\"; \\",
        "\telse \\",
        "\t\techo \"installing $$target\"; \\",
        "\t\t$(INSTALL) ""$$file"" ""$$target""; \\",
        "\tfi; \\",
        "done",
        "# The following is needed to support the `--use-subdirs' option.",
        "# We try using `$(LN_S)', but if that fails, then we just use",
        "# `$(INSTALL)'.",
        "for ext in int int2 int3" ++
            SpaceInt0Str ++ MaybeSpaceOptStr ++ MaybeSpaceTransOptStr ++
            MaybeSpaceDepStr ++ "; do \\",
        "\tdir=""$(INSTALL_INT_DIR)/Mercury/$${ext}s""; \\",
        "\trm -rf ""$$dir""; \\",
        "\t$(LN_S) .. ""$$dir"" || { \\",
        "\t\t{ [ -d ""$$dir"" ] || \\",
        "\t\t$(INSTALL_MKDIR) ""$$dir""; } && \\",
        "\t\t$(INSTALL) ""$(INSTALL_INT_DIR)""/*.$$ext ""$$dir""; \\",
        "\t} || exit 1; \\",
        "done"]),

    ( if
        Intermod = no,
        TransOpt = no
    then
        LibInstallOptsSources = [],
        LibInstallOptsActions = [silent_noop_action]
    else
        LibInstallOptsSources = MaybeOptsVar ++ MaybeTransOptsVar ++
            ["install_grade_dirs"],
        LibInstallOptsFiles =
            """" ++ MaybeOptsVarSpace ++ MaybeTransOptsVarSpace ++ """",
        LibInstallOptsActions =
            ["files=" ++ LibInstallOptsFiles ++ "; \\",
            "for file in $$files; do \\",
            "\ttarget=""$(INSTALL_GRADE_INT_DIR)/`basename $$file`"";\\",
            "\tif cmp -s ""$$file"" ""$$target""; then \\",
            "\t\techo \"$$target unchanged\"; \\",
            "\telse \\",
            "\t\techo \"installing $$target\"; \\",
            "\t\t$(INSTALL) ""$$file"" ""$$target""; \\",
            "\tfi; \\",
            "done",
            "# The following is needed to support the `--use-subdirs' option",
            "# We try using `$(LN_S)', but if that fails, then we just use",
            "# `$(INSTALL)'.",
            "for ext in " ++ MaybeSpaceOptStr ++ MaybeSpaceTransOptStr ++
                "; do \\",
            "\tdir=""$(INSTALL_GRADE_INT_DIR)/Mercury/$${ext}s""; \\",
            "\trm -rf ""$$dir""; \\",
            "\t$(LN_S) .. ""$$dir"" || { \\",
            "\t\t{ [ -d ""$$dir"" ] || \\",
            "\t\t\t$(INSTALL_MKDIR) ""$$dir""; } && \\",
            "\t\t$(INSTALL) ""$(INSTALL_GRADE_INT_DIR)""/*.$$ext \\",
            "\t\t\t""$$dir""; \\",
            "\t} || exit 1; \\",
            "done"]
    ),
    MmakeRuleLibInstallOpts = mmake_simple_rule("lib_install_opts",
        mmake_rule_is_phony,
        LibInstallOptsTargetName,
        LibInstallOptsSources,
        LibInstallOptsActions),

    % XXX Note that we install the header files in two places:
    % in the `lib/inc' or `lib/$(GRADE)/$(FULLARCH)/inc' directory,
    % so that the C compiler will find them, and also in the `ints' directory,
    % so that Mmake will find them. That is not ideal, but it works.
    %
    % (A better fix would be to change the VPATH setting in
    % scripts/Mmake.vars.in so that Mmake also searches the
    % `lib/$(GRADE)/$(FULLARCH)/inc' directory, but doing that properly
    % is non-trivial.)

    ModuleMakeVarNameMhs = string.format("$(%s.mhs)", [s(ModuleMakeVarName)]),
    MmakeRuleLibInstallHdrsNoMhs = mmake_simple_rule("install_lib_hdrs_nomhs",
        mmake_rule_is_phony,
        LibInstallHdrsTargetName,
        [ModuleMakeVarNameMhs, "install_lib_dirs"],
        [silent_noop_action]),
    MmakeRuleLibInstallHdrsMhs = mmake_simple_rule("install_lib_hdrs_mhs",
        mmake_rule_is_phony,
        LibInstallHdrsTargetName,
        [ModuleMakeVarNameMhs, "install_lib_dirs"],
        ["for hdr in " ++ ModuleMakeVarNameMhs ++ "; do \\",
        "\t$(INSTALL) $$hdr $(INSTALL_INT_DIR); \\",
        "\t$(INSTALL) $$hdr $(INSTALL_INC_DIR); \\",
        "done"]),
    MmakeFragmentLibInstallHdrs = mmf_conditional_entry(
        mmake_cond_strings_equal(ModuleMakeVarNameMhs, ""),
        MmakeRuleLibInstallHdrsNoMhs,
        MmakeRuleLibInstallHdrsMhs),

    ModuleMakeVarNameMihs =
        string.format("$(%s.mihs)", [s(ModuleMakeVarName)]),
    MmakeRuleLibInstallGradeHdrsNoMihs = mmake_simple_rule(
        "install_grade_hdrs_no_mihs",
        mmake_rule_is_phony,
        LibInstallGradeHdrsTargetName,
        [ModuleMakeVarNameMihs, "install_grade_dirs"],
        [silent_noop_action]),
    MmakeRuleLibInstallGradeHdrsMihs = mmake_simple_rule(
        "install_grade_hdrs_mihs",
        mmake_rule_is_phony,
        LibInstallGradeHdrsTargetName,
        [ModuleMakeVarNameMihs, "install_grade_dirs"],
        ["for hdr in " ++ ModuleMakeVarNameMihs ++ "; do \\",
        "\t$(INSTALL) $$hdr $(INSTALL_INT_DIR); \\",
        "\t$(INSTALL) $$hdr $(INSTALL_GRADE_INC_DIR); \\",
        "done",
        "# The following is needed to support the `--use-subdirs' option.",
        "# We try using `$(LN_S)', but if that fails, then we just use",
        "# `$(INSTALL)'.",
        "rm -rf $(INSTALL_GRADE_INC_SUBDIR)",
        "$(LN_S) .. $(INSTALL_GRADE_INC_SUBDIR) || { \\",
        "\t{ [ -d $(INSTALL_GRADE_INC_SUBDIR) ] || \\",
        "\t\t$(INSTALL_MKDIR) $(INSTALL_GRADE_INC_SUBDIR); \\",
        "\t} && \\",
        "\t$(INSTALL) $(INSTALL_GRADE_INC_DIR)/*.mih \\",
        "\t\t$(INSTALL_GRADE_INC_SUBDIR); \\",
        "} || exit 1",
        "rm -rf $(INSTALL_INT_DIR)/Mercury/mihs",
        "$(LN_S) .. $(INSTALL_INT_DIR)/Mercury/mihs || { \\",
        "\t{ [ -d $(INSTALL_INT_DIR)/Mercury/mihs ] || \\",
        "\t\t$(INSTALL_MKDIR) \\",
        "\t\t$(INSTALL_INT_DIR)/Mercury/mihs; \\",
        "\t} && \\",
        "\t$(INSTALL) $(INSTALL_GRADE_INC_DIR)/*.mih \\",
        "\t\t$(INSTALL_INT_DIR); \\",
        "} || exit 1"]),
    MmakeFragmentLibInstallGradeHdrs = mmf_conditional_entry(
        mmake_cond_strings_equal(ModuleMakeVarNameMihs, ""),
        MmakeRuleLibInstallGradeHdrsNoMihs,
        MmakeRuleLibInstallGradeHdrsMihs),

    add_mmake_entry(MmakeRuleLibInstallInts, !MmakeFile),
    add_mmake_entry(MmakeRuleLibInstallOpts, !MmakeFile),
    add_mmake_fragment(MmakeFragmentLibInstallHdrs, !MmakeFile),
    add_mmake_fragment(MmakeFragmentLibInstallGradeHdrs, !MmakeFile).

:- pred generate_dep_file_collective_targets(globals::in,
    module_name::in, string::in,
    mmakefile::in, mmakefile::out, io::di, io::uo) is det.

generate_dep_file_collective_targets(Globals, ModuleName,
        ModuleMakeVarName, !MmakeFile, !IO) :-
    list.map_foldl(
        generate_dep_file_collective_target(Globals, ModuleName,
            ModuleMakeVarName), 
        [{ext_other(other_ext(".check")),
            newext_mmake_target(ext_mt_check), ".errs"},
        {ext_other(other_ext(".ints")),
            newext_mmake_target(ext_mt_ints), ".dates"},
        {ext_other(other_ext(".int3s")),
            newext_mmake_target(ext_mt_int3s), ".date3s"},
        {ext_other(other_ext(".opts")),
            newext_mmake_target(ext_mt_opts), ".optdates"},
        {ext_other(other_ext(".trans_opts")),
            newext_mmake_target(ext_mt_trans_opts), ".trans_opt_dates"},
        {ext_other(other_ext(".javas")),
            newext_mmake_target(ext_mt_javas), ".javas"},
        {ext_other(other_ext(".classes")),
            newext_mmake_target(ext_mt_classes), ".classes"},
        {ext_other(other_ext(".all_ints")),
            newext_mmake_target(ext_mt_all_ints), ".dates"},
        {ext_other(other_ext(".all_int3s")),
            newext_mmake_target(ext_mt_all_int3s), ".date3s"},
        {ext_other(other_ext(".all_opts")),
            newext_mmake_target(ext_mt_all_opts), ".optdates"},
        {ext_other(other_ext(".all_trans_opts")),
            newext_mmake_target(ext_mt_all_trans_opts), ".trans_opt_dates"}],
        MmakeRules, !IO),
    add_mmake_entries(MmakeRules, !MmakeFile).

:- pred generate_dep_file_collective_target(globals::in,
    module_name::in, string::in, {ext, newext, string}::in,
    mmake_entry::out, io::di, io::uo) is det.

generate_dep_file_collective_target(Globals, ModuleName, ModuleMakeVarName,
        {Ext, NewExt, VarExtension}, MmakeRule, !IO) :-
    module_name_to_file_name(Globals, $pred, do_not_create_dirs, Ext, NewExt,
        ModuleName, TargetName, !IO),
    Source = string.format("$(%s%s)", [s(ModuleMakeVarName), s(VarExtension)]),
    ExtStr = extension_to_string(Globals, Ext, NewExt),
    MmakeRule = mmake_simple_rule(
        "collective_target_" ++ ExtStr ++ VarExtension, mmake_rule_is_phony,
        TargetName, [Source], []).

:- pred generate_dep_file_clean_targets(globals::in,
    module_name::in, string::in, string::in, string::in,
    string::in, string::in, string::in, string::in, string::in, string::in,
    string::in, string::in,
    mmakefile::in, mmakefile::out, io::di, io::uo) is det.

generate_dep_file_clean_targets(Globals, ModuleName, ModuleMakeVarName,
        ExeFileName, InitCFileName, InitObjFileName, InitPicObjFileName,
        InitFileName, LibFileName, SharedLibFileName, JarFileName,
        DepFileName, DvFileName, !MmakeFile, !IO) :-
    % If you change the clean targets below, please also update the
    % documentation in doc/user_guide.texi.

    module_name_to_file_name(Globals, $pred, do_not_create_dirs,
        ext_other(other_ext(".clean")),
        newext_mmake_target(ext_mt_clean),
        ModuleName, CleanTargetName, !IO),
    module_name_to_file_name(Globals, $pred, do_not_create_dirs,
        ext_other(other_ext(".realclean")),
        newext_mmake_target(ext_mt_realclean),
        ModuleName, RealCleanTargetName, !IO),

    % XXX Put these into a logical order.
    CleanSuffixes = [".dirs", ".cs", ".mihs", ".all_os", ".all_pic_os",
        ".c_dates", ".java_dates", ".useds", ".javas", ".profs",
        ".dep_errs", ".errs", ".foreign_cs"],
    CleanFiles = [InitCFileName, InitObjFileName, InitPicObjFileName],
    MmakeRulesClean =
        % XXX Why is the first rule not phony?
        [mmake_simple_rule("clean_local", mmake_rule_is_not_phony,
            "clean_local", [CleanTargetName], []),
        mmake_simple_rule("clean_target", mmake_rule_is_phony,
            CleanTargetName,
            [],
            list.map(remove_suffix_files_cmd(ModuleMakeVarName),
                CleanSuffixes) ++
            [remove_files_cmd(CleanFiles)])],

    % XXX We delete $(ModuleMakeVarName).all_int0s instead of
    % $(ModuleMakeVarName).int0s to make sure that we delete
    % any spurious .int0 files created for nested submodules.
    % For further details, see the XXX comments above.
    RealCleanSuffixes = [".dates", ".date0s", ".date3s",
        ".optdates", ".trans_opt_dates", ".ints", ".all_int0s", ".int3s",
        ".opts", ".trans_opts", ".analysiss", ".requests", ".imdgs",
        ".ds", ".module_deps", ".all_mhs", ".all_mihs", ".dlls",
        ".foreign_dlls", ".classes"],
    RealCleanFiles = [ExeFileName ++ "$(EXT_FOR_EXE) ", InitFileName,
        LibFileName, SharedLibFileName, JarFileName, DepFileName, DvFileName],
    MmakeRulesRealClean =
        % XXX Why is the first rule not phony?
        [mmake_simple_rule("realclean_local", mmake_rule_is_not_phony,
            "realclean_local", [RealCleanTargetName], []),
        mmake_simple_rule("realclean_target", mmake_rule_is_phony,
            RealCleanTargetName,
            [CleanTargetName],
            list.map(remove_suffix_files_cmd(ModuleMakeVarName),
                RealCleanSuffixes) ++
            [remove_files_cmd(RealCleanFiles)])],

    add_mmake_entries(MmakeRulesClean ++ MmakeRulesRealClean, !MmakeFile).

    % remove_suffix_files_cmd(ModuleMakeVarName, Extension):
    %
    % Return a command to delete the files in $(ModuleMakeVarNameExtension).
    %
    % XXX Xargs doesn't handle special characters in the file names correctly.
    % This is currently not a problem in practice as we never generate
    % file names containing special characters.
    %
    % Any fix for this problem will also require a fix in `mmake.in'.
    %
:- func remove_suffix_files_cmd(string, string) = string.

remove_suffix_files_cmd(ModuleMakeVarName, Extension) =
    string.format("-echo $(%s%s) | xargs rm -f",
        [s(ModuleMakeVarName), s(Extension)]).

:- func remove_files_cmd(list(string)) = string.

remove_files_cmd(Files) =
    "-rm -f " ++ string.join_list(" ", Files).

%---------------------------------------------------------------------------%

:- pred get_source_file(deps_map::in, module_name::in, file_name::out) is det.

get_source_file(DepsMap, ModuleName, FileName) :-
    map.lookup(DepsMap, ModuleName, Deps),
    Deps = deps(_, BurdenedModule),
    Baggage = BurdenedModule ^ bm_baggage,
    SourceFileName = Baggage ^ mb_source_file_name,
    ( if string.remove_suffix(SourceFileName, ".m", SourceFileBase) then
        FileName = SourceFileBase
    else
        unexpected($pred, "source file name doesn't end in `.m'")
    ).

%---------------------------------------------------------------------------%

output_module_order(Globals, ModuleName, Ext, NewExt, DepsOrdering, !IO) :-
    module_name_to_file_name(Globals, $pred, do_create_dirs,
        ext_other(Ext), NewExt, ModuleName, OrdFileName, !IO),
    get_progress_output_stream(Globals, ModuleName, ProgressStream, !IO),
    globals.lookup_bool_option(Globals, verbose, Verbose),
    string.format("%% Creating module order file `%s'...",
        [s(OrdFileName)], CreatingMsg),
    maybe_write_string(ProgressStream, Verbose, CreatingMsg, !IO),
    io.open_output(OrdFileName, OrdResult, !IO),
    (
        OrdResult = ok(OrdStream),
        io.write_list(OrdStream, DepsOrdering, "\n\n",
            write_module_scc(OrdStream), !IO),
        io.close_output(OrdStream, !IO),
        maybe_write_string(ProgressStream, Verbose, " done.\n", !IO)
    ;
        OrdResult = error(IOError),
        maybe_write_string(ProgressStream, Verbose, " failed.\n", !IO),
        maybe_flush_output(ProgressStream, Verbose, !IO),
        get_error_output_stream(Globals, ModuleName, ErrorStream, !IO),
        io.error_message(IOError, IOErrorMessage),
        string.format("error opening file `%s' for output: %s",
            [s(OrdFileName), s(IOErrorMessage)], OrdMessage),
        report_error(ErrorStream, OrdMessage, !IO)
    ).

:- pred write_module_scc(io.text_output_stream::in, set(module_name)::in,
    io::di, io::uo) is det.

write_module_scc(Stream, SCC0, !IO) :-
    set.to_sorted_list(SCC0, SCC),
    % XXX This is suboptimal (the stream should be specified once, not twice),
    % but in the absence of a test case, I (zs) am leaving it alone for now.
    io.write_list(Stream, SCC, "\n", write_sym_name(Stream), !IO).

%---------------------------------------------------------------------------%

    % get_both_opt_deps(Globals, BuildOptFiles, Deps, IntermodDirs,
    %   OptDeps, TransOptDeps, !Cache, !IO):
    %
    % For each dependency, search intermod_directories for a .m file.
    % If it exists, add it to both output lists. Otherwise, if a .opt
    % file exists, add it to the OptDeps list, and if a .trans_opt
    % file exists, add it to the TransOptDeps list.
    % If --use-opt-files is set, don't look for `.m' files, since we are
    % not building `.opt' files, only using those which are available.
    % XXX This won't find nested submodules.
    % XXX Use `mmc --make' if that matters.
    %
:- pred get_both_opt_deps(globals::in, bool::in, list(string)::in,
    list(module_name)::in, list(module_name)::out, list(module_name)::out,
    module_file_name_cache::in, module_file_name_cache::out,
    io::di, io::uo) is det.

get_both_opt_deps(_, _, _, [], [], [], !Cache, !IO).
get_both_opt_deps(Globals, BuildOptFiles, IntermodDirs, [Dep | Deps],
        !:OptDeps, !:TransOptDeps, !Cache, !IO) :-
    get_both_opt_deps(Globals, BuildOptFiles, IntermodDirs, Deps,
        !:OptDeps, !:TransOptDeps, !Cache, !IO),
    (
        BuildOptFiles = yes,
        search_for_module_source(IntermodDirs, Dep, MaybeFileName, !IO),
        (
            MaybeFileName = ok(_),
            !:OptDeps = [Dep | !.OptDeps],
            !:TransOptDeps = [Dep | !.TransOptDeps],
            Found = yes
        ;
            MaybeFileName = error(_),
            Found = no
        )
    ;
        BuildOptFiles = no,
        Found = no
    ),
    (
        Found = no,
        make_module_file_name(Globals, $pred,
            ext_other(other_ext(".opt")), newext_opt(ext_opt_plain),
            Dep, OptName, !Cache, !IO),
        search_for_file_returning_dir(IntermodDirs, OptName, MaybeOptDir, !IO),
        (
            MaybeOptDir = ok(_),
            !:OptDeps = [Dep | !.OptDeps]
        ;
            MaybeOptDir = error(_)
        ),
        make_module_file_name(Globals, $pred,
            ext_other(other_ext(".trans_opt")), newext_opt(ext_opt_trans),
            Dep, TransOptName, !Cache, !IO),
        search_for_file_returning_dir(IntermodDirs, TransOptName,
            MaybeTransOptDir, !IO),
        (
            MaybeTransOptDir = ok(_),
            !:TransOptDeps = [Dep | !.TransOptDeps]
        ;
            MaybeTransOptDir = error(_)
        )
    ;
        Found = yes
    ).

get_opt_deps(_, _, _, _, _, [], [], !IO).
get_opt_deps(Globals, BuildOptFiles, IntermodDirs, OtherExt, NewExt,
        [Dep | Deps], !:OptDeps, !IO) :-
    get_opt_deps(Globals, BuildOptFiles, IntermodDirs, OtherExt, NewExt,
        Deps, !:OptDeps, !IO),
    (
        BuildOptFiles = yes,
        search_for_module_source(IntermodDirs, Dep, Result1, !IO),
        (
            Result1 = ok(_),
            !:OptDeps = [Dep | !.OptDeps],
            Found = yes
        ;
            Result1 = error(_),
            Found = no
        )
    ;
        BuildOptFiles = no,
        Found = no
    ),
    (
        Found = no,
        module_name_to_search_file_name(Globals, $pred,
            ext_other(OtherExt), NewExt, Dep, OptName, !IO),
        search_for_file(IntermodDirs, OptName, MaybeOptDir, !IO),
        (
            MaybeOptDir = ok(_),
            !:OptDeps = [Dep | !.OptDeps]
        ;
            MaybeOptDir = error(_)
        )
    ;
        Found = yes
    ).

%---------------------------------------------------------------------------%

:- pred compare_module_names(module_name::in, module_name::in,
    comparison_result::out) is det.

compare_module_names(Sym1, Sym2, Result) :-
    Str1 = sym_name_to_string(Sym1),
    Str2 = sym_name_to_string(Sym2),
    compare(Result, Str1, Str2).

%---------------------------------------------------------------------------%
:- end_module parse_tree.write_deps_file.
%---------------------------------------------------------------------------%
