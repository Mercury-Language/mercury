%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2008-2012 The University of Melbourne.
% Copyright (C) 2013-2017, 2019-2024 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: generate_mmakefile_fragments.m.
%
% This module constructs mmakefile fragments that we put into each module's
% .d file, and each program's .dv and .dep files.
%
%---------------------------------------------------------------------------%

:- module parse_tree.generate_mmakefile_fragments.
:- interface.

:- import_module libs.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module libs.mmakefiles.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.deps_map.
:- import_module parse_tree.file_names.
:- import_module parse_tree.make_module_file_names.
:- import_module parse_tree.module_baggage.

:- import_module io.
:- import_module list.
:- import_module maybe.
:- import_module set.

%---------------------------------------------------------------------------%

:- type std_deps
    --->    std_deps(
                sd_direct_deps      :: set(module_name),
                sd_indirect_deps    :: set(module_name),
                sd_fim_deps         :: set(module_name),
                sd_trans_opt_deps   :: maybe_trans_opt_deps
            ).

:- type maybe_trans_opt_deps
    --->    no_trans_opt_deps
    ;       trans_opt_deps(
                % This is the set of modules whose .trans_opt files that
                % we may want to read when *making* this module's .trans_opt
                % file. However, it still may need to be reduced further
                % to prevent circularities in trans_opt_deps mmake rules.
                set(module_name)
            ).

%---------------------%

:- type intermod_deps
    --->    intermod_deps(
                maybe_intermod_mh_deps,
                maybe_opt_file_deps
            ).

:- type maybe_intermod_mh_deps
    --->    no_intermod_mh_deps
    ;       intermod_mh_deps.

:- type maybe_opt_file_deps
    --->    no_opt_file_deps
    ;       opt_file_deps(
                ofd_plain_opt_modules   :: list(module_name),
                ofd_trans_opt_modules   :: maybe(list(module_name))
            ).

%---------------------%

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

%---------------------------------------------------------------------------%

    % generate_d_mmakefile(Globals, BurdenedAugCompUnit, StdDeps,
    %   IntermodDeps, AllDeps, MaybeInclTransOptRule, !:MmakeFile,
    %   !Cache, !IO):
    %
    % Generate the contents of the module's .d file.
    %
    % The mmake rules we construct treat C differently from Java and C#.
    % The reason is that we support using mmake when targeting C, but require
    % the use of --use-mmc-make when targeting Java and C#.
    %
    % Initially, when the only target language was C, the only build system
    % we had was mmake, so the mmake rules we generate here can do everything
    % that one wants to do when targeting C. When we added the ability to
    % target C#, we implemented it for --use-mmc-make only, *not* for mmake,
    % so the entries we generate for C# mostly just forward the work to
    % --use-mmc-make. Java is in between; there are more mmake rules for it
    % than for C#, but far from enough for full functionality. In an email
    % to m-rev on 2020 may 25, Julien said: "IIRC, most of the mmake rules
    % for Java that are not required by --use-mmc-make are long obsolete".
    % Unfortunately, apparently there is no documentation of *which*
    % mmake rules for Java are required by --use-mmc-make.
    %
    % XXX The StdDeps argument allows generate_dependencies_write_d_file
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
:- pred generate_d_mmakefile(globals::in, burdened_aug_comp_unit::in,
    std_deps::in, intermod_deps::in, set(module_name)::in,
    maybe_include_trans_opt_rule::in, mmakefile::out,
    module_file_name_cache::in, module_file_name_cache::out,
    io::di, io::uo) is det.

    % Generate the contents of a program's .dv file.
    %
:- pred generate_dv_mmakefile(globals::in, file_name::in, module_name::in,
    deps_map::in, mmakefile::out,
    module_file_name_cache::in, module_file_name_cache::out,
    io::di, io::uo) is det.

    % Generate the contents of a program's .dep file.
    %
    % XXX Why does this predicate not take an in/out pair of filename caches
    % as arguments?
    %
:- pred generate_dep_mmakefile(globals::in, file_name::in, module_name::in,
    deps_map::in, mmakefile::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.options.
:- import_module parse_tree.get_dependencies.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.module_cmds.
:- import_module parse_tree.parse_error.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_item.
:- import_module parse_tree.prog_parse_tree.
:- import_module parse_tree.source_file_map.

:- import_module bool.
:- import_module cord.
:- import_module library.
:- import_module map.
:- import_module one_or_more.
:- import_module pair.
:- import_module require.
:- import_module string.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

generate_d_mmakefile(Globals, BurdenedAugCompUnit, StdDeps, IntermodDeps,
        AllDeps, MaybeInclTransOptRule, !:MmakeFile, !Cache, !IO) :-
    BurdenedAugCompUnit = burdened_aug_comp_unit(Baggage, AugCompUnit),
    SourceFileName = Baggage ^ mb_source_file_name,
    SourceFileModuleName = Baggage ^ mb_source_file_module_name,
    MaybeTopModule = Baggage ^ mb_maybe_top_module,
    ParseTreeModuleSrc = AugCompUnit ^ acu_module_src,
    ModuleName = ParseTreeModuleSrc ^ ptms_module_name,
    ModuleNameString = sym_name_to_string(ModuleName),
    Ancestors = get_ancestors_set(ModuleName),
    InclMap = ParseTreeModuleSrc ^ ptms_include_map,
    AccPublicChildren =
        ( pred(MN::in, InclInfo::in, PC0::in, PC::out) is det :-
            InclInfo = include_module_info(Section, _Context),
            (
                Section = ms_interface,
                set.insert(MN, PC0, PC)
            ;
                Section = ms_implementation,
                % XXX If you leave out the next line,
                % the error message you get is very uninformative;
                % it merely tells you, at the call to map.foldl,
                % that AccPublicChildren is free; it does not say *why*
                % its unification with this lambda expression does not
                % bind it.
                PC = PC0
            )
        ),
    map.foldl(AccPublicChildren, InclMap, set.init, PublicChildren),
    StdDeps = std_deps(DirectDeps0, IndirectDeps0,
        ForeignImportedModuleNamesSet, MaybeTransOptDeps),

    set.delete(ModuleName, DirectDeps0, DirectDeps),
    set.difference(IndirectDeps0, DirectDeps, IndirectDeps1),
    set.delete(ModuleName, IndirectDeps1, IndirectDeps),

    get_fact_tables(ParseTreeModuleSrc, FactTableFileNamesSet),
    get_foreign_include_file_infos(ParseTreeModuleSrc, ForeignIncludeFiles),

    library.version(Version, FullArch),

    MmakeStartComment = mmake_start_comment("module dependencies",
        ModuleNameString, SourceFileName, Version, FullArch),

    module_name_to_make_var_name(ModuleName, ModuleMakeVarName),

    make_module_file_name(Globals, $pred,
        ext_cur_ngs_gs(ext_cur_ngs_gs_opt_date_trans),
        ModuleName, TransOptDateFileName, !Cache, !IO),
    construct_trans_opt_deps_rule(Globals, MaybeInclTransOptRule,
        MaybeTransOptDeps, TransOptDateFileName, MmakeRulesTransOpt,
        !Cache, !IO),

    construct_fact_tables_entries(ModuleMakeVarName,
        SourceFileName, ObjFileName, FactTableFileNamesSet,
        MmakeVarsFactTables, FactTableSourceGroups, MmakeRulesFactTables),

    ( if string.remove_suffix(SourceFileName, ".m", SourceFileBase) then
        ErrFileName = SourceFileBase ++ ".err"
    else
        unexpected($pred, "source file name doesn't end in `.m'")
    ),

    make_module_file_name(Globals, $pred,
        ext_cur_ngs_gs(ext_cur_ngs_gs_opt_date_plain),
        ModuleName, OptDateFileName, !Cache, !IO),
    make_module_file_name(Globals, $pred,
        ext_cur_ngs_gs(ext_cur_ngs_gs_target_date_c),
        ModuleName, CDateFileName, !Cache, !IO),
    make_module_file_name(Globals, $pred,
        ext_cur_ngs_gas(ext_cur_ngs_gas_obj_dollar_o),
        ModuleName, ObjFileName, !Cache, !IO),
    make_module_file_name(Globals, $pred,
        ext_cur_ngs_gs(ext_cur_ngs_gs_target_date_java),
        ModuleName, JavaDateFileName, !Cache, !IO),
    % XXX Why is the extension hardcoded to .pic_o here?  That looks wrong.
    % It should probably be .$(EXT_FOR_PIC_OBJECT) - juliensf.
    make_module_file_name(Globals, $pred,
        ext_cur_ngs_gas(ext_cur_ngs_gas_obj_pic_o),
        ModuleName, PicObjFileName, !Cache, !IO),
    make_module_file_name(Globals, $pred, ext_cur_ngs(ext_cur_ngs_int_int0),
        ModuleName, Int0FileName, !Cache, !IO),

    construct_date_file_deps_rule(Globals, ModuleName, SourceFileName,
        Ancestors, DirectDeps, IndirectDeps, PublicChildren, Int0FileName,
        OptDateFileName, TransOptDateFileName, ForeignIncludeFiles,
        CDateFileName, JavaDateFileName, ErrFileName,
        FactTableSourceGroups, MmakeRuleDateFileDeps, !Cache, !IO),

    construct_build_nested_children_first_rule(Globals,
        ModuleName, MaybeTopModule, MmakeRulesNestedDeps, !Cache, !IO),

    construct_intermod_rules(Globals, IntermodDeps, AllDeps, ErrFileName,
        TransOptDateFileName, CDateFileName, JavaDateFileName, ObjFileName,
        MmakeRulesIntermod, !Cache, !IO),

    make_module_file_name(Globals, $pred,
        ext_cur_ngs_gs(ext_cur_ngs_gs_target_c),
        ModuleName, CFileName, !Cache, !IO),
    construct_c_header_rules(Globals, ModuleName, AllDeps,
        CFileName, ObjFileName, PicObjFileName, MmakeRulesCHeaders,
        !Cache, !IO),

    construct_module_dep_fragment(Globals, ModuleName, CFileName,
        MmakeFragmentModuleDep, !Cache, !IO),

    make_module_file_name(Globals, $pred,
        ext_cur_ngs(ext_cur_ngs_int_date_int12),
        ModuleName, DateFileName, !Cache, !IO),
    make_module_file_name(Globals, $pred,
        ext_cur_ngs(ext_cur_ngs_int_date_int0),
        ModuleName, Date0FileName, !Cache, !IO),
    construct_self_and_parent_date_date0_rules(Globals, SourceFileName,
        Date0FileName, DateFileName, Ancestors, DirectDeps, IndirectDeps,
        MmakeRulesParentDates, !Cache, !IO),

    construct_foreign_import_rules(Globals, ParseTreeModuleSrc,
        ForeignImportedModuleNamesSet, ObjFileName, PicObjFileName,
        MmakeRulesForeignImports, !Cache, !IO),

    make_module_file_name(Globals, $pred,
        ext_cur_ngs(ext_cur_ngs_int_date_int3),
        ModuleName, Date3FileName, !Cache, !IO),
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
    maybe_include_trans_opt_rule::in, maybe_trans_opt_deps::in,
    string::in, list(mmake_entry)::out,
    module_file_name_cache::in, module_file_name_cache::out,
    io::di, io::uo) is det.

construct_trans_opt_deps_rule(Globals, MaybeInclTransOptRule,
        MaybeTransOptDeps0, TransOptDateFileName, MmakeRulesTransOpt,
        !Cache, !IO) :-
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
                MaybeTransOptDeps0 = trans_opt_deps(TransOptDeps0),
                set.intersect(TransOptOrder, TransOptDeps0, TransOptDeps)
            ;
                MaybeTransOptDeps0 = no_trans_opt_deps,
                unexpected($pred, "no trans_opt_deps")
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
            % Note: we used to take the intersection with DirectDeps (in the
            % caller), but this case was not separated from the previous case
            % and it greatly reduces the set of dependencies, so I'm not sure
            % if it was intentional. --pw
            TransOptDeps = DFileTransOptDeps
        ),
        % Note that maybe_read_dependency_file searches for
        % this exact pattern.
        % XXX LEGACY
        make_module_file_names_with_ext(Globals,
            ext_cur_ngs_gs_max_ngs(ext_cur_ngs_gs_max_ngs_legacy_opt_trans),
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
        MmakeVarFactTablesAllOs = mmake_var_defn(
            ModuleMakeVarName ++ ".fact_tables.all_os",
            "$(" ++ ModuleMakeVarName ++ ".fact_tables:%=$(os_subdir)%.$O)"),
        MmakeVarFactTablesAllCs = mmake_var_defn(
            ModuleMakeVarName ++ ".fact_tables.all_cs",
            "$(" ++ ModuleMakeVarName ++ ".fact_tables:%=$(cs_subdir)%.c)"),
        MmakeVarsFactTables = [MmakeVarFactTables,
            MmakeVarFactTablesAllOs, MmakeVarFactTablesAllCs],

        FactTableSourceGroup = mmake_file_name_group("fact tables",
            one_or_more("$(" ++ ModuleMakeVarName ++ ".fact_tables)", [])),
        FactTableSourceGroups = [FactTableSourceGroup],

        % XXX These rules seem wrong to me. -zs
        MmakeRuleFactOs = mmake_simple_rule("fact_table_os",
            mmake_rule_is_not_phony,
            "$(" ++ ModuleMakeVarName ++ ".fact_tables.all_os)",
            ["$(" ++ ModuleMakeVarName ++  ".fact_tables)", SourceFileName],
            []),
        MmakeRuleFactCs = mmake_simple_rule("fact_table_cs",
            mmake_rule_is_not_phony,
            "$(" ++ ModuleMakeVarName ++ ".fact_tables.all_cs)",
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
        Ancestors, DirectDeps, IndirectDeps, PublicChildren, Int0FileName,
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

    SourceFileNameGroup =
        [make_singleton_file_name_group("source", SourceFileName)],
    % If the module contains nested submodules, then the `.int0' file
    % must first be built.
    ( if set.is_empty(PublicChildren) then
        Int0FileNameGroups = []
    else
        Int0FileNameGroups =
            [make_singleton_file_name_group("int0", Int0FileName)]
    ),
    make_module_file_name_group_with_ext(Globals,
        "ancestors", ext_cur_ngs(ext_cur_ngs_int_int0),
        Ancestors, AncestorSourceGroups, !Cache, !IO),
    make_module_file_name_group_with_ext(Globals,
        "direct deps", ext_cur_ngs(ext_cur_ngs_int_int1),
        DirectDeps, DirectDepsSourceGroups, !Cache, !IO),
    make_module_file_name_group_with_ext(Globals,
        "indirect deps", ext_cur_ngs(ext_cur_ngs_int_int2),
        IndirectDeps, IndirectDepsSourceGroups, !Cache, !IO),
    make_module_file_name_group_with_ext(Globals,
        "type_repn self dep", ext_cur_ngs(ext_cur_ngs_int_int1),
        set.make_singleton_set(ModuleName), TypeRepnSelfDepGroups,
        !Cache, !IO),
    make_module_file_name_group_with_ext(Globals,
        "type_repn ancestor dep", ext_cur_ngs(ext_cur_ngs_int_int1),
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
        DirectDepsSourceGroups ++ IndirectDepsSourceGroups ++
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
        NestedOtherExts =
            [ext_cur_ngs_gs(ext_cur_ngs_gs_opt_date_plain),
            ext_cur_ngs_gs(ext_cur_ngs_gs_opt_date_trans),
            ext_cur_ngs_gs(ext_cur_ngs_gs_target_date_c),
            % XXX C# We would need this to support targeting C# with mmake.
            % ext_target_date(ext_target_date_cs),
            ext_cur_ngs_gs(ext_cur_ngs_gs_target_date_java)],
        list.map_foldl2(
            gather_nested_deps(Globals, ModuleName, NestedModuleNames),
            NestedOtherExts, MmakeRulesNestedDeps, !Cache, !IO)
    ).

%---------------------------------------------------------------------------%

:- pred construct_intermod_rules(globals::in,
    intermod_deps::in, set(module_name)::in,
    string::in, string::in, string::in, string::in, string::in,
    list(mmake_entry)::out,
    module_file_name_cache::in, module_file_name_cache::out,
    io::di, io::uo) is det.

construct_intermod_rules(Globals, IntermodDeps, AllDeps,
        ErrFileName, TransOptDateFileName, CDateFileName, JavaDateFileName,
        ObjFileName, MmakeRulesIntermod, !Cache, !IO) :-
    IntermodDeps = intermod_deps(MaybeMhDeps, MaybeOptFileDeps),
    (
        MaybeMhDeps = intermod_mh_deps,
        make_module_file_names_with_ext(Globals,
            ext_cur_pgs_max_cur(ext_cur_pgs_max_cur_mh),
            set.to_sorted_list(AllDeps), AllDepsFileNames, !Cache, !IO),
        MmakeRuleMhDeps = mmake_simple_rule("machine_dependent_header_deps",
            mmake_rule_is_not_phony,
            ObjFileName,
            AllDepsFileNames,
            []),
        MmakeRulesMhDeps = [MmakeRuleMhDeps]
    ;
        MaybeMhDeps = no_intermod_mh_deps,
        MmakeRulesMhDeps = []
    ),
    (
        MaybeOptFileDeps = opt_file_deps(PlainOptDeps, MaybeTransOptDeps),

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
        Targets = one_or_more(TransOptDateFileName,
            [ErrFileName, CDateFileName, JavaDateFileName]),
        PlainOptInt0Deps =
            set.union_list(list.map(get_ancestors_set, PlainOptDeps)),
        % XXX LEGACY
        make_module_file_names_with_ext(Globals,
            ext_cur_ngs_gs_max_ngs(ext_cur_ngs_gs_max_ngs_legacy_opt_plain),
            PlainOptDeps, PlainOptDepsFileNames, !Cache, !IO),
        make_module_file_names_with_ext(Globals,
            ext_cur_ngs(ext_cur_ngs_int_int0),
            set.to_sorted_list(PlainOptInt0Deps), PlainOptInt0DepsFileNames,
            !Cache, !IO),
        MmakeRuleDateOptInt0Deps = mmake_flat_rule("dates_on_opts_and_int0s",
            mmake_rule_is_not_phony,
            Targets,
            PlainOptDepsFileNames ++ PlainOptInt0DepsFileNames,
            []),

        (
            MaybeTransOptDeps = yes(TransOptDeps),
            ErrDateTargets = one_or_more(ErrFileName,
                [CDateFileName, JavaDateFileName]),
            % XXX LEGACY
            make_module_file_names_with_ext(Globals,
                ext_cur_ngs_gs_max_ngs(
                    ext_cur_ngs_gs_max_ngs_legacy_opt_trans),
                TransOptDeps, TransOptDepsOptFileNames, !Cache, !IO),
            MmakeRuleTransOptOpts = mmake_flat_rule("dates_on_trans_opts",
                mmake_rule_is_not_phony,
                ErrDateTargets,
                TransOptDepsOptFileNames,
                []),
            MmakeRulesIntermod = MmakeRulesMhDeps ++
                [MmakeRuleDateOptInt0Deps, MmakeRuleTransOptOpts]
        ;
            MaybeTransOptDeps = no,
            MmakeRulesIntermod = MmakeRulesMhDeps ++ [MmakeRuleDateOptInt0Deps]
        )
    ;
        MaybeOptFileDeps = no_opt_file_deps,
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
        make_module_file_names_with_ext(Globals,
            ext_cur_ngs_gs_max_cur(ext_cur_ngs_gs_max_cur_mih),
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
        ext_cur_pgs_max_cur(ext_cur_pgs_max_cur_mh),
        ModuleName, MhHeaderFileName, !Cache, !IO),
    make_module_file_name(Globals, $pred,
        ext_cur_ngs_gs_max_cur(ext_cur_ngs_gs_max_cur_mih),
        ModuleName, MihHeaderFileName, !Cache, !IO),
    MmakeRuleMhMihOnC = mmake_flat_rule("mh_and_mih_on_c",
        mmake_rule_is_not_phony,
        one_or_more(MhHeaderFileName, [MihHeaderFileName]),
        [CFileName],
        []),
    MmakeRulesCHeaders = MmakeRulesObjOnMihs ++ [MmakeRuleMhMihOnC].

%---------------------%

    % The `.module_dep' file is made as a side effect of
    % creating the `.c' or `.java' file.
    % XXX What about C#?
    % (See the main comment on generate_d_file above).
    %
:- pred construct_module_dep_fragment(globals::in, module_name::in,
    string::in, mmake_fragment::out,
    module_file_name_cache::in, module_file_name_cache::out,
    io::di, io::uo) is det.

construct_module_dep_fragment(Globals, ModuleName, CFileName,
        MmakeFragmentModuleDep, !Cache, !IO) :-
    make_module_file_name(Globals, $pred,
        ext_cur_ngs_gs_java(ext_cur_ngs_gs_java_java),
        ModuleName, JavaFileName, !Cache, !IO),
    make_module_file_name(Globals, $pred,
        ext_cur_ngs(ext_cur_ngs_misc_module_dep),
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

    % The .date and .date0 files depend on
    %
    % - the .int0 files for the parent modules, and
    % - the .int3 files for the directly and indirectly imported modules.
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
        Date0FileName, DateFileName, Ancestors, DirectDeps, IndirectDeps,
        MmakeRulesParentDates, !Cache, !IO) :-
    make_module_file_names_with_ext(Globals,
        ext_cur_ngs(ext_cur_ngs_int_date_int12),
        set.to_sorted_list(Ancestors), AncestorDateFileNames, !Cache, !IO),
    make_module_file_names_with_ext(Globals,
        ext_cur_ngs(ext_cur_ngs_int_int0),
        set.to_sorted_list(Ancestors), AncestorInt0FileNames, !Cache, !IO),
    make_module_file_names_with_ext(Globals,
        ext_cur_ngs(ext_cur_ngs_int_int3),
        set.to_sorted_list(DirectDeps), DirectDepInt3FileNames, !Cache, !IO),
    make_module_file_names_with_ext(Globals,
        ext_cur_ngs(ext_cur_ngs_int_int3),
        set.to_sorted_list(IndirectDeps), IndirectDepInt3FileNames,
        !Cache, !IO),

    MmakeRuleParentDates = mmake_general_rule("self_and_parent_date_deps",
        mmake_rule_is_not_phony,
        one_or_more(
            mmake_file_name_group("date files",
                one_or_more(DateFileName,
                    [Date0FileName | AncestorDateFileNames])),
            []),
        [make_singleton_file_name_group("source", SourceFileName)] ++
            make_file_name_group("ancestor int0", AncestorInt0FileNames) ++
            make_file_name_group("direct dep int3s", DirectDepInt3FileNames) ++
            make_file_name_group("indirect dep int3s",
                IndirectDepInt3FileNames),
        []),
    make_module_file_names_with_ext(Globals,
        ext_cur_ngs(ext_cur_ngs_int_date_int0),
        set.to_sorted_list(Ancestors), AncestorDate0FileNames, !Cache, !IO),
    MmakeRuleParentDate0s = mmake_general_rule("self_and_parent_date0_deps",
        mmake_rule_is_not_phony,
        one_or_more(
            mmake_file_name_group("date0s",
                one_or_more(Date0FileName, AncestorDate0FileNames)),
            []),
        [make_singleton_file_name_group("source", SourceFileName)] ++
            make_file_name_group("direct dep int3s", DirectDepInt3FileNames) ++
            make_file_name_group("indirect dep int3s",
                IndirectDepInt3FileNames),
        []),
    MmakeRulesParentDates = [MmakeRuleParentDates, MmakeRuleParentDate0s].

%---------------------%

    % Handle dependencies introduced by
    % `:- pragma foreign_import_module' declarations.
    %
:- pred construct_foreign_import_rules(globals::in, parse_tree_module_src::in,
    set(module_name)::in, string::in, string::in, list(mmake_entry)::out,
    module_file_name_cache::in, module_file_name_cache::out,
    io::di, io::uo) is det.

construct_foreign_import_rules(Globals, ParseTreeModuleSrc,
        ForeignImportedModuleNamesSet,
        ObjFileName, PicObjFileName, MmakeRulesForeignImports, !Cache, !IO) :-
    ModuleName = ParseTreeModuleSrc ^ ptms_module_name,
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
            ForeignImportExt = ext_cur_pgs_max_cur(ext_cur_pgs_max_cur_mh),
            gather_foreign_import_deps(Globals, ForeignImportExt,
                ForeignImportTargets, ForeignImportedModuleNames,
                MmakeRuleForeignImports, !Cache, !IO),
            MmakeRulesForeignImports = [MmakeRuleForeignImports]
        ;
            Target = target_java,
            make_module_file_name(Globals, $pred,
                ext_cur_ngs_gs_java(ext_cur_ngs_gs_java_class),
                ModuleName, ClassFileName, !Cache, !IO),
            ForeignImportTargets = one_or_more(ClassFileName, []),
            ForeignImportExt = ext_cur_ngs_gs_java(ext_cur_ngs_gs_java_java),
            gather_foreign_import_deps(Globals, ForeignImportExt,
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
    make_module_file_name(Globals, $pred, ext_cur_ngs(ext_cur_ngs_int_int1),
        ModuleName, IntFileName, !Cache, !IO),
    make_module_file_name(Globals, $pred, ext_cur_ngs(ext_cur_ngs_int_int2),
        ModuleName, Int2FileName, !Cache, !IO),
    make_module_file_name(Globals, $pred, ext_cur_ngs(ext_cur_ngs_int_int3),
        ModuleName, Int3FileName, !Cache, !IO),
    % XXX LEGACY
    make_module_file_name(Globals, $pred,
        ext_cur_ngs_gs_max_ngs(ext_cur_ngs_gs_max_ngs_legacy_opt_plain),
        ModuleName, OptFileName, !Cache, !IO),
    % XXX LEGACY
    make_module_file_name(Globals, $pred,
        ext_cur_ngs_gs_max_ngs(ext_cur_ngs_gs_max_ngs_legacy_opt_trans),
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
    globals.get_subdir_setting(Globals, SubdirSetting),
    (
        SubdirSetting = use_cur_dir,
        MmakeRulesSubDirShorthand = []
    ;
        ( SubdirSetting = use_cur_ngs_subdir
        ; SubdirSetting = use_cur_ngs_gs_subdir
        ),
        SubDirShorthandOtherExts =
            [ext_cur_ngs_gs(ext_cur_ngs_gs_target_c),
            ext_cur_ngs_gas(ext_cur_ngs_gas_obj_dollar_o),
            ext_cur_ngs_gas(ext_cur_ngs_gas_obj_pic_o),
            ext_cur_ngs_gs_java(ext_cur_ngs_gs_java_java),
            ext_cur_ngs_gs_java(ext_cur_ngs_gs_java_class),
            ext_cur_gs(ext_cur_gs_lib_cil_dll)],
        list.map_foldl2(
            construct_subdirs_shorthand_rule(Globals, ModuleName),
            SubDirShorthandOtherExts, MmakeRulesSubDirShorthand, !Cache, !IO)
    ).

%---------------------%

:- pred construct_any_needed_pattern_rules(maybe_found::in,
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
        HaveMap = found,
        module_name_to_file_name_stem(SourceFileModuleName, ModuleArg)
    ;
        HaveMap = not_found,
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

:- pred gather_nested_deps(globals::in, module_name::in, list(module_name)::in,
    ext::in, mmake_entry::out,
    module_file_name_cache::in, module_file_name_cache::out,
    io::di, io::uo) is det.

gather_nested_deps(Globals, ModuleName, NestedDeps, Ext, MmakeRule,
        !Cache, !IO) :-
    make_module_file_name(Globals, $pred, Ext,
        ModuleName, ModuleExtName, !Cache, !IO),
    make_module_file_names_with_ext(Globals, Ext,
        NestedDeps, NestedDepsFileNames, !Cache, !IO),
    ExtStr = extension_to_string(Globals, Ext),
    MmakeRule = mmake_simple_rule("nested_deps_for_" ++ ExtStr,
        mmake_rule_is_not_phony,
        ModuleExtName,
        NestedDepsFileNames,
        []).

:- pred gather_foreign_import_deps(globals::in, ext::in,
    one_or_more(string)::in, list(module_name)::in, mmake_entry::out,
    module_file_name_cache::in, module_file_name_cache::out,
    io::di, io::uo) is det.

gather_foreign_import_deps(Globals, ForeignImportExt, ForeignImportTargets,
        ForeignImportedModuleNames, MmakeRule, !Cache, !IO) :-
    make_module_file_names_with_ext(Globals, ForeignImportExt,
        ForeignImportedModuleNames, ForeignImportedFileNames, !Cache, !IO),
    ForeignImportExtStr = extension_to_string(Globals,
        ForeignImportExt),
    RuleName = "foreign_deps_for_" ++
        string.remove_prefix_if_present(".", ForeignImportExtStr),
    MmakeRule = mmake_flat_rule(RuleName,
        mmake_rule_is_not_phony,
        ForeignImportTargets,
        ForeignImportedFileNames,
        []).

%---------------------------------------------------------------------------%

:- func foreign_include_file_path_name(file_name, foreign_include_file_info)
    = string.

foreign_include_file_path_name(SourceFileName, IncludeFile) = IncludePath :-
    IncludeFile = foreign_include_file_info(_Lang, IncludeFileName),
    make_include_file_path(SourceFileName, IncludeFileName, IncludePath).

    % With `--use-subdirs', allow users to type `mmake module.c'
    % rather than `mmake Mercury/cs/module.c'.
    %
:- pred construct_subdirs_shorthand_rule(globals::in, module_name::in,
    ext::in, mmake_entry::out,
    module_file_name_cache::in, module_file_name_cache::out,
    io::di, io::uo) is det.

construct_subdirs_shorthand_rule(Globals, ModuleName, Ext,
        MmakeRule, !Cache, !IO) :-
    module_name_to_file_name_stem(ModuleName, ModuleStr),
    make_module_file_name(Globals, $pred, Ext, ModuleName, Target,
        !Cache, !IO),
    ExtStr = extension_to_string(Globals, Ext),
    ShorthandTarget = ModuleStr ++ ExtStr,
    MmakeRule = mmake_simple_rule("subdir_shorthand_for_" ++ ExtStr,
        mmake_rule_is_phony, ShorthandTarget, [Target], []).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

generate_dv_mmakefile(Globals, SourceFileName, ModuleName, DepsMap,
        MmakeFile, !Cache, !IO) :-
    ModuleNameString = sym_name_to_string(ModuleName),
    library.version(Version, FullArch),
    MmakeStartComment = mmake_start_comment("dependency variables",
        ModuleNameString, SourceFileName, Version, FullArch),

    map.keys(DepsMap, Modules0),
    select_no_fatal_error_modules(DepsMap, Modules0, Modules1),
    list.sort(compare_module_names, Modules1, Modules),

    module_name_to_make_var_name(ModuleName, ModuleMakeVarName),
    list.map(get_source_file(DepsMap), Modules, SourceFiles0),
    list.sort_and_remove_dups(SourceFiles0, SourceFiles),

    MmakeVarModuleMs = mmake_var_defn_list(ModuleMakeVarName ++ ".ms",
        list.map(add_suffix(".m"), SourceFiles)),

    MmakeVarModuleDepErrs = mmake_var_defn_list(
        ModuleMakeVarName ++ ".dep_errs",
        list.map(add_suffix(".dep_err"), SourceFiles)),

    MmakeVarModuleErrs = mmake_var_defn_list(ModuleMakeVarName ++ ".errs",
        list.map(add_suffix(".err"), SourceFiles)),

    StartFragments = list.map(mmake_entry_to_fragment,
        [MmakeStartComment, MmakeVarModuleMs,
        MmakeVarModuleDepErrs, MmakeVarModuleErrs]),

    % The call to generate_dv_file_define_mod_misc_vars must come *before*
    % any calls that generate rules that refer to the make variables
    % it defines, which effectively means it must be first.
    % (None of the other predicates called in this block of code define
    % any variables used by any other such predicate.)
    generate_dv_file_define_mod_misc_vars(Globals, DepsMap, Modules,
        ModuleMakeVarName, ModMiscFragments),
    generate_dv_file_define_intn_vars(ModuleMakeVarName, IntnFragments),
    generate_dv_file_define_opt_vars(ModuleMakeVarName, OptFragments),
    generate_dv_file_define_c_vars(Globals, DepsMap, Modules,
        ModuleMakeVarName, CFragments, !Cache, !IO),
    generate_dv_file_define_java_vars(ModuleMakeVarName, JavaFragments),
    generate_dv_file_define_csharp_vars(ModuleMakeVarName, CsharpFragments),
    generate_dv_file_define_smart_recomp_vars(ModuleMakeVarName,
        SmartRecompFragments),

    MmakeFile =
        cord.from_list(StartFragments) ++
        cord.from_list(ModMiscFragments) ++
        cord.from_list(IntnFragments) ++
        cord.from_list(OptFragments) ++
        cord.from_list(CFragments) ++
        cord.from_list(JavaFragments) ++
        cord.from_list(CsharpFragments) ++
        cord.from_list(SmartRecompFragments).

%---------------------%

:- pred select_no_fatal_error_modules(deps_map::in,
    list(module_name)::in, list(module_name)::out) is det.

select_no_fatal_error_modules(_, [], []).
select_no_fatal_error_modules(DepsMap, [ModuleName | ModuleNames0],
        ModuleNames) :-
    select_no_fatal_error_modules(DepsMap, ModuleNames0, ModuleNamesTail),
    map.lookup(DepsMap, ModuleName, deps(_, _, BurdenedModule)),
    Baggage = BurdenedModule ^ bm_baggage,
    ModuleErrors = Baggage ^ mb_errors,
    FatalErrors = ModuleErrors ^ rm_fatal_errors,
    ( if set.is_empty(FatalErrors) then
        ModuleNames = [ModuleName | ModuleNamesTail]
    else
        ModuleNames = ModuleNamesTail
    ).

:- pred compare_module_names(module_name::in, module_name::in,
    comparison_result::out) is det.

compare_module_names(Sym1, Sym2, Result) :-
    Str1 = sym_name_to_string(Sym1),
    Str2 = sym_name_to_string(Sym2),
    compare(Result, Str1, Str2).

%---------------------------------------------------------------------------%

:- pred generate_dv_file_define_mod_misc_vars(globals::in, deps_map::in,
    list(module_name)::in, string::in, list(mmake_fragment)::out) is det.

generate_dv_file_define_mod_misc_vars(Globals, DepsMap, Modules,
        ModuleMakeVarName, ModMiscFragments) :-
    ModuleNameStrs = list.map(sym_name_to_string, Modules),
    MmakeVarModuleMods = mmake_var_defn_list(ModuleMakeVarName ++ ".mods",
        ModuleNameStrs),

    % The modules for which we need to generate .int0 files.
    HasSubmodules =
        ( pred(Module::in) is semidet :-
            map.lookup(DepsMap, Module, deps(_, _, BurdenedModule)),
            ParseTreeModuleSrc = BurdenedModule ^ bm_module,
            IncludeMap = ParseTreeModuleSrc ^ ptms_include_map,
            not map.is_empty(IncludeMap)
        ),
    list.filter(HasSubmodules, Modules, ModulesWithSubmodules),

    ModuleWithSubmodulesNameStrs =
        list.map(sym_name_to_string, ModulesWithSubmodules),
    MmakeVarModuleParentMods = mmake_var_defn_list(
        ModuleMakeVarName ++ ".parent_mods",
        ModuleWithSubmodulesNameStrs),

    MmakeVarDs = mmake_var_defn(ModuleMakeVarName ++ ".ds",
        string.format("$(%s.mods:%%=$(ds_subdir)%%.d)",
            [s(ModuleMakeVarName)])),

    ModuleDepFileExtStr = extension_to_string(Globals,
        ext_cur_ngs(ext_cur_ngs_misc_module_dep)),
    MmakeVarModuleDeps = mmake_var_defn(ModuleMakeVarName ++ ".module_deps",
        string.format("$(%s.mods:%%=$(module_deps_subdir)%%%s)",
            [s(ModuleMakeVarName), s(ModuleDepFileExtStr)])),

    MmakeVarProfs = mmake_var_defn(ModuleMakeVarName ++ ".profs",
        string.format("$(%s.mods:%%=%%.prof)",
            [s(ModuleMakeVarName)])),

    ModMiscFragments = list.map(mmake_entry_to_fragment,
        [MmakeVarModuleMods, MmakeVarModuleParentMods,
        MmakeVarDs, MmakeVarModuleDeps, MmakeVarProfs]).

%---------------------%

:- pred generate_dv_file_define_intn_vars(string::in,
    list(mmake_fragment)::out) is det.

generate_dv_file_define_intn_vars(ModuleMakeVarName, IntnFragments) :-
    MmakeVarInt1s = mmake_var_defn_list(ModuleMakeVarName ++ ".int1s",
        [string.format("$(%s.mods:%%=$(int1s_subdir)%%.int)",
            [s(ModuleMakeVarName)])]),
    MmakeVarInt2s = mmake_var_defn_list(ModuleMakeVarName ++ ".int2s",
        [string.format("$(%s.mods:%%=$(int2s_subdir)%%.int2)",
            [s(ModuleMakeVarName)])]),
    MmakeVarInts = mmake_var_defn_list(ModuleMakeVarName ++ ".ints",
        % This is intentionally ints_subdir, not int1s_subdir.
        [string.format("$(%s.mods:%%=$(ints_subdir)%%.int)",
            [s(ModuleMakeVarName)]),
        string.format("$(%s.mods:%%=$(int2s_subdir)%%.int2)",
            [s(ModuleMakeVarName)])]),
    MmakeVarInt3s = mmake_var_defn(ModuleMakeVarName ++ ".int3s",
        string.format("$(%s.mods:%%=$(int3s_subdir)%%.int3)",
            [s(ModuleMakeVarName)])),
    % `.int0' files are only generated for modules with submodules.
    % XXX Once, we also generated .int0 files for nested submodules that
    % don't have any children, but this bug seems to have been fixed.
    MmakeVarInt0s = mmake_var_defn(ModuleMakeVarName ++ ".int0s",
        string.format("$(%s.parent_mods:%%=$(int0s_subdir)%%.int0)",
            [s(ModuleMakeVarName)])),
    % XXX The `<module>.int0s_to_clean' variable is like `<module>.int0s'
    % except that it contains .int0 files for all modules, regardless of
    % whether they should have been created or not. It is used by the rule for
    % `mmake realclean' to ensure that we clean up all the .int0 files,
    % including the ones that were accidentally created by the bug described
    % above.
    MmakeVarInt0sToClean = mmake_var_defn(
        ModuleMakeVarName ++ ".int0s_to_clean",
        string.format("$(%s.mods:%%=$(int0s_subdir)%%.int0)",
            [s(ModuleMakeVarName)])),
    % The deprecated old version of .int0s_to_clean.
    MmakeVarAllInt0s = mmake_var_defn(ModuleMakeVarName ++ ".all_int0s",
        string.format("$(%s.mods:%%=$(int0s_subdir)%%.int0)",
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

    IntnFragments = list.map(mmake_entry_to_fragment,
        [MmakeVarInt1s, MmakeVarInt2s, MmakeVarInts,
        MmakeVarInt0s, MmakeVarInt0sToClean, MmakeVarAllInt0s,
        MmakeVarInt3s,
        MmakeVarDates, MmakeVarDate0s, MmakeVarDate3s]).

%---------------------%

:- pred generate_dv_file_define_opt_vars(string::in,
    list(mmake_fragment)::out) is det.

generate_dv_file_define_opt_vars(ModuleMakeVarName, OptFragments) :-
    MmakeVarAllOpts = mmake_var_defn(ModuleMakeVarName ++ ".all_opts",
        string.format("$(%s.mods:%%=$(opts_subdir)%%.opt)",
            [s(ModuleMakeVarName)])),
    % The deprecated old version of .all_opts.
    MmakeVarOpts = mmake_var_defn(ModuleMakeVarName ++ ".opts",
        string.format("$(%s.mods:%%=$(opts_subdir)%%.opt)",
            [s(ModuleMakeVarName)])),
    MmakeVarAllTransOpts = mmake_var_defn(
        ModuleMakeVarName ++ ".all_trans_opts",
        string.format("$(%s.mods:%%=$(trans_opts_subdir)%%.trans_opt)",
            [s(ModuleMakeVarName)])),
    % The deprecated old version of .all_trans_opts.
    MmakeVarTransOpts = mmake_var_defn(ModuleMakeVarName ++ ".trans_opts",
        string.format("$(%s.mods:%%=$(trans_opts_subdir)%%.trans_opt)",
            [s(ModuleMakeVarName)])),

    MmakeVarOptDates = mmake_var_defn(ModuleMakeVarName ++ ".optdates",
        string.format("$(%s.mods:%%=$(optdates_subdir)%%.optdate)",
            [s(ModuleMakeVarName)])),
    MmakeVarTransOptDates =
        mmake_var_defn(ModuleMakeVarName ++ ".trans_opt_dates",
            string.format(
                "$(%s.mods:%%=$(trans_opt_dates_subdir)%%.trans_opt_date)",
                [s(ModuleMakeVarName)])),

    OptFragments = list.map(mmake_entry_to_fragment,
        [MmakeVarAllOpts, MmakeVarOpts,
        MmakeVarAllTransOpts, MmakeVarTransOpts,
        MmakeVarOptDates, MmakeVarTransOptDates]).

%---------------------%

:- pred generate_dv_file_define_c_vars(globals::in, deps_map::in,
    list(module_name)::in, string::in, list(mmake_fragment)::out,
    module_file_name_cache::in, module_file_name_cache::out,
    io::di, io::uo) is det.

generate_dv_file_define_c_vars(Globals, DepsMap, Modules, ModuleMakeVarName,
        CFragments, !Cache, !IO) :-
    MmakeVarAllCs = mmake_var_defn(ModuleMakeVarName ++ ".all_cs",
        string.format("$(%s.mods:%%=$(cs_subdir)%%.c)",
            [s(ModuleMakeVarName)])),
    MmakeVarInitCs = mmake_var_defn(ModuleMakeVarName ++ ".init_cs",
        string.format("$(%s.mods:%%=$(cs_subdir)%%.c)",
            [s(ModuleMakeVarName)])),
    MmakeVarCDates = mmake_var_defn(ModuleMakeVarName ++ ".c_dates",
        string.format("$(%s.mods:%%=$(c_dates_subdir)%%.c_date)",
            [s(ModuleMakeVarName)])),

    get_fact_table_file_names(DepsMap, Modules, FactTableFileNames),
    % XXX LEGACY
    list.map2(
        fact_table_file_name(Globals, $pred,
            ext_cur_ngs_gas(ext_cur_ngs_gas_obj_dollar_o)),
        FactTableFileNames,
        FactTableFileNamesOs, _FactTableFileNamesOsProposed),
    list.map2(
        fact_table_file_name(Globals, $pred,
            ext_cur_ngs_gas(ext_cur_ngs_gas_obj_dollar_efpo)),
        FactTableFileNames,
        FactTableFileNamesPicOs, _FactTableFileNamesPicOsProposed),

    MmakeVarAllOs = mmake_var_defn_list(ModuleMakeVarName ++ ".all_os",
        [string.format("$(%s.mods:%%=$(os_subdir)%%.$O)",
            [s(ModuleMakeVarName)]) |
        FactTableFileNamesOs]),
    MmakeVarAllPicOs = mmake_var_defn_list(ModuleMakeVarName ++ ".all_pic_os",
        [string.format("$(%s.mods:%%=$(os_subdir)%%.$(EXT_FOR_PIC_OBJECTS))",
            [s(ModuleMakeVarName)]) |
        FactTableFileNamesPicOs]),

    % This used to be (the equivalent of):
    % generate_dv_file_define_c_foreign_vars(Globals, ModuleMakeVarName,
    %   [MmakeVarForeignModules, MmakeVarForeignFileNames], !Cache, !IO).
    MmakeVarForeignModules =
        mmake_var_defn_list(ModuleMakeVarName ++ ".foreign", []),
    MmakeVarForeignFileNames =
        mmake_var_defn_list(ModuleMakeVarName ++ ".foreign_cs", []),

    globals.get_target(Globals, Target),
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
            [string.format("$(%s.mods:%%=$(mhs_subdir)%%.mh)",
                [s(ModuleMakeVarName)])]
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
    MmakeVarMhs =
        mmake_var_defn_list(ModuleMakeVarName ++ ".mhs", MhSources),

    % The `<module>.mihs_to_clean' variable is like `<module>.mihs' except that
    % it contains header files for all the modules, regardless of the grade
    % or --target option. It is used by the rule for `mmake realclean',
    % which should remove anything that could have been automatically
    % generated, even if the grade or --target option has changed.
    MmakeVarMihsToClean = mmake_var_defn(ModuleMakeVarName ++ ".mihs_to_clean",
        string.format("$(%s.mods:%%=$(mihs_subdir)%%.mih)",
            [s(ModuleMakeVarName)])),
    % The deprecated old version of .mihs_to_clean.
    MmakeVarAllMihs = mmake_var_defn(ModuleMakeVarName ++ ".all_mihs",
        string.format("$(%s.mods:%%=$(mihs_subdir)%%.mih)",
            [s(ModuleMakeVarName)])),

    % The `<module>.all_mhs' variable is like `<module>.mhs' except that
    % it contains header files for all the modules, as for `<module>.all_mihs'
    % above.
    MmakeVarMhsToClean = mmake_var_defn(ModuleMakeVarName ++ ".mhs_to_clean",
        string.format("$(%s.mods:%%=$(mhs_subdir)%%.mh)",
            [s(ModuleMakeVarName)])),
    % The deprecated old version of .mhs_to_clean.
    MmakeVarAllMhs = mmake_var_defn(ModuleMakeVarName ++ ".all_mhs",
        string.format("$(%s.mods:%%=$(mhs_subdir)%%.mh)",
            [s(ModuleMakeVarName)])),

    CFragments = list.map(mmake_entry_to_fragment,
        [MmakeVarAllCs, MmakeVarInitCs, MmakeVarCDates,
        MmakeVarAllOs, MmakeVarAllPicOs,
        MmakeVarForeignModules, MmakeVarForeignFileNames,
        MmakeVarMihs, MmakeVarMhs,
        MmakeVarMihsToClean, MmakeVarAllMihs,
        MmakeVarMhsToClean, MmakeVarAllMhs]).

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
    acc_fact_table_file_names(DepsMap, Modules,
        set.init, FactTableFileNamesSet),
    set.to_sorted_list(FactTableFileNamesSet, FactTableFileNames).

    % Gather file names of fact tables.
    %
:- pred acc_fact_table_file_names(deps_map::in, list(module_name)::in,
    set(file_name)::in, set(file_name)::out) is det.

acc_fact_table_file_names(_DepsMap, [], !FactTableFileNames).
acc_fact_table_file_names(DepsMap, [Module | Modules], !FactTableFileNames) :-
    map.lookup(DepsMap, Module, deps(_, _, BurdenedModule)),
    ParseTreeModuleSrc = BurdenedModule ^ bm_module,
    get_fact_tables(ParseTreeModuleSrc, FactTableFileNames),
    % Handle object files for foreign code.
    % NOTE: currently none of the backends support foreign code
    % in a non target language.
    set.union(FactTableFileNames, !FactTableFileNames),
    acc_fact_table_file_names(DepsMap, Modules, !FactTableFileNames).

%---------------------%

    % XXX Maybe not needed. We don't support building Java executables
    % using mmake, but the support for building Java executables via
    % mmc --make relies on *some* parts of what we generate here.
    % It would be nice if it was documented *which* parts are needed,
    % and which were added in the hope of future support that never
    % actually materialized.
    %
:- pred generate_dv_file_define_java_vars(string::in,
    list(mmake_fragment)::out) is det.

generate_dv_file_define_java_vars(ModuleMakeVarName, JavaFragments) :-
    MmakeVarAllJavas = mmake_var_defn(ModuleMakeVarName ++ ".all_javas",
        string.format("$(%s.mods:%%=$(javas_subdir)%%.java)",
            [s(ModuleMakeVarName)])),
    MmakeVarJavaDates = mmake_var_defn(ModuleMakeVarName ++ ".java_dates",
        string.format("$(%s.mods:%%=$(java_dates_subdir)%%.java_date)",
            [s(ModuleMakeVarName)])),
    % The Java compiler creates a .class file for each class within the
    % original .java file. The filenames of all these can be matched with
    % `module\$*.class', hence the "\\$$*.class" below.
    % If no such files exist, Make will use the pattern verbatim,
    % so we enclose the pattern in a `wildcard' function to prevent this.
    %
    % Evaluating the .classes variable can be slow, so we make its definition
    % conditional on the grade.
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

    JavaFragments =
        [mmake_entry_to_fragment(MmakeVarAllJavas),
        mmake_entry_to_fragment(MmakeVarJavaDates),
        MmakeFragmentVarClasses].

%---------------------%

    % XXX Probably not needed, since we don't support building C# executables
    % using mmake.
    %
:- pred generate_dv_file_define_csharp_vars(string::in,
    list(mmake_fragment)::out) is det.

generate_dv_file_define_csharp_vars(ModuleMakeVarName, CsharpFragments) :-
    MmakeVarAllCss = mmake_var_defn(ModuleMakeVarName ++ ".all_css",
        string.format("$(%s.mods:%%=$(css_subdir)%%.cs)",
            [s(ModuleMakeVarName)])),
    MmakeVarCsDates = mmake_var_defn(ModuleMakeVarName ++ ".cs_dates",
        string.format("$(%s.mods:%%=$(cs_dates_subdir)%%.cs_date)",
            [s(ModuleMakeVarName)])),
    MmakeVarDlls = mmake_var_defn(ModuleMakeVarName ++ ".dlls",
        string.format("$(%s.mods:%%=$(dlls_subdir)%%.dll)",
            [s(ModuleMakeVarName)])),
    % The dlls that contain the foreign_code.
    MmakeVarForeignDlls = mmake_var_defn(ModuleMakeVarName ++ ".foreign_dlls",
        string.format("$(%s.foreign:%%=$(dlls_subdir)%%.dll)",
            [s(ModuleMakeVarName)])),

%---------------------%

    CsharpFragments = list.map(mmake_entry_to_fragment,
        [MmakeVarAllCss, MmakeVarCsDates,
        MmakeVarDlls, MmakeVarForeignDlls]).

:- pred generate_dv_file_define_smart_recomp_vars(string::in,
    list(mmake_fragment)::out) is det.

generate_dv_file_define_smart_recomp_vars(ModuleMakeVarName,
        SmartRecompFragments) :-
    MmakeVarUseds = mmake_var_defn(ModuleMakeVarName ++ ".useds",
        string.format("$(%s.mods:%%=$(useds_subdir)%%.used)",
            [s(ModuleMakeVarName)])),
    MmakeVarAnalysiss = mmake_var_defn(ModuleMakeVarName ++ ".analyses",
        string.format("$(%s.mods:%%=$(analyses_subdir)%%.analysis)",
            [s(ModuleMakeVarName)])),
    MmakeVarRequests = mmake_var_defn(ModuleMakeVarName ++ ".requests",
        string.format("$(%s.mods:%%=$(requests_subdir)%%.request)",
            [s(ModuleMakeVarName)])),
    MmakeVarImdgs = mmake_var_defn(ModuleMakeVarName ++ ".imdgs",
        string.format("$(%s.mods:%%=$(imdgs_subdir)%%.imdg)",
            [s(ModuleMakeVarName)])),

    SmartRecompFragments = list.map(mmake_entry_to_fragment,
        [MmakeVarUseds, MmakeVarAnalysiss, MmakeVarRequests, MmakeVarImdgs]).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

generate_dep_mmakefile(Globals, SourceFileName, ModuleName, DepsMap,
        !:MmakeFile, !IO) :-
    ModuleNameString = sym_name_to_string(ModuleName),
    library.version(Version, FullArch),

    MmakeStartComment = mmake_start_comment("program dependencies",
        ModuleNameString, SourceFileName, Version, FullArch),

    module_name_to_make_var_name(ModuleName, ModuleMakeVarName),

    % XXX LEGACY
    module_name_to_file_name_return_dirs(Globals, $pred,
        ext_cur_gs(ext_cur_gs_lib_init), ModuleName,
        InitDirNames, _InitDirNamesProposed,
        InitFileName, _InitFileNameProposed),
    module_name_to_file_name_return_dirs(Globals, $pred,
        ext_cur_ngs_gs(ext_cur_ngs_gs_init_c), ModuleName,
        InitCDirNames, _InitCDirNamesProposed,
        InitCFileName, _InitCFileNameProposed),
    module_name_to_file_name_return_dirs(Globals, $pred,
        ext_cur_ngs_gas(ext_cur_ngs_gas_init_obj_dollar_o), ModuleName,
        InitObjDirNames, _InitObjDirNamesProposed,
        InitObjFileName, _InitObjFileNameProposed),
    module_name_to_file_name_return_dirs(Globals, $pred,
        ext_cur_ngs_gas(ext_cur_ngs_gas_init_obj_pic_o), ModuleName,
        InitPicObjDirNames, _InitPicObjDirNamesProposed,
        InitPicObjFileName, _InitPicObjFileNameProposed),
    % XXX Not creating the "bin" directory for the executable,
    % (and then probably not putting the executable there)
    % seems like an oversight.
    module_name_to_file_name(Globals, $pred,
        ext_cur_gas(ext_cur_gas_exec_noext), ModuleName,
        ExeFileName, _ExeFileNameProposed),
    module_name_to_lib_file_name_return_dirs(Globals, $pred, "lib",
        ext_cur_gas(ext_cur_gas_lib_dollar_a), ModuleName,
        StaticLibDirNames, _StaticLibDirNamesProposed,
        StaticLibFileName, _StaticLibFileNameProposed),
    module_name_to_lib_file_name_return_dirs(Globals, $pred, "lib",
        ext_cur_gas(ext_cur_gas_lib_dollar_efsl), ModuleName,
        SharedLibDirNames, _SharedLibDirNamesProposed,
        SharedLibFileName, _SharedLibFileNameProposed),
    % XXX Not creating the directory for the .jar file seems to be
    % the "incomplete" part of our "incomplete mmake support" for Java.
    module_name_to_file_name(Globals, $pred,
        ext_cur_gs(ext_cur_gs_lib_jar), ModuleName,
        JarFileName, _JarFileNameProposed),

    expect(unify(StaticLibDirNames, SharedLibDirNames), $pred,
        "StaticLibDirNames != SharedLibDirNames"),

    % This is the complete list of the directories that we construct when
    % we create .dep files, i.e. when "mmc --generate-dependencies" is run.
    % XXX This seems a strange set, because it contains the directories
    % that contain *some* of the files generated by the Mercury systerm,
    % but not *others*. It is my (zs's) guess that the reason why we
    % construct these directories here is that .init, .init_c, .init.o,
    % .init.pic_o, .a (or .lib etc) and .so (or .dylib etc) files
    % are constructed by programs other than mmc, which means that
    % we can tell them to put their output into a file with a specified name
    % in a specified directory, but we can't easily tell them to *construct*
    % that directory first if it does not exist already.
    DirNamesList = [InitDirNames, InitCDirNames,
        InitObjDirNames, InitPicObjDirNames, StaticLibDirNames],
    list.foldl(create_any_dirs_on_path, DirNamesList, !IO),

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
        OptsVar = "$(" ++ ModuleMakeVarName ++ ".all_opts)",
        MaybeOptsVar = [OptsVar],
        MaybeOptsVarSpace = OptsVar ++ " "
    ;
        Intermod = no,
        MaybeOptsVar = [],
        MaybeOptsVarSpace = ""
    ),
    (
        TransOpt = yes,
        TransOptsVar = "$(" ++ ModuleMakeVarName ++ ".all_trans_opts)",
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
        ExeFileName, JarFileName, StaticLibFileName, SharedLibFileName,
        !MmakeFile, !IO),
    generate_dep_file_init_targets(Globals, ModuleName, ModuleMakeVarName,
        InitCFileName, InitFileName, DepFileName, DvFileName, !MmakeFile),
    generate_dep_file_install_targets_legacy(ModuleName, DepsMap,
        ModuleMakeVarName, MmcMakeDeps, Intermod, TransOpt,
        MaybeModuleDepsVarPair, MaybeOptsVarPair, MaybeTransOptsVarPair,
        !MmakeFile),
    generate_dep_file_install_targets_proposed(Globals, DepsMap, ModuleName,
        ModuleMakeVarName, MmcMakeDeps, Intermod, TransOpt, !MmakeFile),
    generate_dep_file_collective_targets(ModuleName, ModuleMakeVarName,
        !MmakeFile),
    generate_dep_file_clean_targets(ModuleName, ModuleMakeVarName,
        ExeFileName, InitCFileName, InitObjFileName, InitPicObjFileName,
        InitFileName, StaticLibFileName, SharedLibFileName, JarFileName,
        DepFileName, DvFileName, !MmakeFile).

%---------------------%

:- pred generate_dep_file_exec_library_targets(globals::in,
    module_name::in, string::in, string::in, string::in,
    list(string)::in, list(string)::in,
    string::in, string::in, string::in, string::in,
    mmakefile::in, mmakefile::out, io::di, io::uo) is det.

generate_dep_file_exec_library_targets(Globals, ModuleName,
        ModuleMakeVarName, InitFileName, InitObjFileName,
        MaybeOptsVar, MaybeTransOptsVar,
        ExeFileName, JarFileName, StaticLibFileName, SharedLibFileName,
        !MmakeFile, !IO) :-
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
    % XXX The above comment seems to have suffered bit rot.

    ModuleMakeVarNameClasses = "$(" ++ ModuleMakeVarName ++ ".classes)",

    ModuleMakeVarNameOs = "$(" ++ ModuleMakeVarName ++ ".all_os)",
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

    LibTargetName = "lib" ++ sym_name_to_string(ModuleName),
    ModuleMakeVarNameInts = "$(" ++ ModuleMakeVarName ++ ".ints)",
    ModuleMakeVarNameInt3s = "$(" ++ ModuleMakeVarName ++ ".int3s)",
    IntsOptsInitVars = [ModuleMakeVarNameInts, ModuleMakeVarNameInt3s] ++
        MaybeOptsVar ++ MaybeTransOptsVar ++ [InitFileName],
    MmakeRuleLibTargetJava = mmake_simple_rule("lib_target_java",
        mmake_rule_is_phony,
        LibTargetName,
        [JarFileName | IntsOptsInitVars],
        []),
    MmakeRuleLibTargetNonJava = mmake_simple_rule("lib_target_non_java",
        mmake_rule_is_phony,
        LibTargetName,
        [StaticLibFileName, SharedLibFileName | IntsOptsInitVars],
        []),
    MmakeFragmentLibTarget = mmf_conditional_entry(
        mmake_cond_grade_has_component("java"),
        MmakeRuleLibTargetJava, MmakeRuleLibTargetNonJava),

    ModuleMakeVarNamePicOs = "$(" ++ ModuleMakeVarName ++ ".all_pic_os)",
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

    LibAction1 = "rm -f " ++ StaticLibFileName,
    LibAction2Line1 =
        "$(AR) $(ALL_ARFLAGS) $(AR_LIBFILE_OPT)" ++ StaticLibFileName ++
            " " ++ ModuleMakeVarNameOs ++ " \\",
    LibAction2Line2 = "\t" ++ All_MLObjs,
    LibAction3 = "$(RANLIB) $(ALL_RANLIBFLAGS) " ++ StaticLibFileName,
    MmakeRuleLib = mmake_simple_rule("lib",
        mmake_rule_is_not_phony,
        StaticLibFileName,
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

%---------------------%

:- pred generate_dep_file_init_targets(globals::in,
    module_name::in, string::in, string::in, string::in,
    string::out, string::out, mmakefile::in, mmakefile::out) is det.

generate_dep_file_init_targets(Globals, ModuleName, ModuleMakeVarName,
        InitCFileName, InitFileName, DepFileName, DvFileName, !MmakeFile) :-
    % XXX LEGACY
    module_name_to_file_name(Globals, $pred,
        ext_cur_ngs(ext_cur_ngs_mf_dep), ModuleName,
        DepFileName, _DepFileNameProposed),
    module_name_to_file_name(Globals, $pred,
        ext_cur_ngs(ext_cur_ngs_mf_dv), ModuleName,
        DvFileName, _DvFileNameProposed),

    ModuleMakeVarNameCs = "$(" ++ ModuleMakeVarName ++ ".all_cs)",
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

%---------------------%

:- type maybe_mmake_var == pair(list(string), string).

:- pred generate_dep_file_install_targets_legacy(module_name::in,
    deps_map::in, string::in, bool::in, bool::in, bool::in,
    maybe_mmake_var::in, maybe_mmake_var::in, maybe_mmake_var::in,
    mmakefile::in, mmakefile::out) is det.

generate_dep_file_install_targets_legacy(ModuleName, DepsMap,
        ModuleMakeVarName, MmcMakeDeps, Intermod, TransOpt,
        MaybeModuleDepsVarPair, MaybeOptsVarPair, MaybeTransOptsVarPair,
        !MmakeFile) :-
    % XXX LEGACY Note that we install the `.opt' and `.trans_opt' files
    % in two places: in the `lib/$(GRADE)/opts' directory, so
    % that mmc will find them, and also in the `ints' directory,
    % so that Mmake will find them. That is not ideal, but it works.

    MaybeOptsVarPair = MaybeOptsVar - MaybeOptsVarSpace,
    MaybeTransOptsVarPair = MaybeTransOptsVar - MaybeTransOptsVarSpace,
    MaybeModuleDepsVarPair = MaybeModuleDepsVar - MaybeModuleDepsVarSpace,

    ModuleNameStr = sym_name_to_string(ModuleName),
    LibModuleNameStr = "lib" ++ ModuleNameStr,

    LibInstallIntsTargetName = LibModuleNameStr ++ ".install_ints",
    LibInstallOptsTargetName = LibModuleNameStr ++ ".install_opts",
    LibInstallHdrsTargetName = LibModuleNameStr ++ ".install_hdrs",
    LibInstallGradeHdrsTargetName = LibModuleNameStr ++ ".install_grade_hdrs",

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
            map.member(DepsMap, _, deps(_, _, BurdenedModule)),
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
    LibInstallIntsFilesActionsLegacy =
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
        "for ext in mh int int2 int3" ++
            SpaceInt0Str ++ MaybeSpaceOptStr ++ MaybeSpaceTransOptStr ++
            MaybeSpaceDepStr ++ "; do \\",
        "\tdir=""$(INSTALL_INT_DIR)/Mercury/$${ext}s""; \\",
        "\trm -rf ""$$dir""; \\",
        "\t$(LN_S) .. ""$$dir"" || { \\",
        "\t\t{ test -d ""$$dir"" || \\",
        "\t\t$(INSTALL_MKDIR) ""$$dir""; } && \\",
        "\t\t$(INSTALL) ""$(INSTALL_INT_DIR)""/*.$$ext ""$$dir""; \\",
        "\t} || exit 1; \\",
        "done"],
    LibInstallIntsFilesActions = LibInstallIntsFilesActionsLegacy,
    MmakeRuleLibInstallInts = mmake_simple_rule("lib_install_ints",
        mmake_rule_is_phony,
        LibInstallIntsTargetName,
        [ModuleMakeVarNameInts, ModuleMakeVarNameInt3s] ++
            MaybeModuleVarNameInt0s ++ MaybeOptsVar ++ MaybeTransOptsVar ++
            MaybeModuleDepsVar ++ ["install_lib_dirs"],
        LibInstallIntsFilesActions),

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
        LibInstallOptsActionsLegacy =
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
            "\t\t{ test -d ""$$dir"" || \\",
            "\t\t\t$(INSTALL_MKDIR) ""$$dir""; } && \\",
            "\t\t$(INSTALL) ""$(INSTALL_GRADE_INT_DIR)""/*.$$ext \\",
            "\t\t\t""$$dir""; \\",
            "\t} || exit 1; \\",
            "done"],
        LibInstallOptsActions = LibInstallOptsActionsLegacy
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
    LibInstallHdrsMhsActionsLegacy =
        ["for hdr in " ++ ModuleMakeVarNameMhs ++ "; do \\",
        "\t$(INSTALL) $$hdr $(INSTALL_INT_DIR); \\",
        "\t$(INSTALL) $$hdr $(INSTALL_INC_DIR); \\",
        "done"],
    LibInstallHdrsMhsActions = LibInstallHdrsMhsActionsLegacy,
    MmakeRuleLibInstallHdrsMhs = mmake_simple_rule("install_lib_hdrs_mhs",
        mmake_rule_is_phony,
        LibInstallHdrsTargetName,
        [ModuleMakeVarNameMhs, "install_lib_dirs"],
        LibInstallHdrsMhsActions),
    MmakeFragmentLibInstallHdrsMaybeMhs = mmf_conditional_entry(
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
    LibInstallGradeHdrsMihsActionsLegacy =
        ["for hdr in " ++ ModuleMakeVarNameMihs ++ "; do \\",
        "\t$(INSTALL) $$hdr $(INSTALL_INT_DIR); \\",
        "\t$(INSTALL) $$hdr $(INSTALL_GRADE_INC_DIR); \\",
        "done",
        "# The following is needed to support the `--use-subdirs' option.",
        "# We try using `$(LN_S)', but if that fails, then we just use",
        "# `$(INSTALL)'.",
        "rm -rf $(INSTALL_GRADE_INC_SUBDIR)",
        "$(LN_S) .. $(INSTALL_GRADE_INC_SUBDIR) || { \\",
        "\t{ test -d $(INSTALL_GRADE_INC_SUBDIR) || \\",
        "\t\t$(INSTALL_MKDIR) $(INSTALL_GRADE_INC_SUBDIR); \\",
        "\t} && \\",
        "\t$(INSTALL) $(INSTALL_GRADE_INC_DIR)/*.mih \\",
        "\t\t$(INSTALL_GRADE_INC_SUBDIR); \\",
        "} || exit 1",
        "rm -rf $(INSTALL_INT_DIR)/Mercury/mihs",
        "$(LN_S) .. $(INSTALL_INT_DIR)/Mercury/mihs || { \\",
        "\t{ test -d $(INSTALL_INT_DIR)/Mercury/mihs || \\",
        "\t\t$(INSTALL_MKDIR) \\",
        "\t\t$(INSTALL_INT_DIR)/Mercury/mihs; \\",
        "\t} && \\",
        "\t$(INSTALL) $(INSTALL_GRADE_INC_DIR)/*.mih \\",
        "\t\t$(INSTALL_INT_DIR); \\",
        "} || exit 1"],
    LibInstallGradeHdrsMihsActions = LibInstallGradeHdrsMihsActionsLegacy,
    MmakeRuleLibInstallGradeHdrsMihs = mmake_simple_rule(
        "install_grade_hdrs_mihs",
        mmake_rule_is_phony,
        LibInstallGradeHdrsTargetName,
        [ModuleMakeVarNameMihs, "install_grade_dirs"],
        LibInstallGradeHdrsMihsActions),
    MmakeFragmentLibInstallGradeHdrsMaybeMihs = mmf_conditional_entry(
        mmake_cond_strings_equal(ModuleMakeVarNameMihs, ""),
        MmakeRuleLibInstallGradeHdrsNoMihs,
        MmakeRuleLibInstallGradeHdrsMihs),

    add_mmake_entry(MmakeRuleLibInstallInts, !MmakeFile),
    add_mmake_entry(MmakeRuleLibInstallOpts, !MmakeFile),
    add_mmake_fragment(MmakeFragmentLibInstallHdrsMaybeMhs, !MmakeFile),
    add_mmake_fragment(MmakeFragmentLibInstallGradeHdrsMaybeMihs, !MmakeFile).

:- pred generate_dep_file_install_targets_proposed(globals::in, deps_map::in,
    module_name::in, string::in, bool::in, bool::in, bool::in,
    mmakefile::in, mmakefile::out) is det.

generate_dep_file_install_targets_proposed(Globals, DepsMap,
        MainModuleName, ModuleMVN, MmcMakeDeps, Intermod, TransOpt,
        !MmakeFile) :-
    MainModuleNameStr = sym_name_to_string(MainModuleName),
    LibModuleNameStr = "lib" ++ MainModuleNameStr,
    generate_dep_file_install_targets_proposed_ngs_pgs(DepsMap,
        ModuleMVN, LibModuleNameStr, NgsPgsTarget, MmakeRuleLibInstallNgsPgs),
    generate_dep_file_install_targets_proposed_gs_gas_c(Globals,
        MainModuleName, LibModuleNameStr, ModuleMVN,
        MmcMakeDeps, Intermod, TransOpt, GsGasTarget,
        MmakeRuleLibInstallGsGas),
    generate_dep_file_install_all_files(MainModuleNameStr, LibModuleNameStr,
        NgsPgsTarget, GsGasTarget, MmakeRuleLibInstallAll),
    add_mmake_entry(MmakeRuleLibInstallAll, !MmakeFile),
    add_mmake_entry(MmakeRuleLibInstallNgsPgs, !MmakeFile),
    add_mmake_entry(MmakeRuleLibInstallGsGas, !MmakeFile).

:- pred generate_dep_file_install_targets_proposed_ngs_pgs(deps_map::in,
    string::in, string::in, string::out, mmake_entry::out) is det.

generate_dep_file_install_targets_proposed_ngs_pgs(DepsMap,
        ModuleMVN, LibModuleNameStr, LibNgsPgsInstallTargetName,
        MmakeRuleLibInstallNgsPgs) :-
    LibNgsPgsInstallTargetName = LibModuleNameStr ++ ".install_ngs_pgs_files",

    string.format("$(%s.int0s)", [s(ModuleMVN)], Int0sMVN),
    % string.format("$(%s.int1s)", [s(ModuleMVN)], Int1sMVN),
    % string.format("$(%s.int2s)", [s(ModuleMVN)], Int2sMVN),
    string.format("$(%s.ints)",  [s(ModuleMVN)], Int12sMVN),
    string.format("$(%s.int3s)", [s(ModuleMVN)], Int3sMVN),
    string.format("$(%s.mhs)",   [s(ModuleMVN)], MhsMVN),

    ( if some_module_in_deps_map_has_a_submodule(DepsMap) then
        MaybeInt0sMVN = [Int0sMVN],
        ActionsInt0 = proposed_action_lines(Int0sMVN, "int0s")
    else
        MaybeInt0sMVN = [],
        ActionsInt0 = []
    ),

    % ActionsInt1 = proposed_action_lines(Int1sMVN, "int1s"),
    % ActionsInt2 = proposed_action_lines(Int2sMVN, "int2s"),
    ActionsInt12 = proposed_action_lines(Int12sMVN, "ints"),
    ActionsInt3 =  proposed_action_lines(Int3sMVN, "int3s"),
    ActionsMh =    proposed_action_lines(MhsMVN,   "mhs"),

    NgsPgsInstallActions =
        % ActionsInt1 ++ ActionsInt2
        ActionsInt0 ++ ActionsInt12 ++ ActionsInt3 ++ ActionsMh,
    MmakeRuleLibInstallNgsPgs = mmake_simple_rule("install_ngs_pgs_files",
        mmake_rule_is_phony,
        LibNgsPgsInstallTargetName,
        % Int1sMVN, Int2sMVN
        MaybeInt0sMVN ++ [Int12sMVN, Int3sMVN, MhsMVN],
        NgsPgsInstallActions).

:- pred generate_dep_file_install_targets_proposed_gs_gas_c(globals::in,
    module_name::in, string::in, string::in, bool::in, bool::in, bool::in,
    string::out, mmake_entry::out) is det.

generate_dep_file_install_targets_proposed_gs_gas_c(Globals, MainModuleName,
        LibModuleNameStr, ModuleMVN, MmcMakeDeps, Intermod, TransOpt,
        LibGsGasInstallTargetName, MmakeRuleLibInstallGsGas) :-
    LibGsGasInstallTargetName = LibModuleNameStr ++ ".install_gs_gas_files_c",

    string.format("$(%s.module_deps)", [s(ModuleMVN)], ModuleDepsMVN),
    string.format("$(%s.opts)",        [s(ModuleMVN)], PlainOptsMVN),
    string.format("$(%s.trans_opt)",   [s(ModuleMVN)], TransOptsMVN),
    string.format("$(%s.mihs)",        [s(ModuleMVN)], MihsMVN),

    ActionsMDs0 =       proposed_gs_action_lines(ModuleDepsMVN, "module_deps"),
    ActionsPlainOpts0 = proposed_gs_action_lines(PlainOptsMVN,  "opts"),
    ActionsTransOpts0 = proposed_gs_action_lines(TransOptsMVN,  "trans_opts"),

    ActionsMihs  = proposed_cond_gs_action_lines("hlc",    MihsMVN,  "mihs"),

    ExtInit = ext_cur_gs(ext_cur_gs_lib_init),
    ExtA    = ext_cur_gas(ext_cur_gas_lib_dollar_a),
    ExtSo   = ext_cur_gas(ext_cur_gas_lib_dollar_efsl),
    module_name_to_file_name(Globals, $pred, ExtInit, MainModuleName, _, Init),
    module_name_to_lib_file_name(Globals, $pred, "lib", ExtA,
        MainModuleName, _, A),
    module_name_to_lib_file_name(Globals, $pred, "lib", ExtSo,
        MainModuleName, _, So),
    ActionsInits = proposed_cond_gs_action_lines("hlc", Init,  "inits"),
    ActionsAsSos = proposed_cond_gas_action_lines("c",  A ++ " " ++ So, "lib"),

    (
        MmcMakeDeps = no,
        MaybeModuleDepsMVN = [],
        ActionsMDs = []
    ;
        MmcMakeDeps = yes,
        MaybeModuleDepsMVN = [ModuleDepsMVN],
        ActionsMDs = ActionsMDs0
    ),
    (
        Intermod = no,
        MaybePlainOptsMVN = [],
        ActionsPlainOpts = []
    ;
        Intermod = yes,
        MaybePlainOptsMVN = [PlainOptsMVN],
        ActionsPlainOpts = ActionsPlainOpts0
    ),
    (
        TransOpt = no,
        MaybeTransOptsMVN = [],
        ActionsTransOpts = []
    ;
        TransOpt = yes,
        MaybeTransOptsMVN = [TransOptsMVN],
        ActionsTransOpts = ActionsTransOpts0
    ),

    GsGasInstallActions =
        ActionsMDs ++ ActionsPlainOpts ++ ActionsTransOpts ++
        ActionsMihs ++ ActionsInits ++ ActionsAsSos,
    MmakeRuleLibInstallGsGas = mmake_simple_rule("install_gs_gas_files",
        mmake_rule_is_phony,
        LibGsGasInstallTargetName,
        MaybeModuleDepsMVN ++ MaybePlainOptsMVN ++ MaybeTransOptsMVN ++
        [MihsMVN, Init, A, So],
        GsGasInstallActions).

:- pred generate_dep_file_install_all_files(string::in, string::in,
    string::in, string::in, mmake_entry::out) is det.

generate_dep_file_install_all_files(MainModuleNameStr, LibModuleNameStr,
        NgsPgsTarget, GsGasTarget, MmakeRuleLibInstallAll) :-
    LibAllInstallTargetName = LibModuleNameStr ++ ".install_all_files",
    DependTarget = MainModuleNameStr ++ ".depend",

    ActionsSave =
        ["rm -rf tmp_dir && \\",
        "mkdir tmp_dir && \\",
        "rm -rf tmp_dir && \\",
        "mkdir tmp_dir && \\",
        "grade_files=\"$(foreach e,$(GRADE_SUBDIR_MVEXTS)," ++
            "$(" ++ MainModuleNameStr ++ ".$(e)))\" && \\",
        "for file in x $$grade_files; do \\",
        "\tif test \"$$file\" != x; then \\",
        "\t\tmv -f $$file tmp_dir > /dev/null 2>&1; \\",
        "\t\ttrue; \\",
        "\tfi; \\",
        "done && \\",
        "{ mv -f $(deps_subdir)$*.dep $(deps_subdir)$*.dv \\",
        "\t*.$A *.$(EXT_FOR_SHARED_LIB) tmp_dir || \\",
        "\ttrue; } && \\"],

    ActionsGsGas =
        ["for grade in $(ALL_LIBGRADES); do \\",
        "\tif test \"$$grade\" != \"$(GRADE)\"; then \\",
        "\t\tif mmake_grade_test c \"$$grade\"; then \\",
        "\t\t\t$(MMAKE) GRADE=$$grade \\",
        "\t\t\t\t" ++ DependTarget ++ " || exit 1; \\",
        "\t\t\t$(MMAKE) GRADE=$$grade \\",
        "\t\t\t\t" ++ GsGasTarget ++ " || \\",
        "\t\t\t\texit 1; \\",
        "\t\telse \\",
        "\t\t\tmmc --make --grade=$$grade \\",
        "\t\t\t\t--install-prefix $(INSTALL_PREFIX) \\",
        "\t\t\t\t" ++ LibModuleNameStr ++ ".install_gs_gas || \\",
        "\t\t\t\texit 1; \\",
        "\t\tfi; \\",
        "\t\tfor file in x $$grade_files; do \\",
        "\t\t\tif test \"$$file\" != x; then \\",
        "\t\t\t\trm -f $$file; \\",
        "\t\t\tfi; \\",
        "\t\tdone; \\",
        "\t\trm -f $(deps_subdir)$*.dep $(deps_subdir)$*.dv \\",
        "\t\t\t*.$A *.$(EXT_FOR_SHARED_LIB) \\",
        "\t\t\t*.jar *.dll *.err *.dep_err; \\",
        "\tfi; \\",
        "done && \\"],

    ActionsUndoSave =
        ["for file in x $$grade_files; do \\",
        "\tif test \"$$file\" != x; then \\",
        "\t\tmv -f tmp_dir/`basename $$file` $$file \\",
        "\t\t\t> /dev/null 2>&1; \\",
        "\t\ttrue; \\",
        "\tfi; \\",
        "done && \\",
        "{ mv -f tmp_dir/*.dep tmp_dir/*.dv $(deps_subdir).; \\",
        "\tmv -f tmp_dir/* .; rmdir tmp_dir; true; }"],

    MmakeRuleLibInstallAll = mmake_simple_rule("install__all_files",
        mmake_rule_is_phony,
        LibAllInstallTargetName,
        [NgsPgsTarget],
        ActionsSave ++ ActionsGsGas ++ ActionsUndoSave).

:- pred some_module_in_deps_map_has_a_submodule(deps_map::in) is semidet.

some_module_in_deps_map_has_a_submodule(DepsMap) :-
    some [BurdenedModule] (
        map.member(DepsMap, _, deps(_, _, BurdenedModule)),
        ParseTreeModuleSrc = BurdenedModule ^ bm_module,
        IncludeMap = ParseTreeModuleSrc ^ ptms_include_map,
        not map.is_empty(IncludeMap)
    ).

:- func proposed_gs_action_lines(string, string) = list(string).

proposed_gs_action_lines(MmakeVarName, ExtDir) =
    proposed_action_lines(MmakeVarName, ExtDir ++ "/$(GRADESTRING)").

:- func proposed_action_lines(string, string) = list(string).

proposed_action_lines(MmakeVarName, ExtDir) =
    proposed_action_lines_prefix_suffix(MmakeVarName, ExtDir, "", "").

:- func proposed_cond_gs_action_lines(string, string, string) = list(string).

proposed_cond_gs_action_lines(CondName, MmakeVarName, ExtDir) =
    proposed_cond_action_lines(CondName, MmakeVarName,
        ExtDir ++ "/$(GRADESTRING)").

:- func proposed_cond_gas_action_lines(string, string, string) = list(string).

proposed_cond_gas_action_lines(CondName, MmakeVarName, ExtDir) =
    proposed_cond_action_lines(CondName, MmakeVarName,
        ExtDir ++ "/$(GRADESTRING)/$(FULLARCH)").

:- func proposed_cond_action_lines(string, string, string) = list(string).

proposed_cond_action_lines(CondName, MmakeVarName, RelativePath) = Lines :-
    string.format("if mmake_grade_test %s $(GRADE); then \\",
        [s(CondName)], StartLine),
    MidLines = proposed_action_lines_prefix_suffix(MmakeVarName, RelativePath,
        "\t", " ; \\"),
    string.format("fi", [], EndLine),
    Lines = [StartLine] ++ MidLines ++ [EndLine].

:- func proposed_action_lines_prefix_suffix(string, string, string, string)
    = list(string).

proposed_action_lines_prefix_suffix(MmakeVarName, RelativePath, Prefix, Suffix)
        = Lines :-
    string.format(
        "%s$(INSTALL_MKDIR) $(INSTALL_PREFIX)/MercurySystem/%s && \\",
        [s(Prefix), s(RelativePath)], Line1),
    % XXX This is the place to insert code to print progress reports.
    % That code could be
    %
    %   echo Installing $(MmakeVarName)
    %
    % (which could generate very LONG lines), or
    %
    %   foreach f in $(MmakeVarName); do echo ${f}; done
    %
    % (which could generate very MANY lines), or something more sophisticated.
    % That could something that computes how many files there are in
    % $(MmakeVarName), and if this number exceeds a threshold, gets the
    % first and last filenames, and prints something like
    %
    %   Installing 527 files from aaabb.int3 to xyzzy.int3
    %
    % XXX The mmake action line we generate will look short, but after
    % the expansion of MmakeVarName, it will probably be long enough
    % for the limits on the lengths of command lines to be a concern.
    string.format(
        "%s$(INSTALL) %s $(INSTALL_PREFIX)/MercurySystem/%s%s",
        [s(Prefix), s(MmakeVarName), s(RelativePath), s(Suffix)], Line2),
    Lines = [Line1, Line2].

%---------------------%

:- pred generate_dep_file_collective_targets(module_name::in, string::in,
    mmakefile::in, mmakefile::out) is det.

generate_dep_file_collective_targets(ModuleName, ModuleMakeVarName,
        !MmakeFile) :-
    ModuleNameStr = sym_name_to_string(ModuleName),
    list.map(
        construct_dep_file_collective_target_rule(ModuleNameStr,
            ModuleMakeVarName),
        [{".check", ".errs"},
        {".ints", ".dates"},
        {".int3s", ".date3s"},
        {".opts", ".optdates"},
        {".trans_opts", ".trans_opt_dates"},
        {".javas", ".all_javas"},
        {".classes", ".classes"},
        {".all_ints", ".dates"},
        {".all_int3s", ".date3s"},
        {".all_opts", ".optdates"},
        {".all_trans_opts", ".trans_opt_dates"}],
        MmakeRules),
    add_mmake_entries(MmakeRules, !MmakeFile).

:- pred construct_dep_file_collective_target_rule(string::in, string::in,
    {string, string}::in, mmake_entry::out) is det.

construct_dep_file_collective_target_rule(ModuleNameStr, ModuleMakeVarName,
        {ExtStr, VarExtension}, MmakeRule) :-
    TargetName = ModuleNameStr ++ ExtStr,
    Source = string.format("$(%s%s)", [s(ModuleMakeVarName), s(VarExtension)]),
    MmakeRule = mmake_simple_rule(
        "collective_target_" ++ ExtStr ++ VarExtension, mmake_rule_is_phony,
        TargetName, [Source], []).

%---------------------%

:- pred generate_dep_file_clean_targets(module_name::in, string::in,
    string::in, string::in, string::in, string::in, string::in, string::in,
    string::in, string::in, string::in, string::in,
    mmakefile::in, mmakefile::out) is det.

generate_dep_file_clean_targets(ModuleName, ModuleMakeVarName,
        ExeFileName, InitCFileName, InitObjFileName, InitPicObjFileName,
        InitFileName, StaticLibFileName, SharedLibFileName, JarFileName,
        DepFileName, DvFileName, !MmakeFile) :-
    % If you change the clean targets below, please also update the
    % documentation in doc/user_guide.texi.

    ModuleNameStr = sym_name_to_string(ModuleName),
    CleanTargetName = ModuleNameStr ++ ".clean",
    RealCleanTargetName = ModuleNameStr ++ ".realclean",

    % XXX Put these into a logical order.
    % XXX Why not clean up C# files?
    CleanSuffixes = [".dirs", ".all_cs", ".mihs", ".all_os", ".all_pic_os",
        ".c_dates", ".java_dates", ".useds", ".all_javas", ".profs",
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
        ".optdates", ".trans_opt_dates", ".ints", ".int0s_to_clean", ".int3s",
        ".opts", ".trans_opts", ".analyses", ".requests", ".imdgs",
        ".ds", ".module_deps", ".mhs_to_clean", ".mihs_to_clean", ".dlls",
        ".foreign_dlls", ".classes"],
    RealCleanFiles = [ExeFileName ++ "$(EXT_FOR_EXE) ", InitFileName,
        StaticLibFileName, SharedLibFileName, JarFileName,
        DepFileName, DvFileName],
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
    % This is currently not a problem in practice, as we never generate
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
    Deps = deps(_, _, BurdenedModule),
    Baggage = BurdenedModule ^ bm_baggage,
    SourceFileName = Baggage ^ mb_source_file_name,
    ( if string.remove_suffix(SourceFileName, ".m", SourceFileBase) then
        FileName = SourceFileBase
    else
        unexpected($pred, "source file name doesn't end in `.m'")
    ).

%---------------------------------------------------------------------------%
:- end_module parse_tree.generate_mmakefile_fragments.
%---------------------------------------------------------------------------%
