%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2002-2012 The University of Melbourne.
% Copyright (C) 2013-2021 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: make.top_level.m.
% Main author: stayl.
%
% The top level of mmc --make.
%
% TODO:
% - distributed builds
%
%---------------------------------------------------------------------------%

:- module make.top_level.
:- interface.

:- import_module libs.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module libs.maybe_succeeded.
:- import_module make.make_info.
:- import_module make.options_file.

:- import_module io.
:- import_module list.

%---------------------------------------------------------------------------%

:- pred make_process_compiler_args(globals::in, list(string)::in,
    options_variables::in, list(string)::in, list(file_name)::in,
    io::di, io::uo) is det.

:- pred make_top_target(globals::in, top_target_file::in, maybe_succeeded::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.compile_target_code.
:- import_module libs.options.
:- import_module libs.timestamp.
:- import_module make.build.
:- import_module make.dependencies.
:- import_module make.deps_set.
:- import_module make.module_target.
:- import_module make.program_target.
:- import_module make.track_flags.
:- import_module make.util.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.file_names.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.module_cmds.
:- import_module parse_tree.read_modules.
:- import_module parse_tree.write_error_spec.

:- import_module bool.
:- import_module dir.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module set.
:- import_module solutions.
:- import_module string.
:- import_module version_array.
:- import_module version_hash_table.

%---------------------------------------------------------------------------%

make_process_compiler_args(Globals, DetectedGradeFlags, Variables, OptionArgs,
        Targets0, !IO) :-
    io.progname_base("mercury_compile", ProgName, !IO),
    get_main_target_if_needed(ProgName, Variables, Targets0, MaybeTargets0),
    report_any_absolute_targets(ProgName, MaybeTargets0, MaybeTargets),
    (
        MaybeTargets = error1(Specs),
        io.stderr_stream(StdErr, !IO),
        write_error_specs(StdErr, Globals, Specs, !IO)
    ;
        MaybeTargets = ok1(Targets),
        globals.lookup_bool_option(Globals, keep_going, KeepGoingBool),
        ( KeepGoingBool = no,  KeepGoing = do_not_keep_going
        ; KeepGoingBool = yes, KeepGoing = do_keep_going
        ),

        ModuleIndexMap = module_index_map(
            version_hash_table.init_default(module_name_hash),
            version_array.empty, 0u),
        DepIndexMap = dependency_file_index_map(
            version_hash_table.init_default(
                dependency_file_with_module_index_hash),
            version_array.empty, 0u),
        DepStatusMap = version_hash_table.init_default(dependency_file_hash),

        % Accept and ignore `.depend' targets. `mmc --make' does not need
        % a separate make depend step. The dependencies for each module
        % are regenerated on demand.
        NonDependTargets = list.filter(
            ( pred(Target::in) is semidet :-
                not string.suffix(Target, ".depend")
            ), Targets),
        % Classify the remaining targets.
        list.map(classify_target(Globals), NonDependTargets,
            ClassifiedTargets),

        ShouldRebuildModuleDeps = do_rebuild_module_deps,
        globals.lookup_int_option(Globals, analysis_repeat, AnalysisRepeat),

        map.init(ModuleDependencies),
        map.init(FileTimestamps),
        map.init(TargetTimestamps),
        set.init(ErrorFileModules),
        MaybeImportingModule = maybe.no,
        MaybeStdoutLock = maybe.no,
        MakeInfo0 = make_info(
            ModuleDependencies,
            FileTimestamps,
            TargetTimestamps,
            DetectedGradeFlags,
            OptionArgs,
            Variables,
            ModuleIndexMap,
            DepIndexMap,
            DepStatusMap,
            init_cached_direct_imports,
            init_cached_direct_imports,
            init_cached_indirect_imports,
            init_cached_transitive_dependencies,
            init_cached_transitive_foreign_imports,
            ShouldRebuildModuleDeps,
            KeepGoing,
            ErrorFileModules,
            MaybeImportingModule,
            set.list_to_set(ClassifiedTargets),
            AnalysisRepeat,
            MaybeStdoutLock,
            init_have_read_module_maps
        ),

        % Build the targets, stopping on any errors if `--keep-going'
        % was not set.
        foldl2_make_top_targets(KeepGoing, Globals,
            ClassifiedTargets, Succeeded, MakeInfo0, _MakeInfo, !IO),
        maybe_set_exit_status(Succeeded, !IO)
    ).

%---------------------%

:- pred get_main_target_if_needed(string::in, options_variables::in,
    list(string)::in, maybe1(list(string))::out) is det.

get_main_target_if_needed(ProgName, Variables, Targets0, MaybeTargets) :-
    (
        Targets0 = [],
        lookup_main_target(Variables, MaybeMainTargets),
        (
            MaybeMainTargets = error1(Specs),
            MaybeTargets = error1(Specs)
        ;
            MaybeMainTargets = ok1(MainTargets),
            (
                MainTargets = [_ | _],
                MaybeTargets = ok1(MainTargets)
            ;
                MainTargets = [],
                Pieces = [fixed(ProgName), suffix(":"),
                    words("*** Error: no target or MAIN_TARGET specified."),
                    nl],
                Spec = simplest_no_context_spec($pred, severity_error,
                    phase_options, Pieces),
                MaybeTargets = error1([Spec])
            )
        )
    ;
        Targets0 = [_ | _],
        MaybeTargets = ok1(Targets0)
    ).

%---------------------%

    % Ensure none of the targets contains the directory_separator.
    % Such targets are not supported by the rest of the code.
    %
:- pred report_any_absolute_targets(string::in, maybe1(list(string))::in,
    maybe1(list(string))::out) is det.

report_any_absolute_targets(ProgName, MaybeTargets0, MaybeTargets) :-
    (
        MaybeTargets0 = error1(_),
        MaybeTargets = MaybeTargets0
    ;
        MaybeTargets0 = ok1(Targets),
        IsAbsoluteFileName =
            ( pred(Target::in) is semidet :-
                string.contains_char(Target, dir.directory_separator)
            ),
        list.filter(IsAbsoluteFileName, Targets, AbsTargets),
        (
            AbsTargets = [],
            MaybeTargets = MaybeTargets0
        ;
            AbsTargets = [_ | _],
            AbsTargetSpecs =
                list.map(report_target_with_dir_component(ProgName),
                AbsTargets),
            MaybeTargets = error1(AbsTargetSpecs)
        )
    ).

:- func report_target_with_dir_component(string, string) = error_spec.

report_target_with_dir_component(ProgName, Target) = Spec :-
    Pieces = [fixed(ProgName), suffix(":"),
        words("a make target may not contain a directory component,"),
        words("but"), quote(Target), words("does."), nl],
    Spec = simplest_no_context_spec($pred, severity_error,
        phase_make_target, Pieces).

%---------------------%

make_top_target(Globals, Target, Succeeded, !Info, !IO) :-
    Target = top_target_file(ModuleName, TargetType),
    globals.lookup_bool_option(Globals, track_flags, TrackFlags),
    (
        TrackFlags = no,
        TrackFlagsSucceeded = succeeded
    ;
        TrackFlags = yes,
        make_track_flags_files(Globals, ModuleName, TrackFlagsSucceeded,
            !Info, !IO)
    ),
    (
        TrackFlagsSucceeded = succeeded,
        (
            TargetType = module_target(ModuleTargetType),
            TargetFile = target_file(ModuleName, ModuleTargetType),
            make_module_target([], Globals, dep_target(TargetFile), Succeeded,
                !Info, !IO)
        ;
            TargetType = linked_target(ProgramTargetType),
            LinkedTargetFile = linked_target_file(ModuleName,
                ProgramTargetType),
            make_linked_target(Globals, LinkedTargetFile, Succeeded,
                !Info, [], Specs, !IO),
            get_error_output_stream(Globals, ModuleName, ErrorStream, !IO),
            write_error_specs(ErrorStream, Globals, Specs, !IO)
        ;
            TargetType = misc_target(MiscTargetType),
            make_misc_target(Globals, ModuleName - MiscTargetType, Succeeded,
                !Info, [], Specs, !IO),
            get_error_output_stream(Globals, ModuleName, ErrorStream, !IO),
            write_error_specs(ErrorStream, Globals, Specs, !IO)
        )
    ;
        TrackFlagsSucceeded = did_not_succeed,
        Succeeded = did_not_succeed
    ).

%---------------------------------------------------------------------------%

:- pred classify_target(globals::in, string::in, top_target_file::out) is det.

classify_target(Globals, FileName, TopTargetFile) :-
    ( if
        string.length(FileName, NameLength),
        search_backwards_for_dot(FileName, NameLength, DotLocn),
        string.split(FileName, DotLocn, ModuleNameStr0, Suffix),
        solutions(classify_target_2(Globals, ModuleNameStr0, Suffix),
            TopTargetFiles),
        TopTargetFiles = [OnlyTopTargetFile]
    then
        TopTargetFile = OnlyTopTargetFile
    else if
        string.append("lib", ModuleNameStr, FileName)
    then
        file_name_to_module_name(ModuleNameStr, ModuleName),
        TargetType = misc_target(misc_target_build_library),
        TopTargetFile = top_target_file(ModuleName, TargetType)
    else
        file_name_to_module_name(FileName, ModuleName),
        ExecutableType = get_executable_type(Globals),
        TargetType = linked_target(ExecutableType),
        TopTargetFile = top_target_file(ModuleName, TargetType)
    ).

:- pred classify_target_2(globals::in, string::in, string::in,
    top_target_file::out) is nondet.

classify_target_2(Globals, ModuleNameStr0, ExtStr, TopTargetFile) :-
    ( if
        extension_to_target_type(Globals, ExtStr, ModuleTargetType),
        % The .cs extension was used to build all C target files, but .cs is
        % also the file name extension for a C# file. The former use is being
        % migrated over to the .all_cs target but we still accept it for now.
        % NOTE This workaround is still in use as of 2020 may 23, even though
        % it was added in 2010,
        ExtStr \= ".cs"
    then
        ModuleNameStr = ModuleNameStr0,
        TargetType = module_target(ModuleTargetType)
    else if
        target_extension_synonym(ExtStr, ModuleTargetType)
    then
        ModuleNameStr = ModuleNameStr0,
        TargetType = module_target(ModuleTargetType)
    else if
        globals.lookup_string_option(Globals, library_extension, ExtStr),
        string.append("lib", ModuleNameStr1, ModuleNameStr0)
    then
        ModuleNameStr = ModuleNameStr1,
        TargetType = linked_target(static_library)
    else if
        globals.lookup_string_option(Globals, shared_library_extension,
            ExtStr),
        string.append("lib", ModuleNameStr1, ModuleNameStr0)
    then
        ModuleNameStr = ModuleNameStr1,
        TargetType = linked_target(shared_library)
    else if
        globals.lookup_string_option(Globals, executable_file_extension,
            ExtStr)
    then
        ModuleNameStr = ModuleNameStr0,
        ExecutableType = get_executable_type(Globals),
        TargetType = linked_target(ExecutableType)
    else if
        (
            string.append(".all_", Rest, ExtStr),
            string.append(DotlessExtStr1, "s", Rest),
            ExtStr1 = "." ++ DotlessExtStr1
        ;
            % Deprecated.
            string.append(ExtStr1, "s", ExtStr)
        ),
        (
            extension_to_target_type(Globals, ExtStr1, ModuleTargetType)
        ;
            target_extension_synonym(ExtStr1, ModuleTargetType)
        ),
        % Not yet implemented. `build_all' targets are only used by
        % tools/bootcheck, so it doesn't really matter.
        ModuleTargetType \= module_target_c_header(_)
    then
        ModuleNameStr = ModuleNameStr0,
        TargetType = misc_target(misc_target_build_all(ModuleTargetType))
    else if
        ExtStr = ".check"
    then
        ModuleNameStr = ModuleNameStr0,
        TargetType = misc_target(misc_target_build_all(module_target_errors))
    else if
        ExtStr = ".analyse"
    then
        ModuleNameStr = ModuleNameStr0,
        TargetType = misc_target(misc_target_build_analyses)
    else if
        ExtStr = ".clean"
    then
        ModuleNameStr = ModuleNameStr0,
        TargetType = misc_target(misc_target_clean)
    else if
        ExtStr = ".realclean"
    then
        ModuleNameStr = ModuleNameStr0,
        TargetType = misc_target(misc_target_realclean)
    else if
        ExtStr = ".install",
        string.append("lib", ModuleNameStr1, ModuleNameStr0)
    then
        ModuleNameStr = ModuleNameStr1,
        TargetType = misc_target(misc_target_install_library)
    else if
        ExtStr = ".doc"
    then
        ModuleNameStr = ModuleNameStr0,
        TargetType = misc_target(misc_target_build_xml_docs)
    else
        fail
    ),
    file_name_to_module_name(ModuleNameStr, ModuleName),
    TopTargetFile = top_target_file(ModuleName, TargetType).

:- pred search_backwards_for_dot(string::in, int::in, int::out) is semidet.

search_backwards_for_dot(String, Index, DotIndex) :-
    string.unsafe_prev_index(String, Index, CharIndex, Char),
    ( if Char = ('.') then
        DotIndex = CharIndex
    else
        search_backwards_for_dot(String, CharIndex, DotIndex)
    ).

:- func get_executable_type(globals) = linked_target_type.

get_executable_type(Globals) = ExecutableType :-
    globals.get_target(Globals, CompilationTarget),
    (
        CompilationTarget = target_c,
        ExecutableType = executable
    ;
        CompilationTarget = target_csharp,
        ExecutableType = csharp_executable
    ;
        CompilationTarget = target_java,
        ExecutableType = java_executable
    ).

%---------------------------------------------------------------------------%
:- end_module make.top_level.
%---------------------------------------------------------------------------%
