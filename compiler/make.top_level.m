%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2002-2012 The University of Melbourne.
% Copyright (C) 2013-2025 The Mercury team.
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
:- import_module libs.maybe_util.
:- import_module make.make_info.
:- import_module make.options_file.

:- import_module io.
:- import_module list.

%---------------------------------------------------------------------------%

:- pred make_process_compiler_args(io.text_output_stream::in, globals::in,
    env_optfile_variables::in,
    list(string)::in, list(string)::in, list(file_name)::in,
    io::di, io::uo) is det.

:- pred make_top_targets(io.text_output_stream::in, globals::in,
    maybe_keep_going::in, list(top_target_file)::in,
    maybe_succeeded::in, maybe_succeeded::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.compile_target_code.
:- import_module backend_libs.link_target_code.
:- import_module libs.options.
:- import_module libs.timestamp.
:- import_module make.deps_set.
:- import_module make.hash.
:- import_module make.module_target.
:- import_module make.program_target.
:- import_module make.timestamp.
:- import_module make.track_flags.
:- import_module make.util.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.file_names.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.module_cmds.
:- import_module parse_tree.write_error_spec.

:- import_module bool.
:- import_module dir.
:- import_module map.
:- import_module pair.
:- import_module set.
:- import_module string.
:- import_module version_array.
:- import_module version_hash_table.

%---------------------------------------------------------------------------%

make_process_compiler_args(ProgressStream, Globals,
        EnvOptFileVariables, EnvVarArgs, OptionArgs, Targets0, !IO) :-
    io.progname_base("mercury_compile", ProgName, !IO),
    get_main_target_if_needed(ProgName, EnvOptFileVariables,
        Targets0, MaybeTargets0),
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
        globals.lookup_int_option(Globals, analysis_repeat, AnalysisRepeat),

        HashPredMI = module_name_hash,
        ForwardMI = version_hash_table.init_default(HashPredMI),
        ReverseMI = version_array.empty,
        ModuleIndexMap = module_index_map(ForwardMI, ReverseMI, 0u),

        HashPredDI = dependency_file_with_module_index_hash,
        ForwardDI = version_hash_table.init_default(HashPredDI),
        ReverseDI = version_array.empty,
        DepIndexMap = dependency_file_index_map(ForwardDI, ReverseDI, 0u),

        DepStatusMap = version_hash_table.init_default(dependency_file_hash),

        % Accept and ignore `.depend' targets. `mmc --make' does not need
        % a separate make depend step. The dependencies for each module
        % are regenerated on demand.
        list.filter(
            ( pred(Target::in) is semidet :-
                not string.suffix(Target, ".depend"),
                not string.suffix(Target, ".depend_ints")
            ), Targets, NonDependTargets),
        % Classify the remaining targets.
        list.map(classify_target(Globals), NonDependTargets,
            ClassifiedTargets),
        ClassifiedTargetSet = set.list_to_set(ClassifiedTargets),

        globals.get_maybe_stdlib_grades(Globals, MaybeStdLibGrades),
        MakeInfo0 = init_make_info(EnvOptFileVariables, MaybeStdLibGrades,
            KeepGoing, EnvVarArgs, OptionArgs, ClassifiedTargetSet,
            AnalysisRepeat, init_target_file_timestamp_map, ModuleIndexMap,
            DepIndexMap, DepStatusMap),

        % Build the targets, stopping on any errors if `--keep-going'
        % was not set.
        make_top_targets(ProgressStream, Globals, KeepGoing, ClassifiedTargets,
            succeeded, Succeeded, MakeInfo0, _MakeInfo, !IO),
        maybe_set_exit_status(Succeeded, !IO)
    ).

%---------------------%

:- pred get_main_target_if_needed(string::in, env_optfile_variables::in,
    list(string)::in, maybe1(list(string))::out) is det.

get_main_target_if_needed(ProgName, EnvOptFileVariables, Targets0,
        MaybeTargets) :-
    (
        Targets0 = [_ | _],
        MaybeTargets = ok1(Targets0)
    ;
        Targets0 = [],
        lookup_main_target(EnvOptFileVariables, MaybeMainTargets),
        (
            MaybeMainTargets = ok1(MainTargets),
            (
                MainTargets = [_ | _],
                MaybeTargets = ok1(MainTargets)
            ;
                MainTargets = [],
                Pieces = [fixed(ProgName), suffix(":"),
                    words("*** Error: no target or MAIN_TARGET specified."),
                    nl],
                Spec = no_ctxt_spec($pred, severity_error, phase_options,
                    Pieces),
                MaybeTargets = error1([Spec])
            )
        ;
            MaybeMainTargets = error1(Specs),
            MaybeTargets = error1(Specs)
        )
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
    Spec = no_ctxt_spec($pred, severity_error, phase_make_target, Pieces).

%---------------------%

make_top_targets(_ProgressStream, _Globals, _KeepGoing, [],
        !Succeeded, !Info, !IO).
make_top_targets(ProgressStream, Globals, KeepGoing, [Target | Targets],
        !Succeeded, !Info, !IO) :-
    make_top_target(ProgressStream, Globals, Target, TargetSucceeded,
        !Info, !IO),
    should_we_stop_or_continue(KeepGoing, TargetSucceeded, StopOrContinue,
        !Succeeded),
    (
        StopOrContinue = soc_stop
    ;
        StopOrContinue = soc_continue,
        make_top_targets(ProgressStream, Globals, KeepGoing, Targets,
            !Succeeded, !Info, !IO)
    ).

:- pred make_top_target(io.text_output_stream::in, globals::in,
    top_target_file::in, maybe_succeeded::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

make_top_target(ProgressStream, Globals, Target, Succeeded, !Info, !IO) :-
    Target = top_target_file(ModuleName, TargetType),
    globals.lookup_bool_option(Globals, make_track_flags, TrackFlags),
    get_error_output_stream(Globals, ModuleName, ErrorStream, !IO),
    (
        TrackFlags = no,
        TrackFlagsSucceeded = succeeded
    ;
        TrackFlags = yes,
        make_track_flags_files(ErrorStream, ProgressStream, Globals,
            ModuleName, TrackFlagsSucceeded, !Info, !IO)
    ),
    (
        TrackFlagsSucceeded = succeeded,
        (
            TargetType = module_target(ModuleTargetType),
            TargetFile = target_file(ModuleName, ModuleTargetType),
            make_module_target([], ProgressStream, Globals,
                dep_target(TargetFile), Succeeded, !Info, !IO)
        ;
            TargetType = linked_target(LinkedTargetType),
            LinkedTargetFile =
                linked_target_file(ModuleName, LinkedTargetType),
            make_linked_target(ProgressStream, Globals, LinkedTargetFile,
                Succeeded, !Info, [], Specs, !IO),
            write_error_specs(ErrorStream, Globals, Specs, !IO)
        ;
            TargetType = misc_target(MiscTargetType),
            make_misc_target(ProgressStream, Globals,
                ModuleName - MiscTargetType, Succeeded, !Info, [], Specs, !IO),
            write_error_specs(ErrorStream, Globals, Specs, !IO)
        )
    ;
        TrackFlagsSucceeded = did_not_succeed,
        Succeeded = did_not_succeed
    ).

%---------------------------------------------------------------------------%

:- pred classify_target(globals::in, string::in, top_target_file::out) is det.

classify_target(Globals, TargetName, TopTargetFile) :-
    ( if
        string.length(TargetName, NameLength),
        search_backwards_for_dot(TargetName, NameLength, DotLocn),
        string.split(TargetName, DotLocn, ModuleNameStr0, Suffix),
        classify_target_2(Globals, ModuleNameStr0, Suffix, TopTargetFilePrime)
    then
        TopTargetFile = TopTargetFilePrime
    else
        ( if string.remove_prefix("lib", TargetName, ModuleNameStr) then
            file_name_to_module_name(ModuleNameStr, ModuleName),
            TargetType = misc_target(misc_target_build_library)
        else
            file_name_to_module_name(TargetName, ModuleName),
            TargetType = linked_target(get_executable_type(Globals))
        ),
        TopTargetFile = top_target_file(ModuleName, TargetType)
    ).

:- pred classify_target_2(globals::in, string::in, string::in,
    top_target_file::out) is semidet.

classify_target_2(Globals, ModuleNameStr0, ExtStr, TopTargetFile) :-
    ( if fixed_extension_target_type(ExtStr, TargetTypePrime) then
        ModuleNameStr = ModuleNameStr0,
        TargetType = TargetTypePrime
    else
        get_linked_target_ext_map(Globals, LinkedtargetExtMap),
        map.search(LinkedtargetExtMap, ExtStr, LinkedTargetExtInfo),
        LinkedTargetExtInfo = linked_target_ext_info(_, LinkedTargetKind),
        % target_type_to_maybe_target_extension and this part of
        % classify_target_2 in make.top_level.m represent the same relationship
        % between targets and suffixes, but in different directions, and for
        % slightly different sets of targets. (For example, there is no
        % extension that generates module_target_fact_table_object as a
        % target.) Where they talk about the same targets, their codes
        % should be kept in sync.
        (
            (
                LinkedTargetKind = ltk_object_file,
                ModuleTargetType = module_target_object_code(non_pic)
            ;
                LinkedTargetKind = ltk_pic_object_file,
                ModuleTargetType = module_target_object_code(pic)
            ),
            ModuleNameStr = ModuleNameStr0,
            TargetType = module_target(ModuleTargetType)
        ;
            (
                LinkedTargetKind = ltk_all_object_file,
                ModuleTargetType = module_target_object_code(non_pic)
            ;
                LinkedTargetKind = ltk_all_pic_object_file,
                ModuleTargetType = module_target_object_code(pic)
            ),
            ModuleNameStr = ModuleNameStr0,
            TargetType = misc_target(misc_target_build_all(ModuleTargetType))
        ;
            LinkedTargetKind = ltk_executable,
            ModuleNameStr = ModuleNameStr0,
            TargetType = linked_target(get_executable_type(Globals))
        ;
            (
                LinkedTargetKind = ltk_static_library,
                TargetType = linked_target(static_library)
            ;
                LinkedTargetKind = ltk_shared_library,
                TargetType = linked_target(shared_library)
            ;
                LinkedTargetKind = ltk_library_install,
                TargetType = misc_target(misc_target_install_library)
            ;
                LinkedTargetKind = ltk_library_install_gs_gas,
                TargetType = misc_target(misc_target_install_library_gs_gas)
            ),
            string.remove_prefix("lib", ModuleNameStr0, ModuleNameStr)
        )
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

:- pred fixed_extension_target_type(string::in, target_type::out) is semidet.

fixed_extension_target_type(ExtStr, TargetType) :-
    (
        (
            ExtStr = ".m",
            ModuleTarget = module_target_source
        ;
            ExtStr = ".err",
            ModuleTarget = module_target_errors
        ;
            ExtStr = ".int0",
            ModuleTarget = module_target_int0
        ;
            ExtStr = ".int",
            ModuleTarget = module_target_int1
        ;
            ExtStr = ".int2",
            ModuleTarget = module_target_int2
        ;
            ExtStr = ".int3",
            ModuleTarget = module_target_int3
        ;
            ExtStr = ".opt",
            ModuleTarget = module_target_opt
        ;
            ExtStr = ".mih",
            ModuleTarget = module_target_c_header(header_mih)
        ;
            ExtStr = ".mh",
            ModuleTarget = module_target_c_header(header_mh)
        ;
            ExtStr = ".c",
            ModuleTarget = module_target_c_code
        ;
            ExtStr = ".cs",
            ModuleTarget = module_target_csharp_code
        ;
            ExtStr = ".csharp",
            % For a long time, until 2023 oct 5, we treated the ".cs" target
            % name as the build-all target for C files, so we accepted
            % ".csharp" as the target name for C# files. Keep the synonym
            % for a while longer to give people time to update their
            % projects and habits.
            ModuleTarget = module_target_csharp_code
        ;
            ExtStr = ".java",
            ModuleTarget = module_target_java_code
        ;
            ExtStr = ".class",
            ModuleTarget = module_target_java_class_code
        ;
            ExtStr = ".track_flags",
            ModuleTarget = module_target_track_flags
        ;
            ExtStr = ".xml",
            ModuleTarget = module_target_xml_doc
        ;
            ExtStr = ".analysis",
            ModuleTarget = module_target_analysis_registry
        ),
        TargetType = module_target(ModuleTarget)
    ;
        % This block of unifications is required by the fact that
        % switch detection looks into two levels of disjunctions.
        ( ExtStr = ".all_ms"
        ; ExtStr = ".ms"
        ; ExtStr = ".all_errs"
        ; ExtStr = ".errs"
        ; ExtStr = ".all_int0s"
        ; ExtStr = ".int0s"
        ; ExtStr = ".all_ints"
        ; ExtStr = ".ints"
        ; ExtStr = ".all_int2s"
        ; ExtStr = ".int2s"
        ; ExtStr = ".all_int3s"
        ; ExtStr = ".int3s"
        ; ExtStr = ".all_opts"
        ; ExtStr = ".opts"
        ; ExtStr = ".all_cs"
        ; ExtStr = ".all_css"
        ; ExtStr = ".css"
        ; ExtStr = ".all_csharps"
        ; ExtStr = ".csharps"
        ; ExtStr = ".all_javas"
        ; ExtStr = ".javas"
        ; ExtStr = ".all_classs"
        ; ExtStr = ".classs"
        ; ExtStr = ".all_track_flagss"
        ; ExtStr = ".track_flagss"
        ; ExtStr = ".all_xmls"
        ; ExtStr = ".xmls"
        ; ExtStr = ".all_analysiss"
        ; ExtStr = ".analysiss"
        ),
        (
            ( ExtStr = ".all_ms"
            ; ExtStr = ".ms"
            ),
            ModuleTarget = module_target_source
        ;
            ( ExtStr = ".all_errs"
            ; ExtStr = ".errs"
            ),
            ModuleTarget = module_target_errors
        ;
            ( ExtStr = ".all_int0s"
            ; ExtStr = ".int0s"
            ),
            ModuleTarget = module_target_int0
        ;
            ( ExtStr = ".all_ints"
            ; ExtStr = ".ints"
            ),
            ModuleTarget = module_target_int1
        ;
            ( ExtStr = ".all_int2s"
            ; ExtStr = ".int2s"
            ),
            ModuleTarget = module_target_int2
        ;
            ( ExtStr = ".all_int3s"
            ; ExtStr = ".int3s"
            ),
            ModuleTarget = module_target_int3
        ;
            ( ExtStr = ".all_opts"
            ; ExtStr = ".opts"
            ),
            ModuleTarget = module_target_opt
    % Not yet implemented. `build_all' targets are only used by
    % tools/bootcheck, so it doesn't really matter.
    %   ;
    %       ( ExtStr = ".all_mihs"
    %       ; ExtStr = ".mihs"
    %       ),
    %       ModuleTarget = module_target_c_header(header_mih)
    %   ;
    %       ( ExtStr = ".all_mhs"
    %       ; ExtStr = ".mhs"
    %       ),
    %       ModuleTarget = module_target_c_header(header_mh)
        ;
            ( ExtStr = ".all_cs"
            % ; ExtStr = ".cs"              % Duplicates C# target.
            ),
            ModuleTarget = module_target_c_code
        ;
            ( ExtStr = ".all_css"
            ; ExtStr = ".css"
            ),
            ModuleTarget = module_target_csharp_code
        ;
            ( ExtStr = ".all_csharps"
            ; ExtStr = ".csharps"
            ),
            % For a long time, until 2023 oct 5, we treated the ".cs" target
            % name as the build-all target for C files, so we accepted
            % ".csharp" as the target name for C# files. Keep the synonym
            % for a while longer to give people time to update their
            % projects and habits.
            ModuleTarget = module_target_csharp_code
        ;
            ( ExtStr = ".all_javas"
            ; ExtStr = ".javas"
            ),
            ModuleTarget = module_target_java_code
        ;
            % We should s/classs/classes/ here, but this requires
            % documentation and announcement, as well as updating all existing
            % references to the old spelling in the Mercury system.
            ( ExtStr = ".all_classs"
            ; ExtStr = ".classs"
            ),
            ModuleTarget = module_target_java_class_code
        ;
            ( ExtStr = ".all_track_flagss"
            ; ExtStr = ".track_flagss"
            ),
            ModuleTarget = module_target_track_flags
        ;
            ( ExtStr = ".all_xmls"
            ; ExtStr = ".xmls"
            ),
            ModuleTarget = module_target_xml_doc
        ;
            % We should s/analysiss/analyses/ here, but this requires
            % documentation and announcement, as well as updating all existing
            % references to the old spelling in the Mercury system.
            ( ExtStr = ".all_analysiss"
            ; ExtStr = ".analysiss"
            ),
            ModuleTarget = module_target_analysis_registry
        ),
        TargetType = misc_target(misc_target_build_all(ModuleTarget))
    ;
        (
            ExtStr = ".check",
            MiscTarget = misc_target_build_all(module_target_errors)
        ;
            ExtStr = ".doc",
            MiscTarget = misc_target_build_xml_docs
        ;
            ExtStr = ".analyse",
            MiscTarget = misc_target_build_analyses
        ;
            ExtStr = ".clean",
            MiscTarget = misc_target_clean
        ;
            ExtStr = ".realclean",
            MiscTarget = misc_target_realclean
        ),
        TargetType = misc_target(MiscTarget)
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
