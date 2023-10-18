%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 expandtab
%---------------------------------------------------------------------------%
% Copyright (C) 2002-2009, 2011 The University of Melbourne.
% Copyright (C) 2014-2017, 2019-2020 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: make.get_module_dep_info.m.
% Author: stayl.
%
% Code to find module_dep_infos, either by reading `<module>.module_dep'
% files, or by reading a module's source file and creating that file.
%
%---------------------------------------------------------------------------%

:- module make.get_module_dep_info.
:- interface.

:- import_module libs.
:- import_module libs.globals.
:- import_module make.make_info.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.

:- import_module io.

%---------------------------------------------------------------------------%

    % Get the module_dep_info for a given module.
    %
    % The information in that data structure is generated on demand,
    % not by a `mmc --make depend' command, so this predicate may need
    % to read the source for the module.
    %
:- pred get_maybe_module_dep_info(io.text_output_stream::in, globals::in,
    module_name::in, maybe_module_dep_info::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.file_util.
:- import_module libs.maybe_util.
:- import_module libs.options.
:- import_module libs.process_util.
:- import_module libs.timestamp.
:- import_module make.build.
:- import_module make.file_names.
:- import_module make.module_dep_file.
:- import_module make.module_target.
:- import_module make.timestamp.
:- import_module make.util.
:- import_module parse_tree.
:- import_module parse_tree.file_names.
:- import_module parse_tree.find_module.
:- import_module parse_tree.item_util.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.module_baggage.
:- import_module parse_tree.module_dep_info.
:- import_module parse_tree.parse_error.
:- import_module parse_tree.parse_tree_out_sym_name.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_item.
:- import_module parse_tree.read_modules.
:- import_module parse_tree.write_error_spec.
:- import_module parse_tree.write_module_interface_files.

:- import_module bool.
:- import_module dir.
:- import_module getopt.
:- import_module io.file.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module set.
:- import_module string.

%---------------------------------------------------------------------------%

get_maybe_module_dep_info(ProgressStream, Globals, ModuleName,
        MaybeModuleDepInfo, !Info, !IO) :-
    RebuildModuleDeps = make_info_get_rebuild_module_deps(!.Info),
    (
        ModuleName = unqualified(_),
        maybe_get_maybe_module_dep_info(ProgressStream, Globals,
            RebuildModuleDeps, ModuleName, MaybeModuleDepInfo, !Info, !IO)
    ;
        ModuleName = qualified(_, _),
        % For submodules, we need to generate the dependencies for the
        % parent modules first (try_to_write_module_dep_files_for_top_module
        % expects to be given the top-level module in a source file).
        % If the module is a nested module, its dependencies will be generated
        % as a side effect of generating the parent's dependencies.
        AncestorsAndSelf = get_ancestors(ModuleName) ++ [ModuleName],
        Error0 = no,
        maybe_record_modules_maybe_module_dep_infos(ProgressStream, Globals,
            RebuildModuleDeps, AncestorsAndSelf, Error0, !Info, !IO),

        ModuleDepMap = make_info_get_module_dependencies(!.Info),
        map.lookup(ModuleDepMap, ModuleName, MaybeModuleDepInfo)
    ).

:- pred maybe_record_modules_maybe_module_dep_infos(io.text_output_stream::in,
    globals::in, maybe_rebuild_module_deps::in, list(module_name)::in,
    bool::in, make_info::in, make_info::out, io::di, io::uo) is det.

maybe_record_modules_maybe_module_dep_infos(_, _, _, [], _, !Info, !IO).
maybe_record_modules_maybe_module_dep_infos(ProgressStream, Globals,
        RebuildModuleDeps, [ModuleName | ModuleNames], !.Error, !Info, !IO) :-
    (
        !.Error = no,
        maybe_get_maybe_module_dep_info(ProgressStream, Globals,
            RebuildModuleDeps, ModuleName, MaybeModuleDepInfo, !Info, !IO),
        (
            MaybeModuleDepInfo = some_module_dep_info(_)
        ;
            MaybeModuleDepInfo = no_module_dep_info,
            !:Error = yes
        )
    ;
        !.Error = yes,
        % If we found a problem when processing an ancestor, don't even try
        % to process the later modules.
        ModuleDepMap0 = make_info_get_module_dependencies(!.Info),
        % XXX Could this be map.det_update or map.det_insert?
        map.set(ModuleName, no_module_dep_info, ModuleDepMap0, ModuleDepMap),
        make_info_set_module_dependencies(ModuleDepMap, !Info)
    ),
    maybe_record_modules_maybe_module_dep_infos(ProgressStream, Globals,
        RebuildModuleDeps, ModuleNames, !.Error, !Info, !IO).

:- pred maybe_get_maybe_module_dep_info(io.text_output_stream::in, globals::in,
    maybe_rebuild_module_deps::in, module_name::in, maybe_module_dep_info::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

maybe_get_maybe_module_dep_info(ProgressStream, Globals, RebuildModuleDeps,
        ModuleName, MaybeModuleDepInfo, !Info, !IO) :-
    ModuleDepMap0 = make_info_get_module_dependencies(!.Info),
    ( if map.search(ModuleDepMap0, ModuleName, MaybeModuleDepInfo0) then
        MaybeModuleDepInfo = MaybeModuleDepInfo0
    else
        do_get_maybe_module_dep_info(ProgressStream, Globals,
            RebuildModuleDeps, ModuleName, MaybeModuleDepInfo, !Info, !IO)
    ).

:- pred do_get_maybe_module_dep_info(io.text_output_stream::in, globals::in,
    maybe_rebuild_module_deps::in, module_name::in, maybe_module_dep_info::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

do_get_maybe_module_dep_info(ProgressStream, Globals, RebuildModuleDeps,
        ModuleName, !:MaybeModuleDepInfo, !Info, !IO) :-
    globals.lookup_accumulating_option(Globals, search_directories,
        SearchDirs),
    ModuleDepExt = ext_cur_ngs(ext_cur_ngs_misc_module_dep),
    module_name_to_file_name(Globals, $pred, ModuleDepExt,
        ModuleName, DepFileName),
    get_file_timestamp(SearchDirs, DepFileName, MaybeDepFileTimestamp,
        !Info, !IO),
    (
        MaybeDepFileTimestamp = ok(DepFileTimestamp),
        % We can't just use
        %   `get_target_timestamp(ModuleName - source, ..)'
        % because that could recursively call get_maybe_module_dep_info,
        % leading to an infinite loop. Just using module_name_to_file_name
        % will fail if the module name doesn't match the file name, but
        % that case is handled below.
        module_name_to_source_file_name(ModuleName, SourceFileName, !IO),
        get_file_timestamp([dir.this_directory], SourceFileName,
            MaybeSourceFileTimestamp, !Info, !IO),
        (
            MaybeSourceFileTimestamp = ok(SourceFileTimestamp),
            ( if
                ( RebuildModuleDeps = do_not_rebuild_module_deps
                ; compare((>), DepFileTimestamp, SourceFileTimestamp)
                )
            then
                % Since the source file was found in this directory,
                % do not use module_dep files which might be for
                % installed copies of the module.
                %
                % XXX SourceFileName may not actually be the correct source
                % file for the required module. Usually the purported source
                % file would have a later timestamp than the .module_dep file,
                % though, so the other branch would be taken.
                find_and_read_module_dep_file(ProgressStream, Globals,
                    RebuildModuleDeps, [dir.this_directory], ModuleName,
                    !Info, !IO)
            else
                try_to_write_module_dep_files_for_top_module(ProgressStream,
                    Globals, ModuleName, !Info, !IO)
            )
        ;
            MaybeSourceFileTimestamp = error(_),
            find_and_read_module_dep_file(ProgressStream, Globals,
                RebuildModuleDeps, SearchDirs, ModuleName, !Info, !IO),

            % Check for the case where the module name doesn't match the
            % source file name (e.g. parse.m contains module mdb.parse).
            % Get the correct source file name from the module dependency file,
            % then check whether the module dependency file is up to date.
            map.lookup(make_info_get_module_dependencies(!.Info), ModuleName,
                !:MaybeModuleDepInfo),
            ( if
                !.MaybeModuleDepInfo = some_module_dep_info(ModuleDepInfo0),
                module_dep_info_get_source_file_dir(ModuleDepInfo0,
                    DepFileDir),
                DepFileDir = dir.this_directory
            then
                module_dep_info_get_source_file_name(ModuleDepInfo0,
                    SourceFileName1),
                get_file_timestamp([dir.this_directory], SourceFileName1,
                    MaybeSourceFileTimestamp1, !Info, !IO),
                (
                    MaybeSourceFileTimestamp1 = ok(SourceFileTimestamp1),
                    ( if
                        ( RebuildModuleDeps = do_not_rebuild_module_deps
                        ; compare((>), DepFileTimestamp, SourceFileTimestamp1)
                        )
                    then
                        true
                    else
                        % XXX The existence of a .module_dep file reflects
                        % a previous state of the workspace, which may not
                        % match the current workspace.
                        %
                        % Here is a (contrived) case where we run into
                        % an issue:
                        % 1. create prog.m which imports the standard library
                        %    mercury_term_lexer module
                        % 2. copy the standard library mercury_term_lexer.m
                        %    file to the current directory for editing
                        % 3. run mmc --make; it creates
                        %    mercury_term_lexer.module_dep
                        % 4. change mercury_term_lexer.m into a submodule
                        %    of prog
                        % 5. run mmc --make again, it no longer works
                        %
                        % The local mercury_term_lexer.module_dep prevents
                        % mmc --make finding the mercury_term_lexer.module_dep
                        % from the standard library, even though there is
                        % no longer any local source file for the
                        % `mercury_term_lexer' module.
                        try_to_write_module_dep_files_for_top_module(
                            ProgressStream, Globals, ModuleName, !Info, !IO)
                    )
                ;
                    MaybeSourceFileTimestamp1 = error(Message),
                    io.format(ProgressStream,
                        "** Error reading file `%s' " ++
                        "to generate dependencies: %s.\n",
                        [s(SourceFileName1), s(Message)], !IO),
                    maybe_write_importing_module(ProgressStream, ModuleName,
                        make_info_get_importing_module(!.Info), !IO)
                )
            else
                true
            )
        )
    ;
        MaybeDepFileTimestamp = error(_),
        SearchDirsString = join_list(", ",
            map((func(Dir) = "`" ++ Dir ++ "'"), SearchDirs)),
        debug_make_msg(Globals,
            string.format(
                "Module dependencies file '%s' not found in directories %s.\n",
                [s(DepFileName), s(SearchDirsString)]),
            DebugMsg),
        maybe_write_msg(ProgressStream, DebugMsg, !IO),

        % Try to make the dependencies. This will succeed when the module name
        % doesn't match the file name and the dependencies for this module
        % haven't been built before. It will fail if the source file
        % is in another directory.
        (
            RebuildModuleDeps = do_rebuild_module_deps,
            try_to_write_module_dep_files_for_top_module(ProgressStream,
                Globals, ModuleName, !Info, !IO)
        ;
            RebuildModuleDeps = do_not_rebuild_module_deps,
            ModuleDepMap0 = make_info_get_module_dependencies(!.Info),
            % XXX Could this be map.det_update or map.det_insert?
            map.set(ModuleName, no_module_dep_info,
                ModuleDepMap0, ModuleDepMap1),
            make_info_set_module_dependencies(ModuleDepMap1, !Info)
        )
    ),
    ModuleDepMap2 = make_info_get_module_dependencies(!.Info),
    ( if map.search(ModuleDepMap2, ModuleName, MaybeModuleDepInfo0) then
        !:MaybeModuleDepInfo = MaybeModuleDepInfo0
    else
        !:MaybeModuleDepInfo = no_module_dep_info,
        map.det_insert(ModuleName, no_module_dep_info,
            ModuleDepMap2, ModuleDepMap),
        make_info_set_module_dependencies(ModuleDepMap, !Info)
    ).

%---------------------------------------------------------------------------%

:- pred find_and_read_module_dep_file(io.text_output_stream::in, globals::in,
    maybe_rebuild_module_deps::in, list(dir_name)::in, module_name::in,
    make_info::in, make_info::out, io::di, io::uo) is det.

find_and_read_module_dep_file(ProgressStream, Globals, RebuildModuleDeps,
        SearchDirs, ModuleName, !Info, !IO) :-
    module_name_to_search_file_name(Globals, $pred,
        ext_cur_ngs(ext_cur_ngs_misc_module_dep), ModuleName, DepFileName),
    search_for_file_returning_dir_and_contents(SearchDirs, DepFileName,
        MaybeDirAndContents, !IO),
    (
        MaybeDirAndContents = ok(DirAndContents),
        DirAndContents = path_name_and_contents(DepFileDir, DepFileContents),
        read_module_dep_file(DepFileDir, DepFileName, DepFileContents,
            ModuleName, ParseResult),
        (
            ParseResult = ok1(ModuleSummary),
            handle_parsed_module_dep_file(ProgressStream, Globals,
                SearchDirs, ModuleName, DepFileDir, DepFileName,
                ModuleSummary, Result, !Info, !IO)
        ;
            ParseResult = error1(ParseErrorMsg),
            Result = error(ParseErrorMsg)
        ),
        (
            Result = ok
        ;
            Result = error(ErrorMsg),
            error_and_maybe_rebuilding_msg(RebuildModuleDeps,
                DepFileDir ++ "/" ++ DepFileName, ErrorMsg, Msg),
            % XXX MAKE_STREAM
            io.write_string(ProgressStream, Msg, !IO),
            (
                RebuildModuleDeps = do_rebuild_module_deps,
                try_to_write_module_dep_files_for_top_module(ProgressStream,
                    Globals, ModuleName, !Info, !IO)
            ;
                RebuildModuleDeps = do_not_rebuild_module_deps
            )
        )
    ;
        MaybeDirAndContents = error(ErrorMsg),
        % XXX Why is this invocation of error_and_maybe_rebuilding_msg
        % more conditional than the one above?
        debug_make_msg(Globals,
            error_and_maybe_rebuilding_msg(RebuildModuleDeps,
                DepFileName, ErrorMsg),
            DebugMsg),
        maybe_write_msg(ProgressStream, DebugMsg, !IO),
        (
            RebuildModuleDeps = do_rebuild_module_deps,
            try_to_write_module_dep_files_for_top_module(ProgressStream,
                Globals, ModuleName, !Info, !IO)
        ;
            RebuildModuleDeps = do_not_rebuild_module_deps
        )
    ).

:- pred handle_parsed_module_dep_file(io.text_output_stream::in, globals::in,
    list(dir_name)::in, module_name::in, dir_name::in, file_name::in,
    module_dep_summary::in, maybe_error::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

handle_parsed_module_dep_file(ProgressStream, Globals, SearchDirs, ModuleName,
        DepFileDir, DepFileName, ModuleSummary, Result, !Info, !IO) :-
    ModuleDepInfo = module_dep_info_summary(ModuleSummary),
    MaybeModuleDepInfo = some_module_dep_info(ModuleDepInfo),

    % Discard the module dependencies if the module is a local module
    % but the source file no longer exists.
    ( if DepFileDir = dir.this_directory then
        SourceFileName = ModuleSummary ^ mds_source_file_name,
        check_regular_file_exists(SourceFileName, SourceFileExists, !IO),
        (
            SourceFileExists = ok
        ;
            SourceFileExists = error(_),
            io.file.remove_file(DepFileName, _, !IO)
        )
    else
        SourceFileExists = ok
    ),
    (
        SourceFileExists = ok,
        ModuleDepMap0 = make_info_get_module_dependencies(!.Info),
        % XXX Could this be map.det_insert?
        map.set(ModuleName, MaybeModuleDepInfo,
            ModuleDepMap0, ModuleDepMap),
        make_info_set_module_dependencies(ModuleDepMap, !Info),

        % Read the dependencies for any nested children. If something
        % goes wrong (for example one of the files was removed), the
        % dependencies for all modules in the source file will be remade
        % (try_to_write_module_dep_files_for_top_module expects
        % to be given the top-level module in the source file).
        MaybeTopModule = ModuleSummary ^ mds_maybe_top_module,
        NestedSubModules =
            get_nested_children_list_of_top_module(MaybeTopModule),
        list.foldl2(
            find_and_read_module_dep_file(ProgressStream, Globals,
                do_not_rebuild_module_deps, SearchDirs),
            NestedSubModules, !Info, !IO),
        ( if some_bad_module_dependency(!.Info, NestedSubModules) then
            Result = error("error in nested submodules")
        else
            Result = ok
        )
    ;
        SourceFileExists = error(Error),
        Result = error(Error)
    ).

%---------------------------------------------------------------------------%

:- pred some_bad_module_dependency(make_info::in, list(module_name)::in)
    is semidet.

some_bad_module_dependency(Info, ModuleNames) :-
    list.member(ModuleName, ModuleNames),
    map.search(make_info_get_module_dependencies(Info), ModuleName,
        no_module_dep_info).

:- pred check_regular_file_exists(file_name::in, maybe_error::out,
    io::di, io::uo) is det.

check_regular_file_exists(FileName, FileExists, !IO) :-
    FollowSymLinks = yes,
    io.file.file_type(FollowSymLinks, FileName, ResFileType, !IO),
    (
        ResFileType = ok(FileType),
        (
            ( FileType = regular_file
            ; FileType = unknown
            ),
            FileExists = ok
        ;
            ( FileType = directory
            ; FileType = symbolic_link
            ; FileType = named_pipe
            ; FileType = socket
            ; FileType = character_device
            ; FileType = block_device
            ; FileType = message_queue
            ; FileType = semaphore
            ; FileType = shared_memory
            ),
            FileExists = error(FileName ++ ": not a regular file")
        )
    ;
        ResFileType = error(Error),
        FileExists = error(FileName ++ ": " ++ io.error_message(Error))
    ).

%---------------------------------------------------------------------------%

:- pred error_and_maybe_rebuilding_msg(maybe_rebuild_module_deps::in,
    string::in, string::in, string::out) is det.

error_and_maybe_rebuilding_msg(RebuildModuleDeps, ModuleDepsFile,
        ErrorMsg, Msg) :-
    (
        RebuildModuleDeps = do_rebuild_module_deps,
        RebuildSuffix = " ...rebuilding"
    ;
        RebuildModuleDeps = do_not_rebuild_module_deps,
        RebuildSuffix = ""
    ),
    string.format("** Error reading file `%s': %s%s\n",
        [s(ModuleDepsFile), s(ErrorMsg), s(RebuildSuffix)], Msg).

    % The module_name given must be the top level module in the source file.
    % get_module_dependencies ensures this by making the dependencies
    % for all parent modules of the requested module first.
    %
:- pred try_to_write_module_dep_files_for_top_module(io.text_output_stream::in,
    globals::in, module_name::in,
    make_info::in, make_info::out, io::di, io::uo) is det.

try_to_write_module_dep_files_for_top_module(ProgressStream, Globals,
        ModuleName, !Info, !IO) :-
    open_module_error_stream(ProgressStream, Globals, ModuleName,
        MaybeErrorStream, !Info, !IO),
    (
        MaybeErrorStream = es_ok(MESI, ErrorStream),
        % Both cannot_write_module_dep_files and
        % write_module_dep_files_for_source_file call
        % close_module_error_stream_handle_errors,
        % but factoring that call out of the latter would be non-trivial,
        % and is better left for when we replace the whole redirect/unredirect
        % machinery with the use of explicit streams.
        % XXX Why ask for the timestamp if we then ignore it?
        % NOTE: Asking for a timestamp and then ignoring it *could* make sense
        % if we recorded HaveReadSrc in a have_read_module_map, because
        % it would make the timestamp available for a later lookup,
        % However, we do not record HaveReadSrc in a have_read_module_map.
        read_module_src(ProgressStream, Globals, rrm_get_deps,
            do_not_ignore_errors, do_not_search, ModuleName, [],
            always_read_module(do_return_timestamp), HaveReadSrc, !IO),
        % XXX If execution will follow a path on which we call
        % cannot_write_module_dep_files, then as its almost-last act,
        % cannot_write_module_dep_files will delete Module.err.
        % Since that is a possibility, why write to ModuleName.err
        % *unconditionally*?
        (
            HaveReadSrc = have_read_module(SourceFileName, MaybeTimestamp,
                ParseTreeSrc, ReadModuleErrors),
            FatalErrorSpecs0 = ReadModuleErrors ^ rm_fatal_error_specs,
            NonFatalErrorSpecs0 = ReadModuleErrors ^ rm_nonfatal_error_specs,
            write_error_specs(ErrorStream, Globals,
                FatalErrorSpecs0 ++ NonFatalErrorSpecs0, !IO),

            Fatal = ReadModuleErrors ^ rm_fatal_errors,
            NonFatal = ReadModuleErrors ^ rm_nonfatal_errors,
            ( if set.is_non_empty(Fatal) then
                DisplayErrorReadingFile = yes,
                cannot_write_module_dep_files(Globals,
                    ProgressStream, MESI, ErrorStream, SourceFileName,
                    ModuleName, ReadModuleErrors, DisplayErrorReadingFile,
                    !Info, !IO)
            else if set.contains(NonFatal, rme_unexpected_module_name) then
                % If the source file does not contain the expected module,
                % then do not make the .module_dep file; it would leave
                % a .module_dep file for the wrong module lying around,
                % which the user needs to delete manually.
                DisplayErrorReadingFile = no,
                cannot_write_module_dep_files(Globals,
                    ProgressStream, MESI, ErrorStream, SourceFileName,
                    ModuleName, ReadModuleErrors, DisplayErrorReadingFile,
                    !Info, !IO)
            else
                write_module_dep_files_for_source_file(Globals,
                    ProgressStream, MESI, ErrorStream, SourceFileName,
                    ModuleName, ReadModuleErrors, MaybeTimestamp,
                    ParseTreeSrc, !Info, !IO)
            )
        ;
            HaveReadSrc = have_not_read_module(SourceFileName,
                ReadModuleErrors),
            FatalErrorSpecs0 = ReadModuleErrors ^ rm_fatal_error_specs,
            NonFatalErrorSpecs0 = ReadModuleErrors ^ rm_nonfatal_error_specs,
            write_error_specs(ErrorStream, Globals,
                FatalErrorSpecs0 ++ NonFatalErrorSpecs0, !IO),

            DisplayErrorReadingFile = yes,
            cannot_write_module_dep_files(Globals,
                ProgressStream, MESI, ErrorStream, SourceFileName, ModuleName,
                ReadModuleErrors, DisplayErrorReadingFile, !Info, !IO)
        )
    ;
        MaybeErrorStream = es_error_already_reported
    ).

:- pred cannot_write_module_dep_files(globals::in,
    io.text_output_stream::in,
    module_error_stream_info::in, io.text_output_stream::in,
    file_name::in, module_name::in, read_module_errors::in, bool::in,
    make_info::in, make_info::out, io::di, io::uo) is det.

cannot_write_module_dep_files(Globals, ProgressStream, MESI, ErrorStream,
        SourceFileName, ModuleName, ReadModuleErrors,
        DisplayErrorReadingFile, !Info, !IO) :-
    Specs0 = get_read_module_specs(ReadModuleErrors),
    write_error_specs(ErrorStream, Globals, Specs0, !IO),
    (
        DisplayErrorReadingFile = yes,
        io.format(ProgressStream,
            "** Error reading file `%s' to generate dependencies.\n",
            [s(SourceFileName)], !IO),
        maybe_write_importing_module(ProgressStream, ModuleName,
            make_info_get_importing_module(!.Info), !IO)
    ;
        DisplayErrorReadingFile = no
    ),

    % Display the contents of the `.err' file, then remove it
    % so we don't leave `.err' files lying around for nonexistent modules.
    globals.set_option(output_compile_error_lines, maybe_int(no),
        Globals, UnredirectGlobals),
    close_module_error_stream_handle_errors(ProgressStream, UnredirectGlobals,
        ModuleName, MESI, ErrorStream, !Info, !IO),
    module_name_to_file_name(Globals, $pred, ext_cur(ext_cur_user_err),
        ModuleName, ErrFileName),
    io.file.remove_file(ErrFileName, _, !IO),

    ModuleDepMap0 = make_info_get_module_dependencies(!.Info),
    % XXX Could this be map.det_update?
    map.set(ModuleName, no_module_dep_info, ModuleDepMap0, ModuleDepMap),
    make_info_set_module_dependencies(ModuleDepMap, !Info).

:- pred write_module_dep_files_for_source_file(globals::in,
    io.text_output_stream::in,
    module_error_stream_info::in, io.text_output_stream::in,
    file_name::in, module_name::in, read_module_errors::in,
    maybe(timestamp)::in, parse_tree_src::in,
    make_info::in, make_info::out, io::di, io::uo) is det.

write_module_dep_files_for_source_file(Globals, ProgressStream,
        MESI, ErrorStream, SourceFileName, ModuleName, ReadModuleErrors,
        MaybeTimestamp, ParseTreeSrc, !Info, !IO) :-
    parse_tree_src_to_burdened_module_list(Globals, SourceFileName,
        ReadModuleErrors, MaybeTimestamp, ParseTreeSrc,
        Specs, BurdenedModules),
    ParseTreeModuleSrcs = list.map((func(burdened_module(_, PTMS)) = PTMS),
        BurdenedModules),
    SubModuleNames = list.map(parse_tree_module_src_project_name,
         ParseTreeModuleSrcs),

    % XXX Why are we ignoring all previously reported errors?
    io.set_exit_status(0, !IO),
    write_error_specs(ErrorStream, Globals, Specs, !IO),

    list.foldl(make_info_add_burdened_module_as_dep, BurdenedModules, !Info),

    % If there were no errors, write out the `.int3' file
    % while we have the contents of the module. The `int3' file
    % does not depend on anything else.
    MadeTarget = target_file(ModuleName, module_target_int3),
    get_make_target_file_name(Globals, $pred,
        MadeTarget, MadeTargetFileName, !IO),

    globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
    % We already know FatalReadError is empty.
    NonFatalErrors = ReadModuleErrors ^ rm_nonfatal_errors,
    ( if set.is_empty(NonFatalErrors) then
        maybe_making_filename_msg(Globals, MadeTargetFileName, MakingMsg),
        maybe_write_msg(ProgressStream, MakingMsg, !IO),
        setup_checking_for_interrupt(CookieMSI, !IO),

        globals.get_default_options(Globals, DefaultOptionTable),
        DetectedGradeFlags = make_info_get_detected_grade_flags(!.Info),
        OptionVariables = make_info_get_options_variables(!.Info),
        OptionArgs = make_info_get_option_args(!.Info),
        ExtraOptions = ["--make-short-interface"],
        setup_for_build_with_module_options(ProgressStream, DefaultOptionTable,
            invoked_by_mmc_make, ModuleName, DetectedGradeFlags,
            OptionVariables, OptionArgs, ExtraOptions, MayBuild, !IO),
        (
            MayBuild = may_not_build(MSISpecs),
            % XXX MAKE_STREAM
            write_error_specs(ErrorStream, Globals, MSISpecs, !IO),
            Succeeded0 = did_not_succeed
        ;
            MayBuild = may_build(_AllOptions, BuildGlobals),
            make_int3_files(ProgressStream, ErrorStream, BuildGlobals,
                ParseTreeModuleSrcs, Succeeded0, !Info, !IO)
        ),

        CleanupMSI =
            cleanup_int3_files(ProgressStream, Globals, SubModuleNames),
        teardown_checking_for_interrupt(VeryVerbose, CookieMSI,
            CleanupMSI, Succeeded0, Succeeded, !Info, !IO)
    else
        Succeeded = did_not_succeed
    ),

    setup_checking_for_interrupt(CookieWMDF, !IO),
    list.foldl(do_write_module_dep_file(ProgressStream, Globals),
        BurdenedModules, !IO),
    CleanupWMDF =
        cleanup_module_dep_files(ProgressStream, Globals, SubModuleNames),
    teardown_checking_for_interrupt(VeryVerbose, CookieWMDF,
        CleanupWMDF, succeeded, _Succeeded, !Info, !IO),

    record_made_target(ProgressStream, Globals, MadeTarget, MadeTargetFileName,
        process_module(task_make_int3), Succeeded, !Info, !IO),
    close_module_error_stream_handle_errors(ProgressStream, Globals,
        ModuleName, MESI, ErrorStream, !Info, !IO).

:- pred make_info_add_burdened_module_as_dep(burdened_module::in,
    make_info::in, make_info::out) is det.

make_info_add_burdened_module_as_dep(BurdenedModule, !Info) :-
    ParseTreeModuleSrc = BurdenedModule ^ bm_module,
    ModuleName = ParseTreeModuleSrc ^ ptms_module_name,
    ModuleDepInfo = module_dep_info_full(BurdenedModule),
    MaybeModuleDepInfo = some_module_dep_info(ModuleDepInfo),
    ModuleDepMap0 = make_info_get_module_dependencies(!.Info),
    % XXX Could this be map.det_insert?
    map.set(ModuleName, MaybeModuleDepInfo, ModuleDepMap0, ModuleDepMap),
    make_info_set_module_dependencies(ModuleDepMap, !Info).

:- pred make_int3_files(io.text_output_stream::in, io.text_output_stream::in,
    globals::in, list(parse_tree_module_src)::in, maybe_succeeded::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

make_int3_files(ProgressStream, ErrorStream, Globals,
        ParseTreeModuleSrcs, Succeeded, !Info, !IO) :-
    % XXX MAKE We should probably add to, and keep, HaveReadModuleMaps.
    list.map2_foldl2(
        write_short_interface_file_int3(ProgressStream, ErrorStream,
            Globals, do_not_add_new_to_hrmm),
        ParseTreeModuleSrcs, Succeededs, SpecsList,
        init_have_read_module_maps, _HaveReadModuleMaps, !IO),
    list.foldl(write_error_specs(ErrorStream, Globals), SpecsList, !IO),
    Succeeded = and_list(Succeededs).

:- pred cleanup_int3_files(io.text_output_stream::in, globals::in,
    list(module_name)::in,
    make_info::in, make_info::out, io::di, io::uo) is det.

cleanup_int3_files(ProgressStream, Globals, ModuleNames, !Info, !IO) :-
    list.foldl2(cleanup_int3_file(ProgressStream, Globals), ModuleNames,
        !Info, !IO).

:- pred cleanup_int3_file(io.text_output_stream::in, globals::in,
    module_name::in, make_info::in, make_info::out, io::di, io::uo) is det.

cleanup_int3_file(ProgressStream, Globals, ModuleName, !Info, !IO) :-
    remove_make_target_file_by_name(ProgressStream, Globals, $pred,
        very_verbose, ModuleName, module_target_int3, !Info, !IO).

:- pred cleanup_module_dep_files(io.text_output_stream::in, globals::in,
    list(module_name)::in,
    make_info::in, make_info::out, io::di, io::uo) is det.

cleanup_module_dep_files(ProgressStream, Globals, ModuleNames, !Info, !IO) :-
    list.foldl2(cleanup_module_dep_file(ProgressStream, Globals),
        ModuleNames, !Info, !IO).

:- pred cleanup_module_dep_file(io.text_output_stream::in, globals::in,
    module_name::in, make_info::in, make_info::out, io::di, io::uo) is det.

cleanup_module_dep_file(ProgressStream, Globals, ModuleName, !Info, !IO) :-
    remove_module_file_for_make(ProgressStream, Globals, verbose_make,
        ModuleName, ext_cur_ngs(ext_cur_ngs_misc_module_dep), !Info, !IO).

:- pred maybe_write_importing_module(io.text_output_stream::in,
    module_name::in, maybe(import_or_include)::in, io::di, io::uo) is det.

maybe_write_importing_module(ProgressStream, ModuleName, MaybeIoI, !IO) :-
    (
        MaybeIoI = no
    ;
        MaybeIoI = yes(ImportOrInclude),
        (
            ImportOrInclude = ioi_import(ImportingModuleName),
            io.format(ProgressStream,
                "** Module `%s' is imported by module `%s'.\n",
                [s(escaped_sym_name_to_string(ModuleName)),
                s(escaped_sym_name_to_string(ImportingModuleName))], !IO)
        ;
            ImportOrInclude = ioi_include(IncludingModuleName),
            io.format(ProgressStream,
                "** Module `%s' is included by module `%s'.\n",
                [s(escaped_sym_name_to_string(ModuleName)),
                s(escaped_sym_name_to_string(IncludingModuleName))], !IO)
        )
    ).

%---------------------------------------------------------------------------%
:- end_module make.get_module_dep_info.
%---------------------------------------------------------------------------%
