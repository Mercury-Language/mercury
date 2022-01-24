%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2002-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: make.util.m.
% Authors: stayl, wangp.
%
% Assorted predicates used to implement `mmc --make'.
%
%---------------------------------------------------------------------------%

:- module make.util.
:- interface.

:- import_module backend_libs.
:- import_module backend_libs.compile_target_code.
:- import_module libs.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module libs.timestamp.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module make.dependencies.
:- import_module make.make_info.
:- import_module parse_tree.
:- import_module parse_tree.file_names.

:- import_module io.
:- import_module list.
:- import_module maybe.
:- import_module pair.

%---------------------------------------------------------------------------%
%
% Timestamp handling.
%

    % Find the timestamp updated when a target is produced.
    %
:- pred get_timestamp_file_timestamp(globals::in, target_file::in,
    maybe_error(timestamp)::out, make_info::in, make_info::out,
    io::di, io::uo) is det.

    % Find the timestamp for the given dependency file.
    %
:- pred get_dependency_timestamp(globals::in, dependency_file::in,
    maybe_error(timestamp)::out, make_info::in, make_info::out,
    io::di, io::uo) is det.

    % get_target_timestamp(Globals, Search, TargetFile, Timestamp)
    %
    % Find the timestamp for the given target file.
    % `Search' should be `do_search' if the file could be part of an
    % installed library.
    %
:- pred get_target_timestamp(globals::in, maybe_search::in, target_file::in,
    maybe_error(timestamp)::out, make_info::in, make_info::out,
    io::di, io::uo) is det.

    % get_file_name(Globals, Search, TargetFile, FileName, !IO):
    %
    % Compute a file name for the given target file.
    % `Search' should be `do_search' if the file could be part of an
    % installed library.
    %
:- pred get_file_name(globals::in, maybe_search::in, target_file::in,
    file_name::out, make_info::in, make_info::out, io::di, io::uo) is det.

    % Find the timestamp of the first file matching the given
    % file name in one of the given directories.
    %
:- pred get_file_timestamp(list(dir_name)::in, file_name::in,
    maybe_error(timestamp)::out, make_info::in, make_info::out,
    io::di, io::uo) is det.

    % If any of the inputs contain an error, return the first error.
    % XXX We should return all errors, not just the first.
    % If none of the inputs contain an error, return the oldest timestamp.
    %
:- pred find_error_or_older_ok_timestamp(maybe_error(timestamp)::in,
    maybe_error(timestamp)::in, maybe_error(timestamp)::out) is det.
:- pred find_error_or_oldest_ok_timestamp(list(maybe_error(timestamp))::in,
    maybe_error(timestamp)::out) is det.

%---------------------------------------------------------------------------%
%
% Remove file a file, deleting the cached timestamp.
% The removal is reported to the user if the given boolean option is set.
% In general the option given should be `--very-verbose' when making a
% `.clean' or `.realclean target', and `--verbose-make' when cleaning
% after an interrupted build.
%

    % Remove the target file and the corresponding timestamp file.
    %
:- pred make_remove_target_file(globals::in, option::in, target_file::in,
    make_info::in, make_info::out, io::di, io::uo) is det.

    % Remove the target file and the corresponding timestamp file.
    %
:- pred make_remove_target_file_by_name(globals::in, option::in,
    module_name::in, module_target_type::in, make_info::in, make_info::out,
    io::di, io::uo) is det.

    % make_remove_module_file(Globals, VerboseOption, ModuleName, Extension,
    %   !Info, !IO).
    %
:- pred make_remove_module_file(globals::in, option::in, module_name::in,
    ext::in, make_info::in, make_info::out, io::di, io::uo) is det.

:- pred make_remove_file(globals::in, option::in, file_name::in,
    make_info::in, make_info::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- func make_target_file_list(list(module_name), module_target_type) =
    list(target_file).

:- func make_dependency_list(list(module_name), module_target_type)
    = list(dependency_file).

%---------------------------------------------------------------------------%

:- pred target_type_to_extension(globals::in, module_target_type::in,
    ext::out) is semidet.

:- pred extension_to_target_type(globals::in, string::in,
    module_target_type::out) is semidet.

:- pred target_extension_synonym(string::in, module_target_type::out)
    is semidet.

%---------------------------------------------------------------------------%

:- pred linked_target_file_name(globals::in, module_name::in,
    linked_target_type::in, file_name::out, io::di, io::uo) is det.

    % Find the extension for the timestamp file for the given target type,
    % if one exists.
    %
:- pred timestamp_extension(module_target_type::in, other_ext::out) is semidet.

:- pred target_is_grade_or_arch_dependent(module_target_type::in) is semidet.

%---------------------------------------------------------------------------%
%
% Debugging, verbose messages, and error messages.
%

    % Apply the given predicate if `--debug-make' is set.
    % XXX Do we need this, now that we have trace goals?
    %
:- pred debug_make_msg(globals::in, pred(io, io)::(pred(di, uo) is det),
    io::di, io::uo) is det.

    % Apply the given predicate if `--verbose-make' is set.
    % XXX Do we need this, now that we have trace goals?
    %
:- pred verbose_make_msg(globals::in, pred(io, io)::(pred(di, uo) is det),
    io::di, io::uo) is det.

    % Apply the given predicate if the given boolean option is set to `yes'.
    % XXX Do we need this, now that we have trace goals?
    %
:- pred verbose_make_msg_option(globals::in, option::in,
    pred(io, io)::(pred(di, uo) is det), io::di, io::uo) is det.

    % Write a debugging message relating to a given target file.
    %
:- pred debug_file_msg(globals::in, target_file::in, string::in,
    io::di, io::uo) is det.

:- pred dependency_file_to_file_name(globals::in, dependency_file::in,
    string::out, io::di, io::uo) is det.

:- pred make_write_dependency_file(globals::in, dependency_file::in,
    io::di, io::uo) is det.

:- pred make_write_dependency_file_list(globals::in, list(dependency_file)::in,
    io::di, io::uo) is det.

    % Return the file name for the given target_file. The I/O state pair
    % may be needed to find this file name.
    %
:- pred get_make_target_file_name(globals::in, target_file::in, string::out,
    io::di, io::uo) is det.

    % Write out the file name for the given target_file.
    %
:- pred make_write_target_file(globals::in, target_file::in,
    io::di, io::uo) is det.

:- pred make_write_target_file_wrapped(globals::in,
    string::in, target_file::in, string::in, io::di, io::uo) is det.
:- pred make_write_target_file_wrapped(io.text_output_stream::in, globals::in,
    string::in, target_file::in, string::in, io::di, io::uo) is det.

    % Write a message "Making <filename>" if `--verbose-make' is set.
    %
:- pred maybe_make_linked_target_message(globals::in, file_name::in,
    io::di, io::uo) is det.

    % Write a message "Making <filename>" if `--verbose-make' is set.
    %
:- pred maybe_make_target_message(globals::in, target_file::in,
    io::di, io::uo) is det.

:- pred maybe_make_target_message_to_stream(globals::in, io.output_stream::in,
    target_file::in, io::di, io::uo) is det.

    % Write a message "Reanalysing invalid/suboptimal modules" if
    % `--verbose-make' is set.
    %
:- pred maybe_reanalyse_modules_message(globals::in, io::di, io::uo) is det.

    % Write a message "** Error making <filename>".
    %
:- pred target_file_error(make_info::in, globals::in, target_file::in,
    io::di, io::uo) is det.

    % Write a message "** Error making <filename>".
    %
:- pred file_error(make_info::in, file_name::in, io::di, io::uo) is det.

    % If the given target was specified on the command line, warn that it
    % was already up to date.
    %
:- pred maybe_warn_up_to_date_target(globals::in,
    pair(module_name, target_type)::in,
    make_info::in, make_info::out, io::di, io::uo) is det.

    % Write a message "Made symlink/copy of <filename>"
    % if `--verbose-make' is set.
    %
:- pred maybe_symlink_or_copy_linked_target_message(globals::in,
    pair(module_name, target_type)::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%
% Timing.
%

:- pred get_real_milliseconds(int::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%
% Hash functions.
%

:- pred module_name_hash(module_name::in, int::out) is det.

:- pred dependency_file_hash(dependency_file::in, int::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module analysis.
:- import_module make.build.
:- import_module make.module_dep_file.
:- import_module parse_tree.module_dep_info.
:- import_module parse_tree.prog_foreign.
:- import_module transform_hlds.
:- import_module transform_hlds.mmc_analysis.

:- import_module bool.
:- import_module dir.
:- import_module int.
:- import_module map.
:- import_module require.
:- import_module set.
:- import_module string.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

get_timestamp_file_timestamp(Globals, target_file(ModuleName, TargetType),
        MaybeTimestamp, !Info, !IO) :-
    ( if timestamp_extension(TargetType, TimestampOtherExt) then
        module_name_to_file_name(Globals, $pred, do_not_create_dirs,
            ext_other(TimestampOtherExt), ModuleName, FileName, !IO)
    else
        module_target_to_file_name(Globals, do_not_create_dirs, TargetType,
            ModuleName, FileName, !IO)
    ),

    % We should only ever look for timestamp files in the current directory.
    % Timestamp files are only used when processing a module, and only modules
    % in the current directory are processed.
    SearchDirs = [dir.this_directory],
    get_file_timestamp(SearchDirs, FileName, MaybeTimestamp, !Info, !IO).

get_dependency_timestamp(Globals, DependencyFile, MaybeTimestamp, !Info,
        !IO) :-
    (
        DependencyFile = dep_file(FileName),
        SearchDirs = [dir.this_directory],
        get_file_timestamp(SearchDirs, FileName, MaybeTimestamp, !Info, !IO)
    ;
        DependencyFile = dep_target(Target),
        get_target_timestamp(Globals, do_search, Target, MaybeTimestamp0,
            !Info, !IO),
        ( if
            Target = target_file(_, module_target_c_header(header_mih)),
            MaybeTimestamp0 = ok(_)
        then
            % Don't rebuild the `.o' file if an irrelevant part of a
            % `.mih' file has changed. If a relevant part of a `.mih'
            % file changed, the interface files of the imported module
            % must have changed in a way that would force the `.c' and
            % `.o' files of the current module to be rebuilt.
            MaybeTimestamp = ok(oldest_timestamp)
        else
            MaybeTimestamp = MaybeTimestamp0
        )
    ).

get_target_timestamp(Globals, Search, TargetFile, MaybeTimestamp, !Info,
        !IO) :-
    TargetFile = target_file(_ModuleName, TargetType),
    get_file_name(Globals, Search, TargetFile, FileName, !Info, !IO),
    ( if TargetType = module_target_analysis_registry then
        get_target_timestamp_analysis_registry(Globals, Search, TargetFile,
            FileName, MaybeTimestamp, !Info, !IO)
    else
        get_target_timestamp_2(Globals, Search, TargetFile,
            FileName, MaybeTimestamp, !Info, !IO)
    ).

    % Special treatment for `.analysis' files. If the corresponding
    % `.analysis_status' file says the `.analysis' file is invalid then we
    % treat it as out of date.
    %
:- pred get_target_timestamp_analysis_registry(globals::in, maybe_search::in,
    target_file::in, file_name::in, maybe_error(timestamp)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

get_target_timestamp_analysis_registry(Globals, Search, TargetFile, FileName,
        MaybeTimestamp, !Info, !IO) :-
    TargetFile = target_file(ModuleName, _TargetType),
    FileTimestamps0 = !.Info ^ mki_file_timestamps,
    ( if map.search(FileTimestamps0, FileName, MaybeTimestamp0) then
        MaybeTimestamp = MaybeTimestamp0
    else
        do_read_module_overall_status(mmc, Globals, ModuleName, Status, !IO),
        (
            ( Status = optimal
            ; Status = suboptimal
            ),
            get_target_timestamp_2(Globals, Search, TargetFile, FileName,
                MaybeTimestamp, !Info, !IO)
        ;
            Status = invalid,
            MaybeTimestamp = error("invalid module"),
            map.det_insert(FileName, MaybeTimestamp,
                FileTimestamps0, FileTimestamps),
            !Info ^ mki_file_timestamps := FileTimestamps
        )
    ).

:- pred get_target_timestamp_2(globals::in, maybe_search::in, target_file::in,
    file_name::in, maybe_error(timestamp)::out, make_info::in, make_info::out,
    io::di, io::uo) is det.

get_target_timestamp_2(Globals, Search, TargetFile, FileName, MaybeTimestamp,
        !Info, !IO) :-
    TargetFile = target_file(ModuleName, TargetType),
    (
        Search = do_search,
        get_search_directories(Globals, TargetType, SearchDirs)
    ;
        Search = do_not_search,
        SearchDirs = [dir.this_directory]
    ),
    get_file_timestamp(SearchDirs, FileName, MaybeTimestamp0, !Info, !IO),
    ( if
        MaybeTimestamp0 = error(_),
        ( TargetType = module_target_opt
        ; TargetType = module_target_analysis_registry
        )
    then
        % If a `.opt' file in another directory doesn't exist,
        % it just means that a library wasn't compiled with
        % `--intermodule-optimization'.
        % Similarly for `.analysis' files.

        get_module_dependencies(Globals, ModuleName, MaybeModuleDepInfo,
            !Info, !IO),
        ( if
            MaybeModuleDepInfo = some_module_dep_info(ModuleDepInfo),
            module_dep_info_get_source_file_dir(ModuleDepInfo, ModuleDir),
            ModuleDir \= dir.this_directory
        then
            MaybeTimestamp = ok(oldest_timestamp),
            FileTimestamps0 = !.Info ^ mki_file_timestamps,
            map.set(FileName, MaybeTimestamp, FileTimestamps0, FileTimestamps),
            !Info ^ mki_file_timestamps := FileTimestamps
        else
            MaybeTimestamp = MaybeTimestamp0
        )
    else
        MaybeTimestamp = MaybeTimestamp0
    ).

%---------------------------------------------------------------------------%

get_file_name(Globals, Search, TargetFile, FileName, !Info, !IO) :-
    TargetFile = target_file(ModuleName, TargetType),
    ( if TargetType = module_target_source then
        % In some cases the module name won't match the file name
        % (module mdb.parse might be in parse.m or mdb.m), so we need to
        % look up the file name here.
        get_module_dependencies(Globals, ModuleName, MaybeModuleDepInfo,
            !Info, !IO),
        (
            MaybeModuleDepInfo = some_module_dep_info(ModuleDepInfo),
            module_dep_info_get_source_file_name(ModuleDepInfo, FileName)
        ;
            MaybeModuleDepInfo = no_module_dep_info,
            % Something has gone wrong generating the dependencies,
            % so just take a punt (which probably won't work).
            module_name_to_source_file_name(ModuleName, FileName, !IO)
        )
    else
        ( if target_type_to_extension(Globals, TargetType, Ext) then
            (
                Search = do_search,
                module_name_to_search_file_name_cache(Globals, Ext,
                    ModuleName, FileName, !Info, !IO)
            ;
                Search = do_not_search,
                % Not common enough to cache.
                module_name_to_file_name(Globals, $pred, do_not_create_dirs,
                    Ext, ModuleName, FileName, !IO)
            )
        else
            module_target_to_file_name_maybe_search(Globals, Search,
                do_not_create_dirs, TargetType, ModuleName, FileName, !IO)
        )
    ).

:- pred module_name_to_search_file_name_cache(globals::in, ext::in,
    module_name::in, string::out, make_info::in, make_info::out,
    io::di, io::uo) is det.

module_name_to_search_file_name_cache(Globals, Ext, ModuleName, FileName,
        !Info, !IO) :-
    Key = module_name_ext(ModuleName, Ext),
    Cache0 = !.Info ^ mki_search_file_name_cache,
    ( if map.search(Cache0, Key, FileName0) then
        FileName = FileName0
    else
        module_name_to_search_file_name(Globals, $pred, Ext,
            ModuleName, FileName, !IO),
        map.det_insert(Key, FileName, Cache0, Cache),
        !Info ^ mki_search_file_name_cache := Cache
    ).

get_file_timestamp(SearchDirs, FileName, MaybeTimestamp, !Info, !IO) :-
    FileTimestamps0 = !.Info ^ mki_file_timestamps,
    ( if map.search(FileTimestamps0, FileName, MaybeTimestamp0) then
        MaybeTimestamp = MaybeTimestamp0
    else
        search_for_file_mod_time(SearchDirs, FileName, SearchResult, !IO),
        (
            SearchResult = ok(TimeT),
            Timestamp = time_t_to_timestamp(TimeT),
            MaybeTimestamp = ok(Timestamp),
            map.det_insert(FileName, MaybeTimestamp,
                FileTimestamps0, FileTimestamps),
            !Info ^ mki_file_timestamps := FileTimestamps
        ;
            SearchResult = error(_),
            MaybeTimestamp = error("file `" ++ FileName ++ "' not found")
        )
    ).

:- pred get_search_directories(globals::in, module_target_type::in,
    list(dir_name)::out) is det.

get_search_directories(Globals, TargetType, SearchDirs) :-
    MaybeOpt = search_for_file_type(TargetType),
    (
        MaybeOpt = yes(SearchDirOpt),
        globals.lookup_accumulating_option(Globals, SearchDirOpt, SearchDirs0),
        % Make sure the current directory is searched for C headers
        % and libraries.
        ( if list.member(dir.this_directory, SearchDirs0) then
            SearchDirs = SearchDirs0
        else
            SearchDirs = [dir.this_directory | SearchDirs0]
        )
    ;
        MaybeOpt = no,
        SearchDirs = [dir.this_directory]
    ).

find_error_or_older_ok_timestamp(MaybeTimestampA, MaybeTimestampB,
        MaybeTimestamp) :-
    (
        MaybeTimestampA = error(_),
        MaybeTimestamp = MaybeTimestampA
    ;
        MaybeTimestampA = ok(TimestampA),
        (
            MaybeTimestampB = error(_),
            MaybeTimestamp = MaybeTimestampB
        ;
            MaybeTimestampB = ok(TimestampB),
            ( if compare((<), TimestampA, TimestampB) then
                Timestamp = TimestampA
            else
                Timestamp = TimestampB
            ),
            MaybeTimestamp = ok(Timestamp)
        )
    ).

find_error_or_oldest_ok_timestamp(MaybeTimestamps, MaybeTimestamp) :-
    list.foldl(find_error_or_older_ok_timestamp, MaybeTimestamps,
        ok(newest_timestamp), MaybeTimestamp).

%---------------------------------------------------------------------------%

make_remove_target_file(Globals, VerboseOption, Target, !Info, !IO) :-
    Target = target_file(ModuleName, TargetType),
    make_remove_target_file_by_name(Globals, VerboseOption,
        ModuleName, TargetType, !Info, !IO).

make_remove_target_file_by_name(Globals, VerboseOption, ModuleName, TargetType,
        !Info, !IO) :-
    module_target_to_file_name(Globals, do_not_create_dirs, TargetType,
        ModuleName, FileName, !IO),
    make_remove_file(Globals, VerboseOption, FileName, !Info, !IO),
    ( if timestamp_extension(TargetType, TimestampOtherExt) then
        make_remove_module_file(Globals, VerboseOption, ModuleName,
            ext_other(TimestampOtherExt), !Info, !IO)
    else
        true
    ).

make_remove_module_file(Globals, VerboseOption, ModuleName, Ext, !Info, !IO) :-
    module_name_to_file_name(Globals, $pred, do_not_create_dirs, Ext,
        ModuleName, FileName, !IO),
    make_remove_file(Globals, VerboseOption, FileName, !Info, !IO).

make_remove_file(Globals, VerboseOption, FileName, !Info, !IO) :-
    verbose_make_msg_option(Globals, VerboseOption,
        report_remove_file(FileName), !IO),
    io.remove_file_recursively(FileName, _, !IO),
    FileTimestamps0 = !.Info ^ mki_file_timestamps,
    map.delete(FileName, FileTimestamps0, FileTimestamps),
    !Info ^ mki_file_timestamps := FileTimestamps.

:- pred report_remove_file(string::in, io::di, io::uo) is det.

report_remove_file(FileName, !IO) :-
    io.format("Removing %s\n", [s(FileName)], !IO).

%---------------------------------------------------------------------------%

make_target_file_list(ModuleNames, TargetType) =
    list.map((func(ModuleName) = target_file(ModuleName, TargetType)),
        ModuleNames).

make_dependency_list(ModuleNames, TargetType) =
    list.map((func(Module) = dep_target(target_file(Module, TargetType))),
        ModuleNames).

%---------------------------------------------------------------------------%

target_type_to_extension(Globals, Target, Ext) :-
    % target_type_to_extension and extension_to_target_type represent
    % the same relationship between targets and suffixes, but in different
    % directions. Their codes should be kept in sync.
    require_complete_switch [Target]
    (
        Target = module_target_source,
        Ext = ext_src
    ;
        Target = module_target_errors,
        Ext = ext_other(other_ext(".err"))
    ;
        Target = module_target_int0,
        Ext = ext_other(other_ext(".int0"))
    ;
        Target = module_target_int1,
        Ext = ext_other(other_ext(".int"))
    ;
        Target = module_target_int2,
        Ext = ext_other(other_ext(".int2"))
    ;
        Target = module_target_int3,
        Ext = ext_other(other_ext(".int3"))
    ;
        Target = module_target_opt,
        Ext = ext_other(other_ext(".opt"))
    ;
        Target = module_target_analysis_registry,
        Ext = ext_other(other_ext(".analysis"))
    ;
        Target = module_target_track_flags,
        Ext = ext_other(other_ext(".track_flags"))
    ;
        Target = module_target_c_header(header_mih),
        Ext = ext_other(other_ext(".mih"))
    ;
        Target = module_target_c_header(header_mh),
        Ext = ext_other(other_ext(".mh"))
    ;
        Target = module_target_c_code,
        Ext = ext_other(other_ext(".c"))
    ;
        Target = module_target_csharp_code,
        % XXX ".exe" if the module contains main.
        Ext = ext_other(other_ext(".cs"))
    ;
        Target = module_target_java_code,
        Ext = ext_other(other_ext(".java"))
    ;
        Target = module_target_java_class_code,
        Ext = ext_other(other_ext(".class"))
    ;
        Target = module_target_object_code(PIC),
        pic_object_file_extension(Globals, PIC, OtherExt),
        Ext = ext_other(OtherExt)
    ;
        Target = module_target_xml_doc,
        Ext = ext_other(other_ext(".xml"))
    ;
        % These all need to be handled as special cases.
        ( Target = module_target_foreign_object(_, _)
        ; Target = module_target_fact_table_object(_, _)
        ),
        fail
    ).

extension_to_target_type(Globals, ExtStr, Target) :-
    % target_type_to_extension and extension_to_target_type represent
    % the same relationship between targets and suffixes, but in different
    % directions. Their codes should be kept in sync.
    ( if
        (
            ExtStr = ".m",
            TargetPrime = module_target_source
        ;
            ExtStr = ".err",
            TargetPrime = module_target_errors
        ;
            ExtStr = ".int0",
            TargetPrime = module_target_int0
        ;
            ExtStr = ".int",
            TargetPrime = module_target_int1
        ;
            ExtStr = ".int2",
            TargetPrime = module_target_int2
        ;
            ExtStr = ".int3",
            TargetPrime = module_target_int3
        ;
            ExtStr = ".opt",
            TargetPrime = module_target_opt
        ;
            ExtStr = ".analysis",
            TargetPrime = module_target_analysis_registry
        ;
            ExtStr = ".track_flags",
            TargetPrime = module_target_track_flags
        ;
            ExtStr = ".mih",
            TargetPrime = module_target_c_header(header_mih)
        ;
            ExtStr = ".mh",
            TargetPrime = module_target_c_header(header_mh)
        ;
            ExtStr = ".c",
            TargetPrime = module_target_c_code
        ;
            ExtStr = ".cs",
            TargetPrime = module_target_csharp_code
        ;
            ExtStr = ".java",
            TargetPrime = module_target_java_code
        ;
            ExtStr = ".class",
            TargetPrime = module_target_java_class_code
        ;
            ExtStr = ".xml",
            TargetPrime = module_target_xml_doc
        )
    then
        Target = TargetPrime
    else if
        is_pic_object_file_extension(Globals, ExtStr, PIC)
    then
        Target = module_target_object_code(PIC)
    else
        fail
    ).

target_extension_synonym(".csharp", module_target_csharp_code).
    % Currently the ".cs" extension is still treated as the build-all target
    % for C files, so we accept ".csharp" for C# files.

%---------------------------------------------------------------------------%

linked_target_file_name(Globals, ModuleName, TargetType, FileName, !IO) :-
    (
        TargetType = executable,
        globals.lookup_string_option(Globals, executable_file_extension, Ext),
        module_name_to_file_name(Globals, $pred, do_not_create_dirs,
            ext_other(other_ext(Ext)), ModuleName, FileName, !IO)
    ;
        TargetType = static_library,
        globals.lookup_string_option(Globals, library_extension, Ext),
        module_name_to_lib_file_name(Globals, $pred, do_not_create_dirs,
            "lib", other_ext(Ext), ModuleName, FileName, !IO)
    ;
        TargetType = shared_library,
        globals.lookup_string_option(Globals, shared_library_extension, Ext),
        module_name_to_lib_file_name(Globals, $pred, do_not_create_dirs,
            "lib", other_ext(Ext), ModuleName, FileName, !IO)
    ;
        TargetType = csharp_executable,
        module_name_to_file_name(Globals, $pred, do_not_create_dirs,
            ext_other(other_ext(".exe")), ModuleName, FileName, !IO)
    ;
        TargetType = csharp_library,
        module_name_to_file_name(Globals, $pred, do_not_create_dirs,
            ext_other(other_ext(".dll")), ModuleName, FileName, !IO)
    ;
        ( TargetType = java_archive
        ; TargetType = java_executable
        ),
        module_name_to_file_name(Globals, $pred, do_not_create_dirs,
            ext_other(other_ext(".jar")), ModuleName, FileName, !IO)
    ).

:- pred module_target_to_file_name(globals::in, maybe_create_dirs::in,
    module_target_type::in, module_name::in, file_name::out,
    io::di, io::uo) is det.

module_target_to_file_name(Globals, MkDir, TargetType, ModuleName, FileName,
        !IO) :-
    module_target_to_file_name_maybe_search(Globals, do_not_search, MkDir,
        TargetType, ModuleName, FileName, !IO).

:- pred module_target_to_file_name_maybe_search(globals::in,
    maybe_search::in, maybe_create_dirs::in, module_target_type::in,
    module_name::in, file_name::out, io::di, io::uo) is det.

module_target_to_file_name_maybe_search(Globals, Search, MkDir, TargetType,
        ModuleName, FileName, !IO) :-
    ( if target_type_to_extension(Globals, TargetType, Ext) then
        (
            Search = do_search,
            module_name_to_search_file_name(Globals, $pred, Ext,
                ModuleName, FileName, !IO)
        ;
            Search = do_not_search,
            module_name_to_file_name(Globals, $pred, MkDir, Ext,
                ModuleName, FileName, !IO)
        )
    else
        (
            TargetType = module_target_foreign_object(PIC, Lang),
            ( if
                ForeignModuleName =
                    foreign_language_module_name(ModuleName, Lang)
            then
                module_target_to_file_name_maybe_search(Globals,
                    Search, MkDir, module_target_object_code(PIC),
                    ForeignModuleName, FileName, !IO)
            else
                unexpected($pred, "object test failed")
            )
        ;
            TargetType = module_target_fact_table_object(PIC, FactFile),
            pic_object_file_extension(Globals, PIC, OtherExt),
            fact_table_file_name(Globals, $pred, MkDir, OtherExt,
                FactFile, FileName, !IO)
        ;
            ( TargetType = module_target_source
            ; TargetType = module_target_int0
            ; TargetType = module_target_int1
            ; TargetType = module_target_int2
            ; TargetType = module_target_int3
            ; TargetType = module_target_analysis_registry
            ; TargetType = module_target_c_code
            ; TargetType = module_target_c_header(_)
            ; TargetType = module_target_errors
            ; TargetType = module_target_opt
            ; TargetType = module_target_csharp_code
            ; TargetType = module_target_java_code
            ; TargetType = module_target_java_class_code
            ; TargetType = module_target_object_code(_)
            ; TargetType = module_target_xml_doc
            ; TargetType = module_target_track_flags
            ),
            unexpected($pred, "unexpected TargetType")
        )
    ).

timestamp_extension(ModuleTargetType, other_ext(ExtStr)) :-
    (
        ModuleTargetType = module_target_errors,
        % We need a timestamp file for `.err' files because errors are written
        % to the `.err' file even when writing interfaces. The timestamp
        % is only updated when compiling to target code.
        ExtStr = ".err_date"
    ;
        ModuleTargetType = module_target_int0,
        ExtStr = ".date0"
    ;
        ModuleTargetType = module_target_int1,
        ExtStr = ".date"
    ;
        ModuleTargetType = module_target_int2,
        ExtStr = ".date"
    ;
        ModuleTargetType = module_target_int3,
        ExtStr = ".date3"
    ;
        ModuleTargetType = module_target_opt,
        ExtStr = ".optdate"
    ;
        ModuleTargetType = module_target_analysis_registry,
        % We need a timestamp file for `.analysis' files because they
        % can be modified in the process of analysing _another_ module.
        % The timestamp is only updated after actually analysing the module
        % that the `.analysis' file corresponds to.
        ExtStr = ".analysis_date"
    ;
        % Header files share a timestamp file with their corresponding
        % target code files.
        ( ModuleTargetType = module_target_c_code
        ; ModuleTargetType = module_target_c_header(_)
        ),
        ExtStr = ".c_date"
    ;
        ModuleTargetType = module_target_csharp_code,
        ExtStr = ".cs_date"
    ;
        ModuleTargetType = module_target_java_code,
        ExtStr = ".java_date"
    ).

:- func search_for_file_type(module_target_type) = maybe(option).

search_for_file_type(ModuleTargetType) = MaybeSearchOption :-
    (
        ( ModuleTargetType = module_target_source
        ; ModuleTargetType = module_target_errors
        ; ModuleTargetType = module_target_track_flags
        ; ModuleTargetType = module_target_c_code
        ; ModuleTargetType = module_target_csharp_code
        ; ModuleTargetType = module_target_java_code
        ; ModuleTargetType = module_target_java_class_code
        ; ModuleTargetType = module_target_object_code(_)
        ; ModuleTargetType = module_target_foreign_object(_, _)
        ; ModuleTargetType = module_target_fact_table_object(_, _)
        ; ModuleTargetType = module_target_xml_doc
        ),
        MaybeSearchOption = no
    ;
        ( ModuleTargetType = module_target_int0
        ; ModuleTargetType = module_target_int1
        ; ModuleTargetType = module_target_int2
        ; ModuleTargetType = module_target_int3
        ),
        MaybeSearchOption = yes(search_directories)
    ;
        ( ModuleTargetType = module_target_opt
        ; ModuleTargetType = module_target_analysis_registry
        ),
        MaybeSearchOption = yes(intermod_directories)
    ;
        ModuleTargetType = module_target_c_header(_),
        MaybeSearchOption = yes(c_include_directory)
    ).

target_is_grade_or_arch_dependent(Target) :-
    is_target_grade_or_arch_dependent(Target) = yes.

:- func is_target_grade_or_arch_dependent(module_target_type) = bool.

is_target_grade_or_arch_dependent(Target) = IsDependent :-
    (
        ( Target = module_target_source
        ; Target = module_target_errors
        ; Target = module_target_int0
        ; Target = module_target_int1
        ; Target = module_target_int2
        ; Target = module_target_int3
        ; Target = module_target_c_header(header_mh)
        ; Target = module_target_xml_doc
        ),
        IsDependent = no
    ;
        ( Target = module_target_opt
        ; Target = module_target_analysis_registry
        ; Target = module_target_track_flags
        ; Target = module_target_c_header(header_mih)
        ; Target = module_target_c_code
        ; Target = module_target_csharp_code
        ; Target = module_target_java_code
        ; Target = module_target_java_class_code
        ; Target = module_target_object_code(_)
        ; Target = module_target_foreign_object(_, _)
        ; Target = module_target_fact_table_object(_, _)
        ),
        IsDependent = yes
    ).

%---------------------------------------------------------------------------%

debug_make_msg(Globals, P, !IO) :-
    verbose_make_msg_option(Globals, debug_make, P, !IO).

verbose_make_msg(Globals, P, !IO) :-
    verbose_make_msg_option(Globals, verbose_make, P, !IO).

verbose_make_msg_option(Globals, Option, P, !IO) :-
    globals.lookup_bool_option(Globals, Option, OptionValue),
    (
        OptionValue = yes,
        P(!IO),
        io.flush_output(!IO)
    ;
        OptionValue = no
    ).

debug_file_msg(Globals, TargetFile, Msg, !IO) :-
    debug_make_msg(Globals,
        ( pred(!.IO::di, !:IO::uo) is det :-
            get_make_target_file_name(Globals, TargetFile, FileName, !IO),
            io.format("%s: %s\n", [s(FileName), s(Msg)], !IO)
        ), !IO).

dependency_file_to_file_name(Globals, DepFile, FileName, !IO) :-
    (
        DepFile = dep_target(TargetFile),
        get_make_target_file_name(Globals, TargetFile, FileName, !IO)
    ;
        DepFile = dep_file(FileName)
    ).

make_write_dependency_file(Globals, DepFile, !IO) :-
    dependency_file_to_file_name(Globals, DepFile, FileName, !IO),
    io.write_string(FileName, !IO).

make_write_dependency_file_list(_, [], !IO).
make_write_dependency_file_list(Globals, [DepFile | DepFiles], !IO) :-
    dependency_file_to_file_name(Globals, DepFile, FileName, !IO),
    io.format("\t%s\n", [s(FileName)], !IO),
    make_write_dependency_file_list(Globals, DepFiles, !IO).

get_make_target_file_name(Globals, TargetFile, FileName, !IO) :-
    TargetFile = target_file(ModuleName, TargetType),
    module_target_to_file_name(Globals, do_not_create_dirs, TargetType,
        ModuleName, FileName, !IO).

make_write_target_file(Globals, TargetFile, !IO) :-
    get_make_target_file_name(Globals, TargetFile, FileName, !IO),
    io.write_string(FileName, !IO).

make_write_target_file_wrapped(Globals, Prefix, TargetFile, Suffix, !IO) :-
    get_make_target_file_name(Globals, TargetFile, FileName, !IO),
    ( if
        Prefix = "",
        Suffix = ""
    then
        io.write_string(FileName, !IO)
    else
        % Try to write this with one call to avoid interleaved output when
        % doing parallel builds.
        io.write_string(Prefix ++ FileName ++ Suffix, !IO)
    ).

make_write_target_file_wrapped(Stream, Globals, Prefix, TargetFile, Suffix,
        !IO) :-
    get_make_target_file_name(Globals, TargetFile, FileName, !IO),
    ( if
        Prefix = "",
        Suffix = ""
    then
        io.write_string(Stream, FileName, !IO)
    else
        % Try to write this with one call to avoid interleaved output when
        % doing parallel builds.
        io.write_string(Stream, Prefix ++ FileName ++ Suffix, !IO)
    ).

maybe_make_linked_target_message(Globals, TargetFile, !IO) :-
    verbose_make_msg(Globals,
        ( pred(!.IO::di, !:IO::uo) is det :-
            % Write this with one call to avoid interleaved output
            % when doing parallel builds.
            io.write_string("Making " ++ TargetFile ++ "\n", !IO)
        ), !IO).

maybe_make_target_message(Globals, TargetFile, !IO) :-
    io.output_stream(OutputStream, !IO),
    maybe_make_target_message_to_stream(Globals, OutputStream, TargetFile,
        !IO).

maybe_make_target_message_to_stream(Globals, OutputStream, TargetFile, !IO) :-
    verbose_make_msg(Globals,
        ( pred(!.IO::di, !:IO::uo) is det :-
            make_write_target_file_wrapped(OutputStream, Globals,
                "Making ", TargetFile, "\n", !IO)
        ), !IO).

maybe_reanalyse_modules_message(Globals, !IO) :-
    verbose_make_msg(Globals,
        ( pred(!.IO::di, !:IO::uo) is det :-
            io.output_stream(OutputStream, !IO),
            io.write_string(OutputStream,
                "Reanalysing invalid/suboptimal modules\n", !IO)
        ), !IO).

target_file_error(Info, Globals, TargetFile, !IO) :-
    with_locked_stdout(Info,
        make_write_target_file_wrapped(Globals,
            "** Error making `", TargetFile, "'.\n"), !IO).

file_error(Info, TargetFile, !IO) :-
    with_locked_stdout(Info,
        io.write_string("** Error making `" ++ TargetFile ++ "'.\n"), !IO).

maybe_warn_up_to_date_target(Globals, Target, !Info, !IO) :-
    globals.lookup_bool_option(Globals, warn_up_to_date, Warn),
    CmdLineTargets0 = !.Info ^ mki_command_line_targets,
    (
        Warn = yes,
        ( if set.member(Target, CmdLineTargets0) then
            module_or_linked_target_file_name(Globals, Target, FileName, !IO),
            io.format("** Nothing to be done for `%s'.\n", [s(FileName)], !IO)
        else
            true
        )
    ;
        Warn = no
    ),
    set.delete(Target, CmdLineTargets0, CmdLineTargets),
    !Info ^ mki_command_line_targets := CmdLineTargets.

maybe_symlink_or_copy_linked_target_message(Globals, Target, !IO) :-
    verbose_make_msg(Globals,
        ( pred(!.IO::di, !:IO::uo) is det :-
            module_or_linked_target_file_name(Globals, Target, FileName, !IO),
            io.format("Made symlink/copy of %s\n", [s(FileName)], !IO)
        ), !IO).

:- pred module_or_linked_target_file_name(globals::in,
    pair(module_name, target_type)::in, string::out, io::di, io::uo) is det.

module_or_linked_target_file_name(Globals, ModuleName - TargetType,
        FileName, !IO) :-
    (
        TargetType = module_target(ModuleTargetType),
        TargetFile = target_file(ModuleName, ModuleTargetType),
        get_make_target_file_name(Globals, TargetFile, FileName, !IO)
    ;
        TargetType = linked_target(LinkedTargetType),
        linked_target_file_name(Globals, ModuleName, LinkedTargetType,
            FileName, !IO)
    ;
        TargetType = misc_target(_),
        unexpected($pred, "misc_target")
    ).

%---------------------------------------------------------------------------%
%
% Timing.
%

:- pragma foreign_proc("C",
    get_real_milliseconds(Time::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Time = MR_get_real_milliseconds();
").

:- pragma foreign_proc("C#",
    get_real_milliseconds(Time::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Time = System.Environment.TickCount;
").

:- pragma foreign_proc("Java",
    get_real_milliseconds(Time::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    // The loss of precision is acceptable for mmc --make.
    Time = (int) System.currentTimeMillis();
").

get_real_milliseconds(_, _, _) :-
    sorry($file, $pred).

%---------------------------------------------------------------------------%
%
% Hash functions.
%

module_name_hash(SymName, Hash) :-
    (
        SymName = unqualified(String),
        Hash = string.hash(String)
    ;
        SymName = qualified(_Qual, String),
        % Hashing the module qualifier seems to be not worthwhile.
        Hash = string.hash(String)
    ).

dependency_file_hash(DepFile, Hash) :-
    (
        DepFile = dep_target(TargetFile),
        Hash = target_file_hash(TargetFile)
    ;
        DepFile = dep_file(FileName),
        Hash = string.hash(FileName)
    ).

:- func target_file_hash(target_file) = int.

target_file_hash(TargetFile) = Hash :-
    TargetFile = target_file(ModuleName, Type),
    module_name_hash(ModuleName, Hash0),
    Hash1 = module_target_type_to_nonce(Type),
    Hash = mix(Hash0, Hash1).

:- func module_target_type_to_nonce(module_target_type) = int.

module_target_type_to_nonce(Type) = X :-
    (
        Type = module_target_source,
        X = 1
    ;
        Type = module_target_errors,
        X = 2
    ;
        Type = module_target_int0,
        X = 3
    ;
        Type = module_target_int1,
        X = 4
    ;
        Type = module_target_int2,
        X = 5
    ;
        Type = module_target_int3,
        X = 6
    ;
        Type = module_target_opt,
        X = 7
    ;
        Type = module_target_analysis_registry,
        X = 8
    ;
        Type = module_target_c_header(header_mh),
        X = 9
    ;
        Type = module_target_c_header(header_mih),
        X = 10
    ;
        Type = module_target_c_code,
        X = 11
    ;
        Type = module_target_java_code,
        X = 12
%   ;
%       Type = module_target_erlang_header,
%       X = 13
%   ;
%       Type = module_target_erlang_code,
%       X = 14
%   ;
%       Type = module_target_erlang_beam_code,
%       X = 15
    ;
        Type = module_target_object_code(PIC),
        X = 16 `mix` pic_to_nonce(PIC)
    ;
        Type = module_target_foreign_object(_PIC, _ForeignLang),
        X = 17
    ;
        Type = module_target_fact_table_object(_PIC, _FileName),
        X = 18
    ;
        Type = module_target_xml_doc,
        X = 19
    ;
        Type = module_target_track_flags,
        X = 20
    ;
        Type = module_target_java_class_code,
        X = 21
    ;
        Type = module_target_csharp_code,
        X = 22
    ).

:- func pic_to_nonce(pic) = int.

pic_to_nonce(pic) = 1.
pic_to_nonce(non_pic) = 3.
% For compatibility; we used to have pic_to_nonce(link_with_pic) = 2.

:- func mix(int, int) = int.

mix(H0, X) = H :-
    H1 = H0 `xor` (H0 `unchecked_left_shift` 5),
    H = H1 `xor` X.

%---------------------------------------------------------------------------%
:- end_module make.util.
%---------------------------------------------------------------------------%
