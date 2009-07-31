%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2009 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: read_modules.m.
%
%-----------------------------------------------------------------------------%

:- module parse_tree.read_modules.
:- interface.

:- import_module libs.file_util.
:- import_module libs.timestamp.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.file_names.
:- import_module parse_tree.module_imports.
:- import_module parse_tree.prog_item.
:- import_module parse_tree.prog_io.

:- import_module io.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.

%-----------------------------------------------------------------------------%

    % recompilation_check.m records each file read to avoid reading it again.
    % The string is the suffix of the file name.
    %
:- type read_modules == map(pair(module_name, string), read_module).

:- type read_module
    --->    read_module(
                module_timestamp,
                list(item),
                module_error,
                file_name
            ).

    % read_module(ModuleName, Extension, Descr, Search, ReturnTimestamp,
    %       Items, Error, SourceFileName, MaybeTimestamp):
    %
    % Given a module name and a file extension (e.g. `.m', `.int', or `int2'),
    % read in the list of items in that file.
    %
    % If Extension is ".m", and ModuleName is a nested module, then try
    % searching for different filenames: for modules such as `foo.bar.baz.m'
    % search first for `foo.bar.baz.m', then `bar.baz.m', then `baz.m'.
    % If Search is do_search, search all directories given by the option
    % search_directories for the module.
    % If ReturnTimestamp is do_return_timestamp, attempt to return the
    % modification time of the file in MaybeTimestamp.
    % If the actual module name (as determined by the `:- module' declaration)
    % does not match the specified module name, then report an error message.
    % Return the actual source file name found (excluding the directory part).
    %
    % N.B. This reads a module given the module name. If you want to read
    % a module given the file name, use `read_module_from_file'.
    %
:- pred read_module(module_name::in, string::in, string::in, maybe_search::in,
    maybe_return_timestamp::in, list(item)::out, module_error::out,
    file_name::out, maybe(timestamp)::out, io::di, io::uo) is det.

    % read_module_if_changed(ModuleName, Extension, Descr, Search,
    %   OldTimestamp, Items, Error, SourceFileName, MaybeTimestamp):
    %
    % If the timestamp of the file specified by the given module name and
    % file extension is newer than OldTimestamp, read the file, returning
    % the new timestamp.
    %
    % If the file was read, MaybeTimestamp will contain the new timestamp.
    % If the timestamp was unchanged, MaybeTimestamp will be
    % `yes(OldTimestamp)'. If the file could not be read, MaybeTimestamp
    % will be `no'.
    %
:- pred read_module_if_changed(module_name::in, string::in, string::in,
    maybe_search::in, timestamp::in, list(item)::out, module_error::out,
    file_name::out, maybe(timestamp)::out, io::di, io::uo) is det.

    % Similar to read_mod, but doesn't return error messages.
    %
:- pred read_module_ignore_errors(module_name::in, string::in, string::in,
    maybe_search::in, maybe_return_timestamp::in, list(item)::out,
    module_error::out, file_name::out, maybe(timestamp)::out, io::di, io::uo)
    is det.

    % read_module_from_file(SourceFileName, Extension, Descr, Search,
    %   ReturnTimestamp, Items, Error, ModuleName, MaybeTimestamp):
    %
    % Given a file name and a file extension (e.g. `.m', `.int', or `int2'),
    % read in the list of items in that file.
    % If Search is do_search, search all directories given by the option
    % search_directories for the module.
    % If ReturnTimestamp is do_return_timestamp, attempt to return the
    % modification time of the file in MaybeTimestamp. Return the module name
    % (as determined by the `:- module' declaration, if any).
    %
    % N.B.  This reads a module given the file name. If you want to read a
    % module given the module name, use `read_mod'.
    %
:- pred read_module_from_file(file_name::in, string::in, string::in,
    maybe_search::in, maybe_return_timestamp::in, list(item)::out,
    module_error::out, module_name::out, maybe(timestamp)::out, io::di, io::uo)
    is det.

%-----------------------------------------------------------------------------%

:- pred maybe_read_module(read_modules::in, module_name::in, string::in,
    string::in, maybe_search::in, maybe_return_timestamp::in, list(item)::out,
    module_error::out, file_name::out, maybe(timestamp)::out, io::di, io::uo)
    is det.

    % find_read_module(ReadModules, ModuleName, Suffix, ReturnTimestamp,
    %   Items, MaybeTimestamp, Error, FileName)
    %
    % Check whether a file was read during recompilation checking.
    %
:- pred find_read_module(read_modules::in, module_name::in, string::in,
    maybe_return_timestamp::in, list(item)::out, maybe(timestamp)::out,
    module_error::out, file_name::out) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module libs.globals.
:- import_module libs.options.
:- import_module parse_tree.error_util.

:- import_module bool.
:- import_module dir.
:- import_module getopt_io.
:- import_module string.

%-----------------------------------------------------------------------------%

:- type maybe_ignore_errors
    --->    ignore_errors
    ;       do_not_ignore_errors.

read_module(ModuleName, Extension, Descr, Search, ReturnTimestamp,
        Items, Error, FileName, MaybeTimestamp, !IO) :-
    do_read_module(do_not_ignore_errors, ModuleName, Extension, Descr, Search,
        no, ReturnTimestamp, Items, Error, FileName, MaybeTimestamp, !IO).

read_module_if_changed(ModuleName, Extension, Descr, Search, OldTimestamp,
        Items, Error, FileName, MaybeTimestamp, !IO) :-
    do_read_module(do_not_ignore_errors, ModuleName, Extension, Descr, Search,
        yes(OldTimestamp), do_return_timestamp, Items, Error,
        FileName, MaybeTimestamp, !IO).

read_module_ignore_errors(ModuleName, Extension, Descr, Search,
        ReturnTimestamp, Items, Error, FileName, MaybeTimestamp, !IO) :-
    do_read_module(ignore_errors, ModuleName, Extension, Descr, Search,
        no, ReturnTimestamp, Items, Error, FileName, MaybeTimestamp, !IO).

:- pred do_read_module(maybe_ignore_errors::in, module_name::in,
    string::in, string::in, maybe_search::in, maybe(timestamp)::in,
    maybe_return_timestamp::in, list(item)::out, module_error::out,
    file_name::out, maybe(timestamp)::out, io::di, io::uo) is det.

do_read_module(IgnoreErrors, ModuleName, Extension, Descr, Search,
        MaybeOldTimestamp, ReturnTimestamp, Items, Error, FileName,
        MaybeTimestamp, !IO) :-
    (
        Search = do_search,
        module_name_to_search_file_name(ModuleName, Extension, FileName0, !IO)
    ;
        Search = do_not_search,
        module_name_to_file_name(ModuleName, Extension, do_not_create_dirs,
            FileName0, !IO)
    ),
    globals.io_lookup_bool_option(very_verbose, VeryVerbose, !IO),
    Msg = "% " ++ Descr ++ " `" ++ FileName0 ++ "'... ",
    maybe_write_string(VeryVerbose, Msg, !IO),
    maybe_flush_output(VeryVerbose, !IO),

    globals.io_lookup_accumulating_option(search_directories,
        InterfaceSearchDirs, !IO),
    (
        Search = do_search,
        SearchDirs = InterfaceSearchDirs
    ;
        Search = do_not_search,
        SearchDirs = [dir.this_directory]
    ),
    ( Extension = ".m" ->
        % For `.m' files we need to deal with the case where the module name
        % does not match the file name, or where a partial match occurs
        % in the current directory but the full match occurs in a
        % search directory.

        OpenFile = search_for_module_source(SearchDirs,
            InterfaceSearchDirs, ModuleName)
    ;
        OpenFile = search_for_file(open_file, SearchDirs, FileName0)
    ),
    (
        MaybeOldTimestamp = yes(OldTimestamp),
        prog_io.read_module_if_changed(OpenFile, ModuleName, OldTimestamp,
            MaybeFileName, ActualModuleName, Items, Specs, Error,
            MaybeTimestamp0, !IO)
    ;
        MaybeOldTimestamp = no,
        prog_io.read_module(OpenFile, ModuleName, ReturnTimestamp,
            MaybeFileName, ActualModuleName, Items, Specs, Error,
            MaybeTimestamp0, !IO)
    ),

    (
        MaybeFileName = yes(FileName)
    ;
        MaybeFileName = no,
        FileName = FileName0
    ),
    check_module_has_expected_name(FileName, ModuleName, ActualModuleName,
        !IO),

    check_timestamp(FileName0, MaybeTimestamp0, MaybeTimestamp, !IO),
    (
        IgnoreErrors = ignore_errors,
        (
            Error = fatal_module_errors,
            Items = []
        ->
            maybe_write_string(VeryVerbose, "not found.\n", !IO)
        ;
            maybe_write_string(VeryVerbose, "done.\n", !IO)
        )
    ;
        IgnoreErrors = do_not_ignore_errors,
        (
            Error = fatal_module_errors,
            maybe_write_string(VeryVerbose, "fatal error(s).\n", !IO),
            io.set_exit_status(1, !IO)
        ;
            Error = some_module_errors,
            maybe_write_string(VeryVerbose, "parse error(s).\n", !IO),
            io.set_exit_status(1, !IO)
        ;
            Error = no_module_errors,
            maybe_write_string(VeryVerbose, "successful parse.\n", !IO)
        ),
        globals.io_get_globals(Globals, !IO),
        % XXX _NumWarnings _NumErrors
        write_error_specs(Specs, Globals, 0, _NumWarnings, 0, _NumErrors, !IO)
    ).

read_module_from_file(FileName, Extension, Descr, Search, ReturnTimestamp,
        Items, Error, ModuleName, MaybeTimestamp, !IO) :-
    globals.io_lookup_bool_option(very_verbose, VeryVerbose, !IO),
    maybe_write_string(VeryVerbose, "% ", !IO),
    maybe_write_string(VeryVerbose, Descr, !IO),
    maybe_write_string(VeryVerbose, " `", !IO),
    maybe_write_string(VeryVerbose, FileName, !IO),
    maybe_write_string(VeryVerbose, "'... ", !IO),
    maybe_flush_output(VeryVerbose, !IO),
    string.append(FileName, Extension, FullFileName),
    ( dir.basename(FileName, BaseFileNamePrime) ->
        BaseFileName = BaseFileNamePrime
    ;
        BaseFileName = ""
    ),
    file_name_to_module_name(BaseFileName, DefaultModuleName),
    (
        Search = do_search,
        globals.io_lookup_accumulating_option(search_directories,
            SearchDirs, !IO)
    ;
        Search = do_not_search,
        SearchDirs = [dir.this_directory]
    ),
    OpenFile = search_for_file(open_file, SearchDirs, FullFileName),
    prog_io.read_module(OpenFile, DefaultModuleName, ReturnTimestamp,
        _, ModuleName, Items, Specs, Error, MaybeTimestamp0, !IO),
    check_timestamp(FullFileName, MaybeTimestamp0, MaybeTimestamp, !IO),
    (
        Error = fatal_module_errors,
        maybe_write_string(VeryVerbose, "fatal error(s).\n", !IO),
        io.set_exit_status(1, !IO)
    ;
        Error = some_module_errors,
        maybe_write_string(VeryVerbose, "parse error(s).\n", !IO),
        io.set_exit_status(1, !IO)
    ;
        Error = no_module_errors,
        maybe_write_string(VeryVerbose, "successful parse.\n", !IO)
    ),
    globals.io_get_globals(Globals, !IO),
    % XXX _NumWarnings _NumErrors
    write_error_specs(Specs, Globals, 0, _NumWarnings, 0, _NumErrors, !IO).

:- pred check_timestamp(file_name::in, maybe(io.res(timestamp))::in,
    maybe(timestamp)::out, io::di, io::uo) is det.

check_timestamp(FileName, MaybeTimestamp0, MaybeTimestamp, !IO) :-
    (
        MaybeTimestamp0 = yes(ok(Timestamp)),
        MaybeTimestamp = yes(Timestamp)
    ;
        MaybeTimestamp0 = yes(error(IOError)),
        MaybeTimestamp = no,
        globals.io_lookup_bool_option(smart_recompilation, SmartRecompilation,
            !IO),
        (
            SmartRecompilation = yes,
            report_modification_time_warning(FileName, IOError, !IO)
        ;
            SmartRecompilation = no
        )
    ;
        MaybeTimestamp0 = no,
        MaybeTimestamp = no
    ).

%-----------------------------------------------------------------------------%

maybe_read_module(ReadModules, ModuleName, Extension, Descr, Search,
        ReturnTimestamp, Items, Error, FileName, MaybeTimestamp, !IO) :-
    (
        find_read_module(ReadModules, ModuleName, Extension, ReturnTimestamp,
            ItemsPrime, MaybeTimestampPrime, ErrorPrime, FileNamePrime)
    ->
        Error = ErrorPrime,
        Items = ItemsPrime,
        MaybeTimestamp = MaybeTimestampPrime,
        FileName = FileNamePrime
    ;
        read_module(ModuleName, Extension, Descr, Search, ReturnTimestamp,
            Items, Error, FileName, MaybeTimestamp, !IO)
    ).

find_read_module(ReadModules, ModuleName, Suffix, ReturnTimestamp, Items,
        MaybeTimestamp, Error, FileName) :-
    map.search(ReadModules, ModuleName - Suffix, ReadModule),
    ReadModule = read_module(ModuleTimestamp, Items, Error, FileName),
    (
        ReturnTimestamp = do_return_timestamp,
        ModuleTimestamp = module_timestamp(_, Timestamp, _),
        MaybeTimestamp = yes(Timestamp)
    ;
        ReturnTimestamp = do_not_return_timestamp,
        MaybeTimestamp = no
    ).

%-----------------------------------------------------------------------------%

:- pred report_modification_time_warning(file_name::in, io.error::in,
    io::di, io::uo) is det.

report_modification_time_warning(SourceFileName, Error, !IO) :-
    globals.io_set_option(smart_recompilation, bool(no), !IO),
    globals.io_set_option(generate_item_version_numbers, bool(no), !IO),
    globals.io_lookup_bool_option(warn_smart_recompilation, Warn, !IO),
    (
        Warn = yes,
        io.write_string("Warning: cannot find modification time for ", !IO),
        io.write_string(SourceFileName, !IO),
        io.write_string(":\n", !IO),
        io.error_message(Error, Msg),
        io.write_string("  ", !IO),
        io.write_string(Msg, !IO),
        io.write_string(".\n", !IO),
        io.write_string("  Smart recompilation will not work.\n", !IO),
        globals.io_lookup_bool_option(halt_at_warn, HaltAtWarn, !IO),
        (
            HaltAtWarn = yes,
            io.set_exit_status(1, !IO)
        ;
            HaltAtWarn = no
        )
    ;
        Warn = no
    ).

%-----------------------------------------------------------------------------%
