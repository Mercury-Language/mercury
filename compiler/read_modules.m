%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2009, 2011 The University of Melbourne.
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
:- import_module libs.globals.
:- import_module libs.timestamp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.error_util.
:- import_module parse_tree.file_names.
:- import_module parse_tree.module_imports.
:- import_module parse_tree.prog_item.
:- import_module parse_tree.prog_io_error.

:- import_module io.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.

%-----------------------------------------------------------------------------%

    % recompilation_check.m records each file read to avoid reading it again.
    % The string is the suffix of the file name.
    %
:- type have_read_module_map ==
    map(pair(module_name, string), have_read_module).

:- type have_read_module
    --->    have_read_module(
                module_timestamp,
                list(item),
                list(error_spec),
                read_module_errors,
                file_name
            ).

    % read_module(Globals, ModuleName, Extension, Descr, Search,
    %   ReturnTimestamp, Items, Specs, Errors, SourceFileName, MaybeTimestamp):
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
:- pred read_module(globals::in, module_name::in, string::in, string::in,
    maybe_search::in, maybe_return_timestamp::in, list(item)::out,
    list(error_spec)::out, read_module_errors::out, file_name::out,
    maybe(timestamp)::out, io::di, io::uo) is det.

    % read_module_if_changed(Globals, ModuleName, Extension, Descr, Search,
    %   OldTimestamp, Items, Specs, Errors, SourceFileName, MaybeTimestamp):
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
:- pred read_module_if_changed(globals::in, module_name::in,
    string::in, string::in, maybe_search::in, timestamp::in, list(item)::out,
    list(error_spec)::out, read_module_errors::out, file_name::out,
    maybe(timestamp)::out, io::di, io::uo) is det.

    % Similar to read_module, but doesn't return error messages.
    %
:- pred read_module_ignore_errors(globals::in, module_name::in,
    string::in, string::in, maybe_search::in, maybe_return_timestamp::in,
    list(item)::out, read_module_errors::out, file_name::out,
    maybe(timestamp)::out, io::di, io::uo) is det.

    % read_module_from_file(SourceFileName, Extension, Descr, Search,
    %   ReturnTimestamp, Items, Specs, Errors, ModuleName, MaybeTimestamp):
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
:- pred read_module_from_file(globals::in, file_name::in, string::in,
    string::in, maybe_search::in, maybe_return_timestamp::in, list(item)::out,
    list(error_spec)::out, read_module_errors::out, module_name::out,
    maybe(timestamp)::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- pred maybe_read_module(globals::in, have_read_module_map::in,
    module_name::in, string::in, string::in, maybe_search::in,
    maybe_return_timestamp::in, list(item)::out, list(error_spec)::out,
    read_module_errors::out, file_name::out, maybe(timestamp)::out,
    io::di, io::uo) is det.

    % find_read_module(HaveReadModuleMap, ModuleName, Suffix,
    %   ReturnTimestamp, Items, Specs, Errors, FileName, MaybeTimestamp)
    %
    % Check whether a file was read during recompilation checking.
    %
:- pred find_read_module(have_read_module_map::in, module_name::in,
    string::in, maybe_return_timestamp::in, list(item)::out,
    list(error_spec)::out, read_module_errors::out,
    file_name::out, maybe(timestamp)::out) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module libs.globals.
:- import_module libs.options.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_io.
:- import_module parse_tree.prog_io_find.

:- import_module bool.
:- import_module dir.
:- import_module set.
:- import_module string.

%-----------------------------------------------------------------------------%

:- type maybe_ignore_errors
    --->    ignore_errors
    ;       do_not_ignore_errors.

read_module(Globals, ModuleName, Extension, Descr, Search, ReturnTimestamp,
        Items, Specs, Errors, FileName, MaybeTimestamp, !IO) :-
    do_read_module(Globals, do_not_ignore_errors, ModuleName, Extension,
        Descr, Search, no, ReturnTimestamp, Items, Specs, Errors,
        FileName, MaybeTimestamp, !IO).

read_module_if_changed(Globals, ModuleName, Extension, Descr, Search,
        OldTimestamp, Items, Specs, Errors, FileName, MaybeTimestamp, !IO) :-
    do_read_module(Globals, do_not_ignore_errors, ModuleName, Extension,
        Descr, Search, yes(OldTimestamp), do_return_timestamp, Items, Specs,
        Errors, FileName, MaybeTimestamp, !IO).

read_module_ignore_errors(Globals, ModuleName, Extension, Descr, Search,
        ReturnTimestamp, Items, Errors, FileName, MaybeTimestamp, !IO) :-
    do_read_module(Globals, ignore_errors, ModuleName, Extension,
        Descr, Search, no, ReturnTimestamp, Items, _Specs, Errors,
        FileName, MaybeTimestamp, !IO).

:- pred do_read_module(globals::in, maybe_ignore_errors::in, module_name::in,
    string::in, string::in, maybe_search::in, maybe(timestamp)::in,
    maybe_return_timestamp::in, list(item)::out, list(error_spec)::out,
    read_module_errors::out, file_name::out, maybe(timestamp)::out,
    io::di, io::uo) is det.

do_read_module(Globals, IgnoreErrors, ModuleName, Extension, Descr, Search,
        MaybeOldTimestamp, ReturnTimestamp, Items, Specs, Errors, FileName,
        MaybeTimestamp, !IO) :-
    (
        Search = do_search,
        module_name_to_search_file_name(Globals, ModuleName, Extension,
            FileName0, !IO)
    ;
        Search = do_not_search,
        module_name_to_file_name(Globals, ModuleName, Extension,
            do_not_create_dirs, FileName0, !IO)
    ),
    globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
    Msg = "% " ++ Descr ++ " `" ++ FileName0 ++ "'... ",
    maybe_write_string(VeryVerbose, Msg, !IO),
    maybe_flush_output(VeryVerbose, !IO),

    globals.lookup_accumulating_option(Globals, search_directories,
        InterfaceSearchDirs),
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

        OpenFile = search_for_module_source(Globals, SearchDirs,
            InterfaceSearchDirs, ModuleName)
    ;
        OpenFile = search_for_file(open_file, SearchDirs, FileName0)
    ),
    (
        MaybeOldTimestamp = yes(OldTimestamp),
        actually_read_module_if_changed(Globals, OpenFile, ModuleName,
            OldTimestamp, MaybeFileName, ActualModuleName,
            Items, ModuleSpecs, Errors, MaybeTimestamp0, !IO)
    ;
        MaybeOldTimestamp = no,
        actually_read_module(Globals, OpenFile, ModuleName, ReturnTimestamp,
            MaybeFileName, ActualModuleName, Items, ModuleSpecs, Errors,
            MaybeTimestamp0, !IO)
    ),

    (
        MaybeFileName = yes(FileName)
    ;
        MaybeFileName = no,
        FileName = FileName0
    ),
    check_module_has_expected_name(FileName, ModuleName, ActualModuleName,
        NameSpecs),

    check_timestamp(Globals, FileName0, MaybeTimestamp0, MaybeTimestamp, !IO),
    (
        IgnoreErrors = ignore_errors,
        Specs = NameSpecs,      % Do not include ModuleSpecs.
        (
            set.contains(Errors, rme_could_not_open_file),
            Items = []
        ->
            maybe_write_string(VeryVerbose, "not found.\n", !IO)
        ;
            maybe_write_string(VeryVerbose, "done.\n", !IO)
        )
    ;
        IgnoreErrors = do_not_ignore_errors,
        ModuleNameSpecs = NameSpecs ++ ModuleSpecs,
        handle_any_read_module_errors(Globals, VeryVerbose, Errors,
            ModuleNameSpecs, Specs, !IO)
    ).

read_module_from_file(Globals, FileName, Extension, Descr, Search,
        ReturnTimestamp, Items, Specs, Errors, ModuleName, MaybeTimestamp,
        !IO) :-
    globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
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
        globals.lookup_accumulating_option(Globals, search_directories,
            SearchDirs)
    ;
        Search = do_not_search,
        SearchDirs = [dir.this_directory]
    ),
    OpenFile = search_for_file(open_file, SearchDirs, FullFileName),
    actually_read_module(Globals, OpenFile, DefaultModuleName, ReturnTimestamp,
        _, ModuleName, Items, Specs0, Errors, MaybeTimestamp0, !IO),
    check_timestamp(Globals, FullFileName, MaybeTimestamp0, MaybeTimestamp,
        !IO),
    handle_any_read_module_errors(Globals, VeryVerbose, Errors,
        Specs0, Specs, !IO).

:- pred handle_any_read_module_errors(globals::in, bool::in,
    read_module_errors::in, list(error_spec)::in, list(error_spec)::out,
    io::di, io::uo) is det.

handle_any_read_module_errors(Globals, VeryVerbose, Errors, !Specs, !IO) :-
    ( if set.is_empty(Errors) then
        maybe_write_string(VeryVerbose, "successful parse.\n", !IO)
    else
        set.intersect(Errors, fatal_read_module_errors, FatalErrors),
        ( if set.is_empty(FatalErrors) then
            maybe_write_string(VeryVerbose, "parse error(s).\n", !IO)
        else
            maybe_write_string(VeryVerbose, "fatal error(s).\n", !IO)
        ),
        maybe_write_out_errors_no_module(VeryVerbose, Globals, !Specs, !IO),
        io.set_exit_status(1, !IO)
    ).

%-----------------------------------------------------------------------------%

maybe_read_module(Globals, HaveReadModuleMap, ModuleName, Extension, Descr,
        Search, ReturnTimestamp, Items, Specs, Errors, FileName,
        MaybeTimestamp, !IO) :-
    (
        find_read_module(HaveReadModuleMap, ModuleName, Extension,
            ReturnTimestamp, ItemsPrime, SpecsPrime, ErrorsPrime,
            FileNamePrime, MaybeTimestampPrime)
    ->
        Errors = ErrorsPrime,
        Items = ItemsPrime,
        Specs = SpecsPrime,
        MaybeTimestamp = MaybeTimestampPrime,
        FileName = FileNamePrime
    ;
        read_module(Globals, ModuleName, Extension, Descr, Search,
            ReturnTimestamp, Items, Specs, Errors, FileName, MaybeTimestamp,
            !IO)
    ).

find_read_module(HaveReadModuleMap, ModuleName, Suffix, ReturnTimestamp,
        Items, Specs, Errors, FileName, MaybeTimestamp) :-
    map.search(HaveReadModuleMap, ModuleName - Suffix, HaveReadModule),
    HaveReadModule = have_read_module(ModuleTimestamp, Items, Specs, Errors,
        FileName),
    (
        ReturnTimestamp = do_return_timestamp,
        ModuleTimestamp = module_timestamp(_, Timestamp, _),
        MaybeTimestamp = yes(Timestamp)
    ;
        ReturnTimestamp = do_not_return_timestamp,
        MaybeTimestamp = no
    ).

%-----------------------------------------------------------------------------%

:- pred check_timestamp(globals::in, file_name::in,
    maybe(io.res(timestamp))::in, maybe(timestamp)::out,
    io::di, io::uo) is det.

check_timestamp(Globals, FileName, MaybeTimestamp0, MaybeTimestamp, !IO) :-
    (
        MaybeTimestamp0 = yes(ok(Timestamp)),
        MaybeTimestamp = yes(Timestamp)
    ;
        MaybeTimestamp0 = yes(error(IOError)),
        MaybeTimestamp = no,
        globals.lookup_bool_option(Globals, smart_recompilation,
            SmartRecompilation),
        % Should we print the warning if smart recompilation has already been
        % disabled by an earlier error? At the moment, we do.
        (
            SmartRecompilation = yes,
            report_modification_time_warning(Globals, FileName, IOError, !IO)
        ;
            SmartRecompilation = no
        )
    ;
        MaybeTimestamp0 = no,
        MaybeTimestamp = no
    ).

:- pred report_modification_time_warning(globals::in, file_name::in,
    io.error::in, io::di, io::uo) is det.

report_modification_time_warning(Globals, SourceFileName, Error, !IO) :-
    io_set_disable_smart_recompilation(yes, !IO),
    io_set_disable_generate_item_version_numbers(yes, !IO),
    globals.lookup_bool_option(Globals, warn_smart_recompilation, Warn),
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
        globals.lookup_bool_option(Globals, halt_at_warn, HaltAtWarn),
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
:- end_module parse_tree.read_modules.
%-----------------------------------------------------------------------------%
