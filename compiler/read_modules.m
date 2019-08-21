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

:- import_module libs.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module libs.timestamp.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.error_util.
:- import_module parse_tree.file_kind.
:- import_module parse_tree.file_names.
:- import_module parse_tree.module_imports.
:- import_module parse_tree.parse_error.
:- import_module parse_tree.prog_item.

:- import_module io.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module term.

%-----------------------------------------------------------------------------%
%
% After we have read in Mercury code from a source file, interface file or
% optimization file, we record the parse tree we get from it, so we can
% avoid having to read it again.
% XXX ITEM_LIST We seem to sometimes re-read it anyway. Fix this.
%
% Since we use different types to represent the parse trees of source,
% interface and optimization files, we use three maps, one for each
% parse tree type. Each map maps a key, which consists of a module name
% and the kind of a file (e.g. .int0 vs .int2 for interface files)
% to the parse tree we got for that file.
%
% XXX ITEM_LIST The code that reads in optimization files does not
% record its results in hrmm_opt. I (zs) don't know whether that is
% a bug (leading to duplicate reads of optimization files) or a feature
% (keeping files that are by construction read exactly once out of a map
% where they won't be needed again).
%

:- type have_read_module_maps
    --->    have_read_module_maps(
                hrmm_src                :: have_read_module_src_map,
                hrmm_int                :: have_read_module_int_map,
                hrmm_opt                :: have_read_module_opt_map
            ).

:- type have_read_module_src_map ==
    have_read_module_map(src_file_kind, parse_tree_src).
:- type have_read_module_int_map ==
    have_read_module_map(int_file_kind, parse_tree_int).
:- type have_read_module_opt_map ==
    have_read_module_map(opt_file_kind, parse_tree_opt).

:- type have_read_module_map(FK, PT) ==
    map(have_read_module_key(FK), have_read_module(PT)).

:- type have_read_module_key(FK)
    --->    have_read_module_key(module_name, FK).

:- type have_read_module(PT)
    --->    have_read_module(
                file_name,
                module_timestamp,
                PT,
                list(error_spec),
                read_module_errors
            ).

%-----------------------------------------------------------------------------%

:- type maybe_ignore_errors
    --->    ignore_errors
    ;       do_not_ignore_errors.

    % read_module_src(Globals, Descr, IgnoreErrors, Search,
    %   ModuleName, FileName, ReadModuleAndTimestamps, MaybeTimestamp,
    %   ParseTreeSrc, Specs, Errors, !IO):
    %
    % Given a module name, read in and parse the source code of that file,
    % printing progress messages along the way if the verbosity level
    % calls for that.
    %
    % If ModuleName is a nested module, then try searching for different
    % filenames: for modules such as `foo.bar.baz.m', search first for
    % `foo.bar.baz.m', then `bar.baz.m', then `baz.m'. If Search is do_search,
    % search all directories given by the option search_directories for the
    % module; otherwise, search for those filenames only in the current
    % directory. Return in FileName the actual source file name found
    % (excluding the directory part). If the actual module name
    % (as determined by the `:- module' declaration) does not match
    % the specified module name, then report an error message,
    % but record the *expected* module name in the parse tree,
    % not the one we actually found. This is because most parts
    % of the compiler (including deps_map.m and make.module_dep_file.m)
    % rely on the invariant which says that if Errors does not contain
    % any fatal errors, then the returned ParseTreeSrc contains the
    % module with the expected name. Invocations of the compiler
    % that don't specify --make or request dependency map don't really
    % care which module name we return here; they will work either way,
    % the only difference being whether the names of the files they generate
    % are based on the expected or the actual module name.
    %
    % N.B. This reads a module given the MODULE name. If you want to read
    % a module given the FILE name, use `read_module_src_from_file'.
    %
    % If ReadModuleAndTimestamps is always_read_module(dont_return_timestamp),
    % return `no' in MaybeTimestamp.
    %
    % If ReadModuleAndTimestamps is always_read_module(do_return_timestamp),
    % attempt to return the modification time of the file in MaybeTimestamp.
    %
    % If ReadModuleAndTimestamps is dont_read_module_if_match(OldTimeStamp),
    % then
    %
    % - if the timestamp of that file is exactly OldTimestamp, then
    %   don't read the file, but return OldTimestamp as the file's timestamp,
    %   alongside a dummy parse tree; while
    % - if the timestamp of that file differs from OldTimestamp (virtually
    %   always because it is newer), then read the module from the file
    %   as usual, parse and return its contents as usual, and also return
    %   its actual timestamp.
    %
    % If the file could not be read, MaybeTimestamp will be `no'.
    %
:- pred read_module_src(globals::in, string::in,
    maybe_ignore_errors::in, maybe_search::in,
    module_name::in, list(term.context)::in, file_name::out,
    read_module_and_timestamps::in, maybe(timestamp)::out,
    parse_tree_src::out, list(error_spec)::out, read_module_errors::out,
    io::di, io::uo) is det.

    % read_module_int(Globals, Descr, IgnoreErrors, Search,
    %   ModuleName, IntFileKind, FileName, ReturnTimestamp, MaybeTimestamp,
    %   ParseTreeInt, Specs, Errors, !IO):
    %
    % Given a module name, and the identity of one of its interface files,
    % (.int0, .int, .int2 or .int3), read in and parse the contents of that
    % interface file, printing progress messages along the way if the
    % verbosity level calls for that.
    %
    % The meanings of the arguments are pretty much the same as for
    % read_module_src, but while the names of the files that contain source
    % files may not be fully module qualified, the names of interface files
    % are always fully module qualified, so read_module_int does not search
    % for the right filename. It knows what filename it looks for; the only
    % search it does, if Search is do_search, is to decide which directory
    % among the search directories contains the file with that filename.
    %
:- pred read_module_int(globals::in, string::in,
    maybe_ignore_errors::in, maybe_search::in,
    module_name::in, int_file_kind::in, file_name::out,
    read_module_and_timestamps::in, maybe(timestamp)::out,
    parse_tree_int::out, list(error_spec)::out, read_module_errors::out,
    io::di, io::uo) is det.

    % read_module_src_from_file(Globals, FileName, FileNameDotM,
    %   Descr, Search, ReadModuleAndTimestamps, MaybeTimestamp, ParseTreeSrc,
    %   Specs, Errors, !IO):
    %
    % Does pretty much the same job as read_module_src, but its job is
    % to read the module stored in a specified file (FileNameDotM),
    % discovering the name of the module stored there by reading the file,
    % as opposed to looking for the file containing a module with a specified
    % name. It does not search for the right filename (that is SourceFileName),
    % but if Search is do_search, it does search for that filename in the
    % specified directories.
    %
    % The rest of the argument list has the same meaning as in read_module_src.
    %
:- pred read_module_src_from_file(globals::in, file_name::in, file_name::in,
    string::in, maybe_search::in, read_module_and_timestamps::in,
    maybe(timestamp)::out, parse_tree_src::out,
    list(error_spec)::out, read_module_errors::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

    % maybe_read_module_int(Globals, HaveReadModuleMap, Descr, Search,
    %     ModuleName, IntFileKind, FileName, ReturnTimestamp, MaybeTimestamp,
    %     ParseTreeInt, Specs, Errors, !IO):
    %
    % If HaveReadModuleMap contains the already-read contents of the
    % IntFileKind interface file for ModuleName, then return the information
    % stored in HaveReadModuleMap for that file. If it is not there,
    % read that interface file using read_module_int, regardless of its
    % timestamp.
    %
:- pred maybe_read_module_int(globals::in, have_read_module_int_map::in,
    string::in, maybe_search::in, module_name::in, int_file_kind::in,
    file_name::out, maybe_return_timestamp::in, maybe(timestamp)::out,
    parse_tree_int::out, list(error_spec)::out, read_module_errors::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

    % find_read_module_src(HaveReadModuleMap, ModuleName,
    %   ReturnTimestamp, FileName, MaybeTimestamp, ParseTree, Specs, Errors):
    % find_read_module_int(HaveReadModuleMap, ModuleName, IntFileKind,
    %   ReturnTimestamp, FileName, MaybeTimestamp, ParseTree, Specs, Errors):
    %
    % Check whether HaveReadModuleMap contains the already-read contents
    % of the specified source file or interface file. If it does, return
    % its contents. If it does not, fail.
    %
:- pred find_read_module_src(have_read_module_src_map::in, module_name::in,
    maybe_return_timestamp::in, file_name::out, maybe(timestamp)::out,
    parse_tree_src::out, list(error_spec)::out, read_module_errors::out)
    is semidet.
:- pred find_read_module_int(have_read_module_int_map::in, module_name::in,
    int_file_kind::in, maybe_return_timestamp::in,
    file_name::out, maybe(timestamp)::out,
    parse_tree_int::out, list(error_spec)::out, read_module_errors::out)
    is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module libs.options.
:- import_module parse_tree.parse_module.
:- import_module parse_tree.find_module.

:- import_module bool.
:- import_module cord.
:- import_module dir.
:- import_module set.
:- import_module string.

%-----------------------------------------------------------------------------%

read_module_src(Globals, Descr, IgnoreErrors, Search,
        ModuleName, ExpectationContexts, FileName,
        ReadModuleAndTimestamps, MaybeTimestamp,
        ParseTreeSrc, Specs, Errors, !IO) :-
    read_module_begin(Globals, Descr, Search, ModuleName, fk_src,
        FileName0, VeryVerbose, InterfaceSearchDirs, SearchDirs, !IO),
    % For `.m' files we need to deal with the case where the module name
    % does not match the file name, or where a partial match occurs
    % in the current directory but the full match occurs in a search directory.
    search_for_module_source_and_stream(Globals, SearchDirs,
        InterfaceSearchDirs, ModuleName, MaybeFileNameAndStream, !IO),
    actually_read_module_src(Globals, ModuleName, ExpectationContexts,
        MaybeFileNameAndStream, ReadModuleAndTimestamps, MaybeTimestampRes,
        ParseTreeSrc0, ModuleSpecs, Errors, !IO),
    ParseTreeSrc0 = parse_tree_src(_ActualModuleName, ActualModuleNameContext,
        ComponentsCord),
    % If ModuleName = ActualModuleName, this obviously does the right thing.
    % If ModuleName != ActualModuleName, then we must include ModuleName
    % in ParseTreeSrc (see the comment above), and including recording
    % ActualModuleNameContext as its context shouldn't mislead anyone
    % who reads the error spec about the unexpected module name,
    % which should be in Specs.
    ParseTreeSrc = parse_tree_src(ModuleName, ActualModuleNameContext,
        ComponentsCord),
    IsEmpty = (if cord.is_empty(ComponentsCord) then yes else no),
    read_module_end(Globals, IgnoreErrors, VeryVerbose,
        MaybeFileNameAndStream, FileName0, FileName,
        MaybeTimestampRes, MaybeTimestamp,
        IsEmpty, ModuleSpecs, Specs, Errors, !IO).

read_module_int(Globals, Descr, IgnoreErrors, Search, ModuleName, IntFileKind,
        FileName, ReadModuleAndTimestamps, MaybeTimestamp,
        ParseTreeInt, Specs, Errors, !IO) :-
    read_module_begin(Globals, Descr, Search, ModuleName, fk_int(IntFileKind),
        FileName0, VeryVerbose, _InterfaceSearchDirs, SearchDirs, !IO),
    search_for_file_and_stream(SearchDirs, FileName0,
        MaybeFileNameAndStream, !IO),
    actually_read_module_int(IntFileKind, Globals, ModuleName, [],
        MaybeFileNameAndStream, ReadModuleAndTimestamps, MaybeTimestampRes,
        ParseTreeInt, ModuleSpecs, Errors, !IO),
    ParseTreeInt = parse_tree_int(_ActualModuleName, _IntFileKind,
        _ActualModuleNameContext, _MaybeVersionNumbers,
        IntIncls, ImpIncls, IntAvails, ImpAvails,
        IntFIMs, ImpFIMs, IntItems, ImplItems),
    ( if
        IntIncls = [],
        ImpIncls = [],
        IntAvails = [],
        ImpAvails = [],
        IntFIMs = [],
        ImpFIMs = [],
        IntItems = [],
        ImplItems = []
    then
        IsEmpty = yes
    else
        IsEmpty = no
    ),
    read_module_end(Globals, IgnoreErrors, VeryVerbose,
        MaybeFileNameAndStream, FileName0, FileName,
        MaybeTimestampRes, MaybeTimestamp,
        IsEmpty, ModuleSpecs, Specs, Errors, !IO).

read_module_src_from_file(Globals, FileName, FileNameDotM, Descr, Search,
        ReadModuleAndTimestamps, MaybeTimestamp,
        ParseTreeSrc, Specs, Errors, !IO) :-
    globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
    maybe_write_string(VeryVerbose, "% ", !IO),
    maybe_write_string(VeryVerbose, Descr, !IO),
    maybe_write_string(VeryVerbose, " `", !IO),
    maybe_write_string(VeryVerbose, FileNameDotM, !IO),
    maybe_write_string(VeryVerbose, "'... ", !IO),
    maybe_flush_output(VeryVerbose, !IO),
    ( if dir.basename(FileName, BaseFileNamePrime) then
        BaseFileName = BaseFileNamePrime
    else
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
    search_for_file_and_stream(SearchDirs, FileNameDotM,
        MaybeFileNameAndStream, !IO),
    actually_read_module_src(Globals, DefaultModuleName, [],
        MaybeFileNameAndStream, ReadModuleAndTimestamps, MaybeTimestampRes,
        ParseTreeSrc, Specs0, Errors, !IO),
    check_timestamp(Globals, FileNameDotM, MaybeTimestampRes, MaybeTimestamp,
        !IO),
    handle_any_read_module_errors(Globals, VeryVerbose, Errors,
        Specs0, Specs, !IO).

%-----------------------------------------------------------------------------%

:- pred read_module_begin(globals::in, string::in,
    maybe_search::in, module_name::in, file_kind::in, file_name::out,
    bool::out, list(string)::out, list(string)::out, io::di, io::uo) is det.

read_module_begin(Globals, Descr, Search, ModuleName, FileKind,
        FileName0, VeryVerbose, InterfaceSearchDirs, SearchDirs, !IO) :-
    Extension = file_kind_to_extension(FileKind),
    (
        Search = do_search,
        module_name_to_search_file_name(Globals, Extension,
            ModuleName, FileName0, !IO)
    ;
        Search = do_not_search,
        module_name_to_file_name(Globals, do_not_create_dirs, Extension,
            ModuleName, FileName0, !IO)
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
    ).

:- pred read_module_end(globals::in, maybe_ignore_errors::in, bool::in,
    maybe_error(path_name_and_stream)::in, file_name::in, file_name::out,
    maybe(io.res(timestamp))::in, maybe(timestamp)::out, bool::in,
    list(error_spec)::in, list(error_spec)::out, read_module_errors::in,
    io::di, io::uo) is det.

read_module_end(Globals, IgnoreErrors, VeryVerbose,
        MaybeFileNameAndStream, FileName0, FileName,
        MaybeTimestampRes, MaybeTimestamp, IsEmpty,
        ModuleSpecs, Specs, Errors, !IO) :-
    (
        MaybeFileNameAndStream = ok(path_name_and_stream(FileName, _))
    ;
        MaybeFileNameAndStream = error(_),
        FileName = FileName0
    ),
    check_timestamp(Globals, FileName0, MaybeTimestampRes, MaybeTimestamp,
        !IO),
    (
        IgnoreErrors = ignore_errors,
        ( if
            set.contains(Errors, rme_could_not_open_file),
            % I (zs) think the test of IsEmpty is redundant, and could be
            % buggy as well (since a file containing just ":- module x"
            % would now yield an empty item list), but better safe than sorry.
            IsEmpty = yes
        then
            maybe_write_string(VeryVerbose, "not found.\n", !IO),
            Specs = []
        else
            maybe_write_string(VeryVerbose, "done.\n", !IO),
            Specs = ModuleSpecs
        )
    ;
        IgnoreErrors = do_not_ignore_errors,
        handle_any_read_module_errors(Globals, VeryVerbose, Errors,
            ModuleSpecs, Specs, !IO)
    ).

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

maybe_read_module_int(Globals, HaveReadModuleMap, Descr, Search,
        ModuleName, IntFileKind, FileName, ReturnTimestamp, MaybeTimestamp,
        ParseTreeInt, Specs, Errors, !IO) :-
    ( if
        find_read_module_int(HaveReadModuleMap, ModuleName, IntFileKind,
            ReturnTimestamp, FileNamePrime, MaybeTimestampPrime,
            ParseTreeIntPrime, SpecsPrime, ErrorsPrime)
    then
        FileName = FileNamePrime,
        MaybeTimestamp = MaybeTimestampPrime,
        ParseTreeInt = ParseTreeIntPrime,
        Specs = SpecsPrime,
        Errors = ErrorsPrime
    else
        read_module_int(Globals, Descr, do_not_ignore_errors, Search,
            ModuleName, IntFileKind, FileName,
            always_read_module(ReturnTimestamp), MaybeTimestamp,
            ParseTreeInt, Specs, Errors, !IO)
    ).

%-----------------------------------------------------------------------------%

find_read_module_src(HaveReadModuleMap, ModuleName, ReturnTimestamp,
        FileName, MaybeTimestamp, ParseTreeSrc, Specs, Errors) :-
    Key = have_read_module_key(ModuleName, sfk_src),
    map.search(HaveReadModuleMap, Key, HaveReadModule),
    HaveReadModule = have_read_module(FileName, ModuleTimestamp,
        ParseTreeSrc, Specs, Errors),
    (
        ReturnTimestamp = do_return_timestamp,
        ModuleTimestamp = module_timestamp(_, Timestamp, _),
        MaybeTimestamp = yes(Timestamp)
    ;
        ReturnTimestamp = dont_return_timestamp,
        MaybeTimestamp = no
    ).

find_read_module_int(HaveReadModuleMap, ModuleName, IntFileKind,
        ReturnTimestamp, FileName, MaybeTimestamp,
        ParseTreeInt, Specs, Errors) :-
    Key = have_read_module_key(ModuleName, IntFileKind),
    map.search(HaveReadModuleMap, Key, HaveReadModule),
    HaveReadModule = have_read_module(FileName, ModuleTimestamp,
        ParseTreeInt, Specs, Errors),
    (
        ReturnTimestamp = do_return_timestamp,
        ModuleTimestamp = module_timestamp(_, Timestamp, _),
        MaybeTimestamp = yes(Timestamp)
    ;
        ReturnTimestamp = dont_return_timestamp,
        MaybeTimestamp = no
    ).

%-----------------------------------------------------------------------------%

:- pred check_timestamp(globals::in, file_name::in,
    maybe(io.res(timestamp))::in, maybe(timestamp)::out,
    io::di, io::uo) is det.

check_timestamp(Globals, FileName, MaybeTimestampRes, MaybeTimestamp, !IO) :-
    (
        MaybeTimestampRes = yes(ok(Timestamp)),
        MaybeTimestamp = yes(Timestamp)
    ;
        MaybeTimestampRes = yes(error(IOError)),
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
        MaybeTimestampRes = no,
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
