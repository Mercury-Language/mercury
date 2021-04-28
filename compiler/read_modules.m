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

                % XXX CLEANUP We should stop using and delete this field
                % as soon as we can, using the last two fields instead.
                hrmm_opt                :: have_read_module_opt_map,

                % XXX CLEANUP We should store parse_tree_module_srcs,
                % not raw_compilation_units.
                hrmm_rcu                :: have_read_module_rcu_map,

                hrmm_int0               :: have_read_module_int0_map,
                hrmm_int1               :: have_read_module_int1_map,
                hrmm_int2               :: have_read_module_int2_map,
                hrmm_int3               :: have_read_module_int3_map,

                hrmm_plain_opt          :: have_read_module_plain_opt_map,
                hrmm_trans_opt          :: have_read_module_trans_opt_map
            ).

:- func init_have_read_module_maps = have_read_module_maps.

:- type have_read_module_src_map ==
    have_read_module_map(module_name, parse_tree_src).
:- type have_read_module_opt_map ==
    have_read_module_map(have_read_module_key(opt_file_kind), parse_tree_opt).

:- type have_read_module_key(Kind)
    --->    have_read_module_key(module_name, Kind).

:- type have_read_module_rcu_map ==
    map(module_name, raw_compilation_unit).

:- type have_read_module_int0_map ==
    have_read_module_map(module_name, parse_tree_int0).
:- type have_read_module_int1_map ==
    have_read_module_map(module_name, parse_tree_int1).
:- type have_read_module_int2_map ==
    have_read_module_map(module_name, parse_tree_int2).
:- type have_read_module_int3_map ==
    have_read_module_map(module_name, parse_tree_int3).

:- type have_read_module_plain_opt_map ==
    have_read_module_map(module_name, parse_tree_plain_opt).
:- type have_read_module_trans_opt_map ==
    have_read_module_map(module_name, parse_tree_trans_opt).

:- type have_read_module_map(Key, PT) == map(Key, have_read_module(PT)).

:- type have_read_module(PT)
    --->    have_successfully_read_module(
                file_name,
                % Not all reads of e.g. interface files request
                % that the read return the timestamp of the file.
                maybe(timestamp),
                PT,
                list(error_spec),
                read_module_errors
            )
    ;       tried_to_read_module_failed.

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
    % If Search is do_search, search all directories given by the option
    % search_directories for the module;
    % otherwise, search for those filenames only in the current directory.
    % Return in FileName the actual source file name found
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

    % read_module_src_from_file(Globals, FileName, FileNameDotM,
    %   Descr, Search, ReadModuleAndTimestamps, MaybeTimestamp, ParseTreeSrc,
    %   Specs, Errors, !IO):
    %
    % Does pretty much the same job as read_module_src, but its job is
    % to read the module stored in a specified file (FileNameDotM),
    % discovering the name of the module stored there by reading the file,
    % as opposed to looking for the file containing a module with a specified
    % name. It does not search for the right filename (that is FileNameDotM),
    % but if Search is do_search, it does search for that filename in the
    % specified directories.
    %
    % The rest of the argument list has the same meaning as in read_module_src.
    %
:- pred read_module_src_from_file(globals::in, file_name::in, file_name::in,
    string::in, maybe_search::in, read_module_and_timestamps::in,
    maybe(timestamp)::out, parse_tree_src::out,
    list(error_spec)::out, read_module_errors::out, io::di, io::uo) is det.

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
    % XXX CLEANUP Calls to this predicate should be replaced by calls
    % to its int_file_kind-specific versions below. The only such calls
    % are now used in the implementation of smart recompilation.
    %
:- pred read_module_int(globals::in, string::in,
    maybe_ignore_errors::in, maybe_search::in,
    module_name::in, int_file_kind::in, file_name::out,
    read_module_and_timestamps::in, maybe(timestamp)::out,
    parse_tree_int::out, list(error_spec)::out, read_module_errors::out,
    io::di, io::uo) is det.

:- pred read_module_some_int(globals::in, string::in,
    maybe_ignore_errors::in, maybe_search::in,
    module_name::in, int_file_kind::in, file_name::out,
    read_module_and_timestamps::in, maybe(timestamp)::out,
    parse_tree_some_int::out, list(error_spec)::out, read_module_errors::out,
    io::di, io::uo) is det.

    % Versions of read_module_int that read and return a non-generic
    % parse tree.
    %
:- pred read_module_int0(globals::in, string::in,
    maybe_ignore_errors::in, maybe_search::in,
    module_name::in, file_name::out,
    read_module_and_timestamps::in, maybe(timestamp)::out,
    parse_tree_int0::out, list(error_spec)::out, read_module_errors::out,
    io::di, io::uo) is det.
:- pred read_module_int1(globals::in, string::in,
    maybe_ignore_errors::in, maybe_search::in,
    module_name::in, file_name::out,
    read_module_and_timestamps::in, maybe(timestamp)::out,
    parse_tree_int1::out, list(error_spec)::out, read_module_errors::out,
    io::di, io::uo) is det.
:- pred read_module_int2(globals::in, string::in,
    maybe_ignore_errors::in, maybe_search::in,
    module_name::in, file_name::out,
    read_module_and_timestamps::in, maybe(timestamp)::out,
    parse_tree_int2::out, list(error_spec)::out, read_module_errors::out,
    io::di, io::uo) is det.
:- pred read_module_int3(globals::in, string::in,
    maybe_ignore_errors::in, maybe_search::in,
    module_name::in, file_name::out,
    read_module_and_timestamps::in, maybe(timestamp)::out,
    parse_tree_int3::out, list(error_spec)::out, read_module_errors::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

    % maybe_read_module_intN(Globals, Descr, Search,
    %     ModuleName, IntFileKind, FileName, ReturnTimestamp, MaybeTimestamp,
    %     ParseTreeInt, Specs, Errors, !HaveReadModuleMaps, !IO):
    %
    % If HaveReadModuleMap contains the already-read contents of the
    % relevant kind of interface file for ModuleName, then return
    % the information stored in HaveReadModuleMap for that file.
    % If it is not there, read that interface file using read_module_intN,
    % regardless of its timestamp.
    %
:- pred maybe_read_module_int0(globals::in, maybe_search::in, module_name::in,
    file_name::out, maybe_return_timestamp::in, maybe(timestamp)::out,
    parse_tree_int0::out, list(error_spec)::out, read_module_errors::out,
    have_read_module_maps::in, have_read_module_maps::out,
    io::di, io::uo) is det.
:- pred maybe_read_module_int1(globals::in, maybe_search::in, module_name::in,
    file_name::out, maybe_return_timestamp::in, maybe(timestamp)::out,
    parse_tree_int1::out, list(error_spec)::out, read_module_errors::out,
    have_read_module_maps::in, have_read_module_maps::out,
    io::di, io::uo) is det.
:- pred maybe_read_module_int2(globals::in, maybe_search::in, module_name::in,
    file_name::out, maybe_return_timestamp::in, maybe(timestamp)::out,
    parse_tree_int2::out, list(error_spec)::out, read_module_errors::out,
    have_read_module_maps::in, have_read_module_maps::out,
    io::di, io::uo) is det.
:- pred maybe_read_module_int3(globals::in, maybe_search::in, module_name::in,
    file_name::out, maybe_return_timestamp::in, maybe(timestamp)::out,
    parse_tree_int3::out, list(error_spec)::out, read_module_errors::out,
    have_read_module_maps::in, have_read_module_maps::out,
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

:- pred find_read_module_some_int(have_read_module_maps::in,
    module_name::in, int_file_kind::in,
    maybe_return_timestamp::in, file_name::out, maybe(timestamp)::out,
    parse_tree_some_int::out, list(error_spec)::out, read_module_errors::out)
    is semidet.

    % Versions of find_read_module_int that return a non-generic parse tree.
    %
:- pred find_read_module_int0(have_read_module_int0_map::in, module_name::in,
    maybe_return_timestamp::in, file_name::out, maybe(timestamp)::out,
    parse_tree_int0::out, list(error_spec)::out, read_module_errors::out)
    is semidet.
:- pred find_read_module_int1(have_read_module_int1_map::in, module_name::in,
    maybe_return_timestamp::in, file_name::out, maybe(timestamp)::out,
    parse_tree_int1::out, list(error_spec)::out, read_module_errors::out)
    is semidet.
:- pred find_read_module_int2(have_read_module_int2_map::in, module_name::in,
    maybe_return_timestamp::in, file_name::out, maybe(timestamp)::out,
    parse_tree_int2::out, list(error_spec)::out, read_module_errors::out)
    is semidet.
:- pred find_read_module_int3(have_read_module_int3_map::in, module_name::in,
    maybe_return_timestamp::in, file_name::out, maybe(timestamp)::out,
    parse_tree_int3::out, list(error_spec)::out, read_module_errors::out)
    is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module libs.options.
:- import_module parse_tree.find_module.
:- import_module parse_tree.parse_module.
:- import_module parse_tree.source_file_map.

:- import_module bool.
:- import_module cord.
:- import_module dir.
:- import_module require.
:- import_module set.
:- import_module string.

%-----------------------------------------------------------------------------%

init_have_read_module_maps = 
    have_read_module_maps(map.init, map.init,
        map.init, map.init, map.init, map.init, map.init, map.init, map.init).

%-----------------------------------------------------------------------------%

read_module_src(Globals, Descr, IgnoreErrors, Search,
        ModuleName, ExpectationContexts, FileName,
        ReadModuleAndTimestamps, MaybeTimestamp,
        ParseTreeSrc, Specs, Errors, !IO) :-
    read_module_begin(Globals, Descr, Search, ModuleName, fk_src,
        FileName0, VeryVerbose, SearchDirs, !IO),
    % For `.m' files, we need to deal with the case where the module name
    % does not match the file name.
    search_for_module_source_and_stream(SearchDirs, ModuleName,
        MaybeFileNameAndStream, !IO),
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
    read_module_end_module(Globals, IgnoreErrors, VeryVerbose,
        MaybeFileNameAndStream, FileName0, FileName,
        MaybeTimestampRes, MaybeTimestamp, ModuleSpecs, Specs, Errors, !IO).

read_module_src_from_file(Globals, FileName, FileNameDotM, Descr, Search,
        ReadModuleAndTimestamps, MaybeTimestamp,
        ParseTreeSrc, Specs, Errors, !IO) :-
    read_module_begin_from_file(Globals, Descr, Search,
        FileName, FileNameDotM, DefaultModuleName,
        VeryVerbose, SearchDirs, !IO),
    search_for_file_and_stream(SearchDirs, FileNameDotM,
        MaybeFileNameAndStream, !IO),
    actually_read_module_src(Globals, DefaultModuleName, [],
        MaybeFileNameAndStream, ReadModuleAndTimestamps, MaybeTimestampRes,
        ParseTreeSrc, ModuleSpecs, Errors, !IO),
    read_module_end_file(Globals, VeryVerbose, FileNameDotM,
        MaybeTimestampRes, MaybeTimestamp, ModuleSpecs, Specs, Errors, !IO).

read_module_int(Globals, Descr, IgnoreErrors, Search, ModuleName, IntFileKind,
        FileName, ReadModuleAndTimestamps, MaybeTimestamp,
        ParseTreeInt, Specs, Errors, !IO) :-
    read_module_begin(Globals, Descr, Search, ModuleName, fk_int(IntFileKind),
        FileName0, VeryVerbose, SearchDirs, !IO),
    search_for_file_and_stream(SearchDirs, FileName0,
        MaybeFileNameAndStream, !IO),
    actually_read_module_int(IntFileKind, Globals, ModuleName, [],
        MaybeFileNameAndStream, ReadModuleAndTimestamps, MaybeTimestampRes,
        ParseTreeInt, ModuleSpecs, Errors, !IO),
    read_module_end_module(Globals, IgnoreErrors, VeryVerbose,
        MaybeFileNameAndStream, FileName0, FileName,
        MaybeTimestampRes, MaybeTimestamp, ModuleSpecs, Specs, Errors, !IO).

read_module_some_int(Globals, Descr, IgnoreErrors, Search, ModuleName,
        IntFileKind, FileName, ReadModuleAndTimestamps, MaybeTimestamp,
        ParseTreeSomeInt, Specs, Errors, !IO) :-
    (
        IntFileKind = ifk_int0,
        read_module_int0(Globals, Descr, IgnoreErrors, Search, ModuleName,
            FileName, ReadModuleAndTimestamps, MaybeTimestamp,
            ParseTreeInt0, Specs, Errors, !IO),
        ParseTreeSomeInt = parse_tree_some_int0(ParseTreeInt0)
    ;
        IntFileKind = ifk_int1,
        read_module_int1(Globals, Descr, IgnoreErrors, Search, ModuleName,
            FileName, ReadModuleAndTimestamps, MaybeTimestamp,
            ParseTreeInt1, Specs, Errors, !IO),
        ParseTreeSomeInt = parse_tree_some_int1(ParseTreeInt1)
    ;
        IntFileKind = ifk_int2,
        read_module_int2(Globals, Descr, IgnoreErrors, Search, ModuleName,
            FileName, ReadModuleAndTimestamps, MaybeTimestamp,
            ParseTreeInt2, Specs, Errors, !IO),
        ParseTreeSomeInt = parse_tree_some_int2(ParseTreeInt2)
    ;
        IntFileKind = ifk_int3,
        read_module_int3(Globals, Descr, IgnoreErrors, Search, ModuleName,
            FileName, ReadModuleAndTimestamps, MaybeTimestamp,
            ParseTreeInt3, Specs, Errors, !IO),
        ParseTreeSomeInt = parse_tree_some_int3(ParseTreeInt3)
    ).

read_module_int0(Globals, Descr, IgnoreErrors, Search, ModuleName,
        FileName, ReadModuleAndTimestamps, MaybeTimestamp,
        ParseTreeInt0, Specs, Errors, !IO) :-
    read_module_begin(Globals, Descr, Search, ModuleName, fk_int(ifk_int0),
        FileName0, VeryVerbose, SearchDirs, !IO),
    search_for_file_and_stream(SearchDirs, FileName0,
        MaybeFileNameAndStream, !IO),
    actually_read_module_int0(Globals, ModuleName, [],
        MaybeFileNameAndStream, ReadModuleAndTimestamps, MaybeTimestampRes,
        ParseTreeInt0, ModuleSpecs, Errors, !IO),
    read_module_end_module(Globals, IgnoreErrors, VeryVerbose,
        MaybeFileNameAndStream, FileName0, FileName,
        MaybeTimestampRes, MaybeTimestamp, ModuleSpecs, Specs, Errors, !IO).

read_module_int1(Globals, Descr, IgnoreErrors, Search, ModuleName,
        FileName, ReadModuleAndTimestamps, MaybeTimestamp,
        ParseTreeInt1, Specs, Errors, !IO) :-
    read_module_begin(Globals, Descr, Search, ModuleName, fk_int(ifk_int1),
        FileName0, VeryVerbose, SearchDirs, !IO),
    search_for_file_and_stream(SearchDirs, FileName0,
        MaybeFileNameAndStream, !IO),
    actually_read_module_int1(Globals, ModuleName, [],
        MaybeFileNameAndStream, ReadModuleAndTimestamps, MaybeTimestampRes,
        ParseTreeInt1, ModuleSpecs, Errors, !IO),
    read_module_end_module(Globals, IgnoreErrors, VeryVerbose,
        MaybeFileNameAndStream, FileName0, FileName,
        MaybeTimestampRes, MaybeTimestamp, ModuleSpecs, Specs, Errors, !IO).

read_module_int2(Globals, Descr, IgnoreErrors, Search, ModuleName,
        FileName, ReadModuleAndTimestamps, MaybeTimestamp,
        ParseTreeInt2, Specs, Errors, !IO) :-
    read_module_begin(Globals, Descr, Search, ModuleName, fk_int(ifk_int2),
        FileName0, VeryVerbose, SearchDirs, !IO),
    search_for_file_and_stream(SearchDirs, FileName0,
        MaybeFileNameAndStream, !IO),
    actually_read_module_int2(Globals, ModuleName, [],
        MaybeFileNameAndStream, ReadModuleAndTimestamps, MaybeTimestampRes,
        ParseTreeInt2, ModuleSpecs, Errors, !IO),
    read_module_end_module(Globals, IgnoreErrors, VeryVerbose,
        MaybeFileNameAndStream, FileName0, FileName,
        MaybeTimestampRes, MaybeTimestamp, ModuleSpecs, Specs, Errors, !IO).

read_module_int3(Globals, Descr, IgnoreErrors, Search, ModuleName,
        FileName, ReadModuleAndTimestamps, MaybeTimestamp,
        ParseTreeInt3, Specs, Errors, !IO) :-
    read_module_begin(Globals, Descr, Search, ModuleName, fk_int(ifk_int3),
        FileName0, VeryVerbose, SearchDirs, !IO),
    search_for_file_and_stream(SearchDirs, FileName0,
        MaybeFileNameAndStream, !IO),
    actually_read_module_int3(Globals, ModuleName, [],
        MaybeFileNameAndStream, ReadModuleAndTimestamps, MaybeTimestampRes,
        ParseTreeInt3, ModuleSpecs, Errors, !IO),
    read_module_end_module(Globals, IgnoreErrors, VeryVerbose,
        MaybeFileNameAndStream, FileName0, FileName,
        MaybeTimestampRes, MaybeTimestamp, ModuleSpecs, Specs, Errors, !IO).

%-----------------------------------------------------------------------------%

:- pred read_module_begin_from_file(globals::in, string::in,
    maybe_search::in, file_name::in, file_name::in, module_name::out,
    bool::out, list(string)::out, io::di, io::uo) is det.

read_module_begin_from_file(Globals, Descr, Search, FileName, FileNameDotM,
        DefaultModuleName, VeryVerbose, SearchDirs, !IO) :-
    ( if dir.basename(FileName, BaseFileNamePrime) then
        BaseFileName = BaseFileNamePrime
    else
        BaseFileName = ""
    ),
    have_source_file_map(HaveMap, !IO),
    (
        HaveMap = no,
        file_name_to_module_name(BaseFileName, DefaultModuleName)
    ;
        HaveMap = yes,
        lookup_source_file_module(FileNameDotM, MaybeModuleName, !IO),
        (
            MaybeModuleName = yes(DefaultModuleName)
        ;
            MaybeModuleName = no,
            file_name_to_module_name(BaseFileName, DefaultModuleName)
        )
    ),
    % The rest of this predicate should be kept in sync
    % with read_module_begin.
    (
        Search = do_search,
        globals.lookup_accumulating_option(Globals, search_directories,
            SearchDirs)
    ;
        Search = do_not_search,
        SearchDirs = [dir.this_directory]
    ),
    globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
    (
        VeryVerbose = no
    ;
        VeryVerbose = yes,
        io.format("%% Reading %s `%s'... ", [s(Descr), s(FileNameDotM)], !IO),
        io.flush_output(!IO)
    ).

:- pred read_module_begin(globals::in, string::in,
    maybe_search::in, module_name::in, file_kind::in, file_name::out,
    bool::out, list(string)::out, io::di, io::uo) is det.

read_module_begin(Globals, Descr, Search, ModuleName, FileKind,
        FileName0, VeryVerbose, SearchDirs, !IO) :-
    file_kind_to_extension(FileKind, _ExtStr, Ext),
    (
        Search = do_search,
        module_name_to_search_file_name(Globals, $pred, Ext,
            ModuleName, FileName0, !IO)
    ;
        Search = do_not_search,
        module_name_to_file_name(Globals, $pred, do_not_create_dirs, Ext,
            ModuleName, FileName0, !IO)
    ),
    % The rest of this predicate should be kept in sync
    % with read_module_begin_from_file.
    (
        Search = do_search,
        globals.lookup_accumulating_option(Globals, search_directories,
            SearchDirs)
    ;
        Search = do_not_search,
        SearchDirs = [dir.this_directory]
    ),
    globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
    (
        VeryVerbose = no
    ;
        VeryVerbose = yes,
        io.format("%% %s `%s'... ", [s(Descr), s(FileName0)], !IO),
        io.flush_output(!IO)
    ).

:- pred read_module_end_module(globals::in, maybe_ignore_errors::in, bool::in,
    maybe_error(path_name_and_stream)::in, file_name::in, file_name::out,
    maybe(io.res(timestamp))::in, maybe(timestamp)::out,
    list(error_spec)::in, list(error_spec)::out, read_module_errors::in,
    io::di, io::uo) is det.

read_module_end_module(Globals, IgnoreErrors, VeryVerbose,
        MaybeFileNameAndStream, FileName0, FileName,
        MaybeTimestampRes, MaybeTimestamp, ModuleSpecs, Specs, Errors, !IO) :-
    % The code of read_module_end_module and read_module_end_file
    % should be kept in sync.
    (
        MaybeFileNameAndStream = ok(path_name_and_stream(FileName, _))
    ;
        MaybeFileNameAndStream = error(_),
        FileName = FileName0
    ),
    check_timestamp_report_if_needed_and_missing(Globals, FileName0,
        MaybeTimestampRes, MaybeTimestamp, !IO),
    (
        IgnoreErrors = ignore_errors,
        ( if set.contains(Errors, rme_could_not_open_file) then
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
    ),
    globals.lookup_bool_option(Globals, detailed_statistics, Statistics),
    maybe_report_stats(Statistics, !IO).

:- pred read_module_end_file(globals::in, bool::in, file_name::in,
    maybe(io.res(timestamp))::in, maybe(timestamp)::out,
    list(error_spec)::in, list(error_spec)::out, read_module_errors::in,
    io::di, io::uo) is det.

read_module_end_file(Globals, VeryVerbose, FileName,
        MaybeTimestampRes, MaybeTimestamp,
        ModuleSpecs, Specs, Errors, !IO) :-
    % The code of read_module_end_module and read_module_end_file
    % should be kept in sync.
    %
    % Unlike read_module_end_module, we assume do_not_ignore_errors.
    check_timestamp_report_if_needed_and_missing(Globals, FileName,
        MaybeTimestampRes, MaybeTimestamp, !IO),
    handle_any_read_module_errors(Globals, VeryVerbose, Errors,
        ModuleSpecs, Specs, !IO).

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
        pre_hlds_maybe_write_out_errors(VeryVerbose, Globals, !Specs, !IO),
        io.set_exit_status(1, !IO)
    ).

%-----------------------------------------------------------------------------%

maybe_read_module_int0(Globals, Search,
        ModuleName, FileName, ReturnTimestamp, MaybeTimestamp,
        ParseTreeInt0, Specs, Errors, !HaveReadModuleMaps, !IO) :-
    OrigHaveReadModuleMapInt0 = !.HaveReadModuleMaps ^ hrmm_int0,
    ( if
        find_read_module_int0(OrigHaveReadModuleMapInt0, ModuleName,
            ReturnTimestamp, FileNamePrime, MaybeTimestampPrime,
            ParseTreeInt0Prime, SpecsPrime, ErrorsPrime)
    then
        FileName = FileNamePrime,
        MaybeTimestamp = MaybeTimestampPrime,
        ParseTreeInt0 = ParseTreeInt0Prime,
        Specs = SpecsPrime,
        Errors = ErrorsPrime
    else
        Descr = "Reading .int0 interface for module",
        read_module_int0(Globals, Descr, do_not_ignore_errors, Search,
            ModuleName, FileName,
            always_read_module(ReturnTimestamp), MaybeTimestamp,
            ParseTreeInt0, Specs, Errors, !IO),
        ( if set.member(rme_could_not_open_file, Errors) then
            HaveReadModule = tried_to_read_module_failed,
            map.set(ModuleName, HaveReadModule,
                OrigHaveReadModuleMapInt0, HaveReadModuleMapInt0)
        else
            expect(unify(ModuleName, ParseTreeInt0 ^ pti0_module_name), $pred,
                "ModuleName != module name in ParseTreeInt0"),
            HaveReadModule = have_successfully_read_module(FileName,
                MaybeTimestamp, ParseTreeInt0, Specs, Errors),
            map.det_insert(ModuleName, HaveReadModule,
                OrigHaveReadModuleMapInt0, HaveReadModuleMapInt0)
        ),
        !HaveReadModuleMaps ^ hrmm_int0 := HaveReadModuleMapInt0
    ).

maybe_read_module_int1(Globals, Search,
        ModuleName, FileName, ReturnTimestamp, MaybeTimestamp,
        ParseTreeInt1, Specs, Errors, !HaveReadModuleMaps, !IO) :-
    OrigHaveReadModuleMapInt1 = !.HaveReadModuleMaps ^ hrmm_int1,
    ( if
        find_read_module_int1(OrigHaveReadModuleMapInt1, ModuleName,
            ReturnTimestamp, FileNamePrime, MaybeTimestampPrime,
            ParseTreeInt1Prime, SpecsPrime, ErrorsPrime)
    then
        FileName = FileNamePrime,
        MaybeTimestamp = MaybeTimestampPrime,
        ParseTreeInt1 = ParseTreeInt1Prime,
        Specs = SpecsPrime,
        Errors = ErrorsPrime
    else
        Descr = "Reading .int interface for module",
        read_module_int1(Globals, Descr, do_not_ignore_errors, Search,
            ModuleName, FileName,
            always_read_module(ReturnTimestamp), MaybeTimestamp,
            ParseTreeInt1, Specs, Errors, !IO),
        ( if set.member(rme_could_not_open_file, Errors) then
            HaveReadModule = tried_to_read_module_failed,
            map.set(ModuleName, HaveReadModule,
                OrigHaveReadModuleMapInt1, HaveReadModuleMapInt1)
        else
            expect(unify(ModuleName, ParseTreeInt1 ^ pti1_module_name), $pred,
                "ModuleName != module name in ParseTreeInt1"),
            HaveReadModule = have_successfully_read_module(FileName,
                MaybeTimestamp, ParseTreeInt1, Specs, Errors),
            map.det_insert(ModuleName, HaveReadModule,
                OrigHaveReadModuleMapInt1, HaveReadModuleMapInt1)
        ),
        !HaveReadModuleMaps ^ hrmm_int1 := HaveReadModuleMapInt1
    ).

maybe_read_module_int2(Globals, Search,
        ModuleName, FileName, ReturnTimestamp, MaybeTimestamp,
        ParseTreeInt2, Specs, Errors, !HaveReadModuleMaps, !IO) :-
    OrigHaveReadModuleMapInt2 = !.HaveReadModuleMaps ^ hrmm_int2,
    ( if
        find_read_module_int2(OrigHaveReadModuleMapInt2, ModuleName,
            ReturnTimestamp, FileNamePrime, MaybeTimestampPrime,
            ParseTreeInt2Prime, SpecsPrime, ErrorsPrime)
    then
        FileName = FileNamePrime,
        MaybeTimestamp = MaybeTimestampPrime,
        ParseTreeInt2 = ParseTreeInt2Prime,
        Specs = SpecsPrime,
        Errors = ErrorsPrime
    else
        Descr = "Reading .int2 interface for module",
        read_module_int2(Globals, Descr, do_not_ignore_errors, Search,
            ModuleName, FileName,
            always_read_module(ReturnTimestamp), MaybeTimestamp,
            ParseTreeInt2, Specs, Errors, !IO),
        ( if set.member(rme_could_not_open_file, Errors) then
            HaveReadModule = tried_to_read_module_failed,
            map.set(ModuleName, HaveReadModule,
                OrigHaveReadModuleMapInt2, HaveReadModuleMapInt2)
        else
            expect(unify(ModuleName, ParseTreeInt2 ^ pti2_module_name), $pred,
                "ModuleName != module name in ParseTreeInt2"),
            HaveReadModule = have_successfully_read_module(FileName,
                MaybeTimestamp, ParseTreeInt2, Specs, Errors),
            map.det_insert(ModuleName, HaveReadModule,
                OrigHaveReadModuleMapInt2, HaveReadModuleMapInt2)
        ),
        !HaveReadModuleMaps ^ hrmm_int2 := HaveReadModuleMapInt2
    ).

maybe_read_module_int3(Globals, Search,
        ModuleName, FileName, ReturnTimestamp, MaybeTimestamp,
        ParseTreeInt3, Specs, Errors, !HaveReadModuleMaps, !IO) :-
    OrigHaveReadModuleMapInt3 = !.HaveReadModuleMaps ^ hrmm_int3,
    ( if
        find_read_module_int3(OrigHaveReadModuleMapInt3, ModuleName,
            ReturnTimestamp, FileNamePrime, MaybeTimestampPrime,
            ParseTreeInt3Prime, SpecsPrime, ErrorsPrime)
    then
        FileName = FileNamePrime,
        MaybeTimestamp = MaybeTimestampPrime,
        ParseTreeInt3 = ParseTreeInt3Prime,
        Specs = SpecsPrime,
        Errors = ErrorsPrime
    else
        Descr = "Reading .int3 interface for module",
        read_module_int3(Globals, Descr, do_not_ignore_errors, Search,
            ModuleName, FileName,
            always_read_module(ReturnTimestamp), MaybeTimestamp,
            ParseTreeInt3, Specs, Errors, !IO),
        ( if set.member(rme_could_not_open_file, Errors) then
            HaveReadModule = tried_to_read_module_failed,
            map.set(ModuleName, HaveReadModule,
                OrigHaveReadModuleMapInt3, HaveReadModuleMapInt3)
        else
            expect(unify(ModuleName, ParseTreeInt3 ^ pti3_module_name), $pred,
                "ModuleName != module name in ParseTreeInt3"),
            HaveReadModule = have_successfully_read_module(FileName,
                MaybeTimestamp, ParseTreeInt3, Specs, Errors),
            map.det_insert(ModuleName, HaveReadModule,
                OrigHaveReadModuleMapInt3, HaveReadModuleMapInt3)
        ),
        !HaveReadModuleMaps ^ hrmm_int3 := HaveReadModuleMapInt3
    ).

%-----------------------------------------------------------------------------%

find_read_module_src(HaveReadModuleMapSrc, ModuleName, ReturnTimestamp,
        FileName, MaybeTimestamp, ParseTreeSrc, Specs, Errors) :-
    map.search(HaveReadModuleMapSrc, ModuleName, HaveReadModule),
    HaveReadModule = have_successfully_read_module(FileName, MaybeTimestamp0,
        ParseTreeSrc, Specs, Errors),
    return_timestamp_if_needed(ReturnTimestamp, MaybeTimestamp0,
        MaybeTimestamp).

find_read_module_some_int(HaveReadModuleMaps, ModuleName, IntFileKind,
        ReturnTimestamp, FileName, MaybeTimestamp,
        ParseTreeSomeInt, Specs, Errors) :-
    (
        IntFileKind = ifk_int0,
        find_read_module_int0(HaveReadModuleMaps ^ hrmm_int0, ModuleName,
            ReturnTimestamp, FileName, MaybeTimestamp,
            ParseTreeInt0, Specs, Errors),
        ParseTreeSomeInt = parse_tree_some_int0(ParseTreeInt0)
    ;
        IntFileKind = ifk_int1,
        find_read_module_int1(HaveReadModuleMaps ^ hrmm_int1, ModuleName,
            ReturnTimestamp, FileName, MaybeTimestamp,
            ParseTreeInt1, Specs, Errors),
        ParseTreeSomeInt = parse_tree_some_int1(ParseTreeInt1)
    ;
        IntFileKind = ifk_int2,
        find_read_module_int2(HaveReadModuleMaps ^ hrmm_int2, ModuleName,
            ReturnTimestamp, FileName, MaybeTimestamp,
            ParseTreeInt2, Specs, Errors),
        ParseTreeSomeInt = parse_tree_some_int2(ParseTreeInt2)
    ;
        IntFileKind = ifk_int3,
        find_read_module_int3(HaveReadModuleMaps ^ hrmm_int3, ModuleName,
            ReturnTimestamp, FileName, MaybeTimestamp,
            ParseTreeInt3, Specs, Errors),
        ParseTreeSomeInt = parse_tree_some_int3(ParseTreeInt3)
    ).

find_read_module_int0(HaveReadModuleMapInt0, ModuleName,
        ReturnTimestamp, FileName, MaybeTimestamp,
        ParseTreeInt0, Specs, Errors) :-
    map.search(HaveReadModuleMapInt0, ModuleName, HaveReadModule),
    HaveReadModule = have_successfully_read_module(FileName, MaybeTimestamp0,
        ParseTreeInt0, Specs, Errors),
    return_timestamp_if_needed(ReturnTimestamp, MaybeTimestamp0,
        MaybeTimestamp).

find_read_module_int1(HaveReadModuleMapInt1, ModuleName,
        ReturnTimestamp, FileName, MaybeTimestamp,
        ParseTreeInt1, Specs, Errors) :-
    map.search(HaveReadModuleMapInt1, ModuleName, HaveReadModule),
    HaveReadModule = have_successfully_read_module(FileName, MaybeTimestamp0,
        ParseTreeInt1, Specs, Errors),
    return_timestamp_if_needed(ReturnTimestamp, MaybeTimestamp0,
        MaybeTimestamp).

find_read_module_int2(HaveReadModuleMapInt2, ModuleName,
        ReturnTimestamp, FileName, MaybeTimestamp,
        ParseTreeInt2, Specs, Errors) :-
    map.search(HaveReadModuleMapInt2, ModuleName, HaveReadModule),
    HaveReadModule = have_successfully_read_module(FileName, MaybeTimestamp0,
        ParseTreeInt2, Specs, Errors),
    return_timestamp_if_needed(ReturnTimestamp, MaybeTimestamp0,
        MaybeTimestamp).

find_read_module_int3(HaveReadModuleMapInt3, ModuleName,
        ReturnTimestamp, FileName, MaybeTimestamp,
        ParseTreeInt3, Specs, Errors) :-
    map.search(HaveReadModuleMapInt3, ModuleName, HaveReadModule),
    HaveReadModule = have_successfully_read_module(FileName, MaybeTimestamp0,
        ParseTreeInt3, Specs, Errors),
    return_timestamp_if_needed(ReturnTimestamp, MaybeTimestamp0,
        MaybeTimestamp).

%-----------------------------------------------------------------------------%

:- pred return_timestamp_if_needed(maybe_return_timestamp::in,
    maybe(timestamp)::in, maybe(timestamp)::out) is det.

return_timestamp_if_needed(ReturnTimestamp, MaybeTimestamp0, MaybeTimestamp) :-
    (
        ReturnTimestamp = do_return_timestamp,
        (
            MaybeTimestamp0 = no,
            % This can happen if
            %
            % - code that does not need a timestamp enters a parse tree
            %   into the have_read_module_map, and then later
            %
            % - code that does need a timestamp finds the parse tree there.
            %
            % We abort because I (zs) don't think this should happen:
            % the use of have_read_module_maps in module_imports.m never
            % needs timestamps, the smart recompilation modules always
            % need timestamps, but the have_read_module_maps they use
            % are completely separate (for now). If it turns out I am wrong,
            % or we *do* want these two subsystems to use the same
            % have_read_module_maps, then there are two obvious possibilities:
            % either *always* store the timestamp of a file we read in, or
            % get the timestamp from the OS the first time it is needed
            % (have_read_module_maps entries include the filename, so this
            % is possible). The first solution is simpler, the second can
            % execute fewer system calls.
            unexpected($pred, "do_return_timestamp but no timestamp")
        ;
            MaybeTimestamp0 = yes(_),
            MaybeTimestamp = MaybeTimestamp0
        )
    ;
        ReturnTimestamp = dont_return_timestamp,
        MaybeTimestamp = no
    ).

%-----------------------------------------------------------------------------%

:- pred check_timestamp_report_if_needed_and_missing(globals::in,
    file_name::in, maybe(io.res(timestamp))::in, maybe(timestamp)::out,
    io::di, io::uo) is det.

check_timestamp_report_if_needed_and_missing(Globals, FileName,
        MaybeTimestampRes, MaybeTimestamp, !IO) :-
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
            record_and_report_missing_timestamp(Globals, FileName,
                IOError, !IO)
        ;
            SmartRecompilation = no
        )
    ;
        MaybeTimestampRes = no,
        MaybeTimestamp = no
    ).

:- pred record_and_report_missing_timestamp(globals::in, file_name::in,
    io.error::in, io::di, io::uo) is det.

record_and_report_missing_timestamp(Globals, FileName, Error, !IO) :-
    io_set_disable_smart_recompilation(disable_smart_recompilation, !IO),
    io_set_disable_generate_item_version_numbers(disable_item_version_numbers,
        !IO),
    globals.lookup_bool_option(Globals, warn_smart_recompilation, Warn),
    (
        Warn = yes,
        io.error_message(Error, Msg),
        io.format("Warning: cannot find modification time for %s:\n",
            [s(FileName)], !IO),
        io.format("  %s.\n", [s(Msg)], !IO),
        io.format("  Smart recompilation will not work.\n", [], !IO),
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
