%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2009, 2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: read_modules.m.
%
%---------------------------------------------------------------------------%

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

%---------------------------------------------------------------------------%
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

                hrmm_module_src         :: have_read_module_msrc_map,

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

:- type have_read_module_key(Kind)
    --->    have_read_module_key(module_name, Kind).

:- type have_read_module_msrc_map ==
    map(module_name, parse_tree_module_src).

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

%---------------------------------------------------------------------------%

:- type maybe_ignore_errors
    --->    ignore_errors
    ;       do_not_ignore_errors.

    % Why are we attempting to read and parse a file?
:- type read_reason_msg
    --->    rrm_file
            % Because the compiler has been asked to compile the named file
            % in some fashion. (This could be generating target code for it,
            % making its interface files, making its optimization files, etc.)
    ;       rrm_std(module_name)
            % Because the named file belongs to this module (as a source file,
            % interface file, or optimization file), and the compiler has been
            % asked to compile that module in some fashion.
    ;       rrm_old(module_name)
            % Because the named file belongs to this module, and we need
            % the old contents of the file, so that we can write out
            % what we *now* think its contents should be, but *only*
            % if the "new" contents differ from the old. The difference
            % between whether (a) we keep the old contents as is, or
            % (b) we overwrite them with identical contents, is the
            % timestamp on the file. Keeping the old timestamp will
            % prevent unnecessary rebuilds of other files that depend
            % on that one.
    ;       rrm_get_deps(module_name).
            % Because the named file belongs to this module, and we need
            % to get its dependencies.

    % read_module_src(Globals, ReadReasonMsg, IgnoreErrors, Search,
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
:- pred read_module_src(globals::in, read_reason_msg::in,
    maybe_ignore_errors::in, maybe_search::in,
    module_name::in, list(term.context)::in, file_name::out,
    read_module_and_timestamps::in, maybe(timestamp)::out,
    parse_tree_src::out, list(error_spec)::out, read_module_errors::out,
    io::di, io::uo) is det.

    % read_module_src_from_file(Globals, FileName, FileNameDotM,
    %   ReadReasonMsg, Search, ReadModuleAndTimestamps, MaybeTimestamp,
    %   ParseTreeSrc, Specs, Errors, !IO):
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
    read_reason_msg::in, maybe_search::in, read_module_and_timestamps::in,
    maybe(timestamp)::out, parse_tree_src::out,
    list(error_spec)::out, read_module_errors::out, io::di, io::uo) is det.

    % read_module_some_int(Globals, ReadReasonMsg, IgnoreErrors, Search,
    %   ModuleName, IntFileKind, FileName, ReturnTimestamp, MaybeTimestamp,
    %   ParseTreeInt, Specs, Errors, !IO):
    %
    % Given a module name, and the identity of one of its interface files,
    % (.int0, .int, .int2 or .int3), read in and parse the contents of that
    % interface file, printing progress messages along the way if the
    % verbosity level calls for that.
    %
    % XXX CLEANUP Calls to this predicate should be replaced by calls
    % to its int_file_kind-specific versions below. The only such calls
    % are now used in the implementation of smart recompilation.
    % XXX CLEANUP This predicate should be moved to recompilation.check.m.
    %
:- pred read_module_some_int(globals::in, read_reason_msg::in,
    maybe_ignore_errors::in, maybe_search::in,
    module_name::in, int_file_kind::in, file_name::out,
    read_module_and_timestamps::in, maybe(timestamp)::out,
    parse_tree_some_int::out, list(error_spec)::out, read_module_errors::out,
    io::di, io::uo) is det.

    % read_module_intN(Globals, ReadReasonMsg, IgnoreErrors, Search,
    %   ModuleName, FileName, ReadModuleAndTimestamps, MaybeTimestamp,
    %   ParseTreeIntN, Specs, Errors, !IO):
    %
    % Given a module name, read in and parse the specified kind of interface
    % file for that module, printing progress messages along the way
    % if the verbosity level calls for that.
    %
    % The meanings of the arguments are pretty much the same as for
    % read_module_src, but while the names of the files that contain source
    % files may not be fully module qualified, the names of interface files
    % are always fully module qualified, so read_module_int does not search
    % for the right filename. It knows what filename it looks for; the only
    % search it does, if Search is do_search, is to decide which directory
    % among the search directories contains the file with that filename.
    %
:- pred read_module_int0(globals::in, read_reason_msg::in,
    maybe_ignore_errors::in, maybe_search::in,
    module_name::in, file_name::out,
    read_module_and_timestamps::in, maybe(timestamp)::out,
    parse_tree_int0::out, list(error_spec)::out, read_module_errors::out,
    io::di, io::uo) is det.
:- pred read_module_int1(globals::in, read_reason_msg::in,
    maybe_ignore_errors::in, maybe_search::in,
    module_name::in, file_name::out,
    read_module_and_timestamps::in, maybe(timestamp)::out,
    parse_tree_int1::out, list(error_spec)::out, read_module_errors::out,
    io::di, io::uo) is det.
:- pred read_module_int2(globals::in, read_reason_msg::in,
    maybe_ignore_errors::in, maybe_search::in,
    module_name::in, file_name::out,
    read_module_and_timestamps::in, maybe(timestamp)::out,
    parse_tree_int2::out, list(error_spec)::out, read_module_errors::out,
    io::di, io::uo) is det.
:- pred read_module_int3(globals::in, read_reason_msg::in,
    maybe_ignore_errors::in, maybe_search::in,
    module_name::in, file_name::out,
    read_module_and_timestamps::in, maybe(timestamp)::out,
    parse_tree_int3::out, list(error_spec)::out, read_module_errors::out,
    io::di, io::uo) is det.

    % read_module_{plain,trans}_opt(Globals, ModuleName, FileName,
    %   ParseTreeOpt, Specs, Errors, !IO):
    %
    % Given a module name, read in and parse the specified kind of optimization
    % file for that module, printing progress messages along the way
    % if the verbosity level calls for that.
    %
    % These predicates do a very similar job to read_module_intN,
    % but there are several differences.
    %
    % - We do not do anything with errors in .opt and .trans_opt files;
    %   we leave that up to the caller. The reason for this is that
    %   a missing .opt file is not an error, unlike a missing .intN file.
    %
    % - We do always do search for .opt and .trans_opt files, but
    %   in the directories named by the intermod_directories option, *not*
    %   in the directories named by the search_directories option.
    %
    % - None of our callers are ever interested in timestamps,
    %   so these predicates never return them.
    %
:- pred read_module_plain_opt(globals::in,
    module_name::in, file_name::out, parse_tree_plain_opt::out,
    list(error_spec)::out, read_module_errors::out, io::di, io::uo) is det.
:- pred read_module_trans_opt(globals::in,
    module_name::in, file_name::out, parse_tree_trans_opt::out,
    list(error_spec)::out, read_module_errors::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

    % maybe_read_module_intN(Globals, ReadReasonMsg, Search,
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

%---------------------------------------------------------------------------%

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

    % XXX CLEANUP This predicate should be moves to recompilation.check.m.
    %
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

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

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

%---------------------------------------------------------------------------%

init_have_read_module_maps = 
    have_read_module_maps(map.init, map.init, map.init,
        map.init, map.init, map.init, map.init, map.init).

%---------------------------------------------------------------------------%

read_module_src(Globals, ReadReasonMsg, IgnoreErrors, Search,
        ModuleName, ExpectationContexts, FileName,
        ReadModuleAndTimestamps, MaybeTimestamp,
        ParseTreeSrc, Specs, Errors, !IO) :-
    read_module_begin(Globals, ReadReasonMsg, Search, ModuleName, fk_src,
        FileName0, ReadDoneMsg, SearchDirs, !IO),
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
    read_module_end_module(Globals, IgnoreErrors, fk_src,
        ReadDoneMsg, MaybeFileNameAndStream, FileName0, FileName,
        MaybeTimestampRes, MaybeTimestamp, ModuleSpecs, Specs, Errors, !IO).

read_module_src_from_file(Globals, FileName, FileNameDotM, ReadReasonMsg,
        Search, ReadModuleAndTimestamps, MaybeTimestamp,
        ParseTreeSrc, Specs, Errors, !IO) :-
    read_module_begin_from_file(Globals, ReadReasonMsg, Search,
        FileName, FileNameDotM, DefaultModuleName,
        ReadDoneMsg, SearchDirs, !IO),
    search_for_file_and_stream(SearchDirs, FileNameDotM,
        MaybeFileNameAndStream, !IO),
    actually_read_module_src(Globals, DefaultModuleName, [],
        MaybeFileNameAndStream, ReadModuleAndTimestamps, MaybeTimestampRes,
        ParseTreeSrc, ModuleSpecs, Errors, !IO),
    read_module_end_file(Globals, fk_src, ReadDoneMsg, FileNameDotM,
        MaybeTimestampRes, MaybeTimestamp, ModuleSpecs, Specs, Errors, !IO).

read_module_some_int(Globals, ReadReasonMsg, IgnoreErrors, Search, ModuleName,
        IntFileKind, FileName, ReadModuleAndTimestamps, MaybeTimestamp,
        ParseTreeSomeInt, Specs, Errors, !IO) :-
    (
        IntFileKind = ifk_int0,
        read_module_int0(Globals, ReadReasonMsg, IgnoreErrors, Search,
            ModuleName, FileName, ReadModuleAndTimestamps, MaybeTimestamp,
            ParseTreeInt0, Specs, Errors, !IO),
        ParseTreeSomeInt = parse_tree_some_int0(ParseTreeInt0)
    ;
        IntFileKind = ifk_int1,
        read_module_int1(Globals, ReadReasonMsg, IgnoreErrors, Search,
            ModuleName, FileName, ReadModuleAndTimestamps, MaybeTimestamp,
            ParseTreeInt1, Specs, Errors, !IO),
        ParseTreeSomeInt = parse_tree_some_int1(ParseTreeInt1)
    ;
        IntFileKind = ifk_int2,
        read_module_int2(Globals, ReadReasonMsg, IgnoreErrors, Search,
            ModuleName, FileName, ReadModuleAndTimestamps, MaybeTimestamp,
            ParseTreeInt2, Specs, Errors, !IO),
        ParseTreeSomeInt = parse_tree_some_int2(ParseTreeInt2)
    ;
        IntFileKind = ifk_int3,
        read_module_int3(Globals, ReadReasonMsg, IgnoreErrors, Search,
            ModuleName, FileName, ReadModuleAndTimestamps, MaybeTimestamp,
            ParseTreeInt3, Specs, Errors, !IO),
        ParseTreeSomeInt = parse_tree_some_int3(ParseTreeInt3)
    ).

read_module_int0(Globals, ReadReasonMsg, IgnoreErrors, Search, ModuleName,
        FileName, ReadModuleAndTimestamps, MaybeTimestamp,
        ParseTreeInt0, Specs, Errors, !IO) :-
    read_module_begin(Globals, ReadReasonMsg, Search, ModuleName,
        fk_int(ifk_int0), FileName0, ReadDoneMsg, SearchDirs, !IO),
    search_for_file_and_stream(SearchDirs, FileName0,
        MaybeFileNameAndStream, !IO),
    actually_read_module_int0(Globals, ModuleName, [],
        MaybeFileNameAndStream, ReadModuleAndTimestamps, MaybeTimestampRes,
        ParseTreeInt0, ModuleSpecs, Errors, !IO),
    read_module_end_module(Globals, IgnoreErrors, fk_int(ifk_int3),
        ReadDoneMsg, MaybeFileNameAndStream, FileName0, FileName,
        MaybeTimestampRes, MaybeTimestamp, ModuleSpecs, Specs, Errors, !IO).

read_module_int1(Globals, ReadReasonMsg, IgnoreErrors, Search, ModuleName,
        FileName, ReadModuleAndTimestamps, MaybeTimestamp,
        ParseTreeInt1, Specs, Errors, !IO) :-
    read_module_begin(Globals, ReadReasonMsg, Search, ModuleName,
        fk_int(ifk_int1), FileName0, ReadDoneMsg, SearchDirs, !IO),
    search_for_file_and_stream(SearchDirs, FileName0,
        MaybeFileNameAndStream, !IO),
    actually_read_module_int1(Globals, ModuleName, [],
        MaybeFileNameAndStream, ReadModuleAndTimestamps, MaybeTimestampRes,
        ParseTreeInt1, ModuleSpecs, Errors, !IO),
    read_module_end_module(Globals, IgnoreErrors, fk_int(ifk_int3),
        ReadDoneMsg, MaybeFileNameAndStream, FileName0, FileName,
        MaybeTimestampRes, MaybeTimestamp, ModuleSpecs, Specs, Errors, !IO).

read_module_int2(Globals, ReadReasonMsg, IgnoreErrors, Search, ModuleName,
        FileName, ReadModuleAndTimestamps, MaybeTimestamp,
        ParseTreeInt2, Specs, Errors, !IO) :-
    read_module_begin(Globals, ReadReasonMsg, Search, ModuleName,
        fk_int(ifk_int2), FileName0, ReadDoneMsg, SearchDirs, !IO),
    search_for_file_and_stream(SearchDirs, FileName0,
        MaybeFileNameAndStream, !IO),
    actually_read_module_int2(Globals, ModuleName, [],
        MaybeFileNameAndStream, ReadModuleAndTimestamps, MaybeTimestampRes,
        ParseTreeInt2, ModuleSpecs, Errors, !IO),
    read_module_end_module(Globals, IgnoreErrors, fk_int(ifk_int3),
        ReadDoneMsg, MaybeFileNameAndStream, FileName0, FileName,
        MaybeTimestampRes, MaybeTimestamp, ModuleSpecs, Specs, Errors, !IO).

read_module_int3(Globals, ReadReasonMsg, IgnoreErrors, Search, ModuleName,
        FileName, ReadModuleAndTimestamps, MaybeTimestamp,
        ParseTreeInt3, Specs, Errors, !IO) :-
    read_module_begin(Globals, ReadReasonMsg, Search, ModuleName,
        fk_int(ifk_int3), FileName0, ReadDoneMsg, SearchDirs, !IO),
    search_for_file_and_stream(SearchDirs, FileName0,
        MaybeFileNameAndStream, !IO),
    actually_read_module_int3(Globals, ModuleName, [],
        MaybeFileNameAndStream, ReadModuleAndTimestamps, MaybeTimestampRes,
        ParseTreeInt3, ModuleSpecs, Errors, !IO),
    read_module_end_module(Globals, IgnoreErrors, fk_int(ifk_int3),
        ReadDoneMsg, MaybeFileNameAndStream, FileName0, FileName,
        MaybeTimestampRes, MaybeTimestamp, ModuleSpecs, Specs, Errors, !IO).

read_module_plain_opt(Globals, ModuleName, FileName, ParseTreePlainOpt,
        Specs, Errors, !IO) :-
    ReadReasonMsg = rrm_std(ModuleName),
    Search = do_search,
    IgnoreErrors = do_not_ignore_errors,
    MaybeTimestampRes = no,

    read_module_begin(Globals, ReadReasonMsg, Search, ModuleName,
        fk_opt(ofk_opt), FileName0, ReadDoneMsg, SearchDirs, !IO),
    search_for_file_and_stream(SearchDirs, FileName0,
        MaybeFileNameAndStream, !IO),
    actually_read_module_plain_opt(Globals, ModuleName, MaybeFileNameAndStream,
        ParseTreePlainOpt, ModuleSpecs, Errors, !IO),
    read_module_end_module(Globals, IgnoreErrors, fk_opt(ofk_opt),
        ReadDoneMsg, MaybeFileNameAndStream, FileName0, FileName,
        MaybeTimestampRes, _MaybeTimestamp, ModuleSpecs, Specs, Errors, !IO).

read_module_trans_opt(Globals, ModuleName, FileName, ParseTreeTransOpt,
        Specs, Errors, !IO) :-
    ReadReasonMsg = rrm_std(ModuleName),
    Search = do_search,
    IgnoreErrors = do_not_ignore_errors,
    MaybeTimestampRes = no,

    read_module_begin(Globals, ReadReasonMsg, Search, ModuleName,
        fk_opt(ofk_trans_opt), FileName0, ReadDoneMsg, SearchDirs, !IO),
    search_for_file_and_stream(SearchDirs, FileName0,
        MaybeFileNameAndStream, !IO),
    actually_read_module_trans_opt(Globals, ModuleName, MaybeFileNameAndStream,
        ParseTreeTransOpt, ModuleSpecs, Errors, !IO),
    read_module_end_module(Globals, IgnoreErrors, fk_opt(ofk_trans_opt),
        ReadDoneMsg, MaybeFileNameAndStream, FileName0, FileName,
        MaybeTimestampRes, _MaybeTimestamp, ModuleSpecs, Specs, Errors, !IO).

%---------------------------------------------------------------------------%

:- pred read_module_begin_from_file(globals::in, read_reason_msg::in,
    maybe_search::in, file_name::in, file_name::in, module_name::out,
    read_done_msg::out, list(string)::out, io::di, io::uo) is det.

read_module_begin_from_file(Globals, ReadReasonMsg, Search,
        FileName, FileNameDotM, DefaultModuleName, ReadDoneMsg,
        SearchDirs, !IO) :-
    % XXX Do not assume that the name of FileNameDotM guarantees
    % that the string it holds ends in ".m".
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
    output_read_reason_msg(Globals, ReadReasonMsg, FileNameDotM,
        ReadDoneMsg, !IO).

:- pred read_module_begin(globals::in, read_reason_msg::in,
    maybe_search::in, module_name::in, file_kind::in, file_name::out,
    read_done_msg::out, list(string)::out, io::di, io::uo) is det.

read_module_begin(Globals, ReadReasonMsg, Search, ModuleName, FileKind,
        FileName, ReadDoneMsg, SearchDirs, !IO) :-
    file_kind_to_extension(FileKind, _ExtStr, Ext),
    % The rest of this predicate should be kept in sync
    % with read_module_begin_from_file.
    (
        Search = do_search,
        % XXX CLEANUP We should either pass SearchDirs to
        % module_name_to_search_file_name, or get it to give SearchDirs to us.
        module_name_to_search_file_name(Globals, $pred, Ext,
            ModuleName, FileName, !IO),
        (
            ( FileKind = fk_src
            ; FileKind = fk_int(_)
            ),
            globals.lookup_accumulating_option(Globals, search_directories,
                SearchDirs)
        ;
            FileKind = fk_opt(_),
            globals.lookup_accumulating_option(Globals, intermod_directories,
                SearchDirs)
        )
    ;
        Search = do_not_search,
        module_name_to_file_name(Globals, $pred, do_not_create_dirs, Ext,
            ModuleName, FileName, !IO),
        SearchDirs = [dir.this_directory]
    ),
    output_read_reason_msg(Globals, ReadReasonMsg, FileName, ReadDoneMsg, !IO).

:- pred read_module_end_module(globals::in, maybe_ignore_errors::in,
    file_kind::in, read_done_msg::in, maybe_error(path_name_and_stream)::in,
    file_name::in, file_name::out,
    maybe(io.res(timestamp))::in, maybe(timestamp)::out,
    list(error_spec)::in, list(error_spec)::out, read_module_errors::in,
    io::di, io::uo) is det.

read_module_end_module(Globals, IgnoreErrors, FileKind, ReadDoneMsg,
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
            output_read_done_msg(ReadDoneMsg, "not found.\n", !IO),
            Specs = []
        else
            % XXX CLEANUP we should not print "done" if Errors is nonempty.
            output_read_done_msg(ReadDoneMsg, "done.\n", !IO),
            Specs = ModuleSpecs
        )
    ;
        IgnoreErrors = do_not_ignore_errors,
        handle_any_read_module_errors(Globals, FileKind, ReadDoneMsg, Errors,
            ModuleSpecs, Specs, !IO)
    ),
    globals.lookup_bool_option(Globals, detailed_statistics, Statistics),
    maybe_report_stats(Statistics, !IO).

:- pred read_module_end_file(globals::in, file_kind::in, read_done_msg::in,
    file_name::in, maybe(io.res(timestamp))::in, maybe(timestamp)::out,
    list(error_spec)::in, list(error_spec)::out, read_module_errors::in,
    io::di, io::uo) is det.

read_module_end_file(Globals, FileKind, ReadDoneMsg, FileName,
        MaybeTimestampRes, MaybeTimestamp,
        ModuleSpecs, Specs, Errors, !IO) :-
    % The code of read_module_end_module and read_module_end_file
    % should be kept in sync.
    %
    % Unlike read_module_end_module, we assume do_not_ignore_errors.
    check_timestamp_report_if_needed_and_missing(Globals, FileName,
        MaybeTimestampRes, MaybeTimestamp, !IO),
    handle_any_read_module_errors(Globals, FileKind, ReadDoneMsg, Errors,
        ModuleSpecs, Specs, !IO).

:- pred handle_any_read_module_errors(globals::in, file_kind::in,
    read_done_msg::in, read_module_errors::in,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

handle_any_read_module_errors(Globals, FileKind, ReadDoneMsg,
        Errors, !Specs, !IO) :-
    ( if set.is_empty(Errors) then
        % XXX CLEANUP This should just print "done".
        output_read_done_msg(ReadDoneMsg, "successful parse.\n", !IO)
    else
        set.intersect(Errors, fatal_read_module_errors, FatalErrors),
        ( if set.contains(Errors, rme_could_not_open_file) then
            output_read_done_msg(ReadDoneMsg, "not found.\n", !IO)
        else if set.is_non_empty(FatalErrors) then
            output_read_done_msg(ReadDoneMsg, "fatal error(s).\n", !IO)
        else
            output_read_done_msg(ReadDoneMsg, "parse error(s).\n", !IO)
        ),
        (
            FileKind = fk_opt(_)
        ;
            ( FileKind = fk_src
            ; FileKind = fk_int(_)
            ),
            (
                ReadDoneMsg = rdm_none,
                WriteOutErrors = no
            ;
                ( ReadDoneMsg = rdm_current
                ; ReadDoneMsg = rdm_progress(_)
                ),
                WriteOutErrors = yes
            ),
            pre_hlds_maybe_write_out_errors(WriteOutErrors, Globals,
                !Specs, !IO),
            io.set_exit_status(1, !IO)
        )
    ).

%---------------------%

:- type read_done_msg
    --->    rdm_none
            % Do not print a "done" message.
    ;       rdm_current
            % Print a "done" message to the current output stream.
    ;       rdm_progress(io.text_output_stream).
            % Print a "done" message to the specified progress output stream.

:- pred output_read_reason_msg(globals::in, read_reason_msg::in, string::in,
    read_done_msg::out, io::di, io::uo) is det.

output_read_reason_msg(Globals, ReadReasonMsg, FileName, ReadDoneMsg, !IO) :-
    globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
    (
        VeryVerbose = no,
        ReadDoneMsg = rdm_none
    ;
        VeryVerbose = yes,
        (
            ReadReasonMsg = rrm_file,
            string.format("%% Reading `%s'... ", [s(FileName)], Msg),
            % We should be directing the output to the progress stream
            % of the module we are reading, but we don't know its name yet.
            % XXX Now that we require every module to be stored in a file
            % whose name *either* directly corresponds to the name of the
            % module, *or* is stored in Mercury.modules, this is not true.
            io.output_stream(Stream, !IO),
            ReadDoneMsg = rdm_current
        ;
            (
                ReadReasonMsg = rrm_std(ModuleName),
                string.format("%% Reading `%s'... ",
                    [s(FileName)], Msg)
            ;
                ReadReasonMsg = rrm_old(ModuleName),
                string.format("%% Reading old version of `%s'... ",
                    [s(FileName)], Msg)
            ;
                ReadReasonMsg = rrm_get_deps(ModuleName),
                string.format("%% Getting dependencies for `%s'... ",
                    [s(FileName)], Msg)
            ),
            get_progress_output_stream(Globals, ModuleName, Stream, !IO),
            ReadDoneMsg = rdm_progress(Stream)
        ),
        io.write_string(Stream, Msg, !IO),
        io.flush_output(Stream, !IO)
    ).

:- pred output_read_done_msg(read_done_msg::in, string::in,
    io::di, io::uo) is det.

output_read_done_msg(ReadDoneMsg, Msg, !IO) :-
    (
        ReadDoneMsg = rdm_none
    ;
        (
            ReadDoneMsg = rdm_current,
            io.output_stream(Stream, !IO)
        ;
            ReadDoneMsg = rdm_progress(Stream)
        ),
        io.write_string(Stream, Msg, !IO),
        io.flush_output(Stream, !IO)
    ).

%---------------------------------------------------------------------------%

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
        read_module_int0(Globals, rrm_std(ModuleName),
            do_not_ignore_errors, Search, ModuleName, FileName,
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
        read_module_int1(Globals, rrm_std(ModuleName),
            do_not_ignore_errors, Search, ModuleName, FileName,
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
        read_module_int2(Globals, rrm_std(ModuleName),
            do_not_ignore_errors, Search, ModuleName, FileName,
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
        read_module_int3(Globals, rrm_std(ModuleName),
            do_not_ignore_errors, Search, ModuleName, FileName,
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

%---------------------------------------------------------------------------%

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

%---------------------------------------------------------------------------%

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

%---------------------------------------------------------------------------%

:- pred check_timestamp_report_if_needed_and_missing(globals::in,
    file_name::in, maybe(io.res(timestamp))::in, maybe(timestamp)::out,
    io::di, io::uo) is det.

check_timestamp_report_if_needed_and_missing(Globals, FileName,
        MaybeTimestampRes, MaybeTimestamp, !IO) :-
    (
        MaybeTimestampRes = yes(TimestampRes),
        (
            TimestampRes = ok(Timestamp),
            MaybeTimestamp = yes(Timestamp)
        ;
            TimestampRes = error(IOError),
            MaybeTimestamp = no,
            globals.lookup_bool_option(Globals, smart_recompilation,
                SmartRecompilation),
            % Should we print the warning if smart recompilation has
            % already been disabled by an earlier error? At the moment, we do.
            (
                SmartRecompilation = yes,
                record_and_report_missing_timestamp(Globals, FileName,
                    IOError, !IO)
            ;
                SmartRecompilation = no
            )
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
        io.error_message(Error, ErrorMsg),
        Pieces = [words("Warning: cannot find modification time for"),
            quote(FileName), suffix(":"), nl,
            words(ErrorMsg), suffix("."), nl,
            words("Smart recompilation will not work."), nl],
        Spec = simplest_no_context_spec($pred, severity_warning,
            phase_read_files, Pieces),
        write_error_spec(Globals, Spec, !IO)
    ;
        Warn = no
    ).

%---------------------------------------------------------------------------%
:- end_module parse_tree.read_modules.
%---------------------------------------------------------------------------%
