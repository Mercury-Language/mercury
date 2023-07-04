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
    have_read_module_map(parse_tree_src).

:- type have_read_module_key(Kind)
    --->    have_read_module_key(module_name, Kind).

:- type have_read_module_msrc_map ==
    map(module_name, parse_tree_module_src).

:- type have_read_module_int0_map ==
    have_read_module_map(parse_tree_int0).
:- type have_read_module_int1_map ==
    have_read_module_map(parse_tree_int1).
:- type have_read_module_int2_map ==
    have_read_module_map(parse_tree_int2).
:- type have_read_module_int3_map ==
    have_read_module_map(parse_tree_int3).

:- type have_read_module_plain_opt_map ==
    have_read_module_map(parse_tree_plain_opt).
:- type have_read_module_trans_opt_map ==
    have_read_module_map(parse_tree_trans_opt).

:- type have_read_module_map(PT) == map(module_name, have_read_module(PT)).

    % This type records the result of one of the read_module_<filekind>
    % predicates below. There are three possible outcomes:
    %
    % - We have tried to read the module and succeeded.
    % - We have tried to read the module and failed.
    % - We have not tried to read the module.
    %
    % The first will return have_read_module(...), while the second and
    % third will return have_not_read_module(...).
    %
    % The reason for not having a separate function symbol for the third
    % outcome is that it can happen only for the small minority of calls
    % that ask that the file not be read in the event of a timestamp match,
    % but a third function symbol would force *all* calls to handle a situation
    % that cannot arise for them.
:- type have_read_module(PT)
    --->    have_read_module(
                % We have read the module.

                % The name of the file that we have read.
                file_name,

                % The timestamp of the file, if the caller requested
                % that it be returned. Not all do.
                maybe(timestamp),

                % The parse tree of the file we have read.
                PT,

                % Any errors we found during parsing. It should contain
                % neither frme_could_not_open_file or frme_could_not_read_file,
                % but may contain any other errors.
                read_module_errors
            )
    ;       have_not_read_module(
                % We have not read the module.

                % The name of the file that we have either tried to read,
                % or whose timestamp indicated we did not have to read it.
                file_name,

                % The errors we got in the attempt to read the file.
                % If we did not attempt to read the file, this will contain
                % no errors at all. (This is how you can tell these two cases
                % apart.)
                read_module_errors
            ).

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

    % read_module_src(MaybeProgressStream, Globals, ReadReasonMsg,
    %   IgnoreErrors, Search, ModuleName, FileName, ReadModuleAndTimestamps,
    %   HaveReadModule, !IO):
    %
    % Given a module name, read in and parse the source code of that file,
    % printing progress messages along the way if the verbosity level
    % calls for that.
    %
    % If Search is do_search, search all directories given by the option
    % search_directories for the module;
    % otherwise, search for those filenames only in the current directory.
    % Return in the FileName part of HaveReadModule the actual source file
    % name found (excluding the directory part). If the actual module name
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
    % return `no' in the MaybeTimestamp field of HaveReadModule.
    %
    % If ReadModuleAndTimestamps is always_read_module(do_return_timestamp),
    % attempt to return the modification time of the file in the MaybeTimestamp
    % field of HaveReadModule.
    %
    % If ReadModuleAndTimestamps is dont_read_module_if_match(OldTimeStamp),
    % then
    %
    % - if the timestamp of that file is exactly OldTimestamp, then
    %   don't read the file, and return have_not_read_module with no errors,
    %   while
    % - if the timestamp of that file differs from OldTimestamp (virtually
    %   always because it is newer), then read the module from the file
    %   as usual, parse and return its contents as usual, and also return
    %   its actual timestamp.
    %
:- pred read_module_src(maybe(io.text_output_stream)::in, globals::in,
    read_reason_msg::in, maybe_ignore_errors::in, maybe_search::in,
    module_name::in, list(term.context)::in, read_module_and_timestamps::in,
    have_read_module(parse_tree_src)::out, io::di, io::uo) is det.

    % read_module_src_from_file(Globals, FileName, FileNameDotM,
    %   ReadReasonMsg, Search, ReadModuleAndTimestamps, HaveReadModule, !IO):
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
    have_read_module(parse_tree_src)::out, io::di, io::uo) is det.

    % read_module_intN(MaybeProgressStream, Globals, ReadReasonMsg,
    %   IgnoreErrors, Search, ModuleName, FileName, ReadModuleAndTimestamps,
    %   HaveReadModule, !IO):
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
:- pred read_module_int0(maybe(io.text_output_stream)::in, globals::in,
    read_reason_msg::in, maybe_ignore_errors::in, maybe_search::in,
    module_name::in, read_module_and_timestamps::in,
    have_read_module(parse_tree_int0)::out, io::di, io::uo) is det.
:- pred read_module_int1(maybe(io.text_output_stream)::in, globals::in,
    read_reason_msg::in, maybe_ignore_errors::in, maybe_search::in,
    module_name::in, read_module_and_timestamps::in,
    have_read_module(parse_tree_int1)::out, io::di, io::uo) is det.
:- pred read_module_int2(maybe(io.text_output_stream)::in, globals::in,
    read_reason_msg::in, maybe_ignore_errors::in, maybe_search::in,
    module_name::in, read_module_and_timestamps::in,
    have_read_module(parse_tree_int2)::out, io::di, io::uo) is det.
:- pred read_module_int3(maybe(io.text_output_stream)::in, globals::in,
    read_reason_msg::in, maybe_ignore_errors::in, maybe_search::in,
    module_name::in, read_module_and_timestamps::in,
    have_read_module(parse_tree_int3)::out, io::di, io::uo) is det.

:- pred read_module_int0_no_stream(globals::in,
    read_reason_msg::in, maybe_ignore_errors::in, maybe_search::in,
    module_name::in, read_module_and_timestamps::in,
    have_read_module(parse_tree_int0)::out, io::di, io::uo) is det.
:- pred read_module_int1_no_stream(globals::in,
    read_reason_msg::in, maybe_ignore_errors::in, maybe_search::in,
    module_name::in, read_module_and_timestamps::in,
    have_read_module(parse_tree_int1)::out, io::di, io::uo) is det.
:- pred read_module_int2_no_stream(globals::in,
    read_reason_msg::in, maybe_ignore_errors::in, maybe_search::in,
    module_name::in, read_module_and_timestamps::in,
    have_read_module(parse_tree_int2)::out, io::di, io::uo) is det.
:- pred read_module_int3_no_stream(globals::in,
    read_reason_msg::in, maybe_ignore_errors::in, maybe_search::in,
    module_name::in, read_module_and_timestamps::in,
    have_read_module(parse_tree_int3)::out, io::di, io::uo) is det.

    % read_module_{plain,trans}_opt(ProgressStream, Globals, ModuleName,
    %   FileName, HaveReadModule, !IO):
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
:- pred read_module_plain_opt(io.text_output_stream::in, globals::in,
    module_name::in, have_read_module(parse_tree_plain_opt)::out,
    io::di, io::uo) is det.
:- pred read_module_trans_opt(io.text_output_stream::in, globals::in,
    module_name::in, have_read_module(parse_tree_trans_opt)::out,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%

    % get_default_module_name_for_file(FileName, FileNameDotM,
    %   DefaultModuleName, !IO):
    %
    % Return the default module name for a file, based either on the
    % entry for FileNameDotM in the source file map (Mercury.modules)
    % if that is available, and otherwise based just on the file name FileName.
    % XXX Having two separate filenames here is old behavior. I (zs)
    % don't know the reason for it. It can't be a distinction between
    % expected and actual filename, since the caller is calling us
    % because it does not know the actual module name, and thus has
    % no basis on which to decide what filename to expect.
    %
:- pred get_default_module_name_for_file(file_name::in, file_name::in,
    module_name::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

    % maybe_read_module_intN(ProgressStream, Globals, ReadReasonMsg, Search,
    %   ModuleName, IntFileKind, ReturnTimestamp, HaveReadModule,
    %   !HaveReadModuleMaps, !IO):
    %
    % If HaveReadModuleMap contains the already-read contents of the
    % relevant kind of interface file for ModuleName, then return
    % the information stored in HaveReadModuleMap for that file.
    % If it is not there, read that interface file using read_module_intN,
    % regardless of its timestamp.
    %
:- pred maybe_read_module_int0(io.text_output_stream::in, globals::in,
    maybe_search::in, module_name::in, maybe_return_timestamp::in,
    have_read_module(parse_tree_int0)::out,
    have_read_module_maps::in, have_read_module_maps::out,
    io::di, io::uo) is det.
:- pred maybe_read_module_int1(io.text_output_stream::in, globals::in,
    maybe_search::in, module_name::in, maybe_return_timestamp::in,
    have_read_module(parse_tree_int1)::out,
    have_read_module_maps::in, have_read_module_maps::out,
    io::di, io::uo) is det.
:- pred maybe_read_module_int2(io.text_output_stream::in, globals::in,
    maybe_search::in, module_name::in, maybe_return_timestamp::in,
    have_read_module(parse_tree_int2)::out,
    have_read_module_maps::in, have_read_module_maps::out,
    io::di, io::uo) is det.
:- pred maybe_read_module_int3(io.text_output_stream::in, globals::in,
    maybe_search::in, module_name::in, maybe_return_timestamp::in,
    have_read_module(parse_tree_int3)::out,
    have_read_module_maps::in, have_read_module_maps::out,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- pred return_timestamp_if_needed(maybe_return_timestamp::in,
    maybe(timestamp)::in, maybe(timestamp)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.options.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.file_kind.
:- import_module parse_tree.find_module.
:- import_module parse_tree.parse_module.
:- import_module parse_tree.source_file_map.
:- import_module parse_tree.write_error_spec.

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

read_module_src(MaybeProgressStream, Globals, ReadReasonMsg, IgnoreErrors,
        Search, ModuleName, ExpectationContexts, ReadModuleAndTimestamps,
        HaveReadModule, !IO) :-
    read_module_begin(Globals, ReadReasonMsg, Search, ModuleName, fk_src,
        FileName0, ReadDoneMsg, SearchDirs, !IO),
    % For `.m' files, we need to deal with the case where the module name
    % does not match the file name.
    search_for_module_source_and_stream(SearchDirs, ModuleName,
        MaybeFileNameAndStream, !IO),
    (
        MaybeFileNameAndStream = ok(FileNameAndStream),
        actually_read_module_src(Globals, FileNameAndStream, ModuleName,
            ExpectationContexts, ReadModuleAndTimestamps, MaybeTimestampRes,
            MaybeParseTreeSrc0, Errors0, !IO),
        read_module_end_module(MaybeProgressStream, Globals,
            mfas_ok(FileNameAndStream), IgnoreErrors, fk_src, ReadDoneMsg,
            FileName0, FileName, MaybeTimestampRes, MaybeTimestamp,
            Errors0, Errors1, !IO),
        (
            MaybeParseTreeSrc0 = yes(ParseTreeSrc0),
            % If ModuleName = ParseTreeSrc0 ^ pts_module_name, this obviously
            % does the right thing.
            % If ModuleName != ParseTreeSrc0 ^ pts_module_name, then
            % we must include ModuleName in ParseTreeSrc (see the comment
            % above), and including recording its context as
            % ParseTreeSrc0 ^ pts_module_name_context shouldn't mislead anyone
            % who reads the error spec (which should be in Errors0) about
            % the unexpected module name.
            ParseTreeSrc = ParseTreeSrc0 ^ pts_module_name := ModuleName,
            HaveReadModule = have_read_module(FileName, MaybeTimestamp,
                ParseTreeSrc, Errors1)
        ;
            MaybeParseTreeSrc0 = no,
            Errors = no_file_errors(IgnoreErrors, Errors1),
            HaveReadModule = have_not_read_module(FileName, Errors)
        )
    ;
        MaybeFileNameAndStream = error(ErrorMsg),
        io_error_to_read_module_errors(ErrorMsg, frme_could_not_open_file,
            Errors0, !IO),
        read_module_end_module(MaybeProgressStream, Globals,
            mfas_error(Errors0), IgnoreErrors, fk_src, ReadDoneMsg,
            FileName0, FileName, no, _MaybeTimestamp, Errors0, Errors1, !IO),
        Errors = no_file_errors(IgnoreErrors, Errors1),
        HaveReadModule = have_not_read_module(FileName, Errors)
    ).

read_module_src_from_file(Globals, FileName, FileNameDotM, ReadReasonMsg,
        Search, ReadModuleAndTimestamps, HaveReadModule, !IO) :-
    read_module_begin_from_file(Globals, ReadReasonMsg, Search,
        FileName, FileNameDotM, DefaultModuleName,
        ReadDoneMsg, SearchDirs, !IO),
    search_for_file_and_stream(SearchDirs, FileNameDotM,
        MaybeFileNameAndStream, !IO),
    (
        MaybeFileNameAndStream = ok(FileNameAndStream),
        actually_read_module_src(Globals, FileNameAndStream,
            DefaultModuleName, [], ReadModuleAndTimestamps, MaybeTimestampRes,
            MaybeParseTreeSrc, Errors0, !IO),
        read_module_end_file(Globals, fk_src, ReadDoneMsg, FileNameDotM,
            MaybeTimestampRes, MaybeTimestamp, Errors0, Errors1, !IO),
        (
            MaybeParseTreeSrc = yes(ParseTreeSrc),
            HaveReadModule = have_read_module(FileNameDotM, MaybeTimestamp,
                ParseTreeSrc, Errors1)
        ;
            MaybeParseTreeSrc = no,
            HaveReadModule = have_not_read_module(FileNameDotM, Errors1)
        )
    ;
        MaybeFileNameAndStream = error(ErrorMsg),
        io_error_to_read_module_errors(ErrorMsg, frme_could_not_open_file,
            Errors0, !IO),
        read_module_end_file(Globals, fk_src, ReadDoneMsg, FileNameDotM,
            no, _MaybeTimestamp, Errors0, Errors, !IO),
        HaveReadModule = have_not_read_module(FileNameDotM, Errors)
    ).

%---------------------%

read_module_int0(ProgressStream, Globals, ReadReasonMsg, IgnoreErrors, Search,
        ModuleName, ReadModuleAndTimestamps, HaveReadModule, !IO) :-
    read_module_begin(Globals, ReadReasonMsg, Search, ModuleName,
        fk_int(ifk_int0), FileName0, ReadDoneMsg, SearchDirs, !IO),
    search_for_file_and_stream_or_error(SearchDirs, FileName0,
        MaybeFileNameAndStream, !IO),
    (
        MaybeFileNameAndStream = mfas_ok(FileNameAndStream),
        actually_read_module_int0(Globals, FileNameAndStream, ModuleName, [],
            ReadModuleAndTimestamps, MaybeTimestampRes,
            MaybeParseTreeInt0, Errors0, !IO),
        read_module_end_module(ProgressStream, Globals, MaybeFileNameAndStream,
            IgnoreErrors, fk_int(ifk_int3), ReadDoneMsg, FileName0, FileName,
            MaybeTimestampRes, MaybeTimestamp, Errors0, Errors1, !IO),
        (
            MaybeParseTreeInt0 = yes(ParseTreeInt0),
            HaveReadModule = have_read_module(FileName, MaybeTimestamp,
                ParseTreeInt0, Errors1)
        ;
            MaybeParseTreeInt0 = no,
            Errors = no_file_errors(IgnoreErrors, Errors1),
            HaveReadModule = have_not_read_module(FileName, Errors)
        )
    ;
        MaybeFileNameAndStream = mfas_error(Errors0),
        read_module_end_module(ProgressStream, Globals, MaybeFileNameAndStream,
            IgnoreErrors, fk_int(ifk_int3), ReadDoneMsg, FileName0, FileName,
            no, _MaybeTimestamp, Errors0, Errors1, !IO),
        Errors = no_file_errors(IgnoreErrors, Errors1),
        HaveReadModule = have_not_read_module(FileName, Errors)
    ).

read_module_int1(ProgressStream, Globals, ReadReasonMsg, IgnoreErrors, Search,
        ModuleName, ReadModuleAndTimestamps, HaveReadModule, !IO) :-
    read_module_begin(Globals, ReadReasonMsg, Search, ModuleName,
        fk_int(ifk_int1), FileName0, ReadDoneMsg, SearchDirs, !IO),
    search_for_file_and_stream_or_error(SearchDirs, FileName0,
        MaybeFileNameAndStream, !IO),
    (
        MaybeFileNameAndStream = mfas_ok(FileNameAndStream),
        actually_read_module_int1(Globals, FileNameAndStream, ModuleName, [],
            ReadModuleAndTimestamps, MaybeTimestampRes,
            MaybeParseTreeInt1, Errors0, !IO),
        read_module_end_module(ProgressStream, Globals, MaybeFileNameAndStream,
            IgnoreErrors, fk_int(ifk_int3), ReadDoneMsg, FileName0, FileName,
            MaybeTimestampRes, MaybeTimestamp, Errors0, Errors1, !IO),
        (
            MaybeParseTreeInt1 = yes(ParseTreeInt1),
            HaveReadModule = have_read_module(FileName, MaybeTimestamp,
                ParseTreeInt1, Errors1)
        ;
            MaybeParseTreeInt1 = no,
            Errors = no_file_errors(IgnoreErrors, Errors1),
            HaveReadModule = have_not_read_module(FileName, Errors)
        )
    ;
        MaybeFileNameAndStream = mfas_error(Errors0),
        read_module_end_module(ProgressStream, Globals, MaybeFileNameAndStream,
            IgnoreErrors, fk_int(ifk_int3), ReadDoneMsg, FileName0, FileName,
            no, _MaybeTimestamp, Errors0, Errors1, !IO),
        Errors = no_file_errors(IgnoreErrors, Errors1),
        HaveReadModule = have_not_read_module(FileName, Errors)
    ).

read_module_int2(ProgressStream, Globals, ReadReasonMsg, IgnoreErrors, Search,
        ModuleName, ReadModuleAndTimestamps, HaveReadModule, !IO) :-
    read_module_begin(Globals, ReadReasonMsg, Search, ModuleName,
        fk_int(ifk_int2), FileName0, ReadDoneMsg, SearchDirs, !IO),
    search_for_file_and_stream_or_error(SearchDirs, FileName0,
        MaybeFileNameAndStream, !IO),
    (
        MaybeFileNameAndStream = mfas_ok(FileNameAndStream),
        actually_read_module_int2(Globals, FileNameAndStream, ModuleName, [],
            ReadModuleAndTimestamps, MaybeTimestampRes,
            MaybeParseTreeInt2, Errors0, !IO),
        read_module_end_module(ProgressStream, Globals, MaybeFileNameAndStream,
            IgnoreErrors, fk_int(ifk_int3), ReadDoneMsg, FileName0, FileName,
            MaybeTimestampRes, MaybeTimestamp, Errors0, Errors1, !IO),
        (
            MaybeParseTreeInt2 = yes(ParseTreeInt2),
            HaveReadModule = have_read_module(FileName, MaybeTimestamp,
                ParseTreeInt2, Errors1)
        ;
            MaybeParseTreeInt2 = no,
            Errors = no_file_errors(IgnoreErrors, Errors1),
            HaveReadModule = have_not_read_module(FileName, Errors)
        )
    ;
        MaybeFileNameAndStream = mfas_error(Errors0),
        read_module_end_module(ProgressStream, Globals, MaybeFileNameAndStream,
            IgnoreErrors, fk_int(ifk_int3), ReadDoneMsg, FileName0, FileName,
            no, _MaybeTimestamp, Errors0, Errors1, !IO),
        Errors = no_file_errors(IgnoreErrors, Errors1),
        HaveReadModule = have_not_read_module(FileName, Errors)
    ).

read_module_int3(ProgressStream, Globals, ReadReasonMsg, IgnoreErrors, Search,
        ModuleName, ReadModuleAndTimestamps, HaveReadModule, !IO) :-
    read_module_begin(Globals, ReadReasonMsg, Search, ModuleName,
        fk_int(ifk_int3), FileName0, ReadDoneMsg, SearchDirs, !IO),
    search_for_file_and_stream_or_error(SearchDirs, FileName0,
        MaybeFileNameAndStream, !IO),
    (
        MaybeFileNameAndStream = mfas_ok(FileNameAndStream),
        actually_read_module_int3(Globals, FileNameAndStream, ModuleName, [],
            ReadModuleAndTimestamps, MaybeTimestampRes,
            MaybeParseTreeInt3, Errors0, !IO),
        read_module_end_module(ProgressStream, Globals, MaybeFileNameAndStream,
            IgnoreErrors, fk_int(ifk_int3), ReadDoneMsg, FileName0, FileName,
            MaybeTimestampRes, MaybeTimestamp, Errors0, Errors1, !IO),
        (
            MaybeParseTreeInt3 = yes(ParseTreeInt3),
            HaveReadModule = have_read_module(FileName, MaybeTimestamp,
                ParseTreeInt3, Errors1)
        ;
            MaybeParseTreeInt3 = no,
            Errors = no_file_errors(IgnoreErrors, Errors1),
            HaveReadModule = have_not_read_module(FileName, Errors)
        )
    ;
        MaybeFileNameAndStream = mfas_error(Errors0),
        read_module_end_module(ProgressStream, Globals, MaybeFileNameAndStream,
            IgnoreErrors, fk_int(ifk_int3), ReadDoneMsg, FileName0, FileName,
            no, _MaybeTimestamp, Errors0, Errors1, !IO),
        Errors = no_file_errors(IgnoreErrors, Errors1),
        HaveReadModule = have_not_read_module(FileName, Errors)
    ).

%---------------------%

read_module_int0_no_stream(Globals, ReadReasonMsg, IgnoreErrors,
        Search, ModuleName, ReadModuleAndTimestamps, HaveReadModule, !IO) :-
    read_module_int0(no, Globals, ReadReasonMsg, IgnoreErrors,
        Search, ModuleName, ReadModuleAndTimestamps, HaveReadModule, !IO).

read_module_int1_no_stream(Globals, ReadReasonMsg, IgnoreErrors,
        Search, ModuleName, ReadModuleAndTimestamps, HaveReadModule, !IO) :-
    read_module_int1(no, Globals, ReadReasonMsg, IgnoreErrors,
        Search, ModuleName, ReadModuleAndTimestamps, HaveReadModule, !IO).

read_module_int2_no_stream(Globals, ReadReasonMsg, IgnoreErrors,
        Search, ModuleName, ReadModuleAndTimestamps, HaveReadModule, !IO) :-
    read_module_int2(no, Globals, ReadReasonMsg, IgnoreErrors,
        Search, ModuleName, ReadModuleAndTimestamps, HaveReadModule, !IO).

read_module_int3_no_stream(Globals, ReadReasonMsg, IgnoreErrors,
        Search, ModuleName, ReadModuleAndTimestamps, HaveReadModule, !IO) :-
    read_module_int3(no, Globals, ReadReasonMsg, IgnoreErrors,
        Search, ModuleName, ReadModuleAndTimestamps, HaveReadModule, !IO).

%---------------------%

read_module_plain_opt(ProgressStream, Globals, ModuleName,
        HaveReadModule, !IO) :-
    ReadReasonMsg = rrm_std(ModuleName),
    Search = do_search,
    IgnoreErrors = do_not_ignore_errors,
    MaybeTimestampRes = no,

    read_module_begin(Globals, ReadReasonMsg, Search, ModuleName,
        fk_opt(ofk_opt), FileName0, ReadDoneMsg, SearchDirs, !IO),
    search_for_file_and_stream_or_error(SearchDirs, FileName0,
        MaybeFileNameAndStream, !IO),
    (
        MaybeFileNameAndStream = mfas_ok(FileNameAndStream),
        actually_read_module_plain_opt(Globals, FileNameAndStream, ModuleName,
            MaybeParseTreePlainOpt, Errors0, !IO),
        read_module_end_module(yes(ProgressStream), Globals,
            MaybeFileNameAndStream, IgnoreErrors, fk_opt(ofk_opt), ReadDoneMsg,
            FileName0, FileName, MaybeTimestampRes, _MaybeTimestamp,
            Errors0, Errors, !IO),
        (
            MaybeParseTreePlainOpt = yes(ParseTreePlainOpt),
            HaveReadModule = have_read_module(FileName, no,
                ParseTreePlainOpt, Errors)
        ;
            MaybeParseTreePlainOpt = no,
            HaveReadModule = have_not_read_module(FileName, Errors)
        )
    ;
        MaybeFileNameAndStream = mfas_error(Errors0),
        read_module_end_module(yes(ProgressStream), Globals,
            MaybeFileNameAndStream, IgnoreErrors, fk_opt(ofk_opt), ReadDoneMsg,
            FileName0, FileName, MaybeTimestampRes, _MaybeTimestamp,
            Errors0, Errors, !IO),
        HaveReadModule = have_not_read_module(FileName, Errors)
    ).

read_module_trans_opt(ProgressStream, Globals, ModuleName,
        HaveReadModule, !IO) :-
    ReadReasonMsg = rrm_std(ModuleName),
    Search = do_search,
    IgnoreErrors = do_not_ignore_errors,
    MaybeTimestampRes = no,

    read_module_begin(Globals, ReadReasonMsg, Search, ModuleName,
        fk_opt(ofk_trans_opt), FileName0, ReadDoneMsg, SearchDirs, !IO),
    search_for_file_and_stream_or_error(SearchDirs, FileName0,
        MaybeFileNameAndStream, !IO),
    (
        MaybeFileNameAndStream = mfas_ok(FileNameAndStream),
        actually_read_module_trans_opt(Globals, FileNameAndStream, ModuleName,
            MaybeParseTreeTransOpt, Errors0, !IO),
        read_module_end_module(yes(ProgressStream), Globals,
            MaybeFileNameAndStream, IgnoreErrors, fk_opt(ofk_trans_opt),
            ReadDoneMsg, FileName0, FileName,
            MaybeTimestampRes, _MaybeTimestamp, Errors0, Errors, !IO),
        (
            MaybeParseTreeTransOpt = yes(ParseTreeTransOpt),
            HaveReadModule = have_read_module(FileName, no,
                ParseTreeTransOpt, Errors)
        ;
            MaybeParseTreeTransOpt = no,
            HaveReadModule = have_not_read_module(FileName, Errors)
        )
    ;
        MaybeFileNameAndStream = mfas_error(Errors0),
        read_module_end_module(yes(ProgressStream), Globals,
            MaybeFileNameAndStream, IgnoreErrors, fk_opt(ofk_trans_opt),
            ReadDoneMsg, FileName0, FileName,
            MaybeTimestampRes, _MaybeTimestamp, Errors0, Errors, !IO),
        HaveReadModule = have_not_read_module(FileName, Errors)
    ).

%---------------------------------------------------------------------------%

:- type maybe_file_and_stream
    --->    mfas_ok(path_name_and_stream)
    ;       mfas_error(read_module_errors).

    % search_for_file_and_stream_or_error(Dirs, FileName,
    %   MaybeFilePathNameAndStream, !IO):
    %
    % Search Dirs for FileName. If found, return the path name of the file
    % and an open input stream reading from that file. Closing that stream
    % is the caller's responsibility.
    %
:- pred search_for_file_and_stream_or_error(list(dir_name)::in, file_name::in,
    maybe_file_and_stream::out, io::di, io::uo) is det.

search_for_file_and_stream_or_error(SearchDirs, FileName0,
        MaybeFileNameAndStream, !IO) :-
    % NB. Consider using search_for_file_returning_dir_and_stream,
    % which does not canonicalise the path, and is therefore more efficient.
    search_for_file_and_stream(SearchDirs, FileName0,
        RawMaybeFileNameAndStream, !IO),
    (
        RawMaybeFileNameAndStream = ok(FileNameAndStream),
        MaybeFileNameAndStream = mfas_ok(FileNameAndStream)
    ;
        RawMaybeFileNameAndStream = error(ErrorMsg),
        io_error_to_read_module_errors(ErrorMsg,
            frme_could_not_open_file, Errors, !IO),
        MaybeFileNameAndStream = mfas_error(Errors)
    ).

%---------------------------------------------------------------------------%

get_default_module_name_for_file(FileName, FileNameDotM,
        DefaultModuleName, !IO) :-
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
    ).

:- pred read_module_begin_from_file(globals::in, read_reason_msg::in,
    maybe_search::in, file_name::in, file_name::in, module_name::out,
    read_done_msg::out, list(string)::out, io::di, io::uo) is det.

read_module_begin_from_file(Globals, ReadReasonMsg, Search,
        FileName, FileNameDotM, DefaultModuleName, ReadDoneMsg,
        SearchDirs, !IO) :-
    % XXX Do not assume that the name of FileNameDotM guarantees
    % that the string it holds ends in ".m".
    get_default_module_name_for_file(FileName, FileNameDotM,
        DefaultModuleName, !IO),
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
        module_name_to_file_name(Globals, $pred, do_not_create_dirs,
            Ext, ModuleName, FileName, !IO),
        SearchDirs = [dir.this_directory]
    ),
    output_read_reason_msg(Globals, ReadReasonMsg, FileName, ReadDoneMsg, !IO).

:- pred read_module_end_module(maybe(io.text_output_stream)::in, globals::in,
    maybe_file_and_stream::in, maybe_ignore_errors::in,
    file_kind::in, read_done_msg::in, file_name::in, file_name::out,
    maybe(io.res(timestamp))::in, maybe(timestamp)::out,
    read_module_errors::in, read_module_errors::out, io::di, io::uo) is det.

read_module_end_module(MaybeProgressStream, Globals, MaybeFileNameAndStream,
        IgnoreErrors, FileKind, ReadDoneMsg, FileName0, FileName,
        MaybeTimestampRes, MaybeTimestamp, Errors0, Errors, !IO) :-
    % The code of read_module_end_module and read_module_end_file
    % should be kept in sync.
    (
        MaybeFileNameAndStream = mfas_ok(path_name_and_stream(FileName, _))
    ;
        MaybeFileNameAndStream = mfas_error(_),
        FileName = FileName0
    ),
    check_timestamp_report_if_needed_and_missing(Globals, FileName0,
        MaybeTimestampRes, MaybeTimestamp, !IO),
    handle_any_read_module_errors(Globals, FileKind, ReadDoneMsg,
        IgnoreErrors, Errors0, Errors, !IO),
    globals.lookup_bool_option(Globals, detailed_statistics, Statistics),
    (
        MaybeProgressStream = no
    ;
        MaybeProgressStream = yes(ProgressStream),
        maybe_report_stats(ProgressStream, Statistics, !IO)
    ).

:- pred read_module_end_file(globals::in, file_kind::in, read_done_msg::in,
    file_name::in, maybe(io.res(timestamp))::in, maybe(timestamp)::out,
    read_module_errors::in, read_module_errors::out,
    io::di, io::uo) is det.

read_module_end_file(Globals, FileKind, ReadDoneMsg, FileName,
        MaybeTimestampRes, MaybeTimestamp, Errors0, Errors, !IO) :-
    % The code of read_module_end_module and read_module_end_file
    % should be kept in sync.
    check_timestamp_report_if_needed_and_missing(Globals, FileName,
        MaybeTimestampRes, MaybeTimestamp, !IO),
    % Unlike read_module_end_module, we assume do_not_ignore_errors.
    handle_any_read_module_errors(Globals, FileKind, ReadDoneMsg,
        do_not_ignore_errors, Errors0, Errors, !IO).

:- func no_file_errors(maybe_ignore_errors, read_module_errors)
    = read_module_errors.

no_file_errors(IgnoreErrors, Errors0) = Errors :-
    (
        IgnoreErrors = do_not_ignore_errors,
        Errors = Errors0
    ;
        IgnoreErrors = ignore_errors,
        Errors = no_file_errors_ignored
    ).

    % According to IgnoreErrors = ignore_errors, being unable to
    % read the file is not an error, so delete all the error_specs,
    % but keep the fatal error to prevent this module from being put
    % into deps_maps. This is because modules in deps_maps get .d files
    % created for them, which is not appropriate for a file
    % that does not exist, at least in the current directory.
    %
:- func no_file_errors_ignored = read_module_errors.

no_file_errors_ignored = Errors :-
    Errors = read_module_errors(
        set.make_singleton_set(frme_could_not_open_file), [],
        set.init, [], []).

:- pred handle_any_read_module_errors(globals::in, file_kind::in,
    read_done_msg::in, maybe_ignore_errors::in,
    read_module_errors::in, read_module_errors::out, io::di, io::uo) is det.

handle_any_read_module_errors(Globals, FileKind, ReadDoneMsg,
        IgnoreErrors, Errors0, Errors, !IO) :-
    (
        IgnoreErrors = ignore_errors,
        FatalErrors0 = Errors0 ^ rm_fatal_errors,
        ( if set.contains(FatalErrors0, frme_could_not_open_file) then
            output_read_done_msg(ReadDoneMsg, "not found.\n", !IO),
            Errors = no_file_errors_ignored
        else
            % XXX CLEANUP we should not print "done" if Errors is nonempty.
            output_read_done_msg(ReadDoneMsg, "done.\n", !IO),
            Errors = Errors0
        )
    ;
        IgnoreErrors = do_not_ignore_errors,
        ( if there_are_no_errors(Errors0) then
            Errors = Errors0,
            % XXX CLEANUP This should just print "done".
            output_read_done_msg(ReadDoneMsg, "successful parse.\n", !IO)
        else
            FatalErrors0 = Errors0 ^ rm_fatal_errors,
            ( if set.contains(FatalErrors0, frme_could_not_open_file) then
                output_read_done_msg(ReadDoneMsg, "not found.\n", !IO)
            else if set.is_non_empty(FatalErrors0) then
                output_read_done_msg(ReadDoneMsg, "fatal error(s).\n", !IO)
            else
                % XXX CLEANUP Some of the errors are not PARSE errors.
                output_read_done_msg(ReadDoneMsg, "parse error(s).\n", !IO)
            ),
            (
                FileKind = fk_opt(_),
                Errors = Errors0
            ;
                ( FileKind = fk_src
                ; FileKind = fk_int(_)
                ),
                (
                    ReadDoneMsg = rdm_none,
                    Errors = Errors0
                ;
                    (
                        ReadDoneMsg = rdm_current,
                        io.output_stream(ErrorStream, !IO)
                    ;
                        ReadDoneMsg = rdm_progress(ErrorStream)
                    ),
                    % XXX STREAM It should be possible to print error messages
                    % to the relevant module's .err file, not just to the
                    % progress stream.
                    FatalErrorSpecs0 = Errors0 ^ rm_fatal_error_specs,
                    NonFatalErrorSpecs0 = Errors0 ^ rm_nonfatal_error_specs,
                    write_error_specs(ErrorStream, Globals,
                        FatalErrorSpecs0 ++ NonFatalErrorSpecs0, !IO),
                    Errors = ((Errors0
                        ^ rm_fatal_error_specs := [])
                        ^ rm_nonfatal_error_specs := [])
                ),
                io.set_exit_status(1, !IO)
            )
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

maybe_read_module_int0(ProgressStream, Globals, Search, ModuleName,
        ReturnTimestamp, HaveReadModule, !HaveReadModuleMaps, !IO) :-
    OrigHRMM = !.HaveReadModuleMaps ^ hrmm_int0,
    ( if
        search_module_name_timestamp_if_needed(OrigHRMM, ModuleName,
            ReturnTimestamp, HaveReadModulePrime)
    then
        HaveReadModule = HaveReadModulePrime
    else
        read_module_int0(yes(ProgressStream), Globals, rrm_std(ModuleName),
            do_not_ignore_errors, Search, ModuleName,
            always_read_module(ReturnTimestamp), HaveReadModule, !IO),
        map.set(ModuleName, HaveReadModule, OrigHRMM, HRMM),
        !HaveReadModuleMaps ^ hrmm_int0 := HRMM
    ).

maybe_read_module_int1(ProgressStream, Globals, Search, ModuleName,
        ReturnTimestamp, HaveReadModule, !HaveReadModuleMaps, !IO) :-
    OrigHRMM = !.HaveReadModuleMaps ^ hrmm_int1,
    ( if
        search_module_name_timestamp_if_needed(OrigHRMM, ModuleName,
            ReturnTimestamp, HaveReadModulePrime)
    then
        HaveReadModule = HaveReadModulePrime
    else
        read_module_int1(yes(ProgressStream), Globals, rrm_std(ModuleName),
            do_not_ignore_errors, Search, ModuleName,
            always_read_module(ReturnTimestamp), HaveReadModule, !IO),
        map.set(ModuleName, HaveReadModule, OrigHRMM, HRMM),
        !HaveReadModuleMaps ^ hrmm_int1 := HRMM
    ).

maybe_read_module_int2(ProgressStream, Globals, Search, ModuleName,
        ReturnTimestamp, HaveReadModule, !HaveReadModuleMaps, !IO) :-
    OrigHRMM = !.HaveReadModuleMaps ^ hrmm_int2,
    ( if
        search_module_name_timestamp_if_needed(OrigHRMM, ModuleName,
            ReturnTimestamp, HaveReadModulePrime)
    then
        HaveReadModule = HaveReadModulePrime
    else
        read_module_int2(yes(ProgressStream), Globals, rrm_std(ModuleName),
            do_not_ignore_errors, Search, ModuleName,
            always_read_module(ReturnTimestamp), HaveReadModule, !IO),
        map.set(ModuleName, HaveReadModule, OrigHRMM, HRMM),
        !HaveReadModuleMaps ^ hrmm_int2 := HRMM
    ).

maybe_read_module_int3(ProgressStream, Globals, Search, ModuleName,
        ReturnTimestamp, HaveReadModule, !HaveReadModuleMaps, !IO) :-
    OrigHRMM = !.HaveReadModuleMaps ^ hrmm_int3,
    ( if
        search_module_name_timestamp_if_needed(OrigHRMM, ModuleName,
            ReturnTimestamp, HaveReadModulePrime)
    then
        HaveReadModule = HaveReadModulePrime
    else
        read_module_int3(yes(ProgressStream), Globals, rrm_std(ModuleName),
            do_not_ignore_errors, Search, ModuleName,
            always_read_module(ReturnTimestamp), HaveReadModule, !IO),
        map.set(ModuleName, HaveReadModule, OrigHRMM, HRMM),
        !HaveReadModuleMaps ^ hrmm_int3 := HRMM
    ).

:- pred search_module_name_timestamp_if_needed(have_read_module_map(PT)::in,
    module_name::in, maybe_return_timestamp::in, have_read_module(PT)::out)
    is semidet.

search_module_name_timestamp_if_needed(HRMM, ModuleName, ReturnTimestamp,
        HaveReadModule) :-
    map.search(HRMM, ModuleName, HaveReadModule0),
    HaveReadModule0 = have_read_module(FN, MaybeTimeStamp0, PT, E),
    return_timestamp_if_needed(ReturnTimestamp,
        MaybeTimeStamp0, MaybeTimeStamp),
    HaveReadModule = have_read_module(FN, MaybeTimeStamp, PT, E).

%---------------------------------------------------------------------------%

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
