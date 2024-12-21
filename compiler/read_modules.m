%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2009, 2011 The University of Melbourne.
% Copyright (C) 2014-2017, 2019-2024 The Mercury team.
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
:- import_module parse_tree.prog_parse_tree.

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
% record its results in hptm_opt. I (zs) don't know whether that is
% a bug (leading to duplicate reads of optimization files) or a feature
% (keeping files that are by construction read exactly once out of a map
% where they won't be needed again).
%

:- type have_parse_tree_maps
    --->    have_parse_tree_maps(
                hptm_src                :: have_parse_tree_src_map,

                hptm_module_src         :: have_parse_tree_msrc_map,

                hptm_int0               :: have_parse_tree_int0_map,
                hptm_int1               :: have_parse_tree_int1_map,
                hptm_int2               :: have_parse_tree_int2_map,
                hptm_int3               :: have_parse_tree_int3_map,

                hptm_plain_opt          :: have_parse_tree_plain_opt_map,
                hptm_trans_opt          :: have_parse_tree_trans_opt_map
            ).

:- func init_have_parse_tree_maps = have_parse_tree_maps.

:- type have_parse_tree_src_map ==
    have_parse_tree_map(parse_tree_src).

:- type have_parse_tree_key(Kind)
    --->    have_parse_tree_key(module_name, Kind).

:- type have_parse_tree_msrc_map ==
    map(module_name, parse_tree_module_src).

:- type have_parse_tree_int0_map ==
    have_parse_tree_map(parse_tree_int0).
:- type have_parse_tree_int1_map ==
    have_parse_tree_map(parse_tree_int1).
:- type have_parse_tree_int2_map ==
    have_parse_tree_map(parse_tree_int2).
:- type have_parse_tree_int3_map ==
    have_parse_tree_map(parse_tree_int3).

:- type have_parse_tree_plain_opt_map ==
    have_parse_tree_map(parse_tree_plain_opt).
:- type have_parse_tree_trans_opt_map ==
    have_parse_tree_map(parse_tree_trans_opt).

:- type have_parse_tree_map(PT) == map(module_name, have_module(PT)).

    % This type records the result of one of the read_module_<filekind>
    % predicates below. There are three possible outcomes:
    %
    % - We have tried to read the module and succeeded.
    % - We have tried to read the module and failed.
    % - We have not tried to read the module.
    %
    % The first will return have_module(...), while the second and
    % third will return have_not_read_module(...).
    %
    % The reason for not having a separate function symbol for the third
    % outcome is that it can happen only for the small minority of calls
    % that ask that the file not be read in the event of a timestamp match,
    % but a third function symbol would force *all* calls to handle a situation
    % that cannot arise for them.
:- type have_module(PT)
    --->    have_module(
                % We have read, or constructed, the module.

                % The name of the file that either
                % - we read the parse tree from, or
                % - we have written the parse tree to.
                file_name,

                % The parse tree of the file.
                PT,

                % Where did we get the parse tree from? And what other info,
                % if any, did the "getting" process give us?
                have_parse_tree_source

            )
    ;       have_not_read_module(
                % We have tried to read the module, but did not do so.

                % The name of the file that we have either tried to read,
                % or whose timestamp indicated we did not have to read it.
                file_name,

                % The errors we got in the attempt to read the file.
                % If we did not attempt to read the file, this will contain
                % no errors at all. (This is how you can tell these two cases
                % apart.)
                read_module_errors
            ).

:- type have_read_module(PT) =< have_module(PT)
    --->    have_module(file_name, PT, have_parse_tree_read)
    ;       have_not_read_module(file_name, read_module_errors).

    % How did we get access to this parse tree: did we read it in
    % from the file system, or did we construct it? If we read it in,
    % then record any errors we found during parsing. The read_module_errors
    % field should contain neither frme_could_not_open_file nor
    % frme_could_not_read_file, but may contain any other errors.
    % In addition, we record the timestamp of the file we read in,
    % *if* the compiler component that decided to read the file
    % requested that the timestamp be returned. Not all do.
:- type have_parse_tree_source
    --->    was_read(maybe(timestamp), read_module_errors)
    ;       was_constructed.

:- type have_parse_tree_read =< have_parse_tree_source
    --->    was_read(maybe(timestamp), read_module_errors).

:- pred have_parse_tree_source_get_maybe_timestamp_errors(
    have_parse_tree_source::in,
    maybe(timestamp)::out, read_module_errors::out) is det.

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
    ;       rrm_std
            % Because the named file belongs to this module (as a source file,
            % interface file, or optimization file), and the compiler has been
            % asked to compile that module in some fashion.
    ;       rrm_old
            % Because the named file belongs to this module, and we need
            % the old contents of the file, so that we can write out
            % what we *now* think its contents should be, but *only*
            % if the "new" contents differ from the old. The difference
            % between whether (a) we keep the old contents as is, or
            % (b) we overwrite them with identical contents, is the
            % timestamp on the file. Keeping the old timestamp will
            % prevent unnecessary rebuilds of other files that depend
            % on that one.
    ;       rrm_get_deps.
            % Because the named file belongs to this module, and we need
            % to get its dependencies.

    % read_module_src(ProgressStream, Globals, ReadReasonMsg,
    %   IgnoreErrors, Search, ModuleName, FileName, ReadModuleAndTimestamps,
    %   HaveModule, !IO):
    %
    % Given a module name, read in and parse the source code of that file,
    % printing progress messages along the way if the verbosity level
    % calls for that.
    %
    % If Search is do_search, search all directories given by the option
    % search_directories for the module;
    % otherwise, search for those filenames only in the current directory.
    % Return in the FileName part of HaveModule the actual source file
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
    % If ReadModuleAndTimestamps is
    % always_read_module(do_not_return_timestamp), return `no' in the
    % MaybeTimestamp field of HaveModule.
    %
    % If ReadModuleAndTimestamps is always_read_module(do_return_timestamp),
    % attempt to return the modification time of the file in the MaybeTimestamp
    % field of HaveModule.
    %
    % If ReadModuleAndTimestamps is do_not_read_module_if_match(OldTimeStamp),
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
:- pred read_module_src(io.text_output_stream::in, globals::in,
    read_reason_msg::in, maybe_ignore_errors::in, maybe_search::in,
    module_name::in, list(term.context)::in, read_module_and_timestamps::in,
    have_read_module(parse_tree_src)::out, io::di, io::uo) is det.

    % read_module_src_from_file(ProgressStream, Globals,
    %   FileName, FileNameDotM, ReadReasonMsg, Search, ReadModuleAndTimestamps,
    %   HaveModule, !IO):
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
:- pred read_module_src_from_file(io.text_output_stream::in, globals::in,
    file_name::in, file_name::in, read_reason_msg::in, maybe_search::in,
    read_module_and_timestamps::in, have_read_module(parse_tree_src)::out,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%

    % maybe_read_module_intN(ProgressStream, Globals, ReadReasonMsg, Search,
    %   ModuleName, IntFileKind, ReturnTimestamp, HaveModule,
    %   !HaveParseTreeMaps, !IO):
    %
    % If !.HaveParseTreeMaps contains the already-read contents of the
    % relevant kind of interface file for ModuleName, then return
    % the information stored in !.HaveParseTreeMaps for that file.
    % If it is not there, read that interface file using read_module_intN,
    % regardless of its timestamp.
    %
:- pred maybe_read_module_int0(io.text_output_stream::in, globals::in,
    maybe_search::in, module_name::in, maybe_return_timestamp::in,
    have_module(parse_tree_int0)::out,
    have_parse_tree_maps::in, have_parse_tree_maps::out,
    io::di, io::uo) is det.
:- pred maybe_read_module_int1(io.text_output_stream::in, globals::in,
    maybe_search::in, module_name::in, maybe_return_timestamp::in,
    have_module(parse_tree_int1)::out,
    have_parse_tree_maps::in, have_parse_tree_maps::out,
    io::di, io::uo) is det.
:- pred maybe_read_module_int2(io.text_output_stream::in, globals::in,
    maybe_search::in, module_name::in, maybe_return_timestamp::in,
    have_module(parse_tree_int2)::out,
    have_parse_tree_maps::in, have_parse_tree_maps::out,
    io::di, io::uo) is det.
:- pred maybe_read_module_int3(io.text_output_stream::in, globals::in,
    maybe_search::in, module_name::in, maybe_return_timestamp::in,
    have_module(parse_tree_int3)::out,
    have_parse_tree_maps::in, have_parse_tree_maps::out,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%

    % read_module_intN(ProgressStream, Globals, ReadReasonMsg,
    %   IgnoreErrors, Search, ModuleName, FileName, ReadModuleAndTimestamps,
    %   HaveModule, !IO):
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
:- pred read_module_int0(io.text_output_stream::in, globals::in,
    read_reason_msg::in, maybe_ignore_errors::in, maybe_search::in,
    module_name::in, read_module_and_timestamps::in,
    have_module(parse_tree_int0)::out, io::di, io::uo) is det.
:- pred read_module_int1(io.text_output_stream::in, globals::in,
    read_reason_msg::in, maybe_ignore_errors::in, maybe_search::in,
    module_name::in, read_module_and_timestamps::in,
    have_module(parse_tree_int1)::out, io::di, io::uo) is det.
:- pred read_module_int2(io.text_output_stream::in, globals::in,
    read_reason_msg::in, maybe_ignore_errors::in, maybe_search::in,
    module_name::in, read_module_and_timestamps::in,
    have_module(parse_tree_int2)::out, io::di, io::uo) is det.
:- pred read_module_int3(io.text_output_stream::in, globals::in,
    read_reason_msg::in, maybe_ignore_errors::in, maybe_search::in,
    module_name::in, read_module_and_timestamps::in,
    have_module(parse_tree_int3)::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

    % read_module_{plain,trans}_opt(ProgressStream, Globals, ModuleName,
    %   FileName, HaveModule, !IO):
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
    module_name::in, have_module(parse_tree_plain_opt)::out,
    io::di, io::uo) is det.
:- pred read_module_trans_opt(io.text_output_stream::in, globals::in,
    module_name::in, have_module(parse_tree_trans_opt)::out,
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

:- pred return_timestamp_if_needed(maybe_return_timestamp::in,
    maybe(timestamp)::in, maybe(timestamp)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.options.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.file_kind.
:- import_module parse_tree.find_module.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.parse_module.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.source_file_map.

:- import_module bool.
:- import_module cord.
:- import_module dir.
:- import_module io.file.
:- import_module require.
:- import_module set.
:- import_module string.

%---------------------------------------------------------------------------%

init_have_parse_tree_maps =
    have_parse_tree_maps(map.init, map.init, map.init,
        map.init, map.init, map.init, map.init, map.init).

%---------------------------------------------------------------------------%

have_parse_tree_source_get_maybe_timestamp_errors(Source,
        MaybeTimestamp, Errors) :-
    (
        Source = was_read(MaybeTimestamp, Errors)
    ;
        Source = was_constructed,
        MaybeTimestamp = no,
        Errors = read_module_errors(set.init, [], set.init, [], [])
    ).

%---------------------------------------------------------------------------%

read_module_src(ProgressStream, Globals, ReadReasonMsg, IgnoreErrors,
        Search, ModuleName, ExpectationContexts, ReadModuleAndTimestamps,
        HaveModule, !IO) :-
    read_module_begin(ProgressStream, Globals, ReadReasonMsg, Search,
        ModuleName, fk_src, FileName0, ReadDoneMsg, SearchAuthDirs, !IO),
    % For `.m' files, we need to deal with the case where the module name
    % does not match the file name.
    search_for_module_source_and_stream(SearchAuthDirs, ModuleName,
        _SearchDirs, MaybeFileNameAndStream, !IO),
    (
        MaybeFileNameAndStream = ok(FileNameAndStream),
        actually_read_module_src(Globals, FileNameAndStream, ModuleName,
            ExpectationContexts, ReadModuleAndTimestamps, MaybeTimestampRes,
            MaybeParseTreeSrc0, Errors0, !IO),
        read_module_end_module(ProgressStream, Globals,
            mfas_ok(FileNameAndStream), IgnoreErrors, fk_src, ReadDoneMsg,
            FileName0, FileName, MaybeTimestampRes, MaybeTimestamp,
            Errors0, Errors1, !IO),
        FileNameAndStream = path_name_and_stream(_FilePathName, FileStream),
        io.close_input(FileStream, !IO),
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
            HaveModule = have_module(FileName, ParseTreeSrc,
                was_read(MaybeTimestamp, Errors1))
        ;
            MaybeParseTreeSrc0 = no,
            Errors = no_file_errors(IgnoreErrors, Errors1),
            HaveModule = have_not_read_module(FileName, Errors)
        )
    ;
        MaybeFileNameAndStream = error(ErrorMsg),
        io_error_to_read_module_errors(frme_could_not_open_file,
            phase_find_files(FileName0), ErrorMsg, Errors0, !IO),
        read_module_end_module(ProgressStream, Globals, mfas_error(Errors0),
            IgnoreErrors, fk_src, ReadDoneMsg, FileName0, FileName,
            no, _MaybeTimestamp, Errors0, Errors1, !IO),
        Errors = no_file_errors(IgnoreErrors, Errors1),
        HaveModule = have_not_read_module(FileName, Errors)
    ).

read_module_src_from_file(ProgressStream, Globals, FileName, FileNameDotM,
        ReadReasonMsg, Search, ReadModuleAndTimestamps, HaveModule, !IO) :-
    read_module_begin_from_file(ProgressStream, ife_src, Globals,
        ReadReasonMsg, Search, FileName, FileNameDotM, DefaultModuleName,
        ReadDoneMsg, SearchWhichDirs, !IO),
    search_for_file_and_stream(SearchWhichDirs, FileNameDotM,
        _SearchDirs, MaybeFileNameAndStream, !IO),
    (
        MaybeFileNameAndStream = ok(FileNameAndStream),
        actually_read_module_src(Globals, FileNameAndStream,
            DefaultModuleName, [], ReadModuleAndTimestamps, MaybeTimestampRes,
            MaybeParseTreeSrc, Errors0, !IO),
        read_module_end_file(Globals, fk_src, ReadDoneMsg, FileNameDotM,
            MaybeTimestampRes, MaybeTimestamp, Errors0, Errors1, !IO),
        FileNameAndStream = path_name_and_stream(_FilePathName, FileStream),
        io.close_input(FileStream, !IO),
        (
            MaybeParseTreeSrc = yes(ParseTreeSrc),
            HaveModule = have_module(FileNameDotM, ParseTreeSrc,
                was_read(MaybeTimestamp, Errors1))
        ;
            MaybeParseTreeSrc = no,
            HaveModule = have_not_read_module(FileNameDotM, Errors1)
        )
    ;
        MaybeFileNameAndStream = error(ErrorMsg),
        io_error_to_read_module_errors(frme_could_not_open_file,
            phase_find_files(FileNameDotM), ErrorMsg, Errors0, !IO),
        read_module_end_file(Globals, fk_src, ReadDoneMsg, FileNameDotM,
            no, _MaybeTimestamp, Errors0, Errors, !IO),
        HaveModule = have_not_read_module(FileNameDotM, Errors)
    ).

%---------------------------------------------------------------------------%

maybe_read_module_int0(ProgressStream, Globals, Search, ModuleName,
        ReturnTimestamp, HaveModule, !HaveParseTreeMaps, !IO) :-
    OrigHPTM = !.HaveParseTreeMaps ^ hptm_int0,
    ( if
        search_module_name_timestamp_if_needed(OrigHPTM, ModuleName,
            ReturnTimestamp, HaveReadModulePrime)
    then
        HaveModule = HaveReadModulePrime
    else
        read_module_int0(ProgressStream, Globals, rrm_std,
            do_not_ignore_errors, Search, ModuleName,
            always_read_module(ReturnTimestamp), HaveModule, !IO),
        map.set(ModuleName, HaveModule, OrigHPTM, HPTM),
        !HaveParseTreeMaps ^ hptm_int0 := HPTM
    ).

maybe_read_module_int1(ProgressStream, Globals, Search, ModuleName,
        ReturnTimestamp, HaveModule, !HaveParseTreeMaps, !IO) :-
    OrigHPTM = !.HaveParseTreeMaps ^ hptm_int1,
    ( if
        search_module_name_timestamp_if_needed(OrigHPTM, ModuleName,
            ReturnTimestamp, HaveReadModulePrime)
    then
        HaveModule = HaveReadModulePrime
    else
        read_module_int1(ProgressStream, Globals, rrm_std,
            do_not_ignore_errors, Search, ModuleName,
            always_read_module(ReturnTimestamp), HaveModule, !IO),
        map.set(ModuleName, HaveModule, OrigHPTM, HPTM),
        !HaveParseTreeMaps ^ hptm_int1 := HPTM
    ).

maybe_read_module_int2(ProgressStream, Globals, Search, ModuleName,
        ReturnTimestamp, HaveModule, !HaveParseTreeMaps, !IO) :-
    OrigHPTM = !.HaveParseTreeMaps ^ hptm_int2,
    ( if
        search_module_name_timestamp_if_needed(OrigHPTM, ModuleName,
            ReturnTimestamp, HaveReadModulePrime)
    then
        HaveModule = HaveReadModulePrime
    else
        read_module_int2(ProgressStream, Globals, rrm_std,
            do_not_ignore_errors, Search, ModuleName,
            always_read_module(ReturnTimestamp), HaveModule, !IO),
        map.set(ModuleName, HaveModule, OrigHPTM, HPTM),
        !HaveParseTreeMaps ^ hptm_int2 := HPTM
    ).

maybe_read_module_int3(ProgressStream, Globals, Search, ModuleName,
        ReturnTimestamp, HaveModule, !HaveParseTreeMaps, !IO) :-
    OrigHPTM = !.HaveParseTreeMaps ^ hptm_int3,
    ( if
        search_module_name_timestamp_if_needed(OrigHPTM, ModuleName,
            ReturnTimestamp, HaveReadModulePrime)
    then
        HaveModule = HaveReadModulePrime
    else
        read_module_int3(ProgressStream, Globals, rrm_std,
            do_not_ignore_errors, Search, ModuleName,
            always_read_module(ReturnTimestamp), HaveModule, !IO),
        map.set(ModuleName, HaveModule, OrigHPTM, HPTM),
        !HaveParseTreeMaps ^ hptm_int3 := HPTM
    ).

:- pred search_module_name_timestamp_if_needed(have_parse_tree_map(PT)::in,
    module_name::in, maybe_return_timestamp::in, have_module(PT)::out)
    is semidet.

search_module_name_timestamp_if_needed(HPTM, ModuleName, ReturnTimestamp,
        HaveModule) :-
    map.search(HPTM, ModuleName, HaveReadModule0),
    HaveReadModule0 = have_module(FN, PT, Source0),
    (
        Source0 = was_read(MaybeTimeStamp0, E),
        return_timestamp_if_needed(ReturnTimestamp,
            MaybeTimeStamp0, MaybeTimeStamp),
        Source = was_read(MaybeTimeStamp, E)
    ;
        Source0 = was_constructed,
        (
            ReturnTimestamp = do_not_return_timestamp,
            Source = Source0
        ;
            ReturnTimestamp = do_return_timestamp,
            unexpected($pred, "was_constructed")
        )
    ),
    HaveModule = have_module(FN, PT, Source).

%---------------------%

read_module_int0(ProgressStream, Globals, ReadReasonMsg, IgnoreErrors, Search,
        ModuleName, ReadModuleAndTimestamps, HaveModule, !IO) :-
    read_module_begin(ProgressStream, Globals, ReadReasonMsg, Search,
        ModuleName, fk_int(ifk_int0), FileName0, ReadDoneMsg, SearchDirs, !IO),
    search_for_file_and_stream_or_error(SearchDirs, FileName0,
        MaybeFileNameAndStream, !IO),
    (
        MaybeFileNameAndStream = mfas_ok(FileNameAndStream),
        actually_read_module_int0(Globals, FileNameAndStream, ModuleName, [],
            ReadModuleAndTimestamps, MaybeTimestampRes,
            MaybeParseTreeInt0, Errors0, !IO),
        read_module_end_module(ProgressStream, Globals, MaybeFileNameAndStream,
            IgnoreErrors, fk_int(ifk_int0), ReadDoneMsg, FileName0, FileName,
            MaybeTimestampRes, MaybeTimestamp, Errors0, Errors1, !IO),
        FileNameAndStream = path_name_and_stream(_FilePathName, FileStream),
        io.close_input(FileStream, !IO),
        (
            MaybeParseTreeInt0 = yes(ParseTreeInt0),
            HaveModule = have_module(FileName, ParseTreeInt0,
                was_read(MaybeTimestamp, Errors1))
        ;
            MaybeParseTreeInt0 = no,
            Errors = no_file_errors(IgnoreErrors, Errors1),
            HaveModule = have_not_read_module(FileName, Errors)
        )
    ;
        MaybeFileNameAndStream = mfas_error(Errors0),
        read_module_end_module(ProgressStream, Globals, MaybeFileNameAndStream,
            IgnoreErrors, fk_int(ifk_int0), ReadDoneMsg, FileName0, FileName,
            no, _MaybeTimestamp, Errors0, Errors1, !IO),
        Errors = no_file_errors(IgnoreErrors, Errors1),
        HaveModule = have_not_read_module(FileName, Errors)
    ).

read_module_int1(ProgressStream, Globals, ReadReasonMsg, IgnoreErrors, Search,
        ModuleName, ReadModuleAndTimestamps, HaveModule, !IO) :-
    read_module_begin(ProgressStream, Globals, ReadReasonMsg, Search,
        ModuleName, fk_int(ifk_int1), FileName0, ReadDoneMsg, SearchDirs, !IO),
    search_for_file_and_stream_or_error(SearchDirs, FileName0,
        MaybeFileNameAndStream, !IO),
    (
        MaybeFileNameAndStream = mfas_ok(FileNameAndStream),
        actually_read_module_int1(Globals, FileNameAndStream, ModuleName, [],
            ReadModuleAndTimestamps, MaybeTimestampRes,
            MaybeParseTreeInt1, Errors0, !IO),
        read_module_end_module(ProgressStream, Globals, MaybeFileNameAndStream,
            IgnoreErrors, fk_int(ifk_int1), ReadDoneMsg, FileName0, FileName,
            MaybeTimestampRes, MaybeTimestamp, Errors0, Errors1, !IO),
        FileNameAndStream = path_name_and_stream(_FilePathName, FileStream),
        io.close_input(FileStream, !IO),
        (
            MaybeParseTreeInt1 = yes(ParseTreeInt1),
            HaveModule = have_module(FileName, ParseTreeInt1,
                was_read(MaybeTimestamp, Errors1))
        ;
            MaybeParseTreeInt1 = no,
            Errors = no_file_errors(IgnoreErrors, Errors1),
            HaveModule = have_not_read_module(FileName, Errors)
        )
    ;
        MaybeFileNameAndStream = mfas_error(Errors0),
        read_module_end_module(ProgressStream, Globals, MaybeFileNameAndStream,
            IgnoreErrors, fk_int(ifk_int1), ReadDoneMsg, FileName0, FileName,
            no, _MaybeTimestamp, Errors0, Errors1, !IO),
        Errors = no_file_errors(IgnoreErrors, Errors1),
        HaveModule = have_not_read_module(FileName, Errors)
    ).

read_module_int2(ProgressStream, Globals, ReadReasonMsg, IgnoreErrors, Search,
        ModuleName, ReadModuleAndTimestamps, HaveModule, !IO) :-
    read_module_begin(ProgressStream, Globals, ReadReasonMsg, Search,
        ModuleName, fk_int(ifk_int2), FileName0, ReadDoneMsg, SearchDirs, !IO),
    search_for_file_and_stream_or_error(SearchDirs, FileName0,
        MaybeFileNameAndStream, !IO),
    (
        MaybeFileNameAndStream = mfas_ok(FileNameAndStream),
        actually_read_module_int2(Globals, FileNameAndStream, ModuleName, [],
            ReadModuleAndTimestamps, MaybeTimestampRes,
            MaybeParseTreeInt2, Errors0, !IO),
        read_module_end_module(ProgressStream, Globals, MaybeFileNameAndStream,
            IgnoreErrors, fk_int(ifk_int2), ReadDoneMsg, FileName0, FileName,
            MaybeTimestampRes, MaybeTimestamp, Errors0, Errors1, !IO),
        FileNameAndStream = path_name_and_stream(_FilePathName, FileStream),
        io.close_input(FileStream, !IO),
        (
            MaybeParseTreeInt2 = yes(ParseTreeInt2),
            HaveModule = have_module(FileName, ParseTreeInt2,
                was_read(MaybeTimestamp, Errors1))
        ;
            MaybeParseTreeInt2 = no,
            Errors = no_file_errors(IgnoreErrors, Errors1),
            HaveModule = have_not_read_module(FileName, Errors)
        )
    ;
        MaybeFileNameAndStream = mfas_error(Errors0),
        read_module_end_module(ProgressStream, Globals, MaybeFileNameAndStream,
            IgnoreErrors, fk_int(ifk_int2), ReadDoneMsg, FileName0, FileName,
            no, _MaybeTimestamp, Errors0, Errors1, !IO),
        Errors = no_file_errors(IgnoreErrors, Errors1),
        HaveModule = have_not_read_module(FileName, Errors)
    ).

read_module_int3(ProgressStream, Globals, ReadReasonMsg, IgnoreErrors, Search,
        ModuleName, ReadModuleAndTimestamps, HaveModule, !IO) :-
    read_module_begin(ProgressStream, Globals, ReadReasonMsg, Search,
        ModuleName, fk_int(ifk_int3), FileName0, ReadDoneMsg, SearchDirs, !IO),
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
        FileNameAndStream = path_name_and_stream(_FilePathName, FileStream),
        io.close_input(FileStream, !IO),
        (
            MaybeParseTreeInt3 = yes(ParseTreeInt3),
            HaveModule = have_module(FileName, ParseTreeInt3,
                was_read(MaybeTimestamp, Errors1))
        ;
            MaybeParseTreeInt3 = no,
            Errors = no_file_errors(IgnoreErrors, Errors1),
            HaveModule = have_not_read_module(FileName, Errors)
        )
    ;
        MaybeFileNameAndStream = mfas_error(Errors0),
        read_module_end_module(ProgressStream, Globals, MaybeFileNameAndStream,
            IgnoreErrors, fk_int(ifk_int3), ReadDoneMsg, FileName0, FileName,
            no, _MaybeTimestamp, Errors0, Errors1, !IO),
        Errors = no_file_errors(IgnoreErrors, Errors1),
        HaveModule = have_not_read_module(FileName, Errors)
    ).

%---------------------%

read_module_plain_opt(ProgressStream, Globals, ModuleName,
        HaveModule, !IO) :-
    ReadReasonMsg = rrm_std,
    Search = do_search,
    IgnoreErrors = do_not_ignore_errors,
    MaybeTimestampRes = no,

    read_module_begin(ProgressStream, Globals, ReadReasonMsg, Search,
        ModuleName, fk_opt(ofk_opt), FileName0, ReadDoneMsg, SearchDirs, !IO),
    search_for_file_and_stream_or_error(SearchDirs, FileName0,
        MaybeFileNameAndStream, !IO),
    (
        MaybeFileNameAndStream = mfas_ok(FileNameAndStream),
        actually_read_module_plain_opt(Globals, FileNameAndStream, ModuleName,
            MaybeParseTreePlainOpt, Errors0, !IO),
        read_module_end_module(ProgressStream, Globals,
            MaybeFileNameAndStream, IgnoreErrors, fk_opt(ofk_opt), ReadDoneMsg,
            FileName0, FileName, MaybeTimestampRes, _MaybeTimestamp,
            Errors0, Errors, !IO),
        FileNameAndStream = path_name_and_stream(_FilePathName, FileStream),
        io.close_input(FileStream, !IO),
        (
            MaybeParseTreePlainOpt = yes(ParseTreePlainOpt),
            HaveModule = have_module(FileName, ParseTreePlainOpt,
                was_read(no, Errors))
        ;
            MaybeParseTreePlainOpt = no,
            HaveModule = have_not_read_module(FileName, Errors)
        )
    ;
        MaybeFileNameAndStream = mfas_error(Errors0),
        read_module_end_module(ProgressStream, Globals,
            MaybeFileNameAndStream, IgnoreErrors, fk_opt(ofk_opt), ReadDoneMsg,
            FileName0, FileName, MaybeTimestampRes, _MaybeTimestamp,
            Errors0, Errors, !IO),
        HaveModule = have_not_read_module(FileName, Errors)
    ).

read_module_trans_opt(ProgressStream, Globals, ModuleName,
        HaveModule, !IO) :-
    ReadReasonMsg = rrm_std,
    Search = do_search,
    IgnoreErrors = do_not_ignore_errors,
    MaybeTimestampRes = no,

    read_module_begin(ProgressStream, Globals, ReadReasonMsg, Search,
        ModuleName, fk_opt(ofk_trans_opt), FileName0, ReadDoneMsg,
        SearchAuthDirs, !IO),
    search_for_file_and_stream_or_error(SearchAuthDirs, FileName0,
        MaybeFileNameAndStream, !IO),
    (
        MaybeFileNameAndStream = mfas_ok(FileNameAndStream),
        actually_read_module_trans_opt(Globals, FileNameAndStream, ModuleName,
            MaybeParseTreeTransOpt, Errors0, !IO),
        read_module_end_module(ProgressStream, Globals,
            MaybeFileNameAndStream, IgnoreErrors, fk_opt(ofk_trans_opt),
            ReadDoneMsg, FileName0, FileName,
            MaybeTimestampRes, _MaybeTimestamp, Errors0, Errors, !IO),
        FileNameAndStream = path_name_and_stream(_FilePathName, FileStream),
        io.close_input(FileStream, !IO),
        (
            MaybeParseTreeTransOpt = yes(ParseTreeTransOpt),
            HaveModule = have_module(FileName, ParseTreeTransOpt,
                was_read(no, Errors))
        ;
            MaybeParseTreeTransOpt = no,
            HaveModule = have_not_read_module(FileName, Errors)
        )
    ;
        MaybeFileNameAndStream = mfas_error(Errors0),
        read_module_end_module(ProgressStream, Globals,
            MaybeFileNameAndStream, IgnoreErrors, fk_opt(ofk_trans_opt),
            ReadDoneMsg, FileName0, FileName,
            MaybeTimestampRes, _MaybeTimestamp, Errors0, Errors, !IO),
        HaveModule = have_not_read_module(FileName, Errors)
    ).

%---------------------------------------------------------------------------%

    % actually_read_module_src(Globals, FileNameAndStream,
    %   DefaultModuleName, DefaultExpectationContexts,
    %   ReadModuleAndTimestamps, MaybeModuleTimestampRes,
    %   ParseTree, Errors, !IO):
    %
    % Read a Mercury source program from FileNameAndStream.
    % Close the stream when the reading is done. Return the parse tree
    % of that module in ParseTree (which may be a dummy if the file
    % couldn't be opened), and an indication of the errors found
    % in Specs and Errors.
    %
    % For the meaning of ReadModuleAndTimestamps and MaybeModuleTimestampRes,
    % read the comments on read_module_src in read_modules.m.
    %
:- pred actually_read_module_src(globals::in, path_name_and_stream::in,
    module_name::in, list(prog_context)::in,
    read_module_and_timestamps::in, maybe(io.res(timestamp))::out,
    maybe(parse_tree_src)::out, read_module_errors::out,
    io::di, io::uo) is det.

actually_read_module_src(_Globals, FileNameAndStream,
        DefaultModuleName, DefaultExpectationContexts,
        ReadModuleAndTimestamps, MaybeModuleTimestampRes,
        MaybeParseTreeSrc, Errors, !IO) :-
    do_actually_read_file(FileNameAndStream, ReadModuleAndTimestamps,
        ReadFileResult, !IO),
    (
        ReadFileResult = drfr_ok(FileStr, FileStrLen, MaybeModuleTimestampRes),
        FileNameAndStream = path_name_and_stream(FileName, _FileStream),
        parse_src_file(FileName, FileStr, FileStrLen,
            DefaultModuleName, DefaultExpectationContexts,
            MaybeParseTreeSrc, Errors)
    ;
        ReadFileResult = drfr_error(Errors, MaybeModuleTimestampRes),
        MaybeParseTreeSrc = no
    ).

%---------------------------------------------------------------------------%
%
% actually_read_module_intN(Globals, FileNameAndStream,
%   DefaultModuleName, DefaultExpectationContexts,
%   ReadModuleAndTimestamps, MaybeModuleTimestampRes,
%   ParseTree, Errors, !IO):
%
% Analogous to actually_read_module_src, but opens the specified kind
% of interface file for DefaultModuleName.
%

:- pred actually_read_module_int0(globals::in, path_name_and_stream::in,
    module_name::in, list(prog_context)::in,
    read_module_and_timestamps::in, maybe(io.res(timestamp))::out,
    maybe(parse_tree_int0)::out, read_module_errors::out,
    io::di, io::uo) is det.

actually_read_module_int0(Globals, FileNameAndStream,
        DefaultModuleName, DefaultExpectationContexts,
        ReadModuleAndTimestamps, MaybeModuleTimestampRes,
        MaybeParseTreeInt0, Errors, !IO) :-
    do_actually_read_file(FileNameAndStream, ReadModuleAndTimestamps,
        ReadFileResult, !IO),
    (
        ReadFileResult = drfr_ok(FileStr, FileStrLen, MaybeModuleTimestampRes),
        FileNameAndStream = path_name_and_stream(FileName, _FileStream),
        parse_int0_file(Globals, FileName, FileStr, FileStrLen,
            DefaultModuleName, DefaultExpectationContexts,
            MaybeParseTreeInt0, Errors)
    ;
        ReadFileResult = drfr_error(Errors, MaybeModuleTimestampRes),
        MaybeParseTreeInt0 = no
    ).

:- pred actually_read_module_int1(globals::in, path_name_and_stream::in,
    module_name::in, list(prog_context)::in,
    read_module_and_timestamps::in, maybe(io.res(timestamp))::out,
    maybe(parse_tree_int1)::out, read_module_errors::out,
    io::di, io::uo) is det.

actually_read_module_int1(Globals, FileNameAndStream,
        DefaultModuleName, DefaultExpectationContexts,
        ReadModuleAndTimestamps, MaybeModuleTimestampRes,
        MaybeParseTreeInt1, Errors, !IO) :-
    do_actually_read_file(FileNameAndStream, ReadModuleAndTimestamps,
        ReadFileResult, !IO),
    (
        ReadFileResult = drfr_ok(FileStr, FileStrLen, MaybeModuleTimestampRes),
        FileNameAndStream = path_name_and_stream(FileName, _FileStream),
        parse_int1_file(Globals, FileName, FileStr, FileStrLen,
            DefaultModuleName, DefaultExpectationContexts,
            MaybeParseTreeInt1, Errors)
    ;
        ReadFileResult = drfr_error(Errors, MaybeModuleTimestampRes),
        MaybeParseTreeInt1 = no
    ).

:- pred actually_read_module_int2(globals::in, path_name_and_stream::in,
    module_name::in, list(prog_context)::in,
    read_module_and_timestamps::in, maybe(io.res(timestamp))::out,
    maybe(parse_tree_int2)::out, read_module_errors::out,
    io::di, io::uo) is det.

actually_read_module_int2(Globals, FileNameAndStream,
        DefaultModuleName, DefaultExpectationContexts,
        ReadModuleAndTimestamps, MaybeModuleTimestampRes,
        MaybeParseTreeInt2, Errors, !IO) :-
    do_actually_read_file(FileNameAndStream, ReadModuleAndTimestamps,
        ReadFileResult, !IO),
    (
        ReadFileResult = drfr_ok(FileStr, FileStrLen, MaybeModuleTimestampRes),
        FileNameAndStream = path_name_and_stream(FileName, _FileStream),
        parse_int2_file(Globals, FileName, FileStr, FileStrLen,
            DefaultModuleName, DefaultExpectationContexts,
            MaybeParseTreeInt2, Errors)
    ;
        ReadFileResult = drfr_error(Errors, MaybeModuleTimestampRes),
        MaybeParseTreeInt2 = no
    ).

:- pred actually_read_module_int3(globals::in, path_name_and_stream::in,
    module_name::in, list(prog_context)::in,
    read_module_and_timestamps::in, maybe(io.res(timestamp))::out,
    maybe(parse_tree_int3)::out, read_module_errors::out,
    io::di, io::uo) is det.

actually_read_module_int3(Globals, FileNameAndStream,
        DefaultModuleName, DefaultExpectationContexts,
        ReadModuleAndTimestamps, MaybeModuleTimestampRes,
        MaybeParseTreeInt3, Errors, !IO) :-
    do_actually_read_file(FileNameAndStream, ReadModuleAndTimestamps,
        ReadFileResult, !IO),
    (
        ReadFileResult = drfr_ok(FileStr, FileStrLen, MaybeModuleTimestampRes),
        FileNameAndStream = path_name_and_stream(FileName, _FileStream),
        parse_int3_file(Globals, FileName, FileStr, FileStrLen,
            DefaultModuleName, DefaultExpectationContexts,
            MaybeParseTreeInt3, Errors)
    ;
        ReadFileResult = drfr_error(Errors, MaybeModuleTimestampRes),
        MaybeParseTreeInt3 = no
    ).

%---------------------------------------------------------------------------%
%
% actually_read_module_{plain,trans}_opt(Globals, FileNameAndStream,
%   DefaultModuleName, ParseTree, Errors, !IO):
%
% Analogous to actually_read_module_src, but opens the specified kind
% of optimization file for DefaultModuleName.
%

:- pred actually_read_module_plain_opt(globals::in, path_name_and_stream::in,
    module_name::in, maybe(parse_tree_plain_opt)::out, read_module_errors::out,
    io::di, io::uo) is det.

actually_read_module_plain_opt(_Globals, FileNameAndStream, DefaultModuleName,
        MaybeParseTreePlainOpt, Errors, !IO) :-
    ReadModuleAndTimestamps = always_read_module(do_not_return_timestamp),
    do_actually_read_file(FileNameAndStream, ReadModuleAndTimestamps,
        ReadFileResult, !IO),
    (
        ReadFileResult = drfr_ok(FileStr, FileStrLen, _),
        FileNameAndStream = path_name_and_stream(FileName, _FileStream),
        parse_plain_opt_file(FileName, FileStr, FileStrLen, DefaultModuleName,
            MaybeParseTreePlainOpt, Errors)
    ;
        ReadFileResult = drfr_error(Errors, _),
        MaybeParseTreePlainOpt = no
    ).

:- pred actually_read_module_trans_opt(globals::in, path_name_and_stream::in,
    module_name::in, maybe(parse_tree_trans_opt)::out, read_module_errors::out,
    io::di, io::uo) is det.

actually_read_module_trans_opt(_Globals, FileNameAndStream, DefaultModuleName,
        MaybeParseTreeTransOpt, Errors, !IO) :-
    ReadModuleAndTimestamps = always_read_module(do_not_return_timestamp),
    do_actually_read_file(FileNameAndStream, ReadModuleAndTimestamps,
        ReadFileResult, !IO),
    (
        ReadFileResult = drfr_ok(FileStr, FileStrLen, _),
        FileNameAndStream = path_name_and_stream(FileName, _FileStream),
        parse_trans_opt_file(FileName, FileStr, FileStrLen, DefaultModuleName,
            MaybeParseTreeTransOpt, Errors)
    ;
        ReadFileResult = drfr_error(Errors, _),
        MaybeParseTreeTransOpt = no
    ).

%---------------------------------------------------------------------------%

:- type do_read_file_result
    --->    drfr_ok(
                % The contents of the file, and its length.
                %
                % XXX When the compiler is compiled to C, this string will be
                % byte-for-byte identical to the contents of the file.
                % But when the compiler is compiled to C# or Java, the byte
                % sequence in the file will need to be TRANSLATED from UTF-8
                % to UTF-16. When the only reason we want to read e.g. a .intN
                % file is to compare its old contents to its new intended
                % contents, which will have to be converted from its UTF-16
                % version to UTF-8 anyway, this is wasteful, because it
                % requires the sequence
                %
                % - create new contents as UTF-16
                % - read old contents of the file, which is UTF-8
                % - convert old contents to UTF-16 (CONVERSION 1)
                % - test whether the old UTF-16 contents as the same as the new
                % - if the same then
                %   - done
                % - otherwise
                %   - convert the UTF-16 new contents to UTF-8 (CONVERSION 2)
                %   - write out the resulting UTF-8 string
                %
                % whereas if we returned the raw UTF-8 string directly as
                % a byte array, we could use the sequence
                %
                % - create new contents as UTF-16
                % - convert the UTF-16 new contents to UTF-8 (CONVERSION 1)
                % - read old contents of the file, which is UTF-8
                % - test whether the old UTF-8 contents as the same as the new
                % - if the same then
                %   - done
                % - otherwise
                %   - write out the new UTF-8 string
                %
                % This does the same number of conversions in the case where
                % the old and the new contents are the same, i.e. one
                % (though that conversion is in the opposite direction,
                % and the string it is applied to may be either shorter
                % or longer), but in the frequent case of the new contents
                % differing from the old, it does one fewer conversion.
                drfro_file_contents         :: string,
                drfro_num_code_units        :: int,

                % The timestamp of the file, if our caller requested it,
                drfro_maybe_file_timestamp  :: maybe(io.res(timestamp))
            )
    ;       drfr_error(
                drfre_errors                :: read_module_errors,

                % The timestamp of the file, if our caller requested it,
                % XXX This should not be needed if we can't read the file,
                % since it means nothing useful.
                drfre_maybe_file_timestamp  :: maybe(io.res(timestamp))
            ).

:- pred do_actually_read_file(path_name_and_stream::in,
    read_module_and_timestamps::in, do_read_file_result::out,
    io::di, io::uo) is det.

do_actually_read_file(FileNameAndStream, ReadModuleAndTimestamps,
        Result, !IO) :-
    FileNameAndStream = path_name_and_stream(FileName, FileStream),
    (
        ( ReadModuleAndTimestamps = always_read_module(do_return_timestamp)
        ; ReadModuleAndTimestamps = do_not_read_module_if_match(_)
        ),
        io.file.file_modification_time(FileName, TimestampResult, !IO),
        (
            TimestampResult = ok(Timestamp),
            MaybeModuleTimestampRes =
                yes(ok(time_t_to_timestamp(Timestamp)))
        ;
            TimestampResult = error(IOError),
            MaybeModuleTimestampRes = yes(error(IOError))
        )
    ;
        ReadModuleAndTimestamps = always_read_module(do_not_return_timestamp),
        MaybeModuleTimestampRes = no
    ),
    ( if
        ReadModuleAndTimestamps = do_not_read_module_if_match(OldTimestamp),
        MaybeModuleTimestampRes = yes(ok(OldTimestamp))
    then
        % XXX Currently smart recompilation won't work
        % if ModuleName \= DefaultModuleName.
        % In that case, smart recompilation will be disabled
        % and actually_read_module should never be passed an old timestamp.
        Result = drfr_error(init_read_module_errors, MaybeModuleTimestampRes)
    else
        io.read_file_as_string_and_num_code_units(FileStream,
            MaybeResult, !IO),
        (
            MaybeResult = ok2(FileString, NumCodeUnits),
            FileStringLen = string.length(FileString),
            ( if NumCodeUnits = FileStringLen then
                true
            else
                Msg = string.format(
                    "NumCodeUnits = %d, FileStringLen = %d\n<<<\n%s>>>\n",
                    [i(NumCodeUnits), i(FileStringLen), s(FileString)]),
                unexpected($pred, Msg)
            ),
            Result = drfr_ok(FileString, NumCodeUnits, MaybeModuleTimestampRes)
        ;
            MaybeResult = error2(_PartialStr, _PartialLen, ErrorCode),
            io.error_message(ErrorCode, ErrorMsg0),
            ErrorMsg = "I/O error: " ++ ErrorMsg0,
            io_error_to_read_module_errors(frme_could_not_read_file,
                phase_read_files, ErrorMsg, Errors, !IO),
            Result = drfr_error(Errors, MaybeModuleTimestampRes)
        )
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
:- pred search_for_file_and_stream_or_error(search_auth_dirs::in,
    file_name::in, maybe_file_and_stream::out, io::di, io::uo) is det.

search_for_file_and_stream_or_error(SearchAuthDirs, FileName0,
        MaybeFileNameAndStream, !IO) :-
    % NB. Consider using search_for_file_returning_dir_and_stream,
    % which does not canonicalise the path, and is therefore more efficient.
    search_for_file_and_stream(SearchAuthDirs, FileName0,
        _SearchDirs, RawMaybeFileNameAndStream, !IO),
    (
        RawMaybeFileNameAndStream = ok(FileNameAndStream),
        MaybeFileNameAndStream = mfas_ok(FileNameAndStream)
    ;
        RawMaybeFileNameAndStream = error(ErrorMsg),
        % XXX SEARCH_ERROR _SearchDirs
        io_error_to_read_module_errors(frme_could_not_find_file,
            phase_find_files(FileName0), ErrorMsg, Errors, !IO),
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
        HaveMap = not_found,
        file_name_to_module_name(BaseFileName, DefaultModuleName)
    ;
        HaveMap = found,
        lookup_source_file_module(FileNameDotM, MaybeModuleName, !IO),
        (
            MaybeModuleName = yes(DefaultModuleName)
        ;
            MaybeModuleName = no,
            file_name_to_module_name(BaseFileName, DefaultModuleName)
        )
    ).

%---------------------------------------------------------------------------%

:- type read_done_msg
    --->    rdm_none
            % Do not print a "done" message.
    ;       rdm_progress(io.text_output_stream).
            % Print a "done" message to the specified progress output stream.

%---------------------%

:- pred read_module_begin_from_file(io.text_output_stream::in,
    interface_ext::in, globals::in, read_reason_msg::in, maybe_search::in,
    file_name::in, file_name::in, module_name::out, read_done_msg::out,
    search_auth_dirs::out, io::di, io::uo) is det.

read_module_begin_from_file(ProgressStream, InterfaceExt, Globals,
        ReadReasonMsg, Search, FileName, FileNameDotM, DefaultModuleName,
        ReadDoneMsg, SearchAuthDirs, !IO) :-
    % XXX Do not assume that the name of FileNameDotM guarantees
    % that the string it holds ends in ".m".
    get_default_module_name_for_file(FileName, FileNameDotM,
        DefaultModuleName, !IO),
    % The rest of this predicate should be kept in sync
    % with read_module_begin.
    (
        Search = do_search,
        SearchAuthDirs = get_search_auth_interface_dirs(InterfaceExt, Globals)
    ;
        Search = do_not_search,
        SearchAuthDirs = search_auth_cur_dir
    ),
    output_read_reason_msg(ProgressStream, Globals, ReadReasonMsg,
        FileNameDotM, ReadDoneMsg, !IO).

:- pred read_module_begin(io.text_output_stream::in, globals::in,
    read_reason_msg::in, maybe_search::in, module_name::in, file_kind::in,
    file_name::out, read_done_msg::out, search_auth_dirs::out,
    io::di, io::uo) is det.

read_module_begin(ProgressStream, Globals, ReadReasonMsg, Search,
        ModuleName, FileKind, FileName, ReadDoneMsg, SearchAuthDirs, !IO) :-
    (
        FileKind = fk_src,
        % For the call to module_name_to_source_file_name, the value of Search
        % does not matter.
        module_name_to_source_file_name(ModuleName, FileName, !IO),
        (
            Search = do_search,
            SearchAuthDirs = get_search_auth_interface_dirs(ife_src, Globals)
        ;
            Search = do_not_search,
            SearchAuthDirs = search_auth_cur_dir
        )
    ;
        FileKind = fk_int(IntFileKind),
        int_file_kind_to_extension(IntFileKind, _ExtStr, Ext),
        % The rest of this predicate should be kept in sync
        % with read_module_begin_from_file.
        % NOTE: we use the same code for fk_int and fk_opt, but we need
        % separate copies, because the calls to module_name_to_search_file_name
        % check whether the insts of Ext match the values of SearchWhichDirs.
        (
            Search = do_search,
            SearchWhichDirs = search_interface_dirs,
            % XXX LEGACY
            module_name_to_search_file_name(Globals, $pred, Ext, ModuleName,
                SearchWhichDirs, SearchAuthDirs, FileName, _FileNameProposed)
        ;
            Search = do_not_search,
            % XXX LEGACY
            module_name_to_file_name(Globals, $pred, Ext,
                ModuleName, FileName, _FileNameProposed),
            SearchAuthDirs = search_auth_cur_dir
        )
    ;
        FileKind = fk_opt(OptFileKind),
        opt_file_kind_to_extension(OptFileKind, _ExtStr, Ext),
        % The rest of this predicate should be kept in sync
        % with read_module_begin_from_file.
        (
            Search = do_search,
            SearchWhichDirs = search_intermod_dirs,
            % XXX LEGACY
            module_name_to_search_file_name(Globals, $pred, Ext, ModuleName,
                SearchWhichDirs, SearchAuthDirs, FileName, _FileNameProposed)
        ;
            Search = do_not_search,
            % XXX LEGACY
            module_name_to_file_name(Globals, $pred, Ext,
                ModuleName, FileName, _FileNameProposed),
            SearchAuthDirs = search_auth_cur_dir
        )
    ),
    output_read_reason_msg(ProgressStream, Globals, ReadReasonMsg,
        FileName, ReadDoneMsg, !IO).

:- pred read_module_end_module(io.text_output_stream::in, globals::in,
    maybe_file_and_stream::in, maybe_ignore_errors::in,
    file_kind::in, read_done_msg::in, file_name::in, file_name::out,
    maybe(io.res(timestamp))::in, maybe(timestamp)::out,
    read_module_errors::in, read_module_errors::out, io::di, io::uo) is det.

read_module_end_module(ProgressStream, Globals, MaybeFileNameAndStream,
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
        MaybeTimestampRes, MaybeTimestamp, Errors0, Errors1, !IO),
    handle_any_read_module_errors(FileKind, ReadDoneMsg,
        IgnoreErrors, Errors1, Errors, !IO),
    globals.lookup_bool_option(Globals, detailed_statistics, Statistics),
    maybe_report_stats(ProgressStream, Statistics, !IO).

:- pred read_module_end_file(globals::in, file_kind::in, read_done_msg::in,
    file_name::in, maybe(io.res(timestamp))::in, maybe(timestamp)::out,
    read_module_errors::in, read_module_errors::out, io::di, io::uo) is det.

read_module_end_file(Globals, FileKind, ReadDoneMsg, FileName,
        MaybeTimestampRes, MaybeTimestamp, Errors0, Errors, !IO) :-
    % The code of read_module_end_module and read_module_end_file
    % should be kept in sync.
    check_timestamp_report_if_needed_and_missing(Globals, FileName,
        MaybeTimestampRes, MaybeTimestamp, Errors0, Errors1, !IO),
    % Unlike read_module_end_module, we assume do_not_ignore_errors.
    handle_any_read_module_errors(FileKind, ReadDoneMsg,
        do_not_ignore_errors, Errors1, Errors, !IO).

%---------------------------------------------------------------------------%

:- func no_file_errors(maybe_ignore_errors, read_module_errors)
    = read_module_errors.

no_file_errors(IgnoreErrors, Errors0) = Errors :-
    (
        IgnoreErrors = do_not_ignore_errors,
        Errors = Errors0
    ;
        IgnoreErrors = ignore_errors,
        FatalErrors0 = Errors0 ^ rm_fatal_errors,
        ( if set.contains(FatalErrors0, frme_could_not_find_file) then
            FatalError = frme_could_not_find_file
        else
            FatalError = frme_could_not_open_file
        ),
        Errors = no_file_errors_ignored(FatalError)
    ).

    % According to IgnoreErrors = ignore_errors, being unable to
    % read the file is not an error, so delete all the error_specs,
    % but keep the fatal error to prevent this module from being put
    % into deps_maps. This is because modules in deps_maps get .d files
    % created for them, which is not appropriate for a file
    % that does not exist, at least in the current directory.
    %
    % XXX Why not add a field to read_module_errors that specifically
    % says whether the module should be put into deps_maps?
    %
:- func no_file_errors_ignored(fatal_read_module_error) = read_module_errors.

no_file_errors_ignored(FatalError) = Errors :-
    Errors = read_module_errors(
        set.make_singleton_set(FatalError), [],
        set.init, [], []).

%---------------------%

:- pred handle_any_read_module_errors(file_kind::in,
    read_done_msg::in, maybe_ignore_errors::in,
    read_module_errors::in, read_module_errors::out, io::di, io::uo) is det.

handle_any_read_module_errors(FileKind, ReadDoneMsg, IgnoreErrors,
        Errors0, Errors, !IO) :-
    (
        IgnoreErrors = ignore_errors,
        FatalErrors0 = Errors0 ^ rm_fatal_errors,
        ( if set.contains(FatalErrors0, frme_could_not_open_file) then
            output_read_done_msg(ReadDoneMsg, "not found.\n", !IO),
            Errors = no_file_errors_ignored(frme_could_not_open_file)
        else
            % XXX CLEANUP we should not print "done" if Errors is nonempty.
            output_read_done_msg(ReadDoneMsg, "done.\n", !IO),
            Errors = Errors0
        )
    ;
        IgnoreErrors = do_not_ignore_errors,
        FatalErrors0 = Errors0 ^ rm_fatal_errors,
        NonFatalErrors0 = Errors0 ^ rm_nonfatal_errors,
        ( if
            ( set.is_non_empty(FatalErrors0)
            ; set.is_non_empty(NonFatalErrors0)
            )
        then
            ( if set.is_non_empty(FatalErrors0) then
                ( if set.contains(FatalErrors0, frme_could_not_open_file) then
                    Msg = "not found.\n"
                else
                    Msg = "fatal error(s).\n"
                )
            else
                % Any errors must be nonfatal. Are they syntax errors?
                ( if
                    ( set.contains(NonFatalErrors0,
                        rme_no_module_decl_at_start)
                    ; set.contains(NonFatalErrors0,
                        rme_no_section_decl_at_start)
                    ; set.contains(NonFatalErrors0,
                        rme_end_module_not_at_end_of_src)
                    ; set.contains(NonFatalErrors0,
                        rme_unexpected_term_in_int_or_opt)
                    ; set.contains(NonFatalErrors0, rme_could_not_read_term)
                    ; set.contains(NonFatalErrors0, rme_could_not_parse_item)
                    )
                then
                    Msg = "parse error(s).\n"
                else
                    Msg = "nonfatal error(s).\n"
                )
            ),
            output_read_done_msg(ReadDoneMsg, Msg, !IO),
            (
                FileKind = fk_opt(_),
                Errors = Errors0
            ;
                ( FileKind = fk_src
                ; FileKind = fk_int(_)
                ),
                Errors = Errors0
            )
        else
            Errors = Errors0,
            % XXX CLEANUP This should just print "done".
            output_read_done_msg(ReadDoneMsg, "successful parse.\n", !IO)
        )
    ).

%---------------------%

:- pred output_read_reason_msg(io.text_output_stream::in, globals::in,
    read_reason_msg::in, string::in, read_done_msg::out,
    io::di, io::uo) is det.

output_read_reason_msg(ProgressStream, Globals, ReadReasonMsg, FileName,
        ReadDoneMsg, !IO) :-
    globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
    (
        VeryVerbose = no,
        ReadDoneMsg = rdm_none
    ;
        VeryVerbose = yes,
        (
            ReadReasonMsg = rrm_file,
            string.format("%% Reading `%s'... ", [s(FileName)], Msg)
        ;
            ReadReasonMsg = rrm_std,
            string.format("%% Reading `%s'... ", [s(FileName)], Msg)
        ;
            ReadReasonMsg = rrm_old,
            string.format("%% Reading old version of `%s'... ",
                [s(FileName)], Msg)
        ;
            ReadReasonMsg = rrm_get_deps,
            string.format("%% Getting dependencies for `%s'... ",
                [s(FileName)], Msg)
        ),
        ReadDoneMsg = rdm_progress(ProgressStream),
        io.write_string(ProgressStream, Msg, !IO),
        io.flush_output(ProgressStream, !IO)
    ).

:- pred output_read_done_msg(read_done_msg::in, string::in,
    io::di, io::uo) is det.

output_read_done_msg(ReadDoneMsg, Msg, !IO) :-
    (
        ReadDoneMsg = rdm_none
    ;
        ReadDoneMsg = rdm_progress(ProgressStream),
        io.write_string(ProgressStream, Msg, !IO),
        io.flush_output(ProgressStream, !IO)
    ).

%---------------------------------------------------------------------------%

return_timestamp_if_needed(ReturnTimestamp, MaybeTimestamp0, MaybeTimestamp) :-
    (
        ReturnTimestamp = do_return_timestamp,
        (
            MaybeTimestamp0 = no,
            % This can happen if
            %
            % - code that does not need a timestamp enters a parse tree
            %   into the have_module_map, and then later
            %
            % - code that does need a timestamp finds the parse tree there.
            %
            % We abort because I (zs) don't think this should happen:
            % the use of have_parse_tree_maps in module_imports.m never
            % needs timestamps, the smart recompilation modules always
            % need timestamps, but the have_parse_tree_maps they use
            % are completely separate (for now). If it turns out I am wrong,
            % or we *do* want these two subsystems to use the same
            % have_parse_tree_maps, then there are two obvious possibilities:
            % either *always* store the timestamp of a file we read in, or
            % get the timestamp from the OS the first time it is needed
            % (have_parse_tree_maps entries include the filename, so this
            % is possible). The first solution is simpler, the second can
            % execute fewer system calls.
            unexpected($pred, "do_return_timestamp but no timestamp")
        ;
            MaybeTimestamp0 = yes(_),
            MaybeTimestamp = MaybeTimestamp0
        )
    ;
        ReturnTimestamp = do_not_return_timestamp,
        MaybeTimestamp = no
    ).

%---------------------------------------------------------------------------%

:- pred check_timestamp_report_if_needed_and_missing(globals::in,
    file_name::in, maybe(io.res(timestamp))::in, maybe(timestamp)::out,
    read_module_errors::in, read_module_errors::out, io::di, io::uo) is det.

check_timestamp_report_if_needed_and_missing(Globals, FileName,
        MaybeTimestampRes, MaybeTimestamp, !Errors, !IO) :-
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
                    IOError, !Errors, !IO)
            ;
                SmartRecompilation = no
            )
        )
    ;
        MaybeTimestampRes = no,
        MaybeTimestamp = no
    ).

:- pred record_and_report_missing_timestamp(globals::in, file_name::in,
    io.error::in, read_module_errors::in, read_module_errors::out,
    io::di, io::uo) is det.

record_and_report_missing_timestamp(Globals, FileName, IOError,
        !Errors, !IO) :-
    io_set_disable_smart_recompilation(disable_smart_recompilation, !IO),
    io_set_disable_generate_item_version_numbers(disable_item_version_numbers,
        !IO),
    globals.lookup_bool_option(Globals, warn_smart_recompilation, Warn),
    (
        Warn = yes,
        io.error_message(IOError, IOErrorMsg),
        Pieces = [words("Warning: cannot find modification time for"),
            quote(FileName), suffix(":"), nl_indent_delta(1),
            words(IOErrorMsg), suffix("."), nl_indent_delta(-1),
            words("Smart recompilation will not work."), nl],
        Spec = no_ctxt_spec($pred, severity_warning, phase_read_files, Pieces),
        add_nonfatal_error(rme_cannot_find_modify_time, [Spec], !Errors)
    ;
        Warn = no
    ).

%---------------------------------------------------------------------------%
:- end_module parse_tree.read_modules.
%---------------------------------------------------------------------------%
