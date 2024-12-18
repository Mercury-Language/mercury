%-----------------------------------------------------------------------------e
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------e
% Copyright (C) 1993-2012 The University of Melbourne.
% Copyright (C) 2014-2017, 2019-2020, 2022-2024 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This module defines predicates that find Mercury modules.
%
%---------------------------------------------------------------------------%

:- module parse_tree.find_module.
:- interface.

:- import_module libs.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.

:- import_module io.
:- import_module list.
:- import_module maybe.
:- import_module time.

%---------------------------------------------------------------------------%

:- type path_name_and_stream
    --->    path_name_and_stream(file_name, io.text_input_stream).
            % The string is a file name.

:- type dir_name_and_stream
    --->    dir_name_and_stream(dir_name, io.text_input_stream).
            % The string is a dir name.

:- type dir_name_and_contents
    --->    dir_name_and_contents(dir_name, string).
            % The first string is a dir name.
            % The second string is the contents of the file.

%---------------------%

    % The search_auth_dirs type represents a "search authorization",
    % in the sense that it says "it makes sense to search for the associated
    % file name in the directories it specifies".
    %
    % Searching in the current directory, and in a user-specified directory,
    % requires no special permission, so the first four values of this type
    % (modulo the tail dirs) can be constructed by code in any compiler module.
    % The private ones should be constructed *only*
    %
    % - by the module_name_to_search_file_name predicate in file_names.m,
    %   which checks the compatibility between the file's extension and the
    %   specification of the directories to be searched, and
    %
    % - by the get_search_auth_* functions below. Note that ascertaining
    %   whether their callers pair them sensibly with the file names
    %   to be searched for requires inspecting each one separately.
    %
    % Any file name reported by "grep -l private_auth *.m" *other than*
    % file_names.m and find_module.m represents a violation of this rule.
    %
    % The *point* of such authorizations is to centralize the lookup
    % of options representing lists of directories to search in this module,
    % and specifically in the compute_search_dirs predicate. This
    % centralization makes it feasible to replace the existing set of options,
    % which require users to specify more than one directory in e.g. an
    % installed library (e.g. both the non-grade-specific and the
    % grade-specific location containing files with the same extension),
    % with a new set of options that do not require such duplication.
:- type search_auth_dirs
    --->    search_auth_cur_dir
    ;       search_auth_cur_dir_and(search_auth_tail_dirs)
            % XXX LEGACY All existing uses of search_auth_cur_dir_and take
            % the list of tail directories from the value of an option.
            % All these uses should be replaced by code in handle_options.m
            % that includes the current directory in the value of that option.
            % However, there is no point in doing this for the LEGACY searches,
            % and doing it for PROPOSED searches will be much easier once
            % we no longer support LEGACY searches.
    ;       search_auth_this_dir(dir_name)
    ;       search_auth_this_dir_and(dir_name, search_auth_tail_dirs)
    ;       search_auth_private(search_auth_private_dirs).

:- type search_auth_private_dirs
    --->    private_auth_normal_dirs(normal_ext, globals)
    ;       private_auth_intermod_dirs(intermod_ext, globals)
    ;       private_auth_c_include_dirs(c_incl_ext, globals)
    ;       private_auth_options_file_dirs(option_table)
    ;       private_auth_lib_dirs(lib_ext, globals)
    ;       private_auth_stdlib_dirs(stdlib_ext, globals).

    % search_which_tail_dirs/search_auth_private_tail_dirs together
    % differ from search_auth_dirs/search_auth_private_dirs the same way
    % as search_which_tail_dirs differs from search_which_dirs.
    %
:- type search_auth_tail_dirs =< search_auth_dirs
    --->    search_auth_cur_dir
    ;       search_auth_private(search_auth_private_tail_dirs).

:- type search_auth_private_tail_dirs =< search_auth_private_dirs
    --->    private_auth_normal_dirs(normal_ext, globals)
    ;       private_auth_intermod_dirs(intermod_ext, globals)
    ;       private_auth_c_include_dirs(c_incl_ext, globals)
    ;       private_auth_options_file_dirs(option_table).

:- func get_search_auth_normal_dirs(normal_ext, globals)
    = search_auth_dirs.
:- func get_search_auth_intermod_dirs(intermod_ext, globals)
    = search_auth_dirs.
:- func get_search_auth_options_file_dirs(option_table)
    = search_auth_tail_dirs.
:- func get_search_auth_lib_dirs(lib_ext, globals)
    = search_auth_dirs.
:- func get_search_auth_stdlib_dirs(stdlib_ext, globals)
    = search_auth_dirs.

%---------------------%

    % search_for_file(SearchAuthDirs, FileName,
    %   SearchDirs, MaybeFilePathName, !IO):
    %
    % Search the specified directories for FileName. If found,
    % return the path name of the file. We also return the list of directories
    % we searched as SearchDirs, for possible use in error messages.
    %
    % NB. Consider using search_for_file_returning_dir, which does not
    % canonicalise the path, and is therefore more efficient.
    %
:- pred search_for_file(search_auth_dirs::in, file_name::in,
    list(dir_name)::out, maybe_error(file_name)::out, io::di, io::uo) is det.

    % search_for_file_and_stream(SearchAuthDirs, FileName,
    %   SearchDirs, MaybeFilePathNameAndStream, !IO):
    %
    % Search the specified directories for FileName. If found,
    % return the path name of the file and an open input stream
    % reading from that file. Closing that stream is the caller's
    % responsibility.
    %
    % NB. Consider using search_for_file_returning_dir_and_stream,
    % which does not canonicalise the path, and is therefore more efficient.
    %
:- pred search_for_file_and_stream(search_auth_dirs::in, file_name::in,
    list(dir_name)::out, maybe_error(path_name_and_stream)::out,
    io::di, io::uo) is det.

%---------------------%

    % search_for_file_returning_dir(SearchAuthDirs, FileName,
    %   SearchDirs, MaybeDirName, !IO):
    %
    % Search the specified directories for FileName. If found,
    % return the name of the directory in which the file was found.
    %
:- pred search_for_file_returning_dir(search_auth_dirs::in, file_name::in,
    list(dir_name)::out, maybe_error(dir_name)::out, io::di, io::uo) is det.

    % search_for_file_returning_dir_and_stream(SearchAuthDirs, FileName,
    %   SearchDirs, MaybeDirNameAndStream, !IO):
    %
    % Search the specified directory for FileName. If found,
    % return the name of the directory in which the file was found,
    % and an open input stream reading from that file. Closing that stream
    % is the caller's responsibility.
    %
:- pred search_for_file_returning_dir_and_stream(search_auth_dirs::in,
    file_name::in, list(dir_name)::out,
    maybe_error(path_name_and_stream)::out, io::di, io::uo) is det.

    % search_for_file_returning_dir_and_contents(SearchAuthDirs, FileName,
    %   SearchDirs, MaybeDirNameAndContents, !IO):
    %
    % Search the specified directories for FileName. If found,
    % return the name of the directory in which the file was found,
    % and the contents of the file as a string.
    %
:- pred search_for_file_returning_dir_and_contents(search_auth_dirs::in,
    file_name::in, list(dir_name)::out,
    maybe_error(dir_name_and_contents)::out, io::di, io::uo) is det.

%---------------------%

    % search_for_file_mod_time(SearchAuthDirs, FileName,
    %   SearchDirs, MaybeModTime, !IO)
    %
    % Search the specified directories for FileName. If found,
    % return the last modification time of the file that was found.
    % Do NOT open the file for reading.
    %
:- pred search_for_file_mod_time(search_auth_dirs::in, file_name::in,
    list(dir_name)::out, maybe_error(time_t)::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

    % search_for_module_source(SearchAuthDirs, ModuleName,
    %   SearchDirs, FoundSourceFile, !IO):
    %
    % Search the specified directories for the source code of ModuleName.
    % If found, return the (relative or absolute) path name of the
    % source file that contains the module.
    %
:- pred search_for_module_source(search_auth_dirs::in, module_name::in,
    list(dir_name)::out, maybe_error(file_name)::out, io::di, io::uo) is det.

    % search_for_module_source_and_stream(SearchAuthDirs, ModuleName,
    %   SearchDirs, FoundSourceFileNameAndStream, !IO):
    %
    % As search_for_module_source, but if we find the file, then return
    % not just its path name, but also an open stream reading from it.
    % Closing that stream is the caller's responsibility.
    %
:- pred search_for_module_source_and_stream(search_auth_dirs::in,
    module_name::in, list(dir_name)::out,
    maybe_error(path_name_and_stream)::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.file_names.

:- import_module dir.
:- import_module getopt.
:- import_module io.file.
:- import_module map.
:- import_module string.

%---------------------------------------------------------------------------%

get_search_auth_normal_dirs(NormalExt, Globals) =
    search_auth_private(private_auth_normal_dirs(NormalExt, Globals)).

get_search_auth_intermod_dirs(IntermodExt, Globals) =
    search_auth_private(private_auth_intermod_dirs(IntermodExt, Globals)).

get_search_auth_options_file_dirs(OptionTable) =
    search_auth_private(private_auth_options_file_dirs(OptionTable)).

get_search_auth_lib_dirs(LibExt, Globals) =
    search_auth_private(private_auth_lib_dirs(LibExt, Globals)).

get_search_auth_stdlib_dirs(StdLibExt, Globals) =
    search_auth_private(private_auth_stdlib_dirs(StdLibExt, Globals)).

%---------------------------------------------------------------------------%

search_for_file(SearchAuthDirs, FileName,
        SearchDirs, MaybeFilePathName, !IO) :-
    search_for_file_and_stream(SearchAuthDirs, FileName,
        SearchDirs, MaybeFilePathNameAndStream, !IO),
    (
        MaybeFilePathNameAndStream =
            ok(path_name_and_stream(FilePathName, Stream)),
        io.close_input(Stream, !IO),
        MaybeFilePathName = ok(FilePathName)
    ;
        MaybeFilePathNameAndStream = error(Msg),
        MaybeFilePathName = error(Msg)
    ).

search_for_file_and_stream(SearchAuthDirs, FileName,
        SearchDirs, MaybeFilePathNameAndStream, !IO) :-
    compute_search_dirs(SearchAuthDirs, SearchDirs),
    search_for_file_and_stream_loop(SearchDirs, SearchDirs, FileName,
        MaybeFilePathNameAndStream, !IO).

:- pred search_for_file_and_stream_loop(list(dir_name)::in,
    list(dir_name)::in, file_name::in, maybe_error(path_name_and_stream)::out,
    io::di, io::uo) is det.

search_for_file_and_stream_loop(AllDirs, Dirs, FileName,
        MaybeFilePathNameAndStream, !IO) :-
    (
        Dirs = [],
        Msg = cannot_find_in_dirs_msg(FileName, AllDirs),
        MaybeFilePathNameAndStream = error(Msg)
    ;
        Dirs = [HeadDir | TailDirs],
        make_path_name_noncanon(HeadDir, FileName, HeadFilePathNameNC),
        io.open_input(HeadFilePathNameNC, MaybeHeadStream, !IO),
        (
            MaybeHeadStream = ok(HeadStream),
            % HeadFilePathName should be a canonical version of
            % HeadFilePathNameNC.
            ( if dir.this_directory(HeadDir) then
                HeadFilePathName = FileName
            else
                HeadFilePathName = dir.make_path_name(HeadDir, FileName)
            ),
            MaybeFilePathNameAndStream =
                ok(path_name_and_stream(HeadFilePathName, HeadStream))
        ;
            MaybeHeadStream = error(_),
            search_for_file_and_stream_loop(AllDirs, TailDirs,
                FileName, MaybeFilePathNameAndStream, !IO)
        )
    ).

%---------------------%

search_for_file_returning_dir(SearchAuthDirs, FileName,
        SearchDirs, MaybeDirPathName, !IO) :-
    search_for_file_returning_dir_and_stream(SearchAuthDirs, FileName,
        SearchDirs, MaybeDirPathNameAndStream, !IO),
    (
        MaybeDirPathNameAndStream =
            ok(path_name_and_stream(DirPathName, Stream)),
        io.close_input(Stream, !IO),
        MaybeDirPathName = ok(DirPathName)
    ;
        MaybeDirPathNameAndStream = error(Msg),
        MaybeDirPathName = error(Msg)
    ).

search_for_file_returning_dir_and_stream(SearchAuthDirs, FileName,
        SearchDirs, MaybeDirPathNameAndStream, !IO) :-
    compute_search_dirs(SearchAuthDirs, SearchDirs),
    search_for_file_returning_dir_and_stream_loop(SearchDirs, SearchDirs,
        FileName, MaybeDirPathNameAndStream, !IO).

:- pred search_for_file_returning_dir_and_stream_loop(list(dir_name)::in,
    list(dir_name)::in, file_name::in, maybe_error(path_name_and_stream)::out,
    io::di, io::uo) is det.

search_for_file_returning_dir_and_stream_loop(AllDirs, Dirs, FileName,
        MaybeDirNameAndStream, !IO) :-
    (
        Dirs = [],
        Msg = cannot_find_in_dirs_msg(FileName, AllDirs),
        MaybeDirNameAndStream = error(Msg)
    ;
        Dirs = [HeadDir | TailDirs],
        make_path_name_noncanon(HeadDir, FileName, HeadFilePathNameNC),
        io.open_input(HeadFilePathNameNC, MaybeHeadStream, !IO),
        (
            MaybeHeadStream = ok(HeadStream),
            MaybeDirNameAndStream =
                ok(path_name_and_stream(HeadDir, HeadStream))
        ;
            MaybeHeadStream = error(_),
            search_for_file_returning_dir_and_stream_loop(AllDirs, TailDirs,
                FileName, MaybeDirNameAndStream, !IO)
        )
    ).

%---------------------%

search_for_file_returning_dir_and_contents(SearchAuthDirs, FileName,
        SearchDirs, MaybeDirPathNameAndContents, !IO) :-
    compute_search_dirs(SearchAuthDirs, SearchDirs),
    search_for_file_returning_dir_and_contents_loop(SearchDirs, SearchDirs,
        FileName, MaybeDirPathNameAndContents, !IO).

:- pred search_for_file_returning_dir_and_contents_loop(list(dir_name)::in,
    list(dir_name)::in, file_name::in,
    maybe_error(dir_name_and_contents)::out, io::di, io::uo) is det.

search_for_file_returning_dir_and_contents_loop(AllDirs, Dirs, FileName,
        MaybeDirNameAndContents, !IO) :-
    (
        Dirs = [],
        Msg = cannot_find_in_dirs_msg(FileName, AllDirs),
        MaybeDirNameAndContents = error(Msg)
    ;
        Dirs = [HeadDir | TailDirs],
        make_path_name_noncanon(HeadDir, FileName, HeadFilePathNameNC),
        io.read_named_file_as_string_wf(HeadFilePathNameNC,
            MaybeHeadContents, !IO),
        (
            MaybeHeadContents = ok(HeadContents),
            MaybeDirNameAndContents =
                ok(dir_name_and_contents(HeadDir, HeadContents))
        ;
            MaybeHeadContents = error(_),
            search_for_file_returning_dir_and_contents_loop(AllDirs, TailDirs,
                FileName, MaybeDirNameAndContents, !IO)
        )
    ).

%---------------------%

search_for_file_mod_time(SearchAuthDirs, FileName, SearchDirs, Result, !IO) :-
    compute_search_dirs(SearchAuthDirs, SearchDirs),
    search_for_file_mod_time_loop(SearchDirs, SearchDirs, FileName,
        Result, !IO).

:- pred search_for_file_mod_time_loop(list(dir_name)::in, list(dir_name)::in,
    file_name::in, maybe_error(time_t)::out, io::di, io::uo) is det.

search_for_file_mod_time_loop(AllDirs, Dirs, FileName, MaybeModTime, !IO) :-
    (
        Dirs = [],
        Msg = cannot_find_in_dirs_msg(FileName, AllDirs),
        MaybeModTime = error(Msg)
    ;
        Dirs = [HeadDir | TailDirs],
        make_path_name_noncanon(HeadDir, FileName, HeadFilePathNameNC),
        io.file.file_modification_time(HeadFilePathNameNC,
            MaybeHeadModTime, !IO),
        (
            MaybeHeadModTime = ok(HeadModTime),
            MaybeModTime = ok(HeadModTime)
        ;
            MaybeHeadModTime = error(_),
            search_for_file_mod_time_loop(AllDirs, TailDirs, FileName,
                MaybeModTime, !IO)
        )
    ).

%---------------------%

:- func cannot_find_in_dirs_msg(file_name, list(dir_name)) = string.

cannot_find_in_dirs_msg(FileName, Dirs) = Msg :-
    (
        Dirs = [],
        string.format("cannot find `%s' in the empty list of directories",
            [s(FileName)], Msg)
    ;
        Dirs = [SingleDir],
        string.format("cannot find `%s' in directory `%s'",
            [s(FileName), s(SingleDir)], Msg)
    ;
        Dirs = [_, _ | _],
        WrapInQuotes = (func(N) = "`" ++ N ++ "'"),
        DirsStr = string.join_list(", ", list.map(WrapInQuotes, Dirs)),
        string.format("cannot find `%s' in directories %s",
            [s(FileName), s(DirsStr)], Msg)
    ).

:- pred make_path_name_noncanon(dir_name::in, file_name::in, file_name::out)
    is det.

make_path_name_noncanon(Dir, FileName, PathName) :-
    ( if dir.this_directory(Dir) then
        PathName = FileName
    else
        % dir.make_path_name is slow so we avoid it when path names don't
        % need to be canonicalised.
        Sep = string.from_char(dir.directory_separator),
        PathName = string.append_list([Dir, Sep, FileName])
    ).

%---------------------------------------------------------------------------%

search_for_module_source(SearchAuthDirs, ModuleName,
        SearchDirs, MaybeFileName, !IO) :-
    search_for_module_source_and_stream(SearchAuthDirs, ModuleName,
        SearchDirs, MaybeFileNameAndStream, !IO),
    (
        MaybeFileNameAndStream =
            ok(path_name_and_stream(SourceFileName, SourceStream)),
        io.close_input(SourceStream, !IO),
        MaybeFileName = ok(SourceFileName)
    ;
        MaybeFileNameAndStream = error(Msg),
        MaybeFileName = error(Msg)
    ).

search_for_module_source_and_stream(SearchAuthDirs, ModuleName,
        SearchDirs, MaybeFileNameAndStream, !IO) :-
    module_name_to_source_file_name(ModuleName, FileName0, !IO),
    search_for_file_and_stream(SearchAuthDirs, FileName0,
        SearchDirs, MaybeFileNameAndStream0, !IO),
    (
        MaybeFileNameAndStream0 = ok(_),
        MaybeFileNameAndStream = MaybeFileNameAndStream0
    ;
        MaybeFileNameAndStream0 = error(_),
        Error = find_source_error(ModuleName, SearchDirs, no),
        MaybeFileNameAndStream = error(Error)
    ).

%------------%

:- func find_source_error(module_name, list(dir_name), maybe(file_name))
    = string.

find_source_error(ModuleName, Dirs, MaybeBetterMatch) = Msg :-
    ModuleNameStr = sym_name_to_string(ModuleName),
    WrapInQuotes = (func(N) = "`" ++ N ++ "'"),
    DirsStr = string.join_list(", ", list.map(WrapInQuotes, Dirs)),
    string.format("cannot find source for module `%s' in directories %s",
        [s(ModuleNameStr), s(DirsStr)], Msg0),
    (
        MaybeBetterMatch = no,
        Msg = Msg0
    ;
        MaybeBetterMatch = yes(BetterMatchFile),
        string.format("%s, but found `%s' in interface search path",
            [s(Msg0), s(BetterMatchFile)], Msg)
    ).

%---------------------------------------------------------------------------%

:- pred compute_search_dirs(search_auth_dirs::in, list(dir_name)::out) is det.

compute_search_dirs(SearchAuthDirs, Dirs) :-
    % NOTE This code operates on the option settings set up by
    % handle_directory_options in handle_options.m, and by the predicates
    % option_table_add_mercury_library_directory and
    % option_table_add_search_library_files_directory in options.m.
    % These set things up for searches in LEGACY installed libraries.
    % Switching over to the PROPOSED install directory structure
    % will require changes there as well as here.
    %
    % The code of check_stdlib_is_installed in check_libgrades.m also knows
    % the install structure. It should be updated to use the facilities
    % of this module to do its test, or, failing that, it will also need
    % to be updated to use the PROPOSED structure.
    (
        SearchAuthDirs = search_auth_cur_dir,
        dir.this_directory(Dir),
        Dirs = [Dir]
    ;
        SearchAuthDirs = search_auth_this_dir(Dir),
        Dirs = [Dir]
    ;
        (
            SearchAuthDirs = search_auth_cur_dir_and(TailSearchAuthDirs),
            dir.this_directory(Dir)
        ;
            SearchAuthDirs = search_auth_this_dir_and(Dir, TailSearchAuthDirs)
        ),
        compute_search_dirs(coerce(TailSearchAuthDirs), TailDirs0),
        % XXX We could replace this with just Dirs = TailDirs0.
        % The question is: should Dir always precede TailDirs0,
        % or should it be in the position given by TailSearchAuthDirs?
        % Most of the time, Dir will be at the start of TailDirs0,
        % so this may not matter, but I (zs) don't know whether
        % we can rely on that.
        ( if list.delete_first(TailDirs0, Dir, TailDirs) then
            Dirs = [Dir | TailDirs]
        else
            Dirs = [Dir | TailDirs0]
        )
    ;
        SearchAuthDirs = search_auth_private(SearchAuthPrivateDirs),
        % The switchover from the LEGACY library install structure
        % to its PROPOSED version will also be a period of switchover
        % from the old options specifying search dirs to the new options.
        % Given a program P that uses library L1, which in turn uses
        % library L2, one should switch over P first, then L1, and then L2,
        % with the general rule being if entity (program or library) E1
        % uses entity E2, then we switch over E1 first. The value we
        % assign to Dirs is designed to work for this approach.
        (
            SearchAuthPrivateDirs =
                private_auth_normal_dirs(NormalExt, Globals),
            globals.get_ext_dirs_maps(Globals, ExtDirsMaps),
            NormalDirsMap = ExtDirsMaps ^ edm_normal,
            map.lookup(NormalDirsMap, NormalExt, ProposedDirs),
            globals.lookup_accumulating_option(Globals,
                search_directories, LegacyDirs),
            Dirs = ProposedDirs ++ LegacyDirs
        ;
            SearchAuthPrivateDirs =
                private_auth_intermod_dirs(IntermodExt, Globals),
            globals.get_ext_dirs_maps(Globals, ExtDirsMaps),
            IntermodDirsMap = ExtDirsMaps ^ edm_intermod,
            map.lookup(IntermodDirsMap, IntermodExt, ProposedDirs),
            globals.lookup_accumulating_option(Globals,
                intermod_directories, LegacyDirs),
            Dirs = ProposedDirs ++ LegacyDirs
        ;
            SearchAuthPrivateDirs =
                private_auth_c_include_dirs(CInclExt, Globals),
            globals.get_ext_dirs_maps(Globals, ExtDirsMaps),
            CInclDirsMap = ExtDirsMaps ^ edm_c_incl,
            map.lookup(CInclDirsMap, CInclExt, ProposedDirs),
            globals.lookup_accumulating_option(Globals,
                c_include_directories, LegacyDirs),
            Dirs = ProposedDirs ++ LegacyDirs
        ;
            SearchAuthPrivateDirs =
                private_auth_options_file_dirs(OptionTable),
            % Since options files are never installed, the difference
            % between the Proposed vs Legacy library install directory
            % structures does not affect this kind of lookup.
            getopt.lookup_accumulating_option(OptionTable,
                options_search_directories, Dirs)
        ;
            SearchAuthPrivateDirs = private_auth_lib_dirs(LibExt, Globals),
            globals.get_ext_dirs_maps(Globals, ExtDirsMaps),
            LibDirsMap = ExtDirsMaps ^ edm_lib,
            map.lookup(LibDirsMap, LibExt, ProposedDirs),
            globals.get_grade_dir(Globals, GradeDir),
            globals.lookup_accumulating_option(Globals,
                mercury_library_directories, LibDirs),
            LegacyDirs =
                list.map((func(LibDir) = LibDir / "lib" / GradeDir), LibDirs),
            Dirs = ProposedDirs ++ LegacyDirs
        ;
            SearchAuthPrivateDirs =
                private_auth_stdlib_dirs(StdLibExt, Globals),
            globals.get_ext_dirs_maps(Globals, ExtDirsMaps),
            StdLibDirsMap = ExtDirsMaps ^ edm_stdlib,
            map.lookup(StdLibDirsMap, StdLibExt, ProposedDirs),
            (
                StdLibExt = sle_init,
                globals.lookup_accumulating_option(Globals,
                    init_file_directories, LegacyDirs)
            ;
                ( StdLibExt = sle_jar
                ; StdLibExt = sle_dll
                ),
                globals.get_grade_dir(Globals, GradeDir),
                globals.lookup_accumulating_option(Globals,
                    mercury_library_directories, LibDirs),
                LegacyDirs =
                    list.map((func(LibDir) = LibDir / "lib" / GradeDir),
                        LibDirs)
            ),
            Dirs = ProposedDirs ++ LegacyDirs
        )
    ).

%---------------------------------------------------------------------------%
:- end_module parse_tree.find_module.
%---------------------------------------------------------------------------%
