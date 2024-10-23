%-----------------------------------------------------------------------------e
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------e
% Copyright (C) 2014, 2016-2017, 2019-2020 The Mercury team.
% Copyright (C) 2016-2017, 2019-2020, 2022-2024 The Mercury team.
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

:- type search_which_dirs
    --->    search_cur_dir
    ;       search_this_dir(dir_name)
    ;       search_this_dir_and(dir_name, search_which_tail_dirs)
    ;       search_normal_dirs(option_table)
    ;       search_intermod_dirs(option_table)
    ;       search_init_file_dirs(option_table)
    ;       search_c_include_dirs(option_table)
    ;       search_options_file_dirs(option_table)
    ;       search_mercury_library_dirs(globals).

    % This type differs from search_which_dirs by not including
    % search_this_dir_and, thus limiting recursion to one level.
:- type search_which_tail_dirs =< search_which_dirs
    --->    search_cur_dir
    ;       search_this_dir(dir_name)
    ;       search_normal_dirs(option_table)
    ;       search_intermod_dirs(option_table)
    ;       search_init_file_dirs(option_table)
    ;       search_c_include_dirs(option_table)
    ;       search_options_file_dirs(option_table)
    ;       search_mercury_library_dirs(globals).

%---------------------%

    % search_for_file(SearchWhichDirs, FileName,
    %   SearchDirs, MaybeFilePathName, !IO):
    %
    % Search the specified directories for FileName. If found,
    % return the path name of the file. We also return the list of directories
    % we searched as SearchDirs, for possible use in error messages.
    %
    % NB. Consider using search_for_file_returning_dir, which does not
    % canonicalise the path, and is therefore more efficient.
    %
:- pred search_for_file(search_which_dirs::in, file_name::in,
    list(dir_name)::out, maybe_error(file_name)::out, io::di, io::uo) is det.

    % search_for_file_and_stream(SearchWhichDirs, FileName,
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
:- pred search_for_file_and_stream(search_which_dirs::in, file_name::in,
    list(dir_name)::out, maybe_error(path_name_and_stream)::out,
    io::di, io::uo) is det.

%---------------------%

    % search_for_file_returning_dir(SearchWhichDirs, FileName,
    %   SearchDirs, MaybeDirName, !IO):
    %
    % Search the specified directories for FileName. If found,
    % return the name of the directory in which the file was found.
    %
:- pred search_for_file_returning_dir(search_which_dirs::in, file_name::in,
    list(dir_name)::out, maybe_error(dir_name)::out, io::di, io::uo) is det.

    % search_for_file_returning_dir_and_stream(SearchWhichDirs, FileName,
    %   SearchDirs, MaybeDirNameAndStream, !IO):
    %
    % Search the specified directory for FileName. If found,
    % return the name of the directory in which the file was found,
    % and an open input stream reading from that file. Closing that stream
    % is the caller's responsibility.
    %
:- pred search_for_file_returning_dir_and_stream(search_which_dirs::in,
    file_name::in, list(dir_name)::out,
    maybe_error(path_name_and_stream)::out, io::di, io::uo) is det.

    % search_for_file_returning_dir_and_contents(SearchWhichDirs, FileName,
    %   SearchDirs, MaybeDirNameAndContents, !IO):
    %
    % Search the specified directories for FileName. If found,
    % return the name of the directory in which the file was found,
    % and the contents of the file as a string.
    %
:- pred search_for_file_returning_dir_and_contents(search_which_dirs::in,
    file_name::in, list(dir_name)::out,
    maybe_error(dir_name_and_contents)::out, io::di, io::uo) is det.

%---------------------%

    % search_for_file_mod_time(SearchWhichDirs, FileName,
    %   SearchDirs, MaybeModTime, !IO)
    %
    % Search the specified directories for FileName. If found,
    % return the last modification time of the file that was found.
    % Do NOT open the file for reading.
    %
:- pred search_for_file_mod_time(search_which_dirs::in, file_name::in,
    list(dir_name)::out, maybe_error(time_t)::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

    % search_for_module_source(SearchWhichDirs, ModuleName,
    %   SearchDirs, FoundSourceFile, !IO):
    %
    % Search the specified directories for the source code of ModuleName.
    % If found, return the (relative or absolute) path name of the
    % source file that contains the module.
    %
:- pred search_for_module_source(search_which_dirs::in, module_name::in,
    list(dir_name)::out, maybe_error(file_name)::out, io::di, io::uo) is det.

    % search_for_module_source_and_stream(SearchWhichDirs, ModuleName,
    %   SearchDirs, FoundSourceFileNameAndStream, !IO):
    %
    % As search_for_module_source, but if we find the file, then return
    % not just its path name, but also an open stream reading from it.
    % Closing that stream is the caller's responsibility.
    %
:- pred search_for_module_source_and_stream(search_which_dirs::in,
    module_name::in, list(dir_name)::out,
    maybe_error(path_name_and_stream)::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.file_names.

:- import_module dir.
:- import_module getopt.
:- import_module io.file.
:- import_module string.

%---------------------------------------------------------------------------%

search_for_file(SearchWhichDirs, FileName,
        SearchDirs, MaybeFilePathName, !IO) :-
    search_for_file_and_stream(SearchWhichDirs, FileName,
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

search_for_file_and_stream(SearchWhichDirs, FileName,
        SearchDirs, MaybeFilePathNameAndStream, !IO) :-
    compute_search_dirs(SearchWhichDirs, SearchDirs),
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

search_for_file_returning_dir(SearchWhichDirs, FileName,
        SearchDirs, MaybeDirPathName, !IO) :-
    search_for_file_returning_dir_and_stream(SearchWhichDirs, FileName,
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

search_for_file_returning_dir_and_stream(SearchWhichDirs, FileName,
        SearchDirs, MaybeDirPathNameAndStream, !IO) :-
    compute_search_dirs(SearchWhichDirs, SearchDirs),
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

search_for_file_returning_dir_and_contents(SearchWhichDirs, FileName,
        SearchDirs, MaybeDirPathNameAndContents, !IO) :-
    compute_search_dirs(SearchWhichDirs, SearchDirs),
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

search_for_file_mod_time(SearchWhichDirs, FileName, SearchDirs, Result, !IO) :-
    compute_search_dirs(SearchWhichDirs, SearchDirs),
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

search_for_module_source(SearchWhichDirs, ModuleName,
        SearchDirs, MaybeFileName, !IO) :-
    search_for_module_source_and_stream(SearchWhichDirs, ModuleName,
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

search_for_module_source_and_stream(SearchWhichDirs, ModuleName,
        SearchDirs, MaybeFileNameAndStream, !IO) :-
    module_name_to_source_file_name(ModuleName, FileName0, !IO),
    search_for_file_and_stream(SearchWhichDirs, FileName0,
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

:- pred compute_search_dirs(search_which_dirs::in, list(dir_name)::out) is det.

compute_search_dirs(SearchWhich, Dirs) :-
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
        SearchWhich = search_cur_dir,
        dir.this_directory(CurDir),
        Dirs = [CurDir]
    ;
        SearchWhich = search_this_dir(Dir),
        Dirs = [Dir]
    ;
        SearchWhich = search_this_dir_and(Dir, TailSearchWhich),
        compute_search_dirs(coerce(TailSearchWhich), TailDirs),
        Dirs = [Dir | TailDirs]
    ;
        SearchWhich = search_normal_dirs(OptionTable),
        getopt.lookup_accumulating_option(OptionTable,
            search_directories, Dirs)
    ;
        SearchWhich = search_intermod_dirs(OptionTable),
        getopt.lookup_accumulating_option(OptionTable,
            intermod_directories, Dirs)
    ;
        SearchWhich = search_init_file_dirs(OptionTable),
        getopt.lookup_accumulating_option(OptionTable,
            init_file_directories, Dirs)
    ;
        SearchWhich = search_c_include_dirs(OptionTable),
        getopt.lookup_accumulating_option(OptionTable,
            c_include_directories, Dirs)
    ;
        SearchWhich = search_options_file_dirs(OptionTable),
        getopt.lookup_accumulating_option(OptionTable,
            options_search_directories, Dirs)
    ;
        SearchWhich = search_mercury_library_dirs(Globals),
        globals.get_options(Globals, OptionTable),
        getopt.lookup_accumulating_option(OptionTable,
            mercury_library_directories, LibDirs),
        globals.get_grade_dir(Globals, GradeDir),
        Dirs = list.map((func(LibDir) = LibDir / "lib" / GradeDir), LibDirs)
    ).

%---------------------------------------------------------------------------%
:- end_module parse_tree.find_module.
%---------------------------------------------------------------------------%
