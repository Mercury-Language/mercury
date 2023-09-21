%-----------------------------------------------------------------------------e
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------e
% Copyright (C) 2014, 2016-2017, 2019-2020 The Mercury team.
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
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.

:- import_module io.
:- import_module list.
:- import_module maybe.
:- import_module time.

%---------------------------------------------------------------------------%

:- type path_name_and_stream
    --->    path_name_and_stream(string, io.text_input_stream).
            % The string may be a file name or a dir name.

:- type path_name_and_contents
    --->    path_name_and_contents(string, string).
            % The first string may be a file name or a dir name.
            % The second string is the contents of the file.

%---------------------%

    % search_for_file(Dirs, FileName, MaybeFilePathName, !IO):
    %
    % Search Dirs for FileName. If found, return the path name of the file.
    %
    % NB. Consider using search_for_file_returning_dir, which does not
    % canonicalise the path, and is therefore more efficient.
    %
:- pred search_for_file(list(dir_name)::in, file_name::in,
    maybe_error(file_name)::out, io::di, io::uo) is det.

    % search_for_file_and_stream(Dirs, FileName, MaybeFilePathNameAndStream,
    %   !IO):
    %
    % Search Dirs for FileName. If found, return the path name of the file
    % and an open input stream reading from that file. Closing that stream
    % is the caller's responsibility.
    %
    % NB. Consider using search_for_file_returning_dir_and_stream,
    % which does not canonicalise the path, and is therefore more efficient.
    %
:- pred search_for_file_and_stream(list(dir_name)::in, file_name::in,
    maybe_error(path_name_and_stream)::out, io::di, io::uo) is det.

%---------------------%

    % search_for_file_returning_dir(Dirs, FileName, MaybeDirName, !IO):
    %
    % Search Dirs for FileName. If found, return the name of the directory
    % in which the file was found.
    %
:- pred search_for_file_returning_dir(list(dir_name)::in, file_name::in,
    maybe_error(dir_name)::out, io::di, io::uo) is det.

    % search_for_file_returning_dir_and_stream(Dirs, FileName,
    %   MaybeDirNameAndStream, !IO):
    %
    % Search Dirs for FileName. If found, return the name of the directory
    % in which the file was found, and an open input stream reading
    % from that file. Closing that stream is the caller's responsibility.
    %
:- pred search_for_file_returning_dir_and_stream(list(dir_name)::in,
    file_name::in, maybe_error(path_name_and_stream)::out,
    io::di, io::uo) is det.

    % search_for_file_returning_dir_and_contents(Dirs, FileName,
    %   MaybeDirNameAndContents, !IO):
    %
    % Search Dirs for FileName. If found, return the name of the directory
    % in which the file was found, and the contents of the file as a string.
    %
:- pred search_for_file_returning_dir_and_contents(list(dir_name)::in,
    file_name::in, maybe_error(path_name_and_contents)::out,
    io::di, io::uo) is det.

%---------------------%

    % search_for_file_mod_time(Dirs, FileName, MaybeModTime, !IO)
    %
    % Search Dirs for FileName. If found, return the last modification time
    % of the file that was found. Do NOT open the file for reading.
    %
:- pred search_for_file_mod_time(list(dir_name)::in, file_name::in,
    maybe_error(time_t)::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

    % search_for_module_source(Dirs, ModuleName, FoundSourceFile, !IO):
    %
    % Look for the source for ModuleName in Dirs. If found, return the
    % (relative or absolute) path name of the source file that contains
    % the module.
    %
:- pred search_for_module_source(list(dir_name)::in, module_name::in,
    maybe_error(file_name)::out, io::di, io::uo) is det.

    % search_for_module_source_and_stream(Dirs, ModuleName,
    %   FoundSourceFileNameAndStream, !IO):
    %
    % As search_for_module_source, but if we find the file, then return
    % not just its path name, but also an open stream reading from it.
    % Closing that stream is the caller's responsibility.
    %
:- pred search_for_module_source_and_stream(list(dir_name)::in,
    module_name::in, maybe_error(path_name_and_stream)::out,
    io::di, io::uo) is det.

    % find_module_name(ErrorStream, ProgressStream, Globals,
    %  FileName, MaybeModuleName, !IO):
    %
    % Read the first item from the given file to find the module name.
    %
:- pred find_module_name(io.text_output_stream::in, globals::in,
    file_name::in, maybe(module_name)::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.error_spec.
:- import_module parse_tree.file_names.
:- import_module parse_tree.parse_module.        % for peek_at_file
:- import_module parse_tree.write_error_spec.

:- import_module dir.
:- import_module io.file.
:- import_module string.

%---------------------------------------------------------------------------%

search_for_file(Dirs, FileName, MaybeFilePathName, !IO) :-
    search_for_file_and_stream(Dirs, FileName, MaybeFilePathNameAndStream,
        !IO),
    (
        MaybeFilePathNameAndStream =
            ok(path_name_and_stream(FilePathName, Stream)),
        io.close_input(Stream, !IO),
        MaybeFilePathName = ok(FilePathName)
    ;
        MaybeFilePathNameAndStream = error(Msg),
        MaybeFilePathName = error(Msg)
    ).

search_for_file_and_stream(Dirs, FileName, MaybeFilePathNameAndStream, !IO) :-
    search_for_file_and_stream_loop(Dirs, Dirs, FileName,
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

search_for_file_returning_dir(Dirs, FileName, MaybeDirPathName, !IO) :-
    search_for_file_returning_dir_and_stream(Dirs, FileName,
        MaybeDirPathNameAndStream, !IO),
    (
        MaybeDirPathNameAndStream =
            ok(path_name_and_stream(DirPathName, Stream)),
        io.close_input(Stream, !IO),
        MaybeDirPathName = ok(DirPathName)
    ;
        MaybeDirPathNameAndStream = error(Msg),
        MaybeDirPathName = error(Msg)
    ).

search_for_file_returning_dir_and_stream(Dirs, FileName,
        MaybeDirPathNameAndStream, !IO) :-
    search_for_file_returning_dir_and_stream_loop(Dirs, Dirs, FileName,
        MaybeDirPathNameAndStream, !IO).

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

search_for_file_returning_dir_and_contents(Dirs, FileName,
        MaybeDirPathNameAndContents, !IO) :-
    search_for_file_returning_dir_and_contents_loop(Dirs, Dirs, FileName,
        MaybeDirPathNameAndContents, !IO).

:- pred search_for_file_returning_dir_and_contents_loop(list(dir_name)::in,
    list(dir_name)::in, file_name::in, maybe_error(path_name_and_contents)::out,
    io::di, io::uo) is det.

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
                ok(path_name_and_contents(HeadDir, HeadContents))
        ;
            MaybeHeadContents = error(_),
            search_for_file_returning_dir_and_contents_loop(AllDirs, TailDirs,
                FileName, MaybeDirNameAndContents, !IO)
        )
    ).

%---------------------%

search_for_file_mod_time(Dirs, FileName, Result, !IO) :-
    search_for_file_mod_time_loop(Dirs, Dirs, FileName, Result, !IO).

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

search_for_module_source(Dirs, ModuleName, MaybeFileName, !IO) :-
    search_for_module_source_and_stream(Dirs, ModuleName,
        MaybeFileNameAndStream, !IO),
    (
        MaybeFileNameAndStream =
            ok(path_name_and_stream(SourceFileName, SourceStream)),
        io.close_input(SourceStream, !IO),
        MaybeFileName = ok(SourceFileName)
    ;
        MaybeFileNameAndStream = error(Msg),
        MaybeFileName = error(Msg)
    ).

search_for_module_source_and_stream(Dirs, ModuleName,
        MaybeFileNameAndStream, !IO) :-
    module_name_to_source_file_name(ModuleName, FileName0, !IO),
    search_for_file_and_stream(Dirs, FileName0, MaybeFileNameAndStream0, !IO),
    (
        MaybeFileNameAndStream0 = ok(_),
        MaybeFileNameAndStream = MaybeFileNameAndStream0
    ;
        MaybeFileNameAndStream0 = error(_),
        Error = find_source_error(ModuleName, Dirs, no),
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

find_module_name(ProgressStream, Globals, FileName, MaybeModuleName, !IO) :-
    io.open_input(FileName, OpenRes, !IO),
    (
        OpenRes = ok(FileStream),
        ( if string.remove_suffix(FileName, ".m", PartialFileName0) then
            PartialFileName = PartialFileName0
        else
            PartialFileName = FileName
        ),
        ( if dir.basename(PartialFileName, BaseName0) then
            BaseName = BaseName0
        else
            BaseName = ""
        ),
        file_name_to_module_name(BaseName, DefaultModuleName),
        peek_at_file(FileStream, DefaultModuleName, [], FileName, ModuleName,
            Specs, !IO),
        io.close_input(FileStream, !IO),
        MaybeModuleName = yes(ModuleName),
        % XXX We don't check whether ModuleName was actually read
        % from the named file; it could just be DefaultModuleName.
        write_error_specs(ProgressStream, Globals, Specs, !IO)
    ;
        OpenRes = error(Error),
        ErrorMsg = io.error_message(Error),
        io.progname_base("mercury_compile", Progname, !IO),
        Pieces = [fixed(Progname), suffix(":"), words("error opening"),
            quote(FileName), suffix(":"), words(ErrorMsg), suffix("."), nl],
        Spec = simplest_no_context_spec($pred, severity_error,
            phase_read_files, Pieces),
        % XXX Should return maybe1(module_name), not maybe(module_name).
        write_error_spec(ProgressStream, Globals, Spec, !IO),
        MaybeModuleName = no
    ).

%---------------------------------------------------------------------------%
:- end_module parse_tree.find_module.
%---------------------------------------------------------------------------%
