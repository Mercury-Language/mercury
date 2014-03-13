%-----------------------------------------------------------------------------e
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------e
% Copyright (C) 2008-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: file_util.m.
%
% Utility predicates for operating on files that do not require any access
% to the parse_tree package or anything above it.
%
%-----------------------------------------------------------------------------%

:- module libs.file_util.
:- interface.

:- import_module libs.globals.

:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module maybe.
:- import_module time.

%-----------------------------------------------------------------------------%

:- type file_name == string.
:- type dir_name == string.

    % Open a source or interface file, returning `ok(FileInfo)' on success
    % (where FileInfo is information about the file such as the file name
    % or the directory in which it was found), or `error(Message)' on failure.
:- type open_file_pred(FileInfo) == pred(maybe_error(FileInfo), io, io).
:- inst open_file_pred == (pred(out, di, uo) is det).

:- type maybe_open_file
    --->    open_file
    ;       do_not_open_file.

    % search_for_file(MaybeOpen, Dirs, FileName, FoundFileName, !IO):
    %
    % Search Dirs for FileName, returning the path name of the file that was
    % found.  If requested, the found file will be left
    % open as the current input stream.
    %
    % NB. Consider using search_for_file_returning_dir, which does not
    % canonicalise the path so is more efficient.
    %
:- pred search_for_file(maybe_open_file::in, list(dir_name)::in, file_name::in,
    maybe_error(file_name)::out, io::di, io::uo) is det.

    % search_for_file_returning_dir(MaybeOpen, Dirs, FileName, FoundDirName,
    %   !IO):
    %
    % Search Dirs for FileName, returning the name of the directory in
    % which the file was found.  If requested, the found file will be left
    % open as the current input stream.
    %
:- pred search_for_file_returning_dir(maybe_open_file::in, list(dir_name)::in,
    file_name::in, maybe_error(dir_name)::out, io::di, io::uo) is det.

    % search_for_file_mod_time(Dirs, FileName, FoundModTime, !IO)
    %
    % Search Dirs for FileName, returning the last modification time of the
    % file that was found.  Does NOT open the file for reading.
    %
:- pred search_for_file_mod_time(list(dir_name)::in, file_name::in,
    maybe_error(time_t)::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

    % Write to a given filename, giving appropriate status messages
    % and error messages if the file cannot be opened.
    % This will catch and report include_file_error exceptions.
    %
:- pred output_to_file(globals::in, string::in,
    pred(io, io)::in(pred(di, uo) is det), bool::out, io::di, io::uo) is det.

    % Same as output_to_file above, but allow the writing predicate
    % to generate something, and if it succeeds, return its result.
    %
:- pred output_to_file_return_result(globals::in, string::in,
    pred(T, io, io)::in(pred(out, di, uo) is det),
    maybe(T)::out, io::di, io::uo) is det.

    % Write the contents of the given file into the current output stream.
    % Throws include_file_error exceptions for errors relating to the
    % include file.
    %
:- pred write_include_file_contents(string::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

    % get_install_name_option(FileName, Option, !IO):
    %
    % Get the option string for setting the install-name of the shared library
    % FileName. This is only used for systems which support the install-name
    % option for shared libraries (such as Darwin).
    %
:- pred get_install_name_option(globals::in, string::in, string::out) is det.

%-----------------------------------------------------------------------------%

:- pred maybe_report_stats(bool::in, io::di, io::uo) is det.
:- pred maybe_write_string(bool::in, string::in, io::di, io::uo) is det.
:- pred maybe_flush_output(bool::in, io::di, io::uo) is det.

:- pred report_error(string::in, io::di, io::uo) is det.
:- pred report_error_to_stream(io.output_stream::in, string::in,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

    % make_install_command(Globals, FileName, InstallDir) = Command:
    % Command is the command required to install file FileName in directory
    % InstallDir.
    %
:- func make_install_file_command(globals, string, string) = string.

    % make_install_dir_command(Globals, SourceDirName, InstallDir) = Command:
    % Command is the command required to install directory SourceDirName
    % in directory InstallDir.
    %
:- func make_install_dir_command(globals, string, string) = string.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module libs.handle_options.
:- import_module libs.options.

:- import_module dir.
:- import_module exception.
:- import_module string.
:- import_module univ.

:- type include_file_error
    --->    include_file_error(string, string).

%-----------------------------------------------------------------------------%

search_for_file(MaybeOpen, Dirs, FileName, Result, !IO) :-
    search_for_file_returning_dir(MaybeOpen, Dirs, FileName, Result0, !IO),
    (
        Result0 = ok(Dir),
        ( dir.this_directory(Dir) ->
            PathName = FileName
        ;
            PathName = dir.make_path_name(Dir, FileName)
        ),
        Result = ok(PathName)
    ;
        Result0 = error(Message),
        Result = error(Message)
    ).

search_for_file_returning_dir(MaybeOpen, Dirs, FileName, Result, !IO) :-
    do_search_for_file(check_file_return_dir(MaybeOpen), Dirs, FileName,
        MaybeDir, !IO),
    (
        MaybeDir = yes(Dir),
        Result = ok(Dir)
    ;
        MaybeDir = no,
        Msg = "cannot find `" ++ FileName ++ "' in directories " ++
            string.join_list(", ", Dirs),
        Result = error(Msg)
    ).

:- pred check_file_return_dir(maybe_open_file::in, dir_name::in, file_name::in,
    io.res(dir_name)::out, io::di, io::uo) is det.

check_file_return_dir(MaybeOpen, Dir, FileName, Result, !IO) :-
    make_path_name_noncanon(Dir, FileName, PathName),
    io.open_input(PathName, OpenResult, !IO),
    (
        OpenResult = ok(Stream),
        (
            MaybeOpen = open_file,
            io.set_input_stream(Stream, _, !IO)
        ;
            MaybeOpen = do_not_open_file,
            io.close_input(Stream, !IO)
        ),
        Result = ok(Dir)
    ;
        OpenResult = error(Error),
        Result = error(Error)
    ).

search_for_file_mod_time(Dirs, FileName, Result, !IO) :-
    do_search_for_file(check_file_mod_time, Dirs, FileName, MaybeTime, !IO),
    (
        MaybeTime = yes(Time),
        Result = ok(Time)
    ;
        MaybeTime = no,
        Msg = "cannot find `" ++ FileName ++ "' in directories " ++
            string.join_list(", ", Dirs),
        Result = error(Msg)
    ).

:- pred check_file_mod_time(dir_name::in, file_name::in, io.res(time_t)::out,
    io::di, io::uo) is det.

check_file_mod_time(Dir, FileName, Result, !IO) :-
    make_path_name_noncanon(Dir, FileName, PathName),
    io.file_modification_time(PathName, Result, !IO).

:- pred do_search_for_file(
    pred(dir_name, file_name, io.res(T), io, io)
        ::in(pred(in, in, out, di, uo) is det),
    list(dir_name)::in, file_name::in, maybe(T)::out, io::di, io::uo) is det.

do_search_for_file(_P, [], _FileName, no, !IO).
do_search_for_file(P, [Dir | Dirs], FileName, Result, !IO) :-
    P(Dir, FileName, Result0, !IO),
    (
        Result0 = ok(TimeT),
        Result = yes(TimeT)
    ;
        Result0 = error(_),
        do_search_for_file(P, Dirs, FileName, Result, !IO)
    ).

:- pred make_path_name_noncanon(dir_name::in, file_name::in, file_name::out)
    is det.

make_path_name_noncanon(Dir, FileName, PathName) :-
    ( dir.this_directory(Dir) ->
        PathName = FileName
    ;
        % dir.make_path_name is slow so we avoid it when path names don't
        % need to be canonicalised.
        Sep = string.from_char(dir.directory_separator),
        PathName = string.append_list([Dir, Sep, FileName])
    ).

%-----------------------------------------------------------------------------%

output_to_file(Globals, FileName, Action, Succeeded, !IO) :-
    NewAction = (pred(0::out, di, uo) is det --> Action),
    output_to_file_return_result(Globals, FileName, NewAction, Result, !IO),
    (
        Result = yes(_),
        Succeeded = yes
    ;
        Result = no,
        Succeeded = no
    ).

output_to_file_return_result(Globals, FileName, Action, Result, !IO) :-
    globals.lookup_bool_option(Globals, verbose, Verbose),
    globals.lookup_bool_option(Globals, statistics, Stats),
    maybe_write_string(Verbose, "% Writing to file `", !IO),
    maybe_write_string(Verbose, FileName, !IO),
    maybe_write_string(Verbose, "'...\n", !IO),
    maybe_flush_output(Verbose, !IO),
    io.open_output(FileName, Res, !IO),
    (
        Res = ok(FileStream),
        io.set_output_stream(FileStream, OrigOutputStream, !IO),
        promise_equivalent_solutions [TryResult, !:IO] (
            try_io(Action, TryResult, !IO)
        ),
        io.set_output_stream(OrigOutputStream, _, !IO),
        io.close_output(FileStream, !IO),
        maybe_write_string(Verbose, "% done.\n", !IO),
        maybe_report_stats(Stats, !IO),
        (
            TryResult = succeeded(ActionResult),
            Result = yes(ActionResult)
        ;
            TryResult = exception(Univ),
            ( univ_to_type(Univ, IncludeError) ->
                IncludeError = include_file_error(IncludeFileName, Detail),
                string.format("can't open `%s' for input: %s",
                    [s(IncludeFileName), s(Detail)], ErrorMessage),
                maybe_write_string(Verbose, "\n", !IO),
                report_error(ErrorMessage, !IO),
                Result = no
            ;
                rethrow(TryResult)
            )
        )
    ;
        Res = error(_),
        maybe_write_string(Verbose, "\n", !IO),
        string.append_list(["can't open file `", FileName, "' for output."],
            ErrorMessage),
        report_error(ErrorMessage, !IO),
        Result = no
    ).

%-----------------------------------------------------------------------------%

write_include_file_contents(FileName, !IO) :-
    FollowSymLinks = yes,
    io.file_type(FollowSymLinks, FileName, MaybeType, !IO),
    (
        MaybeType = ok(Type),
        ( possibly_regular_file(Type) ->
            io.output_stream(OutputStream, !IO),
            write_include_file_contents_2(OutputStream, FileName, !IO)
        ;
            throw(include_file_error(FileName, "Not a regular file"))
        )
    ;
        MaybeType = error(Error),
        Msg = string.remove_prefix_if_present("io.file_type failed: ",
            io.error_message(Error)),
        throw(include_file_error(FileName, Msg))
    ).

:- pred write_include_file_contents_2(io.output_stream::in, string::in,
    io::di, io::uo) is det.

write_include_file_contents_2(OutputStream, FileName, !IO) :-
    io.open_input(FileName, OpenRes, !IO),
    (
        OpenRes = ok(InputStream),
        promise_equivalent_solutions [TryResult, !:IO] (
            try_io(copy_stream(OutputStream, InputStream), TryResult, !IO)
        ),
        io.close_input(InputStream, !IO),
        (
            TryResult = succeeded(ok)
        ;
            TryResult = succeeded(error(Error)),
            throw(Error)
        ;
            TryResult = exception(_),
            rethrow(TryResult)
        )
    ;
        OpenRes = error(Error),
        throw(include_file_error(FileName, io.error_message(Error)))
    ).

:- pred copy_stream(io.output_stream::in,
    io.input_stream::in, io.res::out, io::di, io::uo) is det.

copy_stream(OutputStream, InputStream, Res, !IO) :-
    io.read_file_as_string(InputStream, ReadRes, !IO),
    (
        ReadRes = ok(InputContents),
        io.write_string(OutputStream, InputContents, !IO),
        Res = ok
    ;
        ReadRes = error(_Partial, Error),
        Res = error(Error)
    ).

:- pred possibly_regular_file(io.file_type::in) is semidet.

possibly_regular_file(regular_file).
possibly_regular_file(unknown).

%-----------------------------------------------------------------------------%

% Changes to the following predicate may require similar changes to
% make.program_target.install_library_grade_files/9.

get_install_name_option(Globals, OutputFileName, InstallNameOpt) :-
    globals.lookup_string_option(Globals, shlib_linker_install_name_flag,
        InstallNameFlag),
    globals.lookup_string_option(Globals, shlib_linker_install_name_path,
        InstallNamePath0),
    ( InstallNamePath0 = "" ->
        globals.lookup_string_option(Globals, install_prefix, InstallPrefix),
        grade_directory_component(Globals, GradeDir),
        InstallNamePath = InstallPrefix / "lib" / "mercury" / "lib" / GradeDir
    ;
        InstallNamePath = InstallNamePath0
    ),
    InstallNameOpt = InstallNameFlag ++
        quote_arg(InstallNamePath) / OutputFileName.

%-----------------------------------------------------------------------------%

maybe_report_stats(yes, !IO) :-
    io.report_stats(!IO).
maybe_report_stats(no, !IO).

maybe_write_string(yes, String, !IO) :-
    io.write_string(String, !IO).
maybe_write_string(no, _, !IO).

maybe_flush_output(yes, !IO) :-
    io.flush_output(!IO).
maybe_flush_output(no, !IO).

report_error(ErrorMessage, !IO) :-
    io.write_string("Error: ", !IO),
    io.write_string(ErrorMessage, !IO),
    io.write_string("\n", !IO),
    io.set_exit_status(1, !IO).

report_error_to_stream(Stream, ErrorMessage, !IO) :-
    io.set_output_stream(Stream, OldStream, !IO),
    report_error(ErrorMessage, !IO),
    io.set_output_stream(OldStream, _, !IO).

%-----------------------------------------------------------------------------%

make_install_file_command(Globals, FileName, InstallDir) = Command :-
    globals.get_file_install_cmd(Globals, FileInstallCmd),
    (
        FileInstallCmd = install_cmd_user(InstallCmd, _InstallCmdDirOpt)
    ;
        FileInstallCmd = install_cmd_cp,
        InstallCmd = "cp"
    ),
    Command = string.join_list("   ", list.map(quote_arg,
        [InstallCmd, FileName, InstallDir])).

make_install_dir_command(Globals, SourceDirName, InstallDir) = Command :-
    globals.get_file_install_cmd(Globals, FileInstallCmd),
    (
        FileInstallCmd = install_cmd_user(InstallCmd, InstallCmdDirOpt)
    ;
        FileInstallCmd = install_cmd_cp,
        InstallCmd = "cp",
        % XXX the POSIX option is -R but for some reason the default in
        % options.m is -r.
        InstallCmdDirOpt = "-R"
    ),
    Command = string.join_list("   ", list.map(quote_arg,
        [InstallCmd, InstallCmdDirOpt, SourceDirName, InstallDir])).
        
%-----------------------------------------------------------------------------%
:- end_module libs.file_util.
%-----------------------------------------------------------------------------%
