%-----------------------------------------------------------------------------e
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------e
% Copyright (C) 2008 The University of Melbourne.
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

:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module maybe.

%-----------------------------------------------------------------------------%

:- type file_name == string.
:- type dir_name == string.

    % Open a source or interface file, returning `ok(FileInfo)' on success
    % (where FileInfo is information about the file such as the file name
    % or the directory in which it was found), or `error(Message)' on failure.
:- type open_file(FileInfo) == pred(maybe_error(FileInfo), io, io).
:- inst open_file == (pred(out, di, uo) is det).

    % search_for_file(Dirs, FileName, FoundFileName, !IO):
    %
    % Search Dirs for FileName, opening the file if it is found,
    % and returning the path name of the file that was found.
    %
:- pred search_for_file(list(dir_name)::in, file_name::in,
    maybe_error(file_name)::out, io::di, io::uo) is det.

    % search_for_file_returning_dir(Dirs, FileName, FoundDirName, !IO):
    %
    % Search Dirs for FileName, opening the file if it is found, and returning
    % the name of the directory in which the file was found.
    %
:- pred search_for_file_returning_dir(list(dir_name)::in, file_name::in,
    maybe_error(dir_name)::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

    % Write to a given filename, giving appropriate status messages
    % and error messages if the file cannot be opened.
    %
:- pred output_to_file(string::in, pred(io, io)::in(pred(di, uo) is det),
    io::di, io::uo) is det.

    % Same as output_to_file/4 above, but allow the writing predicate
    % to generate something, and if it succeeds, return its result.
    %
:- pred output_to_file_return_result(string::in,
    pred(T, io, io)::in(pred(out, di, uo) is det),
    maybe(T)::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

    % get_install_name_option(FileName, Option, !IO):
    %
    % Get the option string for setting the install-name of the shared library
    % FileName. This is only used for systems which support the install-name
    % option for shared libraries (such as Darwin).
    %
:- pred get_install_name_option(string::in, string::out, io::di, io::uo)
    is det.

%-----------------------------------------------------------------------------%

:- pred maybe_report_stats(bool::in, io::di, io::uo) is det.
:- pred maybe_write_string(bool::in, string::in, io::di, io::uo) is det.
:- pred maybe_flush_output(bool::in, io::di, io::uo) is det.

:- pred report_error(string::in, io::di, io::uo) is det.
:- pred report_error_to_stream(io.output_stream::in, string::in,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module libs.compiler_util.
:- import_module libs.globals.
:- import_module libs.handle_options.
:- import_module libs.options.

:- import_module dir.
:- import_module string.

%-----------------------------------------------------------------------------%

search_for_file(Dirs, FileName, Result, !IO) :-
    search_for_file_returning_dir(Dirs, FileName, Result0, !IO),
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

search_for_file_returning_dir(Dirs, FileName, Result, !IO) :-
    search_for_file_returning_dir_2(Dirs, FileName, MaybeDir, !IO),
    (
        MaybeDir = yes(Dir),
        Result = ok(Dir)
    ;
        MaybeDir = no,
        Msg = "cannot find `" ++ FileName ++ "' in directories " ++
            string.join_list(", ", Dirs),
        Result = error(Msg)
    ).

:- pred search_for_file_returning_dir_2(list(dir_name)::in,
    file_name::in, maybe(dir_name)::out, io::di, io::uo) is det.

search_for_file_returning_dir_2([], _FileName, no, !IO).
search_for_file_returning_dir_2([Dir | Dirs], FileName, MaybeDir, !IO) :-
    ( dir.this_directory(Dir) ->
        ThisFileName = FileName
    ;
        ThisFileName = dir.make_path_name(Dir, FileName)
    ),
    io.see(ThisFileName, SeeResult0, !IO),
    (
        SeeResult0 = ok,
        MaybeDir = yes(Dir)
    ;
        SeeResult0 = error(_),
        search_for_file_returning_dir_2(Dirs, FileName, MaybeDir, !IO)
    ).

%-----------------------------------------------------------------------------%

output_to_file(FileName, Action, !IO) :-
    NewAction = (pred(0::out, di, uo) is det --> Action),
    output_to_file_return_result(FileName, NewAction, _Result, !IO).

output_to_file_return_result(FileName, Action, Result, !IO) :-
    globals.io_lookup_bool_option(verbose, Verbose, !IO),
    globals.io_lookup_bool_option(statistics, Stats, !IO),
    maybe_write_string(Verbose, "% Writing to file `", !IO),
    maybe_write_string(Verbose, FileName, !IO),
    maybe_write_string(Verbose, "'...\n", !IO),
    maybe_flush_output(Verbose, !IO),
    io.open_output(FileName, Res, !IO),
    (
        Res = ok(FileStream),
        io.set_output_stream(FileStream, OutputStream, !IO),
        Action(ActionResult, !IO),
        io.set_output_stream(OutputStream, _, !IO),
        io.close_output(FileStream, !IO),
        maybe_write_string(Verbose, "% done.\n", !IO),
        maybe_report_stats(Stats, !IO),
        Result = yes(ActionResult)
    ;
        Res = error(_),
        maybe_write_string(Verbose, "\n", !IO),
        string.append_list(["can't open file `", FileName, "' for output."],
            ErrorMessage),
        report_error(ErrorMessage, !IO),
        Result = no
    ).

%-----------------------------------------------------------------------------%

% Changes to the following predicate may require similar changes to
% make.program_target.install_library_grade_files/9.

get_install_name_option(OutputFileName, InstallNameOpt, !IO) :-
    globals.io_get_globals(Globals, !IO),
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
    InstallNameOpt = InstallNameFlag ++ InstallNamePath / OutputFileName.

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

:- func this_file = string.

this_file = "file_util.m".

%-----------------------------------------------------------------------------%
:- end_module libs.file_util.
%-----------------------------------------------------------------------------%
