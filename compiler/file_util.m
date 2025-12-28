%-----------------------------------------------------------------------------e
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------e
% Copyright (C) 2008-2011 The University of Melbourne.
% Copyright (C) 2013-2015, 2018, 2020-2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: file_util.m.
%
% Utility predicates for operating on files that do not require any access
% to the parse_tree package or anything above it.
%
%---------------------------------------------------------------------------%

:- module libs.file_util.
:- interface.

:- import_module libs.globals.
:- import_module libs.maybe_util.

:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module maybe.

%---------------------------------------------------------------------------%

:- type file_name == string.
:- type dir_name == string.

%---------------------------------------------------------------------------%

    % write_string_to_file(ProgressStream, Globals, FileNameMsg,
    %   FileName, FileContentsStr, Succeeded, !IO)
    %
    % Write FileContentsStr to FileName. If the verbose option is enabled
    % in Globals, then print progress messages as we go along. The initial
    % progress message will start with FileNameMsg.
    %
:- pred write_string_to_file(io.text_output_stream::in, globals::in,
    string::in, file_name::in, string::in, maybe_succeeded::out,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%

    % Write to a given filename, giving appropriate status messages
    % and error messages if the file cannot be opened.
    %
:- pred output_to_file_stream(io.text_output_stream::in, globals::in,
    string::in,
    pred(io.text_output_stream, list(string), io, io)::
        in(pred(in, out, di, uo) is det),
    maybe_succeeded::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

    % Write the contents of the given file to the specified output stream.
    %
:- pred write_include_file_contents(io.text_output_stream::in, globals::in,
    string::in, maybe_error::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

    % get_install_name_option(FileName, Option, !IO):
    %
    % Get the option string for setting the install-name of the shared library
    % FileName. This is only used for systems which support the install-name
    % option for shared libraries (such as Darwin).
    %
:- pred get_install_name_option(globals::in, string::in, string::out) is det.

    % get_std_grade_specific_install_dir(Globals, GradeDir,
    %   GradeSpecificPathName):
    %
    % Return the full pathname of the grade-specific install directory,
    % i.e. the directory in which grade-specific files should be installed.
    %
    % The GradeDir argument should be computed by calling
    % grade_directory_component(Globals, GradeDir). We take it as an argument
    % instead of computing it inside this predicate because our caller
    % is likely to have made that call already for other purposes.
    %
:- pred get_std_grade_specific_install_lib_dir(globals::in, string::in,
    string::out) is det.

%---------------------------------------------------------------------------%

:- pred maybe_report_stats(io.text_output_stream::in, bool::in,
    io::di, io::uo) is det.
:- pred maybe_report_stats_to_stream(maybe(io.text_output_stream)::in,
    io::di, io::uo) is det.

:- pred maybe_write_string(io.text_output_stream::in, bool::in, string::in,
    io::di, io::uo) is det.
:- pred maybe_write_string_to_stream(maybe(io.text_output_stream)::in,
    string::in, io::di, io::uo) is det.

:- pred maybe_flush_output(io.text_output_stream::in, bool::in,
    io::di, io::uo) is det.
:- pred maybe_flush_output_to_stream(maybe(io.text_output_stream)::in,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%

    % msg_for_cannot_open_file_for_input(Globals, FileName, ErrorMsg):
    % msg_for_cannot_open_file_for_output(Globals, FileName, ErrorMsg):
    %
    % Return the error message we should print for when an attempt
    % to open FileName for input or output results in the error
    % described by ErrorMsg.
    %
    % If the canonicalize_error_path_names option is given, then
    % delete the parts of path names in error messages that cause
    % unnecessary differences between .err and .err_exp* files
    % in our test suite.
    %
:- func msg_for_cannot_open_file_for_input(globals, file_name, string)
    = string.
:- func msg_for_cannot_open_file_for_output(globals, file_name, string)
    = string.

    % Print a report to the specified stream about not being able
    % to open the named file. Set the exit status to indicate
    % an error exit.
    %
:- pred report_cannot_open_file_for_input(io.text_output_stream::in,
    globals::in, file_name::in, io.error::in, io::di, io::uo) is det.
:- pred report_cannot_open_file_for_output(io.text_output_stream::in,
    globals::in, file_name::in, io.error::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- pred report_arbitrary_error(io.text_output_stream::in, string::in,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%

    % make_install_file_command(Globals, FileName, InstallDir) = Command:
    % Command is the command required to install file FileName in directory
    % InstallDir.
    %
:- func make_install_file_command(globals, string, string) = string.

%---------------------------------------------------------------------------%

    % open_temp_output(Dir, Prefix, Suffix, Result, !IO):
    %
    % Create a temporary file and open it for writing. If successful, Result
    % returns the file's name and output stream. On error, any temporary
    % file will be removed.
    %
:- pred open_temp_output_with_naming_scheme(string::in, string::in, string::in,
    maybe_error({string, text_output_stream})::out, io::di, io::uo) is det.

    % As above, but with any old name and location for the temporary file.
    %
:- pred open_temp_output(maybe_error({string, text_output_stream})::out,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%

    % compare_file_timestamps(FileNameA, FileNameB, MaybeCompare, !IO):
    %
    % Get the timestamps of both files. If this is successful, compare them,
    % and return the result of the comparison as the argument of "yes".
    % Otherwise, return "no".
    %
:- pred compare_file_timestamps(file_name::in, file_name::in,
    maybe(comparison_result)::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.options.
:- import_module libs.shell_util.

:- import_module benchmarking.
:- import_module dir.
:- import_module exception.
:- import_module io.file.
:- import_module string.
:- import_module univ.

%---------------------------------------------------------------------------%

write_string_to_file(ProgressStream, Globals, FileNameMsg,
        FileName, FileContentsStr, Succeeded, !IO) :-
    globals.lookup_bool_option(Globals, verbose, Verbose),
    string.format("%% %s `%s'...\n", [s(FileNameMsg), s(FileName)],
        CreatingMsg),
    maybe_write_string(ProgressStream, Verbose, CreatingMsg, !IO),
    io.open_output(FileName, OpenFileResult, !IO),
    (
        OpenFileResult = ok(FileStream),
        io.write_string(FileStream, FileContentsStr, !IO),
        io.close_output(FileStream, !IO),
        maybe_write_string(ProgressStream, Verbose, "% done.\n", !IO),
        Succeeded = succeeded
    ;
        OpenFileResult = error(IOError),
        maybe_write_string(ProgressStream, Verbose, " failed.\n", !IO),
        maybe_flush_output(ProgressStream, Verbose, !IO),
        report_cannot_open_file_for_output(ProgressStream, Globals,
            FileName, IOError, !IO),
        Succeeded = did_not_succeed
    ).

%---------------------------------------------------------------------------%

output_to_file_stream(ProgressStream, Globals, FileName, Action0,
        Succeeded, !IO) :-
    globals.lookup_bool_option(Globals, verbose, Verbose),
    globals.lookup_bool_option(Globals, statistics, Stats),

    string.format("%% Writing to file `%s'...\n", [s(FileName)], WritingMsg),
    maybe_write_string(ProgressStream, Verbose, WritingMsg, !IO),
    maybe_flush_output(ProgressStream, Verbose, !IO),
    io.open_output(FileName, OpenResult, !IO),
    (
        OpenResult = ok(FileStream),
        Action =
            ( pred(E::out, S0::di, S::uo) is det :-
                call(Action0, FileStream, E, S0, S)
            ),
        promise_equivalent_solutions [TryResult, !:IO] (
            try_io(Action, TryResult, !IO)
        ),
        io.close_output(FileStream, !IO),
        maybe_write_string(ProgressStream, Verbose, "% done.\n", !IO),
        maybe_report_stats(ProgressStream, Stats, !IO),
        maybe_flush_output(ProgressStream, Verbose, !IO),
        (
            TryResult = succeeded(Errors),
            (
                Errors = [],
                Succeeded = succeeded
            ;
                Errors = [_ | _],
                maybe_write_string(ProgressStream, Verbose, "\n", !IO),
                list.foldl(report_arbitrary_error(ProgressStream),
                    Errors, !IO),
                Succeeded = did_not_succeed
            )
        ;
            TryResult = exception(_),
            rethrow(TryResult)
        )
    ;
        OpenResult = error(IOError),
        maybe_write_string(ProgressStream, Verbose, "\n", !IO),
        report_cannot_open_file_for_output(ProgressStream, Globals,
            FileName, IOError, !IO),
        Succeeded = did_not_succeed
    ).

%---------------------------------------------------------------------------%

write_include_file_contents(OutputStream, Globals, FileName, Result, !IO) :-
    FollowSymLinks = yes,
    io.file.file_type(FollowSymLinks, FileName, MaybeFileType, !IO),
    (
        MaybeFileType = ok(FileType),
        ( if possibly_regular_file(FileType) then
            copy_file_to_stream(FileName, OutputStream, CopyResult, !IO),
            (
                CopyResult = ok,
                Result = ok
            ;
                CopyResult = error(Error),
                ErrorMsg = io.error_message(Error),
                Msg = msg_for_cannot_open_file_for_input(Globals,
                    FileName, ErrorMsg),
                Result = error(Msg)
            )
        else
            NotRegular = "Not a regular file",
            Msg = msg_for_cannot_open_file_for_input(Globals,
                FileName, NotRegular),
            Result = error(Msg)
        )
    ;
        MaybeFileType = error(FileTypeError),
        ErrorMsg = string.remove_prefix_if_present("can't find file type: ",
            io.error_message(FileTypeError)),
        Msg = msg_for_cannot_open_file_for_input(Globals, FileName, ErrorMsg),
        Result = error(Msg)
    ).

:- pred copy_file_to_stream(string::in, io.text_output_stream::in, io.res::out,
    io::di, io::uo) is det.

copy_file_to_stream(FileName, OutputStream, Res, !IO) :-
    io.open_input(FileName, OpenRes, !IO),
    (
        OpenRes = ok(InputStream),
        promise_equivalent_solutions [TryResult, !:IO] (
            try_io(copy_stream(InputStream, OutputStream), TryResult, !IO)
        ),
        io.close_input(InputStream, !IO),
        (
            TryResult = succeeded(ok),
            Res = ok
        ;
            TryResult = succeeded(error(Error)),
            Res = error(Error)
        ;
            TryResult = exception(_),
            rethrow(TryResult)
        )
    ;
        OpenRes = error(Error),
        Res = error(Error)
    ).

:- pred copy_stream(io.text_input_stream::in, io.text_output_stream::in,
    io.res::out, io::di, io::uo) is det.

copy_stream(InputStream, OutputStream, Res, !IO) :-
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

%---------------------------------------------------------------------------%

get_install_name_option(Globals, OutputFileName, InstallNameOpt) :-
    globals.lookup_string_option(Globals, shlib_linker_install_name_flag,
        InstallNameFlag),
    globals.lookup_string_option(Globals, shlib_linker_install_name_path,
        InstallNamePath0),
    ( if InstallNamePath0 = "" then
        globals.get_grade_dir(Globals, GradeDir),
        get_std_grade_specific_install_lib_dir(Globals, GradeDir,
            InstallNamePath)
    else
        InstallNamePath = InstallNamePath0
    ),
    InstallNameOpt = InstallNameFlag ++
        quote_shell_cmd_arg(InstallNamePath) / OutputFileName.

get_std_grade_specific_install_lib_dir(Globals, GradeDir, InstallNamePath) :-
    % NOTE The naming scheme for grade-specific directories in
    % *installed* copies of a library is quite different from the
    % naming schemes of grade-specific directories in workspaces.
    % This difference seems to me (zs) to be both unnecessary and undesirable,
    % since (a) it effectvely doubles the amount of code required to handle
    % the names of such directories, and (b) it makes the system much
    % harder to understand than necessary.
    globals.lookup_string_option(Globals, install_prefix, InstallPrefix),
    InstallNamePath = InstallPrefix / "lib" / "mercury" / "lib" / GradeDir.

%---------------------------------------------------------------------------%

maybe_report_stats(Stream, yes, !IO) :-
    benchmarking.report_standard_stats(Stream, !IO).
maybe_report_stats(_Stream, no, !IO).

maybe_report_stats_to_stream(yes(Stream), !IO) :-
    benchmarking.report_standard_stats(Stream, !IO).
maybe_report_stats_to_stream(no, !IO).

%---------------------%

maybe_write_string(Stream, yes, String, !IO) :-
    io.write_string(Stream, String, !IO).
maybe_write_string(_Stream, no, _, !IO).

maybe_write_string_to_stream(yes(Stream), String, !IO) :-
    io.write_string(Stream, String, !IO).
maybe_write_string_to_stream(no, _, !IO).

%---------------------%

maybe_flush_output(Stream, yes, !IO) :-
    io.flush_output(Stream, !IO).
maybe_flush_output(_Stream, no, !IO).

maybe_flush_output_to_stream(yes(Stream), !IO) :-
    io.flush_output(Stream, !IO).
maybe_flush_output_to_stream(no, !IO).

%---------------------------------------------------------------------------%

msg_for_cannot_open_file_for_input(Globals, FileName, ErrorMsg) = Msg :-
    string.format("can't open `%s' for input: %s",
        [s(FileName), s(ErrorMsg)], Msg0),
    maybe_canonicalize_error_path_names(Globals, Msg0, Msg).

msg_for_cannot_open_file_for_output(Globals, FileName, ErrorMsg) = Msg :-
    string.format("can't open `%s' for output: %s",
        [s(FileName), s(ErrorMsg)], Msg0),
    maybe_canonicalize_error_path_names(Globals, Msg0, Msg).

:- pred maybe_canonicalize_error_path_names(globals::in,
    string::in, string::out) is det.

maybe_canonicalize_error_path_names(Globals, Msg0, Msg) :-
    globals.lookup_bool_option(Globals, canonicalize_error_path_names, Canon),
    (
        Canon = no,
        Msg = Msg0
    ;
        Canon = yes,
        % ZZZ string.replace_all(Msg0, "`", "'", Msg1),
        QuoteChunks0 = string.split_at_char('\'', Msg0),
        canonicalize_quote_chunks(QuoteChunks0, QuoteChunks),
        Msg = string.join_list("'", QuoteChunks)
    ).

:- pred canonicalize_quote_chunks(list(string)::in, list(string)::out) is det.

canonicalize_quote_chunks([], []).
canonicalize_quote_chunks([QChunk0 | QChunks0], [QChunk | QChunks]) :-
    ( if string.remove_prefix("./", QChunk0, FileName) then
        QChunk = FileName
    else if string.remove_prefix(".\\", QChunk0, FileName) then
        QChunk = FileName
    else if string.remove_prefix("/", QChunk0, PathName) then
        PathNameComponents = string.split_at_char('/', PathName),
        ( if list.last(PathNameComponents, LastComponent) then
            QChunk = LastComponent
        else
            QChunk = QChunk0
        )
    else
        QChunk = QChunk0
    ),
    canonicalize_quote_chunks(QChunks0, QChunks).

%---------------------------------------------------------------------------%

report_cannot_open_file_for_input(ProgressStream, Globals,
        FileName, IOError, !IO) :-
    IOErrorMsr = io.error_message(IOError),
    Msg = msg_for_cannot_open_file_for_input(Globals, FileName, IOErrorMsr),
    io.format(ProgressStream, "%s\n", [s(Msg)], !IO),
    io.set_exit_status(1, !IO).

report_cannot_open_file_for_output(ProgressStream, Globals,
        FileName, IOError, !IO) :-
    IOErrorMsr = io.error_message(IOError),
    Msg = msg_for_cannot_open_file_for_output(Globals, FileName, IOErrorMsr),
    io.format(ProgressStream, "%s\n", [s(Msg)], !IO),
    io.set_exit_status(1, !IO).

%---------------------------------------------------------------------------%

report_arbitrary_error(Stream, ErrorMessage, !IO) :-
    io.format(Stream, "Error: %s\n", [s(ErrorMessage)], !IO),
    io.flush_output(Stream, !IO),
    io.set_exit_status(1, !IO).

%---------------------------------------------------------------------------%

make_install_file_command(Globals, FileName, InstallDir) = Command :-
    globals.get_file_install_cmd(Globals, FileInstallCmd),
    (
        FileInstallCmd = install_cmd_user(InstallCmd)
    ;
        FileInstallCmd = install_cmd_cp,
        InstallCmd = "cp"
    ),
    Command = string.join_list(" ", list.map(quote_shell_cmd_arg,
        [InstallCmd, FileName, InstallDir])).

%---------------------------------------------------------------------------%

open_temp_output_with_naming_scheme(Dir, Prefix, Suffix, Result, !IO) :-
    io.file.make_temp_file(Dir, Prefix, Suffix, TempFileResult, !IO),
    open_temp_file(TempFileResult, Result, !IO).

open_temp_output(Result, !IO) :-
    io.file.make_temp_file(TempFileResult, !IO),
    open_temp_file(TempFileResult, Result, !IO).

:- pred open_temp_file(io.res(string)::in,
    maybe_error({string, text_output_stream})::out, io::di, io::uo) is det.

open_temp_file(TempFileResult, Result, !IO) :-
    (
        TempFileResult = ok(TempFileName),
        io.open_output(TempFileName, OpenResult, !IO),
        (
            OpenResult = ok(Stream),
            Result = ok({TempFileName, Stream})
        ;
            OpenResult = error(Error),
            io.file.remove_file(TempFileName, _, !IO),
            Result = error(format(
                "could not open temporary file `%s': %s",
                [s(TempFileName), s(error_message(Error))]))
        )
    ;
        TempFileResult = error(Error),
        Result = error(format("could not create temporary file: %s",
            [s(error_message(Error))]))
    ).

%---------------------------------------------------------------------------%

compare_file_timestamps(FileNameA, FileNameB, MaybeCompare, !IO) :-
    io.file.file_modification_time(FileNameA, TimeResultA, !IO),
    io.file.file_modification_time(FileNameB, TimeResultB, !IO),
    ( if
        TimeResultA = ok(TimeA),
        TimeResultB = ok(TimeB)
    then
        compare(Compare, TimeA, TimeB),
        MaybeCompare = yes(Compare)
    else
        MaybeCompare = no
    ).

%---------------------------------------------------------------------------%
:- end_module libs.file_util.
%---------------------------------------------------------------------------%
