%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright (C) 1995-1998, 2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Main authors: bromage, Marnix Klooster <marnix@worldonline.nl>
%
% Something very similar to the standard diff utility.  Sort of.  :-)
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module diff.

:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module diff_out.
:- import_module file.
:- import_module filter.
:- import_module globals.
:- import_module match.
:- import_module myers.
:- import_module options.

:- import_module getopt.
:- import_module list.
:- import_module maybe.

%-----------------------------------------------------------------------------%

main(!IO) :-
    io.command_line_arguments(Args0, !IO),
    options.get_option_ops(OptionOps),
    getopt.process_options(OptionOps, Args0, Args, Result0),
    Result1 = convert_to_maybe_option_table(Result0),
    postprocess_options(Result1, Result, !IO),
    (
        Result = yes(Msg),
        usage_error(Msg, !IO)
    ;
        Result = no,
        globals.io_get_output_style(OutputStyle, !IO),
        ( if OutputStyle = help_only then
            usage(!IO)
        else if OutputStyle = version_only then
            version(!IO)
        else
            main_2(Args, !IO)
        )
    ).

%-----------------------------------------------------------------------------%

:- pred usage_error(string::in, io::di, io::uo) is det.

usage_error(Msg, !IO) :-
    io.progname_base("diff", ProgName, !IO),
    io.stderr_stream(StdErr, !IO),
    io.write_strings(StdErr, [ProgName, ": ", Msg, "\n"], !IO),
    io.set_exit_status(1, !IO),
    usage(!IO).

:- pred usage_io_error(io.error::in, io::di, io::uo) is det.

usage_io_error(Error, !IO) :-
    io.error_message(Error, Msg),
    usage_error(Msg, !IO).

:- pred usage(io::di, io::uo) is det.

usage(!IO) :-
    io.write_string("Usage: diff [options] from-file to-file\n\n", !IO),
    options_help(!IO).

:- pred version(io::di, io::uo) is det.

version(!IO) :-
    io.write_string("diff - Mercury diff version 0.4\n", !IO).

%-----------------------------------------------------------------------------%

    % main_2 analyses the command-line arguments which are not options
    % and calls diff.do_diff.
    %
:- pred main_2(list(string)::in, io::di, io::uo) is det.

main_2(Args, !IO)  :-
    (
        Args = [],
        usage_error("missing operand", !IO)
    ;
        Args = [_],
        usage_error("missing operand", !IO)
    ;
        Args = [_, _, _ | _],
        usage_error("too many operands", !IO)
    ;
        Args = [Fname1, Fname2],
        ( if Fname1 = Fname2 then
            % Not sure why anyone would want to diff two
            % files with the same name, but just in case ...
            ( if Fname1 = "-" then
                file.read_input(Fname1, Contents1, !IO),
                Contents1 = Contents2
            else
                file.read_file(Fname1, Contents1, !IO),
                Contents1 = Contents2
            )
        else
            % If either file is "-", simply use standard input.
            % (Note: Both can't be "-" since that was dealt with
            % in the previous case.)
            ( if Fname1 = "-" then
                file.read_input(Fname1, Contents1, !IO),
                file.read_file(Fname2, Contents2, !IO)
            else if Fname2 = "-" then
                file.read_file(Fname1, Contents1, !IO),
                file.read_input(Fname2, Contents2, !IO)
            else
                % Otherwise read the files normally.
                file.read_file(Fname1, Contents1, !IO),
                file.read_file(Fname2, Contents2, !IO)
            )
        ),
        % Now do the diff.
        (
            Contents1 = ok(File1),
            (
                Contents2 = ok(File2),
                diff.do_diff(File1, File2, !IO)
            ;
                Contents2 = error(Msg),
                usage_io_error(Msg, !IO)
            )
        ;
            Contents1 = error(Msg),
            usage_io_error(Msg, !IO)
        )
    ).

%-----------------------------------------------------------------------------%

    % do_diff takes the files plus all the command line options
    % and determines what to do with them.
    %
:- pred do_diff(file::in, file::in, io::di, io::uo) is det.

do_diff(File1, File2, !IO) :-
    % There are four passes:
    %
    % - build_matches determines which lines from the input files match
    %   (using the appropriate command-line options).
    % - diff_by_myers takes the matches produced and computes a diff between
    %   them.
    % - filter_diff analyses the diff, filtering out any edits which the user
    %   said that they didn't want to see (using the appropriate command-line
    %   options).
    % - display_diff outputs the diff in whatever output format the user chose.
    %
    build_matches(File1, File2, FileX, FileY, !IO),
    diff_by_myers(FileX, FileY, Diff0, !IO),
    filter_diff(File1, File2, Diff0, Diff, !IO),
    display_diff(File1, File2, Diff, !IO).

%-----------------------------------------------------------------------------%
:- end_module diff.
%-----------------------------------------------------------------------------%
