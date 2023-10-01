%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 expandtab
%---------------------------------------------------------------------------%
% Copyright (C) 2006, 2012 The University of Melbourne.
% Copyright (C) 2015-2017, 2019-2023 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: mtc_diff.m.
% Author: Zoltan Somogyi.
%
% A tool to take the difference of two trace counts.
%
%---------------------------------------------------------------------------%

:- module mtc_diff.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module mdbcomp.
:- import_module mdbcomp.shared_utilities.
:- import_module mdbcomp.trace_counts.

:- import_module bool.
:- import_module getopt.
:- import_module library.
:- import_module list.
:- import_module map.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    io.stdout_stream(StdOutStream, !IO),
    io.stderr_stream(StdErrStream, !IO),
    unlimit_stack(!IO),
    io.command_line_arguments(Args0, !IO),
    OptionOps = option_ops_multi(short_option, long_option, option_default),
    getopt.process_options(OptionOps, Args0, Args, GetoptResult),
    (
        GetoptResult = ok(OptionTable),
        ( if lookup_bool_option(OptionTable, help, yes) then
            long_usage(StdOutStream, !IO)
        else if lookup_bool_option(OptionTable, version, yes) then
            display_version(StdOutStream, !IO)
        else
            compute_and_output_diff(StdErrStream, OptionTable, Args, !IO)
        )
    ;
        GetoptResult = error(GetoptError),
        GetoptErrorMsg = option_error_to_string(GetoptError),
        io.format(StdErrStream, "%s\n", [s(GetoptErrorMsg)], !IO),
        io.set_exit_status(1, !IO)
    ).

:- pred compute_and_output_diff(io.text_output_stream::in, option_table::in,
    list(string)::in, io::di, io::uo) is det.

compute_and_output_diff(StdErrStream, OptionTable, Args, !IO) :-
    lookup_string_option(OptionTable, output_filename, OutputFile),
    ( if
        Args = [Arg1, Arg2],
        OutputFile \= ""
    then
        read_trace_counts_source(Arg1, MaybeTraceCounts1, !IO),
        (
            MaybeTraceCounts1 = list_ok(_, _)
        ;
            MaybeTraceCounts1 = list_error_message(Msg1),
            io.write_string(StdErrStream, Msg1, !IO),
            io.nl(StdErrStream, !IO)
        ),
        read_trace_counts_source(Arg2, MaybeTraceCounts2, !IO),
        (
            MaybeTraceCounts2 = list_ok(_, _)
        ;
            MaybeTraceCounts2 = list_error_message(Msg2),
            io.format(StdErrStream, "%s\n", [s(Msg2)], !IO),
            io.set_exit_status(1, !IO)
        ),
        ( if
            MaybeTraceCounts1 = list_ok(Type1, TraceCounts1),
            MaybeTraceCounts2 = list_ok(Type2, TraceCounts2)
        then
            diff_trace_counts(TraceCounts1, TraceCounts2, TraceCounts),
            write_trace_counts_to_file(diff_file(Type1, Type2),
                TraceCounts, OutputFile, WriteResult, !IO),
            (
                WriteResult = ok
            ;
                WriteResult = error(WriteErrorMsg),
                io.format(StdErrStream,
                    "Error writing to file`%s': %s\n",
                    [s(OutputFile), s(io.error_message(WriteErrorMsg))],
                    !IO),
                io.set_exit_status(1, !IO)
            )
        else
            % The error message has already been printed above.
            true
        )
    else
        short_usage(StdErrStream, !IO),
        io.set_exit_status(1, !IO)
    ).

%---------------------------------------------------------------------------%

:- pred display_version(io.text_output_stream::in, io::di, io::uo) is det.

display_version(OutStream, !IO) :-
    Version = library.mercury_version,
    io.format(OutStream, "Mercury trace count diff, version %s",
        [s(Version)], !IO),
    Package = library.package_version,
    ( if Package = "" then
        io.nl(OutStream, !IO)
    else
        io.format(OutStream, " (%s)\n", [s(Package)], !IO)
    ),
    write_copyright_notice(OutStream, !IO).

:- pred short_usage(io.text_output_stream::in, io::di, io::uo) is det.

short_usage(OutStream, !IO) :-
    io.progname_base("mtc_diff", ProgName, !IO),
    io.format(OutStream,
        "Usage: %s [<options>] -o <outputfile> <tracecountfile1> <tracecountfile2>\n",
        [s(ProgName)], !IO),
    io.format(OutStream, "Use `%s --help' for more information.\n",
        [s(ProgName)], !IO).

:- pred long_usage(io.text_output_stream::in, io::di, io::uo) is det.

long_usage(OutStream, !IO) :-
    io.progname_base("mtc_diff", ProgName, !IO),
    io.write_string(OutStream, "Name: mtc_diff - Mercury trace count diff\n", !IO),
    write_copyright_notice(OutStream, !IO),
    io.write_strings(OutStream, [
        "Usage: ", ProgName, " [<options>] -o <outputfile> <tracecountfile1> <tracecountfile2>\n",
        "\n",
        "Description:\n"
    ], !IO),
    io.write_prefixed_lines(OutStream, "\t", [
        "`mtc_diff' combines multiple trace count files into a single trace",
        "count file."
    ], !IO),
    io.write_string(OutStream, "\nArguments:\n", !IO),
    io.write_prefixed_lines(OutStream, "\t", [
        "<files> is the trace count files that are to be combined."
    ], !IO),
    io.write_string(OutStream, "\nOptions:\n", !IO),
    io.write_prefixed_lines(OutStream, "\t", [
        "-?, -h, --help",
        "\tPrint this usage message.",
        "--version",
        "\tPrint version information.",
        "-o <file>, --output-file <file>",
        "\tWrite the diff of the input trace counts to the specified file."
    ], !IO).

:- pred write_copyright_notice(io.text_output_stream::in, io::di, io::uo)
    is det.

write_copyright_notice(OutStream, !IO) :-
    io.write_strings(OutStream, [
        "Copyright (C) 2006-2012 The University of Melbourne\n",
        "Copyright (C) 2013-2023 The Mercury team\n"
    ], !IO).

%---------------------------------------------------------------------------%

:- type option
    --->    help
    ;       version
    ;       output_filename.

:- type option_table == option_table(option).

:- pred short_option(character::in, option::out) is semidet.

short_option('?', help).
short_option('h', help).
short_option('o', output_filename).

:- pred long_option(string::in, option::out) is semidet.

long_option("help", help).
long_option("version", version).
% For backwards compatibilty we accept --out as a synonym for --output-file.
long_option("out", output_filename).
long_option("output-file", output_filename).

:- pred option_default(option::out, option_data::out) is multi.
:- pragma no_determinism_warning(pred(option_default/2)).

option_default(help, bool(no)).
option_default(version, bool(no)).
option_default(output_filename, string("")).

%---------------------------------------------------------------------------%
:- end_module mtc_diff.
%---------------------------------------------------------------------------%
