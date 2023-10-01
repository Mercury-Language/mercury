%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 expandtab
%---------------------------------------------------------------------------%
% Copyright (C) 2005-2006, 2012 The University of Melbourne.
% Copyright (C) 2015-2016, 2019-2023 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: mtc_union.m.
% Main Author: Ian MacLarty.
%
% A tool to combine several trace counts into one.
%
%---------------------------------------------------------------------------%

:- module mtc_union.
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
:- import_module maybe.
:- import_module set.
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
            compute_and_output_union(StdOutStream, StdErrStream, OptionTable,
                Args, !IO)
        )
    ;
        GetoptResult = error(GetoptError),
        GetoptErrorMsg = option_error_to_string(GetoptError),
        io.format(StdErrStream, "%s\n", [s(GetoptErrorMsg)], !IO),
        io.set_exit_status(1, !IO)
    ).

:- pred compute_and_output_union(io.text_output_stream::in,
    io.text_output_stream::in, option_table::in, list(string)::in,
    io::di, io::uo) is det.

compute_and_output_union(StdOutStream, StdErrStream, OptionTable, Args, !IO) :-
    lookup_string_option(OptionTable, output_filename, OutputFile),
    ( if
        Args = [_ | _],
        OutputFile \= ""
    then
        lookup_bool_option(OptionTable, verbose, Verbose),
        (
            Verbose = yes,
            ShowProgress = yes(StdOutStream)
        ;
            Verbose = no,
            ShowProgress = no
        ),
        read_and_union_trace_counts(ShowProgress, Args, NumTests, Kinds,
            TraceCounts, MaybeReadError, !IO),
        (
            MaybeReadError = yes(ReadErrorMsg),
            io.format(StdErrStream, "%s\n", [s(ReadErrorMsg)], !IO),
            io.set_exit_status(1, !IO)
        ;
            MaybeReadError = no,
            Type = union_file(NumTests, set.to_sorted_list(Kinds)),
            write_trace_counts_to_file(Type, TraceCounts, OutputFile,
                WriteResult, !IO),
            (
                WriteResult = ok
            ;
                WriteResult = error(WriteErrorMsg),
                io.format(StdErrStream,
                    "Error writing to file `%s': %s\n",
                    [s(OutputFile), s(io.error_message(WriteErrorMsg))],
                    !IO),
                io.set_exit_status(1, !IO)
            )
        )
    else
        short_usage(StdErrStream, !IO),
        io.set_exit_status(1, !IO)
    ).

%---------------------------------------------------------------------------%

:- pred display_version(io.text_output_stream::in, io::di, io::uo) is det.

display_version(OutStream, !IO) :-
    Version = library.mercury_version,
    io.format(OutStream, "Mercury trace count union, version %s",
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
    io.progname_base("mtc_union", ProgName, !IO),
    io.format(OutStream, "Usage: %s [<options>] -o <outputfile> <file>...]\n",
        [s(ProgName)], !IO),
    io.format(OutStream, "Use `%s --help' for more information.\n",
        [s(ProgName)], !IO).

:- pred long_usage(io.text_output_stream::in, io::di, io::uo) is det.

long_usage(OutStream, !IO) :-
    io.progname_base("mtc_union", ProgName, !IO),
    io.write_string(OutStream, "Name: mtc_union - Mercury trace count union\n", !IO),
    write_copyright_notice(OutStream, !IO),
    io.write_strings(OutStream, [
        "Usage: ", ProgName, " [<options>] -o <outputfile> <files>...\n",
        "\n",
        "Description:\n"
    ], !IO),
    io.write_prefixed_lines(OutStream, "\t", [
        "`mtc_union' combines multiple trace count files into a single trace",
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
        "-v, --verbose",
        "\tPrint the name of each trace count file as it is added to the union",
        "-o <file>, --output-file <file>",
        "\tWrite the union of the input trace counts to the specified file."
    ], !IO).

:- pred write_copyright_notice(io.text_output_stream::in, io::di, io::uo)
    is det.

write_copyright_notice(OutStream, !IO) :-
    io.write_strings(OutStream, [
        "Copyright (C) 2005-2012 The University of Melbourne\n",
        "Copyright (C) 2013-2023 The Mercury team\n"
    ], !IO).

%---------------------------------------------------------------------------%

:- type option
    --->    help
    ;       version
    ;       output_filename
    ;       verbose.

:- type option_table == option_table(option).

:- pred short_option(character::in, option::out) is semidet.

short_option('?',               help).
short_option('h',               help).
short_option('o',               output_filename).
short_option('v',               verbose).

:- pred long_option(string::in, option::out) is semidet.

long_option("help",             help).
long_option("version",          version).
% For backwards compatibility we accept --out as a synonym for --output-file.
long_option("out",              output_filename).
long_option("output-file",      output_filename).
long_option("verbose",          verbose).

:- pred option_default(option::out, option_data::out) is multi.

option_default(help,            bool(no)).
option_default(version,         bool(no)).
option_default(output_filename, string("")).
option_default(verbose,         bool(no)).

%---------------------------------------------------------------------------%
:- end_module mtc_union.
%---------------------------------------------------------------------------%
