%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 expandtab
%---------------------------------------------------------------------------%
% Copyright (C) 2005-2006, 2012 The University of Melbourne.
% Copyright (C) 2015-2016, 2019-2020, 2022-2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% Mercury slice tool.
%
%---------------------------------------------------------------------------%

:- module mslice.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module mdbcomp.
:- import_module mdbcomp.shared_utilities.
:- import_module mdbcomp.slice_and_dice.

:- import_module bool.
:- import_module getopt.
:- import_module library.
:- import_module list.
:- import_module maybe.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    io.stdout_stream(StdOutStream, !IO),
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
            (
                ( Args = []
                ; Args = [_, _ | _]
                ),
                io.stderr_stream(StdErrStream, !IO),
                short_usage(StdErrStream, !IO),
                io.set_exit_status(1, !IO)
            ;
                Args = [FileName],
                compute_and_output_slice(StdOutStream, OptionTable, FileName,
                    !IO)
            )
        )
    ;
        GetoptResult = error(GetoptError),
        GetoptErrorMsg = option_error_to_string(GetoptError),
        io.stderr_stream(StdErrStream, !IO),
        io.format(StdErrStream, "%s\n", [s(GetoptErrorMsg)], !IO),
        io.set_exit_status(1, !IO)
    ).

:- pred compute_and_output_slice(io.text_output_stream::in, option_table::in,
    string::in, io::di, io::uo) is det.

compute_and_output_slice(StdOutStream, OptionTable, FileName, !IO) :-
    lookup_string_option(OptionTable, sort, SortStr),
    lookup_int_option(OptionTable, max_row, MaxRow),
    lookup_int_option(OptionTable, max_name_column_width, MaxNameColumnWidth),
    lookup_int_option(OptionTable, max_path_column_width, MaxPathColumnWidth),
    lookup_int_option(OptionTable, max_file_column_width, MaxFileColumnWidth),
    lookup_string_option(OptionTable, modulename, Module),
    ( if MaxNameColumnWidth = 0 then
        MaybeMaxNameColumnWidth = no
    else
        MaybeMaxNameColumnWidth = yes(MaxNameColumnWidth)
    ),
    ( if MaxPathColumnWidth = 0 then
        MaybeMaxPathColumnWidth = no
    else
        MaybeMaxPathColumnWidth = yes(MaxPathColumnWidth)
    ),
    ( if MaxFileColumnWidth = 0 then
        MaybeMaxFileColumnWidth = no
    else
        MaybeMaxFileColumnWidth = yes(MaxFileColumnWidth)
    ),
    read_slice_to_string(FileName, SortStr, MaxRow,
        MaybeMaxNameColumnWidth, MaybeMaxPathColumnWidth,
        MaybeMaxFileColumnWidth, Module, SliceStr, Problem, !IO),
    ( if Problem = "" then
        io.write_string(StdOutStream, SliceStr, !IO)
    else
        io.stderr_stream(StdErrStream, !IO),
        io.format(StdErrStream, "%s\n", [s(Problem)], !IO),
        io.set_exit_status(1, !IO)
    ).

%---------------------------------------------------------------------------%

:- pred display_version(io.text_output_stream::in, io::di, io::uo) is det.

display_version(OutStream, !IO) :-
    Version = library.mercury_version,
    io.format(OutStream, "Mercury slice tool, version %s",
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
    io.write_strings(OutStream, [
        "Usage: mslice [<options>] <file>\n",
        "Use `mslice --help' for more information.\n"
    ], !IO).

:- pred long_usage(io.text_output_stream::in, io::di, io::uo) is det.

long_usage(OutStream, !IO) :-
    io.write_string(OutStream, "Name: mslice - Mercury slice tool\n", !IO),
    write_copyright_notice(OutStream, !IO),
    io.write_string(OutStream, "\nUsage: mslice [<options>] <file>\n", !IO),
    io.write_string(OutStream, "\nDescription:\n", !IO),
    io.write_prefixed_lines(OutStream, "\t", [
        "`mslice' is a tool that outputs views of program slices."
    ], !IO),
    io.write_string(OutStream, "\nArguments:\n", !IO),
    io.write_prefixed_lines(OutStream, "\t", [
        "The argument is assumed to be a Mercury trace count file."
    ], !IO),
    io.write_string(OutStream, "\nOptions:\n", !IO),
    io.write_prefixed_lines(OutStream, "\t", [
        "-?, -h, --help",
        "\tPrint help about using mslice (on the standard output) and exit",
        "\twithout doing any further processing.",
        "--version",
        "\tPrint version information.",
        "-s <sortspec>, --sort <sortspec>",
        "\tSpecify how the output should be sorted.",
        "\t(See the Mercury User's Guide for details.)",
        "-l <N>, --limit <N>",
        "\tLimit the output to at most N lines.",
        "-m <module>, --module <module>",
        "\tRestrict the output to the given module and its submodules (if any).",
        "-n <N>, --max-name-column-width <N>",
        "\tThe maximum width of the column containing predicate and function",
        "\tnames. A value of zero means there is no maximum width.",
        "-p <N>, --max-path-column-width <N>",
        "\tThe maximum width of the column containing ports and goal paths.",
        "\tA value of zero means there is no maximum width.",
        "-f <N>, --max-file-column-width <N>",
        "\tThe maximum width of the column containing file names and line numbers.",
        "\tA value of zero means there is no maximum width."
    ], !IO).

:- pred write_copyright_notice(io.text_output_stream::in, io::di, io::uo)
    is det.

write_copyright_notice(OutStream, !IO) :-
    io.write_strings(OutStream, [
        "Copyright (C) 2005-2012 The University of Melbourne\n",
        "Copyright (C) 2013-2025 The Mercury team\n"
    ], !IO).

%---------------------------------------------------------------------------%

:- type option
    --->    help
    ;       version
    ;       sort
    ;       max_row
    ;       max_name_column_width
    ;       max_path_column_width
    ;       max_file_column_width
    ;       modulename.

:- type option_table == option_table(option).

:- pred short_option(character::in, option::out) is semidet.

short_option('?', help).
short_option('h', help).
short_option('s', sort).
short_option('l', max_row).
short_option('n', max_name_column_width).
short_option('p', max_path_column_width).
short_option('f', max_file_column_width).
short_option('m', modulename).

:- pred long_option(string::in, option::out) is semidet.

long_option("help",            help).
long_option("version",         version).
long_option("sort",            sort).
long_option("limit",           max_row).
% The names without the _width suffix are for backwards
% compatibility.
long_option("max-name-column", max_name_column_width).
long_option("max-path-column", max_path_column_width).
long_option("max-file-column", max_file_column_width).
long_option("max-name-column_width", max_name_column_width).
long_option("max-path-column_width", max_path_column_width).
long_option("max-file-column_width", max_file_column_width).
long_option("module",          modulename).

:- pred option_default(option::out, option_data::out) is multi.

option_default(help,            bool(no)).
option_default(version,         bool(no)).
option_default(sort,            string("C")).
option_default(max_row,         int(100)).
option_default(max_name_column_width, int(35)).
option_default(max_path_column_width, int(12)).
option_default(max_file_column_width, int(20)).
option_default(modulename,      string("")).

%---------------------------------------------------------------------------%
:- end_module mslice.
%---------------------------------------------------------------------------%
