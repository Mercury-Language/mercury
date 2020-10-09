%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 expandtab
%-----------------------------------------------------------------------------%
% Copyright (C) 2006, 2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: mtc_diff.m.
% Author: Zoltan Somogyi.
%
% A tool to take the difference of two trace counts.
%
%-----------------------------------------------------------------------------%

:- module mtc_diff.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module mdbcomp.
:- import_module mdbcomp.shared_utilities.
:- import_module mdbcomp.trace_counts.

:- import_module getopt.
:- import_module list.
:- import_module map.
:- import_module string.

%-----------------------------------------------------------------------------%

main(!IO) :-
    io.stdout_stream(StdOutStream, !IO),
    io.stderr_stream(StdErrStream, !IO),
    unlimit_stack(!IO),
    io.command_line_arguments(Args0, !IO),
    OptionOps = option_ops_multi(short_option, long_option, option_default),
    getopt.process_options(OptionOps, Args0, Args, GetoptResult),
    (
        GetoptResult = ok(OptionTable),
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
                io.write_string(StdErrStream, Msg2, !IO),
                io.nl(StdErrStream, !IO)
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
                    io.write_string(StdErrStream, "Error writing to " ++
                        "file `" ++ OutputFile ++ "'" ++ ": " ++
                        string(WriteErrorMsg), !IO),
                    io.nl(StdErrStream, !IO)
                )
            else
                % The error message has already been printed above.
                true
            )
        else
            usage(StdOutStream, !IO)
        )
    ;
        GetoptResult = error(GetoptError),
        GetoptErrorMsg = option_error_to_string(GetoptError),
        io.format(StdOutStream, "%s\n", [s(GetoptErrorMsg)], !IO)
    ).

:- pred usage(io.text_output_stream::in, io::di, io::uo) is det.

usage(OutStream, !IO) :-
    io.write_string(OutStream,
        "Usage: mtc_diff -o outputfile tracecountfile1 tracecountfile2\n",
        !IO).

%-----------------------------------------------------------------------------%

:- type option
    --->    output_filename.

:- type option_table == option_table(option).

:- pred short_option(character::in, option::out) is semidet.

short_option('o',               output_filename).

:- pred long_option(string::in, option::out) is semidet.

long_option("out",              output_filename).

:- pred option_default(option::out, option_data::out) is multi.
:- pragma no_determinism_warning(option_default/2).

option_default(output_filename, string("")).

%-----------------------------------------------------------------------------%
