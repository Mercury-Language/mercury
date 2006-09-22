%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 expandtab
%-----------------------------------------------------------------------------%
% Copyright (C) 2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Tool to take the difference of two trace counts.
%
% Author: Zoltan Somogyi.
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
:- import_module mdbcomp.trace_counts.

:- import_module assoc_list.
:- import_module bool.
:- import_module getopt.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module require.
:- import_module string.
:- import_module std_util.

main(!IO) :-
    io.command_line_arguments(Args0, !IO),
    OptionOps = option_ops_multi(short_option, long_option, option_default),
    getopt.process_options(OptionOps, Args0, Args, GetoptResult),
    (
        GetoptResult = ok(OptionTable),
        lookup_string_option(OptionTable, output_filename, OutputFile),
        (
            Args = [Arg1, Arg2],
            OutputFile \= ""
        ->
            stderr_stream(StdErr, !IO),
            read_trace_counts_source(no, try_single_first, Arg1,
                MaybeTraceCounts1, !IO),
            (
                MaybeTraceCounts1 = list_ok(_, _)
            ;
                MaybeTraceCounts1 = list_error_message(Msg1),
                io.write_string(StdErr, Msg1, !IO),
                io.nl(StdErr, !IO)
            ),
            read_trace_counts_source(no, try_single_first, Arg2,
                MaybeTraceCounts2, !IO),
            (
                MaybeTraceCounts2 = list_ok(_, _)
            ;
                MaybeTraceCounts2 = list_error_message(Msg2),
                io.write_string(StdErr, Msg2, !IO),
                io.nl(StdErr, !IO)
            ),
            (
                MaybeTraceCounts1 = list_ok(Type1, TraceCounts1),
                MaybeTraceCounts2 = list_ok(Type2, TraceCounts2)
            ->
                diff_trace_counts(TraceCounts1, TraceCounts2, TraceCounts),
                write_trace_counts_to_file(diff_file(Type1, Type2),
                    TraceCounts, OutputFile, WriteResult, !IO),
                (
                    WriteResult = ok
                ;
                    WriteResult = error(WriteErrorMsg),
                    io.write_string(StdErr, "Error writing to " ++
                        "file `" ++ OutputFile ++ "'" ++ ": " ++
                        string(WriteErrorMsg), !IO),
                    io.nl(StdErr, !IO)
                )
            ;
                % The error message has already been printed above.
                true
            )
        ;
            usage(!IO)
        )
    ;
        GetoptResult = error(GetoptErrorMsg),
        io.write_string(GetoptErrorMsg, !IO),
        io.nl(!IO)
    ).

:- pred usage(io::di, io::uo) is det.

usage(!IO) :-
    io.write_strings([
        "Usage: mtc_diff -o outputfile file1 file2\n",
        "file1, file2, etc can be trace count files or they can be files\n",
        "that contains lists of the names of other trace count files.\n"],
        !IO).

%-----------------------------------------------------------------------------%

:- type option
    --->    output_filename.

:- type option_table == option_table(option).

:- pred short_option(character::in, option::out) is semidet.
:- pred long_option(string::in, option::out) is semidet.
:- pred option_default(option::out, option_data::out) is multi.

option_default(output_filename, string("")).

short_option('o',               output_filename).

long_option("out",              output_filename).

%-----------------------------------------------------------------------------%
