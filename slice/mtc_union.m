%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 expandtab
%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2006, 2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: mtc_union.m.
% Main Author: Ian MacLarty.
%
% A tool to combine several trace counts into one.
%
%-----------------------------------------------------------------------------%

:- module mtc_union.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module mdbcomp.
:- import_module mdbcomp.shared_utilities.
:- import_module mdbcomp.trace_counts.

:- import_module bool.
:- import_module getopt.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module set.
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
            Args = [_ | _],
            OutputFile \= ""
        then
            lookup_bool_option(OptionTable, verbose, Verbose),
            read_and_union_trace_counts(Verbose, Args, NumTests, Kinds,
                TraceCounts, MaybeReadError, !IO),
            (
                MaybeReadError = yes(ReadErrorMsg),
                io.write_string(StdErrStream, ReadErrorMsg, !IO),
                io.nl(StdErrStream, !IO)
            ;
                MaybeReadError = no,
                Type = union_file(NumTests, set.to_sorted_list(Kinds)),
                write_trace_counts_to_file(Type, TraceCounts, OutputFile,
                    WriteResult, !IO),
                (
                    WriteResult = ok
                ;
                    WriteResult = error(WriteErrorMsg),
                    io.write_string(StdErrStream,
                        "Error writing to file `" ++ OutputFile ++ "'" ++
                        ": " ++ string(WriteErrorMsg), !IO),
                    io.nl(StdErrStream, !IO)
                )
            )
        else
            usage(StdOutStream, !IO)
        )
    ;
        GetoptResult = error(GetoptErrorMsg),
        io.write_string(StdOutStream, GetoptErrorMsg, !IO),
        io.nl(StdOutStream, !IO)
    ).

:- pred usage(io.text_output_stream::in, io::di, io::uo) is det.

usage(OutStream, !IO) :-
    io.write_strings(OutStream, [
        "Usage: mtc_union [-v] -o output_file file1 file2 ...\n",
        "The -v or --verbose option causes each trace count file name\n",
        "to be printed as it is added to the union.\n",
        "file1, file2, etc should be trace count files.\n"],
        !IO).

%-----------------------------------------------------------------------------%

:- type option
    --->    output_filename
    ;       verbose.

:- type option_table == option_table(option).

:- pred short_option(character::in, option::out) is semidet.

short_option('o',               output_filename).
short_option('v',               verbose).

:- pred long_option(string::in, option::out) is semidet.

long_option("out",              output_filename).
long_option("verbose",          verbose).

:- pred option_default(option::out, option_data::out) is multi.

option_default(output_filename, string("")).
option_default(verbose,         bool(no)).

%-----------------------------------------------------------------------------%
