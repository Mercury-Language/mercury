%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 expandtab
%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2006, 2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Mercury slice tool.
%
%-----------------------------------------------------------------------------%

:- module mslice.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module mdbcomp.
:- import_module mdbcomp.slice_and_dice.
:- import_module mdbcomp.shared_utilities.

:- import_module getopt.
:- import_module list.
:- import_module maybe.
:- import_module string.

main(!IO) :-
    io.stdout_stream(StdOutStream, !IO),
    unlimit_stack(!IO),
    io.command_line_arguments(Args0, !IO),
    OptionOps = option_ops_multi(short_option, long_option, option_default),
    getopt.process_options(OptionOps, Args0, Args, GetoptResult),
    (
        GetoptResult = ok(OptionTable),
        (
            Args = [],
            usage(StdOutStream, !IO)
        ;
            Args = [FileName],
            lookup_string_option(OptionTable, sort, SortStr),
            lookup_int_option(OptionTable, max_row, MaxRow),
            lookup_int_option(OptionTable, max_pred_column, MaxPredColumn),
            lookup_int_option(OptionTable, max_path_column, MaxPathColumn),
            lookup_int_option(OptionTable, max_file_column, MaxFileColumn),
            lookup_string_option(OptionTable, modulename, Module),
            ( if MaxPredColumn = 0 then
                MaybeMaxPredColumn = no
            else
                MaybeMaxPredColumn = yes(MaxPredColumn)
            ),
            ( if MaxPathColumn = 0 then
                MaybeMaxPathColumn = no
            else
                MaybeMaxPathColumn = yes(MaxPathColumn)
            ),
            ( if MaxFileColumn = 0 then
                MaybeMaxFileColumn = no
            else
                MaybeMaxFileColumn = yes(MaxFileColumn)
            ),
            read_slice_to_string(FileName, SortStr, MaxRow,
                MaybeMaxPredColumn, MaybeMaxPathColumn, MaybeMaxFileColumn,
                Module, SliceStr, Problem, !IO),
            ( if Problem = "" then
                io.write_string(StdOutStream, SliceStr, !IO)
            else
                io.write_string(StdOutStream, Problem, !IO),
                io.nl(StdOutStream, !IO),
                io.set_exit_status(1, !IO)
            )
        ;
            Args = [_, _ | _],
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
        "Usage: mslice [-s sortspec] [-m module] [-l N] [-n N] [-p N] [-f N] "
        ++ "filename\n",
        !IO).

%-----------------------------------------------------------------------------%

:- type option
    --->    sort
    ;       max_row
    ;       max_pred_column
    ;       max_path_column
    ;       max_file_column
    ;       modulename.

:- type option_table == option_table(option).

:- pred short_option(character::in, option::out) is semidet.

short_option('s',               sort).
short_option('l',               max_row).
short_option('n',               max_pred_column).
short_option('p',               max_path_column).
short_option('f',               max_file_column).
short_option('m',               modulename).

:- pred long_option(string::in, option::out) is semidet.

long_option("sort",             sort).
long_option("limit",            max_row).
long_option("max-name-column",  max_pred_column).
long_option("max-path-column",  max_path_column).
long_option("max-file-column",  max_file_column).
long_option("module",           modulename).

:- pred option_default(option::out, option_data::out) is multi.

option_default(sort,            string("C")).
option_default(max_row,         int(100)).
option_default(max_pred_column, int(35)).
option_default(max_path_column, int(12)).
option_default(max_file_column, int(20)).
option_default(modulename,      string("")).

%-----------------------------------------------------------------------------%
