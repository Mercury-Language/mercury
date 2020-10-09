%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2005-2006, 2008, 2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: mdprof_dump.m.
% Authors: juliensf, zs.
%
% This module is the main module of a tool for dumping out the contents
% of a deep profiling data file (Deep.data file) for the purpose of debugging
% the deep profiler or the part of the Mercury runtime that generates Deep.data
% files.
%
%---------------------------------------------------------------------------%

:- module mdprof_dump.
:- interface.

:- import_module io.

%---------------------------------------------------------------------------%

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module dump.
:- import_module read_profile.

:- import_module bool.
:- import_module char.
:- import_module getopt.
:- import_module list.
:- import_module maybe.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    io.progname_base("mdprof_dump", ProgName, !IO),
    io.command_line_arguments(Args0, !IO),
    OptionOps = option_ops_multi(short_option, long_option, defaults),
    getopt.process_options(OptionOps, Args0, Args, MaybeOptions),
    (
        MaybeOptions = ok(Options),
        getopt.lookup_bool_option(Options, help, NeedsHelp),
        (
            NeedsHelp = yes,
            usage(ProgName, !IO)
        ;
            NeedsHelp = no,
            (
                (
                    Args = [],
                    FileName = "Deep.data"
                ;
                    Args = [FileName]
                ),
                % Process options, check that they make sense.
                make_dump_options(Options, MaybeDumpOptions),
                (
                    MaybeDumpOptions = yes(DumpOptions),
                    main_2(DumpOptions, FileName, !IO)
                ;
                    MaybeDumpOptions = no,
                    usage(ProgName, !IO)
                )
            ;
                Args = [_, _ | _],
                usage(ProgName, !IO)
            )
        )
    ;
        MaybeOptions = error(Error),
        Msg = option_error_to_string(Error),
        io.stderr_stream(Stderr, !IO),
        io.format(Stderr, "%s: %s\n", [s(ProgName), s(Msg)], !IO),
        io.set_exit_status(1, !IO)
    ).

:- pred main_2(dump_options::in, string::in, io::di, io::uo) is det.

main_2(DumpOptions, FileName, !IO) :-
    read_call_graph(FileName, MaybeInitialDeep, !IO),
    (
        MaybeInitialDeep = ok(InitialDeep),
        dump_initial_deep(InitialDeep, DumpOptions, !IO)
    ;
        MaybeInitialDeep = error(Msg),
        io.format("Cannot read %s: %s\n", [s(Msg), s(FileName)], !IO)
    ).

%---------------------------------------------------------------------------%
%
% Option processing
%

    % Process options and the list of arrays to be dumped.
    %
:- pred make_dump_options(option_table(option)::in, maybe(dump_options)::out)
    is det.

make_dump_options(Options, MaybeDumpOptions) :-
    getopt.lookup_accumulating_option(Options, dump_options, ArrayOptionStrs),
    getopt.lookup_bool_option(Options, option_restrict, RestrictBool),
    (
        RestrictBool = yes,
        Restrict = show_restricted_dump
    ;
        RestrictBool = no,
        Restrict = show_complete_dump
    ),
    DumpOptions0 = default_dump_options ^ do_restricted := Restrict,
    ( if dump_array_options(ArrayOptionStrs, ArrayOptions) then
        MaybeDumpOptions = yes(DumpOptions0 ^ do_arrays := ArrayOptions)
    else
        MaybeDumpOptions = no
    ).

:- type option
    --->    help
    ;       dump_options
    ;       option_restrict.

:- type option_table == (option_table(option)).

:- pred short_option(char::in, option::out) is semidet.

short_option('h', help).
short_option('D', dump_options).
short_option('r', option_restrict).

:- pred long_option(string::in, option::out) is semidet.

long_option("help", help).
long_option("dump-options", dump_options).
long_option("restrict", option_restrict).

:- pred defaults(option::out, option_data::out) is multi.

defaults(help, bool(no)).
defaults(dump_options, accumulating([])).
defaults(option_restrict, bool(no)).

%---------------------------------------------------------------------------%

:- pred usage(string::in, io::di, io::uo) is det.

usage(ProgName, !IO) :-
    io.stderr_stream(StdErr, !IO),
    io.format(StdErr, "Usage: %s [-h] [-D what] [filename]\n", [s(ProgName)],
        !IO),
    io.write_string(StdErr, options_description, !IO).

:- func options_description = string.

options_description =
    "Options:\n" ++
    "\t-h, --help\n" ++
    "\t\tDisplay this message.\n" ++
    "\t-r, --restrict\n" ++
    "\t\tDo not dump proc and call-site statics that are\n" ++
    "\t\tnot referenced from the proc dynamics\n" ++
    "\t-D all\n" ++
    "\t\tDump all arrays, (default).\n" ++
    "\t-D csd\n" ++
    "\t\tDump call-site dynamics.\n" ++
    "\t-D pd\n" ++
    "\t\tDump proc dynamics.\n" ++
    "\t-D css\n" ++
    "\t\tDump call-site statics.\n" ++
    "\t-D ps\n" ++
    "\t\tDump proc statics.\n" ++
    "\nThe following options are unimplemented.\n" ++
    "\t-D clique\n" ++
    "\t\tDump information about cliques.\n" ++
    "\t-D rev\n" ++
    "\t\tDump reverse links.\n" ++
    "\t-D prop\n" ++
    "\t\tDump propagated measurement information.\n".

%---------------------------------------------------------------------------%
:- end_module mdprof_dump.
%---------------------------------------------------------------------------%
