%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: mdprof_dump.m.
% Authors: juliensf, zs.
%
% This module is the main module of a tool for dumping out the contents
% of a deep profiling data file (Deep.data file) for the purpose of debugging
% the deep profiler or the part of the Mercury runtime that generates Deep.data
% files.
%
%-----------------------------------------------------------------------------%

:- module mdprof_dump.
:- interface.

:- import_module io.

%-----------------------------------------------------------------------------%

:- pred main(io::di, io::uo) is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module array_util.
:- import_module dump.
:- import_module profile.
:- import_module read_profile.

:- import_module bool.
:- import_module char.
:- import_module getopt.
:- import_module list.
:- import_module maybe.
:- import_module string.

%----------------------------------------------------------------------------%

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
                Args = [],
                FileName = "Deep.data",
                main_2(Options, FileName, !IO)
            ;
                Args = [FileName],
                main_2(Options, FileName, !IO)
            ;
                Args = [_, _ | _],
                usage(ProgName, !IO)
            )
        )
    ;
        MaybeOptions = error(Message),
        io.stderr_stream(Stderr, !IO),
        io.format(Stderr, "%s: %s\n", [s(ProgName), s(Message)], !IO),
        io.set_exit_status(1, !IO)
    ).

:- pred main_2(option_table(option)::in, string::in, io::di, io::uo) is det.

main_2(Options, FileName, !IO) :-
    getopt.lookup_accumulating_option(Options, dump_options, DumpOptions),
    read_call_graph(FileName, MaybeInitialDeep, !IO),
    (
        MaybeInitialDeep = ok(InitialDeep),
        dump_initial_deep(InitialDeep, DumpOptions, !IO)
    ;
        MaybeInitialDeep = error(Msg),
        io.format("Cannot read %s: %s\n", [s(Msg), s(FileName)], !IO)
    ).

%----------------------------------------------------------------------------%
%
% Option processing
%

:- type option
    --->    help
    ;       dump_options.

:- type option_table == (option_table(option)).

:- pred short_option(char::in, option::out) is semidet.

short_option('h', help).
short_option('D', dump_options).

:- pred long_option(string::in, option::out) is semidet.

long_option("help", help).
long_option("dump-options", dump_options).

:- pred defaults(option::out, option_data::out) is multi.

defaults(help, bool(no)).
defaults(dump_options, accumulating([])).

%----------------------------------------------------------------------------%

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
    "\t--D csd\n" ++
    "\t\tDump call-site dynamics.\n" ++
    "\t--D pd\n" ++
    "\t\tDump proc dynamics.\n" ++
    "\t--D css\n" ++
    "\t\tDump call-site statics.\n" ++
    "\t--D ps\n" ++
    "\t\tDump proc statics.\n" ++
    "\t--D restrict\n" ++
    "\t\tDo not dump proc and call-site statics that are\n" ++
    "\t\tnot referenced from the proc dynamics\n" ++
    "\t--D clique\n" ++
    "\t\tDump information about cliques.\n" ++
    "\t--D rev\n" ++
    "\t\tDump reverse links.\n" ++
    "\t--D prop\n" ++
    "\t\tDump propagated measurement information.\n".

%----------------------------------------------------------------------------%
:- end_module mdprof_dump.
%----------------------------------------------------------------------------%
