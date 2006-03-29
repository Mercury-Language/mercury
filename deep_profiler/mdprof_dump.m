%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
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
    getopt.lookup_bool_option(Options, restrict_statics, Restrict),
    getopt.lookup_bool_option(Options, dump_prof_stats, ProfStats0),
    getopt.lookup_bool_option(Options, dump_call_site_dynamics, DumpCSDs0),
    getopt.lookup_bool_option(Options, dump_proc_dynamics, DumpPDs0),
    getopt.lookup_bool_option(Options, dump_call_site_statics, DumpCSSs0),
    getopt.lookup_bool_option(Options, dump_proc_statics, DumpPSs0),
    % If the user doesn't way what he/she wants, they dump everything.
    (
        ProfStats0 = no,
        DumpCSDs0 = no,
        DumpPDs0 = no,
        DumpCSSs0 = no,
        DumpPSs0 = no
    ->
        ProfStats = yes,
        DumpCSDs = yes,
        DumpPDs = yes,
        DumpCSSs = yes,
        DumpPSs = yes
    ;
        ProfStats = ProfStats0,
        DumpCSDs = DumpCSDs0,
        DumpPDs = DumpPDs0,
        DumpCSSs = DumpCSSs0,
        DumpPSs = DumpPSs0
    ),
    read_call_graph(FileName, MaybeInitialDeep, !IO),
    (
        MaybeInitialDeep = ok(InitialDeep),
        dump_initial_deep(ProfStats, Restrict, DumpCSDs, DumpPDs,
            DumpCSSs, DumpPSs, InitialDeep, !IO)
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
    ;       dump_prof_stats
    ;       dump_call_site_dynamics
    ;       dump_proc_dynamics
    ;       dump_call_site_statics
    ;       dump_proc_statics
    ;       restrict_statics.

:- type option_table == (option_table(option)).

:- pred short_option(char::in, option::out) is semidet.

short_option('h', help).

:- pred long_option(string::in, option::out) is semidet.

long_option("help", help).
long_option("prof-stats", dump_prof_stats).
long_option("call-site-dynamics", dump_call_site_dynamics).
long_option("csd", dump_call_site_dynamics).
long_option("proc-dynamics", dump_proc_dynamics).
long_option("pd", dump_proc_dynamics).
long_option("call-site-statics", dump_call_site_statics).
long_option("css", dump_call_site_statics).
long_option("proc-statics", dump_proc_statics).
long_option("ps", dump_proc_statics).
long_option("restrict-statics", restrict_statics).

:- pred defaults(option::out, option_data::out) is multi.

defaults(help, bool(no)).
defaults(dump_prof_stats, bool(no)).
defaults(dump_call_site_dynamics, bool(no)).
defaults(dump_proc_dynamics, bool(no)).
defaults(dump_call_site_statics, bool(no)).
defaults(dump_proc_statics, bool(no)).
defaults(restrict_statics, bool(no)).

%----------------------------------------------------------------------------%

:- pred usage(string::in, io::di, io::uo) is det.

usage(ProgName, !IO) :-
    io.stderr_stream(StdErr, !IO),
    io.format(StdErr, "Usage: %s [<options>] [filename]\n", [s(ProgName)],
        !IO),
    io.write_string(StdErr, "Options:\n", !IO),
    io.write_string(StdErr, "\t-h, --help\n", !IO),
    io.write_string(StdErr, "\t\tDisplay this message.\n", !IO),
    io.write_string(StdErr, "\t--csd, --call-site-dynamics\n", !IO),
    io.write_string(StdErr, "\t\tDump call-site dynamics.\n", !IO),
    io.write_string(StdErr, "\t--pd, --proc-dynamics\n", !IO),
    io.write_string(StdErr, "\t\tDump proc dynamics.\n", !IO),
    io.write_string(StdErr, "\t--css, --call-site-statics\n", !IO),
    io.write_string(StdErr, "\t\tDump call-site statics.\n", !IO),
    io.write_string(StdErr, "\t--ps, --proc-statics\n", !IO),
    io.write_string(StdErr, "\t\tDump proc statics.\n", !IO),
    io.write_string(StdErr, "\t--restrict-statics\n", !IO),
    io.write_string(StdErr, "\t\tOnly dump those proc and ", !IO),
    io.write_string(StdErr, "call-site statics that are referenced ", !IO),
    io.write_string(StdErr, "from the proc dynamics\n", !IO).

%----------------------------------------------------------------------------%
:- end_module mdprof_dump.
%----------------------------------------------------------------------------%
