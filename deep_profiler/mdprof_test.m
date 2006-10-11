%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2002-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Author: zs.
%
% This file contains a tool for testing the behavior of the deep profiler.
%-----------------------------------------------------------------------------%

:- module mdprof_test.

:- interface.

:- import_module io.

%-----------------------------------------------------------------------------%

:- pred main(io::di, io::uo) is cc_multi.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module conf.
:- import_module interface.
:- import_module profile.
:- import_module query.
:- import_module startup.
:- import_module timeout.
:- import_module util.

:- import_module array.
:- import_module bool.
:- import_module char.
:- import_module exception.
:- import_module getopt.
:- import_module int.
:- import_module library.
:- import_module list.
:- import_module maybe.
:- import_module require.
:- import_module set.
:- import_module string.

%-----------------------------------------------------------------------------%

main(!IO) :-
    io.progname_base("mdprof_test", ProgName, !IO),
    io.command_line_arguments(Args0, !IO),
    getopt.process_options(option_ops_multi(short, long, defaults),
        Args0, Args, MaybeOptions),
    (
        MaybeOptions = ok(Options),
        lookup_bool_option(Options, help, Help),
        lookup_bool_option(Options, version, Version),
        lookup_bool_option(Options, verify_profile, Verify),
        (
            Help = yes,
            write_help_message(ProgName, !IO)
        ;
            Help = no
        ),
        (
            Version = yes,
            write_version_message(ProgName, !IO)
        ;
            Version = no
        ),
        (
            Help = no,
            Version = no
        ->
            (
                Verify = no,
                main2(ProgName, Args, Options, !IO)
            ;
                Verify = yes,
                verify_profile(ProgName, Args, Options, !IO)
            )
        ;
            true
        )
    ;
        MaybeOptions = error(Msg),
        io.set_exit_status(1, !IO),
        io.format("%s: error parsing options: %s\n",
            [s(ProgName), s(Msg)], !IO)
    ).

%-----------------------------------------------------------------------------%

:- pred main2(string::in, list(string)::in, option_table::in,
    io::di, io::uo) is cc_multi.

main2(ProgName, Args, Options, !IO) :-
    ( Args = [FileName] ->
        lookup_bool_option(Options, canonical_clique, Canonical),
        lookup_bool_option(Options, verbose, Verbose),
        lookup_accumulating_option(Options, dump, DumpStages),
        server_name(Machine, !IO),
        (
            Verbose = no,
            MaybeOutput = no
        ;
            Verbose = yes,
            io.stdout_stream(Stdout, !IO),
            MaybeOutput = yes(Stdout)
        ),
        read_and_startup(Machine, [FileName], Canonical, MaybeOutput,
            DumpStages, Res, !IO),
        (
            Res = ok(Deep),
            lookup_bool_option(Options, test, Test),
            (
                Test = no
            ;
                Test = yes,
                lookup_string_option(Options, test_dir, TestDir),
                test_server(TestDir, default_preferences, Deep, !IO)
            )
        ;
            Res = error(Error),
            io.set_exit_status(1, !IO),
            io.format("%s: error reading data file: %s\n",
                [s(ProgName), s(Error)], !IO)
        )
    ;
        io.set_exit_status(1, !IO),
        write_help_message(ProgName, !IO)
    ).


%-----------------------------------------------------------------------------%
%
% Verification mode
%

% In this mode of operation mdprof_test just checks to see if the deep
% profiler can read and process a Deep.data file, i.e do everything
% it needs to up to the point where we normally start querying a profile.
% This mode does not cause a server to be started.

:- pred verify_profile(string::in, list(string)::in, option_table::in,
    io::di, io::uo) is det.

verify_profile(ProgName, Args0, Options, !IO) :-
    (
        Args0 = [],
        Args  = ["Deep.data"]
    ;
        Args0 = [_ | _],
        Args = Args0
    ),
    list.foldl(verify_profile_2(ProgName, Options), Args, !IO).

:- pred verify_profile_2(string::in, option_table::in, string::in,
    io::di, io::uo) is det.

verify_profile_2(ProgName, Options, FileName, !IO) :-
    lookup_bool_option(Options, canonical_clique, Canonical),
    Machine = "dummy_server",      % For verification this doesn't matter.
    read_and_startup(Machine, [FileName], Canonical, no, [], Res, !IO),
    (
        Res = ok(_Deep)
    ;
        Res = error(Error),
        io.set_exit_status(1, !IO),
        io.format("%s: error reading data file: %s\n",
            [s(ProgName), s(Error)], !IO)
    ).

%-----------------------------------------------------------------------------%

:- pred write_version_message(string::in, io::di, io::uo) is det.

write_version_message(ProgName, !IO) :-
    library.version(Version),
    io.write_string(ProgName, !IO),
    io.write_string(": Mercury deep profiler", !IO),
    io.nl(!IO),
    io.write_string(Version, !IO),
    io.nl(!IO).

:- pred write_help_message(string::in, io::di, io::uo) is det.

write_help_message(ProgName) -->
    io.format("Usage: %s [<options>] <filename>\n", [s(ProgName)]),
    io.format("<filename> must name a deep profiling data file.\n", []),
    io.format("You should specify one of the following options:\n", []),
    io.format("--help      Generate this help message.\n", []),
    io.format("--version   Report the program's version number.\n", []),
    io.format("--verbose   Generate progress messages during startup.\n", []),
    io.format("--test      Test the deep profiler, generating all\n", []),
    io.format("            possible web pages of the popular types.\n", []),
    io.format("--verify-profile\n", []),
    io.format("            Verify that <filename> is a well-formed\n",
        []),
    io.format("            deep profiling data file.\n", []),
    io.nl,
    io.format("You may also specify the following options:.\n", []),
    io.format("--test-dir <dirname>\n", []),
    io.format("            Put the generated web pages into <dirname>.\n",
        []).
    % --canonical-clique is not documented because it is not yet supported

%-----------------------------------------------------------------------------%

:- pred test_server(string::in, preferences::in, deep::in,
    io::di, io::uo) is cc_multi.

test_server(DirName, Pref, Deep, !IO) :-
    string.format("test -d %s || mkdir -p %s", [s(DirName), s(DirName)], Cmd),
    io.call_system(Cmd, _, !IO),
    array.max(Deep ^ clique_members, NumCliques),
    test_cliques(1, NumCliques, DirName, Pref, Deep, !IO),
    array.max(Deep ^ proc_statics, NumProcStatics),
    test_procs(1, NumProcStatics, DirName, Pref, Deep, !IO).

:- pred test_cliques(int::in, int::in, string::in, preferences::in, deep::in,
    io::di, io::uo) is cc_multi.

test_cliques(Cur, Max, DirName, Pref, Deep, !IO) :-
    ( Cur =< Max ->
        try_exec(clique(Cur), Pref, Deep, HTML, !IO),
        write_test_html(DirName, "clique", Cur, HTML, !IO),
        test_cliques(Cur + 1, Max, DirName, Pref, Deep, !IO)
    ;
        true
    ).

:- pred test_procs(int::in, int::in, string::in, preferences::in, deep::in,
    io::di, io::uo) is cc_multi.

test_procs(Cur, Max, DirName, Pref, Deep, !IO) :-
    ( Cur =< Max ->
        try_exec(proc(Cur), Pref, Deep, HTML, !IO),
        write_test_html(DirName, "proc", Cur, HTML, !IO),
        test_procs(Cur + 1, Max, DirName, Pref, Deep, !IO)
    ;
        true
    ).

:- pred write_test_html(string::in, string::in, int::in, string::in,
    io::di, io::uo) is det.

write_test_html(DirName, BaseName, Num, HTML, !IO) :-
    % For large programs such as the Mercury compiler, the profiler data
    % file may contain hundreds of thousands of cliques. We therefore put
    % each batch of pages in a different subdirectory, thus limiting the
    % number of files/subdirs in each directory.
    %
    % XXX consider splitting up this predicate
    Bunch = (Num - 1) // 1000,
    string.format("%s/%s_%04d",
        [s(DirName), s(BaseName), i(Bunch)], BunchName),
    ( (Num - 1) rem 1000 = 0 ->
        string.format("test -d %s || mkdir -p %s",
            [s(BunchName), s(BunchName)], Cmd),
        io.call_system(Cmd, _, !IO)
    ;
        true
    ),
    string.format("%s/%s_%06d.html",
        [s(BunchName), s(BaseName), i(Num)], FileName),
    io.open_output(FileName, Res, !IO),
    (
        Res = ok(Stream),
        io.write_string(Stream, HTML, !IO),
        io.close_output(Stream, !IO),
        string.format("gzip %s", [s(FileName)], GzipCmd),
        io.call_system(GzipCmd, _, !IO)
    ;
        Res = error(Err),
        io.error_message(Err, ErrMsg),
        error(ErrMsg)
    ).

%-----------------------------------------------------------------------------%

:- type option
    --->    canonical_clique
    ;       dump
    ;       flat
    ;       help
    ;       test
    ;       test_dir
    ;       verbose
    ;       version
    ;       verify_profile.

:- type options ---> options.
:- type option_table == (option_table(option)).

:- pred short(char::in, option::out) is semidet.

short('c',  canonical_clique).
short('d',  dump).
short('v',  verbose).
short('D',  test_dir).
short('T',  test).

:- pred long(string::in, option::out) is semidet.

long("canonical-clique",    canonical_clique).
long("dump",                dump).
long("help",                help).
long("test",                test).
long("test-dir",            test_dir).
long("verbose",             verbose).
long("version",             version).
long("verify-profile",      verify_profile).

:- pred defaults(option::out, option_data::out) is multi.

defaults(canonical_clique,  bool(no)).
defaults(dump,              accumulating([])).
defaults(help,              bool(no)).
defaults(test,              bool(no)).
defaults(test_dir,          string("deep_test")).
defaults(verbose,           bool(no)).
defaults(version,           bool(no)).
defaults(verify_profile,    bool(no)).

%-----------------------------------------------------------------------------%
:- end_module mdprof_test.
%-----------------------------------------------------------------------------%
