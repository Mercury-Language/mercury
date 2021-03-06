%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2002-2008, 2010-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: mdprof_test.m.
% Author: zs.
%
% This file contains a tool for testing the behavior of the deep profiler.
%
%---------------------------------------------------------------------------%

:- module mdprof_test.
:- interface.

:- import_module io.

%---------------------------------------------------------------------------%

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module conf.
:- import_module dump.
:- import_module mdbcomp.
:- import_module mdbcomp.program_representation.
:- import_module profile.
:- import_module query.
:- import_module startup.

:- import_module array.
:- import_module bool.
:- import_module char.
:- import_module getopt.
:- import_module int.
:- import_module library.
:- import_module list.
:- import_module maybe.
:- import_module require.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    io.progname_base("mdprof_test", ProgName, !IO),
    io.command_line_arguments(Args0, !IO),
    getopt.process_options(option_ops_multi(short, long, defaults),
        Args0, Args, MaybeOptions),
    io.stdout_stream(StdOut, !IO),
    io.stderr_stream(StdErr, !IO),
    (
        MaybeOptions = ok(Options),
        lookup_bool_option(Options, help, Help),
        lookup_bool_option(Options, version, Version),
        lookup_bool_option(Options, verify_profile, Verify),
        (
            Help = yes,
            write_help_message(StdOut, ProgName, !IO)
        ;
            Help = no
        ),
        (
            Version = yes,
            write_version_message(StdOut, ProgName, !IO)
        ;
            Version = no
        ),
        ( if
            Help = no,
            Version = no
        then
            (
                Verify = no,
                main_2(StdErr, ProgName, Args, Options, !IO)
            ;
                Verify = yes,
                verify_profile(StdErr, ProgName, Args, Options, !IO)
            )
        else
            true
        )
    ;
        MaybeOptions = error(Error),
        Msg = option_error_to_string(Error),
        io.set_exit_status(1, !IO),
        io.format(StdErr, "%s: error parsing options: %s\n",
            [s(ProgName), s(Msg)], !IO)
    ).

%---------------------------------------------------------------------------%

:- pred main_2(io.text_output_stream::in, string::in, list(string)::in,
    option_table::in, io::di, io::uo) is cc_multi.

main_2(ErrorStream, ProgName, Args, Options, !IO) :-
    ( if Args = [FileName] then
        lookup_bool_option(Options, canonical_clique, Canonical),
        lookup_bool_option(Options, verbose, Verbose),
        lookup_accumulating_option(Options, dump, DumpStages),
        lookup_accumulating_option(Options, dump_options, DumpArrayOptionsStr),
        dump_array_options_to_dump_options(DumpArrayOptionsStr, DumpOptions),
        server_name_port(Machine, !IO),
        script_name(ScriptName, !IO),
        (
            Verbose = no,
            MaybeOutput = no
        ;
            Verbose = yes,
            io.stdout_stream(Stdout, !IO),
            MaybeOutput = yes(Stdout)
        ),
        read_and_startup(Machine, ScriptName, FileName, Canonical,
            MaybeOutput, DumpStages, DumpOptions, StartupResult, !IO),
        (
            StartupResult = ok(Deep),
            lookup_bool_option(Options, test, Test),
            (
                Test = no
            ;
                Test = yes,
                test_server(default_preferences(Deep), Deep, Options, !IO)
            )
        ;
            StartupResult = error(Error),
            io.set_exit_status(1, !IO),
            io.format(ErrorStream, "%s: error reading %s: %s\n",
                [s(ProgName), s(FileName), s(Error)], !IO)
        )
    else
        io.set_exit_status(1, !IO),
        write_help_message(ErrorStream, ProgName, !IO)
    ).

%---------------------------------------------------------------------------%
%
% Verification mode
%

% In this mode of operation mdprof_test just checks to see if the deep
% profiler can read and process a Deep.data file, i.e do everything
% it needs to up to the point where we normally start querying a profile.
% This mode does not cause a server to be started.

:- pred verify_profile(io.text_output_stream::in, string::in, list(string)::in,
    option_table::in, io::di, io::uo) is det.

verify_profile(ErrorStream, ProgName, Args0, Options, !IO) :-
    (
        Args0 = [],
        Args  = ["Deep.data"]
    ;
        Args0 = [_ | _],
        Args = Args0
    ),
    list.foldl(verify_profile_2(ErrorStream, ProgName, Options), Args, !IO).

:- pred verify_profile_2(io.text_output_stream::in, string::in,
    option_table::in, string::in, io::di, io::uo) is det.

verify_profile_2(ErrorStream, ProgName, Options, FileName, !IO) :-
    lookup_bool_option(Options, canonical_clique, Canonical),
    Machine = "dummy_server",      % For verification this doesn't matter.
    script_name(ScriptName, !IO),
    read_and_startup(Machine, ScriptName, FileName, Canonical, no,
        [], default_dump_options, Res, !IO),
    (
        Res = ok(_Deep)
    ;
        Res = error(Error),
        io.set_exit_status(1, !IO),
        io.format(ErrorStream, "%s: error reading %s: %s\n",
            [s(ProgName), s(FileName), s(Error)], !IO)
    ).

%---------------------------------------------------------------------------%

:- pred write_version_message(io.text_output_stream::in, string::in,
    io::di, io::uo) is det.

write_version_message(OutputStream, ProgName, !IO) :-
    library.version(Version, Fullarch),
    io.format(OutputStream, "%s: Mercury deep profiler\n", [s(ProgName)], !IO),
    io.format(OutputStream, "version: %s, on %s.\n",
        [s(Version), s(Fullarch)], !IO).

:- pred write_help_message(io.text_output_stream::in, string::in,
    io::di, io::uo) is det.

write_help_message(OutputStream, ProgName, !IO) :-
    io.format(OutputStream,
        "Usage: %s [<options>] <filename>\n", [s(ProgName)], !IO),
    io.write_string(OutputStream,
        "<filename> must name a deep profiling data file.\n" ++
        "You should specify one of the following options:\n" ++
        "--help      Generate this help message.\n" ++
        "--version   Report the program's version number.\n" ++
        "--verbose   Generate progress messages during startup.\n" ++
        "--test      Test the deep profiler, generating all\n" ++
        "\t\t\tpossible web pages of the popular types.\n" ++
        "--verify-profile\n" ++
        "\t\t\tVerify that <filename> is a well-formed deep profiling\n" ++
        "\t\t\tdata file.\n" ++
        "\n" ++
        "You may also specify the following options:.\n" ++
        "--test-dir <dirname>\n" ++
        "\t\t\tPut the generated web pages into <dirname>.\n" ++
        "--no-compress\n" ++
        "\t\t\tDon't compress the resulting files, this speeds the test.\n" ++
        "--procrep-coverage\n" ++
        "\t\t\tRun the procrep coverage query on every static procedure\n" ++
        "--recursion-types-histogram\n" ++
        "\t\t\tRun the recursion types histogram query\n", !IO).
    % --canonical-clique is not documented because it is not yet supported

%---------------------------------------------------------------------------%

:- pred test_server(preferences::in, deep::in,
    option_table::in, io::di, io::uo) is cc_multi.

test_server(Pref, Deep, Options, !IO) :-
    lookup_string_option(Options, test_dir, DirName),
    string.format("test -d %s || mkdir -p %s", [s(DirName), s(DirName)], Cmd),
    io.call_system(Cmd, _, !IO),

    %XXX: These features have been disabled.  Configuration options should be
    % introduced to enable them as the user desires.
    % array.max(Deep ^ clique_members, NumCliques),
    % test_cliques(1, NumCliques, DirName, Pref, Deep, !IO),
    % test_procs(1, NumProcStatics, DirName, Pref, Deep, !IO).

    lookup_bool_option(Options, static_procrep_coverage,
        StaticProcrepCoverage),
    (
        StaticProcrepCoverage = yes,
        array.max(Deep ^ proc_statics, NumProcStatics),
        test_procrep_static_coverages(1, NumProcStatics, Pref, Deep, Options,
            !IO)
    ;
        StaticProcrepCoverage = no
    ),

    lookup_bool_option(Options, dynamic_procrep_coverage,
        DynamicProcrepCoverage),
    (
        DynamicProcrepCoverage = yes,
        array.max(Deep ^ proc_dynamics, NumProcDynamics),
        test_procrep_dynamic_coverages(1, NumProcDynamics, Pref, Deep, Options,
            !IO)
    ;
        DynamicProcrepCoverage = no
    ),

    lookup_bool_option(Options, recursion_types_histogram, RecTypesHistogram),
    (
        RecTypesHistogram = yes,
        test_recursion_types_histogram(Pref, Deep, Options, !IO)
    ;
        RecTypesHistogram = no
    ).

:- pred test_cliques(int::in, int::in, option_table::in, preferences::in,
    deep::in, io::di, io::uo) is cc_multi.

test_cliques(Cur, Max, Options, Pref, Deep, !IO) :-
    ( if Cur =< Max then
        try_exec(deep_cmd_clique(clique_ptr(Cur)), Pref, Deep, HTML),
        write_test_html(Options, "clique", Cur, HTML, !IO),
        test_cliques(Cur + 1, Max, Options, Pref, Deep, !IO)
    else
        true
    ).

:- pred test_procs(int::in, int::in, option_table::in, preferences::in,
    deep::in, io::di, io::uo) is cc_multi.

test_procs(Cur, Max, Options, Pref, Deep, !IO) :-
    ( if Cur =< Max then
        try_exec(deep_cmd_proc(proc_static_ptr(Cur)), Pref, Deep, HTML),
        write_test_html(Options, "proc", Cur, HTML, !IO),
        test_procs(Cur + 1, Max, Options, Pref, Deep, !IO)
    else
        true
    ).

:- pred test_procrep_static_coverages(int::in, int::in, preferences::in,
    deep::in, option_table::in, io::di, io::uo) is cc_multi.

test_procrep_static_coverages(Cur, Max, Pref, Deep, Options, !IO) :-
    ( if Cur =< Max then
        try_exec(deep_cmd_static_procrep_coverage(proc_static_ptr(Cur)), Pref,
            Deep, HTML),
        write_test_html(Options, "procrep_dynamic_coverage", Cur, HTML, !IO),
        test_procrep_static_coverages(Cur + 1, Max, Pref, Deep, Options, !IO)
    else
        true
    ).

:- pred test_procrep_dynamic_coverages(int::in, int::in, preferences::in,
    deep::in, option_table::in, io::di, io::uo) is cc_multi.

test_procrep_dynamic_coverages(Cur, Max, Pref, Deep, Options, !IO) :-
    ( if Cur =< Max then
        try_exec(deep_cmd_dynamic_procrep_coverage(proc_dynamic_ptr(Cur)),
            Pref, Deep, HTML),
        write_test_html(Options, "procrep_static_coverage", Cur, HTML, !IO),
        test_procrep_dynamic_coverages(Cur + 1, Max, Pref, Deep, Options, !IO)
    else
        true
    ).

:- pred test_recursion_types_histogram(preferences::in, deep::in,
    option_table::in, io::di, io::uo) is det.

test_recursion_types_histogram(Pref, Deep, Options, !IO) :-
    promise_equivalent_solutions [!:IO] (
        try_exec(deep_cmd_recursion_types_frequency, Pref, Deep, HTML),
        write_test_html(Options, "recursion_types_histogram", 1, HTML, !IO)
    ).

:- func progrep_error = maybe_error(prog_rep).

progrep_error =
    error("No Program Representation available when using mdprof_test").

:- pred write_test_html(option_table::in, string::in, int::in, string::in,
    io::di, io::uo) is det.

write_test_html(Options, BaseName, Num, HTML, !IO) :-
    % For large programs such as the Mercury compiler, the profiler data
    % file may contain hundreds of thousands of cliques. We therefore put
    % each batch of pages in a different subdirectory, thus limiting the
    % number of files/subdirs in each directory.
    %
    % XXX consider splitting up this predicate
    Bunch = (Num - 1) // 1000,
    lookup_string_option(Options, test_dir, DirName),
    string.format("%s/%s_%04d",
        [s(DirName), s(BaseName), i(Bunch)], BunchName),
    ( if (Num - 1) rem 1000 = 0 then
        string.format("test -d %s || mkdir -p %s",
            [s(BunchName), s(BunchName)], Cmd),
        io.call_system(Cmd, _, !IO)
    else
        true
    ),
    string.format("%s/%s_%06d.html",
        [s(BunchName), s(BaseName), i(Num)], FileName),
    io.open_output(FileName, Res, !IO),
    (
        Res = ok(Stream),
        io.write_string(Stream, HTML, !IO),
        io.close_output(Stream, !IO),
        lookup_bool_option(Options, compress, Compress),
        (
            Compress = yes,
            string.format("gzip %s", [s(FileName)], GzipCmd),
            io.call_system(GzipCmd, _, !IO)
        ;
            Compress = no
        )
    ;
        Res = error(Err),
        io.error_message(Err, ErrMsg),
        error(ErrMsg)
    ).

%---------------------------------------------------------------------------%

:- type option
    --->    canonical_clique
    ;       dump
    ;       dump_options
    ;       compress
    ;       help
    ;       test
    ;       test_dir
    ;       verbose
    ;       version
    ;       verify_profile
    ;       static_procrep_coverage
    ;       dynamic_procrep_coverage
    ;       recursion_types_histogram.

:- type options ---> options.
:- type option_table == (option_table(option)).

:- pred short(char::in, option::out) is semidet.

short('c',  canonical_clique).
short('d',  dump).
short('D',  dump_options).
short('T',  test).
short('v',  verbose).

:- pred long(string::in, option::out) is semidet.

long("canonical-clique",            canonical_clique).
long("compress",                    compress).
long("dump",                        dump).
long("dump-options",                dump_options).
long("help",                        help).
long("test",                        test).
long("test-dir",                    test_dir).
long("verbose",                     verbose).
long("version",                     version).
long("verify-profile",              verify_profile).
long("static-procrep-coverage",     static_procrep_coverage).
long("dynamic-procrep-coverage",    dynamic_procrep_coverage).
long("recursion-types-histogram",   recursion_types_histogram).

:- pred defaults(option::out, option_data::out) is multi.

defaults(canonical_clique,          bool(no)).
defaults(compress,                  bool(yes)).
defaults(dump,                      accumulating([])).
defaults(dump_options,              accumulating([])).
defaults(help,                      bool(no)).
defaults(test,                      bool(no)).
defaults(test_dir,                  string("deep_test")).
defaults(verbose,                   bool(no)).
defaults(version,                   bool(no)).
defaults(verify_profile,            bool(no)).
defaults(static_procrep_coverage,   bool(no)).
defaults(dynamic_procrep_coverage,  bool(no)).
defaults(recursion_types_histogram, bool(no)).

%---------------------------------------------------------------------------%
:- end_module mdprof_test.
%---------------------------------------------------------------------------%
