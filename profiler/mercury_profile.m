%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1995-1997, 1999, 2002, 2004-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Mercury profiler
% Main author: petdr.
%
% Processes the Prof.* and the *.prof files to produce an output very similar
% to `gprof'
%
% Based on the profiling scheme described in the paper: Graham, Kessler and
% McKusick "Gprof: a call graph execution profiler", Proceedings of the 1982
% SIGPLAN Symposium on Compiler Construction, pages 120-126.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module mercury_profile.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module call_graph.
:- import_module generate_output.
:- import_module globals.
:- import_module options.
:- import_module output.
:- import_module process_file.
:- import_module propagate.
:- import_module snapshots.

:- import_module bool.
:- import_module getopt.
:- import_module library.
:- import_module list.
:- import_module maybe.

%-----------------------------------------------------------------------------%

main(!IO) :-
    io.command_line_arguments(Args0, !IO),
    OptionOps = option_ops_multi(short_option, long_option, option_default,
        special_handler),
    getopt.process_options(OptionOps, Args0, Args, Result0),
    postprocess_options(Result0, Args, Result, !IO),
    (
        Result = yes(ErrorMessage),
        usage_error(ErrorMessage, !IO)
    ;
        Result = no,
        main_2(Args, !IO)
    ).

:- pred postprocess_options(maybe_option_table(option)::in, list(string)::in,
    maybe(string)::out, io::di, io::uo) is det.

postprocess_options(error(ErrorMessage), _Args, yes(ErrorMessage), !IO).
postprocess_options(ok(OptionTable), Args, no, !IO) :-
    globals.io_init(OptionTable, !IO),

    % --very-verbose implies --verbose
    globals.io_lookup_bool_option(very_verbose, VeryVerbose, !IO),
    ( VeryVerbose = yes ->
        globals.io_set_option(verbose, bool(yes), !IO)
    ;
        true
    ),
    %
    % Any empty list of arguments implies that we must build the call
    % graph from the dynamic information.
    %
    (
        Args = [],
        globals.io_set_option(dynamic_cg, bool(yes), !IO)
    ;
        Args = [_ | _]
    ).

    % Display error message and then usage message.
    %
:- pred usage_error(string::in, io::di, io::uo) is det.

usage_error(ErrorMessage, !IO) :-
    io.progname_base("mercury_profile", ProgName, !IO),
    io.stderr_stream(StdErr, !IO),
    io.write_strings(StdErr, [ProgName, ": ", ErrorMessage, "\n"], !IO),
    io.set_exit_status(1, !IO),
    usage(!IO).

    % Display usage message.
    %
:- pred usage(io::di, io::uo) is det.

usage(!IO) :-
    io.progname_base("mprof", ProgName, !IO),
    io.stderr_stream(StdErr, !IO),
    library.version(Version, Fullarch),
    io.write_strings(StdErr, [
        "Mercury Profiler, version ", Version, ", on ", Fullarch, ".\n",
        "Copyright (C) 1995-2012 The University of Melbourne\n",
        "Copyright (C) 2013-2015 The Mercury team\n",
            "Usage: ", ProgName, " [<options>] [<files>]\n",
            "Use `", ProgName, " --help' for more information.\n"
        ], !IO).

:- pred long_usage(io::di, io::uo) is det.

long_usage(!IO) :-
    io.progname_base("mprof", ProgName, !IO),
    library.version(Version, Fullarch),
    io.write_strings([
        "Mercury Profiler, version ", Version, ", on ", Fullarch, ".\n",
        "Copyright (C) 1995-2012 The University of Melbourne\n",
        "Copyright (C) 2013-2015 The Mercury team\n\n",
            "Usage: ", ProgName, " [<options>] [<files>]\n",
        "\n",
        "Description:\n",
        "\t`mprof' produces execution profiles for Mercury programs.\n",
        "\tIt outputs a flat profile and optionally also a hierarchical\n",
        "\t(call graph based) profile based on data collected during program\n",
        "\texecution.\n",
        "\n",
        "Arguments:\n",
        "\tIf no <files> are specified, then the `--use-dynamic' option\n",
        "\tis implied: the call graph will be built dynamically.\n",
        "\tOtherwise, the <files> specified should be the `.prof' file\n",
        "\tfor every module in the program.  The `.prof' files, which are\n",
        "\tgenerated automatically by the Mercury compiler, contain the\n",
        "\tprogram's static call graph.\n",
        "\n",
        "Options:\n"
        ], !IO),
    options_help(!IO).

%-----------------------------------------------------------------------------%

:- pred main_2(list(string)::in, io::di, io::uo) is det.

main_2(Args, !IO) :-
    globals.io_get_globals(Globals, !IO),
    globals.lookup_bool_option(Globals, help, Help),
    (
        Help = yes,
        long_usage(!IO)
    ;
        Help = no,
        globals.lookup_bool_option(Globals, snapshots, Snapshots),
        (
            Snapshots = yes,
            show_snapshots(!IO)
        ;
            Snapshots = no,
            main_3(Args, !IO)
        )
    ).

:- pred main_3(list(string)::in, io::di, io::uo) is det.

main_3(Args, !IO) :-
    io.stderr_stream(StdErr, !IO),
    io.set_output_stream(StdErr, StdOut, !IO),
    globals.io_lookup_bool_option(verbose, Verbose, !IO),

    maybe_write_string(Verbose, "% Processing input files...", !IO),
    process_profiling_data_files(Prof0, CallGraph0, !IO),
    maybe_write_string(Verbose, " done\n", !IO),

    globals.io_lookup_bool_option(call_graph, CallGraphOpt, !IO),
    (
        CallGraphOpt = yes,
        maybe_write_string(Verbose, "% Building call graph...", !IO),
        build_call_graph(Args, CallGraph0, CallGraph, !IO),
        maybe_write_string(Verbose, " done\n", !IO),

        maybe_write_string(Verbose, "% Propagating counts...", !IO),
        propagate_counts(CallGraph, Prof0, Prof, !IO),
        maybe_write_string(Verbose, " done\n", !IO)
    ;
        CallGraphOpt = no,
        Prof = Prof0
    ),

    maybe_write_string(Verbose, "% Generating output...", !IO),
    generate_prof_output(Prof, IndexMap, OutputProf, !IO),
    maybe_write_string(Verbose, " done\n", !IO),

    io.set_output_stream(StdOut, _, !IO),
    output_profile(OutputProf, IndexMap, !IO),
    io.nl(!IO).

%-----------------------------------------------------------------------------%
:- end_module mercury_profile.
%-----------------------------------------------------------------------------%
