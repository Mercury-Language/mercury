%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2006-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: mdprof_create_feedback.m.
% Author: tannier, pbone.
%
% This module contains code for generating feedback files that tell the
% compiler things such as which conjunctions can be profitably parallelised.
%
%---------------------------------------------------------------------------%

:- module mdprof_create_feedback.
:- interface.

:- import_module io.

%---------------------------------------------------------------------------%

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module mdbcomp.
:- import_module mdbcomp.feedback.
:- import_module mdbcomp.feedback.automatic_parallelism.
:- import_module mdprof_fb.
:- import_module mdprof_fb.automatic_parallelism.
:- import_module mdprof_fb.automatic_parallelism.autopar_search_callgraph.
:- import_module mdprof_fb.automatic_parallelism.autopar_reports.
:- import_module message.
:- import_module profile.
:- import_module startup.

:- import_module bool.
:- import_module char.
:- import_module cord.
:- import_module float.
:- import_module getopt.
:- import_module int.
:- import_module library.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module parsing_utils.
:- import_module string.

%---------------------------------------------------------------------------%
%
% This section contains the main predicate as well as code to read the deep
% profiling data and display usage and version messages to the user.
%

main(!IO) :-
    io.progname_base("mdprof_create_feedback", ProgName, !IO),
    io.command_line_arguments(Args0, !IO),
    getopt.process_options(option_ops_multi(short, long, defaults),
        Args0, Args, MaybeOptions),
    io.stderr_stream(Stderr, !IO),
    (
        MaybeOptions = ok(Options0),
        post_process_options(ProgName, Options0, Options, !IO),
        lookup_bool_option(Options, help, Help),
        lookup_bool_option(Options, version, Version),
        ( if Version = yes then
            write_version_message(ProgName, !IO)
        else if Help = yes then
            write_help_message(ProgName, !IO)
        else
            (
                Args = [InputFileName, OutputFileName],
                get_feedback_requests(ProgName, Options, FoundError,
                    RequestedFeedbackInfo, !IO),
                (
                    FoundError = have_not_found_error,
                    generate_requested_feedback(ProgName, Options,
                        InputFileName, OutputFileName, RequestedFeedbackInfo,
                        !IO)
                ;
                    FoundError = found_error
                    % The error message have already been printed.
                )
            ;
                ( Args = []
                ; Args = [_]
                ; Args = [_, _, _ | _]
                ),
                write_help_message(ProgName, !IO),
                io.set_exit_status(1, !IO)
            )
        )
    ;
        MaybeOptions = error(Msg),
        io.format(Stderr, "%s: error parsing options: %s\n",
            [s(ProgName), s(Msg)], !IO),
        write_help_message(ProgName, !IO),
        io.set_exit_status(1, !IO)
    ).

:- pred generate_requested_feedback(string::in, option_table(option)::in,
    string::in, string::in, requested_feedback_info::in, io::di, io::uo)
    is det.

generate_requested_feedback(ProgName, Options, InputFileName, OutputFileName,
        RequestedFeedbackInfo, !IO) :-
    io.stderr_stream(Stderr, !IO),
    RequestedFeedbackInfo = requested_feedback_info(MaybeParallelize),
    (
        MaybeParallelize = yes(_),
        lookup_bool_option(Options, debug_read_profile, DebugReadProfile),
        lookup_bool_option(Options, report, Report),
        read_deep_file(InputFileName, DebugReadProfile, MaybeDeep, !IO),
        (
            MaybeDeep = ok(Deep),
            deep_get_maybe_progrep(Deep, MaybeProgRep),
            (
                MaybeProgRep = ok(_),
                ProfiledProgramName = Deep ^ profile_stats ^ prs_program_name,
                read_or_create_feedback_file(OutputFileName,
                    ProfiledProgramName, FeedbackReadResult, !IO),
                (
                    FeedbackReadResult = ok(Feedback0),
                    process_deep_to_feedback(RequestedFeedbackInfo,
                        Deep, Messages, Feedback0, Feedback),
                    (
                        Report = yes,
                        print_feedback_report(Feedback, !IO)
                    ;
                        Report = no
                    ),
                    write_feedback_file(OutputFileName, Feedback,
                        WriteResult, !IO),
                    (
                        WriteResult = fwr_ok
                    ;
                        ( WriteResult = fwr_open_error(Error)
                        ; WriteResult = fwr_write_error(Error)
                        ),
                        io.error_message(Error, ErrorMessage),
                        io.format(Stderr, "%s: %s: %s\n",
                            [s(ProgName), s(OutputFileName), s(ErrorMessage)],
                            !IO),
                        io.set_exit_status(1, !IO)
                    ),
                    write_out_messages(Stderr, Messages, !IO)
                ;
                    FeedbackReadResult = error(FeedbackReadError),
                    feedback_read_error_message_string(OutputFileName,
                        FeedbackReadError, Message),
                    io.write_string(Stderr, Message, !IO),
                    io.set_exit_status(1, !IO)
                )
            ;
                MaybeProgRep = error(Error),
                io.set_exit_status(1, !IO),
                io.format(Stderr, "%s: %s\n", [s(ProgName), s(Error)], !IO)
            )
        ;
            MaybeDeep = error(Error),
            io.set_exit_status(1, !IO),
            io.format(Stderr, "%s: error reading %s: %s\n",
                [s(ProgName), s(InputFileName), s(Error)], !IO)
        )
    ;
        MaybeParallelize = no,
        io.format(Stderr, "%s: options do not request any form of feedback\n",
            [s(ProgName)], !IO)
    ).

:- func help_message(string) = string.

help_message(ProgName) = HelpMessage :-
    FormatStr =
"Usage: %s [<options>] <profdatafile> <feedbackfile>
    This command generates feedback information from profiling data.

    The first argument must name a deep profiling data file such as Deep.data.
    The second argument should be the name of the file into which this command
    should put the feedback information it generates.

    You may specify the following general options:

    -h --help       Generate this help message.
    -V --version    Report the program's version number.
    -v --verbosity  <0-4>
                    Generate messages. The higher the argument, the more
                    verbose the program becomes. 2 is recommended and the
                    default.
    --debug-read-profile
                    Generate debugging messages when reading the deep profile
                    and creating the deep structure.
    --report        Print a report about the feedback information after any
                    processing has been done.

    The following options select sets of feedback information useful
    for particular compiler optimizations:

    --implicit-parallelism
                Generate information that the compiler can use for automatic
                parallelization.
    --desired-parallelism <value>
                The amount of desired parallelism for implicit parallelism,
                which must be a floating point number above 1.0.
                Note: This option is currently ignored.
    --implicit-parallelism-intermodule-var-use
                Assume that the compiler will be able to push signals and waits
                for futures across module boundaries.
    --ipar-sparking-cost <value>
                The cost of creating a spark, measured in the deep profiler's
                call sequence counts.
    --ipar-sparking-delay <value>
                The time taken from the time a spark is created until the spark
                is executed by another processor, assuming that there is a free
                processor.
    --ipar-barrier-cost <value>
                The cost of executing the barrier code at the end of each
                parallel conjunct.
    --ipar-future-signal-cost <value>
                The cost of the signal() call for the producer of a shared
                variable, measured in the profiler's call sequence counts.
    --ipar-future-wait-cost <value>
                The cost of the wait() call for the consumer of a shared
                variable, measured in the profiler's call sequence counts.
    --ipar-context-wakeup-delay <value>
                The time taken for a context to resume execution after being
                placed on the run queue. This is used to estimate the impact
                of blocking of a context's execution, it is measured in the
                profiler's call sequence counts.
    --ipar-clique-cost-threshold <value>
                The cost threshold for cliques to be considered for implicit
                parallelism, measured on the profiler's call sequence counts.
    --ipar-call-site-cost-threshold <value>
                The cost of a call site to be considered for parallelism
                against another call site.
    --no-ipar-dep-conjs
                Disable parallelisation of dependent conjunctions.
    --ipar-speedup-alg <alg>
                Choose the algorithm that is used to estimate the speedup for
                dependent calculations. The available algorithms are:
                    overlap: Compute the overlap between dependent
                      conjunctions.
                    num_vars: Use the number of shared variables as a proxy for
                      the amount of overlap available.
                    naive: Ignore dependencies.
                The default is overlap.
    --ipar-speedup-threshold <value>
                The threshold that a speedup ratio must meet before the
                feedback tool will accept a parallelization. It must be
                a floating point number, which must be at least 1.0.
                If it is e.g. 1.02, then the feedback tool will ignore
                parallelizations that promise less than a 2%% local speedup.
    --ipar-best-par-alg <alg>
                Select which algorithm to use to find the best way to
                parallelise a conjunction. The available algorithms are:
                    greedy: A greedy algorithm with a linear time complexity.
                    complete: A complete algorithm with a branch and bound
                      search. This can be slow for problems larger than 50
                      conjuncts, since it has an exponential complexity.
                    complete-size(N): As above exept that it takes a single
                      parameter, N. If a conjunction has more than N
                      conjuncts, then the greedy algorithm will be used.
                    complete-branches(N): The same as the complete algorithm,
                      except that it allows at most N branches to be created
                      during the search. Once N branches have been created,
                      a greedy search is used on each open branch.
                The default is complete-branches(1000).

    The following options select specific types of feedback information
    and parameterise them:

    --candidate-parallel-conjunctions
                Produce a list of candidate parallel conjunctions for implicit
                parallelism. This option uses the implicit parallelism
                settings above.

",
    HelpMessage = string.format(FormatStr, [s(ProgName)]).

:- pred write_help_message(string::in, io::di, io::uo) is det.

write_help_message(ProgName, !IO) :-
    io.write_string(help_message(ProgName), !IO).

:- pred write_version_message(string::in, io::di, io::uo) is det.

write_version_message(ProgName, !IO) :-
    library.version(Version, Fullarch),
    io.format("%s: Mercury deep profiler\n", [s(ProgName)], !IO),
    io.format("version %s, on %s.\n",
        [s(Version), s(Fullarch)], !IO).

    % Read a deep profiling data file.
    %
:- pred read_deep_file(string::in, bool::in, maybe_error(deep)::out,
    io::di, io::uo) is det.

read_deep_file(Input, Debug, MaybeDeep, !IO) :-
    % XXX the server_name_port and script_name fields of the deep/0
    % structure are only required by the CGI program.  (They probably
    % shouldn't be part of that structure since not all tools that operate
    % on deep profiles require them.)
    Machine = "DummyServer",
    ScriptName = "DummyScript",
    (
        Debug = yes,
        io.stdout_stream(Stdout, !IO),
        MaybeOutput = yes(Stdout)
    ;
        Debug = no,
        MaybeOutput = no
    ),
    read_and_startup_default_deep_options(Machine, ScriptName, Input, no,
        MaybeOutput, [], MaybeDeep, !IO).

%---------------------------------------------------------------------------%
%
% This section describes and processes command line options. Individual
% feedback information can be requested by the user, as well as options named
% after optimizations that may imply one or more feedback information types,
% which that optimization uses.
%

    % Command line options.
    %
:- type option
    --->    help
    ;       version
    ;       verbosity
    ;       debug_read_profile
    ;       report

            % A list of candidate parallel conjunctions is produced for the new
            % implicit parallelism implementation.
    ;       candidate_parallel_conjunctions

            % Provide suitable feedback information for implicit parallelism
    ;       implicit_parallelism
    ;       desired_parallelism
    ;       ipar_intermodule_var_use
    ;       ipar_sparking_cost
    ;       ipar_sparking_delay
    ;       ipar_barrier_cost
    ;       ipar_future_signal_cost
    ;       ipar_future_wait_cost
    ;       ipar_context_wakeup_delay
    ;       ipar_clique_cost_threshold
    ;       ipar_call_site_cost_threshold
    ;       ipar_speedup_threshold
    ;       ipar_dep_conjs
    ;       ipar_speedup_alg
    ;       ipar_alg_for_finding_best_par.

% TODO: Introduce an option to disable parallelisation of dependent
% conjunctions, or switch to the simple calculations for independent
% conjunctions.

:- pred short(char::in, option::out) is semidet.

short('h', help).
short('v', verbosity).
short('V', version).

:- pred long(string::in, option::out) is semidet.

long("help",
    help).
long("verbosity",
    verbosity).
long("version",
    version).
long("debug-read-profile",
    debug_read_profile).
long("report",
    report).
long("candidate-parallel-conjunctions",
    candidate_parallel_conjunctions).
long("implicit-parallelism",
    implicit_parallelism).
long("desired-parallelism",
    desired_parallelism).
long("implicit-parallelism-intermodule-var-use",
    ipar_intermodule_var_use).
long("ipar-intermodule-var-use",
    ipar_intermodule_var_use).
long("implicit-parallelism-sparking-cost",
    ipar_sparking_cost).
long("ipar-sparking-cost",
    ipar_sparking_cost).
long("implicit-parallelism-sparking-delay",
    ipar_sparking_delay).
long("ipar-sparking-delay",
    ipar_sparking_delay).
long("implicit-parallelism-future-signal-cost",
    ipar_future_signal_cost).
long("ipar-future-signal-cost",
    ipar_future_signal_cost).
long("implicit-parallelism-barrier-cost",
    ipar_barrier_cost).
long("ipar-barrier-cost",
    ipar_barrier_cost).
long("implicit-parallelism-future-wait-cost",
    ipar_future_wait_cost).
long("ipar-future-wait-cost",
    ipar_future_wait_cost).
long("implicit-parallelism-context-wakeup-delay",
    ipar_context_wakeup_delay).
long("ipar-context-wakeup-delay",
    ipar_context_wakeup_delay).
long("implicit-parallelism-clique-cost-threshold",
    ipar_clique_cost_threshold).
long("ipar-clique-cost-threshold",
    ipar_clique_cost_threshold).
long("implicit-parallelism-call-site-cost-threshold",
    ipar_call_site_cost_threshold).
long("ipar-call-site-cost-threshold",
    ipar_call_site_cost_threshold).
long("implicit-parallelism-dependant-conjunctions",
    ipar_dep_conjs).
long("ipar-dep-conjs",
    ipar_dep_conjs).
long("implicit-parallelism-dependant-conjunctions-algorithm",
    ipar_speedup_alg).
long("ipar-speedup-alg",
    ipar_speedup_alg).
long("implicit-parallelism-speedup-threshold",
    ipar_speedup_threshold).
long("ipar-speedup-threshold",
    ipar_speedup_threshold).
long("implicit-parallelism-best-parallelisation-algorithm",
    ipar_alg_for_finding_best_par).
long("ipar-best-par-alg",
    ipar_alg_for_finding_best_par).
long("ipar-alg-for-finding-best-par",
    ipar_alg_for_finding_best_par).

:- pred defaults(option::out, option_data::out) is multi.

defaults(help,                              bool(no)).
defaults(verbosity,                         int(2)).
defaults(version,                           bool(no)).
defaults(debug_read_profile,                bool(no)).
defaults(report,                            bool(no)).

defaults(candidate_parallel_conjunctions,   bool(no)).

defaults(implicit_parallelism,              bool(no)).
defaults(desired_parallelism,               string("8.0")).
% XXX: These values have been chosen arbitrarily; we should set them
% based on measurements.
defaults(ipar_intermodule_var_use,          bool(no)).
defaults(ipar_sparking_cost,                int(100)).
defaults(ipar_sparking_delay,               int(1000)).
defaults(ipar_barrier_cost,                 int(100)).
defaults(ipar_future_signal_cost,           int(100)).
defaults(ipar_future_wait_cost,             int(200)).
defaults(ipar_context_wakeup_delay,         int(1000)).
defaults(ipar_clique_cost_threshold,        int(2000)).
defaults(ipar_call_site_cost_threshold,     int(2000)).
defaults(ipar_dep_conjs,                    bool(yes)).
defaults(ipar_speedup_threshold,            string("1.01")).
defaults(ipar_speedup_alg,                  string("overlap")).
defaults(ipar_alg_for_finding_best_par,     string("complete-branches(1000)")).

:- pred construct_measure(string::in, stat_measure::out) is semidet.

construct_measure("mean",   stat_mean).
construct_measure("median", stat_median).

:- pred post_process_options(string::in,
    option_table(option)::in, option_table(option)::out,
    io::di, io::uo) is det.

post_process_options(ProgName, !Options, !IO) :-
    lookup_int_option(!.Options, verbosity, VerbosityLevel),
    io.stderr_stream(Stderr, !IO),
    ( if VerbosityLevel < 0 then
        io.format(Stderr,
            "%s: warning: verbosity level should not be negative.\n",
            [s(ProgName)], !IO),
        set_option(verbosity, int(0), !Options)
    else if VerbosityLevel > 4 then
        io.format(Stderr,
            "%s: warning: verbosity level should not exceed 4.\n",
            [s(ProgName)], !IO),
        set_option(verbosity, int(4), !Options)
    else
        true
    ),
    set_verbosity_level(VerbosityLevel, !IO),
    lookup_bool_option(!.Options, implicit_parallelism, ImplicitParallelism),
    (
        ImplicitParallelism = yes,
        set_option(candidate_parallel_conjunctions, bool(yes), !Options)
    ;
        ImplicitParallelism = no
    ).

    % This type defines the set of feedback_types that are to be calculated and
    % put into the feedback info file. They should correspond with the values
    % of feedback_type.
    %
:- type requested_feedback_info
    --->    requested_feedback_info(
                rfi_parallel    :: maybe(candidate_par_conjunctions_params)
            ).

:- type maybe_found_error
    --->    have_not_found_error
    ;       found_error.

    % Check all the command line options, and return a representation
    % of the user's request.
    %
:- pred get_feedback_requests(string::in, option_table(option)::in,
    maybe_found_error::out, requested_feedback_info::out,
    io::di, io::uo) is det.

get_feedback_requests(ProgName, Options, !:Error, Requested, !IO) :-
    io.stderr_stream(Stderr, !IO),
    !:Error = have_not_found_error,
    % For each feedback type, determine if it is requested, and fill in the
    % corresponding field in the RequestedFeedbackInfo structure.
    lookup_bool_option(Options, candidate_parallel_conjunctions,
        CandidateParallelConjunctions),
    (
        CandidateParallelConjunctions = yes,
        lookup_string_option(Options, desired_parallelism,
            DesiredParallelismStr),
        ( if
            string.to_float(DesiredParallelismStr, DesiredParallelismPrime)
        then
            DesiredParallelism = DesiredParallelismPrime,
            ( if DesiredParallelism > 1.0 then
                true
            else
                io.format(Stderr,
                    "%s: error: desired parallelism level should be > 1.\n",
                    [s(ProgName)], !IO),
                !:Error = found_error
            )
        else
            io.format(Stderr,
                "%s: error: desired parallelism level should be a number.\n",
                [s(ProgName)], !IO),
            !:Error = found_error,
            DesiredParallelism = 1.0        % dummy value
        ),
        lookup_string_option(Options, ipar_speedup_threshold,
            SpeedupThresholdStr),
        ( if string.to_float(SpeedupThresholdStr, SpeedupThresholdPrime) then
            SpeedupThreshold = SpeedupThresholdPrime,
            ( if SpeedupThreshold >= 1.0 then
                true
            else
                io.format(Stderr,
                    "%s: error: speedup threshold should be >= 1.\n",
                    [s(ProgName)], !IO),
                !:Error = found_error
            )
        else
            io.format(Stderr,
                "%s: error: speedup threshold should be a number.\n",
                [s(ProgName)], !IO),
            !:Error = found_error,
            SpeedupThreshold = 1.0        % dummy value
        ),
        lookup_bool_option(Options, ipar_intermodule_var_use,
            IntermoduleVarUse),
        lookup_int_option(Options, ipar_sparking_cost, SparkingCost),
        lookup_int_option(Options, ipar_sparking_delay, SparkingDelay),
        lookup_int_option(Options, ipar_barrier_cost, BarrierCost),
        lookup_int_option(Options, ipar_future_signal_cost, FutureSignalCost),
        lookup_int_option(Options, ipar_future_wait_cost, FutureWaitCost),
        lookup_int_option(Options, ipar_context_wakeup_delay,
            ContextWakeupDelay),
        lookup_int_option(Options, ipar_clique_cost_threshold,
            CPCCliqueThreshold),
        lookup_int_option(Options, ipar_call_site_cost_threshold,
            CPCCallSiteThreshold),
        lookup_bool_option(Options, ipar_dep_conjs, AllowDepConjs),
        lookup_string_option(Options, ipar_speedup_alg, SpeedupAlgString),
        ( if
            parse_parallelise_dep_conjs_string(AllowDepConjs,
                SpeedupAlgString, SpeedupAlgPrime)
        then
            SpeedupAlg = SpeedupAlgPrime
        else
            io.format(Stderr,
                "%s: error: %s is not a speedup estimate algorithm.\n",
                [s(ProgName), s(SpeedupAlgString)], !IO),
            !:Error = found_error,
            SpeedupAlg = do_not_parallelise_dep_conjs    % dummy value
        ),
        lookup_string_option(Options, ipar_alg_for_finding_best_par,
            AlgForFindingBestParStr),
        parse_best_par_algorithm(AlgForFindingBestParStr,
            MaybeAlgForFindingBestPar),
        (
            MaybeAlgForFindingBestPar = ok(AlgForFindingBestPar)
        ;
            MaybeAlgForFindingBestPar = error(MaybeMessage, _Line, _Col),
            (
                MaybeMessage = yes(Message),
                io.format(Stderr,
                    "%s: error: %s is not an algorithm for " ++
                        "finding the best parallelisation: %s\n",
                    [s(ProgName), s(AlgForFindingBestParStr), s(Message)], !IO)
            ;
                MaybeMessage = no,
                io.format(Stderr,
                    "%s: error: %s is not an algorithm for " ++
                        "finding the best parallelisation.\n",
                    [s(ProgName), s(AlgForFindingBestParStr)], !IO)
            ),
            !:Error = found_error,
            AlgForFindingBestPar = affbp_greedy    % dummy value
        ),
        AutoParOpts = candidate_par_conjunctions_params(
            DesiredParallelism,
            IntermoduleVarUse,
            SparkingCost,
            SparkingDelay,
            BarrierCost,
            FutureSignalCost,
            FutureWaitCost,
            ContextWakeupDelay,
            CPCCliqueThreshold,
            CPCCallSiteThreshold,
            SpeedupThreshold,
            SpeedupAlg,
            AlgForFindingBestPar),
        MaybeAutoParOpts = yes(AutoParOpts)
    ;
        CandidateParallelConjunctions = no,
        MaybeAutoParOpts = no
    ),
    Requested = requested_feedback_info(MaybeAutoParOpts).

:- pred parse_best_par_algorithm(string::in,
    parse_result(alg_for_finding_best_par)::out) is det.

parse_best_par_algorithm(String, Result) :-
    promise_equivalent_solutions [Result] (
        parse(String, parse_alg_for_finding_best_par, Result)
    ).

:- pred parse_alg_for_finding_best_par(src::in, alg_for_finding_best_par::out,
    ps::in, ps::out) is semidet.

parse_alg_for_finding_best_par(Src, Algorithm, !PS) :-
    whitespace(Src, _, !PS),
    ( if
        keyword(idchars, "greedy", Src, _, !PS)
    then
        Algorithm = affbp_greedy
    else if
        keyword(idchars, "complete-branches", Src, _, !PS),
        brackets("(", ")", int_literal, Src, N, !PS),
        N >= 0
    then
        Algorithm = affbp_complete_branches(N)
    else if
        keyword(idchars, "complete-size", Src, _, !PS),
        brackets("(", ")", int_literal, Src, N, !PS),
        N >= 0
    then
        Algorithm = affbp_complete_size(N)
    else
        keyword(idchars, "complete", Src, _, !PS),
        Algorithm = affbp_complete
    ),
    eof(Src, _, !PS).

:- func idchars = string.

idchars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_".

:- pred parse_parallelise_dep_conjs_string(bool::in, string::in,
    parallelise_dep_conjs::out) is semidet.

parse_parallelise_dep_conjs_string(no, _, do_not_parallelise_dep_conjs).
parse_parallelise_dep_conjs_string(yes, "naive",
    parallelise_dep_conjs(estimate_speedup_naively)).
parse_parallelise_dep_conjs_string(yes, "overlap",
    parallelise_dep_conjs(estimate_speedup_by_overlap)).

    % Adjust command line options when one option implies other options.
    %
:- pred option_implies(option::in, option::in, bool::in,
    option_table(option)::in, option_table(option)::out) is det.

option_implies(Option, ImpliedOption, ImpliedValue, !Options) :-
    ( if lookup_bool_option(!.Options, Option, yes) then
        set_option(ImpliedOption, bool(ImpliedValue), !Options)
    else
        true
    ).

    % Set the value of an option in the option table.
    %
:- pred set_option(option::in, option_data::in,
    option_table(option)::in, option_table(option)::out) is det.

set_option(Option, Value, !Options) :-
    map.set(Option, Value, !Options).

%---------------------------------------------------------------------------%

    % process_deep_to_feedback(RequestedFeedbackInfo, Deep, Messages,
    %   !Feedback)
    %
    % Process a deep profiling structure and update the feedback information
    % according to the RequestedFeedbackInfo parameter.
    %
:- pred process_deep_to_feedback(requested_feedback_info::in, deep::in,
    cord(message)::out, feedback_info::in, feedback_info::out) is det.

process_deep_to_feedback(RequestedFeedbackInfo, Deep, Messages, !Feedback) :-
    RequestedFeedbackInfo = requested_feedback_info(MaybeAutoParOpts),
    (
        MaybeAutoParOpts = yes(AutoParOpts),
        candidate_parallel_conjunctions(AutoParOpts, Deep, Messages, !Feedback)
    ;
        MaybeAutoParOpts = no,
        Messages = cord.empty
    ).

%---------------------------------------------------------------------------%
:- end_module mdprof_create_feedback.
%---------------------------------------------------------------------------%
