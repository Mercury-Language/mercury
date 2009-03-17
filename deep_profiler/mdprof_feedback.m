%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2006-2009 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: mdprof_feedback.m.
% Author: tannier, pbone.
%
% This module contains the code for writing to a file the CSSs whose CSDs'
% mean/median call sequence counts (own and desc) exceed the given threshold.
%
% The generated file will then be used by the compiler for implicit parallelism.
%
%-----------------------------------------------------------------------------%

:- module mdprof_feedback.
:- interface.

:- import_module io.

%-----------------------------------------------------------------------------%

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module conf.
:- import_module mdbcomp.
:- import_module mdbcomp.feedback.
:- import_module mdprof_fb.
:- import_module mdprof_fb.automatic_parallelism.
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
:- import_module maybe.
:- import_module require.
:- import_module string.
:- import_module svmap.

%-----------------------------------------------------------------------------%
%
% This section contains the main predicate as well as code to read the deep
% profiling data and display usage and version messages to the user.
%

main(!IO) :-
    io.progname_base("mdprof_feedback", ProgName, !IO),
    io.command_line_arguments(Args0, !IO),
    io.stderr_stream(Stderr, !IO),
    getopt.process_options(option_ops_multi(short, long, defaults),
        Args0, Args, MaybeOptions),
    (
        MaybeOptions = ok(Options),
        lookup_bool_option(Options, help, Help),
        lookup_bool_option(Options, version, Version),
        lookup_bool_option(Options, debug_read_profile, DebugReadProfile),
        (
            Version = yes
        ->
            write_version_message(ProgName, !IO)
        ;
            Help = yes
        ->
            write_help_message(ProgName, !IO)
        ;
            Args = [InputFileName, OutputFileName],
            check_options(Options, RequestedFeedbackInfo),
            check_verbosity_option(Options, VerbosityLevel)
        ->
            read_deep_file(InputFileName, DebugReadProfile, MaybeDeep, !IO),
            (
                MaybeDeep = ok(Deep),
                feedback.read_or_create(OutputFileName, FeedbackReadResult,
                    !IO),
                (
                    FeedbackReadResult = ok(Feedback0),
                    process_deep_to_feedback(RequestedFeedbackInfo,
                        Deep, Messages, Feedback0, Feedback),
                    ProfileProgName = Deep ^ profile_stats ^ program_name,
                    write_feedback_file(OutputFileName, ProfileProgName,
                        Feedback, WriteResult, !IO),
                    (
                        WriteResult = ok
                    ;
                        ( WriteResult = open_error(Error)
                        ; WriteResult = write_error(Error)
                        ),
                        io.error_message(Error, ErrorMessage),
                        io.format(Stderr, "%s: %s\n",
                            [s(OutputFileName), s(ErrorMessage)], !IO),
                        io.set_exit_status(1, !IO)
                    ),
                    cord.foldl_pred(
                        (pred(Message::in, IO0::di, IO::uo) is det :-
                            Level = message_get_level(Message),
                            ( message_level_to_int(Level) =< VerbosityLevel ->
                                message_to_string(Message, MessageStr),
                                io.write_string(Stderr, MessageStr, IO0, IO1),
                                io.nl(IO1, IO)
                            ;
                                IO = IO0
                            )
                        ), Messages, !IO)
                ;
                    FeedbackReadResult = error(FeedbackReadError),
                    feedback.read_error_message_string(OutputFileName,
                        FeedbackReadError, Message),
                    io.write_string(Stderr, Message, !IO),
                    io.set_exit_status(1, !IO)
                )
            ;
                MaybeDeep = error(Error),
                io.set_exit_status(1, !IO),
                io.format(Stderr, "%s: error reading %s: %s\n",
                    [s(ProgName), s(InputFileName), s(Error)], !IO)
            )
        ;
            io.set_exit_status(1, !IO),
            write_help_message(ProgName, !IO)
        )
    ;
        MaybeOptions = error(Msg),
        io.set_exit_status(1, !IO),
        io.format(Stderr, "%s: error parsing options: %s\n",
            [s(ProgName), s(Msg)], !IO),
        write_help_message(ProgName, !IO)
    ).

:- func help_message = string.

help_message =
"Usage: %s [<options>] <input> <output>
    <input> must name a deep profiling data file.
    <output> is the name of the file to be generated by this program.

    You may specify the following general options:

    -h --help       Generate this help message.
    -V --version    Report the program's version number.
    -v --verbosity  <0-4>
                    Generate messages.  The higher the argument the more
                    verbose the program becomes.  2 is recommended and the
                    default.
    --debug-read-profile
                    Generate debugging messages when reading the deep profile
                    and creating the deep structure.
    
    The following options select sets of feedback information useful
    for particular compiler optimizations:

    --implicit-parallelism
                Generate information that the compiler can use for automatic
                parallelization.
    --desired-parallelism <value>
                The amount of desired parallelism for implicit parallelism,
                value must be a floating point number above 1.0.
                Note: This option is currently ignored.
    --implicit-parallelism-sparking-cost <value>
                The cost of creating a spark, measured in the deep profiler's
                call sequence counts.
    --implicit-parallelism-locking-cost <value>
                The cost of maintaining a lock for a single dependant variable
                in a conjunction, measured in the profiler's call sequence
                counts.
    --implicit-parallelism-clique-cost-threshold <value>
                The cost threshold for cliques to be considered for implicit
                parallelism, measured on the profiler's call sequence counts.
    --implicit-parallelism-call-site-cost-threshold <value>
                The cost of a call site to be considered for parallelism
                against another call site.

    The following options select specific types of feedback information
    and parameterise them:

    --calls-above-threshold-sorted
                A list of calls whose typical cost (in call sequence counts) is
                above a given threshold. This option uses the
                --desired-parallelism option to specify the threshold,
                --calls-above-threshold-sorted-measure specifies what 'typical'
                means.  This option is deprecated.
    --calls-above-threshold-sorted-measure mean|median
                mean: Use mean(call site dynamic cost) as the typical cost.
                median: Use median(call site dynamic cost) as the typical cost.
                The default is 'mean'.

    --candidate-parallel-conjunctions
                Produce a list of candidate parallel conjunctions for implicit
                parallelism.  This option uses the implicit parallelism
                settings above.
    ".

:- pred write_help_message(string::in, io::di, io::uo) is det.

write_help_message(ProgName, !IO) :-
    Message = help_message,
    io.format(Message, [s(ProgName)], !IO).

:- pred write_version_message(string::in, io::di, io::uo) is det.

write_version_message(ProgName, !IO) :-
    library.version(Version),
    io.write_string(ProgName, !IO),
    io.write_string(": Mercury deep profiler", !IO),
    io.nl(!IO),
    io.write_string(Version, !IO),
    io.nl(!IO).

    % Read a deep profiling data file.
    %
:- pred read_deep_file(string::in, bool::in, maybe_error(deep)::out,
    io::di, io::uo) is det.

read_deep_file(Input, Debug, MaybeDeep, !IO) :-
    server_name_port(Machine, !IO),
    script_name(ScriptName, !IO),
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

%----------------------------------------------------------------------------%
%
% This section describes and processes command line options. Individual
% feedback information can be requested by the user, as well as options named
% after optimizations that may imply one or more feedback inforemation types,
% which that optimization uses.
%

    % Command line options.
    %
:- type option
    --->    help
    ;       version
    ;       verbosity
    ;       debug_read_profile

            % The calls above threshold sorted feedback information, this is
            % used for the old implicit parallelism implementation.
    ;       calls_above_threshold_sorted
    ;       calls_above_threshold_sorted_measure

            % A list of candidate parallel conjunctions is produced for the new
            % implicit parallelism implementation.
    ;       candidate_parallel_conjunctions

            % Provide suitable feedback information for implicit parallelism
    ;       implicit_parallelism
    ;       desired_parallelism
    ;       implicit_parallelism_sparking_cost
    ;       implicit_parallelism_locking_cost
    ;       implicit_parallelism_clique_cost_threshold
    ;       implicit_parallelism_call_site_cost_threshold.

% TODO: Introduce an option to disable parallelisation of dependant
% conjunctions, or switch to the simple calculations for independent
% conjunctions.

:- pred short(char::in, option::out) is semidet.

short('h',  help).
short('v',  verbosity).
short('V',  version).

:- pred long(string::in, option::out) is semidet.

long("help",                                help).
long("verbosity",                           verbosity).
long("version",                             version).
long("debug-read-profile",                  debug_read_profile).

long("calls-above-threshold-sorted",        calls_above_threshold_sorted).
long("calls-above-threshold-sorted-measure",
    calls_above_threshold_sorted_measure).

long("candidate-parallel-conjunctions",     candidate_parallel_conjunctions).

long("implicit-parallelism",                implicit_parallelism).

long("desired-parallelism",                 desired_parallelism).
long("implicit-parallelism-sparking-cost",  implicit_parallelism_sparking_cost).
long("implicit-parallelism-locking-cost",   implicit_parallelism_locking_cost).
long("implicit-parallelism-clique-cost-threshold", 
    implicit_parallelism_clique_cost_threshold).
long("implicit-parallelism-call-site-cost-threshold",
    implicit_parallelism_call_site_cost_threshold).

:- pred defaults(option::out, option_data::out) is multi.

defaults(help,                  bool(no)).
defaults(verbosity,             int(2)).
defaults(version,               bool(no)).
defaults(debug_read_profile,    bool(no)).

defaults(calls_above_threshold_sorted,                      bool(no)).
defaults(calls_above_threshold_sorted_measure,              string("mean")).

defaults(candidate_parallel_conjunctions,                   bool(no)).

defaults(implicit_parallelism,                              bool(no)).
defaults(desired_parallelism,                               string("4.0")).
% XXX: These values have been chosen arbitrarily, appropriately values should
% be tested for.
defaults(implicit_parallelism_sparking_cost,                int(100)).
defaults(implicit_parallelism_locking_cost,                 int(100)).
defaults(implicit_parallelism_clique_cost_threshold,        int(100000)).
defaults(implicit_parallelism_call_site_cost_threshold,     int(50000)).

:- pred construct_measure(string::in, stat_measure::out) is semidet.

construct_measure("mean",       stat_mean).
construct_measure("median",     stat_median).

    % This type defines the set of feedback_types that are to be calculated and
    % put into the feedback info file. They should correspond with the values
    % of feedback_type.
    %
:- type requested_feedback_info
    --->    requested_feedback_info(
                maybe_calls_above_threshold_sorted
                    :: maybe(calls_above_threshold_sorted_opts),
                maybe_candidate_parallel_conjunctions
                    :: maybe(candidate_parallel_conjunctions_opts)
            ).

:- pred check_verbosity_option(option_table(option)::in, int::out) is semidet.

check_verbosity_option(Options, VerbosityLevel) :-
    lookup_int_option(Options, verbosity, VerbosityLevel),
    VerbosityLevel >= 0, 
    VerbosityLevel =< 4.

    % Check all the command line options and return a well-typed representation
    % of the user's request. Some command line options imply other options,
    % those implications are also handled here.
    %
:- pred check_options(option_table(option)::in, requested_feedback_info::out)
    is det.

check_options(Options0, RequestedFeedbackInfo) :-
    % Handle options that imply other options here.
    some [!Options]
    (
        !:Options = Options0,
        lookup_bool_option(!.Options, implicit_parallelism,
            ImplicitParallelism),
        (
            ImplicitParallelism = yes,
            set_option(calls_above_threshold_sorted, bool(yes), !Options),
            set_option(candidate_parallel_conjunctions, bool(yes), !Options)
        ;
            ImplicitParallelism = no
        ),
        Options = !.Options
    ),
    
    % For each feedback type, determine if it is requested and fill in the
    % field in the RequestedFeedbackInfo structure.
    lookup_bool_option(Options, calls_above_threshold_sorted,
        CallsAboveThresholdSorted),
    (
        CallsAboveThresholdSorted = yes,
        lookup_string_option(Options, calls_above_threshold_sorted_measure,
            Measure),
        ( construct_measure(Measure, MeasureTypePrime) ->
            MeasureType = MeasureTypePrime
        ;
            error("Invalid value for calls_above_threshold_sorted_measure: " ++
                Measure)
        ),
        % Clique costs are used here.  They are almost equivalent to procedure
        % costs.
        lookup_int_option(Options, implicit_parallelism_clique_cost_threshold,
            CATSThreshold),
        CallsAboveThresholdSortedOpts =
            calls_above_threshold_sorted_opts(MeasureType, CATSThreshold),
        MaybeCallsAboveThresholdSortedOpts = yes(CallsAboveThresholdSortedOpts)
    ;
        CallsAboveThresholdSorted = no,
        MaybeCallsAboveThresholdSortedOpts = no
    ),
    lookup_bool_option(Options, candidate_parallel_conjunctions,
        CandidateParallelConjunctions),
    (
        CandidateParallelConjunctions = yes,
        lookup_string_option(Options, desired_parallelism,
            DesiredParallelismStr),
        (
            string.to_float(DesiredParallelismStr, DesiredParallelismPrime),
            DesiredParallelismPrime > 1.0
        ->
            DesiredParallelism = DesiredParallelismPrime
        ;
            error("Invalid value for desired_parallelism: " ++ 
                DesiredParallelismStr)
        ),
        lookup_int_option(Options, implicit_parallelism_sparking_cost,
            SparkingCost),
        lookup_int_option(Options, implicit_parallelism_locking_cost,
            LockingCost),
        lookup_int_option(Options, implicit_parallelism_clique_cost_threshold,
            CPCProcThreshold),
        lookup_int_option(Options, 
            implicit_parallelism_call_site_cost_threshold,
            CPCCallSiteThreshold),
        CandidateParallelConjunctionsOpts =
            candidate_parallel_conjunctions_opts(DesiredParallelism, 
                SparkingCost,
                LockingCost,
                CPCProcThreshold,
                CPCCallSiteThreshold),
        MaybeCandidateParallelConjunctionsOpts =
            yes(CandidateParallelConjunctionsOpts)
    ;
        CandidateParallelConjunctions = no,
        MaybeCandidateParallelConjunctionsOpts = no
    ),
    RequestedFeedbackInfo =
        requested_feedback_info(MaybeCallsAboveThresholdSortedOpts,
            MaybeCandidateParallelConjunctionsOpts).

    % Adjust command line options when one option implies other options.
    %
:- pred option_implies(option::in, option::in, bool::in,
    option_table(option)::in, option_table(option)::out) is det.

option_implies(Option, ImpliedOption, ImpliedValue, !Options) :-
    ( lookup_bool_option(!.Options, Option, yes) ->
        set_option(ImpliedOption, bool(ImpliedValue), !Options)
    ;
        true
    ).

    % Set the value of an option in the option table.
    %
:- pred set_option(option::in, option_data::in,
    option_table(option)::in, option_table(option)::out) is det.

set_option(Option, Value, !Options) :-
    svmap.set(Option, Value, !Options).

%----------------------------------------------------------------------------%

    % process_deep_to_feedback(RequestedFeedbackInfo, Deep, Messages,
    %   !Feedback)
    %
    % Process a deep profiling structure and update the feedback information
    % according to the RequestedFeedbackInfo parameter.
    %
:- pred process_deep_to_feedback(requested_feedback_info::in, deep::in,
    cord(message)::out, feedback_info::in, feedback_info::out) is det.

process_deep_to_feedback(RequestedFeedbackInfo, Deep, Messages, !Feedback) :-
    MaybeCallsAboveThresholdSortedOpts =
        RequestedFeedbackInfo ^ maybe_calls_above_threshold_sorted,
    (
        MaybeCallsAboveThresholdSortedOpts = 
            yes(CallsAboveThresholdSortedOpts),
        css_list_above_threshold(CallsAboveThresholdSortedOpts, Deep,
            !Feedback)
    ;
        MaybeCallsAboveThresholdSortedOpts = no
    ),

    MaybeCandidateParallelConjunctionsOpts =
        RequestedFeedbackInfo ^ maybe_candidate_parallel_conjunctions,
    (
        MaybeCandidateParallelConjunctionsOpts = 
            yes(CandidateParallelConjunctionsOpts),
        candidate_parallel_conjunctions(CandidateParallelConjunctionsOpts,
            Deep, Messages, !Feedback)
    ;
        MaybeCandidateParallelConjunctionsOpts = no,
        Messages = cord.empty
    ).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "mdprof_feedback: ".

%-----------------------------------------------------------------------------%
:- end_module mdprof_feedback.
%-----------------------------------------------------------------------------%
