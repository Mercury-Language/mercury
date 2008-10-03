%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2006-2008 The University of Melbourne.
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
:- import_module mdbcomp.program_representation.
:- import_module measurements.
:- import_module profile.
:- import_module startup.

:- import_module array.
:- import_module bool.
:- import_module cord.
:- import_module char.
:- import_module float.
:- import_module getopt.
:- import_module int.
:- import_module library.
:- import_module list.
:- import_module map.
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
    getopt.process_options(option_ops_multi(short, long, defaults),
        Args0, Args, MaybeOptions),
    (
        MaybeOptions = ok(Options),
        lookup_bool_option(Options, help, Help),
        lookup_bool_option(Options, version, Version),
        lookup_string_option(Options, program_name, ProfileProgName0),
        ProfileProgName = string.strip(ProfileProgName0),
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
            check_options(Options, RequestedFeedbackInfo)
        ->
            lookup_bool_option(Options, verbose, Verbose),
            read_deep_file(InputFileName, Verbose, MaybeDeep, !IO),
            (
                MaybeDeep = ok(Deep),
                feedback.read_or_create(OutputFileName, FeedbackReadResult,
                    !IO),
                (
                    FeedbackReadResult = ok(Feedback0),
                    process_deep_to_feedback(RequestedFeedbackInfo,
                        Deep, Feedback0, Feedback),
                    write_feedback_file(OutputFileName, ProfileProgName,
                        Feedback, WriteResult, !IO),
                    (
                        WriteResult = ok
                    ;
                        ( WriteResult = open_error(Error)
                        ; WriteResult = write_error(Error)
                        ),
                        io.error_message(Error, ErrorMessage),
                        io.stderr_stream(Stderr, !IO),
                        io.format(Stderr, "%s: %s\n",
                            [s(OutputFileName), s(ErrorMessage)], !IO),
                        io.set_exit_status(1, !IO)
                    )
                ;
                    FeedbackReadResult = error(FeedbackReadError),
                    feedback.read_error_message_string(OutputFileName,
                        FeedbackReadError, Message),
                    io.stderr_stream(Stderr, !IO),
                    io.write_string(Stderr, Message, !IO),
                    io.set_exit_status(1, !IO)
                )
            ;
                MaybeDeep = error(Error),
                io.stderr_stream(Stderr, !IO),
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
        io.stderr_stream(Stderr, !IO),
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

    --help      Generate this help message.
    --version   Report the program's version number.
    --verbose   Generate progress messages.
    --program-name <name>
                The name of the program that generated the profiling data.
                This is stored in the feedback file.

    The following options select sets of feedback information useful
    for particular compiler optimizations:

    --implicit-parallelism
                Generate information that the compiler can use for automatic
                parallelization.
    --desired-parallelism <value>
                The amount of desired parallelism for implicit parallelism,
                value must be a floating point number above 1.0.
    --implicit-parallelism-sparking-cost <value>
                The cost of creating a spark, measured in the deep profiler's
                call sequence counts.
    --implicit-parallelism-locking-cost <value>
                The cost of maintaining a lock for a single dependant variable
                in a conjunction, measured in the profiler's call sequence
                counts.
    --implicit-parallelism-proc-cost-threshold <value>
                The cost threshold for procedures to be considered for implicit
                parallelism, measured on the profiler's call sequence counts.

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

read_deep_file(Input, Verbose, MaybeDeep, !IO) :-
    server_name_port(Machine, !IO),
    script_name(ScriptName, !IO),
    (
        Verbose = yes,
        io.stdout_stream(Stdout, !IO),
        MaybeOutput = yes(Stdout)
    ;
        Verbose = no,
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
    ;       program_name
    ;       verbose
    ;       version

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
    ;       implicit_parallelism_proc_cost_threshold.

:- pred short(char::in, option::out) is semidet.

short('h',  help).
short('p',  program_name).
short('V',  verbose).
short('v',  version).

:- pred long(string::in, option::out) is semidet.

long("help",                                help).
long("verbose",                             verbose).
long("version",                             version).
long("program-name",                        program_name).

long("calls-above-threshold-sorted",        calls_above_threshold_sorted).
long("calls-above-threshold-sorted-measure",
    calls_above_threshold_sorted_measure).

long("candidate-parallel-conjunctions",     candidate_parallel_conjunctions).

long("implicit-parallelism",                implicit_parallelism).

long("desired-parallelism",                 desired_parallelism).
long("implicit-parallelism-sparking-cost",  implicit_parallelism_sparking_cost).
long("implicit-parallelism-locking-cost",   implicit_parallelism_locking_cost).
long("implicit-parallelism-proc-cost-threshold", 
    implicit_parallelism_proc_cost_threshold).

:- pred defaults(option::out, option_data::out) is multi.

defaults(help,              bool(no)).
defaults(program_name,      string("")).
defaults(verbose,           bool(no)).
defaults(version,           bool(no)).

defaults(calls_above_threshold_sorted,              bool(no)).
defaults(calls_above_threshold_sorted_measure,      string("mean")).

defaults(candidate_parallel_conjunctions,           bool(no)).

defaults(implicit_parallelism,                      bool(no)).
defaults(desired_parallelism,                       string("4.0")).
% XXX: These values have been chosen arbitrarily, appropriately values should
% be tested for.
defaults(implicit_parallelism_sparking_cost,        int(100)).
defaults(implicit_parallelism_locking_cost,         int(100)).
defaults(implicit_parallelism_proc_cost_threshold,  int(100000)).

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

:- type calls_above_threshold_sorted_opts
    --->    calls_above_threshold_sorted_opts(
                cats_measure                :: stat_measure,
                cats_threshold              :: int
            ).

:- type candidate_parallel_conjunctions_opts
    --->    candidate_parallel_conjunctions_opts(
                cpc_desired_parallelism     :: float,
                cpc_sparking_cost           :: int,
                cpc_locking_cost            :: int,
                cpc_threshold               :: int
            ).

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
        lookup_int_option(Options, implicit_parallelism_proc_cost_threshold,
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
            string.to_float(DesiredParallelismStr, DesiredParallelism),
            DesiredParallelism > 1.0
        ->
            CandidateParallelConjunctionsOpts ^ cpc_desired_parallelism =
                DesiredParallelism
        ;
            error("Invalid value for desired_parallelism: " ++ 
                DesiredParallelismStr)
        ),
        lookup_int_option(Options, implicit_parallelism_sparking_cost,
            SparkingCost),
        CandidateParallelConjunctionsOpts ^ cpc_sparking_cost = SparkingCost,
        lookup_int_option(Options, implicit_parallelism_locking_cost,
            LockingCost),
        CandidateParallelConjunctionsOpts ^ cpc_locking_cost = LockingCost,
        lookup_int_option(Options, implicit_parallelism_proc_cost_threshold,
            CPCThreshold),
        CandidateParallelConjunctionsOpts ^ cpc_threshold = CPCThreshold,
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

    % process_deep_to_feedback(RequestedFeedbackInfo, Deep, !Feedback)
    %
    % Process a deep profiling structure and update the feedback information
    % according to the RequestedFeedbackInfo parameter.
    %
:- pred process_deep_to_feedback(requested_feedback_info::in, deep::in,
    feedback_info::in, feedback_info::out) is det.

process_deep_to_feedback(RequestedFeedbackInfo, Deep, !Feedback) :-
    MaybeCallsAboveThresholdSorted =
        RequestedFeedbackInfo ^ maybe_calls_above_threshold_sorted,
    (
        MaybeCallsAboveThresholdSorted = yes(Opts),
        css_list_above_threshold(Opts, Deep, !Feedback)
    ;
        MaybeCallsAboveThresholdSorted = no
    ).

%----------------------------------------------------------------------------%
%
% Jerome's implicit parallelism feedback information.
%

    % Perform Jerome's analysis and update the feedback info structure.
    %
:- pred css_list_above_threshold(calls_above_threshold_sorted_opts::in,
    deep::in, feedback_info::in, feedback_info::out) is det.

css_list_above_threshold(Options, Deep, !Feedback) :-
    Options = calls_above_threshold_sorted_opts(MeasureType, Threshold),
    compute_css_list_above_threshold(0, Deep, Threshold,
        MeasureType, cord.empty, AboveThresholdCSSCord),
    AboveThresholdCSSs = cord.list(AboveThresholdCSSCord),
    list.map(css_to_call(Deep), AboveThresholdCSSs, Calls),
    FeedbackData = feedback_data_calls_above_threshold_sorted(Threshold,
        MeasureType, Calls),
    put_feedback_data(FeedbackData, !Feedback).

    % Determine those CSSs whose CSDs' average/median call sequence counts
    % exceed the given threshold.
    %
:- pred compute_css_list_above_threshold(int::in, deep::in, int::in,
    stat_measure::in,
    cord(call_site_static)::in, cord(call_site_static)::out) is det.

compute_css_list_above_threshold(Index, Deep, Threshold, Measure, !CSSCord) :-
    array.size(Deep ^ call_site_statics, Size),
    ( Index = Size ->
        true
    ;
        CallSiteCall = array.lookup(Deep ^ call_site_calls, Index),
        CSDListList = map.values(CallSiteCall),
        CSDList = list.condense(CSDListList),
        list.length(CSDList, NumCSD),
        ( NumCSD = 0 ->
            % The CSS doesn't have any CSDs.
            CallSeqs = 0
        ;
            (
                Measure = stat_mean,
                list.foldr(sum_callseqs_csd_ptr(Deep), CSDList,
                    0, SumCallSeqs),
                % NOTE: we have checked that NumCSD is not zero above.
                CallSeqs = SumCallSeqs // NumCSD
            ;
                Measure = stat_median,
                list.sort(compare_csd_ptr(Deep), CSDList, CSDListSorted),
                IndexMedian = NumCSD // 2,
                list.index0_det(CSDListSorted, IndexMedian, MedianPtr),
                sum_callseqs_csd_ptr(Deep, MedianPtr, 0, CallSeqs)
            )
        ),
        ( CallSeqs >= Threshold ->
            CSS = array.lookup(Deep ^ call_site_statics, Index),
            !:CSSCord = snoc(!.CSSCord, CSS),
            compute_css_list_above_threshold(Index + 1, Deep, Threshold,
                Measure, !CSSCord)
        ;
            compute_css_list_above_threshold(Index + 1, Deep, Threshold,
                Measure, !CSSCord)
        )
    ).

    % Add the call sequence counts (own and desc) of CSDPtr to the accumulator.
    %
:- pred sum_callseqs_csd_ptr(deep::in, call_site_dynamic_ptr::in,
    int::in, int::out) is det.

sum_callseqs_csd_ptr(Deep, CSDPtr, !Sum) :-
    lookup_call_site_dynamics(Deep ^ call_site_dynamics, CSDPtr, CSD),
    lookup_csd_desc(Deep ^ csd_desc, CSDPtr, IPO),
    !:Sum = !.Sum + callseqs(CSD ^ csd_own_prof) + inherit_callseqs(IPO).

    % Compare two CSD pointers on the basis of their call sequence counts
    % (own and desc).
    %
:- pred compare_csd_ptr(deep::in, call_site_dynamic_ptr::in,
    call_site_dynamic_ptr::in, comparison_result::out) is det.

compare_csd_ptr(Deep, CSDPtrA, CSDPtrB, Result) :-
    sum_callseqs_csd_ptr(Deep, CSDPtrA, 0, SumA),
    sum_callseqs_csd_ptr(Deep, CSDPtrB, 0, SumB),
    compare(Result, SumA, SumB).

    % Write to the output the list of CSSs.
    %
:- pred css_to_call(deep::in, call_site_static::in, call_site::out) is det.

css_to_call(Deep, CSS, Call) :-
    % Get the caller.
    lookup_proc_statics(Deep ^ proc_statics, CSS ^ css_container, CallerPS),
    Caller = CallerPS ^ ps_id,

    % Get the slot number.
    Slot = CSS ^ css_slot_num,

    % Get the Callee and Call Type.
    (
        CSS ^ css_kind = normal_call_and_callee(PSPtr, _),
        lookup_proc_statics(Deep ^ proc_statics, PSPtr, CalleePS),
        CallTypeAndCallee = plain_call(CalleePS ^ ps_id)
    ;
        CSS ^ css_kind = special_call_and_no_callee,
        CallTypeAndCallee = special_call
    ;
        CSS ^ css_kind = higher_order_call_and_no_callee,
        CallTypeAndCallee = higher_order_call
    ;
        CSS ^ css_kind = method_call_and_no_callee,
        CallTypeAndCallee = method_call
    ;
        CSS ^ css_kind = callback_and_no_callee,
        CallTypeAndCallee = callback_call
    ),

    % Build the call datastructure.
    Call = call_site(Caller, Slot, CallTypeAndCallee).

%-----------------------------------------------------------------------------%
:- end_module mdprof_feedback.
%-----------------------------------------------------------------------------%
