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
:- import_module create_report.
:- import_module mdbcomp.
:- import_module mdbcomp.feedback.
:- import_module mdbcomp.program_representation.
:- import_module measurements.
:- import_module profile.
:- import_module program_representation_utils.
:- import_module query. % For the cmd structure
:- import_module report.
:- import_module startup.

:- import_module array.
:- import_module assoc_list.
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
:- import_module pair.
:- import_module pqueue.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module svmap.
:- import_module svset.

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
                Note: This option is currently ignored.
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
    ;       implicit_parallelism_proc_cost_threshold
    ;       implicit_parallelism_call_site_cost_threshold.

% TODO: Introduce an option to disable parallelisation of dependant
% conjunctions, or switch to the simple calculations for independant
% conjunctions.

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
long("implicit-parallelism-call-site-cost-threshold",
    implicit_parallelism_call_site_cost_threshold).

:- pred defaults(option::out, option_data::out) is multi.

defaults(help,              bool(no)).
defaults(program_name,      string("")).
defaults(verbose,           bool(no)).
defaults(version,           bool(no)).

defaults(calls_above_threshold_sorted,                      bool(no)).
defaults(calls_above_threshold_sorted_measure,              string("mean")).

defaults(candidate_parallel_conjunctions,                   bool(no)).

defaults(implicit_parallelism,                              bool(no)).
defaults(desired_parallelism,                               string("4.0")).
% XXX: These values have been chosen arbitrarily, appropriately values should
% be tested for.
defaults(implicit_parallelism_sparking_cost,                int(100)).
defaults(implicit_parallelism_locking_cost,                 int(100)).
defaults(implicit_parallelism_proc_cost_threshold,          int(100000)).
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
                cpc_proc_threshold          :: int,
                cpc_call_site_threshold     :: int
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
        lookup_int_option(Options, implicit_parallelism_proc_cost_threshold,
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

    % process_deep_to_feedback(RequestedFeedbackInfo, Deep, !Feedback)
    %
    % Process a deep profiling structure and update the feedback information
    % according to the RequestedFeedbackInfo parameter.
    %
:- pred process_deep_to_feedback(requested_feedback_info::in, deep::in,
    feedback_info::in, feedback_info::out) is det.

process_deep_to_feedback(RequestedFeedbackInfo, Deep, !Feedback) :-
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
            Deep, !Feedback)
    ;
        MaybeCandidateParallelConjunctionsOpts = no
    ).

%----------------------------------------------------------------------------%
%
% Build the candidate parallel conjunctions feedback information used for
% implicit parallelism.
%

%
% This doesn't follow higher order or method calls yet it may be possible to
% follow all the higher order and method calls seen during profiling and
% average their statistics based on their execution probability (weight).
%

:- pred candidate_parallel_conjunctions(
    candidate_parallel_conjunctions_opts::in, deep::in,
    feedback_info::in, feedback_info::out) is det.

candidate_parallel_conjunctions(Opts, Deep, !Feedback) :-
    Opts = candidate_parallel_conjunctions_opts(DesiredParallelism,
        SparkingCost, LockingCost, ProcThreshold, _CallSiteThreshold), 
    % First retrieve the top procedures above the configured threshold.
    % This is done 'overall' not 'per_call' because parallelising a procedure
    % that is used a lot is more beneficial than parallelising a procedure that
    % is used once when the more-frequently used function accounts for a higher
    % amount of the programs runtime.  It may be disqualified later if per call
    % it has very little benefit.
    % TODO: don't bother using the 'create_report' interface to get this
    % report, we should call it directly instead.
    TopProcsCmd = deep_cmd_top_procs(threshold_value(float(ProcThreshold)),
        cost_callseqs, self_and_desc, overall),
    create_report(TopProcsCmd, Deep, TopProcsReport),
    ( TopProcsReport = report_top_procs(MaybeTopProcsReport) ->
        (
            MaybeTopProcsReport = ok(TopProcs),
            TopProcsList = TopProcs ^ tp_top_procs
        ;
            MaybeTopProcsReport = error(TopProcsReportError),
            error(TopProcsReportError)
        )
    ;
        error("create_report gave incorrect report, expected top_procs_report")
    ),
    
    % Take the top procs list and look for conjunctions that can be
    % parallelised and give an estimated speedup when parallelised.  There may
    % be more than one opportunity for parallelism in any given procedure.
    list.map(
        candidate_parallel_conjunctions_proc(Opts, Deep), 
        TopProcsList, Conjunctions0),
    list.condense(Conjunctions0, Conjunctions),

    % XXX: Analysing the clique tree to reduce the amount of nested parallel
    % execution should be done here.
    
    CandidateParallelConjunctions =
        feedback_data_candidate_parallel_conjunctions(DesiredParallelism,
        SparkingCost, LockingCost, Conjunctions),
    put_feedback_data(CandidateParallelConjunctions, !Feedback).

:- type implicit_parallelism_info
    --->    implicit_parallelism_info(
                ipi_deep        :: deep,
                ipi_progrep     :: prog_rep,
                ipi_opts        :: candidate_parallel_conjunctions_opts,
                ipi_call_sites  :: map(goal_path, call_site_perf),
                ipi_var_table   :: var_table
            ).

    % Find candidate parallel conjunctions within the given procedure.
    %
:- pred candidate_parallel_conjunctions_proc(
    candidate_parallel_conjunctions_opts::in, deep::in,
    perf_row_data(proc_desc)::in,
    assoc_list(string_proc_label, candidate_par_conjunction)::out) is det.

candidate_parallel_conjunctions_proc(Opts, Deep, PrefRowData, 
        Candidates) :-
    % Lookup the proc static to find the ProcLabel.
    PSPtr = PrefRowData ^ perf_row_subject ^ pdesc_ps_ptr,  
    deep_lookup_proc_statics(Deep, PSPtr, PS),
    ProcLabel = PS ^ ps_id,
    
    % Make a proc query to retrieve cost information for the call sites within
    % the procedure.
    % TODO: As above: this can probably be called directly rather than going via
    % create_report.
    create_report(deep_cmd_proc(PSPtr), Deep, Report),
    ( Report = report_proc(MaybeProcReport) ->
        det_open_maybe_error(MaybeProcReport, ProcReport)
    ;
        error(this_file ++ "Recieved incorrect report for proc report query")
    ),
    list.foldl(add_call_site_perf_to_map,
        ProcReport ^ proc_call_site_summaries, map.init, CallSitesMap),
    MaybeProgRep = Deep ^ procrep_file,
    (
        MaybeProgRep = yes(MaybeProgRep1),
        (
            MaybeProgRep1 = ok(ProgRep)
        ;
            MaybeProgRep1 = error(Error),
            error(this_file ++ Error)
        )
    ;
        MaybeProgRep = no,
        error(this_file ++ "Could not open Deep.procrep")
    ),
    ( progrep_search_proc(ProgRep, ProcLabel, ProcRep) ->
        ProcRep ^ pr_defn = ProcDefnRep,
        ProcDefnRep ^ pdr_goal = Goal,
        ProcDefnRep ^ pdr_var_table = VarTable,
        Info = implicit_parallelism_info(Deep, ProgRep, Opts, CallSitesMap,
            VarTable),
        goal_get_conjunctions_worth_parallelising(Goal, empty_goal_path, Info,
            initial_inst_map(ProcDefnRep), _, Candidates0,
            SeenDuplicateInstantiation, _),
        (
            SeenDuplicateInstantiation = seen_duplicate_instantiation,
            Candidates = []
        ;
            SeenDuplicateInstantiation = have_not_seen_duplicate_instantiation,
            list.map((pred(Candidate0::in, Candidate::out) is det :-
                    Candidate = (ProcLabel - Candidate0)
                ), Candidates0, Candidates)
        )
    ;
        % Builtin procedures cannot be found in the program representation, and
        % cannot be parallelised either.
        Candidates = []
    ).
    
:- pred goal_get_conjunctions_worth_parallelising(goal_rep::in, goal_path::in,
    implicit_parallelism_info::in, inst_map::in, inst_map::out,
    list(candidate_par_conjunction)::out, seen_duplicate_instantiation::out,
    maybe_call_conjunct::out) is det.

goal_get_conjunctions_worth_parallelising(Goal, GoalPath, Info, !InstMap,
        Candidates, SeenDuplicateInstantiation, MaybeCallConjunct) :-
    Goal = goal_rep(GoalExpr, Detism, _),
    (
        (
            GoalExpr = conj_rep(Conjuncts),
            conj_get_conjunctions_worth_parallelising(Conjuncts, GoalPath,
                Info, !InstMap, Candidates, SeenDuplicateInstantiation)
        ;
            GoalExpr = disj_rep(Disjuncts),
            disj_get_conjunctions_worth_parallelising(Disjuncts, GoalPath, 1,
                Info, !InstMap, Candidates, SeenDuplicateInstantiation)
        ;
            GoalExpr = switch_rep(_, _, Cases),
            switch_case_get_conjunctions_worth_parallelising(Cases, GoalPath, 1,
                Info, !InstMap, Candidates, SeenDuplicateInstantiation)
        ;
            GoalExpr = ite_rep(Cond, Then, Else),
            ite_get_conjunctions_worth_parallelising(Cond, Then, Else,
                GoalPath, Info, !InstMap, Candidates,
                SeenDuplicateInstantiation)
        ;
            GoalExpr = scope_rep(SubGoal, MaybeCut),
            ScopeGoalPath = 
                goal_path_add_at_end(GoalPath, step_scope(MaybeCut)),
            goal_get_conjunctions_worth_parallelising(SubGoal, ScopeGoalPath,
                Info, !InstMap, Candidates, SeenDuplicateInstantiation, _) 
        ;
            GoalExpr = negation_rep(SubGoal),
            NegGoalPath = goal_path_add_at_end(GoalPath, step_neg),
            goal_get_conjunctions_worth_parallelising(SubGoal, NegGoalPath,
                Info, !InstMap, Candidates, SeenDuplicateInstantiation, _) 
        ),
        % TODO: Parallelising conjunctions like 
        %   ( call1(A, B) , not call2(C, D) )
        % may be easy to do when writing the compiler's part of the code, if so
        % then MaybeCallAboveThreshold should probably be set from some of
        % these non-atomic goals based on goals within them.
        MaybeCallConjunct = non_atomic_goal 
    ;
        GoalExpr = atomic_goal_rep(_, _, BoundVars, AtomicGoal),
        InstMapBeforeCall = !.InstMap,
        % The binding of a variable may depend on any number of other
        % variables, and of course the variables they depend upon.  Except that
        % variables involved in control flow (switched on vars, vars in ITE
        % conds) are not recorded here, but should be for completeness.
        atomic_goal_get_vars(AtomicGoal, AtomicGoalVars0),
        list.foldl((pred(Var::in, Set0::in, Set::out) is det :-
                ( set.remove(Set0, Var, SetPrime) ->
                    Set = SetPrime
                ;
                    Set = Set0
                )
            ), BoundVars, AtomicGoalVars0, AtomicGoalVars),
        inst_map_ground_vars(BoundVars, AtomicGoalVars, !InstMap,
            SeenDuplicateInstantiation),
        maybe_call_site_conjunct(Info, GoalPath, AtomicGoal, Detism,
            InstMapBeforeCall, !.InstMap, BoundVars, MaybeCallConjunct),
        Candidates = []
    ).

:- pred conj_get_conjunctions_worth_parallelising(list(goal_rep)::in,
    goal_path::in, implicit_parallelism_info::in, inst_map::in,
    inst_map::out, list(candidate_par_conjunction)::out,
    seen_duplicate_instantiation::out) is det.

conj_get_conjunctions_worth_parallelising(Conjs, GoalPath, Info,
        !InstMap, Candidates, SeenDuplicateInstantiation) :-
    % Note: it will be better to look at each pair of conjuncts, determine if
    % they are parallelisable (perhaps by placing middle goals in either of the
    % the parallel conjuncts to create the optimum amount of parallelism.  This
    % will need to have an in-order representation of goals, and for each
    % variable seen have a tree of variables it depends upon.
    %
    % For now consider parallelising conjuncts that seperated only by other
    % atomic goals.
    conj_get_conjunctions_worth_parallelising_2(Conjs, GoalPath, 1, Info,
        !InstMap, Candidates0, CallSiteConjuncts, 
        SeenDuplicateInstantiation),
    (
        % Only perform analysis at this point if it's not going to be
        % thrown away later due to unhandled use of partial instantiation.
        SeenDuplicateInstantiation = have_not_seen_duplicate_instantiation,
        build_candidate_conjunctions(Info, !.InstMap, GoalPath,
            list(CallSiteConjuncts), pqueue.init, CandidatesQueue),
        % Pick best candidate from queue.
        ( pqueue.remove(CandidatesQueue, _, Candidate, _) ->
            Candidates = [Candidate | Candidates0]
        ;
            Candidates = Candidates0
        )
    ;
        SeenDuplicateInstantiation = seen_duplicate_instantiation,
        Candidates = Candidates0
    ). 

:- pred conj_get_conjunctions_worth_parallelising_2(list(goal_rep)::in,
    goal_path::in, int::in, implicit_parallelism_info::in, 
    inst_map::in, inst_map::out, list(candidate_par_conjunction)::out,
    cord(maybe_call_conjunct)::out,
    seen_duplicate_instantiation::out) is det.

conj_get_conjunctions_worth_parallelising_2([], _, _, _, !InstMap, [], cord.empty, 
        have_not_seen_duplicate_instantiation).
conj_get_conjunctions_worth_parallelising_2([Conj | Conjs], GoalPath,
        ConjunctNum, Info, !InstMap, Candidates, CallSiteConjuncts,
        SeenDuplicateInstantiation) :-
    ConjGoalPath = goal_path_add_at_end(GoalPath, step_conj(ConjunctNum)),
    goal_get_conjunctions_worth_parallelising(Conj, ConjGoalPath, Info,
        !InstMap, CandidatesHead, SeenDuplicateInstantiationHead,
        MaybeCallSiteConjunct), 
    
    conj_get_conjunctions_worth_parallelising_2(Conjs, GoalPath, ConjunctNum+1,
        Info, !InstMap, CandidatesTail, CallSiteConjuncts0,
        SeenDuplicateInstantiationTail),

    Candidates = CandidatesHead ++ CandidatesTail,
    CallSiteConjuncts = cord.cons(MaybeCallSiteConjunct, CallSiteConjuncts0),
    SeenDuplicateInstantiation = merge_seen_duplicate_instantiation(
        SeenDuplicateInstantiationHead,
        SeenDuplicateInstantiationTail).

:- pred disj_get_conjunctions_worth_parallelising(list(goal_rep)::in,
    goal_path::in, int::in, implicit_parallelism_info::in, inst_map::in,
    inst_map::out, list(candidate_par_conjunction)::out,
    seen_duplicate_instantiation::out) is det.

disj_get_conjunctions_worth_parallelising([], _, _, _, !InstMap, [],
    have_not_seen_duplicate_instantiation).
disj_get_conjunctions_worth_parallelising([Disj | Disjs], GoalPath, DisjNum,
        Info, InstMap0, InstMap, Candidates, SeenDuplicateInstantiation) :-
    DisjGoalPath = goal_path_add_at_end(GoalPath, step_disj(DisjNum)),
    HeadDetism = Disj ^ goal_detism_rep,
    goal_get_conjunctions_worth_parallelising(Disj, DisjGoalPath, Info,
        InstMap0, HeadInstMap, HeadCandidates, HeadSeenDuplicateInstantiation, 
        _MaybeCallConjunct),
    disj_get_conjunctions_worth_parallelising(Disjs, GoalPath, DisjNum + 1,
        Info, InstMap0, TailInstMap, TailCandidates,
        TailSeenDuplicateInstantiation),
    Candidates = HeadCandidates ++ TailCandidates,
    % merge_inst_map requires the detism of goals that produce both inst maps,
    % we can create fake values that satisfy merge_inst_map easily.
    (
        Disjs = [],
        TailDetism = failure_rep
    ;
        Disjs = [_ | _],
        TailDetism = det_rep
    ),
    InstMap = merge_inst_map(HeadInstMap, HeadDetism, TailInstMap, TailDetism),
    SeenDuplicateInstantiation = merge_seen_duplicate_instantiation(
        HeadSeenDuplicateInstantiation,
        TailSeenDuplicateInstantiation).

:- pred switch_case_get_conjunctions_worth_parallelising(list(case_rep)::in,
    goal_path::in, int::in, implicit_parallelism_info::in, inst_map::in,
    inst_map::out, list(candidate_par_conjunction)::out,
    seen_duplicate_instantiation::out) is det.

switch_case_get_conjunctions_worth_parallelising([], _, _, _, !InstMap, [],
    have_not_seen_duplicate_instantiation).
switch_case_get_conjunctions_worth_parallelising([Case | Cases], GoalPath,
        CaseNum, Info, InstMap0, InstMap, Candidates,
        SeenDuplicateInstantiation) :-
    Case = case_rep(_, _, Goal),
    HeadDetism = Goal ^ goal_detism_rep,
    CaseGoalPath = goal_path_add_at_end(GoalPath, step_switch(CaseNum, no)),
    goal_get_conjunctions_worth_parallelising(Goal, CaseGoalPath, Info,
        InstMap0, HeadInstMap, HeadCandidates, HeadSeenDuplicateInstantiation, 
        _MaybeCallConjs),
    switch_case_get_conjunctions_worth_parallelising(Cases, GoalPath, 
        CaseNum + 1, Info, InstMap0, TailInstMap, TailCandidates,
        TailSeenDuplicateInstantiation),
    Candidates = HeadCandidates ++ TailCandidates,
    % merge_inst_map requires the detism of goals that produce both inst maps,
    % we can create fake values that satisfy merge_inst_map easily.
    (
        Cases = [],
        TailDetism = failure_rep
    ;
        Cases = [_ | _],
        TailDetism = det_rep
    ),
    InstMap = merge_inst_map(HeadInstMap, HeadDetism, TailInstMap, TailDetism),
    SeenDuplicateInstantiation = merge_seen_duplicate_instantiation(
        HeadSeenDuplicateInstantiation,
        TailSeenDuplicateInstantiation).

:- pred ite_get_conjunctions_worth_parallelising(goal_rep::in, goal_rep::in,
    goal_rep::in, goal_path::in, implicit_parallelism_info::in, inst_map::in,
    inst_map::out, list(candidate_par_conjunction)::out,
    seen_duplicate_instantiation::out) is det.

ite_get_conjunctions_worth_parallelising(Cond, Then, Else, GoalPath, Info,
        !InstMap, Candidates, SeenDuplicateInstantiation) :-
    CondGoalPath = goal_path_add_at_end(GoalPath, step_ite_cond),
    ThenGoalPath = goal_path_add_at_end(GoalPath, step_ite_then),
    ElseGoalPath = goal_path_add_at_end(GoalPath, step_ite_else),
    goal_get_conjunctions_worth_parallelising(Cond, CondGoalPath, Info,
        !.InstMap, PostCondInstMap, CondCandidates,
        CondSeenDuplicateInstantiation, _),
    goal_get_conjunctions_worth_parallelising(Then, ThenGoalPath, Info, 
        PostCondInstMap, PostThenInstMap, ThenCandidates,
        ThenSeenDuplicateInstantiation, _),
    goal_get_conjunctions_worth_parallelising(Else, ElseGoalPath, Info, 
        PostCondInstMap, PostElseInstMap, ElseCandidates,
        ElseSeenDuplicateInstantiation, _),
    Candidates = CondCandidates ++ ThenCandidates ++ ElseCandidates,
    (
        CondSeenDuplicateInstantiation = have_not_seen_duplicate_instantiation,
        ThenSeenDuplicateInstantiation = have_not_seen_duplicate_instantiation,
        ElseSeenDuplicateInstantiation = have_not_seen_duplicate_instantiation
    ->
        SeenDuplicateInstantiation = have_not_seen_duplicate_instantiation
    ;
        SeenDuplicateInstantiation = seen_duplicate_instantiation
    ),
    ThenDetism = Then ^ goal_detism_rep,
    ElseDetism = Else ^ goal_detism_rep,
    !:InstMap = merge_inst_map(PostThenInstMap, ThenDetism, 
        PostElseInstMap, ElseDetism).

    % This type represents a goal, if the goal is a call extra information used
    % for parallelising it with another call is provided.
    %
    % This is similar to candidate_parallel_conjunct, except that it stores
    % information that's used for the rest of the implicit parallelism
    % analysis.  It must contain information for the following.
    %
    %   - Average cost information for this call site,
    %
    %   - Enough information to resolve the procedure call, (detism and
    %     argument modes).
    %
    %   - Information that can be used to determine if this is part of a
    %     dependant conjunction, and its role in the dependant conjunction.
    %
    %   - Enough information so that a candidate_par_conjuct structure can be
    %     constructed from it and the deep profiling info.
    %
:- type maybe_call_conjunct 
    --->    call(
                mccc_callee             :: maybe(pair(string, string)),
                mccc_detism             :: detism_rep,
                mccc_args               :: list(var_mode_and_use),
                mccc_perf               :: call_site_perf
            )
    ;       trivial_atomic_goal(
                mccag_detism            :: detism_rep,
                mccag_bound_vars        :: list(var_rep)
            )
    ;       non_atomic_goal.

:- inst call
    --->    call(ground, ground, ground, ground).

    % A variable, it's mode and it's usage in the callee.  The mode information
    % is also summarised within the variable use information.
    %
:- type var_mode_and_use
    --->    var_mode_and_use(
                vmu_var                 :: var_rep,
                vmu_mode                :: var_mode_rep,
                vmu_use                 :: var_use_info
            ).

:- pred maybe_call_site_conjunct(implicit_parallelism_info::in, goal_path::in,
    atomic_goal_rep::in, detism_rep::in, inst_map::in, inst_map::in,
    list(var_rep)::in, maybe_call_conjunct::out) is det.

maybe_call_site_conjunct(Info, GoalPath, AtomicGoal, Detism,
        InstMapBefore, InstMapAfter, BoundVars, MaybeCallConjunct) :-
    (
        ( AtomicGoal = unify_construct_rep(_, _, _)
        ; AtomicGoal = unify_deconstruct_rep(_, _, _)
        ; AtomicGoal = partial_construct_rep(_, _, _)
        ; AtomicGoal = partial_deconstruct_rep(_, _, _)
        ; AtomicGoal = unify_assign_rep(_, _)
        ; AtomicGoal = cast_rep(_, _)
        ; AtomicGoal = unify_simple_test_rep(_, _)
        % Don't bother parallelising foreign code, builtins or events.
        ; AtomicGoal = pragma_foreign_code_rep(_)
        ; AtomicGoal = builtin_call_rep(_, _, _)
        ; AtomicGoal = event_call_rep(_, _)
        ),
        MaybeCallConjunct = trivial_atomic_goal(Detism, BoundVars) 
    ;
        ( AtomicGoal = higher_order_call_rep(_, Args)
        ; AtomicGoal = method_call_rep(_, _, Args)
        ; AtomicGoal = plain_call_rep(_, _, Args)
        ),
        (
            ( AtomicGoal = higher_order_call_rep(_, _)
            ; AtomicGoal = method_call_rep(_, _, _)
            ),
            MaybeCallee = no
        ; 
            AtomicGoal = plain_call_rep(ModuleName, CalleeName, _),
            MaybeCallee = yes(ModuleName - CalleeName)
        ),
        map.lookup(Info ^ ipi_call_sites, GoalPath, CallSitePerf),
        % Lookup var use information.
        CallSiteKind = CallSitePerf ^ csf_kind,
        (
            CallSiteKind = normal_call_and_info(NormalCalleeId),
            PSPtr = NormalCalleeId ^ nci_callee_desc ^ pdesc_ps_ptr,
            Deep = Info ^ ipi_deep,
            create_proc_var_use_dump_report(Deep, PSPtr,
                MaybeVarUseDumpInfo),
            MaybeVarUseDumpInfo = ok(VarUseDumpInfo)
        ->
            VarUseInfos = VarUseDumpInfo ^ pvui_var_uses, 
            list.map_corresponding((pred(Arg::in, VarUseInfo::in, 
                        VarModeAndUse::out) is det :-
                    var_get_mode(InstMapBefore, InstMapAfter, Arg, ArgMode),
                    VarModeAndUse = var_mode_and_use(Arg, ArgMode,
                        VarUseInfo)
                ), Args, VarUseInfos, VarModeAndUses)
        ;
            list.map((pred(Arg::in, VarModeAndUse::out) is det :-
                    var_get_mode(InstMapBefore, InstMapAfter, Arg, ArgMode),
                    var_mode_to_var_use(ArgMode, VarUseType),
                    pessimistic_var_use_info(VarUseType, VarUseInfo),
                    VarModeAndUse = var_mode_and_use(Arg, ArgMode,
                        VarUseInfo)
                ), Args, VarModeAndUses)
        ),
        MaybeCallConjunct = 
            call(MaybeCallee, Detism, VarModeAndUses, CallSitePerf)
    ).

:- pred var_get_mode(inst_map::in, inst_map::in, var_rep::in, var_mode_rep::out)
    is det.

var_get_mode(InstMapBefore, InstMapAfter, VarRep, VarModeRep) :-
    inst_map_get(InstMapBefore, VarRep, InstBefore, _),
    inst_map_get(InstMapAfter, VarRep, InstAfter, _),
    VarModeRep = var_mode_rep(InstBefore, InstAfter).

    % Note: this runs in quadratic time.
    %
:- pred build_candidate_conjunctions(implicit_parallelism_info::in,
    inst_map::in, goal_path::in, list(maybe_call_conjunct)::in,
    pqueue(float, candidate_par_conjunction)::in, 
    pqueue(float, candidate_par_conjunction)::out) is det.

build_candidate_conjunctions(_, _, _, [], !Candidates).
build_candidate_conjunctions(Info, InstMap, GoalPath, [MaybeCall | MaybeCalls],
        !Candidates) :-
    (
        MaybeCall = call(_, _, _, CallSitePerf),
        Cost = CallSitePerf ^ csf_summary_perf ^ perf_row_self 
            ^ perf_row_callseqs_percall,
        ( Cost > float(Info ^ ipi_opts ^ cpc_call_site_threshold) ->
            % This conjunction is a call and is expensive enough to
            % parallelise, find some later conjunct to parallelise against it.
            build_candidate_conjunctions_2(Info, InstMap, GoalPath, MaybeCall,
                cord.empty, MaybeCalls, !Candidates)
            % XXX: pick the most expensive non-overlapping candidates from the
            % result.
        ;
            true
        )
    ;
        MaybeCall = non_atomic_goal
    ;
        MaybeCall = trivial_atomic_goal(_, _)
    ),
    build_candidate_conjunctions(Info, InstMap, GoalPath, MaybeCalls,
        !Candidates).

:- pred build_candidate_conjunctions_2(implicit_parallelism_info::in,
    inst_map::in, goal_path::in, maybe_call_conjunct::in(call),
    cord(maybe_call_conjunct)::in, list(maybe_call_conjunct)::in,
    pqueue(float, candidate_par_conjunction)::in, pqueue(float,
    candidate_par_conjunction)::out) is det.

build_candidate_conjunctions_2(_, _, _, _, _, [], !Candidates).
build_candidate_conjunctions_2(Info, InstMap, GoalPath, CallA,
        IntermediateGoals0, [MaybeCall | MaybeCalls], !Candidates) :-
    (
        MaybeCall = call(_, _, _, CallSitePerf),
        CallB = MaybeCall,
        Cost = call_site_perf_self_callseqs_percall(CallSitePerf),
        ( Cost > float(Info ^ ipi_opts ^ cpc_call_site_threshold) ->
            % This conjunction is a call and is expensive enough to
            % parallelise.
            are_conjuncts_dependant(CallA, CallB, InstMap, Dependance),
            (
                Dependance = conjuncts_are_dependant(DepVars),
                compute_optimal_dependant_parallelisation(Info, CallA, CallB,
                    DepVars, IntermediateGoals0, InstMap, CPCA, CPCB, Speedup)
            ;
                Dependance = conjuncts_are_independent,
                compute_independant_parallelisation_speedup(Info, CallA, CallB, 
                    length(IntermediateGoals0), CPCA, CPCB, Speedup)
            ),
            % XXX: This threshold should be configurable.
            ( Speedup > 0.0 ->
                % XXX: I think this should be a priority queue or somesuch.
                GoalPathString = goal_path_to_string(GoalPath),
                Candidate = candidate_par_conjunction(GoalPathString, 
                    CPCA, CPCB, Dependance, Speedup),
                % So that the candidates with the greatest speedup are removed
                % first multiply speedup by -1.0.
                pqueue.insert(!.Candidates, Speedup * -1.0, Candidate,
                    !:Candidates)
            ;
                true
            )
        ;
            % Don't recurse here, we don't parallelise over call goals.
            true
        )
    ;
        MaybeCall = trivial_atomic_goal(_, _),
        build_candidate_conjunctions_2(Info, InstMap, GoalPath, CallA,
            cord.snoc(IntermediateGoals0, MaybeCall), MaybeCalls, !Candidates)
    ;
        MaybeCall = non_atomic_goal
        % Don't recurse in this case, we don't parallelise over non-atomic goals
        % yet.
    ).

:- pred are_conjuncts_dependant(maybe_call_conjunct::in(call),
    maybe_call_conjunct::in(call), inst_map::in, conjuncts_are_dependant::out)
    is det.

are_conjuncts_dependant(CallA, CallB, InstMap, Dependance) :-
    % Conjuncts are dependant if there exists an input variable in CallB that
    % is made ground by CallA or depends upon a variable made ground by CallA.
    list.foldl(add_output_var_to_set, CallA ^ mccc_args, 
        set.init, CallAOutputs),
    list.foldl(are_conjuncts_dependant_var(CallAOutputs, InstMap), 
        CallB ^ mccc_args, set.init, DepVars),
    ( set.empty(DepVars) ->
        Dependance = conjuncts_are_independent
    ;
        Dependance = conjuncts_are_dependant(DepVars)
    ).

:- pred are_conjuncts_dependant_var(set(var_rep)::in, inst_map::in,
    var_mode_and_use::in, set(var_rep)::in, set(var_rep)::out) is det.

are_conjuncts_dependant_var(CallAOutputs, InstMap, VarModeAndUse, !DepVars) :-
    VarModeAndUse = var_mode_and_use(VarRep, VarModeRep, _),
    ( VarModeRep = var_mode_rep(ir_ground_rep, ir_ground_rep) ->
        inst_map_get_var_deps(InstMap, VarRep, VarDeps),
        (
            (
                contains(CallAOutputs, VarRep)
            ;
                member(VarDep, VarDeps),
                contains(CallAOutputs, VarDep)
            )
        ->
            svset.insert(VarRep, !DepVars)
        ;
            true
        )
    ;
        true
    ).

:- pred add_output_var_to_set(var_mode_and_use::in, 
    set(var_rep)::in, set(var_rep)::out) is det.

add_output_var_to_set(var_mode_and_use(VarRep, VarModeRep, _), !Set) :-
    ( VarModeRep = var_mode_rep(ir_free_rep, ir_ground_rep) ->
        svset.insert(VarRep, !Set)
    ;
        true
    ).

:- pred compute_independant_parallelisation_speedup(
    implicit_parallelism_info::in, 
    maybe_call_conjunct::in(call), maybe_call_conjunct::in(call),
    int::in, candidate_par_conjunct::out, candidate_par_conjunct::out,
    float::out) is det.

compute_independant_parallelisation_speedup(Info, CallA, CallB, NumUnifications,
        CPCA, CPCB, Speedup) :-
    CostA = call_site_perf_self_callseqs_percall(CallA ^ mccc_perf),
    CostB = call_site_perf_self_callseqs_percall(CallB ^ mccc_perf),
    SequentialCost = CostA + CostB,
    ParallelCost = max(CostA, CostB) + 
        float(Info ^ ipi_opts ^ cpc_sparking_cost),
    Speedup = SequentialCost - ParallelCost,
    ( CostA < CostB ->
        NumUnificationsA = NumUnifications,
        NumUnificationsB = 0
    ;
        NumUnificationsA = 0,
        NumUnificationsB = NumUnifications
    ),
    call_site_conj_to_candidate_par_conjunct(Info, CallA, NumUnificationsA,
        CPCA),
    call_site_conj_to_candidate_par_conjunct(Info, CallB, NumUnificationsB,
        CPCB).

:- pred compute_optimal_dependant_parallelisation(
    implicit_parallelism_info::in,
    maybe_call_conjunct::in(call), maybe_call_conjunct::in(call),
    set(var_rep)::in, cord(maybe_call_conjunct)::in, inst_map::in,
    candidate_par_conjunct::out, candidate_par_conjunct::out, float::out) 
    is det.

compute_optimal_dependant_parallelisation(Info, CallA, CallB,
        DepVars, IntermediateGoals, InstMap, CPCA, CPCB, Speedup) :-
    CostA = call_site_perf_self_callseqs_percall(CallA ^ mccc_perf),
    CostB = call_site_perf_self_callseqs_percall(CallB ^ mccc_perf),
    SequentialCost = CostA + CostB,
    NumUnifications = length(IntermediateGoals),
    ( singleton_set(DepVars, DepVar) ->
        % Only parallelise conjunctions with a single dependant variable for
        % now.
        ( get_var_use_from_args(CallB ^ mccc_args, DepVar, DepVarConsume) ->
            DepVarConsume = var_use_info(CostUntilConsume, _),
            (
                get_var_use_from_args(CallA ^ mccc_args, DepVar,
                    DepVarProduce)
            ->
                DepVarProduce = var_use_info(CostUntilProduction, _),
                CostBeforeProduction = 
                    cost_until_to_cost_since_start(CostUntilProduction, CostA),
                CostBeforeConsume = 
                    cost_until_to_cost_since_start(CostUntilConsume, CostB),
                CostAfterConsume = 
                    cost_until_to_cost_before_end(CostUntilConsume, CostB),
                % Unfications between the calls don't bind any variables useful
                % for the calls, so serialise them with the cheaper call.
                ( CostA < CostB ->
                    NumUnificationsA = NumUnifications,
                    NumUnificationsB = 0
                ;
                    NumUnificationsA = 0,
                    NumUnificationsB = NumUnifications
                )
            ;
                inst_map_get_var_deps(InstMap, DepVar, DepVarDeps),
                set.fold(get_var_use_add_to_queue(CallA ^ mccc_args), 
                    DepVarDeps, pqueue.init, ProduceDepVarQueue),
                ( 
                    pqueue.remove(ProduceDepVarQueue, _,
                        CostUntilProductionPrime, _)
                ->
                    CostUntilProduction = CostUntilProductionPrime
                ;
                    error(this_file ++ 
                        "Expected to find at least one dependant variable")
                ),
                CostBeforeProduction0 = 
                    cost_until_to_cost_since_start(CostUntilProduction, CostA),
                CostAfterProduction0 = 
                    cost_until_to_cost_before_end(CostUntilProduction, CostA),
                CostBeforeConsume0 = 
                    cost_until_to_cost_since_start(CostUntilConsume, CostB),
                CostAfterConsume0 = 
                    cost_until_to_cost_before_end(CostUntilConsume, CostB),
                % Compare time before consume vs time after production, the
                % lesser one should have the unifications added to it.  This
                % maximises the amount of parallelism.
                ( CostBeforeConsume0 > CostAfterProduction0 ->
                    NumUnificationsA = 0,
                    NumUnificationsB = NumUnifications,
                    CostBeforeProduction = CostBeforeProduction0,
                    CostBeforeConsume = CostA,
                    CostAfterConsume = 0.0
                ;
                    NumUnificationsA = NumUnifications,
                    NumUnificationsB = 0,
                    CostBeforeProduction = 0.0,
                    CostBeforeConsume = CostA - CostAfterConsume0,
                    CostAfterConsume = CostAfterConsume0 
                )
            ),
            SparkingCost = float(Info ^ ipi_opts ^ cpc_sparking_cost),
            LockingCost = float(Info ^ ipi_opts ^ cpc_locking_cost),
            ParallelCost = max(CostA, 
                    max(CostBeforeProduction, CostBeforeConsume) 
                        + CostAfterConsume) 
                + SparkingCost + LockingCost,
            Speedup = SequentialCost - ParallelCost
        ;
            error("Dependant var not in consumer's arguments")
        )
    ;
        Speedup = -1.0,
        NumUnificationsA = NumUnifications,
        NumUnificationsB = 0
    ),
    call_site_conj_to_candidate_par_conjunct(Info, CallA, NumUnificationsA,
        CPCA),
    call_site_conj_to_candidate_par_conjunct(Info, CallB, NumUnificationsB,
        CPCB).

:- pred get_var_use_from_args(list(var_mode_and_use)::in, var_rep::in, 
    var_use_info::out) is semidet.

get_var_use_from_args([], _, _) :- false.
get_var_use_from_args([Arg | Args], Var, VarUse) :-
    ( Arg = var_mode_and_use(Var, _, VarUsePrime) ->
        VarUse = VarUsePrime
    ;
        get_var_use_from_args(Args, Var, VarUse)
    ).

:- pred get_var_use_add_to_queue(list(var_mode_and_use)::in, var_rep::in,
    pqueue(float, cost_until_var_use)::in,
    pqueue(float, cost_until_var_use)::out) is det.

get_var_use_add_to_queue(VarsModeAndUse, VarRep, !Queue) :-
    ( get_var_use_from_args(VarsModeAndUse, VarRep, VarUse) ->
        VarUse = var_use_info(CostUntilVarUse, _),
        % Priority queues return the smallest items first,  And we want to find
        % the most pessimistic variable production so use the cost before the
        % procedure's end.
        Key = cost_until_to_cost_before_end(CostUntilVarUse, 0.0),
        pqueue.insert(!.Queue, Key, CostUntilVarUse, !:Queue)
    ;
        true
    ).

:- func call_site_perf_self_callseqs_percall(call_site_perf) = float.

call_site_perf_self_callseqs_percall(CSP) = 
    CSP ^ csf_summary_perf ^ perf_row_self ^ perf_row_callseqs_percall.

:- pred call_site_conj_to_candidate_par_conjunct(
    implicit_parallelism_info::in, maybe_call_conjunct::in(call),
    int::in, candidate_par_conjunct::out) is det.

call_site_conj_to_candidate_par_conjunct(Info, Call, NumUnifications, CPC) :-
    Call = call(MaybeCallee, _Detism, Args, Perf),
    VarTable = Info ^ ipi_var_table,
    list.map(var_mode_use_to_var_in_par_conj(VarTable), Args, Vars),
    Cost = call_site_perf_self_callseqs_percall(Perf),
    CPC = candidate_par_conjunct(MaybeCallee, Vars, Cost, NumUnifications).

:- pred var_mode_use_to_var_in_par_conj(var_table::in, var_mode_and_use::in,
    maybe(string)::out) is det.

var_mode_use_to_var_in_par_conj(VarTable, var_mode_and_use(Var, _, _),
        MaybeName) :-
    ( search_var_name(VarTable, Var, Name) ->
        MaybeName = yes(Name)
    ;
        MaybeName = no
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

%----------------------------------------------------------------------------%
%
% Useful utility predicates.
%

    % Remove something from inside a maybe_error type and return it.  Throw an
    % exception if the MaybeError variable has value error(_).
    %
:- pred det_open_maybe_error(maybe_error(T)::in, T::out) is det.

det_open_maybe_error(ok(X), X).
det_open_maybe_error(error(Error), _) :-
    error(Error).

:- pred add_call_site_perf_to_map(call_site_perf::in, 
    map(goal_path, call_site_perf)::in, map(goal_path, call_site_perf)::out) 
    is det.

add_call_site_perf_to_map(CallSitePerf, !Map) :-
    GoalPath = CallSitePerf ^ csf_summary_perf ^ perf_row_subject 
        ^ csdesc_goal_path,
    svmap.det_insert(GoalPath, CallSitePerf, !Map).

:- func this_file = string.

this_file = "mdprof_feedback: ".

%-----------------------------------------------------------------------------%
:- end_module mdprof_feedback.
%-----------------------------------------------------------------------------%
