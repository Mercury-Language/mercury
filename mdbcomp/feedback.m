%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2008-2010 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: feedback.m.
% Main author: pbone.
%
% This module defines data structures for representing feedback information
% as well as procedures for reading and writing feedback files.  It is
% included in the compiler and in any tools that generate feedback data.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module mdbcomp.feedback.

:- interface.

:- import_module mdbcomp.program_representation.

:- import_module assoc_list.
:- import_module int.
:- import_module io.
:- import_module list.
:- import_module string.

%-----------------------------------------------------------------------------%

    % Feedback information is stored in this datatype when in memory.
    %
:- type feedback_info.

%-----------------------------------------------------------------------------%

    % This type stores the data that may be fed back into the compiler.
    % Each constructor here corresponds to a constructor of the feedback_type
    % type.
    %
    % NOTE: When making changes to this structure or structures in
    % mdbcomp.program_representation, be sure to:
    %
    %   - Increment the file format version number towards the bottom of this
    %     file.
    %
    %   - Update the feedback_data_query instantiation state below.
    %
    %   - Update the feedback_type structure within this file.
    %
    %   - Update the feedback_data_type/2 predicate in this file.
    %
:- type feedback_data
    --->    feedback_data_calls_above_threshold_sorted(
                    % Feedback data of this type represents a list of call
                    % sites sorted in descending order of mean or median call
                    % cost where that cost is greater than a given threshold.
 
                threshold       :: int,
                stat_measure    :: stat_measure,
                calls           :: list(call_site)
            )
    ;       feedback_data_candidate_parallel_conjunctions(
                    % Data of this type represents a list of candidate
                    % conjunctions for implicit parallelism.

                parameters      :: candidate_par_conjunctions_params,

                conjunctions    :: assoc_list(string_proc_label, 
                                        candidate_par_conjunctions_proc)
                    % Assoclist of procedure labels and candidate parallel
                    % conjunctions.
            ).

:- inst feedback_data_query
    --->    feedback_data_calls_above_threshold_sorted(free, free, free)
    ;       feedback_data_candidate_parallel_conjunctions(free, free).

:- type stat_measure
    --->    stat_mean
    ;       stat_median.

:- type candidate_par_conjunctions_params
    --->    candidate_par_conjunctions_params(
                cpcp_desired_parallelism    :: float,
                    % The number of desired busy sparks.

                cpcp_sparking_cost          :: int,
                    % The cost of creating a spark and adding it to the local
                    % work queue in call sequence counts.

                cpcp_sparking_delay         :: int,
                    % The time taken between the creation of the spark and when
                    % it starts being executed in call sequence counts.

                cpcp_future_signal_cost     :: int,
                cpcp_future_wait_cost       :: int,
                    % The costs of maintaining a lock on a single dependant
                    % variable in call sequence counts.  The first gives the
                    % cost of the call to signal and the second gives the cost
                    % of the call to wait assuming that the value is already
                    % available.

                cpcp_context_wakeup_delay   :: int,
                    % The time it takes for a context to resume execution once
                    % it has been put on the runnable queue assuming that an
                    % engine is available to pick it up.  This is measured in
                    % call sequence counts.
                    %
                    % This is used to calculate how soon a context can recover
                    % after being blocked by a future.  It is also used to
                    % determine how quickly the context executing
                    % MR_join_and_continue after completing the leftmost
                    % conjunct of a parallel conjunction can recover after
                    % being blocked on the completion of one of the other
                    % conjuncts.

                cpcp_clique_threshold       :: int,
                    % The cost threshold in call sequence counts of a clique
                    % before it is considered for parallel execution.

                cpcp_call_site_threshold    :: int,
                    % The cost threshold in call sequence counts of a call site
                    % before it is considered for parallel execution.

                cpcp_parallelise_dep_conjs  :: parallelise_dep_conjs
                    % Whether we will allow parallelisation to result in
                    % dependant parallel conjunctions.
            ).

:- type parallelise_dep_conjs
    --->    parallelise_dep_conjs
    ;       do_not_parallelise_dep_conjs.

    % The set of candidate parallel conjunctions within a procedure.
    %
:- type candidate_par_conjunctions_proc(GoalType)
    --->    candidate_par_conjunctions_proc(
                cpcp_var_table  :: var_table,
                    % A variable name table for the variables that have
                    % sensible names.
                                        
                cpcp_par_conjs  :: list(candidate_par_conjunction(GoalType))
            ).

:- type candidate_par_conjunctions_proc ==
    candidate_par_conjunctions_proc(pard_goal).

    % A conjunction that is a candidate for parallelisation, it is identified
    % by a procedure label, goal path to the conjunction and the call sites
    % within the conjunction that are to be parallelised.
    %
    % TODO: In the future support more expressive candidate parallel
    % conjunctions, so that more opportunities for parallelism can be found.
    % Although it's probably not a good idea to parallelise three conjuncts or
    % more against one another without first having a good system for reaching
    % and maintaining the target amount of parallelism, this may involve
    % distance granularity.
    %
:- type candidate_par_conjunction(GoalType)
    --->    candidate_par_conjunction(
                cpc_goal_path           :: goal_path_string,
                    % The path within the procedure to this conjunuction.
               
                cpc_partition_number    :: int,
                    % Used to locate the goals to be parallelised within the
                    % conjunction.  Partitions are separated by non atomic
                    % goals, the first partition has the number 1.

                cpc_is_dependant        :: conjuncts_are_dependant,

                cpc_goals_before        :: list(GoalType),

                cpc_conjs               :: list(seq_conj(GoalType)),
                    % A list of parallel conjuncts, each is a sequential
                    % conjunction of inner goals.  All inner goals that are
                    % seen in the program presentation must be stored here
                    % unless they are to be scheduled before or after the
                    % sequential conjunction.  If these conjuncts are flattened
                    % the inner goals will appear in the same order as the
                    % program representation.  By maintaining these two rules
                    % the compiler and analysis tools can use similar
                    % algorithms to construct the same parallel conjunction
                    % from the same program representation/HLDS structure.

                cpc_goals_after         :: list(GoalType),

                cpc_par_exec_metrics    :: parallel_exec_metrics
            ).

:- type seq_conj(GoalType)
    --->    seq_conj(
                sc_conjs            :: list(GoalType)
            ).

:- type callee_rep
    --->    unknown_callee
                % An unknown callee such as a higher order or method call.
                
    ;       named_callee(
                % A known callee.  note that arrity and mode are not stored at
                % all.
               
                nc_module_name  :: string,
                nc_proc_name    :: string
            ).

    % A parallelised goal (pard_goal), a goal within a parallel conjunction.
    % We don't yet have to represent many types of goals or details about them.
    %
:- type pard_goal == goal_rep(pard_goal_annotation).

:- type pard_goal_annotation
    --->    pard_goal_call(
                % A call goal,  These are the most interesting goals WRT
                % parallelisation.

                pgc_cost_percall            :: float,
                    % The per-call cost of this call in call sequence counts.

                pgc_coat_above_threshold    :: cost_above_par_threshold
            )
    ;       pard_goal_other_atomic
                % Some other (cheap) atomic goal.

    ;       pard_goal_non_atomic.
                % A non-atomic goal.

:- type cost_above_par_threshold
    --->    cost_above_par_threshold
                % The goal has a significant enough cost to be considered for
                % parallelisation.

    ;       cost_not_above_par_threshold.
                % The goal is to cheap to be considered for parallelisation,
                % we track it in the feedback information to help inform the
                % compiler about _how_ to parallelise calls around it.

:- type conjuncts_are_dependant
    --->    conjuncts_are_dependant
    ;       conjuncts_are_independent.

:- pred convert_candidate_par_conjunctions_proc(pred(A, B),
    candidate_par_conjunctions_proc(A), candidate_par_conjunctions_proc(B)).
:- mode convert_candidate_par_conjunctions_proc(pred(in, out) is det, 
    in, out) is det.

:- pred convert_candidate_par_conjunction(pred(A, B), 
    candidate_par_conjunction(A), candidate_par_conjunction(B)).
:- mode convert_candidate_par_conjunction(pred(in, out) is det, in, out) is det.

:- pred convert_seq_conj(pred(A, B), seq_conj(A), seq_conj(B)).
:- mode convert_seq_conj(pred(in, out) is det, in, out) is det.

%-----------------------------------------------------------------------------%

    % Represent the metrics of a parallel execution.
    %
:- type parallel_exec_metrics.

    % Represent the metrics of part of a parallel execution.
    %
:- type parallel_exec_metrics_incomplete.
    
    % ParMetrics = init_parallel_exec_metrics_incomplete(PartMetricsA,
    %   TimeSignal, TimeBSeq, TimeBPar) 
    %
    % Use this function to build parallel execution metrics for a parallel
    % conjunction of any size greater than one.
    %
    % Although the parallel conjunction operator is operationally
    % right-associative, parallel overlap due in dependant parallel
    % conjunctions is easier to model if we consider it to be left associative.
    % That is the conjunction ( A & B & C ) should be modeled as two conjuncts
    % (A & B) (which is a conjunction itself and C.  This is because how C
    % waits for variables in either A or B may depend on How B waits for
    % variables in A.
    %
:- func init_parallel_exec_metrics_incomplete(parallel_exec_metrics_incomplete,
    float, float, float) = parallel_exec_metrics_incomplete.

    % StartMetrics = init_empty_parallel_exec_metrics(CostBefore, NumCalls,
    %   SparkCost, SparkDelay, ContextWakeupDelay).
    %
    % Use this function to start with an empty set of metrics for an empty
    % conjunction.  Then use init_parallel_exec_metrics_incomplete to continue
    % adding conjuncts on the right.
    %
:- func init_empty_parallel_exec_metrics(float, int, float, float, float) = 
    parallel_exec_metrics_incomplete.

    % Metrics = finalise_parallel_exec_metrics(IncompleteMetrics,
    %   CostAfterConj).
    %
    % Make the metrics structure complete.
    %
    % RightConjDelay is the delay before the conjunct to the right of & will
    % begin executing.  & is considered to be right-associative since that's
    % how sparks are sparked.
    %
:- func finalise_parallel_exec_metrics(parallel_exec_metrics_incomplete, float)
    = parallel_exec_metrics.

:- func parallel_exec_metrics_get_num_calls(parallel_exec_metrics) = int.

:- func parallel_exec_metrics_get_seq_time(parallel_exec_metrics) = float.

:- func parallel_exec_metrics_get_par_time(parallel_exec_metrics) = float.

    % The amount of time saved per-call. SeqTime - ParTime.
    %
:- func parallel_exec_metrics_get_time_saving(parallel_exec_metrics) = float.

    % The speedup per call.  SeqTime / ParTime.  For example, a value of 2.0
    % means that this is twice as fast when parallelised.
    %
:- func parallel_exec_metrics_get_speedup(parallel_exec_metrics) = float.

    % The amount of time the initial (left most) conjunct spends waiting for
    % the other conjuncts.  During this time the context used by this conjunct
    % must be kept alive because it will resume executing sequential code after
    % the conjunct, however we know that it cannot be resumed before it's
    % children have completed.
    %
:- func parallel_exec_metrics_get_first_conj_dead_time(parallel_exec_metrics) =
    float.

    % The some of the amount of time spent waiting for variables to be produced
    % throughout the whole conjunction.
    %
:- func parallel_exec_metrics_get_future_dead_time(parallel_exec_metrics) =
    float.

    % The sum of the above two times.
    %
:- func parallel_exec_metrics_get_total_dead_time(parallel_exec_metrics) =
    float.

%-----------------------------------------------------------------------------%

    % put_feedback_data(Data, !Info)
    %
    % Put feedback data into the feedback files.
    %
    % Data loaded from file (not added with put) will be removed from the
    % internal state when data for the same type is added.
    %
:- pred put_feedback_data(feedback_data::in,
    feedback_info::in, feedback_info::out) is det.

%-----------------------------------------------------------------------------%

    % get_feedback_data(Info, Data).
    %
    % When given a partially instantiated Data term representing the query this
    % will either fully instantiate the term or fail.
    %
:- pred get_feedback_data(feedback_info::in, 
    feedback_data::feedback_data_query) is semidet.

:- mode feedback_data_query ==
    feedback_data_query >> ground.

:- pred get_all_feedback_data(feedback_info::in, list(feedback_data)::out)
    is det.

    % Get the name of the program that generated this feedback information.
    %
:- func get_feedback_program_name(feedback_info) = string.

%-----------------------------------------------------------------------------%

    % read_feedback_file(Path, FeedbackInfo, !IO)
    %
    % This predicate reads in feedback data from a specified file.
    % It should be called once per compiler invocation.
    %
:- pred read_feedback_file(string::in,
    feedback_read_result(feedback_info)::out, io::di, io::uo) is det.

:- type feedback_read_result(T)
    --->    ok(T)
    ;       error(feedback_read_error).

:- type feedback_read_error
    --->    open_error(io.error)
    ;       read_error(io.error)
    ;       parse_error(
                fre_pe_message          :: string,
                fre_pe_line_no          :: int
            )
    ;       unexpected_eof
    ;       incorrect_version
    ;       incorrect_first_line
    ;       incorrect_program_name(
                fre_ipn_expected        :: string,
                fre_ipn_got             :: string
            ).

%-----------------------------------------------------------------------------%

    % read_error_message_string(File, Error, Message)
    %
    % Create a string describing the read error.
    %
:- pred read_error_message_string(string::in, feedback_read_error::in,
    string::out) is det.

%-----------------------------------------------------------------------------%

    % read_or_create(Path, ProgramName, Result, !IO).
    %
    % Try to read in a feedback file, if the file doesn't exist create a new
    % empty feedback state in memory.
    %
    % ProgramName is the name of the program that generated this feedback file.
    % It is used to set this information for new feedback files or to verify
    % that the name in an existing file matches what was expected.
    %
:- pred read_or_create(string::in, string::in,
    feedback_read_result(feedback_info)::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

    % init_feedback_info(ProgramName) = FeedbackInfo
    %
    % Create a new empty feedback info structure.
    %
:- func init_feedback_info(string) = feedback_info.

%-----------------------------------------------------------------------------%

    % write_feedback_file(Path, ProgName, FeedbackInfo, FeedbackWriteResult,
    %   !IO)
    %
    % Write out the feedback data to a given file name.
    %
:- pred write_feedback_file(string::in, string::in, feedback_info::in,
    feedback_write_result::out, io::di, io::uo) is det.

:- type feedback_write_result
    --->    ok
    ;       open_error(io.error)
    ;       write_error(io.error).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module exception.
:- import_module float.
:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module svmap.
:- import_module unit.
:- import_module univ.

%-----------------------------------------------------------------------------%

:- type feedback_info
    --->    feedback_info(
                fi_program_name                 :: string,
                fi_map                          :: map(feedback_type, 
                                                    feedback_data)
                    % The actual feedback data as read from the feedback file.
            ).

    % This type is used as a key for the data that may be fed back into the
    % compiler.
    %
:- type feedback_type
    --->    feedback_type_calls_above_threshold_sorted
    ;       feedback_type_candidate_parallel_conjunctions.

%-----------------------------------------------------------------------------%

get_feedback_data(Info, Data) :-
    feedback_data_type(Type, Data),
    Map = Info ^ fi_map,
    map.search(Map, Type, DataPrime),
    % This disjunction will either unify Data to DataPrime, or throw an
    % exception, the impure annotation is required so to avoid a compiler
    % warning saying that the second disjunct will not succeed, which must be
    % promised away.
    promise_pure (
        Data = DataPrime
    ;
        impure impure_true,
        feedback_data_mismatch_error("get_feedback_data/3: ", Type,
            DataPrime)
    ).

get_all_feedback_data(Info, AllData) :-
    map.values(Info ^ fi_map, AllData).

get_feedback_program_name(Info) = Info ^ fi_program_name.

%-----------------------------------------------------------------------------%

put_feedback_data(Data, !Info) :-
    feedback_data_type(Type, Data),
    some [!Map] (
        !:Map = !.Info ^ fi_map,
        svmap.set(Type, Data, !Map),
        !:Info = !.Info ^ fi_map := !.Map
    ).

%----------------------------------------------------------------------------%

:- pred feedback_data_type(feedback_type, feedback_data).

:- mode feedback_data_type(out, in(feedback_data_query)) is det.
:- mode feedback_data_type(out, in) is det.

feedback_data_type(feedback_type_calls_above_threshold_sorted,
    feedback_data_calls_above_threshold_sorted(_, _, _)).
feedback_data_type(feedback_type_candidate_parallel_conjunctions,
    feedback_data_candidate_parallel_conjunctions(_, _)).

:- pred feedback_data_mismatch_error(string::in, feedback_type::in, 
    feedback_data::in) is erroneous.

feedback_data_mismatch_error(Predicate, Type, Data) :-
    error(string.format(
        "%s: Feedback data doesn't match type\n\tType: %s\n\tData: %s\n",
        [s(Predicate), s(string(Type)), s(string(Data))])).

%-----------------------------------------------------------------------------%

read_feedback_file(Path, ReadResultFeedbackInfo, !IO) :-
    io.open_input(Path, IOResStream, !IO),
    (
        %
        % Set the data file as the current stream and call read2.
        %
        IOResStream = ok(Stream),
        some [!Result] (
            % Read each part of the file and continue reading of this is
            % succesful.  read_cont takes care of this logic.

            read_check_line(feedback_first_line, incorrect_first_line, Stream,
                unit, !:Result, !IO),
            maybe_read(
                read_check_line(feedback_version, incorrect_version, Stream),
                !Result, !IO),
            maybe_read(
                read_program_name(Stream),
                !Result, !IO),
            maybe_read(read_data(Stream), !Result, !IO),
            ReadResultFeedbackInfo = !.Result
        ),
        io.close_input(Stream, !IO)
    ;
        IOResStream = error(ErrorCode),
        ReadResultFeedbackInfo = error(open_error(ErrorCode))
    ).

    % If the result so far is successful, call the closure and return its
    % result.  Otherwise return the accumulated result without calling the
    % closure.
    %
:- pred maybe_read(pred(A, feedback_read_result(B), io, io),
    feedback_read_result(A), feedback_read_result(B), io, io).
:- mode maybe_read(pred(in, out, di, uo) is det,
    in, out, di, uo) is det.

maybe_read(Pred, Result0, Result, !IO) :-
    (
        Result0 = ok(Acc),
        Pred(Acc, Result, !IO)
    ;
        Result0 = error(Error),
        Result = error(Error)
    ).

    % Read and check a line of the file.
    %
:- pred read_check_line(string::in, feedback_read_error::in,
    io.input_stream::in, unit::in, feedback_read_result(unit)::out,
    io::di, io::uo) is det.

read_check_line(TestLine, NotMatchError, Stream, _, Result, !IO) :-
    io.read_line_as_string(Stream, IOResultLine, !IO),
    (
        IOResultLine = ok(Line),
        (
            ( Line = TestLine
            ; Line = TestLine ++ "\n"
            )
        ->
            Result = ok(unit)
        ;
            Result = error(NotMatchError)
        )
    ;
        IOResultLine = eof,
        Result = error(unexpected_eof)
    ;
        IOResultLine = error(Error),
        Result = error(read_error(Error))
    ).

    % Read and don't check a line of the file.
    %
:- pred read_no_check_line(io.input_stream::in, unit::in,
    feedback_read_result(unit)::out, io::di, io::uo) is det.

read_no_check_line(Stream, _, Result, !IO) :-
    io.read_line_as_string(Stream, IOResultLine, !IO),
    (
        IOResultLine = ok(_),
        Result = ok(unit)
    ;
        IOResultLine = eof,
        Result = error(unexpected_eof)
    ;
        IOResultLine = error(Error),
        Result = error(read_error(Error))
    ).

:- pred read_program_name(io.input_stream::in, unit::in,
    feedback_read_result(string)::out, io::di, io::uo) is det.

read_program_name(Stream, _, Result, !IO) :-
    io.read_line_as_string(Stream, IOResultLine, !IO),
    (
        IOResultLine = ok(String),
        Result = ok(strip(String))
    ;
        IOResultLine = eof,
        Result = error(unexpected_eof)
    ;
        IOResultLine = error(Error),
        Result = error(read_error(Error))
    ).

    % Read the feedback data from the file.
    %
:- pred read_data(io.input_stream::in, string::in,
    feedback_read_result(feedback_info)::out, io::di, io::uo) is det.

read_data(Stream, ProgramName, Result, !IO) :-
    io.read(Stream, ReadResultDataAssocList, !IO),
    (
        ReadResultDataAssocList = ok(DataList),
        list.foldl(det_insert_feedback_data, DataList, map.init, Map),
        Result = ok(feedback_info(ProgramName, Map))
    ;
        ReadResultDataAssocList = eof,
        Result = error(unexpected_eof)
    ;
        ReadResultDataAssocList = error(Error, Line),
        Result = error(parse_error(Error, Line))
    ).

:- pred det_insert_feedback_data(feedback_data::in, map(feedback_type,
    feedback_data)::in, map(feedback_type, feedback_data)::out) is det.

det_insert_feedback_data(Data, !Map) :-
    feedback_data_type(Key, Data),
    svmap.det_insert(Key, Data, !Map).

%-----------------------------------------------------------------------------%

read_or_create(Path, ExpectedProgName, ReadResultFeedback, !IO) :-
    read_feedback_file(Path, ReadResultFeedback1, !IO),
    (
        ReadResultFeedback1 = ok(Feedback),
        GotProgName = get_feedback_program_name(Feedback),
        ( ExpectedProgName = GotProgName ->
            ReadResultFeedback = ReadResultFeedback1
        ;
            ReadResultFeedback = error(
                incorrect_program_name(ExpectedProgName, GotProgName))
        )
    ;
        ReadResultFeedback1 = error(Error),
        (
            % XXX: Assume that an open error is probably caused by the file not
            % existing, (but we can't be sure because io.error is a string
            % internally, and error messages may change and are not portable).
            Error = open_error(_),
            ReadResultFeedback = ok(init_feedback_info(ExpectedProgName))
        ;
            ( Error = read_error(_)
            ; Error = parse_error(_, _)
            ; Error = unexpected_eof
            ; Error = incorrect_version
            ; Error = incorrect_first_line
            ; Error = incorrect_program_name(_, _)
            ),
            ReadResultFeedback = ReadResultFeedback1
        )
    ).

%-----------------------------------------------------------------------------%

read_error_message_string(File, Error, Message) :-
    (
        ( Error = open_error(Code)
        ; Error = read_error(Code)
        ),
        error_message(Code, MessagePart)
    ;
        Error = parse_error(ParseMessage, Line),
        MessagePart = ParseMessage ++ " on line " ++ string(Line)
    ;
        Error = unexpected_eof,
        MessagePart = "Unexpected end of file"
    ;
        Error = incorrect_version,
        MessagePart = "Incorrect file format version"
    ;
        Error = incorrect_first_line,
        MessagePart = "Incorrect file format"
    ;
        Error = incorrect_program_name(Expected, Got),
        MessagePart =
            "Program name didn't match, is this the right feedback file?\n"
            ++ format("Expected: '%s' Got: '%s'", [s(Expected), s(Got)])
    ),
    string.format("%s: %s\n", [s(File), s(MessagePart)], Message).

%-----------------------------------------------------------------------------%

:- pred display_read_error(string::in, feedback_read_error::in,
    io::di, io::uo) is det.

display_read_error(File, Error, !IO) :-
    read_error_message_string(File, Error, Message),
    io.write_string(Message, !IO).

%-----------------------------------------------------------------------------%

init_feedback_info(ProgramName) = feedback_info(ProgramName, map.init).

%-----------------------------------------------------------------------------%

write_feedback_file(Path, ProgName, Feedback, Res, !IO) :-
    io.open_output(Path, OpenRes, !IO),
    (
        OpenRes = ok(Stream),
        promise_equivalent_solutions [!:IO, ExcpRes] (
            try_io(write_feedback_file_2(Stream, ProgName, Feedback),
                ExcpRes, !IO)
        ),
        (
            ExcpRes = succeeded(_),
            Res = ok
        ;
            ExcpRes = exception(ExcpUniv),

            % If the exception is not of a type we expected, then re-throw it.
            ( univ_to_type(ExcpUniv, Excp) ->
                Res = write_error(Excp)
            ;
                rethrow(ExcpRes)
            )
        )
    ;
        OpenRes = error(ErrorCode),
        Res = open_error(ErrorCode)
    ).

    % Write out the data. This is called by try_io to catch any exceptions
    % that close_output and the other predicates we call here (e.g. io.write)
    % may throw.
    %
:- pred write_feedback_file_2(output_stream::in, string::in, feedback_info::in,
    unit::out, io::di, io::uo) is det.

write_feedback_file_2(Stream, ProgName, Feedback, unit, !IO) :-
    io.write_string(Stream, feedback_first_line, !IO),
    io.nl(Stream, !IO),
    io.write_string(Stream, feedback_version, !IO),
    io.nl(Stream, !IO),
    io.write_string(Stream, ProgName, !IO),
    io.nl(Stream, !IO),
    Map = Feedback ^ fi_map,
    map.values(Map, FeedbackList),
    io.write(Stream, FeedbackList, !IO),
    io.write_string(Stream, ".\n", !IO),
    io.close_output(Stream, !IO).

%-----------------------------------------------------------------------------%

:- func feedback_first_line = string.

feedback_first_line = "Mercury Compiler Feedback".

:- func feedback_version = string.

feedback_version = "10".

%-----------------------------------------------------------------------------%
%
% Helper predicates for the candidate parallel conjunctions type.
%
% XXX: These and their types should probably be moved to a new module.
%

convert_candidate_par_conjunctions_proc(Conv, CPCProcA, CPCProcB) :-
    CPCProcA = candidate_par_conjunctions_proc(VarTable, CPCA),
    map(convert_candidate_par_conjunction(Conv), CPCA, CPCB),
    CPCProcB = candidate_par_conjunctions_proc(VarTable, CPCB).

convert_candidate_par_conjunction(Conv, CPC0, CPC) :-
    CPC0 = candidate_par_conjunction(GoalPath, PartNum, IsDependent, 
        GoalsBefore0, Conjs0, GoalsAfter0, Metrics),
    map(convert_seq_conj(Conv), Conjs0, Conjs),
    map(Conv, GoalsBefore0, GoalsBefore),
    map(Conv, GoalsAfter0, GoalsAfter),
    CPC = candidate_par_conjunction(GoalPath, PartNum, IsDependent, 
        GoalsBefore, Conjs, GoalsAfter, Metrics).

convert_seq_conj(Conv, seq_conj(Conjs0), seq_conj(Conjs)) :-
    map(Conv, Conjs0, Conjs).

%-----------------------------------------------------------------------------%

:- type parallel_exec_metrics
    --->    parallel_exec_metrics(
                pem_inner_metrics           :: parallel_exec_metrics_internal,
                pem_num_calls               :: int,
                pem_time_before_conj        :: float,
                pem_time_after_conj         :: float,
                pem_left_conj_cost          :: float,
                    % The cost of calling fork() in the conjunct to the left of
                    % a & symbol.
                pem_right_conj_delay        :: float,
                    % The delay before a conjunct to the right of & begins
                    % executing.

                pem_context_wakeup_delay    :: float
            ).

:- type parallel_exec_metrics_incomplete
    --->    pem_incomplete(
                pemi_time_before_conj       :: float,

                pemi_num_calls              :: int,

                pemi_spark_cost             :: float,

                pemi_spark_delay            :: float,

                pemi_context_wakeup_delay   :: float,

                pemi_internal               ::
                        maybe(parallel_exec_metrics_internal)
                    % If there are no internal conjuncts then the parallel
                    % conjunction is empty.
            ).

:- type parallel_exec_metrics_internal
    --->    pem_left_most(
                pemi_time_seq               :: float,
                pemi_time_par               :: float
            )
    ;       pem_additional(
                pemi_time_left              :: parallel_exec_metrics_internal,
                    % The time of the left conjunct (that may be a conjunction),

                pemi_time_left_signals      :: float,
                    % The additional cost of calling signal within the left
                    % conjunct.
                    % NOTE: Note that this should be added to each of the
                    % individual conjuncts _where_ they call signal but thta is
                    % more difficult and may not be required.  We may visit it
                    % in the future.

                pemi_time_right_seq         :: float,
                    % The time of the right conjunct if it is running after
                    % the left in normal sequential execution.

                pemi_time_right_par         :: float
                    % The time of the right conjunct if it is running in
                    % parallel with the left conjunct.  It may have to stop and
                    % wait for variables to be produced; therefore this time is
                    % different to time_right_seq.  This time also includes
                    % parallel execution overheads and delays.
            ).

init_parallel_exec_metrics_incomplete(Metrics0, TimeSignals, TimeBSeq, 
        TimeBPar) = Metrics :-
    MaybeInternal0 = Metrics0 ^ pemi_internal,
    (
        MaybeInternal0 = yes(Internal0),
        Internal = pem_additional(Internal0, TimeSignals, TimeBSeq, TimeBPar)
    ;
        MaybeInternal0 = no,
        Internal = pem_left_most(TimeBSeq, TimeBPar),
        require(unify(TimeSignals, 0.0),
            this_file ++ "TimeSignal != 0")
    ),
    Metrics = Metrics0 ^ pemi_internal := yes(Internal).

init_empty_parallel_exec_metrics(TimeBefore, NumCalls, SparkCost, 
        SparkDelay, ContextWakeupDelay) = 
    pem_incomplete(TimeBefore, NumCalls, SparkCost, SparkDelay,
        ContextWakeupDelay, no).

finalise_parallel_exec_metrics(IncompleteMetrics, TimeAfter) = Metrics :-
    IncompleteMetrics = pem_incomplete(TimeBefore, NumCalls, SparkCost,
        SparkDelay, ContextWakeupDelay, MaybeInternal),
    (
        MaybeInternal = yes(Internal)
    ;
        MaybeInternal = no,
        error(this_file ++ "Cannot finalise empty parallel metrics.")
    ),
    Metrics = parallel_exec_metrics(Internal, NumCalls, TimeBefore, TimeAfter,
        SparkCost, SparkDelay, ContextWakeupDelay).

parallel_exec_metrics_get_num_calls(PEM) = NumCalls :-
    NumCalls = PEM ^ pem_num_calls.

parallel_exec_metrics_get_par_time(PEM) = Time :-
    Inner = PEM ^ pem_inner_metrics,
    InnerTime = parallel_exec_metrics_internal_get_par_time(Inner),
    FirstConjTime = pem_get_first_conj_time(Inner),
    BeforeAndAfterTime = PEM ^ pem_time_before_conj + PEM ^ pem_time_after_conj,
    ( FirstConjTime < InnerTime ->
        FirstConjWakeupPenalty = PEM ^ pem_context_wakeup_delay
    ;
        FirstConjWakeupPenalty = 0.0
    ),
    Time = InnerTime + BeforeAndAfterTime + FirstConjWakeupPenalty.

parallel_exec_metrics_get_seq_time(PEM) = Time :- 
    Inner = PEM ^ pem_inner_metrics,
    InnerTime = parallel_exec_metrics_internal_get_seq_time(Inner),
    BeforeAndAfterTime = PEM ^ pem_time_before_conj + PEM ^ pem_time_after_conj,
    Time = InnerTime + BeforeAndAfterTime.

parallel_exec_metrics_get_speedup(Metrics) = SeqTime / ParTime :-
    SeqTime = parallel_exec_metrics_get_seq_time(Metrics),
    ParTime = parallel_exec_metrics_get_par_time(Metrics).

    % The expected parallel execution time.
    %
:- func parallel_exec_metrics_internal_get_par_time(
    parallel_exec_metrics_internal) = float.

parallel_exec_metrics_internal_get_par_time(pem_left_most(_, Time)) = Time.
parallel_exec_metrics_internal_get_par_time(pem_additional(MetricsLeft,
        TimeLeftSignal, _, TimeRight)) = Time :-
    TimeLeft = parallel_exec_metrics_internal_get_par_time(MetricsLeft) +
        TimeLeftSignal,
    Time = max(TimeLeft, TimeRight).

    % The expected sequential execution time.
    %
:- func parallel_exec_metrics_internal_get_seq_time(
    parallel_exec_metrics_internal) = float.

parallel_exec_metrics_internal_get_seq_time(pem_left_most(Time, _)) = Time.
parallel_exec_metrics_internal_get_seq_time(pem_additional(MetricsLeft, _,
        TimeRight, _)) = Time :-
    TimeLeft = parallel_exec_metrics_internal_get_seq_time(MetricsLeft),
    Time = TimeLeft + TimeRight.

parallel_exec_metrics_get_time_saving(Metrics) = SeqTime - ParTime :-
    SeqTime = parallel_exec_metrics_get_seq_time(Metrics),
    ParTime = parallel_exec_metrics_get_par_time(Metrics).

parallel_exec_metrics_get_first_conj_dead_time(Metrics) = DeadTime :-
    Inner = Metrics ^ pem_inner_metrics,
    FirstConjTime = pem_get_first_conj_time(Inner),
    ParTime = parallel_exec_metrics_get_par_time(Metrics),
    DeadTime = ParTime - FirstConjTime.

    % Get the parallel execution time of the first conjunct.  This is used for
    % calculating the first conjunct's dead time (above).
    %
:- func pem_get_first_conj_time(parallel_exec_metrics_internal) = float.

pem_get_first_conj_time(pem_left_most(_, Time)) = Time.
pem_get_first_conj_time(pem_additional(Left, LeftSignalTime0, _, _)) = Time :-
    (
        Left = pem_left_most(_, _),
        LeftSignalTime = LeftSignalTime0
    ;
        Left = pem_additional(_, _, _, _),
        LeftSignalTime = 0.0
    ),
    Time = pem_get_first_conj_time(Left) + LeftSignalTime.

parallel_exec_metrics_get_future_dead_time(Metrics) = DeadTime :-
    Inner = Metrics ^ pem_inner_metrics,
    RightConjDelay = Metrics ^ pem_right_conj_delay,
    LeftConjCost = Metrics ^ pem_left_conj_cost,
    DeadTime = pem_get_future_dead_time(Inner, yes, LeftConjCost,
        RightConjDelay).

:- func pem_get_future_dead_time(parallel_exec_metrics_internal, bool,
    float, float) = float.

    % XXX: Delays may new be build into the times.
pem_get_future_dead_time(pem_left_most(_, _), _, _, _) = 0.0.
pem_get_future_dead_time(pem_additional(Left, _, Seq, Par), 
        IsRightmostConj, ForkCost, ForkDelay) = DeadTime :-
    DeadTime = ThisDeadTime + LeftDeadTime,
    ThisDeadTime0 = Par - Seq - ForkDelay,
    (
        IsRightmostConj = yes,
        ThisDeadTime = ThisDeadTime0
    ;
        IsRightmostConj = no,
        ThisDeadTime = ThisDeadTime0 + ForkCost
    ),
    LeftDeadTime = pem_get_future_dead_time(Left, no, ForkCost, ForkDelay).

parallel_exec_metrics_get_total_dead_time(Metrics) = DeadTime :-
    DeadTime = FirstConjDeadTime + FutureDeadTime,
    FirstConjDeadTime = 
        parallel_exec_metrics_get_first_conj_dead_time(Metrics),
    FutureDeadTime = parallel_exec_metrics_get_future_dead_time(Metrics).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "feedback.m: ".

%-----------------------------------------------------------------------------%
:- end_module mdbcomp.feedback.
%-----------------------------------------------------------------------------%
