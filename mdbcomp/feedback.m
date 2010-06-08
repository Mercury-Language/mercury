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
:- import_module maybe.
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

                desired_parallelism :: float,
                    % The number of desired busy sparks.

                sparking_cost       :: int,
                    % The cost of creating a spark in call sequence counts.

                sparking_delay      :: int,
                    % The time taken between the creation of the spark and when
                    % it starts being executed in call sequence counts.

                locking_cost        :: int,
                    % The cost of maintaining a lock on a single dependant
                    % variable in call sequence counts.

                conjunctions        :: assoc_list(string_proc_label,
                                        candidate_par_conjunction(pard_goal))
                    % Assoclist of procedure labels and candidate parallel
                    % conjunctions.
            ).

:- inst feedback_data_query
    --->    feedback_data_calls_above_threshold_sorted(free, free, free)
    ;       feedback_data_candidate_parallel_conjunctions(free, free, free,
                free, free).

:- type stat_measure
    --->    stat_mean
    ;       stat_median.

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
:- type pard_goal
    --->    pg_call(
                % This is a call that we're considering parallelising.  It has
                % a significant enough cost to be considered for
                % parallelisation.
                
                pgc_callee                  :: callee_rep,
                    
                pgc_vars                    :: list(maybe(string)),
                    % The names of variables (if used defined) given as
                    % arguments to this call.
                    
                pgc_cost_percall            :: float
                    % The per-call cost of this call in call sequence counts.
            )
    ;       pg_cheap_call(
                % This call is to cheap to be considered for parallelisation,
                % we track it in the feedback information to help inform the
                % compiler about _how_ to parallelise calls around it.
                
                pgcc_callee                  :: callee_rep,
                pgcc_vars                    :: list(maybe(string))
                    % As above.
            )
    ;       pg_other_atomic_goal.
                % Some other (cheap) atomic goal.

:- type conjuncts_are_dependant
    --->    conjuncts_are_dependant
    ;       conjuncts_are_independent.

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
    %   CostBSeq, CostBPar) 
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
    float, float) = parallel_exec_metrics_incomplete.

    % StartMetrics = init_empty_parallel_exec_metrics(CostBefore).
    %
    % Use this function to start with an empty set of metrics for an empty
    % conjunction.  Then use init_parallel_exec_metrics_incomplete to continue
    % adding conjuncts on the right.
    %
:- func init_empty_parallel_exec_metrics(float) = 
    parallel_exec_metrics_incomplete.

    % Metrics = finalise_parallel_exec_metrics(IncompleteMetrics, NumCalls,
    %   CostAfterConj, RightConjDelay).:w
    %
    % Make the metrics structure complete.
    %
    % RightConjDelay is the delay before the conjunct to the right of & will
    % begin executing.  & is considered to be right-associative since that's
    % how sparks are sparked.
    %
:- func finalise_parallel_exec_metrics(parallel_exec_metrics_incomplete, 
    int, float, float) = parallel_exec_metrics.

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

:- import_module exception.
:- import_module float.
:- import_module map.
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
    feedback_data_candidate_parallel_conjunctions(_, _, _, _, _)).

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

feedback_version = "8".

%-----------------------------------------------------------------------------%
%
% Helper predicates for the candidate parallel conjunctions type.
%
% XXX: These and their types should probably be moved to a new module.
%

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
                pem_inner_metrics       :: parallel_exec_metrics_incomplete,
                pem_num_calls           :: int,
                pem_time_before_conj    :: float,
                pem_time_after_conj     :: float,
                pem_right_conj_delay    :: float
                    % The delay before a conjunct to the right of & begins
                    % executing.
            ).

:- type parallel_exec_metrics_incomplete
    --->    pem_initial(
                pemi_time_before_conj    :: float
            )
    ;       pem_additional(
                pemi_time_left           :: parallel_exec_metrics_incomplete,
                    % The time of the left conjunct (that may be a conjunction),

                pemi_time_right_seq      :: float,
                    % The time of the right conjunct if it is running after
                    % the left in normal sequential execution.

                pemi_time_right_par      :: float
                    % The time of the right conjunct if it is running in
                    % parallel with the left conjunct.  It may have to stop and
                    % wait for variables to be produced; therefore this time is
                    % different to time_right_seq.  This time also includes
                    % parallel execution overheads and delays.
            ).

init_parallel_exec_metrics_incomplete(MetricsA, TimeBSeq, TimeBPar) = 
    pem_additional(MetricsA, TimeBSeq, TimeBPar).

init_empty_parallel_exec_metrics(TimeBefore) = pem_initial(TimeBefore).

finalise_parallel_exec_metrics(IncompleteMetrics, NumCalls, TimeAfter,
        RightConjDelay) 
    =
        parallel_exec_metrics(IncompleteMetrics, NumCalls, TimeBefore,
        TimeAfter, RightConjDelay) :-
    TimeBefore = 
        parallel_exec_metrics_incomp_get_time_before(IncompleteMetrics).

:- func parallel_exec_metrics_incomp_get_time_before(
    parallel_exec_metrics_incomplete) = float.

parallel_exec_metrics_incomp_get_time_before(pem_initial(Time)) = Time.
parallel_exec_metrics_incomp_get_time_before(
        pem_additional(Left, _, _)) = Time :-
    Time = parallel_exec_metrics_incomp_get_time_before(Left).

parallel_exec_metrics_get_num_calls(PEM) = NumCalls :-
    NumCalls = PEM ^ pem_num_calls.

parallel_exec_metrics_get_par_time(PEM) = Time :-
    Inner = PEM ^ pem_inner_metrics,
    InnerTime = parallel_exec_metrics_incomp_get_par_time(Inner),
    BeforeAndAfterTime = PEM ^ pem_time_before_conj + PEM ^ pem_time_after_conj,
    Time = InnerTime + BeforeAndAfterTime.

parallel_exec_metrics_get_seq_time(PEM) = Time :- 
    Inner = PEM ^ pem_inner_metrics,
    InnerTime = parallel_exec_metrics_incomp_get_seq_time(Inner),
    BeforeAndAfterTime = PEM ^ pem_time_before_conj + PEM ^ pem_time_after_conj,
    Time = InnerTime + BeforeAndAfterTime.

parallel_exec_metrics_get_speedup(Metrics) = SeqTime / ParTime :-
    SeqTime = parallel_exec_metrics_get_seq_time(Metrics),
    ParTime = parallel_exec_metrics_get_par_time(Metrics).

    % The expected parallel execution time.
    %
:- func parallel_exec_metrics_incomp_get_par_time(
    parallel_exec_metrics_incomplete) = float.

parallel_exec_metrics_incomp_get_par_time(pem_initial(_)) = 0.0.
parallel_exec_metrics_incomp_get_par_time(pem_additional(MetricsLeft, _, TimeRight)) 
        = Time :-
    TimeLeft = parallel_exec_metrics_incomp_get_par_time(MetricsLeft),
    Time = max(TimeLeft, TimeRight).

    % The expected sequential execution time.
    %
:- func parallel_exec_metrics_incomp_get_seq_time(
    parallel_exec_metrics_incomplete) = float.

parallel_exec_metrics_incomp_get_seq_time(pem_initial(_)) = 0.0.
parallel_exec_metrics_incomp_get_seq_time(pem_additional(MetricsLeft, TimeRight, _)) 
        = Time :-
    TimeLeft = parallel_exec_metrics_incomp_get_seq_time(MetricsLeft),
    Time = TimeLeft + TimeRight.

parallel_exec_metrics_get_time_saving(Metrics) = SeqTime - ParTime :-
    SeqTime = parallel_exec_metrics_get_seq_time(Metrics),
    ParTime = parallel_exec_metrics_get_par_time(Metrics).

parallel_exec_metrics_get_first_conj_dead_time(Metrics) = DeadTime :-
    Inner = Metrics ^ pem_inner_metrics,
    FirstConjTime = pem_get_first_conj_time(Inner),
    MaxConjTime = pem_get_max_conj_time(Inner, 0.0),
    DeadTime = MaxConjTime - FirstConjTime.

:- func pem_get_first_conj_time(parallel_exec_metrics_incomplete) = float.

pem_get_first_conj_time(pem_initial(_)) = _ :- 
    error("pem_get_first_conj_time: Empty conjunction").
pem_get_first_conj_time(pem_additional(Left, _RightSeq, RightPar)) = Time :-
    (
        Left = pem_initial(_),
        Time = RightPar
    ;
        Left = pem_additional(_, _, _),
        Time = pem_get_first_conj_time(Left)
    ).

:- func pem_get_max_conj_time(parallel_exec_metrics_incomplete, float) = float.

pem_get_max_conj_time(pem_initial(_), Max) = Max.
pem_get_max_conj_time(pem_additional(Left, _, Par), Max0) = Max :-
    Max1 = max(Par, Max0),
    Max = pem_get_max_conj_time(Left, Max1).

parallel_exec_metrics_get_future_dead_time(Metrics) = DeadTime :-
    Inner = Metrics ^ pem_inner_metrics,
    RightConjDelay = Metrics ^ pem_right_conj_delay,
    DeadTime = pem_get_future_dead_time(Inner, RightConjDelay).

:- func pem_get_future_dead_time(parallel_exec_metrics_incomplete, float) 
    = float.

pem_get_future_dead_time(pem_initial(_), _) = 0.0.
pem_get_future_dead_time(pem_additional(Left, Seq, Par), Delay) = DeadTime :-
    DeadTime = ThisDeadTime + LeftDeadTime,
    % Only use the delay if this conjunction contains some code in it's left
    % conjunct.
    (
        Left = pem_initial(_),
        ThisDelay = 0.0
    ;
        Left = pem_additional(_, _, _),
        ThisDelay = Delay
    ),
    ThisDeadTime = Par - Seq - ThisDelay,
    LeftDeadTime = pem_get_future_dead_time(Left, Delay).

parallel_exec_metrics_get_total_dead_time(Metrics) = DeadTime :-
    DeadTime = FirstConjDeadTime + FutureDeadTime,
    FirstConjDeadTime = 
        parallel_exec_metrics_get_first_conj_dead_time(Metrics),
    FutureDeadTime = parallel_exec_metrics_get_future_dead_time(Metrics).

%-----------------------------------------------------------------------------%
:- end_module mdbcomp.feedback.
%-----------------------------------------------------------------------------%
