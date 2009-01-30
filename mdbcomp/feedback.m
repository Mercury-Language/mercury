%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2008-2009 The University of Melbourne.
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
:- import_module pair.
:- import_module set.
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

                locking_cost        :: int,
                    % The cost of maintaining a lock on a single dependant
                    % variable in call sequence counts.

                conjunctions        :: assoc_list(string_proc_label,
                                            candidate_par_conjunction)
                    % Assoclist of procedure labels and candidate parallel
                    % conjunctions.
            ).

:- inst feedback_data_query
    --->    feedback_data_calls_above_threshold_sorted(free, free, free)
    ;       feedback_data_candidate_parallel_conjunctions(free, free, free,
                free).

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
:- type candidate_par_conjunction
    --->    candidate_par_conjunction(
                goal_path       :: goal_path_string,
                    % The path within the procedure to this conjunuction.
                
                par_conjunct_a  :: candidate_par_conjunct,
                par_conjunct_b  :: candidate_par_conjunct,
                    % The conjuncts worth parallelising

                dependence      :: conjuncts_are_dependant,

                speedup         :: float
            ).

:- type candidate_par_conjunct
    --->    candidate_par_conjunct(
                callee                  :: maybe(pair(string, string)),
                    % If the name of the callee is known (it's not a HO call),
                    % then store the module and symbol names here.
                    % Note: arity and mode are not represented.
                    
                vars                    :: list(maybe(string)),
                    % The names of variables (if used defined) given as
                    % arguments to this call.
                    
                cost                    :: float
                    % The cost of this call in call sequence counts.
                   
            ).

:- type conjuncts_are_dependant
    --->    conjuncts_are_dependant(
                dependant_vars          :: set(var_rep)
            )
    ;       conjuncts_are_independent.

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
                message     :: string,
                line_no     :: int
            )
    ;       unexpected_eof
    ;       incorrect_version
    ;       incorrect_first_line
    ;       incorrect_program_name.

%-----------------------------------------------------------------------------%

    % read_error_message_string(File, Error, Message)
    %
    % Create a string describing the read error.
    %
:- pred read_error_message_string(string::in, feedback_read_error::in,
    string::out) is det.

%-----------------------------------------------------------------------------%

    % Try to read in a feedback file, if the file doesn't exist create a new
    % empty feedback state in memory.
    %
:- pred read_or_create(string::in, feedback_read_result(feedback_info)::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

    % init_feedback_info = FeedbackInfo
    %
    % Create a new empty feedback info structure.
    %
:- func init_feedback_info = feedback_info.

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
:- import_module map.
:- import_module require.
:- import_module svmap.
:- import_module unit.
:- import_module univ.

%-----------------------------------------------------------------------------%

:- type feedback_info
    --->    feedback_info(
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
    feedback_data_candidate_parallel_conjunctions(_, _, _, _)).

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
                read_no_check_line(Stream),
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

    % Read the feedback data from the file.
    %
:- pred read_data(io.input_stream::in, unit::in,
    feedback_read_result(feedback_info)::out, io::di, io::uo) is det.

read_data(Stream, _, Result, !IO) :-
    io.read(Stream, ReadResultDataAssocList, !IO),
    (
        ReadResultDataAssocList = ok(DataList),
        list.foldl(det_insert_feedback_data, DataList, map.init, Map),
        Result = ok(feedback_info(Map))
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

read_or_create(Path, ReadResultFeedback, !IO) :-
    read_feedback_file(Path, ReadResultFeedback1, !IO),
    (
        ReadResultFeedback1 = ok(_),
        ReadResultFeedback = ReadResultFeedback1
    ;
        ReadResultFeedback1 = error(Error),
        (
            % XXX: Assume that an open error is probably caused by the file not
            % existing, (but we can't be sure because io.error is a string
            % internally, and error messages may change and are not portable).
            Error = open_error(_),
            ReadResultFeedback = ok(init_feedback_info)
        ;
            ( Error = read_error(_)
            ; Error = parse_error(_, _)
            ; Error = unexpected_eof
            ; Error = incorrect_version
            ; Error = incorrect_first_line
            ; Error = incorrect_program_name
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
        Error = incorrect_program_name,
        MessagePart =
            "Program name didn't match, is this the right feedback file?"
    ),
    string.format("%s: %s\n", [s(File), s(MessagePart)], Message).

%-----------------------------------------------------------------------------%

:- pred display_read_error(string::in, feedback_read_error::in,
    io::di, io::uo) is det.

display_read_error(File, Error, !IO) :-
    read_error_message_string(File, Error, Message),
    io.write_string(Message, !IO).

%-----------------------------------------------------------------------------%

init_feedback_info = feedback_info(map.init).

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

feedback_version = "3".

%-----------------------------------------------------------------------------%
:- end_module mdbcomp.feedback.
%-----------------------------------------------------------------------------%
