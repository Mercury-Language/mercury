%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2008 The University of Melbourne.
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

    % This type is used as a key for the data that may be fed back into the
    % compiler.
    %
    % NOTE: When making changes to this structure, be sure to increment
    % the file format version number towards the bottom of this file.
    %
:- type feedback_type
    --->    feedback_type_calls_above_threshold_sorted.
            % Feedback data of this type represents a list of call sites
            % sorted in descending order of mean or median call cost where
            % that cost is greater than a given threshold.

%-----------------------------------------------------------------------------%

    % This type stores the data that may be fed back into the compiler.
    % Each constructor here corresponds to a constructor of the feedback_type
    % type.
    %
    % TODO: We need a mechanism to ensure that the answer for a given query
    % (a value of type feedback_type) is always the corresponding value
    % in this type. The right solution would be to represent a query as
    % a partially instantiated data structure that we later fill in,
    % but Mercury doesn't yet let us do that.
    %
    % NOTE: When making changes to this structure or structures in
    % mdbcomp.program_representation, be sure to increment the file format
    % version number towards the bottom of this file.
    %
:- type feedback_data
    --->    feedback_data_calls_above_threshold_sorted(
                threshold       :: int,
                stat_measure    :: stat_measure,
                calls           :: list(call_site)
            ).

:- type stat_measure
    --->    stat_mean
    ;       stat_median.

%-----------------------------------------------------------------------------%

    % put_feedback_data(InfoType, Info, !State)
    %
    % 'Put' feedback data into the feedback files.  Data is stored based on
    % the type of information being stored.
    %
    % Data loaded from file (not added with put) will be removed from the
    % internal state when data for the same info type is added.
    %
:- pred put_feedback_data(feedback_type::in, feedback_data::in,
    feedback_info::in, feedback_info::out) is det.

%-----------------------------------------------------------------------------%

    % get_feedback_data(InfoType, MaybeInfo, State).
    %
    % To query the feedback files 'get' will give a value for a given info type
    % if it exists.
    %
:- pred get_feedback_data(feedback_type::in, maybe(feedback_data)::out,
    feedback_info::in) is det.

%-----------------------------------------------------------------------------%

    % read_feedback_file(Path, FeedbackState, !IO)
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
:- pred read_or_create(string::in, feedback_info::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

    % init_feedback_info = FeedbackState
    %
    % Create a new empty feedback state.
    %
:- func init_feedback_info = feedback_info.

%-----------------------------------------------------------------------------%

    % write_feedback_file(Path, ProgName, FeedbackState, FeedbackWriteResult,
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
:- import_module svmap.
:- import_module unit.
:- import_module univ.

%-----------------------------------------------------------------------------%

:- type feedback_info
    ---> feedback_info(map(feedback_type, feedback_data)).

%-----------------------------------------------------------------------------%

get_feedback_data(Type, MaybeData, Info) :-
    Info = feedback_info(Map),
    ( map.search(Map, Type, Data) ->
        MaybeData = yes(Data)
    ;
        MaybeData = no
    ).

%-----------------------------------------------------------------------------%

put_feedback_data(Type, Data, !Info) :-
    some [!Map] (
        !.Info = feedback_info(!:Map),
        svmap.set(Type, Data, !Map),
        !:Info = feedback_info(!.Map)
    ).

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
        ReadResultDataAssocList = ok(DataAssocList),
        map.det_insert_from_assoc_list(map.init, DataAssocList, Map),
        Result = ok(feedback_info(Map))
    ;
        ReadResultDataAssocList = eof,
        Result = error(unexpected_eof)
    ;
        ReadResultDataAssocList = error(Error, Line),
        Result = error(parse_error(Error, Line))
    ).

%-----------------------------------------------------------------------------%

read_or_create(Path, Feedback, !IO) :-
    read_feedback_file(Path, ReadResultFeedback, !IO),
    (
        ReadResultFeedback = ok(Feedback)
    ;
        ReadResultFeedback = error(Error),
        display_read_error(Path, Error, !IO),
        Feedback = init_feedback_info
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
    Feedback = feedback_info(Map),
    map.to_assoc_list(Map, FeedbackList),
    io.write(Stream, FeedbackList, !IO),
    io.write_string(Stream, ".\n", !IO),
    io.close_output(Stream, !IO).

%-----------------------------------------------------------------------------%

:- func feedback_first_line = string.

feedback_first_line = "Mercury Compiler Feedback".

:- func feedback_version = string.

feedback_version = "1".

%-----------------------------------------------------------------------------%
:- end_module mdbcomp.feedback.
%-----------------------------------------------------------------------------%
