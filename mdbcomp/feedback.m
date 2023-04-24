%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2008-2011 The University of Melbourne.
% Copyright (C) 2014-2015, 2017-2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: feedback.m.
% Main author: pbone.
%
% This module defines data structures for representing feedback information
% in Mercury code, as well as procedures for reading and writing the feedback
% files that represent such information on disk.
%
% This module is included both in the compiler and in the tools that
% generate this feedback data.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module mdbcomp.feedback.

:- interface.

% If you add any modules here, you should update the lists in
% deep_profiler/Mmakefile and slice/Mmakefile.
:- include_module automatic_parallelism.

:- import_module mdbcomp.feedback.automatic_parallelism.
:- import_module mdbcomp.program_representation.

:- import_module assoc_list.
:- import_module io.
:- import_module maybe.

%---------------------------------------------------------------------------%

    % The feedback_info type stores the data that may be fed back
    % into the compiler. For a detailed description, see the comment
    % on the non-abstract definition in the implementation section.
    %
:- type feedback_info.

    % init_feedback_info(ProfiledProgramName) = FeedbackInfo:
    %
    % Create a new empty feedback info structure, recording that it is
    % intended to hold feedback information for the program with the given
    % name.
    %
    % XXX The predicates that add information to feedback_infos now require
    % their callers to specify what profiled program their information is for,
    % and this is checked against the profiled program name in the
    % feedback_info. We could instead initialize feedback_infos *without*
    % storing the program name, and record that name on the first update
    % of the feedback_info instead. This would remove one source of possible
    % name mismatches.
    %
:- func init_feedback_info(string) = feedback_info.

%---------------------------------------------------------------------------%
%
% The kinds of information that we can record in a feedback file.
%
% Historically, we also supported a feedback type that was used by
% Jerome Tannier's attempt at discovering a useful set of conjunctions
% to parallelise. However, this early attempt at automatic parallelisation
% yielded results that were much inferior to our current system, so it
% should interest only historians. The code supporting Tanner's feedback
% type was removed from the feedback system on 2014 december 1;
% if you want it, look in the git archives for commits on that date.
%

    % Values of this type represent a list of candidate conjunctions
    % for implicit parallelism.
    %
:- type feedback_info_candidate_parallel_conjunctions
    --->    feedback_info_candidate_parallel_conjunctions(
                cpc_parameters      :: candidate_par_conjunctions_params,

                % For each procedure that has some candidate parallel
                % conjunctions, list those candidates.
                cpc_conjunctions    :: assoc_list(string_proc_label,
                                        candidate_par_conjunctions_proc)
            ).

%---------------------------------------------------------------------------%
%
% The getter predicates of feedback_info.
%

    % Get the name of the program whose profiled execution the given
    % feedback_info was derived from.
    %
:- func get_feedback_profiled_program_name(feedback_info) = string.

    % get_feedback_*(Info) = Data:
    %
    % Get any feedback data of the given kind from the given feedback_info.
    %
:- func get_feedback_candidate_parallel_conjunctions(feedback_info) =
    maybe(feedback_info_candidate_parallel_conjunctions).

    % Get all the information held in the given feedback info. Callers should
    % call this predicate, instead of the ones above, if they want to guarantee
    % that even if the feedback_info type is updated, they will still get
    % all the information present in the given feedback_info.
    %
:- pred get_all_feedback_info(feedback_info::in,
    string::out, maybe(feedback_info_candidate_parallel_conjunctions)::out)
    is det.

%---------------------------------------------------------------------------%
%
% The setter predicates of feedback_info.
%

    % add_feedback_*(ProfiledProgramName, Data, !Info)
    %
    % Put Data into the selected field of the feedback_info, which must hold
    % information about ProfiledProgramName. Requires the old feedback_info
    % to have no previous information in that field.
    %
:- pred add_feedback_candidate_parallel_conjunctions(string::in,
    feedback_info_candidate_parallel_conjunctions::in,
    feedback_info::in, feedback_info::out) is det.

    % replace_feedback_*(ProfiledProgramName, Data, !Info)
    %
    % Put Data into the selected field of the feedback_info, which must hold
    % information about ProfiledProgramName. Requires the old feedback_info
    % Any previous information in that field of feedback_info
    % is replaced by Data.
    %
:- pred replace_feedback_candidate_parallel_conjunctions(string::in,
    feedback_info_candidate_parallel_conjunctions::in,
    feedback_info::in, feedback_info::out) is det.

%---------------------------------------------------------------------------%
%
% Reading in feedback files.
%

:- type feedback_read_result(T) == maybe_error(T, feedback_read_error).

:- type feedback_read_error
    --->    fre_open_error(io.error)
    ;       fre_read_error(io.error)
    ;       fre_parse_error(
                fre_pe_message          :: string,
                fre_pe_line_no          :: int
            )
    ;       fre_unexpected_eof
    ;       fre_incorrect_version(string)
    ;       fre_incorrect_first_line
    ;       fre_incorrect_profiled_program_name(
                fre_ippn_expected       :: string,
                fre_ippn_got            :: string
            )
    ;       fre_repeated_component(
                fre_component_name      :: string
            ).

    % feedback_read_error_message_string(File, Error, Message):
    %
    % Create a string describing the read error.
    %
:- pred feedback_read_error_message_string(string::in, feedback_read_error::in,
    string::out) is det.

    % read_or_create_feedback_file(Path, ProfiledProgramName, Result, !IO):
    %
    % If Path stores a feedback file for ProfiledProgramName, read it in.
    % If it does not exist, return an empty feedback state for
    % ProfiledProgramName, and return that. Return an error if Path does exist,
    % but either does not contain a valid feedback file, or contains a valid
    % feedback file for some other profiled program.
    %
    % ProfiledProgramName is the name of the program whose profiled execution
    % the feedback file was (or should be) generated from.
    % We record this to avoid mixing the feedback information of unrelated
    % executables.
    %
:- pred read_or_create_feedback_file(string::in, string::in,
    feedback_read_result(feedback_info)::out, io::di, io::uo) is det.

    % read_feedback_file(Path, MaybeProfiledProgramName, FeedbackInfo, !IO):
    %
    % This predicate attempts to read in feedback data from Path. If
    % MaybeProfiledProgramName is yes(ProfiledProgramName), generate an
    % error if the feedback data is not for ProfiledProgramName.
    %
    % This predicate should be called once per compiler invocation.
    %
:- pred read_feedback_file(string::in, maybe(string)::in,
    feedback_read_result(feedback_info)::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%
% Writing out feedback files.
%

:- type feedback_write_result
    --->    fwr_ok
    ;       fwr_open_error(io.error)
    ;       fwr_write_error(io.error).

    % write_feedback_file(Path, FeedbackInfo, FeedbackWriteResult, !IO):
    %
    % Write out the feedback data in FeedbackWriteResult to Path.
    %
:- pred write_feedback_file(string::in, feedback_info::in,
    feedback_write_result::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module exception.
:- import_module list.
:- import_module require.
:- import_module string.
:- import_module unit.
:- import_module univ.

% There are several kinds of information that we may be interested in
% feeding back to the compiler. Our design for representing feedback
% information allows for a tool to generate an arbitrary subset of
% the possible kinds of information. A feedback-generating tool will
% put each kind of information that it generates in its assigned slot
% with a yes(...) wrapped around it, while it will put a "no" in all
% other slots.
%
% If you want add a new kind of feedback information, you will need to
%
%   - add a new maybe field to the feedback_info type;
%   - add a getter predicate and two setter predicates for that field;
%   - update the init_feedback_info predicate;
%   - update the get_all_feedback_info predicate;
%   - update the print_feedback_report predicate;
%   - add a new alternative to the feedback_component_wrapper type;
%   - add code to add_feedback_components to read in the new kind of
%     information;
%   - add code to actually_write_feedback_file to write out the new kind of
%     information;
%   - increment the file format version number at the bottom of this file.
%
% You will also need to increment the file format version number
% if you change the definition of any of the types referred to, directly
% or indirectly, by the feedback_component_wrapper type, including the types
% in mdbcomp.program_representation.

:- type feedback_info
    --->    feedback_info(
                % The name of the program whose execution generated
                % the profiling data file that the feedback is derived from,
                % and therefore the program whose compilation the feedback
                % is intended for.
                fi_profiled_program_name     :: string,

                % The actual feedback data as read from the feedback file.
                % Should be set to yes(...) iff feedback of the given sort
                % is present in the file.
                fi_maybe_candidate_parallel_conjunctions ::
                        maybe(feedback_info_candidate_parallel_conjunctions)
            ).

:- type feedback_component_wrapper
    --->    fcw_candidate_parallel_conjunctions(
                feedback_info_candidate_parallel_conjunctions
            ).

%---------------------------------------------------------------------------%
%
% Initialization and getter and setter predicates for feedback_infos.
%

init_feedback_info(ProgramName) = feedback_info(ProgramName, no).

get_feedback_profiled_program_name(Info) = Info ^ fi_profiled_program_name.
get_feedback_candidate_parallel_conjunctions(Info) =
    Info ^ fi_maybe_candidate_parallel_conjunctions.

get_all_feedback_info(Info, ProfiledProgramName,
        MaybeCandidateParallelConjs) :-
    Info = feedback_info(ProfiledProgramName, MaybeCandidateParallelConjs).

add_feedback_candidate_parallel_conjunctions(ProfiledProgramName, Data,
        !Info) :-
    expect(unify(!.Info ^ fi_profiled_program_name, ProfiledProgramName),
        $pred, "adding candidate parallel conjunctions for wrong program"),
    expect(unify(!.Info ^ fi_maybe_candidate_parallel_conjunctions, no),
        $pred, "overwriting old candidate_parallel_conjunctions data"),
    !Info ^ fi_maybe_candidate_parallel_conjunctions := yes(Data).

replace_feedback_candidate_parallel_conjunctions(ProfiledProgramName, Data,
        !Info) :-
    expect(unify(!.Info ^ fi_profiled_program_name, ProfiledProgramName),
        $pred, "replacing candidate parallel conjunctions for wrong program"),
    !Info ^ fi_maybe_candidate_parallel_conjunctions := yes(Data).

%---------------------------------------------------------------------------%
%
% Interpreting the errors that can happen when reading in feedback files.
%

feedback_read_error_message_string(File, Error, Message) :-
    (
        ( Error = fre_open_error(Code)
        ; Error = fre_read_error(Code)
        ),
        error_message(Code, MessagePart)
    ;
        Error = fre_parse_error(ParseMessage, Line),
        MessagePart = ParseMessage ++ " on line " ++ string(Line)
    ;
        Error = fre_unexpected_eof,
        MessagePart = "Unexpected end of file"
    ;
        Error = fre_incorrect_version(Expected),
        MessagePart = "Incorrect file format version; expected " ++ Expected
    ;
        Error = fre_incorrect_first_line,
        MessagePart = "Incorrect file format"
    ;
        Error = fre_incorrect_profiled_program_name(Expected, Got),
        MessagePart =
            "The name of the program the feedback is for didn't match,"
            ++ " is this the right feedback file?\n"
            ++ string.format("Expected: '%s' Got: '%s'", [s(Expected), s(Got)])
    ;
        Error = fre_repeated_component(ComponentName),
        MessagePart = "File contains more than one "
            ++ ComponentName ++ " component"
    ),
    string.format("%s: %s\n", [s(File), s(MessagePart)], Message).

%---------------------------------------------------------------------------%
%
% Reading feedback files.
%

read_or_create_feedback_file(Path, ExpectedProfiledProgramName,
        FeedbackResult, !IO) :-
    read_feedback_file(Path, yes(ExpectedProfiledProgramName),
        ReadResult, !IO),
    (
        ReadResult = ok(_Feedback),
        FeedbackResult = ReadResult
    ;
        ReadResult = error(Error),
        (
            % XXX We assume that an open error is probably caused by the file
            % not existing, but we can't be sure because io.error is a string,
            % and the message string for any error may change.
            Error = fre_open_error(_),
            FeedbackResult = ok(
                init_feedback_info(ExpectedProfiledProgramName))
        ;
            ( Error = fre_read_error(_)
            ; Error = fre_parse_error(_, _)
            ; Error = fre_unexpected_eof
            ; Error = fre_incorrect_version(_)
            ; Error = fre_incorrect_first_line
            ; Error = fre_incorrect_profiled_program_name(_, _)
            ; Error = fre_repeated_component(_)
            ),
            FeedbackResult = ReadResult
        )
    ).

read_feedback_file(Path, MaybeExpectedProfiledProgramName,
        ResultFeedbackInfo, !IO) :-
    io.open_input(Path, PathResult, !IO),
    (
        PathResult = ok(PathStream),
        some [!Result] (
            % Read each part of the file. The calls that actually do the
            % reading are wrapped inside calls to maybe_read, which guarantees
            % that we stop reading as soon as we found some error.
            %
            % The result so far starts as a unit (containing no information),
            % turns into a string representing the name of the profiled
            % program after the call to read_profiled_program_name, and
            % then into the feedback_info after read_all_feedback_data.

            read_check_line(feedback_first_line, fre_incorrect_first_line,
                PathStream, unit, !:Result, !IO),
            maybe_read(
                read_check_line(feedback_version,
                    fre_incorrect_version(feedback_version), PathStream),
                !Result, !IO),
            maybe_read(
                read_profiled_program_name(MaybeExpectedProfiledProgramName,
                    PathStream),
                !Result, !IO),
            maybe_read(read_all_feedback_data(PathStream), !Result, !IO),
            ResultFeedbackInfo = !.Result
        ),
        io.close_input(PathStream, !IO)
    ;
        PathResult = error(ErrorCode),
        ResultFeedbackInfo = error(fre_open_error(ErrorCode))
    ).

    % If the result so far is successful, call the closure on the result so far
    % (which may be a unit) and return the closure's output as the new result.
    % Otherwise, return the previous error result without calling the closure.
    %
:- pred maybe_read(
    pred(A, feedback_read_result(B), io, io)::in(pred(in, out, di, uo) is det),
    feedback_read_result(A)::in, feedback_read_result(B)::out,
    io::di, io::uo) is det.

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
    io.text_input_stream::in, unit::in, feedback_read_result(unit)::out,
    io::di, io::uo) is det.

read_check_line(TestLine, NotMatchError, Stream, _, Result, !IO) :-
    io.read_line_as_string(Stream, LineResult, !IO),
    (
        LineResult = ok(Line),
        ( if
            ( Line = TestLine
            ; Line = TestLine ++ "\n"
            )
        then
            Result = ok(unit)
        else
            Result = error(NotMatchError)
        )
    ;
        LineResult = eof,
        Result = error(fre_unexpected_eof)
    ;
        LineResult = error(Error),
        Result = error(fre_read_error(Error))
    ).

:- pred read_profiled_program_name(maybe(string)::in, io.text_input_stream::in,
    unit::in, feedback_read_result(string)::out, io::di, io::uo) is det.

read_profiled_program_name(MaybeExpectedProfiledProgramName, Stream,
        _, Result, !IO) :-
    io.read_line_as_string(Stream, LineResult, !IO),
    (
        LineResult = ok(String),
        ActualProfiledProgramName = string.strip(String),
        (
            MaybeExpectedProfiledProgramName = no,
            Result = ok(ActualProfiledProgramName)
        ;
            MaybeExpectedProfiledProgramName =
                yes(ExpectedProfiledProgramName),
            ( if ActualProfiledProgramName = ExpectedProfiledProgramName then
                Result = ok(ActualProfiledProgramName)
            else
                Result = error(fre_incorrect_profiled_program_name(
                    ExpectedProfiledProgramName, ActualProfiledProgramName))

            )
        )
    ;
        LineResult = eof,
        Result = error(fre_unexpected_eof)
    ;
        LineResult = error(Error),
        Result = error(fre_read_error(Error))
    ).

    % Read the feedback data from the file.
    %
    % The feedback data in the file should be a single large term.
    % This term should be a list, each element of which is an identifying
    % wrapper around a feedback component.
    %
    % The overall term is handled by read_all_feedback_data, while
    % the list elements are handled by add_feedback_components.
    %
:- pred read_all_feedback_data(io.text_input_stream::in, string::in,
    feedback_read_result(feedback_info)::out, io::di, io::uo) is det.

read_all_feedback_data(Stream, ProfiledProgramName, Result, !IO) :-
    io.read(Stream, ReadResult, !IO),
    (
        ReadResult = ok(Components),
        Info0 = init_feedback_info(ProfiledProgramName),
        add_feedback_components(Components, Info0, Result)
    ;
        ReadResult = eof,
        Result = error(fre_unexpected_eof)
    ;
        ReadResult = error(Error, Line),
        Result = error(fre_parse_error(Error, Line))
    ).

:- pred add_feedback_components(list(feedback_component_wrapper)::in,
    feedback_info::in, feedback_read_result(feedback_info)::out) is det.

add_feedback_components([], !.Info, Result) :-
    Result = ok(!.Info).
add_feedback_components([Wrapper | Wrappers], !.Info, Result) :-
    (
        Wrapper = fcw_candidate_parallel_conjunctions(Candidates),
        MaybeCandidates0 = !.Info ^ fi_maybe_candidate_parallel_conjunctions,
        (
            MaybeCandidates0 = no,
            !Info ^ fi_maybe_candidate_parallel_conjunctions
                := yes(Candidates),
            add_feedback_components(Wrappers, !.Info, Result)
        ;
            MaybeCandidates0 = yes(_),
            Result = error(fre_repeated_component(
                "candidate_parallel_conjunctions"))
        )
    ).

%---------------------------------------------------------------------------%
%
% Writing feedback files.
%

write_feedback_file(Path, Feedback, Result, !IO) :-
    io.open_output(Path, PathResult, !IO),
    (
        PathResult = ok(PathStream),
        promise_equivalent_solutions [!:IO, ExcpResult] (
            try_io(actually_write_feedback_file(PathStream, Feedback),
                ExcpResult, !IO)
        ),
        % XXX PathStream ought to be closed here.
        % io.close_output(PathStream, !IO),
        (
            ExcpResult = succeeded(_),
            Result = fwr_ok
        ;
            ExcpResult = exception(ExcpUniv),

            % If the exception is not of a type we expected, then re-throw it.
            ( if univ_to_type(ExcpUniv, Excp) then
                Result = fwr_write_error(Excp)
            else
                rethrow(ExcpResult)
            )
        )
    ;
        PathResult = error(ErrorCode),
        Result = fwr_open_error(ErrorCode)
    ).

    % Write out the data. This is called by try_io to catch any exceptions
    % that close_output and the other predicates we call here (e.g. io.write)
    % may throw.
    % XXX This should NOT be necessary.
    %
:- pred actually_write_feedback_file(io.text_output_stream::in,
    feedback_info::in, unit::out, io::di, io::uo) is det.

actually_write_feedback_file(Stream, FeedbackInfo, unit, !IO) :-
    FeedbackInfo = feedback_info(ProfiledProgramName,
        MaybeCandidateParallelConjs),
    io.format(Stream, "%s\n%s\n%s\n",
        [s(feedback_first_line), s(feedback_version), s(ProfiledProgramName)],
        !IO),
    % In the future, we expect to support more than one kind of feedback.
    some [!RevComponents] (
        !:RevComponents = [],
        (
            MaybeCandidateParallelConjs = no
        ;
            MaybeCandidateParallelConjs = yes(Candidates),
            CandComponent = fcw_candidate_parallel_conjunctions(Candidates),
            !:RevComponents = [CandComponent | !.RevComponents]
        ),
        list.reverse(!.RevComponents, Components)
    ),
    io.write(Stream, Components, !IO),
    io.write_string(Stream, ".\n", !IO),
    io.close_output(Stream, !IO).

%---------------------------------------------------------------------------%
%
% The identifying marks of feedback files.
%

:- func feedback_first_line = string.

feedback_first_line = "Mercury Compiler Feedback".

:- func feedback_version = string.

feedback_version = "19".

%---------------------------------------------------------------------------%
:- end_module mdbcomp.feedback.
%---------------------------------------------------------------------------%
