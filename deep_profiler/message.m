%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2009-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: message.m.
% Author: pbone.
%
% This module contains types and predicates for building messages used by the
% mdprof_create_feedback tool. These messages can represent information such as
% warnings and errors. Code is also included here to print them out.
%
%---------------------------------------------------------------------------%

:- module message.
:- interface.

:- import_module mdbcomp.
:- import_module mdbcomp.goal_path.
:- import_module mdbcomp.program_representation.
:- import_module profile.

:- import_module cord.
:- import_module io.

%---------------------------------------------------------------------------%

    % A message to be displayed to the user.
    %
:- type message
    --->    message(
                message_location    :: program_location,
                message_type        :: message_type
            ).

    % The 'importance' of a message. Debug messages are not covered here since
    % they should be implemented via trace goals. Neither are critical messages
    % since we use exceptions in that case.
    %
:- type message_level
    --->    message_info
    ;       message_notice
    ;       message_warning
    ;       message_error.

:- type program_location
    --->    pl_proc(string_proc_label)
    ;       pl_goal(string_proc_label, reverse_goal_path)
    ;       pl_clique(clique_ptr)
    ;       pl_csd(call_site_dynamic_ptr).

%---------------------------------------------------------------------------%

:- func message_get_level(message) = message_level.

:- func message_level_to_int(message_level) = int.

:- pred message_to_string(message::in, string::out) is det.

    % location_to_string(IndentLevel, Location, String).
    %
    % Pretty-print a location to a cord of strings.
    %
:- pred location_to_string(int::in, program_location::in, cord(string)::out)
    is det.

:- pred append_message(program_location::in, message_type::in,
    cord(message)::in, cord(message)::out) is det.

%---------------------------------------------------------------------------%

:- type message_type
    --->    info_found_candidate_conjunction
            % A candidate parallel conjunction has been found.

    ;       info_found_conjs_above_callsite_threshold(int)
            % There are a number of conjuncts containing calls above the
            % configured call site threshold, we are considering them for
            % parallelisation against one another.

    ;       info_found_pushed_conjs_above_callsite_threshold
            % There are two of conjuncts containing calls above the
            % configured call site threshold that can be pushed together,
            % we are considering them for parallelisation against one another.

    ;       info_split_conjunction_into_partitions(int)
            % The conjunction being considered for parallelisation had to be
            % split into several 'partitions' because it contains some
            % nonatomic goals; this can limit the amount of parallelism
            % available.

    ;       info_found_n_conjunctions_with_positive_speedup(int)
            % There are N conjunctions whose speedup due to parallelisation
            % is positive.

    ;       notice_duplicate_instantiation(
                % This occurs when a variable is instantiated twice in a
                % procedure body (different instantiation states are used).
                % We don't bother parallelising such procedures.
                %
                % The number of conjunctions that could have been
                % parallelised.
                int
            )

    ;       notice_callpair_has_more_than_one_dependant_var
            % A pair of calls that could be parallelised have many
            % dependent variables. We don't yet calculate the speedup in
            % these situations.

    ;       notice_partition_does_not_have_costly_calls(int, int)
            % A partition does not enough costly calls (>1) and
            % could not be parallelised, we could have parallelised them
            % if we could parallelise over non-atomic code.
            %
            % The parameters are the partition number and the number of
            % costly calls found.

    ;       notice_candidate_conjunction_not_det(detism_rep)
            % The candidate conjunction has goals that are not
            % deterministic or cc_multi amongst the costly calls.

    ;       warning_cannot_lookup_proc_defn
            % Couldn't find the proc defn in the progrep data, maybe the
            % procedure is built-in.

    ;       warning_cannot_compute_procrep_coverage_fallback(string)
            % Couldn't compute the coverage annotation for a procedure
            % representation. A fallback method will be used but without
            % this information it may be less accurate.

    ;       warning_cannot_compute_cost_of_recursive_calls(string)
            % Couldn't compute the cost of recursive calls.
            %
            % The parameter contains extra information about this error.

    ;       warning_cannot_compute_first_use_time(string)
            % Couldn't compute the time at which a variable is produced
            % or consumed.
            %
            % The parameter contains extra information about this error.

    ;       error_extra_proc_dynamics_in_clique_proc
            % We don't yet handle clique_proc_reports with multiple proc
            % dynamics.

    ;       error_cannot_lookup_coverage_points
            % An error in the generation of a coverage_procrep report.

    ;       error_exception_thrown(string).

%---------------------------------------------------------------------------%

    % Create an indentation of the appropriate amount.  Indentation is two
    % spaces per indentation level.
    %
:- func indent(int) = cord(string).

    % Create a new line proceeded by an indentation.
    %
:- func nl_indent(int) = cord(string).

    % A newline symbol.
:- func nl = cord(string).

    % The size of an indentation level.  2 x the input.
    %
:- func indent_size(int) = int.

%---------------------------------------------------------------------------%

    % Write out messages.
    %
:- pred write_out_messages(io.text_output_stream::in, cord(message)::in,
    io::di, io::uo) is det.

    % Set the verbosity level to use above.  Higher levels print out more
    % information.  Levels are in the inclusive range 0..4.
    %
:- pred set_verbosity_level(int::in, io::di, io::uo) is det.

    % The default verbosity level if set_verbosity_level is never called.
    %
:- func default_verbosity_level = int.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module program_representation_utils.

:- import_module int.
:- import_module list.
:- import_module require.
:- import_module string.

%---------------------------------------------------------------------------%

message_get_level(message(_, Type)) =
    message_type_to_level(Type).

%---------------------------------------------------------------------------%

message_level_to_int(message_info) = 4.
message_level_to_int(message_notice) = 3.
message_level_to_int(message_warning) = 2.
message_level_to_int(message_error) = 1.

%---------------------------------------------------------------------------%

message_to_string(message(Location, MessageType), String) :-
    location_to_string(1, Location, LocationString),
    Level = message_type_to_level(MessageType),
    LevelString = message_level_to_string(Level),
    MessageStr = message_type_to_string(MessageType),
    Cord = LevelString ++ singleton(":\n") ++ LocationString ++
        indent(1) ++ MessageStr ++ singleton("\n"),
    append_list(cord.list(Cord), String).

location_to_string(Level, Location, String) :-
    (
        Location = pl_proc(ProcLabel),
        print_proc_label_to_string(ProcLabel, ProcLabelString),
        String = indent(Level) ++ singleton("Proc: ") ++
            singleton(ProcLabelString) ++ singleton("\n")
    ;
        Location = pl_goal(ProcLabel, RevGoalPath),
        location_to_string(Level, pl_proc(ProcLabel), FirstLine),
        (
            RevGoalPath = rgp_nil,
            GoalPathString = singleton("Root goal")
        ;
            RevGoalPath = rgp_cons(_, _),
            GoalPathString = singleton("Goal: ") ++
                singleton(rev_goal_path_to_string(RevGoalPath))
        ),
        SecondLine = indent(Level) ++ GoalPathString ++ singleton("\n"),
        String = FirstLine ++ SecondLine
    ;
        Location = pl_clique(clique_ptr(Id)),
        format("clique %d", [i(Id)], String0),
        String = indent(Level) ++ singleton(String0)
    ;
        Location = pl_csd(CSDPtr),
        CSDPtr = call_site_dynamic_ptr(CSDNum),
        format("call site dynamic %d", [i(CSDNum)], String0),
        String = indent(Level) ++ singleton(String0)
    ).

%---------------------------------------------------------------------------%

append_message(Location, MessageType, !Messages) :-
    Message = message(Location, MessageType),
    !:Messages = cord.snoc(!.Messages, Message).

%---------------------------------------------------------------------------%

:- func message_level_to_string(message_level) = cord(string).

message_level_to_string(message_info) = singleton("Info").
message_level_to_string(message_notice) = singleton("Notice").
message_level_to_string(message_warning) = singleton("Warning").
message_level_to_string(message_error) = singleton("Error").

%---------------------------------------------------------------------------%

:- func message_type_to_level(message_type) = message_level.

message_type_to_level(MsgType) = MsgLevel :-
    (
        ( MsgType = info_found_candidate_conjunction
        ; MsgType = info_found_conjs_above_callsite_threshold(_)
        ; MsgType = info_found_pushed_conjs_above_callsite_threshold
        ; MsgType = info_found_n_conjunctions_with_positive_speedup(_)
        ; MsgType = info_split_conjunction_into_partitions(_)
        ),
        MsgLevel = message_info
    ;
        ( MsgType = notice_duplicate_instantiation(_)
        ; MsgType = notice_callpair_has_more_than_one_dependant_var
        ; MsgType = notice_partition_does_not_have_costly_calls(_, _)
        ; MsgType = notice_candidate_conjunction_not_det(_)
        ),
        MsgLevel = message_notice
    ;
        ( MsgType = warning_cannot_lookup_proc_defn
        ; MsgType = warning_cannot_compute_procrep_coverage_fallback(_)
        ; MsgType = warning_cannot_compute_cost_of_recursive_calls(_)
        ; MsgType = warning_cannot_compute_first_use_time(_)
        ),
        MsgLevel = message_warning
    ;
        ( MsgType = error_extra_proc_dynamics_in_clique_proc
        ; MsgType = error_exception_thrown(_)
        ; MsgType = error_cannot_lookup_coverage_points
        ),
        MsgLevel = message_error
    ).

%---------------------------------------------------------------------------%

:- func message_type_to_string(message_type) = cord(string).

message_type_to_string(MessageType) = Cord :-
    (
        MessageType = info_found_candidate_conjunction,
        String = "Found candidate conjunction"
    ;
        MessageType = error_cannot_lookup_coverage_points,
        String = "Cannot lookup coverage points"
    ;
        (
            MessageType = info_found_conjs_above_callsite_threshold(Num),
            MessageStr = "Found %d conjuncts above callsite threshold"
        ;
            MessageType = info_found_n_conjunctions_with_positive_speedup(Num),
            MessageStr = "Found %d conjunctions with a positive speedup due"
                ++ " to parallelisation"
        ;
            MessageType = info_split_conjunction_into_partitions(Num),
            MessageStr = "Split conjunction into %d partitions, "
                ++ "this may reduce parallelism"
        ),
        string.format(MessageStr, [i(Num)], String)
    ;
        MessageType = info_found_pushed_conjs_above_callsite_threshold,
        String = "Found pushed conjuncts above callsite threshold"
    ;
        MessageType = notice_duplicate_instantiation(CandidateConjuncts),
        string.format(
            "%d conjunctions not parallelised: Seen duplicate instantiations",
            [i(CandidateConjuncts)], String)
    ;
        MessageType = notice_callpair_has_more_than_one_dependant_var,
        String = "Parallelising call pairs that have more than one "
            ++ "dependent variable is not yet supported."
    ;
        MessageType = notice_partition_does_not_have_costly_calls(PartNum,
            NumCalls),
        string.format("Partition %d has only %d costly calls and cannot be"
                ++ " parallelised",
            [i(PartNum), i(NumCalls)], String)
    ;
        MessageType = notice_candidate_conjunction_not_det(Detism),
        string.format("There are %s goals amongst goals"
                ++ " above the parallelisation overhead.",
            [s(string(Detism))], String)
    ;
        MessageType = warning_cannot_lookup_proc_defn,
        String = "Could not look up proc defn, perhaps this procedure is"
            ++ " built-in"
    ;
        MessageType = warning_cannot_compute_procrep_coverage_fallback(Error),
        String = "Cannot compute procrep coverage annotation: " ++ Error
            ++ "\n  falling back to some other method"
    ;
        MessageType = error_extra_proc_dynamics_in_clique_proc,
        String = "extra proc dynamics for a clique proc are not currently"
            ++ " handled."
    ;
        (
            MessageType = error_exception_thrown(ErrorStr),
            Template = "Exception thrown: %s"
        ;
            MessageType =
                warning_cannot_compute_cost_of_recursive_calls(ErrorStr),
            Template = "Cannot compute cost of recursive calls: %s"
        ;
            MessageType =
                warning_cannot_compute_first_use_time(ErrorStr),
            Template = "Cannot compute the production or consumption time "
                ++ "of a variable: %s"
        ),
        string.format(Template, [s(ErrorStr)], String)
    ),
    Cord = singleton(String).

%---------------------------------------------------------------------------%

indent(N) = Indent :-
    ( if N < 0 then
        error("automatic_parallelism: Negative indent")
    else if N = 0 then
        Indent = empty
    else
        Indent = snoc(indent(N - 1), "  ")
    ).

nl_indent(N) = nl ++ indent(N).

nl = singleton("\n").

indent_size(N) = 2 * N.

%---------------------------------------------------------------------------%

:- mutable(verbosity_level_mut, int, default_verbosity_level, ground,
    [attach_to_io_state, untrailed]).

write_out_messages(Stream, Messages, !IO) :-
    cord.foldl_pred(write_out_message(Stream), Messages, !IO).

:- pred write_out_message(io.text_output_stream::in, message::in,
    io::di, io::uo) is det.

write_out_message(Stream, Message, !IO) :-
    Level = message_get_level(Message),
    get_verbosity_level_mut(VerbosityLevel, !IO),
    ( if message_level_to_int(Level) =< VerbosityLevel then
        message_to_string(Message, MessageStr),
        io.write_string(Stream, MessageStr, !IO),
        io.nl(Stream, !IO)
    else
        true
    ).

set_verbosity_level(VerbosityLevel, !IO) :-
    set_verbosity_level_mut(VerbosityLevel, !IO).

default_verbosity_level = 2.

%---------------------------------------------------------------------------%
:- end_module message.
%---------------------------------------------------------------------------%
