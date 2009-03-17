%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2009 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: message.m.
% Author: pbone.
%
% This module contains types and predicates for building messages used by the
% mdprof_feedback tool.  These messages can represent information such as
% warnings and errors.  Code is also included here to print them out.
%
%-----------------------------------------------------------------------------%

:- module message.
:- interface.

:- import_module mdbcomp.
:- import_module mdbcomp.program_representation.
:- import_module profile.

:- import_module cord.
:- import_module int.
:- import_module string.

%-----------------------------------------------------------------------------%
    
	% A message to be displayed to the user.
    %
:- type message 
    --->    message(
                message_location    :: program_location,
                message_type        :: message_type 
            ).
    
    % The 'importance' of a message,  Debug messages are not covered here since
    % they should be implemented via trace goals. neither are critical messages
    % since we use exceptions in that case.
    %
:- type message_level
    --->    message_info
    ;       message_notice
    ;       message_warning
    ;       message_error.

:- type program_location
    --->    proc(string_proc_label)
    ;       goal(string_proc_label, goal_path)
    ;       clique(clique_ptr).

%-----------------------------------------------------------------------------%

:- func message_get_level(message) = message_level.

:- func message_level_to_int(message_level) = int.

:- pred message_to_string(message::in, string::out) is det.

:- pred append_message(program_location::in, message_type::in,
    cord(message)::in, cord(message)::out) is det.

%-----------------------------------------------------------------------------%
    
	% A type of message, values of type 'message' are instances of values of
    % type 'message_type'.
    %
:- type message_type

                % A candidate parallel conjunction has been found.
    --->    info_found_candidate_conjunction
                
                % This occurs when a variable is instantiated twice in a
                % procedure body (different instantiation states are used).  We
                % don't bother parallelising such procedures.
                %
    ;       notice_duplicate_instantiation(
                int
                    % The number of conjunctions that could have been
                    % parallelised.
            )

                % Extra call pairs could have been parallelised but weren't in
                % this implementation.
                %
    ;       notice_extra_callpairs_in_conjunction(
                int
                    % THe number of call pairs that were not parallelised.
            )
   
                % A pair of calls that we want to parallelise are separated by
                % some other goal.
                %
    ;       notice_candidate_callpairs_not_adjacent

                % As above, except that the goal in between is a call goal.
                %
    ;       notice_cannot_parallelise_over_cheap_call_goal
        
                % As above, except that the goal in between is non-atomic.
                %
    ;       notice_cannot_parallelise_over_nonatomic_goal
            
                % A pair of calls that could be parallelised have many
                % dependant variables.  We don't yet calculate the speedup in
                % these situations.
    ;       notice_callpair_has_more_than_one_dependant_var
                
                % Couldn't find the proc defn in the progrep data, maybe the
                % procedure is built-in.
                %
    ;       warning_cannot_lookup_proc_defn

                % We don't yet handle clique_proc_reports with multiple proc
                % dynamics.
                %
    ;       error_extra_proc_dynamics_in_clique_proc

                % An error in the generation of a coverage_procrep report.
                %
    ;       error_coverage_procrep_error(string).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module list.

%-----------------------------------------------------------------------------%

message_get_level(message(_, Type)) =
    message_type_to_level(Type).

%-----------------------------------------------------------------------------%

message_to_string(message(Location, MessageType), String) :-
    LocationString = string(Location),
    Level = message_type_to_level(MessageType),
    LevelString = message_level_to_string(Level),
    MessageStr = message_type_to_string(MessageType),
    string.format("%s: In %s: %s",
        [s(LevelString), s(LocationString), s(MessageStr)], String).

%-----------------------------------------------------------------------------%

append_message(Location, MessageType, !Messages) :-
    Message = message(Location, MessageType),
    !:Messages = cord.snoc(!.Messages, Message).

%-----------------------------------------------------------------------------%

:- func message_level_to_string(message_level) = string.

message_level_to_string(message_info) = "Info".
message_level_to_string(message_notice) = "Notice".
message_level_to_string(message_warning) = "Warning".
message_level_to_string(message_error) = "Error".

%-----------------------------------------------------------------------------%

message_level_to_int(message_info) = 4.
message_level_to_int(message_notice) = 3.
message_level_to_int(message_warning) = 2.
message_level_to_int(message_error) = 1.

%-----------------------------------------------------------------------------%

:- func message_type_to_level(message_type) = message_level.

message_type_to_level(info_found_candidate_conjunction) =
    message_info.
message_type_to_level(notice_duplicate_instantiation(_)) = message_notice.
message_type_to_level(notice_extra_callpairs_in_conjunction(_)) =
    message_notice.
message_type_to_level(notice_candidate_callpairs_not_adjacent) = 
    message_notice.
message_type_to_level(notice_cannot_parallelise_over_cheap_call_goal) =
    message_notice.
message_type_to_level(notice_cannot_parallelise_over_nonatomic_goal) =
    message_notice.
message_type_to_level(notice_callpair_has_more_than_one_dependant_var) =
    message_notice.
message_type_to_level(warning_cannot_lookup_proc_defn) = message_warning.
message_type_to_level(error_extra_proc_dynamics_in_clique_proc) = 
    message_error.
message_type_to_level(error_coverage_procrep_error(_)) =
    message_error.

%-----------------------------------------------------------------------------%

:- func message_type_to_string(message_type) = string.

message_type_to_string(MessageType) = String :-
    (
        MessageType = info_found_candidate_conjunction, 
        String = "Found candidate conjunction"
    ;
        MessageType = notice_duplicate_instantiation(CandidateConjuncts), 
        string.format(
            "%d conjunctions not parallelised: Seen duplicate instantiations",
            [i(CandidateConjuncts)], String)
    ;
        MessageType = notice_extra_callpairs_in_conjunction(NumCPCs),
        string.format(
            "%d potential call pairs not parallelised in this conjunction",
            [i(NumCPCs)], String)
    ;
        MessageType = notice_candidate_callpairs_not_adjacent,
        String = "Two callpairs are difficult to parallelise because they are"
            ++ " not adjacent"
    ;
        MessageType = notice_cannot_parallelise_over_cheap_call_goal,
        String = "Parallelising expensive call goals with cheap call goals"
            ++ " between them is not supported"
    ;
        MessageType = notice_cannot_parallelise_over_nonatomic_goal, 
        String = "Parallelising call goals with non-atomic goals between them"
            ++ " is not supported"
    ;
        MessageType = notice_callpair_has_more_than_one_dependant_var,
        String = "Parallelising call pairs that have more than one dependant"
            ++ " variable is not yet supported."
    ;
        MessageType = warning_cannot_lookup_proc_defn,
        String = "Could not look up proc defn, perhaps this procedure is"
            ++ " built-in"
    ;
        MessageType = error_extra_proc_dynamics_in_clique_proc, 
        String = "extra proc dynamnics for a clique proc are not currenty"
            ++ " handled."
    ;
        MessageType = error_coverage_procrep_error(ErrorStr),
        string.format("Error generating coverage procedure report: %s",
            [s(ErrorStr)], String)
    ).

%-----------------------------------------------------------------------------%
:- end_module message.
%-----------------------------------------------------------------------------%
