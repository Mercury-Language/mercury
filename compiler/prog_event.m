%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: prog_event.m.
% Author: zs.
%
% This module defines the database of information the compiler has about
% events other than the built-in set of execution tracing events.
%
%-----------------------------------------------------------------------------%

:- module parse_tree.prog_event.
:- interface.

:- import_module parse_tree.prog_data.

:- import_module list.

:- type event_attribute
    --->    event_attribute(
                attr_name       :: string,
                attr_type       :: mer_type,
                attr_mode       :: mer_mode
            ).

    % Given an event name, returns the types and modes of the arguments
    % of the event.
    %
:- pred event_args(string::in, list(event_attribute)::out) is semidet.

    % Given an event name, returns the names of the arguments of the event.
    %
:- pred event_arg_names(string::in, list(string)::out) is semidet.

    % Given an event name, returns the types of the arguments of the event.
    %
:- pred event_arg_types(string::in, list(mer_type)::out) is semidet.

    % Given an event name, returns the modes of the arguments of the event.
    %
:- pred event_arg_modes(string::in, list(mer_mode)::out) is semidet.

:- implementation.

:- import_module parse_tree.prog_mode.

:- import_module pair.

event_args("test_event",
    [event_attribute("arg1", builtin_type(builtin_type_string), in_mode)]).

event_arg_names(EventName, ArgNames) :-
    event_args(EventName, ArgInfos),
    ArgNames = list.map(project_event_arg_name, ArgInfos).

event_arg_types(EventName, ArgTypes) :-
    event_args(EventName, ArgInfos),
    ArgTypes = list.map(project_event_arg_type, ArgInfos).

event_arg_modes(EventName, ArgModes) :-
    event_args(EventName, ArgInfos),
    ArgModes = list.map(project_event_arg_mode, ArgInfos).

:- func project_event_arg_name(event_attribute) = string.

project_event_arg_name(Attribute) = Attribute ^ attr_name.

:- func project_event_arg_type(event_attribute) = mer_type.

project_event_arg_type(Attribute) = Attribute ^ attr_type.

:- func project_event_arg_mode(event_attribute) = mer_mode.

project_event_arg_mode(Attribute) = Attribute ^ attr_mode.
