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

:- import_module assoc_list.
:- import_module list.

    % Given an event name, returns the types and modes of the arguments
    % of the event.
    %
:- pred event_arg_types_modes(string::in,
    assoc_list(mer_type, mer_mode)::out) is semidet.

    % Given an event name, returns the types of the arguments of the event.
    %
:- pred event_arg_types(string::in, list(mer_type)::out) is semidet.

    % Given an event name, returns the modes of the arguments of the event.
    %
:- pred event_arg_modes(string::in, list(mer_mode)::out) is semidet.

:- implementation.

:- import_module parse_tree.prog_mode.

:- import_module pair.

event_arg_types_modes("test_event",
    [builtin_type(builtin_type_string) - in_mode]).

event_arg_types(EventName, ArgTypes) :-
    event_arg_types_modes(EventName, ArgTypesModes),
    assoc_list.keys(ArgTypesModes, ArgTypes).

event_arg_modes(EventName, ArgModes) :-
    event_arg_types_modes(EventName, ArgTypesModes),
    assoc_list.values(ArgTypesModes, ArgModes).
