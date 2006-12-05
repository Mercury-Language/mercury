%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: stream_util.string_builder.m.
% Main author: maclarty.
% 
% This module implements a string builder stream.  It can be used to
% build up a string using string or character writers.
%
:- module stream_util.string_builder.

:- interface.

:- import_module char.
:- import_module stream.
:- import_module string.

:- type string_builder_stream.

:- type string_builder_state.

:- pred init(string_builder_stream::out, string_builder_state::uo) is det.

:- instance stream.stream(string_builder_stream, string_builder_state).
:- instance stream.output(string_builder_stream, string_builder_state).

:- instance stream.writer(string_builder_stream, string, string_builder_state).
:- instance stream.writer(string_builder_stream, char, string_builder_state).

:- func string_builder_state_to_string(string_builder_state::di) = (string::uo)
    is det.

%----------------------------------------------------------------------------%

:- implementation.

:- import_module list.

:- type string_builder_state
    --->    string_builder_state(list(string)).

:- type string_builder_stream
    --->    string_builder_stream.

init(string_builder_stream, string_builder_state([])).

:- instance stream.stream(string_builder_stream, string_builder_state)
        where [
    name(_, "<<string builder stream>>", !State)
].

:- instance stream.output(string_builder_stream, string_builder_state)
        where [
    flush(_, !State)
].

:- instance stream.writer(string_builder_stream, string, string_builder_state)
        where [
    ( put(_, String, !State) :-
        !.State = string_builder_state(StringList0),
        %
        % The string builder will never clobber the string.  Also we
        % know that nothing else can clobber the string since it isn't unique.
        % Therefore the inst cast below is okay, even though it is a lie.
        %
        StringList = [unsafe_promise_unique(String) | StringList0],
        !:State = string_builder_state(StringList)
    )
].

:- instance stream.writer(string_builder_stream, char, string_builder_state)
        where [
    ( put(_, Char, !State) :-
        !.State = string_builder_state(StringList0),
        StringList = [string.from_char(Char) | StringList0],
        !:State = string_builder_state(StringList)
    )
].

string_builder_state_to_string(State) = String :-
    State = string_builder_state(StringList),
    String = string.join_list("", list.reverse(StringList)).
