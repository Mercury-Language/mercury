%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2006-2007 The University of Melbourne.
% Copyright (C) 2014-2015, 2018, 2022-2024 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: string.builder.m.
% Main author: maclarty.
%
% This module implements a string builder stream. It can be used
% to build up a string using string or character writers.
%
% To build up a string using this module, you first construct an initial
% string builder state by calling the init function. You can then use
% any instances of stream.writer that write strings or characters to update the
% string builder state, using string.builder.handle as the stream argument.
% Once you have finished writing to the string builder, you can get the final
% string by calling string.builder.to_string/1.
%
% For example:
%
%     State0 = string.builder.init,
%     stream.string_writer.put_int(string.builder.handle, 5, State0, State),
%     Str = string.builder.to_string(State),  % Str = "5".
%
%---------------------------------------------------------------------------%

:- module string.builder.
:- interface.

:- import_module char.
:- import_module stream.

%---------------------------------------------------------------------------%

:- type handle
    --->    handle.

:- type state.

:- func init = (string.builder.state::uo) is det.

    % Add a character to the end of the string builder.
    %
:- pred append_char(char::in,
    string.builder.state::di, string.builder.state::uo) is det.

    % Add a string to the end of the string builder.
    %
:- pred append_string(string::in,
    string.builder.state::di, string.builder.state::uo) is det.

    % Add a list of strings to the end of the string builder.
    %
:- pred append_strings(list(string)::in,
    string.builder.state::di, string.builder.state::uo) is det.

%---------------------%

:- pred format(string::in, list(poly_type)::in,
    string.builder.state::di, string.builder.state::uo) is det.

%---------------------%

    % Return the total number of code points in the string that
    % to_string would return, without constructing that string (yet).
    %
    % Note that once you call this function, you cannot add any new entries
    % to the given string builder state, because it loses its uniqueness.
    %
:- func total_num_code_points(string.builder.state) = int.

    % Succeed if and only if the total number of code points in the string
    % that to_string would return is at most the given number. Determine this
    % without constructing that string (yet).
    %
    % Note that once you call this predicate, you cannot add any new entries
    % to the given string builder state, because it loses its uniqueness.
    %
:- pred total_num_code_points_is_at_most(string.builder.state::in, int::in)
    is semidet.

%---------------------%

    % Return the string that the previous calls to append_* constructed.
    %
:- func to_string(string.builder.state::in) = (string::uo) is det.

%---------------------------------------------------------------------------%

:- instance stream.stream(string.builder.handle, string.builder.state).

:- instance stream.output(string.builder.handle, string.builder.state).

:- instance stream.writer(string.builder.handle, string, string.builder.state).
:- instance stream.writer(string.builder.handle, char, string.builder.state).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.

%---------------------------------------------------------------------------%

:- type state
    --->    state(list(string)).
            % A reversed list of the strings that, when unreversed
            % and appended together, yields the string we have built.

%---------------------------------------------------------------------------%

init = state([]).

%---------------------%

:- pragma inline(pred(append_char/3)).      % inline in instance method
append_char(Char, !State) :-
    !.State = state(RevStrings0),
    RevStrings = [string.char_to_string(Char) | RevStrings0],
    !:State = state(RevStrings).

:- pragma inline(pred(append_string/3)).    % inline in instance method
append_string(Str, !State) :-
    !.State = state(RevStrs0),
    copy(Str, UniqueStr),
    RevStrs = [UniqueStr | RevStrs0],
    !:State = state(RevStrs).

append_strings([], !State).
append_strings([Str | Strs], !State) :-
    append_string(Str, !State),
    append_strings(Strs, !State).

%---------------------%

format(FormatStr, PolyTypes, !State) :-
    disable_warning [unknown_format_calls] (
        string.format(FormatStr, PolyTypes, Str)
    ),
    append_string(Str, !State).

%---------------------%

total_num_code_points(State) = TotalLen :-
    State = state(RevStrs),
    total_num_code_points_acc(RevStrs, 0, TotalLen).

:- pred total_num_code_points_acc(list(string)::in, int::in, int::out) is det.

total_num_code_points_acc([], !TotalLen).
total_num_code_points_acc([Str | Strs], !TotalLen) :-
    !:TotalLen = !.TotalLen + string.count_code_points(Str),
    total_num_code_points_acc(Strs, !TotalLen).

total_num_code_points_is_at_most(State, MaxLen) :-
    State = state(RevStrs),
    total_num_code_points_is_at_most_loop(RevStrs, MaxLen).

:- pred total_num_code_points_is_at_most_loop(list(string)::in, int::in)
    is semidet.

total_num_code_points_is_at_most_loop([], _MaxLen0).
total_num_code_points_is_at_most_loop([Str | Strs], MaxLen0) :-
    string.count_code_points(Str, StrLen),
    ( if MaxLen0 >= StrLen then
        MaxLen1 = MaxLen0 - StrLen,
        total_num_code_points_is_at_most_loop(Strs, MaxLen1)
    else
        fail
    ).

%---------------------%

to_string(State) = String :-
    State = state(RevStrings),
    String = string.append_list(list.reverse(RevStrings)).

%---------------------------------------------------------------------------%

:- instance stream.stream(string.builder.handle, string.builder.state)
        where [
    pred(name/4) is string_builder_name
].

:- instance stream.output(string.builder.handle, string.builder.state)
        where [
    pred(flush/3) is string_builder_flush
].

:- instance stream.writer(string.builder.handle, string, string.builder.state)
        where [
    pred(put/4) is string_builder_put_string
].

:- instance stream.writer(string.builder.handle, char, string.builder.state)
        where [
    pred(put/4) is string_builder_put_char
].

%---------------------------------------------------------------------------%

:- pred string_builder_name(string.builder.handle::in, string::out,
    string.builder.state::di, string.builder.state::uo) is det.

string_builder_name(_Handle, "<<string builder stream>>", !State).

:- pred string_builder_flush(string.builder.handle::in,
    string.builder.state::di, string.builder.state::uo) is det.

string_builder_flush(_Handle, !State).

:- pred string_builder_put_string(string.builder.handle::in, string::in,
    string.builder.state::di, string.builder.state::uo) is det.

string_builder_put_string(_Handle, Str, !State) :-
    append_string(Str, !State).

:- pred string_builder_put_char(string.builder.handle::in, character::in,
    string.builder.state::di, string.builder.state::uo) is det.

string_builder_put_char(_Handle, Char, !State) :-
    append_char(Char, !State).

%---------------------------------------------------------------------------%
:- end_module string.builder.
%---------------------------------------------------------------------------%
