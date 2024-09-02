%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2024 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: lines_builder.m.
%
% This module is a variant of string.builder.m. It is intended for a
% very specific use case:
%
% - building up a string that is intended to end up in a file
%   (which string.builder.m can do), but
%
% - being able to find out the line number in that file that corresponds
%   to a given string builder state (which string.builder.m *cannot* do).
%
% The intention is that users of this module will want to put references
% to the line numbers obtained in this way *into the string being built*.
% The motivating example is the construction of the C source files
% that contain mostly machine-generated code, but may also include some
% user-written code. To associate the proper context with each line
% (so that the C compiler can report the right context for any error),
% one can put two #line directives around each piece of user-written code.
% The first directive, just before a piece of user-written code from
% somewhere else would set the context to the location of that
% "somewhere else", while the second directive, just after it, would reset
% the context to its actual position context of the file being constructed.
% If this second directive is on e.g. line 456, it would need to tell
% the C compiler that the next line is line 457.
%
% Supporting this capability requites lines_builder.m to use <in,out> pairs
% of string builder states, while string.builder.m uses <di,uo> pairs.
% This is because With <di,uo> pairs, you can either get the current
% line number out of a state, or add new strings to that state, but you
% cannot do both.
%
% This difference in modes also means that unlike string.builder.state,
% the lines_builder type cannot be a member of the stream typeclasses
% whose operations require <di,uo> pairs of states.
%
%---------------------------------------------------------------------------%

:- module libs.lines_builder.
:- interface.

:- import_module char.
:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

:- type lines_builder.

    % init(FileName, InitialLineNumber):
    %
    % Return a string builder that is intended to construct the new contents
    % of FileName, whose initial line number is InitialLineNumber.
    % (The usual value to specify for InitialLineNumber is 1.)
    %
:- func init(string, int) = lines_builder.

    % Add a character to the end of the string builder.
    %
:- pred append_char(char::in,
    lines_builder::in, lines_builder::out) is det.

    % Add a string to the end of the string builder.
    %
:- pred append_string(string::in,
    lines_builder::in, lines_builder::out) is det.

    % Add a list of strings to the end of the string builder.
    %
:- pred append_strings(list(string)::in,
    lines_builder::in, lines_builder::out) is det.

    % append_strings_sep(Sep, Strings, !LB):
    %
    % Add a list of strings to the end of the string builder,
    % with the given separator string between each pair.
    %
:- pred append_strings_sep(string::in, list(string)::in,
    lines_builder::in, lines_builder::out) is det.

%---------------------%

:- pred format(string::in, list(poly_type)::in,
    lines_builder::in, lines_builder::out) is det.

%---------------------%

    % Return the current context as a filename and a line number.
    %
:- pred current_context(lines_builder::in, string::out, int::out) is det.

    % Return the total number of code points in the string that
    % to_string would return, without constructing that string (yet).
    %
:- func total_num_code_points(lines_builder) = int.

    % Succeed if and only if the total number of code points in the string
    % that to_string would return is at most the given number. Determine this
    % without constructing that string (yet).
    %
:- pred total_num_code_points_is_at_most(lines_builder::in, int::in)
    is semidet.

%---------------------%

    % Return the string that the previous calls to append_* constructed.
    %
:- func to_string(lines_builder) = string.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.

%---------------------------------------------------------------------------%

:- type lines_builder
    --->    lines_builder(
                % The FileName argument given to the initialization function.
                string,

                % The InitialLineNumber argument given to the initialization
                % function, PLUS the number of newline characters in the
                % third field. We update this field whenever we add a string
                % to the third field.
                int,

                % A reversed list of the strings that, when unreversed
                % and appended together, yields the string we have built.
                list(string)
            ).

%---------------------------------------------------------------------------%

init(FileName, InitialLineNumber) = LB :-
    LB = lines_builder(FileName, InitialLineNumber, []).

%---------------------%

append_char(Char, !LB) :-
    !.LB = lines_builder(FileName, LineNumber0, RevStrings0),
    count_newlines_in_char(Char, LineNumber0, LineNumber),
    RevStrings = [string.char_to_string(Char) | RevStrings0],
    !:LB = lines_builder(FileName, LineNumber, RevStrings).

append_string(Str, !LB) :-
    !.LB = lines_builder(FileName, LineNumber0, RevStrs0),
    count_newlines_in_str(Str, LineNumber0, LineNumber),
    RevStrs = [Str | RevStrs0],
    !:LB = lines_builder(FileName, LineNumber, RevStrs).

%---------------------%

:- pred count_newlines_in_char(char::in, int::in, int::out) is det.
:- pragma inline(pred(count_newlines_in_char/3)).

count_newlines_in_char(Char, !LineNumber) :-
    ( if Char = '\n' then
        !:LineNumber = !.LineNumber + 1
    else
        true
    ).

:- pred count_newlines_in_str(string::in, int::in, int::out) is det.
:- pragma inline(pred(count_newlines_in_str/3)).

count_newlines_in_str(Str, !LineNumber) :-
    count_newlines_in_str_loop(Str, 0, !LineNumber).

:- pred count_newlines_in_str_loop(string::in, int::in, int::in, int::out)
    is det.

count_newlines_in_str_loop(Str, CurIndex, !LineNumber) :-
    ( if unsafe_index_next(Str, CurIndex, NextIndex, Char) then
        count_newlines_in_char(Char, !LineNumber),
        count_newlines_in_str_loop(Str, NextIndex, !LineNumber)
    else
        true
    ).

%---------------------%

append_strings([], !LB).
append_strings([Str | Strs], !LB) :-
    append_string(Str, !LB),
    append_strings(Strs, !LB).

append_strings_sep(_, [], !LB).
append_strings_sep(Sep, [Str | Strs], !LB) :-
    append_strings_sep_lag(Sep, Str, Strs, !LB).

:- pred append_strings_sep_lag(string::in, string::in, list(string)::in,
    lines_builder::in, lines_builder::out) is det.

append_strings_sep_lag(Sep, HeadStr, TailStrs, !LB) :-
    append_string(HeadStr, !LB),
    (
        TailStrs = []
    ;
        TailStrs = [HeadTailStr | TailTailStrs],
        append_string(Sep, !LB),
        append_strings_sep_lag(Sep, HeadTailStr, TailTailStrs, !LB)
    ).

%---------------------%

format(FormatStr, PolyTypes, !LB) :-
    disable_warning [unknown_format_calls] (
        string.format(FormatStr, PolyTypes, Str)
    ),
    append_string(Str, !LB).

%---------------------%

current_context(LB, FileName, LineNumber) :-
    LB = lines_builder(FileName, LineNumber, _).

total_num_code_points(LB) = TotalLen :-
    LB = lines_builder(_, _, RevStrs),
    total_num_code_points_acc(RevStrs, 0, TotalLen).

:- pred total_num_code_points_acc(list(string)::in, int::in, int::out) is det.

total_num_code_points_acc([], !TotalLen).
total_num_code_points_acc([Str | Strs], !TotalLen) :-
    !:TotalLen = !.TotalLen + string.count_code_points(Str),
    total_num_code_points_acc(Strs, !TotalLen).

total_num_code_points_is_at_most(LB, MaxLen) :-
    LB = lines_builder(_, _, RevStrs),
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

to_string(LB) = String :-
    LB = lines_builder(_, _, RevStrings),
    String = string.append_list(list.reverse(RevStrings)).

%---------------------------------------------------------------------------%
:- end_module libs.lines_builder.
%---------------------------------------------------------------------------%
