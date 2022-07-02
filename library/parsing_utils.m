%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2009-2012 The University of Melbourne.
% Copyright (C) 2014-2020, 2022 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: parsing_utils.m
% Authors: Ralph Becket <rafe@csse.unimelb.edu.au>, maclarty
% Stability: low
%
% Utilities for recursive descent parsers. Parsers take at least three
% arguments: a source (src) containing the input string, and an input/output
% pair of parser states (ps) tracking the current offset into the input.
%
% Call parse(InputString, SkipWS, Parser, Result) to parse an input string
% and return an error context and message if parsing failed.
% The SkipWS predicate is used by the primitive parsers to skip over any
% following whitespace (providing a skipping predicate allows users to define
% comments as whitespace).
% Alternatively, a new src and ps can be constructed by calling
% new_src_and_ps(InputString, SkipWS, Src, !:PS).
%
% Parsing predicates are semidet and typically take the form
% p(...parameters..., Src, Result, !PS). A parser matching variable
% assignments of the form `x = 42' might be defined like this:
%
%   var_assignment(Src, {Var, Value}, !PS) :-
%       var(Src, Var, !PS),
%       punct(Src, "=", !PS),
%       expr(Src, Expr, !PS).
%
% where var/4 and expr/4 are parsers for variables and expressions respectively
% and punct/4 is provided by this module for matching punctuation.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module parsing_utils.
:- interface.

:- import_module char.
:- import_module list.
:- import_module maybe.
:- import_module unit.

%---------------------------------------------------------------------------%

    % The parser source (input string).
    %
:- type src.

    % The parser "state", passed around in DCG arguments.
    %
:- type ps.

    % These types and insts are useful for specifying "standard" parser
    % signatures.
    %
:- type parser(T) == pred(src, T, ps, ps).
:- inst parser == (pred(in, out, in, out) is semidet).

    % The following are for parsers that also transform a separate state value.
    %
:- type parser_with_state(T, S) == pred(src, T, S, S, ps, ps).
:- inst parser_with_state == (pred(in, out, in, out, in, out) is semidet).

    % Predicates of this type are used to skip whitespace in the primitive
    % parsers provided by this module.
    %
:- type skip_whitespace_pred == parser(unit).

:- type parse_result(T)
    --->    ok(T)
    ;       error(
                error_message :: maybe(string),
                error_line    :: int,
                error_col     :: int
            ).

    % parse(Input, SkipWS, Parser, Result).
    % Try to parse Input using Parser and SkipWS to consume whitespace.
    % If Parser succeeds then return ok with the parsed value,
    % otherwise return error. If there were any calls to fail_with_message
    % without any subsequent progress being made, then the error message
    % passed to the last call to fail_with_message will be returned in the
    % error result. Otherwise no message is returned and the furthest
    % position the parser got in the input string is returned.
    %
:- pred parse(string::in, skip_whitespace_pred::in(parser),
    parser(T)::in(parser), parse_result(T)::out) is cc_multi.

    % As above but using the default whitespace parser, whitespace/4.
    %
:- pred parse(string::in, parser(T)::in(parser), parse_result(T)::out)
    is cc_multi.

%---------------------%

    % Construct a new parser source and state from a string, also specifying
    % a predicate for skipping over whitespace (several primitive parsers
    % use this predicate to consume whitespace after a token; this argument
    % allows the user to specify a predicate for, say, skipping over comments
    % as well).
    %
:- pred new_src_and_ps(string::in, skip_whitespace_pred::in(parser),
    src::out, ps::out) is det.

    % Construct a new parser source and state from a string.
    % The default whitespace parser, whitespace/4, is used.
    %
:- pred new_src_and_ps(string::in, src::out, ps::out) is det.

%---------------------%

    % Return the input string and its length from the parser source.
    %
:- pred input_string(src::in, string::out, int::out) is det.

    % Obtain the current offset from the start of the input string
    % (the first character in the input has offset 0).
    %
:- pred current_offset(src::in, int::out, ps::in, ps::out) is det.

    % Return the parser to skip over whitespace from the parser source.
    %
:- pred get_skip_whitespace_pred(src::in, skip_whitespace_pred::out(parser))
    is det.

%---------------------%

    % input_substring(Src, StartOffset, EndOffsetPlusOne, Substring):
    % Copy the substring from the input occupying the offsets
    % [StartOffset, EndOffsetPlusOne).
    %
:- pred input_substring(src::in, int::in, int::in, string::out) is semidet.

%---------------------%

:- type line_numbers.

    % Compute a structure from the parser source which can be used to
    % convert offsets into line numbers and positions in the file (this
    % is useful for error reporting).
    %
:- func src_to_line_numbers(src) = line_numbers.

    % Convert an offset into a line number and position within the line
    % (the first line is number 1; the first character in a line is
    % position 1).
    %
:- pred offset_to_line_number_and_position(line_numbers::in, int::in,
    int::out, int::out) is det.

%---------------------%

    % Read the next char.
    %
:- pred next_char(src::in, char::out, ps::in, ps::out) is semidet.

    % Read the next char but do not record progress information.
    % This is more efficient than next_char, but may produce less informative
    % error messages in case of a parse error.
    %
:- pred next_char_no_progress(src::in, char::out, ps::in, ps::out) is semidet.

%---------------------%

    % Match a char from the given string.
    %
:- pred char_in_class(string::in, src::in, char::out,
    ps::in, ps::out) is semidet.

%---------------------%

    % Match a string exactly and any subsequent whitespace.
    %
:- pred punct(string::in, src::in, unit::out, ps::in, ps::out) is semidet.

    % keyword(IdChars, Keyword, Src, _, !PS) matches Keyword exactly (i.e., it
    % must not be followed by any character in IdChars) and any subsequent
    % whitespace.
    %
:- pred keyword(string::in, string::in, src::in, unit::out,
    ps::in, ps::out) is semidet.

    % ikeyword(IdChars, Keyword, Src, _, !PS)
    % Case-insensitive version of keyword/6.
    % Only uppercase and lowercase letters in the ASCII range (A-Z, a-z)
    % are compared case insensitively.
    %
:- pred ikeyword(string::in, string::in, src::in, unit::out,
    ps::in, ps::out) is semidet.

    % identifier(InitIdChars, IdChars, Src, Identifier, !PS) matches the next
    % identifier (result in Identifier) comprising a char from InitIdChars
    % followed by zero or more chars from IdChars. Any subsequent whitespace
    % is consumed.
    %
:- pred identifier(string::in, string::in, src::in, string::out,
    ps::in, ps::out) is semidet.

    % Consume any whitespace (defined as a sequence of characters
    % satisfying char.is_whitespace).
    %
:- pred whitespace(src::in, unit::out,
    ps::in, ps::out) is semidet.

%---------------------%

    % Consume any input up to, and including, the next newline character
    % marking the end of the current line.
    %
:- pred skip_to_eol(src::in, unit::out,
    ps::in, ps::out) is semidet.

    % Succeed if we have reached the end of the input.
    %
:- pred eof(src::in, unit::out, ps::in, ps::out) is semidet.

%---------------------%

    % Parse a float literal matching [-][0-9]+[.][0-9]+([Ee][-+][0-9]+)?
    % followed by any whitespace. The float_literal_as_string version simply
    % returns the matched string. The float_literal version uses
    % string.to_float to convert the output of float_literal_as_string; this
    % may return an approximate answer since not all floating point numbers
    % can be perfectly represented as Mercury floats.
    %
:- pred float_literal_as_string(src::in, string::out,
    ps::in, ps::out) is semidet.
:- pred float_literal(src::in, float::out,
    ps::in, ps::out) is semidet.

    % Parse an int literal matching [-][0-9]+, not followed by [.][0-9]+,
    % followed by any whitespace. The int_literal_as_string version simply
    % returns the matched string. The int_literal version uses string.to_int
    % to convert the output of int_literal_as_string; this may fail if the
    % number in question cannot be represented as a Mercury int.
    %
:- pred int_literal_as_string(src::in, string::out,
    ps::in, ps::out) is semidet.
:- pred int_literal(src::in, int::out,
    ps::in, ps::out) is semidet.

    % Parse a string literal. The string argument is the quote character.
    % A backslash (\) character in the string makes the next character
    % literal (e.g., for embedding quotes). These 'escaped' characters
    % are included as-is in the result, along with the preceding backslash.
    % Any following whitespace is also consumed.
    %
:- pred string_literal(char::in, src::in, string::out,
    ps::in, ps::out) is semidet.

%---------------------------------------------------------------------------%

% Each basic parser combinators has a version that has a separate state
% argument is threaded through the computation, for parsers that e.g.
% incrementally construct a symbol table.

    % optional(P, Src, Result, !PS) returns Result = yes(X) if P(Src, X, !PS),
    % or Result = no if P does not succeed.
    %
:- pred optional(parser(T)::in(parser), src::in, maybe(T)::out,
    ps::in, ps::out) is semidet.

    % optional(P, Src, Result, !S, !PS) returns Result = yes(X)
    % if P(Src, X, !S, !PS), or Result = no if P does not succeed.
    %
:- pred optional(parser_with_state(T, S)::in(parser_with_state), src::in,
    maybe(T)::out, S::in, S::out, ps::in, ps::out) is semidet.

%---------------------%

    % zero_or_more(P, Src, Xs, !PS) returns the list of results Xs obtained
    % by repeatedly applying P until P fails. The nth item in Xs is
    % the result from the nth application of P.
    %
:- pred zero_or_more(parser(T)::in(parser), src::in, list(T)::out,
    ps::in, ps::out) is semidet.

    % zero_or_more(P, Src, Xs, !S, !PS) returns the list of results Xs obtained
    % by repeatedly applying P until P fails. The nth item in Xs is
    % the result from the nth application of P.
    %
:- pred zero_or_more(parser_with_state(T, S)::in(parser_with_state), src::in,
    list(T)::out, S::in, S::out, ps::in, ps::out) is semidet.

%---------------------%

    % one_or_more(P, Src, Xs, !PS) returns the list of results Xs obtained
    % by repeatedly applying P until P fails. The nth item in Xs is
    % the result from the nth application of P. P must succeed at least once.
    %
:- pred one_or_more(parser(T)::in(parser), src::in, list(T)::out,
    ps::in, ps::out) is semidet.

    % one_or_more(P, Src, Xs, !S, !PS) returns the list of results Xs obtained
    % by repeatedly applying P until P fails. The nth item in Xs is
    % the result from the nth application of P. P must succeed at least once.
    %
:- pred one_or_more(parser_with_state(T, S)::in(parser_with_state), src::in,
    list(T)::out, S::in, S::out, ps::in, ps::out) is semidet.

%---------------------%

    % brackets(L, R, P, Src, X, !PS) is equivalent to
    %   punct(L, Src, _, !PS), P(Src, X, !PS), punct(R, Src, _, !PS).
    %
:- pred brackets(string::in, string::in, parser(T)::in(parser), src::in,
    T::out, ps::in, ps::out) is semidet.

    % brackets(L, R, P, Src, X, !S, !PS) is equivalent to
    %   punct(L, Src, _, !PS), P(Src, X, !S, !PS), punct(R, Src, _, !PS).
    %
:- pred brackets(string::in, string::in,
    parser_with_state(T, S)::in(parser_with_state), src::in,
    T::out, S::in, S::out, ps::in, ps::out) is semidet.

%---------------------%

    % separated_list(Separator, P, Src, Xs, !PS) is like
    % zero_or_more(P, Src, Xs, !PS) except that successive applications of
    % P must be separated by punct(Separator, Src, _, !PS).
    %
:- pred separated_list(string::in, parser(T)::in(parser), src::in,
    list(T)::out, ps::in, ps::out) is semidet.

    % separated_list(Separator, P, Src, Xs, !S, !PS) is like
    % zero_or_more(P, Src, Xs, !S, !PS) except that successive applications of
    % P must be separated by punct(Separator, Src, _, !PS).
    %
:- pred separated_list(string::in,
    parser_with_state(T, S)::in(parser_with_state),
    src::in, list(T)::out, S::in, S::out, ps::in, ps::out) is semidet.

%---------------------%

    % comma_separated_list(P, Src, Xs) is the same as
    %   separated_list(",", P, Src, Xs).
    %
:- pred comma_separated_list(parser(T)::in(parser), src::in, list(T)::out,
    ps::in, ps::out) is semidet.

    % comma_separated_list(P, Src, Xs, !S, !PS) is the same as
    %   separated_list(",", P, Src, Xs, !S, !PS).
    %
:- pred comma_separated_list(parser_with_state(T, S)::in(parser_with_state),
    src::in, list(T)::out, S::in, S::out, ps::in, ps::out) is semidet.

%---------------------------------------------------------------------------%

    % Declaratively this predicate is equivalent to false. Operationally,
    % it will record an error message that will be returned by parse/4
    % if no further progress is made and then fail.
    %
:- pred fail_with_message(string::in, src::in, T::out, ps::in, ps::out)
    is semidet.

    % As above, but use the given offset for the context of the message.
    %
:- pred fail_with_message(string::in, int::in, src::in, T::out,
    ps::in, ps::out) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module array.
:- import_module int.
:- import_module mutvar.
:- import_module string.

    % The parser "state" is just the offset into the input string.
    %
:- type ps == int.

:- type src
    --->    src(
                input_length        ::  int,
                input_string        ::  string,
                skip_ws_pred        ::  skip_whitespace_pred,

                furthest_offset     ::  mutvar(int),
                % This mutable records the progress of the parser
                % through the input string.

                last_fail_message   ::  mutvar(fail_message_info)
                % This mutable is used to record messages passed to
                % fail_with_message and their context.
            ).

:- type fail_message_info
    --->    fail_message_info(int, maybe(string)).

%---------------------------------------------------------------------------%

parse(InputString, SkipWS, Parser, Result) :-
    % This is pure, because it will always return the same results for
    % the same inputs (the mutable in Src cannot be accessed outside
    % of the promise_pure scope below).
    promise_pure (
        new_src_and_ps(InputString, SkipWS, Src, PS0),
        ( if Parser(Src, Val, PS0, _) then
            Result = ok(Val)
        else
            impure get_mutvar(Src ^ last_fail_message, Info),
            impure get_mutvar(Src ^ furthest_offset, FurthestOffset),
            Info = fail_message_info(MessageOffset, LastFailMsg),
            ( if MessageOffset < FurthestOffset then
                Msg = no,
                Offset = FurthestOffset
            else
                Msg = LastFailMsg,
                Offset = MessageOffset
            ),
            offset_to_line_number_and_position(src_to_line_numbers(Src),
                Offset, Line, Col),
            Result0 = error(Msg, Line, Col),
            % We make parse/4 cc_multi because declaratively
            % parse(Str, SkipWS, Parser, error(MaybeMsg, Line, Col)) is true
            % for all MaybeMsg, Line and Col iff
            %   new_src_and_ps(Str, SkipWS, Src, PS0),
            %   Parser(Src, _, PS0, _)
            % is false, but operationally MaybeMsg, Line and Col are
            % restricted to one value each.
            cc_multi_equal(Result0, Result)
        )
    ).

parse(InputString, Parser, Result) :-
    parse(InputString, whitespace, Parser, Result).

%---------------------------------------------------------------------------%

new_src_and_ps(InputString, SkipWS, Src, PS) :-
    promise_pure (
        impure new_mutvar(fail_message_info(0, no), ErrorInfoMutVar),
        impure new_mutvar(0, FurthestOffsetMutvar),
        Src = src(string.length(InputString), InputString, SkipWS,
            FurthestOffsetMutvar, ErrorInfoMutVar),
        PS = 0
    ).

new_src_and_ps(InputString, Src, PS) :-
    new_src_and_ps(InputString, whitespace, Src, PS).

%---------------------------------------------------------------------------%

input_string(Src, Src ^ input_string, Src ^ input_length).

current_offset(_Src, Offset, !PS) :-
    Offset = !.PS.

get_skip_whitespace_pred(Src, SkipWS) :-
    SkipWS0 = Src ^ skip_ws_pred,
    unsafe_skip_ws_pred_cast(SkipWS0, SkipWS).

:- pred unsafe_skip_ws_pred_cast(skip_whitespace_pred::in,
    skip_whitespace_pred::out(parser)) is det.

:- pragma foreign_proc("C",
    unsafe_skip_ws_pred_cast(SkipWS0::in, SkipWS::out(parser)),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SkipWS = SkipWS0;
").

:- pragma foreign_proc("C#",
    unsafe_skip_ws_pred_cast(SkipWS0::in, SkipWS::out(parser)),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SkipWS = SkipWS0;
").

:- pragma foreign_proc("Java",
    unsafe_skip_ws_pred_cast(SkipWS0::in, SkipWS::out(parser)),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SkipWS = SkipWS0;
").

%---------------------------------------------------------------------------%

input_substring(Src, Start, EndPlusOne, Substring) :-
    promise_pure (
        EndPlusOne =< Src ^ input_length,
        Substring = unsafe_between(Src ^ input_string, Start, EndPlusOne),
        impure record_progress(Src, Start)
    ).

%---------------------------------------------------------------------------%

    % For a source string Src, the following array contains the positions
    % of all the newline characters in the string Src ++ "\n".
    %
:- type line_numbers == array(int).

%---------------------%

src_to_line_numbers(Src) = LineNos :-
    Str = Src ^ input_string,
    src_to_line_numbers_loop(Str, 0, [], RevLineNosList),
    LineNos = array.from_reverse_list(RevLineNosList).

:- pred src_to_line_numbers_loop(string::in, int::in,
    list(int)::in, list(int)::out) is det.

src_to_line_numbers_loop(Str, Pos0, !RevLineNosList) :-
    ( if string.unsafe_index_next(Str, Pos0, Pos, Char) then
        ( if Char = '\n' then
            !:RevLineNosList = [Pos0 | !.RevLineNosList]
        else
            true
        ),
        src_to_line_numbers_loop(Str, Pos, !RevLineNosList)
    else
        !:RevLineNosList = [Pos0 | !.RevLineNosList]
    ).

%---------------------%

offset_to_line_number_and_position(LineNos, Offset, LineNo, Pos) :-
    Lo = 0,
    Hi = array.size(LineNos) - 1,
    offset_to_line_number_and_position_2(LineNos, Lo, Hi, Offset, LineNo, Pos).

:- pred offset_to_line_number_and_position_2(line_numbers::in, int::in,
    int::in, int::in, int::out, int::out) is det.

    % Perform a binary search looking for the offset of the line number
    % of the line containing Offset.
    %
offset_to_line_number_and_position_2(LineNos, Lo, Hi, Offset, LineNo, Pos) :-
    ( if Lo < Hi then
        Mid = (Lo + Hi) / 2,
        MidOffset = LineNos ^ elem(Mid),
        ( if MidOffset < Offset then
            offset_to_line_number_and_position_2(LineNos, Mid + 1, Hi, Offset,
                LineNo, Pos)
        else
            offset_to_line_number_and_position_2(LineNos, Lo, Mid, Offset,
                LineNo, Pos)
        )
    else
        % Lo is the index of the newline that terminates the line that Offset
        % is on. We compute LineBegin as the offset of the first character
        % of the line Offset is on.
        ( if Lo = 0 then
            LineBegin = 0
        else
            LineBegin = LineNos ^ elem(Lo - 1) + 1
        ),
        LineNo = 1 + Lo,
        Pos = 1 + Offset - LineBegin
    ).

%---------------------------------------------------------------------------%

next_char(Src, Char, !PS) :-
    promise_pure (
        current_offset(Src, Offset, !.PS, _),
        Offset < Src ^ input_length,
        string.unsafe_index_next(Src ^ input_string, Offset, NextOffset, Char),
        impure record_progress(Src, Offset),
        !:PS = NextOffset
    ).

next_char_no_progress(Src, Char, !PS) :-
    current_offset(Src, Offset, !.PS, _),
    Offset < Src ^ input_length,
    string.unsafe_index_next(Src ^ input_string, Offset, NextOffset, Char),
    !:PS = NextOffset.

%---------------------------------------------------------------------------%

char_in_class(CharClass, Src, Char, !PS) :-
    next_char(Src, Char, !PS),
    string.contains_char(CharClass, Char).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

punct(Punct, Src, unit, !PS) :-
    match_string(Punct, Src, !PS),
    skip_whitespace(Src, !PS).

%---------------------------------------------------------------------------%

keyword(IdChars, Keyword, Src, unit, !PS) :-
    match_string(Keyword, Src, !PS),
    not char_in_class(IdChars, Src, _, !.PS, _),
    skip_whitespace(Src, !PS).

ikeyword(IdChars, Keyword, Src, unit, !PS) :-
    imatch_string(Keyword, Src, !PS),
    not char_in_class(IdChars, Src, _, !.PS, _),
    skip_whitespace(Src, !PS).

%---------------------------------------------------------------------------%

identifier(InitIdChars, IdChars, Src, Identifier, !PS) :-
    current_offset(Src, Start, !PS),
    char_in_class(InitIdChars, Src, _, !PS),
    identifier_2(IdChars, Src, _, !PS),
    current_offset(Src, EndPlusOne, !PS),
    skip_whitespace(Src, !PS),
    input_substring(Src, Start, EndPlusOne, Identifier).

:- pred identifier_2(string::in, src::in, unit::out,
    ps::in, ps::out) is semidet.

identifier_2(IdChars, Src, unit, !PS) :-
    ( if char_in_class(IdChars, Src, _, !PS) then
        disable_warning [suspicious_recursion] (
            identifier_2(IdChars, Src, _, !PS)
        )
    else
        true
    ).

%---------------------------------------------------------------------------%

whitespace(Src, unit, !PS) :-
    ( if
        next_char(Src, C, !PS),
        char.is_whitespace(C)
    then
        disable_warning [suspicious_recursion] (
            whitespace(Src, _, !PS)
        )
    else
        semidet_true
    ).

%---------------------------------------------------------------------------%

skip_to_eol(Src, unit, !PS) :-
    next_char(Src, C, !PS),
    ( if C = ('\n') then
        true
    else
        disable_warning [suspicious_recursion] (
            skip_to_eol(Src, _, !PS)
        )
    ).

%---------------------------------------------------------------------------%

eof(Src, unit, !PS) :-
    current_offset(Src, Offset, !PS),
    Offset = Src ^ input_length.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

float_literal_as_string(Src, FloatStr, !PS) :-
    current_offset(Src, Start, !PS),
    ( if next_char(Src, ('-'), !PS) then true else true ),
    digits(10, Src, _, !PS),
    next_char(Src, ('.'), !PS),
    digits(10, Src, _, !PS),
    ( if char_in_class("eE", Src, _, !PS) then
        optional_sign(Src, !PS),
        digits(10, Src, _, !PS)
    else
        true
    ),
    current_offset(Src, EndPlusOne, !PS),
    skip_whitespace(Src, !PS),
    input_substring(Src, Start, EndPlusOne, FloatStr).

:- pred optional_sign(src::in, ps::in, ps::out) is det.

optional_sign(Src, !PS) :-
    ( if
        next_char(Src, Char, !PS),
        ( Char = ('-')
        ; Char = ('+')
        )
    then
        true
    else
        true
    ).

float_literal(Src, Float, !PS) :-
    float_literal_as_string(Src, FloatStr, !PS),
    string.to_float(FloatStr, Float).

%---------------------------------------------------------------------------%

int_literal_as_string(Src, IntStr, !PS) :-
    current_offset(Src, Start, !PS),
    optional(char_in_class("-"), Src, _, !PS),
    digits(10, Src, _, !PS),
    not (
        next_char(Src, ('.'), !PS),
        digits(10, Src, _, !.PS, _)
    ),
    current_offset(Src, EndPlusOne, !PS),
    skip_whitespace(Src, !PS),
    input_substring(Src, Start, EndPlusOne, IntStr).

int_literal(Src, Int, !PS) :-
    int_literal_as_string(Src, IntStr, !PS),
    string.to_int(IntStr, Int).

%---------------------------------------------------------------------------%

:- pred digits(int::in, src::in, unit::out, ps::in, ps::out) is semidet.

digits(Base, Src, unit, !PS) :-
    next_char(Src, C, !PS),
    char.is_base_digit(Base, C),
    digits_2(Base, Src, _, !PS).

:- pred digits_2(int::in, src::in, unit::out, ps::in, ps::out) is semidet.

digits_2(Base, Src, unit, !PS) :-
    ( if
        next_char(Src, C, !PS),
        char.is_base_digit(Base, C)
    then
        disable_warning [suspicious_recursion] (
            digits_2(Base, Src, _, !PS)
        )
    else
        true
    ).

%---------------------------------------------------------------------------%

string_literal(QuoteChar, Src, String, !PS) :-
    next_char(Src, QuoteChar, !PS),
    current_offset(Src, Start, !PS),
    string_literal_2(Src, QuoteChar, _, !PS),
    current_offset(Src, EndPlusOne, !PS),
    string.unsafe_prev_index(Src ^ input_string, EndPlusOne, End, QuoteChar),
    skip_whitespace(Src, !PS),
    input_substring(Src, Start, End, String).

:- pred string_literal_2(src::in, char::in, unit::out,
    ps::in, ps::out) is semidet.

string_literal_2(Src, QuoteChar, unit, !PS) :-
    next_char(Src, C, !PS),
    ( if C = QuoteChar then
        true
    else if C = ('\\') then
        next_char(Src, _, !PS),
        disable_warning [suspicious_recursion] (
            string_literal_2(Src, QuoteChar, _, !PS)
        )
    else
        disable_warning [suspicious_recursion] (
            string_literal_2(Src, QuoteChar, _, !PS)
        )
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

optional(P, Src, Result, !PS) :-
    ( if P(Src, X, !PS) then
        Result = yes(X)
    else
        Result = no,
        semidet_true
    ).

%---------------------------------------------------------------------------%

optional(P, Src, Result, !S, !PS) :-
    ( if P(Src, X, !S, !PS) then
        Result = yes(X)
    else
        Result = no,
        semidet_true
    ).

%---------------------------------------------------------------------------%

zero_or_more(P, Src, Result, !PS) :-
    zero_or_more_rev_acc(P, Src, [], RevResult, !PS),
    Result = list.reverse(RevResult).

    % We use an auxiliary predicate to make this tail recursive.
    % This can be an issue with long sequences.
    %
:- pred zero_or_more_rev_acc(parser(T)::in(parser), src::in,
    list(T)::in, list(T)::out, ps::in, ps::out) is semidet.

zero_or_more_rev_acc(P, Src, !RevResult, !PS) :-
    ( if P(Src, X, !PS) then
        list.cons(X, !RevResult),
        disable_warning [suspicious_recursion] (
            zero_or_more_rev_acc(P, Src, !RevResult, !PS)
        )
    else
        semidet_true
    ).

zero_or_more(P, Src, Result, !S, !PS) :-
    zero_or_more_rev_acc(P, Src, [], RevResult, !S, !PS),
    Result = list.reverse(RevResult).

:- pred zero_or_more_rev_acc(parser_with_state(T, S)::in(parser_with_state),
    src::in, list(T)::in, list(T)::out, S::in, S::out, ps::in, ps::out)
    is semidet.

zero_or_more_rev_acc(P, Src, !RevResult, !S, !PS) :-
    ( if P(Src, X, !S, !PS) then
        list.cons(X, !RevResult),
        disable_warning [suspicious_recursion] (
            zero_or_more_rev_acc(P, Src, !RevResult, !S, !PS)
        )
    else
        semidet_true
    ).

%---------------------------------------------------------------------------%

one_or_more(P, Src, Result, !PS) :-
    P(Src, X, !PS),
    zero_or_more(P, Src, Xs, !PS),
    Result = [X | Xs].

one_or_more(P, Src, Result, !S, !PS) :-
    P(Src, X, !S, !PS),
    zero_or_more(P, Src, Xs, !S, !PS),
    Result = [X | Xs].

%---------------------------------------------------------------------------%

brackets(L, R, P, Src, Result, !PS) :-
    punct(L, Src, _, !PS),
    P(Src, Result, !PS),
    punct(R, Src, _, !PS).

brackets(L, R, P, Src, Result, !S, !PS) :-
    punct(L, Src, _, !PS),
    P(Src, Result, !S, !PS),
    punct(R, Src, _, !PS).

%---------------------------------------------------------------------------%

separated_list(Separator, P, Src, Result, !PS) :-
    CommaP =
        ( pred(CommaPSrc::in, CommaPX::out, !.PS::in, !:PS::out) is semidet :-
            punct(Separator, CommaPSrc, _, !PS),
            P(CommaPSrc, CommaPX, !PS)
        ),
    ( if P(Src, X, !PS) then
        zero_or_more(CommaP, Src, Xs, !PS),
        Result = [X | Xs]
    else
        Result = []
    ).

separated_list(Separator, P, Src, Result, !S, !PS) :-
    CommaP =
        ( pred(CommaPSrc::in, CommaPX::out, !.S::in, !:S::out,
                !.PS::in, !:PS::out) is semidet :-
            punct(Separator, CommaPSrc, _, !PS),
            P(CommaPSrc, CommaPX, !S, !PS)
        ),
    ( if P(Src, X, !S, !PS) then
        zero_or_more(CommaP, Src, Xs, !S, !PS),
        Result = [X | Xs]
    else
        Result = []
    ).

%---------------------------------------------------------------------------%

comma_separated_list(P, Src, Result, !PS) :-
    separated_list(",", P, Src, Result, !PS).

comma_separated_list(P, Src, Result, !S, !PS) :-
    separated_list(",", P, Src, Result, !S, !PS).

%---------------------------------------------------------------------------%

fail_with_message(Msg, Src, Val, !PS) :-
    % This is pure, because the mutable can only be accessed via
    % the parse/4 predicate which will always return the same results
    % for the same inputs.
    promise_pure (
        impure set_mutvar(Src ^ last_fail_message,
            fail_message_info(!.PS, yes(Msg))),
        impure set_mutvar(Src ^ furthest_offset, !.PS),
        ( if semidet_fail then
            dynamic_cast(0, Val) % unreachable
        else
            fail
        )
    ).

fail_with_message(Msg, Offset, Src, Val, _, PS) :-
    fail_with_message(Msg, Src, Val, Offset, PS).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- pred match_string(string::in, src::in, ps::in, ps::out) is semidet.

match_string(MatchStr, Src, PS, PS + N) :-
    promise_pure (
        impure record_progress(Src, PS),
        N = string.length(MatchStr),
        PS + N =< Src ^ input_length,
        match_string_2(N, 0, MatchStr, PS, Src ^ input_string)
    ).

:- pred match_string_2(int::in, int::in, string::in, int::in, string::in)
    is semidet.

match_string_2(N, I, MatchStr, Offset, Str) :-
    ( if I < N then
        string.unsafe_index_code_unit(MatchStr, I, CodeUnit),
        string.unsafe_index_code_unit(Str, Offset + I, CodeUnit),
        match_string_2(N, I + 1, MatchStr, Offset, Str)
    else
        true
    ).

:- pred imatch_string(string::in, src::in, ps::in, ps::out) is semidet.

imatch_string(MatchStr, Src, PS, PS + N) :-
    promise_pure (
        impure record_progress(Src, PS),
        N = string.length(MatchStr),
        PS + N =< Src ^ input_length,
        imatch_string_2(N, 0, MatchStr, PS, Src ^ input_string)
    ).

:- pred imatch_string_2(int::in, int::in, string::in, int::in, string::in)
    is semidet.

imatch_string_2(N, I, MatchStr, Offset, Str) :-
    ( if I < N then
        % We can compare by code units because char.to_upper only converts
        % letters in the ASCII range, and ASCII characters are always encoded
        % in a single code unit.
        string.unsafe_index_code_unit(MatchStr, I, CodeUnit1),
        string.unsafe_index_code_unit(Str, Offset + I, CodeUnit2),
        char.det_from_int(CodeUnit1, Chr1),
        char.det_from_int(CodeUnit2, Chr2),
        char.to_upper(Chr1) = char.to_upper(Chr2) : char,
        imatch_string_2(N, I + 1, MatchStr, Offset, Str)
    else
        true
    ).

%---------------------------------------------------------------------------%

:- pred skip_whitespace(src::in, ps::in, ps::out) is semidet.

skip_whitespace(Src, PS0, PS) :-
    get_skip_whitespace_pred(Src, SkipWS),
    SkipWS(Src, _, PS0, PS).

%---------------------------------------------------------------------------%

    % Update the furthest_offset field if any progress has been made.
    %
:- impure pred record_progress(src::in, ps::in) is det.

record_progress(Src, PS) :-
    MutVar = Src ^ furthest_offset,
    impure get_mutvar(MutVar, OS0),
    ( if PS > OS0 then
        impure set_mutvar(MutVar, PS)
    else
        true
    ).

%---------------------------------------------------------------------------%
:- end_module parsing_utils.
%---------------------------------------------------------------------------%
