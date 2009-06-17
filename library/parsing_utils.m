%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 2009 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
% 
% File: parsing_utils.m
% Author: Ralph Becket <rafe@csse.unimelb.edu.au>
% Stability: low
%
% Utilities for recursive descent parsers.  Parsers take at least three
% arguments: a source (src) containing the input string and a parser
% state (ps) input/output pair tracking the current offset into the input.
%
% A new src and ps can be constructed by calling
% new_src_and_ps(InputString, SkipWS, Src, !:PS) where the SkipWS function
% is used by the primitive parsers to skip over any following whitespace
% (providing a skipping function allows users to define comments as
% whitespace).
% Parsing predicates are semidet and typically take the form
% p(...parameters..., Src, Result, !PS).  A parser matching variable
% assignments of the form `x = 42' might be defined like this:
%
%   var_assignment(Src, {Var, Value}, !PS) :-
%       var(Src, Var, !PS),
%       punct(Src, "=", !PS),
%       expr(Src, Expr, !PS).
%
% where var/4 and expr/4 are parsers for variables and expressions
% respectively and punct/4 is provided by this module for matching
% punctuation.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module parsing_utils.

:- interface.

:- import_module char.
:- import_module float.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module string.
:- import_module unit.



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
:- inst parser == ( pred(in, out, in, out) is semidet ).

    % The following are for parsers that also transform a separate state value.
    %
:- type parser_with_state(T, S) == pred(src, T, S, S, ps, ps).
:- inst parser_with_state == ( pred(in, out, in, out, in, out) is semidet ).

    % Construct a new parser source and state from a string, also specifying
    % a function for skipping over whitespace (several primitive parsers
    % use this function to consume whitespace after a token; this argument
    % allows the user to specify a function for, say, skipping over comments
    % as well).
    %
:- pred new_src_and_ps(string::in,
        (func(src, ps) = ps)::in(func(in, in) = out is det),
        src::out, ps::out) is det.

    % Construct a new parser source and state from a string (the default
    % whitespace parser is used).
    %
:- pred new_src_and_ps(string::in, src::out, ps::out) is det.

    % Obtain the current offset from the start of the input string
    % (the first character in the input has offset 0).
    %
:- pred current_offset(src::in, int::out,
        ps::in, ps::out) is det.

    % Compute a structure from the parser source which can be used to
    % convert offsets into line numbers and positions in the file (this
    % is useful for error reporting).
    %
:- type line_numbers.

:- func src_to_line_numbers(src) = line_numbers.

    % Convert an offset into a line number and position within the line
    % (the first line is number 1; the first character in a line is
    % position 1).
    %
:- pred offset_to_line_number_and_position(line_numbers::in, int::in,
        int::out, int::out) is det.

    % input_substring(Src, StartOffset, EndOffsetPlusOne, Substring)
    % Copy the substring from the input occupying the offsets
    % [StartOffset, EndOffsetPlusOne).
    %
:- pred input_substring(src::in, int::in, int::in, string::out) is semidet.

    % Read the next char.
    %
:- pred next_char(src::in, char::out,
        ps::in, ps::out) is semidet.

    % Match a char from the given string.
    %
:- pred char_in_class(string::in, src::in, char::out,
        ps::in, ps::out) is semidet.

    % Match a string exactly and any subsequent whitespace.
    %
:- pred punct(string::in, src::in, unit::out,
        ps::in, ps::out) is semidet.

    % keyword(IdChars, Keyword, Src, _, !PS) matches Keyword exactly (i.e., it
    % must not be followed by any character in IdChars) and any subsequent
    % whitespace.
    %
:- pred keyword(string::in, string::in, src::in, unit::out,
        ps::in, ps::out) is semidet.

    % identifier(Src, InitIdChars, IdChars, Identifier, !PS) matches the next
    % identifer (result in Identifier) comprising a char from InitIdChars
    % followed by zero or more chars from IdChars.  Any subsequent whitespace
    % is consumed.
    %
:- pred identifier(string::in, string::in, src::in, string::out,
        ps::in, ps::out) is semidet.

    % Consume any whitespace (defined as a sequence of characters
    % satisfying char.is_whitespace).
    %
:- pred whitespace(src::in, unit::out,
        ps::in, ps::out) is semidet.

    % Consume any input up to, and including, the next newline character
    % marking the end of the current line.
    %
:- pred skip_to_eol(src::in, unit::out,
        ps::in, ps::out) is semidet.

    % Succeed if we have reached the end of the input.
    %
:- pred eof(src::in, unit::out,
        ps::in, ps::out) is semidet.

    % Parse a float literal matching [-][0-9]+[.][0-9]+([Ee][-][0-9]+)?
    % followed by any whitespace.  The float_literal_as_string version simply
    % returns the matched string.  The float_literal version uses
    % string.to_float to convert the output of float_literal_as_string; this
    % may return an approximate answer since not all floating point numbers
    % can be perfectly represented as Mercury floats.
    %
:- pred float_literal_as_string(src::in, string::out,
        ps::in, ps::out) is semidet.
:- pred float_literal(src::in, float::out,
        ps::in, ps::out) is semidet.

    % Parse an int literal matching [-][0-9]+, not followed by [.][0-9]+,
    % followed by any whitespace.  The int_literal_as_string version simply
    % returns the matched string.  The int_literal version uses string.to_int
    % to convert the output of int_literal_as_string; this may fail if the
    % number in question cannot be represented as a Mercury int.
    %
:- pred int_literal_as_string(src::in, string::out,
        ps::in, ps::out) is semidet.
:- pred int_literal(src::in, int::out,
        ps::in, ps::out) is semidet.
    
    % Parse an string literal.  The string argument is the quote character.
    % A backslash (\) character in the string makes the next character
    % literal (e.g., for embedding quotes).  These 'escaped' characters
    % are included as-is in the result, along with the preceding backslash.
    % Any following whitespace is also consumed.
    %
:- pred string_literal(char::in, src::in, string::out,
        ps::in, ps::out) is semidet.

    % optional(P, Src, Result, !PS) returns Result = yes(X), if P(Src, X, !PS),
    % or Result = no if P does not succeed.
    %
:- pred optional(parser(T)::in(parser), src::in, maybe(T)::out,
        ps::in, ps::out) is semidet.

    % zero_or_more(P, Src, Xs, !PS) returns the list of results Xs obtained
    % by repeatedly applying P until P fails.  The nth item in Xs is
    % the result from the nth application of P.
    %
:- pred zero_or_more(parser(T)::in(parser), src::in, list(T)::out,
        ps::in, ps::out) is semidet.

    % one_or_more(P, Src, Xs, !PS) returns the list of results Xs obtained
    % by repeatedly applying P until P fails.  The nth item in Xs is
    % the result from the nth application of P.  P must succeed at
    % least once.
    %
:- pred one_or_more(parser(T)::in(parser), src::in, list(T)::out,
        ps::in, ps::out) is semidet.

    % brackets(L, R, P, Src, X, !PS) is equivalent to
    %   punct(L, Src, _, !PS), P(Src, X, !PS), punct(R, Src, _, !PS).
    %
:- pred brackets(string::in, string::in, parser(T)::in(parser), src::in,
        T::out, ps::in, ps::out) is semidet.

    % separated_list(Separator, P, Src, Xs, !PS) is like
    % zero_or_more(P, Src, Xs, !PS) except that successive applications of
    % P must be separated by punct(Separator, Src, _, !PS).
    %
:- pred separated_list(string::in, parser(T)::in(parser), src::in, 
        list(T)::out, ps::in, ps::out) is semidet.

    % comma_separated_list(P, Src, Xs) is the same as
    %   separated_list(",", P, Src, Xs).
    %
:- pred comma_separated_list(parser(T)::in(parser), src::in, list(T)::out,
        ps::in, ps::out) is semidet.

% The following parser combinators are equivalent to the above, except that
% a separate state argument is threaded through the computation (e.g., for
% parsers that incrementally construct a symbol table).

    % optional(P, Src, Result, !S, !PS) returns Result = yes(X),
    % if P(Src, X, !S, !PS), or Result = no if P does not succeed.
    %
:- pred optional(parser_with_state(T, S)::in(parser_with_state), src::in,
        maybe(T)::out, S::in, S::out, ps::in, ps::out) is semidet.

    % zero_or_more(P, Src, Xs, !S, !PS) returns the list of results Xs obtained
    % by repeatedly applying P until P fails.  The nth item in Xs is
    % the result from the nth application of P.
    %
:- pred zero_or_more(parser_with_state(T, S)::in(parser_with_state), src::in,
        list(T)::out, S::in, S::out, ps::in, ps::out) is semidet.

    % one_or_more(P, Src, Xs, !S, !PS) returns the list of results Xs obtained
    % by repeatedly applying P until P fails.  The nth item in Xs is
    % the result from the nth application of P.  P must succeed at
    % least once.
    %
:- pred one_or_more(parser_with_state(T, S)::in(parser_with_state), src::in,
        list(T)::out, S::in, S::out, ps::in, ps::out) is semidet.

    % brackets(L, R, P, Src, X, !S, !PS) is equivalent to
    %   punct(L, Src, _, !PS), P(Src, X, !S, !PS), punct(R, Src, _, !PS).
    %
:- pred brackets(string::in, string::in,
        parser_with_state(T, S)::in(parser_with_state), src::in,
        T::out, S::in, S::out, ps::in, ps::out) is semidet.

    % separated_list(Separator, P, Src, Xs, !S, !PS) is like
    % zero_or_more(P, Src, Xs, !S, !PS) except that successive applications of
    % P must be separated by punct(Separator, Src, _, !PS).
    %
:- pred separated_list(string::in,
        parser_with_state(T, S)::in(parser_with_state),
        src::in, list(T)::out, S::in, S::out, ps::in, ps::out) is semidet.

    % comma_separated_list(P, Src, Xs, !S, !PS) is the same as
    %   separated_list(",", P, Src, Xs, !S, !PS).
    %
:- pred comma_separated_list(parser_with_state(T, S)::in(parser_with_state),
        src::in, list(T)::out, S::in, S::out, ps::in, ps::out) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module array.



    % The parser "state" is just the offset into the input string.
    %
:- type ps == int.

:- type src
    --->    src(
                input_length    ::  int,
                input_string    ::  string,
                skip_ws_func    ::  func(src, ps) = ps
            ).

%-----------------------------------------------------------------------------%

new_src_and_ps(InputString, Src, PS) :-
    new_src_and_ps(InputString, skip_whitespace, Src, PS).

%-----------------------------------------------------------------------------%

new_src_and_ps(InputString, SkipWS, Src, PS) :-
    Src = src(string.length(InputString), InputString, SkipWS),
    PS = 0.

%-----------------------------------------------------------------------------%

:- func skip_whitespace(src, ps) = ps.

skip_whitespace(Src, PS0) =
    ( if whitespace(Src, _, PS0, PS) then PS else PS0 ).

%-----------------------------------------------------------------------------%

:- pred skip_whitespace(src::in, ps::in, ps::out) is det.

skip_whitespace(Src, PS0, PS) :-
    SkipWS = Src ^ skip_ws_func,
    PS = SkipWS(Src, PS0).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
% Low-level predicates.
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- type line_numbers == array(int).

%-----------------------------------------------------------------------------%

src_to_line_numbers(Src) = LineNos :-
    Str = Src ^ input_string,
    Lo = 0,
    Hi = Src ^ input_length - 1,
    F = ( func(I, Ns) =
        ( if string.unsafe_index(Str, I) = ('\n') then [I | Ns] else Ns )
    ),
    LineNosList = int.fold_down(F, Lo, Hi, []),
    LineNos = array(LineNosList).

%-----------------------------------------------------------------------------%

offset_to_line_number_and_position(LineNos, Offset, LineNo, Pos) :-
    Lo = 0,
    Hi = array.size(LineNos) - 1,
    offset_to_line_number_and_position_2(LineNos, Lo, Hi, Offset, LineNo, Pos).

%-----------------------------------------------------------------------------%

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
        LoOffset = LineNos ^ elem(Lo),
        LineNo = 1 + Lo,
        Pos = 1 + Offset - LoOffset
    ).

%-----------------------------------------------------------------------------%

current_offset(_Src, Offset, !PS) :-
    Offset = !.PS.

%-----------------------------------------------------------------------------%

eof(Src, unit, !PS) :-
    current_offset(Src, Offset, !PS),
    Offset = Src ^ input_length.

%-----------------------------------------------------------------------------%

next_char(Src, Char, !PS) :-
    current_offset(Src, Offset, !PS),
    Offset < Src ^ input_length,
    Char = Src ^ input_string ^ unsafe_elem(Offset),
    !:PS = !.PS + 1.

%-----------------------------------------------------------------------------%

char_in_class(CharClass, Src, Char, !PS) :-
    next_char(Src, Char, !PS),
    string.contains_char(CharClass, Char).

%-----------------------------------------------------------------------------%

input_substring(Src, Start, EndPlusOne, Substring) :-
    EndPlusOne =< Src ^ input_length,
    Substring =
        unsafe_substring(Src ^ input_string, Start, EndPlusOne - Start).

%-----------------------------------------------------------------------------%

:- pred match_string(string::in, src::in,
        ps::in, ps::out) is semidet.

match_string(MatchStr, Src, PS, PS + N) :-
    N = string.length(MatchStr),
    PS + N =< Src ^ input_length,
    match_string_2(N, 0, MatchStr, PS, Src ^ input_string).


:- pred match_string_2(int::in, int::in, string::in, int::in, string::in)
        is semidet.

match_string_2(N, I, MatchStr, Offset, Str) :-
    ( if I < N then
        MatchStr ^ unsafe_elem(I) = Str ^ unsafe_elem(Offset + I),
        match_string_2(N, I + 1, MatchStr, Offset, Str)
      else
        true
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
% Utility predicates.
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

optional(P, Src, Result, !PS) :-
    ( if P(Src, X, !PS) then
        Result = yes(X)
      else
        Result = no,
        semidet_true
    ).

%-----------------------------------------------------------------------------%

optional(P, Src, Result, !S, !PS) :-
    ( if P(Src, X, !S, !PS) then
        Result = yes(X)
      else
        Result = no,
        semidet_true
    ).

%-----------------------------------------------------------------------------%

zero_or_more(P, Src, Result, !PS) :-
    zero_or_more_2(P, Src, [], RevResult, !PS),
    Result = list.reverse(RevResult).


    % We use an auxiliary predicate to make this tail recursive.  This can
    % be an issue with long sequences.
    %
:- pred zero_or_more_2(parser(T)::in(parser), src::in,
        list(T)::in, list(T)::out, ps::in, ps::out) is semidet.

zero_or_more_2(P, Src, !RevResult, !PS) :-
    ( if P(Src, X, !PS) then
        list.cons(X, !RevResult),
        zero_or_more_2(P, Src, !RevResult, !PS)
      else
        semidet_true
    ).

%-----------------------------------------------------------------------------%

zero_or_more(P, Src, Result, !S, !PS) :-
    zero_or_more_2(P, Src, [], RevResult, !S, !PS),
    Result = list.reverse(RevResult).


:- pred zero_or_more_2(parser_with_state(T, S)::in(parser_with_state), src::in,
        list(T)::in, list(T)::out, S::in, S::out, ps::in, ps::out) is semidet.

zero_or_more_2(P, Src, !RevResult, !S, !PS) :-
    ( if P(Src, X, !S, !PS) then
        list.cons(X, !RevResult),
        zero_or_more_2(P, Src, !RevResult, !S, !PS)
      else
        semidet_true
    ).

%-----------------------------------------------------------------------------%

one_or_more(P, Src, Result, !PS) :-
    P(Src, X, !PS),
    zero_or_more(P, Src, Xs, !PS),
    Result = [X | Xs].

%-----------------------------------------------------------------------------%

one_or_more(P, Src, Result, !S, !PS) :-
    P(Src, X, !S, !PS),
    zero_or_more(P, Src, Xs, !S, !PS),
    Result = [X | Xs].

%-----------------------------------------------------------------------------%

brackets(L, R, P, Src, Result, !PS) :-
    punct(L, Src, _, !PS),
    P(Src, Result, !PS),
    punct(R, Src, _, !PS).

%-----------------------------------------------------------------------------%

brackets(L, R, P, Src, Result, !S, !PS) :-
    punct(L, Src, _, !PS),
    P(Src, Result, !S, !PS),
    punct(R, Src, _, !PS).

%-----------------------------------------------------------------------------%

separated_list(Separator, P, Src, Result, !PS) :-
    CommaP = ( pred(CommaPSrc::in, CommaPX::out, !.PS::in, !:PS::out)
            is semidet :-
        punct(Separator, CommaPSrc, _, !PS),
        P(CommaPSrc, CommaPX, !PS)
    ),
    P(Src, X, !PS),
    zero_or_more(CommaP, Src, Xs, !PS),
    Result = [X | Xs].

%-----------------------------------------------------------------------------%

separated_list(Separator, P, Src, Result, !S, !PS) :-
    CommaP = ( pred(CommaPSrc::in, CommaPX::out,
            !.S::in, !:S::out, !.PS::in, !:PS::out)
            is semidet :-
        punct(Separator, CommaPSrc, _, !PS),
        P(CommaPSrc, CommaPX, !S, !PS)
    ),
    P(Src, X, !S, !PS),
    zero_or_more(CommaP, Src, Xs, !S, !PS),
    Result = [X | Xs].

%-----------------------------------------------------------------------------%

comma_separated_list(P, Src, Result, !PS) :-
    separated_list(",", P, Src, Result, !PS).

%-----------------------------------------------------------------------------%

comma_separated_list(P, Src, Result, !S, !PS) :-
    separated_list(",", P, Src, Result, !S, !PS).

%-----------------------------------------------------------------------------%

whitespace(Src, unit, !PS) :-
    ( if
        next_char(Src, C, !PS),
        char.is_whitespace(C)
      then
        skip_whitespace(Src, !PS)
      else
        semidet_true
    ).

%-----------------------------------------------------------------------------%

skip_to_eol(Src, unit, !PS) :-
    next_char(Src, C, !PS),
    ( if C = ('\n') then true else skip_to_eol(Src, _, !PS) ).

%-----------------------------------------------------------------------------%

punct(Punct, Src, unit, !PS) :-
    match_string(Punct, Src, !PS),
    skip_whitespace(Src, !PS).

%---------------------------------------------------------------------------%

keyword(IdChars, Keyword, Src, unit, !PS) :-
    match_string(Keyword, Src, !PS),
    not char_in_class(IdChars, Src, _, !.PS, _),
    skip_whitespace(Src, !PS).

%-----------------------------------------------------------------------------%

float_literal_as_string(Src, FloatStr, !PS) :-
    current_offset(Src, Start, !PS),
    ( if next_char(Src, ('-'), !PS) then true else true ),
    digits(10, Src, _, !PS),
    next_char(Src, ('.'), !PS),
    digits(10, Src, _, !PS),
    ( if char_in_class("eE", Src, _, !PS) then
        ( if next_char(Src, ('-'), !PS) then true else true ),
        digits(10, Src, _, !PS)
      else
        true
    ),
    current_offset(Src, EndPlusOne, !PS),
    skip_whitespace(Src, !PS),
    input_substring(Src, Start, EndPlusOne, FloatStr).

%-----------------------------------------------------------------------------%

float_literal(Src, Float, !PS) :-
    float_literal_as_string(Src, FloatStr, !PS),
    string.to_float(FloatStr, Float).

%-----------------------------------------------------------------------------%

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

%-----------------------------------------------------------------------------%

int_literal(Src, Int, !PS) :-
    int_literal_as_string(Src, IntStr, !PS),
    string.to_int(IntStr, Int).

%-----------------------------------------------------------------------------%

:- pred digits(int::in, src::in, unit::out,
        ps::in, ps::out) is semidet.

digits(Base, Src, unit, !PS) :-
    next_char(Src, C, !PS),
    char.digit_to_int(C, D),
    D < Base,
    digits_2(Base, Src, _, !PS).


:- pred digits_2(int::in, src::in, unit::out,
        ps::in, ps::out) is semidet.

digits_2(Base, Src, unit, !PS) :-
    ( if
        next_char(Src, C, !PS),
        char.digit_to_int(C, D),
        D < Base
      then
        digits_2(Base, Src, _, !PS)
      else
        true
    ).

%-----------------------------------------------------------------------------%

string_literal(QuoteChar, Src, String, !PS) :-
    current_offset(Src, Start, !PS),
    next_char(Src, QuoteChar, !PS),
    string_literal_2(Src, QuoteChar, _, !PS),
    current_offset(Src, EndPlusOne, !PS),
    skip_whitespace(Src, !PS),
    input_substring(Src, Start + 1, EndPlusOne - 1, String).

%-----------------------------------------------------------------------------%

:- pred string_literal_2(src::in, char::in, unit::out,
        ps::in, ps::out) is semidet.

string_literal_2(Src, QuoteChar, unit, !PS) :-
    next_char(Src, C, !PS),
    ( if C = QuoteChar then
        true
      else if C = ('\\') then
        next_char(Src, _, !PS),
        string_literal_2(Src, QuoteChar, _, !PS)
      else
        string_literal_2(Src, QuoteChar, _, !PS)
    ).

%-----------------------------------------------------------------------------%

identifier(InitIdChars, IdChars, Src, Identifier, !PS) :-
    current_offset(Src, Start, !PS),
    char_in_class(InitIdChars, Src, _, !PS),
    identifier_2(IdChars, Src, _, !PS),
    current_offset(Src, EndPlusOne, !PS),
    skip_whitespace(Src, !PS),
    input_substring(Src, Start, EndPlusOne, Identifier).

%-----------------------------------------------------------------------------%

:- pred identifier_2(string::in, src::in, unit::out,
        ps::in, ps::out) is semidet.

identifier_2(IdChars, Src, unit, !PS) :-
    ( if char_in_class(IdChars, Src, _, !PS) then
        identifier_2(IdChars, Src, _, !PS)
      else
        true
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
