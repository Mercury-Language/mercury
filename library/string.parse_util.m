%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Copyright (C) 2014-2015, 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: string.parse_runtime.m.
%
% This module parses format strings for the runtime system.
% The module string.parse_compiler.m does the same job for the compiler.
% Any changes in one of these modules will probably also require
% a corresponding change in the other.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module string.parse_util.
:- interface.

:- import_module list.

%---------------------------------------------------------------------------%

:- type string_format_flag_hash
    --->    flag_hash_clear
    ;       flag_hash_set.

:- type string_format_flag_space
    --->    flag_space_clear
    ;       flag_space_set.

:- type string_format_flag_zero
    --->    flag_zero_clear
    ;       flag_zero_set.

:- type string_format_flag_minus
    --->    flag_minus_clear
    ;       flag_minus_set.

:- type string_format_flag_plus
    --->    flag_plus_clear
    ;       flag_plus_set.

:- type string_format_flags
    --->    string_format_flags(
                flag_hash       :: string_format_flag_hash,
                flag_space      :: string_format_flag_space,
                flag_zero       :: string_format_flag_zero,
                flag_minus      :: string_format_flag_minus,
                flag_plus       :: string_format_flag_plus
            ).

:- type string_format_maybe_width
    --->    no_specified_width
    ;       specified_width(int).

:- type string_format_maybe_prec
    --->    no_specified_prec
    ;       specified_prec(int).

:- type string_format_int_base
    --->    base_octal
    ;       base_decimal
    ;       base_hex_lc
    ;       base_hex_uc
    ;       base_hex_p.

:- type string_format_float_kind
    --->    kind_e_scientific_lc
    ;       kind_e_scientific_uc
    ;       kind_f_plain_lc
    ;       kind_f_plain_uc
    ;       kind_g_flexible_lc
    ;       kind_g_flexible_uc.

:- type poly_kind
    --->    poly_kind_char
    ;       poly_kind_str
    ;       poly_kind_int
    ;       poly_kind_float.

:- type string_format_error
    --->    error_no_specifier(
                int,        % Which specifier were we expecting?
                int         % How many extra polytypes?
            )
    ;       error_unknown_specifier(
                int,        % Which specifier?
                char        % The unexpected specifier character.
            )
    ;       error_wrong_polytype(
                int,        % Which specifier?
                char,       % The specifier character.
                poly_kind   % The polytype we found.
            )
    ;       error_no_polytype(
                int,        % Which specifier?
                char        % The specifier character.
            )
    ;       error_nonint_star_width(
                int,        % Which specifier?
                poly_kind   % The non-i() polytype we found.
            )
    ;       error_missing_star_width(
                int         % Which specifier?
            )
    ;       error_nonint_star_prec(
                int,        % Which specifier?
                poly_kind   % The non-i() polytype we found.
            )
    ;       error_missing_star_prec(
                int         % Which specifier?
            )
    ;       error_extra_polytypes(
                int,        % Which specifier were we expecting?
                int         % How many extra polytypes?
            ).

    % Convert an internal report of an error discovered in a format string
    % to an error message that can be delivered to users.
    %
:- func string_format_error_to_msg(string_format_error) = string.

:- type gather_ended_by
    --->    found_end_of_string
    ;       found_percent(list(char)).  % The list of chars after the percent.

    % gather_non_percent_chars(Chars, ManifestChars, EndedBy):
    %
    % Parse the part of a format string which doesn't contain any conversion
    % specifications. The manifest characters in the format string up to
    % the next percent sign (if any) are returned in ManifestChars.
    % If these were ended by a percent sign, then the characters after the
    % percent sign are returning as the argument of found_percent in EndedBy.
    %
:- pred gather_non_percent_chars(list(char)::in, list(char)::out,
    gather_ended_by::out) is det.

    % Record and skip past any flag characters at the start of the char list.
    %
:- pred gather_flag_chars(list(char)::in, list(char)::out,
    string_format_flags::in, string_format_flags::out) is det.

    % get_number_prefix(!Chars, N):
    %
    % Consume any decimal digits at the start of !.Chars. Return the value
    % of the digits found in N, and the left over characters in !:Chars.
    % If there are no decimal digits at the start !.Chars, return 0 for N,
    % and leave !Chars unchanged.
    %
:- pred get_number_prefix(list(char)::in, list(char)::out,
    int::out) is det.

    % get_nonzero_number_prefix(!Chars, N):
    %
    % Consume any decimal digits at the start of !.Chars. Return the value
    % of the digits found in N, and the left over characters in !:Chars.
    % If there are no decimal digits at the start !.Chars, or if the first
    % such digit is a zero, fail.
    %
:- pred get_nonzero_number_prefix(list(char)::in, list(char)::out,
    int::out) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module maybe.
:- import_module require.

%---------------------------------------------------------------------------%

string_format_error_to_msg(Error) = Msg :-
    (
        Error = error_no_specifier(SpecNum, NumExtraPolyTypes),
        Msg0 = nth_specifier(SpecNum) ++ " is missing",
        ( if NumExtraPolyTypes = 0 then
            Msg = Msg0 ++ ", along with its input."
        else if NumExtraPolyTypes = 1 then
            Msg = Msg0 ++ "."
        else
            Msg = Msg0 ++ ", and there are "
                ++ string.int_to_string(NumExtraPolyTypes - 1)
                ++ " extra inputs."
        )
    ;
        Error = error_unknown_specifier(SpecNum, SpecChar),
        Msg = nth_specifier(SpecNum) ++ " uses the unknown "
            ++ specifier_char(SpecChar) ++ "."
    ;
        Error = error_wrong_polytype(SpecNum, SpecChar, PolyKind),
        Msg = nth_specifier(SpecNum) ++ " uses the "
            ++ specifier_char(SpecChar)
            ++ ", but the corresponding input is "
            ++ poly_kind_desc(PolyKind) ++ "."
    ;
        Error = error_no_polytype(SpecNum, SpecChar),
        Msg = nth_specifier(SpecNum)
            ++ ", which uses " ++ specifier_char(SpecChar)
            ++ ", is missing its input."
    ;
        Error = error_nonint_star_width(SpecNum, PolyKind),
        Msg = nth_specifier(SpecNum)
            ++ " says the width is a runtime input,"
            ++ " but the next input is " ++ poly_kind_desc(PolyKind)
            ++ ", not an integer."
    ;
        Error = error_missing_star_width(SpecNum),
        Msg = nth_specifier(SpecNum)
            ++ " says the width is a runtime input,"
            ++ " but there is no next input."
    ;
        Error = error_nonint_star_prec(SpecNum, PolyKind),
        Msg = nth_specifier(SpecNum)
            ++ " says the precision is a runtime input,"
            ++ " but the next input is " ++ poly_kind_desc(PolyKind)
            ++ ", not an integer."
    ;
        Error = error_missing_star_prec(SpecNum),
        Msg = nth_specifier(SpecNum)
            ++ " says the precision is a runtime input,"
            ++ " but there is no next input."
    ;
        Error = error_extra_polytypes(SpecNum, NumExtraPolyTypes),
        ( if SpecNum = 1 then
            % They aren't extra, since there is no other inputs before them.
            Extra = ""
        else
            Extra = "extra "
        ),
        Msg0 = "There is no " ++ nth(SpecNum) ++ " conversion specifier,",
        ( if NumExtraPolyTypes = 1 then
            Msg = Msg0 ++ " but there is an " ++ Extra ++ "input."
        else
            Msg = Msg0 ++ " but there are " ++
                string.int_to_string(NumExtraPolyTypes) ++
                " " ++ Extra ++ "inputs."
        )
    ).

:- func nth_specifier(int) = string.

nth_specifier(SpecNum) =
    "The " ++ nth(SpecNum) ++ " conversion specifier".

:- func nth(int) = string.

nth(N) = NStr :-
    ( if N = 1 then
        NStr = "first"
    else if N = 2 then
        NStr = "second"
    else if N = 3 then
        NStr = "third"
    else if N = 4 then
        NStr = "fourth"
    else if N = 5 then
        NStr = "fifth"
    else if N = 6 then
        NStr = "sixth"
    else if N = 7 then
        NStr = "seventh"
    else if N = 8 then
        NStr = "eighth"
    else if N = 9 then
        NStr = "ninth"
    else if N = 10 then
        NStr = "tenth"
    else
        NStr = string.int_to_string(N) ++ "th"
    ).

:- func specifier_char(char) = string.

specifier_char(SpecChar) =
    "specifier character `" ++ string.char_to_string(SpecChar) ++ "'".

:- func poly_kind_desc(poly_kind) = string.

poly_kind_desc(poly_kind_char) = "a character".
poly_kind_desc(poly_kind_str) = "a string".
poly_kind_desc(poly_kind_int) = "an integer".
poly_kind_desc(poly_kind_float) = "a float".

%---------------------------------------------------------------------------%

gather_non_percent_chars(Chars, NonConversionSpecChars, GatherEndedBy) :-
    (
        Chars = [HeadChar | TailChars],
        ( if HeadChar = '%' then
            NonConversionSpecChars = [],
            % We eat the percent sign.
            GatherEndedBy = found_percent(TailChars)
        else
            gather_non_percent_chars(TailChars, TailNonConversionSpecChars,
                GatherEndedBy),
            NonConversionSpecChars = [HeadChar | TailNonConversionSpecChars]
        )
    ;
        Chars = [],
        NonConversionSpecChars = [],
        GatherEndedBy = found_end_of_string
    ).

gather_flag_chars(!Chars, !Flags) :-
    % XXX Should we return an error if we find that the format string
    % sets the same flag twice?
    ( if
        !.Chars = [Char | !:Chars],
        ( Char = '#',   !Flags ^ flag_hash  := flag_hash_set
        ; Char = ' ',   !Flags ^ flag_space := flag_space_set
        ; Char = '0',   !Flags ^ flag_zero  := flag_zero_set
        ; Char = ('-'), !Flags ^ flag_minus := flag_minus_set
        ; Char = ('+'), !Flags ^ flag_plus  := flag_plus_set
        )
    then
        gather_flag_chars(!Chars, !Flags)
    else
        true
    ).

get_number_prefix(!Chars, N) :-
    get_number_prefix_loop(!Chars, 0, N).

get_nonzero_number_prefix(!Chars, N) :-
    !.Chars = [Char | !:Chars],
    char.decimal_digit_to_int(Char, CharValue),
    Char \= '0',
    get_number_prefix_loop(!Chars, CharValue, N).

:- pred get_number_prefix_loop(list(char)::in, list(char)::out,
    int::in, int::out) is det.

get_number_prefix_loop(!Chars, N0, N) :-
    ( if
        !.Chars = [Char | !:Chars],
        char.decimal_digit_to_int(Char, CharValue)
    then
        N1 = N0 * 10 + CharValue,
        get_number_prefix_loop(!Chars, N1, N)
    else
        N = N0
    ).

%---------------------------------------------------------------------------%
