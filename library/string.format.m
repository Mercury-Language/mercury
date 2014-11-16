%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Copyright (C) 2014 The Mercury team.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: string.format.m.
%
% This module implements string.format.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module string.format.
:- interface.

:- import_module list.

%-----------------------------------------------------------------------------%

:- pred format_impl(string::in, list(poly_type)::in, string::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module float.
:- import_module int.
:- import_module integer.
:- import_module maybe.
:- import_module require.

%-----------------------------------------------------------------------------%

:- pragma foreign_decl("C",
"
#include ""mercury_float.h""    /* for MR_float_to_string */

/*
** The following macro should expand to MR_TRUE if the C grades should
** implement string.format using C's sprintf function.
** Setting it to MR_FALSE will cause string.format to use the Mercury
** implementation of string formatting in C grades.
*/

#define ML_USE_SPRINTF MR_TRUE
").

%---------------------------------------------------------------------------%

:- type flag_hash
    --->    flag_hash_clear
    ;       flag_hash_set.

:- type flag_space
    --->    flag_space_clear
    ;       flag_space_set.

:- type flag_zero
    --->    flag_zero_clear
    ;       flag_zero_set.

:- type flag_minus
    --->    flag_minus_clear
    ;       flag_minus_set.

:- type flag_plus
    --->    flag_plus_clear
    ;       flag_plus_set.

:- type flags
    --->    flags(
                flag_hash       :: flag_hash,
                flag_space      :: flag_space,
                flag_zero       :: flag_zero,
                flag_minus      :: flag_minus,
                flag_plus       :: flag_plus
            ).

:- type maybe_width == maybe(int).
:- type maybe_prec == maybe(int).

:- type int_base
    --->    base_octal
    ;       base_decimal
    ;       base_hex_lc
    ;       base_hex_uc
    ;       base_hex_p.

:- type float_kind
    --->    kind_e_scientific_lc
    ;       kind_e_scientific_uc
    ;       kind_f_plain_lc
    ;       kind_f_plain_uc
    ;       kind_g_flexible_lc
    ;       kind_g_flexible_uc.

:- type format_str_spec
    --->    spec_percent
    ;       spec_signed_int(flags, maybe_width, maybe_prec, int)
    ;       spec_unsigned_int(flags, maybe_width, maybe_prec, int_base, int)
    ;       spec_float(flags, maybe_width, maybe_prec, float_kind, float)
    ;       spec_char(flags, maybe_width, char)
    ;       spec_string(flags, maybe_width, maybe_prec, string).

:- type format_str_component
    --->    comp_str(string)
    ;       comp_conv_spec(format_str_spec).

:- type poly_kind
    --->    poly_kind_char
    ;       poly_kind_str
    ;       poly_kind_int
    ;       poly_kind_float.

:- type format_str_error
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

%-----------------------------------------------------------------------------%

string.format.format_impl(FormatString, PolyList, String) :-
    % The call tree of predicate should be optimised to turn over
    % the least amount of memory possible, since memory usage is a significant
    % problem for programs which do a lot of formatted IO.
    %
    % XXX The repeated string appends performed in the call tree
    % do still allocate nontrivial amounts of memory for temporaries.
    Chars = to_char_list(FormatString),
    format_string_to_components(Chars, PolyList, 1, Components, Errors),
    (
        Errors = [],
        components_to_strings(Components, ComponentStrs),
        String = string.append_list(ComponentStrs)
    ;
        Errors = [HeadError | _],
        % In the common cases of missing or extra PolyTypes, all the errors
        % after the first may be avalanche errors, and would probably be more
        % confusing than helpful. This is why we traditionally print a message
        % only for the first one.
        %
        % XXX We should try printing messages for all the errors, not just
        % the first, and see whether the usefulness of the extra information
        % outweighs the costs of any extra confusion.
        Msg = format_str_error_to_msg(HeadError),
        error("string.format", Msg)
    ).

:- func format_str_error_to_msg(format_str_error) = string.

format_str_error_to_msg(Error) = Msg :-
    (
        Error = error_no_specifier(SpecNum, NumExtraPolyTypes),
        Msg0 = nth_specifier(SpecNum) ++ " is missing",
        ( NumExtraPolyTypes = 0 ->
            Msg = Msg0 ++ ", along with its input."
        ; NumExtraPolyTypes = 1 ->
            Msg = Msg0 ++ "."
        ;
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
        ( SpecNum = 1 ->
            % They aren't extra, since there is no other inputs before them.
            Extra = ""
        ;
            Extra = "extra "
        ),
        Msg0 = "There is no " ++ nth(SpecNum) ++ " conversion specifier,",
        ( NumExtraPolyTypes = 1 ->
            Msg = Msg0 ++ " but there is an " ++ Extra ++ "input."
        ;
            Msg = Msg0 ++ " but there are " ++
                string.int_to_string(NumExtraPolyTypes) ++ Extra ++ "inputs."
        )
    ).

:- func nth_specifier(int) = string.

nth_specifier(SpecNum) =
    "The " ++ nth(SpecNum) ++ " conversion specifier".

:- func nth(int) = string.

nth(N) = NStr :-
    ( N = 1 ->
        NStr = "first"
    ; N = 2 ->
        NStr = "second"
    ; N = 3 ->
        NStr = "third"
    ; N = 4 ->
        NStr = "fourth"
    ; N = 5 ->
        NStr = "fifth"
    ; N = 6 ->
        NStr = "sixth"
    ; N = 7 ->
        NStr = "seventh"
    ; N = 8 ->
        NStr = "eighth"
    ; N = 9 ->
        NStr = "ninth"
    ; N = 10 ->
        NStr = "tenth"
    ;
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

:- func poly_type_to_kind(poly_type) = poly_kind.

poly_type_to_kind(c(_)) = poly_kind_char.
poly_type_to_kind(s(_)) = poly_kind_str.
poly_type_to_kind(i(_)) = poly_kind_int.
poly_type_to_kind(f(_)) = poly_kind_float.

%-----------------------------------------------------------------------------%

    % This predicate parses as much of a format string as it can.
    % It stops parsing when it encounters something that looks like
    % a conversion specification (i.e. it starts with a '%' character),
    % but which cannot be parsed as one.
    %
    % Note that making this predicate use an accumulator for the lists
    % of components and errors seen so far would yield cleaner code,
    % but would probably be slower since our caller would have to unreverse
    % the list of components we return.
    %
    % The lack of tail recursion here should not be a problem, since no
    % format string will be long enough to make us consume too much stack.
    %
:- pred format_string_to_components(list(char)::in, list(string.poly_type)::in,
    int::in, list(format_str_component)::out, list(format_str_error)::out)
    is det.

format_string_to_components(!.Chars, !.PolyTypes, SpecNum,
        Components, Errors) :-
    gather_non_percent_chars(!.Chars, NonConversionSpecChars, GatherEndedBy),
    (
        GatherEndedBy = found_end_of_string,
        Components0 = [],
        (
            !.PolyTypes = [],
            Errors = []
        ;
            !.PolyTypes = [_ | _],
            Errors = [error_extra_polytypes(SpecNum, list.length(!.PolyTypes))]
        )
    ;
        GatherEndedBy = found_percent(!:Chars),
        parse_conversion_specification(!Chars, !PolyTypes, SpecNum,
            Spec, SpecErrors),
        format_string_to_components(!.Chars, !.PolyTypes, SpecNum + 1,
            ComponentsTail, ErrorsTail),
        (
            SpecErrors = [],
            ConvComponent = comp_conv_spec(Spec),
            Components0 = [ConvComponent | ComponentsTail],
            Errors = ErrorsTail
        ;
            SpecErrors = [_ | _],
            Components0 = ComponentsTail,
            Errors = SpecErrors ++ ErrorsTail
        )
    ),
    (
        NonConversionSpecChars = [],
        Components = Components0
    ;
        NonConversionSpecChars = [_ | _],
        NonConversionSpecString =
            string.from_char_list(NonConversionSpecChars),
        StringComponent = comp_str(NonConversionSpecString),
        Components = [StringComponent | Components0]
    ).

:- type gather_ended_by
    --->    found_end_of_string
    ;       found_percent(list(char)).  % The list of chars after the percent.

    % Parse a string which doesn't contain any conversion specifications.
    %
:- pred gather_non_percent_chars(list(char)::in, list(char)::out,
    gather_ended_by::out) is det.

gather_non_percent_chars(Chars, NonConversionSpecChars, GatherEndedBy) :-
    (
        Chars = [HeadChar | TailChars],
        ( HeadChar = '%' ->
            NonConversionSpecChars = [],
            % We eat the percent sign.
            GatherEndedBy = found_percent(TailChars)
        ;
            gather_non_percent_chars(TailChars, TailNonConversionSpecChars,
                GatherEndedBy),
            NonConversionSpecChars = [HeadChar | TailNonConversionSpecChars]
        )
    ;
        Chars = [],
        NonConversionSpecChars = [],
        GatherEndedBy = found_end_of_string
    ).

    % Each conversion specification is introduced by the character '%'
    % (which our caller has already read) and ends with a conversion specifier.
    % In between there may be (in this order) zero or more flags, an optional
    % minimum field width, and an optional precision.
    %
:- pred parse_conversion_specification(list(char)::in, list(char)::out,
    list(string.poly_type)::in, list(string.poly_type)::out, int::in,
    format_str_spec::out, list(format_str_error)::out) is det.

parse_conversion_specification(!Chars, !PolyTypes, SpecNum, Spec, Errors) :-
    Flags0 = flags(flag_hash_clear, flag_space_clear, flag_zero_clear,
        flag_minus_clear, flag_plus_clear),
    gather_flag_chars(!Chars, Flags0, Flags),
    get_optional_width(!Chars, !PolyTypes, SpecNum, MaybeWidth, WidthErrors),
    get_optional_prec(!Chars, !PolyTypes, SpecNum, MaybePrec, PrecErrors),
    get_first_spec(!Chars, !PolyTypes, Flags, MaybeWidth, MaybePrec, SpecNum,
        Spec, SpecErrors),
    Errors = WidthErrors ++ PrecErrors ++ SpecErrors.

    % Record and skip past any flag characters at the start of the char list.
    %
:- pred gather_flag_chars(list(char)::in, list(char)::out,
    flags::in, flags::out) is det.

gather_flag_chars(!Chars, !Flags) :-
    % XXX Should we return an error if we find that the format string
    % sets the same flag twice?
    (
        !.Chars = [Char | !:Chars],
        ( Char = '#',   !Flags ^ flag_hash  := flag_hash_set
        ; Char = ' ',   !Flags ^ flag_space := flag_space_set
        ; Char = '0',   !Flags ^ flag_zero  := flag_zero_set
        ; Char = ('-'), !Flags ^ flag_minus := flag_minus_set
        ; Char = ('+'), !Flags ^ flag_plus  := flag_plus_set
        )
    ->
        gather_flag_chars(!Chars, !Flags)
    ;
        true
    ).

    % Do we have a minimum field width? If yes, get it.
    %
:- pred get_optional_width(list(char)::in, list(char)::out,
    list(string.poly_type)::in, list(string.poly_type)::out, int::in,
    maybe_width::out, list(format_str_error)::out) is det.

get_optional_width(!Chars, !PolyTypes, SpecNum, MaybeWidth, Errors) :-
    ( !.Chars = ['*' | !:Chars] ->
        (
            !.PolyTypes = [PolyType | !:PolyTypes],
            ( PolyType = i(PolyWidth) ->
                MaybeWidth = yes(PolyWidth),
                Errors = []
            ;
                MaybeWidth = no,
                Errors = [error_nonint_star_width(SpecNum,
                    poly_type_to_kind(PolyType))]
            )
        ;
            !.PolyTypes = [],
            MaybeWidth = no,
            Errors = [error_missing_star_width(SpecNum)]
        )
    ; get_nonzero_number_prefix(!Chars, Width) ->
        MaybeWidth = yes(Width),
        Errors = []
    ;
        MaybeWidth = no,
        Errors = []
    ).

    % Do we have a precision? If yes, get it.
    %
:- pred get_optional_prec(list(char)::in, list(char)::out,
    list(string.poly_type)::in, list(string.poly_type)::out, int::in,
    maybe_prec::out, list(format_str_error)::out) is det.

get_optional_prec(!Chars, !PolyTypes, SpecNum, MaybePrec, Errors) :-
    ( !.Chars = ['.' | !:Chars] ->
        ( !.Chars = ['*' | !:Chars] ->
            (
                !.PolyTypes = [PolyType | !:PolyTypes],
                ( PolyType = i(PolyPrec) ->
                    MaybePrec = yes(PolyPrec),
                    Errors = []
                ;
                    MaybePrec = no,
                    Errors = [error_nonint_star_prec(SpecNum,
                        poly_type_to_kind(PolyType))]
                )
            ;
                !.PolyTypes = [],
                MaybePrec = no,
                Errors = [error_missing_star_prec(SpecNum)]
            )
        ;
            % This treats an empty string as an EXPLICIT zero.
            get_number_prefix(!Chars, 0, Prec),
            MaybePrec = yes(Prec),
            Errors = []
        )
    ;
        MaybePrec = no,
        Errors = []
    ).

:- pred get_nonzero_number_prefix(list(char)::in, list(char)::out,
    int::out) is semidet.

get_nonzero_number_prefix(!Chars, N) :-
    !.Chars = [Char | !:Chars],
    Char \= '0',
    char.decimal_digit_to_int(Char, CharValue),
    get_number_prefix(!Chars, CharValue, N).

:- pred get_number_prefix(list(char)::in, list(char)::out,
    int::in, int::out) is det.

get_number_prefix(!Chars, N0, N) :-
    (
        !.Chars = [Char | !:Chars],
        char.decimal_digit_to_int(Char, CharValue)
    ->
        N1 = N0 * 10 + CharValue,
        get_number_prefix(!Chars, N1, N)
    ;
        N = N0
    ).

%---------------------------------------------------------------------------%

    % get_first_spec(!Chars, !PolyTypes, Flags, MaybeWidth, MaybePrec,
    %   SpecNum, Spec, Errors):
    %
    % Try to read one conversion specifier, whose percent sign, flags,
    % width and precision have already been read, from !Chars.
    %
    % If successful, consume the corresponding poly_type from !PolyTypes,
    % we return the specifier as Spec and return an empty error list.
    %
    % If there is a problem, we return garbage Spec and a nonempty errors list.
    % We also consume the poly_type that corresponds (or at least, looks like
    % it corresponds) to the specifier, if there is one.
    %
:- pred get_first_spec(list(char)::in, list(char)::out,
    list(string.poly_type)::in, list(string.poly_type)::out,
    flags::in, maybe_width::in, maybe_prec::in, int::in,
    format_str_spec::out, list(format_str_error)::out) is det.

get_first_spec(!Chars, !PolyTypes, _Flags, _MaybeWidth, _MaybePrec, SpecNum,
        Spec, Errors) :-
    !.Chars = [],
    Spec = spec_percent,
    Errors = [error_no_specifier(SpecNum, list.length(!.PolyTypes))].
get_first_spec(!Chars, !PolyTypes, !.Flags, MaybeWidth, MaybePrec, SpecNum,
        Spec, Errors) :-
    !.Chars = [SpecChar | !:Chars],
    (
        (
            SpecChar = '%',
            SpecPrime = spec_percent,
            ErrorsPrime = []
        ;
            ( SpecChar = 'd'
            ; SpecChar = 'i'
            ),
            require_det
            (
                !.PolyTypes = [SpecPolyType | !:PolyTypes],
                ( SpecPolyType = i(Int) ->
                    % Base is always decimal
                    SpecPrime = spec_signed_int(!.Flags,
                        MaybeWidth, MaybePrec, Int),
                    ErrorsPrime = []
                ;
                    Error = error_wrong_polytype(SpecNum, SpecChar,
                        poly_type_to_kind(SpecPolyType)),
                    SpecPrime = spec_percent,
                    ErrorsPrime = [Error]
                )
            ;
                !.PolyTypes = [],
                Error = error_no_polytype(SpecNum, SpecChar),
                SpecPrime = spec_percent,
                ErrorsPrime = [Error]
            )
        ;
            (
                SpecChar = 'o',
                Base = base_octal
            ;
                SpecChar = 'u',
                Base = base_decimal
            ;
                SpecChar = 'x',
                Base = base_hex_lc
            ;
                SpecChar = 'X',
                Base = base_hex_uc
            ;
                SpecChar = 'p',
                Base = base_hex_p,
                % XXX This should not be necessary.
                !Flags ^ flag_hash := flag_hash_set
            ),
            require_det
            (
                !.PolyTypes = [SpecPolyType | !:PolyTypes],
                ( SpecPolyType = i(Int) ->
                    SpecPrime = spec_unsigned_int(!.Flags,
                        MaybeWidth, MaybePrec, Base, Int),
                    ErrorsPrime = []
                ;
                    Error = error_wrong_polytype(SpecNum, SpecChar,
                        poly_type_to_kind(SpecPolyType)),
                    SpecPrime = spec_percent,
                    ErrorsPrime = [Error]
                )
            ;
                !.PolyTypes = [],
                Error = error_no_polytype(SpecNum, SpecChar),
                SpecPrime = spec_percent,
                ErrorsPrime = [Error]
            )
        ;
            (
                SpecChar = 'e',
                FloatKind = kind_e_scientific_lc
            ;
                SpecChar = 'E',
                FloatKind = kind_e_scientific_uc
            ;
                SpecChar = 'f',
                FloatKind = kind_f_plain_lc
            ;
                SpecChar = 'F',
                FloatKind = kind_f_plain_uc
            ;
                SpecChar = 'g',
                FloatKind = kind_g_flexible_lc
            ;
                SpecChar = 'G',
                FloatKind = kind_g_flexible_uc
            ),
            require_det
            (
                !.PolyTypes = [SpecPolyType | !:PolyTypes],
                ( SpecPolyType = f(Float) ->
                    SpecPrime = spec_float(!.Flags,
                        MaybeWidth, MaybePrec, FloatKind, Float),
                    ErrorsPrime = []
                ;
                    Error = error_wrong_polytype(SpecNum, SpecChar,
                        poly_type_to_kind(SpecPolyType)),
                    SpecPrime = spec_percent,
                    ErrorsPrime = [Error]
                )
            ;
                !.PolyTypes = [],
                Error = error_no_polytype(SpecNum, SpecChar),
                SpecPrime = spec_percent,
                ErrorsPrime = [Error]
            )
        ;
            SpecChar = 'c',
            require_det
            (
                !.PolyTypes = [SpecPolyType | !:PolyTypes],
                ( SpecPolyType = c(Char) ->
                    % XXX Should we generate an error if MaybePrec = yes(...)?
                    SpecPrime = spec_char(!.Flags, MaybeWidth, Char),
                    ErrorsPrime = []
                ;
                    Error = error_wrong_polytype(SpecNum, SpecChar,
                        poly_type_to_kind(SpecPolyType)),
                    SpecPrime = spec_percent,
                    ErrorsPrime = [Error]
                )
            ;
                !.PolyTypes = [],
                Error = error_no_polytype(SpecNum, SpecChar),
                SpecPrime = spec_percent,
                ErrorsPrime = [Error]
            )
        ;
            SpecChar = 's',
            require_det
            (
                !.PolyTypes = [SpecPolyType | !:PolyTypes],
                ( SpecPolyType = s(Str) ->
                    SpecPrime = spec_string(!.Flags,
                        MaybeWidth, MaybePrec, Str),
                    ErrorsPrime = []
                ;
                    Error = error_wrong_polytype(SpecNum, SpecChar,
                        poly_type_to_kind(SpecPolyType)),
                    SpecPrime = spec_percent,
                    ErrorsPrime = [Error]
                )
            ;
                !.PolyTypes = [],
                Error = error_no_polytype(SpecNum, SpecChar),
                SpecPrime = spec_percent,
                ErrorsPrime = [Error]
            )
        )
    ->
        Spec = SpecPrime,
        Errors = ErrorsPrime
    ;
        Error = error_unknown_specifier(SpecNum, SpecChar),
        Spec = spec_percent,
        Errors = [Error]
    ).

:- pred components_to_strings(list(format_str_component)::in,
    list(string)::out) is det.

components_to_strings([], []).
components_to_strings([Component | Components], [String | Strings]) :-
    component_to_string(Component, String),
    components_to_strings(Components, Strings).

:- pred component_to_string(format_str_component::in, string::out) is det.

component_to_string(Component, String) :-
    Component = comp_str(String).
component_to_string(Component, String) :-
    Component = comp_conv_spec(Spec),
    (
        % Conversion specifier representing the "%" sign.
        Spec = spec_percent,
        String = "%"
    ;
        % Signed int conversion specifiers.
        Spec = spec_signed_int(Flags, MaybeWidth, MaybePrec, Int),
        ( using_sprintf ->
            % XXX The "d" could be "i"; we don't keep track.
            FormatStr = make_format(Flags, MaybeWidth, MaybePrec,
                int_length_modifer, "d"),
            String = native_format_int(FormatStr, Int)
        ;
            String = format_int(Flags, MaybeWidth, MaybePrec, Int)
        )
    ;
        % Unsigned int conversion specifiers.
        Spec = spec_unsigned_int(Flags, MaybeWidth, MaybePrec, Base, Int),
        ( using_sprintf ->
            ( Base = base_octal,   SpecChar = "o"
            ; Base = base_decimal, SpecChar = "u"
            ; Base = base_hex_lc,  SpecChar = "x"
            ; Base = base_hex_uc,  SpecChar = "X"
            ; Base = base_hex_p,   SpecChar = "p"
            ),
            FormatStr = make_format(Flags, MaybeWidth, MaybePrec,
                int_length_modifer, SpecChar),
            String = native_format_int(FormatStr, Int)
        ;
            String = format_unsigned_int(Flags, MaybeWidth, MaybePrec,
                Base, Int)
        )
    ;
        % Float conversion specifiers.
        Spec = spec_float(Flags, MaybeWidth, MaybePrec, Kind, Float),
        (
            is_finite(Float),
            using_sprintf
        ->
            ( Kind = kind_e_scientific_lc, SpecChar = "e"
            ; Kind = kind_e_scientific_uc, SpecChar = "E"
            ; Kind = kind_f_plain_lc,      SpecChar = "f"
            ; Kind = kind_f_plain_uc,      SpecChar = "F"
            ; Kind = kind_g_flexible_lc,   SpecChar = "g"
            ; Kind = kind_g_flexible_uc,   SpecChar = "G"
            ),
            FormatStr = make_format(Flags, MaybeWidth, MaybePrec,
                "", SpecChar),
            String = native_format_float(FormatStr, Float)
        ;
            String = format_float(Flags, MaybeWidth, MaybePrec, Kind, Float)
        )
    ;
        % Char conversion specifiers.
        Spec = spec_char(Flags, MaybeWidth, Char),
        ( using_sprintf_for_char(Char) ->
            FormatStr = make_format(Flags, MaybeWidth, no, "", "c"),
            String = native_format_char(FormatStr, Char)
        ;
            String = format_char(Flags, MaybeWidth, Char)
        )
    ;
        % String conversion specifiers.
        Spec = spec_string(Flags, MaybeWidth, MaybePrec, Str),
        (
            (
                using_sprintf,
                Flags = flags(flag_hash_clear, flag_space_clear,
                    flag_zero_clear, flag_minus_clear, flag_plus_clear),
                MaybeWidth = no,
                MaybePrec = no
            ;
                using_sprintf_for_string(Str)
            )
        ->
            FormatStr = make_format(Flags, MaybeWidth, MaybePrec, "", "s"),
            String = native_format_string(FormatStr, Str)
        ;
            String = format_string(Flags, MaybeWidth, MaybePrec, Str)
        )
    ).

%-----------------------------------------------------------------------------%

    % Construct a format string.
    %
:- func make_format(flags, maybe_width, maybe_prec, string, string) = string.

make_format(Flags, MaybeWidth, MaybePrec, LengthMod, Spec) =
    ( using_sprintf ->
        make_format_sprintf(Flags, MaybeWidth, MaybePrec, LengthMod, Spec)
    ;
        make_format_dotnet(Flags, MaybeWidth, MaybePrec, LengthMod, Spec)
    ).

    % Are we using C's sprintf? All backends other than C return false.
    % Note that any backends which return true for using_sprintf/0 must
    % also implement:
    %
    %   int_length_modifer/0
    %   native_format_float/2
    %   native_format_int/2
    %   native_format_string/2
    %   native_format_char/2
    %
:- pred using_sprintf is semidet.

:- pragma foreign_proc("C", using_sprintf,
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"
    SUCCESS_INDICATOR = ML_USE_SPRINTF;
").
:- pragma foreign_proc("C#", using_sprintf,
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = false;
").
:- pragma foreign_proc("Java", using_sprintf,
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = false;
").
:- pragma foreign_proc("Erlang", using_sprintf,
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = false
").

:- pred using_sprintf_for_char(char::in) is semidet.

using_sprintf_for_char(_) :-
    semidet_fail.

:- pragma foreign_proc("C",
    using_sprintf_for_char(Char::in),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"
    /* sprintf %c specifier is inadequate for multi-byte UTF-8 characters. */
    SUCCESS_INDICATOR = ML_USE_SPRINTF && MR_is_ascii(Char);
").

:- pred using_sprintf_for_string(string::in) is semidet.

using_sprintf_for_string(_) :-
    semidet_fail.

:- pragma foreign_proc("C",
    using_sprintf_for_string(Str::in),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"
    const char *s;

    SUCCESS_INDICATOR = ML_USE_SPRINTF;
    for (s = Str; *s != '\\0'; s++) {
        /*
         * sprintf %s specifier is inadequate for multi-byte UTF-8 characters,
         * if there is a field width or precision specified.
         */
        if (!MR_utf8_is_single_byte(*s)) {
            SUCCESS_INDICATOR = MR_FALSE;
            break;
        }
    }
").

    % Construct a format string suitable to passing to sprintf.
    %
:- func make_format_sprintf(flags, maybe_width, maybe_prec, string,
    string) = string.

make_format_sprintf(Flags, MaybeWidth, MaybePrec, LengthMod, Spec) = String :-
    Flags = flags(FlagHash, FlagSpace, FlagZero, FlagMinus, FlagPlus),
    ( FlagHash  = flag_hash_clear,  FlagHashStr  = ""
    ; FlagHash  = flag_hash_set,    FlagHashStr  = "#"
    ),
    ( FlagSpace = flag_space_clear, FlagSpaceStr = ""
    ; FlagSpace = flag_space_set,   FlagSpaceStr = " "
    ),
    ( FlagZero  = flag_zero_clear,  FlagZeroStr  = ""
    ; FlagZero  = flag_zero_set,    FlagZeroStr  = "0"
    ),
    ( FlagMinus = flag_minus_clear, FlagMinusStr = ""
    ; FlagMinus = flag_minus_set,   FlagMinusStr = "-"
    ),
    ( FlagPlus  = flag_plus_clear,  FlagPlusStr  = ""
    ; FlagPlus  = flag_plus_set,    FlagPlusStr  = "+"
    ),
    (
        MaybeWidth = yes(Width),
        WidthStr = int_to_string(Width)
    ;
        MaybeWidth = no,
        WidthStr = ""
    ),
    (
        MaybePrec = yes(Prec),
        PrecPrefixStr = ".",
        PrecStr = int_to_string(Prec)
    ;
        MaybePrec = no,
        PrecPrefixStr = "",
        PrecStr = ""
    ),
    String = string.append_list(["%",
        FlagHashStr, FlagSpaceStr, FlagZeroStr, FlagMinusStr, FlagPlusStr,
        WidthStr, PrecPrefixStr, PrecStr, LengthMod, Spec]).

    % Construct a format string suitable to passing to .NET's formatting
    % functions.
    % XXX this code is not yet complete. We need to do a lot more work
    % to make this work perfectly.
    %
:- func make_format_dotnet(flags, maybe_width, maybe_prec, string,
    string) = string.

make_format_dotnet(_Flags, MaybeWidth, MaybePrec, _LengthMod, Spec0)
        = String :-
    (
        MaybeWidth = yes(Width),
        WidthPrefixStr = ",",
        WidthStr = int_to_string(Width)
    ;
        MaybeWidth = no,
        WidthPrefixStr = "",
        WidthStr = ""
    ),
    (
        MaybePrec = yes(Prec),
        PrecStr = int_to_string(Prec)
    ;
        MaybePrec = no,
        PrecStr = ""
    ),
    ( Spec0 = "i" ->
        Spec = "d"
    ; Spec0 = "f" ->
        Spec = "e"
    ;
        Spec = Spec0
    ),
    String = string.append_list([
        "{0",
        WidthPrefixStr,
        WidthStr,
        ":",
        Spec,
        PrecStr,
%       LengthMod,
%       from_char_list(Flags),
        "}"]).

:- func int_length_modifer = string.

:- pragma foreign_proc("C",
    int_length_modifer = (LengthModifier::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"{
    MR_make_aligned_string(LengthModifier, MR_INTEGER_LENGTH_MODIFIER);
}").

int_length_modifer = _ :-
    % This predicate is only called if using_sprintf/0, so we produce an error
    % by default.
    error("string.int_length_modifer/0 not defined").

    % Create a string from a float using the format string.
    % Note it is the responsibility of the caller to ensure that the
    % format string is valid.
    %
:- func native_format_float(string, float) = string.

:- pragma foreign_proc("C",
    native_format_float(FormatStr::in, Val::in) = (Str::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"{
    MR_save_transient_hp();
    Str = MR_make_string(MR_ALLOC_ID, FormatStr, (double) Val);
    MR_restore_transient_hp();
}").
native_format_float(_, _) = _ :-
    % This predicate is only called if using_sprintf/0, so we produce an error
    % by default.
    error("string.native_format_float/2 not defined").

    % Create a string from a int using the format string.
    % Note it is the responsibility of the caller to ensure that the
    % format string is valid.
    %
:- func native_format_int(string, int) = string.

:- pragma foreign_proc("C",
    native_format_int(FormatStr::in, Val::in) = (Str::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"{
    MR_save_transient_hp();
    Str = MR_make_string(MR_ALLOC_ID, FormatStr, Val);
    MR_restore_transient_hp();
}").
native_format_int(_, _) = _ :-
    % This predicate is only called if using_sprintf/0, so we produce an error
    % by default.
    error("string.native_format_int/2 not defined").

    % Create a string from a string using the format string.
    % Note it is the responsibility of the caller to ensure that the
    % format string is valid.
    %
:- func native_format_string(string, string) = string.

:- pragma foreign_proc("C",
    native_format_string(FormatStr::in, Val::in) = (Str::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"{
    MR_save_transient_hp();
    Str = MR_make_string(MR_ALLOC_ID, FormatStr, Val);
    MR_restore_transient_hp();
}").
native_format_string(_, _) = _ :-
    % This predicate is only called if using_sprintf/0, so we produce an error
    % by default.
    error("string.native_format_string/2 not defined").

    % Create a string from a char using the format string.
    % Note it is the responsibility of the caller to ensure that the
    % format string is valid.
    %
:- func native_format_char(string, char) = string.

:- pragma foreign_proc("C",
    native_format_char(FormatStr::in, Val::in) = (Str::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"{
    MR_save_transient_hp();
    Str = MR_make_string(MR_ALLOC_ID, FormatStr, Val);
    MR_restore_transient_hp();
}").
native_format_char(_, _) = _ :-
    % This predicate is only called if using_sprintf/0, so we produce an error
    % by default.
    error("string.native_format_char/2 not defined").

%-----------------------------------------------------------------------------%

    % Format a character.
    %
:- func format_char(flags, maybe_width, char) = string.

format_char(Flags, MaybeWidth, Char) = String :-
    CharStr = string.char_to_string(Char),
    String = justify_string(Flags, MaybeWidth, CharStr).

    % Format a string.
    %
:- func format_string(flags, maybe_width, maybe_prec, string) = string.

format_string(Flags, MaybeWidth, MaybePrec, OldStr) = NewStr :-
    (
        MaybePrec = yes(NumChars),
        PrecStr = string.left_by_codepoint(OldStr, NumChars)
    ;
        MaybePrec = no,
        PrecStr = OldStr
    ),
    NewStr = justify_string(Flags, MaybeWidth, PrecStr).

:- func format_int(flags, maybe_width, maybe_prec, int) = string.

format_int(Flags, MaybeWidth, MaybePrec, Int) = String :-
    ( Int = 0 ->
        % Zero is a special case. The abs_integer_to_decimal function
        % returns "" for 0, but returning no digits at all is ok
        % only if our caller explicitly allowed us to do so.
        ( MaybePrec = yes(0) ->
            AbsIntStr = ""
        ;
            AbsIntStr = "0"
        )
    ;
        % If the platform we are running on can't represent the absolute
        % value of a 16 bit signed number natively, we are in big trouble.
        % (The "absolute value of" part is why the test below excludes
        % -32768.)
        ( -32767 =< Int, Int =< 32767 ->
            AbsInt = int.abs(Int),
            AbsIntStr = abs_int_to_decimal(AbsInt)
        ;
            AbsInteger = integer.abs(integer(Int)),
            AbsIntStr = abs_integer_to_decimal(AbsInteger)
        )
    ),
    AbsIntStrLength = string.count_codepoints(AbsIntStr),

    % Do we need to increase precision?
    (
        MaybePrec = yes(Prec),
        Prec > AbsIntStrLength
    ->
        PrecStr = string.pad_left(AbsIntStr, '0', Prec)
    ;
        PrecStr = AbsIntStr
    ),

    % Do we need to pad to the field width?
    (
        MaybeWidth = yes(Width),
        Width > string.count_codepoints(PrecStr),
        Flags ^ flag_zero = flag_zero_set,
        Flags ^ flag_minus = flag_minus_clear,
        MaybePrec = no
    ->
        FieldStr = string.pad_left(PrecStr, '0', Width - 1),
        ZeroPadded = yes
    ;
        FieldStr = PrecStr,
        ZeroPadded = no
    ),

    % Prefix with appropriate sign or zero padding.
    % The previous step has deliberately left room for this.
    SignedStr = add_sign_like_prefix_to_int_if_needed(Flags, ZeroPadded, Int,
        FieldStr),
    String = justify_string(Flags, MaybeWidth, SignedStr).

    % Format an unsigned int, unsigned octal, or unsigned hexadecimal
    % (u,o,x,X,p).
    %
:- func format_unsigned_int(flags, maybe_width, maybe_prec, int_base, int)
    = string.

format_unsigned_int(Flags, MaybeWidth, MaybePrec, Base, Int) = String :-
    ( Int = 0 ->
        % Zero is a special case. The abs_integer_to_decimal function
        % returns "" for 0, but returning no digits at all is ok
        % only if our caller explicitly allowed us to do so.
        ( MaybePrec = yes(0) ->
            AbsIntStr = ""
        ;
            AbsIntStr = "0"
        )
    ;
        % If the platform we are running on can't represent the absolute
        % value of a 16 bit signed number natively, we are in big trouble.
        %
        % Our caller wants us to treat Int as unsigned, but Mercury treats it
        % as signed. We use native arithmetic on ints (as opposed to arbitrary
        % precision arithmetic on integers) on Int only in cases where
        % the two notions coincide, i.e. if we know that Int is positive
        % even when viewed as a signed number, and that is so even on
        % 16 bit machines.
        ( 0 =< Int, Int =< 32767 ->
            (
                Base = base_octal,
                AbsIntStr = abs_int_to_octal(Int)
            ;
                Base = base_decimal,
                AbsIntStr = abs_int_to_decimal(Int)
            ;
                ( Base = base_hex_lc
                ; Base = base_hex_p
                ),
                AbsIntStr = abs_int_to_hex_lc(Int)
            ;
                Base = base_hex_uc,
                AbsIntStr = abs_int_to_hex_uc(Int)
            )
        ;
            Div = integer.pow(integer(2), integer(int.bits_per_int)),
            UnsignedInteger = integer(Int) mod Div,
            (
                Base = base_octal,
                AbsIntStr = abs_integer_to_octal(UnsignedInteger)
            ;
                Base = base_decimal,
                AbsIntStr = abs_integer_to_decimal(UnsignedInteger)
            ;
                ( Base = base_hex_lc
                ; Base = base_hex_p
                ),
                AbsIntStr = abs_integer_to_hex_lc(UnsignedInteger)
            ;
                Base = base_hex_uc,
                AbsIntStr = abs_integer_to_hex_uc(UnsignedInteger)
            )
        )
    ),
    AbsIntStrLength = string.count_codepoints(AbsIntStr),

    % Do we need to increase precision?
    (
        MaybePrec = yes(Prec),
        Prec > AbsIntStrLength
    ->
        PrecStr = string.pad_left(AbsIntStr, '0', Prec)
    ;
        PrecStr = AbsIntStr
    ),

    % Do we need to increase the precision of an octal?
    (
        Base = base_octal,
        Flags ^ flag_hash = flag_hash_set,
        \+ string.prefix(PrecStr, "0")
    ->
        PrecModStr = append("0", PrecStr)
    ;
        PrecModStr = PrecStr
    ),

    % Do we need to pad to the field width?
    (
        MaybeWidth = yes(Width),
        Width > string.count_codepoints(PrecModStr),
        Flags ^ flag_zero = flag_zero_set,
        Flags ^ flag_minus = flag_minus_clear,
        MaybePrec = no
    ->
        % Do we need to make room for "0x" or "0X" ?
        (
            Flags ^ flag_hash = flag_hash_set,
            (
                Base = base_hex_p,
                Prefix = "0x"
            ;
                Base = base_hex_lc,
                Int \= 0,
                Prefix = "0x"
            ;
                Base = base_hex_uc,
                Int \= 0,
                Prefix = "0X"
            )
        ->
            FieldStr = string.pad_left(PrecModStr, '0', Width - 2),
            FieldModStr = Prefix ++ FieldStr
        ;
            FieldStr = string.pad_left(PrecModStr, '0', Width),
            FieldModStr = FieldStr
        )
    ;
        FieldStr = PrecModStr,
        % Do we have to prefix "0x" or "0X"?
        (
            Flags ^ flag_hash = flag_hash_set,
            (
                Base = base_hex_p,
                Prefix = "0x"
            ;
                Base = base_hex_lc,
                Int \= 0,
                Prefix = "0x"
            ;
                Base = base_hex_uc,
                Int \= 0,
                Prefix = "0X"
            )
        ->
            FieldModStr = Prefix ++ FieldStr
        ;
            FieldModStr = FieldStr
        )
    ),

    String = justify_string(Flags, MaybeWidth, FieldModStr).

%-----------------------------------------------------------------------------%

    % Format a float.
    %
:- func format_float(flags, maybe_width, maybe_prec, float_kind, float)
    = string.

format_float(Flags, MaybeWidth, MaybePrec, Kind, Float) = String :-
    ( is_nan(Float) ->
        (
            ( Kind = kind_e_scientific_lc
            ; Kind = kind_f_plain_lc
            ; Kind = kind_g_flexible_lc
            ),
            SignedStr = "nan"
        ;
            ( Kind = kind_e_scientific_uc
            ; Kind = kind_f_plain_uc
            ; Kind = kind_g_flexible_uc
            ),
            SignedStr = "NAN"
        )
    ; is_infinite(Float) ->
        (
            ( Kind = kind_e_scientific_lc
            ; Kind = kind_f_plain_lc
            ; Kind = kind_g_flexible_lc
            ),
            SignedStr = ( if Float < 0.0 then "-infinity" else "infinity" )
        ;
            ( Kind = kind_e_scientific_uc
            ; Kind = kind_f_plain_uc
            ; Kind = kind_g_flexible_uc
            ),
            SignedStr = ( if Float < 0.0 then "-INFINITY" else "INFINITY" )
        )
    ;
        % XXX This general approach of converting the float to a string
        % using convert_float_to_string and then post-processing it
        % is far from ideal, since it is significantly less efficient
        % than building the right string directly.
        AbsFloat = abs(Float),
        AbsStr = convert_float_to_string(AbsFloat),

        % Change precision if needed.
        (
            ( Kind = kind_e_scientific_lc, E = "e"
            ; Kind = kind_e_scientific_uc, E = "E"
            ),
            Prec = get_prec_to_use(MaybePrec),
            PrecStr = change_to_e_notation(AbsStr, Prec, E),

            % Do we need to remove the decimal point?
            (
                Flags ^ flag_hash = flag_hash_clear,
                MaybePrec = yes(0)
            ->
                split_at_decimal_point(PrecStr, BaseStr, ExponentStr),
                PrecModStr = BaseStr ++ ExponentStr
            ;
                PrecModStr = PrecStr
            )
        ;
            ( Kind = kind_f_plain_lc
            ; Kind = kind_f_plain_uc
            ),
            Prec = get_prec_to_use(MaybePrec),
            PrecStr = change_precision(AbsStr, Prec),

            % Do we need to remove the decimal point?
            (
                Flags ^ flag_hash = flag_hash_clear,
                MaybePrec = yes(0)
            ->
                PrecStrLen = string.count_codepoints(PrecStr),
                PrecModStr = string.between(PrecStr, 0, PrecStrLen - 1)
            ;
                PrecModStr = PrecStr
            )
        ;
            ( Kind = kind_g_flexible_lc, E = "e"
            ; Kind = kind_g_flexible_uc, E = "E"
            ),
            Prec = get_prec_to_use_minimum_1(MaybePrec),
            PrecStr = change_to_g_notation(AbsStr, Prec, E, Flags),
            % Don't ever remove the decimal point.
            % XXX Why? Does change_to_g_notation do it?
            PrecModStr = PrecStr
        ),

        % Do we need to change field width?
        (
            MaybeWidth = yes(Width),
            Width > string.count_codepoints(PrecModStr),
            Flags ^ flag_zero = flag_zero_set,
            Flags ^ flag_minus = flag_minus_clear
        ->
            FieldStr = string.pad_left(PrecModStr, '0', Width - 1),
            ZeroPadded = yes
        ;
            FieldStr = PrecModStr,
            ZeroPadded = no
        ),

        % Finishing up.
        SignedStr = add_sign_like_prefix_to_float_if_needed(Flags, ZeroPadded,
            Float, FieldStr)
    ),
    String = justify_string(Flags, MaybeWidth, SignedStr).

:- func get_prec_to_use(maybe_prec) = int.
:- pragma inline(get_prec_to_use/1).

get_prec_to_use(MaybePrec) = Prec :-
    (
        MaybePrec = yes(Prec)
    ;
        MaybePrec = no,
        % The default precision is 6.
        Prec = 6
    ).

:- func get_prec_to_use_minimum_1(maybe_prec) = int.
:- pragma inline(get_prec_to_use_minimum_1/1).

get_prec_to_use_minimum_1(MaybePrec) = Prec :-
    (
        MaybePrec = yes(Prec0),
        ( Prec0 = 0 ->
            Prec = 1
        ;
            Prec = Prec0
        )
    ;
        MaybePrec = no,
        % The default precision is 6.
        Prec = 6
    ).

%---------------------------------------------------------------------------%

:- func add_sign_like_prefix_to_int_if_needed(flags, bool, int, string)
    = string.
:- pragma inline(add_sign_like_prefix_to_int_if_needed/4).

add_sign_like_prefix_to_int_if_needed(Flags, ZeroPadded, Int, FieldStr)
        = SignedStr :-
    ( Int < 0 ->
        SignedStr = "-" ++ FieldStr
    ; Flags ^ flag_plus = flag_plus_set ->
        SignedStr = "+" ++ FieldStr
    ; Flags ^ flag_space = flag_space_set ->
        SignedStr = " " ++ FieldStr
    ;
        (
            ZeroPadded = yes,
            SignedStr = "0" ++ FieldStr
        ;
            ZeroPadded = no,
            SignedStr = FieldStr
        )
    ).

:- func add_sign_like_prefix_to_float_if_needed(flags, bool, float, string)
    = string.
:- pragma inline(add_sign_like_prefix_to_float_if_needed/4).

add_sign_like_prefix_to_float_if_needed(Flags, ZeroPadded, Float, FieldStr)
        = SignedStr :-
    % XXX Float < 0.0 is the wrong test, because it fails for -0.0.
    % We should test the sign bit instead. This can be done using
    % signbit(Float) in C, but I (zs) don't know its equivalents
    % for the other backends.
    ( Float < 0.0 ->
        SignedStr = "-" ++ FieldStr
    ; Flags ^ flag_plus = flag_plus_set ->
        SignedStr = "+" ++ FieldStr
    ; Flags ^ flag_space = flag_space_set ->
        SignedStr = " " ++ FieldStr
    ;
        (
            ZeroPadded = yes,
            SignedStr = "0" ++ FieldStr
        ;
            ZeroPadded = no,
            SignedStr = FieldStr
        )
    ).

:- func justify_string(flags, maybe_width, string) = string.

justify_string(Flags, MaybeWidth, Str) = JustifiedStr :-
    (
        MaybeWidth = yes(Width),
        Width > string.count_codepoints(Str)
    ->
        ( Flags ^ flag_minus = flag_minus_set ->
            string.pad_right(Str, ' ', Width, JustifiedStr)
        ;
            string.pad_left(Str, ' ', Width, JustifiedStr)
        )
    ;
        JustifiedStr = Str
    ).

%---------------------------------------------------------------------------%
%
% Each of these functions converts a non-negative integer (that originally
% came from a Mercury int) to a string of octal, decimal or hex digits.
%
% The input is an arbitrary precision integer because if either
%
% - the original number is a signed int, and its value is minint, or
% - the original number is an unsigned int, and its value has the most
%   significant bit set,
%
% then the absolute value of that number cannot be represented as
% a Mercury int, which is always signed and always word-sized. However,
% once we have divided the original integer by 8, 10 or 16, the result
% is guaranteed not to suffer from either of the problems above,
% so we process it as an Mercury int, which is a lot faster.

    % Convert a non-negative integer to an octal string.
    %
:- func abs_integer_to_octal(integer) = string.
:- func abs_int_to_octal(int) = string.

abs_integer_to_octal(Num) = NumStr :-
    ( Num > integer(0) ->
        Integer8 = integer(8),
        Rest = abs_int_to_octal(int(Num // Integer8)),
        Rem = Num rem Integer8,
        RemStr = get_octal_digit(int(Rem)),
        NumStr = append(Rest, RemStr)
    ;
        NumStr = ""
    ).

abs_int_to_octal(Num) = NumStr :-
    ( Num > 0 ->
        Rest = abs_int_to_octal(Num // 8),
        Rem = Num rem 8,
        RemStr = get_octal_digit(Rem),
        NumStr = append(Rest, RemStr)
    ;
        NumStr = ""
    ).

    % Convert a non-negative integer to a decimal string.
    %
:- func abs_integer_to_decimal(integer) = string.
:- func abs_int_to_decimal(int) = string.

abs_integer_to_decimal(Num) = NumStr :-
    ( Num > integer(0) ->
        Integer10 = integer(10),
        Rest = abs_int_to_decimal(int(Num // Integer10)),
        Rem = Num rem Integer10,
        RemStr = get_decimal_digit(int(Rem)),
        NumStr = append(Rest, RemStr)
    ;
        NumStr = ""
    ).

abs_int_to_decimal(Num) = NumStr :-
    ( Num > 0 ->
        Rest = abs_int_to_decimal(Num // 10),
        Rem = Num rem 10,
        RemStr = get_decimal_digit(Rem),
        NumStr = append(Rest, RemStr)
    ;
        NumStr = ""
    ).

    % Convert a non-negative integer to a hexadecimal string,
    % using a-f for to_hex_lc and A-F for to_hex_uc.
    %
:- func abs_integer_to_hex_lc(integer) = string.
:- func abs_integer_to_hex_uc(integer) = string.
:- func abs_int_to_hex_lc(int) = string.
:- func abs_int_to_hex_uc(int) = string.

abs_integer_to_hex_lc(Num) = NumStr :-
    ( Num > integer(0) ->
        Integer16 = integer(16),
        RestStr = abs_int_to_hex_lc(int(Num // Integer16)),
        Rem = Num rem Integer16,
        RemStr = get_hex_digit_lc(int(Rem)),
        NumStr = append(RestStr, RemStr)
    ;
        NumStr = ""
    ).

abs_integer_to_hex_uc(Num) = NumStr :-
    ( Num > integer(0) ->
        Integer16 = integer(16),
        RestStr = abs_int_to_hex_uc(int(Num // Integer16)),
        Rem = Num rem Integer16,
        RemStr = get_hex_digit_uc(int(Rem)),
        NumStr = append(RestStr, RemStr)
    ;
        NumStr = ""
    ).

abs_int_to_hex_lc(Num) = NumStr :-
    ( Num > 0 ->
        RestStr = abs_int_to_hex_lc(Num // 16),
        Rem = Num rem 16,
        RemStr = get_hex_digit_lc(Rem),
        NumStr = append(RestStr, RemStr)
    ;
        NumStr = ""
    ).

abs_int_to_hex_uc(Num) = NumStr :-
    ( Num > 0 ->
        RestStr = abs_int_to_hex_uc(Num // 16),
        Rem = Num rem 16,
        RemStr = get_hex_digit_uc(Rem),
        NumStr = append(RestStr, RemStr)
    ;
        NumStr = ""
    ).

%-----------------------------------------------------------------------------%

    % Given an int between 0 and 7, return the octal digit representing it.
    %
:- func get_octal_digit(int) = string.
:- pragma inline(get_octal_digit/1).

get_octal_digit(Int) = Octal :-
    ( octal_digit(Int, OctalPrime) ->
        Octal = OctalPrime
    ;
        unexpected($module, $pred, "octal_digit failed")
    ).

    % Given an int between 0 and 9, return the decimal digit representing it.
    %
:- func get_decimal_digit(int) = string.
:- pragma inline(get_decimal_digit/1).

get_decimal_digit(Int) = Decimal :-
    ( decimal_digit(Int, DecimalPrime) ->
        Decimal = DecimalPrime
    ;
        unexpected($module, $pred, "decimal_digit failed")
    ).

    % Given an int between 0 and 15, return the hexadecimal digit
    % representing it, using a-f for get_hex_digit_lc and
    % A-F for get_hex_digit_uc.
    %
:- func get_hex_digit_lc(int) = string.
:- func get_hex_digit_uc(int) = string.
:- pragma inline(get_hex_digit_lc/1).
:- pragma inline(get_hex_digit_uc/1).

get_hex_digit_lc(Int) = HexLC :-
    ( hex_digit(Int, HexLCPrime, _HexUC) ->
        HexLC = HexLCPrime
    ;
        unexpected($module, $pred, "hex_digit failed")
    ).

get_hex_digit_uc(Int) = HexUC :-
    ( hex_digit(Int, _HexLC, HexUCPrime) ->
        HexUC = HexUCPrime
    ;
        unexpected($module, $pred, "hex_digit failed")
    ).

:- pred octal_digit(int::in, string::out) is semidet.

octal_digit(0, "0").
octal_digit(1, "1").
octal_digit(2, "2").
octal_digit(3, "3").
octal_digit(4, "4").
octal_digit(5, "5").
octal_digit(6, "6").
octal_digit(7, "7").

:- pred decimal_digit(int::in, string::out) is semidet.

decimal_digit(0, "0").
decimal_digit(1, "1").
decimal_digit(2, "2").
decimal_digit(3, "3").
decimal_digit(4, "4").
decimal_digit(5, "5").
decimal_digit(6, "6").
decimal_digit(7, "7").
decimal_digit(8, "8").
decimal_digit(9, "9").

:- pred hex_digit(int::in, string::out, string::out) is semidet.

hex_digit( 0, "0", "0").
hex_digit( 1, "1", "1").
hex_digit( 2, "2", "2").
hex_digit( 3, "3", "3").
hex_digit( 4, "4", "4").
hex_digit( 5, "5", "5").
hex_digit( 6, "6", "6").
hex_digit( 7, "7", "7").
hex_digit( 8, "8", "8").
hex_digit( 9, "9", "9").
hex_digit(10, "a", "A").
hex_digit(11, "b", "B").
hex_digit(12, "c", "C").
hex_digit(13, "d", "D").
hex_digit(14, "e", "E").
hex_digit(15, "f", "F").

%-----------------------------------------------------------------------------%

    % Unlike the standard library function, this function converts a float
    % to a string without resorting to scientific notation.
    %
    % This predicate relies on the fact that string.float_to_string returns
    % a float which is round-trippable, ie to the full precision needed.
    %
:- func convert_float_to_string(float) = string.

convert_float_to_string(Float) = String :-
    float_to_string_first_pass(Float, FloatStr),

    % Check for scientific representation.
    (
        ( string.contains_char(FloatStr, 'e')
        ; string.contains_char(FloatStr, 'E')
        )
    ->
        split_at_exponent(FloatStr, FloatPtStr, ExpStr),
        split_at_decimal_point(FloatPtStr, MantissaStr, FractionStr),

        % What is the exponent?
        ExpInt = string.det_to_int(ExpStr),
        ( ExpInt >= 0 ->
            % Move decimal pt to the right.
            ExtraDigits = ExpInt,
            PaddedFracStr = string.pad_right(FractionStr, '0', ExtraDigits),
            string.split(PaddedFracStr, ExtraDigits, MantissaRest,
                NewFraction),

            NewMantissa = MantissaStr ++ MantissaRest,
            MantAndPoint = NewMantissa ++ ".",
            ( NewFraction = "" ->
                String = MantAndPoint ++ "0"
            ;
                String = MantAndPoint ++ NewFraction
            )
        ;
            % Move decimal pt to the left.
            ExtraDigits = abs(ExpInt),
            PaddedMantissaStr = string.pad_left(MantissaStr, '0',
                ExtraDigits),
            string.split(PaddedMantissaStr,
                length(PaddedMantissaStr) - ExtraDigits,
                NewMantissa, FractionRest),

            ( NewMantissa = "" ->
                MantAndPoint = "0."
            ;
                MantAndPoint = NewMantissa ++ "."
            ),
            String = MantAndPoint ++ FractionRest ++ FractionStr
        )
    ;
        String = FloatStr
    ).

    % float_to_string_first_pass differs from string.float_to_string in that
    % it must be implemented without calling string.format, as this is the
    % predicate that the implementation of string.format uses to get
    % the initial string representation of a float.
    %
    % The string returned must match one of the following regular expression:
    %   ^[+-]?[0-9]*\.?[0-9]+((e|E)[0-9]+)?$
    %   ^[nN][aA][nN]$
    %   ^[+-]?[iI][nN][fF][iI][nN][iI][tT][yY]$
    %   ^[+-]?[iI][nN][fF]$
    % and the string returned must have sufficient precision for representing
    % the float.
    %
:- pred float_to_string_first_pass(float::in, string::uo) is det.

:- pragma foreign_proc("C",
    float_to_string_first_pass(FloatVal::in, FloatString::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"{
    /*
    ** Note any changes here will require the same changes in
    ** string.float_to_string.
    */
    MR_float_to_string(FloatVal, FloatString, MR_ALLOC_ID);
}").
:- pragma foreign_proc("C#",
    float_to_string_first_pass(FloatVal::in, FloatString::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    // The R format string prints the double out such that it can be
    // round-tripped.
    // XXX According to the documentation it tries the 15 digits of precision,
    // then 17 digits skipping 16 digits of precision, unlike what we do
    // for the C backend.
    FloatString = FloatVal.ToString(""R"");
").
:- pragma foreign_proc("Java",
    float_to_string_first_pass(FloatVal::in, FloatString::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    FloatString = java.lang.Double.toString(FloatVal);
").
:- pragma foreign_proc("Erlang",
    float_to_string_first_pass(FloatVal::in, FloatString::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    List = io_lib:format(""~.17g"", [FloatVal]),
    FloatString = list_to_binary(List)
").

    % Converts a floating point number to a specified number of standard
    % figures. The style used depends on the value converted; style e (or E)
    % is used only if the exponent resulting from such a conversion is less
    % than -4 or greater than or equal to the precision. Trailing zeros are
    % removed from the fractional portion of the result unless the # flag
    % is specified: a decimal-point character appears only if it is followed
    % by a digit.
    %
:- func change_to_g_notation(string, int, string, flags) = string.

change_to_g_notation(Float, Prec, E, Flags) = FormattedFloat :-
    Exponent = size_of_required_exponent(Float, Prec),
    (
        Exponent >= -4,
        Exponent < Prec
    ->
        % Float will be represented normally.
        % -----------------------------------
        % Need to calculate precision to pass to the change_precision function,
        % because the current precision represents significant figures,
        % not decimal places.
        %
        % Now change float's precision.
        %
        ( Exponent =< 0 ->
            % Deal with floats such as 0.00000000xyz.
            DecimalPos = decimal_pos(Float),
            FormattedFloat0 = change_precision(Float,
                abs(DecimalPos) - 1 + Prec)
        ;
            % Deal with floats such as ddddddd.mmmmmmmm.
            ScientificFloat = change_to_e_notation(Float, Prec - 1, "e"),
            split_at_exponent(ScientificFloat, BaseStr, ExponentStr),
            Exp = string.det_to_int(ExponentStr),
            split_at_decimal_point(BaseStr, MantissaStr, FractionStr),
            RestMantissaStr = between(FractionStr, 0, Exp),
            NewFraction = between(FractionStr, Exp, Prec - 1),
            FormattedFloat0 = MantissaStr ++ RestMantissaStr
                ++ "." ++ NewFraction
        ),

        % Do we remove trailing zeros?
        ( Flags ^ flag_hash = flag_hash_set ->
            FormattedFloat = FormattedFloat0
        ;
            FormattedFloat = remove_trailing_zeros(FormattedFloat0)
        )
    ;
        % Float will be represented in scientific notation.
        % -------------------------------------------------
        UncheckedFloat = change_to_e_notation(Float, Prec - 1, E),

        % Do we need to remove trailing zeros?
        ( Flags ^ flag_hash = flag_hash_set ->
            FormattedFloat = UncheckedFloat
        ;
            split_at_exponent(UncheckedFloat, BaseStr, ExponentStr),
            NewBaseStr = remove_trailing_zeros(BaseStr),
            FormattedFloat = NewBaseStr ++ E ++ ExponentStr
        )
    ).

    % Convert floating point notation to scientific notation.
    %
:- func change_to_e_notation(string, int, string) = string.

change_to_e_notation(Float, Prec, E) = ScientificFloat :-
    UnsafeExponent = decimal_pos(Float),
    UnsafeBase = calculate_base_unsafe(Float, Prec),

    % Is mantissa greater than one digit long?
    split_at_decimal_point(UnsafeBase, MantissaStr, _FractionStr),
    ( string.count_codepoints(MantissaStr) > 1 ->
        % Need to append 0, to fix the problem of having no numbers
        % after the decimal point.
        SafeBase = calculate_base_unsafe(string.append(UnsafeBase, "0"),
            Prec),
        SafeExponent = UnsafeExponent + 1
    ;
        SafeBase = UnsafeBase,
        SafeExponent = UnsafeExponent
    ),
    % Creating exponent.
    ( SafeExponent >= 0 ->
        ( SafeExponent < 10 ->
            ExponentStr = string.append_list(
                [E, "+0", string.int_to_string(SafeExponent)])
        ;
            ExponentStr = string.append_list(
                [E, "+", string.int_to_string(SafeExponent)])
        )
    ;
        ( SafeExponent > -10 ->
            ExponentStr = string.append_list(
                [E, "-0", string.int_to_string(int.abs(SafeExponent))])
        ;
            ExponentStr = E ++ string.int_to_string(SafeExponent)
        )
    ),
    ScientificFloat = SafeBase ++ ExponentStr.

    % Given a floating point number, this function calculates the size of
    % the exponent needed to represent the float in scientific notation.
    %
:- func size_of_required_exponent(string, int) = int.

size_of_required_exponent(Float, Prec) = Exponent :-
    UnsafeExponent = decimal_pos(Float),
    UnsafeBase = calculate_base_unsafe(Float, Prec),

    % Is mantissa one digit long?
    split_at_decimal_point(UnsafeBase, MantissaStr, _FractionStr),
    ( string.count_codepoints(MantissaStr) > 1 ->
        % We will need to move decimal pt one place to the left:
        % therefore, increment exponent.
        Exponent = UnsafeExponent + 1
    ;
        Exponent = UnsafeExponent
    ).

    % Given a string representing a floating point number, function returns
    % a string with all trailing zeros removed.
    %
:- func remove_trailing_zeros(string) = string.

remove_trailing_zeros(Float) = TrimmedFloat :-
    FloatCharList = string.to_char_list(Float),
    FloatCharListRev = list.reverse(FloatCharList),
    TrimmedFloatRevCharList = remove_zeros(FloatCharListRev),
    TrimmedFloatCharList = list.reverse(TrimmedFloatRevCharList),
    TrimmedFloat = string.from_char_list(TrimmedFloatCharList).

    % Given a char list, this function removes all leading zeros, including
    % decimal point, if need be.
    %
:- func remove_zeros(list(char)) = list(char).

remove_zeros(CharNum) = TrimmedNum :-
    ( CharNum = ['0' | Rest] ->
        TrimmedNum = remove_zeros(Rest)
    ; CharNum = ['.' | Rest] ->
        TrimmedNum = Rest
    ;
        TrimmedNum = CharNum
    ).

    % Determine the location of the decimal point in the string that
    % represents a floating point number.
    %
:- func decimal_pos(string) = int.

decimal_pos(Float) = Pos :-
    split_at_decimal_point(Float, MantissaStr, _FractionStr),
    NumZeros = string.count_codepoints(MantissaStr) - 1,
    Pos = find_non_zero_pos(string.to_char_list(Float), NumZeros).

    % Given a list of chars representing a floating point number, this
    % function determines the first position containing a non-zero digit.
    % Positions after the decimal point are negative, and those before the
    % decimal point are positive.
    %
:- func find_non_zero_pos(list(char), int) = int.

find_non_zero_pos(L, CurrentPos) = ActualPos :-
    (
        L = [H | T],
        ( is_decimal_point(H) ->
            ActualPos = find_non_zero_pos(T, CurrentPos)
        ; H = '0' ->
            ActualPos = find_non_zero_pos(T, CurrentPos - 1)
        ;
            ActualPos = CurrentPos
        )
    ;
        L = [],
        ActualPos = 0
    ).

    % Representing a floating point number in scientific notation requires
    % a base and an exponent. This function returns the base. But it is unsafe,
    % since particular input result in the base having a mantissa with more
    % than one digit. Therefore, the calling function must check for this
    % problem.
    %
:- func calculate_base_unsafe(string, int) = string.

calculate_base_unsafe(Float, Prec) = Exp :-
    Place = decimal_pos(Float),
    split_at_decimal_point(Float, MantissaStr, FractionStr),
    ( Place < 0 ->
        DecimalPos = abs(Place),
        PaddedMantissaStr = string.between(FractionStr, 0, DecimalPos),

        % Get rid of superfluous zeros.
        MantissaInt = string.det_to_int(PaddedMantissaStr),
        ExpMantissaStr = string.int_to_string(MantissaInt),

        % Create fractional part.
        PaddedFractionStr = pad_right(FractionStr, '0', Prec + 1),
        ExpFractionStr = string.between(PaddedFractionStr, DecimalPos,
            DecimalPos + Prec + 1)
    ; Place > 0 ->
        ExpMantissaStr = string.between(MantissaStr, 0, 1),
        FirstHalfOfFractionStr = string.between(MantissaStr, 1, Place + 1),
        ExpFractionStr = FirstHalfOfFractionStr ++ FractionStr
    ;
        ExpMantissaStr = MantissaStr,
        ExpFractionStr = FractionStr
    ),
    MantissaAndPoint = ExpMantissaStr ++ ".",
    UnroundedExpStr = MantissaAndPoint ++ ExpFractionStr,
    Exp = change_precision(UnroundedExpStr, Prec).

    % Change the precision of a float to a specified number of decimal places.
    %
    % n.b. OldFloat must be positive for this function to work.
    %
:- func change_precision(string, int) = string.

change_precision(OldFloat, Prec) = NewFloat :-
    split_at_decimal_point(OldFloat, MantissaStr, FractionStr),
    FracStrLen = string.count_codepoints(FractionStr),
    ( Prec > FracStrLen ->
        PrecFracStr = string.pad_right(FractionStr, '0', Prec),
        PrecMantissaStr = MantissaStr
    ; Prec < FracStrLen ->
        UnroundedFrac = string.between(FractionStr, 0, Prec),
        NextDigit = string.det_index(FractionStr, Prec),
        (
            UnroundedFrac \= "",
            (char.to_int(NextDigit) - char.to_int('0')) >= 5
        ->
            NewPrecFrac = string.det_to_int(UnroundedFrac) + 1,
            NewPrecFracStrNotOK = string.int_to_string( NewPrecFrac),
            NewPrecFracStr = string.pad_left(NewPrecFracStrNotOK, '0', Prec),
            (
                string.count_codepoints(NewPrecFracStr) >
                    string.count_codepoints(UnroundedFrac)
            ->
                PrecFracStr = between(NewPrecFracStr, 1, 1 + Prec),
                PrecMantissaInt = det_to_int(MantissaStr) + 1,
                PrecMantissaStr = int_to_string(PrecMantissaInt)
            ;
                PrecFracStr = NewPrecFracStr,
                PrecMantissaStr = MantissaStr
            )
        ;
            UnroundedFrac = "",
            (char.to_int(NextDigit) - char.to_int('0')) >= 5
        ->
            PrecMantissaInt = det_to_int(MantissaStr) + 1,
            PrecMantissaStr = int_to_string(PrecMantissaInt),
            PrecFracStr = ""
        ;
            PrecFracStr = UnroundedFrac,
            PrecMantissaStr = MantissaStr
        )
    ;
        PrecFracStr = FractionStr,
        PrecMantissaStr = MantissaStr
    ),
    HalfNewFloat = PrecMantissaStr ++ ".",
    NewFloat = HalfNewFloat ++ PrecFracStr.

:- pred split_at_exponent(string::in, string::out, string::out) is det.

split_at_exponent(Str, Float, Exponent) :-
    FloatAndExponent = string.words_separator(is_exponent, Str),
    list.det_index0(FloatAndExponent, 0, Float),
    list.det_index0(FloatAndExponent, 1, Exponent).

:- pred split_at_decimal_point(string::in, string::out, string::out) is det.

split_at_decimal_point(Str, Mantissa, Fraction) :-
    MantAndFrac = string.words_separator(is_decimal_point, Str),
    list.det_index0(MantAndFrac, 0, Mantissa),
    ( list.index0(MantAndFrac, 1, Fraction0) ->
        Fraction = Fraction0
    ;
        Fraction = ""
    ).

:- pred is_decimal_point(char::in) is semidet.

is_decimal_point('.').

:- pred is_exponent(char::in) is semidet.

is_exponent('e').
is_exponent('E').

%-----------------------------------------------------------------------------%
