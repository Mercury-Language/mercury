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
    ;       base_hex_lc     % use spec_case?
    ;       base_hex_uc
    ;       base_hex_p.

:- type float_kind
    --->    kind_e_scientific_lc
    ;       kind_e_scientific_uc
    ;       kind_f_plain_lc
    ;       kind_f_plain_uc
    ;       kind_g_flexible_lc
    ;       kind_g_flexible_uc.

:- type spec_case
    --->    case_is_capital
    ;       case_is_not_capital.

:- type spec(I, F, C, S)
    --->    spec_percent
    ;       spec_signed_int(flags, maybe_width, maybe_prec, I)
    ;       spec_unsigned_int(flags, maybe_width, maybe_prec, int_base, I)
    ;       spec_float(flags, maybe_width, maybe_prec, float_kind, F)
    ;       spec_char(flags, maybe_width, maybe_prec, C)
    ;       spec_string(flags, maybe_width, maybe_prec, S).

:- type spec == spec(int, float, char, string).

:- type format_str_component
    --->    comp_str(list(char))
    ;       comp_conv_spec(spec).

%-----------------------------------------------------------------------------%

string.format.format_impl(FormatString, PolyList, String) :-
    % This predicate has been optimised to produce the least memory possible
    % -- memory usage is a significant problem for programs which do a lot of
    % formatted IO.
    Chars = to_char_list(FormatString),
    format_string_to_components(Chars, CharsLeftOver,
        PolyList, PolyListLeftOver, Components),
    (
        PolyListLeftOver = [],
        CharsLeftOver = []
    ->
        components_to_strings(Components, ComponentStrs),
        String = string.append_list(ComponentStrs)
    ;
        error("string.format: format string invalid.")
    ).

    % This predicate parses as much of a format string as it can.
    % It stops parsing when it encounters something that looks like
    % a conversion specification (i.e. it starts with a '%' character),
    % but which cannot be parsed as one.
    %
:- pred format_string_to_components(list(char)::in, list(char)::out,
    list(string.poly_type)::in, list(string.poly_type)::out,
    list(format_str_component)::out) is det.

format_string_to_components(!Chars, !PolyTypes, Components) :-
    gather_non_percent_chars(NonConversionSpecChars, !Chars),
    ( !.Chars = ['%' | !:Chars] ->
        % NOTE conversion_specification could return a list of errors,
        % so that if the format string has more than one error, we can
        % throw an exception describing them all, not just one the first one.
        % Unfortunately, in the common cases of missing or extra PolyTypes,
        % all the errors after the first would be avalanche errors,
        % and would probably be more confusing than helpful.
        parse_conversion_specification(!Chars, !PolyTypes, ConvComponent),
        format_string_to_components(!Chars, !PolyTypes, ComponentsTail),
        Components0 = [ConvComponent | ComponentsTail]
    ;
        Components0 = []
    ),
    (
        NonConversionSpecChars = [],
        Components = Components0
    ;
        NonConversionSpecChars = [_ | _],
        StringComponent = comp_str(NonConversionSpecChars),
        Components = [StringComponent | Components0]
    ).

    % Parse a string which doesn't contain any conversion specifications.
    %
:- pred gather_non_percent_chars(list(char)::out,
    list(char)::in, list(char)::out) is det.

gather_non_percent_chars(Result, !Chars) :-
    (
        !.Chars = [Char | !:Chars],
        Char \= '%'
    ->
        gather_non_percent_chars(Result0, !Chars),
        Result = [Char | Result0]
    ;
        Result = []
    ).

    % Each conversion specification is introduced by the character '%'
    % (which our caller has already read) and ends with a conversion specifier.
    % In between there may be (in this order) zero or more flags, an optional
    % minimum field width, and an optional precision.
    %
:- pred parse_conversion_specification(list(char)::in, list(char)::out,
    list(string.poly_type)::in, list(string.poly_type)::out,
    format_str_component::out) is det.

parse_conversion_specification(!Chars, !PolyTypes, Component) :-
    Flags0 = flags(flag_hash_clear, flag_space_clear, flag_zero_clear,
        flag_minus_clear, flag_plus_clear),
    gather_flag_chars(!Chars, Flags0, Flags),
    get_optional_width(!Chars, !PolyTypes, MaybeWidth),
    get_optional_prec(!Chars, !PolyTypes, MaybePrec),
    ( get_first_spec(!Chars, !PolyTypes, Flags, MaybeWidth, MaybePrec, Spec) ->
        Component = comp_conv_spec(Spec)
    ;
        error("string.format: invalid conversion specifier.")
    ).

:- pred gather_flag_chars(list(char)::in, list(char)::out,
    flags::in, flags::out) is det.

gather_flag_chars(!Chars, !Flags) :-
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

    % Is it a valid flag character?
    %
:- pred is_flag_char(char::in) is semidet.

is_flag_char('#').
is_flag_char('0').
is_flag_char('-').
is_flag_char(' ').
is_flag_char('+').

    % Do we have a minimum field width? If yes, get it.
    %
:- pred get_optional_width(list(char)::in, list(char)::out,
    list(string.poly_type)::in, list(string.poly_type)::out,
    maybe_width::out) is det.

get_optional_width(!Chars, !PolyTypes, MaybeWidth) :-
    ( !.Chars = ['*' | !:Chars] ->
        ( !.PolyTypes = [i(PolyWidth) | !:PolyTypes] ->
            MaybeWidth = yes(PolyWidth)
        ;
            error("string.format",
                "`*' width modifier not associated with an integer.")
        )
    ; get_nonzero_number_prefix(!Chars, Width) ->
        MaybeWidth = yes(Width)
    ;
        MaybeWidth = no
    ).

    % Do we have a precision? If yes, get it.
    %
:- pred get_optional_prec(list(char)::in, list(char)::out,
    list(string.poly_type)::in, list(string.poly_type)::out,
    maybe_prec::out) is det.

get_optional_prec(!Chars, !PolyTypes, MaybePrec) :-
    ( !.Chars = ['.' | !:Chars] ->
        ( !.Chars = ['*' | !:Chars] ->
            ( !.PolyTypes = [i(PolyPrec) | !:PolyTypes] ->
                Prec = PolyPrec
            ;
                error("string.format",
                    "`*' precision modifier not associated with an integer.")
            )
        ;
            get_number_prefix(!Chars, 0, Prec)
        ),
        MaybePrec = yes(Prec)
    ;
        MaybePrec = no
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

    % Do we have a valid conversion specifier?
    % We check to ensure that the specifier also matches the type
    % from the input list.
    %
:- pred get_first_spec(list(char)::in, list(char)::out,
    list(string.poly_type)::in, list(string.poly_type)::out,
    flags::in, maybe_width::in, maybe_prec::in, spec::out) is semidet.

get_first_spec(!Chars, !PolyTypes, !.Flags, MaybeWidth, MaybePrec, Spec) :-
    !.Chars = [SpecChar | !:Chars],
    (
        SpecChar = '%',
        Spec = spec_percent
    ;
        (
            SpecChar = 'd'
        ;
            SpecChar = 'i'
        ),
        !.PolyTypes = [SpecPolyType | !:PolyTypes],
        SpecPolyType = i(Int),
        % Base is always decimal
        Spec = spec_signed_int(!.Flags, MaybeWidth, MaybePrec, Int)
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
        !.PolyTypes = [SpecPolyType | !:PolyTypes],
        SpecPolyType = i(Int),
        Spec = spec_unsigned_int(!.Flags, MaybeWidth, MaybePrec, Base, Int)
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
        !.PolyTypes = [SpecPolyType | !:PolyTypes],
        SpecPolyType = f(Float),
        Spec = spec_float(!.Flags, MaybeWidth, MaybePrec, FloatKind, Float)
    ;
        SpecChar = 'c',
        !.PolyTypes = [SpecPolyType | !:PolyTypes],
        SpecPolyType = c(Char),
        Spec = spec_char(!.Flags, MaybeWidth, MaybePrec, Char)
    ;
        SpecChar = 's',
        !.PolyTypes = [SpecPolyType | !:PolyTypes],
        SpecPolyType = s(Str),
        Spec = spec_string(!.Flags, MaybeWidth, MaybePrec, Str)
    ).

:- pred components_to_strings(list(format_str_component)::in,
    list(string)::out) is det.

components_to_strings([], []).
components_to_strings([Component | Components], [String | Strings]) :-
    component_to_string(Component, String),
    components_to_strings(Components, Strings).

:- pred component_to_string(format_str_component::in, string::out) is det.

component_to_string(Component, String) :-
    Component = comp_str(Chars),
    String = string.from_char_list(Chars).
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
            String = gen_format_unsigned_int(Flags, MaybeWidth, MaybePrec,
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
            String = gen_format_float(Flags, MaybeWidth, MaybePrec, Kind,
                Float)
        )
    ;
        % Char conversion specifiers.
        Spec = spec_char(Flags, MaybeWidth, MaybePrec, Char),
        ( using_sprintf_for_char(Char) ->
            FormatStr = make_format(Flags, MaybeWidth, MaybePrec, "", "c"),
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
    % Find the integer's absolute value, and take care of the special case
    % of precision zero with an integer of 0.
    (
        Int = 0,
        MaybePrec = yes(0)
    ->
        AbsIntStr = ""
    ;
        Integer = integer(Int),
        AbsInteger = integer.abs(Integer),
        AbsIntStr = integer.to_string(AbsInteger)
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
    SignedStr = add_int_prefix_if_needed(Flags, ZeroPadded, Int, FieldStr),
    String = justify_string(Flags, MaybeWidth, SignedStr).

:- func gen_format_unsigned_int(flags, maybe_width, maybe_prec, int_base,
    int) = string.

gen_format_unsigned_int(Flags, MaybeWidth, MaybePrec, Base, Int) = String :-
    % XXX The octal prefix used to be "".
    ( Base = base_octal,   BaseNum =  8, IsP =  no, Prefix = "0"
    ; Base = base_decimal, BaseNum = 10, IsP =  no, Prefix = ""
    ; Base = base_hex_lc,  BaseNum = 16, IsP =  no, Prefix = "0x"
    ; Base = base_hex_uc,  BaseNum = 16, IsP =  no, Prefix = "0X"
    ; Base = base_hex_p,   BaseNum = 16, IsP = yes, Prefix = "0x"
    ),
    % XXX arglist
    String = format_unsigned_int(Flags, MaybeWidth, MaybePrec,
        BaseNum, Int, IsP, Prefix).

    % Format an unsigned int, unsigned octal, or unsigned hexadecimal
    % (u,o,x,X).
    %
:- func format_unsigned_int(flags, maybe_width, maybe_prec,
    int, int, bool, string) = string.

format_unsigned_int(Flags, MaybeWidth, MaybePrec, Base, Int, IsTypeP, Prefix)
        = String :-
    % Find the integer's absolute value, and take care of the special case
    % of precision zero with an integer of 0.
    (
        Int = 0,
        MaybePrec = yes(0)
    ->
        AbsIntStr = ""
    ;
        Div = integer.pow(integer(2), integer(int.bits_per_int)),
        UnsignedInteger = integer(Int) mod Div,
        ( Base = 10 ->
            AbsIntStr0 = integer.to_string(UnsignedInteger)
        ; Base = 8 ->
            AbsIntStr0 = abs_integer_to_octal(UnsignedInteger)
        ; Prefix = "0x" ->
            AbsIntStr0 = abs_integer_to_hex_lc(UnsignedInteger)
        ;
            AbsIntStr0 = abs_integer_to_hex_uc(UnsignedInteger)
        ),

        % Just in case Int = 0 (base converters return "").
        ( AbsIntStr0 = "" ->
            AbsIntStr = "0"
        ;
            AbsIntStr = AbsIntStr0
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
        Base = 8,
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
            Base = 16,
            Flags ^ flag_hash = flag_hash_set,
            ( Int \= 0
            ; IsTypeP = yes
            )
        ->
            FieldStr = string.pad_left(PrecModStr, '0', Width - 2)
        ;
            FieldStr = string.pad_left(PrecModStr, '0', Width)
        )
    ;
        FieldStr = PrecModStr
    ),

    % Do we have to prefix "0x" or "0X"?
    (
        Base = 16,
        Flags ^ flag_hash = flag_hash_set,
        ( Int \= 0
        ; IsTypeP = yes
        )
    ->
        FieldModStr = Prefix ++ FieldStr
    ;
        FieldModStr = FieldStr
    ),

    String = justify_string(Flags, MaybeWidth, FieldModStr).

%-----------------------------------------------------------------------------%

:- func gen_format_float(flags, maybe_width, maybe_prec, float_kind, float)
    = string.

gen_format_float(Flags, MaybeWidth, MaybePrec, Kind, Float) = String :-
    % XXX function choice, arglists
    (
        Kind = kind_e_scientific_lc,
        String = format_scientific_number(Flags, case_is_not_capital,
            MaybeWidth, MaybePrec, Float, "e")
    ;
        Kind = kind_e_scientific_uc,
        String = format_scientific_number(Flags, case_is_capital,
            MaybeWidth, MaybePrec, Float, "E")
    ;
        Kind = kind_f_plain_lc,
        String = format_float(Flags, case_is_not_capital,
            MaybeWidth, MaybePrec, Float)
    ;
        Kind = kind_f_plain_uc,
        String = format_float(Flags, case_is_capital,
            MaybeWidth, MaybePrec, Float)
    ;
        Kind = kind_g_flexible_lc,
        String = format_scientific_number_g(Flags, case_is_not_capital,
            MaybeWidth, MaybePrec, Float, "e")
    ;
        Kind = kind_g_flexible_uc,
        String = format_scientific_number_g(Flags, case_is_capital,
            MaybeWidth, MaybePrec, Float, "E")
    ).

    % Format a float.
    %
:- func format_float(flags, spec_case, maybe_width, maybe_prec, float)
    = string.

format_float(Flags, SpecCase, MaybeWidth, MaybePrec, Float) = NewFloat :-
    ( is_nan(Float) ->
        SignedStr = format_nan(SpecCase)
    ; is_infinite(Float) ->
        SignedStr = format_infinity(Float, SpecCase)
    ;
        % Determine absolute value of string.
        Abs = abs(Float),

        % Change precision (default is 6).
        AbsStr = convert_float_to_string(Abs),
        (
            MaybePrec = yes(Prec),
            PrecStr = change_precision(Prec, AbsStr)
        ;
            MaybePrec = no,
            PrecStr = change_precision(6, AbsStr)
        ),

        % Do we need to remove the decimal point?
        (
            Flags ^ flag_hash = flag_hash_clear,
            MaybePrec = yes(0)
        ->
            PrecStrLen = string.count_codepoints(PrecStr),
            PrecModStr = string.between(PrecStr, 0, PrecStrLen - 1)
        ;
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
        SignedStr = add_float_prefix_if_needed(Flags, ZeroPadded, Float,
            FieldStr)
    ),
    NewFloat = justify_string(Flags, MaybeWidth, SignedStr).

    % Format a scientific number to a specified number of significant
    % figures (g,G)
    %
:- func format_scientific_number_g(flags, spec_case, maybe_width, maybe_prec,
    float, string) = string.

format_scientific_number_g(Flags, SpecCase, MaybeWidth, MaybePrec, Float, E)
        = String :-
    ( is_nan(Float) ->
        SignedStr = format_nan(SpecCase)
    ; is_infinite(Float) ->
        SignedStr = format_infinity(Float, SpecCase)
    ;
        % Determine absolute value of string.
        Abs = abs(Float),

        % Change precision (default is 6).
        AbsStr = convert_float_to_string(Abs),
        (
            MaybePrec = yes(Prec),
            ( Prec = 0 ->
                PrecStr = change_to_g_notation(AbsStr, 1, E, Flags)
            ;
                PrecStr = change_to_g_notation(AbsStr, Prec, E, Flags)
            )
        ;
            MaybePrec = no,
            PrecStr = change_to_g_notation(AbsStr, 6, E, Flags)
        ),

        % Do we need to change field width?
        (
            MaybeWidth = yes(Width),
            Width > string.count_codepoints(PrecStr),
            Flags ^ flag_zero = flag_zero_set,
            Flags ^ flag_minus = flag_minus_clear
        ->
            FieldStr = string.pad_left(PrecStr, '0', Width - 1),
            ZeroPadded = yes
        ;
            FieldStr = PrecStr,
            ZeroPadded = no
        ),

        % Finishing up.
        SignedStr = add_float_prefix_if_needed(Flags, ZeroPadded, Float,
            FieldStr)
    ),
    String = justify_string(Flags, MaybeWidth, SignedStr).

    % Format a scientific number (e,E)
    %
:- func format_scientific_number(flags, spec_case, maybe_width, maybe_prec,
    float, string) = string.

format_scientific_number(Flags, SpecCase, MaybeWidth, MaybePrec, Float, E)
        = String :-
    ( is_nan(Float) ->
        SignedStr = format_nan(SpecCase)
    ; is_infinite(Float) ->
        SignedStr = format_infinity(Float, SpecCase)
    ;
        % Determine absolute value of string.
        Abs = abs(Float),

        % Change precision (default is 6).
        AbsStr = convert_float_to_string(Abs),
        (
            MaybePrec = yes(Prec),
            PrecStr = change_to_e_notation(AbsStr, Prec, E)
        ;
            MaybePrec = no,
            PrecStr = change_to_e_notation(AbsStr, 6, E)
        ),

        % Do we need to remove the decimal point?
        (
            Flags ^ flag_hash = flag_hash_clear,
            MaybePrec = yes(0)
        ->
            split_at_decimal_point(PrecStr, BaseStr, ExponentStr),
            PrecModStr = BaseStr ++ ExponentStr
        ;
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
        SignedStr = add_float_prefix_if_needed(Flags, ZeroPadded, Float,
            FieldStr)
    ),
    String = justify_string(Flags, MaybeWidth, SignedStr).

:- func add_int_prefix_if_needed(flags, bool, int, string) = string.

add_int_prefix_if_needed(Flags, ZeroPadded, Int, FieldStr) = SignedStr :-
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

:- func add_float_prefix_if_needed(flags, bool, float, string) = string.

add_float_prefix_if_needed(Flags, ZeroPadded, Float, FieldStr) = SignedStr :-
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

    % Convert a non-negative integer to an octal string.
    %
:- func abs_integer_to_octal(integer) = string.

abs_integer_to_octal(Num) = NumStr :-
    ( Num > integer(0) ->
        Rest = abs_integer_to_octal(Num // integer(8)),
        Rem = Num rem integer(8),
        RemStr = integer.to_string(Rem),
        NumStr = append(Rest, RemStr)
    ;
        NumStr = ""
    ).

    % Convert a non-negative integer to a hexadecimal string,
    % using a-f for to_hex_lc and A-F for to_hex_uc.
    %
    % XXX append; integer vs int
    %
:- func abs_integer_to_hex_lc(integer) = string.
:- func abs_integer_to_hex_uc(integer) = string.

abs_integer_to_hex_lc(Num) = NumStr :-
    ( Num > integer(0) ->
        Rest = abs_integer_to_hex_lc(Num // integer(16)),
        Rem = Num rem integer(16),
        RemInt = int(Rem),
        RemStr = get_hex_digit_lc(RemInt),
        NumStr = append(Rest, RemStr)
    ;
        NumStr = ""
    ).

abs_integer_to_hex_uc(Num) = NumStr :-
    ( Num > integer(0) ->
        Rest = abs_integer_to_hex_uc(Num // integer(16)),
        Rem = Num rem integer(16),
        RemInt = int(Rem),
        RemStr = get_hex_digit_uc(RemInt),
        NumStr = append(Rest, RemStr)
    ;
        NumStr = ""
    ).

    % Given an int between 0 and 15, return the hexadecimal digit
    % representing it, using a-f for get_hex_digit_lc and
    % A-F for get_hex_digit_uc.
    %
:- func get_hex_digit_lc(int) = string.
:- func get_hex_digit_uc(int) = string.

get_hex_digit_lc(Int) = HexLC :-
    ( hex_digits(Int, HexLCPrime, _HexUC) ->
        HexLC = HexLCPrime
    ;
        unexpected($module, $pred, "hex_digits failed")
    ).

get_hex_digit_uc(Int) = HexUC :-
    ( hex_digits(Int, _HexLC, HexUCPrime) ->
        HexUC = HexUCPrime
    ;
        unexpected($module, $pred, "hex_digits failed")
    ).

:- pred hex_digits(int::in, string::out, string::out) is semidet.

hex_digits( 0, "0", "0").
hex_digits( 1, "1", "1").
hex_digits( 2, "2", "2").
hex_digits( 3, "3", "3").
hex_digits( 4, "4", "4").
hex_digits( 5, "5", "5").
hex_digits( 6, "6", "6").
hex_digits( 7, "7", "7").
hex_digits( 8, "8", "8").
hex_digits( 9, "9", "9").
hex_digits(10, "a", "A").
hex_digits(11, "b", "B").
hex_digits(12, "c", "C").
hex_digits(13, "d", "D").
hex_digits(14, "e", "E").
hex_digits(15, "f", "F").

    % Unlike the standard library function, this function converts a float
    % to a string without resorting to scientific notation.
    %
    % This predicate relies on the fact that string.float_to_string returns
    % a float which is round-trippable, ie to the full precision needed.
    %
:- func convert_float_to_string(float) = string.

convert_float_to_string(Float) = String :-
    string.lowlevel_float_to_string(Float, FloatStr),

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
            FormattedFloat0 = change_precision(abs(DecimalPos) - 1 + Prec,
                Float)
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
    Exp = change_precision(Prec, UnroundedExpStr).

    % Change the precision of a float to a specified number of decimal places.
    %
    % n.b. OldFloat must be positive for this function to work.
    %
:- func change_precision(int, string) = string.

change_precision(Prec, OldFloat) = NewFloat :-
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

:- func format_nan(spec_case) = string.

format_nan(case_is_capital) = "NAN".
format_nan(case_is_not_capital) = "nan".

:- func format_infinity(float, spec_case) = string.

format_infinity(F, SpecCase) = String :-
    (
        SpecCase = case_is_capital,
        String = ( if F < 0.0 then "-INFINITY" else "INFINITY" )
    ;
        SpecCase = case_is_not_capital,
        String = ( if F < 0.0 then "-infinity" else "infinity" )
    ).

%-----------------------------------------------------------------------------%
