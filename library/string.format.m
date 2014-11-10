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
% This modules implement string.format.
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
#include <ctype.h>
#include <string.h>
#include <stdio.h>

#include ""mercury_string.h""   /* for MR_allocate_aligned_string*() etc. */
#include ""mercury_tags.h"" /* for MR_list_cons*() */

/*
** The following macro should expand to MR_TRUE if the C grades should
** implement string.format using C's sprintf function.
** Setting it to MR_FALSE will cause string.format to use the Mercury
** implementation of string formatting in C grades.
*/
#define ML_USE_SPRINTF MR_TRUE
").

%-----------------------------------------------------------------------------%

string.format.format_impl(FormatString, PolyList, String) :-
    % This predicate has been optimised to produce the least memory possible
    % -- memory usage is a significant problem for programs which do a lot of
    % formatted IO.
    Chars = to_char_list(FormatString),
    format_string_to_components(Specifiers, PolyList, PolyListLeftOver,
        Chars, CharsLeftOver),
    (
        PolyListLeftOver = [],
        CharsLeftOver = []
    ->
        components_to_strings(Specifiers, SpecStrs),
        String = string.append_list(SpecStrs)
    ;
        error("string.format: format string invalid.")
    ).

:- type format_str_component
    --->    comp_str(list(char))
    ;       comp_conv(
                % We should consider using a tuple of bools to represent flags.
                flags       :: list(char),
                width       :: maybe(int),
                precision   :: maybe(int),
                spec        :: spec
            ).

    % This predicate parses as much of a format string as it can.
    % It stops parsing when it encounters something that looks like
    % a conversion specification (i.e. it starts with a '%' character),
    % but which cannot be parsed as one.
    %
:- pred format_string_to_components(list(format_str_component)::out,
    list(string.poly_type)::in, list(string.poly_type)::out,
    list(char)::in, list(char)::out) is det.

format_string_to_components(Specifiers, !PolyTypes, !Chars) :-
    gather_non_percent_chars(NonConversionSpecChars, !Chars),
    ( !.Chars = ['%' | !:Chars] ->
        % NOTE conversion_specification could return a list of errors,
        % so that if the format string has more than one error, we can
        % throw an exception describing them all, not just one the first one.
        % Unfortunately, in the common cases of missing or extra PolyTypes,
        % all the errors after the first would be avalanche errors,
        % and would probably be more confusing than helpful.
        parse_conversion_specification(ConversionSpec, !PolyTypes, !Chars),
        format_string_to_components(SpecifiersTail, !PolyTypes, !Chars),
        Specifiers0 = [ConversionSpec | SpecifiersTail]
    ;
        Specifiers0 = []
    ),
    (
        NonConversionSpecChars = [],
        Specifiers = Specifiers0
    ;
        NonConversionSpecChars = [_ | _],
        NonConversionSpec = comp_str(NonConversionSpecChars),
        Specifiers = [NonConversionSpec | Specifiers0]
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
:- pred parse_conversion_specification(format_str_component::out,
    list(string.poly_type)::in, list(string.poly_type)::out,
    list(char)::in, list(char)::out) is det.

parse_conversion_specification(Specifier, !PolyTypes, !Chars) :-
    gather_flag_chars(Flags, !Chars),
    get_optional_width(MaybeWidth, !PolyTypes, !Chars),
    get_optional_prec(MaybePrec, !PolyTypes, !Chars),
    ( spec(Spec, !PolyTypes, !Chars) ->
        Specifier = comp_conv(Flags, MaybeWidth, MaybePrec, Spec)
    ;
        error("string.format: invalid conversion specifier.")
    ).

:- pred gather_flag_chars(list(char)::out,
    list(char)::in, list(char)::out) is det.

gather_flag_chars(FlagChars, !Chars) :-
    (
        !.Chars = [Char | !:Chars],
        is_flag_char(Char)
    ->
        gather_flag_chars(TailFlagChars, !Chars),
        FlagChars = [Char | TailFlagChars]
    ;
        FlagChars = []
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
:- pred get_optional_width(maybe(int)::out,
    list(string.poly_type)::in, list(string.poly_type)::out,
    list(char)::in, list(char)::out) is det.

get_optional_width(MaybeWidth, !PolyTypes, !Chars) :-
    ( !.Chars = ['*' | !:Chars] ->
        ( !.PolyTypes = [i(PolyWidth) | !:PolyTypes] ->
            MaybeWidth = yes(PolyWidth)
        ;
            error("string.format",
                "`*' width modifier not associated with an integer.")
        )
    ; get_nonzero_number_prefix(Width, !Chars) ->
        MaybeWidth = yes(Width)
    ;
        MaybeWidth = no
    ).

    % Do we have a precision? If yes, get it.
    %
:- pred get_optional_prec(maybe(int)::out,
    list(string.poly_type)::in, list(string.poly_type)::out,
    list(char)::in, list(char)::out) is det.

get_optional_prec(MaybePrec, !PolyTypes, !Chars) :-
    ( !.Chars = ['.' | !:Chars] ->
        ( !.Chars = ['*' | !:Chars] ->
            ( !.PolyTypes = [i(PolyPrec) | !:PolyTypes] ->
                Prec = PolyPrec
            ;
                error("string.format",
                    "`*' precision modifier not associated with an integer.")
            )
        ;
            get_number_prefix(0, Prec, !Chars)
        ),
        MaybePrec = yes(Prec)
    ;
        MaybePrec = no
    ).

:- pred get_nonzero_number_prefix(int::out, list(char)::in, list(char)::out)
    is semidet.

get_nonzero_number_prefix(N, !Chars) :-
    !.Chars = [Char | !:Chars],
    Char \= '0',
    char.decimal_digit_to_int(Char, CharValue),
    get_number_prefix(CharValue, N, !Chars).

:- pred get_number_prefix(int::in, int::out, list(char)::in, list(char)::out)
    is det.

get_number_prefix(N0, N, !Chars) :-
    (
        !.Chars = [Char | !:Chars],
        char.decimal_digit_to_int(Char, CharValue)
    ->
        N1 = N0 * 10 + CharValue,
        get_number_prefix(N1, N, !Chars)
    ;
        N = N0
    ).

% NOTE the capital letter specifiers are preceded with a 'c'.
:- type spec
            % valid integer specifiers
    --->    d(int)
    ;       i(int)
    ;       o(int)
    ;       u(int)
    ;       x(int)
    ;       cX(int)
    ;       p(int)

            % valid float specifiers
    ;       e(float)
    ;       cE(float)
    ;       f(float)
    ;       cF(float)
    ;       g(float)
    ;       cG(float)

            % valid char specifiers
    ;       c(char)

            % valid string specifiers
    ;       s(string)

            % specifier representing "%%"
    ;       percent.

    % Is the spec a capital letter?
    %
:- type spec_case
    --->    spec_is_capital
    ;       spec_is_not_capital.

    % Do we have a valid conversion specifier?
    % We check to ensure that the specifier also matches the type
    % from the input list.
    %
:- pred spec(spec::out,
    list(string.poly_type)::in, list(string.poly_type)::out,
    list(char)::in, list(char)::out) is semidet.

% Valid integer conversion specifiers.
spec(d(Int),  [i(Int) | Ps], Ps, !Chars) :- !.Chars = ['d' | !:Chars].
spec(i(Int),  [i(Int) | Ps], Ps, !Chars) :- !.Chars = ['i' | !:Chars].
spec(o(Int),  [i(Int) | Ps], Ps, !Chars) :- !.Chars = ['o' | !:Chars].
spec(u(Int),  [i(Int) | Ps], Ps, !Chars) :- !.Chars = ['u' | !:Chars].
spec(x(Int),  [i(Int) | Ps], Ps, !Chars) :- !.Chars = ['x' | !:Chars].
spec(cX(Int), [i(Int) | Ps], Ps, !Chars) :- !.Chars = ['X' | !:Chars].
spec(p(Int),  [i(Int) | Ps], Ps, !Chars) :- !.Chars = ['p' | !:Chars].

% Valid float conversion specifiers.
spec(e(Float),  [f(Float) | Ps], Ps, !Chars) :- !.Chars = ['e' | !:Chars].
spec(cE(Float), [f(Float) | Ps], Ps, !Chars) :- !.Chars = ['E' | !:Chars].
spec(f(Float),  [f(Float) | Ps], Ps, !Chars) :- !.Chars = ['f' | !:Chars].
spec(cF(Float), [f(Float) | Ps], Ps, !Chars) :- !.Chars = ['F' | !:Chars].
spec(g(Float),  [f(Float) | Ps], Ps, !Chars) :- !.Chars = ['g' | !:Chars].
spec(cG(Float), [f(Float) | Ps], Ps, !Chars) :- !.Chars = ['G' | !:Chars].

% Valid char conversion specifiers.
spec(c(Char), [c(Char) | Ps], Ps, !Chars) :- !.Chars = ['c' | !:Chars].

% Valid string conversion specifiers.
spec(s(Str), [s(Str) | Ps], Ps, !Chars) :- !.Chars = ['s' | !:Chars].

% Conversion specifier representing the "%" sign.
spec(percent, Ps, Ps, !Chars) :- !.Chars = ['%' | !:Chars].

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
    Component = comp_conv(Flags, Width, Prec, Spec),
    (
        % Valid int conversion specifiers.
        Spec = d(Int),
        ( using_sprintf ->
            FormatStr = make_format(Flags, Width, Prec, int_length_modifer,
                "d"),
            String = native_format_int(FormatStr, Int)
        ;
            String = format_int(Flags, Width, Prec, Int)
        )
    ;
        Spec = i(Int),
        ( using_sprintf ->
            FormatStr = make_format(Flags, Width, Prec, int_length_modifer,
                "i"),
            String = native_format_int(FormatStr, Int)
        ;
            String = format_int(Flags, Width, Prec, Int)
        )
    ;
        Spec = o(Int),
        ( using_sprintf ->
            FormatStr = make_format(Flags, Width, Prec, int_length_modifer,
                "o"),
            String = native_format_int(FormatStr, Int)
        ;
            String = format_unsigned_int(Flags, Width, Prec,
                8, Int, no, "")
        )
    ;
        Spec = u(Int),
        ( using_sprintf ->
            FormatStr = make_format(Flags, Width, Prec, int_length_modifer,
                "u"),
            String = native_format_int(FormatStr, Int)
        ;
            String = format_unsigned_int(Flags, Width, Prec,
                10, Int, no, "")
        )
    ;
        Spec = x(Int),
        ( using_sprintf ->
            FormatStr = make_format(Flags, Width, Prec, int_length_modifer,
                "x"),
            String = native_format_int(FormatStr, Int)
        ;
            String = format_unsigned_int(Flags, Width, Prec,
                16, Int, no, "0x")
        )
    ;
        Spec = cX(Int),
        ( using_sprintf ->
            FormatStr = make_format(Flags, Width, Prec, int_length_modifer,
                "X"),
            String = native_format_int(FormatStr, Int)
        ;
            String = format_unsigned_int(Flags, Width, Prec,
                16, Int, no, "0X")
        )
    ;
        Spec = p(Int),
        ( using_sprintf ->
            FormatStr = make_format(Flags, Width, Prec, int_length_modifer,
                "p"),
            String = native_format_int(FormatStr, Int)
        ;
            String = format_unsigned_int(['#' | Flags], Width, Prec,
                16, Int, yes, "0x")
        )
    ;
        % Valid float conversion specifiers.
        Spec = e(Float),
        (
            is_finite(Float),
            using_sprintf
        ->
            FormatStr = make_format(Flags, Width, Prec, "", "e"),
            String = native_format_float(FormatStr, Float)
        ;
            String = format_scientific_number(Flags, spec_is_not_capital,
                Width, Prec, Float, "e")
        )
    ;
        Spec = cE(Float),
        (
            is_finite(Float),
            using_sprintf
        ->
            FormatStr = make_format(Flags, Width, Prec, "", "E"),
            String = native_format_float(FormatStr, Float)
        ;
            String = format_scientific_number(Flags, spec_is_capital,
                Width, Prec, Float, "E")
        )
    ;
        Spec = f(Float),
        (
            is_finite(Float),
            using_sprintf
        ->
            FormatStr = make_format(Flags, Width, Prec, "", "f"),
            String = native_format_float(FormatStr, Float)
        ;
            String = format_float(Flags, spec_is_not_capital, Width, Prec,
                Float)
        )
    ;
        Spec = cF(Float),
        (
            is_finite(Float),
            using_sprintf
        ->
            FormatStr = make_format(Flags, Width, Prec, "", "F"),
            String = native_format_float(FormatStr, Float)
        ;
            String = format_float(Flags, spec_is_capital, Width, Prec, Float)
        )
    ;
        Spec = g(Float),
        (
            is_finite(Float),
            using_sprintf
        ->
            FormatStr = make_format(Flags, Width, Prec, "", "g"),
            String = native_format_float(FormatStr, Float)
        ;
            String = format_scientific_number_g(Flags, spec_is_not_capital,
                Width, Prec, Float, "e")
        )
    ;
        Spec = cG(Float),
        (
            is_finite(Float),
            using_sprintf
        ->
            FormatStr = make_format(Flags, Width, Prec, "", "G"),
            String = native_format_float(FormatStr, Float)
        ;
            String = format_scientific_number_g(Flags, spec_is_capital,
                Width, Prec, Float, "E")
        )
    ;
        % Valid char conversion specifiers.
        Spec = c(Char),
        ( using_sprintf_for_char(Char) ->
            FormatStr = make_format(Flags, Width, Prec, "", "c"),
            String = native_format_char(FormatStr, Char)
        ;
            String = format_char(Flags, Width, Char)
        )
    ;
        % Valid string conversion specifiers.
        Spec = s(Str),
        (
            (
                using_sprintf,
                Flags = [],
                Width = no,
                Prec = no
            ;
                using_sprintf_for_string(Str)
            )
        ->
            FormatStr = make_format(Flags, Width, Prec, "", "s"),
            String = native_format_string(FormatStr, Str)
        ;
            String = format_string(Flags, Width, Prec, Str)
        )
    ;
        % Conversion specifier representing the "%" sign.
        Spec = percent,
        String = "%"
    ).

%-----------------------------------------------------------------------------%

    % Construct a format string.
    %
:- func make_format(list(char), maybe(int), maybe(int), string, string)
    = string.

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
:- func make_format_sprintf(list(char), maybe(int), maybe(int), string,
    string) = string.

make_format_sprintf(Flags, MaybeWidth, MaybePrec, LengthMod, Spec) = String :-
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
    String = string.append_list(["%", from_char_list(Flags),
        WidthStr, PrecPrefixStr, PrecStr, LengthMod, Spec]).

    % Construct a format string suitable to passing to .NET's formatting
    % functions.
    % XXX this code is not yet complete. We need to do a lot more work
    % to make this work perfectly.
    %
:- func make_format_dotnet(list(char), maybe(int), maybe(int), string,
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

:- type flags == list(char).
:- type maybe_width == maybe(int).
:- type maybe_precision == maybe(int).

    % Format a character (c).
    %
:- func format_char(flags, maybe_width, char) = string.

format_char(Flags, Width, Char) = String :-
    CharStr = string.char_to_string(Char),
    String = justify_string(Flags, Width, CharStr).

    % Format a string (s).
    %
:- func format_string(flags, maybe_width, maybe_precision, string) = string.

format_string(Flags, Width, Prec, OldStr) = NewStr :-
    (
        Prec = yes(NumChars),
        PrecStr = string.left_by_codepoint(OldStr, NumChars)
    ;
        Prec = no,
        PrecStr = OldStr
    ),
    NewStr = justify_string(Flags, Width, PrecStr).

:- func format_int(flags, maybe_width, maybe_precision, int) = string.

format_int(Flags, Width, Prec, Int) = String :-
    % Find the integer's absolute value, and take care of the special case
    % of precision zero with an integer of 0.
    (
        Int = 0,
        Prec = yes(0)
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
        Prec = yes(Precision),
        Precision > AbsIntStrLength
    ->
        PrecStr = string.pad_left(AbsIntStr, '0', Precision)
    ;
        PrecStr = AbsIntStr
    ),

    % Do we need to pad to the field width?
    (
        Width = yes(FieldWidth),
        FieldWidth > string.count_codepoints(PrecStr),
        member('0', Flags),
        \+ member('-', Flags),
        Prec = no
    ->
        FieldStr = string.pad_left(PrecStr, '0', FieldWidth - 1),
        ZeroPadded = yes
    ;
        FieldStr = PrecStr,
        ZeroPadded = no
    ),

    % Prefix with appropriate sign or zero padding.
    % The previous step has deliberately left room for this.
    SignedStr = add_int_prefix_if_needed(Flags, ZeroPadded, Int, FieldStr),
    String = justify_string(Flags, Width, SignedStr).

    % Format an unsigned int, unsigned octal, or unsigned hexadecimal
    % (u,o,x,X).
    %
:- func format_unsigned_int(flags, maybe_width, maybe_precision,
    int, int, bool, string) = string.

format_unsigned_int(Flags, Width, Prec, Base, Int, IsTypeP, Prefix) = String :-
    % Find the integer's absolute value, and take care of the special case
    % of precision zero with an integer of 0.
    (
        Int = 0,
        Prec = yes(0)
    ->
        AbsIntStr = ""
    ;
        Div = integer.pow(integer(2), integer(int.bits_per_int)),
        UnsignedInteger = integer(Int) mod Div,
        ( Base = 10 ->
            AbsIntStr0 = integer.to_string(UnsignedInteger)
        ; Base = 8 ->
            AbsIntStr0 = to_octal(UnsignedInteger)
        ; Prefix = "0x" ->
            AbsIntStr0 = to_hex(UnsignedInteger)
        ;
            AbsIntStr0 = to_capital_hex(UnsignedInteger)
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
        Prec = yes(Precision),
        Precision > AbsIntStrLength
    ->
        PrecStr = string.pad_left(AbsIntStr, '0', Precision)
    ;
        PrecStr = AbsIntStr
    ),

    % Do we need to increase the precision of an octal?
    (
        Base = 8,
        member('#', Flags),
        \+ string.prefix(PrecStr, "0")
    ->
        PrecModStr = append("0", PrecStr)
    ;
        PrecModStr = PrecStr
    ),

    % Do we need to pad to the field width?
    (
        Width = yes(FieldWidth),
        FieldWidth > string.count_codepoints(PrecModStr),
        member('0', Flags),
        \+ member('-', Flags),
        Prec = no
    ->
        % Do we need to make room for "0x" or "0X" ?
        (
            Base = 16,
            member('#', Flags),
            ( Int \= 0
            ; IsTypeP = yes
            )
        ->
            FieldStr = string.pad_left(PrecModStr, '0', FieldWidth - 2)
        ;
            FieldStr = string.pad_left(PrecModStr, '0', FieldWidth)
        )
    ;
        FieldStr = PrecModStr
    ),

    % Do we have to prefix "0x" or "0X"?
    (
        Base = 16,
        member('#', Flags),
        ( Int \= 0
        ; IsTypeP = yes
        )
    ->
        FieldModStr = Prefix ++ FieldStr
    ;
        FieldModStr = FieldStr
    ),

    String = justify_string(Flags, Width, FieldModStr).

    % Format a float (f)
    %
:- func format_float(flags, spec_case, maybe_width, maybe_precision, float)
    = string.

format_float(Flags, SpecCase, Width, Prec, Float) = NewFloat :-
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
            Prec = yes(Precision),
            PrecStr = change_precision(Precision, AbsStr)
        ;
            Prec = no,
            PrecStr = change_precision(6, AbsStr)
        ),

        % Do we need to remove the decimal point?
        (
            \+ member('#', Flags),
            Prec = yes(0)
        ->
            PrecStrLen = string.count_codepoints(PrecStr),
            PrecModStr = string.between(PrecStr, 0, PrecStrLen - 1)
        ;
            PrecModStr = PrecStr
        ),

        % Do we need to change field width?
        (
            Width = yes(FieldWidth),
            FieldWidth > string.count_codepoints(PrecModStr),
            member('0', Flags),
            \+ member('-', Flags)
        ->
            FieldStr = string.pad_left(PrecModStr, '0', FieldWidth - 1),
            ZeroPadded = yes
        ;
            FieldStr = PrecModStr,
            ZeroPadded = no
        ),
        % Finishing up.
        SignedStr = add_float_prefix_if_needed(Flags, ZeroPadded, Float,
            FieldStr)
    ),
    NewFloat = justify_string(Flags, Width, SignedStr).

    % Format a scientific number to a specified number of significant
    % figures (g,G)
    %
:- func format_scientific_number_g(flags, spec_case, maybe_width,
    maybe_precision, float, string) = string.

format_scientific_number_g(Flags, SpecCase, Width, Prec, Float, E)
        = NewFloat :-
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
            Prec = yes(Precision),
            ( Precision = 0 ->
                PrecStr = change_to_g_notation(AbsStr, 1, E, Flags)
            ;
                PrecStr = change_to_g_notation(AbsStr, Precision, E, Flags)
            )
        ;
            Prec = no,
            PrecStr = change_to_g_notation(AbsStr, 6, E, Flags)
        ),

        % Do we need to change field width?
        (
            Width = yes(FieldWidth),
            FieldWidth > string.count_codepoints(PrecStr),
            member('0', Flags),
            \+ member('-', Flags)
        ->
            FieldStr = string.pad_left(PrecStr, '0', FieldWidth - 1),
            ZeroPadded = yes
        ;
            FieldStr = PrecStr,
            ZeroPadded = no
        ),

        % Finishing up ..
        SignedStr = add_float_prefix_if_needed(Flags, ZeroPadded, Float,
            FieldStr)
    ),
    NewFloat = justify_string(Flags, Width, SignedStr).

    % Format a scientific number (e,E)
    %
:- func format_scientific_number(flags, spec_case, maybe_width,
    maybe_precision, float, string) = string.

format_scientific_number(Flags, SpecCase, Width, Prec, Float, E) = NewFloat :-
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
            Prec = yes(Precision),
            PrecStr = change_to_e_notation(AbsStr, Precision, E)
        ;
            Prec = no,
            PrecStr = change_to_e_notation(AbsStr, 6, E)
        ),

        % Do we need to remove the decimal point?
        (
            \+ member('#', Flags),
            Prec = yes(0)
        ->
            split_at_decimal_point(PrecStr, BaseStr, ExponentStr),
            PrecModStr = BaseStr ++ ExponentStr
        ;
            PrecModStr = PrecStr
        ),

        % Do we need to change field width?
        (
            Width = yes(FieldWidth),
            FieldWidth > string.count_codepoints(PrecModStr),
            member('0', Flags),
            \+ member('-', Flags)
        ->
            FieldStr = string.pad_left(PrecModStr, '0', FieldWidth - 1),
            ZeroPadded = yes
        ;
            FieldStr = PrecModStr,
            ZeroPadded = no
        ),

        % Finishing up ..
        SignedStr = add_float_prefix_if_needed(Flags, ZeroPadded, Float,
            FieldStr)
    ),
    NewFloat = justify_string(Flags, Width, SignedStr).

:- func add_int_prefix_if_needed(flags, bool, int, string) = string.

add_int_prefix_if_needed(Flags, ZeroPadded, Int, FieldStr) = SignedStr :-
    ( Int < 0 ->
        SignedStr = "-" ++ FieldStr
    ; member('+', Flags) ->
        SignedStr = "+" ++ FieldStr
    ; member(' ', Flags) ->
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
    ; member('+', Flags) ->
        SignedStr = "+" ++ FieldStr
    ; member(' ', Flags) ->
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

justify_string(Flags, Width, Str) = JustifiedStr :-
    (
        Width = yes(FWidth),
        FWidth > string.count_codepoints(Str)
    ->
        ( member('-', Flags) ->
            string.pad_right(Str, ' ', FWidth, JustifiedStr)
        ;
            string.pad_left(Str, ' ', FWidth, JustifiedStr)
        )
    ;
        JustifiedStr = Str
    ).

    % Convert an integer to an octal string.
    %
:- func to_octal(integer) = string.

to_octal(Num) = NumStr :-
    ( Num > integer(0) ->
        Rest = to_octal(Num // integer(8)),
        Rem = Num rem integer(8),
        RemStr = integer.to_string(Rem),
        NumStr = append(Rest, RemStr)
    ;
        NumStr = ""
    ).

    % Convert an integer to a hexadecimal string using a-f.
    %
:- func to_hex(integer) = string.

to_hex(Num) = NumStr :-
    ( Num > integer(0) ->
        Rest = to_hex(Num // integer(16)),
        Rem = Num rem integer(16),
        RemStr = get_hex_int(Rem),
        NumStr = append(Rest, RemStr)
    ;
        NumStr = ""
    ).

    % Convert an integer to a hexadecimal string using A-F.
    %
:- func to_capital_hex(integer) = string.

to_capital_hex(Num) = NumStr :-
    ( Num > integer(0) ->
        Rest = to_capital_hex(Num // integer(16)),
        Rem = Num rem integer(16),
        RemStr = get_capital_hex_int(Rem),
        NumStr = append(Rest, RemStr)
    ;
        NumStr = ""
    ).

    % Given a decimal integer, return the hexadecimal equivalent (using % a-f).
    %
:- func get_hex_int(integer) = string.

get_hex_int(Int) = HexStr :-
    ( Int < integer(10) ->
        HexStr = integer.to_string(Int)
    ; Int = integer(10) ->
        HexStr = "a"
    ; Int = integer(11) ->
        HexStr = "b"
    ; Int = integer(12) ->
        HexStr = "c"
    ; Int = integer(13) ->
        HexStr = "d"
    ; Int = integer(14) ->
        HexStr = "e"
    ;
        HexStr = "f"
    ).

    % Convert an integer to a hexadecimal string using A-F.
    %
:- func get_capital_hex_int(integer) = string.

get_capital_hex_int(Int) = HexStr :-
    ( Int < integer(10) ->
        HexStr = integer.to_string(Int)
    ; Int = integer(10) ->
        HexStr = "A"
    ; Int = integer(11) ->
        HexStr = "B"
    ; Int = integer(12) ->
        HexStr = "C"
    ; Int = integer(13) ->
        HexStr = "D"
    ; Int = integer(14) ->
        HexStr = "E"
    ;
        HexStr = "F"
    ).

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
        ( member('#', Flags) ->
            FormattedFloat = FormattedFloat0
        ;
            FormattedFloat = remove_trailing_zeros(FormattedFloat0)
        )
    ;
        % Float will be represented in scientific notation.
        % -------------------------------------------------
        UncheckedFloat = change_to_e_notation(Float, Prec - 1, E),

        % Do we need to remove trailing zeros?
        ( member('#', Flags) ->
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

format_nan(spec_is_capital) = "NAN".
format_nan(spec_is_not_capital) = "nan".

:- func format_infinity(float, spec_case) = string.

format_infinity(F, SpecCase) = String :-
    (
        SpecCase = spec_is_capital,
        String = ( if F < 0.0 then "-INFINITY" else "INFINITY" )
    ;
        SpecCase = spec_is_not_capital,
        String = ( if F < 0.0 then "-infinity" else "infinity" )
    ).

%-----------------------------------------------------------------------------%
