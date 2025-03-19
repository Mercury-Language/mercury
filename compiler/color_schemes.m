%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2024-2025 The Mercury Team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: color_schemes.m.
% Main author: zs.
%
% This module converts user-written specifications of color schemes
% to their internal representation.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module libs.color_schemes.
:- interface.

:- import_module libs.options.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.maybe_error.

:- import_module io.
:- import_module list.
:- import_module maybe.

%---------------------------------------------------------------------------%

    % record_color_scheme_in_options(Source, SchemeName, Specs,
    %   !OptionTable, !IO):
    %
    % Given a name we got from Source, if it is the name of a recognized
    % color scheme, then set update !OptionTable to record the colors
    % it selects in a way that allows convert_color_spec_options to retrieve
    % those colors, and set ErrorSpecs to the empty list. If it is not the name
    % of a recognized color scheme, then return a diagnostic in Specs,
    % and leave !OptionTable unchanged. If it contains a recognized color
    % scheme but this leaves some color roles unassigned, then record a
    % report about this in a mutable in write_error_specs.m.
    %
    % This predicate is intended to be used by handle_options.m during
    % the creation of the first globals structure. Its result has to be
    % recorded in an option_table, not a globals structure, for the reason
    % explained by the comment below on convert_color_spec_options.
    %
:- pred record_color_scheme_in_options(list(format_piece)::in,
    string::in, list(error_spec)::out, option_table::in, option_table::out,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- type color_spec
    --->    color_8bit(uint8)
    ;       color_24bit(uint8, uint8, uint8).
            % Red, Green, Blue.

:- type color_specs
    --->    color_specs(
                color_spec_subject          ::  maybe(color_spec),
                color_spec_correct          ::  maybe(color_spec),
                color_spec_incorrect        ::  maybe(color_spec),
                color_spec_inconsistent     ::  maybe(color_spec),
                color_spec_hint             ::  maybe(color_spec)
            ).

    % This function is intended to be used by write_error_spec.m
    % to find out the colors it should use in the diagnostics it writes out.
    % Its jobs is to convert the values of the options which record
    % the color shades write_error_spec.m should use for each color name
    % (color_subject, color_correct, color_incorrect, color_inconsistent, and
    % color_hint). These colors could have been chosen by the user, in which
    % case record_color_scheme_in_options will have checked their
    % well-formedness, or they could be the defaults, which we use in the
    % absence of a choice by the user.
    %
    % This function takes as input an option_table, because it cannot take
    % a full globals structure. The reason for that is that the process
    % of creating the very first globals structure itself may detect errors,
    % and we want to use the facilities of write_error_spec.m to print
    % the diagnostics we generate for those errors. We don't have a globals
    % available then, but we do have an option_table.
    %
:- func convert_color_spec_options(option_table) = maybe1(color_specs).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.write_error_spec.

:- import_module bool.
:- import_module char.
:- import_module getopt.
:- import_module int.
:- import_module map.
:- import_module string.
:- import_module uint8.

%---------------------------------------------------------------------------%

record_color_scheme_in_options(Source, SchemeName, Specs, !OptionTable, !IO) :-
    % Any error message we generate cannot use color, because the existence
    % of such errors would mean that we cannot set up the colors we would
    % want to use for that. :-(
    ( if
        (
            ( SchemeName = "dark16"
            ; SchemeName = "darkmode16"
            ),
            Subject =       "14",   % bright cyan       (by default)
            Correct =       "10",   % bright green      (by default)
            Incorrect =     "9",    % bright red        (by default)
            Inconsistent =  "11",   % bright yellow     (by default)
            Hint =          "13"    % bright magenta    (by default)
        ;
            ( SchemeName = "dark256"
            ; SchemeName = "darkmode256"
            ),
            Subject =       "111",  % #87afff
            Correct =       "107",  % #87af5f
            Incorrect =     "174",  % #d78787
            Inconsistent =  "179",  % #d7af5f
            Hint =          "140"   % #af87df
        ;
            ( SchemeName = "dark"
            ; SchemeName = "darkmode"
            ),
            Subject =       "#7ca2ee",
            Correct =       "#89b260",
            Incorrect =     "#e48384",
            Inconsistent =  "#d2a74c",
            Hint =          "#b68fde"
        ;
            ( SchemeName = "light16"
            ; SchemeName = "lightmode16"
            ),
            Subject =       "6",    % normal cyan       (by default)
            Correct =       "2",    % normal green      (by default)
            Incorrect =     "9",    % bright red        (by default)
            Inconsistent =  "3",    % normal yellow     (by default)
            Hint =          "5"     % normal magenta    (by default)
        ;
            ( SchemeName = "light256"
            ; SchemeName = "lightmode256"
            ),
            Subject =       "27",   % #005fff
            Correct =       "28",   % #008700
            Incorrect =     "160",  % #d70000
            Inconsistent =  "166",  % #d75f00
            Hint =          "92"    % #8700d7
        ;
            ( SchemeName = "light"
            ; SchemeName = "lightmode"
            ),
            Subject =       "#1b65ef",
            Correct =       "#008e00",
            Incorrect =     "#d41009",
            Inconsistent =  "#cf5600",
            Hint =          "#903fd6"
        )
    then
        map.set(set_color_subject, string(Subject), !OptionTable),
        map.set(set_color_correct, string(Correct), !OptionTable),
        map.set(set_color_incorrect, string(Incorrect), !OptionTable),
        map.set(set_color_inconsistent, string(Inconsistent), !OptionTable),
        map.set(set_color_hint, string(Hint), !OptionTable),
        Specs = []
    else if
        string.remove_prefix("specified@", SchemeName, SettingsStr)
    then
        Settings = string.split_at_char(':', SettingsStr),
        MaybeColorStrs0 = maybe_color_strings(no, no, no, no, no),
        parse_color_specifications(Source, Settings,
            MaybeColorStrs0, MaybeColorStrs, [], SettingSpecs),
        (
            SettingSpecs = [],
            MaybeColorStrs = maybe_color_strings(MaybeSubject, MaybeCorrect,
                MaybeIncorrect, MaybeInconsistent, MaybeHint),
            record_maybe_color(set_color_subject, MaybeSubject,
                !OptionTable),
            record_maybe_color(set_color_correct, MaybeCorrect,
                !OptionTable),
            record_maybe_color(set_color_incorrect, MaybeIncorrect,
                !OptionTable),
            record_maybe_color(set_color_inconsistent, MaybeInconsistent,
                !OptionTable),
            record_maybe_color(set_color_hint, MaybeHint,
                !OptionTable),
            (
                MaybeSubject = no,
                MissingRoles1 = [words("subject")]
            ;
                MaybeSubject = yes(_),
                MissingRoles1 = []
            ),
            (
                MaybeCorrect = no,
                MissingRoles2 = MissingRoles1 ++ [words("correct")]
            ;
                MaybeCorrect = yes(_),
                MissingRoles2 = MissingRoles1
            ),
            (
                MaybeIncorrect = no,
                MissingRoles3 = MissingRoles2 ++ [words("incorrect")]
            ;
                MaybeIncorrect = yes(_),
                MissingRoles3 = MissingRoles2
            ),
            (
                MaybeInconsistent = no,
                MissingRoles4 = MissingRoles3 ++ [words("inconsistent")]
            ;
                MaybeInconsistent = yes(_),
                MissingRoles4 = MissingRoles3
            ),
            (
                MaybeHint = no,
                MissingRoles = MissingRoles4 ++ [words("hint")]
            ;
                MaybeHint = yes(_),
                MissingRoles = MissingRoles4
            ),
            (
                MissingRoles = [],
                Specs = []
            ;
                MissingRoles = [_ | _],
                ColorColors = choose_number(MissingRoles, "color", "colors"),
                RoleRoles = choose_number(MissingRoles, "role", "roles"),
                Pieces = [words("The value of")] ++ Source ++
                    [words("does not specify the"), words(ColorColors),
                    words("to use for the"), words(RoleRoles), words("of")] ++
                    piece_list_to_pieces("and", MissingRoles) ++
                    [suffix("."), nl],
                Msg = no_ctxt_msg(Pieces),
                InformSpec = conditional_spec($pred,
                    inform_incomplete_color_scheme, yes,
                    severity_informational, phase_options, [Msg]),
                record_bad_color_scheme(InformSpec, !IO),
                Specs = []
            )
        ;
            SettingSpecs = [_ | _],
            Specs = SettingSpecs
        )
    else
        Pieces = [words("Error in the value of")] ++ Source ++
            [suffix(":"), quote(SchemeName), words("is not the name"),
            words("of a recognized color scheme."), nl],
        Specs = [no_ctxt_spec($pred, severity_error, phase_options, Pieces)]
    ).

:- type maybe_color_strings
    --->    maybe_color_strings(
                mcs_subject             ::  maybe(string),
                mcs_correct             ::  maybe(string),
                mcs_incorrect           ::  maybe(string),
                mcs_inconsistent        ::  maybe(string),
                mcs_hint                ::  maybe(string)
            ).

:- pred parse_color_specifications(list(format_piece)::in, list(string)::in,
    maybe_color_strings::in, maybe_color_strings::out,
    list(error_spec)::in, list(error_spec)::out) is det.

parse_color_specifications(_, [], !MaybeColorStrs, !Specs).
parse_color_specifications(Source, [Setting | Settings],
        !MaybeColorStrs, !Specs) :-
    ( if
        [Name, Value] = string.split_at_char('=', Setting),
        ( Name = "subject"
        ; Name = "correct"
        ; Name = "incorrect"
        ; Name = "inconsistent"
        ; Name = "hint"
        )
    then
        Result = is_string_a_color_spec(Value),
        (
            Result = is_color(_Color),
            (
                Name = "subject",
                !MaybeColorStrs ^ mcs_subject := yes(Value)
            ;
                Name = "correct",
                !MaybeColorStrs ^ mcs_correct := yes(Value)
            ;
                Name = "incorrect",
                !MaybeColorStrs ^ mcs_incorrect := yes(Value)
            ;
                Name = "inconsistent",
                !MaybeColorStrs ^ mcs_inconsistent := yes(Value)
            ;
                Name = "hint",
                !MaybeColorStrs ^ mcs_hint := yes(Value)
            )
        ;
            Result = is_not_color(WhyNot),
            Spec = report_why_not_color(Source, Value, WhyNot),
            !:Specs = [Spec | !.Specs]
        )
    else
        Pieces = [words("Error in")] ++ Source ++
            [suffix(":"), words("expected a string of the form"),
            quote("role=color"), words("where"),
            quote("role"), words("is one of"),
            quote("subject"), suffix(","),
            quote("correct"), suffix(","),
            quote("incorrect"), suffix(","),
            quote("inconsistent"), words("and"),
            quote("hint"), suffix(","),
            words("got"), quote(Setting), suffix("."), nl],
        Spec = no_ctxt_spec($pred, severity_error, phase_options, Pieces),
        !:Specs = [Spec | !.Specs]
    ),
    parse_color_specifications(Source, Settings,
        !MaybeColorStrs, !Specs).

:- pred record_maybe_color(option::in, maybe(string)::in,
    option_table::in, option_table::out) is det.

record_maybe_color(Option, MaybeColorStr, !OptionTable) :-
    (
        MaybeColorStr = no,
        ColorStr = ""
    ;
        MaybeColorStr = yes(ColorStr)
    ),
    map.set(Option, string(ColorStr), !OptionTable).

%---------------------%

convert_color_spec_options(OptionTable) = MaybeColorSpecs :-
    getopt.lookup_string_option(OptionTable,
        set_color_subject, OptSubject),
    getopt.lookup_string_option(OptionTable,
        set_color_correct, OptCorrect),
    getopt.lookup_string_option(OptionTable,
        set_color_incorrect, OptIncorrect),
    getopt.lookup_string_option(OptionTable,
        set_color_inconsistent, OptInconsistent),
    getopt.lookup_string_option(OptionTable,
        set_color_hint, OptHint),
    % There is no simple way to convert each option to its name.
    MaybeMaybeSubject =
        convert_color_spec_option("--set-color-subject", OptSubject),
    MaybeMaybeCorrect =
        convert_color_spec_option("--set-color-correct", OptCorrect),
    MaybeMaybeIncorrect =
        convert_color_spec_option("--set-color-incorrect", OptIncorrect),
    MaybeMaybeInconsistent =
        convert_color_spec_option("--set-color-inconsistent", OptInconsistent),
    MaybeMaybeHint =
        convert_color_spec_option("--set-color-hint", OptHint),
    ( if
        MaybeMaybeSubject = ok1(MaybeSubject),
        MaybeMaybeCorrect = ok1(MaybeCorrect),
        MaybeMaybeIncorrect = ok1(MaybeIncorrect),
        MaybeMaybeInconsistent = ok1(MaybeInconsistent),
        MaybeMaybeHint = ok1(MaybeHint)
    then
        ColorSpecs = color_specs(MaybeSubject, MaybeCorrect, MaybeIncorrect,
            MaybeInconsistent, MaybeHint),
        MaybeColorSpecs = ok1(ColorSpecs)
    else
        Specs =
            get_any_errors1(MaybeMaybeSubject) ++
            get_any_errors1(MaybeMaybeCorrect) ++
            get_any_errors1(MaybeMaybeIncorrect) ++
            get_any_errors1(MaybeMaybeInconsistent) ++
            get_any_errors1(MaybeMaybeHint),
        MaybeColorSpecs = error1(Specs)
    ).

:- func convert_color_spec_option(string, string) = maybe1(maybe(color_spec)).

convert_color_spec_option(OptionName, OptionValue) = MaybeMaybeColorSpec :-
    % If/when we want to support 24-bit color, or indeed any form of
    % color specification beyond 8-bit, we would do it here.
    % (The options that specify colors are strings, not integers,
    % specifically to make it possible to specify 24-bit colors
    % as strings of the form "R-G-B".)
    ( if OptionValue = "" then
        MaybeColor = no,
        MaybeMaybeColorSpec = ok1(MaybeColor)
    else
        ColorResult = is_string_a_color_spec(OptionValue),
        (
            ColorResult = is_color(Color),
            MaybeMaybeColorSpec = ok1(yes(Color))
        ;
            ColorResult = is_not_color(WhyNot),
            Source = [words("the argument of"), fixed(OptionName)],
            Spec = report_why_not_color(Source, OptionValue, WhyNot),
            MaybeMaybeColorSpec = error1([Spec])
        )
    ).

:- type is_color_result
    --->    is_color(color_spec)
    ;       is_not_color(why_not_color).

:- type why_not_color
    --->    wnc_int_outside_range(int, int)
            % The range the int is outside of, both inclusive.
    ;       wnc_afterhash_length(int)
    ;       wnc_afterhash_nondigits
    ;       wnc_unknown_format.

:- func is_string_a_color_spec(string) = is_color_result.

is_string_a_color_spec(Str) = Result :-
    ( if standard_color_name(Str, Color) then
        Result = is_color(Color)
    else if string.remove_prefix("#", Str, StrAfterHash) then
        string.to_char_list(StrAfterHash, CharsAfterHash),
        ( if CharsAfterHash = [RH, RL, GH, GL, BH, BL] then
            ( if
                char.hex_digit_to_int(RH, ValRH),
                char.hex_digit_to_int(RL, ValRL),
                char.hex_digit_to_int(GH, ValGH),
                char.hex_digit_to_int(GL, ValGL),
                char.hex_digit_to_int(BH, ValBH),
                char.hex_digit_to_int(BL, ValBL)
            then
                ValR = uint8.det_from_int(ValRH * 16 + ValRL),
                ValG = uint8.det_from_int(ValGH * 16 + ValGL),
                ValB = uint8.det_from_int(ValBH * 16 + ValBL),
                Color = color_24bit(ValR, ValG, ValB),
                Result = is_color(Color)
            else
                Result = is_not_color(wnc_afterhash_nondigits)
            )
        else
            list.length(CharsAfterHash, NumCharsAfterHash),
            WhyNot = wnc_afterhash_length(NumCharsAfterHash),
            Result = is_not_color(WhyNot)
        )
    else if string.to_int(Str, N) then
        % The value range we want is exactly the range of uint8s.
        ( if uint8.from_int(N, ColorNum) then
            Color = color_8bit(ColorNum),
            Result = is_color(Color)
        else
            Result = is_not_color(wnc_int_outside_range(0, 255))
        )
    else
        Result = is_not_color(wnc_unknown_format)
    ).

:- pred standard_color_name(string::in, color_spec::out) is semidet.

standard_color_name("black",            color_8bit(0u8)).
standard_color_name("red",              color_8bit(1u8)).
standard_color_name("green",            color_8bit(2u8)).
standard_color_name("yellow",           color_8bit(3u8)).
standard_color_name("blue",             color_8bit(4u8)).
standard_color_name("magenta",          color_8bit(5u8)).
standard_color_name("cyan",             color_8bit(6u8)).
standard_color_name("white",            color_8bit(7u8)).

standard_color_name("gray",             color_8bit(8u8)).
standard_color_name("grey",             color_8bit(8u8)).
standard_color_name("bright-black",     color_8bit(8u8)).
standard_color_name("bright black",     color_8bit(8u8)).
standard_color_name("bright-red",       color_8bit(9u8)).
standard_color_name("bright red",       color_8bit(9u8)).
standard_color_name("bright-green",     color_8bit(10u8)).
standard_color_name("bright green",     color_8bit(10u8)).
standard_color_name("bright-yellow",    color_8bit(11u8)).
standard_color_name("bright yellow",    color_8bit(11u8)).
standard_color_name("bright-blue",      color_8bit(12u8)).
standard_color_name("bright blue",      color_8bit(12u8)).
standard_color_name("bright-magenta",   color_8bit(13u8)).
standard_color_name("bright magenta",   color_8bit(13u8)).
standard_color_name("bright-cyan",      color_8bit(14u8)).
standard_color_name("bright cyan",      color_8bit(14u8)).
standard_color_name("bright-white",     color_8bit(15u8)).
standard_color_name("bright white",     color_8bit(15u8)).

:- func report_why_not_color(list(format_piece), string, why_not_color)
    = error_spec.

report_why_not_color(Source, Value, WhyNot) = Spec :-
    (
        WhyNot = wnc_int_outside_range(Min, Max),
        Pieces = [words("Error in")] ++ Source ++ [suffix(":"),
            quote(Value), words("is outside the range"),
            int_fixed(Min), words("to"), int_fixed(Max), suffix("."), nl],
        Spec = no_ctxt_spec($pred, severity_error, phase_options, Pieces)
    ;
        WhyNot = wnc_afterhash_length(Len),
        Pieces = [words("Error in")] ++ Source ++ [suffix(":"),
            words("expected six hexadecimal digits after the # sign, got"),
            int_name(Len), words("characters."), nl],
        Spec = no_ctxt_spec($pred, severity_error, phase_options, Pieces)
    ;
        WhyNot = wnc_afterhash_nondigits,
        Pieces = [words("Error in")] ++ Source ++ [suffix(":"),
            words("expected all six characters after the # sign"),
            words("to be hexadecimal digits, but some are not."), nl],
        Spec = no_ctxt_spec($pred, severity_error, phase_options, Pieces)
    ;
        WhyNot = wnc_unknown_format,
        Pieces = [words("Error in")] ++ Source ++ [suffix(":"), quote(Value),
            words("is not the name of a known color,"),
            words("the #RRGGBB specification of a 24 bit color, or"),
            words("a decimal integer between 0 and 255"),
            words("specifying an 8 bit color."), nl],
        Spec = no_ctxt_spec($pred, severity_error, phase_options, Pieces)
    ).

%---------------------------------------------------------------------------%
:- end_module libs.color_schemes.
%---------------------------------------------------------------------------%
