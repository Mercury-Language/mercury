%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2024-2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: format_call_errors.m.
% Author: zs.
%
% This module constructs any diagnostics we generate when format_call.m
% find that one of its semantic checks has failed.
%
%---------------------------------------------------------------------------%

:- module check_hlds.simplify.format_call_errors.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.prog_data.

:- import_module list.
:- import_module maybe.
:- import_module string.
:- import_module string.parse_util.

%---------------------------------------------------------------------------%

:- type maybe_warn_unknown_format
    --->    do_not_warn_unknown_format
    ;       warn_unknown_format.

:- func report_unknown_format_string(module_info, pred_id,
    maybe_warn_unknown_format, prog_context) = list(error_spec).

:- func report_unknown_format_values(module_info, pred_id,
    maybe_warn_unknown_format, prog_context) = list(error_spec).

%---------------------------------------------------------------------------%

:- func report_format_mismatch(module_info, pred_id, maybe({int, int, int}),
    string_format_error, list(string_format_error), prog_context)
    = list(error_spec).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_error_util.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.

:- import_module bool.
:- import_module char.
:- import_module int.

%---------------------------------------------------------------------------%

report_unknown_format_string(ModuleInfo, PredId, WarnUnknownFormat, Context)
        = Specs :-
    (
        WarnUnknownFormat = do_not_warn_unknown_format,
        Specs = []
    ;
        WarnUnknownFormat = warn_unknown_format,
        PredNameDotPieces = describe_one_pred_name(ModuleInfo,
            yes(color_subject), should_module_qualify, [suffix(".")], PredId),
        Pieces = [words("Warning:")] ++
            color_as_incorrect([words("unknown format string")]) ++
            [words("in call to")] ++ PredNameDotPieces ++ [nl],
        Phase = phase_simplify(report_in_any_mode),
        Spec = spec($pred, severity_warning, Phase, Context, Pieces),
        Specs = [Spec]
    ).

report_unknown_format_values(ModuleInfo, PredId, WarnUnknownFormat, Context)
        = Specs :-
    (
        WarnUnknownFormat = do_not_warn_unknown_format,
        Specs = []
    ;
        WarnUnknownFormat = warn_unknown_format,
        PredNameDotPieces = describe_one_pred_name(ModuleInfo,
            yes(color_subject), should_module_qualify, [suffix(".")], PredId),
        Pieces = [words("Warning:")] ++
            color_as_incorrect([words("unknown list of values"),
                words("to be formatted")]) ++
            [words("in call to")] ++ PredNameDotPieces ++ [nl],
        Phase = phase_simplify(report_in_any_mode),
        Spec = spec($pred, severity_warning, Phase, Context, Pieces),
        Specs = [Spec]
    ).

%---------------------------------------------------------------------------%

report_format_mismatch(ModuleInfo, PredId, MaybePos, HeadError, TailErrors,
        Context) = Specs :-
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, warn_known_bad_format_calls,
        WarnKnownBadFormatCalls),
    (
        WarnKnownBadFormatCalls = no,
        Specs = []
    ;
        WarnKnownBadFormatCalls = yes,
        (
            MaybePos = no,
            PredNameDotPieces = describe_one_pred_name(ModuleInfo,
                yes(color_subject), should_module_qualify,
                [suffix(".")], PredId)
        ;
            MaybePos = yes({Pos, ArgNumFS, ArgNumVL}),
            % XXX Any ideas for better wording?
            PredNameDotPieces =
                describe_one_pred_name(ModuleInfo, yes(color_subject),
                    should_module_qualify, [], PredId) ++
                [words("when considering the"),
                nth_fixed(Pos), words("entry in its"),
                pragma_decl("format_call"), words("declaration,"),
                words("which places the format string as the"),
                nth_fixed(ArgNumFS), words("argument, and"),
                words("the values list as the"),
                nth_fixed(ArgNumVL), words("argument"), suffix(".")]
        ),
        globals.lookup_bool_option(Globals, warn_all_format_string_errors,
            WarnAllFormatStringErrors),
        (
            WarnAllFormatStringErrors = no,
            ErrorPieces = string_format_error_to_pieces(HeadError)
        ;
            WarnAllFormatStringErrors = yes,
            ErrorPiecesLists = list.map(string_format_error_to_pieces,
                [HeadError | TailErrors]),
            list.condense(ErrorPiecesLists, ErrorPieces)
        ),
        Pieces = [words("Error: the format string")] ++
            color_as_incorrect([words("does not match")]) ++
            [words("the list of values to be formatted"),
            words("in call to")] ++ PredNameDotPieces ++ [nl] ++
            ErrorPieces,
        Phase = phase_simplify(report_in_any_mode),
        Spec = spec($pred, severity_warning, Phase, Context, Pieces),
        Specs = [Spec]
    ).

%---------------------------------------------------------------------------%
%
% The rest of this module turns string_format_errors into format_pieces
% for presentation to users. It shares its logic with the code in the tail
% section of library/string.parse_util.m, which does the same job,
% but returns a raw string.
%

:- func string_format_error_to_pieces(string_format_error)
    = list(format_piece).

string_format_error_to_pieces(Error) = Pieces :-
    % NOTE Please keep this in sync with string_format_error_to_msg.
    (
        Error = error_no_specifier(SpecNum, NumExtraPolyTypes),
        Pieces0 = [words("The")] ++
            color_as_subject([nth_fixed(SpecNum),
                words("conversion specifier")]),
        ( if NumExtraPolyTypes = 0 then
            Pieces = Pieces0 ++
                color_as_incorrect([words("is missing,")]) ++
                [words("along with")] ++
                color_as_incorrect([words("its input.")]) ++
                [nl]
        else if NumExtraPolyTypes = 1 then
            Pieces = Pieces0 ++
                color_as_incorrect([words("is missing.")]) ++
                [nl]
        else
            Pieces = Pieces0 ++
                color_as_incorrect([words("is missing,")]) ++
                [words("and")] ++
                color_as_incorrect([words("there are"),
                    int_fixed(NumExtraPolyTypes - 1),
                    words("extra inputs.")]) ++
                [nl]
        )
    ;
        Error = error_unknown_specifier(SpecNum, SpecChar),
        Pieces =
            [words("The")] ++
            color_as_subject([nth_fixed(SpecNum),
                words("conversion specifier")]) ++
            [words("uses the")] ++
            color_as_incorrect([words("unknown")] ++
                specifier_char_pieces(SpecChar) ++
                [suffix(".")]) ++
            [nl]
    ;
        Error = error_wrong_polytype(SpecNum, SpecChar, PolyKind),
        SpecCharStr = string.char_to_string(SpecChar),
        poly_kind_desc(PolyKind, AAn, PolyKindDesc),
        % There is a minor inconsistency here. Pieces0 talks about
        % the specifier *character*, while the pieces being appended to it
        % talk about *specifiers*, which contain both a percent sign and
        % the specifier character.
        %
        % Unfortunately, we can't change Pieces0 to talk about e.g.
        % the specifier "%s" instead of the specifier character "s",
        % because the actual specifier in the code could have modifiers
        % between the "%" and the "s". And deleting the "%" from e.g. "%s"
        % in the output of acceptable_specifier_chars_for_poly_kind_msg
        % would make harder for users to understand that part of
        % the diagnostic.
        Pieces0 =
            [words("The")] ++
            color_as_subject([nth_fixed(SpecNum),
                words("conversion specifier")]) ++
            [words("uses the specifier character")] ++
            color_as_inconsistent([quote(SpecCharStr), suffix(",")]) ++
            [words("but the corresponding input is"), words(AAn)] ++
            color_as_inconsistent([words(PolyKindDesc), suffix("."), nl]),
        acceptable_specifier_chars_for_poly_kind_msg(PolyKind, ValDesc,
            HeadSpec, TailSpecs),
        (
            TailSpecs = [],
            Pieces = Pieces0 ++
                [words("The only specifier applicable to"), words(ValDesc),
                words("is")] ++
                color_as_correct([quote(HeadSpec), suffix(".")]) ++
                [nl]
        ;
            TailSpecs = [_ | _],
            % The call to component_list_to_color_pieces does not add
            % a comma after the second-last item, the one before the "and".
            % This is a difference from string_format_error_to_msg,
            % but it is one we can live with.
            Pieces = Pieces0 ++
                [words("The specifiers applicable to"), words(ValDesc),
                words("are")] ++
                quote_list_to_color_pieces(color_correct, "and",
                    [suffix(".")], [HeadSpec | TailSpecs]) ++
                [nl]
        )
    ;
        Error = error_no_polytype(SpecNum, SpecChar),
        Pieces =
            [words("The")] ++
            color_as_subject([nth_fixed(SpecNum),
                words("conversion specifier"), suffix(",")]) ++
            [words("which uses")] ++
            specifier_char_pieces(SpecChar) ++ [suffix(","),
            words("is")] ++
            color_as_incorrect([words("missing its input.")]) ++
            [nl]
    ;
        (
            Error = error_nonint_star_width(SpecNum, PolyKind),
            Attr = "width"
        ;
            Error = error_nonint_star_prec(SpecNum, PolyKind),
            Attr = "precision"
        ),
        poly_kind_desc(PolyKind, AAn, PolyKindDesc),
        Pieces =
            [words("The")] ++
            color_as_subject([nth_fixed(SpecNum),
                words("conversion specifier")]) ++
            [words("says the"), words(Attr), words("is a runtime input,"),
            words("but the next input is"), words(AAn)] ++
            color_as_incorrect([words(PolyKindDesc), suffix(",")]) ++
            [words("not an")] ++
            color_as_correct([words("integer.")]) ++
            [nl]
    ;
        (
            Error = error_missing_star_width(SpecNum),
            Attr = "width"
        ;
            Error = error_missing_star_prec(SpecNum),
            Attr = "precision"
        ),
        Pieces =
            [words("The")] ++
            color_as_subject([nth_fixed(SpecNum),
                words("conversion specifier")]) ++
            [words("says the"), words(Attr), words("is a runtime input,"),
            words("but")] ++
            color_as_incorrect([words("there is no next input.")]) ++
            [nl]
    ;
        Error = error_extra_polytypes(SpecNum, NumExtraPolyTypes),
        ( if SpecNum = 1 then
            % Any inputs aren't "extra", since there is no other inputs
            % before them.
            Extra = []
        else
            Extra = [words("extra")]
        ),
        % XXX Wouldn't it be easier to understand this error if the message
        % said something like: "the format specifier expects SpecNum-1 inputs,
        % but there are NumExtraPolyTypes more inputs than that"?
        Pieces0 =
            [words("There is no"),  nth_fixed(SpecNum),
            words("conversion specifier,")],
        ( if NumExtraPolyTypes = 1 then
            Pieces = Pieces0 ++
                [words("but there is")] ++
                % We usually try not to color articles like "an",
                % but we color it in this case, because here it plays
                % the role of the word "one".
                color_as_incorrect([words("an")] ++
                    Extra ++ [words("input.")]) ++
                [nl]
        else
            Pieces = Pieces0 ++
                [words("but there are")] ++
                color_as_incorrect([int_name(NumExtraPolyTypes)] ++
                    Extra ++ [words("inputs.")]) ++
                [nl]
        )
    ).

:- func specifier_char_pieces(char) = list(format_piece).

specifier_char_pieces(SpecChar) = Pieces :-
    SpecCharStr = string.char_to_string(SpecChar),
    Pieces = [words("specifier character"), quote(SpecCharStr)].

:- pred poly_kind_desc(poly_kind::in, string::out, string::out) is det.

poly_kind_desc(poly_kind_char,   "a",  "character").
poly_kind_desc(poly_kind_str,    "a",  "string").
poly_kind_desc(poly_kind_int,    "an", "integer").
poly_kind_desc(poly_kind_int8,   "an", "8-bit integer").
poly_kind_desc(poly_kind_int16,  "a",  "16-bit integer").
poly_kind_desc(poly_kind_int32,  "a",  "32-bit integer").
poly_kind_desc(poly_kind_int64,  "a",  "64-bit integer").
poly_kind_desc(poly_kind_uint,   "an", "unsigned integer").
poly_kind_desc(poly_kind_uint8,  "an", "8-bit unsigned integer").
poly_kind_desc(poly_kind_uint16, "a",  "16-bit unsigned integer").
poly_kind_desc(poly_kind_uint32, "a",  "32-bit unsigned integer").
poly_kind_desc(poly_kind_uint64, "a",  "64-bit unsigned integer").
poly_kind_desc(poly_kind_float,  "a",  "float").

:- pred acceptable_specifier_chars_for_poly_kind_msg(poly_kind::in,
    string::out, string::out, list(string)::out) is det.

acceptable_specifier_chars_for_poly_kind_msg(Kind, ValDesc,
        HeadSpec, TailSpecs) :-
    (
        Kind = poly_kind_char,
        ValDesc = "characters",
        HeadSpec = "%c",
        TailSpecs = []
    ;
        Kind = poly_kind_str,
        ValDesc = "strings",
        HeadSpec = "%s",
        TailSpecs = []
    ;
        Kind = poly_kind_int,
        ValDesc = "ints",
        HeadSpec = "%d",
        TailSpecs = ["%i", "%o", "%x", "%X", "%u", "%p"]
    ;
        Kind = poly_kind_int8,
        ValDesc = "int8s",
        HeadSpec = "%d",
        TailSpecs = ["%i", "%o", "%x", "%X", "%u", "%p"]
    ;
        Kind = poly_kind_int16,
        ValDesc = "int16s",
        HeadSpec = "%d",
        TailSpecs = ["%i", "%o", "%x", "%X", "%u", "%p"]
    ;
        Kind = poly_kind_int32,
        ValDesc = "int32s",
        HeadSpec = "%d",
        TailSpecs = ["%i", "%o", "%x", "%X", "%u", "%p"]
    ;
        Kind = poly_kind_int64,
        ValDesc = "int64s",
        HeadSpec = "%d",
        TailSpecs = ["%i", "%o", "%x", "%X", "%u", "%p"]
    ;
        Kind = poly_kind_uint,
        ValDesc = "uints",
        HeadSpec = "%o",
        TailSpecs = ["%x", "%X", "%u", "%p"]
    ;
        Kind = poly_kind_uint8,
        ValDesc = "uint8s",
        HeadSpec = "%o",
        TailSpecs = ["%x", "%X", "%u", "%p"]
    ;
        Kind = poly_kind_uint16,
        ValDesc = "uint16s",
        HeadSpec = "%o",
        TailSpecs = ["%x", "%X", "%u", "%p"]
    ;
        Kind = poly_kind_uint32,
        ValDesc = "uint32s",
        HeadSpec = "%o",
        TailSpecs = ["%x", "%X", "%u", "%p"]
    ;
        Kind = poly_kind_uint64,
        ValDesc = "uint64s",
        HeadSpec = "%o",
        TailSpecs = ["%x", "%X", "%u", "%p"]
    ;
        Kind = poly_kind_float,
        ValDesc = "floats",
        HeadSpec = "%f",
        TailSpecs = ["%e", "%E", "%g", "%G"]
    ).

%---------------------------------------------------------------------------%
:- end_module check_hlds.simplify.format_call_errors.
%---------------------------------------------------------------------------%
