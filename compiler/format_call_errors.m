%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2024 The Mercury team.
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
        Pieces = [words("Error:")] ++
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
        Pieces = [words("Error:")] ++
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
            PredNameColonPieces = describe_one_pred_name(ModuleInfo,
                yes(color_subject), should_module_qualify,
                [suffix(":")], PredId)
        ;
            MaybePos = yes({Pos, ArgNumFS, ArgNumVL}),
            % XXX Any ideas for better wording?
            PredNameColonPieces =
                describe_one_pred_name(ModuleInfo, yes(color_subject),
                    should_module_qualify, [], PredId) ++
                [words("when considering the"),
                nth_fixed(Pos), words("entry in its"),
                pragma_decl("format_call"), words("declaration,"),
                words("which places the format string as the"),
                nth_fixed(ArgNumFS), words("argument, and"),
                words("the values list as the"),
                nth_fixed(ArgNumVL), words("argument"), suffix(":")]
        ),
        globals.lookup_bool_option(Globals, warn_only_one_format_string_error,
            WarnOnlyOneFormatStringError),
        (
            WarnOnlyOneFormatStringError = yes,
            ErrorPieces = [string_format_error_to_words(HeadError)]
        ;
            WarnOnlyOneFormatStringError = no,
            ErrorPieces = [string_format_error_to_words(HeadError) |
                list.map(string_format_error_to_words, TailErrors)]
        ),
        Pieces = [words("Error: the format string"),
            words("does not match the list of values to be formatted"),
            words("in call to")] ++ PredNameColonPieces ++ [nl] ++
            ErrorPieces,
        Phase = phase_simplify(report_in_any_mode),
        Spec = spec($pred, severity_warning, Phase, Context, Pieces),
        Specs = [Spec]
    ).

:- func string_format_error_to_words(string_format_error) = format_piece.

string_format_error_to_words(Error) =
    words(string_format_error_to_msg(Error)).

%---------------------------------------------------------------------------%
:- end_module check_hlds.simplify.format_call_errors.
%---------------------------------------------------------------------------%
