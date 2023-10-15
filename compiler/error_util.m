%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1997-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: error_util.m.
% Main author: zs.
%
% This module contains utility predicates and functions operating on
% error_specs.
%
%---------------------------------------------------------------------------%

:- module parse_tree.error_util.
:- interface.

:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module parse_tree.error_spec.

:- import_module bool.
:- import_module list.
:- import_module maybe.

%---------------------------------------------------------------------------%

    % Would trying to print the given spec result in any output?
    % The answer can be "no" if all parts of the error_spec are
    % under a condition that happens to be false.
    %
:- pred does_spec_print_anything(globals::in, error_spec::in) is semidet.

%---------------------------------------------------------------------------%

    % Return the worst of two actual severities.
    %
:- func worst_severity(actual_severity, actual_severity)
    = actual_severity.

    % Compute the actual severity of a message with the given severity
    % (if it actually prints anything).
    %
:- func actual_error_severity(globals, error_severity)
    = maybe(actual_severity).
:- func actual_error_severity_opt_table(option_table, error_severity)
    = maybe(actual_severity).

    % Compute the actual severity of an error_spec
    % (if it actually prints anything).
    %
:- func actual_spec_severity(globals, error_spec) = maybe(actual_severity).
:- func actual_spec_severity_opt_table(option_table, error_spec) =
    maybe(actual_severity).

    % Compute the worst actual severity (if any) occurring in a list of
    % error_specs.
    %
:- func worst_severity_in_specs(globals, list(error_spec))
    = maybe(actual_severity).
:- func worst_severity_in_specs_opt_table(option_table, list(error_spec))
    = maybe(actual_severity).

    % Return `yes' if the given list contains error_specs whose actual severity
    % is actual_severity_error.
    %
:- func contains_errors(globals, list(error_spec)) = bool.
:- func contains_errors_option_table(option_table, list(error_spec)) = bool.

    % Return `yes' if the given list contains error_specs whose actual severity
    % is actual_severity_error or actual_severity_warning.
    %
:- func contains_errors_and_or_warnings(globals, list(error_spec)) = bool.
:- func contains_errors_and_or_warnings_opt_table(option_table,
    list(error_spec)) = bool.

    % If --halt-at-warn is not set, then return `yes' if the given list
    % contains error_specs whose actual severity is actual_severity_error.
    %
    % If --halt-at-warn is set, then return `yes' if the given list
    % contains error_specs whose actual severity is either
    % actual_severity_error or actual_severity_warning.
    %
:- func contains_errors_or_warnings_treated_as_errors(globals,
    list(error_spec)) = bool.
:- func contains_errors_or_warnings_treated_as_errors_opt_table(option_table,
    list(error_spec)) = bool.

%---------------------------------------------------------------------------%

    % Delete all the given error_specs, which are supposed to have been
    % gathered during the process that generates the contents of an interface
    % file, if halt_at_invalid_interface is not set.
    %
    % Even if it is set, delete any conditional error specs whose conditions
    % are false.
    %
:- pred filter_interface_generation_specs(globals::in,
    list(error_spec)::in, list(error_spec)::out) is det.

%---------------------------------------------------------------------------%
%
% The error_spec_accumulator type can be used to accumulate errors for
% multiple modes of a predicate. accumulate_error_specs_for_proc will
% eliminate warnings that should only be reported if they occur in every mode,
% but don't occur in every mode.

:- type error_spec_accumulator.

:- func init_error_spec_accumulator = error_spec_accumulator.

:- pred accumulate_error_specs_for_proc(list(error_spec)::in,
    error_spec_accumulator::in, error_spec_accumulator::out) is det.

:- func error_spec_accumulator_to_list(error_spec_accumulator) =
    list(error_spec).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module getopt.
:- import_module pair.
:- import_module set.
:- import_module term_context.

%---------------------------------------------------------------------------%

does_spec_print_anything(Globals, Spec) :-
    does_spec_print_anything_2(Globals, Spec) = yes.

:- func does_spec_print_anything_2(globals, error_spec) = bool.

does_spec_print_anything_2(Globals, Spec) = Prints :-
    (
        ( Spec = simplest_spec(_, _, _, _, _)
        ; Spec = simplest_no_context_spec(_, _, _, _)
        ),
        Prints = yes
    ;
        Spec = error_spec(_, _, _, Msgs),
        PrintsList = list.map(does_msg_print_anything(Globals), Msgs),
        bool.or_list(PrintsList, Prints)
    ;
        Spec = conditional_spec(_, Option, MatchValue, _, _, Msgs),
        globals.lookup_bool_option(Globals, Option, OptionValue),
        ( if OptionValue = MatchValue then
            PrintsList = list.map(does_msg_print_anything(Globals), Msgs),
            bool.or_list(PrintsList, Prints)
        else
            Prints = no
        )
    ).

:- func does_msg_print_anything(globals, error_msg) = bool.

does_msg_print_anything(Globals, Msg) = Prints :-
    (
        ( Msg = simplest_msg(_, _)
        ; Msg = simplest_no_context_msg(_)
        ),
        Prints = yes
    ;
        ( Msg = simple_msg(_, MsgComponents)
        ; Msg = error_msg(_, _, _, MsgComponents)
        ),
        PrintsList = list.map(does_msg_component_print_anything(Globals),
            MsgComponents),
        bool.or_list(PrintsList, Prints)
    ).

:- func does_msg_component_print_anything(globals, error_msg_component) = bool.

does_msg_component_print_anything(Globals, MsgComponent) = Prints :-
    (
        ( MsgComponent = always(_)
        ; MsgComponent = verbose_only(_, _)
        ; MsgComponent = verbose_and_nonverbose(_, _)
        ; MsgComponent = print_anything(_)
        ),
        Prints = yes
    ;
        MsgComponent = option_is_set(Option, MatchValue, MsgComponents),
        globals.lookup_bool_option(Globals, Option, OptionValue),
        ( if OptionValue = MatchValue then
            PrintsList = list.map(does_msg_component_print_anything(Globals),
                MsgComponents),
            bool.or_list(PrintsList, Prints)
        else
            Prints = no
        )
    ).

%---------------------------------------------------------------------------%

worst_severity(actual_severity_error, actual_severity_error) =
    actual_severity_error.
worst_severity(actual_severity_error, actual_severity_warning) =
    actual_severity_error.
worst_severity(actual_severity_error, actual_severity_informational) =
    actual_severity_error.
worst_severity(actual_severity_warning, actual_severity_error) =
    actual_severity_error.
worst_severity(actual_severity_warning, actual_severity_warning) =
    actual_severity_warning.
worst_severity(actual_severity_warning, actual_severity_informational) =
    actual_severity_warning.
worst_severity(actual_severity_informational, actual_severity_error) =
    actual_severity_error.
worst_severity(actual_severity_informational, actual_severity_warning) =
    actual_severity_warning.
worst_severity(actual_severity_informational, actual_severity_informational) =
    actual_severity_informational.

%---------------------%

actual_error_severity(Globals, Severity) = MaybeActual :-
    globals.get_options(Globals, OptionTable),
    MaybeActual = actual_error_severity_opt_table(OptionTable, Severity).

actual_error_severity_opt_table(OptionTable, Severity) = MaybeActual :-
    (
        Severity = severity_error,
        MaybeActual = yes(actual_severity_error)
    ;
        Severity = severity_warning,
        MaybeActual = yes(actual_severity_warning)
    ;
        Severity = severity_informational,
        MaybeActual = yes(actual_severity_informational)
    ;
        Severity = severity_conditional(Option, MatchValue,
            Match, MaybeNoMatch),
        getopt.lookup_bool_option(OptionTable, Option, Value),
        ( if Value = MatchValue then
            MaybeActual = actual_error_severity_opt_table(OptionTable, Match)
        else
            (
                MaybeNoMatch = no,
                MaybeActual = no
            ;
                MaybeNoMatch = yes(NoMatch),
                MaybeActual =
                    actual_error_severity_opt_table(OptionTable, NoMatch)
            )
        )
    ).

%---------------------%

actual_spec_severity(Globals, Spec) = MaybeSeverity :-
    globals.get_options(Globals, OptionTable),
    MaybeSeverity = actual_spec_severity_opt_table(OptionTable, Spec).

actual_spec_severity_opt_table(OptionTable, Spec) = MaybeSeverity :-
    (
        ( Spec = error_spec(_, Severity, _, _)
        ; Spec = simplest_spec(_, Severity, _, _, _)
        ; Spec = simplest_no_context_spec(_, Severity, _, _)
        ),
        MaybeSeverity = actual_error_severity_opt_table(OptionTable, Severity)
    ;
        Spec = conditional_spec(_, Option, MatchValue, Severity, _, _),
        getopt.lookup_bool_option(OptionTable, Option, OptionValue),
        ( if OptionValue = MatchValue then
            MaybeSeverity =
                actual_error_severity_opt_table(OptionTable, Severity)
        else
            MaybeSeverity = no
        )
    ).

%---------------------%

worst_severity_in_specs(Globals, Specs) = MaybeWorst :-
    globals.get_options(Globals, OptionTable),
    worst_severity_in_specs_loop(OptionTable, Specs, no, MaybeWorst).

worst_severity_in_specs_opt_table(OptionTable, Specs) = MaybeWorst :-
    worst_severity_in_specs_loop(OptionTable, Specs, no, MaybeWorst).

:- pred worst_severity_in_specs_loop(option_table::in, list(error_spec)::in,
    maybe(actual_severity)::in, maybe(actual_severity)::out) is det.

worst_severity_in_specs_loop(_OptionTable, [], !MaybeWorst).
worst_severity_in_specs_loop(OptionTable, [Spec | Specs], !MaybeWorst) :-
    MaybeThis = actual_spec_severity_opt_table(OptionTable, Spec),
    (
        !.MaybeWorst = no,
        !:MaybeWorst = MaybeThis
    ;
        !.MaybeWorst = yes(Worst),
        (
            MaybeThis = no
        ;
            MaybeThis = yes(This),
            !:MaybeWorst = yes(worst_severity(Worst, This))
        )
    ),
    worst_severity_in_specs_loop(OptionTable, Specs, !MaybeWorst).

%---------------------%

contains_errors(Globals, Specs) = Errors :-
    globals.get_options(Globals, OptionTable),
    Errors = contains_errors_option_table(OptionTable, Specs).

contains_errors_option_table(OptionTable, Specs) = Errors :-
    MaybeWorstActual = worst_severity_in_specs_opt_table(OptionTable, Specs),
    (
        MaybeWorstActual = no,
        Errors = no
    ;
        MaybeWorstActual = yes(WorstActual),
        (
            WorstActual = actual_severity_error,
            Errors = yes
        ;
            ( WorstActual = actual_severity_warning
            ; WorstActual = actual_severity_informational
            ),
            Errors = no
        )
    ).

%---------------------%

contains_errors_and_or_warnings(Globals, Specs) = ErrorsOrWarnings :-
    globals.get_options(Globals, OptionTable),
    ErrorsOrWarnings =
        contains_errors_and_or_warnings_opt_table(OptionTable, Specs).

contains_errors_and_or_warnings_opt_table(OptionTable, Specs) =
        ErrorsOrWarnings :-
    MaybeWorstActual = worst_severity_in_specs_opt_table(OptionTable, Specs),
    (
        MaybeWorstActual = no,
        ErrorsOrWarnings = no
    ;
        MaybeWorstActual = yes(WorstActual),
        (
            ( WorstActual = actual_severity_error
            ; WorstActual = actual_severity_warning
            ),
            ErrorsOrWarnings = yes
        ;
            WorstActual = actual_severity_informational,
            ErrorsOrWarnings = no
        )
    ).

%---------------------%

contains_errors_or_warnings_treated_as_errors(Globals, Specs) = Halt :-
    globals.get_options(Globals, OptionTable),
    Halt = contains_errors_or_warnings_treated_as_errors_opt_table(OptionTable,
        Specs).

contains_errors_or_warnings_treated_as_errors_opt_table(OptionTable, Specs)
        = Halt :-
    MaybeWorstActual = worst_severity_in_specs_opt_table(OptionTable, Specs),
    (
        MaybeWorstActual = no,
        Halt = no
    ;
        MaybeWorstActual = yes(WorstActual),
        (
            WorstActual = actual_severity_error,
            Halt = yes
        ;
            WorstActual = actual_severity_warning,
            getopt.lookup_bool_option(OptionTable, halt_at_warn, HaltAtWarn),
            (
                HaltAtWarn = yes,
                Halt = yes
            ;
                HaltAtWarn = no,
                Halt = no
            )
        ;
            WorstActual = actual_severity_informational,
            Halt = no
        )
    ).

%---------------------------------------------------------------------------%

filter_interface_generation_specs(Globals, Specs, SpecsToPrint) :-
    globals.lookup_bool_option(Globals,
        halt_at_invalid_interface, HaltInvalidInterface),
    (
        HaltInvalidInterface = yes,
        list.filter(does_spec_print_anything(Globals), Specs, SpecsToPrint)
    ;
        HaltInvalidInterface = no,
        SpecsToPrint = []
    ).

%---------------------------------------------------------------------------%

:- type error_spec_accumulator == maybe(pair(set(error_spec))).

init_error_spec_accumulator = no.

accumulate_error_specs_for_proc(ProcSpecs, !MaybeSpecs) :-
    list.filter(
        ( pred(Spec::in) is semidet :-
            Phase = project_spec_phase(Spec),
            ModeReportControl = get_maybe_mode_report_control(Phase),
            ModeReportControl = yes(report_only_if_in_all_modes)
        ), ProcSpecs, ProcAllModeSpecs, ProcAnyModeSpecs),
    ProcAnyModeSpecSet = set.list_to_set(ProcAnyModeSpecs),
    ProcAllModeSpecSet = set.list_to_set(ProcAllModeSpecs),
    (
        !.MaybeSpecs = yes(AnyModeSpecSet0 - AllModeSpecSet0),
        set.union(AnyModeSpecSet0, ProcAnyModeSpecSet, AnyModeSpecSet),
        set.intersect(AllModeSpecSet0, ProcAllModeSpecSet, AllModeSpecSet),
        !:MaybeSpecs = yes(AnyModeSpecSet - AllModeSpecSet)
    ;
        !.MaybeSpecs = no,
        !:MaybeSpecs = yes(ProcAnyModeSpecSet - ProcAllModeSpecSet)
    ).

:- func project_spec_phase(error_spec) = error_phase.

project_spec_phase(Spec) = Phase :-
    (
        Spec = error_spec(_, _, Phase, _)
    ;
        Spec = simplest_spec(_, _, Phase, _, _)
    ;
        Spec = simplest_no_context_spec(_, _, Phase, _)
    ;
        Spec = conditional_spec(_, _, _, _, Phase, _)
    ).

error_spec_accumulator_to_list(no) = [].
error_spec_accumulator_to_list(yes(AnyModeSpecSet - AllModeSpecSet)) =
    set.to_sorted_list(set.union(AnyModeSpecSet, AllModeSpecSet)).

:- func get_maybe_mode_report_control(error_phase) =
    maybe(mode_report_control).

get_maybe_mode_report_control(phase_options) = no.
get_maybe_mode_report_control(phase_check_libs) = no.
get_maybe_mode_report_control(phase_make_target) = no.
get_maybe_mode_report_control(phase_make_int) = no.
get_maybe_mode_report_control(phase_find_files(_)) = no.
get_maybe_mode_report_control(phase_read_files) = no.
get_maybe_mode_report_control(phase_module_name) = no.
get_maybe_mode_report_control(phase_term_to_parse_tree) = no.
get_maybe_mode_report_control(phase_type_inst_mode_check) = no.
get_maybe_mode_report_control(phase_type_inst_mode_check_invalid_type) = no.
get_maybe_mode_report_control(phase_type_inst_mode_check_invalid_inst_mode)
    = no.
get_maybe_mode_report_control(phase_type_repn) = no.
get_maybe_mode_report_control(phase_parse_tree_to_hlds) = no.
get_maybe_mode_report_control(phase_expand_types) = no.
get_maybe_mode_report_control(phase_type_check) = no.
get_maybe_mode_report_control(phase_inst_check) = no.
get_maybe_mode_report_control(phase_polymorphism) = no.
get_maybe_mode_report_control(phase_mode_check(Control)) = yes(Control).
get_maybe_mode_report_control(phase_purity_check) = no.
get_maybe_mode_report_control(phase_detism_check) = no.
get_maybe_mode_report_control(phase_fact_table_check) = no.
get_maybe_mode_report_control(phase_oisu_check) = no.
get_maybe_mode_report_control(phase_simplify(Control)) = yes(Control).
get_maybe_mode_report_control(phase_direct_arg_in_out) = no.
get_maybe_mode_report_control(phase_style) = no.
get_maybe_mode_report_control(phase_dead_code) = no.
get_maybe_mode_report_control(phase_termination_analysis) = no.
get_maybe_mode_report_control(phase_accumulator_intro) = no.
get_maybe_mode_report_control(phase_auto_parallelism) = no.
get_maybe_mode_report_control(phase_interface_gen) = no.
get_maybe_mode_report_control(phase_code_gen) = no.

%---------------------------------------------------------------------------%
:- end_module parse_tree.error_util.
%---------------------------------------------------------------------------%
