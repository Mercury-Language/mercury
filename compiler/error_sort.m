%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1997-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: error_sort.m.
% Main author: zs.
%
% This module sorts error_specs and error_msgs.
%
%---------------------------------------------------------------------------%

:- module parse_tree.error_sort.
:- interface.

:- import_module libs.
:- import_module libs.globals.
:- import_module parse_tree.error_spec.

:- import_module list.

%---------------------------------------------------------------------------%

:- pred sort_error_specs(globals::in,
    list(error_spec)::in, list(error_spec)::out) is det.

%---------------------------------------------------------------------------%

:- pred sort_error_msgs(list(error_msg)::in, list(error_msg)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.options.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module cord.
:- import_module maybe.
:- import_module term_context.

%---------------------------------------------------------------------------%

sort_error_specs(Globals, !Specs) :-
    % The purpose of remove_conditionals_in_spec is to remove differences
    % between error_specs that exist only in the structure of the error_specs
    % themselves, as opposed to the text that we output for them.
    %
    % For example, the parser can generate two error specs for a bad module
    % name that differ in two things.
    %
    % - The first difference is that one has "severity_error", while the other
    %   has "severity_conditional(warn_wrong_module_name, yes, severity_error,
    %   no)". However, since warn_wrong_module_name is yes by default,
    %   this difference has no effect.
    %
    % - The second difference is that some error_msg_components consist of
    %   "always(...)" in one, and "option_is_set(warn_wrong_module_name, yes,
    %   always(...))" in the other. But if warn_wrong_module_name is yes,
    %   this difference has no effect either.
    %
    % (The parser should no longer generate duplicate error messages
    % for bad module names, but we still keep this workaround in place,
    % since the cost of doing so is trivial.)
    %
    list.filter_map(remove_conditionals_in_spec(Globals), !Specs),
    list.sort_and_remove_dups(compare_error_specs(Globals), !Specs).

:- pred remove_conditionals_in_spec(globals::in,
    error_spec::in, error_spec::out) is semidet.

remove_conditionals_in_spec(Globals, Spec0, Spec) :-
    require_det (
        (
            Spec0 = error_spec(Id, Severity0, Phase, Msgs0),
            MaybeActualSeverity = actual_error_severity(Globals, Severity0),
            list.filter_map(remove_conditionals_in_msg(Globals), Msgs0, Msgs)
        ;
            Spec0 = simplest_spec(Id, Severity0, Phase, Context0, Pieces0),
            MaybeActualSeverity = actual_error_severity(Globals, Severity0),
            Msgs = [simplest_msg(Context0, Pieces0)]
        ;
            Spec0 = simplest_no_context_spec(Id, Severity0, Phase, Pieces0),
            MaybeActualSeverity = actual_error_severity(Globals, Severity0),
            Msgs = [simplest_no_context_msg(Pieces0)]
        ;
            Spec0 = conditional_spec(Id, Option, MatchValue,
                Severity0, Phase, Msgs0),
            globals.lookup_bool_option(Globals, Option, OptionValue),
            ( if OptionValue = MatchValue then
                MaybeActualSeverity =
                    actual_error_severity(Globals, Severity0),
                Msgs = Msgs0
            else
                MaybeActualSeverity = no,
                Msgs = []
            )
        )
    ),
    ( if
        MaybeActualSeverity = yes(ActualSeverity),
        Msgs = [_ | _]
    then
        require_det (
            (
                ActualSeverity = actual_severity_error,
                Severity = severity_error
            ;
                ActualSeverity = actual_severity_warning,
                Severity = severity_warning
            ;
                ActualSeverity = actual_severity_informational,
                Severity = severity_informational
            ),
            Spec = error_spec(Id, Severity, Phase, Msgs)
        )
    else
        % Spec0 would result in nothing being printed.
        fail
    ).

:- pred remove_conditionals_in_msg(globals::in,
    error_msg::in, error_msg::out) is semidet.

remove_conditionals_in_msg(Globals, Msg0, Msg) :-
    require_det (
        (
            Msg0 = simplest_msg(Context, Pieces0),
            Components0 = [always(Pieces0)],
            MaybeContext = yes(Context),
            TreatAsFirst = treat_based_on_posn,
            ExtraIndent = 0
        ;
            Msg0 = simplest_no_context_msg(Pieces0),
            Components0 = [always(Pieces0)],
            MaybeContext = no,
            TreatAsFirst = treat_based_on_posn,
            ExtraIndent = 0
        ;
            Msg0 = simple_msg(Context, Components0),
            MaybeContext = yes(Context),
            TreatAsFirst = treat_based_on_posn,
            ExtraIndent = 0
        ;
            Msg0 = error_msg(MaybeContext, TreatAsFirst, ExtraIndent,
                Components0)
        ),
        list.foldl(remove_conditionals_in_msg_component(Globals),
            Components0, cord.init, ComponentCord),
        Components = cord.list(ComponentCord),
        Msg = error_msg(MaybeContext, TreatAsFirst, ExtraIndent, Components)
    ),
    % Don't include the Msg if Components is empty.
    Components = [_ | _].

:- pred remove_conditionals_in_msg_component(globals::in,
    error_msg_component::in,
    cord(error_msg_component)::in, cord(error_msg_component)::out) is det.

remove_conditionals_in_msg_component(Globals, Component, !ComponentCord) :-
    (
        Component = option_is_set(Option, MatchValue, EmbeddedComponents),
        % We could recurse down into EmbeddedComponents, but we currently
        % have any places in the compiler that can generate two error messages
        % that differ only in nested option settings, so there would be
        % no point.
        globals.lookup_bool_option(Globals, Option, OptionValue),
        ( if OptionValue = MatchValue then
            !:ComponentCord =
                !.ComponentCord ++ cord.from_list(EmbeddedComponents)
        else
            true
        )
    ;
        % We don't want to eliminate the verbose only part of a message
        % even if verbose_errors isn't set. We want to keep them around
        % until we print the component, so that we can record the presence
        % of such verbose components, and generate a reminder of their
        % existence at the end of the compilation.
        %
        % Besides, the compiler can't (yet) generate two error_msg_components
        % that differ only in the presence of a verbose error.
        ( Component = always(_)
        ; Component = verbose_only(_, _)
        ; Component = verbose_and_nonverbose(_, _)
        ; Component = print_anything(_)
        ),
        !:ComponentCord = cord.snoc(!.ComponentCord, Component)
    ).

%---------------------%

:- pred compare_error_specs(globals::in, error_spec::in, error_spec::in,
    comparison_result::out) is det.

compare_error_specs(Globals, SpecA, SpecB, Result) :-
    extract_spec_msgs(Globals, SpecA, MsgsA),
    extract_spec_msgs(Globals, SpecB, MsgsB),
    compare_error_msg_lists(MsgsA, MsgsB, MsgsResult),
    (
        MsgsResult = (=),
        compare(Result, SpecA, SpecB)
    ;
        ( MsgsResult = (>)
        ; MsgsResult = (<)
        ),
        Result = MsgsResult
    ).

:- pred compare_error_msg_lists(list(error_msg)::in, list(error_msg)::in,
    comparison_result::out) is det.

compare_error_msg_lists(MsgsA, MsgsB, Result) :-
    (
        MsgsA = [],
        MsgsB = [],
        Result = (=)
    ;
        MsgsA = [],
        MsgsB = [_ | _],
        Result = (<)
    ;
        MsgsA = [_ | _],
        MsgsB = [],
        Result = (>)
    ;
        MsgsA = [HeadMsgA | TailMsgsA],
        MsgsB = [HeadMsgB | TailMsgsB],
        compare_error_msgs(HeadMsgA, HeadMsgB, HeadResult),
        (
            HeadResult = (=),
            compare_error_msg_lists(TailMsgsA, TailMsgsB, Result)
        ;
            ( HeadResult = (>)
            ; HeadResult = (<)
            ),
            Result = HeadResult
        )
    ).

%---------------------------------------------------------------------------%

sort_error_msgs(Msgs0, Msgs) :-
    list.sort_and_remove_dups(compare_error_msgs, Msgs0, Msgs).

:- pred compare_error_msgs(error_msg::in, error_msg::in,
    comparison_result::out) is det.

compare_error_msgs(MsgA, MsgB, Result) :-
    MaybeContextA = project_msg_context(MsgA),
    MaybeContextB = project_msg_context(MsgB),
    compare(ContextResult, MaybeContextA, MaybeContextB),
    (
        ContextResult = (=),
        ComponentsA = project_msg_components(MsgA),
        ComponentsB = project_msg_components(MsgB),
        compare(ComponentsResult, ComponentsA, ComponentsB),
        (
            ComponentsResult = (=),
            compare(Result, MsgA, MsgB)
        ;
            ( ComponentsResult = (>)
            ; ComponentsResult = (<)
            ),
            Result = ComponentsResult
        )
    ;
        ( ContextResult = (>)
        ; ContextResult = (<)
        ),
        Result = ContextResult
    ).

:- func project_msg_context(error_msg) = maybe(prog_context).

project_msg_context(Msg) = MaybeContext :-
    (
        Msg = simplest_msg(Context, _),
        MaybeContext = yes(Context)
    ;
        Msg = simplest_no_context_msg(_),
        MaybeContext = no
    ;
        Msg = simple_msg(Context, _),
        MaybeContext = yes(Context)
    ;
        Msg = error_msg(MaybeContext, _, _, _)
    ).

:- func project_msg_components(error_msg) = list(error_msg_component).

project_msg_components(Msg) = Components :-
    (
        ( Msg = simplest_msg(_, Pieces)
        ; Msg = simplest_no_context_msg(Pieces)
        ),
        Components = [always(Pieces)]
    ;
        Msg = simple_msg(_, Components)
    ;
        Msg = error_msg(_, _, _, Components)
    ).

%---------------------------------------------------------------------------%
:- end_module parse_tree.error_sort.
%---------------------------------------------------------------------------%
