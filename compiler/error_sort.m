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
:- import_module libs.options.
:- import_module parse_tree.error_spec.

:- import_module list.

%---------------------------------------------------------------------------%

:- pred sort_error_specs(globals::in,
    list(error_spec)::in, list(error_spec)::out) is det.
:- pred sort_error_specs_opt_table(option_table::in,
    list(error_spec)::in, list(error_spec)::out) is det.

%---------------------------------------------------------------------------%

:- type error_msg_group
    --->    error_msg_group(error_msg, list(error_msg)).

:- pred sort_error_msg_groups(list(error_msg_group)::in,
    list(error_msg_group)::out) is det.

:- func flatten_error_msg_groups(list(error_msg_group)) = list(error_msg).
:- func flatten_error_msg_group(error_msg_group) = list(error_msg).

%---------------------------------------------------------------------------%

:- pred sort_error_msgs(list(error_msg)::in, list(error_msg)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.error_util.

:- import_module bool.
:- import_module cord.
:- import_module getopt.
:- import_module maybe.
:- import_module term_context.

%---------------------------------------------------------------------------%

sort_error_specs(Globals, !Specs) :-
    globals.get_options(Globals, OptionTable),
    sort_error_specs_opt_table(OptionTable, !Specs).

sort_error_specs_opt_table(OptionTable, !Specs) :-
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
    list.filter_map(remove_conditionals_in_spec(OptionTable), !Specs),
    getopt.lookup_bool_option(OptionTable, reverse_error_order,
        ReverseErrorOrder),
    list.sort_and_remove_dups(
        compare_error_specs(OptionTable, ReverseErrorOrder),
        !Specs).

:- pred remove_conditionals_in_spec(option_table::in,
    error_spec::in, error_spec::out) is semidet.

remove_conditionals_in_spec(OptionTable, Spec0, Spec) :-
    require_det (
        (
            Spec0 = error_spec(Id, Severity0, Phase, Msgs0),
            MaybeActualSeverity =
                actual_error_severity_opt_table(OptionTable, Severity0),
            list.filter_map(remove_conditionals_in_msg(OptionTable),
                Msgs0, Msgs)
        ;
            Spec0 = spec(Id, Severity0, Phase, Context0, Pieces0),
            MaybeActualSeverity =
                actual_error_severity_opt_table(OptionTable, Severity0),
            Msgs = [msg(Context0, Pieces0)]
        ;
            Spec0 = no_ctxt_spec(Id, Severity0, Phase, Pieces0),
            MaybeActualSeverity =
                actual_error_severity_opt_table(OptionTable, Severity0),
            Msgs = [no_ctxt_msg(Pieces0)]
        ;
            Spec0 = conditional_spec(Id, Option, MatchValue,
                Severity0, Phase, Msgs0),
            getopt.lookup_bool_option(OptionTable, Option, OptionValue),
            ( if OptionValue = MatchValue then
                MaybeActualSeverity =
                    actual_error_severity_opt_table(OptionTable, Severity0),
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

:- pred remove_conditionals_in_msg(option_table::in,
    error_msg::in, error_msg::out) is semidet.

remove_conditionals_in_msg(OptionTable, Msg0, Msg) :-
    require_det (
        (
            Msg0 = msg(Context, Pieces0),
            MaybeContext = yes(Context),
            TreatAsFirst = treat_based_on_posn,
            ExtraIndent = 0u,
            Components0 = [always(Pieces0)]
        ;
            Msg0 = no_ctxt_msg(Pieces0),
            MaybeContext = no,
            TreatAsFirst = treat_based_on_posn,
            ExtraIndent = 0u,
            Components0 = [always(Pieces0)]
        ;
            Msg0 = simple_msg(Context, Components0),
            MaybeContext = yes(Context),
            TreatAsFirst = treat_based_on_posn,
            ExtraIndent = 0u
        ;
            Msg0 = error_msg(MaybeContext, TreatAsFirst, ExtraIndent,
                Components0)
        ;
            Msg0 = blank_msg(MaybeContext),
            TreatAsFirst = always_treat_as_first,
            ExtraIndent = 0u,
            Components0 = [always([blank_line])]
        ),
        list.foldl(remove_conditionals_in_msg_component(OptionTable),
            Components0, cord.init, ComponentCord),
        Components = cord.list(ComponentCord),
        Msg = error_msg(MaybeContext, TreatAsFirst, ExtraIndent, Components)
    ),
    % Don't include the Msg if Components is empty.
    Components = [_ | _].

:- pred remove_conditionals_in_msg_component(option_table::in,
    error_msg_component::in,
    cord(error_msg_component)::in, cord(error_msg_component)::out) is det.

remove_conditionals_in_msg_component(OptionTable, Component, !ComponentCord) :-
    (
        Component = option_is_set(Option, MatchValue, EmbeddedComponents),
        % We could recurse down into EmbeddedComponents, but we currently
        % have any places in the compiler that can generate two error messages
        % that differ only in nested option settings, so there would be
        % no point.
        getopt.lookup_bool_option(OptionTable, Option, OptionValue),
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
        ),
        !:ComponentCord = cord.snoc(!.ComponentCord, Component)
    ).

%---------------------%

:- pred compare_error_specs(option_table::in, bool::in,
    error_spec::in, error_spec::in, comparison_result::out) is det.

compare_error_specs(OptionTable, ReverseErrorOrder, SpecA, SpecB, Result) :-
    extract_spec_msgs_opt_table(OptionTable, SpecA, MsgsA),
    extract_spec_msgs_opt_table(OptionTable, SpecB, MsgsB),
    compare_error_msg_lists(ReverseErrorOrder, MsgsA, MsgsB, MsgsResult),
    (
        MsgsResult = (=),
        compare(Result, SpecA, SpecB)
    ;
        ( MsgsResult = (>)
        ; MsgsResult = (<)
        ),
        Result = MsgsResult
    ).

:- pred compare_error_msg_lists(bool::in,
    list(error_msg)::in, list(error_msg)::in, comparison_result::out) is det.

compare_error_msg_lists(ReverseErrorOrder, MsgsA, MsgsB, Result) :-
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
        compare_error_msgs(ReverseErrorOrder, HeadMsgA, HeadMsgB,
            HeadResult),
        (
            HeadResult = (=),
            compare_error_msg_lists(ReverseErrorOrder, TailMsgsA, TailMsgsB,
                Result)
        ;
            ( HeadResult = (>)
            ; HeadResult = (<)
            ),
            Result = HeadResult
        )
    ).

%---------------------------------------------------------------------------%

sort_error_msg_groups(MsgGroups0, MsgGroups) :-
    list.sort_and_remove_dups(compare_error_msg_groups, MsgGroups0, MsgGroups).

:- pred compare_error_msg_groups(error_msg_group::in, error_msg_group::in,
    comparison_result::out) is det.

compare_error_msg_groups(GroupA, GroupB, Result) :-
    GroupA = error_msg_group(HeadMsgA, TailMsgsA),
    GroupB = error_msg_group(HeadMsgB, TailMsgsB),
    compare_error_msgs(no, HeadMsgA, HeadMsgB, Result0),
    (
        Result0 = (=),
        (
            TailMsgsA = [],
            TailMsgsB = [],
            Result = (=)
        ;
            TailMsgsA = [],
            TailMsgsB = [_HeadTailMsgB | _TailTailMsgsB],
            Result = (<)
        ;
            TailMsgsA = [_HeadTailMsgA | _TailTailMsgsA],
            TailMsgsB = [],
            Result = (>)
        ;
            TailMsgsA = [HeadTailMsgA | TailTailMsgsA],
            TailMsgsB = [HeadTailMsgB | TailTailMsgsB],
            TailGroupA = error_msg_group(HeadTailMsgA, TailTailMsgsA),
            TailGroupB = error_msg_group(HeadTailMsgB, TailTailMsgsB),
            compare_error_msg_groups(TailGroupA, TailGroupB, Result)
        )
    ;
        ( Result0 = (<)
        ; Result0 = (>)
        ),
        Result = Result0
    ).

flatten_error_msg_groups(Groups) = Msgs :-
    MsgLists = list.map(flatten_error_msg_group, Groups),
    list.condense(MsgLists, Msgs).

flatten_error_msg_group(Group) = Msgs :-
    Group= error_msg_group(HeadMsg, TailMsgs),
    Msgs = [HeadMsg | TailMsgs].

%---------------------------------------------------------------------------%

sort_error_msgs(Msgs0, Msgs) :-
    list.sort_and_remove_dups(compare_error_msgs(no), Msgs0, Msgs).

:- pred compare_error_msgs(bool::in, error_msg::in, error_msg::in,
    comparison_result::out) is det.

compare_error_msgs(ReverseErrorOrder, MsgA, MsgB, Result) :-
    extract_msg_maybe_context(MsgA, MaybeContextA),
    extract_msg_maybe_context(MsgB, MaybeContextB),
    % The context comparison makes sense only if both Msgs have a context.
    % If one or both Msgs lack a context, then go on to compare the components.
    ( if
        MaybeContextA = yes(ContextA),
        MaybeContextB = yes(ContextB)
    then
        compare(ContextResult, ContextA, ContextB)
    else
        ContextResult = (=)
    ),
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
        ContextResult = (>),
        (
            ReverseErrorOrder = no,
            Result = ContextResult
        ;
            ReverseErrorOrder = yes,
            Result = (<)
        )
    ;
        ContextResult = (<),
        (
            ReverseErrorOrder = no,
            Result = ContextResult
        ;
            ReverseErrorOrder = yes,
            Result = (>)
        )
    ).

:- func project_msg_components(error_msg) = list(error_msg_component).

project_msg_components(Msg) = Components :-
    (
        ( Msg = msg(_, Pieces)
        ; Msg = no_ctxt_msg(Pieces)
        ),
        Components = [always(Pieces)]
    ;
        Msg = simple_msg(_, Components)
    ;
        Msg = error_msg(_, _, _, Components)
    ;
        Msg = blank_msg(_),
        Components = [always([blank_line])]
    ).

%---------------------------------------------------------------------------%
:- end_module parse_tree.error_sort.
%---------------------------------------------------------------------------%
