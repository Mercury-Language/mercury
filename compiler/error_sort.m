%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1997-2012 The University of Melbourne.
% Copyright (C) 2022-2026 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: error_sort.m.
% Main author: zs.
%
% This module sorts diag_specs and diag_msgs.
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

    % The purpose of standardizing diag_specs is to remove differences
    % between diag_specs that exist only in the structure of the diag_specs
    % themselves, as opposed to the text that we output for them.
    %
    % For example, the compiler could in theory generate (and once upon a time,
    % it did generate) error specs that differ in that some error msg
    % components consist of
    %
    % - "always(...)" in one, and
    % - "option_is_set(OptionName, yes, always(...))" in the other.
    %
    % But if OptionName is yes, then this difference has no effect.
    %
:- pred standardize_diag_specs(list(diag_spec)::in,
    list(std_diag_spec)::out) is det.

%---------------------------------------------------------------------------%

:- pred sort_std_diag_specs(globals::in,
    list(std_diag_spec)::in, list(std_diag_spec)::out) is det.
:- pred sort_std_diag_specs_opt_table(option_table::in,
    list(std_diag_spec)::in, list(std_diag_spec)::out) is det.

%---------------------------------------------------------------------------%

:- type diag_msg_group
    --->    diag_msg_group(diag_msg, list(diag_msg)).

:- pred sort_diag_msg_groups(list(diag_msg_group)::in,
    list(diag_msg_group)::out) is det.

%---------------------------------------------------------------------------%

:- func flatten_diag_msg_groups(list(diag_msg_group)) = list(diag_msg).
:- func flatten_diag_msg_group(diag_msg_group) = list(diag_msg).

%---------------------------------------------------------------------------%

:- pred sort_diag_msgs(list(diag_msg)::in, list(diag_msg)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module getopt.
:- import_module maybe.
:- import_module require.
:- import_module term_context.

%---------------------------------------------------------------------------%

standardize_diag_specs(Specs, StdSpecs) :-
    list.map(standardize_diag_spec, Specs, StdSpecs).

:- pred standardize_diag_spec(diag_spec::in, std_diag_spec::out) is det.

standardize_diag_spec(Spec0, StdSpec) :-
    (
        Spec0 = gen_spec(Id, Severity, Phase, Msgs0),
        list.map(standardize_diag_msg, Msgs0, StdMsgs)
    ;
        Spec0 = spec(Id, Severity, Phase, Context0, Pieces0),
        StdMsgs = [gen_msg(yes(Context0), treat_based_on_posn, 0u,
            [always(Pieces0)])]
    ;
        Spec0 = no_ctxt_spec(Id, Severity, Phase, Pieces0),
        StdMsgs = [gen_msg(no, treat_based_on_posn, 0u,
            [always(Pieces0)])]
    ),
    (
        StdMsgs = [],
        unexpected($pred, "StdMsgs = []")
    ;
        StdMsgs = [_ | _],
        StdSpec = gen_spec(Id, Severity, Phase, StdMsgs)
    ).

:- pred standardize_diag_msg(diag_msg::in, std_diag_msg::out) is det.

standardize_diag_msg(Msg0, StdMsg) :-
    (
        Msg0 = msg(Context, Pieces0),
        MaybeContext = yes(Context),
        TreatAsFirst = treat_based_on_posn,
        ExtraIndent = 0u,
        StdComponents = [always(Pieces0)]
    ;
        Msg0 = no_ctxt_msg(Pieces0),
        MaybeContext = no,
        TreatAsFirst = treat_based_on_posn,
        ExtraIndent = 0u,
        StdComponents = [always(Pieces0)]
    ;
        Msg0 = simple_msg(Context, StdComponents),
        MaybeContext = yes(Context),
        TreatAsFirst = treat_based_on_posn,
        ExtraIndent = 0u
    ;
        Msg0 = gen_msg(MaybeContext, TreatAsFirst, ExtraIndent,
            StdComponents)
    ;
        Msg0 = blank_msg(MaybeContext),
        TreatAsFirst = always_treat_as_first,
        ExtraIndent = 0u,
        StdComponents = [always([blank_line])]
    ),
    (
        StdComponents = [],
        % In this situation, we used to just fail, making both this predicate,
        % and its caller standardize_diag_spec, semidet.
        %
        % This was when we still had diag_msgs that were conditional on
        % the value of an option. Ever since we moved all such conditionality
        % to the top level (to the severity of the diag_spec), we never delete
        % any diag_msgs from an diag_spec. The only way that execution can
        % *now* get here is if the diag_spec was *constructed* with zero
        % components. That would be a bug, which we catch here.
        %
        % We *could* use the type system to enforce the "no empty list of
        % components" rule, but the type analysis system cannot handle
        % any subtype of the list type with acceptable performance, and
        % using the one_or_more type instead in diag_specs would be
        % inconvenient.
        unexpected($pred, "StdComponents = []")
    ;
        StdComponents = [_ | _],
        StdMsg = gen_msg(MaybeContext, TreatAsFirst,
            ExtraIndent, StdComponents)
    ).

%---------------------------------------------------------------------------%

sort_std_diag_specs(Globals, StdSpecs, SortedStdSpecs) :-
    globals.get_options(Globals, OptionTable),
    sort_std_diag_specs_opt_table(OptionTable, StdSpecs, SortedStdSpecs).

sort_std_diag_specs_opt_table(OptionTable, StdSpecs, SortedStdSpecs) :-
    getopt.lookup_bool_option(OptionTable, reverse_error_order,
        ReverseErrorOrder),
    list.sort_and_remove_dups(
        compare_std_diag_specs(ReverseErrorOrder),
        StdSpecs, SortedStdSpecs).

%---------------------%

:- pred compare_std_diag_specs(bool::in,
    std_diag_spec::in, std_diag_spec::in, comparison_result::out) is det.

compare_std_diag_specs(ReverseErrorOrder, SpecA, SpecB, Result) :-
    SpecA = gen_spec(_, _, _, MsgsA),
    SpecB = gen_spec(_, _, _, MsgsB),
    compare_std_diag_msg_lists(ReverseErrorOrder, MsgsA, MsgsB, MsgsResult),
    (
        MsgsResult = (=),
        compare(Result, SpecA, SpecB)
    ;
        ( MsgsResult = (>)
        ; MsgsResult = (<)
        ),
        Result = MsgsResult
    ).

:- pred compare_std_diag_msg_lists(bool::in,
    list(std_diag_msg)::in, list(std_diag_msg)::in,
    comparison_result::out) is det.

compare_std_diag_msg_lists(ReverseErrorOrder, MsgsA, MsgsB, Result) :-
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
        compare_diag_msgs(ReverseErrorOrder,
            coerce(HeadMsgA), coerce(HeadMsgB), HeadResult),
        (
            HeadResult = (=),
            compare_std_diag_msg_lists(ReverseErrorOrder,
                TailMsgsA, TailMsgsB, Result)
        ;
            ( HeadResult = (>)
            ; HeadResult = (<)
            ),
            Result = HeadResult
        )
    ).

%---------------------------------------------------------------------------%

sort_diag_msg_groups(MsgGroups0, MsgGroups) :-
    list.sort_and_remove_dups(compare_diag_msg_groups, MsgGroups0, MsgGroups).

:- pred compare_diag_msg_groups(diag_msg_group::in, diag_msg_group::in,
    comparison_result::out) is det.

compare_diag_msg_groups(GroupA, GroupB, Result) :-
    GroupA = diag_msg_group(HeadMsgA, TailMsgsA),
    GroupB = diag_msg_group(HeadMsgB, TailMsgsB),
    compare_diag_msgs(no, HeadMsgA, HeadMsgB, Result0),
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
            TailGroupA = diag_msg_group(HeadTailMsgA, TailTailMsgsA),
            TailGroupB = diag_msg_group(HeadTailMsgB, TailTailMsgsB),
            compare_diag_msg_groups(TailGroupA, TailGroupB, Result)
        )
    ;
        ( Result0 = (<)
        ; Result0 = (>)
        ),
        Result = Result0
    ).

%---------------------------------------------------------------------------%

flatten_diag_msg_groups(Groups) = Msgs :-
    MsgLists = list.map(flatten_diag_msg_group, Groups),
    list.condense(MsgLists, Msgs).

flatten_diag_msg_group(Group) = Msgs :-
    Group= diag_msg_group(HeadMsg, TailMsgs),
    Msgs = [HeadMsg | TailMsgs].

%---------------------------------------------------------------------------%

sort_diag_msgs(Msgs0, Msgs) :-
    list.sort_and_remove_dups(compare_diag_msgs(no), Msgs0, Msgs).

:- pred compare_diag_msgs(bool::in, diag_msg::in, diag_msg::in,
    comparison_result::out) is det.

compare_diag_msgs(ReverseErrorOrder, MsgA, MsgB, Result) :-
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

:- func project_msg_components(diag_msg) = list(diag_msg_component).

project_msg_components(Msg) = Components :-
    (
        ( Msg = msg(_, Pieces)
        ; Msg = no_ctxt_msg(Pieces)
        ),
        Components = [always(Pieces)]
    ;
        Msg = simple_msg(_, Components)
    ;
        Msg = gen_msg(_, _, _, Components)
    ;
        Msg = blank_msg(_),
        Components = [always([blank_line])]
    ).

%---------------------------------------------------------------------------%
:- end_module parse_tree.error_sort.
%---------------------------------------------------------------------------%
