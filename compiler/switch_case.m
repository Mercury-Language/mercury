%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2007, 2009-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: switch_case.m.
% Author: zs.
%
% Utility predicates for handling switch cases, especially those representing
% more than one cons_id, for the LLDS backend.
%
%-----------------------------------------------------------------------------%

:- module ll_backend.switch_case.

:- interface.

:- import_module hlds.
:- import_module hlds.code_model.
:- import_module hlds.hlds_goal.
:- import_module ll_backend.code_info.
:- import_module ll_backend.code_loc_dep.
:- import_module ll_backend.llds.

:- import_module map.

:- type represent_params
    --->    represent_params(
                switch_var_name     :: string,
                switch_goal_info    :: hlds_goal_info,
                switch_code_model   :: code_model,
                starting_position   :: position_info,
                switch_end_label    :: label
            ).

:- type case_code_included
    --->    case_code_not_yet_included
    ;       case_code_already_included.

:- type case_label_info
    --->    case_label_info(
                case_description    :: string,
                case_code           :: llds_code,
                case_code_included  :: case_code_included
            ).

:- type case_label_map == map(label, case_label_info).

    % represent_tagged_case_for_llds(Params, TaggedCase, Label,
    %   !CaseLabelMap, !MaybeEnd, !CI):
    %
    % Given TaggedCase, generate code for it (using the information in Params,
    % and updating MaybeEnd and CI). The code will start with the newly
    % allocated label Label. This label will represent the case in
    % CaseLabelMap. The corresponding case_label_info will contain a comment
    % describing the case in terms of the cons_ids it handles, the generated
    % code (starting with the label instruction for Label and ending with
    % the jump to the end label of the switch), and an indication that this
    % code has not yet been included anywhere.
    %
:- pred represent_tagged_case_for_llds(represent_params::in,
    tagged_case::in, label::out, case_label_map::in, case_label_map::out,
    branch_end::in, branch_end::out, code_info::in, code_info::out) is det.

    % generate_case_code_or_jump(CaseLabel, Code, !CaseLabelMap):
    %
:- pred generate_case_code_or_jump(label::in, llds_code::out,
    case_label_map::in, case_label_map::out) is det.

:- pred add_remaining_case(label::in, case_label_info::in,
    llds_code::in, llds_code::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_llds.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_out.hlds_out_goal.
:- import_module ll_backend.code_gen.
:- import_module ll_backend.trace_gen.

:- import_module cord.
:- import_module list.
:- import_module string.

represent_tagged_case_for_llds(Params, TaggedCase, Label, !CaseLabelMap,
        !MaybeEnd, !CI) :-
    some [!CLD] (
        Params = represent_params(SwitchVarName, SwitchGoalInfo, CodeModel,
            BranchStart, EndLabel),
        TaggedCase =
            tagged_case(MainTaggedConsId, OtherTaggedConsIds, _, Goal),
        project_cons_name_and_tag(MainTaggedConsId, MainConsName, _),
        list.map2(project_cons_name_and_tag, OtherTaggedConsIds,
            OtherConsNames, _),
        Comment = case_comment(SwitchVarName, MainConsName, OtherConsNames),
        reset_to_position(BranchStart, !.CI, !:CLD),
        get_next_label(Label, !CI),
        LabelCode = singleton(
            llds_instr(label(Label), Comment)
        ),
        maybe_generate_internal_event_code(Goal, SwitchGoalInfo, TraceCode,
            !CI, !CLD),
        generate_goal(CodeModel, Goal, GoalCode, !CI, !CLD),
        goal_info_get_store_map(SwitchGoalInfo, StoreMap),
        generate_branch_end(StoreMap, !MaybeEnd, SaveCode, !.CI, !.CLD),
        GotoEndCode = singleton(
            llds_instr(goto(code_label(EndLabel)),
                "goto end of switch on " ++ SwitchVarName)
        ),
        Code = LabelCode ++ TraceCode ++ GoalCode ++ SaveCode ++ GotoEndCode,
        CaseInfo = case_label_info(Comment, Code, case_code_not_yet_included),
        map.det_insert(Label, CaseInfo, !CaseLabelMap)
    ).

generate_case_code_or_jump(CaseLabel, Code, !CaseLabelMap) :-
    map.lookup(!.CaseLabelMap, CaseLabel, CaseInfo0),
    CaseInfo0 = case_label_info(Comment, CaseCode, CaseIncluded),
    (
        CaseIncluded = case_code_not_yet_included,
        Code = CaseCode,
        CaseInfo = CaseInfo0 ^ case_code_included
            := case_code_already_included,
        map.det_update(CaseLabel, CaseInfo, !CaseLabelMap)
    ;
        CaseIncluded = case_code_already_included,
        % We cannot include the case's code, since it has already been included
        % somewhere else.
        Code = singleton(
            llds_instr(goto(code_label(CaseLabel)), "goto " ++ Comment)
        )
    ).

add_remaining_case(_Label, CaseInfo, !Code) :-
    CaseInfo = case_label_info(_Comment, CaseCode, CaseIncluded),
    (
        CaseIncluded = case_code_not_yet_included,
        !:Code = !.Code ++ CaseCode
    ;
        CaseIncluded = case_code_already_included
    ).

%-----------------------------------------------------------------------------%
:- end_module ll_backend.switch_case.
%-----------------------------------------------------------------------------%
