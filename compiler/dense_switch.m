%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: dense_switch.m.
% Author: fjh.

% For switches on atomic types, generate code using a dense jump table.

%-----------------------------------------------------------------------------%

:- module ll_backend.dense_switch.
:- interface.

:- import_module backend_libs.switch_util.
:- import_module hlds.code_model.
:- import_module hlds.hlds_goal.
:- import_module ll_backend.code_info.
:- import_module ll_backend.llds.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_type.

%-----------------------------------------------------------------------------%

    % Should this switch be implemented as a dense jump table?
    % If so, we return the starting and ending values for the table,
    % and whether the switch is not covers all cases or not
    % (we may convert locally semidet switches into locally det
    % switches by adding extra cases whose body is just `fail').
    %
:- pred is_dense_switch(code_info::in, prog_var::in, cases_list::in,
    can_fail::in, int::in, int::out, int::out, can_fail::out) is semidet.

    % Generate code for a switch using a dense jump table.
    %
:- pred generate_dense_switch(cases_list::in, int::in, int::in, prog_var::in,
    code_model::in, can_fail::in, hlds_goal_info::in, label::in,
    branch_end::in, branch_end::out, code_tree::out,
    code_info::in, code_info::out) is det.

    % Also used by lookup_switch.
    %
:- pred calc_density(int::in, int::in, int::out) is det.

    % Also used by lookup_switch.
    %
:- pred type_range(code_info::in, type_category::in, mer_type::in, int::out)
    is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.builtin_ops.
:- import_module check_hlds.type_util.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_llds.
:- import_module hlds.hlds_module.
:- import_module libs.tree.
:- import_module ll_backend.code_gen.
:- import_module ll_backend.trace.

:- import_module char.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module std_util.

%-----------------------------------------------------------------------------%

is_dense_switch(CI, CaseVar, TaggedCases, CanFail0, ReqDensity,
        FirstVal, LastVal, CanFail) :-
    list.length(TaggedCases, NumCases),
    NumCases > 2,
    TaggedCases = [FirstCase | _],
    FirstCase = case(_, int_constant(FirstCaseVal), _, _),
    list.index1_det(TaggedCases, NumCases, LastCase),
    LastCase = case(_, int_constant(LastCaseVal), _, _),
    Span = LastCaseVal - FirstCaseVal,
    Range = Span + 1,
    dense_switch.calc_density(NumCases, Range, Density),
    Density > ReqDensity,
    ( CanFail0 = can_fail ->
        % For semidet switches, we normally need to check that the variable
        % is in range before we index into the jump table. However, if the
        % range of the type is sufficiently small, we can make the jump table
        % large enough to hold all of the values for the type.
        Type = code_info.variable_type(CI, CaseVar),
        code_info.get_module_info(CI, ModuleInfo),
        classify_type(ModuleInfo, Type) = TypeCategory,
        (
            dense_switch.type_range(CI, TypeCategory, Type, TypeRange),
            dense_switch.calc_density(NumCases, TypeRange, DetDensity),
            DetDensity > ReqDensity
        ->
            CanFail = cannot_fail,
            FirstVal = 0,
            LastVal = TypeRange - 1
        ;
            CanFail = CanFail0,
            FirstVal = FirstCaseVal,
            LastVal = LastCaseVal
        )
    ;
        CanFail = CanFail0,
        FirstVal = FirstCaseVal,
        LastVal = LastCaseVal
    ).

%---------------------------------------------------------------------------%

    % Calculate the percentage density given the range and the number of cases.
    %
calc_density(NumCases, Range, Density) :-
    N1 = NumCases * 100,
    Density = N1 // Range.

%---------------------------------------------------------------------------%

    % Determine the range of an atomic type. Fail if the type isn't the sort
    % of type that has a range or if the type's range is to big to switch on
    % (e.g. int).
    %
type_range(CI, TypeCategory, Type, Range) :-
    code_info.get_module_info(CI, ModuleInfo),
    switch_util.type_range(TypeCategory, Type, ModuleInfo, Min, Max),
    Range = Max - Min + 1.

%---------------------------------------------------------------------------%

generate_dense_switch(Cases, StartVal, EndVal, Var, CodeModel, CanFail,
        SwitchGoalInfo, EndLabel, MaybeEnd0, MaybeEnd, Code, !CI) :-
    % Evaluate the variable which we are going to be switching on.
    code_info.produce_variable(Var, VarCode, Rval, !CI),
    % If the case values start at some number other than 0,
    % then subtract that number to give us a zero-based index.
    ( StartVal = 0 ->
        Index = Rval
    ;
        Index = binop(int_sub, Rval, const(int_const(StartVal)))
    ),
    % If the switch is not locally deterministic, we need to check that
    % the value of the variable lies within the appropriate range.
    (
        CanFail = can_fail,
        Difference = EndVal - StartVal,
        code_info.fail_if_rval_is_false(
            binop(unsigned_le, Index, const(int_const(Difference))),
            RangeCheck, !CI)
    ;
        CanFail = cannot_fail,
        RangeCheck = empty
    ),
    % Now generate the jump table and the cases.
    generate_cases(Cases, StartVal, EndVal, CodeModel, SwitchGoalInfo,
        EndLabel, MaybeEnd0, MaybeEnd, Labels, CasesCode, !CI),

    % XXX We keep track of the code_info at the end of one of the non-fail
    % cases. We have to do this because generating a `fail' slot last would
    % yield the wrong liveness and would not unset the failure continuation
    % for a nondet switch.
    DoJump = node([
        computed_goto(Index, Labels) - "switch (using dense jump table)"
    ]),
    % Assemble the code fragments.
    Code = tree_list([VarCode, RangeCheck, DoJump, CasesCode]).

:- pred generate_cases(cases_list::in, int::in, int::in, code_model::in,
    hlds_goal_info::in, label::in, branch_end::in, branch_end::out,
    list(label)::out, code_tree::out, code_info::in, code_info::out) is det.

generate_cases(Cases0, NextVal, EndVal, CodeModel, SwitchGoalInfo, EndLabel,
        !MaybeEnd, Labels, Code, !CI) :-
    ( NextVal > EndVal ->
        Labels = [],
        Code = node([
            label(EndLabel) - "End of dense switch"
        ])
    ;
        code_info.get_next_label(ThisLabel, !CI),
        generate_case(Cases0, Cases1, NextVal, CodeModel,
            SwitchGoalInfo, !MaybeEnd, ThisCode, Comment, !CI),
        LabelCode = node([
            label(ThisLabel) - Comment
        ]),
        JumpCode = node([
            goto(label(EndLabel)) - "branch to end of dense switch"
        ]),
        % Generate the rest of the cases.
        NextVal1 = NextVal + 1,
        generate_cases(Cases1, NextVal1, EndVal, CodeModel, SwitchGoalInfo,
            EndLabel, !MaybeEnd, Labels1, OtherCasesCode, !CI),
        Labels = [ThisLabel | Labels1],
        Code = tree_list([LabelCode, ThisCode, JumpCode, OtherCasesCode])
    ).

%---------------------------------------------------------------------------%

:- pred generate_case(cases_list::in, cases_list::out, int::in, code_model::in,
    hlds_goal_info::in, branch_end::in, branch_end::out, code_tree::out,
    string::out, code_info::in, code_info::out) is det.

generate_case(!Cases, NextVal, CodeModel, SwitchGoalInfo, !MaybeEnd, Code,
        Comment, !CI) :-
    (
        !.Cases = [Case | !:Cases],
        Case = case(_, int_constant(NextVal), _, Goal)
    ->
        Comment = "case of dense switch",
        % We need to save the expression cache, etc.,
        % and restore them when we've finished.
        code_info.remember_position(!.CI, BranchStart),
        trace.maybe_generate_internal_event_code(Goal, SwitchGoalInfo,
            TraceCode, !CI),
        code_gen.generate_goal(CodeModel, Goal, GoalCode, !CI),
        goal_info_get_store_map(SwitchGoalInfo, StoreMap),
        code_info.generate_branch_end(StoreMap, !MaybeEnd, SaveCode, !CI),
        Code = tree_list([TraceCode, GoalCode, SaveCode]),
        code_info.reset_to_position(BranchStart, !CI)
    ;
        % This case didn't occur in the original case list
        % - just generate a `fail' for it.
        Comment = "compiler-introduced `fail' case of dense switch",
        code_info.generate_failure(Code, !CI)
    ).

%----------------------------------------------------------------------------%
:- end_module dense_switch.
%----------------------------------------------------------------------------%
