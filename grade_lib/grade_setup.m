%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Copyright (C) 2016-2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% This module provides predicates that set up a grade problem to be solved.
%
% The state of the solver consists of a set of solver variables, each with
% a set of possible values. A grade problem narrows the set of possible
% values of some of the solver variables. It can do so in one of two ways.
%
% 1:    A solver variable can be set to be equal to some value, which sets
%       all the other previously-possible values of that solver variable
%       to not possible.
%
% 2:    A solver variable can be set to not-equal some value, which sets
%       only that value of the variable to not possible.
%
% Some of the solver variables are intended to always start out with known
% values; these are the solver variables that record the results of
% autoconfiguration (by the configure script). The setup_solver_info predicate,
% which creates the initial solver_info, takes these results in the form
% of a value of the autoconf_results type.
%

:- module grade_lib.grade_setup.
:- interface.

:- import_module grade_lib.grade_spec.
:- import_module grade_lib.grade_state.

:- import_module maybe.

%---------------------------------------------------------------------------%
%
% The autoconf_results type and its components record the results
% of the autoconfiguration tests that are relevant to either the user-visible
% or to the link-check grades.
%

:- type autoconf_results
    --->    autoconf_results(
                autoconf_gcc_regs_avail,
                autoconf_gcc_gotos_avail,
                autoconf_gcc_labels_avail,
                autoconf_low_tag_bits_avail,
                autoconf_size_of_double,
                autoconf_merc_file
            ).

:- type autoconf_gcc_regs_avail
    --->    autoconf_gcc_regs_avail_no
    ;       autoconf_gcc_regs_avail_yes.

:- type autoconf_gcc_gotos_avail
    --->    autoconf_gcc_gotos_avail_no
    ;       autoconf_gcc_gotos_avail_yes.

:- type autoconf_gcc_labels_avail
    --->    autoconf_gcc_labels_avail_no
    ;       autoconf_gcc_labels_avail_yes.

:- type autoconf_low_tag_bits_avail
    --->    autoconf_low_tag_bits_avail_0
    ;       autoconf_low_tag_bits_avail_2
    ;       autoconf_low_tag_bits_avail_3.

:- type autoconf_size_of_double
    --->    autoconf_size_of_double_eq_ptr
    ;       autoconf_size_of_double_ne_ptr.

:- type autoconf_merc_file
    --->    autoconf_merc_file_no
    ;       autoconf_merc_file_yes.

    % setup_solver_info(SpecsVersion, AutoconfResults, SolverInfo):
    %
    % Create a blank solver_info that records the results of autoconfiguration
    % as given by AutoconfResults, but constrains no other solver variables.
    %
    % SpecsVersion controls which version of the grade implications we put
    % into SolverInfo. (See grade_spec.m for the meanings of the versions.)
    %
:- pred setup_solver_info(specs_version::in, autoconf_results::in,
    solver_info::out) is det.

%---------------------------------------------------------------------------%

    % assign_var_in_map(WhyNot, VarId, ValueId, !SolverVarMap):
    %
    % Impose the constraint VarId = ValueId on !:SolverVarMap.
    % Record WhyNot as the reason why VarId cannot have any value
    % other than ValueId.
    %
    % Abort if ValueId was never a possible value of VarId, or if the
    % constraint this call is trying to impose is inconsistent with
    % a constraint that was imposed earlier, on !.SolverVarMap.
    %
:- pred assign_var_in_map(not_possible_why::in,
    solver_var_id::in, solver_var_value_id::in,
    solver_var_map::in, solver_var_map::out) is det.

%---------------------------------------------------------------------------%

:- type solver_var_set_to
    --->    set_to_false
    ;       set_to_true.

    % set_solver_var(VarName, ValueName, VarId, ValueId, SetTo, WhyNot,
    %   MaybeError, !SolverInfo):
    %
    % If SetTo = set_to_true, then impose the constraint VarId = ValueId
    % on !:SolverInfo, and record WhyNot as the reason why VarId cannot have
    % any value other than ValueId.
    % If SetTo = set_to_false, then impose the constraint VarId != ValueId
    % on !:SolverInfo, and record WhyNot as the reason why VarId cannot have
    % ValueId as its value.
    %
    % Return MaybeError = ok if adding the constraint worked.
    % If it did not work, either because ValueId was never a possible value
    % for VarId, or because the constraint this call is trying to impose
    % is inconsistent with one that we have imposed earlier on !.SolverInfo,
    % then return MaybeError = error(Msg), where Msg describes the problem.
    % Use VarName and ValueName as the names of the variable and the value
    % respectively in Msg.
    %
:- pred set_solver_var(string::in, string::in,
    solver_var_id::in, solver_var_value_id::in,
    solver_var_set_to::in, not_possible_why::in, maybe_error::out,
    solver_info::in, solver_info::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module var_value_names.

:- import_module cord.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module require.
:- import_module set.
:- import_module string.

%---------------------------------------------------------------------------%

setup_solver_info(SpecsVersion, AutoconfResults, !:SolverInfo) :-
    setup_solver_vars(SpecsVersion, SolverVarMap, SolverVarPriority),
    setup_requirements(SolverVarMap, Requirements),
    !:SolverInfo = solver_info(Requirements, SolverVarPriority, SolverVarMap),

    AutoconfResults = autoconf_results(GccRegsAvail, GccGotosAvail,
        GccLabelsAvail, LowTagBitsAvail, SizeOfDouble, MercFile),
    (
        GccRegsAvail = autoconf_gcc_regs_avail_no,
        GccRegsAvailValue = svalue_ac_gcc_regs_avail_no
    ;
        GccRegsAvail = autoconf_gcc_regs_avail_yes,
        GccRegsAvailValue = svalue_ac_gcc_regs_avail_yes
    ),
    (
        GccGotosAvail = autoconf_gcc_gotos_avail_no,
        GccGotosAvailValue = svalue_ac_gcc_gotos_avail_no
    ;
        GccGotosAvail = autoconf_gcc_gotos_avail_yes,
        GccGotosAvailValue = svalue_ac_gcc_gotos_avail_yes
    ),
    (
        GccLabelsAvail = autoconf_gcc_labels_avail_no,
        GccLabelsAvailValue = svalue_ac_gcc_labels_avail_no
    ;
        GccLabelsAvail = autoconf_gcc_labels_avail_yes,
        GccLabelsAvailValue = svalue_ac_gcc_labels_avail_yes
    ),
    (
        LowTagBitsAvail = autoconf_low_tag_bits_avail_0,
        LowTagBitsAvailValue = svalue_ac_low_tag_bits_avail_0
    ;
        LowTagBitsAvail = autoconf_low_tag_bits_avail_2,
        LowTagBitsAvailValue = svalue_ac_low_tag_bits_avail_2
    ;
        LowTagBitsAvail = autoconf_low_tag_bits_avail_3,
        LowTagBitsAvailValue = svalue_ac_low_tag_bits_avail_3
    ),
    (
        SizeOfDouble = autoconf_size_of_double_eq_ptr,
        SizeOfDoubleValue = svalue_ac_size_of_double_eq_ptr
    ;
        SizeOfDouble = autoconf_size_of_double_ne_ptr,
        SizeOfDoubleValue = svalue_ac_size_of_double_ne_ptr
    ),
    (
        MercFile = autoconf_merc_file_no,
        MercFileValue = svalue_ac_merc_file_no
    ;
        MercFile = autoconf_merc_file_yes,
        MercFileValue = svalue_ac_merc_file_yes
    ),
    assign_autoconf_var(svar_ac_gcc_regs_avail, GccRegsAvailValue,
        !SolverInfo),
    assign_autoconf_var(svar_ac_gcc_gotos_avail, GccGotosAvailValue,
        !SolverInfo),
    assign_autoconf_var(svar_ac_gcc_labels_avail, GccLabelsAvailValue,
        !SolverInfo),
    assign_autoconf_var(svar_ac_low_tag_bits_avail, LowTagBitsAvailValue,
        !SolverInfo),
    assign_autoconf_var(svar_ac_size_of_double, SizeOfDoubleValue,
        !SolverInfo),
    assign_autoconf_var(svar_ac_merc_file, MercFileValue, !SolverInfo).

:- pred assign_autoconf_var(solver_var_id::in, solver_var_value_id::in,
    solver_info::in, solver_info::out) is det.

assign_autoconf_var(VarId, ValueId, !SolverInfo) :-
    SolverVarMap0 = !.SolverInfo ^ si_solver_var_map,
    assign_var_in_map(npw_config, VarId, ValueId, SolverVarMap0, SolverVarMap),
    !SolverInfo ^ si_solver_var_map := SolverVarMap.

assign_var_in_map(WhyNot, VarId, ValueId, !SolverMap) :-
    solver_var_name(VarName, VarId),
    solver_var_value_name(ValueName, ValueId),
    set_solver_var_in_map(VarName, ValueName, VarId, ValueId, set_to_true,
        WhyNot, MaybeError, !SolverMap),
    (
        MaybeError = ok
    ;
        MaybeError = error(Msg),
        unexpected($pred, Msg)
    ).

%---------------------------------------------------------------------------%

:- pred setup_solver_vars(specs_version::in,
    solver_var_map::out, list(solver_var_id)::out) is det.

setup_solver_vars(SpecsVersion, SolverVarMap, SolverVarPriority) :-
    SolverVarSpecs = init_solver_var_specs(SpecsVersion),
    init_solver_vars(SolverVarSpecs, map.init, SolverVarMap,
        cord.init, SolverVarPriorityCord),
    SolverVarPriority = cord.to_list(SolverVarPriorityCord).

:- pred init_solver_vars(list(solver_var_spec)::in,
    solver_var_map::in, solver_var_map::out,
    cord(solver_var_id)::in, cord(solver_var_id)::out) is det.

init_solver_vars([], !SolverVarMap, !SolverVarPriorityCord).
init_solver_vars([Spec | Specs], !SolverVarMap, !SolverVarPriorityCord) :-
    Spec = solver_var_spec(SolverVarId, SolverValueIds),
    !:SolverVarPriorityCord = cord.snoc(!.SolverVarPriorityCord, SolverVarId),
    init_solver_var_values(0, NumValues, SolverValueIds, Values),
    SolverVar = solver_var(NumValues, NumValues, Values),
    expect_not(unify(0, NumValues), $pred, "no values for solver var"),
    map.det_insert(SolverVarId, SolverVar, !SolverVarMap),
    init_solver_vars(Specs, !SolverVarMap, !SolverVarPriorityCord).

:- pred init_solver_var_values(int::in, int::out,
    list(solver_var_value_id)::in, list(solver_var_value)::out) is det.

init_solver_var_values(CurNumValues, CurNumValues, [], []).
init_solver_var_values(CurNumValues, NumValues,
        [ValueId | ValueIds], [Value | Values]) :-
    Value = solver_var_value(ValueId, is_possible),
    init_solver_var_values(CurNumValues + 1, NumValues, ValueIds, Values).

%---------------------------------------------------------------------------%

set_solver_var(VarName, ValueName, VarId, ValueId, SetTo, WhyNot,
        MaybeError, !SolverInfo) :-
    SolverVarMap0 = !.SolverInfo ^ si_solver_var_map,
    set_solver_var_in_map(VarName, ValueName, VarId, ValueId, SetTo, WhyNot,
        MaybeError, SolverVarMap0, SolverVarMap),
    !SolverInfo ^ si_solver_var_map := SolverVarMap.

:- pred set_solver_var_in_map(string::in, string::in,
    solver_var_id::in, solver_var_value_id::in,
    solver_var_set_to::in, not_possible_why::in, maybe_error::out,
    solver_var_map::in, solver_var_map::out) is det.

set_solver_var_in_map(VarName, ValueName, VarId, ValueId, SetTo, WhyNot,
        MaybeError, !SolverVarMap) :-
    map.lookup(!.SolverVarMap, VarId, SolverVar0),
    SolverVar0 = solver_var(CntAll, CntPoss0, Values0),
    (
        SetTo = set_to_true,
        set_value_to_true(ValueId, WhyNot, [], OldPossibles, Values0, Values)
    ;
        SetTo = set_to_false,
        set_value_to_false(ValueId, WhyNot, [], OldPossibles, Values0, Values)
    ),
    (
        OldPossibles = [],
        string.format("solver variable %s has no value named %s\n",
            [s(VarName), s(ValueName)], Msg),
        MaybeError = error(Msg)
    ;
        OldPossibles = [OldPossible],
        (
            SetTo = set_to_true,
            (
                OldPossible = is_possible,
                % This is the expected case. VarId may still be ValueId,
                % but now it cannot be any other value.
                MaybeError = ok,
                CntPoss = 1,
                SolverVar = solver_var(CntAll, CntPoss, Values),
                map.det_update(VarId, SolverVar, !SolverVarMap)
            ;
                OldPossible = not_possible(_),
                % Other parameter settings have already ruled out
                % VarId = ValueId, so this is an inconsistency
                % between those earlier settings and this one.
                string.format(
                    "inconsistent settings for solver variable %s\n",
                    [s(VarName)], Msg),
                MaybeError = error(Msg)
            )
        ;
            SetTo = set_to_false,
            (
                OldPossible = is_possible,
                % If the variable could have had this value before,
                % that is expected.
                MaybeError = ok,
                CntPoss = CntPoss0 - 1,
                SolverVar = solver_var(CntAll, CntPoss, Values),
                map.det_update(VarId, SolverVar, !SolverVarMap)
            ;
                OldPossible = not_possible(_),
                % If the variable having this value had been ruled out before,
                % our caller asking us to set it to not possible is redundant,
                % but ok.
                MaybeError = ok
            )
        )
    ;
        OldPossibles = [_, _ | _],
        string.format(
            "solver var %s has more than one copy of value %s\n",
            [s(VarName), s(ValueName)], Msg),
        % This should have caught during setup.
        unexpected($pred, Msg)
    ).

:- pred set_value_to_false(solver_var_value_id::in, not_possible_why::in,
    list(solver_var_value_possible)::in, list(solver_var_value_possible)::out,
    list(solver_var_value)::in, list(solver_var_value)::out) is det.

set_value_to_false(_SetId, _WhyNot, !OldPossibles, [], []).
set_value_to_false(SetId, WhyNot, !OldPossibles,
        [HeadValue0 | TailValues0], [HeadValue | TailValues]) :-
    HeadValue0 = solver_var_value(Id, Possible0),
    ( if SetId = Id then
        !:OldPossibles = [Possible0 | !.OldPossibles],
        Possible = not_possible(WhyNot),
        HeadValue = solver_var_value(Id, Possible)
    else
        HeadValue = HeadValue0
    ),
    set_value_to_false(SetId, WhyNot, !OldPossibles, TailValues0, TailValues).

:- pred set_value_to_true(solver_var_value_id::in, not_possible_why::in,
    list(solver_var_value_possible)::in, list(solver_var_value_possible)::out,
    list(solver_var_value)::in, list(solver_var_value)::out) is det.

set_value_to_true(_SetId, _WhyNot, !OldPossibles, [], []).
set_value_to_true(SetId, WhyNot, !OldPossibles,
        [HeadValue0 | TailValues0], [HeadValue | TailValues]) :-
    HeadValue0 = solver_var_value(Id, Possible0),
    ( if SetId = Id then
        !:OldPossibles = [Possible0 | !.OldPossibles],
        Possible = is_possible,
        HeadValue = solver_var_value(Id, Possible)
    else
        Possible = not_possible(WhyNot),
        HeadValue = solver_var_value(Id, Possible)
    ),
    set_value_to_true(SetId, WhyNot, !OldPossibles, TailValues0, TailValues).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- pred setup_requirements(solver_var_map::in, list(requirement)::out) is det.

setup_requirements(SolverVarMap, Requirements) :-
    RequirementSpecs = init_requirement_specs,
    convert_and_check_requirements(0, SolverVarMap,
        RequirementSpecs, Requirements, BadRequirements),
    (
        BadRequirements = []
    ;
        BadRequirements = [_ | _],
        trace [io(!IO)] (
            io.write(BadRequirements, !IO)
        ),
        unexpected($pred, "some bad requirements")
    ).

:- pred convert_and_check_requirements(int::in, solver_var_map::in,
    list(requirement_spec)::in, list(requirement)::out, list(requirement)::out)
    is det.

convert_and_check_requirements(_CurId, _SolverVarMap, [], [], []).
convert_and_check_requirements(CurId, SolverVarMap,
        [ReqSpec | ReqSpecs], OkRequirements, BadRequirements) :-
    convert_and_check_requirements(CurId + 1, SolverVarMap, ReqSpecs,
        OkRequirementsTail, BadRequirementsTail),
    ReqSpec = requirement_spec(ReqDesc,
        (IfVar `being` IfValue) `implies_that`
        (ThenVar `is_one_of` ThenValues)),
    Requirement = requirement(requirement_id(CurId), ReqDesc,
        IfVar, IfValue, ThenVar, ThenValues),
    ( if
        % Both variable ids in the requirement must be in SolverMap,
        % and all the value ids must be in the SolverMap entries
        % of their respective variables. If they are not, the requirement
        % does not make sense.
        known_var_values(SolverVarMap, IfVar, [IfValue]),
        known_var_values(SolverVarMap, ThenVar, ThenValues),

        % ReqSpec represents the implication:
        %
        %   (IfVar = IfValue) -> (ThenVar in ThenValues)
        %
        % If ThenValues = [], then the requirement is *never* satisfiable.
        ThenValues = [_ | _]
    then
        OkRequirements = [Requirement | OkRequirementsTail],
        BadRequirements = BadRequirementsTail
    else
        OkRequirements = OkRequirementsTail,
        BadRequirements = [Requirement | BadRequirementsTail]
    ).

:- pred known_var_values(solver_var_map::in,
    solver_var_id::in, list(solver_var_value_id)::in) is semidet.

known_var_values(SolverVarMap, SearchVarId, SearchValueId) :-
    map.search(SolverVarMap, SearchVarId, SolverVar),
    SolverVar = solver_var(_, _, VarValues),
    set.list_to_set(SearchValueId, SearchSet),
    ValueId = list.map(solver_var_value_project_id, VarValues),
    set.list_to_set(ValueId, Set),
    set.subset(SearchSet, Set).

:- func solver_var_value_project_id(solver_var_value)
    = solver_var_value_id.

solver_var_value_project_id(SolverVarValue) = SolverVarValue ^ svv_id.

%---------------------------------------------------------------------------%
:- end_module grade_lib.grade_setup.
%---------------------------------------------------------------------------%
