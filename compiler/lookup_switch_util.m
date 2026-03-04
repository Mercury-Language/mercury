%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2000-2012 The University of Melbourne.
% Copyright (C) 2013-2026 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: lookup_switch_util.m.
% Authors: fjh, zs.
%
% This module defines stuff for generating dense and lookup switches
% that is shared between the MLDS and LLDS back-ends.
%
%---------------------------------------------------------------------------%

:- module backend_libs.lookup_switch_util.
:- interface.

:- import_module backend_libs.switch_util.
:- import_module hlds.
:- import_module hlds.code_model.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.set_of_var.

:- import_module assoc_list.
:- import_module bool.
:- import_module list.
:- import_module map.

%---------------------------------------------------------------------------%
%
% Stuff for dense switches.
%

    % type_range(ModuleInfo, TypeCtorCategory, Type,
    %   NumValuesInRange, Limits):
    %
    % Determine the range [Min..Max] of an atomic type, and the number of
    % values in that range (including both endpoints). Values within the range
    % are not necessarily used by the type.
    % Fail if the type isn't the sort of type that has a range, or
    % if the type's range is too big to do a *dense* switch on (e.g. int).
    %
:- pred type_range(module_info::in, type_ctor_category::in, mer_type::in,
    uint::out, int_switch_limits::out) is semidet.

    % switch_density(NumCases, NumValuesInRange):
    %
    % Calculate the percentage density given the range and the number of cases.
    %
:- func switch_density(uint, uint) = uint.

%---------------------------------------------------------------------------%
%
% Stuff for lookup switches.
%

:- type case_consts(Key, Rval, SeveralInfo)
    --->    all_one_soln(
                map(Key, list(Rval))
            )
    ;       some_several_solns(
                map(Key, soln_consts(Rval)),
                SeveralInfo
            ).

:- type case_consts_several_llds
    --->    case_consts_several_llds(
                % The resume vars.
                set_of_progvar,

                % The Boolean "or" of the result of invoking
                % goal_may_modify_trail on the goal_infos of the switch arms
                % that are disjunctions.
                bool
            ).

:- type soln_consts(Rval)
    --->    one_soln(list(Rval))
    ;       several_solns(list(Rval), list(list(Rval))).
            % The first solution, and all the later solutions.

:- type need_range_check
    --->    do_not_need_range_check
    ;       need_range_check.

    % do_not_need_bit_vec_check_with_gaps should be used if the
    % generated lookup table is expected to contain dummy rows.
    % Otherwise, do_not_need_bit_vec_check_no_gaps should be used.
    %
:- type need_bit_vec_check
    --->    do_not_need_bit_vec_check_no_gaps
    ;       do_not_need_bit_vec_check_with_gaps
    ;       need_bit_vec_check.

:- pred filter_out_failing_cases_if_needed(code_model::in,
    list(tagged_case)::in, list(tagged_case)::out,
    can_fail::in, can_fail::out) is det.

:- pred find_int_lookup_switch_params(module_info::in, mer_type::in,
    can_fail::in, int_switch_info::in, uint::in,
    need_bit_vec_check::out, need_range_check::out,
    int_switch_limits::out) is semidet.

:- pred project_all_to_one_solution(map(Key, soln_consts(Rval))::in,
    map(Key, list(Rval))::out) is semidet.

:- pred project_solns_to_rval_lists(assoc_list(T, soln_consts(Rval))::in,
    list(list(Rval))::in, list(list(Rval))::out) is det.

%---------------------------------------------------------------------------%
%
% These predicates are used in both the LLDS and MLDS backends
% when testing whether a switch is a lookup switch.
%
% Note that for switches on (signed or unsigned, sized or not) integers,
% we should have checked that both the min and max tag values fit
% into the range of an int32. Therefore this should also be true for
% all the case tags we test afterwards.
%

    % If the int_tag specifies a value that fits in an int32,
    % return the value; otherwise, abort.
    %
:- func get_int32_tag_value(int_tag) = int32.

    % If the cons_tag specifies an int_tag whose value fits in an int32,
    % return the value; otherwise, abort.
    %
:- func get_int_in_cons_tag(cons_tag) = int.

    % If the cons_tag specifies a string_tag, return the string;
    % otherwise, abort.
    %
:- func get_string_in_cons_tag(cons_tag) = string.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.string_encoding.
:- import_module hlds.type_util.
:- import_module libs.
:- import_module libs.globals.

:- import_module cord.
:- import_module int.
:- import_module int32.
:- import_module maybe.
:- import_module one_or_more.
:- import_module pair.
:- import_module require.
:- import_module uint.
:- import_module uint8.
:- import_module uint16.
:- import_module uint32.
:- import_module uint64.

%---------------------------------------------------------------------------%
%
% Stuff for dense switches.
%

type_range(ModuleInfo, TypeCtorCat, Type, NumValuesInRangeU, Limits) :-
    (
        TypeCtorCat = ctor_cat_builtin(cat_builtin_char),
        % Note also that some code in both dense_switch.m and in
        % lookup_switch.m assumes that min_char_value is 0.
        module_info_get_globals(ModuleInfo, Globals),
        globals.get_target(Globals, Target),
        target_char_range(Target, Min, Max),
        ( if
            int32.from_int(Min, Min32),
            int32.from_int(Max, Max32)
        then
            NumValuesInRangeI32 = Max32 - Min32 + 1i32,
            NumValuesInRangeI = int32.cast_to_int(NumValuesInRangeI32),
            NumValuesInRangeU = uint.cast_from_int(NumValuesInRangeI),
            Limits = int_switch_limits(Min32, Max32)
        else
            unexpected($pred, "char range does not fit in int32")
        )
    ;
        TypeCtorCat = ctor_cat_enum(cat_enum_mercury),
        type_to_ctor_det(Type, TypeCtor),
        module_info_get_type_table(ModuleInfo, TypeTable),
        lookup_type_ctor_defn(TypeTable, TypeCtor, TypeDefn),
        hlds_data.get_type_defn_body(TypeDefn, TypeBody),
        (
            TypeBody = hlds_du_type(TypeBodyDu),
            TypeBodyDu = type_body_du(OoMCtors, _, MaybeSuperType, _,
                MaybeRepn, _),
            (
                MaybeRepn = yes(Repn)
            ;
                MaybeRepn = no,
                unexpected($pred, "MaybeRepn = no")
            ),
            (
                MaybeSuperType = not_a_subtype,
                Min = 0,
                OoMCtors = one_or_more(_HeadCtor, TailCtors),
                list.length(TailCtors, NumTailConstructors),
                % NumConstructors = 1 + NumTailConstructors
                % Max = NumConstructors - 1
                Max = NumTailConstructors
            ;
                MaybeSuperType = subtype_of(_),
                % A subtype enum does not necessarily use all values from 0 to
                % the max.
                CtorRepns = Repn ^ dur_ctor_repns,
                ctor_repns_int_tag_range(CtorRepns, Min, Max)
            ),
            ( if
                int32.from_int(Min, Min32),
                int32.from_int(Max, Max32)
            then
                NumValuesInRangeI32 = Max32 - Min32 + 1i32,
                NumValuesInRangeI = int32.cast_to_int(NumValuesInRangeI32),
                NumValuesInRangeU = uint.cast_from_int(NumValuesInRangeI),
                Limits = int_switch_limits(Min32, Max32)
            else
                fail
            )
        ;
            ( TypeBody = hlds_eqv_type(_)
            ; TypeBody = hlds_foreign_type(_)
            ; TypeBody = hlds_solver_type(_)
            ; TypeBody = hlds_abstract_type(_)
            ),
            unexpected($pred, "enum type is not d.u. type?")
        )
    ).

:- pred ctor_repns_int_tag_range(list(constructor_repn)::in,
    int::out, int::out) is semidet.

ctor_repns_int_tag_range([CtorRepn | CtorRepns], Min, Max) :-
    ConsTag = CtorRepn ^ cr_tag,
    Int = get_enum_int_in_cons_tag(ConsTag),
    list.foldl2(add_to_ctor_repn_int_tag_range, CtorRepns, Int, Min, Int, Max).

:- pred add_to_ctor_repn_int_tag_range(constructor_repn::in,
    int::in, int::out, int::in, int::out) is det.

add_to_ctor_repn_int_tag_range(CtorRepn, !Min, !Max) :-
    ConsTag = CtorRepn ^ cr_tag,
    Int = get_enum_int_in_cons_tag(ConsTag),
    int.min(Int, !Min),
    int.max(Int, !Max).

%---------------------------------------------------------------------------%

switch_density(NumCases, NumValuesInRange) = Density :-
    Density = (NumCases * 100u) // NumValuesInRange.

%---------------------------------------------------------------------------%
%
% Stuff for lookup switches.
%

filter_out_failing_cases_if_needed(CodeModel, !TaggedCases, !SwitchCanFail) :-
    (
        ( CodeModel = model_non
        ; CodeModel = model_semi
        ),
        filter_out_failing_cases(!TaggedCases, !SwitchCanFail)
    ;
        CodeModel = model_det
    ).

:- pred filter_out_failing_cases(list(tagged_case)::in, list(tagged_case)::out,
    can_fail::in, can_fail::out) is det.

filter_out_failing_cases(TaggedCases0, TaggedCases, !SwitchCanFail) :-
    filter_out_failing_cases_loop(TaggedCases0, cord.init, TaggedCasesCord,
        !SwitchCanFail),
    TaggedCases = cord.list(TaggedCasesCord).

:- pred filter_out_failing_cases_loop(list(tagged_case)::in,
    cord(tagged_case)::in, cord(tagged_case)::out,
    can_fail::in, can_fail::out) is det.

filter_out_failing_cases_loop([], !TaggedCasesCord, !SwitchCanFail).
filter_out_failing_cases_loop([TaggedCase | TaggedCases], !TaggedCasesCord,
        !SwitchCanFail) :-
    TaggedCase = tagged_case(_, _, _, Goal),
    Goal = hlds_goal(GoalExpr, _),
    ( if GoalExpr = disj([]) then
        !:SwitchCanFail = can_fail
    else
        cord.snoc(TaggedCase, !TaggedCasesCord)
    ),
    filter_out_failing_cases_loop(TaggedCases, !TaggedCasesCord,
        !SwitchCanFail).

%---------------------------------------------------------------------------%

find_int_lookup_switch_params(ModuleInfo, SwitchVarType, SwitchCanFail,
        IntSwitchInfo, ReqDensity, NeedBitVecCheck, NeedRangeCheck, Limits) :-
    IntSwitchInfo = int_switch_info(_IntType,
        NumValuesInRange, NumValuesInCases, CaseLimits),

    % We want to generate a lookup switch for any switch that is dense enough
    % - we don't care how many cases it has. A memory lookup tends to be
    % cheaper than a branch.
    Density = switch_density(NumValuesInCases, NumValuesInRange),
    Density > ReqDensity,

    (
        SwitchCanFail = can_fail,
        % For can_fail switches, we normally need to check that the variable
        % is in range before we index into the jump table. However, if the
        % range of the type is sufficiently small, we can make the jump table
        % large enough to hold all of the values for the type (with gaps),
        % but then we will need to do the bitvector test.
        classify_type(ModuleInfo, SwitchVarType) = TypeCategory,
        ( if
            type_range(ModuleInfo, TypeCategory, SwitchVarType,
                NumValuesInType, TypeLimits),
            DetDensity = switch_density(NumValuesInCases, NumValuesInType),
            DetDensity > ReqDensity
        then
            NeedRangeCheck = do_not_need_range_check,
            NeedBitVecCheck = need_bit_vec_check,
            Limits = TypeLimits
        else
            % First check the variable is in range.
            NeedRangeCheck = need_range_check,
            % We will need to perform the bitvector test if the lookup table
            % is going to contain any gaps.
            ( if NumValuesInCases = NumValuesInRange then
                NeedBitVecCheck = do_not_need_bit_vec_check_no_gaps
            else
                NeedBitVecCheck = need_bit_vec_check
            ),
            Limits = CaseLimits
        )
    ;
        SwitchCanFail = cannot_fail,
        % The cannot_fail guarantees that the values that are in range
        % but are not covered by any of the cases won't actually be reached.
        NeedRangeCheck = do_not_need_range_check,
        % There may be gaps in the lookup table, for example when
        % switching on a variable of a subtype which does not use
        % some values in the range.
        ( if NumValuesInCases = NumValuesInRange then
            NeedBitVecCheck = do_not_need_bit_vec_check_no_gaps
        else
            NeedBitVecCheck = do_not_need_bit_vec_check_with_gaps
        ),
        Limits = CaseLimits
    ).

%---------------------------------------------------------------------------%

project_all_to_one_solution(CaseSolnsMap, CaseValuesMap) :-
    map.map_values(project_soln_consts_to_one_soln,
        CaseSolnsMap, CaseValuesMap).

:- pred project_soln_consts_to_one_soln(Key::in,
    soln_consts(Rval)::in, list(Rval)::out) is semidet.

project_soln_consts_to_one_soln(_Key, Solns, Values) :-
    Solns = one_soln(Values).

%---------------------------------------------------------------------------%

project_solns_to_rval_lists([], !RvalsList).
project_solns_to_rval_lists([Case | Cases], !RvalsList) :-
    Case = _Index - Soln,
    (
        Soln = one_soln(Rvals),
        !:RvalsList = [Rvals | !.RvalsList]
    ;
        Soln = several_solns(FirstSolnRvals, LaterSolnsRvalsList),
        !:RvalsList = [FirstSolnRvals | LaterSolnsRvalsList] ++ !.RvalsList
    ),
    project_solns_to_rval_lists(Cases, !RvalsList).

%---------------------------------------------------------------------------%

get_int32_tag_value(IntTag) = I32 :-
    (
        IntTag = int_tag_int(IW),
        I32 = int32.det_from_int(IW)
    ;
        IntTag = int_tag_int8(I8),
        I32 = int32.cast_from_int8(I8)
    ;
        IntTag = int_tag_int16(I16),
        I32 = int32.cast_from_int16(I16)
    ;
        IntTag = int_tag_int32(I32)
    ;
        IntTag = int_tag_int64(I64),
        % XXX We should test for fit, and abort if needed.
        I32 = int32.cast_from_int64(I64)
    ;
        IntTag = int_tag_uint(U),
        I32 = int32.cast_from_int(uint.cast_to_int(U))
    ;
        IntTag = int_tag_uint8(U8),
        I32 = int32.cast_from_int(uint8.cast_to_int(U8))
    ;
        IntTag = int_tag_uint16(U16),
        I32 = int32.cast_from_int(uint16.cast_to_int(U16))
    ;
        IntTag = int_tag_uint32(U32),
        % XXX We should test for fit, and abort if needed.
        I32 = int32.cast_from_int(uint32.cast_to_int(U32))
    ;
        IntTag = int_tag_uint64(U64),
        % XXX We should test for fit, and abort if needed.
        I32 = int32.cast_from_int(uint64.cast_to_int(U64))
    ).

:- func get_enum_int_in_cons_tag(cons_tag) = int.

get_enum_int_in_cons_tag(ConsTag) = I :-
    ( if ConsTag = int_tag(int_tag_int(IPrime)) then
        I = IPrime
    else
        unexpected($pred, "not int_tag_int")
    ).

get_int_in_cons_tag(ConsTag) = I :-
    ( if ConsTag = int_tag(IntTag) then
        I32 = get_int32_tag_value(IntTag),
        I = int32.cast_to_int(I32)
    else
        unexpected($pred, "not int_tag")
    ).

get_string_in_cons_tag(ConsTag) = Str :-
    ( if ConsTag = string_tag(StrPrime) then
        Str = StrPrime
    else
        unexpected($pred, "not string_tag")
    ).

%---------------------------------------------------------------------------%
:- end_module backend_libs.lookup_switch_util.
%---------------------------------------------------------------------------%
