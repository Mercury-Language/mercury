%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2000-2012 The University of Melbourne.
% Copyright (C) 2013-2024 The Mercury team.
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

:- import_module hlds.
:- import_module hlds.code_model.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module libs.
:- import_module libs.globals.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.set_of_var.

:- import_module assoc_list.
:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module pair.

%---------------------------------------------------------------------------%
%
% Stuff for dense switches.
%

    % type_range(ModuleInfo, TypeCtorCategory, Type, Min, Max,
    %   NumValuesInRange):
    %
    % Determine the range [Min..Max] of an atomic type, and the number of
    % values in that range (including both endpoints). Values within the range
    % are not necessarily used by the type.
    % Fail if the type isn't the sort of type that has a range
    % or if the type's range is too big to switch on (e.g. int).
    %
:- pred type_range(module_info::in, type_ctor_category::in, mer_type::in,
    int::out, int::out, int::out) is semidet.

    % switch_density(NumCases, NumValuesInRange):
    %
    % Calculate the percentage density given the range and the number of cases.
    %
:- func switch_density(int, int) = int.

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
    --->    need_range_check
    ;       dont_need_range_check.

    % dont_need_bit_vec_check_with_gaps should be used if the
    % generated lookup table is expected to contain dummy rows.
    % Otherwise, dont_need_bit_vec_check_no_gaps should be used.
    %
:- type need_bit_vec_check
    --->    need_bit_vec_check
    ;       dont_need_bit_vec_check_no_gaps
    ;       dont_need_bit_vec_check_with_gaps.

:- pred filter_out_failing_cases_if_needed(code_model::in,
    list(tagged_case)::in, list(tagged_case)::out,
    can_fail::in, can_fail::out) is det.

:- pred find_int_lookup_switch_params(module_info::in, mer_type::in,
    can_fail::in, int::in, int::in, int::in, int::in,
    need_bit_vec_check::out, need_range_check::out, int::out, int::out)
    is semidet.

:- pred project_all_to_one_solution(map(Key, soln_consts(Rval))::in,
    map(Key, list(Rval))::out) is semidet.

:- pred project_solns_to_rval_lists(assoc_list(T, soln_consts(Rval))::in,
    list(list(Rval))::in, list(list(Rval))::out) is det.

    % get_word_bits(Globals, WordBits, Log2WordBits):
    %
    % Return in WordBits the largest number of bits that
    % - fits into a word on the host machine
    % - fits into a word on the target machine
    % - is a power of 2.
    %
    % WordBits will be 2^Log2WordBits.
    %
    % We use this predicate to prevent cross-compilation errors when generating
    % bit vector tests for lookup switches by making sure that the bitvector
    % uses a number of bits that will fit both on this machine (so that
    % we can correctly generate it), and on the target machine (so that
    % it can be executed correctly). We require the number of bits to be
    % a power of 2, so that we implement division as right-shift.
    %
:- pred get_word_bits(globals::in, int::out, int::out) is det.

%---------------------------------------------------------------------------%
%
% These predicates are used in both the LLDS and MLDS backends
% when testing whether a switch is a lookup switch.
% ZZZ turn them into functions
%

    % If the cons_tag specifies an int_tag, return the int;
    % otherwise abort.
    %
:- pred get_int_tag(cons_tag::in, int::out) is det.

    % If the cons_tag specifies a string_tag, return the string;
    % otherwise abort.
    %
:- pred get_string_tag(cons_tag::in, string::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.string_encoding.
:- import_module check_hlds.
:- import_module check_hlds.type_util.
:- import_module libs.options.

:- import_module int.
:- import_module maybe.
:- import_module one_or_more.
:- import_module require.

%---------------------------------------------------------------------------%
%
% Stuff for dense switches.
%

type_range(ModuleInfo, TypeCtorCat, Type, Min, Max, NumValuesInRange) :-
    (
        TypeCtorCat = ctor_cat_builtin(cat_builtin_char),
        % Note also that some code in both dense_switch.m and in
        % lookup_switch.m assumes that min_char_value is 0.
        module_info_get_globals(ModuleInfo, Globals),
        globals.get_target(Globals, Target),
        target_char_range(Target, Min, Max)
    ;
        TypeCtorCat = ctor_cat_enum(cat_enum_mercury),
        type_to_ctor_det(Type, TypeCtor),
        module_info_get_type_table(ModuleInfo, TypeTable),
        lookup_type_ctor_defn(TypeTable, TypeCtor, TypeDefn),
        hlds_data.get_type_defn_body(TypeDefn, TypeBody),
        (
            TypeBody = hlds_du_type(TypeBodyDu),
            TypeBodyDu = type_body_du(OoMCtors, MaybeSuperType, _,
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
            )
        ;
            ( TypeBody = hlds_eqv_type(_)
            ; TypeBody = hlds_foreign_type(_)
            ; TypeBody = hlds_solver_type(_)
            ; TypeBody = hlds_abstract_type(_)
            ),
            unexpected($pred, "enum type is not d.u. type?")
        )
    ),
    NumValuesInRange = Max - Min + 1.

:- pred ctor_repns_int_tag_range(list(constructor_repn)::in,
    int::out, int::out) is semidet.

ctor_repns_int_tag_range([CtorRepn | CtorRepns], Min, Max) :-
    ConsTag = CtorRepn ^ cr_tag,
    get_int_tag(ConsTag, Int),
    list.foldl2(add_to_ctor_repn_int_tag_range, CtorRepns, Int, Min, Int, Max).

:- pred add_to_ctor_repn_int_tag_range(constructor_repn::in,
    int::in, int::out, int::in, int::out) is det.

add_to_ctor_repn_int_tag_range(CtorRepn, !Min, !Max) :-
    ConsTag = CtorRepn ^ cr_tag,
    get_int_tag(ConsTag, Int),
    int.min(Int, !Min),
    int.max(Int, !Max).

%---------------------------------------------------------------------------%

switch_density(NumCases, NumValuesInRange) = Density :-
    Density = (NumCases * 100) // NumValuesInRange.

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
    filter_out_failing_cases_loop(TaggedCases0, [], RevTaggedCases,
        !SwitchCanFail),
    list.reverse(RevTaggedCases, TaggedCases).

:- pred filter_out_failing_cases_loop(list(tagged_case)::in,
    list(tagged_case)::in, list(tagged_case)::out,
    can_fail::in, can_fail::out) is det.

filter_out_failing_cases_loop([], !RevTaggedCases, !SwitchCanFail).
filter_out_failing_cases_loop([TaggedCase | TaggedCases], !RevTaggedCases,
        !SwitchCanFail) :-
    TaggedCase = tagged_case(_, _, _, Goal),
    Goal = hlds_goal(GoalExpr, _),
    ( if GoalExpr = disj([]) then
        !:SwitchCanFail = can_fail
    else
        !:RevTaggedCases = [TaggedCase | !.RevTaggedCases]
    ),
    filter_out_failing_cases_loop(TaggedCases, !RevTaggedCases,
        !SwitchCanFail).

%---------------------------------------------------------------------------%

find_int_lookup_switch_params(ModuleInfo, SwitchVarType, SwitchCanFail,
        LowerLimit, UpperLimit, NumValues, ReqDensity,
        NeedBitVecCheck, NeedRangeCheck, FirstVal, LastVal) :-
    % We want to generate a lookup switch for any switch that is dense enough
    % - we don't care how many cases it has. A memory lookup tends to be
    % cheaper than a branch.
    Span = UpperLimit - LowerLimit,
    Range = Span + 1,
    Density = switch_density(NumValues, Range),
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
                TypeMin, TypeMax, TypeRange),
            DetDensity = switch_density(NumValues, TypeRange),
            DetDensity > ReqDensity
        then
            NeedRangeCheck = dont_need_range_check,
            NeedBitVecCheck = need_bit_vec_check,
            FirstVal = TypeMin,
            LastVal = TypeMax
        else
            % First check the variable is in range.
            NeedRangeCheck = need_range_check,
            % We will need to perform the bitvector test if the lookup table
            % is going to contain any gaps.
            ( if NumValues = Range then
                NeedBitVecCheck = dont_need_bit_vec_check_no_gaps
            else
                NeedBitVecCheck = need_bit_vec_check
            ),
            FirstVal = LowerLimit,
            LastVal = UpperLimit
        )
    ;
        SwitchCanFail = cannot_fail,
        % The cannot_fail guarantees that the values that are in range
        % but are not covered by any of the cases won't actually be reached.
        NeedRangeCheck = dont_need_range_check,
        % There may be gaps in the lookup table if switching on a variable of
        % a subtype which does not use some values in the range.
        ( if NumValues = Range then
            NeedBitVecCheck = dont_need_bit_vec_check_no_gaps
        else
            NeedBitVecCheck = dont_need_bit_vec_check_with_gaps
        ),
        FirstVal = LowerLimit,
        LastVal = UpperLimit
    ).

%---------------------------------------------------------------------------%

project_all_to_one_solution(CaseSolns, CaseValuePairs) :-
    map.map_values(project_soln_consts_to_one_soln, CaseSolns, CaseValuePairs).

:- pred project_soln_consts_to_one_soln(Key::in,
    soln_consts(Rval)::in, list(Rval)::out) is semidet.

project_soln_consts_to_one_soln(_Key, Solns, Values) :-
    Solns = one_soln(Values).

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

get_word_bits(Globals, WordBits, Log2WordBits) :-
    int.bits_per_int(HostWordBits),
    globals.lookup_int_option(Globals, bits_per_word, TargetWordBits),
    int.min(HostWordBits, TargetWordBits, WordBits0),
    % Round down to the nearest power of 2.
    Log2WordBits = log2_rounded_down(WordBits0),
    int.pow(2, Log2WordBits, WordBits).

:- func log2_rounded_down(int) = int.

% ZZZ
log2_rounded_down(X) = Log :-
    int.log2(X + 1, Log + 1).  % int.log2 rounds up

%---------------------------------------------------------------------------%

get_int_tag(ConsTag, Int) :-
    ( if ConsTag = int_tag(int_tag_int(IntPrime)) then
        Int = IntPrime
    else
        unexpected($pred, "not int_tag")
    ).


get_string_tag(ConsTag, Str) :-
    ( if ConsTag = string_tag(StrPrime) then
        Str = StrPrime
    else
        unexpected($pred, "not string_tag")
    ).

%---------------------------------------------------------------------------%
:- end_module backend_libs.lookup_switch_util.
%---------------------------------------------------------------------------%
