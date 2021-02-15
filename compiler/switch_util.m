%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2000-2012 The University of Melbourne.
% Copyright (C) 2013-2018 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: switch_util.m.
% Authors: fjh, zs.
%
% This module defines stuff for generating switches that is shared
% between the MLDS and LLDS back-ends.
%
%-----------------------------------------------------------------------------%

:- module backend_libs.switch_util.
:- interface.

:- import_module backend_libs.rtti.         % for sectag_locn
:- import_module backend_libs.builtin_ops.
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
:- import_module set.

%-----------------------------------------------------------------------------%
%
% General stuff, for adding tags to cons_ids in switches and for representing
% switch arms.
%

:- type maybe_int_switch_info
    --->    int_switch(int_switch_info(int))
    ;       uint_switch(int_switch_info(uint))
    ;       int8_switch(int_switch_info(int8))
    ;       uint8_switch(int_switch_info(uint8))
    ;       int16_switch(int_switch_info(int16))
    ;       uint16_switch(int_switch_info(uint16))
    ;       int32_switch(int_switch_info(int32))
    ;       uint32_switch(int_switch_info(uint32))
    ;       int64_switch(int_switch_info(int64))
    ;       uint64_switch(int_switch_info(uint64))
    ;       not_int_switch.

:- type int_switch_info(T)
    --->    int_switch_info(
                lower_limt  :: T,
                upper_limit :: T,
                num_values  :: int
            ).

    % tag_cases(ModuleInfo, Type, Cases, TaggedCases, MaybeIntSwitchInfo):
    %
    % Given a switch on a variable of type Type, tag each case in Cases
    % with the tags corresponding to its cons_ids. If all tags are integers,
    % return the lower and upper limits on these integers, as well as a count
    % of how many of them there are.
    %
:- pred tag_cases(module_info::in, mer_type::in, list(case)::in,
    list(tagged_case)::out, maybe_int_switch_info::out) is det.

    % num_cons_ids_in_tagged_cases(Cases, NumConsIds, NumArms):
    %
    % Count the number of cons_ids and the number of arms in Cases.
    %
:- pred num_cons_ids_in_tagged_cases(list(tagged_case)::in, int::out, int::out)
    is det.

%-----------------------------------------------------------------------------%
%
% Stuff for categorizing switches.
%

:- type switch_category
    --->    atomic_switch
            % A switch on int, uint, char, enum or a 8-, 16 or 32-bit signed or
            % unsigned integer.

    ;       int64_switch
            % A switch on a 64-bit integer.
            % These require special treatment on the Java backend.

    ;       string_switch
    ;       tag_switch
    ;       float_switch.

    % Convert a type constructor category to a switch category.
    %
:- func type_ctor_cat_to_switch_cat(type_ctor_category) = switch_category.

    % Return an estimate of the runtime cost of a constructor test for the
    % given tag. We try to put the cheap tests first.
    %
    % Abort on cons_tags that should never be switched on.
    %
:- func estimate_switch_tag_test_cost(cons_tag) = int.

:- type may_use_smart_indexing
    --->    may_not_use_smart_indexing
    ;       may_use_smart_indexing.

    % Succeeds if smart indexing for the given switch category has been
    % disabled by the user on the command line.
    %
:- pred find_switch_category(module_info::in, mer_type::in,
    switch_category::out, may_use_smart_indexing::out) is det.

%-----------------------------------------------------------------------------%
%
% Stuff for dense switches.
%

    % type_range(ModuleInfo, TypeCtorCategory, Type, Min, Max, NumValues):
    %
    % Determine the range [Min..Max] of an atomic type, and the number of
    % values in that range (including both endpoints).
    % Fail if the type isn't the sort of type that has a range
    % or if the type's range is too big to switch on (e.g. int).
    %
:- pred type_range(module_info::in, type_ctor_category::in, mer_type::in,
    int::out, int::out, int::out) is semidet.

    % Calculate the percentage density given the range and the number of cases.
    %
:- func switch_density(int, int) = int.

%-----------------------------------------------------------------------------%
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

:- type need_bit_vec_check
    --->    need_bit_vec_check
    ;       dont_need_bit_vec_check.

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

%-----------------------------------------------------------------------------%
%
% Stuff for string hash switches.
%

:- type string_hash_slot(CaseRep)
    --->    string_hash_slot(string, int, CaseRep).

:- type table_size_upgrade
    --->    keep_first_size
    ;       allow_doubling.

    % construct_string_hash_cases(StrsData, AllowDouble,
    %   TableSize, HashMap, HashOp, NumCollisions):
    %
    % For a string switch, compute the hash value for each string in the
    % arms, and store the results as a map from hash values to case
    % representations.
    %
:- pred construct_string_hash_cases(assoc_list(string, CaseRep)::in,
    table_size_upgrade::in, int::out, map(int, string_hash_slot(CaseRep))::out,
    unary_op::out, int::out) is det.

%-----------------------------------------------------------------------------%
%
% Stuff for string binary switches.
%

    % Given a list of cases, represent each case using the supplied predicate,
    % map each string to the representation of its corresponding case,
    % and return a sorted assoc_list version of that map.
    %
:- pred string_binary_cases(list(tagged_case)::in,
    pred(tagged_case, CaseRep, StateA, StateA, StateB, StateB, StateC, StateC)
        ::in(pred(in, out, in, out, in, out, in, out) is det),
    StateA::in, StateA::out, StateB::in, StateB::out, StateC::in, StateC::out,
    assoc_list(string, CaseRep)::out) is det.

%-----------------------------------------------------------------------------%
%
% Stuff for tag switches.
%

% Map secondary tag values (-1 stands for none) to information about their
% switch arm. This "information about the switch arm" is polymorphic, because
% in the presence of switch arms that correspond to more than one cons_id,
% cons_ids whose tags may not all use the same primary tag, we will need to
% duplicate this information, with at least one copy per primary tag.
%
% In the LLDS backend, we can (and do) give a label to each goal. The
% predicates in this module will duplicate only the label, and our caller
% has the responsibility of ensuring that each label/goal pair is defined
% only once.
%
% With the MLDS, we don't (yet) do this, because some MLDS backends (e.g. Java)
% don't support labels. Instead, if need be we duplicate the HLDS goal, which
% means we will generate MLDS code for it more than once.

    % Map primary tag values to the set of their switch arms.
    %
    % Given a key-value pair in this map, the key is duplicated
    % in the ptag field of the value.
    %
:- type ptag_case_map(CaseRep) == map(ptag, ptag_case(CaseRep)).

:- type ptag_case_entry(CaseRep)
    --->    ptag_case_entry(
                % If we are generating code of a shape that works with
                % two possibly unrelated (e.g. non-consecutive) ptag values
                % having the same code, use ptag_case_group_entry. This type
                % is for code shapes that cannot exploit such sharing.

                % The ptag value that has this code.
                ptag,

                % A representation of the code for this primary tag.
                ptag_case(CaseRep)
            ).

:- type ptag_case_group_entry(CaseRep)
    --->    ptag_case_group_entry(
                % It is possible for two or more primary tag values
                % to have exactly the same action, if those ptags represent
                % cons_ids that share the same arm of the switch.
                % The primary tag values

                % The first and any later ptag values that have this code.
                ptag,
                list(ptag),

                % A representation of the code for this primary tag.
                ptag_case(CaseRep)
            ).

:- type ptag_case(CaseRep)
    --->    ptag_case(
                sectag_locn,
                stag_goal_map(CaseRep)
            ).

    % Map each secondary tag value to the representation of the associated
    % code. A negative secondary tag "value" means "no secondary tag".
    %
    % It is of course possible that there is more than one secondary tag value
    % that maps to the same code. Exploiting such sharing is up to
    % backend-specific code.
    %
:- type stag_goal_map(CaseRep)  == map(int, CaseRep).
:- type stag_goal_list(CaseRep) == assoc_list(int, CaseRep).

:- type ptag_case_list(CaseRep) == list(ptag_case_entry(CaseRep)).
:- type ptag_case_group_list(CaseRep) == list(ptag_case_group_entry(CaseRep)).

    % Map primary tag values to the number of constructors sharing them.
    %
:- type ptag_count_map == map(ptag, pair(sectag_locn, int)).

    % Map case ids to the set of primary tags used in the cons_ids
    % of that case.
    %
:- type case_id_ptags_map == map(case_id, set(ptag)).

    % Find out how many secondary tags share each primary tag
    % of the given variable.
    %
:- pred get_ptag_counts(mer_type::in, module_info::in,
    uint8::out, ptag_count_map::out) is det.

    % Group together all the cases that depend on the given variable
    % having the same primary tag value.
    %
:- pred group_cases_by_ptag(list(tagged_case)::in,
    pred(tagged_case, CaseRep, StateA, StateA, StateB, StateB, StateC, StateC)
        ::in(pred(in, out, in, out, in, out, in, out) is det),
    StateA::in, StateA::out, StateB::in, StateB::out, StateC::in, StateC::out,
    case_id_ptags_map::out, ptag_case_map(CaseRep)::out) is det.

    % Group together any primary tags with the same cases.
    % Order the groups based on the number of secondary tags associated
    % with them, putting the ones with the most secondary tags first.
    %
    % Note that it is not an error for a primary tag to have no case list;
    % this can happen in semidet switches, or in det switches where the
    % initial inst of the switch variable is a bound(...) inst representing
    % a subtype.
    %
:- pred order_ptags_by_count(ptag_count_map::in,
    ptag_case_map(CaseRep)::in, ptag_case_group_list(CaseRep)::out) is det.

    % order_ptags_by_value(FirstPtag, MaxPtag, !PtagCaseList):
    %
    % Order the primary tags based on their value, lowest value first.
    % We scan through the primary tags values from zero to maximum.
    % Note that it is not an error for a primary tag to have no case list,
    % for the reason documented in the comment above for order_ptags_by_count.
    %
:- pred order_ptags_by_value(ptag::in, ptag::in,
    ptag_case_map(CaseRep)::in, ptag_case_list(CaseRep)::out) is det.

%-----------------------------------------------------------------------------%

    % If the cons_tag specifies an int_tag, return the int;
    % otherwise abort.
    %
:- pred get_int_tag(cons_tag::in, int::out) is det.

    % If the cons_tag specifies a string_tag, return the string;
    % otherwise abort.
    %
:- pred get_string_tag(cons_tag::in, string::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.string_encoding.
:- import_module check_hlds.
:- import_module check_hlds.type_util.
:- import_module hlds.hlds_code_util.
:- import_module libs.optimization_options.
:- import_module libs.options.

:- import_module int.
:- import_module int8.
:- import_module int16.
:- import_module int32.
:- import_module int64.
:- import_module io.
:- import_module maybe.
:- import_module one_or_more.
:- import_module require.
:- import_module string.
:- import_module uint.
:- import_module uint8.
:- import_module uint16.
:- import_module uint32.
:- import_module uint64.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% General stuff, for adding tags to cons_ids in switches and for representing
% switch arms.
%

:- type is_int_switch
    --->    is_int_switch
    ;       is_not_int_switch.

tag_cases(_ModuleInfo, _SwitchType, [], [], _) :-
    unexpected($pred, "no cases").
tag_cases(ModuleInfo, SwitchVarType, [Case | Cases],
        [TaggedCase | TaggedCases], MaybeIntSwitchLimits) :-
    Case = case(MainConsId, OtherConsIds, Goal),
    MainConsTag = cons_id_to_tag(ModuleInfo, MainConsId),
    TaggedMainConsId = tagged_cons_id(MainConsId, MainConsTag),
    ( if MainConsTag = int_tag(IntTag) then
        (
            IntTag = int_tag_int(IntTagVal),
            tag_cases_in_int_switch(ModuleInfo, SwitchVarType,
                TaggedMainConsId, OtherConsIds, Goal, Cases, IntTagVal,
                TaggedCase, TaggedCases, MaybeIntSwitchLimits)
        ;
            IntTag = int_tag_uint(UIntTagVal),
            tag_cases_in_int_switch(ModuleInfo, SwitchVarType,
                TaggedMainConsId, OtherConsIds, Goal, Cases, UIntTagVal,
                TaggedCase, TaggedCases, MaybeIntSwitchLimits)
        ;
            IntTag = int_tag_int8(Int8TagVal),
            tag_cases_in_int_switch(ModuleInfo, SwitchVarType,
                TaggedMainConsId, OtherConsIds, Goal, Cases, Int8TagVal,
                TaggedCase, TaggedCases, MaybeIntSwitchLimits)
        ;
            IntTag = int_tag_uint8(UInt8TagVal),
            tag_cases_in_int_switch(ModuleInfo, SwitchVarType,
                TaggedMainConsId, OtherConsIds, Goal, Cases, UInt8TagVal,
                TaggedCase, TaggedCases, MaybeIntSwitchLimits)
        ;
            IntTag = int_tag_int16(Int16TagVal),
            tag_cases_in_int_switch(ModuleInfo, SwitchVarType,
                TaggedMainConsId, OtherConsIds, Goal, Cases, Int16TagVal,
                TaggedCase, TaggedCases, MaybeIntSwitchLimits)
        ;
            IntTag = int_tag_uint16(UInt16TagVal),
            tag_cases_in_int_switch(ModuleInfo, SwitchVarType,
                TaggedMainConsId, OtherConsIds, Goal, Cases, UInt16TagVal,
                TaggedCase, TaggedCases, MaybeIntSwitchLimits)
        ;
            IntTag = int_tag_int32(Int32TagVal),
            tag_cases_in_int_switch(ModuleInfo, SwitchVarType,
                TaggedMainConsId, OtherConsIds, Goal, Cases, Int32TagVal,
                TaggedCase, TaggedCases, MaybeIntSwitchLimits)
        ;
            IntTag = int_tag_uint32(UInt32TagVal),
            tag_cases_in_int_switch(ModuleInfo, SwitchVarType,
                TaggedMainConsId, OtherConsIds, Goal, Cases, UInt32TagVal,
                TaggedCase, TaggedCases, MaybeIntSwitchLimits)
        ;
            IntTag = int_tag_int64(Int64TagVal),
            tag_cases_in_int_switch(ModuleInfo, SwitchVarType,
                TaggedMainConsId, OtherConsIds, Goal, Cases, Int64TagVal,
                TaggedCase, TaggedCases, MaybeIntSwitchLimits)
        ;
            IntTag = int_tag_uint64(UInt64TagVal),
            tag_cases_in_int_switch(ModuleInfo, SwitchVarType,
                TaggedMainConsId, OtherConsIds, Goal, Cases, UInt64TagVal,
                TaggedCase, TaggedCases, MaybeIntSwitchLimits)
        )
    else
        list.map(tag_cons_id(ModuleInfo), OtherConsIds, TaggedOtherConsIds),
        TaggedCase = tagged_case(TaggedMainConsId, TaggedOtherConsIds,
            case_id(0), Goal),
        tag_cases_plain(ModuleInfo, SwitchVarType, 1, Cases, TaggedCases),
        MaybeIntSwitchLimits = not_int_switch
    ).

%---------------------%

:- typeclass int_tag_value(T) where [
    func int_tag_min(T, T) = T,
    func int_tag_max(T, T) = T,
    pred cons_tag_is_int_tag(cons_tag::in, T::out) is semidet,
    func wrap_int_switch_info(int_switch_info(T)) = maybe_int_switch_info
].

:- instance int_tag_value(int) where [
    func(int_tag_min/2) is int.min,
    func(int_tag_max/2) is int.max,
    ( cons_tag_is_int_tag(ConsTag, TagVal) :-
        ConsTag = int_tag(int_tag_int(TagVal))
    ),
    ( wrap_int_switch_info(IntSwitchInfo) =
        int_switch(IntSwitchInfo)
    )
].

:- instance int_tag_value(int8) where [
    func(int_tag_min/2) is int8.min,
    func(int_tag_max/2) is int8.max,
    ( cons_tag_is_int_tag(ConsTag, TagVal) :-
        ConsTag = int_tag(int_tag_int8(TagVal))
    ),
    ( wrap_int_switch_info(IntSwitchInfo) =
       int8_switch(IntSwitchInfo)
    )
].

:- instance int_tag_value(int16) where [
    func(int_tag_min/2) is int16.min,
    func(int_tag_max/2) is int16.max,
    ( cons_tag_is_int_tag(ConsTag, TagVal) :-
        ConsTag = int_tag(int_tag_int16(TagVal))
    ),
    ( wrap_int_switch_info(IntSwitchInfo) =
       int16_switch(IntSwitchInfo)
    )
].

:- instance int_tag_value(int32) where [
    func(int_tag_min/2) is int32.min,
    func(int_tag_max/2) is int32.max,
    ( cons_tag_is_int_tag(ConsTag, TagVal) :-
        ConsTag = int_tag(int_tag_int32(TagVal))
    ),
    ( wrap_int_switch_info(IntSwitchInfo) =
        int32_switch(IntSwitchInfo)
    )
].

:- instance int_tag_value(int64) where [
    func(int_tag_min/2) is int64.min,
    func(int_tag_max/2) is int64.max,
    ( cons_tag_is_int_tag(ConsTag, TagVal) :-
        ConsTag = int_tag(int_tag_int64(TagVal))
    ),
    ( wrap_int_switch_info(IntSwitchInfo) =
       int64_switch(IntSwitchInfo)
    )
].

:- instance int_tag_value(uint) where [
    func(int_tag_min/2) is uint.min,
    func(int_tag_max/2) is uint.max,
    ( cons_tag_is_int_tag(ConsTag, TagVal) :-
        ConsTag = int_tag(int_tag_uint(TagVal))
    ),
    ( wrap_int_switch_info(IntSwitchInfo) =
       uint_switch(IntSwitchInfo)
    )
].

:- instance int_tag_value(uint8) where [
    func(int_tag_min/2) is uint8.min,
    func(int_tag_max/2) is uint8.max,
    ( cons_tag_is_int_tag(ConsTag, TagVal) :-
        ConsTag = int_tag(int_tag_uint8(TagVal))
    ),
    ( wrap_int_switch_info(IntSwitchInfo) =
       uint8_switch(IntSwitchInfo)
    )
].

:- instance int_tag_value(uint16) where [
    func(int_tag_min/2) is uint16.min,
    func(int_tag_max/2) is uint16.max,
    ( cons_tag_is_int_tag(ConsTag, TagVal) :-
        ConsTag = int_tag(int_tag_uint16(TagVal))
    ),
    ( wrap_int_switch_info(IntSwitchInfo) =
       uint16_switch(IntSwitchInfo)
    )
].

:- instance int_tag_value(uint32) where [
    func(int_tag_min/2) is uint32.min,
    func(int_tag_max/2) is uint32.max,
    ( cons_tag_is_int_tag(ConsTag, TagVal) :-
        ConsTag = int_tag(int_tag_uint32(TagVal))
    ),
    ( wrap_int_switch_info(IntSwitchInfo) =
       uint32_switch(IntSwitchInfo)
    )
].

:- instance int_tag_value(uint64) where [
    func(int_tag_min/2) is uint64.min,
    func(int_tag_max/2) is uint64.max,
    ( cons_tag_is_int_tag(ConsTag, TagVal) :-
        ConsTag = int_tag(int_tag_uint64(TagVal))
    ),
    ( wrap_int_switch_info(IntSwitchInfo) =
       uint64_switch(IntSwitchInfo)
    )
].

:- pred tag_cases_in_int_switch(module_info::in, mer_type::in,
    tagged_cons_id::in, list(cons_id)::in, hlds_goal::in,
    list(case)::in, T::in, tagged_case::out, list(tagged_case)::out,
    maybe_int_switch_info::out) is det <= int_tag_value(T).

tag_cases_in_int_switch(ModuleInfo, SwitchVarType, TaggedMainConsId,
        OtherConsIds, Goal, Cases, IntTagVal, TaggedCase, TaggedCases,
        MaybeIntSwitchLimits) :-
    list.map_foldl4(tag_cons_id_in_int_switch(ModuleInfo),
        OtherConsIds, TaggedOtherConsIds,
        IntTagVal, LowerLimit1, IntTagVal, UpperLimit1,
        1, NumValues1, is_int_switch, IsIntSwitch1),
    TaggedCase = tagged_case(TaggedMainConsId, TaggedOtherConsIds,
        case_id(0), Goal),
    do_tag_cases_in_int_switch(ModuleInfo, SwitchVarType, 1,
        Cases, TaggedCases,
        LowerLimit1, LowerLimit, UpperLimit1, UpperLimit,
        NumValues1, NumValues, IsIntSwitch1, IsIntSwitch),
    (
        IsIntSwitch = is_int_switch,
        IntSwitchInfo = int_switch_info(LowerLimit, UpperLimit, NumValues),
        MaybeIntSwitchLimits = wrap_int_switch_info(IntSwitchInfo)
    ;
        IsIntSwitch = is_not_int_switch,
        MaybeIntSwitchLimits = not_int_switch
    ).

:- pred do_tag_cases_in_int_switch(module_info::in, mer_type::in, int::in,
    list(case)::in, list(tagged_case)::out, T::in, T::out,
    T::in, T::out, int::in, int::out, is_int_switch::in, is_int_switch::out)
    is det <= int_tag_value(T).

do_tag_cases_in_int_switch(_, _, _, [], [], !LowerLimit, !UpperLimit,
        !NumValues, !IsIntSwitch).
do_tag_cases_in_int_switch(ModuleInfo, SwitchVarType, CaseNum, [Case | Cases],
        [TaggedCase | TaggedCases], !LowerLimit, !UpperLimit,
        !NumValues, !IsIntSwitch) :-
    Case = case(MainConsId, OtherConsIds, Goal),
    tag_cons_id_in_int_switch(ModuleInfo, MainConsId, TaggedMainConsId,
        !LowerLimit, !UpperLimit, !NumValues, !IsIntSwitch),
    list.map_foldl4(tag_cons_id_in_int_switch(ModuleInfo),
        OtherConsIds, TaggedOtherConsIds, !LowerLimit, !UpperLimit,
        !NumValues, !IsIntSwitch),
    TaggedCase = tagged_case(TaggedMainConsId, TaggedOtherConsIds,
        case_id(CaseNum), Goal),
    do_tag_cases_in_int_switch(ModuleInfo, SwitchVarType, CaseNum + 1,
        Cases, TaggedCases, !LowerLimit, !UpperLimit,
        !NumValues, !IsIntSwitch).

:- pred tag_cons_id_in_int_switch(module_info::in,
    cons_id::in, tagged_cons_id::out,
    T::in, T::out, T::in, T::out, int::in, int::out,
    is_int_switch::in, is_int_switch::out) is det <= int_tag_value(T).

tag_cons_id_in_int_switch(ModuleInfo, ConsId, TaggedConsId,
        !LowerLimit, !UpperLimit, !NumValues, !IsIntSwitch) :-
    ConsTag = cons_id_to_tag(ModuleInfo, ConsId),
    TaggedConsId = tagged_cons_id(ConsId, ConsTag),
    ( if cons_tag_is_int_tag(ConsTag, IntTag) then
        !:LowerLimit = int_tag_min(IntTag, !.LowerLimit),
        !:UpperLimit = int_tag_max(IntTag, !.UpperLimit),
        !:NumValues = !.NumValues + 1
    else
        !:IsIntSwitch = is_not_int_switch
    ).

%---------------------%

:- pred tag_cons_id(module_info::in, cons_id::in, tagged_cons_id::out) is det.

tag_cons_id(ModuleInfo, ConsId, TaggedConsId) :-
    ConsTag = cons_id_to_tag(ModuleInfo, ConsId),
    TaggedConsId = tagged_cons_id(ConsId, ConsTag).

:- pred tag_cases_plain(module_info::in, mer_type::in, int::in, list(case)::in,
    list(tagged_case)::out) is det.

tag_cases_plain(_, _, _, [], []).
tag_cases_plain(ModuleInfo, SwitchVarType, CaseNum, [Case | Cases],
        [TaggedCase | TaggedCases]) :-
    Case = case(MainConsId, OtherConsIds, Goal),
    tag_cons_id(ModuleInfo, MainConsId, TaggedMainConsId),
    list.map(tag_cons_id(ModuleInfo), OtherConsIds, TaggedOtherConsIds),
    TaggedCase = tagged_case(TaggedMainConsId, TaggedOtherConsIds,
        case_id(CaseNum), Goal),
    tag_cases_plain(ModuleInfo, SwitchVarType, CaseNum + 1, Cases,
        TaggedCases).

%-----------------------------------------------------------------------------%

num_cons_ids_in_tagged_cases(TaggedCases, NumConsIds, NumArms) :-
    num_cons_ids_in_tagged_cases_loop(TaggedCases, 0, NumConsIds, 0, NumArms).

:- pred num_cons_ids_in_tagged_cases_loop(list(tagged_case)::in,
    int::in, int::out, int::in, int::out) is det.

num_cons_ids_in_tagged_cases_loop([], !NumConsIds, !NumArms).
num_cons_ids_in_tagged_cases_loop([TaggedCase | TaggedCases],
        !NumConsIds, !NumArms) :-
    TaggedCase = tagged_case(_MainConsId, OtherCondIds, _, _),
    !:NumConsIds = !.NumConsIds + 1 + list.length(OtherCondIds),
    !:NumArms = !.NumArms + 1,
    num_cons_ids_in_tagged_cases_loop(TaggedCases, !NumConsIds, !NumArms).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% Stuff for categorizing switches.
%

type_ctor_cat_to_switch_cat(CtorCat) = SwitchCat :-
    (
        CtorCat = ctor_cat_builtin(cat_builtin_int(IntType)),
        (
            ( IntType = int_type_int
            ; IntType = int_type_uint
            ; IntType = int_type_int8
            ; IntType = int_type_uint8
            ; IntType = int_type_int16
            ; IntType = int_type_uint16
            ; IntType = int_type_int32
            ; IntType = int_type_uint32
            ),
            SwitchCat = atomic_switch
        ;
            ( IntType = int_type_int64
            ; IntType = int_type_uint64
            ),
            SwitchCat = int64_switch
        )
    ;
        ( CtorCat = ctor_cat_enum(_)
        ; CtorCat = ctor_cat_builtin(cat_builtin_char)
        ),
        SwitchCat = atomic_switch
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_string),
        SwitchCat = string_switch
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_float),
        SwitchCat = float_switch
    ;
        CtorCat = ctor_cat_user(cat_user_general),
        SwitchCat = tag_switch
    ;
        ( CtorCat = ctor_cat_builtin_dummy
        ; CtorCat = ctor_cat_user(cat_user_direct_dummy)
        ; CtorCat = ctor_cat_user(cat_user_notag)
        ; CtorCat = ctor_cat_user(cat_user_abstract_dummy)
        ; CtorCat = ctor_cat_user(cat_user_abstract_notag)
        ; CtorCat = ctor_cat_tuple
        ; CtorCat = ctor_cat_system(_)
        ; CtorCat = ctor_cat_variable
        ; CtorCat = ctor_cat_void
        ; CtorCat = ctor_cat_higher_order
        ),
        % You can't have a switch without at least two arms, or without values
        % that can be deconstructed.
        unexpected($pred, "bad type ctor cat")
    ).

estimate_switch_tag_test_cost(ConsTag) = Cost :-
    (
        ( ConsTag = int_tag(_)
        ; ConsTag = foreign_tag(_, _)
        ; ConsTag = shared_local_tag_no_args(_, _, lsectag_always_rest_of_word)
        ),
        % You need only a single word compare.
        Cost = 1
    ;
        ConsTag = direct_arg_tag(_),
        % You need to compute the primary tag and compare it.
        Cost = 2
    ;
        ConsTag = remote_args_tag(RemoteArgsTagInfo),
        (
            RemoteArgsTagInfo = remote_args_only_functor,
            % The cost we return here does not matter, since one cannot switch
            % on a value of a type that has only one functor.
            Cost = 2
        ;
            RemoteArgsTagInfo = remote_args_unshared(_),
            % You need to compute the primary tag and compare it.
            Cost = 2
        ;
            RemoteArgsTagInfo = remote_args_shared(_, RemoteSectag),
            RemoteSectag = remote_sectag(_, SectagSize),
            (
                SectagSize = rsectag_word,
                % You need to compute the primary tag, compare it, and then
                % fetch and compare the remote secondary tag.
                Cost = 5
            ;
                SectagSize = rsectag_subword(_),
                % You need to compute the primary tag, compare it, and then
                % fetch, mask out and compare the remote secondary tag.
                Cost = 6
            )
        ;
            RemoteArgsTagInfo = remote_args_ctor(_),
            % You need to fetch the data field and compare it.
            Cost = 4
        )
    ;
        ConsTag = float_tag(_),
        % You need to follow a pointer and then compare 64 bits
        % (two words on 32 bit machines, which are still the most common).
        % XXX they're not that common anymore.
        Cost = 3
    ;
        ( ConsTag = shared_local_tag_no_args(_, _, lsectag_must_be_masked)
        ; ConsTag = local_args_tag(_)
        ),
        % You need to compute the primary tag, compare it, then compute
        % and compare the local secondary tag.
        Cost = 4
    ;
        ConsTag = string_tag(String),
        % You need to follow a pointer and then compare all the characters to
        % the end of the string. The multiplication is an attempt to factor in
        % the fact that each character comparison is in a loop, and thus takes
        % more than one instruction.
        % On non-ASCII strings, this cost depends on the compiler back-end.
        Cost = 1 + 2 * string.length(String)
    ;
        ( ConsTag = no_tag
        ; ConsTag = dummy_tag
        ; ConsTag = closure_tag(_, _, _)
        ; ConsTag = type_ctor_info_tag(_, _, _)
        ; ConsTag = base_typeclass_info_tag(_, _, _)
        ; ConsTag = type_info_const_tag(_)
        ; ConsTag = typeclass_info_const_tag(_)
        ; ConsTag = ground_term_const_tag(_, _)
        ; ConsTag = tabling_info_tag(_, _)
        ; ConsTag = deep_profiling_proc_layout_tag(_, _)
        ; ConsTag = table_io_entry_tag(_, _)
        ),
        unexpected($pred, "non-switch tag")
    ).

find_switch_category(ModuleInfo, SwitchVarType, SwitchCategory,
        MayUseSmartIndexing) :-
    SwitchTypeCtorCat = classify_type(ModuleInfo, SwitchVarType),
    SwitchCategory = type_ctor_cat_to_switch_cat(SwitchTypeCtorCat),

    module_info_get_globals(ModuleInfo, Globals),
    globals.get_opt_tuple(Globals, OptTuple),
    ( if
        (
            % We cannot use smart indexing if smart indexing is turned off
            % in general.
            OptTuple ^ ot_use_smart_indexing = do_not_use_smart_indexing
        ;
            % We cannot use smart indexing if smart indexing is turned off
            % for this category of switches.
            SmartIndexingForCategory = is_smart_indexing_allowed_for_category(
                Globals, SwitchCategory),
            SmartIndexingForCategory = no
        )
    then
        MayUseSmartIndexing = may_not_use_smart_indexing
    else
        MayUseSmartIndexing = may_use_smart_indexing
    ).

:- func is_smart_indexing_allowed_for_category(globals, switch_category)
    = bool.

is_smart_indexing_allowed_for_category(Globals, SwitchCategory) = Allowed :-
    globals.get_opt_tuple(Globals, OptTuple),
    (
        SwitchCategory = atomic_switch,
        Atomic = OptTuple ^ ot_use_smart_indexing_atomic,
        ( Atomic = use_smart_indexing_atomic, Allowed = yes
        ; Atomic = do_not_use_smart_indexing_atomic, Allowed = no
        )
    ;
        SwitchCategory = string_switch,
        String = OptTuple ^ ot_use_smart_indexing_string,
        ( String = use_smart_indexing_string, Allowed = yes
        ; String = do_not_use_smart_indexing_string, Allowed = no
        )
    ;
        SwitchCategory = tag_switch,
        Tag = OptTuple ^ ot_use_smart_indexing_tag,
        ( Tag = use_smart_indexing_tag, Allowed = yes
        ; Tag = do_not_use_smart_indexing_tag, Allowed = no
        )
    ;
        SwitchCategory = float_switch,
        Float = OptTuple ^ ot_use_smart_indexing_float,
        ( Float = use_smart_indexing_float, Allowed = yes
        ; Float = do_not_use_smart_indexing_float, Allowed = no
        )
    ;
        SwitchCategory = int64_switch,
        % We do not have a separate option for controlling smart indexing
        % of 64-bit integers.
        Gen = OptTuple ^ ot_use_smart_indexing,
        ( Gen = use_smart_indexing, Allowed = yes
        ; Gen = do_not_use_smart_indexing, Allowed = no
        )
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% Stuff for dense switches.
%

type_range(ModuleInfo, TypeCtorCat, Type, Min, Max, NumValues) :-
    (
        TypeCtorCat = ctor_cat_builtin(cat_builtin_char),
        % Note also that some code in both dense_switch.m and in
        % lookup_switch.m assumes that min_char_value is 0.
        module_info_get_globals(ModuleInfo, Globals),
        globals.get_target(Globals, Target),
        target_char_range(Target, Min, Max)
    ;
        TypeCtorCat = ctor_cat_enum(cat_enum_mercury),
        Min = 0,
        type_to_ctor_det(Type, TypeCtor),
        module_info_get_type_table(ModuleInfo, TypeTable),
        lookup_type_ctor_defn(TypeTable, TypeCtor, TypeDefn),
        hlds_data.get_type_defn_body(TypeDefn, TypeBody),
        (
            TypeBody = hlds_du_type(Constructors, _, _, _, _),
            Constructors = one_or_more(_HeadCtor, TailCtors),
            list.length(TailCtors, NumTailConstructors),
            % NumConstructors = 1 + NumTailConstructors
            % Max = NumConstructors - 1
            Max = NumTailConstructors
        ;
            ( TypeBody = hlds_eqv_type(_)
            ; TypeBody = hlds_foreign_type(_)
            ; TypeBody = hlds_solver_type(_)
            ; TypeBody = hlds_abstract_type(_)
            ),
            unexpected($pred, "enum type is not d.u. type?")
        )
    ),
    NumValues = Max - Min + 1.

switch_density(NumCases, Range) = Density :-
    Density = (NumCases * 100) // Range.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
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
        % large enough to hold all of the values for the type, but then we
        % will need to do the bitvector test.
        classify_type(ModuleInfo, SwitchVarType) = TypeCategory,
        % If there are going to be no gaps in the lookup table, then we
        % won't need a bitvector test to see if this switch has a value
        % for this case.
        ( if NumValues = Range then
            NeedBitVecCheck0 = dont_need_bit_vec_check
        else
            NeedBitVecCheck0 = need_bit_vec_check
        ),
        ( if
            type_range(ModuleInfo, TypeCategory, SwitchVarType, _, _,
                TypeRange),
            DetDensity = switch_density(NumValues, TypeRange),
            DetDensity > ReqDensity
        then
            NeedRangeCheck = dont_need_range_check,
            NeedBitVecCheck = need_bit_vec_check,
            FirstVal = 0,
            LastVal = TypeRange - 1
        else
            NeedRangeCheck = need_range_check,
            NeedBitVecCheck = NeedBitVecCheck0,
            FirstVal = LowerLimit,
            LastVal = UpperLimit
        )
    ;
        SwitchCanFail = cannot_fail,
        % Even if NumValues \= Range, the cannot_fail guarantees that
        % the values that are in range but are not covered by any of the cases
        % won't actually be reached.
        NeedRangeCheck = dont_need_range_check,
        NeedBitVecCheck = dont_need_bit_vec_check,
        FirstVal = LowerLimit,
        LastVal = UpperLimit
    ).

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

get_word_bits(Globals, WordBits, Log2WordBits) :-
    int.bits_per_int(HostWordBits),
    globals.lookup_int_option(Globals, bits_per_word, TargetWordBits),
    int.min(HostWordBits, TargetWordBits, WordBits0),
    % Round down to the nearest power of 2.
    Log2WordBits = log2_rounded_down(WordBits0),
    int.pow(2, Log2WordBits, WordBits).

:- func log2_rounded_down(int) = int.

log2_rounded_down(X) = Log :-
    int.log2(X + 1, Log + 1).  % int.log2 rounds up

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% Stuff for string hash switches.
%

construct_string_hash_cases(StrsDatas, Upgrade, TableSize,
        HashSlotsMap, HashOp, NumCollisions) :-
    % Determine how big to make the hash table. Currently we round the number
    % of strings up to the nearest power of two, and then double it.
    % If this yields a hash table without collisions, fine.
    % Otherwise, if our caller allows us, we see whether we can avoid
    % coliisions if we double the table size again.

    list.length(StrsDatas, NumStrs),
    int.log2(NumStrs, LogNumStrs),
    int.pow(2, LogNumStrs, RoundedUpNumStrs),

    TableSizeA = 2 * RoundedUpNumStrs,
    % With this tablesize, the hash table load factor will be
    % between 0.25 and 0.5.
    HashMaskA = TableSizeA - 1,
    string_hash_cases(StrsDatas, HashMaskA,
        map.init, HashValsMap4A, map.init, HashValsMap5A,
        map.init, HashValsMap6A,
        0, NumCollisions4A, 0, NumCollisions5A, 0, NumCollisions6A),
    trace [compiletime(flag("hashcollisions")), io(!IO)] (
        io.stderr_stream(StdErr, !IO),
        io.format(StdErr, "string hash collisions A: %d %d %d\n",
            [i(NumCollisions4A), i(NumCollisions5A), i(NumCollisions6A)], !IO)
    ),
    ( if
        NumCollisions4A =< NumCollisions5A,
        NumCollisions4A =< NumCollisions6A
    then
        HashValsMapA = HashValsMap4A,
        HashOpA = hash_string4,
        NumCollisionsA = NumCollisions4A
    else if
        NumCollisions5A =< NumCollisions6A
    then
        HashValsMapA = HashValsMap5A,
        HashOpA = hash_string5,
        NumCollisionsA = NumCollisions5A
    else
        HashValsMapA = HashValsMap6A,
        HashOpA = hash_string6,
        NumCollisionsA = NumCollisions6A
    ),

    ( if
        ( NumCollisionsA = 0
        ; Upgrade = keep_first_size
        )
    then
        TableSize = TableSizeA,
        HashValsMap = HashValsMapA,
        HashOp = HashOpA,
        NumCollisions = NumCollisionsA
    else
        TableSizeB = 4 * RoundedUpNumStrs,
        % With this tablesize, the hash table load factor will be
        % between 0.125 and 0.25.
        HashMaskB = TableSizeB - 1,
        string_hash_cases(StrsDatas, HashMaskB,
            map.init, HashValsMap4B, map.init, HashValsMap5B,
            map.init, HashValsMap6B,
            0, NumCollisions4B, 0, NumCollisions5B, 0, NumCollisions6B),
        trace [compiletime(flag("hashcollisions")), io(!IO)] (
            io.stderr_stream(StdErr, !IO),
            io.format(StdErr, "string hash collisions B: %d %d %d\n",
                [i(NumCollisions4B), i(NumCollisions5B), i(NumCollisions6B)],
                !IO)
        ),
        ( if NumCollisions4B = 0 then
            TableSize = TableSizeB,
            HashValsMap = HashValsMap4B,
            HashOp = hash_string4,
            NumCollisions = NumCollisions4B
        else if NumCollisions5B = 0 then
            TableSize = TableSizeB,
            HashValsMap = HashValsMap5B,
            HashOp = hash_string5,
            NumCollisions = NumCollisions5B
        else if NumCollisions6B = 0 then
            TableSize = TableSizeB,
            HashValsMap = HashValsMap6B,
            HashOp = hash_string6,
            NumCollisions = NumCollisions6B
        else
            TableSize = TableSizeA,
            HashValsMap = HashValsMapA,
            HashOp = HashOpA,
            NumCollisions = NumCollisionsA
        ),
        trace [compiletime(flag("hashcollisions")), io(!IO)] (
            io.stderr_stream(StdErr, !IO),
            ( if NumCollisions = 0, NumCollisionsA > 0 then
                io.write_string(StdErr, "string hash IMPROVEMENT\n", !IO)
            else
                io.write_string(StdErr, "string hash NO IMPROVEMENT\n", !IO)
            )
        )
    ),
    map.to_assoc_list(HashValsMap, HashValsList),
    calc_string_hash_slots(TableSize, HashValsList, HashValsMap, HashSlotsMap).

%-----------------------------------------------------------------------------%

:- pred string_hash_cases(assoc_list(string, CaseRep)::in, int::in,
    map(int, assoc_list(string, CaseRep))::in,
    map(int, assoc_list(string, CaseRep))::out,
    map(int, assoc_list(string, CaseRep))::in,
    map(int, assoc_list(string, CaseRep))::out,
    map(int, assoc_list(string, CaseRep))::in,
    map(int, assoc_list(string, CaseRep))::out,
    int::in, int::out, int::in, int::out, int::in, int::out) is det.

string_hash_cases([], _, !HashMap4, !HashMap5, !HashMap6,
        !NumCollisions4, !NumCollisions5, !NumCollisions6).
string_hash_cases([StrData | StrsDatas], HashMask,
        !HashMap4, !HashMap5, !HashMap6,
        !NumCollisions4, !NumCollisions5, !NumCollisions6) :-
    string_hash_case(StrData, HashMask,
        !HashMap4, !HashMap5, !HashMap6,
        !NumCollisions4, !NumCollisions5, !NumCollisions6),
    string_hash_cases(StrsDatas, HashMask,
        !HashMap4, !HashMap5, !HashMap6,
        !NumCollisions4, !NumCollisions5, !NumCollisions6).

:- pred string_hash_case(pair(string, CaseRep)::in, int::in,
    map(int, assoc_list(string, CaseRep))::in,
    map(int, assoc_list(string, CaseRep))::out,
    map(int, assoc_list(string, CaseRep))::in,
    map(int, assoc_list(string, CaseRep))::out,
    map(int, assoc_list(string, CaseRep))::in,
    map(int, assoc_list(string, CaseRep))::out,
    int::in, int::out, int::in, int::out, int::in, int::out) is det.

string_hash_case(StrCaseRep, HashMask,
        !HashMap4, !HashMap5, !HashMap6,
        !NumCollisions4, !NumCollisions5, !NumCollisions6) :-
    StrCaseRep = String - _CaseRep,
    HashVal4 = string.hash4(String) /\ HashMask,
    HashVal5 = string.hash5(String) /\ HashMask,
    HashVal6 = string.hash6(String) /\ HashMask,
    ( if map.search(!.HashMap4, HashVal4, OldEntries4) then
        map.det_update(HashVal4, [StrCaseRep | OldEntries4], !HashMap4),
        !:NumCollisions4 = !.NumCollisions4 + 1
    else
        map.det_insert(HashVal4, [StrCaseRep], !HashMap4)
    ),
    ( if map.search(!.HashMap5, HashVal5, OldEntries5) then
        map.det_update(HashVal5, [StrCaseRep | OldEntries5], !HashMap5),
        !:NumCollisions5 = !.NumCollisions5 + 1
    else
        map.det_insert(HashVal5, [StrCaseRep], !HashMap5)
    ),
    ( if map.search(!.HashMap6, HashVal6, OldEntries6) then
        map.det_update(HashVal6, [StrCaseRep | OldEntries6], !HashMap6),
        !:NumCollisions6 = !.NumCollisions6 + 1
    else
        map.det_insert(HashVal6, [StrCaseRep], !HashMap6)
    ).

%-----------------------------------------------------------------------------%

    % calc_string_hash_slots(AssocList, HashMap, Map):
    %
    % For each (HashVal - Case) pair in AssocList, allocate a hash slot in Map
    % for the case. If the hash slot corresponding to HashVal is not already
    % used, then use that one. Otherwise, find the next spare slot (making sure
    % that we don't use slots which can be used for a direct match with the
    % hash value for one of the other cases), and use it instead.
    % Keep track of the hash chains as we do this.
    %
:- pred calc_string_hash_slots(int::in,
    assoc_list(int, assoc_list(string, CaseRep))::in,
    map(int, assoc_list(string, CaseRep))::in,
    map(int, string_hash_slot(CaseRep))::out) is det.

calc_string_hash_slots(TableSize, HashValList, HashMap, SlotMap) :-
    trace [compile_time(flag("hash_slots")), io(!IO)] (
        io.stderr_stream(StdErr, !IO),
        io.write_string(StdErr, "CALCULATING HASH SLOTS START\n", !IO)
    ),
    calc_string_hash_slots_loop_over_hashes(HashValList, TableSize, HashMap,
        map.init, SlotMap, 0, _),
    trace [compile_time(flag("hash_slots")), io(!IO)] (
        io.stderr_stream(StdErr, !IO),
        io.write_string(StdErr, "CALCULATING HASH SLOTS END\n", !IO)
    ).

:- pred calc_string_hash_slots_loop_over_hashes(
    assoc_list(int, assoc_list(string, CaseRep))::in, int::in,
    map(int, assoc_list(string, CaseRep))::in,
    map(int, string_hash_slot(CaseRep))::in,
    map(int, string_hash_slot(CaseRep))::out,
    int::in, int::out) is det.

calc_string_hash_slots_loop_over_hashes([], _, _, !SlotMap, !LastUsed).
calc_string_hash_slots_loop_over_hashes([HashVal - StringCaseReps | Rest],
        TableSize, HashMap, !SlotMap, !LastUsed) :-
    calc_string_hash_slots_loop_over_hash_strings(StringCaseReps, TableSize,
        HashVal, HashMap, !SlotMap, !LastUsed),
    calc_string_hash_slots_loop_over_hashes(Rest, TableSize,
        HashMap, !SlotMap, !LastUsed).

:- pred calc_string_hash_slots_loop_over_hash_strings(
    assoc_list(string, CaseRep)::in, int::in, int::in,
    map(int, assoc_list(string, CaseRep))::in,
    map(int, string_hash_slot(CaseRep))::in,
    map(int, string_hash_slot(CaseRep))::out,
    int::in, int::out) is det.

calc_string_hash_slots_loop_over_hash_strings([],
        _TableSize, _HashVal, _HashMap, !SlotMap, !LastUsed).
calc_string_hash_slots_loop_over_hash_strings([StringCaseRep | StringCaseReps],
        TableSize, HashVal, HashMap, !SlotMap, !LastUsed) :-
    calc_string_hash_slots_loop_over_hash_strings(StringCaseReps,
        TableSize, HashVal, HashMap, !SlotMap, !LastUsed),
    StringCaseRep = String - CaseRep,
    NewSlot = string_hash_slot(String, -1, CaseRep),
    ( if map.contains(!.SlotMap, HashVal) then
        follow_hash_chain(!.SlotMap, HashVal, ChainEnd),
        next_free_hash_slot(!.SlotMap, HashMap, TableSize, !LastUsed),
        map.lookup(!.SlotMap, ChainEnd, ChainEndSlot0),
        ChainEndSlot0 = string_hash_slot(PrevString, _, PrevCaseRep),
        ChainEndSlot = string_hash_slot(PrevString, !.LastUsed, PrevCaseRep),
        map.det_update(ChainEnd, ChainEndSlot, !SlotMap),
        map.det_insert(!.LastUsed, NewSlot, !SlotMap),
        trace [compile_time(flag("hash_slots")), io(!IO)] (
            io.stderr_stream(StdErr, !IO),
            io.format(StdErr, "%s: home %d, remapped slot %d\n",
                [s(String), i(HashVal), i(!.LastUsed)], !IO)
        )
    else
        map.det_insert(HashVal, NewSlot, !SlotMap),
        trace [compile_time(flag("hash_slots")), io(!IO)] (
            io.stderr_stream(StdErr, !IO),
            io.format(StdErr, "%s: native slot %d\n",
                [s(String), i(HashVal)], !IO)
        )
    ).

:- pred follow_hash_chain(map(int, string_hash_slot(CaseRep))::in,
    int::in, int::out) is det.

follow_hash_chain(Map, Slot, LastSlot) :-
    map.lookup(Map, Slot, string_hash_slot(_, NextSlot, _)),
    ( if
        NextSlot >= 0,
        map.contains(Map, NextSlot)
    then
        follow_hash_chain(Map, NextSlot, LastSlot)
    else
        LastSlot = Slot
    ).

    % next_free_hash_slot(M, H_M, LastUsed, FreeSlot):
    %
    % Find the next available slot FreeSlot in the hash table which is not
    % already used (contained in Map) and which is not going to be used as a
    % primary slot (contained in HomeMap), starting at the slot after LastUsed.
    %
:- pred next_free_hash_slot(map(int, string_hash_slot(CaseRep))::in,
    map(int, assoc_list(string, CaseRep))::in, int::in, int::in, int::out)
    is det.

next_free_hash_slot(Map, HomeMap, TableSize, LastUsed, FreeSlot) :-
    NextSlot = LastUsed + 1,
    expect(NextSlot < TableSize, $pred, "overflow"),
    ( if
        ( map.contains(Map, NextSlot)
        ; map.contains(HomeMap, NextSlot)
        )
    then
        next_free_hash_slot(Map, HomeMap, TableSize, NextSlot, FreeSlot)
    else
        FreeSlot = NextSlot
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% Stuff for string binary switches.
%

string_binary_cases(TaggedCases, RepresentCase,
        !StateA, !StateB, !StateC, SortedTable) :-
    string_binary_entries(TaggedCases, RepresentCase,
        !StateA, !StateB, !StateC, [], UnsortedTable),
    list.sort(UnsortedTable, SortedTable).

:- pred string_binary_entries(list(tagged_case)::in,
    pred(tagged_case, CaseRep, StateA, StateA, StateB, StateB, StateC, StateC)
        ::in(pred(in, out, in, out, in, out, in, out) is det),
    StateA::in, StateA::out, StateB::in, StateB::out, StateC::in, StateC::out,
    assoc_list(string, CaseRep)::in, assoc_list(string, CaseRep)::out) is det.

string_binary_entries([], _, !StateA, !StateB, !StateC, !UnsortedTable).
string_binary_entries([TaggedCase | TaggedCases], RepresentCase,
        !StateA, !StateB, !StateC, !UnsortedTable) :-
    string_binary_entries(TaggedCases, RepresentCase,
        !StateA, !StateB, !StateC, !UnsortedTable),
    RepresentCase(TaggedCase, CaseRep, !StateA, !StateB, !StateC),
    TaggedCase = tagged_case(MainTaggedConsId, OtherTaggedConsIds, _, _),
    add_string_binary_entry(CaseRep, MainTaggedConsId, !UnsortedTable),
    list.foldl(add_string_binary_entry(CaseRep), OtherTaggedConsIds,
        !UnsortedTable).

:- pred add_string_binary_entry(CaseRep::in, tagged_cons_id::in,
    assoc_list(string, CaseRep)::in, assoc_list(string, CaseRep)::out) is det.

add_string_binary_entry(CaseRep, TaggedConsId, !UnsortedTable) :-
    TaggedConsId = tagged_cons_id(_ConsId, Tag),
    ( if Tag = string_tag(StringPrime) then
        String = StringPrime
    else
        unexpected($pred, "non-string case?")
    ),
    !:UnsortedTable = [String - CaseRep | !.UnsortedTable].

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% Stuff for tag switches.
%

get_ptag_counts(Type, ModuleInfo, MaxPrimary, PtagCountMap) :-
    type_to_ctor_det(Type, TypeCtor),
    module_info_get_type_table(ModuleInfo, TypeTable),
    lookup_type_ctor_defn(TypeTable, TypeCtor, TypeDefn),
    hlds_data.get_type_defn_body(TypeDefn, TypeBody),
    (
        TypeBody = hlds_du_type(_, _, _, MaybeRepn, _),
        (
            MaybeRepn = no,
            unexpected($pred, "MaybeRepn = no")
        ;
            MaybeRepn = yes(Repn),
            CtorRepns = Repn ^ dur_ctor_repns
        )
    ;
        ( TypeBody = hlds_eqv_type(_)
        ; TypeBody = hlds_foreign_type(_)
        ; TypeBody = hlds_solver_type(_)
        ; TypeBody = hlds_abstract_type(_)
        ),
        unexpected($pred, "non-du type")
    ),
    MaXPrimary0 = 0u8,
    map.init(PtagCountMap0),
    get_ptag_counts_loop(CtorRepns,
        MaXPrimary0, MaxPrimary, PtagCountMap0, PtagCountMap).

:- pred get_ptag_counts_loop(list(constructor_repn)::in, uint8::in, uint8::out,
    ptag_count_map::in, ptag_count_map::out) is det.

get_ptag_counts_loop([], !MaxPrimary, !PtagCountMap).
get_ptag_counts_loop([CtorRepn | CtorRepns], !MaxPrimary, !PtagCountMap) :-
    ConsTag = CtorRepn ^ cr_tag,
    (
        ConsTag = direct_arg_tag(Ptag),
        SectagLocn = sectag_none_direct_arg,
        Ptag = ptag(Primary),
        !:MaxPrimary = uint8.max(Primary, !.MaxPrimary),
        ( if map.search(!.PtagCountMap, Ptag, _) then
            unexpected($pred, "unshared tag is shared")
        else
            map.det_insert(Ptag, SectagLocn - (-1), !PtagCountMap)
        )
    ;
        ConsTag = remote_args_tag(RemoteArgsTagInfo),
        (
            (
                RemoteArgsTagInfo = remote_args_only_functor,
                Ptag = ptag(0u8),
                SectagLocn = sectag_none
            ;
                RemoteArgsTagInfo = remote_args_unshared(Ptag),
                SectagLocn = sectag_none
            ),
            Ptag = ptag(Primary),
            !:MaxPrimary = uint8.max(Primary, !.MaxPrimary),
            ( if map.search(!.PtagCountMap, Ptag, _) then
                unexpected($pred, "unshared tag is shared")
            else
                map.det_insert(Ptag, SectagLocn - (-1), !PtagCountMap)
            )
        ;
            (
                RemoteArgsTagInfo = remote_args_shared(Ptag, RemoteSectag),
                Ptag = ptag(Primary),
                RemoteSectag = remote_sectag(SecondaryUint, SectagSize),
                (
                    SectagSize = rsectag_word,
                    SectagLocn = sectag_remote_word
                ;
                    SectagSize = rsectag_subword(SectagBits),
                    SectagBits = sectag_bits(NumSectagBits, Mask),
                    SectagLocn = sectag_remote_bits(NumSectagBits, Mask)
                ),
                Secondary = uint.cast_to_int(SecondaryUint)
            ;
                RemoteArgsTagInfo = remote_args_ctor(Data),
                Primary = 0u8,
                Ptag = ptag(Primary),
                SectagLocn = sectag_remote_word,
                Secondary = uint.cast_to_int(Data)
            ),
            !:MaxPrimary = uint8.max(Primary, !.MaxPrimary),
            ( if map.search(!.PtagCountMap, Ptag, Target) then
                Target = OldSectagLocn - MaxSoFar,
                expect(unify(OldSectagLocn, SectagLocn), $pred,
                    "remote tag is shared with non-remote"),
                int.max(Secondary, MaxSoFar, Max),
                map.det_update(Ptag, SectagLocn - Max, !PtagCountMap)
            else
                map.det_insert(Ptag, SectagLocn - Secondary, !PtagCountMap)
            )
        )
    ;
        (
            ConsTag = shared_local_tag_no_args(Ptag, LocalSectag, MustMask),
            LocalSectag = local_sectag(SecondaryUint, _, SectagBits),
            (
                MustMask = lsectag_always_rest_of_word,
                SectagLocn = sectag_local_rest_of_word
            ;
                MustMask = lsectag_must_be_masked,
                SectagBits = sectag_bits(NumSectagBits, Mask),
                SectagLocn = sectag_local_bits(NumSectagBits, Mask)
            )
        ;
            ConsTag = local_args_tag(LocalArgsTagInfo),
            (
                LocalArgsTagInfo = local_args_only_functor,
                % You can't switch on a variable of a type that has
                % only one function symbol.
                unexpected($pred, "local_args_only_functor")
            ;
                LocalArgsTagInfo = local_args_not_only_functor(Ptag,
                    LocalSectag),
                LocalSectag = local_sectag(SecondaryUint, _, SectagBits),
                SectagBits = sectag_bits(NumSectagBits, Mask),
                SectagLocn = sectag_local_bits(NumSectagBits, Mask)
            )
        ),
        Ptag = ptag(Primary),
        Secondary = uint.cast_to_int(SecondaryUint),
        !:MaxPrimary = uint8.max(Primary, !.MaxPrimary),
        ( if map.search(!.PtagCountMap, Ptag, Target) then
            Target = OldSectagLocn - MaxSoFar,
            expect(unify(OldSectagLocn, SectagLocn), $pred,
                "local tag is shared with something else"),
            int.max(Secondary, MaxSoFar, Max),
            map.det_update(Ptag, SectagLocn - Max, !PtagCountMap)
        else
            map.det_insert(Ptag, SectagLocn - Secondary, !PtagCountMap)
        )
    ;
        ( ConsTag = no_tag
        ; ConsTag = dummy_tag
        ; ConsTag = string_tag(_)
        ; ConsTag = float_tag(_)
        ; ConsTag = int_tag(_)
        ; ConsTag = foreign_tag(_, _)
        ; ConsTag = closure_tag(_, _, _)
        ; ConsTag = type_ctor_info_tag(_, _, _)
        ; ConsTag = base_typeclass_info_tag(_, _, _)
        ; ConsTag = type_info_const_tag(_)
        ; ConsTag = typeclass_info_const_tag(_)
        ; ConsTag = ground_term_const_tag(_, _)
        ; ConsTag = tabling_info_tag(_, _)
        ; ConsTag = deep_profiling_proc_layout_tag(_, _)
        ; ConsTag = table_io_entry_tag(_, _)
        ),
        unexpected($pred, "non-du tag")
    ),
    get_ptag_counts_loop(CtorRepns, !MaxPrimary, !PtagCountMap).

%-----------------------------------------------------------------------------%

group_cases_by_ptag(TaggedCases, RepresentCase, !StateA, !StateB, !StateC,
        CaseNumPtagsMap, PtagCaseMap) :-
    group_cases_by_ptag_loop(TaggedCases, RepresentCase,
        !StateA, !StateB, !StateC,
        map.init, CaseNumPtagsMap, map.init, PtagCaseMap).

:- pred group_cases_by_ptag_loop(list(tagged_case)::in,
    pred(tagged_case, CaseRep, StateA, StateA, StateB, StateB, StateC, StateC)
        ::in(pred(in, out, in, out, in, out, in, out) is det),
    StateA::in, StateA::out, StateB::in, StateB::out, StateC::in, StateC::out,
    case_id_ptags_map::in, case_id_ptags_map::out,
    ptag_case_map(CaseRep)::in, ptag_case_map(CaseRep)::out) is det.

group_cases_by_ptag_loop([], _,
        !StateA, !StateB, !StateC, !CaseNumPtagsMap, !PtagCaseMap).
group_cases_by_ptag_loop([TaggedCase | TaggedCases], RepresentCase,
        !StateA, !StateB, !StateC, !CaseNumPtagsMap, !PtagCaseMap) :-
    TaggedCase = tagged_case(MainTaggedConsId, OtherConsIds, CaseId, _Goal),
    RepresentCase(TaggedCase, CaseRep, !StateA, !StateB, !StateC),
    group_case_by_ptag(CaseId, CaseRep, MainTaggedConsId,
        !CaseNumPtagsMap, !PtagCaseMap),
    list.foldl2(group_case_by_ptag(CaseId, CaseRep), OtherConsIds,
        !CaseNumPtagsMap, !PtagCaseMap),
    group_cases_by_ptag_loop(TaggedCases, RepresentCase,
        !StateA, !StateB, !StateC, !CaseNumPtagsMap, !PtagCaseMap).

:- pred group_case_by_ptag(case_id::in, CaseRep::in, tagged_cons_id::in,
    map(case_id, set(ptag))::in, map(case_id, set(ptag))::out,
    ptag_case_map(CaseRep)::in, ptag_case_map(CaseRep)::out) is det.

group_case_by_ptag(CaseId, CaseRep, TaggedConsId,
        !CaseIdPtagsMap, !PtagCaseMap) :-
    TaggedConsId = tagged_cons_id(_ConsId, ConsTag),
    (
        ConsTag = direct_arg_tag(Ptag),
        SectagLocn = sectag_none_direct_arg,
        ( if map.search(!.PtagCaseMap, Ptag, _Group) then
            unexpected($pred, "unshared tag is shared")
        else
            StagGoalMap = map.singleton(-1, CaseRep),
            map.det_insert(Ptag, ptag_case(SectagLocn, StagGoalMap),
                !PtagCaseMap)
        )
    ;
        ConsTag = remote_args_tag(RemoteArgsTagInfo),
        (
            (
                RemoteArgsTagInfo = remote_args_only_functor,
                Ptag = ptag(0u8),
                SectagLocn = sectag_none
            ;
                RemoteArgsTagInfo = remote_args_unshared(Ptag),
                SectagLocn = sectag_none
            ),
            ( if map.search(!.PtagCaseMap, Ptag, _Group) then
                unexpected($pred, "unshared tag is shared")
            else
                StagGoalMap = map.singleton(-1, CaseRep),
                map.det_insert(Ptag, ptag_case(SectagLocn, StagGoalMap),
                    !PtagCaseMap)
            )
        ;
            (
                RemoteArgsTagInfo = remote_args_shared(Ptag, RemoteSectag),
                RemoteSectag = remote_sectag(SecondaryUint, SectagSize),
                (
                    SectagSize = rsectag_word,
                    SectagLocn = sectag_remote_word
                ;
                    SectagSize = rsectag_subword(SectagBits),
                    SectagBits = sectag_bits(NumSectagBits, Mask),
                    SectagLocn = sectag_remote_bits(NumSectagBits, Mask)
                ),
                Secondary = uint.cast_to_int(SecondaryUint)
            ;
                RemoteArgsTagInfo = remote_args_ctor(Data),
                Primary = 0u8,
                Ptag = ptag(Primary),
                SectagLocn = sectag_remote_word,
                Secondary = uint.cast_to_int(Data)
            ),
            ( if map.search(!.PtagCaseMap, Ptag, Group) then
                Group = ptag_case(OldSectagLocn, StagGoalMap0),
                expect(unify(OldSectagLocn, SectagLocn), $pred,
                    "remote tag is shared with non-remote"),
                map.det_insert(Secondary, CaseRep, StagGoalMap0, StagGoalMap),
                map.det_update(Ptag, ptag_case(SectagLocn, StagGoalMap),
                    !PtagCaseMap)
            else
                StagGoalMap = map.singleton(Secondary, CaseRep),
                map.det_insert(Ptag, ptag_case(SectagLocn, StagGoalMap),
                    !PtagCaseMap)
            )
        )
    ;
        (
            ConsTag = shared_local_tag_no_args(Ptag, LocalSectag, MustMask),
            LocalSectag = local_sectag(SecondaryUint, _, SectagBits),
            (
                MustMask = lsectag_always_rest_of_word,
                SectagLocn = sectag_local_rest_of_word
            ;
                MustMask = lsectag_must_be_masked,
                SectagBits = sectag_bits(NumSectagBits, Mask),
                SectagLocn = sectag_local_bits(NumSectagBits, Mask)
            )
        ;
            ConsTag = local_args_tag(LocalArgsTagInfo),
            (
                LocalArgsTagInfo = local_args_only_functor,
                % You can't switch on a variable of a type that has
                % only one function symbol.
                unexpected($pred, "local_args_only_functor")
            ;
                LocalArgsTagInfo = local_args_not_only_functor(Ptag,
                    LocalSectag),
                LocalSectag = local_sectag(SecondaryUint, _, SectagBits),
                SectagBits = sectag_bits(NumSectagBits, Mask),
                SectagLocn = sectag_local_bits(NumSectagBits, Mask)
            )
        ),
        Secondary = uint.cast_to_int(SecondaryUint),
        ( if map.search(!.PtagCaseMap, Ptag, Group) then
            Group = ptag_case(OldSectagLocn, StagGoalMap0),
            expect(unify(OldSectagLocn, SectagLocn), $pred,
                "local tag is shared with something different"),
            map.det_insert(Secondary, CaseRep, StagGoalMap0, StagGoalMap),
            map.det_update(Ptag, ptag_case(SectagLocn, StagGoalMap),
                !PtagCaseMap)
        else
            StagGoalMap = map.singleton(Secondary, CaseRep),
            map.det_insert(Ptag, ptag_case(SectagLocn, StagGoalMap),
                !PtagCaseMap)
        )
    ;
        ( ConsTag = no_tag
        ; ConsTag = dummy_tag
        ; ConsTag = string_tag(_)
        ; ConsTag = float_tag(_)
        ; ConsTag = int_tag(_)
        ; ConsTag = foreign_tag(_, _)
        ; ConsTag = closure_tag(_, _, _)
        ; ConsTag = type_ctor_info_tag(_, _, _)
        ; ConsTag = base_typeclass_info_tag(_, _, _)
        ; ConsTag = type_info_const_tag(_)
        ; ConsTag = typeclass_info_const_tag(_)
        ; ConsTag = ground_term_const_tag(_, _)
        ; ConsTag = tabling_info_tag(_, _)
        ; ConsTag = deep_profiling_proc_layout_tag(_, _)
        ; ConsTag = table_io_entry_tag(_, _)
        ),
        unexpected($pred, "non-du tag")
    ),
    ( if map.search(!.CaseIdPtagsMap, CaseId, Ptags0) then
        set.insert(Ptag, Ptags0, Ptags),
        map.det_update(CaseId, Ptags, !CaseIdPtagsMap)
    else
        Ptags = set.make_singleton_set(Ptag),
        map.det_insert(CaseId, Ptags, !CaseIdPtagsMap)
    ).

%-----------------------------------------------------------------------------%

order_ptags_by_count(PtagCountMap, PtagCaseMap, PtagGroupCaseList) :-
    map.to_assoc_list(PtagCaseMap, PtagCaseList),
    build_ptag_case_rev_map(PtagCaseList, PtagCountMap,
        map.init, PtagCaseRevMap),
    map.values(PtagCaseRevMap, PtagCaseRevList),
    list.sort(PtagCaseRevList, PtagCaseRevSortedList),
    % The sort puts the groups with the smallest counts first; we want the
    % largest counts first.
    list.reverse(PtagCaseRevSortedList, PtagCaseSortedList),
    list.map(interpret_rev_map_entry, PtagCaseSortedList, PtagGroupCaseList).

:- pred interpret_rev_map_entry(ptag_case_rev_map_entry(CaseRep)::in,
    ptag_case_group_entry(CaseRep)::out) is det.

interpret_rev_map_entry(RevEntry, GroupEntry) :-
    RevEntry = ptag_case_rev_map_entry(_Count, MainPtag, OtherPtags, Case),
    GroupEntry = ptag_case_group_entry(MainPtag, OtherPtags, Case).

:- type ptag_case_rev_map_entry(CaseRep)
    --->    ptag_case_rev_map_entry(
                % The total number of function symbols sharing this case.
                % This must be the first field for the sort to work as
                % intended.
                int,

                % The primary tags sharing this case.
                ptag,
                list(ptag),

                % The case itself.
                ptag_case(CaseRep)
            ).

:- type ptag_case_rev_map(CaseRep)  ==
    map(ptag_case(CaseRep), ptag_case_rev_map_entry(CaseRep)).

:- pred build_ptag_case_rev_map(assoc_list(ptag, ptag_case(CaseRep))::in,
    ptag_count_map::in,
    ptag_case_rev_map(CaseRep)::in, ptag_case_rev_map(CaseRep)::out) is det.

build_ptag_case_rev_map([], _PtagCountMap, !RevMap).
build_ptag_case_rev_map([Entry | Entries], PtagCountMap, !RevMap) :-
    Entry = Ptag - Case,
    map.lookup(PtagCountMap, Ptag, CountSectagLocn - Count),
    (
        ( CountSectagLocn = sectag_none
        ; CountSectagLocn = sectag_none_direct_arg
        ),
        ( if map.search(!.RevMap, Case, OldEntry) then
            OldEntry = ptag_case_rev_map_entry(OldCount,
                OldFirstPtag, OldLaterPtags0, OldCase),
            expect(unify(Case, OldCase), $pred, "Case != OldCase"),
            NewEntry = ptag_case_rev_map_entry(OldCount + Count,
                OldFirstPtag, OldLaterPtags0 ++ [Ptag], OldCase),
            map.det_update(Case, NewEntry, !RevMap)
        else
            NewEntry = ptag_case_rev_map_entry(Count, Ptag, [], Case),
            map.det_insert(Case, NewEntry, !RevMap)
        )
    ;
        ( CountSectagLocn = sectag_local_rest_of_word
        ; CountSectagLocn = sectag_local_bits(_, _)
        ; CountSectagLocn = sectag_remote_word
        ; CountSectagLocn = sectag_remote_bits(_, _)
        ),
        % There will only ever be at most one primary tag value with
        % a shared local tag, and there will only ever be at most one primary
        % tag value with a shared remote tag, so we can never have
        %
        % - two ptags with CountSectagLocn = sectag_local_*
        % - two ptags with CountSectagLocn = sectag_remote
        %
        % We can never have two entries where one is sectag_local_bits(_)
        % and the other is sectag_local_rest_of_word; all function symbols
        % whose representation includes a local sectag must agree on whether
        % that sectag may ever be followed by arguments (sectag_local_bits)
        % or not (sectag_local_rest_of_word).
        %
        % We can have two ptags, one with either CountSectagLocn =
        % sectag_local_bits(_) or CountSectagLocn = sectag_local_rest_of_word,
        % and the other with CountSectagLocn = sectag_remote, but even if their
        % sectag_value to code maps were identical, their overall code couldn't
        % be identical, since they would have to get the secondary tags from
        % different places.
        NewEntry = ptag_case_rev_map_entry(Count, Ptag, [], Case),
        map.det_insert(Case, NewEntry, !RevMap)
    ),
    build_ptag_case_rev_map(Entries, PtagCountMap, !RevMap).

%-----------------------------------------------------------------------------%

order_ptags_by_value(Ptag, MaxPtag, PtagCaseMap0, PtagCaseList) :-
    Ptag = ptag(PtagUint8),
    MaxPtag = ptag(MaxPtagUint8),
    ( if PtagUint8 =< MaxPtagUint8 then
        NextPtagUint8 = PtagUint8 + 1u8,
        NextPtag = ptag(NextPtagUint8),
        ( if map.search(PtagCaseMap0, Ptag, PtagCase) then
            map.delete(Ptag, PtagCaseMap0, PtagCaseMap1),
            order_ptags_by_value(NextPtag, MaxPtag,
                PtagCaseMap1, PtagCaseList1),
            PtagCaseEntry = ptag_case_entry(Ptag, PtagCase),
            PtagCaseList = [PtagCaseEntry | PtagCaseList1]
        else
            order_ptags_by_value(NextPtag, MaxPtag, PtagCaseMap0, PtagCaseList)
        )
    else
        ( if map.is_empty(PtagCaseMap0) then
            PtagCaseList = []
        else
            unexpected($pred, "PtagCaseMap0 is not empty")
        )
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

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

%-----------------------------------------------------------------------------%
:- end_module backend_libs.switch_util.
%-----------------------------------------------------------------------------%
