%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2000-2009 The University of Melbourne.
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
:- import_module hlds.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_type.

:- import_module assoc_list.
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
    --->    int_switch(
                lower_limit     :: int,
                upper_limit     :: int,
                num_values      :: int
            )
    ;       not_int_switch.

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
    --->    atomic_switch   % a switch on int/char/enum
    ;       string_switch
    ;       tag_switch
    ;       other_switch.

    % Convert a type constructor category to a switch category.
    %
:- func type_ctor_cat_to_switch_cat(type_ctor_category) = switch_category.

    % Return an estimate of the runtime cost of a constructor test for the
    % given tag. We try to put the cheap tests first.
    %
    % Abort on cons_tags that should never be switched on.
    %
:- func estimate_switch_tag_test_cost(cons_tag) = int.

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
% Stuff for string hash switches.
%

    % For a string switch, compute the hash value for each case in the list
    % of cases, and store the cases in a map from hash values to cases.
    %
:- pred string_hash_cases(list(tagged_case)::in, int::in,
    pred(tagged_case, CaseRep, StateA, StateA, StateB, StateB, StateC, StateC)
        ::in(pred(in, out, in, out, in, out, in, out) is det),
    StateA::in, StateA::out, StateB::in, StateB::out, StateC::in, StateC::out,
    map(int, assoc_list(string, CaseRep))::out) is det.

:- type string_hash_slot(CaseRep)
    --->    string_hash_slot(int, string, CaseRep).

    % calc_string_hash_slots(AssocList, HashMap, Map):
    %
    % For each (HashVal - Case) pair in AssocList, allocate a hash slot in Map
    % for the case. If the hash slot corresponding to HashVal is not already
    % used, then use that one. Otherwise, find the next spare slot (making sure
    % that we don't use slots which can be used for a direct match with the
    % hash value for one of the other cases), and use it instead.
    % Keep track of the hash chains as we do this.
    %
    % XXX
:- pred calc_string_hash_slots(
    assoc_list(int, assoc_list(string, CaseRep))::in,
    map(int, assoc_list(string, CaseRep))::in,
    map(int, string_hash_slot(CaseRep))::out) is det.

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
    % in the tag_bits field of the value.
    %
:- type ptag_case_map(CaseRep) ==
    map(tag_bits, ptag_case(CaseRep)).

:- type ptag_case_entry(CaseRep)
    --->    ptag_case_entry(
                % If we are generating code of a shape that works with
                % two possibly unrelated (e.g. non-consecutive) ptag values
                % having the same code, use ptag_case_group_entry. This type
                % is for code shapes that cannot exploit such sharing.

                % The ptag value that has this code.
                tag_bits,

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
                tag_bits,
                list(tag_bits),

                % A representation of the code for this primary tag.
                ptag_case(CaseRep)
            ).

:- type ptag_case(CaseRep)
    --->    ptag_case(
                sectag_locn,
                stag_goal_map(CaseRep)
            ).

    % Map each secondary tag value to the representation of the associated
    % code.
    %
    % It is of course possible that there is more than one secondary tag value
    % that maps to the same code. Exploiting such sharing is up to
    % backend-specific code.
    %
:- type stag_goal_map(CaseRep)   ==  map(int, CaseRep).
:- type stag_goal_list(CaseRep)  ==  assoc_list(int, CaseRep).

:- type ptag_case_list(CaseRep) ==  list(ptag_case_entry(CaseRep)).
:- type ptag_case_group_list(CaseRep) ==  list(ptag_case_group_entry(CaseRep)).

    % Map primary tag values to the number of constructors sharing them.
    %
:- type ptag_count_map  ==  map(tag_bits, pair(sectag_locn, int)).

    % Map case numbers to the set of primary tags used in the cons_ids
    % of that case.
    %
:- type case_num_ptags_map == map(int, set(int)).

    % Group together all the cases that depend on the given variable
    % having the same primary tag value.
    %
:- pred group_cases_by_ptag(list(tagged_case)::in,
    pred(tagged_case, CaseRep, StateA, StateA, StateB, StateB, StateC, StateC)
        ::in(pred(in, out, in, out, in, out, in, out) is det),
    StateA::in, StateA::out, StateB::in, StateB::out, StateC::in, StateC::out,
    case_num_ptags_map::out, ptag_case_map(CaseRep)::out) is det.

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
:- pred order_ptags_by_value(int::in, int::in,
    ptag_case_map(CaseRep)::in, ptag_case_list(CaseRep)::out) is det.

    % Find out how many secondary tags share each primary tag
    % of the given variable.
    %
:- pred get_ptag_counts(mer_type::in, module_info::in,
    int::out, ptag_count_map::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_data.
:- import_module hlds.hlds_code_util.
:- import_module hlds.hlds_out.
:- import_module libs.
:- import_module libs.compiler_util.
:- import_module parse_tree.prog_type.

:- import_module char.
:- import_module cord.
:- import_module int.
:- import_module string.
:- import_module svmap.

%-----------------------------------------------------------------------------%
%
% General stuff, for adding tags to cons_ids in switches and for representing
% switch arms.
%

:- type is_int_switch
    --->    is_int_switch
    ;       is_not_int_switch.

tag_cases(_ModuleInfo, _SwitchType, [], [], _) :-
    unexpected(this_file, "tag_cases: no cases").
tag_cases(ModuleInfo, SwitchVarType, [Case | Cases],
        [TaggedCase | TaggedCases], MaybeIntSwitchLimits) :-
    Case = case(MainConsId, OtherConsIds, Goal),
    MainConsTag = cons_id_to_tag(ModuleInfo, MainConsId),
    TaggedMainConsId = tagged_cons_id(MainConsId, MainConsTag),
    ( MainConsTag = int_tag(IntTag) ->
        list.map_foldl4(tag_cons_id_in_int_switch(ModuleInfo),
            OtherConsIds, TaggedOtherConsIds,
            IntTag, LowerLimit1, IntTag, UpperLimit1,
            1, NumValues1, is_int_switch, IsIntSwitch1),
        TaggedCase = tagged_case(TaggedMainConsId, TaggedOtherConsIds,
            0, Goal),
        tag_cases_in_int_switch(ModuleInfo, SwitchVarType, 1,
            Cases, TaggedCases,
            LowerLimit1, LowerLimit, UpperLimit1, UpperLimit,
            NumValues1, NumValues, IsIntSwitch1, IsIntSwitch),
        (
            IsIntSwitch = is_int_switch,
            MaybeIntSwitchLimits = int_switch(LowerLimit, UpperLimit,
                NumValues)
        ;
            IsIntSwitch = is_not_int_switch,
            MaybeIntSwitchLimits = not_int_switch
        )
    ;
        list.map(tag_cons_id(ModuleInfo), OtherConsIds, TaggedOtherConsIds),
        TaggedCase = tagged_case(TaggedMainConsId, TaggedOtherConsIds,
            0, Goal),
        tag_cases_plain(ModuleInfo, SwitchVarType, 1, Cases, TaggedCases),
        MaybeIntSwitchLimits = not_int_switch
    ).

:- pred tag_cases_plain(module_info::in, mer_type::in, int::in, list(case)::in,
    list(tagged_case)::out) is det.

tag_cases_plain(_, _, _, [], []).
tag_cases_plain(ModuleInfo, SwitchVarType, CaseNum, [Case | Cases],
        [TaggedCase | TaggedCases]) :-
    Case = case(MainConsId, OtherConsIds, Goal),
    tag_cons_id(ModuleInfo, MainConsId, TaggedMainConsId),
    list.map(tag_cons_id(ModuleInfo), OtherConsIds, TaggedOtherConsIds),
    TaggedCase = tagged_case(TaggedMainConsId, TaggedOtherConsIds,
        CaseNum, Goal),
    tag_cases_plain(ModuleInfo, SwitchVarType, CaseNum + 1, Cases,
        TaggedCases).

:- pred tag_cases_in_int_switch(module_info::in, mer_type::in, int::in,
    list(case)::in, list(tagged_case)::out, int::in, int::out, int::in,
    int::out, int::in, int::out, is_int_switch::in, is_int_switch::out) is det.

tag_cases_in_int_switch(_, _, _, [], [], !LowerLimit, !UpperLimit, !NumValues,
        !IsIntSwitch).
tag_cases_in_int_switch(ModuleInfo, SwitchVarType, CaseNum, [Case | Cases],
        [TaggedCase | TaggedCases], !LowerLimit, !UpperLimit, !NumValues,
        !IsIntSwitch) :-
    Case = case(MainConsId, OtherConsIds, Goal),
    tag_cons_id_in_int_switch(ModuleInfo, MainConsId, TaggedMainConsId,
        !LowerLimit, !UpperLimit, !NumValues, !IsIntSwitch),
    list.map_foldl4(tag_cons_id_in_int_switch(ModuleInfo),
        OtherConsIds, TaggedOtherConsIds, !LowerLimit, !UpperLimit,
        !NumValues, !IsIntSwitch),
    TaggedCase = tagged_case(TaggedMainConsId, TaggedOtherConsIds,
        CaseNum, Goal),
    tag_cases_in_int_switch(ModuleInfo, SwitchVarType, CaseNum + 1,
        Cases, TaggedCases, !LowerLimit, !UpperLimit, !NumValues,
        !IsIntSwitch).

:- pred tag_cons_id(module_info::in, cons_id::in, tagged_cons_id::out) is det.

tag_cons_id(ModuleInfo, ConsId, TaggedConsId) :-
    ConsTag = cons_id_to_tag(ModuleInfo, ConsId),
    TaggedConsId = tagged_cons_id(ConsId, ConsTag).

:- pred tag_cons_id_in_int_switch(module_info::in,
    cons_id::in, tagged_cons_id::out,
    int::in, int::out, int::in, int::out, int::in, int::out,
    is_int_switch::in, is_int_switch::out) is det.

tag_cons_id_in_int_switch(ModuleInfo, ConsId, TaggedConsId,
        !LowerLimit, !UpperLimit, !NumValues, !IsIntSwitch) :-
    ConsTag = cons_id_to_tag(ModuleInfo, ConsId),
    TaggedConsId = tagged_cons_id(ConsId, ConsTag),
    ( ConsTag = int_tag(IntTag) ->
        int.min(IntTag, !LowerLimit),
        int.max(IntTag, !UpperLimit),
        !:NumValues = !.NumValues + 1
    ;
        !:IsIntSwitch = is_not_int_switch
    ).

%-----------------------------------------------------------------------------%

num_cons_ids_in_tagged_cases(TaggedCases, NumConsIds, NumArms) :-
    num_cons_ids_in_tagged_cases_2(TaggedCases, 0, NumConsIds, 0, NumArms).

:- pred num_cons_ids_in_tagged_cases_2(list(tagged_case)::in,
    int::in, int::out, int::in, int::out) is det.

num_cons_ids_in_tagged_cases_2([], !NumConsIds, !NumArms).
num_cons_ids_in_tagged_cases_2([TaggedCase | TaggedCases],
        !NumConsIds, !NumArms) :-
    TaggedCase = tagged_case(_MainConsId, OtherCondIds, _, _),
    !:NumConsIds = !.NumConsIds + 1 + list.length(OtherCondIds),
    !:NumArms = !.NumArms + 1,
    num_cons_ids_in_tagged_cases_2(TaggedCases, !NumConsIds, !NumArms).

%-----------------------------------------------------------------------------%
%
% Stuff for categorizing switches.
%

type_ctor_cat_to_switch_cat(CtorCat) = SwitchCat :-
    (
        ( CtorCat = ctor_cat_enum(_)
        ; CtorCat = ctor_cat_builtin(cat_builtin_int)
        ; CtorCat = ctor_cat_builtin(cat_builtin_char)
        ),
        SwitchCat = atomic_switch
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_string),
        SwitchCat = string_switch
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_float),
        SwitchCat = other_switch
    ;
        CtorCat = ctor_cat_user(cat_user_general),
        SwitchCat = tag_switch
    ;
        ( CtorCat = ctor_cat_builtin_dummy
        ; CtorCat = ctor_cat_user(cat_user_direct_dummy)
        ; CtorCat = ctor_cat_user(cat_user_notag)
        ; CtorCat = ctor_cat_tuple
        ; CtorCat = ctor_cat_system(_)
        ; CtorCat = ctor_cat_variable
        ; CtorCat = ctor_cat_void
        ; CtorCat = ctor_cat_higher_order
        ),
        % You can't have a switch without at least two arms, or without values
        % that can be deconstructed.
        unexpected(this_file, "type_ctor_cat_to_switch_cat: bad type ctor cat")
    ).

estimate_switch_tag_test_cost(Tag) = Cost :-
    (
        ( Tag = int_tag(_)
        ; Tag = foreign_tag(_, _)
        ; Tag = reserved_address_tag(_)
        ; Tag = shared_local_tag(_, _)
        ),
        % You need only a single word compare.
        Cost = 1
    ;
        Tag = single_functor_tag,
        % There is no cost incurred here except the cost of testing for all the
        % reserved addresses this tag is shared with; the Cost = 2 is an
        % estimate (XXX probably not very accurate) of the fixed cost
        % of the scan over them.
        Cost = 2
    ;
        Tag = unshared_tag(_),
        % You need to compute the primary tag and compare it.
        Cost = 2
    ;
        Tag = float_tag(_),
        % You need to follow a pointer and then compare 64 bits
        % (two words on 32 bit machines, which are still the most common).
        Cost = 3
    ;
        Tag = shared_remote_tag(_, _),
        % You need to compute the primary tag, compare it, follow a pointer
        % and then compare the remote secondary tag.
        Cost = 4
    ;
        Tag = string_tag(String),
        % You need to follow a pointer and then compare all the characters to
        % the end of the string. The multiplication is an attempt to factor in
        % the fact that each character comparison is in a loop, and thus takes
        % more than one instruction.
        Cost = 1 + 2 * string.length(String)
    ;
        Tag = shared_with_reserved_addresses_tag(RAs, SubTag),
        % You need to rule out all reserved addresses before testing SubTag.
        Cost = 2 * list.length(RAs) + estimate_switch_tag_test_cost(SubTag)
    ;
        ( Tag = no_tag
        ; Tag = closure_tag(_, _, _)
        ; Tag = type_ctor_info_tag(_, _, _)
        ; Tag = base_typeclass_info_tag(_, _, _)
        ; Tag = tabling_info_tag(_, _)
        ; Tag = deep_profiling_proc_layout_tag(_, _)
        ; Tag = table_io_decl_tag(_, _)
        ),
        unexpected(this_file, "estimate_switch_tag_test_cost: non-switch tag")
    ).

%-----------------------------------------------------------------------------%
%
% Stuff for dense switches.
%

type_range(ModuleInfo, TypeCtorCat, Type, Min, Max, NumValues) :-
    (
        TypeCtorCat = ctor_cat_builtin(cat_builtin_char),
        % XXX The following code uses the host's character size, not the
        % target's, so it won't work if cross-compiling to a machine with
        % a different character size. Note also that some code in both
        % dense_switch.m and in lookup_switch.m assumes that
        % char.min_char_value is 0.
        char.min_char_value(Min),
        char.max_char_value(Max)
    ;
        TypeCtorCat = ctor_cat_enum(cat_enum_mercury),
        Min = 0,
        type_to_ctor_det(Type, TypeCtor),
        module_info_get_type_table(ModuleInfo, TypeTable),
        map.lookup(TypeTable, TypeCtor, TypeDefn),
        hlds_data.get_type_defn_body(TypeDefn, TypeBody),
        (
            TypeBody = hlds_du_type(_, ConsTable, _, _, _, _, _, _),
            map.count(ConsTable, TypeRange),
            Max = TypeRange - 1
        ;
            ( TypeBody = hlds_eqv_type(_)
            ; TypeBody = hlds_foreign_type(_)
            ; TypeBody = hlds_solver_type(_, _)
            ; TypeBody = hlds_abstract_type(_)
            ),
            unexpected(this_file, "type_range: enum type is not d.u. type?")
        )
    ),
    NumValues = Max - Min + 1.

switch_density(NumCases, Range) = Density :-
    Density = (NumCases * 100) // Range.

%-----------------------------------------------------------------------------%
%
% Stuff for string hash switches.
%

string_hash_cases([], _, _, !StateA, !StateB, !StateC, !:HashMap) :-
    map.init(!:HashMap).
string_hash_cases([TaggedCase | TaggedCases], HashMask, RepresentCase,
        !StateA, !StateB, !StateC, !:HashMap) :-
    string_hash_cases(TaggedCases, HashMask, RepresentCase,
        !StateA, !StateB, !StateC, !:HashMap),
    RepresentCase(TaggedCase, CaseRep, !StateA, !StateB, !StateC),
    TaggedCase = tagged_case(MainTaggedConsId, OtherTaggedConsIds, _, _),
    string_hash_cons_id(CaseRep, HashMask, MainTaggedConsId, !HashMap),
    list.foldl(string_hash_cons_id(CaseRep, HashMask), OtherTaggedConsIds,
        !HashMap).

:- pred string_hash_cons_id(CaseRep::in, int::in, tagged_cons_id::in,
    map(int, assoc_list(string, CaseRep))::in,
    map(int, assoc_list(string, CaseRep))::out) is det.

string_hash_cons_id(CaseRep, HashMask, TaggedConsId, !HashMap) :-
    TaggedConsId = tagged_cons_id(_ConsId, Tag),
    ( Tag = string_tag(StringPrime) ->
        String = StringPrime
    ;
        unexpected(this_file, "string_hash_cases: non-string case?")
    ),
    string.hash(String, StringHashVal),
    HashVal = StringHashVal /\ HashMask,
    ( map.search(!.HashMap, HashVal, OldStringCaseReps) ->
        svmap.det_update(HashVal, [String - CaseRep | OldStringCaseReps],
            !HashMap)
    ;
        svmap.det_insert(HashVal, [String - CaseRep], !HashMap)
    ).

calc_string_hash_slots(HashValList, HashMap, SlotMap) :-
    calc_string_hash_slots_1(HashValList, HashMap, map.init, SlotMap, 0, _).

:- pred calc_string_hash_slots_1(
    assoc_list(int, assoc_list(string, CaseRep))::in,
    map(int, assoc_list(string, CaseRep))::in,
    map(int, string_hash_slot(CaseRep))::in,
    map(int, string_hash_slot(CaseRep))::out,
    int::in, int::out) is det.

calc_string_hash_slots_1([], _, !SlotMap, !LastUsed).
calc_string_hash_slots_1([HashVal - StringCaseReps | Rest], HashMap,
        !SlotMap, !LastUsed) :-
    calc_string_hash_slots_2(StringCaseReps, HashVal, HashMap,
        !SlotMap, !LastUsed),
    calc_string_hash_slots_1(Rest, HashMap, !SlotMap, !LastUsed).

:- pred calc_string_hash_slots_2(assoc_list(string, CaseRep)::in, int::in,
    map(int, assoc_list(string, CaseRep))::in,
    map(int, string_hash_slot(CaseRep))::in,
    map(int, string_hash_slot(CaseRep))::out,
    int::in, int::out) is det.

calc_string_hash_slots_2([], _HashVal, _HashMap, !SlotMap, !LastUsed).
calc_string_hash_slots_2([StringCaseRep | StringCaseReps], HashVal, HashMap,
        !SlotMap, !LastUsed) :-
    calc_string_hash_slots_2(StringCaseReps, HashVal, HashMap,
        !SlotMap, !LastUsed),
    StringCaseRep = String - CaseRep,
    NewSlot = string_hash_slot(-1, String, CaseRep),
    ( map.contains(!.SlotMap, HashVal) ->
        follow_hash_chain(!.SlotMap, HashVal, ChainEnd),
        next_free_hash_slot(!.SlotMap, HashMap, !LastUsed),
        map.lookup(!.SlotMap, ChainEnd, ChainEndSlot0),
        ChainEndSlot0 = string_hash_slot(_, PrevString, PrevCaseRep),
        ChainEndSlot = string_hash_slot(!.LastUsed, PrevString, PrevCaseRep),
        svmap.det_update(ChainEnd, ChainEndSlot, !SlotMap),
        svmap.det_insert(!.LastUsed, NewSlot, !SlotMap)
    ;
        svmap.det_insert(HashVal, NewSlot, !SlotMap)
    ).

:- pred follow_hash_chain(map(int, string_hash_slot(CaseRep))::in,
    int::in, int::out) is det.

follow_hash_chain(Map, Slot, LastSlot) :-
    map.lookup(Map, Slot, string_hash_slot(NextSlot, _, _)),
    (
        NextSlot >= 0,
        map.contains(Map, NextSlot)
    ->
        follow_hash_chain(Map, NextSlot, LastSlot)
    ;
        LastSlot = Slot
    ).

    % next_free_hash_slot(M, H_M, LastUsed, FreeSlot):
    %
    % Find the next available slot FreeSlot in the hash table which is not
    % already used (contained in M) and which is not going to be used a
    % primary slot (contained in H_M), starting at the slot after LastUsed.
    %
:- pred next_free_hash_slot(map(int, string_hash_slot(CaseRep))::in,
    map(int, assoc_list(string, CaseRep))::in, int::in, int::out) is det.

next_free_hash_slot(Map, H_Map, LastUsed, FreeSlot) :-
    NextSlot = LastUsed + 1,
    (
        \+ map.contains(Map, NextSlot),
        \+ map.contains(H_Map, NextSlot)
    ->
        FreeSlot = NextSlot
    ;
        next_free_hash_slot(Map, H_Map, NextSlot, FreeSlot)
    ).

%-----------------------------------------------------------------------------%
%
% Stuff for tag switches.
%

get_ptag_counts(Type, ModuleInfo, MaxPrimary, PtagCountMap) :-
    type_to_ctor_det(Type, TypeCtor),
    module_info_get_type_table(ModuleInfo, TypeTable),
    map.lookup(TypeTable, TypeCtor, TypeDefn),
    hlds_data.get_type_defn_body(TypeDefn, TypeBody),
    (
        TypeBody = hlds_du_type(_, ConsTable, _, _, _, _, _, _),
        map.to_assoc_list(ConsTable, ConsList),
        assoc_list.values(ConsList, TagList)
    ;
        ( TypeBody = hlds_eqv_type(_)
        ; TypeBody = hlds_foreign_type(_)
        ; TypeBody = hlds_solver_type(_, _)
        ; TypeBody = hlds_abstract_type(_)
        ),
        unexpected(this_file, "non-du type in get_ptag_counts")
    ),
    map.init(PtagCountMap0),
    get_ptag_counts_2(TagList, -1, MaxPrimary, PtagCountMap0, PtagCountMap).

:- pred get_ptag_counts_2(list(cons_tag)::in, int::in, int::out,
    ptag_count_map::in, ptag_count_map::out) is det.

get_ptag_counts_2([], !MaxPrimary, !PtagCountMap).
get_ptag_counts_2([Tag | Tags], !MaxPrimary, !PtagCountMap) :-
    (
        ( Tag = single_functor_tag, Primary = 0
        ; Tag = unshared_tag(Primary)
        ),
        int.max(Primary, !MaxPrimary),
        ( map.search(!.PtagCountMap, Primary, _) ->
            unexpected(this_file, "unshared tag is shared")
        ;
            svmap.det_insert(Primary, sectag_none - (-1), !PtagCountMap)
        )
    ;
        Tag = shared_remote_tag(Primary, Secondary),
        int.max(Primary, !MaxPrimary),
        ( map.search(!.PtagCountMap, Primary, Target) ->
            Target = TagType - MaxSoFar,
            (
                TagType = sectag_remote
            ;
                ( TagType = sectag_local
                ; TagType = sectag_none
                ),
                unexpected(this_file, "remote tag is shared with non-remote")
            ),
            int.max(Secondary, MaxSoFar, Max),
            svmap.det_update(Primary, sectag_remote - Max, !PtagCountMap)
        ;
            svmap.det_insert(Primary, sectag_remote - Secondary, !PtagCountMap)
        )
    ;
        Tag = shared_local_tag(Primary, Secondary),
        int.max(Primary, !MaxPrimary),
        ( map.search(!.PtagCountMap, Primary, Target) ->
            Target = TagType - MaxSoFar,
            (
                TagType = sectag_local
            ;
                ( TagType = sectag_remote
                ; TagType = sectag_none
                ),
                unexpected(this_file, "local tag is shared with non-local")
            ),
            int.max(Secondary, MaxSoFar, Max),
            svmap.det_update(Primary, sectag_local - Max, !PtagCountMap)
        ;
            svmap.det_insert(Primary, sectag_local - Secondary, !PtagCountMap)
        )
    ;
        ( Tag = no_tag
        ; Tag = string_tag(_)
        ; Tag = float_tag(_)
        ; Tag = int_tag(_)
        ; Tag = foreign_tag(_, _)
        ; Tag = closure_tag(_, _, _)
        ; Tag = type_ctor_info_tag(_, _, _)
        ; Tag = base_typeclass_info_tag(_, _, _)
        ; Tag = tabling_info_tag(_, _)
        ; Tag = deep_profiling_proc_layout_tag(_, _)
        ; Tag = table_io_decl_tag(_, _)
        ; Tag = reserved_address_tag(_)
        ; Tag = shared_with_reserved_addresses_tag(_, _)
        ),
        unexpected(this_file, "non-du tag in get_ptag_counts_2")
    ),
    get_ptag_counts_2(Tags, !MaxPrimary, !PtagCountMap).

%-----------------------------------------------------------------------------%

group_cases_by_ptag(TaggedCases, RepresentCase, !StateA, !StateB, !StateC,
        CaseNumPtagsMap, PtagCaseMap) :-
    group_cases_by_ptag_2(TaggedCases, RepresentCase,
        !StateA, !StateB, !StateC,
        map.init, CaseNumPtagsMap, map.init, PtagCaseMap).

:- pred group_cases_by_ptag_2(list(tagged_case)::in,
    pred(tagged_case, CaseRep, StateA, StateA, StateB, StateB, StateC, StateC)
        ::in(pred(in, out, in, out, in, out, in, out) is det),
    StateA::in, StateA::out, StateB::in, StateB::out, StateC::in, StateC::out,
    case_num_ptags_map::in, case_num_ptags_map::out,
    ptag_case_map(CaseRep)::in, ptag_case_map(CaseRep)::out) is det.

group_cases_by_ptag_2([], _,
        !StateA, !StateB, !StateC, !CaseNumPtagsMap, !PtagCaseMap).
group_cases_by_ptag_2([TaggedCase | TaggedCases], RepresentCase,
        !StateA, !StateB, !StateC, !CaseNumPtagsMap, !PtagCaseMap) :-
    TaggedCase = tagged_case(MainTaggedConsId, OtherConsIds, CaseNum, _Goal),
    RepresentCase(TaggedCase, CaseRep, !StateA, !StateB, !StateC),
    group_case_by_ptag(CaseNum, CaseRep, MainTaggedConsId,
        !CaseNumPtagsMap, !PtagCaseMap),
    list.foldl2(group_case_by_ptag(CaseNum, CaseRep), OtherConsIds,
        !CaseNumPtagsMap, !PtagCaseMap),
    group_cases_by_ptag_2(TaggedCases, RepresentCase,
        !StateA, !StateB, !StateC, !CaseNumPtagsMap, !PtagCaseMap).

:- pred group_case_by_ptag(int::in, CaseRep::in, tagged_cons_id::in,
    map(int, set(int))::in, map(int, set(int))::out,
    ptag_case_map(CaseRep)::in, ptag_case_map(CaseRep)::out) is det.

group_case_by_ptag(CaseNum, CaseRep, TaggedConsId,
        !CaseNumPtagsMap, !PtagCaseMap) :-
    TaggedConsId = tagged_cons_id(_ConsId, Tag),
    (
        ( Tag = single_functor_tag, Primary = 0
        ; Tag = unshared_tag(Primary)
        ),
        ( map.search(!.PtagCaseMap, Primary, _Group) ->
            unexpected(this_file, "unshared tag is shared")
        ;
            map.init(StagGoalMap0),
            map.det_insert(StagGoalMap0, -1, CaseRep, StagGoalMap),
            svmap.det_insert(Primary, ptag_case(sectag_none, StagGoalMap),
                !PtagCaseMap)
        )
    ;
        Tag = shared_remote_tag(Primary, Secondary),
        ( map.search(!.PtagCaseMap, Primary, Group) ->
            Group = ptag_case(StagLoc, StagGoalMap0),
            expect(unify(StagLoc, sectag_remote), this_file,
                "remote tag is shared with non-remote"),
            map.det_insert(StagGoalMap0, Secondary, CaseRep, StagGoalMap),
            svmap.det_update(Primary, ptag_case(sectag_remote, StagGoalMap),
                !PtagCaseMap)
        ;
            map.init(StagGoalMap0),
            map.det_insert(StagGoalMap0, Secondary, CaseRep, StagGoalMap),
            svmap.det_insert(Primary, ptag_case(sectag_remote, StagGoalMap),
                !PtagCaseMap)
        )
    ;
        Tag = shared_local_tag(Primary, Secondary),
        ( map.search(!.PtagCaseMap, Primary, Group) ->
            Group = ptag_case(StagLoc, StagGoalMap0),
            expect(unify(StagLoc, sectag_local), this_file,
                "local tag is shared with non-local"),
            map.det_insert(StagGoalMap0, Secondary, CaseRep, StagGoalMap),
            svmap.det_update(Primary, ptag_case(sectag_local, StagGoalMap),
                !PtagCaseMap)
        ;
            map.init(StagGoalMap0),
            map.det_insert(StagGoalMap0, Secondary, CaseRep, StagGoalMap),
            svmap.det_insert(Primary, ptag_case(sectag_local, StagGoalMap),
                !PtagCaseMap)
        )
    ;
        ( Tag = no_tag
        ; Tag = string_tag(_)
        ; Tag = float_tag(_)
        ; Tag = int_tag(_)
        ; Tag = foreign_tag(_, _)
        ; Tag = closure_tag(_, _, _)
        ; Tag = type_ctor_info_tag(_, _, _)
        ; Tag = base_typeclass_info_tag(_, _, _)
        ; Tag = tabling_info_tag(_, _)
        ; Tag = deep_profiling_proc_layout_tag(_, _)
        ; Tag = table_io_decl_tag(_, _)
        ; Tag = reserved_address_tag(_)
        ; Tag = shared_with_reserved_addresses_tag(_, _)
        ),
        unexpected(this_file, "non-du tag in group_case_by_ptag")
    ),
    ( map.search(!.CaseNumPtagsMap, CaseNum, Ptags0) ->
        set.insert(Ptags0, Primary, Ptags),
        svmap.det_update(CaseNum, Ptags, !CaseNumPtagsMap)
    ;
        Ptags = set.make_singleton_set(Primary),
        svmap.det_insert(CaseNum, Ptags, !CaseNumPtagsMap)
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

                % The primary tag bit values sharing this case.
                tag_bits,
                list(tag_bits),

                % The case itself.
                ptag_case(CaseRep)
            ).

:- type ptag_case_rev_map(CaseRep)  ==
    map(ptag_case(CaseRep), ptag_case_rev_map_entry(CaseRep)).

:- pred build_ptag_case_rev_map(assoc_list(tag_bits, ptag_case(CaseRep))::in,
    ptag_count_map::in,
    ptag_case_rev_map(CaseRep)::in, ptag_case_rev_map(CaseRep)::out) is det.

build_ptag_case_rev_map([], _PtagCountMap, !RevMap).
build_ptag_case_rev_map([Entry | Entries], PtagCountMap, !RevMap) :-
    Entry = Ptag - Case,
    map.lookup(PtagCountMap, Ptag, CountSecTagLocn - Count),
    (
        CountSecTagLocn = sectag_none,
        ( map.search(!.RevMap, Case, OldEntry) ->
            OldEntry = ptag_case_rev_map_entry(OldCount,
                OldFirstPtag, OldLaterPtags0, OldCase),
            expect(unify(Case, OldCase), this_file,
                "build_ptag_case_rev_map: Case != OldCase"),
            NewEntry = ptag_case_rev_map_entry(OldCount + Count,
                OldFirstPtag, OldLaterPtags0 ++ [Ptag], OldCase),
            svmap.det_update(Case, NewEntry, !RevMap)
        ;
            NewEntry = ptag_case_rev_map_entry(Count, Ptag, [], Case),
            svmap.det_insert(Case, NewEntry, !RevMap)
        )
    ;
        ( CountSecTagLocn = sectag_local
        ; CountSecTagLocn = sectag_remote
        ),
        % There will only ever be at most one primary tag value with
        % a shared local tag, and there will only ever be at most one primary
        % tag value with a shared remote tag, so we can never have
        % 
        % - two ptags with CountSecTagLocn = sectag_local
        % - two ptags with CountSecTagLocn = sectag_remote
        %
        % We can have two ptags, one with CountSecTagLocn = sectag_local and
        % the other with CountSecTagLocn = sectag_remote, but even if their
        % sectag_value to code maps were identical, their overall code couldn't
        % be identical, since they would have to get the secondary tags from
        % different places.
        NewEntry = ptag_case_rev_map_entry(Count, Ptag, [], Case),
        svmap.det_insert(Case, NewEntry, !RevMap)
    ),
    build_ptag_case_rev_map(Entries, PtagCountMap, !RevMap).

%-----------------------------------------------------------------------------%

order_ptags_by_value(Ptag, MaxPtag, PtagCaseMap0, PtagCaseList) :-
    ( MaxPtag >= Ptag ->
        NextPtag = Ptag + 1,
        ( map.search(PtagCaseMap0, Ptag, PtagCase) ->
            map.delete(PtagCaseMap0, Ptag, PtagCaseMap1),
            order_ptags_by_value(NextPtag, MaxPtag,
                PtagCaseMap1, PtagCaseList1),
            PtagCaseEntry = ptag_case_entry(Ptag, PtagCase),
            PtagCaseList = [PtagCaseEntry | PtagCaseList1]
        ;
            order_ptags_by_value(NextPtag, MaxPtag, PtagCaseMap0, PtagCaseList)
        )
    ;
        ( map.is_empty(PtagCaseMap0) ->
            PtagCaseList = []
        ;
            unexpected(this_file,
                "PtagCaseMap0 is not empty in order_ptags_by_value")
        )
    ).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "switch_util.m".

%-----------------------------------------------------------------------------%
