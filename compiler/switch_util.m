%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2000-2012 The University of Melbourne.
% Copyright (C) 2013-2023 The Mercury team.
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

:- import_module backend_libs.builtin_ops.
:- import_module backend_libs.rtti.         % for sectag_locn
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
:- import_module one_or_more.
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

    % Return an estimate of the runtime cost of a constructor test for the
    % given tag. We try to put the cheap tests first.
    %
    % Abort on cons_tags that should never be switched on.
    %
:- func estimate_switch_tag_test_cost(cons_tag) = int.

:- type switch_category
    --->    ite_chain_switch
            % A chain of if-then-elses; a dumb switch.

    ;       int_max_32_switch
            % A switch on int, uint, char, enum, or intN/uintN where N < 64.

    ;       int_64_switch
            % A switch on a int64 or uint64.
            % These require special treatment on the Java backend, because
            % Java does not support switches on 64-bit integers.

    ;       string_switch
            % A switch on a string.

    ;       float_switch
            % A switch on a float.

    ;       tag_switch.
            % A switch on a value of a non-enum, non-notag
            % discriminated union type, with a primary tag and possible
            % local and/or remote secondary tags.

    % If smart switches on values of the given type are allowed by the
    % option values in the module_info, then return switch category wrapped up
    % inside may_use_smart_indexing. Otherwise, return
    % may_not_use_smart_indexing.
    %
:- pred find_switch_category(module_info::in, mer_type::in,
    switch_category::out) is det.

%-----------------------------------------------------------------------------%
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
    pred(tagged_case, CaseRep, StateA, StateA, StateB, StateB,
        StateC, StateC, StateD, StateD)
        ::in(pred(in, out, in, out, in, out, in, out, in, out) is det),
    StateA::in, StateA::out, StateB::in, StateB::out,
    StateC::in, StateC::out, StateD::in, StateD::out,
    assoc_list(string, CaseRep)::out) is det.

%-----------------------------------------------------------------------------%
%
% Stuff for tag switches.
%

% XXX This comment seems to have been misplaced. -zs 2022 feb 10.
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
    % This data structure does not recognize situations in which different
    % ptag values need the same treatment. It is now used only as an
    % intermediate data structure on the way to a data structure,
    % a list of ptag_case_groups, which *does* recognize such sharing.
    %
    % ZZZ it should now be possible to stop exporting the types of this
    % intermediate structure.
:- type ptag_case_map(CaseRep) == map(ptag, ptag_case(CaseRep)).

:- type ptag_case_entry(CaseRep)
    --->    ptag_case_entry(
                % The ptag value that has this code.
                ptag,

                % A representation of the code for this primary tag.
                ptag_case(CaseRep)
            ).

:- type ptag_case(CaseRep)
    --->    ptag_case_unshared(
                % The ptag is not shared between function symbols.
                CaseRep
            )
    ;       ptag_case_shared(
                % The ptag is shared between function symbols.
                % The next two fields give the location of the sectag,
                % and its maximum value.
                shared_sectag_locn,
                uint,

                % The map from the value of the sectag to its case.
                stag_goal_map(CaseRep)
            ).

    % Note that it is not an error for a primary tag to have no case.
    % This can happen when
    %
    % - the in semidet switches, or in det switches where the
    % initial inst of the switch variable is a bound(...) inst representing
    % a subtype.
    %
:- type ptag_case_group(CaseRep)
    --->    one_or_more_whole_ptags(whole_ptags_info(CaseRep))
    ;       one_shared_ptag(shared_ptag_info(CaseRep)).

    % It is possible for two or more primary tag values
    % to have exactly the same action, if those ptags represent
    % cons_ids that share the same arm of the switch.
:- type whole_ptags_info(CaseRep)
    --->    whole_ptags_info(
                % The first and any later ptag values that have this code.
                % For all of them, the following invariant holds:
                %
                % - either the ptag is unshared (i.e. its sectag_locn
                %   is either sectag_none or sectag_none_direct_arg,
                %
                % - or the ptag is shared between two or more function symbols,
                %   meaning its sectag_locn is one of the alternatives
                %   in shared_sectag_locn, but the selected switch arm
                %   is CaseRep for *all* the possible sectag values.
                wpi_head_ptag           :: ptag,
                wpi_tail_ptags          :: list(ptag),

                % The number of function symbols represented by this group.
                wpi_num_functors        :: uint,

                % A representation of the code for this primary tag.
                wpi_goal                :: CaseRep
            ).

:- type shared_ptag_info(CaseRep)
    --->    shared_ptag_info(
                % This ptag is shared by more than one function symbol,
                % and the selected switch arm is NOT the same for all of them.
                % (If the selected switch arm *were* the same for all of them,
                % this ptag value would occur in a whole_ptags_info.)

                spi_ptag                :: ptag,
                spi_sectag_locn         :: shared_sectag_locn,

                % MaxSectag, the maximum secondary tag value for this ptag.
                % The number of function symbols that share this ptag
                % will be MaxSectag + 1 (since sectag values start at 0u).
                spi_max_sectag          :: uint,

                % The number of function symbols represented by this group.
                spi_num_functors        :: uint,

                % The next two fields contain the same information in
                % slightly different form. The spi_sectag_to_goal_map field
                % contains this info in the form preferred as the input
                % for the jump table and binary search methods of switching
                % on the secondary tag, while the spi_goal_to_sectags_map
                % contains it in the form preferred by the try_chain and
                % try_me_else chain methods.

                % This field maps each secondary tag value to the case arm
                % it selects.
                spi_sectag_to_goal_map  :: stag_goal_map(CaseRep),

                % This field contains the reverse map, and groups together
                % the sectag values that all select the same switch arm.
                %
                % As explained above, we use shared_ptag_infos only when
                % the switch requires at least two different actions for
                % some of the different sectag values sharing this ptag.
                % However, it is still possible for this map to have
                % just one entry, if
                %
                % - the function symbols using this shared ptag and the
                %   sectag values listed as the value of that one entry select
                %   the switch arm listed in the key of that entry, and
                %
                % - some of the other possible sectag values for this shared
                %   ptag have *no* selected arm in this switch, either because
                %   the switched-on variable cannot be bound to the
                %   corresponding function symbol when the switch is entered,
                %   or because the switch should fail if the variable *is*
                %   bound to that function symbol.
                %
                % However, this map cannot be empty.
                spi_goal_to_sectags_map :: stag_case_map(CaseRep)
            ).

:- type shared_sectag_locn =< sectag_locn
    --->    sectag_local_rest_of_word
    ;       sectag_local_bits(uint8, uint)              % #bits, mask
    ;       sectag_remote_word
    ;       sectag_remote_bits(uint8, uint).            % #bits, mask

% ZZZ g/stag_/s//sectag_/g

    % Map each secondary tag value to the representation of the associated
    % code. A negative secondary tag "value" means "no secondary tag".
    %
    % It is of course possible that there is more than one secondary tag value
    % that maps to the same code. Exploiting such sharing is up to
    % backend-specific code.
    %
:- type stag_goal_map(CaseRep)  == map(uint, CaseRep).
:- type stag_goal_list(CaseRep) == assoc_list(uint, CaseRep).

:- type stag_case_map(CaseRep)  == map(CaseRep, one_or_more(uint)).
:- type stag_case_list(CaseRep) == assoc_list(CaseRep, one_or_more(uint)).

:- type ptag_case_list(CaseRep) == list(ptag_case_entry(CaseRep)).

    % Map primary tag values to information about the secondary tag, if any,
    % for the ptag.the number of constructors sharing them.
    %
    % ZZZ Rename this type, and stop exporting it. All its main uses
    % are in this module; its uses in other modules are just sanity checks
    % that duplicate what this module is doing.
    %
:- type ptag_count_map == map(ptag, ptag_sectag_info).
:- type ptag_sectag_info
    --->    ptag_sectag_info(
                % Terms using this primary tags have their secondary tags,
                % if any, in the location indicated by this field.
                sectag_locn,
                
                % The maximum value of the secondary tag, or -1
                % if this ptag has no secondary tag.
                %
                % If this field is MaxSectag, then the number of values using
                % this primary tag is
                %
                % 1,                if MaxSectag = -1
                % MaxSectag + 1,    if MaxSectag != -1
                %
                % The +1 is there to account for the fact that we start
                % assigning secondary tags values at 0, not 1.
                int
            ).

    % Map case ids to the set of primary tags used in the cons_ids
    % of that case.
    %
:- type case_id_ptags_map == map(case_id, set(ptag)).

    % get_ptag_counts(ModuleInfo, Type, MaxPrimary, PtagCountMap):
    %
    % Find the maximum primary used in Type, and find out how many
    % secondary tags share each one of the used primary tags.
    %
    % ZZZ As of 2024 03 06, this predicate has exactly two calls,
    % from tag_switch.m and ml_tag_switch.m, and both are just before calls
    % to group_cases_by_ptag. We could stop exporting it if we folded its
    % functionality into group_cases_by_ptag.
    %
    % ZZZ It may be interesting to instrument this code to see whether
    % it is worth caching its output somewhere, though in that case we would
    % have to change the interface to take a type_ctor, not a mer_type.
    % (We only ever look at the top type_ctor of the supplied type.)
    %
:- pred get_ptag_counts(module_info::in, mer_type::in,
    uint8::out, ptag_count_map::out) is det.

    % Group together all the cases that depend on the given variable
    % having the same primary tag value.
    %
    % ZZZ The case_id_ptags_map output is no longer used.
    % ZZZ The ptag_case_map output is still used, but for a purpose
    % for which it is overkill.
    %
:- pred group_cases_by_ptag(ptag_count_map::in, list(tagged_case)::in,
    pred(tagged_case, CaseRep, StateA, StateA, StateB, StateB,
        StateC, StateC, StateD, StateD)
        ::in(pred(in, out, in, out, in, out, in, out, in, out) is det),
    StateA::in, StateA::out, StateB::in, StateB::out,
    StateC::in, StateC::out, StateD::in, StateD::out,
    case_id_ptags_map::out, ptag_case_map(CaseRep)::out,
    list(ptag_case_group(CaseRep))::out) is det.

    % Order the given groups based on the number of function symbols they
    % represent, putting the ones with the most function symbols first.
    % Break ties by using the order of the first ptag in each group.
    %
:- pred order_ptag_groups_by_count(list(ptag_case_group(CaseRep))::in,
    list(ptag_case_group(CaseRep))::out) is det.

    % Ensure that each ptag_case_group covers only one ptag value,
    % breaking up any entry in the input that lists two or more ptag values.
    % Put the resulting ptag_case_groups, which are now all specific
    % to one ptag value, into ascending order of those values.
    %
    % ZZZ consider returning either a subtype that hardwires the wpi_tail_ptags
    % field to nil, or a new type that just omits that field.
    %
:- pred order_ptag_specific_groups_by_value(
    list(ptag_case_group(CaseRep))::in,
    list(ptag_case_group(CaseRep))::out) is det.

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
:- import_module hlds.hlds_pred.
:- import_module libs.optimization_options.
:- import_module libs.options.

:- import_module cord.
:- import_module int.
:- import_module int16.
:- import_module int32.
:- import_module int64.
:- import_module int8.
:- import_module io.
:- import_module maybe.
:- import_module require.
:- import_module string.
:- import_module uint.
:- import_module uint16.
:- import_module uint32.
:- import_module uint64.
:- import_module uint8.

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

%-----------------------------------------------------------------------------%

find_switch_category(ModuleInfo, SwitchVarType, SwitchCategory) :-
    SwitchTypeCtorCat = classify_type(ModuleInfo, SwitchVarType),
    SwitchCategory0 = type_ctor_cat_to_switch_cat(SwitchTypeCtorCat),

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
                Globals, SwitchCategory0),
            SmartIndexingForCategory = no
        )
    then
        SwitchCategory = ite_chain_switch
    else
% Enable this code (after getting callers to pass PredId) if you need
% to debug one of the smart indexing methods.
%       globals.lookup_string_option(Globals, experiment, Experiment),
%       ( if string.split_at_char('-', Experiment) = [MinStr, MaxStr] then
%           PredIdInt = pred_id_to_int(PredId),
%           ( if num_is_in_range(PredIdInt, MinStr, MaxStr) then
%               SwitchCategory = SwitchCategory0
%           else
%               SwitchCategory = ite_chain_switch
%           )
%       else
            SwitchCategory = SwitchCategory0
%       )
    ).

:- pred num_is_in_range(int::in, string::in, string::in) is semidet.
:- pragma consider_used(pred(num_is_in_range/3)).

num_is_in_range(PredIdInt, MinStr, MaxStr) :-
    ( if MinStr = "" then
        % Cannot be less than the minimum if there is no minimum.
        true
    else if string.to_int(MinStr, Min) then
        Min =< PredIdInt 
    else
        unexpected($pred, "minimum is not a number")
    ),
    ( if MaxStr = "" then
        % Cannot be greater than the maximum if there is no maximum.
        true
    else if string.to_int(MaxStr, Max) then
        PredIdInt =< Max
    else
        unexpected($pred, "maximum is not a number")
    ).

:- func is_smart_indexing_allowed_for_category(globals, switch_category)
    = bool.

is_smart_indexing_allowed_for_category(Globals, SwitchCategory) = Allowed :-
    globals.get_opt_tuple(Globals, OptTuple),
    (
        SwitchCategory = ite_chain_switch,
        Allowed = yes
    ;
        SwitchCategory = int_max_32_switch,
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
        SwitchCategory = int_64_switch,
        % We do not have a separate option for controlling smart indexing
        % of 64-bit integers.
        Gen = OptTuple ^ ot_use_smart_indexing,
        ( Gen = use_smart_indexing, Allowed = yes
        ; Gen = do_not_use_smart_indexing, Allowed = no
        )
    ).

    % Convert a type constructor category to a switch category.
    %
:- func type_ctor_cat_to_switch_cat(type_ctor_category) = switch_category.

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
            SwitchCat = int_max_32_switch
        ;
            ( IntType = int_type_int64
            ; IntType = int_type_uint64
            ),
            SwitchCat = int_64_switch
        )
    ;
        ( CtorCat = ctor_cat_enum(_)
        ; CtorCat = ctor_cat_builtin(cat_builtin_char)
        ),
        SwitchCat = int_max_32_switch
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

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
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

switch_density(NumCases, NumValuesInRange) = Density :-
    Density = (NumCases * 100) // NumValuesInRange.

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

% ZZZ
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
    % collisions if we double the table size again.

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
        !StateA, !StateB, !StateC, !StateD, SortedTable) :-
    string_binary_entries(TaggedCases, RepresentCase,
        !StateA, !StateB, !StateC, !StateD, [], UnsortedTable),
    list.sort(UnsortedTable, SortedTable).

:- pred string_binary_entries(list(tagged_case)::in,
    pred(tagged_case, CaseRep, StateA, StateA, StateB, StateB,
        StateC, StateC, StateD, StateD)
        ::in(pred(in, out, in, out, in, out, in, out, in, out) is det),
    StateA::in, StateA::out, StateB::in, StateB::out,
    StateC::in, StateC::out, StateD::in, StateD::out,
    assoc_list(string, CaseRep)::in, assoc_list(string, CaseRep)::out) is det.

string_binary_entries([], _,
        !StateA, !StateB, !StateC, !StateD, !UnsortedTable).
string_binary_entries([TaggedCase | TaggedCases], RepresentCase,
        !StateA, !StateB, !StateC, !StateD, !UnsortedTable) :-
    string_binary_entries(TaggedCases, RepresentCase,
        !StateA, !StateB, !StateC, !StateD, !UnsortedTable),
    RepresentCase(TaggedCase, CaseRep, !StateA, !StateB, !StateC, !StateD),
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

get_ptag_counts(ModuleInfo, Type, MaxPrimary, PtagCountMap) :-
    type_to_ctor_det(Type, TypeCtor),
    module_info_get_type_table(ModuleInfo, TypeTable),
    lookup_type_ctor_defn(TypeTable, TypeCtor, TypeDefn),
    hlds_data.get_type_defn_body(TypeDefn, TypeBody),
    (
        TypeBody = hlds_du_type(type_body_du(_, _, _, MaybeRepn, _)),
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
            Info = ptag_sectag_info(SectagLocn, -1),
            map.det_insert(Ptag, Info, !PtagCountMap)
        )
    ;
        ConsTag = remote_args_tag(RemoteArgsTagInfo),
        (
            (
                RemoteArgsTagInfo = remote_args_only_functor,
                Ptag = ptag(0u8)
            ;
                RemoteArgsTagInfo = remote_args_unshared(Ptag)
            ),
            SectagLocn = sectag_none,
            Ptag = ptag(Primary),
            !:MaxPrimary = uint8.max(Primary, !.MaxPrimary),
            ( if map.search(!.PtagCountMap, Ptag, _) then
                unexpected($pred, "unshared tag is shared")
            else
                Info = ptag_sectag_info(SectagLocn, -1),
                map.det_insert(Ptag, Info, !PtagCountMap)
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
            ( if map.search(!.PtagCountMap, Ptag, Info0) then
                Info0 = ptag_sectag_info(OldSectagLocn, MaxSoFar),
                expect(unify(OldSectagLocn, SectagLocn), $pred,
                    "remote tag is shared with non-remote"),
                int.max(Secondary, MaxSoFar, Max),
                Info = ptag_sectag_info(SectagLocn, Max),
                map.det_update(Ptag, Info, !PtagCountMap)
            else
                Info = ptag_sectag_info(SectagLocn, Secondary),
                map.det_insert(Ptag, Info, !PtagCountMap)
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
        ( if map.search(!.PtagCountMap, Ptag, Info0) then
            Info0 = ptag_sectag_info(OldSectagLocn, MaxSoFar),
            expect(unify(OldSectagLocn, SectagLocn), $pred,
                "local tag is shared with non-local"),
            int.max(Secondary, MaxSoFar, Max),
            Info = ptag_sectag_info(SectagLocn, Max),
            map.det_update(Ptag, Info, !PtagCountMap)
        else
            Info = ptag_sectag_info(SectagLocn, Secondary),
            map.det_insert(Ptag, Info, !PtagCountMap)
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

group_cases_by_ptag(PtagCountMap, TaggedCases, RepresentCase,
        !StateA, !StateB, !StateC, !StateD,
        CaseNumPtagsMap, PtagCaseMap, PtagGroups) :-
    group_cases_by_ptag_loop(PtagCountMap, TaggedCases, RepresentCase,
        !StateA, !StateB, !StateC, !StateD,
        map.init, CaseNumPtagsMap, map.init, PtagCaseMap),
    map.to_assoc_list(PtagCaseMap, PtagCaseList),
    build_ptag_groups(PtagCountMap, PtagCaseList,
        map.init, WholePtagsMap, [], SharedPtagInfos),
    map.values(WholePtagsMap, WholePtagsInfos),
    WholePtagGroups = list.map(wrap_whole_ptags_info, WholePtagsInfos),
    SharedPtagGroups = list.map(wrap_shared_ptag_info, SharedPtagInfos),
    PtagGroups = WholePtagGroups ++ SharedPtagGroups.

:- pred group_cases_by_ptag_loop(ptag_count_map::in, list(tagged_case)::in,
    pred(tagged_case, CaseRep, StateA, StateA, StateB, StateB,
        StateC, StateC, StateD, StateD)
        ::in(pred(in, out, in, out, in, out, in, out, in, out) is det),
    StateA::in, StateA::out, StateB::in, StateB::out,
    StateC::in, StateC::out, StateD::in, StateD::out,
    case_id_ptags_map::in, case_id_ptags_map::out,
    ptag_case_map(CaseRep)::in, ptag_case_map(CaseRep)::out) is det.

group_cases_by_ptag_loop(_, [], _,
        !StateA, !StateB, !StateC, !StateD, !CaseNumPtagsMap, !PtagCaseMap).
group_cases_by_ptag_loop(PtagCountMap, [TaggedCase | TaggedCases],
        RepresentCase, !StateA, !StateB, !StateC, !StateD,
        !CaseNumPtagsMap, !PtagCaseMap) :-
    TaggedCase = tagged_case(MainTaggedConsId, OtherConsIds, CaseId, _Goal),
    RepresentCase(TaggedCase, CaseRep, !StateA, !StateB, !StateC, !StateD),
    group_case_by_ptag(PtagCountMap, CaseId, CaseRep,
        MainTaggedConsId, !CaseNumPtagsMap, !PtagCaseMap),
    list.foldl2(group_case_by_ptag(PtagCountMap, CaseId, CaseRep),
        OtherConsIds, !CaseNumPtagsMap, !PtagCaseMap),
    group_cases_by_ptag_loop(PtagCountMap, TaggedCases, RepresentCase,
        !StateA, !StateB, !StateC, !StateD, !CaseNumPtagsMap, !PtagCaseMap).

:- pred group_case_by_ptag(ptag_count_map::in, case_id::in, CaseRep::in,
    tagged_cons_id::in,
    map(case_id, set(ptag))::in, map(case_id, set(ptag))::out,
    ptag_case_map(CaseRep)::in, ptag_case_map(CaseRep)::out) is det.

group_case_by_ptag(PtagCountMap, CaseId, CaseRep, TaggedConsId,
        !CaseIdPtagsMap, !PtagCaseMap) :-
    TaggedConsId = tagged_cons_id(_ConsId, ConsTag),
    (
        ConsTag = direct_arg_tag(Ptag),
        ( if map.search(!.PtagCaseMap, Ptag, _Group) then
            unexpected($pred, "unshared tag is shared")
        else
            PtagCase = ptag_case_unshared(CaseRep),
            map.det_insert(Ptag, PtagCase, !PtagCaseMap)
        )
    ;
        ConsTag = remote_args_tag(RemoteArgsTagInfo),
        (
            (
                RemoteArgsTagInfo = remote_args_only_functor,
                Ptag = ptag(0u8)
            ;
                RemoteArgsTagInfo = remote_args_unshared(Ptag)
            ),
            ( if map.search(!.PtagCaseMap, Ptag, _Group) then
                unexpected($pred, "unshared tag is shared")
            else
                PtagCase = ptag_case_unshared(CaseRep),
                map.det_insert(Ptag, PtagCase, !PtagCaseMap)
            )
        ;
            (
                RemoteArgsTagInfo = remote_args_shared(Ptag, RemoteSectag),
                RemoteSectag = remote_sectag(Sectag, SectagSize),
                (
                    SectagSize = rsectag_word,
                    SectagLocn = sectag_remote_word
                ;
                    SectagSize = rsectag_subword(SectagBits),
                    SectagBits = sectag_bits(NumSectagBits, Mask),
                    SectagLocn = sectag_remote_bits(NumSectagBits, Mask)
                )
            ;
                RemoteArgsTagInfo = remote_args_ctor(Data),
                Primary = 0u8,
                Ptag = ptag(Primary),
                SectagLocn = sectag_remote_word,
                Sectag = Data
            ),
            add_sectag_to_shared_ptag(PtagCountMap, Ptag,
                SectagLocn, Sectag, CaseRep, !PtagCaseMap)
        )
    ;
        (
            ConsTag = shared_local_tag_no_args(Ptag, LocalSectag, MustMask),
            LocalSectag = local_sectag(Sectag, _, SectagBits),
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
                LocalSectag = local_sectag(Sectag, _, SectagBits),
                SectagBits = sectag_bits(NumSectagBits, Mask),
                SectagLocn = sectag_local_bits(NumSectagBits, Mask)
            )
        ),
        add_sectag_to_shared_ptag(PtagCountMap, Ptag,
            SectagLocn, Sectag, CaseRep, !PtagCaseMap)
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

:- pred add_sectag_to_shared_ptag(ptag_count_map::in,
    ptag::in, shared_sectag_locn::in, uint::in, CaseRep::in,
    ptag_case_map(CaseRep)::in, ptag_case_map(CaseRep)::out) is det.

add_sectag_to_shared_ptag(PtagCountMap, Ptag, SectagLocn, Sectag, CaseRep,
        !PtagCaseMap) :-
    ( if map.search(!.PtagCaseMap, Ptag, PtagCase0) then
        (
            PtagCase0 = ptag_case_unshared(_),
            unexpected($pred, "adding shared to unshared")
        ;
            PtagCase0 = ptag_case_shared(SectagLocn0, MaxSectag, StagGoalMap0),
            expect(unify(SectagLocn0, SectagLocn), $pred,
                "sectag locn mismatch"),
            map.det_insert(Sectag, CaseRep, StagGoalMap0, StagGoalMap),
            PtagCase = ptag_case_shared(SectagLocn, MaxSectag, StagGoalMap),
            map.det_update(Ptag, PtagCase, !PtagCaseMap)
        )
    else
        map.lookup(PtagCountMap, Ptag, PtagSectagInfo),
        PtagSectagInfo = ptag_sectag_info(InfoSectagLocn, MaxSectagInt),
        expect(unify(coerce(SectagLocn), InfoSectagLocn), $pred,
            "SectagLocn != InfoSectagLocn"),
        % For a shared secondary tag, MaxSectagInt cannot be negative.
        MaxSectag = uint.det_from_int(MaxSectagInt),
        StagGoalMap = map.singleton(Sectag, CaseRep),
        PtagCase = ptag_case_shared(SectagLocn, MaxSectag, StagGoalMap),
        map.det_insert(Ptag, PtagCase, !PtagCaseMap)
    ).

%-----------------------------------------------------------------------------%

:- type whole_ptags_map(CaseRep) == map(CaseRep, whole_ptags_info(CaseRep)).

:- pred build_ptag_groups(ptag_count_map::in,
    assoc_list(ptag, ptag_case(CaseRep))::in,
    whole_ptags_map(CaseRep)::in, whole_ptags_map(CaseRep)::out,
    list(shared_ptag_info(CaseRep))::in, list(shared_ptag_info(CaseRep))::out)
    is det.

build_ptag_groups(_PtagCountMap, [], !WholePtagsMap, !SharedPtagInfos).
build_ptag_groups(PtagCountMap, [Entry | Entries],
        !WholePtagsMap, !SharedPtagInfos) :-
    Entry = Ptag - PtagCase,
    (
        PtagCase = ptag_case_unshared(CaseRep),
        record_whole_ptag(Ptag, 1u, CaseRep, !WholePtagsMap)
    ;
        PtagCase = ptag_case_shared(SharedSectagLocn, MaxSectag, StagGoalMap),
        % There will only ever be at most one primary tag value with
        % a shared local tag, and there will only ever be at most one primary
        % tag value with a shared remote tag, so we can never have
        %
        % - two ptags with SectagLocn = sectag_local_*
        % - two ptags with SectagLocn = sectag_remote
        %
        % We can never have two entries where one is sectag_local_bits(_)
        % and the other is sectag_local_rest_of_word; all function symbols
        % whose representation includes a local sectag must agree on whether
        % that sectag may ever be followed by arguments (sectag_local_bits)
        % or not (sectag_local_rest_of_word).
        %
        % We can have two ptags, one with either SectagLocn =
        % sectag_local_bits(_) or SectagLocn = sectag_local_rest_of_word,
        % and the other with SectagLocn = sectag_remote, but even if their
        % sectag_value to code maps were identical, their overall code couldn't
        % be identical, since they would have to get the secondary tags from
        % different places.

        map.lookup(PtagCountMap, Ptag, SectagInfo),
        SectagInfo = ptag_sectag_info(SectagLocn, MaybeMaxSectag),
        expect(unify(coerce(SharedSectagLocn), SectagLocn), $pred,
            "SharedSectagLocn != SectagLocn"),
        ( if
            uint.from_int(MaybeMaxSectag, MaxSectag0),
            MaxSectag0 \= MaxSectag
        then
            unexpected($pred, "MaxSectag0 != MaxSectag")
        else
            true
        ),
        map.foldl2(build_sectag_case_cord_map, StagGoalMap,
            map.init, StagCaseCordMap, 0u, NumFunctors),
        StagCaseMap =
            map.map_values_only(cord_to_one_or_more, StagCaseCordMap),
        map.to_sorted_assoc_list(StagCaseMap, StagCaseList),
        ( if
            StagCaseList = [OneStagCase],
            NumFunctors = MaxSectag + 1u
        then
            OneStagCase = CaseRep - _SectagValues,
            record_whole_ptag(Ptag, NumFunctors, CaseRep, !WholePtagsMap)
        else
            SharedPtagInfo = shared_ptag_info(Ptag, SharedSectagLocn,
                MaxSectag, NumFunctors, StagGoalMap, StagCaseMap),
            !:SharedPtagInfos = [SharedPtagInfo | !.SharedPtagInfos]
        )
    ),
    build_ptag_groups(PtagCountMap, Entries, !WholePtagsMap, !SharedPtagInfos).

:- pred record_whole_ptag(ptag::in, uint::in, CaseRep::in,
    whole_ptags_map(CaseRep)::in, whole_ptags_map(CaseRep)::out) is det.

record_whole_ptag(Ptag, PtagNumFunctors, CaseRep, !GroupMap) :-
    ( if map.search(!.GroupMap, CaseRep, Entry0) then
        Entry0 = whole_ptags_info(HeadPtag, TailPtags0, NumFunctors0,
            CaseRep0),
        expect(unify(CaseRep, CaseRep0), $pred, "CaseRep != CaseRep0"),
        % This is quadratic, but this does not matter, because
        % we can append to the list at most 6 time. The reason is:
        %
        % - a type can have at most eight ptags;
        % - you can't have a switch in which all eight map to the same
        %   CaseRep, because that would be a switch with only one arm,
        %   which we simplify away, and
        % - for the first ptag, the map.search fails, and does not
        %   require an append.
        TailPtags = TailPtags0 ++ [Ptag],
        NumFunctors = NumFunctors0 + PtagNumFunctors,
        Entry = whole_ptags_info(HeadPtag, TailPtags, NumFunctors, CaseRep),
        map.det_update(CaseRep, Entry, !GroupMap)
    else
        Entry = whole_ptags_info(Ptag, [], PtagNumFunctors, CaseRep),
        map.det_insert(CaseRep, Entry, !GroupMap)
    ).

:- type stag_case_cord_map(CaseRep) == map(CaseRep, cord(uint)).

:- pred build_sectag_case_cord_map(uint::in, CaseRep::in,
    stag_case_cord_map(CaseRep)::in, stag_case_cord_map(CaseRep)::out,
    uint::in, uint::out) is det.

build_sectag_case_cord_map(Sectag, CaseRep, !StagCaseCordMap, !NumFunctors) :-
    !:NumFunctors = !.NumFunctors + 1u,
    ( if map.search(!.StagCaseCordMap, CaseRep, Cord0) then
        cord.snoc(Sectag, Cord0, Cord),
        map.det_update(CaseRep, Cord, !StagCaseCordMap)
    else
        map.det_insert(CaseRep, cord.singleton(Sectag), !StagCaseCordMap)
    ).

:- func cord_to_one_or_more(cord(uint)) = one_or_more(uint).

cord_to_one_or_more(Cord) = OoM :-
    List = cord.list(Cord),
    list.det_head_tail(List, Head, Tail),
    OoM = one_or_more(Head, Tail).

:- func wrap_whole_ptags_info(whole_ptags_info(CaseRep)) =
    ptag_case_group(CaseRep).

wrap_whole_ptags_info(WholeInfo) = one_or_more_whole_ptags(WholeInfo).

:- func wrap_shared_ptag_info(shared_ptag_info(CaseRep)) = 
    ptag_case_group(CaseRep).

wrap_shared_ptag_info(SharedInfo) = one_shared_ptag(SharedInfo).

%-----------------------------------------------------------------------------%

order_ptag_groups_by_count(Groups, SortedGroups) :-
    list.sort(order_groups_by_more_functors, Groups, SortedGroups).

:- pred order_groups_by_more_functors(ptag_case_group(CaseRep)::in,
    ptag_case_group(CaseRep)::in, comparison_result::out) is det.

order_groups_by_more_functors(GroupA, GroupB, CompareResult) :-
    NumFunctorsA = num_functors_in_ptag_case_group(GroupA),
    NumFunctorsB = num_functors_in_ptag_case_group(GroupB),
    ( if NumFunctorsA > NumFunctorsB then
        % We want the groups with the largest counts first ...
        CompareResult = (<)
    else if NumFunctorsA < NumFunctorsB then
        % ... and the groups with the smallest counts last.
        CompareResult = (>)
    else
        % If two groups cover the same number of function symbols,
        % then put them in the order given by their main ptags.
        MainPtagA = main_ptag_in_ptag_case_group(GroupA),
        MainPtagB = main_ptag_in_ptag_case_group(GroupB),
        compare(CompareResult, MainPtagA, MainPtagB)
    ).

:- func num_functors_in_ptag_case_group(ptag_case_group(CaseRep)) = uint.

num_functors_in_ptag_case_group(Group) = NumFunctors :-
    (
        Group = one_or_more_whole_ptags(WholeInfo),
        NumFunctors = WholeInfo ^ wpi_num_functors
    ;
        Group = one_shared_ptag(SharedInfo),
        NumFunctors = SharedInfo ^ spi_num_functors
    ).

:- func main_ptag_in_ptag_case_group(ptag_case_group(CaseRep)) = uint8.

main_ptag_in_ptag_case_group(Group) = MainPtagUint8 :-
    (
        Group = one_or_more_whole_ptags(WholeInfo),
        ptag(MainPtagUint8) = WholeInfo ^ wpi_head_ptag
    ;
        Group = one_shared_ptag(SharedInfo),
        ptag(MainPtagUint8) = SharedInfo ^ spi_ptag
    ).

%-----------------------------------------------------------------------------%

order_ptag_specific_groups_by_value(Groups0, SortedSpecificGroups) :-
    specialize_and_record_ptag_case_groups(Groups0, map.init, SpecificMap),
    map.values(SpecificMap, SortedSpecificGroups).

:- pred specialize_and_record_ptag_case_groups(
    list(ptag_case_group(CaseRep))::in,
    map(ptag, ptag_case_group(CaseRep))::in,
    map(ptag, ptag_case_group(CaseRep))::out) is det.

specialize_and_record_ptag_case_groups([], !SpecificMap).
specialize_and_record_ptag_case_groups([Group | Groups], !SpecificMap) :-
    (
        Group = one_or_more_whole_ptags(WholeInfo),
        WholeInfo = whole_ptags_info(MainPtag, OtherPtags, _NF, CaseRep),
        record_specialized_versions(CaseRep, [MainPtag | OtherPtags],
            !SpecificMap)
    ;
        Group = one_shared_ptag(SharedInfo),
        SharedInfo = shared_ptag_info(Ptag, _, _, _, _, _),
        map.det_insert(Ptag, Group, !SpecificMap)
    ),
    specialize_and_record_ptag_case_groups(Groups, !SpecificMap).

:- pred record_specialized_versions(CaseRep::in, list(ptag)::in,
    map(ptag, ptag_case_group(CaseRep))::in,
    map(ptag, ptag_case_group(CaseRep))::out) is det.

record_specialized_versions(_, [], !SpecificMap).
record_specialized_versions(CaseRep, [Ptag | Ptags], !SpecificMap) :-
    % In the original whole_ptags_info we are splitting up, each of the
    % ptags is an unshared ptag, which means that it corresponds to
    % exactly one function symbol.
    WholeInfo = whole_ptags_info(Ptag, [], 1u, CaseRep),
    Group = one_or_more_whole_ptags(WholeInfo),
    map.det_insert(Ptag, Group, !SpecificMap),
    record_specialized_versions(CaseRep, Ptags, !SpecificMap).

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
