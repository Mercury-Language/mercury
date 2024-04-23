%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2000-2012 The University of Melbourne.
% Copyright (C) 2013-2024 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: switch_util.m.
% Authors: fjh, zs.
%
% This module defines stuff for generating switches
%
% - that is shared between the MLDS and LLDS backends, and
% - is not specific to any one specialized method of switch implementation.
%
% The modules lookup_switch_util.m, string_switch_util.m and tag_switch_util.m
% contains the stuff that *is* specific to a method of implementation.
%
%---------------------------------------------------------------------------%

:- module backend_libs.switch_util.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module libs.
:- import_module libs.globals.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module list.

%---------------------------------------------------------------------------%
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

%---------------------------------------------------------------------------%
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

%---------------------------------------------------------------------------%

    % get_word_bits(Globals, MinWordSize):
    %
    % Return in MinWordSize the largest word_size that both the host machine
    % and the target machine can handle.
    %
    % We use this predicate to prevent cross-compilation errors when generating
    % bit vector tests for lookup switches by making sure that the bitvector
    % uses a number of bits that will fit both on this machine (so that
    % we can correctly generate it), and on the target machine (so that
    % it can be executed correctly).
    %
    % Since the only two word sizes we support are 32 and 64 bits,
    % the returned value will be one of these.
    %
:- pred get_target_host_min_word_size(globals::in, word_size::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.type_util.
:- import_module hlds.hlds_code_util.
:- import_module hlds.hlds_pred.
:- import_module libs.optimization_options.
:- import_module parse_tree.prog_type.

:- import_module bool.
:- import_module int.
:- import_module int16.
:- import_module int32.
:- import_module int64.
:- import_module int8.
:- import_module require.
:- import_module string.
:- import_module uint.
:- import_module uint16.
:- import_module uint32.
:- import_module uint64.
:- import_module uint8.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
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

%---------------------------------------------------------------------------%

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

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
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
        ; ConsTag = closure_tag(_, _)
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

%---------------------------------------------------------------------------%

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

%---------------------------------------------------------------------------%

get_target_host_min_word_size(Globals, MinWordSize) :-
    globals.get_word_size(Globals, TargetWordSize),
    int.bits_per_int(HostWordBits),
    ( if HostWordBits = 64 then
        HostWordSize = word_size_64
    else if HostWordBits = 32 then
        HostWordSize = word_size_32
    else
        unexpected($pred, "HostWordSize not 64 or 32 bits")
    ),
    ( if TargetWordSize = word_size_64, HostWordSize = word_size_64 then
        MinWordSize = word_size_64
    else
        MinWordSize = word_size_32
    ).

%---------------------------------------------------------------------------%
:- end_module backend_libs.switch_util.
%---------------------------------------------------------------------------%
