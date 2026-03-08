%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2000-2012 The University of Melbourne.
% Copyright (C) 2013-2026 The Mercury team.
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
    --->    not_int_switch
    ;       int_switch(int_switch_info).

:- type int_switch_info
    --->    int_switch_info(
                % The original type of the values the switch is on.
                % (We store the min and the max of these values
                % in the isi_limits field, but as int32s.)
                %
                % This field is not currently used. It is recorded
                % because later it may turn out that if this field
                % is not int_type_int, but e.g. init_type_uint16,
                % then some code generator may need to do some casts.
                isi_orig_type               :: int_type,

                % The number of values in the min to max range.
                isi_num_values_in_range     :: uint,

                % The number of values mentioned in case arms.
                isi_num_values_in_cases     :: uint,

                % The min and the max of the values mentioned by case arms.
                isi_limits                  :: int_switch_limits
            ).

    % While switches on values of some integer types may not be representable
    % as int32s (definitely including int64, uint32 and uint64, and also
    % including int and uint on 64 bit platforms), we only implement
    % dense and lookup switches if all the cons_ids in all switch cases
    % *are* so representable.
    %
    % Note that this restricts switches on unsigned integers more than
    % switches on signed integers, since they can make use of only
    % the non-negative half of the int32 range.
:- type int_switch_limits
    --->    int_switch_limits(
                isl_min_value               :: int32,
                isl_max_value               :: int32
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
    % NOTE Conceptually, we *should* return two uints, not two ints,
    % but at the moment (2026 mar 4), pretty much all the callers
    % compare the returned values with ints, not uints.
    % Keeping the return values as ints prevents the need for a lot of casts.
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

:- import_module hlds.hlds_code_util.
:- import_module hlds.hlds_pred.
:- import_module hlds.type_util.
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
            tag_cases_in_int_switch(ModuleInfo, SwitchVarType, int_type_int,
                TaggedMainConsId, OtherConsIds, Goal, Cases, IntTagVal,
                TaggedCase, TaggedCases, MaybeIntSwitchLimits)
        ;
            IntTag = int_tag_uint(UIntTagVal),
            tag_cases_in_int_switch(ModuleInfo, SwitchVarType, int_type_uint,
                TaggedMainConsId, OtherConsIds, Goal, Cases, UIntTagVal,
                TaggedCase, TaggedCases, MaybeIntSwitchLimits)
        ;
            IntTag = int_tag_int8(Int8TagVal),
            tag_cases_in_int_switch(ModuleInfo, SwitchVarType, int_type_int8,
                TaggedMainConsId, OtherConsIds, Goal, Cases, Int8TagVal,
                TaggedCase, TaggedCases, MaybeIntSwitchLimits)
        ;
            IntTag = int_tag_uint8(UInt8TagVal),
            tag_cases_in_int_switch(ModuleInfo, SwitchVarType, int_type_uint8,
                TaggedMainConsId, OtherConsIds, Goal, Cases, UInt8TagVal,
                TaggedCase, TaggedCases, MaybeIntSwitchLimits)
        ;
            IntTag = int_tag_int16(Int16TagVal),
            tag_cases_in_int_switch(ModuleInfo, SwitchVarType, int_type_int16,
                TaggedMainConsId, OtherConsIds, Goal, Cases, Int16TagVal,
                TaggedCase, TaggedCases, MaybeIntSwitchLimits)
        ;
            IntTag = int_tag_uint16(UInt16TagVal),
            tag_cases_in_int_switch(ModuleInfo, SwitchVarType, int_type_uint16,
                TaggedMainConsId, OtherConsIds, Goal, Cases, UInt16TagVal,
                TaggedCase, TaggedCases, MaybeIntSwitchLimits)
        ;
            IntTag = int_tag_int32(Int32TagVal),
            tag_cases_in_int_switch(ModuleInfo, SwitchVarType, int_type_int32,
                TaggedMainConsId, OtherConsIds, Goal, Cases, Int32TagVal,
                TaggedCase, TaggedCases, MaybeIntSwitchLimits)
        ;
            IntTag = int_tag_uint32(UInt32TagVal),
            tag_cases_in_int_switch(ModuleInfo, SwitchVarType, int_type_uint32,
                TaggedMainConsId, OtherConsIds, Goal, Cases, UInt32TagVal,
                TaggedCase, TaggedCases, MaybeIntSwitchLimits)
        ;
            IntTag = int_tag_int64(Int64TagVal),
            tag_cases_in_int_switch(ModuleInfo, SwitchVarType, int_type_int64,
                TaggedMainConsId, OtherConsIds, Goal, Cases, Int64TagVal,
                TaggedCase, TaggedCases, MaybeIntSwitchLimits)
        ;
            IntTag = int_tag_uint64(UInt64TagVal),
            tag_cases_in_int_switch(ModuleInfo, SwitchVarType, int_type_uint64,
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

%---------------------------------------------------------------------------%

:- typeclass int_tag_value(T) where [
    func int_tag_min(T, T) = T,
    func int_tag_max(T, T) = T,
    pred cons_tag_is_int_tag(cons_tag::in, T::out) is semidet,
    pred fit_into_int32(T::in, int32::out) is semidet
].

% XXX Many of the calls to uint*.cast_to_int lose mathematical equality.
% Are there semidet versions of conversions?

:- instance int_tag_value(int) where [
    func(int_tag_min/2) is int.min,
    func(int_tag_max/2) is int.max,
    ( cons_tag_is_int_tag(ConsTag, TagVal) :-
        ConsTag = int_tag(int_tag_int(TagVal))
    ),
    ( fit_into_int32(IW, I32) :-
        int32.from_int(IW, I32)
    )
].

:- instance int_tag_value(int8) where [
    func(int_tag_min/2) is int8.min,
    func(int_tag_max/2) is int8.max,
    ( cons_tag_is_int_tag(ConsTag, TagVal) :-
        ConsTag = int_tag(int_tag_int8(TagVal))
    ),
    ( fit_into_int32(I8, I32) :-
        IW = int8.to_int(I8),
        int32.from_int(IW, I32)
    )
].

:- instance int_tag_value(int16) where [
    func(int_tag_min/2) is int16.min,
    func(int_tag_max/2) is int16.max,
    ( cons_tag_is_int_tag(ConsTag, TagVal) :-
        ConsTag = int_tag(int_tag_int16(TagVal))
    ),
    ( fit_into_int32(I16, I32) :-
        IW = int16.to_int(I16),
        int32.from_int(IW, I32)
    )
].

:- instance int_tag_value(int32) where [
    func(int_tag_min/2) is int32.min,
    func(int_tag_max/2) is int32.max,
    ( cons_tag_is_int_tag(ConsTag, TagVal) :-
        ConsTag = int_tag(int_tag_int32(TagVal))
    ),
    ( fit_into_int32(I32, I32) :-
        true
    )
].

:- instance int_tag_value(int64) where [
    func(int_tag_min/2) is int64.min,
    func(int_tag_max/2) is int64.max,
    ( cons_tag_is_int_tag(ConsTag, TagVal) :-
        ConsTag = int_tag(int_tag_int64(TagVal))
    ),
    ( fit_into_int32(I64, I32) :-
        int64.to_int(I64, IW),
        int32.from_int(IW, I32)
    )
].

:- instance int_tag_value(uint) where [
    func(int_tag_min/2) is uint.min,
    func(int_tag_max/2) is uint.max,
    ( cons_tag_is_int_tag(ConsTag, TagVal) :-
        ConsTag = int_tag(int_tag_uint(TagVal))
    ),
    ( fit_into_int32(UW, I32) :-
        IW = uint.cast_to_int(UW),
        int32.from_int(IW, I32)
    )
].

:- instance int_tag_value(uint8) where [
    func(int_tag_min/2) is uint8.min,
    func(int_tag_max/2) is uint8.max,
    ( cons_tag_is_int_tag(ConsTag, TagVal) :-
        ConsTag = int_tag(int_tag_uint8(TagVal))
    ),
    ( fit_into_int32(U8, I32) :-
        IW = uint8.to_int(U8),
        int32.from_int(IW, I32)
    )
].

:- instance int_tag_value(uint16) where [
    func(int_tag_min/2) is uint16.min,
    func(int_tag_max/2) is uint16.max,
    ( cons_tag_is_int_tag(ConsTag, TagVal) :-
        ConsTag = int_tag(int_tag_uint16(TagVal))
    ),
    ( fit_into_int32(U16, I32) :-
        IW = uint16.cast_to_int(U16),
        int32.from_int(IW, I32)
    )
].

:- instance int_tag_value(uint32) where [
    func(int_tag_min/2) is uint32.min,
    func(int_tag_max/2) is uint32.max,
    ( cons_tag_is_int_tag(ConsTag, TagVal) :-
        ConsTag = int_tag(int_tag_uint32(TagVal))
    ),
    ( fit_into_int32(U32, I32) :-
        IW = uint32.cast_to_int(U32),
        int32.from_int(IW, I32)
    )
].

:- instance int_tag_value(uint64) where [
    func(int_tag_min/2) is uint64.min,
    func(int_tag_max/2) is uint64.max,
    ( cons_tag_is_int_tag(ConsTag, TagVal) :-
        ConsTag = int_tag(int_tag_uint64(TagVal))
    ),
    ( fit_into_int32(U64, I32) :-
        IW = uint64.cast_to_int(U64),
        int32.from_int(IW, I32)
    )
].

%---------------------------------------------------------------------------%

:- pred tag_cases_in_int_switch(module_info::in, mer_type::in, int_type::in,
    tagged_cons_id::in, list(cons_id)::in, hlds_goal::in,
    list(case)::in, T::in, tagged_case::out, list(tagged_case)::out,
    maybe_int_switch_info::out) is det <= int_tag_value(T).

tag_cases_in_int_switch(ModuleInfo, SwitchVarType, IntType, TaggedMainConsId,
        OtherConsIds, Goal, Cases, IntTagVal, TaggedCase, TaggedCases,
        MaybeIntSwitchInfo) :-
    % This accounts for MainConsId.
    NumValuesInCasesU0 = 1u,
    list.map_foldl3(tag_cons_id_in_int_switch(ModuleInfo),
        OtherConsIds, TaggedOtherConsIds,
        IntTagVal, LowerLimit1, IntTagVal, UpperLimit1,
        NumValuesInCasesU0, NumValuesInCasesU1),
    TaggedCase = tagged_case(TaggedMainConsId, TaggedOtherConsIds,
        case_id(0), Goal),
    do_tag_cases_in_int_switch(ModuleInfo, SwitchVarType, 1,
        Cases, TaggedCases,
        LowerLimit1, LowerLimit, UpperLimit1, UpperLimit,
        NumValuesInCasesU1, NumValuesInCasesU),
    ( if
        fit_into_int32(LowerLimit, LowerLimitI32),
        fit_into_int32(UpperLimit, UpperLimitI32)
    then
        LimitsU32 = int_switch_limits(LowerLimitI32, UpperLimitI32),
        NumValuesInRangeI32 = UpperLimitI32 - LowerLimitI32 + 1i32,
        NumValuesInRangeI = int32.to_int(NumValuesInRangeI32),
        % Since UpperLimitI32 cannot be smaller than LowerLimitI32,
        % NumValuesInRangeI cannot be negative.
        NumValuesInRangeU = uint.det_from_int(NumValuesInRangeI),
        IntSwitchInfo = int_switch_info(IntType,
            NumValuesInRangeU, NumValuesInCasesU, LimitsU32),
        MaybeIntSwitchInfo = int_switch(IntSwitchInfo)
    else
        MaybeIntSwitchInfo = not_int_switch
    ).

:- pred do_tag_cases_in_int_switch(module_info::in, mer_type::in, int::in,
    list(case)::in, list(tagged_case)::out, T::in, T::out,
    T::in, T::out, uint::in, uint::out) is det <= int_tag_value(T).

do_tag_cases_in_int_switch(_, _, _, [], [],
        !LowerLimit, !UpperLimit, !NumValues).
do_tag_cases_in_int_switch(ModuleInfo, SwitchVarType, CaseNum, [Case | Cases],
        [TaggedCase | TaggedCases], !LowerLimit, !UpperLimit, !NumValues) :-
    Case = case(MainConsId, OtherConsIds, Goal),
    tag_cons_id_in_int_switch(ModuleInfo, MainConsId, TaggedMainConsId,
        !LowerLimit, !UpperLimit, !NumValues),
    list.map_foldl3(tag_cons_id_in_int_switch(ModuleInfo),
        OtherConsIds, TaggedOtherConsIds,
        !LowerLimit, !UpperLimit, !NumValues),
    TaggedCase = tagged_case(TaggedMainConsId, TaggedOtherConsIds,
        case_id(CaseNum), Goal),
    do_tag_cases_in_int_switch(ModuleInfo, SwitchVarType, CaseNum + 1,
        Cases, TaggedCases, !LowerLimit, !UpperLimit, !NumValues).

:- pred tag_cons_id_in_int_switch(module_info::in,
    cons_id::in, tagged_cons_id::out,
    T::in, T::out, T::in, T::out, uint::in, uint::out) is det
    <= int_tag_value(T).

tag_cons_id_in_int_switch(ModuleInfo, ConsId, TaggedConsId,
        !LowerLimit, !UpperLimit, !NumValues) :-
    ConsTag = cons_id_to_tag(ModuleInfo, ConsId),
    TaggedConsId = tagged_cons_id(ConsId, ConsTag),
    ( if cons_tag_is_int_tag(ConsTag, IntTag) then
        !:LowerLimit = int_tag_min(IntTag, !.LowerLimit),
        !:UpperLimit = int_tag_max(IntTag, !.UpperLimit),
        !:NumValues = !.NumValues + 1u
    else
        % We only get called if the MainConsId of the first TaggedCase
        % is an int_tag wrapping a value of type T. If this is true,
        % then *all* the cons_ids in *all* TaggedCases must have
        % the exact same value.
        unexpected($pred, "ConsId is not int_tag")
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
            ; IntType = int_type_int8
            ; IntType = int_type_int16
            ; IntType = int_type_int32
            ; IntType = int_type_uint
            ; IntType = int_type_uint8
            ; IntType = int_type_uint16
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
