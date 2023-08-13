%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2018 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: hlds_inst_mode.m.
%
% This module defines the part of the HLDS that deals with insts and modes.
%
%---------------------------------------------------------------------------%

:- module hlds.hlds_inst_mode.
:- interface.

:- import_module hlds.status.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module assoc_list.
:- import_module list.
:- import_module map.
:- import_module maybe.

%---------------------------------------------------------------------------%

    % An inst that is defined to be equivalent to a bound inst may be
    % declared by the programmer to be for a particular type constructor.
:- type inst_for_type_ctor
    --->    iftc_not_bound_inst
            % The inst is not defined to be equivalent to a bound inst.

    ;       iftc_applicable_declared(type_ctor)
            % The inst is defined to be equivalent to a bound inst,
            % and it is declared to be for this type constructor.
            % This requires that all the top level cons_ids in the bound inst
            % be function symbols of the given type constructor.
            % Until we invoke inst_check.m, this setting is only a claim;
            % after it has finished, if an inst's iftc field is still
            % set to this value, it will be a checked fact.
            %
            % Later, it will also require that this inst be applied
            % only to values of this type.

    ;       iftc_applicable_not_known
            % The inst is defined to be equivalent to a bound inst.
            % It is not declared to be for a specific type constructor,
            % and the list of type constructors that its cons_ids match
            % is not (yet) known.
            %
            % No inst should have this iftc field after inst_check.m's
            % code has been run.

    ;       iftc_applicable_known(list(type_ctor))
            % The inst is defined to be equivalent to a bound inst.
            % It is not declared to be for a specific type constructor,
            % but the list of type constructors that its cons_ids match
            % is known to be the given list of type constructors.

    ;       iftc_applicable_error_unknown_type
            % The inst is defined to be equivalent to a bound inst,
            % and it is declared to be for a specific type constructor,
            % but that type constructor does not exist.

    ;       iftc_applicable_error_eqv_type(type_ctor)
            % The inst is defined to be equivalent to a bound inst,
            % and it is declared to be for a specific type constructor,
            % but that type constructor is not a du type.
            % XXX We generate this iftc value only for equivalence types,
            % not for foreign types, solver types or (imported) abstract types.

    ;       iftc_applicable_error_visibility(type_ctor)
            % The inst is defined to be equivalent to a bound inst.
            % It is declared to be for a specific type constructor,
            % which is a discriminated union type, but there is a
            % visibility mismatch; the inst is exported, but the
            % type constructor is not.

    ;       iftc_applicable_error_mismatches(type_ctor).
            % The inst is defined to be equivalent to a bound inst.
            % It is declared to be for a specific type constructor,
            % which is a discriminated union type of the appropriate
            % visibility, but the inst does not match its function symbols.

    % An `hlds_inst_defn' holds the information we need to store
    % about inst definitions such as
    %   :- inst list_skel(I) = bound([] ; [I | list_skel(I)].
    %
:- type hlds_inst_defn
    --->    hlds_inst_defn(
                % The names of the inst parameters (if any).
                inst_varset             :: inst_varset,

                % The inst parameters (if any). ([I] in the above example.)
                inst_params             :: list(inst_var),

                % The definition of this inst.
                inst_body               :: inst_defn,

                % If this inst is equivalent to a bound inst, is it
                % specified to be applicable only to a specific
                % type constructor? If not, is it known to be compatible
                % with a known set of type constructors? (This means
                % having top level function symbols that all come from
                % the type constructor.)
                inst_for_type           :: inst_for_type_ctor,

                % The location in the source code of this inst definition.
                inst_context            :: prog_context,

                % So intermod.m can tell whether to output this inst.
                inst_status             :: inst_status
            ).

:- type user_inst_table == map(inst_ctor, hlds_inst_defn).

:- type maybe_inst
    --->    inst_unknown
    ;       inst_known(mer_inst).

:- type maybe_inst_det
    --->    inst_det_unknown
    ;       inst_det_known(mer_inst, determinism).

:- type unify_inst_table.
:- type merge_inst_table.
:- type ground_inst_table.
:- type any_inst_table.
:- type shared_inst_table.
:- type mostly_uniq_inst_table.

:- pred search_unify_inst(unify_inst_table::in,
    unify_inst_info::in, maybe_inst_det::out) is semidet.
:- pred search_merge_inst(merge_inst_table::in,
    merge_inst_info::in, maybe_inst::out) is semidet.
:- pred search_ground_inst(ground_inst_table::in,
    ground_inst_info::in, maybe_inst_det::out) is semidet.
:- pred search_any_inst(any_inst_table::in,
    any_inst_info::in, maybe_inst_det::out) is semidet.
:- pred search_shared_inst(shared_inst_table::in,
    inst_name::in, maybe_inst::out) is semidet.
:- pred search_mostly_uniq_inst(mostly_uniq_inst_table::in,
    inst_name::in, maybe_inst::out) is semidet.

:- pred lookup_unify_inst(unify_inst_table::in,
    unify_inst_info::in, maybe_inst_det::out) is det.
:- pred lookup_merge_inst(merge_inst_table::in,
    merge_inst_info::in, maybe_inst::out) is det.
:- pred lookup_ground_inst(ground_inst_table::in,
    ground_inst_info::in, maybe_inst_det::out) is det.
:- pred lookup_any_inst(any_inst_table::in,
    any_inst_info::in, maybe_inst_det::out) is det.
:- pred lookup_shared_inst(shared_inst_table::in,
    inst_name::in, maybe_inst::out) is det.
:- pred lookup_mostly_uniq_inst(mostly_uniq_inst_table::in,
    inst_name::in, maybe_inst::out) is det.

:- pred search_insert_unify_inst(
    unify_inst_info::in, maybe(maybe_inst_det)::out,
    unify_inst_table::in, unify_inst_table::out) is det.
:- pred search_insert_merge_inst(
    merge_inst_info::in, maybe(maybe_inst)::out,
    merge_inst_table::in, merge_inst_table::out) is det.
:- pred search_insert_ground_inst(
    ground_inst_info::in, maybe(maybe_inst_det)::out,
    ground_inst_table::in, ground_inst_table::out) is det.
:- pred search_insert_any_inst(
    any_inst_info::in, maybe(maybe_inst_det)::out,
    any_inst_table::in, any_inst_table::out) is det.
:- pred search_insert_shared_inst(
    inst_name::in, maybe(maybe_inst)::out,
    shared_inst_table::in, shared_inst_table::out) is det.
:- pred search_insert_mostly_uniq_inst(
    inst_name::in, maybe(maybe_inst)::out,
    mostly_uniq_inst_table::in, mostly_uniq_inst_table::out) is det.

:- pred det_update_unify_inst(unify_inst_info::in, maybe_inst_det::in,
    unify_inst_table::in, unify_inst_table::out) is det.
:- pred det_update_merge_inst(merge_inst_info::in, maybe_inst::in,
    merge_inst_table::in, merge_inst_table::out) is det.
:- pred det_update_ground_inst(ground_inst_info::in, maybe_inst_det::in,
    ground_inst_table::in, ground_inst_table::out) is det.
:- pred det_update_any_inst(any_inst_info::in, maybe_inst_det::in,
    any_inst_table::in, any_inst_table::out) is det.
:- pred det_update_shared_inst(inst_name::in, maybe_inst::in,
    shared_inst_table::in, shared_inst_table::out) is det.
:- pred det_update_mostly_uniq_inst(inst_name::in, maybe_inst::in,
    mostly_uniq_inst_table::in, mostly_uniq_inst_table::out) is det.

:- type inst_pair
    --->    inst_pair(mer_inst, mer_inst).

:- pred unify_insts_to_sorted_pairs(unify_inst_table::in,
    assoc_list(unify_inst_info, maybe_inst_det)::out) is det.
:- pred unify_insts_to_four_assoc_lists(unify_inst_table::in,
    assoc_list(inst_pair, maybe_inst_det)::out,
    assoc_list(inst_pair, maybe_inst_det)::out,
    assoc_list(inst_pair, maybe_inst_det)::out,
    assoc_list(inst_pair, maybe_inst_det)::out) is det.
:- pred merge_insts_to_sorted_pairs(merge_inst_table::in,
    assoc_list(merge_inst_info, maybe_inst)::out) is det.
:- pred ground_insts_to_sorted_pairs(ground_inst_table::in,
    assoc_list(ground_inst_info, maybe_inst_det)::out) is det.
:- pred any_insts_to_sorted_pairs(any_inst_table::in,
    assoc_list(any_inst_info, maybe_inst_det)::out) is det.
:- pred shared_insts_to_sorted_pairs(shared_inst_table::in,
    assoc_list(inst_name, maybe_inst)::out) is det.
:- pred mostly_uniq_insts_to_sorted_pairs(mostly_uniq_inst_table::in,
    assoc_list(inst_name, maybe_inst)::out) is det.

:- pred unify_insts_from_four_assoc_lists(
    assoc_list(inst_pair, maybe_inst_det)::in,
    assoc_list(inst_pair, maybe_inst_det)::in,
    assoc_list(inst_pair, maybe_inst_det)::in,
    assoc_list(inst_pair, maybe_inst_det)::in,
    unify_inst_table::out) is det.
:- pred merge_insts_from_sorted_pairs(
    assoc_list(merge_inst_info, maybe_inst)::in,
    merge_inst_table::out) is det.
:- pred ground_insts_from_sorted_pairs(
    assoc_list(ground_inst_info, maybe_inst_det)::in,
    ground_inst_table::out) is det.
:- pred any_insts_from_sorted_pairs(
    assoc_list(any_inst_info, maybe_inst_det)::in,
    any_inst_table::out) is det.
:- pred shared_insts_from_sorted_pairs(
    assoc_list(inst_name, maybe_inst)::in,
    shared_inst_table::out) is det.
:- pred mostly_uniq_insts_from_sorted_pairs(
    assoc_list(inst_name, maybe_inst)::in,
    mostly_uniq_inst_table::out) is det.

%---------------------------------------------------------------------------%

    % The symbol table for insts.
    %
:- type inst_table.

:- pred inst_table_init(inst_table::out) is det.

:- pred inst_table_get_user_insts(inst_table::in, user_inst_table::out) is det.
:- pred inst_table_get_unify_insts(inst_table::in, unify_inst_table::out)
    is det.
:- pred inst_table_get_merge_insts(inst_table::in, merge_inst_table::out)
    is det.
:- pred inst_table_get_ground_insts(inst_table::in, ground_inst_table::out)
    is det.
:- pred inst_table_get_any_insts(inst_table::in, any_inst_table::out) is det.
:- pred inst_table_get_shared_insts(inst_table::in, shared_inst_table::out)
    is det.
:- pred inst_table_get_mostly_uniq_insts(inst_table::in,
    mostly_uniq_inst_table::out) is det.

:- pred inst_table_set_user_insts(user_inst_table::in,
    inst_table::in, inst_table::out) is det.
:- pred inst_table_set_unify_insts(unify_inst_table::in,
    inst_table::in, inst_table::out) is det.
:- pred inst_table_set_merge_insts(merge_inst_table::in,
    inst_table::in, inst_table::out) is det.
:- pred inst_table_set_ground_insts(ground_inst_table::in,
    inst_table::in, inst_table::out) is det.
:- pred inst_table_set_any_insts(any_inst_table::in,
    inst_table::in, inst_table::out) is det.
:- pred inst_table_set_shared_insts(shared_inst_table::in,
    inst_table::in, inst_table::out) is det.
:- pred inst_table_set_mostly_uniq_insts(mostly_uniq_inst_table::in,
    inst_table::in, inst_table::out) is det.

%---------------------------------------------------------------------------%

    % The symbol table for modes.
    %
:- type mode_table.
:- type mode_defns == map(mode_ctor, hlds_mode_defn).

    % A hlds_mode_defn stores the information about a mode
    % definition such as
    %   :- mode out == free >> ground.
    % or
    %   :- mode in(I) == I >> I.
    % or
    %   :- mode in_list_skel == in(list_skel).
    %
:- type hlds_mode_defn
    --->    hlds_mode_defn(
                % The names of the inst parameters (if any).
                mode_varset     :: inst_varset,

                % The list of the inst parameters (if any).
                % (e.g. [I] for the second example above.)
                mode_params     :: list(inst_var),

                % The definition of this mode.
                mody_body       :: hlds_mode_body,

                % The location of this mode definition in the source code.
                mode_context    :: prog_context,

                % So intermod.m can tell whether to output this mode.
                mode_status     :: mode_status
            ).

    % The only sort of mode definitions allowed are equivalence modes:
    % the given mode name is defined as being equivalent to some other mode.
    %
:- type hlds_mode_body
    --->    hlds_mode_body(mer_mode).

    % Given a mode table get the mode_id - hlds_mode_defn map.
    %
:- pred mode_table_get_mode_defns(mode_table::in, mode_defns::out) is det.

    % Insert a mode_id and corresponding hlds_mode_defn into the mode_table.
    % Fail if the mode_id is already present in the table.
    %
:- pred mode_table_insert(mode_ctor::in, hlds_mode_defn::in,
    mode_table::in, mode_table::out) is semidet.

:- pred mode_table_init(mode_table::out) is det.

    % Optimize the mode table for lookups.
    %
:- pred mode_table_optimize(mode_table::in, mode_table::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module pair.
:- import_module varset.

%---------------------------------------------------------------------------%
%
% I (zs) have tried making the merge_inst_table a two-stage table,
% i.e. being map(mer_inst, map(mer_inst, maybe_inst)), in the hope
% of making lookups faster, but it led to a slowdown, not a speedup.
% The main reason was the extra cost of insertions. While you can always
% use a search_insert operation on the inner map, you can do it on
% the outer map only if the first inst does not occur in the outer map.
% If it does, then you are *modifying* an existing entry, not inserting
% a new one, and you can't modify it without knowing what it is. Therefore
% in the common case, a search_insert on the whole merge_inst_table
% requires first a search_insert on the outer table, and when the search
% part of that succeeds, a search_insert on the inner table and then
% a straight *update* on the outer map. The main performance problem is
% the need for this update.
%
% I expect (though I have not tested it) that the same problem would arise
% if we turned the subtables of the unify_inst_table into two-stage maps.
%

:- type unify_inst_table
    --->    unify_inst_table(
                uit_live_real   ::  map(inst_pair, maybe_inst_det),
                uit_live_fake   ::  map(inst_pair, maybe_inst_det),
                uit_dead_real   ::  map(inst_pair, maybe_inst_det),
                uit_dead_fake   ::  map(inst_pair, maybe_inst_det)
            ).

:- type merge_inst_table ==         map(merge_inst_info, maybe_inst).
:- type ground_inst_table ==        map(ground_inst_info, maybe_inst_det).
:- type any_inst_table ==           map(any_inst_info, maybe_inst_det).
:- type shared_inst_table ==        map(inst_name, maybe_inst).
:- type mostly_uniq_inst_table ==   map(inst_name, maybe_inst).

%---------------------------------------------------------------------------%

search_unify_inst(UnifyInstTable, UnifyInstInfo, MaybeInstDet) :-
    UnifyInstInfo = unify_inst_info(IsLive, IsReal, InstA, InstB),
    InstPair = inst_pair(InstA, InstB),
    (
        IsLive = is_live, IsReal = real_unify,
        LiveRealTable = UnifyInstTable ^ uit_live_real,
        map.search(LiveRealTable, InstPair, MaybeInstDet)
    ;
        IsLive = is_live, IsReal = fake_unify,
        LiveFakeTable = UnifyInstTable ^ uit_live_fake,
        map.search(LiveFakeTable, InstPair, MaybeInstDet)
    ;
        IsLive = is_dead, IsReal = real_unify,
        DeadRealTable = UnifyInstTable ^ uit_dead_real,
        map.search(DeadRealTable, InstPair, MaybeInstDet)
    ;
        IsLive = is_dead, IsReal = fake_unify,
        DeadFakeTable = UnifyInstTable ^ uit_dead_fake,
        map.search(DeadFakeTable, InstPair, MaybeInstDet)
    ).

search_merge_inst(MergeInstTable, MergeInstInfo, MaybeInst) :-
    map.search(MergeInstTable, MergeInstInfo, MaybeInst).

search_ground_inst(GroundInstTable, GroundInstInfo, MaybeInstDet) :-
    map.search(GroundInstTable, GroundInstInfo, MaybeInstDet).

search_any_inst(AnyInstTable, AnyInstInfo, MaybeInstDet) :-
    map.search(AnyInstTable, AnyInstInfo, MaybeInstDet).

search_shared_inst(SharedInstTable, InstName, MaybeInst) :-
    map.search(SharedInstTable, InstName, MaybeInst).

search_mostly_uniq_inst(MostlyUniqInstTable, InstName, MaybeInst) :-
    map.search(MostlyUniqInstTable, InstName, MaybeInst).

%---------------------------------------------------------------------------%

lookup_unify_inst(UnifyInstTable, UnifyInstInfo, MaybeInstDet) :-
    UnifyInstInfo = unify_inst_info(IsLive, IsReal, InstA, InstB),
    InstPair = inst_pair(InstA, InstB),
    (
        IsLive = is_live, IsReal = real_unify,
        LiveRealTable = UnifyInstTable ^ uit_live_real,
        map.lookup(LiveRealTable, InstPair, MaybeInstDet)
    ;
        IsLive = is_live, IsReal = fake_unify,
        LiveFakeTable = UnifyInstTable ^ uit_live_fake,
        map.lookup(LiveFakeTable, InstPair, MaybeInstDet)
    ;
        IsLive = is_dead, IsReal = real_unify,
        DeadRealTable = UnifyInstTable ^ uit_dead_real,
        map.lookup(DeadRealTable, InstPair, MaybeInstDet)
    ;
        IsLive = is_dead, IsReal = fake_unify,
        DeadFakeTable = UnifyInstTable ^ uit_dead_fake,
        map.lookup(DeadFakeTable, InstPair, MaybeInstDet)
    ).

lookup_merge_inst(MergeInstTable, MergeInstInfo, MaybeInst) :-
    map.lookup(MergeInstTable, MergeInstInfo, MaybeInst).

lookup_ground_inst(GroundInstTable, GroundInstInfo, MaybeInstDet) :-
    map.lookup(GroundInstTable, GroundInstInfo, MaybeInstDet).

lookup_any_inst(AnyInstTable, AnyInstInfo, MaybeInstDet) :-
    map.lookup(AnyInstTable, AnyInstInfo, MaybeInstDet).

lookup_shared_inst(SharedInstTable, InstName, MaybeInst) :-
    map.lookup(SharedInstTable, InstName, MaybeInst).

lookup_mostly_uniq_inst(MostlyUniqInstTable, InstName, MaybeInst) :-
    map.lookup(MostlyUniqInstTable, InstName, MaybeInst).

%---------------------------------------------------------------------------%

search_insert_unify_inst(UnifyInstInfo, MaybeMaybeInstDet, !UnifyInstTable) :-
    UnifyInstInfo = unify_inst_info(IsLive, IsReal, InstA, InstB),
    InstPair = inst_pair(InstA, InstB),
    (
        IsLive = is_live, IsReal = real_unify,
        LiveRealTable0 = !.UnifyInstTable ^ uit_live_real,
        map.search_insert(InstPair, inst_det_unknown, MaybeMaybeInstDet,
            LiveRealTable0, LiveRealTable),
        !UnifyInstTable ^ uit_live_real := LiveRealTable
    ;
        IsLive = is_live, IsReal = fake_unify,
        LiveFakeTable0 = !.UnifyInstTable ^ uit_live_fake,
        map.search_insert(InstPair, inst_det_unknown, MaybeMaybeInstDet,
            LiveFakeTable0, LiveFakeTable),
        !UnifyInstTable ^ uit_live_fake := LiveFakeTable
    ;
        IsLive = is_dead, IsReal = real_unify,
        DeadRealTable0 = !.UnifyInstTable ^ uit_dead_real,
        map.search_insert(InstPair, inst_det_unknown, MaybeMaybeInstDet,
            DeadRealTable0, DeadRealTable),
        !UnifyInstTable ^ uit_dead_real := DeadRealTable
    ;
        IsLive = is_dead, IsReal = fake_unify,
        DeadFakeTable0 = !.UnifyInstTable ^ uit_dead_fake,
        map.search_insert(InstPair, inst_det_unknown, MaybeMaybeInstDet,
            DeadFakeTable0, DeadFakeTable),
        !UnifyInstTable ^ uit_dead_fake := DeadFakeTable
    ).

search_insert_merge_inst(MergeInstInfo, MaybeMaybeInst, !MergeInstTable) :-
    map.search_insert(MergeInstInfo, inst_unknown, MaybeMaybeInst,
        !MergeInstTable).

search_insert_ground_inst(GroundInstInfo, MaybeMaybeInstDet,
        !GroundInstTable) :-
    map.search_insert(GroundInstInfo, inst_det_unknown, MaybeMaybeInstDet,
        !GroundInstTable).

search_insert_any_inst(AnyInstInfo, MaybeMaybeInstDet, !AnyInstTable) :-
    map.search_insert(AnyInstInfo, inst_det_unknown, MaybeMaybeInstDet,
        !AnyInstTable).

search_insert_shared_inst(InstName, MaybeMaybeInst, !SharedInstTable) :-
    map.search_insert(InstName, inst_unknown, MaybeMaybeInst,
        !SharedInstTable).

search_insert_mostly_uniq_inst(InstName, MaybeMaybeInst,
        !MostlyUniqInstTable) :-
    map.search_insert(InstName, inst_unknown, MaybeMaybeInst,
        !MostlyUniqInstTable).

%---------------------------------------------------------------------------%

det_update_unify_inst(UnifyInstInfo, MaybeInstDet, !UnifyInstTable) :-
    UnifyInstInfo = unify_inst_info(IsLive, IsReal, InstA, InstB),
    InstPair = inst_pair(InstA, InstB),
    (
        IsLive = is_live, IsReal = real_unify,
        LiveRealTable0 = !.UnifyInstTable ^ uit_live_real,
        map.det_update(InstPair, MaybeInstDet, LiveRealTable0, LiveRealTable),
        !UnifyInstTable ^ uit_live_real := LiveRealTable
    ;
        IsLive = is_live, IsReal = fake_unify,
        LiveFakeTable0 = !.UnifyInstTable ^ uit_live_fake,
        map.det_update(InstPair, MaybeInstDet, LiveFakeTable0, LiveFakeTable),
        !UnifyInstTable ^ uit_live_fake := LiveFakeTable
    ;
        IsLive = is_dead, IsReal = real_unify,
        DeadRealTable0 = !.UnifyInstTable ^ uit_dead_real,
        map.det_update(InstPair, MaybeInstDet, DeadRealTable0, DeadRealTable),
        !UnifyInstTable ^ uit_dead_real := DeadRealTable
    ;
        IsLive = is_dead, IsReal = fake_unify,
        DeadFakeTable0 = !.UnifyInstTable ^ uit_dead_fake,
        map.det_update(InstPair, MaybeInstDet, DeadFakeTable0, DeadFakeTable),
        !UnifyInstTable ^ uit_dead_fake := DeadFakeTable
    ).

det_update_merge_inst(MergeInstInfo, MaybeInst, !MergeInstTable) :-
    map.det_update(MergeInstInfo, MaybeInst, !MergeInstTable).

det_update_ground_inst(GroundInstInfo, MaybeInstDet, !GroundInstTable) :-
    map.det_update(GroundInstInfo, MaybeInstDet, !GroundInstTable).

det_update_any_inst(AnyInstInfo, MaybeInstDet, !AnyInstTable) :-
    map.det_update(AnyInstInfo, MaybeInstDet, !AnyInstTable).

det_update_shared_inst(InstName, MaybeInst, !SharedInstTable) :-
    map.det_update(InstName, MaybeInst, !SharedInstTable).

det_update_mostly_uniq_inst(InstName, MaybeInst, !MostlyUniqInstTable) :-
    map.det_update(InstName, MaybeInst, !MostlyUniqInstTable).

%---------------------------------------------------------------------------%

unify_insts_to_sorted_pairs(UnifyInstTable, AssocList) :-
    UnifyInstTable = unify_inst_table(LiveRealTable, LiveFakeTable,
        DeadRealTable, DeadFakeTable),
    map.to_assoc_list(LiveRealTable, LiveRealPairInsts),
    map.to_assoc_list(LiveFakeTable, LiveFakePairInsts),
    map.to_assoc_list(DeadRealTable, DeadRealPairInsts),
    map.to_assoc_list(DeadFakeTable, DeadFakePairInsts),
    some [!RevAssocList] (
        !:RevAssocList = [],
        accumulate_unify_insts(is_live, real_unify, LiveRealPairInsts,
            !RevAssocList),
        accumulate_unify_insts(is_live, fake_unify, LiveFakePairInsts,
            !RevAssocList),
        accumulate_unify_insts(is_dead, real_unify, DeadRealPairInsts,
            !RevAssocList),
        accumulate_unify_insts(is_dead, fake_unify, DeadFakePairInsts,
            !RevAssocList),
        list.reverse(!.RevAssocList, AssocList)
    ).

:- pred accumulate_unify_insts(is_live::in, unify_is_real::in,
    assoc_list(inst_pair, maybe_inst_det)::in,
    assoc_list(unify_inst_info, maybe_inst_det)::in,
    assoc_list(unify_inst_info, maybe_inst_det)::out) is det.

accumulate_unify_insts(_IsLive, _IsReal, [], !RevAssocList).
accumulate_unify_insts(IsLive, IsReal, [PairMaybeInst | PairMaybeInsts],
        !RevAssocList) :-
    PairMaybeInst = inst_pair(InstA, InstB) - MaybeInst,
    UnifyInstInfo = unify_inst_info(IsLive, IsReal, InstA, InstB),
    !:RevAssocList = [UnifyInstInfo - MaybeInst | !.RevAssocList],
    accumulate_unify_insts(IsLive, IsReal, PairMaybeInsts, !RevAssocList).

%---------------------%

unify_insts_to_four_assoc_lists(UnifyInstTable,
        LiveRealAL, LiveFakeAL, DeadRealAL, DeadFakeAL) :-
    UnifyInstTable = unify_inst_table(LiveRealTable, LiveFakeTable,
        DeadRealTable, DeadFakeTable),
    map.to_sorted_assoc_list(LiveRealTable, LiveRealAL),
    map.to_sorted_assoc_list(LiveFakeTable, LiveFakeAL),
    map.to_sorted_assoc_list(DeadRealTable, DeadRealAL),
    map.to_sorted_assoc_list(DeadFakeTable, DeadFakeAL).

%---------------------%

merge_insts_to_sorted_pairs(MergeInstTable, AssocList) :-
    map.to_sorted_assoc_list(MergeInstTable, AssocList).

ground_insts_to_sorted_pairs(GroundInstTable, AssocList) :-
    map.to_sorted_assoc_list(GroundInstTable, AssocList).

any_insts_to_sorted_pairs(AnyInstTable, AssocList) :-
    map.to_sorted_assoc_list(AnyInstTable, AssocList).

shared_insts_to_sorted_pairs(SharedInstTable, AssocList) :-
    map.to_sorted_assoc_list(SharedInstTable, AssocList).

mostly_uniq_insts_to_sorted_pairs(MostlyUniqInstTable, AssocList) :-
    map.to_sorted_assoc_list(MostlyUniqInstTable, AssocList).

%---------------------------------------------------------------------------%

unify_insts_from_four_assoc_lists(LiveRealAL, LiveFakeAL,
        DeadRealAL, DeadFakeAL, UnifyInstTable) :-
    map.from_sorted_assoc_list(LiveRealAL, LiveRealTable),
    map.from_sorted_assoc_list(LiveFakeAL, LiveFakeTable),
    map.from_sorted_assoc_list(DeadRealAL, DeadRealTable),
    map.from_sorted_assoc_list(DeadFakeAL, DeadFakeTable),
    UnifyInstTable = unify_inst_table(LiveRealTable, LiveFakeTable,
        DeadRealTable, DeadFakeTable).

%---------------------%

merge_insts_from_sorted_pairs(AssocList, MergeInstTable) :-
    map.from_sorted_assoc_list(AssocList, MergeInstTable).

ground_insts_from_sorted_pairs(AssocList, GroundInstTable) :-
    map.from_sorted_assoc_list(AssocList, GroundInstTable).

any_insts_from_sorted_pairs(AssocList, AnyInstTable) :-
    map.from_sorted_assoc_list(AssocList, AnyInstTable).

shared_insts_from_sorted_pairs(AssocList, SharedInstTable) :-
    map.from_sorted_assoc_list(AssocList, SharedInstTable).

mostly_uniq_insts_from_sorted_pairs(AssocList, MostlyUniqInstTable) :-
    map.from_sorted_assoc_list(AssocList, MostlyUniqInstTable).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- type inst_table
    --->    inst_table(
                inst_table_user         :: user_inst_table,
                inst_table_unify        :: unify_inst_table,
                inst_table_merge        :: merge_inst_table,
                inst_table_ground       :: ground_inst_table,
                inst_table_any          :: any_inst_table,
                inst_table_shared       :: shared_inst_table,
                inst_table_mostly_uniq  :: mostly_uniq_inst_table
            ).

inst_table_init(InstTable) :-
    map.init(UserInsts),
    UnifyInsts = unify_inst_table(map.init, map.init, map.init, map.init),
    map.init(MergeInsts),
    map.init(GroundInsts),
    map.init(SharedInsts),
    map.init(AnyInsts),
    map.init(NondetLiveInsts),
    InstTable = inst_table(UserInsts, UnifyInsts, MergeInsts, GroundInsts,
        AnyInsts, SharedInsts, NondetLiveInsts).

inst_table_get_user_insts(InstTable, X) :-
    X = InstTable ^ inst_table_user.
inst_table_get_unify_insts(InstTable, X) :-
    X = InstTable ^ inst_table_unify.
inst_table_get_merge_insts(InstTable, X) :-
    X = InstTable ^ inst_table_merge.
inst_table_get_ground_insts(InstTable, X) :-
    X = InstTable ^ inst_table_ground.
inst_table_get_any_insts(InstTable, X) :-
    X = InstTable ^ inst_table_any.
inst_table_get_shared_insts(InstTable, X) :-
    X = InstTable ^ inst_table_shared.
inst_table_get_mostly_uniq_insts(InstTable, X) :-
    X = InstTable ^ inst_table_mostly_uniq.

inst_table_set_user_insts(X, !InstTable) :-
    !InstTable ^ inst_table_user := X.
inst_table_set_unify_insts(X, !InstTable) :-
    !InstTable ^ inst_table_unify := X.
inst_table_set_merge_insts(X, !InstTable) :-
    !InstTable ^ inst_table_merge := X.
inst_table_set_ground_insts(X, !InstTable) :-
    !InstTable ^ inst_table_ground := X.
inst_table_set_any_insts(X, !InstTable) :-
    !InstTable ^ inst_table_any := X.
inst_table_set_shared_insts(X, !InstTable) :-
    !InstTable ^ inst_table_shared := X.
inst_table_set_mostly_uniq_insts(X, !InstTable) :-
    !InstTable ^ inst_table_mostly_uniq := X.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- type mode_table == mode_defns.

mode_table_get_mode_defns(ModeDefns, ModeDefns).

mode_table_insert(ModeId, ModeDefn, !ModeDefns) :-
    map.insert(ModeId, ModeDefn, !ModeDefns).

mode_table_init(map.init).

mode_table_optimize(!ModeDefns) :-
    map.optimize(!ModeDefns).

%---------------------------------------------------------------------------%
:- end_module hlds.hlds_inst_mode.
%---------------------------------------------------------------------------%
