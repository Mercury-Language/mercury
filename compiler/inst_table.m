%-----------------------------------------------------------------------------%
% Copyright (C) 1999 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% This module contains the definitition of the inst_table which holds
% procedure-wide inst information.  This includes compiler-defined recursive
% insts and the inst_key_table which holds alias information.

% Main authors: bromage, dmo.

:- module inst_table.

:- interface.

:- import_module prog_data, (inst).

:- import_module map, set_bbbtree.

:- type inst_table.

:- type unify_inst_table ==	map(inst_name, maybe_inst_det).

:- type unify_inst_pair	--->	unify_inst_pair(is_live, inst, inst,
					unify_is_real).

:- type merge_inst_table ==	map(merge_inst_pair, maybe_inst).

:- type merge_inst_pair --->	merge_inst_pair(is_live, inst, inst).

:- type ground_inst_table == 	map(inst_name, maybe_inst_det).

:- type any_inst_table == 	map(inst_name, maybe_inst_det).

:- type shared_inst_table == 	map(inst_name, maybe_inst).

:- type mostly_uniq_inst_table == map(inst_name, maybe_inst).

:- type clobbered_inst_table == map(inst_name, maybe_inst).

	% The other_inst_table can be used when transforming a recursive inst
	% into a new inst.  You should call `other_inst_table_new_id' at the
	% start of each new use of the other_inst_table.
	%
	% XXX It would be possible to use the other_inst_table instead
	%     of the shared_inst_table, mostly_uniq_inst_table
	%     and clobbered_inst_table.
	%     Maybe we should do this to reduce the overall complexity
	%     of the inst_table.
:- type other_inst_table.

:- type inst_key_set == set_bbbtree(inst_key).

:- type maybe_inst	--->	unknown
			;	known(inst).

:- type maybe_inst_det	--->	unknown
			;	known(inst, determinism).

%-----------------------------------------------------------------------------%

:- pred inst_table_init(inst_table).
:- mode inst_table_init(out) is det.

:- pred inst_table_get_all_tables(inst_table, unify_inst_table,
	merge_inst_table, ground_inst_table, any_inst_table, shared_inst_table,
	mostly_uniq_inst_table, clobbered_inst_table, other_inst_table,
	inst_key_table).
:- mode inst_table_get_all_tables(in, out, out, out, out, out, out, out, out,
	out) is det.

:- pred inst_table_set_all_tables(inst_table, unify_inst_table,
	merge_inst_table, ground_inst_table, any_inst_table, shared_inst_table,
	mostly_uniq_inst_table, clobbered_inst_table, other_inst_table,
	inst_key_table, inst_table).
:- mode inst_table_set_all_tables(in, in, in, in, in, in, in, in, in, in, out)
	is det.

:- pred inst_table_get_unify_insts(inst_table, unify_inst_table).
:- mode inst_table_get_unify_insts(in, out) is det.

:- pred inst_table_get_merge_insts(inst_table, merge_inst_table).
:- mode inst_table_get_merge_insts(in, out) is det.

:- pred inst_table_get_ground_insts(inst_table, ground_inst_table).
:- mode inst_table_get_ground_insts(in, out) is det.

:- pred inst_table_get_any_insts(inst_table, any_inst_table).
:- mode inst_table_get_any_insts(in, out) is det.

:- pred inst_table_get_shared_insts(inst_table, shared_inst_table).
:- mode inst_table_get_shared_insts(in, out) is det.

:- pred inst_table_get_mostly_uniq_insts(inst_table, mostly_uniq_inst_table).
:- mode inst_table_get_mostly_uniq_insts(in, out) is det.

:- pred inst_table_get_clobbered_insts(inst_table, clobbered_inst_table).
:- mode inst_table_get_clobbered_insts(in, out) is det.

:- pred inst_table_get_other_insts(inst_table, other_inst_table).
:- mode inst_table_get_other_insts(in, out) is det.

:- pred inst_table_get_inst_key_table(inst_table, inst_key_table).
:- mode inst_table_get_inst_key_table(in, out) is det.

:- pred inst_table_set_unify_insts(inst_table, unify_inst_table, inst_table).
:- mode inst_table_set_unify_insts(in, in, out) is det.

:- pred inst_table_set_merge_insts(inst_table, merge_inst_table, inst_table).
:- mode inst_table_set_merge_insts(in, in, out) is det.

:- pred inst_table_set_ground_insts(inst_table, ground_inst_table, inst_table).
:- mode inst_table_set_ground_insts(in, in, out) is det.

:- pred inst_table_set_any_insts(inst_table, any_inst_table, inst_table).
:- mode inst_table_set_any_insts(in, in, out) is det.

:- pred inst_table_set_shared_insts(inst_table, shared_inst_table, inst_table).
:- mode inst_table_set_shared_insts(in, in, out) is det.

:- pred inst_table_set_mostly_uniq_insts(inst_table, mostly_uniq_inst_table,
					inst_table).
:- mode inst_table_set_mostly_uniq_insts(in, in, out) is det.

:- pred inst_table_set_clobbered_insts(inst_table, clobbered_inst_table,
					inst_table).
:- mode inst_table_set_clobbered_insts(in, in, out) is det.

:- pred inst_table_set_other_insts(inst_table, other_inst_table, inst_table).
:- mode inst_table_set_other_insts(in, in, out) is det.

:- pred inst_table_set_inst_key_table(inst_table, inst_key_table, inst_table).
:- mode inst_table_set_inst_key_table(in, in, out) is det.

%-----------------------------------------------------------------------------%

	% Operations on the other_inst_table.

	% other_inst_table_new_id should be called when starting a new 
	% transformation on a set of insts. It increments an internal
	% flag (of type other_inst_id) which is used to distinguish 
	% insts created by this transformation from those created by 
	% previous transformation.
:- pred other_inst_table_new_id(other_inst_table, other_inst_table).
:- mode other_inst_table_new_id(in, out) is det.

	% Place the inst_name in the other_inst_table, marked with the
	% current other_inst_id.
:- pred other_inst_table_set(other_inst_table, inst_name, maybe_inst,
		other_inst_table).
:- mode other_inst_table_set(in, in, in, out) is det.

	% Search for an entry in the other_inst_table containing the
	% specified inst_name marked witht he current other_inst_id.
:- pred other_inst_table_search(other_inst_table, inst_name, maybe_inst).
:- mode other_inst_table_search(in, in, out) is semidet.

:- pred other_inst_table_lookup(other_inst_table, other_inst_id, inst_name,
		maybe_inst).
:- mode other_inst_table_lookup(in, in, in, out) is det.

	% Return an inst_name for the new derived_inst by marking the input
	% inst_name with the current other_inst_id.
:- pred other_inst_table_mark_inst_name(other_inst_table, inst_name,
		inst_name).
:- mode other_inst_table_mark_inst_name(in, in, out) is det.

%-----------------------------------------------------------------------------%

:- type inst_table_sub.

:- pred inst_table_create_sub(inst_table, inst_table, inst_table_sub,
		inst_table).
:- mode inst_table_create_sub(in, in, out, out) is det.

:- pred inst_apply_inst_table_sub(inst_table_sub, inst, inst).
:- mode inst_apply_inst_table_sub(in, in, out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module list, std_util.

:- type inst_table
	--->	inst_table(
			unit,
			unify_inst_table,
			merge_inst_table,
			ground_inst_table,
			any_inst_table,
			shared_inst_table,
			mostly_uniq_inst_table,
			clobbered_inst_table,
			other_inst_table,
			inst_key_table
		).

:- type other_inst_table
	--->	other_inst_table(
			other_inst_id,		% the next other_inst_id to use.
			map(other_inst, maybe_inst)
		).

:- type other_inst
	--->	other_inst(
			other_inst_id,
			inst_name
		).

inst_table_init(inst_table(unit, UnifyInsts, MergeInsts, GroundInsts,
		AnyInsts, SharedInsts, NondetLiveInsts, ClobberedInsts,
		other_inst_table(init_other_inst_id, OtherInsts), InstKeys)) :-
	map__init(UnifyInsts),
	map__init(MergeInsts),
	map__init(GroundInsts),
	map__init(AnyInsts),
	map__init(SharedInsts),
	map__init(NondetLiveInsts),
	map__init(ClobberedInsts),
	map__init(OtherInsts),
	inst_key_table_init(InstKeys).

inst_table_get_all_tables(InstTable, UnifyInsts, MergeInsts,
		GroundInsts, AnyInsts, SharedInsts, NondetLiveInsts,
		ClobberedInsts, OtherInsts, InstKeys) :-
	InstTable = inst_table(_, UnifyInsts, MergeInsts, GroundInsts,
		AnyInsts, SharedInsts, NondetLiveInsts, ClobberedInsts,
		OtherInsts, InstKeys).

inst_table_set_all_tables(_InstTable0, UnifyInsts, MergeInsts,
		GroundInsts, AnyInsts, SharedInsts, NondetLiveInsts,
		ClobberedInsts, OtherInstTable, InstKeys, InstTable) :-
	InstTable  = inst_table(unit, UnifyInsts, MergeInsts, GroundInsts,
		AnyInsts, SharedInsts, NondetLiveInsts, ClobberedInsts,
		OtherInstTable, InstKeys).

inst_table_get_unify_insts(inst_table(_, UnifyInsts, _, _, _, _, _, _, _, _),
			UnifyInsts).

inst_table_get_merge_insts(inst_table(_, _, MergeInsts, _, _, _, _, _, _, _),
			MergeInsts).

inst_table_get_ground_insts(inst_table(_, _, _, GroundInsts, _, _, _, _, _, _),
			GroundInsts).

inst_table_get_any_insts(inst_table(_, _, _, _, AnyInsts, _, _, _, _, _),
			AnyInsts).

inst_table_get_shared_insts(inst_table(_, _, _, _, _, SharedInsts, _, _, _, _),
			SharedInsts).

inst_table_get_mostly_uniq_insts(
			inst_table(_, _, _, _, _, _, NondetLiveInsts, _, _, _),
			NondetLiveInsts).

inst_table_get_clobbered_insts(
			inst_table(_, _, _, _, _, _, _, ClobberedInsts, _, _),
			ClobberedInsts).

inst_table_get_other_insts(inst_table(_, _, _, _, _, _, _, _, OtherInsts, _),
			OtherInsts).

inst_table_get_inst_key_table(
			inst_table(_, _, _, _, _, _, _, _, _, InstKeyTable),
			InstKeyTable).

inst_table_set_unify_insts(inst_table(A, _, C, D, E, F, G, H, I, J),
			UnifyInsts,
			inst_table(A, UnifyInsts, C, D, E, F, G, H, I, J)).

inst_table_set_merge_insts(inst_table(A, B, _, D, E, F, G, H, I, J),
			MergeInsts,
			inst_table(A, B, MergeInsts, D, E, F, G, H, I, J)).

inst_table_set_ground_insts(inst_table(A, B, C, _, E, F, G, H, I, J),
			GroundInsts,
			inst_table(A, B, C, GroundInsts, E, F, G, H, I, J)).

inst_table_set_any_insts(inst_table(A, B, C, D, _, F, G, H, I, J), AnyInsts,
			inst_table(A, B, C, D, AnyInsts, F, G, H, I, J)).

inst_table_set_shared_insts(inst_table(A, B, C, D, E, _, G, H, I, J),
			SharedInsts,
			inst_table(A, B, C, D, E, SharedInsts, G, H, I, J)).

inst_table_set_mostly_uniq_insts(inst_table(A, B, C, D, E, F, _, H, I, J),
			NondetLiveInsts,
			inst_table(A, B, C, D, E, F, NondetLiveInsts, H, I, J)).

inst_table_set_clobbered_insts(inst_table(A, B, C, D, E, F, G, _, I, J),
			ClobberedInsts,
			inst_table(A, B, C, D, E, F, G, ClobberedInsts, I, J)).

inst_table_set_other_insts(inst_table(A, B, C, D, E, F, G, H, _, J),
			OtherInsts,
			inst_table(A, B, C, D, E, F, G, H, OtherInsts, J)).

inst_table_set_inst_key_table(inst_table(A, B, C, D, E, F, G, H, I, _),
			InstKeys,
			inst_table(A, B, C, D, E, F, G, H, I, InstKeys)).

%-----------------------------------------------------------------------------%

other_inst_table_new_id(other_inst_table(Id, Map),
		other_inst_table(inc_other_inst_id(Id), Map)).

other_inst_table_set(other_inst_table(Id, Map0), InstName, MaybeInst,
		other_inst_table(Id, Map)) :-
	map__set(Map0, other_inst(Id, InstName), MaybeInst, Map).

other_inst_table_search(other_inst_table(Id, Map), InstName, MaybeInst) :-
	map__search(Map, other_inst(Id, InstName), MaybeInst).

other_inst_table_lookup(other_inst_table(_, Map), OtherInstId, InstName,
		MaybeInst) :-
	map__lookup(Map, other_inst(OtherInstId, InstName), MaybeInst).

other_inst_table_mark_inst_name(other_inst_table(Id, _), InstName,
		other_inst(Id, InstName)).

%-----------------------------------------------------------------------------%

:- type inst_table_sub
	--->	inst_table_sub(
			inst_key_sub,
			other_inst_id_sub
		).

inst_table_create_sub(InstTable0, NewInstTable, Sub, InstTable) :-
	inst_table_get_all_tables(InstTable0, UnifyInstTable0, MergeInstTable0,
		GroundInstTable0, AnyInstTable0, SharedInstTable0,
		ClobberedInstTable0, MostlyUniqInstTable0, OtherInstTable0,
		IKT0),
	inst_table_get_all_tables(NewInstTable, NewUnifyInstTable,
		NewMergeInstTable, NewGroundInstTable, NewAnyInstTable,
		NewSharedInstTable, NewMostlyUniqInstTable,
		NewClobberedInstTable, NewOtherInstTable, NewIKT),
	inst_key_table_create_sub(IKT0, NewIKT, IKSub, IKT),
	OtherInstTable0 = other_inst_table(Id0, OtherInsts0),
	OtherInstIdSub = other_inst_id_create_sub(Id0),
	Sub = inst_table_sub(IKSub, OtherInstIdSub),

	map_apply_sub(Sub, UnifyInstTable0, NewUnifyInstTable, UnifyInstTable),
	map_apply_sub(Sub, MergeInstTable0, NewMergeInstTable, MergeInstTable),
	map_apply_sub(Sub, GroundInstTable0, NewGroundInstTable,
		GroundInstTable),
	map_apply_sub(Sub, AnyInstTable0, NewAnyInstTable, AnyInstTable),
	map_apply_sub(Sub, SharedInstTable0, NewSharedInstTable,
		SharedInstTable),
	map_apply_sub(Sub, MostlyUniqInstTable0, NewMostlyUniqInstTable,
		MostlyUniqInstTable),
	map_apply_sub(Sub, ClobberedInstTable0, NewClobberedInstTable,
		ClobberedInstTable),

	NewOtherInstTable = other_inst_table(NewId, NewOtherInsts),
	map_apply_sub(Sub, OtherInsts0, NewOtherInsts, OtherInsts),
	OtherInstId = other_inst_id_apply_sub(OtherInstIdSub, NewId),
	OtherInstTable = other_inst_table(OtherInstId, OtherInsts),

	inst_table_set_all_tables(InstTable0, UnifyInstTable,
		MergeInstTable, GroundInstTable, AnyInstTable,
		SharedInstTable, MostlyUniqInstTable, ClobberedInstTable,
		OtherInstTable, IKT, InstTable).

:- pred map_apply_sub(inst_table_sub, map(K, V), map(K, V), map(K, V)) 
		<= (inst_table_entry(K), inst_table_entry(V)).
:- mode map_apply_sub(in, in, in, out) is det.

map_apply_sub(Sub, Map0, NewMap, Map) :-
	map__foldl((pred(K0::in, V0::in, M0::in, M::out) is det :-
			inst_table_entry_apply_sub(Sub, K0, K),
			inst_table_entry_apply_sub(Sub, V0, V),
			map__set(M0, K, V, M)),
		NewMap, Map0, Map).

:- typeclass inst_table_entry(T) where [
	pred inst_table_entry_apply_sub(inst_table_sub, T, T),
	mode inst_table_entry_apply_sub(in, in, out) is det
].

:- instance inst_table_entry(inst_name) where [
	pred(inst_table_entry_apply_sub/3) is inst_name_apply_sub
].

:- instance inst_table_entry(maybe_inst) where [
	pred(inst_table_entry_apply_sub/3) is maybe_inst_apply_sub
].

:- instance inst_table_entry(maybe_inst_det) where [
	pred(inst_table_entry_apply_sub/3) is maybe_inst_det_apply_sub
].

:- instance inst_table_entry(merge_inst_pair) where [
	pred(inst_table_entry_apply_sub/3) is merge_inst_pair_apply_sub
].

:- instance inst_table_entry(other_inst) where [
	pred(inst_table_entry_apply_sub/3) is other_inst_apply_sub
].

:- pred inst_name_apply_sub(inst_table_sub, inst_name, inst_name).
:- mode inst_name_apply_sub(in, in, out) is det.

inst_name_apply_sub(Sub, user_inst(Sym, Insts0), user_inst(Sym, Insts)) :-
	list__map(inst_apply_inst_table_sub(Sub), Insts0, Insts).
inst_name_apply_sub(Sub, merge_inst(IsLive, InstA0, InstB0),
		merge_inst(IsLive, InstA, InstB)) :-
	inst_apply_inst_table_sub(Sub, InstA0, InstA),
	inst_apply_inst_table_sub(Sub, InstB0, InstB).
inst_name_apply_sub(Sub, unify_inst(IsLive, InstA0, InstB0, IsReal),
		unify_inst(IsLive, InstA, InstB, IsReal)) :-
	inst_apply_inst_table_sub(Sub, InstA0, InstA),
	inst_apply_inst_table_sub(Sub, InstB0, InstB).
inst_name_apply_sub(Sub, ground_inst(Name0, IsLive, Uniq, IsReal),
		ground_inst(Name, IsLive, Uniq, IsReal)) :-
	inst_name_apply_sub(Sub, Name0, Name).
inst_name_apply_sub(Sub, any_inst(Name0, IsLive, Uniq, IsReal),
		any_inst(Name, IsLive, Uniq, IsReal)) :-
	inst_name_apply_sub(Sub, Name0, Name).
inst_name_apply_sub(Sub, shared_inst(Name0), shared_inst(Name)) :-
	inst_name_apply_sub(Sub, Name0, Name).
inst_name_apply_sub(Sub, mostly_uniq_inst(Name0),
		mostly_uniq_inst(Name)) :-
	inst_name_apply_sub(Sub, Name0, Name).
inst_name_apply_sub(_Sub, typed_ground(Uniq, Type),
		typed_ground(Uniq, Type)).
inst_name_apply_sub(Sub, typed_inst(Type, Name0),
		typed_inst(Type, Name)) :-
	inst_name_apply_sub(Sub, Name0, Name).
inst_name_apply_sub(Sub, other_inst(OtherInstId0, Name0),
		other_inst(OtherInstId, Name)) :-
	Sub = inst_table_sub(_IKSub, IdSub),
	OtherInstId = other_inst_id_apply_sub(IdSub, OtherInstId0),
	inst_name_apply_sub(Sub, Name0, Name).

inst_apply_inst_table_sub(_Sub, any(Uniq), any(Uniq)).
inst_apply_inst_table_sub(Sub, alias(Key0), Inst) :-
	Sub = inst_table_sub(IKSub, _),
	( map__search(IKSub, Key0, Key1) ->
		inst_apply_inst_table_sub(Sub, alias(Key1), Inst)
	;
		Inst = alias(Key0)
	).
inst_apply_inst_table_sub(_Sub, free(Aliasing), free(Aliasing)).
inst_apply_inst_table_sub(_Sub, free(Aliasing, Type), free(Aliasing, Type)).
inst_apply_inst_table_sub(Sub, bound(Uniq, BoundInsts0),
		bound(Uniq, BoundInsts)) :-
	list__map((pred(functor(C, B0)::in, functor(C, B)::out) is det :-
			list__map(inst_apply_inst_table_sub(Sub), B0, B)),
		BoundInsts0, BoundInsts).
inst_apply_inst_table_sub(_Sub, ground(Uniq, MaybePredInstInfo),
		ground(Uniq, MaybePredInstInfo)).
inst_apply_inst_table_sub(_Sub, not_reached, not_reached).
inst_apply_inst_table_sub(_Sub, inst_var(V), inst_var(V)).
inst_apply_inst_table_sub(Sub, defined_inst(Name0), defined_inst(Name)) :-
	inst_name_apply_sub(Sub, Name0, Name).
inst_apply_inst_table_sub(Sub, abstract_inst(SymName, Insts0),
		abstract_inst(SymName, Insts)) :-
	list__map(inst_apply_inst_table_sub(Sub), Insts0, Insts).

:- pred maybe_inst_apply_sub(inst_table_sub, maybe_inst, maybe_inst).
:- mode maybe_inst_apply_sub(in, in, out) is det.

maybe_inst_apply_sub(_, unknown, unknown).
maybe_inst_apply_sub(Sub, known(I0), known(I)) :-
	inst_apply_inst_table_sub(Sub, I0, I).

:- pred maybe_inst_det_apply_sub(inst_table_sub, maybe_inst_det,
		maybe_inst_det).
:- mode maybe_inst_det_apply_sub(in, in, out) is det.

maybe_inst_det_apply_sub(_, unknown, unknown).
maybe_inst_det_apply_sub(Sub, known(I0, D), known(I, D)) :-
	inst_apply_inst_table_sub(Sub, I0, I).

:- pred merge_inst_pair_apply_sub(inst_table_sub, merge_inst_pair,
		merge_inst_pair).
:- mode merge_inst_pair_apply_sub(in, in, out) is det.

merge_inst_pair_apply_sub(Sub, Pair0, Pair) :-
	Pair0 = merge_inst_pair(IsLive, IA0, IB0),
	inst_apply_inst_table_sub(Sub, IA0, IA),
	inst_apply_inst_table_sub(Sub, IB0, IB),
	Pair = merge_inst_pair(IsLive, IA, IB).

:- pred other_inst_apply_sub(inst_table_sub, other_inst, other_inst).
:- mode other_inst_apply_sub(in, in, out) is det.

other_inst_apply_sub(Sub, other_inst(Id0, Name0), other_inst(Id, Name)) :-
	Sub = inst_table_sub(_IKSub, IdSub),
	Id = other_inst_id_apply_sub(IdSub, Id0),
	inst_name_apply_sub(Sub, Name0, Name).

