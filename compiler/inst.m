%-----------------------------------------------------------------------------%
% Copyright (C) 1997 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% inst.m - Contains the (inst) data type.
% Main author: bromage
%
%-----------------------------------------------------------------------------%

:- module (inst).
:- interface.

:- import_module prog_data, hlds_data, hlds_pred.
:- import_module list, std_util, term, map, io.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- type (inst)
	--->		any(uniqueness)
	;		alias(inst_key)
	;		free
	;		free(type)
	;		bound(uniqueness, list(bound_inst))
				% The list(bound_inst) must be sorted
	;		ground(uniqueness, maybe(pred_inst_info))
				% The pred_inst_info is used for
				% higher-order pred modes
	;		not_reached
	;		inst_var(var)
				% A defined_inst is possibly recursive
				% inst whose value is stored in the
				% inst_table.  This is used both for
				% user-defined insts and for
				% compiler-generated insts.
	;		defined_inst(inst_name)
				% An abstract inst is a defined inst which
				% has been declared but not actually been
				% defined (yet).
	;		abstract_inst(sym_name, list(inst)).

:- type uniqueness
	--->		shared		% there might be other references
	;		unique		% there is only one reference
	;		mostly_unique	% there is only one reference
					% but there might be more on
					% backtracking
	;		clobbered	% this was the only reference, but
					% the data has already been reused
	;		mostly_clobbered.
					% this was the only reference, but
					% the data has already been reused;
					% however, there may be more references
					% on backtracking, so we will need to
					% restore the old value on backtracking

	% higher-order predicate terms are given the inst
	%	`ground(shared, yes(PredInstInfo))'
	% where the PredInstInfo contains the extra modes and the determinism
	% for the predicate.  Note that the higher-order predicate term
	% itself must be ground.

:- type pred_inst_info
	---> pred_inst_info(
			pred_or_func,		% is this a higher-order func
						% mode or a higher-order pred
						% mode?
			list(mode),		% the modes of the additional
						% (i.e. not-yet-supplied)
						% arguments of the pred;
						% for a function, this includes
						% the mode of the return value
						% as the last element of the
						% list.
			determinism		% the determinism of the
						% predicate or function
	).

:- type bound_inst	--->	functor(cons_id, list(inst)).

:- type inst_key.
:- type inst_key_table.

:- type inst_key_sub == map(inst_key, inst_key).

:- pred inst_key_table_init(inst_key_table).
:- mode inst_key_table_init(out) is det.

:- pred inst_key_table_lookup(inst_key_table, inst_key, inst).
:- mode inst_key_table_lookup(in, in, out) is det.

:- pred inst_key_table_add(inst_key_table, inst, inst_key, inst_key_table).
:- mode inst_key_table_add(in, in, out, out) is det.

:- pred inst_key_table_dependent_keys(inst_key_table, inst_key, list(inst_key)).
:- mode inst_key_table_dependent_keys(in, in, out) is det.

:- pred inst_key_table_kill_keys(inst_key_table, set(inst_key),
	inst_key_table).
:- mode inst_key_table_kill_keys(in, in, out) is det.

:- pred inst_key_table_key_is_dead(inst_key_table, inst_key).
:- mode inst_key_table_key_is_dead(in, in) is semidet.

:- pred inst_key_table_added_keys(inst_key_table, inst_key_table,
		set(inst_key)).
:- mode inst_key_table_added_keys(in, in, out) is det.

:- pred inst_key_table_write_inst_key(inst_key_table, inst_key,
		io__state, io__state).
:- mode inst_key_table_write_inst_key(in, in, di, uo) is det.

:- pred inst_key_table_sanity_check(inst_key_table :: in) is semidet.

:- pred inst_key_table_dump(inst_key_table, io__state, io__state).
:- mode inst_key_table_dump(in, di, uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred inst_keys_in_inst(inst, list(inst_key), list(inst_key)).
:- mode inst_keys_in_inst(in, in, out) is det.

:- pred inst_expand_fully(inst, inst_key_table, inst).
:- mode inst_expand_fully(in, in, out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module mercury_to_mercury.	% ZZZ Remove this dependency!
:- import_module set, multi_map, assoc_list, require, int, varset.

:- type inst_key == int.

:- type inst_key_table --->
	inst_key_table(
		inst_key,
		map(inst_key, inst),
		multi_map(inst_key, inst_key),
		set(inst_key)		% inst_keys which are dead
	).

inst_key_table_init(InstKeyTable) :-
	map__init(FwdMap),
	multi_map__init(BwdMap),
	set__init(DeadKeys),
	InstKeyTable = inst_key_table(0, FwdMap, BwdMap, DeadKeys).

inst_key_table_lookup(inst_key_table(_NextKey, FwdMap, _BwdMap, DeadKeys),
		Key, Inst) :-
	( set__member(Key, DeadKeys) ->
		error("inst_key_table_lookup: dead key")
	;
		map__lookup(FwdMap, Key, Inst)
	).

inst_key_table_add(InstKeyTable0, Inst, ThisKey, InstKeyTable) :-
	InstKeyTable0 = inst_key_table(ThisKey, FwdMap0, BwdMap0, DeadKeys),
	NextKey is ThisKey + 1,
	map__det_insert(FwdMap0, ThisKey, Inst, FwdMap),
	inst_keys_in_inst(Inst, [], DependentKeys0),
	list__sort_and_remove_dups(DependentKeys0, DependentKeys),
	add_backward_dependencies(DependentKeys, ThisKey, BwdMap0, BwdMap),
	InstKeyTable = inst_key_table(NextKey, FwdMap, BwdMap, DeadKeys),
	( inst_key_table_sanity_check(InstKeyTable) ->
		true
	;
		error("inst_key_table_add: Failed sanity check")
	).

inst_key_table_dependent_keys(inst_key_table(_NextKey, _FwdMap, BwdMap,
		DeadKeys), Key, DependentKeys) :-
	( multi_map__search(BwdMap, Key, DependentKeys0) ->
		set__sorted_list_to_set(DependentKeys0, DependentKeys1),
		set__difference(DependentKeys1, DeadKeys, DependentKeys2),
		set__to_sorted_list(DependentKeys2, DependentKeys)
	;
		DependentKeys = []
	).

%-----------------------------------------------------------------------------%

inst_key_table_kill_keys(InstKeyTable0, NewDeadSet, InstKeyTable) :-
	InstKeyTable0 = inst_key_table(NextKey, FwdMap0, BwdMap0, DeadKeys0),
	set__to_sorted_list(NewDeadSet, NewDeadKeys),
	inst_key_table_kill_keys_2(BwdMap0, NewDeadKeys, [], BwdKeys),
	list__foldl(lambda([Key :: in, Map0 :: in, Map :: out] is det,
		map__delete(Map0, Key, Map)),
		NewDeadKeys, FwdMap0, FwdMap),
	list__foldl(lambda([Key :: in, Map0 :: in, Map :: out] is det,
		delete_dead_keys_from_bwd_map(Map0, Key, NewDeadSet, Map)),
		BwdKeys, BwdMap0, BwdMap),
	set__union(DeadKeys0, NewDeadSet, DeadKeys),
	InstKeyTable = inst_key_table(NextKey, FwdMap, BwdMap, DeadKeys).

:- pred inst_key_table_kill_keys_2(multi_map(inst_key, inst_key),
		list(inst_key), list(inst_key), list(inst_key)).
:- mode inst_key_table_kill_keys_2(in, in, in, out) is det.

inst_key_table_kill_keys_2(_, [], BwdKeys, BwdKeys).
inst_key_table_kill_keys_2(BwdMap, [K | Ks], BwdKeys0, BwdKeys) :-
	( multi_map__search(BwdMap, K, NewKeys) ->
		list__append(NewKeys, BwdKeys0, BwdKeys1)
	;
		BwdKeys0 = BwdKeys1
	),
	inst_key_table_kill_keys_2(BwdMap, Ks, BwdKeys1, BwdKeys).

:- pred delete_dead_keys_from_bwd_map(multi_map(inst_key, inst_key),
		inst_key, set(inst_key), multi_map(inst_key, inst_key)).
:- mode delete_dead_keys_from_bwd_map(in, in, in, out) is det.

delete_dead_keys_from_bwd_map(Map0, Key, DeadKeys, Map) :-
	( map__search(Map0, Key, DepKeys0) ->
		list__filter(lambda([TestKey :: in] is semidet,
			\+ set__member(TestKey, DeadKeys)), DepKeys0, DepKeys),
		map__set(Map0, Key, DepKeys, Map)
	;
		Map = Map0
	).

%-----------------------------------------------------------------------------%

inst_key_table_key_is_dead(inst_key_table(_, _, _, DeadKeys), Key) :-
	set__member(Key, DeadKeys).

%-----------------------------------------------------------------------------%

inst_key_table_added_keys(inst_key_table(FirstKey, _, _, _),
		inst_key_table(LastKey1, _, _, _), AddedKeys) :-
	set__init(AddedKeys0),
	LastKey is LastKey1 - 1,
	inst_key_table_added_keys_2(FirstKey, LastKey, AddedKeys0, AddedKeys).

:- pred inst_key_table_added_keys_2(inst_key, inst_key, set(inst_key),
		set(inst_key)).
:- mode inst_key_table_added_keys_2(in, in, in, out) is det.

inst_key_table_added_keys_2(FirstKey, LastKey, AddedKeys0, AddedKeys) :-
	( FirstKey > LastKey ->
		AddedKeys0 = AddedKeys
	;
		set__insert(AddedKeys0, LastKey, AddedKeys1),
		LastKey1 is LastKey - 1,
		inst_key_table_added_keys_2(FirstKey, LastKey1,
				AddedKeys1, AddedKeys)
	).

%-----------------------------------------------------------------------------%

inst_key_table_write_inst_key(_IKT, InstKey) -->
	io__write_string("IK_"),
	io__write_int(InstKey).

%-----------------------------------------------------------------------------%

/***********
inst_key_table_sanity_check(inst_key_table(_NextKey, FwdMap, BwdMap,
		_DeadKeys)) :-
	map__to_assoc_list(FwdMap, FwdAL0),
	list__map(lambda([Item0 :: in, Item :: out] is det,
		(Item0 = InstKeyK - Inst, inst_keys_in_inst(Inst, [], Keys),
			Item = InstKeyK - Keys)), FwdAL0, FwdMAL0),
	break_multi_assoc_list(FwdMAL0, FwdAL),
	set__list_to_set(FwdAL, FwdSet),

	map__to_assoc_list(BwdMap, BwdAL0),
	break_multi_assoc_list(BwdAL0, BwdAL1),
	list__map(lambda([KV :: in, VK :: out] is det,
			(KV = K - V, VK = V - K)),
		BwdAL1, BwdAL),
	set__list_to_set(BwdAL, BwdSet),

	set__subset(FwdSet, BwdSet).
***********/
inst_key_table_sanity_check(_) :- semidet_succeed.


	% break_multi_assoc_list makes no guarantee about the final
	% order of the assoc_list.
	%
:- pred break_multi_assoc_list(assoc_list(K, list(V)), assoc_list(K, V)).
:- mode break_multi_assoc_list(in, out) is det.

break_multi_assoc_list([], []).
break_multi_assoc_list([K - Vs | AL0], AL) :-
	break_multi_assoc_list(AL0, AL1),
	break_multi_assoc_list_2(K, Vs, AL1, AL).

:- pred break_multi_assoc_list_2(K, list(V), assoc_list(K, V),assoc_list(K, V)).
:- mode break_multi_assoc_list_2(in, in, in, out) is det.

break_multi_assoc_list_2(_K, [], AL, AL).
break_multi_assoc_list_2(K, [V | Vs], AL0, [K - V | AL]) :-
	break_multi_assoc_list_2(K, Vs, AL0, AL).

%-----------------------------------------------------------------------------%

inst_key_table_dump(InstKeyTable) -->
	{ InstKeyTable = inst_key_table(_NextKey, FwdMap, _BwdMap, _DeadKeys) },
	{ map__to_assoc_list(FwdMap, FwdAL) },
	inst_key_table_dump_2(FwdAL, InstKeyTable).

:- pred inst_key_table_dump_2(assoc_list(inst_key, inst), inst_key_table,
		io__state, io__state).
:- mode inst_key_table_dump_2(in, in, di, uo) is det.

inst_key_table_dump_2([], _IKT) --> [].
inst_key_table_dump_2([IK - I | AL], IKT) -->
	io__nl,
	io__write_string("% "),
	inst_key_table_write_inst_key(IKT, IK),
	io__write_string(" = "),
	{ varset__init(VarSet) },
	mercury_output_inst(dont_expand, I, VarSet, IKT),
	inst_key_table_dump_2(AL, IKT).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred add_backward_dependencies(list(K), V, multi_map(K, V), multi_map(K, V)).
:- mode add_backward_dependencies(in, in, in, out) is det.

add_backward_dependencies([], _V, BwdMap, BwdMap).
add_backward_dependencies([K | Ks], V, BwdMap0, BwdMap) :-
	( map__search(BwdMap0, K, Vs0) ->
%		list__merge([V], Vs0, Vs)
		Vs = [V | Vs0]
	;
		Vs = [V]
	),
	map__set(BwdMap0, K, Vs, BwdMap1),
	add_backward_dependencies(Ks, V, BwdMap1, BwdMap).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

inst_keys_in_inst(any(_Uniq), Keys, Keys).
inst_keys_in_inst(alias(Key), Keys, [Key | Keys]).
inst_keys_in_inst(free, Keys, Keys).
inst_keys_in_inst(free(_Type), Keys, Keys).
inst_keys_in_inst(bound(_Uniq, BoundInsts), Keys0, Keys) :-
	inst_keys_in_bound_insts(BoundInsts, Keys0, Keys).
	inst_keys_in_inst(ground(_Uniq, _MaybePredInstInfo), Keys, Keys).
	inst_keys_in_inst(not_reached, Keys, Keys).
	inst_keys_in_inst(inst_var(_Var), Keys, Keys).
	inst_keys_in_inst(defined_inst(_InstName), Keys, Keys).
inst_keys_in_inst(abstract_inst(_SymName, Insts), Keys0, Keys) :-
	inst_keys_in_insts(Insts, Keys0, Keys).

:- pred inst_keys_in_insts(list(inst), list(inst_key), list(inst_key)).
:- mode inst_keys_in_insts(in, in, out) is det.

inst_keys_in_insts([], Keys, Keys).
inst_keys_in_insts([I | Is], Keys0, Keys) :-
	inst_keys_in_inst(I, Keys0, Keys1),
	inst_keys_in_insts(Is, Keys1, Keys).

:- pred inst_keys_in_bound_insts(list(bound_inst),
		list(inst_key), list(inst_key)).
:- mode inst_keys_in_bound_insts(in, in, out) is det.

inst_keys_in_bound_insts([], Keys, Keys).
inst_keys_in_bound_insts([functor(_ConsId, Insts) | BIs], Keys0, Keys) :-
	inst_keys_in_insts(Insts, Keys0, Keys1),
	inst_keys_in_bound_insts(BIs, Keys1, Keys).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

inst_expand_fully(any(Uniq), _IKT, any(Uniq)).
inst_expand_fully(alias(Key), IKT, Inst) :-
	inst_key_table_lookup(IKT, Key, Inst0),
	inst_expand_fully(Inst0, IKT, Inst).
inst_expand_fully(free, _IKT, free).
inst_expand_fully(free(Type), _IKT, free(Type)).
inst_expand_fully(bound(Uniq, BoundInsts0), IKT, bound(Uniq, BoundInsts)) :-
	bound_insts_expand_fully(BoundInsts0, IKT, BoundInsts).
inst_expand_fully(ground(Uniq, PredInstInfo), _IKT, ground(Uniq, PredInstInfo)).
inst_expand_fully(not_reached, _IKT, not_reached).
inst_expand_fully(inst_var(Var), _IKT, inst_var(Var)).
inst_expand_fully(defined_inst(InstName), _IKT, defined_inst(InstName)).
inst_expand_fully(abstract_inst(SymName, Insts0), IKT,
			abstract_inst(SymName, Insts)) :-
	inst_list_expand_fully(Insts0, IKT, Insts).

:- pred bound_insts_expand_fully(list(bound_inst), inst_key_table,
		list(bound_inst)).
:- mode bound_insts_expand_fully(in, in, out) is det.

bound_insts_expand_fully([], _IKT, []).
bound_insts_expand_fully([functor(ConsId, Insts0) | BIs0], IKT,
		[functor(ConsId, Insts) | BIs]) :-
	inst_list_expand_fully(Insts0, IKT, Insts),
	bound_insts_expand_fully(BIs0, IKT, BIs).

:- pred inst_list_expand_fully(list(inst), inst_key_table, list(inst)).
:- mode inst_list_expand_fully(in, in, out) is det.

inst_list_expand_fully([], _IKT, []).
inst_list_expand_fully([I0 | Is0], IKT, [I | Is]) :-
	inst_expand_fully(I0, IKT, I),
	inst_list_expand_fully(Is0, IKT, Is).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
