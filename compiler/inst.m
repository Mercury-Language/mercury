%-----------------------------------------------------------------------------%
% Copyright (C) 1997-1998 The University of Melbourne.
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
:- import_module list, std_util, term, map, io, set.

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
			argument_modes,		% the modes of the additional
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

:- pred inst_key_table_update(inst_key_table, inst_key, inst, inst_key_table).
:- mode inst_key_table_update(in, in, in, out) is det.

:- pred inst_key_table_dependent_keys(inst_key_table, inst_key, list(inst_key)).
:- mode inst_key_table_dependent_keys(in, in, out) is det.

:- pred inst_key_table_added_keys(inst_key_table, inst_key_table,
		set(inst_key)).
:- mode inst_key_table_added_keys(in, in, out) is det.

:- pred inst_key_table_project(inst_key_table, list(inst_key), inst_key_table).
:- mode inst_key_table_project(in, in, out) is det.

	% `inst_key_table_create_sub(IKT0, IKT1, Sub, IKT)' renames
	% apart all the inst_keys in IKT1 with respect to IKT0 and
	% returns the substitution used (Sub) and IKT0 with all of
	% the inst_keys from IKT1 renamed and added (IKT).
:- pred inst_key_table_create_sub(inst_key_table, inst_key_table,
		inst_key_sub, inst_key_table).
:- mode inst_key_table_create_sub(in, in, out, out) is det.

%-----------------------------------------------------------------------------%

:- pred inst_key_table_optimise(inst_key_table, list(inst), inst_key_sub,
		inst_key_table).
:- mode inst_key_table_optimise(in, in, out, out) is det.

:- pred inst_key_table_write_inst_key(inst_key_table, inst_key,
		io__state, io__state).
:- mode inst_key_table_write_inst_key(in, in, di, uo) is det.

%-----------------------------------------------------------------------------%

	% Debugging trace code.
:- type inst_key_table_mark.

:- pred inst_key_table_mark_init(inst_key_table_mark).
:- mode inst_key_table_mark_init(out) is det.

:- pred inst_key_table_get_mark(inst_key_table, inst_key_table_mark).
:- mode inst_key_table_get_mark(in, out) is det.

:- pred inst_key_table_print_since_mark(inst_key_table, inst_key_table_mark,
		io__state, io__state).
:- mode inst_key_table_print_since_mark(in, in, di, uo) is det.

:- pred inst_key_table_sanity_check(inst_key_table :: in) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred inst_keys_in_inst(inst, list(inst_key), list(inst_key)).
:- mode inst_keys_in_inst(in, in, out) is det.

:- pred inst_expand_fully(inst_key_table, inst, inst).
:- mode inst_expand_fully(in, in, out) is det.

:- pred inst_apply_sub(inst_key_sub, inst, inst).
:- mode inst_apply_sub(in, in, out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module mercury_to_mercury.	% ZZZ Remove this dependency!
:- import_module set, multi_map, assoc_list, require, int, varset.
:- import_module set_bbbtree, string.

:- type inst_key == int.

:- type inst_key_table --->
	inst_key_table(
		inst_key,
		map(inst_key, inst),
		multi_map(inst_key, inst_key)
	).

inst_key_table_init(InstKeyTable) :-
	map__init(FwdMap),
	multi_map__init(BwdMap),
	InstKeyTable = inst_key_table(0, FwdMap, BwdMap).

inst_key_table_lookup(inst_key_table(_NextKey, FwdMap, _BwdMap),
		Key, Inst) :-
	map__lookup(FwdMap, Key, Inst).

inst_key_table_add(InstKeyTable0, Inst, ThisKey, InstKeyTable) :-
	InstKeyTable0 = inst_key_table(ThisKey, FwdMap0, BwdMap0),
	NextKey is ThisKey + 1,
	map__det_insert(FwdMap0, ThisKey, Inst, FwdMap),
	inst_keys_in_inst(Inst, [], DependentKeys0),
	list__sort_and_remove_dups(DependentKeys0, DependentKeys),
	add_backward_dependencies(DependentKeys, ThisKey, BwdMap0, BwdMap),
	InstKeyTable = inst_key_table(NextKey, FwdMap, BwdMap),
	( inst_key_table_sanity_check(InstKeyTable) ->
		true
	;
		error("inst_key_table_add: Failed sanity check")
	).

inst_key_table_update(InstKeyTable0, Key, Inst, InstKeyTable) :-
	InstKeyTable0 = inst_key_table(NextKey, FwdMap0, BwdMap0),
	map__det_update(FwdMap0, Key, Inst, FwdMap),
	inst_keys_in_inst(Inst, [], DependentKeys0),
	list__sort_and_remove_dups(DependentKeys0, DependentKeys),
	add_backward_dependencies(DependentKeys, Key, BwdMap0, BwdMap),
	InstKeyTable = inst_key_table(NextKey, FwdMap, BwdMap),
	( inst_key_table_sanity_check(InstKeyTable) ->
		true
	;
		error("inst_key_table_update: Failed sanity check")
	).

inst_key_table_dependent_keys(inst_key_table(_NextKey, _FwdMap, BwdMap),
		Key, DependentKeys) :-
	( multi_map__search(BwdMap, Key, DependentKeys0) ->
		DependentKeys = DependentKeys0
	;
		DependentKeys = []
	).

%-----------------------------------------------------------------------------%

inst_key_table_added_keys(inst_key_table(FirstKey, _, _),
		inst_key_table(LastKey1, _, _), AddedKeys) :-
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

inst_key_table_project(inst_key_table(NextKey, FwdMap, BwdMap0), Keys0,
		inst_key_table(NextKey, FwdMap, BwdMap)) :-
	set_bbbtree__init(SeenKeys0),	
	inst_key_table_project_2(Keys0, SeenKeys0, FwdMap, SeenKeys),
	map__to_assoc_list(BwdMap0, BwdAL),
	map__init(BwdMap1),
	list__foldl(inst_key_table_project_bwd_mapping(SeenKeys), BwdAL,
			BwdMap1, BwdMap).

:- pred inst_key_table_project_2(list(inst_key), set_bbbtree(inst_key),
		map(inst_key, inst), set_bbbtree(inst_key)).
:- mode inst_key_table_project_2(in, in, in, out) is det.

inst_key_table_project_2(KeysList, Keys0, FwdMap, Keys) :-
	( KeysList = [] ->
		Keys = Keys0
	;
		set_bbbtree__init(NewKeys0),
		inst_key_table_project_3(KeysList, FwdMap, Keys0, Keys1,
			NewKeys0, NewKeys),
		set_bbbtree__to_sorted_list(NewKeys, NewKeysList),
		inst_key_table_project_2(NewKeysList, Keys1, FwdMap, Keys)
	).

:- pred inst_key_table_project_3(list(inst_key), map(inst_key, inst),
		set_bbbtree(inst_key), set_bbbtree(inst_key),
		set_bbbtree(inst_key), set_bbbtree(inst_key)).
:- mode inst_key_table_project_3(in, in, in, out, in, out) is det.

inst_key_table_project_3([], _FwdMap, Keys, Keys, NewKeys, NewKeys).
inst_key_table_project_3([K | Ks], FwdMap, Keys0, Keys, NewKeys0, NewKeys) :-
	map__lookup(FwdMap, K, Inst),
	inst_keys_in_inst(Inst, [], InstKeys),
	inst_key_table_project_4(InstKeys, Keys0, Keys1, NewKeys0, NewKeys1),
	inst_key_table_project_3(Ks, FwdMap, Keys1, Keys, NewKeys1, NewKeys).

:- pred inst_key_table_project_4(list(inst_key),
		set_bbbtree(inst_key), set_bbbtree(inst_key),
		set_bbbtree(inst_key), set_bbbtree(inst_key)).
:- mode inst_key_table_project_4(in, in, out, in, out) is det.

inst_key_table_project_4([], Keys, Keys, NewKeys, NewKeys).
inst_key_table_project_4([K | Ks], Keys0, Keys, NewKeys0, NewKeys) :-
	( set_bbbtree__member(K, Keys0) ->
		Keys1 = Keys0, NewKeys1 = NewKeys0
	;
		set_bbbtree__insert(Keys0, K, Keys1),
		set_bbbtree__insert(NewKeys0, K, NewKeys1)
	),
	inst_key_table_project_4(Ks, Keys1, Keys, NewKeys1, NewKeys).

:- pred inst_key_table_project_bwd_mapping(set_bbbtree(inst_key),
		pair(inst_key, list(inst_key)),
		map(inst_key, list(inst_key)), map(inst_key, list(inst_key))).
:- mode inst_key_table_project_bwd_mapping(in, in, in, out) is det.

inst_key_table_project_bwd_mapping(SeenKeys, IK - IKs0, Map0, Map) :-
	list__filter(lambda([X :: in] is semidet,
			set_bbbtree__member(X, SeenKeys)),
		IKs0, IKs),
	map__det_insert(Map0, IK, IKs, Map).

%-----------------------------------------------------------------------------%

inst_key_table_create_sub(IKT0, IKT1, Sub, IKT) :-
	IKT0 = inst_key_table(NextKey0, FwdMap0, BwdMap0),
	IKT1 = inst_key_table(_, FwdMap1, _),
	map__to_assoc_list(FwdMap1, FwdAL),
	map__init(Sub0),
	inst_key_table_create_sub_2(FwdAL, NextKey0, NextKey, Sub0, Sub),
	inst_key_table_create_sub_3(FwdAL, Sub, FwdMap0, FwdMap,
		BwdMap0, BwdMap),
	IKT  = inst_key_table(NextKey, FwdMap, BwdMap).

:- pred inst_key_table_create_sub_2(assoc_list(inst_key, inst), inst_key,
		inst_key, inst_key_sub, inst_key_sub).
:- mode inst_key_table_create_sub_2(in, in, out, in, out) is det.

inst_key_table_create_sub_2([], NextKey, NextKey, Sub, Sub).
inst_key_table_create_sub_2([K - _ | AL], NextKey0, NextKey, Sub0, Sub) :-
	map__det_insert(Sub0, K, NextKey0, Sub1),
	NextKey1 is NextKey0 + 1,
	inst_key_table_create_sub_2(AL, NextKey1, NextKey, Sub1, Sub).

:- pred inst_key_table_create_sub_3(assoc_list(inst_key, inst), inst_key_sub,
		map(inst_key, inst), map(inst_key, inst),
		multi_map(inst_key, inst_key), multi_map(inst_key, inst_key)).
:- mode inst_key_table_create_sub_3(in, in, in, out, in, out) is det.

inst_key_table_create_sub_3([], _Sub, Fwd, Fwd, Bwd, Bwd).
inst_key_table_create_sub_3([K0 - I0 | AL], Sub, Fwd0, Fwd, Bwd0, Bwd) :-
	map__lookup(Sub, K0, K),
	inst_apply_sub(Sub, I0, I),
	map__det_insert(Fwd0, K, I, Fwd1),
	inst_keys_in_inst(I, [], Deps0),
	list__sort_and_remove_dups(Deps0, Deps),
	add_backward_dependencies(Deps, K, Bwd0, Bwd1),
	inst_key_table_create_sub_3(AL, Sub, Fwd1, Fwd, Bwd1, Bwd).

%-----------------------------------------------------------------------------%

inst_key_table_optimise(IKT0, Insts, Sub, IKT) :-
	IKT0 = inst_key_table(_, Fwd, _),
	set_bbbtree__init(Used0),
	set_bbbtree__init(UsedMultiply0),
	find_replacable_keys(Insts, Fwd, Used0, Used,
		UsedMultiply0, UsedMultiply),
	set_bbbtree__difference(UsedMultiply, Used, UsedOnce),
	set_bbbtree__to_sorted_list(UsedMultiply, KeysToRename),
	map__init(NewFwd0),
	multi_map__init(NewBwd0),
	map__init(Sub0),
	inst_key_table_optimise_2(KeysToRename, 0, NextKey, Sub0, Sub),
	inst_key_table_optimise_3(Fwd, KeysToRename, UsedOnce, Sub,
		NewFwd0, NewFwd, NewBwd0, NewBwd),
	IKT = inst_key_table(NextKey, NewFwd, NewBwd).

:- pred inst_key_table_optimise_2(list(inst_key), inst_key, inst_key,
		inst_key_sub, inst_key_sub).
:- mode inst_key_table_optimise_2(in, in, out, in, out) is det.

inst_key_table_optimise_2([], NextKey, NextKey, Sub, Sub).
inst_key_table_optimise_2([IK | IKs], ThisKey, NextKey, Sub0, Sub) :-
	map__det_insert(Sub0, ThisKey, IK, Sub1),
	NextKey0 is ThisKey + 1,
	inst_key_table_optimise_2(IKs, NextKey0, NextKey, Sub1, Sub).

:- pred inst_key_table_optimise_3(map(inst_key, inst), list(inst_key),
		set_bbbtree(inst_key), inst_key_sub,
		map(inst_key, inst), map(inst_key, inst),
		multi_map(inst_key, inst_key), multi_map(inst_key, inst_key)).
:- mode inst_key_table_optimise_3(in, in, in, in, in, out, in, out) is det.

inst_key_table_optimise_3(_OldFwd, [], _UsedOnce, _Sub, Fwd, Fwd, Bwd, Bwd).
inst_key_table_optimise_3(OldFwd, [IK0 | IKs], UsedOnce, Sub,
		Fwd0, Fwd, Bwd0, Bwd) :-
	( set_bbbtree__member(IK0, UsedOnce) ->
		Fwd1 = Fwd0, Bwd1 = Bwd0
	;
		map__lookup(OldFwd, IK0, Inst0),
		map__lookup(Sub, IK0, IK),
		optimise_inst_keys_in_inst(OldFwd, UsedOnce, Sub, Inst0, Inst,
				[], Deps0),
		list__sort_and_remove_dups(Deps0, Deps),
		map__det_insert(Fwd0, IK, Inst, Fwd1),
		add_backward_dependencies(Deps, IK, Bwd0, Bwd1)
	),
	inst_key_table_optimise_3(OldFwd, IKs, UsedOnce, Sub, Fwd1, Fwd,
		Bwd1, Bwd).
		
:- pred optimise_inst_keys_in_inst(map(inst_key, inst), set_bbbtree(inst_key),
		inst_key_sub, inst, inst, list(inst_key), list(inst_key)).
:- mode optimise_inst_keys_in_inst(in, in, in, in, out, in, out) is det.

optimise_inst_keys_in_inst(OldFwd, UsedOnce, Sub,
		alias(IK), Inst, IKs0, IKs) :-
	( set_bbbtree__member(IK, UsedOnce) ->
		map__lookup(OldFwd, IK, Inst0),
		optimise_inst_keys_in_inst(OldFwd, UsedOnce, Sub,
		                Inst0, Inst, IKs0, IKs)
	;
		map__lookup(Sub, IK, NewIK),
		IKs = [NewIK | IKs0],
		Inst = alias(NewIK)
	).
optimise_inst_keys_in_inst(_OldFwd, _UsedOnce, _Sub,
		any(Uniq), any(Uniq), IKs, IKs).
optimise_inst_keys_in_inst(_OldFwd, _UsedOnce, _Sub, free, free, IKs, IKs).
optimise_inst_keys_in_inst(_OldFwd, _UsedOnce, _Sub, free(T), free(T),
		IKs, IKs).
optimise_inst_keys_in_inst(OldFwd, UsedOnce, Sub,
		bound(Uniq, Insts0), bound(Uniq, Insts), IKs0, IKs) :-
	list__map_foldl(optimise_inst_keys_in_bound_inst(OldFwd, UsedOnce, Sub),
		Insts0, Insts, IKs0, IKs).
optimise_inst_keys_in_inst(_OldFwd, _UsedOnce, _Sub,
		ground(Uniq, no), ground(Uniq, no), IKs, IKs).
optimise_inst_keys_in_inst(_OldFwd, _UsedOnce, _Sub,
		ground(Uniq, yes(Pred)), ground(Uniq, yes(Pred)), IKs, IKs).
		% ZZZ ERROR
optimise_inst_keys_in_inst(_OldFwd, _UsedOnce, _Sub, not_reached, not_reached,
		IKs, IKs).
optimise_inst_keys_in_inst(_OldFwd, _UsedOnce, _Sub, inst_var(V), inst_var(V),
		IKs, IKs).
optimise_inst_keys_in_inst(_OldFwd, _UsedOnce, _Sub,
		defined_inst(Name), defined_inst(Name), IKs, IKs).
optimise_inst_keys_in_inst(OldFwd, UsedOnce, Sub,
		abstract_inst(Name, Insts0), abstract_inst(Name, Insts),
		IKs0, IKs) :-
	list__map_foldl(optimise_inst_keys_in_inst(OldFwd, UsedOnce, Sub),
		Insts0, Insts, IKs0, IKs).

:- pred optimise_inst_keys_in_bound_inst(map(inst_key, inst),
		set_bbbtree(inst_key), inst_key_sub, bound_inst, bound_inst,
		list(inst_key), list(inst_key)).
:- mode optimise_inst_keys_in_bound_inst(in, in, in, in, out, in, out) is det.

optimise_inst_keys_in_bound_inst(OldFwd, UsedOnce, Sub,
	 	functor(Cons, Insts0), functor(Cons, Insts), IKs0, IKs) :-
	list__map_foldl(optimise_inst_keys_in_inst(OldFwd, UsedOnce, Sub),
		Insts0, Insts, IKs0, IKs).

:- pred find_replacable_keys(list(inst), map(inst_key, inst),
		set_bbbtree(inst_key), set_bbbtree(inst_key),
		set_bbbtree(inst_key), set_bbbtree(inst_key)).
:- mode find_replacable_keys(in, in, in, out, in, out) is det.

find_replacable_keys([], _, Used, Used, UsedMultiply, UsedMultiply).
find_replacable_keys([I | Is0], Fwd, Used0, Used,
		UsedMultiply0, UsedMultiply) :-
	inst_keys_in_inst(I, [], IKs),
	find_replacable_keys_2(IKs, Fwd, Is0, Is, Used0, Used1,
		UsedMultiply0, UsedMultiply1),
	find_replacable_keys(Is, Fwd, Used1, Used,
		UsedMultiply1, UsedMultiply).

:- pred find_replacable_keys_2(list(inst_key), map(inst_key, inst),
		list(inst), list(inst),
		set_bbbtree(inst_key), set_bbbtree(inst_key),
		set_bbbtree(inst_key), set_bbbtree(inst_key)).
:- mode find_replacable_keys_2(in, in, in, out, in, out, in, out) is det.

find_replacable_keys_2([], _, Is, Is, Used, Used, UsedMultiply, UsedMultiply).
find_replacable_keys_2([IK | IKs], Fwd, Is0, Is, Used0, Used,
		UsedMultiply0, UsedMultiply) :-
	( set_bbbtree__member(IK, UsedMultiply0) ->
		Used1 = Used0,
		UsedMultiply1 = UsedMultiply0,
		Is1 = Is0
	; set_bbbtree__member(IK, Used0) ->
		Used1 = Used0,
		set_bbbtree__insert(UsedMultiply0, IK, UsedMultiply1),
		Is1 = Is0
	;
		UsedMultiply1 = UsedMultiply0,
		set_bbbtree__insert(Used0, IK, Used1),
		map__lookup(Fwd, IK, I),
		Is1 = [I | Is0]
	),
	find_replacable_keys_2(IKs, Fwd, Is1, Is, Used1, Used,
	                UsedMultiply1, UsedMultiply).

%-----------------------------------------------------------------------------%

inst_key_table_write_inst_key(_IKT, InstKey) -->
	io__write_string("IK_"),
	io__write_int(InstKey).

%-----------------------------------------------------------------------------%

/***********
inst_key_table_sanity_check(inst_key_table(_NextKey, FwdMap, BwdMap)) :-
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
%-----------------------------------------------------------------------------%

:- type inst_key_table_mark == inst_key.

inst_key_table_mark_init(-1).

inst_key_table_get_mark(inst_key_table(NextKey, _, _), NextKey).

inst_key_table_print_since_mark(inst_key_table(_, FwdTable, _), Mark) -->
	{ map__keys(FwdTable, KeysList) },
	inst_key_table_print_since_mark_2(KeysList, Mark, FwdTable).

:- pred inst_key_table_print_since_mark_2(list(inst_key), inst_key_table_mark,
		map(inst_key, inst), io__state, io__state).
:- mode inst_key_table_print_since_mark_2(in, in, in, di, uo) is det.

inst_key_table_print_since_mark_2([], _, _) --> [].
inst_key_table_print_since_mark_2([K | Ks], Mark, FwdMap) -->
	( { K < Mark } ->
		[]
	;
		io__format("\tIK_%d = \t", [i(K)]),
		{ map__lookup(FwdMap, K, Inst) },
		io__write(Inst),
		io__nl
	),
	inst_key_table_print_since_mark_2(Ks, Mark, FwdMap).

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

inst_expand_fully(_IKT, any(Uniq), any(Uniq)).
inst_expand_fully(IKT, alias(Key), Inst) :-
	inst_key_table_lookup(IKT, Key, Inst0),
	inst_expand_fully(IKT, Inst0, Inst).
inst_expand_fully(_IKT, free, free).
inst_expand_fully(_IKT, free(Type), free(Type)).
inst_expand_fully(IKT, bound(Uniq, BoundInsts0), bound(Uniq, BoundInsts)) :-
	bound_insts_expand_fully(IKT, BoundInsts0, BoundInsts).
inst_expand_fully(_IKT, ground(Uniq, PredInstInfo), ground(Uniq, PredInstInfo)).
inst_expand_fully(_IKT, not_reached, not_reached).
inst_expand_fully(_IKT, inst_var(Var), inst_var(Var)).
inst_expand_fully(_IKT, defined_inst(InstName), defined_inst(InstName)).
inst_expand_fully(IKT, abstract_inst(SymName, Insts0),
			abstract_inst(SymName, Insts)) :-
	list__map(inst_expand_fully(IKT), Insts0, Insts).

:- pred bound_insts_expand_fully(inst_key_table, list(bound_inst),
		list(bound_inst)).
:- mode bound_insts_expand_fully(in, in, out) is det.

bound_insts_expand_fully(_IKT, [], []).
bound_insts_expand_fully(IKT, [functor(ConsId, Insts0) | BIs0],
		[functor(ConsId, Insts) | BIs]) :-
	list__map(inst_expand_fully(IKT), Insts0, Insts),
	bound_insts_expand_fully(IKT, BIs0, BIs).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

inst_apply_sub(_Sub, any(Uniq), any(Uniq)).
inst_apply_sub(Sub, alias(Key0), alias(Key)) :-
	( map__search(Sub, Key0, Key1) ->
		Key = Key1
	;
		Key = Key0
	).
inst_apply_sub(_Sub, free, free).
inst_apply_sub(_Sub, free(Type), free(Type)).
inst_apply_sub(Sub, bound(Uniq, BoundInsts0), bound(Uniq, BoundInsts)) :-
	list__map(bound_inst_apply_sub(Sub), BoundInsts0, BoundInsts).
inst_apply_sub(_Sub, ground(Uniq, MaybePredInstInfo),
		ground(Uniq, MaybePredInstInfo)).
inst_apply_sub(_Sub, not_reached, not_reached).
inst_apply_sub(_Sub, inst_var(V), inst_var(V)).
inst_apply_sub(_Sub, defined_inst(Name), defined_inst(Name)).
inst_apply_sub(Sub, abstract_inst(SymName, Insts0),
		abstract_inst(SymName, Insts)) :-
	list__map(inst_apply_sub(Sub), Insts0, Insts).

:- pred bound_inst_apply_sub(map(inst_key, inst_key), bound_inst, bound_inst).
:- mode bound_inst_apply_sub(in, in, out) is det.

bound_inst_apply_sub(Sub, functor(ConsId, Insts0), functor(ConsId, Insts)) :-
	list__map(inst_apply_sub(Sub), Insts0, Insts).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
