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

:- import_module prog_data, hlds_data, hlds_pred, instmap.
:- import_module list, std_util, term, map, io, set.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- type (inst)
	--->		any(uniqueness)
	;		alias(inst_key)
	;		free(aliasing)
	;		free(aliasing, type)
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

	% Aliasing information for free insts.
	% This information is needed during code generation because free(alias)
	% and free(alias_many) variables have references associated with them
	% whereas free(unique) variables need no representation until they are
	% bound (or become free(alias)).
:- type aliasing
	--->		unique	% No aliasing.  Such variables either have no
				% representation or are represented by junk.
	;		alias.	% Inst is for a free variable in a known number
				% of partially instantiated data structures.
				% Such variables are represented by a reference
				% to where their value needs to be put when
				% they are bound.
	% ;		alias_many.
				% XXX not yet implemented.
				% Inst is for a variable that has an arbitrary
				% number of references which are not all known
				% in the current scope.

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

:- pred inst_key_table_added_keys(inst_key_table, inst_key_table,
		set(inst_key)).
:- mode inst_key_table_added_keys(in, in, out) is det.

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

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred inst_keys_in_inst(inst, list(inst_key), list(inst_key)).
:- mode inst_keys_in_inst(in, in, out) is det.

:- pred inst_expand_fully(inst_key_table, instmap, inst, inst).
:- mode inst_expand_fully(in, in, in, out) is det.

:- pred inst_apply_sub(inst_key_sub, inst, inst).
:- mode inst_apply_sub(in, in, out) is det.

:- pred bound_inst_apply_sub(map(inst_key, inst_key), bound_inst, bound_inst).
:- mode bound_inst_apply_sub(in, in, out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module mercury_to_mercury.	% ZZZ Remove this dependency!
:- import_module set, assoc_list, require, int, varset.
:- import_module set_bbbtree, string.

:- type inst_key == int.

:- type inst_key_table --->
	inst_key_table(
		inst_key,
		map(inst_key, inst)
	).

inst_key_table_init(InstKeyTable) :-
	map__init(FwdMap),
	InstKeyTable = inst_key_table(0, FwdMap).

inst_key_table_lookup(inst_key_table(_NextKey, FwdMap), Key, Inst) :-
	map__lookup(FwdMap, Key, Inst).

inst_key_table_add(InstKeyTable0, Inst, ThisKey, InstKeyTable) :-
	( Inst = alias(_) ->
		error("inst_key_table_add: Attempt to create an inst_key for alias(_).  This makes bad things happen later on.")
	;
		InstKeyTable0 = inst_key_table(ThisKey, FwdMap0),
		(
			% sanity check
			inst_contains_invalid_inst_key(Inst, ThisKey,
				InstKeyTable0)
		->
			error("inst_key_table_add: inst key already in use.  Are you sure you're using the right inst key table?")
		;
			NextKey is ThisKey + 1,
			map__det_insert(FwdMap0, ThisKey, Inst, FwdMap),
			InstKeyTable = inst_key_table(NextKey, FwdMap)
		)
	).

	% Sanity check.  If the inst we're inserting into the inst key
	% table contains an inst key >= the current inst key in
	% the table then something is wrong.  This probably means that
	% we are using the wrong inst key table.
:- pred inst_contains_invalid_inst_key(inst, inst_key, inst_key_table).
:- mode inst_contains_invalid_inst_key(in, in, in) is semidet.

inst_contains_invalid_inst_key(alias(IK), ThisIK, _IKT) :-
	IK >= ThisIK.
inst_contains_invalid_inst_key(bound(_, BoundInsts), ThisIK, IKT) :-
	list__member(functor(_, ArgInsts), BoundInsts),
	list__member(Inst, ArgInsts),
	inst_contains_invalid_inst_key(Inst, ThisIK, IKT).

inst_key_table_update(InstKeyTable0, Key, Inst, InstKeyTable) :-
	( Inst = alias(_) ->
		error("inst_key_table_update: Attempt to update an inst_key to alias(_).  This makes bad things happen later on.")
	;
		InstKeyTable0 = inst_key_table(NextKey, FwdMap0),
		map__det_update(FwdMap0, Key, Inst, FwdMap),
		InstKeyTable = inst_key_table(NextKey, FwdMap)
	).

%-----------------------------------------------------------------------------%

inst_key_table_added_keys(inst_key_table(FirstKey, _),
		inst_key_table(LastKey1, _), AddedKeys) :-
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

inst_key_table_create_sub(IKT0, IKT1, Sub, IKT) :-
	IKT0 = inst_key_table(NextKey0, FwdMap0),
	IKT1 = inst_key_table(_, FwdMap1),
	map__to_assoc_list(FwdMap1, FwdAL),
	map__init(Sub0),
	inst_key_table_create_sub_2(FwdAL, NextKey0, NextKey, Sub0, Sub),
	inst_key_table_create_sub_3(FwdAL, Sub, FwdMap0, FwdMap),
	IKT  = inst_key_table(NextKey, FwdMap).

:- pred inst_key_table_create_sub_2(assoc_list(inst_key, inst), inst_key,
		inst_key, inst_key_sub, inst_key_sub).
:- mode inst_key_table_create_sub_2(in, in, out, in, out) is det.

inst_key_table_create_sub_2([], NextKey, NextKey, Sub, Sub).
inst_key_table_create_sub_2([K - _ | AL], NextKey0, NextKey, Sub0, Sub) :-
	map__det_insert(Sub0, K, NextKey0, Sub1),
	NextKey1 is NextKey0 + 1,
	inst_key_table_create_sub_2(AL, NextKey1, NextKey, Sub1, Sub).

:- pred inst_key_table_create_sub_3(assoc_list(inst_key, inst), inst_key_sub,
		map(inst_key, inst), map(inst_key, inst)).
:- mode inst_key_table_create_sub_3(in, in, in, out) is det.

inst_key_table_create_sub_3([], _Sub, Fwd, Fwd).
inst_key_table_create_sub_3([K0 - I0 | AL], Sub, Fwd0, Fwd) :-
	map__lookup(Sub, K0, K),
	inst_apply_sub(Sub, I0, I),
	map__det_insert(Fwd0, K, I, Fwd1),
	inst_key_table_create_sub_3(AL, Sub, Fwd1, Fwd).

%-----------------------------------------------------------------------------%

inst_key_table_optimise(IKT0, Insts, Sub, IKT) :-
	IKT0 = inst_key_table(_, Fwd),
	set_bbbtree__init(Used0),
	set_bbbtree__init(UsedMultiply0),
	find_replacable_keys(Insts, Fwd, Used0, Used,
		UsedMultiply0, UsedMultiply),
	set_bbbtree__difference(UsedMultiply, Used, UsedOnce),
	set_bbbtree__to_sorted_list(UsedMultiply, KeysToRename),
	map__init(NewFwd0),
	map__init(Sub0),
	inst_key_table_optimise_2(KeysToRename, 0, NextKey, Sub0, Sub),
	inst_key_table_optimise_3(Fwd, KeysToRename, UsedOnce, Sub,
		NewFwd0, NewFwd),
	IKT = inst_key_table(NextKey, NewFwd).

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
		map(inst_key, inst), map(inst_key, inst)).
:- mode inst_key_table_optimise_3(in, in, in, in, in, out) is det.

inst_key_table_optimise_3(_OldFwd, [], _UsedOnce, _Sub, Fwd, Fwd).
inst_key_table_optimise_3(OldFwd, [IK0 | IKs], UsedOnce, Sub, Fwd0, Fwd) :-
	( set_bbbtree__member(IK0, UsedOnce) ->
		Fwd1 = Fwd0
	;
		map__lookup(OldFwd, IK0, Inst0),
		map__lookup(Sub, IK0, IK),
		optimise_inst_keys_in_inst(OldFwd, UsedOnce, Sub, Inst0, Inst),
		map__det_insert(Fwd0, IK, Inst, Fwd1)
	),
	inst_key_table_optimise_3(OldFwd, IKs, UsedOnce, Sub, Fwd1, Fwd).

:- pred optimise_inst_keys_in_inst(map(inst_key, inst), set_bbbtree(inst_key),
		inst_key_sub, inst, inst).
:- mode optimise_inst_keys_in_inst(in, in, in, in, out) is det.

optimise_inst_keys_in_inst(OldFwd, UsedOnce, Sub, alias(IK), Inst) :-
	( set_bbbtree__member(IK, UsedOnce) ->
		map__lookup(OldFwd, IK, Inst0),
		optimise_inst_keys_in_inst(OldFwd, UsedOnce, Sub, Inst0, Inst)
	;
		map__lookup(Sub, IK, NewIK),
		Inst = alias(NewIK)
	).
optimise_inst_keys_in_inst(_OldFwd, _UsedOnce, _Sub,
		any(Uniq), any(Uniq)).
optimise_inst_keys_in_inst(_OldFwd, _UsedOnce, _Sub, free(A), free(A)).
optimise_inst_keys_in_inst(_OldFwd, _UsedOnce, _Sub, free(A, T), free(A, T)).
optimise_inst_keys_in_inst(OldFwd, UsedOnce, Sub,
		bound(Uniq, Insts0), bound(Uniq, Insts)) :-
	list__map(optimise_inst_keys_in_bound_inst(OldFwd, UsedOnce, Sub),
		Insts0, Insts).
optimise_inst_keys_in_inst(_OldFwd, _UsedOnce, _Sub,
		ground(Uniq, no), ground(Uniq, no)).
optimise_inst_keys_in_inst(_OldFwd, _UsedOnce, _Sub,
		ground(Uniq, yes(Pred)), ground(Uniq, yes(Pred))).
		% ZZZ ERROR
optimise_inst_keys_in_inst(_OldFwd, _UsedOnce, _Sub, not_reached, not_reached).
optimise_inst_keys_in_inst(_OldFwd, _UsedOnce, _Sub, inst_var(V), inst_var(V)).
optimise_inst_keys_in_inst(_OldFwd, _UsedOnce, _Sub,
		defined_inst(Name), defined_inst(Name)).
optimise_inst_keys_in_inst(OldFwd, UsedOnce, Sub,
		abstract_inst(Name, Insts0), abstract_inst(Name, Insts)) :-
	list__map(optimise_inst_keys_in_inst(OldFwd, UsedOnce, Sub),
		Insts0, Insts).

:- pred optimise_inst_keys_in_bound_inst(map(inst_key, inst),
		set_bbbtree(inst_key), inst_key_sub, bound_inst, bound_inst).
:- mode optimise_inst_keys_in_bound_inst(in, in, in, in, out) is det.

optimise_inst_keys_in_bound_inst(OldFwd, UsedOnce, Sub,
	 	functor(Cons, Insts0), functor(Cons, Insts)) :-
	list__map(optimise_inst_keys_in_inst(OldFwd, UsedOnce, Sub),
		Insts0, Insts).

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
%-----------------------------------------------------------------------------%

:- type inst_key_table_mark == inst_key.

inst_key_table_mark_init(-1).

inst_key_table_get_mark(inst_key_table(NextKey, _), NextKey).

inst_key_table_print_since_mark(inst_key_table(_, FwdTable), Mark) -->
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

inst_keys_in_inst(any(_Uniq), Keys, Keys).
inst_keys_in_inst(alias(Key), Keys, [Key | Keys]).
inst_keys_in_inst(free(_), Keys, Keys).
inst_keys_in_inst(free(_, _Type), Keys, Keys).
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

inst_expand_fully(_IKT, _InstMap, any(Uniq), any(Uniq)).
inst_expand_fully(IKT, InstMap, alias(Key), Inst) :-
	instmap__inst_key_table_lookup(InstMap, IKT, Key, Inst0),
	inst_expand_fully(IKT, InstMap, Inst0, Inst).
inst_expand_fully(_IKT, _InstMap, free(A), free(A)).
inst_expand_fully(_IKT, _InstMap, free(A, Type), free(A, Type)).
inst_expand_fully(IKT, InstMap, bound(Uniq, BoundInsts0),
		bound(Uniq, BoundInsts)) :-
	bound_insts_expand_fully(IKT, InstMap, BoundInsts0, BoundInsts).
inst_expand_fully(_IKT, _InstMap, ground(Uniq, PredInstInfo),
		ground(Uniq, PredInstInfo)).
inst_expand_fully(_IKT, _InstMap, not_reached, not_reached).
inst_expand_fully(_IKT, _InstMap, inst_var(Var), inst_var(Var)).
inst_expand_fully(_IKT, _InstMap, defined_inst(InstName),
		defined_inst(InstName)).
inst_expand_fully(IKT, InstMap, abstract_inst(SymName, Insts0),
			abstract_inst(SymName, Insts)) :-
	list__map(inst_expand_fully(IKT, InstMap), Insts0, Insts).

:- pred bound_insts_expand_fully(inst_key_table, instmap, list(bound_inst),
		list(bound_inst)).
:- mode bound_insts_expand_fully(in, in, in, out) is det.

bound_insts_expand_fully(_IKT, _InstMap, [], []).
bound_insts_expand_fully(IKT, InstMap, [functor(ConsId, Insts0) | BIs0],
		[functor(ConsId, Insts) | BIs]) :-
	list__map(inst_expand_fully(IKT, InstMap), Insts0, Insts),
	bound_insts_expand_fully(IKT, InstMap, BIs0, BIs).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

inst_apply_sub(_Sub, any(Uniq), any(Uniq)).
inst_apply_sub(Sub, alias(Key0), Inst) :-
	( map__search(Sub, Key0, Key1) ->
		inst_apply_sub(Sub, alias(Key1), Inst)
	;
		Inst = alias(Key0)
	).
inst_apply_sub(_Sub, free(Aliasing), free(Aliasing)).
inst_apply_sub(_Sub, free(Aliasing, Type), free(Aliasing, Type)).
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

bound_inst_apply_sub(Sub, functor(ConsId, Insts0), functor(ConsId, Insts)) :-
	list__map(inst_apply_sub(Sub), Insts0, Insts).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
