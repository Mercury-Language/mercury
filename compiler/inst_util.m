%-----------------------------------------------------------------------------%
% Copyright (C) 1997-1999 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% file: inst_util.m
% author: fjh
%
% This module defines some utility routines for manipulating insts.
%

/*
The handling of `any' insts is not complete.  (See also inst_match.m)

The major limitation is that we don't allow `free' to be passed
where `any' is expected.  To handle that, modes.m would have to be
changed to handle the implicit conversions from `free' to `any' at

	(1) procedure calls (this is just an extension of implied modes)
	(2) the end of branched goals
	(3) the end of predicates.

Since that is not yet done, we currently require the user to
insert explicit calls to initialize constraint variables.

Another limitation is that we don't allow any unifications between functors
and variables of mode `any'; the reason for that is that I have no
idea what code we should generate for them.  Currently `any' insts
are only used for abstract types, so the type system should prevent
any unification between functors and variables of mode `any'.

Another limitation is that currently code generation assumes that insts
`bound', `ground', and `any' are all represented the same way.
That works fine for the CLP(R) interface but might not be ideal
in the general case.
*/

%-----------------------------------------------------------------------------%

:- module inst_util.
:- interface.

:- import_module hlds_module, hlds_data, prog_data, (inst), instmap.
:- import_module list, map, std_util, set.

:- pred abstractly_unify_inst(is_live, inst, inst, unify_is_real,
		inst_table, module_info, instmap, inst, determinism,
		inst_table, module_info, instmap).
:- mode abstractly_unify_inst(in, in, in, in, in, in, in,
			out, out, out, out, out) is semidet.

	% Compute the inst that results from abstractly unifying two variables.

:- pred abstractly_unify_inst_functor(is_live, inst, cons_id, list(inst),
			list(is_live), unify_is_real, inst_table,
			module_info, instmap, inst, determinism,
			inst_table, module_info, instmap).
:- mode abstractly_unify_inst_functor(in, in, in, in, in, in, in, in, in,
		out, out, out, out, out) is semidet.

	% Compute the inst that results from abstractly unifying
	% a variable with a functor.

	% Mode checking is like abstract interpretation.
	% The above predicates define the abstract unification operation
	% which unifies two instantiatednesses.  If the unification
	% would be illegal, then abstract unification fails.
	% If the unification would fail, then the abstract unification
	% will succeed, and the resulting instantiatedness will be
	% `not_reached'.

%-----------------------------------------------------------------------------%

:- pred make_mostly_uniq_inst(inst, inst_table, module_info, instmap,
		inst, inst_table, module_info, instmap).
:- mode make_mostly_uniq_inst(in, in, in, in, out, out, out, out) is det.

	% Given an inst, return a new inst which is the same as the
	% original inst but with all occurrences of `unique' replaced
	% with `mostly_unique'.

:- pred make_shared_inst_list(list(inst), inst_table, module_info, instmap,
		list(inst), inst_table, module_info, instmap).
:- mode make_shared_inst_list(in, in, in, in, out, out, out, out) is det.

	% Given a list of insts, return a new list of insts which is the
	% same as the original list of insts, but with all occurrences
	% of `unique' replaced with `shared'.  It is an error if any part
	% of the inst list is free.

:- pred make_clobbered_inst(inst, inst_table, module_info, instmap,
		inst, inst_table, module_info, instmap, list(inst_key)).
:- mode make_clobbered_inst(in, in, in, in, out, out, out, out, out) is det.

%-----------------------------------------------------------------------------%
	% Info collected by inst_merge for use in instmap__merge.
:- type merge_info
	--->	merge_info(
			merge_subs,
			ref_counts
		).

	% Inst_key substitutions made when merging pairs of alias() insts.
:- type merge_subs == map(pair(inst_key), inst_key).

	% Reference counts for inst_keys in the two branches.  The first two
	% fields are for references in live variable only.  These are passed
	% in by instmap__merge and not changed within inst_merge.
	% The last two fields are the total number of references among all
	% non-local variables.  The total reference counts are decremented by
	% one whenever a dead variable containing an inst_key reference is
	% clobbered.
:- type ref_counts
	--->	ref_counts(
			inst_key_counts,	% Live count for branch A.
			inst_key_counts,	% Live count for branch B.
			inst_key_counts,	% Total reference count, A.
			inst_key_counts		% Total reference count, B.
		).

:- pred inst_merge(inst, inst, is_live, instmap, inst_table,
		module_info, merge_info, inst, instmap, inst_table,
		module_info, merge_info).
:- mode inst_merge(in, in, in, in, in, in, in, out, out, out, out, out)
		is semidet.

	% inst_merge(InstA, InstB, InstC):
	%       Combine the insts found in different arms of a
	%       disjunction (or if-then-else).
	%       The information in InstC is the minimum of the
	%       information in InstA and InstB.  Where InstA and
	%       InstB specify a binding (free or bound), it must be
	%       the same in both.
	%	Note: inst_merge is designed to be called only from
	%	instmap__merge.  It does not make sense to merge a single pair
	%	of insts in isolation.  In particular, the returned instmap is
	%	not completely correct and needs to be fixed up by
	%	instmap__merge after all insts in the instmap have been merged.

%-----------------------------------------------------------------------------%

	% get_single_arg_inst(Inst, ConsId, Arity, ArgInsts):
	% Given an inst `Inst', figure out what the inst of the
	% argument would be, assuming that the functor is
	% the one given by the specified ConsId, whose arity is 1.
	%
:- pred get_single_arg_inst(inst, instmap, inst_table, module_info, cons_id,
			inst).
:- mode get_single_arg_inst(in, in, in, in, in, out) is det.

%-----------------------------------------------------------------------------%

:- pred inst_table_create_sub(inst_table, inst_table, inst_key_sub, inst_table).
:- mode inst_table_create_sub(in, in, out, out) is det.

%-----------------------------------------------------------------------------%

:- type inst_fold_pred(T) == pred(inst, set(inst_name), T, T).
:- mode inst_fold_pred :: (pred(in, in, in, out) is semidet).

:- type inst_fold_merge_pred(T) == pred(T, T, T).
:- mode inst_fold_merge_pred :: (pred(in, in, out) is det).

%	inst_fold(InstMap, InstTable, ModuleInfo, Before, After, Merge,
%			Inst, T0, T)
% 		Recursively traverse Inst calling Before before recursive
%		calls and After after recursive calls.  Traverses sub-insts
%		of `bound' and argument insts of `abstract_inst' and expands
%		and traverses `alias' and `defined_inst'.
%		Before and After are passed the current sub-inst, the set of
%		previously seen inst_names and the current state of the
%		accumulator; they return the new state of the accumulator.
%		If a call to Before or After fails, the state of the accumulator
%		is passed on unchanged.
%		Merge is called to combine the results of the or-nodes of
%		a `bound' inst.

:- pred inst_fold(instmap, inst_table, module_info, inst_fold_pred(T),
		inst_fold_pred(T), inst_fold_merge_pred(T), inst, T, T).
:- mode inst_fold(in, in, in, inst_fold_pred, inst_fold_pred,
		inst_fold_merge_pred, in, in, out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module hlds_data, inst_match, mode_util, det_analysis.
:- import_module bool, std_util, require, map, list, set, int, assoc_list.

:- pred add_new_keys_to_sub(map(inst_key, inst_key), list(inst_key),
		inst_key, map(inst_key, inst_key)).
:- mode add_new_keys_to_sub(in, in, in, out) is det.

add_new_keys_to_sub(Sub, [], _V, Sub).
add_new_keys_to_sub(Sub0, [K | Ks], V, Sub) :-
	map__set(Sub0, K, V, Sub1),
	add_new_keys_to_sub(Sub1, Ks, V, Sub).

%-----------------------------------------------------------------------------%

:- type unify_inst_info
	--->	unify_inst_info(
			module_info,
			inst_table,
			instmap
		).

:- pred unify_inst_info_get_module_info(unify_inst_info :: in,
		module_info :: out) is det.
unify_inst_info_get_module_info(unify_inst_info(M, _, _), M).

:- pred unify_inst_info_set_module_info(unify_inst_info :: in,
		module_info :: in, unify_inst_info :: out) is det.
unify_inst_info_set_module_info(unify_inst_info(_, B, C), M,
		unify_inst_info(M, B, C)).

:- pred unify_inst_info_get_inst_table(unify_inst_info :: in,
		inst_table :: out) is det.
unify_inst_info_get_inst_table(unify_inst_info(_, InstTable, _), InstTable).

:- pred unify_inst_info_set_inst_table(unify_inst_info :: in,
		inst_table :: in, unify_inst_info :: out) is det.
unify_inst_info_set_inst_table(unify_inst_info(A, _, C), InstTable,
		unify_inst_info(A, InstTable, C)).

:- pred unify_inst_info_get_instmap(unify_inst_info :: in,
		instmap :: out) is det.
unify_inst_info_get_instmap(unify_inst_info(_, _, Instmap), Instmap).

:- pred unify_inst_info_set_instmap(unify_inst_info :: in,
		instmap :: in, unify_inst_info :: out) is det.
unify_inst_info_set_instmap(unify_inst_info(A, B, _), Instmap,
		unify_inst_info(A, B, Instmap)).

	% Abstractly unify two insts.

abstractly_unify_inst(Live, InstA, InstB, UnifyIsReal, InstTable0, M0, S0,
		Inst, Det, InstTable, M, S) :-
	UI0 = unify_inst_info(M0, InstTable0, S0),
	abstractly_unify_inst(Live, InstA, InstB, UnifyIsReal, UI0,
		Inst, Det, UI),
	UI  = unify_inst_info(M, InstTable, S).

:- pred abstractly_unify_inst(is_live, inst, inst, unify_is_real,
		unify_inst_info, inst, determinism, unify_inst_info).
:- mode abstractly_unify_inst(in, in, in, in, in, out, out, out) is semidet.

abstractly_unify_inst(Live, InstA, InstB, UnifyIsReal, UI0, Inst, Det, UI) :-
	UI0 = unify_inst_info(ModuleInfo0, InstTable0, Sub0),
		% check whether this pair of insts is already in
		% the unify_insts table
	ThisInstPair = unify_inst(Live, InstA, InstB, UnifyIsReal),
	inst_table_get_unify_insts(InstTable0, UnifyInsts0),
	( map__search(UnifyInsts0, ThisInstPair, Result) ->
		( Result = known(UnifyInst, UnifyDet) ->
			Inst1 = UnifyInst,
			Det = UnifyDet
		;
			Inst1 = defined_inst(ThisInstPair),
				% It's ok to assume that the unification is
				% deterministic here, because the only time that
				% this will happen is when we get to the
				% recursive case for a recursively defined inst.
				% If the unification as a whole is semidet then
				% it must be semidet somewhere else too.
			Det = det
		),
		UI = UI0
	;
			% insert ThisInstPair into the table with value
			% `unknown'
		map__det_insert(UnifyInsts0, ThisInstPair, unknown,
			UnifyInsts1),
		inst_table_set_unify_insts(InstTable0, UnifyInsts1, InstTable1),
			% unify the insts
		UI1 = unify_inst_info(ModuleInfo0, InstTable1, Sub0),
		abstractly_unify_inst_2(Live, UnifyIsReal, InstA, InstB,
			UI1, Inst0, Det, UI2),
		UI2 = unify_inst_info(ModuleInfo2, InstTable2, Sub2),

			% If this unification cannot possible succeed,
			% the correct inst is not_reached.
		(
			determinism_components(Det, _, at_most_zero)
		->
			Inst1 = not_reached
		;
			Inst1 = Inst0
		),

			% now update the value associated with ThisInstPair
		inst_table_get_unify_insts(InstTable2, UnifyInsts2),
		map__det_update(UnifyInsts2, ThisInstPair,
			known(Inst1, Det), UnifyInsts),
		inst_table_set_unify_insts(InstTable2, UnifyInsts, InstTable),
		UI = unify_inst_info(ModuleInfo2, InstTable, Sub2)
	),
	unify_inst_info_get_module_info(UI, LastModuleInfo),
	unify_inst_info_get_inst_table(UI, LastInstTable),
	unify_inst_info_get_instmap(UI, LastInstMap),

		% avoid expanding recursive insts
	( inst_contains_instname(Inst1, LastInstMap, LastInstTable,
			LastModuleInfo, ThisInstPair) ->
		Inst = defined_inst(ThisInstPair)
	;
		Inst = Inst1
	).

:- pred abstractly_unify_inst_2(is_live, unify_is_real, inst, inst,
			unify_inst_info, inst, determinism, unify_inst_info).
:- mode abstractly_unify_inst_2(in, in, in, in, in, out, out, out) is semidet.

abstractly_unify_inst_2(IsLive, Real, InstA, InstB, UI0, Inst, Det, UI) :-
	unify_inst_info_get_module_info(UI0, ModuleInfo0),
	unify_inst_info_get_instmap(UI0, InstMap0),
	unify_inst_info_get_inst_table(UI0, InstTable0),
	inst_expand_defined_inst(InstTable0, ModuleInfo0, InstA, InstA2),
	inst_expand_defined_inst(InstTable0, ModuleInfo0, InstB, InstB2),
	(
		InstB2 = not_reached
	->
		Inst = not_reached,
		Det = det,
		UI = UI0
	;
		( InstA2 = alias(_) ; InstB2 = alias(_) )
	->
		% Optimise common cases

		(
			% free = alias(K) where alias(K) is ground

			( InstA2 = free(_) ; InstA2 = free(_, _) ),
			inst_is_ground(InstB2, InstMap0, InstTable0,
					ModuleInfo0)
		->
			UI = UI0,
			Inst = InstB2, Det = det
		;
			% alias(K) = free where alias(K) is ground

			( InstB2 = free(_) ; InstB2 = free(_, _) ),
			inst_is_ground(InstA2, InstMap0, InstTable0,
					ModuleInfo0)
		->
			UI = UI0,
			Inst = InstA2, Det = det
		;
			% alias(K) = alias(K)

			InstA2 = InstB2
		->
			UI = UI0,
			Inst = InstA2, Det = det
		;
			% At least one inst is an alias, so we must preserve
			% this.

			unify_inst_info_get_instmap(UI0, InstMap0),
			inst_table_get_inst_key_table(InstTable0, IKT0),
			( InstA2 = alias(KeyA) ->
				instmap__inst_key_table_lookup(InstMap0, IKT0,
						KeyA, InstA3),
				KeysToUpdate0 = [KeyA]
			;
				InstA3 = InstA2,
				KeysToUpdate0 = []
			),
			( InstB2 = alias(KeyB) ->
				instmap__inst_key_table_lookup(InstMap0, IKT0,
						KeyB, InstB3),
				KeysToUpdate = [KeyB | KeysToUpdate0]
			;
				InstB3 = InstB2,
				KeysToUpdate = KeysToUpdate0
			), 

			abstractly_unify_inst_2(IsLive, Real, InstA3, InstB3,
				UI0, Inst0, Det, UI1),

			% Optimise some more common cases
			unify_inst_info_get_instmap(UI1, InstMap1),
			(
				% If the unified inst is the same as
				% InstA and InstA was aliased, don't
				% bother allocating a new inst_key.
				% Reuse the old one instead.

				InstA2 = alias(KA), InstA3 = Inst0
			->
				Inst = InstA2,
				( InstB2 = alias(KB) ->
					instmap__add_alias(InstMap1, KB, KA,
						InstMap)
				;
					InstMap = InstMap1
				),
				unify_inst_info_set_instmap(UI1, InstMap, UI)
			;
				% If the unified inst is the same as
				% InstB and InstB was aliased, don't
				% bother allocating a new inst_key.
				% Reuse the old one instead. 

				InstB2 = alias(KB), InstB3 = Inst0
			->
				Inst = InstB2,
				( InstA2 = alias(KA) ->
					instmap__add_alias(InstMap1, KA, KB,
						InstMap)
				;
					InstMap = InstMap1
				),
				unify_inst_info_set_instmap(UI1, InstMap, UI)
			;
				unify_inst_info_get_inst_table(UI1, InstTable1),
				inst_table_get_inst_key_table(InstTable1, IKT1),
				inst_key_table_add(IKT1, Inst0, NewKey, IKT),
				Inst = alias(NewKey),
				inst_table_set_inst_key_table(InstTable1, IKT,
					InstTable),
				unify_inst_info_set_inst_table(UI1, InstTable,
					UI2),
				list__foldl(lambda([K::in, IM0::in, IM::out]
							is det,
					instmap__add_alias(IM0, K, NewKey, IM)
					), KeysToUpdate, InstMap1, InstMap),
				unify_inst_info_set_instmap(UI2, InstMap, UI)
			)
		)
	;
		abstractly_unify_inst_3(IsLive, Real, InstA2, InstB2,
			UI0, Inst, Det, UI)
	).


	% Abstractly unify two expanded insts.
	% The is_live parameter is `live' iff *both* insts are live.
	% Given the two insts to be unified, this produces
	% a resulting inst and a determinism for the unification.

:- pred abstractly_unify_inst_3(is_live, unify_is_real, inst, inst,
			unify_inst_info, inst, determinism, unify_inst_info).
:- mode abstractly_unify_inst_3(in, in, in, in, in, out, out, out) is semidet.

% XXX could be extended to handle `any' insts better

abstractly_unify_inst_3(live, _, not_reached, _,
					UI, not_reached, det, UI).

abstractly_unify_inst_3(live, Real, any(Uniq), Inst0, UI0, Inst, Det, UI) :-
	make_any_inst(Inst0, live, Uniq, Real, UI0, Inst, Det, UI).

abstractly_unify_inst_3(live, Real, free(_), any(UniqY), UI,
					any(Uniq), det, UI) :-
	unify_uniq(live, Real, det, unique, UniqY, Uniq).

abstractly_unify_inst_3(live, _Real, free(A0), free(A1), UI,
					free(A), det, UI) :-
	unify_free_free_alias(live, A0, A1, A).

abstractly_unify_inst_3(live, Real, free(_),	bound(UniqY, List0), UI0,
						bound(Uniq, List), det, UI) :-
	unify_uniq(live, Real, det, unique, UniqY, Uniq),

		% since both are live, we must disallow free-free unifications
	unify_inst_info_get_module_info(UI0, M0),
	unify_inst_info_get_inst_table(UI0, InstTable0),
	unify_inst_info_get_instmap(UI0, InstMap0),
	bound_inst_list_is_ground_or_any(List0, InstMap0, InstTable0, M0),

		% since both are live, we must make the result shared
		% (unless it was already shared)
	( ( UniqY = unique ; UniqY = mostly_unique ) ->
		make_shared_bound_inst_list(List0, UI0, List, UI, yes)
	;
		List = List0, UI = UI0
	).

abstractly_unify_inst_3(live, Real, free(_),   ground(UniqY, PredInst), UI,
					    ground(Uniq, PredInst), det, UI) :-
	unify_uniq(live, Real, det, unique, UniqY, Uniq).

% abstractly_unify_inst_3(live, _, free, abstract_inst(_,_), _, _, _, _) :- fail.

abstractly_unify_inst_3(live, Real, bound(UniqX, List0), any(UniqY),  UI0,
					bound(Uniq, List), Det, UI) :-
	allow_unify_bound_any(Real),
	unify_uniq(live, Real, semidet, UniqX, UniqY, Uniq),
	make_any_bound_inst_list(List0, live, UniqY, Real, UI0,
			List, Det1, UI),
	det_par_conjunction_detism(Det1, semidet, Det).

abstractly_unify_inst_3(live, Real,	bound(UniqY, List0), free(_), UI0,
					bound(Uniq, List), det,  UI) :-
	unify_uniq(live, Real, det, unique, UniqY, Uniq),
		% since both are live, we must disallow free-free unifications
	unify_inst_info_get_module_info(UI0, M0),
	unify_inst_info_get_inst_table(UI0, InstTable0),
	unify_inst_info_get_instmap(UI0, InstMap0),
	bound_inst_list_is_ground_or_any(List0, InstMap0, InstTable0, M0),
	make_shared_bound_inst_list(List0, UI0, List, UI, yes).

abstractly_unify_inst_3(live, Real, bound(UniqX, ListX), bound(UniqY, ListY),
			UI0,     bound(Uniq, List), Det, UI) :-
	abstractly_unify_bound_inst_list(live, ListX, ListY, Real, UI0,
		List, Det, UI),
	unify_uniq(live, Real, Det, UniqX, UniqY, Uniq).

abstractly_unify_inst_3(live, Real, bound(UniqX, BoundInsts0), ground(UniqY, _),
		UI0, bound(Uniq, BoundInsts), Det, UI) :-
	unify_uniq(live, Real, semidet, UniqX, UniqY, Uniq),
	make_ground_bound_inst_list(BoundInsts0, live, UniqY, Real, UI0,
			BoundInsts, Det1, UI),
	det_par_conjunction_detism(Det1, semidet, Det).

/*** abstract insts not supported
abstractly_unify_inst_3(live, Real, bound(Uniq, List), abstract_inst(_,_), UI,
					ground(shared), semidet, UI) :-
	unify_inst_info_get_module_info(UI, M),
	unify_inst_info_get_inst_table(UI, InstTable),
	unify_uniq(live, Real, semidet, unique, UniqY, Uniq),
	bound_inst_list_is_ground(InstTable, List, M).
***/

abstractly_unify_inst_3(live, Real, ground(UniqX, yes(PredInst)), any(UniqY),
			UI,	 ground(Uniq, yes(PredInst)), semidet, UI) :-
	Real = fake_unify,
	unify_uniq(live, Real, det, UniqX, UniqY, Uniq).

abstractly_unify_inst_3(live, Real,  ground(Uniq0, yes(PredInst)), free(_), UI,
				     ground(Uniq, yes(PredInst)), det, UI) :-
	unify_uniq(live, Real, det, unique, Uniq0, Uniq).

abstractly_unify_inst_3(live, Real, ground(UniqX, yes(_)),
			bound(UniqY, BoundInsts0), UI0,
			bound(Uniq, BoundInsts), Det, UI) :-
	% check `Real = fake_unify' ?
	unify_uniq(live, Real, semidet, UniqX, UniqY, Uniq),
	make_ground_bound_inst_list(BoundInsts0, live, UniqX, Real, UI0,
			BoundInsts, Det1, UI),
	det_par_conjunction_detism(Det1, semidet, Det).

abstractly_unify_inst_3(live, Real, ground(UniqA, yes(PredInstA)),
				ground(UniqB, _MaybePredInstB), UI,
				ground(Uniq, PredInst), semidet, UI) :-
	% It is an error to unify higher-order preds,
	% so if it's a real_unify, then we must fail.
	Real \= real_unify,
	% In theory we should choose take the union of the
	% information specified by PredInstA and _MaybePredInstB.
	% However, since our data representation provides no
	% way of doing that, and since this will only happen
	% for fake_unifys, for which it shouldn't make any difference,
	% we just choose the information specified by PredInstA.
	PredInst = yes(PredInstA),
	unify_uniq(live, Real, semidet, UniqA, UniqB, Uniq).

abstractly_unify_inst_3(live, Real, ground(Uniq, no), Inst0, UI0,
				Inst, Det, UI) :-
	unify_inst_info_get_inst_table(UI0, InstTable0),
	unify_inst_info_get_module_info(UI0, M0),
	unify_inst_info_get_instmap(UI0, InstMap0),
	( inst_is_free(Inst0, InstMap0, InstTable0, M0) ->
		Det = det
	;
		Det = semidet
	),
	make_ground_inst(Inst0, live, Uniq, Real, UI0, Inst, Det, UI).

% abstractly_unify_inst_3(live, _, abstract_inst(_,_), free,     _, _, _, _, _)
%       :- fail.

/*** abstract insts not supported
abstractly_unify_inst_3(live, Real, abstract_inst(_,_), bound(Uniq, List),
		UI, ground(shared, no), semidet, UI) :-
	check_not_clobbered(Real, Uniq),
	unify_inst_info_get_inst_table(UI, InstTable),
	unify_inst_info_get_module_info(UI, M),
	bound_inst_list_is_ground(InstTable, List, ModuleInfo).

abstractly_unify_inst_3(live, Real, abstract_inst(_,_), ground(Uniq, no), UI,
				ground(shared, no), semidet, UI) :-
	check_not_clobbered(Real, Uniq).

abstractly_unify_inst_3(live, Real, abstract_inst(Name, ArgsA),
			abstract_inst(Name, ArgsB), UI0,
			abstract_inst(Name, Args), Det, UI) :-
	abstractly_unify_inst_list(ArgsA, ArgsB, live, Real, UI0,
		Args, Det, UI).
***/

abstractly_unify_inst_3(dead, _Real, not_reached, _, UI, not_reached, det, UI).

abstractly_unify_inst_3(dead, Real, any(Uniq), Inst0, UI0, Inst, Det, UI) :-
	make_any_inst(Inst0, dead, Uniq, Real, UI0, Inst, Det, UI).

abstractly_unify_inst_3(dead, _Real, free(A0), Inst1, UI, Inst, det, UI) :-
	( Inst1 = free(A1) ->
		unify_free_free_alias(dead, A0, A1, A),
		Inst = free(A)
	;
		Inst = Inst1
	).

abstractly_unify_inst_3(dead, Real, bound(UniqX, List0), any(UniqY), UI0,
					bound(Uniq, List), Det, UI) :-
	allow_unify_bound_any(Real),
	unify_uniq(dead, Real, semidet, UniqX, UniqY, Uniq),
	make_any_bound_inst_list(List0, live, UniqY, Real, UI0,
					List, Det1, UI),
	det_par_conjunction_detism(Det1, semidet, Det).

abstractly_unify_inst_3(dead, Real, bound(UniqX, List), free(_), UI,
				bound(Uniq, List), det, UI) :-
	unify_uniq(dead, Real, det, UniqX, unique, Uniq).

abstractly_unify_inst_3(dead, Real, bound(UniqX, ListX), bound(UniqY, ListY),
			UI0, bound(Uniq, List), Det, UI) :-
	abstractly_unify_bound_inst_list(dead, ListX, ListY, Real, UI0,
		List, Det, UI),
	unify_uniq(dead, Real, Det, UniqX, UniqY, Uniq).

abstractly_unify_inst_3(dead, Real, bound(UniqX, BoundInsts0), ground(UniqY, _),
			UI0, bound(Uniq, BoundInsts), Det, UI) :-
	unify_uniq(dead, Real, semidet, UniqX, UniqY, Uniq),
	make_ground_bound_inst_list(BoundInsts0, dead, UniqY, Real, UI0,
					BoundInsts, Det1, UI),
	det_par_conjunction_detism(Det1, semidet, Det).

/***** abstract insts aren't really supported
abstractly_unify_inst_3(dead, _Real, bound(Uniq, List), abstract_inst(N,As),
			UI, Result, Det, UI) :-
	unify_inst_info_get_module_info(UI, M),
	unify_inst_info_get_inst_table(UI, InstTable),
	( bound_inst_list_is_ground(List, InstTable, M) ->
		Result = bound(Uniq, List),
		Det = semidet
	; bound_inst_list_is_free(List, InstTable, M) ->
		Result = abstract_inst(N,As),
		Det = det
	;
		fail
	).
*****/

abstractly_unify_inst_3(dead, Real, ground(UniqX, yes(PredInst)), any(UniqY),
			UI,	 ground(Uniq, yes(PredInst)), semidet, UI) :-
	allow_unify_bound_any(Real),
	unify_uniq(dead, Real, semidet, UniqX, UniqY, Uniq).

abstractly_unify_inst_3(dead, _Real, ground(Uniq, yes(PredInst)), free(_), UI,
				ground(Uniq, yes(PredInst)), det, UI).

abstractly_unify_inst_3(dead, Real, ground(UniqA, yes(_)),
			bound(UniqB, BoundInsts0), UI0,
			bound(Uniq, BoundInsts), Det, UI) :-
	unify_uniq(dead, Real, semidet, UniqA, UniqB, Uniq),
	make_ground_bound_inst_list(BoundInsts0, dead, UniqA, Real, UI0,
					BoundInsts, Det1, UI),
	det_par_conjunction_detism(Det1, semidet, Det).

abstractly_unify_inst_3(dead, Real, ground(UniqA, yes(PredInstA)),
				ground(UniqB, _MaybePredInstB), UI,
				ground(Uniq, PredInst), det, UI) :-
	Real = fake_unify,
	PredInst = yes(PredInstA),
	unify_uniq(dead, Real, det, UniqA, UniqB, Uniq).

abstractly_unify_inst_3(dead, Real, ground(Uniq, no), Inst0, UI0,
				Inst, Det, UI) :-
	make_ground_inst(Inst0, dead, Uniq, Real, UI0, Inst, Det, UI).

/***** abstract insts aren't really supported
abstractly_unify_inst_3(dead, _Real, abstract_inst(N,As), bound(List), 
			UI, Result, Det, UI) :-
	unify_inst_info_get_module_info(UI, M),
	unify_inst_info_get_inst_table(UI, InstTable),

	( bound_inst_list_is_ground(InstTable, List, M) ->
		Result = bound(List),
		Det = semidet
	; bound_inst_list_is_free(InstTable, List, M) ->
		Result = abstract_inst(N,As),
		Det = det
	;
		fail
	).

abstractly_unify_inst_3(dead, _Real, abstract_inst(_,_), ground, UI,
		ground, semidet, UI).

abstractly_unify_inst_3(dead, Real, abstract_inst(Name, ArgsA),
			abstract_inst(Name, ArgsB), UI0,
			abstract_inst(Name, Args), Det, UI) :-
	abstractly_unify_inst_list(ArgsA, ArgsB, dead, Real, UI0,
			Args, Det, UI).

*****/

%-----------------------------------------------------------------------------%

	% Abstractly unify two inst lists.

:- pred abstractly_unify_inst_list(list(inst), list(inst), is_live,
		unify_is_real,
		unify_inst_info, list(inst), determinism, unify_inst_info).
:- mode abstractly_unify_inst_list(in, in, in, in, in,
		out, out, out) is semidet.

abstractly_unify_inst_list([], [], _, _, UI, [], det, UI).
abstractly_unify_inst_list([X|Xs], [Y|Ys], Live, Real, UI0, [Z|Zs], Det, UI) :-
	abstractly_unify_inst(Live, X, Y, Real, UI0, Z, Det1, UI1),
	abstractly_unify_inst_list(Xs, Ys, Live, Real, UI1, Zs, Det2, UI),
	det_par_conjunction_detism(Det1, Det2, Det).

%-----------------------------------------------------------------------------%

	% This is the abstract unification operation which
	% unifies a variable (or rather, it's instantiatedness)
	% with a functor.

abstractly_unify_inst_functor(Live, InstA, ConsId, ArgInsts, ArgLives, Real,
		InstTable0, ModuleInfo0, InstMap0, Inst, Det, InstTable,
		ModuleInfo, InstMap) :-
	inst_expand_defined_inst(InstTable0, ModuleInfo0, InstA, InstA2),

	( InstA2 = alias(KeyA) ->
		inst_table_get_inst_key_table(InstTable0, IKT0),
		instmap__inst_key_table_lookup(InstMap0, IKT0, KeyA, InstA3),

		abstractly_unify_inst_functor(Live, InstA3, ConsId, ArgInsts,
			ArgLives, Real, InstTable0, ModuleInfo0, InstMap0,
			Inst0, Det, InstTable1, ModuleInfo, InstMap1),

		( determinism_components(Det, _, at_most_zero) ->
			Inst = not_reached,
			InstMap = InstMap1,
			InstTable = InstTable1
		;
			inst_table_get_inst_key_table(InstTable1, IKT1),
			inst_key_table_add(IKT1, Inst0, NewKey, IKT),
			inst_table_set_inst_key_table(InstTable1, IKT,
				InstTable),
			Inst = alias(NewKey),
			instmap__add_alias(InstMap1, KeyA, NewKey, InstMap)
		)
	;
		UI0 = unify_inst_info(ModuleInfo0, InstTable0, InstMap0),
		abstractly_unify_inst_functor_2(Live, Real, InstA2, ConsId,
			ArgInsts, ArgLives, UI0, Inst0, Det, UI),
		UI = unify_inst_info(ModuleInfo, InstTable, InstMap),
		( determinism_components(Det, _, at_most_zero) ->
			Inst = not_reached
		;
			Inst = Inst0
		)
	).

:- pred abstractly_unify_inst_functor_2(is_live, unify_is_real, inst, cons_id,
		list(inst), list(is_live), unify_inst_info,
		inst, determinism, unify_inst_info).
:- mode abstractly_unify_inst_functor_2(in, in, in, in, in, in, in,
			out, out, out) is semidet.

	% XXX need to handle `any' insts

abstractly_unify_inst_functor_2(live, _, not_reached, _, _, _, UI,
			not_reached, erroneous, UI).

abstractly_unify_inst_functor_2(live, Real, free(_), ConsId, Args0, ArgLives,
			UI0, bound(unique, [functor(ConsId, Args)]), det, UI) :-
	assoc_list__from_corresponding_lists(Args0, ArgLives, ArgsAndLives),
	abstractly_unify_bound_inst_list_with_free(Real, ArgsAndLives, Args,
			UI0, UI).

abstractly_unify_inst_functor_2(live, Real, bound(Uniq, ListX), ConsId, Args,
			ArgLives, UI0, bound(Uniq, List), Det, UI) :-
	abstractly_unify_bound_inst_list_lives(ListX, ConsId, Args, ArgLives,
					Real, UI0, List, Det0, UI),
		% Determine if there is a tag test required to factor
		% into the determinism.
	( ListX = [_, _ | _] ->
		det_par_conjunction_detism(Det0, semidet, Det)
	;
		Det = Det0
	).

abstractly_unify_inst_functor_2(live, Real, ground(Uniq, _), ConsId, ArgInsts,
		ArgLives, UI0, Inst, Det, UI) :-
	make_ground_inst_list_lives(ArgInsts, live, ArgLives, Uniq, Real, UI0,
		GroundArgInsts, Det0, UI),
	Inst = bound(Uniq, [functor(ConsId, GroundArgInsts)]),
	( Real = real_unify,
		det_par_conjunction_detism(Det0, semidet, Det)
	; Real = fake_unify,
		Det = Det0
	).

% abstractly_unify_inst_functor_2(live, _, abstract_inst(_,_), _, _, _, _,
%		_, _, _) :-
%       fail.

abstractly_unify_inst_functor_2(dead, _, not_reached, _, _, _, UI,
					not_reached, erroneous, UI).

abstractly_unify_inst_functor_2(dead, _Real, free(_), ConsId, Args, _ArgLives,
			UI, bound(unique, [functor(ConsId, Args)]), det, UI).

abstractly_unify_inst_functor_2(dead, Real, bound(Uniq, ListX), ConsId, Args,
			_ArgLives, UI0, bound(Uniq, List), Det, UI) :-
	ListY = [functor(ConsId, Args)],
	abstractly_unify_bound_inst_list(dead, ListX, ListY, Real, UI0,
		List, Det, UI).

abstractly_unify_inst_functor_2(dead, Real, ground(Uniq, _), ConsId, ArgInsts,
		_ArgLives, UI0, Inst, Det, UI) :-
	make_ground_inst_list(ArgInsts, dead, Uniq, Real, UI0, GroundArgInsts,
		Det0, UI),
	Inst = bound(Uniq, [functor(ConsId, GroundArgInsts)]),
	( Real = real_unify,
		det_par_conjunction_detism(Det0, semidet, Det)
	; Real = fake_unify,
		Det = Det0
	).

% abstractly_unify_inst_functor_2(dead, _, abstract_inst(_,_), _, _, _, _,
%		_, _, _) :-
%       fail.

%-----------------------------------------------------------------------------%

	% This code performs abstract unification of two bound(...) insts.
	% like a sorted merge operation.  If two elements have the
	% The lists of bound_inst are guaranteed to be sorted.
	% Abstract unification of two bound(...) insts proceeds
	% like a sorted merge operation.  If two elements have the
	% same functor name, they are inserted in the output list,
	% assuming their argument inst list can be abstractly unified.
	% (If it can't, the whole thing fails).  If a functor name
	% occurs in only one of the two input lists, it is not inserted
	% in the output list.

:- pred abstractly_unify_bound_inst_list(is_live, list(bound_inst),
		list(bound_inst), unify_is_real, unify_inst_info,
		list(bound_inst), determinism, unify_inst_info).
:- mode abstractly_unify_bound_inst_list(in, in, in, in, in,
		out, out, out) is semidet.

abstractly_unify_bound_inst_list(Live, Xs, Ys, Real, UnifyInstInfo0,
			L, Det, UnifyInstInfo) :-
	abstractly_unify_bound_inst_list_2(Live, Xs, Ys, Real,
		UnifyInstInfo0, 0, L, Det0, UnifyInstInfo),
	( L = [] ->
		det_par_conjunction_detism(Det0, erroneous, Det)
	;
		Det = Det0
	).

:- pred abstractly_unify_bound_inst_list_2(is_live, list(bound_inst),
		list(bound_inst), unify_is_real, unify_inst_info,
		int, list(bound_inst), determinism, unify_inst_info).
:- mode abstractly_unify_bound_inst_list_2(in, in, in, in, in, in,
		out, out, out) is semidet.

abstractly_unify_bound_inst_list_2(_, [], [], _, UI, N, [], Det, UI) :-
	(
			% The only time an abstract unification should
			% be det, is when both of the bound_inst lists
			% are of length one and have the same cons_ids.
			%
			% If N=0, we need to make the determinism det
			% so that determinism is inferred as erroneous
			% rather then failure in 
			% abstractly_unify_bound_inst_list
		N =< 1
	->
		Det = det
	;
		Det = semidet
	).
abstractly_unify_bound_inst_list_2(_, [], [_|_], _, UI, _, [], semidet, UI).
abstractly_unify_bound_inst_list_2(_, [_|_], [], _, UI, _, [], semidet, UI).
abstractly_unify_bound_inst_list_2(Live, [X|Xs], [Y|Ys], Real, UI0,
		N, L, Det, UI) :-
	X = functor(ConsIdX, ArgsX),
	Y = functor(ConsIdY, ArgsY),
	( ConsIdX = ConsIdY ->
		abstractly_unify_inst_list(ArgsX, ArgsY, Live, Real,
			UI0, Args, Det1, UI1),
		abstractly_unify_bound_inst_list_2(Live, Xs, Ys, Real,
				UI1, N+1, L1, Det2, UI),

		% If the unification of the two cons_ids is guaranteed
		% not to succeed, don't include it in the list.
		( determinism_components(Det1, _, at_most_zero) ->
			L = L1,
			Det = Det2
		;
			L = [functor(ConsIdX, Args) | L1],
			det_par_conjunction_detism(Det1, Det2, Det)
		)
	;
		( compare(<, ConsIdX, ConsIdY) ->
			abstractly_unify_bound_inst_list_2(Live,
				Xs, [Y|Ys], Real, UI0, N+1, L, Det1, UI)
		;
			abstractly_unify_bound_inst_list_2(Live,
				[X|Xs], Ys, Real, UI0, N+1, L, Det1, UI)
		),
		det_par_conjunction_detism(Det1, semidet, Det)
	).

:- pred abstractly_unify_bound_inst_list_lives(list(bound_inst), cons_id,
		list(inst), list(is_live), unify_is_real, unify_inst_info,
		list(bound_inst), determinism, unify_inst_info).
:- mode abstractly_unify_bound_inst_list_lives(in, in, in, in, in, in,
		out, out, out) is semidet.

abstractly_unify_bound_inst_list_lives([], _, _, _, _, UI,
					[], failure, UI).
abstractly_unify_bound_inst_list_lives([X|Xs], ConsIdY, ArgsY, LivesY, Real,
		UI0, L, Det, UI) :-
	X = functor(ConsIdX, ArgsX),
	(
		ConsIdX = ConsIdY
	->
		abstractly_unify_inst_list_lives(ArgsX, ArgsY, LivesY,
				Real, UI0, Args, Det, UI),
		L = [functor(ConsIdX, Args)]
	;
		abstractly_unify_bound_inst_list_lives(Xs, ConsIdY, ArgsY,
				LivesY, Real, UI0, L, Det, UI)
	).

:- pred abstractly_unify_inst_list_lives(list(inst), list(inst), list(is_live),
		unify_is_real, unify_inst_info, list(inst),
		determinism, unify_inst_info).
:- mode abstractly_unify_inst_list_lives(in, in, in, in, in,
		out, out, out) is semidet.

abstractly_unify_inst_list_lives([], [], [], _, UI, [], det, UI).
abstractly_unify_inst_list_lives([X|Xs], [Y|Ys], [Live|Lives],
		Real, UI0, [Z|Zs], Det, UI) :-
	abstractly_unify_inst(Live, X, Y, Real, UI0, Z, Det1, UI1),
	abstractly_unify_inst_list_lives(Xs, Ys, Lives, Real, UI1, Zs, Det2,
		UI),
	det_par_conjunction_detism(Det1, Det2, Det).

%-----------------------------------------------------------------------------%

:- pred abstractly_unify_bound_inst_list_with_free(unify_is_real,
		assoc_list(inst, is_live), list(inst),
		unify_inst_info, unify_inst_info).
:- mode abstractly_unify_bound_inst_list_with_free(in, in, out,
		in, out) is semidet.

abstractly_unify_bound_inst_list_with_free(_, [], []) --> [].
abstractly_unify_bound_inst_list_with_free(UnifyIsReal, [InstLive | InstLives],
			[Inst | Insts]) -->
	abstractly_unify_bound_inst_arg_with_free(UnifyIsReal, InstLive, Inst),
	abstractly_unify_bound_inst_list_with_free(UnifyIsReal, InstLives,
			Insts).

:- pred abstractly_unify_bound_inst_arg_with_free(unify_is_real,
		pair(inst, is_live), inst, unify_inst_info, unify_inst_info).
:- mode abstractly_unify_bound_inst_arg_with_free(in, in, out,
		in, out) is semidet.

abstractly_unify_bound_inst_arg_with_free(_, Inst - dead, Inst, UI, UI).

abstractly_unify_bound_inst_arg_with_free(UnifyIsReal, Inst0 - live, Inst,
		UI0, UI) :-
	unify_inst_info_get_module_info(UI0, ModuleInfo),
	unify_inst_info_get_inst_table(UI0, InstTable0),
	unify_inst_info_get_instmap(UI0, InstMap0),
	(
		inst_is_free(Inst0, InstMap0, InstTable0, ModuleInfo),
		\+ inst_is_free_alias(Inst0, InstMap0, InstTable0, ModuleInfo)
	->
		abstractly_unify_inst(live, Inst0, free(alias), UnifyIsReal,
			UI0, Inst, _Det, UI)
	;
		Inst = Inst0,
		UI = UI0
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred unify_free_free_alias(is_live, aliasing, aliasing, aliasing).
:- mode unify_free_free_alias(in, in, in, out) is semidet.

% unify_free_free_alias(live, unique, unique, _) :- fail.	% AAA
unify_free_free_alias(live, unique, alias, alias).
unify_free_free_alias(live, alias, unique, alias).
unify_free_free_alias(live, alias, alias, _) :-
	error("unify_free_free_alias: live free(alias) = free(alias) NYI").
unify_free_free_alias(dead, unique, unique, unique).
unify_free_free_alias(dead, unique, alias, alias).
unify_free_free_alias(dead, alias, unique, alias).
unify_free_free_alias(dead, alias, alias, alias).

%-----------------------------------------------------------------------------%

:- pred unify_uniq(is_live, unify_is_real, determinism, uniqueness, uniqueness,
		uniqueness).
:- mode unify_uniq(in, in, in, in, in, out) is semidet.

	% Unifying shared with either shared or unique gives shared.
	% Unifying unique with unique gives shared if live, unique if
	% dead.  Unifying clobbered with anything gives clobbered,
	% except that if live then it is an internal error (a clobbered
	% value should not be live, right?), and except that unifying
	% with clobbered is not allowed for semidet unifications,
	% unless they are "fake".
	%
	% The only way this predicate can abort is if a clobbered value
	% is live.
	% The only way this predicate can fail (indicating a unique mode error)
	% is if we are attempting to unify with a clobbered value, and
	% this was a "real" unification, not a "fake" one,
	% and the determinism of the unification is semidet.
	% (See comment in prog_data.m for more info on "real" v.s. "fake".)
	% Note that if a unification or sub-unification is det, then it is
	% OK to unify with a clobbered value.  This can occur e.g. with
	% unifications between free and clobbered, or with free and
	% bound(..., clobbered, ...).  Such det unifications are OK because
	% the clobbered value will not be examined, instead all that will
	% happen is that a variable or a field of a variable will become
	% bound to the clobbered value; and since the final inst will also
	% be clobbered, the variable or field's value can never be examined
	% later either.  Only semidet unifications would test the value
	% of a clobbered variable, so those are the only ones we need to
	% disallow.

unify_uniq(_,      _, _,       shared,   shared,	    shared).
unify_uniq(_,      _, _,       shared,   unique,	    shared).
unify_uniq(_,      _, _,       shared,   mostly_unique,     shared).
unify_uniq(Live,   Real, Det,  shared,   clobbered,	 clobbered) :-
	allow_unify_with_clobbered(Live, Real, Det).
unify_uniq(Live,   Real, Det,  shared,   mostly_clobbered,  mostly_clobbered) :-
	allow_unify_with_clobbered(Live, Real, Det).

unify_uniq(_,      _, _,       unique,   shared,	    shared).
unify_uniq(live,   _, _,       unique,   unique,	    unique).
unify_uniq(live,   _, _,       unique,   mostly_unique,     mostly_unique).
unify_uniq(dead,   _, _,       unique,   unique,	    unique).
unify_uniq(dead,   _, _,       unique,   mostly_unique,     mostly_unique).
		% XXX the above line is a conservative approximation
		% sometimes it should return unique not mostly_unique
unify_uniq(Live,   Real, Det,  unique,   clobbered,	 clobbered) :-
	allow_unify_with_clobbered(Live, Real, Det).
unify_uniq(Live,   Real, Det,  unique,   mostly_clobbered,  mostly_clobbered) :-
	allow_unify_with_clobbered(Live, Real, Det).

unify_uniq(_,      _, _,       mostly_unique,    shared,    shared).
unify_uniq(live,   _, _,       mostly_unique,    unique,    mostly_unique).
unify_uniq(live,   _, _,       mostly_unique,    mostly_unique, mostly_unique).
unify_uniq(dead,   _, _,       mostly_unique,    unique,    mostly_unique).
		% XXX the above line is a conservative approximation
		% sometimes it should return unique not mostly_unique
unify_uniq(dead,   _, _,       mostly_unique,    mostly_unique, mostly_unique).
unify_uniq(Live,   Real, Det,  mostly_unique,    clobbered, clobbered) :-
	allow_unify_with_clobbered(Live, Real, Det).
unify_uniq(Live,   Real, Det,  mostly_unique,    mostly_clobbered,
							    mostly_clobbered) :-
	allow_unify_with_clobbered(Live, Real, Det).

unify_uniq(Live,   Real, Det,  clobbered,	_,	 clobbered) :-
	allow_unify_with_clobbered(Live, Real, Det).

unify_uniq(Live,   Real, Det,  mostly_clobbered, Uniq0,     Uniq) :-
	( Uniq0 = clobbered -> Uniq = clobbered ; Uniq = mostly_clobbered ),
	allow_unify_with_clobbered(Live, Real, Det).

:- pred allow_unify_with_clobbered(is_live, unify_is_real, determinism).
:- mode allow_unify_with_clobbered(in, in, in) is semidet.

allow_unify_with_clobbered(live, _, _) :-
	error("allow_unify_with_clobbered: clobbered value is live?").
allow_unify_with_clobbered(dead, fake_unify, _).
allow_unify_with_clobbered(dead, _, det).

%-----------------------------------------------------------------------------%

:- pred check_not_clobbered(uniqueness, unify_is_real).
:- mode check_not_clobbered(in, in) is det.

	% sanity check
check_not_clobbered(Uniq, Real) :-
	( Real = real_unify, Uniq = clobbered ->
		error("abstractly_unify_inst_3: clobbered inst")
	; Real = real_unify, Uniq = mostly_clobbered ->
		error("abstractly_unify_inst_3: mostly_clobbered inst")
	;
		true
	).

%-----------------------------------------------------------------------------%

:- pred make_ground_inst_list_lives(list(inst), is_live, list(is_live),
		uniqueness, unify_is_real, unify_inst_info, list(inst),
		determinism, unify_inst_info).
:- mode make_ground_inst_list_lives(in, in, in, in, in, in,
		out, out, out) is semidet.

make_ground_inst_list_lives([], _, _, _, _, UI, [], det, UI).
make_ground_inst_list_lives([Inst0 | Insts0], Live, [ArgLive | ArgLives],
		Uniq, Real, UI0, [Inst | Insts], Det, UI) :-
	( Live = live, ArgLive = live ->
		BothLive = live
	;
		BothLive = dead
	),
	make_ground_inst(Inst0, BothLive, Uniq, Real, UI0, Inst, Det1, UI1),
	make_ground_inst_list_lives(Insts0, Live, ArgLives, Uniq, Real, UI1,
			Insts, Det2, UI),
	det_par_conjunction_detism(Det1, Det2, Det).

:- pred make_ground_inst_list(list(inst), is_live, uniqueness, unify_is_real,
		unify_inst_info, list(inst), determinism, unify_inst_info).
:- mode make_ground_inst_list(in, in, in, in, in, out, out, out) is semidet.

make_ground_inst_list([], _, _, _, UI, [], det, UI).
make_ground_inst_list([Inst0 | Insts0], Live, Uniq, Real, UI0,
		[Inst | Insts], Det, UI) :-
	make_ground_inst(Inst0, Live, Uniq, Real, UI0, Inst, Det1, UI1),
	make_ground_inst_list(Insts0, Live, Uniq, Real, UI1, Insts, Det2, UI),
	det_par_conjunction_detism(Det1, Det2, Det).

% abstractly unify an inst with `ground' and calculate the new inst
% and the determinism of the unification.

:- pred make_ground_inst(inst, is_live, uniqueness, unify_is_real,
		unify_inst_info, inst, determinism, unify_inst_info).
:- mode make_ground_inst(in, in, in, in, in, out, out, out) is semidet.

make_ground_inst(not_reached, _, _, _, UI, not_reached, erroneous, UI).
make_ground_inst(any(Uniq0), IsLive, Uniq1, Real, UI, ground(Uniq, no),
		semidet, UI) :-
	unify_uniq(IsLive, Real, semidet, Uniq0, Uniq1, Uniq).
make_ground_inst(free(_), IsLive, Uniq0, Real, UI, ground(Uniq, no), det, UI) :-
	unify_uniq(IsLive, Real, det, unique, Uniq0, Uniq).
make_ground_inst(free(_, T), IsLive, Uniq0, Real, UI,
		defined_inst(typed_ground(Uniq, T)), det, UI) :-
	unify_uniq(IsLive, Real, det, unique, Uniq0, Uniq).
make_ground_inst(bound(Uniq0, BoundInsts0), IsLive, Uniq1, Real, UI0,
		bound(Uniq, BoundInsts), Det, UI) :-
	unify_uniq(IsLive, Real, semidet, Uniq0, Uniq1, Uniq),
	make_ground_bound_inst_list(BoundInsts0, IsLive, Uniq1, Real, UI0,
					BoundInsts, Det1, UI),
	det_par_conjunction_detism(Det1, semidet, Det).
make_ground_inst(ground(Uniq0, _PredInst), IsLive, Uniq1, Real, UI,
		ground(Uniq, no), semidet, UI) :-
	unify_uniq(IsLive, Real, semidet, Uniq0, Uniq1, Uniq).
make_ground_inst(inst_var(_), _, _, _, _, _, _, _) :-
	error("free inst var").
make_ground_inst(abstract_inst(_,_), _, _, _, UI, ground(shared, no),
		semidet, UI).
make_ground_inst(defined_inst(InstName), IsLive, Uniq, Real, UI0,
			Inst, Det, UI) :-
		% check whether the inst name is already in the
		% ground_inst table
	unify_inst_info_get_module_info(UI0, ModuleInfo0),
	unify_inst_info_get_inst_table(UI0, InstTable0),
	inst_table_get_ground_insts(InstTable0, GroundInsts0),
	GroundInstKey = ground_inst(InstName, IsLive, Uniq, Real),
	(
		map__search(GroundInsts0, GroundInstKey, Result)
	->
		( Result = known(GroundInst0, Det0) ->
			% unify_inst_info_get_instmap(UI0, InstMap0),
			% inst_apply_sub(Sub0, GroundInst0, GroundInst),
			GroundInst = GroundInst0,	% XXX
			Det = Det0,
			UI = UI0
		;
			GroundInst = defined_inst(GroundInstKey),
			Det = det,
			UI = UI0
				% We can safely assume this is det, since
				% if it were semidet, we would have noticed
				% this in the process of unfolding the
				% definition.
		)
	;
		% insert the inst name in the ground_inst table, with
		% value `unknown' for the moment
		map__det_insert(GroundInsts0, GroundInstKey, unknown,
			GroundInsts1),
		inst_table_set_ground_insts(InstTable0, GroundInsts1,
			InstTable1),
		unify_inst_info_set_inst_table(UI0, InstTable1, UI1),

		% expand the inst name, and invoke ourself recursively on
		% it's expansion
		inst_lookup(InstTable1, ModuleInfo0, InstName, Inst0),
		unify_inst_info_get_instmap(UI1, InstMap1),
		inst_expand(InstMap1, InstTable1, ModuleInfo0, Inst0, Inst1),
		make_ground_inst(Inst1, IsLive, Uniq, Real, UI1, 
				GroundInst, Det, UI2),
		unify_inst_info_get_inst_table(UI2, InstTable2),

		% now that we have determined the resulting Inst, store
		% the appropriate value `known(GroundInst, Det)' in the
		% ground_inst table
		inst_table_get_ground_insts(InstTable2, GroundInsts2),
		map__det_update(GroundInsts2, GroundInstKey,
			known(GroundInst, Det), GroundInsts),
		inst_table_set_ground_insts(InstTable2, GroundInsts,
			InstTable),
		unify_inst_info_set_inst_table(UI2, InstTable, UI)
	),
		% avoid expanding recursive insts
	unify_inst_info_get_module_info(UI, LastModuleInfo),
	unify_inst_info_get_inst_table(UI, LastInstTable),
	unify_inst_info_get_instmap(UI, LastInstMap),
	( inst_contains_instname(GroundInst, LastInstMap, LastInstTable,
			LastModuleInfo, GroundInstKey) ->
		Inst = defined_inst(GroundInstKey)
	;
		Inst = GroundInst
	).
make_ground_inst(alias(InstKey), IsLive, Uniq, Real, UI0, Inst, Det, UI) :-
	unify_inst_info_get_inst_table(UI0, InstTable0),
	inst_table_get_inst_key_table(InstTable0, IKT0),
	unify_inst_info_get_instmap(UI0, InstMap0),
	instmap__inst_key_table_lookup(InstMap0, IKT0, InstKey, Inst0),
	make_ground_inst(Inst0, IsLive, Uniq, Real, UI0, Inst1, Det, UI1),
	( Inst0 = Inst1 ->
		Inst = alias(InstKey),
		UI = UI1
	;
		unify_inst_info_get_inst_table(UI1, InstTable1),
		inst_table_get_inst_key_table(InstTable1, IKT1),
		inst_key_table_add(IKT1, Inst1, NewKey, IKT),
		inst_table_set_inst_key_table(InstTable1, IKT, InstTable),
		Inst = alias(NewKey),
		unify_inst_info_get_instmap(UI1, InstMap1),
		instmap__add_alias(InstMap1, InstKey, NewKey, InstMap),
		unify_inst_info_set_inst_table(UI1, InstTable, UI2),
		unify_inst_info_set_instmap(UI2, InstMap, UI)
	).

:- pred make_ground_bound_inst_list(list(bound_inst), is_live, uniqueness,
		unify_is_real, unify_inst_info, list(bound_inst), determinism,
		unify_inst_info).
:- mode make_ground_bound_inst_list(in, in, in, in, in,
		out, out, out) is semidet.

make_ground_bound_inst_list([], _, _, _, UI, [], det, UI).
make_ground_bound_inst_list([Bound0 | Bounds0], IsLive, Uniq, Real, UI0,
			[Bound | Bounds], Det, UI) :-
	Bound0 = functor(ConsId, ArgInsts0),
	make_ground_inst_list(ArgInsts0, IsLive, Uniq, Real, UI0,
				ArgInsts, Det1, UI1),
	Bound = functor(ConsId, ArgInsts),
	make_ground_bound_inst_list(Bounds0, IsLive, Uniq, Real, UI1, Bounds,
		Det2, UI),
	det_par_conjunction_detism(Det1, Det2, Det).

%-----------------------------------------------------------------------------%

% abstractly unify an inst with `any' and calculate the new inst
% and the determinism of the unification.

:- pred make_any_inst(inst, is_live, uniqueness, unify_is_real, unify_inst_info,
				inst, determinism, unify_inst_info).
:- mode make_any_inst(in, in, in, in, in, out, out, out) is semidet.

make_any_inst(not_reached, _, _, _, UI, not_reached, erroneous, UI).
make_any_inst(alias(IK0), IsLive, Uniq0, Real, UI0, alias(IK), Det, UI) :-
	unify_inst_info_get_inst_table(UI0, InstTable0),
	inst_table_get_inst_key_table(InstTable0, IKT0),
	unify_inst_info_get_instmap(UI0, InstMap0),
	instmap__inst_key_table_lookup(InstMap0, IKT0, IK0, Inst0),
	make_any_inst(Inst0, IsLive, Uniq0, Real, UI0, Inst, Det, UI1),
	( Inst = Inst0 ->
		IK = IK0,
		UI = UI1
	;
		unify_inst_info_get_inst_table(UI1, InstTable1),
		inst_table_get_inst_key_table(InstTable1, IKT1),
		inst_key_table_add(IKT1, Inst, IK, IKT),
		inst_table_set_inst_key_table(InstTable1, IKT, InstTable),
		unify_inst_info_set_inst_table(UI1, InstTable, UI2),
		unify_inst_info_get_instmap(UI2, InstMap2),
		instmap__add_alias(InstMap2, IK0, IK, InstMap),
		unify_inst_info_set_instmap(UI2, InstMap, UI)
	).
make_any_inst(any(Uniq0), IsLive, Uniq1, Real, UI, any(Uniq),
		semidet, UI) :-
	allow_unify_bound_any(Real),
	unify_uniq(IsLive, Real, semidet, Uniq0, Uniq1, Uniq).
make_any_inst(free(unique), IsLive, Uniq0, Real, UI, any(Uniq), det, UI) :-
	unify_uniq(IsLive, Real, det, unique, Uniq0, Uniq).
make_any_inst(free(unique, T), IsLive, Uniq, Real, UI,
		defined_inst(Any), det, UI) :-
	% The following is a round-about way of doing this
	%	unify_uniq(IsLive, Real, det, unique, Uniq0, Uniq),
	%	Any = typed_any(Uniq, T).
	% without the need for a `typed_any' inst.
	Any = typed_inst(T, unify_inst(IsLive, free(unique), any(Uniq),
		Real)).
make_any_inst(bound(Uniq0, BoundInsts0), IsLive, Uniq1, Real, UI0,
		bound(Uniq, BoundInsts), Det, UI) :-
	allow_unify_bound_any(Real),
	unify_uniq(IsLive, Real, semidet, Uniq0, Uniq1, Uniq),
	make_any_bound_inst_list(BoundInsts0, IsLive, Uniq1, Real, UI0,
					BoundInsts, Det1, UI),
	det_par_conjunction_detism(Det1, semidet, Det).
make_any_inst(ground(Uniq0, PredInst), IsLive, Uniq1, Real, UI,
		ground(Uniq, PredInst), semidet, UI) :-
	allow_unify_bound_any(Real),
	unify_uniq(IsLive, Real, semidet, Uniq0, Uniq1, Uniq).
make_any_inst(inst_var(_), _, _, _, _, _, _, _) :-
	error("free inst var").
make_any_inst(abstract_inst(_,_), _, _, _, UI, any(shared),
		semidet, UI).
make_any_inst(defined_inst(InstName), IsLive, Uniq, Real, UI0,
			Inst, Det, UI) :-
		% check whether the inst name is already in the
		% any_inst table
	unify_inst_info_get_inst_table(UI0, InstTable0),
	inst_table_get_any_insts(InstTable0, AnyInsts0),
	AnyInstKey = any_inst(InstName, IsLive, Uniq, Real),
	(
		map__search(AnyInsts0, AnyInstKey, Result)
	->
		( Result = known(AnyInst0, Det0) ->
			AnyInst = AnyInst0,
			Det = Det0
		;
			AnyInst = defined_inst(AnyInstKey),
			Det = det
				% We can safely assume this is det, since
				% if it were semidet, we would have noticed
				% this in the process of unfolding the
				% definition.
		),
		UI = UI0
	;
		% insert the inst name in the any_inst table, with
		% value `unknown' for the moment
		map__det_insert(AnyInsts0, AnyInstKey, unknown, AnyInsts1),
		inst_table_set_any_insts(InstTable0, AnyInsts1, InstTable1),
		unify_inst_info_set_inst_table(UI0, InstTable1, UI1),

		% expand the inst name, and invoke ourself recursively on
		% it's expansion
		unify_inst_info_get_module_info(UI1, ModuleInfo1),
		unify_inst_info_get_instmap(UI1, InstMap1),
		inst_lookup(InstTable1, ModuleInfo1, InstName, Inst0),
		inst_expand(InstMap1, InstTable1, ModuleInfo1, Inst0, Inst1),
		make_any_inst(Inst1, IsLive, Uniq, Real, UI1,
				AnyInst, Det, UI2),

		% now that we have determined the resulting Inst, store
		% the appropriate value `known(AnyInst, Det)' in the
		% any_inst table
		unify_inst_info_get_inst_table(UI2, InstTable2),
		inst_table_get_any_insts(InstTable2, AnyInsts2),
		map__det_update(AnyInsts2, AnyInstKey,
			known(AnyInst, Det), AnyInsts),
		inst_table_set_any_insts(InstTable2, AnyInsts, InstTable),
		unify_inst_info_set_inst_table(UI2, InstTable, UI)
	),
		% avoid expanding recursive insts
	unify_inst_info_get_inst_table(UI, FinalInstTable),
	unify_inst_info_get_module_info(UI, FinalModuleInfo),
	unify_inst_info_get_instmap(UI, FinalInstMap),
	(
		inst_contains_instname(AnyInst, FinalInstMap, FinalInstTable,
			FinalModuleInfo, AnyInstKey)
	->
		Inst = defined_inst(AnyInstKey)
	;
		Inst = AnyInst
	).

:- pred make_any_bound_inst_list(list(bound_inst), is_live, uniqueness,
	unify_is_real, unify_inst_info, list(bound_inst), determinism,
	unify_inst_info).
:- mode make_any_bound_inst_list(in, in, in, in, in,
	out, out, out) is semidet.

make_any_bound_inst_list([], _, _, _, UI, [], det, UI).
make_any_bound_inst_list([Bound0 | Bounds0], IsLive, Uniq, Real, UI0,
			[Bound | Bounds], Det, UI) :-
	Bound0 = functor(ConsId, ArgInsts0),
	make_any_inst_list(ArgInsts0, IsLive, Uniq, Real, UI0,
				ArgInsts, Det1, UI1),
	Bound = functor(ConsId, ArgInsts),
	make_any_bound_inst_list(Bounds0, IsLive, Uniq, Real, UI1,
				Bounds, Det2, UI),
	det_par_conjunction_detism(Det1, Det2, Det).

:- pred make_any_inst_list(list(inst), is_live, uniqueness, unify_is_real,
		unify_inst_info, list(inst), determinism, unify_inst_info).
:- mode make_any_inst_list(in, in, in, in, in, out, out, out) is semidet.

make_any_inst_list([], _, _, _, UI, [], det, UI).
make_any_inst_list([Inst0 | Insts0], Live, Uniq, Real, UI0,
		[Inst | Insts], Det, UI) :-
	make_any_inst(Inst0, Live, Uniq, Real, UI0, Inst, Det1, UI1),
	make_any_inst_list(Insts0, Live, Uniq, Real, UI1, Insts, Det2, UI),
	det_par_conjunction_detism(Det1, Det2, Det).

%-----------------------------------------------------------------------------%

:- pred maybe_make_shared_inst_list(list(inst), list(is_live), unify_inst_info,
				list(inst), unify_inst_info).
:- mode maybe_make_shared_inst_list(in, in, in, out, out) is semidet.

maybe_make_shared_inst_list([], [], UI, [], UI).
maybe_make_shared_inst_list([Inst0 | Insts0], [IsLive | IsLives], UI0,
		[Inst | Insts], UI) :-
	( IsLive = live ->
		make_shared_inst(Inst0, UI0, Inst, UI1, yes)
	;
		Inst = Inst0,
		UI1 = UI0
	),
	maybe_make_shared_inst_list(Insts0, IsLives, UI1, Insts, UI).
maybe_make_shared_inst_list([], [_|_], _, _, _) :-
	error("maybe_make_shared_inst_list: length mismatch").
maybe_make_shared_inst_list([_|_], [], _, _, _) :-
	error("maybe_make_shared_inst_list: length mismatch").

:- pred make_shared_inst_list(list(inst), unify_inst_info,
				list(inst), unify_inst_info, bool).
:- mode make_shared_inst_list(in, in, out, out, out) is det.

make_shared_inst_list([], UI, [], UI, yes).
make_shared_inst_list([Inst0 | Insts0], UI0, [Inst | Insts], UI, Success) :-
	make_shared_inst(Inst0, UI0, Inst, UI1, Success0),
	make_shared_inst_list(Insts0, UI1, Insts, UI, Success1),
	bool__and(Success0, Success1, Success).

	% This is a wrapper for the above predicate, since this
	% operation is exported (used in modecheck_unify.m).
make_shared_inst_list(Insts0, InstTable0, ModuleInfo0, Sub0,
		Insts, InstTable, ModuleInfo, Sub) :-
	UI0 = unify_inst_info(ModuleInfo0, InstTable0, Sub0),
	( make_shared_inst_list(Insts0, UI0, Insts1, UI1, yes) ->
		Insts = Insts1,
		UI = UI1
	;
		% The caller should ensure that this case never happens.
		error("make_shared_inst_list: inst is partially instantiated")
	),
	UI  = unify_inst_info(ModuleInfo, InstTable, Sub).

% make an inst shared; replace all occurrences of `unique' or
% `mostly_unique' in the inst with `shared'.  The bool parameter is `yes' if
% the inst is ground (i.e. fully instantiated) and `no' otherwise.

:- pred make_shared_inst(inst, unify_inst_info, inst, unify_inst_info, bool).
:- mode make_shared_inst(in, in, out, out, out) is det.

make_shared_inst(not_reached, UI, not_reached, UI, yes).
make_shared_inst(alias(Key), UI0, Inst, UI, Success) :-
	unify_inst_info_get_instmap(UI0, InstMap0),
	unify_inst_info_get_inst_table(UI0, InstTable0),
	inst_table_get_inst_key_table(InstTable0, IKT0),
	instmap__inst_key_table_lookup(InstMap0, IKT0, Key, Inst0),
	make_shared_inst(Inst0, UI0, Inst1, UI1, Success),
	( Inst0 = Inst1 ->
		Inst = alias(Key),
		UI = UI1
	;
		unify_inst_info_get_instmap(UI1, InstMap1),
		unify_inst_info_get_inst_table(UI1, InstTable1),
		inst_table_get_inst_key_table(InstTable1, IKT1),
		inst_key_table_add(IKT1, Inst1, NewKey, IKT),
		inst_table_set_inst_key_table(InstTable1, IKT, InstTable),
		unify_inst_info_set_inst_table(UI1, InstTable, UI2),
		instmap__add_alias(InstMap1, Key, NewKey, InstMap),
		unify_inst_info_set_instmap(UI2, InstMap, UI),
		Inst = alias(NewKey)
	).
make_shared_inst(any(Uniq0), UI, any(Uniq), UI, yes) :-
	make_shared(Uniq0, Uniq).
make_shared_inst(free(A), UI, free(A), UI, no).
make_shared_inst(free(A, T), UI, free(A, T), UI, no).
make_shared_inst(bound(Uniq0, BoundInsts0), UI0, bound(Uniq, BoundInsts), UI,
		Success) :-
	make_shared(Uniq0, Uniq),
	make_shared_bound_inst_list(BoundInsts0, UI0, BoundInsts, UI, Success).
make_shared_inst(ground(Uniq0, PredInst), UI, ground(Uniq, PredInst), UI, yes)
		:-
	make_shared(Uniq0, Uniq).
make_shared_inst(inst_var(_), _, _, _, _) :-
	error("make_shared_inst: free inst var").
make_shared_inst(abstract_inst(_,_), UI, _, UI, _) :-
	error("make_shared_inst(abstract_inst)").
make_shared_inst(defined_inst(InstName), UI0, Inst, UI, Success) :-
		% check whether the inst name is already in the
		% shared_inst table
	unify_inst_info_get_inst_table(UI0, InstTable0),
	inst_table_get_shared_insts(InstTable0, SharedInsts0),
	(
		map__search(SharedInsts0, InstName, Result)
	->
		( Result = known(SharedInst0) ->
			SharedInst = SharedInst0
		;
			SharedInst = defined_inst(InstName)
		),
		UI = UI0,
		Success = yes
	;
		% insert the inst name in the shared_inst table, with
		% value `unknown' for the moment
		map__det_insert(SharedInsts0, InstName, unknown, SharedInsts1),
		inst_table_set_shared_insts(InstTable0, SharedInsts1,
			InstTable1),
		unify_inst_info_set_inst_table(UI0, InstTable1, UI1),

		% expand the inst name, and invoke ourself recursively on
		% it's expansion
		unify_inst_info_get_module_info(UI1, ModuleInfo1),
		inst_lookup(InstTable1, ModuleInfo1, InstName, Inst0),
		inst_expand_defined_inst(InstTable1, ModuleInfo1, Inst0, Inst1),
		make_shared_inst(Inst1, UI1, SharedInst, UI2, Success),

		% now that we have determined the resulting Inst, store
		% the appropriate value `known(SharedInst)' in the shared_inst
		% table
		unify_inst_info_get_inst_table(UI2, InstTable2),
		inst_table_get_shared_insts(InstTable2, SharedInsts2),
		map__det_update(SharedInsts2, InstName, known(SharedInst),
			SharedInsts),
		inst_table_set_shared_insts(InstTable2, SharedInsts, InstTable),
		unify_inst_info_set_inst_table(UI2, InstTable, UI)
	),
		% avoid expanding recursive insts
	unify_inst_info_get_inst_table(UI, LastInstTable),
	unify_inst_info_get_module_info(UI, LastModuleInfo),
	unify_inst_info_get_instmap(UI, LastInstMap),
	(
		inst_contains_instname(SharedInst, LastInstMap, LastInstTable,
			LastModuleInfo, InstName)
	->
		Inst = defined_inst(InstName)
	;
		Inst = SharedInst
	).

:- pred make_shared(uniqueness, uniqueness).
:- mode make_shared(in, out) is det.

make_shared(unique, shared).
make_shared(mostly_unique, shared).
make_shared(shared, shared).
make_shared(mostly_clobbered, mostly_clobbered).
make_shared(clobbered, clobbered).

:- pred make_shared_bound_inst_list(list(bound_inst), unify_inst_info,
			list(bound_inst), unify_inst_info, bool).
:- mode make_shared_bound_inst_list(in, in, out, out, out) is det.

make_shared_bound_inst_list([], UI, [], UI, yes).
make_shared_bound_inst_list([Bound0 | Bounds0], UI0, [Bound | Bounds], UI,
		Success) :-
	Bound0 = functor(ConsId, ArgInsts0),
	make_shared_inst_list(ArgInsts0, UI0, ArgInsts, UI1, Success0),
	Bound = functor(ConsId, ArgInsts),
	make_shared_bound_inst_list(Bounds0, UI1, Bounds, UI, Success1),
	bool__and(Success0, Success1, Success).

%-----------------------------------------------------------------------------%

% make an inst mostly-uniq: replace all occurrences of `unique'
% in the inst with `mostly_unique'.  (Used by unique_modes.m to
% change the insts of semidet-live or nondet-live insts.)

make_mostly_uniq_inst(Inst0, InstTable0, M0, Sub0, Inst, InstTable, M, Sub) :-
	UnifyInstInfo0 = unify_inst_info(M0, InstTable0, Sub0),
	make_mostly_uniq_inst_2(Inst0, UnifyInstInfo0, Inst, UnifyInstInfo),
	UnifyInstInfo  = unify_inst_info(M, InstTable, Sub).

:- pred make_mostly_uniq_inst_2(inst, unify_inst_info, inst, unify_inst_info).
:- mode make_mostly_uniq_inst_2(in, in, out, out) is det.

make_mostly_uniq_inst_2(not_reached, UI, not_reached, UI).
make_mostly_uniq_inst_2(any(Uniq0), UI, any(Uniq), UI) :-
	make_mostly_uniq(Uniq0, Uniq).
make_mostly_uniq_inst_2(free(A), UI, free(A), UI).
make_mostly_uniq_inst_2(free(A, T), UI, free(A, T), UI).
make_mostly_uniq_inst_2(bound(Uniq0, BoundInsts0), UI0,
			bound(Uniq, BoundInsts), UI) :-
		% XXX could improve efficiency by avoiding recursion here
	make_mostly_uniq(Uniq0, Uniq),
	make_mostly_uniq_bound_inst_list(BoundInsts0, UI0, BoundInsts, UI).
make_mostly_uniq_inst_2(ground(Uniq0, PredInst), UI,
			ground(Uniq, PredInst), UI) :-
	make_mostly_uniq(Uniq0, Uniq).
make_mostly_uniq_inst_2(inst_var(_), _, _, _) :-
	error("free inst var").
make_mostly_uniq_inst_2(abstract_inst(_,_), UI, _, UI) :-
	error("make_mostly_uniq_inst_2(abstract_inst)").
make_mostly_uniq_inst_2(defined_inst(InstName), UI0, Inst, UI) :-
		% check whether the inst name is already in the
		% mostly_uniq_inst table
	unify_inst_info_get_inst_table(UI0, InstTable0),
	inst_table_get_mostly_uniq_insts(InstTable0, NondetLiveInsts0),
	(
		map__search(NondetLiveInsts0, InstName, Result)
	->
		( Result = known(NondetLiveInst0) ->
			NondetLiveInst = NondetLiveInst0
		;
			NondetLiveInst = defined_inst(InstName)
		),
		UI = UI0
	;
		% insert the inst name in the mostly_uniq_inst table, with
		% value `unknown' for the moment
		map__det_insert(NondetLiveInsts0, InstName, unknown,
			NondetLiveInsts1),
		inst_table_set_mostly_uniq_insts(InstTable0, NondetLiveInsts1,
			InstTable1),
		unify_inst_info_set_inst_table(UI0, InstTable1, UI1),

		% expand the inst name, and invoke ourself recursively on
		% it's expansion
		unify_inst_info_get_module_info(UI1, ModuleInfo1),
		inst_lookup(InstTable1, ModuleInfo1, InstName, Inst0),
		inst_expand_defined_inst(InstTable1, ModuleInfo1, Inst0, Inst1),
		make_mostly_uniq_inst_2(Inst1, UI1, NondetLiveInst, UI2),

		% now that we have determined the resulting Inst, store
		% the appropriate value `known(NondetLiveInst)' in the
		% mostly_uniq_inst table
		unify_inst_info_get_inst_table(UI2, InstTable2),
		inst_table_get_mostly_uniq_insts(InstTable2, NondetLiveInsts2),
		map__det_update(NondetLiveInsts2, InstName,
			known(NondetLiveInst), NondetLiveInsts),
		inst_table_set_mostly_uniq_insts(InstTable2, NondetLiveInsts,
			InstTable),
		unify_inst_info_set_inst_table(UI2, InstTable, UI)
	),
		% avoid expanding recursive insts
	unify_inst_info_get_module_info(UI, LastModuleInfo),
	unify_inst_info_get_inst_table(UI, LastInstTable),
	unify_inst_info_get_instmap(UI, LastInstMap),
	(
		inst_contains_instname(NondetLiveInst, LastInstMap,
			LastInstTable, LastModuleInfo, InstName)
	->
		Inst = defined_inst(InstName)
	;
		Inst = NondetLiveInst
	).
make_mostly_uniq_inst_2(alias(Key), UI0, Inst, UI) :-
	unify_inst_info_get_inst_table(UI0, InstTable0),
	unify_inst_info_get_instmap(UI0, InstMap0),
	inst_table_get_inst_key_table(InstTable0, IKT0),
	instmap__inst_key_table_lookup(InstMap0, IKT0, Key, Inst0),
	make_mostly_uniq_inst_2(Inst0, UI0, Inst1, UI1),
	( Inst0 = Inst1 ->
		Inst = alias(Key),
		UI = UI1
	;
		unify_inst_info_get_inst_table(UI1, InstTable1),
		unify_inst_info_get_instmap(UI1, InstMap1),
		inst_table_get_inst_key_table(InstTable1, IKT1),
		inst_key_table_add(IKT1, Inst1, NewKey, IKT),
		inst_table_set_inst_key_table(InstTable1, IKT, InstTable),
		unify_inst_info_set_inst_table(UI1, InstTable, UI2),
		instmap__add_alias(InstMap1, Key, NewKey, InstMap),
		unify_inst_info_set_instmap(UI2, InstMap, UI),
		Inst = alias(NewKey)
	).

:- pred make_mostly_uniq(uniqueness, uniqueness).
:- mode make_mostly_uniq(in, out) is det.

make_mostly_uniq(unique, mostly_unique).
make_mostly_uniq(mostly_unique, mostly_unique).
make_mostly_uniq(shared, shared).
make_mostly_uniq(mostly_clobbered, mostly_clobbered).
make_mostly_uniq(clobbered, clobbered).

:- pred make_mostly_uniq_bound_inst_list(list(bound_inst), unify_inst_info,
					list(bound_inst), unify_inst_info).
:- mode make_mostly_uniq_bound_inst_list(in, in, out, out) is det.

make_mostly_uniq_bound_inst_list([], UI, [], UI).
make_mostly_uniq_bound_inst_list([Bound0 | Bounds0], UI0,
				[Bound | Bounds], UI) :-
	Bound0 = functor(ConsId, ArgInsts0),
	make_mostly_uniq_inst_list(ArgInsts0, UI0, ArgInsts, UI1),
	Bound = functor(ConsId, ArgInsts),
	make_mostly_uniq_bound_inst_list(Bounds0, UI1, Bounds, UI).

:- pred make_mostly_uniq_inst_list(list(inst), unify_inst_info,
				list(inst), unify_inst_info).
:- mode make_mostly_uniq_inst_list(in, in, out, out) is det.

make_mostly_uniq_inst_list([], UI, [], UI).
make_mostly_uniq_inst_list([Inst0 | Insts0], UI0, [Inst | Insts], UI) :-
	make_mostly_uniq_inst_2(Inst0, UI0, Inst, UI1),
	make_mostly_uniq_inst_list(Insts0, UI1, Insts, UI).

%-----------------------------------------------------------------------------%

make_clobbered_inst(Inst0, InstTable0, ModuleInfo0, InstMap0,
		Inst, InstTable, ModuleInfo, InstMap, RemovedKeys) :-
	UI0 = unify_inst_info(ModuleInfo0, InstTable0, InstMap0),
	make_clobbered_inst(Inst0, Inst, [] - UI0, RemovedKeys - UI),
	UI = unify_inst_info(ModuleInfo, InstTable, InstMap).

:- type clobber_inst_info == pair(list(inst_key), unify_inst_info).

:- pred make_clobbered_inst(inst, inst, clobber_inst_info, clobber_inst_info).
:- mode make_clobbered_inst(in, out, in, out) is det.

make_clobbered_inst(any(U0), any(U), CI, CI) :-
	clobber(U0, U).
make_clobbered_inst(alias(IK), Inst, Rem0 - UI0, CI) :-
	unify_inst_info_get_inst_table(UI0, InstTable0),
	inst_table_get_inst_key_table(InstTable0, IKT0),
	unify_inst_info_get_instmap(UI0, InstMap0),
	instmap__inst_key_table_lookup(InstMap0, IKT0, IK, Inst0),
	make_clobbered_inst(Inst0, Inst, [IK | Rem0] - UI0, CI).
make_clobbered_inst(free(A), free(A), CI, CI).
make_clobbered_inst(free(A, T), free(A, T), CI, CI).
make_clobbered_inst(bound(U0, Bs0), bound(U, Bs), CI0, CI) :-
	clobber(U0, U),
	list__map_foldl(
	    ( pred(functor(C, Is0)::in, functor(C, Is)::out, in, out) is det -->
		    list__map_foldl(make_clobbered_inst, Is0, Is)
	    ), Bs0, Bs, CI0, CI).
make_clobbered_inst(ground(U0, P), ground(U, P), CI, CI) :-
	clobber(U0, U).
make_clobbered_inst(not_reached, not_reached, CI, CI).
make_clobbered_inst(inst_var(_), _, _, _) :-
	error("inst_util__make_clobbered_inst: inst_var").
make_clobbered_inst(abstract_inst(S, Is0), abstract_inst(S, Is), CI0, CI) :-
	list__map_foldl(make_clobbered_inst, Is0, Is, CI0, CI).
make_clobbered_inst(defined_inst(InstName), Inst, Rem0 - UI0, Rem - UI) :-
	unify_inst_info_get_inst_table(UI0, InstTable0),
	inst_table_get_clobbered_insts(InstTable0, ClobberedInsts0),
	(
		map__search(ClobberedInsts0, InstName, Result)
	->
		( Result = known(ClobberedInst0) ->
			ClobberedInst = ClobberedInst0
		;
			ClobberedInst = defined_inst(InstName)
		),
		UI = UI0,
		Rem = Rem0
	;
		% insert the inst name in the clobbered_inst table, with
		% value `unknown' for the moment
		map__det_insert(ClobberedInsts0, InstName, unknown,
			ClobberedInsts1),
		inst_table_set_clobbered_insts(InstTable0, ClobberedInsts1,
			InstTable1),
		unify_inst_info_set_inst_table(UI0, InstTable1, UI1),

		% expand the inst name, and invoke ourself recursively on
		% it's expansion
		unify_inst_info_get_module_info(UI1, ModuleInfo1),
		inst_lookup(InstTable1, ModuleInfo1, InstName, Inst0),
		inst_expand_defined_inst(InstTable1, ModuleInfo1, Inst0, Inst1),
		make_clobbered_inst(Inst1, ClobberedInst, Rem0 - UI1,
			Rem - UI2),

		% now that we have determined the resulting Inst, store 
		% the appropriate value `known(ClobberedInst)' in the
		% clobbered_inst table
		unify_inst_info_get_inst_table(UI2, InstTable2),
		inst_table_get_clobbered_insts(InstTable2, ClobberedInsts2),
		map__det_update(ClobberedInsts2, InstName, known(ClobberedInst),
			ClobberedInsts),
		inst_table_set_clobbered_insts(InstTable2, ClobberedInsts,
			InstTable),
		unify_inst_info_set_inst_table(UI2, InstTable, UI)
	),
		% avoid expanding recursive insts
	unify_inst_info_get_inst_table(UI, LastInstTable),
	unify_inst_info_get_module_info(UI, LastModuleInfo),
	unify_inst_info_get_instmap(UI, LastInstMap),
	(
		inst_contains_instname(ClobberedInst, LastInstMap,
			LastInstTable, LastModuleInfo, InstName)
	->
		Inst = defined_inst(InstName)
	;
		Inst = ClobberedInst
	).

:- pred clobber(uniqueness, uniqueness).
:- mode clobber(in, out) is det.

clobber(shared, shared).
clobber(unique, clobbered).
clobber(mostly_unique, mostly_clobbered).
clobber(clobbered, clobbered).
clobber(mostly_clobbered, mostly_clobbered).

%-----------------------------------------------------------------------------%

	% Should we allow unifications between bound (or ground) insts
	% and `any' insts?
	% Previously we only allowed this for fake_unifies,
	% but now we allow it for real_unifies too.

:- pred allow_unify_bound_any(unify_is_real::in) is det.
allow_unify_bound_any(_) :- true.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% inst_merge(InstA, InstB, InstC):
	%       Combine the insts found in different arms of a
	%       disjunction (or if-then-else).
	%       The information in InstC is the minimum of the
	%       information in InstA and InstB.  Where InstA and
	%       InstB specify a binding (free or bound), it must be
	%       the same in both.
	%	Note: inst_merge is designed to be called only from
	%	instmap__merge.  It does not make sense to merge a single pair
	%	of insts in isolation.  In particular, the returned instmap is
	%	not completely correct and needs to be fixed up by
	%	instmap__merge after all insts in the instmap have been merged.

inst_merge(InstA, InstB, IsLive, InstMap0, InstTable0,
		ModuleInfo0, MergeInfo0, Inst, InstMap, InstTable, ModuleInfo,
		MergeInfo) :-
		% check whether this pair of insts is already in
		% the merge_insts table
	inst_table_get_merge_insts(InstTable0, MergeInstTable0),
	ThisInstPair = merge_inst_pair(IsLive, InstA, InstB),
	( map__search(MergeInstTable0, ThisInstPair, Result) ->
		ModuleInfo = ModuleInfo0,
		( Result = known(MergedInst) ->
			Inst0 = MergedInst
		;
			Inst0 = defined_inst(merge_inst(IsLive, InstA, InstB))
		),
		InstTable = InstTable0,
		InstMap = InstMap0,
		MergeInfo = MergeInfo0
	;
			% insert ThisInstPair into the table with value
			%`unknown'
		map__det_insert(MergeInstTable0, ThisInstPair, unknown,
			MergeInstTable1),
		inst_table_set_merge_insts(InstTable0, MergeInstTable1,
			InstTable1),

			% merge the insts
		inst_merge_2(InstA, InstB, IsLive, InstMap0,
			InstTable1, ModuleInfo0, MergeInfo0, Inst0, InstMap,
			InstTable2, ModuleInfo, MergeInfo),

			% now update the value associated with ThisInstPair
		inst_table_get_merge_insts(InstTable2, MergeInstTable2),
		map__det_update(MergeInstTable2, ThisInstPair,
				known(Inst0), MergeInstTable3),
		inst_table_set_merge_insts(InstTable2, MergeInstTable3,
				InstTable)
	),
		% avoid expanding recursive insts
	(
		inst_contains_instname(Inst0, InstMap, InstTable, ModuleInfo,
			merge_inst(IsLive, InstA, InstB))
	->
		Inst = defined_inst(merge_inst(IsLive, InstA, InstB))
	;
		Inst = Inst0
	).

:- pred inst_merge_2(inst, inst, is_live, instmap, inst_table,
		module_info, merge_info, inst, instmap, inst_table,
		module_info, merge_info).
:- mode inst_merge_2(in, in, in, in, in, in, in, out, out, out, out, out)
		is semidet.

inst_merge_2(InstA, InstB, IsLive, InstMap0, InstTable0,
		ModuleInfo0, MergeInfo0, Inst, InstMap, InstTable, ModuleInfo,
		MergeInfo) :-
	MergeInfo0 = merge_info(MergeSubs0, RefCounts0),
	RefCounts0 = ref_counts(LiveCountsA, LiveCountsB, CountsA0, CountsB0),
	expand_inst(IsLive, LiveCountsA, InstA, InstMap0, InstTable0,
		ModuleInfo0, CountsA0, InstA2, InstMap1,
		InstTable1, ModuleInfo1, CountsA),
	expand_inst(IsLive, LiveCountsB, InstB, InstMap1, InstTable1,
		ModuleInfo1, CountsB0, InstB2, InstMap2,
		InstTable2, ModuleInfo2, CountsB),
	RefCounts1 = ref_counts(LiveCountsA, LiveCountsB, CountsA, CountsB),
	MergeInfo1 = merge_info(MergeSubs0, RefCounts1),
	(
		InstB2 = not_reached
	->
		Inst = InstA2,
		ModuleInfo = ModuleInfo2,
		InstTable = InstTable2,
		InstMap = InstMap2,
		MergeInfo = MergeInfo1
	;
		InstA2 = not_reached
	->
		Inst = InstB2,
		ModuleInfo = ModuleInfo2,
		InstTable = InstTable2,
		InstMap = InstMap2,
		MergeInfo = MergeInfo1
	;
		InstA2 = alias(IKA0),
		InstB2 \= alias(_)
	->
		Lambda = lambda([I::out] is nondet, (
			map__member(MergeSubs0, IKA0 - _, MergeIK),
			I = alias(MergeIK))),
		make_shared_merge_insts(IKA0, Lambda, InstMap2,
			InstTable2, ModuleInfo2, InstA3, InstMap3,
			InstTable3, ModuleInfo3),
		inst_merge_3(InstA3, InstB2, IsLive, 
			InstMap3, InstTable3, ModuleInfo3, MergeInfo1,
			Inst, InstMap, InstTable, ModuleInfo, MergeInfo)
	;
		InstB2 = alias(IKB0),
		InstA2 \= alias(_)
	->
		Lambda = lambda([I::out] is nondet, (
			map__member(MergeSubs0, _ - IKB0, MergeIK),
			I = alias(MergeIK))),
		make_shared_merge_insts(IKB0, Lambda, InstMap2,
			InstTable2, ModuleInfo2, InstB3, InstMap3,
			InstTable3, ModuleInfo3),
		inst_merge_3(InstA2, InstB3, IsLive, 
			InstMap3, InstTable3, ModuleInfo3, MergeInfo1,
			Inst, InstMap, InstTable, ModuleInfo, MergeInfo)
	;
		InstA2 = alias(IKA),
		InstB2 = alias(IKB)
	->
		(
			instmap__inst_keys_are_equivalent(IKA, InstMap2,
				IKB, InstMap2)
		->
			IK = IKA,
			InstTable = InstTable2,
			ModuleInfo = ModuleInfo2,
			InstMap = InstMap2,
			map__set(MergeSubs0, IKA - IKB, IKA, MergeSubs),
			MergeInfo = merge_info(MergeSubs, RefCounts1)
		;
			map__search(MergeSubs0, IKA - IKB, IK0)
		->
			IK = IK0,
			InstTable = InstTable2,
			ModuleInfo = ModuleInfo2,
			InstMap = InstMap2,
			MergeInfo = MergeInfo1
		;
			inst_table_get_inst_key_table(InstTable2, IKT0),
			instmap__inst_key_table_lookup(InstMap2, IKT0, IKA,
			    InstA3),
			instmap__inst_key_table_lookup(InstMap2, IKT0, IKB,
			    InstB3),
			inst_merge_3(InstA3, InstB3, IsLive, InstMap2,
			    InstTable2, ModuleInfo2, MergeInfo1,
			    Inst0, InstMap, InstTable3, ModuleInfo,
			    MergeInfo2),
			MergeInfo2 = merge_info(MergeSubs2, RefCounts),
			( map__search(MergeSubs2, IKA - IKB, IK1) ->
			    IK = IK1,
			    MergeSubs = MergeSubs2,
			    InstTable = InstTable3
			;
			    % Create a new inst key for the merged inst.
			    inst_table_get_inst_key_table(InstTable3, IKT1),
			    inst_key_table_add(IKT1, Inst0, IK, IKT),
			    inst_table_set_inst_key_table(InstTable3, IKT,
				InstTable),
			    map__det_insert(MergeSubs2, IKA - IKB, IK,
				MergeSubs)
			),
			MergeInfo = merge_info(MergeSubs, RefCounts)
		),
		Inst = alias(IK)
	;
		inst_merge_3(InstA2, InstB2, IsLive, InstMap2,
			InstTable2, ModuleInfo2, MergeInfo1, Inst,
			InstMap, InstTable, ModuleInfo, MergeInfo)
	).

:- pred inst_merge_3(inst, inst, is_live, instmap, inst_table,
		module_info, merge_info, inst, instmap, inst_table,
		module_info, merge_info).
:- mode inst_merge_3(in, in, in, in, in, in, in, out, out, out, out, out)
		is semidet.

% We do not yet allow merging of `free' and `any',
% except in the case where the any is `mostly_clobbered_any'
% or `clobbered_any', because that would require inserting
% additional code to initialize the free var.
%
% For similar reasons, we also don't yet allow merging of free(alias) and 
% free(unique).  This would require inserting additional code to create
% a reference for the free(unique) var.
%
% We do NOT plan to allow merging of `free' and `ground'
% to produce `any', because that would introduce `any'
% insts even for builtin types such as `int' which can't
% support `any'.  It might also make the mode system
% too weak -- it might not be able to detect bugs as well
% as it can currently.

inst_merge_3(any(UniqA), any(UniqB), _, InstMap, InstTable, M, MS,
		any(Uniq), InstMap, InstTable, M, MS) :-
	merge_uniq(UniqA, UniqB, Uniq).
inst_merge_3(any(Uniq), free(_), _, InstMap, InstTable, M, MS, any(Uniq),
		InstMap, InstTable, M, MS) :-
	% we do not yet allow merge of any with free, except for clobbered anys
	( Uniq = clobbered ; Uniq = mostly_clobbered ).
inst_merge_3(any(UniqA), bound(UniqB, ListB), _, InstMap, InstTable, M, MS,
		any(Uniq), InstMap, InstTable, M, MS) :-
	merge_uniq_bound(UniqA, UniqB, ListB, InstMap, InstTable, M, Uniq),
	% we do not yet allow merge of any with free, except for clobbered anys
	( ( Uniq = clobbered ; Uniq = mostly_clobbered ) ->
		true
	;
		bound_inst_list_is_ground_or_any(ListB, InstMap, InstTable, M)
	).
inst_merge_3(any(UniqA), ground(UniqB, _), _, InstMap, InstTable, M, MS,
		any(Uniq), InstMap, InstTable, M, MS) :-
	merge_uniq(UniqA, UniqB, Uniq).
inst_merge_3(any(UniqA), abstract_inst(_, _), _, InstMap, InstTable, M, MS,
		any(Uniq), InstMap, InstTable, M, MS) :-
	merge_uniq(UniqA, shared, Uniq),
	% we do not yet allow merge of any with free, except for clobbered anys
	( Uniq = clobbered ; Uniq = mostly_clobbered ).
inst_merge_3(free(_), any(Uniq), _, InstMap, InstTable, M, MS, any(Uniq),
		InstMap, InstTable, M, MS) :-
	% we do not yet allow merge of any with free, except for clobbered anys
	( Uniq = clobbered ; Uniq = mostly_clobbered ).
inst_merge_3(bound(UniqA, ListA), any(UniqB), _, InstMap, InstTable, M, MS,
		any(Uniq), InstMap, InstTable, M, MS) :-
	merge_uniq_bound(UniqB, UniqA, ListA, InstMap, InstTable, M, Uniq),
	% we do not yet allow merge of any with free, except for clobbered anys
	( ( Uniq = clobbered ; Uniq = mostly_clobbered ) ->
		true
	;
		bound_inst_list_is_ground_or_any(ListA, InstMap, InstTable, M)
	).
inst_merge_3(ground(UniqA, _), any(UniqB), _, InstMap, InstTable, M, MS,
		any(Uniq), InstMap, InstTable, M, MS) :-
	merge_uniq(UniqA, UniqB, Uniq).
inst_merge_3(abstract_inst(_, _), any(UniqB), _, InstMap, InstTable, M, MS,
		any(Uniq), InstMap, InstTable, M, MS) :-
	merge_uniq(shared, UniqB, Uniq),
	% we do not yet allow merge of any with free, except for clobbered anys
	( Uniq = clobbered ; Uniq = mostly_clobbered ).
inst_merge_3(free(A), free(A), _, InstMap, InstTable, M, MS, free(A),
		InstMap, InstTable, M, MS).
inst_merge_3(bound(UniqA, ListA), bound(UniqB, ListB), IsLive, InstMap0,
		InstTable0, ModuleInfo0, MergeInfo0, bound(Uniq, List),
		InstMap, InstTable, ModuleInfo, MergeInfo) :-
	merge_uniq(UniqA, UniqB, Uniq),
	bound_inst_list_merge(ListA, ListB, IsLive, InstMap0, InstTable0,
		ModuleInfo0, MergeInfo0, List, InstMap, InstTable,
		ModuleInfo, MergeInfo).
inst_merge_3(bound(UniqA, ListA), ground(UniqB, _), _, InstMap,
		InstTable, ModuleInfo, MS, ground(Uniq, no), InstMap, InstTable,
		ModuleInfo, MS) :-
	merge_uniq_bound(UniqB, UniqA, ListA, InstMap, InstTable, ModuleInfo,
		Uniq),
	bound_inst_list_is_ground(ListA, InstMap, InstTable, ModuleInfo).
inst_merge_3(ground(UniqA, _), bound(UniqB, ListB), _, InstMap,
		InstTable, ModuleInfo, MS, ground(Uniq, no), InstMap, InstTable,
		ModuleInfo, MS) :-
	merge_uniq_bound(UniqA, UniqB, ListB, InstMap, InstTable,
			ModuleInfo, Uniq),
	bound_inst_list_is_ground(ListB, InstMap, InstTable, ModuleInfo).
inst_merge_3(ground(UniqA, MaybePredA), ground(UniqB, MaybePredB), _,
		InstMap, InstTable, ModuleInfo, MS, ground(Uniq, MaybePred),
		InstMap, InstTable, ModuleInfo, MS) :-
	(
		MaybePredA = yes(PredA),
		MaybePredB = yes(PredB)
	->
		% if they specify matching pred insts, but one is more
		% precise (specifies more info) than the other,
		% then we want to choose the least precise one
		(
			pred_inst_matches(PredA, InstMap, PredB, InstMap,
					InstTable, ModuleInfo)
		->
			MaybePred = yes(PredB)
		;
			pred_inst_matches(PredB, InstMap, PredA, InstMap,
				InstTable, ModuleInfo)
		->
			MaybePred = yes(PredA)
		;
			MaybePred = no
		)
	;       
		MaybePred = no
	),
	merge_uniq(UniqA, UniqB, Uniq).
inst_merge_3(abstract_inst(Name, ArgsA), abstract_inst(Name, ArgsB), IsLive,
		InstMap0, InstTable0, ModuleInfo0, MergeInfo0,
		abstract_inst(Name, Args), InstMap, InstTable, ModuleInfo,
		MergeInfo) :-
	inst_list_merge(ArgsA, ArgsB, IsLive, InstMap0, InstTable0,
		ModuleInfo0, MergeInfo0, Args, InstMap, InstTable, ModuleInfo,
		MergeInfo).
inst_merge_3(not_reached, Inst, _, InstMap, InstTable, M, MS, Inst, InstMap,
			InstTable, M, MS).

:- pred merge_uniq(uniqueness, uniqueness, uniqueness).
:- mode merge_uniq(in, in, out) is det.

	% merge_uniq(A, B, C) succeeds if C is minimum of A and B in
	% the ordering
	% clobbered < mostly_clobbered < shared < mostly_unique < unique

merge_uniq(UniqA, UniqB, Merged) :-
	( unique_matches_initial(UniqA, UniqB) ->       % A >= B
		Merged = UniqB
	;
		Merged = UniqA
	).

	% merge_uniq_bound(UniqA, UniqB, ListB, InstTable, ModuleInfo, Uniq)x
	% succeeds iff Uniq is the result of merging

:- pred merge_uniq_bound(uniqueness, uniqueness, list(bound_inst), instmap,
		inst_table, module_info, uniqueness).
:- mode merge_uniq_bound(in, in, in, in, in, in, out) is det.

merge_uniq_bound(UniqA, UniqB, ListB, InstMap, InstTable, ModuleInfo, Uniq) :-
	merge_uniq(UniqA, UniqB, Uniq0),
	set__init(Expansions),
	merge_bound_inst_list_uniq(ListB, Uniq0, InstMap, InstTable,
		ModuleInfo, Expansions, Uniq).

:- pred merge_bound_inst_list_uniq(list(bound_inst), uniqueness, instmap,
		inst_table, module_info, set(inst_name), uniqueness).
:- mode merge_bound_inst_list_uniq(in, in, in, in, in, in, out) is det.

merge_bound_inst_list_uniq([], Uniq, _, _, _, _, Uniq).
merge_bound_inst_list_uniq([BoundInst | BoundInsts], Uniq0,
			InstMap, InstTable, ModuleInfo, Expansions, Uniq) :-
	BoundInst = functor(_ConsId, ArgInsts),
	merge_inst_list_uniq(ArgInsts, Uniq0, InstMap, InstTable, ModuleInfo,
		Expansions, Uniq1),
	merge_bound_inst_list_uniq(BoundInsts, Uniq1, InstMap, InstTable,
		ModuleInfo, Expansions, Uniq).

:- pred merge_inst_list_uniq(list(inst), uniqueness, instmap, inst_table,
		module_info, set(inst_name), uniqueness).
:- mode merge_inst_list_uniq(in, in, in, in, in, in, out) is det.

merge_inst_list_uniq([], Uniq, _, _, _, _, Uniq).
merge_inst_list_uniq([Inst | Insts], Uniq0, InstMap, InstTable, ModuleInfo,
		Expansions, Uniq) :-
	merge_inst_uniq(Inst, Uniq0, InstMap, InstTable, ModuleInfo,
		Expansions, Uniq1),
	merge_inst_list_uniq(Insts, Uniq1, InstMap, InstTable, ModuleInfo,
		Expansions, Uniq).

:- pred merge_inst_uniq(inst, uniqueness, instmap, inst_table, module_info,
			set(inst_name), uniqueness).
:- mode merge_inst_uniq(in, in, in, in, in, in, out) is det.

merge_inst_uniq(any(UniqA), UniqB, _, _, _, _, Uniq) :-
	merge_uniq(UniqA, UniqB, Uniq).
merge_inst_uniq(alias(InstKey), UniqB, InstMap, InstTable, ModuleInfo,
		Expansions, Uniq) :-
	inst_table_get_inst_key_table(InstTable, IKT),
	instmap__inst_key_table_lookup(InstMap, IKT, InstKey, Inst),
	merge_inst_uniq(Inst, UniqB, InstMap, InstTable, ModuleInfo,
		Expansions, Uniq).
merge_inst_uniq(free(_), Uniq, _, _, _, _, Uniq).
merge_inst_uniq(free(_, _), Uniq, _, _, _, _, Uniq).
merge_inst_uniq(bound(UniqA, ListA), UniqB, InstMap, InstTable, ModuleInfo,
		Expansions, Uniq) :-
	merge_uniq(UniqA, UniqB, Uniq0),
	merge_bound_inst_list_uniq(ListA, Uniq0, InstMap, InstTable,
		ModuleInfo, Expansions, Uniq).
merge_inst_uniq(ground(UniqA, _), UniqB, _, _, _, _, Uniq) :-
	merge_uniq(UniqA, UniqB, Uniq).
merge_inst_uniq(abstract_inst(_,_), UniqB, _, _, _, _, Uniq) :-
	merge_uniq(shared, UniqB, Uniq).
merge_inst_uniq(defined_inst(InstName), UniqB, InstMap, InstTable,
		ModuleInfo, Expansions, Uniq) :-
	( set__member(InstName, Expansions) ->
		Uniq = UniqB
	;
		set__insert(Expansions, InstName, Expansions1),
		inst_lookup(InstTable, ModuleInfo, InstName, Inst),
		merge_inst_uniq(Inst, UniqB, InstMap, InstTable, ModuleInfo,
			Expansions1, Uniq)
	).
merge_inst_uniq(not_reached, Uniq, _, _, _, _, Uniq).
merge_inst_uniq(inst_var(_), _, _, _, _, _, _) :-
	error("merge_inst_uniq: unexpected inst_var").

%-----------------------------------------------------------------------------%

:- pred inst_list_merge(list(inst), list(inst), is_live, instmap,
		inst_table, module_info, merge_info, list(inst), instmap,
		inst_table, module_info, merge_info).
:- mode inst_list_merge(in, in, in, in, in, in, in, out, out, out,
		out, out) is semidet.

inst_list_merge([], [], _, InstMap, InstTable, ModuleInfo, MergeInfo, [],
		InstMap, InstTable, ModuleInfo, MergeInfo).
inst_list_merge([ArgA | ArgsA], [ArgB | ArgsB], IsLive, InstMap0,
		InstTable0, ModuleInfo0, MergeInfo0, [Arg | Args], InstMap,
		InstTable, ModuleInfo, MergeInfo) :-
	inst_merge(ArgA, ArgB, IsLive, InstMap0, InstTable0,
		ModuleInfo0, MergeInfo0, Arg, InstMap1, InstTable1,
		ModuleInfo1, MergeInfo1),
	inst_list_merge(ArgsA, ArgsB, IsLive, InstMap1, InstTable1,
		ModuleInfo1, MergeInfo1, Args, InstMap, InstTable, ModuleInfo,
		MergeInfo).

	% bound_inst_list_merge(Xs, Ys, InstTable0, ModuleInfo0, Zs, InstTable,
	%	ModuleInfo):
	% The two input lists Xs and Ys must already be sorted.
	% Here we perform a sorted merge operation,
	% so that the functors of the output list Zs are the union
	% of the functors of the input lists Xs and Ys.

:- pred bound_inst_list_merge(list(bound_inst), list(bound_inst), is_live,
		instmap, inst_table, module_info, merge_info,
		list(bound_inst), instmap, inst_table, module_info, merge_info).
:- mode bound_inst_list_merge(in, in, in, in, in, in, in, out, out, out,
		out, out) is semidet.

bound_inst_list_merge(Xs, Ys, IsLive, InstMap0, InstTable0,
		ModuleInfo0, MergeInfo0, Zs, InstMap, InstTable, ModuleInfo,
		MergeInfo) :-
	( Xs = [] ->
		MergeInfo = MergeInfo0,
		ModuleInfo = ModuleInfo0,
		InstTable = InstTable0,
		InstMap = InstMap0,
		Zs = Ys
	; Ys = [] ->
		MergeInfo = MergeInfo0,
		ModuleInfo = ModuleInfo0,
		InstTable = InstTable0,
		InstMap = InstMap0,
		Zs = Xs
	;
		Xs = [X | Xs1],
		Ys = [Y | Ys1],
		X = functor(ConsIdX, ArgsX),
		Y = functor(ConsIdY, ArgsY),
		( ConsIdX = ConsIdY ->
			MergeInfo0 = merge_info(_, RefCounts0),
			RefCounts0 = ref_counts(_, _, TotalA0, TotalB0),

			inst_list_merge(ArgsX, ArgsY, IsLive, InstMap0,
				InstTable0, ModuleInfo0, MergeInfo0,
				Args, InstMap1, InstTable1, ModuleInfo1,
				MergeInfo1),
			Z = functor(ConsIdX, Args),
			Zs = [Z | Zs1],

			MergeInfo1 = merge_info(MergeSubs1, RefCounts1),
			RefCounts1 = ref_counts(LiveA1, LiveB1, TotalA1,
				TotalB1),
			RefCounts2 = ref_counts(LiveA1, LiveB1, TotalA0,
				TotalB0),
			MergeInfo2 = merge_info(MergeSubs1, RefCounts2),

			bound_inst_list_merge(Xs1, Ys1, IsLive, InstMap1,
				InstTable1, ModuleInfo1, MergeInfo2, Zs1,
				InstMap, InstTable, ModuleInfo, MergeInfo3),

			MergeInfo3 = merge_info(MergeSubs, RefCounts3),
			RefCounts3 = ref_counts(LiveA, LiveB, TotalA3, TotalB3),
			uniq_counts_max_merge(TotalA1, TotalA3, TotalA),
			uniq_counts_max_merge(TotalB1, TotalB3, TotalB),
			RefCounts = ref_counts(LiveA, LiveB, TotalA, TotalB),
			MergeInfo = merge_info(MergeSubs, RefCounts)
		; compare(<, ConsIdX, ConsIdY) ->
			Zs = [X | Zs1],
			bound_inst_list_merge(Xs1, Ys, IsLive, InstMap0,
				InstTable0, ModuleInfo0, MergeInfo0,
				Zs1, InstMap, InstTable, ModuleInfo, MergeInfo)
		;
			Zs = [Y | Zs1],
			bound_inst_list_merge(Xs, Ys1, IsLive, InstMap0,
				InstTable0, ModuleInfo0, MergeInfo0,
				Zs1, InstMap, InstTable, ModuleInfo, MergeInfo)
		)
	).

%-----------------------------------------------------------------------------%

:- pred expand_inst(is_live, inst_key_counts, inst, instmap, inst_table,
		module_info, inst_key_counts, inst, instmap, inst_table,
		module_info, inst_key_counts).
:- mode expand_inst(in, in, in, in, in, in, in, out, out, out, out, out)
		is det.

expand_inst(IsLive, LiveCounts, Inst0, InstMap0, InstTable0, ModuleInfo0,
		Counts0, Inst, InstMap, InstTable, ModuleInfo, Counts) :-
	( Inst0 = alias(IK) ->
		( IsLive = dead ->
			inst_expand(InstMap0, InstTable0, ModuleInfo0, Inst0,
				Inst1),
			( has_count_many(Counts0, IK) ->
				make_clobbered_inst(Inst1, InstTable0,
					ModuleInfo0, InstMap0, Inst, InstTable,
					ModuleInfo, InstMap, RemovedKeys),
				list__foldl(dec_uniq_count, [IK | RemovedKeys],
					Counts0, Counts)
			;
				Inst = Inst1,
				InstMap = InstMap0,
				InstTable = InstTable0,
				ModuleInfo = ModuleInfo0,
				Counts = Counts0
			)
		; has_count_many(LiveCounts, IK) ->
			% If the inst_key has multiple references, we need to
			% hang on to it.
			Inst = Inst0,
			InstMap = InstMap0,
			InstTable = InstTable0,
			ModuleInfo = ModuleInfo0,
			Counts = Counts0
		;
			% This is the only live reference to the inst_key so
			% we can remove the alias.
			InstMap = InstMap0,
			InstTable = InstTable0,
			ModuleInfo = ModuleInfo0,
			inst_expand(InstMap0, InstTable0, ModuleInfo0, Inst0,
				Inst),
			Counts = Counts0
		)
	;
		InstMap = InstMap0,
		InstTable = InstTable0,
		ModuleInfo = ModuleInfo0,
		Counts = Counts0,
		inst_expand_defined_inst(InstTable, ModuleInfo, Inst0, Inst)
	).

:- pred make_shared_merge_insts(inst_key, pred(inst), instmap,
	inst_table, module_info, inst, instmap, inst_table, module_info).
:- mode make_shared_merge_insts(in, pred(out) is nondet,
	in, in, in, out, out, out, out) is semidet.

make_shared_merge_insts(IK0, Lambda, InstMap0, InstTable0, ModuleInfo0,
		Inst, InstMap, InstTable, ModuleInfo) :-
	UI0 = unify_inst_info(ModuleInfo0, InstTable0, InstMap0),
	make_shared_inst(alias(IK0), UI0, Inst1, UI1, yes),
	solutions(Lambda, Insts),
	make_shared_inst_list(Insts, UI1, _, UI, yes),
	UI = unify_inst_info(ModuleInfo, InstTable, InstMap),
	Inst1 = alias(IK),
	inst_table_get_inst_key_table(InstTable, IKT),
	instmap__inst_key_table_lookup(InstMap, IKT, IK, Inst).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

get_single_arg_inst(defined_inst(InstName), InstMap, InstTable, ModuleInfo,
			ConsId, ArgInst) :-
	inst_lookup(InstTable, ModuleInfo, InstName, Inst),
	get_single_arg_inst(Inst, InstMap, InstTable, ModuleInfo, ConsId,
			ArgInst).
get_single_arg_inst(not_reached, _, _InstTable, _, _, not_reached).
get_single_arg_inst(ground(Uniq, _PredInst), _, _InstTable, _, _,
		ground(Uniq, no)).
get_single_arg_inst(bound(_Uniq, List), _, _InstTable, _, ConsId, ArgInst) :-
	( get_single_arg_inst_2(List, ConsId, ArgInst0) ->
		ArgInst = ArgInst0
	;
		% the code is unreachable
		ArgInst = not_reached
	).
get_single_arg_inst(free(A), _, _, _, _, free(A)).
get_single_arg_inst(free(A, _Type), _, _, _, _, free(A)).  % XXX loses type info
get_single_arg_inst(alias(Key), InstMap, InstTable, ModuleInfo, ConsId, Inst) :-
	inst_table_get_inst_key_table(InstTable, IKT),
	instmap__inst_key_table_lookup(InstMap, IKT, Key, Inst0),
	get_single_arg_inst(Inst0, InstMap, InstTable, ModuleInfo, ConsId,
			Inst).
get_single_arg_inst(any(Uniq), _, _InstTable, _, _, any(Uniq)).
get_single_arg_inst(abstract_inst(_, _), _, _, _, _, _) :-
	error("get_single_arg_inst: abstract insts not supported").
get_single_arg_inst(inst_var(_), _, _, _, _, _) :-
	error("get_single_arg_inst: inst_var").

:- pred get_single_arg_inst_2(list(bound_inst), cons_id, inst).
:- mode get_single_arg_inst_2(in, in, out) is semidet.

get_single_arg_inst_2([BoundInst | BoundInsts], ConsId, ArgInst) :-
	(
		BoundInst = functor(ConsId, [ArgInst0])
	->
		ArgInst = ArgInst0
	;
		get_single_arg_inst_2(BoundInsts, ConsId, ArgInst)
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

inst_table_create_sub(InstTable0, NewInstTable, Sub, InstTable) :-
	inst_table_get_all_tables(InstTable0, SubInstTable0, UnifyInstTable0,
		MergeInstTable0, GroundInstTable0, AnyInstTable0,
		SharedInstTable0, ClobberedInstTable0, MostlyUniqInstTable0,
		IKT0),
	inst_table_get_all_tables(NewInstTable, NewSubInstTable,
		NewUnifyInstTable, NewMergeInstTable, NewGroundInstTable,
		NewAnyInstTable, NewSharedInstTable, NewMostlyUniqInstTable,
		NewClobberedInstTable, NewIKT),
	inst_key_table_create_sub(IKT0, NewIKT, Sub, IKT),

	maybe_inst_det_table_apply_sub(UnifyInstTable0, NewUnifyInstTable,
		UnifyInstTable, Sub),
	merge_inst_table_apply_sub(MergeInstTable0, NewMergeInstTable,
		MergeInstTable, Sub),
	substitution_inst_table_apply_sub(SubInstTable0, NewSubInstTable,
		SubInstTable, Sub),
	maybe_inst_det_table_apply_sub(GroundInstTable0, NewGroundInstTable,
		GroundInstTable, Sub),
	maybe_inst_det_table_apply_sub(AnyInstTable0, NewAnyInstTable,
		AnyInstTable, Sub),
	maybe_inst_table_apply_sub(SharedInstTable0, NewSharedInstTable,
		SharedInstTable, Sub),
	maybe_inst_table_apply_sub(MostlyUniqInstTable0,
		NewMostlyUniqInstTable, MostlyUniqInstTable, Sub),
	maybe_inst_table_apply_sub(ClobberedInstTable0,
		NewClobberedInstTable, ClobberedInstTable, Sub),

	inst_table_set_all_tables(InstTable0, SubInstTable, UnifyInstTable,
		MergeInstTable, GroundInstTable, AnyInstTable,
		SharedInstTable, MostlyUniqInstTable, ClobberedInstTable, IKT,
		InstTable).

:- pred maybe_inst_table_apply_sub(map(inst_name, maybe_inst),
		map(inst_name, maybe_inst), map(inst_name, maybe_inst),
		inst_key_sub).
:- mode maybe_inst_table_apply_sub(in, in, out, in) is det.

maybe_inst_table_apply_sub(Table0, NewTable, Table, Sub) :-
	( map__is_empty(NewTable) ->
		% Optimise common case
		Table = Table0
	;
		map__to_assoc_list(NewTable, NewTableAL),
		maybe_inst_table_apply_sub_2(NewTableAL, Table0, Table, Sub)
	).

:- pred maybe_inst_table_apply_sub_2(assoc_list(inst_name, maybe_inst),
		map(inst_name, maybe_inst), map(inst_name, maybe_inst),
		inst_key_sub).
:- mode maybe_inst_table_apply_sub_2(in, in, out, in) is det.

maybe_inst_table_apply_sub_2([], Table, Table, _).
maybe_inst_table_apply_sub_2([InstName0 - Inst0 | Rest], Table0, Table, Sub) :-
	inst_name_apply_sub(Sub, InstName0, InstName),
	( Inst0 = unknown,
		Inst = unknown
	; Inst0 = known(I0),
		inst_apply_sub(Sub, I0, I),
		Inst = known(I)
	),
	map__set(Table0, InstName, Inst, Table1),
	maybe_inst_table_apply_sub_2(Rest, Table1, Table, Sub).

:- pred maybe_inst_det_table_apply_sub(map(inst_name, maybe_inst_det),
		map(inst_name, maybe_inst_det), map(inst_name, maybe_inst_det),
		inst_key_sub).
:- mode maybe_inst_det_table_apply_sub(in, in, out, in) is det.

maybe_inst_det_table_apply_sub(Table0, NewTable, Table, Sub) :-
	( map__is_empty(NewTable) ->
		% Optimise common case
		Table = Table0
	;
		map__to_assoc_list(NewTable, NewTableAL),
		maybe_inst_det_table_apply_sub_2(NewTableAL, Table0, Table,
			Sub)
	).

:- pred maybe_inst_det_table_apply_sub_2(assoc_list(inst_name, maybe_inst_det),
		map(inst_name, maybe_inst_det), map(inst_name, maybe_inst_det),
		inst_key_sub).
:- mode maybe_inst_det_table_apply_sub_2(in, in, out, in) is det.

maybe_inst_det_table_apply_sub_2([], Table, Table, _).
maybe_inst_det_table_apply_sub_2([InstName0 - InstDet0 | Rest], Table0, Table, 
		Sub) :-
	inst_name_apply_sub(Sub, InstName0, InstName),
	( InstDet0 = unknown,
		InstDet = unknown
	; InstDet0 = known(Inst0, Det),
		inst_apply_sub(Sub, Inst0, Inst),
		InstDet = known(Inst, Det)
	),
	map__set(Table0, InstName, InstDet, Table1),
	maybe_inst_det_table_apply_sub_2(Rest, Table1, Table, Sub).

:- pred merge_inst_table_apply_sub(merge_inst_table, merge_inst_table,
		merge_inst_table, inst_key_sub).
:- mode merge_inst_table_apply_sub(in, in, out, in) is det.

merge_inst_table_apply_sub(Table0, NewTable, Table, Sub) :-
	( map__is_empty(Table0) ->
		% Optimise common case
		Table = Table0
	;
		map__to_assoc_list(NewTable, NewTableAL),
		merge_inst_table_apply_sub_2(NewTableAL, Table0, Table, Sub)
	).

:- pred merge_inst_table_apply_sub_2(assoc_list(merge_inst_pair, maybe_inst),
		merge_inst_table, merge_inst_table, inst_key_sub).
:- mode merge_inst_table_apply_sub_2(in, in, out, in) is det.

merge_inst_table_apply_sub_2([], Table, Table, _).
merge_inst_table_apply_sub_2([Pair0 - Inst0 | Rest], Table0, Table, Sub) :-
	Pair0 = merge_inst_pair(IsLive, IA0, IB0),
	inst_apply_sub(Sub, IA0, IA),
	inst_apply_sub(Sub, IB0, IB),
	( Inst0 = unknown,
		Inst = unknown
	; Inst0 = known(I0),
		inst_apply_sub(Sub, I0, I),
		Inst = known(I)
	),
	map__set(Table0, merge_inst_pair(IsLive, IA, IB), Inst, Table1),
	merge_inst_table_apply_sub_2(Rest, Table1, Table, Sub).

:- pred substitution_inst_table_apply_sub(substitution_inst_table,
	substitution_inst_table, substitution_inst_table, inst_key_sub).
:- mode substitution_inst_table_apply_sub(in, in, out, in) is det.

substitution_inst_table_apply_sub(Table0, NewTable, Table, Sub) :-
	( map__is_empty(Table0) ->
		% Optimise common case
		Table = Table0
	;
		map__to_assoc_list(NewTable, NewTableAL),
		substitution_inst_table_apply_sub_2(NewTableAL, Table0, Table,
			Sub)
	).

:- pred substitution_inst_table_apply_sub_2(assoc_list(substitution_inst,
	maybe_inst), substitution_inst_table, substitution_inst_table,
	inst_key_sub).
:- mode substitution_inst_table_apply_sub_2(in, in, out, in) is det.

substitution_inst_table_apply_sub_2([], Table, Table, _).
substitution_inst_table_apply_sub_2(
		[substitution_inst(InstName0, K, S) - Inst0 | Rest],
		Table0, Table, Sub) :-
	inst_name_apply_sub(Sub, InstName0, InstName),
	( Inst0 = unknown,
		Inst = unknown
	; Inst0 = known(I0),
		inst_apply_sub(Sub, I0, I),
		Inst = known(I)
	),
	map__set(Table0, substitution_inst(InstName, K, S), Inst, Table1),
	substitution_inst_table_apply_sub_2(Rest, Table1, Table, Sub).

:- pred inst_name_apply_sub(inst_key_sub, inst_name, inst_name).
:- mode inst_name_apply_sub(in, in, out) is det.

inst_name_apply_sub(Sub, user_inst(Sym, Insts0), user_inst(Sym, Insts)) :-
	list__map(inst_apply_sub(Sub), Insts0, Insts).
inst_name_apply_sub(Sub, merge_inst(IsLive, InstA0, InstB0),
		merge_inst(IsLive, InstA, InstB)) :-
	inst_apply_sub(Sub, InstA0, InstA),
	inst_apply_sub(Sub, InstB0, InstB).
inst_name_apply_sub(Sub, unify_inst(IsLive, InstA0, InstB0, IsReal),
		unify_inst(IsLive, InstA, InstB, IsReal)) :-
	inst_apply_sub(Sub, InstA0, InstA),
	inst_apply_sub(Sub, InstB0, InstB).
inst_name_apply_sub(Sub, ground_inst(Name0, IsLive, Uniq, IsReal),
		ground_inst(Name, IsLive, Uniq, IsReal)) :-
	inst_name_apply_sub(Sub, Name0, Name).
inst_name_apply_sub(Sub, any_inst(Name0, IsLive, Uniq, IsReal),
		any_inst(Name, IsLive, Uniq, IsReal)) :-
	inst_name_apply_sub(Sub, Name0, Name).
inst_name_apply_sub(Sub, shared_inst(Name0), shared_inst(Name)) :-
	inst_name_apply_sub(Sub, Name0, Name).
inst_name_apply_sub(Sub, mostly_uniq_inst(Name0), mostly_uniq_inst(Name)) :-
	inst_name_apply_sub(Sub, Name0, Name).
inst_name_apply_sub(_Sub, typed_ground(Uniq, Type), typed_ground(Uniq, Type)).
inst_name_apply_sub(Sub, typed_inst(Type, Name0), typed_inst(Type, Name)) :-
	inst_name_apply_sub(Sub, Name0, Name).
inst_name_apply_sub(Sub, substitution_inst(Name0, K, S),
		substitution_inst(Name, K, S)) :-
	inst_name_apply_sub(Sub, Name0, Name).

%-----------------------------------------------------------------------------%

inst_fold(InstMap, InstTable, ModuleInfo, Before, After, Merge, Inst) -->
	{ set__init(Recursive) },
	inst_fold_2(InstMap, InstTable, ModuleInfo, Recursive, Before, After,
		Merge, Inst).

:- pred inst_fold_2(instmap, inst_table, module_info, set(inst_name),
		inst_fold_pred(T), inst_fold_pred(T), inst_fold_merge_pred(T),
		inst, T, T).
:- mode inst_fold_2(in, in, in, in, inst_fold_pred, inst_fold_pred,
		inst_fold_merge_pred, in, in, out) is det.

inst_fold_2(InstMap, InstTable, ModuleInfo, Recursive, Before, After, Merge,
		Inst) -->
	( call(Before, Inst, Recursive) -> [] ; [] ),
	inst_fold_3(InstMap, InstTable, ModuleInfo, Recursive, Before, After,
		Merge, Inst),
	( call(After, Inst, Recursive) -> [] ; [] ).

:- pred inst_fold_3(instmap, inst_table, module_info, set(inst_name),
		inst_fold_pred(T), inst_fold_pred(T), inst_fold_merge_pred(T),
		inst, T, T).
:- mode inst_fold_3(in, in, in, in, inst_fold_pred, inst_fold_pred,
		inst_fold_merge_pred, in, in, out) is det.

inst_fold_3(_, _, _, _, _, _, _, any(_)) --> [].
inst_fold_3(InstMap, InstTable, ModuleInfo, Recursive, Before, After, Merge,
		alias(Key)) -->
	{ inst_table_get_inst_key_table(InstTable, IKT) },
	{ instmap__inst_key_table_lookup(InstMap, IKT, Key, Inst) },
	inst_fold_2(InstMap, InstTable, ModuleInfo, Recursive, Before, After,
		Merge, Inst).
inst_fold_3(_, _, _, _, _, _, _, free(_)) --> [].
inst_fold_3(_, _, _, _, _, _, _, free(_, _)) --> [].
inst_fold_3(InstMap, InstTable, ModuleInfo, Recursive, Before, After, Merge,
		bound(_, BoundInsts0)) -->
	( { BoundInsts0 = [] }
	; { BoundInsts0 = [functor(_, Insts0) | BoundInsts1] },
		=(Acc0),
		list__foldl(inst_fold_2(InstMap, InstTable, ModuleInfo,
			Recursive, Before, After, Merge), Insts0),
		list__foldl(lambda([BI::in, A0::in, A::out] is det,
		(   
			BI = functor(_, Insts),
			list__foldl(inst_fold_2(InstMap, InstTable, ModuleInfo,
			    Recursive, Before, After, Merge), Insts, Acc0, A1),
			call(Merge, A0, A1, A)
		)), BoundInsts1)
	).
inst_fold_3(_, _, _, _, _, _, _, ground(_, _)) --> [].
inst_fold_3(_, _, _, _, _, _, _, not_reached) --> [].
inst_fold_3(_, _, _, _, _, _, _, inst_var(_)) --> [].
inst_fold_3(InstMap, InstTable, ModuleInfo, Recursive0, Before, After,
		Merge, defined_inst(InstName)) -->
	( { set__member(InstName, Recursive0) } ->
		[]
	;
		{ set__insert(Recursive0, InstName, Recursive) },
		{ inst_lookup(InstTable, ModuleInfo, InstName, Inst) },
		inst_fold_2(InstMap, InstTable, ModuleInfo, Recursive, Before,
			After, Merge, Inst)
	).
inst_fold_3(InstMap, InstTable, ModuleInfo, Recursive, Before, After,
		Merge, abstract_inst(_, Insts)) -->
	list__foldl(inst_fold_2(InstMap, InstTable, ModuleInfo, Recursive,
		Before, After, Merge), Insts).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
