%-----------------------------------------------------------------------------%
% Copyright (C) 1997-1998 The University of Melbourne.
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

:- import_module hlds_module, prog_data, (inst).

:- pred abstractly_unify_inst(is_live, inst, inst, unify_is_real,
		inst_table, module_info, inst_key_sub, inst, determinism,
		inst_table, module_info, inst_key_sub).
:- mode abstractly_unify_inst(in, in, in, in, in, in, in,
			out, out, out, out, out) is semidet.

	% Compute the inst that results from abstractly unifying two variables.

:- pred abstractly_unify_inst_functor(is_live, inst, cons_id, list(inst),
			list(is_live), unify_is_real, inst_table,
			module_info, inst_key_sub, inst, determinism,
			inst_table, module_info, inst_key_sub).
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

:- pred make_mostly_uniq_inst(inst, inst_table, module_info, inst_key_sub,
		inst, inst_table, module_info, inst_key_sub).
:- mode make_mostly_uniq_inst(in, in, in, in, out, out, out, out) is det.

	% Given an inst, return a new inst which is the same as the
	% original inst but with all occurrences of `unique' replaced
	% with `mostly_unique'.

%-----------------------------------------------------------------------------%

:- pred inst_merge(inst, inst, inst_table, module_info, inst,
		inst_table, module_info).
:- mode inst_merge(in, in, in, in, out, out, out) is semidet.

	% inst_merge(InstA, InstB, InstC):
	%       Combine the insts found in different arms of a
	%       disjunction (or if-then-else).
	%       The information in InstC is the minimum of the
	%       information in InstA and InstB.  Where InstA and
	%       InstB specify a binding (free or bound), it must be
	%       the same in both.

%-----------------------------------------------------------------------------%

:- pred inst_table_create_sub(inst_table, inst_table, inst_key_sub, inst_table).
:- mode inst_table_create_sub(in, in, out, out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module hlds_data, inst_match, mode_util, det_analysis.
:- import_module bool, std_util, require, map, list, set, assoc_list.

:- pred find_latest_inst_key(inst_key_sub, inst_key, inst_key).
:- mode find_latest_inst_key(in, in, out) is det.

find_latest_inst_key(Sub, IK0, IK) :-
	( map__search(Sub, IK0, IK1) ->
		find_latest_inst_key(Sub, IK1, IK)
	;
		IK = IK0
	).

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
			inst_key_sub
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

:- pred unify_inst_info_get_inst_key_sub(unify_inst_info :: in,
		inst_key_sub :: out) is det.
unify_inst_info_get_inst_key_sub(unify_inst_info(_, _, Sub), Sub).

:- pred unify_inst_info_set_inst_key_sub(unify_inst_info :: in,
		inst_key_sub :: in, unify_inst_info :: out) is det.
unify_inst_info_set_inst_key_sub(unify_inst_info(A, B, _), Sub,
		unify_inst_info(A, B, Sub)).

:- pred unify_inst_info_find_latest_inst_key(unify_inst_info :: in,
		inst_key :: in, inst_key :: out) is det.
unify_inst_info_find_latest_inst_key(UI, Key0, Key) :-
	unify_inst_info_get_inst_key_sub(UI, Sub),
	find_latest_inst_key(Sub, Key0, Key).

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

		% avoid expanding recursive insts
	( inst_contains_instname(Inst1, LastInstTable, LastModuleInfo,
			ThisInstPair) ->
		Inst = defined_inst(ThisInstPair)
	;
		Inst = Inst1
	).

:- pred abstractly_unify_inst_2(is_live, unify_is_real, inst, inst,
			unify_inst_info, inst, determinism, unify_inst_info).
:- mode abstractly_unify_inst_2(in, in, in, in, in, out, out, out) is semidet.

abstractly_unify_inst_2(IsLive, Real, InstA, InstB, UI0, Inst, Det, UI) :-
	unify_inst_info_get_module_info(UI0, ModuleInfo0),
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

                        InstA2 = free,
			inst_is_ground(InstB2, InstTable0, ModuleInfo0)
                ->
			UI = UI0,
                        Inst = InstB2, Det = det
                ;
                        % alias(K) = free where alias(K) is ground

                        InstB2 = free,
			inst_is_ground(InstA2, InstTable0, ModuleInfo0)
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

			unify_inst_info_get_inst_key_sub(UI0, Sub0),
			inst_table_get_inst_key_table(InstTable0, IKT0),
                        ( InstA2 = alias(KeyA0) ->
                                find_latest_inst_key(Sub0, KeyA0, KeyA),
                                inst_key_table_lookup(IKT0, KeyA, InstA3),
                                KeysToUpdate0 = [KeyA0]
                        ;
                                InstA3 = InstA2,
                                KeysToUpdate0 = []
                        ),
                        ( InstB2 = alias(KeyB0) ->
                                find_latest_inst_key(Sub0, KeyB0, KeyB),
                                inst_key_table_lookup(IKT0, KeyB, InstB3),
                                KeysToUpdate = [KeyB0 | KeysToUpdate0]
                        ;
                                InstB3 = InstB2,
                                KeysToUpdate = KeysToUpdate0
                        ), 

                        abstractly_unify_inst_2(IsLive, Real, InstA3, InstB3,
                                UI0, Inst0, Det, UI1),

			unify_inst_info_get_inst_key_sub(UI1, Sub1),
                        % Optimise some more common cases
                        (
                                % If the unified inst is the same as
                                % InstA and InstA was aliased, don't
                                % bother allocating a new inst_key.
                                % Reuse the old one instead.

                                InstA2 = alias(KA), InstA3 = Inst0
                        ->
                                Inst = InstA2,
                                ( InstB2 = alias(KB) ->
                                        map__set(Sub1, KB, KA, Sub)
                                ;
                                        Sub = Sub1
                                ),
				unify_inst_info_set_inst_key_sub(UI1, Sub, UI)
                        ;
                                % If the unified inst is the same as
                                % InstB and InstB was aliased, don't
                                % bother allocating a new inst_key.
                                % Reuse the old one instead. 

                                InstB2 = alias(KB), InstB3 = Inst0
                        ->
                                Inst = InstB2,
                                ( InstA2 = alias(KA) ->
                                        map__set(Sub1, KA, KB, Sub)
                                ;
                                        Sub = Sub1
                                ),
				unify_inst_info_set_inst_key_sub(UI1, Sub, UI)
                        ;
				unify_inst_info_get_inst_table(UI1, InstTable1),
				inst_table_get_inst_key_table(InstTable1, IKT1),
                                inst_key_table_add(IKT1, Inst0, NewKey, IKT),
                                Inst = alias(NewKey),
                                add_new_keys_to_sub(Sub1, KeysToUpdate, NewKey,
                                        Sub),
				inst_table_set_inst_key_table(InstTable1, IKT,
					InstTable),
				unify_inst_info_set_inst_table(UI1, InstTable,
					UI2),
				unify_inst_info_set_inst_key_sub(UI2, Sub, UI)
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

abstractly_unify_inst_3(live, Real, free, any(UniqY), UI,
					any(Uniq), det, UI) :-
	unify_uniq(live, Real, det, unique, UniqY, Uniq).

% abstractly_unify_inst_3(live, _, free,   free, _,	_, _, _, _) :- fail.

abstractly_unify_inst_3(live, Real, free,     bound(UniqY, List0), UI0,
		 			      bound(Uniq, List), det, UI) :-
	unify_uniq(live, Real, det, unique, UniqY, Uniq),

		% since both are live, we must disallow free-free unifications
	unify_inst_info_get_module_info(UI0, M0),
	unify_inst_info_get_inst_table(UI0, InstTable0),
	bound_inst_list_is_ground_or_any(List0, InstTable0, M0),

		% since both are live, we must make the result shared
		% (unless it was already shared)
	( ( UniqY = unique ; UniqY = mostly_unique ) ->
		make_shared_bound_inst_list(List0, UI0, List, UI)
	;
		List = List0, UI = UI0
	).

abstractly_unify_inst_3(live, Real, free,   ground(UniqY, PredInst), UI,
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

abstractly_unify_inst_3(live, Real,	bound(UniqY, List0), free, UI0,
					bound(Uniq, List), det,  UI) :-
	unify_uniq(live, Real, det, unique, UniqY, Uniq),
		% since both are live, we must disallow free-free unifications
	unify_inst_info_get_module_info(UI0, M0),
	unify_inst_info_get_inst_table(UI0, InstTable0),
	bound_inst_list_is_ground_or_any(List0, InstTable0, M0),
	make_shared_bound_inst_list(List0, UI0, List, UI).

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
			UI,         ground(Uniq, yes(PredInst)), semidet, UI) :-
	Real = fake_unify,
	unify_uniq(live, Real, det, UniqX, UniqY, Uniq).

abstractly_unify_inst_3(live, Real,  ground(Uniq0, yes(PredInst)), free, UI,
				     ground(Uniq, yes(PredInst)), det, UI) :-
	unify_uniq(live, Real, det, unique, Uniq0, Uniq).

abstractly_unify_inst_3(live, Real, ground(UniqX, yes(_)),
			bound(UniqY, BoundInsts0), UI0,
			bound(Uniq, BoundInsts), Det, UI) :-
	% check `Real = fake_unify' ?
	unify_uniq(dead, Real, semidet, UniqX, UniqY, Uniq),
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
	( inst_is_free(Inst0, InstTable0, M0) ->
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

	% YYY This looks right, but it wasn't on the main branch.  Hmmm
abstractly_unify_inst_3(dead, _Real, free, Inst, UI, Inst, det, UI).

abstractly_unify_inst_3(dead, Real, bound(UniqX, List0), any(UniqY), UI0,
					bound(Uniq, List), Det, UI) :-
	allow_unify_bound_any(Real),
	unify_uniq(dead, Real, semidet, UniqX, UniqY, Uniq),
	make_any_bound_inst_list(List0, live, UniqY, Real, UI0,
					List, Det1, UI),
	det_par_conjunction_detism(Det1, semidet, Det).

abstractly_unify_inst_3(dead, Real, bound(UniqX, List), free, UI,
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
			UI,         ground(Uniq, yes(PredInst)), semidet, UI) :-
	allow_unify_bound_any(Real),
	unify_uniq(dead, Real, semidet, UniqX, UniqY, Uniq).

abstractly_unify_inst_3(dead, _Real, ground(Uniq, yes(PredInst)), free, UI,
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
		InstTable0, ModuleInfo0, Sub0, Inst, Det, InstTable,
		ModuleInfo, Sub) :-
	inst_expand_defined_inst(InstTable0, ModuleInfo0, InstA, InstA2),

	UI0 = unify_inst_info(ModuleInfo0, InstTable0, Sub0),

	( InstA2 = alias(KeyA0) ->
		find_latest_inst_key(Sub0, KeyA0, KeyA),
		inst_table_get_inst_key_table(InstTable0, IKT0),
		inst_key_table_lookup(IKT0, KeyA, InstA3),

		abstractly_unify_inst_functor_2(Live, Real, InstA3, ConsId,
			ArgInsts, ArgLives, UI0, Inst0, Det, UI),
		UI = unify_inst_info(ModuleInfo, InstTable1, Sub1),
		( determinism_components(Det, _, at_most_zero) ->
			Inst = Inst0,
			Sub = Sub1,
			InstTable = InstTable1
		;
			inst_table_get_inst_key_table(InstTable1, IKT1),
			inst_key_table_add(IKT1, Inst0, NewKey, IKT),
			inst_table_set_inst_key_table(InstTable1, IKT,
				InstTable),
			Inst = alias(NewKey),
			map__set(Sub1, KeyA0, NewKey, Sub)
		)
	;
		abstractly_unify_inst_functor_2(Live, Real, InstA2, ConsId,
			ArgInsts, ArgLives, UI0, Inst0, Det, UI),
		UI = unify_inst_info(ModuleInfo, InstTable, Sub),
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

abstractly_unify_inst_functor_2(live, _Real, free, ConsId, Args, ArgLives, UI,
			bound(unique, [functor(ConsId, Args)]), det, UI) :-
	unify_inst_info_get_module_info(UI, M),
	unify_inst_info_get_inst_table(UI, InstTable),
	inst_list_is_ground_or_any_or_dead(Args, ArgLives, InstTable, M).

abstractly_unify_inst_functor_2(live, Real, bound(Uniq, ListX), ConsId, Args,
			ArgLives, UI0, bound(Uniq, List), Det, UI) :-
	abstractly_unify_bound_inst_list_lives(ListX, ConsId, Args, ArgLives,
					Real, UI0, List, Det, UI).

abstractly_unify_inst_functor_2(live, Real, ground(Uniq, _), ConsId, ArgInsts,
		ArgLives, UI0, Inst, Det, UI) :-
	make_ground_inst_list_lives(ArgInsts, live, ArgLives, Uniq, Real, UI0,
		GroundArgInsts, Det, UI),
	Inst = bound(Uniq, [functor(ConsId, GroundArgInsts)]).

% abstractly_unify_inst_functor_2(live, _, abstract_inst(_,_), _, _, _, _,
%		_, _, _) :-
%       fail.

abstractly_unify_inst_functor_2(dead, _, not_reached, _, _, _, UI,
					not_reached, erroneous, UI).

abstractly_unify_inst_functor_2(dead, _Real, free, ConsId, Args, _ArgLives, UI,
			bound(unique, [functor(ConsId, Args)]), det, UI).

abstractly_unify_inst_functor_2(dead, Real, bound(Uniq, ListX), ConsId, Args,
			_ArgLives, UI0, bound(Uniq, List), Det, UI) :-
	ListY = [functor(ConsId, Args)],
	abstractly_unify_bound_inst_list(dead, ListX, ListY, Real, UI0,
		List, Det, UI).

abstractly_unify_inst_functor_2(dead, Real, ground(Uniq, _), ConsId, ArgInsts,
		_ArgLives, UI0, Inst, Det, UI) :-
	make_ground_inst_list(ArgInsts, dead, Uniq, Real, UI0, GroundArgInsts,
		Det, UI),
	Inst = bound(Uniq, [functor(ConsId, GroundArgInsts)]).

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
		UnifyInstInfo0, L, Det0, UnifyInstInfo),
	( L = [] ->
		det_par_conjunction_detism(Det0, erroneous, Det)
	;
		Det = Det0
	).

:- pred abstractly_unify_bound_inst_list_2(is_live, list(bound_inst),
		list(bound_inst), unify_is_real, unify_inst_info,
		list(bound_inst), determinism, unify_inst_info).
:- mode abstractly_unify_bound_inst_list_2(in, in, in, in, in,
		out, out, out) is semidet.

abstractly_unify_bound_inst_list_2(_, [], [], _, UI, [], det, UI).
abstractly_unify_bound_inst_list_2(_, [], [_|_], _, UI, [], semidet, UI).
abstractly_unify_bound_inst_list_2(_, [_|_], [], _, UI, [], semidet, UI).
abstractly_unify_bound_inst_list_2(Live, [X|Xs], [Y|Ys], Real, UnifyInstInfo0,
		L, Det, UnifyInstInfo) :-
	X = functor(ConsIdX, ArgsX),
	Y = functor(ConsIdY, ArgsY),
	( ConsIdX = ConsIdY ->
		abstractly_unify_inst_list(ArgsX, ArgsY, Live, Real,
			UnifyInstInfo0, Args, Det1, UnifyInstInfo1),
		abstractly_unify_bound_inst_list_2(Live, Xs, Ys, Real,
				UnifyInstInfo1, L1, Det2, UnifyInstInfo),

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
				Xs, [Y|Ys], Real, UnifyInstInfo0, L, Det1,
				UnifyInstInfo)
		;
			abstractly_unify_bound_inst_list_2(Live,
				[X|Xs], Ys, Real, UnifyInstInfo0, L, Det1,
				UnifyInstInfo)
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
unify_uniq(live,   _, _,       unique,   unique,	    shared).
unify_uniq(live,   _, _,       unique,   mostly_unique,     shared).
unify_uniq(dead,   _, _,       unique,   unique,	    unique).
unify_uniq(dead,   _, _,       unique,   mostly_unique,     mostly_unique).
		% XXX the above line is a conservative approximation
		% sometimes it should return unique not mostly_unique
unify_uniq(Live,   Real, Det,  unique,   clobbered,	 clobbered) :-
	allow_unify_with_clobbered(Live, Real, Det).
unify_uniq(Live,   Real, Det,  unique,   mostly_clobbered,  mostly_clobbered) :-
	allow_unify_with_clobbered(Live, Real, Det).

unify_uniq(_,      _, _,       mostly_unique,    shared,    shared).
unify_uniq(live,   _, _,       mostly_unique,    unique,    shared).
unify_uniq(live,   _, _,       mostly_unique,    mostly_unique,    shared).
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
make_ground_inst(free, IsLive, Uniq0, Real, UI, ground(Uniq, no), det, UI) :-
	unify_uniq(IsLive, Real, det, unique, Uniq0, Uniq).
make_ground_inst(free(T), IsLive, Uniq0, Real, UI,
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
			unify_inst_info_get_inst_key_sub(UI0, Sub0),
			inst_apply_sub(Sub0, GroundInst0, GroundInst),
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
		inst_expand(InstTable1, ModuleInfo0, Inst0, Inst1),
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
	( inst_contains_instname(GroundInst, LastInstTable, LastModuleInfo,
			GroundInstKey) ->
		Inst = defined_inst(GroundInstKey)
	;
		Inst = GroundInst
	).
make_ground_inst(alias(InstKey), IsLive, Uniq, Real, UI0, Inst, Det, UI) :-
	unify_inst_info_get_inst_table(UI0, InstTable0),
	inst_table_get_inst_key_table(InstTable0, IKT0),
        inst_key_table_lookup(IKT0, InstKey, Inst0),
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
		unify_inst_info_get_inst_key_sub(UI1, S1),
                map__set(S1, InstKey, NewKey, S),
		unify_inst_info_set_inst_table(UI1, InstTable, UI2),
		unify_inst_info_set_inst_key_sub(UI2, S, UI)
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
make_any_inst(alias(_), _, _, _, _, _, _, _) :-
	error("make_any_inst: alias() NYI").	% YYY
make_any_inst(any(Uniq0), IsLive, Uniq1, Real, UI, any(Uniq),
		semidet, UI) :-
	allow_unify_bound_any(Real),
	unify_uniq(IsLive, Real, semidet, Uniq0, Uniq1, Uniq).
make_any_inst(free, IsLive, Uniq0, Real, UI, any(Uniq), det, UI) :-
	unify_uniq(IsLive, Real, det, unique, Uniq0, Uniq).
make_any_inst(free(T), IsLive, Uniq, Real, UI,
		defined_inst(Any), det, UI) :-
	% The following is a round-about way of doing this
	%	unify_uniq(IsLive, Real, det, unique, Uniq0, Uniq),
	%	Any = typed_any(Uniq, T).
	% without the need for a `typed_any' inst.
	Any = typed_inst(T, unify_inst(IsLive, free, any(Uniq), Real)).
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
		inst_lookup(InstTable1, ModuleInfo1, InstName, Inst0),
		inst_expand(InstTable1, ModuleInfo1, Inst0, Inst1),
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
	(
		inst_contains_instname(AnyInst, FinalInstTable, FinalModuleInfo,
			AnyInstKey)
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
:- mode maybe_make_shared_inst_list(in, in, in, out, out) is det.

maybe_make_shared_inst_list([], [], UI, [], UI).
maybe_make_shared_inst_list([Inst0 | Insts0], [IsLive | IsLives], UI0,
		[Inst | Insts], UI) :-
	( IsLive = live ->
		make_shared_inst(Inst0, UI0, Inst, UI1)
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
				list(inst), unify_inst_info).
:- mode make_shared_inst_list(in, in, out, out) is det.

make_shared_inst_list([], UI, [], UI).
make_shared_inst_list([Inst0 | Insts0], UI0, [Inst | Insts], UI) :-
	make_shared_inst(Inst0, UI0, Inst, UI1),
	make_shared_inst_list(Insts0, UI1, Insts, UI).

% make an inst shared; replace all occurrences of `unique' or `mostly_unique'
% in the inst with `shared'.

:- pred make_shared_inst(inst, unify_inst_info, inst, unify_inst_info).
:- mode make_shared_inst(in, in, out, out) is det.

make_shared_inst(not_reached, UI, not_reached, UI).
make_shared_inst(alias(Key0), UI0, Inst, UI) :-
	unify_inst_info_find_latest_inst_key(UI0, Key0, Key1),
	unify_inst_info_get_inst_table(UI0, InstTable0),
	inst_table_get_inst_key_table(InstTable0, IKT0),
	inst_key_table_lookup(IKT0, Key1, Inst0),
	make_shared_inst(Inst0, UI0, Inst1, UI1),
	unify_inst_info_find_latest_inst_key(UI1, Key1, Key),
	( Inst0 = Inst1 ->
		Inst = alias(Key),
		UI = UI1
	;
		unify_inst_info_get_inst_table(UI1, InstTable1),
		inst_table_get_inst_key_table(InstTable1, IKT1),
		inst_key_table_add(IKT1, Inst1, NewKey, IKT),
		inst_table_set_inst_key_table(InstTable1, IKT, InstTable),
		unify_inst_info_set_inst_table(UI1, InstTable, UI2),
		unify_inst_info_get_inst_key_sub(UI2, S0),
		map__set(S0, Key, NewKey, S),
		unify_inst_info_set_inst_key_sub(UI2, S, UI),
		Inst = alias(NewKey)
	).
make_shared_inst(any(Uniq0), UI, any(Uniq), UI) :-
	make_shared(Uniq0, Uniq).
make_shared_inst(free, UI, free, UI) :-
	% the caller should ensure that this never happens
	error("make_shared_inst: cannot make shared version of `free'").
make_shared_inst(free(T), UI, free(T), UI) :-
	% the caller should ensure that this never happens
	error("make_shared_inst: cannot make shared version of `free(T)'").
make_shared_inst(bound(Uniq0, BoundInsts0), UI0, bound(Uniq, BoundInsts), UI) :-
	make_shared(Uniq0, Uniq),
	make_shared_bound_inst_list(BoundInsts0, UI0, BoundInsts, UI).
make_shared_inst(ground(Uniq0, PredInst), UI, ground(Uniq, PredInst), UI) :-
	make_shared(Uniq0, Uniq).
make_shared_inst(inst_var(_), _, _, _) :-
	error("free inst var").
make_shared_inst(abstract_inst(_,_), UI, _, UI) :-
	error("make_shared_inst(abstract_inst)").
make_shared_inst(defined_inst(InstName), UI0, Inst, UI) :-
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
		UI = UI0
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
		inst_expand(InstTable1, ModuleInfo1, Inst0, Inst1),
		make_shared_inst(Inst1, UI1, SharedInst, UI2),

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
	(
		inst_contains_instname(SharedInst, LastInstTable,
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
					list(bound_inst), unify_inst_info).
:- mode make_shared_bound_inst_list(in, in, out, out) is det.

make_shared_bound_inst_list([], UI, [], UI).
make_shared_bound_inst_list([Bound0 | Bounds0], UI0, [Bound | Bounds], UI) :-
	Bound0 = functor(ConsId, ArgInsts0),
	make_shared_inst_list(ArgInsts0, UI0, ArgInsts, UI1),
	Bound = functor(ConsId, ArgInsts),
	make_shared_bound_inst_list(Bounds0, UI1, Bounds, UI).

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
make_mostly_uniq_inst_2(free, UI, free, UI).
make_mostly_uniq_inst_2(free(T), UI, free(T), UI).
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
		inst_expand(InstTable1, ModuleInfo1, Inst0, Inst1),
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
	(
		inst_contains_instname(NondetLiveInst, LastInstTable,
			LastModuleInfo, InstName)
	->
		Inst = defined_inst(InstName)
	;
		Inst = NondetLiveInst
	).
make_mostly_uniq_inst_2(alias(InstKey0), UI0, Inst, UI) :-
	unify_inst_info_find_latest_inst_key(UI0, InstKey0, InstKey1),
	unify_inst_info_get_inst_table(UI0, InstTable0),
	inst_table_get_inst_key_table(InstTable0, IKT0),
	inst_key_table_lookup(IKT0, InstKey1, Inst0),
	make_mostly_uniq_inst_2(Inst0, UI0, Inst1, UI1),
	unify_inst_info_find_latest_inst_key(UI1, InstKey1, InstKey),
	( Inst0 = Inst1 ->
		Inst = alias(InstKey),
		UI = UI1
	;
		unify_inst_info_get_inst_table(UI1, InstTable1),
		inst_table_get_inst_key_table(InstTable1, IKT1),
		inst_key_table_add(IKT1, Inst1, NewKey, IKT),
		inst_table_set_inst_key_table(InstTable1, IKT, InstTable),
		unify_inst_info_set_inst_table(UI1, InstTable, UI2),
		unify_inst_info_get_inst_key_sub(UI2, S0),
		map__set(S0, InstKey, NewKey, S),
		unify_inst_info_set_inst_key_sub(UI2, S, UI),
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

	% Should we allow unifications between bound (or ground) insts
	% and `any' insts?
	% Previously we only allowed this for fake_unifies,
	% but now we allow it for real_unifies too.

:- pred allow_unify_bound_any(unify_is_real::in) is det.
allow_unify_bound_any(_) :- true.

%-----------------------------------------------------------------------------%

	% inst_merge(InstA, InstB, InstC):
	%       Combine the insts found in different arms of a
	%       disjunction (or if-then-else).
	%       The information in InstC is the minimum of the
	%       information in InstA and InstB.  Where InstA and
	%       InstB specify a binding (free or bound), it must be
	%       the same in both.

inst_merge(InstA, InstB, InstTable0, ModuleInfo0, Inst, InstTable,
			ModuleInfo) :-
		% check whether this pair of insts is already in
		% the merge_insts table
	inst_table_get_merge_insts(InstTable0, MergeInstTable0),
	ThisInstPair = InstA - InstB,
	( map__search(MergeInstTable0, ThisInstPair, Result) ->
		ModuleInfo = ModuleInfo0,
		( Result = known(MergedInst) ->
			Inst0 = MergedInst
		;
			Inst0 = defined_inst(merge_inst(InstA, InstB))
		),
		InstTable = InstTable0
	;
			% insert ThisInstPair into the table with value
			%`unknown'
		map__det_insert(MergeInstTable0, ThisInstPair, unknown,
			MergeInstTable1),
		inst_table_set_merge_insts(InstTable0, MergeInstTable1,
			InstTable1),

			% merge the insts
		inst_merge_2(InstA, InstB, InstTable1, ModuleInfo0,
				Inst0, InstTable2, ModuleInfo),

			% now update the value associated with ThisInstPair
		inst_table_get_merge_insts(InstTable2, MergeInstTable2),
		map__det_update(MergeInstTable2, ThisInstPair,
			known(Inst0), MergeInstTable3),
		inst_table_set_merge_insts(InstTable2, MergeInstTable3,
			InstTable)
	),
		% avoid expanding recursive insts
	( inst_contains_instname(Inst0, InstTable, ModuleInfo,
			merge_inst(InstA, InstB)) ->
		Inst = defined_inst(merge_inst(InstA, InstB))
	;
		Inst = Inst0
	).

:- pred inst_merge_2(inst, inst, inst_table, module_info,
		inst, inst_table, module_info).
:- mode inst_merge_2(in, in, in, in, out, out, out) is semidet.

inst_merge_2(InstA, InstB, InstTable0, ModuleInfo0, Inst, InstTable,
		ModuleInfo) :-
/*********
		% would this test improve efficiency??
	( InstA = InstB ->
		Inst = InstA,
		ModuleInfo = ModuleInfo0
	;
*********/
	%     fixed!
	inst_expand(InstTable0, ModuleInfo0, InstA, InstA2),
	inst_expand(InstTable0, ModuleInfo0, InstB, InstB2),
	(
		InstB2 = not_reached
	->
		Inst = InstA2,
		ModuleInfo = ModuleInfo0,
		InstTable = InstTable0
	;
		% YYY The following calls implement `may alias' semantics.
		%     DO NOT merge this with the main branch without this
		InstA2 = alias(IKA)
	->
		inst_table_get_inst_key_table(InstTable0, IKT0),
		inst_key_table_lookup(IKT0, IKA, InstA3),
		inst_merge_3(InstA3, InstB2, InstTable0, ModuleInfo0, Inst,
			InstTable, ModuleInfo)
	;
		InstB2 = alias(IKB)
	->
		inst_table_get_inst_key_table(InstTable0, IKT0),
		inst_key_table_lookup(IKT0, IKB, InstB3),
		inst_merge_3(InstA2, InstB3, InstTable0, ModuleInfo0, Inst,
			InstTable, ModuleInfo)
	;
		inst_merge_3(InstA2, InstB2, InstTable0, ModuleInfo0, Inst,
			InstTable, ModuleInfo)
	).

:- pred inst_merge_3(inst, inst, inst_table, module_info, inst, inst_table,
		module_info).
:- mode inst_merge_3(in, in, in, in, out, out, out) is semidet.

% We do not yet allow merging of `free' and `any',
% except in the case where the any is `mostly_clobbered_any'
% or `clobbered_any', because that would require inserting
% additional code to initialize the free var.
%
% We do NOT plan to allow merging of `free' and `ground'
% to produce `any', because that would introduce `any'
% insts even for builtin types such as `int' which can't
% support `any'.  It might also make the mode system
% too weak -- it might not be able to detect bugs as well
% as it can currently.

inst_merge_3(any(UniqA), any(UniqB), InstTable, M, any(Uniq), InstTable, M) :-
	merge_uniq(UniqA, UniqB, Uniq).
inst_merge_3(any(Uniq), free, InstTable, M, any(Uniq), InstTable, M) :-
	% we do not yet allow merge of any with free, except for clobbered anys
	( Uniq = clobbered ; Uniq = mostly_clobbered ).
inst_merge_3(any(UniqA), bound(UniqB, ListB), InstTable, M, any(Uniq),
		InstTable, M) :-
	merge_uniq_bound(UniqA, UniqB, ListB, InstTable, M, Uniq),
	% we do not yet allow merge of any with free, except for clobbered anys
	( ( Uniq = clobbered ; Uniq = mostly_clobbered ) ->
		true
	;
		bound_inst_list_is_ground_or_any(ListB, InstTable, M)
	).
inst_merge_3(any(UniqA), ground(UniqB, _), InstTable, M, any(Uniq), InstTable,
		M) :-
	merge_uniq(UniqA, UniqB, Uniq).
inst_merge_3(any(UniqA), abstract_inst(_, _), InstTable, M, any(Uniq),
		InstTable, M) :-
	merge_uniq(UniqA, shared, Uniq),
	% we do not yet allow merge of any with free, except for clobbered anys
	( Uniq = clobbered ; Uniq = mostly_clobbered ).
inst_merge_3(free, any(Uniq), InstTable, M, any(Uniq), InstTable, M) :-
	% we do not yet allow merge of any with free, except for clobbered anys
	( Uniq = clobbered ; Uniq = mostly_clobbered ).
inst_merge_3(bound(UniqA, ListA), any(UniqB), InstTable, M, any(Uniq),
		InstTable, M) :-
	merge_uniq_bound(UniqB, UniqA, ListA, InstTable, M, Uniq),
	% we do not yet allow merge of any with free, except for clobbered anys
	( ( Uniq = clobbered ; Uniq = mostly_clobbered ) ->
		true
	;
		bound_inst_list_is_ground_or_any(ListA, InstTable, M)
	).
inst_merge_3(ground(UniqA, _), any(UniqB), InstTable, M, any(Uniq), InstTable,
		M) :-
	merge_uniq(UniqA, UniqB, Uniq).
inst_merge_3(abstract_inst(_, _), any(UniqB), InstTable, M, any(Uniq),
		InstTable, M) :-
	merge_uniq(shared, UniqB, Uniq),
	% we do not yet allow merge of any with free, except for clobbered anys
	( Uniq = clobbered ; Uniq = mostly_clobbered ).
inst_merge_3(free, free, InstTable, M, free, InstTable, M).
inst_merge_3(bound(UniqA, ListA), bound(UniqB, ListB), InstTable0, ModuleInfo0,
		bound(Uniq, List), InstTable, ModuleInfo) :-
	merge_uniq(UniqA, UniqB, Uniq),
	bound_inst_list_merge(ListA, ListB, InstTable0, ModuleInfo0, List,
		InstTable, ModuleInfo).
inst_merge_3(bound(UniqA, ListA), ground(UniqB, _), InstTable, ModuleInfo,
		ground(Uniq, no), InstTable, ModuleInfo) :-
	merge_uniq_bound(UniqB, UniqA, ListA, InstTable, ModuleInfo, Uniq),
	bound_inst_list_is_ground(ListA, InstTable, ModuleInfo).
inst_merge_3(ground(UniqA, _), bound(UniqB, ListB), InstTable, ModuleInfo,
		ground(Uniq, no), InstTable, ModuleInfo) :-
	merge_uniq_bound(UniqA, UniqB, ListB, InstTable, ModuleInfo, Uniq),
	bound_inst_list_is_ground(ListB, InstTable, ModuleInfo).
inst_merge_3(ground(UniqA, MaybePredA), ground(UniqB, MaybePredB), InstTable,
		ModuleInfo, ground(Uniq, MaybePred), InstTable, ModuleInfo) :-
	(
		MaybePredA = yes(PredA),
		MaybePredB = yes(PredB)
	->
		% if they specify matching pred insts, but one is more
		% precise (specifies more info) than the other,
		% then we want to choose the least precise one
		( pred_inst_matches(PredA, PredB, InstTable, ModuleInfo) ->
			MaybePred = yes(PredB)
		; pred_inst_matches(PredB, PredA, InstTable, ModuleInfo) ->
			MaybePred = yes(PredA)
		;
			MaybePred = no
		)
	;       
		MaybePred = no
	),
	merge_uniq(UniqA, UniqB, Uniq).
inst_merge_3(abstract_inst(Name, ArgsA), abstract_inst(Name, ArgsB),
			InstTable0, ModuleInfo0,
			abstract_inst(Name, Args), InstTable, ModuleInfo) :-
	inst_list_merge(ArgsA, ArgsB, InstTable0, ModuleInfo0, Args, InstTable, ModuleInfo).
inst_merge_3(not_reached, Inst, InstTable, M, Inst, InstTable, M).

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

:- pred merge_uniq_bound(uniqueness, uniqueness, list(bound_inst), inst_table,
			module_info, uniqueness).
:- mode merge_uniq_bound(in, in, in, in, in, out) is det.

merge_uniq_bound(UniqA, UniqB, ListB, InstTable, ModuleInfo, Uniq) :-
	merge_uniq(UniqA, UniqB, Uniq0),
	set__init(Expansions),
	merge_bound_inst_list_uniq(ListB, Uniq0, InstTable, ModuleInfo,
		Expansions, Uniq).

:- pred merge_bound_inst_list_uniq(list(bound_inst), uniqueness, inst_table,
			module_info, set(inst_name), uniqueness).
:- mode merge_bound_inst_list_uniq(in, in, in, in, in, out) is det.

merge_bound_inst_list_uniq([], Uniq, _, _, _, Uniq).
merge_bound_inst_list_uniq([BoundInst | BoundInsts], Uniq0,
			InstTable, ModuleInfo, Expansions, Uniq) :-
	BoundInst = functor(_ConsId, ArgInsts),
	merge_inst_list_uniq(ArgInsts, Uniq0, InstTable, ModuleInfo,
		Expansions, Uniq1),
	merge_bound_inst_list_uniq(BoundInsts, Uniq1, InstTable, ModuleInfo,
		Expansions, Uniq).

:- pred merge_inst_list_uniq(list(inst), uniqueness, inst_table, module_info,
			set(inst_name), uniqueness).
:- mode merge_inst_list_uniq(in, in, in, in, in, out) is det.

merge_inst_list_uniq([], Uniq, _, _, _, Uniq).
merge_inst_list_uniq([Inst | Insts], Uniq0, InstTable, ModuleInfo, Expansions,
		Uniq) :-
	merge_inst_uniq(Inst, Uniq0, InstTable, ModuleInfo, Expansions, Uniq1),
	merge_inst_list_uniq(Insts, Uniq1, InstTable, ModuleInfo, Expansions,
		Uniq).

:- pred merge_inst_uniq(inst, uniqueness, inst_table, module_info,
			set(inst_name), uniqueness).
:- mode merge_inst_uniq(in, in, in, in, in, out) is det.

merge_inst_uniq(any(UniqA), UniqB, _, _, _, Uniq) :-
	merge_uniq(UniqA, UniqB, Uniq).
merge_inst_uniq(alias(InstKey), UniqB, InstTable, ModuleInfo, Expansions,
		Uniq) :-
	inst_table_get_inst_key_table(InstTable, IKT),
	inst_key_table_lookup(IKT, InstKey, Inst),
	merge_inst_uniq(Inst, UniqB, InstTable, ModuleInfo, Expansions, Uniq).
merge_inst_uniq(free, Uniq, _, _, _, Uniq).
merge_inst_uniq(free(_), Uniq, _, _, _, Uniq).
merge_inst_uniq(bound(UniqA, ListA), UniqB, InstTable, ModuleInfo, Expansions,
		Uniq) :-
	merge_uniq(UniqA, UniqB, Uniq0),
	merge_bound_inst_list_uniq(ListA, Uniq0, InstTable, ModuleInfo,
		Expansions, Uniq).
merge_inst_uniq(ground(UniqA, _), UniqB, _, _, _, Uniq) :-
	merge_uniq(UniqA, UniqB, Uniq).
merge_inst_uniq(abstract_inst(_,_), UniqB, _, _, _, Uniq) :-
	merge_uniq(shared, UniqB, Uniq).
merge_inst_uniq(defined_inst(InstName), UniqB, InstTable, ModuleInfo,
		Expansions, Uniq) :-
	( set__member(InstName, Expansions) ->
		Uniq = UniqB
	;
		set__insert(Expansions, InstName, Expansions1),
		inst_lookup(InstTable, ModuleInfo, InstName, Inst),
		merge_inst_uniq(Inst, UniqB, InstTable, ModuleInfo, Expansions1,
			Uniq)
	).
merge_inst_uniq(not_reached, Uniq, _, _, _, Uniq).
merge_inst_uniq(inst_var(_), _, _, _, _, _) :-
	error("merge_inst_uniq: unexpected inst_var").

%-----------------------------------------------------------------------------%

:- pred inst_list_merge(list(inst), list(inst), inst_table, module_info,
		list(inst), inst_table, module_info).
:- mode inst_list_merge(in, in, in, in, out, out, out) is semidet.

inst_list_merge([], [], InstTable, ModuleInfo, [], InstTable, ModuleInfo).
inst_list_merge([ArgA | ArgsA], [ArgB | ArgsB], InstTable0, ModuleInfo0,
		[Arg | Args], InstTable, ModuleInfo) :-
	inst_merge(ArgA, ArgB, InstTable0, ModuleInfo0, Arg, InstTable1,
		ModuleInfo1),
	inst_list_merge(ArgsA, ArgsB, InstTable1, ModuleInfo1, Args, InstTable,
		ModuleInfo).

	% bound_inst_list_merge(Xs, Ys, InstTable0, ModuleInfo0, Zs, InstTable,
	%	ModuleInfo):
	% The two input lists Xs and Ys must already be sorted.
	% Here we perform a sorted merge operation,
	% so that the functors of the output list Zs are the union
	% of the functors of the input lists Xs and Ys.

:- pred bound_inst_list_merge(list(bound_inst), list(bound_inst),
			inst_table, module_info,
			list(bound_inst), inst_table, module_info).
:- mode bound_inst_list_merge(in, in, in, in, out, out, out) is semidet.

bound_inst_list_merge(Xs, Ys, InstTable0, ModuleInfo0, Zs, InstTable,
		ModuleInfo) :-
	( Xs = [] ->
		Zs = Ys,
		ModuleInfo = ModuleInfo0,
		InstTable = InstTable0
	; Ys = [] ->
		Zs = Xs,
		ModuleInfo = ModuleInfo0,
		InstTable = InstTable0
	;
		Xs = [X | Xs1],
		Ys = [Y | Ys1],
		X = functor(ConsIdX, ArgsX),
		Y = functor(ConsIdY, ArgsY),
		( ConsIdX = ConsIdY ->
			inst_list_merge(ArgsX, ArgsY, InstTable0, ModuleInfo0,
					Args, InstTable1, ModuleInfo1),
			Z = functor(ConsIdX, Args),
			Zs = [Z | Zs1],
			bound_inst_list_merge(Xs1, Ys1, InstTable1, ModuleInfo1,
				Zs1, InstTable, ModuleInfo)
		; compare(<, ConsIdX, ConsIdY) ->
			Zs = [X | Zs1],
			bound_inst_list_merge(Xs1, Ys, InstTable0, ModuleInfo0,
						Zs1, InstTable, ModuleInfo)
		;
			Zs = [Y | Zs1],
			bound_inst_list_merge(Xs, Ys1, InstTable0, ModuleInfo0,
						Zs1, InstTable, ModuleInfo)
		)
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

inst_table_create_sub(InstTable0, NewInstTable, Sub, InstTable) :-
	inst_table_get_all_tables(InstTable0, UnifyInstTable0,
	        MergeInstTable0, GroundInstTable0, AnyInstTable0,
	        SharedInstTable0, MostlyUniqInstTable0, IKT0),
	inst_table_get_all_tables(NewInstTable, NewUnifyInstTable,
	        NewMergeInstTable, NewGroundInstTable, NewAnyInstTable,
	        NewSharedInstTable, NewMostlyUniqInstTable, NewIKT),
	inst_key_table_create_sub(IKT0, NewIKT, Sub0, IKT),

	maybe_inst_det_table_apply_sub(UnifyInstTable0, NewUnifyInstTable,
		UnifyInstTable, Sub0),

	Sub1 = Sub0,
	( map__is_empty(NewMergeInstTable) ->
		MergeInstTable = MergeInstTable0,
		Sub2 = Sub1
	;
		error("NYI: inst_table_create_sub (merge_inst_table)")
	),
	( map__is_empty(NewGroundInstTable) ->
		GroundInstTable = GroundInstTable0,
		Sub3 = Sub2
	;
		error("NYI: inst_table_create_sub (ground_inst_table)")
	),
	( map__is_empty(NewAnyInstTable) ->
		AnyInstTable = AnyInstTable0,
		Sub4 = Sub3
	;
		error("NYI: inst_table_create_sub (any_inst_table)")
	),
	( map__is_empty(NewSharedInstTable) ->
		SharedInstTable = SharedInstTable0,
		Sub5 = Sub4
	;
		error("NYI: inst_table_create_sub (shared_inst_table)")
	),
	( map__is_empty(NewMostlyUniqInstTable) ->
		MostlyUniqInstTable = MostlyUniqInstTable0,
		Sub = Sub5
	;
		error("NYI: inst_table_create_sub (mostly_uniq_inst_table)")
	),
	inst_table_set_all_tables(InstTable0, UnifyInstTable,
	        MergeInstTable, GroundInstTable, AnyInstTable,
	        SharedInstTable, MostlyUniqInstTable, IKT, InstTable).

:- pred maybe_inst_det_table_apply_sub(map(inst_name, maybe_inst_det),
		map(inst_name, maybe_inst_det), map(inst_name, maybe_inst_det),
		inst_key_sub).
:- mode maybe_inst_det_table_apply_sub(in, in, out, in) is det.

maybe_inst_det_table_apply_sub(Table0, NewTable, Table, Sub) :-
	( map__is_empty(Table0) ->
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
	map__det_insert(Table0, InstName, InstDet, Table1),
	maybe_inst_det_table_apply_sub_2(Rest, Table1, Table, Sub).

:- pred inst_name_apply_sub(inst_key_sub, inst_name, inst_name).
:- mode inst_name_apply_sub(in, in, out) is det.

inst_name_apply_sub(Sub, user_inst(Sym, Insts0), user_inst(Sym, Insts)) :-
	list__map(inst_apply_sub(Sub), Insts0, Insts).
inst_name_apply_sub(Sub, merge_inst(InstA0, InstB0),
		merge_inst(InstA, InstB)) :-
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


%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
