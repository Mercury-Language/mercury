%-----------------------------------------------------------------------------%
% Copyright (C) 1997-2001 The University of Melbourne.
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

:- import_module hlds_module, hlds_data, prog_data, (inst).
:- import_module list, std_util.

:- pred abstractly_unify_inst(is_live, inst, inst, unify_is_real, module_info,
				inst, determinism, module_info).
:- mode abstractly_unify_inst(in, in, in, in, in, out, out, out) is semidet.

	% Compute the inst that results from abstractly unifying two variables.

:- pred abstractly_unify_inst_functor(is_live, inst, cons_id, list(inst),
				list(is_live), unify_is_real, module_info,
				inst, determinism, module_info).
:- mode abstractly_unify_inst_functor(in, in, in, in, in, in, in, out, out, out)
	is semidet.

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

:- pred make_mostly_uniq_inst(inst, module_info, inst, module_info).
:- mode make_mostly_uniq_inst(in, in, out, out) is det.

	% Given an inst, return a new inst which is the same as the
	% original inst but with all occurrences of `unique' replaced
	% with `mostly_unique'.

:- pred make_shared_inst_list(list(inst), module_info, list(inst), module_info).
:- mode make_shared_inst_list(in, in, out, out) is det.

	% Given a list of insts, return a new list of insts which is the
	% same as the original list of insts, but with all occurrences
	% of `unique' replaced with `shared'.  It is an error if any part
	% of the inst list is free.

%-----------------------------------------------------------------------------%

:- pred inst_merge(inst, inst, maybe(type), module_info, inst, module_info).
:- mode inst_merge(in, in, in, in, out, out) is semidet.

	% inst_merge(InstA, InstB, InstC):
	%       Combine the insts found in different arms of a
	%       disjunction (or if-then-else).
	%       The information in InstC is the minimum of the
	%       information in InstA and InstB.  Where InstA and
	%       InstB specify a binding (free or bound), it must be
	%       the same in both.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module hlds_data, inst_match, mode_util, det_analysis, type_util.
:- import_module bool, std_util, require, map, list, set, int.

	% Abstractly unify two insts.

abstractly_unify_inst(Live, InstA, InstB, UnifyIsReal, ModuleInfo0,
			Inst, Det, ModuleInfo) :-
		% check whether this pair of insts is already in
		% the unify_insts table
	ThisInstPair = unify_inst(Live, InstA, InstB, UnifyIsReal),
	module_info_insts(ModuleInfo0, InstTable0),
	inst_table_get_unify_insts(InstTable0, UnifyInsts0),
	( map__search(UnifyInsts0, ThisInstPair, Result) ->
		( Result = known(UnifyInst, UnifyDet) ->
			Inst0 = UnifyInst,
			Det = UnifyDet
		;
			Inst0 = defined_inst(ThisInstPair),
				% It's ok to assume that the unification is
				% deterministic here, because the only time that
				% this will happen is when we get to the
				% recursive case for a recursively defined inst.
				% If the unification as a whole is semidet then
				% it must be semidet somewhere else too.
			Det = det
		),
		ModuleInfo = ModuleInfo0,
		Inst1 = Inst0
	;
			% insert ThisInstPair into the table with value
			% `unknown'
		map__det_insert(UnifyInsts0, ThisInstPair, unknown,
			UnifyInsts1),
		inst_table_set_unify_insts(InstTable0, UnifyInsts1, InstTable1),
		module_info_set_insts(ModuleInfo0, InstTable1, ModuleInfo1),
			% unify the insts
		inst_expand(ModuleInfo0, InstA, InstA2),
		inst_expand(ModuleInfo0, InstB, InstB2),
		abstractly_unify_inst_2(Live, InstA2, InstB2, UnifyIsReal,
			ModuleInfo1, Inst0, Det, ModuleInfo2),

			% If this unification cannot possible succeed,
			% the correct inst is not_reached.
		( determinism_components(Det, _, at_most_zero) ->
			Inst1 = not_reached
		;
			Inst1 = Inst0
		),

			% now update the value associated with ThisInstPair
		module_info_insts(ModuleInfo2, InstTable2),
		inst_table_get_unify_insts(InstTable2, UnifyInsts2),
		map__det_update(UnifyInsts2, ThisInstPair, known(Inst1, Det),
			UnifyInsts),
		inst_table_set_unify_insts(InstTable2, UnifyInsts, InstTable),
		module_info_set_insts(ModuleInfo2, InstTable, ModuleInfo)
	),
		% avoid expanding recursive insts
	( inst_contains_instname(Inst1, ModuleInfo, ThisInstPair) ->
		Inst = defined_inst(ThisInstPair)
	;
		Inst = Inst1
	).

:- pred abstractly_unify_inst_2(is_live, inst, inst, unify_is_real, module_info,
				inst, determinism, module_info).
:- mode abstractly_unify_inst_2(in, in, in, in, in, out, out, out) is semidet.

abstractly_unify_inst_2(IsLive, InstA, InstB, UnifyIsReal, ModuleInfo0,
		Inst, Det, ModuleInfo) :-
	( InstB = not_reached ->
		Inst = not_reached,
		Det = det,
		ModuleInfo = ModuleInfo0
	;
		abstractly_unify_inst_3(IsLive, InstA, InstB, UnifyIsReal,
			ModuleInfo0, Inst, Det, ModuleInfo)
	).

	% Abstractly unify two expanded insts.
	% The is_live parameter is `live' iff *both* insts are live.
	% Given the two insts to be unified, this produces
	% a resulting inst and a determinism for the unification.

:- pred abstractly_unify_inst_3(is_live, inst, inst, unify_is_real, module_info,
				inst, determinism, module_info).
:- mode abstractly_unify_inst_3(in, in, in, in, in, out, out, out) is semidet.

% XXX could be extended to handle `any' insts better

abstractly_unify_inst_3(live, not_reached, _, _,	M, not_reached, det, M).

abstractly_unify_inst_3(live, any(Uniq), Inst0, Real, M0, Inst, Det, M) :-
	make_any_inst(Inst0, live, Uniq, Real, M0, Inst, Det, M).

abstractly_unify_inst_3(live, free, any(UniqY), Real, M,
					any(Uniq), det, M) :-
	unify_uniq(live, Real, det, unique, UniqY, Uniq).

% abstractly_unify_inst_3(live, free,   free, _,	_, _, _, _) :- fail.

abstractly_unify_inst_3(live, free,     bound(UniqY, List0), Real, M0,
					bound(Uniq, List), det, M) :-
	unify_uniq(live, Real, det, unique, UniqY, Uniq),
		% since both are live, we must disallow free-free unifications
	bound_inst_list_is_ground_or_any(List0, M0),
		% since both are live, we must make the result shared
		% (unless it was already shared)
	( ( UniqY = unique ; UniqY = mostly_unique ) ->
		make_shared_bound_inst_list(List0, M0, List, M)
	;
		List = List0, M = M0
	).

abstractly_unify_inst_3(live, free,     ground(UniqY, PredInst), Real, M,
					ground(Uniq, PredInst), det, M) :-
	unify_uniq(live, Real, det, unique, UniqY, Uniq).

% abstractly_unify_inst_3(live, free,   abstract_inst(_,_), _, _, _, _) :- fail.

abstractly_unify_inst_3(live, bound(UniqX, List0), any(UniqY),  Real, M0,
					bound(Uniq, List), Det, M) :-
	allow_unify_bound_any(Real),
	unify_uniq(live, Real, semidet, UniqX, UniqY, Uniq),
	make_any_bound_inst_list(List0, live, UniqY, Real, M0,
			List, Det1, M),
	det_par_conjunction_detism(Det1, semidet, Det).

abstractly_unify_inst_3(live,	   bound(UniqY, List0), free, Real, M0,
					bound(Uniq, List), det,  M) :-
	unify_uniq(live, Real, det, unique, UniqY, Uniq),
		% since both are live, we must disallow free-free unifications
	bound_inst_list_is_ground_or_any(List0, M0),
	make_shared_bound_inst_list(List0, M0, List, M).

abstractly_unify_inst_3(live, bound(UniqX, ListX), bound(UniqY, ListY), Real,
			M0,     bound(Uniq, List), Det, M) :-
	abstractly_unify_bound_inst_list(live, ListX, ListY, Real, M0,
		List, Det, M),
	unify_uniq(live, Real, Det, UniqX, UniqY, Uniq).

abstractly_unify_inst_3(live, bound(UniqX, BoundInsts0), ground(UniqY, _),
		Real, M0, bound(Uniq, BoundInsts), Det, M) :-
	unify_uniq(live, Real, semidet, UniqX, UniqY, Uniq),
	make_ground_bound_inst_list(BoundInsts0, live, UniqY, Real, M0,
			BoundInsts, Det1, M),
	det_par_conjunction_detism(Det1, semidet, Det).

/*** abstract insts not supported
abstractly_unify_inst_3(live, bound(Uniq, List), abstract_inst(_,_), Real, M,
					ground(shared), semidet, M) :-
	unify_uniq(live, Real, semidet, unique, UniqY, Uniq),
	bound_inst_list_is_ground(List, M).
***/

abstractly_unify_inst_3(live, ground(UniqX, higher_order(PredInst)),
		any(UniqY), Real, M, ground(Uniq, higher_order(PredInst)),
		semidet, M) :-
	Real = fake_unify,
	unify_uniq(live, Real, det, UniqX, UniqY, Uniq).

abstractly_unify_inst_3(live, ground(Uniq0, higher_order(PredInst)), free,
		Real, M, ground(Uniq, higher_order(PredInst)), det, M) :-
	unify_uniq(live, Real, det, unique, Uniq0, Uniq).

abstractly_unify_inst_3(live, ground(UniqX, higher_order(_)),
		bound(UniqY, BoundInsts0), Real, M0, bound(Uniq, BoundInsts),
		Det, M) :-
	% check `Real = fake_unify' ?
	unify_uniq(live, Real, semidet, UniqX, UniqY, Uniq),
	make_ground_bound_inst_list(BoundInsts0, live, UniqX, Real, M0,
			BoundInsts, Det1, M),
	det_par_conjunction_detism(Det1, semidet, Det).

abstractly_unify_inst_3(live, ground(UniqA, higher_order(PredInstA)),
				ground(UniqB, _GroundInstInfoB), Real, M,
				ground(Uniq, GroundInstInfo), semidet, M) :-
	% It is an error to unify higher-order preds,
	% so if Real \= fake_unify, then we must fail.
	Real = fake_unify,
	% In theory we should choose take the union of the
	% information specified by PredInstA and _GroundInstInfoB.
	% However, since our data representation provides no
	% way of doing that, and since this will only happen
	% for fake_unifys, for which it shouldn't make any difference,
	% we just choose the information specified by PredInstA.
	GroundInstInfo = higher_order(PredInstA),
	unify_uniq(live, Real, semidet, UniqA, UniqB, Uniq).

abstractly_unify_inst_3(live, ground(Uniq, none), Inst0, Real, M0,
				Inst, Det, M) :-
	make_ground_inst(Inst0, live, Uniq, Real, M0, Inst, Det, M).

abstractly_unify_inst_3(live, ground(UniqX, constrained_inst_var(Var)),
		any(UniqY), Real, M, ground(Uniq, constrained_inst_var(Var)),
		semidet, M) :-
	unify_uniq(live, Real, det, UniqX, UniqY, Uniq).

abstractly_unify_inst_3(live, ground(Uniq0, constrained_inst_var(Var)), free,
		Real, M, ground(Uniq, constrained_inst_var(Var)), det, M) :-
	unify_uniq(live, Real, det, unique, Uniq0, Uniq).

abstractly_unify_inst_3(live, ground(UniqX, constrained_inst_var(_)),
		bound(UniqY, BoundInsts0), Real, M0, bound(Uniq, BoundInsts),
		Det, M) :-
	unify_uniq(live, Real, semidet, UniqX, UniqY, Uniq),
	make_ground_bound_inst_list(BoundInsts0, live, UniqX, Real, M0,
			BoundInsts, Det1, M),
	det_par_conjunction_detism(Det1, semidet, Det).

abstractly_unify_inst_3(live, ground(UniqA, constrained_inst_var(_V)),
		ground(UniqB, GII), Real, M, ground(Uniq, GII), semidet, M) :-
	unify_uniq(live, Real, semidet, UniqA, UniqB, Uniq).

% abstractly_unify_inst_3(live, abstract_inst(_,_), free,       _, _, _, _, _)
%       :- fail.

/*** abstract insts not supported
abstractly_unify_inst_3(live, abstract_inst(_,_), bound(Uniq, List), Real,
		ModuleInfo, ground(shared, no), semidet, ModuleInfo) :-
	check_not_clobbered(Real, Uniq),
	bound_inst_list_is_ground(List, ModuleInfo).

abstractly_unify_inst_3(live, abstract_inst(_,_), ground(Uniq, no), Real, M,
				ground(shared, no), semidet, M) :-
	check_not_clobbered(Real, Uniq).

abstractly_unify_inst_3(live, abstract_inst(Name, ArgsA),
			abstract_inst(Name, ArgsB), Real, ModuleInfo0,
			abstract_inst(Name, Args), Det, ModuleInfo) :-
	abstractly_unify_inst_list(ArgsA, ArgsB, live, Real, ModuleInfo0,
		Args, Det, ModuleInfo).
***/

abstractly_unify_inst_3(dead, not_reached, _, _, M, not_reached, det, M).

abstractly_unify_inst_3(dead, any(Uniq), Inst0, Real, M0, Inst, Det, M) :-
	make_any_inst(Inst0, dead, Uniq, Real, M0, Inst, Det, M).

abstractly_unify_inst_3(dead, free, Inst, _, M, Inst, det, M).

abstractly_unify_inst_3(dead, bound(UniqX, List0), any(UniqY), Real, M0,
					bound(Uniq, List), Det, M) :-
	allow_unify_bound_any(Real),
	unify_uniq(dead, Real, semidet, UniqX, UniqY, Uniq),
	make_any_bound_inst_list(List0, live, UniqY, Real, M0,
					List, Det1, M),
	det_par_conjunction_detism(Det1, semidet, Det).

abstractly_unify_inst_3(dead, bound(UniqX, List), free, Real, ModuleInfo,
				bound(Uniq, List), det, ModuleInfo) :-
	unify_uniq(dead, Real, det, UniqX, unique, Uniq).

abstractly_unify_inst_3(dead, bound(UniqX, ListX), bound(UniqY, ListY),
			Real, M0, bound(Uniq, List), Det, M) :-
	abstractly_unify_bound_inst_list(dead, ListX, ListY, Real, M0,
		List, Det, M),
	unify_uniq(dead, Real, Det, UniqX, UniqY, Uniq).

abstractly_unify_inst_3(dead, bound(UniqX, BoundInsts0), ground(UniqY, _),
			Real, M0, bound(Uniq, BoundInsts), Det, M) :-
	unify_uniq(dead, Real, semidet, UniqX, UniqY, Uniq),
	make_ground_bound_inst_list(BoundInsts0, dead, UniqY, Real, M0,
					BoundInsts, Det1, M),
	det_par_conjunction_detism(Det1, semidet, Det).

/***** abstract insts aren't really supported
abstractly_unify_inst_3(dead, bound(Uniq, List), abstract_inst(N,As),
			ModuleInfo, Result, Det, ModuleInfo) :-
	( bound_inst_list_is_ground(List, ModuleInfo) ->
		Result = bound(Uniq, List),
		Det = semidet
	; bound_inst_list_is_free(List, ModuleInfo) ->
		Result = abstract_inst(N,As),
		Det = det
	;
		fail
	).
*****/

abstractly_unify_inst_3(dead, ground(UniqX, higher_order(PredInst)),
		any(UniqY), Real, M, ground(Uniq, higher_order(PredInst)),
		semidet, M) :-
	allow_unify_bound_any(Real),
	unify_uniq(dead, Real, semidet, UniqX, UniqY, Uniq).

abstractly_unify_inst_3(dead, ground(Uniq, higher_order(PredInst)), free,
		_Real, M, ground(Uniq, higher_order(PredInst)), det, M).

abstractly_unify_inst_3(dead, ground(UniqA, higher_order(_)),
		bound(UniqB, BoundInsts0), Real, M0, bound(Uniq, BoundInsts),
		Det, M) :-
	unify_uniq(dead, Real, semidet, UniqA, UniqB, Uniq),
	make_ground_bound_inst_list(BoundInsts0, dead, UniqA, Real, M0,
					BoundInsts, Det1, M),
	det_par_conjunction_detism(Det1, semidet, Det).

abstractly_unify_inst_3(dead, ground(UniqA, higher_order(PredInstA)),
				ground(UniqB, _GroundInstInfoB), Real, M,
				ground(Uniq, GroundInstInfo), det, M) :-
	Real = fake_unify,
	GroundInstInfo = higher_order(PredInstA),
	unify_uniq(dead, Real, det, UniqA, UniqB, Uniq).

abstractly_unify_inst_3(dead, ground(Uniq, none), Inst0, Real, M0,
				Inst, Det, M) :-
	make_ground_inst(Inst0, dead, Uniq, Real, M0, Inst, Det, M).

abstractly_unify_inst_3(dead, ground(UniqX, constrained_inst_var(Var)),
		any(UniqY), Real, M, ground(Uniq, constrained_inst_var(Var)),
		semidet, M) :-
	allow_unify_bound_any(Real),
	unify_uniq(dead, Real, semidet, UniqX, UniqY, Uniq).

abstractly_unify_inst_3(dead, ground(Uniq, constrained_inst_var(Var)), free,
		_Real, M, ground(Uniq, constrained_inst_var(Var)), det, M).

abstractly_unify_inst_3(dead, ground(UniqA, constrained_inst_var(_)),
		bound(UniqB, BoundInsts0), Real, M0, bound(Uniq, BoundInsts),
		Det, M) :-
	unify_uniq(dead, Real, semidet, UniqA, UniqB, Uniq),
	make_ground_bound_inst_list(BoundInsts0, dead, UniqA, Real, M0,
					BoundInsts, Det1, M),
	det_par_conjunction_detism(Det1, semidet, Det).

abstractly_unify_inst_3(dead, ground(UniqA, constrained_inst_var(_Var)),
				ground(UniqB, GroundInstInfo), Real, M,
				ground(Uniq, GroundInstInfo), det, M) :-
	unify_uniq(dead, Real, det, UniqA, UniqB, Uniq).

/***** abstract insts aren't really supported
abstractly_unify_inst_3(dead, abstract_inst(N,As), bound(List), Real,
			ModuleInfo, Result, Det, ModuleInfo) :-
	( bound_inst_list_is_ground(List, ModuleInfo) ->
		Result = bound(List),
		Det = semidet
	; bound_inst_list_is_free(List, ModuleInfo) ->
		Result = abstract_inst(N,As),
		Det = det
	;
		fail
	).

abstractly_unify_inst_3(dead, abstract_inst(_,_), ground, _Real, ModuleInfo,
		ground, semidet, ModuleInfo).

abstractly_unify_inst_3(dead, abstract_inst(Name, ArgsA),
			abstract_inst(Name, ArgsB), Real, ModuleInfo0,
			abstract_inst(Name, Args), Det, ModuleInfo) :-
	abstractly_unify_inst_list(ArgsA, ArgsB, dead, Real, ModuleInfo0,
			Args, Det, ModuleInfo).

*****/

%-----------------------------------------------------------------------------%

	% Abstractly unify two inst lists.

:- pred abstractly_unify_inst_list(list(inst), list(inst), is_live,
					unify_is_real, module_info,
					list(inst), determinism, module_info).
:- mode abstractly_unify_inst_list(in, in, in, in, in, out, out, out)
	is semidet.

abstractly_unify_inst_list([], [], _, _, M, [], det, M).
abstractly_unify_inst_list([X|Xs], [Y|Ys], Live, Real, ModuleInfo0,
				[Z|Zs], Det, ModuleInfo) :-
	abstractly_unify_inst(Live, X, Y, Real, ModuleInfo0,
		Z, Det1, ModuleInfo1),
	abstractly_unify_inst_list(Xs, Ys, Live, Real, ModuleInfo1,
		Zs, Det2, ModuleInfo),
	det_par_conjunction_detism(Det1, Det2, Det).

%-----------------------------------------------------------------------------%

	% This is the abstract unification operation which
	% unifies a variable (or rather, it's instantiatedness)
	% with a functor.

abstractly_unify_inst_functor(Live, InstA, ConsId, ArgInsts, ArgLives,
		Real, ModuleInfo0, Inst, Det, ModuleInfo) :-
	inst_expand(ModuleInfo0, InstA, InstA2),
	abstractly_unify_inst_functor_2(Live, InstA2, ConsId, ArgInsts,
			ArgLives, Real, ModuleInfo0, Inst, Det, ModuleInfo).

:- pred abstractly_unify_inst_functor_2(is_live, inst, cons_id, list(inst),
			list(is_live), unify_is_real, module_info,
			inst, determinism, module_info).
:- mode abstractly_unify_inst_functor_2(in, in, in, in, in, in, in,
			out, out, out) is semidet.

	% XXX need to handle `any' insts

abstractly_unify_inst_functor_2(live, not_reached, _, _, _, _, M,
			not_reached, erroneous, M).

abstractly_unify_inst_functor_2(live, free, ConsId, Args0, ArgLives, _Real,
			ModuleInfo0,
			bound(unique, [functor(ConsId, Args)]), det,
			ModuleInfo) :-
	inst_list_is_ground_or_any_or_dead(Args0, ArgLives, ModuleInfo0),
	maybe_make_shared_inst_list(Args0, ArgLives, ModuleInfo0,
			Args, ModuleInfo).

abstractly_unify_inst_functor_2(live, bound(Uniq, ListX), ConsId, Args,
			ArgLives, Real, M0, bound(Uniq, List), Det, M) :-
	abstractly_unify_bound_inst_list_lives(ListX, ConsId, Args, ArgLives,
					Real, M0, List, Det, M).

abstractly_unify_inst_functor_2(live, ground(Uniq, _), ConsId, ArgInsts,
		ArgLives, Real, M0, Inst, Det, M) :-
	make_ground_inst_list_lives(ArgInsts, live, ArgLives, Uniq, Real, M0,
		GroundArgInsts, Det, M),
	Inst = bound(Uniq, [functor(ConsId, GroundArgInsts)]).

% abstractly_unify_inst_functor_2(live, abstract_inst(_,_), _, _, _, _, _,
%		_, _) :-
%       fail.

abstractly_unify_inst_functor_2(dead, not_reached, _, _, _, _, M,
					not_reached, erroneous, M).

abstractly_unify_inst_functor_2(dead, free, ConsId, Args, _ArgLives, _Real, M,
			bound(unique, [functor(ConsId, Args)]), det, M).

abstractly_unify_inst_functor_2(dead, bound(Uniq, ListX), ConsId, Args,
			_ArgLives, Real, M0, bound(Uniq, List), Det, M) :-
	ListY = [functor(ConsId, Args)],
	abstractly_unify_bound_inst_list(dead, ListX, ListY, Real, M0,
		List, Det, M).

abstractly_unify_inst_functor_2(dead, ground(Uniq, _), ConsId, ArgInsts,
		_ArgLives, Real, M0, Inst, Det, M) :-
	make_ground_inst_list(ArgInsts, dead, Uniq, Real, M0,
		GroundArgInsts, Det, M),
	Inst = bound(Uniq, [functor(ConsId, GroundArgInsts)]).

% abstractly_unify_inst_functor_2(dead, abstract_inst(_,_), _, _, _, _,
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
		list(bound_inst), unify_is_real, module_info,
		list(bound_inst), determinism, module_info).
:- mode abstractly_unify_bound_inst_list(in, in, in, in, in,
		out, out, out) is semidet.

abstractly_unify_bound_inst_list(Live, Xs, Ys, Real, ModuleInfo0, L, Det,
		ModuleInfo) :-
	abstractly_unify_bound_inst_list_2(Live, Xs, Ys, Real,
		ModuleInfo0, 0, L, Det0, ModuleInfo),
	( L = [] ->
		det_par_conjunction_detism(Det0, erroneous, Det)
	;
		Det = Det0
	).

:- pred abstractly_unify_bound_inst_list_2(is_live, list(bound_inst),
		list(bound_inst), unify_is_real, module_info, int,
		list(bound_inst), determinism, module_info).
:- mode abstractly_unify_bound_inst_list_2(in, in, in, in, in, in,
		out, out, out) is semidet.

abstractly_unify_bound_inst_list_2(_, [], [], _, ModuleInfo, N, [], Det,
		ModuleInfo) :-
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
abstractly_unify_bound_inst_list_2(_, [], [_|_], _, M, _, [], semidet, M).
abstractly_unify_bound_inst_list_2(_, [_|_], [], _, M, _, [], semidet, M).
abstractly_unify_bound_inst_list_2(Live, [X|Xs], [Y|Ys], Real, ModuleInfo0,
		N, L, Det, ModuleInfo) :-
	X = functor(ConsIdX, ArgsX),
	Y = functor(ConsIdY, ArgsY),
	( ConsIdX = ConsIdY ->
		abstractly_unify_inst_list(ArgsX, ArgsY, Live, Real,
			ModuleInfo0, Args, Det1, ModuleInfo1),
		abstractly_unify_bound_inst_list_2(Live, Xs, Ys, Real,
					ModuleInfo1, N+1, L1, Det2, ModuleInfo),

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
			abstractly_unify_bound_inst_list_2(Live, Xs, [Y|Ys],
				Real, ModuleInfo0, N+1, L, Det1, ModuleInfo)
		;
			abstractly_unify_bound_inst_list_2(Live, [X|Xs], Ys,
				Real, ModuleInfo0, N+1, L, Det1, ModuleInfo)
		),
		det_par_conjunction_detism(Det1, semidet, Det)
	).

:- pred abstractly_unify_bound_inst_list_lives(list(bound_inst), cons_id,
	list(inst), list(is_live), unify_is_real, module_info,
	list(bound_inst), determinism, module_info).
:- mode abstractly_unify_bound_inst_list_lives(in, in, in, in, in, in,
	out, out, out) is semidet.

abstractly_unify_bound_inst_list_lives([], _, _, _, _, ModuleInfo,
					[], failure, ModuleInfo).
abstractly_unify_bound_inst_list_lives([X|Xs], ConsIdY, ArgsY, LivesY, Real,
		ModuleInfo0, L, Det, ModuleInfo) :-
	X = functor(ConsIdX, ArgsX),
	(
		ConsIdX = ConsIdY
	->
		abstractly_unify_inst_list_lives(ArgsX, ArgsY, LivesY, Real,
			ModuleInfo0, Args, Det, ModuleInfo),
		L = [functor(ConsIdX, Args)]
	;
		abstractly_unify_bound_inst_list_lives(Xs, ConsIdY, ArgsY,
			LivesY, Real, ModuleInfo0, L, Det, ModuleInfo)
	).

:- pred abstractly_unify_inst_list_lives(list(inst), list(inst), list(is_live),
	unify_is_real, module_info, list(inst), determinism, module_info).
:- mode abstractly_unify_inst_list_lives(in, in, in, in, in, out, out, out)
	is semidet.

abstractly_unify_inst_list_lives([], [], [], _, ModuleInfo,
		[], det, ModuleInfo).
abstractly_unify_inst_list_lives([X|Xs], [Y|Ys], [Live|Lives], Real,
		ModuleInfo0, [Z|Zs], Det, ModuleInfo) :-
	abstractly_unify_inst(Live, X, Y, Real, ModuleInfo0,
			Z, Det1, ModuleInfo1),
	abstractly_unify_inst_list_lives(Xs, Ys, Lives, Real, ModuleInfo1,
			Zs, Det2, ModuleInfo),
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
			uniqueness, unify_is_real,
			module_info, list(inst), determinism, module_info).
:- mode make_ground_inst_list_lives(in, in, in, in, in, in, out, out, out)
				is semidet.

make_ground_inst_list_lives([], _, _, _, _, ModuleInfo, [], det, ModuleInfo).
make_ground_inst_list_lives([Inst0 | Insts0], Live, [ArgLive | ArgLives],
		Uniq, Real, ModuleInfo0, [Inst | Insts], Det, ModuleInfo) :-
	( Live = live, ArgLive = live ->
		BothLive = live
	;
		BothLive = dead
	),
	make_ground_inst(Inst0, BothLive, Uniq, Real, ModuleInfo0,
		Inst, Det1, ModuleInfo1),
	make_ground_inst_list_lives(Insts0, Live, ArgLives, Uniq, Real,
		ModuleInfo1, Insts, Det2, ModuleInfo),
	det_par_conjunction_detism(Det1, Det2, Det).

:- pred make_ground_inst_list(list(inst), is_live, uniqueness, unify_is_real,
			module_info, list(inst), determinism, module_info).
:- mode make_ground_inst_list(in, in, in, in, in, out, out, out) is semidet.

make_ground_inst_list([], _, _, _, ModuleInfo, [], det, ModuleInfo).
make_ground_inst_list([Inst0 | Insts0], Live, Uniq, Real, ModuleInfo0,
		[Inst | Insts], Det, ModuleInfo) :-
	make_ground_inst(Inst0, Live, Uniq, Real, ModuleInfo0,
		Inst, Det1, ModuleInfo1),
	make_ground_inst_list(Insts0, Live, Uniq, Real, ModuleInfo1,
		Insts, Det2, ModuleInfo),
	det_par_conjunction_detism(Det1, Det2, Det).

% abstractly unify an inst with `ground' and calculate the new inst
% and the determinism of the unification.

:- pred make_ground_inst(inst, is_live, uniqueness, unify_is_real, module_info,
				inst, determinism, module_info).
:- mode make_ground_inst(in, in, in, in, in, out, out, out) is semidet.

make_ground_inst(not_reached, _, _, _, M, not_reached, erroneous, M).
make_ground_inst(any(Uniq0), IsLive, Uniq1, Real, M, ground(Uniq, none),
		semidet, M) :-
	unify_uniq(IsLive, Real, semidet, Uniq0, Uniq1, Uniq).
make_ground_inst(free, IsLive, Uniq0, Real, M, ground(Uniq, none), det, M) :-
	unify_uniq(IsLive, Real, det, unique, Uniq0, Uniq).
make_ground_inst(free(T), IsLive, Uniq0, Real, M,
		defined_inst(typed_ground(Uniq, T)), det, M) :-
	unify_uniq(IsLive, Real, det, unique, Uniq0, Uniq).
make_ground_inst(bound(Uniq0, BoundInsts0), IsLive, Uniq1, Real, M0,
		bound(Uniq, BoundInsts), Det, M) :-
	unify_uniq(IsLive, Real, semidet, Uniq0, Uniq1, Uniq),
	make_ground_bound_inst_list(BoundInsts0, IsLive, Uniq1, Real, M0,
					BoundInsts, Det1, M),
	det_par_conjunction_detism(Det1, semidet, Det).
make_ground_inst(ground(Uniq0, _GII0), IsLive, Uniq1, Real, M,
		ground(Uniq, none), semidet, M) :-
	unify_uniq(IsLive, Real, semidet, Uniq0, Uniq1, Uniq).
make_ground_inst(inst_var(_), _, _, _, _, _, _, _) :-
	error("free inst var").
make_ground_inst(abstract_inst(_,_), _, _, _, M, ground(shared, none),
		semidet, M).
make_ground_inst(defined_inst(InstName), IsLive, Uniq, Real, ModuleInfo0,
			Inst, Det, ModuleInfo) :-
		% check whether the inst name is already in the
		% ground_inst table
	module_info_insts(ModuleInfo0, InstTable0),
	inst_table_get_ground_insts(InstTable0, GroundInsts0),
	GroundInstKey = ground_inst(InstName, IsLive, Uniq, Real),
	(
		map__search(GroundInsts0, GroundInstKey, Result)
	->
		( Result = known(GroundInst0, Det0) ->
			GroundInst = GroundInst0,
			Det = Det0
		;
			GroundInst = defined_inst(GroundInstKey),
			Det = det
				% We can safely assume this is det, since
				% if it were semidet, we would have noticed
				% this in the process of unfolding the
				% definition.
		),
		ModuleInfo = ModuleInfo0
	;
		% insert the inst name in the ground_inst table, with
		% value `unknown' for the moment
		map__det_insert(GroundInsts0, GroundInstKey, unknown,
			GroundInsts1),
		inst_table_set_ground_insts(InstTable0, GroundInsts1,
			InstTable1),
		module_info_set_insts(ModuleInfo0, InstTable1, ModuleInfo1),

		% expand the inst name, and invoke ourself recursively on
		% it's expansion
		inst_lookup(ModuleInfo1, InstName, Inst0),
		inst_expand(ModuleInfo1, Inst0, Inst1),
		make_ground_inst(Inst1, IsLive, Uniq, Real, ModuleInfo1,
				GroundInst, Det, ModuleInfo2),

		% now that we have determined the resulting Inst, store
		% the appropriate value `known(GroundInst, Det)' in the
		% ground_inst table
		module_info_insts(ModuleInfo2, InstTable2),
		inst_table_get_ground_insts(InstTable2, GroundInsts2),
		map__det_update(GroundInsts2, GroundInstKey,
			known(GroundInst, Det), GroundInsts),
		inst_table_set_ground_insts(InstTable2, GroundInsts,
			InstTable),
		module_info_set_insts(ModuleInfo2, InstTable, ModuleInfo)
	),
		% avoid expanding recursive insts
	( inst_contains_instname(GroundInst, ModuleInfo, GroundInstKey) ->
		Inst = defined_inst(GroundInstKey)
	;
		Inst = GroundInst
	).

:- pred make_ground_bound_inst_list(list(bound_inst), is_live, uniqueness,
	unify_is_real, module_info, list(bound_inst), determinism, module_info).
:- mode make_ground_bound_inst_list(in, in, in, in, in,
	out, out, out) is semidet.

make_ground_bound_inst_list([], _, _, _, ModuleInfo, [], det, ModuleInfo).
make_ground_bound_inst_list([Bound0 | Bounds0], IsLive, Uniq, Real, ModuleInfo0,
			[Bound | Bounds], Det, ModuleInfo) :-
	Bound0 = functor(ConsId, ArgInsts0),
	make_ground_inst_list(ArgInsts0, IsLive, Uniq, Real, ModuleInfo0,
				ArgInsts, Det1, ModuleInfo1),
	Bound = functor(ConsId, ArgInsts),
	make_ground_bound_inst_list(Bounds0, IsLive, Uniq, Real, ModuleInfo1,
				Bounds, Det2, ModuleInfo),
	det_par_conjunction_detism(Det1, Det2, Det).

%-----------------------------------------------------------------------------%

% abstractly unify an inst with `any' and calculate the new inst
% and the determinism of the unification.

:- pred make_any_inst(inst, is_live, uniqueness, unify_is_real, module_info,
				inst, determinism, module_info).
:- mode make_any_inst(in, in, in, in, in, out, out, out) is semidet.

make_any_inst(not_reached, _, _, _, M, not_reached, erroneous, M).
make_any_inst(any(Uniq0), IsLive, Uniq1, Real, M, any(Uniq),
		semidet, M) :-
	allow_unify_bound_any(Real),
	unify_uniq(IsLive, Real, semidet, Uniq0, Uniq1, Uniq).
make_any_inst(free, IsLive, Uniq0, Real, M, any(Uniq), det, M) :-
	unify_uniq(IsLive, Real, det, unique, Uniq0, Uniq).
make_any_inst(free(T), IsLive, Uniq, Real, M,
		defined_inst(Any), det, M) :-
	% The following is a round-about way of doing this
	%	unify_uniq(IsLive, Real, det, unique, Uniq0, Uniq),
	%	Any = typed_any(Uniq, T).
	% without the need for a `typed_any' inst.
	Any = typed_inst(T, unify_inst(IsLive, free, any(Uniq), Real)).
make_any_inst(bound(Uniq0, BoundInsts0), IsLive, Uniq1, Real, M0,
		bound(Uniq, BoundInsts), Det, M) :-
	allow_unify_bound_any(Real),
	unify_uniq(IsLive, Real, semidet, Uniq0, Uniq1, Uniq),
	make_any_bound_inst_list(BoundInsts0, IsLive, Uniq1, Real, M0,
					BoundInsts, Det1, M),
	det_par_conjunction_detism(Det1, semidet, Det).
make_any_inst(ground(Uniq0, PredInst), IsLive, Uniq1, Real, M,
		ground(Uniq, PredInst), semidet, M) :-
	allow_unify_bound_any(Real),
	unify_uniq(IsLive, Real, semidet, Uniq0, Uniq1, Uniq).
make_any_inst(inst_var(_), _, _, _, _, _, _, _) :-
	error("free inst var").
make_any_inst(abstract_inst(_,_), _, _, _, M, any(shared),
		semidet, M).
make_any_inst(defined_inst(InstName), IsLive, Uniq, Real, ModuleInfo0,
			Inst, Det, ModuleInfo) :-
		% check whether the inst name is already in the
		% any_inst table
	module_info_insts(ModuleInfo0, InstTable0),
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
		ModuleInfo = ModuleInfo0
	;
		% insert the inst name in the any_inst table, with
		% value `unknown' for the moment
		map__det_insert(AnyInsts0, AnyInstKey, unknown,
			AnyInsts1),
		inst_table_set_any_insts(InstTable0, AnyInsts1,
			InstTable1),
		module_info_set_insts(ModuleInfo0, InstTable1, ModuleInfo1),

		% expand the inst name, and invoke ourself recursively on
		% it's expansion
		inst_lookup(ModuleInfo1, InstName, Inst0),
		inst_expand(ModuleInfo1, Inst0, Inst1),
		make_any_inst(Inst1, IsLive, Uniq, Real, ModuleInfo1,
				AnyInst, Det, ModuleInfo2),

		% now that we have determined the resulting Inst, store
		% the appropriate value `known(AnyInst, Det)' in the
		% any_inst table
		module_info_insts(ModuleInfo2, InstTable2),
		inst_table_get_any_insts(InstTable2, AnyInsts2),
		map__det_update(AnyInsts2, AnyInstKey,
			known(AnyInst, Det), AnyInsts),
		inst_table_set_any_insts(InstTable2, AnyInsts,
			InstTable),
		module_info_set_insts(ModuleInfo2, InstTable, ModuleInfo)
	),
		% avoid expanding recursive insts
	( inst_contains_instname(AnyInst, ModuleInfo, AnyInstKey) ->
		Inst = defined_inst(AnyInstKey)
	;
		Inst = AnyInst
	).

:- pred make_any_bound_inst_list(list(bound_inst), is_live, uniqueness,
	unify_is_real, module_info, list(bound_inst), determinism, module_info).
:- mode make_any_bound_inst_list(in, in, in, in, in,
	out, out, out) is semidet.

make_any_bound_inst_list([], _, _, _, ModuleInfo, [], det, ModuleInfo).
make_any_bound_inst_list([Bound0 | Bounds0], IsLive, Uniq, Real, ModuleInfo0,
			[Bound | Bounds], Det, ModuleInfo) :-
	Bound0 = functor(ConsId, ArgInsts0),
	make_any_inst_list(ArgInsts0, IsLive, Uniq, Real, ModuleInfo0,
				ArgInsts, Det1, ModuleInfo1),
	Bound = functor(ConsId, ArgInsts),
	make_any_bound_inst_list(Bounds0, IsLive, Uniq, Real, ModuleInfo1,
				Bounds, Det2, ModuleInfo),
	det_par_conjunction_detism(Det1, Det2, Det).

:- pred make_any_inst_list(list(inst), is_live, uniqueness, unify_is_real,
			module_info, list(inst), determinism, module_info).
:- mode make_any_inst_list(in, in, in, in, in, out, out, out) is semidet.

make_any_inst_list([], _, _, _, ModuleInfo, [], det, ModuleInfo).
make_any_inst_list([Inst0 | Insts0], Live, Uniq, Real, ModuleInfo0,
		[Inst | Insts], Det, ModuleInfo) :-
	make_any_inst(Inst0, Live, Uniq, Real, ModuleInfo0,
		Inst, Det1, ModuleInfo1),
	make_any_inst_list(Insts0, Live, Uniq, Real, ModuleInfo1,
		Insts, Det2, ModuleInfo),
	det_par_conjunction_detism(Det1, Det2, Det).

%-----------------------------------------------------------------------------%

:- pred maybe_make_shared_inst_list(list(inst), list(is_live), module_info,
				list(inst), module_info).
:- mode maybe_make_shared_inst_list(in, in, in, out, out) is det.

maybe_make_shared_inst_list([], [], ModuleInfo, [], ModuleInfo).
maybe_make_shared_inst_list([Inst0 | Insts0], [IsLive | IsLives], ModuleInfo0,
		[Inst | Insts], ModuleInfo) :-
	( IsLive = live ->
		make_shared_inst(Inst0, ModuleInfo0, Inst, ModuleInfo1)
	;
		Inst = Inst0,
		ModuleInfo1 = ModuleInfo0
	),
	maybe_make_shared_inst_list(Insts0, IsLives, ModuleInfo1,
		Insts, ModuleInfo).
maybe_make_shared_inst_list([], [_|_], _, _, _) :-
	error("maybe_make_shared_inst_list: length mismatch").
maybe_make_shared_inst_list([_|_], [], _, _, _) :-
	error("maybe_make_shared_inst_list: length mismatch").


make_shared_inst_list([], ModuleInfo, [], ModuleInfo).
make_shared_inst_list([Inst0 | Insts0], ModuleInfo0,
		[Inst | Insts], ModuleInfo) :-
	make_shared_inst(Inst0, ModuleInfo0, Inst, ModuleInfo1),
	make_shared_inst_list(Insts0, ModuleInfo1, Insts, ModuleInfo).

% make an inst shared; replace all occurrences of `unique' or `mostly_unique'
% in the inst with `shared'.

:- pred make_shared_inst(inst, module_info, inst, module_info).
:- mode make_shared_inst(in, in, out, out) is det.

make_shared_inst(not_reached, M, not_reached, M).
make_shared_inst(any(Uniq0), M, any(Uniq), M) :-
	make_shared(Uniq0, Uniq).
make_shared_inst(free, M, free, M) :-
	% the caller should ensure that this never happens
	error("make_shared_inst: cannot make shared version of `free'").
make_shared_inst(free(T), M, free(T), M) :-
	% the caller should ensure that this never happens
	error("make_shared_inst: cannot make shared version of `free(T)'").
make_shared_inst(bound(Uniq0, BoundInsts0), M0, bound(Uniq, BoundInsts), M) :-
	make_shared(Uniq0, Uniq),
	make_shared_bound_inst_list(BoundInsts0, M0, BoundInsts, M).
make_shared_inst(ground(Uniq0, PredInst), M, ground(Uniq, PredInst), M) :-
	make_shared(Uniq0, Uniq).
make_shared_inst(inst_var(_), _, _, _) :-
	error("free inst var").
make_shared_inst(abstract_inst(_,_), M, _, M) :-
	error("make_shared_inst(abstract_inst)").
make_shared_inst(defined_inst(InstName), ModuleInfo0, Inst, ModuleInfo) :-
		% check whether the inst name is already in the
		% shared_inst table
	module_info_insts(ModuleInfo0, InstTable0),
	inst_table_get_shared_insts(InstTable0, SharedInsts0),
	(
		map__search(SharedInsts0, InstName, Result)
	->
		( Result = known(SharedInst0) ->
			SharedInst = SharedInst0
		;
			SharedInst = defined_inst(InstName)
		),
		ModuleInfo = ModuleInfo0
	;
		% insert the inst name in the shared_inst table, with
		% value `unknown' for the moment
		map__det_insert(SharedInsts0, InstName, unknown, SharedInsts1),
		inst_table_set_shared_insts(InstTable0, SharedInsts1,
			InstTable1),
		module_info_set_insts(ModuleInfo0, InstTable1, ModuleInfo1),

		% expand the inst name, and invoke ourself recursively on
		% it's expansion
		inst_lookup(ModuleInfo1, InstName, Inst0),
		inst_expand(ModuleInfo1, Inst0, Inst1),
		make_shared_inst(Inst1, ModuleInfo1, SharedInst, ModuleInfo2),

		% now that we have determined the resulting Inst, store
		% the appropriate value `known(SharedInst)' in the shared_inst
		% table
		module_info_insts(ModuleInfo2, InstTable2),
		inst_table_get_shared_insts(InstTable2, SharedInsts2),
		map__det_update(SharedInsts2, InstName, known(SharedInst),
			SharedInsts),
		inst_table_set_shared_insts(InstTable2, SharedInsts,
			InstTable),
		module_info_set_insts(ModuleInfo2, InstTable, ModuleInfo)
	),
		% avoid expanding recursive insts
	( inst_contains_instname(SharedInst, ModuleInfo, InstName) ->
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

:- pred make_shared_bound_inst_list(list(bound_inst), module_info,
					list(bound_inst), module_info).
:- mode make_shared_bound_inst_list(in, in, out, out) is det.

make_shared_bound_inst_list([], ModuleInfo, [], ModuleInfo).
make_shared_bound_inst_list([Bound0 | Bounds0], ModuleInfo0,
				[Bound | Bounds], ModuleInfo) :-
	Bound0 = functor(ConsId, ArgInsts0),
	make_shared_inst_list(ArgInsts0, ModuleInfo0,
				ArgInsts, ModuleInfo1),
	Bound = functor(ConsId, ArgInsts),
	make_shared_bound_inst_list(Bounds0, ModuleInfo1,
				Bounds, ModuleInfo).

%-----------------------------------------------------------------------------%

% make an inst mostly-uniq: replace all occurrences of `unique'
% in the inst with `mostly_unique'.  (Used by unique_modes.m to
% change the insts of semidet-live or nondet-live insts.)

make_mostly_uniq_inst(not_reached, M, not_reached, M).
make_mostly_uniq_inst(any(Uniq0), M, any(Uniq), M) :-
	make_mostly_uniq(Uniq0, Uniq).
make_mostly_uniq_inst(free, M, free, M).
make_mostly_uniq_inst(free(T), M, free(T), M).
make_mostly_uniq_inst(bound(Uniq0, BoundInsts0), M0, bound(Uniq, BoundInsts),
		M) :-
		% XXX could improve efficiency by avoiding recursion here
	make_mostly_uniq(Uniq0, Uniq),
	make_mostly_uniq_bound_inst_list(BoundInsts0, M0, BoundInsts, M).
make_mostly_uniq_inst(ground(Uniq0, PredInst), M, ground(Uniq, PredInst), M) :-
	make_mostly_uniq(Uniq0, Uniq).
make_mostly_uniq_inst(inst_var(_), _, _, _) :-
	error("free inst var").
make_mostly_uniq_inst(abstract_inst(_,_), M, _, M) :-
	error("make_mostly_uniq_inst(abstract_inst)").
make_mostly_uniq_inst(defined_inst(InstName), ModuleInfo0, Inst, ModuleInfo) :-
		% check whether the inst name is already in the
		% mostly_uniq_inst table
	module_info_insts(ModuleInfo0, InstTable0),
	inst_table_get_mostly_uniq_insts(InstTable0, NondetLiveInsts0),
	(
		map__search(NondetLiveInsts0, InstName, Result)
	->
		( Result = known(NondetLiveInst0) ->
			NondetLiveInst = NondetLiveInst0
		;
			NondetLiveInst = defined_inst(InstName)
		),
		ModuleInfo = ModuleInfo0
	;
		% insert the inst name in the mostly_uniq_inst table, with
		% value `unknown' for the moment
		map__det_insert(NondetLiveInsts0, InstName, unknown,
			NondetLiveInsts1),
		inst_table_set_mostly_uniq_insts(InstTable0, NondetLiveInsts1,
			InstTable1),
		module_info_set_insts(ModuleInfo0, InstTable1, ModuleInfo1),

		% expand the inst name, and invoke ourself recursively on
		% it's expansion
		inst_lookup(ModuleInfo1, InstName, Inst0),
		inst_expand(ModuleInfo1, Inst0, Inst1),
		make_mostly_uniq_inst(Inst1, ModuleInfo1, NondetLiveInst,
			ModuleInfo2),

		% now that we have determined the resulting Inst, store
		% the appropriate value `known(NondetLiveInst)' in the
		% mostly_uniq_inst table
		module_info_insts(ModuleInfo2, InstTable2),
		inst_table_get_mostly_uniq_insts(InstTable2, NondetLiveInsts2),
		map__det_update(NondetLiveInsts2, InstName,
			known(NondetLiveInst), NondetLiveInsts),
		inst_table_set_mostly_uniq_insts(InstTable2, NondetLiveInsts,
			InstTable),
		module_info_set_insts(ModuleInfo2, InstTable, ModuleInfo)
	),
		% avoid expanding recursive insts
	( inst_contains_instname(NondetLiveInst, ModuleInfo, InstName) ->
		Inst = defined_inst(InstName)
	;
		Inst = NondetLiveInst
	).

:- pred make_mostly_uniq(uniqueness, uniqueness).
:- mode make_mostly_uniq(in, out) is det.

make_mostly_uniq(unique, mostly_unique).
make_mostly_uniq(mostly_unique, mostly_unique).
make_mostly_uniq(shared, shared).
make_mostly_uniq(mostly_clobbered, mostly_clobbered).
make_mostly_uniq(clobbered, clobbered).

:- pred make_mostly_uniq_bound_inst_list(list(bound_inst), module_info,
					list(bound_inst), module_info).
:- mode make_mostly_uniq_bound_inst_list(in, in, out, out) is det.

make_mostly_uniq_bound_inst_list([], ModuleInfo, [], ModuleInfo).
make_mostly_uniq_bound_inst_list([Bound0 | Bounds0], ModuleInfo0,
				[Bound | Bounds], ModuleInfo) :-
	Bound0 = functor(ConsId, ArgInsts0),
	make_mostly_uniq_inst_list(ArgInsts0, ModuleInfo0,
				ArgInsts, ModuleInfo1),
	Bound = functor(ConsId, ArgInsts),
	make_mostly_uniq_bound_inst_list(Bounds0, ModuleInfo1,
				Bounds, ModuleInfo).

:- pred make_mostly_uniq_inst_list(list(inst), module_info,
				list(inst), module_info).
:- mode make_mostly_uniq_inst_list(in, in, out, out) is det.

make_mostly_uniq_inst_list([], ModuleInfo, [], ModuleInfo).
make_mostly_uniq_inst_list([Inst0 | Insts0], ModuleInfo0,
		[Inst | Insts], ModuleInfo) :-
	make_mostly_uniq_inst(Inst0, ModuleInfo0, Inst, ModuleInfo1),
	make_mostly_uniq_inst_list(Insts0, ModuleInfo1, Insts, ModuleInfo).

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

inst_merge(InstA, InstB, MaybeType, ModuleInfo0, Inst, ModuleInfo) :-
		% check whether this pair of insts is already in
		% the merge_insts table
	module_info_insts(ModuleInfo0, InstTable0),
	inst_table_get_merge_insts(InstTable0, MergeInstTable0),
	ThisInstPair = InstA - InstB,
	( map__search(MergeInstTable0, ThisInstPair, Result) ->
		ModuleInfo = ModuleInfo0,
		( Result = known(MergedInst) ->
			Inst0 = MergedInst
		;
			Inst0 = defined_inst(merge_inst(InstA, InstB))
		)
	;
			% insert ThisInstPair into the table with value
			% `unknown'
		map__det_insert(MergeInstTable0, ThisInstPair, unknown,
			MergeInstTable1),
		inst_table_set_merge_insts(InstTable0, MergeInstTable1,
			InstTable1),
		module_info_set_insts(ModuleInfo0, InstTable1, ModuleInfo1),

			% merge the insts
		inst_merge_2(InstA, InstB, MaybeType, ModuleInfo1, Inst0,
			ModuleInfo2),
			

			% now update the value associated with ThisInstPair
		module_info_insts(ModuleInfo2, InstTable2),
		inst_table_get_merge_insts(InstTable2, MergeInstTable2),
		map__det_update(MergeInstTable2, ThisInstPair, known(Inst0),
			MergeInstTable3),
		inst_table_set_merge_insts(InstTable2, MergeInstTable3,
			InstTable3),
		module_info_set_insts(ModuleInfo2, InstTable3, ModuleInfo)
	),
		% avoid expanding recursive insts
	( inst_contains_instname(Inst0, ModuleInfo, merge_inst(InstA, InstB)) ->
		Inst = defined_inst(merge_inst(InstA, InstB))
	;
		Inst = Inst0
	).

:- pred inst_merge_2(inst, inst, maybe(type), module_info, inst, module_info).
:- mode inst_merge_2(in, in, in, in, out, out) is semidet.

inst_merge_2(InstA, InstB, MaybeType, ModuleInfo0, Inst, ModuleInfo) :-
/*********
		% would this test improve efficiency??
	( InstA = InstB ->
		Inst = InstA,
		ModuleInfo = ModuleInfo0
	;
*********/
	inst_expand(ModuleInfo0, InstA, InstA2),
	inst_expand(ModuleInfo0, InstB, InstB2),
	( InstB2 = not_reached ->
		Inst = InstA2,
		ModuleInfo = ModuleInfo0
	;
		inst_merge_3(InstA2, InstB2, MaybeType, ModuleInfo0, Inst,
			ModuleInfo)
	).

:- pred inst_merge_3(inst, inst, maybe(type), module_info, inst, module_info).
:- mode inst_merge_3(in, in, in, in, out, out) is semidet.

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

inst_merge_3(any(UniqA), any(UniqB), _, M, any(Uniq), M) :-
	merge_uniq(UniqA, UniqB, Uniq).
inst_merge_3(any(Uniq), free, _, M, any(Uniq), M) :-
	% we do not yet allow merge of any with free, except for clobbered anys
	( Uniq = clobbered ; Uniq = mostly_clobbered ).
inst_merge_3(any(UniqA), bound(UniqB, ListB), _, ModInfo, any(Uniq), ModInfo) :-
	merge_uniq_bound(UniqA, UniqB, ListB, ModInfo, Uniq),
	% we do not yet allow merge of any with free, except for clobbered anys
	( ( Uniq = clobbered ; Uniq = mostly_clobbered ) ->
		true
	;
		bound_inst_list_is_ground_or_any(ListB, ModInfo)
	).
inst_merge_3(any(UniqA), ground(UniqB, _), _, M, any(Uniq), M) :-
	merge_uniq(UniqA, UniqB, Uniq).
inst_merge_3(any(UniqA), abstract_inst(_, _), _, M, any(Uniq), M) :-
	merge_uniq(UniqA, shared, Uniq),
	% we do not yet allow merge of any with free, except for clobbered anys
	( Uniq = clobbered ; Uniq = mostly_clobbered ).
inst_merge_3(free, any(Uniq), _, M, any(Uniq), M) :-
	% we do not yet allow merge of any with free, except for clobbered anys
	( Uniq = clobbered ; Uniq = mostly_clobbered ).
inst_merge_3(bound(UniqA, ListA), any(UniqB), _, ModInfo, any(Uniq), ModInfo) :-
	merge_uniq_bound(UniqB, UniqA, ListA, ModInfo, Uniq),
	% we do not yet allow merge of any with free, except for clobbered anys
	( ( Uniq = clobbered ; Uniq = mostly_clobbered ) ->
		true
	;
		bound_inst_list_is_ground_or_any(ListA, ModInfo)
	).
inst_merge_3(ground(UniqA, _), any(UniqB), _, M, any(Uniq), M) :-
	merge_uniq(UniqA, UniqB, Uniq).
inst_merge_3(abstract_inst(_, _), any(UniqB), _, M, any(Uniq), M) :-
	merge_uniq(shared, UniqB, Uniq),
	% we do not yet allow merge of any with free, except for clobbered anys
	( Uniq = clobbered ; Uniq = mostly_clobbered ).
inst_merge_3(free, free, _, M, free, M).
inst_merge_3(bound(UniqA, ListA), bound(UniqB, ListB), MaybeType, ModuleInfo0,
		bound(Uniq, List), ModuleInfo) :-
	merge_uniq(UniqA, UniqB, Uniq),
	bound_inst_list_merge(ListA, ListB, MaybeType, ModuleInfo0, List,
		ModuleInfo).
inst_merge_3(bound(UniqA, ListA), ground(UniqB, _), MaybeType, ModuleInfo0,
		Result, ModuleInfo) :-
	inst_merge_bound_ground(UniqA, ListA, UniqB, MaybeType,
		ModuleInfo0, Result, ModuleInfo).
inst_merge_3(ground(UniqA, _), bound(UniqB, ListB), MaybeType, ModuleInfo0,
		Result, ModuleInfo) :-
	inst_merge_bound_ground(UniqB, ListB, UniqA, MaybeType,
		ModuleInfo0, Result, ModuleInfo).
inst_merge_3(ground(UniqA, GroundInstInfoA), ground(UniqB, GroundInstInfoB),
		_, ModuleInfo, ground(Uniq, GroundInstInfo), ModuleInfo) :-
	(
		GroundInstInfoA = higher_order(PredA),
		GroundInstInfoB = higher_order(PredB)
	->
		% if they specify matching pred insts, but one is more
		% precise (specifies more info) than the other,
		% then we want to choose the least precise one
		( pred_inst_matches(PredA, PredB, ModuleInfo) ->
			GroundInstInfo = higher_order(PredB)
		; pred_inst_matches(PredB, PredA, ModuleInfo) ->
			GroundInstInfo = higher_order(PredA)
		;
			GroundInstInfo = none
		)
	;       
		GroundInstInfoA = constrained_inst_var(V),
		GroundInstInfoB = constrained_inst_var(V)
	->
		GroundInstInfo = constrained_inst_var(V)
	;
		GroundInstInfo = none
	),
	merge_uniq(UniqA, UniqB, Uniq).
inst_merge_3(abstract_inst(Name, ArgsA), abstract_inst(Name, ArgsB),
		_, ModuleInfo0, abstract_inst(Name, Args), ModuleInfo) :-
	% We don't know the arguments types of an abstract inst.
	MaybeTypes = list__duplicate(list__length(ArgsA), no),
	inst_list_merge(ArgsA, ArgsB, MaybeTypes, ModuleInfo0, Args,
		ModuleInfo).
inst_merge_3(not_reached, Inst, _, M, Inst, M).

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

	% merge_uniq_bound(UniqA, UniqB, ListB, ModuleInfo, Uniq) succeeds iff
	% Uniq is the result of merging

:- pred merge_uniq_bound(uniqueness, uniqueness, list(bound_inst), module_info,
			uniqueness).
:- mode merge_uniq_bound(in, in, in, in, out) is det.

merge_uniq_bound(UniqA, UniqB, ListB, ModuleInfo, Uniq) :-
	merge_uniq(UniqA, UniqB, Uniq0),
	set__init(Expansions0),
	merge_bound_inst_list_uniq(ListB, Uniq0, ModuleInfo,
		Expansions0, _Expansions, Uniq).

:- pred merge_bound_inst_list_uniq(list(bound_inst), uniqueness, module_info,
			set(inst_name), set(inst_name), uniqueness).
:- mode merge_bound_inst_list_uniq(in, in, in, in, out, out) is det.

merge_bound_inst_list_uniq([], Uniq, _, Expansions, Expansions, Uniq).
merge_bound_inst_list_uniq([BoundInst | BoundInsts], Uniq0,
			ModuleInfo, Expansions0, Expansions, Uniq) :-
	BoundInst = functor(_ConsId, ArgInsts),
	merge_inst_list_uniq(ArgInsts, Uniq0, ModuleInfo,
		Expansions0, Expansions1, Uniq1),
	merge_bound_inst_list_uniq(BoundInsts, Uniq1, ModuleInfo,
		Expansions1, Expansions, Uniq).

:- pred merge_inst_list_uniq(list(inst), uniqueness, module_info,
			set(inst_name), set(inst_name), uniqueness).
:- mode merge_inst_list_uniq(in, in, in, in, out, out) is det.

merge_inst_list_uniq([], Uniq, _, Expansions, Expansions, Uniq).
merge_inst_list_uniq([Inst | Insts], Uniq0, ModuleInfo,
		Expansions0, Expansions, Uniq) :-
	merge_inst_uniq(Inst, Uniq0, ModuleInfo, Expansions0, Expansions1,
		Uniq1),
	merge_inst_list_uniq(Insts, Uniq1, ModuleInfo, Expansions1, Expansions,
		Uniq).

:- pred merge_inst_uniq(inst, uniqueness, module_info,
		set(inst_name), set(inst_name), uniqueness).
:- mode merge_inst_uniq(in, in, in, in, out, out) is det.

merge_inst_uniq(any(UniqA), UniqB, _, Expansions, Expansions, Uniq) :-
	merge_uniq(UniqA, UniqB, Uniq).
merge_inst_uniq(free, Uniq, _, Expansions, Expansions, Uniq).
merge_inst_uniq(free(_), Uniq, _, Expansions, Expansions, Uniq).
merge_inst_uniq(bound(UniqA, ListA), UniqB, ModuleInfo,
		Expansions0, Expansions, Uniq) :-
	merge_uniq(UniqA, UniqB, Uniq0),
	merge_bound_inst_list_uniq(ListA, Uniq0, ModuleInfo,
		Expansions0, Expansions, Uniq).
merge_inst_uniq(ground(UniqA, _), UniqB, _, Expansions, Expansions, Uniq) :-
	merge_uniq(UniqA, UniqB, Uniq).
merge_inst_uniq(abstract_inst(_,_), UniqB, _, Expansions, Expansions, Uniq) :-
	merge_uniq(shared, UniqB, Uniq).
merge_inst_uniq(defined_inst(InstName), UniqB, ModuleInfo,
		Expansions0, Expansions, Uniq) :-
	( set__member(InstName, Expansions0) ->
		Uniq = UniqB,
		Expansions = Expansions0
	;
		set__insert(Expansions0, InstName, Expansions1),
		inst_lookup(ModuleInfo, InstName, Inst),
		merge_inst_uniq(Inst, UniqB, ModuleInfo,
			Expansions1, Expansions, Uniq)
	).
merge_inst_uniq(not_reached, Uniq, _, Expansions, Expansions, Uniq).
merge_inst_uniq(inst_var(_), _, _, Expansions, Expansions, _) :-
	error("merge_inst_uniq: unexpected inst_var").

%-----------------------------------------------------------------------------%

:- pred inst_merge_bound_ground(uniqueness, list(bound_inst),
		uniqueness, maybe(type), module_info, inst, module_info).
:- mode inst_merge_bound_ground(in, in, in, in, in, out, out) is semidet.

inst_merge_bound_ground(UniqA, ListA, UniqB, MaybeType, ModuleInfo0,
		Result, ModuleInfo) :-
	( bound_inst_list_is_ground(ListA, ModuleInfo0) ->
		merge_uniq_bound(UniqB, UniqA, ListA, ModuleInfo0, Uniq),
		Result = ground(Uniq, none),
		ModuleInfo = ModuleInfo0
	;
		bound_inst_list_is_ground_or_any(ListA, ModuleInfo0),
		% If we know the type, we can give a more accurate result than
		% just "any".
		(
			MaybeType = yes(Type),
			type_constructors(Type, ModuleInfo0, Constructors),
			constructors_to_bound_insts(Constructors, UniqB,
				ModuleInfo0, ListB0),
			list__sort_and_remove_dups(ListB0, ListB),
			inst_merge_3(bound(UniqA, ListA),
				bound(UniqB, ListB), MaybeType,
				ModuleInfo0, Result, ModuleInfo)
		;
			MaybeType = no,
			merge_uniq_bound(UniqB, UniqA, ListA, ModuleInfo0,
				Uniq),
			Result = any(Uniq),
			ModuleInfo = ModuleInfo0
		)
	).
	

%-----------------------------------------------------------------------------%

:- pred inst_list_merge(list(inst), list(inst), list(maybe(type)), module_info,
		list(inst), module_info).
:- mode inst_list_merge(in, in, in, in, out, out) is semidet.

inst_list_merge([], [], _, ModuleInfo, [], ModuleInfo).
inst_list_merge([ArgA | ArgsA], [ArgB | ArgsB], [MaybeType | MaybeTypes],
		ModuleInfo0, [Arg | Args], ModuleInfo) :-
	inst_merge(ArgA, ArgB, MaybeType, ModuleInfo0, Arg, ModuleInfo1),
	inst_list_merge(ArgsA, ArgsB, MaybeTypes, ModuleInfo1, Args,
		ModuleInfo).

	% bound_inst_list_merge(Xs, Ys, ModuleInfo0, Zs, ModuleInfo):
	% The two input lists Xs and Ys must already be sorted.
	% Here we perform a sorted merge operation,
	% so that the functors of the output list Zs are the union
	% of the functors of the input lists Xs and Ys.

:- pred bound_inst_list_merge(list(bound_inst), list(bound_inst),
		maybe(type), module_info, list(bound_inst), module_info).
:- mode bound_inst_list_merge(in, in, in, in, out, out) is semidet.

bound_inst_list_merge(Xs, Ys, MaybeType, ModuleInfo0, Zs, ModuleInfo) :-
	( Xs = [] ->
		Zs = Ys,
		ModuleInfo = ModuleInfo0
	; Ys = [] ->
		Zs = Xs,
		ModuleInfo = ModuleInfo0
	;
		Xs = [X | Xs1],
		Ys = [Y | Ys1],
		X = functor(ConsIdX, ArgsX),
		Y = functor(ConsIdY, ArgsY),
		( ConsIdX = ConsIdY ->
			maybe_get_cons_id_arg_types(ModuleInfo0, MaybeType,
				ConsIdX, list__length(ArgsX), MaybeTypes),
			inst_list_merge(ArgsX, ArgsY, MaybeTypes, ModuleInfo0,
					Args, ModuleInfo1),
			Z = functor(ConsIdX, Args),
			Zs = [Z | Zs1],
			bound_inst_list_merge(Xs1, Ys1, MaybeType, ModuleInfo1,
				Zs1, ModuleInfo)
		; compare(<, ConsIdX, ConsIdY) ->
			Zs = [X | Zs1],
			bound_inst_list_merge(Xs1, Ys, MaybeType, ModuleInfo0,
						Zs1, ModuleInfo)
		;
			Zs = [Y | Zs1],
			bound_inst_list_merge(Xs, Ys1, MaybeType, ModuleInfo0,
						Zs1, ModuleInfo)
		)
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
