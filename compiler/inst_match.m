%-----------------------------------------------------------------------------%
% Copyright (C) 1995-1998, 2000 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% file: inst_match.m
% author: fjh
%
% This module defines some utility routines for comparing insts
% that are used by modes.m and det_analysis.m.

/*
The handling of `any' insts is not complete.  (See also inst_util.m)
It would be nice to allow `free' to match `any', but right now we
only allow a few special cases of that.
The reason is that although the mode analysis would be pretty
straight-forward, generating the correct code is quite a bit trickier.
modes.m would have to be changed to handle the implicit
conversions from `free'/`bound'/`ground' to `any' at

	(1) procedure calls (this is just an extension of implied modes)
		currently we support only the easy cases of this
	(2) the end of branched goals
	(3) the end of predicates.

Since that is not yet done, we currently require the user to
insert explicit calls to initialize constraint variables.

We do allow `bound' and `ground' to match `any', based on the
assumption that `bound' and `ground' are represented in the same
way as `any', i.e. that we use the type system rather than the
mode system to distinguish between different representations.
*/

%-----------------------------------------------------------------------------%

:- module inst_match.

:- interface.

:- import_module hlds_module, prog_data, (inst).
:- import_module list.

%-----------------------------------------------------------------------------%

:- pred inst_expand(module_info, inst, inst).
:- mode inst_expand(in, in, out) is det.

	% inst_expand(ModuleInfo, Inst0, Inst) checks if the top-level 
	% part of the inst is a defined inst, and if so replaces it
	% with the definition.

%-----------------------------------------------------------------------------%

:- pred inst_matches_initial(inst, inst, module_info).
:- mode inst_matches_initial(in, in, in) is semidet.

:- pred inst_matches_final(inst, inst, module_info).
:- mode inst_matches_final(in, in, in) is semidet.

	% inst_matches_initial(InstA, InstB, ModuleInfo):
	%	Succeed iff `InstA' specifies at least as much
	%	information as `InstB', and in those parts where they
	%	specify the same information, `InstA' is at least as
	%	instantiated as `InstB'.
	%	Thus, inst_matches_initial(not_reached, ground, _)
	%	succeeds, since not_reached contains more information
	%	than ground - but not vice versa.  Similarly,
	%	inst_matches_initial(bound(a), bound(a;b), _) should
	%	succeed, but not vice versa.

	% inst_matches_final(InstA, InstB, ModuleInfo):
	%	Succeed iff InstA is compatible with InstB,
	%	i.e. iff InstA will satisfy the final inst
	%	requirement InstB.  This is true if the
	%	information specified by InstA is at least as
	%	great as that specified by InstB, and where the information
	%	is the same and both insts specify a binding, the binding
	%	must be identical.
	%
	%	The difference between inst_matches_initial and
	%	inst_matches_final is that inst_matches_initial requires
	%	only something which is at least as instantiated,
	%	whereas this predicate wants something which is an
	%	exact match (or not reachable).
	%
	%	Note that this predicate is not symmetric,
	%	because of the existence of `not_reached' insts:
	%	not_reached matches_final with anything,
	%	but not everything matches_final with not_reached -
	%	in fact only not_reached matches_final with not_reached.
	%	It is also asymmetric with respect to unique insts.

	% It might be a good idea to fold inst_matches_initial and
	% inst_matches_final into a single predicate inst_matches(When, ...)
	% where When is either `initial' or `final'.

:- pred unique_matches_initial(uniqueness, uniqueness).
:- mode unique_matches_initial(in, in) is semidet.

	% unique_matches_initial(A, B) succeeds if A >= B in the ordering
	% clobbered < mostly_clobbered < shared < mostly_unique < unique

:- pred unique_matches_final(uniqueness, uniqueness).
:- mode unique_matches_final(in, in) is semidet.

	% unique_matches_final(A, B) succeeds if A >= B in the ordering
	% clobbered < mostly_clobbered < shared < mostly_unique < unique

:- pred inst_matches_binding(inst, inst, module_info).
:- mode inst_matches_binding(in, in, in) is semidet.

	% inst_matches_binding(InstA, InstB, ModuleInfo):
	%	 Succeed iff the binding of InstA is definitely exactly the
	%	 same as that of InstB.  This is the same as
	%	 inst_matches_final except that it ignores uniqueness, and
	%	 that `any' does not match itself.  It is used to check
	%	 whether variables get bound in negated contexts.

%-----------------------------------------------------------------------------%

	% pred_inst_matches(PredInstA, PredInstB, ModuleInfo)
	% 	Succeeds if PredInstA specifies a pred that can
	%	be used wherever and whenever PredInstB could be used.
	%	This is true if they both have the same PredOrFunc indicator
	%	and the same determinism, and if the arguments match
	%	using pred_inst_argmodes_match.
	%
:- pred pred_inst_matches(pred_inst_info, pred_inst_info, module_info).
:- mode pred_inst_matches(in, in, in) is semidet.

%-----------------------------------------------------------------------------%

/*
** Predicates to test various properties of insts.
** Note that `not_reached' insts are considered to satisfy
** all of these predicates except inst_is_clobbered.
*/

        % succeed if the inst is fully ground (i.e. contains only
        % `ground', `bound', and `not_reached' insts, with no `free'
        % or `any' insts).
:- pred inst_is_ground(module_info, inst).
:- mode inst_is_ground(in, in) is semidet.

        % succeed if the inst is not partly free (i.e. contains only
        % `any', `ground', `bound', and `not_reached' insts, with no
        % `free' insts).
:- pred inst_is_ground_or_any(module_info, inst).
:- mode inst_is_ground_or_any(in, in) is semidet.

        % succeed if the inst is `mostly_unique' or `unique'
:- pred inst_is_mostly_unique(module_info, inst).
:- mode inst_is_mostly_unique(in, in) is semidet.

        % succeed if the inst is `unique'
:- pred inst_is_unique(module_info, inst).
:- mode inst_is_unique(in, in) is semidet.

        % succeed if the inst is not `mostly_unique' or `unique'
:- pred inst_is_not_partly_unique(module_info, inst).
:- mode inst_is_not_partly_unique(in, in) is semidet.

        % succeed if the inst is not `unique'
:- pred inst_is_not_fully_unique(module_info, inst).
:- mode inst_is_not_fully_unique(in, in) is semidet.

:- pred inst_is_clobbered(module_info, inst).
:- mode inst_is_clobbered(in, in) is semidet.

:- pred inst_list_is_ground(list(inst), module_info).
:- mode inst_list_is_ground(in, in) is semidet.

:- pred inst_list_is_ground_or_any(list(inst), module_info).
:- mode inst_list_is_ground_or_any(in, in) is semidet.

:- pred inst_list_is_unique(list(inst), module_info).
:- mode inst_list_is_unique(in, in) is semidet.

:- pred inst_list_is_mostly_unique(list(inst), module_info).
:- mode inst_list_is_mostly_unique(in, in) is semidet.

:- pred inst_list_is_not_partly_unique(list(inst), module_info).
:- mode inst_list_is_not_partly_unique(in, in) is semidet.

:- pred inst_list_is_not_fully_unique(list(inst), module_info).
:- mode inst_list_is_not_fully_unique(in, in) is semidet.

:- pred bound_inst_list_is_ground(list(bound_inst), module_info).
:- mode bound_inst_list_is_ground(in, in) is semidet.

:- pred bound_inst_list_is_ground_or_any(list(bound_inst), module_info).
:- mode bound_inst_list_is_ground_or_any(in, in) is semidet.

:- pred bound_inst_list_is_unique(list(bound_inst), module_info).
:- mode bound_inst_list_is_unique(in, in) is semidet.

:- pred bound_inst_list_is_mostly_unique(list(bound_inst), module_info).
:- mode bound_inst_list_is_mostly_unique(in, in) is semidet.

:- pred bound_inst_list_is_not_partly_unique(list(bound_inst), module_info).
:- mode bound_inst_list_is_not_partly_unique(in, in) is semidet.

:- pred bound_inst_list_is_not_fully_unique(list(bound_inst), module_info).
:- mode bound_inst_list_is_not_fully_unique(in, in) is semidet.

:- pred inst_is_free(module_info, inst).
:- mode inst_is_free(in, in) is semidet.

:- pred inst_list_is_free(list(inst), module_info).
:- mode inst_list_is_free(in, in) is semidet.

:- pred bound_inst_list_is_free(list(bound_inst), module_info).
:- mode bound_inst_list_is_free(in, in) is semidet.

:- pred inst_is_bound(module_info, inst).
:- mode inst_is_bound(in, in) is semidet.

:- pred inst_is_bound_to_functors(module_info, inst, list(bound_inst)).
:- mode inst_is_bound_to_functors(in, in, out) is semidet.

%-----------------------------------------------------------------------------%

	% Succeed iff the specified inst contains (directly or indirectly)
	% the specified inst_name.

:- pred inst_contains_instname(inst, module_info, inst_name).
:- mode inst_contains_instname(in, in, in) is semidet.

	% Nondeterministically produce all the inst_vars contained
	% in the specified list of modes.

:- pred mode_list_contains_inst_var(list(mode), module_info, inst_var).
:- mode mode_list_contains_inst_var(in, in, out) is nondet.

	% Given a list of insts, and a corresponding list of livenesses,
	% return true iff for every element in the list of insts, either
	% the elemement is ground or the corresponding element in the liveness
	% list is dead.

:- pred inst_list_is_ground_or_dead(list(inst), list(is_live), module_info).
:- mode inst_list_is_ground_or_dead(in, in, in) is semidet.

	% Given a list of insts, and a corresponding list of livenesses,
	% return true iff for every element in the list of insts, either
	% the elemement is ground or any, or the corresponding element
	% in the liveness list is dead.

:- pred inst_list_is_ground_or_any_or_dead(list(inst), list(is_live),
					module_info).
:- mode inst_list_is_ground_or_any_or_dead(in, in, in) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module hlds_data, mode_util, prog_data, inst_util.
:- import_module list, set, map, term, std_util, require, bool.

inst_matches_initial(InstA, InstB, ModuleInfo) :-
	set__init(Expansions0),
	inst_matches_initial_2(InstA, InstB, ModuleInfo,
		Expansions0, _Expansions).

:- type expansions == set(pair(inst)).

:- pred inst_matches_initial_2(inst, inst, module_info,
		expansions, expansions).
:- mode inst_matches_initial_2(in, in, in, in, out) is semidet.

inst_matches_initial_2(InstA, InstB, ModuleInfo, Expansions0, Expansions) :-
	ThisExpansion = InstA - InstB,
	( set__member(ThisExpansion, Expansions0) ->
		Expansions = Expansions0
/********* 
		% does this test improve efficiency??
	; InstA = InstB ->
		Expansions = Expansions0
**********/
	;
		inst_expand(ModuleInfo, InstA, InstA2),
		inst_expand(ModuleInfo, InstB, InstB2),
		set__insert(Expansions0, ThisExpansion, Expansions1),
		inst_matches_initial_3(InstA2, InstB2, ModuleInfo,
			Expansions1, Expansions)
	).

:- pred inst_matches_initial_3(inst, inst, module_info, expansions, expansions).
:- mode inst_matches_initial_3(in, in, in, in, out) is semidet.

	% To avoid infinite regress, we assume that
	% inst_matches_initial is true for any pairs of insts which
	% occur in `Expansions'.

inst_matches_initial_3(any(UniqA), any(UniqB), _, Expansions, Expansions) :-
	unique_matches_initial(UniqA, UniqB).
inst_matches_initial_3(any(_), free, _, Expansions, Expansions).
inst_matches_initial_3(free, any(_), _, Expansions, Expansions).
inst_matches_initial_3(free, free, _, Expansions, Expansions).
inst_matches_initial_3(bound(UniqA, ListA), any(UniqB), ModuleInfo,
		Expansions, Expansions) :-
	unique_matches_initial(UniqA, UniqB),
	bound_inst_list_matches_uniq(ListA, UniqB, ModuleInfo).
inst_matches_initial_3(bound(_Uniq, _List), free, _, Expansions, Expansions).
inst_matches_initial_3(bound(UniqA, ListA), bound(UniqB, ListB), ModuleInfo,
		Expansions0, Expansions) :-
	unique_matches_initial(UniqA, UniqB),
	bound_inst_list_matches_initial(ListA, ListB, ModuleInfo,
		Expansions0, Expansions).
inst_matches_initial_3(bound(UniqA, ListA), ground(UniqB, no), ModuleInfo,
		Expansions, Expansions) :-
	unique_matches_initial(UniqA, UniqB),
	bound_inst_list_is_ground(ListA, ModuleInfo),
	bound_inst_list_matches_uniq(ListA, UniqB, ModuleInfo).
inst_matches_initial_3(bound(Uniq, List), abstract_inst(_,_), ModuleInfo,
		Expansions, Expansions) :-
	Uniq = unique,
	bound_inst_list_is_ground(List, ModuleInfo),
	bound_inst_list_is_unique(List, ModuleInfo).
inst_matches_initial_3(bound(Uniq, List), abstract_inst(_,_), ModuleInfo,
		Expansions, Expansions) :-
	Uniq = mostly_unique,
	bound_inst_list_is_ground(List, ModuleInfo),
	bound_inst_list_is_mostly_unique(List, ModuleInfo).
inst_matches_initial_3(ground(UniqA, _PredInst), any(UniqB), _,
		Expansions, Expansions) :-
	unique_matches_initial(UniqA, UniqB).
inst_matches_initial_3(ground(_Uniq, _PredInst), free, _,
		Expansions, Expansions).
inst_matches_initial_3(ground(UniqA, _), bound(UniqB, List), ModuleInfo,
		Expansions, Expansions) :-
	unique_matches_initial(UniqA, UniqB),
	uniq_matches_bound_inst_list(UniqA, List, ModuleInfo),
	fail.	% XXX BUG! should fail only if 
		% List does not include all the constructors for the type,
		% or if List contains some not_reached insts.
		% Should succeed if List contains all the constructors
		% for the type.  Problem is we don't know what the type was :-(
inst_matches_initial_3(ground(UniqA, PredInstA), ground(UniqB, PredInstB),
		ModuleInfo, Expansions, Expansions) :-
	maybe_pred_inst_matches_initial(PredInstA, PredInstB, ModuleInfo),
	unique_matches_initial(UniqA, UniqB).
inst_matches_initial_3(ground(_UniqA, no), abstract_inst(_,_), _, _, _) :-
		% I don't know what this should do.
		% Abstract insts aren't really supported.
	error("inst_matches_initial(ground, abstract_inst) == ??").
inst_matches_initial_3(abstract_inst(_,_), any(shared), _,
		Expansions, Expansions).
inst_matches_initial_3(abstract_inst(_,_), free, _, Expansions, Expansions).
inst_matches_initial_3(abstract_inst(Name, ArgsA), abstract_inst(Name, ArgsB),
				ModuleInfo, Expansions0, Expansions) :-
	inst_list_matches_initial(ArgsA, ArgsB, ModuleInfo,
		Expansions0, Expansions).
inst_matches_initial_3(not_reached, _, _, Expansions, Expansions).

%-----------------------------------------------------------------------------%

:- pred maybe_pred_inst_matches_initial(maybe(pred_inst_info),
		maybe(pred_inst_info), module_info).
:- mode maybe_pred_inst_matches_initial(in, in, in) is semidet.

maybe_pred_inst_matches_initial(no, no, _).
maybe_pred_inst_matches_initial(yes(_), no, _).
maybe_pred_inst_matches_initial(yes(PredInstA), yes(PredInstB), ModuleInfo) :-
	pred_inst_matches(PredInstA, PredInstB, ModuleInfo).

pred_inst_matches(PredInstA, PredInstB, ModuleInfo) :-
	set__init(Expansions0),
	pred_inst_matches_2(PredInstA, PredInstB, ModuleInfo,
		Expansions0, _).

	% pred_inst_matches_2(PredInstA, PredInstB, ModuleInfo, Expansions)
	%	Same as pred_inst_matches/3, except that inst pairs in
	%	Expansions are assumed to match_final each other.
	%	(This avoids infinite loops when calling inst_matches_final
	%	on higher-order recursive insts.)
	%
:- pred pred_inst_matches_2(pred_inst_info, pred_inst_info, module_info,
			expansions, expansions).
:- mode pred_inst_matches_2(in, in, in, in, out) is semidet.

pred_inst_matches_2(pred_inst_info(PredOrFunc, ModesA, Det),
		pred_inst_info(PredOrFunc, ModesB, Det),
		ModuleInfo, Expansions0, Expansions) :-
	pred_inst_argmodes_matches(ModesA, ModesB, ModuleInfo, Expansions0,
		Expansions).

:- pred pred_inst_matches_2(pred_inst_info, pred_inst_info,
				module_info, expansions).
:- mode pred_inst_matches_2(in, in, in, in) is semidet.

pred_inst_matches_2(PredInstInfoA, PredInstInfoB, ModuleInfo, Expansions) :-
	pred_inst_matches_2(PredInstInfoA, PredInstInfoB, ModuleInfo,
		Expansions, _).
	
:- pred pred_inst_argmodes_matches(list(mode), list(mode),
				module_info, expansions).
:- mode pred_inst_argmodes_matches(in, in, in, in) is semidet.

pred_inst_argmodes_matches(ModesA, ModesB, ModuleInfo, Expansions) :-
	pred_inst_argmodes_matches(ModesA, ModesB, ModuleInfo,
		Expansions, _).
	
	% pred_inst_matches_argmodes(ModesA, ModesB, ModuleInfo, Expansions):
	% succeeds if the initial insts of ModesB specify at least as
	% much information as, and the same binding as, the initial
	% insts of ModesA; and the final insts of ModesA specify at
	% least as much information as, and the same binding as, the
	% final insts of ModesB.  Any inst pairs in Expansions are assumed
	% to match_final each other.
	%
:- pred pred_inst_argmodes_matches(list(mode), list(mode),
				module_info, expansions, expansions).
:- mode pred_inst_argmodes_matches(in, in, in, in, out) is semidet.

pred_inst_argmodes_matches([], [], _, Expansions, Expansions).
pred_inst_argmodes_matches([ModeA|ModeAs], [ModeB|ModeBs],
		ModuleInfo, Expansions0, Expansions) :-
	mode_get_insts(ModuleInfo, ModeA, InitialA, FinalA),
	mode_get_insts(ModuleInfo, ModeB, InitialB, FinalB),
	inst_matches_final_2(InitialB, InitialA, ModuleInfo,
		Expansions0, Expansions1),
	inst_matches_final_2(FinalA, FinalB, ModuleInfo,
		Expansions1, Expansions2),
	pred_inst_argmodes_matches(ModeAs, ModeBs, ModuleInfo,
		Expansions2, Expansions).

%-----------------------------------------------------------------------------%

unique_matches_initial(unique, _).
unique_matches_initial(mostly_unique, mostly_unique).
unique_matches_initial(mostly_unique, shared).
unique_matches_initial(mostly_unique, mostly_clobbered).
unique_matches_initial(mostly_unique, clobbered).
unique_matches_initial(shared, shared).
unique_matches_initial(shared, mostly_clobbered).
unique_matches_initial(shared, clobbered).
unique_matches_initial(mostly_clobbered, mostly_clobbered).
unique_matches_initial(mostly_clobbered, clobbered).
unique_matches_initial(clobbered, clobbered).

unique_matches_final(A, B) :-
	unique_matches_initial(A, B).

%-----------------------------------------------------------------------------%

:- pred bound_inst_list_matches_uniq(list(bound_inst), uniqueness,
					module_info).
:- mode bound_inst_list_matches_uniq(in, in, in) is semidet.

bound_inst_list_matches_uniq(List, Uniq, ModuleInfo) :-
	( Uniq = unique ->
		bound_inst_list_is_unique(List, ModuleInfo)
	; Uniq = mostly_unique ->
		bound_inst_list_is_mostly_unique(List, ModuleInfo)
	;
		true
	).

:- pred uniq_matches_bound_inst_list(uniqueness, list(bound_inst),
					module_info).
:- mode uniq_matches_bound_inst_list(in, in, in) is semidet.

uniq_matches_bound_inst_list(Uniq, List, ModuleInfo) :-
	( Uniq = shared ->
		bound_inst_list_is_not_partly_unique(List, ModuleInfo)
	; Uniq = mostly_unique ->
		bound_inst_list_is_not_fully_unique(List, ModuleInfo)
	;
		true
	).

%-----------------------------------------------------------------------------%

	% Here we check that the functors in the first list are a
	% subset of the functors in the second list. 
	% (If a bound(...) inst only specifies the insts for some of
	% the constructors of its type, then it implicitly means that
	% all other constructors must have all their arguments
	% `not_reached'.)
	% The code here makes use of the fact that the bound_inst lists
	% are sorted.

:- pred bound_inst_list_matches_initial(list(bound_inst), list(bound_inst),
					module_info, expansions, expansions).
:- mode bound_inst_list_matches_initial(in, in, in, in, out) is semidet.

bound_inst_list_matches_initial([], _, _, Expansions, Expansions).
bound_inst_list_matches_initial([X|Xs], [Y|Ys], ModuleInfo,
		Expansions0, Expansions) :-
	X = functor(ConsIdX, ArgsX),
	Y = functor(ConsIdY, ArgsY),
	( ConsIdX = ConsIdY ->
		inst_list_matches_initial(ArgsX, ArgsY, ModuleInfo,
			Expansions0, Expansions1),
		bound_inst_list_matches_initial(Xs, Ys, ModuleInfo,
			Expansions1, Expansions)
	;
		compare(>, ConsIdX, ConsIdY),
			% ConsIdY does not occur in [X|Xs].
			% Hence [X|Xs] implicitly specifies `not_reached'
			% for the args of ConsIdY, and hence 
			% automatically matches_initial Y.  We just need to
			% check that [X|Xs] matches_initial Ys.
		bound_inst_list_matches_initial([X|Xs], Ys, ModuleInfo,
					Expansions0, Expansions)
	).

:- pred inst_list_matches_initial(list(inst), list(inst), module_info,
				expansions, expansions).
:- mode inst_list_matches_initial(in, in, in, in, out) is semidet.

inst_list_matches_initial([], [], _, Expansions, Expansions).
inst_list_matches_initial([X|Xs], [Y|Ys], ModuleInfo,
		Expansions0, Expansions) :-
	inst_matches_initial_2(X, Y, ModuleInfo, Expansions0, Expansions1),
	inst_list_matches_initial(Xs, Ys, ModuleInfo, Expansions1, Expansions).

%-----------------------------------------------------------------------------%

inst_expand(ModuleInfo, Inst0, Inst) :-
	( Inst0 = defined_inst(InstName) ->
		inst_lookup(ModuleInfo, InstName, Inst1),
		inst_expand(ModuleInfo, Inst1, Inst)
	;
		Inst = Inst0
	).

%-----------------------------------------------------------------------------%

inst_matches_final(InstA, InstB, ModuleInfo) :-
	set__init(Expansions0),
	inst_matches_final_2(InstA, InstB, ModuleInfo,
		Expansions0, _Expansions).

:- pred inst_matches_final_2(inst, inst, module_info, expansions, expansions).
:- mode inst_matches_final_2(in, in, in, in, out) is semidet.

inst_matches_final_2(InstA, InstB, ModuleInfo, Expansions0, Expansions) :-
	ThisExpansion = InstA - InstB,
	( set__member(ThisExpansion, Expansions0) ->
		Expansions = Expansions0
	; InstA = InstB ->
		Expansions = Expansions0
	;
		inst_expand(ModuleInfo, InstA, InstA2),
		inst_expand(ModuleInfo, InstB, InstB2),
		set__insert(Expansions0, ThisExpansion, Expansions1),
		inst_matches_final_3(InstA2, InstB2, ModuleInfo,
			Expansions1, Expansions)
	).

:- pred inst_matches_final_3(inst, inst, module_info, expansions, expansions).
:- mode inst_matches_final_3(in, in, in, in, out) is semidet.

inst_matches_final_3(any(UniqA), any(UniqB), _, Expansions, Expansions) :-
	unique_matches_final(UniqA, UniqB).
inst_matches_final_3(free, any(Uniq), _, Expansions, Expansions) :-
	% We do not yet allow `free' to match `any',
	% unless the `any' is `clobbered_any' or `mostly_clobbered_any'.
	% Among other things, changing this would break compare_inst
	% in modecheck_call.m.
	( Uniq = clobbered ; Uniq = mostly_clobbered ).
inst_matches_final_3(free, free, _, Expansions, Expansions).
inst_matches_final_3(bound(UniqA, ListA), any(UniqB), ModuleInfo,
		Expansions, Expansions) :-
	unique_matches_final(UniqA, UniqB),
	bound_inst_list_matches_uniq(ListA, UniqB, ModuleInfo),
	% We do not yet allow `free' to match `any'.
	% Among other things, changing this would break compare_inst
	% in modecheck_call.m.
	bound_inst_list_is_ground_or_any(ListA, ModuleInfo).
inst_matches_final_3(bound(UniqA, ListA), bound(UniqB, ListB), ModuleInfo,
		Expansions0, Expansions) :-
	unique_matches_final(UniqA, UniqB),
	bound_inst_list_matches_final(ListA, ListB, ModuleInfo,
		Expansions0, Expansions).
inst_matches_final_3(bound(UniqA, ListA), ground(UniqB, no), ModuleInfo,
		Expansions, Expansions) :-
	unique_matches_final(UniqA, UniqB),
	bound_inst_list_is_ground(ListA, ModuleInfo),
	bound_inst_list_matches_uniq(ListA, UniqB, ModuleInfo).
inst_matches_final_3(ground(UniqA, _), any(UniqB), _ModuleInfo,
		Expansions, Expansions) :-
	unique_matches_final(UniqA, UniqB).
inst_matches_final_3(ground(UniqA, _), bound(UniqB, ListB), ModuleInfo,
		Expansions, Expansions) :-
	unique_matches_final(UniqA, UniqB),
	bound_inst_list_is_ground(ListB, ModuleInfo),
	uniq_matches_bound_inst_list(UniqA, ListB, ModuleInfo).
		% XXX BUG! Should fail if there are not_reached
		% insts in ListB, or if ListB does not contain a complete list
		% of all the constructors for the type in question.
	%%% error("not implemented: `ground' matches_final `bound(...)'").
inst_matches_final_3(ground(UniqA, PredInstA), ground(UniqB, PredInstB),
		ModuleInfo, Expansions0, Expansions) :-
	maybe_pred_inst_matches_final(PredInstA, PredInstB,
		ModuleInfo, Expansions0, Expansions),
	unique_matches_final(UniqA, UniqB).
inst_matches_final_3(abstract_inst(_, _), any(shared), _,
		Expansions, Expansions).
inst_matches_final_3(abstract_inst(Name, ArgsA), abstract_inst(Name, ArgsB),
		ModuleInfo, Expansions0, Expansions) :-
	inst_list_matches_final(ArgsA, ArgsB, ModuleInfo,
		Expansions0, Expansions).
inst_matches_final_3(not_reached, _, _, Expansions, Expansions).

:- pred maybe_pred_inst_matches_final(maybe(pred_inst_info),
		maybe(pred_inst_info), module_info, expansions, expansions).
:- mode maybe_pred_inst_matches_final(in, in, in, in, out) is semidet.

maybe_pred_inst_matches_final(no, no, _, Expansions, Expansions).
maybe_pred_inst_matches_final(yes(_), no, _, Expansions, Expansions).
maybe_pred_inst_matches_final(yes(PredInstA), yes(PredInstB),
		ModuleInfo, Expansions0, Expansions) :-
	pred_inst_matches_2(PredInstA, PredInstB, ModuleInfo,
		Expansions0, Expansions).

:- pred inst_list_matches_final(list(inst), list(inst), module_info,
				expansions, expansions).
:- mode inst_list_matches_final(in, in, in, in, out) is semidet.

inst_list_matches_final([], [], _ModuleInfo, Expansions, Expansions).
inst_list_matches_final([ArgA | ArgsA], [ArgB | ArgsB], ModuleInfo,
			Expansions0, Expansions) :-
	inst_matches_final_2(ArgA, ArgB, ModuleInfo,
		Expansions0, Expansions1),
	inst_list_matches_final(ArgsA, ArgsB, ModuleInfo,
		Expansions1, Expansions).

	% Here we check that the functors in the first list are a
	% subset of the functors in the second list. 
	% (If a bound(...) inst only specifies the insts for some of
	% the constructors of its type, then it implicitly means that
	% all other constructors must have all their arguments
	% `not_reached'.)
	% The code here makes use of the fact that the bound_inst lists
	% are sorted.

:- pred bound_inst_list_matches_final(list(bound_inst), list(bound_inst),
				module_info, expansions, expansions).
:- mode bound_inst_list_matches_final(in, in, in, in, out) is semidet.

bound_inst_list_matches_final([], _, _, Expansions, Expansions).
bound_inst_list_matches_final([X|Xs], [Y|Ys], ModuleInfo,
		Expansions0, Expansions) :-
	X = functor(ConsIdX, ArgsX),
	Y = functor(ConsIdY, ArgsY),
	( ConsIdX = ConsIdY ->
		inst_list_matches_final(ArgsX, ArgsY, ModuleInfo,
			Expansions0, Expansions1),
		bound_inst_list_matches_final(Xs, Ys, ModuleInfo,
			Expansions1, Expansions)
	;
		compare(>, ConsIdX, ConsIdY),
			% ConsIdY does not occur in [X|Xs].
			% Hence [X|Xs] implicitly specifies `not_reached'
			% for the args of ConsIdY, and hence 
			% automatically matches_final Y.  We just need to
			% check that [X|Xs] matches_final Ys.
		bound_inst_list_matches_final([X|Xs], Ys, ModuleInfo,
			Expansions0, Expansions)
	).

inst_matches_binding(InstA, InstB, ModuleInfo) :-
	set__init(Expansions0),
	inst_matches_binding_2(InstA, InstB, ModuleInfo,
		Expansions0, _Expansions).

:- pred inst_matches_binding_2(inst, inst, module_info, expansions, expansions).
:- mode inst_matches_binding_2(in, in, in, in, out) is semidet.

inst_matches_binding_2(InstA, InstB, ModuleInfo, Expansions0, Expansions) :-
	ThisExpansion = InstA - InstB,
	( set__member(ThisExpansion, Expansions0) ->
		Expansions = Expansions0
	;
		inst_expand(ModuleInfo, InstA, InstA2),
		inst_expand(ModuleInfo, InstB, InstB2),
		set__insert(Expansions0, ThisExpansion, Expansions1),
		inst_matches_binding_3(InstA2, InstB2, ModuleInfo,
			Expansions1, Expansions)
	).

:- pred inst_matches_binding_3(inst, inst, module_info, expansions, expansions).
:- mode inst_matches_binding_3(in, in, in, in, out) is semidet.

% Note that `any' is *not* considered to match `any'.
inst_matches_binding_3(free, free, _, Expansions, Expansions).
inst_matches_binding_3(bound(_UniqA, ListA), bound(_UniqB, ListB), ModuleInfo,
		Expansions0, Expansions) :-
	bound_inst_list_matches_binding(ListA, ListB, ModuleInfo,
		Expansions0, Expansions).
inst_matches_binding_3(bound(_UniqA, ListA), ground(_UniqB, no), ModuleInfo,
		Expansions, Expansions) :-
	bound_inst_list_is_ground(ListA, ModuleInfo).
inst_matches_binding_3(ground(_UniqA, _), bound(_UniqB, ListB), ModuleInfo,
		Expansions, Expansions) :-
	bound_inst_list_is_ground(ListB, ModuleInfo).
		% XXX BUG! Should fail if there are not_reached
		% insts in ListB, or if ListB does not contain a complete list
		% of all the constructors for the type in question.
	%%% error("not implemented: `ground' matches_binding `bound(...)'").
inst_matches_binding_3(ground(_UniqA, PredInstA), ground(_UniqB, PredInstB),
		ModuleInfo, Expansions, Expansions) :-
	pred_inst_matches_binding(PredInstA, PredInstB, ModuleInfo).
inst_matches_binding_3(abstract_inst(Name, ArgsA), abstract_inst(Name, ArgsB),
		ModuleInfo, Expansions0, Expansions) :-
	inst_list_matches_binding(ArgsA, ArgsB, ModuleInfo,
		Expansions0, Expansions).
inst_matches_binding_3(not_reached, _, _, Expansions, Expansions).

:- pred pred_inst_matches_binding(maybe(pred_inst_info), maybe(pred_inst_info),
		module_info).
:- mode pred_inst_matches_binding(in, in, in) is semidet.

pred_inst_matches_binding(no, no, _).
pred_inst_matches_binding(yes(_), no, _).
pred_inst_matches_binding(yes(PredInstA), yes(PredInstB), ModuleInfo) :-
	pred_inst_matches(PredInstA, PredInstB, ModuleInfo).

:- pred inst_list_matches_binding(list(inst), list(inst), module_info,
				expansions, expansions).
:- mode inst_list_matches_binding(in, in, in, in, out) is semidet.

inst_list_matches_binding([], [], _ModuleInfo, Expansions, Expansions).
inst_list_matches_binding([ArgA | ArgsA], [ArgB | ArgsB], ModuleInfo,
			Expansions0, Expansions) :-
	inst_matches_binding_2(ArgA, ArgB, ModuleInfo,
		Expansions0, Expansions1),
	inst_list_matches_binding(ArgsA, ArgsB, ModuleInfo,
		Expansions1, Expansions).

	% Here we check that the functors in the first list are a
	% subset of the functors in the second list. 
	% (If a bound(...) inst only specifies the insts for some of
	% the constructors of its type, then it implicitly means that
	% all other constructors must have all their arguments
	% `not_reached'.)
	% The code here makes use of the fact that the bound_inst lists
	% are sorted.

:- pred bound_inst_list_matches_binding(list(bound_inst), list(bound_inst),
					module_info, expansions, expansions).
:- mode bound_inst_list_matches_binding(in, in, in, in, out) is semidet.

bound_inst_list_matches_binding([], _, _, Expansions, Expansions).
bound_inst_list_matches_binding([X|Xs], [Y|Ys], ModuleInfo,
		Expansions0, Expansions) :-
	X = functor(ConsIdX, ArgsX),
	Y = functor(ConsIdY, ArgsY),
	( ConsIdX = ConsIdY ->
		inst_list_matches_binding(ArgsX, ArgsY, ModuleInfo,
			Expansions0, Expansions1),
		bound_inst_list_matches_binding(Xs, Ys, ModuleInfo,
			Expansions1, Expansions)
	;
		compare(>, ConsIdX, ConsIdY),
			% ConsIdX does not occur in [X|Xs].
			% Hence [X|Xs] implicitly specifies `not_reached'
			% for the args of ConsIdY, and hence 
			% automatically matches_binding Y.  We just need to
			% check that [X|Xs] matches_binding Ys.
		bound_inst_list_matches_binding([X|Xs], Ys, ModuleInfo,
			Expansions0, Expansions)
	).

%-----------------------------------------------------------------------------%

        % inst_is_clobbered succeeds iff the inst passed is `clobbered'
        % or `mostly_clobbered' or if it is a user-defined inst which
        % is defined as one of those.

inst_is_clobbered(_, not_reached) :- fail.
inst_is_clobbered(_, any(mostly_clobbered)).
inst_is_clobbered(_, any(clobbered)).
inst_is_clobbered(_, ground(clobbered, _)).
inst_is_clobbered(_, ground(mostly_clobbered, _)).
inst_is_clobbered(_, bound(clobbered, _)).
inst_is_clobbered(_, bound(mostly_clobbered, _)).
inst_is_clobbered(_, inst_var(_)) :-
        error("internal error: uninstantiated inst parameter").
inst_is_clobbered(ModuleInfo, defined_inst(InstName)) :-
        inst_lookup(ModuleInfo, InstName, Inst),
        inst_is_clobbered(ModuleInfo, Inst).

        % inst_is_free succeeds iff the inst passed is `free'
        % or is a user-defined inst which is defined as `free'.
        % Abstract insts must not be free.

inst_is_free(_, free).
inst_is_free(_, free(_Type)).
inst_is_free(_, inst_var(_)) :-
        error("internal error: uninstantiated inst parameter").
inst_is_free(ModuleInfo, defined_inst(InstName)) :-
        inst_lookup(ModuleInfo, InstName, Inst),
        inst_is_free(ModuleInfo, Inst).

        % inst_is_bound succeeds iff the inst passed is not `free'
        % or is a user-defined inst which is not defined as `free'.
        % Abstract insts must be bound.

inst_is_bound(_, not_reached).
inst_is_bound(_, any(_)).
inst_is_bound(_, ground(_, _)).
inst_is_bound(_, bound(_, _)).
inst_is_bound(_, inst_var(_)) :-
        error("internal error: uninstantiated inst parameter").
inst_is_bound(ModuleInfo, defined_inst(InstName)) :-
        inst_lookup(ModuleInfo, InstName, Inst),
        inst_is_bound(ModuleInfo, Inst).
inst_is_bound(_, abstract_inst(_, _)).

        % inst_is_bound_to_functors succeeds iff the inst passed is
        % `bound(_Uniq, Functors)' or is a user-defined inst which expands to
        % `bound(_Uniq, Functors)'.

inst_is_bound_to_functors(_, bound(_Uniq, Functors), Functors).
inst_is_bound_to_functors(_, inst_var(_), _) :-
        error("internal error: uninstantiated inst parameter").
inst_is_bound_to_functors(ModuleInfo, defined_inst(InstName), Functors) :-
        inst_lookup(ModuleInfo, InstName, Inst),
        inst_is_bound_to_functors(ModuleInfo, Inst, Functors).

%-----------------------------------------------------------------------------%

        % inst_is_ground succeeds iff the inst passed is `ground'
        % or the equivalent.  Abstract insts are not considered ground.

inst_is_ground(ModuleInfo, Inst) :-
        set__init(Expansions0),
        inst_is_ground_2(ModuleInfo, Inst, Expansions0, _Expansions).

        % The third arg is the set of insts which have already
        % been expanded - we use this to avoid going into an
        % infinite loop.

:- pred inst_is_ground_2(module_info, inst, set(inst), set(inst)).
:- mode inst_is_ground_2(in, in, in, out) is semidet.

inst_is_ground_2(_, not_reached, Expansions, Expansions).
inst_is_ground_2(ModuleInfo, bound(_, List), Expansions0, Expansions) :-
        bound_inst_list_is_ground_2(List, ModuleInfo, Expansions0, Expansions).
inst_is_ground_2(_, ground(_, _), Expansions, Expansions).
inst_is_ground_2(_, inst_var(_), Expansions, Expansions) :-
        error("internal error: uninstantiated inst parameter").
inst_is_ground_2(ModuleInfo, Inst, Expansions0, Expansions) :-
	Inst = defined_inst(InstName),
        ( set__member(Inst, Expansions0) ->
                Expansions = Expansions0
        ;
                set__insert(Expansions0, Inst, Expansions1),
                inst_lookup(ModuleInfo, InstName, Inst2),
                inst_is_ground_2(ModuleInfo, Inst2, Expansions1, Expansions)
        ).

        % inst_is_ground_or_any succeeds iff the inst passed is `ground',
        % `any', or the equivalent.  Fails for abstract insts.

inst_is_ground_or_any(ModuleInfo, Inst) :-
        set__init(Expansions0),
        inst_is_ground_or_any_2(ModuleInfo, Inst, Expansions0, _Expansions).

        % The third arg is the set of insts which have already
        % been expanded - we use this to avoid going into an
        % infinite loop.

:- pred inst_is_ground_or_any_2(module_info, inst, set(inst), set(inst)).
:- mode inst_is_ground_or_any_2(in, in, in, out) is semidet.

inst_is_ground_or_any_2(_, not_reached, Expansions, Expansions).
inst_is_ground_or_any_2(ModuleInfo, bound(_, List), Expansions0, Expansions) :-
        bound_inst_list_is_ground_or_any_2(List, ModuleInfo,
		Expansions0, Expansions).
inst_is_ground_or_any_2(_, ground(_, _), Expansions, Expansions).
inst_is_ground_or_any_2(_, any(_), Expansions, Expansions).
inst_is_ground_or_any_2(_, inst_var(_), _, _) :-
        error("internal error: uninstantiated inst parameter").
inst_is_ground_or_any_2(ModuleInfo, Inst, Expansions0, Expansions) :-
	Inst = defined_inst(InstName),
        ( set__member(Inst, Expansions0) ->
                Expansions = Expansions0
        ;
                set__insert(Expansions0, Inst, Expansions1),
                inst_lookup(ModuleInfo, InstName, Inst2),
                inst_is_ground_or_any_2(ModuleInfo, Inst2,
			Expansions1, Expansions)
        ).

        % inst_is_unique succeeds iff the inst passed is unique
        % or free.  Abstract insts are not considered unique.

inst_is_unique(ModuleInfo, Inst) :-
        set__init(Expansions0),
        inst_is_unique_2(ModuleInfo, Inst, Expansions0, _Expansions).

        % The third arg is the set of insts which have already
        % been expanded - we use this to avoid going into an
        % infinite loop.

:- pred inst_is_unique_2(module_info, inst, set(inst), set(inst)).
:- mode inst_is_unique_2(in, in, in, out) is semidet.

inst_is_unique_2(_, not_reached, Expansions, Expansions).
inst_is_unique_2(ModuleInfo, bound(unique, List), Expansions0, Expansions) :-
        bound_inst_list_is_unique_2(List, ModuleInfo, Expansions0, Expansions).
inst_is_unique_2(_, any(unique), Expansions, Expansions).
inst_is_unique_2(_, free, Expansions, Expansions).
inst_is_unique_2(_, ground(unique, _), Expansions, Expansions).
inst_is_unique_2(_, inst_var(_), _, _) :-
        error("internal error: uninstantiated inst parameter").
inst_is_unique_2(ModuleInfo, Inst, Expansions0, Expansions) :-
	Inst = defined_inst(InstName),
        ( set__member(Inst, Expansions0) ->
                Expansions = Expansions0
        ;
                set__insert(Expansions0, Inst, Expansions1),
                inst_lookup(ModuleInfo, InstName, Inst2),
                inst_is_unique_2(ModuleInfo, Inst2, Expansions1, Expansions)
        ).

        % inst_is_mostly_unique succeeds iff the inst passed is unique,
        % mostly_unique, or free.  Abstract insts are not considered unique.

inst_is_mostly_unique(ModuleInfo, Inst) :-
        set__init(Expansions0),
        inst_is_mostly_unique_2(ModuleInfo, Inst, Expansions0, _Expansions).

        % The third arg is the set of insts which have already
        % been expanded - we use this to avoid going into an
        % infinite loop.

:- pred inst_is_mostly_unique_2(module_info, inst, set(inst), set(inst)).
:- mode inst_is_mostly_unique_2(in, in, in, out) is semidet.

inst_is_mostly_unique_2(_, not_reached, Expansions, Expansions).
inst_is_mostly_unique_2(ModuleInfo, bound(unique, List),
		Expansions0, Expansions) :-
        bound_inst_list_is_mostly_unique_2(List, ModuleInfo,
		Expansions0, Expansions).
inst_is_mostly_unique_2(ModuleInfo, bound(mostly_unique, List),
		Expansions0, Expansions) :-
        bound_inst_list_is_mostly_unique_2(List, ModuleInfo,
		Expansions0, Expansions).
inst_is_mostly_unique_2(_, any(unique), Expansions, Expansions).
inst_is_mostly_unique_2(_, any(mostly_unique), Expansions, Expansions).
inst_is_mostly_unique_2(_, free, Expansions, Expansions).
inst_is_mostly_unique_2(_, ground(unique, _), Expansions, Expansions).
inst_is_mostly_unique_2(_, ground(mostly_unique, _), Expansions, Expansions).
inst_is_mostly_unique_2(_, inst_var(_), _, _) :-
        error("internal error: uninstantiated inst parameter").
inst_is_mostly_unique_2(ModuleInfo, Inst, Expansions0, Expansions) :-
	Inst = defined_inst(InstName),
        ( set__member(Inst, Expansions0) ->
                Expansions = Expansions0
        ;
                set__insert(Expansions0, Inst, Expansions1),
                inst_lookup(ModuleInfo, InstName, Inst2),
                inst_is_mostly_unique_2(ModuleInfo, Inst2,
			Expansions1, Expansions)
        ).

        % inst_is_not_partly_unique succeeds iff the inst passed is
        % not unique or mostly_unique, i.e. if it is shared
        % or free.  It fails for abstract insts.

inst_is_not_partly_unique(ModuleInfo, Inst) :-
        set__init(Expansions0),
        inst_is_not_partly_unique_2(ModuleInfo, Inst, Expansions0, _Expansions).

        % The third arg is the set of insts which have already
        % been expanded - we use this to avoid going into an
        % infinite loop.

:- pred inst_is_not_partly_unique_2(module_info, inst, set(inst), set(inst)).
:- mode inst_is_not_partly_unique_2(in, in, in, out) is semidet.

inst_is_not_partly_unique_2(_, not_reached, Expansions, Expansions).
inst_is_not_partly_unique_2(ModuleInfo, bound(shared, List),
		Expansions0, Expansions) :-
        bound_inst_list_is_not_partly_unique_2(List, ModuleInfo,
		Expansions0, Expansions).
inst_is_not_partly_unique_2(_, free, Expansions, Expansions).
inst_is_not_partly_unique_2(_, any(shared), Expansions, Expansions).
inst_is_not_partly_unique_2(_, ground(shared, _), Expansions, Expansions).
inst_is_not_partly_unique_2(_, inst_var(_), _, _) :-
        error("internal error: uninstantiated inst parameter").
inst_is_not_partly_unique_2(ModuleInfo, Inst, Expansions0, Expansions) :-
	Inst = defined_inst(InstName),
        ( set__member(Inst, Expansions0) ->
                Expansions = Expansions0
        ;
                set__insert(Expansions0, Inst, Expansions1),
                inst_lookup(ModuleInfo, InstName, Inst2),
                inst_is_not_partly_unique_2(ModuleInfo, Inst2,
			Expansions1, Expansions)
        ).

	% inst_is_not_fully_unique succeeds iff the inst passed is
        % not unique, i.e. if it is mostly_unique, shared,
        % or free.  It fails for abstract insts.

inst_is_not_fully_unique(ModuleInfo, Inst) :-
        set__init(Expansions0),
        inst_is_not_fully_unique_2(ModuleInfo, Inst,
		Expansions0, _Expansions).

        % The third arg is the set of insts which have already
        % been expanded - we use this to avoid going into an
        % infinite loop.

:- pred inst_is_not_fully_unique_2(module_info, inst, set(inst), set(inst)).
:- mode inst_is_not_fully_unique_2(in, in, in, out) is semidet.

inst_is_not_fully_unique_2(_, not_reached, Expansions, Expansions).
inst_is_not_fully_unique_2(ModuleInfo, bound(shared, List),
		Expansions0, Expansions) :-
        bound_inst_list_is_not_fully_unique_2(List, ModuleInfo,
		Expansions0, Expansions).
inst_is_not_fully_unique_2(ModuleInfo, bound(mostly_unique, List), 
                Expansions0, Expansions) :-
        bound_inst_list_is_not_fully_unique_2(List, ModuleInfo,
		Expansions0, Expansions).
inst_is_not_fully_unique_2(_, any(shared), Expansions, Expansions).
inst_is_not_fully_unique_2(_, any(mostly_unique), Expansions, Expansions).
inst_is_not_fully_unique_2(_, free, Expansions, Expansions).
inst_is_not_fully_unique_2(_, ground(shared, _), Expansions, Expansions).
inst_is_not_fully_unique_2(_, ground(mostly_unique, _), Expansions, Expansions).
inst_is_not_fully_unique_2(_, inst_var(_), _, _) :-
        error("internal error: uninstantiated inst parameter").
inst_is_not_fully_unique_2(ModuleInfo, Inst, Expansions0, Expansions) :-
	Inst = defined_inst(InstName),
        ( set__member(Inst, Expansions0) ->
                Expansions = Expansions0
        ;
                set__insert(Expansions0, Inst, Expansions1),
                inst_lookup(ModuleInfo, InstName, Inst2),
                inst_is_not_fully_unique_2(ModuleInfo, Inst2,
			Expansions1, Expansions)
        ).

%-----------------------------------------------------------------------------%

bound_inst_list_is_ground([], _). 
bound_inst_list_is_ground([functor(_Name, Args)|BoundInsts], ModuleInfo) :-
        inst_list_is_ground(Args, ModuleInfo),
        bound_inst_list_is_ground(BoundInsts, ModuleInfo).

bound_inst_list_is_ground_or_any([], _).
bound_inst_list_is_ground_or_any([functor(_Name, Args)|BoundInsts],
                ModuleInfo) :-
        inst_list_is_ground_or_any(Args, ModuleInfo),
        bound_inst_list_is_ground_or_any(BoundInsts, ModuleInfo).

bound_inst_list_is_unique([], _). 
bound_inst_list_is_unique([functor(_Name, Args)|BoundInsts], ModuleInfo) :-
        inst_list_is_unique(Args, ModuleInfo),
        bound_inst_list_is_unique(BoundInsts, ModuleInfo).

bound_inst_list_is_mostly_unique([], _).
bound_inst_list_is_mostly_unique([functor(_Name, Args)|BoundInsts],
                ModuleInfo) :-
        inst_list_is_mostly_unique(Args, ModuleInfo),
        bound_inst_list_is_mostly_unique(BoundInsts, ModuleInfo).

bound_inst_list_is_not_partly_unique([], _).
bound_inst_list_is_not_partly_unique([functor(_Name, Args)|BoundInsts],
                ModuleInfo) :-
        inst_list_is_not_partly_unique(Args, ModuleInfo),
        bound_inst_list_is_not_partly_unique(BoundInsts, ModuleInfo).

bound_inst_list_is_not_fully_unique([], _).
bound_inst_list_is_not_fully_unique([functor(_Name, Args)|BoundInsts],
                ModuleInfo) :-
        inst_list_is_not_fully_unique(Args, ModuleInfo),
        bound_inst_list_is_not_fully_unique(BoundInsts, ModuleInfo).

%-----------------------------------------------------------------------------%

:- pred bound_inst_list_is_ground_2(list(bound_inst), module_info,
		set(inst), set(inst)).
:- mode bound_inst_list_is_ground_2(in, in, in, out) is semidet.

bound_inst_list_is_ground_2([], _, Expansions, Expansions).
bound_inst_list_is_ground_2([functor(_Name, Args)|BoundInsts], ModuleInfo,
                Expansions0, Expansions) :-
	inst_list_is_ground_2(Args, ModuleInfo, Expansions0, Expansions1),
        bound_inst_list_is_ground_2(BoundInsts, ModuleInfo,
		Expansions1, Expansions).

:- pred bound_inst_list_is_ground_or_any_2(list(bound_inst), module_info,
						set(inst), set(inst)).
:- mode bound_inst_list_is_ground_or_any_2(in, in, in, out) is semidet.

bound_inst_list_is_ground_or_any_2([], _, Expansions, Expansions).
bound_inst_list_is_ground_or_any_2([functor(_Name, Args)|BoundInsts],
                ModuleInfo, Expansions0, Expansions) :-
        inst_list_is_ground_or_any_2(Args, ModuleInfo,
		Expansions0, Expansions1),
        bound_inst_list_is_ground_or_any_2(BoundInsts, ModuleInfo,
		Expansions1, Expansions).

:- pred bound_inst_list_is_unique_2(list(bound_inst), module_info,
		set(inst), set(inst)).
:- mode bound_inst_list_is_unique_2(in, in, in, out) is semidet.

bound_inst_list_is_unique_2([], _, Expansions, Expansions).
bound_inst_list_is_unique_2([functor(_Name, Args)|BoundInsts], ModuleInfo,
                Expansions0, Expansions) :-
        inst_list_is_unique_2(Args, ModuleInfo, Expansions0, Expansions1),
        bound_inst_list_is_unique_2(BoundInsts, ModuleInfo,
		Expansions1, Expansions).

:- pred bound_inst_list_is_mostly_unique_2(list(bound_inst), module_info,
                                                set(inst), set(inst)).
:- mode bound_inst_list_is_mostly_unique_2(in, in, in, out) is semidet.

bound_inst_list_is_mostly_unique_2([], _, Expansions, Expansions).
bound_inst_list_is_mostly_unique_2([functor(_Name, Args)|BoundInsts],
                ModuleInfo, Expansions0, Expansions) :-
        inst_list_is_mostly_unique_2(Args, ModuleInfo,
		Expansions0, Expansions1),
        bound_inst_list_is_mostly_unique_2(BoundInsts, ModuleInfo,
		Expansions1, Expansions).

:- pred bound_inst_list_is_not_partly_unique_2(list(bound_inst), module_info,
                                                set(inst), set(inst)).
:- mode bound_inst_list_is_not_partly_unique_2(in, in, in, out) is semidet.

bound_inst_list_is_not_partly_unique_2([], _, Expansions, Expansions).
bound_inst_list_is_not_partly_unique_2([functor(_Name, Args)|BoundInsts],
                ModuleInfo, Expansions0, Expansions) :-
        inst_list_is_not_partly_unique_2(Args, ModuleInfo,
		Expansions0, Expansions1),
        bound_inst_list_is_not_partly_unique_2(BoundInsts, ModuleInfo,
                Expansions1, Expansions).

:- pred bound_inst_list_is_not_fully_unique_2(list(bound_inst), module_info,
                                                set(inst), set(inst)).
:- mode bound_inst_list_is_not_fully_unique_2(in, in, in, out) is semidet.

bound_inst_list_is_not_fully_unique_2([], _, Expansions, Expansions).
bound_inst_list_is_not_fully_unique_2([functor(_Name, Args)|BoundInsts],
                ModuleInfo, Expansions0, Expansions) :-
        inst_list_is_not_fully_unique_2(Args, ModuleInfo,
		Expansions0, Expansions1),
        bound_inst_list_is_not_fully_unique_2(BoundInsts, ModuleInfo,
                Expansions1, Expansions).

%-----------------------------------------------------------------------------%

inst_list_is_ground([], _).
inst_list_is_ground([Inst | Insts], ModuleInfo) :-
        inst_is_ground(ModuleInfo, Inst),
        inst_list_is_ground(Insts, ModuleInfo).

inst_list_is_ground_or_any([], _).
inst_list_is_ground_or_any([Inst | Insts], ModuleInfo) :-
        inst_is_ground_or_any(ModuleInfo, Inst),
        inst_list_is_ground_or_any(Insts, ModuleInfo).

inst_list_is_unique([], _).
inst_list_is_unique([Inst | Insts], ModuleInfo) :-
        inst_is_unique(ModuleInfo, Inst),
        inst_list_is_unique(Insts, ModuleInfo).

inst_list_is_mostly_unique([], _).
inst_list_is_mostly_unique([Inst | Insts], ModuleInfo) :-
        inst_is_mostly_unique(ModuleInfo, Inst),
        inst_list_is_mostly_unique(Insts, ModuleInfo).

inst_list_is_not_partly_unique([], _).
inst_list_is_not_partly_unique([Inst | Insts], ModuleInfo) :-
        inst_is_not_partly_unique(ModuleInfo, Inst),
        inst_list_is_not_partly_unique(Insts, ModuleInfo).

inst_list_is_not_fully_unique([], _).
inst_list_is_not_fully_unique([Inst | Insts], ModuleInfo) :-
        inst_is_not_fully_unique(ModuleInfo, Inst),
        inst_list_is_not_fully_unique(Insts, ModuleInfo).

%-----------------------------------------------------------------------------%

:- pred inst_list_is_ground_2(list(inst), module_info, set(inst), set(inst)).
:- mode inst_list_is_ground_2(in, in, in, out) is semidet.

inst_list_is_ground_2([], _, Expansions, Expansions).
inst_list_is_ground_2([Inst | Insts], ModuleInfo, Expansions0, Expansions) :-
        inst_is_ground_2(ModuleInfo, Inst, Expansions0, Expansions1),
        inst_list_is_ground_2(Insts, ModuleInfo, Expansions1, Expansions).

:- pred inst_list_is_ground_or_any_2(list(inst), module_info,
		set(inst), set(inst)).
:- mode inst_list_is_ground_or_any_2(in, in, in, out) is semidet.

inst_list_is_ground_or_any_2([], _, Expansions, Expansions).
inst_list_is_ground_or_any_2([Inst | Insts], ModuleInfo,
		Expansions0, Expansions) :-
        inst_is_ground_or_any_2(ModuleInfo, Inst, Expansions0, Expansions1),
        inst_list_is_ground_or_any_2(Insts, ModuleInfo,
		Expansions1, Expansions).

:- pred inst_list_is_unique_2(list(inst), module_info, set(inst), set(inst)).
:- mode inst_list_is_unique_2(in, in, in, out) is semidet.

inst_list_is_unique_2([], _, Expansions, Expansions).
inst_list_is_unique_2([Inst | Insts], ModuleInfo, Expansions0, Expansions) :-
        inst_is_unique_2(ModuleInfo, Inst, Expansions0, Expansions1),
        inst_list_is_unique_2(Insts, ModuleInfo, Expansions1, Expansions).

:- pred inst_list_is_mostly_unique_2(list(inst), module_info,
		set(inst), set(inst)).
:- mode inst_list_is_mostly_unique_2(in, in, in, out) is semidet.

inst_list_is_mostly_unique_2([], _, Expansions, Expansions).
inst_list_is_mostly_unique_2([Inst | Insts], ModuleInfo,
		Expansions0, Expansions) :-
        inst_is_mostly_unique_2(ModuleInfo, Inst, Expansions0, Expansions1),
        inst_list_is_mostly_unique_2(Insts, ModuleInfo,
		Expansions1, Expansions).

:- pred inst_list_is_not_partly_unique_2(list(inst), module_info,
		set(inst), set(inst)).
:- mode inst_list_is_not_partly_unique_2(in, in, in, out) is semidet.

inst_list_is_not_partly_unique_2([], _, Expansions, Expansions).
inst_list_is_not_partly_unique_2([Inst | Insts], ModuleInfo,
		Expansions0, Expansions) :-
        inst_is_not_partly_unique_2(ModuleInfo, Inst, Expansions0, Expansions1),
        inst_list_is_not_partly_unique_2(Insts, ModuleInfo,
		Expansions1, Expansions).

:- pred inst_list_is_not_fully_unique_2(list(inst), module_info,
		set(inst), set(inst)).
:- mode inst_list_is_not_fully_unique_2(in, in, in, out) is semidet.

inst_list_is_not_fully_unique_2([], _, Expansions, Expansions).
inst_list_is_not_fully_unique_2([Inst | Insts], ModuleInfo,
		Expansions0, Expansions) :-
        inst_is_not_fully_unique_2(ModuleInfo, Inst, Expansions0, Expansions1),
        inst_list_is_not_fully_unique_2(Insts, ModuleInfo,
		Expansions1, Expansions).

%-----------------------------------------------------------------------------%

bound_inst_list_is_free([], _).
bound_inst_list_is_free([functor(_Name, Args)|BoundInsts], ModuleInfo) :-
        inst_list_is_free(Args, ModuleInfo),
        bound_inst_list_is_free(BoundInsts, ModuleInfo).

inst_list_is_free([], _).
inst_list_is_free([Inst | Insts], ModuleInfo) :-
        inst_is_free(ModuleInfo, Inst),
        inst_list_is_free(Insts, ModuleInfo).

%-----------------------------------------------------------------------------%

inst_list_is_ground_or_dead([], [], _).
inst_list_is_ground_or_dead([Inst | Insts], [Live | Lives], ModuleInfo) :-
	( Live = live ->
		inst_is_ground(ModuleInfo, Inst)
	;
		true
	),
	inst_list_is_ground_or_dead(Insts, Lives, ModuleInfo).

inst_list_is_ground_or_any_or_dead([], [], _).
inst_list_is_ground_or_any_or_dead([Inst | Insts], [Live | Lives],
		ModuleInfo) :-
	( Live = live ->
		inst_is_ground_or_any(ModuleInfo, Inst)
	;
		true
	),
	inst_list_is_ground_or_any_or_dead(Insts, Lives, ModuleInfo).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

inst_contains_instname(Inst, ModuleInfo, InstName) :-
	set__init(Expansions0),
	inst_contains_instname_2(Inst, ModuleInfo, InstName,
		yes, Expansions0, _Expansions).

:- pred inst_contains_instname_2(inst, module_info, inst_name, bool,
		set(inst_name), set(inst_name)).
:- mode inst_contains_instname_2(in, in, in, out, in, out) is det.

inst_contains_instname_2(abstract_inst(_, _), _, _, no, Expns, Expns).
inst_contains_instname_2(any(_), _, _, no, Expns, Expns).
inst_contains_instname_2(free, _, _, no, Expns, Expns).
inst_contains_instname_2(free(_T), _, _, no, Expns, Expns).
inst_contains_instname_2(ground(_Uniq, _), _, _, no, Expns, Expns).
inst_contains_instname_2(inst_var(_), _, _, no, Expns, Expns).
inst_contains_instname_2(not_reached, _, _, no, Expns, Expns).
inst_contains_instname_2(defined_inst(InstName1), ModuleInfo, InstName,
		Result, Expansions0, Expansions) :-
	( InstName = InstName1 ->
		Result = yes,
		Expansions = Expansions0
	;
		( set__member(InstName1, Expansions0) ->
			Result = no,
			Expansions = Expansions0
		;
			inst_lookup(ModuleInfo, InstName1, Inst1),
			set__insert(Expansions0, InstName1, Expansions1),
			inst_contains_instname_2(Inst1, ModuleInfo,
				InstName, Result, Expansions1, Expansions)
		)
	).
inst_contains_instname_2(bound(_Uniq, ArgInsts), ModuleInfo, 
		InstName, Result, Expansions0, Expansions) :-
	bound_inst_list_contains_instname(ArgInsts, ModuleInfo,
		InstName, Result, Expansions0, Expansions).

:- pred bound_inst_list_contains_instname(list(bound_inst), module_info,
		inst_name, bool, set(inst_name), set(inst_name)).
:- mode bound_inst_list_contains_instname(in, in, in, out, in, out) is det.

bound_inst_list_contains_instname([], _ModuleInfo,
		_InstName, no, Expansions, Expansions).
bound_inst_list_contains_instname([BoundInst|BoundInsts], ModuleInfo,
		InstName, Result, Expansions0, Expansions) :-
	BoundInst = functor(_Functor, ArgInsts),
	inst_list_contains_instname(ArgInsts, ModuleInfo, InstName, Result1,
		Expansions0, Expansions1),
	( Result1 = yes ->
		Result = yes,
		Expansions = Expansions1
	;
		bound_inst_list_contains_instname(BoundInsts, ModuleInfo,
			InstName, Result, Expansions1, Expansions)
	).

:- pred inst_list_contains_instname(list(inst), module_info, inst_name, bool,
		set(inst_name), set(inst_name)).
:- mode inst_list_contains_instname(in, in, in, out, in, out) is det.

inst_list_contains_instname([], _ModuleInfo, _InstName, no,
		Expansions, Expansions).
inst_list_contains_instname([Inst|Insts], ModuleInfo, InstName, Result,
		Expansions0, Expansions) :-
	inst_contains_instname_2(Inst, ModuleInfo, InstName, Result1,
		Expansions0, Expansions1),
	( Result1 = yes ->
		Result = yes,
		Expansions = Expansions1
	;
		inst_list_contains_instname(Insts, ModuleInfo, InstName,
			Result, Expansions1, Expansions)
	).

%-----------------------------------------------------------------------------%

:- pred inst_name_contains_inst_var(inst_name, inst_var).
:- mode inst_name_contains_inst_var(in, out) is nondet.

inst_name_contains_inst_var(user_inst(_Name, ArgInsts), InstVar) :-
	inst_list_contains_inst_var(ArgInsts, InstVar).
inst_name_contains_inst_var(merge_inst(InstA, InstB), InstVar) :-
	(	inst_contains_inst_var(InstA, InstVar)
	;	inst_contains_inst_var(InstB, InstVar)
	).
inst_name_contains_inst_var(unify_inst(_Live, InstA, InstB, _Real), InstVar) :-
	(	inst_contains_inst_var(InstA, InstVar)
	;	inst_contains_inst_var(InstB, InstVar)
	).
inst_name_contains_inst_var(ground_inst(InstName, _Live, _Uniq, _Real), InstVar)
		:-
	inst_name_contains_inst_var(InstName, InstVar).
inst_name_contains_inst_var(any_inst(InstName, _Live, _Uniq, _Real), InstVar) :-
	inst_name_contains_inst_var(InstName, InstVar).
inst_name_contains_inst_var(shared_inst(InstName), InstVar) :-
	inst_name_contains_inst_var(InstName, InstVar).
inst_name_contains_inst_var(mostly_uniq_inst(InstName), InstVar) :-
	inst_name_contains_inst_var(InstName, InstVar).
inst_name_contains_inst_var(typed_ground(_Uniq, _Type), _InstVar) :- fail.
inst_name_contains_inst_var(typed_inst(_Type, InstName), InstVar) :-
	inst_name_contains_inst_var(InstName, InstVar).

:- pred inst_contains_inst_var(inst, inst_var).
:- mode inst_contains_inst_var(in, out) is nondet.

inst_contains_inst_var(inst_var(InstVar), InstVar).
inst_contains_inst_var(defined_inst(InstName), InstVar) :-
	inst_name_contains_inst_var(InstName, InstVar).
inst_contains_inst_var(bound(_Uniq, ArgInsts), InstVar) :-
	bound_inst_list_contains_inst_var(ArgInsts, InstVar).
inst_contains_inst_var(ground(_Uniq, PredInstInfo), InstVar) :-
	PredInstInfo = yes(pred_inst_info(_PredOrFunc, Modes, _Det)),
	mode_list_contains_inst_var(Modes, InstVar).
inst_contains_inst_var(abstract_inst(_Name, ArgInsts), InstVar) :-
	inst_list_contains_inst_var(ArgInsts, InstVar).

:- pred bound_inst_list_contains_inst_var(list(bound_inst), inst_var).
:- mode bound_inst_list_contains_inst_var(in, out) is nondet.

bound_inst_list_contains_inst_var([BoundInst|BoundInsts], InstVar) :-
	BoundInst = functor(_Functor, ArgInsts),
	(
		inst_list_contains_inst_var(ArgInsts, InstVar)
	;
		bound_inst_list_contains_inst_var(BoundInsts, InstVar)
	).

:- pred inst_list_contains_inst_var(list(inst), inst_var).
:- mode inst_list_contains_inst_var(in, out) is nondet.

inst_list_contains_inst_var([Inst|Insts], InstVar) :-
	(
		inst_contains_inst_var(Inst, InstVar)
	;
		inst_list_contains_inst_var(Insts, InstVar)
	).

mode_list_contains_inst_var(Modes, _ModuleInfo, InstVar) :-
	mode_list_contains_inst_var(Modes, InstVar).

:- pred mode_list_contains_inst_var(list(mode), inst_var).
:- mode mode_list_contains_inst_var(in, out) is nondet.

mode_list_contains_inst_var([Mode|_Modes], InstVar) :-
	mode_contains_inst_var(Mode, InstVar).
mode_list_contains_inst_var([_|Modes], InstVar) :-
	mode_list_contains_inst_var(Modes, InstVar).

:- pred mode_contains_inst_var(mode, inst_var).
:- mode mode_contains_inst_var(in, out) is nondet.

mode_contains_inst_var(Mode, InstVar) :-
	(
		Mode = (Initial -> Final),
	  	( Inst = Initial ; Inst = Final )
	;
		Mode = user_defined_mode(_Name, Insts),
		list__member(Inst, Insts)
	),
	inst_contains_inst_var(Inst, InstVar).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
