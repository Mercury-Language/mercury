%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
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
The handling of `any' insts is not complete.
It would be nice to allow `free', `bound' and `ground' to
match `any', but right now we don't.  Also currently we
don't allow any unifications with variables of mode `any'.
The reason is that although the mode analysis would be pretty
straight-forward, generating the correct code is quite a bit trickier.
In fact, much of the mode analysis code in this file is already
done, just commented out with the remark "not yet".
The exception is abstract unification, which hasn't been done.
In addition, modes.m would have to be changed to handle the implicit
conversions from `free'/`bound'/`ground' to `any' at

	(1) procedure calls (this is just an extension of implied modes)
	(2) the end of branched goals
	(3) the end of predicates.

Since that is not yet done, we currently require the user to
insert explicit calls to initialize constraint variables.
*/

%-----------------------------------------------------------------------------%

:- module inst_match.

:- interface.

:- import_module hlds_module, prog_data.

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

:- pred inst_matches_binding(inst, inst, module_info).
:- mode inst_matches_binding(in, in, in) is semidet.

	% inst_matches_binding(InstA, InstB, ModuleInfo):
	%	 Succeed iff the binding of InstA is definitely exactly the
	%	 same as that of InstB.  This is the same as
	%	 inst_matches_final except that it ignores uniqueness, and
	%	 that `any' does not match itself.  It is used to check
	%	 whether variables get bound in negated contexts.

%-----------------------------------------------------------------------------%

:- pred inst_merge(inst, inst, module_info, inst, module_info).
:- mode inst_merge(in, in, in, out, out) is semidet.

	% inst_merge(InstA, InstB, InstC):
	%	Combine the insts found in different arms of a
	%	disjunction (or if-then-else).
	%	The information in InstC is the minimum of the
	%	information in InstA and InstB.  Where InstA and
	%	InstB specify a binding (free or bound), it must be
	%	the same in both.

%-----------------------------------------------------------------------------%

:- pred abstractly_unify_inst(is_live, inst, inst, unify_is_real, module_info,
				inst, determinism, module_info).
:- mode abstractly_unify_inst(in, in, in, in, in, out, out, out) is semidet.

	% Compute the inst that results from abstractly unifying two variables.

:- pred abstractly_unify_inst_functor(is_live, inst, cons_id, list(inst),
				list(is_live), unify_is_real, module_info,
				inst, module_info).
:- mode abstractly_unify_inst_functor(in, in, in, in, in, in, in, out, out)
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

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module hlds_data, mode_util.
:- import_module list, set, map, std_util, require.

inst_matches_initial(InstA, InstB, ModuleInfo) :-
	set__init(Expansions),
	inst_matches_initial_2(InstA, InstB, ModuleInfo, Expansions).

:- type expansions == set(pair(inst)).

:- pred inst_matches_initial_2(inst, inst, module_info, expansions).
:- mode inst_matches_initial_2(in, in, in, in) is semidet.

inst_matches_initial_2(InstA, InstB, ModuleInfo, Expansions) :-
	ThisExpansion = InstA - InstB,
	( set__member(ThisExpansion, Expansions) ->
		true
/********* 
		% does this test improve efficiency??
	; InstA = InstB ->
		true
**********/
	;
		inst_expand(ModuleInfo, InstA, InstA2),
		inst_expand(ModuleInfo, InstB, InstB2),
		set__insert(Expansions, ThisExpansion, Expansions2),
		inst_matches_initial_3(InstA2, InstB2, ModuleInfo, Expansions2)
	).

:- pred inst_matches_initial_3(inst, inst, module_info, expansions).
:- mode inst_matches_initial_3(in, in, in, in) is semidet.

:- inst_matches_initial_3(InstA, InstB, _, _) when InstA and InstB. % Indexing.

	% To avoid infinite regress, we assume that
	% inst_matches_initial is true for any pairs of insts which
	% occur in `Expansions'.

inst_matches_initial_3(any(UniqA), any(UniqB), _, _) :-
	unique_matches_initial(UniqA, UniqB).
/* not yet:
inst_matches_initial_3(any(_), free, _, _).
inst_matches_initial_3(free, any(_), _, _).
*/
inst_matches_initial_3(free, free, _, _).
/* not yet:
inst_matches_initial_3(bound(UniqA, ListA), any(UniqB), ModuleInfo, _) :-
	unique_matches_initial(UniqA, UniqB),
	bound_inst_list_matches_uniq(ListA, UniqB, ModuleInfo).
*/
inst_matches_initial_3(bound(_Uniq, _List), free, _, _).
inst_matches_initial_3(bound(UniqA, ListA), bound(UniqB, ListB), ModuleInfo,
		Expansions) :-
	unique_matches_initial(UniqA, UniqB),
	bound_inst_list_matches_initial(ListA, ListB, ModuleInfo, Expansions).
inst_matches_initial_3(bound(UniqA, ListA), ground(UniqB, no), ModuleInfo, _) :-
	unique_matches_initial(UniqA, UniqB),
	bound_inst_list_is_ground(ListA, ModuleInfo),
	bound_inst_list_matches_uniq(ListA, UniqB, ModuleInfo).
inst_matches_initial_3(bound(Uniq, List), abstract_inst(_,_), ModuleInfo, _) :-
	Uniq = unique,
	bound_inst_list_is_ground(List, ModuleInfo),
	bound_inst_list_is_unique(List, ModuleInfo).
inst_matches_initial_3(bound(Uniq, List), abstract_inst(_,_), ModuleInfo, _) :-
	Uniq = mostly_unique,
	bound_inst_list_is_ground(List, ModuleInfo),
	bound_inst_list_is_mostly_unique(List, ModuleInfo).
/* not yet:
inst_matches_initial_3(ground(UniqA, _PredInst), any(UniqB), _, _) :-
	unique_matches_initial(UniqA, UniqB).
*/
inst_matches_initial_3(ground(_Uniq, _PredInst), free, _, _).
inst_matches_initial_3(ground(UniqA, _), bound(UniqB, List), ModuleInfo, _) :-
	unique_matches_initial(UniqA, UniqB),
	uniq_matches_bound_inst_list(UniqA, List, ModuleInfo),
	fail.	% XXX BUG! should fail only if 
		% List does not include all the constructors for the type,
		% or if List contains some not_reached insts.
		% Should succeed if List contains all the constructors
		% for the type.  Problem is we don't know what the type was :-(
inst_matches_initial_3(ground(UniqA, PredInstA), ground(UniqB, PredInstB),
		ModuleInfo, _) :-
	maybe_pred_inst_matches_initial(PredInstA, PredInstB, ModuleInfo),
	unique_matches_initial(UniqA, UniqB).
inst_matches_initial_3(ground(_UniqA, no), abstract_inst(_,_), _, _) :-
		% I don't know what this should do.
		% Abstract insts aren't really supported.
	error("inst_matches_initial(ground, abstract_inst) == ??").
/* not yet:
inst_matches_initial_3(abstract_inst(_,_), any(shared), _, _).
*/
inst_matches_initial_3(abstract_inst(_,_), free, _, _).
inst_matches_initial_3(abstract_inst(Name, ArgsA), abstract_inst(Name, ArgsB),
				ModuleInfo, Expansions) :-
	inst_list_matches_initial(ArgsA, ArgsB, ModuleInfo, Expansions).
inst_matches_initial_3(not_reached, _, _, _).

:- pred maybe_pred_inst_matches_initial(maybe(pred_inst_info),
		maybe(pred_inst_info), module_info).
:- mode maybe_pred_inst_matches_initial(in, in, in) is semidet.

maybe_pred_inst_matches_initial(no, no, _).
maybe_pred_inst_matches_initial(yes(_), no, _).
maybe_pred_inst_matches_initial(yes(PredInstA), yes(PredInstB), ModuleInfo) :-
	pred_inst_matches(PredInstA, PredInstB, ModuleInfo).

	% pred_inst_matches(PredInstA, PredInstB, ModuleInfo)
	% 	Succeeds if PredInstA specifies a pred that can
	%	be used wherever and whenever PredInstB could be used.
	%	This is true if they both have the same PredOrFunc indicator
	%	and the same determinism, and if the arguments match
	%	using pred_inst_argmodes_match.
	% pred_inst_matches_2(PredInstA, PredInstB, ModuleInfo, Expansions)
	%	Same as pred_inst_matches/3, except that inst pairs in
	%	Expansions are assumed to match_final each other.
	%	(This avoids infinite loops when calling inst_matches_final
	%	on higher-order recursive insts.)
	%
:- pred pred_inst_matches(pred_inst_info, pred_inst_info, module_info).
:- mode pred_inst_matches(in, in, in) is semidet.

pred_inst_matches(PredInstA, PredInstB, ModuleInfo) :-
	set__init(Expansions),
	pred_inst_matches_2(PredInstA, PredInstB, ModuleInfo, Expansions).

:- pred pred_inst_matches_2(pred_inst_info, pred_inst_info, module_info,
			expansions).
:- mode pred_inst_matches_2(in, in, in, in) is semidet.

pred_inst_matches_2(pred_inst_info(PredOrFunc, ModesA, Det),
		pred_inst_info(PredOrFunc, ModesB, Det),
		ModuleInfo, Expansions) :-
	pred_inst_argmodes_matches(ModesA, ModesB, ModuleInfo, Expansions).

	% pred_inst_matches_argmodes(ModesA, ModesB, ModuleInfo, Expansions):
	% succeeds if the initial insts of ModesB specify at least as
	% much information as, and the same binding as, the initial
	% insts of ModesA; and the final insts of ModesA specify at
	% least as much information as, and the same binding as, the
	% final insts of ModesB.  Any inst pairs in Expansions are assumed
	% to match_final each other.
	%
:- pred pred_inst_argmodes_matches(list(mode), list(mode),
				module_info, expansions).
:- mode pred_inst_argmodes_matches(in, in, in, in) is semidet.

pred_inst_argmodes_matches([], [], _, _).
pred_inst_argmodes_matches([ModeA|ModeAs], [ModeB|ModeBs],
		ModuleInfo, Expansions) :-
	mode_get_insts(ModuleInfo, ModeA, InitialA, FinalA),
	mode_get_insts(ModuleInfo, ModeB, InitialB, FinalB),
	inst_matches_final_2(InitialB, InitialA, ModuleInfo, Expansions),
	inst_matches_final_2(FinalA, FinalB, ModuleInfo, Expansions),
	pred_inst_argmodes_matches(ModeAs, ModeBs, ModuleInfo, Expansions).

:- pred unique_matches_initial(uniqueness, uniqueness).
:- mode unique_matches_initial(in, in) is semidet.

	% unique_matches_initial(A, B) succeeds if A >= B in the ordering
	% clobbered < mostly_clobbered < shared < mostly_unique < unique

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

:- pred unique_matches_final(uniqueness, uniqueness).
:- mode unique_matches_final(in, in) is semidet.

unique_matches_final(A, B) :-
	unique_matches_initial(A, B).

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

	% Here we check that the functors in the first list are a
	% subset of the functors in the second list. 
	% (If a bound(...) inst only specifies the insts for some of
	% the constructors of its type, then it implicitly means that
	% all other constructors must have all their arguments
	% `not_reached'.)
	% The code here makes use of the fact that the bound_inst lists
	% are sorted.

:- pred bound_inst_list_matches_initial(list(bound_inst), list(bound_inst),
					module_info, expansions).
:- mode bound_inst_list_matches_initial(in, in, in, in) is semidet.

bound_inst_list_matches_initial([], _, _, _).
bound_inst_list_matches_initial([X|Xs], [Y|Ys], ModuleInfo, Expansions) :-
	X = functor(ConsIdX, ArgsX),
	Y = functor(ConsIdY, ArgsY),
	( ConsIdX = ConsIdY ->
		inst_list_matches_initial(ArgsX, ArgsY, ModuleInfo, Expansions),
		bound_inst_list_matches_initial(Xs, Ys, ModuleInfo, Expansions)
	;
		compare(>, ConsIdX, ConsIdY),
			% ConsIdY does not occur in [X|Xs].
			% Hence [X|Xs] implicitly specifies `not_reached'
			% for the args of ConsIdY, and hence 
			% automatically matches_initial Y.  We just need to
			% check that [X|Xs] matches_initial Ys.
		bound_inst_list_matches_initial([X|Xs], Ys, ModuleInfo,
					Expansions)
	).

:- pred inst_list_matches_initial(list(inst), list(inst), module_info,
				expansions).
:- mode inst_list_matches_initial(in, in, in, in) is semidet.

inst_list_matches_initial([], [], _, _).
inst_list_matches_initial([X|Xs], [Y|Ys], ModuleInfo, Expansions) :-
	inst_matches_initial_2(X, Y, ModuleInfo, Expansions),
	inst_list_matches_initial(Xs, Ys, ModuleInfo, Expansions).

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
	set__init(Expansions),
	inst_matches_final_2(InstA, InstB, ModuleInfo, Expansions).

:- pred inst_matches_final_2(inst, inst, module_info, expansions).
:- mode inst_matches_final_2(in, in, in, in) is semidet.

inst_matches_final_2(InstA, InstB, ModuleInfo, Expansions) :-
	ThisExpansion = InstA - InstB,
	( set__member(ThisExpansion, Expansions) ->
		true
	; InstA = InstB ->
		true
	;
		inst_expand(ModuleInfo, InstA, InstA2),
		inst_expand(ModuleInfo, InstB, InstB2),
		set__insert(Expansions, ThisExpansion, Expansions2),
		inst_matches_final_3(InstA2, InstB2, ModuleInfo,
			Expansions2)
	).

:- pred inst_matches_final_3(inst, inst, module_info, expansions).
:- mode inst_matches_final_3(in, in, in, in) is semidet.

:- inst_matches_final_3(A, B, _, _) when A and B.

inst_matches_final_3(any(UniqA), any(UniqB), _, _) :-
	unique_matches_final(UniqA, UniqB).
/***
	% not yet:
inst_matches_final_3(free, any(_), _, _).
***/
inst_matches_final_3(free, free, _, _).
/*
not yet:
inst_matches_final_3(bound(UniqA, ListA), any(UniqB), ModuleInfo, Expansions) :-
	unique_matches_final(UniqA, UniqB),
	bound_inst_list_matches_uniq(ListA, UniqB).
*/
inst_matches_final_3(bound(UniqA, ListA), bound(UniqB, ListB), ModuleInfo,
		Expansions) :-
	unique_matches_final(UniqA, UniqB),
	bound_inst_list_matches_final(ListA, ListB, ModuleInfo, Expansions).
inst_matches_final_3(bound(UniqA, ListA), ground(UniqB, no), ModuleInfo,
		_Exps) :-
	unique_matches_final(UniqA, UniqB),
	bound_inst_list_is_ground(ListA, ModuleInfo),
	bound_inst_list_matches_uniq(ListA, UniqB, ModuleInfo).
/* not yet:
inst_matches_final_3(ground(UniqA, _), any(UniqB), ModuleInfo, Expansions) :-
	unique_matches_final(UniqA, UniqB).
*/
inst_matches_final_3(ground(UniqA, _), bound(UniqB, ListB), ModuleInfo,
			_Exps) :-
	unique_matches_final(UniqA, UniqB),
	bound_inst_list_is_ground(ListB, ModuleInfo),
	uniq_matches_bound_inst_list(UniqA, ListB, ModuleInfo).
		% XXX BUG! Should fail if there are not_reached
		% insts in ListB, or if ListB does not contain a complete list
		% of all the constructors for the type in question.
	%%% error("not implemented: `ground' matches_final `bound(...)'").
inst_matches_final_3(ground(UniqA, PredInstA), ground(UniqB, PredInstB),
		ModuleInfo, Expansions) :-
	maybe_pred_inst_matches_final(PredInstA, PredInstB,
		ModuleInfo, Expansions),
	unique_matches_final(UniqA, UniqB).
/* not yet:
inst_matches_final_2(abstract_inst(_, _), any(shared), _, _).
*/
inst_matches_final_3(abstract_inst(Name, ArgsA), abstract_inst(Name, ArgsB),
		ModuleInfo, Expansions) :-
	inst_list_matches_final(ArgsA, ArgsB, ModuleInfo, Expansions).
inst_matches_final_3(not_reached, _, _, _).

:- pred maybe_pred_inst_matches_final(maybe(pred_inst_info),
		maybe(pred_inst_info), module_info, expansions).
:- mode maybe_pred_inst_matches_final(in, in, in, in) is semidet.

maybe_pred_inst_matches_final(no, no, _, _).
maybe_pred_inst_matches_final(yes(_), no, _, _).
maybe_pred_inst_matches_final(yes(PredInstA), yes(PredInstB),
		ModuleInfo, Expansions) :-
	pred_inst_matches_2(PredInstA, PredInstB, ModuleInfo, Expansions).

:- pred inst_list_matches_final(list(inst), list(inst), module_info,
				expansions).
:- mode inst_list_matches_final(in, in, in, in) is semidet.

inst_list_matches_final([], [], _ModuleInfo, _).
inst_list_matches_final([ArgA | ArgsA], [ArgB | ArgsB], ModuleInfo,
			Expansions) :-
	inst_matches_final_2(ArgA, ArgB, ModuleInfo, Expansions),
	inst_list_matches_final(ArgsA, ArgsB, ModuleInfo, Expansions).

	% Here we check that the functors in the first list are a
	% subset of the functors in the second list. 
	% (If a bound(...) inst only specifies the insts for some of
	% the constructors of its type, then it implicitly means that
	% all other constructors must have all their arguments
	% `not_reached'.)
	% The code here makes use of the fact that the bound_inst lists
	% are sorted.

:- pred bound_inst_list_matches_final(list(bound_inst), list(bound_inst),
					module_info, expansions).
:- mode bound_inst_list_matches_final(in, in, in, in) is semidet.

bound_inst_list_matches_final([], _, _, _).
bound_inst_list_matches_final([X|Xs], [Y|Ys], ModuleInfo, Expansions) :-
	X = functor(ConsIdX, ArgsX),
	Y = functor(ConsIdY, ArgsY),
	( ConsIdX = ConsIdY ->
		inst_list_matches_final(ArgsX, ArgsY, ModuleInfo, Expansions),
		bound_inst_list_matches_final(Xs, Ys, ModuleInfo, Expansions)
	;
		compare(>, ConsIdX, ConsIdY),
			% ConsIdY does not occur in [X|Xs].
			% Hence [X|Xs] implicitly specifies `not_reached'
			% for the args of ConsIdY, and hence 
			% automatically matches_final Y.  We just need to
			% check that [X|Xs] matches_final Ys.
		bound_inst_list_matches_final([X|Xs], Ys, ModuleInfo,
					Expansions)
	).

inst_matches_binding(InstA, InstB, ModuleInfo) :-
	set__init(Expansions),
	inst_matches_binding_2(InstA, InstB, ModuleInfo, Expansions).

:- pred inst_matches_binding_2(inst, inst, module_info, expansions).
:- mode inst_matches_binding_2(in, in, in, in) is semidet.

inst_matches_binding_2(InstA, InstB, ModuleInfo, Expansions) :-
	ThisExpansion = InstA - InstB,
	( set__member(ThisExpansion, Expansions) ->
		true
	;
		inst_expand(ModuleInfo, InstA, InstA2),
		inst_expand(ModuleInfo, InstB, InstB2),
		set__insert(Expansions, ThisExpansion, Expansions2),
		inst_matches_binding_3(InstA2, InstB2, ModuleInfo,
			Expansions2)
	).

:- pred inst_matches_binding_3(inst, inst, module_info, expansions).
:- mode inst_matches_binding_3(in, in, in, in) is semidet.

:- inst_matches_binding_3(A, B, _, _) when A and B.

% Note that `any' is *not* considered to match `any'.
inst_matches_binding_3(free, free, _, _).
inst_matches_binding_3(bound(_UniqA, ListA), bound(_UniqB, ListB), ModuleInfo,
		Expansions) :-
	bound_inst_list_matches_binding(ListA, ListB, ModuleInfo, Expansions).
inst_matches_binding_3(bound(_UniqA, ListA), ground(_UniqB, no), ModuleInfo,
		_Exps) :-
	bound_inst_list_is_ground(ListA, ModuleInfo).
inst_matches_binding_3(ground(_UniqA, _), bound(_UniqB, ListB), ModuleInfo,
			_Exps) :-
	bound_inst_list_is_ground(ListB, ModuleInfo).
		% XXX BUG! Should fail if there are not_reached
		% insts in ListB, or if ListB does not contain a complete list
		% of all the constructors for the type in question.
	%%% error("not implemented: `ground' matches_binding `bound(...)'").
inst_matches_binding_3(ground(_UniqA, PredInstA), ground(_UniqB, PredInstB),
		ModuleInfo, _) :-
	pred_inst_matches_binding(PredInstA, PredInstB, ModuleInfo).
inst_matches_binding_3(abstract_inst(Name, ArgsA), abstract_inst(Name, ArgsB),
		ModuleInfo, Expansions) :-
	inst_list_matches_binding(ArgsA, ArgsB, ModuleInfo, Expansions).
inst_matches_binding_3(not_reached, _, _, _).

:- pred pred_inst_matches_binding(maybe(pred_inst_info), maybe(pred_inst_info),
		module_info).
:- mode pred_inst_matches_binding(in, in, in) is semidet.

pred_inst_matches_binding(no, no, _).
pred_inst_matches_binding(yes(_), no, _).
pred_inst_matches_binding(yes(PredInstA), yes(PredInstB), ModuleInfo) :-
	pred_inst_matches(PredInstA, PredInstB, ModuleInfo).

:- pred inst_list_matches_binding(list(inst), list(inst), module_info,
				expansions).
:- mode inst_list_matches_binding(in, in, in, in) is semidet.

inst_list_matches_binding([], [], _ModuleInfo, _).
inst_list_matches_binding([ArgA | ArgsA], [ArgB | ArgsB], ModuleInfo,
			Expansions) :-
	inst_matches_binding_2(ArgA, ArgB, ModuleInfo, Expansions),
	inst_list_matches_binding(ArgsA, ArgsB, ModuleInfo, Expansions).

	% Here we check that the functors in the first list are a
	% subset of the functors in the second list. 
	% (If a bound(...) inst only specifies the insts for some of
	% the constructors of its type, then it implicitly means that
	% all other constructors must have all their arguments
	% `not_reached'.)
	% The code here makes use of the fact that the bound_inst lists
	% are sorted.

:- pred bound_inst_list_matches_binding(list(bound_inst), list(bound_inst),
					module_info, expansions).
:- mode bound_inst_list_matches_binding(in, in, in, in) is semidet.

bound_inst_list_matches_binding([], _, _, _).
bound_inst_list_matches_binding([X|Xs], [Y|Ys], ModuleInfo, Expansions) :-
	X = functor(ConsIdX, ArgsX),
	Y = functor(ConsIdY, ArgsY),
	( ConsIdX = ConsIdY ->
		inst_list_matches_binding(ArgsX, ArgsY, ModuleInfo, Expansions),
		bound_inst_list_matches_binding(Xs, Ys, ModuleInfo, Expansions)
	;
		compare(>, ConsIdX, ConsIdY),
			% ConsIdX does not occur in [X|Xs].
			% Hence [X|Xs] implicitly specifies `not_reached'
			% for the args of ConsIdY, and hence 
			% automatically matches_binding Y.  We just need to
			% check that [X|Xs] matches_binding Ys.
		bound_inst_list_matches_binding([X|Xs], Ys, ModuleInfo,
					Expansions)
	).

%-----------------------------------------------------------------------------%

	% inst_merge(InstA, InstB, InstC):
	%	Combine the insts found in different arms of a
	%	disjunction (or if-then-else).
	%	The information in InstC is the minimum of the
	%	information in InstA and InstB.  Where InstA and
	%	InstB specify a binding (free or bound), it must be
	%	the same in both.

inst_merge(InstA, InstB, ModuleInfo0, Inst, ModuleInfo) :-
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
			%`unknown' 
		map__insert(MergeInstTable0, ThisInstPair, unknown,
			MergeInstTable1),
		inst_table_set_merge_insts(InstTable0, MergeInstTable1,
			InstTable1),
		module_info_set_insts(ModuleInfo0, InstTable1, ModuleInfo1),

			% merge the insts
		inst_merge_2(InstA, InstB, ModuleInfo1, Inst0, ModuleInfo2),

			% now update the value associated with ThisInstPair
		module_info_insts(ModuleInfo2, InstTable2),
		inst_table_get_merge_insts(InstTable2, MergeInstTable2),
		map__set(MergeInstTable2, ThisInstPair, known(Inst0),
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

:- pred inst_merge_2(inst, inst, module_info, inst, module_info).
:- mode inst_merge_2(in, in, in, out, out) is semidet.

inst_merge_2(InstA, InstB, ModuleInfo0, Inst, ModuleInfo) :-
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
		inst_merge_3(InstA2, InstB2, ModuleInfo0, Inst, ModuleInfo)
	).

:- pred inst_merge_3(inst, inst, module_info, inst, module_info).
:- mode inst_merge_3(in, in, in, out, out) is semidet.

:- inst_merge_3(A, B, _, _, _) when A and B.

inst_merge_3(any(UniqA), any(UniqB), M, any(Uniq), M) :-
	merge_uniq(UniqA, UniqB, Uniq).
/* not yet:
inst_merge_3(any(Uniq), free, M, any(Uniq), M).
inst_merge_3(any(UniqA), bound(UniqB, ListB), M, any(Uniq), M) :-
	merge_uniq_bound(UniqA, UniqB, ListB, ModuleInfo, Uniq),
inst_merge_3(any(UniqA), ground(UniqB, _), M, any(Uniq), M) :-
	merge_uniq(UniqA, UniqB, Uniq).
inst_merge_3(any(UniqA), abstract_inst(_, _), M, any(Uniq), M) :-
	merge_uniq(UniqA, shared, Uniq).
inst_merge_3(free, any(Uniq), M, any(Uniq), M).
inst_merge_3(bound(UniqA, ListA), any(UniqB), M, any(Uniq), M) :-
	merge_uniq_bound(UniqB, UniqA, ListA, ModuleInfo, Uniq),
inst_merge_3(ground(UniqA, _), any(UniqB), M, any(Uniq), M) :-
	merge_uniq(UniqA, UniqB).
inst_merge_3(abstract_inst(_, _), any(UniqB), M, any(Uniq), M) :-
	merge_uniq(shared, UniqB, Uniq).
*/
inst_merge_3(free, free, M, free, M).
inst_merge_3(bound(UniqA, ListA), bound(UniqB, ListB), ModuleInfo0,
		bound(Uniq, List), ModuleInfo) :-
	merge_uniq(UniqA, UniqB, Uniq),
	bound_inst_list_merge(ListA, ListB, ModuleInfo0, List, ModuleInfo).
inst_merge_3(bound(UniqA, ListA), ground(UniqB, _), ModuleInfo,
		ground(Uniq, no), ModuleInfo) :-
	merge_uniq_bound(UniqB, UniqA, ListA, ModuleInfo, Uniq),
	bound_inst_list_is_ground(ListA, ModuleInfo).
inst_merge_3(ground(UniqA, _), bound(UniqB, ListB), ModuleInfo,
		ground(Uniq, no), ModuleInfo) :-
	merge_uniq_bound(UniqA, UniqB, ListB, ModuleInfo, Uniq),
	bound_inst_list_is_ground(ListB, ModuleInfo).
inst_merge_3(ground(UniqA, MaybePredA), ground(UniqB, MaybePredB), ModuleInfo,
		ground(Uniq, MaybePred), ModuleInfo) :-
	(
		MaybePredA = yes(PredA),
		MaybePredB = yes(PredB)
	->
		% if they specify matching pred insts, but one is more
		% precise (specifies more info) than the other,
		% then we want to choose the least precise one
		( pred_inst_matches(PredA, PredB, ModuleInfo) ->
			MaybePred = yes(PredB)
		; pred_inst_matches(PredB, PredA, ModuleInfo) ->
			MaybePred = yes(PredA)
		;	
			MaybePred = no
		)
	;	
		MaybePred = no
	),
	merge_uniq(UniqA, UniqB, Uniq).
inst_merge_3(abstract_inst(Name, ArgsA), abstract_inst(Name, ArgsB),
		ModuleInfo0, abstract_inst(Name, Args), ModuleInfo) :-
	inst_list_merge(ArgsA, ArgsB, ModuleInfo0, Args, ModuleInfo).
inst_merge_3(not_reached, Inst, M, Inst, M).

:- pred merge_uniq(uniqueness, uniqueness, uniqueness).
:- mode merge_uniq(in, in, out) is det.

	% merge_uniq(A, B, C) succeeds if C is minimum of A and B in
	% the ordering
	% clobbered < mostly_clobbered < shared < mostly_unique < unique

merge_uniq(UniqA, UniqB, Merged) :-
	( unique_matches_initial(UniqA, UniqB) ->	% A >= B
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
	set__init(Expansions),
	merge_bound_inst_list_uniq(ListB, Uniq0, ModuleInfo, Expansions, Uniq).

:- pred merge_bound_inst_list_uniq(list(bound_inst), uniqueness, module_info,
			set(inst_name), uniqueness).
:- mode merge_bound_inst_list_uniq(in, in, in, in, out) is det.

merge_bound_inst_list_uniq([], Uniq, _, _, Uniq).
merge_bound_inst_list_uniq([BoundInst | BoundInsts], Uniq0,
			ModuleInfo, Expansions, Uniq) :-
	BoundInst = functor(_ConsId, ArgInsts),
	merge_inst_list_uniq(ArgInsts, Uniq0, ModuleInfo, Expansions, Uniq1),
	merge_bound_inst_list_uniq(BoundInsts, Uniq1, ModuleInfo, Expansions,
		Uniq).

:- pred merge_inst_list_uniq(list(inst), uniqueness, module_info,
			set(inst_name), uniqueness).
:- mode merge_inst_list_uniq(in, in, in, in, out) is det.

merge_inst_list_uniq([], Uniq, _, _, Uniq).
merge_inst_list_uniq([Inst | Insts], Uniq0, ModuleInfo, Expansions, Uniq) :-
	merge_inst_uniq(Inst, Uniq0, ModuleInfo, Expansions, Uniq1),
	merge_inst_list_uniq(Insts, Uniq1, ModuleInfo, Expansions, Uniq).

:- pred merge_inst_uniq(inst, uniqueness, module_info, set(inst_name),
			uniqueness).
:- mode merge_inst_uniq(in, in, in, in, out) is det.

merge_inst_uniq(any(UniqA), UniqB, _, _, Uniq) :-
	merge_uniq(UniqA, UniqB, Uniq).
merge_inst_uniq(free, Uniq, _, _, Uniq).
merge_inst_uniq(free(_), Uniq, _, _, Uniq).
merge_inst_uniq(bound(UniqA, ListA), UniqB, ModuleInfo, Expansions, Uniq) :-
	merge_uniq(UniqA, UniqB, Uniq0),
	merge_bound_inst_list_uniq(ListA, Uniq0, ModuleInfo, Expansions, Uniq).
merge_inst_uniq(ground(UniqA, _), UniqB, _, _, Uniq) :-
	merge_uniq(UniqA, UniqB, Uniq).
merge_inst_uniq(abstract_inst(_,_), UniqB, _, _, Uniq) :-
	merge_uniq(shared, UniqB, Uniq).
merge_inst_uniq(defined_inst(InstName), UniqB, ModuleInfo, Expansions,
		Uniq) :-
	( set__member(InstName, Expansions) ->
		Uniq = UniqB
	;
		set__insert(Expansions, InstName, Expansions1),
		inst_lookup(ModuleInfo, InstName, Inst),
		merge_inst_uniq(Inst, UniqB, ModuleInfo, Expansions1, Uniq)
	).
merge_inst_uniq(not_reached, Uniq, _, _, Uniq).
merge_inst_uniq(inst_var(_), _, _, _, _) :-
	error("merge_inst_uniq: unexpected inst_var").

%-----------------------------------------------------------------------------%

:- pred inst_list_merge(list(inst), list(inst), module_info, list(inst),
			module_info).
:- mode inst_list_merge(in, in, in, out, out) is semidet.

inst_list_merge([], [], ModuleInfo, [], ModuleInfo).
inst_list_merge([ArgA | ArgsA], [ArgB | ArgsB], ModuleInfo0,
		[Arg | Args], ModuleInfo) :-
	inst_merge(ArgA, ArgB, ModuleInfo0, Arg, ModuleInfo1),
	inst_list_merge(ArgsA, ArgsB, ModuleInfo1, Args, ModuleInfo).

	% bound_inst_list_merge(Xs, Ys, ModuleInfo0, Zs, ModuleInfo):
	% The two input lists Xs and Ys must already be sorted.
	% Here we perform a sorted merge operation,
	% so that the functors of the output list Zs are the union
	% of the functors of the input lists Xs and Ys.

:- pred bound_inst_list_merge(list(bound_inst), list(bound_inst),
				module_info, list(bound_inst), module_info).
:- mode bound_inst_list_merge(in, in, in, out, out) is semidet.

bound_inst_list_merge(Xs, Ys, ModuleInfo0, Zs, ModuleInfo) :-
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
			inst_list_merge(ArgsX, ArgsY, ModuleInfo0,
					Args, ModuleInfo1),
			Z = functor(ConsIdX, Args),
			Zs = [Z | Zs1],
			bound_inst_list_merge(Xs1, Ys1, ModuleInfo1,
				Zs1, ModuleInfo)
		; compare(<, ConsIdX, ConsIdY) ->
			Zs = [X | Zs1],
			bound_inst_list_merge(Xs1, Ys, ModuleInfo0,
						Zs1, ModuleInfo)
		;
			Zs = [Y | Zs1],
			bound_inst_list_merge(Xs, Ys1, ModuleInfo0,
						Zs1, ModuleInfo)
		)
	).

%-----------------------------------------------------------------------------%

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
		ModuleInfo = ModuleInfo0
	;
			% insert ThisInstPair into the table with value
			% `unknown' 
		map__set(UnifyInsts0, ThisInstPair, unknown, UnifyInsts1),
		inst_table_set_unify_insts(InstTable0, UnifyInsts1, InstTable1),
		module_info_set_insts(ModuleInfo0, InstTable1, ModuleInfo1),
			% unify the insts
		inst_expand(ModuleInfo0, InstA, InstA2),
		inst_expand(ModuleInfo0, InstB, InstB2),
		abstractly_unify_inst_2(Live, InstA2, InstB2, UnifyIsReal,
			ModuleInfo1, Inst0, Det, ModuleInfo2),
			% now update the value associated with ThisInstPair
		module_info_insts(ModuleInfo2, InstTable2),
		inst_table_get_unify_insts(InstTable2, UnifyInsts2),
		map__set(UnifyInsts2, ThisInstPair, known(Inst0, Det),
			UnifyInsts),
		inst_table_set_unify_insts(InstTable2, UnifyInsts, InstTable),
		module_info_set_insts(ModuleInfo2, InstTable, ModuleInfo)
	),
		% avoid expanding recursive insts
	( inst_contains_instname(Inst0, ModuleInfo, ThisInstPair) ->
		Inst = defined_inst(ThisInstPair)
	;		
		Inst = Inst0
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

:- abstractly_unify_inst_3(A, B, C, _, _, _, _, _) when A and B and C.

% XXX could be extended to handle `any' insts better

abstractly_unify_inst_3(live, not_reached, _, _,	M, not_reached, det, M).

abstractly_unify_inst_3(live, any(UniqX), any(UniqY), Real, M,
					any(Uniq), semidet, M) :-
	Real = fake_unify,
	unify_uniq(live, Real, semidet, UniqX, UniqY, Uniq).

abstractly_unify_inst_3(live, any(UniqX), free, Real, M,
					any(Uniq), det, M) :-
	unify_uniq(live, Real, det, UniqX, unique, Uniq).

abstractly_unify_inst_3(live, free, any(UniqY), Real, M,
					any(Uniq), det, M) :-
	unify_uniq(live, Real, det, unique, UniqY, Uniq).

% abstractly_unify_inst_3(live, free,	free, _,	_, _, _, _) :- fail.

abstractly_unify_inst_3(live, free,	bound(UniqY, List0), Real, M0,
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

abstractly_unify_inst_3(live, free,	ground(UniqY, PredInst), Real, M,
					ground(Uniq, PredInst), det, M) :-
	unify_uniq(live, Real, det, unique, UniqY, Uniq).

% abstractly_unify_inst_3(live, free,	abstract_inst(_,_), _, _, _, _) :- fail.

abstractly_unify_inst_3(live,		bound(UniqY, List0), free, Real, M0,
					bound(Uniq, List), det,	 M) :-
	unify_uniq(live, Real, det, unique, UniqY, Uniq),
		% since both are live, we must disallow free-free unifications
	bound_inst_list_is_ground_or_any(List0, M0),
	make_shared_bound_inst_list(List0, M0, List, M).

abstractly_unify_inst_3(live, bound(UniqX, ListX), bound(UniqY, ListY), Real,
			M0,	bound(Uniq, List), Det, M) :-
	abstractly_unify_bound_inst_list(live, ListX, ListY, Real, M0,
		List, Det, M),
	unify_uniq(dead, Real, Det, UniqX, UniqY, Uniq).

abstractly_unify_inst_3(live, bound(UniqX, BoundInsts0), ground(UniqY, _),
		Real, M0, bound(Uniq, BoundInsts), semidet, M) :-
	unify_uniq(dead, Real, semidet, UniqX, UniqY, Uniq),
	make_ground_bound_inst_list(BoundInsts0, live, UniqY, Real, M0,
			BoundInsts, M).

/*** abstract insts not supported
abstractly_unify_inst_3(live, bound(Uniq, List), abstract_inst(_,_), Real, M,
					ground(shared), semidet, M) :-
	unify_uniq(live, Real, semidet, unique, UniqY, Uniq),
	bound_inst_list_is_ground(List, M).
***/

abstractly_unify_inst_3(live, ground(Uniq0, yes(PredInst)), free, Real, M,
				ground(Uniq, yes(PredInst)), det, M) :-
	unify_uniq(live, Real, det, unique, Uniq0, Uniq).

abstractly_unify_inst_3(live, ground(UniqX, yes(_)), bound(UniqY, BoundInsts0),
		Real, M0, bound(Uniq, BoundInsts), semidet, M) :-
	unify_uniq(dead, Real, semidet, UniqX, UniqY, Uniq),
	make_ground_bound_inst_list(BoundInsts0, live, UniqX, Real, M0,
			BoundInsts, M).

abstractly_unify_inst_3(live, ground(UniqA, yes(PredInstA)),
				ground(UniqB, _MaybePredInstB), Real, M,
				ground(Uniq, PredInst), semidet, M) :-
	% It is an error to unify higher-order preds,
	% so if Real \= fake_unify, then we must fail.
	Real = fake_unify,
	% In theory we should choose take the union of the
	% information specified by PredInstA and _MaybePredInstB.
	% However, since our data representation provides no
	% way of doing that, and since this will only happen
	% for fake_unifys, for which it shouldn't make any difference,
	% we just choose the information specified by PredInstA.
	PredInst = yes(PredInstA),
	unify_uniq(live, Real, semidet, UniqA, UniqB, Uniq).

abstractly_unify_inst_3(live, ground(Uniq, no), Inst0, Real, M0,
				Inst, Det, M) :-
	( inst_is_free(M0, Inst0) ->
		Det = det
	;
		Det = semidet
	),
	make_ground_inst(Inst0, live, Uniq, Real, M0, Inst, M).

% abstractly_unify_inst_3(live, abstract_inst(_,_), free,	_, _, _, _, _)
%	:- fail.

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

abstractly_unify_inst_3(dead, any(UniqX), any(UniqY), Real, M,
					any(Uniq), semidet, M) :-
	Real = fake_unify,
	unify_uniq(dead, Real, semidet, UniqX, UniqY, Uniq).

abstractly_unify_inst_3(dead, any(UniqX), free, _Real, M,
					any(UniqX), det, M).

abstractly_unify_inst_3(dead, free, Inst, _, M, Inst, det, M).

abstractly_unify_inst_3(dead, bound(UniqX, List), free, Real, ModuleInfo,
				bound(Uniq, List), det, ModuleInfo) :-
	unify_uniq(dead, Real, det, UniqX, unique, Uniq).

abstractly_unify_inst_3(dead, bound(UniqX, ListX), bound(UniqY, ListY),
			Real, M0, bound(Uniq, List), Det, M) :-
	abstractly_unify_bound_inst_list(dead, ListX, ListY, Real, M0,
		List, Det, M),
	unify_uniq(dead, Real, Det, UniqX, UniqY, Uniq).

abstractly_unify_inst_3(dead, bound(UniqX, BoundInsts0), ground(UniqY, _),
			Real, M0, bound(Uniq, BoundInsts), semidet, M) :-
	unify_uniq(dead, Real, semidet, UniqX, UniqY, Uniq),
	make_ground_bound_inst_list(BoundInsts0, dead, UniqY, Real, M0,
					BoundInsts, M).

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

abstractly_unify_inst_3(dead, ground(Uniq, yes(PredInst)), free, _Real, M,
				ground(Uniq, yes(PredInst)), det, M).

abstractly_unify_inst_3(dead, ground(UniqA, yes(_)), bound(UniqB, BoundInsts0),
			Real, M0, bound(Uniq, BoundInsts), semidet, M) :-
	unify_uniq(dead, Real, semidet, UniqA, UniqB, Uniq),
	make_ground_bound_inst_list(BoundInsts0, dead, UniqA, Real, M0,
					BoundInsts, M).

abstractly_unify_inst_3(dead, ground(UniqA, yes(PredInstA)),
				ground(UniqB, _MaybePredInstB), Real, M,
				ground(Uniq, PredInst), det, M) :-
	Real = fake_unify,
	PredInst = yes(PredInstA),
	unify_uniq(dead, Real, det, UniqA, UniqB, Uniq).

abstractly_unify_inst_3(dead, ground(Uniq, no),	Inst0, Real, M0,
				Inst, Det, M) :-
	( inst_is_free(M0, Inst0) ->
		Det = det
	;
		Det = semidet
	),
	make_ground_inst(Inst0, dead, Uniq, Real, M0, Inst, M).

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

unify_uniq(_,      _, _,       shared,   shared,    	    shared).
unify_uniq(_,      _, _,       shared,   unique,    	    shared).
unify_uniq(_,      _, _,       shared,   mostly_unique,     shared).
unify_uniq(Live,   Real, Det,  shared,   clobbered, 	    clobbered) :-
	allow_unify_with_clobbered(Live, Real, Det).
unify_uniq(Live,   Real, Det,  shared,   mostly_clobbered,  mostly_clobbered) :-
	allow_unify_with_clobbered(Live, Real, Det).

unify_uniq(_,      _, _,       unique,   shared,    	    shared).
unify_uniq(live,   _, _,       unique,   unique,    	    shared).
unify_uniq(live,   _, _,       unique,   mostly_unique,     shared).
unify_uniq(dead,   _, _,       unique,   unique,    	    unique).
unify_uniq(dead,   _, _,       unique,   mostly_unique,     mostly_unique).
		% XXX the above line is a conservative approximation
		% sometimes it should return unique not mostly_unique
unify_uniq(Live,   Real, Det,  unique,   clobbered, 	    clobbered) :-
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

unify_uniq(Live,   Real, Det,  clobbered,	 _,         clobbered) :-
	allow_unify_with_clobbered(Live, Real, Det).

unify_uniq(Live,   Real, Det,  mostly_clobbered, Uniq0,	    Uniq) :-
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
	( Det1 = semidet ->
		Det = semidet
	;
		Det = Det2
	).

%-----------------------------------------------------------------------------%

	% This is the abstract unification operation which
	% unifies a variable (or rather, it's instantiatedness)
	% with a functor.

abstractly_unify_inst_functor(Live, InstA, ConsId, ArgInsts, ArgLives,
		Real, ModuleInfo0, Inst, ModuleInfo) :-
	inst_expand(ModuleInfo0, InstA, InstA2),
	abstractly_unify_inst_functor_2(Live, InstA2, ConsId, ArgInsts,
			ArgLives, Real, ModuleInfo0, Inst, ModuleInfo).

:- pred abstractly_unify_inst_functor_2(is_live, inst, cons_id, list(inst),
			list(is_live), unify_is_real, module_info,
			inst, module_info).
:- mode abstractly_unify_inst_functor_2(in, in, in, in, in, in, in, out, out)
	is semidet.

	% XXX need to handle `any' insts

abstractly_unify_inst_functor_2(live, not_reached, _, _, _, _, M,
			not_reached, M).

abstractly_unify_inst_functor_2(live, free, ConsId, Args0, ArgLives, _Real,
			ModuleInfo0,
			bound(unique, [functor(ConsId, Args)]), ModuleInfo) :-
	inst_list_is_ground_or_any_or_dead(Args0, ArgLives, ModuleInfo0),
	maybe_make_shared_inst_list(Args0, ArgLives, ModuleInfo0,
			Args, ModuleInfo).

abstractly_unify_inst_functor_2(live, bound(Uniq, ListX), ConsId, Args,
				ArgLives, Real, M0, bound(Uniq, List), M) :-
	abstractly_unify_bound_inst_list_lives(ListX, ConsId, Args, ArgLives,
					Real, M0, List, M).

abstractly_unify_inst_functor_2(live, ground(Uniq, _), ConsId, ArgInsts,
		ArgLives, Real, M0, Inst, M) :-
	make_ground_inst_list_lives(ArgInsts, live, ArgLives, Uniq, Real, M0,
		GroundArgInsts, M), 
	Inst = bound(Uniq, [functor(ConsId, GroundArgInsts)]).

% abstractly_unify_inst_functor_2(live, abstract_inst(_,_), _, _, _, _, _, _) :-
% 	fail.

abstractly_unify_inst_functor_2(dead, not_reached, _, _, _, _, M,
					not_reached, M).

abstractly_unify_inst_functor_2(dead, free, ConsId, Args, _ArgLives, _Real, M,
			bound(unique, [functor(ConsId, Args)]), M).

abstractly_unify_inst_functor_2(dead, bound(Uniq, ListX), ConsId, Args,
			_ArgLives, Real, M0, bound(Uniq, List), M) :-
	ListY = [functor(ConsId, Args)],
	abstractly_unify_bound_inst_list(dead, ListX, ListY, Real, M0,
		List, _, M). 

abstractly_unify_inst_functor_2(dead, ground(Uniq, _), ConsId, ArgInsts,
		_ArgLives, Real, M0, Inst, M) :-
	make_ground_inst_list(ArgInsts, dead, Uniq, Real, M0,
		GroundArgInsts, M),
	Inst = bound(Uniq, [functor(ConsId, GroundArgInsts)]).

% abstractly_unify_inst_functor_2(dead, abstract_inst(_,_), _, _, _, _, _, _) :-
% 	fail.

%-----------------------------------------------------------------------------%

:- pred make_ground_inst_list_lives(list(inst), is_live, list(is_live),
				uniqueness, unify_is_real,
				module_info, list(inst), module_info).
:- mode make_ground_inst_list_lives(in, in, in, in, in, in, out, out)
				is semidet.

make_ground_inst_list_lives([], _, _, _, _, ModuleInfo, [], ModuleInfo).
make_ground_inst_list_lives([Inst0 | Insts0], Live, [ArgLive | ArgLives],
		Uniq, Real, ModuleInfo0, [Inst | Insts], ModuleInfo) :-
	( Live = live, ArgLive = live ->
		BothLive = live
	;
		BothLive = dead
	),
	make_ground_inst(Inst0, BothLive, Uniq, Real, ModuleInfo0,
		Inst, ModuleInfo1),
	make_ground_inst_list_lives(Insts0, Live, ArgLives, Uniq, Real,
		ModuleInfo1, Insts, ModuleInfo).

:- pred make_ground_inst_list(list(inst), is_live, uniqueness, unify_is_real,
				module_info, list(inst), module_info).
:- mode make_ground_inst_list(in, in, in, in, in, out, out) is semidet.

make_ground_inst_list([], _, _, _, ModuleInfo, [], ModuleInfo).
make_ground_inst_list([Inst0 | Insts0], Live, Uniq, Real, ModuleInfo0,
		[Inst | Insts], ModuleInfo) :-
	make_ground_inst(Inst0, Live, Uniq, Real, ModuleInfo0,
		Inst, ModuleInfo1),
	make_ground_inst_list(Insts0, Live, Uniq, Real, ModuleInfo1,
		Insts, ModuleInfo).

% abstractly unify an inst with `ground' and calculate the new inst
% and the determinism of the unification.

:- pred make_ground_inst(inst, is_live, uniqueness, unify_is_real, module_info,
				inst, module_info).
:- mode make_ground_inst(in, in, in, in, in, out, out) is semidet.

make_ground_inst(not_reached, _, _, _, M, not_reached, M).
make_ground_inst(any(Uniq0), IsLive, Uniq1, Real, M, ground(Uniq, no), M) :-
	unify_uniq(IsLive, Real, semidet, Uniq0, Uniq1, Uniq).
make_ground_inst(free, IsLive, Uniq0, Real, M, ground(Uniq, no), M) :-
	unify_uniq(IsLive, Real, det, unique, Uniq0, Uniq).
make_ground_inst(free(T), IsLive, Uniq0, Real, M,
		defined_inst(typed_ground(Uniq, T)), M) :-
	unify_uniq(IsLive, Real, det, unique, Uniq0, Uniq).
make_ground_inst(bound(Uniq0, BoundInsts0), IsLive, Uniq1, Real, M0,
		bound(Uniq, BoundInsts), M) :-
	unify_uniq(dead, Real, semidet, Uniq0, Uniq1, Uniq),
	make_ground_bound_inst_list(BoundInsts0, IsLive, Uniq1, Real, M0,
					BoundInsts, M).
make_ground_inst(ground(Uniq0, _PredInst), _IsLive, Uniq1, Real, M,
		ground(Uniq, no), M) :-
	unify_uniq(dead, Real, semidet, Uniq0, Uniq1, Uniq).
make_ground_inst(inst_var(_), _, _, _, _, _, _) :-
	error("free inst var").
make_ground_inst(abstract_inst(_,_), _, _, _, M, ground(shared, no), M).
make_ground_inst(defined_inst(InstName), IsLive, Uniq, Real, ModuleInfo0,
			Inst, ModuleInfo) :-
		% check whether the inst name is already in the
		% ground_inst table
	module_info_insts(ModuleInfo0, InstTable0),
	inst_table_get_ground_insts(InstTable0, GroundInsts0),
	GroundInstKey = ground_inst(InstName, IsLive, Uniq, Real),
	(
		map__search(GroundInsts0, GroundInstKey, Result)
	->
		( Result = known(GroundInst0) ->
			GroundInst = GroundInst0
		;
			GroundInst = defined_inst(GroundInstKey)
		),
		ModuleInfo = ModuleInfo0
	;
		% insert the inst name in the ground_inst table, with
		% value `unknown' for the moment
		map__set(GroundInsts0, GroundInstKey, unknown, GroundInsts1),
		inst_table_set_ground_insts(InstTable0, GroundInsts1,
			InstTable1),
		module_info_set_insts(ModuleInfo0, InstTable1, ModuleInfo1),

		% expand the inst name, and invoke ourself recursively on
		% it's expansion
		inst_lookup(ModuleInfo1, InstName, Inst0),
		inst_expand(ModuleInfo1, Inst0, Inst1),
		make_ground_inst(Inst1, IsLive, Uniq, Real, ModuleInfo1,
				GroundInst, ModuleInfo2),

		% now that we have determined the resulting Inst, store
		% the appropriate value `known(GroundInst)' in the ground_inst
		% table
		module_info_insts(ModuleInfo2, InstTable2),
		inst_table_get_ground_insts(InstTable2, GroundInsts2),
		map__set(GroundInsts2, GroundInstKey, known(GroundInst),
			GroundInsts),
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
		unify_is_real, module_info, list(bound_inst), module_info).
:- mode make_ground_bound_inst_list(in, in, in, in, in, out, out) is semidet.

make_ground_bound_inst_list([], _, _, _, ModuleInfo, [], ModuleInfo).
make_ground_bound_inst_list([Bound0 | Bounds0], IsLive, Uniq, Real, ModuleInfo0,
			[Bound | Bounds], ModuleInfo) :-
	Bound0 = functor(ConsId, ArgInsts0),
	make_ground_inst_list(ArgInsts0, IsLive, Uniq, Real, ModuleInfo0,
				ArgInsts, ModuleInfo1),
	Bound = functor(ConsId, ArgInsts),
	make_ground_bound_inst_list(Bounds0, IsLive, Uniq, Real, ModuleInfo1,
				Bounds, ModuleInfo).

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

:- pred make_shared_inst_list(list(inst), module_info,
				list(inst), module_info).
:- mode make_shared_inst_list(in, in, out, out) is det.

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
		map__set(SharedInsts0, InstName, unknown, SharedInsts1),
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
		map__set(SharedInsts2, InstName, known(SharedInst),
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
		map__set(NondetLiveInsts0, InstName, unknown, NondetLiveInsts1),
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
		map__set(NondetLiveInsts2, InstName, known(NondetLiveInst),
			NondetLiveInsts),
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

	% Given a list of insts, and a corresponding list of livenesses,
	% return true iff for every element in the list of insts, either
	% the elemement is ground or the corresponding element in the liveness
	% list is dead.

:- pred inst_list_is_ground_or_dead(list(inst), list(is_live), module_info).
:- mode inst_list_is_ground_or_dead(in, in, in) is semidet.

inst_list_is_ground_or_dead([], [], _).
inst_list_is_ground_or_dead([Inst | Insts], [Live | Lives], ModuleInfo) :-
	( Live = live ->
		inst_is_ground(ModuleInfo, Inst)
	;
		true
	),
	inst_list_is_ground_or_dead(Insts, Lives, ModuleInfo).

	% Given a list of insts, and a corresponding list of livenesses,
	% return true iff for every element in the list of insts, either
	% the elemement is ground or any, or the corresponding element
	% in the liveness list is dead.

:- pred inst_list_is_ground_or_any_or_dead(list(inst), list(is_live),
					module_info).
:- mode inst_list_is_ground_or_any_or_dead(in, in, in) is semidet.

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

:- abstractly_unify_bound_inst_list(_, Xs, Ys, _, _, _, _, _)
	when Xs and Ys. % Index

abstractly_unify_bound_inst_list(_, [], [], _, ModuleInfo, [], det, ModuleInfo).
abstractly_unify_bound_inst_list(_, [], [_|_], _, M, [], semidet, M).
abstractly_unify_bound_inst_list(_, [_|_], [], _, M, [], semidet, M).
abstractly_unify_bound_inst_list(Live, [X|Xs], [Y|Ys], Real, ModuleInfo0,
		L, Det, ModuleInfo) :-
	X = functor(ConsIdX, ArgsX),
	Y = functor(ConsIdY, ArgsY),
	( ConsIdX = ConsIdY ->
	    	abstractly_unify_inst_list(ArgsX, ArgsY, Live, Real,
			ModuleInfo0, Args, Det1, ModuleInfo1),
		L = [functor(ConsIdX, Args) | L1],
		abstractly_unify_bound_inst_list(Live, Xs, Ys, Real,
					ModuleInfo1, L1, Det2, ModuleInfo),
		( Det1 = semidet ->
		    Det = semidet
		;
		    Det = Det2
		)
	;
		Det = semidet,
		( compare(<, ConsIdX, ConsIdY) ->
			abstractly_unify_bound_inst_list(Live, Xs, [Y|Ys],
				Real, ModuleInfo0, L, _, ModuleInfo)
		;
			abstractly_unify_bound_inst_list(Live, [X|Xs], Ys,
				Real, ModuleInfo0, L, _, ModuleInfo)
		)
	).

:- pred abstractly_unify_bound_inst_list_lives(list(bound_inst), cons_id,
	list(inst), list(is_live), unify_is_real, module_info,
	list(bound_inst), module_info).
:- mode abstractly_unify_bound_inst_list_lives(in, in, in, in, in, in, out, out)
	is semidet.

abstractly_unify_bound_inst_list_lives([], _, _, _, _, ModuleInfo,
					[], ModuleInfo).
abstractly_unify_bound_inst_list_lives([X|Xs], ConsIdY, ArgsY, LivesY, Real,
		ModuleInfo0, L, ModuleInfo) :-
	X = functor(ConsIdX, ArgsX),
	( 
		ConsIdX = ConsIdY
	->
		abstractly_unify_inst_list_lives(ArgsX, ArgsY, LivesY, Real,
			ModuleInfo0, Args, ModuleInfo),
		L = [functor(ConsIdX, Args)]
	;
		abstractly_unify_bound_inst_list_lives(Xs, ConsIdY, ArgsY, 
				LivesY, Real, ModuleInfo0, L, ModuleInfo)
	).

:- pred abstractly_unify_inst_list_lives(list(inst), list(inst), list(is_live),
			unify_is_real, module_info, list(inst), module_info).
:- mode abstractly_unify_inst_list_lives(in, in, in, in, in, out, out)
	is semidet.

abstractly_unify_inst_list_lives([], [], [], _, ModuleInfo, [], ModuleInfo).
abstractly_unify_inst_list_lives([X|Xs], [Y|Ys], [Live|Lives], Real,
		ModuleInfo0, [Z|Zs], ModuleInfo) :-
	abstractly_unify_inst(Live, X, Y, Real, ModuleInfo0,
			Z, _Det, ModuleInfo1),
	abstractly_unify_inst_list_lives(Xs, Ys, Lives, Real, ModuleInfo1,
			Zs, ModuleInfo).

%-----------------------------------------------------------------------------%

	% Succeed iff the specified inst contains (directly or indirectly)
	% the specified inst_name.

:- pred inst_contains_instname(inst, module_info, inst_name).
:- mode inst_contains_instname(in, in, in) is semidet.

inst_contains_instname(Inst, ModuleInfo, InstName) :-
	set__init(Expansions),
	inst_contains_instname_2(Inst, ModuleInfo, Expansions, InstName).

:- pred inst_contains_instname_2(inst, module_info, set(inst_name), inst_name).
:- mode inst_contains_instname_2(in, in, in, in) is semidet.

inst_contains_instname_2(defined_inst(InstName1), ModuleInfo, Expansions0,
		InstName) :-
	(
		InstName = InstName1
	;
		\+ set__member(InstName1, Expansions0),
		inst_lookup(ModuleInfo, InstName1, Inst1),
		set__insert(Expansions0, InstName1, Expansions),
		inst_contains_instname_2(Inst1, ModuleInfo, Expansions,
			InstName)
	).
inst_contains_instname_2(bound(_Uniq, ArgInsts), ModuleInfo, Expansions,
		InstName) :-
	bound_inst_list_contains_instname(ArgInsts, ModuleInfo, Expansions,
		InstName).

:- pred bound_inst_list_contains_instname(list(bound_inst), module_info,
						set(inst_name), inst_name).
:- mode bound_inst_list_contains_instname(in, in, in, in) is semidet.

bound_inst_list_contains_instname([BoundInst|BoundInsts], ModuleInfo,
		Expansions, InstName) :-
	BoundInst = functor(_Functor, ArgInsts),
	(
		inst_list_contains_instname(ArgInsts, ModuleInfo, Expansions,
			InstName)
	;
		bound_inst_list_contains_instname(BoundInsts, ModuleInfo,
			Expansions, InstName)
	).

:- pred inst_list_contains_instname(list(inst), module_info, set(inst_name),
					inst_name).
:- mode inst_list_contains_instname(in, in, in, in) is semidet.

inst_list_contains_instname([Inst|Insts], ModuleInfo, Expansions, InstName) :-
	(
		inst_contains_instname_2(Inst, ModuleInfo, Expansions, InstName)
	;
		inst_list_contains_instname(Insts, ModuleInfo, Expansions,
			InstName)
	).

%-----------------------------------------------------------------------------%
