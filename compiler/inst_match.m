%-----------------------------------------------------------------------------%
% Copyright (C) 1995-1998 The University of Melbourne.
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

:- import_module hlds_module, hlds_data, prog_data, (inst).
:- import_module list, term.

%-----------------------------------------------------------------------------%

:- pred inst_expand(inst_table, module_info, inst, inst).
:- mode inst_expand(in, in, in, out) is det.

	% inst_expand(InstTable, ModuleInfo, Inst0, Inst) checks if the
	% top-level % part of the inst is a defined inst or an alias, and
	% if so replaces it with the definition.

:- pred inst_expand_defined_inst(inst_table, module_info, inst, inst).
:- mode inst_expand_defined_inst(in, in, in, out) is det.

	% inst_expand_defined_inst(InstTable, ModuleInfo, Inst0, Inst) checks
	% if the top-level part of the inst is a defined inst, and if so
	% replaces it with the definition.

%-----------------------------------------------------------------------------%

:- pred inst_matches_initial(inst, inst, inst_table, module_info).
:- mode inst_matches_initial(in, in, in, in) is semidet.

:- pred inst_matches_final(inst, inst, inst_table, module_info).
:- mode inst_matches_final(in, in, in, in) is semidet.

	% inst_matches_initial(InstA, InstB, InstTable, ModuleInfo):
	%	Succeed iff `InstA' specifies at least as much
	%	information as `InstB', and in those parts where they
	%	specify the same information, `InstA' is at least as
	%	instantiated as `InstB'.
	%	Thus, inst_matches_initial(not_reached, ground, _)
	%	succeeds, since not_reached contains more information
	%	than ground - but not vice versa.  Similarly,
	%	inst_matches_initial(bound(a), bound(a;b), _) should
	%	succeed, but not vice versa.

	% inst_matches_final(InstA, InstB, InstTable, ModuleInfo):
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

:- pred inst_matches_binding(inst, inst, inst_table, module_info).
:- mode inst_matches_binding(in, in, in, in) is semidet.

	% inst_matches_binding(InstA, InstB, InstTable, ModuleInfo):
	%	 Succeed iff the binding of InstA is definitely exactly the
	%	 same as that of InstB.  This is the same as
	%	 inst_matches_final except that it ignores uniqueness, and
	%	 that `any' does not match itself.  It is used to check
	%	 whether variables get bound in negated contexts.

%-----------------------------------------------------------------------------%

	% pred_inst_matches(PredInstA, PredInstB, InstTable, ModuleInfo)
	% 	Succeeds if PredInstA specifies a pred that can
	%	be used wherever and whenever PredInstB could be used.
	%	This is true if they both have the same PredOrFunc indicator
	%	and the same determinism, and if the arguments match
	%	using pred_inst_argmodes_match.
	%
:- pred pred_inst_matches(pred_inst_info, pred_inst_info, inst_table,
		module_info).
:- mode pred_inst_matches(in, in, in, in) is semidet.

%-----------------------------------------------------------------------------%

/*
** Predicates to test various properties of insts.
** Note that `not_reached' insts are considered to satisfy
** all of these predicates except inst_is_clobbered.
*/

        % succeed if the inst is fully ground (i.e. contains only
        % `ground', `bound', and `not_reached' insts, with no `free'
        % or `any' insts).
:- pred inst_is_ground(inst, inst_table, module_info).
:- mode inst_is_ground(in, in, in) is semidet.

        % succeed if the inst is not partly free (i.e. contains only
        % `any', `ground', `bound', and `not_reached' insts, with no
        % `free' insts).
:- pred inst_is_ground_or_any(inst, inst_table, module_info).
:- mode inst_is_ground_or_any(in, in, in) is semidet.

	% succeed if the inst is fully ground and higher order
	% (i.e. contains a pred_inst_info.
:- pred inst_is_higher_order_ground(inst, inst_table, module_info).
:- mode inst_is_higher_order_ground(in, in, in) is semidet.

        % succeed if the inst is `mostly_unique' or `unique'
:- pred inst_is_mostly_unique(inst, inst_table, module_info).
:- mode inst_is_mostly_unique(in, in, in) is semidet.

        % succeed if the inst is `unique'
:- pred inst_is_unique(inst, inst_table, module_info).
:- mode inst_is_unique(in, in, in) is semidet.

        % succeed if the inst is not `mostly_unique' or `unique'
:- pred inst_is_not_partly_unique(inst, inst_table, module_info).
:- mode inst_is_not_partly_unique(in, in, in) is semidet.

        % succeed if the inst is not `unique'
:- pred inst_is_not_fully_unique(inst, inst_table, module_info).
:- mode inst_is_not_fully_unique(in, in, in) is semidet.

:- pred inst_is_clobbered(inst, inst_table, module_info).
:- mode inst_is_clobbered(in, in, in) is semidet.

:- pred inst_list_is_ground(list(inst), inst_table, module_info).
:- mode inst_list_is_ground(in, in, in) is semidet.

:- pred inst_list_is_ground_or_any(list(inst), inst_table, module_info).
:- mode inst_list_is_ground_or_any(in, in, in) is semidet.

:- pred inst_list_is_unique(list(inst), inst_table, module_info).
:- mode inst_list_is_unique(in, in, in) is semidet.

:- pred inst_list_is_mostly_unique(list(inst), inst_table, module_info).
:- mode inst_list_is_mostly_unique(in, in, in) is semidet.

:- pred inst_list_is_not_partly_unique(list(inst), inst_table, module_info).
:- mode inst_list_is_not_partly_unique(in, in, in) is semidet.

:- pred inst_list_is_not_fully_unique(list(inst), inst_table, module_info).
:- mode inst_list_is_not_fully_unique(in, in, in) is semidet.

:- pred bound_inst_list_is_ground(list(bound_inst), inst_table, module_info).
:- mode bound_inst_list_is_ground(in, in, in) is semidet.

:- pred bound_inst_list_is_ground_or_any(list(bound_inst), inst_table,
		module_info).
:- mode bound_inst_list_is_ground_or_any(in, in, in) is semidet.

:- pred bound_inst_list_is_unique(list(bound_inst), inst_table, module_info).
:- mode bound_inst_list_is_unique(in, in, in) is semidet.

:- pred bound_inst_list_is_mostly_unique(list(bound_inst), inst_table,
		module_info).
:- mode bound_inst_list_is_mostly_unique(in, in, in) is semidet.

:- pred bound_inst_list_is_not_partly_unique(list(bound_inst), inst_table,
		module_info).
:- mode bound_inst_list_is_not_partly_unique(in, in, in) is semidet.

:- pred bound_inst_list_is_not_fully_unique(list(bound_inst), inst_table,
		module_info).
:- mode bound_inst_list_is_not_fully_unique(in, in, in) is semidet.

:- pred inst_is_free(inst, inst_table, module_info).
:- mode inst_is_free(in, in, in) is semidet.

:- pred inst_is_free_alias(inst, inst_table, module_info).
:- mode inst_is_free_alias(in, in, in) is semidet.

:- pred inst_contains_free_alias(inst, inst_table, module_info).
:- mode inst_contains_free_alias(in, in, in) is semidet.

:- pred inst_list_is_free(list(inst), inst_table, module_info).
:- mode inst_list_is_free(in, in, in) is semidet.

:- pred bound_inst_list_is_free(list(bound_inst), inst_table, module_info).
:- mode bound_inst_list_is_free(in, in, in) is semidet.

:- pred inst_is_bound(inst, inst_table, module_info).
:- mode inst_is_bound(in, in, in) is semidet.

:- pred inst_is_bound_to_functors(inst, inst_table, module_info,
		list(bound_inst)).
:- mode inst_is_bound_to_functors(in, in, in, out) is semidet.

%-----------------------------------------------------------------------------%

	% Succeed iff the specified inst contains (directly or indirectly)
	% the specified inst_name.

:- pred inst_contains_instname(inst, inst_table, module_info, inst_name).
:- mode inst_contains_instname(in, in, in, in) is semidet.

	% Nondeterministically produce all the inst_vars contained
	% in the specified list of modes.

:- type inst_var == var.
:- pred mode_list_contains_inst_var(list(mode), inst_table, module_info,
		inst_var).
:- mode mode_list_contains_inst_var(in, in, in, out) is nondet.

	% Given a list of insts, and a corresponding list of livenesses,
	% return true iff for every element in the list of insts, either
	% the elemement is ground or the corresponding element in the liveness
	% list is dead.

:- pred inst_list_is_ground_or_dead(list(inst), list(is_live),
		inst_table, module_info).
:- mode inst_list_is_ground_or_dead(in, in, in, in) is semidet.

	% Given a list of insts, and a corresponding list of livenesses,
	% return true iff for every element in the list of insts, either
	% the elemement is ground or any, or the corresponding element
	% in the liveness list is dead.

:- pred inst_list_is_ground_or_any_or_dead(list(inst), list(is_live),
		inst_table, module_info).
:- mode inst_list_is_ground_or_any_or_dead(in, in, in, in) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module hlds_data, mode_util, prog_data, inst_util.
:- import_module list, set, map, term, std_util, require.

inst_matches_initial(InstA, InstB, InstTable, ModuleInfo) :-
	set__init(Expansions),
	inst_matches_initial_2(InstA, InstB, InstTable, ModuleInfo, Expansions).

:- type expansions == set(pair(inst)).

:- pred inst_matches_initial_2(inst, inst, inst_table, module_info, expansions).
:- mode inst_matches_initial_2(in, in, in, in, in) is semidet.

inst_matches_initial_2(InstA, InstB, InstTable, ModuleInfo, Expansions) :-
	ThisExpansion = InstA - InstB,
	( set__member(ThisExpansion, Expansions) ->
		true
/********* 
		% does this test improve efficiency??
	; InstA = InstB ->
		true
**********/
	;
		inst_expand(InstTable, ModuleInfo, InstA, InstA2),
		inst_expand(InstTable, ModuleInfo, InstB, InstB2),
		set__insert(Expansions, ThisExpansion, Expansions2),
		inst_matches_initial_3(InstA2, InstB2, InstTable, ModuleInfo,
			Expansions2)
	).

:- pred inst_matches_initial_3(inst, inst, inst_table, module_info,
		expansions).
:- mode inst_matches_initial_3(in, in, in, in, in) is semidet.

	% To avoid infinite regress, we assume that
	% inst_matches_initial is true for any pairs of insts which
	% occur in `Expansions'.

inst_matches_initial_3(any(UniqA), any(UniqB), _, _, _) :-
	unique_matches_initial(UniqA, UniqB).
inst_matches_initial_3(any(_), free(unique), _, _, _).
inst_matches_initial_3(free(unique), any(_), _, _, _).
inst_matches_initial_3(free(alias), free(alias), _, _, _).
			% AAA free(alias) should match_initial free(unique)
			% and vice-versa.  They will as soon as the mode
			% checker supports the implied modes that would result.
inst_matches_initial_3(free(unique), free(unique), _, _, _).
inst_matches_initial_3(bound(UniqA, ListA), any(UniqB), InstTable, ModuleInfo,
		_) :-
	unique_matches_initial(UniqA, UniqB),
	bound_inst_list_matches_uniq(ListA, UniqB, InstTable, ModuleInfo).
inst_matches_initial_3(bound(_Uniq, _List), free(_), _, _, _).
inst_matches_initial_3(bound(UniqA, ListA), bound(UniqB, ListB), 
			InstTable, ModuleInfo, Expansions) :-
	unique_matches_initial(UniqA, UniqB),
	bound_inst_list_matches_initial(ListA, ListB, InstTable, ModuleInfo,
			Expansions).
inst_matches_initial_3(bound(UniqA, ListA), ground(UniqB, no), InstTable, ModuleInfo,
		_) :-
	unique_matches_initial(UniqA, UniqB),
	bound_inst_list_is_ground(ListA, InstTable, ModuleInfo),
	bound_inst_list_matches_uniq(ListA, UniqB, InstTable, ModuleInfo).
inst_matches_initial_3(bound(Uniq, List), abstract_inst(_,_), InstTable, ModuleInfo,
		_) :-
	Uniq = unique,
	bound_inst_list_is_ground(List, InstTable, ModuleInfo),
	bound_inst_list_is_unique(List, InstTable, ModuleInfo).
inst_matches_initial_3(bound(Uniq, List), abstract_inst(_,_), InstTable, ModuleInfo,
		_) :-
	Uniq = mostly_unique,
	bound_inst_list_is_ground(List, InstTable, ModuleInfo),
	bound_inst_list_is_mostly_unique(List, InstTable, ModuleInfo).
inst_matches_initial_3(ground(UniqA, _PredInst), any(UniqB), _, _, _) :-
	unique_matches_initial(UniqA, UniqB).
inst_matches_initial_3(ground(_Uniq, _PredInst), free(_), _, _, _).
inst_matches_initial_3(ground(UniqA, _), bound(UniqB, List), InstTable,
		ModuleInfo, _) :-
	unique_matches_initial(UniqA, UniqB),
	uniq_matches_bound_inst_list(UniqA, List, InstTable, ModuleInfo),
	fail.	% XXX BUG! should fail only if 
		% List does not include all the constructors for the type,
		% or if List contains some not_reached insts.
		% Should succeed if List contains all the constructors
		% for the type.  Problem is we don't know what the type was :-(
inst_matches_initial_3(ground(UniqA, PredInstA), ground(UniqB, PredInstB),
		InstTable, ModuleInfo, _) :-
	maybe_pred_inst_matches_initial(PredInstA, PredInstB, InstTable, ModuleInfo),
	unique_matches_initial(UniqA, UniqB).
inst_matches_initial_3(ground(_UniqA, no), abstract_inst(_,_), _, _, _) :-
		% I don't know what this should do.
		% Abstract insts aren't really supported.
	error("inst_matches_initial(ground, abstract_inst) == ??").
inst_matches_initial_3(abstract_inst(_,_), any(shared), _, _, _).
inst_matches_initial_3(abstract_inst(_,_), free(_), _, _, _).
inst_matches_initial_3(abstract_inst(Name, ArgsA), abstract_inst(Name, ArgsB),
				InstTable, ModuleInfo, Expansions) :-
	inst_list_matches_initial(ArgsA, ArgsB, InstTable, ModuleInfo,
				Expansions).
inst_matches_initial_3(not_reached, _, _, _, _).

%-----------------------------------------------------------------------------%

:- pred maybe_pred_inst_matches_initial(maybe(pred_inst_info),
		maybe(pred_inst_info), inst_table, module_info).
:- mode maybe_pred_inst_matches_initial(in, in, in, in) is semidet.

maybe_pred_inst_matches_initial(no, no, _, _).
maybe_pred_inst_matches_initial(yes(_), no, _, _).
maybe_pred_inst_matches_initial(yes(PredInstA), yes(PredInstB), InstTable,
		ModuleInfo) :-
	pred_inst_matches(PredInstA, PredInstB, InstTable, ModuleInfo).

pred_inst_matches(PredInstA, PredInstB, InstTable, ModuleInfo) :-
	set__init(Expansions),
	pred_inst_matches_2(PredInstA, PredInstB, InstTable, ModuleInfo, Expansions).

	% pred_inst_matches_2(PredInstA, PredInstB, InstTable, ModuleInfo,
	%		Expansions)
	%	Same as pred_inst_matches/4, except that inst pairs in
	%	Expansions are assumed to match_final each other.
	%	(This avoids infinite loops when calling inst_matches_final
	%	on higher-order recursive insts.)
	%
:- pred pred_inst_matches_2(pred_inst_info, pred_inst_info, inst_table,
			module_info, expansions).
:- mode pred_inst_matches_2(in, in, in, in, in) is semidet.

pred_inst_matches_2(pred_inst_info(PredOrFunc, ArgModesA, Det),
		pred_inst_info(PredOrFunc, ArgModesB, Det),
		InstTable, ModuleInfo, Expansions) :-
	ArgModesA = argument_modes(_, ModesA),
	ArgModesB = argument_modes(_, ModesB),
	% XXX This is incorrect in the case where the pred insts have
	%     aliasing in their argument_modes.
	pred_inst_argmodes_matches(ModesA, ModesB, InstTable, ModuleInfo, 
		Expansions).

	% pred_inst_matches_argmodes(ModesA, ModesB, ModuleInfo, Expansions):
	% succeeds if the initial insts of ModesB specify at least as
	% much information as, and the same binding as, the initial
	% insts of ModesA; and the final insts of ModesA specify at
	% least as much information as, and the same binding as, the
	% final insts of ModesB.  Any inst pairs in Expansions are assumed
	% to match_final each other.
	%
:- pred pred_inst_argmodes_matches(list(mode), list(mode),
				inst_table, module_info, expansions).
:- mode pred_inst_argmodes_matches(in, in, in, in, in) is semidet.

pred_inst_argmodes_matches([], [], _, _, _).
pred_inst_argmodes_matches([ModeA|ModeAs], [ModeB|ModeBs],
		InstTable, ModuleInfo, Expansions) :-
	mode_get_insts(ModuleInfo, ModeA, InitialA, FinalA),
	mode_get_insts(ModuleInfo, ModeB, InitialB, FinalB),
	inst_matches_final_2(InitialB, InitialA, InstTable, ModuleInfo, Expansions),
	inst_matches_final_2(FinalA, FinalB, InstTable, ModuleInfo, Expansions),
	pred_inst_argmodes_matches(ModeAs, ModeBs, InstTable, ModuleInfo, Expansions).

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
					inst_table, module_info).
:- mode bound_inst_list_matches_uniq(in, in, in, in) is semidet.

bound_inst_list_matches_uniq(List, Uniq, InstTable, ModuleInfo) :-
	( Uniq = unique ->
		bound_inst_list_is_unique(List, InstTable, ModuleInfo)
	; Uniq = mostly_unique ->
		bound_inst_list_is_mostly_unique(List, InstTable, ModuleInfo)
	;
		true
	).

:- pred uniq_matches_bound_inst_list(uniqueness, list(bound_inst),
					inst_table, module_info).
:- mode uniq_matches_bound_inst_list(in, in, in, in) is semidet.

uniq_matches_bound_inst_list(Uniq, List, InstTable, ModuleInfo) :-
	( Uniq = shared ->
		bound_inst_list_is_not_partly_unique(List, InstTable, ModuleInfo)
	; Uniq = mostly_unique ->
		bound_inst_list_is_not_fully_unique(List, InstTable, ModuleInfo)
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
				inst_table, module_info, expansions).
:- mode bound_inst_list_matches_initial(in, in, in, in, in) is semidet.

bound_inst_list_matches_initial([], _, _, _, _).
bound_inst_list_matches_initial([X|Xs], [Y|Ys], InstTable, ModuleInfo, Expansions) :-
	X = functor(ConsIdX, ArgsX),
	Y = functor(ConsIdY, ArgsY),
	( ConsIdX = ConsIdY ->
		inst_list_matches_initial(ArgsX, ArgsY, InstTable, ModuleInfo,
			Expansions),
		bound_inst_list_matches_initial(Xs, Ys, InstTable, ModuleInfo,
			Expansions)
	;
		compare(>, ConsIdX, ConsIdY),
			% ConsIdY does not occur in [X|Xs].
			% Hence [X|Xs] implicitly specifies `not_reached'
			% for the args of ConsIdY, and hence 
			% automatically matches_initial Y.  We just need to
			% check that [X|Xs] matches_initial Ys.
		bound_inst_list_matches_initial([X|Xs], Ys, 
					InstTable, ModuleInfo, Expansions)
	).

:- pred inst_list_matches_initial(list(inst), list(inst), 
				inst_table, module_info, expansions).
:- mode inst_list_matches_initial(in, in, in, in, in) is semidet.

inst_list_matches_initial([], [], _, _, _).
inst_list_matches_initial([X|Xs], [Y|Ys], InstTable, ModuleInfo, Expansions) :-
	inst_matches_initial_2(X, Y, InstTable, ModuleInfo, Expansions),
	inst_list_matches_initial(Xs, Ys, InstTable, ModuleInfo, Expansions).

%-----------------------------------------------------------------------------%

inst_expand(InstTable, ModuleInfo, Inst0, Inst) :-
	( Inst0 = defined_inst(InstName) ->
		inst_lookup(InstTable, ModuleInfo, InstName, Inst1),
		inst_expand(InstTable, ModuleInfo, Inst1, Inst)
	; Inst0 = alias(InstKey) ->
		inst_table_get_inst_key_table(InstTable, IKT),
		inst_key_table_lookup(IKT, InstKey, Inst1),
		inst_expand(InstTable, ModuleInfo, Inst1, Inst)
	;
		Inst = Inst0
	).

inst_expand_defined_inst(InstTable, ModuleInfo, Inst0, Inst) :-
	( Inst0 = defined_inst(InstName) ->
		inst_lookup(InstTable, ModuleInfo, InstName, Inst1),
		inst_expand_defined_inst(InstTable, ModuleInfo, Inst1, Inst)
	;
		Inst = Inst0
	).

%-----------------------------------------------------------------------------%

inst_matches_final(InstA, InstB, InstTable, ModuleInfo) :-
	set__init(Expansions),
	inst_matches_final_2(InstA, InstB, InstTable, ModuleInfo, Expansions).

:- pred inst_matches_final_2(inst, inst, inst_table, module_info,
		expansions).
:- mode inst_matches_final_2(in, in, in, in, in) is semidet.

inst_matches_final_2(InstA, InstB, InstTable, ModuleInfo, Expansions) :-
	ThisExpansion = InstA - InstB,
	( set__member(ThisExpansion, Expansions) ->
		true
	; InstA = InstB ->
		true
	;
		inst_expand(InstTable, ModuleInfo, InstA, InstA2),
		inst_expand(InstTable, ModuleInfo, InstB, InstB2),
		set__insert(Expansions, ThisExpansion, Expansions2),
		inst_matches_final_3(InstA2, InstB2, InstTable, ModuleInfo,
			Expansions2)
	).

:- pred inst_matches_final_3(inst, inst, inst_table, module_info,
		expansions).
:- mode inst_matches_final_3(in, in, in, in, in) is semidet.

inst_matches_final_3(any(UniqA), any(UniqB), _, _, _) :-
	unique_matches_final(UniqA, UniqB).
inst_matches_final_3(free(unique), any(Uniq), _, _, _) :-
	% We do not yet allow `free' to match `any',
	% unless the `any' is `clobbered_any' or `mostly_clobbered_any'.
	% Among other things, changing this would break compare_inst
	% in modecheck_call.m.
	( Uniq = clobbered ; Uniq = mostly_clobbered ).
inst_matches_final_3(free(Aliasing), free(Aliasing), _, _, _).
inst_matches_final_3(bound(UniqA, ListA), any(UniqB), InstTable, ModuleInfo,
		_) :-
	unique_matches_final(UniqA, UniqB),
	bound_inst_list_matches_uniq(ListA, UniqB, InstTable, ModuleInfo),
	% We do not yet allow `free' to match `any'.
	% Among other things, changing this would break compare_inst
	% in modecheck_call.m.
	bound_inst_list_is_ground_or_any(ListA, InstTable, ModuleInfo).
inst_matches_final_3(bound(UniqA, ListA), bound(UniqB, ListB),
		InstTable, ModuleInfo, Expansions) :-
	unique_matches_final(UniqA, UniqB),
	bound_inst_list_matches_final(ListA, ListB, InstTable, ModuleInfo,
			Expansions).
inst_matches_final_3(bound(UniqA, ListA), ground(UniqB, no), InstTable,
			ModuleInfo, _Exps) :-
	unique_matches_final(UniqA, UniqB),
	bound_inst_list_is_ground(ListA, InstTable, ModuleInfo),
	bound_inst_list_matches_uniq(ListA, UniqB, InstTable, ModuleInfo).
inst_matches_final_3(ground(UniqA, _), any(UniqB), _InstTable, _ModuleInfo,
		_Expansions) :-
	unique_matches_final(UniqA, UniqB).
inst_matches_final_3(ground(UniqA, _), bound(UniqB, ListB), InstTable,
		ModuleInfo, _Exps) :-
	unique_matches_final(UniqA, UniqB),
	uniq_matches_bound_inst_list(UniqA, ListB, InstTable, ModuleInfo).
		% XXX BUG! Should fail if there are not_reached
		% insts in ListB, or if ListB does not contain a complete list
		% of all the constructors for the type in question.
	%%% error("not implemented: `ground' matches_final `bound(...)'").
inst_matches_final_3(ground(UniqA, PredInstA), ground(UniqB, PredInstB),
		InstTable, ModuleInfo, Expansions) :-
	maybe_pred_inst_matches_final(PredInstA, PredInstB,
		InstTable, ModuleInfo, Expansions),
	unique_matches_final(UniqA, UniqB).
inst_matches_final_3(abstract_inst(_, _), any(shared), _, _, _).
inst_matches_final_3(abstract_inst(Name, ArgsA), abstract_inst(Name, ArgsB),
			InstTable, ModuleInfo, Expansions) :-
	inst_list_matches_final(ArgsA, ArgsB, InstTable, ModuleInfo, Expansions).
inst_matches_final_3(not_reached, _, _, _, _).

:- pred maybe_pred_inst_matches_final(maybe(pred_inst_info),
		maybe(pred_inst_info), inst_table, module_info, expansions).
:- mode maybe_pred_inst_matches_final(in, in, in, in, in) is semidet.

maybe_pred_inst_matches_final(no, no, _, _, _).
maybe_pred_inst_matches_final(yes(_), no, _, _, _).
maybe_pred_inst_matches_final(yes(PredInstA), yes(PredInstB),
		InstTable, ModuleInfo, Expansions) :-
	pred_inst_matches_2(PredInstA, PredInstB, InstTable, ModuleInfo, Expansions).

:- pred inst_list_matches_final(list(inst), list(inst), 
			inst_table, module_info, expansions).
:- mode inst_list_matches_final(in, in, in, in, in) is semidet.

inst_list_matches_final([], [], _, _ModuleInfo, _).
inst_list_matches_final([ArgA | ArgsA], [ArgB | ArgsB],
			InstTable, ModuleInfo, Expansions) :-
	inst_matches_final_2(ArgA, ArgB, InstTable, ModuleInfo, Expansions),
	inst_list_matches_final(ArgsA, ArgsB, InstTable, ModuleInfo, Expansions).

	% Here we check that the functors in the first list are a
	% subset of the functors in the second list. 
	% (If a bound(...) inst only specifies the insts for some of
	% the constructors of its type, then it implicitly means that
	% all other constructors must have all their arguments
	% `not_reached'.)
	% The code here makes use of the fact that the bound_inst lists
	% are sorted.

:- pred bound_inst_list_matches_final(list(bound_inst), list(bound_inst),
				inst_table, module_info, expansions).
:- mode bound_inst_list_matches_final(in, in, in, in, in) is semidet.

bound_inst_list_matches_final([], _, _, _, _).
bound_inst_list_matches_final([X|Xs], [Y|Ys], InstTable, ModuleInfo, Expansions) :-
	X = functor(ConsIdX, ArgsX),
	Y = functor(ConsIdY, ArgsY),
	( ConsIdX = ConsIdY ->
		inst_list_matches_final(ArgsX, ArgsY, InstTable, ModuleInfo,
			Expansions),
		bound_inst_list_matches_final(Xs, Ys, InstTable, ModuleInfo,
			Expansions)
	;
		compare(>, ConsIdX, ConsIdY),
			% ConsIdY does not occur in [X|Xs].
			% Hence [X|Xs] implicitly specifies `not_reached'
			% for the args of ConsIdY, and hence 
			% automatically matches_final Y.  We just need to
			% check that [X|Xs] matches_final Ys.
		bound_inst_list_matches_final([X|Xs], Ys, InstTable, ModuleInfo,
			Expansions)
	).

inst_matches_binding(InstA, InstB, InstTable, ModuleInfo) :-
	set__init(Expansions),
	inst_matches_binding_2(InstA, InstB, InstTable, ModuleInfo, Expansions).

:- pred inst_matches_binding_2(inst, inst, inst_table, module_info,
		expansions).
:- mode inst_matches_binding_2(in, in, in, in, in) is semidet.

inst_matches_binding_2(InstA, InstB, InstTable, ModuleInfo, Expansions) :-
	ThisExpansion = InstA - InstB,
	( set__member(ThisExpansion, Expansions) ->
		true
	;
		inst_expand(InstTable, ModuleInfo, InstA, InstA2),
		inst_expand(InstTable, ModuleInfo, InstB, InstB2),
		set__insert(Expansions, ThisExpansion, Expansions2),
		inst_matches_binding_3(InstA2, InstB2, InstTable, ModuleInfo,
			Expansions2)
	).

:- pred inst_matches_binding_3(inst, inst, inst_table, module_info,
		expansions).
:- mode inst_matches_binding_3(in, in, in, in, in) is semidet.

% Note that `any' is *not* considered to match `any'.
inst_matches_binding_3(free(Aliasing), free(Aliasing), _, _, _).
inst_matches_binding_3(bound(_UniqA, ListA), bound(_UniqB, ListB), InstTable,
		ModuleInfo, Expansions) :-
	bound_inst_list_matches_binding(ListA, ListB, InstTable, ModuleInfo,
		Expansions).
inst_matches_binding_3(bound(_UniqA, ListA), ground(_UniqB, no), InstTable,
			ModuleInfo, _Exps) :-
	bound_inst_list_is_ground(ListA, InstTable, ModuleInfo).
inst_matches_binding_3(ground(_UniqA, _), bound(_UniqB, ListB), InstTable,
			ModuleInfo, _Exps) :-
	bound_inst_list_is_ground(ListB, InstTable, ModuleInfo).
		% XXX BUG! Should fail if there are not_reached
		% insts in ListB, or if ListB does not contain a complete list
		% of all the constructors for the type in question.
	%%% error("not implemented: `ground' matches_binding `bound(...)'").
inst_matches_binding_3(ground(_UniqA, PredInstA), ground(_UniqB, PredInstB),
		InstTable, ModuleInfo, _) :-
	pred_inst_matches_binding(PredInstA, PredInstB, InstTable, ModuleInfo).
inst_matches_binding_3(abstract_inst(Name, ArgsA), abstract_inst(Name, ArgsB),
		InstTable, ModuleInfo, Expansions) :-
	inst_list_matches_binding(ArgsA, ArgsB, InstTable, ModuleInfo, Expansions).
inst_matches_binding_3(not_reached, _, _, _, _).

:- pred pred_inst_matches_binding(maybe(pred_inst_info), maybe(pred_inst_info),
		inst_table, module_info).
:- mode pred_inst_matches_binding(in, in, in, in) is semidet.

pred_inst_matches_binding(no, no, _, _).
pred_inst_matches_binding(yes(_), no, _, _).
pred_inst_matches_binding(yes(PredInstA), yes(PredInstB), InstTable, ModuleInfo) :-
	pred_inst_matches(PredInstA, PredInstB, InstTable, ModuleInfo).

:- pred inst_list_matches_binding(list(inst), list(inst), inst_table,
		module_info, expansions).
:- mode inst_list_matches_binding(in, in, in, in, in) is semidet.

inst_list_matches_binding([], [], _InstTable, _ModuleInfo, _).
inst_list_matches_binding([ArgA | ArgsA], [ArgB | ArgsB], InstTable, ModuleInfo,
			Expansions) :-
	inst_matches_binding_2(ArgA, ArgB, InstTable, ModuleInfo, Expansions),
	inst_list_matches_binding(ArgsA, ArgsB, InstTable, ModuleInfo, Expansions).

	% Here we check that the functors in the first list are a
	% subset of the functors in the second list. 
	% (If a bound(...) inst only specifies the insts for some of
	% the constructors of its type, then it implicitly means that
	% all other constructors must have all their arguments
	% `not_reached'.)
	% The code here makes use of the fact that the bound_inst lists
	% are sorted.

:- pred bound_inst_list_matches_binding(list(bound_inst), list(bound_inst),
				inst_table, module_info, expansions).
:- mode bound_inst_list_matches_binding(in, in, in, in, in) is semidet.

bound_inst_list_matches_binding([], _, _, _, _).
bound_inst_list_matches_binding([X|Xs], [Y|Ys], InstTable, ModuleInfo, Expansions) :-
	X = functor(ConsIdX, ArgsX),
	Y = functor(ConsIdY, ArgsY),
	( ConsIdX = ConsIdY ->
		inst_list_matches_binding(ArgsX, ArgsY, InstTable, ModuleInfo,
				Expansions),
		bound_inst_list_matches_binding(Xs, Ys, InstTable, ModuleInfo,
				Expansions)
	;
		compare(>, ConsIdX, ConsIdY),
			% ConsIdX does not occur in [X|Xs].
			% Hence [X|Xs] implicitly specifies `not_reached'
			% for the args of ConsIdY, and hence 
			% automatically matches_binding Y.  We just need to
			% check that [X|Xs] matches_binding Ys.
		bound_inst_list_matches_binding([X|Xs], Ys, InstTable, ModuleInfo,
			Expansions)
	).

%-----------------------------------------------------------------------------%

:- type inst_property == pred(inst, inst_table, module_info, set(inst)).
:- inst inst_property = (pred(in, in, in, in) is semidet).

        % inst_is_clobbered succeeds iff the inst passed is `clobbered'
        % or `mostly_clobbered' or if it is a user-defined inst which
        % is defined as one of those.

inst_is_clobbered(not_reached, _, _) :- fail.
inst_is_clobbered(any(mostly_clobbered), _, _).
inst_is_clobbered(any(clobbered), _, _).
inst_is_clobbered(ground(clobbered, _), _, _).
inst_is_clobbered(ground(mostly_clobbered, _), _, _).
inst_is_clobbered(bound(clobbered, _), _, _).
inst_is_clobbered(bound(mostly_clobbered, _), _, _).
inst_is_clobbered(inst_var(_), _, _) :-
        error("internal error: uninstantiated inst parameter").
inst_is_clobbered(defined_inst(InstName), InstTable, ModuleInfo) :-
        inst_lookup(InstTable, ModuleInfo, InstName, Inst),
        inst_is_clobbered(Inst, InstTable, ModuleInfo).
inst_is_clobbered(alias(Key), InstTable, ModuleInfo) :-
	inst_table_get_inst_key_table(InstTable, IKT),
	inst_key_table_lookup(IKT, Key, Inst),
	inst_is_clobbered(Inst, InstTable, ModuleInfo).


        % inst_is_free succeeds iff the inst passed is `free'
        % or is a user-defined inst which is defined as `free'.
        % Abstract insts must not be free.

inst_is_free(free(_), _, _).
inst_is_free(free(_, _), _, _).
inst_is_free(inst_var(_), _, _) :-
        error("internal error: uninstantiated inst parameter").
inst_is_free(defined_inst(InstName), InstTable, ModuleInfo) :-
        inst_lookup(InstTable, ModuleInfo, InstName, Inst),
        inst_is_free(Inst, InstTable, ModuleInfo).
inst_is_free(alias(Key), InstTable, ModuleInfo) :-
	inst_table_get_inst_key_table(InstTable, IKT),
	inst_key_table_lookup(IKT, Key, Inst),
	inst_is_free(Inst, InstTable, ModuleInfo).

	% inst_is_free_alias succeeds iff the inst passed is `free(alias)'
	% or a user-defined inst which is defined as `free(alias)' or
	% `alias(IK)' where `IK' points to a `free(alias)' inst in the IKT.

inst_is_free_alias(free(alias), _, _).
inst_is_free_alias(free(alias, _), _, _).
inst_is_free_alias(inst_var(_), _, _) :-
	error("internal error: uninstantiated inst parameter").
inst_is_free_alias(defined_inst(InstName), InstTable, ModuleInfo) :-
	inst_lookup(InstTable, ModuleInfo, InstName, Inst),
	inst_is_free_alias(Inst, InstTable, ModuleInfo).
inst_is_free_alias(alias(Key), InstTable, ModuleInfo) :-
	inst_table_get_inst_key_table(InstTable, IKT),
	inst_key_table_lookup(IKT, Key, Inst),
	inst_is_free_alias(Inst, InstTable, ModuleInfo).

	% inst_contains_free_alias succeeds iff the inst passed is free(alias)
	% or is bound to a functor with an argument containing a free(alias).
inst_contains_free_alias(Inst, InstTable, ModuleInfo) :-
	set__init(Seen0),
	inst_contains_free_alias_2(Inst, InstTable, ModuleInfo, Seen0).

:- pred inst_contains_free_alias_2(inst, inst_table, module_info,
	set(inst_name)).
:- mode inst_contains_free_alias_2(in, in, in, in) is semidet.

inst_contains_free_alias_2(free(alias), _, _, _).
inst_contains_free_alias_2(free(alias, _), _, _, _).
inst_contains_free_alias_2(inst_var(_), _, _, _) :-
        error("internal error: uninstantiated inst parameter").
inst_contains_free_alias_2(defined_inst(InstName), InstTable, ModuleInfo,
		Seen0) :-
	\+ set__member(InstName, Seen0),
	inst_lookup(InstTable, ModuleInfo, InstName, Inst),
	set__insert(Seen0, InstName, Seen1),
	inst_contains_free_alias_2(Inst, InstTable, ModuleInfo, Seen1).
inst_contains_free_alias_2(alias(Key), InstTable, ModuleInfo, Seen) :-
	inst_table_get_inst_key_table(InstTable, IKT),
	inst_key_table_lookup(IKT, Key, Inst),
	inst_contains_free_alias_2(Inst, InstTable, ModuleInfo, Seen).
inst_contains_free_alias_2(bound(_, BoundInsts), InstTable, ModuleInfo, Seen) :-
	list__member(functor(_, ArgInsts), BoundInsts),
	list__member(Inst, ArgInsts),
	inst_contains_free_alias_2(Inst, InstTable, ModuleInfo, Seen).

        % inst_is_bound succeeds iff the inst passed is not `free'
        % or is a user-defined inst which is not defined as `free'.
        % Abstract insts must be bound.

inst_is_bound(not_reached, _, _).
inst_is_bound(any(_), _, _).
inst_is_bound(ground(_, _), _, _).
inst_is_bound(bound(_, _), _, _).
inst_is_bound(inst_var(_), _, _) :-
        error("internal error: uninstantiated inst parameter").
inst_is_bound(defined_inst(InstName), InstTable, ModuleInfo) :-
        inst_lookup(InstTable, ModuleInfo, InstName, Inst),
        inst_is_bound(Inst, InstTable, ModuleInfo).
inst_is_bound(abstract_inst(_, _), _, _).
inst_is_bound(alias(Key), InstTable, ModuleInfo) :-
	inst_table_get_inst_key_table(InstTable, IKT),
	inst_key_table_lookup(IKT, Key, Inst),
	inst_is_bound(Inst, InstTable, ModuleInfo).

        % inst_is_bound_to_functors succeeds iff the inst passed is
        % `bound(_Uniq, Functors)' or is a user-defined inst which expands to
        % `bound(_Uniq, Functors)'.

inst_is_bound_to_functors(bound(_Uniq, Functors), _, _, Functors).
inst_is_bound_to_functors(inst_var(_), _, _, _) :-
        error("internal error: uninstantiated inst parameter").
inst_is_bound_to_functors(defined_inst(InstName), InstTable, ModuleInfo,
		Functors) :-
        inst_lookup(InstTable, ModuleInfo, InstName, Inst),
        inst_is_bound_to_functors(Inst, InstTable, ModuleInfo, Functors).
inst_is_bound_to_functors(alias(Key), InstTable, ModuleInfo, Functors) :-
	inst_table_get_inst_key_table(InstTable, IKT),
	inst_key_table_lookup(IKT, Key, Inst),
	inst_is_bound_to_functors(Inst, InstTable, ModuleInfo, Functors).

%-----------------------------------------------------------------------------%

        % inst_is_ground succeeds iff the inst passed is `ground'
        % or the equivalent.  Abstract insts are not considered ground.

inst_is_ground(Inst, InstTable, ModuleInfo) :-
        set__init(Expansions),
        inst_is_ground_2(Inst, InstTable, ModuleInfo, Expansions).

        % The fourth arg is the set of insts which have already
        % been expanded - we use this to avoid going into an
        % infinite loop.

:- pred inst_is_ground_2(inst, inst_table, module_info, set(inst)).
:- mode inst_is_ground_2(in, in, in, in) is semidet.

inst_is_ground_2(not_reached, _, _, _).
inst_is_ground_2(bound(_, List), InstTable, ModuleInfo, Expansions) :-
	bound_inst_list_has_property(inst_is_ground_2, List, InstTable,
		ModuleInfo, Expansions).
inst_is_ground_2(ground(_, _), _, _, _).
inst_is_ground_2(inst_var(_), _, _, _) :-
        error("internal error: uninstantiated inst parameter").
inst_is_ground_2(Inst, InstTable, ModuleInfo, Expansions) :-
	Inst = defined_inst(InstName),
        ( set__member(Inst, Expansions) ->
                true
        ;
                set__insert(Expansions, Inst, Expansions2),
                inst_lookup(InstTable, ModuleInfo, InstName, Inst2),
                inst_is_ground_2(Inst2, InstTable, ModuleInfo, Expansions2)
        ).
inst_is_ground_2(alias(Key), InstTable, ModuleInfo, Expansions) :-
	inst_table_get_inst_key_table(InstTable, IKT),
	inst_key_table_lookup(IKT, Key, Inst),
	inst_is_ground_2(Inst, InstTable, ModuleInfo, Expansions).

        % inst_is_ground_or_any succeeds iff the inst passed is `ground',
        % `any', or the equivalent.  Fails for abstract insts.

inst_is_ground_or_any(Inst, InstTable, ModuleInfo) :-
        set__init(Expansions),
        inst_is_ground_or_any_2(Inst, InstTable, ModuleInfo, Expansions).

        % The fourth arg is the set of insts which have already
        % been expanded - we use this to avoid going into an
        % infinite loop.

:- pred inst_is_ground_or_any_2(inst, inst_table, module_info, set(inst)).
:- mode inst_is_ground_or_any_2(in, in, in, in) is semidet.

inst_is_ground_or_any_2(not_reached, _, _, _).
inst_is_ground_or_any_2(bound(_, List), InstTable, ModuleInfo, Expansions) :-
	bound_inst_list_has_property(inst_is_ground_or_any_2, List, InstTable,
		ModuleInfo, Expansions).
inst_is_ground_or_any_2(ground(_, _), _, _, _).
inst_is_ground_or_any_2(any(_), _, _, _).
inst_is_ground_or_any_2(inst_var(_), _, _, _) :-
        error("internal error: uninstantiated inst parameter").
inst_is_ground_or_any_2(Inst, InstTable, ModuleInfo, Expansions) :-
	Inst = defined_inst(InstName),
        ( set__member(Inst, Expansions) ->
                true
        ;
                set__insert(Expansions, Inst, Expansions2),
                inst_lookup(InstTable, ModuleInfo, InstName, Inst2),
                inst_is_ground_or_any_2(Inst2, InstTable, ModuleInfo,
				Expansions2)
        ).
inst_is_ground_or_any_2(alias(Key), InstTable, ModuleInfo, Expansions) :-
	inst_table_get_inst_key_table(InstTable, IKT),
	inst_key_table_lookup(IKT, Key, Inst),
	inst_is_ground_or_any_2(Inst, InstTable, ModuleInfo, Expansions).

        % inst_is_higher_order_ground succeeds iff the inst passed is `ground'
        % or equivalent and has a pred_inst_info.

inst_is_higher_order_ground(ground(_, yes(_PredInstInfo)), _, _).
inst_is_higher_order_ground(inst_var(_), _, _) :-
        error("internal error: uninstantiated inst parameter").
inst_is_higher_order_ground(Inst, InstTable, ModuleInfo) :-
	Inst = defined_inst(InstName),
	inst_lookup(InstTable, ModuleInfo, InstName, Inst2),
	inst_is_higher_order_ground(Inst2, InstTable, ModuleInfo).
inst_is_higher_order_ground(alias(Key), InstTable, ModuleInfo) :-
	inst_table_get_inst_key_table(InstTable, IKT),
	inst_key_table_lookup(IKT, Key, Inst),
	inst_is_higher_order_ground(Inst, InstTable, ModuleInfo).

        % inst_is_unique succeeds iff the inst passed is unique
        % or free.  Abstract insts are not considered unique.

inst_is_unique(Inst, InstTable, ModuleInfo) :-
        set__init(Expansions),
        inst_is_unique_2(Inst, InstTable, ModuleInfo, Expansions).

        % The fourth arg is the set of insts which have already
        % been expanded - we use this to avoid going into an
        % infinite loop.

:- pred inst_is_unique_2(inst, inst_table, module_info, set(inst)).
:- mode inst_is_unique_2(in, in, in, in) is semidet.

inst_is_unique_2(not_reached, _, _, _).
inst_is_unique_2(bound(unique, List), InstTable, ModuleInfo, Expansions) :-
	bound_inst_list_has_property(inst_is_unique_2, List, InstTable,
		ModuleInfo, Expansions).
inst_is_unique_2(any(unique), _, _, _).
inst_is_unique_2(free(_), _, _, _).
inst_is_unique_2(free(_, _), _, _, _).
inst_is_unique_2(ground(unique, _), _, _, _).
inst_is_unique_2(inst_var(_), _, _, _) :-
        error("internal error: uninstantiated inst parameter").
inst_is_unique_2(Inst, InstTable, ModuleInfo, Expansions) :-
	Inst = defined_inst(InstName),
        ( set__member(Inst, Expansions) ->
                true
        ;
                set__insert(Expansions, Inst, Expansions2),
                inst_lookup(InstTable, ModuleInfo, InstName, Inst2),
                inst_is_unique_2(Inst2, InstTable, ModuleInfo, Expansions2)
        ).
inst_is_unique_2(alias(Key), InstTable, ModuleInfo, Expansions) :-
	inst_table_get_inst_key_table(InstTable, IKT),
	inst_key_table_lookup(IKT, Key, Inst),
	inst_is_unique_2(Inst, InstTable, ModuleInfo, Expansions).

        % inst_is_mostly_unique succeeds iff the inst passed is unique,
        % mostly_unique, or free.  Abstract insts are not considered unique.

inst_is_mostly_unique(Inst, InstTable, ModuleInfo) :-
        set__init(Expansions),
        inst_is_mostly_unique_2(Inst, InstTable, ModuleInfo, Expansions).

        % The fourth arg is the set of insts which have already
        % been expanded - we use this to avoid going into an
        % infinite loop.

:- pred inst_is_mostly_unique_2(inst, inst_table, module_info, set(inst)).
:- mode inst_is_mostly_unique_2(in, in, in, in) is semidet.

inst_is_mostly_unique_2(not_reached, _, _, _).
inst_is_mostly_unique_2(bound(mostly_unique, List), InstTable, ModuleInfo,
		Expansions) :-
	bound_inst_list_has_property(inst_is_mostly_unique_2, List, InstTable,
		ModuleInfo, Expansions).
inst_is_mostly_unique_2(any(unique), _, _, _).
inst_is_mostly_unique_2(any(mostly_unique), _, _, _).
inst_is_mostly_unique_2(free(_), _, _, _).
inst_is_mostly_unique_2(free(_, _), _, _, _).
inst_is_mostly_unique_2(ground(unique, _), _, _, _).
inst_is_mostly_unique_2(ground(mostly_unique, _), _, _, _).
inst_is_mostly_unique_2(inst_var(_), _, _, _) :-
        error("internal error: uninstantiated inst parameter").
inst_is_mostly_unique_2(Inst, InstTable, ModuleInfo, Expansions) :-
	Inst = defined_inst(InstName),
        ( set__member(Inst, Expansions) ->
                true
        ;
                set__insert(Expansions, Inst, Expansions2),
                inst_lookup(InstTable, ModuleInfo, InstName, Inst2),
                inst_is_mostly_unique_2(Inst2, InstTable, ModuleInfo,
				Expansions2)
        ).
inst_is_mostly_unique_2(alias(Key), InstTable, ModuleInfo, Expansions) :-
	inst_table_get_inst_key_table(InstTable, IKT),
	inst_key_table_lookup(IKT, Key, Inst),
	inst_is_mostly_unique_2(Inst, InstTable, ModuleInfo, Expansions).

        % inst_is_not_partly_unique succeeds iff the inst passed is
        % not unique or mostly_unique, i.e. if it is shared
        % or free.  It fails for abstract insts.

inst_is_not_partly_unique(Inst, InstTable, ModuleInfo) :-
        set__init(Expansions),
        inst_is_not_partly_unique_2(Inst, InstTable, ModuleInfo, Expansions).

        % The fourth arg is the set of insts which have already
        % been expanded - we use this to avoid going into an
        % infinite loop.

:- pred inst_is_not_partly_unique_2(inst, inst_table, module_info,
		set(inst)).
:- mode inst_is_not_partly_unique_2(in, in, in, in) is semidet.

inst_is_not_partly_unique_2(not_reached, _, _, _).
inst_is_not_partly_unique_2(bound(shared, List), InstTable, ModuleInfo,
		Expansions) :-
	bound_inst_list_has_property(inst_is_not_partly_unique_2, List,
		InstTable, ModuleInfo, Expansions).
inst_is_not_partly_unique_2(free(_), _, _, _).
inst_is_not_partly_unique_2(any(shared), _, _, _).
inst_is_not_partly_unique_2(ground(shared, _), _, _, _).
inst_is_not_partly_unique_2(inst_var(_), _, _, _) :-
        error("internal error: uninstantiated inst parameter").
inst_is_not_partly_unique_2(Inst, InstTable, ModuleInfo, Expansions) :-
	Inst = defined_inst(InstName),
        ( set__member(Inst, Expansions) ->
                true
        ;
                set__insert(Expansions, Inst, Expansions2),
                inst_lookup(InstTable, ModuleInfo, InstName, Inst2),
                inst_is_not_partly_unique_2(Inst2, InstTable, ModuleInfo,
			Expansions2)
        ).
inst_is_not_partly_unique_2(alias(Key), InstTable, ModuleInfo, Expansions) :-
	inst_table_get_inst_key_table(InstTable, IKT),
	inst_key_table_lookup(IKT, Key, Inst),
	inst_is_not_partly_unique_2(Inst, InstTable, ModuleInfo, Expansions).

	% inst_is_not_fully_unique succeeds iff the inst passed is
        % not unique, i.e. if it is mostly_unique, shared,
        % or free.  It fails for abstract insts.

inst_is_not_fully_unique(Inst, InstTable, ModuleInfo) :-
        set__init(Expansions),
        inst_is_not_fully_unique_2(Inst, InstTable, ModuleInfo, Expansions).

        % The fourth arg is the set of insts which have already
        % been expanded - we use this to avoid going into an
        % infinite loop.

:- pred inst_is_not_fully_unique_2(inst, inst_table, module_info,
		set(inst)).
:- mode inst_is_not_fully_unique_2(in, in, in, in) is semidet.

inst_is_not_fully_unique_2(not_reached, _, _, _).
inst_is_not_fully_unique_2(bound(shared, List), InstTable, ModuleInfo,
		Expansions) :-
	bound_inst_list_has_property(inst_is_not_fully_unique_2, List,
		InstTable, ModuleInfo, Expansions).
inst_is_not_fully_unique_2(bound(mostly_unique, List), InstTable, ModuleInfo,
		Expansions) :-
	bound_inst_list_has_property(inst_is_not_fully_unique_2, List,
		InstTable, ModuleInfo, Expansions).
inst_is_not_fully_unique_2(any(shared), _, _, _).
inst_is_not_fully_unique_2(any(mostly_unique), _, _, _).
inst_is_not_fully_unique_2(free(_), _, _, _).
inst_is_not_fully_unique_2(ground(shared, _), _, _, _).
inst_is_not_fully_unique_2(ground(mostly_unique, _), _, _, _).
inst_is_not_fully_unique_2(inst_var(_), _, _, _) :-
        error("internal error: uninstantiated inst parameter").
inst_is_not_fully_unique_2(Inst, InstTable, ModuleInfo, Expansions) :-
        Inst = defined_inst(InstName),
	( set__member(Inst, Expansions) ->
		true
        ;
                set__insert(Expansions, Inst, Expansions2),
                inst_lookup(InstTable, ModuleInfo, InstName, Inst2),
                inst_is_not_fully_unique_2(Inst2, InstTable, ModuleInfo,
				Expansions2)
        ).
inst_is_not_fully_unique_2(alias(Key), InstTable, ModuleInfo, Expansions) :-
	inst_table_get_inst_key_table(InstTable, IKT),
	inst_key_table_lookup(IKT, Key, Inst),
	inst_is_not_fully_unique_2(Inst, InstTable, ModuleInfo, Expansions).

%-----------------------------------------------------------------------------%

:- pred bound_inst_list_has_property(inst_property, list(bound_inst),
		inst_table, module_info, set(inst)).
:- mode bound_inst_list_has_property(in(inst_property), in, in, in, in)
		is semidet.

bound_inst_list_has_property(_, [], _, _, _).
bound_inst_list_has_property(Property, [functor(_Name, Args) | BoundInsts],
		InstTable, ModuleInfo, Expansions) :-
	inst_list_has_property(Property, Args, InstTable, ModuleInfo,
		Expansions),
	bound_inst_list_has_property(Property, BoundInsts, InstTable,
		ModuleInfo, Expansions).
% bound_inst_list_has_property(Property, [functor(_Name, Args) | BoundInsts],
% 		InstTable, ModuleInfo, Expansions) :-
% 	all [Args] (
% 		list__member(functor(_Name, Args), BoundInsts)
% 	=>
% 		inst_list_has_property(Property, BoundInsts, InstTable,
% 			ModuleInfo, Expansions)
% 	).

bound_inst_list_is_ground(BoundInsts, InstTable, ModuleInfo) :-
	set__init(Expansions),
	bound_inst_list_has_property(inst_is_ground_2, BoundInsts, InstTable,
		ModuleInfo, Expansions).

bound_inst_list_is_ground_or_any(BoundInsts, InstTable, ModuleInfo) :-
	set__init(Expansions),
	bound_inst_list_has_property(inst_is_ground_or_any_2, BoundInsts,
		InstTable, ModuleInfo, Expansions).

bound_inst_list_is_unique(BoundInsts, InstTable, ModuleInfo) :-
	set__init(Expansions),
	bound_inst_list_has_property(inst_is_unique_2, BoundInsts, InstTable,
		ModuleInfo, Expansions).

bound_inst_list_is_mostly_unique(BoundInsts, InstTable, ModuleInfo) :-
	set__init(Expansions),
	bound_inst_list_has_property(inst_is_mostly_unique_2, BoundInsts,
		InstTable, ModuleInfo, Expansions).

bound_inst_list_is_not_partly_unique(BoundInsts, InstTable, ModuleInfo) :-
	set__init(Expansions),
	bound_inst_list_has_property(inst_is_not_partly_unique_2, BoundInsts,
		InstTable, ModuleInfo, Expansions).

bound_inst_list_is_not_fully_unique(BoundInsts, InstTable, ModuleInfo) :-
	set__init(Expansions),
	bound_inst_list_has_property(inst_is_not_fully_unique_2, BoundInsts,
		InstTable, ModuleInfo, Expansions).

%-----------------------------------------------------------------------------%

:- pred inst_list_has_property(inst_property, list(inst), inst_table,
		module_info, set(inst)).
:- mode inst_list_has_property(in(inst_property), in, in, in, in) is semidet.

inst_list_has_property(_Property, [], _InstTable, _ModuleInfo, _Expansions).
inst_list_has_property(Property, [Inst | Insts], InstTable, ModuleInfo,
		Expansions) :-
	call(Property, Inst, InstTable, ModuleInfo, Expansions),
	inst_list_has_property(Property, Insts, InstTable, ModuleInfo,
		Expansions).
% inst_list_has_property(Property, Insts, ModuleInfo, Expansions) :-
% 	all [Inst] (
% 		list__member(Inst, Insts)
% 	=>
% 		call(Property, Inst, InstTable, ModuleInfo, Expansions)
% 	).

inst_list_is_ground(Insts, InstTable, ModuleInfo) :-
	set__init(Expansions),
	inst_list_has_property(inst_is_ground_2, Insts, InstTable, ModuleInfo,
			Expansions).

inst_list_is_ground_or_any(Insts, InstTable, ModuleInfo) :-
	set__init(Expansions),
	inst_list_has_property(inst_is_ground_or_any_2, Insts, InstTable,
			ModuleInfo, Expansions).

inst_list_is_unique(Insts, InstTable, ModuleInfo) :-
	set__init(Expansions),
	inst_list_has_property(inst_is_unique_2, Insts, InstTable, ModuleInfo,
			Expansions).

inst_list_is_mostly_unique(Insts, InstTable, ModuleInfo) :-
	set__init(Expansions),
	inst_list_has_property(inst_is_mostly_unique_2, Insts, InstTable,
			ModuleInfo, Expansions).

inst_list_is_not_partly_unique(Insts, InstTable, ModuleInfo) :-
	set__init(Expansions),
	inst_list_has_property(inst_is_not_partly_unique_2, Insts, InstTable,
			ModuleInfo, Expansions).

inst_list_is_not_fully_unique(Insts, InstTable, ModuleInfo) :-
	set__init(Expansions),
	inst_list_has_property(inst_is_not_fully_unique_2, Insts, InstTable,
			ModuleInfo, Expansions).

%-----------------------------------------------------------------------------%

bound_inst_list_is_free([], _, _).
bound_inst_list_is_free([functor(_Name, Args)|BoundInsts], InstTable,
		ModuleInfo) :-
        inst_list_is_free(Args, InstTable, ModuleInfo),
        bound_inst_list_is_free(BoundInsts, InstTable, ModuleInfo).

inst_list_is_free([], _, _).
inst_list_is_free([Inst | Insts], InstTable, ModuleInfo) :-
        inst_is_free(Inst, InstTable, ModuleInfo),
        inst_list_is_free(Insts, InstTable, ModuleInfo).

%-----------------------------------------------------------------------------%

inst_list_is_ground_or_dead([], [], _InstTable, _ModuleInfo).
inst_list_is_ground_or_dead([Inst | Insts], [Live | Lives], InstTable,
		ModuleInfo) :-
	( Live = live ->
		inst_is_ground(Inst, InstTable, ModuleInfo)
	;
		true
	),
	inst_list_is_ground_or_dead(Insts, Lives, InstTable, ModuleInfo).

inst_list_is_ground_or_any_or_dead([], [], _, _).
inst_list_is_ground_or_any_or_dead([Inst | Insts], [Live | Lives],
		InstTable, ModuleInfo) :-
	( Live = live ->
		inst_is_ground_or_any(Inst, InstTable, ModuleInfo)
	;
		true
	),
	inst_list_is_ground_or_any_or_dead(Insts, Lives, InstTable, ModuleInfo).

%-----------------------------------------------------------------------------%

inst_contains_instname(Inst, InstTable, ModuleInfo, InstName) :-
	set__init(Expansions),
	inst_contains_instname_2(Inst, InstTable, ModuleInfo, Expansions,
			InstName).

:- pred inst_contains_instname_2(inst, inst_table, module_info,
		set(inst_name), inst_name).
:- mode inst_contains_instname_2(in, in, in, in, in) is semidet.

inst_contains_instname_2(defined_inst(InstName1), InstTable, ModuleInfo,
		Expansions0, InstName) :-
	(
		InstName = InstName1
	;
		not set__member(InstName1, Expansions0),
		inst_lookup(InstTable, ModuleInfo, InstName1, Inst1),
		set__insert(Expansions0, InstName1, Expansions),
		inst_contains_instname_2(Inst1, InstTable, ModuleInfo,
			Expansions, InstName)
	).
inst_contains_instname_2(bound(_Uniq, ArgInsts), InstTable, ModuleInfo,
		Expansions, InstName) :-
	bound_inst_list_contains_instname(ArgInsts, InstTable, ModuleInfo,
		Expansions, InstName).
inst_contains_instname_2(alias(InstKey), InstTable, ModuleInfo, Expansions,
		InstName) :-
	inst_table_get_inst_key_table(InstTable, IKT),
	inst_key_table_lookup(IKT, InstKey, Inst),
	inst_contains_instname_2(Inst, InstTable, ModuleInfo, Expansions,
		InstName).

:- pred bound_inst_list_contains_instname(list(bound_inst), inst_table,
		module_info, set(inst_name), inst_name).
:- mode bound_inst_list_contains_instname(in, in, in, in, in) is semidet.

bound_inst_list_contains_instname([BoundInst|BoundInsts], InstTable, ModuleInfo,
		Expansions, InstName) :-
	BoundInst = functor(_Functor, ArgInsts),
	(
		inst_list_contains_instname(ArgInsts, InstTable, ModuleInfo,
			Expansions, InstName)
	;
		bound_inst_list_contains_instname(BoundInsts, InstTable,
			ModuleInfo, Expansions, InstName)
	).

:- pred inst_list_contains_instname(list(inst), inst_table, module_info,
			set(inst_name), inst_name).
:- mode inst_list_contains_instname(in, in, in, in, in) is semidet.

inst_list_contains_instname([Inst|Insts], InstTable, ModuleInfo, Expansions,
		InstName) :-
	(
		inst_contains_instname_2(Inst, InstTable, ModuleInfo,
				Expansions, InstName)
	;
		inst_list_contains_instname(Insts, InstTable, ModuleInfo,
				Expansions, InstName)
	).

%-----------------------------------------------------------------------------%

:- pred inst_contains_inst_var(inst, inst_table, module_info, inst_var).
:- mode inst_contains_inst_var(in, in, in, out) is nondet.

inst_contains_inst_var(Inst, InstTable, ModuleInfo, InstVar) :-
	set__init(Expansions),
	inst_contains_inst_var_2(Inst, InstTable, ModuleInfo, Expansions,
			InstVar).

:- pred inst_contains_inst_var_2(inst, inst_table, module_info,
		set(inst_name), inst_var).
:- mode inst_contains_inst_var_2(in, in, in, in, out) is nondet.

inst_contains_inst_var_2(inst_var(InstVar), _, _, _, InstVar).
inst_contains_inst_var_2(alias(Key), InstTable, ModuleInfo, Expansions,
		InstVar) :-
	inst_table_get_inst_key_table(InstTable, IKT),
	inst_key_table_lookup(IKT, Key, Inst),
	inst_contains_inst_var_2(Inst, InstTable, ModuleInfo, Expansions,
		InstVar).
inst_contains_inst_var_2(defined_inst(InstName), InstTable, ModuleInfo,
		Expansions0, InstVar) :-
	\+ set__member(InstName, Expansions0),
	inst_lookup(InstTable, ModuleInfo, InstName, Inst),
	set__insert(Expansions0, InstName, Expansions),
	inst_contains_inst_var_2(Inst, InstTable, ModuleInfo, Expansions,
		InstVar).
inst_contains_inst_var_2(bound(_Uniq, ArgInsts), InstTable, ModuleInfo,
		Expansions, InstVar) :-
	bound_inst_list_contains_inst_var(ArgInsts, InstTable, ModuleInfo,
		Expansions, InstVar).
inst_contains_inst_var_2(ground(_Uniq, PredInstInfo), _InstTable, ModuleInfo,
		Expansions, InstVar) :-
	PredInstInfo = yes(pred_inst_info(_PredOrFunc,
		argument_modes(ArgInstTable, Modes), _Det)),
	mode_list_contains_inst_var_2(Modes, ArgInstTable, ModuleInfo,
		Expansions, InstVar).
inst_contains_inst_var_2(abstract_inst(_Name, ArgInsts), InstTable, ModuleInfo,
		Expansions, InstVar) :-
	inst_list_contains_inst_var(ArgInsts, InstTable, ModuleInfo, Expansions,
		InstVar).
inst_contains_inst_var_2(ground(_Uniq, PredInstInfo), InstTable,
		ModuleInfo, Expansions, InstVar) :-
	PredInstInfo = yes(pred_inst_info(_PredOrFunc, ArgModes, _Det)),
	ArgModes = argument_modes(_, Modes),
	mode_list_contains_inst_var_2(Modes, InstTable, ModuleInfo, Expansions,
		InstVar).
inst_contains_inst_var_2(abstract_inst(_Name, ArgInsts), InstTable, ModuleInfo,
		Expansions, InstVar) :-
	inst_list_contains_inst_var(ArgInsts, InstTable, ModuleInfo, Expansions,
		InstVar).

:- pred bound_inst_list_contains_inst_var(list(bound_inst), inst_table,
			module_info, set(inst_name), inst_var).
:- mode bound_inst_list_contains_inst_var(in, in, in, in, out) is nondet.

bound_inst_list_contains_inst_var([BoundInst|BoundInsts], InstTable, ModuleInfo,
		Expansions, InstVar) :-
	BoundInst = functor(_Functor, ArgInsts),
	(
		inst_list_contains_inst_var(ArgInsts, InstTable, ModuleInfo,
			Expansions, InstVar)
	;
		bound_inst_list_contains_inst_var(BoundInsts, InstTable,
			ModuleInfo, Expansions, InstVar)
	).

:- pred inst_list_contains_inst_var(list(inst), inst_table, module_info,
		set(inst_name), inst_var).
:- mode inst_list_contains_inst_var(in, in, in, in, out) is nondet.

inst_list_contains_inst_var([Inst|Insts], InstTable, ModuleInfo, Expansions,
		InstVar) :-
	(
		inst_contains_inst_var_2(Inst, InstTable, ModuleInfo,
			Expansions, InstVar)
	;
		inst_list_contains_inst_var(Insts, InstTable, ModuleInfo,
			Expansions, InstVar)
	).

mode_list_contains_inst_var(Modes, InstTable, ModuleInfo, InstVar) :-
	set__init(Expansions),
	mode_list_contains_inst_var_2(Modes, InstTable, ModuleInfo, Expansions,
		InstVar).

:- pred mode_list_contains_inst_var_2(list(mode), inst_table, module_info,
		set(inst_name), inst_var).
:- mode mode_list_contains_inst_var_2(in, in, in, in, out) is nondet.

mode_list_contains_inst_var_2([Mode|_Modes], InstTable, ModuleInfo, Expansions,
		InstVar) :-
	mode_get_insts_semidet(ModuleInfo, Mode, Initial, Final),
	( Inst = Initial ; Inst = Final ),
	inst_contains_inst_var_2(Inst, InstTable, ModuleInfo, Expansions,
		InstVar).
mode_list_contains_inst_var_2([_|Modes], InstTable, ModuleInfo, Expansions,
		InstVar) :-
	mode_list_contains_inst_var_2(Modes, InstTable, ModuleInfo, Expansions,
		InstVar).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
