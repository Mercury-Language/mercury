%-----------------------------------------------------------------------------%
% Copyright (C) 1995-1998, 2000-2002 The University of Melbourne.
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

:- module check_hlds__inst_match.

:- interface.

:- import_module hlds__hlds_module, parse_tree__prog_data, (parse_tree__inst).
:- import_module list.

%-----------------------------------------------------------------------------%

:- pred inst_expand(module_info, inst, inst).
:- mode inst_expand(in, in, out) is det.

	% inst_expand(ModuleInfo, Inst0, Inst) checks if the top-level 
	% part of the inst is a defined inst, and if so replaces it
	% with the definition.

:- pred inst_expand_and_remove_constrained_inst_vars(module_info, inst, inst).
:- mode inst_expand_and_remove_constrained_inst_vars(in, in, out) is det.

	% inst_expand_and_remove_constrained_inst_vars is the same as
	% inst_expand except that it also removes constrained_inst_vars from the
	% top level, replacing them with the constraining inst.

%-----------------------------------------------------------------------------%

	% inst_matches_initial(InstA, InstB, Type, ModuleInfo):
	%	Succeed iff `InstA' specifies at least as much
	%	information as `InstB', and in those parts where they
	%	specify the same information, `InstA' is at least as
	%	instantiated as `InstB'.
	%	Thus, inst_matches_initial(not_reached, ground, _)
	%	succeeds, since not_reached contains more information
	%	than ground - but not vice versa.  Similarly,
	%	inst_matches_initial(bound(a), bound(a;b), _) should
	%	succeed, but not vice versa.

:- pred inst_matches_initial(inst, inst, type, module_info).
:- mode inst_matches_initial(in, in, in, in) is semidet.

	% This version of inst_matches_initial builds up a substitution map
	% (inst_var_sub).  For each inst_var which occurs in InstA there will be
	% a substitution to the corresponding inst in InstB.

:- pred inst_matches_initial(inst, inst, type, module_info, module_info,
		inst_var_sub, inst_var_sub).
:- mode inst_matches_initial(in, in, in, in, out, in, out) is semidet.

	% inst_matches_final(InstA, InstB, ModuleInfo):
	%	Succeed iff InstA is compatible with InstB,
	%	i.e. iff InstA will satisfy the final inst
	%	requirement InstB.  This is true if the
	%	information specified by InstA is at least as
	%	great as that specified by InstB, and where the information
	%	is the same and both insts specify a binding, the binding
	%	must be identical.

:- pred inst_matches_final(inst, inst, module_info).
:- mode inst_matches_final(in, in, in) is semidet.

	% This version of inst_matches_final allows you to pass in the type of
	% the variables being compared.  This allows it to be more precise (i.e.
	% less conservative) for cases such as
	%	inst_matches_final(ground(...), bound(...), ...).
	% This version is to be preferred when the type is available.

:- pred inst_matches_final(inst, inst, type, module_info).
:- mode inst_matches_final(in, in, in, in) is semidet.

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

:- pred inst_matches_binding(inst, inst, type, module_info).
:- mode inst_matches_binding(in, in, in, in) is semidet.

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
	% This predicate succeeds for non-standard function insts so some care
	% needs to be taken since these insts may not be replaced by a less
	% precise inst that uses the higher-order mode information.
:- pred inst_is_ground(module_info, inst).
:- mode inst_is_ground(in, in) is semidet.

        % succeed if the inst is not partly free (i.e. contains only
        % `any', `ground', `bound', and `not_reached' insts, with no
        % `free' insts).
	% This predicate succeeds for non-standard function insts so some care
	% needs to be taken since these insts may not be replaced by a less
	% precise inst that uses the higher-order mode information.
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
:- import_module hlds__hlds_data, check_hlds__mode_util.
:- import_module parse_tree__prog_data, check_hlds__inst_util.
:- import_module check_hlds__type_util.
:- import_module list, set, map, term, std_util, require, bool.

inst_matches_initial(InstA, InstB, Type, ModuleInfo) :-
	inst_matches_initial_1(InstA, InstB, Type, ModuleInfo, _, no, _).

inst_matches_initial(InstA, InstB, Type, ModuleInfo0, ModuleInfo, Sub0, Sub) :-
	inst_matches_initial_1(InstA, InstB, Type, ModuleInfo0, ModuleInfo,
		yes(Sub0), MaybeSub),
	(
		MaybeSub = yes(Sub)
	;
		MaybeSub = no,
		error("inst_matches_initial: missing inst_var_sub")
	).

:- pred inst_matches_initial_1(inst, inst, type, module_info, module_info,
		maybe(inst_var_sub), maybe(inst_var_sub)).
:- mode inst_matches_initial_1(in, in, in, in, out, in, out) is semidet.

inst_matches_initial_1(InstA, InstB, Type, ModuleInfo0, ModuleInfo,
		MaybeSub0, MaybeSub) :-
	Info0 = init_inst_match_info(ModuleInfo0) ^ maybe_sub := MaybeSub0,
	inst_matches_initial_2(InstA, InstB, yes(Type), Info0, Info),
	ModuleInfo = Info^module_info,
	MaybeSub = Info ^ maybe_sub.

:- type expansions == set(pair(inst)).

:- type inst_match_info
	--->	inst_match_info(
			module_info	:: module_info,
			expansions	:: expansions,
			maybe_sub	:: maybe(inst_var_sub)
		).

:- func sub(inst_match_info) = inst_var_sub is semidet.

sub(Info) = Sub :-
	Info ^ maybe_sub = yes(Sub).

:- func 'sub :='(inst_match_info, inst_var_sub) = inst_match_info.

'sub :='(Info, Sub) =
	Info ^ maybe_sub := yes(Sub).

:- func init_inst_match_info(module_info) = inst_match_info.

init_inst_match_info(ModuleInfo) = inst_match_info(ModuleInfo, Exp, no) :-
	set__init(Exp).

:- pred inst_matches_initial_2(inst, inst, maybe(type), 
		inst_match_info, inst_match_info).
:- mode inst_matches_initial_2(in, in, in, in, out) is semidet.

inst_matches_initial_2(InstA, InstB, Type, Info0, Info) :-
	ThisExpansion = InstA - InstB,
	( set__member(ThisExpansion, Info0^expansions) ->
		Info = Info0

	;
		inst_expand(Info0^module_info, InstA, InstA2),
		inst_expand(Info0^module_info, InstB, InstB2),
		set__insert(Info0^expansions, ThisExpansion, Expansions1),
		inst_matches_initial_3(InstA2, InstB2, Type, 
			Info0^expansions := Expansions1, Info)
	).

:- pred inst_matches_initial_3(inst, inst, maybe(type), 
		inst_match_info, inst_match_info).
:- mode inst_matches_initial_3(in, in, in, in, out) is semidet.

inst_matches_initial_3(InstA, InstB, Type, Info0, Info) :-
	( InstB = constrained_inst_vars(InstVarsB, InstB1) ->
		% InstB is a constrained_inst_var with upper bound InstB1.
		% We need to check that InstA matches_initial InstB1 and add the
		% appropriate inst_var substitution.

		inst_matches_initial_2(InstA, InstB1, Type, Info0, Info1),

		ModuleInfo0 = Info1^module_info,

		% Call abstractly_unify_inst to calculate the uniqueness of the
		% inst represented by the constrained_inst_var.
		% We pass `Live = dead' because we want
		% abstractly_unify(unique, unique) = unique, not shared.
		Live = dead,
		abstractly_unify_inst(Live, InstA, InstB1, fake_unify,
			ModuleInfo0, Inst, _Det, ModuleInfo1),
		Info2 = Info1 ^ module_info := ModuleInfo1,
		update_inst_var_sub(InstVarsB, Inst, Type, Info2, Info)
	; InstA = constrained_inst_vars(_InstVarsA, InstA1) ->
		inst_matches_initial_2(InstA1, InstB, Type, Info0, Info)
	;
		inst_matches_initial_4(InstA, InstB, Type, Info0, Info)
	).

:- pred inst_matches_initial_4(inst, inst, maybe(type), 
		inst_match_info, inst_match_info).
:- mode inst_matches_initial_4(in, in, in, in, out) is semidet.

	% To avoid infinite regress, we assume that
	% inst_matches_initial is true for any pairs of insts which
	% occur in `Expansions'.

inst_matches_initial_4(any(UniqA), any(UniqB), _, I, I) :-
	unique_matches_initial(UniqA, UniqB).
inst_matches_initial_4(any(_), free, _, I, I).
inst_matches_initial_4(free, any(_), _, I, I).
inst_matches_initial_4(free, free, _, I, I).
inst_matches_initial_4(bound(UniqA, ListA), any(UniqB), _, Info, Info) :-
	unique_matches_initial(UniqA, UniqB),
	bound_inst_list_matches_uniq(ListA, UniqB, Info^module_info).
inst_matches_initial_4(bound(_Uniq, _List), free, _, I, I).
inst_matches_initial_4(bound(UniqA, ListA), bound(UniqB, ListB), Type,
		Info0, Info) :-
	unique_matches_initial(UniqA, UniqB),
	bound_inst_list_matches_initial(ListA, ListB, Type, Info0, Info).
inst_matches_initial_4(bound(UniqA, ListA), ground(UniqB, none), _,
		Info, Info) :-
	unique_matches_initial(UniqA, UniqB),
	bound_inst_list_is_ground(ListA, Info^module_info),
	bound_inst_list_matches_uniq(ListA, UniqB, Info^module_info).
inst_matches_initial_4(bound(Uniq, List), abstract_inst(_,_), _, Info, Info) :-
	Uniq = unique,
	bound_inst_list_is_ground(List, Info^module_info),
	bound_inst_list_is_unique(List, Info^module_info).
inst_matches_initial_4(bound(Uniq, List), abstract_inst(_,_), _, Info, Info) :-
	Uniq = mostly_unique,
	bound_inst_list_is_ground(List, Info^module_info),
	bound_inst_list_is_mostly_unique(List, Info^module_info).
inst_matches_initial_4(ground(UniqA, GroundInstInfoA), any(UniqB), _,
		Info, Info) :-
	\+ ground_inst_info_is_nonstandard_func_mode(GroundInstInfoA,
		Info^module_info),
	unique_matches_initial(UniqA, UniqB).
inst_matches_initial_4(ground(_Uniq, _PredInst), free, _, I, I).
inst_matches_initial_4(ground(UniqA, _GII_A), bound(UniqB, ListB), MaybeType,
		Info0, Info) :-
	MaybeType = yes(Type),
		% We can only check this case properly if the type is known.
	unique_matches_initial(UniqA, UniqB),
	bound_inst_list_is_complete_for_type(set__init, Info0^module_info,
		ListB, Type),
	ground_matches_initial_bound_inst_list(UniqA, ListB, yes(Type),
		Info0, Info).
inst_matches_initial_4(ground(UniqA, GroundInstInfoA),
		ground(UniqB, GroundInstInfoB), Type, Info0, Info) :-
	unique_matches_initial(UniqA, UniqB),
	ground_inst_info_matches_initial(GroundInstInfoA, GroundInstInfoB,
		UniqB, Type, Info0, Info).
inst_matches_initial_4(ground(_UniqA, none), abstract_inst(_,_),_,_,_) :-
		% I don't know what this should do.
		% Abstract insts aren't really supported.
	error("inst_matches_initial(ground, abstract_inst) == ??").
inst_matches_initial_4(abstract_inst(_,_), any(shared), _, I, I).
inst_matches_initial_4(abstract_inst(_,_), free, _, I, I).
inst_matches_initial_4(abstract_inst(Name, ArgsA), abstract_inst(Name, ArgsB),
		_Type, Info0, Info) :-
	list__duplicate(length(ArgsA), no, MaybeTypes),
		% XXX how do we get the argument types for an abstract inst?
	inst_list_matches_initial(ArgsA, ArgsB, MaybeTypes, Info0, Info).
inst_matches_initial_4(not_reached, _, _, I, I).

%-----------------------------------------------------------------------------%

	% This predicate assumes that the check of
	% `bound_inst_list_is_complete_for_type' is done by the caller.
:- pred ground_matches_initial_bound_inst_list(uniqueness, list(bound_inst),
	maybe(type), inst_match_info, inst_match_info).
:- mode ground_matches_initial_bound_inst_list(in, in, in, in, out) is semidet.

ground_matches_initial_bound_inst_list(_, [], _) --> [].
ground_matches_initial_bound_inst_list(Uniq, [functor(ConsId, Args) | List],
		MaybeType) -->
	ModuleInfo0 =^ module_info,
	{ maybe_get_cons_id_arg_types(ModuleInfo0, MaybeType, ConsId,
		list__length(Args), MaybeTypes) },
	ground_matches_initial_inst_list(Uniq, Args, MaybeTypes),
	ground_matches_initial_bound_inst_list(Uniq, List, MaybeType).

:- pred ground_matches_initial_inst_list(uniqueness, list(inst),
	list(maybe(type)), inst_match_info, inst_match_info).
:- mode ground_matches_initial_inst_list(in, in, in, in, out) is semidet.

ground_matches_initial_inst_list(_, [], []) --> [].
ground_matches_initial_inst_list(Uniq, [Inst | Insts], [Type | Types]) -->
	inst_matches_initial_2(ground(Uniq, none), Inst, Type),
	ground_matches_initial_inst_list(Uniq, Insts, Types).

%-----------------------------------------------------------------------------%

	% A list(bound_inst) is ``complete'' for a given type iff it
	% includes each functor of the type and each argument of each
	% functor is also ``complete'' for the type.
:- pred bound_inst_list_is_complete_for_type(set(inst_name), module_info,
		list(bound_inst), type).
:- mode bound_inst_list_is_complete_for_type(in, in, in, in) is semidet.

bound_inst_list_is_complete_for_type(Expansions, ModuleInfo, BoundInsts, Type)
		:-
	% Is this a type for which cons_ids are recorded in the type_table?
	type_util__cons_id_arg_types(ModuleInfo, Type, _, _),

	% Is there a bound_inst for each cons_id in the type_table?
	all [ConsId, ArgTypes] (
		type_util__cons_id_arg_types(ModuleInfo, Type, ConsId,
			ArgTypes)
	=>
		(
			list__member(functor(ConsId0, ArgInsts), BoundInsts),
			% Cons_ids returned from type_util__cons_id_arg_types
			% are not module-qualified so we need to call
			% equivalent_cons_ids instead of just using `=/2'.
			equivalent_cons_ids(ConsId0, ConsId),
			list__map(inst_is_complete_for_type(Expansions,
				ModuleInfo), ArgInsts, ArgTypes)
		)
	).

:- pred inst_is_complete_for_type(set(inst_name), module_info, inst, type).
:- mode inst_is_complete_for_type(in, in, in, in) is semidet.

inst_is_complete_for_type(Expansions, ModuleInfo, Inst, Type) :-
	( Inst = defined_inst(Name) ->
		( set__member(Name, Expansions) ->
			true
		;
			inst_lookup(ModuleInfo, Name, ExpandedInst),
			inst_is_complete_for_type(Expansions `set__insert` Name,
				ModuleInfo, ExpandedInst, Type)
		)
	; Inst = bound(_, List) ->
		bound_inst_list_is_complete_for_type(Expansions, ModuleInfo,
			List, Type)
	;
		Inst \= not_reached
	).

	% Check that two cons_ids are the same, except that one may be less
	% module qualified than the other.
:- pred equivalent_cons_ids(cons_id, cons_id).
:- mode equivalent_cons_ids(in, in) is semidet.

equivalent_cons_ids(ConsIdA, ConsIdB) :-
	(
		ConsIdA = cons(NameA, ArityA),
		ConsIdB = cons(NameB, ArityB)
	->
		ArityA = ArityB,
		equivalent_sym_names(NameA, NameB)
	;
		ConsIdA = ConsIdB
	).

:- pred equivalent_sym_names(sym_name, sym_name).
:- mode equivalent_sym_names(in, in) is semidet.

equivalent_sym_names(unqualified(S), unqualified(S)).
equivalent_sym_names(qualified(_, S), unqualified(S)).
equivalent_sym_names(unqualified(S), qualified(_, S)).
equivalent_sym_names(qualified(QualA, S), qualified(QualB, S)) :-
	equivalent_sym_names(QualA, QualB).

%-----------------------------------------------------------------------------%

	% Update the inst_var_sub that is computed by inst_matches_initial.
	% The inst_var_sub records what inst should be substituted for each
	% inst_var that occurs in the called procedure's argument modes.
:- pred update_inst_var_sub(set(inst_var), inst, maybe(type), inst_match_info,
		inst_match_info).
:- mode update_inst_var_sub(in, in, in, in, out) is semidet.

update_inst_var_sub(InstVars, InstA, MaybeType) -->
	( yes(_) =^ maybe_sub ->
		set__fold((pred(InstVar::in, in, out) is semidet -->
			( InstB =^ sub ^ elem(InstVar) ->
				% If InstVar already has an inst associated with
				% it, merge the old inst and the new inst.  Fail
				% if this merge is not possible.
				M0 =^ module_info,
				{ inst_merge(InstA, InstB, MaybeType, M0,
					Inst, M) },
				^ module_info := M,
				^ sub ^ elem(InstVar) := Inst
			;
				^ sub ^ elem(InstVar) := InstA
			)), InstVars)
	;
		[]
	).

%-----------------------------------------------------------------------------%

	% This predicate checks if two ground_inst_infos match_initial.
	% It does not check uniqueness.
:- pred ground_inst_info_matches_initial(ground_inst_info, ground_inst_info,
		uniqueness, maybe(type), inst_match_info, inst_match_info).
:- mode ground_inst_info_matches_initial(in, in, in, in, in, out) is semidet.

ground_inst_info_matches_initial(GroundInstInfoA, none, _, _) -->
	ModuleInfo =^ module_info,
	{ \+ ground_inst_info_is_nonstandard_func_mode(GroundInstInfoA,
		ModuleInfo) }.
ground_inst_info_matches_initial(none, higher_order(PredInstB), _, Type) -->
	{ PredInstB = pred_inst_info(function, ArgModes, _Det) },
	{ Arity = list__length(ArgModes) },
	{ PredInstA = pred_inst_info_standard_func_mode(Arity) },
	pred_inst_matches_initial(PredInstA, PredInstB, Type).
ground_inst_info_matches_initial(higher_order(PredInstA),
		higher_order(PredInstB), _, MaybeType) -->
	pred_inst_matches_initial(PredInstA, PredInstB, MaybeType).

:- pred pred_inst_matches_initial(pred_inst_info, pred_inst_info, maybe(type),
	inst_match_info, inst_match_info).
:- mode pred_inst_matches_initial(in, in, in, in, out) is semidet.

pred_inst_matches_initial(pred_inst_info(PredOrFunc, ModesA, Det),
		pred_inst_info(PredOrFunc, ModesB, Det), MaybeType) -->
	{ maybe_get_higher_order_arg_types(MaybeType, length(ModesA),
		MaybeTypes) },
	pred_inst_argmodes_matches_initial(ModesA, ModesB, MaybeTypes),
	( 
		Sub =^ sub
	->
		{ mode_list_apply_substitution(ModesA, Sub, ModesASub) },
		{ mode_list_apply_substitution(ModesB, Sub, ModesBSub) }
	;
		{ ModesASub = ModesA },
		{ ModesBSub = ModesB }
	),
	pred_inst_argmodes_matches(ModesASub, ModesBSub, MaybeTypes).

:- pred pred_inst_argmodes_matches_initial(list(mode), list(mode),
	list(maybe(type)), inst_match_info, inst_match_info).
:- mode pred_inst_argmodes_matches_initial(in, in, in, in, out) is semidet.

pred_inst_argmodes_matches_initial([], [], []) --> [].
pred_inst_argmodes_matches_initial([ModeA|ModeAs], [ModeB|ModeBs],
		[Type|Types]) -->
	ModuleInfo0 =^ module_info,
	{ mode_get_insts(ModuleInfo0, ModeA, InitialA, FinalA) },
	{ mode_get_insts(ModuleInfo0, ModeB, InitialB, FinalB) },
	inst_matches_initial_2(InitialA, InitialB, Type),
	inst_matches_initial_2(FinalA, FinalB, Type),
	pred_inst_argmodes_matches_initial(ModeAs, ModeBs, Types).

pred_inst_matches(PredInstA, PredInstB, ModuleInfo) :-
	pred_inst_matches_1(PredInstA, PredInstB, no, ModuleInfo).

:- pred pred_inst_matches_1(pred_inst_info, pred_inst_info, maybe(type),
		module_info).
:- mode pred_inst_matches_1(in, in, in, in) is semidet.

pred_inst_matches_1(PredInstA, PredInstB, MaybeType, ModuleInfo) :-
	Info0 = init_inst_match_info(ModuleInfo),
	pred_inst_matches_2(PredInstA, PredInstB, MaybeType, Info0, _).

	% pred_inst_matches_2(PredInstA, PredInstB, ModuleInfo, Expansions)
	%	Same as pred_inst_matches/3, except that inst pairs in
	%	Expansions are assumed to match_final each other.
	%	(This avoids infinite loops when calling inst_matches_final
	%	on higher-order recursive insts.)
	%
:- pred pred_inst_matches_2(pred_inst_info, pred_inst_info, maybe(type),
		inst_match_info, inst_match_info).
:- mode pred_inst_matches_2(in, in, in, in, out) is semidet.

pred_inst_matches_2(pred_inst_info(PredOrFunc, ModesA, Det),
		pred_inst_info(PredOrFunc, ModesB, Det),
		MaybeType) -->
	{ maybe_get_higher_order_arg_types(MaybeType, length(ModesA),
		MaybeTypes) },
	pred_inst_argmodes_matches(ModesA, ModesB, MaybeTypes).

	% pred_inst_matches_argmodes(ModesA, ModesB, ModuleInfo, Expansions):
	% succeeds if the initial insts of ModesB specify at least as
	% much information as, and the same binding as, the initial
	% insts of ModesA; and the final insts of ModesA specify at
	% least as much information as, and the same binding as, the
	% final insts of ModesB.  Any inst pairs in Expansions are assumed
	% to match_final each other.
	%
:- pred pred_inst_argmodes_matches(list(mode), list(mode), list(maybe(type)),
				inst_match_info, inst_match_info).
:- mode pred_inst_argmodes_matches(in, in, in, in, out) is semidet.

pred_inst_argmodes_matches([], [], []) --> [].
pred_inst_argmodes_matches([ModeA|ModeAs], [ModeB|ModeBs],
		[MaybeType | MaybeTypes]) -->
	ModuleInfo =^ module_info,
	{ mode_get_insts(ModuleInfo, ModeA, InitialA, FinalA) },
	{ mode_get_insts(ModuleInfo, ModeB, InitialB, FinalB) },
	inst_matches_final_2(InitialB, InitialA, MaybeType),
	inst_matches_final_2(FinalA, FinalB, MaybeType),
	pred_inst_argmodes_matches(ModeAs, ModeBs, MaybeTypes).

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
	maybe(type), inst_match_info, inst_match_info).
:- mode bound_inst_list_matches_initial(in, in, in, in, out) is semidet.

bound_inst_list_matches_initial([], _, _) --> [].
bound_inst_list_matches_initial([X|Xs], [Y|Ys], MaybeType) -->
	{ X = functor(ConsIdX, ArgsX) },
	{ Y = functor(ConsIdY, ArgsY) },
	( { ConsIdX = ConsIdY } ->
		ModuleInfo =^ module_info,
		{ maybe_get_cons_id_arg_types(ModuleInfo, MaybeType, ConsIdX,
			list__length(ArgsX), MaybeTypes) },
		inst_list_matches_initial(ArgsX, ArgsY, MaybeTypes),
		bound_inst_list_matches_initial(Xs, Ys, MaybeType)
	;
		{ compare(>, ConsIdX, ConsIdY) },
			% ConsIdY does not occur in [X|Xs].
			% Hence [X|Xs] implicitly specifies `not_reached'
			% for the args of ConsIdY, and hence 
			% automatically matches_initial Y.  We just need to
			% check that [X|Xs] matches_initial Ys.
		bound_inst_list_matches_initial([X|Xs], Ys, MaybeType)
	).

:- pred inst_list_matches_initial(list(inst), list(inst), list(maybe(type)),
	inst_match_info, inst_match_info).
:- mode inst_list_matches_initial(in, in, in, in, out) is semidet.

inst_list_matches_initial([], [], []) --> [].
inst_list_matches_initial([X|Xs], [Y|Ys], [Type | Types]) -->
	inst_matches_initial_2(X, Y, Type),
	inst_list_matches_initial(Xs, Ys, Types).

%-----------------------------------------------------------------------------%

inst_expand(ModuleInfo, Inst0, Inst) :-
	( Inst0 = defined_inst(InstName) ->
		inst_lookup(ModuleInfo, InstName, Inst1),
		inst_expand(ModuleInfo, Inst1, Inst)
	;
		Inst = Inst0
	).

inst_expand_and_remove_constrained_inst_vars(ModuleInfo, Inst0, Inst) :-
	( Inst0 = defined_inst(InstName) ->
		inst_lookup(ModuleInfo, InstName, Inst1),
		inst_expand(ModuleInfo, Inst1, Inst)
	; Inst0 = constrained_inst_vars(_, Inst1) ->
		inst_expand(ModuleInfo, Inst1, Inst)
	;
		Inst = Inst0
	).

%-----------------------------------------------------------------------------%

inst_matches_final(InstA, InstB, ModuleInfo) :-
	Info0 = init_inst_match_info(ModuleInfo),
	inst_matches_final_2(InstA, InstB, no, Info0, _).

inst_matches_final(InstA, InstB, Type, ModuleInfo) :-
	Info0 = init_inst_match_info(ModuleInfo),
	inst_matches_final_2(InstA, InstB, yes(Type), Info0, _).

:- pred inst_matches_final_2(inst, inst, maybe(type),
		inst_match_info, inst_match_info).
:- mode inst_matches_final_2(in, in, in, in, out) is semidet.

inst_matches_final_2(InstA, InstB, MaybeType, Info0, Info) :-
	ThisExpansion = InstA - InstB,
	( set__member(ThisExpansion, Info0^expansions) ->
		Info = Info0
	; InstA = InstB ->
		Info = Info0
	;
		inst_expand(Info0^module_info, InstA, InstA2),
		inst_expand(Info0^module_info, InstB, InstB2),
		set__insert(Info0^expansions, ThisExpansion, Expansions1),
		inst_matches_final_3(InstA2, InstB2, MaybeType,
			Info0^expansions := Expansions1, Info)
	).

:- pred inst_matches_final_3(inst, inst, maybe(type),
		inst_match_info, inst_match_info).
:- mode inst_matches_final_3(in, in, in, in, out) is semidet.

inst_matches_final_3(any(UniqA), any(UniqB), _, I, I) :-
	unique_matches_final(UniqA, UniqB).
inst_matches_final_3(free, any(Uniq), _, I, I) :-
	% We do not yet allow `free' to match `any',
	% unless the `any' is `clobbered_any' or `mostly_clobbered_any'.
	% Among other things, changing this would break compare_inst
	% in modecheck_call.m.
	( Uniq = clobbered ; Uniq = mostly_clobbered ).
inst_matches_final_3(free, free, _, I, I).
inst_matches_final_3(bound(UniqA, ListA), any(UniqB), _, Info, Info) :-
	unique_matches_final(UniqA, UniqB),
	bound_inst_list_matches_uniq(ListA, UniqB, Info^module_info),
	% We do not yet allow `free' to match `any'.
	% Among other things, changing this would break compare_inst
	% in modecheck_call.m.
	bound_inst_list_is_ground_or_any(ListA, Info^module_info).
inst_matches_final_3(bound(UniqA, ListA), bound(UniqB, ListB), MaybeType,
		Info0, Info) :-
	unique_matches_final(UniqA, UniqB),
	bound_inst_list_matches_final(ListA, ListB, MaybeType, Info0, Info).
inst_matches_final_3(bound(UniqA, ListA), ground(UniqB, none), _,
		Info, Info) :-
	unique_matches_final(UniqA, UniqB),
	bound_inst_list_is_ground(ListA, Info^module_info),
	bound_inst_list_matches_uniq(ListA, UniqB, Info^module_info).
inst_matches_final_3(ground(UniqA, GroundInstInfoA), any(UniqB), _,
		Info, Info) :-
	\+ ground_inst_info_is_nonstandard_func_mode(GroundInstInfoA,
		Info^module_info),
	unique_matches_final(UniqA, UniqB).
inst_matches_final_3(ground(UniqA, GroundInstInfoA), bound(UniqB, ListB),
		MaybeType, Info, Info) :-
	\+ ground_inst_info_is_nonstandard_func_mode(GroundInstInfoA,
		Info^module_info),
	unique_matches_final(UniqA, UniqB),
	bound_inst_list_is_ground(ListB, Info^module_info),
	uniq_matches_bound_inst_list(UniqA, ListB, Info^module_info),
	(
		MaybeType = yes(Type),
		% We can only do this check if the type is known.
		bound_inst_list_is_complete_for_type(set__init,
			Info^module_info, ListB, Type)
	;
		true
		% XXX enabling the check for bound_inst_list_is_complete
		% for type makes the mode checker too conservative in
		% the absence of alias tracking, so we currently always
		% succeed, even if this check fails.
	).
inst_matches_final_3(ground(UniqA, GroundInstInfoA),
		ground(UniqB, GroundInstInfoB), MaybeType, Info0, Info) :-
	ground_inst_info_matches_final(GroundInstInfoA, GroundInstInfoB,
		MaybeType, Info0, Info),
	unique_matches_final(UniqA, UniqB).
inst_matches_final_3(abstract_inst(_, _), any(shared), _, I, I).
inst_matches_final_3(abstract_inst(Name, ArgsA), abstract_inst(Name, ArgsB),
		_MaybeType, Info0, Info) :-
	list__duplicate(length(ArgsA), no, MaybeTypes),
		% XXX how do we get the argument types for an abstract inst?
	inst_list_matches_final(ArgsA, ArgsB, MaybeTypes, Info0, Info).
inst_matches_final_3(not_reached, _, _, I, I).
inst_matches_final_3(constrained_inst_vars(InstVarsA, InstA), InstB, MaybeType,
		Info0, Info) :-
	( InstB = constrained_inst_vars(InstVarsB, InstB1) ->
		% Constrained_inst_vars match_final only if InstVarsA contains
		% all the variables in InstVarsB
		InstVarsB `set__subset` InstVarsA,
		inst_matches_final_2(InstA, InstB1, MaybeType, Info0, Info)
	;
		inst_matches_final_2(InstA, InstB, MaybeType, Info0, Info)
	).

:- pred ground_inst_info_matches_final(ground_inst_info, ground_inst_info,
		maybe(type), inst_match_info, inst_match_info).
:- mode ground_inst_info_matches_final(in, in, in, in, out) is semidet.

ground_inst_info_matches_final(GroundInstInfoA, none, _) -->
	ModuleInfo =^ module_info,
	{ \+ ground_inst_info_is_nonstandard_func_mode(GroundInstInfoA,
		ModuleInfo) }.
ground_inst_info_matches_final(none, higher_order(PredInstB), Type) -->
	{ PredInstB = pred_inst_info(function, ArgModes, _Det) },
	{ Arity = list__length(ArgModes) },
	{ PredInstA = pred_inst_info_standard_func_mode(Arity) },
	pred_inst_matches_2(PredInstA, PredInstB, Type).
ground_inst_info_matches_final(higher_order(PredInstA),
		higher_order(PredInstB), MaybeType) -->
	pred_inst_matches_2(PredInstA, PredInstB, MaybeType).

:- pred inst_list_matches_final(list(inst), list(inst), list(maybe(type)),
		inst_match_info, inst_match_info).
:- mode inst_list_matches_final(in, in, in, in, out) is semidet.

inst_list_matches_final([], [], []) --> [].
inst_list_matches_final([ArgA | ArgsA], [ArgB | ArgsB], [Type | Types]) -->
	inst_matches_final_2(ArgA, ArgB, Type),
	inst_list_matches_final(ArgsA, ArgsB, Types).

	% Here we check that the functors in the first list are a
	% subset of the functors in the second list. 
	% (If a bound(...) inst only specifies the insts for some of
	% the constructors of its type, then it implicitly means that
	% all other constructors must have all their arguments
	% `not_reached'.)
	% The code here makes use of the fact that the bound_inst lists
	% are sorted.

:- pred bound_inst_list_matches_final(list(bound_inst), list(bound_inst),
				maybe(type), inst_match_info, inst_match_info).
:- mode bound_inst_list_matches_final(in, in, in, in, out) is semidet.

bound_inst_list_matches_final([], _, _) --> [].
bound_inst_list_matches_final([X|Xs], [Y|Ys], MaybeType) -->
	{ X = functor(ConsIdX, ArgsX) },
	{ Y = functor(ConsIdY, ArgsY) },
	( { ConsIdX = ConsIdY } ->
		ModuleInfo =^ module_info,
		{ maybe_get_cons_id_arg_types(ModuleInfo, MaybeType, ConsIdX,
			list__length(ArgsX), MaybeTypes) },
		inst_list_matches_final(ArgsX, ArgsY, MaybeTypes),
		bound_inst_list_matches_final(Xs, Ys, MaybeType)
	;
		{ compare(>, ConsIdX, ConsIdY) },
			% ConsIdY does not occur in [X|Xs].
			% Hence [X|Xs] implicitly specifies `not_reached'
			% for the args of ConsIdY, and hence 
			% automatically matches_final Y.  We just need to
			% check that [X|Xs] matches_final Ys.
		bound_inst_list_matches_final([X|Xs], Ys, MaybeType)
	).

inst_matches_binding(InstA, InstB, Type, ModuleInfo) :-
	Info0 = init_inst_match_info(ModuleInfo),
	inst_matches_binding_2(InstA, InstB, yes(Type), Info0, _).

:- pred inst_matches_binding_2(inst, inst, maybe(type), inst_match_info,
		inst_match_info).
:- mode inst_matches_binding_2(in, in, in, in, out) is semidet.

inst_matches_binding_2(InstA, InstB, MaybeType, Info0, Info) :-
	ThisExpansion = InstA - InstB,
	( set__member(ThisExpansion, Info0^expansions) ->
		Info = Info0
	;
		inst_expand_and_remove_constrained_inst_vars(Info0^module_info,
			InstA, InstA2),
		inst_expand_and_remove_constrained_inst_vars(Info0^module_info,
			InstB, InstB2),
		set__insert(Info0^expansions, ThisExpansion, Expansions1),
		inst_matches_binding_3(InstA2, InstB2, MaybeType,
			Info0^expansions := Expansions1, Info)
	).

:- pred inst_matches_binding_3(inst, inst, maybe(type), inst_match_info,
		inst_match_info).
:- mode inst_matches_binding_3(in, in, in, in, out) is semidet.

% Note that `any' is *not* considered to match `any'.
inst_matches_binding_3(free, free, _, I, I).
inst_matches_binding_3(bound(_UniqA, ListA), bound(_UniqB, ListB), MaybeType,
		Info0, Info) :-
	bound_inst_list_matches_binding(ListA, ListB, MaybeType, Info0, Info).
inst_matches_binding_3(bound(_UniqA, ListA), ground(_UniqB, none), _,
		Info, Info) :-
	bound_inst_list_is_ground(ListA, Info^module_info).
inst_matches_binding_3(ground(_UniqA, _), bound(_UniqB, ListB), MaybeType,
		Info, Info) :-
	bound_inst_list_is_ground(ListB, Info^module_info),
	(
		MaybeType = yes(Type),
		% We can only do this check if the type is known.
		bound_inst_list_is_complete_for_type(set__init,
			Info^module_info, ListB, Type)
	;
		true
		% XXX enabling the check for bound_inst_list_is_complete
		% for type makes the mode checker too conservative in
		% the absence of alias tracking, so we currently always
		% succeed, even if this check fails.
	).
inst_matches_binding_3(ground(_UniqA, GroundInstInfoA),
		ground(_UniqB, GroundInstInfoB), MaybeType, Info, Info) :-
	ground_inst_info_matches_binding(GroundInstInfoA, GroundInstInfoB,
		MaybeType, Info^module_info).
inst_matches_binding_3(abstract_inst(Name, ArgsA), abstract_inst(Name, ArgsB),
		_MaybeType, Info0, Info) :-
	list__duplicate(length(ArgsA), no, MaybeTypes),
		% XXX how do we get the argument types for an abstract inst?
	inst_list_matches_binding(ArgsA, ArgsB, MaybeTypes, Info0, Info).
inst_matches_binding_3(not_reached, _, _, I, I).

:- pred ground_inst_info_matches_binding(ground_inst_info, ground_inst_info,
		maybe(type), module_info).
:- mode ground_inst_info_matches_binding(in, in, in, in) is semidet.

ground_inst_info_matches_binding(_, none, _, _).
ground_inst_info_matches_binding(none, higher_order(PredInstB), MaybeType,
		ModuleInfo) :-
	PredInstB = pred_inst_info(function, ArgModes, _Det),
	Arity = list__length(ArgModes),
	PredInstA = pred_inst_info_standard_func_mode(Arity),
	pred_inst_matches_1(PredInstA, PredInstB, MaybeType, ModuleInfo).
ground_inst_info_matches_binding(higher_order(PredInstA),
		higher_order(PredInstB), MaybeType, ModuleInfo) :-
	pred_inst_matches_1(PredInstA, PredInstB, MaybeType, ModuleInfo).

:- pred inst_list_matches_binding(list(inst), list(inst), list(maybe(type)),
		inst_match_info, inst_match_info).
:- mode inst_list_matches_binding(in, in, in, in, out) is semidet.

inst_list_matches_binding([], [], []) --> [].
inst_list_matches_binding([ArgA | ArgsA], [ArgB | ArgsB],
		[MaybeType | MaybeTypes]) -->
	inst_matches_binding_2(ArgA, ArgB, MaybeType),
	inst_list_matches_binding(ArgsA, ArgsB, MaybeTypes).

	% Here we check that the functors in the first list are a
	% subset of the functors in the second list. 
	% (If a bound(...) inst only specifies the insts for some of
	% the constructors of its type, then it implicitly means that
	% all other constructors must have all their arguments
	% `not_reached'.)
	% The code here makes use of the fact that the bound_inst lists
	% are sorted.

:- pred bound_inst_list_matches_binding(list(bound_inst), list(bound_inst),
			maybe(type), inst_match_info, inst_match_info).
:- mode bound_inst_list_matches_binding(in, in, in, in, out) is semidet.

bound_inst_list_matches_binding([], _, _) --> [].
bound_inst_list_matches_binding([X|Xs], [Y|Ys], MaybeType) -->
	{ X = functor(ConsIdX, ArgsX) },
	{ Y = functor(ConsIdY, ArgsY) },
	( { ConsIdX = ConsIdY } ->
		ModuleInfo =^ module_info,
		{ maybe_get_cons_id_arg_types(ModuleInfo, MaybeType, ConsIdX,
			list__length(ArgsX), MaybeTypes) },
		inst_list_matches_binding(ArgsX, ArgsY, MaybeTypes),
		bound_inst_list_matches_binding(Xs, Ys, MaybeType)
	;
		{ compare(>, ConsIdX, ConsIdY) },
			% ConsIdX does not occur in [X|Xs].
			% Hence [X|Xs] implicitly specifies `not_reached'
			% for the args of ConsIdY, and hence 
			% automatically matches_binding Y.  We just need to
			% check that [X|Xs] matches_binding Ys.
		bound_inst_list_matches_binding([X|Xs], Ys, MaybeType)
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
inst_is_clobbered(ModuleInfo, constrained_inst_vars(_, Inst)) :-
	inst_is_clobbered(ModuleInfo, Inst).
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
inst_is_free(ModuleInfo, constrained_inst_vars(_, Inst)) :-
	inst_is_free(ModuleInfo, Inst).
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
inst_is_bound(ModuleInfo, constrained_inst_vars(_, Inst)) :-
	inst_is_bound(ModuleInfo, Inst).
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
inst_is_bound_to_functors(ModuleInfo, constrained_inst_vars(_, Inst),
		Functors) :-
	inst_is_bound_to_functors(ModuleInfo, Inst, Functors).
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
	Inst = constrained_inst_vars(_, Inst2),
	inst_is_ground_2(ModuleInfo, Inst2, Expansions0, Expansions).
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
	Inst = constrained_inst_vars(_, Inst2),
	inst_is_ground_or_any_2(ModuleInfo, Inst2, Expansions0, Expansions).
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
	Inst = constrained_inst_vars(_, Inst2),
	inst_is_unique_2(ModuleInfo, Inst2, Expansions0, Expansions).
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
	Inst = constrained_inst_vars(_, Inst2),
	inst_is_mostly_unique_2(ModuleInfo, Inst2, Expansions0, Expansions).
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
	Inst = constrained_inst_vars(_, Inst2),
	inst_is_not_partly_unique_2(ModuleInfo, Inst2, Expansions0, Expansions).
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
	Inst = constrained_inst_vars(_, Inst2),
	inst_is_not_fully_unique_2(ModuleInfo, Inst2, Expansions0, Expansions).
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

:- type inst_names == set(inst_name).

:- pred inst_contains_instname_2(inst, module_info, inst_name, bool,
		inst_names, inst_names).
:- mode inst_contains_instname_2(in, in, in, out, in, out) is det.

inst_contains_instname_2(abstract_inst(_, _), _, _, no, Expns, Expns).
inst_contains_instname_2(any(_), _, _, no, Expns, Expns).
inst_contains_instname_2(free, _, _, no, Expns, Expns).
inst_contains_instname_2(free(_T), _, _, no, Expns, Expns).
inst_contains_instname_2(ground(_Uniq, _), _, _, no, Expns, Expns).
inst_contains_instname_2(inst_var(_), _, _, no, Expns, Expns).
inst_contains_instname_2(not_reached, _, _, no, Expns, Expns).
inst_contains_instname_2(constrained_inst_vars(_, Inst), ModuleInfo, InstName,
		Result, Expansions0, Expansions) :-
	inst_contains_instname_2(Inst, ModuleInfo, InstName, Result,
		Expansions0, Expansions).
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
		inst_name, bool, inst_names, inst_names).
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
		inst_names, inst_names).
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
inst_contains_inst_var(ground(_Uniq, GroundInstInfo), InstVar) :-
	GroundInstInfo = higher_order(pred_inst_info(_PredOrFunc, Modes, _Det)),
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
