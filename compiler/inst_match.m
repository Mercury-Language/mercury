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
%
%-----------------------------------------------------------------------------%

:- module inst_match.
:- interface.
:- import_module prog_io, hlds.

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
	%	information as InstA, and in those parts where they
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

	% It might be a good idea to fold inst_matches_initial and
	% inst_matches_final into a single predicate inst_matches(When, ...)
	% where When is either `initial' or `final'.

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

:- pred abstractly_unify_inst(is_live, inst, inst, module_info,
				inst, determinism, module_info).
:- mode abstractly_unify_inst(in, in, in, in, out, out, out) is semidet.

	% Compute the inst that results from abstractly unifying two variables.

:- pred abstractly_unify_inst_functor(is_live, inst, const, list(inst),
				list(is_live), module_info, inst, module_info).
:- mode abstractly_unify_inst_functor(in, in, in, in, in, in, out, out)
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
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module list, set, map, std_util, require, mode_util.

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
	% occur in Expansions.

inst_matches_initial_3(free, free, _, _).
inst_matches_initial_3(bound(_Uniq, _List), free, _, _).
inst_matches_initial_3(bound(UniqA, ListA), bound(UniqB, ListB), ModuleInfo,
		Expansions) :-
	unique_matches_initial(UniqA, UniqB),
	bound_inst_list_matches_initial(ListA, ListB, ModuleInfo, Expansions).
inst_matches_initial_3(bound(UniqA, List), ground(UniqB, no), ModuleInfo, _) :-
	unique_matches_initial(UniqA, UniqB),
	bound_inst_list_is_ground(List, ModuleInfo),
	( UniqB = unique ->
		bound_inst_list_is_unique(List, ModuleInfo)
	;
		true
	).
inst_matches_initial_3(bound(Uniq, List), abstract_inst(_,_), ModuleInfo, _) :-
	Uniq = unique,
	bound_inst_list_is_ground(List, ModuleInfo),
	bound_inst_list_is_unique(List, ModuleInfo).
inst_matches_initial_3(ground(_Uniq, _PredInst), free, _, _).
inst_matches_initial_3(ground(UniqA, _), bound(UniqB, List), ModuleInfo, _) :-
	unique_matches_initial(UniqA, UniqB),
	( UniqA = shared ->
		bound_inst_list_is_shared(List, ModuleInfo)
	;
		true
	),
	fail.	% XXX BUG! should fail only if 
		% List does not include all the constructors for the type,
		% or if List contains some not_reached insts.
		% Should succeed if List contains all the constructors
		% for the type.  Problem is we don't know what the type was :-(
inst_matches_initial_3(ground(UniqA, PredInstA), ground(UniqB, PredInstB),
		_, _) :-
	pred_inst_matches_initial(PredInstA, PredInstB),
	unique_matches_initial(UniqA, UniqB).
inst_matches_initial_3(ground(_UniqA, no), abstract_inst(_,_), _, _) :-
		% I don't know what this should do.
		% Abstract insts aren't really supported.
	error("inst_matches_initial(ground, abstract_inst) == ??").
inst_matches_initial_3(abstract_inst(_,_), free, _, _).
inst_matches_initial_3(abstract_inst(Name, ArgsA), abstract_inst(Name, ArgsB),
				ModuleInfo, Expansions) :-
	inst_list_matches_initial(ArgsA, ArgsB, ModuleInfo, Expansions).
inst_matches_initial_3(not_reached, _, _, _).

:- pred pred_inst_matches_initial(maybe(pred_inst_info), maybe(pred_inst_info)).
:- mode pred_inst_matches_initial(in, in) is semidet.

pred_inst_matches_initial(no, no).
pred_inst_matches_initial(yes(_), no).
pred_inst_matches_initial(yes(PredInstA), yes(PredInstB)) :-
	PredInstA = PredInstB.
	% We require higher-order pred insts to match exactly;
	% this requirement may be too strict and hence may need to be relaxed.

:- pred unique_matches_initial(uniqueness, uniqueness).
:- mode unique_matches_initial(in, in) is semidet.

unique_matches_initial(unique, unique).
unique_matches_initial(unique, shared).
unique_matches_initial(shared, shared).
unique_matches_initial(_, clobbered).

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
	X = functor(NameX, ArgsX),
	Y = functor(NameY, ArgsY),
	list__length(ArgsX, ArityX),
	list__length(ArgsY, ArityY),
	( NameX = NameY, ArityX = ArityY ->
		inst_list_matches_initial(ArgsX, ArgsY, ModuleInfo, Expansions),
		bound_inst_list_matches_initial(Xs, Ys, ModuleInfo, Expansions)
	;
		compare(>, X, Y),
			% NameY/ArityY does not occur in [X|Xs].
			% Hence [X|Xs] implicitly specifies `not_reached'
			% for the args of NameY/ArityY, and hence 
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

inst_matches_final_3(free, free, _, _).
inst_matches_final_3(bound(UniqA, ListA), bound(UniqB, ListB), ModuleInfo,
		Expansions) :-
	unique_matches_final(UniqA, UniqB),
	bound_inst_list_matches_final(ListA, ListB, ModuleInfo, Expansions).
inst_matches_final_3(bound(UniqA, ListA), ground(UniqB, no), ModuleInfo,
		_Exps) :-
	unique_matches_final(UniqA, UniqB),
	bound_inst_list_is_ground(ListA, ModuleInfo),
	( UniqB = unique ->
		bound_inst_list_is_unique(ListA, ModuleInfo)
	;
		true
	).
inst_matches_final_3(ground(UniqA, _), bound(UniqB, ListB), ModuleInfo,
			_Exps) :-
	unique_matches_final(UniqA, UniqB),
	bound_inst_list_is_ground(ListB, ModuleInfo),
	( UniqA = shared ->
		bound_inst_list_is_shared(ListB, ModuleInfo)
	;
		true
	).
		% XXX BUG! Should fail if there are not_reached
		% insts in ListB, or if ListB does not contain a complete list
		% of all the constructors for the type in question.
	%%% error("not implemented: `ground' matches_final `bound(...)'").
inst_matches_final_3(ground(UniqA, PredInstA), ground(UniqB, PredInstB),
		_, _) :-
	pred_inst_matches_final(PredInstA, PredInstB),
	unique_matches_final(UniqA, UniqB).
inst_matches_final_3(abstract_inst(Name, ArgsA), abstract_inst(Name, ArgsB),
		ModuleInfo, Expansions) :-
	inst_list_matches_final(ArgsA, ArgsB, ModuleInfo, Expansions).
inst_matches_final_3(not_reached, _, _, _).

:- pred pred_inst_matches_final(maybe(pred_inst_info), maybe(pred_inst_info)).
:- mode pred_inst_matches_final(in, in) is semidet.

pred_inst_matches_final(no, no).
pred_inst_matches_final(yes(_), no).
pred_inst_matches_final(yes(PredInstA), yes(PredInstB)) :-
	PredInstA = PredInstB.
	% We require higher-order pred insts to match exactly;
	% this requirement may be too strict and hence may need to be relaxed.

:- pred unique_matches_final(uniqueness, uniqueness).
:- mode unique_matches_final(in, in) is semidet.

unique_matches_final(unique, unique).
unique_matches_final(unique, shared).
unique_matches_final(shared, shared).
unique_matches_final(_, clobbered).

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
	X = functor(NameX, ArgsX),
	Y = functor(NameY, ArgsY),
	list__length(ArgsX, ArityX),
	list__length(ArgsY, ArityY),
	( NameX = NameY, ArityX = ArityY ->
		inst_list_matches_final(ArgsX, ArgsY, ModuleInfo, Expansions),
		bound_inst_list_matches_final(Xs, Ys, ModuleInfo, Expansions)
	;
		compare(>, X, Y),
			% NameY/ArityY does not occur in [X|Xs].
			% Hence [X|Xs] implicitly specifies `not_reached'
			% for the args of NameY/ArityY, and hence 
			% automatically matches_final Y.  We just need to
			% check that [X|Xs] matches_final Ys.
		bound_inst_list_matches_final([X|Xs], Ys, ModuleInfo,
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
			Inst = MergedInst
		;
			Inst = defined_inst(merge_inst(InstA, InstB))
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
		inst_merge_2(InstA, InstB, ModuleInfo1, Inst, ModuleInfo2),

			% now update the value associated with ThisInstPair
		module_info_insts(ModuleInfo2, InstTable2),
		inst_table_get_merge_insts(InstTable2, MergeInstTable2),
		map__set(MergeInstTable2, ThisInstPair, known(Inst),
			MergeInstTable3),
		inst_table_set_merge_insts(InstTable2, MergeInstTable3,
			InstTable3),
		module_info_set_insts(ModuleInfo2, InstTable3, ModuleInfo)
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

inst_merge_3(free, free, M, free, M).
inst_merge_3(bound(UniqA, ListA), bound(UniqB, ListB), ModuleInfo0,
		bound(Uniq, List), ModuleInfo) :-
	merge_uniq(UniqA, UniqB, Uniq),
	bound_inst_list_merge(ListA, ListB, ModuleInfo0, List, ModuleInfo).
inst_merge_3(bound(UniqA, ListA), ground(UniqB, _), ModuleInfo,
		ground(Uniq, no), ModuleInfo) :-
	merge_uniq(UniqA, UniqB, Uniq),
	bound_inst_list_is_ground(ListA, ModuleInfo).
inst_merge_3(ground(UniqA, _), bound(UniqB, ListB), ModuleInfo,
		ground(Uniq, no), ModuleInfo) :-
	merge_uniq(UniqA, UniqB, Uniq),
	bound_inst_list_is_ground(ListB, ModuleInfo).
inst_merge_3(ground(UniqA, PredA), ground(UniqB, PredB), M,
		ground(Uniq, Pred), M) :-
	% for higher order pred insts,
	% we require the pred_inst_info in each branch to
	% be identical - this requirement may perhaps be more
	% restrictive than it needs to be
	(
		PredA = PredB
	->
		Pred = PredA
	;	
		Pred = no
	),
	merge_uniq(UniqA, UniqB, Uniq).
inst_merge_3(abstract_inst(Name, ArgsA), abstract_inst(Name, ArgsB),
		ModuleInfo0, abstract_inst(Name, Args), ModuleInfo) :-
	inst_list_merge(ArgsA, ArgsB, ModuleInfo0, Args, ModuleInfo).
inst_merge_3(not_reached, Inst, M, Inst, M).

:- pred merge_uniq(uniqueness, uniqueness, uniqueness).
:- mode merge_uniq(in, in, out) is det.

	% if var was clobbered on either branch, the result is clobbered
	% otherwise if var was shared on either branch, the result is shared
	% otherwise var was unique on both branches and the result is unique

merge_uniq(shared, shared, shared).
merge_uniq(shared, unique, shared).
merge_uniq(shared, clobbered, clobbered).
merge_uniq(unique, shared, shared).
merge_uniq(unique, unique, unique).
merge_uniq(unique, clobbered, clobbered).
merge_uniq(clobbered, _, clobbered).

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
		X = functor(NameX, ArgsX),
		Y = functor(NameY, ArgsY),
		list__length(ArgsX, ArityX),
		list__length(ArgsY, ArityY),
		( NameX = NameY, ArityX = ArityY ->
			inst_list_merge(ArgsX, ArgsY, ModuleInfo0,
					Args, ModuleInfo1),
			Z = functor(NameX, Args),
			Zs = [Z | Zs1],
			bound_inst_list_merge(Xs1, Ys1, ModuleInfo1,
				Zs1, ModuleInfo)
		; compare(<, X, Y) ->
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

abstractly_unify_inst(Live, InstA, InstB, ModuleInfo0, Inst, Det, ModuleInfo) :-
		% check whether this pair of insts is already in
		% the unify_insts table
	ThisInstPair = unify_inst_pair(Live, InstA, InstB),
	module_info_insts(ModuleInfo0, InstTable0),
	inst_table_get_unify_insts(InstTable0, UnifyInsts0),
	( map__search(UnifyInsts0, ThisInstPair, Result) ->
		( Result = known(UnifyInst, UnifyDet) ->
			Inst = UnifyInst,
			Det = UnifyDet
		;
			Inst = defined_inst(unify_inst(Live, InstA, InstB)),
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
		abstractly_unify_inst_2(Live, InstA2, InstB2, ModuleInfo1,
			Inst, Det, ModuleInfo2),
			% now update the value associated with ThisInstPair
		module_info_insts(ModuleInfo2, InstTable2),
		inst_table_get_unify_insts(InstTable2, UnifyInsts2),
		map__set(UnifyInsts2, ThisInstPair, known(Inst, Det),
			UnifyInsts),
		inst_table_set_unify_insts(InstTable2, UnifyInsts, InstTable),
		module_info_set_insts(ModuleInfo2, InstTable, ModuleInfo)
	).

:- pred abstractly_unify_inst_2(is_live, inst, inst, module_info,
				inst, determinism, module_info).
:- mode abstractly_unify_inst_2(in, in, in, in, out, out, out) is semidet.

abstractly_unify_inst_2(IsLive, InstA, InstB, ModuleInfo0, Inst, Det,
		ModuleInfo) :-
	( InstB = not_reached ->
		Inst = InstA,
		Det = det,
		ModuleInfo = ModuleInfo0
	;
		abstractly_unify_inst_3(IsLive, InstA, InstB, ModuleInfo0,
			Inst, Det, ModuleInfo)
	).

	% Abstractly unify two expanded insts.
	% The is_live parameter is `live' iff *both* insts are live.
	% Given the two insts to be unified, this produces
	% a resulting inst and a determinism for the unification.

:- pred abstractly_unify_inst_3(is_live, inst, inst, module_info,
				inst, determinism, module_info).
:- mode abstractly_unify_inst_3(in, in, in, in, out, out, out) is semidet.

:- abstractly_unify_inst_3(A, B, C, _, _, _, _) when A and B and C.

abstractly_unify_inst_3(live, not_reached, _,		M, not_reached, det, M).

% abstractly_unify_inst_3(live, free,	free,		_, _, _, _) :- fail.

abstractly_unify_inst_3(live, free,	bound(UniqY, List0),	M0,
					bound(Uniq, List), det, M) :-
	unify_uniq(live, unique, UniqY, Uniq),
	bound_inst_list_is_ground(List0, M),
	( Uniq = unique ->
		make_shared_bound_inst_list(List0, M0, List, M)
	;
		List = List0, M = M0
	).

abstractly_unify_inst_3(live, free,	ground(UniqY, PredInst),	M,
					ground(Uniq, PredInst), det, M) :-
	unify_uniq(live, unique, UniqY, Uniq).

% abstractly_unify_inst_3(live, free,	abstract_inst(_,_), _, _, _, _) :- fail.

abstractly_unify_inst_3(live,		bound(UniqY, List0), free,	M0,
					bound(Uniq, List), det,	 M) :-
	unify_uniq(live, unique, UniqY, Uniq),
	bound_inst_list_is_ground(List0, M0),
	make_shared_bound_inst_list(List0, M0, List, M).

abstractly_unify_inst_3(live, bound(UniqX, ListX), bound(UniqY, ListY), M0,
				bound(Uniq, List), Det, M) :-
	unify_uniq(live, UniqX, UniqY, Uniq),
	abstractly_unify_bound_inst_list(live, ListX, ListY, M0, List, Det, M).

abstractly_unify_inst_3(live, bound(UniqX, BoundInsts0), ground(UniqY, _),
		M0, bound(Uniq, BoundInsts), semidet, M) :-
	unify_uniq(live, UniqX, UniqY, Uniq),
	make_ground_bound_inst_list(BoundInsts0, live, UniqY, M0,
			BoundInsts, M).

/*** abstract insts not supported
abstractly_unify_inst_3(live, bound(Uniq, List), abstract_inst(_,_), M,
					ground(shared), semidet, M) :-
	unify_uniq(live, unique, UniqY, Uniq),
	bound_inst_list_is_ground(List, M).
***/

abstractly_unify_inst_3(live, ground(Uniq0, yes(PredInst)), free, M,
				ground(Uniq, yes(PredInst)), det, M) :-
	unify_uniq(live, unique, Uniq0, Uniq).

abstractly_unify_inst_3(live, ground(UniqX, yes(_)), bound(UniqY, BoundInsts0),
		M0, bound(Uniq, BoundInsts), semidet, M) :-
	unify_uniq(live, UniqX, UniqY, Uniq),
	make_ground_bound_inst_list(BoundInsts0, live, UniqX, M0,
			BoundInsts, M).

abstractly_unify_inst_3(live, ground(UniqA, yes(PredInstA)),
				ground(UniqB, yes(PredInstB)), M,
				ground(Uniq, PredInst), semidet, M) :-
	% this might be too restrictive
	( PredInstA = PredInstB ->
		PredInst = yes(PredInstA)
	;
		PredInst = no
	),
	unify_uniq(live, UniqA, UniqB, Uniq).

abstractly_unify_inst_3(live, ground(Uniq, no), Inst0, M0,
				Inst, Det, M) :-
	( inst_is_free(M0, Inst0) ->
		Det = det
	;
		Det = semidet
	),
	make_ground_inst(Inst0, live, Uniq, M0, Inst, M).

% abstractly_unify_inst_3(live, abstract_inst(_,_), free,	_, _, _, _)
%	:- fail.

/*** abstract insts not supported
abstractly_unify_inst_3(live, abstract_inst(_,_), bound(Uniq, List), ModuleInfo,
				ground(shared, no), semidet, ModuleInfo) :-
	check_not_clobbered(Uniq),
	bound_inst_list_is_ground(List, ModuleInfo).

abstractly_unify_inst_3(live, abstract_inst(_,_), ground(Uniq, no), M,
				ground(shared, no), semidet, M) :-
	check_not_clobbered(Uniq).

abstractly_unify_inst_3(live, abstract_inst(Name, ArgsA),
			abstract_inst(Name, ArgsB), ModuleInfo0,
			abstract_inst(Name, Args), Det, ModuleInfo) :-
	abstractly_unify_inst_list(ArgsA, ArgsB, live, ModuleInfo0,
		Args, Det, ModuleInfo).
***/

abstractly_unify_inst_3(dead, not_reached, _, M, not_reached, det, M).

abstractly_unify_inst_3(dead, free, Inst, M, Inst, det, M).

abstractly_unify_inst_3(dead, bound(UniqX, List), free, ModuleInfo,
				bound(Uniq, List), det, ModuleInfo) :-
	unify_uniq(dead, UniqX, unique, Uniq).

abstractly_unify_inst_3(dead, bound(UniqX, ListX), bound(UniqY, ListY), M0,
			bound(Uniq, List), Det, M) :-
	unify_uniq(dead, UniqX, UniqY, Uniq),
	abstractly_unify_bound_inst_list(dead, ListX, ListY, M0, List, Det, M).

abstractly_unify_inst_3(dead, bound(UniqX, BoundInsts0), ground(UniqY, _), M0,
			bound(Uniq, BoundInsts), semidet, M) :-
	unify_uniq(dead, UniqX, UniqY, Uniq),
	make_ground_bound_inst_list(BoundInsts0, dead, UniqY, M0,
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

abstractly_unify_inst_3(dead, ground(Uniq, yes(PredInst)), free, M,
				ground(Uniq, yes(PredInst)), det, M).

abstractly_unify_inst_3(dead, ground(UniqA, yes(_)), bound(UniqB, BoundInsts0),
				M0, bound(Uniq, BoundInsts), semidet, M) :-
	unify_uniq(dead, UniqA, UniqB, Uniq),
	make_ground_bound_inst_list(BoundInsts0, dead, UniqA, M0,
					BoundInsts, M).

abstractly_unify_inst_3(dead, ground(UniqA, yes(PredInstA)),
				ground(UniqB, yes(PredInstB)), M,
				ground(Uniq, PredInst), det, M) :-
	% this might be too restrictive
	( PredInstA = PredInstB ->
		PredInst = yes(PredInstA)
	;
		PredInst = no
	),
	unify_uniq(dead, UniqA, UniqB, Uniq).

abstractly_unify_inst_3(dead, ground(Uniq, no),	Inst0,	M0, Inst, Det, M) :-
	( inst_is_free(M0, Inst0) ->
		Det = det
	;
		Det = semidet
	),
	make_ground_inst(Inst0, dead, Uniq, M0, Inst, M).

/***** abstract insts aren't really supported
abstractly_unify_inst_3(dead, abstract_inst(N,As), bound(List), ModuleInfo, 
						Result, Det, ModuleInfo) :-
	( bound_inst_list_is_ground(List, ModuleInfo) ->
		Result = bound(List),
		Det = semidet
	; bound_inst_list_is_free(List, ModuleInfo) ->
		Result = abstract_inst(N,As),
		Det = det
	;
		fail
	).

abstractly_unify_inst_3(dead, abstract_inst(_,_), ground, ModuleInfo,
		ground, semidet, ModuleInfo).

abstractly_unify_inst_3(dead, abstract_inst(Name, ArgsA),
			abstract_inst(Name, ArgsB), ModuleInfo0,
			abstract_inst(Name, Args), Det, ModuleInfo) :-
	abstractly_unify_inst_list(ArgsA, ArgsB, dead, ModuleInfo0,
			Args, Det, ModuleInfo).

*****/

%-----------------------------------------------------------------------------%

:- pred unify_uniq(is_live, uniqueness, uniqueness, uniqueness).
:- mode unify_uniq(in, in, in, out) is det.

unify_uniq(_, shared, shared, shared).
unify_uniq(_, shared, unique, shared).
unify_uniq(live, shared, clobbered, _) :- error("unify_uniq: clobbered").
unify_uniq(dead, shared, clobbered, clobbered). % XXX needed for proc call
unify_uniq(_, unique, shared, shared).
unify_uniq(live, unique, unique, shared).
unify_uniq(dead, unique, unique, unique).
unify_uniq(live, unique, clobbered, _) :- error("unify_uniq: clobbered").
unify_uniq(dead, unique, clobbered, clobbered). % XXX needed for proc call
unify_uniq(live, clobbered, _, _) :- error("unify_uniq: clobbered").
unify_uniq(dead, clobbered, _, clobbered). % XXX needed for proc call

:- pred check_not_clobbered(uniqueness).
:- mode check_not_clobbered(in) is det.

	% sanity check
check_not_clobbered(Uniq) :-
	( Uniq = clobbered ->
		error("abstractly_unify_inst_3: clobbered inst")
	;
		true
	).

%-----------------------------------------------------------------------------%

	% Abstractly unify two inst lists.

:- pred abstractly_unify_inst_list(list(inst), list(inst), is_live, module_info,
					list(inst), determinism, module_info).
:- mode abstractly_unify_inst_list(in, in, in, in, out, out, out) is semidet.

abstractly_unify_inst_list([], [], _, M, [], det, M).
abstractly_unify_inst_list([X|Xs], [Y|Ys], Live, ModuleInfo0,
				[Z|Zs], Det, ModuleInfo) :-
	abstractly_unify_inst(Live, X, Y, ModuleInfo0, Z, Det1, ModuleInfo1),
	abstractly_unify_inst_list(Xs, Ys, Live, ModuleInfo1, Zs, Det2,
		ModuleInfo),
	( Det1 = semidet ->
		Det = semidet
	;
		Det = Det2
	).

%-----------------------------------------------------------------------------%

	% This is the abstract unification operation which
	% unifies a variable (or rather, it's instantiatedness)
	% with a functor.

abstractly_unify_inst_functor(Live, InstA, Name, ArgInsts, ArgLives,
		ModuleInfo0, Inst, ModuleInfo) :-
	inst_expand(ModuleInfo0, InstA, InstA2),
	abstractly_unify_inst_functor_2(Live, InstA2, Name, ArgInsts, ArgLives,
			ModuleInfo0, Inst, ModuleInfo).

:- pred abstractly_unify_inst_functor_2(is_live, inst, const, list(inst),
				list(is_live), module_info, inst, module_info).
:- mode abstractly_unify_inst_functor_2(in, in, in, in, in, in, out, out)
	is semidet.

abstractly_unify_inst_functor_2(live, not_reached, _, _, _, M, not_reached, M).

abstractly_unify_inst_functor_2(live, free, Name, Args0, ArgLives, ModuleInfo0,
			bound(unique, [functor(Name, Args)]), ModuleInfo) :-
	inst_list_is_ground_or_dead(Args0, ArgLives, ModuleInfo0),
	maybe_make_shared_inst_list(Args0, ArgLives, ModuleInfo0,
			Args, ModuleInfo).

abstractly_unify_inst_functor_2(live, bound(Uniq, ListX), Name, Args,
				ArgLives, M0, bound(Uniq, List), M) :-
	abstractly_unify_bound_inst_list_lives(ListX, Name, Args, ArgLives,
						M0, List, M).

abstractly_unify_inst_functor_2(live, ground(Uniq, _), Name, ArgInsts,
		_ArgLives, M0, Inst, M) :-
	make_ground_inst_list(ArgInsts, live, Uniq, M0, GroundArgInsts, M), 
	Inst = bound(Uniq, [functor(Name, GroundArgInsts)]).

% abstractly_unify_inst_functor_2(live, abstract_inst(_,_), _, _, _, _, _, _) :-
% 	fail.

abstractly_unify_inst_functor_2(dead, not_reached, _, _, _, M, not_reached, M).

abstractly_unify_inst_functor_2(dead, free, Name, Args, _ArgLives, M,
			bound(unique, [functor(Name, Args)]), M).

abstractly_unify_inst_functor_2(dead, bound(Uniq, ListX), Name, Args,
			_ArgLives, M0, bound(Uniq, List), M) :-
	ListY = [functor(Name, Args)],
	abstractly_unify_bound_inst_list(dead, ListX, ListY, M0, List, _, M). 

abstractly_unify_inst_functor_2(dead, ground(Uniq, _), Name, ArgInsts,
		_ArgLives, M0, Inst, M) :-
	make_ground_inst_list(ArgInsts, dead, Uniq, M0, GroundArgInsts, M),
	Inst = bound(Uniq, [functor(Name, GroundArgInsts)]).

% abstractly_unify_inst_functor_2(dead, abstract_inst(_,_), _, _, _, _, _, _) :-
% 	fail.

%-----------------------------------------------------------------------------%

:- pred make_ground_inst_list(list(inst), is_live, uniqueness, module_info,
				list(inst), module_info).
:- mode make_ground_inst_list(in, in, in, in, out, out) is det.

make_ground_inst_list([], _, _, ModuleInfo, [], ModuleInfo).
make_ground_inst_list([Inst0 | Insts0], Live, Uniq, ModuleInfo0,
		[Inst | Insts], ModuleInfo) :-
	make_ground_inst(Inst0, Live, Uniq, ModuleInfo0, Inst, ModuleInfo1),
	make_ground_inst_list(Insts0, Live, Uniq, ModuleInfo1, Insts,
		ModuleInfo).

% abstractly unify an inst with `ground' and calculate the new inst
% and the determinism of the unification.

:- pred make_ground_inst(inst, is_live, uniqueness, module_info,
				inst, module_info).
:- mode make_ground_inst(in, in, in, in, out, out) is det.

make_ground_inst(not_reached, _, _, M, not_reached, M).
make_ground_inst(free, IsLive, Uniq0, M, ground(Uniq, no), M) :-
	unify_uniq(IsLive, unique, Uniq0, Uniq).
make_ground_inst(free(T), IsLive, Uniq0, M,
		defined_inst(typed_ground(Uniq, T)), M) :-
	unify_uniq(IsLive, unique, Uniq0, Uniq).
make_ground_inst(bound(Uniq0, BoundInsts0), IsLive, Uniq1, M0,
		bound(Uniq, BoundInsts), M) :-
	unify_uniq(IsLive, Uniq0, Uniq1, Uniq),
	make_ground_bound_inst_list(BoundInsts0, IsLive, Uniq1, M0,
					BoundInsts, M).
make_ground_inst(ground(Uniq0, _PredInst), IsLive, Uniq1, M,
		ground(Uniq, no), M) :-
	unify_uniq(IsLive, Uniq0, Uniq1, Uniq).
make_ground_inst(inst_var(_), _, _, _, _, _) :-
	error("free inst var").
make_ground_inst(abstract_inst(_,_), _, _, M, ground(shared, no), M).
make_ground_inst(defined_inst(InstName), IsLive, Uniq, ModuleInfo0,
			Inst, ModuleInfo) :-
		% check whether the inst name is already in the
		% ground_inst table
	module_info_insts(ModuleInfo0, InstTable0),
	inst_table_get_ground_insts(InstTable0, GroundInsts0),
	GroundInstKey = ground_inst(InstName, IsLive, Uniq),
	(
		map__search(GroundInsts0, GroundInstKey, Result)
	->
		( Result = known(GroundInst) ->
			Inst = GroundInst
		;
			Inst = defined_inst(GroundInstKey)
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
		make_ground_inst(Inst1, IsLive, Uniq, ModuleInfo1,
				Inst, ModuleInfo2),

		% now that we have determined the resulting Inst, store
		% the appropriate value `known(Inst)' in the ground_inst
		% table
		module_info_insts(ModuleInfo2, InstTable2),
		inst_table_get_ground_insts(InstTable2, GroundInsts2),
		map__set(GroundInsts2, GroundInstKey, known(Inst), GroundInsts),
		inst_table_set_ground_insts(InstTable2, GroundInsts,
			InstTable),
		module_info_set_insts(ModuleInfo2, InstTable, ModuleInfo)
	).

:- pred make_ground_bound_inst_list(list(bound_inst), is_live, uniqueness,
				module_info, list(bound_inst), module_info).
:- mode make_ground_bound_inst_list(in, in, in, in, out, out) is det.

make_ground_bound_inst_list([], _, _, ModuleInfo, [], ModuleInfo).
make_ground_bound_inst_list([Bound0 | Bounds0], IsLive, Uniq, ModuleInfo0,
			[Bound | Bounds], ModuleInfo) :-
	Bound0 = functor(Name, ArgInsts0),
	make_ground_inst_list(ArgInsts0, IsLive, Uniq, ModuleInfo0,
				ArgInsts, ModuleInfo1),
	Bound = functor(Name, ArgInsts),
	make_ground_bound_inst_list(Bounds0, IsLive, Uniq, ModuleInfo1,
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

% make an inst shared; replace all occurrences of `unique' in the
% inst with `shared'.

:- pred make_shared_inst(inst, module_info, inst, module_info).
:- mode make_shared_inst(in, in, out, out) is det.

make_shared_inst(not_reached, M, not_reached, M).
make_shared_inst(free, M, free, M).
make_shared_inst(free(T), M, free(T), M).
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
		( Result = known(SharedInst) ->
			Inst = SharedInst
		;
			Inst = defined_inst(InstName)
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
		make_shared_inst(Inst1, ModuleInfo1, Inst, ModuleInfo2),

		% now that we have determined the resulting Inst, store
		% the appropriate value `known(Inst)' in the shared_inst
		% table
		module_info_insts(ModuleInfo2, InstTable2),
		inst_table_get_shared_insts(InstTable2, SharedInsts2),
		map__set(SharedInsts2, InstName, known(Inst), SharedInsts),
		inst_table_set_shared_insts(InstTable2, SharedInsts,
			InstTable),
		module_info_set_insts(ModuleInfo2, InstTable, ModuleInfo)
	).

:- pred make_shared(uniqueness, uniqueness).
:- mode make_shared(in, out) is det.

make_shared(unique, shared).
make_shared(shared, shared).
make_shared(clobbered, clobbered).

:- pred make_shared_bound_inst_list(list(bound_inst), module_info,
					list(bound_inst), module_info).
:- mode make_shared_bound_inst_list(in, in, out, out) is det.

make_shared_bound_inst_list([], ModuleInfo, [], ModuleInfo).
make_shared_bound_inst_list([Bound0 | Bounds0], ModuleInfo0,
				[Bound | Bounds], ModuleInfo) :-
	Bound0 = functor(Name, ArgInsts0),
	make_shared_inst_list(ArgInsts0, ModuleInfo0,
				ArgInsts, ModuleInfo1),
	Bound = functor(Name, ArgInsts),
	make_shared_bound_inst_list(Bounds0, ModuleInfo1,
				Bounds, ModuleInfo).

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

%-----------------------------------------------------------------------------%

	% This code performs abstract unification of two bound(...) insts.
	% like a sorted merge operation.  If two elements have the
	% The lists of bound_inst are guaranteed to be sorted.
	% Abstract unification of two bound(...) insts proceeds
	% like a sorted merge operation.  If two elements have the
	% same functor name, they are inserted in the output list
	% iff their argument inst list can be abstractly unified.

:- pred abstractly_unify_bound_inst_list(is_live, list(bound_inst),
		list(bound_inst), module_info,
		list(bound_inst), determinism, module_info).
:- mode abstractly_unify_bound_inst_list(in, in, in, in,
		out, out, out) is semidet.

:- abstractly_unify_bound_inst_list(_, Xs, Ys, _, _, _, _)
	when Xs and Ys. % Index

abstractly_unify_bound_inst_list(_, [], [], ModuleInfo, [], det, ModuleInfo).
abstractly_unify_bound_inst_list(_, [], [_|_], M, [], semidet, M).
abstractly_unify_bound_inst_list(_, [_|_], [], M, [], semidet, M).
abstractly_unify_bound_inst_list(Live, [X|Xs], [Y|Ys], ModuleInfo0,
		L, Det, ModuleInfo) :-
	X = functor(NameX, ArgsX),
	list__length(ArgsX, ArityX),
	Y = functor(NameY, ArgsY),
	list__length(ArgsY, ArityY),
	( NameX = NameY, ArityX = ArityY ->
	    ( abstractly_unify_inst_list(ArgsX, ArgsY, Live, ModuleInfo0,
			Args, Det1, ModuleInfo1)
	    ->
		L = [functor(NameX, Args) | L1],
		abstractly_unify_bound_inst_list(Live, Xs, Ys,
					ModuleInfo1, L1, Det2, ModuleInfo),
		( Det1 = semidet ->
		    Det = semidet
		;
		    Det = Det2
		)
	    ;
		abstractly_unify_bound_inst_list(Live, Xs, Ys,
						ModuleInfo0, L, Det, ModuleInfo)
	    )
	;
	    Det = semidet,
	    ( compare(<, X, Y) ->
		abstractly_unify_bound_inst_list(Live, Xs, [Y|Ys],
						ModuleInfo0, L, _, ModuleInfo)
	    ;
		abstractly_unify_bound_inst_list(Live, [X|Xs], Ys,
						ModuleInfo0, L, _, ModuleInfo)
	    )
	).

:- pred abstractly_unify_bound_inst_list_lives(list(bound_inst), const,
	list(inst), list(is_live), module_info, list(bound_inst), module_info).
:- mode abstractly_unify_bound_inst_list_lives(in, in, in, in, in, out, out)
	is semidet.

abstractly_unify_bound_inst_list_lives([], _, _, _, ModuleInfo, [], ModuleInfo).
abstractly_unify_bound_inst_list_lives([X|Xs], NameY, ArgsY, LivesY,
		ModuleInfo0, L, ModuleInfo) :-
	X = functor(NameX, ArgsX),
	list__length(ArgsX, ArityX),
	list__length(ArgsY, ArityY),
	( 
		NameX = NameY,
		ArityX = ArityY
	->
		abstractly_unify_inst_list_lives(ArgsX, ArgsY, LivesY,
			ModuleInfo0, Args, ModuleInfo),
		L = [functor(NameX, Args)]
	;
		abstractly_unify_bound_inst_list_lives(Xs, NameY, ArgsY,
					LivesY, ModuleInfo0, L, ModuleInfo)
	).

:- pred abstractly_unify_inst_list_lives(list(inst), list(inst), list(is_live),
					module_info, list(inst), module_info).
:- mode abstractly_unify_inst_list_lives(in, in, in, in, out, out) is semidet.

abstractly_unify_inst_list_lives([], [], [], ModuleInfo, [], ModuleInfo).
abstractly_unify_inst_list_lives([X|Xs], [Y|Ys], [Live|Lives], ModuleInfo0,
		[Z|Zs], ModuleInfo) :-
	abstractly_unify_inst(Live, X, Y, ModuleInfo0, Z, _Det, ModuleInfo1),
	abstractly_unify_inst_list_lives(Xs, Ys, Lives, ModuleInfo1, Zs,
		ModuleInfo).

%-----------------------------------------------------------------------------%
