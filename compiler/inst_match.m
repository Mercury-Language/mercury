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

:- pred inst_expand(module_info, inst, inst).
:- mode inst_expand(in, in, out) is det.

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

:- implementation.
:- import_module list, set, std_util, require, mode_util.

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
inst_matches_initial_3(bound(_List), free, _, _).
inst_matches_initial_3(bound(ListA), bound(ListB), ModuleInfo, Expansions) :-
	bound_inst_list_matches_initial(ListA, ListB, ModuleInfo, Expansions).
inst_matches_initial_3(bound(List), ground, ModuleInfo, _) :-
	bound_inst_list_is_ground(List, ModuleInfo).
inst_matches_initial_3(bound(List), abstract_inst(_,_), ModuleInfo, _) :-
	bound_inst_list_is_ground(List, ModuleInfo).
inst_matches_initial_3(ground, free, _, _).
inst_matches_initial_3(ground, bound(_List), _, _ModuleInfo) :-
	fail.	% XXX BUG! should fail only if 
		% List does not include all the constructors for the type,
		% or if List contains some not_reached insts.
		% Should succeed if List contains all the constructors
		% for the type.  Problem is we don't know what the type was :-(
inst_matches_initial_3(ground, ground, _, _).
inst_matches_initial_3(ground, abstract_inst(_,_), _, _) :-
		% I don't know what this should do.
	error("inst_matches_initial(ground, abstract_inst) == ??").
inst_matches_initial_3(abstract_inst(_,_), free, _, _).
inst_matches_initial_3(abstract_inst(Name, ArgsA), abstract_inst(Name, ArgsB),
				ModuleInfo, Expansions) :-
	inst_list_matches_initial(ArgsA, ArgsB, ModuleInfo, Expansions).
inst_matches_initial_3(not_reached, _, _, _).

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
inst_matches_final_3(bound(ListA), bound(ListB), ModuleInfo, Expansions) :-
	bound_inst_list_matches_final(ListA, ListB, ModuleInfo, Expansions).
inst_matches_final_3(bound(ListA), ground, ModuleInfo, _Expansions) :-
	bound_inst_list_is_ground(ListA, ModuleInfo).
inst_matches_final_3(ground, bound(ListB), ModuleInfo, _Expansions) :-
	bound_inst_list_is_ground(ListB, ModuleInfo).
		% XXX BUG! Should fail if there are not_reached
		% insts in ListB, or if ListB does not contain a complete list
		% of all the constructors for the type in question.
	%%% error("not implemented: `ground' matches_final `bound(...)'").
inst_matches_final_3(ground, ground, _, _).
inst_matches_final_3(abstract_inst(Name, ArgsA), abstract_inst(Name, ArgsB),
		ModuleInfo, Expansions) :-
	inst_list_matches_final(ArgsA, ArgsB, ModuleInfo, Expansions).
inst_matches_final_3(not_reached, _, _, _).

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
