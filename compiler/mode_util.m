%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% mode_util.m - utility predicates dealing with modes and insts.

% Main author: fjh.

%-----------------------------------------------------------------------------%

:- module mode_util.

:- interface.

:- import_module hlds_module, hlds_pred, hlds_goal, hlds_data, prog_data.
:- import_module instmap.
:- import_module bool, list.

	% mode_get_insts returns the initial instantiatedness and
	% the final instantiatedness for a given mode, aborting
	% if the mode is undefined.
	%
:- pred mode_get_insts(module_info, mode, inst, inst).
:- mode mode_get_insts(in, in, out, out) is det.

	% a version of mode_get_insts which fails if the mode is undefined
:- pred mode_get_insts_semidet(module_info, mode, inst, inst).
:- mode mode_get_insts_semidet(in, in, out, out) is semidet.

	% a mode is considered input if the initial inst is bound
:- pred mode_is_input(module_info, mode).
:- mode mode_is_input(in, in) is semidet.

	% a mode is considered fully input if the inital inst is ground
:- pred mode_is_fully_input(module_info, mode).
:- mode mode_is_fully_input(in, in) is semidet.

	% a mode is considered output if the initial inst is free
	% and the final inst is bound
:- pred mode_is_output(module_info, mode).
:- mode mode_is_output(in, in) is semidet.

	% a mode is considered fully output if the inital inst is free and
	% the final inst is ground
:- pred mode_is_fully_output(module_info, mode).
:- mode mode_is_fully_output(in, in) is semidet.

	% a mode is considered unused if both initial and final insts are free
:- pred mode_is_unused(module_info, mode).
:- mode mode_is_unused(in, in) is semidet.

	% mode_to_arg_mode converts a mode (and corresponding type) to
	% an arg_mode.  A mode is a high-level notion, the normal
	% Mercury language mode.  An `arg_mode' is a low-level notion
	% used for code generation, which indicates the argument
	% passing convention (top_in, top_out, or top_unused) that
	% corresponds to that mode.  We need to know the type, not just
	% the mode, because the argument passing convention can depend
	% on the type's representation.
	%
:- pred mode_to_arg_mode(module_info, mode, type, arg_mode).
:- mode mode_to_arg_mode(in, in, in, out) is det.

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

	% Given an expanded inst and a cons_id and its arity, return the 
	% insts of the arguments of the top level functor, failing if the
	% inst could not be bound to the functor.
:- pred get_arg_insts(inst, cons_id, arity, list(inst)).
:- mode get_arg_insts(in, in, in, out) is semidet.

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

        % Given a list of bound_insts, get the corresponding list of cons_ids
        %
:- pred functors_to_cons_ids(list(bound_inst), list(cons_id)).
:- mode functors_to_cons_ids(in, out) is det.

:- pred mode_id_to_int(mode_id, int).
:- mode mode_id_to_int(in, out) is det.

:- pred mode_list_get_initial_insts(list(mode), module_info, list(inst)).
:- mode mode_list_get_initial_insts(in, in, out) is det.

:- pred mode_list_get_final_insts(list(mode), module_info, list(inst)).
:- mode mode_list_get_final_insts(in, in, out) is det.

:- pred mode_util__modes_to_uni_modes(list(mode), list(mode), module_info,
							list(uni_mode)).
:- mode mode_util__modes_to_uni_modes(in, in, in, out) is det.

	% inst_lists_to_mode_list(InitialInsts, FinalInsts, Modes):
	%	Given two lists of corresponding initial and final
	%	insts, return a list of modes which maps from the
	%	initial insts to the final insts.
:- pred inst_lists_to_mode_list(list(inst), list(inst), list(mode)).
:- mode inst_lists_to_mode_list(in, in, out) is det.

	% Given a user-defined or compiler-defined inst name,
	% lookup the corresponding inst in the inst table.
	%
:- pred inst_lookup(module_info, inst_name, inst).
:- mode inst_lookup(in, in, out) is det.

	% Use the instmap deltas for all the atomic sub-goals to recompute
	% the instmap deltas for all the non-atomic sub-goals of a goal.
	% Used to ensure that the instmap deltas remain valid after
	% code has been re-arranged, e.g. by followcode.
	% This also takes the module_info as input and output since it
	% may need to insert new merge_insts into the merge_inst table.
	% If the first argument is yes, the instmap_deltas for calls
	% and deconstruction unifications are also recomputed.
:- pred recompute_instmap_delta(bool, hlds_goal, hlds_goal, instmap,
				module_info, module_info).
:- mode recompute_instmap_delta(in, in, out, in, in, out) is det.

	% Given corresponding lists of types and modes, produce a new
	% list of modes which includes the information provided by the
	% corresponding types.
	%
:- pred propagate_types_into_mode_list(list(type), module_info, list(mode),
				list(mode)).
:- mode propagate_types_into_mode_list(in, in, in, out) is det.

	% Given corresponding lists of types and insts and a substitution
	% for the type variables in the type, produce a new list of insts
	% which includes the information provided by the corresponding types.
	%
:- pred propagate_types_into_inst_list(list(type), tsubst, module_info,
		list(inst), list(inst)).
:- mode propagate_types_into_inst_list(in, in, in, in, out) is det.

	% Given the mode of a predicate,
	% work out which arguments are live (might be used again
	% by the caller of that predicate) and which are dead.
:- pred get_arg_lives(list(mode), module_info, list(is_live)).
:- mode get_arg_lives(in, in, out) is det.

	% Predicates to make error messages more readable by stripping
	% "mercury_builtin" module qualifiers from modes.

:- pred strip_builtin_qualifier_from_cons_id(cons_id, cons_id).
:- mode strip_builtin_qualifier_from_cons_id(in, out) is det.

:- pred strip_builtin_qualifiers_from_mode_list(list(mode), list(mode)).
:- mode strip_builtin_qualifiers_from_mode_list(in, out) is det.

:- pred strip_builtin_qualifiers_from_inst_list(list(inst), list(inst)).
:- mode strip_builtin_qualifiers_from_inst_list(in, out) is det.

:- pred strip_builtin_qualifiers_from_inst((inst), (inst)).
:- mode strip_builtin_qualifiers_from_inst(in, out) is det.

	% Given the switched on variable and the instmaps before the switch
	% and after a branch make sure that any information added by the
	% functor test gets added to the instmap for the case.
:- pred fixup_switch_var(var, instmap, instmap, hlds_goal, hlds_goal). 
:- mode fixup_switch_var(in, in, in, in, out) is det.

%-----------------------------------------------------------------------------%

:- pred normalise_insts(list(inst), module_info, list(inst)).
:- mode normalise_insts(in, in, out) is det.

:- pred normalise_inst(inst, module_info, inst).
:- mode normalise_inst(in, in, out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module require, int, map, set, term, std_util, assoc_list.
:- import_module prog_util, type_util.
:- import_module inst_match.

%-----------------------------------------------------------------------------%

mode_list_get_final_insts([], _ModuleInfo, []).
mode_list_get_final_insts([Mode | Modes], ModuleInfo, [Inst | Insts]) :-
	mode_get_insts(ModuleInfo, Mode, _, Inst),
	mode_list_get_final_insts(Modes, ModuleInfo, Insts).

mode_list_get_initial_insts([], _ModuleInfo, []).
mode_list_get_initial_insts([Mode | Modes], ModuleInfo, [Inst | Insts]) :-
	mode_get_insts(ModuleInfo, Mode, Inst, _),
	mode_list_get_initial_insts(Modes, ModuleInfo, Insts).

inst_lists_to_mode_list([], [_|_], _) :-
	error("inst_lists_to_mode_list: length mis-match").
inst_lists_to_mode_list([_|_], [], _) :-
	error("inst_lists_to_mode_list: length mis-match").
inst_lists_to_mode_list([], [], []).
inst_lists_to_mode_list([Initial|Initials], [Final|Finals], [Mode|Modes]) :-
	insts_to_mode(Initial, Final, Mode),
	inst_lists_to_mode_list(Initials, Finals, Modes).

:- pred insts_to_mode(inst, inst, mode).
:- mode insts_to_mode(in, in, out) is det.

insts_to_mode(Initial, Final, Mode) :-
	%
	% Use some abbreviations.
	% This is just to make error messages and inferred modes
	% more readable.
	%
	( Initial = free, Final = ground(shared, no) ->
		Mode = user_defined_mode(
				qualified("mercury_builtin", "out"), [])
	; Initial = free, Final = ground(unique, no) ->
		Mode = user_defined_mode(qualified("mercury_builtin", "uo"), [])
	; Initial = ground(shared, no), Final = ground(shared, no) ->
		Mode = user_defined_mode(qualified("mercury_builtin", "in"), [])
	; Initial = ground(unique, no), Final = ground(clobbered, no) ->
		Mode = user_defined_mode(qualified("mercury_builtin", "di"), [])
	; Initial = ground(unique, no), Final = ground(unique, no) ->
		Mode = user_defined_mode(qualified("mercury_builtin", "ui"), [])
	; Initial = free ->
		Mode = user_defined_mode(qualified("mercury_builtin", "out"),
								[Final])
	; Final = ground(clobbered, no) ->
		Mode = user_defined_mode(qualified("mercury_builtin", "di"),
								[Initial])
	; Initial = Final ->
		Mode = user_defined_mode(qualified("mercury_builtin", "in"),
								[Initial])
	;
		Mode = (Initial -> Final)
	).

%-----------------------------------------------------------------------------%

	% A mode is considered an input mode if the top-level
	% node is input.

mode_is_input(ModuleInfo, Mode) :-
	mode_get_insts(ModuleInfo, Mode, InitialInst, _FinalInst),
	inst_is_bound(ModuleInfo, InitialInst).

	% A mode is considered fully input if its initial inst is ground.

mode_is_fully_input(ModuleInfo, Mode) :-
	mode_get_insts(ModuleInfo, Mode, InitialInst, _FinalInst),
	inst_is_ground(ModuleInfo, InitialInst).

	% A mode is considered an output mode if the top-level
	% node is output.

mode_is_output(ModuleInfo, Mode) :-
	mode_get_insts(ModuleInfo, Mode, InitialInst, FinalInst),
	inst_is_free(ModuleInfo, InitialInst),
	inst_is_bound(ModuleInfo, FinalInst).

	% A mode is considered fully output if its initial inst is free
	% and its final insts is ground.

mode_is_fully_output(ModuleInfo, Mode) :-
	mode_get_insts(ModuleInfo, Mode, InitialInst, FinalInst),
	inst_is_free(ModuleInfo, InitialInst),
	inst_is_ground(ModuleInfo, FinalInst).

	% A mode is considered a unused mode if it is equivalent
	% to free->free.

mode_is_unused(ModuleInfo, Mode) :-
	mode_get_insts(ModuleInfo, Mode, InitialInst, FinalInst),
	inst_is_free(ModuleInfo, InitialInst),
	inst_is_free(ModuleInfo, FinalInst).

%-----------------------------------------------------------------------------%

mode_to_arg_mode(ModuleInfo, Mode, Type, ArgMode) :-
	%
	% We need to handle no_tag types (types which have
	% exactly one constructor, and whose one constructor
	% has exactly one argument) specially here,
	% since for them an inst of bound(f(free)) is not really bound
	% as far as code generation is concerned, since the f/1
	% will get optimized away.
	%
	(
		% is this a no_tag type?
		type_constructors(Type, ModuleInfo, Constructors),
		type_is_no_tag_type(Constructors, FunctorName, ArgType)
	->
		% the arg_mode will be determined by the mode and
		% type of the functor's argument,
		% so we figure out the mode and type of the argument,
		% and then recurse
		mode_get_insts(ModuleInfo, Mode, InitialInst, FinalInst),
		ConsId = cons(FunctorName, 1),
		get_single_arg_inst(InitialInst, ModuleInfo, ConsId,
			InitialArgInst),
		get_single_arg_inst(FinalInst, ModuleInfo, ConsId,
			FinalArgInst),
		ModeOfArg = (InitialArgInst -> FinalArgInst),
		mode_to_arg_mode(ModuleInfo, ModeOfArg, ArgType, ArgMode)
	;
		mode_to_arg_mode_2(ModuleInfo, Mode, ArgMode)
	).

:- pred mode_to_arg_mode_2(module_info, mode, arg_mode).
:- mode mode_to_arg_mode_2(in, in, out) is det.
mode_to_arg_mode_2(ModuleInfo, Mode, ArgMode) :-
	mode_get_insts(ModuleInfo, Mode, InitialInst, FinalInst),
	( inst_is_bound(ModuleInfo, InitialInst) ->
		ArgMode = top_in
	; inst_is_bound(ModuleInfo, FinalInst) ->
		ArgMode = top_out
	;
		ArgMode = top_unused
	).

%-----------------------------------------------------------------------------%

	% get_single_arg_inst(Inst, ConsId, Arity, ArgInsts):
	% Given an inst `Inst', figure out what the inst of the
	% argument would be, assuming that the functor is
	% the one given by the specified ConsId, whose arity is 1.
	%
:- pred get_single_arg_inst(inst, module_info, cons_id, inst).
:- mode get_single_arg_inst(in, in, in, out) is det.

get_single_arg_inst(defined_inst(InstName), ModuleInfo, ConsId, ArgInst) :-
	inst_lookup(ModuleInfo, InstName, Inst),
	get_single_arg_inst(Inst, ModuleInfo, ConsId, ArgInst).
get_single_arg_inst(not_reached, _, _, not_reached).
get_single_arg_inst(ground(Uniq, _PredInst), _, _, ground(Uniq, no)).
get_single_arg_inst(bound(_Uniq, List), _, ConsId, ArgInst) :-
	( get_single_arg_inst_2(List, ConsId, ArgInst0) ->
		ArgInst = ArgInst0
	;
		% the code is unreachable
		ArgInst = not_reached
	).
get_single_arg_inst(free, _, _, free).
get_single_arg_inst(free(_Type), _, _, free).	% XXX loses type info
get_single_arg_inst(any(Uniq), _, _, any(Uniq)).
get_single_arg_inst(abstract_inst(_, _), _, _, _) :-
	error("get_single_arg_inst: abstract insts not supported").
get_single_arg_inst(inst_var(_), _, _, _) :-
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

	% Given two lists of modes (inst mappings) of equal length,
	% convert them into a single list of inst pair mappings.

mode_util__modes_to_uni_modes([], [], _ModuleInfo, []).
mode_util__modes_to_uni_modes([], [_|_], _, _) :-
	error("mode_util__modes_to_uni_modes: length mismatch").
mode_util__modes_to_uni_modes([_|_], [], _, _) :-
	error("mode_util__modes_to_uni_modes: length mismatch").
mode_util__modes_to_uni_modes([X|Xs], [Y|Ys], ModuleInfo, [A|As]) :-
	mode_get_insts(ModuleInfo, X, InitialX, FinalX),
	mode_get_insts(ModuleInfo, Y, InitialY, FinalY),
	A = ((InitialX - InitialY) -> (FinalX - FinalY)),
	mode_util__modes_to_uni_modes(Xs, Ys, ModuleInfo, As).

%-----------------------------------------------------------------------------%

	% inst_is_clobbered succeeds iff the inst passed is `clobbered'
	% or `mostly_clobbered' or if it is a user-defined inst which
	% is defined as one of those.

:- inst_is_clobbered(_, X) when X.		% NU-Prolog indexing.

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

:- inst_is_free(_, X) when X.		% NU-Prolog indexing.

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

:- inst_is_bound(_, X) when X.		% NU-Prolog indexing.

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

:- inst_is_bound_to_functors(_, X, _) when X.		% NU-Prolog indexing.

inst_is_bound_to_functors(_, bound(_Uniq, Functors), Functors).
inst_is_bound_to_functors(_, inst_var(_), _) :-
	error("internal error: uninstantiated inst parameter").
inst_is_bound_to_functors(ModuleInfo, defined_inst(InstName), Functors)
		:-
	inst_lookup(ModuleInfo, InstName, Inst),
	inst_is_bound_to_functors(ModuleInfo, Inst, Functors).

%-----------------------------------------------------------------------------%

functors_to_cons_ids([], []).
functors_to_cons_ids([Functor | Functors], [ConsId | ConsIds]) :-
        Functor = functor(ConsId, _ArgInsts),
        functors_to_cons_ids(Functors, ConsIds).

%-----------------------------------------------------------------------------%

get_arg_insts(not_reached, _ConsId, Arity, ArgInsts) :-
	list__duplicate(Arity, not_reached, ArgInsts).
get_arg_insts(ground(Uniq, _PredInst), _ConsId, Arity, ArgInsts) :-
	list__duplicate(Arity, ground(Uniq, no), ArgInsts).
get_arg_insts(bound(_Uniq, List), ConsId, Arity, ArgInsts) :-
	( get_arg_insts_2(List, ConsId, ArgInsts0) ->
		ArgInsts = ArgInsts0
	;
		% the code is unreachable
		list__duplicate(Arity, not_reached, ArgInsts)
	).
get_arg_insts(free, _ConsId, Arity, ArgInsts) :-
	list__duplicate(Arity, free, ArgInsts).
get_arg_insts(free(_Type), _ConsId, Arity, ArgInsts) :-
	list__duplicate(Arity, free, ArgInsts).
get_arg_insts(any(Uniq), _ConsId, Arity, ArgInsts) :-
	list__duplicate(Arity, any(Uniq), ArgInsts).

:- pred get_arg_insts_2(list(bound_inst), cons_id, list(inst)).
:- mode get_arg_insts_2(in, in, out) is semidet.

get_arg_insts_2([BoundInst | BoundInsts], ConsId, ArgInsts) :-
	(
		BoundInst = functor(ConsId, ArgInsts0)
	->
		ArgInsts = ArgInsts0
	;
		get_arg_insts_2(BoundInsts, ConsId, ArgInsts)
	).

%-----------------------------------------------------------------------------%

	% inst_is_ground succeeds iff the inst passed is `ground'
	% or the equivalent.  Abstract insts are not considered ground.

inst_is_ground(ModuleInfo, Inst) :-
	set__init(Expansions),
	inst_is_ground_2(ModuleInfo, Inst, Inst, Expansions).

	% The third argument must be the same as the second.
	% The fourth arg is the set of insts which have already
	% been expanded - we use this to avoid going into an
	% infinite loop.

:- pred inst_is_ground_2(module_info, inst, inst, set(inst)).
:- mode inst_is_ground_2(in, in, in, in) is semidet.

:- inst_is_ground_2(_, X, _, _) when X.		% NU-Prolog indexing.

inst_is_ground_2(_, not_reached, _, _).
inst_is_ground_2(ModuleInfo, bound(_, List), _, Expansions) :-
	bound_inst_list_is_ground_2(List, ModuleInfo, Expansions).
inst_is_ground_2(_, ground(_, _), _, _).
inst_is_ground_2(_, inst_var(_), _, _) :-
	error("internal error: uninstantiated inst parameter").
inst_is_ground_2(ModuleInfo, defined_inst(InstName), Inst, Expansions) :-
	( set__member(Inst, Expansions) ->
		true
	;
		set__insert(Expansions, Inst, Expansions2),
		inst_lookup(ModuleInfo, InstName, Inst2),
		inst_is_ground_2(ModuleInfo, Inst2, Inst2, Expansions2)
	).

	% inst_is_ground_or_any succeeds iff the inst passed is `ground',
	% `any', or the equivalent.  Fails for abstract insts.

inst_is_ground_or_any(ModuleInfo, Inst) :-
	set__init(Expansions),
	inst_is_ground_or_any_2(ModuleInfo, Inst, Inst, Expansions).

	% The third argument must be the same as the second.
	% The fourth arg is the set of insts which have already
	% been expanded - we use this to avoid going into an
	% infinite loop.

:- pred inst_is_ground_or_any_2(module_info, inst, inst, set(inst)).
:- mode inst_is_ground_or_any_2(in, in, in, in) is semidet.

:- inst_is_ground_or_any_2(_, X, _, _) when X.		% NU-Prolog indexing.

inst_is_ground_or_any_2(_, not_reached, _, _).
inst_is_ground_or_any_2(ModuleInfo, bound(_, List), _, Expansions) :-
	bound_inst_list_is_ground_or_any_2(List, ModuleInfo, Expansions).
inst_is_ground_or_any_2(_, ground(_, _), _, _).
inst_is_ground_or_any_2(_, any(_), _, _).
inst_is_ground_or_any_2(_, inst_var(_), _, _) :-
	error("internal error: uninstantiated inst parameter").
inst_is_ground_or_any_2(ModuleInfo, defined_inst(InstName), Inst, Expansions) :-
	( set__member(Inst, Expansions) ->
		true
	;
		set__insert(Expansions, Inst, Expansions2),
		inst_lookup(ModuleInfo, InstName, Inst2),
		inst_is_ground_or_any_2(ModuleInfo, Inst2, Inst2, Expansions2)
	).

	% inst_is_unique succeeds iff the inst passed is unique
	% or free.  Abstract insts are not considered unique.

inst_is_unique(ModuleInfo, Inst) :-
	set__init(Expansions),
	inst_is_unique_2(ModuleInfo, Inst, Inst, Expansions).

	% The third argument must be the same as the second.
	% The fourth arg is the set of insts which have already
	% been expanded - we use this to avoid going into an
	% infinite loop.

:- pred inst_is_unique_2(module_info, inst, inst, set(inst)).
:- mode inst_is_unique_2(in, in, in, in) is semidet.

:- inst_is_unique_2(_, X, _, _) when X.		% NU-Prolog indexing.

inst_is_unique_2(_, not_reached, _, _).
inst_is_unique_2(ModuleInfo, bound(unique, List), _, Expansions) :-
	bound_inst_list_is_unique_2(List, ModuleInfo, Expansions).
inst_is_unique_2(_, any(unique), _, _).
inst_is_unique_2(_, free, _, _).
inst_is_unique_2(_, ground(unique, _), _, _).
inst_is_unique_2(_, inst_var(_), _, _) :-
	error("internal error: uninstantiated inst parameter").
inst_is_unique_2(ModuleInfo, defined_inst(InstName), Inst, Expansions) :-
	( set__member(Inst, Expansions) ->
		true
	;
		set__insert(Expansions, Inst, Expansions2),
		inst_lookup(ModuleInfo, InstName, Inst2),
		inst_is_unique_2(ModuleInfo, Inst2, Inst2, Expansions2)
	).

	% inst_is_mostly_unique succeeds iff the inst passed is unique,
	% mostly_unique, or free.  Abstract insts are not considered unique.

inst_is_mostly_unique(ModuleInfo, Inst) :-
	set__init(Expansions),
	inst_is_mostly_unique_2(ModuleInfo, Inst, Inst, Expansions).

	% The third argument must be the same as the second.
	% The fourth arg is the set of insts which have already
	% been expanded - we use this to avoid going into an
	% infinite loop.

:- pred inst_is_mostly_unique_2(module_info, inst, inst, set(inst)).
:- mode inst_is_mostly_unique_2(in, in, in, in) is semidet.

:- inst_is_mostly_unique_2(_, X, _, _) when X.		% NU-Prolog indexing.

inst_is_mostly_unique_2(_, not_reached, _, _).
inst_is_mostly_unique_2(ModuleInfo, bound(mostly_unique, List), _, Expansions)
		:-
	bound_inst_list_is_mostly_unique_2(List, ModuleInfo, Expansions).
inst_is_mostly_unique_2(ModuleInfo, bound(mostly_unique, List), _, Expansions)
		:-
	bound_inst_list_is_mostly_unique_2(List, ModuleInfo, Expansions).
inst_is_mostly_unique_2(_, any(unique), _, _).
inst_is_mostly_unique_2(_, any(mostly_unique), _, _).
inst_is_mostly_unique_2(_, free, _, _).
inst_is_mostly_unique_2(_, ground(unique, _), _, _).
inst_is_mostly_unique_2(_, ground(mostly_unique, _), _, _).
inst_is_mostly_unique_2(_, inst_var(_), _, _) :-
	error("internal error: uninstantiated inst parameter").
inst_is_mostly_unique_2(ModuleInfo, defined_inst(InstName), Inst, Expansions) :-
	( set__member(Inst, Expansions) ->
		true
	;
		set__insert(Expansions, Inst, Expansions2),
		inst_lookup(ModuleInfo, InstName, Inst2),
		inst_is_mostly_unique_2(ModuleInfo, Inst2, Inst2, Expansions2)
	).

	% inst_is_not_partly_unique succeeds iff the inst passed is 
	% not unique or mostly_unique, i.e. if it is shared
	% or free.  It fails for abstract insts.

inst_is_not_partly_unique(ModuleInfo, Inst) :-
	set__init(Expansions),
	inst_is_not_partly_unique_2(ModuleInfo, Inst, Inst, Expansions).

	% The third argument must be the same as the second.
	% The fourth arg is the set of insts which have already
	% been expanded - we use this to avoid going into an
	% infinite loop.

:- pred inst_is_not_partly_unique_2(module_info, inst, inst, set(inst)).
:- mode inst_is_not_partly_unique_2(in, in, in, in) is semidet.

:- inst_is_not_partly_unique_2(_, X, _, _) when X.	% NU-Prolog indexing.

inst_is_not_partly_unique_2(_, not_reached, _, _).
inst_is_not_partly_unique_2(ModuleInfo, bound(shared, List), _, Expansions) :-
	bound_inst_list_is_not_partly_unique_2(List, ModuleInfo, Expansions).
inst_is_not_partly_unique_2(_, free, _, _).
inst_is_not_partly_unique_2(_, any(shared), _, _).
inst_is_not_partly_unique_2(_, ground(shared, _), _, _).
inst_is_not_partly_unique_2(_, inst_var(_), _, _) :-
	error("internal error: uninstantiated inst parameter").
inst_is_not_partly_unique_2(ModuleInfo, defined_inst(InstName), Inst,
		Expansions) :-
	( set__member(Inst, Expansions) ->
		true
	;
		set__insert(Expansions, Inst, Expansions2),
		inst_lookup(ModuleInfo, InstName, Inst2),
		inst_is_not_partly_unique_2(ModuleInfo, Inst2, Inst2,
			Expansions2)
	).

	% inst_is_not_fully_unique succeeds iff the inst passed is 
	% not unique, i.e. if it is mostly_unique, shared,
	% or free.  It fails for abstract insts.

inst_is_not_fully_unique(ModuleInfo, Inst) :-
	set__init(Expansions),
	inst_is_not_fully_unique_2(ModuleInfo, Inst, Inst, Expansions).

	% The third argument must be the same as the second.
	% The fourth arg is the set of insts which have already
	% been expanded - we use this to avoid going into an
	% infinite loop.

:- pred inst_is_not_fully_unique_2(module_info, inst, inst, set(inst)).
:- mode inst_is_not_fully_unique_2(in, in, in, in) is semidet.

:- inst_is_not_fully_unique_2(_, X, _, _) when X.	% NU-Prolog indexing.

inst_is_not_fully_unique_2(_, not_reached, _, _).
inst_is_not_fully_unique_2(ModuleInfo, bound(shared, List), _, Expansions) :-
	bound_inst_list_is_not_fully_unique_2(List, ModuleInfo, Expansions).
inst_is_not_fully_unique_2(ModuleInfo, bound(mostly_unique, List), _,
		Expansions) :-
	bound_inst_list_is_not_fully_unique_2(List, ModuleInfo, Expansions).
inst_is_not_fully_unique_2(_, any(shared), _, _).
inst_is_not_fully_unique_2(_, any(mostly_unique), _, _).
inst_is_not_fully_unique_2(_, free, _, _).
inst_is_not_fully_unique_2(_, ground(shared, _), _, _).
inst_is_not_fully_unique_2(_, ground(mostly_unique, _), _, _).
inst_is_not_fully_unique_2(_, inst_var(_), _, _) :-
	error("internal error: uninstantiated inst parameter").
inst_is_not_fully_unique_2(ModuleInfo, defined_inst(InstName), Inst,
		Expansions) :-
	( set__member(Inst, Expansions) ->
		true
	;
		set__insert(Expansions, Inst, Expansions2),
		inst_lookup(ModuleInfo, InstName, Inst2),
		inst_is_not_fully_unique_2(ModuleInfo, Inst2, Inst2,
			Expansions2)
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

:- pred bound_inst_list_is_ground_2(list(bound_inst), module_info, set(inst)).
:- mode bound_inst_list_is_ground_2(in, in, in) is semidet.

bound_inst_list_is_ground_2([], _, _).
bound_inst_list_is_ground_2([functor(_Name, Args)|BoundInsts], ModuleInfo,
		Expansions) :-
	inst_list_is_ground_2(Args, ModuleInfo, Expansions),
	bound_inst_list_is_ground_2(BoundInsts, ModuleInfo, Expansions).

:- pred bound_inst_list_is_ground_or_any_2(list(bound_inst), module_info, set(inst)).
:- mode bound_inst_list_is_ground_or_any_2(in, in, in) is semidet.

bound_inst_list_is_ground_or_any_2([], _, _).
bound_inst_list_is_ground_or_any_2([functor(_Name, Args)|BoundInsts],
		ModuleInfo, Expansions) :-
	inst_list_is_ground_or_any_2(Args, ModuleInfo, Expansions),
	bound_inst_list_is_ground_or_any_2(BoundInsts, ModuleInfo, Expansions).

:- pred bound_inst_list_is_unique_2(list(bound_inst), module_info, set(inst)).
:- mode bound_inst_list_is_unique_2(in, in, in) is semidet.

bound_inst_list_is_unique_2([], _, _).
bound_inst_list_is_unique_2([functor(_Name, Args)|BoundInsts], ModuleInfo,
		Expansions) :-
	inst_list_is_unique_2(Args, ModuleInfo, Expansions),
	bound_inst_list_is_unique_2(BoundInsts, ModuleInfo, Expansions).

:- pred bound_inst_list_is_mostly_unique_2(list(bound_inst), module_info,
						set(inst)).
:- mode bound_inst_list_is_mostly_unique_2(in, in, in) is semidet.

bound_inst_list_is_mostly_unique_2([], _, _).
bound_inst_list_is_mostly_unique_2([functor(_Name, Args)|BoundInsts],
		ModuleInfo, Expansions) :-
	inst_list_is_mostly_unique_2(Args, ModuleInfo, Expansions),
	bound_inst_list_is_mostly_unique_2(BoundInsts, ModuleInfo, Expansions).

:- pred bound_inst_list_is_not_partly_unique_2(list(bound_inst), module_info,
						set(inst)).
:- mode bound_inst_list_is_not_partly_unique_2(in, in, in) is semidet.

bound_inst_list_is_not_partly_unique_2([], _, _).
bound_inst_list_is_not_partly_unique_2([functor(_Name, Args)|BoundInsts],
		ModuleInfo, Expansions) :-
	inst_list_is_not_partly_unique_2(Args, ModuleInfo, Expansions),
	bound_inst_list_is_not_partly_unique_2(BoundInsts, ModuleInfo,
		Expansions).

:- pred bound_inst_list_is_not_fully_unique_2(list(bound_inst), module_info,
						set(inst)).
:- mode bound_inst_list_is_not_fully_unique_2(in, in, in) is semidet.

bound_inst_list_is_not_fully_unique_2([], _, _).
bound_inst_list_is_not_fully_unique_2([functor(_Name, Args)|BoundInsts],
		ModuleInfo, Expansions) :-
	inst_list_is_not_fully_unique_2(Args, ModuleInfo, Expansions),
	bound_inst_list_is_not_fully_unique_2(BoundInsts, ModuleInfo,
		Expansions).

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

:- pred inst_list_is_ground_2(list(inst), module_info, set(inst)).
:- mode inst_list_is_ground_2(in, in, in) is semidet.

inst_list_is_ground_2([], _, _).
inst_list_is_ground_2([Inst | Insts], ModuleInfo, Expansions) :-
	inst_is_ground_2(ModuleInfo, Inst, Inst, Expansions),
	inst_list_is_ground_2(Insts, ModuleInfo, Expansions).

:- pred inst_list_is_ground_or_any_2(list(inst), module_info, set(inst)).
:- mode inst_list_is_ground_or_any_2(in, in, in) is semidet.

inst_list_is_ground_or_any_2([], _, _).
inst_list_is_ground_or_any_2([Inst | Insts], ModuleInfo, Expansions) :-
	inst_is_ground_or_any_2(ModuleInfo, Inst, Inst, Expansions),
	inst_list_is_ground_or_any_2(Insts, ModuleInfo, Expansions).

:- pred inst_list_is_unique_2(list(inst), module_info, set(inst)).
:- mode inst_list_is_unique_2(in, in, in) is semidet.

inst_list_is_unique_2([], _, _).
inst_list_is_unique_2([Inst | Insts], ModuleInfo, Expansions) :-
	inst_is_unique_2(ModuleInfo, Inst, Inst, Expansions),
	inst_list_is_unique_2(Insts, ModuleInfo, Expansions).

:- pred inst_list_is_mostly_unique_2(list(inst), module_info, set(inst)).
:- mode inst_list_is_mostly_unique_2(in, in, in) is semidet.

inst_list_is_mostly_unique_2([], _, _).
inst_list_is_mostly_unique_2([Inst | Insts], ModuleInfo, Expansions) :-
	inst_is_mostly_unique_2(ModuleInfo, Inst, Inst, Expansions),
	inst_list_is_mostly_unique_2(Insts, ModuleInfo, Expansions).

:- pred inst_list_is_not_partly_unique_2(list(inst), module_info, set(inst)).
:- mode inst_list_is_not_partly_unique_2(in, in, in) is semidet.

inst_list_is_not_partly_unique_2([], _, _).
inst_list_is_not_partly_unique_2([Inst | Insts], ModuleInfo, Expansions) :-
	inst_is_not_partly_unique_2(ModuleInfo, Inst, Inst, Expansions),
	inst_list_is_not_partly_unique_2(Insts, ModuleInfo, Expansions).

:- pred inst_list_is_not_fully_unique_2(list(inst), module_info, set(inst)).
:- mode inst_list_is_not_fully_unique_2(in, in, in) is semidet.

inst_list_is_not_fully_unique_2([], _, _).
inst_list_is_not_fully_unique_2([Inst | Insts], ModuleInfo, Expansions) :-
	inst_is_not_fully_unique_2(ModuleInfo, Inst, Inst, Expansions),
	inst_list_is_not_fully_unique_2(Insts, ModuleInfo, Expansions).

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

inst_lookup(ModuleInfo, InstName, Inst) :-
	inst_lookup_2(InstName, ModuleInfo, Inst).

:- pred inst_lookup_2(inst_name, module_info, inst).
:- mode inst_lookup_2(in, in, out) is det.

inst_lookup_2(InstName, ModuleInfo, Inst) :-
	( InstName = unify_inst(_, _, _, _),
		module_info_insts(ModuleInfo, InstTable),
		inst_table_get_unify_insts(InstTable, UnifyInstTable),
		map__lookup(UnifyInstTable, InstName, MaybeInst),
		( MaybeInst = known(Inst0, _) ->
			Inst = Inst0
		;
			Inst = defined_inst(InstName)
		)
	; InstName = merge_inst(A, B),
		module_info_insts(ModuleInfo, InstTable),
		inst_table_get_merge_insts(InstTable, MergeInstTable),
		map__lookup(MergeInstTable, A - B, MaybeInst),
		( MaybeInst = known(Inst0) ->
			Inst = Inst0
		;
			Inst = defined_inst(InstName)
		)
	; InstName = ground_inst(_, _, _, _),
		module_info_insts(ModuleInfo, InstTable),
		inst_table_get_ground_insts(InstTable, GroundInstTable),
		map__lookup(GroundInstTable, InstName, MaybeInst),
		( MaybeInst = known(Inst0) ->
			Inst = Inst0
		;
			Inst = defined_inst(InstName)
		)
	; InstName = shared_inst(SharedInstName),
		module_info_insts(ModuleInfo, InstTable),
		inst_table_get_shared_insts(InstTable, SharedInstTable),
		map__lookup(SharedInstTable, SharedInstName, MaybeInst),
		( MaybeInst = known(Inst0) ->
			Inst = Inst0
		;
			Inst = defined_inst(InstName)
		)
	; InstName = mostly_uniq_inst(NondetLiveInstName),
		module_info_insts(ModuleInfo, InstTable),
		inst_table_get_mostly_uniq_insts(InstTable,
			NondetLiveInstTable),
		map__lookup(NondetLiveInstTable, NondetLiveInstName, MaybeInst),
		( MaybeInst = known(Inst0) ->
			Inst = Inst0
		;
			Inst = defined_inst(InstName)
		)
	; InstName = user_inst(Name, Args),
		module_info_insts(ModuleInfo, InstTable),
		inst_table_get_user_insts(InstTable, UserInstTable),
		user_inst_table_get_inst_defns(UserInstTable, InstDefns),
		list__length(Args, Arity),
		( map__search(InstDefns, Name - Arity, InstDefn) ->
			InstDefn = hlds_inst_defn(_VarSet, Params, Inst0,
					_Cond, _C, _),
			inst_lookup_subst_args(Inst0, Params, Name, Args, Inst)
		;
			Inst = abstract_inst(Name, Args)
		)
	; InstName = typed_ground(Uniq, Type),
		map__init(Subst),
		propagate_type_into_inst(Type, Subst, ModuleInfo,
			ground(Uniq, no), Inst)
	; InstName = typed_inst(Type, TypedInstName),
		inst_lookup_2(TypedInstName, ModuleInfo, Inst0),
		map__init(Subst),
		propagate_type_into_inst(Type, Subst, ModuleInfo, Inst0, Inst)
	),
	!.

%-----------------------------------------------------------------------------%

	% Given corresponding lists of types and modes, produce a new
	% list of modes which includes the information provided by the
	% corresponding types.

propagate_types_into_mode_list([], _, [], []).
propagate_types_into_mode_list([Type | Types], ModuleInfo, [Mode0 | Modes0],
		[Mode | Modes]) :-
	propagate_type_into_mode(Type, ModuleInfo, Mode0, Mode),
	propagate_types_into_mode_list(Types, ModuleInfo, Modes0, Modes).
propagate_types_into_mode_list([], _, [_|_], []) :-
	error("propagate_types_into_mode_list: length mismatch").
propagate_types_into_mode_list([_|_], _, [], []) :-
	error("propagate_types_into_mode_list: length mismatch").

propagate_types_into_inst_list([], _, _, [], []).
propagate_types_into_inst_list([Type | Types], Subst, ModuleInfo,
		[Inst0 | Insts0], [Inst | Insts]) :-
	propagate_type_into_inst(Type, Subst, ModuleInfo, Inst0, Inst),
	propagate_types_into_inst_list(Types, Subst, ModuleInfo, Insts0, Insts).
propagate_types_into_inst_list([], _, _, [_|_], []) :-
	error("propagate_types_into_inst_list: length mismatch").
propagate_types_into_inst_list([_|_], _, _, [], []) :-
	error("propagate_types_into_inst_list: length mismatch").

	% Given a type and a mode, produce a new mode which includes
	% the information provided by the type.

:- pred propagate_type_into_mode(type, module_info, mode, mode).
:- mode propagate_type_into_mode(in, in, in, out) is det.

propagate_type_into_mode(Type, ModuleInfo, Mode0, Mode) :-
	mode_get_insts(ModuleInfo, Mode0, InitialInst0, FinalInst0),
	map__init(Subst),
	propagate_type_into_inst_lazily(Type, Subst, ModuleInfo, InitialInst0,
		InitialInst),
	propagate_type_into_inst_lazily(Type, Subst, ModuleInfo, FinalInst0, 
		FinalInst),
	Mode = (InitialInst -> FinalInst).

	% Given a type, an inst and a substitution for the type variables in
	% the type, produce a new inst which includes the information provided
	% by the type.
	%
	% There are three sorts of information added:
	%	1.  Module qualifiers.
	%	2.  The set of constructors in the type.
	%	3.  For higher-order function types
	%	    (but not higher-order predicate types),
	%	    the higher-order inst, i.e. the argument modes
	%	    and the determinism.
	%
	% Currently #2 is not yet implemented, due to unsolved
	% efficiency problems.  (See the XXX's below.)
	%
	% There are two versions, an "eager" one and a "lazy" one.
	% In general eager expansion is to be preferred, because
	% the expansion is done just once, whereas with lazy expansion
	% the work will be done N times.
	% However, for recursive insts we must use lazy expansion
	% (otherwise we would get infinite regress).
	% Also, usually many of the imported procedures will not be called,
	% so for the insts in imported mode declarations N is often zero.

:- pred propagate_type_into_inst(type, tsubst, module_info, inst, inst).
:- mode propagate_type_into_inst(in, in, in, in, out) is det.

:- pred propagate_type_into_inst_lazily(type, tsubst, module_info, inst, inst).
:- mode propagate_type_into_inst_lazily(in, in, in, in, out) is det.

/*********
	% XXX We ought to expand things eagerly here, using the commented
	% out code below.  However, that causes efficiency problems,
	% so for the moment it is disabled.
propagate_type_into_inst(Type, Subst, ModuleInfo, Inst0, Inst) :-
	apply_type_subst(Type0, Subst, Type),
	(
	        type_constructors(Type, ModuleInfo, Constructors)
	->
	        propagate_ctor_info(Inst0, Type, Constructors, ModuleInfo,
	               Inst) 
	;
	        Inst = Inst0
	).
*********/

propagate_type_into_inst(Type, Subst, ModuleInfo, Inst0, Inst) :-
	propagate_ctor_info_lazily(Inst0, Type, Subst, ModuleInfo, Inst).

propagate_type_into_inst_lazily(Type, Subst, ModuleInfo, Inst0, Inst) :-
	propagate_ctor_info_lazily(Inst0, Type, Subst, ModuleInfo, Inst).

%-----------------------------------------------------------------------------%

:- pred propagate_ctor_info(inst, type, list(constructor), module_info, inst).
:- mode propagate_ctor_info(in, in, in, in, out) is det.

propagate_ctor_info(any(Uniq), _Type, _, _, any(Uniq)).	% XXX loses type info!

% propagate_ctor_info(free, Type, _, _, free(Type)).	% temporarily disabled
propagate_ctor_info(free, _Type, _, _, free).	% XXX temporary hack

propagate_ctor_info(free(_), _, _, _, _) :-
	error("propagate_ctor_info: type info already present").
propagate_ctor_info(bound(Uniq, BoundInsts0), Type, _Constructors, ModuleInfo,
		Inst) :-
	propagate_ctor_info_2(BoundInsts0, Type, ModuleInfo, BoundInsts),
	( BoundInsts = [] ->
		Inst = not_reached
	;
		% XXX do we need to sort the BoundInsts?
		Inst = bound(Uniq, BoundInsts)
	).
propagate_ctor_info(ground(Uniq, no), Type, Constructors, ModuleInfo, Inst) :-
	( type_is_higher_order(Type, function, ArgTypes) ->
		default_higher_order_func_inst(ArgTypes, ModuleInfo,
			HigherOrderInstInfo),
		Inst = ground(Uniq, yes(HigherOrderInstInfo))
	;
		constructors_to_bound_insts(Constructors, Uniq, ModuleInfo,
			BoundInsts0),
		list__sort_and_remove_dups(BoundInsts0, BoundInsts),
		Inst = bound(Uniq, BoundInsts)
	).
propagate_ctor_info(ground(Uniq, yes(PredInstInfo0)), Type, _Ctors, ModuleInfo,
			ground(Uniq, yes(PredInstInfo))) :-
	PredInstInfo0 = pred_inst_info(PredOrFunc, Modes0, Det),
	PredInstInfo = pred_inst_info(PredOrFunc, Modes, Det),
	(
		type_is_higher_order(Type, PredOrFunc, ArgTypes),
		list__same_length(ArgTypes, Modes0)
	->
		propagate_types_into_mode_list(ArgTypes, ModuleInfo,
			Modes0, Modes)
	;
		% The inst is not a valid inst for the type,
		% so leave it alone. This can only happen if the user
		% has made a mistake.  A mode error should hopefully
		% be reported if anything tries to match with the inst.
		Modes = Modes0
	).

propagate_ctor_info(not_reached, _Type, _Constructors, _ModuleInfo,
		not_reached).
propagate_ctor_info(inst_var(_), _, _, _, _) :-
	error("propagate_ctor_info: unbound inst var").
propagate_ctor_info(abstract_inst(Name, Args), _, _, _,
		abstract_inst(Name, Args)).	% XXX loses info
propagate_ctor_info(defined_inst(InstName), Type, Ctors, ModuleInfo, Inst) :-
	inst_lookup(ModuleInfo, InstName, Inst0),
	propagate_ctor_info(Inst0, Type, Ctors, ModuleInfo, Inst).

:- pred propagate_ctor_info_lazily(inst, type, tsubst, module_info, inst).
:- mode propagate_ctor_info_lazily(in, in, in, in, out) is det.

propagate_ctor_info_lazily(any(Uniq), _Type, _, _, any(Uniq)).
						% XXX loses type info!

% propagate_ctor_info_lazily(free, Type, _, _, free(Type)).
							% temporarily disabled
propagate_ctor_info_lazily(free, _Type, _, _, free).	% XXX temporary hack

propagate_ctor_info_lazily(free(_), _, _, _, _) :-
	error("propagate_ctor_info_lazily: type info already present").
propagate_ctor_info_lazily(bound(Uniq, BoundInsts0), Type0, Subst, 
		ModuleInfo, Inst) :-
	apply_type_subst(Type0, Subst, Type),
	propagate_ctor_info_2(BoundInsts0, Type, ModuleInfo, BoundInsts),
	( BoundInsts = [] ->
		Inst = not_reached
	;
		% XXX do we need to sort the BoundInsts?
		Inst = bound(Uniq, BoundInsts)
	).
propagate_ctor_info_lazily(ground(Uniq, no), Type0, Subst, ModuleInfo, Inst) :-
	apply_type_subst(Type0, Subst, Type),
	( type_is_higher_order(Type, function, ArgTypes) ->
		default_higher_order_func_inst(ArgTypes, ModuleInfo,
			HigherOrderInstInfo),
		Inst = ground(Uniq, yes(HigherOrderInstInfo))
	;
		% XXX The information added by this is not yet used,
		% so it's disabled since it unnecessarily complicates
		% the insts.
		/*********
		Inst = defined_inst(typed_ground(Uniq, Type)) 
		*********/
		Inst = ground(Uniq, no)
	).

propagate_ctor_info_lazily(ground(Uniq, yes(PredInstInfo0)), Type0, Subst,
		ModuleInfo, ground(Uniq, yes(PredInstInfo))) :-
	PredInstInfo0 = pred_inst_info(PredOrFunc, Modes0, Det),
	PredInstInfo = pred_inst_info(PredOrFunc, Modes, Det),
	apply_type_subst(Type0, Subst, Type),
	(
		type_is_higher_order(Type, PredOrFunc, ArgTypes),
		list__same_length(ArgTypes, Modes0)
	->
		propagate_types_into_mode_list(ArgTypes, ModuleInfo,
			Modes0, Modes)
	;
		% The inst is not a valid inst for the type,
		% so leave it alone. This can only happen if the user
		% has made a mistake.  A mode error should hopefully
		% be reported if anything tries to match with the inst.
		Modes = Modes0
	).
propagate_ctor_info_lazily(not_reached, _Type, _, _ModuleInfo, not_reached).
propagate_ctor_info_lazily(inst_var(_), _, _, _, _) :-
	error("propagate_ctor_info_lazily: unbound inst var").
propagate_ctor_info_lazily(abstract_inst(Name, Args), _, _, _,
		abstract_inst(Name, Args)).	% XXX loses info
propagate_ctor_info_lazily(defined_inst(InstName0), Type0, Subst, _,
		defined_inst(InstName)) :-
	apply_type_subst(Type0, Subst, Type),
	( InstName0 = typed_inst(_, _) ->
		% If this happens, it means that we have already
		% lazily propagated type info into this inst.
		% We want to avoid creating insts of the form
		% typed_inst(_, typed_inst(...)), because that would be
		% unnecessary, and could cause efficiency problems
		% or perhaps even infinite loops (?).
		InstName = InstName0
	;
		InstName = typed_inst(Type, InstName0)
	).

	%
	% If the user does not explicitly specify a higher-order inst
	% for a higher-order function type, it defaults to
	% `func(in, in, ..., in) = out is det',
	% i.e. all args input, return value output, and det.
	% This applies recursively to the arguments and return
	% value too.
	%
:- pred default_higher_order_func_inst(list(type), module_info, pred_inst_info).
:- mode default_higher_order_func_inst(in, in, out) is det.

default_higher_order_func_inst(PredArgTypes, ModuleInfo, PredInstInfo) :-
	In = (ground(shared, no) -> ground(shared, no)),
	Out = (free -> ground(shared, no)),
	list__length(PredArgTypes, NumPredArgs),
	NumFuncArgs is NumPredArgs - 1,
	list__duplicate(NumFuncArgs, In, FuncArgModes),
	FuncRetMode = Out,
	list__append(FuncArgModes, [FuncRetMode], PredArgModes0),
	propagate_types_into_mode_list(PredArgTypes, ModuleInfo,
		PredArgModes0, PredArgModes),
	PredInstInfo = pred_inst_info(function, PredArgModes, det).

:- pred constructors_to_bound_insts(list(constructor), uniqueness, module_info,
				list(bound_inst)).
:- mode constructors_to_bound_insts(in, in, in, out) is det.

constructors_to_bound_insts([], _, _, []).
constructors_to_bound_insts([Ctor | Ctors], Uniq, ModuleInfo,
		[BoundInst | BoundInsts]) :-
	Ctor = Name - Args,
	ctor_arg_list_to_inst_list(Args, Uniq, Insts),
	list__length(Insts, Arity),
	BoundInst = functor(cons(Name, Arity), Insts),
	constructors_to_bound_insts(Ctors, Uniq, ModuleInfo, BoundInsts).

:- pred ctor_arg_list_to_inst_list(list(constructor_arg), uniqueness,
	list(inst)).
:- mode ctor_arg_list_to_inst_list(in, in, out) is det.

ctor_arg_list_to_inst_list([], _, []).
ctor_arg_list_to_inst_list([_Name - _Type | Args], Uniq, [Inst | Insts]) :-
	% The information added by this is not yet used, so it's disabled 
	% since it unnecessarily complicates the insts.
	% Inst = defined_inst(typed_ground(Uniq, Type)), 
	Inst = ground(Uniq, no),
	ctor_arg_list_to_inst_list(Args, Uniq, Insts).

:- pred propagate_ctor_info_2(list(bound_inst), (type), module_info,
		list(bound_inst)).
:- mode propagate_ctor_info_2(in, in, in, out) is det.

propagate_ctor_info_2(BoundInsts0, Type, ModuleInfo, BoundInsts) :-
	(
		type_to_type_id(Type, TypeId, TypeArgs),
		TypeId = qualified(TypeModule, _) - _,
		module_info_types(ModuleInfo, TypeTable),
		map__search(TypeTable, TypeId, TypeDefn),
		hlds_data__get_type_defn_tparams(TypeDefn, TypeParams0),
		hlds_data__get_type_defn_body(TypeDefn, TypeBody),
		TypeBody = du_type(Constructors, _, _)
	->
		term__term_list_to_var_list(TypeParams0, TypeParams),
		map__from_corresponding_lists(TypeParams, TypeArgs, ArgSubst),
		propagate_ctor_info_3(BoundInsts0, TypeModule, Constructors,
			ArgSubst, ModuleInfo, BoundInsts1),
		list__sort(BoundInsts1, BoundInsts)
	;
		% Builtin types don't need processing.
		BoundInsts = BoundInsts0
	).

:- pred propagate_ctor_info_3(list(bound_inst), string, list(constructor),
		tsubst, module_info, list(bound_inst)).
:- mode propagate_ctor_info_3(in, in, in, in, in, out) is det.

propagate_ctor_info_3([], _, _, _, _, []).
propagate_ctor_info_3([BoundInst0 | BoundInsts0], TypeModule, Constructors,
		Subst, ModuleInfo, [BoundInst | BoundInsts]) :-
	BoundInst0 = functor(ConsId0, ArgInsts0),
	( ConsId0 = cons(unqualified(Name), Ar) ->
		ConsId = cons(qualified(TypeModule, Name), Ar)
	;
		ConsId = ConsId0
	),
	(
		ConsId = cons(ConsName, Arity),
		GetCons = lambda([Ctor::in] is semidet, (
				Ctor = ConsName - CtorArgs,
				list__length(CtorArgs, Arity)
			)),
		list__filter(GetCons, Constructors, [Constructor])
	->
		Constructor = _ - Args,
		GetArgTypes = lambda([CtorArg::in, ArgType::out] is det, (
				CtorArg = _ArgName - ArgType
			)),
		list__map(GetArgTypes, Args, ArgTypes),
		propagate_types_into_inst_list(ArgTypes, Subst,
			ModuleInfo, ArgInsts0, ArgInsts),
		BoundInst = functor(ConsId, ArgInsts)
	;
		% The cons_id is not a valid constructor for the type,
		% so leave it alone. This can only happen in a user defined
		% bound_inst. A mode error should be reported if anything
		% tries to match with the inst.
		BoundInst = functor(ConsId, ArgInsts0)
	),
	propagate_ctor_info_3(BoundInsts0, TypeModule,
		Constructors, Subst, ModuleInfo, BoundInsts).

:- pred apply_type_subst(type, tsubst, type).
:- mode apply_type_subst(in, in, out) is det.

apply_type_subst(Type0, Subst, Type) :-
	% optimize common case
	( map__is_empty(Subst) ->
		Type = Type0
	;
		term__apply_substitution(Type0, Subst, Type)
	).

%-----------------------------------------------------------------------------%

:- pred inst_lookup_subst_args(hlds_inst_body, list(inst_param), sym_name,
			list(inst), inst).
:- mode inst_lookup_subst_args(in, in, in, in, out) is det.

inst_lookup_subst_args(eqv_inst(Inst0), Params, _Name, Args, Inst) :-
	inst_substitute_arg_list(Inst0, Params, Args, Inst).
inst_lookup_subst_args(abstract_inst, _Params, Name, Args,
		abstract_inst(Name, Args)).

%-----------------------------------------------------------------------------%
	% mode_get_insts returns the initial instantiatedness and
	% the final instantiatedness for a given mode.

mode_get_insts_semidet(_ModuleInfo, (InitialInst -> FinalInst), 
		InitialInst, FinalInst).
mode_get_insts_semidet(ModuleInfo, user_defined_mode(Name, Args), 
		Initial, Final) :-
	list__length(Args, Arity),
	module_info_modes(ModuleInfo, Modes),
	mode_table_get_mode_defns(Modes, ModeDefns),
	map__search(ModeDefns, Name - Arity, HLDS_Mode),
	HLDS_Mode = hlds_mode_defn(_VarSet, Params, ModeDefn, _Cond,
						_Context, _Status),
	ModeDefn = eqv_mode(Mode0),
	mode_substitute_arg_list(Mode0, Params, Args, Mode),
	mode_get_insts(ModuleInfo, Mode, Initial, Final).

mode_get_insts(ModuleInfo, Mode, Inst1, Inst2) :-
	( mode_get_insts_semidet(ModuleInfo, Mode, Inst1a, Inst2a) ->
		Inst1 = Inst1a,
		Inst2 = Inst2a
	;
		error("mode_get_insts_semidet failed")
	).


	% mode_substitute_arg_list(Mode0, Params, Args, Mode) is true
	% iff Mode is the mode that results from substituting all
	% occurrences of Params in Mode0 with the corresponding
	% value in Args.

:- pred mode_substitute_arg_list(mode, list(inst_param), list(inst), mode).
:- mode mode_substitute_arg_list(in, in, in, out) is det.

mode_substitute_arg_list(Mode0, Params, Args, Mode) :-
	( Params = [] ->
		Mode = Mode0	% optimize common case
	;
		map__from_corresponding_lists(Params, Args, Subst),
		mode_apply_substitution(Mode0, Subst, Mode)
	).

	% inst_substitute_arg_list(Inst0, Params, Args, Inst) is true
	% iff Inst is the inst that results from substituting all
	% occurrences of Params in Inst0 with the corresponding
	% value in Args.

:- pred inst_substitute_arg_list(inst, list(inst_param), list(inst), inst).
:- mode inst_substitute_arg_list(in, in, in, out) is det.

inst_substitute_arg_list(Inst0, Params, Args, Inst) :-
	( Params = [] ->
		Inst = Inst0	% optimize common case
	;
		map__from_corresponding_lists(Params, Args, Subst),
		inst_apply_substitution(Inst0, Subst, Inst)
	).

	% mode_apply_substitution(Mode0, Subst, Mode) is true iff
	% Mode is the mode that results from apply Subst to Mode0.

:- type inst_subst == map(inst_param, inst).

:- pred mode_apply_substitution(mode, inst_subst, mode).
:- mode mode_apply_substitution(in, in, out) is det.

mode_apply_substitution((I0 -> F0), Subst, (I -> F)) :-
	inst_apply_substitution(I0, Subst, I),
	inst_apply_substitution(F0, Subst, F).
mode_apply_substitution(user_defined_mode(Name, Args0), Subst,
		    user_defined_mode(Name, Args)) :-
	inst_list_apply_substitution(Args0, Subst, Args).

	% inst_list_apply_substitution(Insts0, Subst, Insts) is true
	% iff Inst is the inst that results from applying Subst to Insts0.

:- pred inst_list_apply_substitution(list(inst), inst_subst, list(inst)).
:- mode inst_list_apply_substitution(in, in, out) is det.

inst_list_apply_substitution([], _, []).
inst_list_apply_substitution([A0 | As0], Subst, [A | As]) :-
	inst_apply_substitution(A0, Subst, A),
	inst_list_apply_substitution(As0, Subst, As).

	% inst_substitute_arg(Inst0, Subst, Inst) is true
	% iff Inst is the inst that results from substituting all
	% occurrences of Param in Inst0 with Arg.

:- pred inst_apply_substitution(inst, inst_subst, inst).
:- mode inst_apply_substitution(in, in, out) is det.

inst_apply_substitution(any(Uniq), _, any(Uniq)).
inst_apply_substitution(free, _, free).
inst_apply_substitution(free(T), _, free(T)).
inst_apply_substitution(ground(Uniq, PredStuff0), Subst,
			ground(Uniq, PredStuff)) :-
	maybe_pred_inst_apply_substitution(PredStuff0, Subst, PredStuff).
inst_apply_substitution(bound(Uniq, Alts0), Subst, bound(Uniq, Alts)) :-
	alt_list_apply_substitution(Alts0, Subst, Alts).
inst_apply_substitution(not_reached, _, not_reached).
inst_apply_substitution(inst_var(Var), Subst, Result) :-
	(
		% XXX should params be vars?
		map__search(Subst, term__variable(Var), Replacement)
	->
		Result = Replacement
	;
		Result = inst_var(Var)
	).
inst_apply_substitution(defined_inst(InstName0), Subst,
		    defined_inst(InstName)) :-
	inst_name_apply_substitution(InstName0, Subst, InstName).
inst_apply_substitution(abstract_inst(Name, Args0), Subst,
		    abstract_inst(Name, Args)) :-
	inst_list_apply_substitution(Args0, Subst, Args).

:- pred inst_name_apply_substitution(inst_name, inst_subst, inst_name).
:- mode inst_name_apply_substitution(in, in, out) is det.

inst_name_apply_substitution(user_inst(Name, Args0), Subst,
		user_inst(Name, Args)) :-
	inst_list_apply_substitution(Args0, Subst, Args).
inst_name_apply_substitution(unify_inst(Live, InstA0, InstB0, Real), Subst,
		unify_inst(Live, InstA, InstB, Real)) :-
	inst_apply_substitution(InstA0, Subst, InstA),
	inst_apply_substitution(InstB0, Subst, InstB).
inst_name_apply_substitution(merge_inst(InstA0, InstB0), Subst,
		merge_inst(InstA, InstB)) :-
	inst_apply_substitution(InstA0, Subst, InstA),
	inst_apply_substitution(InstB0, Subst, InstB).
inst_name_apply_substitution(ground_inst(Inst0, IsLive, Uniq, Real), Subst,
				ground_inst(Inst, IsLive, Uniq, Real)) :-
	inst_name_apply_substitution(Inst0, Subst, Inst).
inst_name_apply_substitution(shared_inst(InstName0), Subst,
				shared_inst(InstName)) :-
	inst_name_apply_substitution(InstName0, Subst, InstName).
inst_name_apply_substitution(mostly_uniq_inst(InstName0), Subst,
				mostly_uniq_inst(InstName)) :-
	inst_name_apply_substitution(InstName0, Subst, InstName).
inst_name_apply_substitution(typed_inst(T, Inst0), Subst,
		typed_inst(T, Inst)) :-
	inst_name_apply_substitution(Inst0, Subst, Inst).
inst_name_apply_substitution(typed_ground(Uniq, T), _, typed_ground(Uniq, T)).

:- pred alt_list_apply_substitution(list(bound_inst), inst_subst,
				list(bound_inst)).
:- mode alt_list_apply_substitution(in, in, out) is det.

alt_list_apply_substitution([], _, []).
alt_list_apply_substitution([Alt0|Alts0], Subst, [Alt|Alts]) :-
	Alt0 = functor(Name, Args0),
	inst_list_apply_substitution(Args0, Subst, Args),
	Alt = functor(Name, Args),
	alt_list_apply_substitution(Alts0, Subst, Alts).

:- pred maybe_pred_inst_apply_substitution(maybe(pred_inst_info), inst_subst,
					maybe(pred_inst_info)).
:- mode maybe_pred_inst_apply_substitution(in, in, out) is det.

maybe_pred_inst_apply_substitution(no, _, no).
maybe_pred_inst_apply_substitution(yes(pred_inst_info(PredOrFunc, Modes0, Det)),
			Subst, yes(pred_inst_info(PredOrFunc, Modes, Det))) :-
	mode_list_apply_substitution(Modes0, Subst, Modes).

	% mode_list_apply_substitution(Modes0, Subst, Modes) is true
	% iff Mode is the mode that results from applying Subst to Modes0.

:- pred mode_list_apply_substitution(list(mode), inst_subst, list(mode)).
:- mode mode_list_apply_substitution(in, in, out) is det.

mode_list_apply_substitution([], _, []).
mode_list_apply_substitution([A0 | As0], Subst, [A | As]) :-
	mode_apply_substitution(A0, Subst, A),
	mode_list_apply_substitution(As0, Subst, As).

%-----------------------------------------------------------------------------%

	% In case we later decided to change the representation
	% of mode_ids.

mode_id_to_int(_ - X, X).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Use the instmap deltas for all the atomic sub-goals to recompute
	% the instmap deltas for all the non-atomic sub-goals of a goal.
	% Used to ensure that the instmap deltas remain valid after
	% code has been re-arranged, e.g. by followcode.
	% After common.m has been run, it may be necessary to recompute
	% instmap deltas for atomic goals, since more outputs of calls
	% and deconstructions may become non-local (XXX does this require
	% rerunning mode analysis rather than just recompute_instmap_delta?).

recompute_instmap_delta(RecomputeAtomic, Goal0, Goal, InstMap0) -->
	recompute_instmap_delta(RecomputeAtomic, Goal0, Goal, InstMap0, _).

:- pred recompute_instmap_delta(bool, hlds_goal, hlds_goal, instmap,
		instmap_delta, module_info, module_info).
:- mode recompute_instmap_delta(in, in, out, in, out, in, out) is det.

recompute_instmap_delta(RecomputeAtomic, Goal0 - GoalInfo0, Goal - GoalInfo,
		InstMap0, InstMapDelta) -->
	( 
		{ RecomputeAtomic = no },
		{ goal_is_atomic(Goal0) }
	->
		{ Goal = Goal0 },
		{ GoalInfo = GoalInfo0 },
		{ goal_info_get_instmap_delta(GoalInfo, InstMapDelta) } 
	;
		recompute_instmap_delta_2(RecomputeAtomic, Goal0,
			 GoalInfo0, Goal, InstMap0, InstMapDelta0),
		{ goal_info_get_nonlocals(GoalInfo0, NonLocals) },
		{ instmap_delta_restrict(InstMapDelta0,
			NonLocals, InstMapDelta) },
		{ goal_info_set_instmap_delta(GoalInfo0,
			InstMapDelta, GoalInfo) }
	).

:- pred recompute_instmap_delta_2(bool, hlds_goal_expr, hlds_goal_info,
		hlds_goal_expr, instmap, instmap_delta,
		module_info, module_info).
:- mode recompute_instmap_delta_2(in, in, in, out, in, out, in, out) is det.

recompute_instmap_delta_2(Atomic, switch(Var, Det, Cases0, SM), GoalInfo,
		switch(Var, Det, Cases, SM), InstMap, InstMapDelta) -->
	{ goal_info_get_nonlocals(GoalInfo, NonLocals) },
	recompute_instmap_delta_cases(Atomic, Var, Cases0, Cases,
		InstMap, NonLocals, InstMapDelta).

recompute_instmap_delta_2(Atomic, conj(Goals0), _, conj(Goals),
		InstMap, InstMapDelta) -->
	recompute_instmap_delta_conj(Atomic, Goals0, Goals,
		InstMap, InstMapDelta).

recompute_instmap_delta_2(Atomic, disj(Goals0, SM), GoalInfo, disj(Goals, SM),
		InstMap, InstMapDelta) -->
	{ goal_info_get_nonlocals(GoalInfo, NonLocals) },
	recompute_instmap_delta_disj(Atomic, Goals0, Goals,
		InstMap, NonLocals, InstMapDelta).

recompute_instmap_delta_2(Atomic, not(Goal0), _, not(Goal),
		InstMap, InstMapDelta) -->
	{ instmap_delta_init_reachable(InstMapDelta) },
	recompute_instmap_delta(Atomic, Goal0, Goal, InstMap).

recompute_instmap_delta_2(Atomic, if_then_else(Vars, A0, B0, C0, SM), GoalInfo,
		if_then_else(Vars, A, B, C, SM), InstMap0, InstMapDelta) -->
	recompute_instmap_delta(Atomic, A0, A, InstMap0, InstMapDelta1),
	{ instmap__apply_instmap_delta(InstMap0, InstMapDelta1, InstMap1) },
	recompute_instmap_delta(Atomic, B0, B, InstMap1, InstMapDelta2),
	recompute_instmap_delta(Atomic, C0, C, InstMap0, InstMapDelta3),
	{ instmap_delta_apply_instmap_delta(InstMapDelta1, InstMapDelta2,
		InstMapDelta4) },
	{ goal_info_get_nonlocals(GoalInfo, NonLocals) },
	merge_instmap_delta(InstMap0, NonLocals, InstMapDelta3,
		InstMapDelta4, InstMapDelta).

recompute_instmap_delta_2(Atomic, some(Vars, Goal0), _, some(Vars, Goal),
		InstMap, InstMapDelta) -->
	recompute_instmap_delta(Atomic, Goal0, Goal, InstMap, InstMapDelta).

recompute_instmap_delta_2(_, higher_order_call(A, Vars, B, Modes, C), _,
		higher_order_call(A, Vars, B, Modes, C),
		_InstMap, InstMapDelta) -->
	=(ModuleInfo),
	{ instmap_delta_from_mode_list(Vars, Modes,
		ModuleInfo, InstMapDelta) }.

recompute_instmap_delta_2(_, call(PredId, ProcId, Args, D, E, F), _,
		call(PredId, ProcId, Args, D, E, F), InstMap, InstMapDelta) -->
	recompute_instmap_delta_call(PredId, ProcId,
		Args, InstMap, InstMapDelta).

recompute_instmap_delta_2(_, unify(A, B, UniMode0, Uni, E), GoalInfo, 
		unify(A, B, UniMode, Uni, E), InstMap, InstMapDelta) -->
	recompute_instmap_delta_unify(Uni, UniMode0, UniMode,
		GoalInfo, InstMap, InstMapDelta).

recompute_instmap_delta_2(_, pragma_c_code(A, B, PredId, ProcId, Args, F, G,
		H), _, pragma_c_code(A, B, PredId, ProcId, Args, F, G, H),
		InstMap, InstMapDelta) -->
	recompute_instmap_delta_call(PredId, ProcId,
		Args, InstMap, InstMapDelta).

%-----------------------------------------------------------------------------%

:- pred recompute_instmap_delta_conj(bool, list(hlds_goal), list(hlds_goal),
		instmap, instmap_delta, module_info, module_info).
:- mode recompute_instmap_delta_conj(in, in, out, in, out, in, out) is det.

recompute_instmap_delta_conj(_, [], [], _InstMap, InstMapDelta) -->
	{ instmap_delta_init_reachable(InstMapDelta) }.
recompute_instmap_delta_conj(Atomic, [Goal0 | Goals0], [Goal | Goals],
		InstMap0, InstMapDelta) -->
	recompute_instmap_delta(Atomic, Goal0, Goal,
		InstMap0, InstMapDelta0),
	{ instmap__apply_instmap_delta(InstMap0, InstMapDelta0, InstMap1) },
	recompute_instmap_delta_conj(Atomic, Goals0, Goals,
		InstMap1, InstMapDelta1),
	{ instmap_delta_apply_instmap_delta(InstMapDelta0, InstMapDelta1,
		InstMapDelta) }.

%-----------------------------------------------------------------------------%

:- pred recompute_instmap_delta_disj(bool, list(hlds_goal), list(hlds_goal),
		instmap, set(var), instmap_delta, module_info, module_info).
:- mode recompute_instmap_delta_disj(in, in, out, in, in, out, in, out) is det.

recompute_instmap_delta_disj(_, [], [], _, _, InstMapDelta) -->
	{ instmap_delta_init_unreachable(InstMapDelta) }.
recompute_instmap_delta_disj(Atomic, [Goal0], [Goal],
		InstMap, _, InstMapDelta) -->
	recompute_instmap_delta(Atomic, Goal0, Goal, InstMap, InstMapDelta).
recompute_instmap_delta_disj(Atomic, [Goal0 | Goals0], [Goal | Goals],
		InstMap, NonLocals, InstMapDelta) -->
	{ Goals0 = [_|_] },
	recompute_instmap_delta(Atomic, Goal0, Goal,
		InstMap, InstMapDelta0),
	recompute_instmap_delta_disj(Atomic, Goals0, Goals,
		InstMap, NonLocals, InstMapDelta1),
	merge_instmap_delta(InstMap, NonLocals, InstMapDelta0,
		InstMapDelta1, InstMapDelta).

%-----------------------------------------------------------------------------%

:- pred recompute_instmap_delta_cases(bool, var, list(case), list(case),
		instmap, set(var), instmap_delta, module_info, module_info).
:- mode recompute_instmap_delta_cases(in, in, in, out,
		in, in, out, in, out) is det.

recompute_instmap_delta_cases(_, _, [], [], _, _, InstMapDelta) -->
	{ instmap_delta_init_unreachable(InstMapDelta) }.
recompute_instmap_delta_cases(Atomic, Var, [Case0 | Cases0], [Case | Cases],
		InstMap0, NonLocals, InstMapDelta) -->
	{ Case0 = case(Functor, Goal0) },
	instmap__bind_var_to_functor(Var, Functor, InstMap0, InstMap),
	recompute_instmap_delta(Atomic, Goal0, Goal, InstMap, InstMapDelta0),
	instmap_delta_bind_var_to_functor(Var, Functor,
		InstMap0, InstMapDelta0, InstMapDelta1),
	{ Case = case(Functor, Goal) },
	recompute_instmap_delta_cases(Atomic, Var, Cases0, Cases,
		InstMap0, NonLocals, InstMapDelta2),
	merge_instmap_delta(InstMap0, NonLocals, InstMapDelta1,
		InstMapDelta2, InstMapDelta).

%-----------------------------------------------------------------------------%

:- pred recompute_instmap_delta_call(pred_id, proc_id,
		list(var), instmap, instmap_delta, module_info, module_info).
:- mode recompute_instmap_delta_call(in, in, in, in, out, in, out) is det.

recompute_instmap_delta_call(PredId, ProcId, Args, InstMap,
		InstMapDelta, ModuleInfo0, ModuleInfo) :-
	module_info_pred_proc_info(ModuleInfo0, PredId, ProcId, _, ProcInfo),
	proc_info_interface_determinism(ProcInfo, Detism),
	( determinism_components(Detism, _, at_most_zero) ->
		instmap_delta_init_unreachable(InstMapDelta),
		ModuleInfo = ModuleInfo0
	;
		proc_info_argmodes(ProcInfo, ArgModes0),
		recompute_instmap_delta_call_2(Args, InstMap,
			ArgModes0, ArgModes, ModuleInfo0, ModuleInfo),
		instmap_delta_from_mode_list(Args, ArgModes,
			ModuleInfo, InstMapDelta)
	).

:- pred recompute_instmap_delta_call_2(list(var), instmap, list(mode),
		list(mode), module_info, module_info).
:- mode recompute_instmap_delta_call_2(in, in, in, out, in, out) is det.

recompute_instmap_delta_call_2([], _, [], [], ModuleInfo, ModuleInfo).
recompute_instmap_delta_call_2([_|_], _, [], _, _, _) :-
	error("recompute_instmap_delta_call_2").
recompute_instmap_delta_call_2([], _, [_|_], _, _, _) :-
	error("recompute_instmap_delta_call_2").
recompute_instmap_delta_call_2([Arg | Args], InstMap, [Mode0 | Modes0],
		[Mode | Modes], ModuleInfo0, ModuleInfo) :-
	% This is similar to modecheck_set_var_inst.
	( instmap__is_reachable(InstMap) ->
		instmap__lookup_var(InstMap, Arg, ArgInst0),
		mode_get_insts(ModuleInfo0, Mode0, _, FinalInst),
		(
			abstractly_unify_inst(dead, ArgInst0, FinalInst,
				fake_unify, ModuleInfo0, UnifyInst, _,
				ModuleInfo1)
		->
			ModuleInfo2 = ModuleInfo1,
			Mode = (ArgInst0 -> UnifyInst),
			recompute_instmap_delta_call_2(Args, InstMap,
				Modes0, Modes, ModuleInfo2, ModuleInfo)
		;
			error("recompute_instmap_delta_call_2: unify_inst failed")
		)
	;
		Mode = (not_reached -> not_reached),
		Modes = [],
		ModuleInfo = ModuleInfo0
	).

:- pred recompute_instmap_delta_unify(unification, unify_mode, unify_mode,
	hlds_goal_info, instmap, instmap_delta, module_info, module_info).
:- mode recompute_instmap_delta_unify(in, in, out,
	in, in, out, in, out) is det.

recompute_instmap_delta_unify(Uni, UniMode0, UniMode, GoalInfo,
		InstMap, InstMapDelta, ModuleInfo, ModuleInfo) :-
	% Deconstructions are the only types of unifications
	% that can require updating of the instmap_delta after simplify.m
	% has been run.
	(
		Uni = deconstruct(Var, _ConsId, Vars, UniModes, _)
	->
		% Get the final inst of the deconstructed var, which
		% will be the same as in the old instmap.
		goal_info_get_instmap_delta(GoalInfo, OldInstMapDelta),
		instmap_delta_lookup_var(OldInstMapDelta, Var, FinalInst1),
		instmap__lookup_var(InstMap, Var, InitialInst),
		( inst_is_free(ModuleInfo, FinalInst1) ->
			% it wasn't in the instmap_delta, so the inst didn't
			% change.
			FinalInst = InitialInst
		;
			FinalInst = FinalInst1
		),
		UniModeToRhsMode =
			 lambda([UMode::in, Mode::out] is det, (
				UMode = ((_ - Inst0) -> (_ - Inst)),
				Mode = (Inst0 -> Inst)
			)),
		list__map(UniModeToRhsMode, UniModes, Modes),
		instmap_delta_from_mode_list([Var | Vars],
			[(InitialInst -> FinalInst) |  Modes],
			ModuleInfo, InstMapDelta),
		UniMode = UniMode0
	;	
		goal_info_get_instmap_delta(GoalInfo, InstMapDelta),
		UniMode = UniMode0
	).

%-----------------------------------------------------------------------------%

	% Arguments with final inst `clobbered' are dead, any
	% others are assumed to be live.

get_arg_lives([], _, []).
get_arg_lives([Mode|Modes], ModuleInfo, [IsLive|IsLives]) :-
	mode_get_insts(ModuleInfo, Mode, _InitialInst, FinalInst),
	( inst_is_clobbered(ModuleInfo, FinalInst) ->
		IsLive = dead
	;
		IsLive = live
	),
	get_arg_lives(Modes, ModuleInfo, IsLives).

%-----------------------------------------------------------------------------%

	% 
	% Predicates to make error messages more readable by stripping
	% "mercury_builtin" module qualifiers from modes and insts.
	% The interesting part is strip_builtin_qualifier_from_sym_name;
	% the rest is basically just recursive traversals.
	%

strip_builtin_qualifiers_from_mode_list(Modes0, Modes) :-
	list__map(strip_builtin_qualifiers_from_mode, Modes0, Modes).

:- pred strip_builtin_qualifiers_from_mode((mode)::in, (mode)::out) is det.

strip_builtin_qualifiers_from_mode((Initial0 -> Final0), (Initial -> Final)) :-
	strip_builtin_qualifiers_from_inst(Initial0, Initial),
	strip_builtin_qualifiers_from_inst(Final0, Final).

strip_builtin_qualifiers_from_mode(user_defined_mode(SymName0, Insts0),
				user_defined_mode(SymName, Insts)) :-
	strip_builtin_qualifiers_from_inst_list(Insts0, Insts),
	strip_builtin_qualifier_from_sym_name(SymName0, SymName).

strip_builtin_qualifier_from_cons_id(ConsId0, ConsId) :-
	( ConsId0 = cons(Name0, Arity) ->
		strip_builtin_qualifier_from_sym_name(Name0, Name),
		ConsId = cons(Name, Arity)
	;
		ConsId = ConsId0
	).

:- pred strip_builtin_qualifier_from_sym_name(sym_name::in,
						sym_name::out) is det.

strip_builtin_qualifier_from_sym_name(SymName0, SymName) :-
	( SymName0 = qualified("mercury_builtin", Name) ->
		SymName = unqualified(Name)
	;
		SymName = SymName0
	).

strip_builtin_qualifiers_from_inst_list(Insts0, Insts) :-
	list__map(strip_builtin_qualifiers_from_inst, Insts0, Insts).

strip_builtin_qualifiers_from_inst(inst_var(V), inst_var(V)).
strip_builtin_qualifiers_from_inst(not_reached, not_reached).
strip_builtin_qualifiers_from_inst(free, free).
strip_builtin_qualifiers_from_inst(free(Type), free(Type)).
strip_builtin_qualifiers_from_inst(any(Uniq), any(Uniq)).
strip_builtin_qualifiers_from_inst(ground(Uniq, Pred0), ground(Uniq, Pred)) :-
	strip_builtin_qualifiers_from_pred_inst(Pred0, Pred).
strip_builtin_qualifiers_from_inst(bound(Uniq, BoundInsts0),
					bound(Uniq, BoundInsts)) :-
	strip_builtin_qualifiers_from_bound_inst_list(BoundInsts0, BoundInsts).
strip_builtin_qualifiers_from_inst(defined_inst(Name0), Inst) :-
	strip_builtin_qualifiers_from_inst_name(Name0,
		defined_inst(Name0), Inst).
strip_builtin_qualifiers_from_inst(abstract_inst(Name0, Args0),
				abstract_inst(Name, Args)) :-
	strip_builtin_qualifier_from_sym_name(Name0, Name),
	strip_builtin_qualifiers_from_inst_list(Args0, Args).

:- pred strip_builtin_qualifiers_from_bound_inst_list(list(bound_inst)::in,
					list(bound_inst)::out) is det.
strip_builtin_qualifiers_from_bound_inst_list(Insts0, Insts) :-
	list__map(strip_builtin_qualifiers_from_bound_inst, Insts0, Insts).

:- pred strip_builtin_qualifiers_from_bound_inst(bound_inst::in,
					bound_inst::out) is det.
strip_builtin_qualifiers_from_bound_inst(BoundInst0, BoundInst) :-
	BoundInst0 = functor(ConsId0, Insts0),
	strip_builtin_qualifier_from_cons_id(ConsId0, ConsId),
	BoundInst = functor(ConsId, Insts),
	list__map(strip_builtin_qualifiers_from_inst, Insts0, Insts).

:- pred strip_builtin_qualifiers_from_inst_name(inst_name::in, (inst)::in,
		(inst)::out) is det.

strip_builtin_qualifiers_from_inst_name(InstName0, Inst0, Inst) :-
	( InstName0 = user_inst(SymName0, Insts0) ->
		strip_builtin_qualifier_from_sym_name(SymName0, SymName),
		strip_builtin_qualifiers_from_inst_list(Insts0, Insts),
		Inst = defined_inst(user_inst(SymName, Insts))
	; InstName0 = typed_inst(_, InstName1) ->
		% Don't output the $typed_inst in error messages.
		strip_builtin_qualifiers_from_inst_name(InstName1, Inst0, Inst)
	; InstName0 = typed_ground(Uniq, _Type) ->
		% Don't output the $typed_ground in error messages.
		Inst = ground(Uniq, no)
	;
		% for the compiler-generated insts, don't bother.
		Inst = Inst0
	).

:- pred strip_builtin_qualifiers_from_pred_inst(maybe(pred_inst_info)::in,
					maybe(pred_inst_info)::out) is det.

strip_builtin_qualifiers_from_pred_inst(no, no).
strip_builtin_qualifiers_from_pred_inst(yes(Pred0), yes(Pred)) :-
	Pred0 = pred_inst_info(Uniq, Modes0, Det),
	Pred = pred_inst_info(Uniq, Modes, Det),
	strip_builtin_qualifiers_from_mode_list(Modes0, Modes).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

normalise_insts([], _, []).
normalise_insts([Inst0|Insts0], ModuleInfo, [Inst|Insts]) :-
	normalise_inst(Inst0, ModuleInfo, Inst),
	normalise_insts(Insts0, ModuleInfo, Insts).

	% This is a bit of a hack.
	% The aim is to avoid non-termination due to the creation
	% of ever-expanding insts.
	% XXX should also normalise partially instantiated insts.

normalise_inst(Inst0, ModuleInfo, NormalisedInst) :-
	inst_expand(ModuleInfo, Inst0, Inst),
	( Inst = bound(_, _) ->
		(
			inst_is_ground(ModuleInfo, Inst),
			inst_is_unique(ModuleInfo, Inst)
		->
			NormalisedInst = ground(unique, no)
		;
			inst_is_ground(ModuleInfo, Inst),
			inst_is_mostly_unique(ModuleInfo, Inst)
		->
			NormalisedInst = ground(mostly_unique, no)
		;
			inst_is_ground(ModuleInfo, Inst),
			\+ inst_is_clobbered(ModuleInfo, Inst)
		->
			NormalisedInst = ground(shared, no)
		;
			% XXX need to limit the potential size of insts
			% here in order to avoid infinite loops in
			% mode inference
			NormalisedInst = Inst
		)
	;
		NormalisedInst = Inst
	).

%-----------------------------------------------------------------------------%

fixup_switch_var(Var, InstMap0, InstMap, Goal0, Goal) :-
	Goal0 = GoalExpr - GoalInfo0,
	goal_info_get_instmap_delta(GoalInfo0, InstMapDelta0),
	instmap__lookup_var(InstMap0, Var, Inst0),
	instmap__lookup_var(InstMap, Var, Inst),
	( Inst = Inst0 ->
		GoalInfo = GoalInfo0
	;
		instmap_delta_set(InstMapDelta0, Var, Inst, InstMapDelta),
		goal_info_set_instmap_delta(GoalInfo0, InstMapDelta, GoalInfo)
	),
	Goal = GoalExpr - GoalInfo.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
