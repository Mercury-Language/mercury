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
:- import_module hlds, int, string, list, prog_io.

	% mode_get_insts returns the initial instantiatedness and
	% the final instantiatedness for a given mode.
	%
:- pred mode_get_insts(module_info, mode, inst, inst).
:- mode mode_get_insts(in, in, out, out) is det.

:- pred mode_is_input(module_info, mode).
:- mode mode_is_input(in, in) is semidet.

:- pred mode_is_output(module_info, mode).
:- mode mode_is_output(in, in) is semidet.

:- pred mode_is_unused(module_info, mode).
:- mode mode_is_unused(in, in) is semidet.

:- pred inst_is_ground(module_info, inst).
:- mode inst_is_ground(in, in) is semidet.

:- pred inst_is_unique(module_info, inst).
:- mode inst_is_unique(in, in) is semidet.

:- pred inst_is_shared(module_info, inst).
:- mode inst_is_shared(in, in) is semidet.

:- pred inst_is_clobbered(module_info, inst).
:- mode inst_is_clobbered(in, in) is semidet.

:- pred inst_list_is_ground(list(inst), module_info).
:- mode inst_list_is_ground(in, in) is semidet.

:- pred inst_list_is_unique(list(inst), module_info).
:- mode inst_list_is_unique(in, in) is semidet.

:- pred inst_list_is_shared(list(inst), module_info).
:- mode inst_list_is_shared(in, in) is semidet.

:- pred bound_inst_list_is_ground(list(bound_inst), module_info).
:- mode bound_inst_list_is_ground(in, in) is semidet.

:- pred bound_inst_list_is_unique(list(bound_inst), module_info).
:- mode bound_inst_list_is_unique(in, in) is semidet.

:- pred bound_inst_list_is_shared(list(bound_inst), module_info).
:- mode bound_inst_list_is_shared(in, in) is semidet.

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

:- pred mode_id_to_int(mode_id, int).
:- mode mode_id_to_int(in, out) is det.

:- pred mode_list_get_initial_insts(list(mode), module_info, list(inst)).
:- mode mode_list_get_initial_insts(in, in, out) is det.

:- pred mode_list_get_final_insts(list(mode), module_info, list(inst)).
:- mode mode_list_get_final_insts(in, in, out) is det.

:- pred mode_util__modes_to_uni_modes(list(mode), list(mode), module_info,
							list(uni_mode)).
:- mode mode_util__modes_to_uni_modes(in, in, in, out) is det.

:- pred mode_list_from_inst_list(list(inst), list(mode)).
:- mode mode_list_from_inst_list(in, out) is det.

	% Given a user-defined or compiler-defined inst name,
	% lookup the corresponding inst in the inst table.
	%
:- pred inst_lookup(module_info, inst_name, inst).
:- mode inst_lookup(in, in, out) is det.

	% Initialize an empty instmap.
	%
:- pred instmap_init(instmap).
:- mode instmap_init(out) is det.

	% Given an instmap and an instmap_delta, apply the instmap_delta
	% to the instmap to produce a new instmap.
	%
:- pred apply_instmap_delta(instmap, instmap_delta, instmap).
:- mode apply_instmap_delta(in, in, out) is det.

	% Use the instmap deltas for all the atomic sub-goals to recompute
	% the instmap deltas for all the non-atomic sub-goals of a goal.
	% Used to ensure that the instmap deltas remain valid after
	% code has been re-arranged, e.g. by followcode.
	% This also takes the module_info as input and output since it
	% may need to insert new merge_insts into the merge_inst table.
	%
:- pred recompute_instmap_delta(hlds__goal, hlds__goal,
				module_info, module_info).
:- mode recompute_instmap_delta(in, out, in, out) is det.

	% Given an instmap and a variable, determine the inst of
	% that variable.
	%
:- pred instmap_lookup_var(instmap, var, inst).
:- mode instmap_lookup_var(in, in, out) is det.

:- pred instmapping_lookup_var(instmapping, var, inst).
:- mode instmapping_lookup_var(in, in, out) is det.

	% Given corresponding lists of types and modes, produce a new
	% list of modes which includes the information provided by the
	% corresponding types.
	%
:- pred propagate_type_info_mode_list(list(type), module_info, list(mode),
				list(mode)).
:- mode propagate_type_info_mode_list(in, in, in, out) is det.

	% Given corresponding lists of types and insts, produce a new
	% list of insts which includes the information provided by the
	% corresponding types.
	%
:- pred propagate_type_info_inst_list(list(type), module_info, list(inst),
				list(inst)).
:- mode propagate_type_info_inst_list(in, in, in, out) is det.

	% Given a type and an inst, produce a new inst which includes
	% the information provided by the type.
	%
:- pred propagate_type_info_inst(type, module_info, inst, inst).
:- mode propagate_type_info_inst(in, in, in, out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module require, map, set, term, std_util, assoc_list.
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

mode_list_from_inst_list([], []).
mode_list_from_inst_list([Inst|Insts], [(Inst -> Inst) | Modes]) :-
	mode_list_from_inst_list(Insts, Modes).

%-----------------------------------------------------------------------------%

	% A mode is considered an input mode if the top-level
	% node is input.

mode_is_input(ModuleInfo, Mode) :-
	mode_get_insts(ModuleInfo, Mode, InitialInst, _FinalInst),
	inst_is_bound(ModuleInfo, InitialInst).

	% A mode is considered an output mode if the top-level
	% node is output.

mode_is_output(ModuleInfo, Mode) :-
	mode_get_insts(ModuleInfo, Mode, InitialInst, FinalInst),
	inst_is_free(ModuleInfo, InitialInst),
	inst_is_bound(ModuleInfo, FinalInst).

	% A mode is considered a unused mode if it is equivalent
	% to free->free.

mode_is_unused(ModuleInfo, Mode) :-
	mode_get_insts(ModuleInfo, Mode, InitialInst, FinalInst),
	inst_is_free(ModuleInfo, InitialInst),
	inst_is_free(ModuleInfo, FinalInst).

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
	% or is a user-defined inst which is defined as `clobbered'.

:- inst_is_clobbered(_, X) when X.		% NU-Prolog indexing.

inst_is_clobbered(_, ground(clobbered, _)).
inst_is_clobbered(_, bound(clobbered, _)).
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
inst_is_free(_, inst_var(_)) :-
	error("internal error: uninstantiated inst parameter").
inst_is_free(ModuleInfo, defined_inst(InstName)) :-
	inst_lookup(ModuleInfo, InstName, Inst),
	inst_is_free(ModuleInfo, Inst).

	% inst_is_bound succeeds iff the inst passed is not `free'
	% or is a user-defined inst which is not defined as `free'.
	% Abstract insts must be bound.

:- inst_is_bound(_, X) when X.		% NU-Prolog indexing.

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

:- inst_is_bound_to_functors(_, _, X) when X.		% NU-Prolog indexing.

inst_is_bound_to_functors(_, bound(_Uniq, Functors), Functors).
inst_is_bound_to_functors(_, inst_var(_), _) :-
	error("internal error: uninstantiated inst parameter").
inst_is_bound_to_functors(ModuleInfo, defined_inst(InstName), Functors)
		:-
	inst_lookup(ModuleInfo, InstName, Inst),
	inst_is_bound_to_functors(ModuleInfo, Inst, Functors).

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

inst_is_unique_2(ModuleInfo, bound(unique, List), _, Expansions) :-
	bound_inst_list_is_unique_2(List, ModuleInfo, Expansions).
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

	% inst_is_shared succeeds iff the inst passed is shared
	% or free.  Abstract insts are not considered shared.

inst_is_shared(ModuleInfo, Inst) :-
	set__init(Expansions),
	inst_is_shared_2(ModuleInfo, Inst, Inst, Expansions).

	% The third argument must be the same as the second.
	% The fourth arg is the set of insts which have already
	% been expanded - we use this to avoid going into an
	% infinite loop.

:- pred inst_is_shared_2(module_info, inst, inst, set(inst)).
:- mode inst_is_shared_2(in, in, in, in) is semidet.

:- inst_is_shared_2(_, X, _, _) when X.		% NU-Prolog indexing.

inst_is_shared_2(ModuleInfo, bound(shared, List), _, Expansions) :-
	bound_inst_list_is_shared_2(List, ModuleInfo, Expansions).
inst_is_shared_2(_, free, _, _).
inst_is_shared_2(_, ground(shared, _), _, _).
inst_is_shared_2(_, inst_var(_), _, _) :-
	error("internal error: uninstantiated inst parameter").
inst_is_shared_2(ModuleInfo, defined_inst(InstName), Inst, Expansions) :-
	( set__member(Inst, Expansions) ->
		true
	;
		set__insert(Expansions, Inst, Expansions2),
		inst_lookup(ModuleInfo, InstName, Inst2),
		inst_is_shared_2(ModuleInfo, Inst2, Inst2, Expansions2)
	).

%-----------------------------------------------------------------------------%

bound_inst_list_is_ground([], _).
bound_inst_list_is_ground([functor(_Name, Args)|BoundInsts], ModuleInfo) :-
	inst_list_is_ground(Args, ModuleInfo),
	bound_inst_list_is_ground(BoundInsts, ModuleInfo).

bound_inst_list_is_unique([], _).
bound_inst_list_is_unique([functor(_Name, Args)|BoundInsts], ModuleInfo) :-
	inst_list_is_unique(Args, ModuleInfo),
	bound_inst_list_is_unique(BoundInsts, ModuleInfo).

bound_inst_list_is_shared([], _).
bound_inst_list_is_shared([functor(_Name, Args)|BoundInsts], ModuleInfo) :-
	inst_list_is_shared(Args, ModuleInfo),
	bound_inst_list_is_shared(BoundInsts, ModuleInfo).

%-----------------------------------------------------------------------------%

:- pred bound_inst_list_is_ground_2(list(bound_inst), module_info, set(inst)).
:- mode bound_inst_list_is_ground_2(in, in, in) is semidet.

bound_inst_list_is_ground_2([], _, _).
bound_inst_list_is_ground_2([functor(_Name, Args)|BoundInsts], ModuleInfo,
		Expansions) :-
	inst_list_is_ground_2(Args, ModuleInfo, Expansions),
	bound_inst_list_is_ground_2(BoundInsts, ModuleInfo, Expansions).

:- pred bound_inst_list_is_unique_2(list(bound_inst), module_info, set(inst)).
:- mode bound_inst_list_is_unique_2(in, in, in) is semidet.

bound_inst_list_is_unique_2([], _, _).
bound_inst_list_is_unique_2([functor(_Name, Args)|BoundInsts], ModuleInfo,
		Expansions) :-
	inst_list_is_unique_2(Args, ModuleInfo, Expansions),
	bound_inst_list_is_unique_2(BoundInsts, ModuleInfo, Expansions).

:- pred bound_inst_list_is_shared_2(list(bound_inst), module_info, set(inst)).
:- mode bound_inst_list_is_shared_2(in, in, in) is semidet.

bound_inst_list_is_shared_2([], _, _).
bound_inst_list_is_shared_2([functor(_Name, Args)|BoundInsts], ModuleInfo,
		Expansions) :-
	inst_list_is_shared_2(Args, ModuleInfo, Expansions),
	bound_inst_list_is_shared_2(BoundInsts, ModuleInfo, Expansions).

%-----------------------------------------------------------------------------%

inst_list_is_ground([], _).
inst_list_is_ground([Inst | Insts], ModuleInfo) :-
	inst_is_ground(ModuleInfo, Inst),
	inst_list_is_ground(Insts, ModuleInfo).

inst_list_is_unique([], _).
inst_list_is_unique([Inst | Insts], ModuleInfo) :-
	inst_is_unique(ModuleInfo, Inst),
	inst_list_is_unique(Insts, ModuleInfo).

inst_list_is_shared([], _).
inst_list_is_shared([Inst | Insts], ModuleInfo) :-
	inst_is_shared(ModuleInfo, Inst),
	inst_list_is_shared(Insts, ModuleInfo).

%-----------------------------------------------------------------------------%

:- pred inst_list_is_ground_2(list(inst), module_info, set(inst)).
:- mode inst_list_is_ground_2(in, in, in) is semidet.

inst_list_is_ground_2([], _, _).
inst_list_is_ground_2([Inst | Insts], ModuleInfo, Expansions) :-
	inst_is_ground_2(ModuleInfo, Inst, Inst, Expansions),
	inst_list_is_ground_2(Insts, ModuleInfo, Expansions).

:- pred inst_list_is_unique_2(list(inst), module_info, set(inst)).
:- mode inst_list_is_unique_2(in, in, in) is semidet.

inst_list_is_unique_2([], _, _).
inst_list_is_unique_2([Inst | Insts], ModuleInfo, Expansions) :-
	inst_is_unique_2(ModuleInfo, Inst, Inst, Expansions),
	inst_list_is_unique_2(Insts, ModuleInfo, Expansions).

:- pred inst_list_is_shared_2(list(inst), module_info, set(inst)).
:- mode inst_list_is_shared_2(in, in, in) is semidet.

inst_list_is_shared_2([], _, _).
inst_list_is_shared_2([Inst | Insts], ModuleInfo, Expansions) :-
	inst_is_shared_2(ModuleInfo, Inst, Inst, Expansions),
	inst_list_is_shared_2(Insts, ModuleInfo, Expansions).

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
	; InstName = user_inst(Name, Args),
		module_info_insts(ModuleInfo, InstTable),
		inst_table_get_user_insts(InstTable, UserInstTable),
		list__length(Args, Arity),
		( map__search(UserInstTable, Name - Arity, InstDefn) ->
			InstDefn = hlds__inst_defn(_VarSet, Params, Inst0,
					_Cond, _C),
			inst_lookup_subst_args(Inst0, Params, Name, Args, Inst)
		;
			Inst = abstract_inst(Name, Args)
		)
	; InstName = typed_ground(Uniq, Type),
		propagate_type_info_inst(Type, ModuleInfo, ground(Uniq, no),
			Inst)
	; InstName = typed_inst(Type, TypedInstName),
		inst_lookup_2(TypedInstName, ModuleInfo, Inst0),
		propagate_type_info_inst(Type, ModuleInfo, Inst0, Inst)
	),
	!.

%-----------------------------------------------------------------------------%

	% Given corresponding lists of types and modes, produce a new
	% list of modes which includes the information provided by the
	% corresponding types.

:- propagate_type_info_mode_list(A, B, _, _) when A and B.

propagate_type_info_mode_list([], _, [], []).
propagate_type_info_mode_list([Type | Types], ModuleInfo, [Mode0 | Modes0],
		[Mode | Modes]) :-
	propagate_type_info_mode(Type, ModuleInfo, Mode0, Mode),
	propagate_type_info_mode_list(Types, ModuleInfo, Modes0, Modes).
propagate_type_info_mode_list([], _, [_|_], []) :-
	error("propagate_type_info_mode_list: length mismatch").
propagate_type_info_mode_list([_|_], _, [], []) :-
	error("propagate_type_info_mode_list: length mismatch").

:- propagate_type_info_inst_list(A, B, _, _) when A and B.

propagate_type_info_inst_list([], _, [], []).
propagate_type_info_inst_list([Type | Types], ModuleInfo, [Inst0 | Insts0],
		[Inst | Insts]) :-
	propagate_type_info_inst(Type, ModuleInfo, Inst0, Inst),
	propagate_type_info_inst_list(Types, ModuleInfo, Insts0, Insts).
propagate_type_info_inst_list([], _, [_|_], []) :-
	error("propagate_type_info_inst_list: length mismatch").
propagate_type_info_inst_list([_|_], _, [], []) :-
	error("propagate_type_info_inst_list: length mismatch").

	% Given a type and a mode, produce a new mode which includes
	% the information provided by the type.

:- pred propagate_type_info_mode(type, module_info, mode, mode).
:- mode propagate_type_info_mode(in, in, in, out) is det.

propagate_type_info_mode(Type, ModuleInfo, Mode0, Mode) :-
	mode_get_insts(ModuleInfo, Mode0, InitialInst0, FinalInst0),
	ex_propagate_type_info_inst(Type, ModuleInfo, InitialInst0,
		InitialInst),
	ex_propagate_type_info_inst(Type, ModuleInfo, FinalInst0, FinalInst),
	Mode = (InitialInst -> FinalInst).

	% Given a type and an inst, produce a new inst which includes
	% the information provided by the type.

propagate_type_info_inst(Type, ModuleInfo, Inst0, Inst) :-
	(
		type_constructors(Type, ModuleInfo, Constructors)
	->
		propagate_ctor_info(Inst0, Type, Constructors, ModuleInfo,
			Inst)
	;
		Inst = Inst0
	).

	% Given a type and an inst, produce a new inst which includes
	% the information provided by the type.

:- pred ex_propagate_type_info_inst(type, module_info, inst, inst).
:- mode ex_propagate_type_info_inst(in, in, in, out) is det.

ex_propagate_type_info_inst(Type, ModuleInfo, Inst0, Inst) :-
	(
		type_constructors(Type, ModuleInfo, Constructors)
	->
		ex_propagate_ctor_info(Inst0, Type, Constructors, ModuleInfo,
			Inst)
	;
		Inst = Inst0
	).

%-----------------------------------------------------------------------------%

:- pred propagate_ctor_info(inst, type, list(constructor), module_info, inst).
:- mode propagate_ctor_info(in, in, in, in, out) is det.

% propagate_ctor_info(free, Type, _, _, free(Type)).	% temporarily disabled
propagate_ctor_info(free, _Type, _, _, free).	% XXX temporary hack

propagate_ctor_info(free(_), _, _, _, _) :-
	error("propagate_ctor_info: type info already present").
propagate_ctor_info(bound(Uniq, BoundInsts0), _Type, Constructors, ModuleInfo,
		Inst) :-
	propagate_ctor_info_2(BoundInsts0, Constructors, ModuleInfo,
		BoundInsts),
	( BoundInsts = [] ->
		Inst = not_reached
	;
		% XXX do we need to sort the BoundInsts?
		Inst = bound(Uniq, BoundInsts)
	).
propagate_ctor_info(ground(Uniq, no), _Type, Constructors, ModuleInfo, Inst) :-
	constructors_to_bound_insts(Constructors, Uniq, ModuleInfo,
		BoundInsts0),
	list__sort_and_remove_dups(BoundInsts0, BoundInsts),
	Inst = bound(Uniq, BoundInsts).
propagate_ctor_info(ground(Uniq, yes(PredInstInfo)), _, _, _,
	% for higher-order pred modes, the information we need is already
	% in the inst, so we leave it unchanged
			ground(Uniq, yes(PredInstInfo))).
propagate_ctor_info(not_reached, _Type, _Constructors, _ModuleInfo,
		not_reached).
propagate_ctor_info(inst_var(_), _, _, _, _) :-
	error("propagate_ctor_info: unbound inst var").
propagate_ctor_info(abstract_inst(Name, Args), _, _, _,
		abstract_inst(Name, Args)).	% XXX loses info
propagate_ctor_info(defined_inst(InstName), Type, Ctors, ModuleInfo, Inst) :-
	inst_lookup(ModuleInfo, InstName, Inst0),
	propagate_ctor_info(Inst0, Type, Ctors, ModuleInfo, Inst).

:- pred ex_propagate_ctor_info(inst, type, list(constructor), module_info, inst).
:- mode ex_propagate_ctor_info(in, in, in, in, out) is det.

% ex_propagate_ctor_info(free, Type, _, _, free(Type)).	% temporarily disabled
ex_propagate_ctor_info(free, _Type, _, _, free).	% XXX temporary hack

ex_propagate_ctor_info(free(_), _, _, _, _) :-
	error("ex_propagate_ctor_info: type info already present").
ex_propagate_ctor_info(bound(Uniq, BoundInsts0), _Type, Constructors,
		ModuleInfo, Inst) :-
	propagate_ctor_info_2(BoundInsts0, Constructors, ModuleInfo,
		BoundInsts),
	( BoundInsts = [] ->
		Inst = not_reached
	;
		% XXX do we need to sort the BoundInsts?
		Inst = bound(Uniq, BoundInsts)
	).
ex_propagate_ctor_info(ground(Uniq, no), Type, _, _, Inst) :-
	Inst = defined_inst(typed_ground(Uniq, Type)).
ex_propagate_ctor_info(ground(Uniq, yes(PredInstInfo)), _, _, _,
	% for higher-order pred modes, the information we need is already
	% in the inst, so we leave it unchanged
			ground(Uniq, yes(PredInstInfo))).
ex_propagate_ctor_info(not_reached, _Type, _Constructors, _ModuleInfo,
		not_reached).
ex_propagate_ctor_info(inst_var(_), _, _, _, _) :-
	error("propagate_ctor_info: unbound inst var").
ex_propagate_ctor_info(abstract_inst(Name, Args), _, _, _,
		abstract_inst(Name, Args)).	% XXX loses info
ex_propagate_ctor_info(defined_inst(InstName), Type, _, _,
		defined_inst(typed_inst(Type, InstName))).

:- pred constructors_to_bound_insts(list(constructor), uniqueness, module_info,
				list(bound_inst)).
:- mode constructors_to_bound_insts(in, in, in, out) is det.

constructors_to_bound_insts([], _, _, []).
constructors_to_bound_insts([Ctor | Ctors], Uniq, ModuleInfo,
		[BoundInst | BoundInsts]) :-
	Ctor = Name0 - Args,
	type_list_to_inst_list(Args, Uniq, Insts),
	unqualify_name(Name0, Name),
	BoundInst = functor(term_atom(Name), Insts),
	constructors_to_bound_insts(Ctors, Uniq, ModuleInfo, BoundInsts).

:- pred type_list_to_inst_list(list(type), uniqueness, list(inst)).
:- mode type_list_to_inst_list(in, in, out) is det.

type_list_to_inst_list([], _, []).
type_list_to_inst_list([Type | Types], Uniq, [Inst | Insts]) :-
	Inst = defined_inst(typed_ground(Uniq, Type)),
	type_list_to_inst_list(Types, Uniq, Insts).

:- pred propagate_ctor_info_2(list(bound_inst), list(constructor),
		module_info, list(bound_inst)).
:- mode propagate_ctor_info_2(in, in, in, out) is det.

propagate_ctor_info_2(BoundInsts0, _Constructors, _ModuleInfo, BoundInsts) :-
	BoundInsts = BoundInsts0.	% XXX Stub only!!

%-----------------------------------------------------------------------------%

:- pred inst_lookup_subst_args(hlds__inst_body, list(inst_param), sym_name,
			list(inst), inst).
:- mode inst_lookup_subst_args(in, in, in, in, out) is det.

inst_lookup_subst_args(eqv_inst(Inst0), Params, _Name, Args, Inst) :-
	inst_substitute_arg_list(Inst0, Params, Args, Inst).
inst_lookup_subst_args(abstract_inst, _Params, Name, Args,
		abstract_inst(Name, Args)).

%-----------------------------------------------------------------------------%
	% mode_get_insts returns the initial instantiatedness and
	% the final instantiatedness for a given mode.

mode_get_insts(_ModuleInfo, (InitialInst -> FinalInst), InitialInst, FinalInst).
mode_get_insts(ModuleInfo, user_defined_mode(Name, Args), Initial, Final) :-
	list__length(Args, Arity),
	module_info_modes(ModuleInfo, Modes),
	map__lookup(Modes, Name - Arity, HLDS_Mode),
	HLDS_Mode = hlds__mode_defn(_VarSet, Params, ModeDefn, _Cond, _Context),
	ModeDefn = eqv_mode(Mode0),
	mode_substitute_arg_list(Mode0, Params, Args, Mode),
	mode_get_insts(ModuleInfo, Mode, Initial, Final).

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

inst_apply_substitution(free, _, free).
inst_apply_substitution(free(T), _, free(T)).
inst_apply_substitution(ground(Uniq, PredStuff), _, ground(Uniq, PredStuff)).
inst_apply_substitution(bound(Uniq, Alts0), Subst, bound(Uniq, Alts)) :-
	alt_list_apply_substitution(Alts0, Subst, Alts).
inst_apply_substitution(not_reached, _, not_reached).
inst_apply_substitution(inst_var(Var), Subst, Result) :-
	(
		% XXX should params be vars?
		map__search(Subst, term_variable(Var), Replacement)
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

%-----------------------------------------------------------------------------%

	% In case we later decided to change the representation
	% of mode_ids.

mode_id_to_int(_ - X, X).

%-----------------------------------------------------------------------------%

	% Initialize an empty instmap.

instmap_init(reachable(InstMapping)) :-
	map__init(InstMapping).

%-----------------------------------------------------------------------------%

	% Given an instmap and a variable, determine the inst of
	% that variable.

instmap_lookup_var(unreachable, _Var, not_reached).
instmap_lookup_var(reachable(InstMap), Var, Inst) :-
	instmapping_lookup_var(InstMap, Var, Inst).

instmapping_lookup_var(InstMap, Var, Inst) :-
	( map__search(InstMap, Var, VarInst) ->
		Inst = VarInst
	;
		Inst = free
	).

%-----------------------------------------------------------------------------%

	% Given two instmaps, overlay the entries in the second map 
	% on top of those in the first map to produce a new map.

apply_instmap_delta(unreachable, _, unreachable).
apply_instmap_delta(reachable(_), unreachable, unreachable).
apply_instmap_delta(reachable(InstMapping0), reachable(InstMappingDelta), 
			reachable(InstMapping)) :-
	map__overlay(InstMapping0, InstMappingDelta, InstMapping).

%-----------------------------------------------------------------------------%

:- pred instmap_restrict(instmap, set(var), instmap).
:- mode instmap_restrict(in, in, out) is det.

instmap_restrict(unreachable, _, unreachable).
instmap_restrict(reachable(InstMapping0), Vars, reachable(InstMapping)) :-
	map_restrict(InstMapping0, Vars, InstMapping).

:- pred map_restrict(map(K,V), set(K), map(K,V)).
:- mode map_restrict(in, in, out) is det.

map_restrict(Map0, Domain0, Map) :-
	map__keys(Map0, MapKeys),
	set__sorted_list_to_set(MapKeys, MapKeysSet),
	set__intersect(Domain0, MapKeysSet, Domain),
	set__to_sorted_list(Domain, Keys),
	map__apply_to_list(Keys, Map0, Values),
	assoc_list__from_corresponding_lists(Keys, Values, AssocList),
	map__from_sorted_assoc_list(AssocList, Map).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Use the instmap deltas for all the atomic sub-goals to recompute
	% the instmap deltas for all the non-atomic sub-goals of a goal.
	% Used to ensure that the instmap deltas remain valid after
	% code has been re-arranged, e.g. by followcode.

recompute_instmap_delta(Goal0, Goal) -->
	recompute_instmap_delta(Goal0, Goal, _).

:- pred recompute_instmap_delta(hlds__goal, hlds__goal, instmap_delta,
				module_info, module_info).
:- mode recompute_instmap_delta(in, out, out, in, out) is det.

recompute_instmap_delta(Goal0 - GoalInfo0, Goal - GoalInfo, InstMapDelta) -->
	( { goal_is_atomic(Goal0) } ->
		{ goal_info_get_instmap_delta(GoalInfo0, InstMapDelta) },
		{ Goal = Goal0 },
		{ GoalInfo = GoalInfo0 }
	;
		recompute_instmap_delta_2(Goal0, Goal, InstMapDelta0),
		{ goal_info_get_nonlocals(GoalInfo0, NonLocals) },
		{ instmap_restrict(InstMapDelta0, NonLocals, InstMapDelta) },
		{ goal_info_set_instmap_delta(GoalInfo0, InstMapDelta,
			GoalInfo) }
	).

:- pred recompute_instmap_delta_2(hlds__goal_expr, hlds__goal_expr,
				instmap_delta, module_info, module_info).
:- mode recompute_instmap_delta_2(in, out, out, in, out) is det.

recompute_instmap_delta_2(switch(Var, Det, Cases0), switch(Var, Det, Cases),
		InstMapDelta) -->
	recompute_instmap_delta_cases(Cases0, Cases, InstMapDelta).

recompute_instmap_delta_2(conj(Goals0), conj(Goals), InstMapDelta) -->
	recompute_instmap_delta_conj(Goals0, Goals, InstMapDelta).

recompute_instmap_delta_2(disj(Goals0), disj(Goals), InstMapDelta) -->
	recompute_instmap_delta_disj(Goals0, Goals, InstMapDelta).

recompute_instmap_delta_2(not(Goal0), not(Goal), InstMapDelta) -->
	{ instmap_init(InstMapDelta) },
	recompute_instmap_delta(Goal0, Goal).

recompute_instmap_delta_2(if_then_else(Vars,A0,B0,C0),
			if_then_else(Vars,A,B,C), InstMapDelta) -->
	recompute_instmap_delta(A0, A, InstMapDelta1),
	recompute_instmap_delta(B0, B, InstMapDelta2),
	recompute_instmap_delta(C0, C, InstMapDelta3),
	{ apply_instmap_delta(InstMapDelta1, InstMapDelta2, InstMapDelta4) },
	merge_instmap_delta(InstMapDelta3, InstMapDelta4, InstMapDelta).

recompute_instmap_delta_2(some(Vars, Goal0), some(Vars, Goal), InstMapDelta) -->
	recompute_instmap_delta(Goal0, Goal, InstMapDelta).

	% calls and unifies shouldn't occur, since atomic goals are
	% handled directly in recompute_instmap_delta

recompute_instmap_delta_2(call(_, _, _, _, _, _, _), _, _) -->
	{ error("recompute_instmap_delta: recomputing for atomic goal (call)")
	}.

recompute_instmap_delta_2(unify(_, _, _, _, _), _, _) -->
	{ error("recompute_instmap_delta: recomputing for atomic goal (unify)")
	}.

recompute_instmap_delta_2(pragma_c_code(_, _, _, _, _), _, _) -->
	{ error("recompute_instmap_delta: recomputing for atomic goal (pragma)")
	}.

%-----------------------------------------------------------------------------%

:- pred recompute_instmap_delta_conj(list(hlds__goal), list(hlds__goal),
		instmap_delta, module_info, module_info).
:- mode recompute_instmap_delta_conj(in, out, out, in, out) is det.

recompute_instmap_delta_conj([], [], InstMapDelta) -->
	{ instmap_init(InstMapDelta) }.
recompute_instmap_delta_conj([Goal0 | Goals0], [Goal | Goals], InstMapDelta) -->
	recompute_instmap_delta(Goal0, Goal, InstMapDelta0),
	recompute_instmap_delta_conj(Goals0, Goals, InstMapDelta1),
	{ apply_instmap_delta(InstMapDelta0, InstMapDelta1, InstMapDelta) }.

%-----------------------------------------------------------------------------%

:- pred recompute_instmap_delta_disj(list(hlds__goal), list(hlds__goal),
		instmap_delta, module_info, module_info).
:- mode recompute_instmap_delta_disj(in, out, out, in, out) is det.

recompute_instmap_delta_disj([], [], InstMapDelta) -->
	{ instmap_init(InstMapDelta) }.
recompute_instmap_delta_disj([Goal0], [Goal], InstMapDelta) -->
	recompute_instmap_delta(Goal0, Goal, InstMapDelta).
recompute_instmap_delta_disj([Goal0 | Goals0], [Goal | Goals], InstMapDelta)
		-->
	{ Goals0 = [_|_] },
	recompute_instmap_delta(Goal0, Goal, InstMapDelta0),
	recompute_instmap_delta_disj(Goals0, Goals, InstMapDelta1),
	merge_instmap_delta(InstMapDelta0, InstMapDelta1, InstMapDelta).

%-----------------------------------------------------------------------------%

:- pred recompute_instmap_delta_cases(list(case), list(case), instmap_delta,
					module_info, module_info).
:- mode recompute_instmap_delta_cases(in, out, out, in, out) is det.

recompute_instmap_delta_cases([], [], InstMapDelta) -->
	{ instmap_init(InstMapDelta) }.
recompute_instmap_delta_cases([Case0], [Case], InstMapDelta) -->
	{ Case0 = case(Functor, Goal0) },
	recompute_instmap_delta(Goal0, Goal, InstMapDelta),
	{ Case = case(Functor, Goal) }.
recompute_instmap_delta_cases([Case0 | Cases0], [Case | Cases], InstMapDelta) 
		-->
	{ Cases0 = [_|_] },
	{ Case0 = case(Functor, Goal0) },
	recompute_instmap_delta(Goal0, Goal, InstMapDelta0),
	{ Case = case(Functor, Goal) },
	recompute_instmap_delta_cases(Cases0, Cases, InstMapDelta1),
	merge_instmap_delta(InstMapDelta0, InstMapDelta1, InstMapDelta).

%-----------------------------------------------------------------------------%

	% Given two instmap deltas, merge them to produce a new instmap.

:- pred merge_instmap_delta(instmap_delta, instmap_delta, instmap,
				module_info, module_info).
:- mode merge_instmap_delta(in, in, out, in, out) is det.

merge_instmap_delta(unreachable, InstMap, InstMap) --> [].
merge_instmap_delta(reachable(InstMapping), unreachable,
				reachable(InstMapping)) --> [].
merge_instmap_delta(reachable(InstMappingA), reachable(InstMappingB),
			reachable(InstMapping))  -->
	merge_instmapping_delta(InstMappingA, InstMappingB, InstMapping).

:- pred merge_instmapping_delta(instmapping, instmapping, instmapping,
				module_info, module_info).
:- mode merge_instmapping_delta(in, in, out, in, out) is det.

merge_instmapping_delta(InstMappingA, InstMappingB, InstMapping) -->
	{ map__keys(InstMappingA, VarsInA) },
	merge_instmapping_delta_2(VarsInA, InstMappingA, InstMappingB,
		InstMapping).

:- pred merge_instmapping_delta_2(list(var), instmapping, instmapping,
				instmapping, module_info, module_info).
:- mode merge_instmapping_delta_2(in, in, in, out, in, out) is det.

merge_instmapping_delta_2([], _, InstMapping, InstMapping, ModInfo, ModInfo).
merge_instmapping_delta_2([Var | Vars], MergeInstMapping, InstMapping0,
			InstMapping, ModuleInfo0, ModuleInfo) :-
	map__lookup(MergeInstMapping, Var, MergeInst),
	( map__search(InstMapping0, Var, Inst0) ->
	    ( inst_merge(Inst0, MergeInst, ModuleInfo0, Inst, ModuleInfoPrime) ->
		ModuleInfo1 = ModuleInfoPrime,
		map__det_update(InstMapping0, Var, Inst, InstMapping1)
	    ;
		error("merge_instmapping_delta_2: unexpected mode error")
	    )
	;
	    % if a variable only occurs in one of the instmap deltas,
	    % then mode correctness means that the delta must be adding
	    % information only, not binding the variable any further;
	    % since we don't know which path will actually get executed,
	    % we should not add that information - the merged delta should
	    % not have any entry for that variable.
	    ModuleInfo1 = ModuleInfo0,
	    InstMapping1 = InstMapping0
	),
	merge_instmapping_delta_2(Vars, MergeInstMapping, InstMapping1,
				InstMapping, ModuleInfo1, ModuleInfo).

%-----------------------------------------------------------------------------%
