%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% mode_util.nl - utility predicates dealing with modes and insts.

% Main author: fjh.

:- module mode_util.
:- interface.
:- import_module hlds, int, string, list, prog_io.

	% mode_get_insts returns the initial instantiatedness and
	% the final instantiatedness for a given mode.

:- pred mode_get_insts(module_info, mode, inst, inst).
:- mode mode_get_insts(in, in, out, out) is det.

	% XXX how should these predicates handle abstract insts?
	% In that case, we don't _know_ whether the mode is input
	% or not!

:- pred mode_is_input(module_info, mode).
:- mode mode_is_input(in, in) is semidet.

:- pred mode_is_output(module_info, mode).
:- mode mode_is_output(in, in) is semidet.

:- pred mode_is_unused(module_info, mode).
:- mode mode_is_unused(in, in) is semidet.

:- pred mode_util__modes_to_uni_modes(mode, list(mode), module_info,
							list(uni_mode)).
:- mode mode_util__modes_to_uni_modes(in, in, in, out) is det.

:- pred inst_is_ground(module_info, inst).
:- mode inst_is_ground(in, in) is semidet.

:- pred inst_list_is_ground(list(inst), module_info).
:- mode inst_list_is_ground(in, in) is semidet.

:- pred bound_inst_list_is_ground(list(bound_inst), module_info).
:- mode bound_inst_list_is_ground(in, in) is semidet.

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

:- pred mode_list_get_final_insts(list(mode), module_info, list(inst)).
:- mode mode_list_get_final_insts(in, in, out) is det.

:- pred mode_list_get_initial_insts(list(mode), module_info, list(inst)).
:- mode mode_list_get_initial_insts(in, in, out) is det.

	% Given a user-defined or compiler-defined inst name,
	% lookup the corresponding inst in the inst table.

:- pred inst_lookup(module_info, inst_name, inst).
:- mode inst_lookup(in, in, out) is det.

	% Given an instmap and an instmap_delta, apply the instmap_delta
	% to the instmap to produce a new instmap.

:- pred apply_instmap_delta(instmap, instmap_delta, instmap).
:- mode apply_instmap_delta(in, in, out) is det.

	% Given an instmap and a variable, determine the inst of
	% that variable.

:- pred instmap_lookup_var(instmap, var, inst).
:- mode instmap_lookup_var(in, in, out) is det.

:- pred instmapping_lookup_var(instmapping, var, inst).
:- mode instmapping_lookup_var(in, in, out) is det.

	% Given corresponding lists of types and modes, produce a new
	% list of modes which includes the information provided by the
	% corresponding types.

:- pred propagate_type_info_mode_list(list(type), module_info, list(mode),
				list(mode)).
:- mode propagate_type_info_mode_list(in, in, in, out) is det.

	% Given a type and an inst, produce a new inst which includes
	% the information provided by the type.

:- pred propagate_type_info_inst(type, module_info, inst, inst).
:- mode propagate_type_info_inst(in, in, in, out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module require, map, set, term, std_util.
:- import_module prog_util, type_util.

mode_list_get_final_insts([], _ModuleInfo, []).
mode_list_get_final_insts([Mode | Modes], ModuleInfo, [Inst | Insts]) :-
	mode_get_insts(ModuleInfo, Mode, _, Inst),
	mode_list_get_final_insts(Modes, ModuleInfo, Insts).

mode_list_get_initial_insts([], _ModuleInfo, []).
mode_list_get_initial_insts([Mode | Modes], ModuleInfo, [Inst | Insts]) :-
	mode_get_insts(ModuleInfo, Mode, Inst, _),
	mode_list_get_initial_insts(Modes, ModuleInfo, Insts).

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

mode_util__modes_to_uni_modes(_X, [], _ModuleInfo, []).
mode_util__modes_to_uni_modes(X, [M|Ms], ModuleInfo, [A|As]) :-
	X = (Initial0 -> Final0),
	M = (Initial1 -> Final1),
	A = ((Initial0 - Initial1) -> (Final0 - Final1)),
	mode_util__modes_to_uni_modes(X, Ms, ModuleInfo, As).

%-----------------------------------------------------------------------------%

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

inst_is_bound(_, ground).
inst_is_bound(_, bound(_)).
inst_is_bound(_, inst_var(_)) :-
	error("internal error: uninstantiated inst parameter").
inst_is_bound(ModuleInfo, defined_inst(InstName)) :-
	inst_lookup(ModuleInfo, InstName, Inst),
	inst_is_bound(ModuleInfo, Inst).
inst_is_bound(_, abstract_inst(_, _)).

	% inst_is_bound_to_functors succeeds iff the inst passed is
	% `bound(Functors)' or is a user-defined inst which expands to
	% `bound(Functors)'.

:- inst_is_bound_to_functors(_, _, X) when X.		% NU-Prolog indexing.

inst_is_bound_to_functors(_, bound(Functors), Functors).
inst_is_bound_to_functors(_, inst_var(_), _) :-
	error("internal error: uninstantiated inst parameter").
inst_is_bound_to_functors(ModuleInfo, defined_inst(InstName), Functors)
		:-
	inst_lookup(ModuleInfo, InstName, Inst),
	inst_is_bound_to_functors(ModuleInfo, Inst, Functors).

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

inst_is_ground_2(ModuleInfo, bound(List), _, Expansions) :-
	bound_inst_list_is_ground_2(List, ModuleInfo, Expansions).
inst_is_ground_2(_, ground, _, _).
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

bound_inst_list_is_ground([], _).
bound_inst_list_is_ground([functor(_Name, Args)|BoundInsts], ModuleInfo) :-
	inst_list_is_ground(Args, ModuleInfo),
	bound_inst_list_is_ground(BoundInsts, ModuleInfo).

:- pred bound_inst_list_is_ground_2(list(bound_inst), module_info, set(inst)).
:- mode bound_inst_list_is_ground_2(in, in, in) is semidet.

bound_inst_list_is_ground_2([], _, _).
bound_inst_list_is_ground_2([functor(_Name, Args)|BoundInsts], ModuleInfo,
		Expansions) :-
	inst_list_is_ground_2(Args, ModuleInfo, Expansions),
	bound_inst_list_is_ground_2(BoundInsts, ModuleInfo, Expansions).

inst_list_is_ground([], _).
inst_list_is_ground([Inst | Insts], ModuleInfo) :-
	inst_is_ground(ModuleInfo, Inst),
	inst_list_is_ground(Insts, ModuleInfo).

:- pred inst_list_is_ground_2(list(inst), module_info, set(inst)).
:- mode inst_list_is_ground_2(in, in, in) is semidet.

inst_list_is_ground_2([], _, _).
inst_list_is_ground_2([Inst | Insts], ModuleInfo, Expansions) :-
	inst_is_ground_2(ModuleInfo, Inst, Inst, Expansions),
	inst_list_is_ground_2(Insts, ModuleInfo, Expansions).

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

inst_lookup_2(unify_inst(Live, A, B), ModuleInfo, Inst) :-
	module_info_insts(ModuleInfo, InstTable),
	inst_table_get_unify_insts(InstTable, UnifyInstTable),
	map__lookup(UnifyInstTable, unify_inst_pair(Live, A, B), MaybeInst),
	( MaybeInst = known(Inst0) ->
		Inst = Inst0
	;
		Inst = defined_inst(unify_inst(Live, A, B))
	).
inst_lookup_2(merge_inst(A, B), ModuleInfo, Inst) :-
	module_info_insts(ModuleInfo, InstTable),
	inst_table_get_merge_insts(InstTable, MergeInstTable),
	map__lookup(MergeInstTable, A - B, MaybeInst),
	( MaybeInst = known(Inst0) ->
		Inst = Inst0
	;
		Inst = defined_inst(merge_inst(A, B))
	).
inst_lookup_2(ground_inst(A), ModuleInfo, Inst) :-
	module_info_insts(ModuleInfo, InstTable),
	inst_table_get_ground_insts(InstTable, GroundInstTable),
	map__lookup(GroundInstTable, A, MaybeInst),
	( MaybeInst = known(Inst0) ->
		Inst = Inst0
	;
		Inst = defined_inst(ground_inst(A))
	).
inst_lookup_2(user_inst(Name, Args), ModuleInfo, Inst) :-
	module_info_insts(ModuleInfo, InstTable),
	inst_table_get_user_insts(InstTable, UserInstTable),
	list__length(Args, Arity),
	( map__search(UserInstTable, Name - Arity, InstDefn) ->
		InstDefn = hlds__inst_defn(_VarSet, Params, Inst0, _Cond, _C),
		inst_lookup_subst_args(Inst0, Params, Name, Args, Inst)
	;
		Inst = abstract_inst(Name, Args)
	).
inst_lookup_2(typed_ground(Type), ModuleInfo, Inst) :-
	propagate_type_info_inst(Type, ModuleInfo, ground, Inst).
inst_lookup_2(typed_inst(Type, InstName), ModuleInfo, Inst) :-
	inst_lookup_2(InstName, ModuleInfo, Inst0),
	propagate_type_info_inst(Type, ModuleInfo, Inst0, Inst).

%-----------------------------------------------------------------------------%

	% Given corresponding lists of types and modes, produce a new
	% list of modes which includes the information provided by the
	% corresponding types.

propagate_type_info_mode_list([], _, [], []).
propagate_type_info_mode_list([Type | Types], ModuleInfo, [Mode0 | Modes0],
		[Mode | Modes]) :-
	propagate_type_info_mode(Type, ModuleInfo, Mode0, Mode),
	propagate_type_info_mode_list(Types, ModuleInfo, Modes0, Modes).

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
	( Type = term__variable(_) ->
		% A type variable doesn't provide any extra information
		Inst = Inst0
	;
		module_info_types(ModuleInfo, TypeTable),
		type_to_type_id(Type, TypeId, TypeArgs),
		(
			map__search(TypeTable, TypeId, TypeDefn),
			TypeDefn = hlds__type_defn(_, TypeParams, TypeBody,
							_, _),
			TypeBody = du_type(Constructors0, _, _)
		->
			substitute_type_args(TypeParams, TypeArgs,
				Constructors0, Constructors),
			propagate_ctor_info(Inst0, Type, Constructors,
				ModuleInfo, Inst)
		;
			Inst = Inst0
		)
	).

	% Given a type and an inst, produce a new inst which includes
	% the information provided by the type.

:- pred ex_propagate_type_info_inst(type, module_info, inst, inst).
:- mode ex_propagate_type_info_inst(in, in, in, out) is det.

ex_propagate_type_info_inst(Type, ModuleInfo, Inst0, Inst) :-
	( Type = term__variable(_) ->
		% A type variable doesn't provide any extra information
		Inst = Inst0
	;
		module_info_types(ModuleInfo, TypeTable),
		type_to_type_id(Type, TypeId, TypeArgs),
		(
			map__search(TypeTable, TypeId, TypeDefn),
			TypeDefn = hlds__type_defn(_, TypeParams, TypeBody,
							_, _),
			TypeBody = du_type(Constructors0, _, _)
		->
			substitute_type_args(TypeParams, TypeArgs,
				Constructors0, Constructors),
			ex_propagate_ctor_info(Inst0, Type, Constructors,
				ModuleInfo, Inst)
		;
			Inst = Inst0
		)
	).

%-----------------------------------------------------------------------------%

	% Substitute the actual values of the type parameters
	% in list of constructors, for a particular instance of
	% a polymorphic type.

:- pred substitute_type_args(list(type_param), list(type),
				list(constructor), list(constructor)).
:- mode substitute_type_args(in, in, in, out) is det.

substitute_type_args(TypeParams0, TypeArgs, Constructors0, Constructors) :-
	( TypeParams0 = [] ->
		Constructors = Constructors0
	;
		term__term_list_to_var_list(TypeParams0, TypeParams),
		substitute_type_args_2(Constructors0, TypeParams, TypeArgs,
			Constructors)
	).

:- pred substitute_type_args_2(list(constructor), list(var), list(type),
				list(constructor)).
:- mode substitute_type_args_2(in, in, in, out) is det.

substitute_type_args_2([], _TypeParams, _TypeArgs, []).
substitute_type_args_2([Name - Args0 | Ctors0], TypeParams, TypeArgs,
		[Name - Args | Ctors]) :-
	term__substitute_corresponding_list(TypeParams, TypeArgs, Args0, Args),
	substitute_type_args_2(Ctors0, TypeParams, TypeArgs, Ctors).

%-----------------------------------------------------------------------------%

:- pred propagate_ctor_info(inst, type, list(constructor), module_info, inst).
:- mode propagate_ctor_info(in, in, in, in, out) is det.

% propagate_ctor_info(free, Type, _, _, free(Type)).	% temporarily disabled
propagate_ctor_info(free, _Type, _, _, free).	% XXX temporary hack

propagate_ctor_info(free(_), _, _, _, _) :-
	error("propagate_ctor_info: type info already present").
propagate_ctor_info(bound(BoundInsts0), _Type, Constructors, ModuleInfo,
		Inst) :-
	propagate_ctor_info_2(BoundInsts0, Constructors, ModuleInfo,
		BoundInsts),
	( BoundInsts = [] ->
		Inst = not_reached
	;
		% XXX do we need to sort the BoundInsts?
		Inst = bound(BoundInsts)
	).
propagate_ctor_info(ground, _Type, Constructors, ModuleInfo, Inst) :-
	constructors_to_bound_insts(Constructors, ModuleInfo, BoundInsts0),
	list__sort(BoundInsts0, BoundInsts),
	Inst = bound(BoundInsts).
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
ex_propagate_ctor_info(bound(BoundInsts0), _Type, Constructors, ModuleInfo,
		Inst) :-
	propagate_ctor_info_2(BoundInsts0, Constructors, ModuleInfo,
		BoundInsts),
	( BoundInsts = [] ->
		Inst = not_reached
	;
		% XXX do we need to sort the BoundInsts?
		Inst = bound(BoundInsts)
	).
ex_propagate_ctor_info(ground, Type, _, _, Inst) :-
	Inst = defined_inst(typed_ground(Type)).
ex_propagate_ctor_info(not_reached, _Type, _Constructors, _ModuleInfo,
		not_reached).
ex_propagate_ctor_info(inst_var(_), _, _, _, _) :-
	error("propagate_ctor_info: unbound inst var").
ex_propagate_ctor_info(abstract_inst(Name, Args), _, _, _,
		abstract_inst(Name, Args)).	% XXX loses info
ex_propagate_ctor_info(defined_inst(InstName), Type, _, _,
		defined_inst(typed_inst(Type, InstName))).

:- pred constructors_to_bound_insts(list(constructor), module_info,
				list(bound_inst)).
:- mode constructors_to_bound_insts(in, in, out) is det.

constructors_to_bound_insts([], _, []).
constructors_to_bound_insts([Ctor | Ctors], ModuleInfo,
		[BoundInst | BoundInsts]) :-
	Ctor = Name0 - Args,
	type_list_to_inst_list(Args, Insts),
	unqualify_name(Name0, Name),
	BoundInst = functor(term__atom(Name), Insts),
	constructors_to_bound_insts(Ctors, ModuleInfo, BoundInsts).

:- pred type_list_to_inst_list(list(type), list(inst)).
:- mode type_list_to_inst_list(in, out) is det.

type_list_to_inst_list([], []).
type_list_to_inst_list([Type | Types], [Inst | Insts]) :-
	Inst = defined_inst(typed_ground(Type)),
	type_list_to_inst_list(Types, Insts).

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

mode_get_insts(_ModuleInfo, InitialInst -> FinalInst, InitialInst, FinalInst).
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

mode_apply_substitution(I0 -> F0, Subst, I -> F) :-
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
inst_apply_substitution(ground, _, ground).
inst_apply_substitution(bound(Alts0), Subst, bound(Alts)) :-
	alt_list_apply_substitution(Alts0, Subst, Alts).
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
inst_name_apply_substitution(unify_inst(Live, InstA0, InstB0), Subst,
		unify_inst(Live, InstA, InstB)) :-
	inst_apply_substitution(InstA0, Subst, InstA),
	inst_apply_substitution(InstB0, Subst, InstB).
inst_name_apply_substitution(merge_inst(InstA0, InstB0), Subst,
		merge_inst(InstA, InstB)) :-
	inst_apply_substitution(InstA0, Subst, InstA),
	inst_apply_substitution(InstB0, Subst, InstB).
inst_name_apply_substitution(ground_inst(Inst0), Subst, ground_inst(Inst)) :-
	inst_name_apply_substitution(Inst0, Subst, Inst).

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

apply_instmap_delta(unreachable, _, unreachable).
apply_instmap_delta(reachable(_), unreachable, unreachable).
apply_instmap_delta(reachable(InstMapping0), reachable(InstMappingDelta), 
			reachable(InstMapping)) :-
	map__overlay(InstMapping0, InstMappingDelta, InstMapping).

	% Given two maps, overlay the entries in the second map 
	% on top of those in the first map to produce a new map.

%-----------------------------------------------------------------------------%
