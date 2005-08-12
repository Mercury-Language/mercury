%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% mode_util.m - utility predicates dealing with modes and insts.

% Main author: fjh.

%-----------------------------------------------------------------------------%

:- module check_hlds__mode_util.

:- interface.

:- import_module hlds__hlds_goal.
:- import_module hlds__hlds_module.
:- import_module hlds__hlds_pred.
:- import_module hlds__instmap.
:- import_module parse_tree__prog_data.

:- import_module bool.
:- import_module list.

	% mode_get_insts returns the initial instantiatedness and
	% the final instantiatedness for a given mode, aborting
	% if the mode is undefined.
	%
:- pred mode_get_insts(module_info::in, (mode)::in, (inst)::out, (inst)::out)
	is det.

	% a version of mode_get_insts which fails if the mode is undefined
:- pred mode_get_insts_semidet(module_info::in, (mode)::in,
	(inst)::out, (inst)::out) is semidet.

	% a mode is considered input if the initial inst is bound
:- pred mode_is_input(module_info::in, (mode)::in) is semidet.

	% a mode is considered fully input if the initial inst is ground
:- pred mode_is_fully_input(module_info::in, (mode)::in) is semidet.

	% a mode is considered output if the initial inst is free
	% and the final inst is bound
:- pred mode_is_output(module_info::in, (mode)::in) is semidet.

	% a mode is considered fully output if the initial inst is free and
	% the final inst is ground
:- pred mode_is_fully_output(module_info::in, (mode)::in) is semidet.

	% a mode is considered unused if both initial and final insts are free
:- pred mode_is_unused(module_info::in, (mode)::in) is semidet.

	% Succeeds iff the given mode is undefined.
	%
:- pred mode_is_undefined(module_info::in, (mode)::in) is semidet.

	% mode_to_arg_mode converts a mode (and corresponding type) to
	% an arg_mode.  A mode is a high-level notion, the normal
	% Mercury language mode.  An `arg_mode' is a low-level notion
	% used for code generation, which indicates the argument
	% passing convention (top_in, top_out, or top_unused) that
	% corresponds to that mode.  We need to know the type, not just
	% the mode, because the argument passing convention can depend
	% on the type's representation.
	%
:- pred mode_to_arg_mode(module_info::in, (mode)::in, (type)::in,
	arg_mode::out) is det.

:- pred modes_to_arg_modes(module_info::in, list(mode)::in, list(type)::in,
	list(arg_mode)::out) is det.

:- pred mode_list_get_initial_insts(list(mode)::in, module_info::in,
	list(inst)::out) is det.

:- pred mode_list_get_final_insts(list(mode)::in, module_info::in,
	list(inst)::out) is det.

:- pred mode_util__modes_to_uni_modes(list(mode)::in, list(mode)::in,
	module_info::in, list(uni_mode)::out) is det.

	% Given a user-defined or compiler-defined inst name,
	% lookup the corresponding inst in the inst table.
	%
:- pred inst_lookup(module_info::in, inst_name::in, (inst)::out) is det.

	% Use the instmap deltas for all the atomic sub-goals to recompute
	% the instmap deltas for all the non-atomic sub-goals of a goal.
	% Used to ensure that the instmap deltas remain valid after
	% code has been re-arranged, e.g. by followcode.
	% This also takes the module_info as input and output since it
	% may need to insert new merge_insts into the merge_inst table.
	% If the first argument is yes, the instmap_deltas for calls
	% and deconstruction unifications are also recomputed.
:- pred recompute_instmap_delta_proc(bool::in, proc_info::in, proc_info::out,
	module_info::in, module_info::out) is det.

:- pred recompute_instmap_delta(bool::in, hlds_goal::in, hlds_goal::out,
	vartypes::in, inst_varset::in, instmap::in, module_info::in,
	module_info::out) is det.

	% Given corresponding lists of types and modes, produce a new
	% list of modes which includes the information provided by the
	% corresponding types.
	%
:- pred propagate_types_into_mode_list(list(type)::in, module_info::in,
	list(mode)::in, list(mode)::out) is det.

	% Given corresponding lists of types and insts and a substitution
	% for the type variables in the type, produce a new list of insts
	% which includes the information provided by the corresponding types.
	%
:- pred propagate_types_into_inst_list(list(type)::in, tsubst::in,
	module_info::in, list(inst)::in, list(inst)::out) is det.

	% Convert a list of constructors to a list of bound_insts where the
	% arguments are `ground'.
	% Note that the list(bound_inst) is not sorted and may contain
	% duplicates.
:- pred constructors_to_bound_insts(list(constructor)::in, uniqueness::in,
	module_info::in, list(bound_inst)::out) is det.

	% Convert a list of constructors to a list of bound_insts where the
	% arguments are `any'.
	% Note that the list(bound_inst) is not sorted and may contain
	% duplicates.
:- pred constructors_to_bound_any_insts(list(constructor)::in, uniqueness::in,
	module_info::in, list(bound_inst)::out) is det.

	% Given the mode of a predicate,
	% work out which arguments are live (might be used again
	% by the caller of that predicate) and which are dead.
:- pred get_arg_lives(list(mode)::in, module_info::in, list(is_live)::out)
	is det.

	% Given the switched on variable and the instmaps before the switch
	% and after a branch make sure that any information added by the
	% functor test gets added to the instmap for the case.
:- pred fixup_switch_var(prog_var::in, instmap::in, instmap::in,
	hlds_goal::in, hlds_goal::out) is det.

%-----------------------------------------------------------------------------%

:- pred normalise_insts(list(inst)::in, list(type)::in, module_info::in,
	list(inst)::out) is det.

:- pred normalise_inst((inst)::in, (type)::in, module_info::in, (inst)::out)
	is det.

%-----------------------------------------------------------------------------%

	% Partition a list of arguments into inputs and others.
:- pred partition_args(module_info::in, list(mode)::in, list(T)::in,
	list(T)::out, list(T)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds__inst_match.
:- import_module check_hlds__inst_util.
:- import_module check_hlds__mode_info.
:- import_module check_hlds__type_util.
:- import_module hlds__hlds_data.
:- import_module mdbcomp__prim_data.
:- import_module parse_tree__prog_io.
:- import_module parse_tree__prog_mode.
:- import_module parse_tree__prog_util.
:- import_module parse_tree__prog_type.

:- import_module assoc_list.
:- import_module int.
:- import_module map.
:- import_module require.
:- import_module set.
:- import_module std_util.
:- import_module string.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

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

mode_is_undefined(ModuleInfo, Mode) :-
	not mode_get_insts_semidet(ModuleInfo, Mode, _, _).	

%-----------------------------------------------------------------------------%

modes_to_arg_modes(ModuleInfo, Modes, Types, ArgModes) :-
	( Modes = [], Types = [] ->
		ArgModes = []
	; Modes = [Mode | Modes1], Types = [Type | Types1] ->
		mode_to_arg_mode(ModuleInfo, Mode, Type, ArgMode),
		modes_to_arg_modes(ModuleInfo, Modes1, Types1, ArgModes1),
		ArgModes = [ArgMode | ArgModes1]
	;
		error("modes_to_arg_modes: length mismatch")
	).

mode_to_arg_mode(ModuleInfo, Mode, Type, ArgMode) :-
	mode_to_arg_mode_2(ModuleInfo, Mode, Type, [], ArgMode).

:- pred mode_to_arg_mode_2(module_info::in, (mode)::in, (type)::in,
	list(type_ctor)::in, arg_mode::out) is det.

mode_to_arg_mode_2(ModuleInfo, Mode, Type, ContainingTypes, ArgMode) :-
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
		type_is_no_tag_type(ModuleInfo, Type, FunctorName, ArgType),
		% avoid infinite recursion
		type_to_ctor_and_args(Type, TypeCtor, _TypeArgs),
		\+ list__member(TypeCtor, ContainingTypes)
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
		mode_to_arg_mode_2(ModuleInfo, ModeOfArg, ArgType,
			[TypeCtor | ContainingTypes], ArgMode)
	;
		base_mode_to_arg_mode(ModuleInfo, Mode, ArgMode)
	).

:- pred base_mode_to_arg_mode(module_info::in, (mode)::in, arg_mode::out)
	is det.

base_mode_to_arg_mode(ModuleInfo, Mode, ArgMode) :-
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
:- pred get_single_arg_inst((inst)::in, module_info::in, cons_id::in,
	(inst)::out) is det.

get_single_arg_inst(defined_inst(InstName), ModuleInfo, ConsId, ArgInst) :-
	inst_lookup(ModuleInfo, InstName, Inst),
	get_single_arg_inst(Inst, ModuleInfo, ConsId, ArgInst).
get_single_arg_inst(not_reached, _, _, not_reached).
get_single_arg_inst(ground(Uniq, _PredInst), _, _, ground(Uniq, none)).
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
get_single_arg_inst(constrained_inst_vars(_, Inst), ModuleInfo, ConsId,
		ArgInst) :-
	get_single_arg_inst(Inst, ModuleInfo, ConsId, ArgInst).

:- pred get_single_arg_inst_2(list(bound_inst)::in, cons_id::in, (inst)::out)
	is semidet.

get_single_arg_inst_2([BoundInst | BoundInsts], ConsId, ArgInst) :-
	( BoundInst = functor(ConsId, [ArgInst0]) ->
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

inst_lookup(ModuleInfo, InstName, Inst) :-
	(
		InstName = unify_inst(_, _, _, _),
		module_info_insts(ModuleInfo, InstTable),
		inst_table_get_unify_insts(InstTable, UnifyInstTable),
		map__lookup(UnifyInstTable, InstName, MaybeInst),
		( MaybeInst = known(Inst0, _) ->
			Inst = Inst0
		;
			Inst = defined_inst(InstName)
		)
	;
		InstName = merge_inst(A, B),
		module_info_insts(ModuleInfo, InstTable),
		inst_table_get_merge_insts(InstTable, MergeInstTable),
		map__lookup(MergeInstTable, A - B, MaybeInst),
		( MaybeInst = known(Inst0) ->
			Inst = Inst0
		;
			Inst = defined_inst(InstName)
		)
	;
		InstName = ground_inst(_, _, _, _),
		module_info_insts(ModuleInfo, InstTable),
		inst_table_get_ground_insts(InstTable, GroundInstTable),
		map__lookup(GroundInstTable, InstName, MaybeInst),
		( MaybeInst = known(Inst0, _) ->
			Inst = Inst0
		;
			Inst = defined_inst(InstName)
		)
	;
		InstName = any_inst(_, _, _, _),
		module_info_insts(ModuleInfo, InstTable),
		inst_table_get_any_insts(InstTable, AnyInstTable),
		map__lookup(AnyInstTable, InstName, MaybeInst),
		( MaybeInst = known(Inst0, _) ->
			Inst = Inst0
		;
			Inst = defined_inst(InstName)
		)
	;
		InstName = shared_inst(SharedInstName),
		module_info_insts(ModuleInfo, InstTable),
		inst_table_get_shared_insts(InstTable, SharedInstTable),
		map__lookup(SharedInstTable, SharedInstName, MaybeInst),
		( MaybeInst = known(Inst0) ->
			Inst = Inst0
		;
			Inst = defined_inst(InstName)
		)
	;
		InstName = mostly_uniq_inst(NondetLiveInstName),
		module_info_insts(ModuleInfo, InstTable),
		inst_table_get_mostly_uniq_insts(InstTable,
			NondetLiveInstTable),
		map__lookup(NondetLiveInstTable, NondetLiveInstName, MaybeInst),
		( MaybeInst = known(Inst0) ->
			Inst = Inst0
		;
			Inst = defined_inst(InstName)
		)
	;
		InstName = user_inst(Name, Args),
		module_info_insts(ModuleInfo, InstTable),
		inst_table_get_user_insts(InstTable, UserInstTable),
		user_inst_table_get_inst_defns(UserInstTable, InstDefns),
		list__length(Args, Arity),
		( map__search(InstDefns, Name - Arity, InstDefn) ->
			InstDefn = hlds_inst_defn(_VarSet, Params, Inst0,
				_C, _),
			inst_lookup_subst_args(Inst0, Params, Name, Args, Inst)
		;
			Inst = abstract_inst(Name, Args)
		)
	;
		InstName = typed_ground(Uniq, Type),
		map__init(Subst),
		propagate_type_into_inst(Type, Subst, ModuleInfo,
			ground(Uniq, none), Inst)
	;
		InstName = typed_inst(Type, TypedInstName),
		inst_lookup(ModuleInfo, TypedInstName, Inst0),
		map__init(Subst),
		propagate_type_into_inst(Type, Subst, ModuleInfo, Inst0, Inst)
	).

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

:- pred propagate_type_into_mode((type)::in, module_info::in,
	(mode)::in, (mode)::out) is det.

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

:- pred propagate_type_into_inst((type)::in, tsubst::in, module_info::in,
	(inst)::in, (inst)::out) is det.

:- pred propagate_type_into_inst_lazily((type)::in, tsubst::in,
	module_info::in, (inst)::in, (inst)::out) is det.

%	% XXX We ought to expand things eagerly here, using the commented
%	% out code below.  However, that causes efficiency problems,
%	% so for the moment it is disabled.
% propagate_type_into_inst(Type, Subst, ModuleInfo, Inst0, Inst) :-
%	apply_type_subst(Type0, Subst, Type),
%	(
%	        type_constructors(Type, ModuleInfo, Constructors)
%	->
%	        propagate_ctor_info(Inst0, Type, Constructors, ModuleInfo,
%	               Inst)
%	;
%	        Inst = Inst0
%	).

propagate_type_into_inst(Type, Subst, ModuleInfo, Inst0, Inst) :-
	propagate_ctor_info_lazily(Inst0, Type, Subst, ModuleInfo, Inst).

propagate_type_into_inst_lazily(Type, Subst, ModuleInfo, Inst0, Inst) :-
	propagate_ctor_info_lazily(Inst0, Type, Subst, ModuleInfo, Inst).

%-----------------------------------------------------------------------------%

:- pred propagate_ctor_info((inst)::in, (type)::in, list(constructor)::in,
	module_info::in, (inst)::out) is det.

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
propagate_ctor_info(ground(Uniq, none), Type, Constructors, ModuleInfo, Inst)
		:-
	( type_is_higher_order(Type, _Purity, function, _, ArgTypes) ->
		default_higher_order_func_inst(ArgTypes, ModuleInfo,
			HigherOrderInstInfo),
		Inst = ground(Uniq, higher_order(HigherOrderInstInfo))
	;
		constructors_to_bound_insts(Constructors, Uniq, ModuleInfo,
			BoundInsts0),
		list__sort_and_remove_dups(BoundInsts0, BoundInsts),
		Inst = bound(Uniq, BoundInsts)
	).
propagate_ctor_info(ground(Uniq, higher_order(PredInstInfo0)), Type, _Ctors,
		ModuleInfo, ground(Uniq, higher_order(PredInstInfo))) :-
	PredInstInfo0 = pred_inst_info(PredOrFunc, Modes0, Det),
	PredInstInfo = pred_inst_info(PredOrFunc, Modes, Det),
	(
		type_is_higher_order(Type, _Purity, PredOrFunc, _, ArgTypes),
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
propagate_ctor_info(inst_var(V), _, _, _, inst_var(V)).
propagate_ctor_info(constrained_inst_vars(V, Inst0), Type, Constructors,
		ModuleInfo, constrained_inst_vars(V, Inst)) :-
	propagate_ctor_info(Inst0, Type, Constructors, ModuleInfo, Inst).
propagate_ctor_info(abstract_inst(Name, Args), _, _, _,
		abstract_inst(Name, Args)).	% XXX loses info
propagate_ctor_info(defined_inst(InstName), Type, Ctors, ModuleInfo, Inst) :-
	inst_lookup(ModuleInfo, InstName, Inst0),
	propagate_ctor_info(Inst0, Type, Ctors, ModuleInfo, Inst).

:- pred propagate_ctor_info_lazily((inst)::in, (type)::in, tsubst::in,
	module_info::in, (inst)::out) is det.

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
propagate_ctor_info_lazily(ground(Uniq, none), Type0, Subst, ModuleInfo, Inst)
		:-
	apply_type_subst(Type0, Subst, Type),
	( type_is_higher_order(Type, _Purity, function, _, ArgTypes) ->
		default_higher_order_func_inst(ArgTypes, ModuleInfo,
			HigherOrderInstInfo),
		Inst = ground(Uniq, higher_order(HigherOrderInstInfo))
	;
		% XXX The information added by this is not yet used,
		% so it's disabled since it unnecessarily complicates
		% the insts.
		%
		% Inst = defined_inst(typed_ground(Uniq, Type))
		Inst = ground(Uniq, none)
	).

propagate_ctor_info_lazily(ground(Uniq, higher_order(PredInstInfo0)), Type0,
		Subst, ModuleInfo, ground(Uniq, higher_order(PredInstInfo))) :-
	PredInstInfo0 = pred_inst_info(PredOrFunc, Modes0, Det),
	PredInstInfo = pred_inst_info(PredOrFunc, Modes, Det),
	apply_type_subst(Type0, Subst, Type),
	(
		type_is_higher_order(Type, _Purity, PredOrFunc, _, ArgTypes),
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
propagate_ctor_info_lazily(inst_var(Var), _, _, _, inst_var(Var)).
propagate_ctor_info_lazily(constrained_inst_vars(V, Inst0), Type, Constructors,
		ModuleInfo, constrained_inst_vars(V, Inst)) :-
	propagate_ctor_info_lazily(Inst0, Type, Constructors, ModuleInfo, Inst).
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
:- pred default_higher_order_func_inst(list(type)::in, module_info::in,
	pred_inst_info::out) is det.

default_higher_order_func_inst(PredArgTypes, ModuleInfo, PredInstInfo) :-
	In = (ground(shared, none) -> ground(shared, none)),
	Out = (free -> ground(shared, none)),
	list__length(PredArgTypes, NumPredArgs),
	NumFuncArgs = NumPredArgs - 1,
	list__duplicate(NumFuncArgs, In, FuncArgModes),
	FuncRetMode = Out,
	list__append(FuncArgModes, [FuncRetMode], PredArgModes0),
	propagate_types_into_mode_list(PredArgTypes, ModuleInfo,
		PredArgModes0, PredArgModes),
	PredInstInfo = pred_inst_info(function, PredArgModes, det).

constructors_to_bound_insts(Constructors, Uniq, ModuleInfo, BoundInsts) :-
	constructors_to_bound_insts_2(Constructors, Uniq, ModuleInfo,
		ground(Uniq, none), BoundInsts).

constructors_to_bound_any_insts(Constructors, Uniq, ModuleInfo, BoundInsts) :-
	constructors_to_bound_insts_2(Constructors, Uniq, ModuleInfo,
		any(Uniq), BoundInsts).

:- pred constructors_to_bound_insts_2(list(constructor)::in, uniqueness::in,
	module_info::in, (inst)::in, list(bound_inst)::out) is det.

constructors_to_bound_insts_2([], _, _, _, []).
constructors_to_bound_insts_2([Ctor | Ctors], Uniq, ModuleInfo, ArgInst,
		[BoundInst | BoundInsts]) :-
	Ctor = ctor(_ExistQVars, _Constraints, Name, Args),
	ctor_arg_list_to_inst_list(Args, ArgInst, Insts),
	list__length(Insts, Arity),
	BoundInst = functor(cons(Name, Arity), Insts),
	constructors_to_bound_insts_2(Ctors, Uniq, ModuleInfo, ArgInst,
		BoundInsts).

:- pred ctor_arg_list_to_inst_list(list(constructor_arg)::in, (inst)::in,
	list(inst)::out) is det.

ctor_arg_list_to_inst_list([], _, []).
ctor_arg_list_to_inst_list([_Name - _Type | Args], Inst, [Inst | Insts]) :-
	ctor_arg_list_to_inst_list(Args, Inst, Insts).

:- pred propagate_ctor_info_2(list(bound_inst)::in, (type)::in,
	module_info::in, list(bound_inst)::out) is det.

propagate_ctor_info_2(BoundInsts0, Type, ModuleInfo, BoundInsts) :-
	(
		type_is_tuple(Type, TupleArgTypes)
	->
		list__map(propagate_ctor_info_tuple(ModuleInfo, TupleArgTypes),
			BoundInsts0, BoundInsts)
	;
		type_to_ctor_and_args(Type, TypeCtor, TypeArgs),
		TypeCtor = qualified(TypeModule, _) - _,
		module_info_types(ModuleInfo, TypeTable),
		map__search(TypeTable, TypeCtor, TypeDefn),
		hlds_data__get_type_defn_tparams(TypeDefn, TypeParams0),
		hlds_data__get_type_defn_body(TypeDefn, TypeBody),
		Constructors = TypeBody ^ du_type_ctors
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

:- pred propagate_ctor_info_tuple(module_info::in, list(type)::in,
	bound_inst::in, bound_inst::out) is det.

propagate_ctor_info_tuple(ModuleInfo, TupleArgTypes, BoundInst0, BoundInst) :-
	BoundInst0 = functor(Functor, ArgInsts0),
	(
		Functor = cons(unqualified("{}"), _),
		list__length(ArgInsts0, ArgInstsLen),
		list__length(TupleArgTypes, TupleArgTypesLen),
		ArgInstsLen = TupleArgTypesLen
	->
		map__init(Subst),
		propagate_types_into_inst_list(TupleArgTypes,
			Subst, ModuleInfo, ArgInsts0, ArgInsts)
	;
		% The bound_inst's arity does not match the
		% tuple's arity, so leave it alone. This can
		% only happen in a user defined bound_inst.
		% A mode error should be reported if anything
		% tries to match with the inst.
		ArgInsts = ArgInsts0
	),
	BoundInst = functor(Functor, ArgInsts).

:- pred propagate_ctor_info_3(list(bound_inst)::in, module_name::in,
	list(constructor)::in, tsubst::in, module_info::in,
	list(bound_inst)::out) is det.

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
		GetCons = (pred(Ctor::in) is semidet :-
				Ctor = ctor(_, _, ConsName, CtorArgs),
				list__length(CtorArgs, Arity)
			),
		list__filter(GetCons, Constructors, [Constructor])
	->
		Constructor = ctor(_ExistQVars, _Constraints, _Name, Args),
		GetArgTypes = (pred(CtorArg::in, ArgType::out) is det :-
				CtorArg = _ArgName - ArgType
			),
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

:- pred apply_type_subst((type)::in, tsubst::in, (type)::out) is det.

apply_type_subst(Type0, Subst, Type) :-
	% optimize common case
	( map__is_empty(Subst) ->
		Type = Type0
	;
		term__apply_substitution(Type0, Subst, Type)
	).

%-----------------------------------------------------------------------------%

:- pred inst_lookup_subst_args(hlds_inst_body::in, list(inst_var)::in,
	sym_name::in, list(inst)::in, (inst)::out) is det.

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
	HLDS_Mode = hlds_mode_defn(_VarSet, Params, ModeDefn,
		_Context, _Status),
	ModeDefn = eqv_mode(Mode0),
	mode_substitute_arg_list(Mode0, Params, Args, Mode),
	mode_get_insts_semidet(ModuleInfo, Mode, Initial, Final).

mode_get_insts(ModuleInfo, Mode, Inst1, Inst2) :-
	( mode_get_insts_semidet(ModuleInfo, Mode, Inst1a, Inst2a) ->
		Inst1 = Inst1a,
		Inst2 = Inst2a
	;
		error("mode_get_insts_semidet failed")
	).

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

recompute_instmap_delta_proc(RecomputeAtomic, !ProcInfo, !ModuleInfo) :-
	proc_info_get_initial_instmap(!.ProcInfo, !.ModuleInfo, InstMap0),
	proc_info_vartypes(!.ProcInfo, VarTypes),
	proc_info_goal(!.ProcInfo, Goal0),
	proc_info_inst_varset(!.ProcInfo, InstVarSet),
	recompute_instmap_delta(RecomputeAtomic, Goal0, Goal,
		VarTypes, InstVarSet, InstMap0, !ModuleInfo),
	proc_info_set_goal(Goal, !ProcInfo).

recompute_instmap_delta(RecomputeAtomic, Goal0, Goal, VarTypes, InstVarSet,
		InstMap0, ModuleInfo0, ModuleInfo) :-
	RI0 = recompute_info(ModuleInfo0, InstVarSet),
	recompute_instmap_delta_1(RecomputeAtomic, Goal0, Goal, VarTypes,
		InstMap0, _, RI0, RI),
	ModuleInfo = RI^module_info.

:- pred recompute_instmap_delta_1(bool::in, hlds_goal::in, hlds_goal::out,
	vartypes::in, instmap::in, instmap_delta::out,
	recompute_info::in, recompute_info::out) is det.

recompute_instmap_delta_1(RecomputeAtomic, Goal0 - GoalInfo0, Goal - GoalInfo,
		VarTypes, InstMap0, InstMapDelta, !RI) :-
	(
		RecomputeAtomic = no,
		goal_is_atomic(Goal0),
		Goal0 \= unify(_,lambda_goal(_,_,_,_,_,_,_,_,_),_,_,_)
			% Lambda expressions always need to be processed.
	->
		Goal = Goal0,
		GoalInfo1 = GoalInfo0
	;
		recompute_instmap_delta_2(RecomputeAtomic, Goal0, GoalInfo0,
			Goal, VarTypes, InstMap0, InstMapDelta0, !RI),
		goal_info_get_nonlocals(GoalInfo0, NonLocals),
		instmap_delta_restrict(InstMapDelta0, NonLocals,
			InstMapDelta1),
		goal_info_set_instmap_delta(GoalInfo0,
			InstMapDelta1, GoalInfo1)
	),

	% If the initial instmap is unreachable so is the final instmap.
	( instmap__is_unreachable(InstMap0) ->
		instmap_delta_init_unreachable(UnreachableInstMapDelta),
		goal_info_set_instmap_delta(GoalInfo1,
			UnreachableInstMapDelta, GoalInfo)
	;
		GoalInfo = GoalInfo1
	),
	goal_info_get_instmap_delta(GoalInfo, InstMapDelta).

:- type recompute_info
	--->	recompute_info(
			module_info :: module_info,
			inst_varset :: inst_varset
		).

	% update_module_info(P, R, RI0, RI) will call predicate P, passing it
	% the module_info from RI0 and placing the output module_info in RI.
	% The output of P's first argument is returned in R.
:- pred update_module_info(
	pred(T, module_info, module_info)::in(pred(out, in, out) is det),
	T::out, recompute_info::in, recompute_info::out) is det.

update_module_info(P, R, !RI) :-
	ModuleInfo0 = !.RI ^ module_info,
	P(R, ModuleInfo0, ModuleInfo),
	!:RI = !.RI ^ module_info := ModuleInfo.

:- pred recompute_instmap_delta_2(bool::in, hlds_goal_expr::in,
	hlds_goal_info::in, hlds_goal_expr::out, vartypes::in, instmap::in,
	instmap_delta::out, recompute_info::in, recompute_info::out) is det.

recompute_instmap_delta_2(Atomic, switch(Var, Det, Cases0), GoalInfo,
		switch(Var, Det, Cases), VarTypes, InstMap, InstMapDelta,
		!RI) :-
	( goal_info_has_feature(GoalInfo, mode_check_clauses_goal) ->
		Cases = Cases0,
		goal_info_get_instmap_delta(GoalInfo, InstMapDelta)
	;
		goal_info_get_nonlocals(GoalInfo, NonLocals),
		recompute_instmap_delta_cases(Atomic, Var, Cases0, Cases,
			VarTypes, InstMap, NonLocals, InstMapDelta, !RI)
	).

recompute_instmap_delta_2(Atomic, conj(Goals0), _, conj(Goals),
		VarTypes, InstMap, InstMapDelta, !RI) :-
	recompute_instmap_delta_conj(Atomic, Goals0, Goals,
		VarTypes, InstMap, InstMapDelta, !RI).

recompute_instmap_delta_2(Atomic, par_conj(Goals0), GoalInfo,
		par_conj(Goals), VarTypes, InstMap, InstMapDelta, !RI) :-
	goal_info_get_nonlocals(GoalInfo, NonLocals),
	recompute_instmap_delta_par_conj(Atomic, Goals0, Goals,
		VarTypes, InstMap, NonLocals, InstMapDelta, !RI).

recompute_instmap_delta_2(Atomic, disj(Goals0), GoalInfo, disj(Goals),
		VarTypes, InstMap, InstMapDelta, !RI) :-
	( goal_info_has_feature(GoalInfo, mode_check_clauses_goal) ->
		Goals = Goals0,
		goal_info_get_instmap_delta(GoalInfo, InstMapDelta)
	;
		goal_info_get_nonlocals(GoalInfo, NonLocals),
		recompute_instmap_delta_disj(Atomic, Goals0, Goals,
			VarTypes, InstMap, NonLocals, InstMapDelta, !RI)
	).

recompute_instmap_delta_2(Atomic, not(Goal0), _, not(Goal),
		VarTypes, InstMap, InstMapDelta, !RI) :-
	instmap_delta_init_reachable(InstMapDelta),
	recompute_instmap_delta_1(Atomic, Goal0, Goal, VarTypes, InstMap, _,
		!RI).

recompute_instmap_delta_2(Atomic, if_then_else(Vars, Cond0, Then0, Else0),
		GoalInfo, if_then_else(Vars, Cond, Then, Else), VarTypes,
		InstMap0, InstMapDelta, !RI) :-
	recompute_instmap_delta_1(Atomic, Cond0, Cond, VarTypes, InstMap0,
		InstMapDeltaCond, !RI),
	instmap__apply_instmap_delta(InstMap0, InstMapDeltaCond, InstMapCond),
	recompute_instmap_delta_1(Atomic, Then0, Then, VarTypes, InstMapCond,
		InstMapDeltaThen, !RI),
	recompute_instmap_delta_1(Atomic, Else0, Else, VarTypes, InstMap0,
		InstMapDeltaElse, !RI),
	instmap_delta_apply_instmap_delta(InstMapDeltaCond, InstMapDeltaThen,
		test_size, InstMapDeltaCondThen),
	goal_info_get_nonlocals(GoalInfo, NonLocals),
	update_module_info(merge_instmap_delta(InstMap0, NonLocals,
		VarTypes, InstMapDeltaElse, InstMapDeltaCondThen),
		InstMapDelta, !RI).

recompute_instmap_delta_2(Atomic, scope(Reason, Goal0), _,
		scope(Reason, Goal), VarTypes, InstMap, InstMapDelta, !RI) :-
	recompute_instmap_delta_1(Atomic, Goal0, Goal, VarTypes, InstMap,
		InstMapDelta, !RI).

recompute_instmap_delta_2(_, generic_call(Details, Vars, Modes, Detism), _,
		generic_call(Details, Vars, Modes, Detism),
		_VarTypes, _InstMap, InstMapDelta, !RI) :-
	ModuleInfo = !.RI ^ module_info,
	instmap_delta_from_mode_list(Vars, Modes, ModuleInfo, InstMapDelta).

recompute_instmap_delta_2(_, call(PredId, ProcId, Args, D, E, F), _,
		call(PredId, ProcId, Args, D, E, F), VarTypes,
		InstMap, InstMapDelta, !RI) :-
	recompute_instmap_delta_call(PredId, ProcId,
		Args, VarTypes, InstMap, InstMapDelta, !RI).

recompute_instmap_delta_2(Atomic, unify(LHS, RHS0, UniMode0, Uni, Context),
		GoalInfo, unify(LHS, RHS, UniMode, Uni, Context), VarTypes,
		InstMap0, InstMapDelta, !RI) :-
	(
		RHS0 = lambda_goal(Purity, PorF, EvalMethod, FixModes,
			NonLocals, LambdaVars, Modes, Det, Goal0)
	->
		ModuleInfo0 = !.RI ^ module_info,
		instmap__pre_lambda_update(ModuleInfo0, LambdaVars, Modes,
			InstMap0, InstMap),
		recompute_instmap_delta_1(Atomic, Goal0, Goal, VarTypes,
			InstMap, _, !RI),
		RHS = lambda_goal(Purity, PorF, EvalMethod, FixModes,
			NonLocals, LambdaVars, Modes, Det, Goal)
	;
		RHS = RHS0
	),
	(
		Atomic = yes,
		recompute_instmap_delta_unify(Uni, UniMode0, UniMode,
			GoalInfo, InstMap0, InstMapDelta, !.RI)
	;
		Atomic = no,
		UniMode = UniMode0,
		goal_info_get_instmap_delta(GoalInfo, InstMapDelta)
	).

recompute_instmap_delta_2(_,
		foreign_proc(A, PredId, ProcId, Args, ExtraArgs, F), GoalInfo,
		foreign_proc(A, PredId, ProcId, Args, ExtraArgs, F),
		VarTypes, InstMap, InstMapDelta, !RI) :-
	ArgVars = list__map(foreign_arg_var, Args),
	recompute_instmap_delta_call(PredId, ProcId,
		ArgVars, VarTypes, InstMap, InstMapDelta0, !RI),
	(
		ExtraArgs = [],
		InstMapDelta = InstMapDelta0
	;
		ExtraArgs = [_ | _],
		goal_info_get_instmap_delta(GoalInfo, OldInstMapDelta),
		ExtraArgVars = list__map(foreign_arg_var, ExtraArgs),
		instmap_delta_restrict(OldInstMapDelta,
			set__list_to_set(ExtraArgVars), ExtraArgsInstMapDelta),
		instmap_delta_apply_instmap_delta(InstMapDelta0,
			ExtraArgsInstMapDelta, large_base, InstMapDelta)
	).

recompute_instmap_delta_2(_, shorthand(_), _, _, _, _, _, !RI) :-
	% these should have been expanded out by now
	error("recompute_instmap_delta_2: unexpected shorthand").

%-----------------------------------------------------------------------------%

:- pred recompute_instmap_delta_conj(bool::in, list(hlds_goal)::in,
	list(hlds_goal)::out, vartypes::in, instmap::in, instmap_delta::out,
	recompute_info::in, recompute_info::out) is det.

recompute_instmap_delta_conj(_, [], [], _, _, InstMapDelta, !RI) :-
	instmap_delta_init_reachable(InstMapDelta).
recompute_instmap_delta_conj(Atomic, [Goal0 | Goals0], [Goal | Goals],
		VarTypes, InstMap0, InstMapDelta, !RI) :-
	recompute_instmap_delta_1(Atomic, Goal0, Goal, VarTypes, InstMap0,
		InstMapDelta0, !RI),
	instmap__apply_instmap_delta(InstMap0, InstMapDelta0, InstMap1),
	recompute_instmap_delta_conj(Atomic, Goals0, Goals, VarTypes, InstMap1,
		InstMapDelta1, !RI),
	instmap_delta_apply_instmap_delta(InstMapDelta0, InstMapDelta1,
		large_overlay, InstMapDelta).

%-----------------------------------------------------------------------------%

:- pred recompute_instmap_delta_disj(bool::in, list(hlds_goal)::in,
	list(hlds_goal)::out, vartypes::in, instmap::in, set(prog_var)::in,
	instmap_delta::out, recompute_info::in, recompute_info::out) is det.

recompute_instmap_delta_disj(_, [], [], _, _, _, InstMapDelta, !RI) :-
	instmap_delta_init_unreachable(InstMapDelta).
recompute_instmap_delta_disj(Atomic, [Goal0], [Goal],
		VarTypes, InstMap, _, InstMapDelta, !RI) :-
	recompute_instmap_delta_1(Atomic, Goal0, Goal, VarTypes, InstMap,
		InstMapDelta, !RI).
recompute_instmap_delta_disj(Atomic, [Goal0 | Goals0], [Goal | Goals],
		VarTypes, InstMap, NonLocals, InstMapDelta, !RI) :-
	Goals0 = [_ | _],
	recompute_instmap_delta_1(Atomic, Goal0, Goal,
		VarTypes, InstMap, InstMapDelta0, !RI),
	recompute_instmap_delta_disj(Atomic, Goals0, Goals,
		VarTypes, InstMap, NonLocals, InstMapDelta1, !RI),
	update_module_info(merge_instmap_delta(InstMap, NonLocals,
		VarTypes, InstMapDelta0, InstMapDelta1), InstMapDelta, !RI).

:- pred recompute_instmap_delta_par_conj(bool::in, list(hlds_goal)::in,
	list(hlds_goal)::out, vartypes::in, instmap::in, set(prog_var)::in,
	instmap_delta::out, recompute_info::in, recompute_info::out) is det.

recompute_instmap_delta_par_conj(_, [], [], _, _, _, InstMapDelta, !RI) :-
	instmap_delta_init_unreachable(InstMapDelta).
recompute_instmap_delta_par_conj(Atomic, [Goal0], [Goal],
		VarTypes, InstMap, _, InstMapDelta, !RI) :-
	recompute_instmap_delta_1(Atomic, Goal0, Goal, VarTypes, InstMap,
		InstMapDelta, !RI).
recompute_instmap_delta_par_conj(Atomic, [Goal0 | Goals0], [Goal | Goals],
		VarTypes, InstMap, NonLocals, InstMapDelta, !RI) :-
	Goals0 = [_ | _],
	recompute_instmap_delta_1(Atomic, Goal0, Goal,
		VarTypes, InstMap, InstMapDelta0, !RI),
	recompute_instmap_delta_par_conj(Atomic, Goals0, Goals,
		VarTypes, InstMap, NonLocals, InstMapDelta1, !RI),
	update_module_info(unify_instmap_delta(InstMap, NonLocals,
		InstMapDelta0, InstMapDelta1), InstMapDelta, !RI).

%-----------------------------------------------------------------------------%

:- pred recompute_instmap_delta_cases(bool::in, prog_var::in, list(case)::in,
	list(case)::out, vartypes::in, instmap::in, set(prog_var)::in,
	instmap_delta::out, recompute_info::in, recompute_info::out) is det.

recompute_instmap_delta_cases(_, _, [], [], _, _, _, InstMapDelta, !RI) :-
	instmap_delta_init_unreachable(InstMapDelta).
recompute_instmap_delta_cases(Atomic, Var, [Case0 | Cases0], [Case | Cases],
		VarTypes, InstMap0, NonLocals, InstMapDelta, !RI) :-
	Case0 = case(Functor, Goal0),
	map__lookup(VarTypes, Var, Type),
	update_module_info(instmap__bind_var_to_functor(Var, Type, Functor,
		InstMap0), InstMap, !RI),
	recompute_instmap_delta_1(Atomic, Goal0, Goal, VarTypes, InstMap,
		InstMapDelta0, !RI),
	update_module_info(instmap_delta_bind_var_to_functor(Var, Type,
		Functor, InstMap0, InstMapDelta0), InstMapDelta1, !RI),
	Case = case(Functor, Goal),
	recompute_instmap_delta_cases(Atomic, Var, Cases0, Cases,
		VarTypes, InstMap0, NonLocals, InstMapDelta2, !RI),
	update_module_info(merge_instmap_delta(InstMap0, NonLocals,
		VarTypes, InstMapDelta1, InstMapDelta2), InstMapDelta, !RI).

%-----------------------------------------------------------------------------%

:- pred recompute_instmap_delta_call(pred_id::in, proc_id::in,
	list(prog_var)::in, vartypes::in, instmap::in, instmap_delta::out,
	recompute_info::in, recompute_info::out) is det.

recompute_instmap_delta_call(PredId, ProcId, Args, VarTypes, InstMap,
		InstMapDelta, !RI) :-
	ModuleInfo = !.RI ^ module_info,
	module_info_pred_proc_info(ModuleInfo, PredId, ProcId, _, ProcInfo),
	proc_info_interface_determinism(ProcInfo, Detism),
	( determinism_components(Detism, _, at_most_zero) ->
		instmap_delta_init_unreachable(InstMapDelta)
	;
		proc_info_argmodes(ProcInfo, ArgModes0),
		proc_info_inst_varset(ProcInfo, ProcInstVarSet),
		InstVarSet = !.RI ^ inst_varset,
		rename_apart_inst_vars(InstVarSet, ProcInstVarSet,
			ArgModes0, ArgModes1),
		mode_list_get_initial_insts(ArgModes1, ModuleInfo,
			InitialInsts),

		% Compute the inst_var substitution from the initial insts
		% of the called procedure and the insts of the argument
		% variables.
		map__init(InstVarSub0),
		update_module_info(compute_inst_var_sub(Args, VarTypes, InstMap,
			InitialInsts, InstVarSub0), InstVarSub, !RI),

		% Apply the inst_var substitution to the argument modes.
		mode_list_apply_substitution(ArgModes1, InstVarSub,
			ArgModes2),

		% Calculate the final insts of the argument variables
		% from their initial insts and the final insts of the called
		% procedure (with inst_var substitutions applied).
		update_module_info(recompute_instmap_delta_call_2(Args, InstMap,
			ArgModes2), ArgModes, !RI),
		instmap_delta_from_mode_list(Args, ArgModes,
			ModuleInfo, InstMapDelta)
	).

:- pred compute_inst_var_sub(list(prog_var)::in, vartypes::in, instmap::in,
	list(inst)::in, inst_var_sub::in, inst_var_sub::out,
	module_info::in, module_info::out) is det.

compute_inst_var_sub([], _, _, [], !Sub, !ModuleInfo).
compute_inst_var_sub([_|_], _, _, [], !Sub, !ModuleInfo) :-
	error("compute_inst_var_sub").
compute_inst_var_sub([], _, _, [_|_], !Sub, !ModuleInfo) :-
	error("compute_inst_var_sub").
compute_inst_var_sub([Arg | Args], VarTypes, InstMap, [Inst | Insts],
		!Sub, !ModuleInfo) :-
	% This is similar to modecheck_var_has_inst.
	SaveModuleInfo = !.ModuleInfo,
	SaveSub = !.Sub,
	( instmap__is_reachable(InstMap) ->
		instmap__lookup_var(InstMap, Arg, ArgInst),
		map__lookup(VarTypes, Arg, Type),
		(
			inst_matches_initial(ArgInst, Inst, Type, !ModuleInfo,
				!Sub)
		->
			true
		;
			% error("compute_inst_var_sub: " ++
			%	++ "inst_matches_initial failed")
			% XXX  We shouldn't ever get here, but unfortunately
			% the mode system currently has several problems (most
			% noticeably lack of alias tracking for unique modes)
			% which mean inst_matches_initial can sometimes fail
			% here.
			!:ModuleInfo = SaveModuleInfo,
			!:Sub = SaveSub
		)
	;
		true
	),
	compute_inst_var_sub(Args, VarTypes, InstMap, Insts, !Sub,
		!ModuleInfo).

:- pred recompute_instmap_delta_call_2(list(prog_var)::in, instmap::in,
	list(mode)::in, list(mode)::out, module_info::in, module_info::out)
	is det.

recompute_instmap_delta_call_2([], _, [], [], !ModuleInfo).
recompute_instmap_delta_call_2([_ | _], _, [], _, !ModuleInfo) :-
	error("recompute_instmap_delta_call_2").
recompute_instmap_delta_call_2([], _, [_ | _], _, !ModuleInfo) :-
	error("recompute_instmap_delta_call_2").
recompute_instmap_delta_call_2([Arg | Args], InstMap, [Mode0 | Modes0],
		[Mode | Modes], !ModuleInfo) :-
	% This is similar to modecheck_set_var_inst.
	( instmap__is_reachable(InstMap) ->
		instmap__lookup_var(InstMap, Arg, ArgInst0),
		mode_get_insts(!.ModuleInfo, Mode0, _, FinalInst),
		(
			abstractly_unify_inst(dead, ArgInst0, FinalInst,
				fake_unify, UnifyInst, _, !ModuleInfo)
		->
			Mode = (ArgInst0 -> UnifyInst)
		;
			error("recompute_instmap_delta_call_2: " ++
				"unify_inst failed")
		)
	;
		Mode = (not_reached -> not_reached)
	),
	recompute_instmap_delta_call_2(Args, InstMap, Modes0, Modes,
		!ModuleInfo).

:- pred recompute_instmap_delta_unify(unification::in, unify_mode::in,
	unify_mode::out, hlds_goal_info::in, instmap::in, instmap_delta::out,
	recompute_info::in) is det.

recompute_instmap_delta_unify(Uni, UniMode0, UniMode, GoalInfo,
		InstMap, InstMapDelta, RI) :-
	% Deconstructions are the only types of unifications
	% that can require updating of the instmap_delta after simplify.m
	% has been run.
	ModuleInfo = RI ^ module_info,
	(
		Uni = deconstruct(Var, _ConsId, Vars, UniModes, _, _CanCGC)
	->
		% Get the final inst of the deconstructed var, which
		% will be the same as in the old instmap.
		goal_info_get_instmap_delta(GoalInfo, OldInstMapDelta),
		instmap__lookup_var(InstMap, Var, InitialInst),
		( instmap_delta_search_var(OldInstMapDelta, Var, FinalInst1) ->
			% XXX we need to merge the information in InitialInst
			% and FinalInst1. In puzzle_detism_bug, InitialInst
			% has a var bound to one function symbol (james), while
			% FinalInst1 has it bound to another (katherine).
			% The correct final inst is thus `unreachable', but
			% we don't return that.
			FinalInst = FinalInst1
		;
			% it wasn't in the instmap_delta, so the inst didn't
			% change.
			FinalInst = InitialInst
		),
		UniModeToRhsMode = (pred(UMode::in, Mode::out) is det :-
				UMode = ((_ - Inst0) -> (_ - Inst)),
				Mode = (Inst0 -> Inst)
			),
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
%-----------------------------------------------------------------------------%

normalise_insts([], [], _, []).
normalise_insts([Inst0|Insts0], [Type|Types], ModuleInfo, [Inst|Insts]) :-
	normalise_inst(Inst0, Type, ModuleInfo, Inst),
	normalise_insts(Insts0, Types, ModuleInfo, Insts).
normalise_insts([], [_|_], _, _) :-
	error("normalise_insts: length mismatch").
normalise_insts([_|_], [], _, _) :-
	error("normalise_insts: length mismatch").

	% This is a bit of a hack.
	% The aim is to avoid non-termination due to the creation
	% of ever-expanding insts.
	% XXX should also normalise partially instantiated insts.

normalise_inst(Inst0, Type, ModuleInfo, NormalisedInst) :-
	inst_expand(ModuleInfo, Inst0, Inst),
	( Inst = bound(_, _) ->
		(
			inst_is_ground(ModuleInfo, Inst),
			inst_is_unique(ModuleInfo, Inst),
			% don't infer unique modes for introduced type_infos
			% arguments, because that leads to an increase
			% in the number of inferred modes without any benefit
			\+ is_introduced_type_info_type(Type),
			\+ inst_contains_nonstandard_func_mode(Inst, ModuleInfo)
		->
			NormalisedInst = ground(unique, none)
		;
			inst_is_ground(ModuleInfo, Inst),
			inst_is_mostly_unique(ModuleInfo, Inst),
			% don't infer unique modes for introduced type_infos
			% arguments, because that leads to an increase
			% in the number of inferred modes without any benefit
			\+ is_introduced_type_info_type(Type),
			\+ inst_contains_nonstandard_func_mode(Inst, ModuleInfo)
		->
			NormalisedInst = ground(mostly_unique, none)
		;
			inst_is_ground(ModuleInfo, Inst),
			\+ inst_is_clobbered(ModuleInfo, Inst),
			\+ inst_contains_nonstandard_func_mode(Inst, ModuleInfo)
		->
			NormalisedInst = ground(shared, none)
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

partition_args(_, [], [_|_], _, _) :-
	error("partition_args").
partition_args(_, [_|_], [], _, _) :-
	error("partition_args").
partition_args(_, [], [], [], []).
partition_args(ModuleInfo, [ArgMode | ArgModes], [Arg | Args],
		InputArgs, OutputArgs) :-
	partition_args(ModuleInfo, ArgModes, Args, InputArgs1, OutputArgs1),
	( mode_is_input(ModuleInfo, ArgMode) ->
		InputArgs = [Arg | InputArgs1],
		OutputArgs = OutputArgs1
	;
		InputArgs = InputArgs1,
		OutputArgs = [Arg | OutputArgs1]
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
