%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% mode_util.m - utility predicates dealing with modes and insts.

% Main author: fjh.

%-----------------------------------------------------------------------------%

:- module mode_util.

:- interface.

:- import_module hlds_module, hlds_pred, hlds_goal, hlds_data, prog_data.
:- import_module (inst), instmap.
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

:- pred modes_to_arg_modes(module_info, list(mode), list(type),
		list(arg_mode)).
:- mode modes_to_arg_modes(in, in, in, out) is det.

	% Given an expanded inst and a cons_id and its arity, return the 
	% insts of the arguments of the top level functor, failing if the
	% inst could not be bound to the functor.
:- pred get_arg_insts(inst, cons_id, arity, list(inst)).
:- mode get_arg_insts(in, in, in, out) is semidet.

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
:- pred recompute_instmap_delta_proc(bool::in, proc_info::in, proc_info::out,
	module_info::in, module_info::out) is det.

:- pred recompute_instmap_delta(bool::in, hlds_goal::in, hlds_goal::out,
	vartypes::in, inst_varset::in, instmap::in, module_info::in,
	module_info::out) is det.


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

	% Convert a list of constructors to a list of bound_insts.
	% Note that the list(bound_inst) is not sorted and may contain
	% duplicates.
:- pred constructors_to_bound_insts(list(constructor), uniqueness, module_info,
				list(bound_inst)).
:- mode constructors_to_bound_insts(in, in, in, out) is det.


	% Given the mode of a predicate,
	% work out which arguments are live (might be used again
	% by the caller of that predicate) and which are dead.
:- pred get_arg_lives(list(mode), module_info, list(is_live)).
:- mode get_arg_lives(in, in, out) is det.

	% Predicates to make error messages more readable by stripping
	% "builtin:" module qualifiers from modes.

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
:- pred fixup_switch_var(prog_var, instmap, instmap, hlds_goal, hlds_goal). 
:- mode fixup_switch_var(in, in, in, in, out) is det.

%-----------------------------------------------------------------------------%

:- pred normalise_insts(list(inst), list(type), module_info, list(inst)).
:- mode normalise_insts(in, in, in, out) is det.

:- pred normalise_inst(inst, (type), module_info, inst).
:- mode normalise_inst(in, in, in, out) is det.

%-----------------------------------------------------------------------------%

	% Partition a list of arguments into inputs and others.
:- pred partition_args(module_info, list(mode), list(T), list(T), list(T)).
:- mode partition_args(in, in, in, out, out) is det.

%-----------------------------------------------------------------------------%

:- pred inst_list_apply_substitution(list(inst), inst_var_sub, list(inst)).
:- mode inst_list_apply_substitution(in, in, out) is det.

:- pred mode_list_apply_substitution(list(mode), inst_var_sub, list(mode)).
:- mode mode_list_apply_substitution(in, in, out) is det.

%-----------------------------------------------------------------------------%

:- pred rename_apart_inst_vars(inst_varset, inst_varset, list(mode),
		list(mode)).
:- mode rename_apart_inst_vars(in, in, in, out) is det.

%-----------------------------------------------------------------------------%

	% Construct a mode corresponding to the standard `in',
	% `out', `uo' or `unused' mode.
:- pred in_mode((mode)::out) is det.
:- pred out_mode((mode)::out) is det.
:- pred uo_mode((mode)::out) is det.
:- pred unused_mode((mode)::out) is det.

	% Construct the modes used for `aditi__state' arguments.
	% XXX These should be unique, but are not yet because that
	% would require alias tracking.
:- func aditi_mui_mode = (mode).
:- func aditi_di_mode = (mode).
:- func aditi_uo_mode = (mode).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module require, int, map, set, std_util, assoc_list, varset.
:- import_module prog_util, prog_io, type_util.
:- import_module inst_match, inst_util, mode_info.
:- import_module require, int, map, set, term, std_util, assoc_list, varset.

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
	( Initial = free, Final = ground(shared, none) ->
		make_std_mode("out", [], Mode)
	; Initial = free, Final = ground(unique, none) ->
		make_std_mode("uo", [], Mode)
	; Initial = free, Final = ground(mostly_unique, none) ->
		make_std_mode("muo", [], Mode)
	; Initial = ground(shared, none), Final = ground(shared, none) ->
		make_std_mode("in", [], Mode)
	; Initial = ground(unique, none), Final = ground(clobbered, none) ->
		make_std_mode("di", [], Mode)
	; Initial = ground(mostly_unique, none),
	  Final = ground(mostly_clobbered, none) ->
		make_std_mode("mdi", [], Mode)
	; Initial = ground(unique, none), Final = ground(unique, none) ->
		make_std_mode("ui", [], Mode)
	; Initial = ground(mostly_unique, none),
	  Final = ground(mostly_unique, none) ->
		make_std_mode("mdi", [], Mode)
	; Initial = free ->
		make_std_mode("out", [Final], Mode)
	; Final = ground(clobbered, none) ->
		make_std_mode("di", [Initial], Mode)
	; Initial = Final ->
		make_std_mode("in", [Initial], Mode)
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

:- pred mode_to_arg_mode_2(module_info, mode, type, list(type_id), arg_mode).
:- mode mode_to_arg_mode_2(in, in, in, in, out) is det.

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
		type_to_type_id(Type, TypeId, _TypeArgs),
		\+ list__member(TypeId, ContainingTypes)
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
			[TypeId | ContainingTypes], ArgMode)
	;
		base_mode_to_arg_mode(ModuleInfo, Mode, ArgMode)
	).

:- pred base_mode_to_arg_mode(module_info, mode, arg_mode).
:- mode base_mode_to_arg_mode(in, in, out) is det.
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
:- pred get_single_arg_inst(inst, module_info, cons_id, inst).
:- mode get_single_arg_inst(in, in, in, out) is det.

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

functors_to_cons_ids([], []).
functors_to_cons_ids([Functor | Functors], [ConsId | ConsIds]) :-
        Functor = functor(ConsId, _ArgInsts),
        functors_to_cons_ids(Functors, ConsIds).

%-----------------------------------------------------------------------------%

get_arg_insts(not_reached, _ConsId, Arity, ArgInsts) :-
	list__duplicate(Arity, not_reached, ArgInsts).
get_arg_insts(ground(Uniq, _PredInst), _ConsId, Arity, ArgInsts) :-
	list__duplicate(Arity, ground(Uniq, none), ArgInsts).
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
		( MaybeInst = known(Inst0, _) ->
			Inst = Inst0
		;
			Inst = defined_inst(InstName)
		)
	; InstName = any_inst(_, _, _, _),
		module_info_insts(ModuleInfo, InstTable),
		inst_table_get_any_insts(InstTable, AnyInstTable),
		map__lookup(AnyInstTable, InstName, MaybeInst),
		( MaybeInst = known(Inst0, _) ->
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
			ground(Uniq, none), Inst)
	; InstName = typed_inst(Type, TypedInstName),
		inst_lookup_2(TypedInstName, ModuleInfo, Inst0),
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
propagate_ctor_info(ground(Uniq, none), Type, Constructors, ModuleInfo, Inst)
		:-
	( type_is_higher_order(Type, function, _, ArgTypes) ->
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
		type_is_higher_order(Type, PredOrFunc, _, ArgTypes),
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

propagate_ctor_info(ground(Uniq, constrained_inst_var(Var)), _, _, _,
		ground(Uniq, constrained_inst_var(Var))).
propagate_ctor_info(not_reached, _Type, _Constructors, _ModuleInfo,
		not_reached).
propagate_ctor_info(inst_var(V), _, _, _, inst_var(V)).
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
propagate_ctor_info_lazily(ground(Uniq, none), Type0, Subst, ModuleInfo, Inst)
		:-
	apply_type_subst(Type0, Subst, Type),
	( type_is_higher_order(Type, function, _, ArgTypes) ->
		default_higher_order_func_inst(ArgTypes, ModuleInfo,
			HigherOrderInstInfo),
		Inst = ground(Uniq, higher_order(HigherOrderInstInfo))
	;
		% XXX The information added by this is not yet used,
		% so it's disabled since it unnecessarily complicates
		% the insts.
		/*********
		Inst = defined_inst(typed_ground(Uniq, Type)) 
		*********/
		Inst = ground(Uniq, none)
	).

propagate_ctor_info_lazily(ground(Uniq, higher_order(PredInstInfo0)), Type0,
		Subst, ModuleInfo, ground(Uniq, higher_order(PredInstInfo))) :-
	PredInstInfo0 = pred_inst_info(PredOrFunc, Modes0, Det),
	PredInstInfo = pred_inst_info(PredOrFunc, Modes, Det),
	apply_type_subst(Type0, Subst, Type),
	(
		type_is_higher_order(Type, PredOrFunc, _, ArgTypes),
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
propagate_ctor_info_lazily(ground(Uniq, constrained_inst_var(Var)), _, _, _,
		ground(Uniq, constrained_inst_var(Var))).
propagate_ctor_info_lazily(not_reached, _Type, _, _ModuleInfo, not_reached).
propagate_ctor_info_lazily(inst_var(Var), _, _, _, inst_var(Var)).
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
	In = (ground(shared, none) -> ground(shared, none)),
	Out = (free -> ground(shared, none)),
	list__length(PredArgTypes, NumPredArgs),
	NumFuncArgs is NumPredArgs - 1,
	list__duplicate(NumFuncArgs, In, FuncArgModes),
	FuncRetMode = Out,
	list__append(FuncArgModes, [FuncRetMode], PredArgModes0),
	propagate_types_into_mode_list(PredArgTypes, ModuleInfo,
		PredArgModes0, PredArgModes),
	PredInstInfo = pred_inst_info(function, PredArgModes, det).

constructors_to_bound_insts([], _, _, []).
constructors_to_bound_insts([Ctor | Ctors], Uniq, ModuleInfo,
		[BoundInst | BoundInsts]) :-
	Ctor = ctor(_ExistQVars, _Constraints, Name, Args),
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
	Inst = ground(Uniq, none),
	ctor_arg_list_to_inst_list(Args, Uniq, Insts).

:- pred propagate_ctor_info_2(list(bound_inst), (type), module_info,
		list(bound_inst)).
:- mode propagate_ctor_info_2(in, in, in, out) is det.

propagate_ctor_info_2(BoundInsts0, Type, ModuleInfo, BoundInsts) :-
	(
		type_is_tuple(Type, TupleArgTypes)
	->
		list__map(
		    (pred(BoundInst0::in, BoundInst::out) is det :-
			BoundInst0 = functor(Functor, ArgInsts0),
			(
				Functor = cons(unqualified("{}"), _),
				list__length(ArgInsts0,
					list__length(TupleArgTypes))
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
			BoundInst = functor(Functor, ArgInsts)
		    ), BoundInsts0, BoundInsts)
	;
		type_to_type_id(Type, TypeId, TypeArgs),
		TypeId = qualified(TypeModule, _) - _,
		module_info_types(ModuleInfo, TypeTable),
		map__search(TypeTable, TypeId, TypeDefn),
		hlds_data__get_type_defn_tparams(TypeDefn, TypeParams0),
		hlds_data__get_type_defn_body(TypeDefn, TypeBody),
		TypeBody = du_type(Constructors, _, _, _)
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

:- pred propagate_ctor_info_3(list(bound_inst), module_name, list(constructor),
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
				Ctor = ctor(_, _, ConsName, CtorArgs),
				list__length(CtorArgs, Arity)
			)),
		list__filter(GetCons, Constructors, [Constructor])
	->
		Constructor = ctor(_ExistQVars, _Constraints, _Name, Args),
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

:- pred inst_lookup_subst_args(hlds_inst_body, list(inst_var), sym_name,
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
	mode_get_insts_semidet(ModuleInfo, Mode, Initial, Final).

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

:- pred mode_substitute_arg_list(mode, list(inst_var), list(inst), mode).
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

:- pred inst_substitute_arg_list(inst, list(inst_var), list(inst), inst).
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

:- pred mode_apply_substitution(mode, inst_var_sub, mode).
:- mode mode_apply_substitution(in, in, out) is det.

mode_apply_substitution((I0 -> F0), Subst, (I -> F)) :-
	inst_apply_substitution(I0, Subst, I),
	inst_apply_substitution(F0, Subst, F).
mode_apply_substitution(user_defined_mode(Name, Args0), Subst,
		    user_defined_mode(Name, Args)) :-
	inst_list_apply_substitution_2(Args0, Subst, Args).

	% inst_list_apply_substitution(Insts0, Subst, Insts) is true
	% iff Inst is the inst that results from applying Subst to Insts0.

inst_list_apply_substitution(Insts0, Subst, Insts) :-
	( map__is_empty(Subst) ->
		Insts = Insts0
	;
		inst_list_apply_substitution_2(Insts0, Subst, Insts)
	).
		
:- pred inst_list_apply_substitution_2(list(inst), inst_var_sub, list(inst)).
:- mode inst_list_apply_substitution_2(in, in, out) is det.

inst_list_apply_substitution_2([], _, []).
inst_list_apply_substitution_2([A0 | As0], Subst, [A | As]) :-
	inst_apply_substitution(A0, Subst, A),
	inst_list_apply_substitution_2(As0, Subst, As).

	% inst_substitute_arg(Inst0, Subst, Inst) is true
	% iff Inst is the inst that results from substituting all
	% occurrences of Param in Inst0 with Arg.

:- pred inst_apply_substitution(inst, inst_var_sub, inst).
:- mode inst_apply_substitution(in, in, out) is det.

inst_apply_substitution(any(Uniq), _, any(Uniq)).
inst_apply_substitution(free, _, free).
inst_apply_substitution(free(T), _, free(T)).
inst_apply_substitution(ground(Uniq, GroundInstInfo0), Subst, Inst) :-
	ground_inst_info_apply_substitution(GroundInstInfo0, Subst, Uniq, Inst).
inst_apply_substitution(bound(Uniq, Alts0), Subst, bound(Uniq, Alts)) :-
	alt_list_apply_substitution(Alts0, Subst, Alts).
inst_apply_substitution(not_reached, _, not_reached).
inst_apply_substitution(inst_var(Var), Subst, Result) :-
	(
		map__search(Subst, Var, Replacement)
	->
		Result = Replacement
	;
		Result = inst_var(Var)
	).
inst_apply_substitution(defined_inst(InstName0), Subst,
		    defined_inst(InstName)) :-
	( inst_name_apply_substitution(InstName0, Subst, InstName1) ->
		InstName = InstName1
	;
		InstName = InstName0
	).
inst_apply_substitution(abstract_inst(Name, Args0), Subst,
		    abstract_inst(Name, Args)) :-
	inst_list_apply_substitution_2(Args0, Subst, Args).

	% This predicate fails if the inst_name is not one of user_inst,
	% typed_inst or typed_ground.  The other types of inst_names are just
	% used as keys in the inst_table so it does not make sense to apply
	% substitutions to them.
:- pred inst_name_apply_substitution(inst_name, inst_var_sub, inst_name).
:- mode inst_name_apply_substitution(in, in, out) is semidet.

inst_name_apply_substitution(user_inst(Name, Args0), Subst,
		user_inst(Name, Args)) :-
	inst_list_apply_substitution_2(Args0, Subst, Args).
inst_name_apply_substitution(typed_inst(T, Inst0), Subst,
		typed_inst(T, Inst)) :-
	inst_name_apply_substitution(Inst0, Subst, Inst).
inst_name_apply_substitution(typed_ground(Uniq, T), _, typed_ground(Uniq, T)).

:- pred alt_list_apply_substitution(list(bound_inst), inst_var_sub,
				list(bound_inst)).
:- mode alt_list_apply_substitution(in, in, out) is det.

alt_list_apply_substitution([], _, []).
alt_list_apply_substitution([Alt0|Alts0], Subst, [Alt|Alts]) :-
	Alt0 = functor(Name, Args0),
	inst_list_apply_substitution_2(Args0, Subst, Args),
	Alt = functor(Name, Args),
	alt_list_apply_substitution(Alts0, Subst, Alts).

:- pred ground_inst_info_apply_substitution(ground_inst_info, inst_var_sub,
				uniqueness, inst).
:- mode ground_inst_info_apply_substitution(in, in, in, out) is det.

ground_inst_info_apply_substitution(none, _, Uniq, ground(Uniq, none)).
ground_inst_info_apply_substitution(GII0, Subst, Uniq, ground(Uniq, GII)) :-
	GII0 = higher_order(pred_inst_info(PredOrFunc, Modes0, Det)),
	mode_list_apply_substitution(Modes0, Subst, Modes),
	GII = higher_order(pred_inst_info(PredOrFunc, Modes, Det)).
ground_inst_info_apply_substitution(constrained_inst_var(Var), Subst, Uniq,
		Inst) :-
	(
		map__search(Subst, Var, Inst0)
	->
		Inst = Inst0
	;
		Inst = ground(Uniq, constrained_inst_var(Var))
	).

	% mode_list_apply_substitution(Modes0, Subst, Modes) is true
	% iff Mode is the mode that results from applying Subst to Modes0.

mode_list_apply_substitution(Modes0, Subst, Modes) :-
	( map__is_empty(Subst) ->
		Modes = Modes0
	;
		mode_list_apply_substitution_2(Modes0, Subst, Modes)
	).

:- pred mode_list_apply_substitution_2(list(mode), inst_var_sub, list(mode)).
:- mode mode_list_apply_substitution_2(in, in, out) is det.

mode_list_apply_substitution_2([], _, []).
mode_list_apply_substitution_2([A0 | As0], Subst, [A | As]) :-
	mode_apply_substitution(A0, Subst, A),
	mode_list_apply_substitution_2(As0, Subst, As).

%-----------------------------------------------------------------------------%

rename_apart_inst_vars(VarSet, NewVarSet, Modes0, Modes) :-
	varset__merge_subst(VarSet, NewVarSet, _, Sub),
	list__map(rename_apart_inst_vars_in_mode(Sub), Modes0, Modes).

:- pred rename_apart_inst_vars_in_mode(substitution(inst_var_type), mode, mode).
:- mode rename_apart_inst_vars_in_mode(in, in, out) is det.

rename_apart_inst_vars_in_mode(Sub, I0 -> F0, I -> F) :-
	rename_apart_inst_vars_in_inst(Sub, I0, I),
	rename_apart_inst_vars_in_inst(Sub, F0, F).
rename_apart_inst_vars_in_mode(Sub, user_defined_mode(Name, Insts0),
		user_defined_mode(Name, Insts)) :-
	list__map(rename_apart_inst_vars_in_inst(Sub), Insts0, Insts).

:- pred rename_apart_inst_vars_in_inst(substitution(inst_var_type), inst, inst).
:- mode rename_apart_inst_vars_in_inst(in, in, out) is det.

rename_apart_inst_vars_in_inst(_, any(U), any(U)).
rename_apart_inst_vars_in_inst(_, free, free).
rename_apart_inst_vars_in_inst(_, free(T), free(T)).
rename_apart_inst_vars_in_inst(Sub, bound(U, BIs0), bound(U, BIs)) :-
	list__map((pred(functor(C, Is0)::in, functor(C, Is)::out) is det :-
		list__map(rename_apart_inst_vars_in_inst(Sub), Is0, Is)),
		BIs0, BIs).
rename_apart_inst_vars_in_inst(Sub, ground(U, GI0), ground(U, GI)) :-
	(
		GI0 = higher_order(pred_inst_info(PoF, Modes0, Det)),
		list__map(rename_apart_inst_vars_in_mode(Sub), Modes0, Modes),
		GI = higher_order(pred_inst_info(PoF, Modes, Det))
	;
		GI0 = constrained_inst_var(V0),
		( map__search(Sub, V0, term__variable(V)) ->
			GI = constrained_inst_var(V)
		;
			GI = GI0
		)
	;
		GI0 = none,
		GI = none
	).
rename_apart_inst_vars_in_inst(_, not_reached, not_reached).
rename_apart_inst_vars_in_inst(Sub, inst_var(V0), inst_var(V)) :-
	( map__search(Sub, V0, term__variable(V1)) ->
		V = V1
	;
		V = V0
	).
rename_apart_inst_vars_in_inst(Sub, defined_inst(Name0), defined_inst(Name)) :-
	( rename_apart_inst_vars_in_inst_name(Sub, Name0, Name1) ->
		Name = Name1
	;
		Name = Name0
	).
rename_apart_inst_vars_in_inst(Sub, abstract_inst(Sym, Insts0),
		abstract_inst(Sym, Insts)) :-
	list__map(rename_apart_inst_vars_in_inst(Sub), Insts0, Insts).

:- pred rename_apart_inst_vars_in_inst_name(substitution(inst_var_type),
		inst_name, inst_name).
:- mode rename_apart_inst_vars_in_inst_name(in, in, out) is semidet.

rename_apart_inst_vars_in_inst_name(Sub, user_inst(Sym, Insts0),
		user_inst(Sym, Insts)) :-
	list__map(rename_apart_inst_vars_in_inst(Sub), Insts0, Insts).
rename_apart_inst_vars_in_inst_name(Sub, typed_inst(Type, Name0),
		typed_inst(Type, Name)) :-
	rename_apart_inst_vars_in_inst_name(Sub, Name0, Name).
rename_apart_inst_vars_in_inst_name(_, typed_ground(U, T), typed_ground(U, T)).


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

recompute_instmap_delta_proc(RecomputeAtomic, ProcInfo0, ProcInfo) -->
	=(ModuleInfo0),
	{ proc_info_get_initial_instmap(ProcInfo0, ModuleInfo0, InstMap0) },
	{ proc_info_vartypes(ProcInfo0, VarTypes) },
	{ proc_info_goal(ProcInfo0, Goal0) },
	{ proc_info_inst_varset(ProcInfo0, InstVarSet) },
	recompute_instmap_delta(RecomputeAtomic, Goal0, Goal,
		VarTypes, InstVarSet, InstMap0),
	{ proc_info_set_goal(ProcInfo0, Goal, ProcInfo) }.

recompute_instmap_delta(RecomputeAtomic, Goal0, Goal, VarTypes, InstVarSet,
		InstMap0, ModuleInfo0, ModuleInfo) :-
	RI0 = recompute_info(ModuleInfo0, InstVarSet),
	recompute_instmap_delta_1(RecomputeAtomic, Goal0, Goal, VarTypes,
		InstMap0, _, RI0, RI),
	ModuleInfo = RI^module_info.

:- pred recompute_instmap_delta_1(bool, hlds_goal, hlds_goal, vartypes,
	instmap, instmap_delta, recompute_info, recompute_info).
:- mode recompute_instmap_delta_1(in, in, out, in, in, out, in, out) is det.

recompute_instmap_delta_1(RecomputeAtomic, Goal0 - GoalInfo0, Goal - GoalInfo,
		VarTypes, InstMap0, InstMapDelta, RI0, RI) :-
	( 
		RecomputeAtomic = no,
		goal_is_atomic(Goal0),
		Goal0 \= unify(_,lambda_goal(_,_,_,_,_,_,_,_),_,_,_)
			% Lambda expressions always need to be processed.
	->
		Goal = Goal0,
		GoalInfo1 = GoalInfo0,
		RI0 = RI
	;
		recompute_instmap_delta_2(RecomputeAtomic, Goal0, GoalInfo0,
			Goal, VarTypes, InstMap0, InstMapDelta0, RI0, RI),
		goal_info_get_nonlocals(GoalInfo0, NonLocals),
		instmap_delta_restrict(InstMapDelta0,
			NonLocals, InstMapDelta1),
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
:- pred update_module_info(pred(T, module_info, module_info), T,
		recompute_info, recompute_info).
:- mode update_module_info(pred(out, in, out) is det, out, in, out) is det.

update_module_info(P, R) -->
	ModuleInfo0 =^ module_info,
	{ P(R, ModuleInfo0, ModuleInfo) },
	^module_info := ModuleInfo.

:- pred recompute_instmap_delta_2(bool, hlds_goal_expr, hlds_goal_info,
		hlds_goal_expr, vartypes, instmap, instmap_delta,
		recompute_info, recompute_info).
:- mode recompute_instmap_delta_2(in, in, in, out, in, in, out, in, out) is det.

recompute_instmap_delta_2(Atomic, switch(Var, Det, Cases0, SM), GoalInfo,
		switch(Var, Det, Cases, SM), VarTypes, InstMap, InstMapDelta)
		-->
	{ goal_info_get_nonlocals(GoalInfo, NonLocals) },
	recompute_instmap_delta_cases(Atomic, Var, Cases0, Cases,
		VarTypes, InstMap, NonLocals, InstMapDelta).

recompute_instmap_delta_2(Atomic, conj(Goals0), _, conj(Goals),
		VarTypes, InstMap, InstMapDelta) -->
	recompute_instmap_delta_conj(Atomic, Goals0, Goals,
		VarTypes, InstMap, InstMapDelta).

recompute_instmap_delta_2(Atomic, par_conj(Goals0, SM), GoalInfo,
		par_conj(Goals, SM), VarTypes, InstMap, InstMapDelta) -->
	{ goal_info_get_nonlocals(GoalInfo, NonLocals) },
	recompute_instmap_delta_par_conj(Atomic, Goals0, Goals,
		VarTypes, InstMap, NonLocals, InstMapDelta).

recompute_instmap_delta_2(Atomic, disj(Goals0, SM), GoalInfo, disj(Goals, SM),
		VarTypes, InstMap, InstMapDelta) -->
	{ goal_info_get_nonlocals(GoalInfo, NonLocals) },
	recompute_instmap_delta_disj(Atomic, Goals0, Goals,
		VarTypes, InstMap, NonLocals, InstMapDelta).

recompute_instmap_delta_2(Atomic, not(Goal0), _, not(Goal),
		VarTypes, InstMap, InstMapDelta) -->
	{ instmap_delta_init_reachable(InstMapDelta) },
	recompute_instmap_delta_1(Atomic, Goal0, Goal, VarTypes, InstMap, _).

recompute_instmap_delta_2(Atomic, if_then_else(Vars, A0, B0, C0, SM), GoalInfo,
		if_then_else(Vars, A, B, C, SM), VarTypes, InstMap0,
		InstMapDelta) -->
	recompute_instmap_delta_1(Atomic, A0, A, VarTypes, InstMap0,
		InstMapDelta1),
	{ instmap__apply_instmap_delta(InstMap0, InstMapDelta1, InstMap1) },
	recompute_instmap_delta_1(Atomic, B0, B, VarTypes, InstMap1,
		InstMapDelta2),
	recompute_instmap_delta_1(Atomic, C0, C, VarTypes, InstMap0,
		InstMapDelta3),
	{ instmap_delta_apply_instmap_delta(InstMapDelta1, InstMapDelta2,
		InstMapDelta4) },
	{ goal_info_get_nonlocals(GoalInfo, NonLocals) },
	update_module_info(merge_instmap_delta(InstMap0, NonLocals,
		VarTypes, InstMapDelta3, InstMapDelta4), InstMapDelta).

recompute_instmap_delta_2(Atomic, some(Vars, CanRemove, Goal0), _,
		some(Vars, CanRemove, Goal),
		VarTypes, InstMap, InstMapDelta) -->
	recompute_instmap_delta_1(Atomic, Goal0, Goal, VarTypes, InstMap,
		InstMapDelta).

recompute_instmap_delta_2(_, generic_call(A, Vars, Modes, D), _,
		generic_call(A, Vars, Modes, D),
		_VarTypes, _InstMap, InstMapDelta) -->
	ModuleInfo =^ module_info,
	{ instmap_delta_from_mode_list(Vars, Modes,
		ModuleInfo, InstMapDelta) }.

recompute_instmap_delta_2(_, call(PredId, ProcId, Args, D, E, F), _,
		call(PredId, ProcId, Args, D, E, F), VarTypes,
		InstMap, InstMapDelta) -->
	recompute_instmap_delta_call(PredId, ProcId,
		Args, VarTypes, InstMap, InstMapDelta).

recompute_instmap_delta_2(Atomic, unify(A, Rhs0, UniMode0, Uni, E), GoalInfo, 
		unify(A, Rhs, UniMode, Uni, E), VarTypes, InstMap0,
		InstMapDelta) -->
	(
		{ Rhs0 = lambda_goal(PorF, EvalMethod, FixModes, NonLocals,
			LambdaVars, Modes, Det, Goal0) }
	->
		ModuleInfo0 =^ module_info,
		{ instmap__pre_lambda_update(ModuleInfo0, LambdaVars, Modes,
			InstMap0, InstMap) },
		recompute_instmap_delta_1(Atomic, Goal0, Goal, VarTypes,
			InstMap, _),
		{ Rhs = lambda_goal(PorF, EvalMethod, FixModes, NonLocals,
			LambdaVars, Modes, Det, Goal) }
	;
		{ Rhs = Rhs0 }
	),
	( { Atomic = yes } ->
		recompute_instmap_delta_unify(Uni, UniMode0, UniMode,
			GoalInfo, InstMap0, InstMapDelta)
	;
		{ UniMode = UniMode0 },
		{ goal_info_get_instmap_delta(GoalInfo, InstMapDelta) }
	).

recompute_instmap_delta_2(_, 
		foreign_proc(A, PredId, ProcId, Args, E, F, G), _,
		foreign_proc(A, PredId, ProcId, Args, E, F, G), 
		VarTypes, InstMap, InstMapDelta) -->
	recompute_instmap_delta_call(PredId, ProcId,
		Args, VarTypes, InstMap, InstMapDelta).

recompute_instmap_delta_2(_, shorthand(_), _, _, _, _, _) -->
	% these should have been expanded out by now
	{ error("recompute_instmap_delta_2: unexpected shorthand") }.
	
%-----------------------------------------------------------------------------%

:- pred recompute_instmap_delta_conj(bool::in, list(hlds_goal)::in,
	list(hlds_goal)::out, vartypes::in, instmap::in, instmap_delta::out,
	recompute_info::in, recompute_info::out) is det.

recompute_instmap_delta_conj(_, [], [], _, _, InstMapDelta) -->
	{ instmap_delta_init_reachable(InstMapDelta) }.
recompute_instmap_delta_conj(Atomic, [Goal0 | Goals0], [Goal | Goals],
		VarTypes, InstMap0, InstMapDelta) -->
	recompute_instmap_delta_1(Atomic, Goal0, Goal,
		VarTypes, InstMap0, InstMapDelta0),
	{ instmap__apply_instmap_delta(InstMap0, InstMapDelta0, InstMap1) },
	recompute_instmap_delta_conj(Atomic, Goals0, Goals, VarTypes, InstMap1,
		InstMapDelta1),
	{ instmap_delta_apply_instmap_delta(InstMapDelta0, InstMapDelta1,
		InstMapDelta) }.

%-----------------------------------------------------------------------------%

:- pred recompute_instmap_delta_disj(bool::in, list(hlds_goal)::in,
	list(hlds_goal)::out, vartypes::in, instmap::in, set(prog_var)::in,
	instmap_delta::out, recompute_info::in, recompute_info::out) is det.

recompute_instmap_delta_disj(_, [], [], _, _, _, InstMapDelta) -->
	{ instmap_delta_init_unreachable(InstMapDelta) }.
recompute_instmap_delta_disj(Atomic, [Goal0], [Goal],
		VarTypes, InstMap, _, InstMapDelta) -->
	recompute_instmap_delta_1(Atomic, Goal0, Goal, VarTypes, InstMap,
		InstMapDelta).
recompute_instmap_delta_disj(Atomic, [Goal0 | Goals0], [Goal | Goals],
		VarTypes, InstMap, NonLocals, InstMapDelta) -->
	{ Goals0 = [_|_] },
	recompute_instmap_delta_1(Atomic, Goal0, Goal,
		VarTypes, InstMap, InstMapDelta0),
	recompute_instmap_delta_disj(Atomic, Goals0, Goals,
		VarTypes, InstMap, NonLocals, InstMapDelta1),
	update_module_info(merge_instmap_delta(InstMap, NonLocals,
		VarTypes, InstMapDelta0, InstMapDelta1), InstMapDelta).

:- pred recompute_instmap_delta_par_conj(bool::in, list(hlds_goal)::in,
	list(hlds_goal)::out, vartypes::in, instmap::in, set(prog_var)::in,
	instmap_delta::out, recompute_info::in, recompute_info::out) is det.

recompute_instmap_delta_par_conj(_, [], [], _, _, _, InstMapDelta) -->
	{ instmap_delta_init_unreachable(InstMapDelta) }.
recompute_instmap_delta_par_conj(Atomic, [Goal0], [Goal],
		VarTypes, InstMap, _, InstMapDelta) -->
	recompute_instmap_delta_1(Atomic, Goal0, Goal, VarTypes, InstMap,
		InstMapDelta).
recompute_instmap_delta_par_conj(Atomic, [Goal0 | Goals0], [Goal | Goals],
		VarTypes, InstMap, NonLocals, InstMapDelta) -->
	{ Goals0 = [_|_] },
	recompute_instmap_delta_1(Atomic, Goal0, Goal,
		VarTypes, InstMap, InstMapDelta0),
	recompute_instmap_delta_par_conj(Atomic, Goals0, Goals,
		VarTypes, InstMap, NonLocals, InstMapDelta1),
	update_module_info(unify_instmap_delta(InstMap, NonLocals,
		InstMapDelta0, InstMapDelta1), InstMapDelta).

%-----------------------------------------------------------------------------%

:- pred recompute_instmap_delta_cases(bool::in, prog_var::in, list(case)::in,
	list(case)::out, vartypes::in, instmap::in, set(prog_var)::in,
	instmap_delta::out, recompute_info::in, recompute_info::out) is det.

recompute_instmap_delta_cases(_, _, [], [], _, _, _, InstMapDelta) -->
	{ instmap_delta_init_unreachable(InstMapDelta) }.
recompute_instmap_delta_cases(Atomic, Var, [Case0 | Cases0], [Case | Cases],
		VarTypes, InstMap0, NonLocals, InstMapDelta) -->
	{ Case0 = case(Functor, Goal0) },
	{ map__lookup(VarTypes, Var, Type) },
	update_module_info(instmap__bind_var_to_functor(Var, Type, Functor,
		InstMap0), InstMap),
	recompute_instmap_delta_1(Atomic, Goal0, Goal, VarTypes, InstMap,
		InstMapDelta0),
	update_module_info(instmap_delta_bind_var_to_functor(Var, Type, Functor,
		InstMap0, InstMapDelta0), InstMapDelta1),
	{ Case = case(Functor, Goal) },
	recompute_instmap_delta_cases(Atomic, Var, Cases0, Cases,
		VarTypes, InstMap0, NonLocals, InstMapDelta2),
	update_module_info(merge_instmap_delta(InstMap0, NonLocals,
		VarTypes, InstMapDelta1, InstMapDelta2), InstMapDelta).

%-----------------------------------------------------------------------------%

:- pred recompute_instmap_delta_call(pred_id::in, proc_id::in,
	list(prog_var)::in, vartypes::in, instmap::in, instmap_delta::out,
	recompute_info::in, recompute_info::out) is det.

recompute_instmap_delta_call(PredId, ProcId, Args, VarTypes, InstMap,
		InstMapDelta) -->
	ModuleInfo =^ module_info,
	{ module_info_pred_proc_info(ModuleInfo, PredId, ProcId, _, ProcInfo) },
	{ proc_info_interface_determinism(ProcInfo, Detism) },
	( { determinism_components(Detism, _, at_most_zero) } ->
		{ instmap_delta_init_unreachable(InstMapDelta) }
	;
		{ proc_info_argmodes(ProcInfo, ArgModes0) },
		{ proc_info_inst_varset(ProcInfo, ProcInstVarSet) },
		InstVarSet =^ inst_varset,
		{ rename_apart_inst_vars(InstVarSet, ProcInstVarSet,
			ArgModes0, ArgModes1) },
		{ mode_list_get_initial_insts(ArgModes1, ModuleInfo,
			InitialInsts) },

		% Compute the inst_var substitution from the initial insts
		% of the called procedure and the insts of the argument
		% variables.
		{ map__init(InstVarSub0) },
		update_module_info(compute_inst_var_sub(Args, VarTypes, InstMap,
			InitialInsts, InstVarSub0), InstVarSub),

		% Apply the inst_var substitution to the argument modes.
		{ mode_list_apply_substitution(ArgModes1, InstVarSub,
			ArgModes2) },

		% Calculate the final insts of the argument variables
		% from their initial insts and the final insts of the called
		% procedure (with inst_var substitutions applied).
		update_module_info(recompute_instmap_delta_call_2(Args, InstMap,
			ArgModes2), ArgModes),
		{ instmap_delta_from_mode_list(Args, ArgModes,
			ModuleInfo, InstMapDelta) }
	).

:- pred compute_inst_var_sub(list(prog_var), vartypes, instmap,
	list(inst), inst_var_sub, inst_var_sub, module_info, module_info).
:- mode compute_inst_var_sub(in, in, in, in, in, out, in, out) is det.

compute_inst_var_sub([], _, _, [], Sub, Sub, ModuleInfo, ModuleInfo).
compute_inst_var_sub([_|_], _, _, [], _, _, _, _) :-
	error("compute_inst_var_sub").
compute_inst_var_sub([], _, _, [_|_], _, _, _, _) :-
	error("compute_inst_var_sub").
compute_inst_var_sub([Arg | Args], VarTypes, InstMap, [Inst | Insts],
		Sub0, Sub, ModuleInfo0, ModuleInfo) :-
	% This is similar to modecheck_var_has_inst.
	( instmap__is_reachable(InstMap) ->
		instmap__lookup_var(InstMap, Arg, ArgInst),
		map__lookup(VarTypes, Arg, Type),
		(
			inst_matches_initial(ArgInst, Inst, Type, ModuleInfo0,
				ModuleInfo1, Sub0, Sub1)
		->
			ModuleInfo2 = ModuleInfo1,
			Sub2 = Sub1
		;
			% error("compute_inst_var_sub: inst_matches_initial failed")
			% XXX  We shouldn't ever get here, but unfortunately the
			% mode system currently has several problems (most
			% noticeably lack of alias tracking for unique modes)
			% which mean inst_matches_initial can sometimes fail
			% here.
			ModuleInfo2 = ModuleInfo0,
			Sub2 = Sub0
		)
	;
		ModuleInfo2 = ModuleInfo0,
		Sub2 = Sub0
	),
	compute_inst_var_sub(Args, VarTypes, InstMap, Insts, Sub2,
		Sub, ModuleInfo2, ModuleInfo).

:- pred recompute_instmap_delta_call_2(list(prog_var)::in, instmap::in,
	list(mode)::in, list(mode)::out, module_info::in, module_info::out)
	is det.

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
			Mode = (ArgInst0 -> UnifyInst)
		;
			error("recompute_instmap_delta_call_2: unify_inst failed")
		)
	;
		Mode = (not_reached -> not_reached),
		ModuleInfo2 = ModuleInfo0
	),
	recompute_instmap_delta_call_2(Args, InstMap,
		Modes0, Modes, ModuleInfo2, ModuleInfo).

:- pred recompute_instmap_delta_unify(unification::in, unify_mode::in,
	unify_mode::out, hlds_goal_info::in, instmap::in, instmap_delta::out,
	recompute_info::in, recompute_info::out) is det.

recompute_instmap_delta_unify(Uni, UniMode0, UniMode, GoalInfo,
		InstMap, InstMapDelta) -->
	% Deconstructions are the only types of unifications
	% that can require updating of the instmap_delta after simplify.m
	% has been run.
	ModuleInfo =^ module_info,
	{
		Uni = deconstruct(Var, _ConsId, Vars, UniModes, _, _CanCGC)
	->
		% Get the final inst of the deconstructed var, which
		% will be the same as in the old instmap.
		goal_info_get_instmap_delta(GoalInfo, OldInstMapDelta),
		instmap__lookup_var(InstMap, Var, InitialInst),
		( instmap_delta_search_var(OldInstMapDelta, Var, FinalInst1) ->
			FinalInst = FinalInst1
		;
			% it wasn't in the instmap_delta, so the inst didn't
			% change.
			FinalInst = InitialInst
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
	}.

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
	% "builtin:" module qualifiers from modes and insts.
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

:- pred strip_builtin_qualifier_from_sym_name(sym_name::in, sym_name::out)
	is det.

strip_builtin_qualifier_from_sym_name(SymName0, SymName) :-
	(
		SymName0 = qualified(Module, Name),
		mercury_public_builtin_module(Module)
	->
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
strip_builtin_qualifiers_from_inst(ground(Uniq, GII0), ground(Uniq, GII)) :-
	strip_builtin_qualifiers_from_ground_inst_info(GII0, GII).
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
		Inst = ground(Uniq, none)
	;
		% for the compiler-generated insts, don't bother.
		Inst = Inst0
	).

:- pred strip_builtin_qualifiers_from_ground_inst_info(ground_inst_info::in,
		ground_inst_info::out) is det.

strip_builtin_qualifiers_from_ground_inst_info(none, none).
strip_builtin_qualifiers_from_ground_inst_info(higher_order(Pred0),
		higher_order(Pred)) :-
	Pred0 = pred_inst_info(Uniq, Modes0, Det),
	Pred = pred_inst_info(Uniq, Modes, Det),
	strip_builtin_qualifiers_from_mode_list(Modes0, Modes).
strip_builtin_qualifiers_from_ground_inst_info(constrained_inst_var(Var),
		constrained_inst_var(Var)).

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

in_mode(Mode) :- make_std_mode("in", [], Mode).

out_mode(Mode) :- make_std_mode("out", [], Mode).

uo_mode(Mode) :- make_std_mode("uo", [], Mode).

unused_mode(Mode) :- make_std_mode("unused", [], Mode).

aditi_mui_mode = Mode :- in_mode(Mode). 

aditi_di_mode = Mode :- in_mode(Mode).

aditi_uo_mode = Mode :- out_mode(Mode).

:- pred make_std_mode(string::in, list(inst)::in, (mode)::out) is det.

make_std_mode(Name, Args, Mode) :-
	mercury_public_builtin_module(MercuryBuiltin),
	QualifiedName = qualified(MercuryBuiltin, Name),
	Mode = user_defined_mode(QualifiedName, Args).

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
