%-----------------------------------------------------------------------------%
% Copyright (C) 1994-1999 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% mode_util.m - utility predicates dealing with modes and insts.

% Main author: fjh.

%-----------------------------------------------------------------------------%

:- module mode_util.

:- interface.

:- import_module hlds_module, hlds_pred, hlds_goal, hlds_data, prog_data.
:- import_module instmap, (inst), inst_table.
:- import_module bool, list, map, assoc_list.

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
:- pred mode_is_input(instmap, inst_table, module_info, mode).
:- mode mode_is_input(in, in, in, in) is semidet.

	% a mode is considered fully input if the inital inst is ground
:- pred mode_is_fully_input(instmap, inst_table, module_info, mode).
:- mode mode_is_fully_input(in, in, in, in) is semidet.

	% a mode is considered output if the initial inst is free
	% and the final inst is bound
:- pred mode_is_output(instmap, inst_table, module_info, mode).
:- mode mode_is_output(in, in, in, in) is semidet.

	% a mode is considered fully output if the inital inst is free and
	% the final inst is ground
:- pred mode_is_fully_output(instmap, inst_table, module_info, mode).
:- mode mode_is_fully_output(in, in, in, in) is semidet.

	% a mode is considered unused if both initial and final insts are free
:- pred mode_is_unused(instmap, inst_table, module_info, mode).
:- mode mode_is_unused(in, in, in, in) is semidet.

	% mode_to_arg_mode converts a mode (and corresponding type) to
	% an arg_mode.  A mode is a high-level notion, the normal
	% Mercury language mode.  An `arg_mode' is a low-level notion
	% used for code generation, which indicates the argument
	% passing convention (top_in, top_out, or top_unused) that
	% corresponds to that mode.  We need to know the type, not just
	% the mode, because the argument passing convention can depend
	% on the type's representation.
	%
:- pred mode_to_arg_mode(instmap, inst_table, module_info, mode, type,
		arg_mode).
:- mode mode_to_arg_mode(in, in, in, in, in, out) is det.

	% mode_to_arg_mode converts a mode (and corresponding type) to
	% an arg_mode.  A mode is a high-level notion, the normal
	% Mercury language mode.  An `arg_mode' is a low-level notion
	% used for code generation, which indicates the argument
	% passing convention (top_in, top_out, or top_unused) that
	% corresponds to that mode.  We need to know the type, not just
	% the mode, because the argument passing convention can depend
	% on the type's representation.
	%
:- pred insts_to_arg_mode(inst_table, module_info, inst, instmap,
		inst, instmap, type, arg_mode).
:- mode insts_to_arg_mode(in, in, in, in, in, in, in, out) is det.

	% Given an expanded inst and a cons_id and its arity, return the 
	% insts of the arguments of the top level functor, failing if the
	% inst could not be bound to the functor.
:- pred get_arg_insts(inst, cons_id, arity, list(inst)).
:- mode get_arg_insts(in, in, in, out) is semidet.

	% get_mode_of_args(FinalInst, InitialArgInsts, ArgModes):
	%       for a var-functor unification,
	%       given the final inst of the var
	%       and the initial insts of the functor arguments,
	%       compute the modes of the functor arguments
:- pred get_mode_of_args(inst, list(inst), assoc_list(inst, inst)).
:- mode get_mode_of_args(in, in, out) is semidet.

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

:- pred mode_util__inst_pairs_to_uni_modes(assoc_list(inst, inst),
			assoc_list(inst, inst), list(uni_mode)).
:- mode mode_util__inst_pairs_to_uni_modes(in, in, out) is det.

	% inst_lists_to_mode_list(InitialInsts, FinalInsts, Modes):
	%	Given two lists of corresponding initial and final
	%	insts, return a list of modes which maps from the
	%	initial insts to the final insts.
:- pred inst_lists_to_mode_list(list(inst), list(inst), list(mode)).
:- mode inst_lists_to_mode_list(in, in, out) is det.

	% Given a user-defined or compiler-defined inst name,
	% lookup the corresponding inst in the inst table.
	%
:- pred inst_lookup(inst_table, module_info, inst_name, inst).
:- mode inst_lookup(in, in, in, out) is det.

	% Use the instmap deltas for all the atomic sub-goals to recompute
	% the instmap deltas for all the non-atomic sub-goals of a goal.
	% Used to ensure that the instmap deltas remain valid after
	% code has been re-arranged, e.g. by followcode.
	% This also takes the module_info as input and output since it
	% may need to insert new merge_insts into the merge_inst table.
	% If the first argument is yes, the instmap_deltas for calls
	% and deconstruction unifications are also recomputed.
:- pred recompute_instmap_delta_proc(proc_info, proc_info,
		module_info, module_info).
:- mode recompute_instmap_delta_proc(in, out, in, out) is det.

:- pred recompute_instmap_delta(list(prog_var), list(is_live),
		map(prog_var, type), hlds_goal, hlds_goal, instmap, inst_table,
		inst_table, bool, module_info, module_info).
:- mode recompute_instmap_delta(in, in, in, in, out, in, in, out, out, in, out)
		is det.

	% Given corresponding lists of types and modes, produce a new
	% list of modes which includes the information provided by the
	% corresponding types.
	%
:- pred propagate_types_into_mode_list(list(type), inst_table,
		module_info, list(mode), list(mode)).
:- mode propagate_types_into_mode_list(in, in, in, in, out) is det.

	% Given corresponding lists of types and insts and a substitution
	% for the type variables in the type, produce a new list of insts
	% which includes the information provided by the corresponding types.
	%
:- pred propagate_types_into_inst_list(list(type), tsubst, inst_table,
		module_info, list(inst), list(inst)).
:- mode propagate_types_into_inst_list(in, in, in, in, in, out) is det.

	% Given the mode of a predicate,
	% work out which arguments are live (might be used again
	% by the caller of that predicate) and which are dead.
:- pred get_arg_lives(list(mode), instmap, inst_table, module_info,
			list(is_live)).
:- mode get_arg_lives(in, in, in, in, out) is det.

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
% :- pred fixup_switch_var(prog_var, instmap, instmap, hlds_goal, hlds_goal). 
% :- mode fixup_switch_var(in, in, in, in, out) is det.

%-----------------------------------------------------------------------------%

:- pred normalise_insts(list(inst), list(type), instmap, inst_table,
		module_info, list(inst)).
:- mode normalise_insts(in, in, in, in, in, out) is det.

:- pred normalise_inst(inst, (type), instmap, inst_table, module_info, inst).
:- mode normalise_inst(in, in, in, in, in, out) is det.

%-----------------------------------------------------------------------------%

% :- pred apply_inst_key_sub(inst_key_sub, instmap, instmap,
% 		inst_table, inst_table).
% :- mode apply_inst_key_sub(in, in, out, in, out) is det.

%-----------------------------------------------------------------------------%

:- pred apply_inst_table_sub_mode(inst_table_sub, mode, mode).
:- mode apply_inst_table_sub_mode(in, in, out) is det.

%-----------------------------------------------------------------------------%

	% Given a list of variables, and a list of livenesses,
	% select the live variables.
	%
:- pred get_live_vars(list(prog_var), list(is_live), list(prog_var)).
:- mode get_live_vars(in, in, out) is det.

%-----------------------------------------------------------------------------%

	% Partition a list of arguments into inputs and others.
:- pred partition_args(instmap, inst_table, module_info, list(mode),
		list(T), list(T), list(T)).
:- mode partition_args(in, in, in, in, in, out, out) is det.

%-----------------------------------------------------------------------------%

	% Construct a mode corresponding to the standard `in',
	% `out', `uo', `in(any)', `out(any)' or `unused' mode.
:- pred in_mode((mode)::out) is det.
:- pred out_mode((mode)::out) is det.
:- pred uo_mode((mode)::out) is det.
:- pred in_any_mode((mode)::out) is det.
:- pred out_any_mode((mode)::out) is det.
:- pred unused_mode((mode)::out) is det.

	% Construct the modes used for `aditi__state' arguments.
	% XXX These should be unique, but are not yet because that
	% would require alias tracking.
:- func aditi_ui_mode = (mode).
:- func aditi_di_mode = (mode).
:- func aditi_uo_mode = (mode).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module require, int, map, set, std_util, assoc_list, bag.
:- import_module prog_util, prog_io, type_util, unify_proc.
:- import_module inst_match, inst_util, det_analysis, term.

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
	( Initial = free(unique), Final = ground(shared, no) ->
		make_std_mode("out", [], Mode)
	; Initial = free(unique), Final = ground(unique, no) ->
		make_std_mode("uo", [], Mode)
	; Initial = free(unique), Final = ground(mostly_unique, no) ->
		make_std_mode("muo", [], Mode)
	; Initial = ground(shared, no), Final = ground(shared, no) ->
		make_std_mode("in", [], Mode)
	; Initial = ground(unique, no), Final = ground(clobbered, no) ->
		make_std_mode("di", [], Mode)
	; Initial = ground(mostly_unique, no),
	  Final = ground(mostly_clobbered, no) ->
		make_std_mode("mdi", [], Mode)
	; Initial = ground(unique, no), Final = ground(unique, no) ->
		make_std_mode("ui", [], Mode)
	; Initial = ground(mostly_unique, no),
	  Final = ground(mostly_unique, no) ->
		make_std_mode("mdi", [], Mode)
	; Initial = free(unique) ->
		make_std_mode("out", [Final], Mode)
	; Final = ground(clobbered, no) ->
		make_std_mode("di", [Initial], Mode)
	; Initial = Final ->
		make_std_mode("in", [Initial], Mode)
	;
		Mode = (Initial -> Final)
	).

%-----------------------------------------------------------------------------%

	% A mode is considered an input mode if the top-level
	% node is input.

mode_is_input(InstMap, InstTable, ModuleInfo, Mode) :-
	mode_get_insts(ModuleInfo, Mode, InitialInst, _FinalInst),
	inst_is_bound(InitialInst, InstMap, InstTable, ModuleInfo).

	% A mode is considered fully input if its initial inst is ground.

mode_is_fully_input(InstMap, InstTable, ModuleInfo, Mode) :-
	mode_get_insts(ModuleInfo, Mode, InitialInst, _FinalInst),
	inst_is_ground(InitialInst, InstMap, InstTable, ModuleInfo).

	% A mode is considered an output mode if the top-level
	% node is output.

mode_is_output(InstMap, InstTable, ModuleInfo, Mode) :-
	mode_get_insts(ModuleInfo, Mode, InitialInst, FinalInst),
	inst_is_free(InitialInst, InstMap, InstTable, ModuleInfo),
	inst_is_bound(FinalInst, InstMap, InstTable, ModuleInfo).

	% A mode is considered fully output if its initial inst is free
	% and its final insts is ground.

mode_is_fully_output(InstMap, InstTable, ModuleInfo, Mode) :-
	mode_get_insts(ModuleInfo, Mode, InitialInst, FinalInst),
	inst_is_free(InitialInst, InstMap, InstTable, ModuleInfo),
	inst_is_ground(FinalInst, InstMap, InstTable, ModuleInfo).

	% A mode is considered a unused mode if it is equivalent
	% to free->free.

mode_is_unused(InstMap, InstTable, ModuleInfo, Mode) :-
	mode_get_insts(ModuleInfo, Mode, InitialInst, FinalInst),
	inst_is_free(InitialInst, InstMap, InstTable, ModuleInfo),
	inst_is_free(FinalInst, InstMap, InstTable, ModuleInfo).

%-----------------------------------------------------------------------------%

mode_to_arg_mode(InstMap, InstTable, ModuleInfo, Mode, Type, ArgMode) :-
	mode_get_insts(ModuleInfo, Mode, InitialInst, FinalInst),
	insts_to_arg_mode(InstTable, ModuleInfo, InitialInst, InstMap,
		FinalInst, InstMap, Type, ArgMode).

insts_to_arg_mode(InstTable, ModuleInfo, InstA, InstMapA, InstB, InstMapB,
		Type, ArgMode) :-
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
		module_info_globals(ModuleInfo, Globals),
		type_is_no_tag_type(Constructors, Globals, FunctorName, Type)
	->
		% the arg_mode will be determined by the mode and
		% type of the functor's argument,
		% so we figure out the mode and type of the argument,
		% and then recurse
		ConsId = cons(FunctorName, 1),
		get_single_arg_inst(InstA, InstMapA, InstTable,
			ModuleInfo, ConsId, ArgInstA),
		get_single_arg_inst(InstB, InstMapB, InstTable,
			ModuleInfo, ConsId, ArgInstB),
		insts_to_arg_mode(InstTable, ModuleInfo, ArgInstA, InstMapA,
			ArgInstB, InstMapB, Type, ArgMode)
	;
		insts_to_arg_mode_2(InstTable, ModuleInfo, InstA, InstMapA,
			InstB, InstMapB, ArgMode)
	).

:- pred insts_to_arg_mode_2(inst_table, module_info, inst, instmap,
		inst, instmap, arg_mode).
:- mode insts_to_arg_mode_2(in, in, in, in, in, in, out) is det.

insts_to_arg_mode_2(InstTable, ModuleInfo, InstA, InstMapA, InstB, InstMapB,
		ArgMode) :-
	( inst_is_bound(InstA, InstMapA, InstTable, ModuleInfo) ->
		ArgMode = top_in
	; inst_is_bound(InstB, InstMapB, InstTable, ModuleInfo) ->
		(
			inst_is_free_alias(InstA, InstMapA, InstTable,
					ModuleInfo)
		->
			ArgMode = ref_in
		;
			ArgMode = top_out
		)
	;
		( 
			inst_is_free_alias(InstB, InstMapB, InstTable,
					ModuleInfo)
		->
			( inst_is_free_alias(InstA, InstMapA, InstTable,
					ModuleInfo)
			->
				ArgMode = top_unused
				% XXX this may eventually require a new arg_mode
				% `ref_unused', but I can't see a reason for
				% it at the moment.
			;
				ArgMode = ref_out
			)
		;
			ArgMode = top_unused
		)
	).

%-----------------------------------------------------------------------------%

	% Given two lists of modes (inst mappings) of equal length,
	% convert them into a single list of inst pair mappings.

mode_util__modes_to_uni_modes([], [], _, []).
mode_util__modes_to_uni_modes([], [_|_], _, _) :-
	error("mode_util__modes_to_uni_modes: length mismatch").
mode_util__modes_to_uni_modes([_|_], [], _, _) :-
	error("mode_util__modes_to_uni_modes: length mismatch").
mode_util__modes_to_uni_modes([ModeOfX|Xs], [ModeOfY|Ys], ModuleInfo, [A|As]) :-
	mode_get_insts(ModuleInfo, ModeOfX, IX, FX),
	mode_get_insts(ModuleInfo, ModeOfY, IY, FY),
	A = ((IX - IY) -> (FX - FY)),
	mode_util__modes_to_uni_modes(Xs, Ys, ModuleInfo, As).

	% Given two lists of inst pairs (inst mappings) of equal length,
	% convert them into a single list of inst pair mappings.

mode_util__inst_pairs_to_uni_modes([], [], []).
mode_util__inst_pairs_to_uni_modes([], [_|_], _) :-
	error("mode_util__inst_pairs_to_uni_modes: length mismatch").
mode_util__inst_pairs_to_uni_modes([_|_], [], _) :-
	error("mode_util__inst_pairs_to_uni_modes: length mismatch").
mode_util__inst_pairs_to_uni_modes([IX - FX|Xs], [IY - FY|Ys], [A|As]) :-
	A = ((IX - IY) -> (FX - FY)),
	mode_util__inst_pairs_to_uni_modes(Xs, Ys, As).

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
get_arg_insts(free(A), _ConsId, Arity, ArgInsts) :-
	list__duplicate(Arity, free(A), ArgInsts).
get_arg_insts(free(A, _Type), _ConsId, Arity, ArgInsts) :-
	list__duplicate(Arity, free(A), ArgInsts).
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

get_mode_of_args(not_reached, ArgInsts, ArgModes) :-
        mode_set_args(ArgInsts, not_reached, ArgModes).
get_mode_of_args(any(Uniq), ArgInsts, ArgModes) :-
        mode_set_args(ArgInsts, any(Uniq), ArgModes).
get_mode_of_args(ground(Uniq, no), ArgInsts, ArgModes) :-
        mode_set_args(ArgInsts, ground(Uniq, no), ArgModes).
get_mode_of_args(bound(_Uniq, List), ArgInstsA, ArgModes) :-
        ( List = [] ->
                % the code is unreachable
                mode_set_args(ArgInstsA, not_reached, ArgModes)
        ;
                List = [functor(_Name, ArgInstsB)],
                assoc_list__from_corresponding_lists(ArgInstsA, ArgInstsB,
				ArgModes)
        ).
get_mode_of_args(alias(Key), ArgInsts, ArgModes) :-
	mode_set_args(ArgInsts, alias(Key), ArgModes).
	
:- pred mode_set_args(list(inst), inst, assoc_list(inst, inst)).
:- mode mode_set_args(in, in, out) is det.

mode_set_args([], _, []).
mode_set_args([Inst | Insts], FinalInst, [Mode | Modes]) :-
        Mode = (Inst - FinalInst),
        mode_set_args(Insts, FinalInst, Modes).

%-----------------------------------------------------------------------------%

inst_lookup(InstTable, ModuleInfo, InstName, Inst) :-
	inst_lookup_2(InstName, InstTable, ModuleInfo, Inst).

:- pred inst_lookup_2(inst_name, inst_table, module_info, inst).
:- mode inst_lookup_2(in, in, in, out) is det.

inst_lookup_2(InstName, InstTable, ModuleInfo, Inst) :-
	( InstName = unify_inst(_, _, _, _),
		inst_table_get_unify_insts(InstTable, UnifyInstTable),
		map__lookup(UnifyInstTable, InstName, MaybeInst),
		( MaybeInst = known(Inst0, _) ->
			Inst = Inst0
		;
			Inst = defined_inst(InstName)
		)
	; InstName = merge_inst(IsLive, A, B),
		inst_table_get_merge_insts(InstTable, MergeInstTable),
		map__lookup(MergeInstTable, merge_inst_pair(IsLive, A, B),
			MaybeInst),
		( MaybeInst = known(Inst0) ->
			Inst = Inst0
		;
			Inst = defined_inst(InstName)
		)
	; InstName = ground_inst(_, _, _, _),
		inst_table_get_ground_insts(InstTable, GroundInstTable),
		map__lookup(GroundInstTable, InstName, MaybeInst),
		( MaybeInst = known(Inst0, _) ->
			Inst = Inst0
		;
			Inst = defined_inst(InstName)
		)
	; InstName = any_inst(_, _, _, _),
		inst_table_get_any_insts(InstTable, AnyInstTable),
		map__lookup(AnyInstTable, InstName, MaybeInst),
		( MaybeInst = known(Inst0, _) ->
			Inst = Inst0
		;
			Inst = defined_inst(InstName)
		)
	; InstName = shared_inst(SharedInstName),
		inst_table_get_shared_insts(InstTable, SharedInstTable),
		map__lookup(SharedInstTable, SharedInstName, MaybeInst),
		( MaybeInst = known(Inst0) ->
			Inst = Inst0
		;
			Inst = defined_inst(InstName)
		)
	; InstName = mostly_uniq_inst(NondetLiveInstName),
		inst_table_get_mostly_uniq_insts(InstTable,
			NondetLiveInstTable),
		map__lookup(NondetLiveInstTable, NondetLiveInstName, MaybeInst),
		( MaybeInst = known(Inst0) ->
			Inst = Inst0
		;
			Inst = defined_inst(InstName)
		)
	; InstName = user_inst(Name, Args),
		module_info_user_insts(ModuleInfo, UserInstTable),
		user_inst_table_get_inst_defns(UserInstTable, InstDefns),
		list__length(Args, Arity),
		( map__search(InstDefns, Name - Arity, InstDefn) ->
			InstDefn = hlds_inst_defn(_VarSet, Params, Inst0,
					_Cond, _C, _),
			inst_lookup_subst_args(Inst0, Params, Name, InstTable,
				Args, Inst)
		;
			Inst = abstract_inst(Name, Args)
		)
	; InstName = typed_ground(Uniq, Type),
		map__init(Subst),
		propagate_type_into_inst(Type, Subst, InstTable, ModuleInfo,
			ground(Uniq, no), Inst)
	; InstName = typed_inst(Type, TypedInstName),
		inst_lookup_2(TypedInstName, InstTable, ModuleInfo, Inst0),
		map__init(Subst),
		propagate_type_into_inst(Type, Subst, InstTable, ModuleInfo,
			Inst0, Inst)
	; InstName = other_inst(OtherInstId, OtherInstName),
		inst_table_get_other_insts(InstTable, OtherInsts),
		other_inst_table_lookup(OtherInsts, OtherInstId, OtherInstName,
			MaybeInst),
		( MaybeInst = known(Inst0) ->
			Inst = Inst0
		;
			Inst = defined_inst(InstName)
		)
	),
	!.

%-----------------------------------------------------------------------------%

:- pred inst_list_has_no_duplicate_inst_keys(set(inst_key), set(inst_key),
		list(inst), inst_table, module_info).
:- mode inst_list_has_no_duplicate_inst_keys(in, out, in, in, in) is semidet.

inst_list_has_no_duplicate_inst_keys(Set, Set, [], _InstTable, _ModuleInfo).
inst_list_has_no_duplicate_inst_keys(Set0, Set, [Inst | Insts],
		InstTable, ModuleInfo) :-
	inst_has_no_duplicate_inst_keys(Set0, Set1, Inst, InstTable, ModuleInfo),
	inst_list_has_no_duplicate_inst_keys(Set1, Set, Insts, InstTable, ModuleInfo).

:- pred inst_has_no_duplicate_inst_keys(set(inst_key), set(inst_key),
		inst, inst_table, module_info).
:- mode inst_has_no_duplicate_inst_keys(in, out, in, in, in) is semidet.

inst_has_no_duplicate_inst_keys(Set, Set, any(_), _InstTable, _ModuleInfo).
inst_has_no_duplicate_inst_keys(Set0, Set, alias(Key), InstTable, ModuleInfo) :-
	\+ set__member(Key, Set0),
	set__insert(Set0, Key, Set1),
	inst_table_get_inst_key_table(InstTable, IKT),
	inst_key_table_lookup(IKT, Key, Inst),
	inst_has_no_duplicate_inst_keys(Set1, Set, Inst, InstTable, ModuleInfo).
inst_has_no_duplicate_inst_keys(Set, Set, free(_, _), _InstTable, _ModuleInfo).
inst_has_no_duplicate_inst_keys(Set, Set, free(_), _InstTable, _ModuleInfo).
inst_has_no_duplicate_inst_keys(Set0, Set, bound(_, BoundInsts), InstTable,
		ModuleInfo) :-
	bound_insts_list_has_no_duplicate_inst_keys(Set0, Set, BoundInsts,
		InstTable, ModuleInfo).
inst_has_no_duplicate_inst_keys(Set, Set, ground(_, _), _InstTable, _ModuleInfo).
inst_has_no_duplicate_inst_keys(Set, Set, not_reached, _InstTable, _ModuleInfo) :-
	error("inst_has_no_duplicate_inst_keys: not_reached").
inst_has_no_duplicate_inst_keys(Set, Set, inst_var(_), _InstTable, _ModuleInfo).
inst_has_no_duplicate_inst_keys(Set, Set, defined_inst(_), _InstTable, _ModuleInfo).
inst_has_no_duplicate_inst_keys(Set0, Set, abstract_inst(_, Insts),
		InstTable, ModuleInfo) :-
	inst_list_has_no_duplicate_inst_keys(Set0, Set, Insts, InstTable, ModuleInfo).

:- pred bound_insts_list_has_no_duplicate_inst_keys(set(inst_key),
		set(inst_key), list(bound_inst), inst_table, module_info).
:- mode bound_insts_list_has_no_duplicate_inst_keys(in, out, in, in, in)
		is semidet.

bound_insts_list_has_no_duplicate_inst_keys(Set, Set, [], _InstTable, _ModuleInfo).
bound_insts_list_has_no_duplicate_inst_keys(Set0, Set,
		[functor(_, Insts) | BoundInsts], InstTable, ModuleInfo) :-
	inst_list_has_no_duplicate_inst_keys(Set0, Set1, Insts, InstTable,
		ModuleInfo),
	bound_insts_list_has_no_duplicate_inst_keys(Set1, Set, BoundInsts,
		InstTable, ModuleInfo).

%-----------------------------------------------------------------------------%

	% Given corresponding lists of types and modes, produce a new
	% list of modes which includes the information provided by the
	% corresponding types.

propagate_types_into_mode_list(Types, InstTable, ModuleInfo, Modes0, Modes) :-
	mode_list_get_initial_insts(Modes0, ModuleInfo, Initials0),
	mode_list_get_final_insts(Modes0, ModuleInfo, Finals0),
	(
		set__init(InitDups0),
		inst_list_has_no_duplicate_inst_keys(InitDups0, _, Initials0,
			InstTable, ModuleInfo),
		set__init(FinalDups0),
		inst_list_has_no_duplicate_inst_keys(FinalDups0, _, Finals0,
			InstTable, ModuleInfo)
	->
		propagate_types_into_mode_list_2(Types, InstTable, ModuleInfo,
			Modes0, Modes)
	;
		error("propagate_types_into_mode_list: Duplicate inst_keys NYI")
	).

:- pred propagate_types_into_mode_list_2(list(type), inst_table,
		module_info, list(mode), list(mode)).
:- mode propagate_types_into_mode_list_2(in, in, in, in, out) is det.

propagate_types_into_mode_list_2([], _, _, [], []).
propagate_types_into_mode_list_2([Type | Types], InstTable, ModuleInfo,
		[Mode0 | Modes0], [Mode | Modes]) :-
	propagate_type_into_mode(Type, InstTable, ModuleInfo, Mode0, Mode),
	propagate_types_into_mode_list_2(Types, InstTable, ModuleInfo, Modes0, Modes).
propagate_types_into_mode_list_2([], _, _, [_|_], []) :-
	error("propagate_types_into_mode_list: length mismatch").
propagate_types_into_mode_list_2([_|_], _, _, [], []) :-
	error("propagate_types_into_mode_list: length mismatch").

propagate_types_into_inst_list(Types, Subst, InstTable, ModuleInfo, Insts0, Insts) :-
	(
		set__init(Dups0),
		inst_list_has_no_duplicate_inst_keys(Dups0, _, Insts0,
			InstTable, ModuleInfo)
	->
		propagate_types_into_inst_list_2(Types, Subst, InstTable, ModuleInfo,
			Insts0, Insts)
	;
		error("propagate_types_into_inst_list: Duplicate inst_keys NYI")
	).

:- pred propagate_types_into_inst_list_2(list(type), tsubst, inst_table,
			module_info, list(inst), list(inst)).
:- mode propagate_types_into_inst_list_2(in, in, in, in, in, out) is det.

propagate_types_into_inst_list_2([], _, _, _, [], []).
propagate_types_into_inst_list_2([Type | Types], Subst, InstTable, ModuleInfo,
		[Inst0 | Insts0], [Inst | Insts]) :-
	propagate_type_into_inst(Type, Subst, InstTable, ModuleInfo, Inst0, Inst),
	propagate_types_into_inst_list_2(Types, Subst, InstTable, ModuleInfo,
		Insts0, Insts).
propagate_types_into_inst_list_2([], _, _, _, [_|_], []) :-
	error("propagate_types_into_inst_list: length mismatch").
propagate_types_into_inst_list_2([_|_], _, _, _, [], []) :-
	error("propagate_types_into_inst_list: length mismatch").

	% Given a type and a mode, produce a new mode which includes
	% the information provided by the type.

:- pred propagate_type_into_mode(type, inst_table, module_info, mode, mode).
:- mode propagate_type_into_mode(in, in, in, in, out) is det.

propagate_type_into_mode(Type, InstTable, ModuleInfo, Mode0, Mode) :-
	mode_get_insts(ModuleInfo, Mode0, InitialInst0, FinalInst0),
	map__init(Subst),
	propagate_type_into_inst_lazily(Type, Subst, InstTable, ModuleInfo,
		InitialInst0, InitialInst),
	propagate_type_into_inst_lazily(Type, Subst, InstTable, ModuleInfo,
			FinalInst0, FinalInst),
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

:- pred propagate_type_into_inst(type, tsubst, inst_table, module_info,
		inst, inst).
:- mode propagate_type_into_inst(in, in, in, in, in, out) is det.

:- pred propagate_type_into_inst_lazily(type, tsubst, inst_table,
		module_info, inst, inst).
:- mode propagate_type_into_inst_lazily(in, in, in, in, in, out) is det.

/*********
	% XXX We ought to expand things eagerly here, using the commented
	% out code below.  However, that causes efficiency problems,
	% so for the moment it is disabled.
propagate_type_into_inst(Type, Subst, InstTable, ModuleInfo, Inst0, Inst) :-
	apply_type_subst(Type0, Subst, Type),
	(
		type_constructors(Type, ModuleInfo, Constructors)
	->
		propagate_ctor_info(Inst0, Type, Constructors, InstTable0,
			ModuleInfo, InstTable, Inst) 
	;
		Inst = Inst0
	).
*********/

propagate_type_into_inst(Type, Subst, InstTable, ModuleInfo, Inst0, Inst) :-
	propagate_ctor_info_lazily(Inst0, Type, Subst, InstTable, ModuleInfo,
		Inst).

propagate_type_into_inst_lazily(Type, Subst, InstTable, ModuleInfo, Inst0,
		Inst) :-
	propagate_ctor_info_lazily(Inst0, Type, Subst, InstTable, ModuleInfo,
		Inst).

%-----------------------------------------------------------------------------%

:- pred propagate_ctor_info(inst, type, list(constructor), inst_table,
		module_info, inst).
:- mode propagate_ctor_info(in, in, in, in, in, out) is det.

propagate_ctor_info(any(Uniq), _Type, _, _, _, any(Uniq)).
			% XXX loses type info!
propagate_ctor_info(alias(Key), Type, Constructors, InstTable, ModuleInfo,
			Inst) :-
	inst_table_get_inst_key_table(InstTable, IKT),
	inst_key_table_lookup(IKT, Key, Inst0),
	propagate_ctor_info(Inst0, Type, Constructors, InstTable, ModuleInfo,
			Inst).

% propagate_ctor_info(free, Type, _, _, _, free(Type)).
							% temporarily disabled
propagate_ctor_info(free(A), _Type, _, _, _, free(A)).
							% XXX temporary hack

propagate_ctor_info(free(_, _), _, _, _, _, _) :-
	error("propagate_ctor_info: type info already present").
propagate_ctor_info(bound(Uniq, BoundInsts0), Type, _Constructors, InstTable,
		ModuleInfo, Inst) :-
	propagate_ctor_info_2(BoundInsts0, Type, InstTable, ModuleInfo,
		BoundInsts),
	( BoundInsts = [] ->
		Inst = not_reached
	;
		% XXX do we need to sort the BoundInsts?
		Inst = bound(Uniq, BoundInsts)
	).
propagate_ctor_info(ground(Uniq, no), Type, Constructors, _, ModuleInfo,
		Inst) :-
	( type_is_higher_order(Type, function, _, ArgTypes) ->
		default_higher_order_func_inst(ArgTypes, ModuleInfo,
			HigherOrderInstInfo),
		Inst = ground(Uniq, yes(HigherOrderInstInfo))
	;
		constructors_to_bound_insts(Constructors, Uniq, ModuleInfo,
			BoundInsts0),
		list__sort_and_remove_dups(BoundInsts0, BoundInsts),
		Inst = bound(Uniq, BoundInsts)
	).
propagate_ctor_info(ground(Uniq, yes(PredInstInfo0)), Type, _Ctors, _InstTable,
		ModuleInfo, ground(Uniq, yes(PredInstInfo))) :-
	PredInstInfo0 = pred_inst_info(PredOrFunc,
		argument_modes(ArgInstTable, ArgModes0), Det),
	PredInstInfo = pred_inst_info(PredOrFunc,
		argument_modes(ArgInstTable, ArgModes), Det),
	(
		type_is_higher_order(Type, PredOrFunc, _, ArgTypes),
		list__same_length(ArgTypes, ArgModes0)
	->
		propagate_types_into_mode_list(ArgTypes, ArgInstTable,
			ModuleInfo, ArgModes0, ArgModes)
	;
		% The inst is not a valid inst for the type,
		% so leave it alone. This can only happen if the user
		% has made a mistake.  A mode error should hopefully
		% be reported if anything tries to match with the inst.
		ArgModes = ArgModes0
	).

propagate_ctor_info(not_reached, _Type, _Constructors, _, _ModuleInfo,
		not_reached).
propagate_ctor_info(inst_var(V), _, _, _, _, inst_var(V)).
propagate_ctor_info(abstract_inst(Name, Args), _, _, _, _,
		abstract_inst(Name, Args)).	% XXX loses info
propagate_ctor_info(defined_inst(InstName), Type, Ctors, InstTable, ModuleInfo,
		Inst) :-
	inst_lookup(InstTable, ModuleInfo, InstName, Inst0),
	propagate_ctor_info(Inst0, Type, Ctors, InstTable, ModuleInfo, Inst).

:- pred propagate_ctor_info_lazily(inst, type, tsubst, inst_table,
		module_info, inst).
:- mode propagate_ctor_info_lazily(in, in, in, in, in, out) is det.

propagate_ctor_info_lazily(alias(Key), Type, Constructors, InstTable,
		ModuleInfo, Inst) :-
	inst_table_get_inst_key_table(InstTable, IKT),
	inst_key_table_lookup(IKT, Key, Inst0),
	propagate_ctor_info_lazily(Inst0, Type, Constructors, InstTable,
			ModuleInfo, Inst).

propagate_ctor_info_lazily(any(Uniq), _Type, _, _, _, any(Uniq)).
						% XXX loses type info!

% propagate_ctor_info_lazily(free, Type, _, _, _, free(Type)).
							% temporarily disabled
propagate_ctor_info_lazily(free(A), _Type, _, _, _, free(A)).
							% XXX temporary hack

propagate_ctor_info_lazily(free(_, _), _, _, _, _, _) :-
	error("propagate_ctor_info_lazily: type info already present").
propagate_ctor_info_lazily(bound(Uniq, BoundInsts0), Type0, Subst, 
		InstTable, ModuleInfo, Inst) :-
	apply_type_subst(Type0, Subst, Type),
	propagate_ctor_info_2(BoundInsts0, Type, InstTable, ModuleInfo,
		BoundInsts),
	( BoundInsts = [] ->
		Inst = not_reached
	;
		% XXX do we need to sort the BoundInsts?
		Inst = bound(Uniq, BoundInsts)
	).
propagate_ctor_info_lazily(ground(Uniq, no), Type0, Subst, _InstTable,
		ModuleInfo, Inst) :-
	apply_type_subst(Type0, Subst, Type),
	( type_is_higher_order(Type, function, _, ArgTypes) ->
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
		_InstTable, ModuleInfo, ground(Uniq, yes(PredInstInfo))) :-
	PredInstInfo0 = pred_inst_info(PredOrFunc,
		argument_modes(ArgInstTable, ArgModes0), Det),
	PredInstInfo = pred_inst_info(PredOrFunc,
		argument_modes(ArgInstTable, ArgModes), Det),
	apply_type_subst(Type0, Subst, Type),
	(
		type_is_higher_order(Type, PredOrFunc, _, ArgTypes),
		list__same_length(ArgTypes, ArgModes0)
	->
		propagate_types_into_mode_list(ArgTypes, ArgInstTable,
			ModuleInfo, ArgModes0, ArgModes)
	;
		% The inst is not a valid inst for the type,
		% so leave it alone. This can only happen if the user
		% has made a mistake.  A mode error should hopefully
		% be reported if anything tries to match with the inst.
		ArgModes = ArgModes0
	).
propagate_ctor_info_lazily(not_reached, _Type, _, _, _M, not_reached).
propagate_ctor_info_lazily(inst_var(Var), _, _, _, _, inst_var(Var)).
propagate_ctor_info_lazily(abstract_inst(Name, Args), _, _, _, _,
		abstract_inst(Name, Args)).	% XXX loses info
propagate_ctor_info_lazily(defined_inst(InstName0), Type0, Subst, _, _,
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
	Out = (free(unique) -> ground(shared, no)),
	list__length(PredArgTypes, NumPredArgs),
	NumFuncArgs is NumPredArgs - 1,
	list__duplicate(NumFuncArgs, In, FuncArgModes),
	FuncRetMode = Out,
	list__append(FuncArgModes, [FuncRetMode], PredArgModes0),
	inst_table_init(InstTable),
	propagate_types_into_mode_list(PredArgTypes, InstTable, ModuleInfo,
		PredArgModes0, PredArgModes),
	PredInstInfo = pred_inst_info(function,
		argument_modes(InstTable, PredArgModes), det).

:- pred constructors_to_bound_insts(list(constructor), uniqueness, module_info,
				list(bound_inst)).
:- mode constructors_to_bound_insts(in, in, in, out) is det.

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
	Inst = ground(Uniq, no),
	ctor_arg_list_to_inst_list(Args, Uniq, Insts).

:- pred propagate_ctor_info_2(list(bound_inst), (type), inst_table,
		module_info, list(bound_inst)).
:- mode propagate_ctor_info_2(in, in, in, in, out) is det.

propagate_ctor_info_2(BoundInsts0, Type, InstTable, ModuleInfo, BoundInsts) :-
	(
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
			ArgSubst, InstTable, ModuleInfo, BoundInsts1),
		list__sort(BoundInsts1, BoundInsts)
	;
		% Builtin types don't need processing.
		BoundInsts = BoundInsts0
	).

:- pred propagate_ctor_info_3(list(bound_inst), sym_name, list(constructor),
		tsubst, inst_table, module_info, list(bound_inst)).
:- mode propagate_ctor_info_3(in, in, in, in, in, in, out) is det.

propagate_ctor_info_3([], _, _, _, _, _, []).
propagate_ctor_info_3([BoundInst0 | BoundInsts0], TypeModule, Constructors,
		Subst, InstTable, ModuleInfo, [BoundInst | BoundInsts]) :-
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
			InstTable, ModuleInfo, ArgInsts0, ArgInsts),
		BoundInst = functor(ConsId, ArgInsts)
	;
		% The cons_id is not a valid constructor for the type,
		% so leave it alone. This can only happen in a user defined
		% bound_inst. A mode error should be reported if anything
		% tries to match with the inst.
		BoundInst = functor(ConsId, ArgInsts0)
	),
	propagate_ctor_info_3(BoundInsts0, TypeModule,
		Constructors, Subst, InstTable, ModuleInfo, BoundInsts).

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
		inst_table, list(inst), inst).
:- mode inst_lookup_subst_args(in, in, in, in, in, out) is det.

inst_lookup_subst_args(eqv_inst(Inst0), Params, _Name, _InstTable,
			Args, Inst) :-
	inst_substitute_arg_list(Inst0, Params, Args, Inst).
inst_lookup_subst_args(abstract_inst, _Params, Name, _InstTable, Args,
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
inst_apply_substitution(alias(Var), _, alias(Var)) :-
	error("inst_apply_substitution: alias").
inst_apply_substitution(free(A), _, free(A)).
inst_apply_substitution(free(A, T), _, free(A, T)).
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
inst_name_apply_substitution(merge_inst(IsLive, InstA0, InstB0), Subst,
		merge_inst(IsLive, InstA, InstB)) :-
	inst_apply_substitution(InstA0, Subst, InstA),
	inst_apply_substitution(InstB0, Subst, InstB).
inst_name_apply_substitution(ground_inst(Inst0, IsLive, Uniq, Real), Subst,
				ground_inst(Inst, IsLive, Uniq, Real)) :-
	inst_name_apply_substitution(Inst0, Subst, Inst).
inst_name_apply_substitution(any_inst(Inst0, IsLive, Uniq, Real), Subst,
				any_inst(Inst, IsLive, Uniq, Real)) :-
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
inst_name_apply_substitution(other_inst(Id, InstName0), Subst,
		other_inst(Id, InstName)) :-
	inst_name_apply_substitution(InstName0, Subst, InstName).

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
	% XXX This will not work properly if the pred has aliasing in
	%     its argument_modes.
	Modes0 = argument_modes(ArgInstTable, ArgModes0),
	Modes  = argument_modes(ArgInstTable, ArgModes),
	mode_list_apply_substitution(ArgModes0, Subst, ArgModes).

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

:- type recompute_info --->
		recompute_info(
			map(prog_var, type),
			module_info,
			inst_table,
			bag(prog_var),
			bool
		).

:- pred slap_recompute_info(recompute_info, recompute_info, recompute_info).
:- mode slap_recompute_info(in, in, out) is det.

slap_recompute_info(RI, _, RI).

:- pred recompute_info_get_vartypes(recompute_info, map(prog_var, type)).
:- mode recompute_info_get_vartypes(in, out) is det.

recompute_info_get_vartypes(recompute_info(VarTypes, _, _, _, _), VarTypes).

:- pred recompute_info_get_module_info(recompute_info, module_info).
:- mode recompute_info_get_module_info(in, out) is det.

recompute_info_get_module_info(recompute_info(_, ModuleInfo, _, _, _),
		ModuleInfo).

:- pred recompute_info_set_module_info(module_info,
		recompute_info, recompute_info).
:- mode recompute_info_set_module_info(in, in, out) is det.

recompute_info_set_module_info(ModuleInfo, recompute_info(A, _, C, D, E),
		recompute_info(A, ModuleInfo, C, D, E)).

:- pred recompute_info_get_inst_table(recompute_info, inst_table).
:- mode recompute_info_get_inst_table(in, out) is det.

recompute_info_get_inst_table(recompute_info(_, _, InstTable, _, _), InstTable).

:- pred recompute_info_set_inst_table(inst_table,
		recompute_info, recompute_info).
:- mode recompute_info_set_inst_table(in, in, out) is det.

recompute_info_set_inst_table(InstTable, recompute_info(A, B, _, D, E),
		recompute_info(A, B, InstTable, D, E)).

:- pred recompute_info_set_live_vars(bag(prog_var), recompute_info,
		recompute_info).
:- mode recompute_info_set_live_vars(in, in, out) is det.

recompute_info_set_live_vars(LiveVars, recompute_info(A, B, C, _, E),
		recompute_info(A, B, C, LiveVars, E)).

:- pred recompute_info_get_live_vars(recompute_info, bag(prog_var)).
:- mode recompute_info_get_live_vars(in, out) is det.

recompute_info_get_live_vars(recompute_info(_, _, _, LiveVars, _), LiveVars).

:- pred recompute_info_set_goal_changed(recompute_info, recompute_info).
:- mode recompute_info_set_goal_changed(in, out) is det.

recompute_info_set_goal_changed(recompute_info(A, B, C, D, _),
		recompute_info(A, B, C, D, yes)).


:- pred recompute_info_get_live_vars_list(recompute_info, list(prog_var)).
:- mode recompute_info_get_live_vars_list(in, out) is det.

recompute_info_get_live_vars_list(RI, LiveVarsList) :-
	recompute_info_get_live_vars(RI, LiveVarsBag),
	bag__to_list_without_duplicates(LiveVarsBag, LiveVarsList).

:- pred recompute_info_add_live_vars(set(prog_var), recompute_info,
		recompute_info).
:- mode recompute_info_add_live_vars(in, in, out) is det.

recompute_info_add_live_vars(VarsSet, RI0, RI) :-
	set__to_sorted_list(VarsSet, VarsList),
	recompute_info_add_live_vars_list(VarsList, RI0, RI).

:- pred recompute_info_add_live_vars_list(list(prog_var),
				recompute_info, recompute_info).
:- mode recompute_info_add_live_vars_list(in, in, out) is det.

recompute_info_add_live_vars_list(Vars, RI0, RI) :-
	recompute_info_get_live_vars(RI0, LiveVars0),
	bag__insert_list(LiveVars0, Vars, LiveVars),
	recompute_info_set_live_vars(LiveVars, RI0, RI).

:- pred recompute_info_remove_live_vars(set(prog_var),
				recompute_info, recompute_info).
:- mode recompute_info_remove_live_vars(in, in, out) is det.

recompute_info_remove_live_vars(VarsSet, RI0, RI) :-
	set__to_sorted_list(VarsSet, VarsList),
	recompute_info_remove_live_vars_list(VarsList, RI0, RI).

:- pred recompute_info_remove_live_vars_list(list(prog_var),
				recompute_info, recompute_info).
:- mode recompute_info_remove_live_vars_list(in, in, out) is det.

recompute_info_remove_live_vars_list(Vars, RI0, RI) :-
	recompute_info_get_live_vars(RI0, LiveVars0),
	bag__det_remove_list(LiveVars0, Vars, LiveVars),
	recompute_info_set_live_vars(LiveVars, RI0, RI).

:- pred recompute_info_var_is_live(recompute_info, prog_var, is_live).
:- mode recompute_info_var_is_live(in, in, out) is det.

recompute_info_var_is_live(RI, Var, IsLive) :-
	recompute_info_get_live_vars(RI, LiveVars),
	( bag__contains(LiveVars, Var) ->
		IsLive = live
	;
		IsLive = dead
	).

%-----------------------------------------------------------------------------%

recompute_instmap_delta_proc(ProcInfo0, ProcInfo, ModuleInfo0, ModuleInfo) :-
	proc_info_headvars(ProcInfo0, HeadVars),
	proc_info_arglives(ProcInfo0, ModuleInfo0, ArgLives),
	proc_info_vartypes(ProcInfo0, VarTypes),
	proc_info_goal(ProcInfo0, Goal0),
	proc_info_inst_table(ProcInfo0, InstTable0),
	proc_info_get_initial_instmap(ProcInfo0, ModuleInfo0, InstMap0),
	recompute_instmap_delta(HeadVars, ArgLives, VarTypes, Goal0, Goal,
		InstMap0, InstTable0, InstTable, _GoalChanged,
		ModuleInfo0, ModuleInfo),
	proc_info_set_goal(ProcInfo0, Goal, ProcInfo1),
	proc_info_set_inst_table(ProcInfo1, InstTable, ProcInfo).

recompute_instmap_delta(ArgVars, ArgLives, VarTypes, Goal0, Goal, Instmap,
		InstTable0, InstTable, GoalChanged, M0, M) :-
	get_live_vars(ArgVars, ArgLives, LiveVars),
	bag__from_list(LiveVars, LiveVarsBag),
	RI0 = recompute_info(VarTypes, M0, InstTable0, LiveVarsBag, no),
	recompute_instmap_delta_2(Goal0, Goal, Instmap, RI0, RI),
	RI  = recompute_info(_, M, InstTable, _, GoalChanged).

:- pred recompute_instmap_delta_2(hlds_goal, hlds_goal, instmap,
		recompute_info, recompute_info).
:- mode recompute_instmap_delta_2(in, out, in, in, out) is det.

recompute_instmap_delta_2(Goal0, Goal, InstMap0) -->
	recompute_instmap_delta_2(Goal0, Goal, InstMap0, _, _).

:- pred recompute_instmap_delta_2(hlds_goal, hlds_goal, instmap,
		instmap, instmap_delta, recompute_info, recompute_info).
:- mode recompute_instmap_delta_2(in, out, in, out, out, in, out) is det.

recompute_instmap_delta_2(Goal0 - GoalInfo0, Goal - GoalInfo,
		InstMap0, InstMap, InstMapDelta, RI0, RI) :-
	recompute_instmap_delta_3(Goal0, GoalInfo0, Goal, GoalInfo1, InstMap0,
			InstMapDelta, RI0, RI1),
	(
		( 
			% If the initial instmap is unreachable so is
			% the final instmap.
			instmap__is_unreachable(InstMap0)
		;
			% If the determinism is erroneous or failure the
			% instmap_delta should be "unreachable".
			goal_info_get_determinism(GoalInfo0, Det),
			determinism_components(Det, _, at_most_zero)
		)
	->
		instmap_delta_init_unreachable(Unreachable),
		goal_info_set_instmap_delta(GoalInfo1, Unreachable, GoalInfo),
		instmap__init_unreachable(InstMap)
	;
		goal_info_set_instmap_delta(GoalInfo1, InstMapDelta, GoalInfo),
		instmap__apply_instmap_delta(InstMap0, InstMapDelta, InstMap)
	),
	RI = RI1.

:- pred recompute_instmap_delta_3(hlds_goal_expr, hlds_goal_info,
		hlds_goal_expr, hlds_goal_info, instmap, instmap_delta,
		recompute_info, recompute_info).
:- mode recompute_instmap_delta_3(in, in, out, out, in, out, in, out) is det.

recompute_instmap_delta_3(switch(Var, Det, Cases0, SM), GoalInfo,
		switch(Var, Det, Cases, SM), GoalInfo, InstMap0,
		InstMapDelta) -->
	{ instmap__vars(InstMap0, OuterNonLocals) },
	{ goal_info_get_nonlocals(GoalInfo, InnerNonLocals) },
	{ set__union(InnerNonLocals, OuterNonLocals, NonLocals) },
	recompute_instmap_delta_cases(Var, Cases0, Cases,
		InstMap0, NonLocals, InstMap),
	{ compute_instmap_delta(InstMap0, InstMap, InstMapDelta) }.

recompute_instmap_delta_3(conj(Goals0), GoalInfo, conj(Goals), GoalInfo,
		InstMap, InstMapDelta) -->
	list__foldl(lambda([Goal :: in, RI0 :: in, RI :: out] is det,
		(
			Goal = _ - ConjGoalInfo,
			goal_info_get_nonlocals(ConjGoalInfo, NonLocals),
			recompute_info_add_live_vars(NonLocals, RI0, RI)
		)), Goals0),
	recompute_instmap_delta_conj(Goals0, Goals, InstMap, InstMapDelta).

recompute_instmap_delta_3(par_conj(Goals0, SM), GoalInfo,
		par_conj(Goals, SM), GoalInfo, InstMap0, InstMapDelta) -->
	{ instmap__vars(InstMap0, OuterNonLocals) },
	{ goal_info_get_nonlocals(GoalInfo, InnerNonLocals) },
	{ set__union(InnerNonLocals, OuterNonLocals, NonLocals) },
	{ set__to_sorted_list(NonLocals, NonLocalsList) },
	recompute_instmap_delta_par_conj(Goals0, Goals, InstMap0, [],
			InstMapNonLocalsList),
	=(RI0),
	{ recompute_info_get_module_info(RI0, M0) },
	{ recompute_info_get_inst_table(RI0, InstTable0) },
	{ instmap__unify(NonLocalsList, InstMap0, InstMapNonLocalsList,
		InstTable0, InstTable, M0, M, InstMap, Errors) },
	{ Errors = [] ->
		compute_instmap_delta(InstMap0, InstMap, InstMapDelta)
	;
		error("recompute_instmap_delta_3: Unexpected error in par_conj!")
	},
	recompute_info_set_module_info(M),
	recompute_info_set_inst_table(InstTable).

recompute_instmap_delta_3(disj(Goals0, SM), GoalInfo,
		disj(Goals, SM), GoalInfo, InstMap0, InstMapDelta) -->
	{ instmap__vars(InstMap0, OuterNonLocals) },
	{ goal_info_get_nonlocals(GoalInfo, InnerNonLocals) },
	{ set__union(InnerNonLocals, OuterNonLocals, NonLocals) },
	recompute_instmap_delta_disj(Goals0, Goals,
		InstMap0, NonLocals, InstMap),
	{ compute_instmap_delta(InstMap0, InstMap, InstMapDelta) }.

recompute_instmap_delta_3(not(Goal0), GoalInfo, not(Goal), GoalInfo,
		InstMap, InstMapDelta) -->
	{ instmap_delta_init_reachable(InstMapDelta) },
	recompute_instmap_delta_2(Goal0, Goal, InstMap).

recompute_instmap_delta_3(if_then_else(Vars, A0, B0, C0, SM), GoalInfo0,
		Goal, GoalInfo, InstMap0, InstMapDelta) -->
	=(RI0),
	{ B0 = _ - ThenInfo0 },
	{ goal_info_get_nonlocals(ThenInfo0, NonLocalsB) },
	recompute_info_add_live_vars(NonLocalsB),
	recompute_instmap_delta_2(A0, A, InstMap0, InstMap1, _InstMapDelta1),
	recompute_info_remove_live_vars(NonLocalsB),
	{ A = Cond0 - CondInfo0 },
	{ goal_info_get_determinism(CondInfo0, CondDetism0) },
	{ determinism_components(CondDetism0, CondCanFail0, CondSolns0) },
	( { CondCanFail0 = cannot_fail } ->
		{ goal_to_conj_list(A0, CondList) },
		{ goal_to_conj_list(B0, ThenList) },
		{ list__append(CondList, ThenList, List) },
		slap_recompute_info(RI0),
		recompute_instmap_delta_3(conj(List), GoalInfo0,
				Goal, GoalInfo, InstMap0, InstMapDelta)
	; { CondSolns0 = at_most_zero } ->
		{ det_negation_det(CondDetism0, MaybeNegDetism) },
		{ Cond0 = not(NegCond) ->
			Cond = NegCond
		;
			(
				MaybeNegDetism = yes(NegDetism1),
				(
					NegDetism1 = erroneous,
					instmap_delta_init_unreachable(
						NegInstMapDelta1)
				;
					NegDetism1 = det,
					instmap_delta_init_reachable(
						NegInstMapDelta1)
				)
			->
				NegDetism = NegDetism1,
				NegInstMapDelta = NegInstMapDelta1
			;
				error("recompute_instmap_delta_3: cannot get negated determinism")
			),
			goal_info_set_determinism(CondInfo0,
				NegDetism, NegCondInfo0),
			goal_info_set_instmap_delta(NegCondInfo0,
				NegInstMapDelta, NegCondInfo),
			Cond = not(A0) - NegCondInfo
		},
		{ goal_to_conj_list(C0, ElseList) },
		{ List = [Cond | ElseList] },
		slap_recompute_info(RI0),
		recompute_instmap_delta_3(conj(List), GoalInfo0,
				Goal, GoalInfo, InstMap0, InstMapDelta)
	;
		recompute_instmap_delta_2(B0, B, InstMap1, InstMap2,
				_InstMapDelta2),
		recompute_instmap_delta_2(C0, C, InstMap0, InstMap3,
				_InstMapDelta3),
		{ instmap__vars(InstMap0, OuterNonLocals) },
		{ goal_info_get_nonlocals(GoalInfo0, InnerNonLocals) },
		{ set__union(InnerNonLocals, OuterNonLocals, NonLocals) },
		=(RI4),
		{ recompute_info_get_module_info(RI4, M4) },
		{ recompute_info_get_inst_table(RI4, InstTable4) },
		{ recompute_info_get_live_vars_list(RI4, Liveness) },
		{ instmap__merge(NonLocals, Liveness, [InstMap2, InstMap3],
			InstMap0, InstMapAfter, M4, InstTable4, M, InstTable,
			Errors) },
		{ Errors = [] ->
			compute_instmap_delta(InstMap0, InstMapAfter,
				InstMapDelta)
		;
			error("recompute_instmap_delta_3: if_then_else merge error")
		},
		{ Goal = if_then_else(Vars, A, B, C, SM) },
		{ GoalInfo = GoalInfo0 },
		recompute_info_set_module_info(M),
		recompute_info_set_inst_table(InstTable)
	).

recompute_instmap_delta_3(some(Vars, CanRemove, Goal0), GoalInfo,
		some(Vars, CanRemove, Goal), GoalInfo,
		InstMap, InstMapDelta) -->
	recompute_instmap_delta_2(Goal0, Goal, InstMap, _, InstMapDelta).

recompute_instmap_delta_3(generic_call(A, Vars, Modes, D), GoalInfo,
		generic_call(A, Vars, Modes, D), GoalInfo,
		InstMap0, InstMapDelta) -->
	=(RI0),
	{ recompute_info_get_inst_table(RI0, InstTable0) },
	{ Modes = argument_modes(ArgInstTable, ArgModes0) },
	{ inst_table_create_sub(InstTable0, ArgInstTable, Sub, InstTable) },
	{ list__map(apply_inst_table_sub_mode(Sub), ArgModes0, ArgModes) },
	recompute_info_set_inst_table(InstTable),
	recompute_instmap_delta_call_2(Vars, InstMap0, ArgModes, InstMap),
	{ compute_instmap_delta(InstMap0, InstMap, InstMapDelta) }.

recompute_instmap_delta_3(call(PredId, ProcId, Args, D, E, F), GoalInfo,
		call(PredId, ProcId, Args, D, E, F), GoalInfo, InstMap,
		InstMapDelta) -->
	recompute_instmap_delta_call(PredId, ProcId,
		Args, InstMap, InstMapDelta).

recompute_instmap_delta_3(unify(Var, UnifyRhs0, UniMode0, Uni0, UniContext),
		GoalInfo0, Goal, GoalInfo, InstMap, InstMapDelta) -->
	recompute_instmap_delta_unify(Var, UnifyRhs0, UniMode0, Uni0,
		UniContext, GoalInfo0, Goal, Det, InstMap, InstMapDelta),
	{ goal_info_set_determinism(GoalInfo0, Det, GoalInfo) }.

recompute_instmap_delta_3(pragma_c_code(A, PredId, ProcId, Args, E, F, G),
		GoalInfo, pragma_c_code(A, PredId, ProcId, Args, E, F, G),
		GoalInfo, InstMap, InstMapDelta) -->
	recompute_instmap_delta_call(PredId, ProcId,
		Args, InstMap, InstMapDelta).

%-----------------------------------------------------------------------------%

:- pred recompute_instmap_delta_conj(list(hlds_goal), list(hlds_goal),
		instmap, instmap_delta, recompute_info, recompute_info).
:- mode recompute_instmap_delta_conj(in, out, in, out, in, out) is det.

recompute_instmap_delta_conj([], [], _InstMap, InstMapDelta) -->
	{ instmap_delta_init_reachable(InstMapDelta) }.
recompute_instmap_delta_conj([Goal0 | Goals0], [Goal | Goals],
		InstMap0, InstMapDelta) -->
	{ Goal0 = _ - GoalInfo0 },
	{ goal_info_get_nonlocals(GoalInfo0, NonLocals) },
	recompute_info_remove_live_vars(NonLocals),
	recompute_instmap_delta_2(Goal0, Goal, InstMap0, InstMap1,
			InstMapDelta0),
	( { instmap__is_unreachable(InstMap1) } ->
		list__foldl(lambda([ConjGoal :: in, RI0 :: in, RI :: out]
					is det,
			(
				ConjGoal = _ - ConjGoalInfo,
				goal_info_get_nonlocals(ConjGoalInfo,
					ConjNonLocals),
				recompute_info_remove_live_vars(ConjNonLocals,
					RI0, RI)
			)), Goals0),
		{ instmap_delta_init_unreachable(InstMapDelta) },
		{ Goals = [] }
	;
		recompute_instmap_delta_conj(Goals0, Goals, InstMap1,
				InstMapDelta1),
		{ instmap_delta_apply_instmap_delta(InstMapDelta0,
				InstMapDelta1, InstMapDelta) }
	).

%-----------------------------------------------------------------------------%

:- pred recompute_instmap_delta_par_conj(list(hlds_goal), list(hlds_goal),
		instmap, list(pair(instmap, set(prog_var))),
		list(pair(instmap, set(prog_var))), recompute_info,
		recompute_info).
:- mode recompute_instmap_delta_par_conj(in, out, in, in, out, in, out) is det.

recompute_instmap_delta_par_conj([], [], _, IMNonLocals, IMNonLocals) --> [].
recompute_instmap_delta_par_conj([Goal0], [Goal],
		InstMap0, IMNonLocals0, IMNonLocals) -->
	recompute_instmap_delta_2(Goal0, Goal, InstMap0, InstMap, _),
	{ instmap__vars(InstMap0, OuterNonLocals) },
	{ Goal0 = _ - GoalInfo0 },
	{ goal_info_get_nonlocals(GoalInfo0, InnerNonLocals) },
	{ set__union(InnerNonLocals, OuterNonLocals, NonLocals) },
	{ IMNonLocals = [InstMap - NonLocals | IMNonLocals0] }.
recompute_instmap_delta_par_conj([Goal0 | Goals0], [Goal | Goals],
		InstMap0, IMNonLocals0, IMNonLocals) -->
	{ Goals0 = [_|_] },
	recompute_instmap_delta_2(Goal0, Goal, InstMap0, InstMap, _),
	{ instmap__vars(InstMap0, OuterNonLocals) },
	{ Goal0 = _ - GoalInfo0 },
	{ goal_info_get_nonlocals(GoalInfo0, InnerNonLocals) },
	{ set__union(InnerNonLocals, OuterNonLocals, NonLocals) },
	{ IMNonLocals1 = [InstMap - NonLocals | IMNonLocals0] },
	recompute_instmap_delta_par_conj(Goals0, Goals, InstMap,
			IMNonLocals1, IMNonLocals).

%-----------------------------------------------------------------------------%

:- pred recompute_instmap_delta_disj(list(hlds_goal), list(hlds_goal), instmap,
		set(prog_var), instmap, recompute_info, recompute_info).
:- mode recompute_instmap_delta_disj(in, out, in, in, out, in, out) is det.

recompute_instmap_delta_disj([], [], _, _, InstMap) -->
	{ instmap__init_unreachable(InstMap) }.
recompute_instmap_delta_disj([Goal0], [Goal],
		InstMap0, _, InstMap) -->
	recompute_instmap_delta_2(Goal0, Goal, InstMap0, InstMap, _).
recompute_instmap_delta_disj([Goal0 | Goals0], [Goal | Goals],
		InstMap0, NonLocals, InstMap) -->
	{ Goals0 = [_|_] },
	recompute_instmap_delta_2(Goal0, Goal, InstMap0, InstMapThisBranch, _),
	recompute_instmap_delta_disj(Goals0, Goals,
			InstMap0, NonLocals, InstMapBranches),
	=(RI2),
	{ recompute_info_get_module_info(RI2, M2) },
	{ recompute_info_get_inst_table(RI2, InstTable2) },
	{ recompute_info_get_live_vars_list(RI2, Liveness) },
	{ instmap__merge(NonLocals, Liveness, 
		[InstMapBranches, InstMapThisBranch], InstMap0, InstMapAfter,
		M2, InstTable2, M, InstTable, Errors) },
	{ Errors = [] ->
		InstMap = InstMapAfter
	;
		error("recompute_instmap_delta_disj: merge error")
	},
	recompute_info_set_module_info(M),
	recompute_info_set_inst_table(InstTable).

%-----------------------------------------------------------------------------%

:- pred recompute_instmap_delta_cases(prog_var, list(case), list(case), instmap,
		set(prog_var), instmap, recompute_info, recompute_info).
:- mode recompute_instmap_delta_cases(in, in, out, in, in, out, in, out) is det.

recompute_instmap_delta_cases(_, [], [], _, _, InstMap) -->
	{ instmap__init_unreachable(InstMap) }.
recompute_instmap_delta_cases(Var, [Case0 | Cases0], [Case | Cases],
		InstMap0, NonLocals, InstMap) -->
	{ Case0 = case(Functor, _, Goal0) },
	=(RI0),
	{ recompute_info_get_module_info(RI0, M0) },
	{ recompute_info_get_inst_table(RI0, InstTable0) },
	{ instmap__bind_var_to_functor(Var, Functor, InstMap0, InstMap1,
		InstTable0, InstTable1, M0, M1) },
	{ compute_instmap_delta(InstMap0, InstMap1, IMDelta) },
	recompute_info_set_module_info(M1),
	recompute_info_set_inst_table(InstTable1),
	recompute_instmap_delta_2(Goal0, Goal, InstMap1, InstMapThisBranch, _),
	{ Case = case(Functor, IMDelta, Goal) },
	recompute_instmap_delta_cases(Var, Cases0, Cases,
		InstMap0, NonLocals, InstMapBranches),
	=(RI4),
	{ recompute_info_get_module_info(RI4, M4) },
	{ recompute_info_get_inst_table(RI4, InstTable4) },
	{ recompute_info_get_live_vars_list(RI4, Liveness) },
	{ instmap__merge(NonLocals, Liveness,
		[InstMapBranches, InstMapThisBranch], InstMap0, InstMapAfter,
		M4, InstTable4, M, InstTable, Errors) },
	{ Errors = [] ->
		InstMap = InstMapAfter
	;
		error("recompute_instmap_delta_disj: merge error")
	},
	recompute_info_set_module_info(M),
	recompute_info_set_inst_table(InstTable).

%-----------------------------------------------------------------------------%

:- pred recompute_instmap_delta_call(pred_id, proc_id, list(prog_var), instmap,
		instmap_delta, recompute_info, recompute_info).
:- mode recompute_instmap_delta_call(in, in, in, in, out, in, out) is det.

recompute_instmap_delta_call(PredId, ProcId, Args, InstMap0,
		InstMapDelta, RI0, RI) :-
	recompute_info_get_module_info(RI0, ModuleInfo0),
	recompute_info_get_inst_table(RI0, InstTable0),
	module_info_pred_proc_info(ModuleInfo0, PredId, ProcId, _, ProcInfo),
	proc_info_interface_determinism(ProcInfo, Detism),
	( determinism_components(Detism, _, at_most_zero) ->
		instmap_delta_init_unreachable(InstMapDelta),
		RI = RI0
	;
		proc_info_argmodes(ProcInfo,
			argument_modes(ArgInstTable, ArgModes0)),
		inst_table_create_sub(InstTable0, ArgInstTable, Sub, InstTable),
		list__map(apply_inst_table_sub_mode(Sub), ArgModes0, ArgModes),
		recompute_info_set_inst_table(InstTable, RI0, RI1),
		recompute_instmap_delta_call_2(Args, InstMap0,
			ArgModes, InstMap, RI1, RI),
		compute_instmap_delta(InstMap0, InstMap, InstMapDelta)
	).

:- pred recompute_instmap_delta_call_2(list(prog_var), instmap, list(mode),
		instmap, recompute_info, recompute_info).
:- mode recompute_instmap_delta_call_2(in, in, in, out, in, out) is det.

recompute_instmap_delta_call_2([], InstMap, [], InstMap,
		ModuleInfo, ModuleInfo).
recompute_instmap_delta_call_2([_|_], _, [], _, _, _) :-
	error("recompute_instmap_delta_call_2: not enough modes").
recompute_instmap_delta_call_2([], _, [_|_], _, _, _) :-
	error("recompute_instmap_delta_call_2: not enough vars").
recompute_instmap_delta_call_2([Arg | Args], InstMap0, [Mode | Modes],
		InstMap, RI0, RI) :-
	% This is similar to modecheck_set_var_inst.
	recompute_info_get_module_info(RI0, ModuleInfo0),
	recompute_info_get_inst_table(RI0, InstTable0),
	( instmap__is_reachable(InstMap0) ->
		instmap__lookup_var(InstMap0, Arg, ArgInst0),
		mode_get_insts(ModuleInfo0, Mode, _, FinalInst),
		(
			abstractly_unify_inst(dead, ArgInst0, FinalInst,
				fake_unify, InstTable0, ModuleInfo0, InstMap0,
				UnifyInst, _, InstTable, ModuleInfo1, InstMap1)
		->
			ModuleInfo = ModuleInfo1,
			instmap__set(InstMap1, Arg, UnifyInst, InstMap2),
			recompute_info_set_module_info(ModuleInfo, RI0, RI1),
			recompute_info_set_inst_table(InstTable, RI1, RI2),
			recompute_instmap_delta_call_2(Args, InstMap2,
				Modes, InstMap, RI2, RI)
		;
			error("recompute_instmap_delta_call_2: unify_inst failed")
		)
	;
		instmap__init_unreachable(InstMap),
		RI = RI0
	).

:- pred recompute_instmap_delta_unify(prog_var, unify_rhs, unify_mode,
	unification, unify_context, hlds_goal_info, hlds_goal_expr,
	determinism, instmap, instmap_delta, recompute_info, recompute_info).
:- mode recompute_instmap_delta_unify(in, in, in, in, in, in, out, out, in, out,
	in, out) is det.

	% var-functor unification
	%
	% Make aliases for the arguments of the functor.  Note
	% that we do not need to make one for the var on the
	% LHS because:
	%	- If the unification is a construction, the
	%	  LHS variable was either free or aliased to
	%	  free.  In the latter case, we do not need
	%	  to make a new alias since one already exists,
	%	  in the former case the variable is not aliased
	%	  to anything and therefore
	%	  abstractly_unify_inst_functor will not demand
	%	  that the variable have an alias.
	%	- If the unification is a deconstruction, then
	%	  whatever defined the variable will have given
	%	  it an alias.
	%
recompute_instmap_delta_unify(Var, functor(ConsId, Vars), _UniMode0,
		Unification0, UniContext, _GoalInfo, Goal, Det, InstMap0,
		InstMapDelta, RI0, RI) :-

	recompute_info_get_module_info(RI0, ModuleInfo0),
	recompute_info_get_inst_table(RI0, InstTable0),

	inst_table_get_inst_key_table(InstTable0, IKT0),
	list__map(recompute_info_var_is_live(RI0), Vars, ArgLives),
	make_var_aliases(Vars, ArgLives, InstMap0, InstMap1, IKT0, IKT1),
	inst_table_set_inst_key_table(InstTable0, IKT1, InstTable1),
	instmap__lookup_var(InstMap1, Var, InitialInst),

	list__length(Vars, Arity),
	recompute_info_var_is_live(RI0, Var, VarLive),
	list__map(instmap__lookup_var(InstMap1), Vars, ArgInsts),
	(
		abstractly_unify_inst_functor(VarLive, InitialInst,
			ConsId, ArgInsts, ArgLives, real_unify,
			InstTable1, ModuleInfo0, InstMap1,
			UnifyInst0, Det0, InstTable2, ModuleInfo2, InstMap2)
	->
		ModuleInfo = ModuleInfo2,
		InstTable = InstTable2,
		UnifyInst = UnifyInst0,
		InstMap3 = InstMap2,
		Det1 = Det0
	;
		error("recompute_instmap_delta_unify: var-functor unify failed")
	),
	instmap__set(InstMap3, Var, UnifyInst, InstMap),
	ModeOfX = (InitialInst - UnifyInst),
	ModeOfY = (bound(unique, [functor(ConsId, ArgInsts)]) - UnifyInst),
	UniMode = ModeOfX - ModeOfY,
	UnifyRhs = functor(ConsId, Vars),
	(
		inst_expand(InstMap1, InstTable, ModuleInfo, InitialInst,
			InstOfX2),
		get_arg_insts(InstOfX2, ConsId, Arity, InstOfXArgs),
		get_mode_of_args(UnifyInst, InstOfXArgs, ModeOfXArgs0)
	->
		ModeOfXArgs = ModeOfXArgs0
	;
		error("recompute_instmap_delta_unify: get_(inst/mode)_of_args failed")
	),
	(
		get_mode_of_args(UnifyInst, ArgInsts, ModeArgs0)
	->
		ModeArgs = ModeArgs0
	;
		error("recompute_instmap_delta_unify: get_mode_of_args failed")
	),

	compute_instmap_delta(InstMap0, InstMap, InstMapDelta),

	mode_util__inst_pairs_to_uni_modes(ModeOfXArgs, ModeArgs, UniModes),
	(
		Det1 = failure
	->
		Det = Det1,
		map__init(StoreMap),
		Goal = disj([], StoreMap)
	;
		Unification0 = construct(_, RealConsId, _, _,
			ReuseVar, CellIsUnique, RLExprnId)
	->
		Unification = construct(Var, RealConsId, Vars, UniModes,
				ReuseVar, CellIsUnique, RLExprnId),
		Det = det,
		Goal = unify(Var, UnifyRhs, UniMode, Unification, UniContext)
	;
		Unification0 = deconstruct(_, RealConsId, _, _,
				_)
	->
		determinism_components(Det1, CanFail, _),
		(
			recompute_info_get_vartypes(RI0, VarTypes),
			map__lookup(VarTypes, Var, TypeOfX),
			type_constructors(TypeOfX, ModuleInfo, [_])
		->
			Unification = deconstruct(Var, RealConsId, Vars,
				UniModes, cannot_fail),
				Det = det
		;
			Unification = deconstruct(Var, RealConsId, Vars,
				UniModes, CanFail),
				Det = Det1
		),
		Goal = unify(Var, UnifyRhs, UniMode, Unification, UniContext)
	;
		error("recompute_instmap_delta_unify: bad var-functor unification")
	),

	recompute_info_set_module_info(ModuleInfo, RI0, RI1),
	recompute_info_set_inst_table(InstTable, RI1, RI).

	% var-lambda unification
	%
recompute_instmap_delta_unify(Var,
		lambda_goal(PredOrFunc, EvalMethod, FixModes, LambdaNonLocals,
			Vars, LambdaModes, LambdaDet, _, LambdaGoal0),
		_UniMode0, Unification0, UniContext, _GoalInfo, Goal, Det,
		InstMap0, InstMapDelta, RI0, RI) :-

	% First, compute the instmap_delta of the goal.

	% Set the head modes of the lambda.
	recompute_info_get_module_info(RI0, ModuleInfo0),
	recompute_info_get_inst_table(RI0, InstTable0),

	instmap__pre_lambda_update(ModuleInfo0, Vars, LambdaModes,
		IMDelta, InstTable0, InstTable1, InstMap0, InstMap1),

	% Analyse the lambda goal

	recompute_info_set_inst_table(InstTable1, RI0, RI1),
	LambdaModes = argument_modes(LambdaArgInstTable,
			LambdaArgModes),
		% YYY is InstMap1 correct here?	
	get_arg_lives(LambdaArgModes, InstMap1, LambdaArgInstTable,
			ModuleInfo0, LambdaArgLives),
	get_live_vars(Vars, LambdaArgLives, LiveVars),

	recompute_info_add_live_vars_list(LiveVars, RI1, RI1a),
	recompute_instmap_delta_2(LambdaGoal0, LambdaGoal, InstMap1,
		_, _, RI1a, RI2a),
	recompute_info_remove_live_vars_list(LiveVars, RI2a, RI2),
	recompute_info_get_module_info(RI2, ModuleInfo2),
	recompute_info_get_inst_table(RI2, InstTable2),

	instmap__lookup_var(InstMap1, Var, InstOfX),

	LambdaPredInfo = pred_inst_info(PredOrFunc, LambdaModes,
		LambdaDet),
	InstOfY = ground(unique, yes(LambdaPredInfo)),

	(
		abstractly_unify_inst(dead, InstOfX, InstOfY,
			real_unify, InstTable2, ModuleInfo2, InstMap1,
			UnifyInst0, Det0, InstTable3, ModuleInfo3, InstMap2)
	->
		InstTable = InstTable3,
		InstMap3 = InstMap2,
		UnifyInst0 = UnifyInst,
		ModuleInfo = ModuleInfo3,
		Det = Det0
	;
		error("recompute_instmap_delta_unify: var-lambda unify failed")
	),
	instmap__set(InstMap3, Var, UnifyInst, InstMapUnify),
	compute_instmap_delta(InstMap0, InstMapUnify, InstMapDelta),

	ModeOfX = (InstOfX - UnifyInst),
	ModeOfY = (InstOfY - UnifyInst),
	UniMode = ModeOfX - ModeOfY,
	UnifyRhs = lambda_goal(PredOrFunc, EvalMethod, FixModes,
			LambdaNonLocals, Vars, LambdaModes,
			LambdaDet, IMDelta, LambdaGoal),
	(
		Unification0 = construct(_, RealConsId, _, _,
			ReuseVar, CellIsUnique, RLExprnId)
	->
		list__delete_elems(LambdaNonLocals, Vars, ArgVars),
		instmap__lookup_vars(ArgVars, InstMap0, ArgInsts),
		assoc_list__from_corresponding_lists(ArgInsts, ArgInsts,
			ArgModes0),
		mode_util__inst_pairs_to_uni_modes(ArgModes0, ArgModes0,
			ArgModes),
		Unification = construct(Var, RealConsId, Vars, ArgModes,
			ReuseVar, CellIsUnique, RLExprnId)
	;
		error("recompute_instmap_delta_unify: bad var-lambda unification")
	),

	recompute_info_set_module_info(ModuleInfo, RI2, RI3),
	recompute_info_set_inst_table(InstTable, RI3, RI),
	Goal = unify(Var, UnifyRhs, UniMode, Unification, UniContext).

	% var-var unification
	%
	% Make a new alias for one of the vars (either one
	% will do).  abstractly_unify_inst will not demand
	% both vars to have aliases since it will create a
	% new alias for the unified inst if only one does.
	% We will then set both VarX and VarY to the new
	% alias.
	%
recompute_instmap_delta_unify(Var, var(VarY), _UniMode0, Unification0,
		UniContext, GoalInfo, Goal, Det, InstMap0, InstMapDelta,
		RI0, RI) :-
	recompute_info_get_module_info(RI0, ModuleInfo0),
	recompute_info_get_inst_table(RI0, InstTable0),
	instmap__lookup_var(InstMap0, VarX, InitialInstX0),
	instmap__lookup_var(InstMap0, VarY, InitialInstY0),
	recompute_info_var_is_live(RI0, VarX, LiveX),
	recompute_info_var_is_live(RI0, VarY, LiveY),

	inst_table_get_inst_key_table(InstTable0, IKT0),
	make_var_alias(Var, LiveX, InstMap0, InstMap1, IKT0, IKT1),
	inst_table_set_inst_key_table(InstTable0, IKT1, InstTable1),
	VarX = Var,	% Keep the names orthogonal
	instmap__lookup_var(InstMap1, VarX, InitialInstX),
	instmap__lookup_var(InstMap1, VarY, InitialInstY),
	( LiveX = live, LiveY = live ->
		BothLive = live
	;
		BothLive = dead
	),
	(
		abstractly_unify_inst(BothLive, InitialInstX,
			InitialInstY, real_unify, InstTable1,
			ModuleInfo0, InstMap1, UnifyInst0, Det0,
			InstTable2, ModuleInfo1, InstMap2)
	->
		InstMap3 = InstMap2,
		InstTable3 = InstTable2,
		UnifyInst = UnifyInst0,
		InstTable = InstTable3,
		ModuleInfo2 = ModuleInfo1,
		Det1 = Det0
	;
		error("recompute_instmap_delta_unify: var-var unify failed")
	),
	instmap__set(InstMap3, VarX, UnifyInst, InstMap4),
	instmap__set(InstMap4, VarY, UnifyInst, InstMap5),
	ModeOfX = (InitialInstX - UnifyInst),
	ModeOfY = (InitialInstY - UnifyInst),
	UniMode = ModeOfX - ModeOfY,
	UnifyRhs = var(VarY),

	(
		InitialInstX0 = alias(IK),
		InitialInstY0 = alias(IK)
	->
		Det = det,
		Goal = conj([]),
		ModuleInfo = ModuleInfo2,
		GoalChanged = yes
	;
		Det1 = failure
	->
		map__init(StoreMap),
		Goal = disj([], StoreMap),
		Det = failure,
		ModuleInfo = ModuleInfo2,
		GoalChanged = yes
	;
		( Unification0 = assign(_, _)
		; Unification0 = simple_test(_, _)
		)
	->
		(
			Unification0 = simple_test(_, _),
			determinism_components(Det1, DetCanFail1, _),
			DetCanFail1 = cannot_fail
		->
			Det = det,
			Goal = conj([]),
			GoalChanged = yes
		;
			Det = Det1,
			Goal = unify(Var, UnifyRhs, UniMode, Unification0,
					UniContext),
			GoalChanged = no
		),
		ModuleInfo = ModuleInfo2
	;
		Unification0 = complicated_unify(_, CanFail, TIVars)
	->
		Det = Det1,
		ComplUniMode = ((InitialInstX - InitialInstY) ->
					(UnifyInst - UnifyInst)),
		Unification = complicated_unify(ComplUniMode, CanFail, TIVars),
		goal_info_get_context(GoalInfo, Context),
		recompute_info_get_vartypes(RI0, VarTypes),
		map__lookup(VarTypes, Var, Type),
		( type_to_type_id(Type, TypeId, _) ->
			unify_proc__request_unify(unify_proc_id(TypeId,
					InitialInstX, InitialInstY, InstTable),
				Det, Context, InstMap0, ModuleInfo2,
				ModuleInfo)
		;
			ModuleInfo = ModuleInfo2
		),
		Goal = unify(Var, UnifyRhs, UniMode,
				Unification, UniContext),
		GoalChanged = no
	;
		error("recompute_instmap_delta_unify: bad var-var unification")
	),
	InstMap = InstMap5,

	compute_instmap_delta(InstMap0, InstMap, InstMapDelta),
	recompute_info_set_module_info(ModuleInfo, RI0, RI1),
	recompute_info_set_inst_table(InstTable, RI1, RI2),
	( GoalChanged = bool__yes ->
		recompute_info_set_goal_changed(RI2, RI)
	;
		RI = RI2
	).

%-----------------------------------------------------------------------------%

:- pred make_var_alias(prog_var, is_live, instmap, instmap,
		inst_key_table, inst_key_table).
:- mode make_var_alias(in, in, in, out, in, out) is det.

make_var_alias(Var, _Live, InstMap0, InstMap, IKT0, IKT) :-
	instmap__lookup_var(InstMap0, Var, Inst),
	( Inst = alias(_) ->
		InstMap0 = InstMap,
		IKT0 = IKT
	;
		inst_key_table_add(IKT0, Inst, InstKey, IKT),
		instmap__set(InstMap0, Var, alias(InstKey), InstMap)
	).

:- pred make_var_aliases(list(prog_var), list(is_live), instmap, instmap,
		inst_key_table, inst_key_table).
:- mode make_var_aliases(in, in, in, out, in, out) is det.

make_var_aliases([], [], InstMap, InstMap) --> [].
make_var_aliases([V | Vs], [Live | Lives], InstMap0, InstMap) -->
	make_var_alias(V, Live, InstMap0, InstMap1),
	make_var_aliases(Vs, Lives, InstMap1, InstMap).
make_var_aliases([], [_|_], _, _) -->
	{ error("make_var_aliases") }.
make_var_aliases([_|_], [], _, _) -->
	{ error("make_var_aliases") }.

%-----------------------------------------------------------------------------%

	% Arguments with final inst `clobbered' are dead, any
	% others are assumed to be live.

get_arg_lives([], _, _, _, []).
get_arg_lives([Mode|Modes], InstMap, InstTable, ModuleInfo, [IsLive|IsLives]) :-
	mode_get_insts(ModuleInfo, Mode, _InitialInst, FinalInst),
	( inst_is_clobbered(FinalInst, InstMap, InstTable, ModuleInfo) ->
		IsLive = dead
	;
		IsLive = live
	),
	get_arg_lives(Modes, InstMap, InstTable, ModuleInfo, IsLives).

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

:- pred strip_builtin_qualifier_from_sym_name(sym_name::in,
						sym_name::out) is det.

strip_builtin_qualifier_from_sym_name(SymName0, SymName) :-
	(
		SymName0 = qualified(Module, Name),
		( mercury_public_builtin_module(Module)
		; mercury_private_builtin_module(Module)
		)
	->
		SymName = unqualified(Name)
	;
		SymName = SymName0
	).

strip_builtin_qualifiers_from_inst_list(Insts0, Insts) :-
	list__map(strip_builtin_qualifiers_from_inst, Insts0, Insts).

strip_builtin_qualifiers_from_inst(inst_var(V), inst_var(V)).
strip_builtin_qualifiers_from_inst(alias(V), alias(V)).
strip_builtin_qualifiers_from_inst(not_reached, not_reached).
strip_builtin_qualifiers_from_inst(free(A), free(A)).
strip_builtin_qualifiers_from_inst(free(A, Type), free(A, Type)).
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
	Modes0 = argument_modes(ArgInstTable, ArgModes0),
	Modes = argument_modes(ArgInstTable, ArgModes),
	strip_builtin_qualifiers_from_mode_list(ArgModes0, ArgModes).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

normalise_insts([], [], _, _, _, []).
normalise_insts([Inst0|Insts0], [Type|Types], InstMap, InstTable, ModuleInfo,
		[Inst|Insts]) :-
	normalise_inst(Inst0, Type, InstMap, InstTable, ModuleInfo, Inst),
	normalise_insts(Insts0, Types, InstMap, InstTable, ModuleInfo, Insts).
normalise_insts([], [_|_], _, _, _, _) :-
	error("normalise_insts: length mismatch").
normalise_insts([_|_], [], _, _, _, _) :-
	error("normalise_insts: length mismatch").

	% This is a bit of a hack.
	% The aim is to avoid non-termination due to the creation
	% of ever-expanding insts.
	% XXX should also normalise partially instantiated insts.

normalise_inst(Inst0, Type, InstMap, InstTable, ModuleInfo, NormalisedInst) :-
	inst_expand(InstMap, InstTable, ModuleInfo, Inst0, Inst),
	( Inst = bound(_, _) ->
		(
			inst_is_ground(Inst, InstMap, InstTable, ModuleInfo),
			inst_is_unique(Inst, InstMap, InstTable, ModuleInfo),
			% don't infer unique modes for introduced type_infos
			% arguments, because that leads to an increase
			% in the number of inferred modes without any benefit
			\+ is_introduced_type_info_type(Type)
		->
			NormalisedInst = ground(unique, no)
		;
			inst_is_ground(Inst, InstMap, InstTable, ModuleInfo),
			inst_is_mostly_unique(Inst, InstMap, InstTable,
					ModuleInfo),
			% don't infer unique modes for introduced type_infos
			% arguments, because that leads to an increase
			% in the number of inferred modes without any benefit
			\+ is_introduced_type_info_type(Type)
		->
			NormalisedInst = ground(mostly_unique, no)
		;
			inst_is_ground(Inst, InstMap, InstTable, ModuleInfo),
			\+ inst_is_clobbered(Inst, InstMap, InstTable,
					ModuleInfo)
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

% fixup_switch_var(Var, InstMap0, InstMap, Goal0, Goal) :-
% 	Goal0 = GoalExpr - GoalInfo0,
% 	goal_info_get_instmap_delta(GoalInfo0, InstMapDelta0),
% 	instmap__lookup_var(InstMap0, Var, Inst0),
% 	instmap__lookup_var(InstMap, Var, Inst),
% 	( Inst = Inst0 ->
% 		GoalInfo = GoalInfo0
% 	;
% 		instmap_delta_set(InstMapDelta0, Var, Inst, InstMapDelta),
% 		goal_info_set_instmap_delta(GoalInfo0, InstMapDelta, GoalInfo)
% 	),
% 	Goal = GoalExpr - GoalInfo.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

in_mode(Mode) :- make_std_mode("in", [], Mode).

out_mode(Mode) :- make_std_mode("out", [], Mode).

uo_mode(Mode) :- make_std_mode("uo", [], Mode).

in_any_mode(Mode) :- make_std_mode("in", [any(shared)], Mode).

out_any_mode(Mode) :- make_std_mode("out", [any(shared)], Mode).

unused_mode(Mode) :- make_std_mode("unused", [], Mode).

aditi_ui_mode = Mode :- in_mode(Mode). 

aditi_di_mode = Mode :- in_mode(Mode).

aditi_uo_mode = Mode :- out_mode(Mode).

:- pred make_std_mode(string, list(inst), mode).
:- mode make_std_mode(in, in, out) is det.

make_std_mode(Name, Args, Mode) :-
	mercury_public_builtin_module(MercuryBuiltin),
	QualifiedName = qualified(MercuryBuiltin, Name),
	Mode = user_defined_mode(QualifiedName, Args).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% apply_inst_key_sub(Sub0, InstMap0, InstMap, InstTable0, InstTable) :-
% 	set__init(DeadKeys),
% 	inst_table_get_inst_key_table(InstTable0, IKT0),
% 	apply_inst_key_sub_2(Sub0, Sub0, Sub, DeadKeys, IKT0, IKT),
% 	map__keys(Sub, SubDomain),
% 
% 	% Do the instmap
% 	instmap__vars(InstMap0, VarsToChangeList),
% 	apply_inst_key_sub_instmap(VarsToChangeList, Sub, InstMap0, InstMap),
% 
% 	inst_table_get_all_tables(InstTable0, UnifyInsts0, MergeInsts0,
% 		GroundInsts0, AnyInsts0, SharedInsts0, NondetLiveInsts0,
% 		_IKT),
% 	% YYY Should do renaming in these.
% 	UnifyInsts = UnifyInsts0,
% 	MergeInsts = MergeInsts0,
% 	GroundInsts = GroundInsts0,
% 	AnyInsts = AnyInsts0,
% 	SharedInsts = SharedInsts0,
% 	NondetLiveInsts = NondetLiveInsts0,
% 	inst_table_set_all_tables(InstTable0, UnifyInsts, MergeInsts,
% 		GroundInsts, AnyInsts, SharedInsts, NondetLiveInsts,
% 		IKT, InstTable).
% 
% :- pred apply_inst_key_sub_2(inst_key_sub, inst_key_sub, inst_key_sub,
% 			set(inst_key), inst_key_table, inst_key_table).
% :- mode apply_inst_key_sub_2(in, in, out, in, in, out) is det.
% 
% apply_inst_key_sub_2(ThisSub, Sub0, Sub, DeadKeys0, IKT0, IKT) :-
% 	( map__is_empty(ThisSub) ->
% 		IKT = IKT0,
% 		Sub = Sub0
% 	;
% 		map__keys(ThisSub, SubDomain),
% 		set__init(KeysToChange0),
% 		list__foldl(lambda([K :: in, Ks0 :: in, Ks :: out]
% 					is det,
% 				( inst_key_table_dependent_keys(IKT0, K, Ks1),
% 				  set__insert_list(Ks0, Ks1, Ks)
% 				)
% 			), SubDomain, KeysToChange0, KeysToChange1),
% 		set__difference(KeysToChange1, DeadKeys0, KeysToChange),
% 		set__to_sorted_list(KeysToChange, KeysToChangeList),
% 
% 		map__init(NewSub0),
% 
% 		apply_inst_key_sub_inst_key_table(KeysToChangeList, ThisSub,
% 			DeadKeys0, DeadKeys, NewSub0, NewSub, IKT0, IKT1),
% 		map__overlay(Sub0, NewSub, Sub1),
% 
% 		apply_inst_key_sub_2(NewSub, Sub1, Sub, DeadKeys, IKT1, IKT)
% 	).
% 
% :- pred apply_inst_key_sub_instmap(list(var), inst_key_sub, instmap, instmap).
% :- mode apply_inst_key_sub_instmap(in, in, in, out) is det.
% 
% apply_inst_key_sub_instmap([], _Sub, InstMap, InstMap).
% apply_inst_key_sub_instmap([V | Vs], Sub, 
% 		InstMap0, InstMap) :-
% 	instmap__lookup_var(InstMap0, V, Inst0),
% 	inst_apply_sub(Sub, Inst0, Inst),
% 	instmap__set(InstMap0, V, Inst, InstMap1),
% 	apply_inst_key_sub_instmap(Vs, Sub, InstMap1, InstMap).
% 
% :- pred apply_inst_key_sub_inst_key_table(list(inst_key),
% 		inst_key_sub, set(inst_key), set(inst_key),
% 		inst_key_sub, inst_key_sub, inst_key_table, inst_key_table).
% :- mode apply_inst_key_sub_inst_key_table(in, in, in, out, in, out, in, out)
% 		is det.
% 
% apply_inst_key_sub_inst_key_table([], _Sub, DeadKeys, DeadKeys, NewSub, NewSub,
% 		IKT, IKT).
% apply_inst_key_sub_inst_key_table([Key0 | Keys], Sub, DeadKeys0, DeadKeys,
% 		NewSub0, NewSub, IKT0, IKT) :-
% 	inst_key_table_lookup(IKT0, Key0, Inst0),
% 	inst_apply_sub(Sub, Inst0, Inst),
% 	( Inst0 = Inst ->
% 		IKT0 = IKT1,
% 		NewSub1 = NewSub0,
% 		DeadKeys1 = DeadKeys0
% 	;
% 		inst_key_table_add(IKT0, Inst, Key, IKT1),
% 		set__insert(DeadKeys0, Key0, DeadKeys1),
% 		map__det_insert(NewSub0, Key0, Key, NewSub1)
% 	),
% 	apply_inst_key_sub_inst_key_table(Keys, Sub, DeadKeys1, DeadKeys,
% 		NewSub1, NewSub, IKT1, IKT).

%-----------------------------------------------------------------------------%

apply_inst_table_sub_mode(Sub, (I0 -> F0), (I -> F)) :-
	inst_apply_inst_table_sub(Sub, I0, I),
	inst_apply_inst_table_sub(Sub, F0, F).
apply_inst_table_sub_mode(Sub, user_defined_mode(SymName, Insts0),
		user_defined_mode(SymName, Insts)) :-
	list__map(inst_apply_inst_table_sub(Sub), Insts0, Insts).

%-----------------------------------------------------------------------------%

	% Given a list of variables, and a list of livenesses,
	% select the live variables.

get_live_vars([_|_], [], _) :- error("get_live_vars: length mismatch").
get_live_vars([], [_|_], _) :- error("get_live_vars: length mismatch").
get_live_vars([], [], []).
get_live_vars([Var|Vars], [IsLive|IsLives], LiveVars) :-
	( IsLive = live ->
		LiveVars = [Var | LiveVars0]
	;
		LiveVars = LiveVars0
	),
	get_live_vars(Vars, IsLives, LiveVars0).

%-----------------------------------------------------------------------------%

partition_args(_, _, _, [], [_|_], _, _) :-
        error("partition_args").
partition_args(_, _, _, [_|_], [], _, _) :-
	error("partition_args").
partition_args(_, _, _, [], [], [], []).
partition_args(InstMap, InstTable, ModuleInfo, [ArgMode | ArgModes],
		[Arg | Args], InputArgs, OutputArgs) :-
	partition_args(InstMap, InstTable, ModuleInfo, ArgModes,
		Args, InputArgs1, OutputArgs1),
	( mode_is_input(InstMap, InstTable, ModuleInfo, ArgMode) ->
		InputArgs = [Arg | InputArgs1],
		OutputArgs = OutputArgs1
	;
		InputArgs = InputArgs1,
		OutputArgs = [Arg | OutputArgs1]
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
