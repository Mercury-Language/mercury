%---------------------------------------------------------------------------%
% Copyright (C) 1995-2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% Original author: squirrel (Jane Anna Langley).
% Some bugs fixed by fjh.
% Extensive revision by zs.
% More revision by stayl.
%
% This module attempts to optimise out instances where a variable is
% decomposed and then soon after reconstructed from the parts. If possible
% we would like to "short-circuit" this process.
% It also optimizes deconstructions of known cells, replacing them with
% assignments to the arguments where this is guaranteed to not increase
% the number of stack slots required by the goal.
% Repeated calls to predicates with the same input arguments are replaced by
% assigments and warnings are returned.
%
% IMPORTANT: This module does a small subset of the job of compile-time
% garbage collection, but it does so without paying attention to uniqueness
% information, since the compiler does not yet have such information.
% Once we implement ctgc, the assumptions made by this module will have
% to be revisited.
%
%---------------------------------------------------------------------------%

:- module check_hlds__common.
:- interface.

:- import_module hlds__hlds_pred, hlds__hlds_goal, parse_tree__prog_data.
:- import_module check_hlds__simplify.
:- import_module list.

	% If we find a deconstruction or a construction we cannot optimize,
	% record the details of the memory cell in CommonInfo.

	% If we find a construction that constructs a cell identical to one
	% we have seen before, replace the construction with an assignment
	% from the variable unified with that cell.

:- pred common__optimise_unification(unification, prog_var, unify_rhs,
	unify_mode, unify_context, hlds_goal_expr, hlds_goal_info,
	hlds_goal_expr, hlds_goal_info, simplify_info, simplify_info).
:- mode common__optimise_unification(in, in, in, in, in, in, in, 
	out, out, in, out) is det.

	% Check whether this call has been seen before and is replaceable, if
	% so produce assignment unification for the non-local output variables,
	% and give a warning.
	% A call is considered replaceable if it has no uniquely moded outputs
	% and no destructive inputs.
	% It is the caller's responsibility to check that the call is pure.

:- pred common__optimise_call(pred_id, proc_id, list(prog_var), hlds_goal_expr,
	hlds_goal_info, hlds_goal_expr, simplify_info, simplify_info).
:- mode common__optimise_call(in, in, in, in, in, out, in, out) is det.

:- pred common__optimise_higher_order_call(prog_var, list(prog_var), list(mode),
		determinism, hlds_goal_expr, hlds_goal_info, hlds_goal_expr,
		simplify_info, simplify_info).
:- mode common__optimise_higher_order_call(in, in, in, in, in, in, out,
		in, out) is det.

	% succeeds if the two variables are equivalent
	% according to the specified equivalence class.
:- pred common__vars_are_equivalent(prog_var, prog_var, common_info).
:- mode common__vars_are_equivalent(in, in, in) is semidet.

	% Assorted stuff used here that simplify.m doesn't need to know about.
:- type common_info.

:- pred common_info_init(common_info).
:- mode common_info_init(out) is det.

:- pred common_info_clear_structs(common_info, common_info).
:- mode common_info_clear_structs(in, out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds__quantification, check_hlds__mode_util.
:- import_module check_hlds__type_util, parse_tree__prog_util.
:- import_module check_hlds__det_util, check_hlds__det_report, libs__globals.
:- import_module libs__options, check_hlds__inst_match, hlds__instmap.
:- import_module hlds__hlds_data, hlds__hlds_module, (parse_tree__inst).
:- import_module transform_hlds__pd_cost, term.
:- import_module bool, map, set, eqvclass, require, std_util, string.

:- type structure
	--->	structure(prog_var, type, cons_id, list(prog_var)).

:- type call_args
	--->	call_args(prog_context, list(prog_var), list(prog_var)).
			% input, output args. For higher-order calls, 
			% the closure is the first input argument.

:- type struct_map	==	map(cons_id, list(structure)).
:- type seen_calls 	==	map(seen_call_id, list(call_args)).

:- type common_info
	--->	common(
			eqvclass(prog_var),
			struct_map,	% all structs seen.
			struct_map,	% structs seen since the last call.
			seen_calls
		).

%---------------------------------------------------------------------------%

common_info_init(CommonInfo) :-
	eqvclass__init(VarEqv0),
	map__init(StructMap0),
	map__init(SeenCalls0),
	CommonInfo = common(VarEqv0, StructMap0, StructMap0, SeenCalls0).

	% Clear structs seen since the last call. Replacing deconstructions
	% of these structs with assignments after the call would cause an
	% increase in the number of stack slots required.
common_info_clear_structs(common(VarEqv, StructMap, _, SeenCalls),
		common(VarEqv, StructMap, Empty, SeenCalls)) :-
	map__init(Empty).
		
%---------------------------------------------------------------------------%

common__optimise_unification(Unification0, _Left0, _Right0, Mode, _Context,
		Goal0, GoalInfo0, Goal, GoalInfo, Info0, Info) :-
	(
		Unification0 = construct(Var, ConsId, ArgVars, _, _, _, _),
		Mode = LVarMode - _,
		simplify_info_get_module_info(Info0, ModuleInfo),
		mode_get_insts(ModuleInfo, LVarMode, _, Inst),
		(
				% Don't optimise partially instantiated
				% deconstruction unifications, because it's
				% tricky to work out how to mode the
				% replacement asssignment unifications.
				% In the vast majority of cases, the
				% variable is ground.
			\+ inst_is_ground(ModuleInfo, Inst)
		->
			Goal = Goal0,
			GoalInfo = GoalInfo0,
			Info = Info0
		;
			% common__generate_assign assumes that the
			% output variable is in the instmap_delta, which
			% will not be true if the variable is a local.
			% The optimization is pointless in that case.
			goal_info_get_instmap_delta(GoalInfo0, InstMapDelta),
			instmap_delta_search_var(InstMapDelta, Var, _),
			common__find_matching_cell(Var, ConsId, ArgVars,
				construction, Info0, OldStruct)
		->
			OldStruct = structure(OldVar, _, _, _),
			UniMode = ((free - Inst) -> (Inst - Inst)),
			common__generate_assign(Var, OldVar, UniMode,
				GoalInfo0, Goal - GoalInfo, Info0, Info1),
			simplify_info_set_requantify(Info1, Info2),
			pd_cost__goal(Goal0 - GoalInfo0, Cost),
			simplify_info_incr_cost_delta(Info2, Cost, Info)
		;
			Goal = Goal0,
			GoalInfo = GoalInfo0,
			common__record_cell(Var, ConsId, ArgVars, Info0, Info)
		)
	;
		Unification0 = deconstruct(Var, ConsId,
				ArgVars, UniModes, CanFail, _),
		simplify_info_get_module_info(Info0, ModuleInfo),
		(
				% Don't optimise partially instantiated
				% deconstruction unifications, because it's
				% tricky to work out how to mode the
				% replacement asssignment unifications.
				% In the vast majority of cases, the
				% variable is ground.
			Mode = LVarMode - _,
			mode_get_insts(ModuleInfo, LVarMode, Inst0, _),
			\+ inst_is_ground(ModuleInfo, Inst0)
		->
			Goal = Goal0,
			Info = Info0
		;
			% Do not delete deconstruction unifications inserted by
			% stack_opt.m, which has done a more comprehensive cost
			% analysis than common.m can do.
			\+ goal_info_has_feature(GoalInfo, stack_opt),
			common__find_matching_cell(Var, ConsId, ArgVars,
				deconstruction, Info0, OldStruct)
		->
			OldStruct = structure(_, _, _, OldArgVars),
			common__create_output_unifications(GoalInfo0, ArgVars,
				OldArgVars, UniModes, Goals, Info0, Info1),
			Goal = conj(Goals),
			pd_cost__goal(Goal0 - GoalInfo0, Cost),
			simplify_info_incr_cost_delta(Info1, Cost, Info2),
			simplify_info_set_requantify(Info2, Info3),
			( CanFail = can_fail ->
				simplify_info_set_rerun_det(Info3, Info)
			;	
				Info = Info3
			)
		;
			Goal = Goal0,
			common__record_cell(Var, ConsId, ArgVars, Info0, Info)
		),
		GoalInfo = GoalInfo0
	;
		Unification0 = assign(Var1, Var2),
		Goal = Goal0,
		common__record_equivalence(Var1, Var2, Info0, Info),
		GoalInfo = GoalInfo0
	;
		Unification0 = simple_test(Var1, Var2),
		Goal = Goal0,
		common__record_equivalence(Var1, Var2, Info0, Info),
		GoalInfo = GoalInfo0
	;
		Unification0 = complicated_unify(_, _, _),
		Goal = Goal0,
		Info = Info0,
		GoalInfo = GoalInfo0
	).

%---------------------------------------------------------------------------%

:- type unification_type
	--->	deconstruction
	;	construction.
		
:- pred common__find_matching_cell(prog_var, cons_id,
		list(prog_var), unification_type, simplify_info, structure).
:- mode common__find_matching_cell(in, in, in, in, in, out) is semidet.

common__find_matching_cell(Var, ConsId, ArgVars, UniType, Info, OldStruct) :-
	simplify_info_get_common_info(Info, CommonInfo),
	simplify_info_get_var_types(Info, VarTypes),
	CommonInfo = common(VarEqv, StructMapAll, StructMapSinceLastFlush, _),
	(
		UniType = construction,
		StructMapToUse = StructMapAll
	;
		% For deconstructions, using the arguments of a cell
		% created before the last stack flush would cause more
		% variables to be saved on the stack.
		UniType = deconstruction,
		StructMapToUse = StructMapSinceLastFlush
	),
	map__search(StructMapToUse, ConsId, Structs),
	common__find_matching_cell_2(Structs, Var, ConsId, ArgVars, UniType,
		VarEqv, VarTypes, OldStruct).

:- pred common__find_matching_cell_2(list(structure), prog_var, cons_id,
	list(prog_var),
	unification_type, eqvclass(prog_var), map(prog_var, type), structure).
:- mode common__find_matching_cell_2(in, in, in, in, in,
	in, in, out) is semidet.

common__find_matching_cell_2([Struct | Structs], Var, ConsId, ArgVars,
		UniType, VarEqv, VarTypes, OldStruct) :-
	Struct = structure(OldVar, StructType, StructConsId, StructArgVars),
	(
		% Are the arguments the same (or equivalent) variables?
		ConsId = StructConsId,
		(
			UniType = construction,
			common__var_lists_are_equiv(ArgVars,
				StructArgVars, VarEqv),

			% Two structures of the same shape may have different 
			% types and therefore different representations.
			map__lookup(VarTypes, Var, VarType),
			common__compatible_types(VarType, StructType)
		;
			UniType = deconstruction,
			common__vars_are_equiv(Var, OldVar, VarEqv)
		)
	->
		OldStruct = Struct
	;
		common__find_matching_cell_2(Structs, Var, ConsId, ArgVars,
			UniType, VarEqv, VarTypes, OldStruct)
	).

%---------------------------------------------------------------------------%

	% Two structures have compatible representations if the top
	% level of their types are unifiable.  % For example, if we have
	%
	%	:- type maybe_err(T) --> ok(T) ; err(string).
	%
	%	:- pred p(maybe_err(foo)::in, maybe_err(bar)::out) is semidet.
	%	p(err(X), err(X)).
	%
	% then we want to reuse the `err(X)' in the first arg rather than
	% constructing a new copy of it for the second arg.
	% The two occurrences of `err(X)' have types `maybe_err(int)'
	% and `maybe(float)', but we know that they have the same 
	% representation.

:- pred common__compatible_types(type, type).
:- mode common__compatible_types(in, in) is semidet.

common__compatible_types(Type1, Type2) :-
	type_to_ctor_and_args(Type1, TypeCtor1, _),
	type_to_ctor_and_args(Type2, TypeCtor2, _),
	TypeCtor1 = TypeCtor2.

%---------------------------------------------------------------------------%

	% succeeds if the two lists of variables are equivalent
	% according to the specified equivalence class.
:- pred common__var_lists_are_equiv(list(prog_var), list(prog_var),
		eqvclass(prog_var)).
:- mode common__var_lists_are_equiv(in, in, in) is semidet.

common__var_lists_are_equiv([], [], _VarEqv).
common__var_lists_are_equiv([X | Xs], [Y | Ys], VarEqv) :-
	common__vars_are_equiv(X, Y, VarEqv),
	common__var_lists_are_equiv(Xs, Ys, VarEqv).

common__vars_are_equivalent(X, Y, CommonInfo) :-
	CommonInfo = common(EqvVars, _, _, _),
	common__vars_are_equiv(X, Y, EqvVars).

	% succeeds if the two variables are equivalent
	% according to the specified equivalence class.
:- pred common__vars_are_equiv(prog_var, prog_var, eqvclass(prog_var)).
:- mode common__vars_are_equiv(in, in, in) is semidet.

common__vars_are_equiv(X, Y, VarEqv) :-
	% write('looking for equivalence of '),
	% write(X),
	% write(' and '),
	% write(Y),
	% nl,
	(
		X = Y
	;
		eqvclass__is_member(VarEqv, X),
		eqvclass__is_member(VarEqv, Y),
		eqvclass__same_eqvclass(VarEqv, X, Y)
	).
	% write('they are equivalent'),
	% nl.

%---------------------------------------------------------------------------%

:- pred common__record_cell(prog_var, cons_id, list(prog_var),
		simplify_info, simplify_info).
:- mode common__record_cell(in, in, in, in, out) is det.

common__record_cell(Var, ConsId, ArgVars, Info0, Info) :-
	simplify_info_get_common_info(Info0, CommonInfo0),
	simplify_info_get_var_types(Info0, VarTypes),
	( ArgVars = [] ->
		% Constants do not have memory cells to reuse,
		% at least in the memory models we are interested in.
		CommonInfo = CommonInfo0
	;
		CommonInfo0 = common(VarEqv, StructMapAll0,
			StructMapLastCall0, SeenCalls),
		map__lookup(VarTypes, Var, VarType),
		Struct = structure(Var, VarType, ConsId, ArgVars),
		common__do_record_cell(StructMapAll0, ConsId,
			Struct, StructMapAll),
		common__do_record_cell(StructMapLastCall0, ConsId, Struct,
			StructMapLastCall),
		CommonInfo = common(VarEqv, StructMapAll,
			StructMapLastCall, SeenCalls)
	),
	simplify_info_set_common_info(Info0, CommonInfo, Info).

:- pred common__do_record_cell(struct_map, cons_id, structure, struct_map).
:- mode common__do_record_cell(in, in, in, out) is det.

common__do_record_cell(StructMap0, ConsId, Struct, StructMap) :-
	( map__search(StructMap0, ConsId, StructList0Prime) ->
		StructList0 = StructList0Prime
	;
		StructList0 = []
	),

	% Insert the new cell at the front of the list. If it hides
	% an equivalent cell, at least the reuse of this cell will
	% require saving its address over fewer calls.

	StructList = [Struct | StructList0],
	map__set(StructMap0, ConsId, StructList, StructMap).

%---------------------------------------------------------------------------%

:- pred common__record_equivalence(prog_var, prog_var,
		simplify_info, simplify_info).
:- mode common__record_equivalence(in, in, in, out) is det.

common__record_equivalence(Var1, Var2, Info0, Info) :-
	simplify_info_get_common_info(Info0, CommonInfo0),
	CommonInfo0 = common(VarEqv0, StructMap0, StructMap1, SeenCalls),
	% write('ensuring equivalence of '),
	% write(Var1),
	% write(' and '),
	% write(Var2),
	% nl,
	eqvclass__ensure_equivalence(VarEqv0, Var1, Var2, VarEqv),
	CommonInfo = common(VarEqv, StructMap0, StructMap1, SeenCalls),
	simplify_info_set_common_info(Info0, CommonInfo, Info).
	
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

common__optimise_call(PredId, ProcId, Args, Goal0,
		GoalInfo, Goal, Info0, Info) :-
	(
		goal_info_get_determinism(GoalInfo, Det),
		common__check_call_detism(Det),
		simplify_info_get_var_types(Info0, VarTypes),
		simplify_info_get_module_info(Info0, ModuleInfo),
		module_info_pred_proc_info(ModuleInfo, PredId,
			ProcId, _, ProcInfo),
		proc_info_argmodes(ProcInfo, ArgModes),
	    	common__partition_call_args(VarTypes, ModuleInfo, ArgModes,
	    		Args, InputArgs, OutputArgs, OutputModes)
	->
		common__optimise_call_2(seen_call(PredId, ProcId), InputArgs,
			OutputArgs, OutputModes, Goal0, GoalInfo, Goal,
			Info0, Info)
	;
		Goal = Goal0,
		Info = Info0
	).

common__optimise_higher_order_call(Closure, Args, Modes, Det, Goal0,
		GoalInfo, Goal, Info0, Info) :-
	(
		common__check_call_detism(Det),
		simplify_info_get_var_types(Info0, VarTypes),
		simplify_info_get_module_info(Info0, ModuleInfo),
	    	common__partition_call_args(VarTypes, ModuleInfo, Modes, Args,
			InputArgs, OutputArgs, OutputModes)
	->
		common__optimise_call_2(higher_order_call,
			[Closure | InputArgs], OutputArgs, OutputModes, Goal0,
			GoalInfo, Goal, Info0, Info)
	;
		Goal = Goal0,
		Info = Info0
	).	

:- pred common__check_call_detism(determinism::in) is semidet.

common__check_call_detism(Det) :-
	determinism_components(Det, _, SolnCount),
	% Replacing nondet or mulidet calls would cause
	% loss of solutions.
	(	SolnCount = at_most_one
	;	SolnCount = at_most_many_cc
	).

:- pred common__optimise_call_2(seen_call_id, list(prog_var), list(prog_var),
		list(mode), hlds_goal_expr, hlds_goal_info, hlds_goal_expr,
		simplify_info, simplify_info).
:- mode common__optimise_call_2(in, in, in, in, in, in, out, in, out) is det.

common__optimise_call_2(SeenCall, InputArgs, OutputArgs, Modes, Goal0,
		GoalInfo, Goal, Info0, Info) :-
	simplify_info_get_common_info(Info0, CommonInfo0),
	CommonInfo0 = common(Eqv0, Structs0, Structs1, SeenCalls0),
	(
		map__search(SeenCalls0, SeenCall, SeenCallsList0)
	->
		( common__find_previous_call(SeenCallsList0, InputArgs,
			Eqv0, OutputArgs2, PrevContext)
		->
			simplify_info_get_module_info(Info0, ModuleInfo),
			mode_util__modes_to_uni_modes(Modes, Modes, ModuleInfo,
				UniModes), 
			common__create_output_unifications(GoalInfo,
			    OutputArgs, OutputArgs2, UniModes,
			    Goals, Info0, Info1),
			Goal = conj(Goals),
			simplify_info_get_var_types(Info0, VarTypes),
			(
			    simplify_do_warn_calls(Info1),
				% Don't warn for cases such as:
				% set__init(Set1 : set(int)),
				% set__init(Set2 : set(float)).
			    map__apply_to_list(OutputArgs, VarTypes,
					OutputArgTypes1),
			    map__apply_to_list(OutputArgs2, VarTypes,
					OutputArgTypes2),
			    common__types_match_exactly_list(OutputArgTypes1,
			    	OutputArgTypes2)
			->
			    goal_info_get_context(GoalInfo, Context),
			    simplify_info_do_add_msg(Info1,
			    	duplicate_call(SeenCall, PrevContext,
					Context),
			        Info2)
			;
			    Info2 = Info1
			),
			CommonInfo = common(Eqv0, Structs0,
				Structs1, SeenCalls0),
			pd_cost__goal(Goal0 - GoalInfo, Cost),
			simplify_info_incr_cost_delta(Info2, Cost, Info3),
			simplify_info_set_requantify(Info3, Info4),
			goal_info_get_determinism(GoalInfo, Detism0),
			( Detism0 \= det ->
				simplify_info_set_rerun_det(Info4, Info5)
			;
				Info5 = Info4
			)
		;
			goal_info_get_context(GoalInfo, Context),
			ThisCall = call_args(Context, InputArgs, OutputArgs),
			map__det_update(SeenCalls0, SeenCall,
				[ThisCall | SeenCallsList0], SeenCalls),
			CommonInfo = common(Eqv0, Structs0,
				Structs1, SeenCalls),
			Goal = Goal0,
			Info5 = Info0
		)
	;
		goal_info_get_context(GoalInfo, Context),
		ThisCall = call_args(Context, InputArgs, OutputArgs),
		map__det_insert(SeenCalls0, SeenCall, [ThisCall], SeenCalls),
		CommonInfo = common(Eqv0, Structs0, Structs1, SeenCalls),
		Goal = Goal0,
		Info5 = Info0
	),
	simplify_info_set_common_info(Info5, CommonInfo, Info).

%---------------------------------------------------------------------------%

	% Partition the arguments of a call into inputs and outputs,
	% failing if any of the outputs have a unique component
	% or if any of the outputs contain any `any' insts.
:- pred common__partition_call_args(vartypes::in, module_info::in,
		list(mode)::in, list(prog_var)::in, list(prog_var)::out,
		list(prog_var)::out, list(mode)::out) is semidet.

common__partition_call_args(_, _, [], [_ | _], _, _, _) :-
	error("common__partition_call_args").
common__partition_call_args(_, _, [_ | _], [], _, _, _) :-
	error("common__partition_call_args").
common__partition_call_args(_, _, [], [], [], [], []).
common__partition_call_args(VarTypes, ModuleInfo, [ArgMode | ArgModes],
		[Arg | Args], InputArgs, OutputArgs, OutputModes) :-
	common__partition_call_args(VarTypes, ModuleInfo, ArgModes, Args,
		InputArgs1, OutputArgs1, OutputModes1),
	mode_get_insts(ModuleInfo, ArgMode, InitialInst, FinalInst),
	map__lookup(VarTypes, Arg, Type),
	( inst_matches_binding(InitialInst, FinalInst, Type, ModuleInfo) ->
		InputArgs = [Arg | InputArgs1],
		OutputArgs = OutputArgs1,
		OutputModes = OutputModes1
	; 
		% Calls with partly unique outputs cannot be replaced,
		% since a unique copy of the outputs must be produced.
		inst_is_not_partly_unique(ModuleInfo, FinalInst),

		% Don't optimize calls whose outputs include any
		% `any' insts, since that would create false aliasing
		% between the different variables.
		% (inst_matches_binding applied to identical insts
		% fails only for `any' insts.)
		inst_matches_binding(FinalInst, FinalInst, Type, ModuleInfo),

		% Don't optimize calls where a partially instantiated
		% variable is further instantiated. That case is difficult
		% to test properly because mode analysis currently
		% rejects most potential test cases.
		inst_is_free(ModuleInfo, InitialInst),

		InputArgs = InputArgs1,
		OutputArgs = [Arg | OutputArgs1],
		OutputModes = [ArgMode | OutputModes1]
	).

%---------------------------------------------------------------------------%

:- pred common__find_previous_call(list(call_args)::in, list(prog_var)::in,
		eqvclass(prog_var)::in, list(prog_var)::out,
		prog_context::out) is semidet.

common__find_previous_call([SeenCall | SeenCalls], InputArgs,
		Eqv, OutputArgs2, PrevContext) :-
	SeenCall = call_args(PrevContext, InputArgs1, OutputArgs1),
	( common__var_lists_are_equiv(InputArgs, InputArgs1, Eqv) ->
		OutputArgs2 = OutputArgs1
	;
		common__find_previous_call(SeenCalls, InputArgs, Eqv,
			OutputArgs2, PrevContext)
	).

%---------------------------------------------------------------------------%

:- pred common__create_output_unifications(hlds_goal_info::in, 
		list(prog_var)::in, list(prog_var)::in, list(uni_mode)::in,
		list(hlds_goal)::out, simplify_info::in,
		simplify_info::out) is det.

	% Create unifications to assign the vars in OutputArgs from 
	% the corresponding var in OutputArgs2.
	% This needs to be done even if OutputArg is not a nonlocal in
	% the original goal because later goals in the conjunction may
	% match against the cell and need all the output arguments.
	% The unneeded assignments will be removed later.

common__create_output_unifications(GoalInfo, OutputArgs, OldOutputArgs,
		UniModes, Goals, Info0, Info) :-
	(
		OutputArgs = [OutputArg | OutputArgs1],
		OldOutputArgs = [OldOutputArg | OldOutputArgs1],
		UniModes = [UniMode | UniModes1]
	->	
		( 
			% This can happen if the first cell was created
			% with a partially instantiated deconstruction.
			OutputArg \= OldOutputArg
		->
			common__generate_assign(OutputArg, OldOutputArg,
				UniMode, GoalInfo, Goal, Info0, Info1),
			common__create_output_unifications(GoalInfo,
				OutputArgs1, OldOutputArgs1, UniModes1,
				Goals1, Info1, Info),
			Goals = [Goal | Goals1]
		;
			common__create_output_unifications(GoalInfo,
				OutputArgs1, OldOutputArgs1, UniModes1, Goals,
				Info0, Info)
		)
	;
		OutputArgs = [],
		OldOutputArgs = [],
		UniModes = []
	->
		Goals = [],
		Info = Info0
	;
		error("common__create_output_unifications: mode mismatch")
	).


%---------------------------------------------------------------------------%

:- pred common__generate_assign(prog_var, prog_var, uni_mode,
		hlds_goal_info, hlds_goal, simplify_info, simplify_info).
:- mode common__generate_assign(in, in, in, in, out, in, out) is det.
	
common__generate_assign(ToVar, FromVar, UniMode,
		GoalInfo0, Goal, Info0, Info) :-
	goal_info_get_instmap_delta(GoalInfo0, InstMapDelta0),
	simplify_info_get_var_types(Info0, VarTypes),
	map__lookup(VarTypes, ToVar, ToVarType),
	map__lookup(VarTypes, FromVar, FromVarType),

	set__list_to_set([ToVar, FromVar], NonLocals),
	( common__types_match_exactly(ToVarType, FromVarType) ->
		UniMode = ((_ - ToVarInst0) -> (_ - ToVarInst)),
		UnifyContext = unify_context(explicit, []),
		UnifyMode = (ToVarInst0 -> ToVarInst) -
				(ToVarInst -> ToVarInst),
		GoalExpr = unify(ToVar, var(FromVar), UnifyMode,
			assign(ToVar, FromVar), UnifyContext),
		instmap_delta_from_assoc_list([ToVar - ToVarInst],
			InstMapDelta)
	;	
		% If the cells we are optimizing don't have exactly the same
		% type, we insert explicit type casts to ensure type
		% correctness. This avoids problems with HLDS optimizations
		% such as inlining which expect the HLDS to be well-typed.
		% Unfortunately this loses information for other optimizations,
		% since the call to the type cast hides the equivalence of
		% the input and output.
		simplify_info_get_module_info(Info0, ModuleInfo),
		module_info_get_predicate_table(ModuleInfo, PredTable),
		mercury_private_builtin_module(MercuryBuiltin),
		TypeCast = qualified(MercuryBuiltin, "unsafe_type_cast"),
		(
			predicate_table_search_pred_sym_arity(
				PredTable, TypeCast, 2, [PredId])
		->
			hlds_pred__initial_proc_id(ProcId),
			GoalExpr = call(PredId, ProcId, [FromVar, ToVar],
				inline_builtin, no, TypeCast)
		;
			error("common__generate_assign: \
				can't find unsafe_type_cast")
		),
		instmap_delta_restrict(InstMapDelta0, NonLocals, InstMapDelta)
	),
	goal_info_init(NonLocals, InstMapDelta, det, GoalInfo),
	Goal = GoalExpr - GoalInfo,	
	common__record_equivalence(ToVar, FromVar, Info0, Info).

:- pred common__types_match_exactly((type), (type)).
:- mode common__types_match_exactly(in, in) is semidet.

common__types_match_exactly(term__variable(Var), term__variable(Var)).
common__types_match_exactly(Type1, Type2) :-
	type_to_ctor_and_args(Type1, TypeCtor1, Args1),
	type_to_ctor_and_args(Type2, TypeCtor2, Args2),
	TypeCtor1 = TypeCtor2,
	common__types_match_exactly_list(Args1, Args2).

:- pred common__types_match_exactly_list(list(type), list(type)).
:- mode common__types_match_exactly_list(in, in) is semidet.

common__types_match_exactly_list([], []).
common__types_match_exactly_list([Type1 | Types1], [Type2 | Types2]) :-
	common__types_match_exactly(Type1, Type2),
	common__types_match_exactly_list(Types1, Types2).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
