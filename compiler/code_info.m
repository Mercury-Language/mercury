%---------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% file: code_info.m
%
% main author: conway.
%
% This file defines the code_info type and various operations on it.
%
% The code_info structure is a 'state' used by the code generator.
%
% The following assumptions are made about the state of the high-level
% data structure:
%	o  Variables can be stored in any number of distinct places.
%	o  Registers may contain a value corresponding to more than
%		one variable.
%	o  Procedures are in superhomogeneous form. This means that
%		construction unifications and builtins are not nested.
%	o  Evaluation of arguments in construction and deconstruction
%		unifications is lazy. This means that arguments in a
%		`don't care' mode are ignored, and that assignments
%		are cached.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module code_info.

:- interface.

:- import_module hlds, llds.
:- import_module code_util, tree, set, std_util, globals, unify_proc.

:- type code_info.

		% Create a new code_info structure.
:- pred code_info__init(varset, liveness_info, call_info, bool, globals,
			pred_id, proc_id, proc_info, code_model,
			instmap, follow_vars, module_info, code_info).
:- mode code_info__init(in, in, in, in, in, in, in, in, in, in, in, in, out)
			is det.

		% Generate the next local label in sequence.
		% Bool option is if the label can be accessed externally.
:- pred code_info__get_next_label(label, bool, code_info, code_info).
:- mode code_info__get_next_label(out, in, in, out) is det.

		% Generate the next local label number in sequence.
:- pred code_info__get_next_label_number(int, code_info, code_info).
:- mode code_info__get_next_label_number(out, in, out) is det.

		% Get the variables for the current procedure.
:- pred code_info__get_varset(varset, code_info, code_info).
:- mode code_info__get_varset(out, in, out) is det.

		% Set the variables for the current procedure.
:- pred code_info__set_varset(varset, code_info, code_info).
:- mode code_info__set_varset(in, in, out) is det.

		% Get the hlds mapping from variables to
		% stack slots
:- pred code_info__get_call_info(call_info, code_info, code_info).
:- mode code_info__get_call_info(out, in, out) is det.

		% Set the hlds mapping from variables to
		% stack slots
:- pred code_info__set_call_info(call_info, code_info, code_info).
:- mode code_info__set_call_info(in, in, out) is det.

:- pred code_info__get_pred_id(pred_id, code_info, code_info).
:- mode code_info__get_pred_id(out, in, out) is det.

:- pred code_info__set_pred_id(pred_id, code_info, code_info).
:- mode code_info__set_pred_id(in, in, out) is det.

:- pred code_info__get_proc_id(proc_id, code_info, code_info).
:- mode code_info__get_proc_id(out, in, out) is det.

:- pred code_info__set_proc_id(proc_id, code_info, code_info).
:- mode code_info__set_proc_id(in, in, out) is det.

:- pred code_info__set_module_info(module_info, code_info, code_info).
:- mode code_info__set_module_info(in, in, out) is det.

:- pred code_info__cache_expression(var, rval, code_info, code_info).
:- mode code_info__cache_expression(in, in, in, out) is det.

		% Generate code to either setup the input arguments for a call
		% (i.e. in the caller), or to setup the output arguments in the
		% predicate epilog (i.e. in the callee).

:- type call_direction ---> caller ; callee.

:- pred code_info__setup_call(assoc_list(var, arg_info),
			call_direction, code_tree, code_info, code_info).
:- mode code_info__setup_call(in, in, out, in, out) is det.

:- pred code_info__save_variable_on_stack(var, code_tree,
							code_info, code_info).
:- mode code_info__save_variable_on_stack(in, out, in, out) is det.

		% Succeed if the given variable is live at the
		% end of this goal.
:- pred code_info__variable_is_live(var, code_info, code_info).
:- mode code_info__variable_is_live(in, in, out) is semidet.

		% Get the call argument info for a given mode
		% of a given predicate
:- pred code_info__get_pred_proc_arginfo(pred_id, proc_id, list(arg_info),
							code_info, code_info).
:- mode code_info__get_pred_proc_arginfo(in, in, out, in, out) is det.

		% Get the call argument information for the
		% current procedure
:- pred code_info__get_arginfo(list(arg_info), code_info, code_info).
:- mode code_info__get_arginfo(out, in, out) is det.

:- pred code_info__get_headvars(list(var), code_info, code_info).
:- mode code_info__get_headvars(out, in, out) is det.

:- pred code_info__cons_id_to_tag(var, cons_id, cons_tag,
							code_info, code_info).
:- mode code_info__cons_id_to_tag(in, in, out, in, out) is det.

:- pred code_info__get_total_stackslot_count(int, code_info, code_info).
:- mode code_info__get_total_stackslot_count(out, in, out) is det.

:- pred code_info__clear_all_registers(code_info, code_info).
:- mode code_info__clear_all_registers(in, out) is det.

:- pred code_info__get_live_variables(list(var), code_info, code_info).
:- mode code_info__get_live_variables(out, in, out) is det.

:- pred code_info__generate_forced_saves(code_tree, code_info, code_info).
:- mode code_info__generate_forced_saves(out, in, out) is det.

:- pred code_info__grab_code_info(code_info, code_info, code_info).
:- mode code_info__grab_code_info(out, in, out) is det.

:- pred code_info__slap_code_info(code_info, code_info, code_info).
:- mode code_info__slap_code_info(in, in, out) is det.

:- pred code_info__get_proc_info(proc_info, code_info, code_info).
:- mode code_info__get_proc_info(out, in, out) is det.

:- pred code_info__get_succip_used(bool, code_info, code_info).
:- mode code_info__get_succip_used(out, in, out) is det.

:- pred code_info__set_succip_used(bool, code_info, code_info).
:- mode code_info__set_succip_used(in, in, out) is det.

:- pred code_info__remake_with_store_map(code_info, code_info).
:- mode code_info__remake_with_store_map(in, out) is det.

:- pred code_info__remake_with_call_info(code_info, code_info).
:- mode code_info__remake_with_call_info(in, out) is det.

:- pred code_info__update_liveness_info(delta_liveness, code_info, code_info).
:- mode code_info__update_liveness_info(in, in, out) is det.

:- pred code_info__update_deadness_info(delta_liveness, code_info, code_info).
:- mode code_info__update_deadness_info(in, in, out) is det.

:- pred code_info__set_var_location(var, lval, code_info, code_info).
:- mode code_info__set_var_location(in, in, in, out) is det.

:- pred code_info__produce_variable(var, code_tree, rval, code_info, code_info).
:- mode code_info__produce_variable(in, out, out, in, out) is det.

:- pred code_info__place_var(var, lval, code_tree, code_info, code_info).
:- mode code_info__place_var(in, in, out, in, out) is det.

:- pred code_info__make_vars_dead(set(var), code_info, code_info).
:- mode code_info__make_vars_dead(in, in, out) is det.

:- pred code_info__make_vars_live(set(var), code_info, code_info).
:- mode code_info__make_vars_live(in, in, out) is det.

:- pred code_info__acquire_reg(reg, code_info, code_info).
:- mode code_info__acquire_reg(out, in, out) is det.

:- pred code_info__release_reg(reg, code_info, code_info).
:- mode code_info__release_reg(in, in, out) is det.

:- pred code_info__get_module_info(module_info, code_info, code_info).
:- mode code_info__get_module_info(out, in, out) is det.

:- type failure_cont
	--->	known(label)	% on failure we jump to `Label'
	;	do_fail		% on failure we do a `fail()'
	;	unknown.	% on failure we do a `redo()'

	% push a new failure continuation onto the stack

:- pred code_info__push_failure_cont(failure_cont, code_info, code_info).
:- mode code_info__push_failure_cont(in, in, out) is det.

	% pop the failure continuation stack

:- pred code_info__pop_failure_cont(code_info, code_info).
:- mode code_info__pop_failure_cont(in, out) is det.

	% lookup the value on the top of the failure continuation stack

:- pred code_info__failure_cont(failure_cont, code_info, code_info).
:- mode code_info__failure_cont(out, in, out) is det.

:- pred code_info__failure_cont_address(failure_cont, code_addr).
:- mode code_info__failure_cont_address(in, out) is det.

	% generate some code to restore the current redoip, by looking
	% at the top of the failure continuation stack.

:- pred code_info__restore_failure_cont(code_tree, code_info, code_info).
:- mode code_info__restore_failure_cont(out, in, out) is det.

:- pred code_info__stack_variable(int, lval, code_info, code_info).
:- mode code_info__stack_variable(in, out, in, out) is det.

:- pred code_info__generate_nondet_saves(code_tree, code_info, code_info).
:- mode code_info__generate_nondet_saves(out, in, out) is det.

:- pred code_info__generate_failure(code_tree, code_info, code_info).
:- mode code_info__generate_failure(out, in, out) is det.

:- pred code_info__generate_test_and_fail(rval, code_tree,
							code_info, code_info).
:- mode code_info__generate_test_and_fail(in, out, in, out) is det.

:- pred code_info__generate_pre_commit(code_tree, label, code_info, code_info).
:- mode code_info__generate_pre_commit(out, out, in, out) is det.

:- pred code_info__generate_commit(label, code_tree, code_info, code_info).
:- mode code_info__generate_commit(in, out, in, out) is det.

:- pred code_info__save_hp(code_tree, code_info, code_info).
:- mode code_info__save_hp(out, in, out) is det.

:- pred code_info__restore_hp(code_tree, code_info, code_info).
:- mode code_info__restore_hp(out, in, out) is det.

:- pred code_info__save_redoip(code_tree, code_info, code_info).
:- mode code_info__save_redoip(out, in, out) is det.

:- pred code_info__restore_redoip(code_tree, code_info, code_info).
:- mode code_info__restore_redoip(out, in, out) is det.

:- pred code_info__get_old_hp(code_tree, code_info, code_info).
:- mode code_info__get_old_hp(out, in, out) is det.

:- pred code_info__pop_stack(code_tree, code_info, code_info).
:- mode code_info__pop_stack(out, in, out) is det.

:- pred code_info__maybe_save_hp(bool, code_tree, code_info, code_info).
:- mode code_info__maybe_save_hp(in, out, in, out) is det.

:- pred code_info__maybe_restore_hp(bool, code_tree, code_info, code_info).
:- mode code_info__maybe_restore_hp(in, out, in, out) is det.

:- pred code_info__maybe_get_old_hp(bool, code_tree, code_info, code_info).
:- mode code_info__maybe_get_old_hp(in, out, in, out) is det.

:- pred code_info__maybe_pop_stack(bool, code_tree, code_info, code_info).
:- mode code_info__maybe_pop_stack(in, out, in, out) is det.

:- pred code_info__get_globals(globals, code_info, code_info).
:- mode code_info__get_globals(out, in, out) is det.

:- pred code_info__set_globals(globals, code_info, code_info).
:- mode code_info__set_globals(in, in, out) is det.

:- pred code_info__push_store_map(map(var, lval), code_info, code_info).
:- mode code_info__push_store_map(in, in, out) is det.

:- pred code_info__pop_store_map(code_info, code_info).
:- mode code_info__pop_store_map(in, out) is det.

:- pred code_info__current_store_map(map(var, lval), code_info, code_info).
:- mode code_info__current_store_map(out, in, out) is det.

:- pred code_info__variable_type(var, type, code_info, code_info).
:- mode code_info__variable_type(in, out, in, out) is det.

:- pred code_info__get_requests(unify_requests, code_info, code_info).
:- mode code_info__get_requests(out, in, out) is det.

:- pred code_info__generate_stack_livevals(set(var), set(lval),
						code_info, code_info).
:- mode code_info__generate_stack_livevals(in, out, in, out) is det.

:- pred code_info__generate_stack_livelvals(set(var), list(liveinfo),
						code_info, code_info).
:- mode code_info__generate_stack_livelvals(in, out, in, out) is det.

:- pred code_info__variable_to_string(var, string, code_info, code_info).
:- mode code_info__variable_to_string(in, out, in, out) is det.

:- pred code_info__apply_instmap_delta(instmap_delta, code_info, code_info).
:- mode code_info__apply_instmap_delta(in, in, out) is det.

:- pred code_info__get_instmap(instmap, code_info, code_info).
:- mode code_info__get_instmap(out, in, out) is det.

:- pred code_info__set_instmap(instmap, code_info, code_info).
:- mode code_info__set_instmap(in, in, out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
:- implementation.

:- import_module string, require, char, list, map, bimap, tree, int.
:- import_module code_exprn, set, varset, term, stack, prog_io.
:- import_module type_util, mode_util, options, shapes.

:- pred code_info__get_label_count(int, code_info, code_info).
:- mode code_info__get_label_count(out, in, out) is det.

:- pred code_info__set_label_count(int, code_info, code_info).
:- mode code_info__set_label_count(in, in, out) is det.

:- type junk		---> junk.

:- type code_info	--->
		code_info(
			int,		% The number of stack slots allocated.
					% for storing variables.
					% (Some extra stack slots are used
					% for saving and restoring registers.)
			int,		% Counter for the local labels used
					% by this procedure.
			varset,		% The variables in this procedure.
			call_info,	% The storage allocations for the
					% live variables at the end of the
					% current switch.
			pred_id,	% The label of the current predicate.
			proc_id,	% The label of the current procedure.
			junk,		% JUNK
					% what is stored in each register.
			exprn_info,	% A map storing the information about
					% the status of each variable.
			proc_info,	% The proc_info for the this procedure.
			bool,		% do we need to store succip?
			fall_through,	% The failure continuation stack
			module_info,	% The module_info structure - you just
					% never know when you might need it.
			liveness_info,	% Variables that are live
					% after this goal
			stack(map(var, lval)),
					% Store Map - where to put things
			code_model,	% The model of the current procedure
			instmap,	% insts of variables
			pair(int),	% The current and maximum (respectively)
					% number of extra temporary stackslots
					% that have been pushed during the
					% procedure
			globals,	% code generation options
			stack(pair(lval))% the locations in use on the stack
	).

:- type fall_through	==	stack(failure_cont).

%---------------------------------------------------------------------------%

code_info__init(Varset, Liveness, CallInfo, SaveSuccip, Globals,
					PredId, ProcId, ProcInfo, CodeModel,
					Requests, _FollowVars, ModuleInfo, C) :-
	proc_info_headvars(ProcInfo, HeadVars),
	proc_info_arg_info(ProcInfo, ArgInfos),
	assoc_list__from_corresponding_lists(HeadVars, ArgInfos, Args),
	code_info__build_input_arg_list(Args, ArgList),
	globals__get_options(Globals, Options),
	code_exprn__init_state(ArgList, Varset, Options, ExprnInfo),
	stack__init(Continue),
	stack__init(StoreMapStack0),
	stack__init(PushedVals0),
	map__init(StoreMap),
	stack__push(StoreMapStack0, StoreMap, StoreMapStack),
	code_info__max_slot(CallInfo, SlotCount0),
	(
		CodeModel = model_non
	->
		SlotCount is SlotCount0 + 1
	;
		SlotCount = SlotCount0
	),
	C = code_info(
		SlotCount,
		0,
		Varset,
		CallInfo, 
		PredId,
		ProcId,
		junk,
		ExprnInfo,
		ProcInfo,
		SaveSuccip,
		Continue,
		ModuleInfo,
		Liveness,
		StoreMapStack,
		CodeModel,
		Requests,
		0 - 0,
		Globals,
		PushedVals0
	).

:- pred code_info__build_input_arg_list(assoc_list(var, arg_info),
						assoc_list(var, lval)).
:- mode code_info__build_input_arg_list(in, out) is det.

code_info__build_input_arg_list([], []).
code_info__build_input_arg_list([V - Arg|Rest0], VarArgs) :-
	Arg = arg_info(Loc, Mode),
	(
		Mode = top_in
	->
		code_util__arg_loc_to_register(Loc, Reg),
		VarArgs = [V - reg(Reg)|VarArgs0]
	;
		VarArgs = VarArgs0
	),
	code_info__build_input_arg_list(Rest0, VarArgs0).

%---------------------------------------------------------------------------%

:- pred code_info__max_slot(call_info, int).
:- mode code_info__max_slot(in, out) is det.

code_info__max_slot(CallInfo, SlotCount) :-
	map__values(CallInfo, CallList),
	code_info__max_slot_2(CallList, 0, SlotCount).

:- pred code_info__max_slot_2(list(lval), int, int).
:- mode code_info__max_slot_2(in, in, out) is det.

code_info__max_slot_2([], Max, Max).
code_info__max_slot_2([L|Ls], Max0, Max) :-
	(
		L = stackvar(N)
	->
		int__max(N, Max0, Max1)
	;
		L = framevar(N)
	->
		int__max(N, Max0, Max1)
	;
		Max1 = Max0
	),
	code_info__max_slot_2(Ls, Max1, Max).

%---------------------------------------------------------------------------%

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

code_info__get_next_label(Label, Cont0) -->
	code_info__get_pred_id(PredId),
	code_info__get_proc_id(ProcId),
	code_info__get_next_label_number(N),
	code_info__get_module_info(ModuleInfo),
	{ module_info_pred_info(ModuleInfo, PredId, PredInfo) },
	(
		{ Cont0 = yes }
	->
		(
			{ pred_info_is_exported(PredInfo) }
		->
			{ Cont = exported }
		;
			{ Cont = local }
		)
	;
		{ Cont = no }
	),
	{ code_util__make_local_label(ModuleInfo, PredId, ProcId, N, Cont,
		Label) 
	}.

code_info__get_next_label_number(N) -->
	code_info__get_label_count(N0),
	{ N is N0 + 1 },
	code_info__set_label_count(N).

%---------------------------------------------------------------------------%

	% XXX We could use the sanity checking mechanism...
code_info__clear_all_registers -->
	code_info__get_exprn_info(Exprn0),
	{ code_exprn__clobber_regs([], Exprn0, Exprn) },
	code_info__set_exprn_info(Exprn).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- pred code_info__get_variable_slot(var, lval, code_info, code_info).
:- mode code_info__get_variable_slot(in, out, in, out) is det.

code_info__get_variable_slot(Var, Slot) -->
	code_info__get_call_info(CallInfo),
	{ map__lookup(CallInfo, Var, Slot) }.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

	% Given a constructor id, and a variable (so that we can work out the
	% type of the constructor), determine correct tag (representation)
	% of that constructor.

:- code_info__cons_id_to_tag(_, X, _, _, _) when X. % NU-Prolog indexing.

code_info__cons_id_to_tag(_Var, int_const(X), int_constant(X)) --> [].
code_info__cons_id_to_tag(_Var, float_const(X), float_constant(X)) --> [].
code_info__cons_id_to_tag(_Var, string_const(X), string_constant(X)) --> [].
code_info__cons_id_to_tag(_Var, address_const(P,M), address_constant(P,M))
	--> [].
code_info__cons_id_to_tag(_Var, pred_const(P,M), pred_closure_tag(P,M)) --> [].
code_info__cons_id_to_tag(Var, cons(Name, Arity), Tag) -->
		%
		% Lookup the type of the variable
		%
	code_info__variable_type(Var, Type),
	(
		% handle the `character' type specially
		{ Type = term__functor(term__atom("character"), [], _) },
	 	{ string__char_to_string(Char, Name) }
	->
		{ char_to_int(Char, CharCode) },
		{ Tag = int_constant(CharCode) }
	;
		% handle higher-order pred types specially
		{ Type = term__functor(term__atom("pred"), PredArgTypes, _) }
	->
		{ list__length(PredArgTypes, PredArity) },
		code_info__get_module_info(ModuleInfo),
		{ module_info_get_predicate_table(ModuleInfo, PredicateTable) },
		{ TotalArity is Arity + PredArity },
		{
		    predicate_table_search_name_arity(PredicateTable,
			Name, TotalArity, PredIds)
		->
		    (
			PredIds = [PredId]
		    ->
			predicate_table_get_preds(PredicateTable, Preds),
			map__lookup(Preds, PredId, PredInfo),
			pred_info_procedures(PredInfo, Procs),
			map__keys(Procs, ProcIds),
			(
			    ProcIds = [ProcId]
			->
			    Tag = pred_closure_tag(PredId, ProcId)
			;
			    error("sorry, not implemented: taking address of predicate with multiple modes")
			)
		    ;
			% cons_id ought to include the module prefix, so
			% that we could use predicate_table__search_m_n_a to 
			% prevent this from happening
			error("code_info__cons_id_to_tag: ambiguous pred")
		    )
		;
		    % the type-checker should ensure that this never happens
		    error("code_info__cons_id_to_tag: invalid pred")
		}
	;
			%
			% Use the type to determine the type_id
			%
		{ type_to_type_id(Type, TypeId0, _) ->
			TypeId = TypeId0
		;
			% the type-checker should ensure that this never happens
			error("code_info__cons_id_to_tag: invalid type")
		},

			%
			% Given the type_id, lookup up the constructor tag
			% table for that type
			%
		code_info__get_module_info(ModuleInfo),
		{ module_info_types(ModuleInfo, TypeTable) },
		{ map__lookup(TypeTable, TypeId, TypeDefn) },
		{
			TypeDefn = hlds__type_defn(_, _,
				du_type(_, ConsTable0, _), _, _)
		->
			ConsTable = ConsTable0
		;
			% this should never happen
			error(
			"code_info__cons_id_to_tag: type is not d.u. type?"
			)
		},
			% Finally look up the cons_id in the table
		{ map__lookup(ConsTable, cons(Name, Arity), Tag) }
	).

%---------------------------------------------------------------------------%

code_info__get_live_variables(VarList) -->
	code_info__get_liveness_info(Vars),
	{ set__to_sorted_list(Vars, VarList) }.

%---------------------------------------------------------------------------%

code_info__variable_is_live(Var) -->
	code_info__get_liveness_info(Liveness),
	{ set__member(Var, Liveness) }.

%---------------------------------------------------------------------------%

code_info__get_pred_proc_arginfo(PredId, ProcId, ArgInfo) -->
	code_info__get_module_info(ModuleInfo),
	{ module_info_preds(ModuleInfo, Preds) },
	{ map__lookup(Preds, PredId, PredInfo) },
	{ pred_info_procedures(PredInfo, Procs) },
	{ map__lookup(Procs, ProcId, ProcInfo) },
	{ proc_info_arg_info(ProcInfo, ArgInfo) }.

%---------------------------------------------------------------------------%

code_info__set_var_location(Var, Lval) -->
	code_info__get_exprn_info(Exprn0),
	{ code_exprn__set_var_location(Var, Lval, Exprn0, Exprn) },
	code_info__set_exprn_info(Exprn).

%---------------------------------------------------------------------------%

code_info__produce_variable(Var, Code, Rval) -->
	code_info__get_exprn_info(Exprn0),
	{ code_exprn__produce_var(Var, Rval, Code, Exprn0, Exprn) },
	code_info__set_exprn_info(Exprn).

%---------------------------------------------------------------------------%

code_info__place_var(Var, Lval, Code) -->
	code_info__get_exprn_info(Exprn0),
	{ code_exprn__place_var(Var, Lval, Code, Exprn0, Exprn) },
	code_info__set_exprn_info(Exprn).

%---------------------------------------------------------------------------%

code_info__acquire_reg(Reg) -->
	code_info__get_exprn_info(Exprn0),
	{ code_exprn__acquire_reg(Reg, Exprn0, Exprn) },
	code_info__set_exprn_info(Exprn).

%---------------------------------------------------------------------------%

code_info__release_reg(Reg) -->
	code_info__get_exprn_info(Exprn0),
	{ code_exprn__release_reg(Reg, Exprn0, Exprn) },
	code_info__set_exprn_info(Exprn).

%---------------------------------------------------------------------------%

code_info__cache_expression(Var, Rval) -->
	code_info__get_exprn_info(Exprn0),
	{ code_exprn__cache_exprn(Var, Rval, Exprn0, Exprn) },
	code_info__set_exprn_info(Exprn).

%---------------------------------------------------------------------------%

code_info__setup_call([], _Direction, empty) --> [].
code_info__setup_call([V - arg_info(Loc,Mode)|Rest], Direction, Code) -->
	(
		{
			Mode = top_in,
			Direction = caller
		;
			Mode = top_out,
			Direction = callee
		}
	->
		{ code_util__arg_loc_to_register(Loc, Reg) },
		code_info__get_exprn_info(Exprn0),
		{ code_exprn__place_var(V, reg(Reg), Code0, Exprn0, Exprn1) },
			% We need to test that either the variable
			% is live OR it occurs in the remaining arguments
			% because of a bug in polymorphism.m which
			% causes some compiler generated code to violate
			% superhomogeneous form
		(
			code_info__variable_is_live(V)
		->
			{ IsLive = yes }
		;
			{ IsLive = no }
		),
		{
			list__member(Vtmp - _, Rest),
			V = Vtmp
		->
			Occurs = yes
		;
			Occurs = no
		},
		(
				% We can't simply use a disj here
				% because of bugs in modes/det_analysis
			{ bool__or(Occurs, IsLive, yes) }
		->
			{ code_exprn__lock_reg(Reg, Exprn1, Exprn2) },
			code_info__set_exprn_info(Exprn2),
			code_info__setup_call(Rest, Direction, Code1),
			code_info__get_exprn_info(Exprn3),
			{ code_exprn__unlock_reg(Reg, Exprn3, Exprn) },
			code_info__set_exprn_info(Exprn),
			{ Code = tree(Code0, Code1) }
		;
			{ code_exprn__lock_reg(Reg, Exprn1, Exprn2) },
			{ code_exprn__var_becomes_dead(V, Exprn2, Exprn3) },
			code_info__set_exprn_info(Exprn3),
			code_info__setup_call(Rest, Direction, Code1),
			code_info__get_exprn_info(Exprn4),
			{ code_exprn__unlock_reg(Reg, Exprn4, Exprn) },
			code_info__set_exprn_info(Exprn),
			{ Code = tree(Code0, Code1) }
		)
	;
		code_info__setup_call(Rest, Direction, Code)
	).

%---------------------------------------------------------------------------%

code_info__save_variable_on_stack(Var, Code) -->
	code_info__get_variable_slot(Var, Slot),
	code_info__get_exprn_info(Exprn0),
	{ code_exprn__place_var(Var, Slot, Code, Exprn0, Exprn) },
	code_info__set_exprn_info(Exprn).

%---------------------------------------------------------------------------%

code_info__get_arginfo(ArgInfo) -->
	code_info__get_pred_id(PredId),
	code_info__get_proc_id(ProcId),
	code_info__get_pred_proc_arginfo(PredId, ProcId, ArgInfo).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

code_info__generate_forced_saves(Code) --> 
	code_info__current_store_map(StoreMap),
	code_info__get_call_info(CallInfo),
	code_info__get_live_variables(Vars),
	code_info__generate_forced_saves_2(Vars, StoreMap, CallInfo, Code).

:- pred code_info__generate_forced_saves_2(list(var), map(var, lval),
			map(var, lval), code_tree, code_info, code_info).
:- mode code_info__generate_forced_saves_2(in, in, in, out, in, out) is det.

code_info__generate_forced_saves_2([], _Store, _Call, empty) --> [].
code_info__generate_forced_saves_2([V|Vs], Store, Call, Code) -->
	(
		{ map__search(Store, V, Lval) }
	->
		code_info__get_exprn_info(Exprn0),
		{ code_exprn__place_var(V, Lval, Code0, Exprn0, Exprn) },
		code_info__set_exprn_info(Exprn)
	;
		{ map__search(Call, V, Lval) }
	->
		code_info__get_exprn_info(Exprn0),
		{ code_exprn__place_var(V, Lval, Code0, Exprn0, Exprn) },
		code_info__set_exprn_info(Exprn)
	;
		{ error("code_info__generate_forced_saves: variable not found") }
	),
	code_info__generate_forced_saves_2(Vs, Store, Call, Code1),
	{ Code = tree(Code0, Code1) }.

%---------------------------------------------------------------------------%

code_info__generate_nondet_saves(Code) -->
	code_info__get_call_info(CallInfo),
	{ map__to_assoc_list(CallInfo, CallList) },
	code_info__generate_nondet_saves_2(CallList, Code).

:- pred code_info__generate_nondet_saves_2(assoc_list(var, lval), code_tree,
							code_info, code_info).
:- mode code_info__generate_nondet_saves_2(in, out, in, out) is det.

code_info__generate_nondet_saves_2([], empty) --> [].
code_info__generate_nondet_saves_2([Var - StackThing|VarSlots], Code) --> 
	(
		code_info__variable_is_live(Var)
	->
		code_info__get_exprn_info(Exprn0),
		{ code_exprn__place_var(Var, StackThing, Code0,
							Exprn0, Exprn) },
		code_info__set_exprn_info(Exprn)
	;
		{ Code0 = empty }
	),
	{ Code = tree(Code0, Code1) },
	code_info__generate_nondet_saves_2(VarSlots, Code1).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

code_info__get_headvars(HeadVars) -->
	code_info__get_module_info(ModuleInfo),
	code_info__get_pred_id(PredId),
	code_info__get_proc_id(ProcId),
	{ module_info_preds(ModuleInfo, Preds) },
	{ map__lookup(Preds, PredId, PredInfo) },
	{ pred_info_procedures(PredInfo, Procs) },
	{ map__lookup(Procs, ProcId, ProcInfo) },
	{ proc_info_headvars(ProcInfo, HeadVars) }.

%---------------------------------------------------------------------------%

% code_info__remake_with_store_map rebuilds the register info and
% variable info data structures. It operates under the assumption
% that if a variable is live, it is stored somewhere. What is more,
% it assumes that if a variable is live, then there is a copy of it
% stored on the stack in the location given by the store map. This
% means that it only makes sense to call this predicate in situations
% such as the start of disjuncts where it only makes sense to have
% variables on the stack. (Note that disj_gen contains a piece of
% magic to use the register versions of variables if possible in the
% first disjunct).

code_info__remake_with_store_map -->
	code_info__get_varset(Varset),
	code_info__get_live_variables(VarList),
	{ set__list_to_set(VarList, Vars) },
	code_info__get_call_info(CallInfo),
	code_info__current_store_map(StoreMap),
	{ map__overlay(CallInfo, StoreMap, LvalMap0) },
	{ map__select(LvalMap0, Vars, LvalMap) },
	{ map__to_assoc_list(LvalMap, VarLvals) },
	code_info__get_globals(Globals),
	{ globals__get_options(Globals, Options) },
	{ code_exprn__init_state(VarLvals, Varset, Options, Exprn) },
	code_info__set_exprn_info(Exprn).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

% code_info__remake_with_call_info rebuilds the register info and the
% variable info structures. It operates under the assumption that if
% a variable is live, then it should be located in the storage indicated
% by the call info information. This is for use at the end of the branches
% in branched structures, where we need to ensure that variables are
% stored in consistient places.

code_info__remake_with_call_info -->
	code_info__get_varset(Varset),
	code_info__get_live_variables(VarList),
	{ set__list_to_set(VarList, Vars) },
	code_info__get_call_info(CallInfo),
	{ map__select(CallInfo, Vars, LvalMap) },
	{ map__to_assoc_list(LvalMap, VarLvals) },
	code_info__get_globals(Globals),
	{ globals__get_options(Globals, Options) },
	{ code_exprn__init_state(VarLvals, Varset, Options, Exprn) },
	code_info__set_exprn_info(Exprn).

%---------------------------------------------------------------------------%

code_info__update_liveness_info(Births - _Deaths) -->
	code_info__get_liveness_info(Liveness0),
	{ set__union(Liveness0, Births, Liveness) },
	code_info__set_liveness_info(Liveness).

code_info__update_deadness_info(_Births - Deaths) -->
	code_info__get_liveness_info(Liveness0),
	{ set__difference(Liveness0, Deaths, Liveness) },
	code_info__set_liveness_info(Liveness).

%---------------------------------------------------------------------------%

code_info__make_vars_dead(Vars) -->
	{ set__to_sorted_list(Vars, VarList) },
	code_info__make_vars_dead_2(VarList).

:- pred code_info__make_vars_dead_2(list(var), code_info, code_info).
:- mode code_info__make_vars_dead_2(in, in, out) is det.

code_info__make_vars_dead_2([]) --> [].
code_info__make_vars_dead_2([V|Vs]) -->
	code_info__get_exprn_info(Exprn0),
	{ code_exprn__var_becomes_dead(V, Exprn0, Exprn) },
	code_info__set_exprn_info(Exprn),
	code_info__make_vars_dead_2(Vs).

%---------------------------------------------------------------------------%

code_info__make_vars_live(Vars) -->
	{ set__to_sorted_list(Vars, VarList) },
	code_info__make_vars_live_2(VarList).

:- pred code_info__make_vars_live_2(list(var), code_info, code_info).
:- mode code_info__make_vars_live_2(in, in, out) is det.

code_info__make_vars_live_2([]) --> [].
code_info__make_vars_live_2([V|Vs]) -->
	(
		code_info__current_store_map(Store),
		{ map__search(Store, V, Lval0) }
	->
		{ Lval = Lval0 }
	;
		code_info__get_call_info(Call),
		{ map__search(Call, V, Lval0) }
	->
		{ Lval = Lval0 }
	;
		{ error("I don't know where to put this variable") }
	),
	code_info__get_exprn_info(Exprn0),
	{ code_exprn__maybe_set_var_location(V, Lval, Exprn0, Exprn) },
	code_info__set_exprn_info(Exprn),
	code_info__make_vars_live_2(Vs).

%---------------------------------------------------------------------------%

code_info__maybe_save_hp(Maybe, Code) -->
	( { Maybe = yes } ->
		code_info__save_hp(Code)
	;
		{ Code = empty }
	).

code_info__maybe_restore_hp(Maybe, Code) -->
	( { Maybe = yes } ->
		code_info__restore_hp(Code)
	;
		{ Code = empty }
	).

code_info__maybe_get_old_hp(Maybe, Code) -->
	( { Maybe = yes } ->
		code_info__get_old_hp(Code)
	;
		{ Code = empty }
	).

code_info__maybe_pop_stack(Maybe, Code) -->
	( { Maybe = yes } ->
		code_info__pop_stack(Code)
	;
		{ Code = empty }
	).

%---------------------------------------------------------------------------%

code_info__save_hp(Code) -->
	code_info__push_temp(hp, HpSlot),
	{ Code = node([ mark_hp(HpSlot) - "Save heap pointer" ]) }.

code_info__restore_hp(Code) -->
	code_info__pop_temp(Lval),
	{ Code = node([ restore_hp(lval(Lval)) - "Restore heap pointer" ]) }.

code_info__get_old_hp(Code) -->
	code_info__get_stack_top(Lval),
	{ Code = node([ restore_hp(lval(Lval)) - "Reset heap pointer" ]) }.

code_info__save_redoip(Code) -->
	code_info__push_temp(redoip(lval(maxfr)), RedoIpSlot),
	{ Code = node([ assign(RedoIpSlot, lval(redoip(lval(maxfr))))
				- "Save the redoip" ]) }.

code_info__restore_redoip(Code) -->
	code_info__pop_temp(Lval),
	{ Code = node([ assign(redoip(lval(maxfr)), lval(Lval))
			- "Restore the redoip" ]) }.

%---------------------------------------------------------------------------%

code_info__stack_variable(Num, Lval) -->
	code_info__get_proc_model(CodeModel),
	(
		{ CodeModel = model_non }
	->
		{ Num1 is Num - 1 },		% framevars start at zero
		{ Lval = framevar(Num1) }
	;
		{ Lval = stackvar(Num) }	% stackvars start at one
	).

%---------------------------------------------------------------------------%

	% The det stack frame is organized as follows.
	%
	%		... unused ...
	%	sp --->	<first unused slot>
	%		<space for local var 1>
	%		... local vars ...
	%		<space for local var n>
	%		<space for temporary reg save 1>
	%		... temporary reg saves ...
	%		<space for temporary reg save n>
	%		<space for succip>
	%
	% The stack pointer points to the first free location at the
	% top of the stack.
	%
	% `code_info__num_stackslots' counts the number of slots reserved 
	% for saving local variables.
	%
	% `code_info__max_push_count' counts the number of slots reserved
	% for saving and restoring registers (hp, redoip, etc.)
	%
	% `code_info__succip_used' determines whether we need a slot to
	% hold the succip.
	%
	% The variable part of the nondet stack is organized in the same way
	% as the det stack (but the nondet stack also contains several other
	% fixed fields.)

%---------------------------------------------------------------------------%

	% Returns the total stackslot count, but not including space for
	% succip.
code_info__get_total_stackslot_count(NumSlots) -->
	code_info__get_stackslot_count(SlotsForVars),
	code_info__get_max_push_count(SlotsForTemps),
	{ NumSlots is SlotsForVars + SlotsForTemps }.

%---------------------------------------------------------------------------%

	% `push_temp' doesn't actually increment the stack pointer, it just
	% increments the push count.  The space will be allocated in the
	% procedure prologue.

:- pred code_info__push_temp(lval, lval, code_info, code_info).
:- mode code_info__push_temp(in, out, in, out) is det.

code_info__push_temp(Item, StackVar) -->
	code_info__get_push_count(Count0),
	{ Count is Count0 + 1 },
	code_info__set_push_count(Count),
	code_info__get_stackslot_count(NumSlots),
	{ Slot is Count + NumSlots },
	code_info__stack_variable(Slot, StackVar),
	code_info__get_pushed_values(VStack0),
	{ stack__push(VStack0, StackVar - Item, VStack) },
	code_info__set_pushed_values(VStack).

%---------------------------------------------------------------------------%

	% `pop_stack' and `pop_temp' don't actually decrement the stack
	% pointer, they just decrement the push count.  The space will
	% be deallocated in the procedure epilogue.

code_info__pop_stack(empty) -->
	code_info__get_push_count(Count0),
	{ Count is Count0 - 1 },
	code_info__set_push_count(Count),
	code_info__get_pushed_values(VStack0),
	{ stack__pop_det(VStack0, _, VStack) },
	code_info__set_pushed_values(VStack).

:- pred code_info__pop_temp(lval, code_info, code_info).
:- mode code_info__pop_temp(out, in, out) is det.

code_info__pop_temp(StackVar) -->
	code_info__get_push_count(Count0),
	{ Count is Count0 - 1 },
	code_info__set_push_count(Count),
	code_info__get_pushed_values(VStack0),
	{ stack__pop_det(VStack0, _, VStack) },
	code_info__set_pushed_values(VStack),
	code_info__get_stackslot_count(NumSlots),
	{ Slot is Count0 + NumSlots },
	code_info__stack_variable(Slot, StackVar).

:- pred code_info__get_stack_top(lval, code_info, code_info).
:- mode code_info__get_stack_top(out, in, out) is det.

code_info__get_stack_top(StackVar) -->
	code_info__get_push_count(Count),
	code_info__get_stackslot_count(NumSlots),
	{ Slot is Count + NumSlots },
	code_info__stack_variable(Slot, StackVar).

%---------------------------------------------------------------------------%

code_info__generate_failure(Code) -->
	code_info__failure_cont(Cont),
	{ code_info__failure_cont_address(Cont, FailureAddress) },
	{ Code = node([ goto(FailureAddress, FailureAddress) - "Fail" ]) }.

code_info__generate_test_and_fail(Rval, Code) -->
	code_info__failure_cont(Cont),
	{ code_info__failure_cont_address(Cont, FailureAddress) },
	{ code_util__neg_rval(Rval, NegRval) },
	{ Code = node([ if_val(NegRval, FailureAddress) -
				"Test for failure" ]) }.

code_info__failure_cont_address(known(Label), label(Label)).
code_info__failure_cont_address(do_fail, do_fail).
code_info__failure_cont_address(unknown, do_redo).

%---------------------------------------------------------------------------%

	% `pre_commit' and `commit' should be generated as a pair
	% surrounding a non-det goal.
	% If the goal succeeds, the `commit' will cut any choice points
	% generated in the goal.

code_info__generate_pre_commit(PreCommit, FailLabel) -->
	code_info__get_next_label(FailLabel, yes),
	code_info__push_temp(maxfr, MaxfrSlot),
	code_info__push_temp(redoip(lval(maxfr)), RedoipSlot),
	{ SaveCode = node([
		assign(MaxfrSlot, lval(maxfr)) -
				"Save pointer to top of nondet stack",
		assign(RedoipSlot, lval(redoip(lval(maxfr)))) - "Save the top redoip"
	]) },
	code_info__push_failure_cont(known(FailLabel)),
	{ SetRedoIp = node([
		assign(redoip(lval(maxfr)),
				const(address_const(label(FailLabel)))) -
			"Hijack the topmost failure continuation"
	]) },
	{ PreCommit = tree(SaveCode, SetRedoIp) }.

code_info__generate_commit(FailLabel, Commit) -->
	% XXX Not sure if this label can be accessed externally
	code_info__get_next_label(SuccLabel, yes),
	{ GotoSuccCode = node([
		goto(label(SuccLabel), label(SuccLabel)) -
			"Jump to success continuation",
		label(FailLabel) - "Failure continuation"
	]) },
	{ SuccLabelCode = node([
		label(SuccLabel) - "Success continuation"
	]) },
	code_info__pop_failure_cont,
	code_info__pop_temp(RedoIpSlot),
	code_info__pop_temp(MaxfrSlot),
	{ RestoreMaxfr = node([
		assign(maxfr, lval(MaxfrSlot)) -
			"Prune away unwanted choice-points"
	]) },
	{ RestoreRedoIp = node([
		assign(redoip(lval(maxfr)), lval(RedoIpSlot)) -
			"Restore the top redoip"
	]) },
	code_info__generate_failure(Fail),
	{ SuccessCode = tree(RestoreMaxfr, RestoreRedoIp) },
	{ FailCode = tree(RestoreRedoIp, Fail) },
	{ Commit = tree(GotoSuccCode, tree(FailCode,
		tree(SuccLabelCode, SuccessCode))) }.

%---------------------------------------------------------------------------%

code_info__push_failure_cont(Cont) -->
	code_info__get_fall_through(Fall0),
	{ stack__push(Fall0, Cont, Fall) },
	code_info__set_fall_through(Fall).

code_info__pop_failure_cont -->
	( 
		code_info__get_fall_through(Fall0),
		{ stack__pop(Fall0, _, Fall) },
		code_info__set_fall_through(Fall)
	->
		[]
	;
		{ error("code_info__pop_failure_cont: empty stack") }
	).

code_info__failure_cont(Cont) -->
	code_info__get_fall_through(Fall),
	(
		{ stack__top(Fall, Cont0) }
	->
		{ Cont = Cont0 }
	;
		{ error("code_info__failure_cont: no failure continuation") }
	).

code_info__restore_failure_cont(ContCode) -->
	code_info__failure_cont(FailCont),
	( { FailCont = known(ContLab) } ->
		{ Label = label(ContLab) }
	; { FailCont = do_fail } ->
		{ Label = do_fail }
	;
		{ error("cannot restore unknown failure continuation") }
	),
	{ ContCode = node([
		modframe(Label) - "Restore failure continuation"
	]) }.

%---------------------------------------------------------------------------%

code_info__push_store_map(Map) -->
	code_info__get_store_map(Maps0),
	{ stack__push(Maps0, Map, Maps) },
	code_info__set_store_map(Maps).

code_info__pop_store_map -->
	code_info__get_store_map(Maps0),
	{ stack__pop_det(Maps0, _, Maps) },
	code_info__set_store_map(Maps).

code_info__current_store_map(Map) -->
	code_info__get_store_map(Maps0),
	(
		{ stack__top(Maps0, Map0) }
	->
		{ Map = Map0 }
	;
		{ error("No store map on stack") }
	).

%---------------------------------------------------------------------------%

code_info__generate_stack_livevals(Args, LiveVals) -->
	code_info__get_live_variables(LiveVars),
	{ set__list_to_set(LiveVars, Vars0) },
	{ set__difference(Vars0, Args, Vars) },
	{ set__to_sorted_list(Vars, VarList) },
	{ set__init(LiveVals0) },
	code_info__generate_stack_livevals_2(VarList, LiveVals0, LiveVals1),
	code_info__get_pushed_values(Pushed),
	{ code_info__generate_stack_livevals_3(Pushed, LiveVals1, LiveVals) }.

:- pred code_info__generate_stack_livevals_2(list(var), set(lval), set(lval),
						code_info, code_info).
:- mode code_info__generate_stack_livevals_2(in, in, out, in, out) is det.

code_info__generate_stack_livevals_2([], Vals, Vals) --> [].
code_info__generate_stack_livevals_2([V|Vs], Vals0, Vals) -->
	code_info__get_variable_slot(V, Slot),
	{ set__insert(Vals0, Slot, Vals1) },
	code_info__generate_stack_livevals_2(Vs, Vals1, Vals).

:- pred code_info__generate_stack_livevals_3(stack(pair(lval)), 
					set(lval), set(lval)).
:- mode code_info__generate_stack_livevals_3(in, in, out) is det.

code_info__generate_stack_livevals_3(Stack0, Vals0, Vals) :-
	(
		stack__pop(Stack0, Top - _, Stack1)
	->
		set__insert(Vals0, Top, Vals1),
		code_info__generate_stack_livevals_3(Stack1, Vals1, Vals)
	;
		Vals = Vals0
	).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

code_info__get_requests(Requests) -->
	code_info__get_module_info(ModuleInfo),
	{ module_info_get_unify_requests(ModuleInfo, Requests) }.

%---------------------------------------------------------------------------%

code_info__apply_instmap_delta(Delta) -->
	code_info__get_instmap(InstMap0),
	{ apply_instmap_delta(InstMap0, Delta, InstMap) },
	code_info__set_instmap(InstMap).

%---------------------------------------------------------------------------%

code_info__generate_stack_livelvals(Args, LiveVals) -->
	code_info__get_live_variables(LiveVars),
	{ set__list_to_set(LiveVars, Vars0) },
	{ set__difference(Vars0, Args, Vars) },
	{ set__to_sorted_list(Vars, VarList) },
        { set__init(LiveVals0) },
        code_info__generate_stack_livelvals_2(VarList, LiveVals0, LiveVals1),
	{ set__to_sorted_list(LiveVals1, LiveVals2) },
	code_info__livevals_to_livelvals(LiveVals2, LiveVals3),
        code_info__get_pushed_values(Pushed),
        { code_info__generate_stack_livelvals_3(Pushed, LiveVals3, LiveVals) }.

:- pred code_info__generate_stack_livelvals_2(list(var), 
					set(pair(lval, var)),
					set(pair(lval, var)),
					code_info, code_info).
:- mode code_info__generate_stack_livelvals_2(in, in, out, in, out) is det.

code_info__generate_stack_livelvals_2([], Vals, Vals) --> [].
code_info__generate_stack_livelvals_2([V|Vs], Vals0, Vals) -->
	code_info__get_variable_slot(V, Slot),
	{ set__insert(Vals0, Slot - V, Vals1) },
	code_info__generate_stack_livelvals_2(Vs, Vals1, Vals).

:- pred code_info__generate_stack_livelvals_3(stack(pair(lval)), 
					list(liveinfo), list(liveinfo)).
:- mode code_info__generate_stack_livelvals_3(in, in, out) is det.

code_info__generate_stack_livelvals_3(Stack0, LiveInfo0, LiveInfo) :-
	(
		stack__pop(Stack0, Top - StoredLval , Stack1)
	->
		code_info__get_shape_num(StoredLval, S_Num),
		LiveInfo = [live_lvalue(Top, S_Num) | Lives],
		code_info__generate_stack_livelvals_3(Stack1, LiveInfo0, Lives)
	;
		LiveInfo = LiveInfo0
	).

:- pred code_info__get_shape_num(lval, int).
:- mode code_info__get_shape_num(in, out) is det.

code_info__get_shape_num(succip, -1).
code_info__get_shape_num(hp, -2).
code_info__get_shape_num(maxfr, -3).
code_info__get_shape_num(curfr, -7).	% XXX magic numbers! is this one right?
code_info__get_shape_num(redoip(_), -4). % And this one?
code_info__get_shape_num(sp, -5).
code_info__get_shape_num(lvar(_), -6).
code_info__get_shape_num(field(_, _, _), -6).
code_info__get_shape_num(temp(_), -6).
code_info__get_shape_num(reg(_), -6).
code_info__get_shape_num(stackvar(_), -6).
code_info__get_shape_num(framevar(_), -6).

:- pred code_info__livevals_to_livelvals(list(pair(lval, var)), list(liveinfo),
					 code_info, code_info).
:- mode code_info__livevals_to_livelvals(in, out, in, out) is det.

code_info__livevals_to_livelvals([], [], C, C).
code_info__livevals_to_livelvals([L - V|Ls], [live_lvalue(L, S_Num )|Lives]) --> 
	code_info__get_module_info(Module),
	{ module_info_shapes(Module, S_Tab0) } ,
	{ module_info_types(Module, Type_Table) } ,
	code_info__variable_type(V, Type),
	{ shapes__request_shape_number(Type - ground, Type_Table, 
					S_Tab0, S_Tab1, S_Num) } ,
	{ module_info_set_shapes(Module, S_Tab1, Module1) },
	code_info__set_module_info(Module1),
	code_info__livevals_to_livelvals(Ls, Lives).

%---------------------------------------------------------------------------%

code_info__variable_type(Var, Type) -->
	code_info__get_proc_info(ProcInfo),
	{ proc_info_vartypes(ProcInfo, VarTypes) },
	{ map__lookup(VarTypes, Var, Type) }.

%---------------------------------------------------------------------------%

%---------------------------------------------------------------------------%

:- pred code_info__get_stackslot_count(int, code_info, code_info).
:- mode code_info__get_stackslot_count(out, in, out) is det.

% :- pred code_info__get_registers(register_info, code_info, code_info).
% :- mode code_info__get_registers(out, in, out) is det.
% 
% :- pred code_info__set_registers(register_info, code_info, code_info).
% :- mode code_info__set_registers(in, in, out) is det.

:- pred code_info__get_exprn_info(exprn_info, code_info, code_info).
:- mode code_info__get_exprn_info(out, in, out) is det.

:- pred code_info__set_exprn_info(exprn_info, code_info, code_info).
:- mode code_info__set_exprn_info(in, in, out) is det.

		% Get the fall though point for failure
:- pred code_info__get_fall_through(fall_through, code_info, code_info).
:- mode code_info__get_fall_through(out, in, out) is det.

		% Set the fall though point for failure
:- pred code_info__set_fall_through(fall_through, code_info, code_info).
:- mode code_info__set_fall_through(in, in, out) is det.

:- pred code_info__get_liveness_info(liveness_info, code_info, code_info).
:- mode code_info__get_liveness_info(out, in, out) is det.

:- pred code_info__set_liveness_info(liveness_info, code_info, code_info).
:- mode code_info__set_liveness_info(in, in, out) is det.

:- pred code_info__get_proc_model(code_model, code_info, code_info).
:- mode code_info__get_proc_model(out, in, out) is det.

:- pred code_info__get_push_count(int, code_info, code_info).
:- mode code_info__get_push_count(out, in, out) is det.

:- pred code_info__set_push_count(int, code_info, code_info).
:- mode code_info__set_push_count(in, in, out) is det.

:- pred code_info__get_max_push_count(int, code_info, code_info).
:- mode code_info__get_max_push_count(out, in, out) is det.

:- pred code_info__set_max_push_count(int, code_info, code_info).
:- mode code_info__set_max_push_count(in, in, out) is det.

:- pred code_info__get_store_map(stack(map(var, lval)), code_info, code_info).
:- mode code_info__get_store_map(out, in, out) is det.

:- pred code_info__set_store_map(stack(map(var, lval)), code_info, code_info).
:- mode code_info__set_store_map(in, in, out) is det.

:- pred code_info__get_pushed_values(stack(pair(lval)), code_info, code_info).
:- mode code_info__get_pushed_values(out, in, out) is det.

:- pred code_info__set_pushed_values(stack(pair(lval)), code_info, code_info).
:- mode code_info__set_pushed_values(in, in, out) is det.

code_info__get_stackslot_count(A, CI, CI) :-
	CI = code_info(A, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _).

% code_info__set_stackslot_count(A, CI0, CI) :-
% 	CI0 =code_info(_, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S),
% 	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S).

code_info__get_label_count(B, CI, CI) :-
	CI = code_info(_, B, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _).

code_info__set_label_count(B, CI0, CI) :-
	CI0 = code_info(A, _, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S).

code_info__get_varset(C, CI, CI) :-
	CI = code_info(_, _, C, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _).

code_info__set_varset(C, CI0, CI) :-
	CI0 = code_info(A, B, _, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S).

code_info__get_call_info(D, CI, CI) :-
	CI = code_info(_, _, _, D, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _).

code_info__set_call_info(D, CI0, CI) :-
	CI0 = code_info(A, B, C, _, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S).

code_info__get_pred_id(E, CI, CI) :-
	CI = code_info(_, _, _, _, E, _, _, _, _, _, _, _, _, _, _, _, _, _, _).

code_info__set_pred_id(E, CI0, CI) :-
	CI0 = code_info(A, B, C, D, _, F, G, H, I, J, K, L, M, N, O, P, Q, R, S),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S).

code_info__get_proc_id(F, CI, CI) :-
	CI = code_info(_, _, _, _, _, F, _, _, _, _, _, _, _, _, _, _, _, _, _).

code_info__set_proc_id(F, CI0, CI) :-
	CI0 = code_info(A, B, C, D, E, _, G, H, I, J, K, L, M, N, O, P, Q, R, S),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S).

% code_info__get_registers(G, CI, CI) :-
% 	CI = code_info(_, _, _, _, _, _, G, _, _, _, _, _, _, _, _, _, _, _, _).

% code_info__set_registers(G, CI0, CI) :-
% 	CI0 = code_info(A, B, C, D, E, F, _, H, I, J, K, L, M, N, O, P, Q, R, S),
% 	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S).

code_info__get_exprn_info(H, CI, CI) :-
	CI = code_info(_, _, _, _, _, _, _, H, _, _, _, _, _, _, _, _, _, _, _).

code_info__set_exprn_info(H, CI0, CI) :-
	CI0 = code_info(A, B, C, D, E, F, G, _, I, J, K, L, M, N, O, P, Q, R, S),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S).

code_info__get_proc_info(I, CI, CI) :-
	CI = code_info(_, _, _, _, _, _, _, _, I, _, _, _, _, _, _, _, _, _, _).

code_info__get_succip_used(J, CI, CI) :-
	CI = code_info(_, _, _, _, _, _, _, _, _, J, _, _, _, _, _, _, _, _, _).

code_info__set_succip_used(J, CI0, CI) :-
	CI0 = code_info(A, B, C, D, E, F, G, H, I, _, K, L, M, N, O, P, Q, R, S),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S).

code_info__get_fall_through(K, CI, CI) :-
	CI = code_info(_, _, _, _, _, _, _, _, _, _, K, _, _, _, _, _, _, _, _).

code_info__set_fall_through(K, CI0, CI) :-
	CI0 = code_info(A, B, C, D, E, F, G, H, I, J, _, L, M, N, O, P, Q, R, S),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S).

code_info__get_module_info(L, CI, CI) :-
	CI = code_info(_, _, _, _, _, _, _, _, _, _, _, L, _, _, _, _, _, _, _).

code_info__set_module_info(L, CI0, CI) :-
	CI0 = code_info(A, B, C, D, E, F, G, H, I, J, K, _, M, N, O, P, Q, R, S),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S).

code_info__get_liveness_info(M, CI, CI) :-
	CI = code_info(_, _, _, _, _, _, _, _, _, _, _, _, M, _, _, _, _, _, _).

code_info__set_liveness_info(M, CI0, CI) :-
	CI0 = code_info(A, B, C, D, E, F, G, H, I, J, K, L, _, N, O, P, Q, R, S),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S).

code_info__get_store_map(N, CI, CI) :-
	CI = code_info(_, _, _, _, _, _, _, _, _, _, _, _, _, N, _, _, _, _, _).

code_info__set_store_map(N, CI0, CI) :-
	CI0 = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, _, O, P, Q, R, S),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S).

code_info__get_proc_model(O, CI, CI) :-
	CI = code_info(_, _, _, _, _, _, _, _, _, _, _, _, _, _, O, _, _, _, _).

code_info__get_instmap(P, CI, CI) :-
	CI = code_info(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, P, _, _, _).

code_info__set_instmap(P, CI0, CI) :-
	CI0 = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, _, Q, R, S),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S).

code_info__get_push_count(Q, CI, CI) :-
	CI = code_info(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _,
		Q - _, _, _).

code_info__get_max_push_count(QMax, CI, CI) :-
	CI = code_info(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _,
		_ - QMax, _, _).

code_info__set_push_count(Q, CI0, CI) :-
	CI0 = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P,
		_ - QMax0, R, S),
	int__max(QMax0, Q, QMax),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P,
		Q - QMax, R, S).

code_info__set_max_push_count(QMax, CI0, CI) :-
	CI0 = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P,
		Q - _, R, S),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P,
		Q - QMax, R, S).

code_info__get_globals(R, CI, CI) :-
	CI = code_info(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, R, _).

code_info__set_globals(R, CI0, CI) :-
	CI0 = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, _, S),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S).

code_info__get_pushed_values(S, CI, CI) :-
	CI = code_info(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, S).

code_info__set_pushed_values(S, CI0, CI) :-
	CI0 = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, _),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S).

%---------------------------------------------------------------------------%

code_info__grab_code_info(C, C, C).

code_info__slap_code_info(C0, C1, C) :-
	code_info__get_label_count(L, C1, _),
	code_info__set_label_count(L, C0, C2),
	code_info__get_succip_used(S, C1, _),
	code_info__set_succip_used(S, C2, C3),
	code_info__get_store_map(F, C1, _),
	code_info__set_store_map(F, C3, C4),
	code_info__get_fall_through(J, C1, _),
	code_info__set_fall_through(J, C4, C5),
	code_info__get_max_push_count(PC, C1, _),
	code_info__set_max_push_count(PC, C5, C).

%---------------------------------------------------------------------------%

:- pred code_info__make_assignment_comment(var, lval, string,
							code_info, code_info).
:- mode code_info__make_assignment_comment(in, in, out, in, out) is det.

code_info__make_assignment_comment(Var, _Lval, Comment) -->
	code_info__variable_to_string(Var, Name),
	{ string__append("Assigning from ", Name, Comment) }.

code_info__variable_to_string(Var, VarName) -->
	code_info__get_varset(Varset),
	(
		{ varset__lookup_name(Varset, Var, Name) }
	->
		{ VarName = Name }
	;
		{ term__var_to_int(Var, Int) },
		{ string__int_to_string(Int, IntString) },
		{ string__append("variable number ", IntString, VarName) }
	).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
