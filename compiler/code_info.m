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
%
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
% Note: Any new "state" arguments (eg counters) that should be strictly
% threaded (for example the counter used for allocating new labels) will
% require changes to code_info__slap_code_info/3.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module code_info.

:- interface.

:- import_module hlds_pred, hlds_goal, llds, instmap.
:- import_module code_util, tree, globals, unify_proc.
:- import_module bool, set, std_util, assoc_list.

:- type code_info.

		% Create a new code_info structure.
:- pred code_info__init(varset, liveness_info, stack_slots, bool, globals,
			pred_id, proc_id, proc_info, instmap,
			follow_vars, module_info, shape_table, code_info).
:- mode code_info__init(in, in, in, in, in, in, in, in, in, in, in, in, out)
			is det.

		% Generate the next local label in sequence.
:- pred code_info__get_next_label(label, code_info, code_info).
:- mode code_info__get_next_label(out, in, out) is det.

		% Generate the next local cell number in sequence.
:- pred code_info__get_next_cell_number(int, code_info, code_info).
:- mode code_info__get_next_cell_number(out, in, out) is det.

		% Create a code address which holds the address of the specified
		% procedure.
		% The fourth argument should be `no' if the the caller wants the
		% returned address to be valid from everywhere in the program.
		% If being valid from within the current procedure is enough,
		% this argument should be `yes' wrapped around the value of the
		% --procs-per-c-function option and the current procedure id.
		% Using an address that is only valid from within the current
		% procedure may make jumps more efficient.
:- pred code_info__make_entry_label(module_info, pred_id, proc_id, bool,
		code_addr, code_info, code_info).
:- mode code_info__make_entry_label(in, in, in, in, out, in, out) is det.

		% Get the variables for the current procedure.
:- pred code_info__get_varset(varset, code_info, code_info).
:- mode code_info__get_varset(out, in, out) is det.

		% Set the variables for the current procedure.
:- pred code_info__set_varset(varset, code_info, code_info).
:- mode code_info__set_varset(in, in, out) is det.

		% Get the hlds mapping from variables to
		% stack slots
:- pred code_info__get_stack_slots(stack_slots, code_info, code_info).
:- mode code_info__get_stack_slots(out, in, out) is det.

		% Set the hlds mapping from variables to
		% stack slots
:- pred code_info__set_stack_slots(stack_slots, code_info, code_info).
:- mode code_info__set_stack_slots(in, in, out) is det.

:- pred code_info__get_pred_id(pred_id, code_info, code_info).
:- mode code_info__get_pred_id(out, in, out) is det.

:- pred code_info__set_pred_id(pred_id, code_info, code_info).
:- mode code_info__set_pred_id(in, in, out) is det.

:- pred code_info__get_proc_id(proc_id, code_info, code_info).
:- mode code_info__get_proc_id(out, in, out) is det.

:- pred code_info__set_proc_id(proc_id, code_info, code_info).
:- mode code_info__set_proc_id(in, in, out) is det.

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

:- pred code_info__generate_forced_saves(store_map, code_tree,
	code_info, code_info).
:- mode code_info__generate_forced_saves(in, out, in, out) is det.

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

:- pred code_info__remake_with_store_map(store_map, code_info, code_info).
:- mode code_info__remake_with_store_map(in, in, out) is det.

:- pred code_info__remake_with_stack_slots(code_info, code_info).
:- mode code_info__remake_with_stack_slots(in, out) is det.

:- pred code_info__update_liveness_info(set(var), code_info, code_info).
:- mode code_info__update_liveness_info(in, in, out) is det.

:- pred code_info__update_deadness_info(set(var), code_info, code_info).
:- mode code_info__update_deadness_info(in, in, out) is det.

:- pred code_info__set_var_location(var, lval, code_info, code_info).
:- mode code_info__set_var_location(in, in, in, out) is det.

:- pred code_info__produce_variable(var, code_tree, rval, code_info, code_info).
:- mode code_info__produce_variable(in, out, out, in, out) is det.

:- pred code_info__produce_variable_in_reg(var, code_tree, rval,
						code_info, code_info).
:- mode code_info__produce_variable_in_reg(in, out, out, in, out) is det.

:- pred code_info__place_var(var, lval, code_tree, code_info, code_info).
:- mode code_info__place_var(in, in, out, in, out) is det.

:- pred code_info__materialize_vars_in_rval(rval, rval, code_tree, code_info,
		code_info).
:- mode code_info__materialize_vars_in_rval(in, out, out, in, out) is det.

:- pred code_info__make_vars_dead(set(var), code_tree, code_info, code_info).
:- mode code_info__make_vars_dead(in, out, in, out) is det.

:- pred code_info__make_vars_live(set(var), code_info, code_info).
:- mode code_info__make_vars_live(in, in, out) is det.

:- pred code_info__get_liveness_info(liveness_info, code_info, code_info).
:- mode code_info__get_liveness_info(out, in, out) is det.

:- pred code_info__set_liveness_info(liveness_info, code_info, code_info).
:- mode code_info__set_liveness_info(in, in, out) is det.

:- pred code_info__acquire_reg(reg, code_info, code_info).
:- mode code_info__acquire_reg(out, in, out) is det.

:- pred code_info__release_reg(reg, code_info, code_info).
:- mode code_info__release_reg(in, in, out) is det.

:- pred code_info__lock_reg(reg, code_info, code_info).
:- mode code_info__lock_reg(in, in, out) is det.

:- pred code_info__unlock_reg(reg, code_info, code_info).
:- mode code_info__unlock_reg(in, in, out) is det.

:- pred code_info__clear_r1(code_tree, code_info, code_info).
:- mode code_info__clear_r1(out, in, out) is det.

:- pred code_info__get_module_info(module_info, code_info, code_info).
:- mode code_info__get_module_info(out, in, out) is det.

:- pred code_info__get_shapes(shape_table, code_info, code_info).
:- mode code_info__get_shapes(out, in, out) is det.

:- pred code_info__set_shapes(shape_table, code_info, code_info).
:- mode code_info__set_shapes(in, in, out) is det.

:- type failure_cont
	--->	failure_cont(failure_continuation, maybe(label), failure_map).
				% the maybe(label) is yes if we created a
				% temporary frame and we need to restore
				% curfr after a redo()

:- type failure_continuation
	--->	known(bool)	% on failure we jump to a label
				% the bool is `yes' if we are inside
				% a nondet context
	;	unknown.	% on failure we do a `redo()'
				% NB. any piece of code that sets the current
				% failure continuation to `unknown' had
				% better save variables that are live (on
				% redo) onto the stack.

:- type failure_map	==	assoc_list(map(var, set(rval)), code_addr).
			% we assume that there are exactly two
			% alternatives in the failure_map list;
			% the first one is the entry point with variables
			% potentially in registers and the second one in
			% the list is the entry point with variables on the
			% stack.

	% push a new failure continuation onto the stack

:- pred code_info__make_known_failure_cont(set(var), bool, code_tree,
						code_info, code_info).
:- mode code_info__make_known_failure_cont(in, in, out, in, out) is det.

	% We manufacture a failure cont when we start generating
	% code for a proc because on the failure of a procedure
	% we don't need to prepare for execution to resume in this
	% procedure.
:- pred code_info__manufacture_failure_cont(bool, code_info, code_info).
:- mode code_info__manufacture_failure_cont(in, in, out) is det.

	% pop the failure continuation stack

:- pred code_info__pop_failure_cont(code_info, code_info).
:- mode code_info__pop_failure_cont(in, out) is det.

	% set the topmost failure cont to `unknown' (e.g. after
	% a nondet call or after a disjunction).

:- pred code_info__unset_failure_cont(code_info, code_info).
:- mode code_info__unset_failure_cont(in, out) is det.

	% lookup the value on the top of the failure continuation stack

:- pred code_info__failure_cont(failure_cont, code_info, code_info).
:- mode code_info__failure_cont(out, in, out) is det.

	% generate some code to restore the current redoip, by looking
	% at the top of the failure continuation stack.

:- pred code_info__restore_failure_cont(code_tree, code_info, code_info).
:- mode code_info__restore_failure_cont(out, in, out) is det.

:- pred code_info__modify_failure_cont(code_tree, code_info, code_info).
:- mode code_info__modify_failure_cont(out, in, out) is det.

		% do_soft_cut takes the lval where the address of the
		% failure frame is stored, and it returns code to
		% set the redoip of that frame to do_fail.

:- pred code_info__do_soft_cut(lval, code_tree, code_info, code_info).
:- mode code_info__do_soft_cut(in, out, in, out) is det.

:- pred code_info__stack_variable(int, lval, code_info, code_info).
:- mode code_info__stack_variable(in, out, in, out) is det.

:- pred code_info__generate_nondet_saves(code_tree, code_info, code_info).
:- mode code_info__generate_nondet_saves(out, in, out) is det.

:- pred code_info__generate_failure(code_tree, code_info, code_info).
:- mode code_info__generate_failure(out, in, out) is det.

:- pred code_info__generate_under_failure(code_tree, code_info, code_info).
:- mode code_info__generate_under_failure(out, in, out) is det.

:- pred code_info__generate_test_and_fail(rval, code_tree,
							code_info, code_info).
:- mode code_info__generate_test_and_fail(in, out, in, out) is det.

:- pred code_info__generate_det_pre_commit(code_tree, code_info, code_info).
:- mode code_info__generate_det_pre_commit(out, in, out) is det.

:- pred code_info__generate_det_commit(code_tree, code_info, code_info).
:- mode code_info__generate_det_commit(out, in, out) is det.

:- pred code_info__generate_semi_pre_commit(label, code_tree,
					code_info, code_info).
:- mode code_info__generate_semi_pre_commit(out, out, in, out) is det.

:- pred code_info__generate_semi_commit(label, code_tree, code_info, code_info).
:- mode code_info__generate_semi_commit(in, out, in, out) is det.

:- pred code_info__save_maxfr(lval, code_tree, code_info, code_info).
:- mode code_info__save_maxfr(out, out, in, out) is det.

:- pred code_info__save_hp(code_tree, code_info, code_info).
:- mode code_info__save_hp(out, in, out) is det.

:- pred code_info__restore_hp(code_tree, code_info, code_info).
:- mode code_info__restore_hp(out, in, out) is det.

:- pred code_info__save_ticket(code_tree, code_info, code_info).
:- mode code_info__save_ticket(out, in, out) is det.

:- pred code_info__restore_ticket(code_tree, code_info, code_info).
:- mode code_info__restore_ticket(out, in, out) is det.

:- pred code_info__restore_ticket_and_pop(code_tree, code_info, code_info).
:- mode code_info__restore_ticket_and_pop(out, in, out) is det.

:- pred code_info__discard_ticket(code_tree, code_info, code_info).
:- mode code_info__discard_ticket(out, in, out) is det.

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

:- pred code_info__maybe_save_ticket(bool, code_tree, code_info, code_info).
:- mode code_info__maybe_save_ticket(in, out, in, out) is det.

:- pred code_info__maybe_restore_ticket(bool, code_tree, code_info, code_info).
:- mode code_info__maybe_restore_ticket(in, out, in, out) is det.

:- pred code_info__maybe_restore_ticket_and_pop(bool, code_tree, 
	code_info, code_info).
:- mode code_info__maybe_restore_ticket_and_pop(in, out, in, out) is det.

:- pred code_info__maybe_discard_ticket(bool, code_tree, code_info, code_info).
:- mode code_info__maybe_discard_ticket(in, out, in, out) is det.

:- pred code_info__get_globals(globals, code_info, code_info).
:- mode code_info__get_globals(out, in, out) is det.

:- pred code_info__set_globals(globals, code_info, code_info).
:- mode code_info__set_globals(in, in, out) is det.

:- pred code_info__variable_type(var, type, code_info, code_info).
:- mode code_info__variable_type(in, out, in, out) is det.

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

:- pred code_info__get_nondet_lives(set(var), code_info, code_info).
:- mode code_info__get_nondet_lives(out, in, out) is det.

:- pred code_info__set_nondet_lives(set(var), code_info, code_info).
:- mode code_info__set_nondet_lives(in, in, out) is det.

:- pred code_info__can_generate_direct_branch(code_addr, code_info, code_info).
:- mode code_info__can_generate_direct_branch(out, in, out) is semidet.

	% Given a list of type variables, find the lvals where the 
	% corresponding type_infos are being stored.

:- pred code_info__find_type_infos(list(var), list(lval), code_info, code_info).
:- mode code_info__find_type_infos(in, out, in, out) is det.

:- pred code_info__current_resume_point_vars(set(var), code_info, code_info).
:- mode code_info__current_resume_point_vars(out, in, out) is det.

:- pred code_info__push_resume_point_vars(set(var), code_info, code_info).
:- mode code_info__push_resume_point_vars(in, in, out) is det.

:- pred code_info__pop_resume_point_vars(code_info, code_info).
:- mode code_info__pop_resume_point_vars(in, out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds_module, hlds_data.
:- import_module code_exprn, set, varset, term, stack, prog_data.
:- import_module type_util, mode_util, options, shapes.
:- import_module string, require, char, list, map, bimap, tree, int.

:- pred code_info__get_label_count(int, code_info, code_info).
:- mode code_info__get_label_count(out, in, out) is det.

:- pred code_info__set_label_count(int, code_info, code_info).
:- mode code_info__set_label_count(in, in, out) is det.

:- pred code_info__get_cell_count(int, code_info, code_info).
:- mode code_info__get_cell_count(out, in, out) is det.

:- pred code_info__set_cell_count(int, code_info, code_info).
:- mode code_info__set_cell_count(in, in, out) is det.

:- type code_info	--->
		code_info(
			int,		% The number of stack slots allocated.
					% for storing variables.
					% (Some extra stack slots are used
					% for saving and restoring registers.)
			int,		% Counter for the local labels used
					% by this procedure.
			varset,		% The variables in this procedure.
			stack_slots,	% The map giving the stack slot
					% to be used for each variable
					% that will ever need to be stored
					% on the stack.
			pred_id,	% The label of the current predicate.
			proc_id,	% The label of the current procedure.
			int,		% Counter for cells in this proc.
			exprn_info,	% A map storing the information about
					% the status of each variable.
			proc_info,	% The proc_info for the this procedure.
			bool,		% do we need to store succip?
			fall_through,	% The failure continuation stack
			module_info,	% The module_info structure - you just
					% never know when you might need it.
					% It should be read-only.
			liveness_info,	% Variables that are live
					% after this goal
			instmap,	% insts of variables
			int,		% The current number of extra
					% temporary stackslots that have been
					% pushed during the procedure
			int,		% The maximum number of extra
					% temporary stackslots that have been
					% pushed during the procedure
			globals,	% code generation options
			stack(pair(lval, lval_or_ticket)),
					% the locations in use on the stack
			shape_table,	% Table of shapes.
			set(var),	% Variables that are not quite live
					% but are only nondet-live (so that
					% we make sure we save them).
			stack(set(var)),
					% Each resumption point has an
					% associated set of variables
					% whose values may be needed on
					% resumption at that point.
					% This field gives those variables
					% for a nested set of resumption
					% points. Each element must be
					% a superset of the ones below.
					% When a variable included in the top
					% set becomes no longer forward live,
					% we must save its value to the stack.
			list(pair(lval, lval_or_ticket))
					% A list of lvalues (ie curfr, maxfr
					% and redoip) that get saved onto the
					% det stack even though the current
					% context is nondet. We need to store
					% these for GC purposes.
	).

:- type fall_through	==	stack(failure_cont).

:- type lval_or_ticket  --->	ticket ; lval(lval).

%---------------------------------------------------------------------------%

code_info__init(Varset, Liveness, StackSlots, SaveSuccip, Globals,
		PredId, ProcId, ProcInfo, Requests, _FollowVars,
		ModuleInfo, Shapes, C) :-
	proc_info_headvars(ProcInfo, HeadVars),
	proc_info_arg_info(ProcInfo, ArgInfos),
	assoc_list__from_corresponding_lists(HeadVars, ArgInfos, Args),
	code_info__build_input_arg_list(Args, ArgList),
	globals__get_options(Globals, Options),
	code_exprn__init_state(ArgList, Varset, Options, ExprnInfo),
	stack__init(Continue),
	stack__init(PushedVals0),
	stack__init(ResumeSetStack0),
	set__init(NondetLives),
	code_info__max_slot(StackSlots, SlotCount0),
	proc_info_interface_code_model(ProcInfo, CodeModel),
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
		StackSlots, 
		PredId,
		ProcId,
		0,
		ExprnInfo,
		ProcInfo,
		SaveSuccip,
		Continue,
		ModuleInfo,
		Liveness,
		Requests,
		0,
		0,
		Globals,
		PushedVals0,
		Shapes,
		NondetLives,
		ResumeSetStack0,
		[]
	).

:- pred code_info__build_input_arg_list(assoc_list(var, arg_info),
						assoc_list(var, rval)).
:- mode code_info__build_input_arg_list(in, out) is det.

code_info__build_input_arg_list([], []).
code_info__build_input_arg_list([V - Arg | Rest0], VarArgs) :-
	Arg = arg_info(Loc, Mode),
	(
		Mode = top_in
	->
		code_util__arg_loc_to_register(Loc, Reg),
		VarArgs = [V - lval(reg(Reg)) | VarArgs0]
	;
		VarArgs = VarArgs0
	),
	code_info__build_input_arg_list(Rest0, VarArgs0).

%---------------------------------------------------------------------------%

:- pred code_info__max_slot(stack_slots, int).
:- mode code_info__max_slot(in, out) is det.

code_info__max_slot(StackSlots, SlotCount) :-
	map__values(StackSlots, StackSlotList),
	code_info__max_slot_2(StackSlotList, 0, SlotCount).

:- pred code_info__max_slot_2(list(lval), int, int).
:- mode code_info__max_slot_2(in, in, out) is det.

code_info__max_slot_2([], Max, Max).
code_info__max_slot_2([L | Ls], Max0, Max) :-
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

code_info__get_next_label(Label) -->
	code_info__get_module_info(ModuleInfo),
	code_info__get_pred_id(PredId),
	code_info__get_proc_id(ProcId),
	code_info__get_label_count(N0),
	{ N is N0 + 1 },
	code_info__set_label_count(N),
	{ code_util__make_internal_label(ModuleInfo, PredId, ProcId, N,
		Label) }.

code_info__get_next_cell_number(N) -->
	code_info__get_cell_count(N0),
	{ N is N0 + 1 },
	code_info__set_cell_count(N).

%---------------------------------------------------------------------------%

code_info__make_entry_label(ModuleInfo, PredId, ProcId, Immed0, PredAddress) -->
	(
		{ Immed0 = no },
		{ Immed = no }
	;
		{ Immed0 = yes },
		code_info__get_globals(Globals),
		{ globals__lookup_int_option(Globals, procs_per_c_function,
			ProcsPerFunc) },
		code_info__get_pred_id(CurPredId),
		code_info__get_proc_id(CurProcId),
		{ Immed = yes(ProcsPerFunc - proc(CurPredId, CurProcId)) }
	),
	{ code_util__make_entry_label(ModuleInfo, PredId, ProcId, Immed,
		PredAddress) }.

%---------------------------------------------------------------------------%

	% XXX We could use the sanity checking mechanism...
code_info__clear_all_registers -->
	code_info__get_exprn_info(Exprn0),
	{ code_exprn__clobber_regs([], Exprn0, Exprn) },
	code_info__set_exprn_info(Exprn).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- pred code_info__get_variable_slot(var, lval, code_info, code_info).
:- mode code_info__get_variable_slot(in, out, in, out) is det.

code_info__get_variable_slot(Var, Slot) -->
	code_info__get_stack_slots(StackSlots),
	( { map__search(StackSlots, Var, SlotPrime) } ->
		{ Slot = SlotPrime }
	;
		code_info__variable_to_string(Var, Name),
		{ term__var_to_int(Var, Num) },
		{ string__int_to_string(Num, NumStr) },
		{ string__append_list([
			"code_info__get_variable_slot: variable `",
			Name, "' (", NumStr, ") not found"], Str) },
		{ error(Str) }
	).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

	% Given a constructor id, and a variable (so that we can work out the
	% type of the constructor), determine correct tag (representation)
	% of that constructor.

code_info__cons_id_to_tag(Var, ConsId, ConsTag) -->
	code_info__variable_type(Var, Type),
	code_info__get_module_info(ModuleInfo),
	{ code_util__cons_id_to_tag(ConsId, Type, ModuleInfo, ConsTag) }.

%---------------------------------------------------------------------------%

code_info__get_live_variables(VarList) -->
	code_info__get_liveness_info(NormalLiveVars),
	code_info__get_nondet_lives(NondetLiveVars),
	{ set__union(NormalLiveVars, NondetLiveVars, Vars) },
	{ set__to_sorted_list(Vars, VarList) }.

%---------------------------------------------------------------------------%

code_info__variable_is_live(Var) -->
	code_info__get_liveness_info(Liveness),
	code_info__get_nondet_lives(Nondets),
	(
		{ set__member(Var, Liveness) }
	;
		{ set__member(Var, Nondets) }
	).

%---------------------------------------------------------------------------%

code_info__get_pred_proc_arginfo(PredId, ProcId, ArgInfo) -->
	code_info__get_module_info(ModuleInfo),
	{ module_info_pred_proc_info(ModuleInfo, PredId, ProcId, _, ProcInfo) },
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

code_info__produce_variable_in_reg(Var, Code, Rval) -->
	code_info__get_exprn_info(Exprn0),
	{ code_exprn__produce_var_in_reg(Var, Rval, Code, Exprn0, Exprn) },
	code_info__set_exprn_info(Exprn).

%---------------------------------------------------------------------------%

:- pred code_info__produce_variable_in_reg_or_stack(var, code_tree, rval,
						code_info, code_info).
:- mode code_info__produce_variable_in_reg_or_stack(in, out, out, in, out)
						is det.

code_info__produce_variable_in_reg_or_stack(Var, Code, Rval) -->
	code_info__get_exprn_info(Exprn0),
	{ code_exprn__produce_var_in_reg_or_stack(Var, Rval, Code,
		Exprn0, Exprn) },
	code_info__set_exprn_info(Exprn).

%---------------------------------------------------------------------------%

code_info__materialize_vars_in_rval(Rval0, Rval, Code) -->
	code_info__get_exprn_info(Exprn0),
	{ code_exprn__materialize_vars_in_rval(Rval0, Rval, Code,
		Exprn0, Exprn) },
	code_info__set_exprn_info(Exprn).

%---------------------------------------------------------------------------%

code_info__place_var(Var, Lval, Code) -->
	code_info__get_exprn_info(Exprn0),
	{ code_exprn__place_var(Var, Lval, Code, Exprn0, Exprn) },
	code_info__set_exprn_info(Exprn).

%---------------------------------------------------------------------------%

:- pred code_info__place_vars(assoc_list(var, set(rval)), code_tree,
							code_info, code_info).
:- mode code_info__place_vars(in, out, in, out) is det.

code_info__place_vars([], empty) --> [].
code_info__place_vars([V-Rs | RestList], Code) -->
	(
		{ set__to_sorted_list(Rs, RList) },
		{ code_info__lval_member(L, RList) }
	->
		code_info__place_var(V, L, ThisCode)
	;
		{ ThisCode = empty }
	),
	code_info__place_vars(RestList, RestCode),
	{ Code = tree(ThisCode, RestCode) }.

%---------------------------------------------------------------------------%

code_info__lock_reg(Reg) -->
	code_info__get_exprn_info(Exprn0),
	{ code_exprn__lock_reg(Reg, Exprn0, Exprn) },
	code_info__set_exprn_info(Exprn).

%---------------------------------------------------------------------------%

code_info__unlock_reg(Reg) -->
	code_info__get_exprn_info(Exprn0),
	{ code_exprn__unlock_reg(Reg, Exprn0, Exprn) },
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

code_info__clear_r1(Code) -->
	code_info__get_exprn_info(Exprn0),
	{ code_exprn__clear_r1(Code, Exprn0, Exprn) },
	code_info__set_exprn_info(Exprn).

%---------------------------------------------------------------------------%

code_info__cache_expression(Var, Rval) -->
	code_info__get_exprn_info(Exprn0),
	{ code_exprn__cache_exprn(Var, Rval, Exprn0, Exprn) },
	code_info__set_exprn_info(Exprn).

%---------------------------------------------------------------------------%

code_info__setup_call([], _Direction, empty) --> [].
code_info__setup_call([V - arg_info(Loc,Mode) | Rest], Direction, Code) -->
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

code_info__generate_forced_saves(StoreMap, Code) --> 
	code_info__get_stack_slots(StackSlots),
	code_info__get_live_variables(Vars),
	code_info__generate_forced_saves_2(Vars, StoreMap, StackSlots, Code).

:- pred code_info__generate_forced_saves_2(list(var), map(var, lval),
			map(var, lval), code_tree, code_info, code_info).
:- mode code_info__generate_forced_saves_2(in, in, in, out, in, out) is det.

code_info__generate_forced_saves_2([], _Store, _StackSlots, empty) --> [].
code_info__generate_forced_saves_2([V | Vs], Store, StackSlots, Code) -->
	(
		{ map__search(Store, V, Lval) }
	->
		code_info__get_exprn_info(Exprn0),
		{ code_exprn__place_var(V, Lval, Code0, Exprn0, Exprn) },
		code_info__set_exprn_info(Exprn)
	;
		{ map__search(StackSlots, V, Lval) }
	->
		code_info__get_exprn_info(Exprn0),
		{ code_exprn__place_var(V, Lval, Code0, Exprn0, Exprn) },
		code_info__set_exprn_info(Exprn)
	;
		code_info__variable_to_string(V, Name),
		{ term__var_to_int(V, Num) },
		{ string__int_to_string(Num, NumStr) },
		{ string__append_list([
			"code_info__generate_forced_saves: variable `",
			Name, "' (", NumStr, ") not found"], Str) },
		{ error(Str) }
	),
	code_info__generate_forced_saves_2(Vs, Store, StackSlots, Code1),
	{ Code = tree(Code0, Code1) }.

%---------------------------------------------------------------------------%

code_info__generate_nondet_saves(Code) -->
	code_info__get_stack_slots(StackSlots),
	{ map__to_assoc_list(StackSlots, StackSlotList) },
	code_info__generate_nondet_saves_2(StackSlotList, Code).

:- pred code_info__generate_nondet_saves_2(assoc_list(var, lval), code_tree,
							code_info, code_info).
:- mode code_info__generate_nondet_saves_2(in, out, in, out) is det.

code_info__generate_nondet_saves_2([], empty) --> [].
code_info__generate_nondet_saves_2([Var - StackThing | VarSlots], Code) --> 
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

code_info__remake_with_store_map(StoreMap) -->
	code_info__get_varset(Varset),
	code_info__get_live_variables(VarList),
	{ set__list_to_set(VarList, Vars) },
	code_info__get_stack_slots(StackSlots),
	{ map__overlay(StackSlots, StoreMap, LvalMap0) },
	{ map__select(LvalMap0, Vars, LvalMap) },
	{ map__to_assoc_list(LvalMap, VarLvals0) },
	{ code_info__fixup_lvallist(VarLvals0, VarLvals) },
	code_info__get_globals(Globals),
	{ globals__get_options(Globals, Options) },
	{ code_exprn__init_state(VarLvals, Varset, Options, Exprn) },
	code_info__set_exprn_info(Exprn).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

% code_info__remake_with_stack_slots rebuilds the register info and the
% variable info structures. It operates under the assumption that if
% a variable is live, then it should be located in the storage indicated
% by the call info information. This is for use at the end of the branches
% in branched structures, where we need to ensure that variables are
% stored in consistient places.

code_info__remake_with_stack_slots -->
	code_info__get_varset(Varset),
	code_info__get_live_variables(VarList),
	{ set__list_to_set(VarList, Vars) },
	code_info__get_stack_slots(StackSlots),
	{ map__select(StackSlots, Vars, LvalMap) },
	{ map__to_assoc_list(LvalMap, VarLvals0) },
	{ code_info__fixup_lvallist(VarLvals0, VarLvals) },
	code_info__get_globals(Globals),
	{ globals__get_options(Globals, Options) },
	{ code_exprn__init_state(VarLvals, Varset, Options, Exprn) },
	code_info__set_exprn_info(Exprn).

%---------------------------------------------------------------------------%

:- pred code_info__fixup_lvallist(assoc_list(var, lval), assoc_list(var, rval)).
:- mode code_info__fixup_lvallist(in, out) is det.

code_info__fixup_lvallist([], []).
code_info__fixup_lvallist([V - L | Ls], [V - lval(L) | Rs]) :-
	code_info__fixup_lvallist(Ls, Rs).

%---------------------------------------------------------------------------%

code_info__update_liveness_info(Births) -->
	code_info__get_liveness_info(Liveness0),
	{ set__union(Liveness0, Births, Liveness) },
	code_info__set_liveness_info(Liveness).

code_info__update_deadness_info(Deaths) -->
	code_info__get_liveness_info(Liveness0),
	{ set__difference(Liveness0, Deaths, Liveness) },
	code_info__set_liveness_info(Liveness).

%---------------------------------------------------------------------------%

code_info__make_vars_dead(Vars0, Code) -->
	code_info__current_resume_point_vars(ResumeVars),
	{ set__intersect(Vars0, ResumeVars, FlushVars) },
	{ set__to_sorted_list(FlushVars, FlushVarList) },
	code_info__flush_resume_variables(FlushVarList, Code),
	code_info__get_nondet_lives(NondetLives),
		% Don't kill off nondet-live variables
	{ set__difference(Vars0, NondetLives, Vars) },
	{ set__to_sorted_list(Vars, VarList) },
	code_info__make_vars_dead_2(VarList).

:- pred code_info__flush_resume_variables(list(var), code_tree,
	code_info, code_info).
:- mode code_info__flush_resume_variables(in, out, in, out) is det.

code_info__flush_resume_variables([], empty) --> [].
code_info__flush_resume_variables([Var | Vars], Code) -->
	code_info__save_variable_on_stack(Var, FirstCode),
	code_info__flush_resume_variables(Vars, RestCode),
	{ Code = tree(FirstCode, RestCode) }.

:- pred code_info__make_vars_dead_2(list(var), code_info, code_info).
:- mode code_info__make_vars_dead_2(in, in, out) is det.

code_info__make_vars_dead_2([]) --> [].
code_info__make_vars_dead_2([V | Vs]) -->
	code_info__get_exprn_info(Exprn0),
	{ code_exprn__var_becomes_dead(V, Exprn0, Exprn) },
	code_info__set_exprn_info(Exprn),
	code_info__make_vars_dead_2(Vs).

%---------------------------------------------------------------------------%

	% Make these variables appear magically live.
	% We don't care where they are put.

code_info__make_vars_live(Vars) -->
	{ set__to_sorted_list(Vars, VarList) },
	code_info__make_vars_live_2(VarList, 1).

:- pred code_info__make_vars_live_2(list(var), int, code_info, code_info).
:- mode code_info__make_vars_live_2(in, in, in, out) is det.

code_info__make_vars_live_2([], _) --> [].
code_info__make_vars_live_2([V | Vs], N0) -->
	code_info__get_exprn_info(Exprn0),
	code_info__get_stack_slots(StackSlots),
	(
		{ map__search(StackSlots, V, Lval0) }
	->
		{ Lval = Lval0 },
		{ N1 = N0 }
	;
		{ code_info__find_unused_reg(N0, Exprn0, N1) },
		{ Lval = reg(r(N1)) }
	),
	{ code_exprn__maybe_set_var_location(V, Lval, Exprn0, Exprn) },
	code_info__set_exprn_info(Exprn),
	code_info__make_vars_live_2(Vs, N1).

:- pred code_info__find_unused_reg(int, exprn_info, int).
:- mode code_info__find_unused_reg(in, in, out) is det.

code_info__find_unused_reg(N0, Exprn0, N) :-
	( code_exprn__lval_in_use(reg(r(N0)), Exprn0, _) ->
		N1 is N0 + 1,
		code_info__find_unused_reg(N1, Exprn0, N)
	;
		N = N0
	).

% :- pred code_info__make_vars_live_2(list(var), code_info, code_info).
% :- mode code_info__make_vars_live_2(in, in, out) is det.
% 
% code_info__make_vars_live_2([], _) --> [].
% code_info__make_vars_live_2([V | Vs]) -->
% 	(
% 		{ map__search(StoreMap, V, Lval0) }
% 	->
% 		{ Lval = Lval0 }
% 	;
% 		code_info__get_stack_slots(StackSlots),
% 		{ map__search(StackSlots, V, Lval0) }
% 	->
% 		{ Lval = Lval0 }
% 	;
% 		code_info__get_varset(Varset),
% 		{ varset__lookup_name(Varset, V, Name) },
% 		{ string__append("I don't know where to put variable ",
% 			Name, Msg) },
% 		{ error(Msg) }
% 	),
% 	code_info__get_exprn_info(Exprn0),
% 	{ code_exprn__maybe_set_var_location(V, Lval, Exprn0, Exprn) },
% 	code_info__set_exprn_info(Exprn),
% 	code_info__make_vars_live_2(Vs, StoreMap).

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

code_info__maybe_save_ticket(Maybe, Code) -->
	( { Maybe = yes } ->
		code_info__save_ticket(Code)
	;
		{ Code = empty }
	).

code_info__maybe_restore_ticket(Maybe, Code) -->
	( { Maybe = yes } ->
		code_info__restore_ticket(Code)
	;
		{ Code = empty }
	).

code_info__maybe_restore_ticket_and_pop(Maybe, Code) -->
	( { Maybe = yes } ->
		code_info__restore_ticket_and_pop(Code)
	;
		{ Code = empty }
	).

code_info__maybe_discard_ticket(Maybe, Code) -->
	( { Maybe = yes } ->
		code_info__discard_ticket(Code)
	;
		{ Code = empty }
	).

%---------------------------------------------------------------------------%

code_info__save_hp(Code) -->
	code_info__push_temp(lval(hp), HpSlot),
	{ Code = node([ mark_hp(HpSlot) - "Save heap pointer" ]) }.

code_info__restore_hp(Code) -->
	code_info__pop_temp(Lval),
	{ Code = node([ restore_hp(lval(Lval)) - "Restore heap pointer" ]) }.

code_info__get_old_hp(Code) -->
	code_info__get_stack_top(Lval),
	{ Code = node([ restore_hp(lval(Lval)) - "Reset heap pointer" ]) }.

code_info__save_ticket(Code) -->
	code_info__push_temp(ticket, Lval),
	{ Code = node([ store_ticket(Lval) - "Save ticket" ]) }.

code_info__restore_ticket(Code) -->
	code_info__get_stack_top(Lval),
	{ Code = node([ restore_ticket(lval(Lval)) - "Restore solver state" ]) }.

code_info__restore_ticket_and_pop(Code) -->
	code_info__pop_temp(Lval),
	{ Code = tree(
		node([ restore_ticket(lval(Lval)) - "Restore solver state" ]),
		node([ discard_ticket - "Pop ticket stack" ]) )
		}.

code_info__discard_ticket(Code) -->
	code_info__pop_temp(_),
	{ Code = node([ discard_ticket - "Restore ticket" ]) }.

code_info__save_redoip(Code) -->
	code_info__push_temp(lval(redoip(lval(maxfr))), RedoIpSlot),
	{ Code = node([ assign(RedoIpSlot, lval(redoip(lval(maxfr))))
				- "Save the redoip" ]) }.

code_info__save_maxfr(MaxfrSlot, Code) -->
	code_info__push_temp(lval(maxfr), MaxfrSlot),
	{ Code = node([ assign(MaxfrSlot, lval(maxfr))
				- "Save maxfr" ]) }.

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

:- pred code_info__push_temp(lval_or_ticket, lval, code_info, code_info).
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

:- pred code_info__add_commit_val(lval, lval, code_info, code_info).
:- mode code_info__add_commit_val(in, in, in, out) is det.

code_info__add_commit_val(Item, StackVar) -->
	code_info__get_commit_vals(Stack0),
	code_info__set_commit_vals([StackVar - lval(Item) | Stack0]).

%---------------------------------------------------------------------------%

:- pred code_info__rem_commit_val(code_info, code_info).
:- mode code_info__rem_commit_val(in, out) is det.

code_info__rem_commit_val -->
	code_info__get_commit_vals(Stack0),
	( 
		{ Stack0 = [_ | Stack] },
		code_info__set_commit_vals(Stack)
	;
		{ Stack0 = [] },
		{ error("code_info__rem_commit_val: Empty list") }
	).

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
	code_info__grab_code_info(CodeInfo),
	code_info__failure_cont(failure_cont(Cont, _MaybeRedoLab, FailureMap)),
	(
		{ Cont = known(_) },
		(
			code_info__pick_failure(FailureMap, FailureAddress0)
		->
			{ FailureAddress = FailureAddress0 },
			{ PlaceCode = empty }
		;
			{ FailureMap = [Map - Addr | _] }
		->
			{ FailureAddress = Addr },
			{ map__to_assoc_list(Map, AssocList) },
			code_info__place_vars(AssocList, PlaceCode)
		;
			{error("code_info__generate_failure: no valid failmap")}
		),
		{ BranchCode = node([goto(FailureAddress) - "fail"]) },
		{ Code = tree(PlaceCode, BranchCode) }
	;
		{ Cont = unknown },
		{ Code = node([goto(do_redo) - "fail"]) }
	),
	code_info__slap_code_info(CodeInfo).

%---------------------------------------------------------------------------%

code_info__generate_under_failure(Code) -->
	code_info__grab_code_info(CodeInfo),
	code_info__pop_failure_cont,
	code_info__generate_failure(Code),
	code_info__slap_code_info(CodeInfo).

%---------------------------------------------------------------------------%

code_info__generate_test_and_fail(Rval0, Code) -->
	code_info__failure_cont(failure_cont(Cont, _MaybeRedoLab, FailureMap)),
	(
		{ Cont = known(_) },
		(
			code_info__pick_failure(FailureMap, FailureAddress0)
		->
				% We branch away if the test *fails*
			{ code_util__neg_rval(Rval0, Rval) },
			{ Code = node([ if_val(Rval, FailureAddress0) -
						"Test for failure" ]) }
		;
			{ FailureMap = [Map - Addr | _] }
		->
			{ FailureAddress = Addr },
			{ map__to_assoc_list(Map, AssocList) },
			code_info__get_next_label(SuccessLabel),
			code_info__grab_code_info(CodeInfo),
			code_info__place_vars(AssocList, PlaceCode),
			code_info__slap_code_info(CodeInfo),
			{ SuccessAddress = label(SuccessLabel) },
				% We branch away if the test Succeeds
			{ TestCode = node([ if_val(Rval0, SuccessAddress) -
					"Test for failure" ]) },
			{ FailCode = tree(PlaceCode, node([
				goto(FailureAddress) - ""
			])) },
			{ Code = tree(tree(TestCode, FailCode), node([
				label(SuccessLabel) - "success continuation"
			])) }
		;
			{ error("code_info__generate_test_and_fail: no valid failmap") }
		)
	;
		{ Cont = unknown },
		{ FailureAddress = do_redo },
			% We branch away if the test *fails*
		{ code_util__neg_rval(Rval0, Rval) },
		{ Code = node([ if_val(Rval, FailureAddress) -
					"Test for failure" ]) }
	).

%---------------------------------------------------------------------------%

code_info__can_generate_direct_branch(CodeAddr) -->
	code_info__failure_cont(failure_cont(known(no), no, FailureMap)),
	code_info__pick_failure(FailureMap, CodeAddr).
	% { FailureMap = [Map - CodeAddr | _] },
	% { map__is_empty(Map) }.

%---------------------------------------------------------------------------%

:- pred code_info__pick_failure(assoc_list(map(var, set(rval)), code_addr),
			code_addr, code_info, code_info).
:- mode code_info__pick_failure(in, out, in, out) is semidet.

code_info__pick_failure([], _CodeAddr) -->
	{ fail }.
code_info__pick_failure([Map - Addr | Rest], CodeAddr) -->
	{ map__keys(Map, KeyList) },
	{ set__list_to_set(KeyList, Keys) },
	code_info__variable_locations(Locations0),
	{ map__select(Locations0, Keys, Locations) },
	{ map__to_assoc_list(Locations, List) },
	(
		\+ (
			{ list__member(Thingy, List) },
			\+ (
				{ Thingy = Var - Actual },
				{ map__search(Map, Var, Rvals) },
				{ set__subset(Rvals, Actual) }
			)
		)
	->
		{ CodeAddr = Addr }
	;
		code_info__pick_failure(Rest, CodeAddr)
	).

%---------------------------------------------------------------------------%

:- pred code_info__variable_locations(map(var, set(rval)),
						code_info, code_info).
:- mode code_info__variable_locations(out, in, out) is det.

code_info__variable_locations(Locations) -->
	code_info__get_exprn_info(Exprn),
	{ code_exprn__get_varlocs(Exprn, Locations) }.

%---------------------------------------------------------------------------%

	% `semi_pre_commit' and `semi_commit' should be generated as a pair
	% surrounding a nondet goal.
	% `generate_semi_pre_commit' returns a label (a failure cont label)
	% which should be passed to `generate_semi_commit'.
	% If the goal succeeds, the `commit' will cut any choice points
	% generated in the goal.

code_info__generate_semi_pre_commit(RedoLab, PreCommit) -->
	code_info__get_proc_model(CodeModel),
	( { CodeModel = model_non } ->
		% the pushes and pops on the det stack below will cause
		% problems for accurate garbage collection. Hence we 
		% make sure the commit vals are made live, so gc
		% can figure out what is going on later.
		code_info__get_module_info(ModuleInfo),
		code_info__get_pred_id(PredId),
		{ predicate_name(ModuleInfo, PredId, PredName) },
		{ string__append("commit in ", PredName, Message) },
		{ PushCode = node([
			incr_sp(3, Message) - 
			"push space for curfr, maxfr, and redoip" 
		]) },
		{ CurfrSlot = stackvar(1) },
		{ MaxfrSlot = stackvar(2) },
		{ RedoipSlot = stackvar(3) },
		code_info__add_commit_val(curfr, CurfrSlot),
		code_info__add_commit_val(maxfr, MaxfrSlot),
		code_info__add_commit_val(redoip(lval(maxfr)), RedoipSlot)
	;
		{ PushCode = empty },
		code_info__push_temp(lval(curfr), CurfrSlot),
		code_info__push_temp(lval(maxfr), MaxfrSlot),
		code_info__push_temp(lval(redoip(lval(maxfr))), RedoipSlot)
	),
	{ SaveCode = node([
		assign(CurfrSlot, lval(curfr)) -
				"Save current nondet frame pointer",
		assign(MaxfrSlot, lval(maxfr)) -
				"Save top of nondet stack",
		assign(RedoipSlot, lval(redoip(lval(maxfr)))) -
				"Save the top redoip"
	]) },
	code_info__failure_cont(failure_cont(_OrigCont, _MaybeRedo,
				FailureMap0)),
	code_info__relabel_failure_cont(FailureMap0, FailureMap),
	code_info__push_failure_cont(failure_cont(known(yes), no, FailureMap)),
	code_info__get_next_label(RedoLab),
	{ HijackCode = node([
		assign(redoip(lval(maxfr)),
			const(code_addr_const(label(RedoLab)))) -
			"Hijack the failure cont"
	]) },
	{ PreCommit = tree(tree(PushCode, SaveCode), HijackCode) }.

code_info__generate_semi_commit(RedoLab, Commit) -->
	code_info__get_next_label(SuccLabel),
	{ GotoSuccLabel = node([
		goto(label(SuccLabel)) - "Jump to success continuation"
	]) },
	{ SuccLabelCode = node([
		label(SuccLabel) - "Success continuation"
	]) },
	{ RedoLabCode = node([
		label(RedoLab) - "Failure (redo) continuation"
	]) },
	code_info__grab_code_info(CodeInfo0),
	code_info__failure_cont(failure_cont(_OldCont, _MaybeRedo,
				HFailureMap)),
	code_info__generate_failure_continuation(HFailureMap, FailureContCode),
	code_info__generate_under_failure(Fail),
	code_info__slap_code_info(CodeInfo0),
	code_info__pop_failure_cont,
	code_info__get_proc_model(CodeModel),
	( { CodeModel = model_non } ->
		{ PopCode = node([decr_sp(3) - 
			"pop redoip, maxfr & curfr"]) },
		{ RedoipSlot = stackvar(3) },
		{ MaxfrSlot = stackvar(2) },
		{ CurfrSlot = stackvar(1) },
		code_info__rem_commit_val,
		code_info__rem_commit_val,
		code_info__rem_commit_val
	;
		{ PopCode = empty },
		code_info__pop_temp(RedoipSlot),
		code_info__pop_temp(MaxfrSlot),
		code_info__pop_temp(CurfrSlot)
	),
	{ RestoreMaxfr = node([
		assign(maxfr, lval(MaxfrSlot)) -
			"Prune away unwanted choice-points"
	]) },
	{ RestoreRedoip = node([
		assign(redoip(lval(maxfr)), lval(RedoipSlot)) -
			"Restore the top redoip"
	]) },
	{ RestoreCurfr = node([
		assign(curfr, lval(CurfrSlot)) -
			"Restore nondet frame pointer"
	]) },
	{ SuccessCode = tree(
		RestoreMaxfr,
		tree(tree(RestoreRedoip, RestoreCurfr), PopCode)
	) },
	{ FailCode = tree(
		tree(tree(RedoLabCode, RestoreCurfr), FailureContCode),
		tree(tree(RestoreRedoip, PopCode), Fail)
	) },
	{ Commit = tree(tree(SuccessCode, tree(GotoSuccLabel, FailCode)),
			SuccLabelCode) }.

	% `det_pre_commit' and `det_commit' should be generated as a pair
	% surrounding a multidet goal.
	% After the goal succeeds, the `commit' will cut any choice points
	% generated in the goal.

code_info__generate_det_pre_commit(PreCommit) -->
	code_info__get_proc_model(CodeModel),
	( { CodeModel = model_non } ->
		% the pushes and pops on the det stack below will cause
		% problems for accurate garbage collection. Hence we 
		% make sure the commit vals are made live, so gc
		% can figure out what is going on later.
		code_info__get_module_info(ModuleInfo),
		code_info__get_pred_id(PredId),
		{ predicate_name(ModuleInfo, PredId, PredName) },
		{ string__append("commit in ", PredName, Message) },
		{ PushCode = node([
			incr_sp(3, Message) - 
			"push space for curfr, maxfr, and redoip" 
		]) },
		{ CurfrSlot = stackvar(1) },
		{ MaxfrSlot = stackvar(2) },
		{ RedoipSlot = stackvar(3) },
		code_info__add_commit_val(curfr, CurfrSlot),
		code_info__add_commit_val(maxfr, MaxfrSlot),
		code_info__add_commit_val(redoip(lval(maxfr)), RedoipSlot)
	;
		{ PushCode = empty },
		code_info__push_temp(lval(curfr), CurfrSlot),
		code_info__push_temp(lval(maxfr), MaxfrSlot),
		code_info__push_temp(lval(redoip(lval(maxfr))), RedoipSlot)
	),
	{ SaveCode = node([
		assign(CurfrSlot, lval(curfr)) -
				"Save current nondet frame pointer",
		assign(MaxfrSlot, lval(maxfr)) -
				"Save top of nondet stack",
		assign(RedoipSlot, lval(redoip(lval(maxfr)))) -
				"Save the top redoip"
	]) },
	{ PreCommit = tree(PushCode, SaveCode) }.

code_info__generate_det_commit(Commit) -->
	code_info__get_proc_model(CodeModel),
	( { CodeModel = model_non } ->
		{ PopCode = node([decr_sp(3) - 
			"pop redoip, maxfr & curfr"]) },
		{ RedoipSlot = stackvar(3) },
		{ MaxfrSlot = stackvar(2) },
		{ CurfrSlot = stackvar(1) },
		code_info__rem_commit_val,
		code_info__rem_commit_val,
		code_info__rem_commit_val
	;
		{ PopCode = empty },
		code_info__pop_temp(RedoipSlot),
		code_info__pop_temp(MaxfrSlot),
		code_info__pop_temp(CurfrSlot)
	),
	{ RestoreMaxfr = node([
		assign(maxfr, lval(MaxfrSlot)) -
			"Prune away unwanted choice-points"
	]) },
	{ RestoreRedoip = node([
		assign(redoip(lval(maxfr)), lval(RedoipSlot)) -
			"Restore the top redoip"
	]) },
	{ RestoreCurfr = node([
		assign(curfr, lval(CurfrSlot)) -
			"Restore nondet frame pointer"
	]) },
	{ Commit = tree( RestoreMaxfr,
		tree(tree(RestoreRedoip, RestoreCurfr), PopCode)
	) }.

%---------------------------------------------------------------------------%

:- pred code_info__push_failure_cont(failure_cont, code_info, code_info).
:- mode code_info__push_failure_cont(in, in, out) is det.

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

%---------------------------------------------------------------------------%

code_info__manufacture_failure_cont(IsNondet) -->
	{ map__init(Empty) },
	(
		{ IsNondet = no }
	->
		code_info__get_next_label(ContLab1),
		code_info__get_next_label(ContLab2),
		{ Address1 = label(ContLab1) },
		{ Address2 = label(ContLab2) }
	;
		{ Address1 = do_fail },
		{ Address2 = do_fail }
	),
	code_info__push_failure_cont(failure_cont(known(IsNondet),
				no, [Empty - Address1, Empty - Address2])).

code_info__make_known_failure_cont(Vars, IsNondet, ModContCode) -->
	code_info__get_next_label(ContLab),
	code_info__get_next_label(StackLab),
	(
		{ IsNondet = no }
	->
		% In semidet continuations we don't use the redoip
		{ HijackCode = empty },
		{ MaybeRedoLab = no }
	;
		code_info__failure_cont(failure_cont(OrigCont, _, _)),
		{ OrigCont = unknown }
	->
			% efficiency of this code could be improved
			% ("mkframe()" is a bit of a sledge hammer)
		code_info__get_next_label(RedoLab),
		{ MaybeRedoLab = yes(RedoLab) },
		{ HijackCode =
			node([
				mkframe("hijack", 1, label(RedoLab)) -
					"create a temporary frame",
				assign(curfr, lval(succfr(lval(maxfr)))) -
				"restore curfr (which was clobbered by mkframe)"
			])
		}
	;
		{ MaybeRedoLab = no },
		{ HijackCode = node([
			assign(redoip(lval(maxfr)),
				const(code_addr_const(label(StackLab)))) -
				"Set failure continuation"
		]) }
	),
	{ set__to_sorted_list(Vars, VarList) },
	code_info__produce_vars(VarList, RegMap, RegCode),
	code_info__get_stack_slots(StackSlots),
	{ map__select(StackSlots, Vars, StackMap0) },
	{ map__to_assoc_list(StackMap0, StackList0) },
	{ code_info__tweak_stacklist(StackList0, StackList) },
	{ map__from_assoc_list(StackList, StackMap) },
	{ ContMap = [RegMap-label(ContLab), StackMap-label(StackLab)] },
	code_info__push_failure_cont(
		failure_cont(known(IsNondet), MaybeRedoLab, ContMap)),
	{ ModContCode = tree(RegCode, HijackCode) }.

%---------------------------------------------------------------------------%

:- pred code_info__produce_vars(list(var), map(var, set(rval)), code_tree,
						code_info, code_info).
:- mode code_info__produce_vars(in, out, out, in, out) is det.

code_info__produce_vars([], Map, empty) -->
	{ map__init(Map) }.
code_info__produce_vars([V | Vs], Map, Code) -->
	code_info__produce_vars(Vs, Map0, Code0),
	code_info__produce_variable_in_reg_or_stack(V, Code1, Rval),
	{ set__singleton_set(Rvals, Rval) },
	{ map__set(Map0, V, Rvals, Map) },
	{ Code = tree(Code0, Code1) }.

%---------------------------------------------------------------------------%

:- pred code_info__tweak_stacklist(assoc_list(var, lval),
					assoc_list(var, set(rval))).
:- mode code_info__tweak_stacklist(in, out) is det.

code_info__tweak_stacklist([], []).
code_info__tweak_stacklist([V-L | Rest0], [V-Rs | Rest]) :-
	set__singleton_set(Rs, lval(L)),
	code_info__tweak_stacklist(Rest0, Rest).

%---------------------------------------------------------------------------%

code_info__unset_failure_cont -->
	code_info__failure_cont(
		failure_cont(_OrigCont, MaybeRedoLabel, FailureMap)),
	code_info__pop_failure_cont,
	code_info__push_failure_cont(
		failure_cont(unknown, MaybeRedoLabel, FailureMap)).

%---------------------------------------------------------------------------%

:- pred code_info__relabel_failure_cont(failure_map, failure_map,
					code_info, code_info).
:- mode code_info__relabel_failure_cont(in, out, in, out) is det.

code_info__relabel_failure_cont([], []) --> [].
code_info__relabel_failure_cont([Map - _ | Rest0], [Map - L | Rest]) -->
	code_info__get_next_label(L0),
	{ L = label(L0) },
	code_info__relabel_failure_cont(Rest0, Rest).

%---------------------------------------------------------------------------%

code_info__modify_failure_cont(ModifyCode) -->
	code_info__failure_cont(failure_cont(OldCont, MaybeRedo0, FailureMap)),
	code_info__generate_failure_cont(FailureMap, FailureCode),
	code_info__pop_failure_cont,
	code_info__get_next_label(NewRegCont),
	code_info__get_next_label(NewStackCont),
	(
		{ OldCont = unknown ; OldCont = known(yes) }
	->
		{ NewCont = known(yes) },
		( { MaybeRedo0 = yes(_OldRedo) } ->
			code_info__get_next_label(NewRedoCont),
			{ MaybeRedo = yes(NewRedoCont) }
		;
			{ NewRedoCont = NewStackCont },
			{ MaybeRedo = no }
		),
		{ ResetCode = node([
			assign(redoip(lval(maxfr)),
				const(code_addr_const(label(NewRedoCont)))) -
				"modify failure cont"
		]) }
	;
		{ error("code_info__modify_failure_cont: semidet context") }
		% { NewCont = known(no) },
		% { ResetCode = empty },
		% { MaybeRedo = no }
	),
	(
		{ FailureMap = [RegMap - _RegCont, StackMap - _StackCont] }
	->
		code_info__push_failure_cont(failure_cont(NewCont, MaybeRedo,
			[RegMap - label(NewRegCont),
			StackMap - label(NewStackCont)]))
	;
		{ error("code_info__modify_failure_cont: bad failure map.") }
	),
	{ ModifyCode = tree(FailureCode, ResetCode) }.

	% XXX rewrite this to fit on one screeen
code_info__restore_failure_cont(Code) -->
	code_info__failure_cont(failure_cont(CurrentCont, _Redo1, FailureMap)),
	code_info__generate_failure_cont(FailureMap, FailureCode),
	code_info__pop_failure_cont,
		% Fixup the redoip of the top frame if necessary
	(
		{ CurrentCont = known(no) }
	->
		{ RestoreCode = empty },
		{ ResetCode = empty }
	;
		% { CurrentCont = known(yes)  ; CurrentCont = unknown }
		{ RestoreCode = empty },
		code_info__failure_cont(failure_cont(NewCont, MaybeRedoLab,
					NewFailureMap)),
		(
			{ NewCont = unknown },
			{ ResetCode = node([
				assign(redoip(lval(maxfr)),
					const(code_addr_const(do_fail))) -
					"restore failure cont"
			]) }
		;
			{ NewCont = known(NondetCont) },
			(
				{ NondetCont = no },
				{ ResetCode = empty }
			;
				{ NondetCont = yes },
				(
					{ MaybeRedoLab = yes(RedoLab) }
				->
					{ NewRedoAddress = label(RedoLab) }
				;
					{ NewFailureMap = [_, _Map - Cont] }
				->
					{ NewRedoAddress = Cont }
				;
					{ error("code_info__restore_failure_cont: no valid failure-map") }
				),
				{ ResetCode = node([
					assign(redoip(lval(maxfr)),
					const(code_addr_const(NewRedoAddress)))
						- "restore failure cont"
				]) }
			)
		)
	),
	{ Code = tree(FailureCode, tree(RestoreCode, ResetCode)) }.

%---------------------------------------------------------------------------%

code_info__do_soft_cut(TheFrame, Code) -->
	code_info__failure_cont(failure_cont(ContType, MaybeRedo, FailMap)),
	(
		{ ContType = known(_) },
		(
			{ MaybeRedo = yes(RedoLab) }
		->
			{ Address = label(RedoLab) }
		;
			{ FailMap = [_, _Places - StackLab] }
		->
			{ Address = StackLab }
		;
			{ error("code_info__do_soft_cut: invalid failmap") }
		)
	;
			% If the newly uncovered cont is unknown then
			% we must have created a new frame before the
			% condition which we no longer need, so we
			% set its redoip to do_fail.
		{ ContType = unknown },
		{ Address = do_fail }
	),
	{ Code = node([
		assign(redoip(lval(TheFrame)), const(code_addr_const(Address)))
			- "prune away the `else' case of the if-then-else"
	]) }.

%---------------------------------------------------------------------------%

:- pred code_info__generate_failure_cont(failure_map,
					code_tree, code_info, code_info).
:- mode code_info__generate_failure_cont(in, out, in, out) is det.

code_info__generate_failure_cont(FailMap, Code) -->
	code_info__failure_cont(FailureCont),
	{ FailureCont = failure_cont(_CurrentCont, MaybeRedo, _FailureMap) },

		% did we create a temp nondet frame?
		% if so, we need to create a redo() continuation, 
		% which restores curfr before continuing
	(
		{ MaybeRedo = yes(RedoLab) }
	->
		{ FixCurFrCode = 
			node([
				label(RedoLab) - "redo entry point",
				assign(curfr, lval(succfr(lval(maxfr)))) -
					"restore curfr"
			])
		}
	;
		{ FixCurFrCode = empty }
	),
	code_info__generate_failure_continuation(FailMap, Code0),
	{ Code = tree(FixCurFrCode, Code0) }.

:- pred code_info__generate_failure_continuation(failure_map, code_tree,
	code_info, code_info).
:- mode code_info__generate_failure_continuation(in, out, in, out) is det.

code_info__generate_failure_continuation(Cs, Code) -->
	(
		{ Cs = [] },
		{ error("code_info__generate_failure_continuation: |map| = 0") }
	;
		{ Cs = [C1] },
		{ C1 = Map1 - CodeAddr1 },
		{ extract_label_from_code_addr(CodeAddr1, Label1) },
		{ Code = node([label(Label1) - "Failure Continuation" ]) },
		code_info__set_var_locations(Map1)
	;
		{ Cs = [C1, C2] },
		{ C1 = Map1 - CodeAddr1 },
		{ C2 = Map2 - CodeAddr2 },
		{ extract_label_from_code_addr(CodeAddr1, Label1) },
		{ extract_label_from_code_addr(CodeAddr2, Label2) },
		{ Entry = node([label(Label2) - "fail continuation entry" ]) },
		code_info__set_var_locations(Map2),
		{ map__to_assoc_list(Map1, AssocList1) },
		code_info__place_resume_vars(AssocList1, PlaceCode),
		{ Exit = node([label(Label1) - "fail continuation exit" ]) },
		code_info__set_var_locations(Map1),
		{ Code = tree(Entry, tree(PlaceCode, Exit)) }
	;
		{ Cs = [_, _, _ | _] },
		{ error("code_info__generate_failure_continuation: |map| > 2") }
	).

:- pred extract_label_from_code_addr(code_addr, label).
:- mode extract_label_from_code_addr(in, out) is det.

extract_label_from_code_addr(CodeAddr, Label) :-
	( CodeAddr = label(Label0) ->
		Label = Label0
	;
		error("code_info__generate_failure_continuation: non-label!")
	).

:- pred code_info__place_resume_vars(assoc_list(var, set(rval)), code_tree,
	code_info, code_info).
:- mode code_info__place_resume_vars(in, out, in, out) is det.

code_info__place_resume_vars([], empty) --> [].
code_info__place_resume_vars([Var - TargetSet | Rest], Code) -->
	{ set__to_sorted_list(TargetSet, Targets) },
	code_info__place_resume_var(Var, Targets, FirstCode),
	{ Code = tree(FirstCode, RestCode) },
	code_info__place_resume_vars(Rest, RestCode).

:- pred code_info__place_resume_var(var, list(rval), code_tree,
	code_info, code_info).
:- mode code_info__place_resume_var(in, in, out, in, out) is det.

code_info__place_resume_var(_Var, [], empty) --> [].
code_info__place_resume_var(Var, [Target | Targets], Code) -->
	( { Target = lval(TargetLval) } ->
		code_info__place_var(Var, TargetLval, FirstCode)
	;
		{ error("code_info__place_resume_var: not lval") }
	),
	{ Code = tree(FirstCode, RestCode) },
	code_info__place_resume_var(Var, Targets, RestCode).

%---------------------------------------------------------------------------%

:- pred code_info__lval_member(lval, list(rval)).
:- mode code_info__lval_member(out, in) is semidet.

code_info__lval_member(Lval, [X | Xs]) :-
	(
		X = lval(Lval0)
	->
		Lval = Lval0
	;
		code_info__lval_member(Lval, Xs)
	).

%---------------------------------------------------------------------------%

:- pred code_info__set_var_locations(map(var, set(rval)), code_info, code_info).
:- mode code_info__set_var_locations(in, in, out) is det.

code_info__set_var_locations(Map) -->
	{ map__to_assoc_list(Map, List0) },
	{ code_info__flatten_varlval_list(List0, List) },
	code_info__get_varset(Varset),
	code_info__get_globals(Globals),
	{ globals__get_options(Globals, Options) },
	{ code_exprn__init_state(List, Varset, Options, Exprn) },
	code_info__set_exprn_info(Exprn).

:- pred code_info__flatten_varlval_list(assoc_list(var, set(rval)),
						assoc_list(var, rval)).
:- mode code_info__flatten_varlval_list(in, out) is det.

code_info__flatten_varlval_list([], []).
code_info__flatten_varlval_list([V - Rvals | Rest0], All) :-
	code_info__flatten_varlval_list(Rest0, Rest),
	set__to_sorted_list(Rvals, RvalList),
	code_info__flatten_varlval_list_2(RvalList, V, Rest1),
	list__append(Rest1, Rest, All).

:- pred code_info__flatten_varlval_list_2(list(rval), var,
						assoc_list(var, rval)).
:- mode code_info__flatten_varlval_list_2(in, in, out) is det.

code_info__flatten_varlval_list_2([], _V, []).
code_info__flatten_varlval_list_2([R | Rs], V, [V - R | Rest]) :-
	code_info__flatten_varlval_list_2(Rs, V, Rest).

%---------------------------------------------------------------------------%

code_info__generate_stack_livevals(Args, LiveVals) -->
	code_info__get_live_variables(LiveVars),
	{ set__list_to_set(LiveVars, Vars0) },
	{ set__difference(Vars0, Args, Vars) },
	{ set__to_sorted_list(Vars, VarList) },
	{ set__init(LiveVals0) },
	code_info__generate_stack_livevals_2(VarList, LiveVals0, LiveVals1),
	code_info__get_pushed_values(Pushed0),
	code_info__get_commit_vals(CommitVals),
	{ stack__push_list(Pushed0, CommitVals, Pushed) },
	{ code_info__generate_stack_livevals_3(Pushed, LiveVals1, LiveVals) }.

:- pred code_info__generate_stack_livevals_2(list(var), set(lval), set(lval),
						code_info, code_info).
:- mode code_info__generate_stack_livevals_2(in, in, out, in, out) is det.

code_info__generate_stack_livevals_2([], Vals, Vals) --> [].
code_info__generate_stack_livevals_2([V | Vs], Vals0, Vals) -->
	code_info__get_variable_slot(V, Slot),
	{ set__insert(Vals0, Slot, Vals1) },
	code_info__generate_stack_livevals_2(Vs, Vals1, Vals).

:- pred code_info__generate_stack_livevals_3(stack(pair(lval, lval_or_ticket)), 
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

code_info__apply_instmap_delta(Delta) -->
	code_info__get_instmap(InstMap0),
	{ instmap__apply_instmap_delta(InstMap0, Delta, InstMap) },
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
	code_info__get_globals(Globals),
	{ globals__get_gc_method(Globals, GC_Method) },
	code_info__livevals_to_livelvals(LiveVals2, LiveVals3, GC_Method),
        code_info__get_pushed_values(Pushed0),
	code_info__get_commit_vals(CommitVals),
	{ stack__push_list(Pushed0, CommitVals, Pushed) },
        { code_info__generate_stack_livelvals_3(Pushed, LiveVals3, LiveVals) }.

:- pred code_info__generate_stack_livelvals_2(list(var), 
					set(pair(lval, var)),
					set(pair(lval, var)),
					code_info, code_info).
:- mode code_info__generate_stack_livelvals_2(in, in, out, in, out) is det.

code_info__generate_stack_livelvals_2([], Vals, Vals) --> [].
code_info__generate_stack_livelvals_2([V | Vs], Vals0, Vals) -->
	code_info__get_variable_slot(V, Slot),
	{ set__insert(Vals0, Slot - V, Vals1) },
	code_info__generate_stack_livelvals_2(Vs, Vals1, Vals).

:- pred code_info__generate_stack_livelvals_3(stack(pair(lval,
	lval_or_ticket)), list(liveinfo), list(liveinfo)).
:- mode code_info__generate_stack_livelvals_3(in, in, out) is det.

code_info__generate_stack_livelvals_3(Stack0, LiveInfo0, LiveInfo) :-
	(
		stack__pop(Stack0, Top - StoredLval , Stack1)
	->
		code_info__get_shape_num(StoredLval, S_Num),
		LiveInfo = [live_lvalue(Top, S_Num, no) | Lives],
		code_info__generate_stack_livelvals_3(Stack1, LiveInfo0, Lives)
	;
		LiveInfo = LiveInfo0
	).

:- pred code_info__get_shape_num(lval_or_ticket, shape_num).
:- mode code_info__get_shape_num(in, out) is det.

code_info__get_shape_num(lval(succip), succip).
code_info__get_shape_num(lval(hp), hp).
code_info__get_shape_num(lval(maxfr), maxfr).
code_info__get_shape_num(lval(curfr), curfr).	
code_info__get_shape_num(lval(succfr(_)), succfr).	
code_info__get_shape_num(lval(prevfr(_)), prevfr).	
code_info__get_shape_num(lval(redoip(_)), redoip). 
code_info__get_shape_num(lval(succip(_)), succip). 
code_info__get_shape_num(lval(sp), sp).
code_info__get_shape_num(lval(lvar(_)), unwanted).
code_info__get_shape_num(lval(field(_, _, _)), unwanted).
code_info__get_shape_num(lval(temp(_)), unwanted).
code_info__get_shape_num(lval(reg(_)), unwanted).
code_info__get_shape_num(lval(stackvar(_)), unwanted).
code_info__get_shape_num(lval(framevar(_)), unwanted).
code_info__get_shape_num(ticket, ticket).

:- pred code_info__livevals_to_livelvals(list(pair(lval, var)), list(liveinfo),
				gc_method, code_info, code_info).
:- mode code_info__livevals_to_livelvals(in, out, in, in, out) is det.

code_info__livevals_to_livelvals([], [], _GC_Method, C, C).
code_info__livevals_to_livelvals([L - V | Ls], 
		[live_lvalue(L, num(S_Num), TypeParams) | Lives], GC_Method) -->
	( 
		{ GC_Method = accurate }
	->
		code_info__get_module_info(ModuleInfo),
		code_info__get_shapes(S_Tab0),
		{ module_info_types(ModuleInfo, Type_Table) },
		code_info__variable_type(V, Type),

		% XXX We don't yet support partial insts when allocating
		% XXX shapes, so pass ground(shared, no) as a placeholder.
		{ shapes__request_shape_number(Type - ground(shared, no), 
			Type_Table, S_Tab0, S_Tab1, S_Num) },
		{ type_util__vars(Type, TypeVars) },
		(
			% if not polymorphic
			{ TypeVars = [] }
		->
			{ TypeParams = no }
		;
			code_info__find_type_infos(TypeVars, Lvals),
			{ TypeParams = yes(Lvals) }
		),
		code_info__set_shapes(S_Tab1)
	;
		% Dummy values
		{ TypeParams = no },
		{ S_Num = 0 }
	),
	code_info__livevals_to_livelvals(Ls, Lives, GC_Method).

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

:- pred code_info__get_push_count(int, code_info, code_info).
:- mode code_info__get_push_count(out, in, out) is det.

:- pred code_info__set_push_count(int, code_info, code_info).
:- mode code_info__set_push_count(in, in, out) is det.

:- pred code_info__get_max_push_count(int, code_info, code_info).
:- mode code_info__get_max_push_count(out, in, out) is det.

:- pred code_info__set_max_push_count(int, code_info, code_info).
:- mode code_info__set_max_push_count(in, in, out) is det.

:- pred code_info__get_pushed_values(stack(pair(lval, lval_or_ticket)), 
	code_info, code_info).
:- mode code_info__get_pushed_values(out, in, out) is det.

:- pred code_info__set_pushed_values(stack(pair(lval, lval_or_ticket)), 
	code_info, code_info).
:- mode code_info__set_pushed_values(in, in, out) is det.

:- pred code_info__get_resume_point_stack(stack(set(var)),
	code_info, code_info).
:- mode code_info__get_resume_point_stack(out, in, out) is det.

:- pred code_info__set_resume_point_stack(stack(set(var)),
	code_info, code_info).
:- mode code_info__set_resume_point_stack(in, in, out) is det.

:- pred code_info__get_commit_vals(list(pair(lval, lval_or_ticket)), 
	code_info, code_info).
:- mode code_info__get_commit_vals(out, in, out) is det.

:- pred code_info__set_commit_vals(list(pair(lval, lval_or_ticket)), 
	code_info, code_info).
:- mode code_info__set_commit_vals(in, in, out) is det.

code_info__get_stackslot_count(A, CI, CI) :-
	CI = code_info(A, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _,
		_, _, _).

code_info__get_label_count(B, CI, CI) :-
	CI = code_info(_, B, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _,
		_, _, _).

code_info__get_varset(C, CI, CI) :-
	CI = code_info(_, _, C, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _,
		_, _, _).

code_info__get_stack_slots(D, CI, CI) :-
	CI = code_info(_, _, _, D, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _,
		_, _, _).

code_info__get_pred_id(E, CI, CI) :-
	CI = code_info(_, _, _, _, E, _, _, _, _, _, _, _, _, _, _, _, _, _, _,
		_, _, _).

code_info__get_proc_id(F, CI, CI) :-
	CI = code_info(_, _, _, _, _, F, _, _, _, _, _, _, _, _, _, _, _, _, _,
		_, _, _).

code_info__get_cell_count(G, CI, CI) :-
	CI = code_info(_, _, _, _, _, _, G, _, _, _, _, _, _, _, _, _, _, _, _,
		_, _, _).

code_info__get_exprn_info(H, CI, CI) :-
	CI = code_info(_, _, _, _, _, _, _, H, _, _, _, _, _, _, _, _, _, _, _,
		_, _, _).

code_info__get_proc_info(I, CI, CI) :-
	CI = code_info(_, _, _, _, _, _, _, _, I, _, _, _, _, _, _, _, _, _, _,
		_, _, _).

code_info__get_succip_used(J, CI, CI) :-
	CI = code_info(_, _, _, _, _, _, _, _, _, J, _, _, _, _, _, _, _, _, _,
		_, _, _).

code_info__get_fall_through(K, CI, CI) :-
	CI = code_info(_, _, _, _, _, _, _, _, _, _, K, _, _, _, _, _, _, _, _,
		_, _, _).

code_info__get_module_info(L, CI, CI) :-
	CI = code_info(_, _, _, _, _, _, _, _, _, _, _, L, _, _, _, _, _, _, _,
		_, _, _).

code_info__get_liveness_info(M, CI, CI) :-
	CI = code_info(_, _, _, _, _, _, _, _, _, _, _, _, M, _, _, _, _, _, _,
		_, _, _).

code_info__get_instmap(N, CI, CI) :-
	CI = code_info(_, _, _, _, _, _, _, _, _, _, _, _, _, N, _, _, _, _, _,
		_, _, _).

code_info__get_push_count(O, CI, CI) :-
	CI = code_info(_, _, _, _, _, _, _, _, _, _, _, _, _, _, O, _, _, _, _,
		_, _, _).

code_info__get_max_push_count(P, CI, CI) :-
	CI = code_info(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, P, _, _, _,
		_, _, _).

code_info__get_globals(Q, CI, CI) :-
	CI = code_info(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Q, _, _,
		_, _, _).

code_info__get_pushed_values(R, CI, CI) :-
	CI = code_info(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, R, _,
		_, _, _).

code_info__get_shapes(S, CI, CI) :-
	CI = code_info(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, S,
		_, _, _).

code_info__get_nondet_lives(T, CI, CI) :-
	CI = code_info(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _,
		T, _, _).

code_info__get_resume_point_stack(U, CI, CI) :-
	CI = code_info(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _,
		_, U, _).

code_info__get_commit_vals(V, CI, CI) :-
	CI = code_info(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _,
		_, _, V).

% :- type code_info	--->
% 		code_info(
% 	A		int,		% The number of stack slots allocated.
% 					% for storing variables.
% 					% (Some extra stack slots are used
% 					% for saving and restoring registers.)
% 	B		int,		% Counter for the local labels used
% 					% by this procedure.
% 	C		varset,		% The variables in this procedure.
% 	D		stack_slots,	% The map giving the stack slot
% 					% to be used for each variable
% 					% that will ever need to be stored
% 					% on the stack.
% 	E		pred_id,	% The label of the current predicate.
% 	F		proc_id,	% The label of the current procedure.
% 	G		int,		% Counter for cells in this proc.
% 	H		exprn_info,	% A map storing the information about
% 					% the status of each variable.
% 	I		proc_info,	% The proc_info for the this procedure.
% 	J		bool,		% do we need to store succip?
% 	K		fall_through,	% The failure continuation stack
% 	L		module_info,	% The module_info structure - you just
% 					% never know when you might need it.
% 					% It should be read-only.
% 	M		liveness_info,	% Variables that are live
% 					% after this goal
% 	N		instmap,	% insts of variables
% 	O		int,		% The current number of extra
% 					% temporary stackslots that have been
% 					% pushed during the procedure
% 	P		int,		% The maximum number of extra
% 					% temporary stackslots that have been
% 					% pushed during the procedure
% 	Q		globals,	% code generation options
% 	R		stack(pair(lval, lval_or_ticket)),
% 					% the locations in use on the stack
% 	S		shape_table,	% Table of shapes.
% 	T		set(var),	% Variables that are not quite live
% 					% but are only nondet-live (so that
% 					% we make sure we save them).
% 	U		stack(set(var)),
% 					% Each resumption point has an
% 					% associated set of variables
% 					% whose values may be needed on
% 					% resumption at that point.
% 					% This field gives those variables
% 					% for a nested set of resumption
% 					% points. Each element must be
% 					% a superset of the ones below.
% 					% When a variable included in the top
% 					% set becomes no longer forward live,
% 					% we must save its value to the stack.
% 	V		list(pair(lval, lval_or_ticket))
% 					% A list of lvalues (ie curfr, maxfr
% 					% and redoip) that get saved onto the
% 					% det stack even though the current
% 					% context is nondet. We need to store
% 					% these for GC purposes.
% 	).

% we don't need
% code_info__set_stackslot_count(A, CI0, CI)

code_info__set_label_count(B, CI0, CI) :-
	CI0 = code_info(A, _, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q,
		R, S, T, U, V),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q,
		R, S, T, U, V).

code_info__set_varset(C, CI0, CI) :-
	CI0 = code_info(A, B, _, D, E, F, G, H, I, J, K, L, M, N, O, P, Q,
		R, S, T, U, V),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q,
		R, S, T, U, V).

code_info__set_stack_slots(D, CI0, CI) :-
	CI0 = code_info(A, B, C, _, E, F, G, H, I, J, K, L, M, N, O, P, Q,
		R, S, T, U, V),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q,
		R, S, T, U, V).

code_info__set_pred_id(E, CI0, CI) :-
	CI0 = code_info(A, B, C, D, _, F, G, H, I, J, K, L, M, N, O, P, Q,
		R, S, T, U, V),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q,
		R, S, T, U, V).

code_info__set_proc_id(F, CI0, CI) :-
	CI0 = code_info(A, B, C, D, E, _, G, H, I, J, K, L, M, N, O, P, Q,
		R, S, T, U, V),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q,
		R, S, T, U, V).

code_info__set_cell_count(G, CI0, CI) :-
	CI0 = code_info(A, B, C, D, E, F, _, H, I, J, K, L, M, N, O, P, Q,
		R, S, T, U, V),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q,
		R, S, T, U, V).

code_info__set_exprn_info(H, CI0, CI) :-
	CI0 = code_info(A, B, C, D, E, F, G, _, I, J, K, L, M, N, O, P, Q,
		R, S, T, U, V),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q,
		R, S, T, U, V).

code_info__set_succip_used(J, CI0, CI) :-
	CI0 = code_info(A, B, C, D, E, F, G, H, I, _, K, L, M, N, O, P, Q,
		R, S, T, U, V),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q,
		R, S, T, U, V).

code_info__set_fall_through(K, CI0, CI) :-
	CI0 = code_info(A, B, C, D, E, F, G, H, I, J, _, L, M, N, O, P, Q,
		R, S, T, U, V),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q,
		R, S, T, U, V).

% It is a bad idea to allow the code generator to modify module_info
% in arbitrary ways.
% code_info__set_module_info(L, CI0, CI) :-

code_info__set_liveness_info(M, CI0, CI) :-
	CI0 = code_info(A, B, C, D, E, F, G, H, I, J, K, L, _, N, O, P, Q,
		R, S, T, U, V),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q,
		R, S, T, U, V).

code_info__set_instmap(N, CI0, CI) :-
	CI0 = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, _, O, P, Q,
		R, S, T, U, V),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q,
		R, S, T, U, V).

code_info__set_push_count(O, CI0, CI) :-
	CI0 = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, _, P0, Q,
		R, S, T, U, V),
	int__max(P0, O, P),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q,
		R, S, T, U, V).

code_info__set_max_push_count(P, CI0, CI) :-
	CI0 = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, _, Q,
		R, S, T, U, V),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q,
		R, S, T, U, V).

code_info__set_globals(Q, CI0, CI) :-
	CI0 = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, _,
		R, S, T, U, V),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q,
		R, S, T, U, V).

code_info__set_pushed_values(R, CI0, CI) :-
	CI0 = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q,
		_, S, T, U, V),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q,
		R, S, T, U, V).

code_info__set_shapes(S, CI0, CI) :-
	CI0 = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q,
		R, _, T, U, V),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q,
		R, S, T, U, V).

code_info__set_nondet_lives(T, CI0, CI) :-
	CI0 = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q,
		R, S, _, U, V),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q,
		R, S, T, U, V).

code_info__set_resume_point_stack(U, CI0, CI) :-
	CI0 = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q,
		R, S, T, _, V),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q,
		R, S, T, U, V).

code_info__set_commit_vals(V, CI0, CI) :-
	CI0 = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q,
		R, S, T, U, _),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q,
		R, S, T, U, V).

%---------------------------------------------------------------------------%

:- pred code_info__get_proc_model(code_model, code_info, code_info).
:- mode code_info__get_proc_model(out, in, out) is det.

code_info__get_proc_model(CodeModel) -->
	code_info__get_proc_info(ProcInfo),
	{ proc_info_interface_code_model(ProcInfo, CodeModel) }.

%---------------------------------------------------------------------------%

code_info__current_resume_point_vars(ResumeVars) -->
	code_info__get_resume_point_stack(ResumeStack),
	{ stack__top(ResumeStack, TopVars) ->
		ResumeVars = TopVars
	;
		set__init(ResumeVars)
	}.

code_info__push_resume_point_vars(ResumeVars) -->
	code_info__get_resume_point_stack(ResumeStack0),
	{ stack__top(ResumeStack0, OldTopVars) ->
		require(set__subset(OldTopVars, ResumeVars),
		"new resume point variable set does not include old one")
	;
		true
	},
	{ stack__push(ResumeStack0, ResumeVars, ResumeStack) },
	code_info__set_resume_point_stack(ResumeStack).

code_info__pop_resume_point_vars -->
	code_info__get_resume_point_stack(ResumeStack0),
	{ stack__pop_det(ResumeStack0, _, ResumeStack) },
	code_info__set_resume_point_stack(ResumeStack).

%---------------------------------------------------------------------------%

code_info__grab_code_info(C, C, C).

code_info__slap_code_info(C0, C1, C) :-
	code_info__get_label_count(L, C1, _),
	code_info__set_label_count(L, C0, C2),
	code_info__get_succip_used(S, C1, _),
	code_info__set_succip_used(S, C2, C3),
	code_info__get_fall_through(J, C0, _),
	code_info__set_fall_through(J, C3, C4),
	code_info__get_max_push_count(PC, C1, _),
	code_info__set_max_push_count(PC, C4, C5),
	code_info__get_shapes(Shapes, C1, _),
	code_info__set_shapes(Shapes, C5, C).

%---------------------------------------------------------------------------%

code_info__variable_to_string(Var, Name) -->
	code_info__get_varset(Varset),
	{ varset__lookup_name(Varset, Var, Name) }.

%---------------------------------------------------------------------------%

code_info__find_type_infos([], []) --> [].
code_info__find_type_infos([TVar | TVars], [Lval | Lvals]) --> 
	code_info__get_proc_info(ProcInfo),
	{ proc_info_typeinfo_varmap(ProcInfo, TypeInfoMap) },
	(
		{ map__search(TypeInfoMap, TVar, Var0) }
	->
		{ Var = Var0 }
	;
		{ error("cannot find var for type variable") }
	),
	{ proc_info_stack_slots(ProcInfo, StackSlots) },
	( 
		{ map__search(StackSlots, Var, Lval0) }
	->
		{ Lval = Lval0 }
	;
		code_info__variable_to_string(Var, VarString),
		{ string__format("code_info__find_type_infos: can't find lval for type_info var %s",
			[s(VarString)], ErrStr) },
		{ error(ErrStr) }
	),
	code_info__find_type_infos(TVars, Lvals).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
