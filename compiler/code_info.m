%---------------------------------------------------------------------------%
% Copyright (C) 1994-2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: code_info.m
%
% Main authors: conway, zs.
%
% This file defines the code_info type and various operations on it.
% The code_info structure is the 'state' of the code generator.
%
% This file is organized into nine submodules:
%
%	- the code_info structure and its access predicates
%	- simple wrappers around access predicates
%	- handling branched control structures
%	- handling failure continuations
%	- handling liveness issues
%	- saving and restoring heap pointers, trail tickets etc
%	- interfacing to code_exprn
%	- managing the info required by garbage collection and value numbering
%	- managing stack slots
%
%---------------------------------------------------------------------------%

:- module ll_backend__code_info.

:- interface.

:- import_module parse_tree__prog_data.
:- import_module hlds__hlds_module, hlds__hlds_pred, hlds__hlds_goal.
:- import_module hlds__hlds_data, hlds__hlds_llds, hlds__instmap.
:- import_module ll_backend__llds, ll_backend__continuation_info.
:- import_module ll_backend__trace.
:- import_module backend_libs__code_model.
:- import_module libs__globals.

:- import_module bool, set, list, map, std_util, assoc_list, counter.

:- implementation.

:- import_module parse_tree__prog_out.
:- import_module check_hlds__type_util, check_hlds__mode_util.
:- import_module ll_backend__arg_info, ll_backend__code_util.
:- import_module ll_backend__code_exprn, ll_backend__exprn_aux.
:- import_module ll_backend__var_locn.
:- import_module ll_backend__llds_out.
:- import_module libs__trace_params.
:- import_module libs__options, libs__tree.

:- import_module term, varset.
:- import_module set, stack.
:- import_module string, require, char, bimap, int.

%---------------------------------------------------------------------------%

	% Submodule for the code_info type and its access predicates.
	%
	% This submodule has the following components:
	%
	%	declarations for exported access predicates
	%	declarations for non-exported access predicates
	%	the definition of the type and the init predicate
	%	the definition of the get access predicates
	%	the definition of the set access predicates
	%
	% Please keep the order of mention of the various fields
	% consistent in each of these five components.

:- interface.

:- type code_info.

		% Create a new code_info structure. Also return the
		% outermost resumption point, and info about the non-fixed
		% stack slots used for tracing purposes.
:- pred code_info__init(bool::in, globals::in, pred_id::in, proc_id::in,
	proc_info::in, follow_vars::in, module_info::in, counter::in,
	resume_point_info::out, trace_slot_info::out, code_info::out) is det.

		% Get the globals table.
:- pred code_info__get_globals(globals::out,
	code_info::in, code_info::out) is det.

		% Get the HLDS of the entire module.
:- pred code_info__get_module_info(module_info::out,
	code_info::in, code_info::out) is det.

		% Get the id of the predicate we are generating code for.
:- pred code_info__get_pred_id(pred_id::out,
	code_info::in, code_info::out) is det.

		% Get the id of the procedure we are generating code for.
:- pred code_info__get_proc_id(proc_id::out,
	code_info::in, code_info::out) is det.

		% Get the HLDS of the procedure we are generating code for.
:- pred code_info__get_proc_info(proc_info::out,
	code_info::in, code_info::out) is det.

		% Get the variables for the current procedure.
:- pred code_info__get_varset(prog_varset::out,
	code_info::in, code_info::out) is det.

:- pred code_info__get_maybe_trace_info(maybe(trace_info)::out,
	code_info::in, code_info::out) is det.

		% Get the set of currently forward-live variables.
:- pred code_info__get_forward_live_vars(set(prog_var)::out,
	code_info::in, code_info::out) is det.

		% Set the set of currently forward-live variables.
:- pred code_info__set_forward_live_vars(set(prog_var)::in,
	code_info::in, code_info::out) is det.

		% Get the table mapping variables to the current
		% instantiation states.
:- pred code_info__get_instmap(instmap::out,
	code_info::in, code_info::out) is det.

		% Set the table mapping variables to the current
		% instantiation states.
:- pred code_info__set_instmap(instmap::in,
	code_info::in, code_info::out) is det.

		% The number of the last local label allocated.
:- pred code_info__get_label_counter(counter::out,
	code_info::in, code_info::out) is det.

		% The current value of the counter we use to give
		% each "create" rval its unique cell number, for use
		% in case the cell can be allocated statically.
:- pred code_info__get_cell_counter(counter::out,
	code_info::in, code_info::out) is det.

:- pred code_info__set_cell_counter(counter::in,
	code_info::in, code_info::out) is det.

		% Get the flag that indicates whether succip is used or not.
:- pred code_info__get_succip_used(bool::out,
	code_info::in, code_info::out) is det.

		% Get the label layout information created by tracing
		% during code generation.
:- pred code_info__get_layout_info(map(label, internal_layout_info)::out,
	code_info::in, code_info::out) is det.

		% Get the global static data structures that have
		% been created during code generation and which do
		% not have to be scanned by llds_common, since they
		% have no common parts by construction.
:- pred code_info__get_closure_layouts(list(comp_gen_c_data)::out,
	code_info::in, code_info::out) is det.

:- pred code_info__get_max_reg_in_use_at_trace(int::out,
	code_info::in, code_info::out) is det.

:- pred code_info__set_max_reg_in_use_at_trace(int::in,
	code_info::in, code_info::out) is det.

		% Get the flag which is true iff the procedure has so far
		% emitted code that creates a temporary nondet stack frame.
:- pred code_info__get_created_temp_frame(bool::out,
	code_info::in, code_info::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- pred code_info__get_var_slot_count(int::out,
	code_info::in, code_info::out) is det.

:- pred code_info__set_maybe_trace_info(maybe(trace_info)::in,
	code_info::in, code_info::out) is det.

:- pred code_info__get_opt_no_return_calls(bool::out,
	code_info::in, code_info::out) is det.

:- pred code_info__get_zombies(set(prog_var)::out,
	code_info::in, code_info::out) is det.

:- pred code_info__set_zombies(set(prog_var)::in,
	code_info::in, code_info::out) is det.

:- pred code_info__get_var_locns_info(var_locns_info::out,
	code_info::in, code_info::out) is det.

:- pred code_info__set_var_locns_info(var_locns_info::in,
	code_info::in, code_info::out) is det.

:- pred code_info__get_temps_in_use(set(lval)::out,
	code_info::in, code_info::out) is det.

:- pred code_info__set_temps_in_use(set(lval)::in,
	code_info::in, code_info::out) is det.

:- pred code_info__get_fail_info(fail_info::out,
	code_info::in, code_info::out) is det.

:- pred code_info__set_fail_info(fail_info::in,
	code_info::in, code_info::out) is det.

:- pred code_info__set_label_counter(counter::in,
	code_info::in, code_info::out) is det.

:- pred code_info__set_succip_used(bool::in,
	code_info::in, code_info::out) is det.

:- pred code_info__set_layout_info(map(label, internal_layout_info)::in,
	code_info::in, code_info::out) is det.

:- pred code_info__get_max_temp_slot_count(int::out,
	code_info::in, code_info::out) is det.

:- pred code_info__set_max_temp_slot_count(int::in,
	code_info::in, code_info::out) is det.

:- pred code_info__get_temp_content_map(map(lval, slot_contents)::out,
	code_info::in, code_info::out) is det.

:- pred code_info__set_temp_content_map(map(lval, slot_contents)::in,
	code_info::in, code_info::out) is det.

:- pred code_info__set_closure_layouts(list(comp_gen_c_data)::in,
	code_info::in, code_info::out) is det.

:- pred code_info__get_closure_seq_counter(counter::out,
	code_info::in, code_info::out) is det.

:- pred code_info__set_closure_seq_counter(counter::in,
	code_info::in, code_info::out) is det.

:- pred code_info__set_created_temp_frame(bool::in,
	code_info::in, code_info::out) is det.

%---------------------------------------------------------------------------%

	% The code_info structure has three groups of fields.
	%
	% Some fields are static; they are set when the code_info structure
	% is initialized, and never changed afterwards.
	%
	% Some fields record information about the state of the code generator
	% at a particular location in the HLDS code of the current procedure.
	% At the start of the branched control structure, the code generator
	% remembers the values of these fields, and starts generating code
	% for each branch from the same location-dependent state.
	%
	% Some fields record persistent information that does not depend
	% on a code location. Updates to these fields must remain effective
	% even when the code generator resets its location-dependent state.

:- type code_info --->
	code_info(
		code_info_static	:: code_info_static,
		code_info_loc_dep	:: code_info_loc_dep,
		code_info_persistent	:: code_info_persistent
	).

:- type code_info_static --->
	code_info_static(
		globals :: globals,
				% For the code generation options.
		module_info :: module_info,
				% The module_info structure - you just
				% never know when you might need it.
		pred_id :: pred_id,
				% The id of the current predicate.
		proc_id :: proc_id,
				% The id of the current procedure.
		proc_info :: proc_info,
				% The proc_info for the this procedure.
		varset :: prog_varset,
				% The variables in this procedure.
		var_slot_count :: int,
				% The number of stack slots allocated.
				% for storing variables.
				% (Some extra stack slots are used
				% for saving and restoring registers.)
		maybe_trace_info :: maybe(trace_info),
				% Information about which stack slots
				% the call sequence number and depth
				% are stored in, provided tracing is
				% switched on.
		opt_no_resume_calls :: bool
	).

:- type code_info_loc_dep --->
	code_info_loc_dep(
		forward_live_vars :: set(prog_var),
				% Variables that are forward live
				% after this goal.
 		instmap :: instmap,
				% Current insts of the live variables.
		zombies :: set(prog_var),
				% Zombie variables; variables that are not
				% forward live but which are protected by
				% an enclosing resume point.
		var_locns_info :: var_locns_info,
				% A map storing the information about
				% the status of each known variable.
				% (Known vars = forward live vars + zombies)
		temps_in_use :: set(lval),
				% The set of temporary locations currently in
				% use. These lvals must be all be keys in the
				% map of temporary locations ever used, which
				% is one of the persistent fields below. Any
				% keys in that map which are not in this set
				% are free for reuse.
		fail_info :: fail_info
				% Information about how to manage failures.
	).

:- type code_info_persistent --->
	code_info_persistent(
 		label_num_src :: counter,
				% Counter for the local labels used
				% by this procedure.
		cell_num_src :: counter,
				% Counter for cells in this proc.
		store_succip :: bool,
				% do we need to store succip?
		label_info :: map(label, internal_layout_info),
				% Information on which values
				% are live and where at which labels,
				% for tracing and/or accurate gc.
		stackslot_max :: int,
				% The maximum number of extra
				% temporary stackslots that have been
				% used during the procedure.
		temp_contents :: map(lval, slot_contents),
				% The temporary locations that have ever been
				% used on the stack, and what they contain.
				% Once we have used a stack slot to store
				% e.g. a ticket, we never reuse that slot
				% to hold something else, e.g. a saved hp.
				% This policy prevents us from making such
				% conflicting choices in parallel branches,
				% which would make it impossible to describe
				% to gc what the slot contains after the end
				% of the branched control structure.
		closure_layout_seq :: counter,
		closure_layouts :: list(comp_gen_c_data),
				% Closure layout structures generated by this
				% procedure.
		max_reg_used :: int,
				% At each call to MR_trace, we compute the
				% highest rN register number that contains
				% a useful value. This slot contains the
				% maximum of these highest values. Therefore
				% at all calls to MR_trace in the procedure,
				% we need only save the registers whose numbers
				% are equal to or smaller than this field.
				% This slot contains -1 if tracing is not
				% enabled.
		created_temp_frame:: bool
				% True iff the procedure has created one or
				% more temporary nondet frames.
	).

:- type var_locns_info
	--->	exprn_info(exprn_info)
	;	var_locn_info(var_locn_info).

%---------------------------------------------------------------------------%

code_info__init(SaveSuccip, Globals, PredId, ProcId, ProcInfo, FollowVars,
		ModuleInfo, CellCounter, ResumePoint, TraceSlotInfo, CodeInfo)
		:-
	proc_info_get_initial_instmap(ProcInfo, ModuleInfo, InstMap),
	proc_info_liveness_info(ProcInfo, Liveness),
	proc_info_interface_code_model(ProcInfo, CodeModel),
	arg_info__build_input_arg_list(ProcInfo, ArgList),
	proc_info_varset(ProcInfo, VarSet),
	proc_info_stack_slots(ProcInfo, StackSlots),
	globals__lookup_bool_option(Globals, lazy_code, LazyCode),
	globals__get_options(Globals, Options),
	globals__get_trace_level(Globals, TraceLevel),
	( trace_level_is_none(TraceLevel) = no ->
		trace__fail_vars(ModuleInfo, ProcInfo, FailVars),
		MaybeFailVars = yes(FailVars),
		set__union(Liveness, FailVars, EffLiveness)
	;
		MaybeFailVars = no,
		EffLiveness = Liveness
	),
	(
		LazyCode = yes,
		ArgRvalList = assoc_list__map_values(
			exprn_aux__var_lval_to_rval, ArgList),
		code_exprn__init_state(ArgRvalList, VarSet, StackSlots,
			FollowVars, Options, ExprnInfo),
		VarLocnsInfo = exprn_info(ExprnInfo)
	;
		LazyCode = no,
		var_locn__init_state(ArgList, EffLiveness, VarSet,
			StackSlots, FollowVars, Options, VarLocnInfo),
		VarLocnsInfo = var_locn_info(VarLocnInfo)
	),
	stack__init(ResumePoints),
	globals__lookup_bool_option(Globals, allow_hijacks, AllowHijack),
	(
		AllowHijack = yes,
		Hijack = allowed
	;
		AllowHijack = no,
		Hijack = not_allowed
	),
	DummyFailInfo = fail_info(ResumePoints, resume_point_unknown,
		may_be_different, not_inside_non_condition, Hijack),
	map__init(TempContentMap),
	set__init(TempsInUse),
	set__init(Zombies),
	map__init(LayoutMap),
	code_info__max_var_slot(StackSlots, VarSlotMax),
	trace__reserved_slots(ModuleInfo, ProcInfo, Globals, FixedSlots, _),
	int__max(VarSlotMax, FixedSlots, SlotMax),
	globals__lookup_bool_option(Globals, opt_no_return_calls,
		OptNoReturnCalls),
	CodeInfo0 = code_info(
		code_info_static(
			Globals,
			ModuleInfo,
			PredId,
			ProcId,
			ProcInfo,
			VarSet,
			SlotMax,
			no,
			OptNoReturnCalls
		),
		code_info_loc_dep(
			Liveness,
			InstMap,
			Zombies,
			VarLocnsInfo,
			TempsInUse,
			DummyFailInfo	% code_info__init_fail_info
					% will override this dummy value
		),
		code_info_persistent(
			counter__init(1),
			CellCounter,
			SaveSuccip,
			LayoutMap,
			0,
			TempContentMap,
			counter__init(1),
			[],
			-1,
			no
		)
	),
	code_info__init_maybe_trace_info(TraceLevel, Globals, ProcInfo,
		ModuleInfo, TraceSlotInfo, CodeInfo0, CodeInfo1),
	code_info__init_fail_info(CodeModel, MaybeFailVars, ResumePoint,
		CodeInfo1, CodeInfo).

:- pred code_info__init_maybe_trace_info(trace_level::in, globals::in,
	proc_info::in, module_info::in, trace_slot_info::out,
	code_info::in, code_info::out) is det.

code_info__init_maybe_trace_info(TraceLevel, Globals, ProcInfo, ModuleInfo,
		TraceSlotInfo) -->
	( { trace_level_is_none(TraceLevel) = no } ->
		trace__setup(ModuleInfo, ProcInfo, Globals,
			TraceSlotInfo, TraceInfo),
		code_info__set_maybe_trace_info(yes(TraceInfo))
	;
		{ TraceSlotInfo = trace_slot_info(no, no, no, no, no) }
	).

%---------------------------------------------------------------------------%

code_info__get_globals(CI^code_info_static^globals, CI, CI).
code_info__get_module_info(CI^code_info_static^module_info, CI, CI).
code_info__get_pred_id(CI^code_info_static^pred_id, CI, CI).
code_info__get_proc_id(CI^code_info_static^proc_id, CI, CI).
code_info__get_proc_info(CI^code_info_static^proc_info, CI, CI).
code_info__get_varset(CI^code_info_static^varset, CI, CI).
code_info__get_var_slot_count(CI^code_info_static^var_slot_count, CI, CI).
code_info__get_maybe_trace_info(CI^code_info_static^maybe_trace_info, CI, CI).
code_info__get_opt_no_return_calls(CI^code_info_static^opt_no_resume_calls,
	CI, CI).
code_info__get_forward_live_vars(CI^code_info_loc_dep^forward_live_vars,
	CI, CI).
code_info__get_instmap(CI^code_info_loc_dep^instmap, CI, CI).
code_info__get_zombies(CI^code_info_loc_dep^zombies, CI, CI).
code_info__get_var_locns_info(CI^code_info_loc_dep^var_locns_info, CI, CI).
code_info__get_temps_in_use(CI^code_info_loc_dep^temps_in_use, CI, CI).
code_info__get_fail_info(CI^code_info_loc_dep^fail_info, CI, CI).
code_info__get_label_counter(CI^code_info_persistent^label_num_src, CI, CI).
code_info__get_cell_counter(CI^code_info_persistent^cell_num_src, CI, CI).
code_info__get_succip_used(CI^code_info_persistent^store_succip, CI, CI).
code_info__get_layout_info(CI^code_info_persistent^label_info, CI, CI).
code_info__get_max_temp_slot_count(CI^code_info_persistent^stackslot_max,
	CI, CI).
code_info__get_temp_content_map(CI^code_info_persistent^temp_contents, CI, CI).
code_info__get_closure_seq_counter(CI^code_info_persistent^closure_layout_seq,
	CI, CI).
code_info__get_closure_layouts(CI^code_info_persistent^closure_layouts,
	CI, CI).
code_info__get_max_reg_in_use_at_trace(CI^code_info_persistent^max_reg_used,
	CI, CI).
code_info__get_created_temp_frame(CI^code_info_persistent^created_temp_frame,
	CI, CI).

%---------------------------------------------------------------------------%

code_info__set_maybe_trace_info(TI, CI,
	CI^code_info_static^maybe_trace_info := TI).
code_info__set_forward_live_vars(LV, CI,
	CI^code_info_loc_dep^forward_live_vars := LV).
code_info__set_instmap(IM, CI, CI^code_info_loc_dep^instmap := IM).
code_info__set_zombies(Zs, CI, CI^code_info_loc_dep^zombies := Zs).
code_info__set_var_locns_info(EI, CI,
	CI^code_info_loc_dep^var_locns_info := EI).
code_info__set_temps_in_use(TI, CI, CI^code_info_loc_dep^temps_in_use := TI).
code_info__set_fail_info(FI, CI, CI^code_info_loc_dep^fail_info := FI).
code_info__set_label_counter(LC, CI,
	CI^code_info_persistent^label_num_src := LC).
code_info__set_cell_counter(CC, CI,
	CI^code_info_persistent^cell_num_src := CC).
code_info__set_succip_used(SU, CI, CI^code_info_persistent^store_succip := SU).
code_info__set_layout_info(LI, CI, CI^code_info_persistent^label_info := LI).
code_info__set_max_temp_slot_count(TM, CI,
	CI^code_info_persistent^stackslot_max := TM).
code_info__set_temp_content_map(CM, CI,
	CI^code_info_persistent^temp_contents := CM).
code_info__set_closure_seq_counter(CLS, CI,
	CI^code_info_persistent^closure_layout_seq := CLS).
code_info__set_closure_layouts(CG, CI,
	CI^code_info_persistent^closure_layouts := CG).
code_info__set_max_reg_in_use_at_trace(MR, CI,
	CI^code_info_persistent^max_reg_used := MR).
code_info__set_created_temp_frame(MR, CI,
	CI^code_info_persistent^created_temp_frame := MR).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

	% Submodule for simple wrappers around access predicates.

:- interface.

		% Get the hlds mapping from variables to stack slots
:- pred code_info__get_stack_slots(stack_slots, code_info, code_info).
:- mode code_info__get_stack_slots(out, in, out) is det.

		% Get the table that contains advice about where
		% variables should be put.
:- pred code_info__get_follow_var_map(follow_vars_map, code_info, code_info).
:- mode code_info__get_follow_var_map(out, in, out) is det.

		% Get the integer that gives the number of the next
		% non-reserved register.
:- pred code_info__get_next_non_reserved(int, code_info, code_info).
:- mode code_info__get_next_non_reserved(out, in, out) is det.

		% Set the table that contains advice about where
		% variables should be put.
:- pred code_info__set_follow_vars(follow_vars, code_info, code_info).
:- mode code_info__set_follow_vars(in, in, out) is det.

	% code_info__pre_goal_update(GoalInfo, Atomic, OldCodeInfo, NewCodeInfo)
	% updates OldCodeInfo to produce NewCodeInfo with the changes
	% specified by GoalInfo.
:- pred code_info__pre_goal_update(hlds_goal_info, bool, code_info, code_info).
:- mode code_info__pre_goal_update(in, in, in, out) is det.

	% code_info__post_goal_update(GoalInfo, OldCodeInfo, NewCodeInfo)
	% updates OldCodeInfo to produce NewCodeInfo with the changes described
	% by GoalInfo.
:- pred code_info__post_goal_update(hlds_goal_info, code_info, code_info).
:- mode code_info__post_goal_update(in, in, out) is det.

	% Find out whether the body of the current procedure should use
	% typeinfo liveness.
:- pred code_info__body_typeinfo_liveness(bool::out,
	code_info::in, code_info::out) is det.

	% Find out the type of the given variable.
:- pred code_info__variable_type(prog_var, type, code_info, code_info).
:- mode code_info__variable_type(in, out, in, out) is det.

:- pred code_info__lookup_type_defn(type, hlds_type_defn,
	code_info, code_info).
:- mode code_info__lookup_type_defn(in, out, in, out) is det.

	% Given a constructor id, and a variable (so that we can work out the
	% type of the constructor), determine correct tag (representation)
	% of that constructor.
:- pred code_info__cons_id_to_tag(prog_var, cons_id, cons_tag,
		code_info, code_info).
:- mode code_info__cons_id_to_tag(in, in, out, in, out) is det.

	% Get the code model of the current procedure.
:- pred code_info__get_proc_model(code_model, code_info, code_info).
:- mode code_info__get_proc_model(out, in, out) is det.

	% Get the list of the head variables of the current procedure.
:- pred code_info__get_headvars(list(prog_var), code_info, code_info).
:- mode code_info__get_headvars(out, in, out) is det.

	% Get the call argument information for the current procedure
:- pred code_info__get_arginfo(list(arg_info), code_info, code_info).
:- mode code_info__get_arginfo(out, in, out) is det.

	% Get the call argument info for a given mode of a given predicate
:- pred code_info__get_pred_proc_arginfo(pred_id, proc_id, list(arg_info),
	code_info, code_info).
:- mode code_info__get_pred_proc_arginfo(in, in, out, in, out) is det.

	% Get the set of variables currently needed by the resume
	% points of enclosing goals.
:- pred code_info__current_resume_point_vars(set(prog_var),
		code_info, code_info).
:- mode code_info__current_resume_point_vars(out, in, out) is det.

:- pred code_info__variable_to_string(prog_var, string, code_info, code_info).
:- mode code_info__variable_to_string(in, out, in, out) is det.

	% Create a code address which holds the address of the specified
	% procedure.
	% The fourth argument should be `no' if the the caller wants the
	% returned address to be valid from everywhere in the program.
	% If being valid from within the current procedure is enough,
	% this argument should be `yes' wrapped around the value of the
	% --procs-per-c-function option and the current procedure id.
	% Using an address that is only valid from within the current
	% procedure may make jumps more efficient.
	%
	% If the procs_per_c_function option tells us to put more than one
	% procedure into each C function, but not all procedures in the module
	% are in one function, then we would like to be able to use the
	% fast form of reference to a procedure for references not only from
	% within the same procedure but also from other procedures within
	% the same C function. However, at the time of code generation,
	% we do not yet know which procedures will be put into the same
	% C functions, and so we cannot do this.
:- pred code_info__make_entry_label(module_info, pred_id, proc_id, bool,
	code_addr, code_info, code_info).
:- mode code_info__make_entry_label(in, in, in, in, out, in, out) is det.

	% Generate the next local label in sequence.
:- pred code_info__get_next_label(label, code_info, code_info).
:- mode code_info__get_next_label(out, in, out) is det.

	% Generate the next cell number in sequence.
:- pred code_info__get_next_cell_number(int, code_info, code_info).
:- mode code_info__get_next_cell_number(out, in, out) is det.

	% Note that the succip slot is used, and thus cannot be
	% optimized away.
:- pred code_info__succip_is_used(code_info, code_info).
:- mode code_info__succip_is_used(in, out) is det.

:- pred code_info__add_trace_layout_for_label(label, term__context,
	trace_port, goal_path, layout_label_info, code_info, code_info).
:- mode code_info__add_trace_layout_for_label(in, in, in,in,  in, in, out)
	is det.

:- pred code_info__get_cur_proc_label(proc_label, code_info, code_info).
:- mode code_info__get_cur_proc_label(out, in, out) is det.

:- pred code_info__get_next_closure_seq_no(int, code_info, code_info).
:- mode code_info__get_next_closure_seq_no(out, in, out) is det.

:- pred code_info__add_closure_layout(comp_gen_c_data, code_info, code_info).
:- mode code_info__add_closure_layout(in, in, out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- pred code_info__add_resume_layout_for_label(label, layout_label_info,
	code_info, code_info).
:- mode code_info__add_resume_layout_for_label(in, in, in, out) is det.

code_info__get_stack_slots(StackSlots, CI, CI) :-
	code_info__get_var_locns_info(VarInfo, CI, _),
	(
		VarInfo = exprn_info(ExprnInfo),
		code_exprn__get_stack_slots(StackSlots, ExprnInfo, _)
	;
		VarInfo = var_locn_info(VarLocnInfo),
		var_locn__get_stack_slots(StackSlots, VarLocnInfo, _)
	).

code_info__get_follow_var_map(FollowVarMap, CI, CI) :-
	code_info__get_var_locns_info(VarInfo, CI, _),
	(
		VarInfo = exprn_info(ExprnInfo),
		code_exprn__get_follow_vars(FollowVars, ExprnInfo, _),
		FollowVars = follow_vars(FollowVarMap, _)
	;
		VarInfo = var_locn_info(VarLocnInfo),
		var_locn__get_follow_var_map(FollowVarMap, VarLocnInfo, _)
	).

code_info__get_next_non_reserved(NextNonReserved, CI, CI) :-
	code_info__get_var_locns_info(VarInfo, CI, _),
	(
		VarInfo = exprn_info(ExprnInfo),
		code_exprn__get_follow_vars(FollowVars, ExprnInfo, _),
		FollowVars = follow_vars(_, NextNonReserved)
	;
		VarInfo = var_locn_info(VarLocnInfo),
		var_locn__get_next_non_reserved(NextNonReserved, VarLocnInfo,
			_)
	).

code_info__set_follow_vars(FollowVars, CI0, CI) :-
	code_info__get_var_locns_info(VarInfo0, CI0, _),
	(
		VarInfo0 = exprn_info(ExprnInfo0),
		code_exprn__set_follow_vars(FollowVars, ExprnInfo0, ExprnInfo),
		VarInfo = exprn_info(ExprnInfo)
	;
		VarInfo0 = var_locn_info(VarLocnInfo0),
		var_locn__set_follow_vars(FollowVars,
			VarLocnInfo0, VarLocnInfo),
		VarInfo = var_locn_info(VarLocnInfo)
	),
	code_info__set_var_locns_info(VarInfo, CI0, CI).

%-----------------------------------------------------------------------------%

	% Update the code info structure to be consistent
	% immediately prior to generating a goal.
code_info__pre_goal_update(GoalInfo, Atomic) -->
	% The liveness pass puts resume_point annotations on some kinds
	% of goals. The parts of the code generator that handle those kinds
	% of goals should handle the resume point annotation as well;
	% when they do, they remove the annotation. The following code
	% is a sanity check to make sure that this has in fact been done.
	{ goal_info_get_resume_point(GoalInfo, ResumePoint) },
	(
		{ ResumePoint = no_resume_point }
	;
		{ ResumePoint = resume_point(_, _) },
		{ error("pre_goal_update with resume point") }
	),
	{ goal_info_get_follow_vars(GoalInfo, MaybeFollowVars) },
	(
		{ MaybeFollowVars = yes(FollowVars) },
		code_info__set_follow_vars(FollowVars)
	;
		{ MaybeFollowVars = no }
	),
	% note: we must be careful to apply deaths before births
	{ goal_info_get_pre_deaths(GoalInfo, PreDeaths) },
	code_info__rem_forward_live_vars(PreDeaths),
	code_info__maybe_make_vars_forward_dead(PreDeaths, no),
	{ goal_info_get_pre_births(GoalInfo, PreBirths) },
	code_info__add_forward_live_vars(PreBirths),
	( { Atomic = yes } ->
		{ goal_info_get_post_deaths(GoalInfo, PostDeaths) },
		code_info__rem_forward_live_vars(PostDeaths)
	;
		[]
	).

	% Update the code info structure to be consistent
	% immediately after generating a goal.
code_info__post_goal_update(GoalInfo) -->
	% note: we must be careful to apply deaths before births
	{ goal_info_get_post_deaths(GoalInfo, PostDeaths) },
	code_info__rem_forward_live_vars(PostDeaths),
	code_info__maybe_make_vars_forward_dead(PostDeaths, no),
	{ goal_info_get_post_births(GoalInfo, PostBirths) },
	code_info__add_forward_live_vars(PostBirths),
	code_info__make_vars_forward_live(PostBirths),
	{ goal_info_get_instmap_delta(GoalInfo, InstMapDelta) },
	code_info__get_instmap(InstMap0),
	{ instmap__apply_instmap_delta(InstMap0, InstMapDelta, InstMap) },
	code_info__set_instmap(InstMap).

%---------------------------------------------------------------------------%

code_info__body_typeinfo_liveness(TypeInfoLiveness) -->
	code_info__get_module_info(ModuleInfo),
	code_info__get_pred_id(PredId),
	{ module_info_pred_info(ModuleInfo, PredId, PredInfo) },
	code_info__get_globals(Globals),
	{ body_should_use_typeinfo_liveness(PredInfo, Globals,
		TypeInfoLiveness) }.

:- pred code_info__get_var_types(map(prog_var, type), code_info, code_info).
:- mode code_info__get_var_types(out, in, out) is det.

code_info__get_var_types(VarTypes) -->
	code_info__get_proc_info(ProcInfo),
	{ proc_info_vartypes(ProcInfo, VarTypes) }.

code_info__variable_type(Var, Type) -->
	code_info__get_var_types(VarTypes),
	{ map__lookup(VarTypes, Var, Type) }.

code_info__lookup_type_defn(Type, TypeDefn) -->
	code_info__get_module_info(ModuleInfo),
	{ type_to_ctor_and_args(Type, TypeCtorPrime, _) ->
		TypeCtor = TypeCtorPrime
	;
		error("unknown type in code_info__lookup_type_defn")
	},
	{ module_info_types(ModuleInfo, TypeTable) },
	{ map__lookup(TypeTable, TypeCtor, TypeDefn) }.

code_info__cons_id_to_tag(Var, ConsId, ConsTag) -->
	code_info__variable_type(Var, Type),
	code_info__get_module_info(ModuleInfo),
	{ code_util__cons_id_to_tag(ConsId, Type, ModuleInfo, ConsTag) }.

%---------------------------------------------------------------------------%

code_info__get_proc_model(CodeModel) -->
	code_info__get_proc_info(ProcInfo),
	{ proc_info_interface_code_model(ProcInfo, CodeModel) }.

code_info__get_headvars(HeadVars) -->
	code_info__get_module_info(ModuleInfo),
	code_info__get_pred_id(PredId),
	code_info__get_proc_id(ProcId),
	{ module_info_pred_proc_info(ModuleInfo, PredId, ProcId, _, ProcInfo) },
	{ proc_info_headvars(ProcInfo, HeadVars) }.

code_info__get_arginfo(ArgInfo) -->
	code_info__get_pred_id(PredId),
	code_info__get_proc_id(ProcId),
	code_info__get_pred_proc_arginfo(PredId, ProcId, ArgInfo).

code_info__get_pred_proc_arginfo(PredId, ProcId, ArgInfo) -->
	code_info__get_module_info(ModuleInfo),
	{ module_info_pred_proc_info(ModuleInfo, PredId, ProcId, _, ProcInfo) },
	{ proc_info_arg_info(ProcInfo, ArgInfo) }.

code_info__current_resume_point_vars(ResumeVars) -->
	code_info__get_fail_info(FailInfo),
	{ FailInfo = fail_info(ResumePointStack, _, _, _, _) },
	{ stack__top_det(ResumePointStack, ResumePointInfo) },
	{ code_info__pick_first_resume_point(ResumePointInfo, ResumeMap, _) },
	{ map__keys(ResumeMap, ResumeMapVarList) },
	{ set__list_to_set(ResumeMapVarList, ResumeVars) }.

code_info__variable_to_string(Var, Name) -->
	code_info__get_varset(Varset),
	{ varset__lookup_name(Varset, Var, Name) }.

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

code_info__get_next_label(Label) -->
	code_info__get_module_info(ModuleInfo),
	code_info__get_pred_id(PredId),
	code_info__get_proc_id(ProcId),
	code_info__get_label_counter(C0),
	{ counter__allocate(N, C0, C) },
	code_info__set_label_counter(C),
	{ code_util__make_internal_label(ModuleInfo, PredId, ProcId, N,
		Label) }.

code_info__get_next_cell_number(N) -->
	code_info__get_cell_counter(C0),
	{ counter__allocate(N, C0, C) },
	code_info__set_cell_counter(C).

code_info__succip_is_used -->
	code_info__set_succip_used(yes).

code_info__add_trace_layout_for_label(Label, Context, Port, Path, Layout) -->
	code_info__get_layout_info(Internals0),
	{ Exec = yes(trace_port_layout_info(Context, Port, Path, Layout)) },
	{ map__search(Internals0, Label, Internal0) ->
		Internal0 = internal_layout_info(Exec0, Resume, Return),
		( Exec0 = no ->
			true
		;
			error("adding trace layout for already known label")
		),
		Internal = internal_layout_info(Exec, Resume, Return),
		map__set(Internals0, Label, Internal, Internals)
	;
		Internal = internal_layout_info(Exec, no, no),
		map__det_insert(Internals0, Label, Internal, Internals)
	},
	code_info__set_layout_info(Internals).

code_info__add_resume_layout_for_label(Label, LayoutInfo) -->
	code_info__get_layout_info(Internals0),
	{ Resume = yes(LayoutInfo) },
	{ map__search(Internals0, Label, Internal0) ->
		Internal0 = internal_layout_info(Exec, Resume0, Return),
		( Resume0 = no ->
			true
		;
			error("adding gc layout for already known label")
		),
		Internal = internal_layout_info(Exec, Resume, Return),
		map__set(Internals0, Label, Internal, Internals)
	;
		Internal = internal_layout_info(no, Resume, no),
		map__det_insert(Internals0, Label, Internal, Internals)
	},
	code_info__set_layout_info(Internals).

:- pred code_info__get_active_temps_data(assoc_list(lval, slot_contents),
	code_info, code_info).
:- mode code_info__get_active_temps_data(out, in, out) is det.

code_info__get_active_temps_data(Temps) -->
	code_info__get_temps_in_use(TempsInUse),
	code_info__get_temp_content_map(TempContentMap),
	{ map__select(TempContentMap, TempsInUse, TempsInUseContentMap) },
	{ map__to_assoc_list(TempsInUseContentMap, Temps) }.

code_info__get_cur_proc_label(ProcLabel) -->
	code_info__get_module_info(ModuleInfo),
	code_info__get_pred_id(PredId),
	code_info__get_proc_id(ProcId),
	{ code_util__make_proc_label(ModuleInfo, PredId, ProcId, ProcLabel) }.

code_info__get_next_closure_seq_no(SeqNo) -->
	code_info__get_closure_seq_counter(C0),
	{ counter__allocate(SeqNo, C0, C) },
	code_info__set_closure_seq_counter(C).

code_info__add_closure_layout(ClosureLayout) -->
	code_info__get_closure_layouts(ClosureLayouts),
	code_info__set_closure_layouts([ClosureLayout | ClosureLayouts]).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

	% Submodule for handling branched control structures.

:- interface.

:- type position_info.
:- type branch_end_info.

:- type branch_end	==	maybe(branch_end_info).

:- pred code_info__remember_position(position_info, code_info, code_info).
:- mode code_info__remember_position(out, in, out) is det.

:- pred code_info__reset_to_position(position_info, code_info, code_info).
:- mode code_info__reset_to_position(in, in, out) is det.

:- pred code_info__reset_resume_known(position_info, code_info, code_info).
:- mode code_info__reset_resume_known(in, in, out) is det.

:- pred code_info__generate_branch_end(store_map, branch_end, branch_end,
	code_tree, code_info, code_info).
:- mode code_info__generate_branch_end(in, in, out, out, in, out) is det.

:- pred code_info__after_all_branches(store_map, branch_end,
	code_info, code_info).
:- mode code_info__after_all_branches(in, in, in, out) is det.

:- pred code_info__save_hp_in_branch(code_tree, lval, position_info,
	position_info).
:- mode code_info__save_hp_in_branch(out, out, in, out) is det.

:- implementation.

:- type position_info
	--->	position_info(
			code_info	% The code_info at a given position
					% in the code of the procedure.
		).

:- type branch_end_info
	--->	branch_end_info(
			code_info	% The code_info at the end of a branch.
		).

code_info__remember_position(position_info(C), C, C).

code_info__reset_to_position(position_info(PosCI), CurCI, NextCI) :-
	PosCI  = code_info(_, LocDep, _),
	CurCI  = code_info(Static, _, Persistent),
	NextCI = code_info(Static, LocDep, Persistent).

code_info__reset_resume_known(BranchStart) -->
	{ BranchStart = position_info(BranchStartCI) },
	{ code_info__get_fail_info(BranchStartFailInfo, BranchStartCI, _) },
	{ BranchStartFailInfo = fail_info(_, BSResumeKnown, _, _, _) },
	code_info__get_fail_info(CurFailInfo),
	{ CurFailInfo = fail_info(CurFailStack, _,
		CurCurfMaxfr, CurCondEnv, CurHijack) },
	{ NewFailInfo = fail_info(CurFailStack, BSResumeKnown,
		CurCurfMaxfr, CurCondEnv, CurHijack) },
	code_info__set_fail_info(NewFailInfo).

code_info__generate_branch_end(StoreMap, MaybeEnd0, MaybeEnd, Code) -->
	{ map__to_assoc_list(StoreMap, VarLocs) },
	code_info__get_var_locns_info(VarInfo),
	(
		{ VarInfo = exprn_info(_) }
	;
			% The eager code generator generates better code
			% if it knows in advance where each variable should
			% go. We don't need to reset the follow_vars
			% afterwards, since every goal following a branched
			% control structure must in any case be annotated with
			% its own follow_var set.
		{ VarInfo = var_locn_info(_) },
		{ map__from_assoc_list(VarLocs, FollowVarsMap) },
		{ assoc_list__values(VarLocs, Locs) },
		{ code_util__max_mentioned_reg(Locs, MaxMentionedReg) },
		code_info__set_follow_vars(follow_vars(FollowVarsMap,
			MaxMentionedReg + 1))
	),
	code_info__get_instmap(InstMap),
	( { instmap__is_reachable(InstMap) } ->
		code_info__place_vars(VarLocs, Code)
	;
		% With --opt-no-return-call, the variables that we would have
		% saved across a call that cannot return have had the last
		% of their code generation state destroyed, so calling
		% place_vars would cause a code generator abort. However,
		% pretending that all the variables are where the store map
		% says they should be is perfectly fine, since we can never
		% reach the end of *this* branch anyway.
		code_info__remake_with_store_map(StoreMap),
		{ Code = empty }
	),
	=(EndCodeInfo1),
	{
		MaybeEnd0 = no,
		EndCodeInfo = EndCodeInfo1
	;
		MaybeEnd0 = yes(branch_end_info(EndCodeInfo0)),

			% Make sure the left context we leave the
			% branched structure with is valid for all branches.
		code_info__get_fail_info(FailInfo0, EndCodeInfo0, _),
		code_info__get_fail_info(FailInfo1, EndCodeInfo1, _),
		FailInfo0 = fail_info(_, ResumeKnown0, CurfrMaxfr0,
			CondEnv0, Hijack0),
		FailInfo1 = fail_info(R, ResumeKnown1, CurfrMaxfr1,
			CondEnv1, Hijack1),
		(
			ResumeKnown0 = resume_point_known(Redoip0),
			ResumeKnown1 = resume_point_known(Redoip1)
		->
			ResumeKnown = resume_point_known(Redoip0),
			require(unify(Redoip0, Redoip1),
				"redoip mismatch in generate_branch_end")
		;
			ResumeKnown = resume_point_unknown
		),
		(
			CurfrMaxfr0 = must_be_equal,
			CurfrMaxfr1 = must_be_equal
		->
			CurfrMaxfr = must_be_equal
		;
			CurfrMaxfr = may_be_different
		),
		(
			Hijack0 = allowed,
			Hijack1 = allowed
		->
			Hijack = allowed
		;
			Hijack = not_allowed
		),
		require(unify(CondEnv0, CondEnv1),
			"some but not all branches inside a non condition"),
		FailInfo = fail_info(R, ResumeKnown, CurfrMaxfr,
			CondEnv0, Hijack),
		code_info__set_fail_info(FailInfo, EndCodeInfo1, EndCodeInfoA),

			% Make sure the "temps in use" set at the end of the
			% branched control structure includes every slot
			% in use at the end of any branch.
		code_info__get_temps_in_use(TempsInUse0, EndCodeInfo0, _),
		code_info__get_temps_in_use(TempsInUse1, EndCodeInfo1, _),
		set__union(TempsInUse0, TempsInUse1, TempsInUse),
		code_info__set_temps_in_use(TempsInUse, EndCodeInfoA,
			EndCodeInfo)
	},
	{ MaybeEnd = yes(branch_end_info(EndCodeInfo)) }.

code_info__after_all_branches(StoreMap, MaybeEnd, CI0, CI) :-
	(
		MaybeEnd = yes(BranchEnd),
		BranchEnd = branch_end_info(BranchEndCodeInfo),
		code_info__reset_to_position(position_info(BranchEndCodeInfo),
			CI0, CI1),
		code_info__remake_with_store_map(StoreMap, CI1, CI)
	;
		MaybeEnd = no,
		error("no branches in branched control structure")
	).

	% code_info__remake_with_store_map throws away the var_info data
	% structure, forgetting the current locations of all variables,
	% and rebuilds it from scratch based on the given store map.
	% The new var_info will know about only the variables present
	% in the store map, and will believe they are where the store map
	% says they are.

:- pred code_info__remake_with_store_map(store_map, code_info, code_info).
:- mode code_info__remake_with_store_map(in, in, out) is det.

code_info__remake_with_store_map(StoreMap) -->
	{ map__to_assoc_list(StoreMap, VarLvals) },
	code_info__get_var_locns_info(VarInfo0),
	{
		VarInfo0 = exprn_info(Exprn0),
		VarRvals = assoc_list__map_values(exprn_aux__var_lval_to_rval,
			VarLvals),
		code_exprn__reinit_state(VarRvals, Exprn0, Exprn),
		VarInfo = exprn_info(Exprn)
	;
		VarInfo0 = var_locn_info(VarLocnInfo0),
		var_locn__reinit_state(VarLvals, VarLocnInfo0, VarLocnInfo),
		VarInfo = var_locn_info(VarLocnInfo)
	},
	code_info__set_var_locns_info(VarInfo).

code_info__save_hp_in_branch(Code, Slot, Pos0, Pos) :-
	Pos0 = position_info(CodeInfo0),
	code_info__save_hp(Code, Slot, CodeInfo0, CodeInfo),
	Pos  = position_info(CodeInfo).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

	% Submodule for the handling of failure continuations.

	% The principles underlying this submodule of code_info.m are
	% documented in the file compiler/notes/failure.html, which also
	% defines terms such as "quarter hijack"). Some parts of the submodule
	% also require knowledge of compiler/notes/allocation.html.

:- interface.

:- type resume_map.

:- type resume_point_info.

	% `prepare_for_disj_hijack' should be called before entering
	% a disjunction. It saves the values of any nondet stack slots
	% the disjunction may hijack, and if necessary, sets the redofr
	% slot of the top frame to point to this frame. The code at the
	% start of the individual disjuncts will override the redoip slot.
	%
	% `undo_disj_hijack' should be called before entering the last
	% disjunct of a disjunction. It undoes the effects of
	% `prepare_for_disj_hijack'.

:- type disj_hijack_info.

:- pred code_info__prepare_for_disj_hijack(code_model::in,
	disj_hijack_info::out, code_tree::out,
	code_info::in, code_info::out) is det.

:- pred code_info__undo_disj_hijack(disj_hijack_info::in,
	code_tree::out, code_info::in, code_info::out) is det.

	% `prepare_for_ite_hijack' should be called before entering
	% an if-then-else. It saves the values of any nondet stack slots
	% the if-then-else may hijack, and if necessary, sets the redofr
	% slot of the top frame to point to this frame. Our caller
	% will then override the redoip slot to point to the start of
	% the else part before generating the code of the condition.
	%
	% `ite_enter_then', which should be called generating code for
	% the condition, sets up the failure state of the code generator
	% for generating the then-part, and returns the code sequences
	% to be used at the starts of the then-part and the else-part
	% to undo the effects of any hijacking.

:- type ite_hijack_info.

:- pred code_info__prepare_for_ite_hijack(code_model::in,
	ite_hijack_info::out, code_tree::out,
	code_info::in, code_info::out) is det.

:- pred code_info__ite_enter_then(ite_hijack_info::in,
	code_tree::out, code_tree::out, code_info::in, code_info::out) is det.

	% `enter_simple_neg' and `leave_simple_neg' should be called before
	% and after generating the code for a negated unification, in
	% situations where failure is a direct branch. We handle this case
	% specially, because it occurs frequently and should not require
	% a flushing of the expression cache, whereas the general way of
	% handling negations does require a flush. These two predicates
	% handle all aspects of the negation except for the unification
	% itself.

:- type simple_neg_info.

:- pred code_info__enter_simple_neg(set(prog_var)::in, hlds_goal_info::in,
	simple_neg_info::out, code_info::in, code_info::out) is det.

:- pred code_info__leave_simple_neg(hlds_goal_info::in, simple_neg_info::in,
	code_info::in, code_info::out) is det.

	% `prepare_for_det_commit' and `generate_det_commit' should be
	% called before and after generating the code for the multi goal
	% being cut across. If the goal succeeds, the commit will cut
	% any choice points generated in the goal.

:- type det_commit_info.

:- pred code_info__prepare_for_det_commit(det_commit_info::out,
	code_tree::out, code_info::in, code_info::out) is det.

:- pred code_info__generate_det_commit(det_commit_info::in,
	code_tree::out, code_info::in, code_info::out) is det.

	% `prepare_for_semi_commit' and `generate_semi_commit' should be
	% called before and after generating the code for the nondet goal
	% being cut across. If the goal succeeds, the commit will cut
	% any choice points generated in the goal.

:- type semi_commit_info.

:- pred code_info__prepare_for_semi_commit(semi_commit_info::out,
	code_tree::out, code_info::in, code_info::out) is det.

:- pred code_info__generate_semi_commit(semi_commit_info::in,
	code_tree::out, code_info::in, code_info::out) is det.

	% Put the given resume point into effect, by pushing it on to
	% the resume point stack, and if necessary generating code to
	% override the redoip of the top nondet stack frame.

:- pred code_info__effect_resume_point(resume_point_info::in, code_model::in,
	code_tree::out, code_info::in, code_info::out) is det.

:- pred code_info__pop_resume_point(code_info::in, code_info::out) is det.

	% Return the details of the resume point currently on top of the
	% failure continuation stack.

:- pred code_info__top_resume_point(resume_point_info::out,
	code_info::in, code_info::out) is det.

	% Call this predicate to say "we have just left a disjunction;
	% we don't know what address the following code will need to
	% backtrack to".

:- pred code_info__set_resume_point_to_unknown(code_info::in, code_info::out)
	is det.

	% Call this predicate to say "we have just returned from a model_non
	% call; we don't know what address the following code will need to
	% backtrack to, and there may now be nondet frames on top of ours
	% that do not have their redofr slots pointing to our frame".

:- pred code_info__set_resume_point_and_frame_to_unknown(code_info::in,
	code_info::out) is det.

	% Generate code for executing a failure that is appropriate for the
	% current failure environment.

:- pred code_info__generate_failure(code_tree::out,
	code_info::in, code_info::out) is det.

	% Generate code that checks if the given rval is false, and if yes,
	% executes a failure that is appropriate for the current failure
	% environment.

:- pred code_info__fail_if_rval_is_false(rval::in, code_tree::out,
	code_info::in, code_info::out) is det.

	% Checks whether the appropriate code for failure in the current
	% failure environment is a direct branch.

:- pred code_info__failure_is_direct_branch(code_addr::out,
	code_info::in, code_info::out) is semidet.

	% Checks under what circumstances the current failure environment
	% would allow a model_non call at this point to be turned into a
	% tail call, provided of course that the return from the call is
	% followed immediately by succeed().

:- pred code_info__may_use_nondet_tailcall(nondet_tail_call::out,
	code_info::in, code_info::out) is det.

	% Materialize the given variables into registers or stack slots.

:- pred code_info__produce_vars(set(prog_var)::in, resume_map::out,
	code_tree::out, code_info::in, code_info::out) is det.

	% Put the variables needed in enclosing failure continuations
	% into their stack slots.

:- pred code_info__flush_resume_vars_to_stack(code_tree::out,
	code_info::in, code_info::out) is det.

	% Set up the resume_point_info structure.

:- pred code_info__make_resume_point(set(prog_var)::in, resume_locs::in,
	resume_map::in, resume_point_info::out, code_info::in, code_info::out)
	is det.

	% Generate the code for a resume point.

:- pred code_info__generate_resume_point(resume_point_info::in,
	code_tree::out, code_info::in, code_info::out) is det.

	% List the variables that need to be preserved for the given
	% resume point.

:- pred code_info__resume_point_vars(resume_point_info::in, list(prog_var)::out)
	is det.

	% See whether the given resume point includes a code address
	% that presumes all the resume point variables to be in their
	% stack slots. If yes, return that code address; otherwise,
	% abort the compiler.

:- pred code_info__resume_point_stack_addr(resume_point_info::in,
	code_addr::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

	% The part of the code generator state that says how to handle
	% failures; also called the failure continuation stack.

:- type fail_info
	--->	fail_info(
			stack(resume_point_info),
			resume_point_known,
			curfr_vs_maxfr,
			condition_env,
			hijack_allowed
		).

	% A resumption point has one or two labels associated with it.
	% Backtracking can arrive at either label. The code following
	% each label will assume that the variables needed at the resumption
	% point are in the locations given by the resume_map associated with
	% the given label and nowhere else. Any code that can cause
	% backtracking to a label must make sure that those variables are
	% in the positions expected by the label.
	%
	% The only time when a code_addr in a resume_point info is not a label
	% is when the code_addr is do_fail, which indicates that the resumption
	% point is not in (this invocation of) this procedure.

:- type resume_point_info
	--->	orig_only(resume_map, code_addr)
	;	stack_only(resume_map, code_addr)
	;	orig_and_stack(resume_map, code_addr, resume_map, code_addr)
	;	stack_and_orig(resume_map, code_addr, resume_map, code_addr).

	% A resume map maps the variables that will be needed at a resumption
	% point to the locations in which they will be.

:- type resume_map		==	map(prog_var, set(lval)).

:- type redoip_update		--->	has_been_done
				;	wont_be_done.

:- type resume_point_known	--->	resume_point_known(redoip_update)
				;	resume_point_unknown.

:- type curfr_vs_maxfr		--->	must_be_equal
				;	may_be_different.

:- type condition_env		--->	inside_non_condition
				;	not_inside_non_condition.

:- type hijack_allowed		--->	allowed
				;	not_allowed.

%---------------------------------------------------------------------------%

:- type disj_hijack_info
	--->	disj_no_hijack
	;	disj_temp_frame
	;	disj_quarter_hijack
	;	disj_half_hijack(
			lval		% The stack slot in which we saved
					% the value of the hijacked redoip.
		)
	;	disj_full_hijack(
			lval,		% The stack slot in which we saved
					% the value of the hijacked redoip.
			lval		% The stack slot in which we saved
					% the value of the hijacked redofr.
		).

code_info__prepare_for_disj_hijack(CodeModel, HijackInfo, Code) -->
	code_info__get_fail_info(FailInfo),
	{ FailInfo = fail_info(_, ResumeKnown, CurfrMaxfr, CondEnv, Allow) },
	(
		{ CodeModel \= model_non }
	->
		{ HijackInfo = disj_no_hijack },
		{ Code = node([
			comment("disj no hijack")
				- ""
		]) }
	;
		{ Allow = not_allowed ; CondEnv = inside_non_condition }
	->
		{ HijackInfo = disj_temp_frame },
		code_info__create_temp_frame(do_fail,
			"prepare for disjunction", Code)
	;
		{ CurfrMaxfr = must_be_equal },
		{ ResumeKnown = resume_point_known(has_been_done) }
	->
		{ HijackInfo = disj_quarter_hijack },
		{ Code = node([
			comment("disj quarter hijack")
				- ""
		]) }
	;
		{ CurfrMaxfr = must_be_equal }
	->
		% Here ResumeKnown must be resume_point_unknown
		% or resume_point_known(wont_be_done).
		code_info__acquire_temp_slot(lval(redoip(lval(curfr))),
			RedoipSlot),
		{ HijackInfo = disj_half_hijack(RedoipSlot) },
		{ Code = node([
			assign(RedoipSlot, lval(redoip(lval(curfr))))
				- "prepare for half disj hijack"
		]) }
	;
		% Here CurfrMaxfr must be may_be_different.
		code_info__acquire_temp_slot(lval(redoip(lval(maxfr))),
			RedoipSlot),
		code_info__acquire_temp_slot(lval(redofr(lval(maxfr))),
			RedofrSlot),
		{ HijackInfo = disj_full_hijack(RedoipSlot, RedofrSlot) },
		{ Code = node([
			assign(RedoipSlot, lval(redoip(lval(maxfr))))
				- "prepare for full disj hijack",
			assign(RedofrSlot, lval(redofr(lval(maxfr))))
				- "prepare for full disj hijack",
			assign(redofr(lval(maxfr)), lval(curfr))
				- "prepare for full disj hijack"
		]) }
	).

code_info__undo_disj_hijack(HijackInfo, Code) -->
	code_info__get_fail_info(FailInfo0),
	{ FailInfo0 = fail_info(ResumePoints, ResumeKnown, CurfrMaxfr,
		CondEnv, Allow) },
	(
		{ HijackInfo = disj_no_hijack },
		{ Code = empty }
	;
		{ HijackInfo = disj_temp_frame },
		{ Code = node([
			assign(maxfr, lval(prevfr(lval(maxfr))))
				- "restore maxfr for temp frame disj"
		]) }
	;
		{ HijackInfo = disj_quarter_hijack },
		{ require(unify(CurfrMaxfr, must_be_equal),
			"maxfr may differ from curfr in disj_quarter_hijack") },
		{ stack__top_det(ResumePoints, ResumePoint) },
		{ code_info__pick_stack_resume_point(ResumePoint,
			_, StackLabel) },
		{ LabelConst = const(code_addr_const(StackLabel)) },
		{ Code = node([
			assign(redoip(lval(curfr)), LabelConst)
				- "restore redoip for quarter disj hijack"
		]) }
	;
		{ HijackInfo = disj_half_hijack(RedoipSlot) },
		{ require(unify(ResumeKnown, resume_point_unknown),
			"resume point known in disj_half_hijack") },
		{ require(unify(CurfrMaxfr, must_be_equal),
			"maxfr may differ from curfr in disj_half_hijack") },
		{ Code = node([
			assign(redoip(lval(curfr)), lval(RedoipSlot))
				- "restore redoip for half disj hijack"
		]) }
	;
		{ HijackInfo = disj_full_hijack(RedoipSlot, RedofrSlot) },
		{ require(unify(CurfrMaxfr, may_be_different),
			"maxfr same as curfr in disj_full_hijack") },
		{ Code = node([
			assign(redoip(lval(maxfr)), lval(RedoipSlot))
				- "restore redoip for full disj hijack",
			assign(redofr(lval(maxfr)), lval(RedofrSlot))
				- "restore redofr for full disj hijack"
		]) }
	),
	(
			% HijackInfo \= disj_no_hijack if and only if
			% the disjunction is model_non.
		{ HijackInfo \= disj_no_hijack },
		{ CondEnv = inside_non_condition }
	->
		{ FailInfo = fail_info(ResumePoints, resume_point_unknown,
			CurfrMaxfr, CondEnv, Allow) },
		code_info__set_fail_info(FailInfo)
	;
		[]
	).

%---------------------------------------------------------------------------%

:- type ite_hijack_info
	--->	ite_info(
			resume_point_known,
			condition_env,
			ite_hijack_type
		).

:- type ite_hijack_type
	--->	ite_no_hijack
	;	ite_temp_frame(
			lval		% The stack slot in which we saved
					% the value of maxfr.
		)
	;	ite_quarter_hijack
	;	ite_half_hijack(
			lval		% The stack slot in which we saved
					% the value of the hijacked redoip.
		)
	;	ite_full_hijack(
			lval,		% The stack slot in which we saved
					% the value of the hijacked redoip.
			lval,		% The stack slot in which we saved
					% the value of the hijacked redofr.
			lval		% The stack slot in which we saved
					% the value of maxfr.
		).

code_info__prepare_for_ite_hijack(EffCodeModel, HijackInfo, Code) -->
	code_info__get_fail_info(FailInfo),
	{ FailInfo = fail_info(_, ResumeKnown, CurfrMaxfr, CondEnv, Allow) },
	(
		{ EffCodeModel \= model_non }
	->
		{ HijackType = ite_no_hijack },
		{ Code = node([
			comment("ite no hijack")
				- ""
		]) }
	;
		{ Allow = not_allowed ; CondEnv = inside_non_condition }
	->
		code_info__acquire_temp_slot(lval(maxfr), MaxfrSlot),
		{ HijackType = ite_temp_frame(MaxfrSlot) },
		code_info__create_temp_frame(do_fail, "prepare for ite",
			TempFrameCode),
		{ MaxfrCode = node([
			assign(MaxfrSlot, lval(maxfr))
				- "prepare for ite"
		]) },
		{ Code = tree(TempFrameCode, MaxfrCode) }
	;
		{ CurfrMaxfr = must_be_equal },
		{ ResumeKnown = resume_point_known(_) }
	->
		{ HijackType = ite_quarter_hijack },
		{ Code = node([
			comment("ite quarter hijack")
				- ""
		]) }
	;
		{ CurfrMaxfr = must_be_equal }
	->
		% Here ResumeKnown must be resume_point_unknown.
		code_info__acquire_temp_slot(lval(redoip(lval(curfr))),
			RedoipSlot),
		{ HijackType = ite_half_hijack(RedoipSlot) },
		{ Code = node([
			assign(RedoipSlot, lval(redoip(lval(curfr))))
				- "prepare for half ite hijack"
		]) }
	;
		% Here CurfrMaxfr must be may_be_different.
		code_info__acquire_temp_slot(lval(redoip(lval(maxfr))),
			RedoipSlot),
		code_info__acquire_temp_slot(lval(redofr(lval(maxfr))),
			RedofrSlot),
		code_info__acquire_temp_slot(lval(maxfr),
			MaxfrSlot),
		{ HijackType = ite_full_hijack(RedoipSlot, RedofrSlot,
			MaxfrSlot) },
		{ Code = node([
			assign(MaxfrSlot, lval(maxfr))
				- "prepare for full ite hijack",
			assign(RedoipSlot, lval(redoip(lval(maxfr))))
				- "prepare for full ite hijack",
			assign(RedofrSlot, lval(redofr(lval(maxfr))))
				- "prepare for full ite hijack",
			assign(redofr(lval(maxfr)), lval(curfr))
				- "prepare for full ite hijack"
		]) }
	),
	{ HijackInfo = ite_info(ResumeKnown, CondEnv, HijackType) },
	( { EffCodeModel = model_non } ->
		code_info__inside_non_condition
	;
		[]
	).

code_info__ite_enter_then(HijackInfo, ThenCode, ElseCode) -->
	code_info__get_fail_info(FailInfo0),
	{ FailInfo0 = fail_info(ResumePoints0, ResumeKnown0, CurfrMaxfr,
		_, Allow) },
	{ stack__pop_det(ResumePoints0, _, ResumePoints) },
	{ HijackInfo = ite_info(HijackResumeKnown, OldCondEnv, HijackType) },
	{
		HijackType = ite_no_hijack,
		ThenCode = empty,
		ElseCode = ThenCode
	;
		HijackType = ite_temp_frame(MaxfrSlot),
		ThenCode = node([
			% We can't remove the frame, it may not be on top.
			assign(redoip(lval(MaxfrSlot)),
				const(code_addr_const(do_fail)))
				- "soft cut for temp frame ite"
		]),
		ElseCode = node([
			assign(maxfr, lval(prevfr(lval(MaxfrSlot))))
				- "restore maxfr for temp frame ite"
		])
	;
		HijackType = ite_quarter_hijack,
		stack__top_det(ResumePoints, ResumePoint),
		(
			code_info__maybe_pick_stack_resume_point(ResumePoint,
				_, StackLabel)
		->
			LabelConst = const(code_addr_const(StackLabel)),
			ThenCode = node([
				assign(redoip(lval(curfr)), LabelConst) -
					"restore redoip for quarter ite hijack"
			])
		;
			% This can happen only if ResumePoint is unreachable
			% from here.
			ThenCode = empty
		),
		ElseCode = ThenCode
	;
		HijackType = ite_half_hijack(RedoipSlot),
		ThenCode = node([
			assign(redoip(lval(curfr)), lval(RedoipSlot))
				- "restore redoip for half ite hijack"
		]),
		ElseCode = ThenCode
	;
		HijackType = ite_full_hijack(RedoipSlot, RedofrSlot,
			MaxfrSlot),
		ThenCode = node([
			assign(redoip(lval(MaxfrSlot)), lval(RedoipSlot))
				- "restore redoip for full ite hijack",
			assign(redofr(lval(MaxfrSlot)), lval(RedofrSlot))
				- "restore redofr for full ite hijack"
		]),
		ElseCode = node([
			assign(redoip(lval(maxfr)), lval(RedoipSlot))
				- "restore redoip for full ite hijack",
			assign(redofr(lval(maxfr)), lval(RedofrSlot))
				- "restore redofr for full ite hijack"
		])
	},
	{ ResumeKnown0 = resume_point_unknown ->
		ResumeKnown = resume_point_unknown
	;
		ResumeKnown = HijackResumeKnown
	},
	{ FailInfo = fail_info(ResumePoints, ResumeKnown, CurfrMaxfr,
		OldCondEnv, Allow) },
	code_info__set_fail_info(FailInfo).

%---------------------------------------------------------------------------%

:- type simple_neg_info		==	fail_info.

code_info__enter_simple_neg(ResumeVars, GoalInfo, FailInfo0) -->
	code_info__get_fail_info(FailInfo0),
		% The only reason why we push a resume point at all
		% is to protect the variables in ResumeVars from becoming
		% unknown; by including them in the domain of the resume point,
		% we guarantee that they will become zombies instead of
		% unknown if they die in the pre- or post-goal updates.
		% Therefore the only part of ResumePoint that matters
		% is the set of variables in the resume map; the other
		% parts of ResumePoint (the locations, the code address)
		% will not be referenced.
	{ set__to_sorted_list(ResumeVars, ResumeVarList) },
	{ map__init(ResumeMap0) },
	{ code_info__make_fake_resume_map(ResumeVarList,
		ResumeMap0, ResumeMap) },
	{ ResumePoint = orig_only(ResumeMap, do_redo) },
	code_info__effect_resume_point(ResumePoint, model_semi, Code),
	{ require(unify(Code, empty), "nonempty code for simple neg") },
	code_info__pre_goal_update(GoalInfo, yes).

code_info__leave_simple_neg(GoalInfo, FailInfo) -->
	code_info__post_goal_update(GoalInfo),
	code_info__set_fail_info(FailInfo).

:- pred code_info__make_fake_resume_map(list(prog_var)::in,
	map(prog_var, set(lval))::in, map(prog_var, set(lval))::out) is det.

code_info__make_fake_resume_map([], ResumeMap, ResumeMap).
code_info__make_fake_resume_map([Var | Vars], ResumeMap0, ResumeMap) :-
		% a visibly fake location
	set__singleton_set(Locns, reg(r, -1)),
	map__det_insert(ResumeMap0, Var, Locns, ResumeMap1),
	code_info__make_fake_resume_map(Vars, ResumeMap1, ResumeMap).

%---------------------------------------------------------------------------%

:- type det_commit_info
	--->	det_commit_info(
			maybe(lval),		% Location of saved maxfr.
			maybe(pair(lval))	% Location of saved ticket
						% counter and trail pointer.
		).

code_info__prepare_for_det_commit(DetCommitInfo, Code) -->
	code_info__get_fail_info(FailInfo0),
	{ FailInfo0 = fail_info(_, _, CurfrMaxfr, _, _) },
	(
		{ CurfrMaxfr = may_be_different },
		code_info__acquire_temp_slot(lval(maxfr), MaxfrSlot),
		{ SaveMaxfrCode = node([
			assign(MaxfrSlot, lval(maxfr))
				- "save the value of maxfr"
		]) },
		{ MaybeMaxfrSlot = yes(MaxfrSlot) }
	;
		{ CurfrMaxfr = must_be_equal },
		{ SaveMaxfrCode = empty },
		{ MaybeMaxfrSlot = no }
	),
	code_info__maybe_save_trail_info(MaybeTrailSlots, SaveTrailCode),
	{ DetCommitInfo = det_commit_info(MaybeMaxfrSlot, MaybeTrailSlots) },
	{ Code = tree(SaveMaxfrCode, SaveTrailCode) }.

code_info__generate_det_commit(DetCommitInfo, Code) -->
	{ DetCommitInfo = det_commit_info(MaybeMaxfrSlot, MaybeTrailSlots) },
	(
		{ MaybeMaxfrSlot = yes(MaxfrSlot) },
		{ RestoreMaxfrCode = node([
			assign(maxfr, lval(MaxfrSlot))
				- "restore the value of maxfr - perform commit"
		]) },
		code_info__release_temp_slot(MaxfrSlot)
	;
		{ MaybeMaxfrSlot = no },
		{ RestoreMaxfrCode = node([
			assign(maxfr, lval(curfr))
				- "restore the value of maxfr - perform commit"
		]) }
	),
	code_info__maybe_restore_trail_info(MaybeTrailSlots,
		CommitTrailCode, _),
	{ Code = tree(RestoreMaxfrCode, CommitTrailCode) }.

%---------------------------------------------------------------------------%

:- type semi_commit_info
	--->	semi_commit_info(
			fail_info,		% Fail_info on entry.
			resume_point_info,
			commit_hijack_info,
			maybe(pair(lval))	% Location of saved ticket
						% counter and trail pointer.
		).

:- type commit_hijack_info
	--->	commit_temp_frame(
			lval,		% The stack slot in which we saved
					% the old value of maxfr.
			bool		% Do we bracket the goal with
					% MR_commit_mark and MR_commit_cut?
		)
	;	commit_quarter_hijack
	;	commit_half_hijack(
			lval		% The stack slot in which we saved
					% the value of the hijacked redoip.
		)
	;	commit_full_hijack(
			lval,		% The stack slot in which we saved
					% the value of the hijacked redoip.
			lval,		% The stack slot in which we saved
					% the value of the hijacked redofr.
			lval		% The stack slot in which we saved
					% the value of maxfr.
		).

code_info__prepare_for_semi_commit(SemiCommitInfo, Code) -->
	code_info__get_fail_info(FailInfo0),
	{ FailInfo0 = fail_info(ResumePoints0, ResumeKnown, CurfrMaxfr,
		CondEnv, Allow) },
	{ stack__top_det(ResumePoints0, TopResumePoint) },
	code_info__clone_resume_point(TopResumePoint, NewResumePoint),
	{ stack__push(ResumePoints0, NewResumePoint, ResumePoints) },
	{ FailInfo = fail_info(ResumePoints, resume_point_known(has_been_done),
		CurfrMaxfr, CondEnv, Allow) },
	code_info__set_fail_info(FailInfo),

	{ code_info__pick_stack_resume_point(NewResumePoint, _, StackLabel) },
	{ StackLabelConst = const(code_addr_const(StackLabel)) },
	(
		{ Allow = not_allowed ; CondEnv = inside_non_condition }
	->
		code_info__acquire_temp_slot(lval(maxfr), MaxfrSlot),
		{ MaxfrCode = node([
			assign(MaxfrSlot, lval(maxfr))
				- "prepare for temp frame commit"
		]) },
		code_info__create_temp_frame(StackLabel,
			"prepare for temp frame commit", TempFrameCode),
		code_info__get_globals(Globals),
		{ globals__lookup_bool_option(Globals, use_minimal_model,
			UseMinimalModel) },
		{ HijackInfo = commit_temp_frame(MaxfrSlot, UseMinimalModel) },
		{
			UseMinimalModel = yes,
			% If the code we are committing across starts but
			% does not complete the evaluation of a tabled subgoal,
			% the cut will remove the generator's choice point,
			% so that the evaluation of the subgoal will never
			% be completed. We handle such "dangling" generators
			% by removing them from the subgoal trie of the
			% tabled procedure. This requires knowing what
			% tabled subgoals are started inside commits,
			% which is why we wrap the goal being committed across
			% inside MR_commit_{mark,cut}.
			Components = [
				pragma_c_raw_code(
					"\tMR_save_transient_registers();\n",
					live_lvals_info(set__init)),
				pragma_c_raw_code(
					"\tMR_commit_mark();\n",
					live_lvals_info(set__init)),
				pragma_c_raw_code(
					"\tMR_restore_transient_registers();\n",
					live_lvals_info(set__init))
			],
			MarkCode = node([
				pragma_c([], Components, will_not_call_mercury,
					no, no, no, no, no) - ""
			])
		;
			UseMinimalModel = no,
			MarkCode = empty
		},
		{ HijackCode = tree(MaxfrCode, tree(TempFrameCode, MarkCode)) }
	;
		{ ResumeKnown = resume_point_known(has_been_done) },
		{ CurfrMaxfr = must_be_equal }
	->
		{ HijackInfo = commit_quarter_hijack },
		{ HijackCode = node([
			assign(redoip(lval(curfr)), StackLabelConst)
				- "hijack the redofr slot"
		]) }
	;
		{ CurfrMaxfr = must_be_equal }
	->
		% Here ResumeKnown must be resume_point_unknown or
		% resume_point_known(wont_be_done).

		code_info__acquire_temp_slot(lval(redoip(lval(curfr))),
			RedoipSlot),
		{ HijackInfo = commit_half_hijack(RedoipSlot) },
		{ HijackCode = node([
			assign(RedoipSlot, lval(redoip(lval(curfr))))
				- "prepare for half commit hijack",
			assign(redoip(lval(curfr)), StackLabelConst)
				- "hijack the redofr slot"
		]) }
	;
		% Here CurfrMaxfr must be may_be_different.
		code_info__acquire_temp_slot(lval(redoip(lval(maxfr))),
			RedoipSlot),
		code_info__acquire_temp_slot(lval(redofr(lval(maxfr))),
			RedofrSlot),
		code_info__acquire_temp_slot(lval(maxfr), MaxfrSlot),
		{ HijackInfo = commit_full_hijack(RedoipSlot, RedofrSlot,
			MaxfrSlot) },
		{ HijackCode = node([
			assign(RedoipSlot, lval(redoip(lval(maxfr))))
				- "prepare for full commit hijack",
			assign(RedofrSlot, lval(redofr(lval(maxfr))))
				- "prepare for full commit hijack",
			assign(MaxfrSlot, lval(maxfr))
				- "prepare for full commit hijack",
			assign(redofr(lval(maxfr)), lval(curfr))
				- "hijack the redofr slot",
			assign(redoip(lval(maxfr)), StackLabelConst)
				- "hijack the redoip slot"
		]) }
	),
	code_info__maybe_save_trail_info(MaybeTrailSlots, SaveTrailCode),
	{ SemiCommitInfo = semi_commit_info(FailInfo0, NewResumePoint,
		HijackInfo, MaybeTrailSlots) },
	{ Code = tree(HijackCode, SaveTrailCode) }.

code_info__generate_semi_commit(SemiCommitInfo, Code) -->
	{ SemiCommitInfo = semi_commit_info(FailInfo, ResumePoint,
		HijackInfo, MaybeTrailSlots) },

	code_info__set_fail_info(FailInfo),
	% XXX should release the temp slots in each arm of the switch
	(
		{ HijackInfo = commit_temp_frame(MaxfrSlot, UseMinimalModel) },
		{ MaxfrCode = node([
			assign(maxfr, lval(MaxfrSlot))
				- "restore maxfr for temp frame hijack"
		]) },
		{
			UseMinimalModel = yes,
			% See the comment in prepare_for_semi_commit above.
			Components = [
				pragma_c_raw_code("\tMR_commit_cut();\n",
					live_lvals_info(set__init))
			],
			CutCode = node([
				pragma_c([], Components, will_not_call_mercury,
					no, no, no, no, no)
					- "commit for temp frame hijack"
			])
		;
			UseMinimalModel = no,
			CutCode = empty
		},
		{ SuccessUndoCode = tree(MaxfrCode, CutCode) },
		{ FailureUndoCode = tree(MaxfrCode, CutCode) }
	;
		{ HijackInfo = commit_quarter_hijack },
		{ FailInfo = fail_info(ResumePoints, _, _, _, _) },
		{ stack__top_det(ResumePoints, TopResumePoint) },
		{ code_info__pick_stack_resume_point(TopResumePoint,
			_, StackLabel) },
		{ StackLabelConst = const(code_addr_const(StackLabel)) },
		{ SuccessUndoCode = node([
			assign(maxfr, lval(curfr))
				- "restore maxfr for quarter commit hijack",
			assign(redoip(lval(maxfr)), StackLabelConst)
				- "restore redoip for quarter commit hijack"
		]) },
		{ FailureUndoCode = node([
			assign(redoip(lval(maxfr)), StackLabelConst)
				- "restore redoip for quarter commit hijack"
		]) }
	;
		{ HijackInfo = commit_half_hijack(RedoipSlot) },
		{ SuccessUndoCode = node([
			assign(maxfr, lval(curfr))
				- "restore maxfr for half commit hijack",
			assign(redoip(lval(maxfr)), lval(RedoipSlot))
				- "restore redoip for half commit hijack"
		]) },
		{ FailureUndoCode = node([
			assign(redoip(lval(maxfr)), lval(RedoipSlot))
				- "restore redoip for half commit hijack"
		]) }
	;
		{ HijackInfo = commit_full_hijack(RedoipSlot, RedofrSlot,
			MaxfrSlot) },
		{ SuccessUndoCode = node([
			assign(maxfr, lval(MaxfrSlot))
				- "restore maxfr for full commit hijack",
			assign(redoip(lval(maxfr)), lval(RedoipSlot))
				- "restore redoip for full commit hijack",
			assign(redofr(lval(maxfr)), lval(RedofrSlot))
				- "restore redofr for full commit hijack"
		]) },
		{ FailureUndoCode = node([
			assign(redoip(lval(maxfr)), lval(RedoipSlot))
				- "restore redoip for full commit hijack",
			assign(redofr(lval(maxfr)), lval(RedofrSlot))
				- "restore redofr for full commit hijack"
		]) }
	),

	code_info__remember_position(AfterCommit),
	code_info__generate_resume_point(ResumePoint, ResumePointCode),
	code_info__generate_failure(FailCode),
	code_info__reset_to_position(AfterCommit),

	code_info__maybe_restore_trail_info(MaybeTrailSlots,
		CommitTrailCode, RestoreTrailCode),

	code_info__get_next_label(SuccLabel),
	{ GotoSuccLabel = node([
		goto(label(SuccLabel)) - "Jump to success continuation"
	]) },
	{ SuccLabelCode = node([
		label(SuccLabel) - "Success continuation"
	]) },
	{ SuccessCode =
		tree(SuccessUndoCode,
		     CommitTrailCode)
	},
	{ FailureCode =
		tree(ResumePointCode,
		tree(FailureUndoCode,
		tree(RestoreTrailCode,
		     FailCode)))
	},
	{ Code =
		tree(SuccessCode,
		tree(GotoSuccLabel,
		tree(FailureCode,
		     SuccLabelCode)))
	}.

%---------------------------------------------------------------------------%

:- pred code_info__inside_non_condition(code_info::in, code_info::out) is det.

code_info__inside_non_condition -->
	code_info__get_fail_info(FailInfo0),
	{ FailInfo0 = fail_info(ResumePoints, ResumeKnown, CurfrMaxfr,
		_, Allow) },
	{ FailInfo = fail_info(ResumePoints, ResumeKnown, CurfrMaxfr,
		inside_non_condition, Allow) },
	code_info__set_fail_info(FailInfo).

:- pred code_info__create_temp_frame(code_addr::in, string::in, code_tree::out,
	code_info::in, code_info::out) is det.

code_info__create_temp_frame(Redoip, Comment, Code) -->
	code_info__get_proc_model(ProcModel),
	{ ProcModel = model_non ->
		Kind = nondet_stack_proc
	;
		Kind = det_stack_proc
	},
	{ Code = node([
		mkframe(temp_frame(Kind), Redoip)
			- Comment
	]) },
	code_info__set_created_temp_frame(yes),
	code_info__get_fail_info(FailInfo0),
	{ FailInfo0 = fail_info(ResumePoints, ResumeKnown, _, CondEnv, Allow) },
	{ FailInfo = fail_info(ResumePoints, ResumeKnown, may_be_different,
		CondEnv, Allow) },
	code_info__set_fail_info(FailInfo).

%---------------------------------------------------------------------------%

code_info__effect_resume_point(ResumePoint, CodeModel, Code) -->
	code_info__get_fail_info(FailInfo0),
	{ FailInfo0 = fail_info(ResumePoints0, _ResumeKnown, CurfrMaxfr,
		CondEnv, Allow) },

	{ stack__top(ResumePoints0, OldResumePoint) ->
		code_info__pick_first_resume_point(OldResumePoint, OldMap, _),
		code_info__pick_first_resume_point(ResumePoint, NewMap, _),
		map__keys(OldMap, OldKeys),
		map__keys(NewMap, NewKeys),
		set__list_to_set(OldKeys, OldKeySet),
		set__list_to_set(NewKeys, NewKeySet),
		require(set__subset(OldKeySet, NewKeySet),
			"non-nested resume point variable sets")
	;
		true
	},

	{ stack__push(ResumePoints0, ResumePoint, ResumePoints) },
	( { CodeModel = model_non } ->
		{ code_info__pick_stack_resume_point(ResumePoint,
			_, StackLabel) },
		{ LabelConst = const(code_addr_const(StackLabel)) },
		{ Code = node([
			assign(redoip(lval(maxfr)), LabelConst)
				- "hijack redoip to effect resume point"
		]) },
		{ RedoipUpdate = has_been_done }
	;
		{ Code = empty },
		{ RedoipUpdate = wont_be_done }
	),
	{ FailInfo = fail_info(ResumePoints, resume_point_known(RedoipUpdate),
		CurfrMaxfr, CondEnv, Allow) },
	code_info__set_fail_info(FailInfo).

code_info__pop_resume_point -->
	code_info__get_fail_info(FailInfo0),
	{ FailInfo0 = fail_info(ResumePoints0, ResumeKnown, CurfrMaxfr,
		CondEnv, Allow) },
	{ stack__pop_det(ResumePoints0, _, ResumePoints) },
	{ FailInfo = fail_info(ResumePoints, ResumeKnown, CurfrMaxfr,
		CondEnv, Allow) },
	code_info__set_fail_info(FailInfo).

%---------------------------------------------------------------------------%

code_info__top_resume_point(ResumePoint) -->
	code_info__get_fail_info(FailInfo),
	{ FailInfo = fail_info(ResumePoints, _, _, _, _) },
	{ stack__top_det(ResumePoints, ResumePoint) }.

code_info__set_resume_point_to_unknown -->
	code_info__get_fail_info(FailInfo0),
	{ FailInfo0 = fail_info(ResumePoints, _, CurfrMaxfr, CondEnv, Allow) },
	{ FailInfo = fail_info(ResumePoints, resume_point_unknown,
		CurfrMaxfr, CondEnv, Allow) },
	code_info__set_fail_info(FailInfo).

code_info__set_resume_point_and_frame_to_unknown -->
	code_info__get_fail_info(FailInfo0),
	{ FailInfo0 = fail_info(ResumePoints, _, _, CondEnv, Allow) },
	{ FailInfo = fail_info(ResumePoints, resume_point_unknown,
		may_be_different, CondEnv, Allow) },
	code_info__set_fail_info(FailInfo).

%---------------------------------------------------------------------------%

code_info__generate_failure(Code) -->
	code_info__get_fail_info(FailInfo),
	{ FailInfo = fail_info(ResumePoints, ResumeKnown, _, _, _) },
	(
		{ ResumeKnown = resume_point_known(_) },
		{ stack__top_det(ResumePoints, TopResumePoint) },
		(
			code_info__pick_matching_resume_addr(TopResumePoint,
				FailureAddress0)
		->
			{ FailureAddress = FailureAddress0 },
			{ PlaceCode = empty }
		;
			{ code_info__pick_first_resume_point(TopResumePoint,
				Map, FailureAddress) },
			{ map__to_assoc_list(Map, AssocList) },
			code_info__remember_position(CurPos),
			code_info__pick_and_place_vars(AssocList, _,
				PlaceCode),
			code_info__reset_to_position(CurPos)
		),
		{ BranchCode = node([goto(FailureAddress) - "fail"]) },
		{ Code = tree(PlaceCode, BranchCode) }
	;
		{ ResumeKnown = resume_point_unknown },
		{ Code = node([goto(do_redo) - "fail"]) }
	).

code_info__fail_if_rval_is_false(Rval0, Code) -->
	code_info__get_fail_info(FailInfo),
	{ FailInfo = fail_info(ResumePoints, ResumeKnown, _, _, _) },
	(
		{ ResumeKnown = resume_point_known(_) },
		{ stack__top_det(ResumePoints, TopResumePoint) },
		(
			code_info__pick_matching_resume_addr(TopResumePoint,
				FailureAddress0)
		->
				% We branch away if the test *fails*
			{ code_util__neg_rval(Rval0, Rval) },
			{ Code = node([
				if_val(Rval, FailureAddress0) -
					"Test for failure"
			]) }
		;
			{ code_info__pick_first_resume_point(TopResumePoint,
				Map, FailureAddress) },
			{ map__to_assoc_list(Map, AssocList) },
			code_info__get_next_label(SuccessLabel),
			code_info__remember_position(CurPos),
			code_info__pick_and_place_vars(AssocList, _,
				PlaceCode),
			code_info__reset_to_position(CurPos),
			{ SuccessAddress = label(SuccessLabel) },
				% We branch away if the test *fails*,
				% therefore if the test succeeds, we branch
				% around the code that moves variables to
				% their failure locations and branches away
				% to the failure continuation
			{ TestCode = node([
				if_val(Rval0, SuccessAddress) -
					"Test for failure"
			]) },
			{ TailCode = node([
				goto(FailureAddress) -
					"Goto failure",
				label(SuccessLabel) -
					"Success continuation"
			]) },
			{ Code = tree(TestCode, tree(PlaceCode, TailCode)) }
		)
	;
		{ ResumeKnown = resume_point_unknown },
			% We branch away if the test *fails*
		{ code_util__neg_rval(Rval0, Rval) },
		{ Code = node([
			if_val(Rval, do_redo) -
				"Test for failure"
		]) }
	).

%---------------------------------------------------------------------------%

code_info__failure_is_direct_branch(CodeAddr) -->
	code_info__get_fail_info(FailInfo),
	{ FailInfo = fail_info(ResumePoints, resume_point_known(_), _, _, _) },
	{ stack__top(ResumePoints, TopResumePoint) },
	code_info__pick_matching_resume_addr(TopResumePoint, CodeAddr).

code_info__may_use_nondet_tailcall(TailCallStatus) -->
	code_info__get_fail_info(FailInfo),
	{ FailInfo = fail_info(ResumePoints0, ResumeKnown, _, _, _) },
	{
		stack__pop(ResumePoints0, ResumePoint1, ResumePoints1),
		stack__is_empty(ResumePoints1),
		ResumePoint1 = stack_only(_, do_fail)
	->
		(
			ResumeKnown = resume_point_known(_),
			TailCallStatus = unchecked_tail_call
		;
			ResumeKnown = resume_point_unknown,
			TailCallStatus = checked_tail_call
		)
	;
		TailCallStatus = no_tail_call
	}.

%---------------------------------------------------------------------------%

	% See whether the current locations of variables match the locations
	% associated with any of the options in the given failure map.
	% If yes, return the code_addr of that option.

:- pred code_info__pick_matching_resume_addr(resume_point_info::in,
	code_addr::out, code_info::in, code_info::out) is semidet.

code_info__pick_matching_resume_addr(ResumeMaps, Addr) -->
	code_info__variable_locations(CurLocs),
	{
		ResumeMaps = orig_only(Map1, Addr1),
		( code_info__match_resume_loc(Map1, CurLocs) ->
			Addr = Addr1
		;
			fail
		)
	;
		ResumeMaps = stack_only(Map1, Addr1),
		( code_info__match_resume_loc(Map1, CurLocs) ->
			Addr = Addr1
		;
			fail
		)
	;
		ResumeMaps = orig_and_stack(Map1, Addr1, Map2, Addr2),
		( code_info__match_resume_loc(Map1, CurLocs) ->
			Addr = Addr1
		; code_info__match_resume_loc(Map2, CurLocs) ->
			Addr = Addr2
		;
			fail
		)
	;
		ResumeMaps = stack_and_orig(Map1, Addr1, Map2, Addr2),
		( code_info__match_resume_loc(Map1, CurLocs) ->
			Addr = Addr1
		; code_info__match_resume_loc(Map2, CurLocs) ->
			Addr = Addr2
		;
			fail
		)
	}.

:- pred code_info__match_resume_loc(resume_map::in, resume_map::in) is semidet.

code_info__match_resume_loc(Map, Locations0) :-
	map__keys(Map, KeyList),
	set__list_to_set(KeyList, Keys),
	map__select(Locations0, Keys, Locations),
	map__to_assoc_list(Locations, List),
	\+ (
		list__member(Var - Actual, List),
		\+ (
			map__search(Map, Var, Lvals),
			set__subset(Lvals, Actual)
		)
	).

:- pred code_info__pick_first_resume_point(resume_point_info::in,
	resume_map::out, code_addr::out) is det.

code_info__pick_first_resume_point(orig_only(Map, Addr), Map, Addr).
code_info__pick_first_resume_point(stack_only(Map, Addr), Map, Addr).
code_info__pick_first_resume_point(orig_and_stack(Map, Addr, _, _), Map, Addr).
code_info__pick_first_resume_point(stack_and_orig(Map, Addr, _, _), Map, Addr).

:- pred code_info__pick_stack_resume_point(resume_point_info::in,
	resume_map::out, code_addr::out) is det.

code_info__pick_stack_resume_point(ResumePoint, Map, Addr) :-
	( code_info__maybe_pick_stack_resume_point(ResumePoint, Map1, Addr1) ->
		Map = Map1,
		Addr = Addr1
	;
		error("no stack resume point")
	).

:- pred code_info__maybe_pick_stack_resume_point(resume_point_info::in,
	resume_map::out, code_addr::out) is semidet.

code_info__maybe_pick_stack_resume_point(stack_only(Map, Addr), Map, Addr).
code_info__maybe_pick_stack_resume_point(orig_and_stack(_, _, Map, Addr),
	Map, Addr).
code_info__maybe_pick_stack_resume_point(stack_and_orig(Map, Addr, _, _),
	Map, Addr).

%---------------------------------------------------------------------------%

code_info__produce_vars(Vars, Map, Code) -->
	{ set__to_sorted_list(Vars, VarList) },
	code_info__produce_vars_2(VarList, Map, Code).

:- pred code_info__produce_vars_2(list(prog_var)::in,
	map(prog_var, set(lval))::out,
	code_tree::out, code_info::in, code_info::out) is det.

code_info__produce_vars_2([], Map, empty) -->
	{ map__init(Map) }.
code_info__produce_vars_2([V | Vs], Map, Code) -->
	code_info__produce_vars_2(Vs, Map0, Code0),
	code_info__produce_variable_in_reg_or_stack(V, Code1, Lval),
	{ set__singleton_set(Lvals, Lval) },
	{ map__set(Map0, V, Lvals, Map) },
	{ Code = tree(Code0, Code1) }.

code_info__flush_resume_vars_to_stack(Code) -->
	code_info__compute_resume_var_stack_locs(VarLocs),
	code_info__place_vars(VarLocs, Code).

:- pred compute_resume_var_stack_locs(assoc_list(prog_var, lval)::out,
	code_info::in, code_info::out) is det.

code_info__compute_resume_var_stack_locs(VarLocs) -->
	code_info__get_fail_info(FailInfo),
	{ FailInfo = fail_info(ResumePointStack, _, _, _, _) },
	{ stack__top_det(ResumePointStack, ResumePoint) },
	{ code_info__pick_stack_resume_point(ResumePoint, StackMap, _) },
	{ map__to_assoc_list(StackMap, VarLocSets) },
	{ code_info__pick_var_places(VarLocSets, VarLocs) }.

%---------------------------------------------------------------------------%

:- pred code_info__init_fail_info(code_model::in, maybe(set(prog_var))::in,
	resume_point_info::out, code_info::in, code_info::out) is det.

code_info__init_fail_info(CodeModel, MaybeFailVars, ResumePoint) -->
	(
		{ CodeModel = model_det },
		code_info__get_next_label(ResumeLabel),
		{ ResumeAddress = label(ResumeLabel) },
		{ ResumeKnown = resume_point_unknown },
		{ CurfrMaxfr = may_be_different }
	;
		{ CodeModel = model_semi },
			% The resume point for this label
			% will be part of the procedure epilog.
		code_info__get_next_label(ResumeLabel),
		{ ResumeAddress = label(ResumeLabel) },
		{ ResumeKnown = resume_point_known(wont_be_done) },
		{ CurfrMaxfr = may_be_different }
	;
		{ CodeModel = model_non },
		( { MaybeFailVars = yes(_) } ->
			code_info__get_next_label(ResumeLabel),
			{ ResumeAddress = label(ResumeLabel) }
		;
			{ ResumeAddress = do_fail }
		),
		{ ResumeKnown = resume_point_known(has_been_done) },
		{ CurfrMaxfr = must_be_equal }
	),
	( { MaybeFailVars = yes(FailVars) } ->
		code_info__get_stack_slots(StackSlots),
		{ map__select(StackSlots, FailVars, StackMap0) },
		{ map__to_assoc_list(StackMap0, StackList0) },
		{ code_info__make_singleton_sets(StackList0, StackList) },
		{ map__from_assoc_list(StackList, StackMap) }
	;
		{ map__init(StackMap) }
	),
	{ ResumePoint = stack_only(StackMap, ResumeAddress) },
	{ stack__init(ResumeStack0) },
	{ stack__push(ResumeStack0, ResumePoint, ResumeStack) },
	code_info__get_fail_info(FailInfo0),
	{ FailInfo0 = fail_info(_, _, _, _, Allow) },
	{ FailInfo = fail_info(ResumeStack, ResumeKnown, CurfrMaxfr,
		not_inside_non_condition, Allow) },
	code_info__set_fail_info(FailInfo).

%---------------------------------------------------------------------------%

code_info__make_resume_point(ResumeVars, ResumeLocs, FullMap, ResumePoint) -->
	code_info__get_stack_slots(StackSlots),
	{ map__select(FullMap, ResumeVars, OrigMap) },
	(
		{ ResumeLocs = orig_only },
		code_info__get_next_label(OrigLabel),
		{ OrigAddr = label(OrigLabel) },
		{ ResumePoint = orig_only(OrigMap, OrigAddr) }
	;
		{ ResumeLocs = stack_only },
		{ code_info__make_stack_resume_map(ResumeVars,
			StackSlots, StackMap) },
		code_info__get_next_label(StackLabel),
		{ StackAddr = label(StackLabel) },
		{ ResumePoint = stack_only(StackMap, StackAddr) }
	;
		{ ResumeLocs = orig_and_stack },
		{ code_info__make_stack_resume_map(ResumeVars,
			StackSlots, StackMap) },
		code_info__get_next_label(OrigLabel),
		{ OrigAddr = label(OrigLabel) },
		code_info__get_next_label(StackLabel),
		{ StackAddr = label(StackLabel) },
		{ ResumePoint = orig_and_stack(OrigMap, OrigAddr,
			StackMap, StackAddr) }
	;
		{ ResumeLocs = stack_and_orig },
		{ code_info__make_stack_resume_map(ResumeVars,
			StackSlots, StackMap) },
		code_info__get_next_label(StackLabel),
		{ StackAddr = label(StackLabel) },
		code_info__get_next_label(OrigLabel),
		{ OrigAddr = label(OrigLabel) },
		{ ResumePoint = stack_and_orig(StackMap, StackAddr,
			OrigMap, OrigAddr) }
	).

:- pred code_info__make_stack_resume_map(set(prog_var)::in, stack_slots::in,
	map(prog_var, set(lval))::out) is det.

code_info__make_stack_resume_map(ResumeVars, StackSlots, StackMap) :-
	map__select(StackSlots, ResumeVars, StackMap0),
	map__to_assoc_list(StackMap0, StackList0),
	code_info__make_singleton_sets(StackList0, StackList),
	map__from_assoc_list(StackList, StackMap).

:- pred code_info__make_singleton_sets(assoc_list(prog_var, lval)::in,
	assoc_list(prog_var, set(lval))::out) is det.

code_info__make_singleton_sets([], []).
code_info__make_singleton_sets([V - L | Rest0], [V - Ls | Rest]) :-
	set__singleton_set(Ls, L),
	code_info__make_singleton_sets(Rest0, Rest).

%---------------------------------------------------------------------------%

	% The code we generate for a resumption point looks like this:
	%
	% label(StackLabel)
	% <assume variables are where StackMap says they are>
	% <copy variables to their locations according to OrigMap>
	% label(OrigLabel)
	% <assume variables are where OrigMap says they are>
	%
	% Failures at different points may cause control to arrive at
	% the resumption point via either label, which is why the last
	% line is necessary.
	%
	% The idea is that failures from other procedures will go to
	% StackLabel, and that failures from this procedure while
	% everything is in its original place will go to OrigLabel.
	% Failures from this procedure where not everything is in its
	% original place can go to either, after moving the resume variables
	% to the places where the label expects them.
	%
	% The above layout (stack, then orig) is the most common. However,
	% liveness.m may decide that one or other of the two labels will
	% never be referred to (e.g. because there are no calls inside
	% the range of effect of the resumption point or because a call
	% follows immediately after the establishment of the resumption
	% point), or that it would be more efficient to put the two labels
	% in the other order (e.g. because the code after the resumption point
	% needs most of the variables in their stack slots).

code_info__generate_resume_point(ResumePoint, Code) -->
	(
		{ ResumePoint = orig_only(Map1, Addr1) },
		{ extract_label_from_code_addr(Addr1, Label1) },
		{ Code = node([
			label(Label1) -
				"orig only failure continuation"
		]) },
		code_info__set_var_locations(Map1)
	;
		{ ResumePoint = stack_only(Map1, Addr1) },
		{ extract_label_from_code_addr(Addr1, Label1) },
		{ Code = node([
			label(Label1) -
				"stack only failure continuation"
		]) },
		code_info__set_var_locations(Map1),
		code_info__generate_resume_layout(Label1, Map1)
	;
		{ ResumePoint = stack_and_orig(Map1, Addr1, Map2, Addr2) },
		{ extract_label_from_code_addr(Addr1, Label1) },
		{ extract_label_from_code_addr(Addr2, Label2) },
		{ Label1Code = node([
			label(Label1) -
				"stack failure continuation before orig"
		]) },
		code_info__set_var_locations(Map1),
		code_info__generate_resume_layout(Label1, Map1),
		{ map__to_assoc_list(Map2, AssocList2) },
		code_info__place_resume_vars(AssocList2, PlaceCode),
		{ Label2Code = node([
			label(Label2) -
				"orig failure continuation after stack"
		]) },
		code_info__set_var_locations(Map2),
		{ Code = tree(Label1Code, tree(PlaceCode, Label2Code)) }
	;
		{ ResumePoint = orig_and_stack(Map1, Addr1, Map2, Addr2) },
		{ extract_label_from_code_addr(Addr1, Label1) },
		{ extract_label_from_code_addr(Addr2, Label2) },
		{ Label1Code = node([
			label(Label1) -
				"orig failure continuation before stack"
		]) },
		code_info__set_var_locations(Map1),
		{ map__to_assoc_list(Map2, AssocList2) },
		code_info__place_resume_vars(AssocList2, PlaceCode),
		{ Label2Code = node([
			label(Label2) -
				"stack failure continuation after orig"
		]) },
		code_info__set_var_locations(Map2),
		code_info__generate_resume_layout(Label2, Map2),
		{ Code = tree(Label1Code, tree(PlaceCode, Label2Code)) }
	).

:- pred extract_label_from_code_addr(code_addr::in, label::out) is det.

extract_label_from_code_addr(CodeAddr, Label) :-
	( CodeAddr = label(Label0) ->
		Label = Label0
	;
		error("extract_label_from_code_addr: non-label!")
	).

:- pred code_info__place_resume_vars(assoc_list(prog_var, set(lval))::in,
	code_tree::out, code_info::in, code_info::out) is det.

code_info__place_resume_vars([], empty) --> [].
code_info__place_resume_vars([Var - TargetSet | Rest], Code) -->
	{ set__to_sorted_list(TargetSet, Targets) },
	code_info__place_resume_var(Var, Targets, FirstCode),
	{ Code = tree(FirstCode, RestCode) },
	code_info__place_resume_vars(Rest, RestCode).

:- pred code_info__place_resume_var(prog_var::in, list(lval)::in,
	code_tree::out, code_info::in, code_info::out) is det.

code_info__place_resume_var(_Var, [], empty) --> [].
code_info__place_resume_var(Var, [Target | Targets], Code) -->
	code_info__place_var(Var, Target, FirstCode),
	code_info__place_resume_var(Var, Targets, RestCode),
	{ Code = tree(FirstCode, RestCode) }.

	% Reset the code generator's database of what is where.
	% Remember that the variables in the map are available in their
	% associated rvals; forget about all other variables.

:- pred code_info__set_var_locations(resume_map::in,
	code_info::in, code_info::out) is det.

code_info__set_var_locations(Map) -->
	{ map__to_assoc_list(Map, LvalList0) },
	{ code_info__flatten_varlval_list(LvalList0, LvalList) },
	code_info__get_var_locns_info(VarInfo0),
	{
		VarInfo0 = exprn_info(Exprn0),
		RvalList = assoc_list__map_values(exprn_aux__var_lval_to_rval,
			LvalList),
		code_exprn__reinit_state(RvalList, Exprn0, Exprn),
		VarInfo = exprn_info(Exprn)
	;
		VarInfo0 = var_locn_info(VarLocnInfo0),
		var_locn__reinit_state(LvalList, VarLocnInfo0, VarLocnInfo),
		VarInfo = var_locn_info(VarLocnInfo)
	},
	code_info__set_var_locns_info(VarInfo).

:- pred code_info__flatten_varlval_list(assoc_list(prog_var, set(lval))::in,
	assoc_list(prog_var, lval)::out) is det.

code_info__flatten_varlval_list([], []).
code_info__flatten_varlval_list([V - Rvals | Rest0], All) :-
	code_info__flatten_varlval_list(Rest0, Rest),
	set__to_sorted_list(Rvals, RvalList),
	code_info__flatten_varlval_list_2(RvalList, V, Rest1),
	list__append(Rest1, Rest, All).

:- pred code_info__flatten_varlval_list_2(list(lval)::in, prog_var::in,
	assoc_list(prog_var, lval)::out) is det.

code_info__flatten_varlval_list_2([], _V, []).
code_info__flatten_varlval_list_2([R | Rs], V, [V - R | Rest]) :-
	code_info__flatten_varlval_list_2(Rs, V, Rest).

code_info__resume_point_vars(ResumePoint, Vars) :-
	code_info__pick_first_resume_point(ResumePoint, ResumeMap, _),
	map__keys(ResumeMap, Vars).

code_info__resume_point_stack_addr(ResumePoint, StackAddr) :-
	code_info__pick_stack_resume_point(ResumePoint, _, StackAddr).

%---------------------------------------------------------------------------%

:- pred code_info__maybe_save_trail_info(maybe(pair(lval))::out,
	code_tree::out, code_info::in, code_info::out) is det.

code_info__maybe_save_trail_info(MaybeTrailSlots, SaveTrailCode) -->
	code_info__get_globals(Globals),
	{ globals__lookup_bool_option(Globals, use_trail, UseTrail) },
	( { UseTrail = yes } ->
		code_info__acquire_temp_slot(ticket_counter, CounterSlot),
		code_info__acquire_temp_slot(ticket, TrailPtrSlot),
		{ MaybeTrailSlots = yes(CounterSlot - TrailPtrSlot) },
		{ SaveTrailCode = node([
			mark_ticket_stack(CounterSlot)
				- "save the ticket counter",
			store_ticket(TrailPtrSlot)
				- "save the trail pointer"
		]) }
	;
		{ MaybeTrailSlots = no },
		{ SaveTrailCode = empty }
	).

:- pred code_info__maybe_restore_trail_info(maybe(pair(lval))::in,
	code_tree::out, code_tree::out, code_info::in, code_info::out) is det.

code_info__maybe_restore_trail_info(MaybeTrailSlots,
		CommitCode, RestoreCode) -->
	(
		{ MaybeTrailSlots = no },
		{ CommitCode = empty },
		{ RestoreCode = empty }
	;
		{ MaybeTrailSlots = yes(CounterSlot - TrailPtrSlot) },
		{ CommitCode = node([
			reset_ticket(lval(TrailPtrSlot), commit)
				- "discard trail entries and restore trail ptr",
			prune_tickets_to(lval(CounterSlot))
				- "restore ticket counter (but not high water mark)"
		]) },
		{ RestoreCode = node([
			reset_ticket(lval(TrailPtrSlot), undo)
				- "apply trail entries and restore trail ptr",
			discard_ticket
				- "restore ticket counter and high water mark"
		]) },
		code_info__release_temp_slot(CounterSlot),
		code_info__release_temp_slot(TrailPtrSlot)
	).

%---------------------------------------------------------------------------%

:- pred code_info__clone_resume_point(resume_point_info::in,
	resume_point_info::out, code_info::in, code_info::out) is det.

code_info__clone_resume_point(ResumePoint0, ResumePoint) -->
	(
		{ ResumePoint0 = orig_only(_, _) },
		{ error("cloning orig_only resume point") }
	;
		{ ResumePoint0 = stack_only(Map1, _) },
		code_info__get_next_label(Label1),
		{ Addr1 = label(Label1) },
		{ ResumePoint = stack_only(Map1, Addr1) }
	;
		{ ResumePoint0 = stack_and_orig(Map1, _, Map2, _) },
		code_info__get_next_label(Label1),
		{ Addr1 = label(Label1) },
		code_info__get_next_label(Label2),
		{ Addr2 = label(Label2) },
		{ ResumePoint = stack_and_orig(Map1, Addr1, Map2, Addr2) }
	;
		{ ResumePoint0 = orig_and_stack(Map1, _, Map2, _) },
		code_info__get_next_label(Label2),
		{ Addr2 = label(Label2) },
		code_info__get_next_label(Label1),
		{ Addr1 = label(Label1) },
		{ ResumePoint = stack_and_orig(Map2, Addr2, Map1, Addr1) }
	).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

	% Submodule to deal with liveness issues.

	% The principles underlying this submodule of code_info.m are
	% documented in the file compiler/notes/allocation.html.

:- interface.

:- pred code_info__get_known_variables(list(prog_var), code_info, code_info).
:- mode code_info__get_known_variables(out, in, out) is det.

:- pred code_info__variable_is_forward_live(prog_var, code_info, code_info).
:- mode code_info__variable_is_forward_live(in, in, out) is semidet.

:- pred code_info__make_vars_forward_dead(set(prog_var), code_info, code_info).
:- mode code_info__make_vars_forward_dead(in, in, out) is det.

:- pred code_info__pickup_zombies(set(prog_var), code_info, code_info).
:- mode code_info__pickup_zombies(out, in, out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- pred code_info__add_forward_live_vars(set(prog_var), code_info, code_info).
:- mode code_info__add_forward_live_vars(in, in, out) is det.

:- pred code_info__rem_forward_live_vars(set(prog_var), code_info, code_info).
:- mode code_info__rem_forward_live_vars(in, in, out) is det.

	% Make these variables appear magically live.
	% We don't care where they are put.

:- pred code_info__make_vars_forward_live(set(prog_var), code_info, code_info).
:- mode code_info__make_vars_forward_live(in, in, out) is det.

code_info__get_known_variables(VarList) -->
	code_info__get_forward_live_vars(ForwardLiveVars),
	code_info__current_resume_point_vars(ResumeVars),
	{ set__union(ForwardLiveVars, ResumeVars, Vars) },
	{ set__to_sorted_list(Vars, VarList) }.

code_info__variable_is_forward_live(Var) -->
	code_info__get_forward_live_vars(Liveness),
	{ set__member(Var, Liveness) }.

code_info__add_forward_live_vars(Births) -->
	code_info__get_forward_live_vars(Liveness0),
	{ set__union(Liveness0, Births, Liveness) },
	code_info__set_forward_live_vars(Liveness).

code_info__rem_forward_live_vars(Deaths) -->
	code_info__get_forward_live_vars(Liveness0),
	{ set__difference(Liveness0, Deaths, Liveness) },
	code_info__set_forward_live_vars(Liveness).

code_info__make_vars_forward_live(Vars) -->
	code_info__get_stack_slots(StackSlots),
	code_info__get_var_locns_info(VarInfo0),
	{ set__to_sorted_list(Vars, VarList) },
	{ code_info__make_vars_forward_live_2(VarList, StackSlots, 1,
		VarInfo0, VarInfo) },
	code_info__set_var_locns_info(VarInfo).

:- pred code_info__make_vars_forward_live_2(list(prog_var), stack_slots, int,
	var_locns_info, var_locns_info).
:- mode code_info__make_vars_forward_live_2(in, in, in, in, out) is det.

code_info__make_vars_forward_live_2([], _, _, VarInfo, VarInfo).
code_info__make_vars_forward_live_2([Var | Vars], StackSlots, N0,
		VarInfo0, VarInfo) :-
	( map__search(StackSlots, Var, Lval0) ->
		Lval = Lval0,
		N1 = N0
	;
		code_info__find_unused_reg(N0, VarInfo0, N1),
		Lval = reg(r, N1)
	),
	(
		VarInfo0 = exprn_info(Exprn0),
		code_exprn__maybe_set_var_location(Var, Lval, Exprn0, Exprn1),
		VarInfo1 = exprn_info(Exprn1)
	;
		VarInfo0 = var_locn_info(VarLocn0),
		var_locn__set_magic_var_location(Var, Lval,
			VarLocn0, VarLocn1),
		VarInfo1 = var_locn_info(VarLocn1)
	),
	code_info__make_vars_forward_live_2(Vars, StackSlots, N1,
		VarInfo1, VarInfo).

:- pred code_info__find_unused_reg(int, var_locns_info, int).
:- mode code_info__find_unused_reg(in, in, out) is det.

code_info__find_unused_reg(N0, VarInfo0, N) :-
	(
		VarInfo0 = exprn_info(Exprn0),
		code_info__find_unused_reg_lazy(N0, Exprn0, N)
	;
		VarInfo0 = var_locn_info(VarLocn0),
		code_info__find_unused_reg_eager(N0, VarLocn0, N)
	).

:- pred code_info__find_unused_reg_eager(int, var_locn_info, int).
:- mode code_info__find_unused_reg_eager(in, in, out) is det.

code_info__find_unused_reg_eager(N0, Exprn0, N) :-
	( var_locn__lval_in_use(reg(r, N0), Exprn0, _) ->
		N1 is N0 + 1,
		code_info__find_unused_reg_eager(N1, Exprn0, N)
	;
		N = N0
	).

:- pred code_info__find_unused_reg_lazy(int, exprn_info, int).
:- mode code_info__find_unused_reg_lazy(in, in, out) is det.

code_info__find_unused_reg_lazy(N0, Exprn0, N) :-
	( code_exprn__lval_in_use(reg(r, N0), Exprn0, _) ->
		N1 is N0 + 1,
		code_info__find_unused_reg_lazy(N1, Exprn0, N)
	;
		N = N0
	).

code_info__make_vars_forward_dead(Vars) -->
	code_info__maybe_make_vars_forward_dead(Vars, yes).

:- pred code_info__maybe_make_vars_forward_dead(set(prog_var), bool,
	code_info, code_info).
:- mode code_info__maybe_make_vars_forward_dead(in, in, in, out) is det.

code_info__maybe_make_vars_forward_dead(Vars0, FirstTime) -->
	code_info__current_resume_point_vars(ResumeVars),
	{ set__intersect(Vars0, ResumeVars, FlushVars) },
	code_info__get_zombies(Zombies0),
	{ set__union(Zombies0, FlushVars, Zombies) },
	code_info__set_zombies(Zombies),
	{ set__difference(Vars0, Zombies, Vars) },
	{ set__to_sorted_list(Vars, VarList) },
	code_info__maybe_make_vars_forward_dead_2(VarList, FirstTime).

:- pred code_info__maybe_make_vars_forward_dead_2(list(prog_var), bool,
	code_info, code_info).
:- mode code_info__maybe_make_vars_forward_dead_2(in, in, in, out) is det.

code_info__maybe_make_vars_forward_dead_2([], _) --> [].
code_info__maybe_make_vars_forward_dead_2([V | Vs], FirstTime) -->
	code_info__get_var_locns_info(VarInfo0),
	{
		VarInfo0 = exprn_info(Exprn0),
		code_exprn__var_becomes_dead(V, Exprn0, Exprn),
		VarInfo = exprn_info(Exprn)
	;
		VarInfo0 = var_locn_info(VarLocn0),
		var_locn__var_becomes_dead(V, FirstTime, VarLocn0, VarLocn),
		VarInfo = var_locn_info(VarLocn)
	},
	code_info__set_var_locns_info(VarInfo),
	code_info__maybe_make_vars_forward_dead_2(Vs, FirstTime).

code_info__pickup_zombies(Zombies) -->
	code_info__get_zombies(Zombies),
	{ set__init(Empty) },
	code_info__set_zombies(Empty).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

	% Submodule for handling the saving and restoration
	% of trail tickets, heap pointers, stack pointers etc.

:- interface.

:- pred code_info__save_hp(code_tree, lval, code_info, code_info).
:- mode code_info__save_hp(out, out, in, out) is det.

:- pred code_info__restore_hp(lval, code_tree, code_info, code_info).
:- mode code_info__restore_hp(in, out, in, out) is det.

:- pred code_info__release_hp(lval, code_info, code_info).
:- mode code_info__release_hp(in, in, out) is det.

:- pred code_info__restore_and_release_hp(lval, code_tree,
	code_info, code_info).
:- mode code_info__restore_and_release_hp(in, out, in, out) is det.

:- pred code_info__maybe_save_hp(bool, code_tree, maybe(lval),
	code_info, code_info).
:- mode code_info__maybe_save_hp(in, out, out, in, out) is det.

:- pred code_info__maybe_restore_hp(maybe(lval), code_tree,
	code_info, code_info).
:- mode code_info__maybe_restore_hp(in, out, in, out) is det.

:- pred code_info__maybe_release_hp(maybe(lval), code_info, code_info).
:- mode code_info__maybe_release_hp(in, in, out) is det.

:- pred code_info__maybe_restore_and_release_hp(maybe(lval), code_tree,
	code_info, code_info).
:- mode code_info__maybe_restore_and_release_hp(in, out, in, out) is det.

:- pred code_info__save_ticket(code_tree, lval, code_info, code_info).
:- mode code_info__save_ticket(out, out, in, out) is det.

:- pred code_info__reset_ticket(lval, reset_trail_reason, code_tree,
	code_info, code_info).
:- mode code_info__reset_ticket(in, in, out, in, out) is det.

:- pred code_info__release_ticket(lval, code_info, code_info).
:- mode code_info__release_ticket(in, in, out) is det.

:- pred code_info__reset_and_prune_ticket(lval, reset_trail_reason,
	code_tree, code_info, code_info).
:- mode code_info__reset_and_prune_ticket(in, in, out, in, out) is det.

:- pred code_info__reset_prune_and_release_ticket(lval, reset_trail_reason,
	code_tree, code_info, code_info).
:- mode code_info__reset_prune_and_release_ticket(in, in, out, in, out)
	is det.

:- pred code_info__reset_and_discard_ticket(lval, reset_trail_reason,
	code_tree, code_info, code_info).
:- mode code_info__reset_and_discard_ticket(in, in, out, in, out) is det.

:- pred code_info__reset_discard_and_release_ticket(lval, reset_trail_reason,
	code_tree, code_info, code_info).
:- mode code_info__reset_discard_and_release_ticket(in, in, out, in, out)
	is det.

:- pred code_info__maybe_save_ticket(bool, code_tree, maybe(lval),
	code_info, code_info).
:- mode code_info__maybe_save_ticket(in, out, out, in, out) is det.

:- pred code_info__maybe_reset_ticket(maybe(lval), reset_trail_reason,
	code_tree, code_info, code_info).
:- mode code_info__maybe_reset_ticket(in, in, out, in, out) is det.

:- pred code_info__maybe_release_ticket(maybe(lval), code_info, code_info).
:- mode code_info__maybe_release_ticket(in, in, out) is det.

:- pred code_info__maybe_reset_and_prune_ticket(maybe(lval),
	reset_trail_reason, code_tree, code_info, code_info).
:- mode code_info__maybe_reset_and_prune_ticket(in, in, out, in, out) is det.

:- pred code_info__maybe_reset_prune_and_release_ticket(maybe(lval),
	reset_trail_reason, code_tree, code_info, code_info).
:- mode code_info__maybe_reset_prune_and_release_ticket(in, in, out, in, out)
	is det.

:- pred code_info__maybe_reset_and_discard_ticket(maybe(lval),
	reset_trail_reason, code_tree, code_info, code_info).
:- mode code_info__maybe_reset_and_discard_ticket(in, in, out, in, out) is det.

:- pred code_info__maybe_reset_discard_and_release_ticket(maybe(lval),
	reset_trail_reason, code_tree, code_info, code_info).
:- mode code_info__maybe_reset_discard_and_release_ticket(in, in, out, in, out)
	is det.

%---------------------------------------------------------------------------%

:- implementation.

code_info__save_hp(Code, HpSlot) -->
	code_info__acquire_temp_slot(lval(hp), HpSlot),
	{ Code = node([
		mark_hp(HpSlot) - "Save heap pointer"
	]) }.

code_info__restore_hp(HpSlot, Code) -->
	{ Code = node([
		restore_hp(lval(HpSlot)) - "Restore heap pointer"
	]) }.

code_info__release_hp(HpSlot) -->
	code_info__release_temp_slot(HpSlot).

code_info__restore_and_release_hp(HpSlot, Code) -->
	{ Code = node([
		restore_hp(lval(HpSlot)) - "Release heap pointer"
	]) },
	code_info__release_hp(HpSlot).

%---------------------------------------------------------------------------%

code_info__maybe_save_hp(Maybe, Code, MaybeHpSlot) -->
	( { Maybe = yes } ->
		code_info__save_hp(Code, HpSlot),
		{ MaybeHpSlot = yes(HpSlot) }
	;
		{ Code = empty },
		{ MaybeHpSlot = no }
	).

code_info__maybe_restore_hp(MaybeHpSlot, Code) -->
	( { MaybeHpSlot = yes(HpSlot) } ->
		code_info__restore_hp(HpSlot, Code)
	;
		{ Code = empty }
	).

code_info__maybe_release_hp(MaybeHpSlot) -->
	( { MaybeHpSlot = yes(HpSlot) } ->
		code_info__release_hp(HpSlot)
	;
		[]
	).

code_info__maybe_restore_and_release_hp(MaybeHpSlot, Code) -->
	( { MaybeHpSlot = yes(HpSlot) } ->
		code_info__restore_and_release_hp(HpSlot, Code)
	;
		{ Code = empty }
	).

%---------------------------------------------------------------------------%

code_info__save_ticket(Code, TicketSlot) -->
	code_info__acquire_temp_slot(ticket, TicketSlot),
	{ Code = node([
		store_ticket(TicketSlot) - "Save trail state"
	]) }.

code_info__reset_ticket(TicketSlot, Reason, Code) -->
	{ Code = node([
		reset_ticket(lval(TicketSlot), Reason) - "Reset trail"
	]) }.

code_info__release_ticket(TicketSlot) -->
	code_info__release_temp_slot(TicketSlot).

code_info__reset_and_prune_ticket(TicketSlot, Reason, Code) -->
	{ Code = node([
		reset_ticket(lval(TicketSlot), Reason) - "Restore trail",
		prune_ticket - "Prune ticket stack"
	]) }.

code_info__reset_prune_and_release_ticket(TicketSlot, Reason, Code) -->
	{ Code = node([
		reset_ticket(lval(TicketSlot), Reason) - "Release trail",
		prune_ticket - "Prune ticket stack"
	]) },
	code_info__release_temp_slot(TicketSlot).

code_info__reset_and_discard_ticket(TicketSlot, Reason, Code) -->
	{ Code = node([
		reset_ticket(lval(TicketSlot), Reason) - "Restore trail",
		discard_ticket - "Pop ticket stack"
	]) }.

code_info__reset_discard_and_release_ticket(TicketSlot, Reason, Code) -->
	{ Code = node([
		reset_ticket(lval(TicketSlot), Reason) - "Release trail",
		discard_ticket - "Pop ticket stack"
	]) },
	code_info__release_temp_slot(TicketSlot).

%---------------------------------------------------------------------------%

code_info__maybe_save_ticket(Maybe, Code, MaybeTicketSlot) -->
	( { Maybe = yes } ->
		code_info__save_ticket(Code, TicketSlot),
		{ MaybeTicketSlot = yes(TicketSlot) }
	;
		{ Code = empty },
		{ MaybeTicketSlot = no }
	).

code_info__maybe_reset_ticket(MaybeTicketSlot, Reason, Code) -->
	( { MaybeTicketSlot = yes(TicketSlot) } ->
		code_info__reset_ticket(TicketSlot, Reason, Code)
	;
		{ Code = empty }
	).

code_info__maybe_release_ticket(MaybeTicketSlot) -->
	( { MaybeTicketSlot = yes(TicketSlot) } ->
		code_info__release_ticket(TicketSlot)
	;
		[]
	).

code_info__maybe_reset_and_prune_ticket(MaybeTicketSlot, Reason, Code) -->
	( { MaybeTicketSlot = yes(TicketSlot) } ->
		code_info__reset_and_prune_ticket(TicketSlot, Reason, Code)
	;
		{ Code = empty }
	).

code_info__maybe_reset_prune_and_release_ticket(MaybeTicketSlot, Reason,
		Code) -->
	( { MaybeTicketSlot = yes(TicketSlot) } ->
		code_info__reset_prune_and_release_ticket(TicketSlot, Reason,
			Code)
	;
		{ Code = empty }
	).

code_info__maybe_reset_and_discard_ticket(MaybeTicketSlot, Reason, Code) -->
	( { MaybeTicketSlot = yes(TicketSlot) } ->
		code_info__reset_and_discard_ticket(TicketSlot, Reason, Code)
	;
		{ Code = empty }
	).

code_info__maybe_reset_discard_and_release_ticket(MaybeTicketSlot, Reason,
		Code) -->
	( { MaybeTicketSlot = yes(TicketSlot) } ->
		code_info__reset_discard_and_release_ticket(TicketSlot, Reason,
			Code)
	;
		{ Code = empty }
	).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

	% Submodule to deal with code_exprn or var_locn.

:- interface.

:- pred code_info__variable_locations(map(prog_var, set(lval))::out,
	code_info::in, code_info::out) is det.

:- pred code_info__set_var_location(prog_var::in, lval::in,
	code_info::in, code_info::out) is det.

:- pred code_info__assign_var_to_var(prog_var::in, prog_var::in,
	code_info::in, code_info::out) is det.

:- pred code_info__assign_lval_to_var(prog_var::in, lval::in, code_tree::out,
	code_info::in, code_info::out) is det.

:- pred code_info__assign_const_to_var(prog_var::in, rval::in,
	code_info::in, code_info::out) is det.

:- pred code_info__assign_expr_to_var(prog_var::in, rval::in, code_tree::out,
	code_info::in, code_info::out) is det.

:- pred code_info__assign_cell_to_var(prog_var::in, tag::in,
	list(maybe(rval))::in, string::in, code_tree::out,
	code_info::in, code_info::out) is det.

:- pred code_info__place_var(prog_var::in, lval::in, code_tree::out,
	code_info::in, code_info::out) is det.

:- pred code_info__produce_variable(prog_var::in, code_tree::out, rval::out,
	code_info::in, code_info::out) is det.

:- pred code_info__produce_variable_in_reg(prog_var::in, code_tree::out,
	lval::out, code_info::in, code_info::out) is det.

:- pred code_info__produce_variable_in_reg_or_stack(prog_var::in,
	code_tree::out, lval::out, code_info::in, code_info::out) is det.

:- pred code_info__materialize_vars_in_rval(rval::in, rval::out,
	code_tree::out, code_info::in, code_info::out) is det.

:- pred code_info__acquire_reg_for_var(prog_var::in, lval::out,
	code_info::in, code_info::out) is det.

:- pred code_info__acquire_reg(reg_type::in, lval::out,
	code_info::in, code_info::out) is det.

:- pred code_info__release_reg(lval::in, code_info::in, code_info::out) is det.

:- pred code_info__reserve_r1(code_tree::out, code_info::in, code_info::out)
	is det.

:- pred code_info__clear_r1(code_tree::out, code_info::in, code_info::out)
	is det.

:- type call_direction ---> caller ; callee.

	% Move variables to where they need to be at the time of the call:
	%
	% - The variables that need to be saved across the call (either because
	%   they are forward live after the call or because they are protected
	%   by an enclosing resumption point) will be saved on the stack.
	%   Note that if the call cannot succeed and the trace level is none,
	%   then no variables need to be saved across the call. (If the call
	%   cannot succeed but the trace level is not none, then we still
	%   save the usual variables on the stack to make them available
	%   for up-level printing in the debugger.)
	%
	% - The input arguments will be moved to their registers.

:- pred code_info__setup_call(hlds_goal_info::in,
	assoc_list(prog_var, arg_info)::in, set(lval)::out, code_tree::out,
	code_info::in, code_info::out) is det.

	% Move the output arguments of the current procedure to where
	% they need to be at return.

:- pred code_info__setup_return(assoc_list(prog_var, arg_info)::in,
	set(lval)::out, code_tree::out, code_info::in, code_info::out) is det.

:- pred code_info__eager_lock_regs(int::in, assoc_list(prog_var, lval)::in,
	code_info::in, code_info::out) is det.

:- pred code_info__eager_unlock_regs(code_info::in, code_info::out) is det.

	% Record the fact that all the registers have been clobbered (as by a
	% call). If the bool argument is true, then the call cannot return, and
	% thus it is OK for this action to delete the last record of the state
	% of a variable.
:- pred code_info__clear_all_registers(bool::in,
	code_info::in, code_info::out) is det.

:- pred code_info__clobber_regs(list(lval)::in,
	code_info::in, code_info::out) is det.

:- pred code_info__save_variables(set(prog_var)::in,
	set(lval)::out, code_tree::out,
	code_info::in, code_info::out) is det.

:- pred code_info__save_variables_on_stack(list(prog_var)::in, code_tree::out,
	code_info::in, code_info::out) is det.

:- pred code_info__max_reg_in_use(int::out, code_info::in, code_info::out)
	is det.

%---------------------------------------------------------------------------%

:- implementation.

:- pred code_info__pick_and_place_vars(assoc_list(prog_var, set(lval))::in,
	set(lval)::out, code_tree::out, code_info::in, code_info::out) is det.

:- pred code_info__place_vars(assoc_list(prog_var, lval)::in,
	code_tree::out, code_info::in, code_info::out) is det.

code_info__variable_locations(Lvals) -->
	code_info__get_var_locns_info(VarInfo),
	{
		VarInfo = exprn_info(Exprn),
		code_exprn__get_varlocs(Exprn, Rvals),
		Lvals = map__map_values(code_info__rval_map_to_lval_map, Rvals)
	;
		VarInfo = var_locn_info(VarLocn),
		var_locn__get_var_locations(VarLocn, Lvals)
	}.

:- func code_info__rval_map_to_lval_map(prog_var, set(rval)) = set(lval).

code_info__rval_map_to_lval_map(_Var, Rvals) =
	set__filter_map(code_info__rval_is_lval, Rvals).

:- func code_info__rval_is_lval(rval) = lval is semidet.

code_info__rval_is_lval(lval(Lval)) = Lval.

code_info__set_var_location(Var, Lval) -->
	code_info__get_var_locns_info(VarInfo0),
	{
		VarInfo0 = exprn_info(Exprn0),
		code_exprn__set_var_location(Var, Lval, Exprn0, Exprn),
		VarInfo = exprn_info(Exprn)
	;
		VarInfo0 = var_locn_info(VarLocn0),
		var_locn__check_and_set_magic_var_location(Var, Lval,
			VarLocn0, VarLocn),
		VarInfo = var_locn_info(VarLocn)
	},
	code_info__set_var_locns_info(VarInfo).

code_info__assign_var_to_var(Var, AssignedVar) -->
	code_info__get_var_locns_info(VarInfo0),
	{
		VarInfo0 = exprn_info(Exprn0),
		code_exprn__cache_exprn(Var, var(AssignedVar), Exprn0, Exprn),
		VarInfo = exprn_info(Exprn)
	;
		VarInfo0 = var_locn_info(VarLocInfo0),
		var_locn__assign_var_to_var(Var, AssignedVar,
			VarLocInfo0, VarLocInfo),
		VarInfo = var_locn_info(VarLocInfo)
	},
	code_info__set_var_locns_info(VarInfo).

code_info__assign_lval_to_var(Var, Lval, Code) -->
	code_info__get_var_locns_info(VarInfo0),
	{
		VarInfo0 = exprn_info(Exprn0),
		code_exprn__cache_exprn(Var, lval(Lval), Exprn0, Exprn),
		Code = empty,
		VarInfo = exprn_info(Exprn)
	;
		VarInfo0 = var_locn_info(VarLocInfo0),
		var_locn__assign_lval_to_var(Var, Lval, Code,
			VarLocInfo0, VarLocInfo),
		VarInfo = var_locn_info(VarLocInfo)
	},
	code_info__set_var_locns_info(VarInfo).

code_info__assign_const_to_var(Var, ConstRval) -->
	code_info__get_var_locns_info(VarInfo0),
	{
		VarInfo0 = exprn_info(Exprn0),
		code_exprn__cache_exprn(Var, ConstRval, Exprn0, Exprn),
		VarInfo = exprn_info(Exprn)
	;
		VarInfo0 = var_locn_info(VarLocInfo0),
		var_locn__assign_const_to_var(Var, ConstRval,
			VarLocInfo0, VarLocInfo),
		VarInfo = var_locn_info(VarLocInfo)
	},
	code_info__set_var_locns_info(VarInfo).

code_info__assign_expr_to_var(Var, Rval, Code) -->
	code_info__get_var_locns_info(VarInfo0),
	{
		code_util__lvals_in_rval(Rval, Lvals),
		Lvals = []
	->
		(
			VarInfo0 = exprn_info(Exprn0),
			code_exprn__cache_exprn(Var, Rval, Exprn0, Exprn),
			VarInfo = exprn_info(Exprn),
			Code = empty
		;
			VarInfo0 = var_locn_info(VarLocInfo0),
			var_locn__assign_expr_to_var(Var, Rval, Code,
				VarLocInfo0, VarLocInfo),
			VarInfo = var_locn_info(VarLocInfo)
		)
	;
		error("code_info__assign_expr_to_var: non-var lvals")
	},
	code_info__set_var_locns_info(VarInfo).

code_info__assign_cell_to_var(Var, Ptag, Vector, TypeMsg, Code) -->
	code_info__get_var_locns_info(VarInfo0),
	code_info__get_next_cell_number(CellNum),
	{
		VarInfo0 = exprn_info(Exprn0),
			% XXX Later we will need to worry about
			% whether the cell must be unique or not.
		Reuse = no,
		Rval = create(Ptag, Vector, uniform(no), can_be_either,
			CellNum, TypeMsg, Reuse),
		code_exprn__cache_exprn(Var, Rval, Exprn0, Exprn),
		VarInfo = exprn_info(Exprn),
		Code = empty
	;
		VarInfo0 = var_locn_info(VarLocInfo0),
		var_locn__assign_cell_to_var(Var, Ptag, Vector, CellNum,
			TypeMsg, Code, VarLocInfo0, VarLocInfo),
		VarInfo = var_locn_info(VarLocInfo)
	},
	code_info__set_var_locns_info(VarInfo).

code_info__place_var(Var, Lval, Code) -->
	code_info__get_var_locns_info(VarInfo0),
	{
		VarInfo0 = exprn_info(Exprn0),
		code_exprn__place_var(Var, Lval, Code, Exprn0, Exprn),
		VarInfo = exprn_info(Exprn)
	;
		VarInfo0 = var_locn_info(VarLocn0),
		var_locn__place_var(Var, Lval, Code, VarLocn0, VarLocn),
		VarInfo = var_locn_info(VarLocn)
	},
	code_info__set_var_locns_info(VarInfo).

code_info__pick_and_place_vars(VarLocSets, LiveLocs, Code) -->
	{ code_info__pick_var_places(VarLocSets, VarLocs) },
	{ assoc_list__values(VarLocs, Locs) },
	{ set__list_to_set(Locs, LiveLocs) },
	code_info__place_vars(VarLocs, Code).

:- pred code_info__pick_var_places(assoc_list(prog_var, set(lval))::in,
	assoc_list(prog_var, lval)::out) is det.

code_info__pick_var_places([], []).
code_info__pick_var_places([Var - LvalSet | VarLvalSets], VarLvals) :-
	code_info__pick_var_places(VarLvalSets, VarLvals0),
	(
		set__to_sorted_list(LvalSet, LvalList),
		LvalList = [Lval | _]
	->
		VarLvals = [Var - Lval | VarLvals0]
	;
		VarLvals = VarLvals0
	).

code_info__place_vars(VarLocs, Code) -->
	code_info__get_var_locns_info(VarInfo0),
	{
		VarInfo0 = exprn_info(Exprn0),
		code_exprn__place_vars(VarLocs, Code, Exprn0, Exprn),
		VarInfo = exprn_info(Exprn)
	;
		VarInfo0 = var_locn_info(VarLocn0),
		var_locn__place_vars(VarLocs, Code, VarLocn0, VarLocn),
		VarInfo = var_locn_info(VarLocn)
	},
	code_info__set_var_locns_info(VarInfo).

code_info__produce_variable(Var, Code, Rval) -->
	code_info__get_var_locns_info(VarInfo0),
	{
		VarInfo0 = exprn_info(Exprn0),
		code_exprn__produce_var(Var, Rval, Code, Exprn0, Exprn),
		VarInfo = exprn_info(Exprn)
	;
		VarInfo0 = var_locn_info(VarLocn0),
		var_locn__produce_var(Var, Rval, Code, VarLocn0, VarLocn),
		VarInfo = var_locn_info(VarLocn)
	},
	code_info__set_var_locns_info(VarInfo).

code_info__produce_variable_in_reg(Var, Code, Lval) -->
	code_info__get_var_locns_info(VarInfo0),
	{
		VarInfo0 = exprn_info(Exprn0),
		code_exprn__produce_var_in_reg(Var, Lval, Code, Exprn0, Exprn),
		VarInfo = exprn_info(Exprn)
	;
		VarInfo0 = var_locn_info(VarLocn0),
		var_locn__produce_var_in_reg(Var, Lval, Code,
			VarLocn0, VarLocn),
		VarInfo = var_locn_info(VarLocn)
	},
	code_info__set_var_locns_info(VarInfo).

code_info__produce_variable_in_reg_or_stack(Var, Code, Lval) -->
	code_info__get_var_locns_info(VarInfo0),
	{
		VarInfo0 = exprn_info(Exprn0),
		code_exprn__produce_var_in_reg_or_stack(Var, Lval, Code,
			Exprn0, Exprn),
		VarInfo = exprn_info(Exprn)
	;
		VarInfo0 = var_locn_info(VarLocn0),
		var_locn__produce_var_in_reg_or_stack(Var, Lval, Code,
			VarLocn0, VarLocn),
		VarInfo = var_locn_info(VarLocn)
	},
	code_info__set_var_locns_info(VarInfo).

code_info__materialize_vars_in_rval(Rval0, Rval, Code) -->
	code_info__get_var_locns_info(VarInfo0),
	{
		VarInfo0 = exprn_info(Exprn0),
		code_exprn__materialize_vars_in_rval(Rval0, Rval, Code,
			Exprn0, Exprn),
		VarInfo = exprn_info(Exprn)
	;
		VarInfo0 = var_locn_info(VarLocnInfo0),
		( Rval0 = lval(Lval0) ->
			var_locn__materialize_vars_in_lval(Lval0, Lval, Code,
				VarLocnInfo0, VarLocnInfo),
			Rval = lval(Lval),
			VarInfo = var_locn_info(VarLocnInfo)
		; exprn_aux__vars_in_rval(Rval0, []) ->
			Rval = Rval0,
			Code = empty,
			VarInfo = VarInfo0
		;
			error("eager code_info__materialize_vars_in_rval")
		)
	},
	code_info__set_var_locns_info(VarInfo).

code_info__acquire_reg_for_var(Var, Lval) -->
	code_info__get_follow_var_map(FollowVarsMap),
	code_info__get_next_non_reserved(NextNonReserved),
	code_info__get_var_locns_info(VarInfo0),
	{
		map__search(FollowVarsMap, Var, PrefLval),
		PrefLval = reg(PrefRegType, PrefRegNum),
		PrefRegNum >= 1
	->
		(
			VarInfo0 = exprn_info(Exprn0),
			code_exprn__acquire_reg_prefer_given(PrefRegType,
				PrefRegNum, Lval, Exprn0, Exprn),
			VarInfo = exprn_info(Exprn)
		;
			VarInfo0 = var_locn_info(VarLocn0),
			require(unify(PrefRegType, r), "acquire non-r reg"),
			var_locn__acquire_reg_prefer_given(PrefRegNum, Lval,
				VarLocn0, VarLocn),
			VarInfo = var_locn_info(VarLocn)
		)
	;
		% XXX We should only get a register if the map__search
		% succeeded; otherwise we should put the var in its stack slot.
		(
			VarInfo0 = exprn_info(Exprn0),
			code_exprn__acquire_reg(r, Lval, Exprn0, Exprn),
			VarInfo = exprn_info(Exprn)
		;
			VarInfo0 = var_locn_info(VarLocn0),
			var_locn__acquire_reg_start_at_given(
				NextNonReserved, Lval, VarLocn0, VarLocn),
			VarInfo = var_locn_info(VarLocn)
		)
	},
	code_info__set_var_locns_info(VarInfo).

code_info__acquire_reg(Type, Lval) -->
	code_info__get_var_locns_info(VarInfo0),
	{
		VarInfo0 = exprn_info(Exprn0),
		code_exprn__acquire_reg(Type, Lval, Exprn0, Exprn),
		VarInfo = exprn_info(Exprn)
	;
		VarInfo0 = var_locn_info(VarLocnInfo0),
		require(unify(Type, r),
			"code_info__acquire_reg: unknown reg type"),
		var_locn__acquire_reg(Lval, VarLocnInfo0, VarLocnInfo),
		VarInfo = var_locn_info(VarLocnInfo)
	},
	code_info__set_var_locns_info(VarInfo).

code_info__release_reg(Lval) -->
	code_info__get_var_locns_info(VarInfo0),
	{
		VarInfo0 = exprn_info(Exprn0),
		code_exprn__release_reg(Lval, Exprn0, Exprn),
		VarInfo = exprn_info(Exprn)
	;
		VarInfo0 = var_locn_info(VarLocnInfo0),
		var_locn__release_reg(Lval, VarLocnInfo0, VarLocnInfo),
		VarInfo = var_locn_info(VarLocnInfo)
	},
	code_info__set_var_locns_info(VarInfo).

code_info__reserve_r1(Code) -->
	code_info__get_var_locns_info(VarInfo0),
	{
		VarInfo0 = exprn_info(_Exprn0),
		VarInfo = VarInfo0,
		Code = empty
	;
		VarInfo0 = var_locn_info(VarLocnInfo0),
		var_locn__clear_r1(Code, VarLocnInfo0, VarLocnInfo1),
		var_locn__acquire_reg_require_given(reg(r, 1), VarLocnInfo1,
			VarLocnInfo),
		VarInfo = var_locn_info(VarLocnInfo)
	},
	code_info__set_var_locns_info(VarInfo).

code_info__clear_r1(Code) -->
	code_info__get_var_locns_info(VarInfo0),
	{
		VarInfo0 = exprn_info(Exprn0),
		code_exprn__clear_r1(Code, Exprn0, Exprn),
		VarInfo = exprn_info(Exprn)
	;
		VarInfo0 = var_locn_info(VarLocnInfo0),
		var_locn__release_reg(reg(r, 1), VarLocnInfo0, VarLocnInfo),
		VarInfo = var_locn_info(VarLocnInfo),
		Code = empty
	},
	code_info__set_var_locns_info(VarInfo).

%---------------------------------------------------------------------------%

code_info__setup_return(VarArgInfos, OutLocs, Code) -->
	code_info__setup_call_args(VarArgInfos, callee, OutLocs, Code).

code_info__setup_call(GoalInfo, ArgInfos, LiveLocs, Code) -->
	{ partition_args(ArgInfos, InArgInfos, OutArgInfos, _UnusedArgInfos) },
	{ assoc_list__keys(OutArgInfos, OutVars) },
	{ set__list_to_set(OutVars, OutVarSet) },
	{ goal_info_get_determinism(GoalInfo, Detism) },
	code_info__get_opt_no_return_calls(OptNoReturnCalls),
	(
		{ Detism = erroneous },
		{ OptNoReturnCalls = yes }
	->
		{ StackVarLocs = [] }
	;
		code_info__compute_forward_live_var_saves(OutVarSet,
			ForwardVarLocs),
		{ goal_info_get_code_model(GoalInfo, CodeModel) },
		( { CodeModel = model_non } ->
				% Save variables protected by the nearest
				% resumption point on the stack. XXX This
				% should be unnecessary; with the current
				% setup, the code that established the resume
				% point should have saved those variables
				% on the stack already. However, later we
				% should arrange things so that this saving
				% of the resume vars on the stack is delayed
				% until the first call after the setup of
				% the resume point.
			code_info__compute_resume_var_stack_locs(ResumeVarLocs),
			{ list__append(ResumeVarLocs, ForwardVarLocs,
				StackVarLocs) }
		;
			{ StackVarLocs = ForwardVarLocs }
		)
	),

	code_info__get_var_locns_info(VarInfo0),
	(
		{ VarInfo0 = exprn_info(_) },

		code_info__place_vars(StackVarLocs, StackCode),
		{ assoc_list__values(StackVarLocs, StackLocs) },
		{ set__list_to_set(StackLocs, StackLiveLocs) },

		code_info__setup_call_args_lazy(InArgInfos, caller,
			ArgLiveLocs, ArgCode),
		{ set__union(StackLiveLocs, ArgLiveLocs, LiveLocs) },
		{ Code = tree(StackCode, ArgCode) }
	;
		{ VarInfo0 = var_locn_info(VarLocnInfo0) },
		{ code_info__var_arg_info_to_lval(InArgInfos, InArgLocs) },
		{ list__append(StackVarLocs, InArgLocs, AllLocs) },
		{ var_locn__place_vars(AllLocs, Code,
			VarLocnInfo0, VarLocnInfo) },
		code_info__set_var_locns_info(var_locn_info(VarLocnInfo)),
		{ assoc_list__values(AllLocs, LiveLocList) },
		{ set__list_to_set(LiveLocList, LiveLocs) }

		% { assoc_list__keys(InArgLocs, InArgVars) },
		% { set__init(DeadVars0) },
		% code_info__which_variables_are_forward_live(InArgVars,
		% 	DeadVars0, DeadVars),
		% code_info__make_vars_forward_dead(DeadVars)
	).

:- pred code_info__setup_call_args(assoc_list(prog_var, arg_info)::in,
	call_direction::in, set(lval)::out, code_tree::out,
	code_info::in, code_info::out) is det.

code_info__setup_call_args(VarArgInfos, Direction, LiveLocs, Code) -->
	code_info__get_var_locns_info(VarInfo0),
	(
		{ VarInfo0 = exprn_info(_) },
		code_info__setup_call_args_lazy(VarArgInfos, Direction,
			LiveLocs, Code)
	;
		{ VarInfo0 = var_locn_info(_) },
		code_info__setup_call_args_eager(VarArgInfos, Direction,
			LiveLocs, Code)
	).

:- pred code_info__setup_call_args_eager(assoc_list(prog_var, arg_info)::in,
	call_direction::in, set(lval)::out, code_tree::out,
	code_info::in, code_info::out) is det.

code_info__setup_call_args_eager(AllArgsInfos, Direction, LiveLocs, Code) -->
	{ list__filter(code_info__call_arg_in_selected_dir(Direction),
		AllArgsInfos, ArgsInfos) },
	{ code_info__var_arg_info_to_lval(ArgsInfos, ArgsLocns) },
	code_info__get_eager_var_locns_info(VarLocnInfo0),
	{ var_locn__place_vars(ArgsLocns, Code, VarLocnInfo0, VarLocnInfo1) },
	code_info__set_var_locns_info(var_locn_info(VarLocnInfo1)),
	{ assoc_list__values(ArgsLocns, LiveLocList) },
	{ set__list_to_set(LiveLocList, LiveLocs) },
	{ assoc_list__keys(ArgsLocns, ArgVars) },
	{ set__init(DeadVars0) },
	code_info__which_variables_are_forward_live(ArgVars,
		DeadVars0, DeadVars),
	code_info__make_vars_forward_dead(DeadVars).

:- pred code_info__var_arg_info_to_lval(assoc_list(prog_var, arg_info)::in,
	assoc_list(prog_var, lval)::out) is det.

code_info__var_arg_info_to_lval([], []).
code_info__var_arg_info_to_lval([Var - ArgInfo | RestInfos],
		[Var - Lval | RestLvals]) :-
	ArgInfo = arg_info(Loc, _Mode),
	code_util__arg_loc_to_register(Loc, Lval),
	code_info__var_arg_info_to_lval(RestInfos, RestLvals).

:- pred code_info__which_variables_are_forward_live(list(prog_var)::in,
	set(prog_var)::in, set(prog_var)::out,
	code_info::in, code_info::out) is det.

code_info__which_variables_are_forward_live([], DeadVars, DeadVars) --> [].
code_info__which_variables_are_forward_live([Var | Vars], DeadVars0, DeadVars)
		-->
	( code_info__variable_is_forward_live(Var) ->
		{ DeadVars1 = DeadVars0 }
	;
		{ set__insert(DeadVars0, Var, DeadVars1) }
	),
	code_info__which_variables_are_forward_live(Vars, DeadVars1, DeadVars).

:- pred code_info__setup_call_args_lazy(assoc_list(prog_var, arg_info)::in,
	call_direction::in, set(lval)::out, code_tree::out,
	code_info::in, code_info::out) is det.

code_info__setup_call_args_lazy([], _Direction, set__init, empty) --> [].
code_info__setup_call_args_lazy([First | Rest], Direction, LiveLocs, Code) -->
	( { code_info__call_arg_in_selected_dir(Direction, First) } ->
		{ First = V - arg_info(Loc, _Mode) },
		{ code_util__arg_loc_to_register(Loc, Reg) },
		code_info__get_lazy_var_locns_info(Exprn0),
		{ code_exprn__place_var(V, Reg, Code0, Exprn0, Exprn1) },
			% We need to test that either the variable
			% is live OR it occurs in the remaining arguments
			% because of a bug in polymorphism.m which
			% causes some compiler generated code to violate
			% superhomogeneous form
		(
			code_info__variable_is_forward_live(V)
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
			code_info__set_var_locns_info(exprn_info(Exprn2)),
			code_info__setup_call_args_lazy(Rest, Direction,
				LiveLocs1, Code1),
			code_info__get_lazy_var_locns_info(Exprn3),
			{ code_exprn__unlock_reg(Reg, Exprn3, Exprn) },
			code_info__set_var_locns_info(exprn_info(Exprn)),
			{ Code = tree(Code0, Code1) }
		;
			{ code_exprn__lock_reg(Reg, Exprn1, Exprn2) },
			code_info__set_var_locns_info(exprn_info(Exprn2)),
			{ set__singleton_set(Vset, V) },
			code_info__make_vars_forward_dead(Vset),
			code_info__setup_call_args_lazy(Rest, Direction,
				LiveLocs1, Code1),
			code_info__get_lazy_var_locns_info(Exprn4),
			{ code_exprn__unlock_reg(Reg, Exprn4, Exprn) },
			code_info__set_var_locns_info(exprn_info(Exprn)),
			{ Code = tree(Code0, Code1) }
		),
		{ set__insert(LiveLocs1, Reg, LiveLocs) }
	;
		code_info__setup_call_args_lazy(Rest, Direction,
			LiveLocs, Code)
	).

:- pred code_info__call_arg_in_selected_dir(call_direction::in,
	pair(prog_var, arg_info)::in) is semidet.

code_info__call_arg_in_selected_dir(Direction, _ - arg_info(_, Mode)) :-
	(
		Mode = top_in,
		Direction = caller
	;
		Mode = top_out,
		Direction = callee
	).

code_info__eager_lock_regs(N, Exceptions) -->
	code_info__get_var_locns_info(VarInfo0),
	{
		VarInfo0 = exprn_info(_),
		VarInfo = VarInfo0
	;
		VarInfo0 = var_locn_info(VarLocnInfo0),
		var_locn__lock_regs(N, Exceptions, VarLocnInfo0, VarLocnInfo),
		VarInfo = var_locn_info(VarLocnInfo)
	},
	code_info__set_var_locns_info(VarInfo).

code_info__eager_unlock_regs -->
	code_info__get_var_locns_info(VarInfo0),
	{
		VarInfo0 = exprn_info(_),
		VarInfo = VarInfo0
	;
		VarInfo0 = var_locn_info(VarLocnInfo0),
		var_locn__unlock_regs(VarLocnInfo0, VarLocnInfo),
		VarInfo = var_locn_info(VarLocnInfo)
	},
	code_info__set_var_locns_info(VarInfo).

:- pred code_info__get_eager_var_locns_info(var_locn_info::out,
	code_info::in, code_info::out) is det.

code_info__get_eager_var_locns_info(VarLocnInfo) -->
	code_info__get_var_locns_info(VarInfo),
	{
		VarInfo = exprn_info(_),
		error("lazy code_info__get_eager_var_locns_info")
	;
		VarInfo = var_locn_info(VarLocnInfo)
	}.

:- pred code_info__get_lazy_var_locns_info(exprn_info::out,
	code_info::in, code_info::out) is det.

code_info__get_lazy_var_locns_info(Exprn) -->
	code_info__get_var_locns_info(VarInfo),
	{
		VarInfo = exprn_info(Exprn)
	;
		VarInfo = var_locn_info(_),
		error("eager code_info__get_lazy_var_locns_info")
	}.

	% As a sanity check, we could test whether any known variable
	% has its only value in a register, but we do so only with eager
	% code generation.
code_info__clear_all_registers(OkToDeleteAny) -->
	code_info__get_var_locns_info(VarInfo0),
	{
		VarInfo0 = exprn_info(Exprn0),
		code_exprn__clobber_regs([], Exprn0, Exprn),
		VarInfo = exprn_info(Exprn)
	;
		VarInfo0 = var_locn_info(VarLocn0),
		var_locn__clobber_all_regs(OkToDeleteAny, VarLocn0, VarLocn),
		VarInfo = var_locn_info(VarLocn)
	},
	code_info__set_var_locns_info(VarInfo).

code_info__clobber_regs(Regs) -->
	code_info__get_var_locns_info(VarInfo0),
	{
		VarInfo0 = exprn_info(_Exprn0),
		% The clobber_regs predicate has no function except to ensure
		% that sanity check in eager code generation does not fail
		% when it shouldn't. Since lazy code generation lacks this
		% sanity check, it doesn't need to do anything.
		VarInfo = VarInfo0
	;
		VarInfo0 = var_locn_info(VarLocn0),
		var_locn__clobber_regs(Regs, VarLocn0, VarLocn),
		VarInfo = var_locn_info(VarLocn)
	},
	code_info__set_var_locns_info(VarInfo).

code_info__save_variables(OutArgs, SavedLocs, Code) -->
	code_info__compute_forward_live_var_saves(OutArgs, VarLocs),
	{ assoc_list__values(VarLocs, SavedLocList) },
	{ set__list_to_set(SavedLocList, SavedLocs) },
	code_info__place_vars(VarLocs, Code).

code_info__save_variables_on_stack(Vars, Code) -->
	list__map_foldl(code_info__associate_stack_slot, Vars, VarLocs),
	code_info__place_vars(VarLocs, Code).

:- pred code_info__compute_forward_live_var_saves(set(prog_var)::in,
	assoc_list(prog_var, lval)::out, code_info::in, code_info::out) is det.

code_info__compute_forward_live_var_saves(OutArgs, VarLocs) -->
	code_info__get_known_variables(Variables0),
	{ set__list_to_set(Variables0, Vars0) },
	code_info__body_typeinfo_liveness(TypeInfoLiveness),
	code_info__get_proc_info(ProcInfo),
	{ proc_info_vartypes(ProcInfo, VarTypes) },
	{ proc_info_typeinfo_varmap(ProcInfo, TVarMap) },
	{ proc_info_maybe_complete_with_typeinfo_vars(Vars0, TypeInfoLiveness,
		VarTypes, TVarMap, Vars1) },
	{ set__difference(Vars1, OutArgs, Vars) },
	{ set__to_sorted_list(Vars, Variables) },
	list__map_foldl(code_info__associate_stack_slot, Variables, VarLocs).

:- pred code_info__associate_stack_slot(prog_var::in,
	pair(prog_var, lval)::out, code_info::in, code_info::out) is det.

code_info__associate_stack_slot(Var, Var - Slot) -->
	code_info__get_variable_slot(Var, Slot).

code_info__max_reg_in_use(Max) -->
	code_info__get_var_locns_info(VarInfo),
	{
		VarInfo = exprn_info(Exprn),
		code_exprn__max_reg_in_use(Exprn, Max)
	;
		VarInfo = var_locn_info(VarLocn),
		var_locn__max_reg_in_use(VarLocn, Max)
	}.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

	% Submodule for dealing with the recording of variable liveness
	% information around calls.
	%
	% Value numbering needs to know what locations are live before calls;
	% the garbage collector and the debugger need to know what locations
	% are live containing what types of values after calls.

:- interface.

:- pred code_info__generate_call_vn_livevals(list(arg_loc)::in,
	set(prog_var)::in, set(lval)::out, code_info::in, code_info::out)
	is det.

:- pred code_info__generate_return_live_lvalues(
	assoc_list(prog_var, arg_loc)::in, instmap::in, bool::in,
	list(liveinfo)::out, code_info::in, code_info::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

code_info__generate_call_vn_livevals(InputArgLocs, OutputArgs, LiveVals) -->
	code_info__generate_call_stack_vn_livevals(OutputArgs, StackLiveVals),
	{ code_info__generate_input_var_vn(InputArgLocs, StackLiveVals,
		LiveVals) }.

:- pred code_info__generate_call_stack_vn_livevals(set(prog_var)::in,
	set(lval)::out, code_info::in, code_info::out) is det.

code_info__generate_call_stack_vn_livevals(OutputArgs, LiveVals) -->
	code_info__get_known_variables(KnownVarList),
	{ set__list_to_set(KnownVarList, KnownVars) },
	{ set__difference(KnownVars, OutputArgs, LiveVars) },
	{ set__to_sorted_list(LiveVars, LiveVarList) },
	{ set__init(LiveVals0) },
	code_info__generate_stack_var_vn(LiveVarList, LiveVals0, LiveVals1),

	code_info__get_active_temps_data(Temps),
	{ code_info__generate_call_temp_vn(Temps, LiveVals1, LiveVals) }.

:- pred code_info__generate_stack_var_vn(list(prog_var)::in, set(lval)::in,
	set(lval)::out, code_info::in, code_info::out) is det.

code_info__generate_stack_var_vn([], Vals, Vals) --> [].
code_info__generate_stack_var_vn([V | Vs], Vals0, Vals) -->
	code_info__get_variable_slot(V, Lval),
	{ set__insert(Vals0, Lval, Vals1) },
	code_info__generate_stack_var_vn(Vs, Vals1, Vals).

:- pred code_info__generate_call_temp_vn(assoc_list(lval, slot_contents)::in,
	set(lval)::in, set(lval)::out) is det.

code_info__generate_call_temp_vn([], Vals, Vals).
code_info__generate_call_temp_vn([Lval - _ | Temps], Vals0, Vals) :-
	set__insert(Vals0, Lval, Vals1),
	code_info__generate_call_temp_vn(Temps, Vals1, Vals).

:- pred code_info__generate_input_var_vn(list(arg_loc)::in,
	set(lval)::in, set(lval)::out) is det.

code_info__generate_input_var_vn([], Vals, Vals).
code_info__generate_input_var_vn([InputArgLoc | InputArgLocs], Vals0, Vals) :-
	code_util__arg_loc_to_register(InputArgLoc, Lval),
	set__insert(Vals0, Lval, Vals1),
	code_info__generate_input_var_vn(InputArgLocs, Vals1, Vals).

%---------------------------------------------------------------------------%

code_info__generate_return_live_lvalues(OutputArgLocs, ReturnInstMap,
		OkToDeleteAny, LiveLvalues) -->
	code_info__variable_locations(VarLocs),
	code_info__get_known_variables(Vars),
	code_info__get_active_temps_data(Temps),
	code_info__get_proc_info(ProcInfo),
	code_info__get_globals(Globals),
	code_info__get_module_info(ModuleInfo),
	{ continuation_info__generate_return_live_lvalues(OutputArgLocs,
		ReturnInstMap, Vars, VarLocs, Temps, ProcInfo, ModuleInfo,
		Globals, OkToDeleteAny, LiveLvalues) }.

:- pred code_info__generate_resume_layout(label::in, resume_map::in,
	code_info::in, code_info::out) is det.

code_info__generate_resume_layout(Label, ResumeMap) -->
	code_info__get_globals(Globals),
	{ globals__lookup_bool_option(Globals, agc_stack_layout,
		AgcStackLayout) },
	( { AgcStackLayout = yes } ->
		code_info__get_active_temps_data(Temps),
		code_info__get_instmap(InstMap),
		code_info__get_proc_info(ProcInfo),
		code_info__get_module_info(ModuleInfo),
		{ continuation_info__generate_resume_layout(ResumeMap,
			Temps, InstMap, ProcInfo, ModuleInfo, Layout) },
		code_info__add_resume_layout_for_label(Label, Layout)
	;
		[]
	).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

	% Submodule for managing stack slots.

	% Det stack frames are organized as follows.
	%
	%		... unused ...
	%	sp --->	<first unused slot>
	%		<space for local var 1>
	%		... local vars ...
	%		<space for local var n>
	%		<space for temporary 1>
	%		... temporaries ...
	%		<space for temporary n>
	%		<space for saved succip, if needed>
	%
	% The stack pointer points to the first free location at the
	% top of the stack.
	%
	% `code_info__succip_is_used' determines whether we need a slot to
	% hold the succip.
	%
	% Nondet stack frames also have the local variables above the
	% temporaries, but contain several fixed slots on top, and the
	% saved succip is stored in one of these.
	%
	% For both kinds of stack frames, the slots holding variables
	% are allocated during the live_vars pass, while the slots holding
	% temporaries are acquired (and if possible, released) on demand
	% during code generation.

:- interface.

	% Returns the total stackslot count, but not including space for
	% succip. This total can change in the future if this call is
	% followed by further allocations of temp slots.
:- pred code_info__get_total_stackslot_count(int, code_info, code_info).
:- mode code_info__get_total_stackslot_count(out, in, out) is det.

	% Acquire a stack slot for storing a temporary. The slot_contents
	% description is for accurate gc.
:- pred code_info__acquire_temp_slot(slot_contents, lval,
	code_info, code_info).
:- mode code_info__acquire_temp_slot(in, out, in, out) is det.

	% Release a stack slot acquired earlier for a temporary value.
:- pred code_info__release_temp_slot(lval, code_info, code_info).
:- mode code_info__release_temp_slot(in, in, out) is det.

	% Return the lval of the stack slot in which the given variable
	% is stored. Aborts if the variable does not have a stack slot
	% an assigned to it.
:- pred code_info__get_variable_slot(prog_var, lval, code_info, code_info).
:- mode code_info__get_variable_slot(in, out, in, out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

code_info__acquire_temp_slot(Item, StackVar) -->
	code_info__get_temps_in_use(TempsInUse0),
	{ IsTempUsable = lambda([TempContent::in, Lval::out] is semidet, (
		TempContent = Lval - ContentType,
		ContentType = Item,
		\+ set__member(Lval, TempsInUse0)
	)) },
	code_info__get_temp_content_map(TempContentMap0),
	{ map__to_assoc_list(TempContentMap0, TempContentList) },
	{ list__filter_map(IsTempUsable, TempContentList, UsableLvals) },
	(
		{ UsableLvals = [UsableLval | _] },
		{ StackVar = UsableLval }
	;
		{ UsableLvals = [] },
		code_info__get_var_slot_count(VarSlots),
		code_info__get_max_temp_slot_count(TempSlots0),
		{ TempSlots is TempSlots0 + 1 },
		{ Slot is VarSlots + TempSlots },
		code_info__stack_variable(Slot, StackVar),
		code_info__set_max_temp_slot_count(TempSlots),
		{ map__det_insert(TempContentMap0, StackVar, Item,
			TempContentMap) },
		code_info__set_temp_content_map(TempContentMap)
	),
	{ set__insert(TempsInUse0, StackVar, TempsInUse) },
	code_info__set_temps_in_use(TempsInUse).

code_info__release_temp_slot(StackVar) -->
	code_info__get_temps_in_use(TempsInUse0),
	{ set__delete(TempsInUse0, StackVar, TempsInUse) },
	code_info__set_temps_in_use(TempsInUse).

%---------------------------------------------------------------------------%

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

code_info__get_total_stackslot_count(NumSlots) -->
	code_info__get_var_slot_count(SlotsForVars),
	code_info__get_max_temp_slot_count(SlotsForTemps),
	{ NumSlots is SlotsForVars + SlotsForTemps }.

:- pred code_info__max_var_slot(stack_slots, int).
:- mode code_info__max_var_slot(in, out) is det.

code_info__max_var_slot(StackSlots, SlotCount) :-
	map__values(StackSlots, StackSlotList),
	code_info__max_var_slot_2(StackSlotList, 0, SlotCount).

:- pred code_info__max_var_slot_2(list(lval), int, int).
:- mode code_info__max_var_slot_2(in, in, out) is det.

code_info__max_var_slot_2([], Max, Max).
code_info__max_var_slot_2([L | Ls], Max0, Max) :-
	( L = stackvar(N) ->
		int__max(N, Max0, Max1)
	; L = framevar(N) ->
		int__max(N, Max0, Max1)
	;
		Max1 = Max0
	),
	code_info__max_var_slot_2(Ls, Max1, Max).

:- pred code_info__stack_variable(int, lval, code_info, code_info).
:- mode code_info__stack_variable(in, out, in, out) is det.

code_info__stack_variable(Num, Lval) -->
	code_info__get_proc_model(CodeModel),
	( { CodeModel = model_non } ->
		{ Lval = framevar(Num) }
	;
		{ Lval = stackvar(Num) }
	).

:- pred code_info__stack_variable_reference(int, rval, code_info, code_info).
:- mode code_info__stack_variable_reference(in, out, in, out) is det.

code_info__stack_variable_reference(Num, mem_addr(Ref)) -->
	code_info__get_proc_model(CodeModel),
	( { CodeModel = model_non } ->
		{ Ref = framevar_ref(Num) }
	;
		{ Ref = stackvar_ref(Num) }
	).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
