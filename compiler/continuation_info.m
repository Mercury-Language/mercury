%-----------------------------------------------------------------------------%
% Copyright (C) 1997-2000,2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: continuation_info.m.
% Main author: trd.
% Extensive modifications by zs.
%
% This file defines the data structures the code generator uses to collect
% information that will later be converted into layout tables for accurate
% garbage collection, for stack tracing, execution tracing and perhaps
% other purposes.
%
% Information is collected in several passes.
%
% 	1 Before we start generating code for a procedure,
%	  we initialize the set of internal labels for which we have
%	  layout information to the empty set. This set is stored in
%	  the code generator state.
%
%	2 During code generation for the procedure, provided the option
%	  trace_stack_layouts is set, we add layout information for labels
%	  that represent trace ports to the code generator state. If
%	  agc_stack_layouts is set, we add layout information for the stack
%	  label in each resumption point. And regardless of option settings,
%	  we also generate layouts to be attached to any closures we create.
%
% 	3 After we finish generating code for a procedure, we record
%	  all the static information about the procedure (some of which
%	  is available only after code generation), together with the
%	  info about internal labels accumulated in the code generator state,
%	  in the global_data structure.
%
% 	4 If agc_stack_layouts is set, we make a pass over the
% 	  optimized code recorded in the final LLDS instructions.
%	  In this pass, we collect information from call instructions
%	  about the internal labels to which calls can return.
%	  This info will also go straight into the global_data.
%
% This module defines the data structures used by all passes. It also
% implements the whole of pass 4, and various fractions of the other passes.
%
% stack_layout.m converts the information collected in this module into
% stack_layout tables.

%-----------------------------------------------------------------------------%

:- module ll_backend__continuation_info.

:- interface.

:- import_module ll_backend__llds, hlds__hlds_module, hlds__hlds_pred.
:- import_module hlds__hlds_goal, parse_tree__prog_data.
:- import_module (parse_tree__inst), hlds__instmap, ll_backend__trace.
:- import_module backend_libs__rtti, libs__globals.
:- import_module bool, std_util, list, assoc_list, set, map.

	%
	% Information for any procedure, includes information about the
	% procedure itself, and any internal labels within it.
	%
:- type proc_layout_info
	--->	proc_layout_info(
			rtti_proc_label	:: rtti_proc_label,
					% The identity of the procedure.
			entry_label	:: label,
					% Determines which stack is used.
			detism		:: determinism,
					% Number of stack slots.
			stack_slot_count :: int,
					% Location of succip on stack.
			succip_slot	:: maybe(int),
					% If the trace level is not none, this
					% contains the label associated with
					% the call event, whose stack layout
					% gives the locations of the input
					% arguments on procedure entry, for
					% use in implementing retry in the
					% debugger.
			eval_method	:: eval_method,
					% Of the procedure.
			call_label	:: maybe(label),
					% If the trace level is not none,
					% this contains the label associated
					% with the call event, whose stack
					% layout says which variables were
					% live and where on entry.
			max_trace_reg	:: int,
					% The number of the highest numbered
					% rN register that can contain useful
					% information during a call to MR_trace
					% from within this procedure.
			head_vars	:: list(prog_var),
					% The head variables, in order,
					% including the ones introduced by the
					% compiler.
			maybe_proc_body	:: maybe(hlds_goal),
					% The body of the procedure, if
					% required.
			initial_instmap	:: instmap,
					% The instmap at the start of the
					% procedure body.
			trace_slot_info	:: trace_slot_info,
					% Info about the stack slots used
					% for tracing.
			need_proc_id	:: bool,
					% Do we require the procedure id
					% section of the procedure layout
					% to be present, even if the option
					% procid_stack_layout is not set?
			varset		:: prog_varset,
					% The names of all the variables.
			vartypes	:: vartypes,
			internal_map	:: proc_label_layout_info,
					% Info for each internal label,
					% needed for basic_stack_layouts.
			table_io_decl	:: maybe(table_io_decl_info),
					% True if the effective trace level
					% of the procedure is not none.
			is_being_traced :: bool,
					% True iff we need the names of all the
					% variables.
			need_all_names	:: bool
		).

	%
	% Information about the labels internal to a procedure.
	%
:- type proc_label_layout_info	==	map(label, internal_layout_info).

	%
	% Information for an internal label.
	%
	% There are three ways for the compiler to generate labels for
	% which layouts may be required:
	%
	% (a) as the label associated with a trace port,
	% (b) as the label associated with resume point that gets stored
	%     as a redoip in a nondet stack frame, and
	% (c) as the return label of some kind of call (plain, method or h-o).
	%
	% Label optimizations may redirect a call return away from the
	% originally generated label to another label, possibly one
	% that is associated with a trace port. This optimization may
	% also direct returns from more than one call to the same label.
	%
	% We may be interested in the layout of things at a label for three
	% different reasons: for stack tracing, for accurate gc, and for
	% execution tracing (which may include up-level printing from the
	% debugger).
	%
	% - For stack tracing, we are interested only in call return labels.
	%   Even for these, we need only the pointer to the procedure layout
	%   info; we do not need any information about variables.
	%
	% - For accurate gc, we are interested only in resume point labels
	%   and call return labels. We need to know about all the variables
	%   that can be accessed after the label; this is the intersection of
	%   all the variables denoted as live in the respective labels.
	%   (Variables which are not in the intersection are not guaranteed
	%   to have a meaningful value on all execution paths that lead to the
	%   label.)
	%
	% - For execution tracing, our primary interest is in trace port
	%   labels. At these labels we only want info about named variables,
	%   but we may want this info even if the variable will never be
	%   referred to again.
	%
	%   When the trace level requires support for up-level printing,
	%   execution tracing also requires information about return labels.
	%   The variables about which we want info at these labels is a subset
	%   of the variables agc is interested in (the named subset).
	%   We do not collect this set explicitly. Instead, if we are doing
	%   execution tracing, we collect agc layout info as usual, and
	%   (if we not really doing agc) remove the unnamed variables
	%   in stack_layout.m.
	%
	% For labels which correspond to a trace port (part (a) above),
	% we record information in the first field. Since trace.m generates
	% a unique label for each trace port, this field is never updated
	% once it is set in pass 2.
	%
	% For labels which correspond to redoips (part (b) above), we record
	% information in the second field. Since code_info.m generates
	% unique labels for each resumption point, this field is never updated
	% once it is set in pass 2.
	% 
	% For labels which correspond to a call return (part (c) above),
	% we record information in the third field during pass 4. If execution
	% tracing is turned on, then jumpopt.m will not redirect call return
	% addresses, and thus each label will correspond to at most one call
	% return. If execution tracing is turned off, jumpopt.m may redirect
	% call return addresses, which means that a label can serve as the
	% return label for more than one call. In that case, this field can be
	% updated after it is set. This updating requires taking the
	% intersection of the sets of live variables, and gathering up all the
	% contexts into a list. Later, stack_layout.m will pick one (valid)
	% context essentially at random, which is OK because the picked
	% context will not be used for anything, except possibly for debugging
	% native gc.
	%
	% Since a call may return to the label of an internal port, it is
	% possible for both fields to be set. In this case, stack_layout.m
	% will take the union of the relevant info. If neither field is set,
	% then the label's layout is required only for stack tracing.
	%
:- type internal_layout_info
	--->	internal_layout_info(
			maybe(trace_port_layout_info),
			maybe(layout_label_info),
			maybe(return_layout_info)
		).

:- type trace_port_layout_info
	--->	trace_port_layout_info(
			prog_context,
			trace_port,
			goal_path,
			layout_label_info
		).

:- type return_layout_info
	--->	return_layout_info(
			assoc_list(code_addr, pair(prog_context, goal_path)),
			layout_label_info
		).

	%
	% Information about the layout of live data for a label.
	%
:- type layout_label_info
	--->	layout_label_info(
			set(var_info),
				% live vars and their locations/names
			map(tvar, set(layout_locn))
				% locations of polymorphic type vars
		).

:- type var_info
	--->	var_info(
			layout_locn,	% the location of the variable
			live_value_type % info about the variable
		).

:- type closure_layout_info
	--->	closure_layout_info(
			list(closure_arg_info),
				% there is one closure_arg_info for each
				% argument of the called procedure,
				% even the args which are not in the closure
			map(tvar, set(layout_locn))
				% locations of polymorphic type vars,
				% encoded so that rN refers to argument N
		).

:- type closure_arg_info
	--->	closure_arg_info(
			type,	% The type of the argument.
			(inst)	% The initial inst of the argument.

				% It may be useful in the future to include
				% info about the final insts and about
				% the determinism. This would allow us
				% to implement checked dynamic inst casts,
				% which may be helpful for dynamic loading.
				% It may also be useful for printing
				% closures and for providing user-level
				% RTTI access.
		).

:- type slot_contents
	--->	ticket			% a ticket (trail pointer)
	;	ticket_counter		% a copy of the ticket counter
	;	trace_data
	;	sync_term		% a syncronization term used
					% at the end of par_conjs.
					% see par_conj_gen.m for details.
	;	lval(lval).

	% Call continuation_info__maybe_process_proc_llds on the code
	% of every procedure in the list.
:- pred continuation_info__maybe_process_llds(list(c_procedure)::in,
	module_info::in, global_data::in, global_data::out) is det.

	% Check whether this procedure ought to have any layout structures
	% generated for it. If yes, then update the global_data to
	% include all the continuation labels within a proc. Whether or not
	% the information about a continuation label includes the variables
	% live at that label depends on the values of options.
:- pred continuation_info__maybe_process_proc_llds(list(instruction)::in,
	pred_proc_id::in, module_info::in,
	global_data::in, global_data::out) is det.

	% Check whether the given procedure should have at least (a) a basic
	% stack layout, and (b) a procedure id layout generated for it.
	% The two bools returned answer these two questions respectively.
:- pred continuation_info__basic_stack_layout_for_proc(pred_info::in,
	globals::in, bool::out, bool::out) is det.

	% Generate the layout information we need for the return point
	% of a call.
:- pred continuation_info__generate_return_live_lvalues(
	assoc_list(prog_var, arg_loc)::in, instmap::in, list(prog_var)::in,
	map(prog_var, set(lval))::in, assoc_list(lval, slot_contents)::in,
	proc_info::in, module_info::in, globals::in, bool::in,
	list(liveinfo)::out) is det.

	% Generate the layout information we need for a resumption point,
	% a label where forward execution can restart after backtracking.
:- pred continuation_info__generate_resume_layout(map(prog_var, set(lval))::in,
	assoc_list(lval, slot_contents)::in, instmap::in, proc_info::in,
	module_info::in, layout_label_info::out) is det.

	% Generate the layout information we need to include in a closure.
:- pred continuation_info__generate_closure_layout(module_info::in,
	pred_id::in, proc_id::in, closure_layout_info::out) is det.

	% For each type variable in the given list, find out where the
	% typeinfo var for that type variable is.
:- pred continuation_info__find_typeinfos_for_tvars(list(tvar)::in,
	map(prog_var, set(lval))::in, proc_info::in,
	map(tvar, set(layout_locn))::out) is det.

:- pred continuation_info__generate_table_decl_io_layout(proc_info::in,
	assoc_list(prog_var, int)::in, table_io_decl_info::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds__hlds_goal, hlds__hlds_llds.
:- import_module check_hlds__type_util, check_hlds__inst_match.
:- import_module ll_backend__code_util.
:- import_module libs__options.

:- import_module string, require, term, varset.

%-----------------------------------------------------------------------------%

	% Exported predicates.

continuation_info__maybe_process_llds([], _) --> [].
continuation_info__maybe_process_llds([Proc | Procs], ModuleInfo) -->
	{ Proc = c_procedure(_, _, PredProcId, Instrs, _, _, _) },
	continuation_info__maybe_process_proc_llds(Instrs, PredProcId,
		ModuleInfo),
	continuation_info__maybe_process_llds(Procs, ModuleInfo).

continuation_info__maybe_process_proc_llds(Instructions, PredProcId,
		ModuleInfo, ContInfo0, ContInfo) :-
	PredProcId = proc(PredId, _),
	module_info_pred_info(ModuleInfo, PredId, PredInfo),
	module_info_globals(ModuleInfo, Globals),
	continuation_info__basic_stack_layout_for_proc(PredInfo, Globals,
		Layout, _),
	( Layout = yes ->
		globals__want_return_var_layouts(Globals, WantReturnLayout),
		continuation_info__process_proc_llds(PredProcId, Instructions,
			WantReturnLayout, ContInfo0, ContInfo)
	;
		ContInfo = ContInfo0
	).

:- type call_info
	--->	call_info(
			label,		% the return label
			code_addr,	% the target of the call
			list(liveinfo),	% what is live on return
			term__context,	% the position of the call in source
			goal_path	% the position of the call in the body;
					% meaningful only if tracing is enabled
		).

	%
	% Process the list of instructions for this proc, adding
	% all internal label information to global_data.
	%
:- pred continuation_info__process_proc_llds(pred_proc_id::in,
	list(instruction)::in, bool::in,
	global_data::in, global_data::out) is det.

continuation_info__process_proc_llds(PredProcId, Instructions,
		WantReturnInfo, GlobalData0, GlobalData) :-

		% Get all the continuation info from the call instructions.
	global_data_get_proc_layout(GlobalData0, PredProcId, ProcLayoutInfo0),
	Internals0 = ProcLayoutInfo0^internal_map,
	GetCallInfo = lambda([Instr::in, Call::out] is semidet, (
		Instr = call(Target, label(ReturnLabel), LiveInfo, Context,
			GoalPath, _) - _Comment,
		Call = call_info(ReturnLabel, Target, LiveInfo, Context,
			GoalPath)
	)),
	list__filter_map(GetCallInfo, Instructions, Calls),

		% Process the continuation label info.
	list__foldl(continuation_info__process_continuation(WantReturnInfo),
		Calls, Internals0, Internals),

	ProcLayoutInfo = ProcLayoutInfo0^internal_map := Internals,
	global_data_update_proc_layout(GlobalData0, PredProcId, ProcLayoutInfo,
		GlobalData).

%-----------------------------------------------------------------------------%

	%
	% Collect the liveness information from a single return label
	% and add it to the internals.
	%
:- pred continuation_info__process_continuation(bool::in, call_info::in,
	proc_label_layout_info::in, proc_label_layout_info::out) is det.

continuation_info__process_continuation(WantReturnInfo, CallInfo,
		Internals0, Internals) :-
	CallInfo = call_info(ReturnLabel, Target, LiveInfoList,
		Context, MaybeGoalPath),
	( map__search(Internals0, ReturnLabel, Internal0) ->
		Internal0 = internal_layout_info(Port0, Resume0, Return0)
	;
		Port0 = no,
		Resume0 = no,
		Return0 = no
	),
	( WantReturnInfo = yes ->
		continuation_info__convert_return_data(LiveInfoList,
			VarInfoSet, TypeInfoMap),
		(
			Return0 = no,
			Layout = layout_label_info(VarInfoSet, TypeInfoMap),
			ReturnInfo = return_layout_info(
				[Target - (Context - MaybeGoalPath)],
				Layout),
			Return = yes(ReturnInfo)
		;
				% If a var is known to be dead
				% on return from one call, it
				% cannot be accessed on returning
				% from the other calls that reach
				% the same return address either.
			Return0 = yes(ReturnInfo0),
			ReturnInfo0 = return_layout_info(TargetsContexts0,
				Layout0),
			Layout0 = layout_label_info(LV0, TV0),
			set__intersect(LV0, VarInfoSet, LV),
			map__intersect(set__intersect, TV0, TypeInfoMap, TV),
			Layout = layout_label_info(LV, TV),
			TargetContexts = [Target - (Context - MaybeGoalPath)
				| TargetsContexts0],
			ReturnInfo = return_layout_info(TargetContexts,
				Layout),
			Return = yes(ReturnInfo)
		)
	;
		Return = Return0
	),
	Internal = internal_layout_info(Port0, Resume0, Return),
	map__set(Internals0, ReturnLabel, Internal, Internals).

:- pred continuation_info__convert_return_data(list(liveinfo)::in,
	set(var_info)::out, map(tvar, set(layout_locn))::out) is det.

continuation_info__convert_return_data(LiveInfos, VarInfoSet, TypeInfoMap) :-
	GetVarInfo = lambda([LiveLval::in, VarInfo::out] is det, (
		LiveLval = live_lvalue(Lval, LiveValueType, _),
		VarInfo = var_info(Lval, LiveValueType)
	)),
	list__map(GetVarInfo, LiveInfos, VarInfoList),
	GetTypeInfo = lambda([LiveLval::in, LiveTypeInfoMap::out] is det, (
		LiveLval = live_lvalue(_, _, LiveTypeInfoMap)
	)),
	list__map(GetTypeInfo, LiveInfos, TypeInfoMapList),
	map__init(Empty),
	list__foldl(lambda([TIM1::in, TIM2::in, TIM::out] is det,
		map__union(set__intersect, TIM1, TIM2, TIM)),
		TypeInfoMapList, Empty, TypeInfoMap),
	set__list_to_set(VarInfoList, VarInfoSet).

:- pred continuation_info__filter_named_vars(list(liveinfo)::in,
	list(liveinfo)::out) is det.

continuation_info__filter_named_vars([], []).
continuation_info__filter_named_vars([LiveInfo | LiveInfos], Filtered) :-
	continuation_info__filter_named_vars(LiveInfos, Filtered1),
	(
		LiveInfo = live_lvalue(_, LiveType, _),
		LiveType = var(_, Name, _, _),
		Name \= ""
	->
		Filtered = [LiveInfo | Filtered1]
	;
		Filtered = Filtered1
	).

%-----------------------------------------------------------------------------%

continuation_info__basic_stack_layout_for_proc(PredInfo, Globals,
		BasicLayout, ForceProcIdLayout) :-
	(
		globals__lookup_bool_option(Globals, stack_trace_higher_order,
			yes),
		continuation_info__some_arg_is_higher_order(PredInfo)
	->
		BasicLayout = yes,
		ForceProcIdLayout = yes
	;
		globals__lookup_bool_option(Globals, basic_stack_layout, yes)
	->
		BasicLayout = yes,
		ForceProcIdLayout = no
	;
		BasicLayout = no,
		ForceProcIdLayout = no
	).

:- pred continuation_info__some_arg_is_higher_order(pred_info::in) is semidet.

continuation_info__some_arg_is_higher_order(PredInfo) :-
	pred_info_arg_types(PredInfo, ArgTypes),
	some([Type], (
		list__member(Type, ArgTypes),
		type_is_higher_order(Type, _, _, _)
	)).

%-----------------------------------------------------------------------------%

continuation_info__generate_return_live_lvalues(OutputArgLocs, ReturnInstMap,
		Vars, VarLocs, Temps, ProcInfo, ModuleInfo, Globals,
		OkToDeleteAny, LiveLvalues) :-
	globals__want_return_var_layouts(Globals, WantReturnVarLayout),
	proc_info_stack_slots(ProcInfo, StackSlots),
	continuation_info__find_return_var_lvals(Vars, StackSlots,
		OkToDeleteAny, OutputArgLocs, VarLvals),
	continuation_info__generate_var_live_lvalues(VarLvals, ReturnInstMap,
		VarLocs, ProcInfo, ModuleInfo,
		WantReturnVarLayout, VarLiveLvalues),
	continuation_info__generate_temp_live_lvalues(Temps, TempLiveLvalues),
	list__append(VarLiveLvalues, TempLiveLvalues, LiveLvalues).

:- pred continuation_info__find_return_var_lvals(list(prog_var)::in,
	stack_slots::in, bool::in, assoc_list(prog_var, arg_loc)::in,
	assoc_list(prog_var, lval)::out) is det.

continuation_info__find_return_var_lvals([], _, _, _, []).
continuation_info__find_return_var_lvals([Var | Vars], StackSlots,
		OkToDeleteAny, OutputArgLocs, VarLvals) :-
	continuation_info__find_return_var_lvals(Vars, StackSlots,
		OkToDeleteAny, OutputArgLocs, TailVarLvals),
	( assoc_list__search(OutputArgLocs, Var, ArgLoc) ->
		% On return, output arguments are in their registers.
		code_util__arg_loc_to_register(ArgLoc, Lval),
		VarLvals = [Var - Lval | TailVarLvals]
	; map__search(StackSlots, Var, Lval) ->
		% On return, other live variables are in their stack slots.
		VarLvals = [Var - Lval | TailVarLvals]
	; OkToDeleteAny = yes ->
		VarLvals = TailVarLvals
	;
		error("continuation_info__find_return_var_lvals: no slot")
	).

:- pred continuation_info__generate_temp_live_lvalues(
	assoc_list(lval, slot_contents)::in, list(liveinfo)::out) is det.

continuation_info__generate_temp_live_lvalues([], []).
continuation_info__generate_temp_live_lvalues([Temp | Temps], [Live | Lives]) :-
	Temp = Slot - Contents,
	continuation_info__live_value_type(Contents, LiveLvalueType),
	map__init(Empty),
	Live = live_lvalue(direct(Slot), LiveLvalueType, Empty),
	continuation_info__generate_temp_live_lvalues(Temps, Lives).

:- pred continuation_info__generate_var_live_lvalues(
	assoc_list(prog_var, lval)::in, instmap::in,
	map(prog_var, set(lval))::in, proc_info::in, module_info::in,
	bool::in, list(liveinfo)::out) is det.

continuation_info__generate_var_live_lvalues([], _, _, _, _, _, []).
continuation_info__generate_var_live_lvalues([Var - Lval | VarLvals], InstMap,
		VarLocs, ProcInfo, ModuleInfo, WantReturnVarLayout,
		[Live | Lives]) :-
	( WantReturnVarLayout = yes ->
		continuation_info__generate_layout_for_var(Var, InstMap,
			ProcInfo, ModuleInfo, LiveValueType, TypeVars),
		continuation_info__find_typeinfos_for_tvars(TypeVars,
			VarLocs, ProcInfo, TypeParams),
		Live = live_lvalue(direct(Lval), LiveValueType, TypeParams)
	;
		map__init(Empty),
		Live = live_lvalue(direct(Lval), unwanted, Empty)
	),
	continuation_info__generate_var_live_lvalues(VarLvals, InstMap,
		VarLocs, ProcInfo, ModuleInfo, WantReturnVarLayout, Lives).

%---------------------------------------------------------------------------%

continuation_info__generate_resume_layout(ResumeMap, Temps, InstMap,
		ProcInfo, ModuleInfo, Layout) :-
	map__to_assoc_list(ResumeMap, ResumeList),
	set__init(TVars0),
	continuation_info__generate_resume_layout_for_vars(ResumeList,
		InstMap, ProcInfo, ModuleInfo, VarInfos, TVars0, TVars),
	set__list_to_set(VarInfos, VarInfoSet),
	set__to_sorted_list(TVars, TVarList),
	continuation_info__find_typeinfos_for_tvars(TVarList, ResumeMap,
		ProcInfo, TVarInfoMap),
	continuation_info__generate_temp_var_infos(Temps, TempInfos),
	set__list_to_set(TempInfos, TempInfoSet),
	set__union(VarInfoSet, TempInfoSet, AllInfoSet),
	Layout = layout_label_info(AllInfoSet, TVarInfoMap).

:- pred continuation_info__generate_resume_layout_for_vars(
	assoc_list(prog_var, set(lval))::in, instmap::in, proc_info::in,
	module_info::in, list(var_info)::out, set(tvar)::in,
	set(tvar)::out) is det.

continuation_info__generate_resume_layout_for_vars([], _, _, _, [],
		TVars, TVars).
continuation_info__generate_resume_layout_for_vars([Var - LvalSet | VarLvals],
		InstMap, ProcInfo, ModuleInfo, [VarInfo | VarInfos],
		TVars0, TVars) :-
	continuation_info__generate_resume_layout_for_var(Var, LvalSet,
		InstMap, ProcInfo, ModuleInfo, VarInfo, TypeVars),
	set__insert_list(TVars0, TypeVars, TVars1),
	continuation_info__generate_resume_layout_for_vars(VarLvals,
		InstMap, ProcInfo, ModuleInfo, VarInfos, TVars1, TVars).

:- pred continuation_info__generate_resume_layout_for_var(prog_var::in,
	set(lval)::in, instmap::in, proc_info::in, module_info::in,
	var_info::out, list(tvar)::out) is det.

continuation_info__generate_resume_layout_for_var(Var, LvalSet, InstMap,
		ProcInfo, ModuleInfo, VarInfo, TypeVars) :-
	set__to_sorted_list(LvalSet, LvalList),
	( LvalList = [LvalPrime] ->
		Lval = LvalPrime
	;
		error("var has more than one lval in stack resume map")
	),
	continuation_info__generate_layout_for_var(Var, InstMap, ProcInfo,
		ModuleInfo, LiveValueType, TypeVars),
	VarInfo = var_info(direct(Lval), LiveValueType).

:- pred continuation_info__generate_temp_var_infos(
	assoc_list(lval, slot_contents)::in, list(var_info)::out) is det.

continuation_info__generate_temp_var_infos([], []).
continuation_info__generate_temp_var_infos([Temp | Temps], [Live | Lives]) :-
	Temp = Slot - Contents,
	continuation_info__live_value_type(Contents, LiveLvalueType),
	Live = var_info(direct(Slot), LiveLvalueType),
	continuation_info__generate_temp_var_infos(Temps, Lives).

%---------------------------------------------------------------------------%

:- pred continuation_info__generate_layout_for_var(prog_var::in, instmap::in,
	proc_info::in, module_info::in, live_value_type::out, list(tvar)::out)
	is det.

continuation_info__generate_layout_for_var(Var, InstMap, ProcInfo, ModuleInfo,
		LiveValueType, TypeVars) :-
	proc_info_varset(ProcInfo, VarSet),
	proc_info_vartypes(ProcInfo, VarTypes),
	( varset__search_name(VarSet, Var, GivenName) ->
		Name = GivenName
	;
		Name = ""
	),
	instmap__lookup_var(InstMap, Var, Inst),
	map__lookup(VarTypes, Var, Type),
	( inst_match__inst_is_ground(ModuleInfo, Inst) ->
		LldsInst = ground
	;
		LldsInst = partial(Inst)
	),
	LiveValueType = var(Var, Name, Type, LldsInst),
	type_util__real_vars(Type, TypeVars).

%---------------------------------------------------------------------------%

continuation_info__generate_closure_layout(ModuleInfo, PredId, ProcId,
		ClosureLayout) :-
	module_info_pred_proc_info(ModuleInfo, PredId, ProcId,
		PredInfo, ProcInfo),
	proc_info_headvars(ProcInfo, HeadVars),
	proc_info_arg_info(ProcInfo, ArgInfos),
	pred_info_arg_types(PredInfo, ArgTypes),
	proc_info_get_initial_instmap(ProcInfo, ModuleInfo, InstMap),
	map__init(VarLocs0),
	set__init(TypeVars0),
	(
		continuation_info__build_closure_info(HeadVars, ArgTypes,
			ArgInfos, ArgLayouts, InstMap, VarLocs0, VarLocs,
			TypeVars0, TypeVars)
	->
		set__to_sorted_list(TypeVars, TypeVarsList),
		continuation_info__find_typeinfos_for_tvars(TypeVarsList,
			VarLocs, ProcInfo, TypeInfoDataMap),
		ClosureLayout = closure_layout_info(ArgLayouts,
			TypeInfoDataMap)
	;
		error("proc headvars and pred argtypes disagree on arity")
	).

:- pred continuation_info__build_closure_info(list(prog_var)::in,
	list(type)::in, list(arg_info)::in,  list(closure_arg_info)::out,
	instmap::in, map(prog_var, set(lval))::in,
	map(prog_var, set(lval))::out, set(tvar)::in, set(tvar)::out)
	is semidet.

continuation_info__build_closure_info([], [], [], [], _, VarLocs, VarLocs,
		TypeVars, TypeVars).
continuation_info__build_closure_info([Var | Vars], [Type | Types],
		[ArgInfo | ArgInfos], [Layout | Layouts], InstMap,
		VarLocs0, VarLocs, TypeVars0, TypeVars) :-
	ArgInfo = arg_info(ArgLoc, _ArgMode),
	instmap__lookup_var(InstMap, Var, Inst),
	Layout = closure_arg_info(Type, Inst),
	set__singleton_set(Locations, reg(r, ArgLoc)),
	map__det_insert(VarLocs0, Var, Locations, VarLocs1),
	type_util__real_vars(Type, VarTypeVars),
	set__insert_list(TypeVars0, VarTypeVars, TypeVars1),
	continuation_info__build_closure_info(Vars, Types, ArgInfos, Layouts,
		InstMap, VarLocs1, VarLocs, TypeVars1, TypeVars).

%---------------------------------------------------------------------------%

continuation_info__find_typeinfos_for_tvars(TypeVars, VarLocs, ProcInfo,
		TypeInfoDataMap) :-
	proc_info_varset(ProcInfo, VarSet),
	proc_info_typeinfo_varmap(ProcInfo, TypeInfoMap),
	map__apply_to_list(TypeVars, TypeInfoMap, TypeInfoLocns),
	FindLocn = lambda([TypeInfoLocn::in, Locns::out] is det, (
		type_info_locn_var(TypeInfoLocn, TypeInfoVar),
		(
			map__search(VarLocs, TypeInfoVar, TypeInfoLvalSet)
		->
			ConvertLval = lambda([Locn::out] is nondet, (
				set__member(Lval, TypeInfoLvalSet),
				(
					TypeInfoLocn = typeclass_info(_,
						FieldNum),
					Locn = indirect(Lval, FieldNum)
				;
					TypeInfoLocn = type_info(_),
					Locn = direct(Lval)
				)
			)),
			solutions_set(ConvertLval, Locns)
		;
			varset__lookup_name(VarSet, TypeInfoVar,
				VarString),
			string__format("%s: %s %s",
			    [s("continuation_info__find_typeinfos_for_tvars"),
				s("can't find rval for type_info var"),
				s(VarString)], ErrStr),
			error(ErrStr)
		)
	)),
	list__map(FindLocn, TypeInfoLocns, TypeInfoVarLocns),
	map__from_corresponding_lists(TypeVars, TypeInfoVarLocns,
		TypeInfoDataMap).

%---------------------------------------------------------------------------%

continuation_info__generate_table_decl_io_layout(ProcInfo, NumberedVars,
		TableIoDeclLayout) :-
	proc_info_vartypes(ProcInfo, VarTypes),
	set__init(TypeVars0),
	continuation_info__build_table_io_decl_arg_info(VarTypes,
		NumberedVars, ArgLayouts, TypeVars0, TypeVars),
	set__to_sorted_list(TypeVars, TypeVarsList),
	continuation_info__find_typeinfos_for_tvars_table_io_decl(TypeVarsList,
		NumberedVars, ProcInfo, TypeInfoDataMap),
	TableIoDeclLayout = table_io_decl_info(ArgLayouts, TypeInfoDataMap).

:- pred continuation_info__build_table_io_decl_arg_info(vartypes::in,
	assoc_list(prog_var, int)::in, list(table_io_decl_arg_info)::out,
	set(tvar)::in, set(tvar)::out) is det.

continuation_info__build_table_io_decl_arg_info(_, [], [], TypeVars, TypeVars).
continuation_info__build_table_io_decl_arg_info(VarTypes,
		[Var - SlotNum | NumberedVars], [ArgLayout | ArgLayouts],
		TypeVars0, TypeVars) :-
	map__lookup(VarTypes, Var, Type),
	ArgLayout = table_io_decl_arg_info(Var, SlotNum, Type),
	type_util__real_vars(Type, VarTypeVars),
	set__insert_list(TypeVars0, VarTypeVars, TypeVars1),
	continuation_info__build_table_io_decl_arg_info(VarTypes,
		NumberedVars, ArgLayouts, TypeVars1, TypeVars).

%---------------------------------------------------------------------------%

:- pred continuation_info__find_typeinfos_for_tvars_table_io_decl(
	list(tvar)::in, assoc_list(prog_var, int)::in, proc_info::in,
	map(tvar, table_io_decl_locn)::out) is det.

continuation_info__find_typeinfos_for_tvars_table_io_decl(TypeVars,
		NumberedVars, ProcInfo, TypeInfoDataMap) :-
	proc_info_varset(ProcInfo, VarSet),
	proc_info_typeinfo_varmap(ProcInfo, TypeInfoMap),
	map__apply_to_list(TypeVars, TypeInfoMap, TypeInfoLocns),
	FindLocn = lambda([TypeInfoLocn::in, Locn::out] is det, (
		(
			(
				TypeInfoLocn = typeclass_info(TypeInfoVar,
					FieldNum),
				assoc_list__search(NumberedVars, TypeInfoVar,
					Slot),
				LocnPrime = indirect(Slot, FieldNum)
			;
				TypeInfoLocn = type_info(TypeInfoVar),
				assoc_list__search(NumberedVars, TypeInfoVar,
					Slot),
				LocnPrime = direct(Slot)
			)
		->
			Locn = LocnPrime
		;
			type_info_locn_var(TypeInfoLocn, TypeInfoVar),
			varset__lookup_name(VarSet, TypeInfoVar, VarString),
			string__format("%s: %s %s",
				[s("continuation_info__find_typeinfos_for_tvars_table_io_decl"),
				s("can't find slot for type_info var"),
				s(VarString)], ErrStr),
			error(ErrStr)
		)
	)),
	list__map(FindLocn, TypeInfoLocns, TypeInfoVarLocns),
	map__from_corresponding_lists(TypeVars, TypeInfoVarLocns,
		TypeInfoDataMap).

%-----------------------------------------------------------------------------%

:- pred continuation_info__live_value_type(slot_contents::in,
	live_value_type::out) is det.

continuation_info__live_value_type(lval(succip), succip).
continuation_info__live_value_type(lval(hp), hp).
continuation_info__live_value_type(lval(maxfr), maxfr).
continuation_info__live_value_type(lval(curfr), curfr).
continuation_info__live_value_type(lval(succfr(_)), unwanted).
continuation_info__live_value_type(lval(prevfr(_)), unwanted).
continuation_info__live_value_type(lval(redofr(_)), unwanted).
continuation_info__live_value_type(lval(redoip(_)), unwanted).
continuation_info__live_value_type(lval(succip(_)), unwanted).
continuation_info__live_value_type(lval(sp), unwanted).
continuation_info__live_value_type(lval(lvar(_)), unwanted).
continuation_info__live_value_type(lval(field(_, _, _)), unwanted).
continuation_info__live_value_type(lval(temp(_, _)), unwanted).
continuation_info__live_value_type(lval(reg(_, _)), unwanted).
continuation_info__live_value_type(lval(stackvar(_)), unwanted).
continuation_info__live_value_type(lval(framevar(_)), unwanted).
continuation_info__live_value_type(lval(mem_ref(_)), unwanted).	% XXX
continuation_info__live_value_type(ticket, unwanted). % XXX we may need to
					% modify this, if the GC is going
					% to garbage-collect the trail.
continuation_info__live_value_type(ticket_counter, unwanted).
continuation_info__live_value_type(sync_term, unwanted).
continuation_info__live_value_type(trace_data, unwanted).

%-----------------------------------------------------------------------------%
