%-----------------------------------------------------------------------------%
% Copyright (C) 1997-1998 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: continuation_info.m.
% Main author: trd.
% Extensive modifications by zs.
%
% This file defines the continuation_info data structure, which the code
% generator uses to collect information that will later be converted into
% stack_layout tables for accurate garbage collection, for stack tracing,
% execution tracing and perhaps other purposes.
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
%	  that represent trace ports to the code generator state.
%
% 	3 After we finish generating code for a procedure, we record
%	  all the static information about the procedure (some of which
%	  is available only after code generation), together with the
%	  info about internal labels accumulated in the code generator state,
%	  in the continuation_info structure (which is part of HLDS).
%
% 	4 If agc_stack_layouts is set, we make a pass over the
% 	  optimized code recorded in the final LLDS instructions.
%	  In this pass, we collect information from call instructions
%	  about the internal labels to which calls can return.
%	  This info will also go straight into the continuation_info
%	  in the HLDS.
%
% stack_layout.m converts the information collected in this module into
% stack_layout tables.

%-----------------------------------------------------------------------------%

:- module continuation_info.

:- interface.

:- import_module llds, hlds_pred, prog_data, hlds_data.
:- import_module set, map, list, std_util, bool.

	%
	% Information used by the continuation_info module.
	% This is an abstract data type - when processing is finished
	% use continuation_info__get_all_entries to retrieve the
	% completed proc_layout_infos.
	%
:- type continuation_info.

	%
	% Information for any procedure, includes information about the
	% procedure itself, and any internal labels within it.
	%
:- type proc_layout_info
	--->	proc_layout_info(
			label,		% The entry label.
			determinism,	% Determines which stack is used.
			int,		% Number of stack slots.
			maybe(int),	% Location of succip on stack.
			maybe(label),	% If the trace level is not none,
					% this contains the label associated
					% with the call event, whose stack
					% layout says which variables were
					% live and where on entry.
			maybe(int),	% If the trace level is shallow,
					% this contains the number of the
					% stack slot containing the
					% value of MR_trace_from_full
					% at the time of the call.
			proc_label_layout_info
					% Info for each internal label,
					% needed for basic_stack_layouts.
		).

	%
	% Information about the labels internal to a procedure.
	%
:- type proc_label_layout_info	==	map(label, internal_layout_info).

	%
	% Information for an internal label.
	%
	% There are two ways for the compiler to generate labels for
	% which layouts may be required:
	%
	% (a) as the label associated with a trace port, and
	% (b) as the return label of some kind of call (plain, method or h-o).
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
	% - For accurate gc, we are interested only in call return labels.
	%   We need to know about all the variables that can be accessed
	%   after the label; this is the intersection of all the variables
	%   denoted as live in the call instructions. (Variables which
	%   are not in the intersection are not guaranteed to have a
	%   meaningful value on all execution paths that lead to the label.)
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
	% once it is set in pass 2. For labels which correspond to a call
	% return, we record information in the second field during pass 4.
	% Since a label can serve as the return label for more than once call,
	% this field can be updated (by taking the intersection of the live
	% variables) after it is set. Since a call may return to the label
	% of an internal port, it is possible for both fields to be set.
	% In this case, stack_layout.m will take the union of the relevant
	% info. If neither field is set, then the label's layout is required
	% only for stack tracing.
	%
:- type internal_layout_info
	--->	internal_layout_info(
			maybe(layout_label_info),
			maybe(layout_label_info)
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

	% Return an initialized continuation info structure.

:- pred continuation_info__init(continuation_info::out) is det.

	%
	% Add the information for a single proc.
	%
	% Takes the pred_proc_id, entry label, the number of stack slots,
	% the code model for this proc, and the stack slot of the succip
	% in this proc (if there is one).
	%
:- pred continuation_info__add_proc_info(pred_proc_id::in, label::in,
	int::in, determinism::in, maybe(int)::in, maybe(label)::in,
	maybe(int)::in, proc_label_layout_info::in, continuation_info::in,
	continuation_info::out) is det.

	%
	% Call continuation_info__process_instructions on the code
	% of every procedure in the list.
	%
:- pred continuation_info__process_llds(list(c_procedure)::in, bool::in,
	continuation_info::in, continuation_info::out) is det.

	%
	% Add the information for all the continuation labels within a proc.
	% The bool says whether we want information about the variables
	% live at continuation labels.
	%
:- pred continuation_info__process_instructions(pred_proc_id::in,
	list(instruction)::in, bool::in,
	continuation_info::in, continuation_info::out) is det.

	%
	% Get the finished list of proc_layout_infos.
	%
:- pred continuation_info__get_all_proc_layouts(continuation_info::in,
	list(proc_layout_info)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require.

	% The continuation_info data structure
:- type continuation_info	==	map(pred_proc_id, proc_layout_info).

%-----------------------------------------------------------------------------%

	% Exported predicates.

	%
	% Initialize the continuation_info
	%

continuation_info__init(ContInfo) :-
	map__init(ContInfo).

	%
	% Add the info for this proc (a proc_layout_info) to the
	% continuation_info. 
	%
continuation_info__add_proc_info(PredProcId, EntryLabel, StackSize,
		Detism, SuccipLocation, MaybeTraceCallLabel,
		MaybeFromFullSlot, InternalMap, ContInfo0, ContInfo) :-
	( map__contains(ContInfo0, PredProcId) ->
		error("duplicate continuation_info for proc.")
	;
		LayoutInfo = proc_layout_info(EntryLabel, Detism, StackSize,
			SuccipLocation, MaybeTraceCallLabel,
			MaybeFromFullSlot, InternalMap),
		map__det_insert(ContInfo0, PredProcId, LayoutInfo, ContInfo)
	).

	%
	% Get all the proc_layout_infos.
	%
continuation_info__get_all_proc_layouts(ContInfo, Entries) :-
	map__values(ContInfo, Entries).

continuation_info__process_llds([], _) --> [].
continuation_info__process_llds([Proc | Procs], WantReturnInfo) -->
	{ Proc = c_procedure(_, _, PredProcId, Instrs) },
	continuation_info__process_instructions(PredProcId, Instrs,
		WantReturnInfo),
	continuation_info__process_llds(Procs, WantReturnInfo).

	%
	% Process the list of instructions for this proc, adding
	% all internal label information to the continuation_info.
	%
continuation_info__process_instructions(PredProcId, Instructions,
		WantReturnInfo, ContInfo0, ContInfo) :-

		% Get all the continuation info from the call instructions.
	map__lookup(ContInfo0, PredProcId, ProcLayoutInfo0),
	ProcLayoutInfo0 = proc_layout_info(A, B, C, D, E, F, Internals0),
	GetCallLivevals = lambda([Instr::in, Pair::out] is semidet, (
		Instr = call(_, label(Label), LiveInfo, _) - _Comment,
		Pair = Label - LiveInfo
	)),
	list__filter_map(GetCallLivevals, Instructions, Calls),

		% Process the continuation label info.
	list__foldl(continuation_info__process_continuation(WantReturnInfo),
		Calls, Internals0, Internals),

	ProcLayoutInfo = proc_layout_info(A, B, C, D, E, F, Internals),
	map__det_update(ContInfo0, PredProcId, ProcLayoutInfo, ContInfo).

%-----------------------------------------------------------------------------%

	%
	% Collect the liveness information from a single return label
	% and add it to the internals.
	%
:- pred continuation_info__process_continuation(bool::in,
	pair(label, list(liveinfo))::in, 
	proc_label_layout_info::in, proc_label_layout_info::out) is det.

continuation_info__process_continuation(WantReturnInfo, Label - LiveInfoList,
		Internals0, Internals) :-
	( map__search(Internals0, Label, Internal0) ->
		Internal0 = internal_layout_info(Port0, Return0)
	;
		Port0 = no,
		Return0 = no
	),
	( WantReturnInfo = yes ->
		continuation_info__convert_return_data(LiveInfoList,
			VarInfoSet, TypeInfoMap),
		(
			Return0 = no,
			Return = yes(layout_label_info(VarInfoSet,
				TypeInfoMap))
		;
				% If a var is known to be dead
				% on return from one call, it
				% cannot be accessed on returning
				% from the other calls that reach
				% the same return address either.
			Return0 = yes(layout_label_info(LV0, TV0)),
			set__intersect(LV0, VarInfoSet, LV),
			map__intersect(set__intersect, TV0, TypeInfoMap, TV),
			Return = yes(layout_label_info(LV, TV))
		)
	;
		Return = Return0
	),
	Internal = internal_layout_info(Port0, Return),
	map__set(Internals0, Label, Internal, Internals).

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
