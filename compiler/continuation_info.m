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
% 	- Before we start generating code for a procedure,
%	  we initialize the set of internal labels for which we have
%	  layout information to the empty set. This set is stored in
%	  the code generator state.
%
%	- During code generation for the procedure, provided the option
%	  trace_stack_layouts is set, we add layout information for labels
%	  that represent trace ports to the code generator state.
%
% 	- After we finish generating code for a procedure, we record
%	  all the static information about the procedure (some of which
%	  is available only after code generation), together with the
%	  info about internal labels accumulated in the code generator state,
%	  in the continuation_info structure (which is part of HLDS).
%
% 	- If agc_stack_layouts is set, we make a pass over the
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
:- import_module set, map, list, std_util.

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
			proc_label,	% the proc label
			determinism,	% determines which stack is used
			int,		% number of stack slots
			maybe(int),	% location of succip on stack
			maybe(label),	% if generate_trace is set,
					% this contains the label associated
					% with the call event, whose stack
					% layout says which variables were
					% live and where on entry
			proc_label_layout_info
					% info for each internal label,
					% needed for basic_stack_layouts
		).

	%
	% Information about the labels internal to a procedure.
	%
:- type proc_label_layout_info	==	map(label, internal_layout_info).

	%
	% Information for any internal label.
	% At some labels, we are interested in the layout of live data;
	% at others, we are not. The layout_label_info will be present
	% only for labels of the first kind.
	%
:- type internal_layout_info	==	maybe(layout_label_info).

	%
	% Information about the layout of live data for a label.
	%
	% Different calls can assign slightly
	% different liveness annotations to the labels after the call.
	% (Two different paths of computation can leave different
	% information live).
	% We take the intersection of these annotations.  Intersecting
	% is easy if we represent the live values and type infos as sets.
	%
:- type layout_label_info
	--->	layout_label_info(
			set(var_info),
				% live vars and their locations/names
			set(pair(tvar, lval))
				% locations of polymorphic type vars
		).

:- type var_info
	--->	var_info(
			lval,		% the location of the variable
			live_value_type,% pseudo-typeinfo giving the var's type
			string		% the var's name
		).

	% Return an initialized continuation info structure.

:- pred continuation_info__init(continuation_info::out) is det.

	%
	% Add the information for a single proc.
	%
	% Takes the pred_proc_id, proc_label, the number of stack slots,
	% the code model for this proc, and the stack slot of the succip
	% in this proc (if there is one).
	%
:- pred continuation_info__add_proc_info(pred_proc_id::in, proc_label::in,
	int::in, determinism::in, maybe(int)::in, maybe(label)::in,
	proc_label_layout_info::in, continuation_info::in,
	continuation_info::out) is det.

	%
	% Call continuation_info__process_instructions on the code
	% of every procedure in the list.
	%
:- pred continuation_info__process_llds(list(c_procedure)::in,
	continuation_info::in, continuation_info::out) is det.

	%
	% Add the information for all the continuation labels within a proc.
	%
:- pred continuation_info__process_instructions(pred_proc_id::in,
	list(instruction)::in, continuation_info::in, continuation_info::out)
	is det.

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
continuation_info__add_proc_info(PredProcId, ProcLabel, StackSize,
		Detism, SuccipLocation, MaybeTraceCallLabel, InternalMap,
		ContInfo0, ContInfo) :-
	( map__contains(ContInfo0, PredProcId) ->
		error("duplicate continuation_info for proc.")
	;
		LayoutInfo = proc_layout_info(ProcLabel, Detism, StackSize,
			SuccipLocation, MaybeTraceCallLabel, InternalMap),
		map__det_insert(ContInfo0, PredProcId, LayoutInfo, ContInfo)
	).

	%
	% Get all the proc_layout_infos.
	%
continuation_info__get_all_proc_layouts(ContInfo, Entries) :-
	map__values(ContInfo, Entries).

continuation_info__process_llds([]) --> [].
continuation_info__process_llds([Proc | Procs]) -->
	{ Proc = c_procedure(_, _, PredProcId, Instrs) },
	continuation_info__process_instructions(PredProcId, Instrs),
	continuation_info__process_llds(Procs).

	%
	% Process the list of instructions for this proc, adding
	% all internal label information to the continuation_info.
	%
continuation_info__process_instructions(PredProcId, Instructions,
		ContInfo0, ContInfo) :-

		% Get all the continuation info from the call instructions.
	map__lookup(ContInfo0, PredProcId, ProcLayoutInfo0),
	ProcLayoutInfo0 = proc_layout_info(A, B, C, D, E, Internals0),
	GetCallLivevals = lambda([Instr::in, Pair::out] is semidet, (
		Instr = call(_, label(Label), LiveInfo, _) - _Comment,
		Pair = Label - LiveInfo
	)),
	list__filter_map(GetCallLivevals, Instructions, Calls),

		% Process the continuation label info.
	list__foldl(continuation_info__process_continuation, Calls,
		Internals0, Internals),

	ProcLayoutInfo = proc_layout_info(A, B, C, D, E, Internals),
	map__det_update(ContInfo0, PredProcId, ProcLayoutInfo, ContInfo).

%-----------------------------------------------------------------------------%

	%
	% Collect the liveness information from a single label and add
	% it to the internals.
	%
:- pred continuation_info__process_continuation(
	pair(label, list(liveinfo))::in,
	proc_label_layout_info::in, proc_label_layout_info::out) is det.

continuation_info__process_continuation(Label - LiveInfoList,
		Internals0, Internals) :-
	GetVarInfo = lambda([LiveLval::in, VarInfo::out] is det, (
		LiveLval = live_lvalue(Lval, LiveValueType, Name, _),
		VarInfo = var_info(Lval, LiveValueType, Name)
	)),
	list__map(GetVarInfo, LiveInfoList, VarInfoList),
	GetTypeInfo = lambda([LiveLval::in, TypeInfos::out] is det, (
		LiveLval = live_lvalue(_, _, _, TypeInfos)
	)),
	list__map(GetTypeInfo, LiveInfoList, TypeInfoListList),
	list__condense(TypeInfoListList, TypeInfoList),
	list__sort_and_remove_dups(TypeInfoList, SortedTypeInfoList),
	set__sorted_list_to_set(SortedTypeInfoList, TypeInfoSet),
	set__list_to_set(VarInfoList, VarInfoSet),
	NewInternal = yes(layout_label_info(VarInfoSet, TypeInfoSet)),
	continuation_info__add_internal_info(Label, NewInternal,
		Internals0, Internals).

:- pred continuation_info__ensure_label_is_present(label::in,
	proc_label_layout_info::in, proc_label_layout_info::out) is det.

continuation_info__ensure_label_is_present(Label, InternalMap0, InternalMap) :-
	( map__contains(InternalMap0, Label) ->
		InternalMap = InternalMap0
	;
		map__det_insert(InternalMap0, Label, no, InternalMap)
	).

%-----------------------------------------------------------------------------%

	%
	% Add an internal info to the list of internal infos.
	%
:- pred continuation_info__add_internal_info(label::in,
	internal_layout_info::in,
	proc_label_layout_info::in, proc_label_layout_info::out) is det.

continuation_info__add_internal_info(Label, Internal1,
		Internals0, Internals) :-
	(
		map__search(Internals0, Label, Internal0)
	->
		continuation_info__merge_internal_labels(Internal0, Internal1,
			Internal),
		map__set(Internals0, Label, Internal, Internals)
	;
		map__det_insert(Internals0, Label, Internal1, Internals)
	).

	%
	% Merge the continuation label information of two labels.
	%
	% If there are two continuation infos to be merged, we take
	% the intersection.
	%
	% The reason why taking the intersection is correct is that if
	% something is not live on one path, the code following the
	% label is guaranteed not to depend on it.
	% XXX Is this true for non-det code?

:- pred continuation_info__merge_internal_labels(
	maybe(layout_label_info)::in, maybe(layout_label_info)::in,
	maybe(layout_label_info)::out) is det.

continuation_info__merge_internal_labels(no, no, no).
continuation_info__merge_internal_labels(no,
		yes(layout_label_info(LV0, TV0)),
		yes(layout_label_info(LV0, TV0))).
continuation_info__merge_internal_labels(
		yes(layout_label_info(LV0, TV0)),
		no,
		yes(layout_label_info(LV0, TV0))).
continuation_info__merge_internal_labels(
		yes(layout_label_info(LV0, TV0)),
		yes(layout_label_info(LV1, TV1)),
		yes(layout_label_info(LV, TV))) :-
	set__intersect(LV0, LV1, LV),
	set__intersect(TV0, TV1, TV).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
