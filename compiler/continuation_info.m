%-----------------------------------------------------------------------------%
% Copyright (C) 1997-1998 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: continuation_info.m.
% Main author: trd.

% This file defines the continuation_info data structure, which is used
% to hold the information we need to output stack_layout tables for
% accurate garbage collection.
%
% Information is collected in two passes. 
% 	1. After the code for a procedure has been generated, a
% 	   proc_layout_info is added to the continuation info (using
% 	   continuation_info__add_proc_layout_info). 
% 	2. After code has been optimized, a pass is made over the
% 	   final LLDS instructions. Information about internal labels,
% 	   is collected. The liveness information in call instructions
% 	   is stored with the corresponding continuation label.
%
% stack_layout.m converts the information collected in this module into
% stack_layout tables.

%-----------------------------------------------------------------------------%

:- module continuation_info.

:- interface.

:- import_module llds, hlds_pred.
:- import_module list.

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
	--->
		proc_layout_info(
			proc_label,	% the proc label
			int,		% number of stack slots
			code_model,	% which stack is used
			maybe(int),	% location of succip on stack
			map(label, internal_layout_info)
					% info for each internal label
		).

	%
	% Information for any internal label.
	% (Continuation labels are a special case of internal labels).
	%
:- type internal_layout_info
	--->
		internal_layout_info(
			maybe(continuation_label_info)
		).

	%
	% Information for a label that is a continuation.
	%
	% Different calls can assign slightly
	% different liveness annotations to the labels after the call.
	% (Two different paths of computation can leave different
	% information live).
	% We take the intersection of these annotations.  Intersecting
	% is easy if we represent the live values and type infos as
	% sets.
:- type continuation_label_info
	--->
		continuation_label_info(
			set(pair(lval, live_value_type)),
					% live values and their
					% locations
			set(pair(tvar, lval))
				% locations of polymorphic type vars
		).


	% Return an initialized continuation info structure.

:- pred continuation_info__init(continuation_info).
:- mode continuation_info__init(out) is det.

	%
	% Add the information for a single proc.
	%
	% Takes the pred_proc_id, proc_label, the number of stack slots,
	% the code model for this proc, and the stack slot of the succip
	% in this proc (if there is one).
	%
:- pred continuation_info__add_proc_layout_info(pred_proc_id, proc_label,
		int, code_model, maybe(int), continuation_info,
		continuation_info).
:- mode continuation_info__add_proc_layout_info(in, in, in, in, in, in,
		out) is det.


:- pred continuation_info__process_llds(list(c_procedure),
		continuation_info, continuation_info) is det.
:- mode continuation_info__process_llds(in, in, out) is det.


	%
	% Add the information for all the labels within a
	% proc.
	%
	% Takes the list of instructions for this proc, the
	% proc_label, the number of stack slots, the code model for this
	% proc, and the stack slot of the succip in this proc.
	%
:- pred continuation_info__process_instructions(pred_proc_id,
	list(instruction), continuation_info, continuation_info).
:- mode continuation_info__process_instructions(in, in, in, out) is det.


	%
	% Get the finished list of proc_layout_infos.
	%
:- pred continuation_info__get_all_proc_layouts(list(proc_layout_info),
		continuation_info, continuation_info).
:- mode continuation_info__get_all_proc_layouts(out, in, out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module llds, prog_data.
:- import_module map, list, assoc_list, std_util, term, set, require.

	% The continuation_info data structure

:- type continuation_info
	--->	continuation_info(
			map(pred_proc_id, proc_layout_info),
				% A proc_layout_info for every procedure
				% processed
			map(label, internal_layout_info)
				% internal labels processed so far
				% in the current procedure
			).


%-----------------------------------------------------------------------------%

	% Exported predicates.

	%
	% Initialize the continuation_info
	%

continuation_info__init(ContInfo) :-
	map__init(LabelMap),
	map__init(Internals),
	ContInfo = continuation_info(LabelMap, Internals).

continuation_info__process_llds([]) --> [].
continuation_info__process_llds([Proc|Procs]) -->
	{ Proc = c_procedure(_, _, PredProcId, Instrs) },
	continuation_info__process_instructions(PredProcId, Instrs),
	continuation_info__process_llds(Procs).

	%
	% Process the list of instructions for this proc, adding
	% all internal label information to the continuation_info.
	%
continuation_info__process_instructions(PredProcId, Instructions) -->

		% Get all the continuation info from the call instructions
	continuation_info__initialize_internal_info,
	{ GetCallLivevals = lambda([Instr::in, Pair::out] is semidet, (
		Instr = call(_, label(Label), LiveInfo, _) - _Comment,
		Pair = Label - LiveInfo
		)) },
	{ list__filter_map(GetCallLivevals, Instructions, Calls) },

		% Process the continuation label info
	list__foldl(continuation_info__process_internal_info,
		Calls),

		% Get all internal labels.
		% (Some labels are not used as continuations).
	{ GetAllInternalLabels = lambda([Instr::in, Label::out] is semidet, (
		Instr = label(Label) - _Comment,
		Label = local(_, _)
		)) },
	{ list__filter_map(GetAllInternalLabels, Instructions, Labels) },

		% Insert all non-continuation internal labels into the
		% internals, then add the internals to the information
		% for this proc.
	continuation_info__add_non_continuation_labels(Labels),
	continuation_info__get_internal_info(InternalInfo),
	continuation_info__add_internal_info_to_proc(PredProcId, InternalInfo).


	%
	% Add the info for this proc (a proc_layout_info) to the
	% continuation_info. 
	%
continuation_info__add_proc_layout_info(PredProcId, ProcLabel, StackSize,
		CodeModel, SuccipLocation, ContInfo0, ContInfo) :-

		% We don't know anything about the internals yet.
	map__init(InternalMap),
	ProcLayoutInfo = proc_layout_info(ProcLabel, StackSize, CodeModel,
		SuccipLocation, InternalMap),
	continuation_info__insert_proc_layout(PredProcId, ProcLayoutInfo,
		ContInfo0, ContInfo).

	%
	% Get all the proc_layout_infos.
	%
continuation_info__get_all_proc_layouts(Entries, ContInfo, ContInfo) :-
	ContInfo = continuation_info(Map, _),
	map__values(Map, Entries).

%-----------------------------------------------------------------------------%

	%
	% Add the list of internal labels to the internal_info
	% in the continuation_info.
	%
:- pred continuation_info__add_non_continuation_labels(list(label),
		continuation_info, continuation_info).
:- mode continuation_info__add_non_continuation_labels(in, in, out) is det.

continuation_info__add_non_continuation_labels(Labels) -->
	continuation_info__get_internal_info(InternalInfo0),
	{ list__foldl(continuation_info__ensure_label_is_present, Labels,
		InternalInfo0, InternalInfo) },
	continuation_info__set_internal_info(InternalInfo).


	%
	% Add a label to the internals, if it isn't already there.
	%
:- pred continuation_info__ensure_label_is_present(label,
		map(label, internal_layout_info),
		map(label, internal_layout_info)).
:- mode continuation_info__ensure_label_is_present(in, in, out) is det.
continuation_info__ensure_label_is_present(Label, InternalMap0, InternalMap) :-
	( map__contains(InternalMap0, Label) ->
		InternalMap = InternalMap0
	;
		Internal = internal_layout_info(no),
		map__det_insert(InternalMap0, Label,
			Internal, InternalMap)
	).

	%
	% Collect the liveness information from a single label and add
	% it to the internals.
	%
:- pred continuation_info__process_internal_info(pair(label,
		list(liveinfo)), continuation_info, continuation_info).
:- mode continuation_info__process_internal_info(in, in, out) is det.

continuation_info__process_internal_info(Label - LiveInfoList, ContInfo0,
		ContInfo) :-
	GetTypeInfo = lambda([LiveLval::in, TypeInfos::out] is det, (
		LiveLval = live_lvalue(_, _, TypeInfos)
		)),
	GetLvalPair = lambda([LiveLval::in, LvalPair::out] is det, (
		LiveLval = live_lvalue(Lval, LiveValueType, _),
		LvalPair = Lval - LiveValueType
		)),
	list__map(GetLvalPair, LiveInfoList, LvalPairList),
	list__map(GetTypeInfo, LiveInfoList, TypeInfoListList),
	list__condense(TypeInfoListList, TypeInfoList),
	list__sort_and_remove_dups(TypeInfoList, SortedTypeInfoList),
	set__sorted_list_to_set(SortedTypeInfoList, TypeInfoSet),
	set__list_to_set(LvalPairList, LvalPairSet),
	NewInternal = internal_layout_info(
		yes(continuation_label_info(LvalPairSet, TypeInfoSet))),
	continuation_info__add_internal_info(Label, NewInternal, ContInfo0,
		ContInfo).

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

:- pred continuation_info__merge_internal_labels(maybe(continuation_label_info),
	maybe(continuation_label_info), maybe(continuation_label_info)).
:- mode continuation_info__merge_internal_labels(in, in, out) is det.

continuation_info__merge_internal_labels(no, no, no).
continuation_info__merge_internal_labels(no,
		yes(continuation_label_info(LV0, TV0)),
		yes(continuation_label_info(LV0, TV0))).
continuation_info__merge_internal_labels(
		yes(continuation_label_info(LV0, TV0)),
		no,
		yes(continuation_label_info(LV0, TV0))).
continuation_info__merge_internal_labels(
		yes(continuation_label_info(LV0, TV0)),
		yes(continuation_label_info(LV1, TV1)),
		yes(continuation_label_info(LV, TV))) :-
	set__intersect(LV0, LV1, LV),
	set__intersect(TV0, TV1, TV).

%-----------------------------------------------------------------------------%

	% Procedures to manipulate continuation_info


	%
	% Add the given proc_layout_info to the continuation_info.
	%
:- pred continuation_info__insert_proc_layout(pred_proc_id, proc_layout_info,
		continuation_info, continuation_info).
:- mode continuation_info__insert_proc_layout(in, in, in, out) is det.

continuation_info__insert_proc_layout(PredProcId, ProcLayoutInfo,
		ContInfo0, ContInfo) :-
	ContInfo0 = continuation_info(ProcLayoutMap0, Internals),
	map__det_insert(ProcLayoutMap0, PredProcId, ProcLayoutInfo,
		ProcLayoutMap),
	ContInfo = continuation_info(ProcLayoutMap, Internals).

	%
	% Add the given internal_info to the given procedure in
	% the continuation_info.
	%
	% (The procedure proc_layout_info has already been processed and
	% added, but at that time the internal_info wasn't available).
	%
:- pred continuation_info__add_internal_info_to_proc(pred_proc_id,
		map(label, internal_layout_info), continuation_info,
		continuation_info).
:- mode continuation_info__add_internal_info_to_proc(in, in, in, out) is det.

continuation_info__add_internal_info_to_proc(PredProcId, InternalLayout,
		ContInfo0, ContInfo) :-
	ContInfo0 = continuation_info(ProcLayoutMap0, Internals),
	map__lookup(ProcLayoutMap0, PredProcId, ProcLayoutInfo0),
	ProcLayoutInfo0 = proc_layout_info(ProcLabel, StackSize, CodeModel,
		SuccipLocation, _),
	ProcLayoutInfo = proc_layout_info(ProcLabel, StackSize, CodeModel,
		SuccipLocation, InternalLayout),
	map__set(ProcLayoutMap0, PredProcId, ProcLayoutInfo, ProcLayoutMap),
	ContInfo = continuation_info(ProcLayoutMap, Internals).

	%
	% Add an internal info to the list of internal infos.
	%
:- pred continuation_info__add_internal_info(label,
		internal_layout_info, continuation_info, continuation_info).
:- mode continuation_info__add_internal_info(in, in, in, out) is det.

continuation_info__add_internal_info(Label, Internal, ContInfo0, ContInfo) :-
	ContInfo0 = continuation_info(ProcLayoutMap, Internals0),
	Internal = internal_layout_info(ContLabelInfo0),
	(
		map__search(Internals0, Label, Existing)
	->
		Existing = internal_layout_info(ContLabelInfo1),
		continuation_info__merge_internal_labels(ContLabelInfo0,
			ContLabelInfo1, ContLabelInfo),
		New = internal_layout_info(ContLabelInfo),
		map__set(Internals0, Label, New, Internals)
		
	;
		map__det_insert(Internals0, Label, Internal, Internals)
	),
	ContInfo = continuation_info(ProcLayoutMap, Internals).

	%
	% Initialize the internal info.
	%
:- pred continuation_info__initialize_internal_info(
	continuation_info, continuation_info).
:- mode continuation_info__initialize_internal_info(in, out) is det.

continuation_info__initialize_internal_info(ContInfo0, ContInfo) :-
	ContInfo0 = continuation_info(ProcLayoutMap, _),
	map__init(Internals),
	ContInfo = continuation_info(ProcLayoutMap, Internals).

	%
	% Set the internal info.
	%
:- pred continuation_info__set_internal_info(
	map(label, internal_layout_info), continuation_info,
	continuation_info).
:- mode continuation_info__set_internal_info(in, in, out) is det.

continuation_info__set_internal_info(Internals, ContInfo0, ContInfo) :-
	ContInfo0 = continuation_info(ProcLayoutMap, _),
	ContInfo = continuation_info(ProcLayoutMap, Internals).

	%
	% Get the internal_info.
	%
:- pred continuation_info__get_internal_info(
		map(label, internal_layout_info),
		continuation_info, continuation_info).
:- mode continuation_info__get_internal_info(out, in, out) is det.

continuation_info__get_internal_info(InternalMap, ContInfo, ContInfo) :-
	ContInfo = continuation_info(_, InternalMap).


%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
