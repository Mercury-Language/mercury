%-----------------------------------------------------------------------------%
% Copyright (C) 1997 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: continuation_info.m.
% Main author: trd.

% This file defines the continuation_info data structure, which is used
% to hold the information we need to output stack_layout tables for
% accurate garbage collection.

% XXX What about redirected labels? Since we just traverse the call
% instructions looking for return addresses, it's possible that we will 
% store the information about a code address that actually occurs
% outside the procedure.
% We might want to check that code addresses are inside the procedure,
% and keep seperate any that aren't until we can find out where they
% really belong. Then again, maybe they aren't needed at all, since this
% information is likely to be the same elsewhere.
% Check this out - it's likely that tredirections aren't introduced
% until optimization.

%-----------------------------------------------------------------------------%

:- module continuation_info.

:- interface.

:- import_module list, llds.

:- type continuation_info.

	% Return an initialized continuation info structure.

:- pred continuation_info__init(continuation_info).
:- mode continuation_info__init(out) is det.

	% Add the information for all the continuations within a 
	% proc. Takes the list of instructions for this proc, the
	% proc_label, the number of stack slots, the code model for this
	% proc, and the stack slot of the succip in this proc.

:- pred continuation_info__add_proc_info(list(instruction), proc_label,
		int, code_model, int, continuation_info, continuation_info).
:- mode continuation_info__add_proc_info(in, in, in, in, in, in, out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module llds, prog_data.
:- import_module map, list, assoc_list, std_util, term.

	% The continuation_info data structure 

:- type continuation_info
	--->	continuation_info(
			map(proc_label, entry_layout_info),
				% final data table 
			list(internal_layout_info)
				% internal labels processed so far
			).

:- type entry_layout_info
	--->
		entry_layout_info(
			int,		% number of stack slots
			code_model,	% which stack is used
			int,		% location of succip on stack
			list(internal_layout_info) 
					% info for each internal label
		).

:- type internal_layout_info
	--->
		internal_layout_info(
			code_addr,	% what label is this associated with
			assoc_list(lval, live_value_type),
			assoc_list(tvar, lval)
				% locations of polymorphic type vars
		).
					

:- type type_param_locs == assoc_list(var, lval).

%-----------------------------------------------------------------------------%

continuation_info__add_proc_info(Instructions, ProcLabel, StackSize, CodeModel,
	SuccipLocation, ContInfo0, ContInfo) :-
		
	continuation_info__set_internal_info([], ContInfo0, ContInfo1),

		% Get all the info for the internal labels.
	GetCallLivevals = lambda([Instr::in, Pair::out] is semidet, (
		Instr = call(_, Continuation, LiveInfo, _) - _Comment,
		Pair = Continuation - LiveInfo
		)),
	list__filter_map(GetCallLivevals, Instructions, Calls),
	list__foldl(continuation_info__process_internal_info, 
		Calls, ContInfo1, ContInfo2),

	continuation_info__get_internal_info(ContInfo2, InternalInfo),

	EntryLayout = entry_layout_info(StackSize, CodeModel,
		SuccipLocation, InternalInfo),

	continuation_info__add_proc_layout(ProcLabel, EntryLayout,
		ContInfo2, ContInfo).

	% Process a single continuation label.

:- pred continuation_info__process_internal_info(pair(code_addr,
		list(liveinfo)), continuation_info, continuation_info).
:- mode continuation_info__process_internal_info(in, in, out) is det.

continuation_info__process_internal_info(CodeAddr - LiveInfoList, ContInfo0, 
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
	NewInternal = internal_layout_info(CodeAddr, LvalPairList, 
		SortedTypeInfoList), 
	continuation_info__add_internal_info(NewInternal, ContInfo0, 
		ContInfo).

%-----------------------------------------------------------------------------%

	% Initialize the continuation_info

continuation_info__init(ContInfo) :-
	map__init(LabelMap),
	ContInfo = continuation_info(LabelMap, []).

:- pred continuation_info__add_proc_layout(proc_label, entry_layout_info, 
		continuation_info, continuation_info).
:- mode continuation_info__add_proc_layout(in, in, in, out) is det.

continuation_info__add_proc_layout(ProcLabel, EntryLayout, 
		ContInfo0, ContInfo) :-
	ContInfo0 = continuation_info(ProcLayoutMap0, B),
	map__det_insert(ProcLayoutMap0, ProcLabel, EntryLayout, ProcLayoutMap),
	ContInfo = continuation_info(ProcLayoutMap, B).

:- pred continuation_info__add_internal_info(internal_layout_info,
		continuation_info, continuation_info).
:- mode continuation_info__add_internal_info(in, in, out) is det.

continuation_info__add_internal_info(Internal, ContInfo0, ContInfo) :-
	ContInfo0 = continuation_info(A, Internals),
	ContInfo = continuation_info(A, [ Internal | Internals ]).

:- pred continuation_info__set_internal_info(list(internal_layout_info),
		continuation_info, continuation_info).
:- mode continuation_info__set_internal_info(in, in, out) is det.

continuation_info__set_internal_info(Internals, ContInfo0, ContInfo) :-
	ContInfo0 = continuation_info(A, _),
	ContInfo = continuation_info(A, Internals).

:- pred continuation_info__get_internal_info(continuation_info, 
		list(internal_layout_info)).
:- mode continuation_info__get_internal_info(in, out) is det.

continuation_info__get_internal_info(ContInfo, Internals) :-
	ContInfo = continuation_info(_, Internals).


%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
