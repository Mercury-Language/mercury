%---------------------------------------------------------------------------%
% Copyright (C) 1997-1998 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This module generates the LLDS code that defines global constants to
% hold the `stack_layout' structures of the stack frames defined by the
% current module.
%
% The tables generated have a number of `create' rvals within them,
% these are removed by llds_common.m to create static structures.
%
% Author: trd.
%
%---------------------------------------------------------------------------%
%
% Data Stucture: stack_layouts
%
% For each procedure,
% 	mercury_data__stack_layout__mercury__<proc_label>
% containing:
%	code address		(Code *) - address of entry
% 	number of stack slots	(Integer) 
% 	code_model		(Integer) actually, type MR_Code_Model 
% 					0 = DET, 1 = NONDET
% 	succip stack location	(Integer) actually, type MR_Live_Lval
% 					(the location will be set to -1
% 					if there is no succip available).
%
% If we are doing execution tracing, it will also include information
% on the live data at entry and exit:
% 	number of live 		(Integer)
% 	    variables at entry 
% 	live data locations and (Word *) - pointer to vector of
% 	    types			MR_Live_Lval and MR_Live_Type pairs
%	type parameters		(Word *) - pointer to vector of 
%					MR_Live_Lval
%
% 	number of live 		(Integer)
% 	    variables at exit 
% 	live data locations and (Word *) - pointer to vector of
% 	    types			MR_Live_Lval and MR_Live_Type pairs
%	type parameters		(Word *) - pointer to vector of 
%					MR_Live_Lval
%
% For each continuation label in a procedure
% 	mercury_data__stack_layout__mercury__<proc_label>_i<label number>
% containing:
%	procedure info		(Word *) - pointer to procedure stack layout
%	number of live vars	(Integer)
%	live data locations and (Word *) - pointer to vector of 
%		types			MR_Live_Lval and MR_Live_Type pairs
%	type parameters		(Word *) - pointer to vector of 
%					MR_Live_Lval
%
% If the number of live vars is 0, there could be two explanations. The
% continuation label might actually have no live data, or (more likely)
% it isn't a continuation label at all.
%
% If you need to know the live variables at non-continuation labels,
% this code will not be sufficient. In particular, it is expected that
% information about live variables at entry and exit points will be
% added.
%
% Note: That number of type parameters is stored as it is not needed --
% the type parameter vector will simply be indexed by the type parameter
% number stored within pseudo-typeinfos. 
%
% XXX: Presently, type parameter vectors are not created, and
% inst information is ignored.
%
%---------------------------------------------------------------------------%

:- module stack_layout.

:- interface.

:- import_module hlds_module.

:- pred stack_layout__generate_llds(module_info, module_info, list(c_module)).
:- mode stack_layout__generate_llds(in, out, out) is det.

:- implementation.

:- import_module llds, globals, options, continuation_info, llds_out.
:- import_module base_type_layout, prog_data.
:- import_module assoc_list, bool, string, int, list, map, std_util, require.
:- import_module set.

:- type stack_layout_info 	--->	
	stack_layout_info(
		string,		% module name
		int,		% next available cell number 
		bool,		% generate agc layout info?
		bool,		% generate tracing layout info?
		list(c_module)	% generated data
	).


%---------------------------------------------------------------------------%

	% Initialize the StackLayoutInfo, and begin processing.
stack_layout__generate_llds(ModuleInfo0, ModuleInfo, CModules) :-
	module_info_get_continuation_info(ModuleInfo0, ContinuationInfo),
	continuation_info__get_all_proc_layouts(ProcLayoutList, 
		ContinuationInfo, _),

	module_info_name(ModuleInfo0, ModuleName),
	module_info_get_cell_count(ModuleInfo0, CellCount),
	module_info_globals(ModuleInfo0, Globals),
	globals__lookup_bool_option(Globals, agc_stack_layout, AgcLayout),
	globals__lookup_bool_option(Globals, trace_stack_layout, TraceLayout),

	LayoutInfo0 = stack_layout_info(ModuleName, CellCount, AgcLayout,
		TraceLayout, []),
	list__foldl(stack_layout__construct_layouts, ProcLayoutList,
		LayoutInfo0, LayoutInfo),

	stack_layout__get_cmodules(CModules, LayoutInfo, _),
	stack_layout__get_cell_number(FinalCellCount, LayoutInfo, _),
	module_info_set_cell_count(ModuleInfo0, FinalCellCount, ModuleInfo).

%---------------------------------------------------------------------------%

	% Construct the layouts for a single procedure.
	
:- pred stack_layout__construct_layouts(proc_layout_info, 
		stack_layout_info, stack_layout_info).
:- mode stack_layout__construct_layouts(in, in, out) is det.

stack_layout__construct_layouts(ProcLayoutInfo) -->

	{ ProcLayoutInfo = proc_layout_info(MaybeGeneralInfo, InternalMap, 
		EntryInfo, ExitInfo) },

	( { MaybeGeneralInfo = yes(GeneralInfo) } ->
		stack_layout__construct_proc_layout(GeneralInfo, EntryInfo,
			ExitInfo),
		{ GeneralInfo = proc_layout_general_info(ProcLabel, _, _, _) },
		{ map__to_assoc_list(InternalMap, Internals) },
		list__foldl(stack_layout__construct_internal_layout(ProcLabel),
			Internals)
	;
		{ error("stack_layout__construct_layouts: uninitialized proc layout") }
	).


%---------------------------------------------------------------------------%

	% Construct the layout describing a single procedure.

:- pred stack_layout__construct_proc_layout(proc_layout_general_info,
		maybe(continuation_label_info), maybe(continuation_label_info),
		stack_layout_info, stack_layout_info).
:- mode stack_layout__construct_proc_layout(in, in, in, in, out) is det.
stack_layout__construct_proc_layout(GeneralInfo, MaybeEntryInfo,
		MaybeExitInfo) -->
	{ GeneralInfo = proc_layout_general_info(ProcLabel, StackSlots, 
		CodeModel, SuccipLoc) },
	{
		SuccipLoc = yes(Location0)
	->
		Location = Location0
	;
			% Use a dummy location of -1 if there is
			% no succip on the stack. The runtime system
			% might be able to work around this, depending
			% upon what it is using the stack layouts for.
		Location = -1
	},
	{
		CodeModel = model_det,
		SuccipLval = stackvar(Location)
	;
		CodeModel = model_semi,
		SuccipLval = stackvar(Location)
	;
		CodeModel = model_non,
		SuccipLval = framevar(Location)
	},
	{ Label = local(ProcLabel) },
	{ stack_layout__represent_lval(SuccipLval, SuccipRval) },
	{ StackSlotsRval = const(int_const(StackSlots)) },
	{ CodeAddrRval = const(code_addr_const(label(Label))) },

	stack_layout__represent_code_model(CodeModel, CodeModelRval),
	{ MaybeRvals0 = [yes(CodeAddrRval), yes(StackSlotsRval), 
		yes(CodeModelRval), yes(SuccipRval)] },
	stack_layout__get_module_name(ModuleName),

	stack_layout__get_trace_stack_layout(TraceLayout),
	(
		{ TraceLayout = yes }
	->
		stack_layout__construct_trace_rvals(MaybeEntryInfo,
			MaybeExitInfo, TraceRvals)
	;
		{ TraceRvals = [] }
	),

	{ list__append(MaybeRvals0, TraceRvals, MaybeRvals) },

	{ CModule = c_data(ModuleName, stack_layout(Label), yes, 
		MaybeRvals, []) },
	stack_layout__add_cmodule(CModule).

%---------------------------------------------------------------------------%

	% Construct the layout describing a single continuation label.

:- pred stack_layout__construct_internal_layout(proc_label,
		pair(label, internal_layout_info), 
		stack_layout_info, stack_layout_info).
:- mode stack_layout__construct_internal_layout(in, in, in, out) is det.
stack_layout__construct_internal_layout(ProcLabel, Label - Internal) -->
		% generate the required rvals
	stack_layout__get_module_name(ModuleName),
	{ EntryAddrRval = const(data_addr_const(data_addr(ModuleName,
		stack_layout(local(ProcLabel))))) },

	stack_layout__construct_agc_rvals(Internal, AgcRvals),

	{ LayoutRvals = [yes(EntryAddrRval) | AgcRvals] }, 

	{ CModule = c_data(ModuleName, stack_layout(Label), yes, 
		LayoutRvals, []) },
	stack_layout__add_cmodule(CModule).

	
	% Construct the rvals required for tracing.

:- pred stack_layout__construct_trace_rvals(maybe(continuation_label_info), 
		maybe(continuation_label_info), list(maybe(rval)),
		stack_layout_info, stack_layout_info).
:- mode stack_layout__construct_trace_rvals(in, in, out, in, out) is det.
stack_layout__construct_trace_rvals(MaybeEntryInfo, MaybeExitInfo,
		RvalList) -->
	( 
		{ MaybeEntryInfo = yes(EntryInfo) },
		{ MaybeExitInfo = yes(ExitInfo) }
	->
		{ EntryInfo = continuation_label_info(EntryLvals, EntryTVars) },
		{ ExitInfo = continuation_label_info(ExitLvals, ExitTVars) },
		stack_layout__construct_livelval_rvals(EntryLvals, EntryTVars,
			EntryRvals),
		stack_layout__construct_livelval_rvals(ExitLvals, ExitTVars,
			ExitRvals),
		{ list__append(EntryRvals, ExitRvals, RvalList) }
	;
		{ error("stack_layout__construct_agc_rvals: entry or exit information not available.") }
	).


	% Construct the rvals required for accurate GC.

:- pred stack_layout__construct_agc_rvals(internal_layout_info, 
		list(maybe(rval)), stack_layout_info, stack_layout_info).
:- mode stack_layout__construct_agc_rvals(in, out, in, out) is det.
stack_layout__construct_agc_rvals(Internal, RvalList) -->
	stack_layout__get_agc_stack_layout(AgcStackLayout),
	( 
		{ AgcStackLayout = yes }
	->
		{ Internal = internal_layout_info(ContinuationLabelInfo) },
		{
			ContinuationLabelInfo = yes(continuation_label_info(
				LiveLvalSet0, TVars0))
		->
			LiveLvalSet = LiveLvalSet0,
			TVars = TVars0
		;
			% This label is not being used as a continuation,
			% or we are not doing accurate GC, so we record
			% no live values here. 
			% This might not be a true reflection of the
			% liveness at this point, so the values cannot
			% be relied upon by the runtime system unless
			% you know you are at a continuation (and doing
			% accurate GC).
			
			set__init(LiveLvalSet),
			set__init(TVars)
		},
		stack_layout__construct_livelval_rvals(LiveLvalSet, TVars,
			RvalList)
	;
		{ RvalList = [yes(const(int_const(0))), 
			yes(const(int_const(0)))] }
	).


	% XXX Should also create Tvars.

:- pred stack_layout__construct_livelval_rvals(set(pair(lval, live_value_type)),
		set(pair(tvar, lval)), list(maybe(rval)), 
		stack_layout_info, stack_layout_info).
:- mode stack_layout__construct_livelval_rvals(in, in, out, in, out) is det.
stack_layout__construct_livelval_rvals(LiveLvalSet, _TVars, RvalList) -->
	{ set__to_sorted_list(LiveLvalSet, LiveLvals) },
	{ list__length(LiveLvals, Length) },
	{ LengthRval = const(int_const(Length)) },
	stack_layout__construct_liveval_pairs(LiveLvals, LiveValRval),

		% XXX We don't yet generate tvars, so we use 0 as
		% a dummy value.
	{ TVarRval = const(int_const(0)) },
	{ RvalList = [yes(LengthRval), yes(LiveValRval), yes(TVarRval)] }.



%---------------------------------------------------------------------------%

	% Construct a vector of (lval, live_value_type) pairs.

:- pred stack_layout__construct_liveval_pairs(assoc_list(lval, live_value_type),
		rval, stack_layout_info, stack_layout_info).
:- mode stack_layout__construct_liveval_pairs(in, out, in, out) is det.

stack_layout__construct_liveval_pairs(LiveLvals, Rval) -->
	list__map_foldl(stack_layout__construct_liveval_pair, LiveLvals, 
		RvalsList),
	{ list__condense(RvalsList, Rvals) },
	stack_layout__get_next_cell_number(CNum),
	{ Rval = create(0, Rvals, no, CNum, "stack_layout_pair") }.

	% Construct a pair of (lval, live_value_type) representations.

:- pred stack_layout__construct_liveval_pair(pair(lval, live_value_type),
		list(maybe(rval)), stack_layout_info, stack_layout_info).
:- mode stack_layout__construct_liveval_pair(in, out, in, out) is det.

stack_layout__construct_liveval_pair(Lval - LiveValueType, Rvals) -->
	{ stack_layout__represent_lval(Lval, Rval0) },
	stack_layout__represent_live_value_type(LiveValueType, Rval1),
	{ Rvals = [yes(Rval0), yes(Rval1)] }.

%---------------------------------------------------------------------------%

	% The constants here should be kept in sync with constants in
	% the runtime system:
	% 	mercury_accurate_gc.h - contains macros to access these
	%			 	constants.

	% Construct a representation of a live_value_type.
	%
	% Low integers for special values, a pointer for other values.
	% (Remember to keep the low integers below the max varint value in
	% runtime/type_info.h).

:- pred stack_layout__represent_live_value_type(live_value_type, rval,
	stack_layout_info, stack_layout_info).
:- mode stack_layout__represent_live_value_type(in, out, in, out) is det. 

stack_layout__represent_live_value_type(succip, Rval) -->
	{ Rval = const(int_const(0)) }.
stack_layout__represent_live_value_type(hp, Rval) -->
	{ Rval = const(int_const(1)) }.
stack_layout__represent_live_value_type(curfr, Rval) -->
	{ Rval = const(int_const(2)) }.
stack_layout__represent_live_value_type(maxfr, Rval) -->
	{ Rval = const(int_const(3)) }.
stack_layout__represent_live_value_type(redoip, Rval) -->
	{ Rval = const(int_const(4)) }.
stack_layout__represent_live_value_type(unwanted, Rval) -->
	{ Rval = const(int_const(5)) }.
stack_layout__represent_live_value_type(var(Type, _Inst), Rval) -->
	stack_layout__get_cell_number(CNum0),
	{ base_type_layout__construct_pseudo_type_info(Type, Rval0, CNum0,
		CNum) },
	stack_layout__set_cell_number(CNum),
		% XXX hack - don't yet write out insts
	{ Rval1 = const(int_const(-1)) },
	stack_layout__get_next_cell_number(CNum2),
	{ Rval = create(0, [yes(Rval0), yes(Rval1)], no, CNum2,
		"stack_layout_pair") }.

	% Construct a representation of an lval.

:- pred stack_layout__represent_lval(lval, rval).
:- mode stack_layout__represent_lval(in, out) is det.

stack_layout__represent_lval(reg(r, Num), Rval) :-
	stack_layout__make_tagged_rval(0, Num, Rval).
stack_layout__represent_lval(reg(f, Num), Rval) :-
	stack_layout__make_tagged_rval(1, Num, Rval).

stack_layout__represent_lval(stackvar(Num), Rval) :-
	stack_layout__make_tagged_rval(2, Num, Rval).
stack_layout__represent_lval(framevar(Num), Rval) :-
	stack_layout__make_tagged_rval(3, Num, Rval).

stack_layout__represent_lval(succip, Rval) :-
	stack_layout__make_tagged_rval(4, 0, Rval).
stack_layout__represent_lval(maxfr, Rval) :-
	stack_layout__make_tagged_rval(5, 0, Rval).
stack_layout__represent_lval(curfr, Rval) :-
	stack_layout__make_tagged_rval(6, 0, Rval).
stack_layout__represent_lval(hp, Rval) :-
	stack_layout__make_tagged_rval(7, 0, Rval).
stack_layout__represent_lval(sp, Rval) :-
	stack_layout__make_tagged_rval(8, 0, Rval).

stack_layout__represent_lval(temp(_, _), _) :-
	error("stack_layout: continuation live value stored in temp register").

stack_layout__represent_lval(succip(_), _) :-
	error("stack_layout: continuation live value stored in code address").
stack_layout__represent_lval(redoip(_), _) :-
	error("stack_layout: continuation live value stored in code address").
stack_layout__represent_lval(succfr(_), _) :-
	error("stack_layout: continuation live value stored in code address").
stack_layout__represent_lval(prevfr(_), _) :-
	error("stack_layout: continuation live value stored in code address").

stack_layout__represent_lval(field(_, _, _), _) :-
	error("stack_layout: continuation live value stored in field").
stack_layout__represent_lval(mem_ref(_), _) :-
	error("stack_layout: continuation live value stored in mem_ref").
stack_layout__represent_lval(lvar(_), _) :-
	error("stack_layout: continuation live value stored in lvar").

	% Some things in this module are encoded using a low tag.
	% This is not done using the normal compiler mkword, but by
	% doing the bit shifting here.
	%
	% This allows us to use more than the usual 2 or 3 bits, but
	% we have to use low tags and cannot tag pointers this way.

:- pred stack_layout__make_tagged_rval(int::in, int::in, rval::out) is det.

stack_layout__make_tagged_rval(Tag, Value, Rval) :-
	stack_layout__make_tagged_word(Tag, Value, TaggedValue),
	Rval = const(int_const(TaggedValue)).

:- pred stack_layout__make_tagged_word(int::in, int::in, int::out) is det.

stack_layout__make_tagged_word(Tag, Value, TaggedValue) :-
	stack_layout__tag_bits(Bits),
	TaggedValue = (Value << Bits) + Tag.

	% Construct a represntation of  the code model.

:- pred stack_layout__represent_code_model(code_model, rval, stack_layout_info, 
		stack_layout_info).
:- mode stack_layout__represent_code_model(in, out, in, out) is det.
stack_layout__represent_code_model(CodeModel, Rval) -->
	(
		{ CodeModel = model_det },
		{ Rval = const(int_const(0)) }
	;
		{ CodeModel = model_semi },
		{ Rval = const(int_const(0)) }
	;
		{ CodeModel = model_non },
		{ Rval = const(int_const(1)) }
	).

:- pred stack_layout__code_model(code_model::in, int::out) is det.
stack_layout__code_model(model_det, 0).
stack_layout__code_model(model_semi, 0).
stack_layout__code_model(model_non, 1).

:- pred stack_layout__tag_bits(int::out) is det.
stack_layout__tag_bits(8).

%---------------------------------------------------------------------------%

	% Access to the stack_layout data structure.

:- pred stack_layout__get_module_name(string, stack_layout_info, 
		stack_layout_info).
:- mode stack_layout__get_module_name(out, in, out) is det.
stack_layout__get_module_name(ModuleName, LayoutInfo, LayoutInfo) :-
	LayoutInfo = stack_layout_info(ModuleName, _, _, _, _).

:- pred stack_layout__get_next_cell_number(int, stack_layout_info,
	stack_layout_info).
:- mode stack_layout__get_next_cell_number(out, in, out) is det.
stack_layout__get_next_cell_number(CNum0, LayoutInfo0, LayoutInfo) :-
	LayoutInfo0 = stack_layout_info(A, CNum0, C, D, E),
	CNum is CNum0 + 1,
	LayoutInfo = stack_layout_info(A, CNum, C, D, E).

:- pred stack_layout__get_cell_number(int, stack_layout_info,
		stack_layout_info).
:- mode stack_layout__get_cell_number(out, in, out) is det.
stack_layout__get_cell_number(CNum, LayoutInfo, LayoutInfo) :-
	LayoutInfo = stack_layout_info(_, CNum, _, _, _).

:- pred stack_layout__get_cmodules(list(c_module), stack_layout_info, 
		stack_layout_info).
:- mode stack_layout__get_cmodules(out, in, out) is det.
stack_layout__get_cmodules(CModules, LayoutInfo, LayoutInfo) :-
	LayoutInfo = stack_layout_info(_, _, _, _, CModules).

:- pred stack_layout__get_agc_stack_layout(bool, stack_layout_info, 
		stack_layout_info).
:- mode stack_layout__get_agc_stack_layout(out, in, out) is det.
stack_layout__get_agc_stack_layout(AgcStackLayout, LayoutInfo, LayoutInfo) :-
	LayoutInfo = stack_layout_info(_, _, AgcStackLayout, _, _).

:- pred stack_layout__get_trace_stack_layout(bool, stack_layout_info, 
		stack_layout_info).
:- mode stack_layout__get_trace_stack_layout(out, in, out) is det.
stack_layout__get_trace_stack_layout(TraceStackLayout, LayoutInfo,
		LayoutInfo) :-
	LayoutInfo = stack_layout_info(_, _, _, TraceStackLayout, _).

:- pred stack_layout__add_cmodule(c_module, stack_layout_info,
		stack_layout_info).
:- mode stack_layout__add_cmodule(in, in, out) is det.
stack_layout__add_cmodule(CModule, LayoutInfo0, LayoutInfo) :-
	LayoutInfo0 = stack_layout_info(A, B, C, D, CModules0),
	CModules = [CModule | CModules0],
	LayoutInfo = stack_layout_info(A, B, C, D, CModules).

:- pred stack_layout__set_cell_number(int, stack_layout_info,
		stack_layout_info).
:- mode stack_layout__set_cell_number(in, in, out) is det.
stack_layout__set_cell_number(CNum, LayoutInfo0, LayoutInfo) :-
	LayoutInfo0 = stack_layout_info(A, _, C, D, E),
	LayoutInfo = stack_layout_info(A, CNum, C, D, E).

