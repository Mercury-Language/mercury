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
% The tables generated have a number of `create' rvals within them.
% llds_common.m converts these into static data structures.
%
% We can create several types of stack layouts. Which kind we generate
% depends on the values of several options.
%
% Main author: trd.
% Modifications by zs.
%
%---------------------------------------------------------------------------%
%
% Data Stucture: stack_layouts
%
% If the option basic_stack_layout is set, we generate a stack layout table
% for each procedure. This table will be stored in the global variable
% whose name is
%	mercury_data__stack_layout__mercury__<proc_label>.
% This table will always contain the following information:
%
%	code address		(Code *) - address of entry
% 	determinism		(Integer) actually, type MR_determinism
% 	number of stack slots	(Integer)
% 	succip stack location	(Integer) actually, type MR_Live_Lval
% 					(the location will be set to -1
% 					if there is no succip available).
%
% if the option procid_stack_layout is set, i.e. if we are doing stack
% tracing, execution tracing or profiling, the table will also include
% information on the identity of the procedure. This information will take
% one of two forms. Almost all procedures use the first form:
%
%	predicate/function	(Int) actually, MR_pred_func
%	declaring module name	(String)
%	defining module name	(String)
%	predicate name		(String)
%	predicate arity		(Integer)
%	procedure number	(Integer)
%
% Automatically generated unification, index and comparison predicates
% use the second form:
%
%	type name		(String)
%	type module's name	(String)
%	defining module name	(String)
%	predicate name		(String)
%	predicate arity		(Integer)
%	procedure number	(Integer)
%
% The runtime system can figure out which form is present by testing
% the value of the first slot. A value of 0 or 1 indicates the first form;
% any higher value indicates the second form.
%
% The meanings of the fields in both forms are the same as in procedure labels.
%
% If the option trace_stack_layout is set, i.e. if we are doing execution
% tracing, the table will also include information on the variables that are
% live at entry to and exit from the procedure:
%
% 	# of live vars at entry	(Integer)
% 	live data pairs 	(Word *) - pointer to vector of pairs
%				containing MR_Live_Lval and MR_Live_Type
% 	live data names	 	(Word *) - pointer to vector of String
%	type parameters		(Word *) - pointer to vector of MR_Live_Lval
%
% 	# of live vars at exit	(Integer)
% 	live data pairs 	(Word *) - pointer to vector of pairs
%				containing MR_Live_Lval and MR_Live_Type
% 	live data names	 	(Word *) - pointer to vector of String
%	type parameters		(Word *) - pointer to vector of MR_Live_Lval
%
% The live data pair vector will have an entry for each live variable.
% The entry will give the location of the variable and its type (it also
% has room for its instantiation state, but this is not filled in yet).
%
% The live data name vector pointer may be NULL. If it is not, the vector
% will have an entry for each live variable, with each entry being either
% NULL or giving the name of the variable.
%
% The number of type parameters is never stored as it is not needed --
% the type parameter vector will simply be indexed by the type parameter
% number stored within pseudo-typeinfos inside the elements of the live
% data pairs vectors.
%
% If the option basic_stack_layout is set, we generate stack layout tables
% for all labels internal to the procedure. This table will be stored in the
% global variable whose name is
%	mercury_data__stack_layout__mercury__<proc_label>_i<label_number>.
% This table has the following format:
%
%	procedure info		(Word *) - pointer to procedure stack layout
%	internal label number	(Integer)
% 	# of live vars		(Integer)
% 	live data pairs 	(Word *) - pointer to vector of pairs
%				containing MR_Live_Lval and MR_Live_Type
% 	live data names	 	(Word *) - pointer to vector of String
% 	live data names	 	(Word *) - pointer to vector of String
%	type parameters		(Word *) - pointer to vector of MR_Live_Lval
%
% We need detailed information about the variables that are live at an internal
% label in two kinds of circumstances:
%
% -	the option trace_stack_layout is set, and the label represents
%	a traced event (with the current set of events, this means the
%	the entrance to one branch of a branched control structure)
%
% -	the option agc_stack_layout is set, and the label represents
% 	a point where execution can resume after a procedure call or
%	after backtracking.
%
% If either of these conditions holds for a given label at which there are some
% live variables, all the fields above will be present in the stack layout
% table for that label. However, the pointer to the live data names vector
% will be NULL unless the first condition holds for the label (i.e. the label
% is used in execution tracing).
%
% If neither condition holds for a given label, or if the number of live
% variables at that label is zero, then the "# of live vars" field will be zero
% and the last four fields will not be present.
%
% XXX: Presently, type parameter vectors are not created, and
% inst information is ignored. We also do not yet enable procid stack
% layouts for profiling, since profiling does not yet use stack layouts.
%
%---------------------------------------------------------------------------%

:- module stack_layout.

:- interface.

:- import_module hlds_module, list, llds.

:- pred stack_layout__generate_llds(module_info, module_info, list(c_module)).
:- mode stack_layout__generate_llds(in, out, out) is det.

:- implementation.

:- import_module globals, options, continuation_info, llds_out.
:- import_module hlds_data, hlds_pred, base_type_layout, prog_data, prog_out.
:- import_module assoc_list, bool, string, int, map, std_util, require.
:- import_module set.

:- type stack_layout_info 	--->	
	stack_layout_info(
		module_name,	% module name
		int,		% next available cell number
		bool,		% generate agc layout info?
		bool,		% generate tracing layout info?
		bool,		% generate procedure id layout info?
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
	globals__lookup_bool_option(Globals, procid_stack_layout,
		ProcInfoLayout),

	LayoutInfo0 = stack_layout_info(ModuleName, CellCount, AgcLayout,
		TraceLayout, ProcInfoLayout, []),
	list__foldl(stack_layout__construct_layouts, ProcLayoutList,
		LayoutInfo0, LayoutInfo),

	stack_layout__get_cmodules(CModules, LayoutInfo, _),
	stack_layout__get_cell_number(FinalCellCount, LayoutInfo, _),
	module_info_set_cell_count(ModuleInfo0, FinalCellCount, ModuleInfo).

%---------------------------------------------------------------------------%

	% Construct the layouts for a single procedure.
	
:- pred stack_layout__construct_layouts(proc_layout_info::in,
		stack_layout_info::in, stack_layout_info::out) is det.

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

:- pred stack_layout__construct_proc_layout(proc_layout_general_info::in,
		maybe(continuation_label_info)::in,
		maybe(continuation_label_info)::in,
		stack_layout_info::in, stack_layout_info::out) is det.

stack_layout__construct_proc_layout(GeneralInfo, MaybeEntryInfo,
		MaybeExitInfo) -->
	{ GeneralInfo = proc_layout_general_info(ProcLabel, Detism,
		StackSlots, SuccipLoc) },
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
	{ determinism_components(Detism, _, at_most_many) ->
		SuccipLval = framevar(Location)
	;
		SuccipLval = stackvar(Location)
	},
	{ Label = local(ProcLabel) },
	{ stack_layout__represent_lval(SuccipLval, SuccipRval) },
	{ StackSlotsRval = const(int_const(StackSlots)) },
	{ CodeAddrRval = const(code_addr_const(label(Label))) },

	{ stack_layout__represent_determinism(Detism, DetismRval) },
	{ MaybeRvals0 = [yes(CodeAddrRval), yes(DetismRval),
		yes(StackSlotsRval), yes(SuccipRval)] },
	stack_layout__get_module_name(ModuleName),

	stack_layout__get_procid_stack_layout(ProcIdLayout),
	(
		{ ProcIdLayout = yes }
	->
		{ stack_layout__construct_procid_rvals(ProcLabel, IdRvals) },
		{ list__append(MaybeRvals0, IdRvals, MaybeRvals1) },

		stack_layout__get_trace_stack_layout(TraceLayout),
		(
			{ TraceLayout = yes }
		->
			stack_layout__construct_trace_rvals(MaybeEntryInfo,
				MaybeExitInfo, TraceRvals),
			{ list__append(MaybeRvals1, TraceRvals, MaybeRvals) }
		;
			{ MaybeRvals = MaybeRvals1 }
		)
	;
		{ MaybeRvals = MaybeRvals0 }
	),

	{ CModule = c_data(ModuleName, stack_layout(Label), yes,
		MaybeRvals, []) },
	stack_layout__add_cmodule(CModule).

%---------------------------------------------------------------------------%

:- pred stack_layout__construct_procid_rvals(proc_label::in,
	list(maybe(rval))::out) is det.

stack_layout__construct_procid_rvals(ProcLabel, Rvals) :-
	(
		ProcLabel = proc(DefModule, PredFunc, DeclModule,
			PredName, Arity, ProcId),
		stack_layout__represent_pred_or_func(PredFunc, PredFuncCode),
		prog_out__sym_name_to_string(DefModule, DefModuleString),
		prog_out__sym_name_to_string(DeclModule, DeclModuleString),
		proc_id_to_int(ProcId, Mode),
		Rvals = [
				yes(const(int_const(PredFuncCode))),
				yes(const(string_const(DeclModuleString))),
				yes(const(string_const(DefModuleString))),
				yes(const(string_const(PredName))),
				yes(const(int_const(Arity))),
				yes(const(int_const(Mode)))
			]
	;
		ProcLabel = special_proc(DefModule, PredName, TypeModule,
			TypeName, Arity, ProcId),
		prog_out__sym_name_to_string(TypeModule, TypeModuleString),
		prog_out__sym_name_to_string(DefModule, DefModuleString),
		proc_id_to_int(ProcId, Mode),
		Rvals = [
				yes(const(string_const(TypeName))),
				yes(const(string_const(TypeModuleString))),
				yes(const(string_const(DefModuleString))),
				yes(const(string_const(PredName))),
				yes(const(int_const(Arity))),
				yes(const(int_const(Mode)))
			]
	).

:- pred stack_layout__represent_pred_or_func(pred_or_func::in, int::out) is det.

stack_layout__represent_pred_or_func(predicate, 0).
stack_layout__represent_pred_or_func(function, 1).

%---------------------------------------------------------------------------%

	% Construct the layout describing a single continuation label.

:- pred stack_layout__construct_internal_layout(proc_label::in,
	pair(label, internal_layout_info)::in,
	stack_layout_info::in, stack_layout_info::out) is det.

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

:- pred stack_layout__construct_trace_rvals(maybe(continuation_label_info)::in,
	maybe(continuation_label_info)::in, list(maybe(rval))::out,
	stack_layout_info::in, stack_layout_info::out) is det.

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
		{ error("stack_layout__construct_trace_rvals: entry or exit information not available.") }
	).

	% Construct the rvals required for accurate GC.

:- pred stack_layout__construct_agc_rvals(internal_layout_info::in,
	list(maybe(rval))::out,
	stack_layout_info::in, stack_layout_info::out) is det.

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

%---------------------------------------------------------------------------%

:- pred stack_layout__construct_livelval_rvals(set(var_info),
		set(pair(tvar, lval)), list(maybe(rval)),
		stack_layout_info, stack_layout_info).
:- mode stack_layout__construct_livelval_rvals(in, in, out, in, out) is det.

stack_layout__construct_livelval_rvals(LiveLvalSet, TVarSet, RvalList) -->
	{ set__to_sorted_list(LiveLvalSet, LiveLvals) },
	{ list__length(LiveLvals, Length) },
	{ LengthRval = const(int_const(Length)) },
	stack_layout__construct_liveval_pairs(LiveLvals, LiveValRval,
		NamesRval),

	{ set__to_sorted_list(TVarSet, TVars) },
	{ assoc_list__values(TVars, TypeParamLvals) },
	stack_layout__construct_type_parameter_locn_vector(TypeParamLvals,
		TypeParamRval),

	{ RvalList = [yes(LengthRval), yes(LiveValRval),
		yes(NamesRval), yes(TypeParamRval)] }.

%---------------------------------------------------------------------------%

:- pred stack_layout__construct_type_parameter_locn_vector(list(lval)::in,
	rval::out, stack_layout_info::in, stack_layout_info::out) is det.

stack_layout__construct_type_parameter_locn_vector(TypeParamLvals,
		TypeParamVector) -->
	{ MakeLval = lambda([Lval::in, yes(Rval)::out] is det, (
		stack_layout__represent_lval(Lval, Rval))) },
	{ list__map(MakeLval, TypeParamLvals, TypeParamLocs) },
	stack_layout__get_next_cell_number(CNum1),
	{ TypeParamVector = create(0, TypeParamLocs, no, CNum1,
		"stack_layout_type_parameter_locn_vector") }.

	% Construct a vector of (lval, live_value_type) pairs,
	% and a corresponding vector of variable names.

:- pred stack_layout__construct_liveval_pairs(list(var_info)::in,
	rval::out, rval::out, stack_layout_info::in, stack_layout_info::out)
	is det.

stack_layout__construct_liveval_pairs(LiveLvals, LocnVector, NameVector) -->
	list__map_foldl(stack_layout__construct_liveval_pair, LiveLvals,
		LocnTypePairs),
	{ list__condense(LocnTypePairs, LocnTypeVectorArgs) },
	stack_layout__get_next_cell_number(CNum1),
	{ LocnVector = create(0, LocnTypeVectorArgs, no, CNum1,
		"stack_layout_locn_vector") },

	{ list__map(stack_layout__construct_liveval_name, LiveLvals, Names) },
	stack_layout__get_next_cell_number(CNum2),
	{ NameVector = create(0, Names, no, CNum2,
		"stack_layout_name_vector") }.

	% Construct a pair of (lval, live_value_type) representations.

:- pred stack_layout__construct_liveval_pair(var_info::in,
	list(maybe(rval))::out, stack_layout_info::in, stack_layout_info::out)
	is det.

stack_layout__construct_liveval_pair(var_info(Lval, LiveValueType, _),
		MaybeRvals) -->
	{ stack_layout__represent_lval(Lval, Rval0) },
	stack_layout__represent_live_value_type(LiveValueType, Rval1),
	{ MaybeRvals = [yes(Rval0), yes(Rval1)] }.

:- pred stack_layout__construct_liveval_name(var_info::in, maybe(rval)::out)
	is det.

stack_layout__construct_liveval_name(var_info(_, _, Name), MaybeRval) :-
	( Name = "" ->
		% We prefer a null pointer to a pointer to an empty string,
		% since this way we don't need many copies of the empty string.
		Rval = const(int_const(0))
	;
		Rval = const(string_const(Name))
	),
	MaybeRval = yes(Rval).

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
	{ base_type_layout__construct_pseudo_type_info(Type, Rval0,
		CNum0, CNum) },
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

:- pred stack_layout__tag_bits(int::out) is det.

stack_layout__tag_bits(8).

%---------------------------------------------------------------------------%

	% Construct a representation of the interface determinism of a
	% procedure. The code we have chosen is not sequential; instead
	% it encodes the various properties of each determinism.
	%
	% The 8 bit is set iff the context is first_solution.
	% The 4 bit is set iff the min number of solutions is more than zero.
	% The 2 bit is set iff the max number of solutions is more than zero.
	% The 1 bit is set iff the max number of solutions is more than one.

:- pred stack_layout__represent_determinism(determinism::in, rval::out) is det.

stack_layout__represent_determinism(Detism, const(int_const(Code))) :-
	(
		Detism = det,
		Code = 6		/* 0110 */
	;
		Detism = semidet,	/* 0010 */
		Code = 2
	;
		Detism = nondet,
		Code = 3		/* 0011 */
	;
		Detism = multidet,
		Code = 7		/* 0111 */
	;
		Detism = erroneous,
		Code = 4		/* 0100 */
	;
		Detism = failure,
		Code = 0		/* 0000 */
	;
		Detism = cc_nondet,
		Code = 10 		/* 1010 */
	;
		Detism = cc_multidet,
		Code = 14		/* 1110 */
	).

%---------------------------------------------------------------------------%

	% Access to the stack_layout data structure.

:- pred stack_layout__get_module_name(module_name::out,
	stack_layout_info::in, stack_layout_info::out) is det.

stack_layout__get_module_name(ModuleName, LayoutInfo, LayoutInfo) :-
	LayoutInfo = stack_layout_info(ModuleName, _, _, _, _, _).

:- pred stack_layout__get_next_cell_number(int::out,
	stack_layout_info::in, stack_layout_info::out) is det.

stack_layout__get_next_cell_number(CNum0, LayoutInfo0, LayoutInfo) :-
	LayoutInfo0 = stack_layout_info(A, CNum0, C, D, E, F),
	CNum is CNum0 + 1,
	LayoutInfo = stack_layout_info(A, CNum, C, D, E, F).

:- pred stack_layout__get_cell_number(int::out,
	stack_layout_info::in, stack_layout_info::out) is det.

stack_layout__get_cell_number(CNum, LayoutInfo, LayoutInfo) :-
	LayoutInfo = stack_layout_info(_, CNum, _, _, _, _).

:- pred stack_layout__get_cmodules(list(c_module)::out,
	stack_layout_info::in, stack_layout_info::out) is det.

stack_layout__get_cmodules(CModules, LayoutInfo, LayoutInfo) :-
	LayoutInfo = stack_layout_info(_, _, _, _, _, CModules).

:- pred stack_layout__get_agc_stack_layout(bool::out,
	stack_layout_info::in, stack_layout_info::out) is det.

stack_layout__get_agc_stack_layout(AgcStackLayout, LayoutInfo, LayoutInfo) :-
	LayoutInfo = stack_layout_info(_, _, AgcStackLayout, _, _, _).

:- pred stack_layout__get_trace_stack_layout(bool::out,
	stack_layout_info::in, stack_layout_info::out) is det.

stack_layout__get_trace_stack_layout(TraceStackLayout, LayoutInfo,
		LayoutInfo) :-
	LayoutInfo = stack_layout_info(_, _, _, TraceStackLayout, _, _).

:- pred stack_layout__get_procid_stack_layout(bool::out,
	stack_layout_info::in, stack_layout_info::out) is det.

stack_layout__get_procid_stack_layout(ProcIdStackLayout, LayoutInfo,
		LayoutInfo) :-
	LayoutInfo = stack_layout_info(_, _, _, _, ProcIdStackLayout, _).

:- pred stack_layout__add_cmodule(c_module::in,
	stack_layout_info::in, stack_layout_info::out) is det.

stack_layout__add_cmodule(CModule, LayoutInfo0, LayoutInfo) :-
	LayoutInfo0 = stack_layout_info(A, B, C, D, E, CModules0),
	CModules = [CModule | CModules0],
	LayoutInfo = stack_layout_info(A, B, C, D, E, CModules).

:- pred stack_layout__set_cell_number(int::in,
	stack_layout_info::in, stack_layout_info::out) is det.

stack_layout__set_cell_number(CNum, LayoutInfo0, LayoutInfo) :-
	LayoutInfo0 = stack_layout_info(A, _, C, D, E, F),
	LayoutInfo = stack_layout_info(A, CNum, C, D, E, F).

