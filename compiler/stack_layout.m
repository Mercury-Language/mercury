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
% NOTE: If you make changes in this file, you may also need to modify
% runtime/mercury_stack_layout.h.
%
%---------------------------------------------------------------------------%
%
% Data Stucture: stack_layouts
%
% If the option basic_stack_layout is set, we generate a stack layout table
% for each procedure. This table will be stored in the global variable
% whose name is
%	mercury_data__layout__mercury__<proc_label>.
% This table will always contain the following information:
%
%	code address		(Code *) - address of entry
% 	determinism		(Integer) actually, type MR_Determinism
% 	number of stack slots	(Integer)
% 	succip stack location	(Integer) actually, type MR_Live_Lval
% 					(the location will be set to -1
% 					if there is no succip available).
%
% If the option procid_stack_layout is set, i.e. if we are doing stack
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
% any higher value indicates the second form. The distinguished value -1
% indicates that procid_stack_layout is not set, and that the later fields
% are not present.
%
% The meanings of the fields in both forms are the same as in procedure labels.
%
% If the option trace_stack_layout is set, i.e. if we are doing execution
% tracing, the table will also include two extra fields:
%
%	call trace info		(Word *) - pointer to label stack layout
%	maybe from full		(Integer) - stack slot of the from_full
%				flag, if the procedure is shallow traced
%
% The first will point to the per-label layout info for the label associated
% with the call event at the entry to the procedure. The purpose of this
% information is to allow the runtime debugger to find out which variables
% are where on entry, so it can reexecute the procedure if asked to do so
% and if the values of the required variables are still available.
% (If trace_stack_layout is not set, this field will be present,
% but it will be set to NULL.)
%
% If the procedure is compiled with deep tracing, the last field will contain
% a negative number. If it is compiled with shallow tracing, it will contain
% the number of the stack slot that holds the flag that says whether this
% incarnation of the procedure was called from deeply traced code or not.
% (The determinism of the procedure decides whether the stack slot refers
% to a stackvar or a framevar.)
%
% If the option basic_stack_layout is set, we generate stack layout tables
% for some labels internal to the procedure. This table will be stored in the
% global variable whose name is
%	mercury_data__layout__mercury__<proc_label>_i<label_number>.
% This table has the following format:
%
%	procedure info		(Word *) - pointer to procedure stack layout
% 	# of live vars		(Integer)
% 	live data pairs 	(Word *) - pointer to vector of pairs
%				containing MR_Live_Lval and MR_Live_Type
% 	live data names	 	(Word *) - pointer to vector of String
%	type parameters		(Word *) - pointer to vector of MR_Live_Lval
%				in which the first word is an Integer
%				giving the number of entries in the vector;
%				a NULL pointer means no type parameters
%
% The live data pair vector will have an entry for each live variable.
% The entry will give the location of the variable and its type. (It also
% has room for its instantiation state, but this is not filled in yet.)
%
% The live data name vector pointer will be NULL. If it is not, the vector
% will have an entry for each live variable, with each entry giving the name
% of the variable (it is either a pointer to a string, or a NULL pointer,
% which means that the variable has no name).
%
% If the number of type parameters is not zero, we store the number,
% so that the code that needs the type parameters can materialize
% all the type parameters from their location descriptions in one go.
% This is an optimization, since the type parameter vector could simply
% be indexed on demand by the type variable's variable number stored within
% pseudo-typeinfos inside the elements of the live data pairs vectors.
%
% Since we allocate type variable numbers sequentially, the type parameter
% vector will usually be dense. However, after all variables whose types
% include e.g. type variable 2 have gone out of scope, variables whose
% types include type variable 3 may still be around. In cases like this,
% the entry for type variable 2 will be zero; this signals to the code
% in the internal debugger that materializes typeinfo structures that
% this typeinfo structure need not be materialized.
%
% We need detailed information about the variables that are live at an
% internal label in two kinds of circumstances. Stack layout information
% will be present only for labels that fall into one or both of these
% circumstances.
%
% -	The option trace_stack_layout is set, and the label represents
%	a traced event at which variable info is needed (call, exit,
%	or entrance to one branch of a branched control structure;
%	fail events have no variable information).
%
% -	The option agc_stack_layout is set or the trace level specifies
%	a capability for uplevel printing, and the label represents
% 	a point where execution can resume after a procedure call or
%	after backtracking.
%
% For labels that do not fall into one of these two categories, the
% "# of live vars" field will be negative to indicate the absence of
% information about the variables live at this label, and the last
% four fields will not be present.
%
% For labels that do fall into one of these two categories, the
% "# of live vars" field will hold the number of live variables, which
% will not be negative. If it is zero, the last four fields will not be
% present. Even if it is not zero, however, the pointer to the live data
% names vector will be NULL unless the label is used in execution tracing.
%
% XXX: Presently, inst information is ignored. We also do not yet enable
% procid stack layouts for profiling, since profiling does not yet use
% stack layouts.
%
%---------------------------------------------------------------------------%

:- module stack_layout.

:- interface.

:- import_module hlds_module, llds.
:- import_module list, set_bbbtree.

:- pred stack_layout__generate_llds(module_info::in, module_info::out,
	list(c_module)::out, set_bbbtree(label)::out) is det.

:- implementation.

:- import_module globals, options, continuation_info, llds_out.
:- import_module hlds_data, hlds_pred, base_type_layout, prog_data, prog_out.
:- import_module assoc_list, bool, string, int, require.
:- import_module map, std_util, term, set.

:- type stack_layout_info 	--->	
	stack_layout_info(
		module_name,	% module name
		int,		% next available cell number
		bool,		% generate agc layout info?
		bool,		% generate tracing layout info?
		bool,		% generate procedure id layout info?
		list(c_module),	% generated data
		set_bbbtree(label)
				% the set of labels with stack layouts
	).

%---------------------------------------------------------------------------%

	% Process all the continuation information stored in the HLDS,
	% converting it into LLDS data structures.

stack_layout__generate_llds(ModuleInfo0, ModuleInfo, CModules,
		StackLayoutLabels) :-
	module_info_get_continuation_info(ModuleInfo0, ContinuationInfo),
	continuation_info__get_all_proc_layouts(ContinuationInfo,
		ProcLayoutList),

	module_info_name(ModuleInfo0, ModuleName),
	module_info_get_cell_count(ModuleInfo0, CellCount),
	module_info_globals(ModuleInfo0, Globals),
	globals__lookup_bool_option(Globals, agc_stack_layout, AgcLayout),
	globals__lookup_bool_option(Globals, trace_stack_layout, TraceLayout),
	globals__lookup_bool_option(Globals, procid_stack_layout,
		ProcInfoLayout),
	set_bbbtree__init(StackLayoutLabels0),

	LayoutInfo0 = stack_layout_info(ModuleName, CellCount, AgcLayout,
		TraceLayout, ProcInfoLayout, [], StackLayoutLabels0),
	list__foldl(stack_layout__construct_layouts, ProcLayoutList,
		LayoutInfo0, LayoutInfo),

	stack_layout__get_cmodules(CModules, LayoutInfo, _),
	stack_layout__get_label_set(StackLayoutLabels, LayoutInfo, _),
	stack_layout__get_cell_number(FinalCellCount, LayoutInfo, _),
	module_info_set_cell_count(ModuleInfo0, FinalCellCount, ModuleInfo).

%---------------------------------------------------------------------------%

	% Construct the layouts that concern a single procedure:
	% the procedure-specific layout and the layouts of the labels
	% inside that procedure.
	
:- pred stack_layout__construct_layouts(proc_layout_info::in,
	stack_layout_info::in, stack_layout_info::out) is det.

stack_layout__construct_layouts(ProcLayoutInfo) -->
	{ ProcLayoutInfo = proc_layout_info(EntryLabel, Detism,
		StackSlots, SuccipLoc, MaybeCallLabel, MaybeFromFullSlot,
		InternalMap) },
	stack_layout__construct_proc_layout(EntryLabel, Detism,
		StackSlots, SuccipLoc, MaybeCallLabel, MaybeFromFullSlot),
	{ map__to_assoc_list(InternalMap, Internals) },
	list__foldl(stack_layout__construct_internal_layout(EntryLabel),
		Internals).

%---------------------------------------------------------------------------%

	% Construct a procedure-specific layout.

:- pred stack_layout__construct_proc_layout(label::in,
	determinism::in, int::in, maybe(int)::in, maybe(label)::in,
	maybe(int)::in, stack_layout_info::in, stack_layout_info::out) is det.

stack_layout__construct_proc_layout(EntryLabel, Detism, StackSlots,
		MaybeSuccipLoc, MaybeCallLabel, MaybeFromFullSlot) -->
	{
		MaybeSuccipLoc = yes(Location0)
	->
		Location = Location0
	;
			% Use a dummy location of -1 if there is
			% no succip on the stack.
			%
			% This case can arise in two circumstances.
			% First, procedures that use the nondet stack
			% have a special slot for the succip, so the
			% succip is not stored in a general purpose
			% slot. Second, procedures that use the det stack
			% but which do not call other procedures
			% do not save the succip on the stack.
			%
			% The tracing system does not care about the
			% location of the saved succip. The accurate
			% garbage collector does. It should know from
			% the determinism that the procedure uses the
			% nondet stack, which takes care of the first
			% possibility above. Procedures that do not call
			% other procedures do not establish resumption
			% points and thus agc is not interested in them.
			% As far as stack dumps go, calling error counts
			% as a call, so any procedure that may call error
			% (directly or indirectly) will have its saved succip
			% location recorded, so the stack dump will work.
			%
			% Future uses of stack layouts will have to have
			% similar constraints.
		Location = -1
	},
	{ determinism_components(Detism, _, at_most_many) ->
		SuccipLval = framevar(Location)
	;
		SuccipLval = stackvar(Location)
	},
	{ stack_layout__represent_locn(direct(SuccipLval), SuccipRval) },
	{ StackSlotsRval = const(int_const(StackSlots)) },
	{ CodeAddrRval = const(code_addr_const(label(EntryLabel))) },

	{ stack_layout__represent_determinism(Detism, DetismRval) },
	{ MaybeRvals0 = [yes(CodeAddrRval), yes(DetismRval),
		yes(StackSlotsRval), yes(SuccipRval)] },

	stack_layout__get_procid_stack_layout(ProcIdLayout),
	(
		{ ProcIdLayout = yes }
	->
		{ stack_layout__construct_procid_rvals(EntryLabel, IdRvals) },
		{ list__append(MaybeRvals0, IdRvals, MaybeRvals1) }
	;
		{ NoIdRvals = yes(const(int_const(-1))) },
		{ list__append(MaybeRvals0, [NoIdRvals], MaybeRvals1) }
	),

	stack_layout__get_module_name(ModuleName),
	stack_layout__get_trace_stack_layout(TraceLayout),
	{
		TraceLayout = yes
	->
		( MaybeCallLabel = yes(CallLabel) ->
			CallRval = yes(const(data_addr_const(
					data_addr(ModuleName,
						stack_layout(CallLabel)))))
		;
			error("stack_layout__construct_proc_layout: call label not present")
		),
		( MaybeFromFullSlot = yes(FromFullSlot) ->
			FromFullRval = yes(const(int_const(FromFullSlot)))
		;
			FromFullRval = yes(const(int_const(-1)))
		),
		list__append(MaybeRvals1, [CallRval, FromFullRval],
			MaybeRvals)
	;
		NoCallRval = yes(const(int_const(0))),
		list__append(MaybeRvals1, [NoCallRval], MaybeRvals)
	},

	{ Exported = no },	% XXX With the new profiler, we will need to
				% set this to `yes' if the profiling option
				% is given and if the procedure is exported.
				% Beware however that linkage/2 in llds_out.m
				% assumes that this is `no'.
	{ CModule = c_data(ModuleName, stack_layout(EntryLabel), Exported,
		MaybeRvals, []) },
	stack_layout__add_cmodule(CModule, EntryLabel).

%---------------------------------------------------------------------------%

:- pred stack_layout__construct_procid_rvals(label::in,
	list(maybe(rval))::out) is det.

stack_layout__construct_procid_rvals(Label, Rvals) :-
	( 
		Label = local(ProcLabel, _)
	;
		Label = c_local(ProcLabel)
	;
		Label = local(ProcLabel)
	;
		Label = exported(ProcLabel)
	),
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

	% Construct the layout describing a single internal label.

:- pred stack_layout__construct_internal_layout(label::in,
	pair(label, internal_layout_info)::in,
	stack_layout_info::in, stack_layout_info::out) is det.

stack_layout__construct_internal_layout(EntryLabel, Label - Internal) -->
		% generate the required rvals
	stack_layout__get_module_name(ModuleName),
	{ EntryAddrRval = const(data_addr_const(data_addr(ModuleName,
		stack_layout(EntryLabel)))) },
	stack_layout__construct_internal_rvals(Internal, VarInfoRvals),
	% Reenable this code if you want label numbers in label layouts.
	% { Label = local(_, LabelNum0) ->
	% 	LabelNum = LabelNum0
	% ;
	% 	LabelNum = 0
	% },
	% { LabelNumRval = const(int_const(LabelNum)) },
	% { LayoutRvals = [yes(EntryAddrRval), yes(LabelNumRval
	% 	| VarInfoRvals] }
	{ LayoutRvals = [yes(EntryAddrRval) | VarInfoRvals] },
	{ CModule = c_data(ModuleName, stack_layout(Label), no,
		LayoutRvals, []) },
	stack_layout__add_cmodule(CModule, Label).

	% Construct the rvals required for accurate GC or for tracing.

:- pred stack_layout__construct_internal_rvals(internal_layout_info::in,
	list(maybe(rval))::out,
	stack_layout_info::in, stack_layout_info::out) is det.

stack_layout__construct_internal_rvals(Internal, RvalList) -->
	{ Internal = internal_layout_info(Port, Return) },
	{
		Port = no,
		set__init(PortLiveVarSet),
		map__init(PortTypeVarMap)
	;
		Port = yes(layout_label_info(PortLiveVarSet, PortTypeVarMap))
	},
	stack_layout__get_agc_stack_layout(AgcStackLayout),
	{
		Return = no,
		set__init(ReturnLiveVarSet),
		map__init(ReturnTypeVarMap)
	;
		Return = yes(layout_label_info(ReturnLiveVarSet0,
			ReturnTypeVarMap0)),
		( AgcStackLayout = yes ->
			ReturnLiveVarSet = ReturnLiveVarSet0,
			ReturnTypeVarMap = ReturnTypeVarMap0
		;
			% This set of variables must be for uplevel printing
			% in execution tracing, so we are interested only
			% in (a) variables, not temporaries, (b) only named
			% variables, and (c) only those on the stack, not
			% the return values.
			set__to_sorted_list(ReturnLiveVarSet0,
				ReturnLiveVarList0),
			stack_layout__select_trace_return(
				ReturnLiveVarList0, ReturnTypeVarMap0,
				ReturnLiveVarList, ReturnTypeVarMap),
			set__list_to_set(ReturnLiveVarList, ReturnLiveVarSet)
		)
	},
	(
		{ Port = no },
		{ Return = no }
	->
			% The -1 says that there is no info available
			% about variables at this label. (Zero would say
			% that there are no variables live at this label,
			% which may not be true.)
		{ RvalList = [yes(const(int_const(-1)))] }
	;
			% XXX ignore differences in insts inside var_infos
		{ set__union(PortLiveVarSet, ReturnLiveVarSet, LiveVarSet) },
		{ map__union(set__intersect, PortTypeVarMap, ReturnTypeVarMap,
			TypeVarMap) },
		stack_layout__construct_livelval_rvals(LiveVarSet,
			TypeVarMap, RvalList)
	).

%---------------------------------------------------------------------------%

:- pred stack_layout__construct_livelval_rvals(set(var_info)::in,
	map(tvar, set(layout_locn))::in, list(maybe(rval))::out,
	stack_layout_info::in, stack_layout_info::out) is det.

stack_layout__construct_livelval_rvals(LiveLvalSet, TVarLocnMap, RvalList) -->
	{ set__to_sorted_list(LiveLvalSet, LiveLvals) },
	{ list__length(LiveLvals, Length) },
	{ VarLengthRval = const(int_const(Length)) },
	( { Length > 0 } ->
		{ stack_layout__sort_livevals(LiveLvals, SortedLiveLvals) },
		stack_layout__construct_liveval_pairs(SortedLiveLvals,
			LiveValRval, NamesRval),

		{ map__to_assoc_list(TVarLocnMap, TVarLocns) },
		( { TVarLocns = [] } ->
			{ TypeParamRval = const(int_const(0)) }
		;
			stack_layout__construct_type_param_locn_vector(
				TVarLocns, 1, TypeParamLocs),
			{ list__length(TypeParamLocs, TypeParamsLength) },
			{ LengthRval = const(int_const(TypeParamsLength)) },
			{ Vector = [yes(LengthRval) | TypeParamLocs] },
			stack_layout__get_next_cell_number(CNum1),
			{ TypeParamRval = create(0, Vector, no, CNum1,
				"stack_layout_type_param_locn_vector") }
		),
		{ RvalList = [yes(VarLengthRval), yes(LiveValRval),
			yes(NamesRval), yes(TypeParamRval)] }
	;
		{ RvalList = [yes(VarLengthRval)] }
	).

%---------------------------------------------------------------------------%

	% Given a list of var_infos and the type variables that occur in them,
	% select only the var_infos that may be required by up-level printing
	% in the trace-based debugger. At the moment the typeinfo list we
	% return may be bigger than necessary, but this does not compromise
	% correctness; we do this to avoid having to scan the types of all
	% the selected var_infos.

:- pred stack_layout__select_trace_return(
	list(var_info)::in, map(tvar, set(layout_locn))::in,
	list(var_info)::out, map(tvar, set(layout_locn))::out) is det.

stack_layout__select_trace_return(Infos, TVars, TraceReturnInfos, TVars) :-
	IsNamedReturnVar = lambda([LocnInfo::in] is semidet, (
		LocnInfo = var_info(Locn, LvalType),
		LvalType = var(_, Name, _, _),
		Name \= "",
		( Locn = direct(Lval) ; Locn = indirect(Lval, _)),
		( Lval = stackvar(_) ; Lval = framevar(_) )
	)),
	list__filter(IsNamedReturnVar, Infos, TraceReturnInfos).

	% Given a list of var_infos, put the ones that tracing can be
	% interested in (whether at an internal port or for uplevel printing)
	% in a block at the start, and both this block and the remaining
	% block. The division into two blocks can make the job of the
	% debugger somewhat easier, the sorting of the named var block makes
	% the output of the debugger look nicer, and the sorting of the both
	% blocks makes it more likely that different labels' layout structures
	% will have common parts (e.g. name vectors) that can be optimized
	% by llds_common.m.

:- pred stack_layout__sort_livevals(list(var_info)::in, list(var_info)::out)
	is det.

stack_layout__sort_livevals(OrigInfos, FinalInfos) :-
	IsNamedVar = lambda([LvalInfo::in] is semidet, (
		LvalInfo = var_info(_Lval, LvalType),
		LvalType = var(_, Name, _, _),
		Name \= ""
	)),
	list__filter(IsNamedVar, OrigInfos, NamedVarInfos0, OtherInfos0),
	CompareVarInfos = lambda([Var1::in, Var2::in, Result::out] is det, (
		Var1 = var_info(Lval1, LiveType1),
		Var2 = var_info(Lval2, LiveType2),
		stack_layout__get_name_from_live_value_type(LiveType1, Name1),
		stack_layout__get_name_from_live_value_type(LiveType2, Name2),
		compare(NameResult, Name1, Name2),
		( NameResult = (=) ->
			compare(Result, Lval1, Lval2)
		;
			Result = NameResult
		)
	)),
	list__sort(CompareVarInfos, NamedVarInfos0, NamedVarInfos),
	list__sort(CompareVarInfos, OtherInfos0, OtherInfos),
	list__append(NamedVarInfos, OtherInfos, FinalInfos).

:- pred stack_layout__get_name_from_live_value_type(live_value_type::in,
	string::out) is det.

stack_layout__get_name_from_live_value_type(LiveType, Name) :-
	( LiveType = var(_, NamePrime, _, _) ->
		Name = NamePrime
	;
		Name = ""
	).

%---------------------------------------------------------------------------%

	% Given a association list of type variables and their locations
	% sorted on the type variables, represent them in an array of
	% location descriptions indexed by the type variable. The next
	% slot to fill is given by the second argument.

:- pred stack_layout__construct_type_param_locn_vector(
	assoc_list(tvar, set(layout_locn))::in,
	int::in, list(maybe(rval))::out,
	stack_layout_info::in, stack_layout_info::out) is det.

stack_layout__construct_type_param_locn_vector([], _, []) --> [].
stack_layout__construct_type_param_locn_vector([TVar - Locns | TVarLocns],
		CurSlot, Vector) -->
	{ term__var_to_int(TVar, TVarNum) },
	{ NextSlot is CurSlot + 1 },
	( { TVarNum = CurSlot } ->
		{ set__remove_least(Locns, LeastLocn, _) ->
			Locn = LeastLocn
		;
			error("tvar has empty set of locations")
		},
		{ stack_layout__represent_locn(Locn, Rval) },
		stack_layout__construct_type_param_locn_vector(TVarLocns,
			NextSlot, VectorTail),
		{ Vector = [yes(Rval) | VectorTail] }
	; { TVarNum > CurSlot } ->
		stack_layout__construct_type_param_locn_vector(TVarLocns,
			NextSlot, VectorTail),
			% This slot will never be referred to.
		{ Vector = [yes(const(int_const(0))) | VectorTail] }
	;

		{ error("unsorted tvars in construct_type_param_locn_vector") }
	).

	% Construct a vector of (locn, live_value_type) pairs,
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

	% Construct a pair of (locn, live_value_type) representations.

:- pred stack_layout__construct_liveval_pair(var_info::in,
	list(maybe(rval))::out, stack_layout_info::in, stack_layout_info::out)
	is det.

stack_layout__construct_liveval_pair(var_info(Locn, LiveValueType),
		MaybeRvals) -->
	{ stack_layout__represent_locn(Locn, Rval0) },
	stack_layout__represent_live_value_type(LiveValueType, Rval1),
	{ MaybeRvals = [yes(Rval0), yes(Rval1)] }.

:- pred stack_layout__construct_liveval_name(var_info::in, maybe(rval)::out)
	is det.

stack_layout__construct_liveval_name(var_info(_, VarInfo), MaybeRval) :-
	(
		VarInfo = var(Var, Name, _, _),
		Name \= ""
	->
		% We include a representation of the variable number at the
		% start of the variable name, because some functions of the
		% debugger (e.g. restart) require it to be able to distinguish
		% between distinct variables that happen to have the same name.
		% We represent the number as a string, because most variable
		% numbers are so small that this is a very compact
		% representation.
		term__var_to_int(Var, Int),
		string__int_to_string(Int, IntStr),
		string__append_list([IntStr, ":", Name], NumberedName),
		Rval = const(string_const(NumberedName))
	;
		% We prefer a null pointer to a pointer to an empty string,
		% since this way we don't need many copies of the empty string.
		Rval = const(int_const(0))
	),
	MaybeRval = yes(Rval).

%---------------------------------------------------------------------------%

	% The constants and representations here should be kept in sync
	% with constants in the runtime system:
	% 	mercury_stack_layout.h - contains macros to access these
	%			 	constants.

	% Construct a representation of a live_value_type without the name.
	%
	% Low integers for special values, a pointer for other values.
	% (Remember to keep the low integers below the max varint value in
	% runtime/mercury_type_info.h).

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
stack_layout__represent_live_value_type(redofr, Rval) -->
	{ Rval = const(int_const(4)) }.
stack_layout__represent_live_value_type(redoip, Rval) -->
	{ Rval = const(int_const(5)) }.
stack_layout__represent_live_value_type(unwanted, Rval) -->
	{ Rval = const(int_const(6)) }.
stack_layout__represent_live_value_type(var(_, _, Type, _Inst), Rval) -->
	stack_layout__get_cell_number(CNum0),
	{ base_type_layout__construct_pseudo_type_info(Type, Rval0,
		CNum0, CNum1) },
	stack_layout__set_cell_number(CNum1),
		% XXX hack - don't yet write out insts
	{ Rval1 = const(int_const(-1)) },
	stack_layout__get_next_cell_number(CNum2),
	{ Rval = create(0, [yes(Rval0), yes(Rval1)], no, CNum2,
		"stack_layout_pair") }.

	% Construct a representation of a variable location.
	%
	% Most of the time, a layout specifies a location as an lval.
	% However, a type_info variable may be hidden inside a typeclass_info,
	% In this case, accessing the type_info requires indirection.
	% The address of the typeclass_info is given as an lval, and
	% the location of the typeinfo within the typeclass_info as an index;
	% private_builtin:type_info_from_typeclass_info interprets the index.
	%
	% This one level of indirection is sufficient, since type_infos
	% cannot be nested inside typeclass_infos any deeper than this.
	% A more general representation that would allow more indirection
	% would be much harder to fit into one machine word.

:- pred stack_layout__represent_locn(layout_locn, rval).
:- mode stack_layout__represent_locn(in, out) is det.

stack_layout__represent_locn(direct(Lval), Rval) :-
	stack_layout__represent_lval(Lval, Word),
	Rval = const(int_const(Word)).
stack_layout__represent_locn(indirect(Lval, Offset), Rval) :-
	stack_layout__represent_lval(Lval, BaseWord),
	stack_layout__offset_bits(OffsetBits),
	require((1 << OffsetBits) > Offset,
	"stack_layout__represent_locn: offset too large to be represented"),
	BaseAndOffset is (BaseWord << OffsetBits) + Offset,
	stack_layout__make_tagged_word(lval_indirect, BaseAndOffset, Word),
	Rval = const(int_const(Word)).

	% Construct a representation of an lval.

:- pred stack_layout__represent_lval(lval, int).
:- mode stack_layout__represent_lval(in, out) is det.

stack_layout__represent_lval(reg(r, Num), Word) :-
	stack_layout__make_tagged_word(lval_r_reg, Num, Word).
stack_layout__represent_lval(reg(f, Num), Word) :-
	stack_layout__make_tagged_word(lval_f_reg, Num, Word).

stack_layout__represent_lval(stackvar(Num), Word) :-
	stack_layout__make_tagged_word(lval_stackvar, Num, Word).
stack_layout__represent_lval(framevar(Num), Word) :-
	stack_layout__make_tagged_word(lval_framevar, Num, Word).

stack_layout__represent_lval(succip, Word) :-
	stack_layout__make_tagged_word(lval_succip, 0, Word).
stack_layout__represent_lval(maxfr, Word) :-
	stack_layout__make_tagged_word(lval_maxfr, 0, Word).
stack_layout__represent_lval(curfr, Word) :-
	stack_layout__make_tagged_word(lval_curfr, 0, Word).
stack_layout__represent_lval(hp, Word) :-
	stack_layout__make_tagged_word(lval_hp, 0, Word).
stack_layout__represent_lval(sp, Word) :-
	stack_layout__make_tagged_word(lval_sp, 0, Word).

stack_layout__represent_lval(temp(_, _), _) :-
	error("stack_layout: continuation live value stored in temp register").

stack_layout__represent_lval(succip(_), _) :-
	error("stack_layout: continuation live value stored in code address").
stack_layout__represent_lval(redoip(_), _) :-
	error("stack_layout: continuation live value stored in code address").
stack_layout__represent_lval(redofr(_), _) :-
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

:- pred stack_layout__make_tagged_word(locn_type::in, int::in, int::out) is det.

stack_layout__make_tagged_word(Locn, Value, TaggedValue) :-
	stack_layout__locn_type_code(Locn, Tag),
	stack_layout__tag_bits(TagBits),
	TaggedValue is (Value << TagBits) + Tag.

:- type locn_type
	--->	lval_r_reg
	;	lval_f_reg
	;	lval_stackvar
	;	lval_framevar
	;	lval_succip
	;	lval_maxfr
	;	lval_curfr
	;	lval_hp
	;	lval_sp
	;	lval_indirect.

:- pred stack_layout__locn_type_code(locn_type::in, int::out) is det.

stack_layout__locn_type_code(lval_r_reg,    0).
stack_layout__locn_type_code(lval_f_reg,    1).
stack_layout__locn_type_code(lval_stackvar, 2).
stack_layout__locn_type_code(lval_framevar, 3).
stack_layout__locn_type_code(lval_succip,   4).
stack_layout__locn_type_code(lval_maxfr,    5).
stack_layout__locn_type_code(lval_curfr,    6).
stack_layout__locn_type_code(lval_hp,       7).
stack_layout__locn_type_code(lval_sp,       8).
stack_layout__locn_type_code(lval_indirect, 9).

:- pred stack_layout__tag_bits(int::out) is det.

% This number of tag bits must be able to encode all values of
% stack_layout__locn_type_code.

stack_layout__tag_bits(4).

% This number of tag bits must be able to encode the largest offset
% of a type_info within a typeclass_info.

:- pred stack_layout__offset_bits(int::out) is det.

stack_layout__offset_bits(6).

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
	LayoutInfo = stack_layout_info(ModuleName, _, _, _, _, _, _).

:- pred stack_layout__get_next_cell_number(int::out,
	stack_layout_info::in, stack_layout_info::out) is det.

stack_layout__get_next_cell_number(CNum0, LayoutInfo0, LayoutInfo) :-
	LayoutInfo0 = stack_layout_info(A, CNum0, C, D, E, F, G),
	CNum is CNum0 + 1,
	LayoutInfo = stack_layout_info(A, CNum, C, D, E, F, G).

:- pred stack_layout__get_cell_number(int::out,
	stack_layout_info::in, stack_layout_info::out) is det.

stack_layout__get_cell_number(CNum, LayoutInfo, LayoutInfo) :-
	LayoutInfo = stack_layout_info(_, CNum, _, _, _, _, _).

:- pred stack_layout__get_agc_stack_layout(bool::out,
	stack_layout_info::in, stack_layout_info::out) is det.

stack_layout__get_agc_stack_layout(AgcStackLayout, LayoutInfo, LayoutInfo) :-
	LayoutInfo = stack_layout_info(_, _, AgcStackLayout, _, _, _, _).

:- pred stack_layout__get_trace_stack_layout(bool::out,
	stack_layout_info::in, stack_layout_info::out) is det.

stack_layout__get_trace_stack_layout(TraceStackLayout, LayoutInfo,
		LayoutInfo) :-
	LayoutInfo = stack_layout_info(_, _, _, TraceStackLayout, _, _, _).

:- pred stack_layout__get_procid_stack_layout(bool::out,
	stack_layout_info::in, stack_layout_info::out) is det.

stack_layout__get_procid_stack_layout(ProcIdStackLayout, LayoutInfo,
		LayoutInfo) :-
	LayoutInfo = stack_layout_info(_, _, _, _, ProcIdStackLayout, _, _).

:- pred stack_layout__get_cmodules(list(c_module)::out,
	stack_layout_info::in, stack_layout_info::out) is det.

stack_layout__get_cmodules(CModules, LayoutInfo, LayoutInfo) :-
	LayoutInfo = stack_layout_info(_, _, _, _, _, CModules, _).

:- pred stack_layout__get_label_set(set_bbbtree(label)::out,
	stack_layout_info::in, stack_layout_info::out) is det.

stack_layout__get_label_set(StackLayoutLabels, LayoutInfo, LayoutInfo) :-
	LayoutInfo = stack_layout_info(_, _, _, _, _, _, StackLayoutLabels).

:- pred stack_layout__add_cmodule(c_module::in, label::in,
	stack_layout_info::in, stack_layout_info::out) is det.

stack_layout__add_cmodule(CModule, Label, LayoutInfo0, LayoutInfo) :-
	LayoutInfo0 = stack_layout_info(A, B, C, D, E, CModules0,
		StackLayoutLabels0),
	CModules = [CModule | CModules0],
	set_bbbtree__insert(StackLayoutLabels0, Label, StackLayoutLabels),
	LayoutInfo = stack_layout_info(A, B, C, D, E, CModules,
		StackLayoutLabels).

:- pred stack_layout__set_cell_number(int::in,
	stack_layout_info::in, stack_layout_info::out) is det.

stack_layout__set_cell_number(CNum, LayoutInfo0, LayoutInfo) :-
	LayoutInfo0 = stack_layout_info(A, _, C, D, E, F, G),
	LayoutInfo = stack_layout_info(A, CNum, C, D, E, F, G).
