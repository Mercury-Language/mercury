%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2003 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% This module looks for static data structures in create rvals so that
% it can make them global, in the hope of replacing multiple local definitions
% by a single global one.
%
% Processes a list of procedures, and a list of c_modules, intended
% to contain any extra c_data structures the compiler generates.
% Any other contents of the c_modules list will be untouched.

% Main author: zs.

%-----------------------------------------------------------------------------%

:- module ll_backend__llds_common.

:- interface.

:- import_module ll_backend__llds.
:- import_module parse_tree__prog_data. % for module_name

:- import_module bool.

:- import_module list.

:- pred llds_common(module_name::in, bool::in, bool::in,
	list(c_procedure)::in, list(c_procedure)::out,
	list(comp_gen_c_data)::in, list(comp_gen_c_data)::out) is det.

:- implementation.

:- import_module backend_libs__rtti.
:- import_module ll_backend__layout.
:- import_module ll_backend__llds_out.

:- import_module bool, int, assoc_list, counter, set, map, std_util, require.

:- type cell_type_group
	--->	cell_type_group(
			% cell_arg_types		:: list(llds_type)
			cell_type_number 	:: int,
			cell_group_members	:: map(list(rval), data_name)
		).

:- type common_info
	--->	common_info(
			module_name	:: module_name,	% base file name
			unbox_float	:: bool,
			common_data	:: bool,
			cell_counter	:: counter,	% next cell number
			type_counter	:: counter,	% next type number
			cells		:: list(comp_gen_c_data),
			cell_group_map	:: map(list(llds_type),
						cell_type_group)
					% map cell argument types and then cell
					% contents to the id of the common cell
		).

llds_common(BaseName, UnboxFloat, CommonData, Procedures0, Procedures,
		Data0, Data) :-
	map__init(CellMap0),
	Info0 = common_info(BaseName, UnboxFloat, CommonData,
		counter__init(0), counter__init(0), [], CellMap0),
	llds_common__process_procs(Procedures0, Procedures, Info0, Info1),
	llds_common__process_datas(Data0, Data1, Info1, Info),
	RevCommonCells = Info ^ cells,
	list__reverse(RevCommonCells, CommonCells),
	list__append(CommonCells, Data1, Data).

:- pred llds_common__process_create(tag::in, list(maybe(rval))::in,
	create_arg_types::in, rval::out, common_info::in, common_info::out)
	is det.

llds_common__process_create(Tag, MaybeArgs0, ArgTypes0, Rval, !Info) :-
	list__map_foldl(llds_common__process_convert_maybe_rval,
		MaybeArgs0, Args1, !Info),
		% If we have an empty cell, place a dummy field in it,
		% so that the generated C structure isn't empty.
	( Args1 = [] ->
		Args2 = [const(int_const(-1))],
		ArgTypes2 = uniform(yes(integer))
	;
		Args2 = Args1,
		ArgTypes2 = ArgTypes0
	),
	flatten_arg_types(Args2, ArgTypes2, !.Info ^ unbox_float, TypedArgs),
	assoc_list__keys(TypedArgs, Args),
	assoc_list__values(TypedArgs, Types),
	CellGroupMap0 = !.Info ^ cell_group_map,
	( map__search(CellGroupMap0, Types, CellGroup0) ->
		TypeNum = CellGroup0 ^ cell_type_number,
		CellGroup1 = CellGroup0
	;
		TypeNumCounter0 = !.Info ^ type_counter,
		counter__allocate(TypeNum, TypeNumCounter0, TypeNumCounter),
		!:Info = !.Info ^ type_counter := TypeNumCounter,
		CellGroup1 = cell_type_group(TypeNum, map__init)
	),
	MembersMap0 = CellGroup1 ^ cell_group_members,
	ModuleName = !.Info ^ module_name,
	(
		map__search(MembersMap0, Args, DataNamePrime),
		% With --no-common-data, deliberately sabotage the search
		% for DataName in order to ensure that we generate one static
		% structure for reach create rval. This can be useful when
		% comparing the LLDS and MLDS backends.
		!.Info ^ common_data = yes
	->
		DataName = DataNamePrime
	;
		CellNumCounter0 = !.Info ^ cell_counter,
		counter__allocate(CellNum, CellNumCounter0, CellNumCounter),
		!:Info = !.Info ^ cell_counter := CellNumCounter,

		DataName = common(CellNum, TypeNum),
		map__det_insert(MembersMap0, Args, DataName, MembersMap),
		CellGroup = CellGroup1 ^ cell_group_members := MembersMap,
		map__set(CellGroupMap0, Types, CellGroup, CellGroupMap),
		!:Info = !.Info ^ cell_group_map := CellGroupMap,

		Cells0 = !.Info ^ cells,
		Cell = common_data(ModuleName, CellNum, TypeNum, TypedArgs),
		Cells = [Cell | Cells0],
		!:Info = !.Info ^ cells := Cells
	),
	DataConst = data_addr_const(data_addr(ModuleName, DataName)),
	Rval = mkword(Tag, const(DataConst)).

%-----------------------------------------------------------------------------%

:- pred flatten_arg_types(list(rval)::in, create_arg_types::in,
	bool::in, assoc_list(rval, llds_type)::out) is det.

flatten_arg_types(Args, uniform(MaybeType), UnboxFloat, TypedRvals) :-
	flatten_uniform_arg_types(Args, MaybeType, UnboxFloat, TypedRvals).
flatten_arg_types(Args, initial(InitialTypes, RestTypes), UnboxFloat,
		TypedRvals) :-
	flatten_initial_arg_types(Args, InitialTypes, RestTypes, UnboxFloat,
		TypedRvals).
flatten_arg_types(Args, none, _, []) :-
	require(unify(Args, []), "too many args for specified arg types").

:- pred flatten_uniform_arg_types(list(rval)::in, maybe(llds_type)::in,
	bool::in, assoc_list(rval, llds_type)::out) is det.

flatten_uniform_arg_types([], _, _, []).
flatten_uniform_arg_types([Rval | Rvals], MaybeType, UnboxFloat,
		[Rval - Type | TypedRvals]) :-
	llds_arg_type(Rval, MaybeType, UnboxFloat, Type),
	flatten_uniform_arg_types(Rvals, MaybeType, UnboxFloat, TypedRvals).

:- pred flatten_initial_arg_types(list(rval)::in, initial_arg_types::in,
	create_arg_types::in, bool::in, assoc_list(rval, llds_type)::out)
	is det.

flatten_initial_arg_types(Args, [], RestTypes, UnboxFloat, TypedRvals) :-
	flatten_arg_types(Args, RestTypes, UnboxFloat, TypedRvals).
flatten_initial_arg_types(Args, [N - MaybeType | InitTypes], RestTypes,
		UnboxFloat, TypedRvals) :-
	flatten_initial_arg_types_2(Args, N, MaybeType, InitTypes, RestTypes,
		UnboxFloat, TypedRvals).

:- pred flatten_initial_arg_types_2(list(rval)::in, int::in,
	maybe(llds_type)::in, initial_arg_types::in, create_arg_types::in,
	bool::in, assoc_list(rval, llds_type)::out) is det.

flatten_initial_arg_types_2([], N, _, _, _, _, []) :-
	require(unify(N, 0), "not enough args for specified arg types").
flatten_initial_arg_types_2([Rval | Rvals], N, MaybeType, InitTypes,
		RestTypes, UnboxFloat, TypedRvals) :-
	( N = 0 ->
		flatten_initial_arg_types([Rval | Rvals], InitTypes,
			RestTypes, UnboxFloat, TypedRvals)
	;
		llds_arg_type(Rval, MaybeType, UnboxFloat, Type),
		flatten_initial_arg_types_2(Rvals, N - 1, MaybeType,
			InitTypes, RestTypes, UnboxFloat,
			TypedRvalsTail),
		TypedRvals = [Rval - Type | TypedRvalsTail]
	).

	% Given an rval, figure out the type it would have as an argument,
	% if it is not explicitly specified.

:- pred llds_arg_type(rval::in, maybe(llds_type)::in, bool::in,
	llds_type::out) is det.

llds_arg_type(Rval, MaybeType, UnboxFloat, Type) :-
	( MaybeType = yes(SpecType) ->
		Type = SpecType
	;
		rval_type_as_arg(Rval, UnboxFloat, Type)
	).

	% Given an rval, figure out the type it would have as
	% an argument.  Normally that's the same as its usual type;
	% the exception is that for boxed floats, the type is data_ptr
	% (i.e. the type of the boxed value) rather than float
	% (the type of the unboxed value).

:- pred rval_type_as_arg(rval::in, bool::in, llds_type::out) is det.

rval_type_as_arg(Rval, UnboxFloat, Type) :-
	llds__rval_type(Rval, Type0),
	( Type0 = float, UnboxFloat = no ->
		Type = data_ptr
	;
		Type = Type0
	).

%-----------------------------------------------------------------------------%

	% The rest of the file is quite boring. Its only job is to traverse
	% the various components of c_modules to arrive at the creates.

:- pred llds_common__process_datas(list(comp_gen_c_data)::in,
	list(comp_gen_c_data)::out, common_info::in, common_info::out) is det.

llds_common__process_datas([], [], !Info).
llds_common__process_datas([Data0 | Datas0], [Data | Datas], !Info) :-
	llds_common__process_data(Data0, Data, !Info),
	llds_common__process_datas(Datas0, Datas, !Info).

:- pred llds_common__process_data(comp_gen_c_data::in, comp_gen_c_data::out,
	common_info::in, common_info::out) is det.

llds_common__process_data(common_data(Name, CellNum, TypeNum, ArgsTypes0),
		common_data(Name, CellNum, TypeNum, ArgsTypes), !Info) :-
	list__map_foldl(llds_common__process_rval_type, ArgsTypes0, ArgsTypes,
		!Info).
llds_common__process_data(rtti_data(RttiData), rtti_data(RttiData), !Info).
llds_common__process_data(layout_data(LayoutData0), layout_data(LayoutData),
		!Info) :-
	llds_common__process_layout_data(LayoutData0, LayoutData, !Info).

:- pred llds_common__process_layout_data(layout_data::in, layout_data::out,
	common_info::in, common_info::out) is det.

llds_common__process_layout_data(LayoutData0, LayoutData, !Info) :-
	LayoutData0 = label_layout_data(Label, ProcLayoutName,
		MaybePort, MaybeIsHidden, MaybeGoalPath, MaybeVarInfo0),
	(
		MaybeVarInfo0 = no,
		LayoutData = LayoutData0
	;
		MaybeVarInfo0 = yes(VarInfo0),
		VarInfo0 = label_var_info(EncodedCount,
			LocnsTypes0, VarNums0, TypeParams0),
		llds_common__process_rval(LocnsTypes0, LocnsTypes, !Info),
		llds_common__process_rval(VarNums0, VarNums, !Info),
		llds_common__process_rval(TypeParams0, TypeParams, !Info),
		VarInfo = label_var_info(EncodedCount,
			LocnsTypes, VarNums, TypeParams),
		MaybeVarInfo = yes(VarInfo),
		LayoutData = label_layout_data(Label, ProcLayoutName,
			MaybePort, MaybeIsHidden, MaybeGoalPath, MaybeVarInfo)
	).
llds_common__process_layout_data(LayoutData0, LayoutData, !Info) :-
	LayoutData0 = proc_layout_data(ProcLabel, Traversal, MaybeRest0),
	(
		MaybeRest0 = no_proc_id,
		LayoutData = LayoutData0
	;
		MaybeRest0 = proc_id_only,
		LayoutData = LayoutData0
	;
		MaybeRest0 = proc_id_and_exec_trace(Exec0),
		llds_common__process_exec_trace(Exec0, Exec, !Info),
		MaybeRest = proc_id_and_exec_trace(Exec),
		LayoutData = proc_layout_data(ProcLabel, Traversal, MaybeRest)
	).
llds_common__process_layout_data(LayoutData0, LayoutData, !Info) :-
	LayoutData0 = closure_proc_id_data(_, _, _, _, _, _, _),
	LayoutData = LayoutData0.
llds_common__process_layout_data(LayoutData0, LayoutData, !Info) :-
	LayoutData0 = module_layout_data(_, _, _, _, _, _, _),
	LayoutData = LayoutData0.
llds_common__process_layout_data(LayoutData0, LayoutData, !Info) :-
	LayoutData0 = proc_static_data(_, _, _, _, _),
	LayoutData = LayoutData0.
llds_common__process_layout_data(LayoutData0, LayoutData, !Info) :-
	LayoutData0 = table_io_decl_data(RttiProcLabel, Kind, NumPTIs,
		PTIVector0, TVarLocnMap0),
	llds_common__process_rval(PTIVector0, PTIVector, !Info),
	llds_common__process_rval(TVarLocnMap0, TVarLocnMap, !Info),
	LayoutData = table_io_decl_data(RttiProcLabel, Kind, NumPTIs,
		PTIVector, TVarLocnMap).
llds_common__process_layout_data(LayoutData0, LayoutData, !Info) :-
	LayoutData0 = table_gen_data(RttiProcLabel, NumInputs, NumOutputs,
		Steps, PTIVector0, TVarLocnMap0),
	llds_common__process_rval(PTIVector0, PTIVector, !Info),
	llds_common__process_rval(TVarLocnMap0, TVarLocnMap, !Info),
	LayoutData = table_gen_data(RttiProcLabel, NumInputs, NumOutputs,
		Steps, PTIVector, TVarLocnMap).

:- pred llds_common__process_exec_trace(proc_layout_exec_trace::in,
	proc_layout_exec_trace::out, common_info::in, common_info::out) is det.

llds_common__process_exec_trace(ExecTrace0, ExecTrace, Info0, Info) :-
	ExecTrace0 = proc_layout_exec_trace(CallLabel, MaybeProcBody0,
		MaybeTableIoDecl, HeadVarNums, VarNames, MaxVarNum, MaxReg,
		MaybeFromFullSlot, MaybeIoSeqSlot, MaybeTrailSlot,
		MaybeMaxfrSlot, EvalMethod, MaybeCallTableSlot),
	llds_common__process_maybe_rval(MaybeProcBody0, MaybeProcBody,
		Info0, Info),
	ExecTrace = proc_layout_exec_trace(CallLabel, MaybeProcBody,
		MaybeTableIoDecl, HeadVarNums, VarNames, MaxVarNum, MaxReg,
		MaybeFromFullSlot, MaybeIoSeqSlot, MaybeTrailSlot,
		MaybeMaxfrSlot, EvalMethod, MaybeCallTableSlot).

:- pred llds_common__process_procs(list(c_procedure)::in,
	list(c_procedure)::out, common_info::in, common_info::out) is det.

llds_common__process_procs([], [], Info, Info).
llds_common__process_procs([Proc0 | Procs0], [Proc | Procs], Info0, Info) :-
	llds_common__process_proc(Proc0, Proc, Info0, Info1),
	llds_common__process_procs(Procs0, Procs, Info1, Info).

:- pred llds_common__process_proc(c_procedure::in, c_procedure::out,
	common_info::in, common_info::out) is det.

llds_common__process_proc(Proc0, Proc, Info0, Info) :-
	Proc0 = c_procedure(Name, Arity, PredProcId, Instrs0,
		ProcLabel, N, Reconstruction),
	llds_common__process_instrs(Instrs0, Instrs, Info0, Info),
	Proc = c_procedure(Name, Arity, PredProcId, Instrs,
		ProcLabel, N, Reconstruction).

:- pred llds_common__process_instrs(list(instruction)::in,
	list(instruction)::out, common_info::in, common_info::out) is det.

llds_common__process_instrs([], [], Info, Info).
llds_common__process_instrs([Uinstr0 - Comment | Instrs0],
		[Uinstr - Comment | Instrs], Info0, Info) :-
	llds_common__process_instr(Uinstr0, Uinstr, Info0, Info1),
	llds_common__process_instrs(Instrs0, Instrs, Info1, Info).

:- pred llds_common__process_instr(instr::in, instr::out,
	common_info::in, common_info::out) is det.

llds_common__process_instr(Instr0, Instr, Info0, Info) :-
	(
		Instr0 = comment(_),
		Instr = Instr0,
		Info = Info0
	;
		Instr0 = livevals(_),
		Instr = Instr0,
		Info = Info0
	;
		Instr0 = block(NR, NF, Instrs0),
		llds_common__process_instrs(Instrs0, Instrs, Info0, Info),
		Instr = block(NR, NF, Instrs)
	;
		Instr0 = assign(Lval0, Rval0),
		llds_common__process_lval(Lval0, Lval, Info0, Info1),
		llds_common__process_rval(Rval0, Rval, Info1, Info),
		Instr = assign(Lval, Rval)
	;
		Instr0 = call(_, _, _, _, _, _),
		Instr = Instr0,
		Info = Info0
	;
		Instr0 = mkframe(_, _),
		Instr = Instr0,
		Info = Info0
	;
		Instr0 = label(_),
		Instr = Instr0,
		Info = Info0
	;
		Instr0 = goto(_),
		Instr = Instr0,
		Info = Info0
	;
		Instr0 = computed_goto(Rval0, Labels),
		llds_common__process_rval(Rval0, Rval, Info0, Info),
		Instr = computed_goto(Rval, Labels)
	;
		Instr0 = c_code(_, _),
		Instr = Instr0,
		Info = Info0
	;
		Instr0 = if_val(Rval0, Target),
		llds_common__process_rval(Rval0, Rval, Info0, Info),
		Instr = if_val(Rval, Target)
	;
		Instr0 = incr_hp(Lval0, MaybeTag, Rval0, Msg),
		llds_common__process_lval(Lval0, Lval, Info0, Info1),
		llds_common__process_rval(Rval0, Rval, Info1, Info),
		Instr = incr_hp(Lval, MaybeTag, Rval, Msg)
	;
		Instr0 = mark_hp(Lval0),
		llds_common__process_lval(Lval0, Lval, Info0, Info),
		Instr = mark_hp(Lval)
	;
		Instr0 = restore_hp(Rval0),
		llds_common__process_rval(Rval0, Rval, Info0, Info),
		Instr = restore_hp(Rval)
	;
		Instr0 = free_heap(Rval0),
		llds_common__process_rval(Rval0, Rval, Info0, Info),
		Instr = free_heap(Rval)
	;
		Instr0 = store_ticket(Lval0),
		llds_common__process_lval(Lval0, Lval, Info0, Info),
		Instr = store_ticket(Lval)
	;
		Instr0 = reset_ticket(Rval0, Reason),
		llds_common__process_rval(Rval0, Rval, Info0, Info),
		Instr = reset_ticket(Rval, Reason)
	;
		Instr0 = discard_ticket,
		Instr = Instr0,
		Info = Info0
	;
		Instr0 = prune_ticket,
		Instr = Instr0,
		Info = Info0
	;
		Instr0 = mark_ticket_stack(Lval0),
		llds_common__process_lval(Lval0, Lval, Info0, Info),
		Instr = mark_ticket_stack(Lval)
	;
		Instr0 = prune_tickets_to(Rval0),
		llds_common__process_rval(Rval0, Rval, Info0, Info),
		Instr = prune_tickets_to(Rval)
	;
		Instr0 = incr_sp(_, _),
		Instr = Instr0,
		Info = Info0
	;
		Instr0 = decr_sp(_),
		Instr = Instr0,
		Info = Info0
	;
		Instr0 = init_sync_term(Lval0, NumBranches),
		llds_common__process_lval(Lval0, Lval, Info0, Info),
		Instr = init_sync_term(Lval, NumBranches)
	;
		Instr0 = fork(_, _, _),
		Instr = Instr0,
		Info = Info0
	;
		Instr0 = join_and_terminate(Lval0),
		llds_common__process_lval(Lval0, Lval, Info0, Info),
		Instr = join_and_terminate(Lval)
	;
		Instr0 = join_and_continue(Lval0, ContLabel),
		llds_common__process_lval(Lval0, Lval, Info0, Info),
		Instr = join_and_continue(Lval, ContLabel)
	;
		Instr0 = pragma_c(A, Components0, C, D, E, F, G, H),
		list__map_foldl(llds_common__process_pragma_c_component,
			Components0, Components, Info0, Info),
		Instr = pragma_c(A, Components, C, D, E, F, G, H)
	).

:- pred llds_common__process_pragma_c_component(pragma_c_component::in,
	pragma_c_component::out, common_info::in, common_info::out) is det.

llds_common__process_pragma_c_component(Component0, Component, !Info) :-
	(
		Component0 = pragma_c_inputs(Inputs0),
		list__map_foldl(llds_common__process_pragma_c_input,
			Inputs0, Inputs, !Info),
		Component = pragma_c_inputs(Inputs)
	;
		Component0 = pragma_c_outputs(Outputs0),
		list__map_foldl(llds_common__process_pragma_c_output,
			Outputs0, Outputs, !Info),
		Component = pragma_c_outputs(Outputs)
	;
		Component0 = pragma_c_user_code(_, _),
		Component = Component0
	;
		Component0 = pragma_c_raw_code(Code, CCodeLiveLvals0),
		(
			CCodeLiveLvals0 = no_live_lvals_info,
			CCodeLiveLvals = no_live_lvals_info
		;
			CCodeLiveLvals0 = live_lvals_info(Lvals0),
			set__map_fold(llds_common__process_lval,
				Lvals0, Lvals, !Info),
			CCodeLiveLvals = live_lvals_info(Lvals)
		),
		Component = pragma_c_raw_code(Code, CCodeLiveLvals)
	;
		Component0 = pragma_c_fail_to(_),
		Component = Component0
	;
		Component0 = pragma_c_noop,
		Component = Component0
	).

:- pred llds_common__process_pragma_c_input(pragma_c_input::in,
	pragma_c_input::out, common_info::in, common_info::out) is det.

llds_common__process_pragma_c_input(Input0, Input, Info0, Info) :-
	Input0 = pragma_c_input(Name, Type, Rval0, MaybeCType),
	llds_common__process_rval(Rval0, Rval, Info0, Info),
	Input = pragma_c_input(Name, Type, Rval, MaybeCType).

:- pred llds_common__process_pragma_c_output(pragma_c_output::in,
	pragma_c_output::out, common_info::in, common_info::out) is det.

llds_common__process_pragma_c_output(Output0, Output, Info0, Info) :-
	Output0 = pragma_c_output(Lval0, Type, Name, MaybeCType),
	llds_common__process_lval(Lval0, Lval, Info0, Info),
	Output = pragma_c_output(Lval, Type, Name, MaybeCType).

:- pred llds_common__process_rval_type(pair(rval, llds_type)::in, pair(rval,
	llds_type)::out, common_info::in, common_info::out) is det.

llds_common__process_rval_type(Rval0 - Type, Rval - Type, !Info) :-
	llds_common__process_rval(Rval0, Rval, !Info).

:- pred llds_common__process_rval(rval::in, rval::out,
	common_info::in, common_info::out) is det.

llds_common__process_rval(Rval0, Rval, Info0, Info) :-
	(
		Rval0 = lval(Lval0),
		llds_common__process_lval(Lval0, Lval, Info0, Info),
		Rval = lval(Lval)
	;
		Rval0 = var(_),
		error("var rval found in llds_common__process_rval")
	;
		Rval0 = create(Tag, Args, ArgTypes, StatDyn,
				_LabelNo, _Msg, _Reuse),
		( StatDyn \= must_be_dynamic ->
			llds_common__process_create(Tag, Args, ArgTypes, Rval,
				Info0, Info)
		;
			Rval = Rval0,
			Info = Info0
		)
	;
		Rval0 = mkword(Tag, SubRval0),
		llds_common__process_rval(SubRval0, SubRval, Info0, Info),
		Rval = mkword(Tag, SubRval)
	;
		Rval0 = const(_),
		Rval = Rval0,
		Info = Info0
	;
		Rval0 = unop(Unop, SubRval0),
		llds_common__process_rval(SubRval0, SubRval, Info0, Info),
		Rval = unop(Unop, SubRval)
	;
		Rval0 = binop(Binop, Left0, Right0),
		llds_common__process_rval(Left0, Left, Info0, Info1),
		llds_common__process_rval(Right0, Right, Info1, Info),
		Rval = binop(Binop, Left, Right)
	;
		Rval0 = mem_addr(MemRef0),
		llds_common__process_mem_ref(MemRef0, MemRef, Info0, Info),
		Rval = mem_addr(MemRef)
	).

:- pred llds_common__process_mem_ref(mem_ref::in, mem_ref::out,
	common_info::in, common_info::out) is det.

llds_common__process_mem_ref(stackvar_ref(N), stackvar_ref(N), Info, Info).
llds_common__process_mem_ref(framevar_ref(N), framevar_ref(N), Info, Info).
llds_common__process_mem_ref(heap_ref(Rval0, Tag, N), heap_ref(Rval, Tag, N),
		Info0, Info) :-
	llds_common__process_rval(Rval0, Rval, Info0, Info).

:- pred llds_common__process_rvals(list(rval)::in, list(rval)::out,
	common_info::in, common_info::out) is det.

llds_common__process_rvals([], [], Info, Info).
llds_common__process_rvals([Rval0 | Rvals0], [Rval | Rvals], Info0, Info) :-
	llds_common__process_rval(Rval0, Rval, Info0, Info1),
	llds_common__process_rvals(Rvals0, Rvals, Info1, Info).

:- pred llds_common__process_convert_maybe_rval(maybe(rval)::in, rval::out,
	common_info::in, common_info::out) is det.

llds_common__process_convert_maybe_rval(MaybeRval0, Rval, !Info) :-
	(
		MaybeRval0 = yes(Rval0),
		llds_common__process_rval(Rval0, Rval, !Info)
	;
		MaybeRval0 = no,
		Rval = const(int_const(0))
	).

:- pred llds_common__process_maybe_rval(maybe(rval)::in, maybe(rval)::out,
	common_info::in, common_info::out) is det.

llds_common__process_maybe_rval(MaybeRval0, MaybeRval, !Info) :-
	(
		MaybeRval0 = yes(Rval0),
		llds_common__process_rval(Rval0, Rval, !Info),
		MaybeRval = yes(Rval)
	;
		MaybeRval0 = no,
		MaybeRval = no
	).

:- pred llds_common__process_lval(lval::in, lval::out,
	common_info::in, common_info::out) is det.

llds_common__process_lval(Lval0, Lval, !Info) :-
	(
		Lval0 = reg(_, _),
		Lval = Lval0
	;
		Lval0 = succip,
		Lval = Lval0
	;
		Lval0 = maxfr,
		Lval = Lval0
	;
		Lval0 = curfr,
		Lval = Lval0
	;
		Lval0 = hp,
		Lval = Lval0
	;
		Lval0 = sp,
		Lval = Lval0
	;
		Lval0 = temp(_, _),
		Lval = Lval0
	;
		Lval0 = stackvar(_),
		Lval = Lval0
	;
		Lval0 = framevar(_),
		Lval = Lval0
	;
		Lval0 = succip(Rval0),
		llds_common__process_rval(Rval0, Rval, !Info),
		Lval = succip(Rval)
	;
		Lval0 = redoip(Rval0),
		llds_common__process_rval(Rval0, Rval, !Info),
		Lval = redoip(Rval)
	;
		Lval0 = redofr(Rval0),
		llds_common__process_rval(Rval0, Rval, !Info),
		Lval = redofr(Rval)
	;
		Lval0 = succfr(Rval0),
		llds_common__process_rval(Rval0, Rval, !Info),
		Lval = succfr(Rval)
	;
		Lval0 = prevfr(Rval0),
		llds_common__process_rval(Rval0, Rval, !Info),
		Lval = prevfr(Rval)
	;
		Lval0 = field(MaybeTag, Base0, Offset0),
		llds_common__process_rval(Base0, Base, !Info),
		llds_common__process_rval(Offset0, Offset, !Info),
		Lval = field(MaybeTag, Base, Offset)
	;
		Lval0 = mem_ref(Rval0),
		llds_common__process_rval(Rval0, Rval, !Info),
		Lval = mem_ref(Rval)
	;
		Lval0 = lvar(_),
		error("llds_common__process_lval: lvar")
	).
