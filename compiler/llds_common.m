%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2002 The University of Melbourne.
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
:- import_module list.

:- pred llds_common(list(c_procedure)::in, list(comp_gen_c_data)::in,
	module_name::in, list(c_procedure)::out, list(comp_gen_c_data)::out)
	is det.

:- implementation.

:- import_module backend_libs__rtti, ll_backend__layout, ll_backend__llds_out.
:- import_module bool, int, assoc_list, map, std_util, require.

:- type cell_info
	--->	cell_info(
			int		% what is the number of the cell?
		).

:- type cell_content	==	pair(list(maybe(rval)), create_arg_types).
:- type cell_map	==	map(cell_content, cell_info).
:- type cell_list	==	assoc_list(cell_content, cell_info).

:- type common_info
	--->	common_info(
			module_name,	% base file name
			int,		% next cell number
			cell_map
					% map cell contents (including types)
					% to cell declaration information
		).

llds_common(Procedures0, Data0, BaseName, Procedures, Data) :-
	map__init(CellMap0),
	Info0 = common_info(BaseName, 0, CellMap0),
	llds_common__process_procs(Procedures0, Procedures, Info0, Info1),
	llds_common__process_datas(Data0, Data1, Info1, Info),
	Info = common_info(_, _, CellMap),
	map__to_assoc_list(CellMap, CellPairs0),
	list__sort(lambda([CellPairA::in, CellPairB::in, Compare::out] is det, 
		(
			CellPairA = _ - cell_info(ANum),
			CellPairB = _ - cell_info(BNum),
			compare(Compare, ANum, BNum)
		)), CellPairs0, CellPairs),
	llds_common__cell_pairs_to_modules(CellPairs, BaseName, CommonData),
	list__append(CommonData, Data1, Data).

:- pred llds_common__cell_pairs_to_modules(cell_list::in, module_name::in,
	list(comp_gen_c_data)::out) is det.

llds_common__cell_pairs_to_modules([], _, []).
llds_common__cell_pairs_to_modules([CellContent - CellInfo | CellPairs],
		BaseName, [Common | Commons]) :-
	CellInfo = cell_info(VarNum),
	CellContent = Args0 - ArgTypes0,
		
		% If we have an empty data structure place a dummy field
		% in it, so that the generated C structure isn't empty.
	( Args0 = [] ->
		Args = [yes(const(int_const(-1)))],
		ArgTypes = uniform(yes(integer))
	;
		Args = Args0,
		ArgTypes = ArgTypes0
	),

	Common = comp_gen_c_data(BaseName, common(VarNum), no,
		Args, ArgTypes, []),
	llds_common__cell_pairs_to_modules(CellPairs, BaseName, Commons).

:- pred llds_common__process_create(tag::in, list(maybe(rval))::in,
	create_arg_types::in, rval::out, common_info::in, common_info::out)
	is det.

llds_common__process_create(Tag, Args0, ArgTypes, Rval, Info0, Info) :-
	llds_common__process_maybe_rvals(Args0, Args, Info0, Info1),
	Info1 = common_info(BaseName, NextCell0, CellMap0),
	( map__search(CellMap0, Args - ArgTypes, CellInfo0) ->
		CellInfo0 = cell_info(VarNum),
		DataConst = data_addr_const(
			data_addr(BaseName, common(VarNum))),
		Rval = mkword(Tag, const(DataConst)),
		Info = Info1
	;
		DataConst = data_addr_const(
			data_addr(BaseName, common(NextCell0))),
		Rval = mkword(Tag, const(DataConst)),
		CellInfo = cell_info(NextCell0),
		NextCell is NextCell0 + 1,
		map__det_insert(CellMap0, Args - ArgTypes, CellInfo, CellMap),
		Info = common_info(BaseName, NextCell, CellMap)
	).

%-----------------------------------------------------------------------------%

	% The rest of the file is quite boring. Its only job is to traverse
	% the various components of c_modules to arrive at the creates.

:- pred llds_common__process_datas(list(comp_gen_c_data)::in,
	list(comp_gen_c_data)::out, common_info::in, common_info::out) is det.

llds_common__process_datas([], [], Info, Info).
llds_common__process_datas([Data0 | Datas0], [Data | Datas], Info0, Info) :-
	llds_common__process_data(Data0, Data, Info0, Info1),
	llds_common__process_datas(Datas0, Datas, Info1, Info).

:- pred llds_common__process_data(comp_gen_c_data::in, comp_gen_c_data::out,
	common_info::in, common_info::out) is det.

llds_common__process_data(
		comp_gen_c_data(Name, DataName, Export, Args0, ArgTypes, Refs),
		comp_gen_c_data(Name, DataName, Export, Args, ArgTypes, Refs),
		Info0, Info) :-
	llds_common__process_maybe_rvals(Args0, Args, Info0, Info).
llds_common__process_data(rtti_data(RttiData), rtti_data(RttiData),
		Info, Info).
llds_common__process_data(layout_data(LayoutData0), layout_data(LayoutData),
		Info0, Info) :-
	llds_common__process_layout_data(LayoutData0, LayoutData, Info0, Info).

:- pred llds_common__process_layout_data(layout_data::in, layout_data::out,
	common_info::in, common_info::out) is det.

llds_common__process_layout_data(LayoutData0, LayoutData, Info0, Info) :-
	LayoutData0 = label_layout_data(Label, ProcLayoutName,
		MaybePort, MaybeGoalPath, MaybeVarInfo0),
	(
		MaybeVarInfo0 = no,
		LayoutData = LayoutData0,
		Info = Info0
	;
		MaybeVarInfo0 = yes(VarInfo0),
		VarInfo0 = label_var_info(EncodedCount,
			LocnsTypes0, VarNums0, TypeParams0),
		llds_common__process_rval(LocnsTypes0, LocnsTypes,
			Info0, Info1),
		llds_common__process_rval(VarNums0, VarNums,
			Info1, Info2),
		llds_common__process_rval(TypeParams0, TypeParams,
			Info2, Info),
		VarInfo = label_var_info(EncodedCount,
			LocnsTypes, VarNums, TypeParams),
		MaybeVarInfo = yes(VarInfo),
		LayoutData = label_layout_data(Label, ProcLayoutName,
			MaybePort, MaybeGoalPath, MaybeVarInfo)
	).
llds_common__process_layout_data(LayoutData0, LayoutData, Info0, Info) :-
	LayoutData0 = proc_layout_data(ProcLabel, Traversal, MaybeRest0),
	(
		MaybeRest0 = no_proc_id,
		LayoutData = LayoutData0,
		Info = Info0
	;
		MaybeRest0 = proc_id_only,
		LayoutData = LayoutData0,
		Info = Info0
	;
		MaybeRest0 = proc_id_and_exec_trace(Exec0),
		llds_common__process_exec_trace(Exec0, Exec, Info0, Info),
		MaybeRest = proc_id_and_exec_trace(Exec),
		LayoutData = proc_layout_data(ProcLabel, Traversal, MaybeRest)
	).
llds_common__process_layout_data(LayoutData0, LayoutData, Info, Info) :-
	LayoutData0 = closure_proc_id_data(_, _, _, _, _, _, _),
	LayoutData = LayoutData0.
llds_common__process_layout_data(LayoutData0, LayoutData, Info, Info) :-
	LayoutData0 = module_layout_data(_, _, _, _, _, _),
	LayoutData = LayoutData0.
llds_common__process_layout_data(LayoutData0, LayoutData, Info, Info) :-
	LayoutData0 = proc_static_data(_, _, _, _, _),
	LayoutData = LayoutData0.
llds_common__process_layout_data(LayoutData0, LayoutData, Info0, Info) :-
	LayoutData0 = table_io_decl_data(RttiProcLabel, Kind, NumPTIs,
		PTIVector0, TVarLocnMap0),
	llds_common__process_rval(PTIVector0, PTIVector, Info0, Info1),
	llds_common__process_rval(TVarLocnMap0, TVarLocnMap, Info1, Info),
	LayoutData = table_io_decl_data(RttiProcLabel, Kind, NumPTIs,
		PTIVector, TVarLocnMap).

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
		Instr0 = assign(Lval, Rval0),
		llds_common__process_rval(Rval0, Rval, Info0, Info),
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
		% unlikely to find anything to share, but why not try?
		Instr0 = computed_goto(Rval0, Labels),
		llds_common__process_rval(Rval0, Rval, Info0, Info),
		Instr = computed_goto(Rval, Labels)
	;
		Instr0 = c_code(_, _),
		Instr = Instr0,
		Info = Info0
	;
		% unlikely to find anything to share, but why not try?
		Instr0 = if_val(Rval0, Target),
		llds_common__process_rval(Rval0, Rval, Info0, Info),
		Instr = if_val(Rval, Target)
	;
		% unlikely to find anything to share, but why not try?
		Instr0 = incr_hp(Lval, MaybeTag, Rval0, Msg),
		llds_common__process_rval(Rval0, Rval, Info0, Info),
		Instr = incr_hp(Lval, MaybeTag, Rval, Msg)
	;
		Instr0 = mark_hp(_),
		Instr = Instr0,
		Info = Info0
	;
		% unlikely to find anything to share, but why not try?
		Instr0 = restore_hp(Rval0),
		llds_common__process_rval(Rval0, Rval, Info0, Info),
		Instr = restore_hp(Rval)
	;
		Instr0 = free_heap(_),
		Instr = Instr0,
		Info = Info0
	;
		Instr0 = store_ticket(_),
		Instr = Instr0,
		Info = Info0
	;
		% unlikely to find anything to share, but why not try?
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
		Instr0 = mark_ticket_stack(_),
		Instr = Instr0,
		Info = Info0
	;
		Instr0 = prune_tickets_to(_),
		Instr = Instr0,
		Info = Info0
	;
		Instr0 = incr_sp(_, _),
		Instr = Instr0,
		Info = Info0
	;
		Instr0 = decr_sp(_),
		Instr = Instr0,
		Info = Info0
	;
		Instr0 = init_sync_term(_, _),
		Instr = Instr0,
		Info = Info0
	;
		Instr0 = fork(_, _, _),
		Instr = Instr0,
		Info = Info0
	;
		Instr0 = join_and_terminate(_),
		Instr = Instr0,
		Info = Info0
	;
		Instr0 = join_and_continue(_, _),
		Instr = Instr0,
		Info = Info0
	;
		Instr0 = pragma_c(_, _, _, _, _, _, _, _),
		Instr = Instr0,
		Info = Info0
	).

:- pred llds_common__process_rval(rval::in, rval::out,
	common_info::in, common_info::out) is det.

llds_common__process_rval(Rval0, Rval, Info0, Info) :-
	(
		Rval0 = lval(_),
		Rval = Rval0,
		Info = Info0
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

:- pred llds_common__process_maybe_rval(maybe(rval)::in,
	maybe(rval)::out, common_info::in, common_info::out) is det.

llds_common__process_maybe_rval(MaybeRval0, MaybeRval, Info0, Info) :-
	(
		MaybeRval0 = yes(Rval0),
		llds_common__process_rval(Rval0, Rval, Info0, Info),
		MaybeRval = yes(Rval)
	;
		MaybeRval0 = no,
		MaybeRval = no,
		Info = Info0
	).

:- pred llds_common__process_maybe_rvals(list(maybe(rval))::in,
	list(maybe(rval))::out, common_info::in, common_info::out) is det.

llds_common__process_maybe_rvals([], [], Info, Info).
llds_common__process_maybe_rvals([MaybeRval0 | MaybeRvals0],
		[MaybeRval | MaybeRvals], Info0, Info) :-
	llds_common__process_maybe_rval(MaybeRval0, MaybeRval, Info0, Info1),
	llds_common__process_maybe_rvals(MaybeRvals0, MaybeRvals, Info1, Info).
