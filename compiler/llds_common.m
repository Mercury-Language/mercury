%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
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

:- module llds_common.

:- interface.

:- import_module llds.

:- pred llds_common(list(c_procedure), list(c_module), string, 
	list(c_procedure), list(c_module), list(c_module)).
:- mode llds_common(in, in, in, out, out, out) is det.

:- implementation.

:- import_module llds_out.
:- import_module bool, int, list, assoc_list, map, std_util, require.

:- type cell_info
	--->	cell_info(
			int		% what is the number of the cell?
		).

:- type common_info
	--->	common_info(
			string,		% base file name
			int,		% next cell number
			map(list(maybe(rval)), cell_info)
					% map cell contents to cell declaration
					% information
		).

llds_common(Procedures0, Data0, BaseName, Procedures, Data, DataModules) :-
	map__init(CellMap0),
	Info0 = common_info(BaseName, 0, CellMap0),
	llds_common__process_procs(Procedures0, Info0, Info1, Procedures),
	llds_common__process_modules(Data0, Info1, Info, Data),
	Info = common_info(_, _, CellMap),
	map__to_assoc_list(CellMap, CellPairs0),
	list__sort(lambda([CellPairA::in, CellPairB::in, Compare::out] is det, 
		(
			CellPairA = _ - cell_info(ANum),
			CellPairB = _ - cell_info(BNum),
			compare(Compare, ANum, BNum)
		)), CellPairs0, CellPairs),
	llds_common__cell_pairs_to_modules(CellPairs, BaseName, DataModules).

:- pred llds_common__cell_pairs_to_modules(
	assoc_list(list(maybe(rval)), cell_info), string, list(c_module)).
:- mode llds_common__cell_pairs_to_modules(in, in, out) is det.

llds_common__cell_pairs_to_modules([], _, []).
llds_common__cell_pairs_to_modules([Args - CellInfo | CellPairs], BaseName,
		[Module | Modules]) :-
	CellInfo = cell_info(VarNum),
	Module = c_data(BaseName, common(VarNum), no, Args, []),
	llds_common__cell_pairs_to_modules(CellPairs, BaseName, Modules).

:- pred llds_common__process_create(tag, list(maybe(rval)),
	common_info, common_info, rval).
:- mode llds_common__process_create(in, in, in, out, out) is det.

llds_common__process_create(Tag, Args0, Info0, Info, Rval) :-
	llds_common__process_maybe_rvals(Args0, Info0, Info1, Args),
	Info1 = common_info(BaseName, NextCell0, CellMap0),
	( map__search(CellMap0, Args, CellInfo0) ->
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
		map__det_insert(CellMap0, Args, CellInfo, CellMap),
		Info = common_info(BaseName, NextCell, CellMap)
	).

%-----------------------------------------------------------------------------%

	% The rest of the file is quite boring. Its only job is to traverse
	% the various components of c_modules to arrive at the creates.


	% Only c_data elements are processed, all other elements are ignored.
	
:- pred llds_common__process_modules(list(c_module), common_info, common_info,
	list(c_module)).
:- mode llds_common__process_modules(in, in, out, out) is det.

llds_common__process_modules([], Info, Info, []).
llds_common__process_modules([Module0 | Modules0], Info0, Info, 
		[Module | Modules]) :-
	llds_common__process_module(Module0, Info0, Info1, Module),
	llds_common__process_modules(Modules0, Info1, Info, Modules).

:- pred llds_common__process_module(c_module, common_info, common_info,
	c_module).
:- mode llds_common__process_module(in, in, out, out) is det.

llds_common__process_module(c_module(Name, Ps), Info, Info, c_module(Name, Ps)).
llds_common__process_module(c_code(Cde, Ctxt), Info, Info, c_code(Cde, Ctxt)).
llds_common__process_module(c_export(Exports), Info, Info, c_export(Exports)).
llds_common__process_module(
		c_data(Name, DataName, Export, Args0, Refs), Info0, 
		Info, c_data(Name, DataName, Export, Args, Refs)) :-
	llds_common__process_maybe_rvals(Args0, Info0, Info, Args).

:- pred llds_common__process_procs(list(c_procedure), common_info, common_info,
	list(c_procedure)).
:- mode llds_common__process_procs(in, in, out, out) is det.

llds_common__process_procs([], Info, Info, []).
llds_common__process_procs([Proc0 | Procs0], Info0, Info, [Proc | Procs]) :-
	llds_common__process_proc(Proc0, Info0, Info1, Proc),
	llds_common__process_procs(Procs0, Info1, Info, Procs).

:- pred llds_common__process_proc(c_procedure, common_info, common_info,
	c_procedure).
:- mode llds_common__process_proc(in, in, out, out) is det.

llds_common__process_proc(Proc0, Info0, Info, Proc) :-
	Proc0 = c_procedure(Name, Arity, Mode, Instrs0),
	llds_common__process_instrs(Instrs0, Info0, Info, Instrs),
	Proc = c_procedure(Name, Arity, Mode, Instrs).

:- pred llds_common__process_instrs(list(instruction),
	common_info, common_info, list(instruction)).
:- mode llds_common__process_instrs(in, in, out, out) is det.

llds_common__process_instrs([], Info, Info, []).
llds_common__process_instrs([Uinstr0 - Comment | Instrs0], Info0, Info,
		[Uinstr - Comment | Instrs]) :-
	llds_common__process_instr(Uinstr0, Info0, Info1, Uinstr),
	llds_common__process_instrs(Instrs0, Info1, Info, Instrs).

:- pred llds_common__process_instr(instr, common_info, common_info, instr).
:- mode llds_common__process_instr(in, in, out, out) is det.

llds_common__process_instr(Instr0, Info0, Info, Instr) :-
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
		llds_common__process_instrs(Instrs0, Info0, Info, Instrs),
		Instr = block(NR, NF, Instrs)
	;
		Instr0 = assign(Lval, Rval0),
		llds_common__process_rval(Rval0, Info0, Info, Rval),
		Instr = assign(Lval, Rval)
	;
		Instr0 = call(_, _, _, _),
		Instr = Instr0,
		Info = Info0
	;
		Instr0 = mkframe(_, _, _),
		Instr = Instr0,
		Info = Info0
	;
		Instr0 = modframe(_),
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
		llds_common__process_rval(Rval0, Info0, Info, Rval),
		Instr = computed_goto(Rval, Labels)
	;
		Instr0 = c_code(_),
		Instr = Instr0,
		Info = Info0
	;
		% unlikely to find anything to share, but why not try?
		Instr0 = if_val(Rval0, Target),
		llds_common__process_rval(Rval0, Info0, Info, Rval),
		Instr = if_val(Rval, Target)
	;
		% unlikely to find anything to share, but why not try?
		Instr0 = incr_hp(Lval, MaybeTag, Rval0),
		llds_common__process_rval(Rval0, Info0, Info, Rval),
		Instr = incr_hp(Lval, MaybeTag, Rval)
	;
		Instr0 = mark_hp(_),
		Instr = Instr0,
		Info = Info0
	;
		% unlikely to find anything to share, but why not try?
		Instr0 = restore_hp(Rval0),
		llds_common__process_rval(Rval0, Info0, Info, Rval),
		Instr = restore_hp(Rval)
	;
		Instr0 = store_ticket(_),
		Instr = Instr0,
		Info = Info0
	;
		% unlikely to find anything to share, but why not try?
		Instr0 = restore_ticket(Rval0),
		llds_common__process_rval(Rval0, Info0, Info, Rval),
		Instr = restore_ticket(Rval)
	;
		Instr0 = discard_ticket,
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
		Instr0 = pragma_c(_, _, _, _, _),
		Instr = Instr0,
		Info = Info0
	).

:- pred llds_common__process_rval(rval, common_info, common_info, rval).
:- mode llds_common__process_rval(in, in, out, out) is det.

llds_common__process_rval(Rval0, Info0, Info, Rval) :-
	(
		Rval0 = lval(_),
		Rval = Rval0,
		Info = Info0
	;
		Rval0 = var(_),
		error("var rval found in llds_common__process_rval")
	;
		Rval0 = create(Tag, Args, Unique, _LabelNo),
		( Unique = no ->
			llds_common__process_create(Tag, Args, Info0,
				Info, Rval)
		;
			Rval = Rval0,
			Info = Info0
		)
	;
		Rval0 = mkword(Tag, SubRval0),
		llds_common__process_rval(SubRval0, Info0, Info, SubRval),
		Rval = mkword(Tag, SubRval)
	;
		Rval0 = const(_),
		Rval = Rval0,
		Info = Info0
	;
		Rval0 = unop(Unop, SubRval0),
		llds_common__process_rval(SubRval0, Info0, Info, SubRval),
		Rval = unop(Unop, SubRval)
	;
		Rval0 = binop(Binop, Left0, Right0),
		llds_common__process_rval(Left0, Info0, Info1, Left),
		llds_common__process_rval(Right0, Info1, Info, Right),
		Rval = binop(Binop, Left, Right)
	;
		Rval0 = mem_addr(MemRef0),
		llds_common__process_mem_ref(MemRef0, Info0, Info, MemRef),
		Rval = mem_addr(MemRef)
	).

:- pred llds_common__process_mem_ref(mem_ref, common_info, common_info,
	mem_ref).
:- mode llds_common__process_mem_ref(in, in, out, out) is det.

llds_common__process_mem_ref(stackvar_ref(N), Info, Info, stackvar_ref(N)).
llds_common__process_mem_ref(framevar_ref(N), Info, Info, framevar_ref(N)).
llds_common__process_mem_ref(heap_ref(Rval0, Tag, N), Info0, Info,
		heap_ref(Rval, Tag, N)) :-
	llds_common__process_rval(Rval0, Info0, Info, Rval).

:- pred llds_common__process_rvals(list(rval), common_info, common_info,
	list(rval)).
:- mode llds_common__process_rvals(in, in, out, out) is det.

llds_common__process_rvals([], Info, Info, []).
llds_common__process_rvals([Rval0 | Rvals0], Info0, Info,
		[Rval | Rvals]) :-
	llds_common__process_rval(Rval0, Info0, Info1, Rval),
	llds_common__process_rvals(Rvals0, Info1, Info, Rvals).

:- pred llds_common__process_maybe_rvals(list(maybe(rval)),
	common_info, common_info, list(maybe(rval))).
:- mode llds_common__process_maybe_rvals(in, in, out, out) is det.

llds_common__process_maybe_rvals([], Info, Info, []).
llds_common__process_maybe_rvals([MaybeRval0 | MaybeRvals0], Info0, Info,
		[MaybeRval | MaybeRvals]) :-
	(
		MaybeRval0 = yes(Rval0),
		llds_common__process_rval(Rval0, Info0, Info1, Rval),
		MaybeRval = yes(Rval)
	;
		MaybeRval0 = no,
		MaybeRval = no,
		Info1 = Info0
	),
	llds_common__process_maybe_rvals(MaybeRvals0, Info1, Info, MaybeRvals).
