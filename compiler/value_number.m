%-----------------------------------------------------------------------------%

% Value_number.nl - optimization of straight-line LLDS code.

% Main author: zs.

%-----------------------------------------------------------------------------%

:- module value_number.

:- interface.
:- import_module llds, list.

:- pred value_number__optimize(list(instruction), list(instruction)).
:- mode value_number__optimize(in, out) is det.

% the rest are exported only for debugging.

:- type vn == int.

:- type vnlval		--->	vn_reg(reg)
			;	vn_stackvar(int)
			;	vn_framevar(int)
			;	vn_succip
			;	vn_maxfr
			;	vn_curredoip
			;	vn_hp
			;	vn_sp
			;	vn_field(tag, vn, vn)		% lval
			;	vn_temp(int).

			% these lvals do not have vnlval parallels
			%	lvar(var)

:- type vnrval		--->	vn_origlval(vnlval)
			;	vn_mkword(tag, vn)		% rval
			;	vn_const(rval_const)
			;	vn_create(tag, list(maybe(rval)), int)
			;	vn_unop(unary_op, vn)		% rval
			;	vn_binop(binary_op, vn, vn).	% rval, rval

			% these rvals do not have vnrval parallels
			%	var(var)

:- type lval_to_vn_table == map(vnlval, vn).
:- type rval_to_vn_table == map(vnrval, vn).
:- type vn_to_rval_table == map(vn, vnrval).
:- type vn_to_uses_table == map(vn, int).
:- type vn_to_locs_table == map(vn, list(vnlval)).
:- type loc_to_vn_table  == map(vnlval, vn).

:- type vn_tables --->	vn_tables(vn, lval_to_vn_table, rval_to_vn_table,
				vn_to_rval_table, vn_to_uses_table,
				vn_to_locs_table, loc_to_vn_table).

:- type vn_node		--->	node_shared(vn)
			;	node_lval(vnlval)
			;	node_origlval(vnlval)
			;	node_ctrl(int).

:- type vn_instr	--->	vn_call(code_addr, code_addr, list(liveinfo))
			;	vn_call_closure(bool, code_addr, list(liveinfo))
			;	vn_mkframe(string, int, code_addr)
			;	vn_modframe(code_addr)
			;	vn_label(label)
			;	vn_goto(code_addr)
			;	vn_computed_goto(vn, list(label))
			;	vn_if_val(vn, code_addr).

:- type livemap == map(label, lvalset).
:- type lvalset == bintree_set(lval).
:- type vnlvalset == bintree_set(vnlval).

:- type ctrlmap		== map(int, vn_instr).
:- type flushmap	== map(int, flushmapentry).
:- type flushmapentry	== map(vnlval, vn).

% XXX map__update/set should be det, should call error if key not already there

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module atsort, vn_util, opt_util, opt_debug, map, bintree_set.
:- import_module int, string, require, std_util.

	% Find straight-line code sequences and optimize them using
	% value numbering.

	% We can't find out what variables are used by C code sequences,
	% so we don't optimize any predicates containing them.

value_number__optimize(Instrs0, Instrs) :-
	list__reverse(Instrs0, Backinstrs),
	map__init(Livemap0),
	vn__repeat_build_livemap(Backinstrs, Livemap0, Ccode, Livemap),
	(
		Ccode = no,
		vn__insert_livemap(Livemap, Instrs0, Instrs1),
		vn__opt_non_block(Instrs1, Livemap, Instrs)
	;
		Ccode = yes,
		Instrs = Instrs0
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Build up a map of what lvals are live at each label,
	% This step must be iterated in the presence of backward
	% branches, which at the moment are generated only by middle
	% recursion.

:- pred vn__repeat_build_livemap(list(instruction), livemap, bool, livemap).
:- mode vn__repeat_build_livemap(in, di, out, uo) is det.

vn__repeat_build_livemap(Backinstrs, Livemap0, Ccode, Livemap) :-
	bintree_set__init(Livevals0),
	vn__build_livemap(Backinstrs, Livevals0,
		no, Ccode1, Livemap0, Livemap1),
	( Ccode1 = yes ->
		Ccode = yes,
		Livemap = Livemap1
	; vn__equal_livemaps(Livemap0, Livemap1) ->
		Ccode = no,
		Livemap = Livemap1
	;
		vn__repeat_build_livemap(Backinstrs, Livemap1, Ccode, Livemap)
	).

:- pred vn__equal_livemaps(livemap, livemap).
:- mode vn__equal_livemaps(in, in) is semidet.

vn__equal_livemaps(Livemap1, Livemap2) :-
	map__keys(Livemap1, Labels),
	vn__equal_livemaps_keys(Labels, Livemap1, Livemap2).

:- pred vn__equal_livemaps_keys(list(label), livemap, livemap).
:- mode vn__equal_livemaps_keys(in, in, in) is semidet.

vn__equal_livemaps_keys([], _Livemap1, _Livemap2).
vn__equal_livemaps_keys([Label | Labels], Livemap1, Livemap2) :-
	map__lookup(Livemap1, Label, Liveset1),
	map__lookup(Livemap2, Label, Liveset2),
	bintree_set__equal(Liveset1, Liveset2),
	vn__equal_livemaps_keys(Labels, Livemap1, Livemap2).

%-----------------------------------------------------------------------------%

:- pred vn__build_livemap(list(instruction), lvalset, bool, bool,
	livemap, livemap).
:- mode vn__build_livemap(in, in, in, out, di, uo) is det.

vn__build_livemap([], _, Ccode, Ccode, Livemap, Livemap).
vn__build_livemap([Instr|Moreinstrs], Livevals0, Ccode0, Ccode,
		Livemap0, Livemap) :-
	Instr = Uinstr - _Comment,
	(
		Uinstr = comment(_),
		Livemap1 = Livemap0,
		Livevals2 = Livevals0,
		Moreinstrs2 = Moreinstrs,
		Ccode1 = Ccode0
	;
		Uinstr = livevals(_, _),
		error("livevals found in backward scan in vn__build_livemap")
	;
		Uinstr = block(_, _),
		error("block found in backward scan in vn__build_livemap")
	;
		Uinstr = assign(Lval, Rval),
		vn__make_dead(Lval, Livevals0, Livevals1),
		vn__make_live(Rval, Livevals1, Livevals2),
		Livemap1 = Livemap0,
		Moreinstrs2 = Moreinstrs,
		Ccode1 = Ccode0
	;
		Uinstr = call(_, _, _),
		opt_util__skip_comments(Moreinstrs, Moreinstrs1),
		(
			Moreinstrs1 = [Nextinstr | Evenmoreinstrs],
			Nextinstr = Nextuinstr - Nextcomment,
			Nextuinstr = livevals(yes, Livevals2prime)
		->
			Livevals2 = Livevals2prime,
			Livemap1 = Livemap0,
			Moreinstrs2 = Evenmoreinstrs,
			Ccode1 = Ccode0
		;
			error("call not preceded by livevals")
		)
	;
		Uinstr = call_closure(_, _, _),
		opt_util__skip_comments(Moreinstrs, Moreinstrs1),
		(
			Moreinstrs1 = [Nextinstr | Evenmoreinstrs],
			Nextinstr = Nextuinstr - Nextcomment,
			Nextuinstr = livevals(yes, Livevals2prime)
		->
			Livevals2 = Livevals2prime,
			Livemap1 = Livemap0,
			Moreinstrs2 = Evenmoreinstrs,
			Ccode1 = Ccode0
		;
			error("call_closure not preceded by livevals")
		)
	;
		Uinstr = mkframe(_, _, _),
		Livemap1 = Livemap0,
		Livevals2 = Livevals0,
		Moreinstrs2 = Moreinstrs,
		Ccode1 = Ccode0
	;
		Uinstr = modframe(_),
		Livemap1 = Livemap0,
		Livevals2 = Livevals0,
		Moreinstrs2 = Moreinstrs,
		Ccode1 = Ccode0
	;
		Uinstr = label(Label),
		( Ccode0 = no ->
			map__set(Livemap0, Label, Livevals0, Livemap1)
		;
			Livemap1 = Livemap0
		),
		Livevals2 = Livevals0,
		Moreinstrs2 = Moreinstrs,
		Ccode1 = Ccode0
	;
		Uinstr = goto(Codeaddr),
		opt_util__skip_comments(Moreinstrs, Moreinstrs1),
		opt_util__livevals_addr(Codeaddr, LivevalsNeeded),
		( LivevalsNeeded = yes ->
			(
				Moreinstrs1 = [Nextinstr | Evenmoreinstrs],
				Nextinstr = Nextuinstr - Nextcomment,
				Nextuinstr = livevals(yes, Livevals2prime)
			->
				Livevals2 = Livevals2prime,
				Livemap1 = Livemap0,
				Moreinstrs2 = Evenmoreinstrs,
				Ccode1 = Ccode0
			;
				error("tailcall not preceded by livevals")
			)
		; Codeaddr = label(Label) ->
			( map__search(Livemap0, Label, Livevals2prime) ->
				Livevals2 = Livevals2prime
			;
				bintree_set__init(Livevals2)
			),
			Livemap1 = Livemap0,
			Moreinstrs2 = Moreinstrs,
			Ccode1 = Ccode0
		;
			Livevals2 = Livevals0,
			Livemap1 = Livemap0,
			Moreinstrs2 = Moreinstrs,
			Ccode1 = Ccode0
		)
	;
		Uinstr = computed_goto(_, Labels),
		bintree_set__init(Livevals1),
		vn__insert_all_livevals(Labels, Livemap0, Livevals1, Livevals2),
		Livemap1 = Livemap0,
		Moreinstrs2 = Moreinstrs,
		Ccode1 = Ccode0
	;
		Uinstr = c_code(_),
		Livemap1 = Livemap0,
		Livevals2 = Livevals0,
		Moreinstrs2 = Moreinstrs,
		Ccode1 = yes
	;
		Uinstr = if_val(Rval, _),
		vn__make_live(Rval, Livevals0, Livevals2),
		Livemap1 = Livemap0,
		Moreinstrs2 = Moreinstrs,
		Ccode1 = Ccode0
	;
		Uinstr = incr_hp(Lval, Rval),
		vn__make_dead(Lval, Livevals0, Livevals1),
		vn__make_live(Rval, Livevals1, Livevals2),
		Livemap1 = Livemap0,
		Moreinstrs2 = Moreinstrs,
		Ccode1 = Ccode0
	;
		Uinstr = restore_hp(Rval),
		vn__make_live(Rval, Livevals0, Livevals2),
		Livemap1 = Livemap0,
		Moreinstrs2 = Moreinstrs,
		Ccode1 = Ccode0
	;
		Uinstr = incr_sp(_),
		Livevals2 = Livevals0,
		Livemap1 = Livemap0,
		Moreinstrs2 = Moreinstrs,
		Ccode1 = Ccode0
	;
		Uinstr = decr_sp(_),
		Livevals2 = Livevals0,
		Livemap1 = Livemap0,
		Moreinstrs2 = Moreinstrs,
		Ccode1 = Ccode0
	),
	vn__build_livemap(Moreinstrs2, Livevals2, Ccode1, Ccode,
		Livemap1, Livemap).

:- pred vn__insert_all_livevals(list(label), livemap, lvalset, lvalset).
:- mode vn__insert_all_livevals(in, in, di, uo) is det.

vn__insert_all_livevals([], _, Livevals, Livevals).
vn__insert_all_livevals([Label | Labels], Livemap, Livevals0, Livevals) :-
	( map__search(Livemap, Label, LabelLivevals) ->
		bintree_set__union(Livevals0, LabelLivevals, Livevals1)
	;
		Livevals1 = Livevals0
	),
	vn__insert_all_livevals(Labels, Livemap, Livevals1, Livevals).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Insert the livemap into the instruction stream for debugging.

:- pred vn__insert_livemap(livemap, list(instruction), list(instruction)).
:- mode vn__insert_livemap(in, di, uo) is det.

vn__insert_livemap(_Livemap, [], []).
vn__insert_livemap(Livemap, [Instr0 | Instrs0], Instrs) :-
	vn__insert_livemap(Livemap, Instrs0, Instrs1),
	Instr0 = Uinstr0 - _Comment,
	( Uinstr0 = label(Label) ->
		( map__search(Livemap, Label, Livevals) ->
			Instrs = [Instr0, livevals(no, Livevals) - "auto"
				| Instrs1]
		;
			error("no livevals info about a label")
		)
	;
		Instrs = [Instr0 | Instrs1]
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Optimize instructions, assume we are outside of a block.

:- pred vn__opt_non_block(list(instruction), livemap, list(instruction)).
:- mode vn__opt_non_block(in, in, out) is det.

vn__opt_non_block([], _, []).
vn__opt_non_block([Instr0 | Instrs0], Livemap, Instrs) :-
	Instr0 = Uinstr0 - _Comment0,
	(
		Uinstr0 = label(Label),
		map__search(Livemap, Label, _Livevals)
	->
		opt_util__gather_comments(Instrs0, Comments, Instrs1),
		(
			Instrs1 = [Instr1prime | Instrs2prime],
			Instr1prime = livevals(no, Livevals) - _ 
		->
			Instr1 = Instr1prime,
			Instrs2 = Instrs2prime,
			vn__init_templocs(5, 5, Livevals, Templocs)
		;
			error("label is not followed by livevals")
		),
		vn__init_tables(Vn_tables0),
		bintree_set__init(Liveset0),
		map__init(Ctrlmap0),
		map__init(Flushmap0),
		vn__opt_block(Instrs2, Vn_tables0, Livemap, Templocs, Liveset0,
			0, 0, Ctrlmap0, Flushmap0, 0, [], NewInstrs),
		list__condense([Comments, [Instr0], Comments, [Instr1], NewInstrs],
			Instrs)
	;
		Uinstr0 = comment(_)
	->
		vn__opt_non_block(Instrs0, Livemap, Instrs1),
		Instrs = [Instr0 | Instrs1]
	;
		% write(Instr0),
		error("non-label found in opt_non_block")
		% vn__opt_non_block(Instrs0, Livemap, Instrs1),
		% Instrs = [Instr0 | Instrs1]
	).

	% Optimize instructions, assuming we are inside of a block.

:- pred vn__opt_block(list(instruction), vn_tables, livemap, templocs, vnlvalset,
	int, int, ctrlmap, flushmap, int, list(instruction), list(instruction)).
:- mode vn__opt_block(in, in, in, in, in, in, in, in, in, in, in, out) is det.

vn__opt_block([], _, _, _, _, _, _, _, _, _, _, []) :-
	error("block has no terminator").
vn__opt_block([Instr0 | Instrs0], Vn_tables, Livemap, Templocs, Livevals,
		Incrsp, Decrsp, Ctrlmap0, Flushmap0, Ctrl0, Prev, Instrs) :-
	vn__handle_instr(Instr0, Vn_tables, Livemap, Templocs, Livevals,
		Incrsp, Decrsp, Ctrlmap0, Flushmap0, Ctrl0, Prev, Instrs0,
		Instrs).

%-----------------------------------------------------------------------------%

:- pred vn__handle_instr(instruction, vn_tables, livemap, templocs, vnlvalset,
	int, int, ctrlmap, flushmap, int, list(instruction),
	list(instruction), list(instruction)).
:- mode vn__handle_instr(in, in, in, in, in, in, in, in, in, in, in, in, out)
	is det.

vn__handle_instr(Instr0, Vn_tables0, Livemap, Templocs0, Livevals0,
		Incrsp, Decrsp, Ctrlmap0, Flushmap0, Ctrl0, Prev,
		Instrs0, Instrs) :-
	Instr0 = Uinstr0 - _Comment,
	(
		Uinstr0 = comment(_),
		vn__opt_block(Instrs0, Vn_tables0, Livemap,
			Templocs0, Livevals0, Incrsp, Decrsp,
			Ctrlmap0, Flushmap0, Ctrl0, [Instr0 | Prev], Instrs)
	;
		Uinstr0 = livevals(Terminate, Livevals),
		vn__handle_livevals(Terminate, Livevals, Livevals0, Livevals1),
		vn__opt_block(Instrs0, Vn_tables0, Livemap,
			Templocs0, Livevals0, Incrsp, Decrsp,
			Ctrlmap0, Flushmap0, Ctrl0, [Instr0 | Prev], Instrs)
	;
		Uinstr0 = block(_, _),
		error("block should not be found in handle_instr")
	;
		Uinstr0 = assign(Lval, Rval),
		vn__handle_assign(Lval, Rval, Vn_tables0, Vn_tables1),
		vn__opt_block(Instrs0, Vn_tables1, Livemap,
			Templocs0, Livevals0, Incrsp, Decrsp,
			Ctrlmap0, Flushmap0, Ctrl0, [Instr0 | Prev], Instrs)
	;
		Uinstr0 = call(ProcAddr, ReturnAddr, LiveInfo),
		vn__new_ctrl_node(vn_call(ProcAddr, ReturnAddr, LiveInfo),
			Livemap, Vn_tables0, Vn_tables1, Livevals0, Livevals1,
			Ctrlmap0, Ctrlmap1, Flushmap0, Flushmap1, Ctrl0, Ctrl1),
		vn__flush_all_nodes(Livevals1, Templocs0, Vn_tables1,
			Incrsp, Decrsp, Ctrlmap1, Flushmap1, Ctrl1,
			[Instr0 | Prev], FlushInstrs),
		vn__opt_non_block(Instrs0, Livemap, Instrs1),
		list__append(FlushInstrs, Instrs1, Instrs)
	;
		Uinstr0 = call_closure(ClAddr, ReturnAddr, LiveInfo),
		vn__new_ctrl_node(vn_call_closure(ClAddr, ReturnAddr, LiveInfo),
			Livemap, Vn_tables0, Vn_tables1, Livevals0, Livevals1,
			Ctrlmap0, Ctrlmap1, Flushmap0, Flushmap1, Ctrl0, Ctrl1),
		vn__flush_all_nodes(Livevals1, Templocs0, Vn_tables1,
			Incrsp, Decrsp, Ctrlmap1, Flushmap1, Ctrl1,
			[Instr0 | Prev], FlushInstrs),
		vn__opt_non_block(Instrs0, Livemap, Instrs1),
		list__append(FlushInstrs, Instrs1, Instrs)
	;
		Uinstr0 = mkframe(Name, Size, Redoip),
		vn__new_ctrl_node(vn_mkframe(Name, Size, Redoip), Livemap,
			Vn_tables0, Vn_tables1, Livevals0, Livevals1,
			Ctrlmap0, Ctrlmap1, Flushmap0, Flushmap1, Ctrl0, Ctrl1),
		vn__opt_block(Instrs0, Vn_tables1, Livemap,
			Templocs0, Livevals1, Incrsp, Decrsp,
			Ctrlmap1, Flushmap1, Ctrl1, [Instr0 | Prev], Instrs)
	;
		Uinstr0 = modframe(Redoip),
		vn__new_ctrl_node(vn_modframe(Redoip), Livemap,
			Vn_tables0, Vn_tables1, Livevals0, Livevals1,
			Ctrlmap0, Ctrlmap1, Flushmap0, Flushmap1, Ctrl0, Ctrl1),
		vn__opt_block(Instrs0, Vn_tables1, Livemap,
			Templocs0, Livevals1, Incrsp, Decrsp,
			Ctrlmap1, Flushmap1, Ctrl1, [Instr0 | Prev], Instrs)
	;
		Uinstr0 = label(Label),
		vn__new_ctrl_node(vn_label(Label), Livemap,
			Vn_tables0, Vn_tables1, Livevals0, Livevals1,
			Ctrlmap0, Ctrlmap1, Flushmap0, Flushmap1, Ctrl0, Ctrl1),
		vn__flush_all_nodes(Livevals1, Templocs0, Vn_tables1,
			Incrsp, Decrsp, Ctrlmap1, Flushmap1, Ctrl1,
			[Instr0 | Prev], FlushInstrs),
		vn__opt_non_block([Instr0 | Instrs0], Livemap, Instrs1),
		list__append(FlushInstrs, Instrs1, Instrs)
	;
		Uinstr0 = goto(CodeAddr),
		vn__new_ctrl_node(vn_goto(CodeAddr), Livemap,
			Vn_tables0, Vn_tables1, Livevals0, Livevals1,
			Ctrlmap0, Ctrlmap1, Flushmap0, Flushmap1, Ctrl0, Ctrl1),
		vn__flush_all_nodes(Livevals1, Templocs0, Vn_tables1,
			Incrsp, Decrsp, Ctrlmap1, Flushmap1, Ctrl1,
			[Instr0 | Prev], FlushInstrs),
		vn__opt_non_block(Instrs0, Livemap, Instrs1),
		list__append(FlushInstrs, Instrs1, Instrs)
	;
		Uinstr0 = computed_goto(Rval, Labels),
		vn__rval_to_vn(Rval, Vn, Vn_tables0, Vn_tables1),
		vn__new_ctrl_node(vn_computed_goto(Vn, Labels), Livemap,
			Vn_tables1, Vn_tables2, Livevals0, Livevals1,
			Ctrlmap0, Ctrlmap1, Flushmap0, Flushmap1, Ctrl0, Ctrl1),
		vn__flush_all_nodes(Livevals1, Templocs0, Vn_tables2,
			Incrsp, Decrsp, Ctrlmap1, Flushmap1, Ctrl1,
			[Instr0 | Prev], FlushInstrs),
		vn__opt_non_block(Instrs0, Livemap, Instrs1),
		list__append(FlushInstrs, Instrs1, Instrs)
	;
		Uinstr0 = c_code(_),
		error("c_code in handle_instr")
	;
		Uinstr0 = if_val(Rval, CodeAddr),
		vn__rval_to_vn(Rval, Vn, Vn_tables0, Vn_tables1),
		vn__new_ctrl_node(vn_if_val(Vn, CodeAddr), Livemap,
			Vn_tables1, Vn_tables2, Livevals0, Livevals1,
			Ctrlmap0, Ctrlmap1, Flushmap0, Flushmap1, Ctrl0, Ctrl1),
		vn__opt_block(Instrs0, Vn_tables2, Livemap,
			Templocs0, Livevals1, Incrsp, Decrsp,
			Ctrlmap1, Flushmap1, Ctrl1, [Instr0 | Prev], Instrs)
	;
		Uinstr0 = incr_sp(N),
		vn__opt_block(Instrs0, Vn_tables0, Livemap,
			Templocs0, Livevals0, N, Decrsp,
			Ctrlmap0, Flushmap0, Ctrl0, [Instr0 | Prev], Instrs)
	;
		Uinstr0 = decr_sp(N),
		vn__opt_block(Instrs0, Vn_tables0, Livemap,
			Templocs0, Livevals0, Incrsp, N,
			Ctrlmap0, Flushmap0, Ctrl0, [Instr0 | Prev], Instrs)
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% uncount all dead lvals
% map from (livelvals U shared_vn U ctrl) to set of (shared_vn U origlvals)
% count only unique paths, i.e. when arriving at a shared node
%  count only the shared vn, not anything else it points to
% build a topological sort based on this order, plus the order that
%  after the appearance of an lval no refs to its origlval be outstanding
%  this includes appearances as the bases of field references
%  plus the order that the livevals at a label must be before a branch
%  to that label

%-----------------------------------------------------------------------------%

:- pred vn__flush_all_nodes(vnlvalset, templocs, vn_tables,
	int, int, ctrlmap, flushmap, int, list(instruction), list(instruction)).
:- mode vn__flush_all_nodes(in, in, in, in, in, in, in, in, in, out) is det.

vn__flush_all_nodes(Livevals, Templocs0, Vn_tables0, Incrsp, Decrsp,
		Ctrlmap, Flushmap, Ctrl, RevInstrs, Instrs) :-
	bintree_set__to_sorted_list(Livevals, Live),
	(
		vn__req_order(Ctrlmap, Flushmap, Vn_tables0,
			MustSuccmap, MustPredmap)
	->
		vn__build_counts(Livevals, Ctrl, Ctrlmap,
			Vn_tables0, Vn_tables1),

		vn__vn_ctrl_order(0, Ctrlmap, Vn_tables1, Vn_tables2,
			MustSuccmap, Succmap0, MustPredmap, Predmap0),
		vn__ctrl_vn_order(0, Flushmap, Livevals,
			Succmap0, Succmap1, Predmap0, Predmap1),
		vn__prod_cons_order(Live, Vn_tables2, Vn_tables3,
			Succmap1, Succmap2, Predmap1, Predmap2),
		vn__use_before_redef(Succmap2, Succmap, Predmap2, Predmap),

		atsort(Succmap, Predmap, MustSuccmap, MustPredmap, BlockOrder),
		LastCtrl is Ctrl - 1,
		vn__blockorder_to_order(BlockOrder, LastCtrl, Order),

		vn__flush_nodelist(Order, Ctrlmap, Vn_tables3, _Vn_tables,
			Templocs0, Templocs, Instrs0),

		( Incrsp > 0 ->
			vn__insert_incr_sp(Incrsp, Instrs0, Instrs1)
		;
			Instrs1 = Instrs0
		),
		( Decrsp > 0 ->
			vn__insert_decr_sp(Decrsp, Instrs1, Instrs2)
		;
			Instrs2 = Instrs1
		),
%%%		XXX error("Zoltan please fixme (#3)"),
%%%		vn__find_incr_hp(Instrs2, Instrs3),
		Instrs3 = Instrs2,
		( yes = no ->
			Instrs4 = Instrs3
		;
			list__reverse(RevInstrs, OldInstrs),
			Com1 = comment("start of old instrs") - "",
			Com2 = comment("end of old instrs") - "",
			Com3 = comment("end of new instrs") - "",
			list__condense([[Com1], OldInstrs, [Com2], Instrs3,
				[Com3]], Instrs4)
		),
		vn__max_temploc(Templocs, Max),
		Instrs = [block(Max, Instrs4) - ""]
	;
		list__reverse(RevInstrs, Instrs)
	).

%-----------------------------------------------------------------------------%

:- pred vn__build_counts(vnlvalset, int, ctrlmap, vn_tables, vn_tables).
:- mode vn__build_counts(in, in, in, di, uo) is det.

:- external(vn__build_counts/5).
% vn__build_counts(Livevals, Ctrl, Ctrlmap, Vn_tables0, Vn_tables1),

%-----------------------------------------------------------------------------%

	% Given the information gathered on the control nodes, check whether
	% any side branch requires a value to be stored in a vnlval other than
	% the final one. If yes, we can't apply (this version of) value
	% numbering. If no, we can apply it, and we compute the compulsory
	% part of the order of nodes, which is that a vnlval that is live
	% at a side branch must be produced before that side branch.

:- pred vn__req_order(ctrlmap, flushmap, vn_tables,
	relmap(vn_node), relmap(vn_node)).
:- mode vn__req_order(in, in, in, out, out) is semidet.

vn__req_order(Ctrlmap, Flushmap, Vn_tables, MustSuccmap, MustPredmap) :-
	map__init(MustSuccmap0),
	map__init(MustPredmap0),
	vn__req_order_2(0, Ctrlmap, Flushmap, Vn_tables,
		MustSuccmap0, MustSuccmap, MustPredmap0, MustPredmap).

:- pred vn__req_order_2(int, ctrlmap, flushmap, vn_tables,
	relmap(vn_node), relmap(vn_node), relmap(vn_node), relmap(vn_node)).
:- mode vn__req_order_2(in, in, in, in, di, uo, di, uo) is semidet.

vn__req_order_2(Ctrl, Ctrlmap, Flushmap, Vn_tables,
		MustSuccmap0, MustSuccmap, MustPredmap0, MustPredmap) :-
	( map__search(Flushmap, Ctrl, FlushEntry) ->
		( Ctrl > 0 ->
			PrevCtrl is Ctrl - 1,
			vn__add_link(node_ctrl(PrevCtrl), node_ctrl(Ctrl),
				MustSuccmap0, MustSuccmap1,
				MustPredmap0, MustPredmap1)
		;
			MustSuccmap1 = MustSuccmap0,
			MustPredmap1 = MustPredmap0
		),
		map__to_assoc_list(FlushEntry, FlushList),
		vn__record_ctrl_deps(FlushList, node_ctrl(Ctrl), Vn_tables,
			MustSuccmap1, MustSuccmap2, MustPredmap1, MustPredmap2),
		NextCtrl is Ctrl + 1,
		vn__req_order_2(NextCtrl, Ctrlmap, Flushmap, Vn_tables,
			MustSuccmap2, MustSuccmap, MustPredmap2, MustPredmap)
	;
		MustSuccmap = MustSuccmap0,
		MustPredmap = MustPredmap0
	).

:- pred vn__record_ctrl_deps(assoc_list(vnlval, vn), vn_node, vn_tables,
	relmap(vn_node), relmap(vn_node), relmap(vn_node), relmap(vn_node)).
:- mode vn__record_ctrl_deps(in, in, in, di, uo, di, uo) is semidet.

vn__record_ctrl_deps([], _Sink, _Vn_tables,
		MustSuccmap, MustSuccmap, MustPredmap, MustPredmap).
vn__record_ctrl_deps([Vnlval - Vn | FlushList], Sink, Vn_tables,
		MustSuccmap0, MustSuccmap, MustPredmap0, MustPredmap) :-
	vn__lookup_desired_value(Vnlval, Des_vn, Vn_tables),
	Vn = Des_vn,
	vn__add_link(node_lval(Vnlval), Sink,
		MustSuccmap0, MustSuccmap1, MustPredmap0, MustPredmap1),
	vn__record_ctrl_deps(FlushList, Sink, Vn_tables,
		MustSuccmap1, MustSuccmap, MustPredmap1, MustPredmap).

%-----------------------------------------------------------------------------%

	% Record the desirability of producing the vnlvals involved in the
	% expressions inside control nodes before producing the control nodes
	% themselves. This is stated as a suggestion, not a requirement, since
	% it can be overridden during the flushing process.

:- pred vn__vn_ctrl_order(int, ctrlmap, vn_tables, vn_tables,
	relmap(vn_node), relmap(vn_node), relmap(vn_node), relmap(vn_node)).
:- mode vn__vn_ctrl_order(in, in, di, uo, di, uo, di, uo) is det.

vn__vn_ctrl_order(Ctrl, Ctrlmap, Vn_tables0, Vn_tables,
		Succmap0, Succmap, Predmap0, Predmap) :-
	( map__search(Ctrlmap, Ctrl, Vn_instr) ->
		(
			Vn_instr = vn_call(_, _, _),
			Succmap1 = Succmap0,
			Predmap1 = Predmap0,
			Vn_tables1 = Vn_tables0
		;
			Vn_instr = vn_call_closure(_, _, _),
			Succmap1 = Succmap0,
			Predmap1 = Predmap0,
			Vn_tables1 = Vn_tables0
		;
			Vn_instr = vn_mkframe(_, _, _),
			Succmap1 = Succmap0,
			Predmap1 = Predmap0,
			Vn_tables1 = Vn_tables0
		;
			Vn_instr = vn_modframe(_),
			Succmap1 = Succmap0,
			Predmap1 = Predmap0,
			Vn_tables1 = Vn_tables0
		;
			Vn_instr = vn_label(_),
			Succmap1 = Succmap0,
			Predmap1 = Predmap0,
			Vn_tables1 = Vn_tables0
		;
			Vn_instr = vn_goto(_),
			Succmap1 = Succmap0,
			Predmap1 = Predmap0,
			Vn_tables1 = Vn_tables0
		;
			Vn_instr = vn_computed_goto(Vn, _),
			vn__find_links(Vn, node_ctrl(Ctrl),
				Vn_tables0, Vn_tables1,
				Succmap0, Succmap1, Predmap0, Predmap1)
		;
			Vn_instr = vn_if_val(Vn, _),
			vn__find_links(Vn, node_ctrl(Ctrl),
				Vn_tables0, Vn_tables1,
				Succmap0, Succmap1, Predmap0, Predmap1)
		),
		NextCtrl is Ctrl + 1,
		vn__vn_ctrl_order(NextCtrl, Ctrlmap, Vn_tables1, Vn_tables,
			Succmap1, Succmap, Predmap1, Predmap)
	;
		Succmap = Succmap0,
		Predmap = Predmap0,
		Vn_tables = Vn_tables0
	).

%-----------------------------------------------------------------------------%

	% Record the desirability of not producing any lvals before a control
	% node expect those we absolutely have to, since any such production
	% will be wasted on one control path.

:- pred vn__ctrl_vn_order(int, flushmap, vnlvalset,
	relmap(vn_node), relmap(vn_node), relmap(vn_node), relmap(vn_node)).
:- mode vn__ctrl_vn_order(in, in, in, di, uo, di, uo) is det.

vn__ctrl_vn_order(Ctrl, Flushmap, Livevals,
		Succmap0, Succmap, Predmap0, Predmap) :-
	( map__search(Flushmap, Ctrl, FlushEntry) ->
		bintree_set__to_sorted_list(Livevals, Livelist),
		vn__record_antideps(Livelist, FlushEntry, Ctrl,
			Succmap0, Succmap1, Predmap0, Predmap1),
		NextCtrl is Ctrl + 1,
		vn__ctrl_vn_order(NextCtrl, Flushmap, Livevals,
			Succmap1, Succmap, Predmap1, Predmap)
	;
		Succmap = Succmap0,
		Predmap = Predmap0
	).

:- pred vn__record_antideps(list(vnlval), flushmapentry, int,
	relmap(vn_node), relmap(vn_node), relmap(vn_node), relmap(vn_node)).
:- mode vn__record_antideps(in, in, in, di, uo, di, uo) is det.

vn__record_antideps([], _, _, Succmap, Succmap, Predmap, Predmap).
vn__record_antideps([Live | Livelist], FlushEntry, Ctrl,
		Succmap0, Succmap, Predmap0, Predmap) :-
	( map__search(FlushEntry, Live, _) ->
		Succmap1 = Succmap0,
		Predmap1 = Predmap0
	;
		vn__add_link(node_lval(Live), node_ctrl(Ctrl),
			Succmap0, Succmap1, Predmap0, Predmap1)
	),
	vn__record_antideps(Livelist, FlushEntry, Ctrl,
		Succmap1, Succmap, Predmap1, Predmap).

%-----------------------------------------------------------------------------%

	% Record the natural producer-consumer relationships
	% induced by the live vnlvals.

:- pred vn__prod_cons_order(list(vnlval), vn_tables, vn_tables,
	relmap(vn_node), relmap(vn_node), relmap(vn_node), relmap(vn_node)).
:- mode vn__prod_cons_order(in, di, uo, di, uo, di, uo) is det.

vn__prod_cons_order([], Vn_tables, Vn_tables,
		Succmap, Succmap, Predmap, Predmap).
vn__prod_cons_order([Vnlval | Vnlvals], Vn_tables0, Vn_tables,
		Succmap0, Succmap, Predmap0, Predmap) :-
	vn__lookup_desired_value(Vnlval, Vn, Vn_tables0),
	vn__find_links(Vn, node_lval(Vnlval), Vn_tables0, Vn_tables1,
		Succmap0, Succmap1, Predmap0, Predmap1),
	vn__prod_cons_order(Vnlvals, Vn_tables1, Vn_tables,
		Succmap1, Succmap, Predmap1, Predmap).

%-----------------------------------------------------------------------------%

	% Try to make sure that all immediate users of the original value
	% of a vnlval are done before the vnlval is redefined. This avoids
	% an instruction to save the original value somewhere else.

:- pred vn__use_before_redef(relmap(vn_node), relmap(vn_node),
	relmap(vn_node), relmap(vn_node)).
:- mode vn__use_before_redef(di, uo, di, uo) is det.

vn__use_before_redef(Succmap0, Succmap, Predmap0, Predmap) :-
	map__keys(Predmap0, Sinks),
	vn__use_sinks_before_redef(Sinks, Succmap0, Succmap, Predmap0, Predmap).

:- pred vn__use_sinks_before_redef(list(vn_node),
	relmap(vn_node), relmap(vn_node), relmap(vn_node), relmap(vn_node)).
:- mode vn__use_sinks_before_redef(in, di, uo, di, uo) is det.

vn__use_sinks_before_redef([], Succmap, Succmap, Predmap, Predmap).
vn__use_sinks_before_redef([Sink | Sinks],
		Succmap0, Succmap, Predmap0, Predmap) :-
	( Sink = node_lval(Vnlval) ->
		map__lookup(Succmap0, node_origlval(Vnlval), Users),
		vn__add_links(Users, Sink,
			Succmap0, Succmap1, Predmap0, Predmap1)
	;
		Succmap1 = Succmap0,
		Predmap1 = Predmap0
	),
	vn__use_sinks_before_redef(Sinks, Succmap1, Succmap, Predmap1, Predmap).

%-----------------------------------------------------------------------------%

	% Record the dependency of the nodes inside the given vn
	% on the given node.

:- pred vn__find_links(vn, vn_node, vn_tables, vn_tables,
	relmap(vn_node), relmap(vn_node), relmap(vn_node), relmap(vn_node)).
:- mode vn__find_links(in, in, di, uo, di, uo, di, uo) is det.

vn__find_links(Vn, Sink, Vn_tables0, Vn_tables,
		Succmap0, Succmap, Predmap0, Predmap) :-
	vn__lookup_use_count(Vn, Uses0, Vn_tables0),
	( Uses0 > 1, \+ Sink = node_shared(_) ->
		vn__add_link(node_shared(Vn), Sink,
			Succmap0, Succmap1, Predmap0, Predmap1),
		vn__find_links(Vn, node_shared(Vn), Vn_tables0, Vn_tables,
			Succmap1, Succmap, Predmap1, Predmap)
	;
		vn__lookup_defn(Vn, Vnrval, Vn_tables0),
		(
			Vnrval = vn_origlval(Vnlval),
			vn__vnlval_access_vn(Vnlval, Access_vn),
			( Access_vn = yes(SubVn) ->
				vn__find_links(SubVn, Sink, Vn_tables0, Vn_tables,
					Succmap0, Succmap1, Predmap0, Predmap1)
			;
				Succmap1 = Succmap0,
				Predmap1 = Predmap0,
				Vn_tables = Vn_tables0
			),
			vn__add_link(node_origlval(Vnlval), Sink,
				Succmap1, Succmap, Predmap1, Predmap)
		;
			Vnrval = vn_mkword(_Tag1, SubVn),
			vn__find_links(SubVn, Sink, Vn_tables0, Vn_tables,
				Succmap0, Succmap, Predmap0, Predmap)
		;
			Vnrval = vn_const(_Const),
			Succmap = Succmap0,
			Predmap = Predmap0,
			Vn_tables = Vn_tables0
		;
			Vnrval = vn_create(_Tag2, _Args, _Label),
			Succmap = Succmap0,
			Predmap = Predmap0,
			Vn_tables = Vn_tables0
		;
			Vnrval = vn_unop(_Unop, SubVn),
			vn__find_links(SubVn, Sink, Vn_tables0, Vn_tables,
				Succmap0, Succmap, Predmap0, Predmap)
		;
			Vnrval = vn_binop(_Binop, SubVn1, SubVn2),
			vn__find_links(SubVn1, Sink, Vn_tables0, Vn_tables1,
				Succmap0, Succmap1, Predmap0, Predmap1),
			vn__find_links(SubVn2, Sink, Vn_tables1, Vn_tables,
				Succmap1, Succmap, Predmap1, Predmap)
		)
	).

%-----------------------------------------------------------------------------%

	% Add specified link(s) to succmap/predmap pairs.

:- pred vn__add_links(list(vn_node), vn_node,
	relmap(vn_node), relmap(vn_node), relmap(vn_node), relmap(vn_node)).
:- mode vn__add_links(in, in, di, uo, di, uo) is det.

vn__add_links([], _, Succmap, Succmap, Predmap, Predmap).
vn__add_links([Source | Sources], Sink, Succmap0, Succmap, Predmap0, Predmap) :-
	vn__add_link(Source, Sink, Succmap0, Succmap1, Predmap0, Predmap1),
	vn__add_links(Sources, Sink, Succmap1, Succmap, Predmap1, Predmap).

:- pred vn__add_link(vn_node, vn_node,
	relmap(vn_node), relmap(vn_node), relmap(vn_node), relmap(vn_node)).
:- mode vn__add_link(in, in, di, uo, di, uo) is det.

vn__add_link(Source, Sink, Succmap0, Succmap, Predmap0, Predmap) :-
	( Source = Sink ->
		Succmap = Succmap0,
		Predmap = Predmap0
	;
		( map__search(Succmap0, Source, Sinks) ->
			map__set(Succmap0, Source, [Sink | Sinks], Succmap1)
		;
			map__set(Succmap0, Source, [Sink], Succmap1)
		),
		( map__search(Succmap0, Sink, _) ->
			Succmap = Succmap1
		;
			map__set(Succmap1, Sink, [], Succmap)
		),
		( map__search(Predmap0, Sink, Sources) ->
			map__set(Predmap0, Sink, [Source | Sources], Predmap1)
		;
			map__set(Predmap0, Sink, [Source], Predmap1)
		),
		( map__search(Predmap0, Source, _) ->
			Predmap = Predmap1
		;
			map__set(Predmap1, Source, [], Predmap)
		)
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Flush the given nodes in the given order.

:- pred vn__flush_nodelist(list(vn_node), ctrlmap,
	vn_tables, vn_tables, templocs, templocs, list(instruction)).
:- mode vn__flush_nodelist(in, in, di, uo, di, uo, out) is det.

vn__flush_nodelist([], _Ctrlmap, Vn_tables, Vn_tables, Templocs, Templocs, []).
vn__flush_nodelist([Node | Nodes], Ctrlmap, Vn_tables0, Vn_tables,
		Templocs0, Templocs, Instrs) :-
	vn__flush_node(Node, Ctrlmap, Vn_tables0, Vn_tables1,
		Templocs0, Templocs1, Instrs0),
	vn__flush_nodelist(Nodes, Ctrlmap, Vn_tables1, Vn_tables,
		Templocs1, Templocs, Instrs1),
	list__append(Instrs0, Instrs1, Instrs).

	% Flush the given node.

:- pred vn__flush_node(vn_node, ctrlmap, vn_tables, vn_tables,
	templocs, templocs, list(instruction)).
:- mode vn__flush_node(in, in, di, uo, di, uo, out) is det.

vn__flush_node(Node, Ctrlmap, Vn_tables0, Vn_tables, Templocs0, Templocs,
		Instrs) :-
	(
		Node = node_shared(Vn),
		vn__choose_best_location(Vn, Vnlval, Vn_tables0,
			Templocs0, Templocs1),
		vn__generate_assignment(Vnlval, _, Vn, Vn_tables0, Vn_tables,
			Templocs1, Templocs, Instrs)
	;
		Node = node_lval(Vnlval),
		vn__lookup_desired_value(Vnlval, Vn, Vn_tables0),
		vn__generate_assignment(Vnlval, _, Vn, Vn_tables0, Vn_tables,
			Templocs0, Templocs, Instrs)
	;
		Node = node_origlval(Vnlval),
		error("node_origlval found in vn__flush_node")
	;
		Node = node_ctrl(N),
		map__lookup(Ctrlmap, N, Vn_instr),
		(
			Vn_instr = vn_call(ProcAddr, RetAddr, LiveInfo),
			Instrs = [call(ProcAddr, RetAddr, LiveInfo) - ""],
			Vn_tables = Vn_tables0,
			Templocs = Templocs0
		;
			Vn_instr = vn_call_closure(ClAddr, RetAddr, LiveInfo),
			Instrs = [call_closure(ClAddr, RetAddr, LiveInfo) - ""],
			Vn_tables = Vn_tables0,
			Templocs = Templocs0
		;
			Vn_instr = vn_mkframe(Name, Size, Redoip),
			Instrs = [mkframe(Name, Size, Redoip) - ""],
			Vn_tables = Vn_tables0,
			Templocs = Templocs0
		;
			Vn_instr = vn_modframe(Redoip),
			Instrs = [modframe(Redoip) - ""],
			Vn_tables = Vn_tables0,
			Templocs = Templocs0
		;
			Vn_instr = vn_label(_),
			Instrs = [],
			Vn_tables = Vn_tables0,
			Templocs = Templocs0
		;
			Vn_instr = vn_goto(TargetAddr),
			Instrs = [goto(TargetAddr) - ""],
			Vn_tables = Vn_tables0,
			Templocs = Templocs0
		;
			Vn_instr = vn_computed_goto(Vn, Labels),
			vn__flush_vn(Vn, Rval, no, Vn_tables0, Vn_tables,
				Templocs0, Templocs, FlushInstrs),
			Instr = computed_goto(Rval, Labels) - "",
			list__append(FlushInstrs, [Instr], Instrs)
		;
			Vn_instr = vn_if_val(Vn, TargetAddr),
			vn__flush_vn(Vn, Rval, no, Vn_tables0, Vn_tables,
				Templocs0, Templocs, FlushInstrs),
			Instr = if_val(Rval, TargetAddr) - "",
			list__append(FlushInstrs, [Instr], Instrs)
		)
	).

%-----------------------------------------------------------------------------%

	% check for constants, for vars already holding the value etc
	% XXX

:- pred vn__choose_best_location(vn, vnlval, vn_tables, templocs, templocs).
:- mode vn__choose_best_location(in, out, in, di, uo) is det.

:- external(vn__choose_best_location/5).
% vn__choose_best_location(Vn, Vnlval, Vn_tables0).

%-----------------------------------------------------------------------------%

	% Choose the cheapest location from among those already holding
	% the desired vn. Therefore access time is the only consideration.

:- pred vn__choose_cheapest_loc(list(vnlval), maybe(vnlval), maybe(vnlval),
	vnlval).
:- mode vn__choose_cheapest_loc(in, in, in, out) is det.

vn__choose_cheapest_loc([Loc | Locs], Stack0, Heap0, BestLoc) :-
	(
		Loc = vn_reg(_),
		BestLoc = Loc
	;
		Loc = vn_stackvar(_),
		vn__choose_cheapest_loc(Locs, yes(Loc), Heap0, BestLoc)
	;
		Loc = vn_framevar(_),
		vn__choose_cheapest_loc(Locs, yes(Loc), Heap0, BestLoc)
	;
		Loc = vn_succip,
		BestLoc = Loc
	;
		Loc = vn_maxfr,
		BestLoc = Loc
	;
		Loc = vn_curredoip,
		vn__choose_cheapest_loc(Locs, yes(Loc), Heap0, BestLoc)
	;
		Loc = vn_hp,
		BestLoc = Loc
	;
		Loc = vn_sp,
		BestLoc = Loc
	;
		Loc = vn_field(_, _, _),
		vn__choose_cheapest_loc(Locs, Stack0, yes(Loc), BestLoc)
	;
		Loc = vn_temp(_),
		BestLoc = Loc
	).
vn__choose_cheapest_loc([], Stack0, Heap0, BestLoc) :-
	( Stack0 = yes(Stack) ->
		BestLoc = Stack
	; Heap0 = yes(Heap) ->
		BestLoc = Heap
	;
		error("empty locations list in vn__choose_cheapest_loc")
	).

%-----------------------------------------------------------------------------%

:- pred vn__generate_assignment(vnlval, maybe(lval), vn, vn_tables, vn_tables,
	templocs, templocs, list(instruction)).
:- mode vn__generate_assignment(in, out, in, di, uo, di, uo, out) is det.

vn__generate_assignment(Vnlval, MaybeLval, Vn, Vn_tables0, Vn_tables,
		Templocs0, Templocs, Instrs) :-
	vn__lookup_current_value(Vnlval, Cur_vn, Vn_tables0),
	( Vn = Cur_vn ->
		MaybeLval = no,
		Vn_tables = Vn_tables0,
		Templocs = Templocs0,
		Instrs = []
	;
		% Only lvals on the heap must have their access path flushed,
		% but they cannot appear on the temploc list, so of the next
		% two calls is ok: at most one will modify temploc.
		vn__no_temploc(Vnlval, Templocs0, Templocs1),
		vn__flush_access_path(Vnlval, Lval, Vn_tables0, Vn_tables1,
			Templocs1, Templocs2, AccessInstrs),
		MaybeLval = yes(Lval),
		vn__flush_vn(Vn, Rval, yes, Vn_tables1, Vn_tables2,
			Templocs2, Templocs3, FlushInstrs),
		vn__maybe_save_prev_value(Vnlval, Vn_tables2, Vn_tables3,
			Templocs3, Templocs, SaveInstrs),
		vn__point_vnlval_to_vn(Vnlval, Vn, Vn_tables3, Vn_tables),
		Instr = assign(Lval, Rval) - "vn flush",
		list__condense([AccessInstrs, FlushInstrs, SaveInstrs, [Instr]],
			Instrs)
	).

%-----------------------------------------------------------------------------%

:- pred vn__flush_vn(vn, rval, bool, vn_tables, vn_tables,
	templocs, templocs, list(instruction)).
:- mode vn__flush_vn(in, out, in, di, uo, di, uo, out) is det.

vn__flush_vn(Vn, Rval, ImmedAssign, Vn_tables0, Vn_tables,
		Templocs0, Templocs, Instrs) :-
	vn__lookup_current_locs(Vn, Locs, Vn_tables0),
	( Locs = [] ->
		vn__lookup_use_count(Vn, Uses, Vn_tables0),
		( Uses > 0, ImmedAssign = no ->
			vn__choose_best_location(Vn, Vnlval, Vn_tables0,
				Templocs0, Templocs1),
			vn__generate_assignment(Vnlval, MaybeLval, Vn,
				Vn_tables0, Vn_tables1, Templocs1, Templocs,
				Instrs),
			(
				MaybeLval = yes(Lval),
				Rval = lval(Lval)
			;
				MaybeLval = no,
				error("vn__generate_assign didn't return an lval")
			)
		;
			vn__flush_vn_value(Vn, Rval, Vn_tables0, Vn_tables1,
				Templocs0, Templocs, Instrs)
		)
	;
		vn__choose_cheapest_loc(Locs, no, no, Loc),
		( Loc = vn_reg(R) ->
			Rval = lval(reg(R)),
			Vn_tables1 = Vn_tables0,
			Templocs = Templocs0,
			Instrs = []
		; Loc = vn_temp(N) ->
			Rval = lval(temp(N)),
			Vn_tables1 = Vn_tables0,
			Templocs = Templocs0,
			Instrs = []
		; vn__is_const_expr(Vn, Vn_tables0) ->
			vn__flush_vn_value(Vn, Rval, Vn_tables0, Vn_tables1,
				Templocs0, Templocs, Instrs)
		;
			vn__flush_access_path(Loc, Lval, Vn_tables0, Vn_tables1,
				Templocs0, Templocs, Instrs),
			Rval = lval(Lval)
		)
	),
	vn__decr_use_count(Vn, NewUseCount, Vn_tables1, Vn_tables),
	( NewUseCount > 0 ->
		true
		% NewlyFree = NewlyFree0
	;
		true
		% vn__lookup_current_locs(Vn, NewlyFree1, Vn_tables0)
		% list__append(NewlyFree0, NewlyFree1, NewlyFree)
	).

:- pred vn__flush_vn_value(vn, rval, vn_tables, vn_tables,
	templocs, templocs, list(instruction)).
:- mode vn__flush_vn_value(in, out, di, uo, di, uo, out) is det.

vn__flush_vn_value(Vn, Rval, Vn_tables0, Vn_tables, Templocs0, Templocs,
		Instrs) :-
	vn__lookup_defn(Vn, Vnrval, Vn_tables0),
	(
		Vnrval = vn_origlval(Vnlval),
		vn__lookup_current_value(Vnlval, CurVn, Vn_tables0),
		( Vn = CurVn ->
			% For code understandability, and for aesthetics,
			% prefer to take the value from its original home
			Loc = Vnlval
		;
			vn__lookup_current_locs(Vn, Locs, Vn_tables0),
			( Locs = [] ->
				error("cannot find copy of an origlval")
			;
				vn__choose_cheapest_loc(Locs, no, no, Loc)
			)
		),
		vn__flush_access_path(Loc, Lval, Vn_tables0, Vn_tables,
			Templocs0, Templocs, Instrs),
		Rval = lval(Lval)
	;
		Vnrval = vn_mkword(Tag, SubVn1),
		vn__flush_vn(SubVn1, Rval1, no, Vn_tables0, Vn_tables,
			Templocs0, Templocs, Instrs),
		Rval = mkword(Tag, Rval1)
	;
		Vnrval = vn_const(Const),
		Rval = const(Const),
		Vn_tables = Vn_tables0,
		Templocs = Templocs0,
		Instrs = []
	;
		Vnrval = vn_create(Tag, MaybeRvals, Label),
		Rval = create(Tag, MaybeRvals, Label),
		Vn_tables = Vn_tables0,
		Templocs = Templocs0,
		Instrs = []
	;
		Vnrval = vn_unop(Unop, SubVn1),
		vn__flush_vn(SubVn1, Rval1, no, Vn_tables0, Vn_tables,
			Templocs0, Templocs, Instrs),
		Rval = unop(Unop, Rval1)
	;
		Vnrval = vn_binop(Binop, SubVn1, SubVn2),
		vn__flush_vn(SubVn1, Rval1, no, Vn_tables0, Vn_tables1,
			Templocs0, Templocs1, Instrs1),
		vn__flush_vn(SubVn2, Rval2, no, Vn_tables1, Vn_tables,
			Templocs1, Templocs, Instrs2),
		Rval = binop(Binop, Rval1, Rval2),
		list__append(Instrs1, Instrs2, Instrs)
	).

%-----------------------------------------------------------------------------%

:- pred vn__flush_access_path(vnlval, lval, vn_tables, vn_tables,
	templocs, templocs, list(instruction)).
:- mode vn__flush_access_path(in, out, di, uo, di, uo, out) is det.

vn__flush_access_path(Vnlval, Lval, Vn_tables0, Vn_tables,
		Templocs0, Templocs, AccessInstrs) :-
	(
		Vnlval = vn_reg(Reg),
		Lval = reg(Reg),
		Vn_tables = Vn_tables0,
		Templocs = Templocs0,
		AccessInstrs = []
	;
		Vnlval = vn_stackvar(Slot),
		Lval = stackvar(Slot),
		Vn_tables = Vn_tables0,
		Templocs = Templocs0,
		AccessInstrs = []
	;
		Vnlval = vn_framevar(Slot),
		Lval = framevar(Slot),
		Vn_tables = Vn_tables0,
		Templocs = Templocs0,
		AccessInstrs = []
	;
		Vnlval = vn_succip,
		Lval = succip,
		Vn_tables = Vn_tables0,
		Templocs = Templocs0,
		AccessInstrs = []
	;
		Vnlval = vn_maxfr,
		Lval = maxfr,
		Vn_tables = Vn_tables0,
		Templocs = Templocs0,
		AccessInstrs = []
	;
		Vnlval = vn_curredoip,
		Lval = curredoip,
		Vn_tables = Vn_tables0,
		Templocs = Templocs0,
		AccessInstrs = []
	;
		Vnlval = vn_hp,
		Lval = hp,
		Vn_tables = Vn_tables0,
		Templocs = Templocs0,
		AccessInstrs = []
	;
		Vnlval = vn_sp,
		Lval = sp,
		Vn_tables = Vn_tables0,
		Templocs = Templocs0,
		AccessInstrs = []
	;
		Vnlval = vn_field(Tag, Vn1, Vn2),
		vn__flush_vn(Vn1, Rval1, no, Vn_tables0, Vn_tables1,
			Templocs0, Templocs1, AccessInstrs1),
		vn__flush_vn(Vn2, Rval2, no, Vn_tables1, Vn_tables,
			Templocs1, Templocs, AccessInstrs2),
		Lval = field(Tag, Rval1, Rval2),
		list__append(AccessInstrs1, AccessInstrs2, AccessInstrs)
	;
		Vnlval = vn_temp(Num),
		Lval = temp(Num),
		Vn_tables = Vn_tables0,
		Templocs = Templocs0,
		AccessInstrs = []
	).

:- pred vn__find_lvals_matching_vn(assoc_list(vnlval,vn), vn, list(vnlval)).
:- mode vn__find_lvals_matching_vn(in, in, out) is det.

vn__find_lvals_matching_vn([], _, []).
vn__find_lvals_matching_vn([Vnlval - Vn | Tail], MatchVn, Res) :-
	vn__find_lvals_matching_vn(Tail, MatchVn, Res1),
	( Vn = MatchVn ->
		Res = [Vnlval | Res1]
	;
		Res = Res1
	).

	% If the value currently stored in the vnlval is used elsewhere,
	% and if it cannot be recreated blind (or at least not cheaply),
	% then save the value somewhere else.

:- pred vn__maybe_save_prev_value(vnlval, vn_tables, vn_tables,
	templocs, templocs, list(instruction)).
:- mode vn__maybe_save_prev_value(in, di, uo, di, uo, out) is det.

vn__maybe_save_prev_value(Vnlval, Vn_tables0, Vn_tables,
		Templocs0, Templocs, Instrs) :-
	vn__lookup_current_value(Vnlval, Vn, Vn_tables0),
	vn__lookup_use_count(Vn, Uses, Vn_tables0),
	(
		Uses > 0,
		not vn__is_const_expr(Vn, Vn_tables0),
		vn__lookup_current_locs(Vn, Locs0, Vn_tables0),
		list__delete_all(Locs0, Vnlval, Locs),
		Locs = []
	->
		vn__next_temploc(Templocs0, Templocs1, SaveVnlval),
		vn__generate_assignment(SaveVnlval, _, Vn,
			Vn_tables0, Vn_tables, Templocs1, Templocs, Instrs)
	;
		Vn_tables = Vn_tables0,
		Templocs = Templocs0,
		Instrs = []
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Fixup the code sequences flushed above. Insert any missing
	% stack pointer increments and decrements, and put assignments to
	% hp back into their proper form.

:- pred vn__insert_incr_sp(int, list(instruction), list(instruction)).
:- mode vn__insert_incr_sp(in, in, out) is det.

vn__insert_incr_sp(N, Instrs0, Instrs) :-
	Instrs = [incr_sp(N) - "vn flush" | Instrs0].

:- pred vn__insert_decr_sp(int, list(instruction), list(instruction)).
:- mode vn__insert_decr_sp(in, in, out) is det.

vn__insert_decr_sp(N, Instrs0, Instrs) :-
	list__reverse(Instrs0, RevInstrs0),
	(
		RevInstrs0 = [],
		Instrs = [decr_sp(N) - "vn flush"]
	;
		RevInstrs0 = [Last | Rest],
		Last = Ulast - _,
		( Ulast = call(_, _, _) ->
			RevInstrs = [Last, decr_sp(N) - "vn flush" | Rest]
		; Ulast = call_closure(_, _, _) ->
			RevInstrs = [Last, decr_sp(N) - "vn flush" | Rest]
		; Ulast = goto(_) ->
			RevInstrs = [Last, decr_sp(N) - "vn flush" | Rest]
		; Ulast = computed_goto(_, _) ->
			RevInstrs = [Last, decr_sp(N) - "vn flush" | Rest]
		;
			RevInstrs = [decr_sp(N) - "vn flush" | RevInstrs0]
		),
		list__reverse(RevInstrs, Instrs)
	).

:- pred vn__find_incr_hp(list(instruction), list(instruction)).
:- mode vn__find_incr_hp(in, out) is det.

vn__find_incr_hp([], []).
vn__find_incr_hp([Instr0 | Instrs0], [Instr | Instrs]) :-
	( Instr0 = assign(hp, Rval) - _ ->
		( Rval = binop((+), lval(hp), const(int_const(N))) ->
			Instr = incr_hp(N) - "vn flush"
		; Rval = lval(stackvar(_)) ->
			Instr = Instr0
		; Rval = lval(framevar(_)) ->
			Instr = Instr0
		;
			error("tell zs this form of assignment to hp is possible")
		)
	;
		Instr = Instr0
	),
	vn__find_incr_hp(Instrs0, Instrs).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
