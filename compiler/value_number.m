%-----------------------------------------------------------------------------%

% Value_number.nl - optimization of straight-line LLDS code.

% Main author: zs.

% XXX: I changed the incr_hp instruction to a heap_alloc rval.
%	Are heap_alloc rvals handled properly? -fjh.

% XXX: must include all heap locs among the dependencies of each ctrl node

% XXX: when looking up live vals at labels, if it is not in vn_table,
% put it in as an origlval

% XXX create a new predicate for recording assignments
% it should check for assigning the value the register already holds

%-----------------------------------------------------------------------------%

:- module value_number.

:- interface.
:- import_module llds, list.

:- pred value_number__optimize(list(instruction), list(instruction)).
:- mode value_number__optimize(in, out) is det.

% the rest are exported only for debugging.

:- type lvalset == bintree_set(lval).
:- type livemap == map(label, lvalset).

:- type vn == int.

:- type lval_to_vn_table == map(vn_lval, vn).
:- type rval_to_vn_table == map(vn_rval, vn).
:- type vn_to_rval_table == map(vn, vn_rval).
:- type vn_to_uses_table == map(vn, int).
:- type vn_to_locs_table == map(vn, list(vn_lval)).
:- type loc_to_vn_table  == map(vn_lval, vn).

:- type origlist == assoc_list(vn_lval, int).
:- type origmap == map(vn_lval, int).
:- type vn_to_orig_table == map(vn, origmap).

:- type vn_tables --->	vn_tables(vn, lval_to_vn_table, rval_to_vn_table,
				vn_to_rval_table, vn_to_uses_table,
				vn_to_locs_table, loc_to_vn_table).

:- type vn_lval		--->	vn_reg(reg)
			;	vn_stackvar(int)
			;	vn_framevar(int)
			;	vn_succip
			;	vn_maxfr
			;	vn_curredoip
			;	vn_hp
			;	vn_sp
			;	vn_field(tag, vn, vn)		% lval
			;	vn_temp(int).

			% these lvals do not have vn_lval parallels
			%	lvar(var)

:- type vnlvalset == bintree_set(vn_lval).

:- type vn_rval		--->	vn_origlval(vn_lval)
			;	vn_mkword(tag, vn)		% rval
			;	vn_const(rval_const)
			;	vn_create(tag, list(maybe(rval)), int)
			;	vn_heap_alloc(vn)		% rval
			;       vn_field(tag, vn, vn)		% rval
			;	vn_unop(unary_op, vn)		% rval
			;	vn_binop(binary_op, vn, vn).	% rval, rval

			% these rvals do not have vn_rval parallels
			%	var(var)

:- type vn_node		--->	node_shared(vn)
			;	node_lval(vn_lval)
			;	node_origlval(vn_lval)
			;	node_ctrl(int).

:- type vn_instr	--->	vn_call(code_addr, code_addr)
			;	vn_mkframe(string, int, code_addr)
			;	vn_modframe(code_addr)
			;	vn_label(label)
			;	vn_goto(code_addr)
			;	vn_computed_goto(vn, list(label))
			;	vn_if_val(vn, code_addr).

:- type ctrlmap		== map(int, vn_instr).
:- type flushmap	== map(int, flushmapentry).
:- type flushmapentry	== map(vn_lval, vn).

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
		Uinstr = call_closure(_, _),
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
		Uinstr = call(_, _),
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
	Instr0 = Uinstr0 - Comment,
	(
		Uinstr0 = comment(_),
		vn__opt_block(Instrs0, Vn_tables0, Livemap,
			Templocs0, Livevals0, Incrsp, Decrsp,
			Ctrlmap0, Flushmap0, Ctrl0, [Instr0 | Prev], Instrs)
	;
		Uinstr0 = livevals(Terminate, Livevals),
		vn__handle_livevals(Terminate, Livevals, Livevals0, Livevals1,
			Vn_tables0, Vn_tables1),
		vn__opt_block(Instrs0, Vn_tables1, Livemap,
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
		Uinstr0 = call_closure(_, _),
		error("Zoltan please fixme (#2)")
	;
		Uinstr0 = call(ProcAddr, ReturnAddr),
		vn__new_ctrl_node(vn_call(ProcAddr, ReturnAddr), Livemap,
			Vn_tables0, Vn_tables1, Livevals0, Livevals1,
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
		vn__use_rval_find_vn(Rval, Vn, Vn_tables0, Vn_tables1),
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
		vn__use_rval_find_vn(Rval, Vn, Vn_tables0, Vn_tables1),
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
	vn__keep_live_nodes(Live, Vn_tables0, Vn_tables1),
	(
		vn__make_req_order(Ctrlmap, Flushmap, Vn_tables1,
			MustSuccmap, MustPredmap)
	->
		vn__make_ctrlmaps(0, Ctrlmap, Vn_tables1, Vn_tables2,
			MustSuccmap, Succmap0, MustPredmap, Predmap0),
		vn__make_relmaps(Live, Vn_tables2, Vn_tables3,
			Succmap0, Succmap, Predmap0, Predmap),
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
		error("Zoltan please fixme (#3)"),	% XXX
		%%% vn__find_incr_hp(Instrs2, Instrs3),
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

:- pred vn__make_req_order(ctrlmap, flushmap, vn_tables,
	relmap(vn_node), relmap(vn_node)).
:- mode vn__make_req_order(in, in, in, out, out) is semidet.

vn__make_req_order(Ctrlmap, Flushmap, Vn_tables, MustSuccmap, MustPredmap) :-
	map__init(MustSuccmap0),
	map__init(MustPredmap0),
	vn__make_req_order_2(0, Ctrlmap, Flushmap, Vn_tables,
		MustSuccmap0, MustSuccmap, MustPredmap0, MustPredmap).

:- pred vn__make_req_order_2(int, ctrlmap, flushmap, vn_tables,
	relmap(vn_node), relmap(vn_node), relmap(vn_node), relmap(vn_node)).
:- mode vn__make_req_order_2(in, in, in, in, di, uo, di, uo) is semidet.

vn__make_req_order_2(Ctrl, Ctrlmap, Flushmap, Vn_tables,
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
		vn__make_req_order_2(NextCtrl, Ctrlmap, Flushmap, Vn_tables,
			MustSuccmap2, MustSuccmap, MustPredmap2, MustPredmap)
	;
		MustSuccmap = MustSuccmap0,
		MustPredmap = MustPredmap0
	).

:- pred vn__record_ctrl_deps(assoc_list(vn_lval, vn), vn_node, vn_tables,
	relmap(vn_node), relmap(vn_node), relmap(vn_node), relmap(vn_node)).
:- mode vn__record_ctrl_deps(in, in, in, di, uo, di, uo) is semidet.

vn__record_ctrl_deps([], _Sink, _Vn_tables,
		MustSuccmap, MustSuccmap, MustPredmap, MustPredmap).
vn__record_ctrl_deps([Vn_lval - Vn | FlushList], Sink, Vn_tables,
		MustSuccmap0, MustSuccmap, MustPredmap0, MustPredmap) :-
	vn__lookup_desired_value(Vn_lval, Des_vn, Vn_tables),
	Vn = Des_vn,
	vn__add_link(node_lval(Vn_lval), Sink,
		MustSuccmap0, MustSuccmap1, MustPredmap0, MustPredmap1),
	vn__record_ctrl_deps(FlushList, Sink, Vn_tables,
		MustSuccmap1, MustSuccmap, MustPredmap1, MustPredmap).

%-----------------------------------------------------------------------------%

:- pred vn__make_ctrlmaps(int, ctrlmap, vn_tables, vn_tables,
	relmap(vn_node), relmap(vn_node), relmap(vn_node), relmap(vn_node)).
:- mode vn__make_ctrlmaps(in, in, di, uo, di, uo, di, uo) is det.

vn__make_ctrlmaps(Ctrl, Ctrlmap, Vn_tables0, Vn_tables,
		Succmap0, Succmap, Predmap0, Predmap) :-
	( map__search(Ctrlmap, Ctrl, Vn_instr) ->
		(
			Vn_instr = vn_call(_, _),
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
		vn__make_ctrlmaps(NextCtrl, Ctrlmap, Vn_tables1, Vn_tables,
			Succmap1, Succmap, Predmap1, Predmap)
	;
		Succmap = Succmap0,
		Predmap = Predmap0,
		Vn_tables = Vn_tables0
	).

%-----------------------------------------------------------------------------%

:- pred vn__make_relmaps(list(vn_lval), vn_tables, vn_tables,
	relmap(vn_node), relmap(vn_node), relmap(vn_node), relmap(vn_node)).
:- mode vn__make_relmaps(in, di, uo, di, uo, di, uo) is det.

vn__make_relmaps(Livevals, Vn_tables0, Vn_tables,
		Succmap0, Succmap, Predmap0, Predmap) :-
	vn__make_rels(Livevals, Vn_tables0, Vn_tables,
		Succmap0, Succmap1, Predmap0, Predmap1),
	vn__use_before_redef(Succmap1, Succmap, Predmap1, Predmap).

:- pred vn__make_rels(list(vn_lval), vn_tables, vn_tables,
	relmap(vn_node), relmap(vn_node), relmap(vn_node), relmap(vn_node)).
:- mode vn__make_rels(in, di, uo, di, uo, di, uo) is det.

vn__make_rels([], Vn_tables, Vn_tables, Succmap, Succmap, Predmap, Predmap).
vn__make_rels([Lval | Lvals], Vn_tables0, Vn_tables,
		Succmap0, Succmap, Predmap0, Predmap) :-
	vn__make_rel(Lval, Vn_tables0, Vn_tables1,
		Succmap0, Succmap1, Predmap0, Predmap1),
	vn__make_rels(Lvals, Vn_tables1, Vn_tables,
		Succmap1, Succmap, Predmap1, Predmap).

:- pred vn__make_rel(vn_lval, vn_tables, vn_tables,
	relmap(vn_node), relmap(vn_node), relmap(vn_node), relmap(vn_node)).
:- mode vn__make_rel(in, di, uo, di, uo, di, uo) is det.

vn__make_rel(Lval, Vn_tables0, Vn_tables,
		Succmap0, Succmap, Predmap0, Predmap) :-
	Vn_tables0 = vn_tables(_Next_vn0,
		Lval_to_vn_table0, _Rval_to_vn_table0,
		_Vn_to_rval_table0, _Vn_to_uses_table0,
		_Vn_to_locs_table0, _Loc_to_vn_table0),
	map__lookup(Lval_to_vn_table0, Lval, Vn),
	vn__find_links(Vn, node_lval(Lval), Vn_tables0, Vn_tables,
		Succmap0, Succmap, Predmap0, Predmap).

:- pred vn__find_links(vn, vn_node, vn_tables, vn_tables,
	relmap(vn_node), relmap(vn_node), relmap(vn_node), relmap(vn_node)).
:- mode vn__find_links(in, in, di, uo, di, uo, di, uo) is det.

vn__find_links(Vn, Sink, Vn_tables0, Vn_tables,
		Succmap0, Succmap, Predmap0, Predmap) :-
	Vn_tables0 = vn_tables(_Next_vn0,
		_Lval_to_vn_table0, _Rval_to_vn_table0,
		Vn_to_rval_table0, Vn_to_uses_table0,
		_Vn_to_locs_table0, _Loc_to_vn_table0),
	map__lookup(Vn_to_uses_table0, Vn, Uses0),
	( Uses0 > 1, \+ Sink = node_shared(_) ->
		vn__add_link(node_shared(Vn), Sink,
			Succmap0, Succmap1, Predmap0, Predmap1),
		vn__find_links(Vn, node_shared(Vn), Vn_tables0, Vn_tables,
			Succmap1, Succmap, Predmap1, Predmap)
	;
		map__lookup(Vn_to_rval_table0, Vn, Vn_rval),
		(
			Vn_rval = vn_origlval(Vn_lval),
			opt_util__vnlval_access_vn(Vn_lval, Access_vn),
			( Access_vn = yes(Sub_vn) ->
				vn__find_links(Sub_vn, Sink, Vn_tables0, Vn_tables,
					Succmap0, Succmap1, Predmap0, Predmap1)
			;
				Succmap1 = Succmap0,
				Predmap1 = Predmap0,
				Vn_tables = Vn_tables0
			),
			vn__add_link(node_origlval(Vn_lval), Sink,
				Succmap1, Succmap, Predmap1, Predmap)
		;
			Vn_rval = vn_mkword(_Tag1, Sub_vn),
			vn__find_links(Sub_vn, Sink, Vn_tables0, Vn_tables,
				Succmap0, Succmap, Predmap0, Predmap)
		;
			Vn_rval = vn_const(_Const),
			Succmap = Succmap0,
			Predmap = Predmap0,
			Vn_tables = Vn_tables0
		;
			Vn_rval = vn_create(_Tag2, _Args, _Label),
			Succmap = Succmap0,
			Predmap = Predmap0,
			Vn_tables = Vn_tables0
		;
			Vn_rval = vn_heap_alloc(Sub_vn),
			vn__find_links(Sub_vn, Sink, Vn_tables0, Vn_tables,
				Succmap0, Succmap, Predmap0, Predmap)
		;
			Vn_rval = vn_field(_Tag3, Sub_vn, _Slot),
			vn__find_links(Sub_vn, Sink, Vn_tables0, Vn_tables,
				Succmap0, Succmap, Predmap0, Predmap)
		;
			Vn_rval = vn_unop(_Unop, Sub_vn),
			vn__find_links(Sub_vn, Sink, Vn_tables0, Vn_tables,
				Succmap0, Succmap, Predmap0, Predmap)
		;
			Vn_rval = vn_binop(_Binop, Sub_vn1, Sub_vn2),
			vn__find_links(Sub_vn1, Sink, Vn_tables0, Vn_tables1,
				Succmap0, Succmap1, Predmap0, Predmap1),
			vn__find_links(Sub_vn2, Sink, Vn_tables1, Vn_tables,
				Succmap1, Succmap, Predmap1, Predmap)
		)
	).

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

	% Convert the output of the approximate topological sort
	% into the proposed order of evaluation. We must make sure
	% that the condition, if any, comes last.

	% We also try to put registers before stack variables, since this
	% minimizes memory traffic. For example, the sequence
	%	r1 = field(...); stackvar(1) = r1
	% is cheaper than the sequence
	%	stackvar(1) = field(...);  r1 = stackvar(1)
	% and the C compiler may not be able to turn the latter into
	% the former.

:- pred vn__blockorder_to_order(list(list(vn_node)), int, list(vn_node)).
:- mode vn__blockorder_to_order(in, in, out) is det.

vn__blockorder_to_order(BlockOrder, N, Order) :-
	vn__order_equal_lists(BlockOrder, GoodBlockOrder),
	list__condense(GoodBlockOrder, Order0),
	vn__find_last_ctrl(Order0, N, Ctrl, Order1),
	( Ctrl = [_] ->
		true
	; Ctrl = [] ->
		error("last ctrl node does not exist")
	;
		error("last ctrl node exists in multiples")
	),
	list__append(Order1, Ctrl, Order).

:- pred vn__order_equal_lists(list(list(vn_node)), list(list(vn_node))).
:- mode vn__order_equal_lists(di, uo) is det.

vn__order_equal_lists([], []).
vn__order_equal_lists([Block | Blocks], [GoodBlock | GoodBlocks]) :-
	vn__order_equals(Block, GoodBlock),
	vn__order_equal_lists(Blocks, GoodBlocks).

:- pred vn__order_equals(list(vn_node), list(vn_node)).
:- mode vn__order_equals(di, uo) is det.

vn__order_equals(Order0, Order) :-
	vn__find_regs(Order0, Regs, Order1),
	list__append(Regs, Order1, Order).

:- pred vn__find_regs(list(vn_node), list(vn_node), list(vn_node)).
:- mode vn__find_regs(di, out, uo) is det.

vn__find_regs([], [], []).
vn__find_regs([Node0 | Nodes0], Regs, Nodes) :-
	vn__find_regs(Nodes0, Regs1, Nodes1),
	( Node0 = node_lval(vn_reg(_)) ->
		Regs = [Node0 | Regs1],
		Nodes = Nodes1
	;
		Regs = Regs1,
		Nodes = [Node0 | Nodes1]
	).

:- pred vn__find_last_ctrl(list(vn_node), int, list(vn_node), list(vn_node)).
:- mode vn__find_last_ctrl(di, in, out, uo) is det.

vn__find_last_ctrl([], _, [], []).
vn__find_last_ctrl([Node0 | Nodes0], N, Ctrl, Nodes) :-
	vn__find_last_ctrl(Nodes0, N, Ctrl1, Nodes1),
	( Node0 = node_ctrl(N) ->
		Ctrl = [Node0 | Ctrl1],
		Nodes = Nodes1
	;
		Ctrl = Ctrl1,
		Nodes = [Node0 | Nodes1]
	).

%-----------------------------------------------------------------------------%

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
	( Sink = node_lval(Vn_lval) ->
		map__lookup(Succmap0, node_origlval(Vn_lval), Users),
		vn__add_links(Users, Sink,
			Succmap0, Succmap1, Predmap0, Predmap1)
	;
		Succmap1 = Succmap0,
		Predmap1 = Predmap0
	),
	vn__use_sinks_before_redef(Sinks, Succmap1, Succmap, Predmap1, Predmap).

%-----------------------------------------------------------------------------%

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

:- pred vn__flush_node(vn_node, ctrlmap, vn_tables, vn_tables,
	templocs, templocs, list(instruction)).
:- mode vn__flush_node(in, in, di, uo, di, uo, out) is det.

vn__flush_node(Node, Ctrlmap, Vn_tables0, Vn_tables, Templocs0, Templocs,
		Instrs) :-
	(
		Node = node_shared(Vn),
		vn__store_in_best_location(Vn, Vn_tables0, Vn_tables,
			Templocs0, Templocs, Instrs)
	;
		Node = node_lval(Vn_lval),
		vn__flush_access_path(Vn_lval, Lval, Vn_tables0, Vn_tables1,
			Templocs0, Templocs1, AccessInstrs),
		vn__lookup_desired_value(Vn_lval, Vn, Vn_tables1),
		vn__flush_vn(Vn, Rval, Vn_tables1, Vn_tables2,
			Templocs1, Templocs2, FlushInstrs),
		vn__no_temploc(Lval, Templocs2, Templocs3),
		vn__maybe_save_prev_value(Vn_lval, Vn_tables2, Vn_tables3,
			Templocs3, Templocs, SaveInstrs),
		vn__point_vnlval_to_vn(Vn_lval, Vn, Vn_tables3, Vn_tables),
		Instr = assign(Lval, Rval) - "vn flush",
		list__condense([SaveInstrs, AccessInstrs, FlushInstrs, [Instr]],
			Instrs)
	;
		Node = node_origlval(Vn_lval),
		error("node_origlval found in vn__flush_node")
	;
		Node = node_ctrl(N),
		map__lookup(Ctrlmap, N, Vn_instr),
		(
			Vn_instr = vn_call(ProcAddr, ReturnAddr),
			Instrs = [call(ProcAddr, ReturnAddr) - ""],
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
			vn__flush_vn(Vn, Rval, Vn_tables0, Vn_tables,
				Templocs0, Templocs, FlushInstrs),
			Instr = computed_goto(Rval, Labels) - "",
			list__append(FlushInstrs, [Instr], Instrs)
		;
			Vn_instr = vn_if_val(Vn, TargetAddr),
			vn__flush_vn(Vn, Rval, Vn_tables0, Vn_tables,
				Templocs0, Templocs, FlushInstrs),
			Instr = if_val(Rval, TargetAddr) - "",
			list__append(FlushInstrs, [Instr], Instrs)
		)
	).

%-----------------------------------------------------------------------------%

% XXX Instrs will always be [], and neither Vn_tables nor Temploc will change

:- pred vn__flush_vn(vn, rval, vn_tables, vn_tables,
	templocs, templocs, list(instruction)).
:- mode vn__flush_vn(in, out, di, uo, di, uo, out) is det.

vn__flush_vn(Vn, Rval, Vn_tables0, Vn_tables, Templocs0, Templocs, Instrs) :-
	Vn_tables0 = vn_tables(_Next_vn0,
		_Lval_to_vn_table0, _Rval_to_vn_table0,
		Vn_to_rval_table0, _Vn_to_uses_table0,
		Vn_to_locs_table0, _Loc_to_vn_table0),
	map__lookup(Vn_to_locs_table0, Vn, Locs),
	( Locs = [] ->
		% XXX
		% should look for whether this value is needed several times
		% if yes, we should save the Rval in a new temp variable
		map__lookup(Vn_to_rval_table0, Vn, Vn_rval),
		vn__flush_vnrval(Vn_rval, Rval, Vn_tables0, Vn_tables,
			Templocs0, Templocs, Instrs)
	;
		vn__choose_cheapest_loc(Locs, no, no, Loc),
		vn__flush_access_path(Loc, Lval, Vn_tables0, Vn_tables,
			Templocs0, Templocs, Instrs),
		Rval = lval(Lval)
	).

:- pred vn__flush_vnrval(vn_rval, rval, vn_tables, vn_tables,
	templocs, templocs, list(instruction)).
:- mode vn__flush_vnrval(in, out, di, uo, di, uo, out) is det.

vn__flush_vnrval(Vn_rval, Rval, Vn_tables0, Vn_tables, Templocs0, Templocs,
		Instrs) :-
	(
		Vn_rval = vn_origlval(_Vn_lval),
		Vn_tables0 = vn_tables(_Next_vn0,
			_Lval_to_vn_table0, Rval_to_vn_table0,
			_Vn_to_rval_table0, _Vn_to_uses_table0,
			Vn_to_locs_table0, _Loc_to_vn_table0),
		map__lookup(Rval_to_vn_table0, Vn_rval, Vn),
		map__lookup(Vn_to_locs_table0, Vn, Locs),
		vn__choose_cheapest_loc(Locs, no, no, Loc),
		vn__flush_access_path(Loc, Lval, Vn_tables0, Vn_tables,
			Templocs0, Templocs, Instrs),
		Rval = lval(Lval)
	;
		Vn_rval = vn_mkword(Tag, Sub_vn1),
		vn__flush_vn(Sub_vn1, Rval1, Vn_tables0, Vn_tables,
			Templocs0, Templocs, Instrs),
		Rval = mkword(Tag, Rval1)
	;
		Vn_rval = vn_const(Const),
		Rval = const(Const),
		Vn_tables = Vn_tables0,
		Templocs = Templocs0,
		Instrs = []
	;
		Vn_rval = vn_create(Tag, MaybeRvals, Label),
		Rval = create(Tag, MaybeRvals, Label),
		Vn_tables = Vn_tables0,
		Templocs = Templocs0,
		Instrs = []
	;
		Vn_rval = vn_heap_alloc(Sub_vn1),
		vn__flush_vn(Sub_vn1, Rval1, Vn_tables0, Vn_tables,
			Templocs0, Templocs, Instrs),
		Rval = heap_alloc(Rval1)
	;
		Vn_rval = vn_field(Tag, Sub_vn1, Sub_vn2),
		vn__flush_vn(Sub_vn1, Rval1, Vn_tables0, Vn_tables1,
			Templocs0, Templocs1, Instrs1),
		vn__flush_vn(Sub_vn2, Rval2, Vn_tables1, Vn_tables,
			Templocs1, Templocs, Instrs2),
		Rval = lval(field(Tag, Rval1, Rval2)),
		list__append(Instrs1, Instrs2, Instrs)
	;
		Vn_rval = vn_unop(Unop, Sub_vn1),
		vn__flush_vn(Sub_vn1, Rval1, Vn_tables0, Vn_tables,
			Templocs0, Templocs, Instrs),
		Rval = unop(Unop, Rval1)
	;
		Vn_rval = vn_binop(Binop, Sub_vn1, Sub_vn2),
		vn__flush_vn(Sub_vn1, Rval1, Vn_tables0, Vn_tables1,
			Templocs0, Templocs1, Instrs1),
		vn__flush_vn(Sub_vn2, Rval2, Vn_tables1, Vn_tables,
			Templocs1, Templocs, Instrs2),
		Rval = binop(Binop, Rval1, Rval2),
		list__append(Instrs1, Instrs2, Instrs)
	).

:- pred vn__choose_cheapest_loc(list(vn_lval), maybe(vn_lval), maybe(vn_lval),
	vn_lval).
:- mode vn__choose_cheapest_loc(in, in, in, out) is det.

vn__choose_cheapest_loc(Locs0, Stack0, Heap0, BestLoc) :-
	( Locs0 = [Loc | Locs1] ->
		( ( Loc = vn_reg(_) ; Loc = vn_temp(_) ) ->
			BestLoc = Loc
		; ( Loc = vn_stackvar(_) ; Loc = vn_framevar(_) ) ->
			vn__choose_cheapest_loc(Locs1, yes(Loc), Heap0, BestLoc)
		;
			vn__choose_cheapest_loc(Locs1, Stack0, yes(Loc), BestLoc)
		)
	;
		( Stack0 = yes(Stack) ->
			BestLoc = Stack
		; Heap0 = yes(Heap) ->
			BestLoc = Heap
		;
			error("no reg, stack or heap in vn__choose_cheapest_loc")
		)
	).

%-----------------------------------------------------------------------------%

:- pred vn__flush_access_path(vn_lval, lval, vn_tables, vn_tables,
	templocs, templocs, list(instruction)).
:- mode vn__flush_access_path(in, out, di, uo, di, uo, out) is det.

vn__flush_access_path(Vn_lval, Lval, Vn_tables0, Vn_tables,
		Templocs0, Templocs, AccessInstrs) :-
	(
		Vn_lval = vn_reg(Reg),
		Lval = reg(Reg),
		Vn_tables = Vn_tables0,
		Templocs = Templocs0,
		AccessInstrs = []
	;
		Vn_lval = vn_stackvar(Slot),
		Lval = stackvar(Slot),
		Vn_tables = Vn_tables0,
		Templocs = Templocs0,
		AccessInstrs = []
	;
		Vn_lval = vn_framevar(Slot),
		Lval = framevar(Slot),
		Vn_tables = Vn_tables0,
		Templocs = Templocs0,
		AccessInstrs = []
	;
		Vn_lval = vn_succip,
		Lval = succip,
		Vn_tables = Vn_tables0,
		Templocs = Templocs0,
		AccessInstrs = []
	;
		Vn_lval = vn_maxfr,
		Lval = maxfr,
		Vn_tables = Vn_tables0,
		Templocs = Templocs0,
		AccessInstrs = []
	;
		Vn_lval = vn_curredoip,
		Lval = curredoip,
		Vn_tables = Vn_tables0,
		Templocs = Templocs0,
		AccessInstrs = []
	;
		Vn_lval = vn_hp,
		Lval = hp,
		Vn_tables = Vn_tables0,
		Templocs = Templocs0,
		AccessInstrs = []
	;
		Vn_lval = vn_sp,
		Lval = sp,
		Vn_tables = Vn_tables0,
		Templocs = Templocs0,
		AccessInstrs = []
	;
		Vn_lval = vn_field(Tag, Vn1, Vn2),
		vn__flush_vn(Vn1, Rval1, Vn_tables0, Vn_tables1,
			Templocs0, Templocs1, AccessInstrs1),
		vn__flush_vn(Vn2, Rval2, Vn_tables1, Vn_tables,
			Templocs1, Templocs, AccessInstrs2),
		Lval = field(Tag, Rval1, Rval2),
		list__append(AccessInstrs1, AccessInstrs2, AccessInstrs)
	;
		Vn_lval = vn_temp(Num),
		Lval = temp(Num),
		Vn_tables = Vn_tables0,
		Templocs = Templocs0,
		AccessInstrs = []
	).

:- pred vn__lookup_desired_value(vn_lval, vn, vn_tables).
:- mode vn__lookup_desired_value(in, out, in) is det.

vn__lookup_desired_value(Vn_lval, Vn, Vn_tables) :-
	Vn_tables = vn_tables(_Next_vn,
		Lval_to_vn_table,  _Rval_to_vn_table,
		_Vn_to_rval_table, _Vn_to_uses_table,
		_Vn_to_locs_table, _Loc_to_vn_table),
	map__lookup(Lval_to_vn_table, Vn_lval, Vn).

:- pred vn__point_vnlval_to_vn(vn_lval, vn, vn_tables, vn_tables).
:- mode vn__point_vnlval_to_vn(in, in, di, uo) is det.

vn__point_vnlval_to_vn(Vn_lval, Vn, Vn_tables0, Vn_tables) :-
	Vn_tables0 = vn_tables(Next_vn0,
		Lval_to_vn_table0,  Rval_to_vn_table0,
		Vn_to_rval_table0, Vn_to_uses_table0,
		Vn_to_locs_table0, Loc_to_vn_table0),
	% change the forward mapping
	map__lookup(Loc_to_vn_table0, Vn_lval, Old_vn),
	map__set(Loc_to_vn_table0, Vn_lval, Vn, Loc_to_vn_table1),
	% change the reverse mapping
	map__lookup(Vn_to_locs_table0, Old_vn, Old_locs0),
	list__delete_all(Old_locs0, Vn_lval, Old_locs1),
	map__set(Vn_to_locs_table0, Old_vn, Old_locs1, Vn_to_locs_table1),
	Vn_tables = vn_tables(Next_vn0,
		Lval_to_vn_table0,  Rval_to_vn_table0,
		Vn_to_rval_table0, Vn_to_uses_table0,
		Vn_to_locs_table1, Loc_to_vn_table1).

	% check for constants, for vars already holding the value etc

:- pred vn__store_in_best_location(vn, vn_tables, vn_tables,
	templocs, templocs, list(instruction)).
:- mode vn__store_in_best_location(in, di, uo, di, uo, out) is det.

vn__store_in_best_location(Vn, Vn_tables0, Vn_tables, Templocs0, Templocs,
		Instrs) :-
	Vn_tables0 = vn_tables(_Next_vn,
		Lval_to_vn_table0,  _Rval_to_vn_table0,
		_Vn_to_rval_table0, _Vn_to_uses_table0,
		_Vn_to_locs_table0, _Loc_to_vn_table0),
	map__to_assoc_list(Lval_to_vn_table0, Lval_vn_list),
	vn__find_lvals_using_vn(Lval_vn_list, Vn, Lvals),
	vn__flush_vn(Vn, Rval, Vn_tables0, Vn_tables1,
		Templocs0, Templocs1, FlushInstrs),
	vn__select_best_location(Lvals, Save_vn_lval, Vn_tables1, Vn_tables2),
	vn__flush_access_path(Save_vn_lval, Save_lval, Vn_tables2, Vn_tables3,
		Templocs1, Templocs, AccessInstrs),
	vn__point_vnlval_to_vn(Save_vn_lval, Vn, Vn_tables3, Vn_tables),
	string__int_to_base_string(Vn, 10, Num),
	string__append("save of vn #", Num, Comment),
	list__condense([FlushInstrs, AccessInstrs,
		[assign(Save_lval, Rval) - Comment]], Instrs).

:- pred vn__find_lvals_using_vn(assoc_list(vn_lval,vn), vn, list(vn_lval)).
:- mode vn__find_lvals_using_vn(in, in, out) is det.

vn__find_lvals_using_vn([], _, []).
vn__find_lvals_using_vn([Vnlval - Vn | Tail], MatchVn, Res) :-
	vn__find_lvals_using_vn(Tail, MatchVn, Res1),
	( Vn = MatchVn ->
		Res = [Vnlval | Res1]
	;
		Res = Res1
	).

:- pred vn__select_best_location(list(vn_lval), vn_lval, vn_tables, vn_tables).
:- mode vn__select_best_location(in, out, di, uo) is det.

	% Give preference to fast lvals
	% and to lvals holding vns with 0 refcounts.
	% But not yet.

vn__select_best_location(Lvals, Select, Vn_tables0, Vn_tables) :-
	( Lvals = [First | _] ->
		Select = First,
		Vn_tables = Vn_tables0
	;
		error("empty lval list in select_best_location")
	).

:- pred vn__maybe_save_prev_value(vn_lval, vn_tables, vn_tables,
	templocs, templocs, list(instruction)).
:- mode vn__maybe_save_prev_value(in, di, uo, di, uo, out) is det.

vn__maybe_save_prev_value(Vn_lval, Vn_tables0, Vn_tables,
		Templocs0, Templocs, Instrs) :-
	Vn_tables0 = vn_tables(_Next_vn0,
		Lval_to_vn_table0, _Rval_to_vn_table0,
		_Vn_to_rval_table0, Vn_to_uses_table0,
		_Vn_to_locs_table0, _Loc_to_vn_table0),
	map__lookup(Lval_to_vn_table0, Vn_lval, Vn),
	map__lookup(Vn_to_uses_table0, Vn, Uses),
	( Uses > 0 ->
		vn__next_temploc(Templocs0, Templocs1, Save_lval),
		vn__lval_to_vnlval(Save_lval, yes, Save_vn_lval,
			Vn_tables0, Vn_tables1),
		vn__flush_vn(Vn, Rval, Vn_tables1, Vn_tables2,
			Templocs1, Templocs, FlushInstrs),
		vn__point_vnlval_to_vn(Save_vn_lval, Vn, Vn_tables2, Vn_tables),
		string__int_to_base_string(Vn, 10, Num),
		string__append("save of vn #", Num, Comment),
		list__append(FlushInstrs, [assign(Save_lval, Rval) - Comment], Instrs)
	;
		Vn_tables = Vn_tables0,
		Templocs = Templocs0,
		Instrs = []
	).

%-----------------------------------------------------------------------------%

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
		( Ulast = call(_, _) ->
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

/*****
	XXX Zoltan please fix this
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
******/

%-----------------------------------------------------------------------------%
