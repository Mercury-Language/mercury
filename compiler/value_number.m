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
			;	vn_field(tag, vn, int)		% lval
			;	vn_temp(int).

			% these lvals do not have vn_lval parallels
			%	lvar(var)

:- type vn_rval		--->	vn_origlval(vn_lval)
			;	vn_mkword(tag, vn)		% rval
			;	vn_const(rval_const)
			;	vn_create(tag, list(maybe(rval)), int)
			;       vn_field(tag, vn, int)		% rval
			;	vn_unop(unary_op, vn)		% rval
			;	vn_binop(binary_op, vn, vn).	% rval, rval

			% these rvals do not have vn_rval parallels
			%	var(var)

:- type vn_node		--->	node_shared(vn)
			;	node_lval(vn_lval)
			;	node_origlval(vn_lval)
			;	node_cond.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module atsort, opt_util, opt_debug, map, bintree_set.
:- import_module int, string, require, std_util.

% XXX map__update/set should be det, should call error if key not already there

	% Find straight-line code sequences and optimize them using
	% value numbering.

value_number__optimize(Instrs0, Instrs) :-
	map__init(Livemap0),
	bintree_set__init(Livevals0),
	list__reverse(Instrs0, Backinstrs),
	value_number__build_livemap(Backinstrs, Livevals0, no,
		Livemap0, Livemap),
	value_number__insert_livemap(Livemap, Instrs0, Instrs1),
	value_number__opt_instrs(Instrs1, Livemap, Instrs).

:- pred value_number__build_livemap(list(instruction), lvalset,
	bool, livemap, livemap).
:- mode value_number__build_livemap(in, in, in, di, uo) is det.

value_number__build_livemap([], _, _, Livemap, Livemap).
value_number__build_livemap([Instr|Moreinstrs], Livevals0, Ccode0,
		Livemap0, Livemap) :-
	Instr = Uinstr - _Comment,
	( Uinstr = call(_, _) ->
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
			% Moreinstrs1 = [Nextinstr | Evenmoreinstrs],
			% Nextinstr = Nextuinstr - Nextcomment,
			% write(Instr),
			% nl,
			% write(Nextinstr),
			% nl,
			% Livevals2 = Livevals0,
			% Livemap1 = Livemap0,
			% Moreinstrs2 = Evenmoreinstrs,
			% Ccode1 = Ccode0
			error("call not preceded by livevals")
		)
	; Uinstr = goto(Codeaddr) ->
		opt_util__skip_comments(Moreinstrs, Moreinstrs1),
		opt_util__livevals_addr(Codeaddr, Livevals_needed),
		( Livevals_needed = yes ->
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
				Livevals2 = Livevals2prime,
				Livemap1 = Livemap0,
				Moreinstrs2 = Moreinstrs,
				Ccode1 = Ccode0
			;
				error("cannot yet handle backwards local gotos")
			)
		;
			Livevals2 = Livevals0,
			Livemap1 = Livemap0,
			Moreinstrs2 = Moreinstrs,
			Ccode1 = Ccode0
		)
	; Uinstr = label(Label) ->
		( Ccode0 = no ->
			map__set(Livemap0, Label, Livevals0, Livemap1)
		;
			Livemap1 = Livemap0
		),
		Livevals2 = Livevals0,
		Moreinstrs2 = Moreinstrs,
		Ccode1 = Ccode0
	; ( Uinstr = incr_sp(_) ; Uinstr = decr_sp(_)) ->
		value_number__make_live(lval(sp), Livevals0, Livevals2),
		Livemap1 = Livemap0,
		Moreinstrs2 = Moreinstrs,
		Ccode1 = Ccode0
	; Uinstr = incr_hp(_) ->
		value_number__make_live(lval(hp), Livevals0, Livevals2),
		Livemap1 = Livemap0,
		Moreinstrs2 = Moreinstrs,
		Ccode1 = Ccode0
	; Uinstr = if_val(Rval, _) ->
		% XXX insert livemap here
		value_number__make_live(Rval, Livevals0, Livevals2),
		Livemap1 = Livemap0,
		Moreinstrs2 = Moreinstrs,
		Ccode1 = Ccode0
	; Uinstr = assign(Lval, Rval) ->
		value_number__make_dead(Lval, Livevals0, Livevals1),
		value_number__make_live(Rval, Livevals1, Livevals2),
		Livemap1 = Livemap0,
		Moreinstrs2 = Moreinstrs,
		Ccode1 = Ccode0
	; Uinstr = c_code(_) ->
		Livemap1 = Livemap0,
		Livevals2 = Livevals0,
		Moreinstrs2 = Moreinstrs,
		Ccode1 = yes
	;
		Livemap1 = Livemap0,
		Livevals2 = Livevals0,
		Moreinstrs2 = Moreinstrs,
		Ccode1 = Ccode0
	),
	value_number__build_livemap(Moreinstrs2, Livevals2, Ccode1,
		Livemap1, Livemap).

	% Set all lvals found in this rval to live.

:- pred value_number__make_live(rval, lvalset, lvalset).
:- mode value_number__make_live(in, di, uo) is det.

value_number__make_live(lval(Lval), Livevals0, Livevals) :-
	bintree_set__insert(Livevals0, Lval, Livevals).
value_number__make_live(var(_), Livevals0, Livevals0) :-
	error("var rval should not propagate to value_number").
value_number__make_live(create(_, _, _), Livevals0, Livevals0).
value_number__make_live(mkword(_, Rval), Livevals0, Livevals) :-
	value_number__make_live(Rval, Livevals0, Livevals).
value_number__make_live(field(_, Rval, _), Livevals0, Livevals) :-
	value_number__make_live(Rval, Livevals0, Livevals).
value_number__make_live(const(_), Livevals0, Livevals0).
value_number__make_live(unop(_, Rval), Livevals0, Livevals) :-
	value_number__make_live(Rval, Livevals0, Livevals).
value_number__make_live(binop(_, Rval1, Rval2), Livevals0, Livevals) :-
	value_number__make_live(Rval1, Livevals0, Livevals1),
	value_number__make_live(Rval2, Livevals1, Livevals).

	% Set this lval to dead.

:- pred value_number__make_dead(lval, lvalset, lvalset).
:- mode value_number__make_dead(in, di, uo) is det.

value_number__make_dead(Lval, Livevals0, Livevals) :-
	bintree_set__delete(Livevals0, Lval, Livevals).

:- pred value_number__insert_livemap(livemap, list(instruction),
	list(instruction)).
:- mode value_number__insert_livemap(in, di, uo) is det.

value_number__insert_livemap(_Livemap, [], []).
value_number__insert_livemap(Livemap, [Instr0 | Instrs0], Instrs) :-
	value_number__insert_livemap(Livemap, Instrs0, Instrs1),
	Instr0 = Uinstr0 - _Comment,
	( Uinstr0 = label(Label) ->
		( map__search(Livemap, Label, Livevals) ->
			Instrs = [Instr0, livevals(no, Livevals) - "auto" | Instrs1]
		;
			error("no livevals info about a label")
		)
	;
		Instrs = [Instr0 | Instrs1]
	).

%-----------------------------------------------------------------------------%

	% Optimize instructions, assume we are outside of a block.

:- pred value_number__opt_instrs(list(instruction), livemap,
	list(instruction)).
:- mode value_number__opt_instrs(in, in, out) is det.

value_number__opt_instrs([], _, []).
value_number__opt_instrs([Instr0 | Instrs0], Livemap, Instrs) :-
	Instr0 = Uinstr0 - _Comment0,
	(
		Uinstr0 = label(Label),
		map__search(Livemap, Label, _Livevals)
	->
		value_number__init_tables(Vn_tables0),
		value_number__opt_block(Instrs0, Vn_tables0, Livemap, no,
			Instrs1),
		Instrs = [Instr0 | Instrs1]
		% value_number__opt_instrs(Instrs0, Livemap, Instrs1),
		% Instrs = [Instr0 | Instrs1]
	;
		value_number__opt_instrs(Instrs0, Livemap, Instrs1),
		Instrs = [Instr0 | Instrs1]
	).

	% Initialize the tables for optimizing a block of instructions.

:- pred value_number__init_tables(vn_tables).
:- mode value_number__init_tables(out) is det.

value_number__init_tables(Vn_tables) :-
	map__init(Lval_to_vn_table0),
	map__init(Rval_to_vn_table0),
	map__init(Vn_to_rval_table0),
	map__init(Vn_to_uses_table0),
	map__init(Vn_to_locs_table0),
	map__init(Loc_to_vn_table0),
	Vn_tables = vn_tables(0,
		Lval_to_vn_table0, Rval_to_vn_table0,
		Vn_to_rval_table0, Vn_to_uses_table0,
		Vn_to_locs_table0, Loc_to_vn_table0).

	% Optimize instructions, assume we are inside of a block.

:- pred value_number__opt_block(list(instruction), vn_tables, livemap,
	maybe(bintree_set(lval)), list(instruction)).
:- mode value_number__opt_block(in, in, in, in, out) is det.

value_number__opt_block([], _Vn_tables, _Livemap, _Last_livevals, []) :-
	error("block has no terminator").
value_number__opt_block([Instr0 | Instrs0],
		Vn_tables, Livemap, Last_livevals, Instrs) :-
	value_number__handle_instr(Instr0, Vn_tables, Livemap,
		Last_livevals, Instrs0, Instrs).

:- pred value_number__handle_instr(instruction, vn_tables, livemap,
	maybe(bintree_set(lval)), list(instruction), list(instruction)).
:- mode value_number__handle_instr(in, in, in, in, in, out) is det.

value_number__handle_instr(Instr0, Vn_tables0, Livemap, Last_livevals,
		Instrs0, Instrs) :-
	% should be converted to if-then-elses when np efficiency is important
	Instr0 = Uinstr0 - Comment,
	% write(Instr0), nl,
	(
		Uinstr0 = comment(_),
		value_number__opt_block(Instrs0, Vn_tables0, Livemap,
			no, Instrs1),
		Instrs = [Instr0 | Instrs1]
	;
		Uinstr0 = livevals(Terminate, Livevals),
		( Terminate = yes ->
			% error("livevals(Yes, _) should not be found in handle_instr")
			true
		;
			true
		),
		value_number__opt_block(Instrs0, Vn_tables0, Livemap,
			yes(Livevals), Instrs1),
		Instrs = [Instr0 | Instrs1]
	;
		Uinstr0 = block(_, _),
		error("block should not be found in handle_instr")
	;
		Uinstr0 = assign(Lval, Rval),
		% the next call does the counting of the new use
		value_number__use_rval_find_vn(Rval, Vn,
			Vn_tables0, Vn_tables1),
		value_number__lval_to_vnlval(Lval, Vn_lval,
			Vn_tables1, Vn_tables2),
		Vn_tables2 = vn_tables(Next_vn2,
			Lval_to_vn_table2, Rval_to_vn_table2,
			Vn_to_rval_table2, Vn_to_uses_table2,
			Vn_to_locs_table2, Loc_to_vn_table2),
		( map__search(Lval_to_vn_table2, Vn_lval, Old_vn) ->
			map__lookup(Vn_to_uses_table2, Old_vn, Old_vn_uses2),
			Old_vn_uses3 is Old_vn_uses2 - 1,
			map__set(Vn_to_uses_table2, Old_vn, Old_vn_uses3,
				Vn_to_uses_table3)
		;
			Vn_to_uses_table3 = Vn_to_uses_table2
		),
		map__set(Lval_to_vn_table2, Vn_lval, Vn, Lval_to_vn_table3),
		Vn_tables3 = vn_tables(Next_vn2,
			Lval_to_vn_table3, Rval_to_vn_table2,
			Vn_to_rval_table2, Vn_to_uses_table3,
			Vn_to_locs_table2, Loc_to_vn_table2),
		value_number__opt_block(Instrs0, Vn_tables3, Livemap,
			no, Instrs1),
		Instrs = [Instr0 | Instrs1]
	;
		Uinstr0 = call(_, _),
		( Last_livevals = yes(Livevals) ->
			% flush
			true
		;
			error("call not preceded by livevals")
		),
		value_number__opt_instrs(Instrs0, Livemap, Instrs1),
		opt_debug__dump_tables(Vn_tables0, Newcomment),
		Instrs = [comment(Newcomment) - "", Instr0 | Instrs1]
	;
		Uinstr0 = mkframe(_, _, _),
		value_number__opt_block(Instrs0, Vn_tables0, Livemap,
			no, Instrs1),
		Instrs = [Instr0 | Instrs1]
	;
		Uinstr0 = modframe(_),
		value_number__opt_block(Instrs0, Vn_tables0, Livemap,
			no, Instrs1),
		Instrs = [Instr0 | Instrs1]
	;
		Uinstr0 = label(_),
		opt_util__skip_comments(Instrs0, Instrs1),
		( Instrs1 = [livevals(_Terminate, _Nextlivevals) - _ | _] ->
			% XXX flush all these livevals
			true
		;
			% flush all livevals or maybe abort
			true
		),
		value_number__opt_block(Instrs1, Vn_tables0, Livemap,
			no, Instrs2),
		Instrs = [Instr0 | Instrs2]
		% opt_debug__dump_label(L, L_str),
		% string__append_list(["label ", L_str, " in handle_instr"], Str),
		% error(Str)
	;
		Uinstr0 = goto(_),
		( Last_livevals = yes(Livevals) ->
			% flush
			true
		;
			error("goto not preceded by livevals")
		),
		value_number__opt_instrs(Instrs0, Livemap, Instrs1),
		opt_debug__dump_tables(Vn_tables0, Newcomment),
		Instrs = [comment(Newcomment) - "", Instr0 | Instrs1]
	;
		Uinstr0 = computed_goto(_, _),
		( Last_livevals = yes(Livevals) ->
			% flush
			true
		;
			error("computed goto not preceded by livevals")
		),
		value_number__opt_instrs(Instrs0, Livemap, Instrs1),
		opt_debug__dump_tables(Vn_tables0, Newcomment),
		Instrs = [comment(Newcomment) - "", Instr0 | Instrs1]
	;
		Uinstr0 = c_code(_),
		error("c_code in handle_instr")
	;
		Uinstr0 = if_val(_, _),
		% XXX flush values live at Code_addr
		value_number__opt_block(Instrs0, Vn_tables0, Livemap,
			no, Instrs1),
		opt_debug__dump_tables(Vn_tables0, Newcomment),
		Instrs = [comment(Newcomment) - "", Instr0 | Instrs1]
	;
		Uinstr0 = incr_sp(N),
		value_number__opt_block(Instrs0, Vn_tables0, Livemap,
			no, Instrs1),
		Instrs = [Instr0 | Instrs1]
	;
		Uinstr0 = decr_sp(N),
		value_number__opt_block(Instrs0, Vn_tables0, Livemap,
			no, Instrs1),
		Instrs = [Instr0 | Instrs1]
	;
		Uinstr0 = incr_hp(N),
		value_number__handle_instr(assign(hp,
			binop((+), lval(hp), const(int_const(N)))) - Comment,
			Vn_tables0, Livemap, Last_livevals, Instrs0, Instrs)
	).

:- pred value_number__use_rval_find_vn(rval, vn, vn_tables, vn_tables).
:- mode value_number__use_rval_find_vn(in, out, di, uo) is det.

value_number__use_rval_find_vn(lval(Lval), Vn, Vn_tables0, Vn_tables) :-
	value_number__use_lval_find_vn(Lval, Vn, Vn_tables0, Vn_tables).
value_number__use_rval_find_vn(var(_), _, _Vn_tables, _) :-
	error("value_number should never get rval: var").
value_number__use_rval_find_vn(create(Tag, Args, Label), Vn,
		Vn_tables0, Vn_tables) :-
	value_number__use_vnrval_find_vn(vn_create(Tag, Args, Label), Vn,
		Vn_tables0, Vn_tables).
value_number__use_rval_find_vn(mkword(Tag, Rval), Vn, Vn_tables0, Vn_tables) :-
	value_number__use_rval_find_vn(Rval, Sub_vn, Vn_tables0, Vn_tables1),
	value_number__use_vnrval_find_vn(vn_mkword(Tag, Sub_vn), Vn,
		Vn_tables1, Vn_tables).
value_number__use_rval_find_vn(unop(Op, Rval), Vn, Vn_tables0, Vn_tables) :-
	value_number__use_rval_find_vn(Rval, Sub_vn, Vn_tables0, Vn_tables1),
	value_number__use_vnrval_find_vn(vn_unop(Op, Sub_vn), Vn,
		Vn_tables1, Vn_tables).
value_number__use_rval_find_vn(const(Const), Vn, Vn_tables0, Vn_tables) :-
	value_number__use_vnrval_find_vn(vn_const(Const), Vn,
		Vn_tables0, Vn_tables).
value_number__use_rval_find_vn(field(Tag, Rval, Slot), Vn,
		Vn_tables0, Vn_tables) :-
	value_number__use_rval_find_vn(Rval, Sub_vn, Vn_tables0, Vn_tables1),
	value_number__use_vnrval_find_vn(vn_field(Tag, Sub_vn, Slot), Vn,
		Vn_tables1, Vn_tables).
value_number__use_rval_find_vn(binop(Op, Left_rval, Right_rval), Vn,
		Vn_tables0, Vn_tables) :-
	value_number__use_rval_find_vn(Left_rval, Left_vn,
		Vn_tables0, Vn_tables1),
	value_number__use_rval_find_vn(Right_rval, Right_vn,
		Vn_tables1, Vn_tables2),
	value_number__use_vnrval_find_vn(vn_binop(Op, Left_vn, Right_vn), Vn,
		Vn_tables2, Vn_tables).

:- pred value_number__use_vnrval_find_vn(vn_rval, vn, vn_tables, vn_tables).
:- mode value_number__use_vnrval_find_vn(in, out, di, uo) is det.

value_number__use_vnrval_find_vn(Vn_rval, Vn, Vn_tables0, Vn_tables) :-
	value_number__simplify_vnrval(Vn_rval, Vn_rval1,
		Vn_tables0, Vn_tables1),
	value_number__lookup_vnrval(Vn_rval1, Vn, Vn_tables1, Vn_tables).

	% Simplify the vnrval by partially evaluating expressions involving
	% integer constants. To make this simpler, swap the arguments of
	% commutative expressions around to put the constants on the right
	% side.
	%
	% The simplification has to be done on vn_rvals and not on rvals
	% even though this complicates the code. The reason is that an
	% expression such as r1 + 4 can be simplified if we know that
	% r1 was defined as r2 + 8.
	%
	% This code should decrement the use counts of value numbers that
	% the incoming vn_rval refers to that the outgoing vn_rval does not,
	% but this is not yet implemented. In any case, we probably don't need
	% accurate use counts on constants.

:- pred value_number__simplify_vnrval(vn_rval, vn_rval, vn_tables, vn_tables).
:- mode value_number__simplify_vnrval(in, out, di, uo) is det.

value_number__simplify_vnrval(Vn_rval0, Vn_rval, Vn_tables0, Vn_tables) :-
	Vn_tables0 = vn_tables(_Next_vn0,
		_Lval_to_vn_table0, _Rval_to_vn_table0,
		Vn_to_rval_table0, _Vn_to_uses_table0,
		_Vn_to_locs_table0, _Loc_to_vn_table0),
	( Vn_rval0 = vn_binop((+), Vn1, Vn2) ->
		map__lookup(Vn_to_rval_table0, Vn1, Vn_rval1),
		map__lookup(Vn_to_rval_table0, Vn2, Vn_rval2),
		( Vn_rval1 = vn_const(int_const(I1)) ->
			( Vn_rval2 = vn_const(int_const(I2)) ->
				I is I1 + I2,
				Vn_rval = vn_const(int_const(I)),
				Vn_tables = Vn_tables0
			; Vn_rval2 = vn_binop((+), Vn21, Vn22) ->
				map__lookup(Vn_to_rval_table0, Vn22, Vn_rval22),
				( Vn_rval22 = vn_const(int_const(I22)) ->
					I is I1 + I22,
					value_number__unuse_vn(Vn2, Vn_tables0, Vn_tables1),
					value_number__use_vnrval_find_vn(
						vn_const(int_const(I)), Vn_i,
						Vn_tables1, Vn_tables),
					Vn_rval = vn_binop((+), Vn21, Vn_i)
				;
					Vn_rval = vn_binop((+), Vn2, Vn1),
					Vn_tables = Vn_tables0
				)
			;
				Vn_rval = vn_binop((+), Vn2, Vn1),
				Vn_tables = Vn_tables0
			)
		; Vn_rval1 = vn_binop((+), Vn11, Vn12) ->
			map__lookup(Vn_to_rval_table0, Vn12, Vn_rval12),
			( Vn_rval12 = vn_const(int_const(I12)) ->
				( Vn_rval2 = vn_const(int_const(I2)) ->
					I is I12 + I2,
					value_number__unuse_vn(Vn1, Vn_tables0, Vn_tables1),
					value_number__use_vnrval_find_vn(
						vn_const(int_const(I)), Vn_i,
						Vn_tables1, Vn_tables),
					Vn_rval = vn_binop((+), Vn11, Vn_i)
				; Vn_rval2 = vn_binop((+), Vn21, Vn22) ->
					map__lookup(Vn_to_rval_table0, Vn22, Vn_rval22),
					( Vn_rval22 = vn_const(int_const(I22)) ->
						I is I12 + I22,
						value_number__unuse_vn(Vn1, Vn_tables0, Vn_tables1),
						value_number__unuse_vn(Vn2, Vn_tables1, Vn_tables2),
						value_number__use_vnrval_find_vn(
							vn_binop((+), Vn11, Vn21), Vn_e,
							Vn_tables2, Vn_tables3),
						value_number__use_vnrval_find_vn(
							vn_const(int_const(I)), Vn_i,
							Vn_tables3, Vn_tables),
						Vn_rval = vn_binop((+), Vn_e, Vn_i)
					;
						value_number__unuse_vn(Vn1, Vn_tables0, Vn_tables1),
						value_number__use_vnrval_find_vn(
							vn_binop((+), Vn11, Vn2), Vn_e,
							Vn_tables1, Vn_tables),
						Vn_rval = vn_binop((+), Vn_e, Vn12)
					)
				;
					Vn_rval = Vn_rval0,
					Vn_tables = Vn_tables0
				)
			;
				Vn_rval = vn_binop((+), Vn2, Vn1),
				Vn_tables = Vn_tables0
			)
		;
			Vn_rval = Vn_rval0,
			Vn_tables = Vn_tables0
		)
	; Vn_rval0 = vn_binop((-), Vn1, Vn2) ->
		map__lookup(Vn_to_rval_table0, Vn2, Vn_rval2),
		( Vn_rval2 = vn_const(int_const(I2)) ->
			NI2 is 0 - I2,
			value_number__use_vnrval_find_vn(vn_const(int_const(NI2)),
				Vn2prime, Vn_tables0, Vn_tables1),
			value_number__simplify_vnrval(vn_binop((+), Vn1, Vn2prime),
				Vn_rval, Vn_tables1, Vn_tables)
		;
			% XXX more simplification opportunities exist
			Vn_rval = Vn_rval0,
			Vn_tables = Vn_tables0
		)
	;
		% XXX more simplification opportunities exist
		Vn_rval = Vn_rval0,
		Vn_tables = Vn_tables0
	).

:- pred value_number__lookup_vnrval(vn_rval, vn, vn_tables, vn_tables).
:- mode value_number__lookup_vnrval(in, out, di, uo) is det.

value_number__lookup_vnrval(Vn_rval, Vn, Vn_tables0, Vn_tables) :-
	Vn_tables0 = vn_tables(Next_vn0,
		Lval_to_vn_table0, Rval_to_vn_table0,
		Vn_to_rval_table0, Vn_to_uses_table0,
		Vn_to_locs_table0, Loc_to_vn_table0),
	( map__search(Rval_to_vn_table0, Vn_rval, Vn_prime) ->
		Vn = Vn_prime,
		map__lookup(Vn_to_uses_table0, Vn, Usecount),
		Usecount1 is Usecount + 1,
		map__set(Vn_to_uses_table0, Vn, Usecount1, Vn_to_uses_table1),
		Vn_tables = vn_tables(Next_vn0,
			Lval_to_vn_table0, Rval_to_vn_table0,
			Vn_to_rval_table0, Vn_to_uses_table1,
			Vn_to_locs_table0, Loc_to_vn_table0)
	;
		Vn = Next_vn0,
		Next_vn1 is Next_vn0 + 1,
		map__set(Rval_to_vn_table0, Vn_rval, Vn, Rval_to_vn_table1),
		map__set(Vn_to_rval_table0, Vn, Vn_rval, Vn_to_rval_table1),
		map__set(Vn_to_uses_table0, Vn, 1, Vn_to_uses_table1),
		Vn_tables = vn_tables(Next_vn1,
			Lval_to_vn_table0, Rval_to_vn_table1,
			Vn_to_rval_table1, Vn_to_uses_table1,
			Vn_to_locs_table0, Loc_to_vn_table0)
	).

:- pred value_number__use_lval_find_vn(lval, vn, vn_tables, vn_tables).
:- mode value_number__use_lval_find_vn(in, out, di, uo) is det.

value_number__use_lval_find_vn(Lval, Vn, Vn_tables0, Vn_tables) :-
	value_number__lval_to_vnlval(Lval, Vn_lval, Vn_tables0, Vn_tables1),
	Vn_tables1 = vn_tables(Next_vn1,
		Lval_to_vn_table1, Rval_to_vn_table1,
		Vn_to_rval_table1, Vn_to_uses_table1,
		Vn_to_locs_table1, Loc_to_vn_table1),
	( map__search(Lval_to_vn_table1, Vn_lval, Vn_prime) ->
		Vn = Vn_prime,
		map__lookup(Vn_to_uses_table1, Vn, Usecount1),
		Usecount2 is Usecount1 + 1,
		map__set(Vn_to_uses_table1, Vn, Usecount2, Vn_to_uses_table2),
		Vn_tables = vn_tables(Next_vn1,
			Lval_to_vn_table1, Rval_to_vn_table1,
			Vn_to_rval_table1, Vn_to_uses_table2,
			Vn_to_locs_table1, Loc_to_vn_table1)
	;
		Vn = Next_vn1,
		Next_vn2 is Next_vn1 + 1,
		map__set(Lval_to_vn_table1, Vn_lval, Vn,
			Lval_to_vn_table2),
		map__set(Rval_to_vn_table1, vn_origlval(Vn_lval), Vn,
			Rval_to_vn_table2),
		map__set(Vn_to_rval_table1, Vn, vn_origlval(Vn_lval),
			Vn_to_rval_table2),
		map__set(Vn_to_uses_table1, Vn, 1,
			Vn_to_uses_table2),
		map__set(Vn_to_locs_table1, Vn, [Vn_lval],
			Vn_to_locs_table2),
		map__set(Loc_to_vn_table1, Vn_lval, Vn,
			Loc_to_vn_table2),
		Vn_tables = vn_tables(Next_vn2,
			Lval_to_vn_table2, Rval_to_vn_table2,
			Vn_to_rval_table2, Vn_to_uses_table2,
			Vn_to_locs_table2, Loc_to_vn_table2)
	).

:- pred value_number__lval_to_vnlval(lval, vn_lval, vn_tables, vn_tables).
:- mode value_number__lval_to_vnlval(in, out, di, uo) is det.

value_number__lval_to_vnlval(reg(Reg),	vn_reg(Reg),	Vn_tables, Vn_tables).
value_number__lval_to_vnlval(succip,	vn_succip,	Vn_tables, Vn_tables).
value_number__lval_to_vnlval(maxfr,	vn_maxfr,	Vn_tables, Vn_tables).
value_number__lval_to_vnlval(curredoip, vn_curredoip,	Vn_tables, Vn_tables).
value_number__lval_to_vnlval(hp,	vn_hp,		Vn_tables, Vn_tables).
value_number__lval_to_vnlval(sp,	vn_sp,		Vn_tables, Vn_tables).
value_number__lval_to_vnlval(stackvar(Slot), vn_stackvar(Slot),
		Vn_tables, Vn_tables).
value_number__lval_to_vnlval(framevar(Slot), vn_framevar(Slot),
		Vn_tables, Vn_tables).
value_number__lval_to_vnlval(temp(No), vn_temp(No),
		Vn_tables, Vn_tables).
value_number__lval_to_vnlval(field(Tag, Lval, Slot), vn_field(Tag, Vn, Slot),
		Vn_tables0, Vn_tables) :-
	value_number__use_lval_find_vn(Lval, Vn, Vn_tables0, Vn_tables).
value_number__lval_to_vnlval(lvar(_Var), _, _Vn_tables, _) :-
	error("Lvar detected in value_number").

:- pred value_number__unuse_vn(vn, vn_tables, vn_tables).
:- mode value_number__unuse_vn(in, di, uo) is det.

value_number__unuse_vn(Vn, Vn_tables0, Vn_tables) :-
	% write('in unuse_vn'), nl,
	Vn_tables0 = vn_tables(Next_vn0,
		Lval_to_vn_table0, Rval_to_vn_table0,
		Vn_to_rval_table0, Vn_to_uses_table0,
		Vn_to_locs_table0, Loc_to_vn_table0),
	map__lookup(Vn_to_uses_table0, Vn, Uses0),
	Uses is Uses0 - 1,
	map__set(Vn_to_uses_table0, Vn, Uses, Vn_to_uses_table1),
	Vn_tables1 = vn_tables(Next_vn0,
		Lval_to_vn_table0, Rval_to_vn_table0,
		Vn_to_rval_table0, Vn_to_uses_table1,
		Vn_to_locs_table0, Loc_to_vn_table0),
	( Uses > 0 ->
		Vn_tables = Vn_tables1
	;
		map__lookup(Vn_to_rval_table0, Vn, Vn_rval),
		(
			Vn_rval = vn_origlval(Vn_lval),
			(Vn_lval = vn_field(_Tag1, Sub_vn, _Slot1) ->
				Sub_vns = [Sub_vn]
			;
				Sub_vns = []
			)
		;
			Vn_rval = vn_mkword(_Tag2, Sub_vn),
			Sub_vns = [Sub_vn]
		;
			Vn_rval = vn_const(_Const),
			Sub_vns = []
		;
			Vn_rval = vn_create(_Tag3, _Args, _Label),
			Sub_vns = []
		;
			Vn_rval = vn_field(_Tag4, Sub_vn, _Slot4),
			Sub_vns = [Sub_vn]
		;
			Vn_rval = vn_unop(_Unop, Sub_vn),
			Sub_vns = [Sub_vn]
		;
			Vn_rval = vn_binop(_Binop, Sub_vn1, Sub_vn2),
			Sub_vns = [Sub_vn1, Sub_vn2]
		),
		value_number__unuse_vns(Sub_vns, Vn_tables1, Vn_tables)
	).

:- pred value_number__unuse_vns(list(vn), vn_tables, vn_tables).
:- mode value_number__unuse_vns(in, di, uo) is det.

value_number__unuse_vns([], Vn_tables, Vn_tables).
value_number__unuse_vns([Vn | Vns], Vn_tables0, Vn_tables) :-
	value_number__unuse_vn(Vn, Vn_tables0, Vn_tables1),
	value_number__unuse_vns(Vns, Vn_tables1, Vn_tables).

%-----------------------------------------------------------------------------%

:- pred value_number__flush_lvals(list(vn_lval), list(vn_lval),
	maybe(pair(vn_rval, label)),
	list(vn_lval), list(vn_lval), vn_tables, vn_tables, list(instruction)).
:- mode value_number__flush_lvals(in, in, in, di, uo, di, uo, out) is det.

value_number__flush_lvals(Flush, Live, Cond, Free0, Free,
		Vn_tables0, Vn_tables, Instrs) :-
	value_number__keep_live_nodes(Live, Vn_tables0, Vn_tables1),
	value_number__make_relmaps(Live, Cond, Vn_tables1, Vn_tables2,
		Succmap, Predmap),
	atsort(Succmap, Predmap, BlockOrder),
	value_number__order_equal_lists(BlockOrder, GoodBlockOrder),
	list__condense(GoodBlockOrder, Order),
	value_number__flush_nodelist(Order, Flush, Cond, Predmap,	
		Vn_tables2, Vn_tables, Free0, Free, Instrs).

:- pred value_number__keep_live_nodes(list(vn_lval), vn_tables, vn_tables).
:- mode value_number__keep_live_nodes(in, di, uo) is det.

value_number__keep_live_nodes(Live_lvals, Vn_tables0, Vn_tables) :-
	Vn_tables0 = vn_tables(_Next_vn0,
		Lval_to_vn_table0, _Rval_to_vn_table0,
		_Vn_to_rval_table0, _Vn_to_uses_table0,
		_Vn_to_locs_table0, _Loc_to_vn_table0),
	map__keys(Lval_to_vn_table0, All_lvals),
	value_number__find_dead_nodes(All_lvals, Live_lvals, Dead_lvals),
	value_number__remove_dead_nodes(Dead_lvals, Vn_tables0, Vn_tables).

:- pred value_number__find_dead_nodes(list(vn_lval), list(vn_lval),
	list(vn_lval)).
:- mode value_number__find_dead_nodes(in, in, out) is det.

value_number__find_dead_nodes([], _, []).
value_number__find_dead_nodes([Vn_lval | Vn_lvals], Live, Dead) :-
	value_number__find_dead_nodes(Vn_lvals, Live, Dead0),
	(
		( Vn_lval = vn_reg(_)
		; Vn_lval = vn_stackvar(_)
		; Vn_lval = vn_framevar(_)
		)
	->
		( list__member(Vn_lval, Live) ->
			Dead = Dead0
		;
			Dead = [Vn_lval | Dead0]
		)
	;
		Dead = Dead0
	).

:- pred value_number__remove_dead_nodes(list(vn_lval), vn_tables, vn_tables).
:- mode value_number__remove_dead_nodes(in, di, uo) is det.

value_number__remove_dead_nodes([], Vn_tables, Vn_tables).
value_number__remove_dead_nodes([Lval | Lvals], Vn_tables0, Vn_tables) :-
	Vn_tables0 = vn_tables(_Next_vn0,
		Lval_to_vn_table0, _Rval_to_vn_table0,
		_Vn_to_rval_table0, _Vn_to_uses_table0,
		_Vn_to_locs_table0, _Loc_to_vn_table0),
	map__lookup(Lval_to_vn_table0, Lval, Vn),
	value_number__unuse_vn(Vn, Vn_tables0, Vn_tables),
	value_number__remove_dead_nodes(Lvals, Vn_tables0, Vn_tables).

% uncount all dead lvals
% map from (livelvals U shared_vn U cond) to set of (shared_vn U origlvals)
% count only unique paths, i.e. when arriving at a shared node
%  count only the shared vn, not anything else it points to
% build a topological sort based on this order, plus the order that
%  after the appearance of an lval no refs to its origlval be outstanding
%  this includes appearances as the bases of field references

:- pred value_number__make_relmaps(list(vn_lval), maybe(pair(vn_rval, label)),
	vn_tables, vn_tables, relmap(vn_node), relmap(vn_node)).
:- mode value_number__make_relmaps(in, in, di, uo, out, out) is det.

value_number__make_relmaps(Livevals, Cond, Vn_tables0, Vn_tables,
		Succmap, Predmap) :-
	map__init(Succmap0),
	map__init(Predmap0),
	( Cond = yes(Test - _) ->
		value_number__use_vnrval_find_vn(Test, Test_vn,
			Vn_tables0, Vn_tables1),
		value_number__make_rels(Livevals, Vn_tables1, Vn_tables2,
			Succmap0, Succmap1, Predmap0, Predmap1),
		value_number__find_links(Test_vn, node_cond,
			Vn_tables2, Vn_tables,
			Succmap1, Succmap2, Predmap1, Predmap2)
	;
		value_number__make_rels(Livevals, Vn_tables0, Vn_tables,
			Succmap0, Succmap2, Predmap0, Predmap2)
	),
	value_number__use_before_redef(Succmap2, Succmap, Predmap2, Predmap).

:- pred value_number__make_rels(list(vn_lval), vn_tables, vn_tables,
	relmap(vn_node), relmap(vn_node), relmap(vn_node), relmap(vn_node)).
:- mode value_number__make_rels(in, di, uo, di, uo, di, uo) is det.

value_number__make_rels([], Vn_tables, Vn_tables,
		Succmap, Succmap, Predmap, Predmap).
value_number__make_rels([Lval | Lvals], Vn_tables0, Vn_tables,
		Succmap0, Succmap, Predmap0, Predmap) :-
	value_number__make_rel(Lval, Vn_tables0, Vn_tables1,
		Succmap0, Succmap1, Predmap0, Predmap1),
	value_number__make_rels(Lvals, Vn_tables1, Vn_tables,
		Succmap1, Succmap, Predmap1, Predmap).

:- pred value_number__make_rel(vn_lval, vn_tables, vn_tables,
	relmap(vn_node), relmap(vn_node), relmap(vn_node), relmap(vn_node)).
:- mode value_number__make_rel(in, di, uo, di, uo, di, uo) is det.

value_number__make_rel(Lval, Vn_tables0, Vn_tables,
		Succmap0, Succmap, Predmap0, Predmap) :-
	Vn_tables0 = vn_tables(_Next_vn0,
		Lval_to_vn_table0, _Rval_to_vn_table0,
		_Vn_to_rval_table0, _Vn_to_uses_table0,
		_Vn_to_locs_table0, _Loc_to_vn_table0),
	map__lookup(Lval_to_vn_table0, Lval, Vn),
	value_number__find_links(Vn, node_lval(Lval),
		Vn_tables0, Vn_tables,
		Succmap0, Succmap, Predmap0, Predmap).

:- pred value_number__find_links(vn, vn_node, vn_tables, vn_tables,
	relmap(vn_node), relmap(vn_node), relmap(vn_node), relmap(vn_node)).
:- mode value_number__find_links(in, in, di, uo, di, uo, di, uo) is det.

value_number__find_links(Vn, Sink, Vn_tables0, Vn_tables,
		Succmap0, Succmap, Predmap0, Predmap) :-
	Vn_tables0 = vn_tables(_Next_vn0,
		_Lval_to_vn_table0, _Rval_to_vn_table0,
		Vn_to_rval_table0, Vn_to_uses_table0,
		_Vn_to_locs_table0, _Loc_to_vn_table0),
	map__lookup(Vn_to_uses_table0, Vn, Uses0),
	( Uses0 > 1, \+ Sink = node_shared(_) ->
		value_number__add_link(node_shared(Vn), Sink,
			Succmap0, Succmap1, Predmap0, Predmap1),
		value_number__find_links(Vn, node_shared(Vn),
			Vn_tables0, Vn_tables,
			Succmap1, Succmap, Predmap1, Predmap)
	;
		map__lookup(Vn_to_rval_table0, Vn, Vn_rval),
		(
			Vn_rval = vn_origlval(Vn_lval),
			opt_util__vnlval_access_vn(Vn_lval, Access_vn),
			( Access_vn = yes(Sub_vn) ->
				value_number__find_links(Sub_vn, Sink,
					Vn_tables0, Vn_tables,
					Succmap0, Succmap1, Predmap0, Predmap1)
			;
				Succmap1 = Succmap0,
				Predmap1 = Predmap0,
				Vn_tables = Vn_tables0
			),
			value_number__add_link(node_origlval(Vn_lval), Sink,
				Succmap1, Succmap, Predmap1, Predmap)
		;
			Vn_rval = vn_mkword(_Tag1, Sub_vn),
			value_number__find_links(Sub_vn, Sink,
				Vn_tables0, Vn_tables,
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
			Vn_rval = vn_field(_Tag3, Sub_vn, _Slot),
			value_number__find_links(Sub_vn, Sink,
				Vn_tables0, Vn_tables,
				Succmap0, Succmap, Predmap0, Predmap)
		;
			Vn_rval = vn_unop(_Unop, Sub_vn),
			value_number__find_links(Sub_vn, Sink,
				Vn_tables0, Vn_tables,
				Succmap0, Succmap, Predmap0, Predmap)
		;
			Vn_rval = vn_binop(_Binop, Sub_vn1, Sub_vn2),
			value_number__find_links(Sub_vn1, Sink,
				Vn_tables0, Vn_tables1,
				Succmap0, Succmap1, Predmap0, Predmap1),
			value_number__find_links(Sub_vn2, Sink,
				Vn_tables1, Vn_tables,
				Succmap1, Succmap, Predmap1, Predmap)
		)
	).

:- pred value_number__add_links(list(vn_node), vn_node,
	relmap(vn_node), relmap(vn_node), relmap(vn_node), relmap(vn_node)).
:- mode value_number__add_links(in, in, di, uo, di, uo) is det.

value_number__add_links([], _, Succmap, Succmap, Predmap, Predmap).
value_number__add_links([Source | Sources], Sink,
		Succmap0, Succmap, Predmap0, Predmap) :-
	value_number__add_link(Source, Sink,
		Succmap0, Succmap1, Predmap0, Predmap1),
	value_number__add_links(Sources, Sink,
		Succmap1, Succmap, Predmap1, Predmap).

:- pred value_number__add_link(vn_node, vn_node,
	relmap(vn_node), relmap(vn_node), relmap(vn_node), relmap(vn_node)).
:- mode value_number__add_link(in, in, di, uo, di, uo) is det.

value_number__add_link(Source, Sink, Succmap0, Succmap, Predmap0, Predmap) :-
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

:- pred value_number__order_equal_lists(list(list(vn_node)),
	list(list(vn_node))).
:- mode value_number__order_equal_lists(di, uo) is det.

value_number__order_equal_lists([], []).
value_number__order_equal_lists([Block | Blocks], [GoodBlock | GoodBlocks]) :-
	value_number__order_equals(Block, GoodBlock),
	value_number__order_equal_lists(Blocks, GoodBlocks).

:- pred value_number__order_equals(list(vn_node), list(vn_node)).
:- mode value_number__order_equals(di, uo) is det.

value_number__order_equals(BlockOrder0, BlockOrder) :-
	value_number__find_regs(BlockOrder0, Regs, BlockOrder1),
	value_number__find_cond(BlockOrder1, Cond, BlockOrder2),
	list__condense([Regs, BlockOrder2, Cond], BlockOrder).

:- pred value_number__find_regs(list(vn_node), list(vn_node), list(vn_node)).
:- mode value_number__find_regs(di, out, uo) is det.

value_number__find_regs([], [], []).
value_number__find_regs([Node0 | Nodes0], Regs, Nodes) :-
	value_number__find_regs(Nodes0, Regs1, Nodes1),
	( Node0 = node_lval(vn_reg(_)) ->
		Regs = [Node0 | Regs1],
		Nodes = Nodes1
	;
		Regs = Regs1,
		Nodes = [Node0 | Nodes1]
	).

:- pred value_number__find_cond(list(vn_node), list(vn_node), list(vn_node)).
:- mode value_number__find_cond(di, out, uo) is det.

value_number__find_cond([], [], []).
value_number__find_cond([Node0 | Nodes0], Cond, Nodes) :-
	value_number__find_cond(Nodes0, Cond1, Nodes1),
	( Node0 = node_cond ->
		Cond = [Node0 | Cond1],
		Nodes = Nodes1
	;
		Cond = Cond1,
		Nodes = [Node0 | Nodes1]
	).

:- pred value_number__use_before_redef(relmap(vn_node), relmap(vn_node),
	relmap(vn_node), relmap(vn_node)).
:- mode value_number__use_before_redef(di, uo, di, uo) is det.

value_number__use_before_redef(Succmap0, Succmap, Predmap0, Predmap) :-
	map__keys(Predmap0, Sinks),
	value_number__use_sinks_before_redef(Sinks, Succmap0, Succmap,
		Predmap0, Predmap).

:- pred value_number__use_sinks_before_redef(list(vn_node),
	relmap(vn_node), relmap(vn_node), relmap(vn_node), relmap(vn_node)).
:- mode value_number__use_sinks_before_redef(in, di, uo, di, uo) is det.

value_number__use_sinks_before_redef([], Succmap, Succmap, Predmap, Predmap).
value_number__use_sinks_before_redef([Sink | Sinks],
		Succmap0, Succmap, Predmap0, Predmap) :-
	( Sink = node_lval(Vn_lval) ->
		map__lookup(Succmap0, node_origlval(Vn_lval), Users),
		value_number__add_links(Users, Sink,
			Succmap0, Succmap1, Predmap0, Predmap1)
	;
		Succmap1 = Succmap0,
		Predmap1 = Predmap0
	),
	value_number__use_sinks_before_redef(Sinks,
		Succmap1, Succmap, Predmap1, Predmap).

%-----------------------------------------------------------------------------%

% start with a queue of regs: rN where N < eg 5 and not live
%  plus tempM where M < e.g. 5
% when you need a reg for a shared vn, pick the one at front
% when you generate the last ref to a shared vn, put its reg at the back

:- pred value_number__flush_nodelist(list(vn_node), list(vn_lval),
 	maybe(pair(vn_rval, label)), relmap(vn_node), vn_tables, vn_tables,
 	list(vn_lval), list(vn_lval), list(instruction)).
:- mode value_number__flush_nodelist(in, in, in, in, di, uo,
 	di, uo, out) is det.

value_number__flush_nodelist(_Order, _Flush, _Cond, _Predmap,	
 		Vn_tables0, Vn_tables, Free0, Free, Instrs) :-
 	Vn_tables = Vn_tables0,
 	Free = Free0,
 	Instrs = [].
