%-----------------------------------------------------------------------------%

% Vn_type.nl - types for value numbering.

% Author: zs.

%-----------------------------------------------------------------------------%

:- module vn_type.

:- interface.
:- import_module llds, bintree_set, list.

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

:- type vn_src		--->	src_ctrl(int)
			;	src_liveval(vnlval)
			;	src_access(vnlval)
			;	src_vn(int).

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
			;	vn_if_val(vn, code_addr)
			;	vn_mark_hp(vnlval)
			;	vn_restore_hp(vn)
			;	vn_incr_sp(int)
			;	vn_decr_sp(int).

:- type livemap == map(label, lvalset).
:- type lvalset == bintree_set(lval).
:- type vnlvalset == bintree_set(vnlval).

:- type ctrlmap		== map(int, vn_instr).
:- type flushmap	== map(int, flushmapentry).
:- type flushmapentry	== map(vnlval, vn).

% There is no implementation section.
