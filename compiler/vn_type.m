%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% vn_type.m - types for value numbering.

% Author: zs.

%-----------------------------------------------------------------------------%

:- module vn_type.

:- interface.
:- import_module llds, livemap, options.
:- import_module getopt, set, list, std_util.

:- type vn == int.

:- type vnlval		--->	vn_reg(reg)
			;	vn_stackvar(int)
			;	vn_framevar(int)
			;	vn_succip
			;	vn_maxfr
			;	vn_curfr
			;	vn_succfr(vn)
			;	vn_prevfr(vn)
			;	vn_redoip(vn)
			;	vn_succip(vn)
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

:- type vn_instr	--->	vn_livevals(lvalset)
			;	vn_call(code_addr, code_addr,
					list(liveinfo), call_model)
			;	vn_call_closure(code_model, code_addr,
					list(liveinfo))
			;	vn_mkframe(string, int, code_addr)
			;	vn_label(label)
			;	vn_goto(code_addr)
			;	vn_computed_goto(vn, list(label))
			;	vn_if_val(vn, code_addr)
			;	vn_mark_hp(vnlval)
			;	vn_restore_hp(vn)
			;	vn_store_ticket(vnlval)
			;	vn_restore_ticket(vn)
			;	vn_discard_ticket
			;	vn_incr_sp(int)
			;	vn_decr_sp(int).

:- type parentry	==	pair(lval, list(rval)).
:- type parallel	--->	parallel(label, label, list(parentry)).

:- type vnlvalset	==	set(vnlval).

:- type ctrlmap		==	map(int, vn_instr).
:- type flushmap	==	map(int, flushmapentry).
:- type flushmapentry	==	map(vnlval, vn).
:- type parmap		==	map(int, list(parallel)).

:- type vn_ctrl_tuple	--->	tuple(int, ctrlmap, flushmap, int, parmap).

:- type vn_params.

:- pred vn_type__init_params(option_table(option), vn_params).
:- mode vn_type__init_params(in, out) is det.

:- pred vn_type__word_size(vn_params, int).
:- mode vn_type__word_size(in, out) is det.

:- pred vn_type__real_r_regs(vn_params, int).
:- mode vn_type__real_r_regs(in, out) is det.

:- pred vn_type__real_temps(vn_params, int).
:- mode vn_type__real_temps(in, out) is det.

:- pred vn_type__costof_assign(vn_params, int).
:- mode vn_type__costof_assign(in, out) is det.

:- pred vn_type__costof_intops(vn_params, int).
:- mode vn_type__costof_intops(in, out) is det.

:- pred vn_type__costof_stackref(vn_params, int).
:- mode vn_type__costof_stackref(in, out) is det.

:- pred vn_type__costof_heapref(vn_params, int).
:- mode vn_type__costof_heapref(in, out) is det.

:- implementation.

:- import_module int.

:- type vn_params	--->	vn_params(
					int,	% word size in bytes
						% needed for incr_hp; incr_hp
					int,	% number of real r regs
					int,	% number of real temps
					int,	% cost of assign
					int,	% cost of int operation
					int,	% cost of stack reference
					int	% cost of heap reference
				).

vn_type__init_params(OptionTable, VnParams) :-
	getopt__lookup_int_option(OptionTable, num_real_r_regs, RealRegs),
	getopt__lookup_int_option(OptionTable, num_real_temps, RealTemps),
	getopt__lookup_int_option(OptionTable, bytes_per_word, WordBytes),
	VnParams = vn_params(WordBytes, RealRegs, RealTemps, 1, 1, 2, 2).

vn_type__word_size(vn_params(WordSize, _, _, _, _, _, _), WordSize).
vn_type__real_r_regs(vn_params(_, RealRegs, _, _, _, _, _), RealRegs).
vn_type__real_temps(vn_params(_, _, RealTemps, _, _, _, _), RealTemps).
vn_type__costof_assign(vn_params(_, _, _, AssignCost, _, _, _), AssignCost).
vn_type__costof_intops(vn_params(_, _, _, _, IntOpCost, _, _), IntOpCost).
vn_type__costof_stackref(vn_params(_, _, _, _, _, StackCost, _), StackCost).
vn_type__costof_heapref(vn_params(_, _, _, _, _, _, HeapCost), HeapCost).
