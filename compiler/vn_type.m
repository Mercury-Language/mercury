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
:- import_module bool, getopt, map, set, list, std_util.

:- type vn == int.

:- type vnlval		--->	vn_reg(reg_type, int)
			;	vn_temp(reg_type, int)
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
			;	vn_mem_ref(vn).

			% these lvals do not have vnlval parallels
			%	lvar(var)

:- type vnrval		--->	vn_origlval(vnlval)
			;	vn_mkword(tag, vn)
			;	vn_const(rval_const)
			;	vn_create(tag, list(maybe(rval)), bool, int)
			;	vn_unop(unary_op, vn)
			;	vn_binop(binary_op, vn, vn)
			;	vn_stackvar_addr(int)
			;	vn_framevar_addr(int)
			;	vn_heap_addr(vn, int, int).

			% these rvals do not have vnrval parallels
			%	var(var)

	% given a vnlval, figure out its type
:- pred vn_type__vnlval_type(vnlval::in, llds_type::out) is det.

	% given a vnrval, figure out its type
:- pred vn_type__vnrval_type(vnrval::in, llds_type::out) is det.

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
			;	vn_incr_sp(int, string)
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

:- pred vn_type__bytes_per_word(vn_params, int).
:- mode vn_type__bytes_per_word(in, out) is det.

:- pred vn_type__real_r_regs(vn_params, int).
:- mode vn_type__real_r_regs(in, out) is det.

:- pred vn_type__real_f_regs(vn_params, int).
:- mode vn_type__real_f_regs(in, out) is det.

:- pred vn_type__real_r_temps(vn_params, int).
:- mode vn_type__real_r_temps(in, out) is det.

:- pred vn_type__real_f_temps(vn_params, int).
:- mode vn_type__real_f_temps(in, out) is det.

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
					int,	% number of real f regs
					int,	% number of real r temps
					int,	% number of real f temps
					int,	% cost of assign
					int,	% cost of int operation
					int,	% cost of stack reference
					int	% cost of heap reference
				).

vn_type__init_params(OptionTable, VnParams) :-
	getopt__lookup_int_option(OptionTable, num_real_r_regs, RealRRegs),
	getopt__lookup_int_option(OptionTable, num_real_f_regs, RealFRegs),
	getopt__lookup_int_option(OptionTable, num_real_r_temps, RealRTemps),
	getopt__lookup_int_option(OptionTable, num_real_f_temps, RealFTemps),
	getopt__lookup_int_option(OptionTable, bytes_per_word, WordBytes),
	VnParams = vn_params(WordBytes, RealRRegs, RealFRegs,
		RealRTemps, RealFTemps, 1, 1, 2, 2).

vn_type__bytes_per_word(vn_params(BytesPerWord, _, _, _, _, _, _, _, _),
	BytesPerWord).
vn_type__real_r_regs(vn_params(_, RealRRegs, _, _, _, _, _, _, _),
	RealRRegs).
vn_type__real_f_regs(vn_params(_, _, RealFRegs, _, _, _, _, _, _),
	RealFRegs).
vn_type__real_r_temps(vn_params(_, _, _, RealRTemps, _, _, _, _, _),
	RealRTemps).
vn_type__real_f_temps(vn_params(_, _, _, _, RealFTemps, _, _, _, _),
	RealFTemps).
vn_type__costof_assign(vn_params(_, _, _, _, _, AssignCost, _, _, _),
	AssignCost).
vn_type__costof_intops(vn_params(_, _, _, _, _, _, IntOpCost, _, _),
	IntOpCost).
vn_type__costof_stackref(vn_params(_, _, _, _, _, _, _, StackCost, _),
	StackCost).
vn_type__costof_heapref(vn_params(_, _, _, _, _, _, _, _, HeapCost),
	HeapCost).

vn_type__vnrval_type(vn_origlval(Lval), Type) :-
	vn_type__vnlval_type(Lval, Type).
vn_type__vnrval_type(vn_create(_, _, _, _), data_ptr).
vn_type__vnrval_type(vn_mkword(_, _), data_ptr). % see comment in llds.m
vn_type__vnrval_type(vn_const(Const), Type) :-
	llds__const_type(Const, Type).
vn_type__vnrval_type(vn_unop(UnOp, _), Type) :-
	llds__unop_return_type(UnOp, Type).
vn_type__vnrval_type(vn_binop(BinOp, _, _), Type) :-
	llds__binop_return_type(BinOp, Type).
vn_type__vnrval_type(vn_stackvar_addr(_), data_ptr).
vn_type__vnrval_type(vn_framevar_addr(_), data_ptr).
vn_type__vnrval_type(vn_heap_addr(_, _, _), data_ptr).

vn_type__vnlval_type(vn_reg(RegType, _), Type) :-
	llds__register_type(RegType, Type).
vn_type__vnlval_type(vn_succip, code_ptr).
vn_type__vnlval_type(vn_maxfr, data_ptr).
vn_type__vnlval_type(vn_curfr, data_ptr).
vn_type__vnlval_type(vn_hp, data_ptr).
vn_type__vnlval_type(vn_sp, data_ptr).
vn_type__vnlval_type(vn_temp(RegType, _), Type) :-
	llds__register_type(RegType, Type).
vn_type__vnlval_type(vn_stackvar(_), word).
vn_type__vnlval_type(vn_framevar(_), word).
vn_type__vnlval_type(vn_succip(_), code_ptr).
vn_type__vnlval_type(vn_redoip(_), code_ptr).
vn_type__vnlval_type(vn_succfr(_), data_ptr).
vn_type__vnlval_type(vn_prevfr(_), data_ptr).
vn_type__vnlval_type(vn_field(_, _, _), word).
vn_type__vnlval_type(vn_mem_ref(_), word).
