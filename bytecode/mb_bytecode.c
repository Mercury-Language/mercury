
/*
** Copyright (C) 1997,2000-2001 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
**
*/

/* Imports */
#include	<assert.h>
#include	<limits.h>
#include	<string.h>

#include	"mb_bytecode.h"
#include	"mb_mem.h"
#include	"mb_module.h"
#include	"mb_util.h"

/* Exported definitions */

/* Local declarations */

/* 
** All read functions return true if successful
*/
static MB_Bool
MB_read_byte(FILE *fp, MB_Byte *byte_p);

static MB_Bool
MB_read_short(FILE *fp, MB_Short *short_p);

static MB_Bool
MB_read_int(FILE *fp, MB_Integer *int_p);

static MB_Bool
MB_read_word(FILE *fp, MB_Word *word_p);

static MB_Bool
MB_read_float(FILE *fp, MB_Float *float_p);

static MB_Bool
MB_read_cstring(FILE *fp, MB_CString *str_p);

static MB_Bool
MB_read_cons_id(FILE *fp, MB_Cons_id *cons_id_p);

static MB_Bool
MB_read_tag(FILE *fp, MB_Tag *tag_p);

static MB_Bool
MB_read_var_dir(FILE *fp, MB_Var_dir *var_dir_p);

static MB_Bool
MB_read_op_arg(FILE *fp, MB_Op_arg *op_arg_p);


/* Implementation */

MB_Bool
MB_read_bytecode_version_number(FILE *fp, MB_Short *version_number_p) {
	return MB_read_short(fp, version_number_p);
}

MB_Bool
MB_read_bytecode(FILE *fp, MB_Bytecode *bc_p)
{
	MB_Byte	c;

	if (! MB_read_byte(fp, &c)) {
		return FALSE;
	}

	bc_p->id = c;

	switch (bc_p->id) {
		case MB_BC_enter_pred: {
			MB_CString	str;
			MB_Short	pred_arity;
			MB_Byte		is_func;
			MB_Short	proc_count;

			if (MB_read_cstring(fp, &str) && 
				MB_read_short(fp, &pred_arity) &&
				MB_read_byte(fp, &is_func) &&
				MB_read_short(fp, &proc_count))
			{
				bc_p->opt.enter_pred.pred_name = str;
				bc_p->opt.enter_pred.pred_arity = pred_arity;
				bc_p->opt.enter_pred.is_func = is_func;
				bc_p->opt.enter_pred.proc_count = proc_count;

				return TRUE;
			}
			break;
		}
		case MB_BC_endof_pred:
			return TRUE;
			break;
		case MB_BC_enter_proc:
		{
			MB_Byte		proc_id;
			MB_Determinism	det;
			MB_Short	label_count, end_label;
			MB_Short	temp_count, list_length;
			MB_CString	*var_info;
			
			if (MB_read_byte(fp, &proc_id) &&
				MB_read_byte(fp, &det) &&
				MB_read_short(fp, &label_count) &&
				MB_read_short(fp, &end_label) &&
				MB_read_short(fp, &temp_count) &&
				MB_read_short(fp, &list_length))
			{
				int 		i;
				MB_CString	str;

				var_info = (list_length == 0) ?  NULL :
					MB_CODE_DATA_ALLOC(MB_CString,
							list_length); 

				for (i = 0; i < list_length; i++) {
					if (MB_read_cstring(fp, &str)) {
						var_info[i] = str;
					} else {
						MB_fatal("enter_proc var"
							" read error");
					}
				}

				bc_p->opt.enter_proc.mode_num = proc_id;
				bc_p->opt.enter_proc.det = det;
				bc_p->opt.enter_proc.label_count = label_count;
				bc_p->opt.enter_proc.end_label.index =
					end_label;
				bc_p->opt.enter_proc.temp_count = temp_count;
				bc_p->opt.enter_proc.list_length = list_length;
				bc_p->opt.enter_proc.var_info = var_info;
				return TRUE;
			} else {
				MB_fatal("enter_proc read error");
			}
			break;
		}
		case MB_BC_endof_proc:
			return TRUE;
			break;
		case MB_BC_label: {
			MB_Short	label;

			if (MB_read_short(fp, &label)) {
				bc_p->opt.label.label = label;
				return TRUE;
			} else {
				MB_fatal("label bytecode not followed "
					"by label");
			}
			break;
		}
		case MB_BC_enter_disjunction:
		{
			MB_Short	end_label;

			if (MB_read_short(fp, &end_label)) {
				bc_p->opt.enter_disjunction.end_label.index =
					end_label;
				return TRUE;
			} else {
				MB_fatal("enter_disjunction not"
					" followed by label");
			}	
			break;
		}
		case MB_BC_endof_disjunction:
			return TRUE;
			break;
		case MB_BC_enter_disjunct: {
			MB_Short	next_label;

			if (MB_read_short(fp, &next_label)) {
				bc_p->opt.enter_disjunct.next_label.index =
					next_label;
				return TRUE;
			} else {
				MB_fatal("enter_disjunct not followed by"
					" label");
			}
			break;
		}
		case MB_BC_endof_disjunct: {
			MB_Short	label;

			if (MB_read_short(fp, &label)) {
				bc_p->opt.endof_disjunct.end_label.index
					= label;
				return TRUE;
			} else {
				MB_fatal("endof_disjunct read error");
			}
			break;
		}
		case MB_BC_enter_switch: {
			MB_Short	var;
			MB_Short	end_label;

			if (MB_read_short(fp, &var) && 
				MB_read_short(fp, &end_label))
			{
				bc_p->opt.enter_switch.var = var;
				bc_p->opt.enter_switch.end_label.index =
					end_label;
				return TRUE;
			} else {
				MB_fatal("enter_switch read error");
			}
			break;
		}
		case MB_BC_endof_switch:
			return TRUE;
			break;
		case MB_BC_enter_switch_arm: {
			MB_Cons_id	cons_id;
			MB_Short	next_label;
			
			if (MB_read_cons_id(fp, &cons_id) && 
				MB_read_short(fp, &next_label))
			{
				bc_p->opt.enter_switch_arm.cons_id = cons_id;
				bc_p->opt.enter_switch_arm.next_label.index = 
					next_label;
				return TRUE;
			} else {
				MB_fatal("enter_switch_arm read error");
			}
			break;
		}
		case MB_BC_endof_switch_arm: {
			MB_Short	label;

			if (MB_read_short(fp, &label)) {
				bc_p->opt.endof_switch_arm.end_label.index =
					label;
				return TRUE;
			} else {
				MB_fatal("endof_switch_arm read error");
			}
			break;
		}
		case MB_BC_enter_if: {
			MB_Short	else_label, end_label, frame_ptr_tmp;

			if (MB_read_short(fp, &else_label) &&
				MB_read_short(fp, &end_label) &&
				MB_read_short(fp, &frame_ptr_tmp))
			{
				bc_p->opt.enter_if.else_label.index =
					else_label;
				bc_p->opt.enter_if.end_label.index =
					end_label;
				bc_p->opt.enter_if.frame_ptr_tmp =
					frame_ptr_tmp;
				return TRUE;
			} else {
				MB_fatal("enter_if read error");
			}
			break;
		}
		case MB_BC_enter_then: {
			MB_Short	frame_ptr_tmp;

			if (MB_read_short(fp, &frame_ptr_tmp))
			{
				bc_p->opt.enter_then.frame_ptr_tmp =
					frame_ptr_tmp;
				return TRUE;
			} else {
				MB_fatal("enter_then read error");
			}
			break;
		}
		case MB_BC_endof_then: {
			MB_Short	follow_label;

			if (MB_read_short(fp, &follow_label)) {
				bc_p->opt.endof_then.follow_label.index =
					follow_label;
				return TRUE;
			} else {
				MB_fatal("endof_then read error");
			}
			break;
		}

		case MB_BC_enter_else: {
			MB_Short	frame_ptr_tmp;

			if (MB_read_short(fp, &frame_ptr_tmp))
			{
				bc_p->opt.enter_else.frame_ptr_tmp =
					frame_ptr_tmp;
				return TRUE;
			} else {
				MB_fatal("enter_else read error");
			}
			break;
		}
				       
		case MB_BC_endof_if:
			return TRUE;
			break;
			
		case MB_BC_enter_negation: {
			MB_Short	frame_ptr_tmp;
			MB_Short	end_label;
		
			if (MB_read_short(fp, &frame_ptr_tmp) &&
				MB_read_short(fp, &end_label))
			{
				bc_p->opt.enter_negation.frame_ptr_tmp =
					frame_ptr_tmp;
				bc_p->opt.enter_negation.end_label.index =
					end_label;
				return TRUE;
			} else {
				MB_fatal("enter_negation read error");
			}
			break;
		}
		case MB_BC_endof_negation_goal: {
			MB_Short	frame_ptr_tmp;
		
			if (MB_read_short(fp, &frame_ptr_tmp))
			{
				bc_p->opt.endof_negation_goal.frame_ptr_tmp =
					frame_ptr_tmp;
				return TRUE;
			} else {
				MB_fatal("enter_negation_goal read error");
			}
			break;
		}
		case MB_BC_endof_negation:
			return TRUE;
			break;
			
		case MB_BC_enter_commit: {
			MB_Short	frame_ptr_tmp;
			
			if (MB_read_short(fp, &frame_ptr_tmp)) {
				bc_p->opt.enter_commit.frame_ptr_tmp =
					frame_ptr_tmp;
				return TRUE;
			} else {
				MB_fatal("enter_commit read error");
			}
			break;
		}
		case MB_BC_endof_commit: {
			MB_Short	frame_ptr_tmp;
			
			if (MB_read_short(fp, &frame_ptr_tmp)) {
				bc_p->opt.endof_commit.frame_ptr_tmp
					= frame_ptr_tmp;
				return TRUE;
			} else {
				MB_fatal("endof_commit read error");
			}
			break;
		}
		case MB_BC_assign: {
			MB_Short	to_var, from_var;

			if (MB_read_short(fp, &to_var) &&
				MB_read_short(fp, &from_var))
			{
				bc_p->opt.assign.to_var = to_var;
				bc_p->opt.assign.from_var = 
					from_var;
				return TRUE;
			} else {
				MB_fatal("assign read error");
			}
			break;
		}
		case MB_BC_test: {
			MB_Short	var1, var2;

			if (MB_read_short(fp, &var1) && 
				MB_read_short(fp, &var2))
			{
				bc_p->opt.test.var1 = var1;
				bc_p->opt.test.var2 = var2;
				return TRUE;
			} else {
				MB_fatal("test read error");
			}
			break;
		}
		case MB_BC_construct: {
			MB_Short	to_var;
			MB_Cons_id	consid;
			MB_Short	list_length;
			MB_Short	*var_list = NULL;

			if (MB_read_short(fp, &to_var) &&
				MB_read_cons_id(fp, &consid) &&
				MB_read_short(fp, &list_length))
			{
				MB_Short i;

				var_list = (list_length == 0) ?  NULL : 
					MB_CODE_DATA_ALLOC(MB_Short,
							list_length);

				for (i = 0; i < list_length; i++) {
					MB_Short	var;

					if (MB_read_short(fp, &var)) {
						var_list[i] = var;
					} else {
						MB_fatal("construct var"
							" read error");
					}
				}


				bc_p->opt.construct.to_var = to_var;
				bc_p->opt.construct.consid = consid;
				bc_p->opt.construct.list_length = list_length;
				bc_p->opt.construct.var_list = var_list;
				return TRUE;
			} else {
				MB_fatal("construct read error");
			}
			break;
		}
		case MB_BC_deconstruct: {
			MB_Short	from_var;
			MB_Cons_id	consid;
			MB_Short	list_length;
			MB_Short	*var_list;

			if (MB_read_short(fp, &from_var) &&
				MB_read_cons_id(fp, &consid) &&
				MB_read_short(fp, &list_length))
			{
				MB_Short i;

				var_list = (list_length == 0) ?  NULL : 
					MB_CODE_DATA_ALLOC(MB_Short,
							list_length);

				for (i = 0; i < list_length; i++) {
					MB_Short	var;

					if (MB_read_short(fp, &var)) {
						var_list[i] = var;
					} else {
						MB_fatal("deconstruct var"
							" read error");
					}
				}

				bc_p->opt.deconstruct.from_var =
					from_var;
				bc_p->opt.deconstruct.consid = consid;
				bc_p->opt.deconstruct.list_length = 
					list_length;
				bc_p->opt.deconstruct.var_list = var_list;
				return TRUE;
			} else {
				MB_fatal("deconstruct read error");
			} 
			break;
		}
		case MB_BC_complex_construct: {
			MB_Short	from_var;
			MB_Cons_id	consid;
			MB_Short	list_length;

			if (MB_read_short(fp, &from_var) &&
				MB_read_cons_id(fp, &consid) &&
				MB_read_short(fp, &list_length))
			{
				MB_Var_dir	*var_dir_list;
				MB_Var_dir	var_dir;
				int		i;

				var_dir_list = MB_CODE_DATA_ALLOC(MB_Var_dir,
								list_length);

				for (i = 0; i < list_length ; i++) {
					if (MB_read_var_dir(fp, &var_dir)) {
						var_dir_list[i] = var_dir;
					} else {
						MB_fatal("complex_construct"
							" var read error");
					}
				}

				bc_p->opt.complex_construct.to_var = from_var;
				bc_p->opt.complex_construct.consid = consid;
				bc_p->opt.complex_construct.list_length 
					= list_length;
				bc_p->opt.complex_construct.var_dir
					= var_dir_list;
				return TRUE;
			} else {
				MB_fatal("complex_construct read error");
			}
			break;
		}
		case MB_BC_complex_deconstruct: {
			MB_Short	from_var;
			MB_Cons_id	consid;
			MB_Short	list_length;

			if (MB_read_short(fp, &from_var) &&
				MB_read_cons_id(fp, &consid) &&
				MB_read_short(fp, &list_length))
			{
				MB_Var_dir	*var_dir_list;
				MB_Var_dir	var_dir;
				int		i;

				var_dir_list = MB_CODE_DATA_ALLOC(MB_Var_dir,
								list_length);

				for (i = 0; i < list_length; i++) {
					if (MB_read_var_dir(fp, &var_dir)) {
						var_dir_list[i] = var_dir;
					} else {
						MB_fatal("complex_deconstruct"
							" var read error");
					}
				}
				bc_p->opt.complex_deconstruct.from_var =
					from_var;
				bc_p->opt.complex_deconstruct.consid = consid;
				bc_p->opt.complex_deconstruct.list_length =
					list_length;
				bc_p->opt.complex_deconstruct.var_dir =
					var_dir_list;
				return TRUE;
			} else {
				MB_fatal("complex_deconstruct read error");
			}
			break;
		}
		case MB_BC_place_arg: {
			MB_Byte		to_reg;
			MB_Short	from_var;

			if (MB_read_byte(fp, &to_reg) &&
				MB_read_short(fp, &from_var))
			{
				bc_p->opt.place_arg.to_reg = to_reg;
				bc_p->opt.place_arg.from_var =
					from_var;
				return TRUE;
			} else {
				MB_fatal("place_arg read error");
			}
			break;
		}
		case MB_BC_pickup_arg: {
			MB_Byte		from_reg;
			MB_Short	to_var;

			if (MB_read_byte(fp, &from_reg) &&
				MB_read_short(fp, &to_var))
			{
				bc_p->opt.pickup_arg.from_reg =
					from_reg;
				bc_p->opt.pickup_arg.to_var =
					to_var;
				return TRUE;
			} else {
				MB_fatal("pickup_arg read error");
			}
			break;
		}
		case MB_BC_call: {
			MB_CString	module_id;
			MB_CString	pred_id;
			MB_Short	arity;
			MB_Byte		is_func;
			MB_Byte		proc_id;

			if (MB_read_cstring(fp, &module_id) &&
				MB_read_cstring(fp, &pred_id) &&
				MB_read_short(fp, &arity) &&
				MB_read_byte(fp, &is_func) &&
				MB_read_byte(fp, &proc_id))
			{
				bc_p->opt.call.module_name =
					module_id;
				bc_p->opt.call.pred_name =
					pred_id;
				bc_p->opt.call.arity = arity;
				bc_p->opt.call.is_func = is_func;
				bc_p->opt.call.mode_num = proc_id;

				/*
				** Initialise code address to invalid in case
				** it somehow gets executed
				*/
				bc_p->opt.call.addr.is_native = FALSE;
				bc_p->opt.call.addr.addr.bc = MB_CODE_INVALID_ADR;
				return TRUE;
			} else {
				MB_fatal("call read error");
			}
			break;
		}
		case MB_BC_higher_order_call: {
			MB_Short	pred_var;
			MB_Short	in_var_count;
			MB_Short	out_var_count;
			MB_Determinism	det;

			if (MB_read_short(fp, &pred_var) &&
				MB_read_short(fp, &in_var_count) &&
				MB_read_short(fp, &out_var_count) &&
				MB_read_byte(fp, &det))
			{
				bc_p->opt.higher_order_call.pred_var
					= pred_var;
				bc_p->opt.higher_order_call.
					in_var_count = in_var_count;
				bc_p->opt.higher_order_call.
					out_var_count = out_var_count;
				bc_p->opt.higher_order_call.
					det = det;
				return TRUE;
			} else {
				MB_fatal("higher_order_call read error");
			}
			break;
		}
		case MB_BC_builtin_binop: {
			MB_Byte		binop;
			MB_Op_arg	arg1;
			MB_Op_arg	arg2;
			MB_Short	to_var;

			if (MB_read_byte(fp, &binop) &&
				MB_read_op_arg(fp, &arg1) &&
				MB_read_op_arg(fp, &arg2) &&
				MB_read_short(fp, &to_var))
			{
				bc_p->opt.builtin_binop.binop = binop;
				bc_p->opt.builtin_binop.arg1 = arg1;
				bc_p->opt.builtin_binop.arg2 = arg2;
				bc_p->opt.builtin_binop.to_var = to_var;
				return TRUE;
			} else {
				MB_fatal("builtin_binop read error");
			}
			break;
		}
		case MB_BC_builtin_unop: {
			MB_Byte		unop;
			MB_Op_arg	arg;
			MB_Short	to_var;

			if (MB_read_byte(fp, &unop) &&
				MB_read_op_arg(fp, &arg) &&
				MB_read_short(fp, &to_var))
			{
				bc_p->opt.builtin_unop.unop = unop;
				bc_p->opt.builtin_unop.arg = arg;
				bc_p->opt.builtin_unop.to_var = to_var;
				return TRUE;
			} else {
				MB_fatal("builtin_unop read error");
			}
			break;
		}
		case MB_BC_builtin_bintest: {
			MB_Byte		binop;
			MB_Op_arg	arg1;
			MB_Op_arg	arg2;

			if (MB_read_byte(fp, &binop) &&
				MB_read_op_arg(fp, &arg1) &&
				MB_read_op_arg(fp, &arg2))
			{
				bc_p->opt.builtin_bintest.binop = binop;
				bc_p->opt.builtin_bintest.arg1 = arg1;
				bc_p->opt.builtin_bintest.arg2 = arg2;
				return TRUE;
			} else {
				MB_fatal("builtin_bintest read error");
			}
			break;
		}
		case MB_BC_builtin_untest: {
			MB_Byte		unop;
			MB_Op_arg	arg;

			if (MB_read_byte(fp, &unop) &&
				MB_read_op_arg(fp, &arg))
			{
				bc_p->opt.builtin_untest.unop = unop;
				bc_p->opt.builtin_untest.arg = arg;
				return TRUE;
			} else {
				MB_fatal("builtin_untest read error");
			}
			break;
		}
		case MB_BC_semidet_succeed:
			return TRUE;
			break;
		case MB_BC_semidet_success_check:
			return TRUE;
			break;
		case MB_BC_fail:
			return TRUE;
			break;
		case MB_BC_context: {
			MB_Short	line_number;

			if (MB_read_short(fp, &line_number))
			{
				bc_p->opt.context.line_number =
					line_number;
				return TRUE;
			} else {
				MB_fatal("context read error");
			}
			break;
		}
		case MB_BC_not_supported:
			return TRUE;
			break;

		default:
			MB_fatal("bytecode.MB_read_bytecode: unknown bytecode");
			break;
	} /* end switch */
	return FALSE;
} /* end MB_read_bytecode() */

static MB_Bool
MB_read_byte(FILE *fp, MB_Byte *byte_p)
{
	int c;

	if ((c = fgetc(fp)) != EOF) {
		*byte_p = (MB_Byte) c;
		return TRUE;
	} else {
		return FALSE;
	}
}

/*
**	In bytecode file, short is:
**		- bigendian
**		- two bytes
**		- 2's complement
*/
static MB_Bool
MB_read_short(FILE *fp, MB_Short *short_p)
{
	MB_Byte		c0, c1;

	if (MB_read_byte(fp, &c0) && MB_read_byte(fp, &c1)) {
		*short_p = (c0 << 8) | c1;
		return TRUE;
	} else {
		MB_fatal("Unexpected file error reading short");
		return FALSE; /* not reached */
	}
} /* MB_read_short */

/*
**	In bytecode file, int is:
**		- big-endian (read big end first)
**		- eight bytes
**		- 2's complement
*/
static MB_Bool
MB_read_int(FILE *fp, MB_Integer *int_p)
{
	/*
	** c0 is the big end.
	*/
	MB_Byte		c0, c1, c2, c3, c4, c5, c6, c7;

	if (MB_read_byte(fp, &c0) && MB_read_byte(fp, &c1) && 
		MB_read_byte(fp, &c2) && MB_read_byte(fp, &c3) &&
		MB_read_byte(fp, &c4) && MB_read_byte(fp, &c5) &&
		MB_read_byte(fp, &c6) && MB_read_byte(fp, &c7))
	{

		MB_Integer	tmp_int = 0;

		if (sizeof(MB_Integer) * CHAR_BIT == 32) {
			/*
			** If a 64-bit 2's-complement integer fits into
			** 32 bits, then all the bits in the big half
			** are either all ones or all zeros.
			** We test this to make sure we can convert the
			** 64-bit int to 32-bits on a 32-bit platform.
			** I'm not personally enamoured of this approach. 8^(
			*/
			if ((c0==0x0 && c1==0x0 && c2==0x0 && c3==0x0) ||
				(c0==0xff && c1==0xff && c2==0xff && c3==0xff))
			{
				tmp_int = c4;
				tmp_int <<= 8; tmp_int |= c5;
				tmp_int <<= 8; tmp_int |= c6;
				tmp_int <<= 8; tmp_int |= c7;
			} else {
				MB_fatal("64-bit integer constant in bytecode "
					"file does not fit into 32 bits"
				);
			}
		} else if (sizeof(MB_Integer) * CHAR_BIT == 64) {
			tmp_int = c0;
			tmp_int <<= 8; tmp_int |= c1;
			tmp_int <<= 8; tmp_int |= c2;
			tmp_int <<= 8; tmp_int |= c3;
			tmp_int <<= 8; tmp_int |= c4;
			tmp_int <<= 8; tmp_int |= c5;
			tmp_int <<= 8; tmp_int |= c6;
			tmp_int <<= 8; tmp_int |= c7;
		} else {
			/*
			** XXX: What about 16-bit or other sizes?
			*/
			MB_fatal("MB_Integer is neither 32- nor 64-bit");
		}
		
		*int_p = tmp_int;
		return TRUE;
	} else {
		MB_fatal("Unexpected file error reading int");
		return FALSE;
	}
} /* MB_read_int */

/*
** ASSUMPTION: Mercury assumes that `Integer' and `Word' are the same size
** (see runtime/mercury_types.h).  We make the same assumption here.
*/
static MB_Bool
MB_read_word(FILE *fp, MB_Word *word_p)
{
	return MB_read_int(fp, word_p);
}

/*
** Read a Float from the bytecode stream.
** Note: In the bytecode file, a floating point value is represented
** using a Float64 (big-endian 64-bit IEEE-754).
** However, we return a Float, which may differ from Float64.
*/
static MB_Bool
MB_read_float(FILE *fp, MB_Float *float_p)
{
	MB_Byte 	c0, c1, c2, c3, c4, c5, c6, c7;
	MB_Float64	float64;
	MB_Byte		*float64_p;

	float64_p = (MB_Byte *) &float64;

	if (MB_read_byte(fp, &c0) && MB_read_byte(fp, &c1) && 
		MB_read_byte(fp, &c2) && MB_read_byte(fp, &c3) &&
		MB_read_byte(fp, &c4) && MB_read_byte(fp, &c5) &&
		MB_read_byte(fp, &c6) && MB_read_byte(fp, &c7))
	{
		#if	defined(MR_BIG_ENDIAN)
			float64_p[0] = c0;
			float64_p[1] = c1;
			float64_p[2] = c2;
			float64_p[3] = c3;
			float64_p[4] = c4;
			float64_p[5] = c5;
			float64_p[6] = c6;
			float64_p[7] = c7;
		#elif	defined(MR_LITTLE_ENDIAN)
			float64_p[0] = c7;
			float64_p[1] = c6;
			float64_p[2] = c5;
			float64_p[3] = c4;
			float64_p[4] = c3;
			float64_p[5] = c2;
			float64_p[6] = c1;
			float64_p[7] = c0;
		#else
			#error Architecture is neither big- nor little-endian.
		#endif

		/*
		** The following cast may lose information. 
		** We may cast a double to a float, for instance.
		*/
		*float_p = (MB_Float) float64;

		return TRUE;
	} else {
		return FALSE;
	}
}

/*
** Returned string is allocated with string routines MB_str_xxx
** It is the responsibility of the caller to free it using MB_str_delete
*/
static MB_Bool
MB_read_cstring(FILE *fp, MB_CString *str_p)
{
	/*
	** Initially tries to read string into buffer, but if this gets
	** full then mallocs another buffer which is doubled in size
	** whenever it gets full.
	** Returned string is allocated with MB_str_dup
	*/
	char	buffer[64];
	MB_Word	bufsize = sizeof(buffer);
	char	*str = buffer;
	
	MB_Word		i = 0;
	MB_Byte		c;
	
	do {
		/* get the next char */
		if (!MB_read_byte(fp, &c)) {
			MB_fatal("Error reading C String from file");
		}

		/*
		** If the next char is going to overflow the buffer then
		** expand the buffer
		*/
		if (i == bufsize) {
			/* Double the size of the buffer */
			bufsize *= 2;
			
			if (str == buffer) {
				/*
				** If we are still using the stack buffer,
				** allocate a new buffer with malloc
				*/
				str = MB_malloc(bufsize);
				memcpy(str, buffer, bufsize/2);
			} else {
				/*
				** The current buffer is already malloced;
				** realloc it
				*/
				str = MB_realloc(str, bufsize);
			}

			if (str == NULL) return FALSE;
		}

		str[i++] = c;

	} while (c != 0);

	if ((*str_p = MB_str_dup(str)) == NULL) {
		return FALSE;
	}

	/* Free the string if it isn't on the local stack */
	if (str != buffer) {
		MB_free(str);
	}

	return TRUE;
} /* end MB_read_cstring() */



static MB_Bool
MB_read_cons_id(FILE *fp, MB_Cons_id *cons_id_p)
{
	MB_Byte 	c;

	if (!MB_read_byte(fp, &c)) {
		MB_util_error("Unable to read constructor id\n");
		return FALSE;
	}

	cons_id_p->id = c;

	switch (c) {
		case MB_CONSID_CONS: {
			MB_CString	module_id;
			MB_CString	string;
			MB_Short	arity;
			MB_Tag		tag;

			if (MB_read_cstring(fp, &module_id) &&
				MB_read_cstring(fp, &string) &&
				MB_read_short(fp, &arity) &&
				MB_read_tag(fp, &tag))
			{
				cons_id_p->opt.cons.module_name = module_id;
				cons_id_p->opt.cons.string = string;
				cons_id_p->opt.cons.arity = arity;
				cons_id_p->opt.cons.tag = tag;
				return TRUE;
			} else {
				MB_util_error("Unable to read constructor"
						" module id\n");
				return FALSE;
			}
			break;
		}
		case MB_CONSID_INT_CONST: {
			MB_Integer	int_const;

			if (MB_read_int(fp, &int_const)) {
				cons_id_p->opt.int_const = int_const;
				return TRUE;
			} else {
				MB_util_error("Unable to read constructor"
						" integer constant\n");
				return FALSE;
			}
			break;
		}
		case MB_CONSID_STRING_CONST: {
			MB_CString	string_const;

			if (MB_read_cstring(fp, &string_const)) {
				cons_id_p->opt.string_const = string_const;
				return TRUE;
			} else {
				MB_util_error("Unable to read constructor"
						" string constant\n");
				return FALSE;
			}
			break;
		}
		case MB_CONSID_FLOAT_CONST: {
			MB_Float	float_const;

			if (MB_read_float(fp, &float_const)) {
				cons_id_p->opt.float_const = float_const;
				return TRUE;
			} else {
				MB_util_error("Unable to read constructor"
						" float constant\n");
				return FALSE;
			}
			break;
		}
		case MB_CONSID_PRED_CONST: {
			MB_CString	module_id;
			MB_CString	pred_id;
			MB_Short	arity;
			MB_Byte		is_func;
			MB_Byte		proc_id;

			if (MB_read_cstring(fp, &module_id) &&
				MB_read_cstring(fp, &pred_id) &&
				MB_read_short(fp, &arity) &&
				MB_read_byte(fp, &is_func) &&
				MB_read_byte(fp, &proc_id))
			{
				cons_id_p->opt.pred_const.module_name=module_id;
				cons_id_p->opt.pred_const.pred_name = pred_id;
				cons_id_p->opt.pred_const.arity = arity;
				cons_id_p->opt.pred_const.is_func = is_func;
				cons_id_p->opt.pred_const.mode_num = proc_id;
				return TRUE;
			} else {
				MB_util_error("Unable to read predicate"
						" constructor\n");
				return FALSE;
			}
			break;
		}
		case MB_CONSID_CODE_ADDR_CONST:
		{
			MB_CString	module_id;
			MB_CString	pred_id;
			MB_Short	arity;
			MB_Byte		proc_id;

			if (MB_read_cstring(fp, &module_id) &&
				MB_read_cstring(fp, &pred_id) &&
				MB_read_short(fp, &arity) &&
				MB_read_byte(fp, &proc_id))
			{
				cons_id_p->opt.code_addr_const.module_name = 
					module_id;
				cons_id_p->opt.code_addr_const.pred_name =
					pred_id;
				cons_id_p->opt.code_addr_const.arity = arity;
				cons_id_p->opt.code_addr_const.mode_num =
					proc_id;
				return TRUE;
			} else {
				MB_util_error("Unable to read constructor"
						" code address constant\n");
				return FALSE;
			}
			break;
		}
		case MB_CONSID_BASE_TYPE_INFO_CONST: {
			MB_CString	module_id;
			MB_CString	type_name;
			MB_Byte		type_arity;

			if (MB_read_cstring(fp, &module_id) && 
				MB_read_cstring(fp, &type_name) && 
				MB_read_byte(fp, &type_arity)) 
			{
				cons_id_p->opt.base_type_info_const.module_name
					= module_id;
				cons_id_p->opt.base_type_info_const.type_name = 
					type_name;
				cons_id_p->opt.base_type_info_const.type_arity =
					type_arity;
				return TRUE;
			} else {
				MB_util_error("Unable to read constructor"
						" base type information\n");
				return FALSE;
			}
			break;
		}
		case MB_CONSID_CHAR_CONST: {
			MB_Byte		ch;
			
			if (MB_read_byte(fp, &ch)) {
				cons_id_p->opt.char_const.ch = ch;
				return TRUE;
			} else {
				MB_util_error("Unable to read constructor"
						" character constant\n");
				return FALSE;
			}
		}
		default:
			MB_util_error("Unknown constructor type\n");
			return FALSE;
			break;
	} /* end switch */

	assert(FALSE);	/* not reached */
	return FALSE;

} /* end MB_read_cons_id() */


static MB_Bool
MB_read_tag(FILE *fp, MB_Tag *tag_p)
{
	MB_Byte		c;

	if (!MB_read_byte(fp, &c)) {
		MB_util_error("Unable to read tag\n");
		return FALSE; /* not reached */
	}

	tag_p->id = c;

	switch (c) {
		case MB_TAG_SIMPLE: {
			MB_Byte		primary;

			if (MB_read_byte(fp, &primary)) {
				tag_p->opt.primary = primary;
				return TRUE;
			} else {
				MB_util_error("Unable to read simple tag\n");
				return FALSE;
			}
			break;
		}
		/* 
		** The following two cases behave identically. 
		*/
		case MB_TAG_COMPLICATED:
		case MB_TAG_COMPLICATED_CONSTANT:
		{
			MB_Byte		primary;
			MB_Word		secondary; 
			
			if (MB_read_byte(fp, &primary) && 
				MB_read_word(fp, &secondary))
			{
				tag_p->opt.pair.primary = primary;
				tag_p->opt.pair.secondary = secondary;
				return TRUE;
			} else {
				MB_util_error(
					"Unable to read complicated tag\n");
				return FALSE;
			}
			break;
		}
		case MB_TAG_ENUM: {
			MB_Byte		enum_tag;

			if (MB_read_byte(fp, &enum_tag)) {
				tag_p->opt.enum_tag = enum_tag;
				return TRUE;
			} else {
				MB_util_error("Unable to read enum tag\n");
				return FALSE;
			}
			break;
		}
		case MB_TAG_NONE:
			/* XXX: Hmm... What's MB_TAG_NONE for?? */
			return TRUE; 
			break;
		default:
			MB_util_error("Unknown tag type\n");
			return FALSE;
			break;
	} /* switch */

	assert(FALSE);	/* not reached */
	return FALSE;
} /* end MB_read_tag() */


static MB_Bool
MB_read_var_dir(FILE *fp, MB_Var_dir *var_dir_p)
{
	MB_Short	var;
	MB_Direction	dir;

	if (MB_read_short(fp, &var) && MB_read_byte(fp, &dir)) {
		var_dir_p->var = var;
		var_dir_p->dir = dir;
		return TRUE;
	} else {
		return FALSE;
	}
} /* MB_read_var_dir() */


static MB_Bool
MB_read_op_arg(FILE *fp, MB_Op_arg *op_arg_p)
{
	MB_Byte		id;

	if ( ! MB_read_byte(fp, &id)) {
		return FALSE;
	}

	op_arg_p->id = id;

	switch (id) {
		case MB_ARG_VAR: {
			MB_Short	var;

			if (MB_read_short(fp, &var)) {
				op_arg_p->opt.var = var;
				return TRUE;
			} else {
				MB_util_error("Unable to read variable"
						" argument\n");
				return FALSE;
			}
			break;
		}
		case MB_ARG_INT_CONST: {
			MB_Integer	int_const;

			if (MB_read_int(fp, &int_const)) {
				op_arg_p->opt.int_const = int_const;
				return TRUE;
			} else {
				MB_util_error("Unable to read integer constant"
						" argument\n");
				return FALSE;
			}
			break;
		}
		case MB_ARG_FLOAT_CONST: {
			MB_Float	float_const;

			if (MB_read_float(fp, &float_const)) {
				op_arg_p->opt.float_const = 
					float_const;
				return TRUE;
			} else {
				MB_util_error("Unable to read float constant"
						" argument\n");
				return FALSE;
			}
			break;
		}
		default:
			MB_util_error("Unknown op argument type\n");
			return FALSE;
	} /* end switch */

	assert(FALSE);	/* not reached */
	return FALSE;
} /* end MB_read_op_arg() */


