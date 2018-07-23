
/*
** Copyright (C) 1997,2000-2001 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
**
** Contains functions for disassembling bytecodes into human readable form
**
*/

/* Imports */
#include	"mb_disasm.h"

#include	<string.h>
#include	"mb_module.h"

/* Exported definitions */

int MB_str_bytecode(MB_Bytecode_Addr addr, char *buffer, int buffer_len,
			int indent_level);

void MB_listing(MB_Machine_State *ms, FILE *fp, MB_Bytecode_Addr start,
	MB_Bytecode_Addr end, MB_Word line_len);

/* Local declarations */

/* Fills a c string buffer with to the name of a constructor id */
static void str_cons_id(MB_Cons_id cons_id, char *buffer, int buffer_len);

/* Fills a c string buffer with to the name of a tag */
static void str_tag(MB_Tag tag, char *buffer, int buffer_len);

/* Returns a string corresponding to the name of a bytecode type */
static MB_CString_Const str_bytecode_name(MB_Byte bytecode_id);

/* Returns a string corresponding to the name of a test type */
static MB_CString_Const str_test_id(MB_Test_id test_id);

/* Returns a string corresponding to the name of a determinism type */
static MB_CString_Const str_determinism_name(MB_Byte determinism_id);

/* Returns a string corresponding to the name of a unary operation */
static MB_CString_Const str_unop_name(MB_Byte unop);

/* Returns a string corresponding to the name of a binary operation */
static MB_CString_Const str_binop_name(MB_Byte binop);

/* Fills a buffer with a string transformed into a source-style c string 
 * (includes double quotes around string) */
static void quote_cstring(MB_CString str, char *buffer, int buffer_len);

/* Fills a c string buffer with a variable list */
static void str_var_dir(MB_Var_dir var_dir, char *buffer, int buffer_len);

/* Fills a c string buffer with an operation argument */
static void str_op_arg(MB_Op_arg op_arg, char *buffer, int buffer_len);

/* Implementation */


/*
** Macros for printing:
** Expects buffer & buffer_len to be defined.
** Wraps calls to MR_snprintf, checks if the buffer is full and
** if it is, returns from the function
*/

#define PRINT()		if (buffer_len > 1) {			\
				int last_len;			\
				assert(buffer_len > 0);		\
				last_len = MR_snprintf(buffer, buffer_len,

/* printf arguments get sandwiched between these macros */
#define ENDPRINT()				);		\
				if (last_len >= buffer_len) {	\
					last_len = buffer_len-1;\
				} \
				buffer += last_len;		\
				buffer_len -= last_len;		\
				assert(buffer_len > 0);		\
			} else {				\
				assert(buffer_len >= 0);	\
				if (buffer_len == 0) {		\
					buffer--; 		\
					buffer_len++;		\
				}				\
			}

/*
** Call this after calling a function that has added characters to the buffer
** Requires that if the function filled the buffer, it must have put a null
** terminator at the end
*/
#define PRINTFILLCHECK() {					\
				int str_len = strlen(buffer);	\
				buffer += str_len;		\
				buffer_len -= str_len;		\
				assert(buffer_len > 0); 	\
			}

/*
** Macro to call a function of the format f(arg, buffer, buffer_len)
** where the function fills all or part of the buffer
*/
#define PRINTCALL(x, y)	(x)((y), buffer, buffer_len);		\
			PRINTFILLCHECK()

/*
** Fills a string corresponding to a bytecode
** Returns indent level for subsequent instructions
**
** If buffer is NULL or buffer_len is <= 0 then only returns
**  new indent level
**
** The general convention is for arguments to be in square brackets and
** calculated arguments (ie not in the bytecode file) in round brackets
*/

int
MB_str_bytecode(MB_Bytecode_Addr addr, char *buffer,
		int buffer_len, int indent_level)
{
	MB_Byte bc_id = MB_code_get_id(addr);
	MB_Bytecode_Arg *bca = MB_code_get_arg(addr);

	/* calculate indent changes */
	int this_indent = indent_level;
	int next_indent = indent_level;

	switch (bc_id) {
		case MB_BC_enter_pred:
		case MB_BC_enter_proc:
		case MB_BC_enter_disjunction:
		case MB_BC_enter_disjunct:
		case MB_BC_enter_switch:
		case MB_BC_enter_switch_arm:
		case MB_BC_enter_if:
		case MB_BC_enter_negation:
		case MB_BC_enter_commit:
			next_indent++;
			break;
		case MB_BC_label:
		case MB_BC_enter_then:
		case MB_BC_endof_then:
		case MB_BC_enter_else:
		case MB_BC_endof_negation_goal:
			this_indent--;
			break;
		case MB_BC_endof_pred:
		case MB_BC_endof_proc:
		case MB_BC_endof_disjunction:
		case MB_BC_endof_disjunct:
		case MB_BC_endof_switch:
		case MB_BC_endof_switch_arm:
		case MB_BC_endof_if:
		case MB_BC_endof_negation:
		case MB_BC_endof_commit:
			this_indent--;
			next_indent--;
			break;
		case MB_BC_assign:
		case MB_BC_test:
		case MB_BC_construct:
		case MB_BC_deconstruct:
		case MB_BC_complex_construct:
		case MB_BC_complex_deconstruct:
		case MB_BC_place_arg:
		case MB_BC_pickup_arg:
		case MB_BC_call:
		case MB_BC_higher_order_call:
		case MB_BC_builtin_binop:
		case MB_BC_builtin_unop:
		case MB_BC_builtin_bintest:
		case MB_BC_builtin_untest:
		case MB_BC_semidet_succeed:
		case MB_BC_semidet_success_check:
		case MB_BC_fail:
		case MB_BC_context:
		case MB_BC_not_supported:
			break;
		default:
			assert(FALSE);
	}

	if (next_indent < 0) next_indent = 0;
	

	/* if we only wanted to calculate the indents, return now */
	if (buffer == NULL || buffer_len <= 0) return next_indent;

	/* print the indents */
	while (this_indent > 0) {
		PRINT()
			"  "
		ENDPRINT()
		this_indent--;
	}
	
	PRINT()
		"%s",
		str_bytecode_name(bc_id)
	ENDPRINT()

	switch (bc_id) {
		case MB_BC_enter_pred:
			PRINT()
				" %s %s/%d (%d procs)",
				bca->enter_pred.is_func ? "func" : "pred",
				bca->enter_pred.pred_name,
				(int) bca->enter_pred.pred_arity,
				(int) bca->enter_pred.proc_count
			ENDPRINT()
			break;
				
		case MB_BC_endof_pred:
			/* No args */
			buffer[0] = 0;
			break;
			
		case MB_BC_enter_proc: {
			MB_Short	i;
			MB_Short	len;
			
			PRINT()
				" mode %d: [%s] [%d labels] [endlabel %p]"
					" [%d temps] [%d vars]", 
				(int) bca->enter_proc.mode_num,
				str_determinism_name(bca->enter_proc.det),
				bca->enter_proc.label_count,
				bca->enter_proc.end_label.addr,
				bca->enter_proc.temp_count,
				bca->enter_proc.list_length
			ENDPRINT()

			len = bca->enter_proc.list_length;
			for (i = 0; i < len; i++) {
				PRINT()
					" %s",
					bca->enter_proc.var_info[i]

				ENDPRINT()
			}
			break;
		}
		case MB_BC_endof_proc:
			PRINT()
				" (%p)",
				bca->endof_proc.proc_start
			ENDPRINT()
			break;
			
		case MB_BC_label:
			PRINT()
				" %d",
				(int) bca->label.label
			ENDPRINT()
			break;
				
		case MB_BC_enter_disjunction:
			PRINT()
				" [endlabel %p]",
				bca->enter_disjunction.end_label.addr
			ENDPRINT()
			break;
				
		case MB_BC_endof_disjunction:
			/* No args */
			buffer[0] = 0;
			break;
			
		case MB_BC_enter_disjunct:
			PRINT()
				" [nextlabel %p]",
				bca->enter_disjunct.next_label.addr
			ENDPRINT()
			break;
				
		case MB_BC_endof_disjunct:
			PRINT()
				" [endlabel %p]",
				bca->endof_disjunct.end_label.addr
			ENDPRINT()
			break;
				
		case MB_BC_enter_switch:
			PRINT()
				" [var %d] [endlabel %p]",
				(int) bca->enter_switch.var,
				bca->enter_switch.end_label.addr
			ENDPRINT()
			break;
				
		case MB_BC_endof_switch:
			/* No args */
			buffer[0] = 0;
			break;
			
		case MB_BC_enter_switch_arm:
			PRINT()
				" (on var %d) ",
				(int) bca->enter_switch_arm.var
			ENDPRINT()

			PRINTCALL(str_cons_id, bca->enter_switch_arm.cons_id)

			PRINT()
				" [nextlabel %p]",
				bca->enter_switch_arm.next_label.addr
			ENDPRINT()
			break;
				
		case MB_BC_endof_switch_arm:
			PRINT()
				" [endlabel %p]",
				bca->endof_switch_arm.end_label.addr
			ENDPRINT()
			break;
				
		case MB_BC_enter_if:
			PRINT()
				" [else %p] [end %p] [frame %d]",
				bca->enter_if.else_label.addr,
				bca->enter_if.end_label.addr,
				(int) bca->enter_if.frame_ptr_tmp
			ENDPRINT()
			break;
				
		case MB_BC_enter_then:
			PRINT()
				" [frame %d]",
				(int) bca->enter_then.frame_ptr_tmp
			ENDPRINT()
			break;
				
		case MB_BC_endof_then:
			PRINT()
				" [follow %p]",
				bca->endof_then.follow_label.addr
			ENDPRINT()
			break;

		case MB_BC_enter_else:
			PRINT()
				" [frame %d]",
				(int) bca->enter_else.frame_ptr_tmp
			ENDPRINT()
			break;

				
		case MB_BC_endof_if:
			/* No args */
			buffer[0] = 0;
			break;
			
		case MB_BC_enter_negation:
			PRINT()
				" [frame %d] [endlabel %p]",
				(int) bca->enter_negation.frame_ptr_tmp,
				bca->enter_negation.end_label.addr
			ENDPRINT()
			break;

		case MB_BC_endof_negation_goal:
			PRINT()
				" [frame %d]",
				(int) bca->endof_negation_goal.frame_ptr_tmp
			ENDPRINT()
				
		case MB_BC_endof_negation:
			/* No args */
			buffer[0] = 0;
			break;
			
		case MB_BC_enter_commit:
			PRINT()
				" [frame %d]",
				(int) bca->enter_commit.frame_ptr_tmp
			ENDPRINT()
			break;
				
		case MB_BC_endof_commit:
			PRINT()
				" [frame %d]",
				(int) bca->endof_commit.frame_ptr_tmp
			ENDPRINT()
			break;
				
		case MB_BC_assign:
			PRINT()
				" [var %d] <= [var %d]",
				(int) bca->assign.to_var,
				(int) bca->assign.from_var
			ENDPRINT()
			break;
				
		case MB_BC_test:
			PRINT()
				" [var %d] == [var %d] [type %s]",
				(int) bca->test.var1,
				(int) bca->test.var2,
				str_test_id(bca->test.id)
			ENDPRINT()
			break;
				
		case MB_BC_construct: {
			MB_Short	len;
			MB_Short	i;

			PRINT()
				" [var %d] <= ",
				(int) bca->construct.to_var
			ENDPRINT()

			PRINTCALL(str_cons_id,bca->construct.consid)

			len = bca->construct.list_length;
			PRINT()
				" [%d var%s%s",
				(int) len,
				(len != 0) ? "s" : "",
				(len > 0)  ? ":" : ""
			ENDPRINT()
			for (i = 0; i < len; i++) {
				PRINT()
					" %d",
					(int) bca->construct.var_list[i]
				ENDPRINT()
				
			}
			PRINT()
				"]"
			ENDPRINT()
			break;
		}
		case MB_BC_deconstruct: {
			MB_Short	len;
			MB_Short	i;

			PRINT()
				" [var %d] ",
				(int) bca->deconstruct.from_var
			ENDPRINT()
			
			PRINTCALL(str_cons_id,bca->deconstruct.consid)

			len = bca->deconstruct.list_length;
			PRINT()
				" [%d var%s%s",
				(int) len,
				(len != 0) ? "s" : "",
				(len > 0)  ? ":" : ""
			ENDPRINT()

			for (i = 0; i < len; i++) {
				PRINT()
					" %d",
					(int) bca->deconstruct.var_list[i]
				ENDPRINT()
				
			}
			PRINT()
				"]"
			ENDPRINT()
			break;
		}
		case MB_BC_complex_construct: {
			MB_Short	len;
			MB_Short	i;

			PRINT()
				" %d ",
				(int) bca->complex_construct.to_var
			ENDPRINT()

			PRINTCALL(str_cons_id,bca->complex_construct.consid);

			len = bca->complex_construct.list_length;
			
			PRINT()
				" [%d var%s]",
		       		(int) len,
				(len == 1) ? "" : "s"
			ENDPRINT()

			for (i = 0; i < len; i++) {
				PRINT()
					" "
				ENDPRINT()

				PRINTCALL(str_var_dir,
					bca->complex_construct.var_dir[i])
			}
			break;
		}
		case MB_BC_complex_deconstruct: {
			MB_Short	len;
			MB_Short	i;

			PRINT()
				" %d ",
				(int) bca->complex_deconstruct.from_var
			ENDPRINT()

			PRINTCALL(str_cons_id,bca->complex_deconstruct.consid)

			len = bca->complex_deconstruct.list_length;
			PRINT()
				" [%d var%s]",
		       		(int) len,
				(len == 1) ? "" : "s"
			ENDPRINT()

			for (i = 0; i < len; i++) {
				PRINT()
					" "
				ENDPRINT()

				PRINTCALL(str_var_dir,
					bca->complex_deconstruct.var_dir[i])
			}
			break;
		}
		case MB_BC_place_arg:
			PRINT()
				" [r%d] <= [var %d]",
				(int) bca->place_arg.to_reg,
				(int) bca->place_arg.from_var
			ENDPRINT()
			break;
				
		case MB_BC_pickup_arg:
			PRINT()
				" [r%d] => [var %d]",
				(int) bca->pickup_arg.from_reg,
				(int) bca->pickup_arg.to_var
			ENDPRINT()
			break;
				
		case MB_BC_call:
			PRINT()
				" [%s %s__%s/%d mode %d (%s %p)]",
				bca->call.is_func ? "func" : "pred",
				bca->call.module_name,
				bca->call.pred_name,
				(int) bca->call.arity,
				(int) bca->call.mode_num,
				bca->call.addr.is_native ? "natv" : "byte",
				bca->call.addr.is_native ?
				 	(MB_Word *) bca->call.addr.addr.native :
					(MB_Word *) bca->call.addr.addr.bc
			ENDPRINT()
			break;
				
		case MB_BC_higher_order_call:
			PRINT()
				" [var %d] [invars %d] [outvars %d] [%s]",
				(int) bca->higher_order_call.pred_var,
				(int) bca->higher_order_call.in_var_count,
				(int) bca->higher_order_call.out_var_count,
				str_determinism_name(bca->higher_order_call.det)
			ENDPRINT()
			break;
				
		case MB_BC_builtin_binop:
			PRINT()
				": "
			ENDPRINT()

			PRINTCALL(str_op_arg,bca->builtin_binop.arg1)

			PRINT()
				" %s ", 
				str_binop_name(bca->builtin_binop.binop)
			ENDPRINT()

			PRINTCALL(str_op_arg,bca->builtin_binop.arg2)

			PRINT()
				" => [var %d]",
				(int) bca->builtin_binop.to_var
			ENDPRINT()
			break;
				
		case MB_BC_builtin_unop:
			PRINT()
				" %s ", 
				str_unop_name(bca->builtin_unop.unop)
			ENDPRINT()

			PRINTCALL(str_op_arg, bca->builtin_unop.arg)

			PRINT()
				" => [var %d]",
				(int) bca->builtin_unop.to_var
			ENDPRINT()
			break;
				
		case MB_BC_builtin_bintest:

			PRINT()
				" "
			ENDPRINT()

			PRINTCALL(str_op_arg,bca->builtin_binop.arg1)

			PRINT()
				" %s ", 
				str_binop_name(bca->builtin_bintest.binop)
			ENDPRINT()

			PRINTCALL(str_op_arg,bca->builtin_binop.arg2)
			break;
				
		case MB_BC_builtin_untest:
			PRINT()
				" %s ", 
				str_unop_name(bca->builtin_untest.unop)
			ENDPRINT()
			PRINTCALL(str_op_arg,bca->builtin_unop.arg)
			break;
				
		case MB_BC_semidet_succeed:
			/* No args */
			buffer[0] = 0;
			break;
			
		case MB_BC_semidet_success_check:
			/* No args */
			buffer[0] = 0;
			break;
			
		case MB_BC_fail:
			/* No args */
			buffer[0] = 0;
			break;
			
		case MB_BC_context:
			PRINT()
				" %d",
				bca->context.line_number
			ENDPRINT()
			break;
				
		case MB_BC_not_supported:
			/* No args */
			buffer[0] = 0;
			break;

		default:
			MB_fatal("Attempt to disassemble unknown bytecode");
			break;
	} /* end switch */

	return next_indent;

} /* end str_bytecode() */

static void
str_cons_id(MB_Cons_id cons_id, char *buffer, int buffer_len)
{
	PRINT()
		"["
	ENDPRINT()
	switch (cons_id.id) {
		case MB_CONSID_CONS: {
			PRINT()
				"functor %s__%s/%d ",
				cons_id.opt.cons.module_name,
				cons_id.opt.cons.string,
				(int) cons_id.opt.cons.arity
			ENDPRINT()
			PRINTCALL(str_tag, cons_id.opt.cons.tag)
			break;
		}
		case MB_CONSID_INT_CONST:
			PRINT()
				"int_const " MB_FMT_INT " (" MB_FMT_HEX ")",
				cons_id.opt.int_const,
				cons_id.opt.int_const
			ENDPRINT()
			break;
		case MB_CONSID_STRING_CONST: {
			PRINT()
				"string_const"
			ENDPRINT()

			PRINTCALL(quote_cstring, cons_id.opt.string_const);
			break;
		}
		case MB_CONSID_FLOAT_CONST:
			PRINT()
				"float_const %.15g",
				cons_id.opt.float_const
			ENDPRINT()
			break;
		case MB_CONSID_PRED_CONST:
			PRINT()
				"%s %s %s__%s/%d mode %d (%s %p)",
				"pred_const",
				cons_id.opt.pred_const.is_func ?"func":"pred",
				cons_id.opt.pred_const.module_name,
				cons_id.opt.pred_const.pred_name,
				(int) cons_id.opt.pred_const.arity,
				(int) cons_id.opt.pred_const.mode_num,
				"natv",
				(MB_Word *) cons_id.opt.pred_const.native_addr
			ENDPRINT()
			break;
		case MB_CONSID_CODE_ADDR_CONST:
			PRINT()
				"%s %s %s %d %d",
				"code_addr_const",
				cons_id.opt.code_addr_const.module_name,
				cons_id.opt.code_addr_const.pred_name,
				(int) cons_id.opt.code_addr_const.arity,
				(int) cons_id.opt.code_addr_const.mode_num
			ENDPRINT()
			break;
		case MB_CONSID_BASE_TYPE_INFO_CONST:
			PRINT()
				"%s %s__%s/%d",
				"base_type_info_const",
				cons_id.opt.base_type_info_const.module_name,
				cons_id.opt.base_type_info_const.type_name,
				(int) cons_id.opt.base_type_info_const
								.type_arity
			ENDPRINT()
			break;
		case MB_CONSID_CHAR_CONST:
			if (isprint(cons_id.opt.char_const.ch)) {
				PRINT()
					"%s '%c'",
					"char_const",
					cons_id.opt.char_const.ch
				ENDPRINT()
			} else {
				PRINT()
					"%s %d (0x%02X)",
					"char_const",
					(int) cons_id.opt.char_const.ch,
					(int) cons_id.opt.char_const.ch
				ENDPRINT()
			}
			break;
		default:
			MB_fatal("Attempt to disassemble unknown cons");
			break;
	} /* end switch */
	PRINT()
		"]"
	ENDPRINT()
} /* end str_cons_id() */

static void
str_tag(MB_Tag tag, char *buffer, int buffer_len)
{
	
	switch (tag.id) {
		case MB_TAG_SIMPLE:
			MR_snprintf(buffer, buffer_len,
				"%s %d",
				"simple_tag",
				(int) tag.opt.primary);
			break;
		case MB_TAG_COMPLICATED:
			MR_snprintf(buffer, buffer_len,
				"%s %d %ld",
				"complicated_tag", 
				(int) tag.opt.pair.primary, 
				(long int) tag.opt.pair.secondary);
			break;
		case MB_TAG_COMPLICATED_CONSTANT:
			MR_snprintf(buffer, buffer_len,
				"%s %d %ld",
				"complicated_constant_tag", 
				(int) tag.opt.pair.primary, 
				(long int) tag.opt.pair.secondary);
			break;
		case MB_TAG_ENUM:
			MR_snprintf(buffer, buffer_len,
				"%s %d",
				"enum_tag",
				(int) tag.opt.enum_tag);
			break;
		case MB_TAG_NONE:
			MR_snprintf(buffer, buffer_len,
				"%s",
				"no_tag");
			break;
		default:
			MB_util_error("Invalid tag: %d\n", tag.id);
			assert(FALSE); /* XXX */
			break;
	} /* end switch */
} /* end str_tag() */

/*
** XXX ORDER: Currently we depend on the order of elements in the table.
*/
static const char *
bytecode_table[] = {
	"enter_pred",
	"endof_pred",
	"enter_proc",
	"endof_proc",
	"label",
	"enter_disjunction",
	"endof_disjunction",
	"enter_disjunct",
	"endof_disjunct",
	"enter_switch",
	"endof_switch",
	"enter_switch_arm",
	"endof_switch_arm",
	"enter_if",
	"enter_then",
	"endof_then",
	"endof_if",
	"enter_negation",
	"endof_negation",
	"enter_commit",
	"endof_commit",
	"assign",
	"test",
	"construct",
	"deconstruct",
	"complex_construct",
	"complex_deconstruct",
	"place_arg ",
	"pickup_arg",
	"call",
	"higher_order_call",
	"builtin_binop",
	"builtin_unop",
	"builtin_bintest",
	"builtin_untest",
	"semidet_succeed",
	"semidet_success_check",
	"fail",
	"context",
	"not_supported",
	"enter_else",
	"endof_negation_goal"
};

static MB_CString_Const
str_bytecode_name(MB_Byte bytecode_id)
{
	if (bytecode_id >= sizeof(bytecode_table) / sizeof(*bytecode_table)) {
		return (MB_CString_Const) "<<unknown bytecode>>"; /* XXX */
	} else {
		return (MB_CString_Const) bytecode_table[bytecode_id];
	}
}

/*
** XXX ORDER: Currently we depend on the order of elements in the table.
*/
static const char *
test_id_table[] = {
	"int",
	"char",
	"string",
	"float",
	"user"
};

/* Returns a string corresponding to the name of a test type */
static MB_CString_Const
str_test_id(MB_Test_id test_id)
{
	if (test_id >= sizeof(test_id_table) / sizeof(*test_id_table)) {
		return "<<unknown test type";
	} else {
		return (MB_CString_Const) test_id_table[test_id];
	}
}

/*
** XXX ORDER: Currently we depend on the order of elements in the table.
*/
static const char *
determinism_table[] = {
	"det",
	"semidet",
	"multidet",
	"nondet",
	"cc_multidet",
	"cc_nondet",
	"erroneous",
	"failure"
};

/* Returns a string corresponding to the name of a determinism type */
static MB_CString_Const
str_determinism_name(MB_Byte determinism_id)
{
	if (determinism_id >=
		       	sizeof(determinism_table) / sizeof(*determinism_table))
	{
		return (MB_CString_Const) "<<unknown determinism>>"; /* XXX */
	} else {
		return (MB_CString_Const) determinism_table[determinism_id];
	}
}


/*
** XXX ORDER: Currently we depend on the order of elements in the table.
*/
static const char *
unop_table[] = {
	"mktag",
	"tag",
	"unmktag",
	"mkbody",
	"unmkbody",
	"strip_tag",
	"hash_string",
	"bitwise_complement",
	"not"
};

/* Return a string corresponding to the name of a unary operation */
static MB_CString_Const
str_unop_name(MB_Byte unop)
{
	/* bounds check */
	
	if (unop >= sizeof(unop_table) / sizeof(*unop_table)) {
		return (MB_CString_Const) "<<unknown unop>>";	/* XXX */
	} else {
		return (MB_CString_Const) unop_table[unop];
	}
}

/*
** XXX ORDER: Currently we depend on the order of elements in the table.
*/
static const char *
binop_table[] = {
	"+",
	"-",
	"*",
	"/",
	"mod",
	"<<",
	">>",
	"&",
	"|",
	"^",
	"and",
	"or",
	"eq",
	"ne",
	"array_index",
	"str_eq",
	"str_ne",
	"str_lt",
	"str_gt",
	"str_le",
	"str_ge",
	"<",
	">",
	"<=",
	">=",
	"float_plus",
	"float_minus",
	"float_times",
	"float_divide",
	"float_eq",
	"float_ne",
	"float_lt",
	"float_gt",
	"float_le",
	"float_ge",
	"body"
};	/* end binop_table */

static MB_CString_Const
str_binop_name(MB_Byte binop)
{
	/* bounds check */
	if (binop >= sizeof(binop_table) / sizeof(*binop_table)) {
		return (MB_CString_Const) "<<unknown binop>>"; /* XXX */
	} else {
		return (MB_CString_Const) binop_table[binop];
	}
} /* end str_binop_name() */


/* Fills a buffer with a string transformed into a source-style c string 
 * (includes double quotes around string) */
static void
quote_cstring(MB_CString str, char * buffer, int buffer_len)
{
	int		i;	/* index into str */
	int		j;	/* index into buffer */

	i = j = 0;
	if (j < buffer_len) {
		buffer[j] = '"';
		j++;
	}
	while (buffer_len-1 > j) {
		/*
		** According to K&R 2nd ed, page 194, string literals
		** may not contain newline or double-quote; they must be
		** escaped. (And of course the backslash must also be
		** escaped.)  There is no mention of other limitations
		** on what characters may be within a string literal.
		*/
		switch(str[i]) {
			/* following two cases are identical */
			case '\\': 
			case '\"':
				buffer[j] = '\\';
				buffer[j+1] = str[i];
				j += 2;
				i++;
				break;
			case '\n':
				buffer[j] = '\\';
				buffer[j+1] = 'n';
				j += 2;
				i++;
				break;
			case '\0': {
				buffer[j] = '"';
				buffer[j+1] = 0;
				return;
			}
			default:
				buffer[j] = str[i];
				j++;
				i++;
				break;
		} /* end switch */
	} /* end for */
	buffer[buffer_len-1] = 0;
} /* end quote_cstring() */

static void
str_var_dir(MB_Var_dir var_dir, char *buffer, int buffer_len)
{
	PRINT()
		"<<var_dir>>"
	ENDPRINT()
	return;
} /* end str_var_dir() */

static void
str_op_arg(MB_Op_arg op_arg, char *buffer, int buffer_len)
{
	switch (op_arg.id) {
		case MB_ARG_VAR:
			PRINT()
				"[var %d]",
				op_arg.opt.var
			ENDPRINT()
			break;
		case MB_ARG_INT_CONST:
			PRINT()
				"[int " MB_FMT_INT " (" MB_FMT_HEX ")]",
				op_arg.opt.int_const,
				op_arg.opt.int_const
			ENDPRINT()
			break;
		case MB_ARG_FLOAT_CONST:
			PRINT()
				"[float %f]",
				op_arg.opt.float_const
			ENDPRINT()
			break;
		default:
			assert(FALSE); /* XXX */
			break;
	} /* end switch */
} /* end str_op_arg() */

/*
** Displays a code listing from address start to end
** ms may be NULL if desired
*/
void
MB_listing(MB_Machine_State *ms, FILE *fp, MB_Bytecode_Addr start,
		MB_Bytecode_Addr end, MB_Word line_len)
{
	char		buffer[256];
	MB_Word		indent = 0;
	MB_Bytecode_Addr i;
	MB_Bytecode_Addr ip = (ms != NULL) ? MB_ip_get(ms) : NULL;

	start = MB_code_range_clamp(start);
	end = MB_code_range_clamp(end);

	if (sizeof(buffer) < line_len) line_len = sizeof(buffer);

	/*
	** backtrack to the previous predicate 
	** and assume that it is at indent level 0
	*/
	i = MB_code_get_pred_addr(start);

	if (i != MB_CODE_INVALID_ADR) {
		/* work out the indent level at the start */
		while (i != start) {
			indent = MB_str_bytecode(i, NULL, 0, indent);
			i++;
		}
	}

	/* Show the code */
	for (; i != end+1; i++) {
		indent = MB_str_bytecode(i, buffer, line_len, indent);
		fprintf(fp, "%s%p %s\n",
			(i == ip) ? "-> " : "   ",
			i,
			buffer);
	}
}

