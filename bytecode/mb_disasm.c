
/*
** Copyright (C) 2000 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
**
** $Id: mb_disasm.c,v 1.1 2001-01-24 07:42:23 lpcam Exp $
**
** Contains functions for disassembling bytecodes into human readable form
**
*/

/* Imports */
#include	<assert.h>
#include	<ctype.h>
#include	<string.h>

#include	"mb_disasm.h"
#include	"mb_util.h"

/* Exported definitions */

int MB_str_bytecode(MB_Bytecode bc, char* buffer, int buffer_len, int indent_level);
void MB_listing(MB_Machine_State* ms, FILE* fp, MB_Word start, MB_Word end);

/* Local declarations */

static char
rcs_id[]	= "$Id: mb_disasm.c,v 1.1 2001-01-24 07:42:23 lpcam Exp $";

/* Fills a string buffer with the name of a bytecode */
int MB_str_bytecode(MB_Bytecode bc, char* buffer, int buffer_len, int indent_level);

/* Fills a c string buffer with to the name of a constructor id */
static void str_cons_id(MB_Cons_id cons_id, char* buffer, int buffer_len);

/* Fills a c string buffer with to the name of a tag */
static void str_tag(MB_Tag tag, char* buffer, int buffer_len);

/* Returns a string corresponding to the name of a bytecode type */
static const MB_CString str_bytecode_name(MB_Byte bytecode_id);

/* Returns a string corresponding to the name of a determinism type */
static const MB_CString str_determinism_name(MB_Byte determinism_id);

/* Returns a string corresponding to the name of a unary operation */
static const MB_CString str_unop_name(MB_Byte unop);

/* Returns a string corresponding to the name of a binary operation */
static const MB_CString str_binop_name(MB_Byte binop);

/* Fills a buffer with a string transformed into a source-style c string 
 * (includes double quotes around string) */
static void quote_cstring(MB_CString str, char* buffer, int buffer_len);

/* Fills a c string buffer with a variable list */
static void str_var_dir(MB_Var_dir var_dir, char* buffer, int buffer_len);

/* Fills a c string buffer with an operation argument */
static void str_op_arg(MB_Op_arg op_arg, char* buffer, int buffer_len);

/* Implementation */


/* Macros for printing:
** expects buffer, buffer_len & last_len to be defined
** (My kingdom for a C++ stream!)
** wraps calls to snprintf, checks if the buffer is full and
** if it is, returns from the function
*/

#define Print()		if (buffer_len > 1) { \
				int last_len; \
				assert(buffer_len > 0); \
				last_len = snprintf(buffer, buffer_len,

/* printf arguments get sandwiched between these macros */
#define EndPrint()				);	\
				if (last_len >= buffer_len) {\
					last_len = buffer_len-1; \
				} \
				buffer += last_len;	\
				buffer_len -= last_len; \
				assert(buffer_len > 0); \
			} else { \
				assert(buffer_len >= 0); \
				if (buffer_len == 0) { \
					buffer--; \
					buffer_len++; \
				} \
			}


/* Call this after calling a function that has added characters to the buffer */
/* -> Requires that if the function filled the buffer, it must have at least */
/* put a null terminator at the end */
#define PrintFillCheck() {	\
				int str_len = strlen(buffer);	\
				buffer += str_len;		\
				buffer_len -= str_len;		\
				assert(buffer_len > 0); \
			}

/* Macro to call a function of the format f(arg, buffer, buffer_len)
 * where the function fills all or part of the buffer
 */
#define PrintCall(x, y)	(x)((y), buffer, buffer_len); \
			PrintFillCheck()

/* Fills a string corresponding to a bytecode */
/* returns indent level for subsequent instructions */
/* if buffer is NULL then only returns change in indent level */
int
MB_str_bytecode(MB_Bytecode bc, char* buffer, int buffer_len, int indent_level) 
{
	int this_indent = indent_level;
	int next_indent = indent_level;
	switch (bc.id) {
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

	if (buffer == NULL) return next_indent;

	while (this_indent > 0) {
		Print()
			"   "
		EndPrint()
		this_indent--;
	}
	
	Print()
		"%s",
		str_bytecode_name(bc.id)
	EndPrint()

	switch (bc.id) {
		case MB_BC_enter_pred:
			Print()
				" %s %s/%d (%d procs)",
				bc.opt.enter_pred.is_func ? "func" : "pred",
				bc.opt.enter_pred.pred_name,
				bc.opt.enter_pred.pred_arity,
				bc.opt.enter_pred.proc_count
			EndPrint()
			break;
		case MB_BC_endof_pred:
			/* No args */
			buffer[0] = 0;
			break;
		case MB_BC_enter_proc: {
			MB_Short	i;
			MB_Short	len;
			
			Print()
				" proc %d: %s, %d labels, %d temps, %d vars:", 
				bc.opt.enter_proc.proc_id,
				str_determinism_name(bc.opt.enter_proc.det),
				bc.opt.enter_proc.label_count,
				bc.opt.enter_proc.temp_count,
				bc.opt.enter_proc.list_length
			EndPrint()

			len = bc.opt.enter_proc.list_length;
			for (i = 0; i < len; i++) {
				Print()
					" %s",
					bc.opt.enter_proc.var_info_list[i]
				EndPrint()
			}
			break;
		}
		case MB_BC_endof_proc:
			/* No args */
			break;
		case MB_BC_label:
			Print()
				" %d",
				bc.opt.label.label
			EndPrint()
			break;
		case MB_BC_enter_disjunction:
			Print()
				" %d",
				bc.opt.enter_disjunction.end_label
			EndPrint()
			break;
		case MB_BC_endof_disjunction:
			/* No args */
			buffer[0] = 0;
			break;
		case MB_BC_enter_disjunct:
			Print()
				" %d",
				bc.opt.enter_disjunct.next_label
			EndPrint()
			break;
		case MB_BC_endof_disjunct:
			Print()
				" %d",
				bc.opt.endof_disjunct.label
			EndPrint()
			break;
		case MB_BC_enter_switch:
			Print()
				" on %d, label %d",
				bc.opt.enter_switch.var,
				bc.opt.enter_switch.end_label
			EndPrint()
			break;
		case MB_BC_endof_switch:
			/* No args */
			buffer[0] = 0;
			break;
		case MB_BC_enter_switch_arm:
			Print()
				" "
			EndPrint()

			PrintCall(str_cons_id, bc.opt.enter_switch_arm.cons_id)

			Print()
				" %d",
				bc.opt.enter_switch_arm.next_label
			EndPrint()
			break;
		case MB_BC_endof_switch_arm:
			Print()
				" endlabel %d",
				bc.opt.endof_switch_arm.label
			EndPrint()
			break;
		case MB_BC_enter_if:
			Print()
				" else %d, end %d, frame %d",
				bc.opt.enter_if.else_label,
				bc.opt.enter_if.end_label,
				bc.opt.enter_if.frame_ptr_tmp
			EndPrint()
			break;
		case MB_BC_enter_then:
			Print()
				" %d",
				bc.opt.enter_then.frame_ptr_tmp
			EndPrint()
			break;
		case MB_BC_endof_then:
			Print()
				" %d",
				bc.opt.endof_then.follow_label
			EndPrint()
			break;
		case MB_BC_endof_if:
			/* No args */
			buffer[0] = 0;
			break;
		case MB_BC_enter_negation:
			printf(" %d", bc.opt.enter_negation.end_label);
			break;
		case MB_BC_endof_negation:
			/* No args */
			buffer[0] = 0;
			break;
		case MB_BC_enter_commit:
			Print()
				" %d",
				bc.opt.enter_commit.temp
			EndPrint()
			break;
		case MB_BC_endof_commit:
			Print()
				" %d",
				bc.opt.endof_commit.temp
			EndPrint()
			break;
		case MB_BC_assign:
			Print()
				" %d %d",
				bc.opt.assign.to_var,
				bc.opt.assign.from_var
			EndPrint()
			break;
		case MB_BC_test:
			Print()
				" %d %d",
				bc.opt.test.var1,
				bc.opt.test.var2
			EndPrint()
			break;
		case MB_BC_construct: {
			MB_Short	len;
			MB_Short	i;

			Print()
				" %d ",
				bc.opt.construct.to_var
			EndPrint()

			PrintCall(str_cons_id,bc.opt.construct.consid)

			len = bc.opt.construct.list_length;
			Print()
				" %d",
				len
			EndPrint()
			for (i = 0; i < len; i++) {
				Print()
					" %d",
					bc.opt.construct.var_list[i]
				EndPrint()
				
			}
			break;
		}
		case MB_BC_deconstruct: {
			MB_Short	len;
			MB_Short	i;

			Print()
				" %d ",
				bc.opt.deconstruct.from_var
			EndPrint()
			
			PrintCall(str_cons_id,bc.opt.deconstruct.consid)

			len = bc.opt.deconstruct.list_length;
			Print()
				" %d",
				len
			EndPrint()

			for (i = 0; i < len; i++) {
				Print()
					" %d",
					bc.opt.deconstruct.var_list[i]
				EndPrint()
				
			}
			break;
		}
		case MB_BC_complex_construct: {
			MB_Short	len;
			MB_Short	i;

			Print()
				" %d ",
				bc.opt.complex_construct.to_var
			EndPrint()

			PrintCall(str_cons_id,bc.opt.complex_construct.consid);

			len = bc.opt.complex_construct.list_length;
			
			Print()
				" %d", len
			EndPrint()

			for (i = 0; i < len; i++) {
				Print()
					" "
				EndPrint()

				PrintCall(str_var_dir,
					bc.opt.complex_construct.var_dir_list[i])
			}
			break;
		}
		case MB_BC_complex_deconstruct: {
			MB_Short	len;
			MB_Short	i;

			Print()
				" %d ",
				bc.opt.complex_deconstruct.from_var
			EndPrint()

			PrintCall(str_cons_id,bc.opt.complex_deconstruct.consid);

			len = bc.opt.complex_deconstruct.list_length;
			Print()
				" %d",
				len
			EndPrint()

			for (i = 0; i < len; i++) {
				Print()
					" "
				EndPrint()

				PrintCall(str_var_dir,
					bc.opt.complex_deconstruct.var_dir_list[i])
			}
			break;
		}
		case MB_BC_place_arg:
			Print()
				": r[%d] <= slot %d",
				bc.opt.place_arg.to_reg,
				bc.opt.place_arg.from_var
			EndPrint()
			break;
		case MB_BC_pickup_arg:
			Print()
				": r[%d] => slot %d",
				bc.opt.pickup_arg.from_reg,
				bc.opt.pickup_arg.to_var
			EndPrint()
			break;
		case MB_BC_call:
			Print()
				" %s %s %d %d (%08x)",
				bc.opt.call.module_id,
				bc.opt.call.pred_id,
				bc.opt.call.arity,
				bc.opt.call.proc_id,
				bc.opt.call.adr
			EndPrint()
			break;
		case MB_BC_higher_order_call:
			Print()
				" %d %d %d %s",
				bc.opt.higher_order_call.pred_var,
				bc.opt.higher_order_call.in_var_count,
				bc.opt.higher_order_call.out_var_count,
				str_determinism_name(bc.opt.higher_order_call.det)
			EndPrint()
			break;
		case MB_BC_builtin_binop:
			Print()
				": "
			EndPrint()

			PrintCall(str_op_arg,bc.opt.builtin_binop.arg1)

			Print()
				" %s ", 
				str_binop_name(bc.opt.builtin_binop.binop)
			EndPrint()

			PrintCall(str_op_arg,bc.opt.builtin_binop.arg2)

			Print()
				" => %d",
				bc.opt.builtin_binop.to_var
			EndPrint()
			break;
		case MB_BC_builtin_unop:
			Print()
				" %s ", 
				str_unop_name(bc.opt.builtin_unop.unop)
			EndPrint()

			PrintCall(str_op_arg,bc.opt.builtin_unop.arg)

			Print()
				" %d",
				bc.opt.builtin_unop.to_var
			EndPrint()
			break;
		case MB_BC_builtin_bintest:
			Print()
				" %s ", 
				str_binop_name(bc.opt.builtin_bintest.binop)
			EndPrint()

			PrintCall(str_op_arg,bc.opt.builtin_binop.arg1)

			Print()
				" "
			EndPrint()

			PrintCall(str_op_arg,bc.opt.builtin_binop.arg2)
			break;
		case MB_BC_builtin_untest:
			Print()
				" %s ", 
				str_unop_name(bc.opt.builtin_untest.unop)
			EndPrint()
			PrintCall(str_op_arg,bc.opt.builtin_unop.arg)
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
			Print()
				" %d",
				bc.opt.context.line_number
			EndPrint()
			break;
		case MB_BC_not_supported:
			/* No args */
			buffer[0] = 0;
			break;
		default:
			assert(FALSE); /*XXX*/
			break;
	} /* end switch */

	return next_indent;

} /* end print_bytecode() */

static void
str_cons_id(MB_Cons_id cons_id, char* buffer, int buffer_len)
{
	switch (cons_id.id) {
		case MB_CONSID_CONS: {
			Print()
				"functor %s %s %d ",
				cons_id.opt.cons.module_id,
				cons_id.opt.cons.string,
				cons_id.opt.cons.arity
			EndPrint()
			PrintCall(str_tag, cons_id.opt.cons.tag)
			break;
		}
		case MB_CONSID_INT_CONST:
			/*
			** (This comment is labelled "CAST COMMENT".
			** If you remove this comment, also
			** remove references to it in this file.
			** Search for "CAST COMMENT".)
			**
			** XXX: The cast to `long' in the following code
			** is needed to remove a warning. `int_const' has
			** type `Integer', but Integer may be typedef'ed
			** to `int', `long', `long long' or whatever.
			** The correct solution may be to define a
			** format string for Integer in conf.h.
			*/
			Print()
				"int_const %ld",
				(long) cons_id.opt.int_const
			EndPrint()
			break;
		case MB_CONSID_STRING_CONST: {
			Print()
				"string_const "
			EndPrint()
			buffer[buffer_len-1] = 0; /* snprintf may not do it */

			PrintCall(quote_cstring, cons_id.opt.string_const);
			break;
		}
		case MB_CONSID_FLOAT_CONST:
			Print()
				"float_const %.15g",
				cons_id.opt.float_const
			EndPrint()
			break;
		case MB_CONSID_PRED_CONST:
			Print()
				"%s %s %s %d %d",
				"pred_const ",
				cons_id.opt.pred_const.module_id,
				cons_id.opt.pred_const.pred_id,
				cons_id.opt.pred_const.arity,
				cons_id.opt.pred_const.proc_id
			EndPrint()
			break;
		case MB_CONSID_CODE_ADDR_CONST:
			Print()
				"%s %s %s %d %d",
				"code_addr_const",
				cons_id.opt.code_addr_const.module_id,
				cons_id.opt.code_addr_const.pred_id,
				cons_id.opt.code_addr_const.arity,
				cons_id.opt.code_addr_const.proc_id
			EndPrint()
			break;
		case MB_CONSID_BASE_TYPE_INFO_CONST:
			Print()
				"%s %s %s %d",
				"base_type_info_const ",
				cons_id.opt.base_type_info_const.module_id,
				cons_id.opt.base_type_info_const.type_name,
				cons_id.opt.base_type_info_const.type_arity
			EndPrint()
			break;
		case MB_CONSID_CHAR_CONST:
			if (isprint(cons_id.opt.char_const.ch)) {
				Print()
					"%s '%c'",
					"char_const ",
					cons_id.opt.char_const.ch
				EndPrint()
			} else {
				Print()
					"%s %2X",
					"char_const ",
					(int)cons_id.opt.char_const.ch
				EndPrint()
			}
			break;
		default:
			assert(FALSE); /*XXX*/
			break;
	} /* end switch */

	buffer[buffer_len-1] = 0; /* snprintf may not do it if a long string */
} /* end print_cons_id() */

static void
str_tag(MB_Tag tag, char* buffer, int buffer_len)
{
	
	switch (tag.id) {
		case MB_TAG_SIMPLE:
			snprintf(buffer, buffer_len,
				"%s %d", "simple_tag", tag.opt.primary);
			break;
		case MB_TAG_COMPLICATED:
			/*
			** See comment labelled "CAST COMMENT".
			*/
			snprintf(buffer, buffer_len,
				"%s %d %ld", "complicated_tag", 
				tag.opt.pair.primary, 
				(long) tag.opt.pair.secondary);
			break;
		case MB_TAG_COMPLICATED_CONSTANT:
			/*
			** See comment labelled "CAST COMMENT".
			*/
			snprintf(buffer, buffer_len,
				"%s %d %ld", "complicated_constant_tag", 
				tag.opt.pair.primary, 
				(long) tag.opt.pair.secondary);
			break;
		case MB_TAG_ENUM:
			snprintf(buffer, buffer_len,
				"%s %d", "enum_tag", tag.opt.enum_tag);
			break;
		case MB_TAG_NONE:
			snprintf(buffer, buffer_len,
				"%s", "no_tag");
			break;
		default:
			MB_util_error("Invalid tag: %d\n", tag.id);
			assert(FALSE); /*XXX*/
			break;
	} /* end switch */
	
	/* snprintf may not append a null character */
	buffer[buffer_len-1] = 0;
} /* end print_tag() */

/*
**	XXX: Currently we depend on the order of elements in the table.
*/
static const char*
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
	"endof_then",	/* XXX: change to enter_else */
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
	"place_arg",
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
	"not_supported"
};

static const MB_CString
str_bytecode_name(MB_Byte bytecode_id)
{
	if (bytecode_id >= sizeof(bytecode_table) / sizeof(*bytecode_table)) {
		return (const MB_CString)"<<unknown bytecode>>"; /*XXX*/
	} else {
		return (const MB_CString)bytecode_table[bytecode_id];
	}
}

/*
**	XXX: Currently we depend on the order of elements in the table.
*/
static const char*
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
static const MB_CString
str_determinism_name(MB_Byte determinism_id)
{
	if (determinism_id >=
		sizeof(determinism_table) / sizeof(*determinism_table))
	{
		return (const MB_CString)"<<unknown determinism>>"; /*XXX*/
	} else {
		return (const MB_CString)determinism_table[determinism_id];
	}
}


/*
**	XXX: Currently we depend on the order of elements in the table.
*/
static const char*
unop_table[] = {
	"mktag",
	"tag",
	"unmktag",
	"mkbody",
	"unmkbody",
	"cast_to_unsigned",
	"hash_string",
	"bitwise_complement",
	"not"
};

/* Return a string corresponding to the name of a unary operation */
static const MB_CString
str_unop_name(MB_Byte unop)
{
	/* bounds check */
	
	if (unop >= sizeof(unop_table) / sizeof(*unop_table)) {
		return (const MB_CString)"<<unknown unop>>";	/* XXX */
	} else {
		return (const MB_CString)unop_table[unop];
	}
}

/*
**	XXX: Currently we depend on the order of elements in the table.
*/
static const char*
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
};

static const MB_CString
str_binop_name(MB_Byte binop)
{
	/* bounds check */
	if (binop >= sizeof(binop_table) / sizeof(*binop_table)) {
		return (const MB_CString)"<<unknown binop>>"; /*XXX*/
	} else {
		return (const MB_CString)binop_table[binop];
	}
} /* end str_binop_name() */


/* Fills a buffer with a string transformed into a source-style c string 
 * (includes double quotes around string) */
static void
quote_cstring(MB_CString str, char* buffer, int buffer_len)
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
str_var_dir(MB_Var_dir var_dir, char* buffer, int buffer_len)
{
	Print()
		"<<var_dir>>"
	EndPrint()
	return;
} /* end str_var_dir() */

static void
str_op_arg(MB_Op_arg op_arg, char* buffer, int buffer_len)
{
	switch (op_arg.id) {
		case MB_ARG_VAR:
			Print()
				"var %d",
				op_arg.opt.var
			EndPrint()
			break;
		case MB_ARG_INT_CONST:
			/*
			** See comment labelled "CAST COMMENT".
			*/
			Print()
				"int %ld",
				(long) op_arg.opt.int_const
			EndPrint()
			break;
		case MB_ARG_FLOAT_CONST:
			Print()
				"float %f",
				op_arg.opt.float_const
			EndPrint()
			break;
		default:
			assert(FALSE); /*XXX*/
			break;
	} /* end switch */
} /* end str_op_arg() */

/* displays a code listing from address start to end
*/

void
MB_listing(MB_Machine_State* ms, FILE* fp, MB_Word start, MB_Word end)
{
	char buffer[73];
	MB_Word i;
	MB_Word	indent = 0;
	MB_Word ip = MB_ip_get(ms);

	/* backtrack to the previous predicate */
	/* and assume that it is at indent level 0 */
	i = MB_code_get_pred_adr(ms, start);

	/* work out the indent level at the start */
	while (i != start) {
		indent = MB_str_bytecode(MB_code_get(ms, i), NULL, 0, indent);
		i++;
	}

	/* Show the code */
	for (; i != end+1; i++) {
		if (i < 0 || i >= MB_code_size(ms)) {
			fprintf(fp, "   %04x (????)\n", i & 0xffff);
		} else {
			indent = MB_str_bytecode(MB_code_get(ms, i),
					buffer, sizeof(buffer), indent);
			fprintf(fp, "%s%04x %s\n",
				(i == ip) ? "-> " : "   ",
				i,
				buffer);
		}
	}
}



