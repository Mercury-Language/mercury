/*
** Copyright (C) 1997 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
**
** $Id: disasm.c,v 1.14 1997-07-27 14:59:21 fjh Exp $
*/

/* Imports */

#include	<stdlib.h>
#include	<stdio.h>
#include	<string.h>
#include	<assert.h>

#include	"util.h"
#include	"mem.h"
#include	"bytecode.h"
#include	"disasm.h"

/* Local declarations */

static char
rcs_id[]	= "$Id: disasm.c,v 1.14 1997-07-27 14:59:21 fjh Exp $";

static void
print_bytecode(MB_Bytecode bytecode);

static void
print_cons_id(MB_Cons_id cons_id);

static void
print_tag(MB_Tag tag);

static void
print_var_dir(MB_Var_dir var_dir);

static void
print_op_arg(MB_Op_arg op_arg);

static const char*
bytecode_to_name(MB_Byte bytecode_id);

static const char*
determinism_to_name(MB_Byte determinism_id);

static const char*
binop_to_name(MB_Byte binop);

static const char*
unop_to_name(MB_Byte unop);

static MB_CString
quote_cstring(MB_CString str);

/* Implementation */

/*
** We emulate Zoltan's .bytedebug file format.
*/
void
MB_disassemble(FILE* fp)
{
	/*int		byte_count = 0; */
	MB_Short	bytecode_version_number = 0;
	MB_Bytecode	bytecode;

	/* Read two-byte version number */
	if (MB_read_bytecode_version_number(fp, &bytecode_version_number)) {
		printf("bytecode_version %d\n", bytecode_version_number);
		/* XXX: We should check the version number is 
		** what we expect. Should use major and minor version
		** numbers? Should use a magic number?
		** Should have the module name in the bytecode.
		*/
	} else {
		MB_fatal("Failed to read bytecode version number");
	}

	while (MB_read_bytecode(fp, &bytecode)) {
		print_bytecode(bytecode);
		/* 
		** XXX: should free any heap data in the bytecode here
		** Wait till we know what's happening with GC.
		*/
		fflush(stdout);
	} 
} /* end disassemble() */


static void
print_bytecode(MB_Bytecode bc)
{
	printf("%s", bytecode_to_name(bc.id));

	switch (bc.id) {
		case MB_BC_enter_pred:
			printf(" %s %d", bc.opt.enter_pred.pred_name,
				bc.opt.enter_pred.proc_count);
			break;
		case MB_BC_endof_pred:
			/* No args */
			break;
		case MB_BC_enter_proc: {
			MB_Short	i;
			MB_Short	len;

			printf(" %d %s %d %d %d", 
				bc.opt.enter_proc.proc_id,
				determinism_to_name(bc.opt.enter_proc.det),
				bc.opt.enter_proc.label_count,
				bc.opt.enter_proc.temp_count,
				bc.opt.enter_proc.list_length);
			len = bc.opt.enter_proc.list_length;
			for (i = 0; i < len; i++) {
				printf(" %s",
					bc.opt.enter_proc.
					var_info_list[i]);
			}
			break;
		}
		case MB_BC_endof_proc:
			/* No args */
			break;
		case MB_BC_label:
			printf(" %d", bc.opt.label.label);
			break;
		case MB_BC_enter_disjunction:
			printf(" %d", bc.opt.enter_disjunction.end_label);
			break;
		case MB_BC_endof_disjunction:
			/* No args */
			break;
		case MB_BC_enter_disjunct:
			printf(" %d", bc.opt.enter_disjunct.next_label);
			break;
		case MB_BC_endof_disjunct:
			printf(" %d", bc.opt.endof_disjunct.label);
			break;
		case MB_BC_enter_switch:
			printf(" %d %d", bc.opt.enter_switch.var,
				bc.opt.enter_switch.end_label
			);
			break;
		case MB_BC_endof_switch:
			/* No args */
			break;
		case MB_BC_enter_switch_arm:
			printf(" ");
			print_cons_id(bc.opt.enter_switch_arm.cons_id);
			printf(" %d", bc.opt.enter_switch_arm.next_label);
			break;
		case MB_BC_endof_switch_arm:
			printf(" %d", bc.opt.endof_switch_arm.label);
			break;
		case MB_BC_enter_if:
			printf(" %d %d %d",
				bc.opt.enter_if.else_label,
				bc.opt.enter_if.end_label,
				bc.opt.enter_if.frame_ptr_tmp);
			break;
		case MB_BC_enter_then:
			printf(" %d", bc.opt.enter_then.frame_ptr_tmp);
			break;
		case MB_BC_endof_then:
			printf(" %d", bc.opt.endof_then.follow_label);
			break;
		case MB_BC_endof_if:
			/* No args */
			break;
		case MB_BC_enter_negation:
			printf(" %d", bc.opt.enter_negation.end_label);
			break;
		case MB_BC_endof_negation:
			/* No args */
			break;
		case MB_BC_enter_commit:
			printf(" %d", bc.opt.enter_commit.temp);
			break;
		case MB_BC_endof_commit:
			printf(" %d", bc.opt.endof_commit.temp);
			break;
		case MB_BC_assign:
			printf(" %d %d", bc.opt.assign.to_var,
				bc.opt.assign.from_var);
			break;
		case MB_BC_test:
			printf(" %d %d",
				bc.opt.test.var1,
				bc.opt.test.var2);
			break;
		case MB_BC_construct: {
			MB_Short	len;
			MB_Short	i;

			printf(" %d ", bc.opt.construct.to_var);
			print_cons_id(bc.opt.construct.consid);
			len = bc.opt.construct.list_length;
			printf(" %d", len);
			for (i = 0; i < len; i++) {
				printf(" %d",
					bc.opt.construct.var_list[i]);
				
			}
			break;
		}
		case MB_BC_deconstruct: {
			MB_Short	len;
			MB_Short	i;

			printf(" %d ", bc.opt.deconstruct.from_var);
			print_cons_id(bc.opt.deconstruct.consid);
			len = bc.opt.deconstruct.list_length;
			printf(" %d", len);
			for (i = 0; i < len; i++) {
				printf(" %d",
					bc.opt.deconstruct.var_list[i]);
				
			}
			break;
		}
		case MB_BC_complex_construct: {
			MB_Short	len;
			MB_Short	i;

			printf(" %d ", bc.opt.complex_construct.to_var);
			print_cons_id(bc.opt.complex_construct.consid);
			len = bc.opt.complex_construct.list_length;
			printf(" %d", len);
			for (i = 0; i < len; i++) {
				printf(" ");
				print_var_dir(bc.opt.complex_construct.
					var_dir_list[i]);
			}
			break;
		}
		case MB_BC_complex_deconstruct: {
			MB_Short	len;
			MB_Short	i;

			printf(" %d ", bc.opt.complex_deconstruct.from_var);
			print_cons_id(bc.opt.complex_deconstruct.consid);
			len = bc.opt.complex_deconstruct.list_length;
			printf(" %d", len);
			for (i = 0; i < len; i++) {
				printf(" ");
				print_var_dir(bc.opt.complex_deconstruct.
					var_dir_list[i]);
			}
			break;
		}
		case MB_BC_place_arg:
			printf(" %d %d", bc.opt.place_arg.to_reg,
				bc.opt.place_arg.from_var);
			break;
		case MB_BC_pickup_arg:
			printf(" %d %d", bc.opt.pickup_arg.from_reg,
				bc.opt.pickup_arg.to_var);
			break;
		case MB_BC_call:
			printf(" %s %s %d %d",
				bc.opt.call.module_id,
				bc.opt.call.pred_id,
				bc.opt.call.arity,
				bc.opt.call.proc_id);
			break;
		case MB_BC_higher_order_call:
			printf(" %d %d %d %s",
				bc.opt.higher_order_call.pred_var,
				bc.opt.higher_order_call.in_var_count,
				bc.opt.higher_order_call.out_var_count,
				determinism_to_name(bc.opt.
					higher_order_call.det));
			break;
		case MB_BC_builtin_binop:
			printf(" %s ", 
				binop_to_name(bc.opt.builtin_binop.binop));
			print_op_arg(bc.opt.builtin_binop.arg1);
			printf(" ");
			print_op_arg(bc.opt.builtin_binop.arg2);
			printf(" %d", bc.opt.builtin_binop.to_var);
			break;
		case MB_BC_builtin_unop:
			printf(" %s ", 
				unop_to_name(bc.opt.builtin_unop.unop));
			print_op_arg(bc.opt.builtin_unop.arg);
			printf(" %d", bc.opt.builtin_unop.to_var);
			break;
		case MB_BC_builtin_bintest:
			printf(" %s ", 
				binop_to_name(bc.opt.builtin_bintest.binop));
			print_op_arg(bc.opt.builtin_binop.arg1);
			printf(" ");
			print_op_arg(bc.opt.builtin_binop.arg2);
			break;
		case MB_BC_builtin_untest:
			printf(" %s ", 
				unop_to_name(bc.opt.builtin_untest.unop));
			print_op_arg(bc.opt.builtin_unop.arg);
			break;
		case MB_BC_semidet_succeed:
			/* No args */
			break;
		case MB_BC_semidet_success_check:
			/* No args */
			break;
		case MB_BC_fail:
			/* No args */
			break;
		case MB_BC_context:
			printf(" %d", bc.opt.context.line_number);
			break;
		case MB_BC_not_supported:
			/* No args */
			break;
		case MB_BC_noop:
			/* No args */
			break;
		default:
			assert(FALSE); /*XXX*/
			break;
	} /* end switch */

	putchar('\n');

} /* end print_bytecode() */


static void
print_cons_id(MB_Cons_id cons_id)
{
	switch (cons_id.id) {
		case MB_CONSID_CONS:
			printf("functor %s %s ", cons_id.opt.cons.module_id,
				cons_id.opt.cons.string);
			printf("%d ", cons_id.opt.cons.arity);
			print_tag(cons_id.opt.cons.tag);
			break;
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
			printf("int_const %ld", (long) cons_id.opt.int_const);
			break;
		case MB_CONSID_STRING_CONST: {
			MB_CString s = quote_cstring(cons_id.opt.string_const);
			printf("string_const \"%s\"", s);
			break;
		}
		case MB_CONSID_FLOAT_CONST:
			printf("float_const %.15g", cons_id.opt.float_const);
			break;
		case MB_CONSID_PRED_CONST:
			printf("%s", "pred_const ");
			printf("%s ", cons_id.opt.pred_const.module_id);
			printf("%s ", cons_id.opt.pred_const.pred_id);
			printf("%d ", cons_id.opt.pred_const.arity);
			printf("%d ", cons_id.opt.pred_const.proc_id);
			break;
		case MB_CONSID_CODE_ADDR_CONST:
			printf("%s", "code_addr_const ");
			printf("%s ", cons_id.opt.code_addr_const.module_id);
			printf("%s ", cons_id.opt.code_addr_const.pred_id);
			printf("%d ", cons_id.opt.code_addr_const.arity);
			printf("%d ", cons_id.opt.code_addr_const.proc_id);
			break;
		case MB_CONSID_BASE_TYPE_INFO_CONST:
			printf("%s", "base_type_info_const ");
			printf("%s ", cons_id.opt.base_type_info_const
				.module_id);
			printf("%s ", cons_id.opt.base_type_info_const.
				type_name);
			printf("%d ", cons_id.opt.base_type_info_const.
				type_arity);
			break;
		case MB_CONSID_CHAR_CONST:
			printf("%s", "char_const ");
			/* XXX : fix so that char is printed sensibly */
			printf("'%c'", cons_id.opt.char_const.ch);
			break;
		default:
			assert(FALSE); /*XXX*/
			break;
	} /* end switch */
} /* end print_cons_id() */

static void
print_tag(MB_Tag tag)
{
	switch (tag.id) {
		case MB_TAG_SIMPLE:
			printf("%s %d", "simple_tag", tag.opt.primary);
			break;
		case MB_TAG_COMPLICATED:
			/*
			** See comment labelled "CAST COMMENT".
			*/
			printf("%s %d %ld", "complicated_tag", 
				tag.opt.pair.primary, 
				(long) tag.opt.pair.secondary);
			break;
		case MB_TAG_COMPLICATED_CONSTANT:
			/*
			** See comment labelled "CAST COMMENT".
			*/
			printf("%s %d %ld", "complicated_constant_tag", 
				tag.opt.pair.primary, 
				(long) tag.opt.pair.secondary);
			break;
		case MB_TAG_ENUM:
			printf("%s %d", "enum_tag", tag.opt.enum_tag);
			break;
		case MB_TAG_NONE:
			printf("%s", "no_tag");
			break;
		default:
			fprintf(stderr, "ERROR: invalid tag: %d\n", 
				tag.id);
			assert(FALSE); /*XXX*/
			break;
	} /* end switch */
} /* end print_tag() */

static void
print_var_dir(MB_Var_dir var_dir)
{
	printf("<<var_dir>>"); /* XXX */
	return;
} /* end print_var_dir() */

static void
print_op_arg(MB_Op_arg op_arg)
{
	switch (op_arg.id) {
		case MB_ARG_VAR:
			printf("var %d", op_arg.opt.var);
			break;
		case MB_ARG_INT_CONST:
			/*
			** See comment labelled "CAST COMMENT".
			*/
			printf("int %ld", (long) op_arg.opt.int_const);
			break;
		case MB_ARG_FLOAT_CONST:
			printf("float %f", op_arg.opt.float_const);
			break;
		default:
			assert(FALSE); /*XXX*/
			break;
	} /* end switch */
} /* end print_op_arg() */


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

static const char*
bytecode_to_name(MB_Byte bytecode_id)
{
	if (bytecode_id >= sizeof(bytecode_table) / sizeof(*bytecode_table)) {
		return "<<unknown bytecode>>"; /*XXX*/
	} else {
		return bytecode_table[bytecode_id];
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

/*
** Return a const string
*/
static const char*
determinism_to_name(MB_Byte determinism_id)
{
	if (determinism_id >=
		sizeof(determinism_table) / sizeof(*determinism_table))
	{
		return "<<unknown determinism>>"; /*XXX*/
	} else {
		return determinism_table[determinism_id];
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
	"float_ge"
};

static const char*
binop_to_name(MB_Byte binop)
{
	/* bounds check */
	if (binop >= sizeof(binop_table) / sizeof(*binop_table)) {
		return "<<unknown binop>>"; /*XXX*/
	} else {
		return binop_table[binop];
	}
} /* end binop_to_name() */

/*
**	XXX: Currently we depend on the order of elements in the table.
*/
static const char*
unop_table[] = {
	"mktag",
	"tag",
	"unmktag",
	"mkbody",
	"body",
	"unmkbody",
	"cast_to_unsigned",
	"hash_string",
	"bitwise_complement",
	"not"
};

static const char*
unop_to_name(MB_Byte unop)
{
	/* bounds check */
	if (unop >= sizeof(unop_table) / sizeof(*unop_table)) {
		return "<<unknown unop>>"; /*XXX*/
	} else {
		return unop_table[unop];
	}
} /* end unop_to_name() */


/*
** Given a C string, returns a new string with all the special
** characters escaped.
** Uses a highwater-marked static string buffer:
** the string returned lasts only until the next call to quote_cstring().
** XXX: put in util module?
*/
static MB_CString
quote_cstring(MB_CString str)
{
	static char	*str_s = NULL;
	static int	str_size_s = 1;
	int		i;	/* index into str */
	int		j;	/* index into str_s */

	/* Allocate initial static string */
	if (NULL == str_s) {
		str_s = MB_new_array(char, str_size_s);
	}

	i = j = 0;
	for (;;) {
		/* Check our static string is big enough */
		if (i+2 > str_size_s) {
			str_size_s *= 2; /* Double buffer size */
			str_s = MB_resize_array(str_s, char, str_size_s);
		}

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
				str_s[j] = '\\';
				str_s[j+1] = str[i];
				j += 2;
				i++;
				break;
			case '\n':
				str_s[j] = '\\';
				str_s[j+1] = 'n';
				j += 2;
				i++;
				break;
			case '\0': {
				MB_CString		ret_str;

				str_s[j] = '\0';
				ret_str = MB_new_array(char,
					strlen(str_s) + 1) ;
				strcpy(ret_str, str_s);
				return ret_str;
			}
			default:
				str_s[j] = str[i];
				j++;
				i++;
				break;
		} /* end switch */
	} /* end for */
} /* end quote_cstring() */
