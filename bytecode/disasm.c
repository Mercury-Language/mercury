/*
 *	$Id: disasm.c,v 1.5 1997-02-01 13:36:01 aet Exp $
 *
 *	Copyright: The University of Melbourne, 1996
 */

/* Imports */

#include	<stdlib.h>
#include	<stdio.h>
#include	<assert.h>

#include	<util.h>
#include	<bytecode.h>
#include	<disasm.h>

/* Local declarations */

static void
print_bytecode(Bytecode bytecode);

static void
print_cons_id(Cons_id cons_id);

static void
print_tag(Tag tag);

static void
print_var_dir(Var_dir var_dir);

static void
print_op_arg(Op_arg op_arg);

static CString
bytecode_to_name(Byte bytecode_id);

static CString
determinism_to_name(Byte determinism_id);

static CString
binop_to_name(Byte binop);

static CString
unop_to_name(Byte unop);

static CString
quote_cstring(CString str);

/* Implementation */

/*
 * We emulate Zoltan's .bytedebug file format.
 */
void
disassemble(FILE* fp)
{
	/*int		byte_count = 0; */
	short		bytecode_version_number = 0;
	Bytecode	bytecode;

	/* Read two-byte version number */
	if (read_bytecode_version_number(fp, &bytecode_version_number))
	{
		printf("%d\n", bytecode_version_number);
		/* XXX: We should check the version number is 
		 * what we expect. Should use major and minor version
		 * numbers? Should use a magic number?
		 */
	}
	else
	{
		fatal("Failed to read bytecode version number");
	}

	while (read_bytecode(fp, &bytecode))
	{
		print_bytecode(bytecode);
	} 
} /* disassemble */


static void
print_bytecode(Bytecode bc)
{
	/* XXX: nuke the extraneous printed shit */
	printf("%s", bytecode_to_name(bc.id));

	switch (bc.id)
	{
	case BC_enter_pred:
		printf(" %s %d", bc.opt.enter_pred.pred_name,
			bc.opt.enter_pred.proc_count);
		break;
	case BC_endof_pred:
		{
			/* No args */
		}
		break;
	case BC_enter_proc:
		{
		int	i;
		short	len;

		printf(" %d %s %d %d %d", 
			bc.opt.enter_proc.proc_id,
			determinism_to_name(bc.opt.enter_proc.det),
			bc.opt.enter_proc.label_count,
			bc.opt.enter_proc.temp_count,
			bc.opt.enter_proc.list_length);
		len = bc.opt.enter_proc.list_length;
		for (i=0; i < len; i++)
		{
			printf(" %s",
				bc.opt.enter_proc.
				var_info_list[i]);
		}
		}
		break;
	case BC_endof_proc:
		{
			/* No args */
		}
		break;
	case BC_label:
		{
			printf(" %d", bc.opt.label.label);
		}
		break;
	case BC_enter_disjunction:
		{
			printf(" %d", bc.opt.enter_disjunction.end_label);
		}
		break;
	case BC_endof_disjunction:
		{
			/* No args */
		}
		break;
	case BC_enter_disjunct:
		{
			printf(" %d", bc.opt.enter_disjunct.next_label);
		}
		break;
	case BC_endof_disjunct:
		{
			printf(" %d", bc.opt.endof_disjunct.label);
		}
		break;
	case BC_enter_switch:
		{
			printf(" %d %d", bc.opt.enter_switch.var,
				bc.opt.enter_switch.end_label
			);
		}
		break;
	case BC_endof_switch:
		{
			/* No args */
		}
		break;
	case BC_enter_switch_arm:
		{
			printf(" ");
			print_cons_id(bc.opt.enter_switch_arm.cons_id);
			printf(" %d", bc.opt.enter_switch_arm.next_label);
		}
		break;
	case BC_endof_switch_arm:
		{
			printf(" %d", bc.opt.endof_switch_arm.label);
		}
		break;
	case BC_enter_if:
		{
			printf(" %d %d %d",
				bc.opt.enter_if.else_label,
				bc.opt.enter_if.end_label,
				bc.opt.enter_if.frame_ptr_tmp);
		}
		break;
	case BC_enter_then:
		{
			printf(" %d", bc.opt.enter_then.frame_ptr_tmp);
		}
		break;
	case BC_endof_then:
		{
			printf(" %d", bc.opt.endof_then.follow_label);
		}
		break;
	case BC_endof_if:
		{
			/* No args */
		}
		break;
	case BC_enter_negation:
		{
			printf(" %d", bc.opt.enter_negation.end_label);
		}
		break;
	case BC_endof_negation:
		{
			/* No args */
		}
		break;
	case BC_enter_commit:
		{
			printf(" %d", bc.opt.enter_commit.temp);
		}
		break;
	case BC_endof_commit:
		{
			printf(" %d", bc.opt.endof_commit.temp);
		}
		break;
	case BC_assign:
		{
			printf(" %d %d", bc.opt.assign.to_var,
				bc.opt.assign.from_var);
		}
		break;
	case BC_test:
		{
			printf(" %d %d",
				bc.opt.test.var1,
				bc.opt.test.var2);
		}
		break;
	case BC_construct:
		{
			short	len;
			short	i;

			printf(" %d ", bc.opt.construct.to_var);
			print_cons_id(bc.opt.construct.consid);
			len = bc.opt.construct.list_length;
			printf(" %d", len);
			for (i=0; i < len; i++)
			{
				printf(" %d",
					bc.opt.construct.var_list[i]);
				
			}
		}
		break;
	case BC_deconstruct:
		{
			short	len;
			short	i;

			printf(" %d ", bc.opt.deconstruct.from_var);
			print_cons_id(bc.opt.deconstruct.consid);
			len = bc.opt.deconstruct.list_length;
			printf(" %d", len);
			for (i=0; i < len; i++)
			{
				printf(" %d",
					bc.opt.deconstruct.var_list[i]);
				
			}
		}
		break;
	case BC_complex_construct:
		{
			short	len;
			short	i;

			printf(" %d ", bc.opt.complex_construct.to_var);
			print_cons_id(bc.opt.complex_construct.consid);
			len = bc.opt.complex_construct.list_length;
			printf(" %d", len);
			for (i=0; i < len; i++)
			{
				printf(" ");
				print_var_dir(bc.opt.complex_construct.
					var_dir_list[i]);
			}
		}
		break;
	case BC_complex_deconstruct:
		{
			short	len;
			short	i;

			printf(" %d ", bc.opt.complex_deconstruct.from_var);
			print_cons_id(bc.opt.complex_deconstruct.consid);
			len = bc.opt.complex_deconstruct.list_length;
			printf(" %d", len);
			for (i=0; i < len; i++)
			{
				printf(" ");
				print_var_dir(bc.opt.complex_deconstruct.
					var_dir_list[i]);
			}
		}
		break;
	case BC_place_arg:
		{
			printf(" %d %d", bc.opt.place_arg.to_reg,
				bc.opt.place_arg.from_var);
		}
		break;
	case BC_pickup_arg:
		{
			printf(" %d %d", bc.opt.pickup_arg.from_reg,
				bc.opt.pickup_arg.to_var);
		}
		break;
	case BC_call:
		{
			printf(" %s %s %d %d",
				bc.opt.call.module_id,
				bc.opt.call.pred_id,
				bc.opt.call.arity,
				bc.opt.call.proc_id);
		}
		break;
	case BC_higher_order_call:
		{
			printf(" %d %d %d %s",
				bc.opt.higher_order_call.pred_var,
				bc.opt.higher_order_call.in_var_count,
				bc.opt.higher_order_call.out_var_count,
				determinism_to_name(bc.opt.
					higher_order_call.det));
		}
		break;
	case BC_builtin_binop:
		{
			printf(" %s ", 
				binop_to_name(bc.opt.builtin_binop.binop));
			print_op_arg(bc.opt.builtin_binop.arg1);
			printf(" ");
			print_op_arg(bc.opt.builtin_binop.arg2);
			printf(" %d", bc.opt.builtin_binop.to_var);
		}
		break;
	case BC_builtin_unop:
		{
			printf(" %s ", 
				unop_to_name(bc.opt.builtin_unop.unop));
			print_op_arg(bc.opt.builtin_unop.arg);
			printf(" %d", bc.opt.builtin_unop.to_var);
		}
		break;
	case BC_builtin_bintest:
		{
			printf(" %s ", 
				binop_to_name(bc.opt.builtin_bintest.binop));
			print_op_arg(bc.opt.builtin_binop.arg1);
			printf(" ");
			print_op_arg(bc.opt.builtin_binop.arg2);
		}
		break;
	case BC_builtin_untest:
		{
			printf(" %s ", 
				unop_to_name(bc.opt.builtin_untest.unop));
			print_op_arg(bc.opt.builtin_unop.arg);
		}
		break;
	case BC_semidet_succeed:
		{
			/* No args */
		}
		break;
	case BC_semidet_success_check:
		{
			/* No args */
		}
		break;
	case BC_fail:
		{
			/* No args */
		}
		break;
	case BC_context:
		{
			printf(" %d", bc.opt.context.line_number);
		}
		break;
	case BC_not_supported:
		{
			/* No args */
		}
		break;
	default:
		{
			assert(FALSE); /*XXX*/
		}
		break;
	} /* switch */

	putchar('\n');

	return;
} /* print_bytecode */


static void
print_cons_id(Cons_id cons_id)
{
	switch (cons_id.id)
	{
	case CONSID_CONS:
		{
			printf("functor %s ", cons_id.opt.cons.string);
			printf("%d ", cons_id.opt.cons.arity);
			print_tag(cons_id.opt.cons.tag);
		}
		break;
	case CONSID_INT_CONST:
		{
			printf("int_const %d", cons_id.opt.int_const);
		}
		break;
	case CONSID_STRING_CONST:
		{
			/* 
			 * XXX: This string printing is shitful.
			 * Should quote newlines, backslashes, etc.
			 */
			printf("string_const \"%s\"", cons_id.opt.string_const);
		}
		break;
	case CONSID_FLOAT_CONST:
		{
			/* XXX: What's Float defined to be??? */
			printf("float_const %f", cons_id.opt.float_const);
		}
		break;
	case CONSID_PRED_CONST:
		{
			printf("%s", "pred_const ");
			printf("%s ", cons_id.opt.pred_const.module_id);
			printf("%s ", cons_id.opt.pred_const.pred_id);
			printf("%d ", cons_id.opt.pred_const.arity);
			printf("%d ", cons_id.opt.pred_const.proc_id);
		}
		break;
	case CONSID_CODE_ADDR_CONST:
		{
			printf("%s", "code_addr_const ");
			printf("%s ", cons_id.opt.code_addr_const.module_id);
			printf("%s ", cons_id.opt.code_addr_const.pred_id);
			printf("%d ", cons_id.opt.code_addr_const.arity);
			printf("%d ", cons_id.opt.code_addr_const.proc_id);
		}
		break;
	case CONSID_BASE_TYPE_INFO_CONST:
		{
			printf("%s", "base_type_info_const ");
			printf("%s ", cons_id.opt.base_type_info_const
				.module_id);
			printf("%s ", cons_id.opt.base_type_info_const.
				type_name);
			printf("%d ", cons_id.opt.base_type_info_const.
				type_arity);
		}
		break;
	default:
		{
			assert(FALSE); /*XXX*/
		}
		break;
	} /* switch */
	return;
} /* print_cons_id */

static void
print_tag(Tag tag)
{
	switch (tag.id)
	{
	case SIMPLE_TAG:
		{
			printf("%s %d", "simple_tag", tag.opt.primary);
		}
		break;
	case COMPLICATED_TAG:
		{
			printf("%s %d %d", "complicated_tag", 
				tag.opt.pair.primary, 
				tag.opt.pair.secondary);
		}
		break;
	case COMPLICATED_CONSTANT_TAG:
		{
			printf("%s %d %d", "complicated_constant_tag", 
				tag.opt.pair.primary, 
				tag.opt.pair.secondary);
		}
		break;
	case ENUM_TAG:
		{
			printf("%s %d", "enum_tag", tag.opt.enum_tag);
		}
		break;
	case NO_TAG:
		{
			printf("%s", "no_tag");
		}
		break;
	default:
		{
			fprintf(stderr, "ERROR: invalid tag: %d\n", 
				tag.id);
			assert(FALSE); /*XXX*/
		}
		break;
	} /* switch */
	return;
} /* print_tag */

static void
print_var_dir(Var_dir var_dir)
{
	printf("<<var_dir>>"); /* XXX */
	return;
} /* print_var_dir */

static void
print_op_arg(Op_arg op_arg)
{
	switch (op_arg.id)
	{
	case ARG_VAR:
		{
			printf("var %d", op_arg.opt.var);
		}
		break;
	case ARG_INT_CONST:
		{
			printf("int %d", op_arg.opt.int_const);
		}
		break;
	case ARG_FLOAT_CONST:
		{
			printf("float %f", op_arg.opt.float_const);
		}
		break;
	default:
		{
			assert(FALSE); /*XXX*/
		}
		break;
	} /* switch */

	return;
} /* print_op_arg */


static CString
bytecode_table[] =
	{
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

static CString
bytecode_to_name(Byte bytecode_id)
{
	if (bytecode_id >= sizeof(bytecode_table) / sizeof(CString))
	{
		return "<<unknown bytecode>>"; /*XXX*/
	}
	else
	{
		return bytecode_table[bytecode_id];
	}
}

/*
 *	XXX: Currently we depend on the order of elements in the table.
 */
static CString
determinism_table[] =
	{
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
 * Return a const string
 */
static CString
determinism_to_name(Byte determinism_id)
{
	if (determinism_id >= sizeof(determinism_table) / sizeof(CString))
	{
		return "<<unknown determinism>>"; /*XXX*/
	}
	else
	{
		return determinism_table[determinism_id];
	}
}

/*
 *	XXX: Currently we depend on the order of elements in the table.
 */
static CString
binop_table[] = {
	"+",
	"-",
	"*",
	"/",
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

static CString
binop_to_name(Byte binop)
{
	/* bounds check */
	if (binop >= sizeof(binop_table) / sizeof(CString))
	{
		return "<<unknown binop>>"; /*XXX*/
	}
	else
	{
		return binop_table[binop];
	}
} /* binop_to_name */

/*
 *	XXX: Currently we depend on the order of elements in the table.
 */
static CString
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

static CString
unop_to_name(Byte unop)
{
	/* bounds check */
	if (unop >= sizeof(unop_table) / sizeof(CString))
	{
		return "<<unknown unop>>"; /*XXX*/
	}
	else
	{
		return unop_table[unop];
	}
} /* unop_to_name */


/*
 * Return a quoted C String.
 * XXX: put in util module?
 */
static CString
quote_cstring(CString str)
{
	/* XXX: Stubbed out for now. */
	return str;
}
