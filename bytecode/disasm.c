/*
 *	$Id: disasm.c,v 1.3 1997-01-31 04:23:23 aet Exp $
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

/* Implementation */

/* XXX: disassemble is still a stub */
void
disassemble(FILE* fp)
{
	/*int		byte_count = 0; */
	ushort		bytecode_version_number = 0;
	Bytecode	bytecode;

	/* Read two-byte version number */
	if (read_bytecode_version_number(fp, &bytecode_version_number))
	{
/* XXX: Emulate Zoltan's disassembler */
#if 0
		printf("Bytecode Version number: %d\n",
			bytecode_version_number
		);
#endif	/* 0 */
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
		fflush(stdout);
	} 
} /* disassemble */


static void
print_bytecode(Bytecode bc)
{
	/* XXX: nuke the extraneous printed shit */
	printf("%s", bytecode_to_name(bc.bc));

	switch (bc.bc)
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
		ushort	len;

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
			ushort	len;
			ushort	i;

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
			ushort	len;
			ushort	i;

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
			ushort	len;
			ushort	i;

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
			ushort	len;
			ushort	i;

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
	switch (cons_id.cons_id_type)
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
			printf("%d", cons_id.opt.int_const);
		}
		break;
	case CONSID_STRING_CONST:
		{
			printf("\"%s\"", cons_id.opt.string_const);
		}
		break;
	case CONSID_FLOAT_CONST:
		{
			/* XXX: What's Float defined to be??? */
			printf("%f", cons_id.opt.float_const);
		}
		break;
	case CONSID_PRED_CONST:
		{
			printf("%s ", cons_id.opt.pred_const.module_id);
			printf("%s ", cons_id.opt.pred_const.pred_id);
			printf("%d ", cons_id.opt.pred_const.arity);
			printf("%d ", cons_id.opt.pred_const.proc_id);
		}
		break;
	case CONSID_CODE_ADDR_CONST:
		{
			printf("%s ", cons_id.opt.code_addr_const.module_id);
			printf("%s ", cons_id.opt.code_addr_const.pred_id);
			printf("%d ", cons_id.opt.code_addr_const.arity);
			printf("%d ", cons_id.opt.code_addr_const.proc_id);
		}
		break;
	case CONSID_BASE_TYPE_INFO_CONST:
		{
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
	switch (tag.tag_type)
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
				tag.tag_type);
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
	switch (op_arg.arg_type)
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

