/*
** Copyright (C) 2000-2001 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
**
*/

#include "mb_module.h"

#include <string.h>
#include "mb_interface.h"
#include "mb_mem.h"
#include "mb_stack.h"

/* XXX: We should remove these fixed limits */
#define MAX_CODE_COUNT		10000
#define MAX_CODE_DATA_COUNT	160000
#define MAX_MODULES		64

/*
** File version (simple check for correct bytecode file format)
** Should be the same as that in bytecode.m
*/
#define FILEVERSION	9

/* Exported definitions */


/* Local declarations */

/*
** The bytecodes consist of a sequence of words with each containing the
** bytecode and an index into the code_arg_data array, the data at that
** index being the argument for the bytecode
**
** The bytecode id is just a byte with the uppermost bit being used to
** indicate whether the code executing is in a nondet or det procedure
** (this is needed to work out which stack the var list is on)
**
** If you change this, ensure the bytecodes in mb_bytecode.h will
** still fit in the number of bits allocated to an id
**
** XXX: Can only handle 64MB of bytecode data
*/

#define MB_BCID_MAKE(dest, new_id, new_arg) \
		((dest).id = (new_id), \
		(dest).is_det = 0, \
		(dest).arg = (((MB_Word *) (new_arg) - code_arg_data)), \
		(dest))

#define MB_BCID_ID(x)		((x).id)

#define MB_BCID_ISDET		1

#define MB_BCID_DET_GET(x)	((x).is_det)
#define MB_BCID_DET_SET(x, det)	((x).is_det = (det))

#define MB_BCID_ARG(x)		((MB_Bytecode_Arg *) (code_arg_data + (x).arg))

/* XXX: not thread safe */
static MB_Word code_count = 0;
static MB_BCId code_id[MAX_CODE_COUNT];

#define CODE_DATA_NONE	0	/* 0 is reserved for indicating no data */
static MB_Word code_data_count = 1;
static MB_Word code_arg_data[MAX_CODE_DATA_COUNT];

struct MB_Module_Struct {
	/* XXX: Hash the module & predicate names */
	/* The name of the module */
	MB_CString	module_name;

	/*
	** The following should not be directly accessed unless
	** absolutely necessary; use one of the (many) wrapper functions
	*/

	/*
	** The code indices of all the predicates in this module
	** If this is empty, then it means we tried to load
	** the module but we couldn't find bytecodes for it
	*/
	/* XXX: This really should be hashed too */
	MB_Stack	pred_index_stack;

};

/* XXX: The current accesses to these variables are thread safe */
static MB_Word module_count = 0;
static MB_Module *module_arr[MAX_MODULES];

static MB_Bool	translate_calls(MB_Bytecode_Addr bc, MB_Unsigned number_codes);
static MB_Bool	translate_labels(MB_Bytecode_Addr bc, MB_Unsigned number_codes,
					MB_Stack *label_stack);
static MB_Bool	translate_detism(MB_Bytecode_Addr bc, MB_Unsigned number_codes);
static MB_Bool	translate_switch(MB_Bytecode_Addr bc, MB_Unsigned number_codes);
static MB_Bool	translate_vars(MB_Bytecode_Addr bc, MB_Unsigned number_codes);

/* Implementation */

/*
** Translates calls from a predicate name/procedure to an actual code address
** Translates call & higher_order(pred_const) bytecodes
** Returns TRUE if successful
*/
static MB_Bool
translate_calls(MB_Bytecode_Addr bc, MB_Unsigned number_codes)
{
	/*
	** XXX: We should temporarily table the procs, instead of re-searching
	** each time, but since there is usually only one proc per predicate,
	** don't bother for now
	*/

	MB_Unsigned i;
	for (i = 0; i < number_codes; i++, bc++) {
		/* proc to be called attributes */
		MB_CString	module_name = NULL;
		MB_CString	pred_name = NULL;
		MB_Word		arity;
		MB_Bool		is_func;
		MB_Word		mode_num;
		/* location to store the proc to be called */
		MB_Code_Addr	*target_addr = NULL;
		MB_Native_Addr	*target_native = NULL;

		/* Get the information about the procedure to call */
		MB_Byte		call_id = MB_code_get_id(bc);
		if (call_id == MB_BC_call) {
			MB_Bytecode_Arg *call_arg = MB_code_get_arg(bc);
			module_name	= call_arg->call.module_name;
			arity		= call_arg->call.arity;
			is_func 	= call_arg->call.is_func;
			pred_name	= call_arg->call.pred_name;
			mode_num	= call_arg->call.mode_num;
			target_addr	= &call_arg->call.addr;
			target_native	= NULL;

		} else if (call_id == MB_BC_construct) {
			MB_Bytecode_Arg *construct_arg =
				MB_code_get_arg(bc);
			if (construct_arg->construct.consid.id ==
					MB_CONSID_PRED_CONST)
			{
				module_name = construct_arg->construct.
					consid.opt.pred_const.module_name;
				arity = construct_arg->construct.
					consid.opt.pred_const.arity;
				is_func = construct_arg->construct.
					consid.opt.pred_const.is_func;
				pred_name = construct_arg->construct.
					consid.opt.pred_const.pred_name;
				mode_num = construct_arg->construct.
					consid.opt.pred_const.mode_num;
				target_addr = NULL;
				target_native = &construct_arg->construct.
					consid.opt.pred_const.native_addr;
			}
		}


		/* Find the predicate start */
		if (pred_name != NULL) {
			/* First check if we can find it in the bytecode */
			MB_Bytecode_Addr bc_addr =
				MB_code_find_proc(module_name, pred_name,
						mode_num, arity, is_func);

			if (bc_addr == MB_CODE_INVALID_ADR) {
				/* Otherwise look in the native code */
				MB_Native_Addr native_addr =
					MB_code_find_proc_native(module_name,
					pred_name, mode_num, arity, is_func);

				if (native_addr == NULL) {
					/*
					MB_util_error(
						"Warning: Proc ref in bytecode"
						" at %08x to unknown"
						" (will evaluate lazily)\n"
						"  Unknown: %s"
							" %s__%s/%d mode %d\n"
						"  Are you sure the module"
						" was compiled with trace"
						" information enabled?\n",
						(int) i,
						is_func ? "func" : "pred",
						module_name,
						pred_name,
						(int) arity,
						(int) mode_num
						);
					*/
				}
				if (target_addr != NULL) {
					target_addr->is_native = TRUE;
					target_addr->addr.native = native_addr;
				}
				if (target_native != NULL) {
					*target_native = native_addr;
				}
			} else {
				if (target_addr != NULL) {
					target_addr->is_native = FALSE;
					target_addr->addr.bc = bc_addr;
				}
				if (target_native != NULL) {
					*target_native = NULL;
				}
			}
		}
	}

	return TRUE;
} /* translate_calls */

/*
** Translates labels to code addresses for those instructions that need it
** those translated are:
** enter_if, endof_then, enter_disjunction, enter_disjunct, endof_disjunct
** enter_switch, enter_switch_arm, endof_switch_arm, enter_negation, enter_proc
** Returns TRUE if successful
*/

/* Helper function for translate_labels: translates an invididual label */
static void translate_label(MB_Bytecode_Arg* cur_proc_arg,
		MB_Stack* label_stack, MB_Label* label)
{
	if (label->index < cur_proc_arg->enter_proc.label_count
		&& label->index > 0) {
		label->addr = (MB_Bytecode_Addr) MB_stack_peek(label_stack,
			cur_proc_arg->enter_proc.label_index + label->index);
	} else {
		label->addr = MB_CODE_INVALID_ADR;
	}
}

static MB_Bool
translate_labels(MB_Bytecode_Addr bc, MB_Unsigned number_codes,
		MB_Stack *label_stack)
{
	MB_Unsigned i;
	MB_Bytecode_Arg *cur_proc_arg = NULL;

	for (i = 0; i < number_codes; i++, bc++) {
		MB_Bytecode_Arg *cur_arg =
			MB_code_get_arg(bc);

		switch (MB_code_get_id(bc)) {

			case MB_BC_enter_proc:
				cur_proc_arg = cur_arg;
				translate_label(cur_proc_arg, label_stack,
					&cur_arg->enter_proc.end_label);
				break;
				
			case MB_BC_enter_if:
				translate_label(cur_proc_arg, label_stack,
					&cur_arg->enter_if.else_label);
				translate_label(cur_proc_arg, label_stack,
					&cur_arg->enter_if.end_label);
				break;
					     
			case MB_BC_endof_then:
				translate_label(cur_proc_arg, label_stack,
					&cur_arg->endof_then.follow_label);
				break;

			case MB_BC_enter_disjunction:
				translate_label(cur_proc_arg, label_stack,
					&cur_arg->enter_disjunction.end_label);
				break;

			case MB_BC_enter_disjunct:
				translate_label(cur_proc_arg, label_stack,
					&cur_arg->enter_disjunct.next_label);
				break;

			case MB_BC_endof_disjunct:
				translate_label(cur_proc_arg, label_stack,
					&cur_arg->endof_disjunct.end_label);
				break;

			case MB_BC_enter_switch:
				translate_label(cur_proc_arg, label_stack,
					&cur_arg->enter_switch.end_label);
				break;

			case MB_BC_enter_switch_arm:
				translate_label(cur_proc_arg, label_stack,
					&cur_arg->enter_switch_arm.next_label);
				break;

			case MB_BC_endof_switch_arm:
				translate_label(cur_proc_arg, label_stack,
					&cur_arg->endof_switch_arm.end_label);
				break;

			case MB_BC_enter_negation:
				translate_label(cur_proc_arg, label_stack,
					&cur_arg->enter_negation.end_label);
				break;
		}
	}
	return TRUE;
} /* translate_labels */


/*
** Store the procedure's determinism that each instruction is executing under
** This is used when returning into a procedure to decide whether the
** vars & temps are on the det or nondet stack
** Returns TRUE if successful
*/
static MB_Bool
translate_detism(MB_Bytecode_Addr bc, MB_Unsigned number_codes)
{
	MB_Unsigned i;
	MB_Byte bc_id;
	MB_Byte cur_detism = MB_BCID_ISDET;
	
	for (i = 0; i < number_codes; i++, bc++) {
		bc_id = MB_code_get_id(bc);
		if (bc_id == MB_BC_enter_proc) {
			switch (MB_code_get_arg(bc)->enter_proc.det) {
				case MB_DET_FAILURE:
				case MB_DET_CC_NONDET:
				case MB_DET_SEMIDET:
				case MB_DET_CC_MULTIDET:
				case MB_DET_DET:
					cur_detism = MB_BCID_ISDET;
					break;
				case MB_DET_MULTIDET:
				case MB_DET_NONDET:
					cur_detism = 0;
					break;
				case MB_DET_UNUSABLE:
					cur_detism = 0;
					break;
				default:
					assert(FALSE);
			}
		}
		if (cur_detism) {
			MB_BCID_DET_SET(*bc, cur_detism);
		}

		if (bc_id == MB_BC_endof_proc) cur_detism = 0;
	}
	return TRUE;
} /* translate_detism */


/*
** Fill in the variable that each switch arm is using
** Returns TRUE if successful
**
** XXX: Can only handle a fixed number of nested switched
*/
static MB_Bool
translate_switch(MB_Bytecode_Addr bc, MB_Unsigned number_codes)
{
	#define MAXNESTEDSWITCH	32
	MB_Unsigned i;
	/* Leave the first switch as NULL to trap any errors */
	MB_Word cur_switch = 0;
	MB_Bytecode_Arg *switch_ptr[MAXNESTEDSWITCH] = { NULL };
	for (i = 0; i < number_codes; i++, bc++) {
		switch (MB_code_get_id(bc)) {
			case MB_BC_enter_switch:
				cur_switch++;
				if (cur_switch >= MAXNESTEDSWITCH) {
					MB_fatal("Too many nested switches");
				}
				switch_ptr[cur_switch] = MB_code_get_arg(bc);
				break;
				
			case MB_BC_enter_switch_arm: {
				MB_Bytecode_Arg *cur_arg
					= MB_code_get_arg(bc);
				
				cur_arg->enter_switch_arm.var = 
					switch_ptr[cur_switch]
						->enter_switch.var;
				
				break;
			}

			case MB_BC_endof_switch: {
				cur_switch--;
				assert(cur_switch >= 0);
				break;
			}
		}
	}
	return TRUE;
} /* translate_switch */

/*
** Transform variable numbers.
** See mb_interface.h for the det stack layout.
** Since there is no distinction between vars and temps once loaded (they all
** use MB_var_[get/set], the var numbers must be incremented by the number
** of temps
**
** Note that translate_switch must already have been called to fill in
** missing values in enter_switch_arm
**
** Returns TRUE if successful
*/

static MB_Bool
translate_vars(MB_Bytecode_Addr bc, MB_Unsigned number_codes)
{
	MB_Unsigned j;
	MB_Unsigned temp_count = 0;
	MB_Bytecode_Arg *cur_arg ;
	MB_Word code_size = MB_code_size();

	for (j = 0; j < number_codes; j++, bc++) {
		switch (MB_code_get_id(bc)) {
			case MB_BC_enter_proc:
				temp_count = MB_code_get_arg(bc)->
						enter_proc.temp_count;
				break;

			case MB_BC_enter_switch:
				cur_arg = MB_code_get_arg(bc);
				cur_arg->enter_switch.var += temp_count;
				break;

			case MB_BC_enter_switch_arm:
				cur_arg = MB_code_get_arg(bc);
				cur_arg->enter_switch_arm.var += temp_count;
				break;
				
			case MB_BC_assign:
				cur_arg = MB_code_get_arg(bc);
				cur_arg->assign.to_var += temp_count;
				cur_arg->assign.from_var += temp_count;
				break;
				
			case MB_BC_test:
				cur_arg = MB_code_get_arg(bc);
				cur_arg->test.var1 += temp_count;
				cur_arg->test.var2 += temp_count;
				break;

			case MB_BC_construct: {
				MB_Unsigned i;
				cur_arg = MB_code_get_arg(bc);
				cur_arg->construct.to_var += temp_count;
				for (i = 0;
					i < cur_arg->construct.list_length;
					i++)
				{
					cur_arg->construct.var_list[i] +=
						temp_count;
				}

				break;
			}

			case MB_BC_deconstruct: {
				MB_Unsigned i;
				cur_arg = MB_code_get_arg(bc);
				cur_arg->deconstruct.from_var += temp_count;
				for (i = 0;
					i < cur_arg->deconstruct.list_length;
					i++)
				{
					cur_arg->deconstruct.var_list[i] +=
						temp_count;
				}
				break;
			}

			case MB_BC_complex_construct: {
				MB_Unsigned i;
				cur_arg = MB_code_get_arg(bc);
				cur_arg->complex_construct.to_var += temp_count;
				for (i = 0;
					i < cur_arg->complex_construct
							.list_length;
					i++)
				{
					cur_arg->complex_construct.var_dir[i]
						.var += temp_count;
				}
				break;
			}

			case MB_BC_complex_deconstruct: {
				MB_Unsigned i;
				cur_arg = MB_code_get_arg(bc);
				cur_arg->complex_deconstruct.from_var +=
					temp_count;
				for (i = 0;
					i < cur_arg->complex_deconstruct
							.list_length;
					i++)
				{
					cur_arg->complex_deconstruct.var_dir[i]
						.var += temp_count;
				}
				break;
			}

			case MB_BC_place_arg:
				cur_arg = MB_code_get_arg(bc);
				cur_arg->place_arg.from_var += temp_count;
				break;

			case MB_BC_pickup_arg:
				cur_arg = MB_code_get_arg(bc);
				cur_arg->pickup_arg.to_var += temp_count;
				break;

			/* XXX: HIGHER This should not need to be here */
			case MB_BC_higher_order_call:
				cur_arg = MB_code_get_arg(bc);
				cur_arg->higher_order_call.pred_var +=
					temp_count;
				break;

			#define TRANSLATE_OPARG(oparg) \
				if ((oparg).id == MB_ARG_VAR) { \
					(oparg).opt.var += temp_count; \
				}

			case MB_BC_builtin_binop:
				cur_arg = MB_code_get_arg(bc);
				TRANSLATE_OPARG(cur_arg->builtin_binop.arg1);
				TRANSLATE_OPARG(cur_arg->builtin_binop.arg2);
				cur_arg->builtin_binop.to_var += temp_count;
				break;

			case MB_BC_builtin_unop:
				cur_arg = MB_code_get_arg(bc);
				TRANSLATE_OPARG(cur_arg->builtin_unop.arg);
				cur_arg->builtin_unop.to_var += temp_count;
				break;
				
			case MB_BC_builtin_bintest:
				cur_arg = MB_code_get_arg(bc);
				TRANSLATE_OPARG(cur_arg->builtin_bintest.arg1);
				TRANSLATE_OPARG(cur_arg->builtin_bintest.arg2);
				break;

			case MB_BC_builtin_untest:
				cur_arg = MB_code_get_arg(bc);
				TRANSLATE_OPARG(cur_arg->builtin_untest.arg);
				break;
				
		}
	}
	return TRUE;
}

/*
** Load a module by name. Assumes the bytecode file is just the module name
** with '.mbc' appended.
*/
MB_Module *
MB_module_load_name(MB_CString_Const module_name)
{
	MB_Module	*module;
	MB_CString	filename;
	FILE	 	*fp;
	char		*src;
	char		*dst;
	
	/* Turn the : and __ into . for the file name*/
	filename  = MB_str_new_cat(module_name, ".mbc");
	src = filename;
	dst = filename;
	do {
		if (*src == ':') {
			*dst = '.';
		} else if (src[0] == '_' && src[1] == '_') {
			src ++;
			*dst = '.';
		} else {
			*dst = *src;
		}
		dst++;
		src++;
	} while (*src);
	*dst = *src;
	
	fp = fopen(filename, "rb");

	/* Turn the dots back into colons for the module name */
	src = filename;
	do {
		if (*src == '.') {
			*src = ':';
		}
		src++;
	} while (*src);

	module = MB_module_load(module_name, fp);

	MB_str_delete(filename);
	return module;
} /* MB_module_load_name */


/*
** Gets a module. Loads the module if it is not already loaded.
** If there is no bytecode information for this module, returns NULL
*/
MB_Module *
MB_module_get(MB_CString_Const module_name)
{
	/* Search for the module */
	MB_Word i;

	for (i = 0; i < module_count; i++) {
		if (!MB_str_cmp(module_name, module_arr[i]->module_name)) {
			return module_arr[i];
		}
	}

	/* We didn't find it so load it */
	return MB_module_load_name(module_name);
} /* MB_module_get */


#define MB_ARGSIZE_WORDS(name)	\
	MB_NUMBLOCKS(sizeof(((MB_Bytecode *)NULL)->opt.name), sizeof(MB_Word))

/* XXX ORDER */
/* the size of the arguments in a MB_Bytecode struct, in number of MB_Words */
static const MB_Word argument_size[] = {
	MB_ARGSIZE_WORDS(enter_pred),
	MB_ARGSIZE_WORDS(endof_pred),
	MB_ARGSIZE_WORDS(enter_proc),
	MB_ARGSIZE_WORDS(endof_proc),
	MB_ARGSIZE_WORDS(label),
	MB_ARGSIZE_WORDS(enter_disjunction),
	MB_ARGSIZE_WORDS(endof_disjunction),
	MB_ARGSIZE_WORDS(enter_disjunct),
	MB_ARGSIZE_WORDS(endof_disjunct),
	MB_ARGSIZE_WORDS(enter_switch),
	MB_ARGSIZE_WORDS(endof_switch),
	MB_ARGSIZE_WORDS(enter_switch_arm),
	MB_ARGSIZE_WORDS(endof_switch_arm),
	MB_ARGSIZE_WORDS(enter_if),
	MB_ARGSIZE_WORDS(enter_then),
	MB_ARGSIZE_WORDS(endof_then),
	MB_ARGSIZE_WORDS(endof_if),
	MB_ARGSIZE_WORDS(enter_negation),
	MB_ARGSIZE_WORDS(endof_negation),
	MB_ARGSIZE_WORDS(enter_commit),
	MB_ARGSIZE_WORDS(endof_commit),
	MB_ARGSIZE_WORDS(assign),
	MB_ARGSIZE_WORDS(test),
	MB_ARGSIZE_WORDS(construct),
	MB_ARGSIZE_WORDS(deconstruct),
	MB_ARGSIZE_WORDS(complex_construct),
	MB_ARGSIZE_WORDS(complex_deconstruct),
	MB_ARGSIZE_WORDS(place_arg),
	MB_ARGSIZE_WORDS(pickup_arg),
	MB_ARGSIZE_WORDS(call),
	MB_ARGSIZE_WORDS(higher_order_call),
	MB_ARGSIZE_WORDS(builtin_binop),
	MB_ARGSIZE_WORDS(builtin_unop),
	MB_ARGSIZE_WORDS(builtin_bintest),
	MB_ARGSIZE_WORDS(builtin_untest),
	MB_ARGSIZE_WORDS(semidet_succeed),
	MB_ARGSIZE_WORDS(semidet_success_check),
	MB_ARGSIZE_WORDS(fail),
	MB_ARGSIZE_WORDS(context),
	MB_ARGSIZE_WORDS(not_supported),
	MB_ARGSIZE_WORDS(enter_else),
	MB_ARGSIZE_WORDS(endof_negation_goal)
}; /* argument_size */

/*
** Load a module
** If fp is NULL then that means there is no bytecode information
** for this module -- revert to native code, and mark the module
** as native code only
*/
MB_Module *MB_module_load(MB_CString_Const module_name, FILE *fp)
{
	MB_Short	version;
	MB_Word		module_code_count = 0;
	MB_Bytecode_Addr module_start = code_id + code_count;

	/* Array of indexes for label translation (used only during load) */
	MB_Stack label_stack	= MB_stack_new(128, FALSE);

	/* Create the new module */
	MB_Module *module	= MB_GC_NEW(MB_Module);

	module->pred_index_stack= MB_stack_new((fp == NULL) ? 0 : 64, FALSE);
	module->module_name	= MB_str_dup(module_name);

	/* XXX Adding to the array like this is not thread safe */
	if (module_count >= MAX_MODULES) {
		MB_fatal("Too many modules");
	}
	module_arr[module_count++] = module;

	if (fp == NULL) return module;

	/* Check the file version is ok */
	if (!MB_read_bytecode_version_number(fp, &version)) {
		MB_util_error("Unable to read version number\n");
		return NULL;
	}

	if (version != FILEVERSION) {
		MB_util_error("Unknown file format version\n");
		return NULL;
	}

	{
	MB_Bytecode	bc;
	MB_Bytecode_Arg	*cur_proc_arg = NULL;
	MB_Bytecode_Addr cur_proc = MB_CODE_INVALID_ADR;

	/* read in each bytecode */
	while (MB_read_bytecode(fp, &bc)) {
		MB_Bytecode_Arg *cur_arg;

		if (bc.id == MB_BC_label) {
			/*
			** XXX: we strictly don't actually need to save the
			** labels but it makes label translations a lot faster.
			** After translation, the label stack is deleted
			*/
			if (cur_proc_arg == NULL) {
				MB_fatal("Label outside proc\n");
			}

			/* Add the label to the current proc's list of labels */
			MB_stack_poke(&label_stack,
					cur_proc_arg->enter_proc.label_index
						+ bc.opt.label.label,
					(MB_Word)(code_id + code_count));
		} else if (bc.id == MB_BC_not_supported) {
			/*
			** We came across unsupported code. Mark this proc as
			** unusable
			*/
			
			if (cur_proc_arg == NULL) {
				MB_fatal("Code outside proc\n");
			}

			cur_proc_arg->enter_proc.det = MB_DET_UNUSABLE;
		}

		/*
		** Copy the bytecode arguments into the code.data
		** structure, save the index & increment code.data
		** counters
		*/
		if (bc.id < sizeof(argument_size)/sizeof(argument_size[0])) {
			if (argument_size[bc.id] == 0) {
				/* If bytecode has no arguments, skip alloc */
				cur_arg = NULL;
			} else {
				/* Allocate space for bytecode's arguments */
				cur_arg = MB_CODE_DATA_ALLOC(MB_Bytecode_Arg,
							argument_size[bc.id]);

				/* Copy arguments onto argument data stack */
				memcpy(cur_arg,
					&(bc.opt),
					argument_size[bc.id]*sizeof(MB_Word));

				/* Check if we just entered/exited a procedure*/
				switch (bc.id) {
				case MB_BC_enter_proc:
					/*
					** Save the new current proc (so
					** labels know where they are)
					*/
					cur_proc = code_id + code_count;
					cur_proc_arg = cur_arg;

					/*
					** and mark where the label indexes
					** will begin
					*/
					cur_proc_arg->enter_proc.label_index =
						MB_stack_size(&label_stack);

					MB_stack_alloc(&label_stack,
						cur_proc_arg->
							enter_proc.label_count);
					break;
					
				case MB_BC_endof_proc: {
					/* Save the proc we were in */
					cur_arg->endof_proc.proc_start =
						cur_proc;

					cur_proc_arg = NULL;
					break;
				}

				case MB_BC_enter_pred:
					MB_stack_push(&module->pred_index_stack,
						code_count);
					break;
				}
			}

			/* Write bytecode id & argument index */
			MB_BCID_MAKE(code_id[code_count], bc.id, cur_arg);
		} else {
			MB_util_error("Unknown op code");
			MB_module_unload(module);
			MB_stack_delete(&label_stack);
			return NULL;
		}
		code_count++;
		module_code_count++;
	}

	}
	
	if (feof(fp) &&
		(module_code_count > 0) &&
		(translate_labels(module_start, module_code_count,
				  		&label_stack)) &&
		(translate_calls(module_start, module_code_count)) &&
		(translate_detism(module_start, module_code_count)) &&
		(translate_switch(module_start, module_code_count)) &&
		(translate_vars(module_start, module_code_count)))
	{
		/* Delete the label stack (we've done all the translations) */
		MB_stack_delete(&label_stack);

		return module;
	} else {
		MB_fatal("Error reading bytecode file");
	}
	return NULL;
} /* MB_module_load */


/*
** Free memory associated with module structure itself
** (does not unload bytecodes from code array, since other
** modules may have been loaded on top of this one) 
**
** XXX: Should add code to unload all modules and reload
** only the ones needed, thus effectively unloading a
** given module
*/
void
MB_module_unload(MB_Module *module)
{
	if (module != NULL) {
		/*
		** The stacks will always be allocated since it will
		** have aborted if their allocation failed
		*/
		MB_str_delete(module->module_name);
		MB_stack_delete(&module->pred_index_stack);
		MB_GC_free(module);
	}
}

/* Get the actual size of a program, in bytecodes */
MB_Unsigned
MB_code_size(void)
{
	return code_count;
}

/* Get the bytecode type at a given address */
MB_Byte
MB_code_get_id(MB_Bytecode_Addr addr)
{
	if (!MB_ip_normal(addr))
		return MB_BC_debug_invalid;
	
	/* return the code with the determinism flag stripped away */
	return MB_BCID_ID(*addr);
}

/* Get a bytecode's procedure's determinism */
MB_Byte
MB_code_get_det(MB_Bytecode_Addr addr)
{
	assert(MB_ip_normal(addr));
	
	/* return the determinism flag */
	return (MB_BCID_DET_GET(*addr) == MB_BCID_ISDET)
		? MB_ISDET_YES : MB_ISDET_NO;
}

/* Get the bytecode argument at a given address */
MB_Bytecode_Arg *
MB_code_get_arg(MB_Bytecode_Addr addr)
{
	MB_Bytecode_Arg *data_p;

	if (!MB_ip_normal(addr)) return NULL;

	data_p = MB_BCID_ARG(*addr);
	if (data_p == (MB_Bytecode_Arg *)code_arg_data) {
		return NULL;
	} else {
		return data_p;
	}
} /* MB_code_get_arg */

MB_Bytecode_Addr
MB_code_get_pred_addr(MB_Bytecode_Addr addr) {

	while (MB_code_get_id(addr) != MB_BC_enter_pred) {

		addr--;
		if (!MB_ip_normal(addr)) {
			return MB_CODE_INVALID_ADR;
		}
	}

	return addr;
}

MB_Bytecode_Addr
MB_code_get_proc_addr(MB_Bytecode_Addr addr)
{
	MB_Byte bc_id;
	addr++;
	do {
		addr--;
		assert(MB_ip_normal(addr));
		bc_id = MB_code_get_id(addr);
		assert(bc_id != MB_BC_enter_pred);
		assert(bc_id != MB_BC_endof_pred);
	} while (bc_id != MB_BC_enter_proc);
	
	return addr;
} /* MB_code_get_proc_addr */

/* Finds the location of a given proc */
MB_Bytecode_Addr
MB_code_find_proc(MB_CString_Const module_name,
		MB_CString_Const pred_name, MB_Word mode_num,
		MB_Word arity, MB_Bool is_func)
{
	MB_Bytecode_Addr addr;
	MB_Word size;
	MB_Module *module = MB_module_get(module_name);
	MB_Word j;

	if (MB_stack_size(&module->pred_index_stack) == 0) {
		return MB_CODE_INVALID_ADR;
	}
	
	size = MB_stack_size(&module->pred_index_stack);
	for (j = 0; j < size; j++) {
		MB_Bytecode_Arg *pred_arg;
		addr = code_id + MB_stack_peek(&module->pred_index_stack, j);
	
		pred_arg = MB_code_get_arg(addr);

		if ((pred_arg->enter_pred.pred_arity
				== arity)
			&& (pred_arg->enter_pred.is_func
				== is_func)
			&& MB_str_cmp(pred_arg->
					enter_pred.pred_name,
				pred_name) == 0)
		{
			break;
		}
	}

	/* Check if any of the predicates matched */
	if (j == MB_stack_size(&module->pred_index_stack)) {
		return MB_CODE_INVALID_ADR;
	}

	/* one obviously did */
	/* Now find the right proc */
	do {
		MB_Byte bc_id;

		addr++;

		assert(MB_ip_normal(addr));

		bc_id = MB_code_get_id(addr);
		if (bc_id == MB_BC_enter_proc) {
			MB_Bytecode_Arg *proc_arg = MB_code_get_arg(addr);
			if (proc_arg->enter_proc.mode_num == mode_num &&
				proc_arg->enter_proc.det != MB_DET_UNUSABLE)
			{
				return addr;
			}

			/* Check if we've got to the end of this pred */
		} else if ((bc_id == MB_BC_endof_pred) ||
				(bc_id == MB_BC_enter_pred))
		{
			return MB_CODE_INVALID_ADR;
		}
 
	} while (1);

	return MB_CODE_INVALID_ADR;
}



MB_Word *
MB_code_data_alloc_words(MB_Word num_words)
{
	code_data_count += num_words;
	if (code_data_count >= MAX_CODE_DATA_COUNT) {
		MB_fatal("Out of bytecode argument data space");
	}
	return code_arg_data + code_data_count - num_words;
}

/* given a code address, forces it into a valid range */
MB_Bytecode_Addr
MB_code_range_clamp(MB_Bytecode_Addr addr)
{
	MB_Bytecode_Addr max_addr;
	if ((MB_Unsigned) addr < (MB_Unsigned) code_id) return code_id;

	max_addr = code_id + code_count - 1;
	if ((MB_Unsigned) addr > (MB_Unsigned) max_addr) return max_addr;

	return addr;
}

/*
** Returns true if a given instruction pointer points to a normal
** address (ie: valid range and not one of MB_CODE_xxxx)
*/
MB_Bool
MB_ip_normal(MB_Bytecode_Addr ip)
{
	/* XXX pointer comparison; assume cast to unsigned will work */
	return (((MB_Unsigned) ip >= (MB_Unsigned) code_id) &&
		((MB_Unsigned) ip < (MB_Unsigned) (code_id + MAX_CODE_COUNT)));
}

/*
** Returns true if a given instruction pointer is a 'special'
** address (ie: one of the MB_CODE_xxxx macros)
*/
MB_Bool
MB_ip_special(MB_Bytecode_Addr ip)
{
	return ((MB_Unsigned) ip > (MB_Unsigned) MB_CODE_INVALID_ADR);
}


