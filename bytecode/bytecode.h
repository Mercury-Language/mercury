/*
** Copyright (C) 1997 University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
**
** $Id: bytecode.h,v 1.10 1997-04-26 03:16:04 fjh Exp $
*/

#ifndef BYTECODE_H
#define	BYTECODE_H

#include	"conf.h"
#include	"mercury_types.h"
#include	"mercury_float.h"
#include	"gc.h"

/*
 * XXX: We should make bytecode portable from platform to platform.
 *
 * We require the following:
 *	sizeof(Byte) = 1
 *	sizeof(Short16) = 2 (2's complement)
 *	sizeof(Integer64) = 8 (2's complement)
 *	sizeof(Word64) = 8
 *	sizeof(Float64) = 8 (IEEE)	// We don't really need float.
 * Each of the above is big-endian in the bytecode file.
 * That is, we read the byte at the big end first.
 *
 * We should have platform-dependent #defines to ensure that each of
 * these types has identical size on all platforms.
 */

typedef unsigned char
	Byte;

typedef short
	Short;



/*
XXX: Do we need this?
typedef Integer64
	Word64;
*/

typedef char*
	CString;

typedef struct Tag {
	Byte	id;
	union {
		Byte	primary;
		struct {
			Byte	primary;
			Word	secondary; 
		} pair;
		Byte	enum_tag;
	} opt;
} Tag;


/* 
 *	Possible values for Tag.id ...
 */
#define	TAG_SIMPLE			0
#define	TAG_COMPLICATED			1
#define	TAG_COMPLICATED_CONSTANT	2
#define	TAG_ENUM			3
#define	TAG_NONE			4


typedef Byte
	Determinism;
/*
 *	Possible values for Determinism ...
 */
#define	DET_DET			0
#define	DET_SEMIDET		1
#define	DET_MULTIDET		2
#define	DET_NONDET		3
#define	DET_CC_MULTIDET		4
#define	DET_CC_NONDET		5
#define	DET_ERRONEOUS		6
#define	DET_FAILURE		7

typedef struct Op_arg {
	Byte	id;
	union {
		Short	var;
		Integer int_const;
		Float	float_const;
	} opt;
} Op_arg;

/*
 *	Possible values for Op_arg.id
 */
#define	ARG_VAR			0
#define	ARG_INT_CONST		1
#define	ARG_FLOAT_CONST		2


typedef Byte
	Direction;

typedef struct Var_dir {
	Short		var;
	Direction	dir;
} Var_dir;

/*
 *	Possible values for Direction ...
 */
#define	DIR_TO_ARG		0
#define	DIR_TO_VAR		1
#define	DIR_TO_NONE		2


typedef struct Cons_id {
	Byte	id;
	union {
		struct {
			CString		module_id;
			CString		string;
			Short		arity;
			Tag		tag;
		} cons;
		Integer		int_const;	
		CString		string_const;
		Float		float_const;
		struct {
			CString		module_id;
			CString		pred_id;
			Short		arity;
			Byte		proc_id;
		} pred_const;
		struct {
			CString		module_id;
			CString		pred_id;
			Short		arity;
			Byte		proc_id;
		} code_addr_const;
		struct {
			CString		module_id;
			CString		type_name;
			Byte		type_arity;
		} base_type_info_const;
		struct {
			Byte		ch;
		} char_const;
	} opt;
} Cons_id;

/*
 *	Possible values for Cons_id.id ...
 */
#define	CONSID_CONS			0
#define	CONSID_INT_CONST		1
#define	CONSID_STRING_CONST		2
#define	CONSID_FLOAT_CONST		3
#define	CONSID_PRED_CONST		4
#define	CONSID_CODE_ADDR_CONST		5
#define	CONSID_BASE_TYPE_INFO_CONST	6
#define	CONSID_CHAR_CONST		7


typedef struct Bytecode {
	Byte	id;	/* Which bytecode instruction. e.g. BC_fail */
	union {
		struct {
			CString		pred_name;	/* XXX: malloc */
			Short		proc_count;
		} enter_pred;

		/* endof_pred */

		struct {
			Byte		proc_id;
			Determinism	det;
			Short		label_count;
			Short		temp_count;
			Short		list_length;
			CString		*var_info_list; /* XXX: malloc */
		} enter_proc;

		struct {
			Short		label;
		} label;

		struct {
			Short		end_label;
		} enter_disjunction;

		/* endof_disjunction */

		struct {
			Short		next_label;
		} enter_disjunct;

		struct {
			Short		label; /* XXX: what's label for? */
		} endof_disjunct;

		struct {
			Short		var;
			Short		end_label;
		} enter_switch;
			
		/* endof_switch */

		struct {
			Cons_id		cons_id;
			Short		next_label;
		} enter_switch_arm;

		struct {
			Short	label;	/* XXX: what's this label for? */
		} endof_switch_arm;

		struct {
			Short		else_label;
			Short		end_label;
			Short		frame_ptr_tmp;
		} enter_if;

		struct {
			Short		frame_ptr_tmp;
		} enter_then;
		
		struct {
			Short		follow_label;
		} endof_then;	/* XXX: should rename to enter_else */

		/* endof_if */
	
		struct {
			Short		end_label;
		} enter_negation;

		/* endof_negation */

		struct {
			Short		temp;	
		} enter_commit;

		struct {
			Short		temp;	
		} endof_commit;

		struct {
			Short		to_var;
			Short		from_var;
		} assign;

		struct {
			Short		var1;
			Short		var2;
		} test;

		struct {
			Short		to_var;
			Cons_id		consid;
			Short		list_length;
			Short		*var_list;	/* XXX: malloc */
		} construct;

		struct {
			Short		from_var;
			Cons_id		consid;
			Short		list_length;
			Short		*var_list;	/* XXX: malloc */
		} deconstruct;

		struct {
			Short		to_var;
			Cons_id		consid;
			Short		list_length;
			Var_dir		*var_dir_list;/* XXX: malloc */	
		} complex_construct;

		struct {
			Short		from_var;
			Cons_id		consid;
			Short		list_length;
			Var_dir		*var_dir_list;/* XXX: malloc */
		} complex_deconstruct;

		struct {
			Byte		to_reg;
			Short		from_var;
		} place_arg;

		struct {
			Byte		from_reg;
			Short		to_var;
		} pickup_arg;
			
		struct {
			CString		module_id;	/* XXX: malloc */
			CString		pred_id;	/* XXX: malloc */
			Short		arity;
			Byte		proc_id;
		} call;

		struct  {
			Short		pred_var;
			Short		in_var_count;
			Short		out_var_count;
			Determinism	det;
		} higher_order_call;

		struct {
			Byte		binop;
			Op_arg		arg1;
			Op_arg		arg2;
			Short		to_var;
		} builtin_binop;

		struct {
			Byte		unop;
			Op_arg		arg;
			Short		to_var;
		} builtin_unop;

		struct {
			Byte		binop;
			Op_arg		arg1;
			Op_arg		arg2;
		} builtin_bintest;	

		struct {
			Byte		unop;
			Op_arg		arg;
		} builtin_untest;	

		/* semidet_succeed */

		/* semidet_success_check */

		/* fail */

		struct {
			/* XXX: is this int or short?? */
			Short		line_number;
		} context;

		/* not_supported */

	} opt;
} Bytecode;

/*
 *	Possible values for Bytecode.id ...
 *
 *	We use #defines rather than an enumeration here since
 *	C enumeration constant must be of type int whereas we
 *	want byte (unsigned char).
 */
#define	BC_enter_pred			0
#define	BC_endof_pred			1
#define	BC_enter_proc			2
#define	BC_endof_proc			3
#define	BC_label			4
#define BC_enter_disjunction		5
#define	BC_endof_disjunction		6
#define	BC_enter_disjunct		7
#define	BC_endof_disjunct		8
#define	BC_enter_switch			9
#define	BC_endof_switch			10
#define	BC_enter_switch_arm		11
#define	BC_endof_switch_arm		12
#define	BC_enter_if			13
#define	BC_enter_then			14
/* XXX: enter_else would be a better name than endof_then */
#define	BC_endof_then			15
#define	BC_endof_if			16
#define	BC_enter_negation		17
#define	BC_endof_negation		18
#define	BC_enter_commit			19
#define	BC_endof_commit			20
#define	BC_assign			21
#define	BC_test				22
#define	BC_construct			23
#define	BC_deconstruct			24
#define	BC_complex_construct		25
#define	BC_complex_deconstruct		26
#define	BC_place_arg			27
#define	BC_pickup_arg			28
#define	BC_call				29
#define	BC_higher_order_call		30
#define	BC_builtin_binop		31
#define	BC_builtin_unop			32
#define	BC_builtin_bintest		33
#define	BC_builtin_untest		34
#define	BC_semidet_succeed		35
#define	BC_semidet_success_check	36
#define	BC_fail				37
#define	BC_context			38
#define	BC_not_supported		39
#define	BC_noop				255

/*
 *	Read the next bytecode from the stream fp.
 *	If no bytecode can be read, return FALSE.
 *	Otherwise, return TRUE.
 */
Bool
read_bytecode(FILE *fp, Bytecode *bc_p);

/*
 *	Read the bytecode version number from the stream fp.
 *	If the version number cannot be read, return FALSE.
 *	Otherwise, return TRUE.
 */
Bool
read_bytecode_version_number(FILE *fp, Short *version_number_p);

#endif	/* BYTECODE_H */
