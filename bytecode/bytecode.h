
/*
 *	$Id: bytecode.h,v 1.3 1997-01-29 01:41:04 aet Exp $
 *
 *	Copyright: The University of Melbourne, 1996
 */

#if	! defined(BYTECODE_H)
#define	BYTECODE_H


typedef struct Tag {
		Byte	tag_type;
		union {
			Byte	primary;
			struct {
				Byte	primary;
				Word	secondary; 
			} pair;
			Word	enum_tag;
		} opt;
} Tag;


/* 
 *	tag_type 
 */
#define	SIMPLE_TAG			0
#define	COMPLICATED_TAG			1
#define	COMPLICATED_CONSTANT_TAG	2
#define	ENUM_TAG			3
#define	NO_TAG				4


typedef unsigned short
	ushort;

typedef Byte
	Determinism;
/*
 *	Determinism
 */
#define	DET		0
#define	SEMIDET		1
#define	MULTIDET	2
#define	NONDET		3
#define	CC_MULTIDET	4
#define	CC_NONDET	5
#define	ERRONEOUS	6
#define	FAILURE		7

typedef struct Op_arg {
	Byte	arg_type;
	union {
		ushort	var;
		Int	int_const;
		Float	float_const;
	} opt;
} Op_arg;

#define	ARG_VAR	0
#define	ARG_INT_CONST	1
#define	ARG_FLOAT_CONST	2

typedef Byte
	Direction;
/*
 *	Direction
 */
#define	TO_ARG		0
#define	TO_VAR		1
#define	TO_NONE		2


typedef struct Var_dir {
	ushort		var;
	Direction	dir;
} Var_dir;

typedef struct Cons_id {
	Byte	cons_id_type;
	union {
		struct {
			CString		string;
			ushort		arity;
			Tag	tag;
		} cons;
		Int 	int_const;	
		CString	string_const;
		Float	float_const;
		struct {
			CString		module_id;
			CString		pred_id;
			ushort		arity;
			Byte		proc_id;
		} pred_const;
		struct {
			CString		module_id;
			CString		pred_id;
			ushort		arity;
			Byte		proc_id;
		} code_addr_const;
		struct {
			CString		module_id;
			CString		type_name;
			Byte		type_arity;
		} base_type_info_const;
	} opt;
} Cons_id;

#define	CONSID_CONS		0
#define	CONSID_INT_CONST	1
#define	CONSID_STRING_CONST	2
#define	CONSID_FLOAT_CONST	3
#define	CONSID_PRED_CONST	4
#define	CONSID_CODE_ADDR_CONST	5
#define	CONSID_BASE_TYPE_INFO_CONST	6


typedef struct Bytecode {
	Byte	bc;	/* Which bytecode instruction. e.g. BC_fail */
	union {
		struct {
			CString		pred_name;	/* XXX: malloc */
			ushort		proc_count;
		} enter_pred;

		/* endof_pred */

		struct {
			Byte		proc_id;
			Determinism	det;
			ushort		label_count;
			ushort		list_length;
			CString		*var_info_list; /* XXX: malloc */
		} enter_proc;

		struct {
			ushort		label;
		} label;

		struct {
			ushort		end_label;
		} enter_disjunction;

		/* endof_disjunction */

		struct {
			ushort		next_label;
		} enter_disjunct;

		struct {
			ushort		var;
			ushort		end_label;
		} enter_switch;
			
		/* endof_switch */

		struct {
			Cons_id		cons_id;
			ushort			next_label;
		} enter_switch_arm;


		/* endof_switch_arm */

		struct {
			ushort	else_label;
			ushort	end_label;
		} enter_if;

		/* enter_then */
		
		/* enter_else */

		/* endof_if */
	
		struct {
			ushort	end_label;
		} enter_negation;

		/* endof_negation */

		/* enter_commit */

		/* endof_commit */

		struct {
			ushort	to_var;
			ushort	from_var;
		} assign;

		struct {
			ushort	var1;
			ushort	var2;
		} test;

		struct {
			ushort			to_var;
			Cons_id		consid;
			ushort			list_length;
			ushort		*var_list;	/* XXX: malloc */
		} construct;

		struct {
			ushort			from_var;
			Cons_id		consid;
			ushort			list_length;
			ushort		*var_list;	/* XXX: malloc */
		} deconstruct;

		struct {
			ushort			from_var;
			Cons_id		consid;
			ushort			list_length;
			Var_dir		*var_dir_list;/* XXX: malloc */	
		} complex_construct;

		struct {
			ushort			from_var;
			Cons_id		consid;
			ushort			list_length;
			Var_dir		*var_dir_list;/* XXX: malloc */
		} complex_deconstruct;

		struct {
			Byte		to_reg;
			ushort		from_var;
		} place_arg;

		struct {
			CString		module_id;	/* XXX: malloc */
			CString		pred_id;	/* XXX: malloc */
			ushort		arity;
			Byte		proc_id;
		} call;


		struct {
			Byte		from_reg;
			ushort		to_var;
		} pickup_arg;
			
		struct {
			Byte		binop;
			Op_arg		arg1;
			Op_arg		arg2;
			ushort		to_var;
		} builtin_binop;

		struct {
			Byte		unop;
			Op_arg		arg;
			ushort		to_var;
		} builtin_unop;

		struct {
			Byte		binop;
			Op_arg		arg1;
			Op_arg		arg2;
		} builtin_bintest;	

		struct {
			Byte		binop;
			Op_arg		arg;
		} builtin_untest;	

		struct {
			ushort		line_number;
		} context;

		/* not_supported */

	} opt;
} Bytecode;

Bool
bytecode_to_name(Byte bytecode, CString *name_p);

Bool
determinism_to_name(Byte determinism_code, CString *name_p);

Bool
read_bytecode_version_number(FILE* fp, ushort *version_number_p);

Bool
read_tag(FILE *fp, Tag *tag_p);

Bool
read_cons_id(FILE *fp, Cons_id *cons_id_p);

Bool
read_var_dir(FILE *fp, Var_dir *var_dir_p);

Bool
read_op_arg(FILE *fp, Op_arg *op_arg_p);

Bool
read_determinism(FILE *fp, Determinism *det_p);

Bool
read_ushort(FILE *fp, ushort *ushort_p);

Bool
read_float(FILE *fp, float *float_p);

Bool
read_cstring(FILE *fp, CString *cstr_p);

Bool
read_int(FILE *fp, Int *int_p);

Bool
read_word(FILE *fp, Word *word_p);

Bool
read_byte(FILE* fp, Byte *byte_p);

Bool
read_cstring(FILE* fp, CString *str);

Bool
read_ushort(FILE* fp, ushort *ushort_p);

Bool
read_bytecode(FILE* fp, struct Bytecode *bc_p);

/*
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
#define	BC_fail				35
#define	BC_context			36
	/* NOTE: BC_not_supported must be the last bytecode */
#define	BC_not_supported		37

/*
CString
bytecode_to_string[];
*/

#if	0
main()
{
	ushort		shit;
	bytecode_t	bc;

	shit = bc.args.enter_proc.label_count;

	return;
}
#endif	/* 0 */


#endif	/* BYTECODE_H */
