/*
** Copyright (C) 1998-1999 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

#ifndef MERCURY_STACK_LAYOUT_H
#define MERCURY_STACK_LAYOUT_H

/*
** mercury_stack_layout.h -
**	Definitions for the stack layout data structures.
**
** NOTE: The constants and data-structures used here need to be kept in
** sync with the ones generated in the compiler. If you change anything here,
** you may need to change compiler/stack_layout.m as well.
*/

#include "mercury_types.h"
#include "mercury_std.h"			/* for MR_VARIABLE_SIZED */

/*-------------------------------------------------------------------------*/
/*
** Definitions for MR_PredFunc. This enum should EXACTLY match the definition
** of the `pred_or_func' type in browser/debugger_interface.
*/

typedef	enum { MR_PREDICATE, MR_FUNCTION } MR_PredFunc;

/*-------------------------------------------------------------------------*/
/*
** Definitions for MR_Determinism
*/

/*
** The max_soln component of the determinism is encoded in the 1 and 2 bits.
** The can_fail component of the determinism is encoded in the 4 bit.
** The first_solution component of the determinism is encoded in the 8 bit.
**
** MR_DETISM_AT_MOST_MANY could also be defined as ((d) & 3) == 3),
** but this would be less efficient, since the C compiler does not know
** that we do not set the 1 bit unless we also set the 2 bit.
*/

typedef	MR_int_least16_t	MR_Determinism;

#define	MR_DETISM_DET		6
#define	MR_DETISM_SEMI		2
#define	MR_DETISM_NON		3
#define	MR_DETISM_MULTI		7
#define	MR_DETISM_ERRONEOUS	4
#define	MR_DETISM_FAILURE	0
#define	MR_DETISM_CCNON		10
#define	MR_DETISM_CCMULTI	14

#define MR_DETISM_AT_MOST_ZERO(d)	(((d) & 3) == 0)
#define MR_DETISM_AT_MOST_ONE(d)	(((d) & 3) == 2)
#define MR_DETISM_AT_MOST_MANY(d)	(((d) & 1) != 0)

#define MR_DETISM_CAN_FAIL(d)		(((d) & 4) != 0)

#define MR_DETISM_FIRST_SOLN(d)		(((d) & 8) != 0)

#define MR_DETISM_DET_STACK(d)		(!MR_DETISM_AT_MOST_MANY(d) \
					|| MR_DETISM_FIRST_SOLN(d))

/*-------------------------------------------------------------------------*/
/*
** Definitions for MR_Long_Lval and MR_Short_Lval
*/

/*
** MR_Long_Lval is a MR_uint_least32_t which describes an location.
** This includes lvals such as stack slots, general registers, and special
** registers such as succip, hp, etc, as well as locations whose address is
** given as a typeinfo inside the type class info structure pointed to by an
** lval.
**
** What kind of location an MR_Long_Lval refers to is encoded using
** a low tag with MR_LONG_LVAL_TAGBITS bits; the type MR_Lval_Type describes
** the different tag values. The interpretation of the rest of the word
** depends on the location type:
**
**  Locn		Tag	Rest
**  r(Num)		 0	Num (register number)
**  f(Num)		 1	Num (register number)
**  stackvar(Num)	 2	Num (stack slot number)
**  framevar(Num)	 3	Num (stack slot number)
**  succip		 4
**  maxfr		 5
**  curfr		 6
**  hp			 7
**  sp			 8
**  indirect(Base, N)	 9	See below
**  unknown		10	(The location is not known)
**
** For indirect references, the word exclusive of the tag consists of
** (a) an integer with MR_LONG_LVAL_OFFSETBITS bits giving the index of
** the typeinfo inside a type class info (to be interpreted by
** MR_typeclass_info_type_info or the predicate
** private_builtin:type_info_from_typeclass_info, which calls it) and
** (b) a MR_Long_Lval value giving the location of the pointer to the
** type class info.  This MR_Long_Lval value will *not* have an indirect
** tag.
**
** This data is generated in stack_layout__represent_locn_as_int,
** which must be kept in sync with the constants and macros defined here.
*/

typedef MR_uint_least32_t	MR_Long_Lval;

typedef enum {
	MR_LONG_LVAL_TYPE_R,
	MR_LONG_LVAL_TYPE_F,
	MR_LONG_LVAL_TYPE_STACKVAR,
	MR_LONG_LVAL_TYPE_FRAMEVAR,
	MR_LONG_LVAL_TYPE_SUCCIP,
	MR_LONG_LVAL_TYPE_MAXFR,
	MR_LONG_LVAL_TYPE_CURFR,
	MR_LONG_LVAL_TYPE_HP,
	MR_LONG_LVAL_TYPE_SP,
	MR_LONG_LVAL_TYPE_INDIRECT,
	MR_LONG_LVAL_TYPE_UNKNOWN 
} MR_Long_Lval_Type;

/* This must be in sync with stack_layout__long_lval_tag_bits */
#define MR_LONG_LVAL_TAGBITS	4

#define MR_LONG_LVAL_TYPE(Locn) 					\
	((MR_Long_Lval_Type)						\
		(((Word) Locn) & ((1 << MR_LONG_LVAL_TAGBITS) - 1)))

#define MR_LONG_LVAL_NUMBER(Locn) 					\
	((int) ((Word) Locn) >> MR_LONG_LVAL_TAGBITS)

/* This must be in sync with stack_layout__offset_bits */
#define MR_LONG_LVAL_OFFSETBITS	6

#define MR_LONG_LVAL_INDIRECT_OFFSET(LocnNumber) 			\
	((int) ((LocnNumber) & ((1 << MR_LONG_LVAL_OFFSETBITS) - 1)))

#define MR_LONG_LVAL_INDIRECT_BASE_LVAL(LocnNumber)			\
	(((Word) (LocnNumber)) >> MR_LONG_LVAL_OFFSETBITS)

#define	MR_LONG_LVAL_STACKVAR(n)					\
	((Word) ((n) << MR_LONG_LVAL_TAGBITS) + MR_LONG_LVAL_TYPE_STACKVAR)

#define	MR_LONG_LVAL_FRAMEVAR(n)					\
	((Word) ((n) << MR_LONG_LVAL_TAGBITS) + MR_LONG_LVAL_TYPE_FRAMEVAR)

#define	MR_LONG_LVAL_R_REG(n)						\
	((Word) ((n) << MR_LONG_LVAL_TAGBITS) + MR_LONG_LVAL_TYPE_R)

/*
** MR_Short_Lval is a MR_uint_least8_t which describes an location. This
** includes lvals such as stack slots and general registers that have small
** numbers, and special registers such as succip, hp, etc.
**
** What kind of location an MR_Long_Lval refers to is encoded using
** a low tag with 2 bits; the type MR_Short_Lval_Type describes
** the different tag values. The interpretation of the rest of the word
** depends on the location type:
**
**  Locn		Tag	Rest
**  r(Num)		 0	Num (register number)
**  stackvar(Num)	 1	Num (stack slot number)
**  framevar(Num)	 2	Num (stack slot number)
**  special reg		 3	MR_Long_Lval_Type
**
** This data is generated in stack_layout__represent_locn_as_byte,
** which must be kept in sync with the constants and macros defined here.
*/

typedef MR_uint_least8_t	MR_Short_Lval;

typedef enum {
	MR_SHORT_LVAL_TYPE_R,
	MR_SHORT_LVAL_TYPE_STACKVAR,
	MR_SHORT_LVAL_TYPE_FRAMEVAR,
	MR_SHORT_LVAL_TYPE_SPECIAL
} MR_Short_Lval_Type;

/* This must be in sync with stack_layout__short_lval_tag_bits */
#define MR_SHORT_LVAL_TAGBITS	2

#define MR_SHORT_LVAL_TYPE(Locn) 					\
	((MR_Short_Lval_Type)						\
		(((Word) Locn) & ((1 << MR_SHORT_LVAL_TAGBITS) - 1)))

#define	MR_SHORT_LVAL_STACKVAR(n)					\
	((MR_Short_Lval) (((n) << MR_SHORT_LVAL_TAGBITS)		\
		+ MR_SHORT_LVAL_TYPE_STACKVAR))

#define	MR_SHORT_LVAL_FRAMEVAR(n)					\
	((MR_Short_Lval) (((n) << MR_SHORT_LVAL_TAGBITS)		\
		+ MR_SHORT_LVAL_TYPE_FRAMEVAR))

#define	MR_SHORT_LVAL_R_REG(n)						\
	((MR_Short_Lval) (((n) << MR_SHORT_LVAL_TAGBITS)		\
		+ MR_SHORT_LVAL_TYPE_R))

/*-------------------------------------------------------------------------*/
/*
** Definitions for MR_Stack_Layout_Vars
*/

/*
** If MR_slvs_tvars == NULL, there are no type parameters.
** If it is != NULL, then MR_slvs_tvars->MR_tp_param_count is the number
** of type parameters, and the element at index i in the array
** MR_slvs_tvars->MR_tp_param_locns describes the location of the typeinfo
** structure for type variable i+1 (since array offsets start at zero
** but type variable numbers start at one). If one of these type variables
** is not referred to by the variables described in MR_slvs_pairs,
** the corresponding entry will be zero.
*/

typedef	struct MR_Type_Param_Locns_Struct {
	MR_uint_least32_t	MR_tp_param_count;
	MR_Long_Lval		MR_tp_param_locns[MR_VARIABLE_SIZED];
} MR_Type_Param_Locns;

/*
** This structure describes the name of a live value, if it has any.
**
** The MR_var_num field is zero if the live value is not a variable;
** if it is, it gives the HLDS number of the variable.
**
** The MR_var_name_offset field gives the offset, within the string
** table of the module containing the layout structure, of the name
** of the variable. If the variable has no name, or if the live value
** is not a variable, the offset will be zero.
*/

typedef	struct MR_Var_Name_Struct {
	MR_uint_least16_t	MR_var_num;
	MR_uint_least16_t	MR_var_name_offset;
} MR_Var_Name;

/*
** This data structure describes the variables live at a given point.
** The count of live variables is encoded; it gives separately the counts
** of variables that have short and long location descriptions, or it may
** say that there is no information about variables at this point (which is
** very different from saying that there are no variables live at this point).
** You can decode the count using the macros below.
**
** The last three fields are meaningful only if the MR_has_valid_var_count
** macro returns true.
**
** The names array pointer may be NULL, in which case no information about
** variable names is available.
**
** The type parameters array may also be NULL, but this means that there are
** no type parameters in the types of the variables live at this point.
**
** For further information, see the top of compiler/stack_layout.m.
*/

typedef	struct MR_Stack_Layout_Vars_Struct {
	Integer			MR_slvs_var_count;
	void			*MR_slvs_locns_types;
	MR_Var_Name		*MR_slvs_names;
	MR_Type_Param_Locns	*MR_slvs_tvars;
} MR_Stack_Layout_Vars;

#define	MR_SHORT_COUNT_BITS	10
#define	MR_SHORT_COUNT_MASK	((1 << MR_SHORT_COUNT_BITS) - 1)

#define	MR_has_valid_var_count(slvs)					    \
		(((slvs)->MR_slvs_var_count) >= 0)
#define	MR_has_valid_var_info(slvs)					    \
		(((slvs)->MR_slvs_var_count) > 0)
#define	MR_long_desc_var_count(slvs)					    \
		(((slvs)->MR_slvs_var_count) >> MR_SHORT_COUNT_BITS)
#define	MR_short_desc_var_count(slvs)					    \
		(((slvs)->MR_slvs_var_count) & MR_SHORT_COUNT_MASK)
#define	MR_all_desc_var_count(slvs)					    \
		(MR_long_desc_var_count(slvs) + MR_short_desc_var_count(slvs))

#define	MR_var_pti(slvs, i)						    \
		(((MR_PseudoTypeInfo **) ((slvs)->MR_slvs_locns_types))[(i)])
#define	MR_end_of_var_ptis(slvs)					    \
		(&MR_var_pti((slvs), MR_all_desc_var_count(slvs)))
#define	MR_long_desc_var_locn(slvs, i)					    \
		(((MR_uint_least32_t *) MR_end_of_var_ptis(slvs))[(i)])
#define	MR_end_of_long_desc_var_locns(slvs)				    \
		(&MR_long_desc_var_locn((slvs), MR_long_desc_var_count(slvs)))
#define	MR_short_desc_var_locn(slvs, i)					    \
		(((MR_uint_least8_t *)					    \
			MR_end_of_long_desc_var_locns(slvs))[(i)])

#define	MR_name_if_present(module_layout, vars, i)			    \
		(((vars)->MR_slvs_names == NULL) ? "" :			    \
		((module_layout)->MR_ml_string_table +			    \
			(vars)->MR_slvs_names[(i)].MR_var_name_offset))

#define	MR_name_if_present_from_label(label_layout, i)			    \
		MR_name_if_present(					    \
			(label_layout)->MR_sll_entry->MR_sle_module_layout, \
			&((label_layout)->MR_sll_var_info),		    \
			(i))

/*-------------------------------------------------------------------------*/
/*
** Definitions for MR_Stack_Layout_Entry
*/

/*
** This structure records information about a procedure.
** The structure has three groups of fields:
**
**	(1) those needed for traversing the stack;
**	(2) those needed for identifying the procedure;
**	(3) those needed for execution tracing.
**
** For accurate garbage collection, we only need group (1).
** For stack tracing, we need groups (1) and (2).
** For execution tracing, we need groups (1), (2) and (3).
**
** To save space, for each use we only include the fields that belong
** to the needed groups, plus the first field in the first non-included group,
** which is set to a special value to indicate the absence of the group
** and any following groups.
**
** Group (1) is always present and meaningful.
** Group (2) is present and meaningful
** if MR_ENTRY_LAYOUT_HAS_PROC_ID(entry) evaluates to true.
** Group (3) is present and meaningful
** if MR_ENTRY_LAYOUT_HAS_EXEC_TRACE(entry) evaluates to true.
**
** Group (2) fields have a different interpretation if the procedure is
** compiler-generated. You can test whether this is the case by using the macro
** MR_ENTRY_LAYOUT_COMPILER_GENERATED, but only after checking that
** MR_ENTRY_LAYOUT_HAS_PROC_ID is true.
**
** For further details on the semantics of the fields, see stack_layout.m.
*/

/*
** The places that know about the structure of procedure ids include
** browser/dl.m and of course compiler/stack_layout, besides all the places 
** that refer to the C types we now define.
*/

typedef struct MR_Stack_Layout_User_Proc_Struct {
	MR_PredFunc		MR_user_pred_or_func;
	ConstString		MR_user_decl_module;
	ConstString		MR_user_def_module;
	ConstString		MR_user_name;
	MR_int_least16_t	MR_user_arity;
	MR_int_least16_t	MR_user_mode;
} MR_Stack_Layout_User_Proc;

typedef struct MR_Stack_Layout_Compiler_Proc_Struct {
	ConstString		MR_comp_type_name;
	ConstString		MR_comp_type_module;
	ConstString		MR_comp_def_module;
	ConstString		MR_comp_pred_name;
	MR_int_least16_t	MR_comp_arity;
	MR_int_least16_t	MR_comp_mode;
} MR_Stack_Layout_Compiler_Proc;

typedef union MR_Stack_Layout_Proc_Id_Union {
	MR_Stack_Layout_User_Proc	MR_proc_user;
	MR_Stack_Layout_Compiler_Proc	MR_proc_comp;
} MR_Stack_Layout_Proc_Id;

typedef	struct MR_Stack_Layout_Entry_Struct {
	/* stack traversal group */
	Code			*MR_sle_code_addr;
	MR_Long_Lval		MR_sle_succip_locn;
	MR_int_least16_t	MR_sle_stack_slots;
	MR_Determinism		MR_sle_detism;

	/* proc id group */
	MR_Stack_Layout_Proc_Id	MR_sle_proc_id;

	/* exec trace group */
	struct MR_Stack_Layout_Label_Struct
				*MR_sle_call_label;
	struct MR_Module_Layout_Struct
				*MR_sle_module_layout;
	MR_int_least16_t	MR_sle_max_r_num;
	MR_int_least8_t		MR_sle_maybe_from_full;
	MR_int_least8_t		MR_sle_maybe_trail;
	MR_int_least8_t		MR_sle_maybe_decl_debug;
} MR_Stack_Layout_Entry;

#define	MR_sle_user	MR_sle_proc_id.MR_proc_user
#define	MR_sle_comp	MR_sle_proc_id.MR_proc_comp

#define	MR_ENTRY_LAYOUT_HAS_PROC_ID(entry)			\
		((Word) entry->MR_sle_user.MR_user_pred_or_func != -1)

#define	MR_ENTRY_LAYOUT_HAS_EXEC_TRACE(entry)			\
		(MR_ENTRY_LAYOUT_HAS_PROC_ID(entry)		\
		&& entry->MR_sle_call_label != NULL)

#define	MR_ENTRY_LAYOUT_COMPILER_GENERATED(entry)		\
		((Unsigned) entry->MR_sle_user.MR_user_pred_or_func \
		> MR_FUNCTION)

/*
** Define a layout structure for a procedure, containing information
** for the first two groups of fields.
**
** The slot count and the succip location parameters do not have to be
** supplied for procedures that live on the nondet stack, since for such
** procedures the size of the frame can be deduced from the prevfr field
** and the location of the succip is fixed.
**
** An unknown slot count should be signalled by MR_ENTRY_NO_SLOT_COUNT.
** An unknown succip location should be signalled by MR_LONG_LVAL_TYPE_UNKNOWN.
**
** For the procedure identification, we always use the same module name
** for the defining and declaring modules, since procedures whose code
** is hand-written as C modules cannot be inlined in other Mercury modules.
**
** Due to the possibility that code addresses are not static, any use of
** the MR_MAKE_PROC_LAYOUT macro has to be accompanied by a call to the
** MR_INIT_PROC_LAYOUT_ADDR macro in the initialization code of the C module
** that defines the entry. (The cast in the body of MR_INIT_PROC_LAYOUT_ADDR
** is needed because compiler-generated layout structures have their own
** compiler-generated type.)
*/ 

#define	MR_ENTRY_NO_SLOT_COUNT		-1

#ifdef	MR_STATIC_CODE_ADDRESSES
 #define	MR_MAKE_PROC_LAYOUT_ADDR(entry)		STATIC(entry)
 #define	MR_INIT_PROC_LAYOUT_ADDR(entry)		do { } while (0)
#else
 #define	MR_MAKE_PROC_LAYOUT_ADDR(entry)		((Code *) NULL)
 #define	MR_INIT_PROC_LAYOUT_ADDR(entry)				\
		do {							\
			((MR_Stack_Layout_Entry *) &			\
			mercury_data__layout__##entry)			\
				->MR_sle_code_addr = ENTRY(entry);	\
		} while (0)
#endif

#define MR_MAKE_PROC_LAYOUT(entry, detism, slots, succip_locn,		\
		pf, module, name, arity, mode) 				\
	MR_Stack_Layout_Entry mercury_data__layout__##entry = {		\
		MR_MAKE_PROC_LAYOUT_ADDR(entry),			\
		succip_locn,						\
		slots,							\
		detism,							\
		{{							\
			pf,						\
			module,						\
			module,						\
			name,						\
			arity,						\
			mode						\
		}},							\
		NULL							\
	}

/*
** In procedures compiled with execution tracing, three items are stored
** in stack slots with fixed numbers. They are:
**
**	the event number of the last event before the call event,
**	the call number, and
**	the call depth.
**
** Note that the first slot does not store the number of the call event
** itself, but rather the number of the call event minus one. The reason
** for this is that (a) incrementing the number stored in this slot would
** increase executable size, and (b) if the procedure is shallow traced,
** MR_trace may not be called for the call event, so we cannot shift the
** burden of initializing fields to the MR_trace of the call event either.
**
** The following macros will access the fixed slots. They can be used whenever
** MR_ENTRY_LAYOUT_HAS_EXEC_TRACE(entry) is true; which set you should use
** depends on the determinism of the procedure.
**
** These macros have to be kept in sync with compiler/trace.m.
*/

#define MR_event_num_framevar(base_curfr)    MR_based_framevar(base_curfr, 1)
#define MR_call_num_framevar(base_curfr)     MR_based_framevar(base_curfr, 2)
#define MR_call_depth_framevar(base_curfr)   MR_based_framevar(base_curfr, 3)

#define MR_event_num_stackvar(base_sp)	     MR_based_stackvar(base_sp, 1)
#define MR_call_num_stackvar(base_sp)	     MR_based_stackvar(base_sp, 2)
#define MR_call_depth_stackvar(base_sp)	     MR_based_stackvar(base_sp, 3)

/*
** In model_non procedures compiled with --trace-redo, one or two other items
** are stored in fixed stack slots. These are
**
**	the address of the layout structure for the redo event
**	the saved copy of the from-full flag (only if trace level is shallow)
**
** The following macros will access these slots. They should be used only from
** within the code that calls MR_trace for the REDO event.
**
** This macros have to be kept in sync with compiler/trace.m.
*/

#define MR_redo_layout_framevar(base_curfr)   MR_based_framevar(base_curfr, 4)
#define MR_redo_fromfull_framevar(base_curfr) MR_based_framevar(base_curfr, 5)

/*-------------------------------------------------------------------------*/
/*
** Definitions for MR_Stack_Layout_Label
*/

typedef	struct MR_Stack_Layout_Label_Struct {
	MR_Stack_Layout_Entry	*MR_sll_entry;
	MR_int_least16_t	MR_sll_port;
	MR_int_least16_t	MR_sll_goal_path;
	MR_Stack_Layout_Vars	MR_sll_var_info;
} MR_Stack_Layout_Label;

/*
** Define a stack layout for an internal label.
**
** The MR_MAKE_INTERNAL_LAYOUT_WITH_ENTRY variant allows you to specify
** the label name (l) and the entry label name (e) independently, which
** means that it can be used for labels in code fragments which are
** simultaneously part of several procedures. (Some hand-written code
** in the library is like this; the different procedures usually differ
** only in attributes such as the uniqueness of their arguments.)
**
** The MR_MAKE_INTERNAL_LAYOUT variant assumes that the internal label
** is in the procedure named by the entry label.
**
** The only useful information in the structures created by these macros
** is the reference to the procedure layout, which allows you to find the
** stack frame size and the succip location, thereby enabling stack tracing.
**
** For the native garbage collector, we will need to add meaningful
** live value information as well to these macros.
*/ 

#define MR_MAKE_INTERNAL_LAYOUT_WITH_ENTRY(label, entry) \
	MR_Stack_Layout_Label mercury_data__layout__##label = {		\
		&mercury_data__layout__##entry,				\
		-1,							\
		-1,							\
		{							\
			-1,		/* No info about live values */	\
			NULL,						\
			NULL,						\
			NULL						\
		}							\
	}

#define MR_MAKE_INTERNAL_LAYOUT(entry, labelnum)			\
	MR_MAKE_INTERNAL_LAYOUT_WITH_ENTRY(entry##_i##labelnum, entry)

/*-------------------------------------------------------------------------*/
/*
** Definitions for MR_Exception_Layout
**
** The first field will be a copy of the stack layout for the return label,
** with the port changed to `MR_PORT_EXCEPTION', and with an empty goal path.
** The second field will hold the address of the original return label layout.
*/

typedef	struct MR_Exception_Layout_Struct {
	MR_Stack_Layout_Label	MR_el_layout;
	MR_Stack_Layout_Label	*MR_el_return_layout;
} MR_Exception_Layout;

/*-------------------------------------------------------------------------*/
/*
** Definitions for MR_Module_Layout
**
** The layout struct for a module contains three main components.
**
** The first is a string table, which contains strings referred to by other
** layout structures in the module (initially only the tables containing
** variables names, referred to from label layout structures).
**
** The second is a table containing pointers to the proc layout structures
** of all the procedures in the module.
**
** The third component contains N tables if the module has labels corresponding
** to contexts that refer to N filenames. For each filename, the table gives
** the name of the file and the number of labels in that file in this module;
** for each such label it gives its line number and its label layout struct.
**
** The corresponding elements of the label_lineno and label_layout arrays
** refer to the same label. (The reason why they are not stored together
** is space efficiency; adding a 16 bit field to a label layout structure would
** require padding.) The labels are sorted on line number.
*/

typedef struct MR_Module_File_Layout_Struct {
	String			MR_mfl_filename;
	Integer			MR_mfl_label_count;
	/* the following fields point to arrays of size MR_mfl_label_count */
	MR_int_least16_t	*MR_mfl_label_lineno;
	MR_Stack_Layout_Label	**MR_mfl_label_layout;
} MR_Module_File_Layout;

typedef	struct MR_Module_Layout_Struct {
	String			MR_ml_name;
	Integer			MR_ml_string_table_size;
	char			*MR_ml_string_table;
	Integer			MR_ml_proc_count;
	MR_Stack_Layout_Entry	**MR_ml_procs;
	Integer			MR_ml_filename_count;
	MR_Module_File_Layout	**MR_ml_module_file_layout;
} MR_Module_Layout;

#endif /* not MERCURY_STACK_LAYOUT_H */
