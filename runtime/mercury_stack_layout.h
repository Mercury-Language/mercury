/*
** Copyright (C) 1998 The University of Melbourne.
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

/*-------------------------------------------------------------------------*/
/*
** Definitions for MR_PredFunc
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

typedef	Word MR_Determinism;

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
** Definitions for MR_Live_Lval
*/

/*
** MR_Live_Lval is a Word which describes an lval. This includes:
** 	- stack slots, registers, and special lvals such as succip, hp,
** 	  etc.
**
** MR_Live_Lval is encoded using an 8 bit low tag, the rest of the word is a 
** data field describing which stack slot number or register number.
**
**  Lval		Tag	Rest
**  r(Num)		 0	Num
**  f(Num)		 1	Num
**  stackvar(Num)	 2	Num
**  framevar(Num)	 3	Num
**  succip		 4
**  maxfr		 5
**  curfr		 6
**  hp			 7
**  sp			 8
**  unknown		 9		(The location is not known)
**
** The type MR_Lval_Type describes the different tag values.
**
** This data is generated in compiler/stack_layout.m, which must be kept
** in sync with the constants defined here.
*/

typedef Word MR_Live_Lval;

typedef enum { 
	MR_LVAL_TYPE_R,
	MR_LVAL_TYPE_F,
	MR_LVAL_TYPE_STACKVAR,
	MR_LVAL_TYPE_FRAMEVAR,
	MR_LVAL_TYPE_SUCCIP,
	MR_LVAL_TYPE_MAXFR,
	MR_LVAL_TYPE_CURFR,
	MR_LVAL_TYPE_HP,
	MR_LVAL_TYPE_SP,
	MR_LVAL_TYPE_UNKNOWN 
} MR_Lval_Type;

#define MR_LIVE_LVAL_TAGBITS	8

#define MR_LIVE_LVAL_TYPE(Lval) 			\
	((MR_Lval_Type) (((Word) Lval) & ((1 << MR_LIVE_LVAL_TAGBITS) - 1)))

#define MR_LIVE_LVAL_NUMBER(Lval) 			\
	((int) ((Word) Lval) >> MR_LIVE_LVAL_TAGBITS)

/*-------------------------------------------------------------------------*/
/*
** Definitions for MR_Live_Type
*/

/*
** MR_Live_Type describes live data. This includes:
** 	- succip, hp, curfr, maxfr, redoip, and
** 	  mercury data values (vars).
**
** The data is encoded such that low values (less than
** TYPELAYOUT_MAX_VARINT) represent succip, hp, etc.  Higher values
** represent data variables, and are pointers to a 2 word cell, 
** containing a pseudo type_info and an instantiation represention.
**
** This data is generated in compiler/stack_layout.m, which must be kept
** in sync with the constants defined here.
*/

typedef Word MR_Live_Type;

typedef enum { 
	MR_LIVE_TYPE_SUCCIP,
	MR_LIVE_TYPE_HP,
	MR_LIVE_TYPE_CURFR,
	MR_LIVE_TYPE_MAXFR,
	MR_LIVE_TYPE_REDOIP,
	MR_LIVE_TYPE_UNWANTED 
} MR_Lval_NonVar;

typedef struct { 
	Word	*pseudo_type_info;
	Word	inst;	/* not yet used; currently always -1 */
} MR_Var_Shape_Info;

#define MR_LIVE_TYPE_IS_VAR(T)         ( (Word) T > TYPELAYOUT_MAX_VARINT )

#define MR_LIVE_TYPE_GET_NONVAR(T)			\
		((MR_Lval_NonVar) T)

#define MR_LIVE_TYPE_GET_VAR_TYPE(T)   			\
		(((MR_Var_Shape_Info *) T)->pseudo_type_info)

#define MR_LIVE_TYPE_GET_VAR_INST(T)   			\
		(((MR_Var_Shape_Info *) T)->inst)

/* MR_Stack_Layout_Var --------------------------------------------------- */

typedef	struct MR_Stack_Layout_Var_Struct {
	MR_Live_Lval		MR_slv_locn;
	MR_Live_Type		MR_slv_live_type;
} MR_Stack_Layout_Var;

/*-------------------------------------------------------------------------*/
/*
** Definitions for MR_Stack_Layout_Vars
*/

/*
** If MR_slvs_tvars == NULL, there are no type parameters.
** If it is != NULL, then (Integer) MR_slvs_tvars[0] is the index
** of the highest numbered type parameter, and MR_slvs_tvars[i]
** for values of i between 1 and (Integer) MR_slvs_tvars[0] (both inclusive)
** describe the location of the typeinfo structure for the type variable
** of the corresponding number. If one of these type variables
** is not referred to by the variables described in MR_slvs_pairs,
** the corresponding entry will be zero.
*/

typedef	struct MR_Stack_Layout_Vars_Struct {
	MR_Stack_Layout_Var	*MR_slvs_pairs;
	String			*MR_slvs_names;
	MR_Live_Lval		*MR_slvs_tvars;
} MR_Stack_Layout_Vars;

#define	MR_name_if_present(vars, i)					\
				((vars->MR_slvs_names != NULL		\
				&& vars->MR_slvs_names[(i)] != NULL)	\
				? vars->MR_slvs_names[(i)]		\
				: "")

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
*/

typedef	struct MR_Stack_Layout_Entry_Struct {
	/* stack traversal group */
	Code			*MR_sle_code_addr;
	MR_Determinism		MR_sle_detism;
	Integer			MR_sle_stack_slots;
	MR_Live_Lval		MR_sle_succip_locn;

	/* proc id group */
	MR_PredFunc		MR_sle_pred_or_func;
	String			MR_sle_decl_module;
	String			MR_sle_def_module;
	String			MR_sle_name;
	Integer			MR_sle_arity;
	Integer			MR_sle_mode;

	/* exec trace group */
	struct MR_Stack_Layout_Label_Struct
				*MR_sle_call_label;
} MR_Stack_Layout_Entry;

#define	MR_ENTRY_LAYOUT_HAS_PROC_ID(entry)			\
		((int) entry->MR_sle_pred_or_func >= 0)

#define	MR_ENTRY_LAYOUT_HAS_EXEC_TRACE(entry)			\
		(MR_ENTRY_LAYOUT_HAS_PROC_ID(entry)		\
		&& entry->MR_sle_call_label != NULL)

/*
** Define a stack layout for a label that you know very little about.
** It is just a generic entry label, no useful information, except
** the code address for the label.
*/ 

#ifdef MR_USE_STACK_LAYOUTS
  #define MR_MAKE_STACK_LAYOUT_ENTRY(l) 				\
  const struct mercury_data__layout__##l##_struct {			\
	Code	*f1;							\
	Integer	f2;							\
	Integer	f3;							\
	Integer	f4;							\
	Integer	f5;							\
  } mercury_data__layout__##l = {					\
	STATIC(l),							\
	(Integer) -1, 	/* Unknown determinism */			\
	(Integer) -1,	/* Unknown number of stack slots */		\
        (Integer) MR_LVAL_TYPE_UNKNOWN,	/* Unknown succip location */	\
	(Integer) -1, 	/* The procid component is not present */	\
  };
#else
  #define MR_MAKE_STACK_LAYOUT_ENTRY(l)        
#endif	/* MR_USE_STACK_LAYOUTS */

/*-------------------------------------------------------------------------*/
/*
** Definitions for MR_Stack_Layout_Label
*/

/*
** The MR_sll_var_count field should be set to a negative number
** if there is no information about the variables live at the label.
*/

typedef	struct MR_Stack_Layout_Label_Struct {
	MR_Stack_Layout_Entry	*MR_sll_entry;
#ifdef	MR_LABEL_STRUCTS_INCLUDE_NUMBER
	Integer			MR_sll_label_num;
#endif
	Integer			MR_sll_var_count;
	/* the last field is present only if MR_sll_var_count > 0 */
	MR_Stack_Layout_Vars	MR_sll_var_info;
} MR_Stack_Layout_Label;

#ifdef	MR_LABEL_STRUCTS_INCLUDE_NUMBER
  #define	UNKNOWN_INTERNAL_LABEL_FIELD	Integer f2;
  #define	UNKNOWN_INTERNAL_LABEL_NUMBER	(Integer) -1,
#else
  #define	UNKNOWN_INTERNAL_LABEL_FIELD
  #define	UNKNOWN_INTERNAL_LABEL_NUMBER
#endif

/*
** Define a stack layout for an internal label. Need to supply the
** label name (l) and the entry label name (e).
**
** The only useful information in this structure is the code address
** and the reference to the entry for this label.
*/ 

#ifdef MR_USE_STACK_LAYOUTS
  #define MR_MAKE_STACK_LAYOUT_INTERNAL_WITH_ENTRY(l, e)		\
  const struct mercury_data__layout__##l##_struct {			\
	const Word *f1;							\
	UNKNOWN_INTERNAL_LABEL_FIELD					\
	Integer f3;							\
  } mercury_data__layout__##l = {					\
	(const Word *) &mercury_data__layout__##e,			\
	UNKNOWN_INTERNAL_LABEL_NUMBER					\
	(Integer) -1		/* No information about live values */	\
  };
#else
  #define MR_MAKE_STACK_LAYOUT_INTERNAL_WITH_ENTRY(l, e)        
#endif	/* MR_USE_STACK_LAYOUTS */

/*
** Define a stack layout for an internal label.
** Need to supply the label name (l) and the number (x), eg for
** label_name_i3, x is 3. It is assumed the entry label for that
** corresponds to this label is the label name without the _iX suffix.
**
** (MR_MAKE_STACK_LAYOUT_INTERNAL_WITH_ENTRY, above, is a little
** more general than MR_MAKE_STACK_LAYOUT_INTERNAL. This macro can
** only describe relationships between labels that have the same
** base -- MR_MAKE_STACK_LAYOUT_INTERNAL_WITH_ENTRY can create layouts
** for internal labels no matter what the name of the entry layout is).
**
** The only useful information in this structure is the code address
** and the reference to the entry for this label.
*/ 

#ifdef MR_USE_STACK_LAYOUTS
  #define MR_MAKE_STACK_LAYOUT_INTERNAL(e, x)				\
  const struct mercury_data__layout__##e##_i##x##_struct {		\
	const Word *f1;							\
	UNKNOWN_INTERNAL_LABEL_FIELD					\
	Integer f3;							\
  } mercury_data__layout__##e##_i##x = {				\
	(const Word *) &mercury_data__layout__##e,			\
	UNKNOWN_INTERNAL_LABEL_NUMBER					\
	(Integer) -1		/* No information about live values */	\
  };
#else
  #define MR_MAKE_STACK_LAYOUT_INTERNAL(l, x)        
#endif	/* MR_USE_STACK_LAYOUTS */

#endif /* not MERCURY_STACK_LAYOUT_H */
