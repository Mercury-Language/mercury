/*
** Copyright (C) 1997-1998 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

#ifndef MERCURY_ACCURATE_GC_H
#define MERCURY_ACCURATE_GC_H

/*
** mercury_accurate_gc.h -
**	Definitions for use by the accurate garbage collector (and
**	supporting code).
*/

/*---------------------------------------------------------------------------*/

/*
** Defintions used for accessing and creating stack_layouts.
**
** NOTE: The constants and data-structures used here need to be kept in
** 	 sync with the ones generated in the compiler. If you change
** 	 anything here, you may need to change compiler/stack_layout.m
** 	 as well.
*/

#ifdef NATIVE_GC
 #define MR_USE_STACK_LAYOUTS
#endif

/*
** Definitions for MR_Stack_Specifier
*/

typedef enum { MR_STACK_DET, MR_STACK_NONDET } MR_Stack_Specifier;

/*
** Definitions for "MR_Live_Lval"
**
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
	((MR_Lval_Type) ((Lval) & ((1 << MR_LIVE_LVAL_TAGBITS) - 1)))

#define MR_LIVE_LVAL_NUMBER(Lval) 			\
	((Word) (Lval) >> MR_LIVE_LVAL_TAGBITS)

/*
** Definitions for MR_Live_Type
**
** MR_Live_Type describes live data. This includes:
** 	- succip, hp, curfr, maxfr, redoip, and
** 	  mercury data values (vars).
**
** The data is encoded such that low values (less than
** TYPELAYOUT_MAX_VARINT) represent succip, hp, etc.  Higher values
** represent data variables, and are pointers to a 2 word cell, 
** containing a type_info and an instantiation represention.
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
	Word	type;
	Word	inst;
} MR_Var_Shape_Info;

#define MR_LIVE_TYPE_IS_VAR(T)         ( (Word) T > TYPELAYOUT_MAX_VARINT )

#define MR_LIVE_TYPE_GET_NONVAR(T)			\
		((MR_Lval_NonVar) T)

#define MR_LIVE_TYPE_GET_VAR_TYPE(T)   			\
		((Word) ((MR_Var_Shape_Info *) T)->type)

#define MR_LIVE_TYPE_GET_VAR_INST(T)   			\
		((Word) ((MR_Var_Shape_Info *) T)->inst)


/*
** Macros to support hand-written C code.
*/

/*
** Define a stack layout for a label that you know very little about.
** It's just a generic entry label, no useful information, except
** the code address for the label.
*/ 
#ifdef MR_USE_STACK_LAYOUTS
 #define MR_MAKE_STACK_LAYOUT_ENTRY(l) 					\
 const struct mercury_data__stack_layout__##l##_struct {		\
	Code * f1;							\
	Integer f2;							\
	Integer f3;							\
	Integer f4;							\
 } mercury_data__stack_layout__##l = {					\
	STATIC(l),							\
	(Integer) -1,	/* Unknown number of stack slots */		\
	(Integer) -1, 	/* Unknown code model */			\
        (Integer) MR_LVAL_TYPE_UNKNOWN 	/* Unknown succip location */	\
 };
#else
 #define MR_MAKE_STACK_LAYOUT_ENTRY(l)        
#endif	/* MR_USE_STACK_LAYOUTS */

/*
** Define a stack layout for an internal label. Need to supply the
** label name (l) and the entry label name (e).
**
** The only useful information in this structure is the code address
** and the reference to the entry for this label.
*/ 
#ifdef MR_USE_STACK_LAYOUTS
 #define MR_MAKE_STACK_LAYOUT_INTERNAL_WITH_ENTRY(l, e)			\
 const struct mercury_data__stack_layout__##l##_struct {		\
	const Word * f1;						\
	Integer f2;							\
	const Word * f3;						\
 } mercury_data__stack_layout__##l = {					\
	(const Word *) &mercury_data__stack_layout__##e,		\
	(Integer) -1,		/* Unknown number of live values */	\
	(const Word *) NULL	/* No list of live valeus */		\
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
 const struct mercury_data__stack_layout__##e##_i##x##_struct {		\
	const Word * f1;						\
	Integer f2;							\
	const Word * f3;						\
 } mercury_data__stack_layout__##e##_i##x = {				\
	(const Word *) &mercury_data__stack_layout__##e,		\
	(Integer) -1,		/* Unknown number of live values */	\
	(const Word *) NULL	/* No list of live valeus */		\
 };
#else
 #define MR_MAKE_STACK_LAYOUT_INTERNAL(l, x)        
#endif	/* MR_USE_STACK_LAYOUTS */


/*
** Macros to support stack layouts.
** XXX ought to use a MR_Entry_Stack_Layout and MR_Cont_Stack_Layout
** struct to make it easier to access the fields.
*/

#define MR_ENTRY_STACK_LAYOUT_GET_LABEL_ADDRESS(s)		\
		((Code *) field(0, (s), 0))

#define MR_CONT_STACK_LAYOUT_GET_ENTRY_LAYOUT(s)		\
		(field(0, (s), 0))

#define MR_ENTRY_STACK_LAYOUT_GET_NUM_SLOTS(s)			\
		(field(0, (s), 1))

#define MR_ENTRY_STACK_LAYOUT_GET_CODE_MODEL(s)			\
		(field(0, (s), 2))

#define MR_ENTRY_STACK_LAYOUT_GET_SUCCIP_LOC(s)			\
		(field(0, (s), 3))

/*---------------------------------------------------------------------------*/
#endif /* not MERCURY_ACCURATE_GC_H */
