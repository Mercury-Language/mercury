/*
** Copyright (C) 1995 University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/


#ifndef TYPE_INFO_H
#define TYPE_INFO_H

/*
** Decide which type_info representation we will use.
**
** At the end, exactly one of the two macros ONE_CELL_TYPE_INFO and
** ONE_OR_TWO_CELL_TYPE_INFO should defined. If it is the latter, then
** SHARED_ONE_OR_TWO_CELL_TYPE_INFO may be defined as well.
*/

#if defined(SHARED_ONE_OR_TWO_CELL_TYPE_INFO)
    /* #define		SHARED_ONE_OR_TWO_CELL_TYPE_INFO */
    #define		ONE_OR_TWO_CELL_TYPE_INFO
    #undef		ONE_CELL_TYPE_INFO
#elif defined(ONE_OR_TWO_CELL_TYPE_INFO)
    #undef		SHARED_ONE_OR_TWO_CELL_TYPE_INFO
    /* #define		ONE_OR_TWO_CELL_TYPE_INFO */
    #undef		ONE_CELL_TYPE_INFO
#elif defined(ONE_CELL_TYPE_INFO)
    #undef		SHARED_ONE_OR_TWO_CELL_TYPE_INFO
    #undef		ONE_OR_TWO_CELL_TYPE_INFO
    /* #define		ONE_CELL_TYPE_INFO */
#else
    /* use the default type_info representation: */
    /* shared_one_or_two_cell if addresses are constants, otherwise one_cell */
    #if defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)
	#undef		SHARED_ONE_OR_TWO_CELL_TYPE_INFO
	#undef		ONE_OR_TWO_CELL_TYPE_INFO
	#define		ONE_CELL_TYPE_INFO
    #else
	#define		SHARED_ONE_OR_TWO_CELL_TYPE_INFO
	#define		ONE_OR_TWO_CELL_TYPE_INFO
	#undef		ONE_CELL_TYPE_INFO
    #endif
#endif

/*
** Define offsets of fields in the type_info structure.
** See polymorphism.m for explanation of these offsets and how the
** type_info structure is laid out.
**
** The one_or_two_cell type_info representation
** *depends* on OFFSET_FOR_COUNT being 0.
*/


#define OFFSET_FOR_COUNT 0
#define OFFSET_FOR_UNIFY_PRED 1
#define OFFSET_FOR_INDEX_PRED 2
#define OFFSET_FOR_COMPARE_PRED 3
#define OFFSET_FOR_TERM_TO_TYPE_PRED 4
#define OFFSET_FOR_TYPE_TO_TERM_PRED 5

/* 
** USE_TYPE_TO_TERM is presently undefined. Code may break if it is
** just redefined here - changes also need to be made to the compiler.
*/

#ifdef USE_TYPE_TO_TERM
	#define OFFSET_FOR_ARG_TYPE_INFOS 6 
	#define OFFSET_FOR_BASE_TYPE_LAYOUT 6 
#else
	#define OFFSET_FOR_ARG_TYPE_INFOS 4
	#define OFFSET_FOR_BASE_TYPE_LAYOUT 4
#endif

#define COMPARE_EQUAL 0
#define COMPARE_LESS 1
#define COMPARE_GREATER 2

#ifdef  COMPACT_ARGS
#define	mercury__unify__typeinfo	r1
#define	mercury__unify__x		r2
#define	mercury__unify__y		r3
#define	mercury__unify__offset		0
#define	mercury__compare__typeinfo	r1
#define	mercury__compare__x		r2
#define	mercury__compare__y		r3
#define	mercury__compare__offset	0
#define	mercury__term_to_type__typeinfo	r1
#define	mercury__term_to_type__term	r2
#define	mercury__term_to_type__x	r4
#define	mercury__term_to_type__offset	1
#define unify_input1    r1
#define unify_input2    r2
#define unify_output    r1
#define compare_input1  r1
#define compare_input2  r2
#define compare_output  r1
#define index_input     r1
#define index_output    r1
#else
#define	mercury__unify__typeinfo	r2
#define	mercury__unify__x		r3
#define	mercury__unify__y		r4
#define	mercury__unify__offset		1
#define	mercury__compare__typeinfo	r1
#define	mercury__compare__x		r3
#define	mercury__compare__y		r4
#define	mercury__compare__offset	1
#define	mercury__term_to_type__typeinfo	r2
#define	mercury__term_to_type__term	r3
#define	mercury__term_to_type__x	r4
#define	mercury__term_to_type__offset	1
#define unify_input1    r2
#define unify_input2    r3
#define unify_output    r1
#define compare_input1  r2
#define compare_input2  r3
#define compare_output  r1
#define index_input     r1
#define index_output    r2
#endif


/*
** Definitions and macros for base_type_layout definition.
**
** See compiler/base_type_layout.m for more information.
**
** If we don't have enough tags, we have to encode layouts
** less densely. The make_typelayout macro does this, and
** is intended for handwritten code. Compiler generated
** code can (and does) just create two rvals instead of one. 
**
*/

/*
** All code using type_layout structures should check to see if
** USE_TYPE_LAYOUT is defined, and give a fatal error otherwise.
** For USE_TYPE_LAYOUT to be defined, we need to be using
** shared one-or-two cell type_infos (since the type_layouts refer
** to base_type_layouts). USE_TYPE_LAYOUT can be explicitly turned
** off with NO_TYPE_LAYOUT.
**
*/
#if defined(SHARED_ONE_OR_TWO_CELL_TYPE_INFO) && !defined(NO_TYPE_LAYOUT)
	#define USE_TYPE_LAYOUT
#else
	#undef USE_TYPE_LAYOUT
#endif

#if TAGBITS >= 2
	#define make_typelayout(Tag, Value) \
		((Word *) (Integer) mkword(mktag(Tag), Value))
#else
	#define make_typelayout(Tag, Value) \
		((Word *) (Integer) Tag), ((Word *) (Integer) Value)
#endif

/*
** Typelayouts for builtins often defined as 8 indentical
** values (8 because that's the highest number of tag values
** we use at the moment). 
*/

#define make_typelayout_for_all_tags(Tag, Value) \
	make_typelayout(Tag, Value), \
	make_typelayout(Tag, Value), \
	make_typelayout(Tag, Value), \
	make_typelayout(Tag, Value), \
	make_typelayout(Tag, Value), \
	make_typelayout(Tag, Value), \
	make_typelayout(Tag, Value), \
	make_typelayout(Tag, Value)

/* 
** Tags in type_layout structures.
** 
** These definitions are intended for use in handwritten
** C code. 
**
** Some of the type-layout tags are shared.
*/

#define TYPELAYOUT_CONST_TAG		0
#define TYPELAYOUT_COMP_CONST_TAG	0 
#define TYPELAYOUT_SIMPLE_TAG		1
#define TYPELAYOUT_COMPLICATED_TAG	2
#define TYPELAYOUT_EQUIV_TAG		3
#define TYPELAYOUT_NO_TAG		3 

/* 
** Values in type_layout structures,
** presently the values of CONST_TAG words.
**
** Also indended for use in handwritten C code.
**
** Note that TYPELAYOUT_UNASSIGNED_VALUE is not yet
** used for anything.
**
*/

#define TYPELAYOUT_UNASSIGNED_VALUE	((Integer) 0)
#define TYPELAYOUT_UNUSED_VALUE		((Integer) 1)
#define TYPELAYOUT_STRING_VALUE		((Integer) 2)
#define TYPELAYOUT_FLOAT_VALUE		((Integer) 3)
#define TYPELAYOUT_INT_VALUE		((Integer) 4)
#define TYPELAYOUT_CHARACTER_VALUE	((Integer) 5)
#define TYPELAYOUT_UNIV_VALUE		((Integer) 6)
#define TYPELAYOUT_PREDICATE_VALUE	((Integer) 7)

/* 
** Number of defined builtins and reserved values.
*/

#define TYPELAYOUT_BUILTINS		19

/* 
** Highest allowed type variable number
** (corresponds with argument number of type parameter).
*/

#define TYPELAYOUT_MAX_VARINT		1024

#endif
