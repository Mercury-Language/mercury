/*
** Copyright (C) 1995 University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** type_info.h - defines offsets of fields in the type_info structure.
** See polymorphism.m for explanation of these offsets and how the
** type_info structure is laid out.
*/

#define OFFSET_FOR_COUNT 0
#define OFFSET_FOR_UNIFY_PRED 1
#define OFFSET_FOR_INDEX_PRED 2
#define OFFSET_FOR_COMPARE_PRED 3
#define OFFSET_FOR_TERM_TO_TYPE_PRED 4
#define OFFSET_FOR_TYPE_TO_TERM_PRED 5
/* #define OFFSET_FOR_ARG_TYPE_INFOS 6 */
#define OFFSET_FOR_ARG_TYPE_INFOS 4

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
