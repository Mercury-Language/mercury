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
