/*
** Copyright (C) 1997-2000,2002 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_tabling_macros.h
**
** This file defines macros used by the implementation of tabling
** (which means mostly the procedures defined in library/private_builtin.m).
** These macros just call the real implementation routines defined in
** runtime/mercury_tabling.c, but they also optionally print debugging
** information.
*/

#include "mercury_deep_copy.h"	/* for MR_make_permanent */

#define MR_RAW_TABLE_ANY(table, type_info, value)			\
	MR_table_type((table), (type_info), (value))

#define MR_RAW_TABLE_TAG(table, tag)					\
	MR_int_fix_index_lookup_or_add((table), 1 << MR_TAGBITS, (tag))

#define MR_RAW_TABLE_ENUM(table, range, value)				\
	MR_int_fix_index_lookup_or_add((table), (range), (value))

#define MR_RAW_TABLE_START_INT(table, start, value)			\
	MR_int_start_index_lookup_or_add((table), (start), (value));

#define MR_RAW_TABLE_WORD(table, value)					\
	MR_int_hash_lookup_or_add((table), (value));

#define MR_RAW_TABLE_INT(table, value)					\
	MR_int_hash_lookup_or_add((table), (value));

#define MR_RAW_TABLE_CHAR(table, value)					\
	MR_int_hash_lookup_or_add((table), (value));

#define MR_RAW_TABLE_FLOAT(table, value)				\
	MR_float_hash_lookup_or_add((table), (value));

#define MR_RAW_TABLE_STRING(table, value)	 			\
	MR_string_hash_lookup_or_add((table), (value));

#define MR_RAW_TABLE_TYPEINFO(table, type_info)				\
	MR_type_info_lookup_or_add((table), (type_info))

#define MR_RAW_TABLE_TYPECLASSINFO(table, typeclass_info)		\
	MR_type_info_lookup_or_add((table), (typeclass_info))

#ifdef	MR_TABLE_DEBUG

#define	MR_DEBUG_NEW_TABLE_ANY(table, table0, type_info, value)		\
	do {								\
		(table) = MR_RAW_TABLE_ANY((table0), (type_info),	\
					   (value));			\
		if (MR_tabledebug) {					\
			printf("TABLE %p: any %x type %p => %p\n",	\
				(table0), (value), (type_info), (table));\
		}							\
	} while (0)
#define	MR_DEBUG_TABLE_ANY(table, type_info, value)			\
	do {								\
		MR_TrieNode prev_table = (table);			\
		(table) = MR_RAW_TABLE_ANY((table), 		\
					(type_info), (value));		\
		if (MR_tabledebug) {					\
			printf("TABLE %p: any %x type %p => %p\n",	\
				prev_table, (value), (type_info),	\
				(table));				\
		}							\
	} while (0)

#define	MR_DEBUG_NEW_TABLE_TAG(table, table0, value)			\
	do {								\
		(table) = MR_RAW_TABLE_TAG((table0), (value));		\
		if (MR_tabledebug) {					\
			printf("TABLE %p: tag %d => %p\n", 		\
				(table0), (value), (table))		\
		}							\
	} while (0)
#define	MR_DEBUG_TABLE_TAG(table, value)				\
	do {								\
		MR_TrieNode prev_table = (table);			\
		(table) = MR_RAW_TABLE_TAG((table), (value));		\
		if (MR_tabledebug) {					\
			printf("TABLE %p: tag %d => %p\n",		\
				 prev_table, (value), (table));		\
		}							\
	} while (0)

#define	MR_DEBUG_NEW_TABLE_ENUM(table, table0, count, value)		\
	do {								\
		(table) = MR_RAW_TABLE_ENUM((table0), (count), (value));\
		if (MR_tabledebug) {					\
			printf("TABLE %p: enum %d of %d => %p\n", 	\
				(table0), (value), (count), (table));	\
		}							\
	} while (0)
#define	MR_DEBUG_TABLE_ENUM(table, count, value)			\
	do {								\
		MR_TrieNode prev_table = (table);			\
		(table) = MR_RAW_TABLE_ENUM((table), (count), (value));	\
		if (MR_tabledebug) {					\
			printf("TABLE %p: enum %d of %d => %p\n", 	\
				prev_table, (value), (count), (table));	\
		}							\
	} while (0)

#define	MR_DEBUG_NEW_TABLE_START_INT(table, table0, start, value)	\
	do {								\
		(table) = MR_RAW_TABLE_START_INT((table0), (start), (value));\
		if (MR_tabledebug) {					\
			printf("TABLE %p: int %d - %d => %p\n",		\
				(table0), (value), (start), (table));	\
		}							\
	} while (0)
#define	MR_DEBUG_TABLE_START_INT(table, start, value)			\
	do {								\
		MR_TrieNode prev_table = (table);			\
		(table) = MR_RAW_TABLE_START_INT((table), (start), (value));\
		if (MR_tabledebug) {					\
			printf("TABLE %p: int %d - %d => %p\n",		\
				prev_table, (value), (start), (table));	\
		}							\
	} while (0)

#define	MR_DEBUG_NEW_TABLE_WORD(table, table0, value)			\
	do {								\
		(table) = MR_RAW_TABLE_WORD((table0), (value));		\
		if (MR_tabledebug) {					\
			printf("TABLE %p: word %d => %p\n",		\
				(table0), (value), (table));		\
		}							\
	} while (0)
#define	MR_DEBUG_TABLE_WORD(table, value)				\
	do {								\
		MR_TrieNode prev_table = (table);			\
		(table) = MR_RAW_TABLE_WORD((table), (value));		\
		if (MR_tabledebug) {					\
			printf("TABLE %p: word %d => %p\n",		\
				prev_table, (value), (table));		\
		}							\
	} while (0)

#define	MR_DEBUG_NEW_TABLE_INT(table, table0, value)			\
	do {								\
		(table) = MR_RAW_TABLE_INT((table0), (value));		\
		if (MR_tabledebug) {					\
			printf("TABLE %p: int %d => %p\n",		\
				(table0), (value), (table));		\
		}							\
	} while (0)
#define	MR_DEBUG_TABLE_INT(table, value)				\
	do {								\
		MR_TrieNode prev_table = (table);			\
		(table) = MR_RAW_TABLE_INT((table), (value));		\
		if (MR_tabledebug) {					\
			printf("TABLE %p: int %d => %p\n",		\
				prev_table, (value), (table));		\
		}							\
	} while (0)

#define	MR_DEBUG_NEW_TABLE_CHAR(table, table0, value)			\
	do {								\
		(table) = MR_RAW_TABLE_CHAR((table0), (value));		\
		if (MR_tabledebug) {					\
			printf("TABLE %p: char `%c'/%d => %p\n",	\
				(table0), (int) (value),		\
				(int) (value), (table));		\
		}							\
	} while (0)
#define	MR_DEBUG_TABLE_CHAR(table, value)				\
	do {								\
		MR_TrieNode prev_table = (table);			\
		(table) = MR_RAW_TABLE_CHAR((table), (value));		\
		if (MR_tabledebug) {					\
			printf("TABLE %p: char `%c'/%d => %p\n",	\
				prev_table, (int) (value), 		\
				(int) (value), (table));		\
		}							\
	} while (0)

#define	MR_DEBUG_NEW_TABLE_FLOAT(table, table0, value)			\
	do {								\
		(table) = MR_RAW_TABLE_FLOAT((table0), (value));	\
		if (MR_tabledebug) {					\
			printf("TABLE %p: float %f => %p\n",		\
				(table0), (double) (value), (table));	\
		}							\
	} while (0)
#define	MR_DEBUG_TABLE_FLOAT(table, value)				\
	do {								\
		MR_TrieNode prev_table = (table);			\
		(table) = MR_RAW_TABLE_FLOAT((table), (value));		\
		if (MR_tabledebug) {					\
			printf("TABLE %p: float %f => %p\n",		\
				prev_table, (double) value, (table));	\
		}							\
	} while (0)

#define	MR_DEBUG_NEW_TABLE_STRING(table, table0, value)			\
	do {								\
		(table) = MR_RAW_TABLE_STRING((table0), (value));	\
		if (MR_tabledebug) {					\
			printf("TABLE %p: string `%s' => %p\n",		\
				(table), (char *) (value), (table));	\
		}							\
	} while (0)
#define	MR_DEBUG_TABLE_STRING(table, value)				\
	do {								\
		MR_TrieNode prev_table = (table);			\
		(table) = MR_RAW_TABLE_STRING((table), (value));	\
		if (MR_tabledebug) {					\
			printf("TABLE %p: string `%s' => %p\n",		\
				prev_table, (char *) (value), (table));	\
		}							\
	} while (0)

#define	MR_DEBUG_NEW_TABLE_TYPEINFO(table, table0, value)		\
	do {								\
		(table) = MR_RAW_TABLE_TYPEINFO((table0), (value));	\
		if (MR_tabledebug) {					\
			printf("TABLE %p: typeinfo %p => %p\n",		\
				(table), (value), (table));		\
		}							\
	} while (0)
#define	MR_DEBUG_TABLE_TYPEINFO(table, value)				\
	do {								\
		MR_TrieNode prev_table = (table);			\
		(table) = MR_RAW_TABLE_TYPEINFO((table), (value));	\
		if (MR_tabledebug) {					\
			printf("TABLE %p: typeinfo %p => %p\n",		\
				prev_table, (value), (table));		\
		}							\
	} while (0)

#define	MR_DEBUG_NEW_TABLE_TYPECLASSINFO(table, table0, value)		\
	do {								\
		(table) = MR_RAW_TABLE_TYPECLASSINFO((table0), (value));\
		if (MR_tabledebug) {					\
			printf("TABLE %p: typeclassinfo %p => %p\n",	\
				(table), (value), (table));		\
		}							\
	} while (0)
#define	MR_DEBUG_TABLE_TYPECLASSINFO(table, value)			\
	do {								\
		MR_TrieNode prev_table = (table);			\
		(table) = MR_RAW_TABLE_TYPECLASSINFO((table), (value));	\
		if (MR_tabledebug) {					\
			printf("TABLE %p: typeclassinfo %p => %p\n",	\
				prev_table, (value), (table));		\
		}							\
	} while (0)

#else	/* not MR_TABLE_DEBUG */

#define	MR_DEBUG_NEW_TABLE_ANY(table, table0, type_info, value)		\
	do {								\
		(table) = MR_RAW_TABLE_ANY((table0), (type_info), (value));\
	} while (0)
#define	MR_DEBUG_TABLE_ANY(table, type_info, value)			\
	do {								\
		(table) = MR_RAW_TABLE_ANY((table), (type_info), (value));\
	} while (0)

#define	MR_DEBUG_NEW_TABLE_TAG(table, table0, value)			\
	do {								\
		(table) = MR_RAW_TABLE_TAG((table0), (value));		\
	} while (0)
#define	MR_DEBUG_TABLE_TAG(table, value)				\
	do {								\
		(table) = MR_RAW_TABLE_TAG((table), (value));		\
	} while (0)

#define	MR_DEBUG_NEW_TABLE_ENUM(table, table0, count, value)		\
	do {								\
		(table) = MR_RAW_TABLE_ENUM((table0), (count), (value));\
	} while (0)
#define	MR_DEBUG_TABLE_ENUM(table, count, value)			\
	do {								\
		(table) = MR_RAW_TABLE_ENUM((table), (count), (value));	\
	} while (0)

#define	MR_DEBUG_NEW_TABLE_START_INT(table, table0, start, value)	\
	do {								\
		(table) = MR_RAW_TABLE_START_INT((table0), (start), (value));\
	} while (0)
#define	MR_DEBUG_TABLE_START_INT(table, start, value)			\
	do {								\
		(table) = MR_RAW_TABLE_START_INT((table), (start), (value));\
	} while (0)

#define	MR_DEBUG_NEW_TABLE_WORD(table, table0, value)			\
	do {								\
		(table) = MR_RAW_TABLE_WORD((table0), (value));		\
	} while (0)
#define	MR_DEBUG_TABLE_WORD(table, value)				\
	do {								\
		(table) = MR_RAW_TABLE_WORD((table), (value));\
	} while (0)

#define	MR_DEBUG_NEW_TABLE_INT(table, table0, value)			\
	do {								\
		(table) = MR_RAW_TABLE_INT((table0), (value));		\
	} while (0)
#define	MR_DEBUG_TABLE_INT(table, value)				\
	do {								\
		(table) = MR_RAW_TABLE_INT((table), (value));		\
	} while (0)

#define	MR_DEBUG_NEW_TABLE_CHAR(table, table0, value)			\
	do {								\
		(table) = MR_RAW_TABLE_CHAR((table0), (value));		\
	} while (0)
#define	MR_DEBUG_TABLE_CHAR(table, value)				\
	do {								\
		(table) = MR_RAW_TABLE_CHAR((table), (value));		\
	} while (0)

#define	MR_DEBUG_NEW_TABLE_FLOAT(table, table0, value)			\
	do {								\
		(table) = MR_RAW_TABLE_FLOAT((table0), (value));	\
	} while (0)
#define	MR_DEBUG_TABLE_FLOAT(table, value)				\
	do {								\
		(table) = MR_RAW_TABLE_FLOAT((table), (value));		\
	} while (0)

#define	MR_DEBUG_NEW_TABLE_STRING(table, table0, value)			\
	do {								\
		(table) = MR_RAW_TABLE_STRING((table0), (value));	\
	} while (0)
#define	MR_DEBUG_TABLE_STRING(table, value)				\
	do {								\
		(table) = MR_RAW_TABLE_STRING((table), (value));	\
	} while (0)

#define	MR_DEBUG_NEW_TABLE_TYPEINFO(table, table0, value)		\
	do {								\
		(table) = MR_RAW_TABLE_TYPEINFO((table0), (value));	\
	} while (0)
#define	MR_DEBUG_TABLE_TYPEINFO(table, value)				\
	do {								\
		(table) = MR_RAW_TABLE_TYPEINFO((table), (value));	\
	} while (0)

#define	MR_DEBUG_NEW_TABLE_TYPECLASSINFO(table, table0, value)		\
	do {								\
		(table) = MR_RAW_TABLE_TYPECLASSINFO((table0), (value));\
	} while (0)
#define	MR_DEBUG_TABLE_TYPECLASSINFO(table, value)			\
	do {								\
		(table) = MR_RAW_TABLE_TYPECLASSINFO((table), (value));	\
	} while (0)

#endif	/* MR_TABLE_DEBUG */

/***********************************************************************/

#ifdef	MR_TABLE_DEBUG

#define MR_TABLE_CREATE_ANSWER_BLOCK(table, num_slots)	 		\
	do {								\
		(table)->MR_answerblock = MR_TABLE_NEW_ARRAY(MR_Word,	\
						(num_slots));		\
		if (MR_tabledebug)					\
			printf("allocated answer block %p -> %p, %d words\n",\
				(table), (table)->MR_answerblock,	\
				(int) (num_slots));			\
	} while(0)

#define MR_TABLE_GET_ANSWER(table, offset)				\
	(( MR_tabledebug ?						\
		printf("using answer block: %p -> %p, slot %d\n",	\
			(table), (table)->MR_answerblock,		\
			(int) (offset))					\
	:								\
		(void) 0 /* do nothing */				\
	),								\
	((table)->MR_answerblock)[(offset)])

#define MR_TABLE_SAVE_ANSWER(table, offset, value, type_info)		\
	do {								\
		if (MR_tabledebug)					\
			printf("saving to answer block: %p -> %p, "	\
				"slot %d = %lx\n",			\
				(table), (table)->MR_answerblock,	\
				(int) (offset), (long) (value));	\
		(table)->MR_answerblock[offset] =			\
			MR_make_permanent((value),			\
					(MR_TypeInfo) (type_info));	\
	} while(0)

#else

#define MR_TABLE_CREATE_ANSWER_BLOCK(table, num_slots)	 		\
	do {								\
		(table)->MR_answerblock = MR_TABLE_NEW_ARRAY(MR_Word,	\
						(num_slots));		\
	} while(0)

#define MR_TABLE_GET_ANSWER(table, offset)				\
	((table)->MR_answerblock)[(offset)]

#define MR_TABLE_SAVE_ANSWER(table, offset, value, type_info)		\
	do {								\
		(table)->MR_answerblock[offset] =			\
			MR_make_permanent((value),			\
					(MR_TypeInfo) (type_info));	\
	} while(0)

#endif
