/*
** Copyright (C) 1997-1999 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_tabling.h - definitions of some basic stuff used for tabling.
** For tabling code, the Mercury compiler (compiler/table_gen.m) generates
** references to special procedures defined in library/private_builtin.m.
** The types and macros defined here are used by the procedures defined in
** library/private_builtin.m.
*/

#ifndef	MERCURY_TABLING_H
#define	MERCURY_TABLING_H

#include "mercury_types.h"
#include "mercury_float.h"

/*---------------------------------------------------------------------------*/

typedef Word	**TrieNode;
typedef Word	**AnswerBlock;

/*---------------------------------------------------------------------------*/
/*
** The functions defined here are used only via the macros defined below.
*/

/* functions to handle the builtin types: string, int, float, type_info */

/* 
** Look to see if the given integer key is in the given table. If it
** is, return the address of the data pointer associated with the key.
** If it is not, create a new element for the key in the table and
** return the address of its data pointer.
**/
TrieNode MR_int_hash_lookup_or_add(TrieNode Table, Integer Key);

/* 
** Look to see if the given float key is in the given table. If it
** is return the address of the data pointer associated with the key.
** If it is not create a new element for the key in the table and
** return the address of its data pointer.
**/
TrieNode MR_float_hash_lookup_or_add(TrieNode Table, Float Key);

/* 
** Look to see if the given string key is in the given table. If it
** is return the address of the data pointer associated with the key.
** If it is not create a new element for the key in the table and
** return the address of its data pointer.
**/
TrieNode MR_string_hash_lookup_or_add(TrieNode Table, String Key);

/*
** Lookup or insert the given type_info into the given table. Return a 
** pointer to the node of the table reached by the lookup/insert. 
*/
TrieNode MR_type_info_lookup_or_add(TrieNode, Word *);

/* --- a function to handle enumerated types --- */

/*
**  MR_int_index_lookup_or_add() : This function maintains a simple indexed 
**	table of size Range. The return value is a pointer to the table
** 	node found by the lookup/insert. 
*/
TrieNode MR_int_index_lookup_or_add(TrieNode table, Integer range, Integer key);

/* --- a function to handle any type at all --- */

/*
** This function will lookup or insert any type of value into a 
** table. It uses the provided type_info to extract the necessary
** info to do this. It returns a pointer to the node found by the 
** insertion/lookup.
*/
TrieNode MR_table_type(TrieNode Table, Word *type_info, Word data_value);

/*---------------------------------------------------------------------------*/

#define MR_RAW_TABLE_ANY(Table, TypeInfo, Value)			\
	MR_table_type(Table, (Word *) TypeInfo, Value)

#define MR_RAW_TABLE_TAG(Table, Tag)					\
	MR_int_index_lookup_or_add(Table, 1 << TAGBITS, Tag)

#define MR_RAW_TABLE_ENUM(Table, Range, Value)				\
	MR_int_index_lookup_or_add(Table, Range, Value)

#define MR_RAW_TABLE_WORD(Table, Value)					\
	MR_int_hash_lookup_or_add(Table, (Integer) Value);

#define MR_RAW_TABLE_INT(Table, Value)					\
	MR_int_hash_lookup_or_add(Table, Value);

#define MR_RAW_TABLE_CHAR(Table, Value)					\
	MR_int_hash_lookup_or_add(Table, (Integer) Value);

#define MR_RAW_TABLE_FLOAT(Table, Value)				\
	MR_float_hash_lookup_or_add(Table, Value);

#define MR_RAW_TABLE_STRING(Table, Value)	 			\
	MR_string_hash_lookup_or_add(Table, (String) Value);

#define MR_RAW_TABLE_TYPE_INFO(Table, Type)				\
	MR_type_info_lookup_or_add(Table, (Word *) Type)

#ifdef	MR_TABLE_DEBUG

#define	MR_DEBUG_NEW_TABLE_ANY(table, table0, type_info, value)		\
	do {								\
		(table) = (Word) MR_RAW_TABLE_ANY((Word **) (table0),	\
					(type_info), (value));		\
		if (MR_tabledebug) {					\
			printf("TABLE %p: any %x type %p => %p\n",	\
				(Word **) (table0), (value), 		\
				(Word **) (type_info), (Word **) (table));\
		}							\
	} while (0)
#define	MR_DEBUG_TABLE_ANY(table, type_info, value)			\
	do {								\
		TrieNode prev_table = (table);				\
		(table) = (Word **) MR_RAW_TABLE_ANY((table), 		\
					(type_info), (value));		\
		if (MR_tabledebug) {					\
			printf("TABLE %p: any %x type %p => %p\n",	\
				prev_table, (value), (type_info),	\
				(table));				\
		}							\
	} while (0)

#define	MR_DEBUG_NEW_TABLE_TAG(table, table0, value)			\
	do {								\
		(table) = (Word) MR_RAW_TABLE_TAG((Word **) (table0),	\
					(value));			\
		if (MR_tabledebug) {					\
			printf("TABLE %p: tag %d => %p\n", 		\
				(Word **) (table0), (value), 		\
				(Word **) (table))			\
		}							\
	} while (0)
#define	MR_DEBUG_TABLE_TAG(table, value)				\
	do {								\
		TrieNode prev_table = (table);				\
		(table) = (Word **) MR_RAW_TABLE_TAG((table), (value));	\
		if (MR_tabledebug) {					\
			printf("TABLE %p: tag %d => %p\n", prev_table,	\
				(value), (table));			\
		}							\
	} while (0)

#define	MR_DEBUG_NEW_TABLE_ENUM(table, table0, count, value)		\
	do {								\
		(table) = (Word) MR_RAW_TABLE_ENUM((Word **) (table0),	\
					(count), (value));		\
		if (MR_tabledebug) {					\
			printf("TABLE %p: enum %d of %d => %p\n", 	\
				(Word **) (table0), (value), (count),	\
				(Word **) (table));			\
		}							\
	} while (0)
#define	MR_DEBUG_TABLE_ENUM(table, count, value)			\
	do {								\
		TrieNode prev_table = (table);				\
		(table) = (Word **) MR_RAW_TABLE_ENUM((table), (count),	\
					(value));			\
		if (MR_tabledebug) {					\
			printf("TABLE %p: enum %d of %d => %p\n", 	\
				prev_table, (value), (count), (table));	\
		}							\
	} while (0)

#define	MR_DEBUG_NEW_TABLE_WORD(table, table0, value)			\
	do {								\
		(table) = (Word) MR_RAW_TABLE_WORD((Word **) (table0),	\
					(value));			\
		if (MR_tabledebug) {					\
			printf("TABLE %p: word %d => %p\n",		\
				(Word **) (table0), (value),		\
				(Word **) (table));			\
		}							\
	} while (0)
#define	MR_DEBUG_TABLE_WORD(table, value)				\
	do {								\
		TrieNode prev_table = (table);				\
		(table) = (Word **) MR_RAW_TABLE_WORD((table), (value));\
		if (MR_tabledebug) {					\
			printf("TABLE %p: word %d => %p\n",		\
				prev_table, (value), (table));		\
		}							\
	} while (0)

#define	MR_DEBUG_NEW_TABLE_INT(table, table0, value)			\
	do {								\
		(table) = (Word) MR_RAW_TABLE_INT((Word **) (table0),	\
					(value));			\
		if (MR_tabledebug) {					\
			printf("TABLE %p: int %d => %p\n",		\
				(Word **) (table0), (value),		\
				(Word **) (table));			\
		}							\
	} while (0)
#define	MR_DEBUG_TABLE_INT(table, value)				\
	do {								\
		TrieNode prev_table = (table);				\
		(table) = (Word **) MR_RAW_TABLE_INT((table), (value));	\
		if (MR_tabledebug) {					\
			printf("TABLE %p: int %d => %p\n",		\
				prev_table, (value), (table));		\
		}							\
	} while (0)

#define	MR_DEBUG_NEW_TABLE_CHAR(table, table0, value)			\
	do {								\
		(table) = (Word) MR_RAW_TABLE_CHAR((Word **) (table0),	\
					(value));			\
		if (MR_tabledebug) {					\
			printf("TABLE %p: char `%c'/%d => %p\n",	\
				(Word **) (table0), (int) (value),	\
				(int) (value), (Word **) (table));	\
		}							\
	} while (0)
#define	MR_DEBUG_TABLE_CHAR(table, value)				\
	do {								\
		TrieNode prev_table = (table);				\
		(table) = (Word **) MR_RAW_TABLE_CHAR((table), (value));\
		if (MR_tabledebug) {					\
			printf("TABLE %p: char `%c'/%d => %p\n",	\
				prev_table, (int) (value), 		\
				(int) (value), (table));		\
		}							\
	} while (0)

#define	MR_DEBUG_NEW_TABLE_FLOAT(table, table0, value)			\
	do {								\
		(table) = (Word) MR_RAW_TABLE_FLOAT((Word **) (table0),	\
					(value));			\
		if (MR_tabledebug) {					\
			printf("TABLE %p: float %f => %p\n",		\
				(Word **) (table0), value,		\
				(Word **) (table));			\
		}							\
	} while (0)
#define	MR_DEBUG_TABLE_FLOAT(table, value)				\
	do {								\
		TrieNode prev_table = (table);				\
		(table) = (Word **) MR_RAW_TABLE_FLOAT((table), (value));\
		if (MR_tabledebug) {					\
			printf("TABLE %p: float %f => %p\n",		\
				prev_table, (double) word_to_float(value),\
				(table));				\
		}							\
	} while (0)

#define	MR_DEBUG_NEW_TABLE_STRING(table, table0, value)			\
	do {								\
		(table) = (Word) MR_RAW_TABLE_STRING((Word **) (table0),\
					(value));			\
		if (MR_tabledebug) {					\
			printf("TABLE %p: string `%s' => %p\n",		\
				(Word **) (table), (char *) (value),	\
				(Word **) (table));			\
		}							\
	} while (0)
#define	MR_DEBUG_TABLE_STRING(table, value)				\
	do {								\
		TrieNode prev_table = (table);				\
		(table) = (Word **) MR_RAW_TABLE_STRING((table), (value));\
		if (MR_tabledebug) {					\
			printf("TABLE %p: string `%s' => %p\n",		\
				prev_table, (char *) (value), (table));	\
		}							\
	} while (0)

#define	MR_DEBUG_NEW_TABLE_TYPEINFO(table, table0, value)		\
	do {								\
		(table) = (Word) MR_RAW_TABLE_TYPE_INFO((Word **) (table0),\
					(value));			\
		if (MR_tabledebug) {					\
			printf("TABLE %p: typeinfo %p => %p\n",		\
				(Word **) (table), (Word **) (value), 	\
				(Word **) (table));			\
		}							\
	} while (0)
#define	MR_DEBUG_TABLE_TYPEINFO(table, value)				\
	do {								\
		TrieNode prev_table = (table);				\
		(table) = (Word **) MR_RAW_TABLE_TYPE_INFO((table), (value));\
		if (MR_tabledebug) {					\
			printf("TABLE %p: typeinfo %p => %p\n",		\
				prev_table, (value), (table));		\
		}							\
	} while (0)

#else	/* not MR_TABLE_DEBUG */

#define	MR_DEBUG_NEW_TABLE_ANY(table, table0, type_info, value)		\
	do {								\
		(table) = (Word) MR_RAW_TABLE_ANY((Word **) (table0),	\
					(type_info), (value));		\
	} while (0)
#define	MR_DEBUG_TABLE_ANY(table, type_info, value)			\
	do {								\
		(table) = (Word **) MR_RAW_TABLE_ANY((table),		\
					(type_info), (value));		\
	} while (0)

#define	MR_DEBUG_NEW_TABLE_TAG(table, table0, value)			\
	do {								\
		(table) = (Word) MR_RAW_TABLE_TAG((Word **) (table0),	\
					(value));			\
	} while (0)
#define	MR_DEBUG_TABLE_TAG(table, value)				\
	do {								\
		(table) = (Word **) MR_RAW_TABLE_TAG((table), (value));	\
	} while (0)

#define	MR_DEBUG_NEW_TABLE_ENUM(table, table0, count, value)		\
	do {								\
		(table) = (Word) MR_RAW_TABLE_ENUM((Word **) (table0),	\
					(count), (value));		\
	} while (0)
#define	MR_DEBUG_TABLE_ENUM(table, count, value)			\
	do {								\
		(table) = (Word **) MR_RAW_TABLE_ENUM((table), (count),	\
					(value));			\
	} while (0)

#define	MR_DEBUG_NEW_TABLE_WORD(table, table0, value)			\
	do {								\
		(table) = (Word) MR_RAW_TABLE_WORD((Word **) (table0),	\
					(value));			\
	} while (0)
#define	MR_DEBUG_TABLE_WORD(table, value)				\
	do {								\
		(table) = (Word **) MR_RAW_TABLE_WORD((table), (value));\
	} while (0)

#define	MR_DEBUG_NEW_TABLE_INT(table, table0, value)			\
	do {								\
		(table) = (Word) MR_RAW_TABLE_INT((Word **) (table0),	\
					(value));			\
	} while (0)
#define	MR_DEBUG_TABLE_INT(table, value)				\
	do {								\
		(table) = (Word **) MR_RAW_TABLE_INT((table), (value));	\
	} while (0)

#define	MR_DEBUG_NEW_TABLE_CHAR(table, table0, value)			\
	do {								\
		(table) = (Word) MR_RAW_TABLE_CHAR((Word **) (table0),	\
					(value));			\
	} while (0)
#define	MR_DEBUG_TABLE_CHAR(table, value)				\
	do {								\
		(table) = (Word **) MR_RAW_TABLE_CHAR((table), (value));\
	} while (0)

#define	MR_DEBUG_NEW_TABLE_FLOAT(table, table0, value)			\
	do {								\
		(table) = (Word) MR_RAW_TABLE_FLOAT((Word **) (table0),	\
					(value));			\
	} while (0)
#define	MR_DEBUG_TABLE_FLOAT(table, value)				\
	do {								\
		(table) = (Word **) MR_RAW_TABLE_FLOAT((table), (value));\
	} while (0)

#define	MR_DEBUG_NEW_TABLE_STRING(table, table0, value)			\
	do {								\
		(table) = (Word) MR_RAW_TABLE_STRING((Word **) (table0),\
					(value));			\
	} while (0)
#define	MR_DEBUG_TABLE_STRING(table, value)				\
	do {								\
		(table) = (Word **) MR_RAW_TABLE_STRING((table), (value));\
	} while (0)

#define	MR_DEBUG_NEW_TABLE_TYPEINFO(table, table0, value)		\
	do {								\
		(table) = (Word) MR_RAW_TABLE_TYPE_INFO((Word **) (table0),\
					(value));			\
	} while (0)
#define	MR_DEBUG_TABLE_TYPEINFO(table, value)				\
	do {								\
		(table) = (Word **) MR_RAW_TABLE_TYPE_INFO((table), (value));\
	} while (0)

#endif	/* MR_TABLE_DEBUG */

/***********************************************************************/

#define MR_TABLE_CREATE_ANSWER_BLOCK(ABlock, Elements)	 		\
	do {								\
		*((AnswerBlock) ABlock) = 				\
			(Word *) table_allocate_words(Elements);	\
	} while(0)

#define MR_TABLE_GET_ANSWER(Offset, ABlock)				\
	(* ((AnswerBlock) ABlock))[Offset]

#ifdef CONSERVATIVE_GC

  #define MR_TABLE_SAVE_ANSWER(Offset, ABlock, Value, TypeInfo)		\
	do {								\
		(* ((AnswerBlock) ABlock))[Offset] = Value;		\
	} while(0)

#else /* not CONSERVATIVE_GC */

  #define MR_TABLE_SAVE_ANSWER(Offset, ABlock, Value, TypeInfo)		\
	do {								\
		save_transient_hp();					\
		{ Word local_val = Value;				\
		(* ((AnswerBlock) ABlock))[Offset] = 			\
			deep_copy(&local_val, (Word *) (Word) &TypeInfo,\
				NULL, NULL);				\
		}							\
		restore_transient_hp();					\
	} while(0)

#endif /* CONSERVATIVE_GC */

#ifdef CONSERVATIVE_GC

  #define table_allocate_bytes(size)					\
	GC_MALLOC(size)

  #define table_reallocate_bytes(pointer, size)				\
	GC_REALLOC(pointer, size)

  #define table_allocate_words(size)					\
	GC_MALLOC(sizeof(Word) * size)

  #define table_reallocate_words(pointer, size)				\
	GC_REALLOC(pointer, sizeof(Word) * size)

  #define table_free(pointer)						\
	GC_FREE(pointer)

  #define MR_table_list_cons(h, t) list_cons((h), (t))

#else /* not CONSERVATIVE_GC */

  #define table_allocate_bytes(Size)					\
	(fatal_error("Sorry, not implemented: tabling in non-GC grades"), \
	(void *) NULL)
  #define table_reallocate_bytes(Pointer, Size)				\
	(fatal_error("Sorry, not implemented: tabling in non-GC grades"), \
	(void *) NULL)
  #define table_allocate_words(Size)					\
	(fatal_error("Sorry, not implemented: tabling in non-GC grades"), \
	(void *) NULL)
  #define table_reallocate_words(Pointer, Size)				\
	(fatal_error("Sorry, not implemented: tabling in non-GC grades"), \
	(void *) NULL)
  #define table_free(Pointer)						\
	fatal_error("Sorry, not implemented: tabling in non-GC grades")

  #define MR_table_list_cons(h, t)					\
	(fatal_error("Sorry, not implemented: tabling in non-GC grades"), \
	(Word) 0)

#endif /* CONSERVATIVE_GC */

#define table_copy_bytes(Dest, Source, Size)				\
	memcpy(Dest, Source, Size)

#define table_copy_words(Dest, Source, Size)				\
	memcpy((char *) Dest, (char *) Source, sizeof(Word) * Size)

#endif /* not MERCURY_TABLING_H */
