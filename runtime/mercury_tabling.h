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
/*
** The functions defined here are used only via the macros defined below.
*/

typedef Word	**MR_TrieNode;
typedef Word	**MR_AnswerBlock;

/* functions to handle the builtin types: string, int, float, type_info */

/* 
** Look to see if the given integer key is in the given table. If it
** is, return the address of the data pointer associated with the key.
** If it is not, create a new element for the key in the table and
** return the address of its data pointer.
**/
MR_TrieNode MR_int_hash_lookup_or_add(MR_TrieNode Table, Integer Key);

/* 
** Look to see if the given float key is in the given table. If it
** is return the address of the data pointer associated with the key.
** If it is not create a new element for the key in the table and
** return the address of its data pointer.
**/
MR_TrieNode MR_float_hash_lookup_or_add(MR_TrieNode Table, Float Key);

/* 
** Look to see if the given string key is in the given table. If it
** is return the address of the data pointer associated with the key.
** If it is not create a new element for the key in the table and
** return the address of its data pointer.
**/
MR_TrieNode MR_string_hash_lookup_or_add(MR_TrieNode Table, String Key);

/*
** Lookup or insert the given type_info into the given table. Return a 
** pointer to the node of the table reached by the lookup/insert. 
*/
MR_TrieNode MR_type_info_lookup_or_add(MR_TrieNode, Word *);

/* --- a function to handle enumerated types --- */

/*
**  MR_int_index_lookup_or_add() : This function maintains a simple indexed 
**	table of size Range. The return value is a pointer to the table
** 	node found by the lookup/insert. 
*/
MR_TrieNode MR_int_index_lookup_or_add(MR_TrieNode table, Integer range, Integer key);

/* --- a function to handle any type at all --- */

/*
** This function will lookup or insert any type of value into a 
** table. It uses the provided type_info to extract the necessary
** info to do this. It returns a pointer to the node found by the 
** insertion/lookup.
*/
MR_TrieNode MR_table_type(MR_TrieNode Table, Word *type_info, Word data_value);

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
		MR_TrieNode prev_table = (table);			\
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
		MR_TrieNode prev_table = (table);			\
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
		MR_TrieNode prev_table = (table);			\
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
		MR_TrieNode prev_table = (table);			\
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
		MR_TrieNode prev_table = (table);			\
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
		MR_TrieNode prev_table = (table);			\
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
		MR_TrieNode prev_table = (table);			\
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
		MR_TrieNode prev_table = (table);			\
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
		MR_TrieNode prev_table = (table);			\
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

#ifdef	MR_TABLE_DEBUG

#define MR_TABLE_CREATE_ANSWER_BLOCK(ABlock, Elements)	 		\
	do {								\
		*((MR_AnswerBlock) ABlock) = 				\
			(Word *) table_allocate_words(Elements);	\
		if (MR_tabledebug)					\
			printf("allocated answer block %p -> %p\n",	\
				((MR_AnswerBlock) ABlock),		\
				*((MR_AnswerBlock) ABlock));		\
	} while(0)

#define MR_TABLE_GET_ANSWER(Offset, ABlock)				\
	(( MR_tabledebug ?						\
		(printf("using answer block: %p\n",			\
			((MR_AnswerBlock) ABlock)),			\
		printf("pointing to: %p\n",				\
			*((MR_AnswerBlock) ABlock)))			\
	:								\
		(void) 0 /* do nothing */				\
	),								\
	(* ((MR_AnswerBlock) ABlock))[Offset])

#else

#define MR_TABLE_CREATE_ANSWER_BLOCK(ABlock, Elements)	 		\
	do {								\
		*((MR_AnswerBlock) ABlock) = 				\
			(Word *) table_allocate_words(Elements);	\
	} while(0)

#define MR_TABLE_GET_ANSWER(Offset, ABlock)				\
	(* ((MR_AnswerBlock) ABlock))[Offset]

#endif

#ifdef CONSERVATIVE_GC

  #define MR_TABLE_SAVE_ANSWER(Offset, ABlock, Value, TypeInfo)		\
	do {								\
		(* ((MR_AnswerBlock) ABlock))[Offset] = Value;		\
	} while(0)

#else /* not CONSERVATIVE_GC */

  #define MR_TABLE_SAVE_ANSWER(Offset, ABlock, Value, TypeInfo)		\
	do {								\
		save_transient_hp();					\
		{ Word local_val = Value;				\
		(* ((MR_AnswerBlock) ABlock))[Offset] =			\
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
	MR_memcpy(Dest, Source, Size)

#define table_copy_words(Dest, Source, Size)				\
	MR_memcpy((char *) (Dest), (char *) (Source), sizeof(Word) * Size)

/*---------------------------------------------------------------------------*/

typedef	struct MR_AnswerListNodeStruct	MR_AnswerListNode;
typedef	struct MR_AnswerListNodeStruct	*MR_AnswerList;

struct MR_AnswerListNodeStruct {
	Integer		answer_num;
	Word		answer_data;
	MR_AnswerList	next_answer;
};

typedef enum {
	MR_ANS_NOT_GENERATED,
	MR_ANS_GENERATED
} MR_AnswerDuplState;

/*
** The state of a model_det or model_semi subgoal.
**
** Note that the word containing the MR_SimpletableStatus,
** which is at the end of the chain of trie nodes given by
** the input arguments of the tabled subgoal, will be overwritten
** by a pointer to the answer block containing the output arguments
** when the goal succeeds. The MR_SIMPLETABLE_SUCCEEDED status code
** is used only when the goal has no outputs. This is why
** MR_SIMPLETABLE_SUCCEEDED must the last entry in the enum,
** and why code looking at an MR_SimpletableStatus must test
** for success with "(Unsigned) x >= MR_SIMPLETABLE_SUCCEEDED".
*/

typedef enum {
	MR_SIMPLETABLE_UNINITIALIZED,
	MR_SIMPLETABLE_WORKING,
	MR_SIMPLETABLE_FAILED,
	MR_SIMPLETABLE_SUCCEEDED
} MR_SimpletableStatus;

#ifdef	MR_USE_MINIMAL_MODEL

typedef enum {
   	MR_SUBGOAL_INACTIVE,
	MR_SUBGOAL_ACTIVE,
	MR_SUBGOAL_COMPLETE
} MR_SubgoalStatus;

/*
** The saved state of a generator or a consumer. While consumers get
** suspended while they are waiting for generators to produce more solutions,
** generators need their state saved when they restore the state of a consumer
** to consume a new solution.
**
** The saved state contains copies of
**
** - several virtual machine registers:
**   MR_succip, MR_sp, MR_curfr and MR_maxfr
**
** - segments of the nondet and det stacks:
**   the parts that cannot possibly change between the times of saving
**   and restoring the saved state are not saved.
**
**   The segments are described by three fields each. The *_block_start
**   field gives the address of the first word in the real stack
**   that is part of the saved segment, the *_block_size field
**   gives the size of the saved segment in words, and the *_block
**   field points to the area of memory containing the saved segment.
**
** - the entire generator stack and the entire cut stack:
**   they are usually so small, it is faster to save them all
**   than to figure out which parts need saving.
**
**   Each stack is described by its size in words and a pointer to
**   an area of memory containing the entire saved stack.
*/

typedef struct {
	Code	*succ_ip;
	Word	*s_p;
	Word	*cur_fr;
	Word	*max_fr;
	Word	*non_stack_block_start;
	Word	non_stack_block_size;
	Word	*non_stack_block;
	Word	*det_stack_block_start;
	Word	det_stack_block_size;
	Word	*det_stack_block;
	Integer	gen_next;
	char	*generator_stack_block;
	Integer	cut_next;
	char	*cut_stack_block;
} MR_SavedState;

/* The state of a consumer subgoal */
typedef struct {
	MR_SavedState	saved_state;
	MR_AnswerList	*remaining_answer_list_ptr;
} MR_Consumer;

typedef	struct MR_ConsumerListNode	*MR_ConsumerList;

struct MR_ConsumerListNode {
	MR_Consumer			*item;
	MR_ConsumerList			next;
};

typedef struct MR_SubgoalStruct		MR_Subgoal;
typedef	struct MR_SubgoalListNode	*MR_SubgoalList;

/*
** The following structure is used to hold the state and variables used in 
** the table_resume procedure.
*/

typedef struct {
	MR_SavedState		leader_state;
	MR_SubgoalList		subgoal_list;
	MR_Subgoal		*cur_subgoal;
	MR_ConsumerList		consumer_list;	/* for the current subgoal */
	MR_Consumer		*cur_consumer;
	MR_AnswerList		cur_consumer_answer_list;
	bool			changed;
} MR_ResumeInfo;

struct MR_SubgoalListNode {
	MR_Subgoal		*item;
	MR_SubgoalList		next;
};

/* Used to save info about a single subgoal in the table */
struct MR_SubgoalStruct {
	MR_SubgoalStatus	status;
	MR_Subgoal		*leader;
	MR_SubgoalList		followers;
	MR_SubgoalList		*followers_tail;
	MR_ResumeInfo		*resume_info;
	Word			answer_table;	/* Table of answers returned */
						/* by the subgoal */
	Integer			num_ans;	/* # of answers returned */
						/* by the subgoal */
	Integer			num_committed_ans;
						/* # of answers our leader */
						/* is committed to returning */
						/* to every consumer. */
	MR_AnswerList		answer_list;	/* List of answers returned */
						/* by the subgoal */
	MR_AnswerList		*answer_list_tail;
						/* Pointer to the tail of */
						/* the answer list. This is */
						/* used to update the tail. */
	MR_ConsumerList		consumer_list;	/* List of suspended calls */
						/* to the subgoal */
	MR_ConsumerList		*consumer_list_tail;
						/* As for answer_list_tail */
	Word			*generator_maxfr;
						/* MR_maxfr at the time of */
						/* the call to the generator */
	Word			*generator_sp;
						/* MR_sp at the time of the */
						/* call to the generator */
};

	/* 
	** Cast a Word to a MR_Subgoal*: saves on typing and improves 
	** readability. 
	*/
#define MR_SUBGOAL(T)  (*(MR_Subgoal **) T)

/*---------------------------------------------------------------------------*/

#endif	/* MR_USE_MINIMAL_MODEL */

#endif	/* not MERCURY_TABLING_H */
