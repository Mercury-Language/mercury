/*
** Copyright (C) 1997-2000 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_tabling.h - definitions of some basic stuff used for tabling.
** For tabling code, the Mercury compiler (compiler/table_gen.m) generates
** references to special procedures defined in library/table_builtin.m.
** The types and macros defined here are used by the procedures defined in
** library/table_builtin.m.
*/

#ifndef	MERCURY_TABLING_H
#define	MERCURY_TABLING_H

#include "mercury_types.h"
#include "mercury_type_info.h"
#include "mercury_float.h"
#include "mercury_reg_workarounds.h"
#include "mercury_dlist.h"

#ifndef MR_CONSERVATIVE_GC
  #include "mercury_deep_copy.h"
#endif

#include <stdio.h>

/*---------------------------------------------------------------------------*/

/*
** Forward declarations of type names.
*/

typedef	union MR_TableNode_Union		MR_TableNode;
typedef	struct MR_HashTable_Struct		MR_HashTable;
typedef	struct MR_Subgoal_Struct		MR_Subgoal;
typedef	struct MR_SubgoalListNode_Struct	MR_SubgoalListNode;
typedef	struct MR_AnswerListNode_Struct		MR_AnswerListNode;
typedef	struct MR_ConsumerListNode_Struct	MR_ConsumerListNode;

typedef MR_TableNode				*MR_TrieNode;
typedef	MR_SubgoalListNode			*MR_SubgoalList;
typedef	MR_AnswerListNode			*MR_AnswerList;
typedef	MR_ConsumerListNode			*MR_ConsumerList;

/*---------------------------------------------------------------------------*/

/*
** Tabling builds up two kinds of tables, both conceptually tries. For call
** tables, there is one layer in the trie for each input argument; for answer
** tables, there is one layer in the trie for each output argument. However,
** the way each trie node is implemented depends on the type of the relevant
** argument. In addition, what is stored at the tips of the call and answer
** tables also depends on what kind of tabling (e.g. loopcheck, memo, minimal
** model) is being performed on the current predicate, and (in some cases)
** on what stage the execution of the current predicate has reached.
**
** We declare trie nodes to have type MR_TrieNode, which is a pointer to
** MR_TableNode. MR_TableNode is a union of all the types that we may need
** to be able to store in trie nodes: various kinds of trie implementations,
** status indications, and answer blocks. Since in several places we write
** to the union through one member and read from it through another, it is
** important that all members be the same size; this is why the simple table
** status field is an (unsigned) integer, not an enum.
**
** The integer field is for generic code that does not know what kind of node
** the node will be; this means initialization. A value of zero means the node
** is uninitialized; this must be true for all members. (Also, see below on
** duplicate detection.)
**
** The hash table field is used when the "trie" node is implemented with a
** hash table, whether of ints, floats, strings or another type that can be
** coerced to one of these types.
**
** The fix table field implements a true trie node of fixed size, simply
** indexed by an integer.
**
** The start table field implements a dynamically expandable trie node,
** simply indexed by the difference between an integer value and a start value.
**
** The MR_simpletable_status member of the union gives the status of a
** model_det or model_semi subgoal; it should be interpreted using the
** MR_SIMPLETABLE_* macros below. Note that this word, which is at the end of
** the chain of trie nodes given by the input arguments of the tabled subgoal,
** will be overwritten by a pointer to the answer block containing the output
** arguments when the goal succeeds; the MR_SIMPLETABLE_SUCCEEDED status code
** is used only when the goal has no outputs, and thus no answer block.
** This is why MR_SIMPLETABLE_SUCCEEDED must have the highest value, and
** why code looking at MR_simpletable_status must test for success with
** "table->MR_simpletable_status >= MR_SIMPLETABLE_SUCCEEDED".
**
** The subgoal field contains the status of a model_non subgoal.
**
** The answer block field contains a pointer to an array of words, with
** one word per output argument.
**
** The hash table, fix table and start table members may appear at any interior
** node in the trie. The simple table status and subgoal members only appear
** at the tips of call tables. The answer block member appears only at the tips
** of call tables, either directly (for model_det and model_semi procedures),
** or indirectly inside answer lists (for model_non procedures). There are no
** answer tables for model_det and model_semi procedures, since they can only
** ever have at most one answer. You can of course have answer tables for
** model_non procedures, at whose tips you find only a duplicate indication.
** When the tip nodes of answer tables are created, they are initialized to
** zero as usual. Duplicate checking checks that the tip node is zero and
** then sets the tip to a nonzero value; this way if the answer is generated
** again, duplicate checking will fail.
**
** Note that once a tabled predicate has inserted its input arguments into
** its table and got back a pointer to the MR_TableNode representing the
** selected tip of its call table, it may in general call other tabled
** predicates and cause insertions into many tables, including its own,
** before it updates the call table tip node. This means that the tip node
** must not change address; once a tabling operation has returned an
** MR_TrieNode to its caller, that address must be valid and have the same
** meaning until the end of the computation.
**
** The implementation of start tables currently does not obey this requirement.
** This is okay, for two reasons. First, start tables are not yet used. Second,
** when they are used, they will be used by I/O tabling, which guarantees that
** there will be no insertions into the same (or any other) table between
** getting back a tip node on the one hand and updating it and releasing the
** pointer to it on the other hand.
** 
** NOTE: the mercury_type_tables module uses the expandable hash table routines
** defined in this module to implement its tables. This is the only use of the
** MR_type_table field.
*/

union MR_TableNode_Union {
	MR_Integer	MR_integer;
	MR_HashTable	*MR_hash_table;
	MR_TableNode	*MR_fix_table;
	MR_TableNode	*MR_start_table;
	MR_Unsigned	MR_simpletable_status;
	MR_Subgoal	*MR_subgoal;
	MR_Word		*MR_answerblock;
	MR_Dlist	*MR_type_table;
};

#define	MR_SIMPLETABLE_UNINITIALIZED	0
#define	MR_SIMPLETABLE_WORKING		1
#define	MR_SIMPLETABLE_FAILED		2
#define	MR_SIMPLETABLE_SUCCEEDED	3

typedef enum {
   	MR_SUBGOAL_INACTIVE,
	MR_SUBGOAL_ACTIVE,
	MR_SUBGOAL_COMPLETE
} MR_SubgoalStatus;

struct MR_AnswerListNode_Struct {
	MR_Integer	answer_num;
	MR_TableNode	answer_data; /* always uses the MR_answerblock member */
	MR_AnswerList	next_answer;
};

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
	MR_Code			*succ_ip;
	MR_Word			*s_p;
	MR_Word			*cur_fr;
	MR_Word			*max_fr;
	MR_Word			*non_stack_block_start;
	MR_Word			non_stack_block_size;
	MR_Word			*non_stack_block;
	MR_Word			*det_stack_block_start;
	MR_Word			det_stack_block_size;
	MR_Word			*det_stack_block;
	MR_Integer		gen_next;
	char			*generator_stack_block;
	MR_Integer		cut_next;
	char			*cut_stack_block;
} MR_SavedState;

/* The state of a consumer subgoal */
typedef struct {
	MR_SavedState		saved_state;
	MR_AnswerList		*remaining_answer_list_ptr;
} MR_Consumer;

struct MR_ConsumerListNode_Struct {
	MR_Consumer		*item;
	MR_ConsumerList		next;
};

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
	MR_bool			changed;
} MR_ResumeInfo;

struct MR_SubgoalListNode_Struct {
	MR_Subgoal		*item;
	MR_SubgoalList		next;
};

/* Used to save info about a single subgoal in the table */
struct MR_Subgoal_Struct {
	MR_SubgoalStatus	status;
	MR_Subgoal		*leader;
	MR_SubgoalList		followers;
	MR_SubgoalList		*followers_tail;
	MR_ResumeInfo		*resume_info;
	MR_Word			answer_table;	/* Table of answers returned */
						/* by the subgoal */
	MR_Integer		num_ans;	/* # of answers returned */
						/* by the subgoal */
	MR_Integer		num_committed_ans;
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
	MR_Word			*generator_maxfr;
						/* MR_maxfr at the time of */
						/* the call to the generator */
	MR_Word			*generator_sp;
						/* MR_sp at the time of the */
						/* call to the generator */
};

/*---------------------------------------------------------------------------*/

/*
** The functions defined here should be used only via the macros defined
** in mercury_tabling_macros.h.
**
** These functions look to see if the given key is in the given table.
** If it is, they return the address of the data pointer associated with
** the key. If it is not, they create a new element for the key in the table
** and return the address of its data pointer.
*/

/*
** These functions assume that the table is a dynamically resizable hash table.
*/

extern	MR_TrieNode	MR_int_hash_lookup_or_add(MR_TrieNode table,
				MR_Integer key);
extern	MR_TrieNode	MR_float_hash_lookup_or_add(MR_TrieNode table,
				MR_Float key);
extern	MR_TrieNode	MR_string_hash_lookup_or_add(MR_TrieNode table,
				MR_ConstString key);

/*
** This function assumes that the table is a statically sized array,
** with the index ranging from 0 to range - 1.
*/

extern	MR_TrieNode	MR_int_fix_index_lookup_or_add(MR_TrieNode table,
				MR_Integer range, MR_Integer key);

/*
** This function assumes that the table is an expandable array,
** with the smallest valid index value being start.
*/

extern	MR_TrieNode	MR_int_start_index_lookup_or_add(MR_TrieNode table,
				MR_Integer start, MR_Integer key);

/*
** This function tables type_infos in a hash table.
*/

extern	MR_TrieNode	MR_type_info_lookup_or_add(MR_TrieNode table,
				MR_TypeInfo type_info);

/*
** This function tables typeclass_infos in a hash table.
*/

extern	MR_TrieNode	MR_type_class_info_lookup_or_add(MR_TrieNode table,
				MR_Word *type_class_info);

/*
** This function tables values of arbitrary types; the form of the data
** structure depends on the actual type of the value.
*/

extern	MR_TrieNode	MR_table_type(MR_TrieNode table,
				MR_TypeInfo type_info, MR_Word data_value);

/*
** This function prints statistics about the operation of tabling, if the
** collection of such statistics is enabled, on the given stream.
*/

extern	void		MR_table_report_statistics(FILE *fp);

/*---------------------------------------------------------------------------*/

#ifndef MR_NATIVE_GC

  #define MR_TABLE_NEW(type)						\
	MR_GC_NEW(type)

  #define MR_TABLE_NEW_ARRAY(type, count)				\
	MR_GC_NEW_ARRAY(type, (count))

  #define MR_TABLE_RESIZE_ARRAY(ptr, type, count)			\
	MR_GC_RESIZE_ARRAY((ptr), type, (count))

  #define MR_table_allocate_bytes(size)					\
	MR_GC_malloc((size))

  #define MR_table_reallocate_bytes(pointer, size)			\
	MR_GC_realloc((pointer), (size))

  #define MR_table_allocate_words(size)					\
	MR_GC_malloc(sizeof(MR_Word) * (size))

  #define MR_table_reallocate_words(pointer, size)			\
	MR_GC_realloc((pointer), sizeof(MR_Word) * (size))

  #define MR_table_free(pointer)					\
	MR_GC_free((pointer))

  #define MR_table_list_cons(h, t)					\
	MR_list_cons((h), (t))

#else /* MR_NATIVE_GC */

  #define MR_TABLE_NEW(type)						\
	(MR_fatal_error("Sorry, not implemented: tabling in native gc grades"), \
	(void *) NULL)
  #define MR_TABLE_NEW_ARRAY(type, count)				\
	(MR_fatal_error("Sorry, not implemented: tabling in native gc grades"), \
	(void *) NULL)
  #define MR_TABLE_RESIZE_ARRAY(pointer, type, count)			\
	(MR_fatal_error("Sorry, not implemented: tabling in native gc grades"), \
	(void *) NULL)
  #define MR_table_allocate_bytes(size)					\
	(MR_fatal_error("Sorry, not implemented: tabling in native gc grades"), \
	(void *) NULL)
  #define MR_table_reallocate_bytes(pointer, size)				\
	(MR_fatal_error("Sorry, not implemented: tabling in native gc grades"), \
	(void *) NULL)
  #define MR_table_allocate_words(size)					\
	(MR_fatal_error("Sorry, not implemented: tabling in native gc grades"), \
	(void *) NULL)
  #define MR_table_reallocate_words(pointer, size)				\
	(MR_fatal_error("Sorry, not implemented: tabling in native gc grades"), \
	(void *) NULL)
  #define MR_table_free(pointer)						\
	MR_fatal_error("Sorry, not implemented: tabling in native gc grades")
  #define MR_table_list_cons(h, t)					\
	(MR_fatal_error("Sorry, not implemented: tabling in native gc grades"), \
	(MR_Word) 0)

#endif /* MR_NATIVE_GC */

#define MR_table_copy_bytes(dest, source, size)				\
	MR_memcpy((dest), (source), (size))

#define MR_table_copy_words(dest, source, size)				\
	MR_memcpy((char *) (dest), (char *) (source), sizeof(MR_Word) * (size))

/*---------------------------------------------------------------------------*/

#include "mercury_tabling_macros.h"

#endif	/* not MERCURY_TABLING_H */
