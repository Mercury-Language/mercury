/*
** Copyright (C) 1997-2000,2002-2004 The University of Melbourne.
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
#include "mercury_goto.h"		/* for MR_declare_entry */
#include "mercury_stack_layout.h"	/* for MR_Proc_Layout */

#ifndef MR_CONSERVATIVE_GC
  #include "mercury_deep_copy.h"
#endif

#include <stdio.h>

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
** The MR_loop_status member of the union gives the status of a loopcheck
** subgoal, it should be interpreted using the MR_LOOP_* macros below.
**
** The MR_memo_status member of the union gives the status of a memo subgoal,
** it should be interpreted using the MR_MEMO_* macros below. Note that this
** word, which is at the end of the chain of trie nodes given by the input
** arguments of the tabled subgoal, will be overwritten by a pointer to the
** answer block containing the output arguments when the goal succeeds;
** the MR_MEMO_SUCCEEDED status code is used only when the goal has no
** outputs, and thus no answer block. This is why code looking at
** MR_memo_status must consider "table->MR_memo_status >= MR_MEMO_BLOCK"
** to be the same as MR_MEMO_SUCCEEDED. The value MR_MEMO_FAILED is last,
** because the types memo_det_status and memo_semi_status Mercury types
** are implemented by the first three and four MR_MEMO_* macros respectively.
**
** The subgoal field contains the state of a model_non subgoal.
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
** This is okay, because start tables are used only by I/O tabling, which
** guarantees that there will be no insertions into the same (or any other)
** table between getting back a tip node on the one hand and updating it and
** releasing the pointer to it on the other hand.
** 
** NOTE: the mercury_type_tables module uses the expandable hash table routines
** defined in this module to implement its tables. This is the only use of the
** MR_type_table field.
*/

/* these macros are used to interpret the MR_loop_status field */
#define	MR_LOOP_INACTIVE	0
#define	MR_LOOP_ACTIVE		1

/* these macros are used to interpret the MR_memo_status field */
#define	MR_MEMO_INACTIVE	0
#define	MR_MEMO_ACTIVE		1
#define	MR_MEMO_SUCCEEDED	2
#define	MR_MEMO_FAILED		3
#define	MR_MEMO_BLOCK		4

typedef enum {
	MR_MEMO_NON_INACTIVE,
	MR_MEMO_NON_ACTIVE,
	MR_MEMO_NON_INCOMPLETE,
	MR_MEMO_NON_COMPLETE
} MR_MemoNonStatus;

typedef enum {
	MR_SUBGOAL_INACTIVE,
	MR_SUBGOAL_ACTIVE,
	MR_SUBGOAL_COMPLETE
} MR_SubgoalStatus;

struct MR_AnswerListNode_Struct {
	MR_Word			*MR_aln_answer_block;
	MR_AnswerList		MR_aln_next_answer;
};

union MR_TableNode_Union {
	MR_Integer		MR_integer;
	MR_HashTable		*MR_hash_table;
	MR_TableNode		*MR_fix_table;
	MR_TableNode		*MR_start_table;
	MR_Unsigned		MR_loop_status;
	MR_Unsigned		MR_memo_status;
	MR_Subgoal		*MR_subgoal;
	MR_MemoNonRecordPtr	MR_memo_non_record;
	MR_Consumer		*MR_consumer;
	MR_AnswerBlock		MR_answerblock;
	MR_Dlist		*MR_type_table;
};

struct MR_MemoNonRecord_Struct {
	MR_TrieNode		MR_mn_back_ptr;
	MR_MemoNonStatus	MR_mn_status;
	int			MR_mn_num_answers;
	MR_TableNode		MR_mn_answer_table;
	MR_AnswerList		MR_mn_answer_list;
	MR_AnswerList		*MR_mn_answer_list_tail;
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
** These functions look to see if the given key is in the given table.
** If it is, they return the address of the data pointer associated with
** the key. If it is not, they return NULL.
**
** These functions assume that the table is a dynamically resizable hash table.
*/

extern	MR_TrieNode	MR_int_hash_lookup(MR_TrieNode table,
				MR_Integer key);
extern	MR_TrieNode	MR_float_hash_lookup(MR_TrieNode table,
				MR_Float key);
extern	MR_TrieNode	MR_string_hash_lookup(MR_TrieNode table,
				MR_ConstString key);

/*
** These functions return a dynamically resizable array (using the primitives
** in mercury_array_macros.h) containing all the elements in the given
** dynamically resizable hash table.
*/

extern	MR_bool		MR_get_int_hash_table_contents(MR_TrieNode t,
				MR_Integer **values_ptr, int *value_next_ptr);
extern	MR_bool		MR_get_float_hash_table_contents(MR_TrieNode t,
				MR_Float **values_ptr, int *value_next_ptr);
extern	MR_bool		MR_get_string_hash_table_contents(MR_TrieNode t,
				MR_ConstString **values_ptr,
				int *value_next_ptr);

/*
** This function prints statistics about the operation of tabling, if the
** collection of such statistics is enabled, on the given stream.
*/

extern	void		MR_table_report_statistics(FILE *fp);

/*
** These functions return printable representations of the MR_loop_status
** MR_memo_status and MR_mn_status fields.
*/

extern	const char	*MR_loopcheck_status(MR_Unsigned);
extern	const char	*MR_memo_status(MR_Unsigned);
extern	const char	*MR_memo_non_status(MR_MemoNonStatus);

/*
** These functions print the tips of the call tables for loopcheck and memo
** tabled predicates to fp.
*/

extern	void		MR_print_loopcheck_tip(FILE *fp,
				const MR_Proc_Layout *proc, MR_TrieNode table);
extern	void		MR_print_memo_tip(FILE *fp,
				const MR_Proc_Layout *proc, MR_TrieNode table);
extern	void		MR_print_memo_non_record(FILE *fp,
				const MR_Proc_Layout *proc,
				MR_MemoNonRecordPtr record);

/*
** Prints the given answer_block of the given procedure to fp.
*/

extern	void		MR_print_answerblock(FILE *fp,
				const MR_Proc_Layout *proc,
				MR_Word *answer_block);

/*---------------------------------------------------------------------------*/

#ifndef MR_NATIVE_GC

  #define MR_TABLE_NEW(type)						\
	MR_GC_NEW(type)

  #define MR_TABLE_NEW_ARRAY(type, count)				\
	MR_GC_NEW_ARRAY(type, (count))

  #define MR_TABLE_RESIZE_ARRAY(ptr, type, count)			\
	MR_GC_RESIZE_ARRAY((ptr), type, (count))

#if 0
  #define MR_table_allocate_bytes(size)					\
	MR_GC_malloc((size))

  #define MR_table_reallocate_bytes(pointer, size)			\
	MR_GC_realloc((pointer), (size))
#endif

  #define MR_table_allocate_words(size)					\
	((MR_Word *) MR_GC_malloc(sizeof(MR_Word) * (size)))

  #define MR_table_reallocate_words(pointer, size)			\
	(MR_CHECK_EXPR_TYPE((pointer), MR_Word *),			\
	(MR_Word *) MR_GC_realloc((pointer), sizeof(MR_Word) * (size)))

  #define MR_table_allocate_struct(type)				\
	((type *) MR_GC_malloc(sizeof(type)))

  #define MR_table_allocate_structs(num, type)				\
	((type *) MR_GC_malloc(sizeof(type) * (num)))

  #define MR_table_reallocate_structs(pointer, num, type)		\
	(MR_CHECK_EXPR_TYPE((pointer), type *),				\
	(type *) MR_GC_realloc((pointer), sizeof(type) * (num)))

  #define MR_table_free(pointer)					\
	MR_GC_free((pointer))

#else /* MR_NATIVE_GC */

  #define MR_TABLE_NATIVE_GC_MSG					\
	"Sorry, not implemented: tabling in native gc grades"

  #define MR_TABLE_NEW(type)						\
	(MR_fatal_error(MR_TABLE_NATIVE_GC_MSG),			\
	(void *) NULL)
  #define MR_TABLE_NEW_ARRAY(type, count)				\
	(MR_fatal_error(MR_TABLE_NATIVE_GC_MSG),			\
	(void *) NULL)
  #define MR_TABLE_RESIZE_ARRAY(pointer, type, count)			\
	(MR_fatal_error(MR_TABLE_NATIVE_GC_MSG),			\
	(void *) NULL)
#if 0
  #define MR_table_allocate_bytes(size)					\
	(MR_fatal_error(MR_TABLE_NATIVE_GC_MSG),			\
	(void *) NULL)
  #define MR_table_reallocate_bytes(pointer, size)			\
	(MR_fatal_error(MR_TABLE_NATIVE_GC_MSG),			\
	(void *) NULL)
#endif
  #define MR_table_allocate_words(size)					\
	(MR_fatal_error(MR_TABLE_NATIVE_GC_MSG), 			\
	(void *) NULL)
  #define MR_table_reallocate_words(pointer, size)			\
	(MR_fatal_error(MR_TABLE_NATIVE_GC_MSG), 			\
	(void *) NULL)
  #define MR_table_allocate_struct(type)				\
	(MR_fatal_error(MR_TABLE_NATIVE_GC_MSG), 			\
	(void *) NULL)
  #define MR_table_allocate_structs(num, type)				\
	(MR_fatal_error(MR_TABLE_NATIVE_GC_MSG), 			\
	(void *) NULL)
  #define MR_table_reallocate_structs(pointer, num, type)		\
	(MR_fatal_error(MR_TABLE_NATIVE_GC_MSG), 			\
	(void *) NULL)
  #define MR_table_free(pointer)					\
	MR_fatal_error(MR_TABLE_NATIVE_GC_MSG)

#endif /* MR_NATIVE_GC */

#define MR_table_copy_bytes(dest, source, size)				\
	MR_memcpy((dest), (source), (size))

#define MR_table_copy_words(dest, source, size)				\
	(MR_CHECK_EXPR_TYPE((dest), MR_Word *),				\
	(MR_CHECK_EXPR_TYPE((source), MR_Word *),			\
	MR_memcpy((char *) (dest), (char *) (source),			\
		sizeof(MR_Word) * (size))))

#define MR_table_copy_structs(dest, source, num, type)			\
	(MR_CHECK_EXPR_TYPE((dest), type *),				\
	(MR_CHECK_EXPR_TYPE((source), type *),				\
	MR_memcpy((char *) (dest), (char *) (source),			\
		sizeof(type) * (num))))

/*---------------------------------------------------------------------------*/

#ifdef	MR_HIGHLEVEL_CODE
    #ifdef MR_USE_GCC_NESTED_FUNCTIONS
        extern void MR_CALL
            mercury__table_builtin__table_memo_return_all_answers_multi_2_p_0(
                MR_Box record, MR_Box *answer_block_ptr,
		MR_NestedCont cont);

	extern void MR_CALL
	    mercury__table_builtin__table_memo_return_all_answers_nondet_2_p_0(
	        MR_Box record, MR_Box *answer_block_ptr,
		MR_NestedCont cont);
    #else	/* ! MR_USE_GCC_NESTED_FUNCTIONS */
        extern void MR_CALL
             mercury__table_builtin__table_memo_return_all_answers_multi_2_p_0(
                MR_Box record, MR_Box *answer_block_ptr,
                MR_Cont cont, void *cont_env_ptr);
        
	extern void MR_CALL
              mercury__table_builtin__table_memo_return_all_answers_nondet_2_p_0(
                MR_Box record, MR_Box *answer_block_ptr,
                MR_Cont cont, void *cont_env_ptr);
    #endif /* MR_USE_GCC_NESTED_FUNCTIONS */

#else	/* ! MR_HIGHLEVEL_CODE */
  #define MR_MEMO_NON_RET_ALL_NONDET_ENTRY				\
	MR_proc_entry_user_name(table_builtin,				\
		table_memo_return_all_answers_nondet, 2, 0)
  #define MR_MEMO_NON_RET_ALL_MULTI_ENTRY				\
	MR_proc_entry_user_name(table_builtin,				\
		table_memo_return_all_answers_multi, 2, 0)

  MR_declare_entry(MR_MEMO_NON_RET_ALL_NONDET_ENTRY);
  MR_declare_entry(MR_MEMO_NON_RET_ALL_MULTI_ENTRY);
#endif	/* MR_HIGHLEVEL_CODE */

/*---------------------------------------------------------------------------*/

#include "mercury_tabling_macros.h"
#include "mercury_tabling_preds.h"

#endif	/* not MERCURY_TABLING_H */
