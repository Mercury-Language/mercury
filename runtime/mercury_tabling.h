// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1997-2000,2002-2007 The University of Melbourne.
// This file may only be copied under the terms of the GNU Library General
// Public License - see the file COPYING.LIB in the Mercury distribution.

// mercury_tabling.h - definitions of some basic stuff used for tabling.
// For tabling code, the Mercury compiler (compiler/table_gen.m) generates
// references to special procedures defined in library/table_builtin.m.
// The types and macros defined here are used by the procedures defined in
// library/table_builtin.m.

#ifndef MERCURY_TABLING_H
#define MERCURY_TABLING_H

#include "mercury_types.h"
#include "mercury_bitmap.h"
#include "mercury_type_info.h"
#include "mercury_float.h"
#include "mercury_reg_workarounds.h"
#include "mercury_dlist.h"
#include "mercury_goto.h"       // for MR_declare_entry
#include "mercury_tags.h"       // for `MR_DEFINE_BUILTIN_ENUM_CONST'

#ifndef MR_CONSERVATIVE_GC
  #include "mercury_deep_copy.h"
#endif

#include <stdio.h>

#ifdef  MR_TABLE_DEBUG
#define MR_TABLE_DEBUG_BOOL MR_TRUE
#else
#define MR_TABLE_DEBUG_BOOL MR_FALSE
#endif

////////////////////////////////////////////////////////////////////////////

// Tabling builds up two kinds of tables, both conceptually tries. For call
// tables, there is one layer in the trie for each input argument; for answer
// tables, there is one layer in the trie for each output argument. However,
// the way each trie node is implemented depends on the type of the relevant
// argument. In addition, what is stored at the tips of the call and answer
// tables also depends on what kind of tabling (e.g. loopcheck, memo, minimal
// model) is being performed on the current predicate, and (in some cases)
// on what stage the execution of the current predicate has reached.
//
// We declare trie nodes to have type MR_TrieNode, which is a pointer to
// MR_TableNode. MR_TableNode is a union of all the types that we may need
// to be able to store in trie nodes: various kinds of trie implementations,
// status indications, and answer blocks. Since in several places we write
// to the union through one member and read from it through another, it is
// important that all members be the same size; this is why the simple table
// status field is an (unsigned) integer, not an enum.
//
// The integer field is for generic code that does not know what kind of node
// the node will be; this means initialization. A value of zero means the node
// is uninitialized; this must be true for all members. (Also, see below on
// duplicate detection.)
//
// The hash table field is used when the "trie" node is implemented with a
// hash table, whether of ints, floats, strings or another type that can be
// coerced to one of these types.
//
// The fix table field implements a true trie node of fixed size, simply
// indexed by an integer.
//
// The start table field implements a dynamically expandable trie node,
// simply indexed by the difference between an integer value and a start value.
//
// The MR_loop_status member of the union gives the status of a loopcheck
// subgoal, it should be interpreted using the MR_LOOP_* macros below.
//
// The MR_memo_status member of the union gives the status of a memo subgoal,
// it should be interpreted using the MR_MEMO_* macros below. Note that this
// word, which is at the end of the chain of trie nodes given by the input
// arguments of the tabled subgoal, will be overwritten by a pointer to the
// answer block containing the output arguments when the goal succeeds;
// the MR_MEMO_SUCCEEDED status code is used only when the goal has no
// outputs, and thus no answer block. This is why code looking at
// MR_memo_status must consider "table->MR_memo_status >= MR_MEMO_BLOCK"
// to be the same as MR_MEMO_SUCCEEDED. The value MR_MEMO_FAILED is last,
// because the types memo_det_status and memo_semi_status Mercury types
// are implemented by the first three and four MR_MEMO_* macros respectively.
//
// The subgoal field contains the state of a model_non subgoal.
//
// The answer block field contains a pointer to an array of words, with
// one word per output argument.
//
// The hash table, fix table and start table members may appear at any interior
// node in the trie. The simple table status and subgoal members only appear
// at the tips of call tables. The answer block member appears only at the tips
// of call tables, either directly (for model_det and model_semi procedures),
// or indirectly inside answer lists (for model_non procedures). There are no
// answer tables for model_det and model_semi procedures, since they can only
// ever have at most one answer. You can of course have answer tables for
// model_non procedures, at whose tips you find only a duplicate indication.
// When the tip nodes of answer tables are created, they are initialized to
// zero as usual. Duplicate checking checks that the tip node is zero and
// then sets the tip to a nonzero value; this way if the answer is generated
// again, duplicate checking will fail.
//
// Note that once a tabled predicate has inserted its input arguments into
// its table and got back a pointer to the MR_TableNode representing the
// selected tip of its call table, it may in general call other tabled
// predicates and cause insertions into many tables, including its own,
// before it updates the call table tip node. This means that the tip node
// must not change address; once a tabling operation has returned an
// MR_TrieNode to its caller, that address must be valid and have the same
// meaning until the end of the computation.
//
// The implementation of start tables currently does not obey this requirement.
// This is okay, because start tables are used only by I/O tabling, which
// guarantees that there will be no insertions into the same (or any other)
// table between getting back a tip node on the one hand and updating it and
// releasing the pointer to it on the other hand.
//
// NOTE: the mercury_type_tables module uses the expandable hash table routines
// defined in this module to implement its tables. This is the only use of the
// MR_type_table field.

// these macros are used to interpret the MR_loop_status field
#define MR_LOOP_INACTIVE    0
#define MR_LOOP_ACTIVE      1

// these macros are used to interpret the MR_memo_status field
#define MR_MEMO_INACTIVE    0
#define MR_MEMO_ACTIVE      1
#define MR_MEMO_SUCCEEDED   2
#define MR_MEMO_FAILED      3
#define MR_MEMO_BLOCK       4

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
    MR_Word             *MR_aln_answer_block;
    MR_AnswerList       MR_aln_next_answer;
};

union MR_TableNode_Union {
    MR_Integer          MR_integer;
    MR_HashTable        *MR_hash_table;
    MR_TableNode        *MR_fix_table;
    MR_TableNode        *MR_start_table;
    MR_Unsigned         MR_loop_status;
    MR_Unsigned         MR_memo_status;
    MR_Subgoal          *MR_subgoal;
    MR_MemoNonRecordPtr MR_memo_non_record;
    MR_GeneratorPtr     MR_generator;
    MR_AnswerBlock      MR_answerblock;
    MR_Dlist            *MR_type_table;
};

#define MR_trie_node_seen_before(t)         ((t)->MR_integer != 0)

struct MR_MemoNonRecord_Struct {
    MR_TrieNode         MR_mn_back_ptr;
    MR_MemoNonStatus    MR_mn_status;
    int                 MR_mn_num_answers;
    MR_TableNode        MR_mn_answer_table;
    MR_AnswerList       MR_mn_answer_list;
    MR_AnswerList       *MR_mn_answer_list_tail;
};

// The MR_ProcTableInfo structure.
//
// To enable debugging (especially performance debugging) of tabled predicates,
// the compiler generates one of these structures for each tabled predicate
// (except I/O primitives, for which it generates an MR_TableIoEntry
// structure).
//
// Each argument of a tabled predicate is an input or an output. Inputs are put
// into the call trie (stored in the tablenode field), which has one level
// per input argument. The structure of each level depends on what kind of type
// the corresponding input argument is; this is recorded in the input_steps
// field, which points to an array of size num_inputs. If the type is an enum,
// we cannot interpret the data structures on that level without also knowing
// how many alternatives the type has; this is recorded in the corresponding
// element of the enum_params array, which is likewise of size num_inputs.
// (Elements of the enum_params array that correspond to arguments whose types
// are not enums are not meaningful.)
//
// The ptis field points to an array of pseudotypeinfos of size num_inputs +
// num_outputs. The first num_inputs elements give the types of the input
// arguments, while the remaining num_outputs elements give the types of the
// output arguments. The type_params field describes where any typeinfos
// among the input arguments are at call, since without this information
// the debugger cannot turn the pseudotypeinfos pointed to by ptis field info
// typeinfos.
//
// If the collection of statistics was not enabled for this table, then the
// stats field will point to an array num_inputs MR_TableStepStats structures,
// one for each input argument. Each element of this array contains statistics
// about the corresponding level of the trie.
//
// Users can use the stats field to retrieve statistics derived from the
// entire lifetime of the table so far. To enable users to derive information
// derived only since the last such lookup, we record the information retrieved
// on each lookup in the prev_stats field (which will be NULL until the first
// such lookup).
//
// If there is no size limit on the table, then the size_limit field will be
// zero and the call_table_tips, num_call_table_tips and next_to_evict fields
// are not meaningful. If there is a size limit on the table, then the
// size_limit field says how many call table tips are allowed to exist at
// any one time, the num_call_table_tips field says how many exist at this time
// (this number will be between zero and size_limit, both inclusive),
// the call_table_tips field will point to an array of size_limit call table
// tips, of which the first num_call_table_tips will be meaningful. The
// next_to_evict field says which one of these entries is scheduled to be
// evicted next under the FIFO replacement strategy.
//
// XXX We need other fields (e.g. in hash tables and tries) to allow us
// to delete internal nodes of the trie that become empty after evictions.

typedef enum {
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TABLE_TYPE_LOOPCHECK),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TABLE_TYPE_MEMO),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TABLE_TYPE_MINIMAL_MODEL_STACK_COPY),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TABLE_TYPE_MINIMAL_MODEL_OWN_STACKS)
} MR_TableType;

// The definition of this type should correspond to the type table_step_kind
// in library/table_statistics.m.

typedef enum {
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TABLE_STEP_DUMMY),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TABLE_STEP_INT),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TABLE_STEP_UINT),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TABLE_STEP_CHAR),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TABLE_STEP_STRING),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TABLE_STEP_FLOAT),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TABLE_STEP_ENUM),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TABLE_STEP_FOREIGN_ENUM),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TABLE_STEP_GEN),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TABLE_STEP_GEN_ADDR),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TABLE_STEP_GEN_POLY),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TABLE_STEP_GEN_POLY_ADDR),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TABLE_STEP_TYPEINFO),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TABLE_STEP_TYPECLASSINFO),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TABLE_STEP_PROMISE_IMPLIED),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TABLE_STEP_INT8),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TABLE_STEP_UINT8),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TABLE_STEP_INT16),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TABLE_STEP_UINT16),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TABLE_STEP_INT32),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TABLE_STEP_UINT32),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TABLE_STEP_INT64),
    MR_DEFINE_BUILTIN_ENUM_CONST(MR_TABLE_STEP_UINT64)
} MR_TableTrieStep;

typedef MR_Unsigned MR_Counter;

typedef enum {
    MR_TABLE_STATS_DETAIL_HASH,
    MR_TABLE_STATS_DETAIL_ENUM,
    MR_TABLE_STATS_DETAIL_START,
    MR_TABLE_STATS_DETAIL_DU,
    MR_TABLE_STATS_DETAIL_POLY,
    MR_TABLE_STATS_DETAIL_NONE
} MR_TableStepStatsKind;

struct MR_TableStepStats_Struct {
    MR_Counter              MR_tss_num_lookups;
    MR_Counter              MR_tss_num_lookups_is_dupl;
    MR_TableStepStatsKind   MR_tss_detail_kind;

    MR_Counter              MR_tss_hash_num_table_allocs;
    MR_Counter              MR_tss_hash_num_table_alloc_bytes;
    MR_Counter              MR_tss_hash_num_link_chunk_allocs;
    MR_Counter              MR_tss_hash_num_link_chunk_alloc_bytes;
    MR_Counter              MR_tss_hash_num_key_compares_not_dupl;
    MR_Counter              MR_tss_hash_num_key_compares_dupl;
    MR_Counter              MR_tss_hash_num_resizes;
    MR_Counter              MR_tss_hash_resize_old_entries;
    MR_Counter              MR_tss_hash_resize_new_entries;

    MR_Counter              MR_tss_enum_num_node_allocs;
    MR_Counter              MR_tss_enum_num_node_alloc_bytes;

    MR_Counter              MR_tss_du_num_node_allocs;
    MR_Counter              MR_tss_du_num_node_alloc_bytes;
    MR_Counter              MR_tss_du_num_arg_lookups;
    MR_Counter              MR_tss_du_num_exist_lookups;

    MR_Counter              MR_tss_start_num_allocs;
    MR_Counter              MR_tss_start_num_alloc_bytes;
};

struct MR_TableStats_Struct {
    MR_Counter              MR_ts_num_lookups;
    MR_Counter              MR_ts_num_lookups_is_dupl;
    // The number of steps is given by MR_pt_num_inputs for the call table
    // and MR_pt_num_outputs for the answer table.

    MR_TableStepStats       *MR_ts_steps;
};

#define MR_TABLE_CALL_TABLE 0
#define MR_TABLE_ANSWER_TABLE   1

#define MR_TABLE_STATS_CURR 0
#define MR_TABLE_STATS_PREV 1

struct MR_TableStepDesc_Struct {
    MR_ConstString          MR_tsd_var_name;
    MR_TableTrieStep        MR_tsd_trie_step;
    MR_Integer              MR_tsd_trie_enum_param;
};

struct MR_ProcTableInfo_Struct {
    MR_TableType            MR_pt_table_type;
    int                     MR_pt_num_inputs;
    int                     MR_pt_num_outputs;
    int                     MR_pt_has_answer_table;
    const MR_PseudoTypeInfo *MR_pt_ptis;
    const MR_TypeParamLocns *MR_pt_type_params;

    MR_TableNode            MR_pt_tablenode;

    // The index should be either MR_TABLE_CALL_TABLE or
    // MR_TABLE_ANSWER_TABLE. The size of the pointed-to array is
    // MR_pt_num_inputs for MR_TABLE_CALL_TABLE and MR_pt_num_outputs
    // for MR_TABLE_ANSWER_TABLE.

    const MR_TableStepDesc  *MR_pt_steps_desc[2];

    // The first index should be either MR_TABLE_CALL_TABLE or
    // MR_TABLE_ANSWER_TABLE, while the second index should be either
    // MR_TABLE_STATS_CURR or MR_TABLE_STATS_PREV.

    MR_TableStats           MR_pt_stats[2][2];

    MR_Unsigned             MR_pt_size_limit;
    MR_TrieNode             *MR_pt_call_table_tips;
    MR_Unsigned             MR_pt_num_call_table_tips;
    MR_Unsigned             MR_pt_next_to_evict;
};

// This type is only for backward compatibility.
typedef struct MR_Table_Gen_Struct {
    int                     MR_table_gen_num_inputs;
    int                     MR_table_gen_num_outputs;
    const MR_TableTrieStep  *MR_table_gen_input_steps;
    const MR_Integer        *MR_table_gen_enum_params;
    const MR_PseudoTypeInfo *MR_table_gen_ptis;
    const MR_TypeParamLocns *MR_table_gen_type_params;
} MR_Table_Gen;

////////////////////////////////////////////////////////////////////////////

// The functions defined here should be used only via the macros defined
// in mercury_tabling_macros.h.
//
// These functions look to see if the given key is in the given table.
// If it is, they return the address of the data pointer associated with
// the key. If it is not, they create a new element for the key in the table
// and return the address of its data pointer.

// These functions assume that the table is a dynamically resizable hash table.

extern  MR_TrieNode MR_int_hash_lookup_or_add(MR_TrieNode table,
                        MR_Integer key);
extern  MR_TrieNode MR_int_hash_lookup_or_add_stats(
                        MR_TableStepStats *stats, MR_TrieNode table,
                        MR_Integer key);
extern  MR_TrieNode MR_int64_hash_lookup_or_add(MR_TrieNode table,
                        int64_t key);
extern  MR_TrieNode MR_int64_hash_lookup_or_add_stats(
                        MR_TableStepStats *stats, MR_TrieNode table,
                        int64_t key);
extern  MR_TrieNode MR_uint64_hash_lookup_or_add(MR_TrieNode table,
                        uint64_t key);
extern  MR_TrieNode MR_uint64_hash_lookup_or_add_stats(
                        MR_TableStepStats *stats, MR_TrieNode table,
                        uint64_t key);
extern  MR_TrieNode MR_float_hash_lookup_or_add(MR_TrieNode table,
                        MR_Float key);
extern  MR_TrieNode MR_float_hash_lookup_or_add_stats(
                        MR_TableStepStats *stats, MR_TrieNode table,
                        MR_Float key);
extern  MR_TrieNode MR_string_hash_lookup_or_add(MR_TrieNode table,
                        MR_ConstString key);
extern  MR_TrieNode MR_string_hash_lookup_or_add_stats(
                        MR_TableStepStats *stats, MR_TrieNode table,
                        MR_ConstString key);
extern  MR_TrieNode MR_bitmap_hash_lookup_or_add(MR_TrieNode table,
                        MR_ConstBitmapPtr key);
extern  MR_TrieNode MR_bitmap_hash_lookup_or_add_stats(
                        MR_TableStepStats *stats, MR_TrieNode table,
                        MR_ConstBitmapPtr key);
extern  MR_TrieNode MR_word_hash_lookup_or_add(MR_TrieNode table,
                        MR_Word key);
extern  MR_TrieNode MR_word_hash_lookup_or_add_stats(
                        MR_TableStepStats *stats, MR_TrieNode table,
                        MR_Word key);

// These functions assume that the table is a statically sized array,
// with the index ranging from 0 to range - 1.

extern  MR_TrieNode MR_int_fix_index_enum_lookup_or_add(MR_TrieNode table,
                        MR_Integer range, MR_Integer key);
extern  MR_TrieNode MR_int_fix_index_enum_lookup_or_add_stats(
                        MR_TableStepStats *stats, MR_TrieNode table,
                        MR_Integer range, MR_Integer key);

extern  MR_TrieNode MR_int_fix_index_du_lookup_or_add(MR_TrieNode table,
                        MR_Integer range, MR_Integer key);
extern  MR_TrieNode MR_int_fix_index_du_lookup_or_add_stats(
                        MR_TableStepStats *stats, MR_TrieNode table,
                        MR_Integer range, MR_Integer key);

// These functions assume that the table is an expandable array,
// with the smallest valid index value being start.

extern  MR_TrieNode MR_int_start_index_lookup_or_add(MR_TrieNode table,
                        MR_Integer start, MR_Integer key);
extern  MR_TrieNode MR_int_start_index_lookup_or_add_stats(
                        MR_TableStepStats *stats, MR_TrieNode table,
                        MR_Integer start, MR_Integer key);

// These functions table type_infos in a hash table.

extern  MR_TrieNode MR_type_info_lookup_or_add(MR_TrieNode table,
                        MR_TypeInfo type_info);
extern  MR_TrieNode MR_type_info_lookup_or_add_stats(
                        MR_TableStepStats *stats, MR_TrieNode table,
                        MR_TypeInfo type_info);

// These functions table typeclass_infos in a hash table.

extern  MR_TrieNode MR_type_class_info_lookup_or_add(MR_TrieNode table,
                        MR_Word *type_class_info);
extern  MR_TrieNode MR_type_class_info_lookup_or_add_stats(
                        MR_TableStepStats *stats, MR_TrieNode table,
                        MR_Word *type_class_info);

// These functions table values of arbitrary types; the form of the data
// structure depends on the actual type of the value. The tabling is done
// by tabling all the function symbols of the value; unlike
// MR_word_hash_lookup, this function *does* guarantee that all duplicates
// will be detected.

extern  MR_TrieNode MR_table_type(MR_TrieNode table,
                        MR_TypeInfo type_info, MR_Word data_value);
extern  MR_TrieNode MR_table_type_debug(MR_TrieNode table,
                        MR_TypeInfo type_info, MR_Word data_value);
extern  MR_TrieNode MR_table_type_stats(MR_TableStepStats *stats,
                        MR_TrieNode table,
                        MR_TypeInfo type_info, MR_Word data_value);
extern  MR_TrieNode MR_table_type_stats_debug(MR_TableStepStats *stats,
                        MR_TrieNode table,
                        MR_TypeInfo type_info, MR_Word data_value);
extern  MR_TrieNode MR_table_type_back(MR_TrieNode table,
                        MR_TypeInfo type_info, MR_Word data_value);
extern  MR_TrieNode MR_table_type_debug_back(MR_TrieNode table,
                        MR_TypeInfo type_info, MR_Word data_value);
extern  MR_TrieNode MR_table_type_stats_back(MR_TableStepStats *stats,
                        MR_TrieNode table,
                        MR_TypeInfo type_info, MR_Word data_value);
extern  MR_TrieNode MR_table_type_stats_debug_back(MR_TableStepStats *stats,
                        MR_TrieNode table,
                        MR_TypeInfo type_info, MR_Word data_value);

// These functions look to see if the given key is in the given table.
// If it is, they return the address of the data pointer associated with
// the key. If it is not, they return NULL.
//
// These functions assume that the table is a dynamically resizable hash table.

extern  MR_TrieNode MR_int_hash_lookup(MR_TrieNode table,
                        MR_Integer key);
extern  MR_TrieNode MR_int64_hash_lookup(MR_TrieNode table,
                        int64_t key);
extern  MR_TrieNode MR_uint64_hash_lookup(MR_TrieNode table,
                        uint64_t key);
extern  MR_TrieNode MR_float_hash_lookup(MR_TrieNode table,
                        MR_Float key);
extern  MR_TrieNode MR_string_hash_lookup(MR_TrieNode table,
                        MR_ConstString key);
extern  MR_TrieNode MR_bitmap_hash_lookup(MR_TrieNode table,
                        MR_ConstBitmapPtr key);
extern  MR_TrieNode MR_word_hash_lookup(MR_TrieNode table,
                        MR_Word data_value);

// These functions return a dynamically resizable array (using the primitives
// in mercury_array_macros.h) containing all the elements in the given
// dynamically resizable hash table.

extern  MR_bool     MR_get_int_hash_table_contents(MR_TrieNode t,
                        MR_Integer **values_ptr, int *value_next_ptr);
extern  MR_bool     MR_get_float_hash_table_contents(MR_TrieNode t,
                        MR_Float **values_ptr, int *value_next_ptr);
extern  MR_bool     MR_get_string_hash_table_contents(MR_TrieNode t,
                        MR_ConstString **values_ptr, int *value_next_ptr);
extern  MR_bool     MR_get_bitmap_hash_table_contents(MR_TrieNode t,
                        MR_ConstBitmapPtr **values_ptr, int *value_next_ptr);

// This function prints statistics about the operation of tabling, if the
// collection of such statistics is enabled, on the given stream.

extern  void        MR_table_report_statistics(FILE *fp);

// These functions return printable representations of the MR_loop_status
// MR_memo_status and MR_mn_status fields.

extern  const char  *MR_loopcheck_status(MR_Unsigned);
extern  const char  *MR_memo_status(MR_Unsigned);
extern  const char  *MR_memo_non_status(MR_MemoNonStatus);

// These functions print the tips of the call tables for loopcheck and memo
// tabled predicates to fp.

extern  void        MR_print_loopcheck_tip(FILE *fp,
                        const MR_ProcLayout *proc, MR_TrieNode table);
extern  void        MR_print_memo_tip(FILE *fp,
                        const MR_ProcLayout *proc, MR_TrieNode table);
extern  void        MR_print_memo_non_record(FILE *fp,
                        const MR_ProcLayout *proc, MR_MemoNonRecordPtr record);

// Prints the given answer_block of the given procedure to fp.

extern  void        MR_print_answerblock(FILE *fp,
                        const MR_ProcLayout *proc, MR_Word *answer_block);

////////////////////////////////////////////////////////////////////////////

#ifndef MR_NATIVE_GC

  #define MR_TABLE_NEW(type)                                            \
    MR_GC_NEW_ATTRIB(type, MR_ALLOC_SITE_TABLING)

  #define MR_TABLE_NEW_ARRAY(type, count)                               \
    MR_GC_NEW_ARRAY_ATTRIB(type, (count), MR_ALLOC_SITE_TABLING)

  #define MR_TABLE_RESIZE_ARRAY(ptr, type, count)                       \
    MR_GC_RESIZE_ARRAY_ATTRIB((ptr), type, (count))

  #define MR_table_allocate_words(size)                                 \
    ((MR_Word *) MR_GC_malloc_attrib(sizeof(MR_Word) * (size),          \
            MR_ALLOC_SITE_TABLING))

  #define MR_table_allocate_struct(type)                                \
    ((type *) MR_GC_malloc_attrib(sizeof(type),                         \
            MR_ALLOC_SITE_TABLING))

  #define MR_table_allocate_structs(num, type)                          \
    ((type *) MR_GC_malloc_attrib(sizeof(type) * (num),                 \
            MR_ALLOC_SITE_TABLING))

  #define MR_table_free(pointer)                                        \
    MR_GC_free_attrib((pointer))

#else // MR_NATIVE_GC

  #define MR_TABLE_NATIVE_GC_MSG                                        \
    "Sorry, not implemented: tabling in native gc grades"

  #define MR_TABLE_NEW(type)                                            \
    (MR_fatal_error(MR_TABLE_NATIVE_GC_MSG),                            \
    (void *) NULL)
  #define MR_TABLE_NEW_ARRAY(type, count)                               \
    (MR_fatal_error(MR_TABLE_NATIVE_GC_MSG),                            \
    (void *) NULL)
  #define MR_TABLE_RESIZE_ARRAY(pointer, type, count)                   \
    (MR_fatal_error(MR_TABLE_NATIVE_GC_MSG),                            \
    (void *) NULL)
#if 0
  #define MR_table_allocate_bytes(size)                                 \
    (MR_fatal_error(MR_TABLE_NATIVE_GC_MSG),                            \
    (void *) NULL)
#endif
  #define MR_table_allocate_words(size)                                 \
    (MR_fatal_error(MR_TABLE_NATIVE_GC_MSG),                            \
    (void *) NULL)
  #define MR_table_allocate_struct(type)                                \
    (MR_fatal_error(MR_TABLE_NATIVE_GC_MSG),                            \
    (void *) NULL)
  #define MR_table_allocate_structs(num, type)                          \
    (MR_fatal_error(MR_TABLE_NATIVE_GC_MSG),                            \
    (void *) NULL)
  #define MR_table_free(pointer)                                        \
    MR_fatal_error(MR_TABLE_NATIVE_GC_MSG)

#endif // MR_NATIVE_GC

// XXX The extra memory attribution word is not yet copied.

#define MR_table_copy_words(dest, source, size)                         \
    (MR_CHECK_EXPR_TYPE((dest), MR_Word *),                             \
    (MR_CHECK_EXPR_TYPE((source), MR_Word *),                           \
    MR_memcpy((char *) (dest), (char *) (source),                       \
        sizeof(MR_Word) * (size))))

#define MR_table_copy_structs(dest, source, num, type)                  \
    (MR_CHECK_EXPR_TYPE((dest), type *),                                \
    (MR_CHECK_EXPR_TYPE((source), type *),                              \
    MR_memcpy((char *) (dest), (char *) (source),                       \
        sizeof(type) * (num))))

////////////////////////////////////////////////////////////////////////////

#ifdef  MR_HIGHLEVEL_CODE
    extern void MR_CALL
         mercury__table_builtin__table_memo_return_all_answers_multi_2_p_0(
            MR_Box record, MR_Box *answer_block_ptr,
            MR_Cont cont, void *cont_env_ptr);

    extern void MR_CALL
         mercury__table_builtin__table_memo_return_all_answers_nondet_2_p_0(
            MR_Box record, MR_Box *answer_block_ptr,
            MR_Cont cont, void *cont_env_ptr);
#else   // ! MR_HIGHLEVEL_CODE
  #define MR_MEMO_NON_RET_ALL_NONDET_ENTRY                              \
    MR_proc_entry_user_name(table_builtin,                              \
        table_memo_return_all_answers_nondet, 2, 0)
  #define MR_MEMO_NON_RET_ALL_MULTI_ENTRY                               \
    MR_proc_entry_user_name(table_builtin,                              \
        table_memo_return_all_answers_multi, 2, 0)

  MR_declare_entry(MR_MEMO_NON_RET_ALL_NONDET_ENTRY);
  MR_declare_entry(MR_MEMO_NON_RET_ALL_MULTI_ENTRY);
#endif  // MR_HIGHLEVEL_CODE

////////////////////////////////////////////////////////////////////////////

#include "mercury_tabling_macros.h"
#include "mercury_tabling_preds.h"

#include "mercury_stack_layout.h"   // for MR_ProcLayout and
                                    // MR_TypeParamLocns

#endif  // not MERCURY_TABLING_H
