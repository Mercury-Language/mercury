/*
** Copyright (C) 1997-1999 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

#include "mercury_imp.h"

#include "mercury_type_info.h"
#include "mercury_ho_call.h"
#include <stdio.h>

/*---------------------------------------------------------------------------*/

/*
** this part defines the functions
**	MR_int_hash_lookup_or_add(),
**	MR_float_hash_lookup_or_add(), and
** 	MR_string_hash_lookup_or_add().
*/

/* Initial size of a new table */
#define TABLE_START_SIZE primes[0]

/*
** Maximum ratio of used to unused buckets in the table. Must be less than
** 0.9 if you want even poor lookup times.
*/
#define MAX_EL_SIZE_RATIO 0.65

/* Extract info from a table */
#define SIZE(table)		(((TableRoot *) table)->size)
#define ELEMENTS(table)	 	(((TableRoot *) table)->used_elements)
#define BUCKET(table, Bucket) 	((TableNode **) &(((TableRoot *) table)-> \
					elements))[(Bucket)]
typedef struct {
	Word key;
	Word * data;
} TableNode;

typedef struct {
	Word size;
	Word used_elements;
	Word elements;
} TableRoot;

static Word next_prime(Word);
static Word * create_hash_table(Word);
static void re_hash(Word *, Word, TableNode * Node);

/*
** Prime numbers which are close to powers of 2.  Used for choosing
** the next size for a hash table.
*/

#define NUM_OF_PRIMES 16
static Word primes[NUM_OF_PRIMES] =
    {127, 257, 509, 1021, 2053, 4099, 8191, 16381, 32771, 65537, 131071,
       262147, 524287, 1048573, 2097143, 4194301};

/*
** Return the next prime number greater than the number received.
** If no such prime number can be found, compute an approximate one.
*/
static Word
next_prime(Word old_size)
{
	int i;

	i = 0;
	while ( (old_size >= primes[i]) && (i < NUM_OF_PRIMES) ) {
		i++;
	}

	if (i < NUM_OF_PRIMES) {
		return primes[i];
	} else {
		return 2 * old_size - 1;
	}
}

/* Create a new empty hash table. */
static Word *
create_hash_table(Word table_size)
{
   	Word i;
	TableRoot * table =
		table_allocate_bytes(sizeof(Word) * 2 +
				table_size * sizeof(TableNode *));

	table->size = table_size;
	table->used_elements = 0;

	for (i = 0; i < table_size; i++) {
		BUCKET(table, i) = NULL;
	}

	return (Word *) table;
}

/*
** Insert key and Data into a new hash table using the given hash.
** this function does not have to do compares as the given key
** is definitely not in the table.
*/
static void
re_hash(Word * table, Word hash, TableNode * node)
{
	Word bucket = hash % SIZE(table);

	while (BUCKET(table, bucket)) {
		++bucket;
		if (bucket == SIZE(table))
			bucket = 0;
	}

	BUCKET(table, bucket) = node;
	++ELEMENTS(table);
}

/*
** Look to see if the given integer key is in the given table. If it
** is return the address of the data pointer associated with the key.
** If it is not; create a new element for the key in the table and
** return the address of its data pointer.
*/
MR_TrieNode
MR_int_hash_lookup_or_add(MR_TrieNode t, Integer key)
{
	TableNode * p, * q;
	Word * table = *t;	/* Deref the table pointer */
	Word bucket;

	/* Has the the table been built? */
	if (table == NULL) {
		table = create_hash_table(TABLE_START_SIZE);
		*t = table;
	}

	bucket = key % SIZE(table);
	p = BUCKET(table, bucket);

	/* Find if the element is present. If not add it */
	while (p) {
		if (key == p->key) {
			return &p->data;
		}

		bucket++;
		if (bucket == SIZE(table))
			bucket = 0;

		p = BUCKET(table, bucket);
	}

	p = table_allocate_bytes(sizeof(TableNode));
	p->key = key;
	p->data = NULL;

	/* Rehash the table if it has grown to full */
	if ((float) ELEMENTS(table) / (float) SIZE(table) >
	   		MAX_EL_SIZE_RATIO)
	{
		int old_size = SIZE(table);
		int new_size = next_prime(old_size);
		Word * new_table = create_hash_table(new_size);
		int i;

		for (i = 0; i < old_size; i++) {
			q = BUCKET(table, i);
			if (q) {
				re_hash(new_table, q->key, q);
			}
		}

		/* Free the old table */
		table_free(table);

		/* Point to the new table */
		*t = new_table;

		/* Add a new element */
		re_hash(new_table, key, p);
	} else {
		BUCKET(table, bucket) = p;
		++ELEMENTS(table);
	}

	return &p->data;
}

/*
** Look to see if the given float key is in the given table. If it
** is return the address of the data pointer associated with the key.
** If it is not create a new element for the key in the table and
** return the address of its data pointer.
*/
MR_TrieNode
MR_float_hash_lookup_or_add(MR_TrieNode t, Float key)
{
	TableNode	*p, *q;
	Word		*table = *t;	/* Deref the table pointer */
	Word		bucket;
	Word		hash;

	/* Has the the table been built? */
	if (table == NULL) {
		table = create_hash_table(TABLE_START_SIZE);
		*t = table;
	}

	hash = hash_float(key);
	bucket = hash % SIZE(table);

	p = BUCKET(table, bucket);

	/* Find if the element is present. If not add it */
	while (p) {
		if (key == word_to_float(p->key)) {
			return &p->data;
		}

		++bucket;
		if (bucket == SIZE(table))
			bucket = 0;

		p = BUCKET(table, bucket);
	}

	p = table_allocate_bytes(sizeof(TableNode));
	p->key = float_to_word(key);
	p->data = NULL;

	/* Rehash the table if it has grown to full */
	if ((float) ELEMENTS(table) / (float) SIZE(table) >
	   		MAX_EL_SIZE_RATIO)
	{
		int old_size = SIZE(table);
		int new_size = next_prime(old_size);
		Word * new_table = create_hash_table(new_size);
		int i;

		for (i = 0; i < old_size; i++) {
			q = BUCKET(table, i);
			if (q) {
				re_hash(new_table, hash_float(q->key), q);
			}
		}

		/* Free the old table */
		table_free(table);

		/* Point to the new table */
		*t = new_table;

		/* Add a new element */
		re_hash(new_table, hash, p);
	} else {
		++ELEMENTS(table);
		BUCKET(table, bucket) = p;
	}

	return &p->data;
}



/*
** Look to see if the given string key is in the given table. If it
** is return the address of the data pointer associated with the key.
** If it is not create a new element for the key in the table and
** return the address of its data pointer.
*/
MR_TrieNode
MR_string_hash_lookup_or_add(MR_TrieNode t, String key)
{
	TableNode * p, * q;
	Word * table = *t;	/* Deref the table pointer */
	Word bucket;
	Word hash;

	/* Has the the table been built? */
	if (table == NULL) {
		table = create_hash_table(TABLE_START_SIZE);
		*t = table;
	}

	hash = hash_string((Word) key);
	bucket = hash % SIZE(table);

	p = BUCKET(table, bucket);

	/* Find if the element is present. */
	while (p) {
		int res = strtest((String)p->key, key);

		if (res == 0) {
			return &p->data;
		}

		++bucket;
		if (bucket == SIZE(table))
			bucket = 0;

		p = BUCKET(table, bucket);
	}

	p = table_allocate_bytes(sizeof(TableNode));
	p->key = (Word) key;
	p->data = NULL;

	/* Rehash the table if it has grown to full */
	if ((float) ELEMENTS(table) / (float) SIZE(table) >
	   		MAX_EL_SIZE_RATIO)
	{
		int old_size = SIZE(table);
		int new_size = next_prime(old_size);
		Word * new_table = create_hash_table(new_size);
		int i;

		for (i = 0; i < old_size; i++) {
			q = BUCKET(table, i);
			if (q) {
				re_hash(new_table,
					hash_string((Word) q->key), q);
			}
		}

		/* Free the old table */
		table_free(t);

		/* Point to the new table */
		*t = new_table;

		/* Add a new element to rehashed table */
		re_hash(new_table, hash, p);
	} else {
		BUCKET(table, bucket) = p;
		++ELEMENTS(table);
	}

	return &p->data;
}

/*---------------------------------------------------------------------------*/

/*
** This part defines the MR_int_index_lookup_or_add() function.
*/

#define ELEMENT(Table, Key) ((Word**)&((Table)[Key]))

/*
**  MR_int_index_lookup_or_add() : This function maintains a simple indexed
**	table of size Range.
*/

MR_TrieNode
MR_int_index_lookup_or_add(MR_TrieNode t, Integer range, Integer key)
{
	Word *table = *t;		/* Deref table */

#ifdef	MR_TABLE_DEBUG
	if (key >= range) {
		fatal_error("MR_int_index_lookup_or_add: key out of range");
	}
#endif

	if (table == NULL) {
		*t = table = table_allocate_words(range);
		memset(table, 0, sizeof(Word *) * range);
	}

	return ELEMENT(table, key);
}

#undef ELEMENT

/*---------------------------------------------------------------------------*/

/*
** This part defines the type_info_lookup_or_add() function.
*/

typedef struct TreeNode_struct {
	Word * key;
	Word value;
	struct TreeNode_struct * right;
	struct TreeNode_struct * left;
} TreeNode;

MR_TrieNode
MR_type_info_lookup_or_add(MR_TrieNode table, Word * type_info)
{
	TreeNode *p, *q;
	int i;

	if (*table == NULL) {
		p = table_allocate_bytes(sizeof(TreeNode));

		p->key = type_info;
		p->value = (Word) NULL;
		p->left = NULL;
		p->right = NULL;

		*table = (Word *) p;

		return (Word**) &p->value;
	}

	p = (TreeNode *) *table;

	while (p != NULL) {
		i = MR_compare_type_info((Word) p->key, (Word) type_info);

		if (i == COMPARE_EQUAL) {
			return (Word **) &p->value;
		}

		q = p;

		if (i == COMPARE_LESS) {
			p = p->left;
		} else {
			p = p->right;
		}
	}

	p = table_allocate_bytes(sizeof(TreeNode));
	p->key = type_info;
	p->value = (Word) NULL;
	p->left = NULL;
	p->right = NULL;

	if (i == COMPARE_LESS) {
		q->left = p;
	} else {
		q ->right = p;
	}

	return (Word **) &p->value;
}

/*---------------------------------------------------------------------------*/


/*
** This part defines the MR_table_type() function.
*/

MR_DECLARE_TYPE_CTOR_INFO_STRUCT(mercury_data___type_ctor_info_pred_0);
MR_DECLARE_TYPE_CTOR_INFO_STRUCT(mercury_data___type_ctor_info_func_0);

/*
** Due to the depth of the control here, we'll use 4 space indentation.
**
** NOTE : changes to this function will probably also have to be reflected
** in mercury_deep_copy.c and std_util::ML_expand().
*/

MR_TrieNode
MR_table_type(MR_TrieNode table, Word *type_info, Word data)
{
    MR_TypeCtorInfo	type_ctor_info;
    MR_TypeCtorLayout	type_ctor_layout;
    MR_TypeCtorFunctors	type_ctor_functors;
    MR_DiscUnionTagRepresentation tag_rep;
    MR_MemoryList	allocated_memory_cells = NULL;

    Word		layout_for_tag;
    Word		*layout_vector_for_tag;
    Word		*data_value;
    int			data_tag;
    int			entry_tag;

    data_tag = MR_tag(data);
    data_value = (Word *) MR_body(data, data_tag);

    type_ctor_info = MR_TYPEINFO_GET_TYPE_CTOR_INFO(type_info);
    type_ctor_layout = MR_TYPE_CTOR_INFO_GET_TYPE_CTOR_LAYOUT(type_ctor_info);
    type_ctor_functors = MR_TYPE_CTOR_INFO_GET_TYPE_CTOR_FUNCTORS(
                    type_ctor_info);

    layout_for_tag = type_ctor_layout[data_tag];
    layout_vector_for_tag = (Word *) MR_strip_tag(layout_for_tag);

#ifdef  MR_TABLE_DEBUG
    if (MR_tabledebug) {
        printf("ENTRY %p %x, data rep: %d\n",
            table, data, MR_get_new_type_ctor_rep(type_ctor_info));
    }
#endif  /* MR_TABLE_DEBUG */

    switch (MR_get_new_type_ctor_rep(type_ctor_info)) {
        case MR_TYPECTOR_REP_ENUM: 
        case MR_TYPECTOR_REP_ENUM_USEREQ: 
	{
            int functors = MR_TYPE_CTOR_LAYOUT_ENUM_VECTOR_NUM_FUNCTORS(
                                layout_vector_for_tag);
            MR_DEBUG_TABLE_ENUM(table, functors, data);
            break;
        }
        case MR_TYPECTOR_REP_DU: 
        case MR_TYPECTOR_REP_DU_USEREQ: 
	{
            tag_rep = MR_get_tag_representation((Word) layout_for_tag);
            switch(tag_rep) {
            case MR_DISCUNIONTAG_SHARED_LOCAL: {
                int functors = MR_TYPE_CTOR_LAYOUT_ENUM_VECTOR_NUM_FUNCTORS(
                                layout_vector_for_tag);
                MR_DEBUG_TABLE_TAG(table, data_tag);
                MR_DEBUG_TABLE_ENUM(table, functors, MR_unmkbody(data));
                break;
            }
            case MR_DISCUNIONTAG_UNSHARED: {
                int arity, i;
                Word *argument_vector, *type_info_vector, *new_type_info;
    
                argument_vector = data_value;
    
                arity = layout_vector_for_tag[
                            TYPE_CTOR_LAYOUT_UNSHARED_ARITY_OFFSET];
                type_info_vector = &layout_vector_for_tag[
                            TYPE_CTOR_LAYOUT_UNSHARED_ARGS_OFFSET];

                MR_DEBUG_TABLE_TAG(table, data_tag);

                     /* copy arguments */
                for (i = 0; i < arity; i++) {
                    new_type_info = MR_make_type_info(type_info,
                        (Word *) type_info_vector[i], &allocated_memory_cells);

                    MR_DEBUG_TABLE_ANY(table, new_type_info,
                        argument_vector[i]);
                }
                break;
            }
            case MR_DISCUNIONTAG_SHARED_REMOTE: {
                int     arity, i;
                Word    *argument_vector;
                Word    *type_info_vector;
                Word    *new_type_info;
                Word    secondary_tag;
                Word    num_sharers;
                Word    *new_layout_vector;

                secondary_tag = *data_value;
                argument_vector = data_value + 1;

                num_sharers = MR_TYPE_CTOR_LAYOUT_SHARED_REMOTE_VECTOR_NUM_SHARERS(
                                layout_vector_for_tag);
                new_layout_vector =
                    MR_TYPE_CTOR_LAYOUT_SHARED_REMOTE_VECTOR_GET_FUNCTOR_DESCRIPTOR(
                    layout_vector_for_tag, secondary_tag);
                arity = new_layout_vector[TYPE_CTOR_LAYOUT_UNSHARED_ARITY_OFFSET];
                type_info_vector =
                    &new_layout_vector[TYPE_CTOR_LAYOUT_UNSHARED_ARGS_OFFSET];

                MR_DEBUG_TABLE_TAG(table, data_tag);
                MR_DEBUG_TABLE_ENUM(table, num_sharers, secondary_tag);

                for (i = 0; i < arity; i++) {
                    new_type_info = MR_make_type_info(type_info,
                        (Word *) type_info_vector[i], &allocated_memory_cells);

                    MR_DEBUG_TABLE_ANY(table, new_type_info,
                        argument_vector[i]);
                }
                break;
            }
            }
            break;
        }
        case MR_TYPECTOR_REP_NOTAG: 
        case MR_TYPECTOR_REP_NOTAG_USEREQ:
	{
            Word *new_type_info;
            new_type_info = MR_make_type_info(type_info,
                (Word *) *MR_TYPE_CTOR_LAYOUT_NO_TAG_VECTOR_ARGS(
                    layout_vector_for_tag),
                &allocated_memory_cells);
            MR_DEBUG_TABLE_ANY(table, new_type_info, data);
            break;
        }
        case MR_TYPECTOR_REP_EQUIV: {
            Word *new_type_info;
            new_type_info = MR_make_type_info(type_info,
                (Word *) MR_TYPE_CTOR_LAYOUT_EQUIV_TYPE(layout_vector_for_tag),
                &allocated_memory_cells);
            MR_DEBUG_TABLE_ANY(table, new_type_info, data);
            break;
        }
        case MR_TYPECTOR_REP_EQUIV_VAR:
            MR_DEBUG_TABLE_ANY(table,
                (Word *) type_info[(Word) layout_vector_for_tag], data);
            break;

        case MR_TYPECTOR_REP_INT:
            MR_DEBUG_TABLE_INT(table, data);
            break;

        case MR_TYPECTOR_REP_CHAR:
            MR_DEBUG_TABLE_CHAR(table, data);
            break;

        case MR_TYPECTOR_REP_FLOAT:
            MR_DEBUG_TABLE_FLOAT(table, data);
            break;

        case MR_TYPECTOR_REP_STRING:
            MR_DEBUG_TABLE_STRING(table, data);
            break;

        case MR_TYPECTOR_REP_PRED: {
	    /*
	    ** XXX tabling of the closures by tabling their code address
	    ** and arguments is not yet implemented, due to the difficulty
	    ** of figuring out the closure argument types.
	    */
	#if 0
	    MR_closure closure = (MR_Closure *) data_value;
            Word num_hidden_args = closure->MR_closure_num_hidden_args;
            int i;

            MR_DEBUG_TABLE_INT(table, closure->MR_closure_code);
            for (i = 1; i <= num_hidden_args; i++) {
        	MR_DEBUG_TABLE_ANY(table,
                    <type_info for hidden closure argument number i>,
                    closure->MR_closure_hidden_args(i));
            }
            break;
	#endif
	    /*
	    ** Instead, we use the following rather simplistic means of
	    ** tabling closures: we just table based on the closure address.
	    */
            MR_DEBUG_TABLE_INT(table, (Word) data_value);
        }
        case MR_TYPECTOR_REP_UNIV:
            MR_DEBUG_TABLE_TYPEINFO(table,
                (Word *) data_value[UNIV_OFFSET_FOR_TYPEINFO]);
            MR_DEBUG_TABLE_ANY(table,
                (Word *) data_value[UNIV_OFFSET_FOR_TYPEINFO],
                data_value[UNIV_OFFSET_FOR_DATA]);
            break;

        case MR_TYPECTOR_REP_VOID:
            fatal_error("Cannot table a void type");
            break;

        case MR_TYPECTOR_REP_C_POINTER:
            fatal_error("Attempt to table a C_POINTER");
            break;

        case MR_TYPECTOR_REP_TYPEINFO:
            MR_DEBUG_TABLE_TYPEINFO(table, (Word *) data_value);
            break;

        case MR_TYPECTOR_REP_TYPECLASSINFO:
            fatal_error("Attempt to table a type_class_info");
            break;

        case MR_TYPECTOR_REP_ARRAY: {
            int i;
            MR_ArrayType *array;
            Word *new_type_info;
            Integer array_size;

            array = (MR_ArrayType *) data_value;
            array_size = array->size;

            new_type_info = MR_make_type_info(type_info, (Word *) 1,
                &allocated_memory_cells);

            for (i = 0; i < array_size; i++) {
                MR_DEBUG_TABLE_ANY(table, new_type_info, array->elements[i]);
            }
            break;
        }

        case MR_TYPECTOR_REP_SUCCIP:
            fatal_error("Attempt to table a saved succip");
            break;

        case MR_TYPECTOR_REP_HP:
            fatal_error("Attempt to table a saved hp");
            break;

        case MR_TYPECTOR_REP_CURFR:
            fatal_error("Attempt to table a saved curfr");
            break;

        case MR_TYPECTOR_REP_MAXFR:
            fatal_error("Attempt to table a saved maxfr");
            break;

        case MR_TYPECTOR_REP_REDOFR:
            fatal_error("Attempt to table a saved redofr");
            break;

        case MR_TYPECTOR_REP_REDOIP:
            fatal_error("Attempt to table a saved redoip");
            break;

        case MR_TYPECTOR_REP_TRAIL_PTR:
            fatal_error("Attempt to table a saved trail pointer");
            break;

        case MR_TYPECTOR_REP_TICKET:
            fatal_error("Attempt to table a saved ticket");
            break;

        case MR_TYPECTOR_REP_UNKNOWN: /* fallthru */
        default:
            fatal_error("Unknown layout tag in table_any");
            break;
    }

    MR_deallocate(allocated_memory_cells);

    return table;
} /* end table_any() */

/*---------------------------------------------------------------------------*/

#ifdef	MR_USE_MINIMAL_MODEL

/*
** Save the current state of the Mercury abstract machine, so that the
** current computation may be suspended for a while, and restored later.
** The generator_{maxfr,sp} arguments give the points from which we need
** to copy the nondet and the det stacks. The parts of those stacks below
** the given points will not change between the suspension and the resumption
** of this state, or if they do, the stack segments in the saved state
** will be extended (via extend_consumer_stacks).
*/

static void
save_state(MR_SavedState *saved_state,
	Word *generator_maxfr, Word *generator_sp,
	const char *who, const char *what)
{
	restore_transient_registers();

	saved_state->succ_ip = MR_succip;
	saved_state->s_p = MR_sp;
	saved_state->cur_fr = MR_curfr;
	saved_state->max_fr = MR_maxfr;

	saved_state->non_stack_block_start = generator_maxfr + 1;
	if (MR_maxfr > generator_maxfr) {
		saved_state->non_stack_block_size = MR_maxfr - generator_maxfr;
		saved_state->non_stack_block =
			table_allocate_words(saved_state->non_stack_block_size);
		table_copy_words(saved_state->non_stack_block,
			saved_state->non_stack_block_start,
			saved_state->non_stack_block_size);
	} else {
		saved_state->non_stack_block_size = 0;
		saved_state->non_stack_block = NULL;
	}

	saved_state->det_stack_block_start = generator_sp;
	if (MR_sp > generator_sp) {
		saved_state->det_stack_block_size = (MR_sp - 1) - generator_sp;
		saved_state->det_stack_block =
			table_allocate_words(saved_state->det_stack_block_size);
		table_copy_words(saved_state->det_stack_block,
			saved_state->det_stack_block_start,
			saved_state->det_stack_block_size);
	} else {
		saved_state->det_stack_block_size = 0;
		saved_state->det_stack_block = NULL;
	}

	saved_state->gen_next = MR_gen_next;
	saved_state->generator_stack_block = table_allocate_bytes(
			MR_gen_next * sizeof(MR_GeneratorStackFrame));
	table_copy_bytes(saved_state->generator_stack_block,
		MR_gen_stack, MR_gen_next * sizeof(MR_GeneratorStackFrame));

	saved_state->cut_next = MR_cut_next;
	saved_state->cut_stack_block = table_allocate_bytes(
			MR_cut_next * sizeof(MR_CutStackFrame));
	table_copy_bytes(saved_state->cut_stack_block,
		MR_cut_stack, MR_cut_next * sizeof(MR_CutStackFrame));

#ifdef MR_USE_TRAIL
	/*
	** Saving the trail state here would not be sufficient to handle
	** the combination of trailing and minimal model tabling.
	** Consider the following sequence of events:
	**
	**	execution enters a goal being committed across
	**	a new entry is pushed on the trail
	**	a tabled goal suspends,
	**		causing the saving of a trail segment
	**		and then a failure
	**	the goal being committed across fails,
	**		which invokes a failed commit on the trail entry
	**	...
	**	the tabled goal is resumed,
	**		causing the restoring of the saved trail segment
	**		and then a success
	**	the goal being committed across now succeeds,
	**		which invokes a successful commit on the trail entry
	**
	** The trail handler will be thoroughly confused by such a sequence.
	*/

	fatal_error("Sorry, not implemented: "
		"can't have both minimal model tabling and trailing");
#endif

#ifdef	MR_TABLE_DEBUG
	if (MR_tabledebug) {
		printf("\n%s saves %s stacks: ", who, what);
		printf("%d non, %d det, %d generator, %d cut\n",
			saved_state->non_stack_block_size,
			saved_state->det_stack_block_size,
			MR_gen_next, MR_cut_next);

		printf("non region from ");
		MR_printnondstackptr(saved_state->non_stack_block_start);
		printf(" to ");
		MR_printnondstackptr(MR_maxfr);
		printf(" (both inclusive)\n");
		printf("stored at %p to %p (both inclusive)\n",
			saved_state->non_stack_block,
			saved_state->non_stack_block +
				saved_state->non_stack_block_size - 1);

		printf("det region from ");
		MR_printdetstackptr(saved_state->det_stack_block_start);
		printf(" to ");
		MR_printdetstackptr(MR_sp);
		printf(" (both inclusive)\n");
		printf("stored at %p to %p (both inclusive)\n",
			saved_state->det_stack_block,
			saved_state->det_stack_block +
				saved_state->det_stack_block_size - 1);

		printf("succip = %p, sp = ", (void *) MR_succip);
		MR_printdetstackptr(MR_sp);
		printf("\nmaxfr = ");
		MR_printnondstackptr(MR_maxfr);
		printf(", curfr = ");
		MR_printnondstackptr(MR_curfr);
		printf("\n\n");

		MR_print_gen_stack(stdout);

		if (MR_tablestackdebug) {
			MR_dump_nondet_stack_from_layout(stdout, MR_maxfr);
		}
	}
#endif

	save_transient_registers();
}

/*
** Restore the state of the Mercury abstract machine from saved_state.
*/

static void
restore_state(MR_SavedState *saved_state, const char *who, const char *what)
{
	restore_transient_registers();

	MR_succip = saved_state->succ_ip;
	MR_sp = saved_state->s_p;
	MR_curfr = saved_state->cur_fr;
	MR_maxfr = saved_state->max_fr;

	table_copy_words(saved_state->non_stack_block_start,
		saved_state->non_stack_block,
		saved_state->non_stack_block_size);

	table_copy_words(saved_state->det_stack_block_start,
		saved_state->det_stack_block,
		saved_state->det_stack_block_size);

	MR_gen_next = saved_state->gen_next;
	table_copy_bytes(MR_gen_stack, saved_state->generator_stack_block,
		saved_state->gen_next * sizeof(MR_GeneratorStackFrame));

	MR_cut_next = saved_state->cut_next;
	table_copy_bytes(MR_cut_stack, saved_state->cut_stack_block,
		saved_state->cut_next * sizeof(MR_CutStackFrame));

#ifdef	MR_TABLE_DEBUG
	if (MR_tabledebug) {
		printf("\n%s restores %s stacks: ", who, what);
		printf("%d non, %d det, %d generator, %d cut\n",
			saved_state->non_stack_block_size,
			saved_state->det_stack_block_size,
			saved_state->gen_next, saved_state->cut_next);

		printf("non region from ");
		MR_printnondstackptr(saved_state->non_stack_block_start);
		printf(" to ");
		MR_printnondstackptr(saved_state->non_stack_block_start +
			saved_state->non_stack_block_size - 1);
		printf(" (both inclusive)\n");
		printf("stored at %p to %p (both inclusive)\n",
			saved_state->non_stack_block,
			saved_state->non_stack_block +
				saved_state->non_stack_block_size - 1);

		printf("det region from ");
		MR_printdetstackptr(saved_state->det_stack_block_start);
		printf(" to ");
		MR_printdetstackptr(saved_state->det_stack_block_start +
			saved_state->det_stack_block_size - 1);
		printf(" (both inclusive)\n");
		printf("stored at %p to %p (both inclusive)\n",
			saved_state->det_stack_block,
			saved_state->det_stack_block +
				saved_state->det_stack_block_size - 1);

		printf("succip = %p, sp = ", (void *) MR_succip);
		MR_printdetstackptr(MR_sp);
		printf("\nmaxfr = ");
		MR_printnondstackptr(MR_maxfr);
		printf(", curfr = ");
		MR_printnondstackptr(MR_curfr);
		printf("\n");

		MR_print_gen_stack(stdout);

		if (MR_tablestackdebug) {
			MR_dump_nondet_stack_from_layout(stdout, MR_maxfr);
		}
	}
#endif

	save_transient_registers();
}

static void
print_saved_state_stacks(MR_SavedState *saved_state)
{
	int	i;

	printf("saved state parameters:\n");
	printf("succip:\t");
	printlabel(saved_state->succ_ip);
	printf("sp:\t");
	MR_printdetstackptr(saved_state->s_p);
	printf("\ncurfr:\t");
	MR_printnondstackptr(saved_state->cur_fr);
	printf("\nmaxfr:\t");
	MR_printnondstackptr(saved_state->max_fr);

	printf("\n\nnondet stack block: %d words from %p\n",
		saved_state->non_stack_block_size,
		saved_state->non_stack_block_start);
	for (i = 0; i < saved_state->non_stack_block_size; i++) {
		printf("%2d: %x\n", i, saved_state->non_stack_block[i]);
	}

	printf("\ndet stack block: %d words from %p\n",
		saved_state->det_stack_block_size,
		saved_state->det_stack_block_start);
	for (i = 0; i < saved_state->det_stack_block_size; i++) {
		printf("%2d: %x\n", i, saved_state->det_stack_block[i]);
	}

	printf("\n");
}

/*
** The saved state of a consumer for a subgoal (say subgoal A) includes
** the stack segments between the tops of the stack at the time that
** A's generator was entered and the time that A's consumer was entered.
** When A becomes a follower of another subgoal B, the responsibility for
** scheduling A's consumers passes to B's generator. Since by definition
** B's nondet stack frame is lower in the stack than A's generator's,
** we need to extend the stack segments of A's consumers to also include
** the parts of the stacks between the generator of B and the generator of A.
*/

Declare_entry(mercury__table_nondet_resume_1_0);

static void
extend_consumer_stacks(MR_Subgoal *leader, MR_Consumer *suspension)
{
	Word	*arena_block;
	Word	*arena_start;
	Word	arena_size;
	Word	extension_size;
	Word	*saved_fr;
	Word	*real_fr;
	Word	frame_size;
	Word	offset;

#ifdef	MR_TABLE_DEBUG
	if (MR_tablestackdebug) {
		printf("\nextending saved consumer stacks\n");
		print_saved_state_stacks(&suspension->saved_state);
	}
#endif

	arena_start = leader->generator_sp;
	extension_size = suspension->saved_state.det_stack_block_start
			- arena_start;
	arena_size  = extension_size
			+ suspension->saved_state.det_stack_block_size;
	if (arena_size != 0) {
		assert(arena_start + arena_size
				== suspension->saved_state.s_p - 1);
	}

	arena_block = table_allocate_words(arena_size);

	table_copy_words(arena_block, arena_start, extension_size);
	table_copy_words(arena_block + extension_size,
		suspension->saved_state.det_stack_block,
		suspension->saved_state.det_stack_block_size);

#ifdef	MR_TABLE_DEBUG
	if (MR_tabledebug) {
		printf("extending det stack of suspension %p for %p\n",
			suspension, leader);
		printf("start: old %p, new %p\n",
			suspension->saved_state.det_stack_block_start,
			arena_start);
		printf("size:  old %d, new %d\n",
			suspension->saved_state.det_stack_block_size,
			arena_size);
		printf("block: old %p, new %p\n",
			suspension->saved_state.det_stack_block,
			arena_block);
	}
#endif

	suspension->saved_state.det_stack_block = arena_block;
	suspension->saved_state.det_stack_block_size = arena_size;
	suspension->saved_state.det_stack_block_start = arena_start;

	arena_start = leader->generator_maxfr + 1;
	extension_size = suspension->saved_state.non_stack_block_start
			- arena_start;
	arena_size  = extension_size
			+ suspension->saved_state.non_stack_block_size;
	assert(leader->generator_maxfr + arena_size
			== suspension->saved_state.max_fr);

	arena_block = table_allocate_words(arena_size);

	table_copy_words(arena_block, arena_start, extension_size);
	table_copy_words(arena_block + extension_size,
		suspension->saved_state.non_stack_block,
		suspension->saved_state.non_stack_block_size);

#ifdef	MR_TABLE_DEBUG
	if (MR_tabledebug) {
		printf("extending non stack of suspension %p for %p\n",
			suspension, leader);
		printf("start: old %p, new %p\n",
			suspension->saved_state.non_stack_block_start,
			arena_start);
		printf("size:  old %d, new %d\n",
			suspension->saved_state.non_stack_block_size,
			arena_size);
		printf("block: old %p, new %p\n",
			suspension->saved_state.non_stack_block,
			arena_block);
	}
#endif

	suspension->saved_state.non_stack_block = arena_block;
	suspension->saved_state.non_stack_block_size = arena_size;
	suspension->saved_state.non_stack_block_start = arena_start;

#ifdef	MR_TABLE_DEBUG
	if (MR_tablestackdebug) {
		printf("\nbefore pickling nondet stack\n");
		print_saved_state_stacks(&suspension->saved_state);
	}
#endif

	saved_fr = suspension->saved_state.non_stack_block +
		suspension->saved_state.non_stack_block_size - 1;
	real_fr = suspension->saved_state.non_stack_block_start +
		suspension->saved_state.non_stack_block_size - 1;
	while (saved_fr > suspension->saved_state.non_stack_block) {
		frame_size = real_fr - MR_prevfr_slot(saved_fr);

		if (saved_fr - frame_size
			> suspension->saved_state.non_stack_block)
		{
			*MR_redoip_addr(saved_fr) = (Word) ENTRY(do_fail);

#ifdef	MR_TABLE_DEBUG
			if (MR_tabledebug) {
				printf("do_fail to redoip at %p (%d)\n",
					MR_redoip_addr(saved_fr),
					MR_redoip_addr(saved_fr) -
					suspension->
					saved_state.non_stack_block);
			}
#endif
		} else {
			*MR_redoip_addr(saved_fr) = (Word)
				ENTRY(mercury__table_nondet_resume_1_0);
#ifdef	MR_TABLE_DEBUG
			if (MR_tabledebug) {
				printf("resume to redoip at %p (%d)\n",
					MR_redoip_addr(saved_fr),
					MR_redoip_addr(saved_fr) -
					suspension->
					saved_state.non_stack_block);
			}
#endif
		}

		saved_fr -= frame_size;
		real_fr -= frame_size;
	}

#ifdef	MR_TABLE_DEBUG
	if (MR_tablestackdebug) {
		printf("\nfinished extending saved consumer stacks\n");
		print_saved_state_stacks(&suspension->saved_state);
	}
#endif
}

/*
** When we discover that two subgoals depend on each other, neither can be
** completed alone. We therefore pass responsibility for completing all
** the subgoals in an SCC to the subgoal whose nondet stack frame is
** lowest in the nondet stack.
*/

static void
make_subgoal_follow_leader(MR_Subgoal *this_follower, MR_Subgoal *leader)
{
	MR_Consumer		*suspension;
	MR_SubgoalList		sub_followers;
	MR_ConsumerList		suspend_list;

	restore_transient_registers();

#ifdef	MR_TABLE_DEBUG
	if (MR_tabledebug) {
		printf("making %p follow %p\n", this_follower, leader);
	}
#endif

	for (sub_followers = this_follower->followers;
		sub_followers != NULL; sub_followers = sub_followers->next)
	{
		for (suspend_list = sub_followers->item->consumer_list;
			suspend_list != NULL;
			suspend_list = suspend_list->next)
		{
			save_transient_registers();
			extend_consumer_stacks(leader, suspend_list->item);
			restore_transient_registers();
		}
	}

	this_follower->leader = leader;
	*(leader->followers_tail) = this_follower->followers;
	this_follower->followers = NULL;

	save_transient_registers();
}

/*
** The following procedure saves the state of the Mercury runtime
** so that it may be used in the table_nondet_resume procedure below to return
** answers through this saved state. The procedure table_nondet_suspend is
** declared as nondet but the code below is obviously of detism failure;
** the reason for this is quite simple. Normally when a nondet proc
** is called it will first return all of its answers and then fail. In the
** case of calls to this procedure this is reversed: first the call will fail
** then later on, when the answers are found, answers will be returned.
** It is also important to note that the answers are returned not from the
** procedure that was originally called (table_nondet_suspend) but from the
** procedure table_nondet_resume. So essentially what is below is the code
** to do the initial fail; the code to return the answers is in
** table_nondet_resume.
*/

Declare_entry(mercury__table_nondet_resume_1_0);
Declare_entry(MR_do_trace_redo_fail);
Declare_entry(MR_table_nondet_commit);
Define_extern_entry(mercury__table_nondet_suspend_2_0);
MR_MAKE_PROC_LAYOUT(mercury__table_nondet_suspend_2_0,
	MR_DETISM_NON, 0, MR_LONG_LVAL_TYPE_UNKNOWN,
	MR_PREDICATE, "private_builtin", "table_nondet_suspend", 2, 0);
BEGIN_MODULE(table_nondet_suspend_module)
	init_entry_sl(mercury__table_nondet_suspend_2_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury__table_nondet_suspend_2_0);
BEGIN_CODE

Define_entry(mercury__table_nondet_suspend_2_0);
{
	MR_Subgoal	*table;
	MR_Consumer	*consumer;
	MR_ConsumerList	listnode;
	Integer		cur_gen;
	Integer		cur_cut;
	Word		*fr;
	Word		*prev_fr;
	Word		*stop_addr;
	Word		offset;
	Word		*clobber_addr;

	/*
	** This frame is not used in table_nondet_suspend, but it is copied
	** to the suspend list as part of the saved nondet stack fragment,
	** and it *will* be used when table_nondet_resume copies back the
	** nondet stack fragment. The framevar slot is for use by
	** table_nondet_resume.
	*/
	MR_mkframe("mercury__table_nondet_suspend", 1, ENTRY(do_fail));

	table = MR_SUBGOAL(r1);
	consumer = table_allocate_bytes(sizeof(MR_Consumer));
	consumer->remaining_answer_list_ptr = &table->answer_list;

	save_transient_registers();
	save_state(&(consumer->saved_state),
		table->generator_maxfr, table->generator_sp,
		"suspension", "consumer");
	restore_transient_registers();

	cur_gen = MR_gen_next - 1;
	cur_cut = MR_cut_next - 1;
	stop_addr = consumer->saved_state.non_stack_block_start;
	for (fr = MR_maxfr; fr > stop_addr; fr = MR_prevfr_slot(fr))
	{
		offset = MR_redoip_addr(fr) -
			consumer->saved_state.non_stack_block_start;
		clobber_addr = consumer->saved_state.non_stack_block + offset;
#if 0
		if (MR_tablestackdebug) {
			printf("redoip addr ");
			MR_printnondstackptr(MR_redoip_addr(fr));
			printf(", offset %d from start, ", offset);
			printf("saved copy at %p\n", clobber_addr);
		}
#endif

		if (fr == MR_gen_stack[cur_gen].generator_frame) {
			if (MR_gen_stack[cur_gen].generator_table == table) {
				/*
				** This is the nondet stack frame of the
				** generator corresponding to this consumer.
				*/

				assert(MR_prevfr_slot(fr) == (stop_addr - 1));
				*clobber_addr = (Word)
					ENTRY(mercury__table_nondet_resume_1_0);
#ifdef	MR_TABLE_DEBUG
				if (MR_tablestackdebug) {
					printf("completing redoip "
						"of frame at ");
					MR_printnondstackptr(fr);
					printf(" (in saved copy)\n");
				}
#endif

				consumer->saved_state.gen_next = cur_gen + 1;
#ifdef	MR_TABLE_DEBUG
				if (MR_tabledebug) {
					printf("saved gen_next set to %d\n",
						cur_gen + 1);
				}
#endif
			} else {
				/*
				** This is the nondet stack frame of some
				** other generator.
				*/

				assert(MR_prevfr_slot(fr) != (stop_addr - 1));

				*clobber_addr = (Word) ENTRY(do_fail);
#ifdef	MR_TABLE_DEBUG
				if (MR_tablestackdebug) {
					printf("clobbering redoip "
						"of frame at ");
					MR_printnondstackptr(fr);
					printf(" (in saved copy)\n");
				}
#endif

				save_transient_registers();
				make_subgoal_follow_leader(
					MR_gen_stack[cur_gen].generator_table,
					table);
				restore_transient_registers();
			}

			cur_gen--;
		} else if (cur_cut > 0 && fr == MR_cut_stack[cur_cut].frame) {
			*clobber_addr = (Word) ENTRY(MR_table_nondet_commit);
#ifdef	MR_TABLE_DEBUG
			if (MR_tablestackdebug) {
				printf("committing redoip of frame at ");
				MR_printnondstackptr(fr);
				printf(" (in saved copy)\n");
			}
#endif

			cur_cut--;
		} else {
			*clobber_addr = (Word) ENTRY(do_fail);
#ifdef	MR_TABLE_DEBUG
			if (MR_tablestackdebug) {
				printf("clobbering redoip of frame at ");
				MR_printnondstackptr(fr);
				printf(" (in saved copy)\n");
			}
#endif
		}
	}

#ifdef	MR_TABLE_DEBUG
	if (MR_tabledebug) {
		printf("adding suspension node %p to table %p",
			(void *) consumer, (void *) table);
		printf(" at slot %p\n", table->consumer_list_tail);
	}
#endif

	assert(*(table->consumer_list_tail) == NULL);
	listnode = table_allocate_bytes(sizeof(struct MR_ConsumerListNode));
	*(table->consumer_list_tail) = listnode;
	table->consumer_list_tail = &(listnode->next);
	listnode->item = consumer;
	listnode->next = NULL;
}
	MR_fail();
END_MODULE

MR_Subgoal	*MR_cur_leader;

/*
** The procedure defined below restores answers to suspended consumers.
** It works by restoring the consumer state saved by the consumer's call
** to table_nondet_suspend. By restoring such states and then returning
** answers, table_nondet_resume is essentially returning answers out of
** the call to table_nondet_suspend, not out of the call to
** table_nondet_resume.
**
** The code is arranged as a three level iteration to a fixpoint.
** The three levels are: iterating over all subgoals in a connected component,
** iterating over all consumers of each of those subgoals, and iterating
** over all the answers to be returned to each of those consumers.
** Note that returning an answer could lead to further answers for
** any of the subgoals in the connected component; it can even lead
** to the expansion of the component (i.e. the addition of more subgoals
** to it).
*/

Define_extern_entry(mercury__table_nondet_resume_1_0);
Declare_label(mercury__table_nondet_resume_1_0_ChangeLoop);
Declare_label(mercury__table_nondet_resume_1_0_ReachedFixpoint);
Declare_label(mercury__table_nondet_resume_1_0_LoopOverSuspensions);
Declare_label(mercury__table_nondet_resume_1_0_ReturnAnswer);
Declare_label(mercury__table_nondet_resume_1_0_RedoPoint);

MR_MAKE_PROC_LAYOUT(mercury__table_nondet_resume_1_0,
	MR_DETISM_NON, MR_ENTRY_NO_SLOT_COUNT, MR_LONG_LVAL_TYPE_UNKNOWN,
	MR_PREDICATE, "private_builtin", "table_nondet_resume", 1, 0);
MR_MAKE_INTERNAL_LAYOUT_WITH_ENTRY(
	mercury__table_nondet_resume_1_0_ChangeLoop,
	mercury__table_nondet_resume_1_0);
MR_MAKE_INTERNAL_LAYOUT_WITH_ENTRY(
	mercury__table_nondet_resume_1_0_ReachedFixpoint,
	mercury__table_nondet_resume_1_0);
MR_MAKE_INTERNAL_LAYOUT_WITH_ENTRY(
	mercury__table_nondet_resume_1_0_LoopOverSubgoals,
	mercury__table_nondet_resume_1_0);
MR_MAKE_INTERNAL_LAYOUT_WITH_ENTRY(
	mercury__table_nondet_resume_1_0_LoopOverSuspensions,
	mercury__table_nondet_resume_1_0);
MR_MAKE_INTERNAL_LAYOUT_WITH_ENTRY(
	mercury__table_nondet_resume_1_0_ReturnAnswer,
	mercury__table_nondet_resume_1_0);
MR_MAKE_INTERNAL_LAYOUT_WITH_ENTRY(
	mercury__table_nondet_resume_1_0_RedoPoint,
	mercury__table_nondet_resume_1_0);
MR_MAKE_INTERNAL_LAYOUT_WITH_ENTRY(
	mercury__table_nondet_resume_1_0_RestartPoint,
	mercury__table_nondet_resume_1_0);

BEGIN_MODULE(table_nondet_resume_module)
	init_entry_sl(mercury__table_nondet_resume_1_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury__table_nondet_resume_1_0);
	init_label_sl(mercury__table_nondet_resume_1_0_ChangeLoop);
	init_label_sl(mercury__table_nondet_resume_1_0_ReachedFixpoint);
	init_label_sl(mercury__table_nondet_resume_1_0_LoopOverSubgoals);
	init_label_sl(mercury__table_nondet_resume_1_0_LoopOverSuspensions);
	init_label_sl(mercury__table_nondet_resume_1_0_ReturnAnswer);
	init_label_sl(mercury__table_nondet_resume_1_0_RedoPoint);
	init_label_sl(mercury__table_nondet_resume_1_0_RestartPoint);
BEGIN_CODE

Define_entry(mercury__table_nondet_resume_1_0);
	MR_cur_leader = MR_top_generator_table();

	if (MR_cur_leader->leader != NULL) {
		/*
		** The predicate that called table_nondet_resume
		** is not the leader of its component.
		** We will leave all answers to be returned
		** by the leader.
		*/

#ifdef	MR_TABLE_DEBUG
		if (MR_tabledebug) {
			printf("non-leader table_nondet_resume fails\n");
		}
#endif

		(void) MR_pop_generator();
		MR_redo();
	}

#ifdef	MR_TABLE_DEBUG
	if (MR_tabledebug) {
		printf("table_nondet_resume enter: current leader is %p\n",
			MR_cur_leader);
	}
#endif

	if (MR_cur_leader->resume_info != NULL) {
#ifdef	MR_TABLE_DEBUG
		if (MR_tabledebug) {
			printf("using existing resume info %p\n",
				MR_cur_leader->resume_info);
		}
#endif
	} else {
		MR_cur_leader->resume_info = MR_GC_NEW(MR_ResumeInfo);

		save_transient_registers();
		save_state(&(MR_cur_leader->resume_info->leader_state),
			MR_cur_leader->generator_maxfr,
			MR_cur_leader->generator_sp,
			"resumption", "generator");
		restore_transient_registers();

#ifdef	MR_TABLE_DEBUG
		if (MR_tabledebug) {
			printf("creating new resume info %p\n",
				MR_cur_leader->resume_info);
		}
#endif
	}

	MR_cur_leader->resume_info->changed = TRUE;

Define_label(mercury__table_nondet_resume_1_0_ChangeLoop);

	if (MR_cur_leader->resume_info->changed) {
#ifdef	MR_TABLE_DEBUG
		if (MR_tabledebug) {
			printf("changed flag set\n");
		}
#endif
	} else {
		MR_SubgoalList	table_list;

		for (table_list = MR_cur_leader->resume_info->subgoal_list;
			table_list != NULL; table_list = table_list->next)
		{
			if (table_list->item->num_committed_ans
				!= table_list->item->num_ans)
			{
				MR_cur_leader->resume_info->changed = TRUE;
#ifdef	MR_TABLE_DEBUG
				if (MR_tabledebug) {
					printf("table %p has new answers\n",
						table_list->item);
				}
#endif
			}
		}
	}

	if (! MR_cur_leader->resume_info->changed) {
#ifdef	MR_TABLE_DEBUG
		if (MR_tabledebug) {
			printf("no more changes\n");
		}
#endif
		GOTO_LABEL(mercury__table_nondet_resume_1_0_ReachedFixpoint);
	}

	MR_cur_leader->resume_info->subgoal_list = MR_cur_leader->followers;

	/* For each of the subgoals on our list of followers */
Define_label(mercury__table_nondet_resume_1_0_LoopOverSubgoals);

	if (MR_cur_leader->resume_info->subgoal_list == NULL) {
#ifdef	MR_TABLE_DEBUG
		if (MR_tabledebug) {
			printf("no more subgoals in the followers list\n");
		}
#endif

		GOTO_LABEL(mercury__table_nondet_resume_1_0_ChangeLoop);
	}

	MR_cur_leader->resume_info->cur_subgoal =
		MR_cur_leader->resume_info->subgoal_list->item;
	MR_cur_leader->resume_info->subgoal_list =
		MR_cur_leader->resume_info->subgoal_list->next;

	MR_cur_leader->resume_info->consumer_list =
		MR_cur_leader->resume_info->cur_subgoal->consumer_list;

	MR_cur_leader->resume_info->changed = FALSE;
	MR_cur_leader->resume_info->cur_subgoal->num_committed_ans =
		MR_cur_leader->resume_info->cur_subgoal->num_ans;

	/* For each of the suspended nodes for cur_subgoal */
Define_label(mercury__table_nondet_resume_1_0_LoopOverSuspensions);

	if (MR_cur_leader->resume_info->consumer_list == NULL) {
#ifdef	MR_TABLE_DEBUG
		if (MR_tabledebug) {
			printf("no more suspensions for current subgoal\n");
		}
#endif
		GOTO_LABEL(mercury__table_nondet_resume_1_0_LoopOverSubgoals);
	}

	MR_cur_leader->resume_info->cur_consumer =
		MR_cur_leader->resume_info->consumer_list->item;
	MR_cur_leader->resume_info->consumer_list =
		MR_cur_leader->resume_info->consumer_list->next;

	MR_cur_leader->resume_info->cur_consumer_answer_list =
		*(MR_cur_leader->resume_info->cur_consumer->
			remaining_answer_list_ptr);

	if (MR_cur_leader->resume_info->cur_consumer_answer_list == NULL) {
#ifdef	MR_TABLE_DEBUG
		if (MR_tabledebug) {
			printf("no first answer for this suspension\n");
		}
#endif
		GOTO_LABEL(mercury__table_nondet_resume_1_0_LoopOverSuspensions);
	}

#ifdef	MR_TABLE_DEBUG
	if (MR_tabledebug) {
		printf("resuming consumer %p from table %p\n",
			(void *) MR_cur_leader->resume_info->cur_consumer,
			(void *) MR_cur_leader->resume_info->cur_subgoal);
	}
#endif

	save_transient_registers();
	restore_state(
		&(MR_cur_leader->resume_info->cur_consumer->saved_state),
		"resumption", "consumer");
	restore_transient_registers();

	/* check that there is room for exactly one framevar */
	assert((MR_maxfr - MR_prevfr_slot(MR_maxfr)) ==
		(MR_NONDET_FIXED_SIZE + 1));

	MR_gen_next = MR_cur_leader->resume_info->leader_state.gen_next;
	MR_redoip_slot(MR_maxfr) =
		LABEL(mercury__table_nondet_resume_1_0_RedoPoint);
	MR_redofr_slot(MR_maxfr) = MR_maxfr;
	MR_based_framevar(MR_maxfr, 1) = (Word) MR_cur_leader;

Define_label(mercury__table_nondet_resume_1_0_ReturnAnswer);

	/*
	** Return the next answer in MR_cur_leader->resume_info->
	** cur_consumer_answer_list to the current consumer. Since we have
	** already restored the context of the suspended consumer before
	** we returned the first answer, we don't need to restore it again,
	** since will not have changed in the meantime.
	*/


	r1 = (Word) &MR_cur_leader->resume_info->cur_consumer_answer_list->
		answer_data;

	MR_cur_leader->resume_info->cur_consumer->remaining_answer_list_ptr =
		&(MR_cur_leader->resume_info->cur_consumer_answer_list->
		next_answer);

	MR_cur_leader->resume_info->cur_consumer_answer_list =
		MR_cur_leader->resume_info->cur_consumer_answer_list->
		next_answer;

	/*
	** Return the answer. Since we just restored the state of the
	** computation that existed when suspend was called, the code
	** that we return to is the code following the call to suspend.
	*/
	MR_succeed();

Define_label(mercury__table_nondet_resume_1_0_RedoPoint);
	update_prof_current_proc(LABEL(mercury__table_nondet_resume_1_0));

	/*
	** This is where the current consumer suspension will go on
	** backtracking when it wants the next solution. If there is a solution
	** we haven't returned to this consumer yet, we do so, otherwise we
	** remember how many answers we have returned to this consumer so far
	** and move on to the next suspended consumer of the current subgoal.
	*/

	MR_cur_leader = (MR_Subgoal *) MR_based_framevar(MR_maxfr, 1);

Define_label(mercury__table_nondet_resume_1_0_RestartPoint);
#ifdef	MR_TABLE_DEBUG
	if (MR_tabledebug) {
		printf("cur_consumer_answer_list: %p\n",
			MR_cur_leader->resume_info->cur_consumer_answer_list);
		printf("*cur_consumer->remaining_answer_list_ptr: %p\n",
			*(MR_cur_leader->resume_info->cur_consumer->
				remaining_answer_list_ptr));
	}
#endif

	if (MR_cur_leader->resume_info->cur_consumer_answer_list != NULL) {
		GOTO_LABEL(mercury__table_nondet_resume_1_0_ReturnAnswer);
	}

#ifdef	MR_TABLE_DEBUG
	if (MR_tabledebug) {
		printf("no more unreturned answers for this suspension\n");
	}
#endif

	if (MR_cur_leader->resume_info->cur_subgoal->num_committed_ans
		!= MR_cur_leader->resume_info->cur_subgoal->num_ans)
	{
		MR_cur_leader->resume_info->changed = TRUE;
	}

	GOTO_LABEL(mercury__table_nondet_resume_1_0_LoopOverSuspensions);

Define_label(mercury__table_nondet_resume_1_0_ReachedFixpoint);
	{
		MR_SubgoalList	table_list;

		for (table_list = MR_cur_leader->followers;
			table_list != NULL; table_list = table_list->next)
		{
#ifdef	MR_TABLE_DEBUG
			if (MR_tabledebug) {
				printf("marking table %p complete\n",
					table_list->item);
			}
#endif

			table_list->item->status = MR_SUBGOAL_COMPLETE;
			table_list->item->num_committed_ans = -1;
		}
	}

	/* Restore the state we had when table_nondet_resume was called */
	save_transient_registers();
	restore_state(&(MR_cur_leader->resume_info->leader_state),
		"resumption", "generator");
	restore_transient_registers();

	/* XXX we should free this cell and its components */
	MR_cur_leader->resume_info = NULL;

	/* We are done with this generator */
	(void) MR_pop_generator();

	proceed();
END_MODULE

Define_extern_entry(MR_table_nondet_commit);
BEGIN_MODULE(table_nondet_commit_module)
	init_entry_ai(MR_table_nondet_commit);
BEGIN_CODE
Define_entry(MR_table_nondet_commit);
	MR_commit_cut();
	MR_fail();
END_MODULE

#endif

/* Ensure that the initialization code for the above modules gets to run. */
/*
INIT mercury_sys_init_table_modules
*/

#ifdef	MR_USE_MINIMAL_MODEL
extern ModuleFunc table_nondet_suspend_module;
extern ModuleFunc table_nondet_resume_module;
extern ModuleFunc table_nondet_commit_module;
#endif

void mercury_sys_init_table_modules(void);
	/* extra declaration to suppress gcc -Wmissing-decl warning */
void mercury_sys_init_table_modules(void) {
#ifdef	MR_USE_MINIMAL_MODEL
	table_nondet_suspend_module();
	table_nondet_resume_module();
	table_nondet_commit_module();
#endif
}
