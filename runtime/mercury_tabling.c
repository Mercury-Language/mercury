/*
** Copyright (C) 1997-1998 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

#include "mercury_imp.h"

#include "mercury_type_info.h"
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
TrieNode 
MR_int_hash_lookup_or_add(TrieNode t, Integer key)
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
TrieNode 
MR_float_hash_lookup_or_add(TrieNode t, Float key)
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
TrieNode 
MR_string_hash_lookup_or_add(TrieNode t, String key)
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

TrieNode
MR_int_index_lookup_or_add(TrieNode t, Integer range, Integer key)
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

TrieNode
MR_type_info_lookup_or_add(TrieNode table, Word * type_info)
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

MR_DECLARE_STRUCT(mercury_data___base_type_info_pred_0);
MR_DECLARE_STRUCT(mercury_data___base_type_info_func_0);

/*
** Due to the depth of the control here, we'll use 4 space indentation.
**
** NOTE : changes to this function will probably also have to be reflected
** in mercury_deep_copy.c and std_util::ML_expand().
*/

TrieNode
MR_table_type(TrieNode table, Word *type_info, Word data)
{
    Word *base_type_info, *base_type_layout, *base_type_functors;
    Word layout_for_tag, *layout_vector_for_tag, *data_value;
    enum MR_DataRepresentation data_rep;
    int data_tag, entry_tag;

    MR_MemoryList allocated_memory_cells = NULL;

    data_tag = tag(data);
    data_value = (Word *) body(data, data_tag);

    base_type_info = MR_TYPEINFO_GET_BASE_TYPEINFO(type_info);
    base_type_layout = MR_BASE_TYPEINFO_GET_TYPELAYOUT(base_type_info);
    base_type_functors = MR_BASE_TYPEINFO_GET_TYPEFUNCTORS(base_type_info);

    layout_for_tag = base_type_layout[data_tag];
    layout_vector_for_tag = (Word *) strip_tag(layout_for_tag);

    data_rep = MR_categorize_data(MR_TYPEFUNCTORS_INDICATOR(base_type_functors),
		    layout_for_tag);

#ifdef	MR_TABLE_DEBUG
    if (MR_tabledebug) {
	printf("ENTRY %p %x, data rep: %d\n", table, data, data_rep);
    }
#endif	/* MR_TABLE_DEBUG */

    switch (data_rep) {
        case MR_DATAREP_ENUM: {
	    int functors = MR_TYPELAYOUT_ENUM_VECTOR_NUM_FUNCTORS(
				layout_vector_for_tag);
	    MR_DEBUG_TABLE_ENUM(table, functors, data);
            break;
        }
        case MR_DATAREP_COMPLICATED_CONST: {
	    int functors = MR_TYPELAYOUT_ENUM_VECTOR_NUM_FUNCTORS(
				layout_vector_for_tag);
	    MR_DEBUG_TABLE_TAG(table, data_tag);
	    MR_DEBUG_TABLE_ENUM(table, functors, unmkbody(data));
            break;
        }
        case MR_DATAREP_SIMPLE: {
            int arity, i;
            Word *argument_vector, *type_info_vector, *new_type_info;

            argument_vector = data_value;

            arity = layout_vector_for_tag[TYPELAYOUT_SIMPLE_ARITY_OFFSET];
            type_info_vector = &layout_vector_for_tag[
		    		TYPELAYOUT_SIMPLE_ARGS_OFFSET];

	    MR_DEBUG_TABLE_TAG(table, data_tag);

                 /* copy arguments */
            for (i = 0; i < arity; i++) {
                new_type_info = MR_make_type_info(type_info,
                    (Word *) type_info_vector[i], &allocated_memory_cells);

                MR_DEBUG_TABLE_ANY(table, new_type_info, argument_vector[i]);
            }
            break;
        }
        case MR_DATAREP_COMPLICATED: {
            int arity, i;
            Word *argument_vector, *type_info_vector, *new_type_info;
            Word secondary_tag, num_sharers, *new_layout_vector;

            secondary_tag = *data_value;
            argument_vector = data_value + 1;

            num_sharers = MR_TYPELAYOUT_COMPLICATED_VECTOR_NUM_SHARERS(
            			layout_vector_for_tag);
            new_layout_vector =
                MR_TYPELAYOUT_COMPLICATED_VECTOR_GET_SIMPLE_VECTOR(
                    layout_vector_for_tag, secondary_tag);
            arity = new_layout_vector[TYPELAYOUT_SIMPLE_ARITY_OFFSET];
            type_info_vector =
		    &new_layout_vector[TYPELAYOUT_SIMPLE_ARGS_OFFSET];

	    MR_DEBUG_TABLE_TAG(table, data_tag);
	    MR_DEBUG_TABLE_ENUM(table, num_sharers, secondary_tag);

            for (i = 0; i < arity; i++) {
                new_type_info = MR_make_type_info(type_info,
                    (Word *) type_info_vector[i], &allocated_memory_cells);

                MR_DEBUG_TABLE_ANY(table, new_type_info, argument_vector[i]);
            }
            break;
        }
        case MR_DATAREP_NOTAG: {
            Word *new_type_info;
            new_type_info = MR_make_type_info(type_info,
                (Word *) *MR_TYPELAYOUT_NO_TAG_VECTOR_ARGS(
		    layout_vector_for_tag),
                &allocated_memory_cells);
            MR_DEBUG_TABLE_ANY(table, new_type_info, data);
            break;
        }
        case MR_DATAREP_EQUIV: {
            Word *new_type_info;
            new_type_info = MR_make_type_info(type_info,
                (Word *) MR_TYPELAYOUT_EQUIV_TYPE(layout_vector_for_tag),
                &allocated_memory_cells);
            MR_DEBUG_TABLE_ANY(table, new_type_info, data);
            break;
        }
        case MR_DATAREP_EQUIV_VAR:
            MR_DEBUG_TABLE_ANY(table,
		(Word *) type_info[(Word) layout_vector_for_tag], data);
            break;

        case MR_DATAREP_INT:
            MR_DEBUG_TABLE_INT(table, data);
            break;

        case MR_DATAREP_CHAR:
            MR_DEBUG_TABLE_CHAR(table, data);
            break;

        case MR_DATAREP_FLOAT:
            MR_DEBUG_TABLE_FLOAT(table, data);
            break;

        case MR_DATAREP_STRING:
            MR_DEBUG_TABLE_STRING(table, data);
            break;

        case MR_DATAREP_PRED: {
            int i;
            Word args = data_value[0];

            MR_DEBUG_TABLE_STRING(table, args);
            MR_DEBUG_TABLE_STRING(table, data_value[1]);

            for (i = 0; i < args; i++) {
        	MR_DEBUG_TABLE_ANY(table,
                    (Word *) type_info[i + TYPEINFO_OFFSET_FOR_PRED_ARGS],
                    data_value[i+2]);
            }
            break;
        }
        case MR_DATAREP_UNIV:
            MR_DEBUG_TABLE_TYPEINFO(table,
                (Word *) data_value[UNIV_OFFSET_FOR_TYPEINFO]);
            MR_DEBUG_TABLE_ANY(table,
                (Word *) data_value[UNIV_OFFSET_FOR_TYPEINFO],
                data_value[UNIV_OFFSET_FOR_DATA]);
            break;

        case MR_DATAREP_VOID:
            fatal_error("Cannot table a void type");
            break;

        case MR_DATAREP_ARRAY: {
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
        case MR_DATAREP_TYPEINFO:
            MR_DEBUG_TABLE_TYPEINFO(table, (Word *) data_value);
            break;

        case MR_DATAREP_C_POINTER:
            fatal_error("Attempt to use a C_POINTER tag in table");
            break;

        case MR_DATAREP_UNKNOWN: /* fallthru */
        default:
            fatal_error("Unknown layout tag in table_any");
            break;
    }

    MR_deallocate(allocated_memory_cells);

    return table;
} /* end table_any() */

/*---------------------------------------------------------------------------*/
