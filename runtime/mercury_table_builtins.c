/*
** Copyright (C) 1998 the University of Melbourne.
** this file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** this module defines the int_hash_lookup_or_add(), float_hash_lookup_or_add()
** 	and string_hash_lookup_or_add() function.
*/

#include "mercury_imp.h"

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
		table_allocate(sizeof(Word) * 2 + table_size * 
			sizeof(TableNode *));
	
	table->size = table_size;
	table->used_elements = 0;

	for (i=0; i<table_size; i++) {
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

	p = table_allocate(sizeof(TableNode));
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

	p = table_allocate(sizeof(TableNode));
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

	p = table_allocate(sizeof(TableNode));
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
