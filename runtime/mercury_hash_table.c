/*
** Copyright (C) 1993-1999 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
**	Hash table handling module.
**
**	This file supplies data manipulation routines to other modules;
**	it does not store any data itself. Its routines are generic,
**	applicable to the storage of any kind of data structure with
**	a primary key and a hash function on it.
*/

#include	"mercury_imp.h" 

#include	<stdio.h>
#include	"mercury_std.h"
#include	"mercury_dlist.h"
#include	"mercury_hash_table.h"

/*
**	Initialize a table.
*/

void 
MR_ht_init_table(MR_Hash_Table *table)
{
	reg	int	i;

	table->MR_ht_store = MR_GC_NEW_ARRAY(List *, table->MR_ht_size);

	for (i = 0; i < table->MR_ht_size; i++) {
		table->MR_ht_store[i] = NULL;
	}
}

/*
**	Look up and return the entry corresponding to the key
**	in a table.
*/

void *
MR_ht_lookup_table(const MR_Hash_Table *table, const void *key)
{
	reg	List	*ptr;
	reg	int	h;

	h = MR_tablehash(table)(key);

#ifdef	MR_HASHDEBUG
	if (! (0 <= h && h < table->MR_ht_size)) {
		fprintf(stderr, "internal error: bad hash index in "
			"lookup_table: %d, table size %d\n", 
			h, table->MR_ht_size);
	}
#endif

	for_list (ptr, table->MR_ht_store[h]) {
		if (MR_tableequal(table)(key, MR_tablekey(table)(ldata(ptr)))) {
			return ldata(ptr);
		}
	}

	return NULL;
}

/*
**	Insert a new entry into the table.
**	Return whether it was there before.
*/

bool 
MR_ht_insert_table(const MR_Hash_Table *table, void *entry)
{
	reg	List		*ptr;
	reg	const void	*key;
	reg	int		h;

	key = MR_tablekey(table)(entry);
	h   = MR_tablehash(table)(key);

#ifdef	MR_HASHDEBUG
	if (! (0 <= h && h < table->MR_ht_size)) {
		fprintf(stderr, "internal error: bad hash index in "
			"lookup_table: %d, table size %d\n", 
			h, table->MR_ht_size);
	}
#endif

	for_list (ptr, table->MR_ht_store[h]) {
		if (MR_tableequal(table)(key, MR_tablekey(table)(ldata(ptr)))) {
			return TRUE;
		}
	}

	table->MR_ht_store[h] = addhead(table->MR_ht_store[h], entry);
	return FALSE;
}

/*
**	Return all table entries in a list.
*/

List *
MR_ht_get_all_entries(const MR_Hash_Table *table)
{
	reg	List	*list;
	reg	int	i;

	list = makelist0();
	for (i = 0; i < table->MR_ht_size; i++) {
		addndlist(list, table->MR_ht_store[i]);
	}

	return list;
}

/*
**	Process all table entries with the specified function.
*/

void
MR_ht_process_all_entries(const MR_Hash_Table *table, void f(const void *))
{
	reg	List	*ptr;
	reg	int	i;

	for (i = 0; i < table->MR_ht_size; i++) {
		for_list (ptr, table->MR_ht_store[i]) {
			f(ldata(ptr));
		}
	}
}

/*
**	Convert a string to a positive int. The return value
**	mod the table size is a good hash value.
*/

int 
MR_ht_str_to_int(const char *cs)
{
	reg	int		h;
	reg	const char	*s;

	s = cs;
	for (h = 0; *s != '\0'; s++) {
		h = (h << 1) + *s;
	}

	if (h < 0) {
		h = -h;
	}

	return h;
}
