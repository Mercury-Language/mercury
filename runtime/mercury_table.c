/*
** Copyright (C) 1993-1997 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
**	Table handling module.
**
**	This file supplies data manipulation routines to other modules;
**	it does not store any data itself. Its routines are generic,
**	applicable to the storage of any kind of data structure with
**	a primary key and a hash function on it.
*/

#define	HASHDEBUG

#include	"mercury_imp.h" 

#include	<stdio.h>
#include	"mercury_std.h"
#include	"mercury_dlist.h"
#include	"mercury_table.h"

/*
**	Initialize a table.
*/

void 
tab_init_table(Table *table)
{
	reg	int	i;

	table->ta_store = make_many(List *, table->ta_size);

	for (i = 0; i < table->ta_size; i++) {
		table->ta_store[i] = NULL;
	}
}

/*
**	Look up and return the entry corresponding to the key
**	in a table.
*/

void *
tab_lookup_table(const Table *table, const void *key)
{
	reg	List	*ptr;
	reg	int	h;

	h = tablehash(table)(key);

#ifdef	HASHDEBUG
	if (! (0 <= h && h < table->ta_size)) {
		fprintf(stderr, "internal error: bad hash index in "
			"lookup_table: %d, table size %d\n", 
			h, table->ta_size);
	}
#endif

	for_list (ptr, table->ta_store[h]) {
		if (tableequal(table)(key, tablekey(table)(ldata(ptr)))) {
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
tab_insert_table(const Table *table, void *entry)
{
	reg	List		*ptr;
	reg	const void	*key;
	reg	int		h;

	key = tablekey(table)(entry);
	h   = tablehash(table)(key);

#ifdef	HASHDEBUG
	if (! (0 <= h && h < table->ta_size)) {
		fprintf(stderr, "internal error: bad hash index in "
			"lookup_table: %d, table size %d\n", 
			h, table->ta_size);
	}
#endif

	for_list (ptr, table->ta_store[h]) {
		if (tableequal(table)(key, tablekey(table)(ldata(ptr)))) {
			return TRUE;
		}
	}

	table->ta_store[h] = addhead(table->ta_store[h], entry);
	return FALSE;
}

/*
**	Return all table entries in a list.
*/

List *
tab_get_all_entries(const Table *table)
{
	reg	List	*list;
	reg	int	i;

	list = makelist0();
	for (i = 0; i < table->ta_size; i++) {
		addndlist(list, table->ta_store[i]);
	}

	return list;
}

/*
**	Convert a string to a positive int. The return value
**	mod the table size is a good hash value.
*/

int 
tab_str_to_int(const char *cs)
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
