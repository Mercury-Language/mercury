/*
**	Table handling module.
**
**	This file supplies data manipulation routines to other modules;
**	it does not store any data itself. Its routines are generic,
**	applicable to the storage of any kind of data structure with
**	primary key and a hash function on it.
*/

#ifndef lint
static  char    rcs_id[] = "$Header: /srv/scratch/dev/togit/repository/mercury/runtime/Attic/table.c,v 1.2 1993-12-02 05:17:19 zs Exp $";
#endif

#include	<stdio.h>
#include	"std.h"
#include	"list.h"
#include	"table.h"

/*
**	Initialize a table.
*/

void
tab_init_table(Table *table)
{
	reg	int	i;

	table->ta_store = make_many(List *, table->ta_size);
	for (i = 0; i < table->ta_size; i++)
		table->ta_store[i] = NULL;
}

/*
**	Look up and return the entry corresponding to the key
**	in a table.
*/

Cast
tab_lookup_table(const Table *table, const Cast key)
{
	reg	List	*ptr;
	reg	int	h;

	h = tablehash(table)(key);

#ifdef	HASHDEBUG
	if (! (0 <= h && h < table->ta_size))
		fprintf(stderr, "internal error: bad hash index in lookup_table: 0x%x\n", h);
#endif

	for_list (ptr, table->ta_store[h])
		if (tableequal(table)(key, tablekey(table)(ldata(ptr))))
			return ldata(ptr);

	return NULL;
}

/*
**	Insert a new entry into the table.
**	Return whether it was there before.
*/

bool
tab_insert_table(const Table *table, const Cast entry)
{
	reg	List	*ptr;
	reg	Cast	key;
	reg	int	h;

	key = tablekey(table)(entry);
	h   = tablehash(table)(key);

#ifdef	HASHDEBUG
	if (! (0 <= h && h < table->ta_size))
		fprintf(stderr, "internal error: bad hash index in insert_table: 0x%x\n", h);
#endif

	for_list (ptr, table->ta_store[h])
		if (tableequal(table)(key, tablekey(table)(ldata(ptr))))
			return TRUE;

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
	for (i = 0; i < table->ta_size; i++)
		addndlist(list, table->ta_store[i]);

	return list;
}

/*
**	Hash str into the range 0 to SIZE-1.
*/

int
tab_hash(Cast cs)
{
	reg	int	h;
	reg	char	*s;

	s = (char *) cs;
	for (h = 0; *s != '\0'; s++)
		h = (h << 1) + *s;

	return ((unsigned) h) % TABLESIZE;
}
