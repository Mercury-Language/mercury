/*
** Copyright (C) 1993-1995, 1997 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_table.h - defines the interface to the hash table module.
*/

#ifndef	MERCURY_TABLE_H
#define	MERCURY_TABLE_H

#include "mercury_std.h"	/* for bool */
#include "mercury_dlist.h"	/* for List */

typedef	struct	s_table {
	int		ta_size;
	List		**ta_store;
	const void *	(*ta_key)(const void *);    /* applied to entries */
	int		(*ta_hash)(const void *);   /* applied to keys */
	bool		(*ta_equal)(const void *, const void *);
						    /* applied to two keys */
} Table;

#define	init_table(t)		tab_init_table(&t)
#define	lookup_table(t, k)	tab_lookup_table(&t, (const void *) k)
#define	insert_table(t, e)	tab_insert_table(&t, (void *) e)
#define	get_all_entries(t)	tab_get_all_entries(&t)
#define	str_to_int(val)		tab_str_to_int(val)

#define	tablekey(table)		(*(table->ta_key))
#define	tablehash(table)	(*(table->ta_hash))
#define	tableequal(table)	(*(table->ta_equal))

extern	void	tab_init_table(Table *);
extern	void *	tab_lookup_table(const Table *, const void *);
extern	bool	tab_insert_table(const Table *, void *);
extern	List	*tab_get_all_entries(const Table *);
extern	int	tab_str_to_int(const char *);

#endif /* not MERCURY_TABLE_H */
