/*
**	Definitions for the table module.
*/

#ifndef	TABLE_H

#define	TABLE_H

#ifndef		LIST_H
#include	"list.h"
#endif

typedef	struct	s_table
{
	int		ta_size;
	List		**ta_store;
	const void *	(*ta_key)(const void *);    /* applied to entries */
	int		(*ta_hash)(const void *);   /* applied to keys */
	bool		(*ta_equal)(const void *, const void *);
						    /* applied to two keys */
} Table;

#define	TABLESIZE		(97)
#define	init_table(t)		tab_init_table(&t)
#define	lookup_table(t, k)	tab_lookup_table(&t, k)
#define	insert_table(t, e)	tab_insert_table(&t, e)
#define	get_all_entries(t)	tab_get_all_entries(&t)
#define	hash(val)		tab_hash(val)

#define	tablekey(table)		(*(table->ta_key))
#define	tablehash(table)	(*(table->ta_hash))
#define	tableequal(table)	(*(table->ta_equal))

extern	void	tab_init_table(Table *);
extern	void *	tab_lookup_table(const Table *, const void *);
extern	bool	tab_insert_table(const Table *, void *);
extern	List	*tab_get_all_entries(const Table *);
extern	int	tab_hash(const void *);

#endif
