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
	int	ta_size;
	List	**ta_store;
	Cast	(*ta_key)(Cast);	 /* applied to entries */
	int	(*ta_hash)(Cast);	 /* applied to keys */
	bool	(*ta_equal)(Cast, Cast); /* applied to two keys */
} Table;

#define	TABLESIZE		(97)
#define	init_table(t)		tab_init_table(&t)
#define	lookup_table(t, k)	tab_lookup_table(&t, (const Cast) k)
#define	insert_table(t, e)	tab_insert_table(&t, (const Cast) e)
#define	get_all_entries(t)	tab_get_all_entries(&t)
#define	hash(val)		tab_hash((const Cast) val)

#define	tablekey(table)		(*(table->ta_key))
#define	tablehash(table)	(*(table->ta_hash))
#define	tableequal(table)	(*(table->ta_equal))

extern	void	tab_init_table(Table *);
extern	Cast	tab_lookup_table(const Table *, const Cast);
extern	bool	tab_insert_table(const Table *, const Cast);
extern	List	*tab_get_all_entries(const Table *);
extern	int	tab_hash(Cast);

#endif
