/*
** Copyright (C) 1993-1995, 1997-1998,2000 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** Defines the interface to the hash table module.
**
** Note that this module has nothing to do with the implementation
** of the "tabling" pragmas such as `pragma memo' -- the implementation
** of those features uses Tries, not hash tables, and is defined
** in mercury_tabling.h.
*/

#ifndef	MERCURY_HASH_TABLE_H
#define	MERCURY_HASH_TABLE_H

#include "mercury_std.h"	/* for MR_bool */
#include "mercury_dlist.h"	/* for MR_Dlist */

typedef	struct {
	int		MR_ht_size;
	MR_Dlist	**MR_ht_store;
	const void	*(*MR_ht_key)(const void *); /* applied to entries */
	int		(*MR_ht_hash)(const void *); /* applied to keys */
	MR_bool		(*MR_ht_equal)(const void *, const void *);
						     /* applied to two keys */
} MR_Hash_Table;

#define	MR_init_hash_table(t)		MR_ht_init_table(&t)
#define	MR_lookup_hash_table(t, k)	MR_ht_lookup_table(&t, (const void *) k)
#define	MR_insert_hash_table(t, e)	MR_ht_insert_table(&t, (void *) e)
#define	MR_get_all_entries(t)		MR_ht_get_all_entries(&t)
#define	MR_process_all_entries(t, f)	MR_ht_process_all_entries(&t, f)
#define	MR_str_to_int(val)		MR_ht_str_to_int(val)

#define	MR_tablekey(table)		(*(table->MR_ht_key))
#define	MR_tablehash(table)		(*(table->MR_ht_hash))
#define	MR_tableequal(table)		(*(table->MR_ht_equal))

extern	void		MR_ht_init_table(MR_Hash_Table *);
extern	const void	*MR_ht_lookup_table(const MR_Hash_Table *,
				const void *);
extern	MR_bool		MR_ht_insert_table(const MR_Hash_Table *, void *);
extern	MR_Dlist	*MR_ht_get_all_entries(const MR_Hash_Table *);
extern	void		MR_ht_process_all_entries(const MR_Hash_Table *,
				void f(const void *));
extern	int		MR_ht_str_to_int(const char *);

#endif /* not MERCURY_HASH_TABLE_H */
