/*
** Copyright (C) 1998 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** This module defines the MR_int_index_lookup_or_add() function.
*/

#include "mercury_imp.h"

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
