
/*
** Copyright (C) 1997 University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
**
** $Id: dict.c,v 1.1 1997-05-28 05:58:55 aet Exp $
*/

/* Imports */
#include	<stdlib.h>
#include	<unistd.h>
#include	<assert.h>

#include	"mem.h"
#include	"util.h"

#include	"dict.h"

/* Exported definitions */

/* Local declarations */

static char
rcs_id[]	= "$Id: dict.c,v 1.1 1997-05-28 05:58:55 aet Exp $";

static p_Dict_Item *
insert(KeyComparison cmp, void *key, void *val, p_Dict_Item *items);

static MB_Bool
lookup(KeyComparison cmp, void *key, p_Dict_Item *items, void **val_p);

static p_Dict_Item *
delete(KeyComparison cmp, void *key, p_Dict_Item *items);

static p_Dict_Item *
new_item_p(void *key, void *val, p_Dict_Item* next);

/* Implementation */

/*
**	XXX: Yes, this implementation is hopelessly inefficient.
**	I'll replace it with a hash table when things are working.
*/

Dict
dict_new(KeyComparison key_cmp)
{
	Dict	*new_dict_p;

	new_dict_p = (Dict*) MB_malloc(sizeof(Dict));

	new_dict_p->p_key_cmp = key_cmp;
	new_dict_p->p_items = NULL;

	return *new_dict_p;
}

void
dict_insert(void *key, void *val, Dict *dict_p)
{
	assert(dict_p != NULL);
	assert(dict_p->p_key_cmp != NULL);

	dict_p->p_items = insert(dict_p->p_key_cmp, key, val, dict_p->p_items);
}

/*
** Insert items in ascending order sorted on keys.
*/
static p_Dict_Item *
insert(KeyComparison cmp, void *key, void *val, p_Dict_Item *items)
{
	if (items == NULL) {
		return new_item_p(key, val, NULL);
	} else if (cmp(key, items->p_key) < 0) {
		return new_item_p(key, val, items);
	} else if (cmp(key, items->p_key) > 0) {
		items->p_next = insert(cmp, key, val, items->p_next);
		return items;
	} else { /* keys are same */
		items->p_val = val;
		return items;
	}
}

MB_Bool
dict_lookup(void *key, Dict dict, void **val_p)
{
	assert (dict.p_key_cmp != NULL);
	return lookup(dict.p_key_cmp, key, dict.p_items, val_p);
}

static MB_Bool
lookup(KeyComparison cmp, void *key, p_Dict_Item *items, void **val_p)
{
	p_Dict_Item	*cur;

	cur = items;

	while (TRUE) {
		if (cur == NULL) {
			return FALSE;
		} else if (cmp(cur->p_key,key) == 0) {
			*val_p = cur->p_val;
			return TRUE;
		} else {
			cur = cur->p_next;
		}
	}
}

void
dict_delete(void *key, Dict *dict_p)
{
	assert(dict_p != NULL);
	assert(dict_p->p_key_cmp != NULL);

	dict_p->p_items = delete(dict_p->p_key_cmp, key, dict_p->p_items);
	return;
}

static p_Dict_Item *
delete(KeyComparison cmp, void *key, p_Dict_Item *items)
{
	if (items == NULL) {
		return NULL;
	} else if (cmp(key,items->p_key) == 0) {
		/*
		** XXX: Use Boehm GC to collect garbage node.
		*/
		return items->p_next;
	} else {
		p_Dict_Item	*new_items;

		items->p_next = delete(cmp, key, items->p_next);
		return items;
	}
}


KeyComparison
dict_key_compare(Dict dict)
{
	return dict.p_key_cmp;
}

static p_Dict_Item *
new_item_p(void *key, void *val, p_Dict_Item* next)
{
	p_Dict_Item	*item_p;

	item_p = (p_Dict_Item*) MB_malloc(sizeof(p_Dict_Item));

	item_p->p_key = key;
	item_p->p_val = val;
	item_p->p_next = next;

	assert(item_p != NULL);
	return item_p;
}
