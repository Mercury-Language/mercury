/*
** Copyright (C) 1997 University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
**
** $Id: dict.h,v 1.1 1997-05-28 05:58:59 aet Exp $
*/


#ifndef MB_DICT_H
#define	MB_DICT_H

/*
**	A simple abstract data type for a key-value dictionary.
**
**	Note that this type is ABSTRACT. Client code must not refer to
**	any types or fields prefixed with "p_" since they are private 
**	(part of the implementation, not of the interface) and may be
**	changed or removed if the implementation changes. ADTs in C
**	are never pretty.
**
**	Given how slow the current implementation is, you can be sure
**	it will change.
**
**	Note: The "_p" -suffix- denotes a pointer, not a private type or data.
*/

typedef int (*KeyComparison)(const void *, const void *);

typedef struct p_Dict_Item {
	void			*p_key;
	void			*p_val;
	struct p_Dict_Item	*p_next;
} p_Dict_Item;
	

typedef struct Dict {
	KeyComparison	p_key_cmp;
	p_Dict_Item	*p_items;
} Dict;

/*
** Create a new dictionary that uses a given comparison function.
** The comparsion function works like strcmp. Specifically, key_cmp(x1,x2)
** returns negative if x1 < x2, zero if x1==x2, positive if x1>x2.
** XXX: Should also pass in some sort of string to identify the dictionary?
*/
Dict
dict_new(KeyComparison key_cmp);

/*
** Insert key-value pair into dictionary.
*/
void
dict_insert(void *key, void *val, Dict *dict_p);

/*
** Lookup value corresponding to key. Returns TRUE if lookup succeeded,
** FALSE if it failed.
*/
MB_Bool
dict_lookup(void *key, Dict dict, void **val_p);

/*
** Delete key-value pair corresponding to key.
*/
void
dict_delete(void *key, Dict *dict_p);

/*
** Return the key comparison function used by a dictionary.
*/
KeyComparison
dict_key_compare(Dict dict);

/*
** XXX: Need another function to allow us to iterate through the
** contents of a dictionary.
*/

#endif	/* MB_DICT_H */


