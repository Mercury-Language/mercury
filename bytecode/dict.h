/*
** Copyright (C) 1997 University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
**
** $Id: dict.h,v 1.2 1997-05-29 09:52:58 aet Exp $
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
**	it will change. 8^)
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
** Return TRUE if a dictionary is empty, false otherwise.
*/
MB_Bool
dict_is_empty(Dict dict);

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
** Return the `first' key in the dictionary. In fact, this simply
** returns -any- key in the dictionary. This allows us to iterate
** over all elements in the dictionary. Procedure returns FALSE
** if there is no first key (dict is empty) and TRUE otherwise.
** The first key itself is returned through first_key_p.
*/
MB_Bool
dict_first_key(Dict dict, void **first_key_p);

/*
** In the given dictionary, returns the key following this_key.
** The next key is returned through next_key_p. Returns FALSE if
** there is no next key or this_key doesn't exist, TRUE otherwise.
*/
MB_Bool
dict_next_key(Dict dict, void *this_key, void **next_key_p);

#endif	/* MB_DICT_H */


