/*
** Copyright (C) 1997-1998 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

#ifndef MERCURY_TABLE_INT_FLOAT_STRING_H
#define MERCURY_TABLE_INT_FLOAT_STRING_H


/* 
** Look to see if the given integer key is in the given table. If it
** is return the address of the data pointer associated with the key.
** If it is not; create a new element for the key in the table and
** return the address of its data pointer.
**/
TrieNode MR_int_hash_lookup_or_add(TrieNode Table, Integer Key);

/* 
** Look to see if the given float key is in the given table. If it
** is return the address of the data pointer associated with the key.
** If it is not create a new element for the key in the table and
** return the address of its data pointer.
**/
TrieNode MR_float_hash_lookup_or_add(TrieNode Table, Float Key);

/* 
** Look to see if the given string key is in the given table. If it
** is return the address of the data pointer associated with the key.
** If it is not create a new element for the key in the table and
** return the address of its data pointer.
**/
TrieNode MR_string_hash_lookup_or_add(TrieNode Table, String Key);

#endif /* not MERCURY_TABLE_INT_FLOAT_STRING_H */
