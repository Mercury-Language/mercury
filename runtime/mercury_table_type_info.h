/*
** Copyright (C) 1997-1998 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

#ifndef MERCURY_TABLE_TYPE_INFO
#define MERCURY_TABLE_TYPE_INFO

/*
** Lookup or insert the given type_info into the given table. Return a 
** pointer to the node of the table reached by the lookup/insert. 
*/
TrieNode MR_type_info_lookup_or_add(TrieNode, Word *);

#endif /* not MERCURY_TABLE_TYPE_INFO */
