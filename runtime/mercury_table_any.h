/*
** Copyright (C) 1997-1998 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

#ifndef MERCURY_TABLE_ANY_H
#define MERCURY_TABLE_ANY_H

/*
** This function will lookup or insert any type of value into a 
** table. It uses the provided type_info to extract the necessary
** info to do this. It returns a pointer to the node found by the 
** insertion/lookup.
*/

TrieNode MR_table_type(TrieNode Table, Word *type_info, Word data_value);

#endif /* not MERCURY_TABLE_ANY_H */
