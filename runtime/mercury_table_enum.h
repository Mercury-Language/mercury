#ifndef MERCURY_TABLE_ENUM_H
#define MERCURY_TABLE_ENUM_H

/*
**  MR_int_index_lookup_or_add() : This function maintains a simple indexed 
**	table of size Range. The return value is a pointer to the table
** 	node found by the lookup/insert. 
*/
TrieNode MR_int_index_lookup_or_add(TrieNode, Integer, Integer);

#endif /* not MERCURY_TABLE_ENUM_H */
