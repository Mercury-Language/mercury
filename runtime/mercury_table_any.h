#ifndef MERCURY_TABLE_ANY_H
#define MERCURY_TABLE_ANY_H

/*
** This function will lookup or insert any type of value into a 
** table. It uses the provided type_info to extract the necessary
** info to do this. It returns a pointer to the node found by the 
** insertion/lookup.
*/
TrieNode MR_table_type(Word *type_info, Word data_value, TrieNode Table);

#endif /* not MERCURY_TABLE_ANY_H */
