#ifndef MERCURY_TABLE_TYPE_INFO
#define MERCURY_TABLE_TYPE_INFO

/*
** Lookup or insert the given type_info into the given table. Return a 
** pointer to the node of the table reached by the lookup/insert. 
*/
TrieNode MR_type_info_lookup_or_add(TrieNode, Word *);

#endif /* not MERCURY_TABLE_TYPE_INFO */
