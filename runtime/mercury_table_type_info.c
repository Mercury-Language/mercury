/*
** Copyright (C) 1997 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** This module defines the type_info_lookup_or_add() function.
*/

#include "mercury_imp.h"
#include "mercury_table_type_info.h"

typedef struct TreeNode_struct {
	Word * key;
	Word value;
	struct TreeNode_struct * right;
	struct TreeNode_struct * left;
} TreeNode;

TrieNode
MR_type_info_lookup_or_add(TrieNode table, Word * type_info)
{
	TreeNode *p, *q;
	int i;

	if (*table == NULL) {
		p = table_allocate(sizeof(TreeNode));

		p->key = type_info;
		p->value = (Word) NULL;
		p->left = NULL;
		p->right = NULL;

		*table = (Word *) p;

		return (Word**) &p->value;
	}
	
	p = (TreeNode*) *table;

	while (p != NULL) {
		i = MR_compare_type_info((Word) p->key, (Word) type_info);

		if (i == COMPARE_EQUAL) {
			return (Word**) &p->value;
		} 
		
		q = p;
		
		if (i == COMPARE_LESS) {
			p = p->left;
		} else {
			p = p->right;
		}
	}

	p = table_allocate(sizeof(TreeNode));
	p->key = type_info;
	p->value = (Word) NULL; 
	p->left = NULL;
	p->right = NULL;

	if (i == COMPARE_LESS) {
		q->left = p;
	} else {
		q ->right = p;
	}
	
	return (Word**) &p->value;
}
