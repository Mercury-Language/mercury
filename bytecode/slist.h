
/*
** Copyright (C) 1997 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
**
** $Id: slist.h,v 1.2 1997-07-27 14:59:30 fjh Exp $
*/

/*
**	Minimal linked list implementation.
**
**	The exported type is named `SList' rather than `List' since
**	a) there's already a List type in runtime/dlist.h 
**	b) the lists are `S'ingly linked.
**
**	Note types and fields whose name is prefixed with "p_" are
**	private and client code should not refer to them.
*/


#ifndef MB_LIST_H
#define	MB_LIST_H

#include	"util.h" /* for MB_Bool */

typedef struct p_SList_Node {
	void			*p_head;
	struct p_SList_Node	*p_tail;
} p_SList_Node;

typedef struct p_SList_Node
	*SList;

SList
slist_nil(void);

MB_Bool
slist_null(SList list);

SList
slist_cons(void *head, SList tail);

void *
slist_head(SList list);

SList
slist_tail(SList list);

#endif	/* MB_LIST_H */
