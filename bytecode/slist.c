
/*
** Copyright (C) 1997 University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
**
** $Id: slist.c,v 1.1 1997-06-02 13:54:15 aet Exp $
*/

/* Imports */
#include	<assert.h> /* for assert */

#include	"mem.h" /* for MB_malloc */
#include	"slist.h"

/* Exported definitions */

/* Local declarations */

static char
rcs_id[]	= "$Id: slist.c,v 1.1 1997-06-02 13:54:15 aet Exp $";

/* Implementation */

SList
slist_nil()
{
	return (SList) NULL;
}

MB_Bool
slist_null(SList list)
{
	return NULL == list;
}

SList
slist_cons(void *head, SList tail)
{
	p_SList_Node	*tmp;

	tmp = (SList) MB_malloc(sizeof(p_SList_Node));

	tmp->p_head = head;
	tmp->p_tail = tail;

	return tmp;
}


void *
slist_head(SList list)
{
	assert(list != NULL); /* XXX */
	
	return list->p_head;
}

SList
slist_tail(SList list)
{
	assert(list != NULL); /* XXX */

	return list->p_tail;
}


