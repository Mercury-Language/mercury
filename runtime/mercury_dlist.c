/*
** Copyright (C) 1995, 1997, 1999-2000, 2004 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
**	Linked list module.
*/

#include	"mercury_imp.h"
#include	<stdio.h>
#include	"mercury_memory.h"
#include	"mercury_dlist.h"

/*
**	Make an empty list.
*/

MR_Dlist *
MR_dlist_makelist0(void)
{
	MR_Dlist	*list;

	list = MR_GC_NEW(MR_Dlist);
	MR_dlist_data(list) = NULL;
	MR_dlist_next(list) = list;
	MR_dlist_prev(list) = list;
	
	return list;
}

/*
**	Make a list with the argument is its only element.
*/

MR_Dlist *
MR_dlist_makelist(const void *data)
{
	MR_Dlist	*list;

	MR_assert(data != NULL);
	list = MR_dlist_makelist0();
	MR_dlist_addhead(list, data);
	return list;
}

/*
**	Add some data to the head of a list.
*/

MR_Dlist *
MR_dlist_addhead(MR_Dlist *list, const void *data)
{
	MR_Dlist	*item;

	if (list == NULL) {
		list = MR_dlist_makelist0();
	}

	item = MR_GC_NEW(MR_Dlist);
	MR_dlist_data(item) = data;
	MR_dlist_length_field(list)++;

	/* item's pointers	*/
	MR_dlist_next(item) = MR_dlist_next(list);
	MR_dlist_prev(item) = list;
	/* neighbours' pointers	*/
	MR_dlist_next(MR_dlist_prev(item)) = item;
	MR_dlist_prev(MR_dlist_next(item)) = item;

	return list;
}

/*
**	Add some data to the tail of a list.
*/

MR_Dlist *
MR_dlist_addtail(MR_Dlist *list, const void *data)
{
	MR_Dlist	*item;

	if (list == NULL) {
		list = MR_dlist_makelist0();
	}

	item = MR_GC_NEW(MR_Dlist);
	MR_dlist_data(item) = data;
	MR_dlist_length_field(list)++;

	/* item's pointers	*/
	MR_dlist_next(item) = list;
	MR_dlist_prev(item) = MR_dlist_prev(list);
	/* neighbours' pointers	*/
	MR_dlist_next(MR_dlist_prev(item)) = item;
	MR_dlist_prev(MR_dlist_next(item)) = item;

	return list;
}

/*
**	Destructively append list2 to list1. Since the header of
**	list2 is not meaningful after the operation, it is freed.
*/

MR_Dlist *
MR_dlist_addlist(MR_Dlist *list1, MR_Dlist *list2)
{
	if (list1 == NULL) {
		list1 = MR_dlist_makelist0();
	}

	if (list2 == NULL) {
		list2 = MR_dlist_makelist0();
	}

	if (MR_dlist_length(list2) > 0) {
		if (MR_dlist_length(list1) == 0) {
			MR_dlist_data(list1) = MR_dlist_data(list2);
			/* pointers from header	*/
			MR_dlist_next(list1) = MR_dlist_next(list2);
			MR_dlist_prev(list1) = MR_dlist_prev(list2);
			/* pointers to header	*/
			MR_dlist_prev(MR_dlist_next(list1)) = list1;
			MR_dlist_next(MR_dlist_prev(list1)) = list1;
		} else {
			MR_dlist_length_field(list1) = MR_dlist_length(list1)
				+ MR_dlist_length(list2);
			/* end of list 1 to start of list 2	*/
			MR_dlist_next(MR_dlist_prev(list1)) =
				MR_dlist_next(list2);
			MR_dlist_prev(MR_dlist_next(list2)) =
				MR_dlist_prev(list1);
			/* end of list 2 to start of list 1	*/
			MR_dlist_next(MR_dlist_prev(list2)) = list1;
			MR_dlist_prev(list1) = MR_dlist_prev(list2);
		}
	}

	MR_GC_free(list2);
	return list1;
}

/*
**	(Semi-) nondestructively append list2 to list1.
**	The header of the first list is indeed altered,
**	but only the data pointers of the second are used.
*/

MR_Dlist *
MR_dlist_addndlist(MR_Dlist *list1, MR_Dlist *list2)
{
	MR_Dlist	*ptr;

	if (list1 == NULL) {
		list1 = MR_dlist_makelist0();
	}

	if (list2 == NULL) {
		list2 = MR_dlist_makelist0();
	}

	MR_for_dlist (ptr, list2) {
		MR_dlist_addtail(list1, MR_dlist_data(ptr));
	}

	return list1;
}

/*
**	Insert into a list before a given position.
*/

void 
MR_dlist_insert_before(MR_Dlist *list, MR_Dlist *where, const void *data)
{
	MR_Dlist	*item;

	item = MR_GC_NEW(MR_Dlist);
	MR_dlist_data(item) = data;
	MR_dlist_length_field(list)++;

	/* item's pointers */
	MR_dlist_next(item) = where;
	MR_dlist_prev(item) = MR_dlist_prev(where);
	/* neighbour's pointers */
	MR_dlist_next(MR_dlist_prev(item)) = item;
	MR_dlist_prev(MR_dlist_next(item)) = item;
}

/*
**	Insert into a list after a given position.
*/

void 
MR_dlist_insert_after(MR_Dlist *list, MR_Dlist *where, const void *data)
{
	MR_Dlist	*item;

	item = MR_GC_NEW(MR_Dlist);
	MR_dlist_data(item) = data;
	MR_dlist_length_field(list)++;

	/* item's pointers */
	MR_dlist_next(item) = MR_dlist_next(where);
	MR_dlist_prev(item) = where;
	/* neighbour's pointers */
	MR_dlist_next(MR_dlist_prev(item)) = item;
	MR_dlist_prev(MR_dlist_next(item)) = item;

	return;
}

/*
**	Return the length of a given list.
*/

int 
MR_dlist_maybe_null_length(const MR_Dlist *list)
{
	if (list == NULL) {
		return 0;
	}

	return MR_dlist_length(list);
}

/*
**	Delete an item from its linked list, and free the node,
**	and maybe the data.
*/

void 
MR_dlist_delete(MR_Dlist *list, MR_Dlist *item, void (* func)(const void *))
{
	if (list == NULL) {
		return;
	}

	if (item == NULL) {
		return;
	}

	if (func != NULL) {
		func(MR_dlist_data(item));
	}

	MR_dlist_length_field(list)--;
	MR_dlist_next(MR_dlist_prev(item)) = MR_dlist_next(item);
	MR_dlist_prev(MR_dlist_next(item)) = MR_dlist_prev(item);

	MR_GC_free(item);

	return;
}

/*
**	Free a whole list, including the header and maybe the data
**	pointed to by the list. Of course, if they were not allocated
**	by MR_GC_malloc, then all Hell will break loose.
*/

void 
MR_dlist_oldlist(MR_Dlist *list, void (* func)(const void *))
{
	MR_Dlist	*ptr;
	MR_Dlist	*item;

	if (list == NULL) {
		return;
	}

	ptr = MR_dlist_next(list);
	while (ptr != list) {
		item = ptr;
		ptr = MR_dlist_next(ptr);

		if (func != NULL) {
			func(MR_dlist_data(item));
		}
 
		MR_GC_free(item);
	}
	 
	MR_GC_free(list);

	return;
}
