/*
**	Linked list module.
*/

#include	<stdio.h>
#include	"std.h"
#include	"list.h"

/*
**	Make an empty list.
*/

List *
makelist0()
{
	reg	List	*list;

	list = make(List);
	ldata(list) = (Cast) 0;
	next(list) = list;
	prev(list) = list;
	
	return list;
}

/*
**	Make a list with the argument is its only element.
*/

List *
list_makelist(Cast data)
{
	reg	List	*list;

#ifdef	LISTDEBUG
	if (data == 0)
	{
		fprintf(stderr, "NULL passed to makelist\n");
		abort();
	}
#endif
	list = makelist0();
	addhead(list, data);
	return list;
}

/*
**	Add some data to the head of a list.
*/

List *
list_addhead(List *list, Cast data)
{
	reg	List	*item;

	if (list == NULL)
		list = makelist0();

	item = make(List);
	ldata(item) = data;
	ldata(list) = (Cast) ((int) ldata(list) + 1);

	/* item's pointers	*/
	next(item) = next(list);
	prev(item) = list;
	/* neighbours' pointers	*/
	next(prev(item)) = item;
	prev(next(item)) = item;

	return list;
}

/*
**	Add some data to the tail of a list.
*/

List *
list_addtail(List *list, Cast data)
{
	reg	List	*item;

	if (list == NULL)
		list = makelist0();

	item = make(List);
	ldata(item) = data;
	ldata(list) = (Cast) ((int) ldata(list) + 1);

	/* item's pointers	*/
	next(item) = list;
	prev(item) = prev(list);
	/* neighbours' pointers	*/
	next(prev(item)) = item;
	prev(next(item)) = item;

	return list;
}

/*
**	Destructively append list2 to list1. Since the header of
**	list2 is not meaningful after the operation, it is freed.
*/

List *
addlist(List *list1, List *list2)
{
	if (list1 == NULL)
		list1 = makelist0();

	if (list2 == NULL)
		list2 = makelist0();

	if (length(list2) > 0)
	{
		if (length(list1) == 0)
		{
			ldata(list1) = ldata(list2);
			/* pointers from header	*/
			next(list1) = next(list2);
			prev(list1) = prev(list2);
			/* pointers to header	*/
			prev(next(list1)) = list1;
			next(prev(list1)) = list1;
		}
		else
		{
			ldata(list1) = (Cast) ((int) ldata(list1) + (int) ldata(list2));
			/* end of list 1 to start of list 2	*/
			next(prev(list1)) = next(list2);
			prev(next(list2)) = prev(list1);
			/* end of list 2 to start of list 1	*/
			next(prev(list2)) = list1;
			prev(list1) = prev(list2);
		}
	}

	oldmem((Cast) list2);
	return list1;
}

/*
**	(Semi-) nondestructively append list2 to list1.
**	The header of the first list is indeed altered,
**	but only the data pointers of the second are used.
*/

List *
addndlist(List *list1, List *list2)
{
	reg	List	*ptr;

	if (list1 == NULL)
		list1 = makelist0();

	if (list2 == NULL)
		list2 = makelist0();

	for_list (ptr, list2)
		addtail(list1, ldata(ptr));

	return list1;
}

/*
**	Insert into a list before a given position.
*/

void
list_insert_before(List *list, List *where, Cast data)
{
	reg	List	*item;

	item = make(List);
	ldata(item) = (Cast) data;
	ldata(list) = (Cast) ((int) ldata(list) + 1);

	/* item's pointers */
	next(item) = where;
	prev(item) = prev(where);
	/* neighbour's pointers */
	next(prev(item)) = item;
	prev(next(item)) = item;
}

/*
**	Insert into a list after a given position.
*/

void
list_insert_after(List *list, List *where, Cast data)
{
	reg	List	*item;

	item = make(List);
	ldata(item) = (Cast) data;
	ldata(list) = (Cast) ((int) ldata(list) + 1);

	/* item's pointers */
	next(item) = next(where);
	prev(item) = where;
	/* neighbour's pointers */
	next(prev(item)) = item;
	prev(next(item)) = item;
}

/*
**	Return the length of a given list.
*/

int
length(List *list)
{
	if (list == NULL)
		return 0;

	return (int) ldata(list);
}

/*
**	Delete an item from its linked list, and free the node,
**	and maybe the data.
*/

void
delete(List *list, List *item, void (* func)(Cast))
{
	if (list == NULL)
		return;

	if (item == NULL)
		return;

	if (func != NULL)
		(* func)(ldata(item));

	ldata(list) = (Cast) ((int) ldata(list) - 1);
	next(prev(item)) = next(item);
	prev(next(item)) = prev(item);

	oldmem((Cast) item);
}

/*
**	Free a whole list, including the header and maybe the data
**	pointed to by the list. Of course, if they were not allocated
**	by newmem and malloc, then all Hell will break loose.
*/

void
oldlist(List *list, void (* func)(Cast))
{
	reg	List	*ptr;
	reg	List	*item;

	if (list == NULL)
		return;

	ptr = next(list);
	while (ptr != list)
	{
		item = ptr;
		ptr = next(ptr);

		if (func != NULL)
			(* func)(ldata(item));
 
		oldmem((Cast) item);
	}
	 
	oldmem((Cast) list);
}
