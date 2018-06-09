// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1995, 1997, 1999-2000, 2004-2006 The University of Melbourne.
// Copyright (C) 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// Linked list module.

#include    "mercury_imp.h"
#include    <stdio.h>
#include    "mercury_memory.h"
#include    "mercury_dlist.h"

MR_Dlist *
MR_dlist_makelist0(void)
{
    MR_Dlist    *list;

    list = MR_GC_NEW_ATTRIB(MR_Dlist, MR_ALLOC_SITE_RUNTIME);
    MR_dlist_data(list) = NULL;
    MR_dlist_next(list) = list;
    MR_dlist_prev(list) = list;

    return list;
}

MR_Dlist *
MR_dlist_makelist(const void *data)
{
    MR_Dlist    *list;

    MR_assert(data != NULL);
    list = MR_dlist_makelist0();
    MR_dlist_addhead(list, data);
    return list;
}

MR_Dlist *
MR_dlist_addhead(MR_Dlist *list, const void *data)
{
    MR_Dlist    *item;

    if (list == NULL) {
        list = MR_dlist_makelist0();
    }

    item = MR_GC_NEW_ATTRIB(MR_Dlist, MR_ALLOC_SITE_RUNTIME);
    MR_dlist_data(item) = data;
    MR_dlist_length_field(list)++;

    // Item's pointers.
    MR_dlist_next(item) = MR_dlist_next(list);
    MR_dlist_prev(item) = list;
    // Neighbours' pointers.
    MR_dlist_next(MR_dlist_prev(item)) = item;
    MR_dlist_prev(MR_dlist_next(item)) = item;

    return list;
}

MR_Dlist *
MR_dlist_addtail(MR_Dlist *list, const void *data)
{
    MR_Dlist    *item;

    if (list == NULL) {
        list = MR_dlist_makelist0();
    }

    item = MR_GC_NEW_ATTRIB(MR_Dlist, MR_ALLOC_SITE_RUNTIME);
    MR_dlist_data(item) = data;
    MR_dlist_length_field(list)++;

    // Item's pointers.
    MR_dlist_next(item) = list;
    MR_dlist_prev(item) = MR_dlist_prev(list);
    // Neighbours' pointers.
    MR_dlist_next(MR_dlist_prev(item)) = item;
    MR_dlist_prev(MR_dlist_next(item)) = item;

    return list;
}

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
            // Pointers from header.
            MR_dlist_next(list1) = MR_dlist_next(list2);
            MR_dlist_prev(list1) = MR_dlist_prev(list2);
            // Pointers to header.
            MR_dlist_prev(MR_dlist_next(list1)) = list1;
            MR_dlist_next(MR_dlist_prev(list1)) = list1;
        } else {
            MR_dlist_length_field(list1) = MR_dlist_length(list1)
                + MR_dlist_length(list2);
            // End of list 1 to start of list 2.
            MR_dlist_next(MR_dlist_prev(list1)) = MR_dlist_next(list2);
            MR_dlist_prev(MR_dlist_next(list2)) = MR_dlist_prev(list1);
            // End of list 2 to start of list 1.
            MR_dlist_next(MR_dlist_prev(list2)) = list1;
            MR_dlist_prev(list1) = MR_dlist_prev(list2);
        }
    }

    MR_GC_free(list2);
    return list1;
}

MR_Dlist *
MR_dlist_addndlist(MR_Dlist *list1, MR_Dlist *list2)
{
    MR_Dlist    *ptr;

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

void
MR_dlist_insert_before(MR_Dlist *list, MR_Dlist *where, const void *data)
{
    MR_Dlist    *item;

    item = MR_GC_NEW_ATTRIB(MR_Dlist, MR_ALLOC_SITE_RUNTIME);
    MR_dlist_data(item) = data;
    MR_dlist_length_field(list)++;

    // Item's pointers.
    MR_dlist_next(item) = where;
    MR_dlist_prev(item) = MR_dlist_prev(where);
    // Neighbour's pointers.
    MR_dlist_next(MR_dlist_prev(item)) = item;
    MR_dlist_prev(MR_dlist_next(item)) = item;
}

void
MR_dlist_insert_after(MR_Dlist *list, MR_Dlist *where, const void *data)
{
    MR_Dlist    *item;

    item = MR_GC_NEW_ATTRIB(MR_Dlist, MR_ALLOC_SITE_RUNTIME);
    MR_dlist_data(item) = data;
    MR_dlist_length_field(list)++;

    // Item's pointers.
    MR_dlist_next(item) = MR_dlist_next(where);
    MR_dlist_prev(item) = where;
    // Neighbour's pointers.
    MR_dlist_next(MR_dlist_prev(item)) = item;
    MR_dlist_prev(MR_dlist_next(item)) = item;

    return;
}

//Return the length of a given list.

int
MR_dlist_maybe_null_length(const MR_Dlist *list)
{
    if (list == NULL) {
        return 0;
    }

    return MR_dlist_length(list);
}

// Delete an item from its linked list, and free the node,
// and maybe the data.

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

void
MR_dlist_oldlist(MR_Dlist *list, void (* func)(const void *))
{
    MR_Dlist    *ptr;
    MR_Dlist    *item;

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
