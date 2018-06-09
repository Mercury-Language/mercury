// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1995-1997,2000, 2004-2005 The University of Melbourne.
// Copyright (C) 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// mercury_dlist.h - defines a doubly-linked list type.

#ifndef MERCURY_DLIST_H
#define MERCURY_DLIST_H

// The lists we use are doubly-linked.
// They each have a header node, and we store the length of the list
// in the header node.

typedef struct  MR_Dlist_Struct MR_Dlist;

struct  MR_Dlist_Struct {
    union {
        const void  *MR_dlist_data;
        int         MR_dlist_length;
    } MR_dlist_union;
    MR_Dlist        *MR_dlist_prev;
    MR_Dlist        *MR_dlist_next;
};

// Make an empty list.

extern  MR_Dlist    *MR_dlist_makelist0(void);

// Make a list with "data" as its only element.

extern  MR_Dlist    *MR_dlist_makelist(const void *data);

// Add "data" to the head of "list".

extern  MR_Dlist    *MR_dlist_addhead(MR_Dlist *list, const void *data);

// Add "data" to the tail of "list".

extern  MR_Dlist    *MR_dlist_addtail(MR_Dlist *list, const void *data);

// Destructively append list2 to list1. Since the header of list2 is not
// meaningful after the operation, it is freed.

extern  MR_Dlist    *MR_dlist_addlist(MR_Dlist *list1, MR_Dlist *list2);

// (Semi-) nondestructively append list2 to list1. The header of list1 is
// indeed altered, but only the data pointers of list2 are used.

extern  MR_Dlist    *MR_dlist_addndlist(MR_Dlist *list1, MR_Dlist *list2);

// Insert "data" into "list" before the element given by "where".

extern  void        MR_dlist_insert_before(MR_Dlist *list, MR_Dlist *where,
                        const void *data);

// Insert "data" into "list" after the element given by "where".

extern  void        MR_dlist_insert_after(MR_Dlist *list, MR_Dlist *where,
                        const void *data);

// Return the length of "list".

extern  int         MR_dlist_maybe_null_length(const MR_Dlist *list);

// Delete the node given by "item" from its linked list "list". Free this node,
// and, if "func" is non-NULL, use it to free the data item in that node.

extern  void        MR_dlist_delete(MR_Dlist *list, MR_Dlist *item,
                        void (* func)(const void *));

// Free the whole list, including the header. If func is non-NULL, then use it
// to free the data items pointed to by the list nodes.

extern  void        MR_dlist_oldlist(MR_Dlist *list,
                        void (* func)(const void *));

// Macros to make programming with this module easier.

#define MR_dlist_next(ptr)      (ptr)->MR_dlist_next
#define MR_dlist_prev(ptr)      (ptr)->MR_dlist_prev
#define MR_dlist_data(ptr)      (ptr)->MR_dlist_union.MR_dlist_data
#define MR_dlist_length_field(list) ((list)->MR_dlist_union.            \
                                        MR_dlist_length)
#define MR_dlist_length(list)       ((list) != NULL ?                   \
                                        MR_dlist_length_field(list)     \
                                        : 0)
#define MR_dlist_first_ptr(list)    ((list)->MR_dlist_next)
#define MR_dlist_last_ptr(list)     ((list)->MR_dlist_prev)
#define MR_dlist_first(list)        ((list)->MR_dlist_next->            \
                                        MR_dlist_union.MR_dlist_data)
#define MR_dlist_last(list)         ((list)->MR_dlist_prev->            \
                                        MR_dlist_union.MR_dlist_data)

#define MR_for_dlist(p, l)                                              \
    for (                                                               \
        (p) = ((l)? MR_dlist_next((l)): NULL);                          \
        (p) != (l) && (p) != NULL;                                      \
        (p) = MR_dlist_next((p))                                        \
    )
#define MR_for_2dlist(p1, p2, l1, l2)                                   \
    for (                                                               \
        (p1) = ((l1)? MR_dlist_next((l1)): NULL),                       \
            (p2) = ((l2)? MR_dlist_next((l2)): NULL);                   \
        (p1) != (l1) && (p1) != NULL && (p2) != (l2) && (p2) != NULL;   \
        (p1) = MR_dlist_next((p1)), (p2) = MR_dlist_next((p2))          \
    )
#define MR_for_undlist(p, np, l)                                        \
    for (                                                               \
        (p) = ((l)? MR_dlist_next((l)): NULL),                          \
            (np) = ((p)? MR_dlist_next((p)): NULL);                     \
        (p) != (l) && (p) != NULL;                                      \
        (p) = (np), (np) = ((p)? MR_dlist_next((p)): NULL)              \
    )
#define MR_end_dlist(p, l)                                              \
    ((p) == (l) || (p) == NULL)

#endif // not MERCURY_DLIST_H
