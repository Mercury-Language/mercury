/*
** Copyright (C) 1995-1997 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_dlist.h - defines a doubly-linked list type.
*/

#ifndef	MERCURY_DLIST_H
#define	MERCURY_DLIST_H

/*
** The lists we use are doubly-linked.
** They each have a header node, and we store the length of the list
** in the header node.
*/

typedef	struct	s_list {
	union {
		void 	*l_data;
		int	l_length;
	} l_union;
	struct	s_list	*l_prev;
	struct	s_list	*l_next;
} List;

#define	next(ptr)		(ptr)->l_next
#define	prev(ptr)		(ptr)->l_prev
#define	ldata(ptr)		(ptr)->l_union.l_data
#define	llength(list)		((list)->l_union.l_length)
#define	first_ptr(list)		((list)->l_next)
#define	last_ptr(list)		((list)->l_prev)
#define	first(list)		((list)->l_next->l_union.l_data)
#define	last(list)		((list)->l_prev->l_union.l_data)

#define	makelist(d)		list_makelist(d)
#define	addhead(l, d)		list_addhead(l, d)
#define	addtail(l, d)		list_addtail(l, d)
#define	insert_before(l, w, d)	list_insert_before(l, w, d)
#define	insert_after(l, w, d)	list_insert_after(l, w, d)

#define	for_list(p, l) \
	for (p = (l? next(l): NULL); p != l && p != NULL; p = next(p))
#define	for_2list(p1, p2, l1, l2) \
	for (	\
		p1 = (l1? next(l1): NULL), p2 = (l2? next(l2): NULL); \
		p1 != l1 && p1 != NULL && p2 != l2 && p2 != NULL; \
		p1 = next(p1), p2 = next(p2) \
	)
#define for_unlist(p, np, l) \
	for (p = (l? next(l): NULL), np = (p? next(p): NULL); \
		p != l && p != NULL; \
		p = np, np = (p? next(p): NULL))
#define	end_list(p, l)\
	(p == l || p == NULL)

extern	List	*makelist0(void);
extern	List	*list_makelist(void *);
extern	List	*list_addhead(List *, void *);
extern	List	*list_addtail(List *, void *);
extern	List	*addlist(List *, List *);
extern	List	*addndlist(List *, List *);
extern	void	list_insert_before(List *, List *, void *);
extern	void	list_insert_after(List *, List *, void *);
extern	int	length(const List *);
extern	void	dlist_delete(List *, List *, void (*)(void *));
extern	void	oldlist(List *, void (*)(void *));

#endif /* not MERCURY_DLIST_H */
