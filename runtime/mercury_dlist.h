/*
** Copyright (C) 1995-1997,2000, 2004 The University of Melbourne.
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

typedef	struct	MR_Dlist_Struct MR_Dlist;

struct	MR_Dlist_Struct {
	union {
		const void 	*MR_dlist_data;
		int		MR_dlist_length;
	} MR_dlist_union;
	MR_Dlist	*MR_dlist_prev;
	MR_Dlist	*MR_dlist_next;
};

#define	MR_dlist_next(ptr)		(ptr)->MR_dlist_next
#define	MR_dlist_prev(ptr)		(ptr)->MR_dlist_prev
#define	MR_dlist_data(ptr)		(ptr)->MR_dlist_union.MR_dlist_data
#define	MR_dlist_length(list)		((list)->MR_dlist_union.MR_dlist_length)
#define	MR_dlist_first_ptr(list)	((list)->MR_dlist_next)
#define	MR_dlist_last_ptr(list)		((list)->MR_dlist_prev)
#define	MR_dlist_first(list)		((list)->MR_dlist_next->	\
						MR_dlist_union.MR_dlist_data)
#define	MR_dlist_last(list)		((list)->MR_dlist_prev->	\
						MR_dlist_union.MR_dlist_data)

#define	MR_for_dlist(p, l) 						\
	for (								\
		(p) = ((l)? MR_dlist_next((l)): NULL);			\
		(p) != (l) && (p) != NULL;				\
		(p) = MR_dlist_next((p))				\
	)
#define	MR_for_2dlist(p1, p2, l1, l2)					\
	for (								\
		(p1) = ((l1)? MR_dlist_next((l1)): NULL),		\
			(p2) = ((l2)? MR_dlist_next((l2)): NULL);	\
		(p1) != (l1) && (p1) != NULL && (p2) != (l2) && (p2) != NULL;\
		(p1) = MR_dlist_next((p1)), (p2) = MR_dlist_next((p2))	\
	)
#define MR_for_undlist(p, np, l)					\
	for (								\
		(p) = ((l)? MR_dlist_next((l)): NULL),			\
			(np) = ((p)? MR_dlist_next((p)): NULL);		\
		(p) != (l) && (p) != NULL;				\
		(p) = (np), (np) = ((p)? MR_dlist_next((p)): NULL)	\
	)
#define	MR_end_dlist(p, l)						\
	((p) == (l) || (p) == NULL)

extern	MR_Dlist	*MR_dlist_makelist0(void);
extern	MR_Dlist	*MR_dlist_makelist(const void *);
extern	MR_Dlist	*MR_dlist_addhead(MR_Dlist *, const void *);
extern	MR_Dlist	*MR_dlist_addtail(MR_Dlist *, const void *);
extern	MR_Dlist	*MR_dlist_addlist(MR_Dlist *, MR_Dlist *);
extern	MR_Dlist	*MR_dlist_addndlist(MR_Dlist *, MR_Dlist *);
extern	void		MR_dlist_insert_before(MR_Dlist *, MR_Dlist *,
				const void *);
extern	void		MR_dlist_insert_after(MR_Dlist *, MR_Dlist *,
				const void *);
extern	int		MR_dlist_maybe_null_length(const MR_Dlist *);
extern	void		MR_dlist_delete(MR_Dlist *, MR_Dlist *,
				void (*)(const void *));
extern	void		MR_dlist_oldlist(MR_Dlist *, void (*)(const void *));

#endif /* not MERCURY_DLIST_H */
