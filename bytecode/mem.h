
/*
 *	$Id: mem.h,v 1.1 1997-02-11 09:15:43 aet Exp $
 *
 *	Copyright: The University of Melbourne, 1996
 */

#if	! defined(MEMALLOC_H)
#define	MEMALLOC_H

void*
mem_malloc(size_t size);

void
mem_free(void *mem);

void*
mem_realloc(size_t size, void *mem);

#endif	/* MEMALLOC_H */
