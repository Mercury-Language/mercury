/*
** prof_mem.h
**
** Author: petdr
*/

#define prof_make(t)	((t *) prof_malloc(sizeof(t)))

void *prof_malloc(size_t);
