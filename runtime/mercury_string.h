/*
** Copyright (C) 1995-1997 University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/* mercury_string.h - string handling */

#ifndef MERCURY_STRING_H
#define MERCURY_STRING_H

#include <string.h>	/* for strcmp() etc. */

#include "heap.h"	/* for incr_hp_atomic */


typedef char Char;	/* we may eventually move to using wchar_t */

typedef Char *String;
typedef const Char *ConstString;

#define string_const(string, len) ((Word)string)
#define string_equal(s1,s2) (strcmp((char*)(s1),(char*)(s2))==0)


/* 
** If the string is aligned, set ptr equal to it, otherwise allocate one
** on the heap.
*/
#define make_aligned_string(ptr, string) \
	( tag((Word)string) != 0 ? 					\
		(incr_hp_atomic(LVALUE_CAST(Word, (ptr)), 		\
		    (strlen(string) + sizeof(Word)) / sizeof(Word)),	\
		strcpy((ptr), (string)))				\
	: 								\
		((ptr) = (string))					\
	)

/*
** Note that hash_string is also defined in library/string.m.
** The definition here and the definition in string.m
** must be kept equivalent.
*/

#define do_hash_string(hash,s)				\
	{						\
	   int len = 0;					\
	   hash = 0;					\
	   while(((const Char *)(s))[len]) {		\
		hash ^= (hash << 5);			\
		hash ^= ((const Char *)(s))[len];	\
		len++;					\
	   }						\
	   hash ^= len;					\
	}

extern	int	hash_string(Word);

#ifdef __GNUC__
#define hash_string(s)					\
	({ int hash;					\
	   do_hash_string(hash,s);			\
	   hash;					\
	})
#endif

/* 
** if we're not using gcc, the actual definition of hash_string is in aux.c
** it uses the macro HASH_STRING_FUNC_BODY below
*/

#define HASH_STRING_FUNC_BODY				\
	   int hash;					\
	   do_hash_string(hash, s);			\
	   return hash;

#endif /* not MERCURY_STRING_H */
