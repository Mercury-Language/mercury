/*
** Copyright (C) 1995-1999 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/* mercury_string.h - string handling */

#ifndef MERCURY_STRING_H
#define MERCURY_STRING_H

#include <string.h>	/* for strcmp() etc. */

#include "mercury_heap.h"	/* for incr_hp_atomic */

/*
** Mercury characters are given type `Char', which is a typedef for `char'.
** But BEWARE: when stored in an Integer, the value must be
** first cast to `UnsignedChar'.
** Mercury strings are stored as pointers to '\0'-terminated arrays of Char.
**
** We may eventually move to using wchar_t for Mercury characters and strings,
** so it is important to use these typedefs.
**
** The actual typedefs are in mercury_types.h to avoid problems with
** circular #includes.
**
** typedef char Char;
** typedef unsigned char UnsignedChar;
**
** typedef Char *String;
** typedef const Char *ConstString;
*/

/*
** string_const("...", len):
**	Given a C string literal and its length, returns a Mercury string.
*/
#define MR_string_const(string, len) ((String) string)

/*
** bool string_equal(ConstString s1, ConstString s2):
**	Return true iff the two Mercury strings s1 and s2 are equal.
*/
#define MR_string_equal(s1,s2) (strcmp((char*)(s1),(char*)(s2))==0)

/* 
** void MR_make_aligned_string(ConstString & ptr, const char * string):
**	Given a C string `string', set `ptr' to be a Mercury string
**	with the same contents.  (`ptr' must be an lvalue.)
**	If the resulting Mercury string is to be used by Mercury code,
**	then the string pointed to by `string' should have been either
**	statically allocated or allocated on the Mercury heap.
**	
** BEWARE: this may modify `hp', so it must only be called from
** places where `hp' is valid.  If calling it from inside a C function,
** rather than inside Mercury code, you may need to call
** save/restore_transient_hp().
**
** Algorithm: if the string is aligned, just set ptr equal to it.
** Otherwise, allocate space on the heap and copy the C string to
** the Mercury string.
*/
#define MR_make_aligned_string(ptr, string) 				\
	do { 								\
	    if (MR_tag((Word) (string)) != 0) {				\
		MR_make_aligned_string_copy((ptr), (string));		\
	    } else { 							\
	    	(ptr) = (string);					\
	    }								\
	} while(0)

/* void MR_make_aligned_string_copy(ConstString &ptr, const char * string);
**	Same as make_aligned_string(ptr, string), except that the string
**	is guaranteed to be copied. This is useful for copying C strings
**	onto the Mercury heap.
**
** BEWARE: this may modify `hp', so it must only be called from
** places where `hp' is valid.  If calling it from inside a C function,
** rather than inside Mercury code, you may need to call
** save/restore_transient_hp().
*/
#define MR_make_aligned_string_copy(ptr, string) 			\
	do {								\
		Word make_aligned_string_tmp;				\
		char * make_aligned_string_ptr;				\
									\
	  	incr_hp_atomic(make_aligned_string_tmp,			\
	    	    (strlen(string) + sizeof(Word)) / sizeof(Word));	\
	    	make_aligned_string_ptr =				\
		    (char *) make_aligned_string_tmp;			\
	    	strcpy(make_aligned_string_ptr, (string));		\
	    	(ptr) = make_aligned_string_ptr;			\
	} while(0)

/*
** do_hash_string(int & hash, Word string):
**	Given a Mercury string `string', set `hash' to the hash value
**	for that string.  (`hash' must be an lvalue.)
**
** This is an implementation detail used to implement hash_string().
** It should not be used directly.  Use hash_string() instead.
**
** Note that hash_string is also defined in library/string.m.
** The definition here and the definition in string.m
** must be kept equivalent.
*/
#define do_hash_string(hash, s)				\
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

/*
** hash_string(s):
**	Given a Mercury string `s', return a hash value for that string.
*/
int	hash_string(Word);

#ifdef __GNUC__
#define hash_string(s)							\
	({ int hash_string_result;					\
	   do_hash_string(hash_string_result, s);			\
	   hash_string_result;						\
	})
#endif

/* 
** If we're not using gcc, the actual definition of hash_string is in misc.c;
** it uses the macro HASH_STRING_FUNC_BODY below.
*/

#define HASH_STRING_FUNC_BODY						\
	   int hash_string_result;					\
	   do_hash_string(hash_string_result, s);			\
	   return hash_string_result;

#endif /* not MERCURY_STRING_H */
