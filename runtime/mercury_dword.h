/*
** Copyright (C) 1998, 2000-2001 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** File: mercury_dword.h
** Author: zs.
**
** This module provides facilities for maintaining unsigned counters (dwords)
** whose size is at least 64 bits. The reason why we need such counters is that
** e.g. the amount of memory allocated by a program may be too big to be
** represented using 32 bits, even on 32 bit platforms, in the presence of
** e.g. garbage collection.
*/

/*---------------------------------------------------------------------------*/

#ifndef MERCURY_DWORD_H
#define MERCURY_DWORD_H

#include <limits.h>
#include "mercury_types.h"

/*---------------------------------------------------------------------------*/

#ifdef MR_INT_LEAST64_TYPE

  /*
  ** This case is nice and simple.
  */

  typedef MR_uint_least64_t MR_Dword;

  #define MR_convert_dword_to_double(dword_form, double_form) \
		((double_form) = (double) (dword_form))

  #define MR_zero_dword(dword) \
		((dword) = 0)

  #define MR_increment_dword(dword, inc) \
		((dword) += (inc))

  #define MR_increment_dword_tmp(dword, inc, tmp) \
		((dword) += (inc))

  #define MR_add_two_dwords(src_dest_dword, src_dword) \
		((src_dest_dword) += (src_dword))

  #define MR_add_two_dwords_tmp(src_dest_dword, src_dword, tmp) \
		((src_dest_dword) += (src_dword))

#else

  /*
  ** oh well, guess we have to do it the hard way :-(
  */

  typedef struct MR_Dword_Struct
  {
	MR_uint_least32_t MR_dword_low;
	MR_uint_least32_t MR_dword_high;
  } MR_Dword;

  #define MR_HIGHWORD_TO_DOUBLE	(((double) MR_UINT_LEAST32_MAX) + 1.0)

  #define MR_convert_dword_to_double(dword_form, double_form) 		\
	do {								\
		double_form = (MR_HIGHWORD_TO_DOUBLE			\
			* (double) (dword_form).MR_dword_high) 		\
			+ (double) (dword_form).MR_dword_low;		\
	} while (0)

  #define MR_zero_dword(dword) 						\
	do { 								\
		(dword).MR_dword_low = 0;				\
		(dword).MR_dword_high = 0;				\
	} while (0)

  #define MR_increment_dword(dword, inc) 				\
	do { 								\
		MR_uint_least32_t old_low_word = (dword).MR_dword_low;	\
		(dword).MR_dword_low += (inc);				\
		if ((dword).MR_dword_low < old_low_word) {		\
			(dword).MR_dword_high += 1;			\
		}							\
	} while (0)

  #define MR_increment_dword_tmp(dword, inc, low_tmp)			\
	( 								\
		(low_tmp) = (dword).MR_dword_low,			\
		(dword).MR_dword_low += (inc),				\
		((dword).MR_dword_low < (low_tmp)) ?			\
			(dword).MR_dword_high += 1 : (void) 0		\
	)

  #define MR_add_two_dwords(src_dest_dword, src_dword) 			\
	do { 								\
		MR_uint_least32_t old_low_word;				\
		old_low_word = (src_dest_dword).MR_dword_low;		\
		(src_dest_dword).MR_dword_low += (src_dword).MR_dword_low;\
		if ((src_dest_dword).MR_dword_low < old_low_word) {	\
			(src_dest_dword).MR_dword_high += 1;		\
		}							\
		(src_dest_dword).MR_dword_high += (src_dword).MR_dword_high;\
	} while (0)

  #define MR_add_two_dwords_tmp(src_dest_dword, src_dword, low_tmp)	\
	( 								\
		(low_tmp) = (src_dest_dword).MR_dword_low; 		\
		(src_dest_dword).MR_dword_low += (src_dword).MR_dword_low;\
		((src_dest_dword).MR_dword_low < (low_tmp)) ?		\
			(src_dest_dword).MR_dword_high += 1 : (void) 0,	\
		(src_dest_dword).MR_dword_high += (src_dword).MR_dword_high\
	)

#endif /* not MR_INT_LEAST32_TYPE */

#endif /* MERCURY_DWORD_H */
