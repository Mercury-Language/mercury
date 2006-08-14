/* 
 * Copyright (c) 1991-1994 by Xerox Corporation.  All rights reserved.
 * Copyright (c) 1996-1999 by Silicon Graphics.  All rights reserved.
 * Copyright (c) 1999-2003 by Hewlett-Packard Company. All rights reserved.
 *
 *
 * THIS MATERIAL IS PROVIDED AS IS, WITH ABSOLUTELY NO WARRANTY EXPRESSED
 * OR IMPLIED.  ANY USE IS AT YOUR OWN RISK.
 *
 * Permission is hereby granted to use or copy this program
 * for any purpose,  provided the above notices are retained on all copies.
 * Permission to modify the code and to distribute modified code is granted,
 * provided the above notices are retained, and a notice that the code was
 * modified is included with the above copyright notice.
 *
 */

/* There exist multiprocessor SoC ARM processors, so this may actually	*/
/* matter.								*/

/* I found a slide set that, if I read it correctly, claims that	*/
/* Loads followed by either a Load or Store are ordered, but nothing	*/
/* else is.								*/
/* It appears that SWP is the only simple memory barrier.		*/
#include "../all_atomic_load_store.h"

#include "../read_ordered.h"

#include "../test_and_set_t_is_ao_t.h" /* Probably suboptimal */


AO_INLINE AO_TS_VAL_t
AO_test_and_set_full(volatile AO_TS_t *addr) {
  int oldval;
  /* SWP on ARM is very similar to XCHG on x86. 		*/
  /* The first operand is the result, the second the value	*/
  /* to be stored.  Both registers must be different from addr.	*/
  /* Make the address operand an early clobber output so it     */
  /* doesn't overlap with the other operands.  The early clobber*/
  /* on oldval is neccessary to prevent the compiler allocating */
  /* them to the same register if they are both unused.  	*/
  __asm__ __volatile__("swp %0, %2, [%3]"
                        : "=&r"(oldval), "=&r"(addr)
                        : "r"(1), "1"(addr)
                        : "memory");
  return oldval;
}

#define AO_HAVE_test_and_set_full



