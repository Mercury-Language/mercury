
/*
** Copyright (C) 1997-2001 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
**
** High-water marked stack of 'MB_Word's
**
*/

#ifndef MB_STACK_H
#define MB_STACK_H

#include "mb_basetypes.h"
#include "mb_util.h"

/* Stack structure */
typedef struct MB_Stack_Struct {
	MB_Word	*data;
	MB_Word	sp;
	MB_Word max_size: (MR_WORDBITS-1);
	MB_Word gc	: 1;
} MB_Stack;

/*
** Allocates space for a new stack. 'gc' indicates whether the stack region
** should be allocated with the conservative garbage collector (see mb_mem.h)
** with C's malloc (the garbage collector won't follow references from the C
** malloc area)
**
** For the garbage collector, assumes that data is not atomic
*/
MB_Stack	MB_stack_new(MB_Word init_size, MB_Bool gc);

/* Get number of words already pushed on stack */
MB_Word		MB_stack_size(MB_Stack *s);

/* Pushes a value onto the stack. Return index of pushed word */
MB_Word		MB_stack_push(MB_Stack *s, MB_Word x);

/* Removes a value off the stack */
MB_Word		MB_stack_pop(MB_Stack *s);

/* Allocates space for multiple places on the stack */
/* Return value is index of lowest word */
MB_Word		MB_stack_alloc(MB_Stack *s, MB_Word num_words);

/* Remove multiple items off the stack */
void		MB_stack_free(MB_Stack *s, MB_Word num_words);

/* Peek at an item at a given stack index */
MB_Word		MB_stack_peek(MB_Stack *s, MB_Word idx);

/* Peek at an item index items away from the top of the stack */
MB_Word		MB_stack_peek_rel(MB_Stack *s, MB_Word idx);

/* Get the address for the item at index
** NOTE: if you add or remove items, this value could change */
MB_Word		*MB_stack_peek_p(MB_Stack *s, MB_Word idx);

/* Get the address for the item at index relative to the top of the stack */
MB_Word		*MB_stack_peek_rel_p(MB_Stack *s, MB_Word idx);

/* Set the value of an item on the stack */
void		MB_stack_poke(MB_Stack *s, MB_Word idx, MB_Word x);

/* Set the value of an item on the stack, idx items from the top */
void		MB_stack_poke_rel(MB_Stack *s, MB_Word rel_idx, MB_Word value);

/* Deallocate space for the stack */
void		MB_stack_delete(MB_Stack *s);

#endif	/* MB_STACK_H */

