
/*
** Copyright (C) 2000-2001 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
**
** High-water marked stack of 'MB_Word's
**
*/

/* Imports */

#include <assert.h>

#include "mb_bytecode.h"
#include "mb_mem.h"
#include "mb_stack.h"

/* Exported definitions */

MB_Stack	MB_stack_new(MB_Word init_size, MB_Bool gc);
MB_Word		MB_stack_size(MB_Stack *s);
MB_Word		MB_stack_push(MB_Stack *s, MB_Word x);
MB_Word		MB_stack_pop(MB_Stack *s);
MB_Word		MB_stack_alloc(MB_Stack *s, MB_Word num_words);
void		MB_stack_free(MB_Stack *s, MB_Word num_words);
MB_Word		MB_stack_peek(MB_Stack *s, MB_Word index);
MB_Word		MB_stack_peek_rel(MB_Stack *s, MB_Word rel_index);
MB_Word		*MB_stack_peek_p(MB_Stack *s, MB_Word index);
MB_Word		*MB_stack_peek_rel_p(MB_Stack *s, MB_Word rel_index);
void		MB_stack_poke(MB_Stack *s, MB_Word index, MB_Word x);
void		MB_stack_poke_rel(MB_Stack *s, MB_Word rel_idx, MB_Word value);
void		MB_stack_delete(MB_Stack *s);


/* Local declarations */


/* Implementation */

MB_Stack
MB_stack_new(MB_Word init_size, MB_Bool gc) {
	MB_Stack s;

	s.max_size = init_size;
	s.gc = gc;
	if (init_size == 0) {
		s.data = NULL;
	} else {
		s.data  = (gc)
			? MB_GC_NEW_ARRAY(MB_Word, init_size)
			: MB_NEW_ARRAY(MB_Word, init_size);
		if (s.data == NULL) {
			MB_fatal("Unable to allocate memory");
		}
	}
	s.sp = 0;
	
	return s;
}


MB_Word
MB_stack_size(MB_Stack *s) {
	return s->sp;
}

MB_Word
MB_stack_push(MB_Stack *s, MB_Word x)
{
	if (s->sp == s->max_size) {
		s->max_size *= 2;
		if (s->data == NULL) {
			s->data = (s->gc)
				? MB_GC_NEW_ARRAY(MB_Word, s->max_size)
				: MB_NEW_ARRAY(MB_Word, s->max_size);
		} else {
			s->data = (s->gc)
				? MB_GC_RESIZE_ARRAY(s->data, MB_Word,
							s->max_size)
				: MB_RESIZE_ARRAY(s->data, MB_Word,
							s->max_size);
		}

		assert(s->data != NULL);
	}
	s->data[s->sp] = x;
	return s->sp++;
}

MB_Word
MB_stack_pop(MB_Stack *s) {
	assert(s->sp != 0);
	s->sp--;
	return s->data[s->sp];
}

MB_Word
MB_stack_alloc(MB_Stack *s, MB_Word num_words)
{
	MB_Word orig_sp = s->sp;
	
	while (s->sp + num_words > s->max_size) {
		num_words -= (s->max_size - s->sp);
		s->sp = s->max_size;
		num_words--;
		MB_stack_push(s, 0);
	}
	s->sp += num_words;
	return orig_sp;
}


void
MB_stack_free(MB_Stack *s, MB_Word num_words) {
	s->sp -= num_words;
	assert(s->sp >= 0);
}

MB_Word
MB_stack_peek(MB_Stack *s, MB_Word index) {
	assert(index >= 0);
	assert(index < s->sp);
	return s->data[index];
}

MB_Word
MB_stack_peek_rel(MB_Stack *s, MB_Word rel_index) {
	return MB_stack_peek(s, s->sp - rel_index);
}

MB_Word *
MB_stack_peek_p(MB_Stack *s, MB_Word index) {
	assert(index >= 0);
	assert(index < s->sp);
	return s->data + index;
}

MB_Word *
MB_stack_peek_rel_p(MB_Stack *s, MB_Word rel_index) {
	return MB_stack_peek_p(s, s->sp - rel_index);
}

void
MB_stack_poke(MB_Stack *s, MB_Word index, MB_Word x) {
	assert(index >= 0);
	assert(index < s->sp);
	s->data[index] = x;
}

void
MB_stack_poke_rel(MB_Stack *s, MB_Word rel_idx, MB_Word value) {
	MB_stack_poke(s, s->sp - rel_idx, value);
}

void
MB_stack_delete(MB_Stack *s) {
	if (s->gc) {
		MB_GC_free(s->data);
	} else {
		MB_free(s->data);
	}
}


