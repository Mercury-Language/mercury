/*
INIT mercury_sys_init_context
ENDINIT
*/
/*
** Copyright (C) 1995-1997 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/* context.mod - handles multithreading stuff. */

#include "mercury_imp.h"

#include <stdio.h>
#include <unistd.h>		/* for getpid() and fork() */
#ifdef PARALLEL
#include <signal.h>
#endif

#include "mercury_context.h"
#include "mercury_engine.h"	/* for `memdebug' */

Context	*this_context;
static Context	*free_context_list = NULL;

void 
init_process_context(void)
{
	init_heap();

	this_context = new_context();
		/* load the registers so we don't clobber hp */
	restore_transient_registers();
	load_context(this_context);
	save_transient_registers();

	if (memdebug) debug_memory();
}

Context *
new_context(void)
{
	Context *c;

	if (free_context_list == NULL) {
		c = (Context *) make(Context);
		c->detstack_zone = NULL;
		c->nondetstack_zone = NULL;
#ifdef MR_USE_TRAIL
		c->trail_zone = NULL;
#endif
	} else {
		c = free_context_list;
		free_context_list = c->next;
	}

	c->next = NULL;
	c->resume = NULL;
	c->context_succip = ENTRY(do_not_reached);

	if (c->detstack_zone != NULL) {
		reset_zone(c->detstack_zone);
	} else {
		c->detstack_zone = create_zone("detstack", 0,
			detstack_size, next_offset(), detstack_zone_size, 
			default_handler);
	}
	c->context_sp = c->detstack_zone->min;

	if (c->nondetstack_zone != NULL) {
		reset_zone(c->nondetstack_zone);
	} else {
		c->nondetstack_zone = create_zone("nondetstack", 0,
			nondstack_size, next_offset(), nondstack_zone_size,
			default_handler);
	}
	c->context_maxfr = c->nondetstack_zone->min;
	c->context_curfr = c->nondetstack_zone->min;
	bt_redoip(c->context_curfr) = ENTRY(do_not_reached);
	bt_prevfr(c->context_curfr) = NULL;
	bt_succip(c->context_curfr) = ENTRY(do_not_reached);
	bt_succfr(c->context_curfr) = NULL;

#ifdef MR_USE_TRAIL
	if (c->trail_zone != NULL) {
		reset_zone(c->trail_zone);
	} else {
		c->trail_zone = create_zone("trail", 0,
			trail_size, next_offset(), trail_zone_size, 
			default_handler);
	}
	c->context_trail_ptr = (MR_TrailEntry *) c->trail_zone->min;
	c->context_ticket_counter = 0;
#endif

	c->context_hp = NULL;

	return c;
}

void 
delete_context(Context *c)
{
	c->next = free_context_list;
	free_context_list = c;
}

void 
flounder(void)
{
	fatal_error("computation floundered");
}

void mercury_sys_init_context(void); /* suppress gcc warning */
void mercury_sys_init_context(void) {
}
