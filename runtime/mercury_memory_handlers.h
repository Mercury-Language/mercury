/*
** Copyright (C) 1998 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_memory_handlers.h - signal handlers for the memory zones.
**
** This defines various signal handlers for memory access violations,
** including accesses to the redzones at the end of each zone.
*/

#ifndef	MERCURY_MEMORY_HANDLERS_H
#define	MERCURY_MEMORY_HANDLERS_H

#include "mercury_memory_zones.h"

/*
** default_handler is a function that can be passed to create_zone to
** unprotect enough of the redzone to allow the access to succeed, or
** fail if there is no space left in the zone.
*/
ZoneHandler default_handler;

/*
** null_handler is a function that can be passed to create_zone which always
** fails.
*/
ZoneHandler null_handler;

/*
**
** setup_signal() will setup the default signal handlers.
**
*/

void	setup_signal(void);

#endif /* not MERCURY_MEMORY_HANDLERS_H */
