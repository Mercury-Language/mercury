/*
** Copyright (C) 1997 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
**
** $Id: util.c,v 1.8 1997-07-27 14:59:31 fjh Exp $
*/


/* Imports */
#include	<stdlib.h>
#include	<stdio.h>
#include	<string.h>
#include	<stdarg.h>

#include	"util.h"
#include	"mem.h"


/* Local declarations */

static char
rcs_id[]	= "$Id: util.c,v 1.8 1997-07-27 14:59:31 fjh Exp $";

/* Implementation */

void 
MB_util_error(const char *fmt, ...)
{
        va_list arg_p;

        fprintf(stderr, "Error: ");
        va_start(arg_p, fmt);
        vfprintf(stderr, fmt, arg_p);
        va_end(argp);
        fprintf(stderr, "\n");
}


void
MB_fatal(const char* message)
{
	MB_util_error(message);
	fprintf(stderr, "NOTE: The program is aborting. "
		 "Please keep the core file for diagnosis.\n");
	abort();

	return; /* not reached */
}

char*
MB_strdup(const char* str)
{
	int	size;
	char	*str2, *c_p;

	size = strlen(str) + 1;
	str2 = MB_malloc(size);
	for (c_p = str2; *str != '\0'; str++, c_p++)
	{
		*c_p = *str;
	}
	*c_p = '\0';

	return str2;
}
