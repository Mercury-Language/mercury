
/*
** Copyright (C) 1997 University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
**
** $Id: util.c,v 1.4 1997-03-25 03:10:19 aet Exp $
*/


/* Imports */
#include	<stdlib.h>
#include	<stdio.h>
#include	<string.h>
#include	<stdarg.h>

#include	<util.h>


/* Local declarations */

static char
rcs_id[]	= "$Id: util.c,v 1.4 1997-03-25 03:10:19 aet Exp $";

/* Implementation */

void 
util_error(char *fmt, ...)
{
        va_list arg_p;

        fprintf(stderr, "Error: ");
        va_start(arg_p, fmt);
        vfprintf(stderr, fmt, arg_p);
        va_end(argp);
        fprintf(stderr, "\n");
}


void
fatal(char* message)
{
	util_error(message);
	fprintf(stderr, "NOTE: The program is aborting. "
		 "Please keep the core file for diagnosis.\n");
	abort();

	return; /* not reached */
}

/*
 * XXX: Don't use this. We should be using only mem_malloc rather than
 * malloc directly.
 */
#if	0
char*
strdup(char* str)
{
	int	size;
	char	*str2, *c_p;

	size = strlen(str) + 1;
	str2 = malloc(size); /* XXX: use mem_malloc */
	for (c_p=str2; *str != '\0'; str++, c_p++)
	{
		*c_p = *str;
	}
	*c_p = '\0';

	return str2;
}
#endif	/* 0 */


