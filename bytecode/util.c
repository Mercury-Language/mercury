/*
 *	$Id: util.c,v 1.3 1997-02-11 08:05:22 aet Exp $
 *
 *	Copyright: The University of Melbourne, 1996
 */

/* Imports */
#include	<stdlib.h>
#include	<stdio.h>
#include	<string.h>

#include	<util.h>


/* Local declarations */

static char
rcs_id[]	= "$Id: util.c,v 1.3 1997-02-11 08:05:22 aet Exp $";

/* Implementation */

void
util_error(char* message)
{
	fprintf(stderr, "Error: %s\n", message);
	return;
}

void
fatal(char* message)
{
	util_error(message);
	/* XXX: Should dump core here so we can do a post-mortem */
	exit(EXIT_FAILURE);
}

char*
strdup(char* str)
{
	int	size;
	char	*str2, *c_p;

	size = strlen(str) + 1;
	str2 = malloc(size);
	for (c_p=str2; *str != '\0'; str++, c_p++)
	{
		*c_p = *str;
	}
	*c_p = '\0';

	return str2;
}
