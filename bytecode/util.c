/*
 *	$Id: util.c,v 1.2 1997-01-28 02:01:31 aet Exp $
 *
 *	Copyright: The University of Melbourne, 1996
 */

/* Imports */
#include	<stdlib.h>
#include	<stdio.h>
#include	<string.h>

#include	<util.h>


/* Local declarations */

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
