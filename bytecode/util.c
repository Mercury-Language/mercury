/*
 *	$Id: util.c,v 1.1 1997-01-24 07:12:07 aet Exp $
 *
 *	Copyright: The University of Melbourne, 1996
 */

/* Imports */
#include	<stdlib.h>
#include	<stdio.h>

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
util_fatal_error(char* message)
{
	util_error(message);
	/* XXX: Should dump core here so we can do a post-mortem */
	exit(EXIT_FAILURE);
}

