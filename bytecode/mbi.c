/*
** Copyright (C) 1997 University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
**
** $Id: mbi.c,v 1.4 1997-03-25 02:11:01 aet Exp $
*/

/* Imports */
#include	<stdlib.h>
#include	<stdio.h>
#include	<unistd.h>
#include	<getopt.h>

#include	<util.h>
#include	<mem.h>
#include	<mbi.h>


/* Exports */
int
heapsize	= 0;


/* Local declarations */

static char
rcs_id[]	= "$Id: mbi.c,v 1.4 1997-03-25 02:11:01 aet Exp $";

static void
usage(void);

static char*
program_name	= NULL;

/* Implementation */

#if	! defined(UNIT_TESTING)

void
main(int argc, char* argv[])
{
	int	c;

	/* We do this in case we change the program name. */
	program_name = argv[0];

	/* Don't use default error messages from getopt() */
	opterr = 0;

	/* Read options */
	while ((c = getopt(argc,argv,"h:s:")) != EOF)
	{
		int	stacksize;
		int	heapsize;

		switch (c) 
		{
		case 'h':
			/* XXX: Do something with heapsize option */
			heapsize = atoi(optarg);
			if (heapsize <= 0)
			{
				usage();
			}
			break;
		case 's':
			/* XXX: Do something with stacksize option */
			stacksize = atoi(optarg);
			if (stacksize <= 0)
			{
				usage();
			}
			break;
		default:
			usage();
			break;
		}
	}

	/* We _must_ have a file argument */
	if (optind == argc)
	{
		usage();
	}
	else /* Process each bytecode file in order */
	{
		int 	i;
		char	*filename;
		FILE	*fp;

		for (i=optind; i < argc; i++)
		{
			filename = argv[i];
			if ((fp = fopen(filename, "r")) != NULL)
			{
#if 0
				if (is bytecode file) /* file ext = .mb */
				{
					read bytecodes into code area
					and store label and procedure
					entry points, etc.
				}
				else if (is shared library) /* file ext = .so*/
				{
					do a dlopen and add to list
					of shlibs.
				}
				else
				{
					error: wrong file extension
				}
				
#endif /* 0 */
			}
			else
			{
				/* XXX: Give better error message */
				util_error("can not open file \"%s\"",
					filename);
			}
		} /* for */

		/*
		 * XXX: Now start the bytecode interpreter
		 * Fire up the read-eval-print loop?
		 */

	} /* else */

	exit(EXIT_SUCCESS);
} /* main */

#endif	/* ! UNIT_TESTING */

void
usage()
{
	fprintf(stderr, "Usage: %s [-h heapsize] [-s stacksize] "
		"<bytecode files>\n", program_name
	);
	exit(EXIT_FAILURE);
}

