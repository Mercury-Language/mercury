/*
** Copyright (C) 1997 University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
**
** $Id: mdis.c,v 1.4 1997-04-24 05:31:01 aet Exp $
*/

/* Imports */
#include	<stdlib.h>
#include	<stdio.h>
#include	<unistd.h>
#include	<getopt.h>

#include	<util.h>
#include	<mem.h>
#include	<disasm.h>
#include	<mdis.h>

static char
rcs_id[]	= "$Id: mdis.c,v 1.4 1997-04-24 05:31:01 aet Exp $";

/* Local declarations */
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
	while ((c = getopt(argc,argv,"h")) != EOF)
	{
		switch (c) 
		{
		case 'h':
			usage();
			exit(EXIT_SUCCESS);
			break;
		default:
			usage();
			exit(EXIT_FAILURE);
			break;
		}
	}

	/* If no arguments, then assume bytecode stream is on stdin */
	if (optind == argc)
	{
		disassemble(stdin);
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
				disassemble(fp);
			}
			else
			{
				/* XXX: Give better error message */
				util_error("can not open bytecode file \"%s\"",
					filename);
			}
		}
	} /* else */

	exit(EXIT_SUCCESS);
} /* main */

#endif	/* UNIT_TESTING */

void
usage()
{
	fprintf(stderr, "usage: %s [-h] [files]\n",
		program_name);
	return;
}

