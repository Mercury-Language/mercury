/*
 *	$Id: mdis.c,v 1.1 1997-01-24 07:10:56 aet Exp $
 *
 *	Copyright: The University of Melbourne, 1996
 */

/* Imports */
#include	<stdlib.h>
#include	<stdio.h>
#include	<unistd.h>

#include	<util.h>
#include	<disasm.h>
#include	<mdis.h>


/* Local declarations */
static void
usage(void);

static char*
program_name	= NULL;

/* Implementation */

void
main(int argc, char* argv[])
{
	char	c;

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
			break;
		default:
			usage();
			break;
		}
	}

	if (optind != argc-1)
	{
		/* XXX: Should be able to read bytecodes from stdin */
		usage();
	}
	else
	{
		char*	filename;
		FILE*	fp;

		filename = argv[optind];
		if ((fp = fopen(filename, "r")) != NULL)
		{
			disassemble(fp);
		}
		else
		{
			/* XXX: Give better error message */
			util_error("Can't open bytecode file");
			usage();
		}
	}

	exit(EXIT_SUCCESS);
}

void
usage()
{
	fprintf(stderr, "Usage: %s [-h heapsize] <bytecode files>\n",
		program_name
	);
	exit(EXIT_FAILURE);
}

