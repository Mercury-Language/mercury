/*
** Copyright (C) 1997 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
**
** $Id: mdis.c,v 1.7 1997-07-27 14:59:27 fjh Exp $
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
rcs_id[]	= "$Id: mdis.c,v 1.7 1997-07-27 14:59:27 fjh Exp $";

/* Local declarations */
static void
usage(void);

static char*
program_name	= NULL;

/* Implementation */

#if	! defined(UNIT_TESTING)

int
main(int argc, char* argv[])
{
	int	c;

	/* We do this in case we change the program name. */
	program_name = argv[0];

	/* Don't use default error messages from getopt() */
	opterr = 0;

	/* Read options */
	while ((c = getopt(argc,argv,"h")) != EOF) {
		switch (c) {
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
	if (optind == argc) {
		MB_disassemble(stdin);
	} else {
		/* Process each bytecode file in order */
		int 	i;
		char	*filename;
		FILE	*fp;

		for (i = optind; i < argc; i++) {
			filename = argv[i];
			if ((fp = fopen(filename, "r")) != NULL) {
				MB_disassemble(fp);
			} else {
				/* XXX: Give better error message */
				MB_util_error("can not open bytecode file `%s'",					filename);
			}
		}
	} /* end else */

	exit(EXIT_SUCCESS);
} /* end main() */

#endif	/* UNIT_TESTING */

static void
usage(void)
{
	fprintf(stderr, "usage: %s [-h] [files]\n", program_name);
}
