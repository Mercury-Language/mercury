/*
** Copyright (C) 1997 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
**
** $Id: mbi.c,v 1.9 1997-07-27 14:59:24 fjh Exp $
*/

/* Imports */

	/*
	** Interface to Mercury runtime must be included first.
	*/
#include	"imp.h"

#include	<stdlib.h>
#include	<stdio.h>
#include	<unistd.h>
#include	<getopt.h>

#include	"util.h"
#include	"mem.h"
#include	"mbi.h"


/* Exports */


/* Local declarations */

static char
rcs_id[]	= "$Id: mbi.c,v 1.9 1997-07-27 14:59:24 fjh Exp $";

static void
usage(void);

static char*
program_name	= NULL;

/* Implementation */

int
BC_mbi_main(int argc, char* argv[])
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

	/* We _must_ have a file argument */
	if (optind == argc) {
		usage();
	} else {
		/* Process each bytecode file in order */
		int 	i;
		char	*filename;
		FILE	*fp;

		for (i = optind; i < argc; i++) {
			filename = argv[i];
			if ((fp = fopen(filename, "r")) != NULL) {
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
				} else {
					error: wrong file extension
				}
				
#endif /* 0 */
			} else {
				/* XXX: Give better error message */
				MB_util_error("can not open file \"%s\"",
					filename);
			}
		} /* end for */

		/*
		 * XXX: Now start the bytecode interpreter
		 * Fire up the read-eval-print loop?
		 */

	} /* end else */

	exit(EXIT_SUCCESS);
} /* end main() */

static void
usage(void)
{
	fprintf(stderr, "Usage: %s [-h] files\n", program_name);
}
