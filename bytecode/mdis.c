
/*
** Copyright (C) 2000 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
**
** $Id: mdis.c,v 1.8 2001-01-24 07:42:29 lpcam Exp $
*/

static char
rcs_id[]	= "$Id: mdis.c,v 1.8 2001-01-24 07:42:29 lpcam Exp $";

/* Imports */
#include	<getopt.h>
#include	<stdio.h>
#include	<stdlib.h>

#include	"mb_util.h"
#include	"mb_disasm.h"
#include	"mb_machine.h"
/* Exported definitions */

/* Local declarations */
static void
usage(const char* program_name, FILE* fp);

/* Implementation */

int
main(int argc, char* argv[]) {

	int c;

	/* Don't use default error messages from getopt() */
	opterr = 0;

	/* Read options */
	while ((c = getopt(argc,argv,"h")) != EOF) {
		switch (c) {
			case 'h':
				usage(argv[0], stderr);
				exit(EXIT_SUCCESS);
				break;
			default:
				usage(argv[0], stdout);
				exit(EXIT_FAILURE);
				break;
		}
	}

	/* If no arguments, then assume bytecode stream is on stdin */
	if (optind == argc) {
		printf("<<dissassemble here>>\n");
		/*MB_disassemble(stdin);*/
	} else {
		/* Process each bytecode file in order */
		int 	i;
		char	*filename;
		MB_Machine_State* ms;

		for (i = optind; i < argc; i++) {
			int	indent_level = 0;
			ms = MB_load_program_name(argv[i]);
			if (ms != NULL) {
				MB_listing(ms, stdout, 0, MB_code_size(ms)-1);
				
				MB_unload_program(ms);

			} else {
				/* XXX: Give better error message */
				MB_util_error("error reading bytecode file `%s'",
						argv[i]);
			}
		}
	} /* end else */

	exit(EXIT_SUCCESS);
} /* end main() */

/* usage - print a short help screen to given output file */
static void
usage(const char* program_name, FILE* fp) {
	fprintf(fp,
			"usage: %s [-h | file]\n", program_name);
}


