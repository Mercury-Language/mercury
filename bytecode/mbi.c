
/*
** Copyright (C) 2000 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
**
** $Id: mbi.c,v 1.10 2001-01-24 07:42:28 lpcam Exp $
**
** Mercury bytecode interpreter
**
*/

static char
rcs_id[]	= "$Id: mbi.c,v 1.10 2001-01-24 07:42:28 lpcam Exp $";

/* Imports */
#include	<getopt.h>
#include	<stdio.h>
#include	<stdlib.h>
#include	<ctype.h>

#include	"mb_util.h"
#include	"mb_disasm.h"
#include	"mb_machine.h"
/* Exported definitions */

/* Local declarations */
static void usage(const char* program_name, FILE* fp);
static int match(const char* c, const char* controlstr);
static void interactive(MB_Machine_State* ms);
static void show_help(void);

/* Implementation */


static void
show_help(void)
{
	printf( "\n---------------------------------------"
		"\n Mercury Bytecode Interpreter"
		"\n"
		"\n  Interactive mode commands:"
		"\n"
		"\n  ? | help            - this screen" 
		"\n  state               - show machine state"
		"\n  next                - single step execute into predicates"
		"\n  over                - step execute over predicates"
		"\n  run                 - run until exception"
		"\n  list [from [to]]    - source listing"
		"\n  call                - show call stack"
		"\n  x | exit | quit     - exit interpreter"
		"\n"
		"\n---------------------------------------"
		"\n");
}

/* returns true if string pointed to by c could be interpreted
** as matching command (eg: "cyo" will match "cyote" but
** "cyor" will not match "cyote")
**
** White space will be stripped out of c, but should already
** have been taken out of commmand
**
** Strings are not case sensitive
**
*/
static int
match(const char* c, const char* command)
{
	/* Remove leading whitespace from c */
	while (isspace(*c)) c++;
	
	/* Find where the strings differ */
	while (tolower(*c) == tolower(*command)) {
		c++;
		command++;
	}

	/* Allow a truncated c to still match */
	if (isspace(*c) || *c == 0) return TRUE;

	return FALSE;
}

static void
interactive(MB_Machine_State* ms)
{
	char buffer[78];
	char* c;
	
	printf("Found main/2, proc 0 at %04x\n", MB_ip_get(ms));

	/* Show the current machine state */
	MB_show_state(ms, stdout);

	do {
		printf("> ");
		fgets(buffer, sizeof(buffer), stdin);
		printf("\n");

		/* Read the next command */
		c = buffer;
		while (isspace(*c)) c++;

		if (match(c, "next") || (*c == 0)) {
			MB_step(ms);
			MB_show_state(ms, stdout);
			
		} else if (match(c, "run")) {
			MB_run(ms);
			MB_show_state(ms, stdout);
			
		} else if (match(c, "over")) {
			MB_step_over(ms);
			MB_show_state(ms, stdout);
			
		} else if (match(c, "list")) {
			int start = 0;
			int end = -1;
			scanf("list %i %i", &start, &end);
			MB_listing(ms, stdout, start, end);

		} else if (match(c, "quit")
			|| match(c, "exit")
			|| match(c, "x"))
		{
			break;
		} else if (match(c, "?") || match(c, "help")) {
			show_help();
			
		} else if (match(c, "call")) {
			MB_show_call(ms, stdout);

		} else if (match(c, "state")) {
			MB_show_state(ms, stdout);
			
		} else {
			printf("Unrecognised command. Enter ? for Help\n");
		}
		
	} while (!feof(stdin));

}

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

	/* If no arguments, then we obviously don't have a filename */
	if (optind == argc) {
		usage(argv[0], stdout);
	} else {
		
		MB_Machine_State* ms = MB_load_program_name(argv[optind]);
		if (ms == NULL) {
			/* XXX: Give better error message */
			MB_util_error("error reading bytecode file `%s'",
				argv[optind]);
		} else {
			/* Run the interpreter */
			interactive(ms);

			/* And when finished, unload */
			MB_unload_program(ms);
		}
	} /* end else */

	exit(EXIT_SUCCESS);
} /* end main() */

/* usage - print a short help screen to given output file */
static void
usage(const char* program_name, FILE* fp) {
	fprintf(fp,
			"usage: %s [-h | bytecodefile]\n", program_name);
}


