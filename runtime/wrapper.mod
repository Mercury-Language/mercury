/*
** Copyright (C) 1995 University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** file: wrapper.mod
** main authors: zs, fjh
**
**	This file contains the startup code for the Mercury runtime.
**	It defines mercury_main().  The code initializes various things,
**	processes options (which are specified via an environment variable)
**	and then invokes call_engine() to start execution.
*/

#include	"imp.h"
#include	"timing.h"
#include	"getopt.h"
#include	"init.h"
#include	"prof.h"
#include	<ctype.h>
#include	<string.h>

/* global variables concerned with testing (i.e. not with the engine) */

/* command-line options */

/* size of data areas, in kilobytes */
unsigned	heap_size =      4096;
unsigned	detstack_size =   512;
unsigned	nondstack_size =  128;

/* size of the redzones at the end of data areas, in kilobytes */
unsigned	heap_zone_size =       16;
unsigned	detstack_zone_size =   16;
unsigned	nondstack_zone_size =  16;

/* primary cache size to optimize for */
unsigned	pcache_size =    8192;

/* other options */
int		r1val = -1;
int		r2val = -1;
int		r3val = -1;

bool		check_space = FALSE;
static	bool	benchmark_all_solns = FALSE;
static	bool	use_own_timer = FALSE;
static	int	repeats = 1;

static int	repcounter;

/* misc. */

static	int	start_time;
static	int	finish_time;

const char *	progname;
int		mercury_argc;	/* not counting progname or debug options */
char **		mercury_argv;
int		mercury_exit_status = 0;

/*
** The Mercury runtime calls run/0 in the Mercury library, and the Mercury
** library calls main/2 in the user's program.  The Mercury runtime also calls
** init_gc() and init_modules() which are in the automatically generated
** C init file, and mercury_init_io(), which is in the Mercury library.
**
** But to enable Quickstart of shared libraries on Irix 5,
** we need to make sure that we don't have any undefined
** external references when building the shared libraries.
** Hence the statically linked init file saves the addresses of those
** procedures in the following global variables.
*/
void (*address_of_mercury_init_io)(void);
void (*address_of_init_modules)(void);
#ifdef CONSERVATIVE_GC
void (*address_of_init_gc)(void);
#endif
Code *library_entry_point;	/* normally io:run/0 (mercury__io__run_0_0) */
Code *program_entry_point;	/* normally main/2 (mercury__main_2_0) */


#ifdef USE_GCC_NONLOCAL_GOTOS

#define	SAFETY_BUFFER_SIZE	1024	/* size of stack safety buffer */
#define	MAGIC_MARKER_2		142	/* a random character */

#endif

static	void	process_args(int argc, char **argv);
static	void	process_environment_options(void);
static	void	process_options(int argc, char **argv);
static	void	usage(void);
static	void	run_code(void);
static	void	make_argv(const char *, char **, char ***, int *);

#ifdef MEASURE_REGISTER_USAGE
static	void	print_register_usage_counts(void);
#endif

Declare_entry(do_interpreter);

int mercury_main(int argc, char **argv)
{
#ifndef	SPEED
	/*
	** Ensure stdio & stderr are unbuffered even if redirected.
	** Using setvbuf() is more complicated than using setlinebuf(),
	** but also more portable.
	*/

	setvbuf(stdout, NULL, _IONBF, 0);
	setvbuf(stderr, NULL, _IONBF, 0);
#endif

#ifdef CONSERVATIVE_GC
	GC_quiet = TRUE;

	/* double-check that the garbage collector knows about
	   global variables in shared libraries */
	GC_is_visible(fake_reg);

	/* call the init_gc() function defined in <foo>_init.c - */
	/* this is to work around a Solaris 2.X (X <= 4) linker bug */
	(*address_of_init_gc)();

	/* The following code is necessary to tell the conservative */
	/* garbage collector that we are using tagged pointers */
	{
		int i;

		for (i = 1; i < (1 << TAGBITS); i++) {
			GC_register_displacement(i);
		}
	}
#endif

	/* process the command line and the options in the environment
	   variable MERCURY_OPTIONS, and save results in global vars */
	process_args(argc, argv);
	process_environment_options();

#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) || \
		defined(PROFILE_CALLS) || defined(PROFILE_TIME)
	do_init_modules();
#endif

#ifdef	PROFILE_TIME
	prof_init_time_profile();
#endif

	(*address_of_mercury_init_io)();

	/* execute the selected entry point */
	init_engine();
	run_code();

	return mercury_exit_status;
}

void do_init_modules(void)
{
	static	bool	done = FALSE;

	if (! done) {
		(*address_of_init_modules)();
		done = TRUE;
	}
}

/*
** Given a string, parse it into arguments and create an argv vector for it.
** Returns args, argv, and argc.  It is the caller's responsibility to oldmem()
** args and argv when they are no longer needed.
*/

static void
make_argv(const char *string, char **args_ptr, char ***argv_ptr, int *argc_ptr)
{
	char *args;
	char **argv;
	const char *s = string;
	char *d;
	int args_len = 0;
	int argc = 0;
	int i;
	
	/*
	** First do a pass over the string to count how much space we need to
	** allocate
	*/

	for (;;) {
		/* skip leading whitespace */
		while(isspace(*s)) {
			s++;
		}

		/* are there any more args? */
		if(*s != '\0') {
			argc++;
		} else {
			break;
		}

		/* copy arg, translating backslash escapes */
		if (*s == '"') {
			s++;
			/* "double quoted" arg - scan until next double quote */
			while (*s != '"') {
				if (s == '\0') {
					fatal_error(
				"Mercury runtime: unterminated quoted string\n"
				"in MERCURY_OPTIONS environment variable\n"
					);
				}
				if (*s == '\\')
					s++;
				args_len++; s++;
			}
			s++;
		} else {
			/* ordinary white-space delimited arg */
			while(*s != '\0' && !isspace(*s)) {
				if (*s == '\\')
					s++;
				args_len++; s++;
			}
		}
		args_len++;
	}

	/*
	** Allocate the space
	*/
	args = make_many(char, args_len);
	argv = make_many(char *, argc + 1);

	/*
	** Now do a pass over the string, copying the arguments into `args'
	** setting up the contents of `argv' to point to the arguments.
	*/
	s = string;
	d = args;
	for(i = 0; i < argc; i++) {
		/* skip leading whitespace */
		while(isspace(*s)) {
			s++;
		}

		/* are there any more args? */
		if(*s != '\0') {
			argv[i] = d;
		} else {
			argv[i] = NULL;
			break;
		}

		/* copy arg, translating backslash escapes */
		if (*s == '"') {
			s++;
			/* "double quoted" arg - scan until next double quote */
			while (*s != '"') {
				if (*s == '\\')
					s++;
				*d++ = *s++;
			}
			s++;
		} else {
			/* ordinary white-space delimited arg */
			while(*s != '\0' && !isspace(*s)) {
				if (*s == '\\')
					s++;
				*d++ = *s++;
			}
		}
		*d++ = '\0';
	}

	*args_ptr = args;
	*argv_ptr = argv;
	*argc_ptr = argc;
}


/**  
 **  process_args() is a function that sets some global variables from the
 **  command line.  `mercury_arg[cv]' are `arg[cv]' without the program name.
 **  `progname' is program name.
 **/

static void
process_args( int argc, char ** argv)
{
	progname = argv[0];
	mercury_argc = argc - 1;
	mercury_argv = argv + 1;
}


/**
 **  process_environment_options() is a function to parse the MERCURY_OPTIONS
 **  environment variable.  
 **/ 

static void
process_environment_options(void)
{
	char*	options;

	options = getenv("MERCURY_OPTIONS");
	if (options != NULL) {
		char	*arg_str, **argv;
		char	*dummy_command_line;
		int	argc;
		int	c;

		/*
		   getopt() expects the options to start in argv[1],
		   not argv[0], so we need to insert a dummy program
		   name (we use "x") at the start of the options before
		   passing them to make_argv() and then to getopt().
		*/
		dummy_command_line = make_many(char, strlen(options) + 3);
		strcpy(dummy_command_line, "x ");
		strcat(dummy_command_line, options);
		
		make_argv(dummy_command_line, &arg_str, &argv, &argc);
		oldmem(dummy_command_line);

		process_options(argc, argv);

		oldmem(arg_str);
		oldmem(argv);
	}

}

static void
process_options(int argc, char **argv)
{
	int c;
	while ((c = getopt(argc, argv, "acd:hlp:r:s:tw:xz:1:2:3:")) != EOF)
	{
		switch (c)
		{

		case 'a':	benchmark_all_solns = TRUE;
				break;

		case 'c':	check_space = TRUE;
				break;

		case 'd':	if (streq(optarg, "b"))
					nondstackdebug = TRUE;
				else if (streq(optarg, "c"))
					calldebug    = TRUE;
				else if (streq(optarg, "d"))
					detaildebug  = TRUE;
				else if (streq(optarg, "g"))
					gotodebug    = TRUE;
				else if (streq(optarg, "G"))
#ifdef CONSERVATIVE_GC
					GC_quiet = FALSE;
#else
					fatal_error("-dG: GC not enabled");
#endif
				else if (streq(optarg, "s"))
					detstackdebug   = TRUE;
				else if (streq(optarg, "h"))
					heapdebug    = TRUE;
				else if (streq(optarg, "f"))
					finaldebug   = TRUE;
				else if (streq(optarg, "p"))
					progdebug   = TRUE;
				else if (streq(optarg, "m"))
					memdebug    = TRUE;
				else if (streq(optarg, "r"))
					sregdebug    = TRUE;
				else if (streq(optarg, "t"))
					tracedebug   = TRUE;
				else if (streq(optarg, "a"))
				{
					calldebug      = TRUE;
					nondstackdebug = TRUE;
					detstackdebug  = TRUE;
					heapdebug      = TRUE;
					gotodebug      = TRUE;
					sregdebug      = TRUE;
					finaldebug     = TRUE;
					tracedebug     = TRUE;
#ifdef CONSERVATIVE_GC
					GC_quiet = FALSE;
#endif
				}
				else
					usage();

				use_own_timer = FALSE;
				break;

		case 'h':	usage();
				break;

		case 'l': {
				List	*ptr;
				List	*label_list;

				label_list = get_all_labels();
				for_list (ptr, label_list)
				{
					Label	*label;
					label = (Label *) ldata(ptr);
					printf("%lu %lx %s\n",
						(unsigned long) label->e_addr,
						(unsigned long) label->e_addr,
						label->e_name);
				}

				exit(0);
		}

		case 'p':	if (sscanf(optarg, "%d", &pcache_size) != 1)
					usage();

				if (pcache_size < 512)
					pcache_size *= 1024;

				break;

		case 'r':	if (sscanf(optarg, "%d", &repeats) != 1)
					usage();

				break;

		case 's': {
				int val;
				if (sscanf(optarg+1, "%d", &val) != 1)
					usage();

				if (optarg[0] == 'h')
					heap_size = val;
				else if (optarg[0] == 'd')
					detstack_size = val;
				else if (optarg[0] == 'n')
					nondstack_size = val;
				else if (optarg[0] == 'l')
					entry_table_size = val *
						1024 / (2 * sizeof(List *));
				else
					usage();

				break;
		}
		case 't':	use_own_timer = TRUE;

				calldebug      = FALSE;
				nondstackdebug = FALSE;
				detstackdebug  = FALSE;
				heapdebug      = FALSE;
				gotodebug      = FALSE;
				sregdebug      = FALSE;
				finaldebug     = FALSE;
				break;

		case 'w': {
				Label *which_label;

				which_label = lookup_label_name(optarg);
				if (which_label == NULL)
				{
					fprintf(stderr,
			"Mercury runtime: label name `%s' unknown\n",
						optarg);
					exit(1);
				}

				library_entry_point = which_label->e_addr;

				break;
		}
		case 'm': {
				Label *which_label;

				which_label = lookup_label_name(optarg);
				if (which_label == NULL)
				{
					fprintf(stderr,
			"Mercury runtime: label name `%s' unknown\n",
						optarg);
					exit(1);
				}

				program_entry_point = which_label->e_addr;

				break;
		}
		case 'x':
#ifdef CONSERVATIVE_GC
				GC_dont_gc = 1;
#endif

				break;

		case 'z': {
				int val;
				if (sscanf(optarg+1, "%d", &val) != 1)
					usage();

				if (optarg[0] == 'h')
					heap_zone_size = val;
				else if (optarg[0] == 'd')
					detstack_zone_size = val;
				else if (optarg[0] == 'n')
					nondstack_zone_size = val;
				else
					usage();

				break;
		}
		case '1':	if (sscanf(optarg, "%d", &r1val) != 1)
					usage();

				break;

		case '2':	if (sscanf(optarg, "%d", &r2val) != 1)
					usage();

				break;

		case '3':	if (sscanf(optarg, "%d", &r3val) != 1)
					usage();

				break;

		default:	usage();

		}
	}
}

static void usage(void)
{
	printf("Mercury runtime usage:\n"
		"MERCURY_OPTIONS=\"[-hclt] [-d[abcdghs]] [-[sz][hdn]#]\n"
	"                 [-p#] [-r#] [-1#] [-2#] [-3#] [-w name] [-m name]\"\n"
		"-h \t\tprint this usage message\n"
		"-c \t\tcheck cross-function stack usage\n"
		"-l \t\tprint all labels\n"
		"-t \t\tuse own timer\n"
		"-x \t\tdisable garbage collection\n"
		"-dg \t\tdebug gotos\n"
		"-dc \t\tdebug calls\n"
		"-db \t\tdebug backtracking\n"
		"-dh \t\tdebug heap\n"
		"-ds \t\tdebug detstack\n"
		"-df \t\tdebug final success/failure\n"
		"-da \t\tdebug all\n"
		"-dm \t\tdebug memory allocation\n"
		"-dG \t\tdebug garbage collection\n"
		"-dd \t\tdetailed debug\n"
		"-sh<n> \t\tallocate n kb for the heap\n"
		"-sd<n> \t\tallocate n kb for the det stack\n"
		"-sn<n> \t\tallocate n kb for the nondet stack\n"
		"-sl<n> \t\tallocate n kb for the label table\n"
		"-zh<n> \t\tallocate n kb for the heap redzone\n"
		"-zd<n> \t\tallocate n kb for the det stack redzone\n"
		"-zn<n> \t\tallocate n kb for the nondet stack redzone\n"
		"-p<n> \t\tprimary cache size in (k)bytes\n"
		"-r<n> \t\trepeat n times\n"
	"-m<name> \tcall I/O predicate with given name (default: main/2)\n"
		"-w<name> \tcall predicate with given name\n"
		"-1<x> \t\tinitialize register r1 with value x\n"
		"-2<x> \t\tinitialize register r2 with value x\n"
		"-3<x> \t\tinitialize register r3 with value x\n");
	fflush(stdout);
	exit(1);
}

void run_code(void)
{
#if !defined(SPEED) && defined(USE_GCC_NONLOCAL_GOTOS)
	/*
	** double-check to make sure that we're not corrupting
	** the C stack with these non-local gotos, by filling
	** a buffer with a known value and then later checking
	** that it still contains only this value
	*/

	unsigned char	safety_buffer[SAFETY_BUFFER_SIZE];

	global_pointer_2 = safety_buffer;	/* defeat optimization */
	memset(safety_buffer, MAGIC_MARKER_2, SAFETY_BUFFER_SIZE);
#endif

#ifndef SPEED
	heapmax      = heapmin;
	detstackmax  = detstackmin;
	nondstackmax = nondstackmin;
#endif

        if (use_own_timer)
		start_time = get_run_time();

	for (repcounter = 0; repcounter < repeats; repcounter++)
	{
		debugmsg0("About to call engine\n");
		call_engine(ENTRY(do_interpreter));
		debugmsg0("Returning from call_engine\n");
	}

        if (use_own_timer)
		finish_time = get_run_time();

#if defined(USE_GCC_NONLOCAL_GOTOS) && !defined(SPEED)
	{
		int i;

		for (i = 0; i < SAFETY_BUFFER_SIZE; i++)
			assert(safety_buffer[i] == MAGIC_MARKER_2);
	}
#endif

	if (detaildebug) {
		debugregs("after final call");
	}

#ifndef	SPEED
	if (memdebug) {
		printf("\n");
		printf("max heap used:      %6ld words\n",
			(long) (heapmax - heapmin));
		printf("max detstack used:  %6ld words\n",
			(long)(detstackmax - detstackmin));
		printf("max nondstack used: %6ld words\n",
			(long) (nondstackmax - nondstackmin));
	}
#endif

#ifdef MEASURE_REGISTER_USAGE
	printf("\n");
	print_register_usage_counts();
#endif

        if (use_own_timer)
		printf("%8.3fu ", ((double) (finish_time - start_time)) / 1000);
}

#ifdef MEASURE_REGISTER_USAGE
static void print_register_usage_counts(void)
{
	int	i;

	printf("register usage counts:\n");
	for (i = 0; i < MAX_RN; i++) {
		if (1 <= i && i <= ORD_RN) {
			printf("r%d", i);
		} else {
			switch (i) {

			case SI_RN:
				printf("succip");
				break;
			case HP_RN:
				printf("hp");
				break;
			case SP_RN:
				printf("sp");
				break;
			case CF_RN:
				printf("curfr");
				break;
			case MF_RN:
				printf("maxfr");
				break;
			default:
				printf("UNKNOWN%d", i);
				break;
			}
		}

		printf("\t%lu\n", (unsigned long)num_uses[i]);
	}
}
#endif

BEGIN_MODULE(interpreter_module)

BEGIN_CODE

do_interpreter:
	push(hp);
	push(succip);
	push(maxfr);
	mkframe("interpreter", 1, LABEL(global_fail));

	if (library_entry_point == NULL) {
		fatal_error("no library entry point supplied");
	}
	call(library_entry_point, LABEL(global_success), LABEL(do_interpreter));

global_success:
#ifndef	SPEED
	if (finaldebug) {
		save_transient_registers();
		printregs("global succeeded");
		if (detaildebug)
			dumpnondstack();
	}
#endif

	if (benchmark_all_solns)
		redo();
	else
		GOTO_LABEL(all_done);

global_fail:
#ifndef	SPEED
	if (finaldebug) {
		save_transient_registers();
		printregs("global failed");

		if (detaildebug)
			dumpnondstack();
	}
#endif

all_done:
	maxfr = (Word *) pop();
	succip = (Code *) pop();
	hp = (Word *) pop();

#ifndef SPEED
	if (finaldebug && detaildebug) {
		save_transient_registers();
		printregs("after popping...");
	}
#endif
	proceed();
#ifndef	USE_GCC_NONLOCAL_GOTOS
	return 0;
#endif

END_MODULE
