/*
** Copyright (C) 1995 University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

#include	"imp.h"
#include	"timing.h"
#include	"getopt.h"
#include	"io_rt.h"
#include	"init.h"
#include	"prof.h"

/* global variables concerned with testing (i.e. not with the engine) */

/* command-line options */

/* size of data areas, in kilobytes */
uint		heap_size =     40960;
uint		detstack_size =   512;
uint		nondstack_size =  128;

/* size of the redzones at the end of data areas, in kilobytes */
uint		heap_zone_size =       16;
uint		detstack_zone_size =   16;
uint		nondstack_zone_size =  16;

/* primary cache size to optimize for */
uint		pcache_size =    8192;

/* other options */
int		r1val = -1;
int		r2val = -1;
int		r3val = -1;

bool		check_space = FALSE;
bool		benchmark_all_solns = FALSE;

static	bool	use_own_timer = FALSE;
static	int	repeats = 1;
static	Code	*which = NULL;

/* misc. */

int		repcounter = 1;
char		scratchbuf[256];

static	int	start_time;
static	int	finish_time;

const char *	progname;
int		mercury_argc;	/* not counting progname or debug options */
char **		mercury_argv;
int		mercury_exit_status = 0;

#ifdef USE_GCC_NONLOCAL_GOTOS

#define	SAFETY_BUFFER_SIZE	1024	/* size of stack safety buffer */
#define	MAGIC_MARKER_2		142	/* a random character */

#endif

static	void	process_options(int argc, char **argv);
static	void	usage(void);
static	void	run_code(void);

#ifdef MEASURE_REGISTER_USAGE
static	void	print_register_usage_counts(void);
#endif

Declare_entry(do_interpreter);

int main(int argc, char **argv)
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
	/* call the init_gc() function defined in <foo>_init.c - */
	/* this is to work around a Solaris 2.X (X <= 4) linker bug */
	init_gc();

	/* The following code is necessary to tell the conservative */
	/* garbage collector that we are using tagged pointers */
	{
		int i;

		for (i = 1; i < (1 << TAGBITS); i++) {
			GC_register_displacement(i);
		}
	}
#endif

	init_entries();

#if defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS) || \
		defined(PROFILE_CALLS)  || defined(PROFILE_TIME)
	do_init_modules();
#endif

#ifdef	PROFILE_TIME
	prof_init_time_profile();
#endif

	mercury_init_io();

	/* process the command line options, save results in global vars */
	process_options(argc, argv);

	/* execute the selected entry point */
	init_engine();
	run_code();

	return mercury_exit_status;
}

void do_init_modules(void)
{
	static	bool	done = FALSE;

	if (! done)
	{
		init_modules();
		done = TRUE;
	}
}

static void process_options(int argc, char **argv)
{
	int	c;
	int	i;
	int	val;
	List	*ptr;
	List	*label_list;

	progname = argv[0];

	for (i = 0; i < MAXFLAG; i++)
		debugflag[i] = FALSE;

	if (default_entry != NULL)
		which = default_entry;

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
				else if (streq(optarg, "a"))
				{
					calldebug      = TRUE;
					nondstackdebug = TRUE;
					detstackdebug  = TRUE;
					heapdebug      = TRUE;
					gotodebug      = TRUE;
					sregdebug      = TRUE;
					finaldebug     = TRUE;
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

		case 'l':	label_list = get_all_labels();
				for_list (ptr, label_list)
				{
					Label	*label;
					label = (Label *) ldata(ptr);
					printf("%u %x %s\n",
						(unsigned) label->e_addr,
						(unsigned) label->e_addr,
						label->e_name);
				}

				exit(0);

		case 'p':	if (sscanf(optarg, "%d", &pcache_size) != 1)
					usage();

				if (pcache_size < 512)
					pcache_size *= 1024;

				break;

		case 'r':	if (sscanf(optarg, "%d", &repeats) != 1)
					usage();

				break;

		case 's':	if (sscanf(optarg+1, "%d", &val) != 1)
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

		case 't':	use_own_timer = TRUE;

				calldebug      = FALSE;
				nondstackdebug = FALSE;
				detstackdebug  = FALSE;
				heapdebug      = FALSE;
				gotodebug      = FALSE;
				sregdebug      = FALSE;
				finaldebug     = FALSE;
				break;

		case 'w':
				{
					Label *which_label;

					which_label = lookup_label_name(optarg);
					if (which_label == NULL)
					{
						fprintf(stderr, "predicate name %s unknown\n", optarg);
						exit(1);
					}

					which = which_label->e_addr;
				}

				do_init_modules();
				break;

		case 'x':
#ifdef CONSERVATIVE_GC
				GC_dont_gc = 1;
#endif

				break;

		case 'z':	if (sscanf(optarg+1, "%d", &val) != 1)
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

	mercury_argc = argc - optind;
	mercury_argv = argv + optind;

	if (which == NULL)
	{
		fprintf(stderr, "Mercury runtime: no entry point supplied "
				"on command line or by default\n");
		exit(1);
	}
}

static void usage(void)
{
	printf("Usage: %s [-hclt] [-d[abcdghs]] [-[sz][hdn]#] [-p#] [-r#] [-1#] [-2#] [-3#] [-w name]\n",
		progname);
	printf("-h \t\tprint this usage message\n");
	printf("-c \t\tcheck cross-function stack usage\n");
	printf("-l \t\tprint all labels\n");
	printf("-t \t\tuse own timer\n");
	printf("-x \t\tdisable garbage collection\n");
	printf("-dg \t\tdebug gotos\n");
	printf("-dc \t\tdebug calls\n");
	printf("-db \t\tdebug backtracking\n");
	printf("-dh \t\tdebug heap\n");
	printf("-ds \t\tdebug detstack\n");
	printf("-df \t\tdebug final success/failure\n");
	printf("-da \t\tdebug all\n");
	printf("-dm \t\tdebug memory allocation\n");
	printf("-dG \t\tdebug garbage collection\n");
	printf("-dd \t\tdetailed debug\n");
	printf("-sh<n> \t\tallocate n kb for the heap\n");
	printf("-sd<n> \t\tallocate n kb for the det stack\n");
	printf("-sn<n> \t\tallocate n kb for the nondet stack\n");
	printf("-sl<n> \t\tallocate n kb for the label table\n");
	printf("-zh<n> \t\tallocate n kb for the heap redzone\n");
	printf("-zd<n> \t\tallocate n kb for the det stack redzone\n");
	printf("-zn<n> \t\tallocate n kb for the nondet stack redzone\n");
	printf("-p<n> \t\tprimary cache size in (k)bytes\n");
	printf("-r<n> \t\trepeat n times\n");
	printf("-w<name> \tcall predicate with given name\n");
	printf("-1<x> \t\tinitialize register r1 with value x\n");
	printf("-2<x> \t\tinitialize register r2 with value x\n");
	printf("-3<x> \t\tinitialize register r3 with value x\n");
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

	if (detaildebug)
	{
		sprintf(scratchbuf,
			"after final call");
		debugregs(scratchbuf);
	}

#ifndef	SPEED
	if (memdebug)
	{
		printf("\n");
		printf("max heap used:      %6d words\n", heapmax - heapmin);
		printf("max detstack used:  %6d words\n", detstackmax - detstackmin);
		printf("max nondstack used: %6d words\n", nondstackmax - nondstackmin);
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
	for (i = 0; i < MAX_RN; i++)
	{
		if (1 <= i && i <= ORD_RN)
		{
			printf("r%d", i);
		}
		else
		{
			switch (i)
			{

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

	call(which, LABEL(global_success), LABEL(do_interpreter));

global_success:
#ifndef	SPEED
	if (finaldebug)
	{
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
	if (finaldebug)
	{
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
	if (finaldebug && detaildebug)
	{
		save_transient_registers();
		printregs("after popping...");
	}
#endif
	proceed();
#ifndef	USE_GCC_NONLOCAL_GOTOS
	return 0;
#endif

END_MODULE
