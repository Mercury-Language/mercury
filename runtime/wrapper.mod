#include 	<string.h>
#include	"imp.h"
#include	"timing.h"
#include	"getopt.h"
#include	"init.h"
#include	"ext_stdio.h"

/* global variables concerned with testing (i.e. not with the engine) */

/* command-line options */

/* size of data areas, in kilobytes */
int		heap_size =     40960;
int		detstack_size =   128;
int		nondstack_size =  128;

/* size of the redzones at the end of data areas, in kilobytes */
int		heap_zone_size =       16;
int		detstack_zone_size =   16;
int		nondstack_zone_size =  16;

/* primary cache size to optimize for */
int		pcache_size =    8192;

/* other options */
int		r1val = -1;
int		r2val = -1;
int		r3val = -1;

bool		check_space = FALSE;

static	bool	use_own_timer = FALSE;
static	int	repeats = 1;
static	Label	*which = NULL;

/* misc. */

int		repcounter = 1;
char		scratchbuf[256];

static Label 	*which_input = NULL;

static	int	start_time;
static	int	finish_time;

const char *	progname;

#ifdef USE_GCC_NONLOCAL_GOTOS

#define	SAFETY_BUFFER_SIZE	1024	/* size of stack safety buffer */
#define	MAGIC_MARKER_2		142	/* a random character */

#endif

extern	int	main(int argc, char **argv);
static	void	process_options(int argc, char **argv);
static	void	usage(void);
static	void	run_code(void);

#ifdef MEASURE_REGISTER_USAGE
static	void	print_register_usage_counts(void);
#endif

void	interpreter_module(void);
void	special_labels_module(void);

extern	EntryPoint	ENTRY(do_interpreter);

int main(int argc, char **argv)
{
#ifndef	SPEED
	/*
	** ensure stdio & stderr are unbuffered even if redirected
	** using setvbuf() is more complicated than using setlinebuf(),
	** but also more portable
	*/

	setvbuf(stdout, NULL, _IONBF, 0);
	setvbuf(stderr, NULL, _IONBF, 0);
#endif

	/* initialize label table */
	init_entries();
	init_modules();

	/* process the command line options, save results in global vars */
	process_options(argc, argv);

	/* execute the selected entry point */
	init_engine();
	run_code();

	return 0;
}

static void process_options(int argc, char *argv[])
{
	int	c;
	int	i;
	int	val;
	List	*ptr;
	List	*label_list;
	Label	*label;
	FILE	*sfp;
	char    input_name[256];

	progname = argv[0];

	for (i = 0; i < MAXFLAG; i++)
		debugflag[i] = FALSE;

	if (default_entry != NULL)
	{
		which = lookup_label_name(default_entry);
		if (which == NULL)
			printf("default entry %s not found\n", default_entry);
	}

	while ((c = getopt(argc, argv, "cltp:d:r:w:s:z:1:2:3:")) != EOF)
	{
		switch (c)
		{

		case 'c':	check_space = TRUE;

		when 'l':	sfp = popen("sort -n", "w");
				label_list = get_all_labels();
				for_list (ptr, label_list)
				{
					label = (Label *) ldata(ptr);
					fprintf(sfp, "%u %x %s\n",
						(unsigned) label->e_addr,
						(unsigned) label->e_addr,
						label->e_name);
				}

				pclose(sfp);

		when 't':	use_own_timer = TRUE;

				calldebug      = FALSE;
				nondstackdebug = FALSE;
				detstackdebug  = FALSE;
				heapdebug      = FALSE;
				gotodebug      = FALSE;
				finaldebug     = FALSE;

		when 'd':	if (streq(optarg, "b"))
					nondstackdebug = TRUE;
				or (streq(optarg, "c"))
					calldebug    = TRUE;
				or (streq(optarg, "d"))
					detaildebug  = TRUE;
				or (streq(optarg, "g"))
					gotodebug    = TRUE;
				or (streq(optarg, "s"))
					detstackdebug   = TRUE;
				or (streq(optarg, "h"))
					heapdebug    = TRUE;
				or (streq(optarg, "f"))
					finaldebug   = TRUE;
				or (streq(optarg, "p"))
					progdebug   = TRUE;
				or (streq(optarg, "m"))
					memdebug    = TRUE;
				or (streq(optarg, "a"))
				{
					calldebug      = TRUE;
					nondstackdebug = TRUE;
					detstackdebug  = TRUE;
					heapdebug      = TRUE;
					gotodebug      = TRUE;
					finaldebug     = TRUE;
				}
				else
					usage();

				use_own_timer = FALSE;

		when 'p':	if (sscanf(optarg, "%d", &pcache_size) != 1)
					usage();

				if (pcache_size < 512)
					pcache_size *= 1024;

		when 'r':	if (sscanf(optarg, "%d", &repeats) != 1)
					usage();

		when 'w':	which = lookup_label_name(optarg);
				if (which == NULL)
				{
					printf("predicate name %s unknown\n",
						optarg);
					exit(1);
				}

		when 's':	if (sscanf(optarg+1, "%d", &val) != 1)
					usage();

				if (optarg[0] == 'h')
					heap_size = val;
				or (optarg[0] == 'd')
					detstack_size = val;
				or (optarg[0] == 'n')
					nondstack_size = val;
				else
					usage();

		when 'z':	if (sscanf(optarg+1, "%d", &val) != 1)
					usage();

				if (optarg[0] == 'h')
					heap_zone_size = val;
				or (optarg[0] == 'd')
					detstack_zone_size = val;
				or (optarg[0] == 'n')
					nondstack_zone_size = val;
				else
					usage();

		when '1':	if (sscanf(optarg, "%d", &r1val) != 1)
					usage();

		when '2':	if (sscanf(optarg, "%d", &r2val) != 1)
					usage();

		when '3':	if (sscanf(optarg, "%d", &r3val) != 1)
					usage();

		otherwise:	usage();

		}
	}

	if (which == NULL)
	{
		printf("no entry name supplied on command line or by default\n");
		exit(1);
	}

	/* search the table for predname_input */
	if (strneq(which->e_name, "mercury__", 9))
		which_input = NULL;
	else
	{
		sprintf(input_name, "%s_input", which->e_name);
		which_input = lookup_label_name(input_name);

		if (which_input == NULL)
			printf("warning: %s not found\n", input_name);
	}
}

static void usage(void)
{
	printf("Usage: %s [-clt] [-d[abcdghs]] [-[sz][hdn]#] [-p#] [-r#] [-1#] [-2#] [-3#] -w name\n",
		progname);
	printf("-c \t\tcheck cross-function stack usage\n");
	printf("-l \t\tprint all labels\n");
	printf("-t \t\tuse own timer\n");
	printf("-dg \t\tdebug gotos\n");
	printf("-dc \t\tdebug calls\n");
	printf("-db \t\tdebug backtracking\n");
	printf("-dh \t\tdebug heap\n");
	printf("-ds \t\tdebug detstack\n");
	printf("-df \t\tdebug final success/failure\n");
	printf("-da \t\tdebug all\n");
	printf("-dm \t\tdebug memory allocation\n");
	printf("-dd \t\tdetailed debug\n");
	printf("-sh<n> \t\tallocate n kb for the heap\n");
	printf("-sd<n> \t\tallocate n kb for the det stack\n");
	printf("-sn<n> \t\tallocate n kb for the nondet stack\n");
	printf("-zh<n> \t\tallocate n kb for the heap redzone");
	printf("-zd<n> \t\tallocate n kb for the det stack redzone");
	printf("-zn<n> \t\tallocate n kb for the nondet stack redzone");
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
			"after final call to %s", which->e_name);
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
			when HP_RN:
				printf("hp");
			when SP_RN:
				printf("sp");
			when CF_RN:
				printf("curfr");
			when MF_RN:
				printf("maxfr");
			otherwise:
				printf("UNKNOWN%d", i);
			}
		}

		printf("\t%ld\n", num_uses[i]);
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

	/* input routines must be deterministic */
	if (which_input != NULL)
		call(which_input->e_addr, LABEL(global_setupdone));

global_setupdone:
	call(which->e_addr, LABEL(global_success));

global_success:
#ifndef	SPEED
	if (finaldebug)
	{
		save_registers();
		printregs("global succeeded");
		if (detaildebug)
			dumpnondstack();
	}
#endif

	redo();

global_fail:
#ifndef	SPEED
	if (finaldebug)
	{
		save_registers();
		printregs("global failed");

		if (detaildebug)
			dumpnondstack();

		maxfr = (Word *) pop();
		succip = (Code *) pop();
		hp = (Word *) pop();

		if (detaildebug)
		{
			save_registers();
			printregs("after popping...");
		}
	}
	else
#endif
	{
		maxfr = (Word *) pop();
		succip = (Code *) pop();
		hp = (Word *) pop();
	}

	proceed();
#ifndef	USE_GCC_NONLOCAL_GOTOS
	return 0;
#endif

END_MODULE
