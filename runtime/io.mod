/*
**  File: code/io.mod.
**  Main author: fjh.
** 
**  This file implements parts of the Mercury standard library
**  modules `io', `require', and `std_util'.
*/

#include "imp.h"

/*
** Mercury files are not quite the same as C stdio FILEs,
** because we keep track of a little bit more information.
*/

typedef struct {
	FILE *file;
	int line_number;
} MercuryFile;

MercuryFile mercury_stdin = { stdin, 0 };
MercuryFile mercury_stdout = { stdout, 0 };
MercuryFile mercury_stderr = { stderr, 0 };
MercuryFile *mercury_current_input = &mercury_stdin;
MercuryFile *mercury_current_output = &mercury_stdout;

#define update_io(r_src, r_dest) ((r_dest) = (r_src))

static MercuryFile*
mercury_open(const char *filename, const char *type)
{
	MercuryFile *mf;
	FILE *f;
	
	f = fopen(filename, type);
	if (!f) return NULL;
	mf = (MercuryFile*) newmem(sizeof(MercuryFile));
	if (!mf) {
		fclose(f);
		return NULL;
	}
	mf->file = f;
	mf->line_number = 0;
	return mf;
}

static void
mercury_print_string(MercuryFile* mf, const char *s)
{
	fprintf(mf->file, "%s", s);
	while (*s) {
		if (*s++ == '\n') {
			mf->line_number++;
		}
	}
}
	
static int
mercury_getc(MercuryFile* mf)
{
	int c = getc(mf->file);
	if (c == '\n') {
		mf->line_number++;
	}
	return c;
}
	
static void
mercury_close(MercuryFile* mf)
{
	assert(mf != &mercury_stdin);
	assert(mf != &mercury_stdout);
	assert(mf != &mercury_stderr);
	fclose(mf->file);
	oldmem(mf);
}

BEGIN_MODULE(io_module)

BEGIN_CODE

/* input predicates */

mercury__io__read_char_code_3_0:
	r1 = mercury_getc(mercury_current_input);
	update_io(r2, r3);
	proceed();

/* output predicates - with output to mercury_current_output */

mercury__io__write_string_3_0:
	mercury_print_string(mercury_current_output, (char *) r1);
	update_io(r2, r3);
	proceed();

mercury__io__write_int_3_0:
	fprintf(mercury_current_output->file, "%d", (int) r1);
	update_io(r2, r3);
	proceed();

mercury__io__write_float_3_0:
	fatal_error("floating point not implemented");

mercury__io__write_anything_3_0:
	fprintf(mercury_current_output->file,
		"<write_anything not implemented>\n");
	update_io(r2, r3);
	proceed();

mercury__io__flush_output_2_0:
	fflush(mercury_current_output->file);
	update_io(r1, r2);
	proceed();

/* output predicates - with output to the specified stream */

mercury__io__write_string_4_0:
	mercury_print_string((MercuryFile*)r1, (char *)r2);
	update_io(r3, r4);
	proceed();

mercury__io__write_int_4_0:
	fprintf(((MercuryFile*)r1)->file, "%d", (int) r2);
	update_io(r3, r4);
	proceed();

mercury__io__write_float_4_0:
	fatal_error("floating point not implemented");

mercury__io__write_anything_4_0:
	fprintf(((MercuryFile*)r1)->file,
		"<write_anything not implemented>\n");
	update_io(r3, r4);
	proceed();

mercury__io__flush_output_3_0:
	fflush(((MercuryFile*)r1)->file);
	update_io(r2, r3);
	proceed();

/* stream predicates */

mercury__io__stdin_stream_3_0:
	r1 = (int) &mercury_stdin;
	update_io(r2, r3);
	proceed();

mercury__io__stdout_stream_3_0:
	r1 = (int) &mercury_stdout;
	update_io(r2, r3);
	proceed();

mercury__io__stderr_stream_3_0:
	r1 = (int) &mercury_stderr;
	update_io(r2, r3);
	proceed();

mercury__io__input_stream_3_0:
	r1 = (int) mercury_current_input;
	update_io(r2, r3);
	proceed();

mercury__io__output_stream_3_0:
	r1 = (int) mercury_current_output;
	update_io(r2, r3);
	proceed();

mercury__io__set_input_stream_4_0:
	r1 = (int) mercury_current_input;
	mercury_current_input = (MercuryFile*) r2;
	update_io(r3, r4);
	proceed();

mercury__io__set_output_stream_4_0:
	r1 = (int) mercury_current_output;
	mercury_current_output = (MercuryFile*) r2;
	update_io(r3, r4);
	proceed();

/* stream open/close predicates */

mercury__io__do_open_input_5_0:
	r3 = r2 = (int)mercury_open((char*)r1, "r");
	update_io(r4, r5);
	proceed();

mercury__io__do_open_output_5_0:
	r3 = r2 = (int)mercury_open((char*)r1, "w");
	update_io(r4, r5);
	proceed();

mercury__io__do_open_append_5_0:
	r3 = r2 = (int)mercury_open((char*)r1, "a");
	update_io(r4, r5);
	proceed();

mercury__io__close_input_3_0:
	mercury_close((MercuryFile*)r1);
	update_io(r2, r3);
	proceed();

mercury__io__close_output_3_0:
	mercury_close((MercuryFile*)r1);
	update_io(r2, r3);
	proceed();

/* miscellaneous predicates */

mercury__io__progname_4_0:
	r2 = (progname ? (int) progname : r1);
	update_io(r3, r4);
	proceed();

mercury__io__preallocate_heap_space_3_0:
	/* don't do anything - preallocate_heap_space was just a
	   hack for NU-Prolog */
	update_io(r2, r3);
	proceed();

mercury__io_call_system_code_4_0:
	r2 = system((char *)r1);
	update_io(r3, r4);
	proceed();

/* error/1, from require.nl */

mercury__error_1_0:
	fprintf(stderr, "Software error: %s\n", (char *) r1);
	abort();
#ifndef	USE_GCC_NONLOCAL_GOTOS
	return 0;	/* suppress some dumb warnings */
#endif

/* report_stats/0 and type_to_univ/2, from std_util.nl */

mercury__report_stats_0_0:
	fprintf(mercury_current_output->file, 
		"[Heap: %.3fk, D Stack: %.3fk, ND Stack: %.3fk]\n",
		((char *)hp - (char *)heapmin) / 1000.0,
		((char *)sp - (char *)detstackmin) / 1000.0,
		((char *)maxfr - (char *)nondstackmin) / 1000.0
	);
	proceed();

mercury__type_to_univ_2_0:
	r2 = r1;
	proceed();

mercury__type_to_univ_2_1:
	r2 = r3;
	r1 = TRUE;  /* for the moment, run-time type check not implemented */
	proceed();

END_MODULE
