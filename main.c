/*
** Copyright (C) 2000 The University of Melbourne.
** This file may only be copied under the terms of the GNU General
** Public License - see the file COPYING in the Mercury distribution.
*/

/*
** main.c
**
** This file defines the main() function for the `compiler/mercury_compile'
** executable.  We define main() separately here,
** because compiler/mercury_compile is built as a library;
** this is necessary for the GCC back-end port, where the
** mercury_compile library is linked together with the
** GCC back-end into the `cc1mercury' program,
** whose main() function is defined in gcc/toplev.c.
**
** The main() function here just calls mercury_main().
** mercury_main() is defined in compiler/mercury_compile_init.c,
** which is automatically generated (by scripts/c2init,
** which invokes mkinit, whose source is in util/mkinit.c).
** It initializes the Mercury runtime and then calls the
** main/2 predicate, which is defined compiler/mercury_compile.m.
**
** For general information about the design of the Mercury compiler,
** see compiler/notes/compiler_design.html.
*/

extern int mercury_main(int argc, char *argv[]);

int
main(int argc, char *argv[])
{
	return mercury_main(argc, argv);
}
