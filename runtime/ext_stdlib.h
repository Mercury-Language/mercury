/*
** Copyright (C) 1995 University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/
#ifndef	EXT_STDLIB_H
#define	EXT_STDLIB_H

#ifdef HAVE_CWD_DECL
#include <unistd.h>
#else
extern	char	*getcwd(char *, size_t);
#endif

extern	int	getopt(int, char *const *, const char *);
extern	char	*optarg;
extern	int	optind, opterr, optopt;
extern	void	*memalign(size_t, size_t);

#endif /* EXT_STDLIB_H */
