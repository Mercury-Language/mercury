/*
** Copyright (C) 1995 University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/
#ifndef	GETOPT_H
#define	GETOPT_H

#define	GETOPTHUH	'?'
#define	GETOPTDONE	(-1)

extern int	getopt(int, char *const*, const char *);

extern char	*optarg;
extern int	opterr;
extern int	optind;
extern int	optopt;

#endif
