/*
** Copyright (C) 1995 University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/
#ifndef	WRAPPER_H
#define	WRAPPER_H

extern	const	char	*progname;
extern	int	mercury_argc;
extern	char **	mercury_argv;
extern	int	mercury_exit_status;

extern	uint	heap_size;
extern	uint	detstack_size;
extern	uint	nondstack_size;

extern	uint	heap_zone_size;
extern	uint	detstack_zone_size;
extern	uint	nondstack_zone_size;

extern	uint	pcache_size;

extern	int	r1val;
extern	int	r2val;
extern	int	r3val;

extern	bool	check_space;

extern	int	repcounter;
extern	char	scratchbuf[];

#endif
