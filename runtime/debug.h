/*
** Copyright (C) 1995 University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/
#ifndef DEBUG_H
#define DEBUG_H

#ifdef DEBUG_ON
	#define DEBUG(X) X
#else
	#define DEBUG(X)
#endif

#define DONT(X)

#endif
