/*
** Copyright (C) 2003-2004 Peter Schachte and The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

typedef struct IteEntry{
	MR_ROBDD_node *f;
	MR_ROBDD_node *g;
	MR_ROBDD_node *h;
	MR_ROBDD_node *where;
} iteEntry;
