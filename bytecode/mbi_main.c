/*
** Copyright (C) 1997 University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
**
** $Id: mbi_main.c,v 1.1 1997-05-07 12:12:29 fjh Exp $
*/

/* Imports */

#include	"mbi.h"

/* Local declarations */

static char
rcs_id[]	= "$Id: mbi_main.c,v 1.1 1997-05-07 12:12:29 fjh Exp $";

/* Implementation */

int
main(int argc, char* argv[])
{
	return BC_mbi_main(argc, argv);
}
