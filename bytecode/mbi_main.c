/*
** Copyright (C) 1997 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
**
** $Id: mbi_main.c,v 1.2 1997-07-27 14:59:26 fjh Exp $
*/

/* Imports */

#include	"mbi.h"

/* Local declarations */

static char
rcs_id[]	= "$Id: mbi_main.c,v 1.2 1997-07-27 14:59:26 fjh Exp $";

/* Implementation */

int
main(int argc, char* argv[])
{
	return BC_mbi_main(argc, argv);
}
