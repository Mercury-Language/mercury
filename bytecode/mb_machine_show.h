
/*
** Copyright (C) 1997 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
**
** $Id: mb_machine_show.h,v 1.1 2001-01-24 07:42:27 lpcam Exp $
**
** Abstract mercury machine
**
*/


#ifndef MB_MACHINE_SHOW_H
#define	MB_MACHINE_SHOW_H

#include <stdio.h>

#include "mb_machine.h"

/* Display the current state of the machine */
void MB_show_state(MB_Machine_State* ms, FILE* fp);

/* Display the call stack of the machine */
void MB_show_call(MB_Machine_State* ms, FILE* fp);

#endif	/* MB_MACHINE_SHOW_H */


