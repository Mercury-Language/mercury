
/*
** Copyright (C) 2000-2001 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
**
** Module to display the state of the mercury machine
**
*/

#ifndef MB_MACHINE_SHOW_H
#define	MB_MACHINE_SHOW_H

#include "mb_basetypes.h"

#include <stdio.h>
#include "mb_machine.h"

/* Display the current state of the machine */
void MB_show_state(MB_Machine_State *ms, FILE *fp);

/* Display the call stack of the machine */
void MB_show_call(MB_Machine_State *ms, FILE *fp);

#endif	/* MB_MACHINE_SHOW_H */

