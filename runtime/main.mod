/*
** Copyright (C) 1995 University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/
/*
**  File: code/main.mod.
**  Main author: fjh.
** 
**  A default do-nothing implementation of main/2.
*/

#include "imp.h"

BEGIN_MODULE(main_module)
BEGIN_CODE

mercury__main_2_0:
	fprintf(stderr, "Mercury Runtime: main/2 undefined\n");
	r2 = r1;
	proceed();

END_MODULE
