
/*
** Copyright (C) 2000 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** This file contains utility functions for debugger.
**
** Author: zs.
*/

#include "mercury_imp.h"
#include "mercury_trace_util.h"

#include <ctype.h>

bool
MR_trace_is_number(const char *word, int *value)
{
	if (MR_isdigit(*word)) {
		*value = *word - '0';
		word++;
		while (MR_isdigit(*word)) {
			*value = (*value * 10) + *word - '0';
			word++;
		}

		if (*word == '\0') {
			return TRUE;
		}
	}

	return FALSE;
}
