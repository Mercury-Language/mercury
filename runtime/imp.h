/*
** Copyright (C) 1995-1997 University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** imp.h - defines the interface to the Mercury abstract machine.
**
** IMPORTANT: this must be the *first* header file that is #included.
** It must come before any system header files.  This is because on some
** systems, the system header files include inline functions, and this
** causes problems when using global register variables, as gcc requires
** global register variable declarations to precede any function definitions.
**
** This file just #includes most of the other Mercury runtime header files.
*/

#ifndef IMP_H
#define IMP_H

#include	"regs.h"	/* must come before system headers */

#include	"conf.h"

#include	"std.h"

#include	"mercury_types.h"
#include	"mercury_string.h"
#include	"mercury_float.h"

#include	"tags.h"
#include	"goto.h"
#include	"calls.h"
#include	"engine.h"

#include	"memory.h"
#include	"heap.h"
#include	"stacks.h"
#include	"overflow.h"

#include	"label.h"
#include	"wrapper.h"
#include	"context.h"
#include	"type_info.h"
#ifdef CONSTRAINTS
#include	"mercury_solver_backtrack.h"
#endif

#include	"debug.h"
#include	"prof.h"
#include	"misc.h"

#endif /* not IMP_H */
