/*
** Copyright (C) 1997 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/
#include "mercury_imp.h"
#include "mercury_grade.h"

/*
** Define MR_grade_<gr>, where <gr> is the grade that this file is compiled in.
** Every generated .c file includes a reference to this constant;
** if any such file was compiled with a different grade than this file,
** then it will have an unresolved reference which will cause a link error.
*/
const char MR_GRADE_VAR = 0;
