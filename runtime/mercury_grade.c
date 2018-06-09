// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1997 The University of Melbourne.
// Copyright (C) 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

#include "mercury_imp.h"
#include "mercury_grade.h"

// Define MR_grade_<gr>, where <gr> is the grade that this file is compiled in.
// Every generated .c file includes a reference to this constant;
// if any such file was compiled with a different grade than this file,
// then it will have an unresolved reference which will cause a link error.

const char MR_GRADE_VAR = 0;

// To ensure that the final executable file doesn't have multiple
// copies of this object file linked into it, we export a dummy symbol
// whose name does not depend on the grade.

const char MR_runtime_grade = 0;
