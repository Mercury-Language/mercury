// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2011 The University of Melbourne.
// Copyright (C) 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// mercury_windows.h - this header provides a wrapper around windows.h
// Mercury runtime and library modules should #include this header rather
// than #including windows.h directly.
//

#ifndef MERCURY_WINDOWS_H
#define MERCURY_WINDOWS_H

#include "mercury_conf_param.h"

#if defined(MR_WIN32)

    // Defining WIN32_LEAN_AND_MEAN disables a series of #includes inside
    // windows.h -- notably it disables the #include of winsock.h,
    // the inclusion of which renders the winsock2 API unusable in Mercury
    // foreign_procs.

    #if !defined(WIN32_LEAN_AND_MEAN)
        #define WIN32_LEAN_AND_MEAN
    #endif

    #include <windows.h>
#endif

#endif // not MERCURY_WINDOWS_H
