/*
** vim:ts=4 sw=4 expandtab
*/
/*
** Copyright (C) 2011 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_windows.h - this header provides a wrapper around windows.h
** Mercury runtime and library modules should #include this header rather
** than #including windows.h directly.
**
*/

#ifndef MERCURY_WINDOWS_H
#define MERCURY_WINDOWS_H

/*
** NOTE: the following is not protected by any guards - it is the
** responsibility of modules that #include this one to ensure that
** the windows.h is available before #including this file.
*/

/*
** Defining WIN32_LEAN_AND_MEAN disables a series of #includes inside
** windows.h -- notably it disables the #include of winsock.h, the inclusion
** of which renders the winsock2 API unusable in Mercury foreign_procs.
*/
#if !defined(WIN32_LEAN_AND_MEAN)
    #define WIN2_LEAN_AND_MEAN
#endif

#include <windows.h>

#endif /* not MERCURY_WINDOWS_H */
