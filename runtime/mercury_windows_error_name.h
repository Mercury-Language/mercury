// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2022 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

#ifndef MERCURY_WINDOWS_ERROR_NAME_H
#define MERCURY_WINDOWS_ERROR_NAME_H

#include "mercury_windows.h"

#ifdef MR_WIN32

extern const char   *MR_win32_error_name(DWORD errcode);

#endif

#endif  // MERCURY_WINDOWS_ERROR_NAME_H
