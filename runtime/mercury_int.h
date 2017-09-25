// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2017 The Mercury team.
// This file may only be copied under the terms of the GNU Library General
// Public License - see the file COPYING.LIB in the Mercury distribution.

// mercury_int.h - integer type handling

#ifndef MERCURY_INT_H
#define MERCURY_INT_H

#include "mercury_conf.h"       // for MR_BOXED_FLOAT, MR_CONSERVATIVE_GC
#include "mercury_types.h"      // for `MR_Word'
#include "mercury_std.h"        // for `MR_bool'

#if defined(MR_GNUC) || defined(MR_CLANG)
  #define MR_uint16_reverse_bytes(U) __builtin_bswap16((U))
#else
  #define MR_uint16_reverse_bytes(U) (((U & 0xff00) >> 8) | \
                                      ((U & 0x00ff) << 8))
#endif

#if defined(MR_GNUC) || defined(MR_CLANG)
  #define MR_uint32_reverse_bytes(U) __builtin_bswap32((U))
#else
  #define MR_uint32_reverse_bytes(U) ((U & UINT32_C(0x000000ff)) << 24 | \
                                      (U & UINT32_C(0x0000ff00)) << 8  | \
                                      (U & UINT32_C(0x00ff0000)) >> 8  | \
                                      (U & UINT32_C(0xff000000)) >> 24 )
#endif

#endif // not MERCURY_INT_H
