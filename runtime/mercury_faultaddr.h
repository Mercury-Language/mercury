// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1998-1999 The University of Melbourne.
// Copyright (C) 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// mercury_faultaddr.h:
// Macros for determining the fault address of a signal.
// This is usually non-portable, so architecture specific versions
// are given here, so a single macro can be used elsewhere in the
// system (in particular, this code is necessary both in the
// runtime and in the configuration scripts).

#ifndef MERCURY_FAULT_ADDR_H
#define MERCURY_FAULT_ADDR_H

#if defined(__i386__)

  #define MR_GET_FAULT_ADDR(sc)                                         \
    ((void *) (sc).cr2)

#elif defined(__mc68000__)

  #define MR_GET_FAULT_ADDR(sc)                                         \
    ({                                                                  \
        struct sigcontext *scp = (struct sigcontext *) sc;              \
        int format = (scp->sc_formatvec >> 12) & 0xf;                   \
        unsigned long *framedata = (unsigned long *)(scp + 1);          \
        unsigned long ea;                                               \
        if (format == 0xa || format == 0xb) {                           \
            /* 68020/030 */                                             \
            ea = framedata[2];                                          \
        } else if (format == 7) {                                       \
            /* 68040 */                                                 \
            ea = framedata[3];                                          \
        } else if (format == 4) {                                       \
            /* 68060 */                                                 \
            ea = framedata[0];                                          \
            if (framedata[1] & 0x08000000) {                            \
                /* Correct addr on misaligned access. */                \
                ea = (ea+4095)&(~4095);                                 \
            }                                                           \
            (void *) ea;                                                \
        }                                                               \
    })
#else

// This space deliberately left blank.
//
// We will get a compile error if the macro is used but not defined.

#endif

#endif // not MERCURY_FAULT_ADDR_H
