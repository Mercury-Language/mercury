// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1998-2004 The University of Melbourne.
// Copyright (C) 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// mercury_reg_workarounds.h - MR_assign_structure(), MR_memcpy(), MR_fd_zero()

#ifndef MERCURY_REG_WORKAROUNDS_H
#define MERCURY_REG_WORKAROUNDS_H

#include "mercury_conf.h"

#ifdef MR_CAN_DO_PENDING_IO
  #ifdef MR_HAVE_SYS_SELECT_H
    #include <sys/select.h>     // for select() -- POSIX
  #endif
  #include <sys/types.h>        // for fd_set
  #include <sys/time.h>         // for FD_ZERO()
#endif

#include <stdlib.h>         // for size_t

// This macro defines a safe way to perform assignment between structures.
// The obvious way can cause some versions of gcc to abort on x86 processors
// with the message "fixed or forbidden register was spilled."

#if defined(MR_CANNOT_USE_STRUCTURE_ASSIGNMENT) &&                      \
    defined(MR_USE_GCC_GLOBAL_REGISTERS)

  #define MR_assign_structure(dest, src)                                \
            MR_memcpy(&(dest), &(src), sizeof((dest)))

  // We use our own version of memcpy because gcc recognises calls to the
  // standard memcpy (even in things that do not mention memcpy by name, e.g.
  // structure assignments) and generates inline code for them. Unfortunately
  // this causes gcc to abort because it tries to use a register that we have
  // already reserved.
  // XXX We should fix this eventually by using -fno-builtin since pragma
  // c_code may call the builtin functions.

  extern    void    MR_memcpy(void *dest, const void *src, size_t nbytes);
  extern    void    MR_memset(void *dest, char c, size_t nbytes);

#else // !MR_CANNOT_USE_STRUCTURE_ASSIGNMENT || !MR_USE_GCC_GLOBAL_REGISTERS

  #define MR_assign_structure(dest, src)    ((dest) = (src))
  #define MR_memcpy(dest, src, nbytes)      memcpy((dest), (src), (nbytes))
  #define MR_memset(dest, c, nbytes)        memset((dest), (c), (nbytes))

#endif // !MR_CANNOT_USE_STRUCTURE_ASSIGNMENT || !MR_USE_GCC_GLOBAL_REGISTERS

// We use a forwarding function to FD_ZERO because the Linux headers
// use an asm fragment which conflicts with our use of global registers.

#ifdef MR_CAN_DO_PENDING_IO
  void MR_fd_zero(fd_set *fdset);
#endif

#endif // not MERCURY_REG_WORKAROUNDS_H
