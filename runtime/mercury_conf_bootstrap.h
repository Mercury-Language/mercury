/*
 * ** Copyright (C) 2002 The University of Melbourne.
 * ** This file may only be copied under the terms of the GNU Library General
 * ** Public License - see the file COPYING.LIB in the Mercury distribution.
 * */

/*
 * ** mercury_conf_bootstrap.h
 * **
 * ** Backwards compatability definitions for auto-configured macros.
 * ** All of the definitions here will go away eventually, so don't use them!
 * */

#ifndef MERCURY_CONF_BOOTSTRAP_H
#define MERCURY_CONF_BOOTSTRAP_H

#ifndef MR_NO_CONF_BACKWARDS_COMPAT

/*
** Header files.
*/
#ifdef MR_HAVE_SYS_SIGINFO_H
  #define HAVE_SYS_SIGINFO 1
#endif
#ifdef MR_HAVE_SYS_SIGNAL_H
  #define HAVE_SYS_SIGNAL 1
#endif
#ifdef MR_HAVE_UCONTEXT_H
  #define HAVE_UCONTEXT 1
#endif
#ifdef MR_HAVE_SYS_UCONTEXT_H
  #define HAVE_SYS_UCONTEXT 1
#endif
#ifdef MR_HAVE_ASM_SIGCONTEXT_H
  #define HAVE_ASM_SIGCONTEXT 1
#endif
#ifdef MR_HAVE_SYS_TIME_H
  #define HAVE_SYS_TIME 1
#endif
#ifdef MR_HAVE_UNISTD_H
  #define HAVE_UNISTD_H 1
#endif
#ifdef MR_HAVE_SYS_PARAM_H
  #define HAVE_SYS_PARAM 1
#endif
#ifdef MR_HAVE_SYS_WAIT_H
  #define HAVE_SYS_WAIT 1
#endif
#ifdef MR_HAVE_SYS_STAT_H
  #define HAVE_SYS_STAT_H 1
#endif
#ifdef MR_HAVE_SYS_TIMES_H
  #define HAVE_SYS_TIMES_H 1
#endif
#ifdef MR_HAVE_SYS_TYPES_H
  #define HAVE_SYS_TYPES_H 1
#endif
#ifdef MR_HAVE_DLFCN_H
  #define HAVE_DLFCN_H 1
#endif
#ifdef MR_HAVE_FCNTL_H
  #define HAVE_FCNTL_H 1
#endif
#ifdef MR_HAVE_TERMIOS_H
  #define HAVE_TERMIOS_H 1
#endif
#ifdef MR_HAVE_SYS_IOCTL_H
  #define HAVE_SYS_IOCTL_H 1
#endif
#ifdef MR_HAVE_SYS_STROPTS_H
  #define HAVE_SYS_STROPTS_H 1
#endif
#ifdef MR_HAVE_READLINE_READLINE_H
  #define HAVE_READLINE_READLINE 1
#endif
#ifdef MR_HAVE_READLINE_HISTORY_H
  #define HAVE_READLINE_HISTORY 1
#endif

/*
** Library functions.
*/
#ifdef MR_HAVE_GETPID
  #define HAVE_GETPID 1
#endif
#ifdef MR_HAVE_SETPGID
  #define HAVE_SETPGID 1
#endif
#ifdef MR_HAVE_FORK
  #define HAVE_FORK 1
#endif
#ifdef MR_HAVE_EXECLP
  #define HAVE_EXECLP 1
#endif
#ifdef MR_HAVE_WAIT
  #define HAVE_WAIT 1
#endif
#ifdef MR_HAVE_KILL
  #define HAVE_KILL 1
#endif
#ifdef MR_HAVE_GETHOSTNAME
  #define HAVE_GETHOSTNAME 1
#endif
#ifdef MR_HAVE_SNPRINTF
  #define HAVE_SNPRINTF 1
#endif
#ifdef MR_HAVE_VSNPRINTF
  #define HAVE_VSNPRINTF 1
#endif
#ifdef MR_HAVE__VSNPRINTF
  #define HAVE__VSNPRINTF 1
#endif
#ifdef MR_HAVE_SYSCONF
  #define HAVE_SYSCONF 1
#endif
#ifdef MR_HAVE_SIGACTION
  #define HAVE_SIGACTION 1
#endif
#ifdef MR_HAVE_GETPAGESIZE
  #define HAVE_GETPAGESIZE 1
#endif
#ifdef MR_HAVE_MEMALIGN
  #define HAVE_MEMALIGN 1
#endif
#ifdef MR_HAVE_MPROTECT
  #define HAVE_MPROTECT 1
#endif
#ifdef MR_HAVE_STRERROR
  #define HAVE_STRERROR 1
#endif
#ifdef MR_HAVE_SETITIMER
  #define HAVE_SETITIMER 1
#endif
#ifdef MR_HAVE_MEMMOVE
  #define HAVE_MEMMOVE 1
#endif
#ifdef MR_HAVE_DLOPEN
  #define HAVE_DLOPEN 1
#endif
#ifdef MR_HAVE_DLCLOSE
  #define HAVE_DLCLOSE 1
#endif
#ifdef MR_HAVE_DLSYM
  #define HAVE_DLSYM 1
#endif
#ifdef MR_HAVE_DLERROR
  #define HAVE_DLERROR 1
#endif
#ifdef MR_HAVE_STAT
  #define HAVE_STAT 1
#endif
#ifdef MR_HAVE_FSTAT
  #define HAVE_FSTAT 1
#endif
#ifdef MR_HAVE_FDOPEN
  #define HAVE_FDOPEN 1
#endif
#ifdef MR_HAVE_OPEN
  #define HAVE_OPEN 1
#endif
#ifdef MR_HAVE_CLOSE
  #define HAVE_CLOSE 1
#endif
#ifdef MR_HAVE_DUP
  #define HAVE_DUP 1
#endif
#ifdef MR_HAVE_DUP2
  #define HAVE_DUP2 1
#endif
#ifdef MR_HAVE_FILENO
  #define HAVE_FILENO 1
#endif
#ifdef MR_HAVE_ISATTY
  #define HAVE_ISATTY 1
#endif
#ifdef MR_HAVE_GRANTPT
  #define HAVE_GRANTPT 1
#endif
#ifdef MR_HAVE_UNLOCKPT
  #define HAVE_UNLOCKPT 1
#endif
#ifdef MR_HAVE_PTSNAME
  #define HAVE_PTSNAME 1
#endif
#ifdef MR_HAVE_TCGETATTR
  #define HAVE_TCGETATTR 1
#endif
#ifdef MR_HAVE_TCSETATTR
  #define HAVE_TCSETATTR 1
#endif
#ifdef MR_HAVE_IOCTL
  #define HAVE_IOCTL 1
#endif

/*
** Command-line parameters
*/
#ifdef MR_USE_GCC_GLOBAL_REGISTERS
  #define USE_GCC_GLOBAL_REGISTERS 1
#endif
#ifdef MR_USE_GCC_NONLOCAL_GOTOS
  #define USE_GCC_NONLOCAL_GOTOS 1
#endif
#ifdef MR_USE_ASM_LABELS
  #define USE_ASM_LABELS 1
#endif
#ifdef MR_CONSERVATIVE_GC
  #define CONSERVATIVE_GC 1
#endif
#ifdef MR_NATIVE_GC
  #define NATIVE_GC 1
#endif
#ifdef MR_NO_TYPE_LAYOUT
  #define NO_TYPE_LAYOUT 1
#endif
#ifdef MR_BOXED_FLOAT
  #define BOXED_FLOAT 1
#endif
#ifdef MR_USE_SINGLE_PREC_FLOAT
  #define USE_SINGLE_PREC_FLOAT 1
#endif
#ifdef MR_SPLIT_C_FILES
  #define SPLIT_C_FILES 1
#endif
#ifdef MR_INLINE_ALLOC
  #define INLINE_ALLOC 1
#endif
#ifdef MR_PIC_REG
  #define PIC_REG 1
#endif
/*
** We dont't do this for PIC/MR_PIC because MR_PIC can be defined by
** mercury_goto.h. User code should not be using the PIC macro anyway.
*/
#ifdef MR_HIGHTAGS
  #define HIGHTAGS 1
#endif
#ifndef TAGBITS
  #define TAGBITS MR_TAGBITS
#endif

/*
** Other stuff.
*/
#ifdef MR_HAVE_SIGINFO
  #define HAVE_SIGINFO 1
#endif
#ifdef MR_HAVE_SIGINFO_T
  #define HAVE_SIGINFO_T 1
#endif
#ifdef MR_HAVE_SIGCONTEXT_STRUCT
  #define HAVE_SIGCONTEXT_STRUCT 1
#endif
#ifdef MR_HAVE_SIGCONTEXT_STRUCT_3ARG
  #define HAVE_SIGCONTEXT_STRUCT_3ARG 1
#endif
#ifdef MR_HAVE_SIGCONTEXT_STRUCT_2ARG
  #define HAVE_SIGCONTEXT_STRUCT_2ARG 1
#endif
#ifdef MR_SIGACTION_FIELD
  #define SIGACTION_FIELD MR_SIGACTION_FIELD
#endif
#ifdef MR_LOW_TAG_BITS
  #define LOW_TAG_BITS MR_LOW_TAG_BITS
#endif
#ifdef MR_BYTES_PER_WORD
  #define BYTES_PER_WORD MR_BYTES_PER_WORD
#endif
#ifdef MR_BITS_PER_WORD
  #define BITS_PER_WORD MR_BITS_PER_WORD
#endif
#ifdef MR_SYNC_TERM_SIZE
  #define SYNC_TERM_SIZE MR_SYNC_TERM_SIZE
#endif
#ifdef MR_HAVE_GCC_LABELS
  #define HAVE_GCC_LABELS 1
#endif
#ifdef MR_HAVE_ASM_LABELS
  #define HAVE_ASM_LABELS 1
#endif
#ifdef MR_USE_DLLS
  #define USE_DLLS 1
#endif
#ifdef MR_PC_ACCESS_GREG
  #define PC_ACCESS_GREG 1
#endif
#ifdef MR_PC_ACCESS
  #define PC_ACCESS MR_PC_ACCESS
#endif
#ifdef MR_HAVE_DEV_PTMX
  #define HAVE_DEV_PTMX 1
#endif

#endif /* !MR_NO_CONF_BACKWARDS_COMPAT */

#endif /* MERCURY_CONF_BOOTSTRAP_H */
