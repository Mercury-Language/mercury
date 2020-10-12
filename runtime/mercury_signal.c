// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1998,2000,2002, 2006, 2010 The University of Melbourne.
// Copyright (C) 2015-2016, 2018, 2020 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// This module defines functions for setting up signal handlers.

////////////////////////////////////////////////////////////////////////////

#include "mercury_imp.h"
#include "mercury_signal.h"

#ifdef MR_HAVE_UNISTD_H
  #include <unistd.h>
#endif

#include <stdio.h>
#include <string.h>
#include <errno.h>

#include "mercury_signal.h"

#ifdef MR_HAVE_SYS_SIGINFO_H
  #include <sys/siginfo.h>
#endif

#ifdef  MR_HAVE_MPROTECT
  #include <sys/mman.h>
#endif

#ifdef  MR_HAVE_UCONTEXT_H
  #include <ucontext.h>
#endif

#ifdef  MR_HAVE_SYS_UCONTEXT_H
  #include <sys/ucontext.h>
#endif

////////////////////////////////////////////////////////////////////////////

// If we don't have SA_RESTART define it as 0. It is still better to use
// sigaction without SA_RESTART than to use signal.

#if !defined(SA_RESTART)
  #define   SA_RESTART 0
#endif

static void MR_do_setup_signal(int sig, MR_Code *handler, MR_bool need_info,
                MR_bool restart, const char *error_message);

void
MR_setup_signal(int sig, MR_Code *handler, MR_bool need_info,
    const char *error_message)
{
    MR_do_setup_signal(sig, handler, need_info, MR_TRUE, error_message);
}

void
MR_setup_signal_no_restart(int sig, MR_Code *handler, MR_bool need_info,
    const char *error_message)
{
    MR_do_setup_signal(sig, handler, need_info, MR_FALSE, error_message);
}

void
MR_do_setup_signal(int sig, MR_Code *handler, MR_bool need_info,
    MR_bool restart, const char *error_message)
{
    MR_signal_action    act;

    MR_init_signal_action(&act, handler, need_info, restart);
    MR_set_signal_action(sig, &act, error_message);
}

void
MR_reset_signal(int sig)
{
    MR_signal_action    act;

#ifdef MR_HAVE_SIGACTION
    if (sigemptyset(&(act.sa_mask)) != 0) {
        MR_perror("cannot set clear signal mask");
        exit(1);
    }
    errno = 0;

    act.sa_flags = 0;
    act.sa_handler = SIG_DFL;
#else
    act = SIG_DFL;
#endif
    MR_set_signal_action(sig, &act, "Couldn't reset signal");
}

void
MR_init_signal_action(MR_signal_action *act, MR_Code *handler,
    MR_bool need_info, MR_bool restart)
{
#ifdef MR_HAVE_SIGACTION

    act->sa_flags = (restart ? SA_RESTART : 0);

    if (need_info) {
    #ifdef MR_HAVE_SIGINFO_T
        act->sa_flags |= SA_SIGINFO;
        act->sa_sigaction = handler;
    #else
        // This branch should be unreachable in practice.
        // The caller must check that MR_HAVE_SIGINFO_T is defined
        // before calling this function with need_info=TRUE,
        // otherwise the handler will have the wrong type.
        act->sa_handler = handler;
    #endif
    } else {
        act->sa_handler = handler;
    }

    if (sigemptyset(&(act->sa_mask)) != 0) {
        MR_perror("cannot set clear signal mask");
        exit(1);
    }
    errno = 0;

#else // not MR_HAVE_SIGACTION

    *act = handler;

#endif // not MR_HAVE_SIGACTION
}

void
MR_get_signal_action(int sig, MR_signal_action *act, const char *error_message)
{
#ifdef MR_HAVE_SIGACTION
    if (sigaction(sig, NULL, act) != 0) {
        MR_perror(error_message);
        exit(1);
    }
#else // not MR_HAVE_SIGACTION
    *act = signal(sig, NULL);
    if (*act == SIG_ERR) {
        MR_perror(error_message);
        exit(1);
    }
#endif // not MR_HAVE_SIGACTION
}

void
MR_set_signal_action(int sig, MR_signal_action *act,
            const char *error_message)
{
#ifdef MR_HAVE_SIGACTION
    if (sigaction(sig, act, NULL) != 0) {
        MR_perror(error_message);
        exit(1);
    }
#else // not MR_HAVE_SIGACTION
    if (signal(sig, *act) == SIG_ERR) {
        MR_perror(error_message);
        exit(1);
    }
#endif // not MR_HAVE_SIGACTION
}

void
MR_signal_should_restart(int sig, MR_bool restart)
{
#if defined(MR_HAVE_SIGACTION)
    struct sigaction    act;

    if (sigaction(sig, NULL, &act) != 0) {
        MR_perror("error setting signal system call behaviour");
        exit(1);
    }

    if (restart) {
        act.sa_flags |= SA_RESTART;
    } else {
        act.sa_flags &= ~SA_RESTART;
    }

    if (sigaction(sig, &act, NULL) != 0) {
        MR_perror("error setting signal system call behaviour");
        exit(1);
    }
#elif defined(MR_HAVE_SIGINTERRUPT)
    if (siginterrupt(sig, !restart) != 0) {
        MR_perror("error setting signal system call behaviour");
        exit(1);
    }
#endif
}
