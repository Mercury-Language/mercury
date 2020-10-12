// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1998, 2000, 2002, 2004-2005, 2010 The University of Melbourne.
// Copyright (C) 2016, 2018, 2020 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// mercury_signal.h - functions for setting up signal handlers.
//
// This defines a generic signal handler setup mechanism.

#ifndef MERCURY_SIGNAL_H
#define MERCURY_SIGNAL_H

#include "mercury_regs.h"   // include before system headers
#include "mercury_conf.h"

#include <signal.h>

#include "mercury_types.h"
#include "mercury_std.h"

#ifdef MR_HAVE_SIGACTION
typedef struct sigaction    MR_signal_action;
#else
typedef MR_Code *       MR_signal_action;
#endif

// MR_setup_signal sets a signal handler (handler) to handle signals
// of the given signal type (sig).
// If the handler cannot be setup, it aborts with the given error message.
//
// If the signal handler requires siginfo to be provided (e.g. it needs
// access to stored registers), need_info must be MR_TRUE.
// Note that on some platforms, signal information is provided
// regardless of the value of need_info.

extern  void    MR_setup_signal(int sig, MR_Code *handler, MR_bool need_info,
                    const char * error_message);

// As above, but don't arrange for system calls to be restarted
// if the signal is received.

extern  void    MR_setup_signal_no_restart(int sig, MR_Code *handler,
                    MR_bool need_info, const char * error_message);

// Reset a signal to its default handler. (see SIG_DFL in signal(2)).

extern  void    MR_reset_signal(int sig);

// As above, but initialize a signal action suitable to be passed to
// MR_set_signal_action rather than setting a signal handler for a signal.

extern  void    MR_init_signal_action(MR_signal_action* act, MR_Code *handler,
                    MR_bool need_info, MR_bool should_restart_system_call);

// Get the current action for the given signal.
// If the action cannot be retrieved, it aborts with the given error message.

extern  void    MR_get_signal_action(int sig, MR_signal_action *old_action,
                    const char *error_message);

// Restore the action for the given signal to the result
// of a previous call to MR_get_signal_action().
// If the action cannot be set, it aborts with the given error message.

extern  void    MR_set_signal_action(int sig, MR_signal_action *action,
                    const char *error_message);

// Change the behaviour of system calls when the specified signal
// is received. If restart is MR_TRUE, they will be restarted;
// if it is MR_FALSE, they won't. This function may have no effect
// on some systems.

extern  void    MR_signal_should_restart(int sig, MR_bool restart);

#endif // not MERCURY_SIGNAL_H
