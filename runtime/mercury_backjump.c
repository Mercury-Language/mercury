// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2008 The University of Melbourne.
// This file may only be copied under the terms of the GNU Library General
// Public License - see the file COPYING.LIB in the Mercury distribution.

#include "mercury_backjump.h"
#include "mercury_thread.h"

#if defined(MR_THREAD_SAFE)
    #include    <pthread.h>
#endif

// The low-level C counterparts to these globals are the `backjump_handler'
// and `backjump_next_choice_id' fields of the MR_Context structure.
// (See mercury_context.h for details.)

#if defined(MR_HIGHLEVEL_CODE)
    #if defined(MR_THREAD_SAFE)

        MercuryThreadKey    MR_backjump_handler_key;
        MercuryThreadKey    MR_backjump_next_choice_id_key;

    #else // not MR_THREAD_SAFE

        MR_BackJumpHandler  *MR_backjump_handler;
        MR_BackJumpChoiceId MR_backjump_next_choice_id = 0;

    #endif // not MR_THREAD_SAFE
#endif // not MR_HIGHLEVEL_CODE

#if defined(MR_HIGHLEVEL_CODE) && defined(MR_THREAD_SAFE)

MR_BackJumpChoiceId
MR_get_tl_backjump_next_choice_id(void)
{
    MR_BackJumpChoiceId    new_choice_id;

    // NOTE: this only works because sizeof(MR_Integer) == sizeof(void *).
    new_choice_id =
        (MR_Integer) MR_GETSPECIFIC(MR_backjump_next_choice_id_key);
    pthread_setspecific(MR_backjump_next_choice_id_key,
        (void *)(new_choice_id + 1));
    return new_choice_id;
}

#endif // MR_HIGLEVEL_CODE && MR_THREAD_SAFE

////////////////////////////////////////////////////////////////////////////
