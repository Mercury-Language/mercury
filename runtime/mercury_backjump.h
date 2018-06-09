// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2008, 2011 The University of Melbourne.
// Copyright (C) 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// mercury_backjump.h - code for handling backjumping.

#ifndef MERCURY_BACKJUMP_H
#define MERCURY_BACKJUMP_H

#include "mercury_conf.h"
#include "mercury_thread.h"
#include <setjmp.h>

typedef MR_Integer  MR_BackJumpChoiceId;

typedef struct MR_BackJumpHandler_Struct {
    struct MR_BackJumpHandler_Struct    *MR_bjh_prev;
    MR_BackJumpChoiceId                 MR_bjh_id;

#ifdef MR_HIGHLEVEL_CODE
    jmp_buf                             MR_bjh_handler;
#else // !MR_HIGHLEVEL_CODE
    MR_Word                             *MR_bjh_saved_sp;
    MR_Word                             *MR_bjh_saved_fr;
#endif // !MR_HIGHLEVEL_CODE
} MR_BackJumpHandler;

////////////////////////////////////////////////////////////////////////////

// This section defines the global state required to support backjumping.
// In low-level C grades this state is part of the MR_Context structure
// (see mercury_context.h). In non .par high-level C grades we store the
// state in global variables and in .par high-level C grades we need to use
// thread-local data.

#if defined(MR_HIGHLEVEL_CODE)

    #if defined(MR_THREAD_SAFE)

        extern MR_BackJumpChoiceId
        MR_get_tl_backjump_next_choice_id(void);

        // MR_backjump_handler_key stores a key that can be used to get
        // the current backjump handler for the current thread.
        // MR_backjump_next_choice_id_key stores a key that can be used
        // to get the next available backjump choice id for the current thread.
        // NOTE: changes here may need to be reflected in the function
        // MR_init_context_stuff() in mercury_context.c.

        extern MercuryThreadKey MR_backjump_handler_key;
        extern MercuryThreadKey MR_backjump_next_choice_id_key;

        #define MR_GET_BACKJUMP_HANDLER()                               \
            MR_GETSPECIFIC(MR_backjump_handler_key)

        #define MR_SET_BACKJUMP_HANDLER(val)                            \
            pthread_setspecific(MR_backjump_handler_key, (val))

        #define MR_GET_NEXT_CHOICE_ID()                                 \
            MR_get_tl_backjump_next_choice_id()

    #else // not MR_THREAD_SAFE

        // MR_backjump_handler points to the current backjump handler.

        extern MR_BackJumpHandler   *MR_backjump_handler;

        // The value MR_backjump_next_choice_id is the next available
        // backjump choice id.

        extern MR_BackJumpChoiceId  MR_backjump_next_choice_id;

        #define MR_GET_BACKJUMP_HANDLER()      MR_backjump_handler
        #define MR_SET_BACKJUMP_HANDLER(val)   MR_backjump_handler = (val)
        #define MR_GET_NEXT_CHOICE_ID()        (MR_backjump_next_choice_id++)

    #endif // not MR_THREAD_SAFE

#else // not MR_HIGHLEVEL_CODE

    #define MR_GET_BACKJUMP_HANDLER()                                   \
        MR_ENGINE(MR_eng_this_context)->MR_ctxt_backjump_handler

    #define MR_SET_BACKJUMP_HANDLER(val)                                \
        MR_ENGINE(MR_eng_this_context)->MR_ctxt_backjump_handler = (val)

    #define MR_GET_NEXT_CHOICE_ID()                                     \
        (MR_ENGINE(MR_eng_this_context)->MR_ctxt_backjump_next_choice_id++)

#endif // not MR_HIGHLEVEL_CODE

#endif // not MERCURY_BACKJUMP_H
