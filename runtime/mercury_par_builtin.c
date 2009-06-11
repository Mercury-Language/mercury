/*
vim: ft=c ts=4 sw=4 et
*/
/*
** Copyright (C) 2009 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

#include "mercury_types.h"
#include "mercury_par_builtin.h"

#ifdef MR_CONSERVATIVE_GC

void
MR_finalize_future(void *obj, void *cd)
{
    MR_Future *future;
    
    future = (MR_Future *) obj;

  #ifdef MR_THREAD_SAFE
    pthread_mutex_destroy(&(future->MR_fut_lock));
  #endif
}

#endif
