%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1998-1999,2002, 2004, 2006 The University of Melbourne.
% Copyright (C) 2018, 2022 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: scoped_update.m.
% Authors: pets (Peter Schachte).
% Stability: low.
% Purpose: Support for scoping of non-backtrackable changes.
%
% This module, together with `references.m', provide a way of implementing
% dynamically scoped global variables.
%
% Occasionally one wants to use impose some scoping on non-backtrackable
% changes to some memory. That is, one wants to implicitly give a specified
% memory location two possibly different values:  the value it has inside a
% certain scope, and the value it has outside.  Thus after a certain goal
% completes, one wants to reset a memory location to have the value it had on
% entry. However, if a subsequent goal should fail forcing re-entry of the
% scope, the value should be reset to what it was on leaving the scope.  When
% the scope is eventually left, whether by success or failure, the memory
% should again have its "outside the scope" value.
%
% This code implements this functionality. Ideally, this would be implemented
% as a higher-order predicate whose arguments are a memory location and a
% closure specifying the scope. Unfortunately, the closure would always be
% impure (since if it doesn't destructively change the memory location there is
% no point in using the scoping construct), and Mercury doesn't allow impure
% closures.
% (XXX that last bit is no longer true; Mercury does support impure closures
% and this module could be rewritten to use them.)
%
% Therefore, this is implemented as matching enter_update_scope and
% exit_update_scope predicates. Care must be taken to ensure that these are
% always called in pairs.
%
% Note that scoped update can be implemented for backtrackable references, or
% memory that is backtrackably updated, by simply getting the value before
% entering the scope and reseting it to that value on leaving the scope.
% Backtracking will take care of the rest automatically. It is only for
% non-backtrackable updates that this module becomes necessary.
%
% This module is implemented using the trailing features described in the
% "Trailing" section of the "Implementation-dependent extensions" chapter of
% the Mercury Language Reference Manual. This means that in order to use this
% module, you *must* compile with the --use-trail switch. The easiest way to do
% this is to include the line:
%
%     GRADEFLAGS=--use-trail
%
% in your Mmakefile.
%
%---------------------------------------------------------------------------%

:- module scoped_update.
:- interface.

:- type scoped_update_handle.

:- impure pred enter_scope(T::in, scoped_update_handle::muo) is det.

:- impure pred exit_scope(scoped_update_handle::mdi) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_decl("C", "
#include <stdio.h>

#include ""mercury_trail.h""

/*
**  To handle the scoping, we use a ME_ScopeHandle data structure, which
**  holds both the value inside and outside the scope.  Then we have
**  four functions to handle entering and leaving the scope both
**  forwards (on success) and backwards (on failure).  The user only
**  needs to think about the forwards versions; the backwards
**  functions are installed as function trail entries, and so are
**  automatically called at the right time.
*/

#ifndef ME_SCOPEHANDLE_DEFINED
#define ME_SCOPEHANDLE_DEFINED
typedef struct {
    MR_Word *var;
    MR_Word insideval;
    MR_Word outsideval;
} *ME_ScopeHandle;
#endif

void ME_enter_scope_failing(ME_ScopeHandle handle, MR_untrail_reason reason);
void ME_exit_scope_failing(ME_ScopeHandle handle, MR_untrail_reason reason);

#ifdef ME_DEBUG_SCOPE
  #define ME_show_handle(msg, handle)                           \
        printf(""%s <%5d, in: %5d, out: %5d\\n"", (msg),        \
            *(int *) (handle)->var,                             \
            (int) (handle)->insideval,                          \
            (int) (handle)->outsideval)
  #define ME_untrail_msg(msg)                                   \
        printf(msg)
#else
  #define ME_show_handle(msg, handle)
  #define ME_untrail_msg(msg)
#endif
").


:- pragma foreign_code("C", "

void
ME_enter_scope_failing(ME_ScopeHandle handle, MR_untrail_reason reason)
{
    switch (reason) {
        case MR_exception:
        case MR_undo:
        case MR_retry:
            ME_untrail_msg(
                ""ME_enter_scope_failing: exception/undo/retry\\n"");
            ME_show_handle(""=> fail back into scope.  old:  "", handle);
            handle->outsideval = *handle->var;
            *handle->var = handle->insideval;
            ME_show_handle(""=>                        new:  "", handle);
            break;

            default:
                ME_untrail_msg(""ME_enter_scope_failing: default\\n"");
    }
}

void
ME_exit_scope_failing(ME_ScopeHandle handle, MR_untrail_reason reason)
{
    switch (reason) {
        case MR_exception:
        case MR_undo:
        case MR_retry:
            ME_untrail_msg(""ME_exit_scope_failing: exception/undo/retry\\n"");
            ME_show_handle(""<= fail back out of scope.  old:  "", handle);
            *handle->var = handle->outsideval;
            ME_show_handle(""<=                          new:  "", handle);
            break;

        case MR_commit:
        case MR_solve:
            ME_untrail_msg(""ME_exit_scope_failing: commit/solve\\n"");
            // This *may* help GC collect more garbage.
            handle->var = (MR_Word *) 0;
            handle->outsideval = handle->insideval = (MR_Word) 0;
            break;

        default:
            ME_untrail_msg(""ME_exit_scope_failing: default\\n"");
            // We may need to do something if reason == MR_gc */
    }
}
").

:- type scoped_update_handle == c_pointer.

:- pragma foreign_proc("C",
    enter_scope(Ptr::in, Scoped_update_handle::muo),
    [will_not_call_mercury],
"
    MR_Word rec;
    ME_ScopeHandle handle;

    MR_incr_hp(rec, (sizeof(*handle) + sizeof(MR_Word) - 1) / sizeof(MR_Word));
    handle = (ME_ScopeHandle) rec;
    handle->var = (MR_Word *) Ptr;
    handle->insideval = handle->outsideval = *(MR_Word *) Ptr;
    MR_trail_function(ME_exit_scope_failing, handle);

    ME_show_handle("">> enter scope:  "", handle);

    Scoped_update_handle = (MR_Word) handle;
").

:- pragma foreign_proc("C",
    exit_scope(Handle::mdi),
    [will_not_call_mercury],
"
    ME_ScopeHandle handle = (ME_ScopeHandle) Handle;

    ME_show_handle(""<< exit scope.  old:  "", handle);
    handle->insideval = *handle->var;
    *handle->var = handle->outsideval;
    MR_trail_function(ME_enter_scope_failing, handle);
    ME_show_handle(""                new:  "", handle);
").

%---------------------------------------------------------------------------%
:- end_module scoped_update.
%---------------------------------------------------------------------------%
