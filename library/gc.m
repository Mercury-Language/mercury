%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1999,2001-2007 The University of Melbourne.
% Copyright (C) 2014-2015, 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: gc.m.
% Author: fjh.
% Stability: medium.
%
% This module defines some procedures for controlling the actions of the
% garbage collector.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module gc.
:- interface.

:- import_module io.

%---------------------------------------------------------------------------%

    % Force a garbage collection.
    %
:- pred garbage_collect(io.state::di, io.state::uo) is det.

    % Force a garbage collection.
    % Note that this version is not really impure, but it needs to be
    % declared impure to ensure that the compiler won't try to
    % optimize it away.
    %
:- impure pred garbage_collect is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- pragma promise_pure(pred(garbage_collect/2)).

garbage_collect(!IO) :-
    impure garbage_collect.

:- pragma no_inline(pred(garbage_collect/0)).

:- pragma foreign_export("C", garbage_collect, "ML_garbage_collect").

:- pragma foreign_proc("C",
    garbage_collect,
    [may_call_mercury, thread_safe, terminates],
"
#ifdef MR_CONSERVATIVE_GC
  #ifndef MR_HIGHLEVEL_CODE
    // clear out the stacks and registers before garbage collecting.
    MR_clear_zone_for_GC(MR_CONTEXT(MR_ctxt_detstack_zone), MR_sp + 1);
    MR_clear_zone_for_GC(MR_CONTEXT(MR_ctxt_nondetstack_zone),
        MR_maxfr + 1);
    MR_clear_regs_for_GC();
  #endif

    GC_gcollect();
#elif defined(MR_NATIVE_GC)
  #ifdef MR_HIGHLEVEL_CODE
    MR_garbage_collect();
  #else
    // XXX not yet implemented
  #endif
#endif
").

garbage_collect :-
    impure private_builtin.imp.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
