%---------------------------------------------------------------------------%
% Copyright (C) 1999,2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: gc.m.
% Author: fjh.
% Stability: medium.
%
% This module defines some procedures for controlling the actions
% of the garbage collector.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module gc.
:- interface.
:- import_module io.

	% Force a garbage collection.
:- pred garbage_collect(io__state::di, io__state::uo) is det.

	% Force a garbage collection.
	% Note that this version is not really impure, but it needs to be
	% declared impure to ensure that the compiler won't try to
	% optimize it away.
:- impure pred garbage_collect is det.

%---------------------------------------------------------------------------%

:- implementation.

:- pragma promise_pure(garbage_collect/2).

garbage_collect -->
	{ impure garbage_collect }.

:- pragma no_inline(garbage_collect/0).

:- pragma foreign_proc("C", garbage_collect, [may_call_mercury], "
#ifdef MR_CONSERVATIVE_GC
  #ifndef MR_HIGHLEVEL_CODE
	/* clear out the stacks and registers before garbage collecting */
	MR_clear_zone_for_GC(MR_CONTEXT(MR_ctxt_detstack_zone), MR_sp + 1);
	MR_clear_zone_for_GC(MR_CONTEXT(MR_ctxt_nondetstack_zone),
		MR_maxfr + 1);
	MR_clear_regs_for_GC();
  #endif

	GC_gcollect();
#endif
").
:- pragma foreign_proc("MC++", garbage_collect, [will_not_call_mercury], "
	mercury::runtime::Errors::SORRY(""foreign code for this function"");
").

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
