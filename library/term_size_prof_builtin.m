%---------------------------------------------------------------------------%
% Copyright (C) 2003 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: term_size_prof.m.
% Author: zs.
% Stability: low.
%
% This file is automatically imported into every module when term size
% profiling is enabled. It contains support predicates used for term size
% profiling.
%
%---------------------------------------------------------------------------%

:- module term_size_prof_builtin.

:- interface.

	% measure_size(Term, Size): return the size of Term as Size.
	% The cost of the operation is independent of the size of Term;
	% if Term points to the heap, it looks up the size stored at the
	% starts of the cell.
:- pred measure_size(T::in, int::out) is det.

	% measure_size_acc(Term, Size0, Size): similar to measure_size,
	% but instead of returning just the size of term, it returns the
	% size plus Size0.
:- pred measure_size_acc(T::in, int::in, int::out) is det.

	% increment_size(Term, Incr): Term must be a term on the heap
	% that is not fully ground, and whose size slot's contents represents
	% the size of the term in its original binding state. When some of
	% Term's free arguments are bound, we must increment its recorded size
	% by Incr, the sum of the sizes of the terms bound to those arguments.
	% This is what increment_size does. It is impure because it updates
	% Term destructively.
:- impure pred increment_size(T::in, int::in) is det.

	% This function is exactly like int__plus, and is also implemented
	% as a builtin. It is duplicated in this module because only predicates
	% and functions in builtin modules like this one are immune to dead
	% procedure elimination.
:- func term_size_plus(int, int) = int.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.

:- pragma foreign_decl("C", "
#ifndef MR_TERM_SIZE_PROFILING_GUARD
#define MR_TERM_SIZE_PROFILING_GUARD

  #ifdef MR_RECORD_TERM_SIZES
    #include ""mercury_term_size.h""
  #endif /* MR_RECORD_TERM_SIZES */

#endif /* MR_TERM_SIZE_PROFILING_GUARD */
").

:- pragma foreign_proc("C",
	measure_size(Term::in, Size::out),
	[thread_safe, promise_pure, will_not_call_mercury],
"{
#ifdef MR_RECORD_TERM_SIZES
	MR_TypeInfo	type_info;

	type_info = (MR_TypeInfo) TypeInfo_for_T;
	Size = MR_term_size(type_info, Term);
  #ifdef MR_DEBUG_TERM_SIZES
	if (MR_heapdebug && MR_lld_print_enabled) {
		printf(""measure_size: %p -> %d\\n"",
			(void *) Term, (int) Size);
	}
  #endif
#else
	MR_fatal_error(""measure_size: term size profiling not enabled"");
#endif
}").

:- pragma foreign_proc("C",
	measure_size_acc(Term::in, Size0::in, Size::out),
	[thread_safe, promise_pure, will_not_call_mercury],
"{
#ifdef MR_RECORD_TERM_SIZES
	MR_TypeInfo	type_info;

	type_info = (MR_TypeInfo) TypeInfo_for_T;
	Size = MR_term_size(type_info, Term) + Size0;
  #ifdef MR_DEBUG_TERM_SIZES
	if (MR_heapdebug && MR_lld_print_enabled) {
		printf(""measure_size_acc: %p + %d -> %d\\n"",
			(void *) Term, (int) Size0, (int) Size);
	}
  #endif
#else
	MR_fatal_error(""measure_size_acc: term size profiling not enabled"");
#endif
}").

:- pragma foreign_proc("C",
	increment_size(Term::in, Incr::in),
	[thread_safe, will_not_call_mercury],
"{
#ifdef MR_RECORD_TERM_SIZES
  #ifdef MR_DEBUG_TERM_SIZES
	if (MR_heapdebug && MR_lld_print_enabled) {
		printf(""increment_size: %p + %d\\n"",
			(void *) Term, (int) Incr);
	}
  #endif
	MR_mask_field(Term, -1) += Incr;
#else
	MR_fatal_error(""increment_size: term size profiling not enabled"");
#endif
}").
