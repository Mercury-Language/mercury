%-----------------------------------------------------------------------------%
% Copyright (C) 1997-1998 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: exception.m.
% Main author: fjh.
% Stability: low

% This file contains experimental code for exception handling.

% Note that throwing an exception across the C interface won't work.
% That is, if a Mercury procedure that is exported to C using `pragma export'
% throws an exception which is not caught within that procedure, then
% you will get undefined behaviour.

%-----------------------------------------------------------------------------%

% To compile this module you need the following two lines in your Mmakefile:
%
%	RM_C=:
%	C2INITFLAGS=--extra-inits
%
% You also need to add dependencies to ensure that Mmake knows that
% the *_init.c files depend on the *.c files.
%
% This ensures that the module initialization code for this module will be run.
% (Actually these steps are needed only in certain grades, e.g. for profiling.)

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
:- module exception.
:- interface.
:- import_module std_util, list, io.

%
% throw(Exception):
%	Throw the specified exception.
%
:- pred throw(T).
:- mode throw(in) is erroneous.

:- func throw(T) = _.
:- mode throw(in) = out is erroneous.

% The following types are used by try/3 and try/5.

:- type exception_result(T)
	--->	succeeded(T)
	;	failed
	;	exception(univ).

:- inst cannot_fail
	--->	succeeded(ground)
	;	exception(ground).

%
% try(Goal, Result):
%    Operational semantics:
%	Call Goal(R).
%	If Goal(R) fails, succeed with Result = failed.
%	If Goal(R) succeeds, succeed with Result = succeeded(R).
%	If Goal(R) throws an exception E, succeed with Result = exception(E).
%    Declarative semantics:
%       try(Goal, Result) <=>
%               ( Goal(R), Result = succeeded(R)
%               ; not Goal(_), Result = failed
%               ; Result = exception(_)
%		).
%
:- pred try(pred(T),		    exception_result(T)).
:- mode try(pred(out) is det,       out(cannot_fail)) is cc_multi.
:- mode try(pred(out) is semidet,   out)              is cc_multi.
:- mode try(pred(out) is cc_multi,  out(cannot_fail)) is cc_multi.
:- mode try(pred(out) is cc_nondet, out)              is cc_multi.

%
% try_io(Goal, Result, IO_0, IO):
%    Operational semantics:
%	Call Goal(R, IO_0, IO_1).
%	If it succeeds, succeed with Result = succeeded(R) and IO = IO_1.
%	If it throws an exception E, succeed with Result = exception(E)
%	and with the final IO state being whatever state resulted
%	from the partial computation from IO_0.
%    Declarative semantics:
%	try_io(Goal, Result, IO_0, IO) <=>
%		( Goal(R, IO_0, IO), Result = succeeded(R)
%		; Result = exception(_)
%		).
%
:- pred try_io(pred(T, io__state, io__state),
		exception_result(T), io__state, io__state).
:- mode try_io(pred(out, di, uo) is det,     
		out(cannot_fail), di, uo) is cc_multi.
:- mode try_io(pred(out, di, uo) is cc_multi,
		out(cannot_fail), di, uo) is cc_multi.

%
% try_all(Goal, ResultList):
%    Operational semantics:
%	Try to find all solutions to Goal(R), using backtracking.
%	Collect the solutions found in the ResultList, until
%	the goal either throws an exception or fails.
%	If it throws an exception, put that exception at the end of
%	the ResultList.
%    Declaratively:
%       try_all(Goal, ResultList) <=>
%		(if
%			list__reverse(ResultList, [Last | AllButLast]),
%			Last = exception(_)
%		then
%			all [M] (list__member(M, AllButLast) =>
%				(M = succeeded(R), Goal(R))),
%		else
%			all [M] (list__member(M, ResultList) =>
%				(M = succeeded(R), Goal(R))),
%			all [R] (Goal(R) =>
%				list__member(succeeded(R), ResultList)),
%		).

:- pred try_all(pred(T), list(exception_result(T))).
:- mode try_all(pred(out) is det,     out(try_all_det))     is cc_multi.
:- mode try_all(pred(out) is semidet, out(try_all_semidet)) is cc_multi.
:- mode try_all(pred(out) is multi,   out(try_all_multi))   is cc_multi.
:- mode try_all(pred(out) is nondet,  out(try_all_nondet))  is cc_multi.

:- inst [] ---> [].
:- inst try_all_det ---> [cannot_fail].
:- inst try_all_semidet ---> [] ; [cannot_fail].
:- inst try_all_multi ---> [cannot_fail | try_all_nondet].
:- inst try_all_nondet == list_skel(cannot_fail).

%
% incremental_try_all(Goal, AccumulatorPred, Acc0, Acc):
%    Same as
%	try_all(Goal, Results),
%	std_util__unsorted_aggregate(Results, AccumulatorPred, Acc0, Acc)
%    except that operationally, the execution of try_all
%    and std_util__unsorted_aggregate is interleaved.

:- pred incremental_try_all(pred(T), pred(exception_result(T), A, A), A, A).
:- mode incremental_try_all(pred(out) is nondet, 
		pred(in, di, uo) is det, di, uo) is cc_multi.
:- mode incremental_try_all(pred(out) is nondet, 
		pred(in, in, out) is det, in, out) is cc_multi.

%
% rethrow(ExceptionResult):
%	Rethrows the specified exception result
%	(which should be of the form `exception(_)',
%	not `succeeded(_)' or `failed'.).
%
:- pred rethrow(exception_result(T)).
:- mode rethrow(in(bound(exception(ground)))) is erroneous.

:- func rethrow(exception_result(T)) = _.
:- mode rethrow(in(bound(exception(ground)))) = out is erroneous.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module require.

:- pred try(determinism,      	  pred(T),		  exception_result(T)).
:- mode try(in(bound(det)),	  pred(out) is det,       out(cannot_fail))
							       is cc_multi.
:- mode try(in(bound(semidet)),	  pred(out) is semidet,   out) is cc_multi.
:- mode try(in(bound(cc_multi)),  pred(out) is cc_multi,  out(cannot_fail))
							       is cc_multi.
:- mode try(in(bound(cc_nondet)), pred(out) is cc_nondet, out) is cc_multi.

:- pred try_io(determinism, 	    pred(T, io__state, io__state),
				    exception_result(T), io__state, io__state).
:- mode try_io(in(bound(det)),      pred(out, di, uo) is det,
				    out(cannot_fail), di, uo) is cc_multi.
:- mode try_io(in(bound(cc_multi)), pred(out, di, uo) is cc_multi,
				    out(cannot_fail), di, uo) is cc_multi.

:- pred try_all(determinism,        pred(T), list(exception_result(T))).
:- mode try_all(in(bound(det)),	    pred(out) is det, 
				    	     out(try_all_det)) is cc_multi.
:- mode try_all(in(bound(semidet)), pred(out) is semidet,  
				    	     out(try_all_semidet)) is cc_multi.
:- mode try_all(in(bound(multi)),   pred(out) is multi, 
				    	     out(try_all_multi)) is cc_multi.
:- mode try_all(in(bound(nondet)),  pred(out) is nondet,
				    	     out(try_all_nondet)) is cc_multi.

% The functors in this type must be in the same order as the
% enumeration constants in the C enum `ME_Determinism' defined below.
:- type determinism
	--->	det
	;	semidet
	;	cc_multi
	;	cc_nondet
	;	multi
	;	nondet
	;	erroneous
	;	failure.

:- pred get_determinism(pred(T), determinism).
:- mode get_determinism(pred(out) is det,     out(bound(det)))     is cc_multi.
:- mode get_determinism(pred(out) is semidet, out(bound(semidet))) is cc_multi.
:- mode get_determinism(pred(out) is multi, out(bound(multi)))     is cc_multi.
:- mode get_determinism(pred(out) is nondet, out(bound(nondet)))   is cc_multi.
:- mode get_determinism(pred(out) is cc_multi, out(bound(cc_multi)))
								  is cc_multi.
:- mode get_determinism(pred(out) is cc_nondet, out(bound(cc_nondet)))
								  is cc_multi.

:- pred get_determinism_2(pred(T, io__state, io__state), determinism).
:- mode get_determinism_2(pred(out, di, uo) is det,      out(bound(det)))
	is cc_multi.
:- mode get_determinism_2(pred(out, di, uo) is cc_multi, out(bound(cc_multi)))
	is cc_multi.

% Unfortunately the only way to implement get_determinism/2 is to use
% the C interface, since Mercury doesn't allow different code for different
% modes.

% The enumeration constants in this enum must be in the same order as the
% functors in the Mercury type `determinism' defined above.
:- pragma c_header_code("
	typedef enum {
		ME_DET,
		ME_SEMIDET,
		ME_CC_MULTI,
		ME_CC_NONDET,
		ME_MULTI,
		ME_NONDET,
		ME_ERRONEOUS,
		ME_FAILURE
	} ME_Determinism;
").

:- pragma c_code(
	get_determinism(_Pred::pred(out) is det,
			Det::out(bound(det))),
	will_not_call_mercury,
	"Det = ME_DET"
).
:- pragma c_code(
	get_determinism(_Pred::pred(out) is semidet,
			Det::out(bound(semidet))),
	will_not_call_mercury,
	"Det = ME_SEMIDET"
).
:- pragma c_code(
	get_determinism(_Pred::pred(out) is cc_multi,
			Det::out(bound(cc_multi))),
	will_not_call_mercury,
	"Det = ME_CC_MULTI"
).
:- pragma c_code(
	get_determinism(_Pred::pred(out) is cc_nondet,
			Det::out(bound(cc_nondet))),
	will_not_call_mercury,
	"Det = ME_CC_NONDET"
).
:- pragma c_code(
	get_determinism(_Pred::pred(out) is multi,
			Det::out(bound(multi))),
	will_not_call_mercury,
	"Det = ME_MULTI"
).
:- pragma c_code(
	get_determinism(_Pred::pred(out) is nondet,
			Det::out(bound(nondet))),
	will_not_call_mercury,
	"Det = ME_NONDET"
).

:- pragma c_code(
	get_determinism_2(_Pred::pred(out, di, uo) is det,
			Det::out(bound(det))),
	will_not_call_mercury,
	"Det = ME_DET"
).

:- pragma c_code(
	get_determinism_2(_Pred::pred(out, di, uo) is cc_multi,
			Det::out(bound(cc_multi))),
	will_not_call_mercury,
	"Det = ME_CC_MULTI"
).

throw(Exception) :-
	type_to_univ(Exception, Univ),
	builtin_throw(Univ).

throw(Exception) = _ :-
	throw(Exception).

rethrow(exception(Univ)) :-
	builtin_throw(Univ).

rethrow(ExceptionResult) = _ :-
	rethrow(ExceptionResult).

:- pred wrap_success(pred(T), exception_result(T)) is det.
:- mode wrap_success(pred(out) is det, out) is det.
:- mode wrap_success(pred(out) is semidet, out) is semidet.
:- mode wrap_success(pred(out) is multi, out) is multi.
:- mode wrap_success(pred(out) is nondet, out) is nondet.
:- mode wrap_success(pred(out) is cc_multi, out) is cc_multi.
:- mode wrap_success(pred(out) is cc_nondet, out) is cc_nondet.
wrap_success(Goal, succeeded(R)) :- Goal(R).

:- pred wrap_success_or_failure(pred(T), exception_result(T)) is det.
:- mode wrap_success_or_failure(pred(out) is det, out) is det.
:- mode wrap_success_or_failure(pred(out) is semidet, out) is det.
:- mode wrap_success_or_failure(pred(out) is multi, out) is multi.
:- mode wrap_success_or_failure(pred(out) is nondet, out) is multi.
:- mode wrap_success_or_failure(pred(out) is cc_multi, out) is cc_multi.
:- mode wrap_success_or_failure(pred(out) is cc_nondet, out) is cc_multi.
wrap_success_or_failure(Goal, Result) :-
	(if Goal(R) then Result = succeeded(R) else Result = failed).

/*********************
% This doesn't work, due to 
% 	bash$ mmc exception.m
% 	Software error: sorry, not implemented: taking address of pred
% 	`wrap_success_or_failure/2' with multiple modes.
% Instead, we need to switch on the Detism argument.

try(_Detism, Goal, Result) :-
	builtin_catch(wrap_success_or_failure(Goal), wrap_exception, Result).
*********************/

try(Goal, Result) :-
	get_determinism(Goal, Detism),
	try(Detism, Goal, Result).

try(det, Goal, Result) :-
	builtin_catch((pred(R::out) is det :-
				wrap_success_or_failure(Goal, R)),
		wrap_exception, Result0),
	cc_multi_equal(Result0, Result).
try(semidet, Goal, Result) :-
	builtin_catch((pred(R::out) is det :-
				wrap_success_or_failure(Goal, R)),
		wrap_exception, Result0),
	cc_multi_equal(Result0, Result).
try(cc_multi, Goal, Result) :-

	builtin_catch(
		(pred(R::out) is cc_multi :-
				wrap_success_or_failure(Goal, R)
				),
		wrap_exception, Result).
try(cc_nondet, Goal, Result) :-
	builtin_catch((pred(R::out) is cc_multi :-
				wrap_success_or_failure(Goal, R)),
		wrap_exception, Result).


/**********
% This doesn't work, due to 
% 	bash$ mmc exception.m
% 	Software error: sorry, not implemented: taking address of pred
% 	`wrap_success_or_failure/2' with multiple modes.
% Instead, we need to switch on the Detism argument.

try_all(Goal, ResultList) :-
	unsorted_solutions(builtin_catch(wrap_success(Goal), wrap_exception),
		ResultList).
**********/

try_all(Goal, ResultList) :-
	get_determinism(Goal, Detism),
	try_all(Detism, Goal, ResultList).

try_all(det, Goal, [Result]) :-
	try(det, Goal, Result).
try_all(semidet, Goal, ResultList) :-
	try(semidet, Goal, Result),
	( Result = failed, ResultList = []
	; Result = succeeded(_), ResultList = [Result]
	; Result = exception(_), ResultList = [Result]
	).
try_all(multi, Goal, ResultList) :-
	unsorted_solutions((pred(Result::out) is multi :-
		builtin_catch((pred(R::out) is multi :-
				wrap_success(Goal, R)),
			wrap_exception, Result)),
		ResultList).
try_all(nondet, Goal, ResultList) :-
	unsorted_solutions((pred(Result::out) is nondet :-
		builtin_catch((pred(R::out) is nondet :-
				wrap_success(Goal, R)),
			wrap_exception, Result)),
		ResultList).

incremental_try_all(Goal, AccPred, Acc0, Acc) :-
	unsorted_aggregate((pred(Result::out) is nondet :-
		builtin_catch((pred(R::out) is nondet :-
				wrap_success(Goal, R)),
			wrap_exception, Result)),
		AccPred, Acc0, Acc).

try_io(IO_Goal, Result) -->
	{ get_determinism_2(IO_Goal, Detism) },
	try_io(Detism, IO_Goal, Result).

% We'd better not inline try_io/5, since it uses a horrible hack
% with unsafe_perform_io (see below) that might confuse the compiler.
:- pragma no_inline(try_io/5).
try_io(det, IO_Goal, Result) -->
	{ Goal = (pred(R::out) is det :-
		very_unsafe_perform_io(IO_Goal, R)) },
	{ try(det, Goal, Result) }.
try_io(cc_multi, IO_Goal, Result) -->
	{ Goal = (pred(R::out) is cc_multi :-
		very_unsafe_perform_io(IO_Goal, R)) },
	{ try(cc_multi, Goal, Result) }.

:- pred very_unsafe_perform_io(pred(T, io__state, io__state), T).
:- mode very_unsafe_perform_io(pred(out, di, uo) is det, out) is det.
:- mode very_unsafe_perform_io(pred(out, di, uo) is cc_multi, out)
								is det.
% Mercury doesn't support impure higher-order pred terms, so if we want
% to form a closure from unsafe_perform_io, as we need to do above,
% then we must (falsely!) promise that it is pure.
:- pragma promise_pure(very_unsafe_perform_io/2). % XXX this is a lie

very_unsafe_perform_io(Goal, Result) :-
	impure unsafe_perform_io(Goal, Result).

:- pred wrap_exception(univ::in, exception_result(T)::out) is det.
wrap_exception(Exception, exception(Exception)).

%-----------------------------------------------------------------------------%

:- pred builtin_throw(univ).
:- mode builtin_throw(in) is erroneous.

:- type handler(T) == pred(univ, T).
:- inst handler == (pred(in, out) is det).

%
% builtin_catch/3 is actually impure.  But we don't declare it as impure,
% because the code for try_all/3 takes its address (to pass to
% unsorted_solutions/2), and Mercury does not (yet?) support
% impure higher-order pred terms.
%
:- /* impure */
   pred builtin_catch(pred(T), handler(T), T).
:- mode builtin_catch(pred(out) is det,       in(handler), out) is det.
:- mode builtin_catch(pred(out) is semidet,   in(handler), out) is semidet.
:- mode builtin_catch(pred(out) is cc_multi,  in(handler), out) is cc_multi.
:- mode builtin_catch(pred(out) is cc_nondet, in(handler), out) is cc_nondet.
:- mode builtin_catch(pred(out) is multi,     in(handler), out) is multi.
:- mode builtin_catch(pred(out) is nondet,    in(handler), out) is nondet.

% builtin_throw and builtin_catch are implemented below using
% hand-coded low-level C code.

:- external(builtin_throw/1).
:- external(builtin_catch/3).

%-----------------------------------------------------------------------------%

:- pragma c_header_code("
	#include <assert.h>
	#include ""mercury_deep_copy.h""

	MR_DECLARE_STRUCT(mercury_data_std_util__base_type_info_univ_0);
").

:- pragma c_code("

enum CodeModel { MODEL_DET, MODEL_SEMI, MODEL_NON };

/* swap the heap with the solutions heap */
#define swap_heaps()							\\
{									\\
	/* save the current heap */					\\
	Word *swap_heaps_temp_hp = hp;					\\
	MemoryZone *swap_heaps_temp_hp_zone = MR_heap_zone;		\\
									\\
	/* set heap to solutions heap */				\\
	MR_hp = MR_sol_hp;						\\
	MR_heap_zone = MR_solutions_heap_zone;				\\
									\\
	/* set the solutions heap to be the old heap */			\\
	MR_sol_hp = swap_heaps_temp_hp;					\\
	MR_solutions_heap_zone = swap_heaps_temp_hp_zone;		\\
}

/*
** Define a struct for the framevars that we use in an exception handler
** nondet stack frame.  This struct gets allocated on the nondet stack
** using mkpragmaframe().
*/
typedef struct Exception_Handler_Frame_struct {
	Word code_model;
	Word handler;
	Word *stack_ptr;
#ifdef MR_USE_TRAIL
	Word trail_ptr;
	Word ticket_counter;
#endif
#ifndef CONSERVATIVE_GC
	Word heap_ptr;
	Word solns_heap_ptr;
	Word heap_zone;
#endif
} Exception_Handler_Frame;

#define FRAMEVARS \
	(((Exception_Handler_Frame *) (curfr - NONDET_FIXED_SIZE)) - 1)

Define_extern_entry(mercury__exception__builtin_catch_3_0); /* det */
Define_extern_entry(mercury__exception__builtin_catch_3_1); /* semidet */
Define_extern_entry(mercury__exception__builtin_catch_3_2); /* cc_multi */
Define_extern_entry(mercury__exception__builtin_catch_3_3); /* cc_nondet */
Define_extern_entry(mercury__exception__builtin_catch_3_4); /* multi */
Define_extern_entry(mercury__exception__builtin_catch_3_5); /* nondet */

Define_extern_entry(mercury__exception__builtin_throw_1_0);

Define_extern_entry(exception_handler_do_fail);

/* the following are defined in runtime/mercury_ho_call.c */
Declare_entry(do_call_det_closure);
Declare_entry(do_call_semidet_closure);
Declare_entry(do_call_nondet_closure);

Declare_label(mercury__exception__builtin_catch_3_2_i1);
Declare_label(mercury__exception__builtin_catch_3_2_i2);
Declare_label(mercury__exception__builtin_catch_3_3_i1);
Declare_label(mercury__exception__builtin_catch_3_3_i2);
Declare_label(mercury__exception__builtin_catch_3_5_i1);
Declare_label(mercury__exception__builtin_catch_3_5_i2);
#ifdef MR_USE_TRAIL
  Declare_label(mercury__exception__builtin_catch_3_5_i3);
#endif
#ifndef COMPACT_ARGS
  Declare_label(mercury__exception__builtin_throw_1_0_i1);
  Declare_label(mercury__exception__builtin_throw_1_0_i2);
  Declare_label(mercury__exception__builtin_throw_1_0_i3);
#endif

MR_MAKE_STACK_LAYOUT_ENTRY(mercury__exception__builtin_catch_3_0)
MR_MAKE_STACK_LAYOUT_ENTRY(mercury__exception__builtin_catch_3_1)
MR_MAKE_STACK_LAYOUT_ENTRY(mercury__exception__builtin_catch_3_2)
MR_MAKE_STACK_LAYOUT_ENTRY(mercury__exception__builtin_catch_3_3)
MR_MAKE_STACK_LAYOUT_ENTRY(mercury__exception__builtin_catch_3_4)
MR_MAKE_STACK_LAYOUT_ENTRY(mercury__exception__builtin_catch_3_5)
MR_MAKE_STACK_LAYOUT_ENTRY(mercury__exception__builtin_throw_1_0)
MR_MAKE_STACK_LAYOUT_ENTRY(exception_handler_do_fail)
MR_MAKE_STACK_LAYOUT_ENTRY(mercury__exception__builtin_catch_3_0)

MR_MAKE_STACK_LAYOUT_INTERNAL(mercury__builtin_catch_3_2, 1)
MR_MAKE_STACK_LAYOUT_INTERNAL(mercury__builtin_catch_3_2, 2)
MR_MAKE_STACK_LAYOUT_INTERNAL(mercury__builtin_catch_3_3, 1)
MR_MAKE_STACK_LAYOUT_INTERNAL(mercury__builtin_catch_3_3, 2)
MR_MAKE_STACK_LAYOUT_INTERNAL(mercury__builtin_catch_3_5, 1)
MR_MAKE_STACK_LAYOUT_INTERNAL(mercury__builtin_catch_3_5, 2)
#ifdef MR_USE_TRAIL
  MR_MAKE_STACK_LAYOUT_INTERNAL(mercury__builtin_catch_3_5, 3)
#endif
#ifndef COMPACT_ARGS
  MR_MAKE_STACK_LAYOUT_INTERNAL(mercury__builtin_throw_1_0, 1)
  MR_MAKE_STACK_LAYOUT_INTERNAL(mercury__builtin_throw_1_0, 2)
  MR_MAKE_STACK_LAYOUT_INTERNAL(mercury__builtin_throw_1_0, 3)
#endif

BEGIN_MODULE(exceptions_module)
	init_entry(mercury__exception__builtin_catch_3_0);
	init_entry(mercury__exception__builtin_catch_3_1);
	init_entry(mercury__exception__builtin_catch_3_2);
	init_entry(mercury__exception__builtin_catch_3_3);
	init_entry(mercury__exception__builtin_catch_3_4);
	init_entry(mercury__exception__builtin_catch_3_5);
	init_label(mercury__exception__builtin_catch_3_2_i1);
	init_label(mercury__exception__builtin_catch_3_2_i2);
	init_label(mercury__exception__builtin_catch_3_3_i1);
	init_label(mercury__exception__builtin_catch_3_3_i2);
	init_label(mercury__exception__builtin_catch_3_5_i1);
	init_label(mercury__exception__builtin_catch_3_5_i2);
#ifdef MR_USE_TRAIL
	init_label(mercury__exception__builtin_catch_3_5_i3);
#endif
	init_entry(mercury__exception__builtin_throw_1_0);
#ifndef COMPACT_ARGS
	init_label(mercury__exception__builtin_throw_1_0_i1);
	init_label(mercury__exception__builtin_throw_1_0_i2);
	init_label(mercury__exception__builtin_throw_1_0_i3);
#endif
	init_entry(exception_handler_do_fail);
BEGIN_CODE

/*
** builtin_catch(Goal, Handler, Result)
**	call Goal(R).
**	if succeeds, set Result = R.
**	if throws an exception, call Handler(Result).
**
** This is the model_det version.
** On entry, we have a type_info (which we don't use) in r1,
** the Goal to execute in r2 and the Handler in r3.
** On exit, we should put Result in r1 (with COMPACT_ARGS) or r4.
*/
Define_entry(mercury__exception__builtin_catch_3_0); /* det */
#ifdef PROFILE_CALLS
{
	tailcall(ENTRY(mercury__exception__builtin_catch_3_2), 
		ENTRY(mercury__exception__builtin_catch_3_0));
}
#endif
Define_entry(mercury__exception__builtin_catch_3_2); /* cc_multi */
	/*
	** Create a handler on the stack with the special redoip
	** of `exception_handler_do_fail' (we'll look for this redoip
	** when unwinding the nondet stack in builtin_throw/1),
	** and save the stuff we will need if an exception is thrown.
	*/
	mkpragmaframe(""builtin_catch/3 [model_det]"", 0,
		Exception_Handler_Frame_struct,
		ENTRY(exception_handler_do_fail));
	FRAMEVARS->code_model = MODEL_DET;
	FRAMEVARS->handler = r3;		/* save the Handler closure */
	FRAMEVARS->stack_ptr = MR_sp;	/* save the det stack pointer */
#ifndef CONSERVATIVE_GC
	/* save the heap and solutions heap pointers */
	FRAMEVARS->heap_ptr = MR_hp;
	FRAMEVARS->solns_heap_ptr = MR_sol_hp;
	FRAMEVARS->heap_zone = MR_heap_zone;
#endif
#ifdef MR_USE_TRAIL
	/* save the trail state */
	MR_mark_ticket_stack(FRAMEVARS->ticket_counter);
	MR_store_ticket(FRAMEVARS->trail_ptr);
#endif

	/*
	** Now we need to create another frame.
	** This is so that we can be sure that no-one will hijack
	** the redoip of the special frame we created above.
	** (The compiler sometimes generates ``hijacking'' code that saves
	** the topmost redoip on the stack, and temporarily replaces it
	** with a new redoip that will do some processing on failure
	** before restoring the original redoip.  This would cause
	** problems when doing stack unwinding in builtin_throw/1,
	** because we wouldn't be able to find the special redoip.
	** But code will only ever hijack the topmost frame, so we
	** can avoid this by creating a second frame above the special
	** frame.)
	*/
	succip = LABEL(mercury__exception__builtin_catch_3_2_i1);
	mkframe(""builtin_catch_2/1 [model_det]"", 0, ENTRY(do_fail));

	/*
	** Now call `Goal(Result)'.
	*/
	r1 = r2;	/* The Goal to call */
	r2 = 0;		/* Zero additional input arguments */
	r3 = 1;		/* One output argument */
	call(ENTRY(do_call_det_closure), 
		LABEL(mercury__exception__builtin_catch_3_2_i2),
		ENTRY(mercury__exception__builtin_catch_3_2));
		
Define_label(mercury__exception__builtin_catch_3_2_i2);
	update_prof_current_proc(LABEL(mercury__exception__builtin_catch_3_2));
	/*
	** On exit from do_call_det_closure, Result is in r1
	*/
#ifndef COMPACT_ARGS
	r4 = r1;
#endif
#ifdef MR_USE_TRAIL
	MR_discard_ticket();
#endif
	succeed_discard();

Define_label(mercury__exception__builtin_catch_3_2_i1);
	succeed_discard();

/*
** builtin_catch(Goal, Handler, Result)
**	call Goal(R).
**	if succeeds, set Result = R.
**	if fails, fail.
**	if throws an exception, call Handler(Result).
**
** This is the model_semi version.
** With COMPACT_ARGS,
** on entry, we have a type_info (which we don't use) in r1,
** the Goal to execute in r2 and the Handler in r3,
** and on exit, we should put Result in r2.
** Without COMPACT_ARGS,
** on entry, we have a type_info (which we don't use) in r2,
** the Goal to execute in r3 and the Handler in r4,
** and on exit, we should put Result in r5.
*/
Define_entry(mercury__exception__builtin_catch_3_1); /* semidet */
#ifdef PROFILE_CALLS
{
	tailcall(ENTRY(mercury__exception__builtin_catch_3_3), 
		ENTRY(mercury__exception__builtin_catch_3_1));
}
#endif
Define_entry(mercury__exception__builtin_catch_3_3); /* cc_nondet */
	/*
	** Create a handler on the stack with the special redoip
	** of `exception_handler_do_fail' (we'll look for this redoip
	** when unwinding the nondet stack in builtin_throw/1),
	** and save the stuff we will need if an exception is thrown.
	*/
	mkpragmaframe(""builtin_catch/3 [model_semi]"", 0,
		Exception_Handler_Frame_struct,
		ENTRY(exception_handler_do_fail));
	FRAMEVARS->code_model = MODEL_SEMI;
#ifdef COMPACT_ARGS
	FRAMEVARS->handler = r3;	/* save the Handler closure */
#else
	FRAMEVARS->handler = r4;	/* save the Handler closure */
#endif
	FRAMEVARS->stack_ptr = MR_sp;	/* save the det stack pointer */
#ifndef CONSERVATIVE_GC
	/* save the heap and solutions heap pointers */
	FRAMEVARS->heap_ptr = MR_hp;
	FRAMEVARS->solns_heap_ptr = MR_sol_hp;
	FRAMEVARS->heap_zone = MR_heap_zone;
#endif
#ifdef MR_USE_TRAIL
	/* save the trail state */
	MR_mark_ticket_stack(FRAMEVARS->ticket_counter);
	MR_store_ticket(FRAMEVARS->trail_ptr);
#endif


	/*
	** Now we need to create another frame.
	** This is so that we can be sure that no-one will hijack
	** the redoip of the special frame we created above.
	** (The compiler sometimes generates ``hijacking'' code that saves
	** the topmost redoip on the stack, and temporarily replaces it
	** with a new redoip that will do some processing on failure
	** before restoring the original redoip.  This would cause
	** problems when doing stack unwinding in builtin_throw/1,
	** because we wouldn't be able to find the special redoip.
	** But code will only ever hijacks the topmost frame, so we
	** can avoid this by creating a second frame above the special
	** frame.)
	*/
	succip = LABEL(mercury__exception__builtin_catch_3_3_i1);
	mkframe(""builtin_catch_2/1 [model_semi]"", 0, ENTRY(do_fail));

	/*
	** Now call `Goal(Result)'.
	*/
#ifdef COMPACT_ARGS
	r1 = r2;	/* The Goal to call */
#else
	r1 = r3;	/* The Goal to call */
#endif
	r2 = 0;		/* Zero additional input arguments */
	r3 = 1;		/* One output argument */
	call(ENTRY(do_call_semidet_closure), 
		LABEL(mercury__exception__builtin_catch_3_3_i2),
		ENTRY(mercury__exception__builtin_catch_3_3));
		
Define_label(mercury__exception__builtin_catch_3_3_i2);
	update_prof_current_proc(LABEL(mercury__exception__builtin_catch_3_3));
	/*
	** On exit from do_call_semidet_closure, the success/failure
	** indicator is in r1, and Result is in r2.
	** Note that we call succeed_discard() to exit regardless
	** of whether r1 is true or false.  We just return the r1 value
	** back to our caller.
	*/
#ifndef COMPACT_ARGS
	r5 = r2;
#endif
#ifdef MR_USE_TRAIL
	MR_discard_ticket();
#endif
	succeed_discard();

Define_label(mercury__exception__builtin_catch_3_3_i1);
	succeed_discard();

/*
** builtin_catch(Goal, Handler, Result)
**	call Goal(R).
**	if succeeds, set Result = R.
**	if fails, fail.
**	if throws an exception, call Handler(Result).
**
** This is the model_non version.
** On entry, we have a type_info (which we don't use) in r1,
** the Goal to execute in r2 and the Handler in r3.
** On exit, we should put Result in r1 (with COMPACT_ARGS) or r3.
*/
Define_entry(mercury__exception__builtin_catch_3_4); /* multi */
#ifdef PROFILE_CALLS
{
	tailcall(ENTRY(mercury__exception__builtin_catch_3_5), 
		ENTRY(mercury__exception__builtin_catch_3_4));
}
#endif
Define_entry(mercury__exception__builtin_catch_3_5); /* nondet */
	/*
	** Create a handler on the stack with the special redoip
	** of `exception_handler_do_fail' (we'll look for this redoip
	** when unwinding the nondet stack in builtin_throw/1),
	** and save the stuff we will need if an exception is thrown.
	*/
	mkpragmaframe(""builtin_catch/3 [model_nondet]"", 0,
		Exception_Handler_Frame_struct,
		ENTRY(exception_handler_do_fail));
	FRAMEVARS->code_model = MODEL_NON;
	FRAMEVARS->handler = r3;		/* save the Handler closure */
	FRAMEVARS->stack_ptr = MR_sp;	/* save the det stack pointer */
#ifndef CONSERVATIVE_GC
	/* save the heap and solutions heap pointers */
	FRAMEVARS->heap_ptr = MR_hp;
	FRAMEVARS->solns_heap_ptr = MR_sol_hp;
	FRAMEVARS->heap_zone = MR_heap_zone;
#endif
#ifdef MR_USE_TRAIL
	/* save the trail state */
	MR_mark_ticket_stack(FRAMEVARS->ticket_counter);
	MR_store_ticket(FRAMEVARS->trail_ptr);
#endif

	/*
	** Now we need to create another frame.
	** This is so that we can be sure that no-one will hijack
	** the redoip of the special frame we created above.
	** (The compiler sometimes generates ``hijacking'' code that saves
	** the topmost redoip on the stack, and temporarily replaces it
	** with a new redoip that will do some processing on failure
	** before restoring the original redoip.  This would cause
	** problems when doing stack unwinding in builtin_throw/1,
	** because we wouldn't be able to find the special redoip.
	** But code will only ever hijacks the topmost frame, so we
	** can avoid this by creating a second frame above the special
	** frame.)
	*/
	succip = LABEL(mercury__exception__builtin_catch_3_5_i1);
#ifdef MR_USE_TRAIL
	mkframe(""builtin_catch_2/1 [nondet]"", 0,
		LABEL(mercury__exception__builtin_catch_3_5_i3));
#else
	mkframe(""builtin_catch_2/1 [nondet]"", 0, ENTRY(do_fail));
#endif

	/*
	** Now call `Goal(Result)'.
	*/
	r1 = r2;	/* the Goal to call */
	r2 = 0;		/* Zero additional input arguments */
	r3 = 1;		/* One output argument */
	call(ENTRY(do_call_nondet_closure), 
		LABEL(mercury__exception__builtin_catch_3_5_i2),
		ENTRY(mercury__exception__builtin_catch_3_5));
		
Define_label(mercury__exception__builtin_catch_3_5_i2);
	update_prof_current_proc(LABEL(mercury__exception__builtin_catch_3_5));
	/*
	** On exit from do_call_nondet_closure, Result is in r1
	*/
#ifndef COMPACT_ARGS
	r3 = r1;
#endif
	/*
	** Note that we need to keep the trail ticket still,
	** in case it is needed again on backtracking.
	** We can only discard it when we fail() out, or
	** (if an exception is thrown) in the throw.
	*/
	succeed();

Define_label(mercury__exception__builtin_catch_3_5_i1);
	succeed();

#ifdef MR_USE_TRAIL
Define_label(mercury__exception__builtin_catch_3_5_i3);
	MR_discard_ticket();
	fail();
#endif

/*
** builtin_throw(Exception):
**	Throw the specified exception.
**	That means unwinding the nondet stack until we find a handler, and then
**	calling Handler(Result).
**
** On entry, we have Exception in r1.
*/
Define_entry(mercury__exception__builtin_throw_1_0);
{
	Word exception = r1;
	Word handler;
	enum CodeModel catch_code_model;

	/*
	** Search the nondet stack for an exception handler,
	** i.e. a frame whose redoip is `exception_handler_do_fail'
	** (one created by `builtin_catch').
	** N.B.  We search down the `succfr' chain, not the `prevfr' chain;
	** this ensures that we only find handlers installed by our callers,
	** not handlers installed by procedures that we called but which
	** are still on the nondet stack because they choice points behind.
	*/
	do {
		MR_curfr = cursuccfr;
		if (MR_curfr < MR_CONTEXT(nondetstack_zone)->min) {
			fatal_error(""builtin_throw/1: uncaught exception"");
		}
	} while (curredoip != ENTRY(exception_handler_do_fail));

	/*
	** Save the handler we found, and reset the det stack top.
	*/
	MR_succip = cursuccip;
	catch_code_model = FRAMEVARS->code_model;
	handler = FRAMEVARS->handler;	
	MR_sp = FRAMEVARS->stack_ptr;	/* reset the det stack pointer */

#ifdef MR_USE_TRAIL
	/*
	** Reset the trail.
	*/
	MR_reset_ticket(FRAMEVARS->trail_ptr, MR_exception);
	MR_discard_tickets_to(FRAMEVARS->ticket_counter);
#endif
#ifndef CONSERVATIVE_GC
	/*
	** Reset the heap.  But we need to be careful to preserve the
	** thrown exception object.
	**
	** The following algorithm uses the `solutions heap', and will work
	** with non-conservative gc. We copy the exception object to the
	** solutions_heap, reset the heap pointer, and then copy it back.
	**
	** An improvement to this would be to copy the exception object to the
	** solutions heap, but have deep_copy add an offset to the pointers
	** (at least, those that would otherwise point to the solutions heap),
	** so that, when finished, a block move of the solutions heap back to
	** the real heap will leave all the pointers in the correct place.
	*/
{
	Word * saved_solns_heap_ptr;

	/* switch to the solutions heap */
	if (MR_heap_zone == FRAMEVARS->heap_zone) {
		swap_heaps();
	}

	saved_solns_heap_ptr = MR_hp;

	/*
	** deep_copy() the exception to the solutions heap.
	** Note that we need to save/restore the hp register, if it
	** is transient, before/after calling deep_copy().
	*/
	assert(FRAMEVARS->heap_ptr <= FRAMEVARS->heap_zone->top);
	save_transient_registers();
	exception = deep_copy(exception,
		(Word *) (Word) &mercury_data_std_util__base_type_info_univ_0,
		FRAMEVARS->heap_ptr, FRAMEVARS->heap_zone->top);
	restore_transient_registers();

	/* switch back to the ordinary heap */
	swap_heaps();

	/* reset the heap */
	assert(FRAMEVARS->heap_ptr <= MR_hp);
	MR_hp = FRAMEVARS->heap_ptr;

	/* deep_copy the exception back to the ordinary heap */
	assert(FRAMEVARS->solns_heap_ptr <= MR_solutions_heap_zone->top);
	save_transient_registers();
	exception = deep_copy(exception,
		(Word *) (Word) &mercury_data_std_util__base_type_info_univ_0,
		saved_solns_heap_ptr, MR_solutions_heap_zone->top);
	restore_transient_registers();

	/* reset the solutions heap */
	fflush(NULL);
	assert(FRAMEVARS->solns_heap_ptr <= saved_solns_heap_ptr);
	assert(saved_solns_heap_ptr <= MR_sol_hp);
	if (catch_code_model == MODEL_NON) {
		/*
		** If the code inside the try (catch) was nondet,
		** then its caller (which may be solutions/2) may
		** have put some more stuff on the solutions-heap
		** after the goal succeeded; the goal may have
		** only thrown after being re-entered on backtracking.
		** Thus we can only reset the solutions heap to
		** where it was before
		*/
		MR_sol_hp = saved_solns_heap_ptr;
	} else {
		/*
		** If the code inside the try (catch) was det or semidet,
		** we can safely reset the solutions heap to where
		** it was when it try (catch) was entered.
		*/
		MR_sol_hp = FRAMEVARS->solns_heap_ptr;
	}
}
#endif /* !defined(CONSERVATIVE_GC) */

	/*
	** Pop the final exception handler frame off the nondet stack,
	** and reset the nondet stack top.  (This must be done last,
	** since it invalidates all the framevars.)
	*/
	MR_maxfr = curprevfr;
	MR_curfr = MR_maxfr;

	/*
	** Now invoke the handler that we found, as `Handler(Result)',
	*/

	r1 = handler;		/* get the Handler closure */
	r2 = 1;			/* One additional input argument */
	r3 = 1;			/* One output argument */
	r4 = exception;		/* This is our one input argument */

#ifdef COMPACT_ARGS
	/*
	** If the catch was semidet, we need to set the success indicator
	** r1 to TRUE and return the result in r2; otherwise, we return
	** the result in r1, which is where do_call_det_closure puts it,
	** so we can to a tailcall.
	*/
	if (catch_code_model != MODEL_SEMI) {
		tailcall(ENTRY(do_call_det_closure), 
			ENTRY(mercury__exception__builtin_throw_1_0));
	}
	push(succip);
	call(ENTRY(do_call_det_closure), 
		LABEL(mercury__exception__builtin_throw_1_0_i1),
		ENTRY(mercury__exception__builtin_throw_1_0));
}
Define_label(mercury__exception__builtin_throw_1_0_i1);
	update_prof_current_proc(LABEL(mercury__exception__builtin_throw_1_0));
	/* we've just returned from do_call_det_closure */
	r2 = r1;
	r1 = TRUE;
	succip = (Code *) pop();
	proceed();

#else /* not COMPACT_ARGS */

	push(succip);
	push(catch_code_model);
	call(ENTRY(do_call_det_closure), 
		LABEL(mercury__exception__builtin_throw_1_0_i1)),
		ENTRY(mercury__exception__builtin_throw_1_0));
}
Define_label(mercury__exception__builtin_throw_1_0_i1);
	update_prof_current_proc(LABEL(mercury__exception__builtin_throw_1_0));
	/* we've just returned from do_call_det_closure */
	catch_code_model = pop();
	if (catch_code_model == MODEL_SEMI) {
		r5 = r1;
	else {
		r4 = r1;
	}
	succip = (Code *) pop();
	proceed();

#endif /* not COMPACT_ARGS */

Define_entry(exception_handler_do_fail);
	/*
	** `exception_handler_do_fail' is the same as `do_fail':
	** it just invokes fail().  The reason we don't just use
	** `do_fail' for this is that when unwinding the stack we
	** check for a redoip of `exception_handler_do_fail' and
	** handle it specially.
	*/
	fail();

END_MODULE

/* Ensure that the initialization code for the above module gets run. */
/*
INIT mercury_sys_init_exceptions
*/

/* suppress gcc -Wmissing-decls warning */
void mercury_sys_init_exceptions(void);

void mercury_sys_init_exceptions(void) {
	exceptions_module();
}

").

%-----------------------------------------------------------------------------%

/*
** unsafe_perform_io/2 is the same as unsafe_perform_io/1
** (see extras/trailed_update/unsafe.m)
** except that it also allows the predicate to return an output argument.
*/
:- impure pred unsafe_perform_io(pred(T, io__state, io__state), T).
:- mode unsafe_perform_io(pred(out, di, uo) is det, out) is det.
:- mode unsafe_perform_io(pred(out, di, uo) is cc_multi, out) is det.

:- pragma c_code(
unsafe_perform_io(P::(pred(out, di, uo) is det), X::out),
	may_call_mercury,
"{
	ME_exception_call_io_pred_det(TypeInfo_for_T, P, &X);
}").
:- pragma c_code(
unsafe_perform_io(P::(pred(out, di, uo) is cc_multi), X::out),
	may_call_mercury,
"{
	ME_exception_call_io_pred_cc_multi(TypeInfo_for_T, P, &X);
}").

:- pred call_io_pred(pred(T, io__state, io__state), T, io__state, io__state).
:- mode call_io_pred(pred(out, di, uo) is det, out, di, uo) is det.
:- mode call_io_pred(pred(out, di, uo) is cc_multi, out, di, uo) is cc_multi.

:- pragma export(call_io_pred(pred(out, di, uo) is det, out, di, uo),
		"ME_exception_call_io_pred_det").
:- pragma export(call_io_pred(pred(out, di, uo) is cc_multi, out, di, uo),
		"ME_exception_call_io_pred_cc_multi").

call_io_pred(P, X) --> P(X).

%-----------------------------------------------------------------------------%
