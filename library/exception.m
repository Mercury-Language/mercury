%-----------------------------------------------------------------------------%
% Copyright (C) 1997-1999 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: exception.m.
% Main author: fjh.
% Stability: medium

% This file defines the Mercury interface for exception handling.

% Note that throwing an exception across the C interface won't work.
% That is, if a Mercury procedure that is exported to C using `pragma export'
% throws an exception which is not caught within that procedure, then
% you will get undefined behaviour.

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
:- import_module string, require.

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
% enumeration constants in the C enum `ML_Determinism' defined below.
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
		ML_DET,
		ML_SEMIDET,
		ML_CC_MULTI,
		ML_CC_NONDET,
		ML_MULTI,
		ML_NONDET,
		ML_ERRONEOUS,
		ML_FAILURE
	} ML_Determinism;
").

:- pragma c_code(
	get_determinism(_Pred::pred(out) is det,
			Det::out(bound(det))),
	will_not_call_mercury,
	"Det = ML_DET"
).
:- pragma c_code(
	get_determinism(_Pred::pred(out) is semidet,
			Det::out(bound(semidet))),
	will_not_call_mercury,
	"Det = ML_SEMIDET"
).
:- pragma c_code(
	get_determinism(_Pred::pred(out) is cc_multi,
			Det::out(bound(cc_multi))),
	will_not_call_mercury,
	"Det = ML_CC_MULTI"
).
:- pragma c_code(
	get_determinism(_Pred::pred(out) is cc_nondet,
			Det::out(bound(cc_nondet))),
	will_not_call_mercury,
	"Det = ML_CC_NONDET"
).
:- pragma c_code(
	get_determinism(_Pred::pred(out) is multi,
			Det::out(bound(multi))),
	will_not_call_mercury,
	"Det = ML_MULTI"
).
:- pragma c_code(
	get_determinism(_Pred::pred(out) is nondet,
			Det::out(bound(nondet))),
	will_not_call_mercury,
	"Det = ML_NONDET"
).

:- pragma c_code(
	get_determinism_2(_Pred::pred(out, di, uo) is det,
			Det::out(bound(det))),
	will_not_call_mercury,
	"Det = ML_DET"
).

:- pragma c_code(
	get_determinism_2(_Pred::pred(out, di, uo) is cc_multi,
			Det::out(bound(cc_multi))),
	will_not_call_mercury,
	"Det = ML_CC_MULTI"
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
%
% The --high-level-code implementation
%

:- pragma c_header_code("
#ifdef MR_HIGHLEVEL_CODE

	/* det ==> model_det */
	#define mercury__exception__builtin_catch_3_p_0 \
		mercury__exception__builtin_catch_model_det

	/* semidet ==> model_semi */
	#define mercury__exception__builtin_catch_3_p_1 \
		mercury__exception__builtin_catch_model_semi

	/* cc_multi ==> model_det */
	#define mercury__exception__builtin_catch_3_p_2 \
		mercury__exception__builtin_catch_model_det

	/* cc_nondet ==> model_semi */
	#define mercury__exception__builtin_catch_3_p_3 \
		mercury__exception__builtin_catch_model_semi

	/* multi ==> model_non */
	#define mercury__exception__builtin_catch_3_p_4 \
		mercury__exception__builtin_catch_model_non

	/* nondet ==> model_non */
	#define mercury__exception__builtin_catch_3_p_5 \
		mercury__exception__builtin_catch_model_non

	void mercury__exception__builtin_throw_1_p_0(MR_Word);

	void mercury__exception__builtin_throw_1_p_0(MR_Word exception);
	void mercury__exception__builtin_catch_model_det(MR_Word type_info,
		MR_Word pred, MR_Word handler_pred, MR_Box *output);
	bool mercury__exception__builtin_catch_model_semi(MR_Word type_info,
		MR_Word pred, MR_Word handler_pred, MR_Box *output);
	void mercury__exception__builtin_catch_model_non(MR_Word type_info,
		MR_Word pred, MR_Word handler_pred, MR_Box *output,
#ifdef MR_USE_GCC_NESTED_FUNCTIONS
		MR_NestedCont cont
#else
		MR_Cont cont, void *cont_env
#endif
	);
#endif /* MR_HIGHLEVEL_CODE */
").

:- pragma c_code("
#ifdef MR_HIGHLEVEL_CODE

/*---------------------------------------------------------------------------*/

static void
ML_call_goal_det(MR_Word type_info, MR_Word closure, MR_Box *result)
{
	typedef void FuncType(void *, MR_Box *);
	FuncType *code = (FuncType *)
		MR_field(MR_mktag(0), closure, (Integer) 1);
	(*code)((void *) closure, result);
}

static bool
ML_call_goal_semi(MR_Word type_info, MR_Word closure, MR_Box *result)
{
	typedef bool FuncType(void *, MR_Box *);
	FuncType *code = (FuncType *)
		MR_field(MR_mktag(0), closure, (Integer) 1);
	return (*code)((void *) closure, result);
}

#ifdef MR_USE_GCC_NESTED_FUNCTIONS

static void
ML_call_goal_non(MR_Word type_info, MR_Word closure, MR_Box *result,
	MR_NestedCont cont)
{
	typedef void FuncType(void *, MR_Box *, MR_NestedCont);
	FuncType *code = (FuncType *)
		MR_field(MR_mktag(0), closure, (Integer) 1);
	(*code)((void *) closure, result, cont);
}

#else

static void
ML_call_goal_non(MR_Word type_info, MR_Word closure, MR_Box *result,
	MR_Cont cont, void *cont_env)
{
	typedef void FuncType(void *, MR_Box *, MR_Cont, void *);
	FuncType *code = (FuncType *)
		MR_field(MR_mktag(0), closure, (Integer) 1);
	(*code)((void *) closure, result, cont, cont_env);
}

#endif

/*---------------------------------------------------------------------------*/

static void
ML_call_handler_det(MR_Word type_info, MR_Word closure, MR_Word exception,
	MR_Box *result)
{
	typedef void FuncType(void *, MR_Box, MR_Box *);
	FuncType *code = (FuncType *)
		MR_field(MR_mktag(0), closure, (Integer) 1);
	(*code)((void *) closure, exception, result);
}

static bool
ML_call_handler_semi(MR_Word type_info, MR_Word closure, MR_Word exception,
	MR_Box *result)
{
	typedef bool FuncType(void *, MR_Box, MR_Box *);
	FuncType *code = (FuncType *)
		MR_field(MR_mktag(0), closure, (Integer) 1);
	return (*code)((void *) closure, exception, result);
}

#ifdef MR_USE_GCC_NESTED_FUNCTIONS

static void
ML_call_handler_non(MR_Word type_info, MR_Word closure, MR_Word exception,
	MR_Box *result, MR_NestedCont cont)
{
	typedef void FuncType(void *, MR_Box, MR_Box *, MR_NestedCont);
	FuncType *code = (FuncType *)
		MR_field(MR_mktag(0), closure, (Integer) 1);
	(*code)((void *) closure, exception, result, cont);
}

#else

static void
ML_call_handler_non(MR_Word type_info, MR_Word closure, MR_Word exception,
	MR_Box *result, MR_Cont cont, void *cont_env)
{
	typedef void FuncType(void *, MR_Box, MR_Box *, MR_Cont, void *);
	FuncType *code = (FuncType *)
		MR_field(MR_mktag(0), closure, (Integer) 1);
	(*code)((void *) closure, exception, result, cont, cont_env);
}

#endif

/*---------------------------------------------------------------------------*/

#include <stdlib.h>
#include <setjmp.h>

typedef MR_Word MR_Univ;

typedef struct ML_ExceptionHandler_struct {
	struct ML_ExceptionHandler_struct *prev;
	jmp_buf		handler;
	MR_Univ		exception;
} ML_ExceptionHandler;

ML_ExceptionHandler *ML_exception_handler;

void
mercury__exception__builtin_throw_1_p_0(MR_Univ exception)
{
	if (ML_exception_handler->handler == NULL) {
		ML_report_uncaught_exception(exception);
		abort();
	} else {
		ML_exception_handler->exception = exception;
		longjmp(ML_exception_handler->handler, 1);
	}
}

void
mercury__exception__builtin_catch_model_det(MR_Word type_info,
	MR_Word pred, MR_Word handler_pred, MR_Box *output)
{
	ML_ExceptionHandler this_handler;

	this_handler.prev = ML_exception_handler;
	ML_exception_handler = &this_handler;
	if (setjmp(this_handler.handler) == 0) {
		ML_call_goal_det(type_info, pred, output);
		ML_exception_handler = this_handler.prev;
	} else {
		ML_exception_handler = this_handler.prev;
		ML_call_handler_det(type_info, handler_pred,
			this_handler.exception, output);
	}
}

bool
mercury__exception__builtin_catch_model_semi(MR_Word type_info,
	MR_Word pred, MR_Word handler_pred, MR_Box *output)
{
	ML_ExceptionHandler this_handler;

	this_handler.prev = ML_exception_handler;
	ML_exception_handler = &this_handler;
	if (setjmp(this_handler.handler) == 0) {
		bool result = ML_call_goal_semi(type_info, pred, output);
		ML_exception_handler = this_handler.prev;
		return result;
	} else {
		ML_exception_handler = this_handler.prev;
		return ML_call_handler_semi(type_info, handler_pred,
			this_handler.exception, output);
	}
}

#ifdef MR_USE_GCC_NESTED_FUNCTIONS

void
mercury__exception__builtin_catch_model_non(MR_Word type_info,
	MR_Word pred, MR_Word handler_pred, MR_Box *output,
	MR_NestedCont cont)
{
	ML_ExceptionHandler this_handler;

	auto void success_cont(void);
	void success_cont(void) {
		/*
		** If we reach here, it means that
		** the nondet goal has succeeded, so we
		** need to restore the previous exception
		** handler before calling its continuation
		*/
		ML_exception_handler = this_handler.prev;
		(*cont)();

		/* 
		** If we get here, it means that the continuation
		** has failed, and so we are about to redo the
		** nondet goal.  Thus we need to re-establish
		** its exception handler.
		*/
		ML_exception_handler = &this_handler;
	}

	this_handler.prev = ML_exception_handler;
	ML_exception_handler = &this_handler;
	if (setjmp(this_handler.handler) == 0) {
		ML_call_goal_non(type_info, pred, output, success_cont);
		ML_exception_handler = this_handler.prev;
	} else {
		ML_exception_handler = this_handler.prev;
		ML_call_handler_non(type_info, handler_pred,
			this_handler.exception, output, cont);
	}
}

#else /* ! MR_USE_GCC_NESTED_FUNCTIONS */

struct ML_catch_env {
	ML_ExceptionHandler	this_handler;
	MR_Cont			cont;
	void			*cont_env;
};

static void
ML_catch_success_cont(void *env_ptr) {
	struct ML_catch_env *env = (struct ML_catch_env *) env_ptr;

	/*
	** If we reach here, it means that
	** the nondet goal has succeeded, so we
	** need to restore the previous exception
	** handler before calling its continuation
	*/
	ML_exception_handler = env->this_handler.prev;
	(*env->cont)(env->cont_env);

	/* 
	** If we get here, it means that the continuation
	** has failed, and so we are about to redo the
	** nondet goal.  Thus we need to re-establish
	** its exception handler.
	*/
	ML_exception_handler = &env->this_handler;
}

void
mercury__exception__builtin_catch_model_non(MR_Word type_info,
	MR_Word pred, MR_Word handler_pred, MR_Box *output,
	MR_Cont cont, void *cont_env)
{
	struct ML_catch_env locals;
	locals.cont = cont;
	locals.cont_env = cont_env;

	locals.this_handler.prev = ML_exception_handler;
	ML_exception_handler = &locals.this_handler;
	if (setjmp(locals.this_handler.handler) == 0) {
		ML_call_goal_non(type_info, pred, output,
			ML_catch_success_cont, &locals);
		/*
		** If we reach here, it means that
		** the nondet goal has failed, so we
		** need to restore the previous exception
		** handler 
		*/
		ML_exception_handler = locals.this_handler.prev;
		return;
	} else {
		/*
		** We caught an exception.
		** Restore the previous exception handler,
		** and then invoke the handler predicate
		** for this handler.
		*/
		ML_exception_handler = locals.this_handler.prev;
		ML_call_handler_non(type_info, handler_pred,
			locals.this_handler.exception, output,
			cont, cont_env);
	}
}

#endif /* ! MR_USE_GCC_NESTED_FUNCTIONS */

#endif /* MR_HIGHLEVEL_CODE */
").

/*********
This causes problems because the LLDS back-end
does not let you export code with determinism `nondet'.
Instead we handle-code it... see below.

:- pred call_goal(pred(T), T).
:- mode call_goal(pred(out) is det, out) is det.
:- mode call_goal(pred(out) is semidet, out) is semidet.
:- mode call_goal(pred(out) is nondet, out) is nondet.

call_goal(Goal, Result) :- Goal(Result).

:- pred call_handler(pred(univ, T), univ, T).
:- mode call_handler(pred(in, out) is det, in, out) is det.
:- mode call_handler(pred(in, out) is semidet, in, out) is semidet.
:- mode call_handler(pred(in, out) is nondet, in, out) is nondet.

call_handler(Handler, Exception, Result) :- Handler(Exception, Result).

:- pragma export(call_goal(pred(out) is det,     out), "ML_call_goal_det").
:- pragma export(call_goal(pred(out) is semidet, out), "ML_call_goal_semidet").
% :- pragma export(call_goal(pred(out) is nondet,  out), "ML_call_goal_nondet").

:- pragma export(call_handler(pred(in, out) is det,     in, out),
	"ML_call_handler_det").
:- pragma export(call_handler(pred(in, out) is semidet, in, out),
	"ML_call_handler_semidet").
% :- pragma export(call_handler(pred(in, out) is nondet,  in, out),
%	"ML_call_handler_nondet").

*******/

%-----------------------------------------------------------------------------%
%
% The --no-high-level-code implementation
%

:- pragma c_header_code("
#ifndef MR_HIGHLEVEL_CODE
	#include <assert.h>
	#include <stdio.h>
	#include ""mercury_deep_copy.h""
	#include ""mercury_trace_base.h""
	#include ""mercury_stack_trace.h""
	#include ""mercury_layout_util.h""

	MR_DECLARE_TYPE_CTOR_INFO_STRUCT( \
			mercury_data_std_util__type_ctor_info_univ_0);
#endif
").

:- pragma c_code("
#ifndef MR_HIGHLEVEL_CODE

/*
** MR_trace_throw():
**	Unwind the stack as far as possible, until we reach a frame
**	with an exception handler.  As we go, invoke
**	`MR_trace(..., MR_PORT_EXCEPTION, ...)' for each stack frame,
**	to signal to the debugger that that procedure has exited via
**	an exception.  This allows to user to use the `retry' command
**	to restart a goal which exited via an exception.
**
**	Note that if MR_STACK_TRACE is not defined, then we may not be
**	able to traverse the stack all the way; in that case, we just
**	print a warning and then continue.  It might be better to just
**	`#ifdef' out all this code (and the code in builtin_throw which
**	calls it) if MR_STACK_TRACE is not defined.
*/

#define WARNING(msg)							\\
	fprintf(stderr, ""mdb: warning: %s\\n""				\\
		""This may result in some exception events\\n""		\\
		""being omitted from the trace.\\n"", (msg))

static Code *
MR_trace_throw(Code *success_pointer, Word *det_stack_pointer,
	Word *current_frame)
{
	const MR_Internal		*label;
	const MR_Stack_Layout_Label	*return_label_layout;

	/*
	** Find the layout info for the stack frame pointed to by MR_succip
	*/
	label = MR_lookup_internal_by_addr(success_pointer);
	if (label == NULL) {
		WARNING(""internal label not found\\n"");
		return NULL;
	}
	return_label_layout = label->i_layout;

	while (return_label_layout != NULL) {
		const MR_Stack_Layout_Entry	*entry_layout;
		Code 				*MR_jumpaddr;
		MR_Stack_Walk_Step_Result	result;
		const char			*problem;

		/*
		** check if we've reached a frame with an exception handler
		*/
		entry_layout = return_label_layout->MR_sll_entry;
		if (!MR_DETISM_DET_STACK(entry_layout->MR_sle_detism)
		    && MR_redoip_slot(current_frame) ==
			ENTRY(exception_handler_do_fail))
		{
			return NULL;
		}

		/*
		** invoke MR_trace() to trace the exception
		*/
		if (return_label_layout->MR_sll_port != MR_PORT_EXCEPTION) {
			fatal_error(""return layout port is not exception"");
		}

		MR_jumpaddr = MR_trace(return_label_layout);
		if (MR_jumpaddr != NULL) {
			return MR_jumpaddr;
		}

		/*
		** unwind the stacks back to the previous stack frame
		*/
		result = MR_stack_walk_step(entry_layout, &return_label_layout,
			&det_stack_pointer, &current_frame, &problem);
		if (result != STEP_OK) {
			WARNING(problem);
			return NULL;
		}
		restore_transient_registers();
		MR_sp = det_stack_pointer;
		MR_curfr = current_frame;
		save_transient_registers();
	}
	return NULL;
}

/* swap the heap with the solutions heap */
#define swap_heaps()							\\
{									\\
	/* save the current heap */					\\
	Word *swap_heaps_temp_hp = MR_hp;				\\
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

Define_extern_entry(mercury__exception__builtin_catch_3_0); /* det */
Define_extern_entry(mercury__exception__builtin_catch_3_1); /* semidet */
Define_extern_entry(mercury__exception__builtin_catch_3_2); /* cc_multi */
Define_extern_entry(mercury__exception__builtin_catch_3_3); /* cc_nondet */
Define_extern_entry(mercury__exception__builtin_catch_3_4); /* multi */
Define_extern_entry(mercury__exception__builtin_catch_3_5); /* nondet */

Define_extern_entry(mercury__exception__builtin_throw_1_0);

/* the following is defined in runtime/mercury_ho_call.c */
Declare_entry(mercury__do_call_closure);

/* the following is defined in runtime/mercury_trace_base.c */
Declare_entry(MR_do_trace_redo_fail);

Declare_label(mercury__exception__builtin_catch_3_2_i2);
Declare_label(mercury__exception__builtin_catch_3_3_i2);
Declare_label(mercury__exception__builtin_catch_3_5_i2);
#ifdef MR_USE_TRAIL
  Declare_label(mercury__exception__builtin_catch_3_5_i3);
#endif
Declare_label(mercury__exception__builtin_throw_1_0_i1);

#define BUILTIN_THROW_STACK_SIZE 1


/*
** MR_MAKE_PROC_LAYOUT(entry, detism, slots, succip_locn, pred_or_func,
**			module, name, arity, mode)                         
*/

MR_MAKE_PROC_LAYOUT(mercury__exception__builtin_throw_1_0,
        MR_DETISM_DET, BUILTIN_THROW_STACK_SIZE, MR_LONG_LVAL_STACKVAR(1),
        MR_PREDICATE, ""exception"", ""builtin_throw"", 1, 0);
MR_MAKE_INTERNAL_LAYOUT(mercury__exception__builtin_throw_1_0, 1);

/*
** The following procedures all allocate their stack frames on
** the nondet stack, so for the purposes of doing stack traces
** we say they have MR_DETISM_NON, even though they are not
** actually nondet.
*/ 
MR_MAKE_PROC_LAYOUT(mercury__exception__builtin_catch_3_2,
	MR_DETISM_NON,	/* really cc_multi; also used for det */
	MR_ENTRY_NO_SLOT_COUNT, MR_LONG_LVAL_TYPE_UNKNOWN,
	MR_PREDICATE, ""exception"", ""builtin_catch"", 3, 2);
MR_MAKE_PROC_LAYOUT(mercury__exception__builtin_catch_3_3,
	MR_DETISM_NON,	/* really cc_nondet; also used for semidet */
	MR_ENTRY_NO_SLOT_COUNT, MR_LONG_LVAL_TYPE_UNKNOWN,
	MR_PREDICATE, ""exception"", ""builtin_catch"", 3, 3);
MR_MAKE_PROC_LAYOUT(mercury__exception__builtin_catch_3_5,
	MR_DETISM_NON,	/* ; also used for multi */
	MR_ENTRY_NO_SLOT_COUNT, MR_LONG_LVAL_TYPE_UNKNOWN,
	MR_PREDICATE, ""exception"", ""builtin_catch"", 3, 5);

MR_MAKE_INTERNAL_LAYOUT(mercury__exception__builtin_catch_3_2, 1);
MR_MAKE_INTERNAL_LAYOUT(mercury__exception__builtin_catch_3_2, 2);
MR_MAKE_INTERNAL_LAYOUT(mercury__exception__builtin_catch_3_3, 1);
MR_MAKE_INTERNAL_LAYOUT(mercury__exception__builtin_catch_3_3, 2);
MR_MAKE_INTERNAL_LAYOUT(mercury__exception__builtin_catch_3_5, 1);
MR_MAKE_INTERNAL_LAYOUT(mercury__exception__builtin_catch_3_5, 2);
#ifdef MR_USE_TRAIL
  MR_MAKE_INTERNAL_LAYOUT(mercury__exception__builtin_catch_3_5, 3);
#endif

BEGIN_MODULE(exceptions_module)
	init_entry(mercury__exception__builtin_catch_3_0);
	init_entry(mercury__exception__builtin_catch_3_1);
	init_entry_sl(mercury__exception__builtin_catch_3_2);
	init_entry_sl(mercury__exception__builtin_catch_3_3);
	init_entry(mercury__exception__builtin_catch_3_4);
	init_entry_sl(mercury__exception__builtin_catch_3_5);
	init_label_sl(mercury__exception__builtin_catch_3_2_i2);
	init_label_sl(mercury__exception__builtin_catch_3_3_i2);
	init_label_sl(mercury__exception__builtin_catch_3_5_i2);
#ifdef MR_USE_TRAIL
	init_label(mercury__exception__builtin_catch_3_5_i3);
#endif
	init_entry(mercury__exception__builtin_throw_1_0);
	init_label(mercury__exception__builtin_throw_1_0_i1);
BEGIN_CODE

/*
** builtin_catch(Goal, Handler, Result)
**	call Goal(R).
**	if succeeds, set Result = R.
**	if throws an exception, call Handler(Exception, Result).
**
** This is the model_det version.
** On entry, we have a type_info (which we don't use) in r1,
** the Goal to execute in r2 and the Handler in r3.
** On exit, we should put Result in r1.
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
	** Create an exception handler entry on the nondet stack.
	** (Register r3 holds the Handler closure.)
	*/
	MR_create_exception_handler(""builtin_catch/3 [model_det]"",
		MR_MODEL_DET_HANDLER, r3, ENTRY(do_fail));
	
	/*
	** Now call `Goal(Result)'.
	*/
	r1 = r2;	/* The Goal to call */
	r2 = 0;		/* Zero additional input arguments */
	r3 = 1;		/* One output argument */
	call(ENTRY(mercury__do_call_closure), 
		LABEL(mercury__exception__builtin_catch_3_2_i2),
		ENTRY(mercury__exception__builtin_catch_3_2));
		
Define_label(mercury__exception__builtin_catch_3_2_i2);
	update_prof_current_proc(LABEL(mercury__exception__builtin_catch_3_2));
	/*
	** On exit from mercury__do_call_closure, Result is in r1
	**
	** We must now deallocate the ticket and nondet stack frame that
	** were allocated by MR_create_exception_handler().
	*/
#ifdef MR_USE_TRAIL
	MR_discard_ticket();
#endif
	MR_succeed_discard();

/*
** builtin_catch(Goal, Handler, Result)
**	call Goal(R).
**	if succeeds, set Result = R.
**	if fails, fail.
**	if throws an exception, call Handler(Exception, Result).
**
** This is the model_semi version.
** On entry, we have a type_info (which we don't use) in r1,
** the Goal to execute in r2 and the Handler in r3,
** and on exit, we should put Result in r2.
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
	** Create an exception handler entry on the nondet stack.
	** (Register r3 holds the Handler closure.)
	*/
	MR_create_exception_handler(""builtin_catch/3 [model_semi]"",
		MR_MODEL_SEMI_HANDLER, r3, ENTRY(do_fail));
	
	/*
	** Now call `Goal(Result)'.
	*/
	r1 = r2;	/* The Goal to call */
	r2 = 0;		/* Zero additional input arguments */
	r3 = 1;		/* One output argument */
	call(ENTRY(mercury__do_call_closure), 
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
#ifdef MR_USE_TRAIL
	MR_discard_ticket();
#endif
	MR_succeed_discard();

/*
** builtin_catch(Goal, Handler, Result)
**	call Goal(R).
**	if succeeds, set Result = R.
**	if fails, fail.
**	if throws an exception, call Handler(Exception, Result).
**
** This is the model_non version.
** On entry, we have a type_info (which we don't use) in r1,
** the Goal to execute in r2 and the Handler in r3.
** On exit, we should put Result in r1.
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
	** Create an exception handler entry on the nondet stack.
	** (Register r3 holds the Handler closure.)
	*/
#ifdef MR_USE_TRAIL
	MR_create_exception_handler(""builtin_catch/3 [model_nondet]"",
		MR_MODEL_NON_HANDLER, r3,
		LABEL(mercury__exception__builtin_catch_3_5_i3));
#else
	MR_create_exception_handler(""builtin_catch/3 [model_nondet]"",
		MR_MODEL_NON_HANDLER, r3, ENTRY(do_fail));
#endif
	

	/*
	** Now call `Goal(Result)'.
	*/
	r1 = r2;	/* the Goal to call */
	r2 = 0;		/* Zero additional input arguments */
	r3 = 1;		/* One output argument */
	call(ENTRY(mercury__do_call_closure), 
		LABEL(mercury__exception__builtin_catch_3_5_i2),
		ENTRY(mercury__exception__builtin_catch_3_5));
		
Define_label(mercury__exception__builtin_catch_3_5_i2);
	update_prof_current_proc(LABEL(mercury__exception__builtin_catch_3_5));
	/*
	** On exit from do_call_nondet_closure, Result is in r1
	**
	** Note that we need to keep the trail ticket still,
	** in case it is needed again on backtracking.
	** We can only discard it when we MR_fail() out, or
	** (if an exception is thrown) in the throw.
	*/
	MR_succeed();

#ifdef MR_USE_TRAIL
Define_label(mercury__exception__builtin_catch_3_5_i3);
	MR_discard_ticket();
	MR_fail();
#endif

/*
** builtin_throw(Exception):
**	Throw the specified exception.
**	That means unwinding the nondet stack until we find a handler,
**	unwinding all the other Mercury stacks, and then
**	calling longjmp() to unwind the C stack.
**	The longjmp() will branch to builtin_catch which will then
**	call Handler(Exception, Result).
**
** On entry, we have Exception in r1.
*/
Define_entry(mercury__exception__builtin_throw_1_0);
{
	Word exception = r1;
	Word handler;
	enum MR_HandlerCodeModel catch_code_model;
	Word *orig_curfr;
	Unsigned exception_event_number = MR_trace_event_number;

	/*
	** let the debugger trace exception throwing
	*/
	if (MR_trace_enabled) {
		Code *MR_jumpaddr;
		save_transient_registers();
		MR_jumpaddr = MR_trace_throw(MR_succip, MR_sp, MR_curfr);
		restore_transient_registers();
		if (MR_jumpaddr != NULL) GOTO(MR_jumpaddr);
	}

	/*
	** Search the nondet stack for an exception handler,
	** i.e. a frame whose redoip is `exception_handler_do_fail'
	** (one created by `builtin_catch').
	** N.B.  We search down the `succfr' chain, not the `prevfr' chain;
	** this ensures that we only find handlers installed by our callers,
	** not handlers installed by procedures that we called but which
	** are still on the nondet stack because they left choice points
	** behind.
	*/
	orig_curfr = MR_curfr;
	while (MR_redoip_slot(MR_curfr) != ENTRY(exception_handler_do_fail)) {
		MR_curfr = MR_succfr_slot(MR_curfr);
		if (MR_curfr < MR_CONTEXT(nondetstack_zone)->min) {
			Word *save_succip;
			/*
			** There was no exception handler.
			** 
			** We restore the original value of MR_curfr,
			** print out some diagnostics,
			** and then terminate execution.
			**
			** We need to save the registers to the fake_reg
			** array using save_registers() before calling
			** ML_report_uncaught_exception, since that is
			** Mercury code and the C->Mercury interface expects
			** the registers to be saved.
			** We also need to save & restore the MR_succip
			** across that call, since any call to Mercury code
			** may clobber MR_succip (and also the Mercury
			** registers r1, r2, r3, etc., but for those we don't
			** care, since we don't use them).
			** Note that the save_registers() alone is not
			** sufficient since the Mercury code may clobber the
			** copy of MR_succip in the fake_reg.
			*/
			MR_curfr = orig_curfr;
			fflush(stdout);
			save_succip = MR_succip;
			save_registers();
			ML_report_uncaught_exception(exception);
			MR_succip = save_succip;
			MR_trace_report(stderr);
			if (exception_event_number > 0) {
				fprintf(stderr, ""Last trace event before ""
					""the unhandled exception was ""
					""event #%ld.\\n"",
					(long) exception_event_number);
			}
			if (MR_trace_enabled) {
				/*
				** The stack has already been unwound
				** by MR_trace_throw(), so we can't dump it.
				** (In fact, if we tried to dump the now-empty
				** stack, we'd get incorrect results, since
				** MR_trace_throw() does not restore MR_succip
				** to the appropriate value.)
				*/
			} else {
				MR_dump_stack(MR_succip, MR_sp, MR_curfr,
					FALSE);
			}
			exit(1);
		}
	}

	/*
	** Save the handler we found
	*/
	catch_code_model = MR_EXCEPTION_FRAMEVARS->code_model;
	handler = MR_EXCEPTION_FRAMEVARS->handler;	

	/*
	** Reset the success ip (i.e. return address).
	** This ensures that when we return from this procedure,
	** we will return to the caller of `builtin_catch'.
	*/
	MR_succip = MR_succip_slot(MR_curfr);

	/*
	** Reset the det stack.
	*/
	MR_sp = MR_EXCEPTION_FRAMEVARS->stack_ptr;

#ifdef MR_USE_TRAIL
	/*
	** Reset the trail.
	*/
	MR_reset_ticket(MR_EXCEPTION_FRAMEVARS->trail_ptr, MR_exception);
	MR_discard_tickets_to(MR_EXCEPTION_FRAMEVARS->ticket_counter);
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
	if (MR_heap_zone == MR_EXCEPTION_FRAMEVARS->heap_zone) {
		swap_heaps();
	}

	saved_solns_heap_ptr = MR_hp;

	/*
	** deep_copy() the exception to the solutions heap.
	** Note that we need to save/restore the hp register, if it
	** is transient, before/after calling deep_copy().
	*/
	assert(MR_EXCEPTION_FRAMEVARS->heap_ptr <=
		MR_EXCEPTION_FRAMEVARS->heap_zone->top);
	save_transient_registers();
	exception = deep_copy(&exception,
		(Word *) &mercury_data_std_util__type_ctor_info_univ_0,
		MR_EXCEPTION_FRAMEVARS->heap_ptr,
		MR_EXCEPTION_FRAMEVARS->heap_zone->top);
	restore_transient_registers();

	/* switch back to the ordinary heap */
	swap_heaps();

	/* reset the heap */
	assert(MR_EXCEPTION_FRAMEVARS->heap_ptr <= MR_hp);
	MR_hp = MR_EXCEPTION_FRAMEVARS->heap_ptr;

	/* deep_copy the exception back to the ordinary heap */
	assert(MR_EXCEPTION_FRAMEVARS->solns_heap_ptr <=
		MR_solutions_heap_zone->top);
	save_transient_registers();
	exception = deep_copy(&exception,
		(Word *) &mercury_data_std_util__type_ctor_info_univ_0,
		saved_solns_heap_ptr, MR_solutions_heap_zone->top);
	restore_transient_registers();

	/* reset the solutions heap */
	assert(MR_EXCEPTION_FRAMEVARS->solns_heap_ptr <= saved_solns_heap_ptr);
	assert(saved_solns_heap_ptr <= MR_sol_hp);
	if (catch_code_model == MR_MODEL_NON_HANDLER) {
		/*
		** If the code inside the try (catch) was nondet,
		** then its caller (which may be solutions/2) may
		** have put some more stuff on the solutions-heap
		** after the goal succeeded; the goal may have
		** only thrown after being re-entered on backtracking.
		** Thus we can only reset the solutions heap to
		** where it was before copying the exception object to it.
		*/
		MR_sol_hp = saved_solns_heap_ptr;
	} else {
		/*
		** If the code inside the try (catch) was det or semidet,
		** we can safely reset the solutions heap to where
		** it was when it try (catch) was entered.
		*/
		MR_sol_hp = MR_EXCEPTION_FRAMEVARS->solns_heap_ptr;
	}
}
#endif /* !defined(CONSERVATIVE_GC) */

	/*
	** Pop the final exception handler frame off the nondet stack,
	** and reset the nondet stack top.  (This must be done last,
	** since it invalidates all the framevars.)
	*/
	MR_maxfr = MR_prevfr_slot(MR_curfr);
	MR_curfr = MR_maxfr;

	/*
	** Now longjmp to the catch, which will invoke the handler
	** that we found.
	*/

	if (catch_code_model == MR_C_LONGJMP_HANDLER) {
		MR_ENGINE(e_exception) = (Word *) exception;
		save_registers();
		longjmp(*(MR_ENGINE(e_jmp_buf)), 1);
	}

	/*
	** Otherwise, the handler is a Mercury closure.
	** Invoke the handler as `Handler(Exception, Result)'.
	*/
	r1 = handler;		/* get the Handler closure */
	r2 = 1;			/* One additional input argument */
	r3 = 1;			/* One output argument */
	r4 = exception;		/* This is our one input argument */

	/*
	** If the catch was semidet, we need to set the success indicator
	** r1 to TRUE and return the result in r2; otherwise, we return
	** the result in r1, which is where mercury__do_call_closure puts it,
	** so we can to a tailcall.
	*/
	if (catch_code_model != MR_MODEL_SEMI_HANDLER) {
		tailcall(ENTRY(mercury__do_call_closure), 
			ENTRY(mercury__exception__builtin_throw_1_0));
	}
	MR_incr_sp_push_msg(1, ""builtin_throw/1"");
	MR_stackvar(1) = (Word) MR_succip;
	call(ENTRY(mercury__do_call_closure), 
		LABEL(mercury__exception__builtin_throw_1_0_i1),
		ENTRY(mercury__exception__builtin_throw_1_0));
}
Define_label(mercury__exception__builtin_throw_1_0_i1);
	update_prof_current_proc(LABEL(mercury__exception__builtin_throw_1_0));
	/* we've just returned from mercury__do_call_closure */
	r2 = r1;
	r1 = TRUE;
	MR_succip = (Code *) MR_stackvar(1);
	MR_decr_sp_pop_msg(1);
	proceed(); /* return to the caller of `builtin_catch' */

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

#endif /* ! MR_HIGHLEVEL_CODE */

").

%-----------------------------------------------------------------------------%

:- pragma export(report_uncaught_exception(in, di, uo),
	"ML_report_uncaught_exception").

:- pred report_uncaught_exception(univ, io__state, io__state).
:- mode report_uncaught_exception(in, di, uo) is cc_multi.

report_uncaught_exception(Exception) -->
	try_io(report_uncaught_exception_2(Exception), Result),
	(	{ Result = succeeded(_) }
	;	{ Result = exception(_) }
		% if we got a further exception while trying to report
		% the uncaught exception, just ignore it
	).

:- pred report_uncaught_exception_2(univ, unit, io__state, io__state).
:- mode report_uncaught_exception_2(in, out, di, uo) is det.

report_uncaught_exception_2(Exception, unit) -->
	io__stderr_stream(StdErr),
	io__write_string(StdErr, "Uncaught exception:\n"),
	( { univ_to_type(Exception, software_error(Message)) } ->
		io__format(StdErr, "Software Error: %s\n", [s(Message)])
	;
		io__write(StdErr, univ_value(Exception)),
		io__nl(StdErr)
	).

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
	ML_exception_call_io_pred_det(TypeInfo_for_T, P, &X);
}").
:- pragma c_code(
unsafe_perform_io(P::(pred(out, di, uo) is cc_multi), X::out),
	may_call_mercury,
"{
	ML_exception_call_io_pred_cc_multi(TypeInfo_for_T, P, &X);
}").

:- pred call_io_pred(pred(T, io__state, io__state), T, io__state, io__state).
:- mode call_io_pred(pred(out, di, uo) is det, out, di, uo) is det.
:- mode call_io_pred(pred(out, di, uo) is cc_multi, out, di, uo) is cc_multi.

:- pragma export(call_io_pred(pred(out, di, uo) is det, out, di, uo),
		"ML_exception_call_io_pred_det").
:- pragma export(call_io_pred(pred(out, di, uo) is cc_multi, out, di, uo),
		"ML_exception_call_io_pred_cc_multi").

call_io_pred(P, X) --> P(X).

%-----------------------------------------------------------------------------%
