%-----------------------------------------------------------------------------%
% Copyright (C) 1997-2002 The University of Melbourne.
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
:- import_module std_util, list, io, store.

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
% try_store(Goal, Result, Store_0, Store):
%    Just like try_io, but for stores rather than io__states.
%
:- pred try_store(pred(T, store(S), store(S)),
		exception_result(T), store(S), store(S)).
:- mode try_store(pred(out, di, uo) is det,     
		out(cannot_fail), di, uo) is cc_multi.
:- mode try_store(pred(out, di, uo) is cc_multi,
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

:- pred try_store(determinism, 	    pred(T, store(S), store(S)),
				    exception_result(T), store(S), store(S)).
:- mode try_store(in(bound(det)),   pred(out, di, uo) is det,
				    out(cannot_fail), di, uo) is cc_multi.
:- mode try_store(in(bound(cc_multi)), pred(out, di, uo) is cc_multi,
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

:- pred get_determinism_2(pred(T, S, S),                 determinism).
:- mode get_determinism_2(pred(out, di, uo) is det,      out(bound(det)))
	is cc_multi.
:- mode get_determinism_2(pred(out, di, uo) is cc_multi, out(bound(cc_multi)))
	is cc_multi.

% The calls to error/1 here are needed to ensure that the
% declarative semantics of each clause is equivalent,
% but operationally they are unreachable;
% since each mode has determinism cc_multi,
% it will pick the first disjunct and discard the call to error/1.
% This relies on --no-reorder-disj.

:- pragma promise_pure(get_determinism/2).

get_determinism(_Pred::(pred(out) is det), Det::out(bound(det))) :-
	( cc_multi_equal(det, Det)
	; error("get_determinism")
	).
get_determinism(_Pred::(pred(out) is semidet), Det::out(bound(semidet))) :-
	( cc_multi_equal(semidet, Det)
	; error("get_determinism")
	).
get_determinism(_Pred::(pred(out) is cc_multi), Det::out(bound(cc_multi))) :-
	( cc_multi_equal(cc_multi, Det)
	; error("get_determinism")
	).
get_determinism(_Pred::(pred(out) is cc_nondet), Det::out(bound(cc_nondet))) :-
	( cc_multi_equal(cc_nondet, Det)
	; error("get_determinism")
	).
get_determinism(_Pred::(pred(out) is multi), Det::out(bound(multi))) :-
	( cc_multi_equal(multi, Det)
	; error("get_determinism")
	).
get_determinism(_Pred::(pred(out) is nondet), Det::out(bound(nondet))) :-
	( cc_multi_equal(nondet, Det)
	; error("get_determinism")
	).

:- pragma promise_pure(get_determinism_2/2).

get_determinism_2(
	_Pred::pred(out, di, uo) is det,
			Det::out(bound(det))) :-
	( cc_multi_equal(det, Det)
	; error("get_determinism_2")
	).
get_determinism_2(
	_Pred::pred(out, di, uo) is cc_multi,
			Det::out(bound(cc_multi))) :-
	( cc_multi_equal(cc_multi, Det)
	; error("get_determinism_2")
	).

% These are not worth inlining, since they will
% (presumably) not be called frequently, and so
% any increase in speed from inlining is not worth
% the increase in code size.
:- pragma no_inline(throw/1).
:- pragma no_inline(rethrow/1).

throw(Exception) :-
	type_to_univ(Exception, Univ),
	throw_impl(Univ).

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
	catch_impl((pred(R::out) is det :-
				wrap_success_or_failure(Goal, R)),
		wrap_exception, Result0),
	cc_multi_equal(Result0, Result).
try(semidet, Goal, Result) :-
	catch_impl((pred(R::out) is det :-
				wrap_success_or_failure(Goal, R)),
		wrap_exception, Result0),
	cc_multi_equal(Result0, Result).
try(cc_multi, Goal, Result) :-

	catch_impl(
		(pred(R::out) is cc_multi :-
				wrap_success_or_failure(Goal, R)
				),
		wrap_exception, Result).
try(cc_nondet, Goal, Result) :-
	catch_impl((pred(R::out) is cc_multi :-
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

% We need to switch on the Detism argument
% for the same reason as above.

try_store(StoreGoal, Result) -->
	{ get_determinism_2(StoreGoal, Detism) },
	try_store(Detism, StoreGoal, Result).

	% Store0 is not really unique in the calls to unsafe_promise_unique
	% below, since it is also used in the calls to handle_store_result.
	% But it is safe to treat it as if it were unique, because the
	% other reference is only used in the case when an exception is
	% thrown, and in that case the declarative semantics of this
	% predicate say that the final store returned is unspecified.
try_store(det, StoreGoal, Result, Store0, Store) :-
	Goal = (pred({R, S}::out) is det :-
		unsafe_promise_unique(Store0, S0),
		StoreGoal(R, S0, S)),
	try(det, Goal, Result0),
	handle_store_result(Result0, Result, Store0, Store).
try_store(cc_multi, StoreGoal, Result, Store0, Store) :-
	Goal = (pred({R, S}::out) is cc_multi :-
		unsafe_promise_unique(Store0, S0),
		StoreGoal(R, S0, S)),
	try(cc_multi, Goal, Result0),
	handle_store_result(Result0, Result, Store0, Store).

:- pred handle_store_result(exception_result({T, store(S)})::in(cannot_fail),
		exception_result(T)::out(cannot_fail),
		store(S)::in, store(S)::uo) is det.
handle_store_result(Result0, Result, Store0, Store) :-
	(
		Result0 = succeeded({Res, S1}),
		Result = succeeded(Res),
		% S1 is now unique because the only other reference to the
		% store was from Store0, which we're throwing away here
		unsafe_promise_unique(S1, Store)
	;
		Result0 = exception(E0),
		% We need to make a copy of the exception object, in case
		% it contains a value returned from store__extract_ref_value.
		% See tests/hard_coded/exceptions/tricky_try_store.m.
		copy(E0, E),
		Result = exception(E),
		% Store0 is now unique because the only other reference to
		% the store was from the goal which just threw an exception.
		unsafe_promise_unique(Store0, Store)
	).

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
								is cc_multi.
% Mercury doesn't support impure higher-order pred terms, so if we want
% to form a closure from unsafe_perform_io, as we need to do above,
% then we must (falsely!) promise that it is pure.
:- pragma promise_pure(very_unsafe_perform_io/2). % XXX this is a lie

very_unsafe_perform_io(Goal, Result) :-
	impure make_io_state(IOState0),
	Goal(Result, IOState0, IOState),
	impure consume_io_state(IOState).

:- impure pred make_io_state(io__state::uo) is det.
:- pragma foreign_proc("C", make_io_state(_IO::uo),
		[will_not_call_mercury, thread_safe], "").
:- pragma foreign_proc("MC++", make_io_state(_IO::uo),
		[will_not_call_mercury, thread_safe], "").

:- impure pred consume_io_state(io__state::di) is det.
:- pragma foreign_proc("C", consume_io_state(_IO::di),
		[will_not_call_mercury, thread_safe], "").
:- pragma foreign_proc("MC++", consume_io_state(_IO::di),
		[will_not_call_mercury, thread_safe], "").

:- pred wrap_exception(univ::in, exception_result(T)::out) is det.
wrap_exception(Exception, exception(Exception)).

%-----------------------------------------------------------------------------%

:- pred throw_impl(univ).
:- mode throw_impl(in) is erroneous.

:- type handler(T) == pred(univ, T).
:- inst handler == (pred(in, out) is det).

%
% catch_impl/3 is actually impure.  But we don't declare it as impure,
% because the code for try_all/3 takes its address (to pass to
% unsorted_solutions/2), and Mercury does not (yet?) support
% impure higher-order pred terms.
%
:- pragma promise_pure(catch_impl/3).
:- /* impure */
   pred catch_impl(pred(T), handler(T), T).
:- mode catch_impl(pred(out) is det,       in(handler), out) is det.
:- mode catch_impl(pred(out) is semidet,   in(handler), out) is semidet.
:- mode catch_impl(pred(out) is cc_multi,  in(handler), out) is cc_multi.
:- mode catch_impl(pred(out) is cc_nondet, in(handler), out) is cc_nondet.
:- mode catch_impl(pred(out) is multi,     in(handler), out) is multi.
:- mode catch_impl(pred(out) is nondet,    in(handler), out) is nondet.

% by default we call the external implementation, but specific backends
% can provide their own definition using foreign_proc.

throw_impl(Univ::in) :-
	builtin_throw(Univ).


catch_impl(Pred::(pred(out) is det), Handler::in(handler), T::out) :-
	builtin_catch(Pred, Handler, T).
catch_impl(Pred::(pred(out) is semidet), Handler::in(handler), T::out) :-
	builtin_catch(Pred, Handler, T).
catch_impl(Pred::(pred(out) is cc_multi), Handler::in(handler), T::out) :-
	builtin_catch(Pred, Handler, T).
catch_impl(Pred::(pred(out) is cc_nondet), Handler::in(handler), T::out) :-
	builtin_catch(Pred, Handler, T).
catch_impl(Pred::(pred(out) is multi), Handler::in(handler), T::out) :-
	builtin_catch(Pred, Handler, T).
catch_impl(Pred::(pred(out) is nondet), Handler::in(handler), T::out) :-
	builtin_catch(Pred, Handler, T).

% builtin_throw and builtin_catch are implemented below using
% hand-coded low-level C code.
%
:- pred builtin_throw(univ).
:- mode builtin_throw(in) is erroneous.


:- /* impure */
   pred builtin_catch(pred(T), handler(T), T).
:- mode builtin_catch(pred(out) is det, in(handler), out) is det.
:- mode builtin_catch(pred(out) is semidet, in(handler), out) is semidet.
:- mode builtin_catch(pred(out) is cc_multi, in(handler), out) is cc_multi.
:- mode builtin_catch(pred(out) is cc_nondet, in(handler), out) is cc_nondet.
:- mode builtin_catch(pred(out) is multi, in(handler), out) is multi.
:- mode builtin_catch(pred(out) is nondet, in(handler), out) is nondet.

	

:- external(builtin_throw/1).
:- external(builtin_catch/3).


%-----------------------------------------------------------------------------%
%
% The --high-level-code implementation
%

:- pragma c_header_code("
/* protect against multiple inclusion */
#ifndef MR_HLC_EXCEPTION_GUARD
#define MR_HLC_EXCEPTION_GUARD

#ifdef MR_HIGHLEVEL_CODE

  #ifdef MR_USE_GCC_NESTED_FUNCTIONS
  	#define MR_CONT_PARAMS		MR_NestedCont cont
  	#define MR_CONT_PARAM_TYPES	MR_NestedCont
  	#define MR_CONT_ARGS		cont
  #else
  	#define MR_CONT_PARAMS		MR_Cont cont, void *cont_env
  	#define MR_CONT_PARAM_TYPES	MR_Cont, void *
  	#define MR_CONT_ARGS		cont, cont_env
  #endif

	/* det */
	void MR_CALL
	mercury__exception__builtin_catch_3_p_0(MR_Mercury_Type_Info type_info,
		MR_Pred pred, MR_Pred handler_pred, MR_Box *output);

	/* semidet */
	MR_bool MR_CALL
	mercury__exception__builtin_catch_3_p_1(MR_Mercury_Type_Info type_info,
		MR_Pred pred, MR_Pred handler_pred, MR_Box *output);

	/* cc_multi */
	void MR_CALL
	mercury__exception__builtin_catch_3_p_2(MR_Mercury_Type_Info type_info,
		MR_Pred pred, MR_Pred handler_pred, MR_Box *output);

	/* cc_nondet */
	MR_bool MR_CALL
	mercury__exception__builtin_catch_3_p_3(MR_Mercury_Type_Info type_info,
		MR_Pred pred, MR_Pred handler_pred, MR_Box *output);

	/* multi */
	void MR_CALL
	mercury__exception__builtin_catch_3_p_4(MR_Mercury_Type_Info type_info,
		MR_Pred pred, MR_Pred handler_pred, MR_Box *output,
		MR_CONT_PARAMS);

	/* nondet */
	void MR_CALL
	mercury__exception__builtin_catch_3_p_5(MR_Mercury_Type_Info type_info,
		MR_Pred pred, MR_Pred handler_pred, MR_Box *output,
		MR_CONT_PARAMS);

  #ifndef MR_AVOID_MACROS

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

  #endif /* !MR_AVOID_MACROS */

	void MR_CALL mercury__exception__builtin_throw_1_p_0(MR_Univ exception);

	void MR_CALL mercury__exception__builtin_catch_model_det(
		MR_Mercury_Type_Info type_info, MR_Pred pred,
		MR_Pred handler_pred, MR_Box *output);
	MR_bool MR_CALL mercury__exception__builtin_catch_model_semi(
		MR_Mercury_Type_Info type_info, MR_Pred pred,
		MR_Pred handler_pred, MR_Box *output);
	void MR_CALL mercury__exception__builtin_catch_model_non(
		MR_Mercury_Type_Info type_info, MR_Pred pred,
		MR_Pred handler_pred, MR_Box *output,
		MR_CONT_PARAMS);

#endif /* MR_HIGHLEVEL_CODE */

#endif /* MR_HLC_EXCEPTION_GUARD */
").

:- pragma c_code("
#ifdef MR_HIGHLEVEL_CODE

/*
** We also need to provide definitions of these builtins
** as functions rather than as macros.  This is needed
** (a) in case we take their address, and (b) for the
** GCC back-end interface.
*/

#undef mercury__exception__builtin_catch_3_p_0
#undef mercury__exception__builtin_catch_3_p_1
#undef mercury__exception__builtin_catch_3_p_2
#undef mercury__exception__builtin_catch_3_p_3
#undef mercury__exception__builtin_catch_3_p_4
#undef mercury__exception__builtin_catch_3_p_5

/* det ==> model_det */
void MR_CALL
mercury__exception__builtin_catch_3_p_0(MR_Mercury_Type_Info type_info,
	MR_Pred pred, MR_Pred handler_pred, MR_Box *output)
{
	mercury__exception__builtin_catch_model_det(type_info,
		pred, handler_pred, output);
}

/* semidet ==> model_semi */
MR_bool MR_CALL
mercury__exception__builtin_catch_3_p_1(MR_Mercury_Type_Info type_info,
	MR_Pred pred, MR_Pred handler_pred, MR_Box *output)
{
	return mercury__exception__builtin_catch_model_semi(type_info,
		pred, handler_pred, output);
}

/* cc_multi ==> model_det */
void MR_CALL
mercury__exception__builtin_catch_3_p_2(MR_Mercury_Type_Info type_info,
	MR_Pred pred, MR_Pred handler_pred, MR_Box *output)
{
	mercury__exception__builtin_catch_model_det(type_info,
		pred, handler_pred, output);
}

/* cc_nondet ==> model_semi */
MR_bool MR_CALL
mercury__exception__builtin_catch_3_p_3(MR_Mercury_Type_Info type_info,
	MR_Pred pred, MR_Pred handler_pred, MR_Box *output)
{
	return mercury__exception__builtin_catch_model_semi(type_info,
		pred, handler_pred, output);
}

/* multi ==> model_non */
void MR_CALL
mercury__exception__builtin_catch_3_p_4(MR_Mercury_Type_Info type_info,
	MR_Pred pred, MR_Pred handler_pred, MR_Box *output,
	MR_CONT_PARAMS)
{
	mercury__exception__builtin_catch_model_non(type_info,
		pred, handler_pred, output, MR_CONT_ARGS);
}

/* multi ==> model_non */
void MR_CALL
mercury__exception__builtin_catch_3_p_5(MR_Mercury_Type_Info type_info,
	MR_Pred pred, MR_Pred handler_pred, MR_Box *output,
	MR_CONT_PARAMS)
{
	mercury__exception__builtin_catch_model_non(type_info,
		pred, handler_pred, output, MR_CONT_ARGS);
}

/*---------------------------------------------------------------------------*/

static void
ML_call_goal_det_handcoded(MR_Mercury_Type_Info type_info,
	MR_Pred closure, MR_Box *result)
{
	typedef void MR_CALL DetFuncType(void *, MR_Box *);
	DetFuncType *code = (DetFuncType *)
		MR_field(MR_mktag(0), closure, (MR_Integer) 1);
	(*code)((void *) closure, result);
}

static MR_bool
ML_call_goal_semi_handcoded(MR_Mercury_Type_Info type_info,
	MR_Pred closure, MR_Box *result)
{
	typedef MR_bool MR_CALL SemidetFuncType(void *, MR_Box *);
	SemidetFuncType *code = (SemidetFuncType *)
		MR_field(MR_mktag(0), closure, (MR_Integer) 1);
	return (*code)((void *) closure, result);
}

static void
ML_call_goal_non_handcoded(MR_Mercury_Type_Info type_info,
	MR_Pred closure, MR_Box *result, MR_CONT_PARAMS)
{
	typedef void MR_CALL NondetFuncType(void *, MR_Box *,
		MR_CONT_PARAM_TYPES);
	NondetFuncType *code = (NondetFuncType *)
		MR_field(MR_mktag(0), closure, (MR_Integer) 1);
	(*code)((void *) closure, result, MR_CONT_ARGS);
}

/*---------------------------------------------------------------------------*/

static void
ML_call_handler_det_handcoded(MR_Mercury_Type_Info type_info,
	MR_Pred closure, MR_Univ exception, MR_Box *result)
{
	typedef void MR_CALL HandlerFuncType(void *, MR_Box, MR_Box *);
	HandlerFuncType *code = (HandlerFuncType *)
		MR_field(MR_mktag(0), closure, (MR_Integer) 1);
	(*code)((void *) closure, (MR_Box) exception, result);
}

/*---------------------------------------------------------------------------*/

#include <stdlib.h>
#include <setjmp.h>

typedef struct ML_ExceptionHandler_struct {
	struct ML_ExceptionHandler_struct *prev;
	jmp_buf		handler;
	MR_Univ		exception;
} ML_ExceptionHandler;

ML_ExceptionHandler *ML_exception_handler;

void MR_CALL
mercury__exception__builtin_throw_1_p_0(MR_Univ exception)
{
	if (ML_exception_handler == NULL) {
		ML_report_uncaught_exception((MR_Word) exception);
		exit(EXIT_FAILURE);
	} else {
#ifdef	MR_DEBUG_JMPBUFS
		fprintf(stderr, ""throw longjmp %p\\n"",
			ML_exception_handler->handler);
#endif
		ML_exception_handler->exception = exception;
		longjmp(ML_exception_handler->handler, 1);
	}
}

void MR_CALL
mercury__exception__builtin_catch_model_det(MR_Mercury_Type_Info type_info,
	MR_Pred pred, MR_Pred handler_pred, MR_Box *output)
{
	ML_ExceptionHandler this_handler;

	this_handler.prev = ML_exception_handler;
	ML_exception_handler = &this_handler;

#ifdef	MR_DEBUG_JMPBUFS
	fprintf(stderr, ""detcatch setjmp %p\\n"", this_handler.handler);
#endif

	if (setjmp(this_handler.handler) == 0) {
		ML_call_goal_det_handcoded(type_info, pred, output);
		ML_exception_handler = this_handler.prev;
	} else {
#ifdef	MR_DEBUG_JMPBUFS
		fprintf(stderr, ""detcatch caught jmp %p\\n"",
			this_handler.handler);
#endif

		ML_exception_handler = this_handler.prev;
		ML_call_handler_det_handcoded(type_info, handler_pred,
			this_handler.exception, output);
	}
}

MR_bool MR_CALL
mercury__exception__builtin_catch_model_semi(MR_Mercury_Type_Info type_info,
	MR_Pred pred, MR_Pred handler_pred, MR_Box *output)
{
	ML_ExceptionHandler this_handler;

	this_handler.prev = ML_exception_handler;
	ML_exception_handler = &this_handler;

#ifdef	MR_DEBUG_JMPBUFS
	fprintf(stderr, ""semicatch setjmp %p\\n"", this_handler.handler);
#endif

	if (setjmp(this_handler.handler) == 0) {
		MR_bool result = ML_call_goal_semi_handcoded(type_info, pred,
			output);
		ML_exception_handler = this_handler.prev;
		return result;
	} else {
#ifdef	MR_DEBUG_JMPBUFS
		fprintf(stderr, ""semicatch caught jmp %p\\n"",
			this_handler.handler);
#endif

		ML_exception_handler = this_handler.prev;
		ML_call_handler_det_handcoded(type_info, handler_pred,
			this_handler.exception, output);
		return MR_TRUE;
	}
}

#ifdef MR_USE_GCC_NESTED_FUNCTIONS

void MR_CALL
mercury__exception__builtin_catch_model_non(MR_Mercury_Type_Info type_info,
	MR_Pred pred, MR_Pred handler_pred, MR_Box *output,
	MR_NestedCont cont)
{
	ML_ExceptionHandler this_handler;

	auto void MR_CALL success_cont(void);
	void MR_CALL success_cont(void) {
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

#ifdef	MR_DEBUG_JMPBUFS
	fprintf(stderr, ""noncatch setjmp %p\\n"", this_handler.handler);
#endif

	if (setjmp(this_handler.handler) == 0) {
		ML_call_goal_non_handcoded(type_info, pred, output,
			success_cont);
		ML_exception_handler = this_handler.prev;
	} else {
#ifdef	MR_DEBUG_JMPBUFS
		fprintf(stderr, ""noncatch caught jmp %p\\n"",
			this_handler.handler);
#endif

		ML_exception_handler = this_handler.prev;
		ML_call_handler_det_handcoded(type_info, handler_pred,
			this_handler.exception, output);
		(*cont)();
	}
}

#else /* ! MR_USE_GCC_NESTED_FUNCTIONS */

struct ML_catch_env {
	ML_ExceptionHandler	this_handler;
	MR_Cont			cont;
	void			*cont_env;
};

static void MR_CALL
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

void MR_CALL
mercury__exception__builtin_catch_model_non(MR_Mercury_Type_Info type_info,
	MR_Pred pred, MR_Pred handler_pred, MR_Box *output,
	MR_Cont cont, void *cont_env)
{
	struct ML_catch_env locals;
	locals.cont = cont;
	locals.cont_env = cont_env;

	locals.this_handler.prev = ML_exception_handler;
	ML_exception_handler = &locals.this_handler;

#ifdef	MR_DEBUG_JMPBUFS
	fprintf(stderr, ""noncatch setjmp %p\\n"", locals.this_handler.handler);
#endif

	if (setjmp(locals.this_handler.handler) == 0) {
		ML_call_goal_non_handcoded(type_info, pred, output,
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

#ifdef	MR_DEBUG_JMPBUFS
		fprintf(stderr, ""noncatch caught jmp %p\\n"",
			locals.this_handler.handler);
#endif


		ML_exception_handler = locals.this_handler.prev;
		ML_call_handler_det_handcoded(type_info, handler_pred,
			locals.this_handler.exception, output);
		cont(cont_env);
	}
}

#endif /* ! MR_USE_GCC_NESTED_FUNCTIONS */

#endif /* MR_HIGHLEVEL_CODE */
").


	% For the .NET backend we override throw_impl as it is easier to 
	% implement these things using foreign_proc.

:- pragma foreign_proc("C#", throw_impl(T::in),
		[will_not_call_mercury, promise_pure], "
	throw new mercury.runtime.Exception(T);
").


:- pragma foreign_proc("C#", 
	catch_impl(Pred::pred(out) is det, Handler::in(handler), T::out),
		[will_not_call_mercury, promise_pure], "
	try {
		mercury.exception.mercury_code.ML_call_goal_det(
			TypeInfo_for_T, Pred, ref T);
	}
	catch (mercury.runtime.Exception ex) {
		mercury.exception.mercury_code.ML_call_handler_det(
			TypeInfo_for_T, Handler, ex.mercury_exception, ref T);
	}
").
:- pragma foreign_proc("C#", 
	catch_impl(Pred::pred(out) is cc_multi, Handler::in(handler), T::out),
		[will_not_call_mercury, promise_pure], "
	try {
		mercury.exception.mercury_code.ML_call_goal_det(
			TypeInfo_for_T, Pred, ref T);
	}
	catch (mercury.runtime.Exception ex) {
		mercury.exception.mercury_code.ML_call_handler_det(
			TypeInfo_for_T, Handler, ex.mercury_exception, ref T);
	}
").
/*

	% We can't implement these until we implement semidet procedures 
	% for the C# interface.

:- pragma foreign_proc("C#", 
	catch_impl(Pred::pred(out) is semidet, Handler::in(handler), T::out),
		[will_not_call_mercury, promise_pure], "
	mercury.runtime.Errors.SORRY(""foreign code for this function"");
").

:- pragma foreign_proc("C#", 
	catch_impl(Pred::pred(out) is cc_nondet, Handler::in(handler), T::out),
		[will_not_call_mercury, promise_pure], "
	mercury.runtime.Errors.SORRY(""foreign code for this function"");
").


	% We can't implement these because nondet C# foreign_proc for C#
	% is not possible.

:- pragma foreign_proc("C#", 
	catch_impl(_Pred::pred(out) is multi, _Handler::in(handler), _T::out),
		[will_not_call_mercury, promise_pure], 
	local_vars(""),
	first_code(""),
	retry_code(""),
	common_code("
	mercury.runtime.Errors.SORRY(""foreign code for this function"");
	")
).
:- pragma foreign_proc("C#", 
	catch_impl(_Pred::pred(out) is nondet, _Handler::in(handler), _T::out),
		[will_not_call_mercury, promise_pure], 
	local_vars(""),
	first_code(""),
	retry_code(""),
	common_code("
	mercury.runtime.Errors.SORRY(""foreign code for this function"");
	")
).
*/




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

% This causes problems because the LLDS back-end
% does not let you export code with determinism `nondet'.
% Instead for C backends we hand-code it... see below.
% Hand-coding it also avoids the casting needed to use MR_Word
% (which `pragma export' procedures use for polymorphically
% typed arguments) rather than MR_Box.
%
% XXX for .NET backend we don't yet implement nondet exception handling.

% :- pragma export(call_goal(pred(out) is nondet,  out), "ML_call_goal_nondet").

:- pragma export(call_handler(pred(in, out) is det,     in, out),
	"ML_call_handler_det").

/*
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
	#include ""mercury_deep_profiling_hand.h""

	MR_DECLARE_TYPE_CTOR_INFO_STRUCT( \
			mercury_data_std_util__type_ctor_info_univ_0);
#endif
").

:- pragma c_code("

/* forward decls, to suppress gcc -Wmissing-decl warnings */
void mercury_sys_init_exceptions_init(void);
void mercury_sys_init_exceptions_init_type_tables(void);
#ifdef	MR_DEEP_PROFILING
void mercury_sys_init_exceptions_write_out_proc_statics(FILE *fp);
#endif

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
	do {								\\
		fflush(stdout);						\\
		fprintf(stderr, ""mdb: warning: %s\\n""			\\
			""This may result in some exception events\\n""	\\
			""being omitted from the trace.\\n"", (msg));	\\
	} while (0)

/*
** base_sp and base_curfr always hold MR_sp and MR_curfr. They exist
** only because we cannot take the addresses of MR_sp and MR_curfr.
*/

static MR_Code *
MR_trace_throw(MR_Code *success_pointer, MR_Word *base_sp, MR_Word *base_curfr)
{
	const MR_Internal	*label;
	const MR_Label_Layout	*return_label_layout;

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
		const MR_Proc_Layout		*entry_layout;
		MR_Code 			*MR_jumpaddr;
		MR_Stack_Walk_Step_Result	result;
		const char			*problem;

		/*
		** check if we've reached a frame with an exception handler
		*/
		entry_layout = return_label_layout->MR_sll_entry;
		if (!MR_DETISM_DET_STACK(entry_layout->MR_sle_detism)
			&& MR_redoip_slot(base_curfr) ==
			MR_ENTRY(MR_exception_handler_do_fail))
		{
			return NULL;
		}

		/*
		** invoke MR_trace() to trace the exception
		*/
		if (return_label_layout->MR_sll_port != MR_PORT_EXCEPTION) {
			MR_fatal_error(""return layout port is not exception"");
		}

		MR_jumpaddr = MR_trace(return_label_layout);
		if (MR_jumpaddr != NULL) {
			return MR_jumpaddr;
		}

		/*
		** unwind the stacks back to the previous stack frame
		*/
		result = MR_stack_walk_step(entry_layout, &return_label_layout,
			&base_sp, &base_curfr, &problem);
		if (result != MR_STEP_OK) {
			WARNING(problem);
			return NULL;
		}
		MR_restore_transient_registers();
		MR_sp = base_sp;
		MR_curfr = base_curfr;
		MR_save_transient_registers();
	}
	return NULL;
}

/* swap the heap with the solutions heap */
#define swap_heaps()							\\
{									\\
	/* save the current heap */					\\
	MR_Word		*swap_heaps_temp_hp;				\\
	MR_MemoryZone	*swap_heaps_temp_hp_zone;			\\
									\\
	swap_heaps_temp_hp = MR_hp;					\\
	swap_heaps_temp_hp_zone = MR_ENGINE(MR_eng_heap_zone);		\\
									\\
	/* set heap to solutions heap */				\\
	MR_hp = MR_sol_hp;						\\
	MR_ENGINE(MR_eng_heap_zone) =					\\
		MR_ENGINE(MR_eng_solutions_heap_zone);			\\
									\\
	/* set the solutions heap to be the old heap */			\\
	MR_sol_hp = swap_heaps_temp_hp;					\\
	MR_ENGINE(MR_eng_solutions_heap_zone) = swap_heaps_temp_hp_zone;\\
}

MR_define_extern_entry(mercury__exception__builtin_catch_3_0); /* det */
MR_define_extern_entry(mercury__exception__builtin_catch_3_1); /* semidet */
MR_define_extern_entry(mercury__exception__builtin_catch_3_2); /* cc_multi */
MR_define_extern_entry(mercury__exception__builtin_catch_3_3); /* cc_nondet */
MR_define_extern_entry(mercury__exception__builtin_catch_3_4); /* multi */
MR_define_extern_entry(mercury__exception__builtin_catch_3_5); /* nondet */

MR_define_extern_entry(mercury__exception__builtin_throw_1_0);

/* the following is defined in runtime/mercury_ho_call.c */
MR_declare_entry(mercury__do_call_closure);

/* the following is defined in runtime/mercury_trace_base.c */
MR_declare_entry(MR_do_trace_redo_fail);

#ifdef	MR_DEEP_PROFILING
MR_declare_label(mercury__exception__builtin_catch_3_0_i1);
MR_declare_label(mercury__exception__builtin_catch_3_1_i1);
MR_declare_label(mercury__exception__builtin_catch_3_2_i1);
MR_declare_label(mercury__exception__builtin_catch_3_3_i1);
MR_declare_label(mercury__exception__builtin_catch_3_4_i1);
MR_declare_label(mercury__exception__builtin_catch_3_5_i1);
#endif

MR_declare_label(mercury__exception__builtin_catch_3_0_i2);
MR_declare_label(mercury__exception__builtin_catch_3_1_i2);
MR_declare_label(mercury__exception__builtin_catch_3_2_i2);
MR_declare_label(mercury__exception__builtin_catch_3_3_i2);
MR_declare_label(mercury__exception__builtin_catch_3_4_i2);
MR_declare_label(mercury__exception__builtin_catch_3_5_i2);

#ifdef	MR_DEEP_PROFILING
MR_declare_label(mercury__exception__builtin_catch_3_0_i3);
MR_declare_label(mercury__exception__builtin_catch_3_1_i3);
MR_declare_label(mercury__exception__builtin_catch_3_2_i3);
MR_declare_label(mercury__exception__builtin_catch_3_3_i3);
MR_declare_label(mercury__exception__builtin_catch_3_4_i3);
MR_declare_label(mercury__exception__builtin_catch_3_5_i3);

MR_declare_label(mercury__exception__builtin_catch_3_4_i4);
MR_declare_label(mercury__exception__builtin_catch_3_5_i4);
MR_declare_label(mercury__exception__builtin_catch_3_4_i5);
MR_declare_label(mercury__exception__builtin_catch_3_5_i5);
#endif

#if	defined(MR_USE_TRAIL) || defined(MR_DEEP_PROFILING)
  MR_declare_label(mercury__exception__builtin_catch_3_4_i6);
  MR_declare_label(mercury__exception__builtin_catch_3_5_i6);
#endif

#ifdef	MR_DEEP_PROFILING
MR_declare_label(mercury__exception__builtin_catch_3_4_i7);
MR_declare_label(mercury__exception__builtin_catch_3_5_i7);
#endif

MR_declare_label(mercury__exception__builtin_throw_1_0_i1);

/*
** MR_MAKE_PROC_LAYOUT(entry, detism, slots, succip_locn, pred_or_func,
**			module, name, arity, mode)                         
*/

/*
** The various procedures of builtin_catch all allocate their stack frames
** on the nondet stack, so for the purposes of doing stack traces we say
** they have MR_DETISM_NON, even though they are not actually nondet.
*/ 

MR_MAKE_PROC_LAYOUT(mercury__exception__builtin_catch_3_0,
	MR_DETISM_NON, MR_PROC_NO_SLOT_COUNT, MR_LONG_LVAL_TYPE_UNKNOWN,
	MR_PREDICATE, ""exception"", ""builtin_catch"", 3, 0);
MR_MAKE_PROC_LAYOUT(mercury__exception__builtin_catch_3_1,
	MR_DETISM_NON, MR_PROC_NO_SLOT_COUNT, MR_LONG_LVAL_TYPE_UNKNOWN,
	MR_PREDICATE, ""exception"", ""builtin_catch"", 3, 1);
MR_MAKE_PROC_LAYOUT(mercury__exception__builtin_catch_3_2,
	MR_DETISM_NON, MR_PROC_NO_SLOT_COUNT, MR_LONG_LVAL_TYPE_UNKNOWN,
	MR_PREDICATE, ""exception"", ""builtin_catch"", 3, 2);
MR_MAKE_PROC_LAYOUT(mercury__exception__builtin_catch_3_3,
	MR_DETISM_NON, MR_PROC_NO_SLOT_COUNT, MR_LONG_LVAL_TYPE_UNKNOWN,
	MR_PREDICATE, ""exception"", ""builtin_catch"", 3, 3);
MR_MAKE_PROC_LAYOUT(mercury__exception__builtin_catch_3_4,
	MR_DETISM_NON, MR_PROC_NO_SLOT_COUNT, MR_LONG_LVAL_TYPE_UNKNOWN,
	MR_PREDICATE, ""exception"", ""builtin_catch"", 3, 4);
MR_MAKE_PROC_LAYOUT(mercury__exception__builtin_catch_3_5,
	MR_DETISM_NON, MR_PROC_NO_SLOT_COUNT, MR_LONG_LVAL_TYPE_UNKNOWN,
	MR_PREDICATE, ""exception"", ""builtin_catch"", 3, 5);

#ifdef	MR_DEEP_PROFILING
MR_MAKE_INTERNAL_LAYOUT(mercury__exception__builtin_catch_3_0, 1);
MR_MAKE_INTERNAL_LAYOUT(mercury__exception__builtin_catch_3_1, 1);
MR_MAKE_INTERNAL_LAYOUT(mercury__exception__builtin_catch_3_2, 1);
MR_MAKE_INTERNAL_LAYOUT(mercury__exception__builtin_catch_3_3, 1);
MR_MAKE_INTERNAL_LAYOUT(mercury__exception__builtin_catch_3_4, 1);
MR_MAKE_INTERNAL_LAYOUT(mercury__exception__builtin_catch_3_5, 1);
#endif

MR_MAKE_INTERNAL_LAYOUT(mercury__exception__builtin_catch_3_0, 2);
MR_MAKE_INTERNAL_LAYOUT(mercury__exception__builtin_catch_3_1, 2);
MR_MAKE_INTERNAL_LAYOUT(mercury__exception__builtin_catch_3_2, 2);
MR_MAKE_INTERNAL_LAYOUT(mercury__exception__builtin_catch_3_3, 2);
MR_MAKE_INTERNAL_LAYOUT(mercury__exception__builtin_catch_3_4, 2);
MR_MAKE_INTERNAL_LAYOUT(mercury__exception__builtin_catch_3_5, 2);

#ifdef	MR_DEEP_PROFILING
MR_MAKE_INTERNAL_LAYOUT(mercury__exception__builtin_catch_3_0, 3);
MR_MAKE_INTERNAL_LAYOUT(mercury__exception__builtin_catch_3_1, 3);
MR_MAKE_INTERNAL_LAYOUT(mercury__exception__builtin_catch_3_2, 3);
MR_MAKE_INTERNAL_LAYOUT(mercury__exception__builtin_catch_3_3, 3);
MR_MAKE_INTERNAL_LAYOUT(mercury__exception__builtin_catch_3_4, 3);
MR_MAKE_INTERNAL_LAYOUT(mercury__exception__builtin_catch_3_5, 3);
#endif

#ifdef	MR_DEEP_PROFILING
MR_MAKE_INTERNAL_LAYOUT(mercury__exception__builtin_catch_3_4, 4);
MR_MAKE_INTERNAL_LAYOUT(mercury__exception__builtin_catch_3_5, 4);
MR_MAKE_INTERNAL_LAYOUT(mercury__exception__builtin_catch_3_4, 5);
MR_MAKE_INTERNAL_LAYOUT(mercury__exception__builtin_catch_3_5, 5);
#endif

#if	defined(MR_USE_TRAIL) || defined(MR_DEEP_PROFILING)
MR_MAKE_INTERNAL_LAYOUT(mercury__exception__builtin_catch_3_4, 6);
MR_MAKE_INTERNAL_LAYOUT(mercury__exception__builtin_catch_3_5, 6);
#endif

#ifdef	MR_DEEP_PROFILING
MR_MAKE_INTERNAL_LAYOUT(mercury__exception__builtin_catch_3_4, 7);
MR_MAKE_INTERNAL_LAYOUT(mercury__exception__builtin_catch_3_5, 7);
#endif

MR_MAKE_PROC_LAYOUT(mercury__exception__builtin_throw_1_0,
        MR_DETISM_DET, 1, MR_LONG_LVAL_STACKVAR(1),
        MR_PREDICATE, ""exception"", ""builtin_throw"", 1, 0);
MR_MAKE_INTERNAL_LAYOUT(mercury__exception__builtin_throw_1_0, 1);

#ifdef	MR_DEEP_PROFILING
/* XXX the 0s are fake line numbers */
MR_proc_static_user_ho(exception, builtin_catch, 3, 0,
	""exception.m"", 0, MR_TRUE);
MR_proc_static_user_ho(exception, builtin_catch, 3, 1,
	""exception.m"", 0, MR_TRUE);
MR_proc_static_user_ho(exception, builtin_catch, 3, 2,
	""exception.m"", 0, MR_TRUE);
MR_proc_static_user_ho(exception, builtin_catch, 3, 3,
	""exception.m"", 0, MR_TRUE);
MR_proc_static_user_ho(exception, builtin_catch, 3, 4,
	""exception.m"", 0, MR_TRUE);
MR_proc_static_user_ho(exception, builtin_catch, 3, 5,
	""exception.m"", 0, MR_TRUE);
/*
** XXX Builtin_throw will eventually be able to make calls in deep profiling
** grades. In the meantime, we need its proc_static structure for its callers.
*/
MR_proc_static_user_empty(exception, builtin_throw, 1, 0,
	""exception.m"", 0, MR_FALSE);
#endif

MR_BEGIN_MODULE(exceptions_module)
	MR_init_entry_sl(mercury__exception__builtin_catch_3_0);
	MR_init_entry_sl(mercury__exception__builtin_catch_3_1);
	MR_init_entry_sl(mercury__exception__builtin_catch_3_2);
	MR_init_entry_sl(mercury__exception__builtin_catch_3_3);
	MR_init_entry_sl(mercury__exception__builtin_catch_3_4);
	MR_init_entry_sl(mercury__exception__builtin_catch_3_5);

#ifdef	MR_DEEP_PROFILING
	MR_init_label(mercury__exception__builtin_catch_3_0_i1);
	MR_init_label(mercury__exception__builtin_catch_3_1_i1);
	MR_init_label(mercury__exception__builtin_catch_3_2_i1);
	MR_init_label(mercury__exception__builtin_catch_3_3_i1);
	MR_init_label(mercury__exception__builtin_catch_3_4_i1);
	MR_init_label(mercury__exception__builtin_catch_3_5_i1);
#endif

	MR_init_label_sl(mercury__exception__builtin_catch_3_0_i2);
	MR_init_label_sl(mercury__exception__builtin_catch_3_1_i2);
	MR_init_label_sl(mercury__exception__builtin_catch_3_2_i2);
	MR_init_label_sl(mercury__exception__builtin_catch_3_3_i2);
	MR_init_label_sl(mercury__exception__builtin_catch_3_4_i2);
	MR_init_label_sl(mercury__exception__builtin_catch_3_5_i2);

#ifdef	MR_DEEP_PROFILING
	MR_init_label_sl(mercury__exception__builtin_catch_3_0_i3);
	MR_init_label_sl(mercury__exception__builtin_catch_3_1_i3);
	MR_init_label_sl(mercury__exception__builtin_catch_3_2_i3);
	MR_init_label_sl(mercury__exception__builtin_catch_3_3_i3);
	MR_init_label_sl(mercury__exception__builtin_catch_3_4_i3);
	MR_init_label_sl(mercury__exception__builtin_catch_3_5_i3);
#endif
	
#ifdef	MR_DEEP_PROFILING
	MR_init_label_sl(mercury__exception__builtin_catch_3_4_i4);
	MR_init_label_sl(mercury__exception__builtin_catch_3_5_i4);
	MR_init_label_sl(mercury__exception__builtin_catch_3_4_i5);
	MR_init_label_sl(mercury__exception__builtin_catch_3_5_i5);
#endif

#if	defined(MR_USE_TRAIL) || defined(MR_DEEP_PROFILING)
	MR_init_label_sl(mercury__exception__builtin_catch_3_4_i6);
	MR_init_label_sl(mercury__exception__builtin_catch_3_5_i6);
#endif
		
#ifdef	MR_DEEP_PROFILING
	MR_init_label_sl(mercury__exception__builtin_catch_3_4_i7);
	MR_init_label_sl(mercury__exception__builtin_catch_3_5_i7);
#endif

	MR_init_entry_sl(mercury__exception__builtin_throw_1_0);
	MR_init_label_sl(mercury__exception__builtin_throw_1_0_i1);
MR_BEGIN_CODE

/*
** builtin_catch(Goal, Handler, Result)
**	call Goal(R).
**	if succeeds, set Result = R.
**	if fails, fail.
**	if throws an exception, call Handler(Exception, Result).
**
** On entry, we have a type_info (which we don't use) in MR_r1,
** the Goal to execute in MR_r2 and the Handler in MR_r3.
** On exit, we should put Result in MR_r1.
**
** There are slight differences between the versions of the code
** for the different determinisms.
*/
	
#define	save_r1			do {					\
					MR_framevar(1) = MR_r1;		\
				} while (0)
#define	save_r1r2		do {					\
					MR_framevar(1) = MR_r1;		\
					MR_framevar(2) = MR_r2;		\
				} while (0)
#define	restore_r1		do {					\
					MR_r1 = MR_framevar(1);		\
				} while (0)
#define	restore_r1r2		do {					\
					MR_r1 = MR_framevar(1);		\
					MR_r2 = MR_framevar(2);		\
				} while (0)

/* mercury__exception__builtin_catch_3_0: the det version */
#define	proc_label		mercury__exception__builtin_catch_3_0
#define	proc_static		MR_proc_static_user_name(exception,	\
					builtin_catch, 3, 0)
#define	excp_handler		MR_MODEL_DET_HANDLER
#define	model			""[model det]""
#define	save_results()		save_r1
#define	restore_results()	restore_r1
#define	handle_ticket_on_exit()	do {					\
					MR_prune_ticket();		\
				} while (0)

#include ""mercury_exception_catch_body.h""

#undef	proc_static
#undef	proc_label

/* mercury__exception__builtin_catch_3_2: the cc_multi version */
/* identical to mercury__exception__builtin_catch_3_0 except for label names */
#define	proc_label		mercury__exception__builtin_catch_3_2
#define	proc_static		MR_proc_static_user_name(exception,	\
					builtin_catch, 3, 2)

#include ""mercury_exception_catch_body.h""

#undef	handle_ticket_on_exit
#undef	restore_results
#undef	save_results
#undef	model
#undef	excp_handler
#undef	proc_static
#undef	proc_label

/* mercury__exception__builtin_catch_3_1: the semidet version */
#define	proc_label		mercury__exception__builtin_catch_3_1
#define	proc_static		MR_proc_static_user_name(exception,	\
					builtin_catch, 3, 1)
#define	excp_handler		MR_MODEL_SEMI_HANDLER
#define	model			""[model semi]""
#define	save_results()		save_r1r2
#define	restore_results()	restore_r1r2
#define	handle_ticket_on_exit()	do {					\
					if (MR_r1) {			\
						MR_prune_ticket();	\
					} else {			\
						MR_discard_ticket();	\
					}				\
				} while (0)

#include ""mercury_exception_catch_body.h""

#undef	proc_static
#undef	proc_label

/* mercury__exception__builtin_catch_3_3: the cc_nondet version */
/* identical to mercury__exception__builtin_catch_3_1 except for label names */
#define	proc_label		mercury__exception__builtin_catch_3_3
#define	proc_static		MR_proc_static_user_name(exception,	\
					builtin_catch, 3, 3)

#include ""mercury_exception_catch_body.h""

#undef	handle_ticket_on_exit
#undef	restore_results
#undef	save_results
#undef	model
#undef	excp_handler
#undef	proc_static
#undef	proc_label

/* mercury__exception__builtin_catch_3_4: the multi version */
#define	proc_label		mercury__exception__builtin_catch_3_4
#define	proc_static		MR_proc_static_user_name(exception,	\
					builtin_catch, 3, 4)
#define	excp_handler		MR_MODEL_NON_HANDLER
#define	model			""[model non]""
#define	save_results()		save_r1
#define	restore_results()	restore_r1
#define	version_model_non	MR_TRUE
#define	handle_ticket_on_exit()	((void) 0)
#define	handle_ticket_on_fail()	do {					\
					MR_prune_ticket();		\
				} while (0)

#include ""mercury_exception_catch_body.h""

#undef	proc_static
#undef	proc_label

/* mercury__exception__builtin_catch_3_5: the nondet version */
/* identical to mercury__exception__builtin_catch_3_4 except for label names */
#define	proc_label		mercury__exception__builtin_catch_3_5
#define	proc_static		MR_proc_static_user_name(exception,	\
					builtin_catch, 3, 5)

#include ""mercury_exception_catch_body.h""

#undef	handle_ticket_on_fail
#undef	handle_ticket_on_exit
#undef	version_model_non
#undef	restore_results
#undef	save_results
#undef	model
#undef	excp_handler
#undef	proc_static
#undef	proc_label

/*
** builtin_throw(Exception):
**	Throw the specified exception.
**	That means unwinding the nondet stack until we find a handler,
**	unwinding all the other Mercury stacks, and then
**	calling longjmp() to unwind the C stack.
**	The longjmp() will branch to builtin_catch which will then
**	call Handler(Exception, Result).
**
** On entry, we have Exception in MR_r1.
*/

MR_define_entry(mercury__exception__builtin_throw_1_0);
{
	MR_Word				exception;
	MR_Word				handler;
	enum MR_HandlerCodeModel	catch_code_model;
	MR_Word				*orig_curfr;
	MR_Unsigned			exception_event_number;

	exception = MR_r1;
	exception_event_number = MR_trace_event_number;

	/*
	** let the debugger trace exception throwing
	*/
	if (MR_trace_enabled) {
		MR_Code *MR_jumpaddr;
		MR_trace_set_exception_value(exception);
		MR_save_transient_registers();
		MR_jumpaddr = MR_trace_throw(MR_succip, MR_sp, MR_curfr);
		MR_restore_transient_registers();
		if (MR_jumpaddr != NULL) MR_GOTO(MR_jumpaddr);
	}

	/*
	** Search the nondet stack for an exception handler,
	** i.e. a frame whose redoip is `MR_exception_handler_do_fail'
	** (one created by `builtin_catch').
	** N.B.  We search down the `succfr' chain, not the `prevfr' chain;
	** this ensures that we only find handlers installed by our callers,
	** not handlers installed by procedures that we called but which
	** are still on the nondet stack because they left choice points
	** behind.
	*/
	orig_curfr = MR_curfr;
	while (MR_redoip_slot(MR_curfr)
			!= MR_ENTRY(MR_exception_handler_do_fail))
	{
		MR_curfr = MR_succfr_slot(MR_curfr);
		if (MR_curfr < MR_CONTEXT(MR_ctxt_nondetstack_zone)->min) {
			MR_Word *save_succip;
			/*
			** There was no exception handler.
			** 
			** We restore the original value of MR_curfr,
			** print out some diagnostics,
			** and then terminate execution.
			**
			** We need to save the registers to the fake_reg
			** array using MR_save_registers() before calling
			** ML_report_uncaught_exception, since that is
			** Mercury code and the C->Mercury interface expects
			** the registers to be saved.
			** We also need to save & restore the MR_succip
			** across that call, since any call to Mercury code
			** may clobber MR_succip (and also the Mercury
			** registers MR_r1, MR_r2, MR_r3, etc., but for those
			** we don't care, since we don't use them).
			** Note that the MR_save_registers() alone is not
			** sufficient since the Mercury code may clobber the
			** copy of MR_succip in the fake_reg.
			*/
			MR_curfr = orig_curfr;
			fflush(stdout);
			save_succip = MR_succip;
			MR_save_registers();
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
					MR_FALSE);
			}
			exit(1);
		}
	}

	/*
	** Save the handler we found
	*/
	catch_code_model = MR_EXCEPTION_STRUCT->MR_excp_code_model;
	handler = MR_EXCEPTION_STRUCT->MR_excp_handler;

	/*
	** Reset the success ip (i.e. return address).
	** This ensures that when we return from this procedure,
	** we will return to the caller of `builtin_catch'.
	*/
	MR_succip = MR_succip_slot(MR_curfr);

	/*
	** Reset the det stack.
	*/
	MR_sp = MR_EXCEPTION_STRUCT->MR_excp_stack_ptr;

#ifdef MR_USE_TRAIL
	/*
	** Reset the trail.
	*/
	MR_reset_ticket(MR_EXCEPTION_STRUCT->MR_excp_trail_ptr,
		MR_exception);
	MR_discard_tickets_to(MR_EXCEPTION_STRUCT->MR_excp_ticket_counter);
#endif
#ifndef MR_CONSERVATIVE_GC
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
	MR_Word * saved_solns_heap_ptr;

	/* switch to the solutions heap */
	if (MR_ENGINE(MR_eng_heap_zone) ==
		MR_EXCEPTION_STRUCT->MR_excp_heap_zone)
	{
		swap_heaps();
	}

	saved_solns_heap_ptr = MR_hp;

	/*
	** MR_deep_copy() the exception to the solutions heap.
	** Note that we need to save/restore the hp register, if it
	** is transient, before/after calling MR_deep_copy().
	*/
	assert(MR_EXCEPTION_STRUCT->MR_excp_heap_ptr <=
		MR_EXCEPTION_STRUCT->MR_excp_heap_zone->top);
	MR_save_transient_registers();
	exception = MR_deep_copy(&exception,
		(MR_TypeInfo) &mercury_data_std_util__type_ctor_info_univ_0,
		MR_EXCEPTION_STRUCT->MR_excp_heap_ptr,
		MR_EXCEPTION_STRUCT->MR_excp_heap_zone->top);
	MR_restore_transient_registers();

	/* switch back to the ordinary heap */
	swap_heaps();

	/* reset the heap */
	assert(MR_EXCEPTION_STRUCT->MR_excp_heap_ptr <= MR_hp);
	MR_hp = MR_EXCEPTION_STRUCT->MR_excp_heap_ptr;

	/* MR_deep_copy the exception back to the ordinary heap */
	assert(MR_EXCEPTION_STRUCT->MR_excp_solns_heap_ptr <=
		MR_ENGINE(MR_eng_solutions_heap_zone)->top);
	MR_save_transient_registers();
	exception = MR_deep_copy(&exception,
		(MR_TypeInfo) &mercury_data_std_util__type_ctor_info_univ_0,
		saved_solns_heap_ptr,
		MR_ENGINE(MR_eng_solutions_heap_zone)->top);
	MR_restore_transient_registers();

	/* reset the solutions heap */
	assert(MR_EXCEPTION_STRUCT->MR_excp_solns_heap_ptr
		<= saved_solns_heap_ptr);
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
		MR_sol_hp = MR_EXCEPTION_STRUCT->MR_excp_solns_heap_ptr;
	}
}
#endif /* !defined(MR_CONSERVATIVE_GC) */

	/*
	** Pop the final exception handler frame off the nondet stack,
	** and reset the nondet stack top.  (This must be done last,
	** since it invalidates all the framevars.)
	*/
	MR_maxfr = MR_prevfr_slot(MR_curfr);
	MR_curfr = MR_succfr_slot(MR_curfr);

	/*
	** Now longjmp to the catch, which will invoke the handler
	** that we found.
	*/

#ifdef	MR_DEBUG_JMPBUFS
	fprintf(stderr, ""throw catch_code_model %d\\n"", catch_code_model);
#endif

	if (catch_code_model == MR_C_LONGJMP_HANDLER) {
#ifdef	MR_DEBUG_JMPBUFS
		fprintf(stderr, ""throw longjmp %p\\n"",
			*(MR_ENGINE(MR_eng_jmp_buf)));
#endif

		MR_ENGINE(MR_eng_exception) = (MR_Word *) exception;
		MR_save_registers();
		longjmp(*(MR_ENGINE(MR_eng_jmp_buf)), 1);
	}

	/*
	** Otherwise, the handler is a Mercury closure.
	** Invoke the handler as `Handler(Exception, Result)'.
	*/

#ifdef	MR_DEEP_PROFILING
	MR_fatal_error(""builtin_throw cannot (yet) invoke Mercury handlers in deep profiling grades"");
#endif

	MR_r1 = handler;	/* get the Handler closure */
	MR_r2 = 1;		/* One additional input argument */
	MR_r3 = 1;		/* One output argument */
	MR_r4 = exception;	/* This is our one input argument */

	/*
	** If the catch was semidet, we need to set the success indicator
	** MR_r1 to MR_TRUE and return the result in MR_r2; otherwise, we return
	** the result in MR_r1, which is where mercury__do_call_closure puts
	** it, so we can do a tailcall.
	*/
	if (catch_code_model != MR_MODEL_SEMI_HANDLER) {
		MR_tailcall(MR_ENTRY(mercury__do_call_closure), 
			MR_ENTRY(mercury__exception__builtin_throw_1_0));
	}
	MR_incr_sp_push_msg(1, ""builtin_throw/1"");
	MR_stackvar(1) = (MR_Word) MR_succip;
	MR_call(MR_ENTRY(mercury__do_call_closure), 
		MR_LABEL(mercury__exception__builtin_throw_1_0_i1),
		MR_ENTRY(mercury__exception__builtin_throw_1_0));
}
MR_define_label(mercury__exception__builtin_throw_1_0_i1);
	MR_update_prof_current_proc(
		MR_LABEL(mercury__exception__builtin_throw_1_0));
	/* we've just returned from mercury__do_call_closure */
	MR_r2 = MR_r1;
	MR_r1 = MR_TRUE;
	MR_succip = (MR_Code *) MR_stackvar(1);
	MR_decr_sp_pop_msg(1);
	MR_proceed(); /* return to the caller of `builtin_catch' */

MR_END_MODULE

#endif /* ! MR_HIGHLEVEL_CODE */

/* Ensure that the initialization code for the above module gets run. */
/*
INIT mercury_sys_init_exceptions
*/

void
mercury_sys_init_exceptions_init(void)
{
#ifndef	MR_HIGHLEVEL_CODE
	exceptions_module();
#endif
}

void
mercury_sys_init_exceptions_init_type_tables(void)
{
	/* no types to register */
}

#ifdef	MR_DEEP_PROFILING
void
mercury_sys_init_exceptions_write_out_proc_statics(FILE *fp)
{
	MR_write_out_proc_static(fp, (MR_ProcStatic *)
		&MR_proc_static_user_name(exception, builtin_catch, 3, 0));
	MR_write_out_proc_static(fp, (MR_ProcStatic *)
		&MR_proc_static_user_name(exception, builtin_catch, 3, 1));
	MR_write_out_proc_static(fp, (MR_ProcStatic *)
		&MR_proc_static_user_name(exception, builtin_catch, 3, 2));
	MR_write_out_proc_static(fp, (MR_ProcStatic *)
		&MR_proc_static_user_name(exception, builtin_catch, 3, 3));
	MR_write_out_proc_static(fp, (MR_ProcStatic *)
		&MR_proc_static_user_name(exception, builtin_catch, 3, 4));
	MR_write_out_proc_static(fp, (MR_ProcStatic *)
		&MR_proc_static_user_name(exception, builtin_catch, 3, 5));
	MR_write_out_proc_static(fp, (MR_ProcStatic *)
		&MR_proc_static_user_name(exception, builtin_throw, 1, 0));
}
#endif

").

%-----------------------------------------------------------------------------%

:- pragma export(report_uncaught_exception(in, di, uo),
	"ML_report_uncaught_exception").

:- pred report_uncaught_exception(univ, io__state, io__state).
:- mode report_uncaught_exception(in, di, uo) is cc_multi.

report_uncaught_exception(Exception) -->
	try_io(report_uncaught_exception_2(Exception), Result),
	(
		{ Result = succeeded(_) }
	;
		{ Result = exception(_) }
		% if we got a further exception while trying to report
		% the uncaught exception, just ignore it
	).

:- pred report_uncaught_exception_2(univ, unit, io__state, io__state).
:- mode report_uncaught_exception_2(in, out, di, uo) is det.

report_uncaught_exception_2(Exception, unit) -->
	io__flush_output,
	io__stderr_stream(StdErr),
	io__write_string(StdErr, "Uncaught exception:\n"),
	( { univ_to_type(Exception, software_error(Message)) } ->
		io__format(StdErr, "Software Error: %s\n", [s(Message)])
	;
		io__write(StdErr, univ_value(Exception)),
		io__nl(StdErr)
	),
	io__flush_output(StdErr).

%-----------------------------------------------------------------------------%
