%-----------------------------------------------------------------------------%
% Copyright (C) 1998-2000,2003, 2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% File: aditi.m
% Main author: stayl
%
% This module provides an interface to the Aditi deductive database
% system developed at the University of Melbourne. See the 
% "Aditi deductive database interface" section of the Mercury
% Language Reference Manual (listed under "Implementation defined pragmas"
% in the "Pragmas" chapter) for details on how to compile database queries.
%
% For information on how to build programs which use this interface,
% see the example Mmakefile in $ADITI_HOME/demos/transactions. 
%
%
% Compilation grade notes (see the section "Compilation model options"
% in the Mercury User's Guide for more information):
%
%	This module requires a compilation grade with conservative garbage 
%	collection. Any grade containing `.gc' in its name, such as
%	`asm_fast.gc' or `asm_fast.gc.tr', will do.
%
% 	When trailing is not being used (the compilation grade does not
% 	contain `.tr'), resources will sometimes not be cleaned up until
%	the end of a transaction.
%	If there is a commit across a nondet database call, or an exception
%	is thrown, or a database call is retried in the debugger, the output
%	relation from the call and its cursor will not be cleaned up until the
%	transaction ends.
%	It is up to the programmer to decide whether imposing the overhead
%	of trailing on the rest of the program is worthwhile.
%
%	Compilation of this module in a high level C code grade (e.g. `hlc.gc')
%	is not yet supported.
%
%
% The transaction interface used here is described in
%	Kemp, Conway, Harris, Henderson, Ramamohanarao and Somogyi,
% 	"Database transactions in a purely declarative 
%		logic programming language", 
%	In Proceedings of the Fifth International Conference on Database
%	Systems for Advanced Applications, pp. 283-292.
%	Melbourne, Australia, 1-4 April, 1997.
%
%	This paper is also available as
%	Technical Report 96/45, Department of Computer Science, 
%	University of Melbourne, December 1996,
%	<http://www.cs.mu.OZ.AU/publications/tr_db/mu_96_45_cover.ps.gz>
%	and <http://www.cs.mu.OZ.AU/publications/tr_db/mu_96_45.ps.gz>.
%
%-----------------------------------------------------------------------------%
:- module aditi.

:- interface.

:- import_module io.

:- type aditi__state.

% XXX This will change to unique when the mode system is fully implemented.
:- inst aditi_unique == ground.
:- mode aditi_di  == in(aditi_unique).
:- mode aditi_uo  == out(aditi_unique).
:- mode aditi_ui  == in(aditi_unique).
:- mode aditi_mui == in(aditi_unique).

:- type aditi__result(T)
	--->	ok(T)
	;	error(aditi__error, string).

:- type aditi__result
	--->	ok
	;	error(aditi__error, string).

:- type aditi__error
	--->	error_creating_client
	;	invalid_passwd
	;	too_many_connections
	;	invalid_ticket
	;	general_failure
	;	already_logged_in
	;	not_logged_in
	;	not_connected
	;	not_implemented
	;	abort
	;	bad_value
	;	bad_rl_code
	;	error_opening_relation
	;	security_violation
	;	unique_key_violation
	;	relation_or_cursor_not_open
	;	timeout
	;	determinism_error	% The number of solutions returned
					% for a procedure did not match
					% its determinism declaration.
	;	parse_error_in_tuple	% Aditi returned a tuple
					% which the Mercury interface
					% code could not understand.
	.

:- type aditi__exception
	--->	aditi__exception(aditi__error, string).

:- type aditi__connection.

	% aditi__connect(Host, User, Passwd, Result).
	%
	% Only one connection is allowed per process.
:- pred aditi__connect(string, string, string,
		aditi__result(aditi__connection), io__state, io__state).
:- mode aditi__connect(in, in, in, out, di, uo) is det.

:- pred aditi__disconnect(aditi__connection, aditi__result,
		io__state, io__state).
:- mode aditi__disconnect(in, out, di, uo) is det.

:- type aditi__transaction(T) == pred(T, aditi__state, aditi__state).
:- inst aditi__transaction == (pred(out, aditi_di, aditi_uo) is det).

	% aditi__transaction(Connection, Transaction, Result).
	%
	% Start a transaction with the Aditi database referred to by
	% Connection, call Transaction, returning ok(Result) if the
	% transaction is not aborted, or error(Error, Msg) if
	% the transaction fails.
	%
	% If Transaction throws an exception, the transaction will
	% be aborted and the exception will be rethrown to the caller.
	%
	% Predicates with `:- pragma aditi' or `:- pragma base_relation'
	% markers can only be called from within a transaction -- there
	% is no other way to get an `aditi__state' to pass to them.
:- pred aditi__transaction(aditi__connection, aditi__transaction(T),
		aditi__result(T), io__state, io__state).
:- mode aditi__transaction(in, in(aditi__transaction), out, di, uo) is det.

	% As above, except that it throws an exception if the
	% transaction is aborted.
:- pred aditi__transaction_exception(aditi__connection, aditi__transaction(T),
		T, io__state, io__state).
:- mode aditi__transaction_exception(in, in(aditi__transaction),
		out, di, uo) is det.

	% aditi__aggregate_compute_initial(Closure, UpdateAcc, 
	% 		ComputeInitial, Results)
	%
	% When called, the query Closure returns the relation to be 
	% aggregated over. This relation must have two attributes,
	% the first being the attribute to group by. The closure 
	% ComputeInitial computes an initial accumulator for each 
	% group given the first tuple in the group. The closure
	% UpdateAcc is called for each tuple in each group to 
	% update the accumulator. The outputs are the group-by element
	% and final accumulator for each group.
	%
	% For example, to compute a sum over relation `p/3' where
	% the first non-aditi__state attribute of `p' is the group-by
	% attribute:
	% 	aditi__aggregate_compute_initial(p(DB), 
	%		(pred(_::in, Attr::in, Acc0::in, Acc::out) is det :-
	%			Acc = Acc0 + Attr),
	%		(pred(_::in, _::in, 0::out) is det :- true),
	%		GrpBy, Sum).
:- pred aditi__aggregate_compute_initial(pred(GrpBy, NonGrpBy), 
		pred(GrpBy, NonGrpBy, Acc, Acc),
		pred(GrpBy, NonGrpBy, Acc), GrpBy, Acc).
:- mode aditi__aggregate_compute_initial(pred(out, out) is nondet, 
		pred(in, in, in, out) is det,
		pred(in, in, out) is det, out, out) is nondet.
:- mode aditi__aggregate_compute_initial(pred(out, out) is multi, 
		pred(in, in, in, out) is det,
		pred(in, in, out) is det, out, out) is multi.

/*
	This should be translated into the equivalent
	aggregate_compute_initial, but that hasn't been
	done yet. The main problem is collecting the initial
	value - it may not be constant.

	Also, it would be nice to provide versions of aditi__aggregate
	which work over one attribute relations, as in std_util__aggregate. 

	% aditi_aggregate_given_initial(Closure, UpdateAcc, 
	% 		InitialAcc, Results)
	% 
	% Same as aditi__aggregate_compute_initial except the initial
	% accumulator is supplied, rather than computed from the first
	% element.
:- pred aditi__aggregate_given_initial(pred(GrpBy, NonGrpBy), 
		pred(GrpBy, NonGrpBy, Acc, Acc), Acc, GrpBy, Acc).
:- mode aditi__aggregate_given_initial(pred(out, out) is nondet, 
		pred(in, in, in, out) is det,
		in, out, out) is nondet.
*/

%-----------------------------------------------------------------------------%
:- implementation.

:- interface.

% These are used by aditi_private_builtin.m, but otherwise
% shouldn't be in the interface.
:- pragma foreign_type("C", aditi__connection, "MADITI_Connection").
:- pragma foreign_type("C", aditi__state, "MADITI_State").

:- implementation.

:- import_module bool, char, exception, list, require, std_util, string.
:- import_module aditi_private_builtin.

%-----------------------------------------------------------------------------%

:- pragma foreign_decl("C",
"
#include ""v2_api_without_engine.h""

typedef struct {
	apiID		connection;
	apiID		bytecode_transaction;
} MADITI_Connection;

typedef struct {
	apiID		connection;
	apiID		bytecode_transaction;
	apiID		transaction;
} MADITI_State;
").

%-----------------------------------------------------------------------------%

	% These are handled by the RL code generator.
:- external(aditi__aggregate_compute_initial/5).
%:- external(aditi__aggregate_given_initial/5).

%-----------------------------------------------------------------------------%

aditi__connect(Host, User, Passwd, Result) -->
	aditi_private_builtin__connect(Host,
		User, Passwd, Status, Connection),
	{ Status = 0 ->
		Result = ok(Connection)
	;
		aditi_private_builtin__error_code(Status, Error, String),
		Result = error(Error, String)
	}.

%-----------------------------------------------------------------------------%

aditi__disconnect(Connection, Result) -->
	aditi_private_builtin__disconnect(Connection, Status),
	{ Status = 0 ->
		Result = ok
	;
		aditi_private_builtin__error_code(Status, Error, Msg),
		Result = error(Error, Msg)
	}.

%-----------------------------------------------------------------------------%

aditi__transaction(Connection, Transaction, TransResult, IO0, IO) :-
	%
	% aditi__transaction_2 is cc_multi because of the call to
	% try_io, but if an Aditi exception occurs, it really
	% doesn't matter which one we get.
	%
	RunTransaction = 
		(pred(ResultAndIO0::out) is cc_multi :-
			unsafe_promise_unique(IO0, IO1),
			aditi__transaction_2(Connection, Transaction,
				Result, IO1, IO2),
			ResultAndIO0 = Result - IO2
		),
	ResultAndIO = promise_only_solution(RunTransaction),
	ResultAndIO = TransResult - IO3,
	unsafe_promise_unique(IO3, IO).

:- pred aditi__transaction_2(aditi__connection, aditi__transaction(T),
		aditi__result(T), io__state, io__state).
:- mode aditi__transaction_2(in, in(aditi__transaction),
		out, di, uo) is cc_multi.

aditi__transaction_2(Connection, Transaction, TransResult) -->
	aditi_private_builtin__start_transaction(Connection, StartResult),
	(
		{ StartResult = ok(DB0) },
		try_io((pred(Result::out, di, uo) is det -->
			{ Transaction(Result, DB0, DB) },
			aditi_private_builtin__commit_transaction(DB)
		    ), TransExceptionResult),
		(
			{ TransExceptionResult = succeeded(Results) },
			{ TransResult = ok(Results) }
		;
			{ TransExceptionResult = exception(Exception) },
			aditi_private_builtin__abort_transaction(DB0),
			( { univ_to_type(Exception, AditiException) } ->
				{ AditiException =
					aditi__exception(ErrorCode, String) },
				{ TransResult = error(ErrorCode, String) }
			;
				{ rethrow(TransExceptionResult) }
			)
		)
	;
		{ StartResult = error(StartErrorCode, StartMsg) },
		{ TransResult = error(StartErrorCode, StartMsg) }
	).

aditi__transaction_exception(Connection, Transaction, Result) -->
	aditi__transaction(Connection, Transaction, TransResult),
	{
		TransResult = ok(Result)
	;
		TransResult = error(ErrorCode, String),
		throw(aditi__exception(ErrorCode, String))
	}.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
