%---------------------------------------------------------------------------%
% Copyright (C) 1997 Mission Critical.
% Copyright (C) 1997-2000 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
% File: odbc.m
% Authors: Renaud Paquay (rpa@miscrit.be), stayl
% ODBC version: 2.0
%
% The transaction interface used here is described in the following paper:
%
%	Kemp, Conway, Harris, Henderson, Ramamohanarao and Somogyi,
% 	"Database transactions in a purely declarative 
%		logic programming language".
%	In Proceedings of the Fifth International Conference on Database
%	Systems for Advanced Applications, pp. 283-292.
%	Melbourne, Australia, 1-4 April, 1997.
%
%	An earlier(?) version of this paper is available as
%	Technical Report 96/45, Department of Computer Science, 
% 	University of Melbourne, December 1996,
%	<http://www.cs.mu.OZ.AU/publications/tr_db/mu_96_45_cover.ps.gz>
%	and <http://www.cs.mu.OZ.AU/publications/tr_db/mu_96_45.ps.gz>.
%
% This has been tested using the following platforms:
% - MySQL 3.20.19 and iODBC 2.12 under Solaris 2.5
% - MySQL 3.22.32 and iODBC 2.50.3 under Solaris 2.6
% - Microsoft SQL Server 6.5 under Windows NT 4.0 with the
%	GNU-Win32 tools beta 17.1
%
% Notes:
%
% 	Binary data is converted to a string of hexadecimal digits.
%
%	This module requires a compilation grade with conservative garbage 
% 	collection. Any grade containing .gc in its name, such as asm_fast.gc,
%	will do. See the section "Compilation model options" in the Mercury
%	User's Guide for more information.
%
% 	The header files distributed with the Microsoft ODBC SDK require
% 	some modification for compilation with gcc.  In particular,
% 	some conflicting typedefs for SQLUINTEGER and SQLSCHAR must be
% 	removed from sqltypes.h.
% 	(For legal reasons a patch cannot be included in the Mercury
% 	distribution.)
%	The Microsoft ODBC header files also use some C++ style comments,
%	so you need to use the `--no-ansi' option to mgnuc.
%
% To do:
%
%	Improve the interface to the catalog functions.
%
%	Add a nicer interface so the user does not need to manipulate 
%	SQL strings.
% 
%-----------------------------------------------------------------------------%
%
:- module odbc.
%
%-----------------------------------------------------------------------------%

:- interface.

:- import_module io, list, std_util.

%-----------------------------------------------------------------------------%

	% Predicates and types for transaction processing.	

:- type odbc__data_source	== string.
:- type odbc__user_name		== string.
:- type odbc__password		== string.

		% A closure to be executed atomically. 
:- type odbc__transaction(T)	== 	pred(T, odbc__state, odbc__state).
:- mode odbc__transaction	::	pred(out, di, uo) is det.

:- type odbc__state.

	% odbc__transaction(Source, UserName, Password, Transaction, Result).
	%
	% Open a connection to `Source' using the given `UserName'
	% and `Password', perform `Transaction' within a transaction
	% using that connection, then close the connection.
	%
	% `Result' is `ok(Results) - Messages' if the transaction
	% succeeds or `error - Messages' if the transaction is aborted.
	% Whether updates are rolled back if the transaction aborts depends
	% on the database. MySQL will not roll back updates.
	%
	% If `Transaction' throws an exception, odbc__transaction will
	% attempt to roll back the transaction, and will then rethrow
	% the exception to the caller.
:- pred odbc__transaction(odbc__data_source, odbc__user_name, odbc__password,
		odbc__transaction(T), odbc__result(T), io__state, io__state).
:- mode odbc__transaction(in, in, in, odbc__transaction, out, di, uo) is det.

	% Abort the current transaction, returning the given error message.
:- pred odbc__rollback(string, odbc__state, odbc__state).
:- mode odbc__rollback(in, di, uo) is erroneous.

%-----------------------------------------------------------------------------%

	% Predicates and types for execution of SQL statements.

:- type odbc__row	==	list(odbc__attribute).

:- type odbc__attribute
	--->	null		% SQL NULL value
	;	int(int)	
	;	string(string)
	;	float(float)
	;	time(string).	% Time string: "YYYY-MM-DD hh:mm:ss.mmm"

	% The odbc__state arguments threaded through these predicates
	% enforce the restriction that database activity can only occur 
	% within a transaction, since odbc__states are only available 
	% to the closure called by odbc__transaction/5.

	% Execute an SQL statement which doesn't return any results, such
	% as DELETE.
:- pred odbc__execute(string, odbc__state, odbc__state).
:- mode odbc__execute(in, di, uo) is det.

	% Execute an SQL statement, returning a list of results in the 
	% order they are returned from the database. 
:- pred odbc__solutions(string, list(odbc__row), odbc__state, odbc__state).
:- mode odbc__solutions(in, out, di, uo) is det.

	% Execute an SQL statement, applying the accumulator predicate
	% to each element of the result set as it is returned from
	% the database.
:- pred odbc__aggregate(string, pred(odbc__row, T, T), T, T, 
		odbc__state, odbc__state).
:- mode odbc__aggregate(in, pred(in, in, out) is det, in, out, di, uo) is det.
:- mode odbc__aggregate(in, pred(in, di, uo) is det, di, uo, di, uo) is det.

%-----------------------------------------------------------------------------%

	% Predicates and types to get information about database tables.
	% This is very incomplete, it would be nice to be able to get 
	% information about the columns in a table and about privileges 
	% for tables and columns.

:- type odbc__source_desc
	--->	odbc__source_desc(
			odbc__data_source,	% name
			string			% description
		).

:- type odbc__search_pattern
	--->	any
	;	pattern(string).	% _ matches any single character
					% % matches a sequence of characters

	% Information about a table accessible by a transaction.
:- type odbc__table_desc
	--->	odbc__table_desc(
			string,			% table qualifier
			string,			% table owner
			string,			% table name
			string,			% table type
			string,			% description
			list(odbc__attribute)	% data source specific columns
		).

	% Get a list of all the available data sources.
	% Note that iODBC 2.12 doesn't implement this.
:- pred odbc__data_sources(odbc__result(list(odbc__source_desc)), 
		io__state, io__state).
:- mode odbc__data_sources(out, di, uo) is det.

	% odbc__tables(QualifierPattern, OwnerPattern, 
	% 	TableNamePattern, Result)
	% 
	% Get a list of database tables matching the given description.
	% Note that wildcards are not allowed in the QualifierPattern.
	% This is fixed in ODBC 3.0.
:- pred odbc__tables(odbc__search_pattern, odbc__search_pattern,
		odbc__search_pattern, list(odbc__table_desc),
		odbc__state, odbc__state).
:- mode odbc__tables(in, in, in, out, di, uo) is det.

%-----------------------------------------------------------------------------%

	% The following types are used to return status and error 
	% information from ODBC calls.

:- type odbc__result == pair(odbc__status, list(odbc__message)).

:- type odbc__status
	--->	ok
	;	error.

	% The message list returned from a transaction contains all errors 
	% and warnings reported by the driver during the transaction in
	% the order that they were reported.
:- type odbc__result(T) == pair(odbc__status(T), list(odbc__message)).

:- type odbc__status(T)
	--->	ok(T)
	;	error.

:- type odbc__message == pair(odbc__message_type, string).

:- type odbc__message_type
	--->	warning(odbc__warning)
	;	error(odbc__error).

:- type odbc__warning
	--->	disconnect_error
	;	fractional_truncation
	;	general_warning
	;	null_value_in_set_function
	;	privilege_not_revoked
	;	privilege_not_granted
	;	string_data_truncated
	.

:- type odbc__error
	--->	connection_error(odbc__connection_error)
	;	execution_error(odbc__execution_error)
	;	feature_not_implemented
	;	general_error
	;	internal_error
	;	timeout_expired
	;	transaction_error(odbc__transaction_error)
	;	user_requested_rollback
	.

:- type odbc__connection_error
	--->	unable_to_establish
	;	invalid_authorization
	;	connection_name_in_use
	;	nonexistent_connection
	;	connection_rejected_by_server
	;	connection_failure
	;	timeout_expired
	.

:- type odbc__execution_error
	--->	column_already_exists
	;	column_not_found
	;	division_by_zero
	;	general_error
	;	incorrect_count_field
	;	incorrect_derived_table_arity
	;	index_already_exists
	;	index_not_found
	;	integrity_constraint_violation
	;	interval_field_overflow
	;	invalid_cast_specification
	;	invalid_date_time
	;	invalid_escape
	;	invalid_insert_value_list
	;	invalid_schema_name
	;	invalid_use_of_default_parameter
	;	length_mismatch_in_string_data
	;	no_default_for_column
	;	overflow
	;	range_error
	;	restricted_data_type_violation
	;	string_data_length_mismatch
	;	string_data_truncated
	;	syntax_error_or_access_violation
	;	table_or_view_already_exists
	;	table_or_view_not_found
	.

:- type odbc__transaction_error
	--->	rolled_back
	;	still_active
	;	serialization_failure
	;	invalid_state
	.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
:- implementation.

:- import_module assoc_list, exception, int, require, std_util, string.

%-----------------------------------------------------------------------------%

	% We don't actually store anything in the odbc__state, since that
	% would make the exception handling more inconvenient and error-prone.
	% The odbc__state would have to be stored in a global anyway just 
	% before calling longjmp.
	% All the data related to a transaction (ODBC handles, error messages)
	% is stored in the global variables defined below.
:- type odbc__state == unit.

:- pragma c_header_code("

#include ""mercury_imp.h""
#include <stdio.h>
#include <stdlib.h>
#include <setjmp.h>
#include <assert.h>

	/*
	** odbc.m allocates memory within may_call_mercury pragma C code, 
	** which is a bit dodgy in non-conservative GC grades.
	** Allowing non-convservative GC grades would require a bit of fairly 
	** error-prone code to save/restore the heap pointer in the right 
	** places. When accurate garbage collection is implemented and a 
	** nicer way of allocating heap space from within C code is available, 
	** this should be revisited.
	** Conservative garbage collection also makes restoring the state
	** after an exception a bit simpler.
	*/	
#ifndef MR_CONSERVATIVE_GC
#error ""The OBDC interface requires conservative garbage collection. \\
		Use a compilation grade containing .gc.""
#endif /* ! MR_CONSERVATIVE_GC */

#ifdef MODBC_IODBC

#include ""isql.h""
#include ""isqlext.h""
/* #include ""odbc_funcs.h"" */
#include ""sqltypes.h""

	/*
	** Again, iODBC 2.12 doesn't define this, so define it to something
	** harmless.
	*/
#ifndef SQL_NO_DATA
#define SQL_NO_DATA SQL_NO_DATA_FOUND
#endif

#endif /* MODBC_IODBC */

#ifdef MODBC_MS

	/*
	** ODBC_VER set to 0x0250 means that this uses only ODBC 2.0 
	** functionality but compiles with the ODBC 3.0 header files.
	*/
#define ODBC_VER 0x0250

/*
** The following is needed to allow the Microsoft headers to
** compile with GNU C under gnu-win32.
*/

#if defined(__GNUC__) && !defined(__stdcall)
  #define __stdcall __attribute__((stdcall))
#endif

#if defined(__CYGWIN32__) && !defined(WIN32)
  #define WIN32 1
#endif

#include <windows.h>
#include ""sql.h""
#include ""sqlext.h""
#include ""sqltypes.h""

#endif /* MODBC_MS */

	/*
	** Assert the implication: a => b
	*/
#define MR_ASSERT_IMPLY(a,b)	MR_assert( !(a) || (b) )

	/*
	** All integers get converted to long by the driver, then to Integer.
	** All floats get converted to double by the driver, then to Float.
	*/
typedef long 			MODBC_C_INT;
typedef double			MODBC_C_FLOAT;

	/*
	** Define some wrappers around setjmp and longjmp for exception 
	** handling. We need to use MR_setjmp and MR_longjmp because we'll 
	** be longjmping across C->Mercury calls, so we need to restore 
	** some state in runtime/engine.c.
	** Beware: the Mercury registers must be valid when odbc_catch 
	** is called. odbc_throw will clobber the general-purpose registers
	** r1, r2, etc. 
	*/
#define odbc_catch(longjmp_label)					\
			MR_setjmp(&odbc_trans_jmp_buf, longjmp_label)

#define odbc_throw() 	MR_longjmp(&odbc_trans_jmp_buf)

	/* 
	** odbc_trans_jmp_buf stores information saved by odbc_catch (setjmp)
	** to be used by odbc_throw (longjmp) when a database exception is 
	** found.
	*/
static MR_jmp_buf odbc_trans_jmp_buf;

	/*
	** odbc_env_handle is the output of SQLAllocEnv. SQLAllocEnv must
	** be called before attempting to open any connections.	
	*/
static SQLHENV	odbc_env_handle = SQL_NULL_HENV;	

	/* The connection being acted on by the current transaction.	*/
static SQLHDBC	odbc_connection = SQL_NULL_HDBC;

	/* The last return code from an ODBC system call. */
static SQLRETURN odbc_ret_code = SQL_SUCCESS;		

	/* 
	** The list of accumulated warnings and errors for the transaction 
	** in reverse order.
	*/
static Word	odbc_message_list;

static void odbc_transaction_c_code(Word type_info, Word Connection, 
			Word Closure, Word *Results, Word *GotMercuryException,
			Word *Exception, Word *Status, 
			Word *Msgs, Word IO0, Word *IO);
static MR_bool odbc_check(SQLHENV, SQLHDBC, SQLHSTMT, SQLRETURN);

").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

odbc__transaction(Source, User, Password, Closure, Result) -->
	% We could have separate open and close connection predicates in the 
	% interface, but that would just be more effort for the programmer 
	% for a very minor efficiency gain. The connection time will be 
	% insignificant for even trivial queries.
	odbc__open_connection(Source, User, Password, 
		ConnectStatus - ConnectMessages),
	(
		{ ConnectStatus = ok(Connection) },

		% Do the transaction.
		odbc__transaction_2(Connection, Closure, Data, 
			GotMercuryException, Exception, Status, RevMessages),
		{ list__reverse(RevMessages, TransMessages) },

		odbc__close_connection(Connection,
			CloseStatus - CloseMessages),

		%
		% Pass on any exception that was found while
		% processing the transaction.
		%
		( { GotMercuryException = 1 } ->
			{ rethrow(exception(Exception)) }
		;
			[]
		),

		{ list__condense(
			[ConnectMessages, TransMessages, CloseMessages], 
			Messages) },
		( { odbc__ok(Status), CloseStatus = ok } ->
			{ Result = ok(Data) - Messages }
		;
			{ Result = error - Messages }
		)
	;
		{ ConnectStatus = error },
		{ Result = error - ConnectMessages }
	).

:- pred odbc__transaction_2(odbc__connection,
		pred(T, odbc__state, odbc__state), T, 
		int, univ, int, list(odbc__message), io__state, io__state).  
:- mode odbc__transaction_2(in, pred(out, di, uo) is det, 
		out, out, out, out, out, di, uo) is det.

:- pragma c_code(
		odbc__transaction_2(Connection::in, 
			Closure::pred(out, di, uo) is det,
			Results::out, GotMercuryException::out, Exception::out,
			Status::out, Msgs::out, IO0::di, IO::uo),
		may_call_mercury,
"
	/*
	** The Mercury registers must be valid at the call to odbc_catch
	** in odbc_transaction_c_code().
	** odbc_transaction_c_code() may clobber the Mercury general-purpose 
	** registers r1, r2, ..., but that is OK, because this C code is 
	** declared as 'may_call_mercury', so the compiler assumes that it 
	** is allowed to clobber those registers.
	*/
	save_transient_registers();
	odbc_transaction_c_code(TypeInfo_for_T, Connection, Closure, 
			&Results, &GotMercuryException, &Exception, 
			&Status, &Msgs, IO0, &IO);
	restore_transient_registers();
").


:- pragma c_code(
"
static void 
odbc_transaction_c_code(Word TypeInfo_for_T, Word Connection, 
		Word Closure, Word *Results, Word *GotMercuryException,
		Word *Exception, Word *Status, Word *Msgs, Word IO0, Word *IO)
{
	Word DB0 = (Word) 0;
	Word DB = (Word) 0;
	SQLRETURN rc;

	restore_transient_registers();

	/*
	** Mercury state to restore on rollback. 
	*/

	odbc_connection = (SQLHDBC) Connection;
	odbc_message_list = MR_list_empty();

	/*
	** Set up a location to jump to on a database exception.
	** The Mercury registers must be valid here.
	*/	
	odbc_catch(transaction_error);

	/*
	** Anything changed between the call to odbc_catch() and the call to
	** MODBC_odbc__do_transaction() must be declared volatile.
	*/

	MODBC_odbc__do_transaction(TypeInfo_for_T, Closure,
		GotMercuryException, Results, Exception, DB0, &DB);

	/*
	** MR_longjmp() cannot be called after here.
	*/

	if (*GotMercuryException == 0) {

		rc = SQLTransact(odbc_env_handle, odbc_connection, SQL_COMMIT);

		if (! odbc_check(odbc_env_handle, odbc_connection, 
				SQL_NULL_HSTMT, rc)) {
			goto transaction_error;
		}

	} else {

		/*
		** There was a Mercury exception -- abort the transaction.
		** The return value of the call to SQLTransact() is
		** ignored because the caller won't look at the result --
		** it will just rethrow the exception.
		*/
		DEBUG(printf(
			""Mercury exception in transaction: aborting\\n""));
		(void) SQLTransact(odbc_env_handle,
				odbc_connection, SQL_ROLLBACK);
	}
		
	*Status = SQL_SUCCESS;

	goto transaction_done;

transaction_error:

	/*
	** Make the database rollback the transaction if it
	** hasn't already. 
	*/

	*Status = odbc_ret_code;

	*GotMercuryException = 0;

	rc = SQLTransact(odbc_env_handle, odbc_connection, SQL_ROLLBACK);

	odbc_check(odbc_env_handle, odbc_connection, SQL_NULL_HSTMT, rc);

	/* Fall through. */

transaction_done:

	*Msgs = odbc_message_list;
	odbc_message_list = MR_list_empty();
	odbc_connection = SQL_NULL_HDBC;
	odbc_ret_code = SQL_SUCCESS;
	*IO = IO0;

	save_transient_registers();
}
").

%-----------------------------------------------------------------------------%

	% Call the transaction closure.
:- pred odbc__do_transaction(odbc__transaction(T), int, T, univ,
		odbc__state, odbc__state).
:- mode odbc__do_transaction(odbc__transaction,
		out, out, out, di, uo) is cc_multi.

:- pragma export(odbc__do_transaction(odbc__transaction,
		out, out, out, di, uo), 
		"MODBC_odbc__do_transaction").

odbc__do_transaction(Closure, GotException, Results,
		Exception, State0, State) :-
	try((pred(TryResult::out) is det :-
		unsafe_promise_unique(State0, State1),
		Closure(Result, State1, ResultState),
		TryResult = Result - ResultState
	), ExceptResult),
	(
		ExceptResult = succeeded(Results - State2),
		unsafe_promise_unique(State2, State),
		make_dummy_value(Exception),
		GotException = 0
	;
		ExceptResult = exception(Exception),
		make_dummy_value(Results),
		unsafe_promise_unique(State0, State),
		GotException = 1
	).

	% Produce a value which is never looked at, for returning
	% discriminated unions to C.
:- pred make_dummy_value(T::out) is det.
:- pragma c_code(make_dummy_value(T::out),
		[will_not_call_mercury, thread_safe],
		"T = 0;").

%-----------------------------------------------------------------------------%

odbc__rollback(Error) -->
	odbc__add_message(error(user_requested_rollback) - Error),
	odbc__throw.

:- pred odbc__add_message(odbc__message, odbc__state, odbc__state).
:- mode odbc__add_message(in, di, uo) is det.

:- pragma c_code(odbc__add_message(Error::in, DB0::di, DB::uo),
		will_not_call_mercury,
"
{
	odbc_message_list = MR_list_cons(Error, odbc_message_list);
	DB = DB0;
}
").

:- pred odbc__throw(odbc__state, odbc__state).
:- mode odbc__throw(di, uo) is erroneous.

:- pragma c_code(odbc__throw(DB0::di, DB::uo),
		will_not_call_mercury,
"
{
	odbc_ret_code = SQL_ERROR;
	odbc_throw();
	/* DB = DB0; (not reached) */
}
").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Predicates and types to manage connections.

:- type odbc__connection.	% A connection to a specific source.

	% Given the data source to connect to and a user name and password,
	% open a connection.
:- pred odbc__open_connection(odbc__data_source, odbc__user_name, 
		odbc__password, odbc__result(odbc__connection), 
		io__state, io__state).
:- mode odbc__open_connection(in, in, in, out, di, uo) is det.

	% Close the connection to the given data source.
:- pred odbc__close_connection(odbc__connection, odbc__result,
		io__state, io__state).
:- mode odbc__close_connection(in, out, di, uo) is det.

%-----------------------------------------------------------------------------%

:- type odbc__connection == c_pointer.

odbc__open_connection(Source, User, Password, Result - Messages) -->
	odbc__do_open_connection(Source, User, Password, Handle,
		ConnectStatus, RevMessages),
	{ list__reverse(RevMessages, Messages) },
	( { odbc__ok(ConnectStatus) } ->
		{ Result = ok(Handle) }
	;
		{ Result = error }
	).

%-----------------------------------------------------------------------------%

:- pred odbc__do_open_connection(string, string, string,
		odbc__connection, int, list(odbc__message), 
		io__state, io__state).
:- mode odbc__do_open_connection(in, in, in, uo, out, out, di, uo) is det.

:- pragma c_code(
		odbc__do_open_connection(Source::in, User::in, Password::in,
			Handle::uo, Status::out, Messages::out,
			IO0::di, IO::uo),
		may_call_mercury, 
"
{
	SQLHDBC connect_handle;


	if (odbc_env_handle == SQL_NULL_HENV) {
		Status = SQLAllocEnv(&odbc_env_handle);
	} else {
		Status = SQL_SUCCESS;
	}

	DEBUG(printf(""SQLAllocEnv status: %d\\n"", (int) Status));

	if (odbc_check(odbc_env_handle, SQL_NULL_HDBC, 
			SQL_NULL_HSTMT, Status)) {

		Status = SQLAllocConnect(odbc_env_handle, &connect_handle);

		DEBUG(printf(""SQLAllocConnect status: %d\\n"", (int) Status));

		if (odbc_check(odbc_env_handle, connect_handle,
				SQL_NULL_HSTMT, Status)) {
			/* Put the connection into manual commit mode */
			Status = SQLSetConnectOption(connect_handle,
				SQL_AUTOCOMMIT, SQL_AUTOCOMMIT_OFF);

			DEBUG(printf(""manual commit status: %d\\n"", 
					(int) Status));

			odbc_check(odbc_env_handle, connect_handle,
				SQL_NULL_HSTMT, Status);
		}
	}

	Status = SQLConnect(connect_handle, 
			(UCHAR *)Source, strlen(Source),
			(UCHAR *)User, strlen(User),
			(UCHAR *)Password, strlen(Password));

	DEBUG(printf(""connect status: %d\\n"", (int) Status));

	odbc_check(odbc_env_handle, connect_handle, SQL_NULL_HSTMT, Status);

	Messages = odbc_message_list;
	odbc_message_list = MR_list_empty();

	Handle = (Word) connect_handle;
	odbc_connection = SQL_NULL_HDBC;
	IO = IO0;
}
").

%-----------------------------------------------------------------------------%

odbc__close_connection(Connection, Result) -->
	odbc__do_close_connection(Connection, Status, RevMessages),
	{ list__reverse(RevMessages, Messages) },
	( { Status = 0 } ->
		{ Result = ok - Messages }
	;
		{ Result = error - Messages }
	).

:- pred odbc__do_close_connection(odbc__connection, int, 
		list(odbc__message), io__state, io__state).
:- mode odbc__do_close_connection(in, out, out, di, uo) is det.

:- pragma c_code(
		odbc__do_close_connection(Handle::in, Status::out,
			Messages::out, IO0::di, IO::uo),
		may_call_mercury,
"

	Status = SQLDisconnect((SQLHDBC) Handle);
	if (odbc_check(odbc_env_handle, (SQLHDBC) Handle, 
			SQL_NULL_HSTMT, Status)) {
		Status = SQLFreeConnect((SQLHDBC) Handle);
		odbc_check(odbc_env_handle, (SQLHDBC) Handle, 
			SQL_NULL_HSTMT, Status);
	}


	Messages = odbc_message_list;
	odbc_message_list = MR_list_empty();

	IO = IO0;
").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

odbc__execute(SQLString) -->
	odbc__alloc_statement(Statement0),
	odbc__execute_statement(SQLString, Statement0, Statement),
	odbc__cleanup_statement_check_error(Statement).
	
odbc__solutions(SQLString, Results) -->
	% XXX optimize this when we have better support 
	% for last call optimization.
	odbc__do_aggregate(odbc__execute_statement(SQLString), 
		cons, [], Results0),
	{ list__reverse(Results0, Results) }.

odbc__aggregate(SQLString, Accumulator, Acc0, Acc) -->
	odbc__do_aggregate(odbc__execute_statement(SQLString), 
		Accumulator, Acc0, Acc).

:- pred cons(T, list(T), list(T)).
:- mode cons(in, in, out) is det.

cons(H, T, [H|T]).

%-----------------------------------------------------------------------------%

:- pred odbc__do_aggregate(pred(odbc__statement, odbc__statement, 
			odbc__state, odbc__state), pred(odbc__row, T, T),
			T, T, odbc__state, odbc__state).
:- mode odbc__do_aggregate(pred(di, uo, di, uo) is det, 
			pred(in, in, out) is det,
			in, out, di, uo) is det.
:- mode odbc__do_aggregate(pred(di, uo, di, uo) is det, 
			pred(in, di, uo) is det,
			di, uo, di, uo) is det.

odbc__do_aggregate(Execute, Accumulate, Result0, Result) -->
	odbc__alloc_statement(Statement0),
	call(Execute, Statement0, Statement1),
	odbc__bind_columns(Statement1, Statement2),
	odbc__get_rows(Accumulate, Result0, Result, Statement2, Statement),
	odbc__cleanup_statement_check_error(Statement).

%-----------------------------------------------------------------------------%

	% Get the set of result rows from the statement.
:- pred odbc__get_rows(pred(odbc__row, T, T), T, T, 
		odbc__statement, odbc__statement, odbc__state, odbc__state).
:- mode odbc__get_rows(pred(in, in, out) is det, in, out, 
		di, uo, di, uo) is det.
:- mode odbc__get_rows(pred(in, di, uo) is det, di, uo, di, uo, di, uo) is det.

odbc__get_rows(Accumulate, Result0, Result, Statement0, Statement) -->
	odbc__get_number_of_columns(NumColumns, Statement0, Statement1),
	odbc__get_rows_2(NumColumns, Accumulate, Result0, Result, 
		Statement1, Statement).

:- pred odbc__get_rows_2(int, pred(odbc__row, T, T), T, T,
		odbc__statement, odbc__statement, odbc__state, odbc__state).
:- mode odbc__get_rows_2(in, pred(in, in, out) is det, in, out,
		di, uo, di, uo) is det.
:- mode odbc__get_rows_2(in, pred(in, di, uo) is det, di, uo,
		di, uo, di, uo) is det.

odbc__get_rows_2(NumColumns, Accumulate, Result0, Result,
		Statement0, Statement) -->
	% Try to fetch a new row.
	odbc__fetch(Statement0, Statement1, Status),
	( { odbc__no_data(Status) } ->
		{ Result = Result0 },	
		{ Statement = Statement1 }
	;
		odbc__get_attributes(1, NumColumns, Row, 
			Statement1, Statement2),
		{ Accumulate(Row, Result0, Result1) },
		odbc__get_rows_2(NumColumns, Accumulate, 
			Result1, Result, Statement2, Statement)
	).

%-----------------------------------------------------------------------------%

	% Get the values from the current fetched row.
:- pred odbc__get_attributes(int, int, list(odbc__attribute), 
		odbc__statement, odbc__statement,
		odbc__state, odbc__state).
:- mode odbc__get_attributes(in, in, out, di, uo, di, uo) is det.

odbc__get_attributes(CurrCol, NumCols, Row, Statement0, Statement) -->
	( { CurrCol =< NumCols } ->
		{ NextCol is CurrCol + 1 },
		odbc__get_attribute(CurrCol, Attribute, Statement0, Statement1),
		odbc__get_attributes(NextCol, NumCols, Row1, 
			Statement1, Statement),
		{ Row = [Attribute | Row1] }
	;
		{ Row = [] },
		{ Statement = Statement0 }
	).

%-----------------------------------------------------------------------------%

	% Get the value of a column in the current fetched row.
:- pred odbc__get_attribute(int, odbc__attribute, odbc__statement,
		odbc__statement, odbc__state, odbc__state) is det.
:- mode odbc__get_attribute(in, out, di, uo, di, uo) is det.

odbc__get_attribute(NumColumn, Value, Statement0, Statement) -->
	odbc__get_data(NumColumn, Int, Float, String, TypeInt, 
		Statement0, Statement), 
	{ odbc__int_to_attribute_type(TypeInt, Type) },
	(
		{ Type = null },
		{ Value = null }
	;
		{ Type = string },
		{ Value = string(String) }
	;
		{ Type = time },
		{ Value = time(String) }
	; 
		{ Type = int },
		{ Value = int(Int) }
	;
		{ Type = float },
		{ Value = float(Float) }
	).

%-----------------------------------------------------------------------------%

:- type odbc__attribute_type
	--->	int
	;	float
	;	time
	;	string
	;	null.

:- pred odbc__int_to_attribute_type(int, odbc__attribute_type).
:- mode odbc__int_to_attribute_type(in, out) is det.

odbc__int_to_attribute_type(Int, Type) :-
	( odbc__int_to_attribute_type_2(Int, Type1) ->
		Type = Type1
	;
		error("odbc__int_to_attribute_type: invalid type")
	).

	% Keep this in sync with the C enum MODBC_AttrType below.
:- pred odbc__int_to_attribute_type_2(int, odbc__attribute_type).
:- mode odbc__int_to_attribute_type_2(in, out) is semidet.

odbc__int_to_attribute_type_2(0, int).
odbc__int_to_attribute_type_2(1, float).
odbc__int_to_attribute_type_2(2, time).
odbc__int_to_attribute_type_2(3, string).
odbc__int_to_attribute_type_2(4, string).
odbc__int_to_attribute_type_2(5, null).

%-----------------------------------------------------------------------------%

:- type odbc__statement	== c_pointer.

:- pragma c_header_code("

	/*
	** Notes on memory allocation:
	**
	** C data structures (MODBC_Statement and MODBC_Column) are allocated 
	** using MR_GC_malloc/MR_GC_free.
	**
	** MODBC_Statement contains a statement handle which must be freed 
	** using SQLFreeStmt.
	**
	** Variable length data types are collected in chunks allocated on
	** the Mercury heap using incr_hp_atomic. The chunks are then condensed
	** into memory allocated on the Mercury heap using string__append_list.
	** XXX this may need revisiting when accurate garbage collection 
	** is implemented to make sure the collector can see the data when
	** it is stored within a MODBC_Column.
	**
	** Other data types have a buffer which is allocated once using
	** MR_GC_malloc.
	*/

	/*
	** If the driver can't work out how much data is in a blob in advance,
	** get the data in chunks. The chunk size is fairly arbitrary.
	** MODBC_CHUNK_SIZE must be a multiple of sizeof(Word).
	*/
#define MODBC_CHUNK_WORDS	1024
#define MODBC_CHUNK_SIZE	(MODBC_CHUNK_WORDS * sizeof(Word))

typedef enum {
	MODBC_INT	= 0,	/* Word-sized Integer */
	MODBC_FLOAT	= 1,	/* Mercury Float */
	MODBC_TIME	= 2,	/* time and/or date converted to a string */
	MODBC_STRING	= 3,	/* string, or type converted to a string */
	MODBC_VAR_STRING = 4,	/* string with no maximum length */
	MODBC_NULL	= 5
} MODBC_AttrType;

typedef enum { MODBC_BIND_COL, MODBC_GET_DATA } MODBC_BindType;

	/* Information about a column in a result set. */
typedef struct {
		size_t		size;		/* size of allocated buffer */
		MODBC_AttrType	attr_type;
		SWORD		sql_type;	/*
						** the actual type, 	   
						** e.g. SQL_LONG_VAR_CHAR 
						*/
		SWORD		conversion_type;/*
						** the type the data is 
						** being converted into,
						** e.g SQL_C_CHAR
						*/
		SDWORD		value_info;	/* 
						** size of returned data,
						** or SQL_NULL_DATA
						*/
		Word 		*data;
} MODBC_Column;


	/* Information about a result set. */
typedef struct {
		SQLHSTMT	stat_handle;	/* statement handle 	  */
		int		num_columns;	/* columns per row 	  */
		MODBC_Column	*row;		/* 
						** array of columns in 
						** the current row
						*/
		int 		num_rows;	/* number of fetched rows */ 
		MODBC_BindType	binding_type;	/* 
						** are we using SQL_BIND_COL
						** or SQL_GET_DATA
						*/
} MODBC_Statement;

static SQLRETURN odbc_do_cleanup_statement(MODBC_Statement *stat);
static size_t sql_type_to_size(SWORD sql_type, UDWORD cbColDef, 
		SWORD ibScale, SWORD fNullable);
static MODBC_AttrType sql_type_to_attribute_type(SWORD sql_type);
static SWORD attribute_type_to_sql_c_type(MODBC_AttrType AttrType);
static MR_bool is_variable_length_sql_type(SWORD);
void odbc_do_get_data(MODBC_Statement *stat, int column_id);
void odbc_get_data_in_chunks(MODBC_Statement *stat, int column_id);
void odbc_get_data_in_one_go(MODBC_Statement *stat, int column_id);
").

%-----------------------------------------------------------------------------%

:- pred odbc__alloc_statement(odbc__statement, odbc__state, odbc__state).
:- mode odbc__alloc_statement(uo, di, uo) is det.

:- pragma c_code(odbc__alloc_statement(Statement::uo, DB0::di, DB::uo),
		may_call_mercury,
"
{
	MODBC_Statement *statement;
	SQLRETURN rc;


		/* Doing manual deallocation of the statement object. */
	statement = MR_GC_NEW(MODBC_Statement);
		
	statement->num_columns = 0;
	statement->row = NULL;
	statement->num_rows = 0;
	statement->stat_handle = SQL_NULL_HSTMT;

	rc = SQLAllocStmt(odbc_connection, &(statement->stat_handle));
	if (! odbc_check(odbc_env_handle, odbc_connection,
			statement->stat_handle, rc)) 
	{
		odbc_throw();
		/* not reached */
	}

	MR_assert(statement->stat_handle != SQL_NULL_HSTMT);

	DB = DB0;
	Statement = (Word) statement;

}
").

%-----------------------------------------------------------------------------%

:- pred odbc__execute_statement(string, odbc__statement, odbc__statement,
		odbc__state, odbc__state).
:- mode odbc__execute_statement(in, di, uo, di, uo) is det.

:- pragma c_code(
		odbc__execute_statement(SQLString::in, Statement0::di,
			Statement::uo, DB0::di, DB::uo),
		may_call_mercury,
"
{
	MODBC_Statement *statement = (MODBC_Statement *) Statement0;
	SQLRETURN rc;
	SQLHSTMT stat_handle = statement->stat_handle;

	DEBUG(printf(""executing SQL string: %s\\n"", SQLString));

	rc = SQLPrepare(stat_handle, SQLString, strlen(SQLString));

	if (! odbc_check(odbc_env_handle, odbc_connection, stat_handle, rc)) {

		/*
		** We don't check the return status of this because 
		** the programmer is likely to be more interested 
		** in the earlier error.
		*/
		odbc_do_cleanup_statement(statement);
		odbc_throw();
		/* not reached */
	}

	rc = SQLExecute(stat_handle);
	if (! odbc_check(odbc_env_handle, odbc_connection, stat_handle, rc)) {
		odbc_do_cleanup_statement(statement);
		odbc_throw();
		/* not reached */
	}

	DEBUG(printf(""execution succeeded\\n""));

	Statement = (Word) statement;
	DB = DB0;

}").

%-----------------------------------------------------------------------------%

	%
	% There are two methods to get data back from an ODBC application.
	%
	% One involves binding a buffer to each column using SQLBindCol,
	% then calling SQLFetch repeatedly to read rows into the buffers.
	% The problem with this method is it doesn't work with variable 
	% length data, since if the data doesn't fit into the allocated
	% buffer it gets truncated and there's no way to have a second
	% try with a larger buffer.
	%
	% The other method is to not bind any columns. Instead, after
	% SQLFetch is called to update the cursor, SQLGetData is used
	% on each column to get the data. SQLGetData can be called repeatedly
	% to get all the data if it doesn't fit in the buffer. The problem 
	% with this method is that it requires an extra ODBC function call 
	% for each attribute received, which may have a significant impact 
	% on performance if the database is being accessed over a network.
	% 
	% Hybrid methods are also possible if all the variable length columns
	% come after the fixed length columns in the result set, but that 
	% is probably overkill. (SQLGetData can only be used on columns 
	% after those bound using SQLBindCol).
	%
	% The first method is used if there are no variable length columns,
	% otherwise the second method is used.
	%
:- pred odbc__bind_columns(odbc__statement, odbc__statement,
		odbc__state, odbc__state).
:- mode odbc__bind_columns(di, uo, di, uo) is det.

:- pragma c_code(odbc__bind_columns(Statement0::di, Statement::uo, 
		DB0::di, DB::uo), may_call_mercury,
"{ 
	int 		column_no;
	MODBC_Statement *statement;
	SQLSMALLINT 	num_columns;
	MODBC_Column	*column;
	SQLRETURN 	rc;
	SQLHSTMT 	stat_handle;


	statement = (MODBC_Statement *) Statement0;
	stat_handle = statement->stat_handle;

	/*
	** Retrieve number of columns of statement
	*/
	rc = SQLNumResultCols(stat_handle, &num_columns);
	if (! odbc_check(odbc_env_handle, odbc_connection, stat_handle, rc)) {
		odbc_do_cleanup_statement(statement);
		odbc_throw();
		/* not reached */
	} 
	statement->num_columns = num_columns;

	/* 
	** Allocate an array containing the info for each column.
	** The extra column is because ODBC counts columns starting from 1.
	*/
	statement->row = MR_GC_NEW_ARRAY(MODBC_Column, num_columns + 1);

	/*
	** Use SQLBindCol unless there are columns with no set maximum length.
	*/
	statement->binding_type = MODBC_BIND_COL;

	/*
	** Get information about the result set columns.
	** ODBC counts columns from 1. 
	*/
	for (column_no = 1; column_no <= statement->num_columns; column_no++) {

		char		col_name[1]; 	/* Not looked at */
		SWORD		col_name_len;
		SWORD		col_type;
		UDWORD		pcbColDef;
		SWORD		pibScale;
		SWORD		pfNullable;

		column = &(statement->row[column_no]);
		column->size = 0;
		column->data = NULL;

		/*
		** Retrieve the C type of the column.
		** (SQL type mapped to a conversion type).
		** Create an attribute object with room to store the 
		** attribute value.
		*/
			
		rc = SQLDescribeCol(stat_handle, column_no,
				(UCHAR *) col_name, sizeof(col_name), 
				&col_name_len, &col_type, &pcbColDef, 
				&pibScale, &pfNullable);

		/*
		** SQL_SUCCESS_WITH_INFO means there wasn't
		** enough space for the column name, but we
		** aren't collecting the column name anyway.
		*/
		if (rc != SQL_SUCCESS_WITH_INFO && 
				! odbc_check(odbc_env_handle, odbc_connection,
					stat_handle, rc)) 
		{
			odbc_do_cleanup_statement(statement);
			odbc_throw();
			/* not reached */
		}

		column->sql_type = col_type;
		column->size = sql_type_to_size(col_type, pcbColDef,
				pibScale, pfNullable);

		column->attr_type = sql_type_to_attribute_type(col_type);

		/* Request a conversion into one of the supported types. */
		column->conversion_type =
			attribute_type_to_sql_c_type(column->attr_type);

		DEBUG(printf(""Column %i: size %i - sql_type %i - attr_type %i - conversion_type %i\\n"",
			column_no, column->size, column->sql_type,
			column->attr_type, column->conversion_type));
			
		if (is_variable_length_sql_type(col_type)) {
			statement->binding_type = MODBC_GET_DATA;
		} else {
			/* 
			** Do the buffer allocation once for columns which
			** have a fixed maximum length. 
			*/
			column->data = MR_GC_malloc(column->size);
		}
				
	} /* for */

	if (statement->binding_type == MODBC_BIND_COL) {

		for (column_no = 1; column_no <= statement->num_columns; 
				column_no++) {

			DEBUG(printf(""Binding column %d/%d\\n"", 
					column_no, statement->num_columns));
			column = &(statement->row[column_no]);

			rc = SQLBindCol(stat_handle, column_no,
					column->conversion_type, 
					(SQLPOINTER) column->data,
					column->size, &(column->value_info));
			if (! odbc_check(odbc_env_handle, odbc_connection, 
					stat_handle, rc)) 
			{
				odbc_do_cleanup_statement(statement);
				odbc_throw();
				/* not reached */
			}
		}
	}

	Statement = (Word) statement;
	DB = DB0;

} /* odbc__bind_columns */
").

%-----------------------------------------------------------------------------%

	% Fetch the next row of the current statement.
:- pred odbc__fetch(odbc__statement, odbc__statement,
		int, odbc__state, odbc__state).
:- mode odbc__fetch(di, uo, out, di, uo) is det.

:- pragma c_code(odbc__fetch(Statement0::di, Statement::uo,
		Status::out, DB0::di, DB::uo),
		may_call_mercury,
"{
	MODBC_Statement *stat;

	stat = (MODBC_Statement *) Statement0;

	MR_assert(stat != NULL);

	if (stat->num_rows == 0 ) {
		DEBUG(printf(""Fetching rows...\\n""));
	}

	/* Fetching new row */
	Status = SQLFetch(stat->stat_handle);

	if (Status != SQL_NO_DATA_FOUND && 
			! odbc_check(odbc_env_handle, odbc_connection, 
				stat->stat_handle, Status))
	{
		odbc_do_cleanup_statement(stat);
		odbc_throw();
		/* not reached */
	}

	/* Update number of rows fetched */
	if (Status == SQL_SUCCESS) {
		stat->num_rows++;
	}

	if (Status == SQL_NO_DATA_FOUND) {
		DEBUG(printf(""Fetched %d rows\\n"", stat->num_rows));
	}

	Statement = (Word) stat;
	DB = DB0;

}").

%-----------------------------------------------------------------------------%

:- pred odbc__get_number_of_columns(int, odbc__statement, odbc__statement, 
		odbc__state, odbc__state).
:- mode odbc__get_number_of_columns(out, di, uo, di, uo) is det.

:- pragma c_code(odbc__get_number_of_columns(NumColumns::out, Statement0::di,
		Statement::uo, DB0::di, DB::uo), 
		will_not_call_mercury,
"{
	MODBC_Statement * stat;

	stat = (MODBC_Statement *) Statement0;

	MR_assert(stat != NULL);
		
	NumColumns = stat->num_columns;
	DB = DB0;
	Statement = Statement0;
}").

%-----------------------------------------------------------------------------%

:- pred odbc__get_data(int, int, float, string, int, odbc__statement, 
		odbc__statement, odbc__state, odbc__state).
:- mode odbc__get_data(in, out, out, out, out, di, uo, di, uo) is det.

:- pragma c_code(odbc__get_data(Column::in, Int::out, Flt::out, Str::out, 
		Type::out, Statement0::di, Statement::uo, DB0::di, DB::uo),
		may_call_mercury,
"{

	MODBC_Statement *stat;
	MODBC_Column 	*col;
	SQLRETURN 	rc;
	SDWORD		column_info;

	stat = (MODBC_Statement *) Statement0;

	MR_assert(stat != NULL);
	MR_assert(stat->row != NULL);

	DEBUG(printf(""Getting column %i\n"", (int) Column));

	if (stat->binding_type == MODBC_GET_DATA) {

		/* Slurp up the data for this column. */ 
		odbc_do_get_data(stat, Column);
	}

	col = &(stat->row[Column]);

	if (col->value_info == SQL_NULL_DATA) {
		Type = MODBC_NULL;
	} else {
		Type = col->attr_type;
	}

	switch ((int) Type) {

	    case MODBC_NULL:
		break;

	    case MODBC_INT: {

		MODBC_C_INT data = *(MODBC_C_INT *)(col->data);

		Int = (Integer) data;

		DEBUG(printf(""got integer %ld\\n"", (long) Int));

			/* Check for overflow */
		if (Int != data) {
			Word overflow_message;
			MODBC_overflow_message(&overflow_message);
			odbc_message_list =
				MR_list_cons(overflow_message,
					odbc_message_list);
			odbc_do_cleanup_statement(stat);
			odbc_throw();
		}
		break;
	    }

	    case MODBC_FLOAT:

		Flt = (Float) *(MODBC_C_FLOAT *)(col->data);

		DEBUG(printf(""got float %f\\n"", Flt));

		break;

	    case MODBC_STRING:
	    case MODBC_TIME:

		MR_assert(col->data);
		make_aligned_string_copy(Str, (char *) col->data);

		DEBUG(printf(""got string %s\\n"", (char *) Str));

		break;
	
	    case MODBC_VAR_STRING:
		/*
		** The data was allocated on the Mercury heap, 
		** get it then kill the pointer so it can be GC'ed.
		*/
		make_aligned_string(Str, (char *) col->data);

		DEBUG(printf(""got var string %s\\n"", (char *) col->data));

		col->data = NULL;

		/* As far as Mercury is concerned it's an ordinary string */
		Type = MODBC_STRING;
		break;

	    default:
		MR_fatal_error(
			""odbc.m: invalid attribute type in odbc__get_data"");
		break;
	} /* end switch (Type) */

	Statement = (Word) stat;
	DB = DB0;

} /* end odbc__get_data() */
").

:- pragma c_code("

void
odbc_do_get_data(MODBC_Statement *stat, int column_id)
{
	MODBC_Column 	*column;
	SQLRETURN 	rc;
	SDWORD		column_info;
	char 		dummy_buffer[1]; /*
					 ** Room for the NUL termination 
					 ** byte and nothing else.
					 */

	column = &(stat->row[column_id]);
	if (column->attr_type == MODBC_VAR_STRING) {

		/* Just get the length first time through. */
		rc = SQLGetData(stat->stat_handle, column_id,
				column->conversion_type, dummy_buffer, 
				1, &(column->value_info));

		/*
		** SQL_SUCCESS_WITH_INFO is expected here, since
		** we didn't allocate any space for the data, so
		** don't collect the ""data truncated"" message.
		*/
		if (rc != SQL_SUCCESS_WITH_INFO &&
				! odbc_check(odbc_env_handle, 
					odbc_connection, 
					stat->stat_handle, rc)) 
		{
			odbc_do_cleanup_statement(stat);	
			odbc_throw();
		}

		if (column->value_info == SQL_NULL_DATA) {
			/* 
			** The column is NULL, so there is no data to get.
			*/
			return;
		} else if (column->value_info == SQL_NO_TOTAL) {
			/*
			** The driver couldn't work out the length
			** in advance, so get the data in chunks of
			** some arbitrary size, and append the chunks
			** together.
			** This method must be used with MODBC_IODBC,
			** since iODBC-2.12 uses a different interpretation
			** of the ODBC standard to Microsoft, for which
			** the length returned by the first call to SQLGetData
			** above is the minimun of the buffer length and the 
			** length of the available data, rather than the 
			** total length of data available.
			*/
			odbc_get_data_in_chunks(stat, column_id);
		} else { 
			Word data;

			/* 
			** column->value_info == length of data 
			*/
			column->size = column->value_info + 1;
			incr_hp_atomic(LVALUE_CAST(Word, column->data), 
				(column->size + sizeof(Word)) / sizeof(Word));
			odbc_get_data_in_one_go(stat, column_id);
		}
	} else {
		/* 
		** It's a fixed length column, so we can 
		** get the lot in one go.
		*/
		odbc_get_data_in_one_go(stat, column_id);
	}
}

void 
odbc_get_data_in_one_go(MODBC_Statement *stat, int column_id)
{
	MODBC_Column 	*col;
	SQLRETURN 	rc;

	DEBUG(printf(""getting column %i in one go\n"", column_id));

	col = &(stat->row[column_id]);

	rc = SQLGetData(stat->stat_handle, column_id, col->conversion_type, 
			(SQLPOINTER) col->data, col->size, &(col->value_info));

	if (! odbc_check(odbc_env_handle, odbc_connection, 
			stat->stat_handle, rc)) 
	{
		odbc_do_cleanup_statement(stat);	
		odbc_throw();
	}
}

void 
odbc_get_data_in_chunks(MODBC_Statement *stat, int column_id)
{
	MODBC_Column 	*col;
	SQLRETURN 	rc;
	Word		this_bit;
	Word 		chunk_list;
	String		result;

	DEBUG(printf(""getting column %i in chunks\n"", column_id));

	chunk_list = MR_list_empty();

	col = &(stat->row[column_id]);

	rc = SQL_SUCCESS_WITH_INFO;

	incr_hp_atomic(this_bit, MODBC_CHUNK_WORDS);

		/*
		** Keep collecting chunks until we run out.
		*/
	while (rc == SQL_SUCCESS_WITH_INFO) {

		rc = SQLGetData(stat->stat_handle, column_id, 
				col->conversion_type, (SQLPOINTER) this_bit,
				MODBC_CHUNK_SIZE - 1, &(col->value_info));
	
		if (rc == SQL_NO_DATA_FOUND) {
			break;
		}

		if (rc != SQL_SUCCESS_WITH_INFO &&
				! odbc_check(odbc_env_handle, odbc_connection, 
					stat->stat_handle, rc)) 
		{
			odbc_do_cleanup_statement(stat);	
			odbc_throw();
		}

		chunk_list = MR_list_cons(this_bit, chunk_list);
		incr_hp_atomic(this_bit, MODBC_CHUNK_WORDS);
	}

	MODBC_odbc_condense_chunks(chunk_list, &result);
	col->data = (Word *) result;
}
").

:- pred odbc__overflow_message(odbc__message).
:- mode odbc__overflow_message(out) is det.

:- pragma export(odbc__overflow_message(out), 
		"MODBC_overflow_message").

odbc__overflow_message(Error) :-
	Error = error(execution_error(overflow))
		- "[Mercury][odbc.m]Integer overflow detected in result set. Integers must be no larger than a word.".

:- pred odbc__condense_chunks(list(string), string).
:- mode odbc__condense_chunks(in, out) is det.

:- pragma export(odbc__condense_chunks(in, out), "MODBC_odbc_condense_chunks").

odbc__condense_chunks(RevChunks, String) :-
	list__reverse(RevChunks, Chunks),
	string__append_list(Chunks, String).

%-----------------------------------------------------------------------------%

:- pred odbc__cleanup_statement_check_error(odbc__statement,
		odbc__state, odbc__state).
:- mode odbc__cleanup_statement_check_error(di, di, uo) is det.

:- pragma c_code(
	odbc__cleanup_statement_check_error(Statement::di, DB0::di, DB::uo),
	may_call_mercury,
"{
	MODBC_Statement *stat;
	SQLRETURN rc;

	stat = (MODBC_Statement *) Statement;

	rc = odbc_do_cleanup_statement(stat);
	if (! odbc_check(odbc_env_handle, odbc_connection, 
			SQL_NULL_HSTMT, rc)) 
	{
		odbc_throw();
	}	
	DB = DB0;
}").

:- pragma c_code("

static SQLRETURN
odbc_do_cleanup_statement(MODBC_Statement *stat)
{
	int i;
	SQLRETURN rc;

	if (stat != NULL) {
		DEBUG(printf(""cleaning up statement\\n""));
		if (stat->row != NULL) {
		    for (i = 1; i <= stat->num_columns; i++) {
			    /*
			    ** Variable length types are allocated directly
			    ** onto the Mercury heap, so don't free them here. 
			    */
			if (! is_variable_length_sql_type(
				    	stat->row[i].sql_type)) 
			{
				MR_GC_free(stat->row[i].data);
			}
		    }
		    MR_GC_free(stat->row);
		}
		rc = SQLFreeStmt(stat->stat_handle, SQL_DROP);
		MR_GC_free(stat);
		return rc;
	} else {
		return SQL_SUCCESS;
	}
}").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pragma c_code("

/*
**  Map an ODBC SQL type to a supported attribute type.
**  Currently, supported attribute types are minimal,
**  but this function will allow us to ask ODBC to make
**  convertion from SQL types to supported types.
**  Binary types are currently converted to strings. 
*/
static MODBC_AttrType 
sql_type_to_attribute_type(SWORD sql_type)
{
	switch (sql_type) {
		case SQL_BIGINT:		return MODBC_STRING;
		case SQL_BINARY:		return MODBC_STRING;
		case SQL_BIT:			return MODBC_STRING;
		case SQL_CHAR:			return MODBC_STRING;
		case SQL_DATE:			return MODBC_TIME;
		case SQL_DECIMAL:		return MODBC_STRING; /*?*/
		case SQL_DOUBLE:		return MODBC_FLOAT;
		case SQL_FLOAT:			return MODBC_FLOAT;
		case SQL_INTEGER:		return MODBC_INT;

	/* 
	** For MySQL, SQLGetData does not work correctly (multiple calls 
	** return the same data, not successive pieces of the data).
	** It seems to be guaranteed to be able to find the maximum length
	** of the data in the column, so we treat those columns as if
	** they were fixed length. 
	*/
#ifdef MODBC_MYSQL
		case SQL_LONGVARBINARY:		return MODBC_STRING;
		case SQL_LONGVARCHAR:		return MODBC_STRING;
#else /* ! MODBC_MYSQL */
		case SQL_LONGVARBINARY:		return MODBC_VAR_STRING;
		case SQL_LONGVARCHAR:		return MODBC_VAR_STRING;
#endif /* ! MODBC_MYSQL */

		case SQL_NUMERIC:		return MODBC_STRING;
		case SQL_REAL:			return MODBC_FLOAT;
		case SQL_SMALLINT:		return MODBC_INT;
		case SQL_TIME:			return MODBC_TIME;
		case SQL_TIMESTAMP:		return MODBC_TIME;
		case SQL_TINYINT:		return MODBC_INT;
		case SQL_VARBINARY:		return MODBC_STRING;
		case SQL_VARCHAR:		return MODBC_STRING;
		default:
		    MR_fatal_error(
		    	""odbc.m: sql_type_to_attribute_type: unknown type"");
	}
}

/*
** Return the SQL_C type corresponding to a supported attribute type.
*/
static SWORD 
attribute_type_to_sql_c_type(MODBC_AttrType AttrType)
{
	switch (AttrType) {
		case MODBC_FLOAT:		return SQL_C_DOUBLE;
		case MODBC_INT:			return SQL_C_SLONG;
		case MODBC_TIME:		return SQL_C_CHAR;
		case MODBC_STRING:		return SQL_C_CHAR;
		case MODBC_VAR_STRING:		return SQL_C_CHAR;
		default:
			/* Unsuported MODBC_xxx type */
		    MR_fatal_error(
		    	""odbc.m: attribute_type_to_sql_c_type: unknown type"");
	}
}


/*
** Does the data have no maximum length? 
** Note: the implementation of SQLGetData for MySQL does not follow the same 
** standard as SQL Server, but from examination of the sources SQLDescribeCol
** seems guaranteed to find the maximum length of a result column containing 
** variable length data. SQL_NO_TOTAL, which should be returned if the length
** cannot be determined, is not defined by the iODBC header files. 
*/
static MR_bool 
is_variable_length_sql_type(SWORD sql_type) {

#ifdef MODBC_MYSQL

	return MR_FALSE;

#else /* ! MODBC_MYSQL */

	return (
		sql_type == SQL_LONGVARBINARY ||	
		sql_type == SQL_LONGVARCHAR
	);

#endif /* !MODBC_MYSQL */
}

/*
** This function computes to total number of bytes needed
** to store an attribute value, returning -1 if there is no
** maximum size. 
** [SqlType] is the ODBC SQL type of the column
** [cbColDef] is the size returned by SQLDescribeCol
** [ibScaler] is the scale returned by SQLDescribeCol
** [fNullable] is whether the column can be NULL
*/
static size_t 
sql_type_to_size(SWORD sql_type, UDWORD cbColDef, 
		SWORD ibScale, SWORD fNullable)
{
	switch (sql_type)
	{
		/*
		** 64-bit signed int converted to SQL_C_CHAR 
		*/
		case SQL_BIGINT:
			return 1 + cbColDef + 1;  /* +1 for sign, +1 for NUL */

		/*
		** Binary data converted to SQL_C_CHAR 
		** Each byte is converted to 2-digit Hex 
		*/
		case SQL_BINARY:
			return cbColDef * 2 + 1;  /* +1 for NUL */

		/*
		** Bit converted to SQL_C_CHAR 
		*/
		case SQL_BIT:
			return cbColDef + 1;  /* +1 for NUL */

		/*
		** Fixed char to SQL_C_CHAR  
		*/
		case SQL_CHAR:
			return cbColDef + 1;  /* 1 for NUL */

		/*
		** Date YYYY-MM-DD converted to SQL_C_CHAR
		*/
		case SQL_DATE:
			return cbColDef + 1;  /* 1 for NUL */

		/*
		** Signed decimal ddd.dd converted to SQL_C_CHAR
		*/
		case SQL_DECIMAL:
			return 1 + cbColDef + 1 + ibScale + 1; 
			/* 1 for sign 1, 1 for decimal point, 1, for NUL */

		/*
		** 32-bit float converted to MODBC_SQL_C_FLOAT
		*/
		case SQL_DOUBLE:
			return sizeof(MODBC_C_FLOAT);

		/*
		** 32-bit float converted to MODBC_SQL_C_FLOAT
		*/
		case SQL_FLOAT:
			return sizeof(MODBC_C_FLOAT);

		/*
		** 32-bit integer converted to SQL_C_SLONG
		*/
		case SQL_INTEGER:
			return sizeof(MODBC_C_INT);

		/*
		** Any length binary convert to SQL_C_CHAR
		** For MySQL, there are no column types for 
		** which the maximum length cannot be determined before 
		** starting to fetch data, hence the #ifdefs below.
		*/
		case SQL_LONGVARBINARY:
#ifdef MODBC_MYSQL
			return cbColDef * 2 + 1;	/* 1 for NUL */
#else /* !MODBC_MYSQL */
			return -1;
#endif /* !MODBC_MYSQL */

		/*
		** Any length char convert to SQL_C_CHAR
		** For MySQL, there are no column types for 
		** which the maximum length cannot be determined before 
		** starting to fetch data, hence the #ifdefs below.
		*/
		case SQL_LONGVARCHAR:
#ifdef MODBC_MYSQL
			return cbColDef + 1;	/* 1 for NUL */
#else /* !MODBC_MYSQL */
			return -1;
#endif /* !MODBC_MYSQL */

		/*
		** Signed numeric ddd.dd converted to SQL_C_CHAR
		*/
		case SQL_NUMERIC:
			return 1 + cbColDef + 1 + ibScale + 1;
					/* 1 for NUL */

		/*
		** 32-bit float converted to MODBC_SQL_C_FLOAT
		*/
		case SQL_REAL:
			return sizeof(MODBC_C_FLOAT);

		/*
		** 16-bit integer converted to SQL_C_SLONG
		*/
		case SQL_SMALLINT:
			return sizeof(MODBC_C_INT);

		/*
		** Time hh:mm:ss converted to SQL_C_CHAR
		*/
		case SQL_TIME:
			return cbColDef + 1;  /* 1 for NUL */

		/*
		** Time YYYY-MM-DD hh:mm:ss converted to SQL_C_CHAR
		*/
		case SQL_TIMESTAMP:
			return cbColDef + 1;  /* 1 for NUL */

		/*
		** 8-bit integer converted to MODBC_SQL_INT
		*/
		case SQL_TINYINT:
			return sizeof(MODBC_C_INT);

		/*
		** Binary data converted to SQL_C_CHAR 
		** Each byte is converted to 2-digit Hex 
		*/
		case SQL_VARBINARY:
			return cbColDef * 2 + 1;  /* 1 for NUL */

		/*
		** Fixed char to SQL_C_CHAR  
		*/
		case SQL_VARCHAR:
			return cbColDef + 1;  /* 1 for NUL */

		default:
		    MR_fatal_error(""odbc.m: sql_type_to_size: unknown type"");
	}
}
").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	%
	% Catalog functions.
	%

odbc__data_sources(MaybeSources - Messages) -->
	odbc__sql_data_sources(RevSourceNames, RevDescs, Status, RevMessages),
	( { odbc__ok(Status) } ->
		{ list__reverse(RevMessages, Messages) },
		{ list__reverse(RevSourceNames, SourceNames) },
		{ list__reverse(RevDescs, Descs) },
		{ assoc_list__from_corresponding_lists(SourceNames, 
			Descs, SourceAL) },
		{ MakeSource = lambda([Pair::in, SourceDesc::out] is det, (
				Pair = SourceName - Desc,
				SourceDesc = odbc__source_desc(SourceName, Desc)
			)) },
		{ list__map(MakeSource, SourceAL, Sources) },
		{ MaybeSources = ok(Sources) }
	; { odbc__no_data(Status) } ->
		% iODBC 2.12 doesn't implement this function.
		{ Messages = [
			error(feature_not_implemented) - 
			"[Mercury][odbc.m]SQLDataSources not implemented."
		] },
		{ MaybeSources = error }
	;	
		{ list__reverse(RevMessages, Messages) },
		{ MaybeSources = error }
	).
	
:- pred odbc__sql_data_sources(list(string)::out, list(string)::out, int::out, 
		list(odbc__message)::out, io__state::di, io__state::uo) is det.

:- pragma c_code(
		odbc__sql_data_sources(SourceNames::out, SourceDescs::out,
			Status::out, Messages::out, IO0::di, IO::uo),
		may_call_mercury,
"{
	Status = odbc_do_get_data_sources(&SourceNames, 
			&SourceDescs, &Messages);

	IO = IO0;
}").

:- pragma c_header_code("

SQLRETURN odbc_do_get_data_sources(Word *SourceNames, 
		Word *SourceDescs, Word *Messages);
").

:- pragma c_code("

SQLRETURN
odbc_do_get_data_sources(Word *SourceNames, Word *SourceDescs, Word *Messages)
{
	char dsn[SQL_MAX_DSN_LENGTH];
	char desc[128];	/*
			** Arbitrary size, only needs to hold a 
			** descriptive string like ""SQL Server"".
			*/
	String new_dsn;
	String new_desc;
	SWORD dsn_len;
	SWORD desc_len;
	SQLRETURN rc;

	odbc_message_list = MR_list_empty();
	*SourceNames = MR_list_empty();
	*SourceDescs = MR_list_empty();

	if (odbc_env_handle == SQL_NULL_HENV) {
		rc = SQLAllocEnv(&odbc_env_handle);
	} else {
		rc = SQL_SUCCESS;
	}

	DEBUG(printf(""SQLAllocEnv status: %d\\n"", rc));

	if (odbc_check(odbc_env_handle, SQL_NULL_HDBC, 
			SQL_NULL_HSTMT, rc)) {

		rc = SQLDataSources(odbc_env_handle, SQL_FETCH_FIRST,
				dsn, SQL_MAX_DSN_LENGTH - 1,
				&dsn_len, desc, sizeof(desc), &desc_len);

			/*
			** The documentation varies on whether the driver
			** returns SQL_NO_DATA_FOUND or SQL_NO_DATA, so
			** check for both.
			*/
		while (rc != SQL_NO_DATA_FOUND && rc != SQL_NO_DATA && 
				odbc_check(odbc_env_handle, SQL_NULL_HDBC, 
					SQL_NULL_HSTMT, rc)) 
		{

			/* 
			** Copy the new data onto the Mercury heap
			*/
			make_aligned_string_copy(new_dsn, dsn);
			*SourceNames = MR_list_cons(new_dsn, *SourceNames);
			make_aligned_string_copy(new_desc, desc);
			*SourceDescs = MR_list_cons(new_desc, *SourceDescs);
	
			rc = SQLDataSources(odbc_env_handle,
					SQL_FETCH_NEXT, dsn,
					SQL_MAX_DSN_LENGTH - 1, &dsn_len, 
					desc, sizeof(desc), &desc_len);
		}
	}

	if (rc == SQL_NO_DATA_FOUND) {
		rc = SQL_SUCCESS;
	}

	*Messages = odbc_message_list;
	odbc_message_list = MR_list_empty();
	return rc;
}").

%-----------------------------------------------------------------------------%

odbc__tables(Qualifier, Owner, TableName, Tables) -->
	{ odbc__convert_pattern_argument(Qualifier, QualifierStr,
		QualifierStatus) },
	{ odbc__convert_pattern_argument(Owner, OwnerStr, OwnerStatus) },
	{ odbc__convert_pattern_argument(TableName, TableStr, TableStatus) },
	odbc__do_aggregate(odbc__sql_tables(QualifierStr, QualifierStatus,
		OwnerStr, OwnerStatus, TableStr, TableStatus), 
		cons, [], Results0),
	{ list__reverse(Results0, Results) },
	( { list__map(odbc__convert_table_desc, Results, Tables0) } ->
		{ Tables = Tables0 }
	;
		odbc__add_message(error(internal_error) - 
			"[Mercury][odbc.m]Invalid results from SQLTables."),
		odbc__throw
	).

:- pred odbc__convert_table_desc(odbc__row, odbc__table_desc).
:- mode odbc__convert_table_desc(in, out) is semidet.

odbc__convert_table_desc(Row0, Table) :-
	NullToEmptyStr = 
		lambda([Data0::in, Data::out] is det, (
			( Data0 = null ->
				Data = string("")
			;
				Data = Data0
			)
		)),
	list__map(NullToEmptyStr, Row0, Row),
	Row = [string(Qualifier), string(Owner), string(Name), 
		string(Type), string(Description) | DriverColumns],
	Table = odbc__table_desc(Qualifier, Owner, Name, 
		Type, Description, DriverColumns).

%-----------------------------------------------------------------------------%

	% odbc__convert_pattern_argument(Pattern, String, Status).
	% This is used in a fairly crude interface to C. If the Status is 0,
	% the corresponding argument to the ODBC function should be NULL,
	% meaning no constraint on the search. If the Status is 1, the
	% argument to the ODBC function should be the given string.

:- pred odbc__convert_pattern_argument(odbc__search_pattern::in, 
		string::out, int::out) is det.

odbc__convert_pattern_argument(any, "", 0).
odbc__convert_pattern_argument(pattern(Str), Str, 1).

:- pred odbc__sql_tables(string, int, string, int, string, int,
		odbc__statement, odbc__statement,
		odbc__state, odbc__state).
:- mode odbc__sql_tables(in, in, in, in, in, in, di, uo, di, uo) is det.

:- pragma c_code(odbc__sql_tables(QualifierStr::in, QualifierStatus::in,
		OwnerStr::in, OwnerStatus::in, TableStr::in, TableStatus::in,
		Statement0::di, Statement::uo, DB0::di, DB::uo), 
		may_call_mercury,
"{
	MODBC_Statement *statement = (MODBC_Statement *) Statement0;
	char *qualifier_str = NULL;
	char *owner_str = NULL;
	char *table_str = NULL;
	int qualifier_len = 0;
	int owner_len = 0;
	int table_len = 0;
	SQLRETURN rc;

	/*
	** A NULL pointer in any of the string pattern fields
	** means no constraint on the search for that field.
	*/
	if (QualifierStatus) {
		qualifier_str = (char *) QualifierStr;
		qualifier_len = strlen(qualifier_str);
	}
	if (OwnerStatus) {
		owner_str = (char *) OwnerStr;
		owner_len = strlen(owner_str);
	}
	if (TableStatus) {
		table_str = (char *) TableStr;
		table_len = strlen(table_str);	
	}

	rc = SQLTables(statement->stat_handle, qualifier_str, 
			qualifier_len, owner_str, owner_len,
			table_str, table_len, NULL, 0);
	if (! odbc_check(odbc_env_handle, odbc_connection, 
			statement->stat_handle, rc)) {
		odbc_do_cleanup_statement(statement);
		odbc_throw();
	}

	DB = DB0;
	Statement = (Word) statement;
}").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% 
	% Error checking.
	%

:- pred odbc__ok(int).
:- mode odbc__ok(in) is semidet.

:- pragma c_code(odbc__ok(Status::in), will_not_call_mercury,

"
SUCCESS_INDICATOR = (Status == SQL_SUCCESS || Status == SQL_SUCCESS_WITH_INFO);
").

:- pred odbc__no_data(int).
:- mode odbc__no_data(in) is semidet.

:- pragma c_code(odbc__no_data(Status::in), will_not_call_mercury,
"
SUCCESS_INDICATOR = (Status == SQL_NO_DATA_FOUND);
").

%-----------------------------------------------------------------------------%

	% Handle ODBC error codes. Refer to the ODBC API Reference 
	% provided with the ODBC SDK. The first two characters of the 
	% SQLSTATE are meant to specify an error class. Looking at the 
	% predicates below, the classes weren't terribly well chosen.
:- pred odbc__sql_state_to_message(string::in, string::in,
		odbc__message::out) is det.
:- pragma export(odbc__sql_state_to_message(in, in, out), 
		"MODBC_odbc_sql_state_to_message").

odbc__sql_state_to_message(SQLState, String, Message - String) :-
	string__split(SQLState, 2, Class, SubClass),
	( Class = "01" ->
		( sql_state_to_warning(SubClass, Warning) ->
			Message = warning(Warning)
		;
			Message = warning(general_warning)
		)
	;
		( sql_state_to_error(Class, SubClass, Error) ->
			Message = error(Error)
		;
			Message = error(general_error)
		)
	).

:- pred sql_state_to_warning(string::in, odbc__warning::out) is semidet.
sql_state_to_warning("000", general_warning).
sql_state_to_warning("001", general_warning).
sql_state_to_warning("002", disconnect_error).
sql_state_to_warning("003", null_value_in_set_function).
sql_state_to_warning("004", string_data_truncated).
sql_state_to_warning("006", privilege_not_revoked).
sql_state_to_warning("007", privilege_not_granted).
sql_state_to_warning("S03", general_warning).
sql_state_to_warning("S04", general_warning).

:- pred sql_state_to_error(string::in, string::in,
		odbc__error::out) is semidet.

sql_state_to_error("07", "002", execution_error(incorrect_count_field)).
sql_state_to_error("07", "005", general_error).
sql_state_to_error("07", "006", 
			execution_error(restricted_data_type_violation)).
sql_state_to_error("07", "009", general_error).
sql_state_to_error("07", "S01", internal_error).

sql_state_to_error("08", "001", connection_error(unable_to_establish)).
sql_state_to_error("08", "002", connection_error(connection_name_in_use)).
sql_state_to_error("08", "003", connection_error(nonexistent_connection)).
sql_state_to_error("08", "004", 
			connection_error(connection_rejected_by_server)).
sql_state_to_error("08", "007", connection_error(connection_failure)).
sql_state_to_error("08", "S01", connection_error(connection_failure)).


sql_state_to_error("21", "S01", execution_error(invalid_insert_value_list)).
sql_state_to_error("21", "S02", execution_error(incorrect_derived_table_arity)).

sql_state_to_error("22", "001", execution_error(string_data_truncated)).
sql_state_to_error("22", "002", execution_error(general_error)).
sql_state_to_error("22", "003", execution_error(range_error)).
sql_state_to_error("22", "007", execution_error(invalid_date_time)).
sql_state_to_error("22", "008", execution_error(overflow)).
sql_state_to_error("22", "012", execution_error(division_by_zero)).
sql_state_to_error("22", "015", execution_error(overflow)).
sql_state_to_error("22", "018", execution_error(invalid_cast_specification)).
sql_state_to_error("22", "019", execution_error(invalid_escape)).
sql_state_to_error("22", "025", execution_error(invalid_escape)).
sql_state_to_error("22", "026", execution_error(string_data_length_mismatch)).

sql_state_to_error("23", "000", 
			execution_error(integrity_constraint_violation)).

sql_state_to_error("24", "000", execution_error(general_error)).

sql_state_to_error("25", "S00", transaction_error(invalid_state)).
sql_state_to_error("25", "S01", transaction_error(invalid_state)).
sql_state_to_error("25", "S02", transaction_error(still_active)).
sql_state_to_error("25", "S03", transaction_error(rolled_back)).

sql_state_to_error("28", "000", connection_error(invalid_authorization)).

sql_state_to_error("37", "000", 
			execution_error(syntax_error_or_access_violation)).

sql_state_to_error("3C", "000", execution_error(general_error)).

sql_state_to_error("3D", "000", execution_error(general_error)).

sql_state_to_error("3F", "000", execution_error(invalid_schema_name)).

sql_state_to_error("40", "001", transaction_error(serialization_failure)).
sql_state_to_error("40", "003", execution_error(general_error)).

sql_state_to_error("42", "000", 
			execution_error(syntax_error_or_access_violation)).
sql_state_to_error("42", "S01", execution_error(table_or_view_already_exists)).
sql_state_to_error("42", "S02", execution_error(table_or_view_not_found)).
sql_state_to_error("42", "S11", execution_error(index_already_exists)).
sql_state_to_error("42", "S12", execution_error(index_not_found)).
sql_state_to_error("42", "S21", execution_error(column_already_exists)).
sql_state_to_error("42", "S22", execution_error(column_not_found)).

sql_state_to_error("44", "000", execution_error(general_error)).

sql_state_to_error("IM", _, internal_error).

sql_state_to_error("HY", SubClass, Error) :-
	( SubClass = "000" ->
		Error = general_error
	; SubClass = "109" ->
		Error = feature_not_implemented
	; SubClass = "T00" ->
		Error = timeout_expired
	; SubClass = "T01" ->
		Error = connection_error(timeout_expired)
	;
		Error = internal_error
	).

sql_state_to_error("S0", "001", execution_error(table_or_view_already_exists)).
sql_state_to_error("S0", "002", execution_error(table_or_view_not_found)).
sql_state_to_error("S0", "011", execution_error(index_already_exists)).
sql_state_to_error("S0", "012", execution_error(index_not_found)).
sql_state_to_error("S0", "021", execution_error(column_already_exists)).
sql_state_to_error("S0", "022", execution_error(column_not_found)).
sql_state_to_error("S0", "023", execution_error(no_default_for_column)).

sql_state_to_error("S1", SubClass, Error) :-
	( SubClass = "000" ->
		Error = general_error
	; SubClass = "C00" ->
		Error = feature_not_implemented
	; SubClass = "T01" ->
		Error = connection_error(timeout_expired)
	;
		Error = internal_error
	).

:- pragma c_code("

/*
** Return MR_TRUE if the last ODBC call succeded.
** Return MR_FALSE if the ODBC call failed.
** Add any error messages to odbc_message_list.
*/
static MR_bool
odbc_check(SQLHENV env_handle, SQLHDBC connection_handle, 
		SQLHSTMT statement_handle, SQLRETURN rc)
{
	SQLRETURN	status;
	SQLINTEGER	native_error;
	SQLSMALLINT	msg_len;
	UCHAR		message[SQL_MAX_MESSAGE_LENGTH];
	UCHAR		sql_state[SQL_SQLSTATE_SIZE + 1];
	String 		mercury_message;
	Word		new_message;

	MR_ASSERT_IMPLY(connection_handle == SQL_NULL_HDBC, 
			statement_handle == SQL_NULL_HSTMT);

	odbc_ret_code = rc;

	/* Check type of error */
	if (rc == SQL_SUCCESS) { 
		return MR_TRUE;
	} else {

		DEBUG(printf(""getting error message for status %i\\n"", rc));

		while (1) {

			status = SQLError(env_handle, connection_handle, 
				statement_handle, sql_state, &native_error, 
				message, SQL_MAX_MESSAGE_LENGTH - 1, &msg_len);

			DEBUG(printf(""SQLError status: %i\\n"", status));
			DEBUG(printf(""SQL_STATE: %s\\n"", sql_state));
			DEBUG(printf(""Error: %s\\n"", message));

			if (status != SQL_SUCCESS) {
				break;
			}


			/* Copy the error string to the Mercury heap. */
			make_aligned_string_copy(mercury_message, message);

			/* Convert the SQL state to an odbc__message. */
			MODBC_odbc_sql_state_to_message(sql_state,
				mercury_message, &new_message);

			/* Append the message onto the list. */
			odbc_message_list = 
				MR_list_cons(new_message, odbc_message_list);

		}

		if (rc == SQL_SUCCESS_WITH_INFO) {
			return MR_TRUE;
		} else {
			return MR_FALSE;
		}
	}
}

").

:- end_module odbc.
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
