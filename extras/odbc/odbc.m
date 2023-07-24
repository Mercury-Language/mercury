%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1997 Mission Critical.
% Copyright (C) 1997-2000, 2002, 2004-2006, 2010 The University of Melbourne.
% Copyright (C) 2017-2018, 2020, 2023 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: odbc.m.
% Authors: Renaud Paquay (rpa@miscrit.be), stayl.
% ODBC version: 2.0.
%
% The transaction interface used here is described in the following paper:
%
%   Kemp, Conway, Harris, Henderson, Ramamohanarao and Somogyi,
%   "Database transactions in a purely declarative logic programming language".
%   In Proceedings of the Fifth International Conference on Database
%   Systems for Advanced Applications, pp. 283-292.
%   Melbourne, Australia, 1-4 April, 1997.
%
%   An earlier(?) version of this paper is available as
%   Technical Report 96/45, Department of Computer Science,
%   University of Melbourne, December 1996,
%   <https://mercurylang.org/documentation/papers/tr_96_45_cover.ps.gz>
%   and <https://mercurylang.org/documentation/papers/tr_96_45.ps.gz>.
%
% This has been tested using the following platforms:
% - MySQL 3.20.19 and iODBC 2.12 under Solaris 2.5
% - MySQL 3.22.32 and iODBC 2.50.3 under Solaris 2.6
% - Microsoft SQL Server 6.5 under Windows NT 4.0 with the
%   GNU-Win32 tools beta 17.1
%
% Notes:
%
%   Binary data is converted to a string of hexadecimal digits.
%
%   This module requires a compilation grade with conservative garbage
%   collection. Any grade containing .gc in its name, such as asm_fast.gc,
%   will do. See the section "Compilation model options" in the Mercury
%   User's Guide for more information.
%
%   The header files distributed with the Microsoft ODBC SDK require
%   some modification for compilation with gcc. In particular,
%   some conflicting typedefs for SQLUINTEGER and SQLSCHAR must be
%   removed from sqltypes.h.
%   (For legal reasons a patch cannot be included in the Mercury
%   distribution.)
%
% To do:
%
%   Improve the interface to the catalog functions.
%
%   Add a nicer interface so the user does not need to manipulate
%   SQL strings.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module odbc.
:- interface.

:- import_module list.
:- import_module io.
:- import_module pair.
:- import_module string.

%-----------------------------------------------------------------------------%
%
% Predicates and types for transaction processing
%

:- type data_source   == string.
:- type user_name     == string.
:- type password      == string.

    % A closure to be executed atomically.
    %
:- type transaction(T) == pred(T, odbc.state, odbc.state).
:- mode transaction == (pred(out, di, uo) is det).

:- type state.

    % transaction(Source, UserName, Password, Transaction, Result).
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
    % If `Transaction' throws an exception, odbc.transaction will
    % attempt to roll back the transaction, and will then rethrow
    % the exception to the caller.
    %
:- pred transaction(data_source::in, user_name::in, password::in,
    transaction(T)::transaction, odbc.result(T)::out, io::di, io::uo) is det.

    % Abort the current transaction, returning the given error message.
    %
:- pred rollback(string::in, odbc.state::di, odbc.state::uo) is erroneous.

%-----------------------------------------------------------------------------%
%
% Predicates and types for execution of SQL statements
%

:- type row == list(odbc.attribute).

:- type attribute
    --->    null            % SQL NULL value
    ;       int(int)
    ;       string(string)
    ;       float(float)
    ;       time(string).   % Time string: "YYYY-MM-DD hh:mm:ss.mmm"

% The odbc.state arguments threaded through these predicates
% enforce the restriction that database activity can only occur
% within a transaction, since odbc.states are only available
% to the closure called by odbc.transaction/5.

    % Execute an SQL statement which doesn't return any results, such
    % as DELETE.
    %
:- pred execute(string::in, odbc.state::di, odbc.state::uo) is det.

    % Execute an SQL statement, returning a list of results in the
    % order they are returned from the database.
    %
:- pred solutions(string::in, list(odbc.row)::out,
    odbc.state::di, odbc.state::uo) is det.

    % Execute an SQL statement, applying the accumulator predicate
    % to each element of the result set as it is returned from
    % the database.
    %
:- pred aggregate(string, pred(odbc.row, T, T), T, T,
    odbc.state, odbc.state).
:- mode aggregate(in, pred(in, in, out) is det, in, out, di, uo) is det.
:- mode aggregate(in, pred(in, di, uo) is det, di, uo, di, uo) is det.

%-----------------------------------------------------------------------------%
%
% Predicates and types to get information about database tables.
%

% This is very incomplete, it would be nice to be able to get information
% about the columns in a table and about privileges for tables and columns.

:- type source_desc
    --->    source_desc(
                odbc.data_source,   % name
                string              % description
            ).

:- type search_pattern
    --->    any                 % _ matches any single character.
    ;       pattern(string).    % Matches a sequence of characters.

    % Information about a table accessible by a transaction.
    %
:- type table_desc
    --->    table_desc(
                string,         % table qualifier
                string,         % table owner
                string,         % table name
                string,         % table type
                string,         % description
                list(odbc.attribute)   % data source specific columns
            ).

    % Get a list of all the available data sources.
    % Note that iODBC 2.12 doesn't implement this.
    %
:- pred data_sources(odbc.result(list(odbc.source_desc))::out,
    io::di, io::uo) is det.

    % tables(QualifierPattern, OwnerPattern, TableNamePattern, Result).
    %
    % Get a list of database tables matching the given description.
    % Note that wildcards are not allowed in the QualifierPattern.
    % This is fixed in ODBC 3.0.
    %
:- pred tables(odbc.search_pattern::in, odbc.search_pattern::in,
    odbc.search_pattern::in, list(odbc.table_desc)::out,
    odbc.state::di, odbc.state::uo) is det.

%-----------------------------------------------------------------------------%
%
% The following types are used to return status and error information from
% ODBC calls.
%

:- type result == pair(odbc.status, list(odbc.message)).

:- type status
    --->    ok
    ;       error.

    % The message list returned from a transaction contains all errors
    % and warnings reported by the driver during the transaction in
    % the order that they were reported.
    %
:- type result(T) == pair(odbc.status(T), list(odbc.message)).

:- type status(T)
    --->    ok(T)
    ;       error.

:- type message == pair(odbc.message_type, string).

:- type message_type
    --->    warning(odbc.warning)
    ;       error(odbc.error).

:- type warning
    --->    disconnect_error
    ;       fractional_truncation
    ;       general_warning
    ;       null_value_in_set_function
    ;       privilege_not_revoked
    ;       privilege_not_granted
    ;       string_data_truncated.

:- type error
    --->    connection_error(odbc.connection_error)
    ;       execution_error(odbc.execution_error)
    ;       feature_not_implemented
    ;       general_error
    ;       internal_error
    ;       timeout_expired
    ;       transaction_error(odbc.transaction_error)
    ;       user_requested_rollback.

:- type connection_error
    --->    unable_to_establish
    ;       invalid_authorization
    ;       connection_name_in_use
    ;       nonexistent_connection
    ;       connection_rejected_by_server
    ;       connection_failure
    ;       timeout_expired.

:- type execution_error
    --->    column_already_exists
    ;       column_not_found
    ;       division_by_zero
    ;       general_error
    ;       incorrect_count_field
    ;       incorrect_derived_table_arity
    ;       index_already_exists
    ;       index_not_found
    ;       integrity_constraint_violation
    ;       interval_field_overflow
    ;       invalid_cast_specification
    ;       invalid_date_time
    ;       invalid_escape
    ;       invalid_insert_value_list
    ;       invalid_schema_name
    ;       invalid_use_of_default_parameter
    ;       length_mismatch_in_string_data
    ;       no_default_for_column
    ;       overflow
    ;       range_error
    ;       restricted_data_type_violation
    ;       string_data_length_mismatch
    ;       string_data_truncated
    ;       syntax_error_or_access_violation
    ;       table_or_view_already_exists
    ;       table_or_view_not_found.

:- type transaction_error
    --->    rolled_back
    ;       still_active
    ;       serialization_failure
    ;       invalid_state.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module assoc_list.
:- import_module bool.
:- import_module exception.
:- import_module int.
:- import_module require.
:- import_module unit.
:- import_module univ.

:- pragma require_feature_set([conservative_gc]).

%-----------------------------------------------------------------------------%

    % We don't actually store anything in the odbc.state, since that
    % would make the exception handling more inconvenient and error-prone.
    % The odbc.state would have to be stored in a global anyway just
    % before calling longjmp.
    % All the data related to a transaction (ODBC handles, error messages)
    % is stored in the global variables defined below.
    %
:- type state == unit.

:- pragma foreign_decl("C", "
#include ""mercury_imp.h""
#include <stdio.h>
#include <stdlib.h>
#include <setjmp.h>
#include <assert.h>

    // odbc.m allocates memory within may_call_mercury pragma C code,
    // which is a bit dodgy in non-conservative GC grades.
    // Allowing non-conservative GC grades would require a bit of fairly
    // error-prone code to save/restore the heap pointer in the right
    // places. When accurate garbage collection is implemented and a
    // nicer way of allocating heap space from within C code is available,
    // this should be revisited.
    // Conservative garbage collection also makes restoring the state
    // after an exception a bit simpler.
#ifndef MR_CONSERVATIVE_GC
#error ""The OBDC interface requires conservative garbage collection. \\
        Use a compilation grade containing .gc.""
#endif // ! MR_CONSERVATIVE_GC

// For use with iODBC.
#ifdef MODBC_IODBC

    #include ""isql.h""
    #include ""isqlext.h""
    // #include ""odbc_funcs.h""
    #include ""sqltypes.h""

    // iODBC 2.12 doesn't define this, so define it to something harmless.
    #ifndef SQL_NO_DATA
        #define SQL_NO_DATA SQL_NO_DATA_FOUND
    #endif

#endif // MODBC_IODBC

// For use with unixODBC.
#ifdef MODBC_UNIX

    #include ""sql.h""
    #include ""sqlext.h""
    #include ""sqltypes.h""

#endif // MODBC_UNIX

// For interfacing directly with ODBC driver bypassing driver managers
// such as iODBC.
#ifdef MODBC_ODBC

    #include ""sql.h""
    #include ""sqlext.h""
    #include ""sqltypes.h""

    #ifndef SQL_NO_DATA
        #define SQL_NO_DATA SQL_NO_DATA_FOUND
    #endif

#endif // MODBC_ODBC

#ifdef MODBC_MS

    // ODBC_VER set to 0x0250 means that this uses only ODBC 2.0
    // functionality but compiles with the ODBC 3.0 header files.
    #define ODBC_VER 0x0250

    // The following is needed to allow the Microsoft headers to
    // compile with GNU C under gnu-win32.

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

#endif // MODBC_MS

// Assert the implication: a => b
#define MR_ASSERT_IMPLY(a,b)    MR_assert( !(a) || (b) )

// All integers get converted to long by the driver, then to MR_Integer.
// All floats get converted to double by the driver, then to MR_Float.
typedef long            MODBC_C_INT;
typedef double          MODBC_C_FLOAT;

// Define some wrappers around setjmp and longjmp for exception
// handling. We need to use MR_setjmp and MR_longjmp because we'll
// be longjmping across C->Mercury calls, so we need to restore
// some state in runtime/engine.c.
// Beware: the Mercury registers must be valid when odbc_catch
// is called. odbc_throw will clobber the general-purpose registers
// r1, r2, etc.
#define odbc_catch(longjmp_label)                   \
            MR_setjmp(&odbc_trans_jmp_buf, longjmp_label)

#define odbc_throw()    MR_longjmp(&odbc_trans_jmp_buf)

// odbc_trans_jmp_buf stores information saved by odbc_catch (setjmp)
// to be used by odbc_throw (longjmp) when a database exception is found.
extern MR_jmp_buf odbc_trans_jmp_buf;

// odbc_env_handle is the output of SQLAllocEnv. SQLAllocEnv must
// be called before attempting to open any connections.
extern SQLHENV odbc_env_handle;

// The connection being acted on by the current transaction.
extern SQLHDBC odbc_connection;

// The last return code from an ODBC system call.
extern SQLRETURN odbc_ret_code;

// The list of accumulated warnings and errors for the transaction
// in reverse order.
extern MR_Word odbc_message_list;

extern void
odbc_transaction_c_code(MR_Word type_info, SQLHDBC Connection,
    MR_Word Closure, MR_Word *Results, MR_Bool *GotMercuryException,
    MR_Word *Exception, MR_Integer *Status, MR_Word *Msgs);

extern MR_bool
odbc_check(SQLHENV, SQLHDBC, SQLHSTMT, SQLRETURN);

").

:- pragma foreign_code("C", "
MR_jmp_buf odbc_trans_jmp_buf;

SQLHENV  odbc_env_handle = SQL_NULL_HENV;

SQLHDBC  odbc_connection = SQL_NULL_HDBC;

SQLRETURN odbc_ret_code = SQL_SUCCESS;

MR_Word  odbc_message_list;
").

%-----------------------------------------------------------------------------%

transaction(Source, User, Password, Closure, Result, !IO) :-
    % We could have separate open and close connection predicates in the
    % interface, but that would just be more effort for the programmer
    % for a very minor efficiency gain. The connection time will be
    % insignificant for even trivial queries.
    open_connection(Source, User, Password, ConnectStatus - ConnectMessages,
        !IO),
    (
        ConnectStatus = ok(Connection),
        % Do the transaction.
        transaction_2(Connection, Closure, Data, GotMercuryException,
            Exception, Status, RevMessages, !IO),
        list.reverse(RevMessages, TransMessages),

        close_connection(Connection, CloseStatus - CloseMessages, !IO),
        % Pass on any exception that was found while
        % processing the transaction.
        (
            GotMercuryException = yes,
            rethrow(exception(Exception))
        ;
            GotMercuryException = no,
            list.condense([ConnectMessages, TransMessages, CloseMessages],
                Messages),
            ( if odbc.ok(Status), CloseStatus = ok then
                Result = ok(Data) - Messages
            else
                Result = error - Messages
            )
        )
    ;
        ConnectStatus = error,
        Result = error - ConnectMessages
    ).

:- pred transaction_2(connection::in,
    pred(T, odbc.state, odbc.state)::in(pred(out, di, uo) is det),
    T::out, bool::out, univ::out, int::out, list(odbc.message)::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    transaction_2(Connection::in, Closure::in(pred(out, di, uo) is det),
        Results::out, GotMercuryException::out, Exception::out,
        Status::out, Msgs::out, _IO0::di, _IO::uo),
    [promise_pure, may_call_mercury],
"
    // The Mercury registers must be valid at the call to odbc_catch
    // in odbc_transaction_c_code().
    // odbc_transaction_c_code() may clobber the Mercury general-purpose
    // registers r1, r2, ..., but that is OK, because this C code is
    // declared as 'may_call_mercury', so the compiler assumes that it
    // is allowed to clobber those registers.

    MR_save_transient_registers();
    odbc_transaction_c_code(TypeInfo_for_T, Connection, Closure,
        &Results, &GotMercuryException, &Exception,
        &Status, &Msgs);
    MR_restore_transient_registers();
").

:- pragma foreign_code("C", "
void
odbc_transaction_c_code(MR_Word TypeInfo_for_T, SQLHDBC Connection,
    MR_Word Closure, MR_Word *Results, MR_Bool *GotMercuryException,
    MR_Word *Exception, MR_Integer *Status, MR_Word *Msgs)
{
    MR_Word DB0 = (MR_Word) 0;
    MR_Word DB = (MR_Word) 0;
    SQLRETURN rc;

    MR_restore_transient_registers();

    // Mercury state to restore on rollback.
    odbc_connection = Connection;
    odbc_message_list = MR_list_empty();

    // Set up a location to jump to on a database exception.
    // The Mercury registers must be valid here.
    odbc_catch(transaction_error);

    // Anything changed between the call to odbc_catch() and the call to
    // MODBC_odbc__do_transaction() must be declared volatile.

    MODBC_odbc__do_transaction(TypeInfo_for_T, Closure,
        GotMercuryException, Results, Exception);

    // MR_longjmp() cannot be called after here.

    if (*GotMercuryException == MR_NO) {
        rc = SQLTransact(odbc_env_handle, odbc_connection, SQL_COMMIT);

        if (! odbc_check(odbc_env_handle, odbc_connection, SQL_NULL_HSTMT,
                rc))
        {
            goto transaction_error;
        }
    } else {
        // There was a Mercury exception -- abort the transaction.
        // The return value of the call to SQLTransact() is ignored
        // because the caller won't look at the result --
        // it will just rethrow the exception.
        MR_DEBUG(printf(
            ""Mercury exception in transaction: aborting\\n""));
        (void) SQLTransact(odbc_env_handle, odbc_connection, SQL_ROLLBACK);
    }

    *Status = SQL_SUCCESS;

    goto transaction_done;

transaction_error:
    // Make the database rollback the transaction, if it hasn't already.
    *Status = odbc_ret_code;

    *GotMercuryException = 0;

    rc = SQLTransact(odbc_env_handle, odbc_connection, SQL_ROLLBACK);

    odbc_check(odbc_env_handle, odbc_connection, SQL_NULL_HSTMT, rc);

    // Fall through.

transaction_done:
    *Msgs = odbc_message_list;
    odbc_message_list = MR_list_empty();
    odbc_connection = SQL_NULL_HDBC;
    odbc_ret_code = SQL_SUCCESS;

    MR_save_transient_registers();
}
").

%-----------------------------------------------------------------------------%
%
% Call the transaction closure
%

:- pred do_transaction(transaction(T)::transaction, bool::out, T::out,
    univ::out, odbc.state::di, odbc.state::uo) is cc_multi.

:- pragma foreign_export("C",
    do_transaction(transaction, out, out, out, di, uo),
    "MODBC_odbc__do_transaction").

do_transaction(Closure, GotException, Results, Exception, State0, State) :-
    try(
        ( pred(TryResult::out) is det :-
            unsafe_promise_unique(State0, State1),
            Closure(Result, State1, ResultState),
            TryResult = Result - ResultState
        ), ExceptResult),
    (
        ExceptResult = succeeded(Results - State2),
        unsafe_promise_unique(State2, State),
        make_dummy_value(Exception),
        GotException = no
    ;
        ExceptResult = exception(Exception),
        make_dummy_value(Results),
        unsafe_promise_unique(State0, State),
        GotException = yes
    ).

    % Produce a value which is never looked at, for returning
    % discriminated unions to C.
    %
:- pred make_dummy_value(T::out) is det.
:- pragma foreign_proc("C",
    make_dummy_value(T::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    T = 0;
").

%-----------------------------------------------------------------------------%

rollback(Error, !DB) :-
    odbc.add_message(error(user_requested_rollback) - Error, !DB),
    odbc.throw(!DB).

:- pred add_message(odbc.message::in, odbc.state::di, odbc.state::uo) is det.

:- pragma foreign_proc("C",
    add_message(Error::in, DB0::di, DB::uo),
    [promise_pure, will_not_call_mercury],
"
    odbc_message_list = MR_list_cons(Error, odbc_message_list);
    DB = DB0;
").

:- pred throw(odbc.state::di, odbc.state::uo) is erroneous.

:- pragma foreign_proc("C",
    throw(DB0::di, DB::uo),
    [promise_pure, will_not_call_mercury],
"
    odbc_ret_code = SQL_ERROR;
    odbc_throw();
    // DB = DB0; (not reached)
").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% Predicates and types to manage connections
%

    % A connection to a specific source.
    %
:- type connection.

    % Given the data source to connect to and a user name and password,
    % open a connection.
    %
:- pred open_connection(data_source::in, user_name::in,
    password::in, odbc.result(odbc.connection)::out, io::di, io::uo) is det.

    % Close the connection to the given data source.
    %
:- pred close_connection(odbc.connection::in, odbc.result::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- pragma foreign_type("C", connection, "SQLHDBC").

open_connection(Source, User, Password, Result - Messages, !IO) :-
    do_open_connection(Source, User, Password, Handle, ConnectStatus,
        RevMessages, !IO),
    list.reverse(RevMessages, Messages),
    ( if odbc.ok(ConnectStatus) then
        Result = ok(Handle)
    else
        Result = error
    ).

%-----------------------------------------------------------------------------%

:- pred do_open_connection(string::in, string::in, string::in,
    odbc.connection::uo, int::out, list(odbc.message)::out, io::di, io::uo)
    is det.

:- pragma foreign_proc("C",
    do_open_connection(Source::in, User::in, Password::in, Handle::uo,
        Status::out, Messages::out, _IO0::di, _IO::uo),
    [promise_pure, may_call_mercury],
"
    SQLHDBC connect_handle;

    if (odbc_env_handle == SQL_NULL_HENV) {
        Status = SQLAllocEnv(&odbc_env_handle);
    } else {
        Status = SQL_SUCCESS;
    }

    MR_DEBUG(printf(""SQLAllocEnv status: %d\\n"", (int) Status));

    if (odbc_check(odbc_env_handle, SQL_NULL_HDBC,
            SQL_NULL_HSTMT, Status)) {

        Status = SQLAllocConnect(odbc_env_handle, &connect_handle);

        MR_DEBUG(printf(""SQLAllocConnect status: %d\\n"", (int) Status));

        if (odbc_check(odbc_env_handle, connect_handle,
                SQL_NULL_HSTMT, Status)) {
            // Put the connection into manual commit mode.
            Status = SQLSetConnectOption(connect_handle,
                SQL_AUTOCOMMIT, SQL_AUTOCOMMIT_OFF);

            MR_DEBUG(printf(""manual commit status: %d\\n"",
                    (int) Status));

            odbc_check(odbc_env_handle, connect_handle,
                SQL_NULL_HSTMT, Status);
        }
    }

    Status = SQLConnect(connect_handle,
            (UCHAR *)Source, strlen(Source),
            (UCHAR *)User, strlen(User),
            (UCHAR *)Password, strlen(Password));

    MR_DEBUG(printf(""connect status: %d\\n"", (int) Status));

    odbc_check(odbc_env_handle, connect_handle, SQL_NULL_HSTMT, Status);

    Messages = odbc_message_list;
    odbc_message_list = MR_list_empty();

    Handle = connect_handle;
    odbc_connection = SQL_NULL_HDBC;
").

%-----------------------------------------------------------------------------%

close_connection(Connection, Result, !IO) :-
    do_close_connection(Connection, Status, RevMessages, !IO),
    list.reverse(RevMessages, Messages),
    ( if Status = 0 then
        Result = ok - Messages
    else
        Result = error - Messages
    ).

:- pred do_close_connection(odbc.connection::in, int::out,
    list(odbc.message)::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    do_close_connection(Handle::in, Status::out, Messages::out,
        _IO0::di, _IO::uo),
    [promise_pure, may_call_mercury],
"
    Status = SQLDisconnect(Handle);
    if (odbc_check(odbc_env_handle, Handle,
            SQL_NULL_HSTMT, Status)) {
        Status = SQLFreeConnect(Handle);
        odbc_check(odbc_env_handle, Handle,
            SQL_NULL_HSTMT, Status);
    }

    Messages = odbc_message_list;
    odbc_message_list = MR_list_empty();
").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

execute(SQLString, !DB) :-
    some [!Statement] (
        odbc.alloc_statement(!:Statement, !DB),
        odbc.execute_statement(SQLString, !Statement, !DB),
        odbc.cleanup_statement_check_error(!.Statement, !DB)
    ).

solutions(SQLString, Results, !DB) :-
    % XXX optimize this when we have better support
    % for last call optimization.
    odbc.do_aggregate(odbc.execute_statement(SQLString), list.cons, [],
        Results0, !DB),
    list.reverse(Results0, Results).

aggregate(SQLString, Accumulator, !Acc, !DB) :-
    do_aggregate(odbc.execute_statement(SQLString), Accumulator, !Acc, !DB).

%-----------------------------------------------------------------------------%

:- pred do_aggregate(
    pred(odbc.statement, odbc.statement, odbc.state, odbc.state),
    pred(odbc.row, T, T), T, T, odbc.state, odbc.state).
:- mode do_aggregate(
    pred(di, uo, di, uo) is det,
    pred(in, in, out) is det, in, out, di, uo) is det.
:- mode do_aggregate(
    pred(di, uo, di, uo) is det,
    pred(in, di, uo) is det, di, uo, di, uo) is det.

do_aggregate(Execute, Accumulate, !Result, !DB) :-
    some [!Statement] (
        alloc_statement(!:Statement, !DB),
        Execute(!Statement, !DB),
        bind_columns(!Statement, !DB),
        get_rows(Accumulate, !Result, !Statement, !DB),
        cleanup_statement_check_error(!.Statement, !DB)
    ).

%-----------------------------------------------------------------------------%

    % Get the set of result rows from the statement.
    %
:- pred get_rows(pred(odbc.row, T, T), T, T, odbc.statement, odbc.statement,
    odbc.state, odbc.state).
:- mode get_rows(pred(in, in, out) is det, in, out, di, uo, di, uo) is det.
:- mode get_rows(pred(in, di, uo) is det, di, uo, di, uo, di, uo) is det.

get_rows(Accumulate, !Result, !Statement, !DB) :-
    get_number_of_columns(NumColumns, !Statement, !DB),
    get_rows_2(NumColumns, Accumulate, !Result, !Statement, !DB).

:- pred get_rows_2(int, pred(odbc.row, T, T), T, T,
    odbc.statement, odbc.statement, odbc.state, odbc.state).
:- mode get_rows_2(in, pred(in, in, out) is det, in, out, di, uo,
    di, uo) is det.
:- mode get_rows_2(in, pred(in, di, uo) is det, di, uo, di, uo,
    di, uo) is det.

get_rows_2(NumColumns, Accumulate, !Result, !Statement, !DB) :-
    % Try to fetch a new row.
    fetch_row(!Statement, Status, !DB),
    ( if no_data(Status) then
        true
    else
        get_attributes(1, NumColumns, Row, !Statement, !DB),
        Accumulate(Row, !Result),
        get_rows_2(NumColumns, Accumulate, !Result, !Statement, !DB)
    ).

%-----------------------------------------------------------------------------%

    % Get the values from the current fetched row.
    %
:- pred get_attributes(int::in, int::in, list(odbc.attribute)::out,
    odbc.statement::di, odbc.statement::uo,
    odbc.state::di, odbc.state::uo) is det.

get_attributes(CurrCol, NumCols, Row, !Statement, !DB) :-
    ( if CurrCol =< NumCols then
        NextCol = CurrCol + 1,
        get_attribute(CurrCol, Attribute, !Statement, !DB),
        get_attributes(NextCol, NumCols, Row1, !Statement, !DB),
        Row = [Attribute | Row1]
    else
        Row = []
    ).

%-----------------------------------------------------------------------------%

    % Get the value of a column in the current fetched row.
    %
:- pred get_attribute(int::in, odbc.attribute::out,
    odbc.statement::di, odbc.statement::uo, odbc.state::di, odbc.state::uo)
    is det.

get_attribute(NumColumn, Value, !Statement, !DB) :-
    get_data(NumColumn, Int, Float, String, TypeInt, !Statement, !DB),
    int_to_attribute_type(TypeInt, Type),
    (
        Type = null,
        Value = null
    ;
        Type = string,
        Value = string(String)
    ;
        Type = time,
        Value = time(String)
    ;
        Type = int,
        Value = int(Int)
    ;
        Type = float,
        Value = float(Float)
    ).

%-----------------------------------------------------------------------------%

:- type attribute_type
    --->    int
    ;       float
    ;       time
    ;       string
    ;       null.

:- pred int_to_attribute_type(int::in, odbc.attribute_type::out) is det.

int_to_attribute_type(Int, Type) :-
    ( if int_to_attribute_type_2(Int, Type1) then
        Type = Type1
    else
        error("odbc.int_to_attribute_type: invalid type")
    ).

    % Keep this in sync with the C enum MODBC_AttrType below.
    %
:- pred int_to_attribute_type_2(int::in, odbc.attribute_type::out) is semidet.

int_to_attribute_type_2(0, int).
int_to_attribute_type_2(1, float).
int_to_attribute_type_2(2, time).
int_to_attribute_type_2(3, string).
int_to_attribute_type_2(4, string).
int_to_attribute_type_2(5, null).

%-----------------------------------------------------------------------------%

:- type statement.

:- pragma foreign_type("C", statement, "MODBC_Statement *",
    [can_pass_as_mercury_type]).

:- pragma foreign_decl("C", "
// Notes on memory allocation:
//
// C data structures (MODBC_Statement and MODBC_Column) are allocated
// using MR_GC_malloc/MR_GC_free.
//
// MODBC_Statement contains a statement handle which must be freed
// using SQLFreeStmt.
//
// Variable length data types are collected in chunks allocated on
// the Mercury heap using MR_incr_hp_atomic. The chunks are then
// condensed into memory allocated on the Mercury heap using
// string.append_list.
// XXX this may need revisiting when accurate garbage collection
// is implemented to make sure the collector can see the data when
// it is stored within a MODBC_Column.
//
// Other data types have a buffer which is allocated once using
// MR_GC_malloc.

// If the driver can't work out how much data is in a blob in advance,
// get the data in chunks. The chunk size is fairly arbitrary.
// MODBC_CHUNK_SIZE must be a multiple of sizeof(MR_Word).

#define MODBC_CHUNK_WORDS   1024
#define MODBC_CHUNK_SIZE    (MODBC_CHUNK_WORDS * sizeof(MR_Word))

typedef enum {
    MODBC_INT        = 0,   // Word-sized Integer
    MODBC_FLOAT      = 1,   // Mercury Float
    MODBC_TIME       = 2,   // time and/or date converted to a string
    MODBC_STRING     = 3,   // string, or type converted to a string
    MODBC_VAR_STRING = 4,   // string with no maximum length
    MODBC_NULL       = 5
} MODBC_AttrType;

typedef enum { MODBC_BIND_COL, MODBC_GET_DATA } MODBC_BindType;

// Information about a column in a result set.
typedef struct {
        size_t          size;       // size of allocated buffer
        MODBC_AttrType  attr_type;
        SWORD           sql_type;   // the actual type, e.g. SQL_LONG_VAR_CHAR
        SWORD           conversion_type;
                                    // the type the data is being converted
                                    // into, e.g SQL_C_CHAR
        SDWORD          value_info; // size of returned data, or SQL_NULL_DATA
        MR_Word         *data;
} MODBC_Column;

// Information about a result set.
typedef struct {
        SQLHSTMT        stat_handle;    // statement handle
        int             num_columns;    // columns per row
        MODBC_Column    *row;           // array of columns in the current row
        int             num_rows;       // number of fetched rows
        MODBC_BindType  binding_type;   // are we using SQL_BIND_COL
                                        // or SQL_GET_DATA
} MODBC_Statement;

extern SQLRETURN
odbc_do_cleanup_statement(MODBC_Statement *statement);

extern size_t
odbc_sql_type_to_size(SWORD sql_type, UDWORD cbColDef, SWORD ibScale,
    SWORD fNullable);

extern MODBC_AttrType
odbc_sql_type_to_attribute_type(SWORD sql_type);

extern SWORD
odbc_attribute_type_to_sql_c_type(MODBC_AttrType AttrType);

extern MR_bool
odbc_is_variable_length_sql_type(SWORD);

extern void
odbc_do_get_data(MODBC_Statement *statement, int column_id);

extern void
odbc_get_data_in_chunks(MODBC_Statement *statement, int column_id);

extern void
odbc_get_data_in_one_go(MODBC_Statement *statement, int column_id);
").

%-----------------------------------------------------------------------------%

:- pred alloc_statement(odbc.statement::uo,
    odbc.state::di, odbc.state::uo) is det.

:- pragma foreign_proc("C",
    alloc_statement(Statement::uo, DB0::di, DB::uo),
    [promise_pure, may_call_mercury],
"
    SQLRETURN rc;

    // Doing manual deallocation of the statement object.
    Statement = MR_GC_NEW(MODBC_Statement);

    Statement->num_columns = 0;
    Statement->row = NULL;
    Statement->num_rows = 0;
    Statement->stat_handle = SQL_NULL_HSTMT;

    rc = SQLAllocStmt(odbc_connection, &(Statement->stat_handle));
    if (! odbc_check(odbc_env_handle, odbc_connection,
        Statement->stat_handle, rc))
    {
        odbc_throw();
        // not reached
    }

    MR_assert(Statement->stat_handle != SQL_NULL_HSTMT);
    DB = DB0;
").

%-----------------------------------------------------------------------------%

:- pred execute_statement(string::in, odbc.statement::di, odbc.statement::uo,
    odbc.state::di, odbc.state::uo) is det.

:- pragma foreign_proc("C",
    execute_statement(SQLString::in, Statement0::di, Statement::uo,
        DB0::di, DB::uo),
    [promise_pure, may_call_mercury],
"
    SQLRETURN rc;
    SQLHSTMT stat_handle;

    Statement = Statement0;

    stat_handle = Statement->stat_handle;

    MR_DEBUG(printf(""executing SQL string: %s\\n"", SQLString));

    rc = SQLPrepare(stat_handle, (SQLCHAR *)SQLString, strlen(SQLString));

    if (! odbc_check(odbc_env_handle, odbc_connection, stat_handle, rc)) {
        // We don't check the return status of this because the programmer
        // is likely to be more interested in the earlier error.
        odbc_do_cleanup_statement(Statement);
        odbc_throw();
        // not reached
    }

    rc = SQLExecute(stat_handle);
    if (! odbc_check(odbc_env_handle, odbc_connection, stat_handle, rc)) {
        odbc_do_cleanup_statement(Statement);
        odbc_throw();
        // not reached
    }

    MR_DEBUG(printf(""execution succeeded\\n""));
    DB = DB0;
").

%-----------------------------------------------------------------------------%

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
:- pred bind_columns(odbc.statement::di, odbc.statement::uo,
    odbc.state::di, odbc.state::uo) is det.

:- pragma foreign_proc("C",
    bind_columns(Statement0::di, Statement::uo, DB0::di, DB::uo),
    [promise_pure, may_call_mercury],
"
    int             column_no;
    SQLSMALLINT     num_columns;
    MODBC_Column    *column;
    SQLRETURN       rc;
    SQLHSTMT        stat_handle;

    Statement = Statement0;
    stat_handle = Statement->stat_handle;

    // Retrieve the number of columns of the statement.
    rc = SQLNumResultCols(stat_handle, &num_columns);
    if (! odbc_check(odbc_env_handle, odbc_connection, stat_handle, rc)) {
        odbc_do_cleanup_statement(Statement);
        odbc_throw();
        // not reached
    }
    Statement->num_columns = num_columns;

    // Allocate an array containing the info for each column.
    // The extra column is because ODBC counts columns starting from 1.
    Statement->row = MR_GC_NEW_ARRAY(MODBC_Column, num_columns + 1);

    // Use SQLBindCol unless there are columns with no set maximum length.
    Statement->binding_type = MODBC_BIND_COL;

    // Get information about the result set columns.
    // ODBC counts columns from 1.
    for (column_no = 1; column_no <= Statement->num_columns; column_no++) {
        char        col_name[1];    // Not looked at
        SWORD       col_name_len;
        SWORD       col_type;
        UDWORD      pcbColDef;
        SWORD       pibScale;
        SWORD       pfNullable;

        column = &(Statement->row[column_no]);
        column->size = 0;
        column->data = NULL;

        // Retrieve the C type of the column.
        // (SQL type mapped to a conversion type).
        // Create an attribute object with room to store the
        // attribute value.
        rc = SQLDescribeCol(stat_handle, column_no,
                (UCHAR *) col_name, sizeof(col_name),
                &col_name_len, &col_type, &pcbColDef,
                &pibScale, &pfNullable);

        // SQL_SUCCESS_WITH_INFO means there wasn't
        // enough space for the column name, but we
        // aren't collecting the column name anyway.
        if (rc != SQL_SUCCESS_WITH_INFO &&
            ! odbc_check(odbc_env_handle, odbc_connection,
                stat_handle, rc))
        {
            odbc_do_cleanup_statement(Statement);
            odbc_throw();
            // not reached
        }

        column->sql_type = col_type;
        column->size = odbc_sql_type_to_size(col_type, pcbColDef,
            pibScale, pfNullable);

        column->attr_type = odbc_sql_type_to_attribute_type(col_type);

        // Request a conversion into one of the supported types.
        column->conversion_type =
            odbc_attribute_type_to_sql_c_type(column->attr_type);

        MR_DEBUG(printf(""Column %i: size %i - sql_type %i - attr_type %i - conversion_type %i\\n"",
            column_no, column->size, column->sql_type,
            column->attr_type, column->conversion_type));

        if (odbc_is_variable_length_sql_type(col_type)) {
            Statement->binding_type = MODBC_GET_DATA;
        } else {
            // Do the buffer allocation once for columns which have
            // a fixed maximum length.
            column->data = MR_GC_malloc(column->size);
        }

    } // for

    if (Statement->binding_type == MODBC_BIND_COL) {
        for (column_no = 1; column_no <= Statement->num_columns; column_no++) {
            MR_DEBUG(printf(""Binding column %d/%d\\n"",
                column_no, Statement->num_columns));
            column = &(Statement->row[column_no]);

            rc = SQLBindCol(stat_handle, column_no,
                column->conversion_type, (SQLPOINTER) column->data,
                column->size, &(column->value_info));
            if (! odbc_check(odbc_env_handle, odbc_connection, stat_handle,
                rc))
            {
                odbc_do_cleanup_statement(Statement);
                odbc_throw();
                // not reached
            }
        }
    }
    DB = DB0;
").

%-----------------------------------------------------------------------------%

    % Fetch the next row of the current statement.
    %
:- pred fetch_row(odbc.statement::di, odbc.statement::uo, int::out,
    odbc.state::di, odbc.state::uo) is det.

:- pragma foreign_proc("C",
    fetch_row(Statement0::di, Statement::uo, Status::out, DB0::di, DB::uo),
    [promise_pure, may_call_mercury],
"
    Statement = Statement0;
    MR_assert(Statement != NULL);

    if (Statement->num_rows == 0 ) {
        MR_DEBUG(printf(""Fetching rows...\\n""));
    }

    // Fetching new row
    Status = SQLFetch(Statement->stat_handle);

    if (Status != SQL_NO_DATA_FOUND &&
        ! odbc_check(odbc_env_handle, odbc_connection,
            Statement->stat_handle, Status))
    {
        odbc_do_cleanup_statement(Statement);
        odbc_throw();
        // not reached
    }

    // Update number of rows fetched.
    if (Status == SQL_SUCCESS) {
        Statement->num_rows++;
    }

    if (Status == SQL_NO_DATA_FOUND) {
        MR_DEBUG(printf(""Fetched %d rows\\n"", Statement->num_rows));
    }

    DB = DB0;
").

%-----------------------------------------------------------------------------%

:- pred get_number_of_columns(int::out,
    odbc.statement::di, odbc.statement::uo, odbc.state::di, odbc.state::uo)
    is det.

:- pragma foreign_proc("C",
    get_number_of_columns(NumColumns::out, Statement0::di, Statement::uo,
        DB0::di, DB::uo),
    [promise_pure, will_not_call_mercury],
"
    Statement = Statement0;
    MR_assert(Statement != NULL);
    NumColumns = Statement->num_columns;
    DB = DB0;
").

%-----------------------------------------------------------------------------%

:- pred get_data(int::in, int::out, float::out, string::out, int::out,
    odbc.statement::di, odbc.statement::uo, odbc.state::di, odbc.state::uo)
    is det.

:- pragma foreign_proc("C",
    get_data(Column::in, Int::out, Flt::out, Str::out, Type::out,
        Statement0::di, Statement::uo, DB0::di, DB::uo),
    [promise_pure, may_call_mercury],
"
    MODBC_Column    *col;
    SQLRETURN       rc;
    SDWORD          column_info;

    Statement = Statement0;

    MR_assert(Statement != NULL);
    MR_assert(Statement->row != NULL);

    MR_DEBUG(printf(""Getting column %i\\n"", (int) Column));

    if (Statement->binding_type == MODBC_GET_DATA) {
        // Slurp up the data for this column.
        odbc_do_get_data(Statement, Column);
    }

    col = &(Statement->row[Column]);

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

            Int = (MR_Integer) data;

            MR_DEBUG(printf(""got integer %ld\\n"", (long) Int));

            // Check for overflow.
            if (Int != data) {
                MR_Word overflow_message;
                MODBC_overflow_message(&overflow_message);
                odbc_message_list =
                    MR_list_cons(overflow_message,
                        odbc_message_list);
                odbc_do_cleanup_statement(Statement);
                odbc_throw();
            }
            break;
        }

        case MODBC_FLOAT:
            Flt = (MR_Float) *(MODBC_C_FLOAT *)(col->data);

            MR_DEBUG(printf(""got float %f\\n"", Flt));

            break;

        case MODBC_STRING:
        case MODBC_TIME:
            MR_assert(col->data);
            MR_make_aligned_string_copy(Str, (char *) col->data);

            MR_DEBUG(printf(""got string %s\\n"", (char *) Str));

            break;

        case MODBC_VAR_STRING:
            // The data was allocated on the Mercury heap,
            // get it then kill the pointer so it can be GC'ed.
            MR_make_aligned_string(Str, (char *) col->data);

            MR_DEBUG(printf(""got var string %s\\n"", (char *) col->data));

            col->data = NULL;

            // As far as Mercury is concerned it's an ordinary string.
            Type = MODBC_STRING;
            break;

        default:
            MR_external_fatal_error(\"odbc.m\",
                \"invalid attribute type in odbc.get_data\");
            break;
    } // end switch (Type)

    DB = DB0;
").

:- pragma foreign_code("C", "
void
odbc_do_get_data(MODBC_Statement *statement, int column_id)
{
    MODBC_Column    *column;
    SQLRETURN       rc;
    SDWORD          column_info;
    char            dummy_buffer[1]; // Room for the NUL termination byte
                                     // and nothing else.

    column = &(statement->row[column_id]);
    if (column->attr_type == MODBC_VAR_STRING) {
        // Just get the length first time through.
        rc = SQLGetData(statement->stat_handle, column_id,
            column->conversion_type, dummy_buffer,
            1, &(column->value_info));

        // SQL_SUCCESS_WITH_INFO is expected here, since we didn't allocate
        // any space for the data, so don't collect the ""data truncated""
        // message.
        if (rc != SQL_SUCCESS_WITH_INFO &&
            ! odbc_check(odbc_env_handle, odbc_connection,
                statement->stat_handle, rc))
        {
            odbc_do_cleanup_statement(statement);
            odbc_throw();
        }

        if (column->value_info == SQL_NULL_DATA) {
            // The column is NULL, so there is no data to get.
            return;
        } else if (column->value_info == SQL_NO_TOTAL) {
            // The driver couldn't work out the length in advance, so
            // get the data in chunks of some arbitrary size, and append
            // the chunks together.
            // This method must be used with MODBC_IODBC,
            // since iODBC-2.12 uses a different interpretation
            // of the ODBC standard to Microsoft, for which
            // the length returned by the first call to SQLGetData
            // above is the minimum of the buffer length and the
            // length of the available data, rather than the
            // total length of data available.

            odbc_get_data_in_chunks(statement, column_id);
        } else {
            MR_Word data;

            // column->value_info == length of data
            column->size = column->value_info + 1;
            MR_incr_hp_atomic(data,
                (column->size + sizeof(MR_Word)) / sizeof(MR_Word));
            column->data = (MR_Word *) data;
            odbc_get_data_in_one_go(statement, column_id);
        }
    } else {
        // It's a fixed length column, so we can get the lot in one go.
        odbc_get_data_in_one_go(statement, column_id);
    }
}

void
odbc_get_data_in_one_go(MODBC_Statement *statement, int column_id)
{
    MODBC_Column    *col;
    SQLRETURN       rc;

    MR_DEBUG(printf(""getting column %i in one go\\n"", column_id));

    col = &(statement->row[column_id]);

    rc = SQLGetData(statement->stat_handle, column_id,
        col->conversion_type,
        (SQLPOINTER) col->data, col->size, &(col->value_info));

    if (! odbc_check(odbc_env_handle, odbc_connection,
        statement->stat_handle, rc))
    {
        odbc_do_cleanup_statement(statement);
        odbc_throw();
    }
}

void
odbc_get_data_in_chunks(MODBC_Statement *statement, int column_id)
{
    MODBC_Column    *col;
    SQLRETURN       rc;
    MR_Word         this_bit;
    MR_Word         chunk_list;
    MR_String       result;

    MR_DEBUG(printf(""getting column %i in chunks\\n"", column_id));

    chunk_list = MR_list_empty();

    col = &(statement->row[column_id]);

    rc = SQL_SUCCESS_WITH_INFO;

    MR_incr_hp_atomic(this_bit, MODBC_CHUNK_WORDS);

    // Keep collecting chunks until we run out.
    while (rc == SQL_SUCCESS_WITH_INFO) {
        rc = SQLGetData(statement->stat_handle, column_id,
            col->conversion_type, (SQLPOINTER) this_bit,
            MODBC_CHUNK_SIZE - 1, &(col->value_info));

        if (rc == SQL_NO_DATA_FOUND) {
            break;
        }

        if (rc != SQL_SUCCESS_WITH_INFO &&
            ! odbc_check(odbc_env_handle, odbc_connection,
                statement->stat_handle, rc))
        {
            odbc_do_cleanup_statement(statement);
            odbc_throw();
        }

        chunk_list = MR_list_cons(this_bit, chunk_list);
        MR_incr_hp_atomic(this_bit, MODBC_CHUNK_WORDS);
    }

    MODBC_odbc_condense_chunks(chunk_list, &result);
    col->data = (MR_Word *) result;
}
").

:- pragma foreign_export("C", overflow_message(out), "MODBC_overflow_message").
:- pred overflow_message(odbc.message::out) is det.

overflow_message(Error) :-
    ErrorType = error(execution_error(overflow)),
    ErrorMsg  = "[Mercury][odbc.m]Integer overflow detected in result set." ++
                "  Integers must be no larger than a word.",
    Error = ErrorType - ErrorMsg.

:- pragma foreign_export("C", condense_chunks(in, out),
    "MODBC_odbc_condense_chunks").
:- pred condense_chunks(list(string)::in, string::out) is det.

condense_chunks(RevChunks, String) :-
    list.reverse(RevChunks, Chunks),
    string.append_list(Chunks, String).

%-----------------------------------------------------------------------------%

:- pred cleanup_statement_check_error(odbc.statement::di,
    odbc.state::di, odbc.state::uo) is det.

:- pragma foreign_proc("C",
    cleanup_statement_check_error(Statement::di, DB0::di, DB::uo),
    [promise_pure, may_call_mercury],
"{
    MODBC_Statement *statement;
    SQLRETURN rc;

    statement = (MODBC_Statement *) Statement;

    rc = odbc_do_cleanup_statement(statement);
    if (! odbc_check(odbc_env_handle, odbc_connection, SQL_NULL_HSTMT, rc)) {
        odbc_throw();
    }
    DB = DB0;
}").

:- pragma foreign_code("C", "
SQLRETURN
odbc_do_cleanup_statement(MODBC_Statement *statement)
{
    int i;
    SQLRETURN rc;

    if (statement != NULL) {
        MR_DEBUG(printf(""cleaning up statement\\n""));
        if (statement->row != NULL) {
            for (i = 1; i <= statement->num_columns; i++) {
                // Variable length types are allocated directly
                // onto the Mercury heap, so don't free them here.
                if (!odbc_is_variable_length_sql_type(
                    statement->row[i].sql_type))
                {
                    MR_GC_free(statement->row[i].data);
                }
            }
            MR_GC_free(statement->row);
        }
        rc = SQLFreeStmt(statement->stat_handle, SQL_DROP);
        MR_GC_free(statement);
        return rc;
    } else {
        return SQL_SUCCESS;
    }
}
").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pragma foreign_code("C", "
// Map an ODBC SQL type to a supported attribute type.
// Currently, supported attribute types are minimal,
// but this function will allow us to ask ODBC to make
// conversion from SQL types to supported types.
// Binary types are currently converted to strings.

MODBC_AttrType
odbc_sql_type_to_attribute_type(SWORD sql_type)
{
    switch (sql_type) {
        case SQL_BIGINT:        return MODBC_STRING;
        case SQL_BINARY:        return MODBC_STRING;
        case SQL_BIT:           return MODBC_STRING;
        case SQL_CHAR:          return MODBC_STRING;
        case SQL_DATE:          return MODBC_TIME;
        case SQL_DECIMAL:       return MODBC_STRING;    // ?
        case SQL_DOUBLE:        return MODBC_FLOAT;
        case SQL_FLOAT:         return MODBC_FLOAT;
        case SQL_INTEGER:       return MODBC_INT;

    // For MySQL, SQLGetData does not work correctly (multiple calls
    // return the same data, not successive pieces of the data).
    // It seems to be guaranteed to be able to find the maximum length
    // of the data in the column, so we treat those columns as if
    // they were fixed length.
#ifdef MODBC_MYSQL
        case SQL_LONGVARBINARY:     return MODBC_STRING;
        case SQL_LONGVARCHAR:       return MODBC_STRING;
#else // ! MODBC_MYSQL
        case SQL_LONGVARBINARY:     return MODBC_VAR_STRING;
        case SQL_LONGVARCHAR:       return MODBC_VAR_STRING;
#endif // ! MODBC_MYSQL

        case SQL_NUMERIC:       return MODBC_STRING;
        case SQL_REAL:          return MODBC_FLOAT;
        case SQL_SMALLINT:      return MODBC_INT;
        case SQL_TIME:          return MODBC_TIME;
        case SQL_TIMESTAMP:     return MODBC_TIME;
        case SQL_TINYINT:       return MODBC_INT;
        case SQL_VARBINARY:     return MODBC_STRING;
        case SQL_VARCHAR:       return MODBC_STRING;
        default:
            MR_external_fatal_error(\"odbc.m\",
                \"sql_type_to_attribute_type: unknown type\");
    }
}

// Return the SQL_C type corresponding to a supported attribute type.
SWORD
odbc_attribute_type_to_sql_c_type(MODBC_AttrType AttrType)
{
    switch (AttrType) {
        case MODBC_FLOAT:       return SQL_C_DOUBLE;
        case MODBC_INT:         return SQL_C_SLONG;
        case MODBC_TIME:        return SQL_C_CHAR;
        case MODBC_STRING:      return SQL_C_CHAR;
        case MODBC_VAR_STRING:      return SQL_C_CHAR;
        default:
            // Unsupported MODBC_xxx type.
            MR_external_fatal_error(\"odbc.m\",
                \"attribute_type_to_sql_c_type: unknown type\");
    }
}

// Does the data have no maximum length?
// Note: the implementation of SQLGetData for MySQL does not follow the same
// standard as SQL Server, but from examination of the sources SQLDescribeCol
// seems guaranteed to find the maximum length of a result column containing
// variable length data. SQL_NO_TOTAL, which should be returned if the length
// cannot be determined, is not defined by the iODBC header files.
MR_bool
odbc_is_variable_length_sql_type(SWORD sql_type) {
#ifdef MODBC_MYSQL

    return MR_FALSE;

#else // ! MODBC_MYSQL

    return (
        sql_type == SQL_LONGVARBINARY ||
        sql_type == SQL_LONGVARCHAR
    );

#endif // !MODBC_MYSQL
}

// This function computes to total number of bytes needed
// to store an attribute value, returning -1 if there is no
// maximum size.
// [SqlType] is the ODBC SQL type of the column
// [cbColDef] is the size returned by SQLDescribeCol
// [ibScaler] is the scale returned by SQLDescribeCol
// [fNullable] is whether the column can be NULL
size_t
odbc_sql_type_to_size(SWORD sql_type, UDWORD cbColDef,
        SWORD ibScale, SWORD fNullable)
{
    switch (sql_type)
    {
        // 64-bit signed int converted to SQL_C_CHAR.
        case SQL_BIGINT:
            return 1 + cbColDef + 1;  // +1 for sign, +1 for NUL

        // Binary data converted to SQL_C_CHAR
        // Each byte is converted to 2-digit Hex.
        case SQL_BINARY:
            return cbColDef * 2 + 1;  // +1 for NUL

        // Bit converted to SQL_C_CHAR.
        case SQL_BIT:
            return cbColDef + 1;  // +1 for NUL

        // Fixed char to SQL_C_CHAR.
        case SQL_CHAR:
            return cbColDef + 1;  // 1 for NUL

        // Date YYYY-MM-DD converted to SQL_C_CHAR.
        case SQL_DATE:
            return cbColDef + 1;  // 1 for NUL

        // Signed decimal ddd.dd converted to SQL_C_CHAR.
        case SQL_DECIMAL:
            return 1 + cbColDef + 1 + ibScale + 1;
            // 1 for sign 1, 1 for decimal point, 1, for NUL

        // 32-bit float converted to MODBC_SQL_C_FLOAT.
        case SQL_DOUBLE:
            return sizeof(MODBC_C_FLOAT);

        // 32-bit float converted to MODBC_SQL_C_FLOAT.
        case SQL_FLOAT:
            return sizeof(MODBC_C_FLOAT);

        // 32-bit integer converted to SQL_C_SLONG.
        case SQL_INTEGER:
            return sizeof(MODBC_C_INT);

        // Any length binary convert to SQL_C_CHAR
        // For MySQL, there are no column types for
        // which the maximum length cannot be determined before
        // starting to fetch data, hence the #ifdefs below.
        case SQL_LONGVARBINARY:
#ifdef MODBC_MYSQL
            return cbColDef * 2 + 1;    // 1 for NUL
#else // !MODBC_MYSQL
            return -1;
#endif // !MODBC_MYSQL

        // Any length char convert to SQL_C_CHAR
        // For MySQL, there are no column types for
        // which the maximum length cannot be determined before
        // starting to fetch data, hence the #ifdefs below.
        case SQL_LONGVARCHAR:
#ifdef MODBC_MYSQL
            return cbColDef + 1;    // 1 for NUL
#else // !MODBC_MYSQL
            return -1;
#endif // !MODBC_MYSQL

        // Signed numeric ddd.dd converted to SQL_C_CHAR.
        case SQL_NUMERIC:
            return 1 + cbColDef + 1 + ibScale + 1; // 1 for NUL

        // 32-bit float converted to MODBC_SQL_C_FLOAT.
        case SQL_REAL:
            return sizeof(MODBC_C_FLOAT);

        // 16-bit integer converted to SQL_C_SLONG.
        case SQL_SMALLINT:
            return sizeof(MODBC_C_INT);

        // Time hh:mm:ss converted to SQL_C_CHAR.
        case SQL_TIME:
            return cbColDef + 1;  // 1 for NUL

        // Time YYYY-MM-DD hh:mm:ss converted to SQL_C_CHAR.
        case SQL_TIMESTAMP:
            return cbColDef + 1;  // 1 for NUL

        // 8-bit integer converted to MODBC_SQL_INT.
        case SQL_TINYINT:
            return sizeof(MODBC_C_INT);

        // Binary data converted to SQL_C_CHAR.
        // Each byte is converted to 2-digit Hex.
        case SQL_VARBINARY:
            return cbColDef * 2 + 1;  // 1 for NUL

        // Fixed char to SQL_C_CHAR.
        case SQL_VARCHAR:
            return cbColDef + 1;  // 1 for NUL

        default:
            MR_external_fatal_error(\"odbc.m\",
                \"sql_type_to_size: unknown type\");
    }
}
").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% Catalog functions
%

data_sources(MaybeSources - Messages, !IO) :-
    sql_data_sources(RevSourceNames, RevDescs, Status, RevMessages, !IO),
    ( if odbc.ok(Status) then
        list.reverse(RevMessages, Messages),
        list.reverse(RevSourceNames, SourceNames),
        list.reverse(RevDescs, Descs),
        assoc_list.from_corresponding_lists(SourceNames, Descs, SourceAL),
        MakeSource =
            ( pred(Pair::in, SourceDesc::out) is det :-
                Pair = SourceName - Desc,
                SourceDesc = odbc.source_desc(SourceName, Desc)
            ),
        list.map(MakeSource, SourceAL, Sources),
        MaybeSources = ok(Sources)
    else if odbc.no_data(Status)then
        % iODBC 2.12 doesn't implement this function.
        Messages = [
            error(feature_not_implemented) -
            "[Mercury][odbc.m]SQLDataSources not implemented."
        ],
        MaybeSources = error
    else
        list.reverse(RevMessages, Messages),
        MaybeSources = error
    ).

:- pred sql_data_sources(list(string)::out, list(string)::out, int::out,
    list(odbc.message)::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    sql_data_sources(SourceNames::out, SourceDescs::out, Status::out,
        Messages::out, _IO0::di, _IO::uo),
    [promise_pure, may_call_mercury],
"
    Status = odbc_do_get_data_sources(&SourceNames, &SourceDescs, &Messages);
").

:- pragma foreign_decl("C", "
SQLRETURN
odbc_do_get_data_sources(MR_Word *SourceNames, MR_Word *SourceDescs,
        MR_Word *Messages);
").

:- pragma foreign_code("C", "
SQLRETURN
odbc_do_get_data_sources(MR_Word *SourceNames, MR_Word *SourceDescs,
        MR_Word *Messages)
{
    SQLCHAR dsn[SQL_MAX_DSN_LENGTH];
    SQLCHAR desc[128];

    // Arbitrary size, only needs to hold a
    // descriptive string like ""SQL Server"".

    MR_String new_dsn;
    MR_String new_desc;
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

    MR_DEBUG(printf(""SQLAllocEnv status: %d\\n"", rc));

    if (odbc_check(odbc_env_handle, SQL_NULL_HDBC, SQL_NULL_HSTMT, rc)) {
        rc = SQLDataSources(odbc_env_handle, SQL_FETCH_FIRST,
            dsn, SQL_MAX_DSN_LENGTH - 1,
            &dsn_len, desc, sizeof(desc), &desc_len);

        // The documentation varies on whether the driver
        // returns SQL_NO_DATA_FOUND or SQL_NO_DATA, so check for both.
        while (rc != SQL_NO_DATA_FOUND && rc != SQL_NO_DATA &&
            odbc_check(odbc_env_handle, SQL_NULL_HDBC,
                SQL_NULL_HSTMT, rc))
        {
            // Copy the new data onto the Mercury heap
            MR_make_aligned_string_copy(new_dsn, (MR_String)dsn);
            *SourceNames = MR_list_cons((MR_Word)new_dsn, *SourceNames);
            MR_make_aligned_string_copy(new_desc, (MR_String)desc);
            *SourceDescs = MR_list_cons((MR_Word)new_desc, *SourceDescs);

            rc = SQLDataSources(odbc_env_handle,
                SQL_FETCH_NEXT, dsn, SQL_MAX_DSN_LENGTH - 1, &dsn_len,
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

tables(Qualifier, Owner, TableName, Tables, !DB) :-
    convert_pattern_argument(Qualifier, QualifierStr, QualifierStatus),
    convert_pattern_argument(Owner, OwnerStr, OwnerStatus),
    convert_pattern_argument(TableName, TableStr, TableStatus),
    do_aggregate(odbc.sql_tables(QualifierStr, QualifierStatus,
        OwnerStr, OwnerStatus, TableStr, TableStatus),
        list.cons, [], Results0, !DB),
    list.reverse(Results0, Results),
    ( if list.map(convert_table_desc, Results, Tables0)then
        Tables = Tables0
    else
        add_message(error(internal_error) -
            "[Mercury][odbc.m]Invalid results from SQLTables.", !DB),
        odbc.throw(!DB)
    ).

:- pred convert_table_desc(odbc.row::in, odbc.table_desc::out) is semidet.

convert_table_desc(Row0, Table) :-
    NullToEmptyStr =
        ( pred(Data0::in, Data::out) is det :-
            ( if Data0 = null then Data = string("") else Data = Data0 )
        ),
    list.map(NullToEmptyStr, Row0, Row),
    Row = [string(Qualifier), string(Owner), string(Name),
        string(Type), string(Description) | DriverColumns],
    Table = odbc.table_desc(Qualifier, Owner, Name,
        Type, Description, DriverColumns).

%-----------------------------------------------------------------------------%

    % convert_pattern_argument(Pattern, String, Status).
    % This is used in a fairly crude interface to C. If the Status is 0,
    % the corresponding argument to the ODBC function should be NULL,
    % meaning no constraint on the search. If the Status is 1, the
    % argument to the ODBC function should be the given string.
    %
:- pred convert_pattern_argument(search_pattern::in, string::out, int::out)
    is det.

convert_pattern_argument(any, "", 0).
convert_pattern_argument(pattern(Str), Str, 1).

:- pred sql_tables(string::in, int::in, string::in, int::in, string::in,
    int::in, odbc.statement::di, odbc.statement::uo,
    odbc.state::di, odbc.state::uo) is det.

:- pragma foreign_proc("C",
    sql_tables(QualifierStr::in, QualifierStatus::in,
        OwnerStr::in, OwnerStatus::in, TableStr::in, TableStatus::in,
        Statement0::di, Statement::uo, DB0::di, DB::uo),
    [may_call_mercury, promise_pure],
"
    char *qualifier_str = NULL;
    char *owner_str = NULL;
    char *table_str = NULL;
    int qualifier_len = 0;
    int owner_len = 0;
    int table_len = 0;
    SQLRETURN rc;

    Statement = Statement0;

    // A NULL pointer in any of the string pattern fields
    // means no constraint on the search for that field.
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

    rc = SQLTables(Statement->stat_handle, (SQLCHAR *)qualifier_str,
        qualifier_len, (SQLCHAR *)owner_str, owner_len,
        (SQLCHAR *)table_str, table_len, NULL, 0);
    if (! odbc_check(odbc_env_handle, odbc_connection,
            Statement->stat_handle, rc))
    {
        odbc_do_cleanup_statement(Statement);
        odbc_throw();
    }

    DB = DB0;
").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% Error checking
%

:- pred odbc.ok(int::in) is semidet.

:- pragma foreign_proc("C",
    odbc.ok(Status::in),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    SUCCESS_INDICATOR =
        (Status == SQL_SUCCESS || Status == SQL_SUCCESS_WITH_INFO);
").

:- pred odbc.no_data(int::in) is semidet.

:- pragma foreign_proc("C",
    odbc.no_data(Status::in),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    SUCCESS_INDICATOR = (Status == SQL_NO_DATA_FOUND);
").

%-----------------------------------------------------------------------------%

    % Handle ODBC error codes. Refer to the ODBC API Reference
    % provided with the ODBC SDK. The first two characters of the
    % SQLSTATE are meant to specify an error class. Looking at the
    % predicates below, the classes weren't terribly well chosen.
    %
:- pred sql_state_to_message(string::in, string::in,
    odbc.message::out) is det.
:- pragma foreign_export("C", sql_state_to_message(in, in, out),
    "MODBC_odbc_sql_state_to_message").

sql_state_to_message(SQLState, String, Message - String) :-
    string.split(SQLState, 2, Class, SubClass),
    ( if Class = "01" then
        ( if sql_state_to_warning(SubClass, Warning) then
            Message = warning(Warning)
        else
            Message = warning(general_warning)
        )
    else
        ( if sql_state_to_error(Class, SubClass, Error) then
            Message = error(Error)
        else
            Message = error(general_error)
        )
    ).

:- pred sql_state_to_warning(string::in, odbc.warning::out) is semidet.

sql_state_to_warning("000", general_warning).
sql_state_to_warning("001", general_warning).
sql_state_to_warning("002", disconnect_error).
sql_state_to_warning("003", null_value_in_set_function).
sql_state_to_warning("004", string_data_truncated).
sql_state_to_warning("006", privilege_not_revoked).
sql_state_to_warning("007", privilege_not_granted).
sql_state_to_warning("S03", general_warning).
sql_state_to_warning("S04", general_warning).

:- pred sql_state_to_error(string::in, string::in, odbc.error::out) is semidet.

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
    ( if SubClass = "000" then
        Error = general_error
    else if SubClass = "109" then
        Error = feature_not_implemented
    else if SubClass = "T00" then
        Error = timeout_expired
    else if SubClass = "T01" then
        Error = connection_error(timeout_expired)
    else
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
    ( if SubClass = "000" then
        Error = general_error
    else if SubClass = "C00" then
        Error = feature_not_implemented
    else if SubClass = "T01" then
        Error = connection_error(timeout_expired)
    else
        Error = internal_error
    ).

:- pragma foreign_code("C", "
// Return MR_TRUE if the last ODBC call succeeded.
// Return MR_FALSE if the ODBC call failed.
// Add any error messages to odbc_message_list.
MR_bool
odbc_check(SQLHENV env_handle, SQLHDBC connection_handle,
        SQLHSTMT statement_handle, SQLRETURN rc)
{
    SQLRETURN   status;
    SQLINTEGER  native_error;
    SQLSMALLINT msg_len;
    UCHAR       message[SQL_MAX_MESSAGE_LENGTH];
    UCHAR       sql_state[SQL_SQLSTATE_SIZE + 1];
    MR_String   mercury_message;
    MR_Word     new_message;

    MR_ASSERT_IMPLY(connection_handle == SQL_NULL_HDBC,
        statement_handle == SQL_NULL_HSTMT);

    odbc_ret_code = rc;

    // Check type of error.
    if (rc == SQL_SUCCESS) {
        return MR_TRUE;
    } else {
        MR_DEBUG(printf(""getting error message for status %i\\n"", rc));

        while (1) {
            status = SQLError(env_handle, connection_handle,
                statement_handle, sql_state, &native_error,
                message, SQL_MAX_MESSAGE_LENGTH - 1, &msg_len);

            MR_DEBUG(printf(""SQLError status: %i\\n"", status));
            MR_DEBUG(printf(""SQL_STATE: %s\\n"", sql_state));
            MR_DEBUG(printf(""Error: %s\\n"", message));

            if (status != SQL_SUCCESS) {
                break;
            }

            // Copy the error string to the Mercury heap.
            MR_make_aligned_string_copy(mercury_message, (char *)message);

            // Convert the SQL state to an odbc__message.
            MODBC_odbc_sql_state_to_message((MR_String)sql_state,
                mercury_message, &new_message);

            // Append the message onto the list.
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

%-----------------------------------------------------------------------------%
:- end_module odbc.
%-----------------------------------------------------------------------------%
