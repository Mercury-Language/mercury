%-----------------------------------------------------------------------------%
% Sample program for odbc.m.
% Author: stayl
% This source file is hereby placed in the public domain.  -stayl.
%
% Assumes that there is an ODBC data source "test" containing a table
% named "test".
%-----------------------------------------------------------------------------%
:- module odbc_test.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is cc_multi.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module odbc.

:- import_module exception.
:- import_module list.
:- import_module pair.
:- import_module string.
:- import_module univ.

%-----------------------------------------------------------------------------%

main -->
	odbc__data_sources(SourceResult - SourceMessages),
	( 
		{ SourceResult = ok(Sources) },
		io__write_string("Available data source names:"),
		io__nl,
		io__write_list(Sources, "\n", io__write),
		io__nl
	;
		{ SourceResult = error },
		io__write_string("Error getting DSNs:"),
		io__nl
	),
	io__write_list(SourceMessages, "\n", io__write),
	io__nl,
	odbc__transaction("test", "", "", odbc__tables(any, any, any), 
			TableResult - TableMessages),
	(
		{ TableResult = ok(Tables) },
		io__write_string("Available tables:"),
		io__nl,
		io__write_list(Tables, "\n", io__write),
		io__nl
	;
		{ TableResult = error },
		io__write_string("Error getting tables:"),
		io__nl
	),
	io__write_list(TableMessages, "\n", io__write),
	io__nl,

	odbc__transaction("test", "", "", test_trans, 
		TransResult - TransMessages),
	( 
		{ TransResult = ok(Results) },
		io__write_string("transaction ok: "),
		{ list__length(Results, NumRows) },
		io__write_int(NumRows),
		io__write_string(" result rows"),
		io__nl,
		io__write_list(Results, "\n", io__write),
		io__nl
	;
		{ TransResult = error },
		io__write_string("error in transaction:\n")
	),
	io__write_list(TransMessages, "\n", io__write),
	io__nl,

	try_io(odbc__transaction("test", "", "", test_trans_2),
		ExceptionResult),
	(
		{ ExceptionResult = succeeded(Results2) },
		io__set_exit_status(1),
		io__write_string("Error: expected exception, got results:"),
		io__write(Results2),
		io__nl
	;
		{ ExceptionResult = exception(Exception) },
		{ det_univ_to_type(Exception, ExceptionString) },
		io__write_string("Got exception: "),
		io__write_string(ExceptionString),
		io__nl
	).

:- pred test_trans(list(odbc__row)::out,
		odbc__state::di, odbc__state::uo) is det.

test_trans(Results) -->
	odbc__solutions("select * from test", Results).

:- pred test_trans_2(list(odbc__row)::out,
		odbc__state::di, odbc__state::uo) is det.

test_trans_2(Results) -->
	odbc__solutions("select * from test", Results),
	( { semidet_succeed } ->
		{ throw("exception in test_trans_2") }
	;	
		[]
	).

:- pred output_results(list(odbc__row)::in,
		io__state::di, io__state::uo) is det.

output_results(Rows) -->
	io__write_list(Rows, "\n", output_row).

:- pred output_row(odbc__row::in, io__state::di, io__state::uo) is det.

output_row(Row) -->
	io__write_list(Row, " ", output_attribute).

:- pred output_attribute(odbc__attribute::in, 
		io__state::di, io__state::uo) is det.

output_attribute(null) -->
	io__write_string("<NULL>").
output_attribute(int(Int)) -->
	io__write_int(Int).
output_attribute(string(Str)) -->
	io__write_string(Str).
output_attribute(float(Float)) -->
	io__write_float(Float).
output_attribute(time(String)) -->
	io__write_string(String).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
