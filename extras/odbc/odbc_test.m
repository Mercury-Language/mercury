%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
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

:- pred main(io::di, io::uo) is cc_multi.

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

main(!IO) :-
    odbc.data_sources(SourceResult - SourceMessages, !IO),
    (
        SourceResult = ok(Sources),
        io.write_string("Available data source names:", !IO),
        io.nl(!IO),
        io.write_list(Sources, "\n", io.write, !IO),
        io.nl(!IO)
    ;
        SourceResult = error,
        io.write_string("Error getting DSNs:", !IO),
        io.nl(!IO)
    ),
    io.write_list(SourceMessages, "\n", io.write, !IO),
    io.nl(!IO),
    odbc.transaction("test", "", "", odbc.tables(any, any, any),
        TableResult - TableMessages, !IO),
    (
        TableResult = ok(Tables),
        io.write_string("Available tables:", !IO),
        io.nl(!IO),
        io.write_list(Tables, "\n", io.write, !IO),
        io.nl(!IO)
    ;
        TableResult = error,
        io.write_string("Error getting tables:", !IO),
        io.nl(!IO)
    ),
    io.write_list(TableMessages, "\n", io.write, !IO),
    io.nl(!IO),

    odbc.transaction("test", "", "", test_trans,
        TransResult - TransMessages, !IO),
    (
        TransResult = ok(Results),
        io.write_string("transaction ok: ", !IO),
        list.length(Results, NumRows),
        io.write_int(NumRows, !IO),
        io.write_string(" result rows", !IO),
        io.nl(!IO),
        io.write_list(Results, "\n", io.write, !IO),
        io.nl(!IO)
    ;
        TransResult = error,
        io.write_string("error in transaction:\n", !IO)
    ),
    io.write_list(TransMessages, "\n", io.write, !IO),
    io.nl(!IO),

    try_io(odbc.transaction("test", "", "", test_trans_2),
        ExceptionResult, !IO),
    (
        ExceptionResult = succeeded(Results2),
        io.set_exit_status(1, !IO),
        io.write_string("Error: expected exception, got results:", !IO),
        io.write(Results2, !IO),
        io.nl(!IO)
    ;
        ExceptionResult = exception(Exception),
        det_univ_to_type(Exception, ExceptionString),
        io.write_string("Got exception: ", !IO),
        io.write_string(ExceptionString, !IO),
        io.nl(!IO)
    ).

:- pred test_trans(list(odbc.row)::out, odbc.state::di, odbc.state::uo) is det.

test_trans(Results, !DB) :-
    odbc.solutions("select * from test", Results, !DB).

:- pred test_trans_2(list(odbc.row)::out, odbc.state::di, odbc.state::uo)
    is det.

test_trans_2(Results, !DB) :-
    odbc.solutions("select * from test", Results, !DB),
    ( if semidet_succeed then
        throw("exception in test_trans_2")
    else
        true
    ).

:- pred output_results(list(odbc.row)::in, io::di, io::uo) is det.

output_results(Rows, !IO) :-
    io.write_list(Rows, "\n", output_row, !IO).

:- pred output_row(odbc.row::in, io::di, io::uo) is det.

output_row(Row, !IO) :-
    io.write_list(Row, " ", output_attribute, !IO).

:- pred output_attribute(odbc.attribute::in, io::di, io::uo) is det.

output_attribute(null, !IO) :-
    io.write_string("<NULL>", !IO).
output_attribute(int(Int), !IO)  :-
    io.write_int(Int, !IO).
output_attribute(string(Str), !IO) :-
    io.write_string(Str, !IO).
output_attribute(float(Float), !IO) :-
    io.write_float(Float, !IO).
output_attribute(time(String), !IO) :-
    io.write_string(String, !IO).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
