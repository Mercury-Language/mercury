
:- module csv.

:- interface.

:- import_module float.
:- import_module int.
:- import_module io.
:- import_module list.
:- import_module string.

:- type csv
    --->    csv(
                    csv_header  :: list(string),
                    csv_rows    :: list(csv_row)
            ).

:- type csv_row
    --->    csv_row(
                    csv_fields  :: list(csv_data)
            ).

:- type csv_data
    --->    csv_data_int(int)
    ;       csv_data_str(string)
    ;       csv_data_float(float).


:- pred write_csv(string::in, csv::in, io::di, io::uo) is det.

:- pred csv_to_string(csv::in, string::out) is det.

:- implementation.

:- import_module require.

write_csv(Filename, CSV, !IO) :-
    csv_to_string(CSV, String),
    open_output(Filename, Result, !IO),
    (
        Result = ok(Stream),
        io.write_string(Stream, String, !IO),
        close_output(Stream, !IO)
    ;
        Result = error(Error),
        error(error_message(Error))
    ).

csv_to_string(CSV, String) :-
    HeaderStrings = CSV ^ csv_header,
    HeaderString = string.join_list(",", HeaderStrings),
    map(csv_row_to_string, CSV ^ csv_rows, RowStrings),
    RowsString = string.join_list("\n", RowStrings),
    String = HeaderString ++ "\n" ++ RowsString.

:- pred csv_row_to_string(csv_row::in, string::out) is det.

csv_row_to_string(Row, String) :-
    map(csv_field_to_string, Row ^ csv_fields, FieldStrings),
    String = string.join_list(",", FieldStrings).

:- pred csv_field_to_string(csv_data::in, string::out) is det.

csv_field_to_string(csv_data_int(Int), string(Int)).
% XXX: Excape the string.
csv_field_to_string(csv_data_str(String), format("\"%s\"", [s(String)])).
csv_field_to_string(csv_data_float(Float), string(Float)).


