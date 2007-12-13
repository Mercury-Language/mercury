% vim: ft=mercury ts=4 sw=4 et
% This is a regression test for a bug in construct.get_functor/5,
% where it was not handling equivalence types properly.

:- module construct_bug.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module construct_bug_submodule.

:- import_module int.
:- import_module list.
:- import_module require.
:- import_module std_util.
:- import_module string.

main(!IO) :-
    FileName = "temp.term",

    call(
        (pred(!.S::in, !:S::out) is det:-
        count(["A"], !S),
        count(["A","A2"], !S),
        count(["B"], !S),
        count(["B","B1"], !S),
        count(["B","B2"], !S),
        count(["B","B3"], !S),
        count(["C"], !S),
        count(["C","C1"], !S)
        ), construct_bug_submodule.init, Map),

    io.open_binary_output(FileName, Result, !IO),
    ( Result = ok(Temp_ORDIE_Out_OutStream) ->
        OutStream = Temp_ORDIE_Out_OutStream
    ;
        error( "Failed to write to '" ++ FileName ++ "'.")
    ),
    io.write_binary(OutStream, Map, !IO),
    close_binary_output(OutStream, !IO),

    io.write_string("Saved the map to file: " ++ FileName ++ "\n", !IO),

    io.open_binary_input(FileName, Result2, !IO),
    ( Result2 = ok(Temp_ORDIE_Out_InStream) ->
        InStream = Temp_ORDIE_Out_InStream
    ;
        error( "Failed to open '" ++ FileName ++ "'.")
    ),
    io.read_binary(InStream, MayDataTerm, !IO),
    io.close_binary_input(InStream, !IO),
    (
        MayDataTerm = ok(ReadMap `with_type` stat)
    ;
        MayDataTerm = eof,
        error("Unexpected end of file: '" ++ FileName ++ "'.")
    ;
        MayDataTerm = error(E),
        error("Error reading term from: '" ++ FileName ++ "': "
            ++ io.error_message(E) ++ ".")
    ),

    io.write_string("Loaded the map from file: " ++ FileName ++ "\n", !IO),

    ( Map = ReadMap ->
        io.write_string("The representations are identical.\n", !IO)
    ;
        io.write_string("The representations are different.\n", !IO)
    ),

    io.remove_file(FileName, RmResult, !IO),
    (
        RmResult = ok
    ;
        RmResult = error(E2),
        error("Error deleting file '" ++ FileName ++ "': "
            ++ io.error_message(E2) ++ ".")
    ).
