% vim: ts=4 sw=4 et ft=mercury
%
% This is a regression test for bug #28 in Mantis.

:- module testxmlreader.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module require.
:- import_module string.
:- import_module xmlreader.

main(!IO) :-
    io.command_line_arguments(Args, !IO),
    ( Args = [TFN] ->
        FN = TFN
    ;
        error("usage: testxmlreader file.xml")
    ),
    xmlreader.open_file(FN, MayR, !IO),
    (
        MayR = yes(R),
        dump_all_and_close(R, !IO)
    ;
        MayR = no,
        error("Cannot read '" ++ FN ++ "'.")
    ).

:- pred dump_all_and_close(xmlreader::di, io::di, io::uo) is det.

dump_all_and_close(R, !IO) :-
    read(E, R, R2),
    (
        E = eof,
        close_reader(R2, !IO)
    ;
        E = error(Err),
        close_reader(R2, !IO),
        error("Parsing error: "++int_to_string(Err))
    ;
        E = node(D, T, N, Empty, MV),
        (
            MV = yes(V),
            ( length(V) > 40 ->
                UseV = string.left(V, 40)
            ;
                UseV = V
            )
        ;
            MV = no,
            UseV = ""
        ),
        io.format("%d %d %s %s %s\n",
            [i(D), i(T), s(N), s(string(Empty)), s(UseV)], !IO),
        dump_all_and_close(R2, !IO),
        % io.write_string("", !IO), % prevent tail recursion
        true
    ).
