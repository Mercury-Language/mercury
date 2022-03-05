%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module utf8_io.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module require.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    io.write_string("********************\n", !IO),
    io.write_string("** Standard input **\n", !IO),
    io.write_string("********************\n\n", !IO),
    io.stdin_stream(StdIn, !IO),
    do_test(StdIn, !IO),

    io.write_string("\n***********************\n", !IO),
    io.write_string(  "** Text stream input **\n", !IO),
    io.write_string(  "***********************\n\n", !IO),
    io.open_input("utf8_io.inp", OpenResult, !IO),
    (
        OpenResult = ok(InputStream),
        do_test(InputStream, !IO),
        io.close_input(InputStream, !IO)
    ;
        OpenResult = error(Error),
        io.write_line(Error, !IO)
    ).

:- pred do_test(io.text_input_stream::in, io::di, io::uo) is det.

do_test(InputStream, !IO) :-
    io.write_string("read_char:\n", !IO),
    io.read_char(InputStream, RC, !IO),
    io.write_line(RC, !IO),

    (
        RC = ok(C),
        io.write_string("\nputback_char:\n", !IO),
        io.putback_char(InputStream, C, !IO),
        io.read_char(InputStream, RC2, !IO),
        io.write_line(RC2, !IO)
    ;
        RC = eof
    ;
        RC = error(_)
    ),
    io.ignore_whitespace(_, !IO),

    io.write_string("\nread_word:\n", !IO),
    io.read_word(InputStream, RW, !IO),
    io.write_line(RW, !IO),
    io.read_word(InputStream, RW2, !IO),
    io.write_line(RW2, !IO),
    io.ignore_whitespace(InputStream, _, !IO),

    io.write_string("\nread_line:\n", !IO),
    io.read_line(InputStream, RL, !IO),
    io.write_line(RL, !IO),

    io.write_string("\nread_line_as_string:\n", !IO),
    io.read_line_as_string(InputStream, RLAS, !IO),
    io.write_line(RLAS, !IO),

    io.write_string("\nwrite_char:\n", !IO),
    io.write_char('ß', !IO),
    io.write_char('ξ', !IO),
    io.write_char('啕', !IO),
    io.nl(!IO),

    io.write_string("\nformat:\n", !IO),
    io.format("%c.%c.%c\n", [c('ß'), c('ξ'), c('啕')], !IO),
    io.format("<%3c><%3c><%-3c>\n", [c('ß'), c('ξ'), c('啕')], !IO),
    io.format("<%4s><%-4.1s>\n", [s("aß"), s("ξ啕")], !IO),

    io.write_string("\nread_file_as_string:\n", !IO),
    io.read_file_as_string(InputStream, RFAS, !IO),
    io.write_line(RFAS, !IO).
