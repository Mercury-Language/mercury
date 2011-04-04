%-----------------------------------------------------------------------------%

:- module utf8_io.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module require.
:- import_module string.

%-----------------------------------------------------------------------------%

main(!IO) :-
    io.write_string("********************\n", !IO),
    io.write_string("** Standard input **\n", !IO),
    io.write_string("********************\n\n", !IO),
    do_test(!IO),

    io.write_string("\n***********************\n", !IO),
    io.write_string(  "** Text stream input **\n", !IO),
    io.write_string(  "***********************\n\n", !IO),
    io.see("utf8_io.inp", SeeRes, !IO),
    (
        SeeRes = ok,
        do_test(!IO),
        io.seen(!IO)
    ;
        SeeRes = error(Error),
        io.write(Error, !IO),
        io.nl(!IO)
    ).

:- pred do_test(io::di, io::uo) is det.

do_test(!IO) :-
    io.write_string("read_char:\n", !IO),
    io.read_char(RC, !IO),
    io.write(RC, !IO),
    io.nl(!IO),

    (
        RC = ok(C),
        io.write_string("\nputback_char:\n", !IO),
        io.putback_char(C, !IO),
        io.read_char(RC2, !IO),
        io.write(RC2, !IO),
        io.nl(!IO)
    ;
        RC = eof
    ;
        RC = error(_)
    ),
    io.ignore_whitespace(_, !IO),

    io.write_string("\nread_word:\n", !IO),
    io.read_word(RW, !IO),
    io.write(RW, !IO),
    io.nl(!IO),
    io.read_word(RW2, !IO),
    io.write(RW2, !IO),
    io.nl(!IO),
    io.ignore_whitespace(_, !IO),

    io.write_string("\nread_line:\n", !IO),
    io.read_line(RL, !IO),
    io.write(RL, !IO),
    io.nl(!IO),

    io.write_string("\nread_line_as_string:\n", !IO),
    io.read_line_as_string(RLAS, !IO),
    io.write(RLAS, !IO),
    io.nl(!IO),

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
    io.read_file_as_string(RFAS, !IO),
    io.write(RFAS, !IO),
    io.nl(!IO).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=8 sts=4 sw=4 et
