%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module tailrec1.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.
:- import_module string.

main(!IO) :-
    io.open_input("tailrec1.data", Result, !IO),
    (
        Result = ok(Stream),
        tailrec1_read_strings(Stream, [], Words, !IO),
        tailrec1_length(Words, Length),
        tailrec1_write_strings(Words, !IO),
        io.write_int(Length, !IO),
        io.nl(!IO)
    ;
        Result = error(Error),
        io.error_message(Error, Msg),
        io.write_string(Msg, !IO),
        io.nl(!IO)
    ).

:- pred tailrec1_read_strings(input_stream::in,
    list(string)::in, list(string)::out, io::di, io::uo) is det.

tailrec1_read_strings(Stream, !Words, !IO) :-
    tailrec1_read_line(Stream, Word, !IO),
    ( if Word = "" then
        true
    else
        !:Words = [Word | !.Words],
        tailrec1_read_strings(Stream, !Words, !IO)
    ).

:- pred tailrec1_read_line(io.input_stream::in, string::out, io::di, io::uo)
    is det.

:- pragma foreign_proc("C",
    tailrec1_read_line(Stream::in, Line::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    /* this needs to be big enough only for the lines in tailrec1.data. */
    char    buf[100];
    int     c;
    int     i;

    i = 0;
    while ((c = mercury_get_byte((MercuryFilePtr) Stream)) != EOF &&
        c != '\\n')
    {
        if (i < 100) {
            buf[i] = c;
        }

        i++;
    }

    buf[i] = '\\0';
    MR_make_aligned_string_copy(Line, buf);
    IO = IO0;
").

:- pred tailrec1_length(list(T)::in, int::out) is det.

tailrec1_length(List, Length) :-
    tailrec1_length_2(List, 0, Length).

:- pred tailrec1_length_2(list(T)::in, int::in, int::out) is det.

tailrec1_length_2([], !Length).
tailrec1_length_2([_X | Xs], !Length) :-
    !:Length = !.Length + 1,
    tailrec1_length_2(Xs, !Length).

:- pred tailrec1_write_strings(list(string)::in, io::di, io::uo) is det.

tailrec1_write_strings([], !IO).
tailrec1_write_strings([Word | Words], !IO) :-
    io.write_string(Word, !IO),
    io.nl(!IO),
    tailrec1_write_strings(Words, !IO).
