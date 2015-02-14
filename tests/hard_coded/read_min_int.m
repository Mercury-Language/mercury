%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test reading terms with min_int arguments.

:- module read_min_int.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module string.

:- type foo
    --->    foo(int).

main(!IO) :-
    % Test io.read.
    test_stdin(!IO),
    io.nl(!IO),

    % Test io.read_from_string.
    io.open_input("read_min_int.inp", OpenRes, !IO),
    (
        OpenRes = ok(Stream),
        test_read_from_string(Stream, !IO),
        io.close_input(Stream, !IO)
    ;
        OpenRes = error(Error),
        io.write(Error, !IO),
        io.nl(!IO)
    ).

:- pred test_stdin(io::di, io::uo) is det.

test_stdin(!IO) :-
    io.read(Res, !IO),
    (
        Res = ok(X : foo),
        io.write(X, !IO),
        io.nl(!IO),
        test_stdin(!IO)
    ;
        Res = error(Error, _),
        io.write_string(Error, !IO),
        io.nl(!IO),
        test_stdin(!IO)
    ;
        Res = eof
    ).

:- pred test_read_from_string(io.input_stream::in, io::di, io::uo) is det.

test_read_from_string(Stream, !IO) :-
    io.read_line_as_string(Stream, IORes, !IO),
    (
        IORes = ok(String),
        FileName = "",
        Posn0 = posn(1, 0, 0),
        io.read_from_string(FileName, String, length(String), Res,
            Posn0, _Posn),
        (
            Res = ok(X : foo),
            io.write(X, !IO),
            io.nl(!IO)
        ;
            Res = eof
        ;
            Res = error(Error, _),
            io.write_string(Error, !IO),
            io.nl(!IO)
        ),
        test_read_from_string(Stream, !IO)
    ;
        IORes = eof
    ;
        IORes = error(IOError),
        io.write_string(io.error_message(IOError), !IO),
        io.nl(!IO)
    ).
