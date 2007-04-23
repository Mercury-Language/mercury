% vim: ft=mercury ts=4 sw=4 et
:- module hello.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module posix.
:- import_module posix.open.
:- import_module posix.write.
:- import_module text.

:- import_module list.
:- import_module string.

main(!IO) :-
    open("/dev/tty", [wronly], Res0, !IO),
    (
        Res0 = ok(Fd),
        Str = "hello world.\n",
        length(Str, Len),
        write(Fd, Len, text(Str), Res1, !IO),
        (
            Res1 = ok(NWritten),
            ( NWritten \= Len ->
                % We didn't write all of it!
                io.write_string("failed to write it all\n", !IO)
            ;
                true 
            )
        ;
            Res1 = error(Err),
            io.write(Err, !IO),
            io.nl(!IO)
        )
    ;
        Res0 = error(Err),
        io.write(Err, !IO),
        io.nl(!IO)
    ).
