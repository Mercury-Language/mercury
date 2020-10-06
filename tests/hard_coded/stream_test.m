%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module stream_test.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module char.
:- import_module list.
:- import_module pprint.
:- import_module stream.
:- import_module univ.

:- type foo
    --->    foo
    ;       bar
    ;       baz.

main(!IO) :-
    io.stdout_stream(Stdout, !IO),
    put(Stdout, 561, !IO),
    put(Stdout, "\nHello World!\n", !IO),
    put(Stdout, 'a', !IO),
    put(Stdout, "\n", !IO),
    put(Stdout, univ([foo, bar, baz]), !IO),
    put(Stdout, "\n", !IO),
    list.duplicate(10, foo, ListFoo),
    pprint.write(Stdout, 4, to_doc(ListFoo), !IO),
    io.nl(Stdout, !IO),
    io.stdin_stream(Stdin, !IO),
    input_stream_fold(Stdin, char_list_cons, [], PartialResult, !IO),
    (
        PartialResult = ok(Result),
        io.write(Result, !IO),
        io.nl(!IO)
    ;
        PartialResult = error(_, _),
        io.write_string("TEST FAILED\n", !IO)
    ).

:- pred char_list_cons(char::in, list(char)::in, list(char)::out) is det.

char_list_cons(X, Xs, [ X | Xs ]).
