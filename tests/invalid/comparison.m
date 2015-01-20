:- module comparison.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int.

main(!IO) :-
    ( test(1.0, 2.0) ->
        io.write_string("ok\n", !IO)
    ;
        io.write_string("not ok\n", !IO)
    ).

:- pred test(float::in, float::in) is semidet.

test(A, B) :-
    A < B.
