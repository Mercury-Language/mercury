% vim: ts=4 sw=4 et ft=mercury

:- module require_det_in_lambda.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int.

main(!IO) :-
    % The compiler did not look for violations of require_detism scopes inside
    % lambda goals.
    T = 
        ( pred(X::in, Y::out) is semidet :-
            require_det (
                X < 10,
                Y = X + 1
            )
        ),
    test(T, 5, !IO).

:- pred test((pred(int, int))::in(pred(in, out) is semidet), int::in,
    io::di, io::uo) is det.

test(T, A, !IO) :-
    ( if T(A, B) then
        io.write_int(B, !IO),
        io.nl(!IO)
    else
        io.write_string("test failed\n", !IO)
    ).
