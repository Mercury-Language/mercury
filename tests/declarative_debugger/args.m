%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module args.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.
:- import_module library_forwarding.

main(!IO) :-
    ( if
        p(1, X, 3, Y, 5),
        library_forwarding.semidet_fail
    then
        io.write_int(X, !IO),
        io.nl(!IO),
        io.write_int(Y, !IO),
        io.nl(!IO)
    else
        io.write_string("no.\n", !IO)
    ).

:- pred p(int::in, int::out, int::in, int::out, int::in) is nondet.

p(A, A + (B * C), B, (A + B) * C, C) :-
    library_forwarding.semidet_succeed.
p(A, A - B, B, C - B, C) :-
    library_forwarding.semidet_succeed.

