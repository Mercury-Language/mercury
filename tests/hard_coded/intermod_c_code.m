%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module intermod_c_code.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module intermod_c_code2.

main(!IO) :-
    c_code("Hello, world\n", Y),
    io.write(Y, !IO),
    io.nl(!IO).
