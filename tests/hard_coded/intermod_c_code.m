%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module intermod_c_code.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module intermod_c_code_helper_1.

main(!IO) :-
    c_code("Hello, world\n", Y),
    io.write_line(Y, !IO).
