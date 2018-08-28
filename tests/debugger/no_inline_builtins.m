%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module no_inline_builtins.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.

main(!IO) :-
    io.write_int(40 + 2, !IO),
    io.nl(!IO).
