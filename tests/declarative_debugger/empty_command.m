%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module empty_command.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    p(53, P),
    io.write_int(P, !IO),
    io.nl(!IO).

:- pred p(int::in, int::out) is det.
:- pred q(int::in, int::out) is det.
:- pred r(int::in, int::out) is det.
:- pred s(int::in, int::out) is det.

p --> q, r, s.
q --> [].
r --> [].
s --> [].
