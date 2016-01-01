%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module instance_unconstrained_tvar.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- typeclass p(T) where [
    pred m(T, io, io),
    mode m(in, di, uo) is det
].

:- implementation.

:- import_module list.

:- instance p(list(T)) where [
    pred(m/3) is io.write
].

main(!IO) :-
    m([1, 2, 3], !IO),
    io.nl(!IO).
