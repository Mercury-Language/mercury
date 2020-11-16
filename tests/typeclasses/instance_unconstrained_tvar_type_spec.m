%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module instance_unconstrained_tvar_type_spec.

:- interface.

:- import_module io.
:- import_module list.

:- pred main(io::di, io::uo) is det.

:- typeclass p(T) where [
    pred m(T, io, io),
    mode m(in, di, uo) is det
].

:- implementation.

:- instance p(list(T)) where [
    pred(m/3) is io.write
].

main(!IO) :-
    call_m([1, 2, 3], !IO),
    io.nl(!IO).

:- pred call_m(T::in, io::di, io::uo) is det <= p(T).
:- pragma type_spec(call_m/3, T = list(U)).
:- pragma no_inline(call_m/3).

call_m(T, !IO) :-
    m(T, !IO).
