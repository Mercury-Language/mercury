%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module fundeps_vars.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.
:- implementation.
:- import_module list.

main(!IO) :-
    ( if test([0], 1) then
        write_string("yes\n", !IO)
    else
        write_string("no\n", !IO)
    ).

:- typeclass foo(T) where [].

% Syntax error: fundeps not variables.
:- typeclass coll(C, E) <= ((c -> e), foo(E)) where [
    func e = C,
    func i(C, E) = C,
    pred m(E::in, C::in) is semidet
].

% Syntax error: fundep variables not in head.
:- typeclass coll(C, E) <= ((C -> E, F), foo(E)) where [
    func e = C,
    func i(C, E) = C,
    pred m(E::in, C::in) is semidet
].

:- type intcoll == list(int).

:- instance coll(intcoll, int) where [
    (e = []),
    (i(Ns, N) = [N | Ns]),
    m(N, [N | _]),
    m(N, [_ | Ns]) :- m(N, Ns)
].

:- pred test(C, E) <= coll(C, E).
:- mode test(in, in) is semidet.

test(C, E) :-
    m(E, i(C, E)).
