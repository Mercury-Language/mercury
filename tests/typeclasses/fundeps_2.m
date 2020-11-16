%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module fundeps_2.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module list.

main(!S) :-
    ( if test(intcoll([0]), 1) then
        write_string("yes\n", !S)
    else
        write_string("no\n", !S)
    ),
    ( if e = intcoll([1]) then
        write_string("yes\n", !S)
    else
        write_string("no\n", !S)
    ).

:- typeclass foo(T) where [].
:- instance foo(int) where [].

:- typeclass coll(C, E) <= ((C -> E), foo(E)) where [
    func e = C,
    func i(C, E) = C,
    pred m(E::in, C::in) is semidet
].

:- type intcoll
    --->    intcoll(list(int)).

:- instance coll(intcoll, int) where [
    (e = intcoll([])),
    (i(intcoll(Ns), N) = intcoll([N | Ns])),
    m(N, intcoll([N | _])),
    m(N, intcoll([_ | Ns])) :- m(N, intcoll(Ns))
].

:- pred test(C::in, E::in) is semidet <= coll(C, E).

test(C, E) :-
    m(E, i(C, E)).
