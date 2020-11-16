%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module fundeps_3.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module list.

main(!S) :-
    ( if e = 1 then
        io.write_string("yes\n", !S)
    else
        io.write_string("no\n", !S)
    ).

:- typeclass coll(C, E) <= (C -> E) where [
    func e = C
].

:- instance coll(int, int) where [
    (e = 1)
].
