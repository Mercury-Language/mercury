%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module eqvclass_bug.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module eqvclass.

main(!IO) :-
    ensure_equivalence(0, 0, eqvclass.init, NewEqvClass),
    io.print_line(NewEqvClass, !IO).
