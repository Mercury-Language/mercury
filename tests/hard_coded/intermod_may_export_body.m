%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module intermod_may_export_body.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module intermod_may_export_body2.

main(!IO) :-
    plus(1, 2, A),
    cannot_export_plus(4, 5, B),
    io.print_line(A, !IO),
    io.print_line(B, !IO).
