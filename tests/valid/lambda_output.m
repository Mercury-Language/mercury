%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module lambda_output.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main -->
    fork(a(_), b).

:- pred fork(pred(io, io), pred(io, io),
    io, io).
:- mode fork(pred(di, uo) is det, pred(di, uo) is det,
    di, uo) is det.

fork(A, B, !IO) :-
    call(A, !IO),
    call(B, !IO).

:- pred a(int, io, io).
:- mode a(out, di, uo) is det.

a(42, !IO).

:- pred b(io, io).
:- mode b(di, uo) is det.

b(!IO).
