%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Test reuse of cells containing double width arguments.
%---------------------------------------------------------------------------%

:- module reuse_double.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module float.

:- type tt
    --->    tt(float, enum, enum, float, enum, enum, float).

:- type enum
    --->    aa ; bb ; cc ; dd.

%---------------------------------------------------------------------------%

main(!IO) :-
    T0 = tt(1.1, aa, bb, 2.2, cc, dd, 3.3),
    copy(T0, T1),
    swap(T1, T2),
    io.write_line(T2, !IO).

:- pred swap(tt::di, tt::uo) is det.

swap(TT0, TT1) :-
    TT0 = tt(A, X1, X2, B, X3, X4, C),
    TT1 = tt(C, X1, X2, B, X4, X3, A).
