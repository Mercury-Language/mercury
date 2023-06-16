%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Regression test for bug #240 and bug #211.
% When a structure definition has a MR_Float member the C compiler could lay
% the structure out differently from that which is expected by the Mercury
% compiler.

:- module bug240.

:- interface.

:- import_module io.

:- type t
    --->    a(int)
    ;       b(int)
    ;       c(int)
    ;       d(int)
    ;       fc(float, float, int).

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    X = fc(123.0, 456.0, 789),
    write_fc(X, !IO),
    Z = mkfc(123.0, 456.0, 789),
    write_fc(Z, !IO).

:- func mkfc(float::in, float::in, int::in) =
    (t::out(bound(fc(ground, ground, ground)))) is det.

:- pragma no_inline(mkfc/3).

mkfc(F1, F2, I) = G :-
    G = fc(F1, F2, I).

:- pred write_fc(t::in(bound(fc(ground, ground, ground))), io::di, io::uo)
    is det.

:- pragma no_inline(write_fc/3).

write_fc(X, !IO) :-
    X = fc(Y1, Y2, Y3),
    io.write_line(Y1, !IO),
    io.write_line(Y2, !IO),
    io.write_line(Y3, !IO).
