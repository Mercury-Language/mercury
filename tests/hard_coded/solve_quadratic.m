%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module solve_quadratic.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module float.
:- import_module math.

main(!IO) :-
    % Two roots (i.e. B^2 > 4AC)
    check_quad(1.0, 3.0, -2.0, !IO),
    check_quad(1.0, -5.0, 3.0, !IO),
    check_quad(2.0, -5.0, 3.0, !IO),
    check_quad(2.0, -5.0, -6.0, !IO),
    check_quad(-15.0, 68.0, 34.5, !IO),
    check_quad(-4.0, -17.0, 3.5, !IO),

    % One root (i.e. B^2 = 4AC)
    check_quad(5.0, 10.0, 5.0, !IO),
    check_quad(4.0, -8.0, 4.0, !IO),
    check_quad(-3.0, -18.0, -27.0, !IO),

    % No roots (i.e. B^2 < 4AC)
    check_quad(4.0, 3.0, 2.0, !IO),
    check_quad(1.0, 1.0, 1.0, !IO),
    check_quad(-1.0, -2.0, -2.0, !IO),

    % Special cases
    check_quad(1.0, -2.0, 0.0, !IO),
    check_quad(1.0, 0.0, -2.0, !IO),
    check_quad(2.0, 0.0, 0.0, !IO),
    check_quad(-100.0, 0.0, 0.0001, !IO).

:- pred check_quad(float::in, float::in, float::in, io::di, io::uo) is det.

check_quad(A, B, C, !IO) :-
    Ss = solve_quadratic(A, B, C),
    (
        Ss = no_roots,
        io.write_string("No roots.\n", !IO)
    ;
        Ss = one_root(R),
        io.write_string("One root: ", !IO),
        check_result(A, B, C, R, !IO),
        io.write_string(".\n", !IO)
    ;
        Ss = two_roots(R1, R2),
        io.write_string("Two roots: ", !IO),
        check_result(A, B, C, R1, !IO),
        io.write_string(", ", !IO),
        check_result(A, B, C, R2, !IO),
        io.write_string(".\n", !IO)
    ).

:- pred check_result(float::in, float::in, float::in, float::in,
    io::di, io::uo) is det.

check_result(A, B, C, R, !IO) :-
    Val = ((A * R + B) * R) + C,
    % This test is pretty conservative, since I don't know
    % how much error should be expected.
    ( if abs(Val) < 1E-6 then
        io.write_string("ok", !IO)
    else
        io.write_string("problem: Val = ", !IO),
        io.write_float(Val, !IO)
    ).
