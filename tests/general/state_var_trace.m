%---------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%
% This test case models some code in g12/common/g12/propagator.m that is
% a challenge for the state variable transformation.
%
%---------------------------------------------------------------------------%

:- module state_var_trace.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module string.

main(!IO) :-
    X0 = 10,
    p(0, B, X0, X),
    io.format("B = %d\n", [i(B)], !IO),
    io.format("X = %d\n", [i(X)], !IO).

:- pred p(int::in, int::out, int::di, int::uo) is det.

p(A, B, !X) :-
    trace [
        compiletime(flag("state_var_trace")),
        io(!TIO)
    ] (
        ui_format("A  ", A, !TIO),
        ui_format("!.X", !.X, !TIO)
    ),
    B = A + 1.

:- pred ui_format(string::in, int::ui, io::di, io::uo) is det.

ui_format(Name, Var, !IO) :-
    copy(Var, VarCopy),
    io.format("%s = %d\n", [s(Name), i(VarCopy)], !IO).
