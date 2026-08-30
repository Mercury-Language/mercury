%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
%
% Test errors in input_mode_spec pragmas.
%
%---------------------------------------------------------------------------%

:- module bad_input_spec.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    test(4, !IO),
    test(5, !IO).

%---------------------------------------------------------------------------%

:- type control
    --->    ctrl_a
    ;       ctrl_b.

:- inst ctrl_a for control/0
    --->    ctrl_a.
:- inst ctrl_b for control/0
    --->    ctrl_b.

:- type aux
    --->    aux_c
    ;       aux_d.

:- inst aux_c for aux/0
    --->    aux_c.
:- inst aux_d for aux/0
    --->    aux_d.

:- pragma input_mode_spec(aux,     replace_in_mode).
:- pragma input_mode_spec(aux,     replace_in_mode, []).
:- pragma input_mode_spec(control, add_to_in_mod, [9, ctrl_b]).
:- pragma input_mode_spec(control, add_to_in_mode, [42, "fortytwo", ctrl_b]).
:- pragma input_mode_spec(au,      replace_in_mode, [aux_c, aux_d]).
:- pragma input_mode_spec(list(T), replace_in_mode, [aux_c, aux_d]).
:- pragma input_mode_spec(aux,     replace_in_mode, [au_c, aux_]).
:- pragma input_mode_spec(aux,     replace_in_mode, [aux_c, aux_d]).

%---------------------------------------------------------------------------%

:- pred test(int::in, io::di, io::uo) is det.

test(In, !IO) :-
    io.nl(!IO),
    return_control(In, CtrlX),
    test_control(ctrl_a, In, CtrlA),
    test_control(ctrl_b, In, CtrlB),
    test_aux(aux_c, CtrlA, AuxAC),
    test_aux(aux_d, CtrlA, AuxAD),
    test_aux(aux_c, CtrlB, AuxBC),
    test_aux(aux_d, CtrlB, AuxBD),
    test_aux(aux_c, CtrlX, AuxXC),
    test_aux(aux_d, CtrlX, AuxXD),
    io.format("CtrlA = %5d, CtrlB = %5d\n", [i(CtrlA), i(CtrlB)], !IO),
    io.format("AuxAC = %5d, AuxAD = %5d\n", [i(AuxAC), i(AuxAD)], !IO),
    io.format("AuxBC = %5d, AuxBD = %5d\n", [i(AuxBC), i(AuxBD)], !IO),
    io.format("AuxXC = %5d, AuxXD = %5d\n", [i(AuxXC), i(AuxXD)], !IO).

:- pred test_control(control::in, int::in, int::out) is det.

test_control(Control, In, Out) :-
    (
        Control = ctrl_a,
        Out = In * 100
    ;
        Control = ctrl_b,
        Out = In * 1000
    ).

:- pred test_aux(aux::in, int::in, int::out) is det.

test_aux(Aux, In, Out) :-
    (
        Aux = aux_c,
        Out = In + 3
    ;
        Aux = aux_d,
        Out = In + 6
    ).

:- pred return_control(int::in, int::out) is det.

return_control(In, Ctrl) :-
    ( if In mod 2 = 0 then
        Ctrl = 10
    else
        Ctrl = 10000
    ).

%---------------------------------------------------------------------------%
