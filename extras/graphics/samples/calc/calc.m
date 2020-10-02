%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
%
% File: calc.m
% Main author: conway.
%
% This source file is hereby placed in the public domain. -conway (the author).
%
% A simple Tcl/Tk based calculator.
%
% 07/13/01 hkrug@rationalizer.com:
%        * only one toplevel window is created
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module calc.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module mtcltk.
:- import_module mtk.

:- import_module bool.
:- import_module char.
:- import_module float.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module pair.
:- import_module string.
:- import_module require.

%-----------------------------------------------------------------------------%

main(!IO) :-
    CallBack = (pred(Interp::in, !.IO::di, !:IO::uo) is det :-
        make_calculator(Interp, mtk.root_window, !IO)
    ),
    mtcltk.main(CallBack, ["CALC"], !IO).

%-----------------------------------------------------------------------------%

:- type calculator
    --->    calc(
                int,
                list(half_expr),
                maybe(int),
                list(pair(widget, calc_button)),
                widget, % clear
                widget  % answer label
            ).

:- type calc_button
    --->    num(int)
    ;       dot
    ;       equal
    ;       plus
    ;       minus
    ;       times
    ;       divide
    ;       lpar
    ;       rpar.

:- type half_expr
    --->    plus(int)
    ;       minus(int)
    ;       times(int)
    ;       div(int)
    ;       mark.

%-----------------------------------------------------------------------------%

    % Create all the widgets.
    %
:- pred make_calculator(tcl_interp::in, widget::in(window), io::di, io::uo)
    is det.

make_calculator(Interp, Frame, !IO) :-
    label(Interp, [text("ArithMate (TM) C9+")], Frame, DisLab, !IO),
    label(Interp, [text("0"), relief("sunken")], Frame, AnsLab, !IO),
    pack(Interp, [pack(DisLab, [top, expand, fill_x]),
        pack(AnsLab, [top, expand, fill_x])], !IO),

    frame(Interp, [], Frame, NumPad, !IO),

    button(Interp, [text(" 0 ")], NumPad, Zero,   !IO),
    button(Interp, [text(" 1 ")], NumPad, One,    !IO),
    button(Interp, [text(" 2 ")], NumPad, Two,    !IO),
    button(Interp, [text(" 3 ")], NumPad, Three,  !IO),
    button(Interp, [text(" 4 ")], NumPad, Four,   !IO),
    button(Interp, [text(" 5 ")], NumPad, Five,   !IO),
    button(Interp, [text(" 6 ")], NumPad, Six,    !IO),
    button(Interp, [text(" 7 ")], NumPad, Seven,  !IO),
    button(Interp, [text(" 8 ")], NumPad, Eight,  !IO),
    button(Interp, [text(" 9 ")], NumPad, Nine,   !IO),
    button(Interp, [text(" / ")], NumPad, Div,    !IO),
    button(Interp, [text(" x ")], NumPad, Times,  !IO),
    button(Interp, [text(" - ")], NumPad, Minus,  !IO),
    button(Interp, [text(" . ")], NumPad, Dot,    !IO),
    button(Interp, [text(" = ")], NumPad, Equal,  !IO),
    button(Interp, [text(" + ")], NumPad, Plus,   !IO),
    button(Interp, [text(" ( ")], NumPad, LParen, !IO),
    button(Interp, [text(" ) ")], NumPad, RParen, !IO),
    button(Interp, [text(" C ")], NumPad, Clear,  !IO),

    layout_grid(Interp, [
        [yes(Clear), no, yes(LParen), yes(RParen)],
        [yes(Seven), yes(Eight), yes(Nine), yes(Div)],
        [yes(Four), yes(Five), yes(Six), yes(Times)],
        [yes(One), yes(Two), yes(Three), yes(Minus)],
        [yes(Zero), yes(Dot), yes(Equal), yes(Plus)]
    ], !IO),

    init_calc_state(
        [Zero - num(0) , One - num(1), Two - num(2), Three - num(3),
         Four - num(4), Five - num(5), Six - num(6), Seven - num(7),
         Eight - num(8), Nine - num(9), Div - divide, Times - times,
         Minus - minus, Plus - plus, LParen - lpar, RParen - rpar,
         Equal - equal, Dot - dot],
         Clear, AnsLab, State0),

    config_calc(Interp, State0, !IO),
    pack(Interp, [pack(NumPad, [top])], !IO).

:- pred init_calc_state(list(pair(widget, calc_button))::in, widget::in,
    widget::in, calculator::out) is det.

init_calc_state(Buttons, Clear, Answer, Calc) :-
    Calc = calc(0, [], no, Buttons, Clear, Answer).

%-----------------------------------------------------------------------------%

    % Rebind all the widget callbacks to give them the current state
    % of the calculator.
    %
:- pred config_calc(tcl_interp::in, calculator::in, io::di, io::uo) is det.

config_calc(Interp, State, !IO) :-
    State = calc(Ans, _Store, Clr, Buttons, Clear, AnsLab),
    Pred = (pred((Wid - _)::in, IO0::di, IO::uo) is det :-
        unbind_command(Interp, Wid, IO0, IO)
    ),
    list.foldl(Pred, Buttons, !IO),
    unbind_command(Interp, Clear, !IO),
    list.foldl(config_button(Interp, State), Buttons, !IO),
    configure(Interp, Clear, [command(clear(State))], !IO),
    (
        Clr = yes(Num)
    ;
        Clr = no,
        Num = Ans
    ),
    string.format("%d", [i(Num)], Str),
    configure(Interp, AnsLab, [text(Str)], !IO).

:- pred config_button(tcl_interp::in, calculator::in,
    pair(widget, calc_button)::in, io::di, io::uo) is det.

config_button(Interp, State, Widget - Button, !IO) :-
    configure(Interp, Widget, [command(press(State, Button))], !IO).

%-----------------------------------------------------------------------------%

    % The callback for when a button on the calculator gets pressed.
    %
:- pred press(calculator::in, calc_button::in, tcl_interp::in,
    io::di, io::uo) is det.

press(State0, num(N), Interp, !IO) :-
    State0 = calc(Acc0, Store, Clr, Buttons, Clear, AnsLab),
    (
        Clr = yes(_),
        Acc = N
    ;
        Clr = no,
        Acc = Acc0 * 10 + N
    ),
    State = calc(Acc, Store, no, Buttons, Clear, AnsLab),
    config_calc(Interp, State, !IO).
press(State0, plus, Interp, !IO) :-
    State0 = calc(Acc0, Store0, _, Buttons, Clear, AnsLab),
    compute(plus, Acc0, Store0, Acc, Num, Store),
    State = calc(Acc, Store, yes(Num), Buttons, Clear, AnsLab),
    config_calc(Interp, State, !IO).
press(State0, minus, Interp, !IO) :-
    State0 = calc(Acc0, Store0, _, Buttons, Clear, AnsLab),
    compute(minus, Acc0, Store0, Acc, Num, Store),
    State = calc(Acc, Store, yes(Num), Buttons, Clear, AnsLab),
    config_calc(Interp, State, !IO).
press(State0, times, Interp, !IO) :-
    State0 = calc(Acc0, Store0, _, Buttons, Clear, AnsLab),
    compute(times, Acc0, Store0, Acc, Num, Store),
    State = calc(Acc, Store, yes(Num), Buttons, Clear, AnsLab),
    config_calc(Interp, State, !IO).
press(State0, divide, Interp, !IO) :-
    State0 = calc(Acc0, Store0, _, Buttons, Clear, AnsLab),
    compute(divide, Acc0, Store0, Acc, Num, Store),
    State = calc(Acc, Store, yes(Num), Buttons, Clear, AnsLab),
    config_calc(Interp, State, !IO).
press(State0, lpar, Interp, !IO) :-
    State0 = calc(Acc0, Store0, _, Buttons, Clear, AnsLab),
    compute(lpar, Acc0, Store0, Acc, Num, Store),
    State = calc(Acc, Store, yes(Num), Buttons, Clear, AnsLab),
    config_calc(Interp, State, !IO).
press(State0, rpar, Interp, !IO) :-
    State0 = calc(Acc0, Store0, _, Buttons, Clear, AnsLab),
    compute(rpar, Acc0, Store0, Acc, Num, Store),
    State = calc(Acc, Store, yes(Num), Buttons, Clear, AnsLab),
    config_calc(Interp, State, !IO).
press(State0, equal, Interp, !IO) :-
    State0 = calc(Acc0, Store0, _, Buttons, Clear, AnsLab),
    compute(equal, Acc0, Store0, Acc, Num, Store),
    State = calc(Acc, Store, yes(Num), Buttons, Clear, AnsLab),
    config_calc(Interp, State, !IO).
press(_State0, dot, _Interp, _, _) :-
    error("press dot").

%-----------------------------------------------------------------------------%

    % Compute a new state of the calculator when some computation gets done.
    %
:- pred compute(calc_button::in, int::in, list(half_expr)::in,
    int::out, int::out, list(half_expr)::out) is det.

compute(num(_), _, _, _, _, _) :-
    error("shouldn't happen").
compute(plus, Acc, [], 0, Acc, [plus(Acc)]).
compute(plus, Acc, [Op | Ops], NewAcc, DispAcc, Stack) :-
    (
        Op = times(Fac),
        compute(plus, Fac * Acc, Ops, NewAcc, DispAcc, Stack)
    ;
        Op = div(Fac),
        compute(plus, Fac // Acc, Ops, NewAcc, DispAcc, Stack)
    ;
        Op = plus(Sum),
        compute(plus, Sum + Acc, Ops, NewAcc, DispAcc, Stack)
    ;
        Op = minus(Sum),
        compute(plus, Sum - Acc, Ops, NewAcc, DispAcc, Stack)
    ;
        Op = mark,
        NewAcc = Acc,
        DispAcc = Acc,
        Stack = [plus(Acc), Op | Ops]
    ).
compute(minus, Acc, [], 0, Acc, [minus(Acc)]).
compute(minus, Acc, [Op | Ops], NewAcc, DispAcc, Stack) :-
    (
        Op = times(Fac),
        compute(minus, Fac * Acc, Ops, NewAcc, DispAcc, Stack)
    ;
        Op = div(Fac),
        compute(minus, Fac // Acc, Ops, NewAcc, DispAcc, Stack)
    ;
        Op = plus(Sum),
        compute(minus, Sum + Acc, Ops, NewAcc, DispAcc, Stack)
    ;
        Op = minus(Sum),
        compute(minus, Sum - Acc, Ops, NewAcc, DispAcc, Stack)
    ;
        Op = mark,
        NewAcc = Acc,
        DispAcc = Acc,
        Stack = [minus(Acc), Op | Ops]
    ).
compute(times, Acc, [], 0, Acc, [times(Acc)]).
compute(times, Acc, [Op | Ops], NewAcc, DispAcc, Stack) :-
    (
        Op = times(Fac),
        compute(times, Fac * Acc, Ops, NewAcc, DispAcc, Stack)
    ;
        Op = div(Fac),
        compute(times, Fac // Acc, Ops, NewAcc, DispAcc, Stack)
    ;
        Op = plus(_),
        NewAcc = 0,
        DispAcc = Acc,
        Stack = [times(Acc), Op | Ops]
    ;
        Op = minus(_),
        NewAcc = 0,
        DispAcc = Acc,
        Stack = [times(Acc), Op | Ops]
    ;
        Op = mark,
        NewAcc = Acc,
        DispAcc = Acc,
        Stack = [times(Acc), Op | Ops]
    ).
compute(divide, Acc, [], 0, Acc, [div(Acc)]).
compute(divide, Acc, [Op | Ops], NewAcc, DispAcc, Stack) :-
    (
        Op = times(Fac),
        compute(divide, Fac * Acc, Ops, NewAcc, DispAcc, Stack)
    ;
        Op = div(Fac),
        compute(divide, Fac // Acc, Ops, NewAcc, DispAcc, Stack)
    ;
        Op = plus(_),
        NewAcc = 0,
        DispAcc = Acc,
        Stack = [div(Acc), Op | Ops]
    ;
        Op = minus(_),
        NewAcc = 0,
        DispAcc = Acc,
        Stack = [div(Acc), Op | Ops]
    ;
        Op = mark,
        NewAcc = Acc,
        DispAcc = Acc,
        Stack = [div(Acc), Op | Ops]
    ).
compute(equal, Acc, [], Acc, Acc, []).
compute(equal, Acc, [Op | Ops], NewAcc, DispAcc, Stack) :-
    (
        Op = times(Fac),
        compute(equal, Fac * Acc, Ops, NewAcc, DispAcc, Stack)
    ;
        Op = div(Fac),
        compute(equal, Fac // Acc, Ops, NewAcc, DispAcc, Stack)
    ;
        Op = plus(Sum),
        compute(equal, Sum + Acc, Ops, NewAcc, DispAcc, Stack)
    ;
        Op = minus(Sum),
        compute(equal, Sum - Acc, Ops, NewAcc, DispAcc, Stack)
    ;
        Op = mark,
        NewAcc = Acc,
        DispAcc = Acc,
        Stack = [Op | Ops]
    ).
compute(dot, _, _, _, _, _) :-
    error("dot").
compute(lpar, Acc, Store, 0, Acc, [mark | Store]).
compute(rpar, _Acc0, [], 0, 0, []). % error!
compute(rpar, Acc, [Op | Ops], NewAcc, DispAcc, Stack) :-
    (
        Op = times(Fac),
        compute(rpar, Fac*Acc, Ops, NewAcc, DispAcc, Stack)
    ;
        Op = div(Fac),
        compute(rpar, Fac//Acc, Ops, NewAcc, DispAcc, Stack)
    ;
        Op = plus(Sum),
        compute(rpar, Sum+Acc, Ops, NewAcc, DispAcc, Stack)
    ;
        Op = minus(Sum),
        compute(rpar, Sum-Acc, Ops, NewAcc, DispAcc, Stack)
    ;
        Op = mark,
        NewAcc = Acc,
        DispAcc = Acc,
        Stack = Ops
    ).

%-----------------------------------------------------------------------------%

    % Reset the calculator.
    %
:- pred clear(calculator::in, tcl_interp::in, io::di, io::uo) is det.

clear(State0, Interp, !IO) :-
    State0 = calc(_Ans, _Store, _, Buttons, Clear, AnsLab),
    State = calc(0, [], no, Buttons, Clear, AnsLab),
    config_calc(Interp, State, !IO).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
