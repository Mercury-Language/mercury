%-----------------------------------------------------------------------------%
%
% file: calc.m
% main author: conway.
%
% This source file is hereby placed in the public domain. -conway (the author).
%
% A simple Tcl/Tk based calculator.
%
% 07/13/01 hkrug@rationalizer.com:
%        * only one toplevel window is created
% 
%-----------------------------------------------------------------------------%
:- module calc.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

%-----------------------------------------------------------------------------%
:- implementation.
%-----------------------------------------------------------------------------%

:- import_module mtcltk, mtk.
:- import_module bool, list, string, char, int, float, std_util, require.


main -->
	mtcltk__main(pred(Interp::in, di, uo) is det -->
	                 make_calculator(Interp, mtk__root_window)
            , ["CALC"]).

%-----------------------------------------------------------------------------%

:- type calculator
	--->	calc(
			int,
			list(half_expr),
			maybe(int),
			list(pair(widget, calc_button)),
			widget,	% clear
			widget	% answer label
		).

:- type calc_button
	--->	num(int)
	;	dot
	;	equal
	;	plus
	;	minus
	;	times
	;	divide
	;	lpar
	;	rpar
	.

:- type half_expr
	--->	plus(int)
	;	minus(int)
	;	times(int)
	;	div(int)
	;	mark
	.

%-----------------------------------------------------------------------------%

	% Create all the widgets.

:- pred make_calculator(tcl_interp, widget, io__state, io__state).
:- mode make_calculator(in, in(window), di, uo) is det.

make_calculator(Interp, Frame) -->
	label(Interp, [text("ArithMate (TM) C9+")], Frame, DisLab),
	label(Interp, [text("0"), relief("sunken")], Frame, AnsLab),
	pack(Interp, [pack(DisLab, [top, expand, fill_x]),
		pack(AnsLab, [top, expand, fill_x])]),
	
	frame(Interp, [], Frame, NumPad),

	button(Interp, [text(" 0 ")], NumPad, Zero),
	button(Interp, [text(" 1 ")], NumPad, One),
	button(Interp, [text(" 2 ")], NumPad, Two),
	button(Interp, [text(" 3 ")], NumPad, Three),
	button(Interp, [text(" 4 ")], NumPad, Four),
	button(Interp, [text(" 5 ")], NumPad, Five),
	button(Interp, [text(" 6 ")], NumPad, Six),
	button(Interp, [text(" 7 ")], NumPad, Seven),
	button(Interp, [text(" 8 ")], NumPad, Eight),
	button(Interp, [text(" 9 ")], NumPad, Nine),
	button(Interp, [text(" / ")], NumPad, Div),
	button(Interp, [text(" x ")], NumPad, Times),
	button(Interp, [text(" - ")], NumPad, Minus),
	button(Interp, [text(" . ")], NumPad, Dot),
	button(Interp, [text(" = ")], NumPad, Equal),
	button(Interp, [text(" + ")], NumPad, Plus),
	button(Interp, [text(" ( ")], NumPad, LParen),
	button(Interp, [text(" ) ")], NumPad, RParen),
	button(Interp, [text(" C ")], NumPad, Clear),

	layout_grid(Interp, [
		[yes(Clear), no, yes(LParen), yes(RParen)],
		[yes(Seven), yes(Eight), yes(Nine), yes(Div)],
		[yes(Four), yes(Five), yes(Six), yes(Times)],
		[yes(One), yes(Two), yes(Three), yes(Minus)],
		[yes(Zero), yes(Dot), yes(Equal), yes(Plus)]
	]),

	{ init_calc_state(
		[Zero - num(0) , One - num(1), Two - num(2), Three - num(3),
		 Four - num(4), Five - num(5), Six - num(6), Seven - num(7),
		 Eight - num(8), Nine - num(9), Div - divide, Times - times,
		 Minus - minus, Plus - plus, LParen - lpar, RParen - rpar,
		 Equal - equal, Dot - dot],
		 Clear, AnsLab, State0) },

	config_calc(Interp, State0),

	pack(Interp, [pack(NumPad, [top])]).

:- pred init_calc_state(list(pair(widget, calc_button)), widget, widget,
		calculator).
:- mode init_calc_state(in, in, in, out) is det.

init_calc_state(Buttons, Clear, Answer, Calc) :-
	Calc = calc(0, [], no, Buttons, Clear, Answer).

%-----------------------------------------------------------------------------%

	% Rebind all the widget callbacks to give them the current state
	% of the calculator.

:- pred config_calc(tcl_interp, calculator, io__state, io__state).
:- mode config_calc(in, in, di, uo) is det.

config_calc(Interp, State) -->
	{ State = calc(Ans, _Store, Clr, Buttons, Clear, AnsLab) },
	{ Pred = (pred((Wid - _)::in, IO0::di, IO::uo) is det :-
		unbind_command(Interp, Wid, IO0, IO)
	) },
	list__foldl(Pred, Buttons),
	unbind_command(Interp, Clear),
	list__foldl(config_button(Interp, State), Buttons),
	configure(Interp, Clear, [command(clear(State))]),
	{
		Clr = yes(Num)
	;
		Clr = no,
		Num = Ans
	},
	{ string__format("%d", [i(Num)], Str) },
	configure(Interp, AnsLab, [text(Str)]).

:- pred config_button(tcl_interp, calculator, pair(widget, calc_button),
		io__state, io__state).
:- mode config_button(in, in, in, di, uo) is det.

config_button(Interp, State, Widget - Button) -->
	configure(Interp, Widget, [command(press(State, Button))]).

%-----------------------------------------------------------------------------%

	% The callback for when a button on the calculator gets pressed.

:- pred press(calculator, calc_button, tcl_interp, io__state, io__state).
:- mode press(in, in, in, di, uo) is det.

press(State0, num(N), Interp) -->
	{ State0 = calc(Acc0, Store, Clr, Buttons, Clear, AnsLab) },
	{
		Clr = yes(_),
		Acc = N
	;
		Clr = no,
		Acc = Acc0*10+N
	},
	{ State = calc(Acc, Store, no, Buttons, Clear, AnsLab) },
	config_calc(Interp, State).
press(State0, plus, Interp) -->
	{ State0 = calc(Acc0, Store0, _, Buttons, Clear, AnsLab) },
	{ compute(plus, Acc0, Store0, Acc, Num, Store) },
	{ State = calc(Acc, Store, yes(Num), Buttons, Clear, AnsLab) },
	config_calc(Interp, State).
press(State0, minus, Interp) -->
	{ State0 = calc(Acc0, Store0, _, Buttons, Clear, AnsLab) },
	{ compute(minus, Acc0, Store0, Acc, Num, Store) },
	{ State = calc(Acc, Store, yes(Num), Buttons, Clear, AnsLab) },
	config_calc(Interp, State).
press(State0, times, Interp) -->
	{ State0 = calc(Acc0, Store0, _, Buttons, Clear, AnsLab) },
	{ compute(times, Acc0, Store0, Acc, Num, Store) },
	{ State = calc(Acc, Store, yes(Num), Buttons, Clear, AnsLab) },
	config_calc(Interp, State).
press(State0, divide, Interp) -->
	{ State0 = calc(Acc0, Store0, _, Buttons, Clear, AnsLab) },
	{ compute(divide, Acc0, Store0, Acc, Num, Store) },
	{ State = calc(Acc, Store, yes(Num), Buttons, Clear, AnsLab) },
	config_calc(Interp, State).
press(State0, lpar, Interp) -->
	{ State0 = calc(Acc0, Store0, _, Buttons, Clear, AnsLab) },
	{ compute(lpar, Acc0, Store0, Acc, Num, Store) },
	{ State = calc(Acc, Store, yes(Num), Buttons, Clear, AnsLab) },
	config_calc(Interp, State).
press(State0, rpar, Interp) -->
	{ State0 = calc(Acc0, Store0, _, Buttons, Clear, AnsLab) },
	{ compute(rpar, Acc0, Store0, Acc, Num, Store) },
	{ State = calc(Acc, Store, yes(Num), Buttons, Clear, AnsLab) },
	config_calc(Interp, State).
press(State0, equal, Interp) -->
	{ State0 = calc(Acc0, Store0, _, Buttons, Clear, AnsLab) },
	{ compute(equal, Acc0, Store0, Acc, Num, Store) },
	{ State = calc(Acc, Store, yes(Num), Buttons, Clear, AnsLab) },
	config_calc(Interp, State).
press(_State0, dot, _Interp) -->
	{ error("press dot") }.

%-----------------------------------------------------------------------------%

	% Compute a new state of the calculator when some computation gets done.

:- pred compute(calc_button, int, list(half_expr), int, int, list(half_expr)).
:- mode compute(in, in, in, out, out, out) is det.

compute(num(_), _, _, _, _, _) :-
	error("shouldn't happen").
compute(plus, Acc, [], 0, Acc, [plus(Acc)]).
compute(plus, Acc, [Op|Ops], NewAcc, DispAcc, Stack) :-
	( Op = times(Fac),
		compute(plus, Fac*Acc, Ops, NewAcc, DispAcc, Stack)
	; Op = div(Fac),
		compute(plus, Fac//Acc, Ops, NewAcc, DispAcc, Stack)
	; Op = plus(Sum),
		compute(plus, Sum+Acc, Ops, NewAcc, DispAcc, Stack)
	; Op = minus(Sum),
		compute(plus, Sum-Acc, Ops, NewAcc, DispAcc, Stack)
	; Op = mark,
		NewAcc = Acc,
		DispAcc = Acc,
		Stack = [plus(Acc), Op|Ops]
	).
compute(minus, Acc, [], 0, Acc, [minus(Acc)]).
compute(minus, Acc, [Op|Ops], NewAcc, DispAcc, Stack) :-
	( Op = times(Fac),
		compute(minus, Fac*Acc, Ops, NewAcc, DispAcc, Stack)
	; Op = div(Fac),
		compute(minus, Fac//Acc, Ops, NewAcc, DispAcc, Stack)
	; Op = plus(Sum),
		compute(minus, Sum+Acc, Ops, NewAcc, DispAcc, Stack)
	; Op = minus(Sum),
		compute(minus, Sum-Acc, Ops, NewAcc, DispAcc, Stack)
	; Op = mark,
		NewAcc = Acc,
		DispAcc = Acc,
		Stack = [minus(Acc), Op|Ops]
	).
compute(times, Acc, [], 0, Acc, [times(Acc)]).
compute(times, Acc, [Op|Ops], NewAcc, DispAcc, Stack) :-
	( Op = times(Fac),
		compute(times, Fac*Acc, Ops, NewAcc, DispAcc, Stack)
	; Op = div(Fac),
		compute(times, Fac//Acc, Ops, NewAcc, DispAcc, Stack)
	; Op = plus(_),
		NewAcc = 0,
		DispAcc = Acc,
		Stack = [times(Acc), Op|Ops]
	; Op = minus(_),
		NewAcc = 0,
		DispAcc = Acc,
		Stack = [times(Acc), Op|Ops]
	; Op = mark,
		NewAcc = Acc,
		DispAcc = Acc,
		Stack = [times(Acc), Op|Ops]
	).
compute(divide, Acc, [], 0, Acc, [div(Acc)]).
compute(divide, Acc, [Op|Ops], NewAcc, DispAcc, Stack) :-
	( Op = times(Fac),
		compute(divide, Fac*Acc, Ops, NewAcc, DispAcc, Stack)
	; Op = div(Fac),
		compute(divide, Fac//Acc, Ops, NewAcc, DispAcc, Stack)
	; Op = plus(_),
		NewAcc = 0,
		DispAcc = Acc,
		Stack = [div(Acc), Op|Ops]
	; Op = minus(_),
		NewAcc = 0,
		DispAcc = Acc,
		Stack = [div(Acc), Op|Ops]
	; Op = mark,
		NewAcc = Acc,
		DispAcc = Acc,
		Stack = [div(Acc), Op|Ops]
	).
compute(equal, Acc, [], Acc, Acc, []).
compute(equal, Acc, [Op|Ops], NewAcc, DispAcc, Stack) :-
	( Op = times(Fac),
		compute(equal, Fac*Acc, Ops, NewAcc, DispAcc, Stack)
	; Op = div(Fac),
		compute(equal, Fac//Acc, Ops, NewAcc, DispAcc, Stack)
	; Op = plus(Sum),
		compute(equal, Sum+Acc, Ops, NewAcc, DispAcc, Stack)
	; Op = minus(Sum),
		compute(equal, Sum-Acc, Ops, NewAcc, DispAcc, Stack)
	; Op = mark,
		NewAcc = Acc,
		DispAcc = Acc,
		Stack = [Op|Ops]
	).
compute(dot, _, _, _, _, _) :-
	error("dot").
compute(lpar, Acc, Store, 0, Acc, [mark|Store]).
compute(rpar, _Acc0, [], 0, 0, []). % error!
compute(rpar, Acc, [Op|Ops], NewAcc, DispAcc, Stack) :-
	( Op = times(Fac),
		compute(rpar, Fac*Acc, Ops, NewAcc, DispAcc, Stack)
	; Op = div(Fac),
		compute(rpar, Fac//Acc, Ops, NewAcc, DispAcc, Stack)
	; Op = plus(Sum),
		compute(rpar, Sum+Acc, Ops, NewAcc, DispAcc, Stack)
	; Op = minus(Sum),
		compute(rpar, Sum-Acc, Ops, NewAcc, DispAcc, Stack)
	; Op = mark,
		NewAcc = Acc,
		DispAcc = Acc,
		Stack = Ops
	).

%-----------------------------------------------------------------------------%

	% Reset the calculator.

:- pred clear(calculator, tcl_interp, io__state, io__state).
:- mode clear(in, in, di, uo) is det.

clear(State0, Interp) -->
	{ State0 = calc(_Ans, _Store, _, Buttons, Clear, AnsLab) },
	{ State = calc(0, [], no, Buttons, Clear, AnsLab) },
	config_calc(Interp, State).

%------------------------------------------------------------------------------%
