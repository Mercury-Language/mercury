:- module ho_func_call.

% regression test for higher-order function call in
% code with common sub-expression; mercury 0.7 failed this test,
% reporting "Software Error: modecheck fails when repeated",
% due to confusion between h.o. _function_ call and h.o. _predicate_ call.

:- interface.
:- import_module io.
:- pred main(io__state::di, io__state::uo) is det.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- implementation.

:- import_module string.

main(IO, putStocks(getStocks(IO))).

:- type maybe(T) ---> yes(T) ; no.
:- type code == int.
:- type qty == int.
:- type stock ---> stock(code, qty).
% :- type stocks ---> list(stock).
:- type iostocks ---> ios(io__state, stocks).
:- type stocks
	--->	[]
	;	[stock  | stocks]
	;	closure(func(io__state) = iostocks)
	.

:- inst iostocks == unique(ios(unique, stocks)).
:- inst stocks ==
	bound(	[]
	;	[ground | stocks]
	;	closure(func(di) = is_out is det)
	).

:- mode is_in  == di(iostocks).
:- mode is_out == out(iostocks).

:- func putStocks(iostocks) = io__state.
:- mode putStocks(is_in) = uo is det.
putStocks(ios(IO, [])) = IO.
putStocks(ios(IO, [S | T])) = putStocks(ios(IO1, T)) :-
	outStock(S, IO, IO1).
putStocks(ios(IO, closure(Func))) = putStocks(apply(Func, IO)).

:- pred outStock(stock::in, io__state::di, io__state::uo) is det.
outStock(stock(C, Q)) -->
%	io__format("%i %i\n", [i(C), i(Q)]).
	io__write_int(C),
	io__write_char(' '),
	io__write_int(Q),
	io__nl.

:- func getStocks(io__state) = iostocks.
:- mode getStocks(di) = is_out is det.
getStocks(S0) = ios(S, Stocks) :-
	tokenize(MStock, S0, S),
	( MStock = no, Stocks = []
	; MStock = yes(Stock), Stocks = [Stock | closure(getStocks)]
	).

:- pred tokenize(maybe(stock)::out, io__state::di, io__state::uo) is det.
tokenize(S) -->
	io__read_line(CL0),
	{ (CL0 = ok(CL),
		string__from_char_list(CL, Line),
		string__sub_string_search(Line, " ", Index),
		string__split(Line, Index, Left, Right0),
		string__first_char(Right0, _, Right),
		string__to_int(Left, Code),
		string__to_int(Right, Qty)) ->
		S = yes(stock(Code, Qty))
	;	S = no
	}.


