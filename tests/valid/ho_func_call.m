%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Regression test for higher-order function call in code with common
% subexpression; mercury 0.7 failed this test, reporting
% "Software Error: modecheck fails when repeated",
% due to confusion between h.o. _function_ call and h.o. _predicate_ call.

:- module ho_func_call.

:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module string.

main(IO, put_stocks(get_stocks(IO))).

:- type maybe(T)
    --->    yes(T)
    ;       no.

:- type code == int.
:- type qty == int.

:- type stock
    --->    stock(code, qty).

:- type iostocks
    --->    ios(io, stocks).

:- type stocks
    --->    []
    ;       [stock  | stocks]
    ;       closure(func(io) = iostocks).

:- inst iostocks == unique(ios(unique, stocks)).
:- inst stocks ==
    bound( [] ; [ground | stocks] ; closure(func(di) = is_out is det)).

:- mode is_in  == di(iostocks).
:- mode is_out == out(iostocks).

:- func put_stocks(iostocks) = io.
:- mode put_stocks(is_in) = uo is det.

put_stocks(ios(IO, [])) = IO.
put_stocks(ios(IO, [S | T])) = put_stocks(ios(IO1, T)) :-
    out_stock(S, IO, IO1).
put_stocks(ios(IO, closure(Func))) = put_stocks(apply(Func, IO)).

:- pred out_stock(stock::in, io::di, io::uo) is det.

out_stock(stock(C, Q), !IO) :-
%   io.format("%i %i\n", [i(C), i(Q)]).
    io.write_int(C, !IO),
    io.write_char(' ', !IO),
    io.write_int(Q, !IO),
    io.nl(!IO).

:- func get_stocks(io) = iostocks.
:- mode get_stocks(di) = is_out is det.

get_stocks(S0) = ios(S, Stocks) :-
    tokenize(MStock, S0, S),
    ( MStock = no, Stocks = []
    ; MStock = yes(Stock), Stocks = [Stock | closure(get_stocks)]
    ).

:- pred tokenize(maybe(stock)::out, io::di, io::uo) is det.

tokenize(S, !IO) :-
    io.read_line(CL0, !IO),
    ( if
        CL0 = ok(CL),
        string.from_char_list(CL, Line),
        string.sub_string_search(Line, " ", Index),
        string.split(Line, Index, Left, Right0),
        string.first_char(Right0, _, Right),
        string.to_int(Left, Code),
        string.to_int(Right, Qty)
    then
        S = yes(stock(Code, Qty))
    else
        S = no
    ).
