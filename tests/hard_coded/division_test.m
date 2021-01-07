%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% test the handling of `//', rem, div, and mod.
% the first pair should truncate towards zero.
% the second pair should truncate towards negative infinity.

:- module division_test.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int.

main(!IO) :-
    ( if
        quot_test(3, 8, 0, 3),      % 3 / 8 = 0 + 3/8
        quot_test(5, 8, 0, 5),      % 5 / 8 = 0 + 5/8
        quot_test(7, 2, 3, 1),      % 7 / 2 = 3 + 1/2
        quot_test(100, 13, 7, 9)    % 100 / 13 = 7 + 9/13
    then
        io.write_string("`//' test succeeded\n", !IO)
    else
        io.write_string("`//' test failed\n", !IO)
    ),
    ( if
        rem_test(3, 8, 0, 3),       % 3 / 8 = 0 + 3/8
        rem_test(5, 8, 0, 5),       % 5 / 8 = 0 + 5/8
        rem_test(7, 2, 3, 1),       % 7 / 2 = 3 + 1/2
        rem_test(100, 13, 7, 9)     % 100 / 13 = 7 + 9/13
    then
        io.write_string("rem test succeeded\n", !IO)
    else
        io.write_string("rem test failed\n", !IO)
    ),
    ( if
        div_test(3, 8, 0, 3),       % 3 / 8 = 0 + 3/8
        div_test(5, 8, 0, 5),       % 5 / 8 = 0 + 5/8
        div_test(7, 2, 3, 1),       % 7 / 2 = 3 + 1/2
        div_test(100, 13, 7, 9)     % 100 / 13 = 7 + 9/13
    then
        io.write_string("div test succeeded\n", !IO)
    else
        io.write_string("div test failed\n", !IO)
    ),
    ( if
        mod_test(3, 8, 0, 3),       % 3 / 8 = 0 + 3/8
        mod_test(5, 8, 0, 5),       % 5 / 8 = 0 + 5/8
        mod_test(7, 2, 3, 1),       % 7 / 2 = 3 + 1/2
        mod_test(100, 13, 7, 9)     % 100 / 13 = 7 + 9/13
    then
        io.write_string("mod test succeeded\n", !IO)
    else
        io.write_string("mod test failed\n", !IO)
    ).

:- pred quot_test(int::in, int::in, int::in, int::in) is semidet.
quot_test(Num, Div, Quot, _Rem) :-
    Num // Div = Quot,
    (-Num) // Div = -Quot,
    (-Num) // (-Div) = Quot,
    Num // (-Div) = -Quot.

:- pred rem_test(int::in, int::in, int::in, int::in) is semidet.
rem_test(Num, Div, _Quot, Rem) :-
    Num rem Div = Rem,
    (-Num) rem Div = -Rem,
    (-Num) rem (-Div) = -Rem,
    Num rem (-Div) = Rem.

:- pred div_test(int::in, int::in, int::in, int::in) is semidet.
div_test(Num, Div, Quot, _Rem) :-
    Num div Div = Quot,
    (-Num) div Div = -Quot - 1,
    (-Num) div (-Div) = Quot,
    Num div (-Div) = -Quot - 1.

:- pred mod_test(int::in, int::in, int::in, int::in) is semidet.
mod_test(Num, Div, _Quot, Rem) :-
    Num mod Div = Rem,
    (-Num) mod Div = Div - Rem,
    (-Num) mod (-Div) = -Rem,
    Num mod (-Div) = -(Div - Rem).
