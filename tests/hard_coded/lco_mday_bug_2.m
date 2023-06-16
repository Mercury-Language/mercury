%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module lco_mday_bug_2.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.

:- type number
    --->    num_int(int)
    ;       num_float(string).

:- type standard_func
    --->    date_func
    ;       date_parse.

main(!IO) :-
    call_standard_func(date_parse, Res),
    io.write_line(Res, !IO).

%---------------------------------------------------------------------------%

:- pred call_standard_func(standard_func, number).
:- mode call_standard_func(in, out) is det.

call_standard_func(date_func, Res) :-
    Res = num_int(0).
call_standard_func(date_parse, Res) :-
    to_integer(num_int(0), T),
    Res = num_float(T).

%---------------------------------------------------------------------------%

:- pred to_integer(number, string).
:- mode to_integer(in, out) is det.

to_integer(N0, N) :-
    (
        N0 = num_int(_),
        N = "41"
    ;
        N0 = num_float(_),
        call_standard_func(date_func, _Res2),
        N = "42"
    ).
