%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module lco_mday_bug_1.

:- interface.

:- import_module io.

:- pred main(io, io).
:- mode main(di, uo) is det.

:- implementation.

:- import_module float.
:- import_module int.

:- type number
    --->    num_int(int)
    ;       num_float(float).

:- type standard_func
    --->    date_func
    ;       date_parse.

main(!IO) :-
    p(X),
    io.write_line(num_float(X), !IO),
    call_standard_func(date_parse, Res),
    (
        Res = num_int(I),
        io.write_string("int ", !IO),
        io.write_int(I, !IO),
        io.nl(!IO)
    ;
        Res = num_float(F),
        io.write_string("float ", !IO),
        io.write_float(F, !IO),
        io.nl(!IO)
    ),
    io.write_line(Res, !IO).

%---------------------------------------------------------------------------%

:- pragma no_inline(p/1).
:- pred p(float::out) is det.

p(43.0).

%---------------------------------------------------------------------------%

:- pred call_standard_func(standard_func, number).
:- mode call_standard_func(in, out) is det.

call_standard_func(date_func, Res) :-
    Res = num_int(0).
call_standard_func(date_parse, Res) :-
    to_integer(num_int(0), T),
    Res = num_float(T).

%---------------------------------------------------------------------------%

:- pred to_integer(number, float).
:- mode to_integer(in, out) is det.

to_integer(N0, N) :-
    (
        N0 = num_int(_),
        N = 41.0
    ;
        N0 = num_float(_),
        call_standard_func(date_func, _Res2),
        N = 42.0
    ).
