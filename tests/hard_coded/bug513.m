%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%

:- module bug513.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module solutions.

:- type apply
    --->    first
    ;       second
    ;       take(int).

:- pred revert(int::in, apply::out) is multi.

revert(N, Target) :-
    ( if
        ( N rem 0x02 = 0x00, Temp = first
        ; N rem 0x03 = 0x00, Temp = second
        )
    then
        Target = Temp
    else
        Target = take(N)
    ).

main(!IO) :-
    solutions(revert(5), N),
    io.write_line(N, !IO).

:- end_module bug513.
