%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module subtype_order.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module list.

:- type fruit
    --->    apple(int)
    ;       banana
    ;       lemon
    ;       lime
    ;       orange(int)
    ;       peach
    ;       pear
    ;       pomelo
    ;       tangelo
    ;       tomato.

:- type citrus =< fruit
    --->    pomelo
    ;       lime
    ;       tangelo
    ;       orange(int)
    ;       lemon.

main(!IO) :-
    L1 = [orange(123), lime, tangelo, pomelo, lemon] : list(fruit),
    L2 = [orange(123), lime, tangelo, pomelo, lemon] : list(citrus),
    list.sort(L1, S1),
    list.sort(L2, S2),
    io.print_line(S1, !IO),
    io.print_line(S2, !IO).
