%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Regression test for a bug where the compiler would incorrectly specialize
% the call to P in do_stuff.

:- module ho_spec_branch_bug.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module maybe.

main(!IO) :-
    do_stuff(yes(1), !IO).

:- pred ho1(io::di, io::uo) is det.

ho1(!IO) :-
    io.write_string("ho1\n", !IO).

:- pred ho2(io::di, io::uo) is det.

ho2(!IO) :-
    io.write_string("ho2\n", !IO).

:- func get_ho2 = (pred(io, io)).
:- mode get_ho2 = out(pred(di, uo) is det) is det.

get_ho2 = ho2.

:- pred do_stuff(maybe(int)::in, io::di, io::uo) is det.

do_stuff(Maybe, !IO) :-
    (
        Maybe = no,
        P = ho1
    ;
        Maybe = yes(_),
        P = get_ho2
    ),
    P(!IO).
