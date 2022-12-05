%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module factt.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module string.

:- pred example(int::in, int::out, int::out) is semidet.
:- pragma fact_table(example/3, "factt_examples").

%---------------------------------------------------------------------------%

main(!IO) :-
    show_examples(1, !IO).

:- pred show_examples(int::in, io::di, io::uo) is det.

show_examples(Cur, !IO) :-
    ( if Cur > 50 then
        true
    else
        ( if example(Cur, A, B) then
            io.format("%2d %2d %2d\n", [i(Cur), i(A), i(B)], !IO)
        else
            io.format("%2d  -  -\n", [i(Cur)], !IO)
        ),
        show_examples(Cur + 1, !IO)
    ).
