%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% The compiler of 26/10/1999 aborted in code generation (_Var12 not found)
% on this test case due to a bug in common structure elimination.

:- module common_struct_bug.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- type my_list(T)
    --->    cons(data :: T, next :: my_list(T))
    ;       nil.

:- implementation.

main(!IO) :-
    List0_4 = cons(Var16, Var17),
    Var16 = 1,
    Var17 = cons(Var18, Var19),
    Var18 = 2,
    Var19 = cons(Var20, Var21),
    Var20 = 3,
    Var21 = nil,
    some [] (
        List0_4 = cons(_Var12, Var14),
        some [] (
            Var14 = cons(_, Var57),
            Var15 = cons(Var13, Var57)
        ),
        some [] (
            List0_4 = cons(Var58, _),
            List1_5 = cons(Var58, Var15)
        )
    ),
    Var13 = 4,
    io.write_line(List1_5, !IO).
