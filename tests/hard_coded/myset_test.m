%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module myset_test.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.
:- import_module myset.
:- import_module list.

main(!IO) :-
    print_myset_rep({1}, !IO), nl(!IO),
    print_myset_rep({1} + {2}, !IO), nl(!IO),
    print_myset_rep({2} + {1}, !IO), nl(!IO),
    ( if {1} + {2} = [First | Rest] then
        print(First, !IO),
        print("+", !IO),
        print_myset_rep(Rest, !IO),
        nl(!IO)
    else
        print("failed\n", !IO)
    ),
    ( if {2} + {1} = [First2 | Rest2] then
        print(First2, !IO),
        print("+", !IO),
        print_myset_rep(Rest2, !IO),
        nl(!IO)
    else
        print("failed\n", !IO)
    ),
    S1 = {3} + {4},
    S2 = {4} + {3},
    ( if append([S1], [S2], [S2, S1]) then
        print_line("yes", !IO)
    else
        print_line("no", !IO)
    ).
