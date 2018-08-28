%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module print_goal.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module bool.
:- import_module int.

:- type big
    --->    big(big, int, big)
    ;       small.

main(!IO) :-
    big_data(Data),
    io.print(Data, !IO),
    io.write_string(".\n", !IO),
    print_goal(yes, 100, 101, _, Y, 102, Z, !IO),
    io.print(Y, !IO),
    io.write_string(".\n", !IO),
    io.print(Z, !IO),
    io.write_string(".\n", !IO),
    print_goal(no, 100, 101, _, Y2, 102, Z2, !IO),
    io.print(Y2, !IO),
    io.write_string(".\n", !IO),
    io.print(Z2, !IO),
    io.write_string(".\n", !IO).

:- pred big_data(big::out) is det.

big_data(Data) :-
    Data = big(
        big(
            big(
                small,
                1,
                small
            ),
            2,
            small
        ),
        3,
        big(
            big(
                small,
                4,
                big(
                    small,
                    5,
                    small
                )
            ),
            6,
            small
        )
    ).

:- pred print_goal(bool::in, int::in, int::in, int::out, int::out, int::in,
    int::out, io::di, io::uo) is det.

print_goal(yes, _W, X, X + 1, X + 2, Y, Y + 1, !IO).
print_goal(no, _W, X, X + 2, X + 3, Y, Y + 2, !IO).
