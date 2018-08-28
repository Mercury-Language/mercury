%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module browse_pretty.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module list.

:- type big
    --->    big(big, list(int), big)
    ;       small.

main(!IO) :-
    big_data(Data),
    io.print(Data, !IO),
    io.write_string(".\n", !IO).

:- pred big_data(big::out) is det.

big_data(Data) :-
    Data = big(
        big(
            big(
                small,
                [1],
                small
            ),
            [1, 2],
            small
        ),
        [1, 2, 3],
        big(
            big(
                small,
                [1, 2, 3, 4],
                big(
                    small,
                    [1, 2, 3, 4, 5],
                    small
                )
            ),
            [1, 2, 3, 4, 5, 6],
            small
        )
    ).
