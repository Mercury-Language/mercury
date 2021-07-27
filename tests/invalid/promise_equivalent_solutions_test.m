%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Various checks that promise_equivalent_solutions goals are treated properly.

:- module promise_equivalent_solutions_test.

:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module list.
:- import_module string.

main(!IO) :-
    % The equality theory with respect to which all solutions of the goal
    % inside the promise_equivalent_solutions are equivalent is the one
    % that views the lists as unsorted representations of sets, possibly
    % with duplicates.
    promise_equivalent_solutions [B] (
        ( A = [1, 2]
        ; A = [2, 1]
        ),
        ( B = [44, 33]
        ; B = [33, 44]
        )
    ),
    list.sort_and_remove_dups(A, ASorted),
    list.sort_and_remove_dups(B, BSorted),
    io.write_line(ASorted, !IO),
    io.write_line(BSorted, !IO),
    ( if
        promise_equivalent_solutions [C, ASorted] (
            ASorted = [_ | ATail],
            ( C = [5] ++ ATail
            ; C = ATail ++ [5]
            )
        )
    then
        list.sort_and_remove_dups(C, CSorted),
        io.write_line(CSorted, !IO)
    else
        io.write("cannot compute CSorted\n", !IO)
    ).
