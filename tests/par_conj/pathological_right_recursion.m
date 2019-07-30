%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module pathological_right_recursion.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module string.
:- import_module list.

main(!IO) :-
    count_up(0, 100000, List),
    list.chunk(List, 10, ListChunks),
    io.write_list(ListChunks, "", io.write_line, !IO).

:- pred count_up(int::in, int::in, list(int)::out) is det.

count_up(N0, Max, List) :-
    ( if N0 >= Max then
        List = []
    else
        % The pathological case for recursion with the parallel conjunction
        % is right-recursion. That is where any conjunct other than
        % the leftmost is recursive, and the compiler cannot re-order
        % the conjuncts because of a dependency (such as N below).
        %
        % If this code were to create 1000 contexts, each with their own
        % stacks, it would consume too much memory and possibly bring the OS
        % to its knees.
        (
            N = N0 + 1,
            % Spend some time so that the spark representing the second
            % conjunct will probably be stolen.
            loop(0, 1000, N2)
        &
            (
                count_up(N0 + 1, Max, List0),
                List = [N, N2 | List0]
            )
        )
    ).

:- pred loop(int::in, int::in, int::out) is det.

loop(N, Max, Result) :-
    ( if N < Max then
        loop(N + 1, Max, Result)
    else
        Result = N
    ).
