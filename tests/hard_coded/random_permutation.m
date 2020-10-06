%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module random_permutation.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.
:- import_module random.

main(!IO) :-
    List = gen_sorted_list(1, 100),
    random.init(1, RS),
    do_tests(List, 10, RS, !IO).

:- pred do_tests(list(int)::in, int::in, random.supply::mdi,
    io::di, io::uo) is det.

do_tests(List, Count, RS0, !IO) :-
    ( if Count > 1 then
        random.permutation(List, Perm, RS0, RS1),
        list.sort_and_remove_dups(Perm, SortedList),
        ( if SortedList = List then
            io.write_string("Test passed.\n", !IO)
        else
            io.write_string("Test failed!\n", !IO)
        ),
        do_tests(List, Count - 1, RS1, !IO)
    else
        true
    ).

:- func gen_sorted_list(int, int) = list(int).

gen_sorted_list(M, N) =
    ( if M > N then
        []
    else
        [M | gen_sorted_list(M + 1, N)]
    ).

