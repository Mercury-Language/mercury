%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 sts=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module random_shuffle.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.
:- import_module random.
:- import_module random.sfc32.

main(!IO) :-
    List = 1 `..` 100,
    sfc32.init(P, S),
    make_io_urandom(P, S, M, !IO),
    test(M, List, 10, !IO).

:- pred test(M::in, list(int)::in, int::in, io::di, io::uo) is det
    <= urandom(M, io).

test(M, List, Count, !IO) :-
    ( if Count > 0 then
        shuffle_list(M, List, Shuffled, !IO),
        sort_and_remove_dups(Shuffled, Sorted),
        ( if Sorted = List then
            io.write_string("Passed.\n", !IO)
        else
            io.write_string("Failed!\n", !IO)
        ),
        test(M, List, Count - 1, !IO)
    else
        true
    ).

