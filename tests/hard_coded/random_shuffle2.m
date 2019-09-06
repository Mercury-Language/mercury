%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 sts=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module random_shuffle2.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.
:- import_module random.
:- import_module random.sfc16.

main(!IO) :-
    List = 1 `..` 100,
    R = sfc16.init,
    test(List, 10, R, _, !IO).

:- pred test(list(int)::in, int::in, R::in, R::out, io::di, io::uo) is det
    <= random(R).

test(List, Count, !R, !IO) :-
    ( if Count > 0 then
        shuffle_list(List, Shuffled, !R),
        sort_and_remove_dups(Shuffled, Sorted),
        ( if Sorted = List then
            io.write_string("Passed.\n", !IO)
        else
            io.write_string("Failed!\n", !IO)
        ),
        test(List, Count - 1, !R, !IO)
    else
        true
    ).

