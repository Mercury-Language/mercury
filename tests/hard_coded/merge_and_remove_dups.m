%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% A regression test. Make sure list.merge_and_remove_dups works.
% The Mercury library of September 15th, 1998 failed this test.
% Peter Herkenrath <aik01@rrz.uni-koeln.de> sent this one in.
%

:- module merge_and_remove_dups.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.

main(!IO) :-
    List1 = [1, 2, 3],
    List2 = [3, 4, 5],
    P =
        ( pred(I1::in, I2::in, R::out) is det :-
             compare(R, I1, I2)
        ),
    list.merge_and_remove_dups(P, List1, List2, List3),

    io.write_string("List1: ", !IO),
    io.print(List1, !IO),
    io.write_string("\nList2: ", !IO),
    io.print(List2, !IO),
    io.write_string("\nList3: ", !IO),
    io.print(List3, !IO),
    io.write_string("\n", !IO).

% Output:
% List1: [1, 2, 3]
% List2: [3, 4, 5]
% List3: [1, 2, 3, 3, 4, 5]

