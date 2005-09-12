%
% A regression test.  Make sure list__merge_and_remove_dups works.
%
% The Mercury library of September 15th, 1998 failed this test.
%
% Peter Herkenrath <aik01@rrz.uni-koeln.de> sent this one in.

:- module merge_and_remove_dups.

:- interface.

:- import_module io.

:- pred main(io.state::di, io.state::uo) is det.

:- implementation.

:- import_module list.

main -->
        { List1 = [1,2,3],
          List2 = [3,4,5],
          P = (pred(I1::in,I2::in,R::out) is det :-
                         compare(R,I1,I2) ),
          list__merge_and_remove_dups(P,List1,List2,List3) },
        io__write_string("List1: "),
        io__print(List1),
        io__write_string("\nList2: "),
        io__print(List2),
        io__write_string("\nList3: "),
        io__print(List3),
        io__write_string("\n").

%Output:
%List1: [1, 2, 3]
%List2: [3, 4, 5]
%List3: [1, 2, 3, 3, 4, 5]
%              !  !



