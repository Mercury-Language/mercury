%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module record_syntax_bug_4.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.

:- type info
    --->    info(
                field :: int
            ).

main(!IO) :-
    List = list__map(field(info(1)), [1, 2, 3]),
    io.write_line(List, !IO).

:- func field(info, int) = int.

field(_Info, Int) = Int.
