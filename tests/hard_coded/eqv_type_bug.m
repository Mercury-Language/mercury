%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module eqv_type_bug.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module cqueue.

:- import_module int.
:- import_module list.
:- import_module string.

main(!IO) :-
    cqueue.cqueue(CQ0),
    copy(CQ0, CQ0c),
    list.foldl((pred(I::in, Q0::in, Q::out) is det :-
        cqueue.append(Q0, I, Q)), [1, 2, 3, 4, 5], CQ0c, CQ1),
    copy(CQ1, CQ1c),
    io.write_line(CQ1c, !IO),
    cqueue.next(CQ1c, CQ2),
    copy(CQ2, CQ2c),
    io.write_line(CQ2c, !IO).
