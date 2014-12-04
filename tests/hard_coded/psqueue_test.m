%
% Test library/psqueue.m
%
:- module psqueue_test.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module psqueue.
:- import_module set.

:- pred test_psqueue_empty is semidet.

test_psqueue_empty :-
    psqueue.init(PSQ),
    psqueue.is_empty(PSQ).

:- pred test_empty(io::di, io::uo) is det.

test_empty(!IO) :-
    io.print("empty test: ", !IO),
    ( test_psqueue_empty ->
        io.print("ok", !IO)
    ;
        io.print("nok", !IO)
    ),
    io.nl(!IO).

:- pred test_paper_ex(psqueue(int, string)::out, io::di, io::uo) is det.

test_paper_ex(PSQ_EX, !IO) :-
    io.print("paper example test: ", !IO),
    test_psqueue_paper_ex(psqueue.init, PSQ_EX),
    io.print(PSQ_EX, !IO),
    io.nl(!IO).

:- pred test_psqueue_paper_ex(psqueue(int, string)::in,
    psqueue(int, string)::out) is det.

test_psqueue_paper_ex(!PSQ) :-
    psqueue.insert(8, "Warren", !PSQ),
    psqueue.insert(2, "Erik", !PSQ),
    psqueue.insert(7, "Richard", !PSQ),
    psqueue.insert(5, "Simon", !PSQ),
    psqueue.insert(4, "Charles", !PSQ),
    psqueue.insert(6, "Mary", !PSQ),
    psqueue.insert(3, "Phil", !PSQ),
    psqueue.insert(1, "Lennart", !PSQ).

:- pred test_at_most(psqueue(int, string)::in, io::di, io::uo) is det.

test_at_most(PSQ_EX, !IO) :-
    io.print("at_most 4: ", !IO),
    at_most(4, PSQ_EX, AList),
    io.print(set.from_list(AList), !IO),
    io.nl(!IO).

:- pred test_to_ord_list(psqueue(int, string)::in, io::di, io::uo) is det.

test_to_ord_list(PSQ_EX, !IO) :-
    io.print("to_ord_assoc_list test: ", !IO),
    to_ord_assoc_list(PSQ_EX, AList),
    io.print(AList, !IO),
    io.nl(!IO).

:- pred test_delete(psqueue(int, string)::in, io::di, io::uo) is det.

test_delete(PSQ_EX, !IO) :-
    io.print("delete and to_or_assoc: ", !IO),
    delete("Phil", PSQ_EX, PSQ_DEL),
    to_ord_assoc_list(PSQ_DEL, AList),
    io.print(AList, !IO),
    io.nl(!IO).

:- pred test_from_assoc_list(psqueue(int, string)::out, io::di, io::uo) is det.

test_from_assoc_list(PSQ5, !IO) :-
    io.print("from_assoc_list: ", !IO),
    init(PSQ0),
    insert(4, "H", PSQ0, PSQ1),
    insert(1, "L", PSQ1, PSQ2),
    insert(2, "B", PSQ2, PSQ3),
    insert(0, "M", PSQ3, PSQ4),
    insert(3, "N", PSQ4, PSQ5),
    to_ord_assoc_list(PSQ5, AList),
    io.print(AList, !IO),
    io.nl(!IO).

:- pred test_adjust(psqueue(int, string)::in, io::di, io::uo) is det.

test_adjust(PSQ, !IO) :-
    io.print("Adjust: ", !IO),
    adjust(func(_) = 10, "M", PSQ, PSQ0),
    to_ord_assoc_list(PSQ0, AList),
    io.print(AList, !IO).

:- pred test_all(io::di, io::uo) is det.

test_all(!IO) :-
    test_empty(!IO),
    test_paper_ex(PSQ_EX, !IO),
    test_at_most(PSQ_EX, !IO),
    test_to_ord_list(PSQ_EX, !IO),
    test_delete(PSQ_EX, !IO),
    test_from_assoc_list(PSQ0, !IO),
    test_adjust(PSQ0, !IO),
    io.nl(!IO).

main(!IO) :-
    test_all(!IO).
