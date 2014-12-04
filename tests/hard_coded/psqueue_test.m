%
% Test library/psqueue.m
%
:- module psqueue_test.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.
:- import_module psqueue.
:- import_module set.
:- import_module string.

:- pred test_psqueue_empty is semidet.

test_psqueue_empty :-
    psqueue.init(PSQ `with_type` psqueue(int, int)),
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
    psqueue.det_insert(8, "Warren", !PSQ),
    psqueue.det_insert(2, "Erik", !PSQ),
    psqueue.det_insert(7, "Richard", !PSQ),
    psqueue.det_insert(5, "Simon", !PSQ),
    psqueue.det_insert(4, "Charles", !PSQ),
    psqueue.det_insert(6, "Mary", !PSQ),
    psqueue.det_insert(3, "Phil", !PSQ),
    psqueue.det_insert(1, "Lennart", !PSQ).

:- pred test_at_most(psqueue(int, string)::in, io::di, io::uo) is det.

test_at_most(PSQ_EX, !IO) :-
    io.print("at_most 4: ", !IO),
    at_most(PSQ_EX, 4, AList),
    io.print(set.from_list(AList), !IO),
    io.nl(!IO).

:- pred test_to_ord_list(psqueue(int, string)::in, io::di, io::uo) is det.

test_to_ord_list(PSQ_EX, !IO) :-
    io.print("to_ord_assoc_list test: ", !IO),
    to_assoc_list(PSQ_EX, AList),
    io.print(AList, !IO),
    io.nl(!IO).

:- pred test_delete(psqueue(int, string)::in, io::di, io::uo) is det.

test_delete(PSQ_EX, !IO) :-
    ( remove(P, "Phil", PSQ_EX, PSQ_DEL) ->
        to_assoc_list(PSQ_DEL, AList),
        io.format("delete Phil: %s, %d\n", [s(string(AList)), i(P)], !IO)
    ;
        io.write_string("remove failed\n", !IO)
    ).

:- pred test_from_assoc_list(psqueue(int, string)::out, io::di, io::uo) is det.

test_from_assoc_list(PSQ5, !IO) :-
    io.print("from_assoc_list: ", !IO),
    init(PSQ0),
    det_insert(4, "H", PSQ0, PSQ1),
    det_insert(1, "L", PSQ1, PSQ2),
    det_insert(2, "B", PSQ2, PSQ3),
    det_insert(0, "M", PSQ3, PSQ4),
    det_insert(3, "N", PSQ4, PSQ5),
    to_assoc_list(PSQ5, AList),
    io.print(AList, !IO),
    io.nl(!IO).

:- pred test_adjust(psqueue(int, string)::in, io::di, io::uo) is det.

test_adjust(PSQ, !IO) :-
    ( adjust(func(_) = 10, "M", PSQ, PSQ0) ->
        to_assoc_list(PSQ0, AList),
        io.format("Adjust: %s\n", [s(string(AList))], !IO)
    ;
        io.write_string("Adjust failed\n", !IO)
    ).

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
