%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 expandtab ft=mercury
%---------------------------------------------------------------------------%
%
% Test library/psqueue.m.
%

:- module psqueue_test.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.
:- import_module pair.
:- import_module psqueue.
:- import_module set.
:- import_module string.

main(!IO) :-
    test_empty(!IO),
    test_extended_paper_ex(PSQ_A, !IO),
    test_at_most(PSQ_A, !IO),
    test_to_ord_list(PSQ_A, !IO),
    test_delete(PSQ_A, !IO),

    test_from_assoc_list(PSQ_B, !IO),
    test_adjust(PSQ_B, !IO).

:- pred test_empty(io::di, io::uo) is det.

test_empty(!IO) :-
    psqueue.init(PSQ0),
    io.write_string("empty test: ", !IO),
    ( if psqueue.is_empty(PSQ0) then
        io.write_string("ok\n", !IO)
    else
        io.write_string("not ok\n", !IO)
    ),

    psqueue.det_insert(1, "a", PSQ0, PSQ1),
    io.write_string("nonempty test: ", !IO),
    ( if psqueue.is_empty(PSQ1) then
        io.write_string("not ok\n", !IO)
    else
        io.write_string("ok\n", !IO)
    ),

    io.write_string("empty test after remove: ", !IO),
    ( if
        psqueue.remove(_, "a", PSQ1, PSQ2),
        psqueue.is_empty(PSQ2)
    then
        io.write_string("ok\n", !IO)
    else
        io.write_string("not ok\n", !IO)
    ).

:- pred test_extended_paper_ex(psqueue(int, string)::out, io::di, io::uo)
    is det.

test_extended_paper_ex(PSQ, !IO) :-
    io.write_string("\nextended paper example test:\n", !IO),
    some [!PSQ] (
        psqueue.init(!:PSQ),
        det_insert_and_output(1, "Lennart2", !PSQ, !IO),
        det_insert_and_output(8, "Warren",   !PSQ, !IO),
        det_insert_and_output(2, "Erik",     !PSQ, !IO),
        det_insert_and_output(7, "Richard",  !PSQ, !IO),
        det_insert_and_output(5, "Simon",    !PSQ, !IO),
        det_insert_and_output(4, "Charles",  !PSQ, !IO),
        det_insert_and_output(6, "Mary",     !PSQ, !IO),
        det_insert_and_output(3, "Phil",     !PSQ, !IO),
        det_insert_and_output(1, "Lennart",  !PSQ, !IO),
        det_insert_and_output(2, "Erik2",    !PSQ, !IO),
        PSQ = !.PSQ
    ).

:- pred det_insert_and_output(int::in, string::in,
    psqueue(int, string)::in, psqueue(int, string)::out, io::di, io::uo) is det.

det_insert_and_output(Prio, Key, !PSQ, !IO) :-
    io.format("size before insert is %d\n", [i(psqueue.size(!.PSQ))], !IO),
    io.format("inserting prio %d, key %s, giving\n", [i(Prio), s(Key)], !IO),
    psqueue.det_insert(Prio, Key, !PSQ),
    output_psqueue(!.PSQ, !IO),
    io.format("size after insert is %d\n\n", [i(psqueue.size(!.PSQ))], !IO).

:- pred test_at_most(psqueue(int, string)::in, io::di, io::uo) is det.

test_at_most(PSQ, !IO) :-
    io.write_string("\nat_most tests\n", !IO),
    test_at_most_loop(PSQ, 0, 10, !IO).

:- pred test_at_most_loop(psqueue(int, string)::in, int::in, int::in,
    io::di, io::uo) is det.

test_at_most_loop(PSQ, Cur, Max, !IO) :-
    ( if Cur < Max then
        io.format("at_most %d: ", [i(Cur)], !IO),
        psqueue.at_most(PSQ, Cur, AssocList),
        io.print(AssocList, !IO),
        io.nl(!IO),
        test_at_most_loop(PSQ, Cur + 1, Max, !IO)
    else
        true
    ).

:- pred test_to_ord_list(psqueue(int, string)::in, io::di, io::uo) is det.

test_to_ord_list(PSQ, !IO) :-
    io.write_string("\nto_ord_assoc_list test:\n", !IO),
    psqueue.to_assoc_list(PSQ, AssocList),
    io.print(AssocList, !IO),
    io.nl(!IO).

:- pred test_delete(psqueue(int, string)::in, io::di, io::uo) is det.

test_delete(PSQ, !IO) :-
    io.write_string("\ndelete tests\n", !IO),
    test_delete_key(PSQ, "Lennart2", !IO),
    test_delete_key(PSQ, "Warren", !IO),
    test_delete_key(PSQ, "Erik", !IO),
    test_delete_key(PSQ, "Richard", !IO),
    test_delete_key(PSQ, "Simon", !IO),
    test_delete_key(PSQ, "Charles", !IO),
    test_delete_key(PSQ, "Mary", !IO),
    test_delete_key(PSQ, "Phil", !IO),
    test_delete_key(PSQ, "Lennart", !IO),
    test_delete_key(PSQ, "Erik2", !IO),
    test_delete_key(PSQ, "NotThere", !IO).

:- pred test_delete_key(psqueue(int, string)::in, string::in, io::di, io::uo)
    is det.

test_delete_key(PSQ0, Key, !IO) :-
    ( if remove(Prio, Key, PSQ0, PSQ) then
        psqueue.to_assoc_list(PSQ, AssocList),
        io.format("delete key %s: prio %d, left %s\n",
            [s(Key), i(Prio), s(string(AssocList))], !IO)
    else
        io.format("delete key %s: failed\n", [s(Key)], !IO)
    ).

:- pred test_from_assoc_list(psqueue(int, string)::out, io::di, io::uo) is det.

test_from_assoc_list(ViaAssocListPSQ, !IO) :-
    io.write_string("\nfrom_assoc_list test:\n", !IO),
    InList = [4 - "H", 1 - "L", 2 - "B", 0 - "M", 3 - "N"],
    psqueue.from_assoc_list(InList, ViaAssocListPSQ),
    some [!PSQ] (
        psqueue.init(!:PSQ),
        psqueue.det_insert(4, "H", !PSQ),
        psqueue.det_insert(1, "L", !PSQ),
        psqueue.det_insert(2, "B", !PSQ),
        psqueue.det_insert(0, "M", !PSQ),
        psqueue.det_insert(3, "N", !PSQ),
        ViaInsertsPSQ = !.PSQ
    ),

    psqueue.to_assoc_list(ViaAssocListPSQ, ViaAssocListAssocList),
    psqueue.to_assoc_list(ViaInsertsPSQ, ViaInsertsAssocList),

    io.write_string("via from_assoc_list and via inserts assoc lists ", !IO),
    ( if ViaAssocListAssocList = ViaInsertsAssocList then
        io.write_string("agree:\n", !IO),
        io.print(ViaAssocListAssocList, !IO),
        io.nl(!IO)
    else
        io.write_string("disagree:\n", !IO),
        io.write_string("via from_assoc_list:\n", !IO),
        io.print(ViaAssocListAssocList, !IO),
        io.nl(!IO),
        io.write_string("via inserts:\n", !IO),
        io.print(ViaInsertsAssocList, !IO),
        io.nl(!IO)
    ),

    io.write_string("via from_assoc_list and via inserts psqueues ", !IO),
    ( if ViaAssocListPSQ = ViaInsertsPSQ then
        io.write_string("agree:\n", !IO),
        output_psqueue(ViaAssocListPSQ, !IO),
        io.nl(!IO)
    else
        io.write_string("disagree:\n", !IO),
        io.write_string("via from_assoc_list:\n", !IO),
        output_psqueue(ViaAssocListPSQ, !IO),
        io.write_string("via inserts:\n", !IO),
        output_psqueue(ViaInsertsPSQ, !IO)
    ).

:- pred test_adjust(psqueue(int, string)::in, io::di, io::uo) is det.

test_adjust(PSQ, !IO) :-
    psqueue.to_assoc_list(PSQ, AssocList),
    io.format("\nadjust tests on\n%s\n", [s(string(AssocList))], !IO),
    test_adjust_key(PSQ, 0, "H", !IO),  % was 4
    test_adjust_key(PSQ, 8, "L", !IO),  % was 1
    test_adjust_key(PSQ, 2, "B", !IO),  % was 2
    test_adjust_key(PSQ, 2, "M", !IO),  % was 0
    test_adjust_key(PSQ, 1, "N", !IO),  % was 3
    test_adjust_key(PSQ, 7, "X", !IO).  % was not there

:- pred test_adjust_key(psqueue(int, string)::in, int::in, string::in,
    io::di, io::uo) is det.

test_adjust_key(PSQ0, NewPrio, Key, !IO) :-
    io.format("adjusting priority of %s to %d ",
        [s(Key), i(NewPrio)], !IO),
    ( if psqueue.adjust(func(_) = NewPrio, Key, PSQ0, PSQ) then
        to_assoc_list(PSQ, AssocList),
        io.format("succeeded\n%s\n", [s(string(AssocList))], !IO)
    else
        io.write_string("failed\n", !IO)
    ).

:- pred output_psqueue(psqueue(int, string)::in, io::di, io::uo) is det.

output_psqueue(PSQ, !IO) :-
    io.write_string(verify_and_dump_psqueue(PSQ), !IO).
