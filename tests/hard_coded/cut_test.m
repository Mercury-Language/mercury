%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module cut_test.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.

main(!IO) :-
    ( if best(100) then
        io.write_string("best case nondet test 1 succeeded:   BUG.\n", !IO)
    else
        io.write_string("best case nondet test 1 failed:      OK.\n", !IO)
    ),
    ( if best(300) then
        io.write_string("best case nondet test 2 succeeded:   OK.\n", !IO)
    else
        io.write_string("best case nondet test 2 failed:      BUG.\n", !IO)
    ),
    ( if middle(100) then
        io.write_string("middle case nondet test 1 succeeded: BUG.\n", !IO)
    else
        io.write_string("middle case nondet test 1 failed:    OK.\n", !IO)
    ),
    ( if middle(180) then
        io.write_string("middle case nondet test 2 succeeded: OK.\n", !IO)
    else
        io.write_string("middle case nondet test 2 failed:    BUG.\n", !IO)
    ),
    ( if middle(190) then
        io.write_string("middle case nondet test 3 succeeded: OK.\n", !IO)
    else
        io.write_string("middle case nondet test 3 failed:    BUG.\n", !IO)
    ),
    ( if middle(200) then
        io.write_string("middle case nondet test 4 succeeded: OK.\n", !IO)
    else
        io.write_string("middle case nondet test 4 failed:    BUG.\n", !IO)
    ),
    ( if worst(100) then
        io.write_string("worst case nondet test 1 succeeded:  BUG.\n", !IO)
    else
        io.write_string("worst case nondet test 1 failed:     OK.\n", !IO)
    ),
    ( if worst(180) then
        io.write_string("worst case nondet test 2 succeeded:  OK.\n", !IO)
    else
        io.write_string("worst case nondet test 2 failed:     BUG.\n", !IO)
    ),
    ( if worst(190) then
        io.write_string("worst case nondet test 3 succeeded:  OK.\n", !IO)
    else
        io.write_string("worst case nondet test 3 failed:     BUG.\n", !IO)
    ),
    ( if worst(200) then
        io.write_string("worst case nondet test 4 succeeded:  OK.\n", !IO)
    else
        io.write_string("worst case nondet test 4 failed:     BUG.\n", !IO)
    ).

:- pred best(int::in) is semidet.

best(A) :-
    test(A, _).

:- pred middle(int::in) is semidet.

middle(A0) :-
    (
        A1 = A0 + 10
    ;
        A1 = A0 + 20
    ;
        A1 = A0 + 30
    ),
    test(A1, _).

:- pred worst(int::in) is semidet.

worst(A0) :-
    addsome(A0, A1),
    test(A1, _).

:- pred addsome(int::in, int::out) is multi.

addsome(A0, A1) :-
    (
        A1 = A0 + 10
    ;
        A1 = A0 + 20
    ;
        A1 = A0 + 30
    ).

:- pred test(int::in, int::out) is nondet.

test(A, B) :-
    A > 200,
    (
        B = A
    ;
        B = A * 2
    ;
        B = A * 3
    ).
