%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This test case checks the debugger's handling of unify, compare and index
% predicates. Versions of the runtime system before 29 Mar 2003 used to have
% a bug in computing their arities.

:- module uci.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.
:- import_module string.

main(!IO) :-
    test([], RevResults),
    list.reverse(RevResults, Results),
    string.append_list(Results, ResultString),
    io.write_string(ResultString, !IO).

:- pred test(list(string)::in, list(string)::out) is det.

test(!Res) :-
    ( if compare((<), ma0, mb0) then
        add_res("0 lt\n", !Res)
    else
        add_res("0 ge\n", !Res)
    ),
    ( if compare((<), mb1, ma1) then
        add_res("1 lt\n", !Res)
    else
        add_res("1 ge\n", !Res)
    ),
    ( if compare((<), ma2, ma2) then
        add_res("2 lt\n", !Res)
    else
        add_res("2 ge\n", !Res)
    ),
    ( if compare((<), mb3, ma3) then
        add_res("3 lt\n", !Res)
    else
        add_res("3 ge\n", !Res)
    ),
    ( if compare((<), ma4, mb4) then
        add_res("4 lt\n", !Res)
    else
        add_res("4 ge\n", !Res)
    ),
    ( if unify(ma0, mb0) then
        add_res("0 eq\n", !Res)
    else
        add_res("0 ne\n", !Res)
    ),
    ( if unify(ma1, ma1) then
        add_res("1 eq\n", !Res)
    else
        add_res("1 ne\n", !Res)
    ),
    ( if unify(ma2, mb2) then
        add_res("2 eq\n", !Res)
    else
        add_res("2 ne\n", !Res)
    ),
    ( if unify(mb3, mb3) then
        add_res("3 eq\n", !Res)
    else
        add_res("3 ne\n", !Res)
    ),
    ( if unify(ma4, mb4) then
        add_res("4 eq\n", !Res)
    else
        add_res("4 ne\n", !Res)
    ),
    ( if compare((<), mai, mbi) then
        add_res("i lt\n", !Res)
    else
        add_res("i ge\n", !Res)
    ).

:- pred add_res(string::in, list(string)::in, list(string)::out) is det.

add_res(R, Rs0, [R | Rs0]).

:- type t0      --->    a0 ; b0.
:- type t1(A)       --->    a1(A) ; b1(A).
:- type t2(A, B)    --->    a2(A, B) ; b2(A, B).
:- type t3(A, B, C) --->    a3(A, B, C) ; b3(A, B, C).
:- type t4(A, B, C, D)  --->    a4(A, B, C, D) ; b4(A, B, C, D).

:- type i(A, B, C)  --->    ai(A) ; bi(B) ; ci(C).

:- func ma0 = t0.
:- func mb0 = t0.
:- func ma1 = t1(int).
:- func mb1 = t1(int).
:- func ma2 = t2(int, int).
:- func mb2 = t2(int, int).
:- func ma3 = t3(int, int, int).
:- func mb3 = t3(int, int, int).
:- func ma4 = t4(int, int, int, int).
:- func mb4 = t4(int, int, int, int).

:- func mai = i(int, int, int).
:- func mbi = i(int, int, int).
:- func mci = i(int, int, int).

ma0 = a0.
mb0 = b0.
ma1 = a1(1).
mb1 = b1(11).
ma2 = a2(1, 2).
mb2 = b2(11, 12).
ma3 = a3(1, 2, 3).
mb3 = b3(11, 12, 13).
ma4 = a4(1, 2, 3, 4).
mb4 = b4(11, 12, 13, 14).

mai = ai(1).
mbi = bi(11).
mci = ci(111).
