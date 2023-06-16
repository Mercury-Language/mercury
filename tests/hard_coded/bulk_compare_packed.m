%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This test case tests the bulk comparison of sub-word-sized
% unsigned arguments, both when sub-word-sized signed arguments are absent,
% and when they are present, and the bulk unification of sub-word-sized
% arguments in general.
%
%---------------------------------------------------------------------------%

:- module bulk_compare_packed.

:- interface.

:- import_module io.

:- pred main(io ::di, io::uo) is det.

:- implementation.

:- import_module list.
:- import_module require.

:- type enum2
    --->    e00
    ;       e01
    ;       e10
    ;       e11.

% In the tx type, the arguments of the b and c function symbols
% should all be locally packed, i.e. we should not need a memory cell.
:- type tx
    --->    tx_a
    ;       tx_b(xb1 :: enum2, xb2:: enum2, xb3:: enum2)
    ;       tx_c(xc1 :: enum2, xc2:: enum2, xc3:: int8, xc4:: enum2).

% In the ty type, the arguments of the b and c function symbols
% should all be remotely packed into the second word of a memory cell.
:- type ty
    --->    ty_a
    ;       ty_b(yb1 :: int, yb2:: enum2, yb3:: enum2, yb4:: enum2)
    ;       ty_c(yc1 :: int, yc2:: enum2, yc3:: enum2, yc4:: int8, yc5:: enum2).

main(!IO) :-
    % Both these lists of terms are sorted. Therefore each block of output,
    % which compares a given element of the list with all the elements of
    % the list, should consist of
    %
    % - zero or more > results, for the earlier list elements
    % - one = result, for when the element is compared with itself, and
    % - zero or more < results, for the later list elements.
    %
    % And the position of the = result should move one position back
    % in both sequences of result blocks (one sequence for TermsX,
    % and one for TermsY).

    TermsX = [
        tx_a,
        tx_b(e00, e11, e10),
        tx_b(e10, e11, e01),
        tx_c(e00, e11, -44i8, e10),
        tx_c(e00, e11, 44i8, e10),
        tx_c(e10, e11, -55i8, e01),
        tx_c(e10, e11, 55i8, e01)
    ],
    TermsY = [
        ty_a,
        ty_b(42, e00, e11, e10),
        ty_b(42, e10, e11, e01),
        ty_c(42, e00, e11, -44i8, e10),
        ty_c(42, e00, e11, 44i8, e10),
        ty_c(42, e10, e11, -55i8, e01),
        ty_c(42, e10, e11, 55i8, e01)
    ],
    compare_all_all(TermsX, TermsX, !IO),
    compare_all_all(TermsY, TermsY, !IO).

:- pred compare_all_all(list(T)::in, list(T)::in, io::di, io::uo) is det.

compare_all_all([], _Bs, !IO).
compare_all_all([A | As], Bs, !IO) :-
    io.nl(!IO),
    compare_one_all(A, Bs, !IO),
    compare_all_all(As, Bs, !IO).

:- pred compare_one_all(T::in, list(T)::in, io::di, io::uo) is det.

compare_one_all(_A, [], !IO).
compare_one_all(A, [B | Bs], !IO) :-
    compare_one_one(A, B, !IO),
    compare_one_all(A, Bs, !IO).

:- pred compare_one_one(T::in, T::in, io::di, io::uo) is det.

compare_one_one(A, B, !IO) :-
    compare(R, A, B),
    io.write(A, !IO),
    (
        R = (<),
        io.write_string(" < ", !IO),
        expect_not(unify(A, B), $pred, "compare lt but unify")
    ;
        R = (=),
        io.write_string(" = ", !IO),
        expect(unify(A, B), $pred, "compare eq but not unify")
    ;
        R = (>),
        io.write_string(" > ", !IO),
        expect_not(unify(A, B), $pred, "compare gt but unify")
    ),
    io.write_line(B, !IO).
