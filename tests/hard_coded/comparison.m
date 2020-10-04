%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is a test to check the correctness of the way we handle specialized
% comparison predicates for types with three or fewer function symbols.

:- module comparison.

:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module exception.
:- import_module list.
:- import_module std_util.

:- type t1
    --->    a1(int, int).

:- type t2
    --->    a2(int, int)
    ;       b2(int).

:- type t3
    --->    a3(int, int)
    ;       b3(int)
    ;       c3.

main(!IO) :-
    perform_comparison_test(a1(10, 20), a1(10, 21), !IO),
    perform_comparison_test(a1(10, 20), a1(10, 20), !IO),
    perform_comparison_test(a1(10, 20), a1( 9, 20), !IO),

    perform_comparison_test(a2(10, 20), a2(10, 19), !IO),
    perform_comparison_test(a2(10, 20), a2(10, 20), !IO),
    perform_comparison_test(a2(10, 20), a2(11, 20), !IO),
    perform_comparison_test(a2(10, 20), b2(10), !IO),

    perform_comparison_test(b2(30), a2(50, 40), !IO),
    perform_comparison_test(b2(30), b2(29), !IO),
    perform_comparison_test(b2(30), b2(30), !IO),
    perform_comparison_test(b2(30), b2(31), !IO),

    perform_comparison_test(a3(10, 20), a3(10, 19), !IO),
    perform_comparison_test(a3(10, 20), a3(10, 20), !IO),
    perform_comparison_test(a3(10, 20), a3(11, 20), !IO),
    perform_comparison_test(a3(10, 20), b3(10), !IO),
    perform_comparison_test(a3(10, 20), c3, !IO),

    perform_comparison_test(b3(30), a3(50, 40), !IO),
    perform_comparison_test(b3(30), b3(29), !IO),
    perform_comparison_test(b3(30), b3(30), !IO),
    perform_comparison_test(b3(30), b3(31), !IO),
    perform_comparison_test(b3(30), c3, !IO),

    perform_comparison_test(c3, a3(50, 40), !IO),
    perform_comparison_test(c3, b3(50), !IO),
    perform_comparison_test(c3, c3, !IO).

:- pred perform_comparison_test(T::in, T::in, io::di, io::uo) is det.

perform_comparison_test(X, Y, !IO) :-
    compare(R, X, Y),
    io.write(X, !IO),
    (
        R = (<),
        io.write_string(" < ", !IO)
    ;
        R = (=),
        io.write_string(" = ", !IO)
    ;
        R = (>),
        io.write_string(" > ", !IO)
    ),
    io.write_line(Y, !IO).
