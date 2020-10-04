% vim: ts=4 sw=4 et ft=mercury
%
% The test predicate of this test case contains a conditionally enabled
% trace goal that throws an exception. Both the main and the unique mode
% checkers normally discard any goals after any erroneous goals, but if
% the erroneous goal is in a trace scope that may be deleted, they should
% not do so.

:- module conditional_trace_scope.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.
:- import_module require.
:- import_module string.

main(!IO) :-
    test(42, Y),
    io.format("X = %d\n", [i(Y)], !IO).

:- pred test(int::in, int::out) is det.

test(X, Y) :-
    ( if X < 10 then
        X = Y
    else
        trace [compiletime(flag("flag_is_not_set"))] (
            error("error_is_not_thrown")
        ),
        Y = X + 1
    ).
