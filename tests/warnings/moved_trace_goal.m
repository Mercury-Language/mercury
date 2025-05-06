%------------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%------------------------------------------------------------------------------%
%
% This test case tests both the occurs check warning and its suppression.
%

:- module moved_trace_goal.
:- interface.

:- pred test(int::in, int::in, int::out) is det.

:- implementation.

:- import_module int.
:- import_module io.
:- import_module list.
:- import_module set.
:- import_module string.

test(A, B, Z) :-
    trace [io(!IO)] (
        dump_abc(A, B, C, !IO)
    ),

    ( if A > 10 then
        C = A + B
    else
        C = 2 * A
    ),

    ( if C mod 5 = 2 then
        trace [io(!IO)] (
            dump_abcz(A, B, C, Z, !IO)
        ),

        Z = A + B
    else
        trace [io(!IO)] (
            dump_abcz(A, B, C, Z, !IO)
        ),

        Z = C * C
    ).

%------------------------------------------------------------------------------%
%
% These predicate exist because it is easier to read the HLDS dump
% of the test predicate if the trace goals contain just a calls to them
% than if they contain the flattened implementation of io.format.
%

:- pred dump_abc(int::in, int::in, int::in, io::di, io::uo) is det.

dump_abc(A, B, C, !IO) :-
    io.format("A = %d, B = %d, C = %d\n",
        [i(A), i(B), i(C)], !IO).

:- pred dump_abcz(int::in, int::in, int::in, int::in, io::di, io::uo) is det.

dump_abcz(A, B, C, Z, !IO) :-
    io.format("A = %d, B = %d, C = %d, Z = %d\n",
        [i(A), i(B), i(C), i(Z)], !IO).
