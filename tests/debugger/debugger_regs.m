%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This program tests whether the tracer works for procedures with
% lots of arguments (beyond NUM_REAL_REGS and MAX_REAL_REGS).
% At the moment, MAX_REAL_REGS is 32, so a procedure with 41 args
% is a full test.

:- module debugger_regs.

:- interface.

:- import_module io.

:- pred main(io, io).
:- mode main(di, uo) is det.

:- implementation.

:- import_module list.
:- import_module int.

main(!IO) :-
    % The purpose of list is to force the tracer to call the Mercury
    % code to print a list of integers, when the input script asks
    % for the outputs of data to be printed. In the past this was
    % sufficient to cause part of the C stack to be overwritten.
    % It also tests whether the values of A0 etc that the tracer prints
    % are derived from the register contents produced by data,
    % or from the register contents left there by the code that
    % prints _List.
    data(_List,
        A0, A1, A2, A3, A4, A5, A6, A7, A8, A9,
        B0, B1, B2, B3, B4, B5, B6, B7, B8, B9,
        C0, C1, C2, C3, C4, C5, C6, C7, C8, C9,
        D0, D1, D2, D3, D4, D5, D6, D7, D8, D9),
    io.write_string(A0, !IO),
    io.write_string(A1, !IO),
    io.write_string(A2, !IO),
    io.write_string(A3, !IO),
    io.write_string(A4, !IO),
    io.write_string(A5, !IO),
    io.write_string(A6, !IO),
    io.write_string(A7, !IO),
    io.write_string(A8, !IO),
    io.write_string(A9, !IO),
    io.write_string("\n", !IO),
    io.write_string(B0, !IO),
    io.write_string(B1, !IO),
    io.write_string(B2, !IO),
    io.write_string(B3, !IO),
    io.write_string(B4, !IO),
    io.write_string(B5, !IO),
    io.write_string(B6, !IO),
    io.write_string(B7, !IO),
    io.write_string(B8, !IO),
    io.write_string(B9, !IO),
    io.write_string("\n", !IO),
    io.write_string(C0, !IO),
    io.write_string(C1, !IO),
    io.write_string(C2, !IO),
    io.write_string(C3, !IO),
    io.write_string(C4, !IO),
    io.write_string(C5, !IO),
    io.write_string(C6, !IO),
    io.write_string(C7, !IO),
    io.write_string(C8, !IO),
    io.write_string(C9, !IO),
    io.write_string("\n", !IO),
    io.write_string(D0, !IO),
    io.write_string(D1, !IO),
    io.write_string(D2, !IO),
    io.write_string(D3, !IO),
    io.write_string(D4, !IO),
    io.write_string(D5, !IO),
    io.write_string(D6, !IO),
    io.write_string(D7, !IO),
    io.write_string(D8, !IO),
    io.write_string(D9, !IO),
    io.write_string("\n", !IO).

:- pred data(list(int)::out,
    string::out, string::out, string::out, string::out, string::out,
    string::out, string::out, string::out, string::out, string::out,
    string::out, string::out, string::out, string::out, string::out,
    string::out, string::out, string::out, string::out, string::out,
    string::out, string::out, string::out, string::out, string::out,
    string::out, string::out, string::out, string::out, string::out,
    string::out, string::out, string::out, string::out, string::out,
    string::out, string::out, string::out, string::out, string::out) is det.

data([1, 2, 3, 4, 5],
    "a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9",
    "b0", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9",
    "c0", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8", "c9",
    "d0", "d1", "d2", "d3", "d4", "d5", "d6", "d7", "d8", "d9").
