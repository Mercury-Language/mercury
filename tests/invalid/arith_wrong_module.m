%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module arith_wrong_module.

:- interface.

:- import_module io.

:- pred test_func(uint32::in, uint32::in, uint32::out) is det.
:- pred test_pred(int16::in, uint16::in, int16::out) is det.
:- pred game_loop(int64::in, int64::in, io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.
:- import_module string.

test_func(A, B, C) :-
    C = A + B.

test_pred(A, B, C) :-
    pow(A, B, C).

game_loop(Start, End, !IO) :-
    Diff = End - Start,
    io.stdout_stream(StdOut, !IO),
    io.format(StdOut, "start: %di, end: %i, diff: %i\n",
        [i64(Start), i64(End), i64(Diff)], !IO).
