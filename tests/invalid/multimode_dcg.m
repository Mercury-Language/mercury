%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module multimode_dcg.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    In = 42,
    test0(!IO),
    test1(In, !IO),
    test1(_Out0, !IO),
    test2(In, In, !IO),
    test2(In, _Out1, !IO),
    test2(_Out2, In, !IO),
    test2(_Out3, _Out4, !IO).

:- pred test0(io, io).
:- mode test0(di, uo) is det.
test0 -->
    puts("test0").

:- pred test1(int, io, io).
:- mode test1(in, di, uo) is det.
:- mode test1(out, di, uo) is det.
test1(_::in) -->
    puts("test1(in)").
test1(0::out) -->
    puts("test1(out)").

:- pred test2(int, int, io, io).
:- mode test2(in, in, di, uo) is det.
:- mode test2(in, out, di, uo) is det.
:- mode test2(out, in, di, uo) is det.
:- mode test2(out, out, di, uo) is det.
test2(_::in, _::in) -->
    puts("test2(in, in)").
test2(_::in, 0::out) -->
    puts("test2(in, out)").
test2(0::out, _::in) -->
    puts("test2(out, in)").
test2(0::out, 0::out) -->
    puts("test2(out, out)").

:- pred puts(string::in, io::di, io::uo) is det.
puts(S) -->
    io.write_string(S), nl.
