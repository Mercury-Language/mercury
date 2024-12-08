%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module undef_symbol.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.

main(!IO) :-
    p(!IO),
    q(!IO),
    s(42, !IO),
    undef_symbol.r(!IO).

:- pred p(io::di, io::uo) is det.

p(!IO) :-
    string.append("hello ", "world.\n", Str),
    io.write_string(Str, !IO).

:- pred q(io::di, io::uo) is det.

q(!IO) :-
    Context = term.context("random", 17),
    io.write_line(Context, !IO).

% :- pred r(io::di, io::uo) is det.

:- pred s(int::in, io::di, io::uo) is det.

s(A, !IO) :-
    ( if A = 2 then
        Bcde = 3
    else
        Bcde = 4
    ),
    Fghi = Bcde + 99,
    io.write_int(BcDe, !IO),
    io.write_int(Fghi, !IO).
