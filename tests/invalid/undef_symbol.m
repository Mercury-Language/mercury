%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module undef_symbol.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    p(!IO),
    q(!IO),
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
