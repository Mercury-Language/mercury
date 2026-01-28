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
    s(42, 43, 44, _, !IO),
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

:- pred s(int::in, int::in, int::in, int::out, io::di, io::uo) is det.

s(A, PredId, !PredInfo, !IO) :-
    ( if A = 2 then
        Bcde = 3
    else
        Bcde = 4
    ),
    Fghi = Bcde + 99,
    io.write_int(BcDe, !IO),
    io.write_int(Fghi, !IO),
    io.write_int(PredId, !IO),
    !:PredInfo = !.PredInfo + 1,
    % We used to suggest PredId here, which is not all that useful,
    % given that e.g. "!.PredInfo" is closer to "PredInfo" than is "PredId".
    io.write_int(PredInfo, !IO).
