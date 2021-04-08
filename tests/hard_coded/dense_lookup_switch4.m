%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
:- module dense_lookup_switch4.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module char.
:- import_module list.

:- type foo
    --->    a ; b ; c ; d ; e ; f ; g ; h ; i ; j.

:- type bar =< foo
    --->    g ; c ; e ; f ; b ; d ; h.
            % One contiguous range, deliberately misordered.

main(!IO) :-
    Xs = [b, c, d, e, f, g, h],
    list.foldl(test, Xs, !IO).

:- pred test(bar::in, io::di, io::uo) is det.

test(Bar, !IO) :-
    io.write(Bar, !IO),
    io.write_string(": ", !IO),
    p1(Bar, Int),
    io.write_int(Int, !IO),
    io.write_string(" ", !IO),
    ( if p2(Bar, Char) then
        io.write_char(Char, !IO)
    else
        io.write_string("-", !IO)
    ),
    io.nl(!IO).

:- pred p1(bar::in, int::out) is det.

p1(b, 2).
p1(c, 3).
p1(d, 4).
p1(e, 5).
p1(f, 6).
p1(g, 7).
p1(h, 8).

:- pred p2(bar::in, char::out) is semidet.

% p2(b, 'b').
p2(c, 'c').
p2(d, 'd').
% p2(e, 'e').
p2(f, 'f').
p2(g, 'g').
% p2(h, 'h').
