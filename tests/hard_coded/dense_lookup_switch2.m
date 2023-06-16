%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module dense_lookup_switch2.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- type foo
    --->    a
    ;       b
    ;       c
    ;       d
    ;       e
    ;       f
    ;       g
    ;       h.

:- type bar
    --->    f1
    ;       f2
    ;       f3(int)
    ;       f4(string)
    ;       f5(float).

main(!IO) :-
    test(a, !IO),
    test(b, !IO),
    test(c, !IO),
    test(d, !IO),
    test(e, !IO),
    test(f, !IO),
    test(g, !IO),
    test(h, !IO).

:- pred test(foo::in, io::di, io::uo) is det.

test(Foo, !IO) :-
    ( if p(Foo, Str, Bar, Float) then
        io.write(Foo, !IO),
        io.write_string(" -> ", !IO),
        io.write_string(Str, !IO),
        io.write_string(" ", !IO),
        io.write(Bar, !IO),
        io.write_string(" ", !IO),
        io.write_float(Float, !IO),
        io.nl(!IO)
    else
        io.write(Foo, !IO),
        io.write_string(" -> failed", !IO),
        io.nl(!IO)
    ).

:- pred p(foo::in, string::out, bar::out, float::out) is semidet.
:- pragma no_inline(p/4).

p(d, "four", f1, 4.4).
p(e, "five", f2, 5.5).
p(f, "six", f4("hex"), 6.6).
p(g, "seven", f5(77.7), 7.7).
