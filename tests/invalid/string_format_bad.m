% vim: ts=4 sw=4 expandtab ft=mercury

:- module string_format_bad.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module bool.
:- import_module list.
:- import_module stream.
:- import_module stream.string_writer.
:- import_module string.

main(!IO) :-
    S1 = string.format("", [s("x1")]),
    io.write_string(S1, !IO),
    S2 = string.format("%d", [s("x2")]),
    io.write_string(S2, !IO),
    io.stdout_stream(OutputStream, !IO),
    io.format("%d", [s("x3")], !IO),
    io.format(OutputStream, "%d", [s("x4")], !IO),
    stream.string_writer.format(OutputStream, "%d", [s("x4")], !IO),
    io.format("%w", [i(5)], !IO),
    io.write_string(p(s("five")), !IO),
    F6 = "%s %f",
    make_bool(6, T6),
    (
        T6 = yes,
        V6A = i(6)
    ->
        V6 = [s("six"), V6A],
        io.format(OutputStream, F6, V6, !IO),
        make_bool(7, T7),
        F7 = "%d %s %d",
        (
            T7 = yes,
            io.format(OutputStream, F7, [f(7.0) | V6], !IO)
        ;
            T7 = no
        )
    ;
        true
    ).

:- pred make_bool(int::in, bool::out) is det.

make_bool(_, yes).

:- func t(string) = string.

t(S) = S.

:- func p(string.poly_type) = string.

p(s(S)) = t(string.format("%s", [s(S)])).
p(c(C)) = t(string.format("%c", [c(C)])).
p(i(I)) = t(string.format("%d", [i(I)])).
p(f(F)) = t(string.format("%f", [f(F)])).
p(u(U)) = t(string.format("%u", [u(U)])).
