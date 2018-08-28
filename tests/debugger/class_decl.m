%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module class_decl.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

%---------------------------------------------------------------------------%

:- typeclass foo(T) where [
    pred foo_method(T::in, io::di, io::uo) is det
].

:- typeclass bar(T, U) where [
    pred bar_method(T::in, U::in, io::di, io::uo) is det
].

:- type t1
    --->    t1(int).

:- type t2(T)
    --->    t2a(T)
    ;       t2b(T, T).

:- instance foo(string) where [pred(foo_method/3) is foo_string].
:- instance foo(t1) where [pred(foo_method/3) is foo_t1].
:- instance foo(t2(U)) where [pred(foo_method/3) is foo_t2].

:- instance bar(int, int) where [pred(bar_method/4) is bar_int_int].
:- instance bar(string, int) where [pred(bar_method/4) is bar_str_int].
:- instance bar(string, string) where [pred(bar_method/4) is bar_str_str].
:- instance bar(t1, int) where [pred(bar_method/4) is bar_t1_int].
:- instance bar(t1, t2(U)) where [pred(bar_method/4) is bar_t1_t2].

%---------------------------------------------------------------------------%

main(!IO) :-
    foo_method("zero", !IO),
    foo_method(t1(10), !IO),
    foo_method(t2a(20), !IO),
    foo_method(t2b(30, 40), !IO),
    foo_method(t2b("thirty", "forty"), !IO),
    bar_method(11, 22, !IO),
    bar_method("eleven", 22, !IO),
    bar_method("eleven", "twentytwo", !IO),
    bar_method(t1(111), 222, !IO),
    bar_method(t1(333), t2a(444), !IO),
    bar_method(t1(333), t2b(444, 555), !IO),
    bar_method(t1(888), t2b("sixsixsix", "sevensevenseven"), !IO).

:- pred foo_string(string::in, io::di, io::uo) is det.

foo_string(S, !IO) :-
    io.write_string("string: ", !IO),
    io.write_string(S, !IO),
    io.nl(!IO).

:- pred foo_t1(t1::in, io::di, io::uo) is det.

foo_t1(t1(I), !IO) :-
    io.write_string("t1: ", !IO),
    io.write_int(I, !IO),
    io.nl(!IO).

:- pred foo_t2(t2(V)::in, io::di, io::uo) is det.

foo_t2(t2a(I), !IO) :-
    io.write_string("t2a: ", !IO),
    io.write(I, !IO),
    io.nl(!IO).
foo_t2(t2b(I1, I2), !IO) :-
    io.write_string("t2b: ", !IO),
    io.write(I1, !IO),
    io.write_string(", ", !IO),
    io.write(I2, !IO),
    io.nl(!IO).

:- pred bar_int_int(int::in, int::in, io::di, io::uo) is det.

bar_int_int(I1, I2, !IO) :-
    io.write_string("ii: ", !IO),
    io.write_int(I1, !IO),
    io.write_string(", ", !IO),
    io.write_int(I2, !IO),
    io.nl(!IO).

:- pred bar_str_int(string::in, int::in, io::di, io::uo) is det.

bar_str_int(S1, I2, !IO) :-
    io.write_string("si: ", !IO),
    io.write_string(S1, !IO),
    io.write_string(", ", !IO),
    io.write_int(I2, !IO),
    io.nl(!IO).

:- pred bar_str_str(string::in, string::in, io::di, io::uo) is det.

bar_str_str(S1, S2, !IO) :-
    io.write_string("ss: ", !IO),
    io.write_string(S1, !IO),
    io.write_string(", ", !IO),
    io.write_string(S2, !IO),
    io.nl(!IO).

:- pred bar_t1_int(t1::in, int::in, io::di, io::uo) is det.

bar_t1_int(t1(I1), I2, !IO) :-
    io.write_string("t1int: ", !IO),
    io.write_int(I1, !IO),
    io.write_string(", ", !IO),
    io.write_int(I2, !IO),
    io.nl(!IO).

:- pred bar_t1_t2(t1::in, t2(V)::in, io::di, io::uo) is det.

bar_t1_t2(t1(I1), t2a(I2), !IO) :-
    io.write_string("t1t2a: ", !IO),
    io.write_int(I1, !IO),
    io.write_string(", ", !IO),
    io.write(I2, !IO),
    io.nl(!IO).
bar_t1_t2(t1(I1), t2b(I2, I3), !IO) :-
    io.write_string("t1t2b: ", !IO),
    io.write_int(I1, !IO),
    io.write_string(", ", !IO),
    io.write(I2, !IO),
    io.write_string(", ", !IO),
    io.write(I3, !IO),
    io.nl(!IO).
