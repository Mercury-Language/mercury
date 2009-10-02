% vim: ts=4 sw=4 et ft=mercury

:- module dense_lookup_switch_non.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module solutions.

:- type foo
    --->    a
    ;       b
    ;       c
    ;       d
    ;       e
    ;       f
    ;       g
    ;       h.

:- type test_id
    --->    test_p1(foo)
    ;       test_p2(foo)
    ;       test_p3(int)
    ;       test_p4(int).

:- type bar
    --->    f1
    ;       f2
    ;       f3(int)
    ;       f4(string)
    ;       f5(float).

main(!IO) :-
    test(test_p1(a), !IO),
    test(test_p1(b), !IO),
    test(test_p1(c), !IO),
    test(test_p1(d), !IO),
    test(test_p1(e), !IO),
    test(test_p1(f), !IO),
    test(test_p1(g), !IO),
    test(test_p1(h), !IO),

    test(test_p2(a), !IO),
    test(test_p2(b), !IO),
    test(test_p2(c), !IO),
    test(test_p2(d), !IO),
    test(test_p2(e), !IO),
    test(test_p2(f), !IO),
    test(test_p2(g), !IO),
    test(test_p2(h), !IO),

    test(test_p3(-1), !IO),
    test(test_p3(0), !IO),
    test(test_p3(1), !IO),
    test(test_p3(2), !IO),
    test(test_p3(3), !IO),
    test(test_p3(4), !IO),
    test(test_p3(5), !IO),
    test(test_p3(6), !IO),
    test(test_p3(7), !IO),

    test(test_p4(-1), !IO),
    test(test_p4(0), !IO),
    test(test_p4(1), !IO),
    test(test_p4(2), !IO),
    test(test_p4(3), !IO),
    test(test_p4(4), !IO),
    test(test_p4(5), !IO),
    test(test_p4(6), !IO),
    test(test_p4(7), !IO).

:- pred test(test_id::in, io::di, io::uo) is det.

test(FooOrInt, !IO) :-
    solutions(p_tuple(FooOrInt), Solns),
    (
        FooOrInt = test_p1(Foo),
        io.write_string("p1 ", !IO),
        io.write(Foo, !IO)
    ;
        FooOrInt = test_p2(Foo),
        io.write_string("p2 ", !IO),
        io.write(Foo, !IO)
    ;
        FooOrInt = test_p3(Int),
        io.write_string("p3 ", !IO),
        io.write_int(Int, !IO)
    ;
        FooOrInt = test_p4(Int),
        io.write_string("p4 ", !IO),
        io.write_int(Int, !IO)
    ),
    io.write_string(" ->\n", !IO),
    io.write_list(Solns, "", write_tp, !IO),
    io.write_string("end\n\n", !IO).

:- type tp
    --->    tp(string, bar, float).

:- pred write_tp(tp::in, io::di, io::uo) is det.

write_tp(tp(Str, Bar, Float), !IO) :-
    io.write_string(Str, !IO),
    io.write_string(" ", !IO),
    io.write(Bar, !IO),
    io.write_string(" ", !IO),
    io.write_float(Float, !IO),
    io.nl(!IO).

:- pred p_tuple(test_id::in, tp::out) is nondet.

p_tuple(FooOrInt, Tuple) :-
    (
        FooOrInt = test_p1(Foo),
        p1(Foo, Str, Bar, Float)
    ;
        FooOrInt = test_p2(Foo),
        p2(Foo, Str, Bar, Float)
    ;
        FooOrInt = test_p3(Int),
        p3(Int, Str, Bar, Float)
    ;
        FooOrInt = test_p4(Int),
        p4(Int, Str, Bar, Float)
    ),
    Tuple = tp(Str, Bar, Float).

:- pred p1(foo::in, string::out, bar::out, float::out) is multi.
:- pragma no_inline(p1/4).

% This predicate needs neither range check nor bitvec check.
p1(a, "p1_one", f1, 1.1).
p1(b, "p1_two", f2, 2.2).
p1(c, "p1_three", f1, 3.3).
p1(d, "p1_four", f1, 4.4).
p1(e, "p1_five", f2, 5.5).
p1(e, "p1_five2", f3(5), 55.5).
p1(f, "p1_six", f4("hex"), 6.6).
p1(g, "p1_seven", f5(77.7), 7.7).
p1(g, "p1_seven2", f1, 777.7).
p1(g, "p1_seven3", f2, 7777.7).
p1(h, "p1_eight", f1, 8.0).

:- pred p2(foo::in, string::out, bar::out, float::out) is nondet.
:- pragma no_inline(p2/4).

% This predicate needs a bitvec check but not a range check.
p2(a, "p2_one", f1, 1.1).
p2(c, "p2_three", f1, 3.3).
p2(d, "p2_four", f1, 4.4).
p2(e, "p2_five", f2, 5.5).
p2(e, "p2_five2", f3(5), 55.5).
p2(f, "p2_six", f4("hex"), 6.6).
p2(g, "p2_seven", f5(77.7), 7.7).
p2(g, "p2_seven2", f1, 777.7).
p2(g, "p2_seven3", f2, 7777.7).

:- pred p3(int::in, string::out, bar::out, float::out) is nondet.
:- pragma no_inline(p3/4).

% This predicate needs a range check but not a bitvec check.
p3(2, "p3_two_a", f2, 6.6).
p3(2, "p3_two_b", f2, 8.8).
p3(3, "p3_three", f3(5), 66.6).
p3(4, "p3_four_a", f5(78.7), 7.8).
p3(4, "p3_four_b", f1, 999.7).
p3(4, "p3_four_c", f2, 9999.7).
p3(5, "p3_five", f5(88.8), 9999.9).

:- pred p4(int::in, string::out, bar::out, float::out) is nondet.
:- pragma no_inline(p4/4).

% This predicate needs both range check and bitvec check.
p4(2, "p4_two_a", f2, 6.6).
p4(2, "p4_two_b", f2, 8.8).
p4(4, "p4_four_a", f5(78.7), 7.8).
p4(4, "p4_four_b", f1, 999.7).
p4(4, "p4_four_c", f2, 9999.7).
p4(5, "p4_five", f5(88.8), 9999.9).
p4(6, "p4_six", f5(66.0), 666.6).
