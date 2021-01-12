%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
%
% This test exhibits many instances of the problem demonstrated by gh72[ab].
% The instances are designed to stress-test the compilation transformation
% that implements the fix (compiler/direct_arg_in_out.m).
%

:- module gh72.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.
:- import_module string.
:- import_module solutions.

%-----------------------------------------------------------------------------%

main(!IO) :-
    conj_tests(!IO),

    disj_test_init(!IO), 

    switch_test_init(10, !IO), 
    switch_test_init(11, !IO), 
    switch_test_init(12, !IO), 
    switch_test_init(13, !IO), 

    ite_test_init(10, !IO), 
    ite_test_init(11, !IO), 
    ite_test_init(12, !IO), 
    ite_test_init(13, !IO), 

    higher_order_tests("abc", "xyz", "42", !IO),

    method_tests("string", "hij", !IO),
    method_tests("int", 53, !IO).

%-----------------------------------------------------------------------------%

:- pred conj_tests(io::di, io::uo) is det.

conj_tests(!IO) :-
    A = f1(_),
    conj_test(A, !IO),
    B = f2(_),
    conj_test(B, !IO),
    C = f3(_),
    conj_test(C, !IO),
    D = f4(package("start4", "d")),
    conj_test(D, !IO).

:- pred conj_test(t, io, io).
:- mode conj_test(t4 >> ground, di, uo) is det.

conj_test(T, !IO) :-
    fill3("c", T),
    fill2("b", T),
    fill1("a", T),
    io.write_string(dump_t_nl(T), !IO).

%-----------------------------------------------------------------------------%

:- pred disj_test_init(io::di, io::uo) is det.

disj_test_init(!IO) :-
    io.format("\ndisj_test_init(f1)\n", [], !IO),
    solutions(disj_init_f1, SolnStrSet1),
    list.foldl(io.write_string, SolnStrSet1, !IO),
    io.format("disj_test_init(f2)\n", [], !IO),
    solutions(disj_init_f2, SolnStrSet2),
    list.foldl(io.write_string, SolnStrSet2, !IO),
    io.format("disj_test_init(f3)\n", [], !IO),
    solutions(disj_init_f3, SolnStrSet3),
    list.foldl(io.write_string, SolnStrSet3, !IO),
    io.write_string("end\n", !IO).

:- pred disj_init_f1(string).
:- mode disj_init_f1(out) is nondet.
:- pragma no_inline(disj_init_f1/1).

disj_init_f1(Str) :-
    disj_init(N, f1(_), Str0),
    Str = string.int_to_string(N) ++ ": " ++ Str0.

:- pred disj_init_f2(string).
:- mode disj_init_f2(out) is nondet.
:- pragma no_inline(disj_init_f2/1).

disj_init_f2(Str) :-
    disj_init(N, f2(_), Str0),
    Str = string.int_to_string(N) ++ ": " ++ Str0.

:- pred disj_init_f3(string).
:- mode disj_init_f3(out) is nondet.
:- pragma no_inline(disj_init_f3/1).

disj_init_f3(Str) :-
    disj_init(N, f3(_), Str0),
    Str = string.int_to_string(N) ++ ": " ++ Str0.

:- pred disj_init(int, t, string).
:- mode disj_init(out, t4 >> ground, out) is nondet.
:- pragma no_inline(disj_init/3).
:- pragma no_determinism_warning(disj_init/3).

disj_init(N, T, Str) :-
    (
        N = 10,
        NStr = string.int_to_string(N),
        fill1("1" ++ NStr, T),
        fill2("2" ++ NStr, T),
        fill3("3" ++ NStr, T),
        (
            StrA = NStr ++ ", " ++ dump_t(T)
        ;
            StrA = NStr ++ "; " ++ dump_t(T)
        )
    ;
        N = 11,
        NStr = string.int_to_string(N),
        fill1("a" ++ NStr, T),
        fill23("b" ++ NStr, T),
        StrA = NStr ++ ", " ++ dump_t(T)
    ;
        N = 12,
        NStr = string.int_to_string(N),
        fill23(NStr, T),
        fill1(NStr, T),
        StrA = NStr ++ ", no_t"
    ),
    % Test that the most up-to-date version of T reaches the code
    % after the disjunction.
    StrB = dump_t_nl(T),
    Str = StrA ++ " | " ++ StrB.

%-----------------------------------------------------------------------------%

:- pred switch_test_init(int::in, io::di, io::uo) is det.

switch_test_init(N, !IO) :-
    io.format("\nswitch_test_init(%d, f1)\n", [i(N)], !IO),
    solutions(switch_init_f1(N), SolnStrSet1),
    list.foldl(io.write_string, SolnStrSet1, !IO),
    io.format("switch_test_init(%d, f2)\n", [i(N)], !IO),
    solutions(switch_init_f2(N), SolnStrSet2),
    list.foldl(io.write_string, SolnStrSet2, !IO),
    io.format("switch_test_init(%d, f3)\n", [i(N)], !IO),
    solutions(switch_init_f3(N), SolnStrSet3),
    list.foldl(io.write_string, SolnStrSet3, !IO),
    io.write_string("end\n", !IO).

:- pred switch_init_f1(int, string).
:- mode switch_init_f1(in, out) is nondet.
:- pragma no_inline(switch_init_f1/2).

switch_init_f1(N, Str) :-
    switch_init(N, f1(_), Str).

:- pred switch_init_f2(int, string).
:- mode switch_init_f2(in, out) is nondet.
:- pragma no_inline(switch_init_f2/2).

switch_init_f2(N, Str) :-
    switch_init(N, f2(_), Str).

:- pred switch_init_f3(int, string).
:- mode switch_init_f3(in, out) is nondet.
:- pragma no_inline(switch_init_f3/2).

switch_init_f3(N, Str) :-
    switch_init(N, f3(_), Str).

:- pred switch_init(int, t, string).
:- mode switch_init(in, t4 >> ground, out) is nondet.
:- pragma no_inline(switch_init/3).

switch_init(N, T, StrA ++ " | " ++ StrB) :-
    NStr = string.int_to_string(N),
    (
        N = 10,
        fill1("1" ++ NStr, T),
        fill2("2" ++ NStr, T),
        fill3("3" ++ NStr, T),
        (
            StrA = NStr ++ ", " ++ dump_t(T)
        ;
            StrA = NStr ++ "; " ++ dump_t(T)
        )
    ;
        N = 11,
        fill1("a" ++ NStr, T),
        fill23("b" ++ NStr, T),
        StrA = NStr ++ ", " ++ dump_t(T)
    ;
        N = 12,
        fill23(NStr, T),
        fill1(NStr, T),
        StrA = NStr ++ ", no_t"
    ),
    % Test that the most up-to-date version of T reaches the code
    % after the switch.
    StrB = dump_t_nl(T).

%-----------------------------------------------------------------------------%

:- pred ite_test_init(int::in, io::di, io::uo) is det.

ite_test_init(N, !IO) :-
    io.format("\nite_test_init(%d, f1)\n", [i(N)], !IO),
    solutions(ite_init_f1(N), SolnStrSet1),
    list.foldl(io.write_string, SolnStrSet1, !IO),
    io.format("ite_test_init(%d, f2)\n", [i(N)], !IO),
    solutions(ite_init_f2(N), SolnStrSet2),
    list.foldl(io.write_string, SolnStrSet2, !IO),
    io.format("ite_test_init(%d, f3)\n", [i(N)], !IO),
    solutions(ite_init_f3(N), SolnStrSet3),
    list.foldl(io.write_string, SolnStrSet3, !IO),
    io.write_string("end\n", !IO).

:- pred ite_init_f1(int, string).
:- mode ite_init_f1(in, out) is nondet.
:- pragma no_inline(ite_init_f1/2).

ite_init_f1(N, Str) :-
    ite_init(N, f1(_), Str).

:- pred ite_init_f2(int, string).
:- mode ite_init_f2(in, out) is nondet.
:- pragma no_inline(ite_init_f2/2).

ite_init_f2(N, Str) :-
    ite_init(N, f2(_), Str).

:- pred ite_init_f3(int, string).
:- mode ite_init_f3(in, out) is nondet.
:- pragma no_inline(ite_init_f3/2).

ite_init_f3(N, Str) :-
    ite_init(N, f3(_), Str).

:- pred ite_init(int, t, string).
:- mode ite_init(in, t4 >> ground, out) is nondet.
:- pragma no_inline(ite_init/3).

ite_init(N, T, StrA ++ " | " ++ StrB) :-
    NStr = string.int_to_string(N),
    % While direct_arg_in_out.m is prepared for situations in which
    % a daio variable such as T is instantiated in both the condition
    % and the then-part of an if-then-else, we can't test that here,
    % because if e.g. we move the call to fill1 into the condition,
    % mode analysis generates an error.
    ( if
        N = 10
    then
        fill1("1" ++ NStr, T),
        fill2("2" ++ NStr, T),
        fill3("3" ++ NStr, T),
        (
            StrA = NStr ++ ", " ++ dump_t(T)
        ;
            StrA = NStr ++ ", " ++ dump_t(T)
        )
    else if
        N = 11
    then
        fill1("a" ++ NStr, T),
        fill23("b" ++ NStr, T),
        StrA = NStr ++ ", " ++ dump_t(T)
    else if
        N = 12
    then
        fill23(NStr, T),
        fill1(NStr, T),
        StrA = NStr ++ ", no_t"
    else
        fail
    ),
    % Test that the most up-to-date version of T reaches the code
    % after the if-then-else.
    StrB = dump_t_nl(T).

%-----------------------------------------------------------------------------%

:- pred higher_order_tests(string::in, string::in, string::in,
    io::di, io::uo) is det.

higher_order_tests(StrA, StrB, StrC, !IO) :-
    ClosureA = fill1(StrA),
    ClosureB = fill1(StrB),
    ClosureC = fill1(StrC),

    io.format("\nhigher order test A %s\n", [s(StrA)], !IO),
    higher_order_test(ClosureA, f1(_), !IO),
    higher_order_test(ClosureA, f2(package("testA2", "testA2")), !IO),
    higher_order_test(ClosureA, f3(package("testA3", "testA3")), !IO),
    higher_order_test(ClosureA, f4(package("testA4", "testA4")), !IO),

    io.format("\nhigher order test B %s\n", [s(StrB)], !IO),
    higher_order_test(ClosureB, f1(_), !IO),
    higher_order_test(ClosureB, f2(package("testB2", "testB2")), !IO),
    higher_order_test(ClosureB, f3(package("testB3", "testB3")), !IO),
    higher_order_test(ClosureB, f4(package("testB4", "testB4")), !IO),

    io.format("\nhigher order test C %s\n", [s(StrC)], !IO),
    higher_order_test(ClosureC, f1(_), !IO),
    higher_order_test(ClosureC, f2(package("testC2", "testC2")), !IO),
    higher_order_test(ClosureC, f3(package("testC3", "testC3")), !IO),
    higher_order_test(ClosureC, f4(package("testC4", "testC4")), !IO).

:- pred higher_order_test((pred(t)), t, io, io).
:- mode higher_order_test((pred(t234 >> ground) is det), t234 >> ground,
    di, uo) is det.

higher_order_test(Closure, T, !IO) :-
    Closure(T),
    io.write_string(dump_t_nl(T), !IO).

%-----------------------------------------------------------------------------%

:- pred method_tests(string::in, S::in, io::di, io::uo) is det <= fxc(S).

method_tests(Msg, S, !IO) :-
    io.format("\nmethod test %s\n", [s(Msg)], !IO),
    fx(S, f1(_), "p1", S1),
    fx(S, f2(package("test2", "test2")), "p2", S2),
    fx(S, f3(package("test3", "test3")), "p3", S3),
    fx(S, f4(package("test4", "test4")), "p4", S4),
    io.write_string(S1, !IO),
    io.write_string(S2, !IO),
    io.write_string(S3, !IO),
    io.write_string(S4, !IO).

:- typeclass fxc(S) where [
    pred fx(S, t, string, string),
    mode fx(in, t234 >> ground, in, out) is det
].

:- instance fxc(string) where [
    pred(fx/4) is fx_string
].

:- instance fxc(int) where [
    pred(fx/4) is fx_int
].

:- pred fx_string(string, t, string, string).
:- mode fx_string(in, t234 >> ground, in, out) is det.

fx_string(S0, T, Prefix, Str) :-
    fill1(S0, T),
    Str = Prefix ++ " " ++ dump_t_nl(T).

:- pred fx_int(int, t, string, string).
:- mode fx_int(in, t234 >> ground, in, out) is det.

fx_int(I0, T, Prefix, Str) :-
    fill1(string.int_to_string(I0), T),
    Str = Prefix ++ " " ++ dump_t_nl(T).

%-----------------------------------------------------------------------------%

:- type t
    --->    f1(package)
    ;       f2(package)
    ;       f3(package)
    ;       f4(package).

:- inst t4 for t/0
    --->    f1(free)
    ;       f2(free)
    ;       f3(free)
    ;       f4(ground).

:- inst t34 for t/0
    --->    f1(free)
    ;       f2(free)
    ;       f3(ground)
    ;       f4(ground).

:- inst t234 for t/0
    --->    f1(free)
    ;       f2(ground)
    ;       f3(ground)
    ;       f4(ground).

:- type package
    --->    package(string, string).

%-----------------------------------------------------------------------------%

:- func dump_t_nl(t) = string.
:- pragma no_inline(dump_t_nl/1).

dump_t_nl(T) = dump_t(T) ++ "\n".

:- func dump_t(t) = string.
:- pragma no_inline(dump_t/1).

dump_t(f1(Package)) = "f1(" ++ dump_package(Package) ++ ")".
dump_t(f2(Package)) = "f2(" ++ dump_package(Package) ++ ")".
dump_t(f3(Package)) = "f3(" ++ dump_package(Package) ++ ")".
dump_t(f4(Package)) = "f4(" ++ dump_package(Package) ++ ")".

:- func dump_package(package) = string.
:- pragma no_inline(dump_t/1).

dump_package(package(A, B)) = "package(" ++ A ++ ", " ++ B ++ ")".

%-----------------------------------------------------------------------------%

:- pred fill3(string, t).
:- mode fill3(in, t4 >> t34) is det.
:- pragma no_inline(fill3/2).

fill3(S, T0) :-
    ( T0 = f1(_)
    ; T0 = f2(_)
    ; T0 = f3(package("fill3", S))
    ; T0 = f4(_)
    ).

:- pred fill23(string, t).
:- mode fill23(in, t4 >> t234) is det.
:- pragma no_inline(fill3/2).

fill23(S, T0) :-
    ( T0 = f1(_)
    ; T0 = f2(package("fill2", S))
    ; T0 = f3(package("fill3", S))
    ; T0 = f4(_)
    ).

:- pred fill2(string, t).
:- mode fill2(in, t34 >> t234) is det.
:- pragma no_inline(fill2/2).

fill2(S, T0) :-
    ( T0 = f1(_)
    ; T0 = f2(package("fill2", S))
    ; T0 = f3(_)
    ; T0 = f4(_)
    ).

:- pred fill1(string, t).
:- mode fill1(in, t234 >> ground) is det.
:- pragma no_inline(fill1/2).

fill1(S, T0) :-
    ( T0 = f1(package("fill1", S))
    ; T0 = f2(_)
    ; T0 = f3(_)
    ; T0 = f4(_)
    ).

%-----------------------------------------------------------------------------%
