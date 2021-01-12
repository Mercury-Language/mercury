%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
%
% This test exercises the situations which compiler/direct_arg_in_out.m
% cannot handle, each of which should result in an error message.
%

:- module gh72_errors.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.
:- import_module string.
:- import_module solutions.

%-----------------------------------------------------------------------------%

main(!IO) :-
    test_exports(!IO),
    test_fprocs(!IO).
    % test_modes(!IO).

%-----------------------------------------------------------------------------%

:- pred test_exports(io::di, io::uo) is det.

test_exports(!IO) :-
    A = f1(_),
    test_export_1(A, !IO),

    B = f2(_),
    C = f3(_),
    test_export_2(42, B, C, !IO).

:- pred test_export_1(t, io, io).
:- mode test_export_1(t4 >> ground, di, uo) is det.
:- pragma foreign_export("C",
    test_export_1(t4 >> ground, di, uo),
    "exported_test_export_1").

test_export_1(A, !IO) :-
    fill3("c", A),
    fill2("b", A),
    fill1("a", A),
    io.write_string(dump_t(A), !IO).

:- pred test_export_2(int, t, t, io, io).
:- mode test_export_2(in, t4 >> ground, t4 >> ground, di, uo) is det.
:- pragma foreign_export("C",
    test_export_2(in, t4 >> ground, t4 >> ground, di, uo),
    "exported_test_export_2").

test_export_2(_N, A, B, !IO) :-
    fill3("c", A),
    fill2("b", A),
    fill1("a", A),
    io.write_string(dump_t(A), !IO),
    fill3("c", B),
    fill2("b", B),
    fill1("a", B),
    io.write_string(dump_t(B), !IO).

%-----------------------------------------------------------------------------%

:- pred test_fprocs(io::di, io::uo) is det.

test_fprocs(!IO) :-
    A = f1(_),
    test_fproc_1(A, !IO),

    B = f2(_),
    C = f3(_),
    test_fproc_2(42, B, C, !IO).

:- pred test_fproc_1(t, io, io).
:- mode test_fproc_1(t4 >> ground, di, uo) is det.

:- pragma foreign_proc("C",
    test_fproc_1(A::(t4 >> ground), IO0::di, IO::uo),
    [promise_pure, may_call_mercury],
"
    // A
    IO = IO0;
").

:- pred test_fproc_2(int, t, t, io, io).
:- mode test_fproc_2(in, t4 >> ground, t4 >> ground, di, uo) is det.

:- pragma foreign_proc("C",
    test_fproc_2(N::in, A::(t4 >> ground), B::(t4 >> ground), IO0::di, IO::uo),
    [promise_pure, may_call_mercury],
"
    // N, A, B
    IO = IO0;
").

%-----------------------------------------------------------------------------%

/*
These test predicates all cause mode errors before direct_arg_in_out.m
is every invoked, and I (zs) cannot think of any simple way to fix
the mode errors without screwing up the purpose of these tests.
Proposed fixes welcome.

:- pred test_modes(io::di, io::uo) is det.

test_modes(!IO) :-
    A = f1(_),
    test_modes_1(A, !IO),

    B = f2(_),
    C = f3(_),
    test_modes_2(B, 42, C, !IO),

    D = f1(_),
    E = f2(_),
    F = f3(_),
    test_modes_3(D, 42, E, F, !IO),

    G = f1(_),
    H = f2(_),
    I = f3(_),
    J = f1(_),
    test_modes_4(G, 42, H, I, J, !IO).

:- pred test_modes_1(t, io, io).
:- mode test_modes_1(((I =< ground) >> ground), di, uo) is det.

test_modes_1(A, !IO) :-
    fill3("c", A),
    fill2("b", A),
    fill1("a", A),
    io.write_string(dump_t(A), !IO).

:- pred test_modes_2(t, int, t, io, io).
:- mode test_modes_2((I =< ground) >> ground, in, (I =< ground) >> ground,
    di, uo) is det.

test_modes_2(A, _N, B, !IO) :-
    fill3("c", A),
    fill2("b", A),
    fill1("a", A),
    io.write_string(dump_t(A), !IO),
    fill3("c", B),
    fill2("b", B),
    fill1("a", B),
    io.write_string(dump_t(B), !IO).

:- pred test_modes_3(t, int, t, t, io, io).
:- mode test_modes_3(((I =< ground) >> ground), in, (t234 >> ground),
    ((I =< ground) >> ground), di, uo) is det.

test_modes_3(A, _N, B, C, !IO) :-
    fill3("c", A),
    fill2("b", A),
    fill1("a", A),
    io.write_string(dump_t(A), !IO),
    fill3("c", B),
    fill2("b", B),
    fill1("a", B),
    io.write_string(dump_t(B), !IO),
    fill3("c", C),
    fill2("b", C),
    fill1("a", C),
    io.write_string(dump_t(C), !IO).

:- pred test_modes_4(t, int, t, t, t, io, io).
:- mode test_modes_4(((I =< ground) >> ground), in, (t234 >> ground),
    ((I =< ground) >> ground), (t234 >> ground), di, uo) is det.

test_modes_4(A, _N, B, C, D, !IO) :-
    fill3("c", A),
    fill2("b", A),
    fill1("a", A),
    io.write_string(dump_t(A), !IO),
    fill3("c", B),
    fill2("b", B),
    fill1("a", B),
    io.write_string(dump_t(B), !IO),
    fill3("c", C),
    fill2("b", C),
    fill1("a", C),
    io.write_string(dump_t(C), !IO),
    fill3("c", D),
    fill2("b", D),
    fill1("a", D),
    io.write_string(dump_t(D), !IO).
*/

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

:- func dump_t(t) = string.
:- pragma no_inline(dump_t/1).

dump_t(f1(Package)) = "f1(" ++ dump_package(Package) ++ ")\n".
dump_t(f2(Package)) = "f2(" ++ dump_package(Package) ++ ")\n".
dump_t(f3(Package)) = "f3(" ++ dump_package(Package) ++ ")\n".
dump_t(f4(Package)) = "f4(" ++ dump_package(Package) ++ ")\n".

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
