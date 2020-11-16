%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test cases for rtti for existential types.

:- module existential_rtti.

:- interface.

:- import_module io.
:- import_module list.

:- typeclass c(T) where [].

:- typeclass c2(T1, T2) where [].

:- typeclass c3(T1, T2) where [].

:- type f(X)
    --->    some [T] f(int, T, list(X), int).

:- type myf
    --->    some [T] myf(T) => c(T).

:- type f
    --->    some [T] f(int, T, int)
    ;       some [X] g(float, X, float).

:- type g
    --->    some [T] g(T).

:- type g2
    --->    g2(int).

:- type foo
    --->    foo(string, string).

:- type goo
    --->    goo
    ;       hoo.

:- type u(X)
    --->    u(X).

:- type f2(X, Y)
    --->    some [T1, T2] f2(int, T1, u(X), T2, u(Y), int).

:- type multi
    --->    some [T1, T2] multi(T1, T2) => c2(T1, T2).

:- type multi2
    --->    some [T1, T2, T3] multi2(T1, T2, T3) => (c2(T1, T2), c3(T1, T3)).

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module deconstruct.
:- import_module std_util.

:- instance c(int) where [].
:- instance c2(int, string) where [].
:- instance c3(int, float) where [].

main(!IO) :-
    io.write_string("Writing copies of terms:\n", !IO),
    A = 'new myf'(1),
    copy(A, ACopy),
    io.write_line(ACopy, !IO),
        % different types inside
    B = 'new f'(1, "hello", 42),
    copy(B, BCopy),
    io.write_line(BCopy, !IO),
    C = 'new f'(2, 'w', 42),
    copy(C, CCopy),
    io.write_line(CCopy, !IO),
        % an enum
    D = 'new f'(3, goo, 42),
    copy(D, DCopy),
    io.write_line(DCopy, !IO),
        % existential inside an existential
    E = 'new f'(4, 'new g'("hello"), 42),
    copy(E, ECopy),
    io.write_line(ECopy, !IO),
        % A no-tag inside
    F = 'new f'(5, g2(12), 42),
    copy(F, FCopy),
    io.write_line(FCopy, !IO),
    G = 'new f'(6, foo("hello", "world"), 42),
    copy(G, GCopy),
    io.write_line(GCopy, !IO),
    H = 'new g'(7.0, 'new g'("hello"), 42.0),
    copy(H, HCopy),
    io.write_line(HCopy, !IO),
        % universally quantified argument.
    I = 'new f'(8, u("hello"), 42),
    copy(I, ICopy),
    io.write_line(ICopy, !IO),
        % multiple existentially and universally quantified arguments
    J = 'new f2'(9, "hello", u("hello"), 432.1, u("world"), 42),
    copy(J, JCopy),
    io.write_line(JCopy, !IO),
        % multi parameter type class
    K = 'new multi'(10, "multiparameter"),
    copy(K, KCopy),
    io.write_line(KCopy, !IO),
        % multi parameter type class, multiple constraints
    L = 'new multi2'(11, "multiparameter", 42.0),
    copy(L, LCopy),
    io.write_line(LCopy, !IO),

    io.nl(!IO),
    io.write_string("Writing some terms:\n", !IO),
    io.write_line(A, !IO),
    io.write_line(B, !IO),
    io.write_line(C, !IO),
    io.write_line(D, !IO),
    io.write_line(E, !IO),
    io.write_line(F, !IO),
    io.write_line(G, !IO),
    io.write_line(H, !IO),
    io.write_line(I, !IO),
    io.write_line(J, !IO),
    io.write_line(K, !IO),
    io.write_line(L, !IO),

    io.nl(!IO),
    io.write_string("Writing copies of terms again:\n", !IO),
    io.write_line(ACopy, !IO),
    io.write_line(BCopy, !IO),
    io.write_line(CCopy, !IO),
    io.write_line(DCopy, !IO),
    io.write_line(ECopy, !IO),
    io.write_line(FCopy, !IO),
    io.write_line(GCopy, !IO),
    io.write_line(HCopy, !IO),
    io.write_line(ICopy, !IO),
    io.write_line(JCopy, !IO),
    io.write_line(KCopy, !IO),
    io.write_line(LCopy, !IO),

    io.write_string("Writing deconstructed terms:\n", !IO),
    deconstruct_test(A, !IO),
    deconstruct_test(B, !IO),
    deconstruct_test(C, !IO),
    deconstruct_test(D, !IO),
    deconstruct_test(E, !IO),
    deconstruct_test(F, !IO),
    deconstruct_test(G, !IO),
    deconstruct_test(H, !IO),
    deconstruct_test(I, !IO),
    deconstruct_test(J, !IO),
    deconstruct_test(K, !IO),
    deconstruct_test(L, !IO).

:- pred deconstruct_test(T::in, io::di, io::uo) is det.

deconstruct_test(Term, !IO) :-
    deconstruct(Term, canonicalize, Functor, Arity, Args),
    io.write_string(Functor, !IO),
    io.write_string("/", !IO),
    io.write_int(Arity, !IO),
    io.nl(!IO),
    io.write_list(Args, ", ", io.write, !IO),
    io.nl(!IO).
