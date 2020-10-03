%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Try to retain structure sharing information when we encounter generic calls
% whose output argument modes and types tell us they can't introduce more
% sharing.
%

:- module reuse_ho.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    % This higher order call should be predicted to have bottom sharing.
    HO1 = ho1,
    HO1(1, N),

    % This method call should be predicted to have bottom sharing.
    meth(2, M),

    copy(foo(N, M), Foo1),
    addr(Foo1, Foo1_Addr),

    % Both indirect and direct reuse should occur here.
    swap(Foo1, Tmp),
    Tmp = foo(X, Y),
    Foo2 = foo(Y, X),
    addr(Foo2, Foo2_Addr),

    % This higher-order call should cause sharing to become top.
    HO2 = ho2(Foo2),
    HO2(Foo3),
    addr(Foo3, Foo3_Addr),

    % Reuse should not occur.
    swap(Foo3, Foo4),
    addr(Foo4, Foo4_Addr),

    % Reuse should not occur here either.
    copy(foo(-1, -2), Foo5),
    addr(Foo5, Foo5_Addr),
    swap(Foo5, Foo6),
    addr(Foo6, Foo6_Addr),

    ( if capable_grade($grade) then
        ( if
            Foo1_Addr = Foo2_Addr,
            Foo3_Addr \= Foo4_Addr,
            Foo5_Addr \= Foo6_Addr
        then
            io.write_string("addresses as expected\n", !IO)
        else
            io.write_string("addresses NOT as expected\n", !IO)
        )
    else
        io.write_string("grade probably doesn't support reuse\n", !IO)
    ).

%---------------------------------------------------------------------------%

:- type foo
    --->    foo(int, int).

:- pred swap(foo::in, foo::out) is det.
:- pragma no_inline(swap/2).

swap(foo(X, Y), foo(Y, X)).

%---------------------------------------------------------------------------%

:- pred ho1(int::in, int::out) is det.

ho1(N, N * 10).

:- pred ho2(foo::in, foo::out) is det.

ho2(X, X).

%---------------------------------------------------------------------------%

:- typeclass tc(T) where [
    pred meth(T::in, T::out) is det
].

:- instance tc(int) where [
    meth(X, X)
].

%---------------------------------------------------------------------------%

% Only C grades for now.
:- pred capable_grade(string::in) is semidet.

capable_grade(Grade) :-
    string.prefix(Grade, Prefix),
    ( Prefix = "none"
    ; Prefix = "reg"
    ; Prefix = "jump"
    ; Prefix = "asm"
    ; Prefix = "fast"
    ; Prefix = "hl"
    ),
    not string.sub_string_search(Grade, "debug", _),
    not string.sub_string_search(Grade, "profdeep", _).

:- pred addr(T::in, int::out) is cc_multi.

:- pragma foreign_proc("C",
    addr(T::in, Addr::out),
    [will_not_call_mercury, promise_pure, thread_safe, no_sharing],
"
    Addr = (MR_Word) T;
").
