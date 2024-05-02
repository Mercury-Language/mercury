%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module subtype_field_names.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.

:- type fruit
    --->    apple
    ;       banana
    ;       lemon
    ;       orange.

:- type citrus =< fruit
    --->    lemon
    ;       orange.

:- type foo_bar(T)
    --->    foo(
                foo_a   :: int,
                foo_b   :: fruit,
                T   % no field name in base type but present in subtype
            )
    ;       bar(
                bar_a   :: int,
                bar_b   :: fruit,
                bar_c   :: T
            ).

    % This subtype has field names that differ from the base type.
    %
:- type sub_foo(T) =< foo_bar(T)
    --->    foo(
                sub_foo_a   :: int,
                citrus, % no field name in subtype but present in base type
                sub_foo_c   :: T
            ).

    % This subtype repeats some field names from the base type, but referring
    % to different arguments (obviously a bad idea).
    %
:- type sub_bar(T) =< foo_bar(T)
    --->    bar(
                bar_b   :: int,
                bar_c   :: citrus,
                bar_a   :: T
            ).

    % Same again.
    %
:- type subsub_bar(T) =< sub_bar(T)
    --->    bar(
                bar_c   :: int,
                bar_a   :: citrus,
                bar_b   :: T
            ).

:- pred test_sub_foo(citrus::in, sub_foo(T)::in, sub_foo(T)::out) is det.
:- pragma no_inline(pred(test_sub_foo/3)).

test_sub_foo(B, Foo0, Foo) :-
    Foo0 = foo(A, _, C),
    Foo = foo(A, B, C).

:- pred test_sub_bar(citrus::in, sub_bar(T)::in, sub_bar(T)::out) is det.
:- pragma no_inline(pred(test_sub_bar/3)).

test_sub_bar(Fruit, Bar0, Bar) :-
    Bar = Bar0 ^ bar_c := Fruit.

:- pred test_subsub_bar(citrus::in, subsub_bar(T)::in, subsub_bar(T)::out) is det.
:- pragma no_inline(pred(test_subsub_bar/3)).

test_subsub_bar(Fruit, Bar0, Bar) :-
    Bar = Bar0 ^ bar_a := Fruit.

main(!IO) :-
    F0 = foo(111, orange, "str"),
    test_sub_foo(lemon, F0, F),
    io.print_line(F, !IO),

    G0 = bar(222, orange, "str"),
    test_sub_bar(lemon, G0, G),
    io.print_line(G, !IO),

    H0 = bar(333, orange, "str"),
    test_subsub_bar(lemon, H0, H),
    io.print_line(H, !IO).
