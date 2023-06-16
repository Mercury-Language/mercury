%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test the --delay-partial-instantiations option.

:- module delay_partial_test.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module int.

%---------------------------------------------------------------------------%

:- type t
    --->    f(int)
    ;       g(int, int)
    ;       h(t, t).

:- type blah
    --->    blah(
                a :: int,
                b :: int,
                c :: char
            ).

main(!IO) :-
    test1(!IO),
    test2(!IO),
    test3(!IO),
    test4(!IO).

% Test implied mode calls.

:- pred test1(io::di, io::uo) is det.

test1(!IO) :-
    ( if foo(h(_, _)) then
        io.write_string("implied mode call 1 ok\n", !IO)
    else
        io.write_string("implied mode call 1 bad\n", !IO)
    ).

:- pred test2(io::di, io::uo) is det.

test2(!IO) :-
    ( if foo(g(_, _)) then
        io.write_string("implied mode call 2 bad\n", !IO)
    else
        io.write_string("implied mode call 2 ok\n", !IO)
    ).

% Test constructing things.

:- pred test3(io::di, io::uo) is det.

test3(!IO) :-
    Foo ^ a = 42,
    Foo ^ b = 43,
    Foo ^ c = 'x',
    io.print(Foo, !IO),
    io.nl(!IO).

:- pred test4(io::di, io::uo) is det.

test4(!IO) :-
    Bar ^ b = Baz ^ b,
    Baz ^ c = X,
    Bar ^ a = Bar ^ b,
    ( if foo(f(_)) then
        X = Bar ^ c,
        Bar ^ c = 'x'
    else
        X = 'z',
        Bar ^ c = 'y'
    ),
    Baz ^ a = 43,
    Baz ^ b = -42,
    io.print_line({Bar, Baz}, !IO).

:- pred foo(t::out) is multi.
:- pragma no_inline(foo/1).

foo(f(42)).
foo(h(f(43), g(44, 45))).
