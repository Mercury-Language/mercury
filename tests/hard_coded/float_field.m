%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% A test of types with floating point fields.

:- module float_field.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- type foo.
:- type bar.
:- type baz.

:- type foo2 ---> foo2(float). % no_tag type
:- type foo3 ---> foo3(foo2).  % no_tag type containing another notag type
:- type bar2 ---> bar2(int, float, int). % ordinary d.u. type
:- type bar_foo2 ---> bar_foo2(int, foo2, int).
    % notag type inside ordinary d.u. type
:- type bar_foo3 ---> bar_foo3(int, foo3, int).
    % notag type inside notag type inside ordinary d.u. type
:- type baz2 == float. % equivalence type

:- func foo_val(foo) = float.
:- func bar_val(bar) = float.
:- func baz_val(baz) = float.

:- func foo2_val(foo2) = float.
:- func foo3_val(foo3) = float.
:- func bar2_val(bar2) = float.
:- func baz2_val(baz2) = float.

:- func bar_foo2_val(bar_foo2) = float.
:- func bar_foo3_val(bar_foo3) = float.

:- implementation.

:- import_module float.
:- import_module list.
:- import_module math.
:- import_module string.

:- type foo ---> foo(float). % no_tag type
:- type bar ---> bar(int, float, int). % ordinary d.u. type
:- type baz == float. % equivalence type

foo_val(foo(X)) = X.
bar_val(bar(_, X, _)) = X.
baz_val(X) = X.

foo2_val(foo2(X)) = X.
foo3_val(foo3(X)) = foo2_val(X).
bar2_val(bar2(_, X, _)) = X.
baz2_val(X) = X.

bar_foo2_val(bar_foo2(_, foo2(X), _)) = X.
bar_foo3_val(bar_foo3(_, foo3(foo2(X)), _)) = X.

main(!IO) :-
    Foo = foo(1.0),
    io.print_line(Foo, !IO),
    io.print_line(foo_val(Foo), !IO),
    Bar = bar(2, 3.0, 4),
    io.print_line(Bar, !IO),
    io.print_line(bar_val(Bar), !IO),
    Baz = 5.0,
    io.print_line(Baz, !IO),
    io.print_line(baz_val(Baz), !IO),

    Foo2 = foo2(1.0),
    io.print_line(Foo2, !IO),
    io.print_line(foo2_val(Foo2), !IO),
    Foo3 = foo3(Foo2),
    io.print_line(Foo3, !IO),
    io.print_line(foo3_val(Foo3), !IO),
    Bar2 = bar2(2, 3.0, 4),
    io.print_line(Bar2, !IO),
    io.print_line(bar2_val(Bar2), !IO),
    Baz2 = 5.0,
    io.print_line(Baz2, !IO),
    io.print_line(baz2_val(Baz2), !IO),

    BarFoo2 = bar_foo2(6, foo2(7.0), 8),
    io.print_line(BarFoo2, !IO),
    io.print_line(bar_foo2_val(BarFoo2), !IO),

    BarFoo3 = bar_foo3(6, foo3(foo2(7.0)), 8),
    io.print_line(BarFoo3, !IO),
    io.print_line(bar_foo3_val(BarFoo3), !IO).
