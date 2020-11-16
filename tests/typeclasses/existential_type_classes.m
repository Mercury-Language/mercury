%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This test case tests the combination of existential types and
% type classes, i.e. existential type class constraints.
%

:- module existential_type_classes.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module std_util.
:- import_module string.

:- typeclass fooable(T) where [
    pred foo(T::in, int::out) is det
].
:- typeclass barable(T) where [
    pred bar(T::in, int::out) is det
].

:- instance fooable(int) where [
    pred(foo/2) is int_foo
].

:- instance fooable(string) where [
    pred(foo/2) is string_foo
].

    % my_univ_value(Univ):
    %   returns the value of the object stored in Univ.

:- type my_univ
    --->    some [T] f(T) => fooable(T).

:- func my_univ(T) = my_univ <= fooable(T).

:- some [T] func my_univ_value(my_univ) = T => fooable(T).

:- some [T] func call_my_univ_value(my_univ) = T => fooable(T).

:- some [T] func my_exist_t = T => fooable(T).

:- pred int_foo(int::in, int::out) is det.

int_foo(X, 2*X).

:- pred string_foo(string::in, int::out) is det.

string_foo(S, N) :-
    string.length(S, N).

main(!IO) :-
    do_foo(42, !IO),
    do_foo("blah", !IO),
    do_foo(my_exist_t, !IO),
    do_foo(call_my_exist_t, !IO),
    do_foo(my_univ_value(my_univ(45)), !IO),
    do_foo(call_my_univ_value(my_univ("something")), !IO).

:- pred do_foo(T::in, io::di, io::uo) is det <= fooable(T).

do_foo(X, !IO) :-
    foo(X, N),
    io.write_line(N, !IO).

call_my_exist_t = my_exist_t.

call_my_univ_value(Univ) = my_univ_value(Univ).

my_exist_t = 43.

my_univ(T) = 'new f'(T).
my_univ_value(f(T)) = T.
