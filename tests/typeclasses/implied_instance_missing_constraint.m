%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module implied_instance_missing_constraint.

:- interface.

:- pred main(io::di, io::uo) is det.

:- import_module io.

:- implementation.

:- import_module int.
:- import_module list.

:- typeclass printable(A) where [
    pred p(A::in, io::di, io::uo) is det,
    pred foo(A::in, A::out) is det
].

:- instance printable(int) where [
    pred(p/3) is io.write_int,
    pred(foo/2) is foo_int
].

:- pred foo_int(int::in, int::out) is det.

foo_int(X, X+1).

    % This test case is interesting because the "printable(T)" constraint
    % below comes only from the implementation of p/3, not foo/2, so the
    % implementation needs to discard the typeclass_info for printable(T)
    % for that call.
    %
    % XXX we currently fail this
:- instance printable(list(T)) <= printable(T) where [
    pred(p/3) is my_write_list,
    pred(foo/2) is foo_list
].

:- pred foo_list(list(T)::in, list(T)::out) is det.

foo_list(X, Y) :-
    ( if
        X = [A, B | _],
        % Here is where it crashes... rather than the type-info,
        % the typeclass-info for foo(T) was erroneously passed.
        A = B
    then
        Y = X
    else
        Y = []
    ).

main(!IO) :-
    zzz(-2, A),
    p(A, !IO),
    zzz([1, 2, 3], X),
    p(X, !IO),
    zzz([1, 1, 2, 3], Y),
    p(Y, !IO),
    io.nl(!IO).

:- pred zzz(T, T) <= printable(T).
:- mode zzz(in, out) is det.
:- pragma no_inline(zzz/2).

zzz(X, Y) :- foo(X, Y).

:- pred my_write_list(list(T), io__state, io__state) <= printable(T).
:- mode my_write_list(in, di, uo) is det.

my_write_list([]) -->
    io__write_string("[]").
my_write_list([X | Xs]) -->
    io__write_string("[\n"),
    my_write_list_2([X | Xs]),
    io__write_string("]").

:- pred my_write_list_2(list(T), io__state, io__state) <= printable(T).
:- mode my_write_list_2(in, di, uo) is det.

my_write_list_2([]) --> [].
my_write_list_2([X | Xs]) -->
    p(X),
    io__write_string("\n"),
    my_write_list_2(Xs).
