%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This test case tests the combination of existential types and
% type classes, i.e. existential type class constraints.

:- module existential_type_classes.
:- interface.
:- import_module io.

:- pred main(io::di, state::uo) is det.

:- implementation.
:- import_module std_util.
:- import_module int.
:- import_module string.
:- import_module term.

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
:- type my_univ ---> my_univ(c_pointer).

:- func my_univ(T) = my_univ <= fooable(T).

:- some [T] func my_univ_value(my_univ) = T => fooable(T).

:- some [T] func call_my_univ_value(my_univ) = T => fooable(T).

:- some [T] func my_exist_t = T => fooable(T).

:- some [T] func call_my_exist_t = T => (fooable(T)).

:- pred int_foo(int::in, int::out) is det.
int_foo(X, 2*X).

:- pred string_foo(string::in, int::out) is det.
string_foo(S, N) :- string.length(S, N).

main(!IO) :-
    do_foo(42, T1),
    do_foo("blah", T2),
    do_foo(my_exist_t, T3),
    do_foo(call_my_exist_t, T4),
    do_foo(my_univ_value(my_univ(45)), T5),
    do_foo(call_my_univ_value(my_univ("something")), T6),
    io.write_int(T1, !IO), io.nl(!IO),
    io.write_int(T2, !IO), io.nl(!IO),
    io.write_int(T3, !IO), io.nl(!IO),
    io.write_int(T4, !IO), io.nl(!IO),
    io.write_int(T5, !IO), io.nl(!IO),
    io.write_int(T6, !IO), io.nl(!IO).

:- pred do_foo(T::in, int::out) is det <= fooable(T).

do_foo(X, N) :-
    foo(X, N).

call_my_exist_t = my_exist_t.

call_my_univ_value(Univ) = my_univ_value(Univ).

my_exist_t = 43.

:- pragma foreign_proc("C",
    my_univ_value(Univ::in) = (Value::out),
    [will_not_call_mercury, promise_pure],
"
    /* mention TypeInfo_for_T */
    TypeClassInfo_for_existential_type_classes__fooable_T =
        MR_field(MR_UNIV_TAG, Univ, 0);
    Value = MR_field(MR_UNIV_TAG, Univ, 1);
").

:- pragma foreign_proc("C",
    my_univ(Value::in) = (Univ::out),
    [will_not_call_mercury, promise_pure],
"
    MR_tag_incr_hp(Univ, MR_UNIV_TAG, 2);
    MR_field(MR_UNIV_TAG, Univ, 0) =
        (MR_Word) TypeClassInfo_for_existential_type_classes__fooable_T;
    MR_field(MR_UNIV_TAG, Univ, 1) = (MR_Word) Value;
").
