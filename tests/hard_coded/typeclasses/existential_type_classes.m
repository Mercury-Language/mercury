% This test case tests the combination of existential types and
% type classes, i.e. existential type class constraints.

:- module existential_type_classes.
:- interface.
:- import_module io.

:- pred main(io__state::di, state::uo) is det.

:- implementation.
:- import_module std_util, int, string.

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
	%	returns the value of the object stored in Univ.
:- some [T] (func my_univ_value(univ) = T & fooable(T)).

:- some [T] (func call_my_univ_value(univ) = T & fooable(T)).

:- some [T] (func my_exist_t = T & fooable(T)).

:- pred int_foo(int::in, int::out) is det.
int_foo(X, 2*X).

:- pred string_foo(string::in, int::out) is det.
string_foo(S, N) :- string__length(S, N).

main -->
	do_foo(42),
	do_foo("blah"),
	do_foo(my_exist_t),
	do_foo(call_my_exist_t),
	do_foo(my_univ_value(univ(45))),
	do_foo(call_my_univ_value(univ("something"))).

:- pred do_foo(T::in, io__state::di, state::uo) is det <= fooable(T).
do_foo(X) -->
	{ foo(X, N) },
	write(N), nl.

call_my_exist_t = my_exist_t.

call_my_univ_value(Univ) = my_univ_value(Univ).

my_exist_t = 43.

/*
XXX we don't yet support `pragma c_code' for existential type class constraints
:- pragma c_code(my_univ_value(Univ::in) = (Value::out), will_not_call_mercury, "
	TypeInfo_for_T = field(mktag(0), Univ, UNIV_OFFSET_FOR_TYPEINFO);
	Value = field(mktag(0), Univ, UNIV_OFFSET_FOR_DATA);
	ClassInfo_1 = XXX;
").
*/
my_univ_value(_Univ) = 44.

