% Some test cases to test nested modules.

:- module parent2.child.
:- interface.

% module `io' is imported in parent2

:- type t1 == foo.
:- type t2 == parent2.foo.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.
:- import_module std_util.

:- type t3 == foo.
:- type t4 == parent2.foo.

:- func has_type_t1 = t1.
:- func has_type_t2 = t2.
:- func has_type_t3 = t3.
:- func has_type_t4 = t4.

has_type_t1 = bar.
has_type_t2 = parent2.bar.
has_type_t3 = baz(42).
has_type_t4 = parent2.baz(42).

main -->
	parent2.hello,
	hello,

	print("t1 = "), print(type_of(has_type_t1)), nl,
	print("t2 = "), print(type_of(has_type_t2)), nl,
	print("t3 = "), print(type_of(has_type_t3)), nl,
	print("t4 = "), print(type_of(has_type_t4)), nl,

	print("has_type_t1 = "), print(has_type_t1), nl,
	print("has_type_t2 = "), print(has_type_t2), nl,
	print("has_type_t3 = "), print(has_type_t3), nl,
	print("has_type_t4 = "), print(has_type_t4), nl.

