% Another test case to test nested modules.

:- module nested2.
:- interface.
:- import_module io.

%-----------------------------------------------------------------------------%

:- module nested2.child.
:- interface.

% module `io' is imported in nested2

:- type t1 == foo.
:- type t2 == nested2.foo.

:- pred main(io__state::di, io__state::uo) is det.

:- end_module nested2.child.

%-----------------------------------------------------------------------------%

:- implementation.

:- module nested2.child.
:- implementation.
:- import_module std_util.
:- import_module type_desc.

:- type t3 == foo.
:- type t4 == nested2.foo.

:- func has_type_t1 = t1.
:- func has_type_t2 = t2.
:- func has_type_t3 = t3.
:- func has_type_t4 = t4.

has_type_t1 = bar.
has_type_t2 = nested2.bar.
has_type_t3 = baz(42).
has_type_t4 = nested2.baz(42).

main -->
	nested2.hello,
	hello,

	print("t1 = "), print(type_of(has_type_t1)), nl,
	print("t2 = "), print(type_of(has_type_t2)), nl,
	print("t3 = "), print(type_of(has_type_t3)), nl,
	print("t4 = "), print(type_of(has_type_t4)), nl,

	print("has_type_t1 = "), print(has_type_t1), nl,
	print("has_type_t2 = "), print(has_type_t2), nl,
	print("has_type_t3 = "), print(has_type_t3), nl,
	print("has_type_t4 = "), print(has_type_t4), nl.

:- end_module nested2.child.

:- type foo ---> bar ; baz(int).

:- pred hello(io__state::di, io__state::uo) is det.

hello --> print("nested2.hello\n").

:- end_module nested2.
