% "Hello World" in Mercury, using nested modules.

:- module nested.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

%-----------------------------------------------------------------------------%

:- module nested.child.
:- interface.
:- import_module io.

:- type foo ---> bar ; baz(int).

:- pred hello(io__state::di, io__state::uo) is det.

:- implementation.

hello --> io__write_string("nested.child.hello\n").

:- end_module nested.child.

%-----------------------------------------------------------------------------%

:- module nested.child2.
:- interface.
:- import_module io.

:- type foo ---> bar ; baz(int).

:- pred hello(io__state::di, io__state::uo) is det.

:- implementation.

hello --> io__write_string("nested.child2.hello\n").

:- end_module nested.child2.

%-----------------------------------------------------------------------------%

% now we're back in the parent module.

:- import_module nested.child.
:- use_module nested.child2.
:- import_module std_util, require.

:- type t1 == nested.child.foo.
:- type t2 == child.foo.
:- type t3 == foo.
:- type t4 == nested.child2.foo.
:- type t5 == child2.foo.

main -->
	nested.child.hello,
	child.hello,
	hello,
	nested.child2.hello,
	child2.hello,

	print("t1 = "), print(type_of(has_type_t1)), nl,
	print("t2 = "), print(type_of(has_type_t2)), nl,
	print("t3 = "), print(type_of(has_type_t3)), nl,
	print("t4 = "), print(type_of(has_type_t4)), nl,
	print("t5 = "), print(type_of(has_type_t5)), nl,

	print("has_type_t1 = "), print(has_type_t1), nl,
	print("has_type_t2 = "), print(has_type_t2), nl,
	print("has_type_t3 = "), print(has_type_t3), nl,
	print("has_type_t4 = "), print(has_type_t4), nl,
	print("has_type_t5 = "), print(has_type_t5), nl,

	{ true }.

:- func has_type_t1 = t1.
:- func has_type_t2 = t2.
:- func has_type_t3 = t3.
:- func has_type_t4 = t4.
:- func has_type_t5 = t5.

has_type_t1 = nested.child.bar.
has_type_t2 = child.bar.
has_type_t3 = bar.
has_type_t4 = nested.child2.bar.
has_type_t5 = child2.bar.

:- end_module nested.
