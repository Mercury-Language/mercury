% This test case tests the use of module names that are C/C++/Java
% keywords, namely `int', `char', and `class'.  These might cause
% problems for back-ends targetting C/Java.

:- module class.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

%-----------------------------------------------------------------------------%

:- module class.char.
:- interface.
:- import_module io.

:- type foo ---> bar ; baz(int).

:- pred hello(io__state::di, io__state::uo) is det.

:- implementation.

hello --> io__write_string("class.char.hello\n").

:- end_module class.char.

%-----------------------------------------------------------------------------%

:- module class.int.
:- interface.
:- import_module io.

:- type foo ---> bar ; baz(int).

:- pred hello(io__state::di, io__state::uo) is det.

:- implementation.

hello --> io__write_string("class.int.hello\n").

:- end_module class.int.

%-----------------------------------------------------------------------------%

% now we're back in the parent module.

:- import_module class.char.
:- use_module class.int.
:- import_module std_util, require.

:- type t1 == class.char.foo.
:- type t2 == char.foo.
:- type t3 == foo.
:- type t4 == class.int.foo.
:- type t5 == int.foo.

main -->
	class.char.hello,
	char.hello,
	hello,
	class.int.hello,
	int.hello,

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

has_type_t1 = class.char.bar.
has_type_t2 = char.bar.
has_type_t3 = bar.
has_type_t4 = class.int.bar.
has_type_t5 = int.bar.

:- end_module class.
