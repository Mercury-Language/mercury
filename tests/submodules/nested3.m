% "Hello World" in Mercury, using nested modules.

:- module nested3.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

    :- module nested3.child.
    :- interface.
	:- import_module io.

	:- type foo ---> bar ; baz(int).

	:- pred hello(io__state::di, io__state::uo) is det.

    :- end_module nested3.child.

:- implementation.

%-----------------------------------------------------------------------------%

    :- module nested3.child2.
    :- interface.
        :- import_module io.

        :- type foo ---> bar ; baz(int).

        :- pred hello(io__state::di, io__state::uo) is det.
    :- end_module nested3.child2.

%-----------------------------------------------------------------------------%

    :- module nested3.child.
    :- implementation.

        hello --> io__write_string("nested3.child.hello\n").

    :- end_module nested3.child.

    :- module nested3.child2.
    :- implementation.

        hello --> io__write_string("nested3.child2.hello\n").

    :- end_module nested3.child2.

%-----------------------------------------------------------------------------%

% now we're back in the parent module.

:- import_module nested3.child.
:- use_module nested3.child2.
:- import_module require.
:- import_module std_util.
:- import_module type_desc.

:- type t1 == nested3.child.foo.
:- type t2 == child.foo.
:- type t3 == foo.
:- type t4 == nested3.child2.foo.
:- type t5 == child2.foo.

main -->
	nested3.child.hello,
	child.hello,
	hello,
	nested3.child2.hello,
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

has_type_t1 = nested3.child.bar.
has_type_t2 = child.bar.
has_type_t3 = bar.
has_type_t4 = nested3.child2.bar.
has_type_t5 = child2.bar.

:- end_module nested3.
