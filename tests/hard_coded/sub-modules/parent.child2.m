% "Hello World" in Mercury, using nested modules.

:- module parent.child2.
:- interface.
:- import_module io.

:- type foo ---> bar ; baz(int).

:- pred hello(io__state::di, io__state::uo) is det.

:- implementation.

hello --> io__write_string("parent.child2.hello\n").
