% "Hello World" in Mercury,
% using nested modules.

:- module parent2.
:- interface.
:- import_module io.

:- include_module child.

:- implementation.

:- type foo ---> bar ; baz(int).

:- pred hello(io__state::di, io__state::uo) is det.

hello --> print("parent2.hello\n").
