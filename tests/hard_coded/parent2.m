% "Hello World" in Mercury,
% using nested modules.

:- module parent2.
:- interface.
:- import_module io.

:- include_module child.

:- implementation.

:- pred hello(io__state::di, io__state::uo) is det.

hello --> print("Hello world\n").
