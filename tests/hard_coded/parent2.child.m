% "Hello World" in Mercury,
% using nested modules.

:- module parent2:child.
:- interface.

% module `io' is imported in parent2

:- type foo ---> bar ; baz(int).

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

main --> hello, parent2:hello.
