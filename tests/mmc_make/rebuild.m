% "Hello World" in Mercury.

% This source file is hereby placed in the public domain.  -fjh (the author).

:- module rebuild.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

main --> io__write_string("Hello, world\n").
