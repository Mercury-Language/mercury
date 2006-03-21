% This is a simple example of using the C interface to call the C function
% (or macro) puts().

% This source file is hereby placed in the public domain.  -fjh (the author).

:- module short_example.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- pred puts(string::in, io::di, io::uo) is det.

:- pragma foreign_decl("C", "#include <stdio.h>").

:- pragma foreign_proc("C",
	puts(S::in, Old_IO::di, New_IO::uo),
	[promise_pure, will_not_call_mercury],
"
	puts(S);
	New_IO = Old_IO;
").

main(!IO) :-
	puts("Hello, world\n", !IO).
