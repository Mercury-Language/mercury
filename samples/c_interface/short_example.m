% This is a simple example of using the C interface to call the C function
% (or macro) puts().

% This source file is hereby placed in the public domain.  -fjh (the author).

:- module short_example.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- pred puts(string::in, io__state::di, io__state::uo) is det.

:- pragma c_header_code("#include <stdio.h>").

:- pragma c_code(non_recursive, puts(S::in, Old_IO::di, New_IO::uo),
	"puts(S); New_IO = Old_IO; "
).

main --> puts("Hello, world\n").
