% "Hello World" in Mercury, using nested modules.

:- module parent.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- include_module child.
:- import_module parent:child.

main -->
	parent:child:hello,
	% child:hello,		% not yet supported
	hello.

